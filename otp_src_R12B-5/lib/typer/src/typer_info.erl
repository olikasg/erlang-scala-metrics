%% -*- erlang-indent-level: 2 -*-
%%--------------------------------------------------------------------
%% File        : typer_info.erl
%% Author      : Bingwen He <Bingwen.He@gmail.com>
%% Description : 
%%--------------------------------------------------------------------

-module(typer_info).

-export([collect/1]).

-type func_info() :: {non_neg_integer(), atom(), byte()}.
-type inc_file_info() :: {string(), func_info()}.

-record(tmpAcc, {file		:: string(),
		 module		:: atom(),
		 funcAcc=[]	:: [func_info()],
		 incFuncAcc=[]	:: [inc_file_info()],
		 dialyzerObj=[] :: [{mfa(),{_,_}}]}).

-include("typer.hrl").

-spec collect(#typer_analysis{}) -> #typer_analysis{}.

collect(Analysis) ->
  NewPlt =
    try get_dialyzer_plt(Analysis) of
	DialyzerPlt ->
	dialyzer_plt:merge_plts([Analysis#typer_analysis.trust_plt, DialyzerPlt])
    catch
      throw:{dialyzer_error,_Reason} ->
	typer:error("Dialyzer's PLT is missing or is not up-to-date; please (re)create it")
    end,
  lists:foldl(fun collect_one_file_info/2, 
	      Analysis#typer_analysis{trust_plt=NewPlt}, 
	      Analysis#typer_analysis.ana_files).

collect_one_file_info(File, Analysis) ->
  Ds = [{d,Name,Val} || {Name,Val} <- Analysis#typer_analysis.macros],
  %% Current directory should also be included in "Includes".
  Includes = [filename:dirname(File)|Analysis#typer_analysis.includes],
  Is = [{i,Dir} || Dir <- Includes],
  Options = ?SRC_COMPILE_OPTS ++ Is ++ Ds,
  case dialyzer_utils:get_abstract_code_from_src(File, Options) of
    {error, Reason} ->
      %% io:format("File=~p\n,Options=~p\n,Error=~p\n", [File,Options,Reason]),
      typer:compile_error(Reason);
    {ok, AbstractCode} ->
      case dialyzer_utils:get_core_from_abstract_code(AbstractCode, Options) of
	error -> typer:compile_error(["Could not get core erlang for "++File]);
	{ok, Core} ->
	  case dialyzer_utils:get_record_and_type_info(AbstractCode) of
	    {error, Reason} -> typer:compile_error([Reason]);
	    {ok, Records} -> 
	      case dialyzer_utils:get_spec_info(AbstractCode, Records) of
		{error, Reason} -> typer:compile_error([Reason]);
		{ok, SpecInfo} -> 
		  analyze_core_tree(Core, Records, SpecInfo, Analysis, File)
	      end
	  end
      end
  end.

analyze_core_tree(Core, Records, SpecInfo, Analysis, File) ->
  Module = list_to_atom(filename:basename(File, ".erl")),
  TmpTree = cerl:from_records(Core),
  CS1 = Analysis#typer_analysis.code_server,
  NextLabel = dialyzer_codeserver:next_core_label(CS1),
  {Tree,NewLabel} = cerl_trees:label(TmpTree, NextLabel),
  CS2 = dialyzer_codeserver:insert([{Module, Tree}], CS1),
  CS3 = dialyzer_codeserver:update_next_core_label(NewLabel, CS2),
  CS4 = dialyzer_codeserver:store_records(Module, Records, CS3),
  CS5 = dialyzer_codeserver:store_contracts(Module, SpecInfo, CS4),
  Ex_Funcs = [{0,F,A} || {_,_,{F,A}} <- cerl:module_exports(Tree)],
  TmpCG = Analysis#typer_analysis.callgraph,
  CG = dialyzer_callgraph:scan_core_tree(Tree, TmpCG),

  Fun = fun analyze_one_function/2,
  All_Defs = cerl:module_defs(Tree),
  Acc = lists:foldl(Fun, #tmpAcc{file=File,module=Module}, All_Defs),
  Exported_FuncMap = typer_map:insert({File, Ex_Funcs},
				      Analysis#typer_analysis.ex_func),
  %% NOTE: we must sort all functions in the file which
  %% originate from this file by *numerical order* of lineNo
  Sorted_Functions = lists:keysort(1, Acc#tmpAcc.funcAcc),
  FuncMap = typer_map:insert({File,Sorted_Functions},
			     Analysis#typer_analysis.func),
  %% NOTE: However we do not need to sort functions
  %% which are imported from included files.
  IncFuncMap = typer_map:insert({File, Acc#tmpAcc.incFuncAcc}, 
				Analysis#typer_analysis.inc_func),
  
  Final_Files = Analysis#typer_analysis.final_files++[{File, Module}],
  RecordMap = typer_map:insert({File,Records}, Analysis#typer_analysis.record),
  Analysis#typer_analysis{final_files=Final_Files,
			  callgraph=CG,
			  code_server=CS5,
			  ex_func=Exported_FuncMap,
			  inc_func=IncFuncMap,
			  record=RecordMap,
			  func=FuncMap}.

analyze_one_function({Var,FunBody}, Acc) ->
  F = cerl:fname_id(Var),
  A = cerl:fname_arity(Var),
  TmpDialyzerObj = {{Acc#tmpAcc.module,F,A},{Var,FunBody}},
  NewDialyzerObj = Acc#tmpAcc.dialyzerObj++[TmpDialyzerObj],  
  [_,LineNo,{file,FileName}] = cerl:get_ann(FunBody),
  BaseName = filename:basename(FileName),
  FuncInfo = {LineNo,F,A},
  OriginalName = Acc#tmpAcc.file,
  case (FileName =:= OriginalName) orelse (BaseName =:= OriginalName) of
    true -> %% Coming from original file
      %% io:format("Added function ~p\n",[{LineNo,F,A}]),
      FuncAcc    = Acc#tmpAcc.funcAcc++[FuncInfo], 
      IncFuncAcc = Acc#tmpAcc.incFuncAcc;
    false ->
      %% Coming from other sourses, including:
      %%     -- .yrl (yecc-generated file)
      %%     -- yeccpre.hrl (yecc-generated file)
      %%     -- other cases
      FuncAcc    = Acc#tmpAcc.funcAcc,
      IncFuncAcc = Acc#tmpAcc.incFuncAcc++[{FileName,FuncInfo}]
  end,
  Acc#tmpAcc{funcAcc=FuncAcc,
	     incFuncAcc=IncFuncAcc,
	     dialyzerObj=NewDialyzerObj}.

get_dialyzer_plt(#typer_analysis{plt=PltFile0}) ->
  PltFile =
    case PltFile0 =:= none of
      true -> dialyzer_plt:get_default_plt();
      false -> PltFile0
    end,
  dialyzer_plt:from_file(PltFile).
