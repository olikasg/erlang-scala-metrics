%% -*- erlang-indent-level: 2 -*-
%%----------------------------------------------------------------------
%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% Copyright 2006, 2007 Tobias Lindahl and Kostis Sagonas
%% 
%%     $Id$
%%

%%%-------------------------------------------------------------------
%%% File    : dialyzer_plt.erl
%%% Author  : Tobias Lindahl <tobiasl@it.uu.se>
%%% Description : Interface to display information in the persistent 
%%%               lookup tables.
%%%
%%% Created : 23 Jul 2004 by Tobias Lindahl <tobiasl@it.uu.se>
%%%-------------------------------------------------------------------
-module(dialyzer_plt).

-export([
	 check_plt/3,
	 compute_md5_from_files/1,
	 contains_mfa/2,
	 contains_module/2,
	 delete_contract_list/2,
	 delete_list/2,
	 delete_module/2,
	 included_files/1,
	 from_file/1,
	 get_default_plt/0,
	 %% insert/3,
	 insert_list/2,
	 insert_contract_list/2,
	 lookup/2,
	 lookup_contract/2,
	 lookup_module/2,
	 merge_plts/1,
	 new/0,
	 plt_and_info_from_file/1,
	 to_edoc/1,
	 to_edoc/4,
	 to_file/4
	]).

%% Debug utilities
-export([pp_non_returning/0, pp_mod/1]).

-include("dialyzer.hrl").

-type mod_deps() :: dict().

%% XXX: This is only for a version or so, to protect against old plts.
-type old_md5() :: [{atom(), binary()}]. 

-record(dialyzer_file_plt, {version=[]            :: string(), 
			    md5=[]                :: md5() | old_md5(),
			    info=dict:new()       :: dict(),
			    contracts=dict:new()  :: dict(),
			    mod_deps              :: mod_deps(),
			    implementation_md5=[] :: [{atom(), _}]
			   }).

-spec new() -> #dialyzer_plt{}.

new() ->
  #dialyzer_plt{info=table_new(), contracts=table_new()}.

-spec delete_module(#dialyzer_plt{}, atom()) -> #dialyzer_plt{}.

delete_module(#dialyzer_plt{info=Info, contracts=Contracts}, Mod) ->
  #dialyzer_plt{info=table_delete_module(Info, Mod),
		contracts=table_delete_module(Contracts, Mod)}.

-spec delete_list(#dialyzer_plt{}, [_]) -> #dialyzer_plt{}.

delete_list(#dialyzer_plt{info=Info, contracts=Contracts}, List) ->
  #dialyzer_plt{info=table_delete_list(Info, List),
		contracts=table_delete_list(Contracts, List)}.

-spec insert_contract_list(#dialyzer_plt{}, [{mfa(), #contract{}}]) -> #dialyzer_plt{}.

insert_contract_list(Plt = #dialyzer_plt{contracts=Contracts}, List) ->
  Plt#dialyzer_plt{contracts=table_insert_list(Contracts, List)}.

-spec lookup_contract(#dialyzer_plt{}, mfa()) -> 'none' | {'value',#contract{}}.

lookup_contract(#dialyzer_plt{contracts=Contracts}, 
		MFA={M, F, A}) when is_atom(M), is_atom(F), is_integer(A), 
				    0 =< A, A =< 255 ->
  table_lookup(Contracts, MFA).

-spec delete_contract_list(#dialyzer_plt{}, [mfa()]) -> #dialyzer_plt{}.

delete_contract_list(Plt = #dialyzer_plt{contracts=Contracts}, List) ->
  Plt#dialyzer_plt{contracts=table_delete_list(Contracts, List)}.


%% -spec insert(#dialyzer_plt{}, mfa() | integer(), {_, _}) -> #dialyzer_plt{}.
%% 
%% insert(Plt = #dialyzer_plt{info=Info}, Id, Types) ->
%%   Plt#dialyzer_plt{info=table_insert(Info, Id, Types)}.

-spec insert_list(#dialyzer_plt{}, [{mfa() | integer(), {_, _}}]) -> #dialyzer_plt{}.

insert_list(Plt = #dialyzer_plt{info=Info}, List) ->
  Plt#dialyzer_plt{info=table_insert_list(Info, List)}.

-spec lookup(#dialyzer_plt{}, integer() | mfa()) -> 'none' | {'value', {_, _}}.

lookup(#dialyzer_plt{info=Info}, MFA={M, F, A}) when is_atom(M), 
						     is_atom(F),
						     is_integer(A), 
						     0 =< A, A =< 255 ->
  table_lookup(Info, MFA);
lookup(#dialyzer_plt{info=Info}, Label) when is_integer(Label) ->
  table_lookup(Info, Label).

-spec lookup_module(#dialyzer_plt{}, atom()) -> 'none' | {'value', [{mfa(), _, _}]}.

lookup_module(#dialyzer_plt{info=Info}, M) when is_atom(M) ->
  table_lookup_module(Info, M).

-spec contains_module(#dialyzer_plt{}, atom()) -> bool().

contains_module(#dialyzer_plt{info=Info, contracts=Cs}, M) when is_atom(M) ->
  table_contains_module(Info, M) orelse table_contains_module(Cs, M).

-spec contains_mfa(#dialyzer_plt{}, mfa()) -> bool().

contains_mfa(#dialyzer_plt{info=Info, contracts=Contracts}, MFA) ->
  (table_lookup(Info, MFA) =/= none) 
    orelse (table_lookup(Contracts, MFA) =/= none).

-spec get_default_plt() -> string().

get_default_plt() ->
  case os:getenv("DIALYZER_PLT") of
    false ->
      case os:getenv("HOME") of
	false ->
	  error("The HOME environment variable needs to be set " ++
		"so that Dialyzer knows where to find the default PLT");
	HomeDir -> filename:join(HomeDir, ".dialyzer_plt")
      end;
    UserSpecPlt -> UserSpecPlt
  end.

-spec plt_and_info_from_file(string()) -> {#dialyzer_plt{}, {_, _}}.
  
plt_and_info_from_file(FileName) ->
  from_file(FileName, true).

-spec from_file(string()) -> #dialyzer_plt{}.
	 
from_file(FileName) ->
  from_file(FileName, false).

from_file(FileName, ReturnInfo) ->
  case get_record_from_file(FileName) of
    {ok, Rec} ->
      case check_version(Rec) of
	error -> 
	  Msg = io_lib:format("Old PLT file ~s\n", [FileName]),
	  error(Msg);
	ok -> 
	  Plt = #dialyzer_plt{info = Rec#dialyzer_file_plt.info,
			      contracts = Rec#dialyzer_file_plt.contracts},
	  case ReturnInfo of
	    false -> Plt;
	    true ->
	      PltInfo = {Rec#dialyzer_file_plt.md5,
			 Rec#dialyzer_file_plt.mod_deps},
	      {Plt, PltInfo}
	  end
      end;
    {error, Reason} ->
      error(io_lib:format("Could not read PLT file ~s: ~p\n", 
			  [FileName, Reason]))
  end.

-spec included_files(string()) -> {ok, [string()]} 
                               | {error, 'no_such_file' | 'read_error'}.

included_files(FileName) ->
  case get_record_from_file(FileName) of
    {ok, #dialyzer_file_plt{md5=Md5}} ->
      {ok, [File || {File, _} <- Md5]};
    {error, _What} = Error ->
      Error
  end.

check_version(#dialyzer_file_plt{version=?VSN, implementation_md5=ImplMd5}) ->
  case compute_new_md5(ImplMd5, [], []) of
    ok -> ok;
    {differ, _, _} -> error;
    {error, _} -> error
  end;
check_version(#dialyzer_file_plt{}) -> error.

get_record_from_file(FileName) ->
  case file:read_file(FileName) of
    {ok, Bin} ->
      try binary_to_term(Bin) of
	  Rec = #dialyzer_file_plt{} -> {ok, Rec};
	  _ -> {error, not_valid}
      catch 
	_:_ -> {error, not_valid}
      end;
    {error, enoent} ->
      {error, no_such_file};
    {error, _} -> 
      {error, read_error}
  end.

-spec merge_plts([#dialyzer_plt{}]) -> #dialyzer_plt{}.

merge_plts(List) ->
  InfoList = [Info || #dialyzer_plt{info=Info} <- List],
  ContractsList = [Contracts || #dialyzer_plt{contracts=Contracts} <- List],
  #dialyzer_plt{info=table_merge(InfoList),
		contracts=table_merge(ContractsList)}.

-spec to_file(string(), #dialyzer_plt{}, dict(), {md5(), dict()}) -> 'ok'.

to_file(FileName, #dialyzer_plt{info=Info, contracts=Contracts}, 
	ModDeps, {MD5, OldModDeps}) ->
  NewModDeps = dict:merge(fun(_Key, OldVal, NewVal) -> 
			      ordsets:union(OldVal, NewVal)
			  end, 
			  OldModDeps, ModDeps),
  ImplMd5 = compute_implementation_md5(),
  Record = #dialyzer_file_plt{version=?VSN, 
			      md5=MD5, 
			      info=Info,
			      contracts=Contracts,
			      mod_deps=NewModDeps,
			      implementation_md5=ImplMd5},
  Bin = term_to_binary(Record, [compressed]),
  case file:write_file(FileName, Bin) of
    ok -> ok;
    {error, Reason} ->
      Msg = io_lib:format("Could not write PLT file ~s: ~w\n", 
			  [FileName, Reason]),
      throw({dialyzer_error, Msg})
  end.

-type md5_diff()    :: [{'differ',atom()} | {'removed',atom()}].
-type check_error() :: 'not_valid' | 'no_such_file' | 'read_error'
                     | {'no_file_to_remove', string()}.
      
-spec check_plt(string(), [string()], [string()]) -> 
	 'ok'
       | {'error', check_error()}
       | {'differ', md5(), md5_diff(), mod_deps()}
       | {'old_version', md5()}.

check_plt(FileName, RemoveFiles, AddFiles) ->
  case get_record_from_file(FileName) of
    {ok, Rec = #dialyzer_file_plt{md5=Md5, mod_deps=ModDeps}} ->
      case is_old_md5_format(Md5) of
	true ->
	  {error, not_valid};
	false ->
	  case check_version(Rec) of
	    ok -> 
	      case compute_new_md5(Md5, RemoveFiles, AddFiles) of
		ok -> ok;
		{differ, NewMd5, DiffMd5} -> {differ, NewMd5, DiffMd5, ModDeps};
		{error, What} -> {error, What}
	      end;
	    error ->
	      case compute_new_md5(Md5, RemoveFiles, AddFiles) of
		{differ, NewMd5, _DiffMd5} -> {old_version, NewMd5};
		ok -> {old_version, Md5};
		{error, What} -> {error, What}
	      end
	  end
      end;
    Error -> Error
  end.

%% Since the old md5 format is so similar, we need to make sure this
%% does not leak into the new analysis. In a way a non-backwards
%% compability issue.
is_old_md5_format([{Mod, _Md5}|_]) ->
  is_atom(Mod).

compute_new_md5(Md5, [], []) ->
  compute_new_md5_1(Md5, [], []);
compute_new_md5(Md5, RemoveFiles0, AddFiles0) ->
  %% Assume that files are first removed and then added. Files that
  %% are both removed and added will be checked for consistency in the
  %% normal way. If they have moved, we assume that they differ.
  RemoveFiles = RemoveFiles0 -- AddFiles0,
  AddFiles = AddFiles0 -- RemoveFiles0,
  InitDiffList = init_diff_list(RemoveFiles, AddFiles),
  case init_md5_list(Md5, RemoveFiles, AddFiles) of
    {ok, NewMd5} -> compute_new_md5_1(NewMd5, [], InitDiffList);
    {error, What} -> {error, What}
  end.

compute_new_md5_1([{File, Md5}|Left], NewList, Diff) ->
  case compute_md5_from_file(File) of
    Md5 -> compute_new_md5_1(Left, [{File, Md5}|NewList], Diff);
    NewMd5 ->
      ModName = beam_file_to_module(File),
      compute_new_md5_1(Left, [{File, NewMd5}|NewList], [{differ, ModName}|Diff])
  end;
compute_new_md5_1([], _NewList, []) ->
  ok;
compute_new_md5_1([], NewList, Diff) ->
  {differ, lists:keysort(1, NewList), Diff}.

compute_implementation_md5() ->
  Dir = code:lib_dir(hipe),
  Files1 = ["erl_bif_types.beam", "erl_types.beam"],
  Files2 = [filename:join([Dir, "ebin", F]) || F <- Files1],
  compute_md5_from_files(Files2).

-spec compute_md5_from_files([string()]) -> [{string(), binary()}].

compute_md5_from_files(Files) ->
  lists:keysort(1, [{F, compute_md5_from_file(F)} || F <- Files]).

compute_md5_from_file(File) ->
  case filelib:is_regular(File) of
    false -> 
      Msg = io_lib:format("Not a regular file: ~s\n", [File]),
      throw({dialyzer_error, Msg});
    true ->
      case dialyzer_utils:get_abstract_code_from_beam(File) of
	error ->
	  Msg = io_lib:format("Could not get abstract code for file: ~s (please recompile it with +debug_info)\n", [File]),
	  throw({dialyzer_error, Msg});
	{ok, Abs} ->
	  erlang:md5(term_to_binary(Abs))
      end
  end.

init_diff_list(RemoveFiles, AddFiles) ->
  RemoveSet0 = sets:from_list([beam_file_to_module(F) || F <- RemoveFiles]),
  AddSet0 = sets:from_list([beam_file_to_module(F) || F <- AddFiles]),
  DiffSet = sets:intersection(AddSet0, RemoveSet0),
  RemoveSet = sets:subtract(RemoveSet0, DiffSet),
  %% Added files and diff files will appear as diff files from the md5 check.
  [{removed, F} || F <- sets:to_list(RemoveSet)].

init_md5_list(Md5, RemoveFiles, AddFiles) ->
  DiffFiles = lists:keysort(2, [{remove, F} || F <- RemoveFiles]
			    ++ [{add, F}    || F <- AddFiles]),
  Md5Sorted = lists:keysort(1, Md5),
  init_md5_list_1(Md5Sorted, DiffFiles, []).

init_md5_list_1([{File, _Md5}|Md5Left], [{remove, File}|DiffLeft], Acc) ->
  init_md5_list_1(Md5Left, DiffLeft, Acc);
init_md5_list_1([{File, Md5}|Md5Left], [{add, File}|DiffLeft], Acc) ->
  init_md5_list_1(Md5Left, DiffLeft, [{File, Md5}|Acc]);
init_md5_list_1(Md5List = [{File1, Md5}|Md5Left], 
	      DiffList = [{Tag, File2}|DiffLeft], Acc) ->
  case File1 < File2 of
    true -> init_md5_list_1(Md5Left, DiffList, [{File1, Md5}|Acc]);
    false ->
      %% Just an assert.
      true = File1 > File2,
      case Tag of
	add -> init_md5_list_1(Md5List, DiffLeft, [{File2, <<>>}|Acc]);
	remove -> {error, {no_file_to_remove, File2}}
      end
  end;
init_md5_list_1([], DiffList, Acc) ->
  AddFiles = [{F, <<>>} || {add, F} <- DiffList],
  {ok, lists:reverse(Acc, AddFiles)};
init_md5_list_1(Md5List, [], Acc) ->
  {ok, lists:reverse(Acc, Md5List)}.

%%---------------------------------------------------------------------------
%% Edoc

-spec to_edoc(#dialyzer_plt{}) -> string().

to_edoc(#dialyzer_plt{info=Info}) ->
  %% TODO: Should print contracts as well.
  List = 
    lists:sort([{MFA, Val} || {MFA = {_,_,_}, Val} <- table_to_list(Info)]),
  lists:flatten(expand_edoc(List, [])).

beam_file_to_module(Filename) ->
  list_to_atom(filename:basename(Filename, ".beam")).

-spec to_edoc(#dialyzer_plt{}, atom(), atom(), byte()) -> string().

to_edoc(PLT, M, F, A) when is_atom(M), is_atom(F) ->
  {value, Val} = lookup(PLT, {M, F, A}),
  lists:flatten(expand_edoc([{{M, F, A}, Val}], [])).

expand_edoc([{{M, F, A}, {ReturnType, ArgTypes}}|Left], M) ->
  case erl_types:t_is_any(ArgTypes) of
    true ->
      [io_lib:format("%% @spec ~w(~s) -> ~s\n", 
		     [F, expand_args_any(A),
		      erl_types:t_to_string(ReturnType)])
       | expand_edoc(Left, M)];
    false ->
      [io_lib:format("%% @spec ~w(~s) -> ~s\n", 
		     [F, expand_args(ArgTypes), 
		      erl_types:t_to_string(ReturnType)])
       | expand_edoc(Left, M)]
  end;
expand_edoc(List = [{{M1, _F, _A}, {_ReturnType, _ArgTypes}}| _], _M) ->
  [io_lib:format("\n\n%% -------  Module: ~w -------\n\n", [M1]) | 
   expand_edoc(List, M1)];
expand_edoc([], _) ->
  [].

expand_args_any(0) ->
  [];
expand_args_any(1) ->
  ["_"];
expand_args_any(X) ->
  ["_,"|expand_args_any(X-1)].

expand_args([]) ->
  [];
expand_args([ArgType]) ->
  case erl_types:t_is_any(ArgType) of
    true -> ["_"];
    false -> [erl_types:t_to_string(ArgType)]
  end;
expand_args([ArgType|Left]) ->
  [case erl_types:t_is_any(ArgType) of
     true -> "_";
     false -> erl_types:t_to_string(ArgType)
   end ++
   ","|expand_args(Left)].

error(Msg) ->
  throw({dialyzer_error, lists:flatten(Msg)}).

%%---------------------------------------------------------------------------
%% Ets table

table_new() ->
  dict:new().

table_to_list(Plt) ->
  dict:to_list(Plt).

table_delete_module(Plt, Mod) ->
  dict:filter(fun({M, _F, _A}, _Val) -> M =/= Mod;
		 (_, _) -> true
	      end, Plt).

table_delete_list(Plt, [H|T]) ->
  table_delete_list(dict:erase(H, Plt), T);
table_delete_list(Plt, []) ->
  Plt.

table_insert_list(Plt, [{Key, Val}|Left]) ->
  table_insert_list(table_insert(Plt, Key, Val), Left);
table_insert_list(Plt, []) ->
  Plt.

table_insert(Plt, Key, {Ret, Arg}) -> 
  dict:store(Key, {Ret, Arg}, Plt);
table_insert(Plt, Key, C = #contract{}) ->
  dict:store(Key, C, Plt).

table_lookup(Plt, Obj) ->
  case dict:find(Obj, Plt) of
    error -> none;
    {ok, Val} -> {value, Val}
  end.

table_lookup_module(Plt, Mod) ->
  List = dict:fold(fun(Key, Val, Acc) ->
		       case Key of
			 {Mod, _F, _A} -> [{Key, element(1, Val),
					    element(2, Val)}|Acc];
			 _ -> Acc
		       end
		   end, [], Plt),
  case List =:= [] of
    true -> none;
    false -> {value, List}
  end.

table_contains_module(Plt, Mod) ->
  dict:fold(fun({M, _F, _A}, _Val, _Acc) when M =:= Mod -> true;
	       (_, _, Acc) -> Acc
	    end, false, Plt).

table_merge([H|T]) ->
  table_merge(T, H).

table_merge([], Acc) ->
  Acc;
table_merge([Plt|Left], Acc) ->
  NewAcc = dict:merge(fun(_Key, Val, Val) -> Val end, Plt, Acc),
  table_merge(Left, NewAcc).

%%---------------------------------------------------------------------------
%% Debug utilities.

-spec pp_non_returning() -> 'ok'.

pp_non_returning() ->
  PltFile = filename:join([code:lib_dir(dialyzer), "plt", "dialyzer_init_plt"]),
  Plt = from_file(PltFile),
  List = table_to_list(Plt#dialyzer_plt.info),
  Unit = [{MFA, erl_types:t_fun(Dom, Range)} || {MFA, Range, Dom} <- List,
						erl_types:t_is_unit(Range)],
  None = [{MFA, erl_types:t_fun(Dom, Range)} || {MFA, Range, Dom} <- List,
						erl_types:t_is_none(Range)],
  io:format("=========================================\n"),
  io:format("=                Loops                  =\n"),
  io:format("=========================================\n\n"),
  lists:foreach(fun({{M,F,A}, Type}) ->
		    io:format("~w:~w/~p :: ~s\n",
			      [M, F, A, erl_types:t_to_string(Type)])
		end, lists:sort(Unit)),
  io:format("\n\n"),
  io:format("=========================================\n"),
  io:format("=                Errors                 =\n"),
  io:format("=========================================\n\n"),
  lists:foreach(fun({{M,F,A}, Type}) ->
		    io:format("~w:~w/~p :: ~s\n",
			      [M, F, A, erl_types:t_to_string(Type)])
		end, lists:sort(None)),
  ok.

-spec pp_mod(atom()) -> 'ok'.

pp_mod(Mod) when is_atom(Mod) ->
  PltFile = filename:join([code:lib_dir(dialyzer), "plt", "dialyzer_init_plt"]),
  Plt = from_file(PltFile),
  {value, List} = lookup_module(Plt, Mod),
  lists:foreach(fun({{M,F,A}, {Ret, Args}}) ->
		    T = erl_types:t_to_string(erl_types:t_fun(Args, Ret)),
		    io:format("~w:~w/~p :: ~s\n", [M, F, A, T])
		end, lists:sort(List)),
  ok.
