-module(metrics).

-export([run/0,
	 analyze/0]).

%-define(d(X), io:format("~w", X)).

-define(direct_call_mod_to_mod, direct_call_mod_to_mod).
-define(dynfun_call_mod_to_mod, dynfun_call_mod_to_mod).
-define(direct_incoming, direct_incoming).
-define(direct_outgoing, direct_outgoing).
-define(dynfun_incoming, dynfun_incoming).
-define(dynfun_outgoing, dynfun_outgoing).


-include("user.hrl").

run() ->
    initialize(),
    Modules = modules(),
    lists:foreach(fun(Mod) ->
			  ModuleName = ?Mod:name(Mod),
			  io:format("Analyzing module: ~p~n", [ModuleName]),
			  lists:foreach(
			    fun(Fun) -> 
				    store_fun(Fun) 
			    end, functions(Mod))
		  end, Modules),
    write_result(direct_calls, merge_tables(?direct_incoming,?direct_outgoing)).

analyze() ->
    accumulate_table(?direct_call_mod_to_mod, ?direct_incoming, ?direct_outgoing),
    accumulate_table(?dynfun_call_mod_to_mod, ?dynfun_incoming, ?dynfun_outgoing),
    write_result(direct_calls, merge_tables(?direct_incoming,?direct_outgoing)),
    write_result(dynfun_calls, merge_tables(?dynfun_incoming,?dynfun_outgoing)).

accumulate_table(SrcTable, InTable, OutTable) ->
    ets:foldl(fun({SourceMod, DestMod}, _Acc) ->
		      update_counter(InTable, SourceMod),
		      update_counter(OutTable, DestMod),
		      ok
	      end, ok, SrcTable).

merge_tables(IncTable, OutTable) ->
    Tid = ets:new(tmp_table, [set, {keypos, 1}]),
    ets:foldl(fun({Mod, IncomingEdges}, _Acc) ->
		      case ets:lookup(Tid, Mod) of
			  [] ->
			      ets:insert(Tid, {Mod, IncomingEdges, 0});
			  [{Mod, _IncEdges, OugEdges}] ->
			      ets:insert(Tid, {Mod, IncomingEdges, OugEdges})
		      end
	      end, ok, IncTable),
    ets:foldl(fun({Mod, OutgoingEdges}, _Acc) ->
		      case ets:lookup(Tid, Mod) of
			  [] ->
			      ets:insert(Tid, {Mod, 0, OutgoingEdges});
			  [{Mod, IncEdges, _OugEdges}] ->
			      ets:insert(Tid, {Mod, IncEdges, OutgoingEdges})
		      end
	      end, ok, OutTable),
    ResList = ets:tab2list(Tid),
    ets:delete(Tid),
    ResList.

write_result(FileName, Table) ->
    FullFileName = atom_to_list(FileName) ++ ".csv",
    {ok, Fd} = file:open(FullFileName, write),
    io:format(Fd, "Module,Incoming,Outgoing~n", []),
    lists:foldl(fun({Module, InEdges, OutEdges}, _Acc) ->
		      io:format(Fd, "~w,~w,~w~n", [Module, InEdges, OutEdges])
	      end, ok, Table),
    file:close(Fd).

update_counter(Table, Key) ->
    case ets:lookup(Table, Key) of
	[] ->
	    ets:insert(Table, {Key, 1});
	[{Key, N}] ->
	    ets:insert(Table, {Key, N + 1})
    end.

modules() ->
    ?Query:exec(?Graph:root(), ?Mod:all()).

functions(Module) ->
    ?Query:exec(Module, ?Mod:locals()).

store_fun(Fun) ->
    [Module] = ?Query:exec(Fun, ?Fun:module()),
    ModuleName = ?Mod:name(Module),
    DirectFunCalls = ?Query:exec(Fun, ?Fun:funcalls()),
    AllDirectFunCalls = [ FC || FC <- DirectFunCalls, 
				FRef <- ?Query:exec(FC, ?Fun:applications()),
				is_inside(Fun, FRef)], 
    _DynfunFunCalls = [],    %io:format("~w~n",[AllDirectFunCalls]),
    called_modules(AllDirectFunCalls, ModuleName).
    %called_modules(DynfunFunCalls, ModuleName).

is_inside(Fun, FRef) ->
    FuncOfFRef = ?Query:exec(FRef, ?Query:seq([?Expr:clause(),
						 ?Clause:form(),
						 ?Form:func()])),
    case FuncOfFRef of
	[Fun] ->
	    true;
	_ ->
	    false
    end.

called_modules(FunCalls, ModuleName) ->
    CalledModules = 
	lists:flatten(
	  [?Query:exec(F, ?Fun:module()) || F <- FunCalls]),
    lists:foreach(
      fun(M) ->
	      TargetModuleName = ?Mod:name(M),
	      case TargetModuleName of
		  ModuleName -> ok;
		  _ ->
		      update_counter(?direct_incoming, TargetModuleName),
		      update_counter(?direct_outgoing, ModuleName)
	      end
      end, CalledModules).


store_direct({Module, Module}, _StoreTable) ->
    ok;
store_direct(Pair, StoreTable) ->
    % io:format(" * ~w: ~w~n", [StoreTable, Pair]),
    ets:insert(StoreTable, Pair).

initialize() ->
    Tables = [{?direct_call_mod_to_mod, [named_table, bag, {keypos, 1}]},
	      {?dynfun_call_mod_to_mod, [named_table, bag, {keypos, 1}]},
	      {?direct_incoming, [named_table, set, {keypos, 1}]},
	      {?direct_outgoing, [named_table, set, {keypos, 1}]},
	      {?dynfun_incoming, [named_table, set, {keypos, 1}]},
	      {?dynfun_outgoing, [named_table, set, {keypos, 1}]}],
    lists:foreach(fun init_table/1, Tables).

init_table({Name, Options}) ->
    case ets:info(Name) of
	undefined ->
	    ets:new(Name, Options);
	_ ->
	    ets:delete_all_objects(Name)
    end.

