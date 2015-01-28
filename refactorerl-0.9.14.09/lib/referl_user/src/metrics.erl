-module(metrics).

-export([run/0,
	 analyze/0]).

%-define(d(X), io:format("~w", X)).

-define(direct_call_mod_to_mod, direct_call_mod_to_mod).
-define(dynfun_call_mod_to_mod, dynfun_call_mod_to_mod).
-define(direct_incoming, direct_incoming).
-define(direct_outgoing, direct_outgoing).
-define(dynfun_incoming, direct_incoming).
-define(dynfun_outgoing, direct_outgoing).


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
		  end, Modules).

analyze() ->
    accumulate_table(?direct_call_mod_to_mod, ?direct_incoming, ?direct_outgoing),
    accumulate_table(?dynfun_call_mod_to_mod, ?dynfun_incoming, ?dynfun_outgoing).

accumulate_table(SrcTable, InTable, OutTable) ->
    ets:foldl(fun({SourceMod, DestMod}, _Acc) ->
		      update_counter(InTable, SourceMod),
		      update_counter(OutTable, DestMod),
		      ok
	      end, ok, SrcTable).

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
    DynfunFunCalls = ?Query:exec(Fun, ?Query:all([dyncall], [ambcall])),
    %% io:format("~w~n",[DynfunFunCalls]),
    called_modules(DirectFunCalls, ModuleName, ?direct_call_mod_to_mod),
    called_modules(DynfunFunCalls, ModuleName, ?dynfun_call_mod_to_mod).

called_modules(FunCalls, ModuleName, StoreTable) ->
    CalledModules = 
	lists:usort(lists:flatten(
		      [?Query:exec(F, ?Fun:module()) || F <- FunCalls])),
    lists:foreach(fun(M) ->
			  store_direct({ModuleName, ?Mod:name(M)}, StoreTable) 
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

