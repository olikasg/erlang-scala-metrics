%%<copyright>
%% <year>2003-2007</year>
%% <holder>Ericsson AB, All Rights Reserved</holder>
%%</copyright>
%%<legalnotice>
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% The Initial Developer of the Original Code is Ericsson AB.
%%</legalnotice>
%%
%%----------------------------------------------------------------------
%% Purpose:
%%----------------------------------------------------------------------
-module(snmp_agent_mibs_test).

%%----------------------------------------------------------------------
%% Include files
%%----------------------------------------------------------------------
-include("test_server.hrl").
-include("snmp_test_lib.hrl").
-include_lib("snmp/include/snmp_types.hrl").
-include_lib("snmp/include/SNMP-COMMUNITY-MIB.hrl").
-include_lib("snmp/include/SNMP-VIEW-BASED-ACM-MIB.hrl").
-include_lib("snmp/include/SNMP-USER-BASED-SM-MIB.hrl").


%%----------------------------------------------------------------------
%% External exports
%%----------------------------------------------------------------------
-export([
	 all/1, 
         init_per_testcase/2, fin_per_testcase/2,
	 init_all/1, finish_all/1, 

	 start_and_stop/1,
	 size_check/1,
	 size_check_ets/1,
	 size_check_dets/1,
	 size_check_mnesia/1,
	 load_unload/1,
	 me_lookup/1,
	 which_mib/1

	]).

%%----------------------------------------------------------------------
%% Internal exports
%%----------------------------------------------------------------------
-export([
        ]).

%%----------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Records
%%----------------------------------------------------------------------

%%======================================================================
%% External functions
%%======================================================================

init_per_testcase(size_check_dets, Config) when list(Config) ->
    Dir = ?config(priv_dir, Config),
    DetsDir = join(Dir, "dets_dir/"),
    ?line ok = file:make_dir(DetsDir),
    [{dets_dir, DetsDir}|Config];
init_per_testcase(size_check_mnesia, Config) when list(Config) ->
    Dir = ?config(priv_dir, Config),
    MnesiaDir = join(Dir, "mnesia_dir/"),
    ?line ok = file:make_dir(MnesiaDir),
    mnesia_start([{dir, MnesiaDir}]),
    [{mnesia_dir, MnesiaDir}|Config];
init_per_testcase(_Case, Config) when list(Config) ->
    Config.

fin_per_testcase(size_check_dets, Config) when list(Config) ->
    Dir = ?config(dets_dir, Config),
    ?line ok = ?DEL_DIR(Dir),
    lists:keydelete(dets_dir, 1, Config);
fin_per_testcase(size_check_mnesia, Config) when list(Config) ->
    mnesia_stop(),
    Dir = ?config(mnesia_dir, Config),
    ?line ok = ?DEL_DIR(Dir),
    lists:keydelete(mnesia_dir, 1, Config);
fin_per_testcase(_Case, Config) when list(Config) ->
    Config.


%%======================================================================
%% Test case definitions
%%======================================================================

all(suite) ->
    {conf, init_all, cases(), finish_all}.

cases() ->
    [
     start_and_stop,
     load_unload,
     size_check,
     me_lookup,
     which_mib
    ].

init_all(Config) when list(Config) ->
    %% Data dir points wrong
    DataDir0     = ?config(data_dir, Config),
    DataDir1     = filename:split(filename:absname(DataDir0)),
    [_|DataDir2] = lists:reverse(DataDir1),
    DataDir      = filename:join(lists:reverse(DataDir2) ++ [?snmp_test_data]),
    [{snmp_data_dir, DataDir ++ "/"}|Config].

finish_all(Config) when list(Config) ->
    lists:keydelete(snmp_data_dir, 1, Config).


%%======================================================================
%% Test functions
%%======================================================================

start_and_stop(suite) -> [];
start_and_stop(Config) when list(Config) ->
    Prio      = normal,
    Verbosity = trace,

    ?line sym_start(Prio, Verbosity),
    ?line MibsPid = mibs_start(Prio, Verbosity),

    ?line mibs_info(MibsPid),

    ?line mibs_stop(MibsPid),
    ?line sym_stop(),

    ok.


%% ---------------------------------------------------------------------

load_unload(suite) -> [];
load_unload(Config) when list(Config) ->
    Prio       = normal,
    Verbosity  = log,
    %% MibStorage = ets,
    MibDir     = ?config(snmp_data_dir, Config),

    ?DBG("load_unload -> start symbolic store", []),
    ?line sym_start(Prio, Verbosity),

    ?DBG("load_unload -> start mib server", []),
    ?line MibsPid = mibs_start(Prio, Verbosity),
    
    ?DBG("load_unload -> load one not already loaded mib", []),
    ?line ok = verify_loaded_mibs(MibsPid, MibDir, []),
    ?line ok = load_mibs(MibsPid, MibDir, ["Test2"]),
    ?line ok = verify_loaded_mibs(MibsPid, MibDir, ["Test2"]),
    
    ?DBG("load_unload -> load one already loaded mib", []),
    ?line {error, _} = load_mibs(MibsPid, MibDir, ["Test2"]),

    ?DBG("load_unload -> load 2 not already loaded mibs", []),
    ?line ok = load_mibs(MibsPid, MibDir, ["TestTrap", "TestTrapv2"]),
    ?line ok = verify_loaded_mibs(MibsPid, MibDir, 
				  ["Test2", "TestTrap", "TestTrapv2"]),
    
    ?DBG("load_unload -> unload one loaded mib", []),
    ?line ok = unload_mibs(MibsPid, ["Test2"]),
    ?line ok = verify_loaded_mibs(MibsPid, MibDir, ["TestTrap", "TestTrapv2"]),
    
    ?DBG("load_unload -> try unload two loaded mibs and one not loaded", []),
    ?line {error, _} = unload_mibs(MibsPid, ["TestTrap","Test2","TestTrapv2"]),
    ?line ok = verify_loaded_mibs(MibsPid, MibDir, ["TestTrapv2"]),
    
    ?DBG("load_unload -> unload the remaining loaded mib", []),
    ?line ok = unload_mibs(MibsPid, ["TestTrapv2"]),
    ?line ok = verify_loaded_mibs(MibsPid, MibDir, []),
    
    ?DBG("load_unload -> stop mib server", []),
    ?line mibs_stop(MibsPid),

    ?DBG("load_unload -> stop symbolic store", []),
    ?line sym_stop(),

    ok.


%% ---------------------------------------------------------------------

size_check(suite) ->
    [
     size_check_ets,
     size_check_dets,
     size_check_mnesia
    ].

size_check_ets(suite) ->
    [];
size_check_ets(Config) when list(Config) ->
    do_size_check([{mib_storage, ets}|Config]).

size_check_dets(suite) ->
    [];
size_check_dets(Config) when list(Config) ->
    Dir = ?config(dets_dir, Config),
    do_size_check([{mib_storage, {dets, Dir}}|Config]).

size_check_mnesia(suite) ->
    [];
size_check_mnesia(Config) when list(Config) ->
    do_size_check([{mib_storage, {mnesia, [node()]}}|Config]).

do_size_check(Config) ->
    ?DBG("do_size_check -> start", []),
    Prio      = normal,
    Verbosity = trace,

    MibStorage = ?config(mib_storage, Config),
    ?DBG("do_size_check -> MibStorage: ~p", [MibStorage]),
    MibDir     = ?config(snmp_data_dir, Config),
    StdMibDir  = filename:join(code:priv_dir(snmp), "mibs") ++ "/",
    
    ?DBG("do_size_check -> start symbolic store", []),
    ?line sym_start(Prio, MibStorage, Verbosity),
    ?DBG("do_size_check -> start mib server", []),
    ?line MibsPid = mibs_start(Prio, MibStorage, Verbosity),

    Mibs    = ["Test2", "TestTrap", "TestTrapv2"],
    StdMibs = ["OTP-SNMPEA-MIB",
	       "SNMP-COMMUNITY-MIB",
	       "SNMP-FRAMEWORK-MIB",
	       "SNMP-MPD-MIB",
	       "SNMP-NOTIFICATION-MIB",
	       "SNMP-TARGET-MIB",
	       "SNMP-USER-BASED-SM-MIB",
	       "SNMP-VIEW-BASED-ACM-MIB",
	       "SNMPv2-MIB",
	       "SNMPv2-TC",
	       "SNMPv2-TM"],

    ?DBG("do_size_check -> load mibs", []),
    ?line load_mibs(MibsPid, MibDir, Mibs),
    ?DBG("do_size_check -> load std mibs", []),
    ?line load_mibs(MibsPid, StdMibDir, StdMibs),

    ?SLEEP(2000),
    ?DBG("do_size_check -> display mem usage", []),
    ?line display_memory_usage(MibsPid),
    
    ?DBG("do_size_check -> unload std mibs", []),
    ?line unload_mibs(MibsPid, StdMibs),
    ?DBG("do_size_check -> unload mibs", []),
    ?line unload_mibs(MibsPid, Mibs),

    ?DBG("do_size_check -> stop mib server", []),
    ?line mibs_stop(MibsPid),
    ?DBG("do_size_check -> stop symbolic store", []),
    ?line sym_stop(),

    ?DBG("do_size_check -> done", []),
    ok.


%% ---------------------------------------------------------------------

me_lookup(suite) -> [];
me_lookup(Config) when list(Config) ->
    Prio       = normal,
    Verbosity  = trace,
    %% MibStorage = ets,
    MibDir     = ?config(snmp_data_dir, Config),
    StdMibDir  = filename:join(code:priv_dir(snmp), "mibs") ++ "/",
    Mibs    = ["Test2", "TestTrap", "TestTrapv2"],
    StdMibs = ["OTP-SNMPEA-MIB",
	       "SNMP-COMMUNITY-MIB",
	       "SNMP-FRAMEWORK-MIB",
	       "SNMP-MPD-MIB",
	       "SNMP-NOTIFICATION-MIB",
	       "SNMP-TARGET-MIB",
	       %% "SNMP-USER-BASED-SM-MIB",
	       "SNMP-VIEW-BASED-ACM-MIB",
	       "SNMPv2-MIB",
	       "SNMPv2-TC",
	       "SNMPv2-TM"],

    ?DBG("me_lookup -> start symbolic store", []),
    ?line sym_start(Prio, Verbosity),

    ?DBG("me_lookup -> start mib server", []),
    ?line MibsPid = mibs_start(Prio, Verbosity),
    
    ?DBG("me_lookup -> load mibs", []),
    ?line load_mibs(MibsPid, MibDir, Mibs),
    ?DBG("me_lookup -> load std mibs", []),
    ?line load_mibs(MibsPid, StdMibDir, StdMibs),

    ?DBG("me_lookup -> find ~w from SNMP-COMMUNITY-MIB", 
	 [?snmpTrapCommunity_instance]),
    ?line ok = me_lookup(MibsPid, ?snmpTrapCommunity_instance),
    
    ?DBG("me_lookup -> find ~w from SNMP-VIEW-BASED-ACM-MIB", 
	 [?vacmViewSpinLock_instance]),
    ?line ok = me_lookup(MibsPid, ?vacmViewSpinLock_instance),
    
    ?DBG("me_lookup -> find ~w from SNMP-USER-BASED-SM-MIB", 
	 [?usmStatsNotInTimeWindows_instance]),
    ?line {error, _} = me_lookup(MibsPid, ?usmStatsNotInTimeWindows_instance),
    
    ?DBG("me_lookup -> stop mib server", []),
    ?line mibs_stop(MibsPid),

    ?DBG("me_lookup -> stop symbolic store", []),
    ?line sym_stop(),

    ok.


%% ---------------------------------------------------------------------

which_mib(suite) -> [];
which_mib(Config) when list(Config) ->
    Prio       = normal,
    Verbosity  = trace,
    %% MibStorage = ets,
    MibDir     = ?config(snmp_data_dir, Config),
    StdMibDir  = filename:join(code:priv_dir(snmp), "mibs") ++ "/",
    Mibs    = ["Test2", "TestTrap", "TestTrapv2"],
    StdMibs = ["OTP-SNMPEA-MIB",
	       "SNMP-COMMUNITY-MIB",
	       "SNMP-FRAMEWORK-MIB",
	       "SNMP-MPD-MIB",
	       "SNMP-NOTIFICATION-MIB",
	       "SNMP-TARGET-MIB",
	       %% "SNMP-USER-BASED-SM-MIB",
	       "SNMP-VIEW-BASED-ACM-MIB",
	       "SNMPv2-MIB",
	       "SNMPv2-TC",
	       "SNMPv2-TM"],

    ?DBG("which_mib -> start symbolic store", []),
    ?line sym_start(Prio, Verbosity),

    ?DBG("which_mib -> start mib server", []),
    ?line MibsPid = mibs_start(Prio, Verbosity),
    
    ?DBG("which_mib -> load mibs", []),
    ?line load_mibs(MibsPid, MibDir, Mibs),
    ?DBG("which_mib -> load std mibs", []),
    ?line load_mibs(MibsPid, StdMibDir, StdMibs),

    ?DBG("which_mib -> find ~w from SNMP-COMMUNITY-MIB", 
	 [?snmpTrapCommunity_instance]),
    ?line ok = which_mib(MibsPid, ?snmpTrapCommunity_instance, 
			 "SNMP-COMMUNITY-MIB"),
    
    ?DBG("which_mib -> find ~w from SNMP-VIEW-BASED-ACM-MIB", 
	 [?vacmViewSpinLock_instance]),
    ?line ok = which_mib(MibsPid, ?vacmViewSpinLock_instance, 
			 "SNMP-VIEW-BASED-ACM-MIB"),
    
    ?DBG("which_mib -> find ~w from SNMP-USER-BASED-SM-MIB (not loaded)", 
	 [?usmStatsNotInTimeWindows_instance]),
    ?line {error, _} = which_mib(MibsPid, ?usmStatsNotInTimeWindows_instance,
				"SNMP-USER-BASED-SM-MIB"),
    
    ?DBG("which_mib -> stop mib server", []),
    ?line mibs_stop(MibsPid),

    ?DBG("which_mib -> stop symbolic store", []),
    ?line sym_stop(),

    ok.


%%======================================================================
%% Internal functions
%%======================================================================

%% -- Mnesia functions

mnesia_start(Opts) ->
    mnesia_start(Opts, [node()]).

mnesia_start(Opts, Nodes) ->
    ?DBG("mnesia_start -> load mnesia", []),
    ?line ok = application:load(mnesia),
    F = fun({Key, Val}) ->
		?DBG("mnesia_start -> set mnesia env: ~n~p -> ~p", [Key,Val]),
		?line application_controller:set_env(mnesia, Key, Val)
	end,
    lists:foreach(F, Opts),
    ?DBG("mnesia_start -> create mnesia schema on ~p", [Nodes]),
    ?line ok = mnesia:create_schema(Nodes),
    ?DBG("mnesia_start -> start mnesia", []),
    ?line ok = application:start(mnesia),
    ok.

mnesia_stop() ->
    ?DBG("mnesia_stop -> stop mnesia", []),
    application:stop(mnesia),
    ?DBG("mnesia_stop -> unload mnesia", []),
    application:unload(mnesia),
    ok.
    
%% - Symbolic Store mini interface

sym_start(Prio, Verbosity) ->
    sym_start(Prio, ets, Verbosity).

sym_start(Prio, MibStorage, Verbosity) ->
    Opts = [{mib_storage, MibStorage}, {verbosity,Verbosity}],
    {ok, _Pid} = snmpa_symbolic_store:start_link(Prio, Opts),
    ok.

sym_stop() ->
    ok = snmpa_symbolic_store:stop().

sym_info() ->
    snmpa_symbolic_store:info().


%% -- MIB server mini interface 
		   
mibs_start(Prio, Verbosity) ->
    mibs_start(Prio, ets, [], Verbosity).

mibs_start(Prio, MibStorage, Verbosity) ->
    mibs_start(Prio, MibStorage, [], Verbosity).

mibs_start(Prio, MibStorage, Mibs, Verbosity) ->
    Opts = [{mib_storage, MibStorage}, {verbosity,Verbosity}],
    {ok, Pid} = snmpa_mib:start_link(Prio, Mibs, Opts),
    Pid.

mibs_stop(Pid) ->
    ok = snmpa_mib:stop(Pid).

mibs_info(Pid) ->
    snmpa_mib:info(Pid).

load_mibs(Pid, Dir, Mibs0) ->
    Mibs = [join(Dir, Mib) || Mib <- Mibs0],
    snmpa_mib:load_mibs(Pid, Mibs).

unload_mibs(Pid, Mibs) ->
    snmpa_mib:unload_mibs(Pid, Mibs).

verify_loaded_mibs(Pid, Dir, ExpectedMibs0) ->
    ExpectedMibs = [join(Dir, Mib) || Mib <- ExpectedMibs0],
    case snmpa_mib:info(Pid, loaded_mibs) of
	ExpectedMibs ->
	    ok;
	LoadedMibs0 ->
	    ?DBG("verify_loaded_mibs -> LoadedMibs0: ~p", [LoadedMibs0]),
	    LoadedMibs = [filename:rootname(FN, ".bin") || FN <- LoadedMibs0],
	    ?DBG("verify_loaded_mibs -> LoadedMibs: ~p", [LoadedMibs]),
	    ExpNotLoadedMibs = ExpectedMibs -- LoadedMibs,
	    LoadedNotExpMibs = LoadedMibs -- ExpectedMibs,
	    ?DBG("verify_loaded_mibs -> "
		 "~n   ExpNotLoadedMibs: ~p"
		 "~n   LoadedNotExpMibs: ~p", 
		 [ExpNotLoadedMibs, LoadedNotExpMibs]),
	    case ExpNotLoadedMibs of
		[] ->
		    case LoadedNotExpMibs of
			[] ->
			    ok;
			_ ->
			    {error, {unexpected_loaded_mibs, LoadedNotExpMibs}}
		    end;
		_ ->
		    case LoadedNotExpMibs of
			[] ->
			    {error, {not_loaded_mibs, ExpNotLoadedMibs}};
			_ ->
			    {error, {unexpected_mibs, 
				     ExpNotLoadedMibs, LoadedNotExpMibs}}
		    end
	    end
	
    end.

me_lookup(Pid, Oid) ->    
    case snmpa_mib:lookup(Pid, Oid) of
	{variable, #me{oid = Oid}} ->
	    ok;
	{variable, #me{oid = OtherOid}} ->
	    case lists:reverse(Oid) of
		[0|Rest] ->
		    case lists:reverse(Rest) of
			OtherOid ->
			    ok;
			AnotherOid ->
			    {error, {invalid_oid, Oid, AnotherOid}}
		    end;
		_ ->
		    {error, {invalid_oid, Oid, OtherOid}}
	    end;
	{table_column, _ME, _TableEntryOid} ->
            ok;
        {subagent, SubAgentPid, _SANextOid} ->
            {error, {subagent, SubAgentPid}};
        {false, Reason} ->
            {error, Reason};
        Else ->
            {error, Else}
    end.

			    
which_mib(Pid, Oid, Mib1) ->    
    case snmpa_mib:which_mib(Pid, Oid) of
	{ok, Mib2} when atom(Mib2) ->
	    Mib3 = atom_to_list(Mib2),
	    which_mib(Mib1, Mib3);
	{ok, Mib2} ->
	    which_mib(Mib1, Mib2);
        {error, Reason} ->
            {error, Reason};
        Else ->
            {error, Else}
    end.

which_mib(M, M) ->
    ok;
which_mib(M1, M2) ->
    {error, {invalid_mib, M1, M2}}.


%% -- 

display_memory_usage(MibsPid) ->
    SymInfo     = sym_info(),
    SymProcSize = key1search(process_memory, SymInfo),
    DbSize      = key1search(db_memory,      SymInfo),
    MibsInfo    = mibs_info(MibsPid),
    TreeSize    = key1search(tree_size_bytes,  MibsInfo),
    MibsProcMem = key1search(process_memory,   MibsInfo),
    MibDbSize   = key1search([db_memory,mib],  MibsInfo),
    NodeDbSize  = key1search([db_memory,node], MibsInfo),
    TreeDbSize  = key1search([db_memory,tree], MibsInfo),
    ?INF("Symbolic store memory usage: "
	"~n   Process memory size: ~p"
	"~n   Db size:             ~p"
	"~n"
	"~nMib server memory usage: "
	"~n   Tree size:           ~p"
	"~n   Process memory size: ~p"
	"~n   Mib db size:         ~p"
	"~n   Node db size:        ~p"
	"~n   Tree db size:        ~p"
	"~n", 
	[SymProcSize, DbSize,
	TreeSize, MibsProcMem, MibDbSize, NodeDbSize, TreeDbSize]).
    
key1search([], Res) ->
    Res;
key1search([Key|Keys], List) when atom(Key), list(List) ->
    case lists:keysearch(Key, 1, List) of
	{value, {Key, Val}} ->
	    key1search(Keys, Val);
	false ->
	    undefined
    end;
key1search(Key, List) when atom(Key) ->
    case lists:keysearch(Key, 1, List) of
	{value, {Key, Val}} ->
	    Val;
	false ->
	    undefined
    end.

join(Dir, File) ->
    filename:join(Dir, File).
