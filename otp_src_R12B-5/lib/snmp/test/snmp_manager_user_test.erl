%%<copyright>
%% <year>2004-2007</year>
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
%% Purpose: Various (snmp manager) user related tests
%%----------------------------------------------------------------------
-module(snmp_manager_user_test).


%%----------------------------------------------------------------------
%% Include files
%%----------------------------------------------------------------------
-include("test_server.hrl").
-include("snmp_test_lib.hrl").
 

%%----------------------------------------------------------------------
%% External exports
%%----------------------------------------------------------------------
%% -compile(export_all).

-export([
         all/1,
         init_per_testcase/2, fin_per_testcase/2,
	 
	 simple_register_and_unregister1/1,
	 simple_register_and_unregister2/1,
	 simple_register_and_unregister3/1,
	 register_and_crash1/1,
	 register_and_crash2/1,
	 register_and_crash3/1,
	 register_request_and_crash1/1,
	 register_request_and_crash2/1,
	 register_request_and_crash3/1,
	 simple_register_monitor_and_unregister1/1,
	 simple_register_monitor_and_unregister2/1,
	 simple_register_monitor_and_unregister3/1,
	 register_monitor_and_crash1/1, 
	 register_monitor_and_crash2/1, 
	 register_monitor_and_crash3/1, 
	 register_monitor_and_crash4/1, 
	 register_monitor_request_and_crash1/1,
	 register_monitor_request_and_crash2/1,
	 register_monitor_request_and_crash3/1,
	 register_monitor_request_and_crash4/1
	 
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

init_per_testcase(Case, Config) when list(Config) ->
    p("init_per_testcase -> Case: ~p", [Case]),
    SnmpPrivDir = ?config(priv_dir, Config),
    p("init_per_testcase -> SnmpPrivDir: ~p", [SnmpPrivDir]),
    SuiteDir = atom_to_list(?MODULE),
    SuiteTopDir = filename:join(SnmpPrivDir, SuiteDir),
    case file:make_dir(SuiteTopDir) of
	ok ->
	    ok;
	{error, eexist} ->
	    ok;
	{error, Reason} ->
	    ?FAIL({failed_creating, SuiteTopDir, Reason})
    end,
    p("init_per_testcase -> SuiteTopDir: ~p", [SuiteTopDir]),
    CaseDir = atom_to_list(Case),
    ?line ok =
        file:make_dir(CaseTopDir = filename:join(SuiteTopDir, CaseDir)),
    p("init_per_testcase -> CaseTopDir: ~p", [CaseTopDir]),
     ?line ok = 
	file:make_dir(MgrTopDir  = filename:join(CaseTopDir, "manager/")),
    ?line ok = 
	file:make_dir(MgrConfDir = filename:join(MgrTopDir,   "conf/")),
    ?line ok = 
	file:make_dir(MgrDbDir   = filename:join(MgrTopDir,   "db/")),
    ?line ok = 
	file:make_dir(MgrLogDir  = filename:join(MgrTopDir,   "log/")),
    [{suite_top_dir,    SuiteTopDir},
     {case_top_dir,     CaseTopDir},
     {manager_dir,      MgrTopDir},
     {manager_conf_dir, MgrConfDir},
     {manager_db_dir,   MgrDbDir},
     {manager_log_dir,  MgrLogDir} | Config].


fin_per_testcase(Case, Config) when list(Config) ->
    p("fin_per_testcase -> Case: ~p", [Case]),
%     MgrTopDir = ?config(manager_dir, Config),
%     ?DEL_DIR(MgrTopDir),
    Config.


%%======================================================================
%% Test case definitions
%%======================================================================

all(suite) ->
    [
     simple_register_and_unregister1,
     simple_register_and_unregister2,
     simple_register_and_unregister3,
     register_and_crash1,
     register_and_crash2,
     register_and_crash3,
     register_request_and_crash1,
     register_request_and_crash2,
     register_request_and_crash3,
     simple_register_monitor_and_unregister1,
     simple_register_monitor_and_unregister2,
     simple_register_monitor_and_unregister3,
     register_monitor_and_crash1,
     register_monitor_and_crash2,
     register_monitor_and_crash3,
     register_monitor_and_crash4,
     register_monitor_request_and_crash1,
     register_monitor_request_and_crash2,
     register_monitor_request_and_crash3,
     register_monitor_request_and_crash4
    ].


%%======================================================================
%% Test functions
%%======================================================================

simple_register_and_unregister1(suite) -> [];
simple_register_and_unregister1(doc) ->
    "Start a user, register and unregister the user.";
simple_register_and_unregister1(Conf) when list(Conf) ->
    put(tname,srar1),
    p("start"),
    process_flag(trap_exit, true),

    ConfDir = ?config(manager_conf_dir, Conf),
    DbDir = ?config(manager_db_dir, Conf),

    write_manager_conf(ConfDir),

    Opts = [{server, [{verbosity, trace}]},
            {net_if, [{verbosity, trace}]},
            {note_store, [{verbosity, trace}]},
            {config, [{verbosity, trace}, {dir, ConfDir}, {db_dir, DbDir}]}],
 
    p("try starting manager"),
    ok = snmpm:start_link(Opts),
 
    ?SLEEP(1000),

    Id = make_ref(), 

    ?line Pid = start_user(),

    ?line [] = Users1 = which_users(),
    p("Users1: ~p", [Users1]),
    
    ?line ok = register_user(Pid, Id),

    ?line [Id] = Users2 = which_users(),
    p("Users2: ~p", [Users2]),
    
    ?line ok = unregister_user(Pid),

    ?line [] = Users3 = which_users(),
    p("Users3: ~p", [Users3]),
    
    ?line stop_user(Pid),

    ?line ok = snmpm:stop(),

    p("end"),
    ok.


%% ------------------------------------------------------------------

simple_register_and_unregister2(suite) -> [];
simple_register_and_unregister2(doc) ->
    "Start a single user process, "
	"register 2 users (per that process) and unregister the 2 users.";
simple_register_and_unregister2(Conf) when list(Conf) ->
    put(tname,srar2),
    p("start"),
    process_flag(trap_exit, true),

    ConfDir = ?config(manager_conf_dir, Conf),
    DbDir = ?config(manager_db_dir, Conf),

    write_manager_conf(ConfDir),

    Opts = [{server, [{verbosity, trace}]},
            {net_if, [{verbosity, trace}]},
            {note_store, [{verbosity, trace}]},
            {config, [{verbosity, trace}, {dir, ConfDir}, {db_dir, DbDir}]}],
 
    p("try starting manager"),
    ok = snmpm:start_link(Opts),
 
    ?SLEEP(1000),

    Id1 = make_ref(), 
    Id2 = make_ref(), 

    ?line Pid = start_user(),

    ?line [] = Users1 = which_users(),
    p("Users1: ~p", [Users1]),
    
    ?line ok = register_user(Pid, Id1),
    ?line ok = register_user(Pid, Id2),

    ?line Users2 = case which_users() of
		       [Id1, Id2] = U1 ->
			   U1;
		       [Id2, Id1] = U2 ->
			   U2;
		       Else ->
			   ?FAIL({invalid_users, Else})
		   end,
    p("Users2: ~p", [Users2]),

    
    ?line ok = unregister_user(Pid, Id1),
    ?line ok = unregister_user(Pid, Id2),

    ?line [] = Users3 = which_users(),
    p("Users3: ~p", [Users3]),
    
    ?line stop_user(Pid),

    ?line ok = snmpm:stop(),

    p("end"),
    ok.


%% ------------------------------------------------------------------

simple_register_and_unregister3(suite) -> [];
simple_register_and_unregister3(doc) ->
    "Start 2 user processes, "
	"register one users per process and unregister the 2 users.";
simple_register_and_unregister3(Conf) when list(Conf) ->
    put(tname,srar2),
    p("start"),
    process_flag(trap_exit, true),

    ConfDir = ?config(manager_conf_dir, Conf),
    DbDir = ?config(manager_db_dir, Conf),

    write_manager_conf(ConfDir),

    Opts = [{server, [{verbosity, trace}]},
            {net_if, [{verbosity, trace}]},
            {note_store, [{verbosity, trace}]},
            {config, [{verbosity, trace}, {dir, ConfDir}, {db_dir, DbDir}]}],
 
    p("try starting manager"),
    ok = snmpm:start_link(Opts),
 
    ?SLEEP(1000),

    Id1 = make_ref(), 
    Id2 = make_ref(), 

    ?line Pid1 = start_user(),
    ?line Pid2 = start_user(),

    ?line [] = Users1 = which_users(),
    p("Users1: ~p", [Users1]),
    
    ?line ok = register_user(Pid1, Id1),
    ?line ok = register_user(Pid2, Id2),

    ?line Users2 = case which_users() of
		       [Id1, Id2] = U1 ->
			   U1;
		       [Id2, Id1] = U2 ->
			   U2;
		       Else ->
			   ?FAIL({invalid_users, Else})
		   end,
    p("Users2: ~p", [Users2]),

    
    ?line ok = unregister_user(Pid1, Id1),
    ?line ok = unregister_user(Pid2, Id2),

    ?line [] = Users3 = which_users(),
    p("Users3: ~p", [Users3]),
    
    ?line stop_user(Pid1),
    ?line stop_user(Pid2),

    ?line ok = snmpm:stop(),

    p("end"),
    ok.


%% ------------------------------------------------------------------

register_and_crash1(suite) -> [];
register_and_crash1(doc) ->
    "Start a user, register and crash user.";
register_and_crash1(Conf) when list(Conf) ->
    put(tname,racau1),
    p("start"),
    process_flag(trap_exit, true),

    ConfDir = ?config(manager_conf_dir, Conf),
    DbDir = ?config(manager_db_dir, Conf),

    write_manager_conf(ConfDir),

    Opts = [{server, [{verbosity, trace}]},
            {net_if, [{verbosity, trace}]},
            {note_store, [{verbosity, trace}]},
            {config, [{verbosity, trace}, {dir, ConfDir}, {db_dir, DbDir}]}],
 
    p("try starting manager"),
    ?line ok = snmpm:start_link(Opts),
 
    ?SLEEP(1000),

    Id = make_ref(), 

    ?line Pid = start_user(),

    ?line [] = Users1 = which_users(),
    p("Users1: ~p", [Users1]),
    
    ?line ok = register_user(Pid, Id),

    ?line [Id] = Users2 = which_users(),
    p("Users2: ~p", [Users2]),

    ?line ok = simulate_crash(Pid),

    ?line [Id] = Users3 = which_users(),
    p("Users3: ~p", [Users3]),
    
    p("stop manager"),
    ?line ok = snmpm:stop(),

    p("end"),
    ok.


%% ------------------------------------------------------------------

register_and_crash2(suite) -> [];
register_and_crash2(doc) ->
    "Start a single user process, "
	"register 2 users (per that process) and crash the process.";
register_and_crash2(Conf) when list(Conf) ->
    put(tname,racau2),
    p("start"),
    process_flag(trap_exit, true),

    ConfDir = ?config(manager_conf_dir, Conf),
    DbDir = ?config(manager_db_dir, Conf),

    write_manager_conf(ConfDir),

    Opts = [{server, [{verbosity, trace}]},
            {net_if, [{verbosity, trace}]},
            {note_store, [{verbosity, trace}]},
            {config, [{verbosity, trace}, {dir, ConfDir}, {db_dir, DbDir}]}],
 
    p("try starting manager"),
    ?line ok = snmpm:start_link(Opts),
 
    ?SLEEP(1000),

    Id1 = make_ref(), 
    Id2 = make_ref(), 

    ?line Pid = start_user(),

    ?line [] = Users1 = which_users(),
    p("Users1: ~p", [Users1]),
    
    ?line ok = register_user(Pid, Id1),
    ?line ok = register_user(Pid, Id2),

    ?line Users2 = case which_users() of
		       [Id1, Id2] = U1 ->
			   U1;
		       [Id2, Id1] = U2 ->
			   U2;
		       Else1 ->
			   ?FAIL({invalid_users, Else1})
		   end,
    p("Users2: ~p", [Users2]),

    ?line ok = simulate_crash(Pid),

    ?line Users3 = case which_users() of
		       [Id1, Id2] = U3 ->
			   U3;
		       [Id2, Id1] = U4 ->
			   U4;
		       Else2 ->
			   ?FAIL({invalid_users, Else2})
		   end,
    p("Users3: ~p", [Users3]),
    
    p("stop manager"),
    ?line ok = snmpm:stop(),

    p("end"),
    ok.


%% ------------------------------------------------------------------

register_and_crash3(suite) -> [];
register_and_crash3(doc) ->
    "Start 2 user processes, "
	"register one user per process and "
	"crash the first user process.";
register_and_crash3(Conf) when list(Conf) ->
    %%     put(tname,rac3),
    %%     p("start"),
    %%     process_flag(trap_exit, true),
    ?SKIP(not_yet_implemented).


%% ------------------------------------------------------------------

register_request_and_crash1(suite) -> [];
register_request_and_crash1(doc) ->
    "Start a single user process, "
	"register user, send request and crash user.";
register_request_and_crash1(Conf) when list(Conf) ->
    %%     put(tname,rrac1),
    %%     p("start"),
    %%     process_flag(trap_exit, true),
    ?SKIP(not_yet_implemented).


%% ------------------------------------------------------------------

register_request_and_crash2(suite) -> [];
register_request_and_crash2(doc) ->
    "Start a single user process, "
	"register 2 users (per that single user process), "
	"send a request for each user and crash the single user process.";
register_request_and_crash2(Conf) when list(Conf) ->
    %%     put(tname,rrac2),
    %%     p("start"),
    %%     process_flag(trap_exit, true),
    ?SKIP(not_yet_implemented).


%% ------------------------------------------------------------------

register_request_and_crash3(suite) -> [];
register_request_and_crash3(doc) ->
    "Start 2 user processes, "
	"register one user per process, "
	"send a request for each user and crash the first user process.";
register_request_and_crash3(Conf) when list(Conf) ->
    %%     put(tname,rrac3),
    %%     p("start"),
    %%     process_flag(trap_exit, true),
    ?SKIP(not_yet_implemented).


%% ------------------------------------------------------------------

simple_register_monitor_and_unregister1(suite) -> [];
simple_register_monitor_and_unregister1(doc) ->
    "Start a user, register-link and unregister the user.";
simple_register_monitor_and_unregister1(Conf) when list(Conf) ->
    put(tname,srlau1),
    p("start"),
    process_flag(trap_exit, true),

    ConfDir = ?config(manager_conf_dir, Conf),
    DbDir = ?config(manager_db_dir, Conf),

    write_manager_conf(ConfDir),

    Opts = [{server, [{verbosity, trace}]},
            {net_if, [{verbosity, trace}]},
            {note_store, [{verbosity, trace}]},
            {config, [{verbosity, trace}, {dir, ConfDir}, {db_dir, DbDir}]}],
 
    p("try starting manager"),
    ok = snmpm:start_link(Opts),
 
    ?SLEEP(1000),

    Id = make_ref(), 

    ?line Pid = start_user(),

    ?line [] = Users1 = which_users(),
    p("Users1: ~p", [Users1]),
    
    ?line ok = register_user_monitor(Pid, Id),

    ?line [Id] = Users2 = which_users(),
    p("Users2: ~p", [Users2]),
    
    ?line unregister_user(Pid),

    ?line [] = Users3 = which_users(),
    p("Users3: ~p", [Users3]),
    
    ?line stop_user(Pid),

    ?line ok = snmpm:stop(),

    p("end"),
    ok.


%% ------------------------------------------------------------------

simple_register_monitor_and_unregister2(suite) -> [];
simple_register_monitor_and_unregister2(doc) ->
    "Start a single user process, "
	"register-link 2 users (per that process) and "
	"unregister the 2 users.";
simple_register_monitor_and_unregister2(Conf) when list(Conf) ->
    put(tname,srlau2),
    p("start"),
    process_flag(trap_exit, true),

    ConfDir = ?config(manager_conf_dir, Conf),
    DbDir = ?config(manager_db_dir, Conf),

    write_manager_conf(ConfDir),

    Opts = [{server, [{verbosity, trace}]},
            {net_if, [{verbosity, trace}]},
            {note_store, [{verbosity, trace}]},
            {config, [{verbosity, trace}, {dir, ConfDir}, {db_dir, DbDir}]}],
 
    p("try starting manager"),
    ok = snmpm:start_link(Opts),
 
    ?SLEEP(1000),

    Id1 = make_ref(), 
    Id2 = make_ref(), 

    ?line Pid = start_user(),

    ?line [] = Users1 = which_users(),
    p("Users1: ~p", [Users1]),
    
    ?line ok = register_user_monitor(Pid, Id1),
    ?line ok = register_user_monitor(Pid, Id2),

    ?line Users2 = case which_users() of
		       [Id1, Id2] = U1 ->
			   U1;
		       [Id2, Id1] = U2 ->
			   U2;
		       Else ->
			   ?FAIL({invalid_users, Else})
		   end,
    p("Users2: ~p", [Users2]),
    
    ?line ok = unregister_user(Pid, Id1),
    ?line ok = unregister_user(Pid, Id2),

    ?line [] = Users3 = which_users(),
    p("Users3: ~p", [Users3]),
    
    ?line stop_user(Pid),

    ?line ok = snmpm:stop(),

    p("end"),
    ok.


%% ------------------------------------------------------------------

simple_register_monitor_and_unregister3(suite) -> [];
simple_register_monitor_and_unregister3(doc) ->
    "Start a single user process, "
	"register one user and register-monitor one user "
	"(per that process) and "
	"unregister the 2 users.";
simple_register_monitor_and_unregister3(Conf) when list(Conf) ->
    put(tname,srlau3),
    p("start"),
    process_flag(trap_exit, true),

    ConfDir = ?config(manager_conf_dir, Conf),
    DbDir = ?config(manager_db_dir, Conf),

    write_manager_conf(ConfDir),

    Opts = [{server, [{verbosity, trace}]},
            {net_if, [{verbosity, trace}]},
            {note_store, [{verbosity, trace}]},
            {config, [{verbosity, trace}, {dir, ConfDir}, {db_dir, DbDir}]}],
 
    p("try starting manager"),
    ok = snmpm:start_link(Opts),
 
    ?SLEEP(1000),

    Id1 = make_ref(), 
    Id2 = make_ref(), 

    ?line Pid = start_user(),

    ?line [] = Users1 = which_users(),
    p("Users1: ~p", [Users1]),
    
    ?line ok = register_user(Pid, Id1),
    ?line ok = register_user_monitor(Pid, Id2),

    ?line Users2 = case which_users() of
		       [Id1, Id2] = U1 ->
			   U1;
		       [Id2, Id1] = U2 ->
			   U2;
		       Else ->
			   ?FAIL({invalid_users, Else})
		   end,
    p("Users2: ~p", [Users2]),
    
    ?line unregister_user(Pid),

    ?line [] = Users3 = which_users(),
    p("Users3: ~p", [Users3]),
    
    ?line stop_user(Pid),

    ?line ok = snmpm:stop(),

    p("end"),
    ok.


%% ------------------------------------------------------------------

register_monitor_and_crash1(suite) -> [];
register_monitor_and_crash1(doc) ->
    "Start a user, register-monitor and crash the user.";
register_monitor_and_crash1(Conf) when list(Conf) ->
    put(tname,rlac1),
    p("start"),
    process_flag(trap_exit, true),

    ConfDir = ?config(manager_conf_dir, Conf),
    DbDir = ?config(manager_db_dir, Conf),

    write_manager_conf(ConfDir),

    Opts = [{server, [{verbosity, trace}]},
            {net_if, [{verbosity, trace}]},
            {note_store, [{verbosity, trace}]},
            {config, [{verbosity, trace}, {dir, ConfDir}, {db_dir, DbDir}]}],
 
    p("try starting manager"),
    ?line ok = snmpm:start_link(Opts),
 
    ?SLEEP(1000),

    Id = make_ref(), 

    ?line Pid = start_user(),

    ?line [] = Users1 = which_users(),
    p("Users1: ~p", [Users1]),
    
    ?line ok = register_user_monitor(Pid, Id),

    ?line [Id] = Users2 = which_users(),
    p("Users2: ~p", [Users2]),

    ?line ok = simulate_crash(Pid),

    ?SLEEP(1000),

    ?line [] = Users3 = which_users(),
    p("Users3: ~p", [Users3]),
    
    p("stop manager"),
    ?line ok = snmpm:stop(),

    p("end"),
    ok.


%% ------------------------------------------------------------------

register_monitor_and_crash2(suite) -> [];
register_monitor_and_crash2(doc) ->
    "Start a single user process, "
	"register-monitor 2 users (per that process) "
	"and crash the single user process.";
register_monitor_and_crash2(Conf) when list(Conf) ->
    put(tname,rlac2),
    p("start"),
    process_flag(trap_exit, true),

    ConfDir = ?config(manager_conf_dir, Conf),
    DbDir = ?config(manager_db_dir, Conf),

    write_manager_conf(ConfDir),

    Opts = [{server, [{verbosity, trace}]},
            {net_if, [{verbosity, trace}]},
            {note_store, [{verbosity, trace}]},
            {config, [{verbosity, trace}, {dir, ConfDir}, {db_dir, DbDir}]}],
 
    p("try starting manager"),
    ?line ok = snmpm:start_link(Opts),
 
    ?SLEEP(1000),

    Id1 = make_ref(), 
    Id2 = make_ref(), 

    ?line Pid = start_user(),

    ?line [] = Users1 = which_users(),
    p("Users1: ~p", [Users1]),
    
    ?line ok = register_user_monitor(Pid, Id1),
    ?line ok = register_user_monitor(Pid, Id2),

    ?line Users2 = case which_users() of
		       [Id1, Id2] = U1 ->
			   U1;
		       [Id2, Id1] = U2 ->
			   U2;
		       Else ->
			   ?FAIL({invalid_users, Else})
		   end,
    p("Users2: ~p", [Users2]),

    ?line ok = simulate_crash(Pid),

    ?SLEEP(1000),

    ?line [] = Users3 = which_users(),
    p("Users3: ~p", [Users3]),
    
    p("stop manager"),
    ?line ok = snmpm:stop(),

    p("end"),
    ok.


%% ------------------------------------------------------------------

register_monitor_and_crash3(suite) -> [];
register_monitor_and_crash3(doc) ->
    "Start a single user process, "
	"register-monitor one user and register one user, "
	"crash the single user process.";
register_monitor_and_crash3(Conf) when list(Conf) ->
    put(tname,rlac3),
    p("start"),
    process_flag(trap_exit, true),

    ConfDir = ?config(manager_conf_dir, Conf),
    DbDir = ?config(manager_db_dir, Conf),

    write_manager_conf(ConfDir),

    Opts = [{server, [{verbosity, trace}]},
            {net_if, [{verbosity, trace}]},
            {note_store, [{verbosity, trace}]},
            {config, [{verbosity, trace}, {dir, ConfDir}, {db_dir, DbDir}]}],
 
    p("try starting manager"),
    ?line ok = snmpm:start_link(Opts),
 
    ?SLEEP(1000),

    Id1 = make_ref(), 
    Id2 = make_ref(), 

    ?line Pid = start_user(),

    ?line [] = Users1 = which_users(),
    p("Users1: ~p", [Users1]),
    
    ?line ok = register_user(Pid, Id1),
    ?line ok = register_user_monitor(Pid, Id2),

    ?line Users2 = case which_users() of
		       [Id1, Id2] = U1 ->
			   U1;
		       [Id2, Id1] = U2 ->
			   U2;
		       Else ->
			   ?FAIL({invalid_users, Else})
		   end,
    p("Users2: ~p", [Users2]),

    ?line ok = simulate_crash(Pid),

    ?SLEEP(1000),

    ?line [Id1] = Users3 = which_users(),
    p("Users3: ~p", [Users3]),
    
    p("stop manager"),
    ?line ok = snmpm:stop(),

    p("end"),
    ok.


%% ------------------------------------------------------------------

register_monitor_and_crash4(suite) -> [];
register_monitor_and_crash4(doc) ->
    "Start 2 user processes, "
	"register-monitor one user per process "
	"and crash the first user process.";
register_monitor_and_crash4(Conf) when list(Conf) ->
    put(tname,rlac4),
    p("start"),
    process_flag(trap_exit, true),

    ConfDir = ?config(manager_conf_dir, Conf),
    DbDir = ?config(manager_db_dir, Conf),

    write_manager_conf(ConfDir),

    Opts = [{server, [{verbosity, trace}]},
            {net_if, [{verbosity, trace}]},
            {note_store, [{verbosity, trace}]},
            {config, [{verbosity, trace}, {dir, ConfDir}, {db_dir, DbDir}]}],
 
    p("start manager"),
    ?line ok = snmpm:start_link(Opts),
 
    ?SLEEP(1000),

    Id1 = make_ref(), 
    Id2 = make_ref(), 

    p("start user processes"),
    ?line Pid1 = start_user(),
    ?line Pid2 = start_user(),

    ?line [] = Users1 = which_users(),
    p("Users1: ~p", [Users1]),
    
    ?line ok = register_user_monitor(Pid1, Id1),
    ?line ok = register_user_monitor(Pid2, Id2),

    ?line Users2 = case which_users() of
		       [Id1, Id2] = U1 ->
			   U1;
		       [Id2, Id1] = U2 ->
			   U2;
		       Else ->
			   ?FAIL({invalid_users, Else})
		   end,
    p("Users2: ~p", [Users2]),

    ?line ok = simulate_crash(Pid1),

    ?SLEEP(1000),

    ?line [Id2] = Users3 = which_users(),
    p("Users3: ~p", [Users3]),
    
    ?line stop_user(Pid2),

    p("stop manager"),
    ?line ok = snmpm:stop(),

    p("end"),
    ok.


%% ------------------------------------------------------------------

register_monitor_request_and_crash1(suite) -> [];
register_monitor_request_and_crash1(doc) ->
    "Start a single user process, "
	"register-monitor one user, "
	"send request and crash the user.";
register_monitor_request_and_crash1(Conf) when list(Conf) ->
    %% put(tname,rlrac1),
    %% p("start"),
    %% process_flag(trap_exit, true),
    ?SKIP(not_yet_implemented).


%% ------------------------------------------------------------------

register_monitor_request_and_crash2(suite) -> [];
register_monitor_request_and_crash2(doc) ->
    "Start a single user process, "
	"register-monitor 2 user (per that one process), "
	"send a request for each user and crash the single user process.";
register_monitor_request_and_crash2(Conf) when list(Conf) ->
    %% put(tname,rlrac2),
    %% p("start"),
    %% process_flag(trap_exit, true),
    ?SKIP(not_yet_implemented).


%% ------------------------------------------------------------------

register_monitor_request_and_crash3(suite) -> [];
register_monitor_request_and_crash3(doc) ->
    "Start a single user process, "
	"register-monitor one user and register one user, "
	"send a request for each user and crash the single user process.";
register_monitor_request_and_crash3(Conf) when list(Conf) ->
    %% put(tname,rlrac3),
    %% p("start"),
    %% process_flag(trap_exit, true),
    ?SKIP(not_yet_implemented).


%% ------------------------------------------------------------------

register_monitor_request_and_crash4(suite) -> [];
register_monitor_request_and_crash4(doc) ->
    "Start 2 user processes, "
	"register-monitor one user and register one user on the "
	"first user process and do the same for the other user process, "
	"then for each user, send a request and "
	"crash the first user process.";
register_monitor_request_and_crash4(Conf) when list(Conf) ->
    %% put(tname,rlrac4),
    %% p("start"),
    %% process_flag(trap_exit, true),
    ?SKIP(not_yet_implemented).


%% ------------------------------------------------------------------

start_user() ->
    {ok, Pid} = snmp_manager_user_test_lib:start_link(),
    Pid.

stop_user(Pid) ->
    snmp_manager_user_test_lib:stop(Pid).

simulate_crash(Pid) ->
    snmp_manager_user_test_lib:simulate_crash(Pid, simulate_crash),
    receive
	{'EXIT', Pid, simulate_crash} ->
	    ok;
	{'EXIT', Pid, Whatever} ->
	    {ok, Whatever}
    after 5000 ->
	    {error, timeout}
    end.

register_user(Pid, Id) ->
    snmp_manager_user_test_lib:register(Pid, Id).

register_user_monitor(Pid, Id) ->
    snmp_manager_user_test_lib:register_monitor(Pid, Id).

unregister_user(Pid) ->
    case snmp_manager_user_test_lib:unregister(Pid) of
	{ok, Res} ->
	    case [R || R <- Res, R =/= ok] of
		[] ->
		    ok;
		Errs ->
		    {error, Errs}
	    end;
	Error ->
	    Error
    end.

unregister_user(Pid, Id) ->
    snmp_manager_user_test_lib:unregister(Pid, Id).


%% ------

which_users() ->
    snmpm:which_users().


%% ------


write_manager_conf(Dir) ->
    Port = "5000",
    MMS  = "484",
    EngineID = "\"mgrEngine\"",
    Str = lists:flatten(
            io_lib:format("%% Minimum manager config file\n"
                          "{port,             ~s}.\n"
                          "{max_message_size, ~s}.\n"
                          "{engine_id,        ~s}.\n",
                          [Port, MMS, EngineID])),
    write_manager_conf(Dir, Str).

write_manager_conf(Dir, Str) ->
    write_conf_file(Dir, "manager.conf", Str).


write_conf_file(Dir, File, Str) ->
    ?line {ok, Fd} = file:open(filename:join(Dir, File), write),
    ?line ok = io:format(Fd, "~s", [Str]),
    file:close(Fd).


%% ------

p(F) ->
    p(F, []).
 
p(F, A) ->
    p(get(tname), F, A).
 
p(TName, F, A) ->
    io:format("~w -> " ++ F ++ "~n", [TName|A]).

