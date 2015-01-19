%%<copyright>
%% <year>2000-2008</year>
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
%% Purpose: Test application config
%%----------------------------------------------------------------------

-module(megaco_config_test).

-compile(export_all).

-include("megaco_test_lib.hrl").
-include_lib("megaco/include/megaco.hrl").
-include_lib("megaco/src/app/megaco_internal.hrl").

t()     -> megaco_test_lib:t(?MODULE).
t(Case) -> megaco_test_lib:t({?MODULE, Case}).

min(M) -> timer:minutes(M).

%% Test server callbacks
init_per_testcase(Case, Config) ->
    C = lists:keydelete(tc_timeout, 1, Config),
    do_init_per_testcase(Case, [{tc_timeout, min(3)}|C]).

do_init_per_testcase(Case, Config) ->
    process_flag(trap_exit, true),
    megaco_test_lib:init_per_testcase(Case, Config).

fin_per_testcase(Case, Config) ->
    process_flag(trap_exit, false),
    megaco_test_lib:fin_per_testcase(Case, Config).


-record(command, {id, desc, cmd, verify}).

-define(TEST_VERBOSITY, debug).
-define(NUM_CNT_PROCS,  100).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Top test case

all(suite) ->
    [
     config,
     transaction_id_counter,
     tickets
    ].

transaction_id_counter(suite) ->
    [
     transaction_id_counter_mg, 
     transaction_id_counter_mgc
    ].

tickets(suite) ->
    [
     otp_7216
    ].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Config test case

config(suite) ->
    [];
config(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Mid = fake_mid,
    
    %% Nice values
    Int = 3,
    IT  = #megaco_incr_timer{max_retries = Int},

    %% Evil values
    NonInt = non_int,
    IT2 = #megaco_incr_timer{wait_for = NonInt},
    IT3 = #megaco_incr_timer{factor = NonInt},
    IT4 = #megaco_incr_timer{max_retries = NonInt},
    IT5 = #megaco_incr_timer{max_retries = non_infinity},

    %% Command range values
    Initial = 100,
    Verify  = 200,
    Nice    = 300,
    Evil    = 400,
    End     = 500,

    Commands = 
	[
	 %% Initial commands
	 initial_command( Initial + 0, 
			  "enable trace", 
			  fun() -> megaco:enable_trace(100, io) end, ok),
	 initial_command( Initial + 1, 
			  "start", 
			  fun() -> megaco:start() end, ok),
	 initial_command( Initial + 2, 
			  "Verify no active requests", 
			  fun() -> megaco:system_info(n_active_requests) end,
			  0),
	 initial_command( Initial + 3, 
			  "Verify no active replies", 
			  fun() -> megaco:system_info(n_active_replies) end,
			  0),
	 initial_command( Initial + 4, 
			  "Verify no active connections", 
			  fun() -> 
				  megaco:system_info(n_active_connections) 
			  end,
			  0), 
	 initial_command( Initial + 5, 
			  "Verify no connections", 
			  fun() -> megaco:system_info(connections) end, []),
	 initial_command( Initial + 6, 
			  "Verify no users", 
			  fun() -> megaco:system_info(users) end, []), 
	 initial_command( Initial + 7, 
			  "Start user", 
			  fun() -> megaco:start_user(Mid, []) end, ok),


	 %% Verify user defaults
	 verify_user_default_command(Mid, Verify +  1, connections, []),
	 verify_user_default_command(Mid, Verify +  2, min_trans_id, 1), 
	 verify_user_default_command(Mid, Verify +  3, max_trans_id, infinity), 
	 verify_user_default_command(Mid, Verify +  4, request_timer, 
				          #megaco_incr_timer{}), 
	 verify_user_default_command(Mid, Verify +  5, long_request_timer, timer:seconds(60)), 
	 verify_user_default_command(Mid, Verify +  6, auto_ack, false), 
	 verify_user_default_command(Mid, Verify +  7, pending_timer, 30000), 
	 verify_user_default_command(Mid, Verify +  8, reply_timer, 30000), 
	 verify_user_default_command(Mid, Verify +  9, send_mod, megaco_tcp), 
	 verify_user_default_command(Mid, Verify + 10, encoding_mod, 
				          megaco_pretty_text_encoder), 
	 verify_user_default_command(Mid, Verify + 11, encoding_config, []), 
	 verify_user_default_command(Mid, Verify + 12, protocol_version, 1), 
	 verify_user_default_command(Mid, Verify + 13, reply_data, undefined), 
	 verify_user_default_command(Mid, Verify + 14, receive_handle, 
				     fun(H) when is_record(H, megaco_receive_handle) -> {ok, H};
					(R)  -> {error, R}
				     end), 


	 %% Nice update
	 nice_user_update_command(Mid, Nice +  1, min_trans_id, Int), 
	 nice_user_update_command(Mid, Nice +  2, max_trans_id, Int), 
	 nice_user_update_command(Mid, Nice +  3, max_trans_id, infinity), 
	 nice_user_update_command(Mid, Nice +  4, request_timer, Int), 
	 nice_user_update_command(Mid, Nice +  5, request_timer, infinity), 
	 nice_user_update_command(Mid, Nice +  6, request_timer, IT), 
	 nice_user_update_command(Mid, Nice +  7, long_request_timer, Int), 
	 nice_user_update_command(Mid, Nice +  8, long_request_timer, infinity), 
	 nice_user_update_command(Mid, Nice +  9, long_request_timer, IT), 
	 nice_user_update_command(Mid, Nice + 10, auto_ack, true), 
	 nice_user_update_command(Mid, Nice + 11, auto_ack, false), 
	 nice_user_update_command(Mid, Nice + 12, pending_timer, Int), 
	 nice_user_update_command(Mid, Nice + 13, pending_timer, infinity), 
	 nice_user_update_command(Mid, Nice + 14, pending_timer, IT), 
	 nice_user_update_command(Mid, Nice + 15, reply_timer, Int), 
	 nice_user_update_command(Mid, Nice + 16, reply_timer, infinity), 
	 nice_user_update_command(Mid, Nice + 17, reply_timer, IT), 
	 nice_user_update_command(Mid, Nice + 18, send_mod, an_atom), 
	 nice_user_update_command(Mid, Nice + 19, encoding_mod, an_atom), 
	 nice_user_update_command(Mid, Nice + 20, encoding_config, []), 
	 nice_user_update_command(Mid, Nice + 21, protocol_version, Int), 
	 nice_user_update_command(Mid, Nice + 23, reply_data, IT), 
	 nice_user_update_command(Mid, Nice + 23, resend_indication, true), 
	 nice_user_update_command(Mid, Nice + 24, resend_indication, false), 
	 nice_user_update_command(Mid, Nice + 25, resend_indication, flag),


	 %% Evil update
	 evil_user_update_command(Mid, Evil +  1, min_trans_id, NonInt), 
	 evil_user_update_command(Mid, Evil +  2, max_trans_id, NonInt), 
	 evil_user_update_command(Mid, Evil +  3, max_trans_id, non_infinity), 
	 evil_user_update_command(Mid, Evil +  4, request_timer, NonInt), 
	 evil_user_update_command(Mid, Evil +  5, request_timer, non_infinity), 
	 evil_user_update_command(Mid, Evil +  6, request_timer, IT2), 
	 evil_user_update_command(Mid, Evil +  7, request_timer, IT3), 
	 evil_user_update_command(Mid, Evil +  8, request_timer, IT4), 
	 evil_user_update_command(Mid, Evil +  9, request_timer, IT5), 
	 evil_user_update_command(Mid, Evil + 10, long_request_timer, NonInt), 
	 evil_user_update_command(Mid, Evil + 11, long_request_timer, non_infinity), 
	 evil_user_update_command(Mid, Evil + 12, long_request_timer, IT2), 
	 evil_user_update_command(Mid, Evil + 13, long_request_timer, IT3), 
	 evil_user_update_command(Mid, Evil + 14, long_request_timer, IT4), 
	 evil_user_update_command(Mid, Evil + 15, long_request_timer, IT5), 
	 evil_user_update_command(Mid, Evil + 16, auto_ack, non_bool), 
	 evil_user_update_command(Mid, Evil + 17, pending_timer, NonInt), 
	 evil_user_update_command(Mid, Evil + 18, pending_timer, non_infinity), 
	 evil_user_update_command(Mid, Evil + 19, pending_timer, IT2), 
	 evil_user_update_command(Mid, Evil + 20, pending_timer, IT3), 
	 evil_user_update_command(Mid, Evil + 21, pending_timer, IT4), 
	 evil_user_update_command(Mid, Evil + 22, pending_timer, IT5), 
	 evil_user_update_command(Mid, Evil + 23, reply_timer, NonInt), 
	 evil_user_update_command(Mid, Evil + 24, reply_timer, non_infinity), 
	 evil_user_update_command(Mid, Evil + 25, reply_timer, IT2), 
	 evil_user_update_command(Mid, Evil + 26, reply_timer, IT3), 
	 evil_user_update_command(Mid, Evil + 27, reply_timer, IT4), 
	 evil_user_update_command(Mid, Evil + 28, reply_timer, IT5), 
	 evil_user_update_command(Mid, Evil + 29, send_mod, {non_atom}), 
	 evil_user_update_command(Mid, Evil + 30, encoding_mod, {non_atom}), 
	 evil_user_update_command(Mid, Evil + 31, encoding_config, non_list), 
	 evil_user_update_command(Mid, Evil + 32, protocol_version, NonInt),
	 evil_user_update_command(Mid, Evil + 33, resend_indication, flagg),


	 exit_command(End + 1, 
		      "Verify non-existing system info", 
		      fun() -> megaco:system_info(non_exist) end),
	 exit_command(End + 2, 
		      "Verify non-existing user user info", 
		      fun() -> megaco:user_info(non_exist, trans_id) end),
	 exit_command(End + 3, "Verify non-existing user info", 
		      fun() -> megaco:user_info(Mid, non_exist) end),

	 error_command(End + 4, 
		       "Try updating user info for non-existing user", 
		       fun() -> 
			       megaco:update_user_info(non_exist, trans_id, 1) 
		       end,
		       no_such_user, 2),
	 error_command(End + 11, 
		       "Try updating non-existing user info", 
		       fun() -> 
			       megaco:update_user_info(Mid, trans_id, 4711) 
		       end,
		       bad_user_val, 4),
	 error_command(End + 12, 
		       "Try start already started user", 
		       fun() -> 
			       megaco:start_user(Mid, []) 
		       end,
		       user_already_exists, 2),

	 command(End + 13, "Verify started users", 
		 fun() -> megaco:system_info(users) end, [Mid]),
	 command(End + 14, "Stop user", fun() -> megaco:stop_user(Mid) end, ok),
	 command(End + 15, "Verify started users", 
		 fun() -> megaco:system_info(users) end, []),
	 error_command(End + 16, "Try stop not started user",
		       fun() -> megaco:stop_user(Mid) end, no_such_user, 2),
	 error_command(End + 17, "Try start megaco (it's already started)",
		       fun() -> megaco:start() end, already_started, 2),
	 command(End + 18, "Stop megaco", fun() -> megaco:stop() end, ok),
	 error_command(End + 19, "Try stop megaco (it's not running)",
		       fun() -> megaco:stop() end, not_started, 2)
	],

    
    exec(Commands).
    


exec([]) ->
    ok;
exec([#command{id     = No, 
	       desc   = Desc, 
	       cmd    = Cmd, 
	       verify = Verify}|Commands]) ->
    io:format("Executing command ~2w: ~s: ", [No, Desc]),
    case (catch Verify((catch Cmd()))) of
	{ok, OK} ->
	    io:format("ok => ~p~n", [OK]),
	    exec(Commands);
	{error, Reason} ->
	    io:format("error => ~p~n", [Reason]),
	    {error, {bad_result, No, Reason}};
	Error ->
	    io:format("exit => ~p~n", [Error]),
	    {error, {unexpected_result, No, Error}}
    end.

initial_command(No, Desc0, Cmd, VerifyVal) when is_function(Cmd) ->
    Desc = lists:flatten(io_lib:format("Initial - ~s", [Desc0])),
    command(No, Desc, Cmd, VerifyVal).
    
verify_user_default_command(Mid, No, Key, Verify) ->
    Desc = lists:flatten(io_lib:format("Defaults - Verify ~w", [Key])),
    Cmd = fun() -> megaco:user_info(Mid, Key) end,
    command(No, Desc, Cmd, Verify).
		     
nice_user_update_command(Mid, No, Key, Val) ->
    Desc = lists:flatten(io_lib:format("Nice - Update ~w", [Key])),
    Cmd = fun() -> megaco:update_user_info(Mid, Key, Val) end,
    Verify = fun(ok) ->
		     case (catch megaco:user_info(Mid, Key)) of
			 {'EXIT', R} ->
			     {error, {value_retreival_failed, R}};
			 Val ->
			     {ok, Val};
			 Invalid ->
			     {error, {value_update_failed, Val, Invalid}}
		     end;
		(R)  -> 
		     {error, R}
	     end,
    command(No, Desc, Cmd, Verify).


evil_user_update_command(Mid, No, Key, Val) ->
    Desc = lists:flatten(io_lib:format("Evil: Update ~w", [Key])),
    Cmd = fun() ->
		  case (catch megaco:user_info(Mid, Key)) of
		      {'EXIT', R} ->
			  {{error, {old_value_retreival_failed, R}}, 
			   ignore};
		      OldVal ->
			  {OldVal,
			   (catch megaco:update_user_info(Mid, Key, Val))}
		  end
	  end,
    Verify = fun({{error, _} = Error, ignore}) ->
		     Error;
		({OldVal, {error, {bad_user_val, _, _, _}}}) ->
		     case (catch megaco:user_info(Mid, Key)) of
			 {'EXIT', R} ->
			     {error, {value_retreival_failed, R}};
			 OldVal ->
			     {ok, OldVal};
			 Invalid ->
			     {error, {value_update_failed, OldVal, Invalid}}
		     end;
		(R) ->
		     {error, R}
	     end,
    command(No, Desc, Cmd, Verify).

exit_command(No, Desc, Cmd) when is_function(Cmd) ->    
    Verify = fun({'EXIT', _} = E) ->
		     {ok, E};
		(R) ->
		     {error, R}
	     end,
    command(No, Desc, Cmd, Verify).

error_command(No, Desc, Cmd, MainReason, TS) when is_function(Cmd) ->
    Verify = fun({error, Reason}) ->
		     io:format("verify -> Reason: ~n~p~n", [Reason]), 
		     case Reason of
			 {MainReason, _} when TS == 2 ->
			     {ok, MainReason};
			 {MainReason, _, _, _} when TS == 4 ->
			     {ok, MainReason};
			 _ ->
			     {error, Reason}
		     end;
		(R) ->
		     {error, R}
	     end,
    command(No, Desc, Cmd, Verify).

command(No, Desc, Cmd, Verify) when is_integer(No) and is_list(Desc) and 
                                    is_function(Cmd) and is_function(Verify) ->
    #command{id     = No, 
	     desc   = Desc,
	     cmd    = Cmd,
	     verify = Verify};
command(No, Desc, Cmd, VerifyVal) when is_integer(No) and is_list(Desc) and 
				       is_function(Cmd) ->
    Verify = fun(Val) ->
		     case Val of
			 VerifyVal ->
			     {ok, Val};
			 _ ->
			     {error, Val}
		     end
	     end,
    #command{id     = No, 
	     desc   = Desc,
	     cmd    = Cmd,
	     verify = Verify}.
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

transaction_id_counter_mg(suite) ->
    [];
transaction_id_counter_mg(doc) ->
    ["This test case is intended to test and verify the "
     "transaction counter handling of the application "
     "in with one connection (MG). "];
transaction_id_counter_mg(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        transaction_id_counter_mg),
    
    process_flag(trap_exit, true),

    i("starting"),

    {ok, _ConfigPid} = megaco_config:start_link(),

    %% Basic user data
    UserMid = {deviceName, "mg"},
    UserConfig = [
		  {min_trans_id, 1}
		 ],

    %% Basic connection data
    RemoteMid = {deviceName, "mgc"},
    RecvHandle = #megaco_receive_handle{local_mid       = UserMid,
					encoding_mod    = ?MODULE,
					encoding_config = [],
					send_mod        = ?MODULE},
    SendHandle = dummy_send_handle,
    ControlPid = self(), 

    %% Start user
    i("start user"),
    ok = megaco_config:start_user(UserMid, UserConfig),

    %% Create connection
    i("create connection"),
    {ok, CD} = 
	megaco_config:connect(RecvHandle, RemoteMid, SendHandle, ControlPid),

    %% Set counter limits
    i("set counter max limit"),
    CH = CD#conn_data.conn_handle, 
    megaco_config:update_conn_info(CH, max_trans_id, 1000),

    %% Create the counter worker procs
    i("create counter working procs"),
    Pids = create_counter_working_procs(CH, ?NUM_CNT_PROCS, []),

    %% Start the counter worker procs
    i("release the counter working procs"),
    start_counter_working_procs(Pids),

    %% Await the counter worker procs termination
    i("await the counter working procs completion"),
    await_completion_counter_working_procs(Pids),

    %% Verify result
    i("verify counter result"),
    TransId = megaco_config:conn_info(CH, trans_id),
    1 = TransId,

    %% Stop test
    i("disconnect"),
    {ok, _, _} = megaco_config:disconnect(CH),
    i("stop user"),
    ok = megaco_config:stop_user(UserMid),
    i("stop megaco_config"),
    ok = megaco_config:stop(),

    i("done"),
    ok.



create_counter_working_procs(_CH, 0, Pids) ->
    Pids;
create_counter_working_procs(CH, N, Pids) ->
    TC = get(tc),
    Pid = erlang:spawn_link(fun() -> counter_init(CH, TC) end),
    create_counter_working_procs(CH, N-1, [Pid | Pids]).

counter_init(CH, TC) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     lists:flatten(io_lib:format("CNT-~p", [self()]))),
    put(tc,        TC),    
    UserMid = CH#megaco_conn_handle.local_mid,
    Min = megaco_config:user_info(UserMid, min_trans_id),
    Max = megaco_config:conn_info(CH, max_trans_id),
    Num = Max - Min + 1,
    receive
	start ->
	    %% i("received start command (~p)", [Num]),
	    ok
    end,
    counter_loop(CH, Num).

counter_loop(_CH, 0) ->
    %% i("done"),
    exit(normal);
counter_loop(CH, Num) when (Num > 0) ->
    megaco_config:incr_trans_id_counter(CH, 1),
    counter_loop(CH, Num-1).

start_counter_working_procs([]) ->
    %% i("released"),
    ok;
start_counter_working_procs([Pid | Pids]) ->
    Pid ! start,
    start_counter_working_procs(Pids).

await_completion_counter_working_procs([]) ->
    ok;
await_completion_counter_working_procs(Pids) ->
    receive
	{'EXIT', Pid, normal} ->
	    Pids2 = lists:delete(Pid, Pids),
	    await_completion_counter_working_procs(Pids2);
	_Any ->
	    await_completion_counter_working_procs(Pids)
    end.
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

transaction_id_counter_mgc(suite) ->
    [];
transaction_id_counter_mgc(doc) ->
    ["This test case is intended to test and verify the "
     "transaction counter handling of the application "
     "in with several connections (MGC). "];
transaction_id_counter_mgc(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        transaction_id_counter_mgc),
    process_flag(trap_exit, true),

    i("starting"),

    {ok, _ConfigPid} = megaco_config:start_link(),

    %% Basic user data
    UserMid = {deviceName, "mgc"},
    UserConfig = [
		  {min_trans_id, 1}
		 ],

    %% Basic connection data
    RemoteMids = 
	[
	 {deviceName, "mg01"},
	 {deviceName, "mg02"},
	 {deviceName, "mg03"},
	 {deviceName, "mg04"},
	 {deviceName, "mg05"},
	 {deviceName, "mg06"},
	 {deviceName, "mg07"},
	 {deviceName, "mg08"},
	 {deviceName, "mg09"},
	 {deviceName, "mg10"}
	], 
    RecvHandles = 
	[
	 #megaco_receive_handle{local_mid     = UserMid,
	 			encoding_mod    = ?MODULE,
	 			encoding_config = [],
	 			send_mod        = ?MODULE},
	 #megaco_receive_handle{local_mid     = UserMid,
	 			encoding_mod    = ?MODULE,
	 			encoding_config = [],
	 			send_mod        = ?MODULE},
	 #megaco_receive_handle{local_mid     = UserMid,
	 			encoding_mod    = ?MODULE,
	 			encoding_config = [],
	 			send_mod        = ?MODULE},
	 #megaco_receive_handle{local_mid     = UserMid,
	 			encoding_mod    = ?MODULE,
	 			encoding_config = [],
	 			send_mod        = ?MODULE},
	 #megaco_receive_handle{local_mid     = UserMid,
	 			encoding_mod    = ?MODULE,
	 			encoding_config = [],
	 			send_mod        = ?MODULE},
	 #megaco_receive_handle{local_mid     = UserMid,
	 			encoding_mod    = ?MODULE,
	 			encoding_config = [],
	 			send_mod        = ?MODULE},
	 #megaco_receive_handle{local_mid     = UserMid,
	 			encoding_mod    = ?MODULE,
	 			encoding_config = [],
	 			send_mod        = ?MODULE},
	 #megaco_receive_handle{local_mid     = UserMid,
	 			encoding_mod    = ?MODULE,
	 			encoding_config = [],
	 			send_mod        = ?MODULE},
	 #megaco_receive_handle{local_mid     = UserMid,
	 			encoding_mod    = ?MODULE,
	 			encoding_config = [],
	 			send_mod        = ?MODULE},
	 #megaco_receive_handle{local_mid     = UserMid,
				encoding_mod    = ?MODULE,
				encoding_config = [],
				send_mod        = ?MODULE}
	],
    SendHandle = dummy_send_handle,
    ControlPid = self(), 
    
    %% Start user
    i("start user"),
    ok = megaco_config:start_user(UserMid, UserConfig),

    %% Create connection
    i("create connection(s)"),
    CDs = create_connections(RecvHandles, RemoteMids, SendHandle, ControlPid),

    %% Set counter limits
    i("set counter max limit(s)"),
    set_counter_max_limits(CDs, 1000),

    %% Create the counter worker procs
    i("create counter working procs"),
    Pids = create_counter_working_procs(CDs, ?NUM_CNT_PROCS),

    %% Start the counter worker procs
    i("release the counter working procs"),
    start_counter_working_procs(Pids),

    %% Await the counter worker procs termination
    i("await the counter working procs completion"),
    await_completion_counter_working_procs(Pids),

    %% Verify result
    i("verify counter result"),
    verify_counter_results(CDs),

    %% Stop test
    i("disconnect"),
    delete_connections(CDs), 
    i("stop user"),
    ok = megaco_config:stop_user(UserMid),
    i("stop megaco_config"),
    ok = megaco_config:stop(),

    i("done"),
    ok.

create_connections(RecvHandles, RemoteMids, SendHandle, ControlPid) ->
    create_connections(RecvHandles, RemoteMids, SendHandle, ControlPid, []).

create_connections([], [], _SendHandle, _ControlPid, Acc) ->
    lists:reverse(Acc);
create_connections([RecvHandle | RecvHandles], 
		   [RemoteMid  | RemoteMids], 
		   SendHandle, ControlPid, Acc) ->
    {ok, CD} = 
	megaco_config:connect(RecvHandle, RemoteMid, SendHandle, ControlPid),
    create_connections(RecvHandles, RemoteMids, 
		       SendHandle, ControlPid, [CD | Acc]).


set_counter_max_limits([], _MaxTransId) ->
    ok;
set_counter_max_limits([#conn_data{conn_handle = CH} | CDs], MaxTransId) ->
    megaco_config:update_conn_info(CH, max_trans_id, MaxTransId),
    set_counter_max_limits(CDs, MaxTransId).
    

create_counter_working_procs(CDs, NumCntProcs) ->
    lists:flatten(create_counter_working_procs2(CDs, NumCntProcs)).

create_counter_working_procs2([], _NumCntProcs) ->
    [];
create_counter_working_procs2([#conn_data{conn_handle = CH} | CDs], 
			      NumCntProcs) ->
    [create_counter_working_procs(CH, NumCntProcs, []) |
     create_counter_working_procs2(CDs, NumCntProcs)].


verify_counter_results([]) ->
    ok;
verify_counter_results([#conn_data{conn_handle = CH} | CDs]) ->
    TransId = megaco_config:conn_info(CH, trans_id),
    if
	(TransId =:= 1) ->
	    ok;
	true ->
	    ?ERROR({trans_id_verification_failed, CH, TransId})
    end,
    verify_counter_results(CDs).


delete_connections([]) ->
    ok;
delete_connections([#conn_data{conn_handle = CH} | CDs]) ->
    {ok, _, _} = megaco_config:disconnect(CH),
    delete_connections(CDs).

    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

otp_7216(suite) ->
    [];
otp_7216(Config) when list(Config) ->
    put(tc, otp_7216),
    p("start"),

    p("start the megaco config process"),
    megaco_config:start_link(),

    LocalMid1 = {deviceName, "local-mid-1"},
    %% LocalMid2 = {deviceName, "local-mid-2"},
    RemoteMid1 = {deviceName, "remote-mid-1"},
    %% RemoteMid2 = {deviceName, "remote-mid-2"},
    RH = #megaco_receive_handle{local_mid       = LocalMid1,
				encoding_mod    = dummy_codec_module,
				encoding_config = [],
				send_mod        = dummy_transport_module},
    MinTransId = 7216,
    MaxTransId = MinTransId + 10,
    User1Config = [{min_trans_id, MinTransId},
		   {max_trans_id, MaxTransId}], 

    VerifySerial = 
	fun(Actual, Expected) ->
		if
		    Actual == Expected ->
			ok;
		    true ->
			throw({error, {invalid_counter_value, Actual}})
		end
	end,

    p("start local user: ~p", [LocalMid1]),
    ok = megaco_config:start_user(LocalMid1, User1Config),

    p("connect"),
    {ok, CD} = megaco_config:connect(RH, RemoteMid1, 
				     dummy_send_handle, self()),
    p("connect ok: CD = ~n~p", [CD]),
    CH = CD#conn_data.conn_handle,

    
    p("*** make the first counter increment ***"),
    {ok, CD01} = megaco_config:incr_trans_id_counter(CH, 1),
    Serial01   = CD01#conn_data.serial,
    p("serial: ~p", [Serial01]),
    VerifySerial(Serial01, MinTransId),
    p("counter increment 1 ok"),    


    p("*** make two more counter increments ***"),
    {ok, _} = megaco_config:incr_trans_id_counter(CH, 1),
    {ok, CD02} = megaco_config:incr_trans_id_counter(CH, 1),
    Serial02   = CD02#conn_data.serial,
    p("serial: ~p", [Serial02]),
    VerifySerial(Serial02, MinTransId+2),
    p("counter increment 2 ok"), 

    
    p("*** make a big counter increment ***"),
    {ok, CD03} = megaco_config:incr_trans_id_counter(CH, 8),
    Serial03   = CD03#conn_data.serial,
    p("serial: ~p", [Serial03]),
    VerifySerial(Serial03, MinTransId+2+8),
    p("counter increment 3 ok"), 

    
    p("*** make a wrap-around counter increment ***"),
    {ok, CD04} = megaco_config:incr_trans_id_counter(CH, 1),
    Serial04   = CD04#conn_data.serial,
    p("serial: ~p", [Serial04]),
    VerifySerial(Serial04, MinTransId),
    p("counter increment 4 ok"), 


    p("*** make a big counter increment ***"),
    {ok, CD05} = megaco_config:incr_trans_id_counter(CH, 10),
    Serial05   = CD05#conn_data.serial,
    p("serial: ~p", [Serial05]),
    VerifySerial(Serial05, MinTransId+10),
    p("counter increment 5 ok"), 

    
    p("*** make a big wrap-around counter increment ***"),
    {ok, CD06} = megaco_config:incr_trans_id_counter(CH, 3),
    Serial06   = CD06#conn_data.serial,
    p("serial: ~p", [Serial06]),
    VerifySerial(Serial06, MinTransId+(3-1)),
    p("counter increment 6 ok"), 


    p("*** make a big counter increment ***"),
    {ok, CD07} = megaco_config:incr_trans_id_counter(CH, 7),
    Serial07   = CD07#conn_data.serial,
    p("serial: ~p", [Serial07]),
    VerifySerial(Serial07, MinTransId+(3-1)+7),
    p("counter increment 7 ok"), 


    p("*** make a big wrap-around counter increment ***"),
    {ok, CD08} = megaco_config:incr_trans_id_counter(CH, 5),
    Serial08   = CD08#conn_data.serial,
    p("serial: ~p", [Serial08]),
    VerifySerial(Serial08, MinTransId+(5-1-1)),
    p("counter increment 8 ok"), 


    p("disconnect"),
    {ok, CD, RCD} = megaco_config:disconnect(CH),
    p("disconnect ok: RCD = ~n~p", [RCD]),

    p("stop user"),
    ok = megaco_config:stop_user(LocalMid1),

    p("stop megaco config process"),
    megaco_config:stop(),

    p("done"),
    ok.


p(F) ->
    p(F, []).

p(F, A) ->
    io:format("[~w] " ++ F ++ "~n", [get(tc)|A]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

i(F) ->
    i(F, []).

i(F, A) ->
    print(info, get(verbosity), now(), get(tc), "INF", F, A).

printable(_, debug)   -> true;
printable(info, info) -> true;
printable(_,_)        -> false.

print(Severity, Verbosity, Ts, Tc, P, F, A) ->
    print(printable(Severity,Verbosity), Ts, Tc, P, F, A).

print(true, Ts, Tc, P, F, A) ->
    io:format("*** [~s] ~s ~p ~s:~w ***"
              "~n   " ++ F ++ "~n",
              [format_timestamp(Ts), P, self(), get(sname), Tc | A]);
print(_, _, _, _, _, _) ->
    ok.

format_timestamp({_N1, _N2, N3} = Now) ->
    {Date, Time}   = calendar:now_to_datetime(Now),
    {YYYY,MM,DD}   = Date,
    {Hour,Min,Sec} = Time,
    FormatDate =
        io_lib:format("~.4w:~.2.0w:~.2.0w ~.2.0w:~.2.0w:~.2.0w 4~w",
                      [YYYY,MM,DD,Hour,Min,Sec,round(N3/1000)]),
    lists:flatten(FormatDate).

