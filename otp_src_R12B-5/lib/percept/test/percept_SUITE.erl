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
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id$
%%
-module(percept_SUITE).
-include("test_server.hrl").

%% Test server specific exports
-export([all/1]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([
	profile/1,
	analyze/1,
	analyze_dist/1,
	webserver/1
	]).

%% Default timetrap timeout (set in init_per_testcase)
-define(default_timeout, ?t:minutes(2)).

init_per_suite(Config) when is_list(Config) ->
    Config.

end_per_suite(Config) when is_list(Config) ->
    Config.

init_per_testcase(_Case, Config) ->
    Dog = ?t:timetrap(?default_timeout),
    [{max_size, 300}, {watchdog,Dog} | Config].

end_per_testcase(_Case, Config) ->
    Dog = ?config(watchdog, Config),
    ?t:timetrap_cancel(Dog),
    ok.

all(suite) ->
    % Test cases
    [	webserver,
	profile,
	analyze,
	analyze_dist].

%%----------------------------------------------------------------------
%% Tests
%%----------------------------------------------------------------------

webserver(suite) ->
    [];
webserver(doc) ->
    ["Percept webserver test."];
webserver(Config) when is_list(Config) ->
    % Explicit start inets?
    ?line {started, _, Port} = percept:start_webserver(),
    ?line ok = percept:stop_webserver(Port), 
    ?line application:stop(inets),
    ok.

profile(suite) ->
    [];
profile(doc) ->
    ["Percept profile test."];
profile(Config) when is_list(Config) ->
    Path = ?config(data_dir, Config),
    File = filename:join([Path,"profile_test.dat"]),
    ?line {ok, _} = percept:profile(File, [procs]),
    ipc_tree:go(7),
    ?line ok = percept:stop_profile(),
    ok.

analyze(suite) ->
    [];
analyze(doc) ->
    ["Percept analyze test."];
analyze(Config) when is_list(Config) ->
    Begin = processes(),
    Path = ?config(data_dir, Config),
    File = filename:join([Path,"profile_test.dat"]),
    T0 = erlang:now(),
    ?line ok = percept:analyze(File),
    T1 = erlang:now(),
    Secs = timer:now_diff(T1,T0)/1000000,
    io:format("percept:analyze/1 took ~.2f s.~n", [Secs]),
    ?line {stopped, _} = percept_db:stop(),
    print_remainers(remainers(Begin, processes())),
    ok.

analyze_dist(suite) ->
    [];
analyze_dist(doc) ->
    ["Percept analyze distribution test."];
analyze_dist(Config) when is_list(Config) ->
    Begin = processes(),
    Path = ?config(data_dir, Config),
    File = filename:join([Path,"ipc-dist.dat"]),
    T0 = erlang:now(),
    ?line ok = percept:analyze(File),
    T1 = erlang:now(),
    Secs = timer:now_diff(T1,T0)/1000000,
    io:format("percept:analyze/1 took ~.2f s.~n", [Secs]),
    ?line {stopped, _} = percept_db:stop(),
    print_remainers(remainers(Begin, processes())),
    ok.

%%----------------------------------------------------------------------
%% Auxiliary tests
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Auxiliary
%%----------------------------------------------------------------------

print_remainers([])   -> ok;
print_remainers([Pid|Pids]) ->
    io:format("[Pid ~p] [Entry ~p] [Name ~p]~n", [
	Pid,
	erlang:process_info(Pid, initial_call),
	erlang:process_info(Pid, registered_name)
    ]),
    print_remainers(Pids).

remainers(Begin, End) -> remainers(Begin, End, []).
remainers(_, [], Out) -> lists:reverse(Out);
remainers(Begin, [Pid|End], Out) ->
    case lists:member(Pid, Begin) of
	true  -> remainers(Begin, End, Out);
	false -> remainers(Begin, End, [Pid|Out])
    end.



    


