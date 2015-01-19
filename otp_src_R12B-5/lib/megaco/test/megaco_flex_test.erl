%%<copyright>
%% <year>2008-2008</year>
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
%% Purpose: Test various aspects of the flex scanner handling
%%
%% Test:    ts:run(megaco, megaco_flex_test, [batch]).
%%
%%----------------------------------------------------------------------

-module(megaco_flex_test).

-include("megaco_test_lib.hrl").

-export([
	 t/0, t/1, 

	 init_per_testcase/2, fin_per_testcase/2,

	 all/1,
	 plain/1,
	 port_exit/1,
	 garbage_in/1

	]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t()     -> megaco_test_lib:t(?MODULE).
t(Case) -> megaco_test_lib:t({?MODULE, Case}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_per_testcase(Case, Config) ->
    megaco_test_lib:init_per_testcase(Case, Config).

fin_per_testcase(Case, Config) ->
    megaco_test_lib:fin_per_testcase(Case, Config).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all(suite) ->
    [
     plain,
     port_exit,
     garbage_in
    ].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

plain(suite) ->
    [];
plain(doc) ->
    ["This is to simply test that it is possible to start and stop the "
     "flex handler."];
plain(Config) when is_list(Config) ->
    put(tc, plain), 
    p("begin"),
    process_flag(trap_exit, true),
    p("start the flex handler"),
    {ok, Pid, _PortInfo} = flex_scanner_handler_start(), 
    p("stop handler"),
    flex_scanner_handler_stop(Pid),
    p("end"),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

port_exit(suite) ->
    [];
port_exit(doc) ->
    ["Test that the handler detects and handles an exiting port."];
port_exit(Config) when is_list(Config) ->
    put(tc, port_exit), 
    p("begin"),
    process_flag(trap_exit, true),

    p("start the flex handler"),
    {ok, Pid, {flex, Port}} = flex_scanner_handler_start(), 

    p("simulate crash"),
    exit(Port, simulated_crash), 
    
    p("await handler exit"),
    receive
	{'EXIT', Pid, _} ->
	    p("end"),
	    ok
    after 5000 ->
	    p("timeout - stop handler"),
	    flex_scanner_handler_stop(Pid),
	    p("end after timeout"),
	    {error, timeout}
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

garbage_in(suite) ->
    [];
garbage_in(doc) ->
    ["Send in various unexpected messages and requeststo the handler "
     "to see that it does die on us. "];
garbage_in(Config) when is_list(Config) ->
    put(tc, garbage_in), 
    p("begin"),
    process_flag(trap_exit, true),

    p("start the flex handler"),
    {ok, Pid, _PortInfo} = flex_scanner_handler_start(), 

    p("make an invalid call"),
    {error, _} = gen_server:call(Pid, garbage_request), 
    p("make an invalid cast"),
    gen_server:cast(Pid, garbage_msg),
    p("send an unknown message"),
    Pid ! garbage_info,

    p("wait for any garbage response"),
    receive
	Any ->
	    p("end with unexpected message: ~p", [Any]),
	    {error, {unexpected_msg, Any}}
    after 1000 ->
	    p("end with nothing received - stop handler"),
	    flex_scanner_handler_stop(Pid),
	    ok
    end.



%% ------- Misc functions --------

flex_scanner_handler_start() ->
    case megaco_flex_scanner_handler:start_link() of
	{error, {failed_starting_scanner, {error, {load_driver, _}}}} ->
	    p("failed loading driver"),
	    ?SKIP(could_not_load_driver);
	{error, {failed_starting_scanner, {load_driver, _}}} ->
	    p("failed loading driver"),
	    ?SKIP(could_not_load_driver);
	Else ->
	    p("driver load result: ~p", [Else]),
	    Else
    end.

flex_scanner_handler_stop(Pid) ->
    megaco_flex_scanner_handler:stop(Pid).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

p(F) ->
    p(F, []).

p(F, A) ->
    TC = get(tc),
    io:format("*** [~s] ~p ~w ***"
              "~n   " ++ F ++ "~n",
              [formated_timestamp(), self(), TC | A]).

formated_timestamp() ->
    format_timestamp(erlang:now()).

format_timestamp({_N1, _N2, N3} = Now) ->
    {Date, Time}     = calendar:now_to_datetime(Now),
    {YYYY, MM, DD}   = Date,
    {Hour, Min, Sec} = Time,
    FormatDate =
        io_lib:format("~.4w:~.2.0w:~.2.0w ~.2.0w:~.2.0w:~.2.0w 4~w",
                      [YYYY,MM,DD,Hour,Min,Sec,round(N3/1000)]),
    lists:flatten(FormatDate).


