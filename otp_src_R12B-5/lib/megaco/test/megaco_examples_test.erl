%%<copyright>
%% <year>2001-2007</year>
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

-module(megaco_examples_test).

-compile(export_all).

-include("megaco_test_lib.hrl").
-include_lib("megaco/include/megaco.hrl").
-include_lib("megaco/include/megaco_message_v1.hrl").

t()     -> megaco_test_lib:t(?MODULE).
t(Case) -> megaco_test_lib:t({?MODULE, Case}).

%% Test server callbacks
init_per_testcase(Case, Config) ->
    put(dbg,true),
    purge_examples(),
    load_examples(),
    megaco:enable_trace(max, io),
    megaco_test_lib:init_per_testcase(Case, Config).

fin_per_testcase(Case, Config) ->
    purge_examples(),
    erase(dbg),
    megaco:disable_trace(),
    megaco_test_lib:fin_per_testcase(Case, Config).

example_modules() ->
    [megaco_simple_mg, megaco_simple_mgc].

load_examples() ->
    case code:lib_dir(megaco) of
	{error, Reason} ->
	    {error, Reason};
	Dir ->
	    [code:load_abs(filename:join([Dir, examples, simple, M])) || M <- example_modules()]
    end.

purge_examples() ->
    case code:lib_dir(megaco) of
	{error, Reason} ->
	    {error, Reason};
	_Dir ->
	    [code:purge(M) || M <- example_modules()]
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Top test case

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
all(doc) ->
    ["Run all examples mentioned in the documentation",
     "Are really all examples covered?"];
all(suite) ->
    [
     simple
    ].

simple(suite) ->
    [];
simple(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    d("simple -> proxy start",[]),
    ProxyPid = megaco_test_lib:proxy_start({?MODULE, ?LINE}),

    d("simple -> start megaco",[]),
    ?VERIFY(ok, megaco:start()),
    
    d("simple -> start mgc",[]),
    ?APPLY(ProxyPid, fun() -> megaco_simple_mgc:start() end),
    receive
	{res, _, {ok, MgcAll}} when list(MgcAll) ->
	    MgcBad = [MgcRes || MgcRes <- MgcAll, element(1, MgcRes) /= ok],
	    ?VERIFY([], MgcBad),
	    %% MgcGood = MgcAll -- MgcBad,
	    %% MgcRecHandles = [MgcRH || {ok, _MgcPort, MgcRH} <- MgcGood],

	    d("simple -> start mg",[]),
	    ?APPLY(ProxyPid, fun() -> megaco_simple_mg:start() end),
	    receive
		{res, _, MgList} when list(MgList), length(MgList) == 4 ->
		    d("simple -> received res: ~p",[MgList]),		    
		    Verify = 
			fun({_MgMid, {TransId, Res}}) when TransId == 1 ->
				case Res of
				    {ok, [AR]} when record(AR, 'ActionReply') ->
					case AR#'ActionReply'.commandReply of
					    [{serviceChangeReply, SCR}] ->
						case SCR#'ServiceChangeReply'.serviceChangeResult of
						    {serviceChangeResParms, MgcMid} when MgcMid /= asn1_NOVALUE ->
							ok;
						    Error ->
							?ERROR(Error)
						end;
					    Error ->
						?ERROR(Error)
					end;
				    Error ->
					?ERROR(Error)
				end;
			   (Error) ->
				?ERROR(Error)
			end,
		    lists:map(Verify, MgList);
		Error ->
		    ?ERROR(Error)
	    end;
	Error ->
	    ?ERROR(Error)
    end,
    d("simple -> verify info()",[]),
    info(),
    d("simple -> verify system_info(users)",[]),
    users(),
    d("simple -> stop mgc",[]),
    ?VERIFY(5, length(megaco_simple_mgc:stop())),
    d("simple -> verify system_info(users)",[]),
    users(),
    d("simple -> stop megaco",[]),
    ?VERIFY(ok, megaco:stop()),
    d("simple -> kill (exit) ProxyPid: ~p",[ProxyPid]),
    exit(ProxyPid, shutdown), % Controlled kill of transport supervisors

    ok.


info() ->
    case (catch megaco:info()) of
	{'EXIT', _} = Error ->
	    ?ERROR(Error);
	Info ->
	    ?LOG("Ok, ~p~n", [Info])
    end.

users() ->
    case (catch megaco:system_info(users)) of
	{'EXIT', _} = Error ->
	    ?ERROR(Error);
	Users ->
	    ?LOG("Ok, ~p~n", [Users])
    end.




d(F,A) ->
    d(get(dbg),F,A).

d(true,F,A) ->
    io:format("DBG: " ++ F ++ "~n",A);
d(_, _F, _A) ->
    ok.
