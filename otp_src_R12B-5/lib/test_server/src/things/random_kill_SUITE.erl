%%<copyright>
%% <year>1996-2007</year>
%% <holder>Ericsson AB, All Rights Reserved</holder>
%%</copyright>
%%<legalnotice>
%% ``The contents of this file are subject to the Erlang Public License,
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
-module(random_kill_SUITE).
-compile([export_all]).
%%-define(line_trace,1).
-include("test_server.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all(suite) -> [run].

-define(iterations,25). %% Kill this many processes, 
                        %% possibly with reboots in between

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

run(suite) -> [];
run(Config) ->
    registered(?iterations).

registered(0) ->
    ok;
registered(N) ->
    random:seed(3461*N,1159*N,351*N),
    Pid = select_victim(registered),
    test_server:resume_point(?MODULE,registered,[N-1]),
    test_server:format("About to kill pid ~p (~p)\n~p",
		       [Pid,process_info(Pid,registered_name),info(Pid)]),
    %%exit(Pid,kill),
    registered(N-1).

info(Pid) ->
    Rest0 = tl(pid_to_list(Pid)),
    {P1,Rest1} = get_until($.,Rest0),
    {P2,Rest2} = get_until($.,Rest1),
    {P3,_}     = get_until($>,Rest2),
    c:i(list_to_integer(P1),list_to_integer(P2),list_to_integer(P3)).

get_until(Ch,L) ->
    get_until(Ch,L,[]).
get_until(Ch,[],Acc) ->
    {lists:reverse(Acc),[]};
get_until(Ch,[Ch|T],Acc) ->
    {lists:reverse(Acc),T};
get_until(Ch,[H|T],Acc) ->
    get_until(Ch,T,[H|Acc]).

select_victim(registered) ->
    Pids =
	lists:map(fun(Server)-> whereis(Server) end,registered()),
    ImmunePids =
	[self()|lists:map(fun(Job)-> element(2,Job) end,test_server:jobs())],
    SuitablePids =
	lists:filter(fun(Pid)-> case lists:member(Pid,ImmunePids) of
				    true -> false;
				    false -> true
				end
		     end, Pids),
    Selected = random:uniform(length(SuitablePids)),
    io:format("Selected ~p if ~p",[Selected,length(SuitablePids)]),
    lists:nth(Selected,SuitablePids).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

					 
