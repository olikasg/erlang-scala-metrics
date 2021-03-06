%%<copyright>
%% <year>2000-2007</year>
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
%%-----------------------------------------------------------------
%% Purpose: Supervisor for all active UDP port servers
%%-----------------------------------------------------------------
-module(megaco_udp_sup).

-behaviour(supervisor).

%%-----------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------
-include_lib("megaco/include/megaco.hrl").

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([
	 start_link/0, 
	 start_child/2
	]).


%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([
	 init/1, 
	 terminate/2, 
	 start_server/1
	]).


%%-----------------------------------------------------------------
%% External interface functions
%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% Func: start_link
%% Description: Starts the UDP port server supervisor
%%-----------------------------------------------------------------
start_link() ->
    supervisor:start_link(?MODULE, [[]]).


%%-----------------------------------------------------------------
%% Func: start_child
%% Description: Starts the UDP port server supervisor
%%-----------------------------------------------------------------
start_child(SupPid, UdpRec) ->
    supervisor:start_child(SupPid, [UdpRec]).


%%-----------------------------------------------------------------
%% Internal interface functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: start_server/1
%% Description: Function which the supervisor calls to start a child
%%-----------------------------------------------------------------
start_server(Args) ->
    megaco_udp_server:start_link(Args).


%%-----------------------------------------------------------------
%% Server functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: init/1
%% Description: Init funcion for the supervisor
%%-----------------------------------------------------------------
init(_) ->
    SupFlags  = {simple_one_for_one, 500, 100},
    ChildSpec = [
		 {megaco_udp_server,
		  {?MODULE, start_server, []},
		  permanent, 
		  10000, 
		  worker,
		  []}
		],
    {ok, {SupFlags, ChildSpec}}.


%%-----------------------------------------------------------------
%% Func: terminate/1
%% Description: Termination function for the supervisor
%%-----------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------
