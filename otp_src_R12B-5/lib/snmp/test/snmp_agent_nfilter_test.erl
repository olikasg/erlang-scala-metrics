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
%% Purpose:
%%----------------------------------------------------------------------
-module(snmp_agent_nfilter_test).

%%----------------------------------------------------------------------
%% Include files
%%----------------------------------------------------------------------
-include("test_server.hrl").
-include("snmp_test_lib.hrl").


%%----------------------------------------------------------------------
%% External exports
%%----------------------------------------------------------------------
-export([
	 all/1, 
         init_per_testcase/2, fin_per_testcase/2
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

init_per_testcase(_Case, Config) when list(Config) ->
    Config.

fin_per_testcase(_Case, Config) when list(Config) ->
    Config.

%%======================================================================
%% Test case definitions
%%======================================================================
all(_) ->
    ?SKIP(not_yet_implemented).


%%======================================================================
%% Test functions
%%======================================================================


%%======================================================================
%% Internal functions
%%======================================================================
