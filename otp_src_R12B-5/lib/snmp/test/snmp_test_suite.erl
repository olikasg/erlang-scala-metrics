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
%% Purpose: Each snmp test suite implements this behaviour
%%----------------------------------------------------------------------

-module(snmp_test_suite).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [
     {all,               1},
     {init_per_testcase, 2},
     {fin_per_testcase,  2}
    ];
behaviour_info(_Other) ->
    undefined.
