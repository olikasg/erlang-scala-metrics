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
%% Purpose: Test application config
%%----------------------------------------------------------------------

-module(megaco_codec_test).

-compile(export_all).

-include("megaco_test_lib.hrl").
-include_lib("megaco/include/megaco.hrl").

t()     -> megaco_test_lib:t(?MODULE).
t(Case) -> megaco_test_lib:t({?MODULE, Case}).

%% Test server callbacks
init_per_testcase(Case, Config) ->
    megaco_test_lib:init_per_testcase(Case, Config).

fin_per_testcase(Case, Config) ->
    megaco_test_lib:fin_per_testcase(Case, Config).

init() ->
    process_flag(trap_exit, true),
    megaco_test_lib:flush().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Top test case

all(suite) ->
    [
     codec
    ].

codec(suite) ->
    [{megaco_codec_v1_test,     all}, 
     {megaco_codec_v2_test,     all}, 
     {megaco_codec_prev3a_test, all}, 
     {megaco_codec_prev3b_test, all}, 
     {megaco_codec_prev3c_test, all}, 
     {megaco_codec_v3_test,     all}].
