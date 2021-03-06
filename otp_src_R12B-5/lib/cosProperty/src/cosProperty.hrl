%%----------------------------------------------------------------------
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
%%----------------------------------------------------------------------
%% File    : cosProperty.hrl
%% Purpose : 
%%----------------------------------------------------------------------


%%--------------- INCLUDES -----------------------------------
%% External
-include_lib("orber/include/corba.hrl").
-include_lib("orber/include/ifr_types.hrl").

%%-----------------------------------------------------------------
%% Mnesia Table definition record
%%-----------------------------------------------------------------
-record('oe_CosPropertyService', {key, properties}).
 
%%-----------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------
-define(PropertySet,    0).
-define(PropertySetDef, 1).
 
%% This macro returns a read fun suitable for evaluation in a transaction
-define(read_function(Objkey),
        fun() ->
                mnesia:read(Objkey)
        end).
 
%% This macro returns a write fun suitable for evaluation in a transaction
-define(write_function(R),
        fun() ->
                mnesia:write(R)
        end).
 
%% This macro returns a delete fun suitable for evaluation in a transaction
-define(delete_function(R),
        fun() ->
                mnesia:delete(R)
        end).
 
-define(query_check(Q_res), {atomic, Q_res}).
 

-define(write_ErrorMsg(Txt, Arg),
error_logger:error_msg("================ CosProperty ==============~n"
		       Txt
		       "===========================================~n",
		       Arg)).



-ifdef(debug).
-define(debug_print(F,A),
        io:format("[LINE: ~p MODULE: ~p] "++F,[?LINE, ?MODULE]++A)).
-define(property_TypeCheck(O,M), 'cosProperty':type_check(O,M)).
-else.
-define(debug_print(F,A), ok).
-define(property_TypeCheck(O,I), ok).
-endif.    

%%--------------- END OF MODULE ------------------------------
