%%--------------------------------------------------------------------
%%<copyright>
%% <year>1997-2007</year>
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
%% File    : orber_ifr_exceptiondef.erl
%% Purpose : Code for Exceptiondef
%%----------------------------------------------------------------------

-module(orber_ifr_exceptiondef).

-export(['_get_def_kind'/1,
	 destroy/1,
	 cleanup_for_destroy/1,			%not in CORBA 2.0
	 '_get_id'/1,
	 '_set_id'/2,
	 '_get_name'/1,
	 '_set_name'/2,
	 '_get_version'/1,
	 '_set_version'/2,
	 '_get_defined_in'/1,
	 '_get_absolute_name'/1,
	 '_get_containing_repository'/1,
	 describe/1,
	 %%lookup_id/1,				%not in CORBA 2.0
	 move/4,
	 '_get_type'/1,
	 '_get_members'/1,
	 '_set_members'/2
	]).

-import(orber_ifr_utils,[get_field/2,
		   get_object/1,
		   set_object/1
		  ]).
-import(lists,[map/2]).

-include("orber_ifr.hrl").
-include("ifr_objects.hrl").
-include_lib("orber/include/ifr_types.hrl").

%%%======================================================================
%%% ExceptionDef (Contained(IRObject))

%%%----------------------------------------------------------------------
%%%  Interfaces inherited from IRObject

'_get_def_kind'({ObjType, ObjID}) ?tcheck(ir_ExceptionDef, ObjType) ->
    orber_ifr_irobject:'_get_def_kind'({ObjType,ObjID}).

destroy({ObjType, ObjID}) ?tcheck(ir_ExceptionDef, ObjType) ->
    F = fun() -> ObjList = cleanup_for_destroy({ObjType, ObjID}),
		 orber_ifr_irobject:destroy([{ObjType,ObjID} | ObjList])
	end,
    orber_ifr_utils:ifr_transaction_write(F).

cleanup_for_destroy({ObjType,ObjID}) ?tcheck(ir_ExceptionDef, ObjType) ->
    lists:map(fun(X) -> orber_ifr_idltype:cleanup_for_destroy(
			  X#structmember.type_def)
	      end,
	      '_get_members'({ObjType, ObjID})) ++
    orber_ifr_contained:cleanup_for_destroy({ObjType,ObjID}).

%%%----------------------------------------------------------------------
%%%  Interfaces inherited from Contained

'_get_id'({ObjType, ObjID}) ?tcheck(ir_ExceptionDef, ObjType) ->
    orber_ifr_contained:'_get_id'({ObjType,ObjID}).

'_set_id'({ObjType, ObjID}, EO_Value) ?tcheck(ir_ExceptionDef, ObjType) ->
    orber_ifr_contained:'_set_id'({ObjType,ObjID},EO_Value).

'_get_name'({ObjType, ObjID}) ?tcheck(ir_ExceptionDef, ObjType) ->
    orber_ifr_contained:'_get_name'({ObjType,ObjID}).

'_set_name'({ObjType, ObjID}, EO_Value) ?tcheck(ir_ExceptionDef, ObjType) ->
    orber_ifr_contained:'_set_name'({ObjType,ObjID}, EO_Value).

'_get_version'({ObjType, ObjID}) ?tcheck(ir_ExceptionDef, ObjType) ->
    orber_ifr_contained:'_get_version'({ObjType,ObjID}).

'_set_version'({ObjType, ObjID}, EO_Value) ?tcheck(ir_ExceptionDef, ObjType) ->
    orber_ifr_contained:'_set_version'({ObjType,ObjID},EO_Value).

'_get_defined_in'({ObjType, ObjID}) ?tcheck(ir_ExceptionDef, ObjType) ->
    orber_ifr_contained:'_get_defined_in'({ObjType,ObjID}).

'_get_absolute_name'({ObjType, ObjID}) ?tcheck(ir_ExceptionDef, ObjType) ->
    orber_ifr_contained:'_get_absolute_name'({ObjType,ObjID}).

'_get_containing_repository'({ObjType, ObjID})
			    ?tcheck(ir_ExceptionDef, ObjType) ->
    orber_ifr_contained:'_get_containing_repository'({ObjType,ObjID}).

describe({ObjType, ObjID}) ?tcheck(ir_ExceptionDef, ObjType) ->
    orber_ifr_contained:describe({ObjType,ObjID}).

%%% *** This function should be removed. Use
%%% orber_ifr_repository:lookup_id/2 instead.

%%lookup_id(SearchId) ->
%%    _F = fun() ->
%%                 Q = query [X.ir_Internal_ID || X <- table(ir_ExceptionDef)]
%%                     end,
%%                 mnemosyne:eval(Q)
%%         end,
%%    case orber_ifr_utils:ifr_transaction_read(_F) of
%%	?read_check_2() ->
%%	    {ok, []};
%%	?read_check_1(Rep_IDs) ->
%%	    ExceptionDefs = lists:map(fun(X) -> {ir_ExceptionDef, X} end,
%%				      Rep_IDs),
%%	    {ok, lists:filter(fun(X) -> orber_ifr_exceptiondef:'_get_id'(X) ==
%%					    SearchId end,
%%			      ExceptionDefs)}
%%    end.

move({ObjType, ObjID}, New_container, New_name, New_version)
    ?tcheck(ir_ExceptionDef, ObjType) ->
    orber_ifr_contained:move({ObjType,ObjID},New_container,New_name,
			     New_version).

%%%----------------------------------------------------------------------
%%% Non-inherited interfaces

'_get_type'({ObjType, ObjID}) ?tcheck(ir_ExceptionDef, ObjType) ->
    get_field({ObjType,ObjID},type).

'_get_members'({ObjType, ObjID}) ?tcheck(ir_ExceptionDef, ObjType) ->
    get_field({ObjType,ObjID},members).

'_set_members'({ObjType, ObjID}, EO_Value) ?tcheck(ir_ExceptionDef, ObjType) ->
    ExceptionDef = get_object({ObjType, ObjID}),
    Members=map(fun(Exceptionmember) ->
			Exceptionmember#structmember{type=tk_void}
		end, EO_Value),
    New_ExceptionDef =
	ExceptionDef#ir_ExceptionDef{type =
				     {tk_except,
				      ExceptionDef#ir_ExceptionDef.id,
				      ExceptionDef#ir_ExceptionDef.name,
				      map(fun(#structmember{name=Name,
							    type=Type}) ->
						  {Name,Type}
					  end,
					  EO_Value)},
				     members=Members},
    set_object(New_ExceptionDef).
