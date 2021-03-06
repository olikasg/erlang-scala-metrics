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
%% File    : orber_ifr_uniondef.erl
%% Purpose : Code for Uniondef
%%----------------------------------------------------------------------

-module(orber_ifr_uniondef).

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
	 move/4,
	 '_get_type'/1,
	 '_get_discriminator_type'/1,
	 '_get_discriminator_type_def'/1,
	 '_set_discriminator_type_def'/2,
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
%%% UnionDef (TypedefDef(Contained(IRObject), IDLType(IRObject)))

%%%----------------------------------------------------------------------
%%%  Interfaces inherited from IRObject

'_get_def_kind'({ObjType, ObjID}) ?tcheck(ir_UnionDef, ObjType) ->
    orber_ifr_irobject:'_get_def_kind'({ObjType, ObjID}).

destroy({ObjType, ObjID}) ?tcheck(ir_UnionDef, ObjType) ->
    F = fun() -> ObjList = cleanup_for_destroy({ObjType, ObjID}),
		 orber_ifr_irobject:destroy([{ObjType, ObjID} | ObjList])
	end,
    orber_ifr_utils:ifr_transaction_write(F).

cleanup_for_destroy({ObjType,ObjID}) ?tcheck(ir_UnionDef, ObjType) ->
    map(fun(X) -> orber_ifr_idltype:cleanup_for_destroy(
		    X#unionmember.type_def)
	end,
	'_get_members'({ObjType, ObjID})
       ) ++
	orber_ifr_idltype:cleanup_for_destroy(
	  '_get_discriminator_type_def'({ObjType,ObjID})) ++
	orber_ifr_typedef:cleanup_for_destroy({ObjType,ObjID}).

%%%----------------------------------------------------------------------
%%%  Interfaces inherited from Contained

'_get_id'({ObjType, ObjID}) ?tcheck(ir_UnionDef, ObjType) ->
    orber_ifr_contained:'_get_id'({ObjType,ObjID}).

'_set_id'({ObjType, ObjID}, EO_Value) ?tcheck(ir_UnionDef, ObjType) ->
    orber_ifr_contained:'_set_id'({ObjType,ObjID},EO_Value).

'_get_name'({ObjType, ObjID}) ?tcheck(ir_UnionDef, ObjType) ->
    orber_ifr_contained:'_get_name'({ObjType,ObjID}).

'_set_name'({ObjType,ObjID},EO_Value) ?tcheck(ir_UnionDef,ObjType) ->
    orber_ifr_contained:'_set_name'({ObjType,ObjID}, EO_Value).

'_get_version'({ObjType, ObjID}) ?tcheck(ir_UnionDef, ObjType) ->
    orber_ifr_contained:'_get_version'({ObjType,ObjID}).

'_set_version'({ObjType, ObjID}, EO_Value) ?tcheck(ir_UnionDef, ObjType) ->
    orber_ifr_contained:'_set_version'({ObjType,ObjID},EO_Value).

'_get_defined_in'({ObjType, ObjID}) ?tcheck(ir_UnionDef, ObjType) ->
    orber_ifr_contained:'_get_defined_in'({ObjType,ObjID}).

'_get_absolute_name'({ObjType, ObjID}) ?tcheck(ir_UnionDef,ObjType) ->
    orber_ifr_contained:'_get_absolute_name'({ObjType,ObjID}).

'_get_containing_repository'({ObjType, ObjID}) ?tcheck(ir_UnionDef, ObjType) ->
    orber_ifr_contained:'_get_containing_repository'({ObjType,ObjID}).

describe({ObjType, ObjID}) ?tcheck(ir_UnionDef, ObjType) ->
    orber_ifr_contained:describe({ObjType,ObjID}).

move({ObjType, ObjID}, New_container, New_name, New_version)
    ?tcheck(ir_UnionDef, ObjType) ->
    orber_ifr_contained:move({ObjType,ObjID}, New_container, New_name,
			     New_version).

%%%----------------------------------------------------------------------
%%%  Interfaces inherited from IDLType

'_get_type'({ObjType, ObjID}) ?tcheck(ir_UnionDef, ObjType) ->
    orber_ifr_idltype:'_get_type'({ObjType, ObjID}).

%%%----------------------------------------------------------------------
%%% Non-inherited interfaces

'_get_discriminator_type'({ObjType, ObjID}) ?tcheck(ir_UnionDef, ObjType) ->
    get_field({ObjType,ObjID},discriminator_type).

'_get_discriminator_type_def'({ObjType, ObjID}) ?tcheck(ir_UnionDef,ObjType) ->
    get_field({ObjType,ObjID},discriminator_type_def).

'_set_discriminator_type_def'({ObjType, ObjID}, EO_Value)
			     ?tcheck(ir_UnionDef, ObjType) ->
    UnionDef = get_object({ObjType, ObjID}),
    NewUnionDef = UnionDef#ir_UnionDef{type = EO_Value#ir_IDLType.type,
				       discriminator_type =
				       EO_Value#ir_IDLType.type,
				       discriminator_type_def = EO_Value},
    set_object(NewUnionDef).

'_get_members'({ObjType, ObjID}) ?tcheck(ir_UnionDef, ObjType) ->
    get_field({ObjType,ObjID},members).

%%% *** What should the value of the discriminator-typecode be when
%%% updating the type attribute? (CORBA 2.0, p 6-20). For now we just
%%% leave it unchanged, but this is perhaps not the right thing to do.

-define(discr_tc(TC),element(4,TC)).
-define(default(TC),element(5,TC)).

'_set_members'({ObjType, ObjID}, EO_Value) ?tcheck(ir_UnionDef, ObjType) ->
    UnionDef = get_object({ObjType, ObjID}),
    Members=map(fun(Unionmember) -> Unionmember#unionmember{type=tk_void} end,
		EO_Value),
    NewUnionDef = UnionDef#ir_UnionDef{type =
				       {tk_union,
					UnionDef#ir_UnionDef.id,
					UnionDef#ir_UnionDef.name,
					?discr_tc(UnionDef#ir_UnionDef.type),
					?default(UnionDef#ir_UnionDef.type),
					map(fun(#unionmember{name=Name,
							     label=Label,
							     type=Type}) ->
						    {Label,Name,Type}
					    end,
					    EO_Value)},
				       members = Members},
    set_object(NewUnionDef).
