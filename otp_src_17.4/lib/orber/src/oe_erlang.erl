%%  coding: latin-1
%%------------------------------------------------------------
%%
%% Implementation stub file
%% 
%% Target: oe_erlang
%% Source: /net/isildur/ldisk/daily_build/17_prebuild_opu_o.2014-12-09_21/otp_src_17/lib/ic/include/erlang.idl
%% IC vsn: 4.3.6
%% 
%% This file is automatically generated. DO NOT EDIT IT.
%%
%%------------------------------------------------------------

-module(oe_erlang).
-ic_compiled("4_3_6").


-include_lib("orber/include/ifr_types.hrl").

%% Interface functions

-export([oe_register/0, oe_unregister/0, oe_get_module/5]).
-export([oe_dependency/0]).



oe_register() ->
    OE_IFR = orber_ifr:find_repository(),

    register_tests(OE_IFR),

    _OE_1 = oe_get_top_module(OE_IFR, "IDL:erlang:1.0", "erlang", "1.0"),

    orber_ifr:'ModuleDef_create_struct'(_OE_1, "IDL:erlang/pid:1.0", "pid", "1.0", [#structmember{name="node", type={tk_string,256}, type_def=orber_ifr:'Repository_create_idltype'(OE_IFR, {tk_string,256})}, #structmember{name="num", type=tk_ulong, type_def=orber_ifr:'Repository_create_idltype'(OE_IFR, tk_ulong)}, #structmember{name="serial", type=tk_ulong, type_def=orber_ifr:'Repository_create_idltype'(OE_IFR, tk_ulong)}, #structmember{name="creation", type=tk_ulong, type_def=orber_ifr:'Repository_create_idltype'(OE_IFR, tk_ulong)}]),

    orber_ifr:'ModuleDef_create_struct'(_OE_1, "IDL:erlang/port:1.0", "port", "1.0", [#structmember{name="node", type={tk_string,256}, type_def=orber_ifr:'Repository_create_idltype'(OE_IFR, {tk_string,256})}, #structmember{name="id", type=tk_ulong, type_def=orber_ifr:'Repository_create_idltype'(OE_IFR, tk_ulong)}, #structmember{name="creation", type=tk_ulong, type_def=orber_ifr:'Repository_create_idltype'(OE_IFR, tk_ulong)}]),

    orber_ifr:'ModuleDef_create_struct'(_OE_1, "IDL:erlang/ref:1.0", "ref", "1.0", [#structmember{name="node", type={tk_string,256}, type_def=orber_ifr:'Repository_create_idltype'(OE_IFR, {tk_string,256})}, #structmember{name="id", type=tk_ulong, type_def=orber_ifr:'Repository_create_idltype'(OE_IFR, tk_ulong)}, #structmember{name="creation", type=tk_ulong, type_def=orber_ifr:'Repository_create_idltype'(OE_IFR, tk_ulong)}]),

    orber_ifr:'ModuleDef_create_alias'(_OE_1, "IDL:erlang/term:1.0", "term", "1.0", orber_ifr:'Repository_create_idltype'(OE_IFR, tk_any)),

    orber_ifr:'ModuleDef_create_alias'(_OE_1, "IDL:erlang/binary:1.0", "binary", "1.0", orber_ifr:'Repository_create_idltype'(OE_IFR, {tk_sequence,tk_octet,0})),

    ok.


%% General IFR registration checks.
register_tests(OE_IFR)->
  re_register_test(OE_IFR),
  include_reg_test(OE_IFR).


%% IFR type Re-registration checks.
re_register_test(OE_IFR)->
  case orber_ifr:'Repository_lookup_id'(OE_IFR,"IDL:erlang/pid:1.0") of
    []  ->
      true;
    _ ->
      exit({allready_registered,"IDL:erlang/pid:1.0"})
 end.


%% No included idl-files detected.
include_reg_test(_OE_IFR) -> true.


%% Fetch top module reference, register if unregistered.
oe_get_top_module(OE_IFR, ID, Name, Version) ->
  case orber_ifr:'Repository_lookup_id'(OE_IFR, ID) of
    [] ->
      orber_ifr:'Repository_create_module'(OE_IFR, ID, Name, Version);
    Mod  ->
      Mod
   end.

%% Fetch module reference, register if unregistered.
oe_get_module(OE_IFR, OE_Parent, ID, Name, Version) ->
  case orber_ifr:'Repository_lookup_id'(OE_IFR, ID) of
    [] ->
      orber_ifr:'ModuleDef_create_module'(OE_Parent, ID, Name, Version);
    Mod  ->
      Mod
   end.



oe_unregister() ->
    OE_IFR = orber_ifr:find_repository(),

    oe_destroy(OE_IFR, "IDL:erlang/binary:1.0"),
    oe_destroy(OE_IFR, "IDL:erlang/term:1.0"),
    oe_destroy(OE_IFR, "IDL:erlang/ref:1.0"),
    oe_destroy(OE_IFR, "IDL:erlang/port:1.0"),
    oe_destroy(OE_IFR, "IDL:erlang/pid:1.0"),
    oe_destroy_if_empty(OE_IFR, "IDL:erlang:1.0"),
    ok.


oe_destroy_if_empty(OE_IFR,IFR_ID) ->
    case orber_ifr:'Repository_lookup_id'(OE_IFR, IFR_ID) of
	[] ->
	    ok;
	Ref ->
	    case orber_ifr:contents(Ref, 'dk_All', 'true') of
		[] ->
		    orber_ifr:destroy(Ref),
		    ok;
		_ ->
		    ok
	    end
    end.

oe_destroy(OE_IFR,IFR_ID) ->
    case orber_ifr:'Repository_lookup_id'(OE_IFR, IFR_ID) of
	[] ->
	    ok;
	Ref ->
	    orber_ifr:destroy(Ref),
	    ok
    end.



%% Idl file dependency list function
oe_dependency() ->

    [].

