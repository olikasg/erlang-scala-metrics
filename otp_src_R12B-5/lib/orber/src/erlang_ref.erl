%%------------------------------------------------------------
%%
%% Implementation stub file
%% 
%% Target: erlang_ref
%% Source: /ldisk/daily_build/otp_prebuild_r12b.2008-11-05_12/otp_src_R12B-5/lib/ic/include/erlang.idl
%% IC vsn: 4.2.19
%% 
%% This file is automatically generated. DO NOT EDIT IT.
%%
%%------------------------------------------------------------

-module(erlang_ref).
-ic_compiled("4_2_19").


-include("erlang.hrl").

-export([tc/0,id/0,name/0]).



%% returns type code
tc() -> {tk_struct,"IDL:erlang/ref:1.0","ref",
                   [{"node",{tk_string,256}},
                    {"id",tk_ulong},
                    {"creation",tk_ulong}]}.

%% returns id
id() -> "IDL:erlang/ref:1.0".

%% returns name
name() -> "erlang_ref".



