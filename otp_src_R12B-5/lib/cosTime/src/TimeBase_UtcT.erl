%%------------------------------------------------------------
%%
%% Implementation stub file
%% 
%% Target: TimeBase_UtcT
%% Source: /ldisk/daily_build/otp_prebuild_r12b.2008-11-05_12/otp_src_R12B-5/lib/cosTime/src/TimeBase.idl
%% IC vsn: 4.2.19
%% 
%% This file is automatically generated. DO NOT EDIT IT.
%%
%%------------------------------------------------------------

-module('TimeBase_UtcT').
-ic_compiled("4_2_19").


-include("TimeBase.hrl").

-export([tc/0,id/0,name/0]).



%% returns type code
tc() -> {tk_struct,"IDL:omg.org/TimeBase/UtcT:1.0","UtcT",
                   [{"time",tk_ulonglong},
                    {"inacclo",tk_ulong},
                    {"inacchi",tk_ushort},
                    {"tdf",tk_short}]}.

%% returns id
id() -> "IDL:omg.org/TimeBase/UtcT:1.0".

%% returns name
name() -> "TimeBase_UtcT".



