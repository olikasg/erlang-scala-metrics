%%------------------------------------------------------------
%%
%% Implementation stub file
%% 
%% Target: CosNotifyFilter_ConstraintExpSeq
%% Source: /ldisk/daily_build/otp_prebuild_r12b.2008-11-05_12/otp_src_R12B-5/lib/cosNotification/src/CosNotifyFilter.idl
%% IC vsn: 4.2.19
%% 
%% This file is automatically generated. DO NOT EDIT IT.
%%
%%------------------------------------------------------------

-module('CosNotifyFilter_ConstraintExpSeq').
-ic_compiled("4_2_19").


-include("CosNotifyFilter.hrl").

-export([tc/0,id/0,name/0]).



%% returns type code
tc() -> {tk_sequence,
            {tk_struct,"IDL:omg.org/CosNotifyFilter/ConstraintExp:1.0",
                "ConstraintExp",
                [{"event_types",
                  {tk_sequence,
                      {tk_struct,"IDL:omg.org/CosNotification/EventType:1.0",
                          "EventType",
                          [{"domain_name",{tk_string,0}},
                           {"type_name",{tk_string,0}}]},
                      0}},
                 {"constraint_expr",{tk_string,0}}]},
            0}.

%% returns id
id() -> "IDL:omg.org/CosNotifyFilter/ConstraintExpSeq:1.0".

%% returns name
name() -> "CosNotifyFilter_ConstraintExpSeq".



