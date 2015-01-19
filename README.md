RefactorErl
===========

Repository for the original release of RefactorErl 0.9.14.09.

This repository contains modifications for my research.

## Loading mnesia into the database

You need to add the OTP ```lib``` directory as appbase:

```erlang
f(OTP_ROOT).
OTP_ROOT = "/Users/olahgabor/Documents/git/refactorerl/otp_src_R12B-5/".
ri:addenv(appbase, OTP_ROOT ++ "lib").
ri:addenv(def, {'COMPILER_VSN',42}).
ri:addenv(def, {'COMPILERVSN',42}).
ri:addenv(def, {'VSN',42}).
ri:addenv(def, {'vsn',42}).
ri:addenv(def, {'ORBVSN',42}).
ri:addenv(def, {'version',42}).
ri:addenv(def, {'default_verbosity', 42}).
ri:addenv(def, {'erlang_daemon_port',42}).
ri:addenv(def, {'epmd_dist_low',42}).
ri:addenv(def, {'epmd_dist_high',42}).
ri:addenv(def, {'HIPE_SYSTEM_CRC', 42}).
ri:addenv(def, {'ARM_NR_ARG_REGS', 42}).
ri:addenv(include, OTP_ROOT ++ "erts/include").
ri:addenv(include, OTP_ROOT ++ "lib/common_test/include").
ri:addenv(include, OTP_ROOT ++ "lib/cosEvent/include").
ri:addenv(include, OTP_ROOT ++ "lib/cosEventDomain/include").
ri:addenv(include, OTP_ROOT ++ "lib/cosFileTransfer/include").
ri:addenv(include, OTP_ROOT ++ "lib/cosNotification/include").
ri:addenv(include, OTP_ROOT ++ "lib/cosProperty/include").
ri:addenv(include, OTP_ROOT ++ "lib/cosTime/include").
ri:addenv(include, OTP_ROOT ++ "lib/cosTransactions/include").
ri:addenv(include, OTP_ROOT ++ "lib/cosTransactions/src").
ri:addenv(include, OTP_ROOT ++ "lib/dialyzer/src").
ri:addenv(include, OTP_ROOT ++ "lib/edoc/include").
ri:addenv(include, OTP_ROOT ++ "lib/erl_interface/include").
ri:addenv(include, OTP_ROOT ++ "lib/et/include").
ri:addenv(include, OTP_ROOT ++ "lib/eunit/include").
ri:addenv(include, OTP_ROOT ++ "lib/hipe/arm").
ri:addenv(include, OTP_ROOT ++ "lib/hipe/cerl").
ri:addenv(include, OTP_ROOT ++ "lib/hipe/flow").
ri:addenv(include, OTP_ROOT ++ "lib/hipe/icode").
ri:addenv(include, OTP_ROOT ++ "lib/hipe/main").
ri:addenv(include, OTP_ROOT ++ "lib/hipe/misc").
ri:addenv(include, OTP_ROOT ++ "lib/hipe/ppc").
ri:addenv(include, OTP_ROOT ++ "lib/hipe/rtl").
ri:addenv(include, OTP_ROOT ++ "lib/hipe/sparc").
ri:addenv(include, OTP_ROOT ++ "lib/hipe/util").
ri:addenv(include, OTP_ROOT ++ "lib/hipe/x86").
ri:addenv(include, OTP_ROOT ++ "lib/ic/include").
ri:addenv(include, OTP_ROOT ++ "lib/inets/src/inets_app").
ri:addenv(include, OTP_ROOT ++ "lib/inets/src/http_lib").
ri:addenv(include, OTP_ROOT ++ "lib/inviso/include").
ri:addenv(include, OTP_ROOT ++ "lib/kernel/include").
ri:addenv(include, OTP_ROOT ++ "lib/megaco/include").
ri:addenv(include, OTP_ROOT ++ "lib/mnesia/include").
ri:addenv(include, OTP_ROOT ++ "lib/observer/include").
ri:addenv(include, OTP_ROOT ++ "lib/odbc/include").
ri:addenv(include, OTP_ROOT ++ "lib/orber/include").
ri:addenv(include, OTP_ROOT ++ "lib/os_mon/include").
ri:addenv(include, OTP_ROOT ++ "lib/otp_mibs/include").
ri:addenv(include, OTP_ROOT ++ "lib/parsetools/include").
ri:addenv(include, OTP_ROOT ++ "lib/percept/include").
ri:addenv(include, OTP_ROOT ++ "lib/public_key/include").
ri:addenv(include, OTP_ROOT ++ "lib/runtime_tools/include").
ri:addenv(include, OTP_ROOT ++ "lib/sasl/include").
ri:addenv(include, OTP_ROOT ++ "lib/snmp/include").
ri:addenv(include, OTP_ROOT ++ "lib/snmp/src/misc").
ri:addenv(include, OTP_ROOT ++ "lib/snmp/src/compile").
ri:addenv(include, OTP_ROOT ++ "lib/ssl/include").
ri:addenv(include, OTP_ROOT ++ "lib/stdlib/include").
ri:addenv(include, OTP_ROOT ++ "lib/test_server/include").
ri:addenv(include, OTP_ROOT ++ "lib/xmerl/include").
```
Then load the ```src``` directory:
```erlang
> ri:add(otp, mnesia).
```

## Dynfun analysis

To run the built-in dynamic function analysis call the interface function:
```erlang
> ri:anal_dyn().
```



