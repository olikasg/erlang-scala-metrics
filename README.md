RefactorErl
===========

Repository for the original release of RefactorErl 0.9.14.09.

This repository contains modifications for my research.

## Loading mnesia into the database

You need to add the OTP ```lib``` directory as appbase:

```erlang
> ri:addenv(appbase, "/Users/olahgabor/Documents/git/refactorerl/otp_src_17.4/lib").
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



