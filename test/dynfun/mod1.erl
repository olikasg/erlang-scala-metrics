-module(mod1).

-export([f1/0,
	 f2/0,
	 f3/0,
	 f4/0]).

-export([foo/3]).

f1() ->
    Mod = mod1,
    Fun = foo,
    Args = [1,2,3],
    apply(Mod, Fun, Args).

f2() ->
    apply(mod1, foo, [1,2,3]).

f3() ->
    apply(fun foo/3, [1,2,3]).

f4() ->
    Mod = mod1,
    Fun = foo,
    Args = [1,2,3],
    apply(fun Mod:Fun/3, Args).

f5() ->
    Fun = fun foo/3,
    apply(Fun, [1,2,3]).


foo(A, B, C) ->
    A + B + C.
