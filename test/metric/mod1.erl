-module(mod1).

f() ->
    mod2:h(),
    mod2:h(),
    g().

g() ->
    f(),
    ok.

j() ->
    f(),
    apply(mod2, h, []).

k() ->
    mod3:l(),
    mod2:m(),
    g().
