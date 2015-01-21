-module(mod1).

f() ->
    mod2:h(),
    g().

g() ->
    ok.

j() ->
    f(),
    apply(mod2, h, []).

k() ->
    mod3:l(),
    mod2:m(),
    g().
