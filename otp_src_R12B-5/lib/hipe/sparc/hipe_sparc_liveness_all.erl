%%% -*- erlang-indent-level: 2 -*-
%%% $Id$

-module(hipe_sparc_liveness_all).
-export([analyse/1]).
-export([liveout/2]).

-include("hipe_sparc.hrl").
-include("../flow/liveness.inc").

analyse(CFG) -> analyze(CFG).
cfg_bb(CFG, L) -> hipe_sparc_cfg:bb(CFG, L).
cfg_postorder(CFG) -> hipe_sparc_cfg:postorder(CFG).
cfg_succ(CFG, L) -> hipe_sparc_cfg:succ(CFG, L).
uses(Insn) -> hipe_sparc_defuse:insn_use_all(Insn).
defines(Insn) -> hipe_sparc_defuse:insn_def_all(Insn).
liveout_no_succ() ->
  ordsets:from_list(lists:map(fun({Reg,Type}) ->
				  hipe_sparc:mk_temp(Reg, Type)
			      end,
			      hipe_sparc_registers:live_at_return())).
