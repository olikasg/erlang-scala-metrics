%%% -*- erlang-indent-level: 2 -*-
%%% $Id$
%%% Linear Scan register allocator for SPARC

-module(hipe_sparc_ra_ls).
-export([ra/3]).

ra(Defun, SpillIndex, Options) ->
  NewDefun = Defun, %% hipe_${ARCH}_ra_rename:rename(Defun,Options),
  CFG = hipe_sparc_cfg:init(NewDefun),
  SpillLimit = hipe_sparc_specific:number_of_temporaries(CFG),
  alloc(NewDefun, SpillIndex, SpillLimit, Options).

alloc(Defun, SpillIndex, SpillLimit, Options) ->
  CFG = hipe_sparc_cfg:init(Defun),
  {Coloring, _NewSpillIndex} =
    regalloc(
      CFG,
      hipe_sparc_registers:allocatable_gpr()--
      [hipe_sparc_registers:temp3(),
       hipe_sparc_registers:temp2(),
       hipe_sparc_registers:temp1()],
      [hipe_sparc_cfg:start_label(CFG)],
      SpillIndex, SpillLimit, Options,
      hipe_sparc_specific),
  {NewDefun, _DidSpill} =
    hipe_sparc_ra_postconditions:check_and_rewrite(
      Defun, Coloring, 'linearscan'),
  TempMap = hipe_temp_map:cols2tuple(Coloring, hipe_sparc_specific),
  {TempMap2,_NewSpillIndex2} =
    hipe_spillmin:stackalloc(CFG, [], SpillIndex, Options,
			     hipe_sparc_specific, TempMap),
  Coloring2 =
    hipe_spillmin:mapmerge(hipe_temp_map:to_substlist(TempMap), TempMap2),
  {NewDefun, Coloring2}.

regalloc(CFG, PhysRegs, Entrypoints, SpillIndex, DontSpill, Options, Target) ->
  hipe_ls_regalloc:regalloc(
    CFG, PhysRegs, Entrypoints, SpillIndex, DontSpill, Options, Target).
