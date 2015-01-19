%% -*- erlang-indent-level: 2 -*-
%%=====================================================================
%%
%% Driver module for register allocation of SPARC code.
%%
%%=====================================================================

-module(hipe_sparc_ra).
-export([allocate/3]).

-define(HIPE_INSTRUMENT_COMPILER, true). %% Turn on instrumentation.

-include("../main/hipe.hrl").
-include("hipe_sparc.hrl").

%%---------------------------------------------------------------------

allocate(_Fun, SparcCfg0, Options) ->
  ?opt_start_timer("Regalloc"),
  
  SparcCfg = hipe_sparc_multimove:remove_multimoves(SparcCfg0),
  {NewCfg, TempMap, NextPos}  = 
    case proplists:get_value(regalloc,Options) of
      linear_scan ->
	hipe_sparc_ra_ls:alloc(SparcCfg, Options);
      graph_color ->
	ra(SparcCfg, Options, hipe_graph_coloring_regalloc);
      coalescing ->
	ra(SparcCfg, Options, hipe_coalescing_regalloc);
      optimistic ->
	ra(SparcCfg, Options, hipe_optimistic_regalloc);
      naive ->
	hipe_sparc_ra_naive:alloc(SparcCfg, Options);
      _ -> %% linear_scan made default register allocator
	hipe_sparc_ra_ls:alloc(SparcCfg, Options)
    end,
  
  ?opt_stop_timer("Regalloc done"),

  {NewCfg2, FpMap, NextPos2} = 
    case get(hipe_inline_fp) of
      true ->  
	hipe_sparc_ra_fp_ls:alloc(NewCfg, Options, NextPos, TempMap);
      _ -> 
	{NewCfg, [], NextPos}
    end,

  {NewCfg3, NextPos3} = 
    hipe_sparc_caller_saves:rewrite(NewCfg2, TempMap, FpMap,
				    NextPos2, Options),

  {{NewCfg3,TempMap, NextPos3}, FpMap}.

%%---------------------------------------------------------------------
%%
%% Generic interface for register allocating SPARC code via a coloring
%% based scheme (graph_color, iterated or optimistic coalescing).
%%
%%---------------------------------------------------------------------

ra(SparcCfg, Options, Method) ->
  SpillLimit = hipe_gensym:get_var(sparc),
  ra(SparcCfg, SpillLimit, Options, Method).

ra(SparcCfg, SpillLimit, Options, Method) ->
  ?inc_counter(ra_iteration_counter,1), 
  {Map, _NewSpillIndex} = Method:regalloc(SparcCfg, 0, SpillLimit,
					  hipe_sparc_specific, Options),
  TempMap = cols2tuple(Map),
  
  {NewCfg, DontSpill} =
    hipe_sparc_ra_postconditions:rewrite(SparcCfg, TempMap, [], Options),
  
  case DontSpill of
    [] -> 
      %% Code to minimize stack size by allocation of temps to spillpositions
      {TempMap2, NewSpillIndex2} = 
	hipe_spillmin:stackalloc(NewCfg, [], 0, Options, 
				 hipe_sparc_specific, TempMap),
      TempMap3 =
	hipe_spillmin:mapmerge(hipe_temp_map:to_substlist(TempMap), 
			       TempMap2),
      TempMap4 = cols2tuple(TempMap3),
      ?add_spills(Options, NewSpillIndex2),
      {NewCfg, TempMap4, NewSpillIndex2};
    _ -> 
      %% Since SpillLimit is used as a low-water mark
      %% the list of temps not to spill is uninteresting.
      ra(NewCfg, SpillLimit, Options, Method)
  end.

%%
%% Converts a list of [{R1, C1}, {R2, C2}, ...} to a tuple {C17, C23, ...}.
%%
%% The N's must be unique but do not have to be sorted and they can be sparse.
%%

cols2tuple(Map) ->
  hipe_temp_map:cols2tuple(Map, hipe_sparc_specific).


-ifdef(BENCHMARKING).

%%---------------------------------------------------------------------
%% This is only an information gathering function used for benchmarking
%% purposes.
%%---------------------------------------------------------------------

count_caller_saves(CFG) ->
  Liveness = hipe_sparc_liveness:analyze(CFG),
  count_caller_saves(CFG, Liveness).

count_caller_saves(CFG, Liveness) ->
  %% Count how many temps are live over a call.
  length(
    %% Fold the count for each basic block.
    lists:foldr(
      %% For each BB, take the set of CallerSaves from previous BBs
      fun(L, CallerSaves) -> 
	  %% Just keep temps that are not precoloured.
	  [ X || 
	    %% Get the set of caller saves (from {Liveness, CS}).
	    X <- element(2,
	      %% Fold each instruction in the BB (backwards).
	      lists:foldr(
		%% For each instruction
		fun(I,{LiveOut,CS}) ->
		    %% Calculate live-in
		    UsesSet = ordsets:from_list(uses(I)),
		    DefsSet = ordsets:from_list(defines(I)),
		    LiveOverI = ordsets:subtract(LiveOut, DefsSet),      
		      NewCS = 
			case I of
			  %% If this is a call instruction, keep the CS-temps.
			  #call_link{} ->
			    ordsets:union(CS,LiveOverI);
			  _ -> CS
			end,
		    NewLiveOut = 
		      ordsets:union(LiveOverI, UsesSet),
		    {NewLiveOut,NewCS}
		end,
		%% Start with live out of the BB
		{ordsets:from_list(liveout(Liveness,L)),
		 CallerSaves},
		%% Get the instructions in the BB.
		hipe_bb:code(hipe_sparc_specific:bb(CFG,L)))),
	    %% Filter
	    not hipe_sparc_specific:is_precoloured(X)]	
      end,
      [],
      %% Get BBs
      hipe_sparc_specific:labels(CFG))).

liveout(Liveness, L) ->
  regnames(hipe_sparc_specific:liveout(Liveness, L)).

uses(I) ->
  regnames(hipe_sparc_specific:uses(I)).

defines(I) ->
  regnames(hipe_sparc_specific:defines(I)).

regnames(Regs2) ->
  Regs = hipe_sparc:keep_registers(Regs2),
  [hipe_sparc_specific:reg_nr(X) || X <- Regs].

%%--------------------------------------------------------------------- 
%% The following is needed for taking measurements.
%%--------------------------------------------------------------------- 

count_instrs_cfg(CFG) ->
  count_instrs(hipe_sparc:sparc_code(hipe_sparc_cfg:linearize(CFG))).

count_instrs(Code) ->
  count_instrs(Code, 0).

count_instrs([Instr|Is], Acc) ->
  count_instrs(Is, Acc + icount(Instr));
count_instrs([],Acc) -> Acc.

icount(I) ->
  case I of
    #label{} ->
      0;
    #comment{} ->
      0;
    #load_address{} ->
      2;
    #multimove{} ->
      %% These should have been removed before assembly...
      %% To be on the safe side, we calculate that we need one extra
      %% move (to a temp) for each register that need to be moved.
      %% I.e., [r1,r2] <- [r3, r4] can be handled as:
      %%            r5 <- r3
      %%            r6 <- r4
      %%            r1 <- r5
      %%            r2 <- r6
      length(hipe_sparc:multimove_dest(I)) * 2;
    _Other ->
      1
  end.

-endif.  %% BENCHMARKING
