%%% -*- erlang-indent-level: 2 -*-
%%% $Id$

%%---------------------------------------------------------------------
%% THESE DO NOT REALLY BELONG HERE -- PLEASE REMOVE ASAP!
%%---------------------------------------------------------------------

-type(dict()    :: tuple()).
-type(gb_set()  :: tuple()).
-type(gb_tree() :: tuple()).

%%%--------------------------------------------------------------------
%%% Basic Values:
%%%
%%% temp	::= {ppc_temp, reg, type, allocatable}
%%% reg		::= <token from hipe_ppc_registers>
%%% type	::= tagged | untagged
%%% allocatable	::= true | false
%%%
%%% sdesc	::= {ppc_sdesc, exnlab, fsize, arity, live}
%%% exnlab	::= [] | label
%%% fsize	::= int32		(frame size in words)
%%% live	::= <tuple of int32>	(word offsets)
%%% arity	::= uint8
%%%
%%% mfa		::= {ppc_mfa, atom, atom, arity}
%%% prim	::= {ppc_prim, atom}

-record(ppc_mfa, {m::atom(), f::atom(), a::byte()}).
-record(ppc_prim, {prim}).
-record(ppc_sdesc, {exnlab, fsize, arity, live}).
-record(ppc_simm16, {value}).
-record(ppc_temp, {reg, type, allocatable}).
-record(ppc_uimm16, {value}).

%%% Instruction Operands:
%%%
%%% aluop	::= add | add. | addi | addic. | addis | addo. | subf | subf. | subfo.
%%%		  | and | and. | andi. | or | or. | ori | xor | xor. | xori
%%%		  | slw | slw. | slwi | slwi. | srw | srw. | srwi | srwi.
%%%		  | sraw | sraw. | srawi | srawi. | mulli | mullw | mullw. | mullwo.
%%% bcond	::= eq | ne | gt | ge | lt | le | so | ns
%%% cmpop	::= cmp | cmpi | cmpl | cmpli
%%% ldop	::= lbz | lha | lhz | lwz
%%% ldxop	::= lbzx | lhax | lhzx | lwzx | lhbrx | lwbrx
%%% stop	::= stb | stw	(HW has sth, but we don't use it)
%%% stxop	::= stbx | stwx	(HW has sthx/sthbrx/stwbrx, but we don't use them)
%%% unop	::= extsb | extsh | {rlwinm,SH,MB,ME} | {rlwinm.,SH,MB,ME}
%%%
%%% immediate	::= int32 | atom | {label, label_type}
%%% label_type	::= constant | closure | c_const
%%%
%%% dst		::= temp
%%% src		::= temp
%%%		  | simm16 | uimm16	(only in alu.src2, cmp.src2)
%%% base	::= temp
%%% disp	::= sint16		(untagged simm16)
%%%
%%% fun		::= mfa | prim
%%% func	::= mfa | prim | 'ctr'
%%%
%%% spr		::= ctr | lr

%%% Instructions:

-record(alu, {aluop, dst, src1, src2}).
-record(b_fun, {'fun', linkage}).	% known tailcall
-record(b_label, {label}).	% local jump, unconditional
-record(bc, {bcond, label, pred}).	% local jump, conditional
-record(bctr, {labels}).	% computed tailcall or switch
-record(bctrl, {sdesc}).	% computed recursive call
-record(bl, {'fun', sdesc, linkage}).	% known recursive call
-record(blr, {}).		% unconditional bclr (return)
-record(cmp, {cmpop, src1, src2}).
-record(comment, {term}).
-record(label, {label}).
-record(load, {ldop, dst, disp, base}).	% non-indexed, non-update form
-record(loadx, {ldxop, dst, base1, base2}).	% indexed, non-update form
-record(mcrxr, {}).		% for clearing OV and SO in XER
-record(mfspr, {dst, spr}).	% for reading LR
-record(mtspr, {spr, src}).	% for writing LR and CTR
-record(pseudo_bc, {bcond, true_label, false_label, pred}).
-record(pseudo_call, {func, sdesc, contlab, linkage}).
-record(pseudo_call_prepare, {nrstkargs}).
-record(pseudo_li, {dst, imm}).
-record(pseudo_move, {dst, src}).
-record(pseudo_tailcall, {func, arity, stkargs, linkage}).
-record(pseudo_tailcall_prepare, {}).
-record(store, {stop, src, disp, base}).	% non-indexed, non-update form
-record(storex, {stxop, src, base1, base2}).% indexed, non-update form
-record(unary, {unop, dst, src}).
-record(lfd, {dst, disp, base}).
-record(lfdx, {dst, base1, base2}).
-record(stfd, {src, disp, base}).
-record(stfdx, {src, base1, base2}).
-record(fp_binary, {fp_binop, dst, src1, src2}).
-record(fp_unary, {fp_unop, dst, src}).
-record(pseudo_fmove, {dst, src}).

%%% Function definitions.

-record(defun, {mfa :: mfa(), formals, code, data,
		isclosure :: bool(), isleaf :: bool(),
		var_range, label_range}).
