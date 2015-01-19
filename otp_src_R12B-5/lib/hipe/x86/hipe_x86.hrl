%%% $Id$
%%% concrete representation of 2-address pseudo-x86 code

%%---------------------------------------------------------------------
%% THESE DO NOT REALLY BELONG HERE -- PLEASE REMOVE ASAP!
%%---------------------------------------------------------------------

-type(dict()    :: tuple()).
-type(gb_set()  :: tuple()).
-type(gb_tree() :: tuple()).

%%%--------------------------------------------------------------------
%%% x86 operands:
%%%
%%% int32	::= <a 32-bit integer>
%%% reg		::= <token from hipe_x86_registers module>
%%% type	::= 'tagged' | 'untagged'
%%% label	::= <an integer>
%%% label_type	::= 'label' | 'constant'
%%% aluop	::= <an atom denoting a binary alu op>
%%% term	::= <any Erlang term>
%%% cc		::= <an atom denoting a condition code>
%%% pred	::= <a real number between 0.0 and 1.0 inclusive>
%%% npop	::= <a 32-bit natural number which is a multiple of 4>
%%%
%%% temp	::= {x86_temp, reg, type, allocatable}
%%% allocatable ::= 'true' | 'false'
%%%
%%% imm		::= {x86_imm, value}
%%% value	::= int32 | atom | {label, label_type}
%%%
%%% mem		::= {x86_mem, base, off, mem_type}
%%% base	::= temp | []		(XXX BUG: not quite true before RA)
%%% off		::= imm | temp
%%% mem_type	::= 'byte' | 'int16'	(only valid with mov{s,z}x)
%%%		  | type
%%%
%%% src		::= temp | mem | imm
%%% dst		::= temp | mem
%%% arg		::= src
%%% args	::= <list of arg>
%%%
%%% mfa		::= {x86_mfa, atom, atom, byte}
%%% prim	::= {x86_prim, atom}
%%% fun		::= mfa | prim | temp | mem
%%%
%%% jtab	::= label	(equiv. to {x86_imm,{label,'constant'}})
%%%
%%% sdesc	::= {x86_sdesc, exnlab, fsize, arity, live}
%%% exnlab	::= [] | label
%%% fsize	::= <int32>		(frame size in words)
%%% live	::= <tuple of int32>	(word offsets)
%%% arity	::= int32

-record(x86_temp, {reg, type, allocatable}).
-record(x86_imm, {value}).
-record(x86_mem, {base, off, type}).
-record(x86_fpreg, {reg, pseudo}).
-record(x86_mfa, {m::atom(), f::atom(), a::byte()}).
-record(x86_prim, {prim}).
-record(x86_sdesc, {exnlab, fsize, arity, live::tuple()}).

%%% Basic instructions.
%%% These follow the AT&T convention, i.e. op src,dst (dst := dst op src)
%%% After register allocation, at most one operand in a binary
%%% instruction (alu, cmp, move) may denote a memory cell.
%%% After frame allocation, every temp must denote a physical register.

-record(alu, {aluop, src, dst}).
-record(call, {'fun', sdesc, linkage}).
-record(cmovcc, {cc, src, dst}).
-record(cmp, {src, dst}).       	% a 'sub' alu which doesn't update dst
-record(comment, {term}).
-record(fmove, {src, dst}).
-record(fp_binop, {op, src, dst}).
-record(fp_unop, {op, arg}).		% arg may be [] :-(
-record(imul, {imm_opt, src, temp}).	% imm_opt:[]|imm, src:temp|mem
-record(jcc, {cc, label}).
-record(jmp_fun, {'fun', linkage}).	% tailcall, direct or indirect
-record(jmp_label, {label}).		% local jmp, direct
-record(jmp_switch, {temp, jtab, labels}).	% local jmp, indirect
-record(label, {label}).
-record(lea, {mem, temp}).
-record(move, {src, dst}).
-record(move64, {imm, dst}).
-record(movsx, {src, dst}).
-record(movzx, {src, dst}).
-record(pseudo_call, {'fun', sdesc, contlab, linkage}).
-record(pseudo_jcc, {cc, true_label, false_label, pred}).
-record(pseudo_spill, {args=[]}).
-record(pseudo_tailcall, {'fun', arity, stkargs, linkage}).
-record(pseudo_tailcall_prepare, {}).
-record(push, {src}).
-record(pop, {dst}).
-record(ret, {npop}).			% EAX is live-in
-record(shift, {shiftop, src, dst}).
-record(test, {src, dst}).

%%% Function definitions.

-record(defun, {mfa :: mfa(), formals, code, data,
	       	isclosure :: bool(), isleaf :: bool(),
		var_range, label_range}).
