changecom(`/*', `*/')dnl
/*
 * $Id$
 */

include(`hipe/hipe_arm_asm.m4')
#`include' "hipe_literals.h"

	.text
	.p2align 2

`#define JOIN3(A,B,C)		A##B##C
#define TEST_GOT_MBUF(ARITY)	ldr r1, [P, #P_MBUF]; cmp r1, #0; blne JOIN3(nbif_,ARITY,_gc_after_bif)'

/*
 * standard_bif_interface_1(nbif_name, cbif_name)
 * standard_bif_interface_2(nbif_name, cbif_name)
 * standard_bif_interface_3(nbif_name, cbif_name)
 *
 * Generate native interface for a BIF with 1-3 parameters and
 * standard failure mode.
 */
define(standard_bif_interface_1,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.global	$1
$1:
	/* Set up C argument registers. */
	mov	r0, P
	NBIF_ARG(r1,1,0)

	/* Save caller-save registers and call the C function. */
	SAVE_CONTEXT_BIF
	bl	$2
	TEST_GOT_MBUF(1)

	/* Restore registers. Check for exception. */
	cmp	r0, #THE_NON_VALUE
	RESTORE_CONTEXT_BIF
	beq	nbif_1_simple_exception
	NBIF_RET(1)
	.size	$1, .-$1
	.type	$1, %function
#endif')

define(standard_bif_interface_2,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.global	$1
$1:
	/* Set up C argument registers. */
	mov	r0, P
	NBIF_ARG(r1,2,0)
	NBIF_ARG(r2,2,1)

	/* Save caller-save registers and call the C function. */
	SAVE_CONTEXT_BIF
	bl	$2
	TEST_GOT_MBUF(2)

	/* Restore registers. Check for exception. */
	cmp	r0, #THE_NON_VALUE
	RESTORE_CONTEXT_BIF
	beq	nbif_2_simple_exception
	NBIF_RET(2)
	.size	$1, .-$1
	.type	$1, %function
#endif')

define(standard_bif_interface_3,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.global	$1
$1:
	/* Set up C argument registers. */
	mov	r0, P
	NBIF_ARG(r1,3,0)
	NBIF_ARG(r2,3,1)
	NBIF_ARG(r3,3,2)

	/* Save caller-save registers and call the C function. */
	SAVE_CONTEXT_BIF
	bl	$2
	TEST_GOT_MBUF(3)

	/* Restore registers. Check for exception. */
	cmp	r0, #THE_NON_VALUE
	RESTORE_CONTEXT_BIF
	beq	nbif_3_simple_exception
	NBIF_RET(3)
	.size	$1, .-$1
	.type	$1, %function
#endif')

/*
 * trap_bif_interface_0(nbif_name, cbif_name)
 *
 * Generate native interface for a BIF with 0 parameters and
 * trap-only failure mode.
 */
define(trap_bif_interface_0,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.global	$1
$1:
	/* Set up C argument registers. */
	mov	r0, P

	/* Save caller-save registers and call the C function. */
	SAVE_CONTEXT_BIF
	bl	$2
	TEST_GOT_MBUF(0)

	/* Restore registers. Check for exception. */
	cmp	r0, #THE_NON_VALUE
	RESTORE_CONTEXT_BIF
	beq	nbif_0_trap_exception
	NBIF_RET(0)
	.size	$1, .-$1
	.type	$1, %function
#endif')

/*
 * gc_bif_interface_0(nbif_name, cbif_name)
 * gc_bif_interface_1(nbif_name, cbif_name)
 * gc_bif_interface_2(nbif_name, cbif_name)
 *
 * Generate native interface for a BIF with 0-2 parameters and
 * standard failure mode.
 * The BIF may do a GC.
 */
define(gc_bif_interface_0,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.global	$1
$1:
	/* Set up C argument registers. */
	mov	r0, P

	/* Save caller-save registers and call the C function. */
	SAVE_CONTEXT_GC
	bl	$2
	TEST_GOT_MBUF(0)

	/* Restore registers. */
	RESTORE_CONTEXT_GC
	NBIF_RET(0)
	.size	$1, .-$1
	.type	$1, %function
#endif')

define(gc_bif_interface_1,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.global	$1
$1:
	/* Set up C argument registers. */
	mov	r0, P
	NBIF_ARG(r1,1,0)

	/* Save caller-save registers and call the C function. */
	SAVE_CONTEXT_GC
	bl	$2
	TEST_GOT_MBUF(1)

	/* Restore registers. Check for exception. */
	cmp	r0, #THE_NON_VALUE
	RESTORE_CONTEXT_GC
	beq	nbif_1_simple_exception
	NBIF_RET(1)
	.size	$1, .-$1
	.type	$1, %function
#endif')

define(gc_bif_interface_2,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.global	$1
$1:
	/* Set up C argument registers. */
	mov	r0, P
	NBIF_ARG(r1,2,0)
	NBIF_ARG(r2,2,1)

	/* Save caller-save registers and call the C function. */
	SAVE_CONTEXT_GC
	bl	$2
	TEST_GOT_MBUF(2)

	/* Restore registers. Check for exception. */
	cmp	r0, #THE_NON_VALUE
	RESTORE_CONTEXT_GC
	beq	nbif_2_simple_exception
	NBIF_RET(2)
	.size	$1, .-$1
	.type	$1, %function
#endif')

/*
 * gc_nofail_primop_interface_1(nbif_name, cbif_name)
 *
 * Generate native interface for a primop with implicit P
 * parameter, 1 ordinary parameter and no failure mode.
 * The primop may do a GC.
 */
define(gc_nofail_primop_interface_1,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.global	$1
$1:
	/* Set up C argument registers. */
	mov	r0, P
	NBIF_ARG(r1,1,0)

	/* Save caller-save registers and call the C function. */
	SAVE_CONTEXT_GC
	bl	$2

	/* Restore registers. */
	RESTORE_CONTEXT_GC
	NBIF_RET(1)
	.size	$1, .-$1
	.type	$1, %function
#endif')

/*
 * nofail_primop_interface_0(nbif_name, cbif_name)
 * nofail_primop_interface_1(nbif_name, cbif_name)
 * nofail_primop_interface_2(nbif_name, cbif_name)
 * nofail_primop_interface_3(nbif_name, cbif_name)
 *
 * Generate native interface for a primop with implicit P
 * parameter, 0-3 ordinary parameters and no failure mode.
 * Also used for guard BIFs.
 */
define(nofail_primop_interface_0,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.global	$1
$1:
	/* Set up C argument registers. */
	mov	r0, P

	/* Save caller-save registers and call the C function. */
	SAVE_CONTEXT_BIF
	bl	$2
	TEST_GOT_MBUF(0)

	/* Restore registers. */
	RESTORE_CONTEXT_BIF
	NBIF_RET(0)
	.size	$1, .-$1
	.type	$1, %function
#endif')

define(nofail_primop_interface_1,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.global	$1
$1:
	/* Set up C argument registers. */
	mov	r0, P
	NBIF_ARG(r1,1,0)

	/* Save caller-save registers and call the C function. */
	SAVE_CONTEXT_BIF
	bl	$2
	TEST_GOT_MBUF(1)

	/* Restore registers. */
	RESTORE_CONTEXT_BIF
	NBIF_RET(1)
	.size	$1, .-$1
	.type	$1, %function
#endif')

define(nofail_primop_interface_2,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.global	$1
$1:
	/* Set up C argument registers. */
	mov	r0, P
	NBIF_ARG(r1,2,0)
	NBIF_ARG(r2,2,1)

	/* Save caller-save registers and call the C function. */
	SAVE_CONTEXT_BIF
	bl	$2
	TEST_GOT_MBUF(2)

	/* Restore registers. */
	RESTORE_CONTEXT_BIF
	NBIF_RET(2)
	.size	$1, .-$1
	.type	$1, %function
#endif')

define(nofail_primop_interface_3,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.global	$1
$1:
	/* Set up C argument registers. */
	mov	r0, P
	NBIF_ARG(r1,3,0)
	NBIF_ARG(r2,3,1)
	NBIF_ARG(r3,3,2)

	/* Save caller-save registers and call the C function. */
	SAVE_CONTEXT_BIF
	bl	$2
	TEST_GOT_MBUF(3)

	/* Restore registers. */
	RESTORE_CONTEXT_BIF
	NBIF_RET(3)
	.size	$1, .-$1
	.type	$1, %function
#endif')

/*
 * nocons_nofail_primop_interface_0(nbif_name, cbif_name)
 * nocons_nofail_primop_interface_1(nbif_name, cbif_name)
 * nocons_nofail_primop_interface_2(nbif_name, cbif_name)
 * nocons_nofail_primop_interface_3(nbif_name, cbif_name)
 * nocons_nofail_primop_interface_5(nbif_name, cbif_name)
 *
 * Generate native interface for a primop with implicit P
 * parameter, 0-3 or 5 ordinary parameters, and no failure mode.
 * The primop cannot CONS or gc.
 */
define(nocons_nofail_primop_interface_0,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.global	$1
$1:
	/* Set up C argument registers. */
	mov	r0, P

	/* Perform a quick save;call;restore;ret sequence. */
	QUICK_CALL_RET($2,0)
	.size	$1, .-$1
	.type	$1, %function
#endif')

define(nocons_nofail_primop_interface_1,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.global	$1
$1:
	/* Set up C argument registers. */
	mov	r0, P
	NBIF_ARG(r1,1,0)

	/* Perform a quick save;call;restore;ret sequence. */
	QUICK_CALL_RET($2,1)
	.size	$1, .-$1
	.type	$1, %function
#endif')

define(nocons_nofail_primop_interface_2,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.global	$1
$1:
	/* Set up C argument registers. */
	mov	r0, P
	NBIF_ARG(r1,2,0)
	NBIF_ARG(r2,2,1)

	/* Perform a quick save;call;restore;ret sequence. */
	QUICK_CALL_RET($2,2)
	.size	$1, .-$1
	.type	$1, %function
#endif')

define(nocons_nofail_primop_interface_3,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.global	$1
$1:
	/* Set up C argument registers. */
	mov	r0, P
	NBIF_ARG(r1,3,0)
	NBIF_ARG(r2,3,1)
	NBIF_ARG(r3,3,2)

	/* Perform a quick save;call;restore;ret sequence. */
	QUICK_CALL_RET($2,3)
	.size	$1, .-$1
	.type	$1, %function
#endif')

define(nocons_nofail_primop_interface_5,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.global	$1
$1:
	/* Set up C argument stack. */
	NBIF_ARG(r0,5,3)
	str	r0, [sp, #0]
	NBIF_ARG(r0,5,4)
	str	r0, [sp, #4]

	/* Set up C argument registers. */
	mov	r0, P
	NBIF_ARG(r1,5,0)
	NBIF_ARG(r2,5,1)
	NBIF_ARG(r3,5,2)

	/* Perform a quick save;call;restore;ret sequence. */
	QUICK_CALL_RET($2,5)
	.size	$1, .-$1
	.type	$1, %function
#endif')

/*
 * noproc_primop_interface_0(nbif_name, cbif_name)
 * noproc_primop_interface_1(nbif_name, cbif_name)
 * noproc_primop_interface_2(nbif_name, cbif_name)
 * noproc_primop_interface_3(nbif_name, cbif_name)
 * noproc_primop_interface_5(nbif_name, cbif_name)
 *
 * Generate native interface for a primop with no implicit P
 * parameter, 0-3 or 5 ordinary parameters, and no failure mode.
 * The primop cannot CONS or gc.
 */
define(noproc_primop_interface_0,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.global	$1
$1:
	/* XXX: this case is always trivial; how to suppress the branch? */
	/* Perform a quick save;call;restore;ret sequence. */
	QUICK_CALL_RET($2,0)
	.size	$1, .-$1
	.type	$1, %function
#endif')

define(noproc_primop_interface_1,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.global	$1
$1:
	/* Set up C argument registers. */
	NBIF_ARG(r0,1,0)

	/* Perform a quick save;call;restore;ret sequence. */
	QUICK_CALL_RET($2,1)
	.size	$1, .-$1
	.type	$1, %function
#endif')

define(noproc_primop_interface_2,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.global	$1
$1:
	/* Set up C argument registers. */
	NBIF_ARG(r0,2,0)
	NBIF_ARG(r1,2,1)

	/* Perform a quick save;call;restore;ret sequence. */
	QUICK_CALL_RET($2,2)
	.size	$1, .-$1
	.type	$1, %function
#endif')

define(noproc_primop_interface_3,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.global	$1
$1:
	/* Set up C argument registers. */
	NBIF_ARG(r0,3,0)
	NBIF_ARG(r1,3,1)
	NBIF_ARG(r2,3,2)

	/* Perform a quick save;call;restore;ret sequence. */
	QUICK_CALL_RET($2,3)
	.size	$1, .-$1
	.type	$1, %function
#endif')

define(noproc_primop_interface_5,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.global	$1
$1:
	/* Set up C argument registers. */
	NBIF_ARG(r0,5,0)
	NBIF_ARG(r1,5,1)
	NBIF_ARG(r2,5,2)
	NBIF_ARG(r3,5,3)
	NBIF_ARG(r4,5,4)
	str	r4, [sp, #0]

	/* Perform a quick save;call;restore;ret sequence. */
	QUICK_CALL_RET($2,5)
	.size	$1, .-$1
	.type	$1, %function
#endif')

include(`hipe/hipe_bif_list.m4')
