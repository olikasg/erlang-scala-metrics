/* $Id$
 */
#ifndef HIPE_SPARC_GLUE_H
#define HIPE_SPARC_GLUE_H

#include "hipe_sparc_asm.h"		/* for NR_ARG_REGS, SPARC_LEAF_WORDS */
#define NR_LEAF_WORDS			SPARC_LEAF_WORDS
#define HIPE_ARCH_CALL_TO_NATIVE	hipe_sparc_call_to_native
#define HIPE_ARCH_RETURN_TO_NATIVE	hipe_sparc_return_to_native
#define HIPE_ARCH_TAILCALL_TO_NATIVE	hipe_sparc_tailcall_to_native
#define HIPE_ARCH_THROW_TO_NATIVE	hipe_sparc_throw_to_native
#include "hipe_risc_glue.h"

#endif /* HIPE_SPARC_GLUE_H */
