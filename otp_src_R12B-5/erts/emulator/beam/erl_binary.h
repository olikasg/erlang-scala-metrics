/* ``The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved via the world wide web at http://www.erlang.org/.
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 * 
 * The Initial Developer of the Original Code is Ericsson Utvecklings AB.
 * Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
 * AB. All Rights Reserved.''
 * 
 *     $Id$
 */

#ifndef __ERL_BINARY_H
#define __ERL_BINARY_H

#include "erl_threads.h"

/*
 * Maximum number of bytes to place in a heap binary.
 */

#define ERL_ONHEAP_BIN_LIMIT 64

/*
 * This structure represents a SUB_BINARY.
 *
 * Note: The last field (orig) is not counted in arityval in the header.
 * This simplifies garbage collection.
 */

typedef struct erl_sub_bin {
    Eterm thing_word;		/* Subtag SUB_BINARY_SUBTAG. */
    Uint size;			/* Binary size in bytes. */
    Uint offs;			/* Offset into original binary. */
    byte bitsize; 
    byte bitoffs; 
    byte is_writable;		/* The underlying binary is writable */
    Eterm orig;			/* Original binary (REFC or HEAP binary). */
} ErlSubBin;

#define ERL_SUB_BIN_SIZE (sizeof(ErlSubBin)/sizeof(Eterm))
#define HEADER_SUB_BIN	_make_header(ERL_SUB_BIN_SIZE-2,_TAG_HEADER_SUB_BIN)

/*
 * This structure represents a HEAP_BINARY.
 */

typedef struct erl_heap_bin {
    Eterm thing_word;		/* Subtag HEAP_BINARY_SUBTAG. */
    Uint size;			/* Binary size in bytes. */
    Eterm data[1];		/* The data in the binary. */
} ErlHeapBin;

#define heap_bin_size(num_bytes)		\
  (sizeof(ErlHeapBin)/sizeof(Eterm) - 1 +	\
   ((num_bytes)+sizeof(Eterm)-1)/sizeof(Eterm))

#define header_heap_bin(num_bytes) \
  _make_header(heap_bin_size(num_bytes)-1,_TAG_HEADER_HEAP_BIN)

/*
 * Get the size in bytes of any type of binary.
 */

#define binary_size(Bin) (binary_val(Bin)[1])

#define binary_bitsize(Bin)			\
  ((*binary_val(Bin) == HEADER_SUB_BIN) ?	\
   ((ErlSubBin *) binary_val(Bin))->bitsize:	\
   0)

#define binary_bitoffset(Bin)			\
  ((*binary_val(Bin) == HEADER_SUB_BIN) ?	\
   ((ErlSubBin *) binary_val(Bin))->bitoffs:	\
   0)

/*
 * Get the pointer to the actual data bytes in a binary.
 * Works for any type of binary. Always use binary_bytes() if
 * you know that the binary cannot be a sub binary.
 *
 * Bin: input variable (Eterm)
 * Bytep: output variable (byte *)
 * Bitoffs: output variable (Uint)
 * Bitsize: output variable (Uint)
 */

#define ERTS_GET_BINARY_BYTES(Bin,Bytep,Bitoffs,Bitsize)		\
do {									\
    Eterm* _real_bin = binary_val(Bin);					\
    Uint _offs = 0;							\
    Bitoffs = Bitsize = 0;						\
    if (*_real_bin == HEADER_SUB_BIN) {					\
	ErlSubBin* _sb = (ErlSubBin *) _real_bin;			\
	_offs = _sb->offs;						\
        Bitoffs = _sb->bitoffs;						\
        Bitsize = _sb->bitsize;						\
	_real_bin = binary_val(_sb->orig);				\
    }									\
    if (*_real_bin == HEADER_PROC_BIN) {				\
	Bytep = ((ProcBin *) _real_bin)->bytes + _offs;			\
    } else {								\
	Bytep = (byte *)(&(((ErlHeapBin *) _real_bin)->data)) + _offs;	\
    }									\
} while (0)

/*
 * Get the real binary from any binary type, where "real" means
 * a REFC or HEAP binary. Also get the byte and bit offset into the
 * real binary. Useful if you want to build a SUB binary from
 * any binary.
 *
 * Bin: Input variable (Eterm)
 * RealBin: Output variable (Eterm)
 * ByteOffset: Output variable (Uint)
 * BitOffset: Offset in bits (Uint)
 * BitSize: Extra bit size (Uint)
 */

#define ERTS_GET_REAL_BIN(Bin, RealBin, ByteOffset, BitOffset, BitSize)	\
  do {									\
    ErlSubBin* _sb = (ErlSubBin *) binary_val(Bin);			\
    if (_sb->thing_word == HEADER_SUB_BIN) {				\
      RealBin = _sb->orig;						\
      ByteOffset = _sb->offs;						\
      BitOffset = _sb->bitoffs;						\
      BitSize = _sb->bitsize;						\
    } else {								\
      RealBin = Bin;							\
      ByteOffset = BitOffset = BitSize = 0;				\
    }									\
  } while (0)

/*
 * Get a pointer to the binary bytes, for a heap or refc binary
 * (NOT sub binary).
 */
#define binary_bytes(Bin)						\
  (*binary_val(Bin) == HEADER_PROC_BIN ?				\
   ((ProcBin *) binary_val(Bin))->bytes :				\
   (ASSERT_EXPR(thing_subtag(*binary_val(Bin)) == HEAP_BINARY_SUBTAG),	\
   (byte *)(&(((ErlHeapBin *) binary_val(Bin))->data))))

void erts_init_binary(void);

byte* erts_get_aligned_binary_bytes(Eterm, byte**);

#if defined(__i386__) || !defined(__GNUC__)
/*
 * Doubles aren't required to be 8-byte aligned on intel x86.
 * (if not gnuc we don't know if __i386__ is defined on x86;
 *  therefore, assume intel x86...)
 */
#  define ERTS_BIN_ALIGNMENT_MASK ((Uint) 3)
#else
#  define ERTS_BIN_ALIGNMENT_MASK ((Uint) 7)
#endif

#define ERTS_CHK_BIN_ALIGNMENT(B) \
  do { ASSERT(!(B) || (((Uint) &((Binary *)(B))->orig_bytes[0]) & ERTS_BIN_ALIGNMENT_MASK) == ((Uint) 0)) } while(0)

ERTS_GLB_INLINE void erts_free_aligned_binary_bytes(byte* buf);
ERTS_GLB_INLINE Binary *erts_bin_drv_alloc_fnf(Uint size);
ERTS_GLB_INLINE Binary *erts_bin_nrml_alloc(Uint size);
ERTS_GLB_INLINE Binary *erts_bin_realloc_fnf(Binary *bp, Uint size);
ERTS_GLB_INLINE Binary *erts_bin_realloc(Binary *bp, Uint size);
ERTS_GLB_INLINE void erts_bin_free(Binary *bp);
ERTS_GLB_INLINE Binary *erts_create_magic_binary(Uint size,
						 void (*destructor)(Binary *));

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE void
erts_free_aligned_binary_bytes(byte* buf)
{
    if (buf) {
	erts_free(ERTS_ALC_T_TMP, (void *) buf);
    }
}

ERTS_GLB_INLINE Binary *
erts_bin_drv_alloc_fnf(Uint size)
{
    Uint bsize = sizeof(Binary) - 1 + size;
    void *res;
    res = erts_alloc_fnf(ERTS_ALC_T_DRV_BINARY, bsize);
    ERTS_CHK_BIN_ALIGNMENT(res);
    return (Binary *) res;
}

ERTS_GLB_INLINE Binary *
erts_bin_nrml_alloc(Uint size)
{
    Uint bsize = sizeof(Binary) - 1 + size;
    void *res;
    res = erts_alloc_fnf(ERTS_ALC_T_BINARY, bsize);
    if (!res)
	erts_alloc_n_enomem(ERTS_ALC_T2N(ERTS_ALC_T_BINARY), bsize);
    ERTS_CHK_BIN_ALIGNMENT(res);
    return (Binary *) res;
}

ERTS_GLB_INLINE Binary *
erts_bin_realloc_fnf(Binary *bp, Uint size)
{
    Binary *nbp;
    Uint bsize = sizeof(Binary) - 1 + size;
    ASSERT((bp->flags & BIN_FLAG_MAGIC) == 0);
    if (bp->flags & BIN_FLAG_DRV)
	nbp = erts_realloc_fnf(ERTS_ALC_T_DRV_BINARY, (void *) bp, bsize);
    else
	nbp = erts_realloc_fnf(ERTS_ALC_T_BINARY, (void *) bp, bsize);
    ERTS_CHK_BIN_ALIGNMENT(nbp);
    return nbp;
}

ERTS_GLB_INLINE Binary *
erts_bin_realloc(Binary *bp, Uint size)
{
    Binary *nbp;
    Uint bsize = sizeof(Binary) - 1 + size;
    ASSERT((bp->flags & BIN_FLAG_MAGIC) == 0);
    if (bp->flags & BIN_FLAG_DRV)
	nbp = erts_realloc_fnf(ERTS_ALC_T_DRV_BINARY, (void *) bp, bsize);
    else
	nbp = erts_realloc_fnf(ERTS_ALC_T_BINARY, (void *) bp, bsize);
    if (!nbp)
	erts_realloc_n_enomem(ERTS_ALC_T2N(bp->flags & BIN_FLAG_DRV
					   ? ERTS_ALC_T_DRV_BINARY
					   : ERTS_ALC_T_BINARY),
			      bp,
			      bsize);
    ERTS_CHK_BIN_ALIGNMENT(nbp);
    return nbp;
}

ERTS_GLB_INLINE void
erts_bin_free(Binary *bp)
{
    if (bp->flags & BIN_FLAG_MAGIC)
	ERTS_MAGIC_BIN_DESTRUCTOR(bp)(bp);
    if (bp->flags & BIN_FLAG_DRV)
	erts_free(ERTS_ALC_T_DRV_BINARY, (void *) bp);
    else
	erts_free(ERTS_ALC_T_BINARY, (void *) bp);
}

ERTS_GLB_INLINE Binary *
erts_create_magic_binary(Uint size, void (*destructor)(Binary *))
{
    Uint bsize = sizeof(Binary) - 1 + sizeof(ErtsBinaryMagicPart) - 1 + size;
    Binary* bptr = erts_alloc_fnf(ERTS_ALC_T_BINARY, bsize);
    if (!bptr)
	erts_alloc_n_enomem(ERTS_ALC_T2N(ERTS_ALC_T_BINARY), bsize);
    ERTS_CHK_BIN_ALIGNMENT(bptr);
    bptr->flags = BIN_FLAG_MAGIC;
    bptr->orig_size = sizeof(ErtsBinaryMagicPart) - 1 + size;
    erts_refc_init(&bptr->refc, 0);
    ERTS_MAGIC_BIN_DESTRUCTOR(bptr) = destructor;
    return bptr;
}

#endif /* #if ERTS_GLB_INLINE_INCL_FUNC_DEF */

#endif
