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
 * The Initial Developer of the Original Code is Ericsson AB.
 * Portions created by Ericsson are Copyright 2006, Ericsson AB.
 * All Rights Reserved.''
 * 
 *     $Id$
 */

/*
 * Native ethread spinlocks on SPARC V9.
 * Author: Mikael Pettersson.
 */
#ifndef ETHR_SPARC32_SPINLOCK_H
#define ETHR_SPARC32_SPINLOCK_H

/* Locked with ldstub, so unlocked when 0 and locked when non-zero. */
typedef struct {
    volatile unsigned char lock;
} ethr_native_spinlock_t;

#ifdef ETHR_TRY_INLINE_FUNCS

static ETHR_INLINE void
ethr_native_spinlock_init(ethr_native_spinlock_t *lock)
{
    lock->lock = 0;
}

static ETHR_INLINE void
ethr_native_spin_unlock(ethr_native_spinlock_t *lock)
{
    __asm__ __volatile__("membar #LoadStore|#StoreStore");
    lock->lock = 0;
}

static ETHR_INLINE int
ethr_native_spin_trylock(ethr_native_spinlock_t *lock)
{
    unsigned int prev;

    __asm__ __volatile__(
	"ldstub [%1], %0\n\t"
	"membar #StoreLoad|#StoreStore"
	: "=r"(prev)
	: "r"(&lock->lock)
	: "memory");
    return prev == 0;
}

static ETHR_INLINE int
ethr_native_spin_is_locked(ethr_native_spinlock_t *lock)
{
    return lock->lock != 0;
}

static ETHR_INLINE void
ethr_native_spin_lock(ethr_native_spinlock_t *lock)
{
    for(;;) {
	if (__builtin_expect(ethr_native_spin_trylock(lock) != 0, 1))
	    break;
	do {
	    __asm__ __volatile__("membar #LoadLoad");
	} while (ethr_native_spin_is_locked(lock));
    }
}

#endif /* ETHR_TRY_INLINE_FUNCS */

#endif /* ETHR_SPARC32_SPINLOCK_H */
