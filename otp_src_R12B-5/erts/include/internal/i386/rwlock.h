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
 * Native ethread rwlocks on x86/x86-64.
 * Author: Mikael Pettersson.
 *
 * This code requires a 486 or newer processor.
 */
#ifndef ETHREAD_I386_RWLOCK_H
#define ETHREAD_I386_RWLOCK_H

/* XXX: describe the algorithm */
typedef struct {
    volatile int lock;
} ethr_native_rwlock_t;

#ifdef ETHR_TRY_INLINE_FUNCS

#define ETHR_RWLOCK_OFFSET	(1<<24)

static ETHR_INLINE void
ethr_native_rwlock_init(ethr_native_rwlock_t *lock)
{
    lock->lock = 0;
}

static ETHR_INLINE void
ethr_native_read_unlock(ethr_native_rwlock_t *lock)
{
    __asm__ __volatile__(
	"lock; decl %0"
	: "=m"(lock->lock)
	: "m"(lock->lock));
}

static ETHR_INLINE int
ethr_native_read_trylock(ethr_native_rwlock_t *lock)
{
    int tmp;

    tmp = 1;
    __asm__ __volatile__(
	"lock; xaddl %0, %1"
	: "=r"(tmp)
	: "m"(lock->lock), "0"(tmp));
    /* tmp is now the lock's previous value */
    if (__builtin_expect(tmp >= 0, 1))
	return 1;
    ethr_native_read_unlock(lock);
    return 0;
}

static ETHR_INLINE int
ethr_native_read_is_locked(ethr_native_rwlock_t *lock)
{
    return lock->lock < 0;
}

static ETHR_INLINE void
ethr_native_read_lock(ethr_native_rwlock_t *lock)
{
    for(;;) {
	if (__builtin_expect(ethr_native_read_trylock(lock) != 0, 1))
	    break;
	do {
	    __asm__ __volatile__("rep;nop" : "=m"(lock->lock) : : "memory");
	} while (ethr_native_read_is_locked(lock));
    }
}

static ETHR_INLINE void
ethr_native_write_unlock(ethr_native_rwlock_t *lock)
{
    __asm__ __volatile__(
	"lock; addl %2,%0"
	: "=m"(lock->lock)
	: "m"(lock->lock), "i"(ETHR_RWLOCK_OFFSET));
}

static ETHR_INLINE int
ethr_native_write_trylock(ethr_native_rwlock_t *lock)
{
    int tmp;

    tmp = -ETHR_RWLOCK_OFFSET;
    __asm__ __volatile__(
	"lock; xaddl %0, %1"
	: "=r"(tmp)
	: "m"(lock->lock), "0"(tmp));
    /* tmp is now the lock's previous value */
    if (__builtin_expect(tmp == 0, 1))
	return 1;
    ethr_native_write_unlock(lock);
    return 0;
}

static ETHR_INLINE int
ethr_native_write_is_locked(ethr_native_rwlock_t *lock)
{
    return lock->lock != 0;
}

static ETHR_INLINE void
ethr_native_write_lock(ethr_native_rwlock_t *lock)
{
    for(;;) {
	if (__builtin_expect(ethr_native_write_trylock(lock) != 0, 1))
	    break;
	do {
	    __asm__ __volatile__("rep;nop" : "=m"(lock->lock) : : "memory");
	} while (ethr_native_write_is_locked(lock));
    }
}

#endif /* ETHR_TRY_INLINE_FUNCS */

#endif /* ETHREAD_I386_RWLOCK_H */
