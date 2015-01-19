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
 * Portions created by Ericsson are Copyright 2007, Ericsson AB.
 * All Rights Reserved.''
 * 
 *     $Id$
 */


/*
 * Description:	Impementation of Erlang process locks.
 *
 * Author: 	Rickard Green
 */

/*
 * A short explanation of the process lock implementation:
 *     Each process has a lock bitfield and a number of lock wait
 *   queues.
 *     The bit field contains of a number of lock flags (L1, L2, ...)
 *   and a number of wait flags (W1, W2, ...). Each lock flag has a
 *   corresponding wait flag. The bit field isn't guarranteed to be
 *   larger than 32-bits which sets a maximum of 16 different locks
 *   per process. Currently, only 4 locks per process are used. The
 *   bit field is operated on by use of atomic operations (custom
 *   made bitwise atomic operations). When a lock is locked the
 *   corresponding lock bit is set. When a thread is waiting on a
 *   lock the wait flag for the lock is set.
 *     The process table is protected by pix (process index) locks
 *   which is spinlocks that protects a number of process indices in
 *   the process table. The pix locks also protects the lock queues
 *   and modifications of wait flags.
 *     When acquiring a process lock we first try to set the lock
 *   flag. If we are able to set the lock flag and the wait flag
 *   isn't set we are done. If the lock flag was already set we
 *   have to acquire the pix lock, set the wait flag, and put
 *   ourselves in the wait queue. If we are able to set the lock
 *   flag, but the wait flag was set, we have to acquire the pix
 *   lock and transfer the lock to the thread first in the wait
 *   queue. Process locks will always be acquired in fifo order.
 *     When releasing a process lock we first unset the lock flag
 *   (which will succeed). If the wait flag wasn't set we are done.
 *   If the wait flag was set, we have to reacquire the lock flag,
 *   acquire the pix lock, and transfer the lock to the first thread
 *   in the wait queue. It may not be possible to reacquire the
 *   the lock flag; another thread that was trying to lock the lock
 *   may have set it. When this happens the responsibility of the
 *   lock transfer has been taken over by the thread that set the
 *   lock flag.
 *     Note that wait flags may be read without the pix lock, but
 *   it is important that wait flags only are modified when the pix
 *   lock is held.
 *     This implementation assumes that erts_smp_atomic_or_retold()
 *   provides necessary memorybarriers for a lock operation, and that
 *   erts_smp_atomic_and_retold() provides necessary memorybarriers
 *   for an unlock operation.
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "erl_process.h"

const Process erts_proc_lock_busy;

#ifdef ERTS_SMP

/*#define ERTS_PROC_LOCK_SPIN_ON_GATE*/
#define ERTS_PROC_LOCK_SPIN_COUNT 2000

#include "erl_misc_utils.h"

#ifdef ERTS_PROC_LOCK_DEBUG
#define ERTS_PROC_LOCK_HARD_DEBUG
#endif

#ifdef ERTS_PROC_LOCK_HARD_DEBUG
static void check_queue(erts_proc_lock_t *lck);
#endif


typedef struct erts_proc_lock_waiter_t_ erts_proc_lock_waiter_t;
struct erts_proc_lock_waiter_t_ {
    erts_proc_lock_waiter_t *next;
    erts_proc_lock_waiter_t *prev;
    ErtsProcLocks wait_locks;
    erts_smp_gate_t gate;
    erts_proc_lock_queues_t *queues;
};

struct erts_proc_lock_queues_t_ {
    erts_proc_lock_queues_t *next;
    erts_proc_lock_waiter_t *queue[ERTS_PROC_LOCK_MAX_BIT+1];
};

struct erts_proc_lock_thr_spec_data_t_ {
    erts_proc_lock_queues_t *qs;
    erts_proc_lock_waiter_t *wtr;
};

static erts_proc_lock_queues_t zeroqs = {0};

static erts_smp_spinlock_t wtr_lock;
static erts_proc_lock_waiter_t *waiter_free_list;
static erts_proc_lock_queues_t *queue_free_list;
static erts_tsd_key_t waiter_key;

#ifdef ERTS_ENABLE_LOCK_CHECK
static struct {
    Sint16 proc_lock_main;
    Sint16 proc_lock_link;
    Sint16 proc_lock_msgq;
    Sint16 proc_lock_status;
} lc_id;
#endif

erts_pix_lock_t erts_pix_locks[ERTS_NO_OF_PIX_LOCKS];

static int proc_lock_spin_count;
static int proc_lock_trans_spin_cost;

static void cleanup_waiter(void);

void
erts_init_proc_lock(void)
{
    int i;
    int cpus;
    erts_smp_spinlock_init(&wtr_lock, "proc_lck_wtr_alloc");
    for (i = 0; i < ERTS_NO_OF_PIX_LOCKS; i++) {
#if ERTS_PROC_LOCK_MUTEX_IMPL
#ifdef ERTS_ENABLE_LOCK_COUNT
	erts_smp_mtx_init_x(&erts_pix_locks[i].u.mtx, "pix_lock", make_small(i));
#else
	erts_smp_mtx_init(&erts_pix_locks[i].u.mtx, "pix_lock");
#endif
#else
#ifdef ERTS_ENABLE_LOCK_COUNT
	erts_smp_spinlock_init_x(&erts_pix_locks[i].u.spnlck, "pix_lock", make_small(i));
#else
	erts_smp_spinlock_init(&erts_pix_locks[i].u.spnlck, "pix_lock");
#endif
#endif
    }
    waiter_free_list = NULL;
    queue_free_list = NULL;
    erts_tsd_key_create(&waiter_key);
    erts_thr_install_exit_handler(cleanup_waiter);
#ifdef ERTS_ENABLE_LOCK_CHECK
    lc_id.proc_lock_main	= erts_lc_get_lock_order_id("proc_main");
    lc_id.proc_lock_link	= erts_lc_get_lock_order_id("proc_link");
    lc_id.proc_lock_msgq	= erts_lc_get_lock_order_id("proc_msgq");
    lc_id.proc_lock_status	= erts_lc_get_lock_order_id("proc_status");
#endif
    cpus = erts_no_of_cpus();
    if (cpus > 1)
	proc_lock_spin_count = ERTS_PROC_LOCK_SPIN_COUNT;
    else if (cpus == 1)
	proc_lock_spin_count = 0;
    else /* No of cpus unknown. Assume multi proc, but be conservative. */
	proc_lock_spin_count = ERTS_PROC_LOCK_SPIN_COUNT/10;
    proc_lock_trans_spin_cost = proc_lock_spin_count/20;
}

static ERTS_INLINE erts_proc_lock_waiter_t *
alloc_wtr(void)
{
    erts_proc_lock_waiter_t *wtr;
    erts_smp_spin_lock(&wtr_lock);
    wtr = waiter_free_list;
    if (wtr) {
	waiter_free_list = wtr->next;
	ERTS_LC_ASSERT(queue_free_list);
	wtr->queues = queue_free_list;
	queue_free_list = wtr->queues->next;
	erts_smp_spin_unlock(&wtr_lock);
    }
    else {
	erts_smp_spin_unlock(&wtr_lock);
	wtr = erts_alloc(ERTS_ALC_T_PROC_LCK_WTR,
			 sizeof(erts_proc_lock_waiter_t));
	erts_smp_gate_init(&wtr->gate);
	wtr->wait_locks = (ErtsProcLocks) 0;
	wtr->queues = erts_alloc(ERTS_ALC_T_PROC_LCK_QS,
				 sizeof(erts_proc_lock_queues_t));
	sys_memcpy((void *) wtr->queues,
		   (void *) &zeroqs,
		   sizeof(erts_proc_lock_queues_t));
    }
    return wtr;
}

#ifdef ERTS_ENABLE_LOCK_CHECK
static void
check_unused_waiter(erts_proc_lock_waiter_t *wtr)
{
    int i;
    ERTS_LC_ASSERT(wtr->wait_locks == 0);
    for (i = 0; i <= ERTS_PROC_LOCK_MAX_BIT; i++)
	ERTS_LC_ASSERT(!wtr->queues->queue[i]);
}
#define CHECK_UNUSED_WAITER(W) check_unused_waiter((W))
#else
#define CHECK_UNUSED_WAITER(W)
#endif


static ERTS_INLINE void
free_wtr(erts_proc_lock_waiter_t *wtr)
{
    CHECK_UNUSED_WAITER(wtr);
    erts_smp_spin_lock(&wtr_lock);
    wtr->next = waiter_free_list;
    waiter_free_list = wtr;
    wtr->queues->next = queue_free_list;
    queue_free_list = wtr->queues;
    erts_smp_spin_unlock(&wtr_lock);
}

void
erts_proc_lock_prepare_proc_lock_waiter(void)
{
    erts_tsd_set(waiter_key, (void *) alloc_wtr());
}


static void
cleanup_waiter(void)
{
    erts_proc_lock_waiter_t *wtr = erts_tsd_get(waiter_key);
    if (wtr)
	free_wtr(wtr);
}


/*
 * Waiters are queued in a circular double linked list;
 * where qs->queue[lock_ix] is the first waiter in queue, and
 * qs->queue[lock_ix]->prev is the last waiter in queue.
 */

static ERTS_INLINE void
enqueue_waiter(erts_proc_lock_queues_t *qs,
	       int ix,
	       erts_proc_lock_waiter_t *wtr)
{
    if (!qs->queue[ix]) {
	qs->queue[ix] = wtr;
	wtr->next = wtr;
	wtr->prev = wtr;
    }
    else {
	ERTS_LC_ASSERT(qs->queue[ix]->next && qs->queue[ix]->prev);
	wtr->next = qs->queue[ix];
	wtr->prev = qs->queue[ix]->prev;
	wtr->prev->next = wtr;
	qs->queue[ix]->prev = wtr;
    }
}

static erts_proc_lock_waiter_t *
dequeue_waiter(erts_proc_lock_queues_t *qs, int ix)
{
    erts_proc_lock_waiter_t *wtr = qs->queue[ix];
    ERTS_LC_ASSERT(qs->queue[ix]);
    if (wtr->next == wtr) {
	ERTS_LC_ASSERT(qs->queue[ix]->prev == wtr);
	qs->queue[ix] = NULL;
    }
    else {
	ERTS_LC_ASSERT(wtr->next != wtr);
	ERTS_LC_ASSERT(wtr->prev != wtr);
	wtr->next->prev = wtr->prev;
	wtr->prev->next = wtr->next;
	qs->queue[ix] = wtr->next;
    }
    return wtr;
}

/*
 * Tries to aquire as many locks as possible in lock order,
 * and sets the wait flag on the first lock not possible to
 * aquire.
 *
 * Note: We need the pix lock during this operation. Wait
 *       flags are only allowed to be manipulated under pix
 *       lock.
 */
static ERTS_INLINE void
try_aquire(erts_proc_lock_t *lck, erts_proc_lock_waiter_t *wtr)
{
    ErtsProcLocks got_locks = (ErtsProcLocks) 0;
    ErtsProcLocks locks = wtr->wait_locks;
    int lock_no;

    ERTS_LC_ASSERT(lck->queues);
    ERTS_LC_ASSERT(got_locks != locks);

    for (lock_no = 0; lock_no <= ERTS_PROC_LOCK_MAX_BIT; lock_no++) {
	ErtsProcLocks lock = ((ErtsProcLocks) 1) << lock_no;
	if (locks & lock) {
	    ErtsProcLocks wflg, old_lflgs;
	    if (lck->queues->queue[lock_no]) {
		/* Others already waiting */
	    enqueue:
		ERTS_LC_ASSERT(ERTS_PROC_LOCK_FLGS_READ_(lck)
			       & (lock << ERTS_PROC_LOCK_WAITER_SHIFT));
		enqueue_waiter(lck->queues, lock_no, wtr);
		break;
	    }
	    wflg = lock << ERTS_PROC_LOCK_WAITER_SHIFT;
	    old_lflgs = ERTS_PROC_LOCK_FLGS_BOR_(lck, wflg | lock);
	    if (old_lflgs & lock) {
		/* Didn't get the lock */
		goto enqueue;
	    }
	    else {
		/* Got the lock */
		got_locks |= lock;
		ERTS_LC_ASSERT(!(old_lflgs & wflg));
		/* No one else can be waiting for the lock; remove wait flag */
		(void) ERTS_PROC_LOCK_FLGS_BAND_(lck, ~wflg);
		if (got_locks == locks)
		    break;
	    }
	}
    }

    wtr->wait_locks &= ~got_locks;
}

/*
 * Transfer 'trnsfr_lcks' held by this executing thread to other
 * threads waiting for the locks. When a lock has been transferred
 * we also have to try to aquire as many lock as possible for the
 * other thread.
 */
static int
transfer_locks(Process *p,
	       ErtsProcLocks trnsfr_lcks,
	       erts_pix_lock_t *pix_lock,
	       int unlock)
{
    int transferred = 0;
    erts_proc_lock_waiter_t *wake = NULL;
    erts_proc_lock_waiter_t *wtr;
    ErtsProcLocks unset_waiter = 0;
    ErtsProcLocks tlocks = trnsfr_lcks;
    int lock_no;

    ERTS_LC_ASSERT(erts_lc_pix_lock_is_locked(pix_lock));

#ifdef ERTS_PROC_LOCK_HARD_DEBUG
    check_queue(&p->lock);
#endif

    for (lock_no = 0; tlocks && lock_no <= ERTS_PROC_LOCK_MAX_BIT; lock_no++) {
	ErtsProcLocks lock = ((ErtsProcLocks) 1) << lock_no;
	if (tlocks & lock) {
	    erts_proc_lock_queues_t *qs = p->lock.queues;
	    /* Transfer lock */
#ifdef ERTS_ENABLE_LOCK_CHECK
	    tlocks &= ~lock;
#endif
	    ERTS_LC_ASSERT(ERTS_PROC_LOCK_FLGS_READ_(&p->lock)
			   & (lock << ERTS_PROC_LOCK_WAITER_SHIFT));
	    transferred++;
	    wtr = dequeue_waiter(qs, lock_no);
	    ERTS_LC_ASSERT(wtr);
	    if (!qs->queue[lock_no])
		unset_waiter |= lock;
	    ERTS_LC_ASSERT(wtr->wait_locks & lock);
	    wtr->wait_locks &= ~lock;
	    if (wtr->wait_locks)
		try_aquire(&p->lock, wtr);
	    if (!wtr->wait_locks) {
		/*
		 * The other tread got all locks it needs;
		 * need to wake it up.
		 */
		wtr->next = wake;
		wake = wtr;
	    }
	}

    }

    if (unset_waiter) {
	unset_waiter <<= ERTS_PROC_LOCK_WAITER_SHIFT;
	(void) ERTS_PROC_LOCK_FLGS_BAND_(&p->lock, ~unset_waiter);
    }

#ifdef ERTS_PROC_LOCK_HARD_DEBUG
    check_queue(&p->lock);
#endif

    ERTS_LC_ASSERT(tlocks == 0); /* We should have transferred all of them */

    if (!wake) {
	if (unlock)
	    erts_pix_unlock(pix_lock);
    }
    else {
	erts_pix_unlock(pix_lock);
    
	do {
	    erts_proc_lock_waiter_t *tmp = wake;
	    wake = wake->next;
	    erts_smp_gate_let_through(&tmp->gate, 1);
	} while (wake);

	if (!unlock)
	    erts_pix_lock(pix_lock);
    }
    return transferred;
}

/*
 * erts_proc_lock_failed() is called when erts_smp_proc_lock()
 * wasn't able to lock all locks. We may need to transfer locks
 * to waiters and wait for our turn on locks.
 */
void
erts_proc_lock_failed(Process *p,
		      erts_pix_lock_t *pixlck,
		      ErtsProcLocks locks,
		      ErtsProcLocks old_lflgs)
{
    ErtsProcLocks need_locks, tlocks, got_locks, unlock_locks, olflgs;
    int tsd;
    erts_proc_lock_waiter_t *wtr;
    erts_pix_lock_t *pix_lock = pixlck ? pixlck : ERTS_PID2PIXLOCK(p->id);
    ErtsProcLocks mask = ERTS_PROC_LOCKS_ALL;
    int lock_no;
#ifndef ERTS_PROC_LOCK_SPIN_ON_GATE
    int spin_count = proc_lock_spin_count;
#endif

    need_locks = locks;
    got_locks = 0;
    olflgs = old_lflgs;

#ifndef ERTS_PROC_LOCK_SPIN_ON_GATE
    while (1) {
	ErtsProcLocks wflgs;
#endif

	/*
	 * Find out locks that need to be transfered, i.e., locks that had
	 * wait flag but previously wasn't locked.
	 *
	 * This situation only appear if someone just unlocked the locks
	 * and we set the lock flag before the unlocking thread was able
	 * to transfer the locks. We got the responsibilty to transfer the
	 * locks.
	 */

	/* Locks with wait flags ... */
	tlocks = olflgs >> ERTS_PROC_LOCK_WAITER_SHIFT;
	/* ... that we locked */
	tlocks &= need_locks & ~olflgs;
	
	/* Locks we got */
	got_locks |= need_locks & ~tlocks & ~olflgs;

#ifndef ERTS_PROC_LOCK_SPIN_ON_GATE
	if (spin_count <= 0)
	    break;
	
	if (tlocks) {
	    int transferred;
	    /* Transfer locks we had waiters on */
#if ERTS_PROC_LOCK_ATOMIC_IMPL
	    erts_pix_lock(pix_lock);
#endif
	    /*
	     * If ERTS_PROC_LOCK_ATOMIC_IMPL, transfer_locks will
	     * unlock pix_lock; otherwise, not.
	     */
	    transferred = transfer_locks(p,
					 tlocks,
					 pix_lock,
					 ERTS_PROC_LOCK_ATOMIC_IMPL);
	    spin_count -= transferred * proc_lock_trans_spin_cost;
	}

	need_locks &= ~got_locks;

	ERTS_LC_ASSERT(need_locks);

	wflgs = need_locks << ERTS_PROC_LOCK_WAITER_SHIFT;
	olflgs = ERTS_PROC_LOCK_FLGS_BOR_(&p->lock, need_locks);

	if ((olflgs & (wflgs | need_locks)) == 0) {
#if !ERTS_PROC_LOCK_ATOMIC_IMPL
	    erts_pix_unlock(pix_lock);
#endif
	    return; /* got them all */
	}
	spin_count--;
#if !ERTS_PROC_LOCK_ATOMIC_IMPL
	erts_pix_unlock(pix_lock);
	erts_pix_lock(pix_lock);
#endif
    }

#endif

    /* Wait for locks... */

    /*
     * Determine which locks to unlock, i.e., locks that we got that
     * would cause a lock order violation if kept while waiting for
     * other locks needed.
     */
    mask = ERTS_PROC_LOCKS_ALL;
    for (lock_no = 0; lock_no <= ERTS_PROC_LOCK_MAX_BIT; lock_no++) {
	ErtsProcLocks lock = ((ErtsProcLocks) 1) << lock_no;
	if (need_locks & lock)
	    break;
	mask &= ~lock;
    }
    need_locks = mask & locks;
    ERTS_LC_ASSERT(need_locks);
    unlock_locks = need_locks & got_locks;

    wtr = erts_tsd_get(waiter_key);
    if (wtr)
	tsd = 1;
    else {
#if ERTS_PROC_LOCK_SPINLOCK_IMPL
	erts_pix_unlock(pix_lock);
#endif
	wtr = alloc_wtr();
	tsd = 0;
#if ERTS_PROC_LOCK_SPINLOCK_IMPL
	erts_pix_lock(pix_lock);
#endif
    }
    
#if ERTS_PROC_LOCK_ATOMIC_IMPL
    erts_pix_lock(pix_lock);
#endif

    ERTS_LC_ASSERT(erts_lc_pix_lock_is_locked(pix_lock));
	
    if (unlock_locks) {
	/*
	 * More wait flags might have appeared by now. Locks with
	 * waiters need to be transferred. Locks without waiters
	 * can be reset since we have the pix lock here (no
	 * new wait flags can appear until we release the pix
	 * lock).
	 */
	ErtsProcLocks wtd = ERTS_PROC_LOCK_FLGS_READ_(&p->lock);
	wtd >>= ERTS_PROC_LOCK_WAITER_SHIFT;
	wtd &= unlock_locks;
	tlocks |= wtd;
	unlock_locks &= ~wtd;
	if (unlock_locks) {
#ifdef ERTS_ENABLE_LOCK_CHECK
	    wtd =
#else
	    (void)
#endif

		ERTS_PROC_LOCK_FLGS_BAND_(&p->lock, ~unlock_locks);

#ifdef ERTS_ENABLE_LOCK_CHECK
	    wtd >>= ERTS_PROC_LOCK_WAITER_SHIFT;
	    wtd &= unlock_locks;
	    ERTS_LC_ASSERT(!wtd);
#endif
	}
    }

    if (tlocks) /* Transfer locks we had waiters on */
	(void) transfer_locks(p, tlocks, pix_lock, 0);

    wtr->wait_locks = need_locks;
    if (!p->lock.queues) {
	wtr->queues->next = NULL;
	p->lock.queues = wtr->queues;
    }
    else {
	wtr->queues->next = p->lock.queues->next;
	p->lock.queues->next = wtr->queues;
    }

#ifdef ERTS_PROC_LOCK_HARD_DEBUG
    check_queue(&p->lock);
#endif

    /* Try to aquire locks one at a time in lock order and set wait flag */
    try_aquire(&p->lock, wtr);

#ifdef ERTS_PROC_LOCK_HARD_DEBUG
    check_queue(&p->lock);
#endif

    if (wtr->wait_locks) { /* We didn't get them all; need to wait... */
	/* Got to wait for locks... */
	erts_pix_unlock(pix_lock);

	/*
	 * Wait for needed locks. When we return all needed locks have
	 * have been acquired by other threads and transfered to us.
	 */
#ifdef ERTS_PROC_LOCK_SPIN_ON_GATE
	erts_smp_gate_swait(&wtr->gate, proc_lock_spin_count);
#else
	erts_smp_gate_wait(&wtr->gate);
#endif

	erts_pix_lock(pix_lock);

    }

    ERTS_LC_ASSERT(p->lock.queues);
    if (p->lock.queues->next) {
	wtr->queues = p->lock.queues->next;
	p->lock.queues->next = wtr->queues->next;
    }
    else {
	wtr->queues = p->lock.queues;
	p->lock.queues = NULL;
    }

    ERTS_LC_ASSERT(locks == (ERTS_PROC_LOCK_FLGS_READ_(&p->lock) & locks));
    erts_pix_unlock(pix_lock);

    if (tsd)
	CHECK_UNUSED_WAITER(wtr);
    else
	free_wtr(wtr);
}

/*
 * erts_proc_trylock_failed() is called when erts_smp_proc_trylock()
 * wasn't able to lock all locks. We may need to transfer locks
 * to waiters.
 */
void
erts_proc_trylock_failed(Process *p,
			 erts_pix_lock_t *pixlck,
			 ErtsProcLocks locks,
			 ErtsProcLocks old_lflgs)
{
    ErtsProcLocks tlocks, wlocks, ulocks;
    erts_pix_lock_t *pix_lock;

    /* Locks with wait flags ... */
    tlocks = old_lflgs >> ERTS_PROC_LOCK_WAITER_SHIFT;
    /* ... that we locked */
    tlocks &= locks & ~old_lflgs;

    ulocks = locks & ~tlocks & ~old_lflgs;

    pix_lock = pixlck ? pixlck : ERTS_PID2PIXLOCK(p->id);

#if ERTS_PROC_LOCK_ATOMIC_IMPL
    erts_pix_lock(pix_lock);
#endif
    
    ERTS_LC_ASSERT(erts_lc_pix_lock_is_locked(pix_lock));

    /*
     * More wait flags might have appeard by now. Locks with
     * waiters need to be transferred. Locks without waiters
     * can be reset since we have the pix lock here (no new
     * wait flags can appear until we release the pix lock).
     */

    wlocks = ERTS_PROC_LOCK_FLGS_READ_(&p->lock);
    wlocks >>= ERTS_PROC_LOCK_WAITER_SHIFT;
    wlocks &= ulocks; /* New locks with wait flags ... */
    tlocks |= wlocks; /* ... to transfer ... */
    ulocks &= ~wlocks; /* ... not to unlock. */

    if (ulocks) {
#ifdef ERTS_ENABLE_LOCK_CHECK
	wlocks =
#else
	(void)
#endif

	    ERTS_PROC_LOCK_FLGS_BAND_(&p->lock, ~ulocks);

#ifdef ERTS_ENABLE_LOCK_CHECK
	wlocks >>= ERTS_PROC_LOCK_WAITER_SHIFT;
	wlocks &= ulocks;
	ASSERT(!wlocks);
#endif
    }

    if (tlocks)
	(void) transfer_locks(p, tlocks, pix_lock, 1); /* unlocks pix_lock */
    else
	erts_pix_unlock(pix_lock);
}

/*
 * erts_proc_unlock_failed() is called when erts_smp_proc_unlock()
 * wasn't able to unlock all locks. We may need to transfer locks
 * to waiters.
 */
void
erts_proc_unlock_failed(Process *p,
			erts_pix_lock_t *pixlck,
			ErtsProcLocks wait_locks)
{
    ErtsProcLocks lcks = wait_locks, old_lflgs;
    erts_pix_lock_t *pix_lock = pixlck ? pixlck : ERTS_PID2PIXLOCK(p->id);;

#if ERTS_PROC_LOCK_ATOMIC_IMPL
    erts_pix_lock(pix_lock);
#endif

    ERTS_LC_ASSERT(erts_lc_pix_lock_is_locked(pix_lock));
    
    /*
     * Try to acquire the locks that we just released that other were
     * waiting for. If another thread acquires one of these lock flags
     * before us, it gets the responsibility to do the transfer.
     */
    
    old_lflgs = ERTS_PROC_LOCK_FLGS_BOR_(&p->lock, lcks);
    lcks &= ~old_lflgs; /* locks we was able to aquire */
    if (!lcks)
	erts_pix_unlock(pix_lock);
    else {
	ErtsProcLocks no_wtr;
	/*
	 * Waiters might have disapeared by now, since we haven't had the
	 * pix lock all the time.
	 */
	no_wtr = lcks & (~old_lflgs >> ERTS_PROC_LOCK_WAITER_SHIFT);
	if (no_wtr) {
	    /* No new waiters can appear since we have the pix lock */
	    (void) ERTS_PROC_LOCK_FLGS_BAND_(&p->lock, ~no_wtr);
	    lcks &= ~no_wtr;
	}
	if (lcks)
	    (void) transfer_locks(p, lcks, pix_lock, 1); /* unlocks pix_lock */
	else
	    erts_pix_unlock(pix_lock);
    }
    
}

/*
 * proc_safelock() locks process locks on two processes. In order
 * to avoid a deadlock, proc_safelock() unlocks those locks that
 * needs to be unlocked,  and then acquires locks in lock order
 * (including the previously unlocked ones).
 */

static void
proc_safelock(Process *a_proc,
	      erts_pix_lock_t *a_pix_lck,
	      ErtsProcLocks a_have_locks,
	      ErtsProcLocks a_need_locks,
	      Process *b_proc,
	      erts_pix_lock_t *b_pix_lck,
	      ErtsProcLocks b_have_locks,
	      ErtsProcLocks b_need_locks)
{
    Process *p1, *p2;
    Eterm pid1, pid2;
    erts_pix_lock_t *pix_lck1, *pix_lck2;
    ErtsProcLocks need_locks1, have_locks1, need_locks2, have_locks2;
    ErtsProcLocks unlock_mask;
    int lock_no, refc1 = 0, refc2 = 0;

    ERTS_LC_ASSERT(b_proc);


    /* Determine inter process lock order...
     * Locks with the same lock order should be locked on p1 before p2.
     */
    if (a_proc) {
	if (a_proc->id < b_proc->id) {
	    p1 = a_proc;
	    pid1 = a_proc->id;
	    pix_lck1 = a_pix_lck;
	    need_locks1 = a_need_locks;
	    have_locks1 = a_have_locks;
	    p2 = b_proc;
	    pid2 = b_proc->id;
	    pix_lck2 = b_pix_lck;
	    need_locks2 = b_need_locks;
	    have_locks2 = b_have_locks;
	}
	else if (a_proc->id > b_proc->id) {
	    p1 = b_proc;
	    pid1 = b_proc->id;
	    pix_lck1 = b_pix_lck;
	    need_locks1 = b_need_locks;
	    have_locks1 = b_have_locks;
	    p2 = a_proc;
	    pid2 = a_proc->id;
	    pix_lck2 = a_pix_lck;
	    need_locks2 = a_need_locks;
	    have_locks2 = a_have_locks;
	}
	else {
	    ERTS_LC_ASSERT(a_proc == b_proc);
	    ERTS_LC_ASSERT(a_proc->id == b_proc->id);
	    p1 = a_proc;
	    pid1 = a_proc->id;
	    pix_lck1 = a_pix_lck;
	    need_locks1 = a_need_locks | b_need_locks;
	    have_locks1 = a_have_locks | b_have_locks;
	    p2 = NULL;
	    pid2 = 0;
	    pix_lck2 = NULL;
	    need_locks2 = 0;
	    have_locks2 = 0;
	}
    }
    else {
	p1 = b_proc;
	pid1 = b_proc->id;
	pix_lck1 = b_pix_lck;
	need_locks1 = b_need_locks;
	have_locks1 = b_have_locks;
	p2 = NULL;
	pid2 = 0;
	pix_lck2 = NULL;
	need_locks2 = 0;
	have_locks2 = 0;
#ifdef ERTS_ENABLE_LOCK_CHECK
	a_need_locks = 0;
	a_have_locks = 0;
#endif
    }

#ifdef ERTS_ENABLE_LOCK_CHECK
    if (p1)
	erts_proc_lc_chk_proc_locks(p1, have_locks1);
    if (p2)
	erts_proc_lc_chk_proc_locks(p2, have_locks2);

    if ((need_locks1 & have_locks1) != have_locks1)
	erts_lc_fail("Thread tries to release process lock(s) "
		     "on %T via erts_proc_safelock().", pid1);
    if ((need_locks2 & have_locks2) != have_locks2)
	erts_lc_fail("Thread tries to release process lock(s) "
		     "on %T via erts_proc_safelock().",
		     pid2);
#endif


    need_locks1 &= ~have_locks1;
    need_locks2 &= ~have_locks2;

    /* Figure out the range of locks that needs to be unlocked... */
    unlock_mask = ERTS_PROC_LOCKS_ALL;
    for (lock_no = 0;
	 lock_no <= ERTS_PROC_LOCK_MAX_BIT;
	 lock_no++) {
	ErtsProcLocks lock = (1 << lock_no);
	if (lock & need_locks1)
	    break;
	unlock_mask &= ~lock;
	if (lock & need_locks2)
	    break;
    }

    /* ... and unlock locks in that range... */
    if (have_locks1 || have_locks2) {
	ErtsProcLocks unlock_locks;
	unlock_locks = unlock_mask & have_locks1;
	if (unlock_locks) {
	    have_locks1 &= ~unlock_locks;
	    need_locks1 |= unlock_locks;
	    if (!have_locks1) {
		refc1 = 1;
		erts_inc_proc_lock_refc(p1);
	    }
	    erts_smp_proc_unlock__(p1, pix_lck1, unlock_locks);
	}
	unlock_locks = unlock_mask & have_locks2;
	if (unlock_locks) {
	    have_locks2 &= ~unlock_locks;
	    need_locks2 |= unlock_locks;
	    if (!have_locks2) {
		refc2 = 1;
		erts_inc_proc_lock_refc(p2);
	    }
	    erts_smp_proc_unlock__(p2, pix_lck2, unlock_locks);
	}
    }

    /*
     * lock_no equals the number of the first lock to lock on
     * either p1 *or* p2.
     */


#ifdef ERTS_ENABLE_LOCK_CHECK
    if (p1)
	erts_proc_lc_chk_proc_locks(p1, have_locks1);
    if (p2)
	erts_proc_lc_chk_proc_locks(p2, have_locks2);
#endif

    /* Lock locks in lock order... */
    while (lock_no <= ERTS_PROC_LOCK_MAX_BIT) {
	ErtsProcLocks locks;
	ErtsProcLocks lock = (1 << lock_no);
	ErtsProcLocks lock_mask = 0;
	if (need_locks1 & lock) {
	    do {
		lock = (1 << lock_no++);
		lock_mask |= lock;
	    } while (lock_no <= ERTS_PROC_LOCK_MAX_BIT
		     && !(need_locks2 & lock));
	    if (need_locks2 & lock)
		lock_no--;
	    locks = need_locks1 & lock_mask;
	    erts_smp_proc_lock__(p1, pix_lck1, locks);
	    have_locks1 |= locks;
	    need_locks1 &= ~locks;
	}
	else if (need_locks2 & lock) {
	    while (lock_no <= ERTS_PROC_LOCK_MAX_BIT
		   && !(need_locks1 & lock)) {
		lock_mask |= lock;
		lock = (1 << ++lock_no);
	    }
	    locks = need_locks2 & lock_mask;
	    erts_smp_proc_lock__(p2, pix_lck2, locks);
	    have_locks2 |= locks;
	    need_locks2 &= ~locks;
	}
	else
	    lock_no++;
    }

#ifdef ERTS_ENABLE_LOCK_CHECK
    if (p1)
	erts_proc_lc_chk_proc_locks(p1, have_locks1);
    if (p2)
	erts_proc_lc_chk_proc_locks(p2, have_locks2);

    if (p1 && p2) {
	if (p1 == a_proc) {
	    ERTS_LC_ASSERT(a_need_locks == have_locks1);
	    ERTS_LC_ASSERT(b_need_locks == have_locks2);
	}
	else {
	    ERTS_LC_ASSERT(a_need_locks == have_locks2);
	    ERTS_LC_ASSERT(b_need_locks == have_locks1);
	}
    }
    else {
	ERTS_LC_ASSERT(p1);
	if (a_proc) {
	    ERTS_LC_ASSERT(have_locks1 == (a_need_locks | b_need_locks));
	}
	else {
	    ERTS_LC_ASSERT(have_locks1 == b_need_locks);
	}
    }
#endif

    if (refc1)
	erts_dec_proc_lock_refc(p1);
    if (refc2)
	erts_dec_proc_lock_refc(p2);
}

void
erts_proc_safelock(Process *a_proc,
		   ErtsProcLocks a_have_locks,
		   ErtsProcLocks a_need_locks,
		   Process *b_proc,
		   ErtsProcLocks b_have_locks,
		   ErtsProcLocks b_need_locks)
{
    proc_safelock(a_proc,
		  a_proc ? ERTS_PID2PIXLOCK(a_proc->id) : NULL,
		  a_have_locks,
		  a_need_locks,
		  b_proc,
		  b_proc ? ERTS_PID2PIXLOCK(b_proc->id) : NULL,
		  b_have_locks,
		  b_need_locks);
}

/*
 * erts_pid2proc_trylock_failed() is called when erts_pid2proc_opt()
 * has been called with ERTS_P2P_FLG_TRY_LOCK option and
 * erts_pid2proc_opt wasn't able to trylock all locks needed.
 */
void
erts_pid2proc_trylock_failed(Process *p,
			     erts_pix_lock_t *pix_lock,
			     ErtsProcLocks locks,
			     ErtsProcLocks wlocks,
			     ErtsProcLocks old_lflgs)
{
    ErtsProcLocks ulocks;

    ERTS_LC_ASSERT(pix_lock);
    ERTS_LC_ASSERT(erts_lc_pix_lock_is_locked(pix_lock));

    ulocks = locks & ~wlocks & ~old_lflgs;

    if (ulocks) {
#ifdef ERTS_ENABLE_LOCK_CHECK
	ErtsProcLocks w =
#else
	(void)
#endif
	    ERTS_PROC_LOCK_FLGS_BAND_(&p->lock, ~ulocks);

#ifdef ERTS_ENABLE_LOCK_CHECK
	w >>= ERTS_PROC_LOCK_WAITER_SHIFT;
	w &= ulocks;
	ASSERT(!w);
#endif
    }

    if (wlocks)
	(void) transfer_locks(p, wlocks, pix_lock, 1); /* unlocks pix_lock */
    else
	erts_pix_unlock(pix_lock);
}

/*
 * erts_pid2proc_safelock() is called from erts_pid2proc_opt() when
 * it wasn't possible to trylock all locks needed. 
 *   c_p		- current process
 *   c_p_have_locks	- locks held on c_p
 *   pid                - process id of process we are looking up
 *   proc               - process struct of process we are looking
 *			  up (both in and out argument)
 *   have_locks         - locks we were able to trylock
 *   need_locks         - all locks we need (including have_locks)
 *   wait_locks         - locks with waiters which we got the
 *                        responsibilty to transfer locks to
 *   pix_lock		- pix lock for process we are looking up
 *   flags		- option flags
 */
void
erts_pid2proc_safelock(Process *c_p,
		       ErtsProcLocks c_p_have_locks,
		       Process **proc,
		       ErtsProcLocks have_locks,
		       ErtsProcLocks need_locks,
		       ErtsProcLocks wait_locks,
		       erts_pix_lock_t *pix_lock,
		       int flags)
{
    Process *p = *proc;
    ERTS_LC_ASSERT(p->lock.refc > 0);
    ERTS_LC_ASSERT(process_tab[internal_pid_index(p->id)] == p);
    p->lock.refc++;
    if (wait_locks)
	(void) transfer_locks(p, wait_locks, pix_lock, 1); /* unlocks pix_lock */
    else
	erts_pix_unlock(pix_lock);

    proc_safelock(c_p,
		  c_p ? ERTS_PID2PIXLOCK(c_p->id) : NULL,
		  c_p_have_locks,
		  c_p_have_locks,
		  p,
		  pix_lock,
		  have_locks,
		  need_locks);

    erts_pix_lock(pix_lock);

    if (!p->is_exiting
	|| ((flags & ERTS_P2P_FLG_ALLOW_OTHER_X)
	    && process_tab[internal_pid_index(p->id)] == p)) {
	ERTS_LC_ASSERT(p->lock.refc > 1);
	p->lock.refc--;
    }
    else {
	/* No proc. Note, we need to keep refc until after process unlock */
	erts_pix_unlock(pix_lock);
	erts_smp_proc_unlock__(p, pix_lock, need_locks);
	*proc = NULL;
	erts_pix_lock(pix_lock);
	ERTS_LC_ASSERT(p->lock.refc > 0);
	if (--p->lock.refc == 0) {
	    erts_pix_unlock(pix_lock);
	    erts_free_proc(p);
	    erts_pix_lock(pix_lock);
	}
    }
}

void
erts_proc_lock_init(Process *p)
{
    /* We always start with all locks locked */
#if ERTS_PROC_LOCK_ATOMIC_IMPL
    erts_smp_atomic_init(&p->lock.flags, (long) ERTS_PROC_LOCKS_ALL);
#else
    p->lock.flags = ERTS_PROC_LOCKS_ALL;
#endif
    p->lock.queues = NULL;
    p->lock.refc = 1;
    
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_proc_lc_trylock(p, ERTS_PROC_LOCKS_ALL, 1);
#endif
#ifdef ERTS_PROC_LOCK_DEBUG
    {
	int i;
	for (i = 0; i <= ERTS_PROC_LOCK_MAX_BIT; i++)
	    erts_smp_atomic_init(&p->lock.locked[i], (long) 1);
    }
#endif
}

/* --- Process lock checking ----------------------------------------------- */

#ifdef ERTS_ENABLE_LOCK_CHECK

void
erts_proc_lc_lock(Process *p, ErtsProcLocks locks)
{
    erts_lc_lock_t lck = ERTS_LC_LOCK_INIT(-1,
					   p->id,
					   ERTS_LC_FLG_LT_PROCLOCK);
    if (locks & ERTS_PROC_LOCK_MAIN) {
	lck.id = lc_id.proc_lock_main;
	erts_lc_lock(&lck);
    }
    if (locks & ERTS_PROC_LOCK_LINK) {
	lck.id = lc_id.proc_lock_link;
	erts_lc_lock(&lck);
    }
    if (locks & ERTS_PROC_LOCK_MSGQ) {
	lck.id = lc_id.proc_lock_msgq;
	erts_lc_lock(&lck);
    }
    if (locks & ERTS_PROC_LOCK_STATUS) {
	lck.id = lc_id.proc_lock_status;
	erts_lc_lock(&lck);
    }
}

void
erts_proc_lc_trylock(Process *p, ErtsProcLocks locks, int locked)
{
    erts_lc_lock_t lck = ERTS_LC_LOCK_INIT(-1,
					   p->id,
					   ERTS_LC_FLG_LT_PROCLOCK);
    if (locks & ERTS_PROC_LOCK_MAIN) {
	lck.id = lc_id.proc_lock_main;
	erts_lc_trylock(locked, &lck);
    }
    if (locks & ERTS_PROC_LOCK_LINK) {
	lck.id = lc_id.proc_lock_link;
	erts_lc_trylock(locked, &lck);
    }
    if (locks & ERTS_PROC_LOCK_MSGQ) {
	lck.id = lc_id.proc_lock_msgq;
	erts_lc_trylock(locked, &lck);
    }
    if (locks & ERTS_PROC_LOCK_STATUS) {
	lck.id = lc_id.proc_lock_status;
	erts_lc_trylock(locked, &lck);
    }
}

void
erts_proc_lc_unlock(Process *p, ErtsProcLocks locks)
{
    erts_lc_lock_t lck = ERTS_LC_LOCK_INIT(-1,
					   p->id,
					   ERTS_LC_FLG_LT_PROCLOCK);
    if (locks & ERTS_PROC_LOCK_STATUS) {
	lck.id = lc_id.proc_lock_status;
	erts_lc_unlock(&lck);
    }
    if (locks & ERTS_PROC_LOCK_MSGQ) {
	lck.id = lc_id.proc_lock_msgq;
	erts_lc_unlock(&lck);
    }
    if (locks & ERTS_PROC_LOCK_LINK) {
	lck.id = lc_id.proc_lock_link;
	erts_lc_unlock(&lck);
    }
    if (locks & ERTS_PROC_LOCK_MAIN) {
	lck.id = lc_id.proc_lock_main;
	erts_lc_unlock(&lck);
    }
}

void
erts_proc_lc_might_unlock(Process *p, ErtsProcLocks locks)
{
    erts_lc_lock_t lck = ERTS_LC_LOCK_INIT(-1,
					   p->id,
					   ERTS_LC_FLG_LT_PROCLOCK);
    if (locks & ERTS_PROC_LOCK_STATUS) {
	lck.id = lc_id.proc_lock_status;
	erts_lc_might_unlock(&lck);
    }
    if (locks & ERTS_PROC_LOCK_MSGQ) {
	lck.id = lc_id.proc_lock_msgq;
	erts_lc_might_unlock(&lck);
    }
    if (locks & ERTS_PROC_LOCK_LINK) {
	lck.id = lc_id.proc_lock_link;
	erts_lc_might_unlock(&lck);
    }
    if (locks & ERTS_PROC_LOCK_MAIN) {
	lck.id = lc_id.proc_lock_main;
	erts_lc_might_unlock(&lck);
    }
}

void
erts_proc_lc_require_lock(Process *p, ErtsProcLocks locks)
{
    erts_lc_lock_t lck = ERTS_LC_LOCK_INIT(-1,
					   p->id,
					   ERTS_LC_FLG_LT_PROCLOCK);
    if (locks & ERTS_PROC_LOCK_MAIN) {
	lck.id = lc_id.proc_lock_main;
	erts_lc_require_lock(&lck);
    }
    if (locks & ERTS_PROC_LOCK_LINK) {
	lck.id = lc_id.proc_lock_link;
	erts_lc_require_lock(&lck);
    }
    if (locks & ERTS_PROC_LOCK_MSGQ) {
	lck.id = lc_id.proc_lock_msgq;
	erts_lc_require_lock(&lck);
    }
    if (locks & ERTS_PROC_LOCK_STATUS) {
	lck.id = lc_id.proc_lock_status;
	erts_lc_require_lock(&lck);
    }
}

void
erts_proc_lc_unrequire_lock(Process *p, ErtsProcLocks locks)
{
    erts_lc_lock_t lck = ERTS_LC_LOCK_INIT(-1,
					   p->id,
					   ERTS_LC_FLG_LT_PROCLOCK);
    if (locks & ERTS_PROC_LOCK_STATUS) {
	lck.id = lc_id.proc_lock_status;
	erts_lc_unrequire_lock(&lck);
    }
    if (locks & ERTS_PROC_LOCK_MSGQ) {
	lck.id = lc_id.proc_lock_msgq;
	erts_lc_unrequire_lock(&lck);
    }
    if (locks & ERTS_PROC_LOCK_LINK) {
	lck.id = lc_id.proc_lock_link;
	erts_lc_unrequire_lock(&lck);
    }
    if (locks & ERTS_PROC_LOCK_MAIN) {
	lck.id = lc_id.proc_lock_main;
	erts_lc_unrequire_lock(&lck);
    }
}


int
erts_proc_lc_trylock_force_busy(Process *p, ErtsProcLocks locks)
{
    if (locks & ERTS_PROC_LOCKS_ALL) {
	erts_lc_lock_t lck = ERTS_LC_LOCK_INIT(-1,
					       p->id,
					       ERTS_LC_FLG_LT_PROCLOCK);

	if (locks & ERTS_PROC_LOCK_MAIN)
	    lck.id = lc_id.proc_lock_main;
	else if (locks & ERTS_PROC_LOCK_LINK)
	    lck.id = lc_id.proc_lock_link;
	else if (locks & ERTS_PROC_LOCK_MSGQ)
	    lck.id = lc_id.proc_lock_msgq;
	else if (locks & ERTS_PROC_LOCK_STATUS)
	    lck.id = lc_id.proc_lock_status;
	else
	    erts_lc_fail("Unknown proc lock found");

	return erts_lc_trylock_force_busy(&lck);
    }
    return 0;
}

void erts_proc_lc_chk_only_proc_main(Process *p)
{
    erts_lc_lock_t proc_main = ERTS_LC_LOCK_INIT(lc_id.proc_lock_main,
						 p->id,
						 ERTS_LC_FLG_LT_PROCLOCK);
    erts_lc_check_exact(&proc_main, 1);
}

#define ERTS_PROC_LC_EMPTY_LOCK_INIT \
  ERTS_LC_LOCK_INIT(-1, THE_NON_VALUE, ERTS_LC_FLG_LT_PROCLOCK)

void
erts_proc_lc_chk_have_proc_locks(Process *p, ErtsProcLocks locks)
{
    int have_locks_len = 0;
    erts_lc_lock_t have_locks[4] = {ERTS_PROC_LC_EMPTY_LOCK_INIT,
				    ERTS_PROC_LC_EMPTY_LOCK_INIT,
				    ERTS_PROC_LC_EMPTY_LOCK_INIT,
				    ERTS_PROC_LC_EMPTY_LOCK_INIT};
    if (locks & ERTS_PROC_LOCK_MAIN) {
	have_locks[have_locks_len].id = lc_id.proc_lock_main;
	have_locks[have_locks_len++].extra = p->id;
    }
    if (locks & ERTS_PROC_LOCK_LINK) {
	have_locks[have_locks_len].id = lc_id.proc_lock_link;
	have_locks[have_locks_len++].extra = p->id;
    }
    if (locks & ERTS_PROC_LOCK_MSGQ) {
	have_locks[have_locks_len].id = lc_id.proc_lock_msgq;
	have_locks[have_locks_len++].extra = p->id;
    }
    if (locks & ERTS_PROC_LOCK_STATUS) {
	have_locks[have_locks_len].id = lc_id.proc_lock_status;
	have_locks[have_locks_len++].extra = p->id;
    }

    erts_lc_check(have_locks, have_locks_len, NULL, 0);
}

void
erts_proc_lc_chk_proc_locks(Process *p, ErtsProcLocks locks)
{
    int have_locks_len = 0;
    int have_not_locks_len = 0;
    erts_lc_lock_t have_locks[4] = {ERTS_PROC_LC_EMPTY_LOCK_INIT,
				    ERTS_PROC_LC_EMPTY_LOCK_INIT,
				    ERTS_PROC_LC_EMPTY_LOCK_INIT,
				    ERTS_PROC_LC_EMPTY_LOCK_INIT};
    erts_lc_lock_t have_not_locks[4] = {ERTS_PROC_LC_EMPTY_LOCK_INIT,
					ERTS_PROC_LC_EMPTY_LOCK_INIT,
					ERTS_PROC_LC_EMPTY_LOCK_INIT,
					ERTS_PROC_LC_EMPTY_LOCK_INIT};

    if (locks & ERTS_PROC_LOCK_MAIN) {
	have_locks[have_locks_len].id = lc_id.proc_lock_main;
	have_locks[have_locks_len++].extra = p->id;
    }
    else {
	have_not_locks[have_not_locks_len].id = lc_id.proc_lock_main;
	have_not_locks[have_not_locks_len++].extra = p->id;
    }
    if (locks & ERTS_PROC_LOCK_LINK) {
	have_locks[have_locks_len].id = lc_id.proc_lock_link;
	have_locks[have_locks_len++].extra = p->id;
    }
    else {
	have_not_locks[have_not_locks_len].id = lc_id.proc_lock_link;
	have_not_locks[have_not_locks_len++].extra = p->id;
    }
    if (locks & ERTS_PROC_LOCK_MSGQ) {
	have_locks[have_locks_len].id = lc_id.proc_lock_msgq;
	have_locks[have_locks_len++].extra = p->id;
    }
    else {
	have_not_locks[have_not_locks_len].id = lc_id.proc_lock_msgq;
	have_not_locks[have_not_locks_len++].extra = p->id;
    }
    if (locks & ERTS_PROC_LOCK_STATUS) {
	have_locks[have_locks_len].id = lc_id.proc_lock_status;
	have_locks[have_locks_len++].extra = p->id;
    }
    else {
	have_not_locks[have_not_locks_len].id = lc_id.proc_lock_status;
	have_not_locks[have_not_locks_len++].extra = p->id;
    }

    erts_lc_check(have_locks, have_locks_len,
		  have_not_locks, have_not_locks_len);
}

ErtsProcLocks
erts_proc_lc_my_proc_locks(Process *p)
{
    int resv[4];
    erts_lc_lock_t locks[4] = {ERTS_LC_LOCK_INIT(lc_id.proc_lock_main,
						 p->id,
						 ERTS_LC_FLG_LT_PROCLOCK),
			       ERTS_LC_LOCK_INIT(lc_id.proc_lock_link,
						 p->id,
						 ERTS_LC_FLG_LT_PROCLOCK),
			       ERTS_LC_LOCK_INIT(lc_id.proc_lock_msgq,
						 p->id,
						 ERTS_LC_FLG_LT_PROCLOCK),
			       ERTS_LC_LOCK_INIT(lc_id.proc_lock_status,
						 p->id,
						 ERTS_LC_FLG_LT_PROCLOCK)};

    ErtsProcLocks res = 0;

    erts_lc_have_locks(resv, locks, 4);
    if (resv[0])
	res |= ERTS_PROC_LOCK_MAIN;
    if (resv[1])
	res |= ERTS_PROC_LOCK_LINK;
    if (resv[2])
	res |= ERTS_PROC_LOCK_MSGQ;
    if (resv[3])
	res |= ERTS_PROC_LOCK_STATUS;

    return res;
}

void
erts_proc_lc_chk_no_proc_locks(char *file, int line)
{
    int resv[4];
    int ids[4] = {lc_id.proc_lock_main,
		  lc_id.proc_lock_link,
		  lc_id.proc_lock_msgq,
		  lc_id.proc_lock_status};
    erts_lc_have_lock_ids(resv, ids, 4);
    if (resv[0] || resv[1] || resv[2] || resv[3]) {
	erts_lc_fail("%s:%d: Thread has process locks locked when expected "
		     "not to have any process locks locked",
		     file, line);
    }
}

#endif /* #ifdef ERTS_ENABLE_LOCK_CHECK */

#ifdef ERTS_PROC_LOCK_HARD_DEBUG
void
check_queue(erts_proc_lock_t *lck)
{
    int lock_no;
    ErtsProcLocks lflgs = ERTS_PROC_LOCK_FLGS_READ_(lck);

    for (lock_no = 0; lock_no <= ERTS_PROC_LOCK_MAX_BIT; lock_no++) {
	ErtsProcLocks wtr;
	wtr = (((ErtsProcLocks) 1) << lock_no) << ERTS_PROC_LOCK_WAITER_SHIFT;
	if (lflgs & wtr) {
	    int n;
	    erts_proc_lock_waiter_t *wtr;
	    ERTS_LC_ASSERT(lck->queues && lck->queues->queue[lock_no]);
	    wtr = lck->queues->queue[lock_no];
	    n = 0;
	    do {
		wtr = wtr->next;
		n++;
	    } while (wtr != lck->queues->queue[lock_no]);
	    do {
		wtr = wtr->prev;
		n--;
	    } while (wtr != lck->queues->queue[lock_no]);
	    ERTS_LC_ASSERT(n == 0);
	}
	else {
	    ERTS_LC_ASSERT(!lck->queues || !lck->queues->queue[lock_no]);
	}
    }
}
#endif

#endif /* ERTS_SMP (the whole file) */
