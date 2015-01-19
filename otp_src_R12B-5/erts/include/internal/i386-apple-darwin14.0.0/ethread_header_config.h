/* include/internal/i386-apple-darwin14.0.0/ethread_header_config.h.  Generated by configure.  */
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

/* Define to the size of pointers */
#define ETHR_SIZEOF_PTR 8

/* Define if you want to disable native ethread implementations */
/* #undef ETHR_DISABLE_NATIVE_IMPLS */

/* Define if you have win32 threads */
/* #undef ETHR_WIN32_THREADS */

/* Define if you have pthreads */
#define ETHR_PTHREADS 1

/* Define if you have the <pthread.h> header file. */
#define ETHR_HAVE_PTHREAD_H 1

/* Define if the pthread.h header file is in pthread/mit directory. */
/* #undef ETHR_HAVE_MIT_PTHREAD_H */

/* Define if you have the pthread_mutexattr_settype function. */
#define ETHR_HAVE_PTHREAD_MUTEXATTR_SETTYPE 1

/* Define if you have the pthread_mutexattr_setkind_np function. */
/* #undef ETHR_HAVE_PTHREAD_MUTEXATTR_SETKIND_NP */

/* Define if you have the pthread_atfork function. */
#define ETHR_HAVE_PTHREAD_ATFORK 1

/* Define if you have the pthread_spin_lock function. */
/* #undef ETHR_HAVE_PTHREAD_SPIN_LOCK */

/* Define if you have the pthread_rwlock_init function */
#define ETHR_HAVE_PTHREAD_RWLOCK_INIT 1

/* Define if you want to turn on extra sanity checking in the ethread library */
/* #undef ETHR_XCHK */
