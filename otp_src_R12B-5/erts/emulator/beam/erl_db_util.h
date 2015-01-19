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
#ifndef _DB_UTIL_H
#define _DB_UTIL_H

#include "global.h"
#include "erl_message.h"

/*#define HARDDEBUG 1*/

#ifdef DEBUG
/*
** DMC_DEBUG does NOT need DEBUG, but DEBUG needs DMC_DEBUG
*/
#define DMC_DEBUG 1
#endif

/*
 * These values can be returned from the functions performing the 
 * BIF operation for different types of tables. When the
 * actual operations have been performed, the BIF function
 * checks for negative returns and issues BIF_ERRORS based 
 * upon these values.
 */
#define DB_ERROR_NONE      0     /* No error */
#define DB_ERROR_BADITEM  -1     /* The item was malformed ie no 
				   tuple or to small*/
#define DB_ERROR_BADTABLE -2     /* The Table is inconsisitent */
#define DB_ERROR_SYSRES   -3     /* Out of system resources */
#define DB_ERROR_BADKEY   -4     /* Returned if a key that should
				    exist does not. */
#define DB_ERROR_BADPARAM  -5     /* Returned if a specified slot does 
				     not exist (hash table only) or
				     the state parameter in db_match_object
				     is broken.*/
#define DB_ERROR_UNSPEC   -10    /* Unspecified error */


/*
 * A datatype for a database entry stored out of a process heap
 */
typedef struct db_term {
    ErlOffHeap off_heap;   /* Off heap data for term. */
    Uint size;		   /* Size of term in "words" */
    Eterm tpl[1];          /* Untagged "constant pointer" to top tuple */
                           /* (assumed to be first in buffer) */
} DbTerm;

/* "Assign" a value to DbTerm.tpl */
#define DBTERM_SET_TPL(dbtermPtr,tplPtr) ASSERT((tplPtr)==(dbtermPtr->tpl))
/* Get start of term buffer */
#define DBTERM_BUF(dbtermPtr) ((dbtermPtr)->tpl)

union db_table;
typedef union db_table DbTable;

/* Info about a database entry while it's being updated
 * (by update_counter or update_element)
 */
typedef struct {
    DbTable* tb;
    DbTerm* dbterm;
    void** bp;         /* {Hash|Tree}DbTerm** */
    Uint new_size;
    int mustFinalize;  /* Need to call db_finalize_update_element? */
} DbUpdateHandle;


typedef struct db_table_method
{
    int (*db_create)(Process *p, DbTable* tb);
    int (*db_first)(Process* p, 
		    DbTable* tb, /* [in out] */ 
		    Eterm* ret   /* [out] */);
    int (*db_next)(Process* p, 
		   DbTable* tb, /* [in out] */
		   Eterm key,   /* [in] */
		   Eterm* ret /* [out] */);
    int (*db_last)(Process* p, 
		   DbTable* tb, /* [in out] */
		   Eterm* ret   /* [out] */);
    int (*db_prev)(Process* p, 
		   DbTable* tb, /* [in out] */
		   Eterm key, 
		   Eterm* ret);
    int (*db_put)(Process* p, 
		  DbTable* tb, /* [in out] */ 
		  Eterm obj, 
		  Eterm* ret);
    int (*db_get)(Process* p, 
		  DbTable* tb, /* [in out] */ 
		  Eterm key, 
		  Eterm* ret);
    int (*db_get_element)(Process* p, 
			  DbTable* tb, /* [in out] */ 
			  Eterm key, 
			  int index, 
			  Eterm* ret);
    int (*db_member)(Process* p, 
		     DbTable* tb, /* [in out] */ 
		     Eterm key, 
		     Eterm* ret);
    int (*db_erase)(Process* p,
		    DbTable* tb,  /* [in out] */ 
		    Eterm key, 
		    Eterm* ret);
    int (*db_erase_object)(Process* p,
			   DbTable* tb, /* [in out] */ 
			   Eterm obj,
			   Eterm* ret);
    int (*db_slot)(Process* p, 
		   DbTable* tb, /* [in out] */ 
		   Eterm slot, 
		   Eterm* ret);
    int (*db_select_chunk)(Process* p, 
			   DbTable* tb, /* [in out] */ 
			   Eterm pattern,
			   Sint chunk_size,
			   int reverse, 
			   Eterm* ret);
    int (*db_select)(Process* p, 
		     DbTable* tb, /* [in out] */ 
		     Eterm pattern,
		     int reverse, 
		     Eterm* ret);
    int (*db_select_delete)(Process* p, 
			    DbTable* tb, /* [in out] */ 
			    Eterm pattern,
			    Eterm* ret);
    int (*db_select_continue)(Process* p, 
			      DbTable* tb, /* [in out] */ 
			      Eterm continuation,
			      Eterm* ret);
    int (*db_select_delete_continue)(Process* p, 
				     DbTable* tb, /* [in out] */ 
				     Eterm continuation,
				     Eterm* ret);
    int (*db_select_count)(Process* p, 
			   DbTable* tb, /* [in out] */ 
			   Eterm pattern, 
			   Eterm* ret);
    int (*db_select_count_continue)(Process* p, 
				    DbTable* tb, /* [in out] */ 
				    Eterm continuation, 
				    Eterm* ret);

    int (*db_delete_all_objects)(Process* p,
				 DbTable* db /* [in out] */ );

    int (*db_free_table)(DbTable* db /* [in out] */ );
    int (*db_free_table_continue)(DbTable* db, /* [in out] */  
				  int first);
    
    void (*db_print)(int to, 
		     void* to_arg, 
		     int show, 
		     DbTable* tb /* [in out] */ );

    void (*db_foreach_offheap)(DbTable* db,  /* [in out] */ 
			       void (*func)(ErlOffHeap *, void *),
			       void *arg);
    void (*db_check_table)(DbTable* tb);

    /* Allocate and replace a dbterm with a new size.
     * The new DbTerm must be initialized by caller (from the old).
    */
    DbTerm* (*db_alloc_newsize)(DbTable* tb,
				void** bp,  /* XxxDbTerm** */
				Uint new_tpl_sz);

    /* Free a dbterm not in table.
    */
    void (*db_free_dbterm)(DbTable* tb, DbTerm* bp);

    /* Lookup a dbterm by key. Return false if not found.
    */
    int (*db_lookup_dbterm)(DbTable*, Eterm key, 
			    DbUpdateHandle* handle); /* [out] */

} DbTableMethod;

/*
 * This structure contains data for all different types of database
 * tables. Note that these fields must match the same fields
 * in the table-type specific structures.
 * The reason it is placed here and not in db.h is that some table 
 * operations may be the same on different types of tables.
 */

typedef struct db_fixation {
    Eterm pid;
    Uint counter;
    struct db_fixation *next;
} DbFixation;


typedef struct db_table_common {
    erts_refc_t ref;    /* ref count ro prevent table deletion */
#ifdef ERTS_SMP
    erts_smp_rwmtx_t rwlock;  /* rw lock on table */
    Uint32 type;              /* hash or tree; *read only* after creation */
#endif
    Eterm owner;              /* Pid of the creator */
    Eterm the_name;           /* an atom   */
    Eterm id;                 /* atom | integer */
    DbTableMethod* meth;      /* table methods */
    Uint nitems;               /* Total number of items */
    erts_smp_atomic_t memory_size;/* Total memory size. NOTE: in bytes! */
    Uint megasec,sec,microsec; /* Last fixation time */
    DbFixation *fixations;   /* List of processes who have fixed 
				 the table */

    /* All 32-bit fields */
    Uint32 status;            /* bit masks defined  below */
    int slot;                 /* slot index in meta_main_tab */
    int keypos;               /* defaults to 1 */
    int kept_items;           /* Number of kept elements due to fixation */
} DbTableCommon;

/* These are status bit patterns */
#define DB_NORMAL        (1 << 0)
#define DB_PRIVATE       (1 << 1)
#define DB_PROTECTED     (1 << 2)
#define DB_PUBLIC        (1 << 3)
#define DB_BAG           (1 << 4)
#define DB_SET           (1 << 5)
#define DB_LHASH         (1 << 6)  /* not really used!!! */
#define DB_FIXED         (1 << 7)
#define DB_DUPLICATE_BAG (1 << 8)
#define DB_ORDERED_SET   (1 << 9)
#define DB_DELETE        (1 << 10) /* table is being deleted */

#define ERTS_ETS_TABLE_TYPES (DB_BAG|DB_SET|DB_DUPLICATE_BAG|DB_ORDERED_SET)

#define IS_HASH_TABLE(Status) (!!((Status) & \
				  (DB_BAG | DB_SET | DB_DUPLICATE_BAG)))
#define IS_TREE_TABLE(Status) (!!((Status) & \
				  DB_ORDERED_SET))
     /*TT*/

Eterm erts_ets_copy_object(Eterm, Process*);

/* optimised version of copy_object (normal case? atomic object) */
#define COPY_OBJECT(obj, p, objp) \
   if (IS_CONST(obj)) { *(objp) = (obj); } \
   else { *objp = erts_ets_copy_object(obj, p); }

#define DB_READ  (DB_PROTECTED|DB_PUBLIC)
#define DB_WRITE DB_PUBLIC
#define DB_INFO  (DB_PROTECTED|DB_PUBLIC|DB_PRIVATE)

/* tb is an DbTableCommon and obj is an Eterm (tagged) */
#define TERM_GETKEY(tb, obj) db_getkey((tb)->common.keypos, (obj)) 

#define ONLY_WRITER(P,T) (((T)->common.status & DB_PRIVATE) || \
(((T)->common.status & DB_PROTECTED) && (T)->common.owner == (P)->id))

#define ONLY_READER(P,T) (((T)->common.status & DB_PRIVATE) && \
(T)->common.owner == (P)->id)

#define SOLE_LOCKER(P,Fixations) ((Fixations) != NULL && \
(Fixations)->next == NULL && (Fixations)->pid == (P)->id && \
(Fixations)->counter == 1)

/* Function prototypes */
Eterm db_get_trace_control_word_0(Process *p);
Eterm db_set_trace_control_word_1(Process *p, Eterm val);

void db_initialize_util(void);
Eterm db_getkey(int keypos, Eterm obj);
void db_free_term_data(DbTerm* p);
void* db_get_term(DbTableCommon *tb, DbTerm* old, Uint offset, Eterm obj);
int db_has_variable(Eterm obj);
int db_is_variable(Eterm obj);
void db_do_update_element(DbUpdateHandle* handle,
			  Sint position,
			  Eterm newval);
void db_finalize_update_element(DbUpdateHandle* handle);
Eterm db_add_counter(Eterm** hpp, Eterm counter, Eterm incr);
Eterm db_match_set_lint(Process *p, Eterm matchexpr, Uint flags);
Binary *db_match_set_compile(Process *p, Eterm matchexpr, 
			     Uint flags);
void erts_db_match_prog_destructor(Binary *);

typedef struct match_prog {
    ErlHeapFragment *term_save; /* Only if needed, a list of message 
				    buffers for off heap copies 
				    (i.e. binaries)*/
    int single_variable;     /* ets:match needs to know this. */
    int num_bindings;        /* Size of heap */
    /* The following two are only filled in when match specs 
       are used for tracing */
    struct erl_heap_fragment *saved_program_buf;
    Eterm saved_program;
#ifdef DMC_DEBUG
    int label_size;
#endif
    Uint heap_size;          /* size of: heap + eheap + stack */
    Uint eheap_offset;
    Uint stack_offset;
    Uint *labels;            /* Label offset's */
    Uint text[1];            /* Beginning of program */
} MatchProg;

/*
 * The heap-eheap-stack block of a MatchProg is nowadays allocated
 * when the match program is run.
 * - heap: variable bindings
 * - eheap: erlang heap storage
 * - eheap: a "large enough" stack
 */

#define DMC_ERR_STR_LEN 100

typedef enum { dmcWarning, dmcError} DMCErrorSeverity;

typedef struct dmc_error {
    char error_string[DMC_ERR_STR_LEN + 1]; /* printf format string
					       with %d for the variable
					       number (if applicable) */
    int variable;                           /* -1 if no variable is referenced
					       in error string */
    struct dmc_error *next;
    DMCErrorSeverity severity;              /* Error or warning */
} DMCError;

typedef struct dmc_err_info {
    unsigned int *var_trans; /* Translations of variable names, 
				initiated to NULL
				and free'd with sys_free if != NULL 
				after compilation */
    int num_trans;
    int error_added;         /* indicates if the error list contains
				any fatal errors (dmcError severity) */
    DMCError *first;         /* List of errors */
} DMCErrInfo;

/*
** Compilation flags
**
** The dialect is in the 3 least significant bits and are to be interspaced by
** by at least 2 (decimal), thats why ((Uint) 2) isn't used. This is to be 
** able to add DBIF_GUARD or DBIF BODY to it to use in the match_spec bif
** table. The rest of the word is used like ordinary flags, one bit for each 
** flag. Note that DCOMP_TABLE and DCOMP_TRACE are mutually exclusive.
*/
#define DCOMP_TABLE ((Uint) 1) /* Ets and dets. The body returns a value, 
		       * and the parameter to the execution is a tuple. */
#define DCOMP_TRACE ((Uint) 4) /* Trace. More functions are allowed, and the 
		       * parameter to the execution will be an array. */
#define DCOMP_DIALECT_MASK ((Uint) 0x7) /* To mask out the bits marking 
					   dialect */
#define DCOMP_FAKE_DESTRUCTIVE ((Uint) 8) /* When this is active, no setting of
					     trace control words or seq_trace tokens will be done. */


Binary *db_match_compile(Eterm *matchexpr, Eterm *guards,
			 Eterm *body, int num_matches, 
			 Uint flags, 
			 DMCErrInfo *err_info);
/* Returns newly allocated MatchProg binary with refc == 0*/
Eterm db_prog_match(Process *p, Binary *prog, Eterm term, int arity, 
		    Uint32 *return_flags /* Zeroed on enter */);
/* returns DB_ERROR_NONE if matches, 1 if not matches and some db error on 
   error. */
DMCErrInfo *db_new_dmc_err_info(void);
/* Returns allocated error info, where errors are collected for lint. */
Eterm db_format_dmc_err_info(Process *p, DMCErrInfo *ei);
/* Formats an error info structure into a list of tuples. */
void db_free_dmc_err_info(DMCErrInfo *ei);
/* Completely free's an error info structure, including all recorded 
   errors */
Eterm db_make_mp_binary(Process *p, Binary *mp, Eterm **hpp);
/* Convert a match program to a erlang "magic" binary to be returned to userspace,
   increments the reference counter. */
int erts_db_is_compiled_ms(Eterm term);

/*
** Convenience when compiling into Binary structures
*/
#define IsMatchProgBinary(BP) \
  (((BP)->flags & BIN_FLAG_MAGIC) \
   && ERTS_MAGIC_BIN_DESTRUCTOR((BP)) == erts_db_match_prog_destructor)

#define Binary2MatchProg(BP) \
  (ASSERT_EXPR(IsMatchProgBinary((BP))), \
   ((MatchProg *) ERTS_MAGIC_BIN_DATA((BP))))
/*
** Debugging 
*/
#ifdef HARDDEBUG
void db_check_tables(void); /* in db.c */
#define CHECK_TABLES() db_check_tables()
#else 
#define CHECK_TABLES()
#endif

#endif /* _DB_UTIL_H */
