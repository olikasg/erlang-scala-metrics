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

#ifndef __EXPORT_H__
#define __EXPORT_H__

#ifndef __SYS_H__
#include "sys.h"
#endif

#ifndef __INDEX_H__
#include "index.h"
#endif

/*
** Export entry
*/
typedef struct export
{
    IndexSlot slot; /* MUST BE LOCATED AT TOP OF STRUCT!!! */
    void* address;		/* Pointer to code for function. */
    struct binary* match_prog_set; /* Match program for tracing. */

    Eterm fake_op_func_info_for_hipe[2]; /* MUST be just before code[] */
    /*
     * code[0]: Tagged atom for module.
     * code[1]: Tagged atom for function.
     * code[2]: Arity (untagged integer).
     * code[3]: This entry is 0 unless the 'address' field points to it.
     *          Beam: Threaded code instruction to load function (em_call_error_handler),
     *	        execute BIF (em_apply_bif, em_apply_apply), or call a traced
     *          function (em_call_traced_function).
     * code[4]: Beam: Function pointer to BIF function (for BIFs only).
     *		Otherwise: 0.
     */
    Eterm code[5];
} Export;


void init_export_table(void);
void export_info(int, void *);

Export* erts_find_export_entry(Eterm m, Eterm f, unsigned int a);
Export* erts_export_put(Eterm mod, Eterm func, unsigned int arity);


Export* erts_export_get_or_make_stub(Eterm, Eterm, unsigned);
void erts_export_consolidate(void);

Export *export_list(int);
int export_list_size(void);
int export_table_sz(void);
Export *export_get(Export*);

#include "beam_load.h" /* For em_* extern declarations */ 
#define ExportIsBuiltIn(EntryPtr) 			\
(((EntryPtr)->address == (EntryPtr)->code + 3) && 	\
 ((EntryPtr)->code[3] == (Uint) em_apply_bif))

#endif
