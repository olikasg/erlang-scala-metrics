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

#ifndef __MODULE_H__
#define __MODULE_H__

#ifndef __INDEX_H__
#include "index.h"
#endif

typedef struct erl_module {
    IndexSlot slot;		/* Must be located at top of struct! */
    int module;			/* Atom index for module (not tagged). */

    Eterm* code;
    Eterm* old_code;
    int code_length;		/* Length of loaded code in bytes. */
    int old_code_length;	/* Length of old loaded code in bytes */
    unsigned catches, old_catches;
} Module; 

Module* erts_get_module(Eterm mod);
Module* erts_put_module(Eterm mod);

void init_module_table(void);
void module_info(int, void *);

Module *module_code(int);
int module_code_size(void);
int module_table_sz(void);

#endif
