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
#ifndef _HASH_H
#define _HASH_H

#include <stdio.h>

#include "ei.h"			/* We need our types there */

#define ei_hash_size(tab) ((tab)->size)
#define ei_hash_count(tab) ((tab)->count)

#define ALIGN_QUAD 0x7 
#define ei_align(size) while (((unsigned)size) & ALIGN_QUAD) (size)++

int ei_isprime(int n);
int ei_dohash(const char *key);
void *ei_hash_lookup(ei_hash *tab, const char *key);
const char *ei_hash_rlookup(ei_hash *tab, const void *value);
int ei_hash_foreach(ei_hash *tab, int (*f)(const char *key, const void *value));
void *ei_hash_insert(ei_hash *tab, const char *key, const void *value);
void *ei_hash_remove(ei_hash *tab, const char *key);
ei_hash *ei_hash_newtab(int tabsize);
ei_hash *ei_hash_resize(ei_hash *oldtab, int newsize);
int ei_hash_freetab(ei_hash *tab, void (*f)(void *));
void ei_hash_stats(ei_hash *tab, FILE *out);
void ei_hash_bfree(ei_hash *tab, ei_bucket *b);

#endif /* _HASH_H */
