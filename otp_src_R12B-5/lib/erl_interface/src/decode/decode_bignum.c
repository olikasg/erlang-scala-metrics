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

#include "eidef.h"

#if defined(HAVE_GMP_H) && defined(HAVE_LIBGMP)

#include <gmp.h>

#include "eidef.h"
#include "eiext.h"
#include "putget.h"


int ei_decode_bignum(const char *buf, int *index, mpz_t obj)
{
    const char *s = buf + *index;
    const char *s0 = s;
    int arity;
    int sign;
    unsigned long n;

    switch (get8(s)) {
    case ERL_SMALL_INTEGER_EXT:
	n = get8(s);
	mpz_set_ui(obj, n);
	break;
    
    case ERL_INTEGER_EXT:
	n = get32be(s);
	mpz_set_ui(obj, n);
	break;
    
    case ERL_SMALL_BIG_EXT:
	arity = get8(s);
	goto decode_bytes;

    case ERL_LARGE_BIG_EXT:
	arity = get32be(s);
    decode_bytes:
	sign = get8(s);
	mpz_import(obj, arity, -1, 1, 0, 0, s);
	s += arity;
	if (sign) {
	    mpz_neg(obj, obj);
	}
    
	break;
    
    default:
	return -1;
    }

    *index += s-s0;
  
    return 0; 
}

#endif /* HAVE_GMP_H && HAVE_LIBGMP */
