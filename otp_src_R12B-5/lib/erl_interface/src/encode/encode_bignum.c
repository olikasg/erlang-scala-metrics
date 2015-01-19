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
#include "ei_x_encode.h"

int ei_encode_bignum(char *buf, int *index, mpz_t obj)
{
    char *s = buf + *index;
    char *s0 = s;
    size_t count;
    int mpz_sign = mpz_sgn(obj);

    /*
     * FIXME we could code into ERL_[SMALL_]INTEGER_EXT but to make
     * this code simple for now we always code into ERL_SMALL_BIG_EXT
     */

    if (mpz_sign == 0) {	/* Special case, bignum is zero */
	if (!buf) s += 2;
	else {
	    put8(s,ERL_SMALL_INTEGER_EXT);
	    put8(s,0);
	}
    } else {

	if (!buf) {
	    int numb = 8;	/* # bits in each external format limb */
	    s += (mpz_sizeinbase(obj, 2) + numb-1) / numb;
	} else {
	    char *arityp;

	    put8(s,ERL_LARGE_BIG_EXT);
	    arityp = s;		/* fill in later */
	    s += 4;
	    put8(s, mpz_sign == 1); /* save sign separately */
	    mpz_export(s, &count, -1, 1, 0, 0, obj);
	    s += count;
	    put32le(arityp, count);
	}
    }
  
    *index += s-s0; 

    return 0; 
}

int ei_x_encode_bignum(ei_x_buff* x, mpz_t n)
{
    int i = x->index;
    ei_encode_bignum(NULL, &i, n);
    if (!x_fix_buff(x, i))
	return -1;
    return ei_encode_bignum(x->buff, &x->index, n);
}

#endif /* HAVE_GMP_H && HAVE_LIBGMP */