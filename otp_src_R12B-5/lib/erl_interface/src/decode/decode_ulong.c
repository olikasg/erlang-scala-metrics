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
#include "eiext.h"
#include "putget.h"

#ifndef EI_64BIT
int ei_decode_ulong(const char *buf, int *index, unsigned long *p)
{
  const char *s = buf + *index;
  const char *s0 = s;
  unsigned long n;
  long sn;
  int arity;

  switch (get8(s)) {
  case ERL_SMALL_INTEGER_EXT:
    n = get8(s);
    break;
    
  case ERL_INTEGER_EXT:
    sn = get32be(s);
    if (sn < 0) return -1;
    n = (unsigned long)sn;
    break;
    
  case ERL_SMALL_BIG_EXT:
    arity = get8(s);
    goto decode_big;

  case ERL_LARGE_BIG_EXT:
    arity = get32be(s);

  decode_big:
    {
      int sign = get8(s);
      int i;
      n = 0;

      if (sign) return -1;

      /* Little Endian, up to four bytes always fit into unsigned long */
      for (i = 0; i < arity; i++) {
	if (i < 4) {
	  n |= get8(s) << (i * 8);
	} else if (get8(s) != 0) {
	  return -1; /* All but first byte have to be 0 */
	}
      }
    }
    break;
    
  default:
    return -1;
  }

  if (p) *p = (unsigned long)n;
  *index += s-s0;
  
  return 0; 
}
#endif /* EI_64BIT */
