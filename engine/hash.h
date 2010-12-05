/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * This is GNU Go, a Go program. Contact gnugo@gnu.org, or see       *
 * http://www.gnu.org/software/gnugo/ for more information.          *
 *                                                                   *
 * Copyright 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007,   *
 * 2008, 2009 and 2010 by the Free Software Foundation.              *
 *                                                                   *
 * This program is free software; you can redistribute it and/or     *
 * modify it under the terms of the GNU General Public License as    *
 * published by the Free Software Foundation - version 3 or          *
 * (at your option) any later version.                               *
 *                                                                   *
 * This program is distributed in the hope that it will be useful,   *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of    *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     *
 * GNU General Public License in file COPYING for more details.      *
 *                                                                   *
 * You should have received a copy of the GNU General Public         *
 * License along with this program; if not, write to the Free        *
 * Software Foundation, Inc., 51 Franklin Street, Fifth Floor,       *
 * Boston, MA 02111, USA.                                            *
\* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#ifndef _HASH_H_
#define _HASH_H_

#include "config.h"
#include <limits.h>

/*
 * This file, together with engine/hash.c implements hashing of go positions
 * using a method known as Zobrist hashing.  See the Texinfo documentation
 * (Reading/Hashing) for more information.  
 */

/* Hash values and the compact board representation should use the
 * longest integer type that the platform can handle efficiently.
 * Typically this would be a 32 bit integer on a 32 bit platform and a
 * 64 bit integer on a 64 bit platform.
 *
 * Our current assumption is that unsigned long has this
 * characteristic. Should it turn out to be false for some platform
 * we'll add conditional code to choose some other type.
 *
 * At the few places in the code where the actual size of these types
 * matter, the code should use sizeof(type) to test for this. Notice
 * that ISO C guarantees a long to be at least 32 bits.
 *
 * On (future) platforms with word length 128 bits or more, it might
 * be a waste to use more than 64 bit hashvalues, since the decreased
 * risk for hash collisions probably isn't worth the increased storage
 * cost.
 */
typedef unsigned long Hashvalue;
#define SIZEOF_HASHVALUE SIZEOF_LONG
#define HASHVALUE_PRINT_FORMAT "%0*lx"

/* for testing: Enables a lot of checks. */
#define CHECK_HASHING 0

/* Dump (almost) all read results. */
#define TRACE_READ_RESULTS 0

/* How many bits should be used for hashing? Set this to 32 for some
 * memory save and speedup, at the cost of occasional difficult to
 * explain mistakes (and possibly assertion failures).
 * With 64 bits, there should be less than one such mistake in 10^9 games.
 * Set this to 96 if this is not safe enough for you.
 *
 * Note, exactly the number of bits given by NUM_HASHBITS are used,
 * regardless of how many bits fit into struct Hash_data. Additional
 * bits are set to zero.
 */
#define NUM_HASHBITS 64

#define NUM_HASHVALUES (1 + (NUM_HASHBITS - 1) / (CHAR_BIT * SIZEOF_HASHVALUE))

/* This struct is maintained by the machinery that updates the board
 * to provide incremental hashing. Examples: trymove(), play_move(), ...
 */

typedef struct {
  Hashvalue hashval[NUM_HASHVALUES];
} Hash_data;

extern Hash_data board_hash;

Hash_data goal_to_hashvalue(const signed char *goal);

void hash_init_zobrist_array(Hash_data *array, int size);
void hash_init(void);
#define INIT_ZOBRIST_ARRAY(a) \
  hash_init_zobrist_array(a, (int) (sizeof(a) / sizeof(a[0])))

void hashdata_clear(Hash_data *hd);
void hashdata_recalc(Hash_data *hd, Intersection *board, int ko_pos);
void hashdata_invert_ko(Hash_data *hd, int pos);
void hashdata_invert_stone(Hash_data *hd, int pos, int color);
void hashdata_invert_komaster(Hash_data *hd, int komaster);
void hashdata_invert_kom_pos(Hash_data *hd, int kom_pos);
void hashdata_calc_orientation_invariant(Hash_data *hd, Intersection *board,
					 int ko_pos);

char *hashdata_to_string(Hash_data *hashdata);



/* ---------------------------------------------------------------- */

/* There is no need to involve all bits in the remainder computation
 * as long as we only use it to compute a key into a hash table. 32
 * random bits are sufficient to get an even distribution within any
 * hashtable of reasonable size. By never using more than 32 bits we
 * also reduce the platform dependency of the GNU Go engine.
 */
#if CHAR_BIT * SIZEOF_HASHVALUE <= 32
#define hashdata_remainder(hd, num) \
  ((hd).hashval[0] % (num))
#elif CHAR_BIT * SIZEOF_HASHVALUE == 64
#define hashdata_remainder(hd, num) \
  (((hd).hashval[0] >> 32) % (num))
#else
#define hashdata_remainder(hd, num) \
  (((hd).hashval[0] >> (CHAR_BIT * SIZEOF_HASHVALUE - 32)) % (num))
#endif

#if NUM_HASHVALUES == 1

#define hashdata_is_equal(hd1, hd2) \
  ((hd1).hashval[0] == (hd2).hashval[0])

#define hashdata_is_smaller(hd1, hd2) \
  ((hd1).hashval[0] < (hd2).hashval[0])

#define hashdata_xor(hd1, hd2) \
  (hd1).hashval[0] ^= (hd2).hashval[0]

#elif NUM_HASHVALUES == 2

#define hashdata_is_equal(hd1, hd2) \
  ((hd1).hashval[0] == (hd2).hashval[0] \
   && (hd1).hashval[1] == (hd2).hashval[1])

#define hashdata_is_smaller(hd1, hd2) \
  ((hd1).hashval[0] < (hd2).hashval[0] \
   || ((hd1).hashval[0] == (hd2).hashval[0] \
       && (hd1).hashval[1] < (hd2).hashval[1]))

#define hashdata_xor(hd1, hd2) \
  do { \
    (hd1).hashval[0] ^= (hd2).hashval[0]; \
    (hd1).hashval[1] ^= (hd2).hashval[1]; \
  } while (0)

#else

int hashdata_is_equal_func(Hash_data *hd1, Hash_data *hd2);
int hashdata_is_smaller_func(Hash_data *hd1, Hash_data *hd2);

#define hashdata_is_equal(hd1, hd2) \
  hashdata_is_equal_func(&(hd1), &(hd2))

#define hashdata_is_smaller(hd1, hd2) \
  hashdata_is_smaller_func(&(hd1), &(hd2))

#define hashdata_xor(hd1, hd2) \
  do { \
    int i; \
    for (i = 0; i < NUM_HASHVALUES; i++) \
      (hd1).hashval[i] ^= (hd2).hashval[i]; \
  } while (0)

#endif

#endif


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
