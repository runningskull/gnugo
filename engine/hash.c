/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * This is GNU Go, a Go program. Contact gnugo@gnu.org, or see       *
 * http://www.gnu.org/software/gnugo/ for more information.          *
 *                                                                   *
 * Copyright 1999, 2000, 2001, 2002, 2003 and 2004                   *
 * by the Free Software Foundation.                                  *
 *                                                                   *
 * This program is free software; you can redistribute it and/or     *
 * modify it under the terms of the GNU General Public License as    *
 * published by the Free Software Foundation - version 2             *
 *                                                                   *
 * This program is distributed in the hope that it will be useful,   *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of    *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     *
 * GNU General Public License in file COPYING for more details.      *
 *                                                                   *
 * You should have received a copy of the GNU General Public         *
 * License along with this program; if not, write to the Free        *
 * Software Foundation, Inc., 59 Temple Place - Suite 330,           *
 * Boston, MA 02111, USA.                                            *
\* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */


#include "board.h"
#include "hash.h"
#include "random.h"

#include <stdio.h>
#include <stdlib.h>
#include <limits.h>



/*
 * This file, together with engine/hash.h implements hashing of go positions
 * using a method known as Zobrist hashing.  See the Texinfo documentation
 * (Reading/Hashing) for more information.  
 */


/* ================================================================ */


static int is_initialized = 0;


/* Random values for the board hash function. For stones and ko position. */
static Hash_data white_hash[BOARDMAX];
static Hash_data black_hash[BOARDMAX];
static Hash_data ko_hash[BOARDMAX];


/* Get a random Hashvalue, where all bits are used. */
static Hashvalue
hash_rand(void)
{
  int i;
  Hashvalue h = 0;

  for (i = 0; 32*i < (int) (CHAR_BIT*sizeof(Hashvalue)); i++)
    h |= (Hashvalue) gg_urand() << 32*i;

  return h;
}

/* Fill an array with random numbers for Zobrist hashing. */
void
hash_init_zobrist_array(Hash_data *array, int size)
{
  int i, j;
  for (i = 0; i < size; i++)
    for (j = 0; j < NUM_HASHVALUES; j++)
      array[i].hashval[j] = hash_rand();
}

/*
 * Initialize the board hash system.
 */

void
hash_init(void)
{
  if (is_initialized)
    return;
  
  hash_init_zobrist_array(black_hash, BOARDMAX);
  hash_init_zobrist_array(white_hash, BOARDMAX);
  hash_init_zobrist_array(ko_hash, BOARDMAX);
  
  is_initialized = 1;
}


/* ---------------------------------------------------------------- */

/* Calculate the compactboard and the hashvalues in one function.
 * They are always used together and it saves us a loop and a function 
 * call.
 */

void 
hashdata_recalc(Hash_data *target, Intersection *p, int ko_pos)
{
  int pos;
  int i;

  for (i = 0; i < NUM_HASHVALUES; i++)
    target->hashval[i] = 0;
  
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (p[pos] == WHITE)
      hashdata_xor(*target, white_hash[pos]);
    else if (p[pos] == BLACK)
      hashdata_xor(*target, black_hash[pos]);
  }

  if (ko_pos != 0)
    hashdata_xor(*target, ko_hash[ko_pos]);
}


/*
 * Set or remove ko in the hash value and hash position.
 */

void
hashdata_invert_ko(Hash_data *hd, int pos)
{
  hashdata_xor(*hd, ko_hash[pos]);
}


/*
 * Set or remove a stone of COLOR at pos in a Hash_data.
 */

void
hashdata_invert_stone(Hash_data *hd, int pos, int color)
{
  if (color == BLACK)
    hashdata_xor(*hd, black_hash[pos]);
  else if (color == WHITE)
    hashdata_xor(*hd, white_hash[pos]);
}


/* Compute hash value to identify the goal area.
 *
 * FIXME: It would be cleaner to have a separate zobrist array for the
 *        goal and xor the values in goal as usual.
 */
Hash_data
goal_to_hashvalue(const char *goal)
{
  int i, pos;
  Hash_data return_value;
  
  for (i = 0; i < NUM_HASHVALUES; i++)
    return_value.hashval[i] = 0;
  
  for (pos = BOARDMIN; pos < BOARDMAX; pos++)
    if (ON_BOARD(pos) && goal[pos])
      for (i = 0; i < NUM_HASHVALUES; i++) 
	return_value.hashval[i] += (white_hash[pos].hashval[i]
				    + black_hash[pos].hashval[i]);
  
  return return_value;
}


#define HASHVALUE_NUM_DIGITS (1 + (CHAR_BIT * SIZEOF_HASHVALUE - 1) / 4)
#define BUFFER_SIZE (1 + NUM_HASHVALUES * HASHVALUE_NUM_DIGITS)
char *
hashdata_to_string(Hash_data *hashdata)
{
  static char buffer[BUFFER_SIZE];
  int n = 0;
  int k;
  
  for (k = 0; k < NUM_HASHVALUES; k++) {
    n += sprintf(buffer + n, HASHVALUE_PRINT_FORMAT,
		 HASHVALUE_NUM_DIGITS, hashdata->hashval[k]);
    gg_assert(n < BUFFER_SIZE);
  }

  return buffer;
}

#if NUM_HASHVALUES > 2
int
hashdata_is_equal_func(Hash_data *hd1, Hash_data *hd2)
{
  int i;
  for (i = 0; i < NUM_HASHVALUES; i++)
    if (hd1->hashval[i] != hd2->hashval[i])
      return 0;

  return 1;
}
#endif


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
