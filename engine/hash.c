/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * This is GNU Go, a Go program. Contact gnugo@gnu.org, or see       *
 * http://www.gnu.org/software/gnugo/ for more information.          *
 *                                                                   *
 * Copyright 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007,   *
 * 2008 and 2009 by the Free Software Foundation.                    *
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




/* Random values for the board hash function. For stones and ko position. */
static Hash_data white_hash[BOARDMAX];
static Hash_data black_hash[BOARDMAX];
static Hash_data ko_hash[BOARDMAX];
static Hash_data komaster_hash[NUM_KOMASTER_STATES];
static Hash_data kom_pos_hash[BOARDMAX];
static Hash_data goal_hash[BOARDMAX];


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
  static int is_initialized = 0;
  if (is_initialized)
    return;
  
  INIT_ZOBRIST_ARRAY(black_hash);
  INIT_ZOBRIST_ARRAY(white_hash);
  INIT_ZOBRIST_ARRAY(ko_hash);
  INIT_ZOBRIST_ARRAY(komaster_hash);
  INIT_ZOBRIST_ARRAY(kom_pos_hash);
  INIT_ZOBRIST_ARRAY(goal_hash);

  is_initialized = 1;
}


/* ---------------------------------------------------------------- */

/* Calculate the hashvalue from scratch. */
void 
hashdata_recalc(Hash_data *hd, Intersection *p, int ko_pos)
{
  int pos;

  hashdata_clear(hd);
  
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (p[pos] == WHITE)
      hashdata_xor(*hd, white_hash[pos]);
    else if (p[pos] == BLACK)
      hashdata_xor(*hd, black_hash[pos]);
  }

  if (ko_pos != 0)
    hashdata_xor(*hd, ko_hash[ko_pos]);
}

/* Clear hashdata. */
void
hashdata_clear(Hash_data *hd)
{
  int i;
  for (i = 0; i < NUM_HASHVALUES; i++)
    hd->hashval[i] = 0;
}

/* Set or remove ko in the hash value and hash position.  */
void
hashdata_invert_ko(Hash_data *hd, int pos)
{
  hashdata_xor(*hd, ko_hash[pos]);
}


/* Set or remove a stone of COLOR at pos in a Hash_data.  */
void
hashdata_invert_stone(Hash_data *hd, int pos, int color)
{
  if (color == BLACK)
    hashdata_xor(*hd, black_hash[pos]);
  else if (color == WHITE)
    hashdata_xor(*hd, white_hash[pos]);
}


/* Set or remove the komaster value in the hash data. */
void
hashdata_invert_komaster(Hash_data *hd, int komaster)
{
  hashdata_xor(*hd, komaster_hash[komaster]);
}

/* Set or remove the komaster position in the hash data. */
void
hashdata_invert_kom_pos(Hash_data *hd, int kom_pos)
{
  hashdata_xor(*hd, kom_pos_hash[kom_pos]);
}

/* Calculate a transformation invariant hashvalue. */
void 
hashdata_calc_orientation_invariant(Hash_data *hd, Intersection *p, int ko_pos)
{
  int pos;
  int rot;
  Hash_data hd_rot;

  for (rot = 0; rot < 8; rot++) {
    hashdata_clear(&hd_rot);
    for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
      if (p[pos] == WHITE)
	hashdata_xor(hd_rot, white_hash[rotate1(pos, rot)]);
      else if (p[pos] == BLACK)
	hashdata_xor(hd_rot, black_hash[rotate1(pos, rot)]);
    }
    
    if (ko_pos != NO_MOVE)
      hashdata_xor(hd_rot, ko_hash[rotate1(ko_pos, rot)]);

    if (rot == 0 || hashdata_is_smaller(hd_rot, *hd))
      *hd = hd_rot;
  }
}

/* Compute hash value to identify the goal area. */
Hash_data
goal_to_hashvalue(const signed char *goal)
{
  int pos;
  Hash_data return_value;
  
  hashdata_clear(&return_value);
  
  for (pos = BOARDMIN; pos < BOARDMAX; pos++)
    if (ON_BOARD(pos) && goal[pos])
      hashdata_xor(return_value, goal_hash[pos]);
  
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

  /* Loop backwards for consistency between 32 and 64 bit platforms. */
  for (k = NUM_HASHVALUES - 1; k >= 0; k--) {
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

int
hashdata_is_smaller_func(Hash_data *hd1, Hash_data *hd2)
{
  int i;
  for (i = 0; i < NUM_HASHVALUES; i++)
    if (hd1->hashval[i] < hd2->hashval[i])
      return 1;
    else if (hd1->hashval[i] > hd2->hashval[i])
      return 0;

  return 0;
}
#endif


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
