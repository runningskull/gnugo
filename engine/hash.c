/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * This is GNU GO, a Go program. Contact gnugo@gnu.org, or see       *
 * http://www.gnu.org/software/gnugo/ for more information.          *
 *                                                                   *
 * Copyright 1999, 2000, 2001, 2002 and 2003                         *
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

#include "gnugo.h"

#include <stdio.h>
#include <stdlib.h>
#include <limits.h>

#include "liberty.h"
#include "hash.h"
#include "random.h"


/*
 * This file, together with engine/hash.h implements hashing of go positions
 * using a method known as Zobrist hashing.  See the Texinfo documentation
 * (Reading/Hashing) for more information.  
 */


static int is_initialized = 0;


/* Random values for the hash function.  For stones and ko position. */
static Hashvalue white_hash[BOARDMAX][NUM_HASHVALUES];	
static Hashvalue black_hash[BOARDMAX][NUM_HASHVALUES];	
static Hashvalue ko_hash[BOARDMAX][NUM_HASHVALUES];

#if FULL_POSITION_IN_HASH
static Compacttype white_patterns[4 * sizeof(Compacttype)];
static Compacttype black_patterns[4 * sizeof(Compacttype)];
#endif


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


/*
 * Initialize the entire hash system.
 */

void
hash_init(void)
{
  int pos;
  int i;
  struct gg_rand_state state;
#if FULL_POSITION_IN_HASH
  int x;
#endif

  if (is_initialized)
    return;
  
  /* Since the hash initialization consumes a varying number of random
   * numbers depending on the size of the Hashvalue type, we save the
   * state of the random generator now and restore it afterwards.
   */
  gg_get_rand_state(&state);
  
  for (i = 0; i < NUM_HASHVALUES; i++)
    for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
      /* Note: We initialize _all_ positions, not just those on board.
       * This way we don't have to worry about changing board sizes.
       */
      black_hash[pos][i] = hash_rand();
      white_hash[pos][i] = hash_rand();
      ko_hash[pos][i]    = hash_rand();
    }

  gg_set_rand_state(&state);
  
#if FULL_POSITION_IN_HASH
  {
    Compacttype mask;

    for (x = 0, mask = 1; mask; x++, mask <<= 2) {
      white_patterns[x] = mask;
      black_patterns[x] = mask << 1;
    }
  }
#endif

  is_initialized = 1;
}


/* ---------------------------------------------------------------- */


/* Return 0 if *pos1 == *pos2, otherwise return 1.
 * This adheres (almost) to the standard compare function semantics 
 * which are used e.g. by the comparison functions used in qsort().
 */

#if FULL_POSITION_IN_HASH
int
hashposition_compare(Hashposition *pos1, Hashposition *pos2)
{
  int i;

  /* We need only compare to board_size.  MAX_BOARD is not necessary. */
  for (i = 0; i < (int) (board_size * board_size / POINTSPERCOMPACT + 1); i++)
    if (pos1->board[i] != pos2->board[i]) {
      stats.hash_collisions++;
      return 1;
    }

  if (pos1->ko_pos != pos2->ko_pos) {
    stats.hash_collisions++;
    return 1;
  }

  return 0;
}


/*
 * Dump an ASCII representation of the contents of a Hashposition onto
 * the FILE outfile. 
 */

void
hashposition_dump(Hashposition *pos, FILE *outfile)
{
  int i;

  gfprintf(outfile, "Board:  ");
  for (i = 0; i < (int) COMPACT_BOARD_SIZE; ++i)
    gfprintf(outfile, " %lx", (unsigned long) pos->board[i]);

  if (pos->ko_pos == 0)
    gfprintf(outfile, "  No ko");
  else
    gfprintf(outfile, "  Ko position: %1m", pos->ko_pos);
}
#endif 		/* FULL_POSITION_IN_HASH */


/* ---------------------------------------------------------------- */


/* Calculate the compactboard and the hashvalues in one function.
 * They are always used together and it saves us a loop and a function 
 * call.
 */

void 
hashdata_recalc(Hash_data *target, Intersection *p, int ko_pos)
{
#if FULL_POSITION_IN_HASH
  unsigned int index;
  Compacttype bits;
#endif
  int pos;
  int i;

  for (i = 0; i < NUM_HASHVALUES; i++)
    target->hashval[i] = 0;
#if FULL_POSITION_IN_HASH
  bits = 1;
  index = 0;
  target->hashpos.board[index] = 0;
#endif
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (!ON_BOARD(pos))
      continue;
    switch (p[pos]) {
      default:
      case EMPTY: 
#if FULL_POSITION_IN_HASH
	bits <<= 2;
#endif
	break;
      case WHITE:
        for (i = 0; i < NUM_HASHVALUES; i++)
	  target->hashval[i] ^= white_hash[pos][i];
#if FULL_POSITION_IN_HASH
	target->hashpos.board[index] |= bits;
	bits <<= 2;
#endif
	break;
      case BLACK:
        for (i = 0; i < NUM_HASHVALUES; i++)
	  target->hashval[i] ^= black_hash[pos][i];
#if FULL_POSITION_IN_HASH
	bits <<= 1;
	target->hashpos.board[index] |= bits;
	bits <<= 1;
#endif
	break;
    }

#if FULL_POSITION_IN_HASH
    if (!bits) {
      /* This means the bit fell off the left side. */
      bits = 1;
      index++;
      if (index < COMPACT_BOARD_SIZE)
	target->hashpos.board[index] = 0;
    }
#endif
  }

  /* This cleans up garbage bits at the (unused) end of the array.
   * It probably should not really be necessary.
   */
#if FULL_POSITION_IN_HASH
  while (++index < COMPACT_BOARD_SIZE)
    target->hashpos.board[index] = 0;
#endif

  if (ko_pos != 0)
    for (i = 0; i < NUM_HASHVALUES; i++)
      target->hashval[i] ^= ko_hash[ko_pos][i];

#if FULL_POSITION_IN_HASH
  target->hashpos.ko_pos = ko_pos;
#endif
}


/*
 * Set or remove ko in the hash value and hash position.
 */

void
hashdata_invert_ko(Hash_data *hd, int pos)
{
  int i;
  for (i = 0; i < NUM_HASHVALUES; i++)
    hd->hashval[i] ^= ko_hash[pos][i];
#if FULL_POSITION_IN_HASH
  hd->hashpos.ko_pos = pos;
#endif
}



/*
 * Set or remove a stone of COLOR at pos in a Hash_data.
 */

void
hashdata_invert_stone(Hash_data *hd, int pos, int color)
{
#if FULL_POSITION_IN_HASH
  int i = I(pos);
  int j = J(pos);
  int index = (i * board_size + j) / POINTSPERCOMPACT;
  int subindex = (i * board_size + j) % POINTSPERCOMPACT;
#endif
  int k;

  if (color == BLACK) {
    for (k = 0; k < NUM_HASHVALUES; k++)
      hd->hashval[k] ^= black_hash[pos][k];
#if FULL_POSITION_IN_HASH
    hd->hashpos.board[index] ^= black_patterns[subindex];
#endif
  }
  else if (color == WHITE) {
    for (k = 0; k < NUM_HASHVALUES; k++)
      hd->hashval[k] ^= white_hash[pos][k];
#if FULL_POSITION_IN_HASH
    hd->hashpos.board[index] ^= white_patterns[subindex];
#endif
  }
}


/*
 * Compare two Hash_data, if different: dump an ASCII representation 
 * of the differences to stderr.
 * return is the same as for hashposition_compare()
 */

#if FULL_POSITION_IN_HASH
int
hashdata_diff_dump(Hash_data *hd1, Hash_data *hd2)
{
  int retval;
  int pos, i;
  int count1[4], count2[4];
  static const char letter[] = "abcdefghjklmnopqrstuvwxyz";
  static const char *hashcolors[] = {"Empty", "White", "Black", "Grey!"};

  retval = hashdata_compare(hd1, hd2);
  if (retval == 0)
    return retval;

  for (i = 0; i < 4; i++) {
    count1[i] = 0;
    count2[i] = 0;
  }

  fprintf(stderr, "Differences: ");
  for (i = 0; i < COMPACT_BOARD_SIZE; i++) {
    if (hd1->hashpos.board[i] != hd2->hashpos.board[i])
      fprintf(stderr, "\nSlot %d: (%lx <==> %lx)" , i,
	      (unsigned long) hd1->hashpos.board[i],
	      (unsigned long) hd2->hashpos.board[i]);
    
    for (pos = 0; pos < POINTSPERCOMPACT; pos++) {
      unsigned int u1, u2;
      int xx, yy, zz;

      u1 = (hd1->hashpos.board[i] >> (2*pos)) & 3;
      u2 = (hd2->hashpos.board[i] >> (2*pos)) & 3;
      count1[u1]++;
      count2[u2]++;
      if (u1 == u2)
	continue;
      
      zz = (i * POINTSPERCOMPACT) + pos;
      xx = zz / MAX_BOARD;
      yy = zz % MAX_BOARD;
      fprintf(stderr, "\n#%2d: [%c%d] %s<==>%s", pos, letter[xx], yy,
	      hashcolors[u1], hashcolors[u2]);
    }
  }

  if (hd1->hashpos.ko_pos == 0 && hd2->hashpos.ko_pos == 0)
    fprintf(stderr, "\nNo ko\n");
  else if (hd1->hashpos.ko_pos == hd2->hashpos.ko_pos)
    gfprintf(stderr, "\nEqual Ko position:[%1m]\n", hd1->hashpos.ko_pos);
  else
    gfprintf(stderr, "\nDifferent Ko position:[%1m] <==> [%1m]\n",
	    hd1->hashpos.ko_pos, hd2->hashpos.ko_pos);

  fprintf(stderr, "Total [%d,%d,%d,%d]",
	  count1[0], count1[1], count1[2], count1[3]);
  fprintf(stderr, " <==> [%d,%d,%d,%d]\n",
	  count2[0], count2[1], count2[2], count2[3]);

  return retval;
}
#endif


int
hashdata_compare(Hash_data *hd1, Hash_data *hd2)
{
  int rc = 0;
  int i;

  for (i = 0; i < NUM_HASHVALUES; i++)
    if (hd1->hashval[i] != hd2->hashval[i]) 
      rc = 2;
  if ( rc == 2 && i > 0)
    stats.hash_collisions++;

#if FULL_POSITION_IN_HASH
  if (rc == 0)
    rc = hashposition_compare(&hd1->hashpos, &hd2->hashpos);
#endif

  return rc;
}


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
