/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * This is GNU GO, a Go program. Contact gnugo@gnu.org, or see   *
 * http://www.gnu.org/software/gnugo/ for more information.      *
 *                                                               *
 * Copyright 1999, 2000, 2001 by the Free Software Foundation.   *
 *                                                               *
 * This program is free software; you can redistribute it and/or *
 * modify it under the terms of the GNU General Public License   *
 * as published by the Free Software Foundation - version 2.     *
 *                                                               *
 * This program is distributed in the hope that it will be       *
 * useful, but WITHOUT ANY WARRANTY; without even the implied    *
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR       *
 * PURPOSE.  See the GNU General Public License in file COPYING  *
 * for more details.                                             *
 *                                                               *
 * You should have received a copy of the GNU General Public     *
 * License along with this program; if not, write to the Free    *
 * Software Foundation, Inc., 59 Temple Place - Suite 330,       *
 * Boston, MA 02111, USA.                                        *
\* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <limits.h>

#include "liberty.h"
#include "hash.h"
#include "cache.h"
#include "random.h"


/*
 * This file, together with engine/hash.h implements hashing of go positions
 * using a method known as Zobrist hashing.  See the Texinfo documentation
 * (Reading/Hashing) for more information.  
 */


static int is_initialized = 0;



/* Random values for the hash function.  For stones and ko position. */
static Hashvalue white_hash[MAX_BOARD][MAX_BOARD];	
static Hashvalue black_hash[MAX_BOARD][MAX_BOARD];	
static Hashvalue ko_hash[MAX_BOARD][MAX_BOARD];


static Compacttype white_patterns[4 * sizeof(Compacttype)];
static Compacttype black_patterns[4 * sizeof(Compacttype)];


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
  int m, n;
  int x;
  struct gg_rand_state state;

  if (is_initialized)
    return;
  
  /* Since the hash initialization consumes a varying number of random
   * numbers depending on the size of the Hashvalue type, we save the
   * state of the random generator now and restore it afterwards.
   */
  gg_get_rand_state(&state);
  
#if TRACE_READ_RESULTS
  /* We need consistent hash values when this option is enabled. */
  gg_srand(1);
#endif
  
  for (m = 0; m < MAX_BOARD; m++)
    for (n = 0; n < MAX_BOARD; n++) {
      black_hash[m][n] = hash_rand();
      white_hash[m][n] = hash_rand();
      ko_hash[m][n]    = hash_rand();
    }
  
  gg_set_rand_state(&state);
  
  {
    Compacttype mask;

    for (x = 0, mask = 1; mask; x++, mask <<= 2) {
      white_patterns[x] = mask;
      black_patterns[x] = mask << 1;
    }
  }

  is_initialized = 1;
}


/* ---------------------------------------------------------------- */


/* Return 0 if *pos1 == *pos2, otherwise return 1.
 * This adheres (almost) to the standard compare function semantics 
 * which are used e.g. by the comparison functions used in qsort().
 */

int
hashposition_compare(Hashposition *pos1, Hashposition *pos2)
{
  int  i;

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
  int  i;

  fprintf(outfile, "Board:  ");
  for (i = 0; i < (int) COMPACT_BOARD_SIZE; ++i)
     fprintf(outfile, " %lx", (unsigned long) pos->board[i]);

  if (pos->ko_pos == 0)
    fprintf(outfile, "  No ko");
  else
    fprintf(outfile, "  Ko position: (%d, %d)",
	    I(pos->ko_pos), J(pos->ko_pos));
}


/* ---------------------------------------------------------------- */


/* Calculate the compactboard and the hashvalue in one function.
 * They are always used together and it saves us a loop and a function 
 * call.
 */

void 
hashdata_recalc(Hash_data *target, Intersection *p, int ko_pos)
{
  /* USE_SHIFTING is a (CPU dependent?) optimization.
   * The theory behind it is that shifting is relatively cheap.
   * Referencing a precomputed array uses the bus, and might consume 
   * a cacheslot. 
   *
   * FIXME: Do some more measuring.
   *   Here is the result so far:
   *   Platform		Speedup (comparing SHIFT to TABLE)
   *   ------------------------------------------------
   *   Intel PII	2-3% of total run-time
   *   SPARC		???
   */

#define USE_SHIFTING 1
#if USE_SHIFTING
  unsigned int   index;
  int            i, j;
  Compacttype    bits;

  target->hashval = 0;
  bits = 1;
  index = 0;
  target->hashpos.board[index] = 0;
  for (i = 0; i < board_size; i++) {
    for (j = 0; j < board_size; j++) {
      switch (p[POS(i, j)]) {
      default:
      case EMPTY: 
	bits <<= 2;
	break;
      case WHITE:
	target->hashval ^= white_hash[i][j];
	target->hashpos.board[index] |= bits;
	bits <<= 2;
	break;
      case BLACK:
	target->hashval ^= black_hash[i][j];
	bits <<= 1;
	target->hashpos.board[index] |= bits;
	bits <<= 1;
	break;
      }

      if (!bits) {
	/* this means the bit fell off the left side */
	bits = 1;
	index += 1;
	if (index < COMPACT_BOARD_SIZE)
  	  target->hashpos.board[index] = 0;
      }
    }
  }

  /* This cleans up garbage bits at the (unused) end of the array.
   * It probably should not really be necessary.
   */
  for ( ;++index < COMPACT_BOARD_SIZE; )
    target->hashpos.board[index] = 0;

#else /* USE_SHIFTING */

  int            index;
  int            subindex;
  int            i, j;
  Compacttype    bits;

  target->hashval = 0;
  index = 0;
  subindex = 0;
  bits = 0;
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++) {
      switch (p[POS(i, j)]) {
      default:
      case EMPTY: 
	break;
      case WHITE:
	target->hashval ^= white_hash[i][j] ;
	bits |= white_patterns[subindex];
	break;
      case BLACK:
	target->hashval ^= black_hash[i][j] ;
	bits |= black_patterns[subindex];
	break;
      }

      if (++subindex == POINTSPERCOMPACT) {
	target->hashpos.board[index++] = bits;
	bits = 0;
	subindex = 0;
      }
    }
  
  if (subindex != 0)
    target->hashpos.board[index] = bits;
#endif /* USE_SHIFTING */

  if (ko_pos != 0)
    target->hashval ^= ko_hash[I(ko_pos)][J(ko_pos)];

  target->hashpos.ko_pos = ko_pos;
}


/*
 * Set ko in the hash value and hash position.
 */

void
hashdata_set_ko(Hash_data *hd, int pos)
{
  hd->hashval ^= ko_hash[I(pos)][J(pos)];
  hd->hashpos.ko_pos = pos;
}


/*
 * Remove any ko from the hash value and hash position.
 */

void
hashdata_remove_ko(Hash_data *hd)
{
  if (hd->hashpos.ko_pos != 0) {
    hd->hashval ^= ko_hash[I(hd->hashpos.ko_pos)][J(hd->hashpos.ko_pos)];
    hd->hashpos.ko_pos = 0;
  }
}


/*
 * Set or remove a stone of COLOR at pos in a Hash_data.
 */

void
hashdata_invert_stone(Hash_data *hd, int pos, int color)
{
  int i = I(pos);
  int j = J(pos);
  int index = (i * board_size + j) / POINTSPERCOMPACT;
  int subindex = (i * board_size + j) % POINTSPERCOMPACT;

  if (color == BLACK) {
    hd->hashval ^= black_hash[i][j];
    hd->hashpos.board[index] ^= black_patterns[subindex];
  }
  else if (color == WHITE) {
    hd->hashval ^= white_hash[i][j];
    hd->hashpos.board[index] ^= white_patterns[subindex];
  }
}


/*
 * Compare two Hash_data, if different: dump an ASCII representation 
 * of the differences to stderr.
 * return is the same as for hashposition_compare()
 */

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

      u1 = (hd1->hashpos.board[i] >> (2*pos) ) & 3;
      u2 = (hd2->hashpos.board[i] >> (2*pos) ) & 3;
      count1[u1] += 1;
      count2[u2] += 1;
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
    fprintf(stderr, "\nEqual Ko position:[%c%d]\n",
	    letter[I(hd1->hashpos.ko_pos)], J(hd1->hashpos.ko_pos));
  else
    fprintf(stderr, "\nDifferent Ko position:[%c%d] <==> [%c%d]\n",
	    letter[I(hd1->hashpos.ko_pos)], J(hd1->hashpos.ko_pos),
	    letter[I(hd2->hashpos.ko_pos)], J(hd2->hashpos.ko_pos));

  fprintf(stderr, "Total [%d,%d,%d,%d]",
	  count1[0], count1[1], count1[2], count1[3]);
  fprintf(stderr, " <==> [%d,%d,%d,%d]\n",
	  count2[0], count2[1], count2[2], count2[3]);

  return retval;
}


int
hashdata_compare(Hash_data *hd1, Hash_data *hd2)
{
  int  rc;

  rc = (hd1->hashval == hd2->hashval) ? 0 : 2;
  if (rc == 0)
    rc = hashposition_compare(&hd1->hashpos, &hd2->hashpos);

  return rc;
}


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
