/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * This is GNU Go, a Go program. Contact gnugo@gnu.org, or see       *
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

#include <stdio.h>
#include "gnugo.h"

/*
 * This file, together with engine/hash.c implements hashing of go positions
 * using a method known as Zobrist hashing.  See the Texinfo documentation
 * (Reading/Hashing) for more information.  
 */


#ifndef _HASH_H_
#define _HASH_H_

/* Hash values and the compact board representation should use the
 * longest integer type that the platform can handle efficiently.
 * Typically this would be a 32 bit integer on a 32 bit platform and a
 * 64 bit integer on a 64 bit platform.
 *
 * Our preliminary assumption is that unsigned long has this
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
typedef unsigned long Compacttype;

/* for testing: Enables a lot of checks. */
#define CHECK_HASHING 0

/* Old hashing. */
#if (HASHING_SCHEME==1)		
#define NUM_HASHVALUES 		1
#define FULL_POSITION_IN_HASH 	1
#endif

/* Proposed new hashing. */
#if (HASHING_SCHEME==2)

/* How many bits should be used at least for hashing? Set this to 32 for
 * some memory save and speedup, at the cost of occasional irreproducable
 * mistakes (and possibly assertion failures). 
 * With 64 bits, there should be less than one such mistake in 10^9 games.
 * Set this to 96 if this is not safe enough for you.
 */
#define MIN_HASHBITS 		64		

#define NUM_HASHVALUES 		(MIN_HASHBITS / ( 8 * SIZEOF_LONG))
#define FULL_POSITION_IN_HASH 	0	/* Set this to 1 for debugging. */
#endif

/* Use this for self-defined values. */
#if (HASHING_SCHEME==3)
#define NUM_HASHVALUES 		?
#define FULL_POSITION_IN_HASH 	?
#endif


/*
 * We define a special compact representation of the board for the 
 * positions, used if FULL_POSITION_IN_HASH is set. In this representation
 * each location is represented by 2 bits with 0 meaning EMPTY, 1 meaning
 * WHITE and 2 meaning BLACK as defined in gnugo.h.
 * COMPACT_BOARD_SIZE is the size of such a compact representation
 * for the maximum board size allowed.
 *
 * POINTSPERCOMPACT is the number of intersections that fits into a
 * Compacttype. COMPACT_BOARD_SIZE contains the number of Compacttype
 * that is needed to contain a board of size MAX_BOARD. We would like 
 * to have this as a variable instead of a macro since the macro could
 * waste one word, but it is used in the sizeing of Hashposition.
 *
 * A go position consists of the board, possibly a ko point but NOT the
 * player to move.  This will not let us handle the super ko rule, but
 * we deem this sufficient for now.
 *
 * The ko point is defined as the point where, on the last move, one
 * stone was captured.  It is illegal for the player to move to place
 * a stone on this point.  To do so would either be suicide, which is
 * illegal anyhow, or a violation of the ko rule.  If there is no ko
 * going on, ko_pos == -1;
 */

#define POINTSPERCOMPACT    ((int) sizeof(Compacttype) * 4)
#define COMPACT_BOARD_SIZE  ((MAX_BOARD) * (MAX_BOARD) / POINTSPERCOMPACT + 1)

#if FULL_POSITION_IN_HASH
typedef struct hashposition_t {
  Compacttype  board[COMPACT_BOARD_SIZE];
  int          ko_pos;
} Hashposition;
#endif


/*
 * This struct is maintained by the machinery that updates the board
 * to provide incremental hashing. Examples: trymove(), play_move(), ...
 */

typedef struct {
  Hashvalue     hashval[NUM_HASHVALUES];
#if FULL_POSITION_IN_HASH
  Hashposition  hashpos;
#endif
} Hash_data;

Hash_data xor_hashvalues(Hash_data *key1, Hash_data *key2);
Hash_data goal_to_hashvalue(const char *goal);

void hash_init(void);
#if FULL_POSITION_IN_HASH
int hashposition_compare(Hashposition *pos1, Hashposition *pos2);
void hashposition_dump(Hashposition *pos, FILE *outfile);
#endif

void hashdata_recalc(Hash_data *hd, Intersection *board, int ko_pos);
int hashdata_compare(Hash_data *hd1, Hash_data *hd2);
void hashdata_invert_ko(Hash_data *hd, int pos);
void hashdata_invert_stone(Hash_data *hd, int pos, int color);
void hashdata_set_tomove(Hash_data *hd, int to_move);

int hashdata_diff_dump(Hash_data *key1, Hash_data *key2);


#endif


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
