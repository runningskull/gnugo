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

/* How many bits should be used at least for hashing? Set this to 32 for
 * some memory save and speedup, at the cost of occasional irreproducable
 * mistakes (and possibly assertion failures). 
 * With 64 bits, there should be less than one such mistake in 10^9 games.
 * Set this to 96 if this is not safe enough for you.
 */
#define MIN_HASHBITS 		64		

#define NUM_HASHVALUES 		(MIN_HASHBITS / ( 8 * SIZEOF_LONG))



/*
 * This struct is maintained by the machinery that updates the board
 * to provide incremental hashing. Examples: trymove(), play_move(), ...
 */

typedef struct {
  Hashvalue     hashval[NUM_HASHVALUES];
} Hash_data;


void hash_init(void);

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
