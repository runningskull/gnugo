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

#ifndef _RANDOM_H_
#define _RANDOM_H_

/* This random number generator produces 32 bit unsigned integers, no
 * more, no less. Internally in the algorithm and for storing the
 * state we need a type that is at least 32 bits wide. A longer type
 * doesn't hurt but means a waste of bits.
 *
 * ISO C guarantees that an unsigned long always is at least 32 bits.
 * It is not uncommon, however, that it is longer. An unsigned int is
 * not guaranteed to be more than 16 bits wide, but on modern
 * platforms we can be certain that this type too is 32 bits (or
 * more). Also the GNU Coding Standards explicitly state that the
 * possibility of ints shorter than 32 bits should be ignored.
 *
 * We could make a typedef here to choose exactly which type to use.
 * In order to avoid various complications in the interface to the
 * random number generator, however, we prefer to consistently use
 * unsigned int internally and we assume this type to be at least 32
 * bits wide.
 */

/* Internal state of the random number generator. */
struct gg_rand_state {
  unsigned int x[25];   /* Internal state. */
  int k;                /* Word counter. */
};

/* Seed the random number generator. If an unsigned int is larger than
 * 32 bits, only the 32 least significant bits are used for seeding.
 */
void gg_srand(unsigned int seed);

/* Obtain one random integer value in the interval [0, 2^31-1]. */
int gg_rand(void);

/* Obtain one random integer value in the interval [0, 2^32-1]. */
unsigned int gg_urand(void);

/* Obtain one random floating point value in the half open interval
 * [0.0, 1.0).
 *
 * If the value is converted to a floating point type with less than
 * 32 bits mantissa (or if the double type should happen to be
 * unusually short), the value 1.0 may be attained.
 */
double gg_drand(void);

/* Retrieve the internal state of the random generator. */
void gg_get_rand_state(struct gg_rand_state *state);

/* Set the internal state of the random number generator. */
void gg_set_rand_state(struct gg_rand_state *state);


#endif /* _RANDOM_H_ */


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
