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

#include <limits.h>
#include <assert.h>

#include "random.h"

/* This is an implementation of the TGFSR (twisted generalized
 * feedback shift register) random number generator TT800, which was
 * published in:
 *
 * Matsumoto, M. and Kurita, Y.: Twisted GFSR generators II.
 * ACM Transactions on Modeling and Computer Simulations,
 * Vol 4, No. 3, July 1994, pp 254--266
 *
 * The generator produces a pseudo-random sequence of 32 bit integers
 * with period 2^800 - 1 and is reported to have excellent
 * equidistribution properties, as well as being fast.
 */


/* Algorithm parameters. */
#define N 25
static const int m = 7;
static const int s = 7;
static const int t = 15;
static const unsigned int a = 0x8ebfd028U;
static const unsigned int b = 0x2b5b2500U;
static const unsigned int c = 0xdb8b0000U;


/* Global state for the random number generator. */
static unsigned int x[N];
static int k;


/* Set when properly seeded. */
static int rand_initialized = 0;

/* We use this to detect whether unsigned ints are bigger than 32
 * bits. If they are we need to clear higher order bits, otherwise we
 * can optimize by not doing the masking.
 */
#define BIG_UINT (UINT_MAX > 0xffffffffU)


/* Iterate the TGFSR once to get a new state which can be used to
 * produce another 25 random numbers.
 */

static void
iterate_tgfsr(void)
{
  int i;
  for (i=0; i<N-m; i++)
    x[i] = x[i+m]   ^  (x[i] >> 1)  ^  ((x[i] & 1) ? a : 0);
  for (; i<N; i++)
    x[i] = x[i+m-N] ^  (x[i] >> 1)  ^  ((x[i] & 1) ? a : 0);
}


/* Produce a random number from the next word of the internal state.
 */

static unsigned int
next_rand(void)
{
  int y;
  if (!rand_initialized) {
    assert(rand_initialized); /* Abort. */
    gg_srand(1);              /* Initialize silently if assertions disabled. */
  }
  if (++k == N) {
    iterate_tgfsr();
    k = 0;
  }
  y = x[k] ^ ((x[k] << s) & b);
  y ^= ((y << t) & c);
#if BIG_UINT
  y &= 0xffffffffU;
#endif
  return y;
}


/* Seed the random number generator. The first word of the internal
 * state is set by the (lower) 32 bits of seed. The remaining 24 words
 * are generated from the first one by a linear congruential pseudo
 * random generator.
 *
 * FIXME: The constants in this generator has not been checked, but
 * since they only are used to produce a very short sequence, which in
 * turn only is a seed to a stronger generator, it probably doesn't
 * matter much.
 */

void
gg_srand(unsigned int seed)
{
  int i;
  for (i = 0; i < N; i++) {
#if BIG_UINT
    seed &= 0xffffffffU;
#endif
    x[i] = seed;
    seed *= 1313;
    seed += 88897;
  }
  k = N-1; /* Force an immediate iteration of the TGFSR. */
  rand_initialized = 1;
}


/* Obtain one random integer value in the interval [0, 2^31-1].
 */

int
gg_rand(void)
{
  return (int) (next_rand() & 0x7fffffff);
}


/* Obtain one random integer value in the interval [0, 2^32-1].
 */

unsigned int
gg_urand(void)
{
  return next_rand();
}


/* Obtain one random floating point value in the half open interval
 * [0.0, 1.0).
 *
 * If the value is converted to a floating point type with less than
 * 32 bits mantissa (or if the double type should happen to be
 * unusually short), the value 1.0 may be attained.
 */

double
gg_drand(void)
{
  return next_rand() * 2.328306436538696e-10;
}


/* Retrieve the internal state of the random generator.
 */

void
gg_get_rand_state(struct gg_rand_state *state)
{
  int i;
  for (i = 0; i < N; i++)
    state->x[i] = x[i];
  state->k = k;
}


/* Set the internal state of the random number generator.
 */

void
gg_set_rand_state(struct gg_rand_state *state)
{
  int i;
  for (i = 0; i < N; i++)
    x[i] = state->x[i];
  k = state->k;
}


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
