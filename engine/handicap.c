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

#include "liberty.h"
#include "patterns.h"
#include "random.h"

/* ================================================================ */
/*       Set up fixed placement handicap stones for black side      */
/* ================================================================ */


/* Handicap stones are set up according to the following diagrams:
 *  
 * 2 stones:                    3 stones:           
 *
 *   A B C D E F G H J	  	  A B C D E F G H J  
 * 9 . . . . . . . . . 9  	9 . . . . . . . . . 9
 * 8 . . . . . . . . . 8  	8 . . . . . . . . . 8
 * 7 . . + . . . X . . 7  	7 . . X . . . X . . 7
 * 6 . . . . . . . . . 6  	6 . . . . . . . . . 6
 * 5 . . . . + . . . . 5  	5 . . . . + . . . . 5
 * 4 . . . . . . . . . 4  	4 . . . . . . . . . 4
 * 3 . . X . . . + . . 3  	3 . . X . . . + . . 3
 * 2 . . . . . . . . . 2  	2 . . . . . . . . . 2
 * 1 . . . . . . . . . 1  	1 . . . . . . . . . 1
 *   A B C D E F G H J	  	  A B C D E F G H J  
 *   
 * 4 stones:                    5 stones:           
 *						     
 *   A B C D E F G H J	          A B C D E F G H J  
 * 9 . . . . . . . . . 9 	9 . . . . . . . . . 9
 * 8 . . . . . . . . . 8 	8 . . . . . . . . . 8
 * 7 . . X . . . X . . 7 	7 . . X . . . X . . 7
 * 6 . . . . . . . . . 6 	6 . . . . . . . . . 6
 * 5 . . . . + . . . . 5 	5 . . . . X . . . . 5
 * 4 . . . . . . . . . 4 	4 . . . . . . . . . 4
 * 3 . . X . . . X . . 3 	3 . . X . . . X . . 3
 * 2 . . . . . . . . . 2 	2 . . . . . . . . . 2
 * 1 . . . . . . . . . 1 	1 . . . . . . . . . 1
 *   A B C D E F G H J	          A B C D E F G H J  
 *  
 * 6 stones:                    7 stones:           
 *						     
 *   A B C D E F G H J	          A B C D E F G H J  
 * 9 . . . . . . . . . 9 	9 . . . . . . . . . 9
 * 8 . . . . . . . . . 8 	8 . . . . . . . . . 8
 * 7 . . X . . . X . . 7 	7 . . X . . . X . . 7
 * 6 . . . . . . . . . 6 	6 . . . . . . . . . 6
 * 5 . . X . + . X . . 5 	5 . . X . X . X . . 5
 * 4 . . . . . . . . . 4 	4 . . . . . . . . . 4
 * 3 . . X . . . X . . 3 	3 . . X . . . X . . 3
 * 2 . . . . . . . . . 2 	2 . . . . . . . . . 2
 * 1 . . . . . . . . . 1 	1 . . . . . . . . . 1
 *   A B C D E F G H J	          A B C D E F G H J  
 *  
 * 8 stones:                    9 stones:           
 *						     
 *   A B C D E F G H J	          A B C D E F G H J  
 * 9 . . . . . . . . . 9   	9 . . . . . . . . . 9
 * 8 . . . . . . . . . 8   	8 . . . . . . . . . 8
 * 7 . . X . X . X . . 7   	7 . . X . X . X . . 7
 * 6 . . . . . . . . . 6   	6 . . . . . . . . . 6
 * 5 . . X . + . X . . 5   	5 . . X . X . X . . 5
 * 4 . . . . . . . . . 4   	4 . . . . . . . . . 4
 * 3 . . X . X . X . . 3   	3 . . X . X . X . . 3
 * 2 . . . . . . . . . 2   	2 . . . . . . . . . 2
 * 1 . . . . . . . . . 1   	1 . . . . . . . . . 1
 *   A B C D E F G H J	          A B C D E F G H J  
 *  
 * For odd-sized boards larger than 9x9, the same pattern is followed,
 * except that the edge stones are moved to the fourth line for 13x13
 * boards and larger.
 *
 * For even-sized boards at least 8x8, only the four first diagrams
 * are used, because there is no way to place the center stones
 * symmetrically. As for odd-sized boards, the edge stones are moved
 * to the fourth line for boards larger than 11x11.
 *
 * At most four stones are placed on 7x7 boards too (this size may or
 * may not be supported by the rest of the engine). No handicap stones
 * are ever placed on smaller boards.
 *
 * Notice that this function only deals with fixed handicap placement.
 * Larger handicaps can be added by free placement if the used
 * interface supports it.
 */


/* This table contains the (coded) positions of the stones.
 *  2 maps to 2 or 3, depending on board size
 *  0 maps to center
 * -ve numbers map to  board_size - number
 *
 * The stones are placed in this order, *except* if there are
 * 5 or 7 stones, in which case center ({0, 0}) is placed, and
 * then as for 4 or 6.
 */

static const int places[][2] = {

  {2, -2}, {-2, 2}, {2, 2}, {-2, -2}, /* first 4 are easy */
                                      /* for 5, {0,0} is explicitly placed */
  
  {0, 2}, {0, -2},                    /* for 6 these two are placed */
                                      /* for 7, {0,0} is explicitly placed */
  
  {2, 0}, {-2, 0},                    /* for 8, these two are placed */

  {0, 0},                             /* finally tengen for 9 */
};


/*
 * Sets up fixed handicap placement stones, returning the number of
 * placed handicap stones.
 */

int
place_fixed_handicap(int handicap)
{
  int x;
  int maxhand;
  int three = board_size > 11 ? 3 : 2;
  int mid = board_size/2;
  int retval = handicap;

  /* A handicap of 1 just means that B plays first, no komi.
   * Black is not told where to play the first stone so no handicap
   * is set. 
   */
  if (handicap < 2)
    return 0;
  if ((board_size % 2 == 1) && (board_size >= 9))
    maxhand = 9;
  else if (board_size >= 7)
    maxhand = 4;
  else
    maxhand = 0;

  /* It's up to the caller of this function to notice if the handicap
   * was too large for fixed placement and act upon that.
   */
  if (handicap > maxhand) {
    handicap = maxhand;
    retval = maxhand;
  }

  /* special cases: 5 and 7 */
  if (handicap == 5 || handicap == 7) {
    add_stone(POS(mid, mid), BLACK);
    handicap--;
  }

  for (x = 0; x < handicap; ++x) {
    int i = places[x][0];
    int j = places[x][1];

    /* translate the encoded values to board co-ordinates */
    if (i == 2)  i = three;	/* 2 or 3 */
    if (i == -2) i = -three;

    if (j == 2)  j = three;
    if (j == -2) j = -three;

    if (i == 0) i = mid;
    if (j == 0) j = mid;

    if (i < 0) i += board_size-1;
    if (j < 0) j += board_size-1;

    add_stone(POS(i, j), BLACK);
  }

  return retval;
}


/* ================================================================ */
/*       Set up free placement handicap stones for black side       */
/* ================================================================ */


/*
 * Sets up free handicap placement stones, returning the number of
 * placed handicap stones.
 */

static int remaining_handicap_stones = -1;
static int total_handicap_stones = -1;

static int find_free_handicap_pattern(void);
static void free_handicap_callback(int anchor, int color,
				   struct pattern *pattern,
				   int ll, void *data);

int
place_free_handicap(int handicap)
{
  gg_assert(handicap == 0 || handicap >= 2);

  if (handicap == 0)
    return 0;

  total_handicap_stones = handicap;
  remaining_handicap_stones = handicap;

  /* First place black stones in the four corners to enable the
   * pattern matching scheme.
   */
  add_stone(POS(0, 0), BLACK);
  add_stone(POS(0, board_size - 1), BLACK);
  add_stone(POS(board_size - 1, 0), BLACK);
  add_stone(POS(board_size - 1, board_size - 1), BLACK);

  /* Find and place free handicap stones by pattern matching. */
  while (remaining_handicap_stones > 0) {
    if (!find_free_handicap_pattern())
      break;
  }

  /* Remove the artificial corner stones. */
  remove_stone(POS(0, 0));
  remove_stone(POS(0, board_size - 1));
  remove_stone(POS(board_size - 1, 0));
  remove_stone(POS(board_size - 1, board_size - 1));

  /* Find and place additional free handicap stones by the aftermath
   * algorithm.
   */
  while (remaining_handicap_stones > 0) {
    int move;
    /* Call genmove_conservative() in order to prepare the engine for
     * an aftermath_genmove() call. We discard the genmove result.
     */
    genmove_conservative(NULL, NULL, BLACK);
    if (aftermath_genmove(&move, BLACK, NULL, 0) > 0) {
      add_stone(move, BLACK);
      remaining_handicap_stones--;
    }
    else
      break;
  }

  /* Set handicap to the number of actually placed stones. */
  handicap -= remaining_handicap_stones;

  /* Reset these to invalid values, so that improper used of handicap
   * helper functions can be detected.
   */
  total_handicap_stones = -1;
  remaining_handicap_stones = -1;
  
  return handicap;
}

struct handicap_match {
  int value;
  int anchor;
  struct pattern *pattern;
  int ll;
};

#define MAX_HANDICAP_MATCHES 40

static struct handicap_match handicap_matches[MAX_HANDICAP_MATCHES];
static int number_of_matches;

static int
find_free_handicap_pattern()
{
  int k;
  int highest_value = -1;
  int sum_values = 0;
  int r;
  int anchor;
  struct pattern *pattern;
  int ll;
  int move;

  number_of_matches = 0;
  matchpat(free_handicap_callback, BLACK, &handipat_db, NULL, NULL);

  if (number_of_matches == 0)
    return 0;

  /* Find the highest value among the matched patterns. */
  for (k = 0; k < number_of_matches; k++)
    if (highest_value < handicap_matches[k].value)
      highest_value = handicap_matches[k].value;

  /* Replace the values by 2^(value - highest_value + 10) and compute
   * the sum of these values. Fractional values are discarded.
   */
  for (k = 0; k < number_of_matches; k++) {
    if (handicap_matches[k].value < highest_value - 10)
      handicap_matches[k].value = 0;
    else
      handicap_matches[k].value = 1 << (handicap_matches[k].value
					- highest_value + 10);
    sum_values += handicap_matches[k].value;
  }

  /* Pick a random number between 0 and sum_values. Don't bother with
   * the fact that lower numbers will tend to be very slightly
   * overrepresented.
   */
  r = gg_rand() % sum_values;
  
  /* Find the chosen pattern. */
  for (k = 0; k < number_of_matches; k++) {
    r -= handicap_matches[k].value;
    if (r < 0)
      break;
  }

  /* Place handicap stones according to pattern k. */
  anchor = handicap_matches[k].anchor;
  pattern = handicap_matches[k].pattern;
  ll = handicap_matches[k].ll;
  
  /* Pick up the location of the move */
  move = AFFINE_TRANSFORM(pattern->move_offset, ll, anchor);
  add_stone(move, BLACK);
  remaining_handicap_stones--;

  /* Add stones at all '!' in the pattern. */
  for (k = 0; k < pattern->patlen; k++) { 
    if (pattern->patn[k].att == ATT_not) {
      int pos = AFFINE_TRANSFORM(pattern->patn[k].offset, ll, anchor);
      add_stone(pos, BLACK);
      remaining_handicap_stones--;
    }
  }

  return 1;
}

static void
free_handicap_callback(int anchor, int color, struct pattern *pattern,
		       int ll, void *data)
{
  int r = -1;
  int k;
  int number_of_stones = 1;

  /* Pick up the location of the move */
  int move = AFFINE_TRANSFORM(pattern->move_offset, ll, anchor);

  UNUSED(data);

  /* Check how many stones are placed by the pattern. This must not be
   * larger than the number of remaining handicap stones.
   */
  for (k = 0; k < pattern->patlen; k++) { 
    if (pattern->patn[k].att == ATT_not)
      number_of_stones++;
  }
  if (number_of_stones > remaining_handicap_stones)
    return;

  /* If the pattern has a constraint, call the autohelper to see
   * if the pattern must be rejected.
   */
  if (pattern->autohelper_flag & HAVE_CONSTRAINT) {
    if (!pattern->autohelper(ll, move, color, 0))
      return;
  }
  
  if (number_of_matches < MAX_HANDICAP_MATCHES) {
    r = number_of_matches;
    number_of_matches++;
  }
  else {
    int least_value = handicap_matches[0].value + 1;
    for (k = 0; k < number_of_matches; k++) {
      if (handicap_matches[k].value < least_value) {
	r = k;
	least_value = handicap_matches[k].value;
      }
    }
  }
  gg_assert(r >= 0 && r < MAX_HANDICAP_MATCHES);
  handicap_matches[r].value   = pattern->value;
  handicap_matches[r].anchor  = anchor;
  handicap_matches[r].pattern = pattern;
  handicap_matches[r].ll      = ll;
}

int
free_handicap_remaining_stones()
{
  gg_assert(remaining_handicap_stones >= 0);
  return remaining_handicap_stones;
}

int
free_handicap_total_stones()
{
  gg_assert(total_handicap_stones >= 0);
  return total_handicap_stones;
}

/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
