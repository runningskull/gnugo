/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * This is GNU Go, a Go program. Contact gnugo@gnu.org, or see       *
 * http://www.gnu.org/software/gnugo/ for more information.          *
 *                                                                   *
 * Copyright 1999, 2000, 2001, 2002, 2003, 2004, 2005 and 2006       *
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
 * Software Foundation, Inc., 51 Franklin Street, Fifth Floor,       *
 * Boston, MA 02111, USA.                                            *
\* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include "gnugo.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "liberty.h"

/* Capture as many strings of the given color as we can. Played stones
 * are left on the board and the number of played stones is returned.
 * Strings marked in the exceptions array are excluded from capturing
 * attempts. If all non-excepted strings are successfully captured,
 * *none_invincible is set to one. Set none_invincible to NULL if you
 * don't need that information.
 */
static int
capture_non_invincible_strings(int color, int exceptions[BOARDMAX],
			       int *none_invincible)
{
  int other = OTHER_COLOR(color);
  int something_captured = 1; /* To get into the first turn of the loop. */
  int string_found = 0;
  int moves_played = 0;
  int save_moves;
  int libs[MAXLIBS];
  int liberties;
  int pos;
  int k;
  
  while (something_captured) {
    /* Nothing captured so far in this turn of the loop. */
    something_captured = 0;

    /* Is there something left to try to capture? */
    string_found = 0;
    
    /* Visit all friendly strings on the board. */
    for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
      if (board[pos] != color || find_origin(pos) != pos)
	continue;

      if (exceptions && exceptions[pos])
	continue;

      string_found = 1;
      
      /* Try to capture the string at pos. */
      liberties = findlib(pos, MAXLIBS, libs);
      save_moves = moves_played;
      for (k = 0; k < liberties; k++) {
	if (trymove(libs[k], other, "unconditional_life", pos))
	  moves_played++;
      }
      
      /* Successful if already captured or a single liberty remains.
       * Otherwise we must rewind and take back the last batch of moves.
       */
      if (board[pos] == EMPTY)
	something_captured = 1;
      else if (findlib(pos, 2, libs) == 1) {
	/* Need to use tryko as a defense against the extreme case
	 * when the only opponent liberty that is not suicide is an
	 * illegal ko capture, like in this 5x5 position:
	 * +-----+
	 * |.XO.O|
	 * |XXOO.|
	 * |X.XOO|
	 * |XXOO.|
	 * |.XO.O|
	 * +-----+
	 */
	int success = tryko(libs[0], other, "unconditional_life");
	gg_assert(success);
	moves_played++;
	something_captured = 1;
      }
      else
	while (moves_played > save_moves) {
	  popgo();
	  moves_played--;
	}
    }
  }

  if (none_invincible)
    *none_invincible = !string_found;
  
  return moves_played;
}

/* Find those worms of the given color that can never be captured,
 * even if the opponent is allowed an arbitrary number of consecutive
 * moves. The coordinates of the origins of these worms are written to
 * the worm arrays and the number of non-capturable worms is
 * returned.
 *
 * The algorithm is to cycle through the worms until none remains or
 * no more can be captured. A worm is removed when it is found to be
 * capturable, by letting the opponent try to play on all its
 * liberties. If the attack fails, the moves are undone. When no more
 * worm can be removed in this way, the remaining ones are
 * unconditionally alive.
 *
 * After this, unconditionally dead opponent worms and unconditional
 * territory are identified. This is almost, but only almost,
 * straightforward. We first present a simple but only almost correct
 * solution, then show how to patch up its deficiencies.
 *
 * - - - - - - -
 *
 * Algorithm 1, simple but slightly incorrect.
 *
 * To find unconditionally dead opponent worms and unconditional
 * territory, we continue from the position obtained at the end of the
 * previous operation (only unconditionally alive strings remain for
 * color) with the following steps:
 *
 * 1. Play opponent stones on all liberties of the unconditionally
 *    alive strings except where illegal. (That the move order may
 *    determine exactly which liberties can be played legally is not
 *    important. Just pick an arbitrary order).
 * 2. Recursively extend opponent strings in atari, except where this
 *    would be suicide.
 * 3. Play an opponent stone anywhere it can get two empty
 *    neighbors. (I.e. split big eyes into small ones).
 * 4. Play an opponent stone anywhere it can get one empty
 *    neighbor. (I.e. reduce two space eyes to one space eyes.)
 *
 * Remaining opponent strings in atari and remaining liberties of the
 * unconditionally alive strings constitute the unconditional
 * territory.
 *
 * Opponent strings from the initial position placed on
 * unconditional territory are unconditionally dead.
 *
 * - - - - - - -
 *
 * The deficiency with this algorithm is that a certain class of sekis
 * are considered as dead, e.g. this position:
 *
 * .OOOOO.
 * OOXXXOO
 * OXX.XXO
 * OX.O.XO
 * OX.O.XO
 * OXX.XXO
 * OOXXXOO
 * .OOOOO.
 *
 * The problem is that while removing the two O stones, X is reduced
 * to a single small eye. Still O cannot capture these stones under
 * alternating play since the eyespace is too big.
 *
 * Before discussing this seki further we make a preliminary
 * modification of the algorithm.
 *
 * - - - - - - -
 *
 * Algorithm 2. More complex but still slightly incorrect algorithm:
 *
 * 1. Run algorithm 1.
 * 2. Return to the original position.
 * 3. Capture all capturable O strings which according to algorithm 1
 *    do not belong to unconditional territory.
 * 4. Play opponent stones on all liberties of the unconditionally
 *    alive strings except where illegal. (That the move order may
 *    determine exactly which liberties can be played legally is not
 *    important. Just pick an arbitrary order).
 * 5. Recursively extend opponent strings in atari, except where this
 *    would be suicide.
 * 6. Capture all remaining capturable O strings.
 * 7. Repeat 4 and 5 once.
 * 8. Play an opponent stone anywhere it can get two empty
 *    neighbors. (I.e. split big eyes into small ones).
 * 9. Play an opponent stone anywhere it can get one empty
 *    neighbor. (I.e. reduce two space eyes to one space eyes.)
 *
 * Remaining opponent strings in atari and remaining liberties of the
 * unconditionally alive strings constitute the unconditional
 * territory.
 *
 * Opponent strings from the initial position placed on
 * unconditional territory are unconditionally dead.
 *
 * - - - - - - -
 *
 * We can observe that, after step 5, an X group with at least two
 * distinct eyespaces would not risk being reduced to a single small
 * eye. Similarly an X group with a capturable O string of size at
 * least three would allow the formation of two distinct small eyes
 * after being captured. Thus it is easy to see that the only X groups
 * which would live in seki but could not be transformed into
 * unconditionally alive groups would have a single eyespace with a
 * capturable O string of size at most 2. Furthermore the eyespace
 * would not be possible to subdivide. Then if the capturable string
 * would be of size 1 it would in all cases form a nakade and we would
 * not have a seki. The plausible seki positions would all be
 * reducable to the following eyeshape:
 *
 * .OOOOO.
 * OOXXXO.
 * OXX.XOO
 * OX.OXXO
 * OXXO.XO
 * OOX.XXO
 * .OXXXOO
 * .OOOOO.
 *
 * The remaining question is what effects cutting points in the X
 * group would have. For example these X groups are dead:
 *
 * .OOOOO.   .OOOOO.   .OOOOO.   .OOOOO.   ..OOOO.   ..OOOO.
 * .OXXXO.   .OXXXO.   .OXXXO.	 .OXXXO.   OOOXXO.   OOOXXO.
 * OOX.XO.   OOX.XOO   OOX.XOO	 OOX.XOO   OXX.XO.   OXX.XOO
 * OX.OXOO   OX.OXXO   OX.OXXO	 OX.OXXO   OX.OXOO   OX.OXXO
 * OXXO.XO   OXXO.XO   OXXO.XO	 OXXO.XO   OXXO.XO   OXXO.XO
 * OOX.XXO   OOX.XOO   OOX.XXO	 OOX.XXO   OOX.XXO   OOX.XXO
 * .OXXXOO   .OXXXO.   .OXXOOO	 .OOXXOO   .OXXXOO   .OXXOOO
 * .OOOOO.   .OOOOO.   .OOOO..	 ..OOOO.   .OOOOO.   .OOOO..
 *
 * while these are alive in seki
 *
 * ..OOOO.   .OOOO..   .OOOO..   ..OOOO.   ..OOOO.
 * OOOXXO.   .OXXOO.   OOXXOO.	 .OOXXO.   OOOXXO.
 * OXX.XOO   OOX.XOO   OXX.XOO	 OOX.XOO   OXX.XOO
 * OX.OXXO   OX.OXXO   OX.OXXO	 OX.OXXO   OX.OXXO
 * OXXO.XO   OXXO.XO   OOXO.XO	 OXXO.XO   OOXO.XO
 * OOX.XXO   OOX.XXO   .OX.XXO	 OOX.XXO   .OX.XXO
 * .OXXXOO   .OXXXOO   .OXXOOO	 .OXXXOO   .OXXXOO
 * .OOOOO.   .OOOOO.   .OOOO..	 ..OOOO.   .OOOOO.
 *
 * The critical distinction between the dead ones and the seki ones is
 * that the stones marked a and b below,
 *
 * .OOOOO.
 * OOXXXO.
 * OXX.XOO
 * OX.ObXO
 * OXaO.XO
 * OOX.XXO
 * .OXXXOO
 * .OOOOO.
 *
 * belong to different strings for the dead groups and to the same
 * string for the seki groups.
 *
 * The trick to avoid misclassifying areas where the opponent can form
 * a seki group but not an invincible group as unconditional territory
 * is thus to detect the formation above and add a third stone to the
 * O group before the capturing in step 6 above.
 *
 * This leads to the final algorithm.
 *
 * - - - - - - -
 *
 * Algorithm 3. Final and correct algorithm:
 *
 * 1. Run algorithm 1.
 * 2. Return to the original position.
 * 3. Capture all capturable O strings which according to algorithm 1
 *    do not belong to unconditional territory.
 * 4. Play opponent stones on all liberties of the unconditionally
 *    alive strings except where illegal. (That the move order may
 *    determine exactly which liberties can be played legally is not
 *    important. Just pick an arbitrary order).
 * 5. Recursively extend opponent strings in atari, except where this
 *    would be suicide.
 * 6. Identify eyespaces of the kind described above and extend any
 *    matching two-stone string with a third stone.
 * 7. Capture all remaining capturable O strings.
 * 8. Repeat 4 and 5 once.
 * 9. Play an opponent stone anywhere it can get two empty
 *    neighbors. (I.e. split big eyes into small ones).
 * 10. Play an opponent stone anywhere it can get one empty
 *    neighbor. (I.e. reduce two space eyes to one space eyes.)
 *
 * Remaining opponent strings in atari and remaining liberties of the
 * unconditionally alive strings constitute the unconditional
 * territory.
 *
 * Opponent strings from the initial position placed on
 * unconditional territory are unconditionally dead.
 *
 * - - - - - - -
 *
 * On return, unconditional_territory[][] is 1 where color has
 * unconditionally alive stones, 2 where it has unconditional
 * territory, and 0 otherwise.
 */

void
unconditional_life(int unconditional_territory[BOARDMAX], int color)
{
  int found_one;
  int other = OTHER_COLOR(color);
  int libs[MAXLIBS];
  int liberties;
  int pos;
  int k, r;
  int moves_played;
  int potential_sekis[BOARDMAX];
  int none_invincible;

  /* Initialize unconditional_territory array. */
  memset(unconditional_territory, 0,
	 sizeof(unconditional_territory[0]) * BOARDMAX);
  
  /* Find isolated two-stone strings which might be involved in the
   * kind of seki described in the comments.
   */
  memset(potential_sekis, 0, sizeof(potential_sekis));
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    int isolated = 1;
    int stones[2];
    int pos2;
    
    if (board[pos] != color
	|| find_origin(pos) != pos
	|| countstones(pos) != 2)
      continue;
    
    findstones(pos, 2, stones);
    for (k = 0; k < 2 && isolated; k++) {
      for (r = 0; r < 8 && isolated; r++) {
	pos2 = stones[k] + delta[r];
	if (!ON_BOARD(pos2)
	    || (board[pos2] == color
		&& !same_string(pos, pos2)))
	  isolated = 0;
      }
    }

    if (isolated) {
      potential_sekis[stones[0]] = 1;
      potential_sekis[stones[1]] = 1;
    }
  }
  
  moves_played = capture_non_invincible_strings(color, potential_sekis,
						&none_invincible);

  /* If there are no invincible strings, nothing can be unconditionally
   * settled.
   */
  if (none_invincible) {
    /* Take back all moves. */
    while (moves_played > 0) {
      popgo();
      moves_played--;
    }
    return;
  }
  
  /* The strings still remaining except those marked in
   * potential_sekis[] are uncapturable. Now see which opponent
   * strings can survive.
   *
   * 1. Play opponent stones on all liberties of the unconditionally
   *    alive strings except where illegal.
   */
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (board[pos] != color || potential_sekis[pos] || find_origin(pos) != pos)
      continue;
      
    /* Play as many liberties as we can. */
    liberties = findlib(pos, MAXLIBS, libs);
    for (k = 0; k < liberties; k++) {
      if (trymove(libs[k], other, "unconditional_life", pos))
	moves_played++;
    }
  }

  /* 2. Recursively extend opponent strings in atari, except where this
   *    would be suicide.
   */
  found_one = 1;
  while (found_one) {
    /* Nothing found so far in this turn of the loop. */
    found_one = 0;

    for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
      if (board[pos] != other || countlib(pos) > 1)
	continue;
	
      /* Try to extend the string at (m, n). */
      findlib(pos, 1, libs);
      if (trymove(libs[0], other, "unconditional_life", pos)) {
	moves_played++;
	found_one = 1;
      }
    }
  }

  /* Now see whether there are any significant sekis on the board. */
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (!potential_sekis[pos]
	|| board[pos] == EMPTY
	|| find_origin(pos) != pos)
      continue;
    for (r = 0; r < 4; r++) {
      int up = delta[r];
      int right = delta[(r + 1) % 4];
      int locally_played_moves = 0;
      if (board[pos + up] != color
	  || board[pos + up + up] != EMPTY
	  || board[pos - up] != EMPTY)
	continue;
      for (k = 0; k < 2; k++) {
	if (k == 1)
	  right = -right;
	if (board[pos + right] != EMPTY || board[pos + up - right] != EMPTY)
	  continue;
	if (board[pos - right] == EMPTY
	    && trymove(pos - right, other, "unconditional_life", pos))
	  locally_played_moves++;
	if (board[pos + up + right] == EMPTY
	    && trymove(pos + up + right, other, "unconditional_life", pos))
	  locally_played_moves++;
	if (board[pos - right] == other && board[pos + up + right] == other
	    && same_string(pos - right, pos + up + right)) {
	  /* This is a critical seki. Extend the string with one stone
           * in an arbitrary direction to break the seki.
	   */
	  while (locally_played_moves > 0) {
	    popgo();
	    locally_played_moves--;
	  }
	  trymove(pos - up, color, "unconditional_life", pos);
	  moves_played++;
	  break;
	}
	else {
	  while (locally_played_moves > 0) {
	    popgo();
	    locally_played_moves--;
	  }
	}
      }
      if (countstones(pos) > 2)
	break;
    }
  }

  /* Capture the strings involved in potential sekis. */
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (!potential_sekis[pos] || board[pos] == EMPTY)
      continue;
    /* Play as many liberties as we can. */
    liberties = findlib(pos, MAXLIBS, libs);
    for (k = 0; k < liberties; k++) {
      if (trymove(libs[k], other, "unconditional_life", pos))
	moves_played++;
    }
  }
    

  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    int apos;
    int bpos;
    int aopen, bopen;
    int alib, blib;
    if (board[pos] != other || countlib(pos) != 2)
      continue;
    findlib(pos, 2, libs);
    apos = libs[0];
    bpos = libs[1];
    if (abs(I(apos) - I(bpos)) + abs(J(apos) - J(bpos)) != 1)
      continue;
    
    /* Only two liberties and these are adjacent. Play one. We want
     * to maximize the number of open liberties. In this particular
     * situation we can count this with approxlib for the opposite
     * color. If the number of open liberties is the same, we
     * maximize the total number of obtained liberties.
     * Two relevant positions:
     *
     * |XXX. 
     * |OOXX    |XXXXXXX
     * |O.OX    |OOXOOOX
     * |..OX    |..OO.OX
     * +----    +-------
     */
    aopen = approxlib(apos, color, 4, NULL);
    bopen = approxlib(bpos, color, 4, NULL);
    alib  = approxlib(apos, other, 4, NULL);
    blib  = approxlib(bpos, other, 4, NULL);
    
    if (aopen > bopen || (aopen == bopen && alib >= blib)) {
      trymove(apos, other, "unconditional_life", pos);
      moves_played++;
    }
    else {
      trymove(bpos, other, "unconditional_life", pos);
      moves_played++;
    }
  }
  
  /* Identify unconditionally alive stones and unconditional territory. */
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (board[pos] == color && !potential_sekis[pos]) {
      unconditional_territory[pos] = 1;
      if (find_origin(pos) == pos) {
	liberties = findlib(pos, MAXLIBS, libs);
	for (k = 0; k < liberties; k++)
	  unconditional_territory[libs[k]] = 2;
      }
    }
    else if (board[pos] == other && countlib(pos) == 1) {
      unconditional_territory[pos] = 2;
      findlib(pos, 1, libs);
      unconditional_territory[libs[0]] = 2;
    }
  }
  
  /* Take back all moves. */
  while (moves_played > 0) {
    popgo();
    moves_played--;
  }
}


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
