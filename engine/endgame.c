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


static void endgame_analyze_worm_liberties(int pos, int color);


/* Generate endgame moves. These are typically moves in settled positions,
 * which don't cost many points. Currently, we generate such moves using
 * patterns in endgames.db and this algorithmic move generator. It is only
 * called when no move of value higher than 6.0 has been found on board.
 */
void
endgame(int color)
{
  int pos;

  TRACE("\nEndgame move generator tries to look for additional moves...\n");

  /* Try to generate some moves using endgame_analyze_worm_liberties(). See
   * the description of that function to find what moves it generates.
   */
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    /* We are only interested in alive, but not invincible worms which are
     * parts of alive dragons. That is, the position must be stable.
     */
    if (IS_STONE(board[pos])
	&& worm[pos].origin == pos
	&& dragon[pos].status == ALIVE
	&& !worm[pos].invincible
	&& !worm[pos].inessential
	&& worm[pos].attack_codes[0] == 0)
      endgame_analyze_worm_liberties(pos, color);
  }
}


/* This function handles two cases of endgame moves. Consider these two
 * positions (from endgame:301,302 and endgame:801,802 respectively):
 *
 *   OOOOOOO		XXXXXO.|
 *   O.XXX.O		X.O.O*.|
 *   OOX.XXO		X.OOOX.|
 *   .O*X.OX		XXXXOX.|
 *   .OXX..X		X..XOOO|
 *   .OOOXX.		XXXXXXX|
 *
 * The two marked with `*' moves cost one point gote each (for both colors).
 * The first one is obvious - once black runs short on liberties, he'll have
 * to defend in his own eyespace, wasting one point. In the second position,
 * although black sacrifice one point by playing in white's territory, he
 * forces white to eventually capture the black string, losing three points.
 * However, white has to play at `*' sooner or later if black doesn't take
 * that point, so the move costs 3 - 1 - 1 = 1 point only, not two.
 *
 * This function is able to find such moves. Algorithm is based on finding
 * such called "inessential liberties". This are defined as liberties, which
 * satisfy four conditions:
 *
 *	1) they are not within an eye (not in someone's territory),
 *	2) all their adjacent worms and dragons are alive,
 *	3) they have adjacent worms of both colors,
 *	4) they have no other adjacent worms of the same color as the worm
 *	   under consideration,
 *	5) they are safe to fill with stones of other than the worm's color.
 *
 * Such liberties are supposed to never become territory (they can't become
 * and additional eye for the worm under consideration), the worm cannot
 * connect to something via such a liberty and they will (or at least can)
 * be eventually filled by either of the players.
 *
 * FIXME: This function can probably be improved to handle more cases.
 */
static void
endgame_analyze_worm_liberties(int pos, int color)
{
  int k;
  int worm_color = board[pos];
  int other = OTHER_COLOR(worm_color);
  int liberties;
  int libs[MAXLIBS];
  int essential_liberties = 0;
  int essential_libs[MAXLIBS];
  int inessential_liberties = 0;
  int inessential_libs[MAXLIBS];
  int num_attacks;
  int num_attacks2;
  int attacks[MAXLIBS];
  int apos;
  int value;

  /* Find all worm liberties. */
  liberties = findlib(pos, MAXLIBS, libs);

  /* Loop over the liberties and find inessential and essential ones. The
   * latter are defined as those, which are not inside an eye space, but
   * don't otherwise qualify as inessential. If we find a non-alive (dead
   * or critical) worm or dragon around, we stop looking for liberties and
   * skip the current worm (position is unstable).
   */
  for (k = 0; k < liberties; k++) {
    int lib = libs[k];

    if (!is_proper_eye_space(lib)) {
      int i;
      int essential = 0;
      int found_other = 0;

      for (i = 0; i < 4; i++) {
	int pos2 = lib + delta[i];

	if (!IS_STONE(board[pos2]))
	  continue;

	if (worm[pos2].attack_codes[0] != 0 || dragon[pos2].status != ALIVE)
	  return;

	if (board[pos2] == worm_color) {
	  if (worm[pos2].origin != pos)
	    essential = 1;
	}
	else
	  found_other = 1;
      }

      if (i < 4)
	break;

      if (found_other) {
	if (essential)
	  essential_libs[essential_liberties++] = lib;
	else
	  inessential_libs[inessential_liberties++] = lib;
      }
    }
  }

  apos = NO_MOVE;
  num_attacks = 0;

  /* Now, try to predict the final state of the position. We fill all
   * inessential liberties by stones of other than the current worm's
   * color. This is just a guess, we'll have to check the results later.
   */
  for (k = 0; k < inessential_liberties; k++) {
    if (!safe_move(inessential_libs[k], other)
	|| !trymove(inessential_libs[k], other, "endgame", pos, EMPTY, 0))
      break;
  }

  /* If we haven't eaten the worm accidentally, look if any attacks on the
   * worm have appeared.
   */
  if (k == inessential_liberties && board[pos] != EMPTY) {
    /* Try to look for moves as in position 1. If the worm still has more than
     * one liberty, try to play on every essential liberty and see if an attack
     * appears.
     */
    if (countlib(pos) > 1) {
      for (k = 0; k < essential_liberties; k++) {
	int lib = essential_libs[k];

	if (safe_move(lib, worm_color) && safe_move(lib, other)
	    && trymove(lib, other, "endgame", pos, EMPTY, 0)) {
	  if (attack(pos, NULL) != 0) {
	    int dpos;

	    if (find_defense(pos, &dpos) && is_proper_eye_space(dpos)) {
	      int i;

	      for (i = 0; i < essential_liberties; i++) {
		if (i != k && essential_libs[i] != dpos
		    && does_defend(essential_libs[i], pos))
		  break;
	      }

	      /* If the attack cannot be defended against by playing on another
	       * essential liberty, keep it for now.
	       */
	      if (i == essential_liberties)
		attacks[num_attacks++] = lib;
	    }
	  }

	  popgo();
	}
      }
    }
    else if (essential_liberties > 0) {
      /* If the only remaining liberty is essential, it is an attack. */
      attacks[num_attacks++] = essential_libs[0];
    }

    /* Try to fing moves as in position 2. */
    if (attack(pos, &apos) != 0) {
      if (is_proper_eye_space(apos)) {
	/* The attack point is in someone's eye (must be an eye which the worm
	 * bounds). This looks promising. If this attack cannot be averted by
	 * playing on an essential liberty, keep it for further analyzis.
	 */
	for (k = 0; k < essential_liberties; k++) {
	  if (does_defend(essential_libs[k], pos)) {
	    apos = NO_MOVE;
	    break;
	  }
	}

	if (apos != NO_MOVE && worm_color == color && !does_defend(apos, pos))
	  apos = NO_MOVE;
      }
      else
	apos = NO_MOVE;
    }
  }
  else {
    /* We were unable to fill all the liberties. Modify `inessential_liberties'
     * in order to undo the right number of moves.
     */
    inessential_liberties = k;
  }

  /* Undo all the moves made to fill inessential liberties. */
  for (k = 0; k < inessential_liberties; k++)
    popgo();
  ASSERT1(stackp == 0, pos);

  value = 0;
  if (apos != NO_MOVE) {
    /* We use the number of string's liberties minus 2 as the value of the
     * move. Minus 2 is explained in the comment before the function. In
     * some rare cases the value may differ, but this must be a good guess.
     */
    value = accuratelib(apos, other, MAXLIBS, NULL) - 2;
  }

  /* If we haven't found anything interesting on the previous step, there is
   * no point trying more moves, so we return now.
   */
  if (value <= 0 && num_attacks == 0)
    return;

  num_attacks2 = 0;

  /* We filled the liberties with stones of "other" color. That could leed to
   * some strange attacks, since inessential liberties are not always really
   * inessential (see trevorb:320 and trevorb:940 for examples where this step
   * is necessary). Now we fill the liberties with stones of the same color as
   * the current worm. If the results remain unchanged, then we can probably
   * trust them.
   */
  for (k = 0; k < inessential_liberties; k++) {
    if (!trymove(inessential_libs[k], worm_color, "endgame", pos, EMPTY, 0))
      break;
  }

  /* GNU Go currently doesn't allow suicide, but let's assume it does. */
  if (k == inessential_liberties && board[pos] != EMPTY) {
    /* For case 1 moves we'll need third step, so we don't add them now, only
     * remove those which don't work.
     */
    if (countlib(pos) > 1) {
      for (k = 0; k < num_attacks; k++) {
	if (trymove(attacks[k], other, "endgame", pos, EMPTY, 0)) {
	  if (attack(pos, NULL) != 0)
	    attacks[num_attacks2++] = attacks[k];

	  popgo();
	}
      }
    }
    else if (essential_liberties > 0  && essential_libs[0] == attacks[0])
      num_attacks2 = 1;

    /* Case 2 moves can be added now, since they are not supposed to be safe
     * for other color.
     */
    if (value > 0 && does_attack(apos, pos)) {
      TRACE("  endgame move with territorial value %d.0 found at %1m\n",
	    value, apos);
      add_expand_territory_move(apos);
      set_minimum_territorial_value(apos, (float) value);
    }
  }
  else {
    /* Don't undo moves we didn't play. */
    inessential_liberties = k;
  }

  /* Undo all the moves made at the second step. */
  for (k = 0; k < inessential_liberties; k++)
    popgo();
  ASSERT1(stackp == 0, pos);

  for (k = 0; k < num_attacks2; k++) {
    /* These moves must be safe for the other color, otherwise they are
     * pointless. Note that checks for safety on previous steps were not
     * sufficient since we had additional stones on board then.
     */
    if (safe_move(attacks[k], other)) {
      TRACE("  endgame move with territorial value %d.0 found at %1m\n",
	    1, attacks[k]);
      add_expand_territory_move(attacks[k]);
      /* FIXME: We just guess the value here. Find a way to calculate it
       *	(more) precisely.
       */
      set_minimum_territorial_value(attacks[k], 1.0);
    }
  }
}
