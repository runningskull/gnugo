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


#include "gnugo.h"
#include "liberty.h"


static void endgame_analyze_worm_liberties(int pos, int color);
static void endgame_find_backfilling_dame(int str, int color);
static int endgame_find_liberties(int str, int *essential_liberties,
				  int essential_libs[MAXLIBS],
				  int *inessential_liberties,
				  int inessential_libs[MAXLIBS],
				  int *false_eye_liberties,
				  int false_eye_libs[MAXLIBS]);
  

/* Generate endgame moves. These are typically moves in settled positions,
 * they aren't worth many points. Currently, we generate such moves using
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
	&& worm[pos].attack_codes[0] == 0) {
      endgame_analyze_worm_liberties(pos, color);
      endgame_find_backfilling_dame(pos, color);
    }
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
 * The two marked with `*' moves are worth one point in gote each (for
 * both colors). The first one is obvious - once black runs short on
 * liberties, he'll have to defend in his own eyespace, wasting one
 * point. In the second position, although black sacrifices one point
 * by playing in white's territory, he forces white to eventually
 * capture the black string, losing three points. However, white has
 * to play at `*' sooner or later if black doesn't take that vertex, so
 * the move is worth 3 - 1 - 1 = 1 point only, not two.
 *
 * This function is able to find such moves. The algorithm is based on
 * finding so called "inessential liberties". These are defined as
 * liberties, which satisfy five conditions:
 *
 *	1) they are not within an eye (not in someone's territory),
 *	2) all their adjacent worms and dragons are alive,
 *	3) they have adjacent worms of both colors,
 *	4) they have no other adjacent worms of the same color as the worm
 *	   under consideration,
 *	5) they are safe to fill with stones of other than the worm's color.
 *
 * Such liberties are supposed to never become territory (they can't become
 * an additional eye for the worm under consideration), the worm cannot
 * connect to something via such a liberty and they will (or at least can)
 * eventually be filled by either of the players.
 *
 * FIXME: This function can probably be improved to handle more cases.
 */
static void
endgame_analyze_worm_liberties(int pos, int color)
{
  int k;
  int worm_color = board[pos];
  int other = OTHER_COLOR(worm_color);
  int essential_liberties;
  int essential_libs[MAXLIBS];
  int inessential_liberties;
  int inessential_libs[MAXLIBS];
  int false_eye_liberties;
  int false_eye_libs[MAXLIBS];
  int num_attacks;
  int num_attacks2;
  int attacks[MAXLIBS];
  int defenses[MAXLIBS];
  int apos;
  int value;

  if (!endgame_find_liberties(pos, &essential_liberties, essential_libs,
			      &inessential_liberties, inessential_libs,
			      &false_eye_liberties, false_eye_libs))
    return;
  
  apos = NO_MOVE;
  num_attacks = 0;

  /* Now, try to predict the final state of the position. We fill all
   * inessential liberties by stones of other than the current worm's
   * color. This is just a guess, we'll have to check the results later.
   */
  for (k = 0; k < inessential_liberties; k++) {
    if (!safe_move(inessential_libs[k], other)
	|| !trymove(inessential_libs[k], other, "endgame", pos))
      break;
  }

  /* If we haven't eaten the worm accidentally, look if any attacks on the
   * worm have appeared.
   */
  if (k == inessential_liberties && board[pos] != EMPTY) {
    /* Try to look for moves as in position 1. If the worm still has
     * more than one liberty, try to play on every essential liberty
     * and see if an attack appears.
     */
    if (countlib(pos) > 1) {
      for (k = 0; k < essential_liberties; k++) {
	int lib = essential_libs[k];

	if (safe_move(lib, worm_color) && safe_move(lib, other)
	    && trymove(lib, other, "endgame", pos)) {
	  if (attack(pos, NULL) != 0) {
	    int dpos;

	    if (find_defense(pos, &dpos) && is_proper_eye_space(dpos)) {
	      int i;

	      /* If the attack cannot be defended against by playing on
	       * another essential liberty, filling a pure false eye (an
	       * eye which can't become territory) or capturing an opponent
	       * string in atari, keep it for now.
	       */
	      for (i = 0; i < essential_liberties; i++) {
		if (i != k && essential_libs[i] != dpos
		    && does_defend(essential_libs[i], pos))
		  break;
	      }

	      if (i == essential_liberties) {
		for (i = 0; i < false_eye_liberties; i++) {
		  if (does_defend(false_eye_libs[i], pos))
		    break;
		}

		if (i == false_eye_liberties) {
		  int adj[MAXCHAIN];
		  int adjs;

		  adjs = chainlinks2(pos, adj, 1);
		  for (i = 0; i < adjs; i++) {
		    int lib2;
		    findlib(adj[i], 1, &lib2);
		    if (lib2 != dpos && !is_proper_eye_space(lib2)
			&& does_defend(lib2, pos))
		      break;
		  }

		  if (i == adjs) {
		    attacks[num_attacks] = lib;
		    defenses[num_attacks] = dpos;
		    num_attacks++;
		  }
		}
	      }
	    }
	  }

	  popgo();
	}
      }
    }
    else if (essential_liberties > 0) {
      /* If the only remaining liberty is essential, it is an attack. */
      attacks[num_attacks] = essential_libs[0];
      defenses[num_attacks] = NO_MOVE;
      num_attacks++;
    }

    /* Try to find moves as in position 2. */
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
    /* We were unable to fill all the liberties. Modify
     * `inessential_liberties' in order to undo the right number of
     * moves.
     */
    inessential_liberties = k;
  }

  /* Undo all the moves made to fill inessential liberties. */
  for (k = 0; k < inessential_liberties; k++)
    popgo();
  ASSERT1(stackp == 0, pos);

  num_attacks2 = 0;
  for (k = 0; k < num_attacks; k++) {
    /* These moves must be safe for the other color, otherwise they are
     * pointless. Note that checks for safety on previous step were not
     * sufficient since we had additional stones on board then.
     */
    if (safe_move(attacks[k], other)) {
      if (defenses[k] != NO_MOVE) {
	int i;

	/* Consider this position:
	 *
	 *   .X...OO	The move at `*' satisfies the conditions above.
	 *   .X*OO.O	However, it is pointless, since black has a miai
	 *   X.OX..O	move at `a' to force white to play `b'. That is,
	 *   XXObOOO	no matter if white plays `*' or `a', black takes
	 *   .XXaOXO	the other point and white has to fill `b'. So, if
	 *   ...XXXX	there is a point, adjacent to defense point, safe
	 *		for "other" color, we discard the attack.
	 *
	 * Also, in some positions, defense point is adjacent to worm
	 * inessential liberty. In such cases we discard the attack too.
	 */
	for (i = 0; i < 4; i++) {
	  int pos2 = defenses[k] + delta[i];

	  if (board[pos2] == EMPTY) {
	    int m;

	    if (!is_proper_eye_space(pos2) && safe_move(pos2, other))
	      break;

	    for (m = 0; m < inessential_liberties; m++) {
	      if (inessential_libs[m] == pos2)
		break;
	    }

	    if (m < inessential_liberties)
	      break;
	  }
	}

	/* If this is not the case, the attack is kept for the final trial. */
	if (i == 4)
	  attacks[num_attacks2++] = attacks[k];
      }
      else {
	/* This must be the only attack (filling all inessential liberties
	 * gives an atari).
	 */
	ASSERT1(num_attacks == 1, pos);
	attacks[num_attacks2++] = attacks[k];
      }
    }
  }

  value = 0;
  if (apos != NO_MOVE) {
    /* We use the number of string's liberties minus 2 as the value of
     * the move. Minus 2 is explained in the comment before the
     * function. In some rare cases the value may differ, but this
     * should be a good guess.
     */
    value = accuratelib(apos, other, MAXLIBS, NULL) - 2;
  }

  /* If we haven't found anything interesting or have already dropped it,
   * there is no point trying more moves, so we return now.
   */
  if (value <= 0 && num_attacks2 == 0)
    return;

  /* We filled the liberties with stones of "other" color. That could lead to
   * some strange attacks, since inessential liberties are not always really
   * inessential (see trevorb:320 and trevorb:940 for examples where this step
   * is necessary). Now we fill the liberties with stones of the same color as
   * the current worm. If the results remain unchanged, then we can probably
   * trust them.
   */
  for (k = 0; k < inessential_liberties; k++) {
    if (!trymove(inessential_libs[k], worm_color, "endgame", pos))
      break;
  }

  /* GNU Go currently doesn't allow suicide, but let's assume it does. */
  if (k == inessential_liberties && board[pos] != EMPTY) {
    if (countlib(pos) > 1) {
      for (k = 0; k < num_attacks2; k++) {
	if (trymove(attacks[k], other, "endgame", pos)) {
	  if (attack(pos, NULL) != 0) {
	    TRACE("  endgame move with territorial value %d.0 found at %1m\n",
		  1, attacks[k]);
	    add_expand_territory_move(attacks[k]);
	    /* FIXME: We just guess the value here. Find a way to calculate it
	     *	      (more) precisely.
	     */
	    set_minimum_territorial_value(attacks[k], 1.0);
	  }

	  popgo();
	}
      }
    }
    else if (essential_liberties > 0  && essential_libs[0] == attacks[0]) {
      TRACE("  endgame move with territorial value %d.0 found at %1m\n",
	    1, attacks[k]);
      add_expand_territory_move(attacks[0]);
      /* FIXME: We just guess the value here. Find a way to calculate it
       *	(more) precisely.
       */
      set_minimum_territorial_value(attacks[0], 1.0);
    }

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

  /* Undo all the moves made at the third step. */
  for (k = 0; k < inessential_liberties; k++)
    popgo();
  ASSERT1(stackp == 0, pos);
}

/* A backfilling dame is a defense move, usually within potential own
 * territory, which does not have to be played immediately but after
 * outer liberties of some string have been filled. If those outer
 * liberties are dame points (here inessential liberties), it is
 * usually better to play the backfilling moves before filling the
 * dame points. If nothing else it reduces the risk for making stupid
 * blunders while filling dame.
 */
static void
endgame_find_backfilling_dame(int str, int color_to_move)
{
  int k;
  int color = board[str];
  int other = OTHER_COLOR(color);
  int essential_liberties;
  int essential_libs[MAXLIBS];
  int inessential_liberties;
  int inessential_libs[MAXLIBS];
  int false_eye_liberties;
  int false_eye_libs[MAXLIBS];
  int dpos;
  int loop_again = 1;
  int potential_moves[BOARDMAX];
  int num_potential_moves = 0;
  int move = NO_MOVE;

  while (loop_again) {
    loop_again = 0;
    if (!endgame_find_liberties(str, &essential_liberties, essential_libs,
				&inessential_liberties, inessential_libs,
				&false_eye_liberties, false_eye_libs))
      break;
    for (k = 0; k < inessential_liberties; k++) {
      if (!safe_move(inessential_libs[k], other)
	  || !trymove(inessential_libs[k], other, "endgame", str))
	continue;
      increase_depth_values();
      if (board[str] == EMPTY)
	break;
      if (attack_and_defend(str, NULL, NULL, NULL, &dpos)) {
	if (worm[dpos].color == EMPTY) {
	  potential_moves[num_potential_moves] = dpos;
	  num_potential_moves++;
	}
	forced_backfilling_moves[dpos] = 1;
	if (trymove(dpos, color, "endgame", str))
	  increase_depth_values();
	loop_again = 1;
	break;
      }
    }
  }
  
  while (stackp > 0) {
    popgo();
    decrease_depth_values();
  }

  for (k = num_potential_moves - 1; k >= 0; k--)
    if (safe_move(potential_moves[k], color)) {
      move = potential_moves[k];
      TRACE("  backfilling dame found at %1m for string %1m\n", move, str);
      if (color == color_to_move) {
	add_expand_territory_move(move);
	set_minimum_territorial_value(move, 0.1);
      }
      break;
    }
}

/* Find liberties of the string str with various characteristics. See
 * the comments above endgame_analyze_worm_liberties() for more
 * information.
 */
static int
endgame_find_liberties(int str,
		       int *essential_liberties, int essential_libs[MAXLIBS],
		       int *inessential_liberties,
		       int inessential_libs[MAXLIBS],
		       int *false_eye_liberties, int false_eye_libs[MAXLIBS])
{
  int liberties;
  int libs[MAXLIBS];
  int k;

  ASSERT1(IS_STONE(board[str]), str);

  *essential_liberties = 0;
  *inessential_liberties = 0;
  *false_eye_liberties = 0;
  
  /* Find all string liberties. */
  liberties = findlib(str, MAXLIBS, libs);

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
	int pos = lib + delta[i];

	if (!IS_STONE(board[pos]) || !IS_STONE(worm[pos].color))
	  continue;

	if (worm[pos].attack_codes[0] != 0 || dragon[pos].status != ALIVE)
	  return 0;

	if (board[pos] == board[str]) {
	  if (find_origin(pos) != find_origin(str))
	    essential = 1;
	}
	else
	  found_other = 1;
      }

      if (i < 4)
	break;

      if (found_other) {
	if (essential)
	  essential_libs[(*essential_liberties)++] = lib;
	else
	  inessential_libs[(*inessential_liberties)++] = lib;
      }
      else if (is_false_eye(half_eye, lib) && !false_eye_territory[lib])
	false_eye_libs[(*false_eye_liberties)++] = lib;
    }
  }
  return 1;
}

/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
