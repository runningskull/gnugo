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
#include <string.h>
#include "liberty.h"

static SGFTree *aftermath_sgftree;

/* Generate a move to definitely settle the position after the game
 * has been finished. The purpose of this is to robustly determine
 * life and death status and to distinguish between life in seki and
 * life with territory.
 *
 * The strategy is basically to turn all own living stones into
 * invincible ones and remove from the board all dead opponent stones.
 * Stones which cannot be removed, nor turned invincible, are alive in
 * seki.
 *
 * If do_capture_dead_stones is 0, opponent stones are not necessarily
 * removed from the board. This happens if they become unconditionally
 * dead anyway.
 *
 * Moves are generated in the following order of priority:
 * 0. Play edge liberties in certain positions. This is not really
 *    necessary, but often it can simplify the tactical and strategical
 *    reading substantially, making subsequent moves faster to generate.
 * 1. Capture an opponent string in atari and adjacent to own
 *    invincible string. Moves leading to ko or snapback are excluded.
 * 2. Extend an invincible string to a liberty of an opponent string.
 * 3. Connect a non-invincible string to an invincible string.
 * 4. Extend an invincible string towards an opponent string or an own
 *    non-invincible string.
 * 5. Split a big eyespace of an alive own dragon without invincible
 *    strings into smaller pieces.
 * 6. Play a liberty of a dead opponent dragon.
 *
 * Steps 2--4 are interleaved to try to optimize the efficiency of the
 * moves. In step 5 too, efforts are made to play efficient moves.  By
 * efficient we here mean moves which are effectively settling the
 * position and simplify the tactical and strategical reading for
 * subsequent moves.
 *
 * Steps 1--4 are guaranteed to be completely safe. Step 0 and 5
 * should also be risk-free. Step 6 on the other hand definitely
 * isn't. Consider for example this position:
 *
 * .XXXXX.
 * XXOOOXX
 * XOO.OOX
 * XOXXXOX
 * XO.XXOX
 * -------
 *
 * In order to remove the O stones, it is necessary to play on one of
 * the inner liberties, but one of them lets O live. Thus we have to
 * check carefully for blunders at this step.
 *
 * Update: Step 0 is only safe against blunders if care is taken not
 *         to get into a shortage of liberties.
 *         Step 5 also has some risks. Consider this position:
 *
 *         |XXXXX.
 *         |OOOOXX
 *         |..O.OX
 *         |OX*OOX
 *         +------
 *
 *         Playing at * allows X to make seki.
 *
 * IMPORTANT RESTRICTION:
 * Before calling this function it is mandatory to call genmove() or
 * genmove_conservative(). For this function to be meaningful, the
 * genmove() call should return pass.
 */
int
aftermath_genmove(int *aftermath_move, int color,
		  int under_control[BOARDMAX],
		  int do_capture_dead_stones)
{
  int k;
  int other = OTHER_COLOR(color);
  int distance[BOARDMAX];
  int score[BOARDMAX];
  float owl_hotspot[BOARDMAX];
  float reading_hotspot[BOARDMAX];
  int dragons[BOARDMAX];
  int something_found;
  int closest_opponent = NO_MOVE;
  int closest_own = NO_MOVE;
  int d;
  int move = NO_MOVE;
  int pos = NO_MOVE;
  int best_score;
  int best_scoring_move;
  
  owl_hotspots(owl_hotspot);
  reading_hotspots(reading_hotspot);
  
  /* As a preparation we compute a distance map to the invincible strings. */
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (!ON_BOARD(pos))
      continue;
    else if (board[pos] == color && worm[pos].invincible)
      distance[pos] = 0;
    else if (!do_capture_dead_stones
	     && ((board[pos] == other 
		  && worm[pos].unconditional_status == DEAD)
		 || (board[pos] == color
		     && worm[pos].unconditional_status == ALIVE)))
      distance[pos] = 0;
    else
      distance[pos] = -1;
  }
  
  d = 0;
  do {
    something_found = 0;
    for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
      if (ON_BOARD(pos) && distance[pos] == -1) {
	for (k = 0; k < 4; k++) {
	  int pos2 = pos + delta[k];
	  if (!ON_BOARD(pos2))
	    continue;
	  if ((d == 0 || board[pos2] == EMPTY)
	      && distance[pos2] == d) {
	    if (d > 0 && board[pos] == other) {
	      distance[pos] = d + 1;
	      if (closest_opponent == NO_MOVE)
		closest_opponent = pos;
	    }
	    else if (d > 0 && board[pos] == color) {
	      distance[pos] = d + 1;
	      if (closest_own == NO_MOVE)
		closest_own = pos;
	    }
	    else if (board[pos] == EMPTY) {
	      distance[pos] = d + 1;
	      something_found = 1;
	    }
	    break;
	  }
	}
      }
    }
    d++;
  } while (something_found);

  if (under_control) {
    for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
      if (!ON_BOARD(pos))
	continue;
      else if (distance[pos] == -1)
	under_control[pos] = 0;
      else
	under_control[pos] = 1;
    }
  }
  
  if (debug & DEBUG_AFTERMATH) {
    int m, n;
    for (m = 0; m < board_size; m++) {
      for (n = 0; n < board_size; n++) {
	pos = POS(m, n);
	if (distance[pos] > 0)
	  fprintf(stderr, "%2d", distance[pos]);
	else if (distance[pos] == 0) {
	  if (board[pos] == WHITE)
	    gprintf(" o");
	  else if (board[pos] == BLACK)
	    gprintf(" x");
	  else
	    gprintf(" ?");
	}
	else {
	  if (board[pos] == WHITE)
	    gprintf(" O");
	  else if (board[pos] == BLACK)
	    gprintf(" X");
	  else
	    gprintf(" .");
	}
      }
      gprintf("\n");
    }
  
    gprintf("Closest opponent %1m", closest_opponent);
    if (closest_opponent != NO_MOVE)
      gprintf(", distance %d\n", distance[closest_opponent]);
    else
      gprintf("\n");

    gprintf("Closest own %1m", closest_own);
    if (closest_own != NO_MOVE)
      gprintf(", distance %d\n", distance[closest_own]);
    else
      gprintf("\n");
  }

  /* Case 0. This is a special measure to avoid a certain kind of
   * tactical reading inefficiency.
   *
   * Here we play on edge liberties in the configuration
   *
   * XO.
   * .*.
   * ---
   *
   * to stop X from "leaking" out along the edge. Sometimes this can
   * save huge amounts of tactical reading for later moves.
   */
  best_scoring_move = NO_MOVE;
  best_score = 5;
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    int libs;
    if (board[pos] != EMPTY
	|| distance[pos] == 0)
      continue;

    libs = approxlib(pos, color, 3, NULL);
    if (libs < 3)
      continue;

    if (is_self_atari(pos, other))
      continue;
    
    for (k = 0; k < 4; k++) {
      int dir = delta[k];
      int right = delta[(k+1)%4];
      if (!ON_BOARD(pos - dir)
	  && board[pos + dir] == color
	  && board[pos + dir + right] == other
	  && board[pos + dir - right] == other
	  && (libs > countlib(pos + dir)
	      || (libs > 4
		  && libs == countlib(pos + dir)))
	  && (DRAGON2(pos + dir).safety == INVINCIBLE
	      || DRAGON2(pos + dir).safety == STRONGLY_ALIVE)) {
	int this_score = 20 * (owl_hotspot[pos] + reading_hotspot[pos]);
	if (this_score > best_score) {
	  best_score = this_score;
	  best_scoring_move = pos;
	}
      }
    }
  }
  
  if (best_scoring_move != NO_MOVE
      && safe_move(best_scoring_move, color) == WIN) {
    *aftermath_move = best_scoring_move;
    TRACE_AFTERMATH("Closing edge at %1m\n", best_scoring_move);
    return 1;
  }

  /* Case 1. */
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    int lib;
    if (board[pos] == other
	&& worm[pos].unconditional_status != DEAD
	&& countlib(pos) == 1
	&& ((ON_BOARD(SOUTH(pos))    && distance[SOUTH(pos)] == 0)
	    || (ON_BOARD(WEST(pos))  && distance[WEST(pos)]  == 0)
	    || (ON_BOARD(NORTH(pos)) && distance[NORTH(pos)] == 0)
	    || (ON_BOARD(EAST(pos))  && distance[EAST(pos)]  == 0))) {
      findlib(pos, 1, &lib);
      /* Make sure we don't play into a ko or a (proper) snapback. */
      if (countstones(pos) > 1 || !is_self_atari(lib, color)) {
	*aftermath_move = lib;
	return 1;
      }
    }
  }

  /* Cases 2--4. */
  if (closest_opponent != NO_MOVE || closest_own != NO_MOVE) {
    if (closest_own == NO_MOVE)
      move = closest_opponent;
    else
      move = closest_own;

    /* if we're about to play at distance 1, try to optimize the move. */
    if (distance[move] == 2) {
      char mx[BOARDMAX];
      char mark = 0;
      memset(mx, 0, sizeof(mx));
      best_score = 0;
      best_scoring_move = move;

      for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
	int score = 0;
	int move_ok = 0;
	if (!ON_BOARD(pos) || distance[pos] != 1)
	  continue;
	mark++;
	for (k = 0; k < 4; k++) {
	  int pos2 = pos + delta[k];
	  if (!ON_BOARD(pos2))
	    continue;
	  if (distance[pos2] < 1)
	    score--;
	  else if (board[pos2] == EMPTY)
	    score++;
	  else if (mx[pos2] == mark)
	    score--;
	  else {
	    if (board[pos2] == color) {
	      move_ok = 1;
	      score += 7;
	      if (countstones(pos2) > 2)
		score++;
	      if (countstones(pos2) > 4)
		score++;
	      if (countlib(pos2) < 4)
		score++;
	      if (countlib(pos2) < 3)
		score++;
	    }
	    else {
	      int deltalib = (approxlib(pos, other, MAXLIBS, NULL)
			      - countlib(pos2));
	      move_ok = 1;
	      score++;
	      if (deltalib >= 0)
		score++;
	      if (deltalib > 0)
		score++;
	    }
	    mark_string(pos2, mx, mark);
	  }
	}
	if (is_suicide(pos, other))
	  score -= 3;
	
	if (0)
	  gprintf("Score %1m = %d\n", pos, score);
	
	if (move_ok && score > best_score) {
	  best_score = score;
	  best_scoring_move = pos;
	}
      }
      move = best_scoring_move;
    }

    while (distance[move] > 1) {
      for (k = 0; k < 4; k++) {
	int pos2 = move + delta[k];
	if (ON_BOARD(pos2)
	    && board[pos2] == EMPTY
	    && distance[pos2] == distance[move] - 1) {
	  move = pos2;
	  break;
	}
      }
    }
    *aftermath_move = move;
    return 1;
  }
  
  /* Case 5.
   * If we reach here, either all strings of a dragon are invincible
   * or no string is. Next we try to make alive dragons invincible by
   * splitting big eyes into smaller ones. Our strategy is to search
   * for an empty vertex with as many eye points as possible adjacent
   * and with at least one alive but not invincible stone adjacent or
   * diagonal.
   */
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    int eyespace_neighbors = 0;
    int own_neighbors = 0;
    int own_diagonals = 0;
    int opponent_dragons = 0;
    int own_worms = 0;
    int safety = UNKNOWN;
    int bonus = 0;
    int mx[BOARDMAX];
    score[pos] = 0;
      
    if (board[pos] != EMPTY || distance[pos] != -1)
      continue;

    memset(mx, 0, sizeof(mx));
    
    for (k = 0; k < 8; k++) {
      int pos2 = pos + delta[k];
      if (!ON_BOARD(pos2))
	continue;
      
      if (board[pos2] == EMPTY) {
	if (k < 4)
	  eyespace_neighbors++;
	continue;
      }
      
      if (board[pos2] == other) {
	int origin = dragon[pos2].origin;
	
	if (k < 4) {
	  if (dragon[pos2].status == ALIVE) {
	    safety = DEAD;
	    break;
	  }
	  else if (!mx[origin]) {
	    eyespace_neighbors++;
	    opponent_dragons++;
	  }
	}

	if (!mx[origin] && dragon[pos2].status == DEAD) {
	  bonus++;
	  if (k < 4 
	      && countlib(pos2) <= 2 
	      && countstones(pos2) >= 3)
	    bonus++;
	  
	  if (k < 4 && countlib(pos2) == 1)
	    bonus += 3;
	}
	mx[origin] = 1;
      }
      else if (board[pos2] == color) {
	dragons[pos] = pos2;
	
	if (safety == UNKNOWN && dragon[pos2].status == ALIVE)
	  safety = ALIVE;
	
	if (DRAGON2(pos2).safety == INVINCIBLE)
	  safety = INVINCIBLE;
	
	if (k < 4) {
	  int apos = worm[pos2].origin;
	  
	  if (!mx[apos]) {
	    own_worms++;
	    if (countstones(apos) == 1)
	      bonus += 2;
	    if (countlib(apos) < 6
		&& approxlib(pos, color, 5, NULL) < countlib(apos))
	      bonus -= 5;
	    mx[apos] = 1;
	  }
	  
	  if (countlib(apos) <= 2) {
	    int r;
	    int important = 0;
	    int safe_atari = 0;
	    for (r = 0; r < 4; r++) {
	      d = delta[r];
	      if (!ON_BOARD(apos+d))
		continue;
	      if (board[apos+d] == other
		  && dragon[apos+d].status == DEAD)
		important = 1;
	      else if (board[apos+d] == EMPTY
		       && !is_self_atari(apos+d, other))
		safe_atari = 1;
	    }
	    if (approxlib(pos, color, 3, NULL) > 2) {
	      bonus++;
	      if (important) {
		bonus += 2;
		if (safe_atari)
		  bonus += 2;
	      }
	    }
	  }
	  
	  own_neighbors++;
	}
	else
	  own_diagonals++;
      }
    }
    if (safety == DEAD || safety == UNKNOWN
	|| eyespace_neighbors == 0
	|| (own_neighbors + own_diagonals) == 0)
      continue;
    
    if (bonus < 0)
      bonus = 0;
      
    score[pos] = 4 * eyespace_neighbors + bonus;
    if (safety == INVINCIBLE) {
      score[pos] += own_neighbors;
      if (own_neighbors < 2)
	score[pos] += own_diagonals;
      if (own_worms > 1 && eyespace_neighbors >= 1)
	score[pos] += 10 + 5 * (own_worms - 2);
    }
    else if (eyespace_neighbors > 2)
      score[pos] += own_diagonals;
    
    /* Splitting bonus. */
    if (opponent_dragons > 1)
      score[pos] += 10 * (opponent_dragons - 1);
    
    /* Hotspot bonus. */
    {
      int owl_hotspot_bonus = (int) (20.0 * owl_hotspot[pos]);
      int reading_hotspot_bonus = (int) (20.0 * reading_hotspot[pos]);
      int hotspot_bonus = owl_hotspot_bonus + reading_hotspot_bonus;
      
      /* Don't allow the hotspot bonus to turn a positive score into
       * a non-positive one.
       */
      if (score[pos] > 0 && score[pos] + hotspot_bonus <= 0)
	hotspot_bonus = 1 - score[pos];
      
      score[pos] += hotspot_bonus;
      
      if (1 && (debug & DEBUG_AFTERMATH))
	gprintf("Score %1M = %d (hotspot bonus %d + %d)\n", pos, score[pos],
		owl_hotspot_bonus, reading_hotspot_bonus);
    }
    
    /* Avoid taking ko. */
    if (is_ko(pos, color, NULL))
      score[pos] = (score[pos] + 1) / 2;
  }
  
  while (1) {
    int bb;
    best_score = 0;
    move = NO_MOVE;
    for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
      if (ON_BOARD(pos) && score[pos] > best_score) {
	best_score = score[pos];
	move = pos;
      }
    }

    if (move == NO_MOVE)
      break;

    bb = dragons[move];
    if (is_illegal_ko_capture(move, color)
	|| !safe_move(move, color)
	|| (DRAGON2(bb).safety != INVINCIBLE
	    && DRAGON2(bb).safety != STRONGLY_ALIVE
	    && owl_does_defend(move, bb, NULL) != WIN)
	|| (!confirm_safety(move, color, NULL, NULL))) {
      score[move] = 0;
    }
    else {
      /* If we're getting short of liberties, we must be more careful.
       * Check that no adjacent string or dragon gets more alive by
       * the move.
       */
      int libs = approxlib(move, color, 5, NULL);
      int move_ok = 1;
      if (libs < 5) {
	for (k = 0; k < 4; k++) {
	  if (board[move + delta[k]] == color
	      && countlib(move + delta[k]) > libs)
	    break;
	}
	if (k < 4) {
	  if (trymove(move, color, "aftermath-B", move + delta[k],
		      EMPTY, NO_MOVE)) {
	    int adjs[MAXCHAIN];
	    int neighbors;
	    int r;
	    neighbors = chainlinks(move, adjs);
	    for (r = 0; r < neighbors; r++) {
	      if (worm[adjs[r]].attack_codes[0] != 0
		  && (find_defense(adjs[r], NULL)
		      > worm[adjs[r]].defense_codes[0])) {
		TRACE_AFTERMATH("Blunder: %1m becomes tactically safer after %1m\n",
		      adjs[r], move);
		move_ok = 0;
	      }
	    }
	    popgo();
	    for (r = 0; r < neighbors && move_ok; r++) {
	      if (dragon[adjs[r]].status == DEAD
		  && !owl_does_attack(move, adjs[r], NULL)) {
		TRACE_AFTERMATH(
		      "Blunder: %1m becomes more alive after %1m\n",
		      adjs[r], move);
		move_ok = 0;
	      }
	    }
	  }
	}
      }

      if (!move_ok)
	score[move] = 0;
      else {
	*aftermath_move = move;
	TRACE_AFTERMATH("Splitting eyespace at %1m\n", move);
	return 1;
      }
    }
  }

  /* Case 6.
   * Finally we try to play on liberties of remaining DEAD opponent
   * dragons, carefully checking against mistakes.
   */
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    int target;
    int cc = NO_MOVE;
    int self_atari_ok = 0;
    if (board[pos] != EMPTY || distance[pos] != -1)
      continue;
    target = NO_MOVE;
    for (k = 0; k < 4; k++) {
      int pos2 = pos + delta[k];
      if (!ON_BOARD(pos2))
	continue;
      if (board[pos2] == other 
	  && dragon[pos2].status != ALIVE
	  && (do_capture_dead_stones 
	      || worm[pos2].unconditional_status != DEAD)
	  && DRAGON2(pos2).safety != INESSENTIAL) {
	target = pos2;
	break;
      }
    }
    if (target == NO_MOVE)
      continue;
    
    /* At this point, (pos) is a move that potentially may capture
     * a dead opponent string at (target).
     */
    
    if (!trymove(pos, color, "aftermath-A", target, EMPTY, NO_MOVE))
      continue;
    
    /* It is frequently necessary to sacrifice own stones in order
     * to force the opponent's stones to be removed from the board,
     * e.g. by adding stones to fill up a nakade shape. However, we
     * should only play into a self atari if the sacrificed stones
     * are classified as INESSENTIAL. Thus it would be ok for O to
     * try a self atari in this position:
     *
     * |OOOO
     * |XXXO
     * |..XO
     * |OOXO
     * +----
     *
     * but not in this one:
     *
     * |XXX..
     * |OOXX.
     * |.OOXX
     * |XXOOX
     * |.O.OX
     * +-----
     */

    self_atari_ok = 1;
    for (k = 0; k < 4; k++) {
      if (board[pos + delta[k]] == color
	  && DRAGON2(pos + delta[k]).safety != INESSENTIAL) {
	self_atari_ok = 0;
	cc = pos + delta[k];
	break;
      }
    }
    
    /* Copy the potential move to (move). */
    move = pos;
    
    /* If the move is a self atari, but that isn't okay, try to
     * recursively find a backfilling move which later makes the
     * potential move possible.
     */
    if (!self_atari_ok) {
      while (countlib(pos) == 1) {
	int lib;
	findlib(pos, 1, &lib);
	move = lib;
	if (!trymove(move, color, "aftermath-B", target, EMPTY, NO_MOVE))
	  break;
      }
      
      if (countlib(pos) == 1)
	move = NO_MOVE;
    }

    while (stackp > 0)
      popgo();
    
    if (move == NO_MOVE)
      continue;
      
    /* Make sure that the potential move really isn't a self
     * atari. In the case of a move found after backfilling this
     * could happen (because the backfilling moves happened to
     * capture some stones).
     */
    if (!self_atari_ok && is_self_atari(move, color))
      continue;
    
    /* Consult the owl code to determine whether the considered move
     * really is effective. Blunders should be detected here.
     */
    if (owl_does_attack(move, target, NULL) == WIN) {
      /* If we have an adjacent own dragon, which is not inessential,
       * verify that it remains safe.
       */
      if (cc != NO_MOVE && !owl_does_defend(move, cc, NULL))
	continue;

      /* If we don't allow self atari, also call confirm safety to
       * avoid setting up combination attacks.
       */
      if (!self_atari_ok && !confirm_safety(move, color, NULL, NULL))
	continue;
	  
      *aftermath_move = move;
      TRACE_AFTERMATH("Filling opponent liberty at %1m\n", move);
      return 1;
    }
  }
  
  /* Case 7.
   * In very rare cases it turns out we need yet another pass. An
   * example is this position:
   *
   * |.....
   * |OOOO.
   * |XXXO.
   * |.OXO.
   * |O.XO.
   * +-----
   *
   * Here the X stones are found tactically dead and therefore the
   * corner O stones have been amalgamated with the surrounding
   * stones. Since the previous case only allows sacrificing
   * INESSENTIAL stones, it fails to take X off the board.
   *
   * The solution is to look for tactically attackable opponent stones
   * that still remain on the board but should be removed.
   */
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (board[pos] == other
	&& (worm[pos].unconditional_status == UNKNOWN
	    || do_capture_dead_stones)
	&& (DRAGON2(pos).safety == DEAD
	    || DRAGON2(pos).safety == TACTICALLY_DEAD)
	&& worm[pos].attack_codes[0] != 0
	&& !is_illegal_ko_capture(worm[pos].attack_points[0], color)) {
      *aftermath_move = worm[pos].attack_points[0];
      TRACE_AFTERMATH("Tactically attack %1m at %1m\n",
	    pos, *aftermath_move);
      return 1;
    }
  }
  
  /* No move found. */
  return -1;
}

/* This is a substitute for genmove_conservative() which only does
 * what is required when doing the aftermath. Notice though that this
 * generates an "ordinary" move, in contrast to aftermath_genmove().
 * Usually this should turn up a pass, but when it doesn't it's
 * important not to miss the move.
 */
static int
reduced_genmove(int *move, int color)
{
  float val;
  int save_verbose;

  /* no move is found yet. */
  *move = NO_MOVE;  
  val = -1; 
  
  /* Prepare pattern matcher and reading code. */
  reset_engine();

  /* Find out information about the worms and dragons. */
  examine_position(color, EXAMINE_ALL);

  /* Make a score estimate. This can be used in later stages of the 
   * move generation.  If we are ahead, we can play safely and if
   * we are behind, we have to play more daringly.
   */
  if (level >= 8) {
    estimate_score(&lower_bound, &upper_bound);
    if (verbose || showscore) {
      if (lower_bound == upper_bound)
	gprintf("\nScore estimate: %s %f\n",
		lower_bound > 0 ? "W " : "B ", gg_abs(lower_bound));
      else
	gprintf("\nScore estimate: %s %f to %s %f\n",
		lower_bound > 0 ? "W " : "B ", gg_abs(lower_bound),
		upper_bound > 0 ? "W " : "B ", gg_abs(upper_bound));
      fflush(stderr);
    }

    /* The score will be used to determine when we are safely
     * ahead. So we want the most conservative score.
     */
    if (color == WHITE)
      score = lower_bound;
    else
      score = upper_bound;
  }
  else
    score = 0.0;

  gg_assert(stackp == 0);
  
  /*
   * Ok, information gathering is complete. Now start to find some moves!
   */

  /* Pick up tactical moves. */
  worm_reasons(color);
  
  /* Pick up owl moves. */
  save_verbose = verbose;
  if (verbose > 0)
    verbose--;
  owl_reasons(color);
  verbose = save_verbose;
  
  /* Look for combination attacks and defenses against them. */
  combinations(color);
  gg_assert(stackp == 0);

  /* Review the move reasons and estimate move values. */
  if (review_move_reasons(move, &val, color, 0.0, lower_bound, NULL))
    TRACE("Move generation likes %1m with value %f\n", *move, val);
  gg_assert(stackp == 0);

  /* If no move is found then pass. */
  if (val < 0.0) {
    TRACE("I pass.\n");
    *move = NO_MOVE;
  }
  else
    TRACE("reduced_genmove() recommends %1m with value %f\n", *move, val);
 
 return val;
}

/* Preliminary function for playing through the aftermath. */
static void
do_play_aftermath(int color, struct aftermath_data *a)
{
  int move;
  int pass = 0;
  int moves = 0;
  int color_to_play = color;
  TRACE_AFTERMATH("The aftermath starts.\n");

  /* Disable computing worm and owl threats. */
  disable_threat_computation = 1;
  /* Disable matching of endgame patterns. */
  disable_endgame_patterns = 1;

  while (pass < 2 && moves < board_size * board_size) {
    int reading_nodes = get_reading_node_counter();
    int owl_nodes = get_owl_node_counter();
    int move_val = reduced_genmove(&move, color_to_play);
    if (move_val < 0) {
      int save_verbose = verbose;
      if (verbose > 0)
	verbose--;
      move_val = aftermath_genmove(&move, color_to_play,
				   (color_to_play == WHITE ?
				    a->white_control : a->black_control),
				   0);
      verbose = save_verbose;
    }
    play_move(move, color_to_play);
    if (aftermath_sgftree)
      sgftreeAddPlay(aftermath_sgftree, color_to_play, I(move), J(move));
    moves++;
    TRACE_AFTERMATH("%d %C move %1m (nodes %d, %d  total %d, %d)\n",
	  movenum, color_to_play, move, get_owl_node_counter() - owl_nodes,
	  get_reading_node_counter() - reading_nodes,
	  get_owl_node_counter(), get_reading_node_counter());
    if (move != PASS_MOVE)
      pass = 0;
    else
      pass++;
    color_to_play = OTHER_COLOR(color_to_play);
  }
  
  /* Reenable worm and dragon threats and endgame patterns. */
  disable_threat_computation = 0;
  disable_endgame_patterns   = 0;
}

static struct aftermath_data aftermath;

static void
play_aftermath(int color)
{
  int pos;
  struct board_state saved_board;
  struct aftermath_data *a = &aftermath;
  static int current_board[BOARDMAX];
  static int current_color = EMPTY;
  int cached_board = 1;
  gg_assert(color == BLACK || color == WHITE);

  if (current_color != color) {
    current_color = color;
    cached_board = 0;
  }

  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (ON_BOARD(pos) && board[pos] != current_board[pos]) {
      current_board[pos] = board[pos];
      cached_board = 0;
    }
  }

  /* If this is exactly the same position as the one we analyzed the
   * last time, the content of the aftermath struct is up to date.
   */
  if (cached_board)
    return;

  a->white_captured = white_captured;
  a->black_captured = black_captured;
  a->white_prisoners = 0;
  a->black_prisoners = 0;
  a->white_territory = 0;
  a->black_territory = 0;
  a->white_area = 0;
  a->black_area = 0;
  
  store_board(&saved_board);
  do_play_aftermath(color, a);
  restore_board(&saved_board);
  
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (!ON_BOARD(pos))
      continue;
    if (a->black_control[pos]) {
      a->black_area++;
      if (board[pos] == WHITE) {
	a->black_territory++;
	a->white_prisoners++;
	a->final_status[pos] = DEAD;
      }
      else if (board[pos] == EMPTY) {
	a->black_territory++;
	a->final_status[pos] = BLACK_TERRITORY;
      }
      else
	a->final_status[pos] = ALIVE;
    }
    else if (a->white_control[pos]) {
      a->white_area++;
      if (board[pos] == BLACK) {
	a->white_territory++;
	a->black_prisoners++;
	a->final_status[pos] = DEAD;
      }
      else if (board[pos] == EMPTY) {
	a->white_territory++;
	a->final_status[pos] = WHITE_TERRITORY;
      }
      else
	a->final_status[pos] = ALIVE;
    }
    else {
      if (board[pos] == EMPTY)
	a->final_status[pos] = DAME;
      else {
	a->final_status[pos] = ALIVE_IN_SEKI;
	if (board[pos] == WHITE)
	  a->white_area++;
	else
	  a->black_area++;
      }
    }
  }

  if (debug & DEBUG_AFTERMATH) {
    gprintf("White captured: %d\n", a->white_captured);
    gprintf("Black captured: %d\n", a->black_captured);
    gprintf("White prisoners: %d\n", a->white_prisoners);
    gprintf("Black prisoners: %d\n", a->black_prisoners);
    gprintf("White territory: %d\n", a->white_territory);
    gprintf("Black territory: %d\n", a->black_territory);
    gprintf("White area: %d\n", a->white_area);
    gprintf("Black area: %d\n", a->black_area);
  }
}

float
aftermath_compute_score(int color, float komi, SGFTree *tree)
{
  struct aftermath_data *a = &aftermath;
  aftermath_sgftree = tree;
  play_aftermath(color);
  if (chinese_rules)
    return (a->white_area
	    - a->black_area
	    + komi);
  else
    return (a->white_territory
	    + a->black_captured
	    + a->black_prisoners
	    - (a->black_territory
	       + a->white_captured
	       + a->white_prisoners)
	    + komi);
}

/* Report the final status of a vertex on the board.
 * Possible results are ALIVE, DEAD, ALIVE_IN_SEKI, WHITE_TERRITORY,
 * BLACK_TERRITORY, and DAME.
 */
int
aftermath_final_status(int color, int pos)
{
  ASSERT_ON_BOARD1(pos);
  aftermath_sgftree = NULL;
  play_aftermath(color);
  return aftermath.final_status[pos];
}

/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
