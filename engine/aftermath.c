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




#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "liberty.h"

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
 * IMPORTANT RESTRICTION:
 * Before calling this function it is mandatory to call genmove() or
 * genmove_conservative(). For this function to be meaningful, the
 * genmove() call should return pass.
 */
int
aftermath_genmove(int *i, int *j, int color,
		  int under_control[MAX_BOARD][MAX_BOARD],
		  int do_capture_dead_stones)
{
  int m, n;
  int k;
  int other = OTHER_COLOR(color);
  int distance[MAX_BOARD][MAX_BOARD];
  int score[MAX_BOARD][MAX_BOARD];
  float owl_hotspot[MAX_BOARD][MAX_BOARD];
  float reading_hotspot[MAX_BOARD][MAX_BOARD];
  int dragoni[MAX_BOARD][MAX_BOARD];
  int dragonj[MAX_BOARD][MAX_BOARD];
  int something_found;
  int closest_opponent_i = -1;
  int closest_opponent_j = -1;
  int closest_own_i = -1;
  int closest_own_j = -1;
  int d;
  int ai = -1;
  int aj = -1;
  int pos = 0;
  int best_score;
  int best_i;
  int best_j;
  
  owl_hotspots(owl_hotspot);
  reading_hotspots(reading_hotspot);
  
  /* As a preparation we compute a distance map to the invincible strings. */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      if (BOARD(m, n) == color && worm[m][n].unconditional_status == ALIVE)
	distance[m][n] = 0;
      else if (!do_capture_dead_stones
	       && BOARD(m, n) == other 
	       && worm[m][n].unconditional_status == DEAD)
	distance[m][n] = 0;
      else
	distance[m][n] = -1;
    }

  d = 0;
  do {
    something_found = 0;
    for (m = 0; m < board_size; m++)
      for (n = 0; n < board_size; n++) {
	if (distance[m][n] == -1) {
	  for (k = 0; k < 4; k++) {
	    int dm = deltai[k];
	    int dn = deltaj[k];
	    if (!ON_BOARD2(m+dm, n+dn))
	      continue;
	    if ((d == 0 || BOARD(m+dm, n+dn) == EMPTY)
		&& distance[m+dm][n+dn] == d) {
	      if (d > 0 && BOARD(m, n) == other) {
		distance[m][n] = d + 1;
		if (closest_opponent_i == -1) {
		  closest_opponent_i = m;
		  closest_opponent_j = n;
		}
	      }
	      else if (d > 0 && BOARD(m, n) == color) {
		distance[m][n] = d + 1;
		if (closest_own_i == -1) {
		  closest_own_i = m;
		  closest_own_j = n;
		}
	      }
	      else if (BOARD(m, n) == EMPTY) {
		distance[m][n] = d + 1;
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
    for (m = 0; m < board_size; m++)
      for (n = 0; n < board_size; n++) {
	if (distance[m][n] == -1)
	  under_control[m][n] = 0;
	else
	  under_control[m][n] = 1;
      }
  }
  
  if (debug & DEBUG_AFTERMATH) {
    for (m = 0; m < board_size; m++) {
      for (n = 0; n < board_size; n++) {
	if (distance[m][n] > 0)
	  printf("%2d", distance[m][n]);
	else if (distance[m][n] == 0) {
	  if (BOARD(m, n) == WHITE)
	    printf(" o");
	  else if (BOARD(m, n) == BLACK)
	    printf(" x");
	  else
	    printf(" ?");
	}
	else {
	  if (BOARD(m, n) == WHITE)
	    printf(" O");
	  else if (BOARD(m, n) == BLACK)
	    printf(" X");
	  else
	    printf(" .");
	}
      }
      printf("\n");
    }
  
    gprintf("Closest opponent %m", closest_opponent_i, closest_opponent_j);
    if (closest_opponent_i != -1)
      gprintf(", distance %d\n",
	      distance[closest_opponent_i][closest_opponent_j]);
    else
      gprintf("\n");

    gprintf("Closest own %m", closest_own_i, closest_own_j);
    if (closest_own_i != -1)
      gprintf(", distance %d\n", distance[closest_own_i][closest_own_j]);
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
  best_i = -1;
  best_j = -1;
  best_score = 5;
  for (m = 0; m < board_size; m++) {
    for (n = 0; n < board_size; n++) {
      int libs;
      if (distance[m][n] == 0
	  || BOARD(m, n) != EMPTY)
	continue;

      libs = approxlib(POS(m, n), color, 3, NULL);
      if (libs < 3)
	continue;

      for (k = 0; k < 4; k++) {
	int dm = deltai[k];
	int dn = deltaj[k];
	if (!ON_BOARD2(m-dm, n-dn)
	    && BOARD(m+dm, n+dn) == color
	    && libs > countlib2(m+dm, n+dn)
	    && (DRAGON2(m+dm, n+dn).safety == INVINCIBLE
		|| DRAGON2(m+dm, n+dn).safety == STRONGLY_ALIVE)) {
	  int score = 20 * (owl_hotspot[m][n] + reading_hotspot[m][n]);
	  if (score > best_score) {
	    best_score = score;
	    best_i = m;
	    best_j = n;
	  }
	}
      }
    }
  }
  if (best_i != -1 && safe_move2(best_i, best_j, color) == WIN) {
    *i = best_i;
    *j = best_j;
    DEBUG(DEBUG_AFTERMATH, "Closing edge at %m\n", best_i, best_j);
    return 1;
  }

  /* Case 1. */
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    int lib;
    m = I(pos);
    n = J(pos);
    if (board[pos] == other
	&& worm[m][n].unconditional_status != DEAD
	&& countlib(pos) == 1
	&& ((ON_BOARD(SOUTH(pos))    && distance[m+1][n] == 0)
	    || (ON_BOARD(WEST(pos))  && distance[m][n-1] == 0)
	    || (ON_BOARD(NORTH(pos)) && distance[m-1][n] == 0)
	    || (ON_BOARD(EAST(pos))  && distance[m][n+1] == 0))) {
      findlib(pos, 1, &lib);
      /* Make sure we don't play into a ko or a (proper) snapback. */
      if (countstones(pos) > 1 || !is_self_atari(lib, color)) {
	*i = I(lib);
	*j = J(lib);
	return 1;
      }
    }
  }

  /* Cases 2--4. */
  if (closest_opponent_i != -1 || closest_own_i != -1) {
    if (closest_own_i == -1) {
      ai = closest_opponent_i;
      aj = closest_opponent_j;
    }
    else {
      ai = closest_own_i;
      aj = closest_own_j;
    }

    /* if we're about to play at distance 1, try to optimize the move. */
    if (distance[ai][aj] == 2) {
      char mx[BOARDMAX];
      char mark = 0;
      memset(mx, 0, sizeof(mx));
      best_score = 0;
      best_i = ai;
      best_j = aj;

      for (m = 0; m < board_size; m++)
	for (n = 0; n < board_size; n++) {
	  int score = 0;
	  int move_ok = 0;
	  if (distance[m][n] != 1)
	    continue;
	  mark++;
	  for (k = 0; k < 4; k++) {
	    int dm = deltai[k];
	    int dn = deltaj[k];
	    if (!ON_BOARD2(m+dm, n+dn))
	      continue;
	    if (distance[m+dm][n+dn] < 1)
	      score--;
	    else if (BOARD(m+dm, n+dn) == EMPTY)
	      score++;
	    else if (mx[POS(m+dm, n+dn)] == mark)
	      score--;
	    else {
	      if (BOARD(m+dm, n+dn) == color) {
		move_ok = 1;
		score += 7;
		if (countstones2(m+dm, n+dn) > 2)
		  score++;
		if (countstones2(m+dm, n+dn) > 4)
		  score++;
		if (countlib2(m+dm, n+dn) < 4)
		  score++;
		if (countlib2(m+dm, n+dn) < 3)
		  score++;
	      }
	      else {
		int deltalib = (approxlib(POS(m, n), other, MAXLIBS, NULL)
				- countlib2(m+dm, n+dn));
		move_ok = 1;
		score++;
		if (deltalib >= 0)
		  score++;
		if (deltalib > 0)
		  score++;
	      }
	      mark_string(POS(m+dm, n+dn), mx, mark);
	    }
	  }
	  if (is_suicide2(m, n, other))
	    score -= 3;
	  
	  if (0)
	    gprintf("Score %m = %d\n", m, n, score);
	  
	  if (move_ok && score > best_score) {
	    best_score = score;
	    best_i = m;
	    best_j = n;
	  }
	}
      ai = best_i;
      aj = best_j;
    }

    while (distance[ai][aj] > 1) {
      for (k = 0; k < 4; k++) {
	int di = deltai[k];
	int dj = deltaj[k];
	if (ON_BOARD2(ai+di, aj+dj)
	    && BOARD(ai+di, aj+dj) == EMPTY
	    && distance[ai+di][aj+dj] == distance[ai][aj] - 1) {
	  ai = ai + di;
	  aj = aj + dj;
	  break;
	}
      }
    }
    *i = ai;
    *j = aj;
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
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      int eyespace_neighbors = 0;
      int own_neighbors = 0;
      int own_diagonals = 0;
      int opponent_dragons = 0;
      int own_worms = 0;
      int safety = UNKNOWN;
      int bonus = 0;
      int mx[BOARDMAX];
      score[m][n] = 0;
      
      if (BOARD(m, n) != EMPTY || distance[m][n] != -1)
	continue;

      memset(mx, 0, sizeof(mx));

      for (k = 0; k < 8; k++) {
	int dm = deltai[k];
	int dn = deltaj[k];
	if (!ON_BOARD2(m+dm, n+dn))
	  continue;
	
	if (BOARD(m+dm, n+dn) == EMPTY) {
	  if (k < 4)
	    eyespace_neighbors++;
	  continue;
	}
	
	if (BOARD(m+dm, n+dn) == other) {
	  int origin = dragon[m+dm][n+dn].origin;

	  if (k < 4) {
	    if (dragon[m+dm][n+dn].matcher_status == ALIVE) {
	      safety = DEAD;
	      break;
	    }
	    else if (!mx[origin]) {
	      eyespace_neighbors++;
	      opponent_dragons++;
	    }
	  }

	  if (!mx[origin] && dragon[m+dm][n+dn].matcher_status == DEAD) {
	    bonus++;
	    if (k < 4 
		&& countlib2(m+dm, n+dn) <= 2 
		&& countstones2(m+dm, n+dn) >= 3)
	      bonus++;
	    
	    if (k < 4 && countlib2(m+dm, n+dn) == 1)
	      bonus += 3;
	  }
	  mx[origin] = 1;
	}
	else if (BOARD(m+dm, n+dn) == color) {
	  dragoni[m][n] = m+dm;
	  dragonj[m][n] = n+dn;

	  if (safety == UNKNOWN && dragon[m+dm][n+dn].matcher_status == ALIVE)
	    safety = ALIVE;

	  if (DRAGON2(m+dm, n+dn).safety == INVINCIBLE)
	    safety = INVINCIBLE;
	  
	  if (k < 4) {
	    int apos = worm[m+dm][n+dn].origin;
	    
	    if (!mx[apos]) {
	      own_worms++;
	      if (countstones(apos) == 1)
		bonus += 2;
	      mx[apos] = 1;
	    }
	    
	    if (countlib(apos) <= 2) {
	      int r;
	      int important = 0;
	      int safe_atari = 0;
	      for (r = 0; r < 4; r++) {
		int  d = delta[r];
#if 0
		int di = deltai[r];
		int dj = deltaj[r];
#endif
		if (!ON_BOARD(apos+d))
		  continue;
		if (board[apos+d] == other
		    && dragon[I(apos+d)][J(apos+d)].matcher_status == DEAD)
		  important = 1;
		else if (board[apos+d] == EMPTY
		    && !is_self_atari(apos+d, other))
		  safe_atari = 1;
	      }
	      if (approxlib(POS(m, n), color, 3, NULL) > 2) {
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

      score[m][n] = 4 * eyespace_neighbors + bonus;
      if (safety == INVINCIBLE) {
	score[m][n] += own_neighbors;
	if (own_neighbors < 2)
	  score[m][n] += own_diagonals;
	if (own_worms > 1 && eyespace_neighbors >= 1)
	  score[m][n] += 10 + 5 * (own_worms - 2);
      }
      else if (eyespace_neighbors > 2)
	score[m][n] += own_diagonals;

      /* Splitting bonus. */
      if (opponent_dragons > 1)
	score[m][n] += 10 * (opponent_dragons - 1);

      score[m][n] += (int) (20.0 * owl_hotspot[m][n]);
      score[m][n] += (int) (20.0 * reading_hotspot[m][n]);

      if (1 && (debug & DEBUG_AFTERMATH))
	gprintf("Score %M = %d (hotspot bonus %d + %d)\n", m, n, score[m][n],
		(int) (20.0 * owl_hotspot[m][n]),
		(int) (20.0 * reading_hotspot[m][n]));
    }

  while (1) {
    int bi, bj;
    best_score = 0;
    ai = -1;
    aj = -1;
    for (m = 0; m < board_size; m++)
      for (n = 0; n < board_size; n++) {
	if (score[m][n] > best_score) {
	  best_score = score[m][n];
	  ai = m;
	  aj = n;
	}
      }

    if (ai == -1)
      break;

    bi = dragoni[ai][aj];
    bj = dragonj[ai][aj];
    if (is_illegal_ko_capture2(ai, aj, color)
	|| !safe_move2(ai, aj, color)
	|| (DRAGON2(bi, bj).safety != INVINCIBLE
	    && DRAGON2(bi, bj).safety != STRONGLY_ALIVE
	    && !owl_does_defend(ai, aj, bi, bj))
	|| (!confirm_safety(ai, aj, color, 0, NULL, NULL))) {
      score[ai][aj] = 0;
    }
    else {
      *i = ai;
      *j = aj;
      return 1;
    }
  }

  /* Case 6.
   * Finally we try to play on liberties of remaining DEAD opponent
   * dragons, carefully checking against mistakes.
   */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      int bi, bj;
      int self_atari_ok = 0;
      if (BOARD(m, n) != EMPTY || distance[m][n] != -1)
	continue;
      ai = -1;
      aj = -1;
      for (k = 0; k < 4; k++) {
	int dm = deltai[k];
	int dn = deltaj[k];
	if (!ON_BOARD2(m+dm, n+dn))
	  continue;
	if (BOARD(m+dm, n+dn) == other 
	    && dragon[m+dm][n+dn].matcher_status != ALIVE
	    && (do_capture_dead_stones 
		|| worm[m+dm][n+dn].unconditional_status != DEAD)
	    && DRAGON2(m+dm, n+dn).safety != INESSENTIAL) {
	  ai = m + dm;
	  aj = n + dn;
	  break;
	}
      }
      if (ai == -1)
	continue;

      /* At this point, (m, n) is a move that potentially may capture
       * a dead opponent string at (ai, aj).
       */
      
      if (!trymove2(m, n, color, "aftermath-A", ai, aj, EMPTY, -1, -1))
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

      if ((m > 0 
	   && BOARD(m-1, n) == color
	   && DRAGON2(m-1, n).safety != INESSENTIAL)
	  || (m < board_size - 1
	      && BOARD(m+1, n) == color
	      && DRAGON2(m+1, n).safety != INESSENTIAL)
	  || (n > 0 
	      && BOARD(m, n-1) == color
	      && DRAGON2(m, n-1).safety != INESSENTIAL)
	  || (n < board_size - 1
	      && BOARD(m, n+1) == color
	      && DRAGON2(m, n+1).safety != INESSENTIAL))
	self_atari_ok = 0;
      else
	self_atari_ok = 1;

      /* Copy the potential move to (bi, bj). */
      bi = m;
      bj = n;

      /* If the move is a self atari, but that isn't okay, try to
       * recursively find a backfilling move which later makes the
       * potential move possible.
       */
      if (!self_atari_ok) {
	while (countlib2(m, n) == 1) {
	  int lib;
	  findlib(POS(m, n), 1, &lib);
	  bi = I(lib);
	  bj = J(lib);
	  if (!trymove2(bi, bj, color, "aftermath-B", ai, aj, EMPTY, -1, -1))
	    break;
	}
	
	if (countlib2(m, n) == 1) {
	  bi = -1;
	  bj = -1;
	}
      }

      while (stackp > 0)
	popgo();

      if (bi == -1)
	continue;
      
      /* Make sure that the potential move really isn't a self
       * atari. In the case of a move found after backfilling this
       * could happen (because the backfilling moves happened to
       * capture some stones).
       */
      if (!self_atari_ok && is_self_atari2(bi, bj, color))
	continue;
      
      /* Consult the owl code to determine whether the considered move
       * really is effective. Blunders should be detected here.
       */
      if (owl_does_attack(bi, bj, ai, aj)) {
	*i = bi;
	*j = bj;
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
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++)
      if (BOARD(m, n) == other
	  && (worm[m][n].unconditional_status == UNKNOWN
	      || do_capture_dead_stones)
	  && (DRAGON2(m, n).safety == DEAD
	      || DRAGON2(m, n).safety == TACTICALLY_DEAD)
	  && worm[m][n].attack_code != 0) {
	*i = I(worm[m][n].attack_point);
	*j = J(worm[m][n].attack_point);
	return 1;
      }
      
  /* No move found. */
  return -1;
}


/* Preliminary function for playing through the aftermath. */
static void
do_play_aftermath(int color, struct aftermath_data *a)
{
  int i, j;
  int pass = 0;
  int moves = 0;
  int color_to_play = color;
  DEBUG(DEBUG_AFTERMATH, "The aftermath starts.\n");

  /* Disable computing worm and owl threats. */
  disable_threat_computation = 1;
  /* Disable matching of endgame patterns. */
  disable_endgame_patterns = 1;

  while (pass < 2 && moves < board_size * board_size) {
    int reading_nodes = get_reading_node_counter();
    int owl_nodes = get_owl_node_counter();
    int move_val = genmove_conservative(&i, &j, color_to_play);
    if (move_val < 0)
      move_val = aftermath_genmove(&i, &j, color_to_play,
				   (color_to_play == WHITE ?
				    a->white_control : a->black_control),
				   0);
    play_move(POS(i, j), color_to_play);
    moves++;
    DEBUG(DEBUG_AFTERMATH, "%d %C move %m (nodes %d, %d  total %d, %d)\n",
	  movenum, color_to_play, i, j, get_owl_node_counter() - owl_nodes,
	  get_reading_node_counter() - reading_nodes,
	  get_owl_node_counter(), get_reading_node_counter());
    if (i != -1)
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
  int m, n;
  Position saved_pos;
  struct aftermath_data *a = &aftermath;
  static int current_board[MAX_BOARD][MAX_BOARD];
  static int current_color = EMPTY;
  int cached_board = 1;
  gg_assert(color == BLACK || color == WHITE);

  if (current_color != color) {
    current_color = color;
    cached_board = 0;
  }

  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++)
      if (BOARD(m, n) != current_board[m][n]) {
	current_board[m][n] = BOARD(m, n);
	cached_board = 0;
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
  
  store_position(&saved_pos);
  do_play_aftermath(color, a);
  restore_position(&saved_pos);
  
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      if (a->black_control[m][n]) {
	a->black_area++;
	if (BOARD(m, n) == WHITE) {
	  a->black_territory++;
	  a->white_prisoners++;
	  a->final_status[m][n] = DEAD;
	}
	else if (BOARD(m, n) == EMPTY) {
	  a->black_territory++;
	  a->final_status[m][n] = BLACK_TERRITORY;
	}
	else
	  a->final_status[m][n] = ALIVE;
      }
      else if (a->white_control[m][n]) {
	a->white_area++;
	if (BOARD(m, n) == BLACK) {
	  a->white_territory++;
	  a->black_prisoners++;
	  a->final_status[m][n] = DEAD;
	}
	else if (BOARD(m, n) == EMPTY) {
	  a->white_territory++;
	  a->final_status[m][n] = WHITE_TERRITORY;
	}
	else
	  a->final_status[m][n] = ALIVE;
      }
      else {
	if (BOARD(m, n) == EMPTY)
	  a->final_status[m][n] = DAME;
	else {
	  a->final_status[m][n] = ALIVE_IN_SEKI;
	  if (BOARD(m, n) == WHITE)
	    a->white_area++;
	  else
	    a->black_area++;
	}
      }
    }
}

float
aftermath_compute_score(int color, float komi)
{
  struct aftermath_data *a = &aftermath;
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
aftermath_final_status(int color, int m, int n)
{
  ASSERT_ON_BOARD2(m, n);
  play_aftermath(color);
  return aftermath.final_status[m][n];
}

/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
