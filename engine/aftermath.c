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
aftermath_genmove(int *aftermath_move, int color,
		  int under_control[BOARDMAX],
		  int do_capture_dead_stones)
{
  int m, n;
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
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      pos = POS(m, n);
      if (board[pos] == color && worm[pos].unconditional_status == ALIVE)
	distance[pos] = 0;
      else if (!do_capture_dead_stones
	       && board[pos] == other 
	       && worm[pos].unconditional_status == DEAD)
	distance[pos] = 0;
      else
	distance[pos] = -1;
    }

  d = 0;
  do {
    something_found = 0;
    for (m = 0; m < board_size; m++)
      for (n = 0; n < board_size; n++) {
	pos = POS(m, n);
	if (distance[pos] == -1) {
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
    for (m = 0; m < board_size; m++)
      for (n = 0; n < board_size; n++) {
	pos = POS(m, n);
	if (distance[pos] == -1)
	  under_control[pos] = 0;
	else
	  under_control[pos] = 1;
      }
  }
  
  if (debug & DEBUG_AFTERMATH) {
    for (m = 0; m < board_size; m++) {
      for (n = 0; n < board_size; n++) {
	pos = POS(m, n);
	if (distance[pos] > 0)
	  printf("%2d", distance[pos]);
	else if (distance[pos] == 0) {
	  if (board[pos] == WHITE)
	    printf(" o");
	  else if (board[pos] == BLACK)
	    printf(" x");
	  else
	    printf(" ?");
	}
	else {
	  if (board[pos] == WHITE)
	    printf(" O");
	  else if (board[pos] == BLACK)
	    printf(" X");
	  else
	    printf(" .");
	}
      }
      printf("\n");
    }
  
    gprintf("Closest opponent %1m", closest_opponent);
    if (closest_opponent != NO_MOVE)
      gprintf(", distance %d\n",
	      distance[closest_opponent]);
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
  for (m = 0; m < board_size; m++) {
    for (n = 0; n < board_size; n++) {
      int libs;
      pos = POS(m, n);
      if (distance[pos] == 0
	  || board[pos] != EMPTY)
	continue;

      libs = approxlib(pos, color, 3, NULL);
      if (libs < 3)
	continue;

      for (k = 0; k < 4; k++) {
	int dir = delta[k];
	if (!ON_BOARD(pos - dir)
	    && board[pos + dir] == color
	    && libs > countlib(pos + dir)
	    && (DRAGON2(pos + dir).safety == INVINCIBLE
		|| DRAGON2(pos + dir).safety == STRONGLY_ALIVE)) {
	  int score = 20 * (owl_hotspot[pos] + reading_hotspot[pos]);
	  if (score > best_score) {
	    best_score = score;
	    best_scoring_move = pos;
	  }
	}
      }
    }
  }
  
  if (best_scoring_move != NO_MOVE
      && safe_move(best_scoring_move, color) == WIN) {
    *aftermath_move = best_scoring_move;
    DEBUG(DEBUG_AFTERMATH, "Closing edge at %1m\n", best_scoring_move);
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

      for (m = 0; m < board_size; m++)
	for (n = 0; n < board_size; n++) {
	  int score = 0;
	  int move_ok = 0;
	  pos = POS(m, n);
	  if (distance[pos] != 1)
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
	    gprintf("Score %m = %d\n", m, n, score);
	  
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
      pos = POS(m, n);
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
	    if (dragon[pos2].matcher_status == ALIVE) {
	      safety = DEAD;
	      break;
	    }
	    else if (!mx[origin]) {
	      eyespace_neighbors++;
	      opponent_dragons++;
	    }
	  }

	  if (!mx[origin] && dragon[pos2].matcher_status == DEAD) {
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

	  if (safety == UNKNOWN && dragon[pos2].matcher_status == ALIVE)
	    safety = ALIVE;

	  if (DRAGON2(pos2).safety == INVINCIBLE)
	    safety = INVINCIBLE;
	  
	  if (k < 4) {
	    int apos = worm[pos2].origin;
	    
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
		if (!ON_BOARD(apos+d))
		  continue;
		if (board[apos+d] == other
		    && dragon[apos+d].matcher_status == DEAD)
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

      score[pos] += (int) (20.0 * owl_hotspot[pos]);
      score[pos] += (int) (20.0 * reading_hotspot[pos]);

      if (1 && (debug & DEBUG_AFTERMATH))
	gprintf("Score %1M = %d (hotspot bonus %d + %d)\n", pos, score[pos],
		(int) (20.0 * owl_hotspot[pos]),
		(int) (20.0 * reading_hotspot[pos]));
    }

  while (1) {
    int bb;
    best_score = 0;
    move = NO_MOVE;
    for (m = 0; m < board_size; m++)
      for (n = 0; n < board_size; n++) {
	pos = POS(m, n);
	if (score[pos] > best_score) {
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
	    && owl_does_defend(move, bb) != WIN)
	|| (!confirm_safety(move, color, 0, NULL))) {
      score[move] = 0;
    }
    else {
      *aftermath_move = move;
      return 1;
    }
  }

  /* Case 6.
   * Finally we try to play on liberties of remaining DEAD opponent
   * dragons, carefully checking against mistakes.
   */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      int bb;
      int self_atari_ok = 0;
      pos = POS(m, n);
      if (board[pos] != EMPTY || distance[pos] != -1)
	continue;
      move = NO_MOVE;
      for (k = 0; k < 4; k++) {
	int pos2 = pos + delta[k];
	if (!ON_BOARD(pos2))
	  continue;
	if (board[pos2] == other 
	    && dragon[pos2].matcher_status != ALIVE
	    && (do_capture_dead_stones 
		|| worm[pos2].unconditional_status != DEAD)
	    && DRAGON2(pos2).safety != INESSENTIAL) {
	  move = pos2;
	  break;
	}
      }
      if (move == NO_MOVE)
	continue;

      /* At this point, (m, n) is a move that potentially may capture
       * a dead opponent string at (move).
       */
      
      if (!trymove(pos, color, "aftermath-A", move, EMPTY, NO_MOVE))
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

      if ((board[SOUTH(pos)] == color
	   && DRAGON2(SOUTH(pos)).safety != INESSENTIAL)
	  || (board[WEST(pos)] == color
	      && DRAGON2(WEST(pos)).safety != INESSENTIAL)
	  || (board[NORTH(pos)] == color
	      && DRAGON2(NORTH(pos)).safety != INESSENTIAL)
	  || (board[EAST(pos)] == color
	      && DRAGON2(EAST(pos)).safety != INESSENTIAL))
	self_atari_ok = 0;
      else
	self_atari_ok = 1;

      /* Copy the potential move to (bb). */
      bb = pos;

      /* If the move is a self atari, but that isn't okay, try to
       * recursively find a backfilling move which later makes the
       * potential move possible.
       */
      if (!self_atari_ok) {
	while (countlib(pos) == 1) {
	  int lib;
	  findlib(pos, 1, &lib);
	  bb = lib;
	  if (!trymove(bb, color, "aftermath-B", move, EMPTY, NO_MOVE))
	    break;
	}
	
	if (countlib(pos) == 1)
	  bb = NO_MOVE;
      }

      while (stackp > 0)
	popgo();

      if (bb == NO_MOVE)
	continue;
      
      /* Make sure that the potential move really isn't a self
       * atari. In the case of a move found after backfilling this
       * could happen (because the backfilling moves happened to
       * capture some stones).
       */
      if (!self_atari_ok && is_self_atari(bb, color))
	continue;
      
      /* Consult the owl code to determine whether the considered move
       * really is effective. Blunders should be detected here.
       */
      if (owl_does_attack(bb, move) == WIN) {
	*aftermath_move = bb;
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
      pos = POS(m, n);
      if (board[pos] == other
	  && (worm[pos].unconditional_status == UNKNOWN
	      || do_capture_dead_stones)
	  && (DRAGON2(pos).safety == DEAD
	      || DRAGON2(pos).safety == TACTICALLY_DEAD)
	  && worm[pos].attack_codes[0] != 0) {
	*aftermath_move = worm[pos].attack_points[0];
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
  int move;
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
    move = POS(i, j);
    if (move_val < 0)
      move_val = aftermath_genmove(&move, color_to_play,
				   (color_to_play == WHITE ?
				    a->white_control : a->black_control),
				   0);
    play_move(move, color_to_play);
    moves++;
    DEBUG(DEBUG_AFTERMATH, "%d %C move %1m (nodes %d, %d  total %d, %d)\n",
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
  int m, n;
  int ii;
  Position saved_pos;
  struct aftermath_data *a = &aftermath;
  static int current_board[BOARDMAX];
  static int current_color = EMPTY;
  int cached_board = 1;
  gg_assert(color == BLACK || color == WHITE);

  if (current_color != color) {
    current_color = color;
    cached_board = 0;
  }

  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      ii = POS(m, n);

      if (board[ii] != current_board[ii]) {
	current_board[ii] = board[ii];
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
  
  store_position(&saved_pos);
  do_play_aftermath(color, a);
  restore_position(&saved_pos);
  
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      ii = POS(m, n);

      if (a->black_control[ii]) {
	a->black_area++;
	if (board[ii] == WHITE) {
	  a->black_territory++;
	  a->white_prisoners++;
	  a->final_status[ii] = DEAD;
	}
	else if (board[ii] == EMPTY) {
	  a->black_territory++;
	  a->final_status[ii] = BLACK_TERRITORY;
	}
	else
	  a->final_status[ii] = ALIVE;
      }
      else if (a->white_control[ii]) {
	a->white_area++;
	if (board[ii] == BLACK) {
	  a->white_territory++;
	  a->black_prisoners++;
	  a->final_status[ii] = DEAD;
	}
	else if (board[ii] == EMPTY) {
	  a->white_territory++;
	  a->final_status[ii] = WHITE_TERRITORY;
	}
	else
	  a->final_status[ii] = ALIVE;
      }
      else {
	if (board[ii] == EMPTY)
	  a->final_status[ii] = DAME;
	else {
	  a->final_status[ii] = ALIVE_IN_SEKI;
	  if (board[ii] == WHITE)
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
aftermath_final_status(int color, int pos)
{
  ASSERT_ON_BOARD1(pos);
  play_aftermath(color);
  return aftermath.final_status[pos];
}

/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
