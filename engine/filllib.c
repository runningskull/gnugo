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

static int find_backfilling_move(int move, int color, int *backfill_move);
static int filllib_confirm_safety(int move, int color, int *defense_point);

/* Determine whether a point is adjacent to at least one own string which
 * isn't dead.
 */
static int
living_neighbor(int pos, int color)
{
  int k;
  for (k = 0; k < 4; k++) {
    if (board[pos + delta[k]] == color
	&& dragon[pos + delta[k]].status != DEAD)
      return 1;
  }

  return 0;
}


/* Determine whether (pos) effectively is a black or white point.
 * The test for inessentiality is to avoid filling the liberties
 * around a killing nakade string.
 */
static void
analyze_neighbor(int pos, int *found_black, int *found_white)
{
  switch (board[pos]) {
    case EMPTY:
      if (!(*found_black)
	  && living_neighbor(pos, BLACK)
	  && safe_move(pos, WHITE) != WIN)
	*found_black = 1;
      
      if (!(*found_white)
	  && living_neighbor(pos, WHITE)
	  && safe_move(pos, BLACK) != WIN)
	*found_white = 1;
      
      break;

    case BLACK:
      if (!worm[pos].inessential && DRAGON2(pos).safety != INESSENTIAL) {
	if (dragon[pos].status == ALIVE
	    || dragon[pos].status == UNKNOWN)
	  *found_black = 1;
	else
	  *found_white = 1;
      }
      break;

    case WHITE:
      if (!worm[pos].inessential && DRAGON2(pos).safety != INESSENTIAL) {
	if (dragon[pos].status == ALIVE
	    || dragon[pos].status == UNKNOWN)
	  *found_white = 1;
	else
	  *found_black = 1;
      }
      break;
  }
}


/* If no move of value can be found to play, this seeks to fill a
 * common liberty, backfilling or back-capturing if necessary. When
 * backfilling we take care to start from the right end, in the case
 * that several backfilling moves are ultimately necessary.
 *
 * If a move for color is found, return 1, otherwise return 0.
 * The move is returned in (*move).
 */

int 
fill_liberty(int *move, int color)
{
  int k;
  int pos;
  int other = OTHER_COLOR(color);
  int defense_point;
  int potential_color[BOARDMAX];

  /* We first make a fast scan for intersections which are potential
   * candidates for liberty filling. This is not very accurate, but it
   * does filter out intersections which could never pass the real
   * tests below but might still require a lot of tactical reading in
   * the process.
   */
  memset(potential_color, 0, sizeof(potential_color));
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (!IS_STONE(board[pos]))
      continue;

    if (worm[pos].inessential || DRAGON2(pos).safety == INESSENTIAL)
      continue;
    
    if (dragon[pos].status != ALIVE) {
      for (k = 0; k < 4; k++) {
	int pos2 = pos + delta[k];
	if (board[pos2] == EMPTY)
	  potential_color[pos2] |= OTHER_COLOR(board[pos]);
      }
    }
    
    if (dragon[pos].status != DEAD) {
      for (k = 0; k < 12; k++) {
	int d = delta[k%8];
	
	if (k >= 8) {
	  if (board[pos + d] != EMPTY)
	    continue;
	  d *= 2;
	}
	if (board[pos + d] == EMPTY)
	  potential_color[pos + d] |= board[pos];
      }
    }
  }
  
  
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    /* It seems we can't trust an empty liberty to be gray-colored
     * either as a cave or as a cavity. Instead we look for empty
     * intersections with at least one neighbor of each color, where
     * dead stones count as enemy stones. We also count empty
     * neighbors to either color if the opponent can't play there.
     */
    int found_white = 0;
    int found_black = 0;
    
    if (board[pos] != EMPTY)
      continue;

    /* Quick rejection based on preliminary test above. */
    if (potential_color[pos] != GRAY)
      continue;
    
    /* Loop over the neighbors. */
    for (k = 0; k < 4; k++) {
      int d = delta[k];
      if (ON_BOARD(pos + d))
	analyze_neighbor(pos + d, &found_black, &found_white);
    }
    
    /* Do we have neighbors of both colors? */
    if (!(found_white && found_black))
      continue;
    
    /* Ok, we wish to play here, but maybe we can't. The following
     * cases may occur:
     * 1. Move is legal and safe.
     * 2. Move is legal but not safe because it's in the middle of a seki.
     * 3. Move is legal but not safe, can be played after backfilling.
     * 4. Move is an illegal ko recapture.
     * 5. Move is illegal but can be played after back-captures.
     * 6. Move would violate confirm_safety.
     */
    
    TRACE_FILLLIB("Filllib: Considering move at %1m.\n", pos);
    
    /* Legal and tactically safe, play it if it passes
     * confirm_safety test, i.e. that it isn't a blunder which
     * causes problems for other strings.
     */
    if (safe_move(pos, color) == WIN) {
      TRACE_FILLLIB("Filllib: Tactically safe.\n");
      if (filllib_confirm_safety(pos, color, &defense_point)) {
	/* Safety confirmed. */
	TRACE_FILLLIB("Filllib: Safety confirmed.\n");
	*move = pos;
	return 1;
      }
      else if (defense_point != NO_MOVE) {
	/* Safety not confirmed because the move at (pos) would set
	 * up a double threat. (defense_point) is assumed to defend
	 * against this threat.
	 *
	 * FIXME: We should verify that (defense_point) really is effective.
	 */
	TRACE_FILLLIB(
	      "Filllib: Safety not confirmed, but %1m defends.\n",
	      defense_point);
	*move = defense_point;
	return 1;
      }
      else {
	/* The move causes problems somewhere else on the board, so
	 * we have to discard it. If everything works right this
	 * should not happen at this time.
	 */
	TRACE_FILLLIB("Filllib: Safety not confirmed, discarded.\n");
	TRACE("Warning: Blunder detected in fill_liberty().\n");
	continue;
      }
    }
    
    /* Try to play the move. */
    if (trymove(pos, color, "fill_liberty", NO_MOVE, EMPTY, NO_MOVE)) {
      popgo();
      /* Legal, but not safe. Look for backfilling move. */
      TRACE_FILLLIB(
	    "Filllib: Legal but not safe, looking for backfilling move.\n");
      
      if (find_backfilling_move(pos, color, move)) {
	TRACE_FILLLIB("Filllib: Backfilling move at %1m.\n", *move);
	/* In certain positions it may happen that an illegal move
	 * is found. This probably only can happen if we try to play
	 * a move inside a lost semeai. Anyway we should discard the
	 * move.
	 */
	if (!is_legal(*move, color)) {
	  TRACE_FILLLIB("Filllib: Was illegal, discarded.\n");
	  *move = NO_MOVE;
	  continue;
	}
	
	/* If the move turns out to be strategically unsafe, or
	 * setting up a double threat elsewhere, also discard it.
	 */
	if (!filllib_confirm_safety(*move, color, &defense_point)) {
	  TRACE_FILLLIB(
		"Filllib: Safety not confirmed, discarded.\n");
	  continue;
	}
	
	/* Seems to be ok. */
	return 1;
      }
      else {
	/* If we captured some stones, this move should be ok anyway. */
	if (does_capture_something(pos, color)) {
	  TRACE_FILLLIB(
		"Filllib: Not tactically safe, but captures stones.\n");
	  if (!filllib_confirm_safety(pos, color, &defense_point)) {
	    TRACE_FILLLIB(
		  "Filllib: Safety not confirmed, discarded.\n");
	    continue;
	  }
	  *move = pos;
	  return 1;
	}
      }
    }
    else {
      /* Move is illegal. Look for an attack on one of the neighbor
       * worms. If found, return that move for back-capture.
       */
      TRACE_FILLLIB("Filllib: Illegal, looking for back-capture.\n");
      for (k = 0; k < 4; k++) {
	int d = delta[k];
	if (board[pos + d] == other
	    && worm[pos + d].attack_codes[0] == WIN) {
	  *move = worm[pos + d].attack_points[0];
	  TRACE_FILLLIB("Filllib: Found at %1m.\n", *move);
	  return 1;
	}
      }
      
      TRACE_FILLLIB(
	    "Filllib: Nothing found, looking for ko back-capture.\n");
      for (k = 0; k < 4; k++) {
	int d = delta[k];
	if (board[pos + d] == other
	    && worm[pos + d].attack_codes[0] != 0
	    && is_legal(worm[pos + d].attack_points[0], color)) {
	  *move = worm[pos + d].attack_points[0];
	  TRACE_FILLLIB("Filllib: Found at %1m.\n", *move);
	  return 1;
	}
      }

      TRACE_FILLLIB(
	    "Filllib: Nothing found, looking for threat to back-capture.\n");
      for (k = 0; k < 4; k++) {
	int d = delta[k];
	if (board[pos + d] == other
	    && worm[pos + d].attack_codes[0] != 0) {
	  /* Just pick some other liberty. */
	  /* FIXME: Something is odd about this code. */
	  int libs[2];
	  if (findlib(pos + d, 2, libs) > 1) {
	    if (is_legal(libs[0], color))
	      *move = libs[0];
	    else if (is_legal(libs[1], color))
	      *move = libs[1];
	    else
	      continue;
	    
	    TRACE_FILLLIB("Filllib: Found at %1m.\n", *move);
	    return 1;
	  }
	}
      }
    }
  }
  
  /* Nothing found. */
  TRACE_FILLLIB("Filllib: No move found.\n");
  return 0;
}


/* The strategy for finding a backfilling move is to first identify
 * moves that
 *
 * 1. defends the position obtained after playing (move).
 * 2. captures a stone adjacent to our neighbors to (move), before
 *    (move) is played.
 *
 * Then we check which of these are legal before (move) is played. If
 * there is at least one, we take one of these arbitrarily as a
 * backfilling move.
 *
 * Now it may happen that (move) still isn't a safe move. In that case
 * we recurse to find a new backfilling move. To do things really
 * correctly we should also give the opponent the opportunity to keep
 * up the balance of the position by letting him do a backfilling move
 * of his own. Maybe this could also be arranged by recursing this
 * function. Currently we only do a half-hearted attempt to find
 * opponent moves.
 */
static int adjs[MAXCHAIN];
static int libs[MAXLIBS];

static int
find_backfilling_move(int move, int color, int *backfill_move)
{
  int k;
  int liberties;
  int neighbors;
  int found_one = 0;
  int apos = NO_MOVE;
  int bpos = NO_MOVE;
  int extra_pop = 0;
  int success = 0;
  int acode;
  int saved_move = NO_MOVE;
  int opponent_libs;
  
  /* Play (move) and identify all liberties and adjacent strings. */
  if (!trymove(move, color, "find_backfilling_move", move, EMPTY, NO_MOVE))
    return 0; /* This shouldn't happen, I believe. */

  /* The move wasn't safe, so there must be an attack for the
   * opponent. Save it for later use.
   */
  acode = attack(move, &apos);
  gg_assert(acode != 0 && apos > NO_MOVE);
  
  /* Find liberties. */
  liberties = findlib(move, MAXLIBS, libs);

  /* Find neighbors. */
  neighbors = chainlinks(move, adjs);

  /* Remove (move) again. */
  popgo();
  
  /* It's most fun to capture stones. Start by trying to take some
   * neighbor off the board. If the attacking move does not directly
   * reduce the number of liberties of the attacked string we don't
   * trust it but keep it around if we don't find anything else. (See
   * filllib:17 for a position where this matters.)
   *
   * It is also necessary to take care to first attack the string with
   * the fewest liberties, which can probably by removed the fastest.
   * See filllib:37 for an example (J5 tactically attacks K7 but the
   * correct move is H5).
   *
   * FIXME: It seems we have to return immediately when we find an
   * attacking move, because recursing for further backfilling might
   * lead to moves which complete the capture but cannot be played
   * before the attacking move itself. This is not ideal but probably
   * good enough.
   */
  for (opponent_libs = 1; opponent_libs <= 5; opponent_libs++) {
    for (k = 0; k < neighbors; k++) {
      if (opponent_libs < 5 && countlib(adjs[k]) != opponent_libs)
	continue;
      if (attack(adjs[k], &bpos) == WIN) {
	if (liberty_of_string(bpos, adjs[k])) {
	  *backfill_move = bpos;
	  return 1;
	}
	else
	  saved_move = bpos;
      }
    }
  }
  
  /* Otherwise look for a safe move at a liberty. */
  if (!found_one) {
    for (k = 0; k < liberties; k++) {
      if (safe_move(libs[k], color) == WIN) {
	*backfill_move = libs[k];
	found_one = 1;
	break;
      }
    }
  }

  /* If no luck so far, try with superstring liberties. */
  if (!found_one) {
    trymove(move, color, "find_backfilling_move", move, EMPTY, NO_MOVE);
    find_proper_superstring_liberties(move, &liberties, libs, 0);
    popgo();
    for (k = 0; k < liberties; k++) {
      if (safe_move(libs[k], color) == WIN) {
	*backfill_move = libs[k];
	found_one = 1;
	break;
      }
    }
  }

  /* If no luck so far, try attacking superstring neighbors. */
  if (!found_one) {
    trymove(move, color, "find_backfilling_move", move, EMPTY, NO_MOVE);
    superstring_chainlinks(move, &neighbors, adjs, 4);
    popgo();
    for (k = 0; k < neighbors; k++) {
      if (attack(adjs[k], &bpos) == WIN) {
	if (liberty_of_string(bpos, adjs[k])) {
	  *backfill_move = bpos;
	  return 1;
	}
      }
    }
  }

  if (found_one) {
  
    if (!trymove(*backfill_move, color, "find_backfilling_move", move,
		 EMPTY, NO_MOVE))
      return 0; /* This really shouldn't happen. */
    
    /* Allow opponent to get a move in here. */
    if (trymove(apos, OTHER_COLOR(color), "find_backfilling_move", move, 
		EMPTY, NO_MOVE))
      extra_pop = 1;
    
    /* If still not safe, recurse to find a new backfilling move. */
    if (safe_move(move, color) == WIN)
      success = 1;
    else
      success = find_backfilling_move(move, color, backfill_move);

    /* Pop move(s) and return. */
    if (extra_pop)
      popgo();
    popgo();
  }

  if (!success && saved_move != NO_MOVE) {
    *backfill_move = saved_move;
    success = 1;
  }

  return success;
}


/* Confirm that (move) is a safe move for color. In addition to
 * calling the global confirm_safety(), this function also calls the
 * owl code to verify the strategical viability of the move.
 */
static int
filllib_confirm_safety(int move, int color, int *defense_point)
{
  int k;
  int apos = NO_MOVE;
  int save_verbose;

  gg_assert(stackp == 0);
  gg_assert(defense_point != NULL);
  *defense_point = NO_MOVE;

  /* Before we can call the owl code, we need to find a neighbor of
   * our color.
   */
  for (k = 0; k < 4; k++)
    if (board[move + delta[k]] == color) {
      apos = move + delta[k];
      break;
    }

  /* If none found, look for a neighbor of an attacked adjacent string. */
  if (apos == NO_MOVE)
    for (k = 0; k < 4; k++) {
      int pos2 = move + delta[k];
      if (board[pos2] == OTHER_COLOR(color)
	  && !play_attack_defend_n(color, 0, 1, move, pos2)) {
	int adj;
	adj = chainlinks(pos2, adjs);
	/* It seems unlikely that we would ever get no adjacent strings
         * here, but if it should happen we simply give up and say the
         * move is unsafe.
	 */
	if (adj == 0)
	  return 0;
	
	apos = adjs[0];
	break;
      }
    }

  /* Next attempt are diagonal neighbors. */
  if (apos == NO_MOVE) {
    for (k = 4; k < 8; k++)
      if (board[move + delta[k]] == color) {
	apos = move + delta[k];
	break;
      }
  }

  /* And two steps away. */
  if (apos == NO_MOVE) {
    for (k = 0; k < 4; k++)
      if (board[move + 2 * delta[k]] == color) {
	apos = move + 2 * delta[k];
	break;
      }
  }
  
  /* We should have found something by now. If not something's
   * probably broken elsewhere. Declare the move unsafe if it happens.
   */
  if (apos == NO_MOVE)
    return 0;

  /* Ask the owl code whether this move is strategically viable. */
  
  save_verbose = verbose;
  if (verbose > 0)
    verbose--;
  if (!owl_does_defend(move, apos, NULL))
    return 0;
  verbose = save_verbose;
  
  return confirm_safety(move, color, defense_point, NULL);
}


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
