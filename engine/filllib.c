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

static int find_backfilling_move(int m, int n, int color, int *i, int *j);
static int filllib_confirm_safety(int m, int n, int color, int *di, int *dj);

/* Determine whether a point is adjacent to at least one own string which
 * isn't dead.
 */
static int
living_neighbor(int m, int n, int color)
{
  int k;
  for (k = 0; k < 4; k++) {
    int dm = deltai[k];
    int dn = deltaj[k];
    if (ON_BOARD(m+dm, n+dn)
	&& p[m+dm][n+dn] == color
	&& dragon[m+dm][n+dn].matcher_status != DEAD)
      return 1;
  }

  return 0;
}


/* Determine whether (m, n) effectively is a black or white point.
 * The test for inessentiality is to avoid filling the liberties
 * around a killing nakade string.
 */
static void
analyze_neighbor(int m, int n, int *found_black, int *found_white)
{
  switch (p[m][n]) {
    case EMPTY:
      if (!(*found_black)
	  && living_neighbor(m, n, BLACK)
	  && safe_move(m, n, WHITE) == 0)
	*found_black = 1;
      
      if (!(*found_white)
	  && living_neighbor(m, n, WHITE)
	  && safe_move(m, n, BLACK) == 0)
	*found_white = 1;
      
      break;

    case BLACK:
      if (!worm[m][n].inessential && DRAGON2(m, n).safety != INESSENTIAL) {
	if (dragon[m][n].matcher_status == ALIVE)
	  *found_black = 1;
	else
	  *found_white = 1;
      }
      break;

    case WHITE:
      if (!worm[m][n].inessential && DRAGON2(m, n).safety != INESSENTIAL) {
	if (dragon[m][n].matcher_status == ALIVE)
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
 * The move is returned in (*i, *j).
 */

int 
fill_liberty(int *i, int *j, int color)
{
  int m, n;
  int k;
  int other = OTHER_COLOR(color);
  int di, dj;
  int potential_color[MAX_BOARD][MAX_BOARD];

  /* We first make a fast scan for intersections which are potential
   * candidates for liberty filling. This is not very accurate, but it
   * does filter out intersections which could never pass the real
   * tests below but might still require a lot of tactical reading in
   * the process.
   */
  memset(potential_color, 0, sizeof(potential_color));
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      if (p[m][n] == EMPTY)
	continue;

      if (worm[m][n].inessential || DRAGON2(m, n).safety == INESSENTIAL)
	continue;

      if (dragon[m][n].matcher_status != ALIVE) {
	for (k = 0; k < 4; k++) {
	  int di = deltai[k];
	  int dj = deltaj[k];
	  if (ON_BOARD(m+di, n+dj) && p[m+di][n+dj] == EMPTY)
	    potential_color[m+di][n+dj] |= OTHER_COLOR(p[m][n]);
	}
      }
      
      if (dragon[m][n].matcher_status != DEAD) {
	for (k = 0; k < 12; k++) {
	  int di = deltai[k%8];
	  int dj = deltaj[k%8];
	  if (k >= 8) {
 	    if (ON_BOARD(m+di, n+dj) && p[m+di][n+dj] != EMPTY)
 	      continue;
	    di *= 2;
	    dj *= 2;
	  }
	  if (ON_BOARD(m+di, n+dj) && p[m+di][n+dj] == EMPTY)
	    potential_color[m+di][n+dj] |= p[m][n];
	}
      }
    }

  
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      /* It seems we can't trust an empty liberty to be gray-colored
       * either as a cave or as a cavity. Instead we look for empty
       * intersections with at least one neighbor of each color, where
       * dead stones count as enemy stones. We also count empty
       * neighbors to either color if the opponent can't play there.
       */
      int found_white = 0;
      int found_black = 0;

      if (p[m][n] != EMPTY)
	continue;

      /* Quick rejection based on preliminary test above. */
      if (potential_color[m][n] != GRAY)
	continue;

      /* Loop over the neighbors. */
      for (k = 0; k < 4; k++) {
	int dm = deltai[k];
	int dn = deltaj[k];
	if (ON_BOARD(m+dm, n+dn))
	  analyze_neighbor(m+dm, n+dn, &found_black, &found_white);
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

      DEBUG(DEBUG_FILLLIB, "Filllib: Considering move at %m.\n", m, n);
      
      /* Legal and tactically safe, play it if it passes
       * confirm_safety test, i.e. that it isn't a blunder which
       * causes problems for other strings.
       */
      if (safe_move(m, n, color) == WIN) {
	DEBUG(DEBUG_FILLLIB, "Filllib: Tactically safe.\n");
	if (filllib_confirm_safety(m, n, color, &di, &dj)) {
	  /* Safety confirmed. */
	  DEBUG(DEBUG_FILLLIB, "Filllib: Safety confirmed.\n");
	  *i = m;
	  *j = n;
	  return 1;
	}
	else if (di != -1) {
	  /* Safety not confirmed because the move at (m, n) would set
           * up a double threat. (di, dj) is assumed to defend against
           * this threat.
	   *
	   * FIXME: We should verify that (di, dj) really is effective.
	   */
	  DEBUG(DEBUG_FILLLIB,
		"Filllib: Safety not confirmed, but %m defends.\n", di, dj);
	  *i = di;
	  *j = dj;
	  return 1;
	}
	else {
	  /* The move causes problems somewhere else on the board, so
           * we have to discard it. If everything works right this
           * should not happen at this time.
	   */
	  DEBUG(DEBUG_FILLLIB, "Filllib: Safety not confirmed, discarded.\n");
	  TRACE("Warning: Blunder detected in fill_liberty().\n");
	  continue;
	}
      }
      
      /* Try to play the move. */
      if (trymove(m, n, color, "fill_liberty", -1, -1, EMPTY, -1, -1)) {
	popgo();
	/* Legal, but not safe. Look for backfilling move. */
	DEBUG(DEBUG_FILLLIB,
	      "Filllib: Legal but not safe, looking for backfilling move.\n");

	if (find_backfilling_move(m, n, color, i, j)) {
	  DEBUG(DEBUG_FILLLIB, "Filllib: Backfilling move at %m.\n", *i, *j);
	  /* In certain positions it may happen that an illegal move
	   * is found. This probably only can happen if we try to play
	   * a move inside a lost semeai. Anyway we should discard the
	   * move.
	   */
	  if (!is_legal(*i, *j, color)) {
	    DEBUG(DEBUG_FILLLIB, "Filllib: Was illegal, discarded.\n");
	    *i = -1;
	    *j = -1;
	    continue;
	  }

	  /* If the move turns out to be strategically unsafe, or
           * setting up a double threat elsewhere, also discard it.
	   */
	  if (!filllib_confirm_safety(*i, *j, color, &di, &dj)) {
	    DEBUG(DEBUG_FILLLIB,
		  "Filllib: Safety not confirmed, discarded.\n");
	    continue;
	  }

	  /* Seems to be ok. */
	  return 1;
	}
	else {
	  /* If we captured some stones, this move should be ok anyway. */
	  if (does_capture_something(m, n, color)) {
	    DEBUG(DEBUG_FILLLIB,
		  "Filllib: Not tactically safe, but captures stones.\n");
	    if (!filllib_confirm_safety(m, n, color, &di, &dj)) {
	      DEBUG(DEBUG_FILLLIB,
		    "Filllib: Safety not confirmed, discarded.\n");
	      continue;
	    }
	    *i = m;
	    *j = n;
	    return 1;
	  }
	}
      }
      else {
	/* Move is illegal. Look for an attack on one of the neighbor
	 * worms. If found, return that move for back-capture.
	 */
	DEBUG(DEBUG_FILLLIB, "Filllib: Illegal, looking for back-capture.\n");
	for (k = 0; k < 4; k++) {
	  int dm = deltai[k];
	  int dn = deltaj[k];
	  if (ON_BOARD(m+dm, n+dn)
	      && p[m+dm][n+dn] == other
	      && worm[m+dm][n+dn].attack_code == WIN) {
	    *i = worm[m+dm][n+dn].attacki;
	    *j = worm[m+dm][n+dn].attackj;
	    DEBUG(DEBUG_FILLLIB, "Filllib: Found at %m.\n", *i, *j);
	    return 1;
	  }
	}

	DEBUG(DEBUG_FILLLIB,
	      "Filllib: Nothing found, looking for ko back-capture.\n");
	for (k = 0; k < 4; k++) {
	  int dm = deltai[k];
	  int dn = deltaj[k];
	  if (ON_BOARD(m+dm, n+dn)
	      && p[m+dm][n+dn] == other
	      && worm[m+dm][n+dn].attack_code != 0
	      && is_legal(worm[m+dm][n+dn].attacki, worm[m+dm][n+dn].attackj,
			  color)) {
	    *i = worm[m+dm][n+dn].attacki;
	    *j = worm[m+dm][n+dn].attackj;
	    DEBUG(DEBUG_FILLLIB, "Filllib: Found at %m.\n", *i, *j);
	    return 1;
	  }
	}

	DEBUG(DEBUG_FILLLIB,
	      "Filllib: Nothing found, looking for threat to back-capture.\n");
	for (k = 0; k < 4; k++) {
	  int dm = deltai[k];
	  int dn = deltaj[k];
	  if (ON_BOARD(m+dm, n+dn)
	      && p[m+dm][n+dn] == other
	      && worm[m+dm][n+dn].attack_code != 0) {
	    /* Just pick some other liberty. */
	    int libi[2], libj[2];
	    if (findlib(m+dm, n+dn, 2, libi, libj) > 1) {
	      if (is_legal(libi[0], libj[0], color)) {
		*i = libi[0];
		*j = libj[0];
	      }
	      else if (is_legal(libi[1], libj[1], color)) {
		*i = libi[1];
		*j = libj[1];
	      }
	      DEBUG(DEBUG_FILLLIB, "Filllib: Found at %m.\n", *i, *j);
	      return 1;
	    }
	  }
	}
      }
    }
  
  /* Nothing found. */
  DEBUG(DEBUG_FILLLIB, "Filllib: No move found.\n");
  return 0;
}


/* The strategy for finding a backfilling move is to first identify
 * moves that
 *
 * 1. defends the position obtained after playing (m,n).
 * 2. captures a stone adjacent to our neighbors to (m,n), before
 *    (m,n) is played.
 *
 * Then we check which of these are legal before (m,n) is played. If
 * there is at least one, we take one of these arbitrarily as a
 * backfilling move.
 *
 * Now it may happen that (m,n) still isn't a safe move. In that case
 * we recurse to find a new backfilling move. To do things really
 * correctly we should also give the opponent the opportunity to keep
 * up the balance of the position by letting him do a backfilling move
 * of his own. Maybe this could also be arranged by recursing this
 * function. Currently we only do a half-hearted attempt to find
 * opponent moves.
 */
static int adji[MAXCHAIN];
static int adjj[MAXCHAIN];

static int libi[MAXLIBS];
static int libj[MAXLIBS];

static int
find_backfilling_move(int m, int n, int color, int *i, int *j)
{
  int k;
  int libs;
  int neighbors;
  int found_one = 0;
  int ai = -1;
  int aj = -1;
  int extra_pop = 0;
  int success = 0;
  int acode;
  int bi = -1;
  int bj = -1;
  int savei = -1;
  int savej = -1;
  
  /* Play (m,n) and identify all liberties and adjacent strings. */
  if (!trymove(m, n, color, "find_backfilling_move", m, n, EMPTY, -1, -1))
    return 0; /* This shouldn't happen, I believe. */

  /* The move wasn't safe, so there must be an attack for the
   * opponent. Save it for later use.
   */
  acode = attack(m, n, &ai, &aj);
  gg_assert(acode != 0 && ai >= 0 && aj >= 0);
  
  /* Find liberties. */
  libs = findlib(m, n, MAXLIBS, libi, libj);

  /* Find neighbors. */
  neighbors = chainlinks(m, n, adji, adjj);

  /* Remove (m,n) again. */
  popgo();
  
  /* It's most fun to capture stones. Start by trying to take some
   * neighbor off the board. If the attacking move does not directly
   * reduce the number of liberties of the attacked string we don't
   * trust it but keep it around if we don't find anything else. (See
   * filllib:17 for a position where this matters.)
   *
   * FIXME: Maybe we should take care to find the neighbor with the
   * fewest liberties, since that string probably can be removed
   * fastest. For the moment we assume this to be nonimportant.
   *
   * FIXME: It seems we have to return immediately when we find an
   * attacking move, because recursing for further backfilling might
   * lead to moves which complete the capture but cannot be played
   * before the attacking move itself. This is not ideal but probably
   * good enough.
   */
  for (k = 0; k < neighbors; k++) {
    if (attack(adji[k], adjj[k], &bi, &bj) == WIN) {
      if (liberty_of_string(bi, bj, adji[k], adjj[k])) {
	*i = bi;
	*j = bj;
	return 1;
      }
      else {
	savei = bi;
	savej = bj;
      }
    }
  }
  
  /* Otherwise look for a safe move at a liberty. */
  if (!found_one) {
    for (k = 0; k < libs; k++) {
      if (safe_move(libi[k], libj[k], color) == WIN) {
	*i = libi[k];
	*j = libj[k];
	found_one = 1;
	break;
      }
    }
  }

  /* If no luck so far, try with superstring liberties. */
  if (!found_one) {
    trymove(m, n, color, "find_backfilling_move", m, n, EMPTY, -1, -1);
    find_proper_superstring_liberties(m, n, &libs, libi, libj, 0);
    popgo();
    for (k = 0; k < libs; k++) {
      if (safe_move(libi[k], libj[k], color) == WIN) {
	*i = libi[k];
	*j = libj[k];
	found_one = 1;
	break;
      }
    }
  }


  if (found_one) {
  
    if (!trymove(*i, *j, color, "find_backfilling_move", m, n, EMPTY, -1, -1))
      return 0; /* This really shouldn't happen. */
    
    /* Allow opponent to get a move in here. */
    if (trymove(ai, aj, OTHER_COLOR(color), "find_backfilling_move", m, n, 
		EMPTY, -1, -1))
      extra_pop = 1;
    
    /* If still not safe, recurse to find a new backfilling move. */
    if (safe_move(m, n, color) == WIN)
      success = 1;
    else
      success = find_backfilling_move(m, n, color, i, j);

    /* Pop move(s) and return. */
    if (extra_pop)
      popgo();
    popgo();
  }

  if (!success && savei != -1) {
    *i = savei;
    *j = savej;
    success = 1;
  }

  return success;
}


/* Confirm that (m, n) is a safe move for color. In addition to
 * calling the global confirm_safety(), this function also calls the
 * owl code to verify the strategical viability of the move.
 */
static int
filllib_confirm_safety(int m, int n, int color, int *di, int *dj)
{
  int k;
  int ai = -1, aj = -1;
  int save_verbose;

  gg_assert(stackp == 0);
  *di = -1;
  *dj = -1;

  /* Before we can call the owl code, we need to find a neighbor of
   * our color.
   */
  for (k = 0; k < 4; k++)
    if (ON_BOARD(m + deltai[k], n + deltaj[k]) 
	&& p[m + deltai[k]][n + deltaj[k]] == color) {
      ai = m + deltai[k];
      aj = n + deltaj[k];
      break;
    }

  /* If none found, look for a neighbor of an attacked adjacent string. */
  if (ai == -1)
    for (k = 0; k < 4; k++) {
      int i = m + deltai[k];
      int j = n + deltaj[k];
      if (ON_BOARD(i, j) && p[i][j] == OTHER_COLOR(color)
	  && !play_attack_defend_n(color, 0, 1, m, n, i, j)) {
	int adj;
	adj = chainlinks(i, j, adji, adjj);
	/* It seems unlikely that we would ever get adjacent strings
         * here, but if it should happen we simply give up and say the
         * move is unsafe.
	 */
	if (adj == 0)
	  return 0;
	
	ai = adji[0];
	aj = adjj[0];
	break;
      }
    }

  /* We should have found something by now. If not something's
   * probably broken elsewhere. Declare the move unsafe if it happens.
   */
  if (ai == -1)
    return 0;

  /* Ask the owl code whether this move is strategically viable. */
  
  save_verbose = verbose;
  if (verbose > 0)
    verbose--;
  if (!owl_does_defend(m, n, ai, aj))
    return 0;
  verbose = save_verbose;
  
  return confirm_safety(m, n, color, 0, di, dj);
}


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
