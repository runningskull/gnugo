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
#include <math.h>

#include "liberty.h"
#include "gg_utils.h"
#include "random.h"
#include "move_reasons.h"


/* Test all moves which defend, attack, connect or cut to see if they
 * also attack or defend some other worm.
 *
 * FIXME: We would like to see whether an arbitrary move works to cut
 *        or connect something else too.
 *
 * FIXME: Keep track of ko results.
 */

static void
find_more_attack_and_defense_moves(int color)
{
  int unstable_worms[MAX_WORMS];
  int N = 0;  /* number of unstable worms */
  int m, n;
  int ii;
  int k;
  int other = OTHER_COLOR(color);
  int cursor_at_start_of_line;
  
  TRACE("\nLooking for additional attack and defense moves. Trying moves ...\n");
  
  /* Identify the unstable worms and store them in a list. */
  for (m = 0; m < board_size; m++)
    for (n = 0; n  <board_size; n++) {
      ii = POS(m, n);

      if (board[ii]
	  && worm[ii].origin == ii
	  && worm[ii].attack_codes[0] != 0
	  && worm[ii].defend_codes[0] != 0) {
	unstable_worms[N] = find_worm(ii);
	N++;
      }
    }
  
  /* To avoid horizon effects, we temporarily increase the depth values. */
  increase_depth_values();
  
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      ii = POS(m, n);

      for (k = 0; k < MAX_REASONS; k++) {
	int r = move[ii].reason[k];
	int what;

	if (r < 0)
	  break;
	what = move_reasons[r].what;
	if (move_reasons[r].type == ATTACK_MOVE
	    || move_reasons[r].type == ATTACK_MOVE_GOOD_KO
	    || move_reasons[r].type == ATTACK_MOVE_BAD_KO
	    || move_reasons[r].type == DEFEND_MOVE
	    || move_reasons[r].type == DEFEND_MOVE_GOOD_KO
	    || move_reasons[r].type == DEFEND_MOVE_BAD_KO
	    || move_reasons[r].type == CONNECT_MOVE
	    || move_reasons[r].type == CUT_MOVE
	    || move_reasons[r].type == ATTACK_EITHER_MOVE
	    || move_reasons[r].type == DEFEND_BOTH_MOVE)
	  break;
	/* FIXME: Add code for EITHER_MOVE here. */
      }
      
      if (k == MAX_REASONS || move[ii].reason[k] == -1)
	continue;

      /* Try the move at (ii) and see what happens. */
      cursor_at_start_of_line = 0;
      TRACE("%1m ", ii);
      if (trymove(ii, color, "find_more_attack_and_defense_moves",
		  NO_MOVE, EMPTY, NO_MOVE)) {
	for (k = 0; k < N; k++) {
	  int aa = worms[unstable_worms[k]];

	  /* string of our color, see if there still is an attack,
	   * unless we already know the move works as defense move.
	   */
	  if (board[aa] == color
	      && !defense_move_reason_known(ii, unstable_worms[k]))
	    if (!attack(aa, NULL)) {
	      if (!cursor_at_start_of_line)
		TRACE("\n");
	      TRACE("%ofound extra point of defense of %1m at %1m\n", 
		    aa, ii);
	      cursor_at_start_of_line = 1;
	      add_defense_move(ii, aa, WIN);
	    }
	    
	  /* string of opponent color, see if there still is a defense,
	   * unless we already know the move works as attack move.
	   */
	  if (board[aa] == other
	      && !attack_move_reason_known(ii, unstable_worms[k]))
	    if (!find_defense(aa, NULL)) {
	      /* Maybe find_defense() doesn't find the defense. Try to
	       * defend with the stored defense move.
	       */
	      int attack_works = 1;

	      if (trymove(worm[aa].defense_points[0], other, 
			  "find_more_attack_and_defense_moves", 0,
			  EMPTY, 0)) {
		if (!attack(aa, NULL))
		  attack_works = 0;
		popgo();
	      }
		
	      if (attack_works) {
		if (!cursor_at_start_of_line)
		  TRACE("\n");
		TRACE("%ofound extra point of attack of %1m at %1m\n",
		      aa, ii);
		cursor_at_start_of_line = 1;
		add_attack_move(ii, aa, WIN);
	      }
	    }
	}
	popgo();
      }
    }
  
  TRACE("\n");
  decrease_depth_values();
}


/* Test certain moves to see whether they (too) can owl attack or
 * defend an owl critical dragon. Tested moves are
 * 1. Strategical attacks or defenses for the dragon.
 * 2. Vital eye points for the dragon.
 * 3. Tactical attacks or defenses for a part of the dragon.
 * 4. Moves connecting the dragon to something else.
 */
static void
find_more_owl_attack_and_defense_moves(int color)
{
  int pos, pos2;
  int k;
  int s;
  int dd1, dd2;
  int dd = NO_MOVE;
  int worth_trying;
  
  TRACE("\nTrying to upgrade strategical attack and defense moves.\n");

  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (!ON_BOARD(pos))
      continue;
      
    for (k = 0; k < MAX_REASONS; k++) {
      int r = move[pos].reason[k];
      int what;
      dd1 = NO_MOVE;
      dd2 = NO_MOVE;
      
      if (r < 0)
	break;
      what = move_reasons[r].what;
      if (move_reasons[r].type == STRATEGIC_ATTACK_MOVE
	  || move_reasons[r].type == STRATEGIC_DEFEND_MOVE)
	dd1 = dragons[what];
      else if (move_reasons[r].type == ATTACK_MOVE
	       || move_reasons[r].type == ATTACK_MOVE_GOOD_KO
	       || move_reasons[r].type == ATTACK_MOVE_BAD_KO
	       || move_reasons[r].type == DEFEND_MOVE
	       || move_reasons[r].type == DEFEND_MOVE_GOOD_KO
	       || move_reasons[r].type == DEFEND_MOVE_BAD_KO)
	dd1 = worms[what];
      else if (move_reasons[r].type == VITAL_EYE_MOVE) {
	int ee = eyes[move_reasons[r].what];
	int ecolor = eyecolor[move_reasons[r].what];
	
	if (ecolor == WHITE)
	  dd1 = white_eye[ee].dragon;
	else
	  dd1 = black_eye[ee].dragon;
	
	if (dd1 == NO_MOVE) /* Maybe we should assert this not to happen. */
	  continue;
      }      
      else if (move_reasons[r].type == CONNECT_MOVE) {
	int dragon1 = conn_dragon1[move_reasons[r].what];
	int dragon2 = conn_dragon2[move_reasons[r].what];
	dd1 = dragons[dragon1];
	dd2 = dragons[dragon2];
      }
      else
	continue;
      
      for (s = 0; s < 2; s++) {
	if (s == 0)
	  dd = dd1;
	else
	  dd = dd2;
	
	if (dd == NO_MOVE)
	  continue;
	
	/* Don't care about inessential dragons. */
	if (DRAGON2(dd).safety == INESSENTIAL)
	  continue;
	
	if (dragon[dd].owl_status != CRITICAL)
	  continue;
	
	if ((move_reasons[r].type == STRATEGIC_ATTACK_MOVE 
	     || move_reasons[r].type == ATTACK_MOVE
	     || move_reasons[r].type == ATTACK_MOVE_GOOD_KO
	     || move_reasons[r].type == ATTACK_MOVE_BAD_KO
	     || (move_reasons[r].type == VITAL_EYE_MOVE
		 && board[dd] == OTHER_COLOR(color)))
	    && !owl_attack_move_reason_known(pos, find_dragon(dd))) {
	  int acode = owl_does_attack(pos, dd);
	  if (acode >= dragon[dd].owl_attack_code) {
	    add_owl_attack_move(pos, dd, acode);
	    TRACE("Move at %1m owl attacks %1m, result %d.\n", pos, dd, acode);
	  }
	}
	
	if ((move_reasons[r].type == STRATEGIC_DEFEND_MOVE
	     || move_reasons[r].type == CONNECT_MOVE
	     || move_reasons[r].type == DEFEND_MOVE
	     || move_reasons[r].type == DEFEND_MOVE_GOOD_KO
	     || move_reasons[r].type == DEFEND_MOVE_BAD_KO
	     || (move_reasons[r].type == VITAL_EYE_MOVE
		 && board[dd] == color))
	    && !owl_defense_move_reason_known(pos, find_dragon(dd))) {
	  int dcode = owl_does_defend(pos, dd);
	  if (dcode >= dragon[dd].owl_defense_code) {
	    add_owl_defense_move(pos, dd, dcode);
	    TRACE("Move at %1m owl defends %1m, result %d.\n", pos, dd, dcode);
	  }
	}
      }
    }
  }

  /* If two critical dragons are adjacent, test whether a move to owl
   * attack or defend one also is effective on the other.
   */
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (IS_STONE(board[pos])
	&& dragon[pos].origin == pos
	&& dragon[pos].owl_status == CRITICAL) {
      for (pos2 = BOARDMIN; pos2 < BOARDMAX; pos2++) {
	if (board[pos2] != EMPTY)
	  continue;
	worth_trying = 0;
	for (k = 0; k < MAX_REASONS; k++) {
	  int r = move[pos2].reason[k];
	  
	  if (r < 0)
	    break;
	  if (move_reasons[r].type == OWL_ATTACK_MOVE
	      || move_reasons[r].type == OWL_ATTACK_MOVE_GOOD_KO
	      || move_reasons[r].type == OWL_ATTACK_MOVE_BAD_KO
	      || move_reasons[r].type == OWL_DEFEND_MOVE
	      || move_reasons[r].type == OWL_DEFEND_MOVE_GOOD_KO
	      || move_reasons[r].type == OWL_DEFEND_MOVE_BAD_KO) {
	    dd = dragons[move_reasons[r].what];
	    if (are_neighbor_dragons(dd, pos)) {
	      worth_trying = 1;
	      break;
	    }
	  }
	}

	if (worth_trying) {
	  if (board[pos] == color
	      && !owl_defense_move_reason_known(pos2, find_dragon(pos))) {
	    int dcode = owl_does_defend(pos2, pos);
	    if (dcode >= dragon[pos].owl_defense_code)
	      add_owl_defense_move(pos2, pos, dcode);
	  }
	  else if (board[pos] != color
		   && !owl_attack_move_reason_known(pos2, find_dragon(pos))) {
	    int acode = owl_does_attack(pos2, pos);
	    if (acode >= dragon[pos].owl_attack_code)
	      add_owl_attack_move(pos2, pos, acode);
	  }
	}
      }
    }
  }
}


/*
 * It's often bad to run away with a worm that is in a strategically
 * weak position. This function gives heuristics for determining
 * whether a move at (ti, tj) to defend the worm (ai, aj) is
 * strategically sound.
 *
 * FIXME: This function has played out its role. Should be eliminated.
 */
static int
strategically_sound_defense(int aa, int tt)
{
  UNUSED(aa);
  return move[tt].move_safety;
}



/*
 * Any move that captures or defends a worm also connects or cuts
 * the surrounding dragons. Find these secondary move reasons.
 *
 * We also let an owl attack count as a strategical defense of our
 * neighbors of the owl attacked dragon. We only do this for
 * tactically safe dragons, however, because otherwise the effects of
 * capturing has already been taken into account elsewhere.
 *
 * FIXME: There is a certain amount of optimizations that could be
 *        done here.
 *
 * FIXME: Even when we defend a worm, it's possible that the opponent
 *        still can secure a connection, e.g. underneath a string with
 *        few liberties. Thus a defense move isn't necessarily a cut
 *        move. This problem can be solved when we have a working
 *        connection reader.
 *
 */

static void
induce_secondary_move_reasons(int color)
{
  int m;
  int n;
  int pos;
  int k;
  int i;
  int aa;
  int dd = NO_MOVE;
  int biggest;
  
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      pos = POS(m, n);

      for (k = 0; k < MAX_REASONS; k++) {
	int r = move[pos].reason[k];
	
	if (r < 0)
	  break;
	
	if (move_reasons[r].type == ATTACK_MOVE
	    || move_reasons[r].type == DEFEND_MOVE) {
	  aa = worms[move_reasons[r].what];

	  if (worm[aa].defend_codes[0] == 0)
	    continue; /* No defense. */

	  /* Don't care about inessential dragons. */
	  if (DRAGON2(aa).safety == INESSENTIAL)
	    continue;
	  
	  /*
	   * If this is a defense move and the defense is futile for
	   * strategical reasons, we shouldn't induce a cutting move
	   * reason.
	   */
	  if (move_reasons[r].type == DEFEND_MOVE
	      && !strategically_sound_defense(aa, pos))
	    continue;
	  
	  /*
	   * Find the biggest of the surrounding dragons and say that
	   * all other dragons are connected or cut with respect to that
	   * one. We might want to use some other property than size, or
	   * still better induce cuts/connections for all combinations.
	   */
	  biggest = 0;
	  
	  /* A tactically unstable worm should never be amalgamated into
	   * a larger dragon. Occasionally this does still happen and in
	   * that case we need a workaround. Eventually this workaround
	   * should become unnecessary.
	   */
	  if (dragon[aa].size == worm[aa].size) {
	    for (i = 0; i < DRAGON2(aa).neighbors; i++) {
	      int d = DRAGON2(aa).adjacent[i];
	      if (DRAGON(d).color == dragon[aa].color)
		continue;
	      
	      if (DRAGON(d).size > biggest) {
		dd = DRAGON(d).origin;
		biggest = DRAGON(d).size;
	      }
	    }
	    
	    if (biggest == 0)
	      continue;
	    
	    for (i = 0; i < DRAGON2(aa).neighbors; i++) {
	      int d = DRAGON2(aa).adjacent[i];
	      int ee = DRAGON(d).origin;
	      
	      if (DRAGON(d).color == dragon[aa].color)
		continue;
	      
	      if (dd != ee) {
		if (move_reasons[r].type == ATTACK_MOVE) {
		  /* Exclude the case when (aa) is dead and both
		   * (dd) and (ee) are strongly alive or
		   * better. Then the move would only be losing
		   * points.
		   */
		  if (dragon[aa].matcher_status != DEAD
		      || (DRAGON2(dd).safety != STRONGLY_ALIVE
			  && DRAGON2(dd).safety != INVINCIBLE)
		      || (DRAGON2(ee).safety != STRONGLY_ALIVE
			  && DRAGON2(ee).safety != INVINCIBLE)) {
		    /* If one of the strings can be attacked and the
                     * move at (pos) does not defend, do not induce a
                     * connection move.
		     */
		    if ((worm[dd].attack_codes[0] == 0
			 || does_defend(pos, dd))
			&& (worm[ee].attack_codes[0] == 0
			    || does_defend(pos, ee)))
		      add_connection_move(pos, dd, ee);
		  }
		}
		else
		  add_cut_move(pos, dd, ee);
	      }
	    }
	  }
	  else {
	    /* Workaround. If the unstable worm has been amalgamated
	     * with stable worms, it would be incorrect to add
	     * cut/connect move reasons for all neighbors of this
	     * dragon. Instead we fall back to using chainlinks() to
	     * find the neighbors of the worm. The shortcoming of this
	     * is that it only counts neighbors in direct contact with
	     * the worm, which is not always sufficient.
	     */
	    int num_adj, adjs[MAXCHAIN];
	    
	    num_adj = chainlinks(aa, adjs);
	    for (i = 0; i < num_adj; i++) {
	      int adj = adjs[i];
	      
	      if (dragon[adj].color == dragon[aa].color)
		continue;
	      if (dragon[adj].size > biggest) {
		dd = dragon[adj].origin;
		biggest = dragon[adj].size;
	      }
	    }
	    
	    if (biggest == 0)
	      continue;
	    
	    for (i = 0; i < num_adj; i++) {
	      int adj = adjs[i];
	      int ee  = dragon[adj].origin;
	      
	      if (dragon[adj].color == dragon[aa].color)
		continue;
	      
	      if (dd != ee) {
		if (move_reasons[r].type == ATTACK_MOVE) {
		  /* Exclude the case when (aa) is dead and both
		   * (dd) and (ee) are strongly alive or
		   * better. Then the move would only be losing
		   * points.
		   */
		  if (dragon[aa].matcher_status != DEAD
		      || (DRAGON2(dd).safety != STRONGLY_ALIVE
			  && DRAGON2(dd).safety != INVINCIBLE)
		      || (DRAGON2(ee).safety != STRONGLY_ALIVE
			  && DRAGON2(ee).safety != INVINCIBLE)) {
		    /* If one of the strings can be attacked and the
                     * move at (pos) does not defend, do not induce a
                     * connection move.
		     */
		    if ((worm[dd].attack_codes[0] == 0
			 || does_defend(pos, dd))
			&& (worm[ee].attack_codes[0] == 0
			    || does_defend(pos, ee)))
		      add_connection_move(pos, dd, ee);
		  }
		}
		else
		  add_cut_move(pos, dd, ee);
	      }
	    }
	  }
	}
	else if (move_reasons[r].type == OWL_ATTACK_MOVE) {
	  aa = dragons[move_reasons[r].what];
	  for (i = 0; i < DRAGON2(aa).neighbors; i++) {
	    int bb = dragon2[DRAGON2(aa).adjacent[i]].origin;
	    if (dragon[bb].color == color && worm[bb].attack_codes[0] == 0)
	      add_strategical_defense_move(pos, bb);
	  }
	}
      }
    }
}


/* Examine the strategical and tactical safety of the moves. This is
 * used to decide whether or not the stone should generate influence
 * when the move is evaluated. The idea is to avoid overestimating the
 * value of strategically unsafe defense moves and connections of dead
 * dragons. This sets the move.move_safety field.
 */
static void
examine_move_safety(int color)
{
  int i, j;
  int k;
  
  start_timer(3);
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++) {
      int pos = POS(i, j);
      int safety = 0;
      int tactical_safety = 0;
      
      for (k = 0; k < MAX_REASONS; k++) {
	int r = move[pos].reason[k];
	int type;
	int what;

	if (r == -1)
	  break;
	type = move_reasons[r].type;
	what = move_reasons[r].what;
	switch (type) {
	case CUT_MOVE:
	  /* We don't trust cut moves, unless some other move reason
           * indicates they are safe.
	   */
	  break;
	case SEMEAI_MOVE:
	case ATTACK_EITHER_MOVE:
	case DEFEND_BOTH_MOVE:    /* Maybe need better check for this case. */
	case OWL_DEFEND_MOVE:
	case OWL_DEFEND_MOVE_GOOD_KO:
	case OWL_DEFEND_MOVE_BAD_KO:
	case MY_ATARI_ATARI_MOVE:
	case EITHER_MOVE:         /* FIXME: More advanced handling? */
	  tactical_safety = 1;
	  safety = 1;
	  break;
	case BLOCK_TERRITORY_MOVE:
	case EXPAND_TERRITORY_MOVE:
	case EXPAND_MOYO_MOVE:
	  safety = 1;
	  break;
	case ATTACK_MOVE:
	case ATTACK_MOVE_GOOD_KO:
	case ATTACK_MOVE_BAD_KO:
	case OWL_ATTACK_MOVE:
	case OWL_ATTACK_MOVE_GOOD_KO:
	case OWL_ATTACK_MOVE_BAD_KO:
	  {
	    int aa = NO_MOVE;
	    int bb = NO_MOVE;
	    int size;
	    int our_color_neighbors;
	    int k;
	    
	    if (type == ATTACK_MOVE
		|| type == ATTACK_MOVE_GOOD_KO
		|| type == ATTACK_MOVE_BAD_KO) {
	      aa = worms[what];
	      size = worm[aa].effective_size;
	    }
	    else {
	      aa = dragons[what];
	      size = dragon[aa].effective_size;
	    }
	    
	    /* No worries if we catch something big. */
	    if (size >= 8) {
	      tactical_safety = 1;
	      safety = 1;
	      break;
	    }
	    
	    /* If the victim has multiple neighbor dragons of our
             * color, we leave it to the connection move reason to
             * determine safety.
	     *
	     * The exception is an owl_attack where we only require
	     * one neighbor to be alive.
	     */
	    our_color_neighbors = 0;
	    if (type == ATTACK_MOVE
		|| type == ATTACK_MOVE_GOOD_KO
		|| type == ATTACK_MOVE_BAD_KO) {
	      /* We could use the same code as for OWL_ATTACK_MOVE
               * below if we were certain that the capturable string
               * had not been amalgamated with a living dragon.
	       */
	      int num_adj, adjs[MAXCHAIN];

	      num_adj = chainlinks(aa, adjs);
	      for (k = 0; k < num_adj; k++) {
		int adj = adjs[k];

		if (board[adj] == color) {
		  /* Check whether this string is part of the same
                   * dragon as an earlier string. We only want to
                   * count distinct neighbor dragons.
		   */
		  int l;

		  for (l = 0; l < k; l++)
		    if (dragon[adjs[l]].id == dragon[adj].id)
		      break;
		  if (l == k) {
		    /* New dragon. */
		    our_color_neighbors++;
		    bb = adj;
		  }
		}
	      }
	    }
	    else {
	      for (k = 0; k < DRAGON2(aa).neighbors; k++)
		if (DRAGON(DRAGON2(aa).adjacent[k]).color == color) {
		  our_color_neighbors++;
		  bb = dragon2[DRAGON2(aa).adjacent[k]].origin;
		  if (dragon[bb].matcher_status == ALIVE) {
		    tactical_safety = 1;
		    safety = 1;
		  }
		}
	    }
	    
	    if (our_color_neighbors > 1)
	      break;
	    
	    /* It may happen in certain positions that no neighbor of
             * our color is found. The working hypothesis is that
	     * the move is safe then. One example is a position like
	     *
	     * ----+
	     * OX.X|
	     * OOX.|
	     *  OOX|
	     *   OO|
	     *
	     * where the top right stone only has friendly neighbors
	     * but can be attacked.
	     *
	     * As a further improvement, we also look for a friendly
	     * dragon adjacent to the considered move.
	     */

	    for (k = 0; k < 4; k++) {
	      int d = delta[k];
	      if (board[pos+d] == color) {
		bb = pos + d;
		break;
	      }
	    }
	    
	    if (bb == NO_MOVE) {
	      tactical_safety = 1;
	      safety = 1;
	      break;
	    }
	    
	    /* If the attacker is thought to be alive, we trust that
             * sentiment.
	     */
	    if (dragon[bb].matcher_status == ALIVE) {
	      tactical_safety = 1;
	      safety = 1;
	      break;
	    }
	    
	    /* It remains the possibility that what we have captured
             * is just a nakade shape. Ask the owl code whether this
             * move saves our attacking dragon.
	     *
	     * FIXME: Might need to involve semeai code too here.
	     */
	    if (owl_does_defend(pos, bb)) {
	      tactical_safety = 1;
	      safety = 1;
	    }
	    break;
	  }
	case DEFEND_MOVE:
	case DEFEND_MOVE_GOOD_KO:
	case DEFEND_MOVE_BAD_KO:
	  {
	    int aa = worms[what];

	    if (dragon[aa].matcher_status == ALIVE)
	      /* It would be better if this never happened, but it does
	       * sometimes. The owl reading can be very slow then.
	       */
	      safety = 1;
	    
	    else if (owl_does_defend(pos, aa))
	      safety = 1;
	    break;
	  }
	  
	case ATTACK_THREAT:
	case DEFEND_THREAT:
	  break;

	case CONNECT_MOVE:
	  {
	    int dragon1 = conn_dragon1[move_reasons[r].what];
	    int dragon2 = conn_dragon2[move_reasons[r].what];
	    int aa = dragons[dragon1];
	    int bb = dragons[dragon2];

	    if (dragon[aa].owl_status == ALIVE
		|| dragon[bb].owl_status == ALIVE) {
	      tactical_safety = 1;
	      safety = 1;
	    }
	    else if ((dragon[aa].owl_status == UNCHECKED
		      && dragon[aa].status == ALIVE)
		     || (dragon[bb].owl_status == UNCHECKED
			 && dragon[bb].status == ALIVE)) {
	      tactical_safety = 1;
	      safety = 1;
	    }
	    else if (owl_connection_defends(pos, aa, bb)) {
	      tactical_safety = 1;
	      safety = 1;
	    }
	    break;
	  }
	}
	if (safety == 1 && (tactical_safety == 1 || safe_move(pos, color)))
	  break;
      }
      
      if (safety == 1 && (tactical_safety || safe_move(pos, color)))
	move[pos].move_safety = 1;
      else
	move[pos].move_safety = 0;

      time_report(3, "    examine_move_safety: ", pos, 1.0);
    }
}


/*
 * An attempt to estimate the safety of a dragon.
 *
 * FIXME: Important to test more exactly how effective a strategical
 *        attack or defense of a weak dragon is. This can be done by
 *        measuring escape factor and moyo size after the move and
 *        compare with the old values. Also necessary to test whether
 *        an attack or defense of a critical dragon is effective.
 *        Notice that this wouldn't exactly go into this function but
 *        rather where it's called.
 */

static float safety_values[10] = {
/* DEAD        */  0.0,
/* ALIVE       */  0.9,
/* CRITICAL    */  0.1,
/* INESSENTIAL */  1.0,   /* Yes, 1.0. We simply don't worry about it. */
/* TACT. DEAD  */  0.0,
/* WEAK        */  0.4,
/* WEAK ALIVE  */  0.6,
/* SEKI        */  0.8,
/* STR. ALIVE  */  1.0,
/* INVINCIBLE  */  1.0};
		  
static float
dragon_safety(int dr, int ignore_dead_dragons)
{
  int dragon_safety = DRAGON2(dr).safety;

  /* Kludge: If a dragon is dead, we return 1.0 in order not
   * to try to run away.
   */
  if (ignore_dead_dragons
      && (dragon_safety == DEAD
	  || dragon_safety == INESSENTIAL
	  || dragon_safety == TACTICALLY_DEAD))
    return 1.0;

  /* More detailed guesses for WEAK and WEAKLY_ALIVE dragons. */
  if (dragon_safety == WEAK || dragon_safety == WEAKLY_ALIVE) {
    int escape = DRAGON2(dr).escape_route;
    int moyo = DRAGON2(dr).moyo;
    /* If escape <= 5 and moyo <= 10, the dragon won't be WEAK, since
     * the owl code has been run.
     */
    if (escape < 10 && moyo < 5)
      return 0.1;
    else if (escape < 15 && moyo < 5)
      return 0.2;
    else if (escape < 10 && moyo < 10)
      return 0.3;
    else if (escape < 5 && moyo < 15)
      return 0.4;
    else if (escape < 15 && moyo < 15)
      return 0.7;
    else
      return 0.9;
  }
  
  return safety_values[dragon_safety];
}

/*
 * Strategical value of connecting (or cutting) the dragon at (ai, aj)
 * to the dragon at (bi, bj). Notice that this function is assymetric.
 * This is because connection_value(a, b) is intended to measure the
 * strategical value on the a dragon from a connection to the b dragon.
 * 
 * Consider the following position:
 * +---------+
 * |XXO.O.OXX|
 * |.XOOOOOX.|
 * |XXXX.XXXX|
 * |.XOOXOOX.|
 * |XXO.X.O.X|
 * |OOOXXXOOO|
 * |..OOOOO..|
 * |.........|
 * +---------+
 * 
 * X has three dragons, one invincible to the left (A), one critical to
 * the right (B), and one dead in the center (C). The move at the cutting
 * point has three move reasons:
 * connect A and B
 * connect A and C
 * connect B and C
 * 
 * The strategical value on A of either connection is of course zero,
 * since it's very unconditionally alive. The strategical value on B is
 * high when it's connected to A but small (at least should be) from the
 * connection to C. Similarly for dragon C. In effect the total
 * strategical value of this move is computed as:
 * 
 * max(connection_value(A, B), connection_value(A, C))
 * + max(connection_value(B, A), connection_value(B, C))
 * + max(connection_value(C, A), connection_value(C, B))
 *
 * The parameter 'margin' is the margin by which we are ahead.
 * If this exceeds 20 points we use the cautious impact values,
 * which value connections more.  This is because we can afford
 * to waste a move making sure of safety. If the margin is between
 * 0 and 20 points we interpret linearly between the two sets of
 * impact values.
 */

/* Values higher than 1.0 to give connections a bonus over other vital
 * moves.
 */
static float impact_values[10][10] = {
/*        (bi, bj) DEAD ALIV CRIT INES TACT WEAK WE_A SEKI STRO INVI */
/* DEAD        */ {0.0, 0.9, 0.0, 0.0, 0.0, 0.8, 0.85,0.8, 0.95,1.0 },
/* ALIVE       */ {0.0, 0.08,0.05,0.0, 0.0, 0.05,0.07,0.05,0.09,0.1 },
/* CRITICAL    */ {0.0, 1.04,0.85,0.0, 0.0, 0.75,0.9, 0.85,1.08,1.1 },
/* INESSENTIAL */ {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 },
/* TACT. DEAD  */ {0.0, 0.9, 0.0, 0.0, 0.0, 0.8, 0.85,0.8, 0.95,1.0 },
/* WEAK        */ {0.1, 0.6, 0.25,0.0, 0.0, 0.2, 0.25,0.25,0.65,0.65},
/* WEAK ALIVE  */ {0.0, 0.4, 0.3, 0.0, 0.0, 0.15,0.2, 0.2 ,0.45,0.45},
/* SEKI        */ {0.0, 0.2, 0.15,0.0, 0.0, 0.1, 0.15,0.2, 0.25,0.3 },
/* STR. ALIVE  */ {0.0, 0.01,0.01,0.0, 0.0, 0.01,0.01,0.01,0.01,0.01},
/* INVINCIBLE  */ {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 }};
/* (ai, aj)    */
		  
static float cautious_impact_values[10][10] = {
/*        (bi, bj) DEAD ALIV CRIT INES TACT WEAK WE_A SEKI STRO INVI */
/* DEAD        */ {0.3, 0.9, 0.0, 0.0, 0.0, 0.8, 0.85,0.8, 0.95,1.0 },
/* ALIVE       */ {0.0, 0.2, 0.05,0.0, 0.0, 0.1,0.15, 0.10,0.2 ,0.2 },
/* CRITICAL    */ {0.0, 1.04,0.85,0.0, 0.0, 0.75,0.9, 0.85,1.08,1.1 },
/* INESSENTIAL */ {0.1, 0.6, 0.0, 0.0, 0.0, 0.3, 0.5, 0.5, 0.6, 0.6 },
/* TACT. DEAD  */ {0.2, 0.9, 0.0, 0.0, 0.0, 0.8, 0.85,0.8, 0.95,1.0 },
/* WEAK        */ {0.1, 0.6, 0.25,0.0, 0.0, 0.2, 0.25,0.25,0.65,0.65},
/* WEAK ALIVE  */ {0.0, 0.4, 0.3, 0.0, 0.0, 0.2, 0.2, 0.2 ,0.45,0.45},
/* SEKI        */ {0.0, 0.2, 0.15,0.0, 0.0, 0.1, 0.15,0.2, 0.25,0.3 },
/* STR. ALIVE  */ {0.0, 0.02,0.01,0.0, 0.0, 0.01,0.01,0.01,0.02,0.02},
/* INVINCIBLE  */ {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 }};
/* (ai, aj)    */
		  
static float
connection_value(int dragona, int dragonb, int tt, float margin)
{
  int safety1 = DRAGON2(dragona).safety;
  int safety2 = DRAGON2(dragonb).safety;
  int true_genus1 = 2 * DRAGON2(dragona).genus + DRAGON2(dragona).heyes;
  int true_genus2 = 2 * DRAGON2(dragonb).genus + DRAGON2(dragonb).heyes;
  float impact;

  /* If the connected dragon gets sufficient eyespace to live on its
   * own, although neither of the unconnected ones did, we simulate
   * this by upgrading the safety of the second dragon to ALIVE.
   */
  if (true_genus1 < 4 && true_genus2 < 4) {
    if (true_genus1 + true_genus2 >= 4
	||  (true_genus1 + true_genus2 >= 3
	     && (DRAGON2(dragona).heye == tt
		 || DRAGON2(dragonb).heye == tt)))
      safety2 = ALIVE;
  }

  /* If the b dragon is critical but has genus 0 and no moyo, we
   * assume it doesn't help dragon a to connect to b.
   */
  if (safety2 == CRITICAL && true_genus2 == 0
      && DRAGON2(dragonb).moyo == 0)
    return 0.0;
  

  /* When scoring, we want to be restrictive with reinforcement moves
   * inside own territory. Thus if both dragons are strongly alive or
   * invincible, or if one is and the other is alive, no bonus is
   * awarded.
   *
   * Notice that this requires that the territorial value is computed
   * before the strategical value.
   */
  if (doing_scoring && move[tt].territorial_value < 0.0) {
    if (safety1 == ALIVE
	&& (safety2 == STRONGLY_ALIVE || safety2 == INVINCIBLE))
      return 0.0;
    
    if ((safety1 == STRONGLY_ALIVE || safety1 == INVINCIBLE)
	&& (safety2 == ALIVE || safety2 == STRONGLY_ALIVE
	    || safety2 == INVINCIBLE))
      return 0.0;
  }

  if (doing_scoring || margin < 0.0)
    impact = impact_values[safety1][safety2];
  else if (margin > 20.0)
    impact = cautious_impact_values[safety1][safety2];
  else impact = 0.05*margin*cautious_impact_values[safety1][safety2]
	 + (1-0.05*margin)*impact_values[safety1][safety2];


  /* Trying to connect an inessential string to something else with a
   * self atari is almost certainly worthless.
   */
  if (impact > 0.0
      && safety1 == INESSENTIAL
      && is_self_atari(tt, board[dragona]))
    impact = 0.0;
  
  return impact * 2.0 * dragon[dragona].effective_size;
}


/*
 * Usually the value of attacking a worm is twice its effective size,
 * but when evaluating certain move reasons we need to adjust this to
 * take effects on neighbors into account, e.g. for an attack_either
 * move reason. This does not apply to the attack and defense move
 * reasons, however, because then the neighbors already have separate
 * attack or defense move reasons (if such apply).
 *
 * If the worm has an adjacent (friendly) dead dragon we add its
 * value. At least one of the surrounding dragons must be alive. 
 * If not, the worm must produce an eye of sufficient size, and that 
 * should't be accounted for here.  As a guess, we suppose that
 * a critical dragon is alive for our purpose here.
 *
 * On the other hand if it has an adjacent critical worm, and
 * if (pos) does not defend that worm, we subtract the value of the
 * worm, since (pos) may be defended by attacking that worm. We make at
 * most one adjustment of each type.
 */

static float
adjusted_worm_attack_value(int pos, int ww)
{
  int color;
  int num_adj;
  int adjs[MAXCHAIN];
  int has_live_neighbor = 0;
  float adjusted_value = 2 * worm[ww].effective_size;
  float adjustment_up = 0.0;
  float adjustment_down = 0.0;
  int s;

  color = OTHER_COLOR(board[ww]);
  num_adj = chainlinks(ww, adjs);
  for (s = 0; s < num_adj; s++) {
    int adj = adjs[s];

    if (dragon[adj].matcher_status != DEAD)
      has_live_neighbor = 1;

    if (dragon[adj].matcher_status == DEAD
	&& 2*dragon[adj].effective_size > adjustment_up)
      adjustment_up = 2*dragon[adj].effective_size;

    if (worm[adj].attack_codes[0] != 0
	&& !does_defend(pos, ww)
	&& 2*worm[adj].effective_size > adjustment_down)
      adjustment_down = 2*worm[adj].effective_size;
  }

  if (has_live_neighbor)
    adjusted_value += adjustment_up;
  adjusted_value -= adjustment_down;

  return adjusted_value;
}


/*
 * Estimate the direct territorial value of a move at (pos).
 */
static void
estimate_territorial_value(int pos, int color, float score)
{
  int k;
  int aa = NO_MOVE;
  
  float this_value = 0.0;
  float tot_value = 0.0;
  float secondary_value = 0.0;

  int does_block = 0;
  char saved_stones[BOARDMAX];

  memset(saved_stones, 0, BOARDMAX);
  
  for (k = 0; k < MAX_REASONS; k++) {
    int r = move[pos].reason[k];
    if (r < 0)
      break;
    if (move_reasons[r].status & TERRITORY_REDUNDANT)
      continue;

    this_value = 0.0;
    switch (move_reasons[r].type) {
    case ATTACK_MOVE:
    case ATTACK_MOVE_GOOD_KO:
    case ATTACK_MOVE_BAD_KO:
      aa = worms[move_reasons[r].what];
      
      gg_assert(board[aa] != color);
      
      /* Defenseless stone. */
      if (worm[aa].defend_codes[0] == 0) {
	DEBUG(DEBUG_MOVE_REASONS,
	      "    %1m: %f (secondary) - attack on %1m (defenseless)\n",
	      pos, worm[aa].effective_size, aa);
	secondary_value += worm[aa].effective_size;
	break;
      }

      /* Strategically unsafe move. */
      if (!move[pos].move_safety) {
	DEBUG(DEBUG_MOVE_REASONS,
	      "    %1m: 0.0 - attack on %1m (unsafe move)\n", pos, aa);
	break;
      }

      this_value = 2 * worm[aa].effective_size;

      /* If the stones are dead, there is only a secondary value in
       * capturing them tactically as well.
       */
      if (dragon[aa].matcher_status == DEAD) {
	DEBUG(DEBUG_MOVE_REASONS,
	      "    %1m: %f (secondary) - attack on %1m (dead)\n",
	      pos, 0.2 * this_value, aa);
	secondary_value += 0.2 * this_value;
	break;
      }

      /* Mark the string as captured, for evaluation in the influence code. */
      mark_string(aa, saved_stones, INFLUENCE_CAPTURED_STONE);
      TRACE("  %1m: attack on worm %1m\n", pos, aa);
      
      /* FIXME: How much should we reduce the value for ko attacks? */
      if (move_reasons[r].type == ATTACK_MOVE)
	this_value = 0.0;
      else if (move_reasons[r].type == ATTACK_MOVE_GOOD_KO) {
	this_value *= 0.3;
	TRACE("  %1m: -%f - attack on worm %1m only with good ko\n",
	      pos, this_value, aa);
      }	
      else if (move_reasons[r].type == ATTACK_MOVE_BAD_KO) {
	this_value *= 0.5;
	TRACE("  %1m: -%f - attack on worm %1m only with bad ko\n",
	      pos, this_value, aa);
      }
      
      tot_value -= this_value;
      does_block = 1;
      break;
      
    case DEFEND_MOVE:
    case DEFEND_MOVE_GOOD_KO:
    case DEFEND_MOVE_BAD_KO:
      aa = worms[move_reasons[r].what];
      
      gg_assert(board[aa] == color);
      
      /* 
       * Estimate value 
       */
      if (!strategically_sound_defense(aa, pos)) {
	DEBUG(DEBUG_MOVE_REASONS,
	      "    %1m: 0.0 - defense of %1m (strategically unsound defense)\n",
	      pos, aa);
	break;
      }	

      this_value = 2 * worm[aa].effective_size;

      /* If the stones are dead, we use the convention that
       * defending them has a strategical value rather than
       * territorial. Admittedly this make more sense for attacks on
       * dead stones.
       */
      if (dragon[aa].matcher_status == DEAD) {
	DEBUG(DEBUG_MOVE_REASONS,
	      "    %1m: %f (secondary) - defense of %1m (dead)\n",
	      pos, 0.2 * this_value, aa);
	secondary_value += 0.2 * this_value;
	break;
      }

      /* Mark the string as saved, for evaluation in the influence code. */
      mark_string(aa, saved_stones, INFLUENCE_SAVED_STONE);
      TRACE("  %1m: defense of worm %1m\n", pos, aa);
      
      /* FIXME: How much should we reduce the value for ko defenses? */
      if (move_reasons[r].type == DEFEND_MOVE)
	this_value = 0.0;
      else if (move_reasons[r].type == DEFEND_MOVE_GOOD_KO) {
	this_value *= 0.3;
	TRACE("  %1m: -%f - defense of worm %1m with good ko\n",
	      pos, this_value, aa);
      }	
      else if (move_reasons[r].type == DEFEND_MOVE_BAD_KO) {
	this_value *= 0.5;
	TRACE("  %1m: -%f - defense of worm %1m with bad ko\n",
	      pos, this_value, aa);
      }	
    
      tot_value -= this_value;
      does_block = 1;
      break;

    case ATTACK_THREAT:
      aa = worms[move_reasons[r].what];

      /* Threat on our stones. */
      if (board[aa] == color)
	break;
      
      if (dragon[aa].matcher_status == DEAD) {
	DEBUG(DEBUG_MOVE_REASONS,
	      "    %1m: 0.0 - threatens to capture %1m (dead)\n", pos, aa);
	break;
      }

      /* The followup value of a move threatening to attack (aa)
       * is twice its effective size, with adjustments. If the
       * worm has an adjacent (friendly) dead dragon we add its
       * value. On the other hand if it has an adjacent critical
       * worm, and if (pos) does not defend that worm, we subtract
       * the value of the worm, since (aa) may be defended by
       * attacking that worm. We make at most one adjustment
       * of each type.
       *
       * No followup value is awarded if the defense move is a threat
       * back on our move because we're likely to end in gote then.
       *
       * FIXME: It might be possible that parts of the dragon
       *        can be cut in the process of capturing the (aa)
       *        worm. In that case, not the entire size of the 
       *        adjacent dead dragon should be counted as a positive
       *        adjustment.  However, it seems difficult to do this
       *        analysis, and in most cases it won't apply, so we
       *        leave it as it is for now.
       *
       * FIXME: The same analysis should be applied to
       *        DEFEND_THREAT,
       *        ATTACK_EITHER_MOVE, DEFEND_BOTH_MOVE. It should be 
       *        broken out as separate functions and dealt with in
       *        a structured manner.
       */

      if (trymove(pos, color, "estimate_territorial_value-A",
		  NO_MOVE, EMPTY, NO_MOVE)) {
	int adjs[MAXCHAIN];
	float adjusted_value = 2 * worm[aa].effective_size;
	float adjustment_up = 0.0;
	float adjustment_down = 0.0;
	int s;
	int num_adj;
	int defense_move;

	/* In rare cases it may happen that the trymove() above
         * actually removed the string at aa.
	 */
	if (board[aa] == EMPTY)
	  num_adj = 0;
	else
	  num_adj = chainlinks(aa, adjs);

	/* No followup value if string can be defended with threat
         * against our move.
	 *
	 * FIXME: This is somewhat halfhearted since only one defense
	 * move is tested.
	 */
	if (board[aa] != EMPTY
	    && find_defense(aa, &defense_move) != 0
	    && defense_move != NO_MOVE) {
	  if (trymove(defense_move, OTHER_COLOR(color),
		      "estimate_territorial_value-b", NO_MOVE,
		      EMPTY, NO_MOVE)) {
	    if (board[pos] == EMPTY || attack(pos, NULL) != 0) {
	      popgo();
	      popgo();
	      break;
	    }
	    popgo();
	  }
	}
	
	for (s = 0; s < num_adj; s++) {
	  int adj = adjs[s];

	  if (same_string(pos, adj))
	    continue;
	  if (dragon[adj].color == color
	      && dragon[adj].matcher_status == DEAD
	      && 2*dragon[adj].effective_size > adjustment_up)
	    adjustment_up = 2*dragon[adj].effective_size;
	  if (dragon[adj].color == color
	      && attack(adj, NULL)
	      && 2*worm[adj].effective_size > adjustment_down)
	    adjustment_down = 2*worm[adj].effective_size;
	}
	adjusted_value += adjustment_up;
	adjusted_value -= adjustment_down;
	if (adjusted_value > 0.0) {
	  add_followup_value(pos, adjusted_value);
	  /* Inside trymove, so don't re-indented.*/
	  TRACE("%1m:   %f (followup) - threatens to capture %1m\n",
		pos, adjusted_value, aa);
	}
	popgo();
      }
      break;

    case DEFEND_THREAT:
      aa = worms[move_reasons[r].what];

      /* Threat on our stones. */
      if (board[aa] == color)
	break;
      
      if (dragon[aa].matcher_status == DEAD) {
	DEBUG(DEBUG_MOVE_REASONS,
	      "    %1m: 0.0 - threatens to defend %1m (dead)\n", pos, aa);
	break;
      }

      add_followup_value(pos, 2 * worm[aa].effective_size);

      TRACE("  %1m: %f (followup) - threatens to defend %1m\n",
	    pos, 2 * worm[aa].effective_size, aa);

      break;

    case UNCERTAIN_OWL_DEFENSE:
      /* This move reason is valued as a strategical value. */
      break;
      
    case CONNECT_MOVE:
    case CUT_MOVE:
    case STRATEGIC_ATTACK_MOVE:
    case STRATEGIC_DEFEND_MOVE:
    case BLOCK_TERRITORY_MOVE:
      does_block = 1;
      break;
      
    case EXPAND_MOYO_MOVE:
    case EXPAND_TERRITORY_MOVE:
      /* We don't make any difference between blocking and expanding
       * territory.
       *
       * FIXME: Fuse the BLOCK_TERRITORY and EXPAND_TERRITORY move
       * reasons to one and do the same with the b and e class
       * patterns.
       */
      does_block = 1;
      break;
      
    case SEMEAI_MOVE:
      aa = dragons[move_reasons[r].what];
      
      this_value = 2 * dragon[aa].effective_size;
      TRACE("  %1m: %f - semeai involving %1m\n", pos, this_value, aa);
      tot_value += this_value;
      break;
      
    case SEMEAI_THREAT:
      aa = dragons[move_reasons[r].what];

      /* threaten to win the semeai as a ko threat */
      add_followup_value(pos, 2 * dragon[aa].effective_size);
      TRACE("  %1m: %f (followup) - threatens to win semeai for %1m\n",
	    pos, 2 * dragon[aa].effective_size, aa);
      break;
      
    case VITAL_EYE_MOVE:
      /* These are upgraded to owl attacks or defenses in
       * find_more_owl_attack_and_defense_moves() and should no longer
       * be counted here.
       */
      break;
	
    case OWL_ATTACK_MOVE:
    case OWL_ATTACK_MOVE_GOOD_KO:
    case OWL_ATTACK_MOVE_BAD_KO:
    case OWL_DEFEND_MOVE:
    case OWL_DEFEND_MOVE_GOOD_KO:
    case OWL_DEFEND_MOVE_BAD_KO:
      aa = dragons[move_reasons[r].what];

      /* If the dragon is a single ko stone, the owl code currently
       * won't detect that the owl attack is conditional. As a
       * workaround we deduct 0.5 points for the move here.
       */
      if (dragon[aa].size == 1
	  && is_ko_point(aa)) {
	TRACE("  %1m: -0.5 - penalty for ko stone %1m (workaround)\n",
	      pos, aa);
	tot_value -= 0.5;
	break;
      }

      {
	int ii;
	for (ii = BOARDMIN; ii < BOARDMAX; ii++) {
	  if (IS_STONE(board[ii]) && is_same_dragon(ii, aa)) {
	    if (move_reasons[r].type == OWL_ATTACK_MOVE
		|| move_reasons[r].type == OWL_ATTACK_MOVE_GOOD_KO
		|| move_reasons[r].type == OWL_ATTACK_MOVE_BAD_KO)
	      saved_stones[ii] = INFLUENCE_CAPTURED_STONE;
	    else
	      saved_stones[ii] = INFLUENCE_SAVED_STONE;
	  }
	}
      }
      TRACE("  %1m: owl attack/defend for %1m\n", pos, aa);
      
      /* FIXME: How much should we reduce the value for ko attacks? */
      this_value = 2 * dragon[aa].effective_size;
      if (move_reasons[r].type == OWL_ATTACK_MOVE
	  || move_reasons[r].type == OWL_DEFEND_MOVE)
	this_value = 0.0;
      else if (move_reasons[r].type == OWL_ATTACK_MOVE_GOOD_KO
	       || move_reasons[r].type == OWL_DEFEND_MOVE_GOOD_KO) {
	this_value *= 0.3;
	TRACE("  %1m: -%f - owl attack/defense of %1m only with good ko\n",
	      pos, this_value, aa);
      }	
      else if (move_reasons[r].type == OWL_ATTACK_MOVE_BAD_KO
	       || move_reasons[r].type == OWL_DEFEND_MOVE_BAD_KO) {
	this_value *= 0.5;
	TRACE("  %1m: -%f - owl attack/defense of %1m only with bad ko\n",
	      pos, this_value, aa);
      }
      
      tot_value -= this_value;
      does_block = 1;
      break;

    case OWL_ATTACK_THREAT:
      aa = dragons[move_reasons[r].what];

      if (dragon[aa].matcher_status == DEAD) {
	DEBUG(DEBUG_MOVE_REASONS,
	      "    %1m: 0.0 - threatens to owl attack %1m (dead)\n", pos, aa);
	break;
      }

      /* The followup value of a move threatening to attack (aa) is
       * twice its effective size, unless it has an adjacent
       * (friendly) critical dragon. In that case it's probably a
       * mistake to make the threat since it can defend itself with
       * profit.
       *
       * FIXME: We probably need to verify that the critical dragon is
       * substantial enough that capturing it saves the threatened
       * dragon.
       */	 
      {
	float value = 2 * dragon[aa].effective_size;
	int s;

	for (s = 0; s < DRAGON2(aa).neighbors; s++) {
	  int d = DRAGON2(aa).adjacent[s];
	  int adj = dragon2[d].origin;

	  if (dragon[adj].color == color
	      && dragon[adj].matcher_status == CRITICAL
	      && dragon2[d].safety != INESSENTIAL
	      && !owl_defense_move_reason_known(pos, find_dragon(adj)))
	    value = 0.0;
	}
	
	if (value > 0.0) {
	  add_followup_value(pos, value);
	  TRACE("  %1m: %f (followup) - threatens to owl attack %1m\n",
		pos, value, aa);
	}
      }
      break;

    case OWL_DEFEND_THREAT:
      aa = dragons[move_reasons[r].what];

      add_followup_value(pos, 2 * dragon[aa].effective_size);
      TRACE("  %1m: %f (followup) - threatens to owl defend %1m\n",
	    pos, 2 * dragon[aa].effective_size, aa);
      break;

    case OWL_PREVENT_THREAT:
      /* A move attacking a dragon whose defense can be threatened.
       */
      aa = dragons[move_reasons[r].what];

      /* If the opponent just added a stone to a dead dragon, then
       * attack it. If we are ahead, add a safety move here, at most
       * half the margin of victory.
       *
       * This does not apply if we are doing scoring.
       */
      if (!doing_scoring
	  && is_same_dragon(last_moves[0], aa)) {
	this_value = 1.5 * dragon[aa].effective_size;
	TRACE("  %1m: %f - attack last move played, although it seems dead\n",
	      pos, this_value);
	tot_value += this_value;
      }
      else if (!doing_scoring && ((color == BLACK && score < 0.0)
				  || (color == WHITE && score > 0.0))) {
	/* tm - devalued this bonue (3.1.17) */
	this_value = gg_min(0.9 * dragon[aa].effective_size,
			    gg_abs(score/2) - board_size/2 - 1);
	this_value = gg_max(this_value, 0);
	TRACE("  %1m: %f - attack %1m, although it seems dead, as we are ahead\n",
	      pos, this_value, aa);
	tot_value += this_value;
      }
      else {
	/* FIXME: Why are we computing a this_value here when it's
         * never used?
	 */
	if ((color == BLACK && score > 0.0)
	    || (color == WHITE && score < 0.0))
	  this_value = 0.0;
	else 
	  this_value = gg_min(2*dragon[aa].effective_size, gg_abs(score/2));
	
	add_reverse_followup_value(pos, 2 * dragon[aa].effective_size);
	if (board[aa] == color)
	  TRACE("  %1m: %f (reverse followup) - prevent threat to attack %1m\n",
		pos, 2 * dragon[aa].effective_size, aa);
	else 
	  TRACE("  %1m: %f (reverse followup) - prevent threat to defend %1m\n",
		pos, 2 * dragon[aa].effective_size, aa);
      }
      break;
      
    case MY_ATARI_ATARI_MOVE:
      this_value = 2 * move_reasons[r].what + 3.0;
      if (influence_territory_color(pos) == OTHER_COLOR(color))
	does_block = 1;
      tot_value += this_value;
      TRACE("  %1m: %f - combination attack kills one of several worms\n",
	    pos, this_value);
      break;
      
    case YOUR_ATARI_ATARI_MOVE:
      this_value = 2 * move_reasons[r].what + 3.0;
      if (influence_territory_color(pos) == color
	  && !is_self_atari(pos, OTHER_COLOR(color)))
	this_value += 7.0;
      tot_value += this_value;
      TRACE("  %1m: %f - defends against combination attack on several worms\n",
	    pos, this_value);
      break;
    }
  }

  /* Currently no difference in the valuation between blocking and
   * expanding moves.
   */
  this_value = 0.0;

  if (move[pos].move_safety == 1 && safe_move(pos, color) == WIN)
    saved_stones[pos] = INFLUENCE_SAVED_STONE;
  else
    saved_stones[pos] = INFLUENCE_CAPTURED_STONE;
  
  if (does_block) {
    this_value = influence_delta_territory(pos, color, saved_stones);
    if (this_value != 0.0)
      TRACE("  %1m: %f - change in territory\n", pos, this_value);
    else
      DEBUG(DEBUG_MOVE_REASONS, "    %1m: 0.0 - block or expand territory\n", 
	    pos);
  }

  tot_value += this_value;
  
  /* Test if min_territory or max_territory values constrain the
   * delta_territory value.
   */
  if (tot_value < move[pos].min_territory
      && move[pos].min_territory > 0) {
    tot_value = move[pos].min_territory;
    TRACE("  %1m:   %f - revised to meet minimum territory value\n", 
	  pos, tot_value);
  }
  if (tot_value > move[pos].max_territory) {
    tot_value = move[pos].max_territory;
    TRACE("  %1m:   %f - revised to meet maximum territory value\n",
	  pos, tot_value);
  }
    
  /* subtract one point for a sacrifice (playing in opponent's territory) */
  if (tot_value > 1.0 && safe_move(pos, color) != WIN) {
    TRACE("  %1m:   -1 - unsafe move, assumed sacrifice\n", pos);
    tot_value -= 1.0;
  }

  move[pos].territorial_value = tot_value;
  move[pos].secondary_value  += secondary_value;
}


/*
 * Estimate the strategical value of a move at (pos).
 */
static void
estimate_strategical_value(int pos, int color, float score)
{
  int k;
  int l;
  int aa = NO_MOVE;
  int bb = NO_MOVE;
  float aa_value = 0.0;
  float bb_value = 0.0;
  int d1 = -1;
  int d2 = -1;
  int worm1 = -1;
  int worm2 = -1;
  int ecolor = 0;
  
  float this_value = 0.0;
  float tot_value = 0.0;

  /* Strategical value of connecting or cutting dragons. */
  static float dragon_value[MAX_DRAGONS];

  for (k = 0; k < next_dragon; k++)
    dragon_value[k] = 0.0;
  
  for (k = 0; k < MAX_REASONS; k++) {
    int r = move[pos].reason[k];
    if (r < 0)
      break;
    if (move_reasons[r].status & STRATEGICALLY_REDUNDANT)
      continue;
    
    this_value = 0.0;
    switch (move_reasons[r].type) {
      case ATTACK_MOVE:
      case ATTACK_MOVE_GOOD_KO:
      case ATTACK_MOVE_BAD_KO:
      case DEFEND_MOVE:
      case DEFEND_MOVE_GOOD_KO:
      case DEFEND_MOVE_BAD_KO:
	worm1 = move_reasons[r].what;
	aa = worms[worm1];
      
	/* Defenseless stone */
	if (worm[aa].defend_codes[0] == 0)
	  break;

	/* Require the defense to be strategically viable. */
	if ((move_reasons[r].type == DEFEND_MOVE
	     || move_reasons[r].type == DEFEND_MOVE_GOOD_KO
	     || move_reasons[r].type == DEFEND_MOVE_BAD_KO)
	    && !strategically_sound_defense(aa, pos))
	  break;

	/* Do the same for attack moves. */
	if ((move_reasons[r].type == ATTACK_MOVE
	     || move_reasons[r].type == ATTACK_MOVE_GOOD_KO
	     || move_reasons[r].type == ATTACK_MOVE_BAD_KO)
	    && !move[pos].move_safety)
	  break;
	
	/* FIXME: This is totally ad hoc, just guessing the value of
         *        potential cutting points.
	 * FIXME: When worm[aa].cutstone2 == 1 we should probably add
	 *        a followup value.
	 */
	if (worm[aa].cutstone2 > 1) {
	  this_value = 10.0 * (worm[aa].cutstone2 - 1);
	  TRACE("  %1m: %f - %1m cutstone\n", pos, this_value, aa);
	}
	
	tot_value += this_value;
	
	/* If the string is a lunch for a weak dragon, the attack or
         * defense has a strategical value. This can be valued along
	 * the same lines as strategic_attack/strategic_defend.
	 *
	 * No points are awarded if the lunch is an inessential dragon
	 * or worm.
	 */
	if (DRAGON2(aa).safety == INESSENTIAL
	    || worm[aa].inessential)
	  break;

	/* Can't use k in this loop too. */
	for (l = 0; l < next_lunch; l++)
	  if (lunch_worm[l] == worm1) {
	    d1 = lunch_dragon[l];
	    bb = dragons[d1];

	    /* FIXME: This value cannot be computed without some
	     * measurement of how the actual move affects the dragon.
	     * The dragon safety alone is not enough. The question is
	     * whether the dragon is threatened by the move or not.
	     */
	    this_value = (dragon[bb].effective_size
			  * (1.0 - dragon_safety(bb, 0)));

	    /* If this dragon consists of only one worm and that worm
	     * can be tactically captured or defended by this move, we
	     * have already counted the points as territorial value,
	     * unless it's assumed to be dead.
	     */
	    if (dragon[bb].matcher_status != DEAD
		&& dragon[bb].size == worm[bb].size
		&& (attack_move_reason_known(pos, find_worm(bb))
		    || defense_move_reason_known(pos, find_worm(bb))))
	      this_value = 0.0;

	    /* If this dragon can be tactically attacked and the move
             * does not defend, no points.
	     */
	    if (worm[bb].attack_codes[0] != 0 && !does_defend(pos, bb))
	      this_value = 0.0;
	    
	    if (this_value > dragon_value[d1]) {
	      DEBUG(DEBUG_MOVE_REASONS, "  %1m:   %f - %1m attacked/defended\n",
		    pos, this_value, bb);
	      dragon_value[d1] = this_value;
	  }
	  }

	break;
	
      case ATTACK_THREAT:
      case DEFEND_THREAT:
        break;

      case EITHER_MOVE:
	/* FIXME: Generalize this to more types of threats. */
	worm1 = either_data[move_reasons[r].what].what1;
	worm2 = either_data[move_reasons[r].what].what2;
	aa = worms[worm1];
	bb = worms[worm2];

	/* If both worms are dead, this move reason has no value. */
	if (dragon[aa].matcher_status == DEAD 
	    && dragon[bb].matcher_status == DEAD)
	  break;

	/* Also if there is a combination attack, we assume it covers
	 * the same thing.
	 */
	if (move_reason_known(pos, MY_ATARI_ATARI_MOVE, -1))
	  break;

	aa_value = adjusted_worm_attack_value(pos, aa);
	bb_value = adjusted_worm_attack_value(pos, bb);
	this_value = gg_min(aa_value, bb_value);

	TRACE("  %1m: %f - either attacks %1m (%f) or attacks %1m (%f)\n",
	      pos, this_value, aa, aa_value, bb, bb_value);

	tot_value += this_value;
	break;
	
      case ATTACK_EITHER_MOVE:
      case DEFEND_BOTH_MOVE:
	/* This is complete nonsense, but still better than nothing.
	 * FIXME: Do this in a reasonable way.
	 */
	worm1 = worm_pair1[move_reasons[r].what];
	worm2 = worm_pair2[move_reasons[r].what];
	aa = worms[worm1];
	bb = worms[worm2];

	/* If both worms are dead, this move reason has no value. */
	if (dragon[aa].matcher_status == DEAD 
	    && dragon[bb].matcher_status == DEAD)
	  break;

	/* Also if there is a combination attack, we assume it covers
         * the same thing.
	 */
	if (move_reasons[r].type == ATTACK_EITHER_MOVE
	    && move_reason_known(pos, MY_ATARI_ATARI_MOVE, -1))
	  break;
	if (move_reasons[r].type == DEFEND_BOTH_MOVE
	    && move_reason_known(pos, YOUR_ATARI_ATARI_MOVE, -1))
	  break;

	if (move_reasons[r].type == ATTACK_EITHER_MOVE) {
	  aa_value = adjusted_worm_attack_value(pos, aa);
	  bb_value = adjusted_worm_attack_value(pos, bb);
	  this_value = gg_min(aa_value, bb_value);

	  TRACE("  %1m: %f - attacks either %1m (%f) or %1m (%f)\n",
		pos, this_value, aa, aa_value, bb, bb_value);
	}
	else {
	  this_value = 2 * gg_min(worm[aa].effective_size,
				  worm[bb].effective_size);

	  TRACE("  %1m: %f - defends both %1m and %1m\n",
		pos, this_value, aa, bb);
	}

	tot_value += this_value;
	break;
	
      case CONNECT_MOVE:
	if (!move[pos].move_safety)
	  break;
	/* Otherwise fall through. */
      case CUT_MOVE:
	if (doing_scoring && !move[pos].move_safety)
	  break;

	d1 = conn_dragon1[move_reasons[r].what];
	d2 = conn_dragon2[move_reasons[r].what];
	aa = dragons[d1];
	bb = dragons[d2];

	/* If we are ahead by more than 20, value connections more strongly */
	if ((color == WHITE && score > 20.0)
	    || (color == BLACK && score < -20.0))
	  this_value = connection_value(aa, bb, pos, gg_abs(score));
	else
	  this_value = connection_value(aa, bb, pos, 0);
	if (this_value > dragon_value[d1]) {
	  dragon_value[d1] = this_value;
          DEBUG(DEBUG_MOVE_REASONS, "  %1m:   %f - %1m cut/connect strategic value\n",
		    pos, this_value, aa);
	}

	
	if ((color == WHITE && score > 20.0)
	    || (color == BLACK && score < -20.0))
	  this_value = connection_value(bb, aa, pos, gg_abs(score));
	else
	  this_value = connection_value(bb, aa, pos, 0);
	if (this_value > dragon_value[d2]) {
	  dragon_value[d2] = this_value;
          DEBUG(DEBUG_MOVE_REASONS, "  %1m:   %f - %1m cut/connect strategic value\n",
		    pos, this_value, bb);
	}
	
	break;
	
      case SEMEAI_MOVE:
	/*
	 * The strategical value of winning a semeai is
	 * own dragons (usually) becomes fully secure, while adjoining
	 * enemy dragons do not.
	 *
	 * FIXME: Valuation not implemented at all yet. 
	 */

	break;
	
      case VITAL_EYE_MOVE:
	/*
	 * The value of the threatened group itself has already been
	 * accounted for in territorial_value. Now we need to determine
	 * the effect this has on surrounding groups.
	 *
	 * FIXME: Valuation not implemented.
	 */
	aa = eyes[move_reasons[r].what];
	ecolor = eyecolor[move_reasons[r].what];

	if (ecolor == WHITE) 
	  bb = white_eye[aa].dragon;
	else
	  bb = black_eye[aa].dragon;

	if (bb == NO_MOVE) /* Maybe we should assert this not to happen. */
	  break; 

	/* If there is an owl attack/defend move reason for this location,
	 * we don't care about it, since otherwise we would count the
	 * points twice.
	 */
	if (owl_defense_move_reason_known(pos, find_dragon(bb))
	    || owl_attack_move_reason_known(pos, find_dragon(bb))) {
	  DEBUG(DEBUG_MOVE_REASONS,
		"    %1m: 0.0 - vital for %1m: owl attack/defense as well\n",
		pos, bb);
	  break;
	}

#if 0
       if (dragon[bb].matcher_status == CRITICAL) {
	  this_value = ???
         TRACE("  %1m: %f - vital for %1m\n",
               pos, this_value, bb);
	  tot_value += this_value;
	}
#endif
	break;

      case STRATEGIC_ATTACK_MOVE:
      case STRATEGIC_DEFEND_MOVE:	
	/* The right way to do this is to estimate the safety of the
	 * dragon before and after the move. Unfortunately we are
	 * missing good ways to do this currently.
	 *
	 * Temporary solution is to only look at an ad hoc measure of
	 * the dragon safety and ignoring the effectiveness of the
	 * move.
	 *
	 * FIXME: Improve the implementation.
	 */
	d1 = move_reasons[r].what;
	aa = dragons[d1];

	/* FIXME: This value cannot be computed without some
	 * measurement of how the actual move affects the dragon. The
	 * dragon safety alone is not enough. The question is whether
	 * the dragon is threatened by the move or not.
	 */
	this_value = (dragon[aa].effective_size
		      * (1.0 - dragon_safety(aa, 1)));

	/* To prefer good connections and cuts, we lower this value
	 * somewhat.
	 */
	this_value *= 0.75;

	/* No strategical attack value is awarded if the dragon at (aa)
	 * has an adjacent (friendly) critical dragon, which is not
	 * defended by this move. In that case it's probably a mistake
	 * to make the strategical attack since the dragon can defend
	 * itself with profit.
	 *
	 * FIXME: We probably need to verify that the critical dragon is
	 * substantial enough that capturing it saves the strategically
	 * attacked dragon.
	 */
	if (move_reasons[r].type == STRATEGIC_ATTACK_MOVE) {
	  int s;
	  
	  for (s = 0; s < DRAGON2(aa).neighbors; s++) {
	    int d = DRAGON2(aa).adjacent[s];
	    int adj = dragon2[d].origin;
	    
	    if (dragon[adj].color == color
		&& dragon[adj].matcher_status == CRITICAL
		&& dragon2[d].safety != INESSENTIAL
		&& !owl_defense_move_reason_known(pos, find_dragon(adj)))
	      this_value = 0.0;
	  }
	}
		
	if (this_value > dragon_value[d1]) {
	  dragon_value[d1] = this_value;
          DEBUG(DEBUG_MOVE_REASONS, "  %1m:   %f - %1m strategic attack/defend\n",
		    pos, this_value, aa);

	}
	break;

      case UNCERTAIN_OWL_DEFENSE:
	d1 = move_reasons[r].what;
	aa = dragons[d1];
	
	/* If there is an adjacent dragon which is critical we should
	 * skip this type of move reason, since attacking or defending
	 * the critical dragon is more urgent.
	 */
	{
	  int d;
	  int found_one = 0;
	  
	  for (d = 0; d < DRAGON2(aa).neighbors; d++)
	    if (DRAGON(DRAGON2(aa).adjacent[d]).matcher_status == CRITICAL)
	      found_one = 1;
	  if (found_one)
	    break;
	}
	
	/* If we are behind, we should skip this type of move reason. 
	 * If we are ahead, we should value it more. 
	 */
	if ((color == BLACK && score > 0.0)
	    || (color == WHITE && score < 0.0))
	  this_value = 0.0;
	else 
	  this_value = gg_min(2*dragon[aa].effective_size, gg_abs(score/2));
	
	if (this_value > dragon_value[d1]) {
	  dragon_value[d1] = this_value;
	  DEBUG(DEBUG_MOVE_REASONS, "  %1m:   %f - %1m uncertain owl defense bonus\n",
		    pos, this_value, aa);
	}

	break;
    }
  }
  
  for (k = 0; k < next_dragon; k++) {
    if (dragon_value[k] == 0.0)
      continue;

    aa = dragons[k];
    
    /* If this dragon consists of only one worm and that worm can
     * be tactically captured or defended by this move, we have
     * already counted the points as territorial value, unless
     * it's assumed to be dead.
     */
    if (dragon[aa].matcher_status != DEAD
	&& dragon[aa].size == worm[aa].size
	&& (attack_move_reason_known(pos, find_worm(aa))
	    || defense_move_reason_known(pos, find_worm(aa)))) {
      TRACE("  %1m:   %f - %1m strategic value already counted.\n", pos, dragon_value[k], aa);
      continue;
    }
    /* If the dragon has been owl captured, owl defended, or involved
     * in a semeai, we have likewise already counted the points as
     * territorial value.
     */
    if (owl_attack_move_reason_known(pos, k)
	|| owl_defense_move_reason_known(pos, k)
	|| move_reason_known(pos, SEMEAI_MOVE, k)) {
      /* But if the strategical value was larger than the territorial
       * value (e.g. because connecting to strong dragon) we award the
       * excess value as a bonus.
       */
      float excess_value = (dragon_value[k] - 
			    2 * dragon[dragons[k]].effective_size);
      if (excess_value > 0.0) {
	TRACE("  %1m: %f - strategic bonus for %1m\n",
	      pos, excess_value, dragons[k]);
	tot_value += excess_value;
      } else {
	TRACE(" %1m:   %f - %1m strategic value already counted.\n", pos, dragon_value[k], aa);
      }
      
      continue;
    }

    TRACE("  %1m: %f - strategic effect on %1m\n",
	  pos, dragon_value[k], dragons[k]);
    tot_value += dragon_value[k];
  }

  move[pos].strategical_value = tot_value;
}


/* Compare two move reasons, used for sorting before presentation. */
static int
compare_move_reasons(const void *p1, const void *p2)
{
  const int mr1 = *(const int *) p1;
  const int mr2 = *(const int *) p2;

  if (move_reasons[mr1].type != move_reasons[mr2].type)
    return move_reasons[mr2].type - move_reasons[mr1].type;
  else
    return move_reasons[mr2].what - move_reasons[mr1].what;
}


/*
 * Combine the reasons for a move at (pos) into a simple numerical value.
 * These heuristics are now somewhat less ad hoc than before but probably
 * still need a lot of improvement.
 */
static float
value_move_reasons(int pos, int color, float pure_threat_value,
		   float score)
{
  float tot_value;
  float shape_factor;

  gg_assert(stackp == 0);
  
  /* Is it an antisuji? */
  if (is_antisuji_move(pos))
    return 0.0; /* This move must not be played. End of story. */

  /* If this move has no reason at all, we can skip some steps. */
  if ((!urgent || allpats)
      && (move[pos].reason[0] >= 0
	  || move[pos].min_territory > 0.0)) {
    int num_reasons;

    /* Sort the move reasons. This makes it easier to visually compare
     * the reasons for different moves in the trace outputs.
     */
    num_reasons = 0;
    while (move[pos].reason[num_reasons] >= 0)
      num_reasons++;
    gg_sort(move[pos].reason, num_reasons, sizeof(move[pos].reason[0]),
	    compare_move_reasons);

    /* Discard move reasons that only duplicate another. */
    discard_redundant_move_reasons(pos);

    /* Estimate the value of various aspects of the move. The order
     * is significant. Territorial value must be computed before
     * strategical value. See connection_value().
     */
    estimate_territorial_value(pos, color, score);
    estimate_strategical_value(pos, color, score);
  }

  tot_value = move[pos].territorial_value + move[pos].strategical_value;

  shape_factor = compute_shape_factor(pos);

  if (tot_value > 0.0) {
    int c;
    
    /* In the endgame, there are a few situations where the value can
     * be 0 points + followup.  But we want to take the intersections first
     * were we actually get some points.  0.5 points is a 1 point ko which
     * is the smallest value that is actually worth something.
     * tm - But with reverse_followup values, we may want to play a 0
     *      point move. 
     */
    if (tot_value >= 0.5 
        || (move[pos].reverse_followup_value >= 1.0)) {
      float old_tot_value = tot_value;
      float contribution;
      /* We adjust the value according to followup and reverse followup
       * values.
       */
      contribution = gg_min(gg_min(0.5 * move[pos].followup_value
				   + 0.5 * move[pos].reverse_followup_value,
				   1.0 * tot_value
				   + move[pos].followup_value),
			    1.1 * tot_value
			    + move[pos].reverse_followup_value);
      tot_value += contribution;
      /* The first case applies to gote vs gote situation, the
       * second to reverse sente, and the third to sente situations.
       * The usual rule is that a sente move should count at double
       * value. But if we have a 1 point move with big followup (i.e.
       * sente) we want to play that before a 2 point gote move. Hence
       * the factor 1.1 above.
       */
      
      if (contribution != 0.0) {
	TRACE("  %1m: %f - added due to followup (%f) and reverse followup values (%f)\n",
              pos, contribution, move[pos].followup_value,
              move[pos].reverse_followup_value);
      }

      /* If a ko fight is going on, we should use the full followup
       * and reverse followup values in the total value. We save the
       * additional contribution for later access.
       */
      move[pos].additional_ko_value =
	move[pos].followup_value 
	+ move[pos].reverse_followup_value 
	- (tot_value - old_tot_value);

      /* Not sure whether this could happen, but check for safety. */
      if (move[pos].additional_ko_value < 0.0)
	move[pos].additional_ko_value = 0.0;
    }
    else {
      move[pos].additional_ko_value =
	shape_factor * (move[pos].followup_value
			+ move[pos].reverse_followup_value);
    }

    tot_value += 0.05 * move[pos].secondary_value;
    if (move[pos].secondary_value != 0.0)
      TRACE("  %1m: %f - secondary\n", pos, 0.05 * move[pos].secondary_value);

    if (move[pos].numpos_shape + move[pos].numneg_shape > 0) {
      /* shape_factor has already been computed. */
      float old_value = tot_value;
      tot_value *= shape_factor;
      if (verbose) {
	/* Should all have been TRACE, except we want field sizes. */
	gprintf("  %1m: %f - shape ", pos, tot_value - old_value);
	fprintf(stderr,
		"(shape values +%4.2f(%d) -%4.2f(%d), shape factor %5.3f)\n",
		move[pos].maxpos_shape, move[pos].numpos_shape,
		move[pos].maxneg_shape, move[pos].numneg_shape,
		shape_factor);
      }
    }

    /* Add a special shape bonus for moves which connect strings. */
    c = move_connects_strings(pos, color);
    if (c > 0) {
      float shape_factor2 = pow(1.02, (float) c) - 1;
      float base_value = gg_max(gg_min(tot_value, 5.0), 1.0);
      if (verbose) {
	/* Should all have been TRACE, except we want field sizes. */
	gprintf("  %1m: %f - connects strings ", pos,
		base_value * shape_factor2);
	fprintf(stderr, "(connect value %d, shape factor %5.3f)\n", c,
		shape_factor2);
      }
      tot_value += base_value * shape_factor2;
    }
  }
  else {
    move[pos].additional_ko_value =
      shape_factor * (move[pos].followup_value +
		      gg_min(move[pos].followup_value,
			     move[pos].reverse_followup_value));
  }

  /* If the move is valued 0 or small, but has followup values and is
   * flagged as a worthwhile threat, add up to pure_threat_value to
   * the move.
   *
   * FIXME: We shouldn't have to call confirm_safety() here. It's
   * potentially too expensive.
   */
  if (pure_threat_value > 0.0 
      && move[pos].worthwhile_threat
      && tot_value <= pure_threat_value
      && board[pos] == EMPTY
      && move[pos].additional_ko_value > 0.0
      && is_legal(pos, color)
      && move_reasons_confirm_safety(pos, color, 0)) {
    float new_tot_value = gg_min(pure_threat_value,
				 tot_value
				 + 0.25 * move[pos].additional_ko_value);

    /* Make sure that moves with independent value are preferred over
     * those without.
     */
    new_tot_value *= (1.0 - 0.1 * (pure_threat_value - tot_value)
		      / pure_threat_value);
    
    if (new_tot_value > tot_value) {
      TRACE("  %1m: %f - carry out threat or defend against threat\n",
	    pos, new_tot_value - tot_value);
      tot_value = new_tot_value;
    }
  }
  
  /* Test if min_value or max_value values constrain the total value.
   * First avoid contradictions between min_value and max_value,
   * assuming that min_value is right.
   */
  if (move[pos].min_value > move[pos].max_value)
    move[pos].max_value = move[pos].min_value;

  /* If several moves have an identical minimum value, then GNU Go uses the
   * following secondary criterion (unless min_value and max_value agree, and
   * unless min_value is bigger than 25, in which case it probably comes from
   * a J or U pattern): 
   */
  if (move[pos].min_value < 25)
    move[pos].min_value += tot_value / 200;
  if (tot_value < move[pos].min_value
      && move[pos].min_value > 0) {
    tot_value = move[pos].min_value;
    TRACE("  %1m:   %f - minimum accepted value\n", pos, tot_value);
  }
  
  if (tot_value > move[pos].max_value) {
    tot_value = move[pos].max_value;
    TRACE("  %1m:   %f - maximum accepted value\n",
          pos, tot_value);
  }

  if (tot_value > 0
      || move[pos].territorial_value > 0
      || move[pos].strategical_value > 0) {
    TRACE("Move generation values %1m to %f\n", pos, tot_value);
    move_considered(pos, tot_value);
  }

  return tot_value;
}


/*
 * Loop over all possible moves and value the move reasons for each.
 */
static void
value_moves(int color, float pure_threat_value, float score)
{
  int m;
  int n;
  int pos;

  TRACE("\nMove valuation:\n");
  
  /* Visit the moves in the standard lexicographical order */
  for (n = 0; n < board_size; n++)
    for (m = board_size-1; m >= 0; m--) {
      pos = POS(m, n);

      move[pos].value = value_move_reasons(pos, color, 
					   pure_threat_value, score);
      if (move[pos].value == 0.0)
	continue;

      /* Maybe this test should be performed elsewhere. This is just
       * to get some extra safety. We don't filter out illegal ko
       * captures here though, because if that is the best move, we
       * should reevaluate ko threats.
       */
      if (is_legal(pos, color) || is_illegal_ko_capture(pos, color)) {
	/* Add a random number between 0 and 0.01 to use in comparisons. */
	move[pos].value += 0.01 * move[pos].random_number;
      }
      else {
	move[pos].value = 0.0;
	TRACE("Move at %1m wasn't legal.\n", pos);
      }
    }
}


static void
print_top_moves(void)
{
  int k;
  int m, n;
  int pos;
  float tval;
  
  for (k = 0; k < 10; k++)
    best_move_values[k] = 0.0;
  
  /* Search through all board positions for the 10 highest valued
   * moves and print them.
   */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      pos = POS(m, n);

      if (move[pos].final_value <= 0.0)
	continue;
      
      tval = move[pos].final_value;

      for (k = 9; k >= 0; k--)
	if (tval > best_move_values[k]) {
	  if (k < 9) {
	    best_move_values[k+1] = best_move_values[k];
	    best_moves[k+1] = best_moves[k];
	  }
	  best_move_values[k] = tval;
	  best_moves[k] = pos;
	}
    }

  TRACE("\nTop moves:\n");
  for (k = 0; k < 10 && best_move_values[k] > 0.0; k++) {
    TRACE("%d. %1M %f\n", k+1, best_moves[k], best_move_values[k]);
  }
}


static void
reevaluate_ko_threats(void)
{
  int m, n;
  int pos;

  TRACE("Reevaluating ko threats.\n");
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      pos = POS(m, n);

      if (move[pos].additional_ko_value > 0.0) {
	TRACE("%1m: %f + %f = %f\n", pos, move[pos].value,
	      move[pos].additional_ko_value,
	      move[pos].value + move[pos].additional_ko_value);
	move[pos].value += move[pos].additional_ko_value;
      }
    }
}


static void
redistribute_points(void)
{
  int m, n;
  int pos;
  int ii;

  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      pos = POS(m, n);

      move[pos].final_value = move[pos].value;
    }
  
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      pos = POS(m, n);
      
      ii = replacement_map[pos];
      if (ii == NO_MOVE)
	continue;

      TRACE("Redistributing points from %1m to %1m.\n", pos, ii);
      if (move[ii].final_value < move[pos].final_value) {
	TRACE("%1m is now valued %f.\n", ii, move[pos].final_value);
	move[ii].final_value = move[pos].final_value;
      }
      TRACE("%1m is now valued 0.\n", pos);
      move[pos].final_value = 0.0;
    }
}


/*
 * Review the move reasons to find which (if any) move we want to play.
 *
 * The parameter pure_threat_value is the value assigned to a move
 * which only threatens to capture or kill something. The reason for
 * playing these is that the move may be effective because we have
 * misevaluated the dangers or because the opponent misplays.
 */
int
review_move_reasons(int *the_move, float *val, int color,
		    float pure_threat_value, float score)
{
  int m, n;
  int ii;
  float tval;
  float bestval = 0.0;
  int best_move = NO_MOVE;
  int ko_values_have_been_added = 0;
  int allowed_blunder_size = 0;

  int good_move_found = 0;
  int save_verbose;
  
  start_timer(2);
  if (!urgent || allpats) {
    find_more_attack_and_defense_moves(color);
    time_report(2, "  find_more_attack_and_defense_moves", NO_MOVE, 1.0);
  }

  save_verbose = verbose;
  if (verbose > 0)
    verbose--;
  if (level > 5) {
    find_more_owl_attack_and_defense_moves(color);
    time_report(2, "  find_more_owl_attack_and_defense_moves", NO_MOVE, 1.0);
  }
  verbose = save_verbose;

  induce_secondary_move_reasons(color);
  time_report(2, "  induce_secondary_move_reasons", NO_MOVE, 1.0);
  
  if (verbose > 0)
    verbose--;
  examine_move_safety(color);
  time_report(2, "  examine_move_safety", NO_MOVE, 1.0);
  verbose = save_verbose;

  if (printworms || verbose)
    list_move_reasons(color);

  /* Evaluate all moves with move reasons. */
  value_moves(color, pure_threat_value, score);
  time_report(2, "  value_moves", NO_MOVE, 1.0);

  /* Perform point redistribution */
  redistribute_points();

  /* Search through all board positions for the 10 highest valued
   * moves and print them.
   */
  print_top_moves();
  while (!good_move_found) {
    bestval = 0.0;
    best_move = NO_MOVE;

    /* Search through all board positions for the highest valued move. */
    for (m = 0; m < board_size; m++)
      for (n = 0; n < board_size; n++) {
	ii = POS(m, n);

	if (move[ii].final_value == 0.0)
	  continue;
	
	tval = move[ii].final_value;
	
	if (tval > bestval) {
	  if (is_legal(ii, color) || is_illegal_ko_capture(ii, color)) {
	    bestval = tval;
	    best_move = ii;
	  }
	  else {
	    TRACE("Move at %1m would be suicide.\n", ii);
	    move[ii].value = 0.0;
	    move[ii].final_value = 0.0;
	  }
	}
      }
    
    /* Compute the size of strings we can allow to lose due to blunder
     * effects. If ko threat values have been added, only the base
     * value of the move must be taken into account here.
     */
    if (!ko_values_have_been_added || !ON_BOARD(best_move))
      allowed_blunder_size = (int) (bestval / 2 - 1);
    else {
      int base_value;

      ASSERT_ON_BOARD1(best_move);
      base_value = bestval - move[best_move].additional_ko_value;
      allowed_blunder_size = (int) (base_value / 2 - 1);
    }
    
    /* If the best move is an illegal ko capture, reevaluate ko
     * threats and search again.
     */
    if (bestval > 0.0 && is_illegal_ko_capture(best_move, color)) {
      TRACE("Move at %1m would be an illegal ko capture.\n", best_move);
      reevaluate_ko_threats();
      redistribute_points();
      time_report(2, "  reevaluate_ko_threats", NO_MOVE, 1.0);
      ko_values_have_been_added = 1;
      move[best_move].value = 0.0;
      move[best_move].final_value = 0.0;
      print_top_moves();
      good_move_found = 0;
    }
    /* Call confirm_safety() to check that we're not about to make a
     * blunder. Otherwise reject this move and scan through all move
     * values once more.
     */
    else if (bestval > 0.0
	     && !move_reasons_confirm_safety(best_move, color,
					     allowed_blunder_size)) {
      TRACE("Move at %1m would be a blunder.\n", best_move);
      move[best_move].value = 0.0;
      move[best_move].final_value = 0.0;
      good_move_found = 0;
    }
    else
      good_move_found = 1;
  }
  
  if (bestval > 0.0 
      && best_move != NO_MOVE) {
    *the_move = best_move;
    *val = bestval;
    return 1;
  }

  return 0;
}


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
