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
#include <math.h>

#include "liberty.h"
#include "gg_utils.h"
#include "move_reasons.h"


/* Count how many distinct strings are (solidly) connected by the move
 * at (pos). Add a bonus for strings with few liberties. Also add
 * bonus for opponent strings put in atari or removed.
 */
static int
move_connects_strings(int pos, int color)
{
  int ss[4];
  int strings = 0;
  int own_strings = 0;
  int k, l;
  int fewlibs = 0;

  for (k = 0; k < 4; k++) {
    int ii = pos + delta[k];
    int origin;

    if (!ON_BOARD(ii) || board[ii] == EMPTY)
      continue;

    origin = find_origin(ii);

    for (l = 0; l < strings; l++)
      if (ss[l] == origin)
	break;

    if (l == strings) {
      ss[strings] = origin;
      strings++;
    }
  }

  for (k = 0; k < strings; k++) {
    if (board[ss[k]] == color) {
      int newlibs = approxlib(pos, color, MAXLIBS, NULL);
      own_strings++;
      if (newlibs >= countlib(ss[k])) {
	if (countlib(ss[k]) <= 4)
	  fewlibs++;
	if (countlib(ss[k]) <= 2)
	  fewlibs++;
      }
    }
    else {
      if (countlib(ss[k]) <= 2)
	fewlibs++;
      if (countlib(ss[k]) <= 1)
	fewlibs++;
    }
  }

  /* Do some thresholding. */
  if (fewlibs > 4)
    fewlibs = 4;
  if (fewlibs == 0 && own_strings == 1)
    own_strings = 0;

  return own_strings + fewlibs;
}

/* Find saved dragons and worms, then call blunder_size(). */
static float
value_moves_get_blunder_size(int move, int color)
{
  char saved_dragons[BOARDMAX];
  char saved_worms[BOARDMAX];
  char safe_stones[BOARDMAX];

  get_saved_dragons(move, saved_dragons);
  get_saved_worms(move, saved_worms);

  mark_safe_stones(color, move, saved_dragons, saved_worms, safe_stones);
  
  return blunder_size(move, color, NULL, safe_stones);
}

static int
value_moves_confirm_safety(int move, int color)
{
  return (value_moves_get_blunder_size(move, color) == 0.0);
}


/* Test all moves which defend, attack, connect or cut to see if they
 * also attack or defend some other worm.
 *
 * FIXME: We would like to see whether an arbitrary move works to cut
 *        or connect something else too.
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
	  && worm[ii].defense_codes[0] != 0) {
	unstable_worms[N] = ii;
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
	    || move_reasons[r].type == CUT_MOVE)
	  break;
	/* FIXME: Add code for EITHER_MOVE and ALL_MOVE here. */
      }
      
      if (k == MAX_REASONS || move[ii].reason[k] == -1)
	continue;

      /* Try the move at (ii) and see what happens. */
      cursor_at_start_of_line = 0;
      TRACE("%1m ", ii);
      if (trymove(ii, color, "find_more_attack_and_defense_moves",
		  NO_MOVE, EMPTY, NO_MOVE)) {
	for (k = 0; k < N; k++) {
	  int aa = unstable_worms[k];

	  /* string of our color, see if there still is an attack,
	   * unless we already know the move works as defense move.
	   */
	  if (board[aa] == color
	      && !defense_move_reason_known(ii, unstable_worms[k])) {
	    int acode = attack(aa, NULL);
	    if (acode < worm[aa].attack_codes[0]) {
	      /* Maybe attack() doesn't find the attack. Try to
	       * attack with the stored attack move.
	       */
	      int defense_works = 1;

	      if (trymove(worm[aa].attack_points[0], other, 
			  "find_more_attack_and_defense_moves", 0,
			  EMPTY, 0)) {
		if (!board[aa])
		  defense_works = 0;
		else {
		  int this_acode = REVERSE_RESULT(find_defense(aa, NULL));
		  if (this_acode > acode) {
		    acode = this_acode;
		    if (acode >= worm[aa].attack_codes[0])
		      defense_works = 0;
		  }
		}
		popgo();
	      }
		
	      if (defense_works) {
		if (!cursor_at_start_of_line)
		  TRACE("\n");
		TRACE("%ofound extra point of defense of %1m at %1m code %d\n",
		      aa, ii, REVERSE_RESULT(acode));
		cursor_at_start_of_line = 1;
		add_defense_move(ii, aa, REVERSE_RESULT(acode));
	      }
	    }
	  }
	    
	  /* string of opponent color, see if there still is a defense,
	   * unless we already know the move works as attack move.
	   */
	  if (board[aa] == other
	      && !attack_move_reason_known(ii, unstable_worms[k])) {
	    
	    int dcode = find_defense(aa, NULL);
	    if (dcode < worm[aa].defense_codes[0]) {
	      /* Maybe find_defense() doesn't find the defense. Try to
	       * defend with the stored defense move.
	       */
	      int attack_works = 1;

	      if (trymove(worm[aa].defense_points[0], other, 
			  "find_more_attack_and_defense_moves", 0,
			  EMPTY, 0)) {
		int this_dcode = REVERSE_RESULT(attack(aa, NULL));
		if (this_dcode > dcode) {
		  dcode = this_dcode;
		  if (dcode >= worm[aa].defense_codes[0])
		    attack_works = 0;
		}
		popgo();
	      }
		
	      if (attack_works) {
		if (!cursor_at_start_of_line)
		  TRACE("\n");
		TRACE("%ofound extra point of attack of %1m at %1m code %d\n",
		      aa, ii, REVERSE_RESULT(dcode));
		cursor_at_start_of_line = 1;
		add_attack_move(ii, aa, REVERSE_RESULT(dcode));
	      }
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
	dd1 = what;
      else if (move_reasons[r].type == ATTACK_MOVE
	       || move_reasons[r].type == ATTACK_MOVE_GOOD_KO
	       || move_reasons[r].type == ATTACK_MOVE_BAD_KO
	       || move_reasons[r].type == DEFEND_MOVE
	       || move_reasons[r].type == DEFEND_MOVE_GOOD_KO
	       || move_reasons[r].type == DEFEND_MOVE_BAD_KO)
	dd1 = what;
      else if (move_reasons[r].type == VITAL_EYE_MOVE) {
	int ee = eyes[what];
	int ecolor = eyecolor[what];
	
	if (ecolor == WHITE)
	  find_eye_dragons(ee, white_eye, WHITE, &dd1, 1);
	else
	  find_eye_dragons(ee, black_eye, BLACK, &dd1, 1);
	
	if (dd1 == NO_MOVE) /* Maybe we should assert this not to happen. */
	  continue;
      }      
      else if (move_reasons[r].type == CONNECT_MOVE) {
	int worm1 = conn_worm1[what];
	int worm2 = conn_worm2[what];
	dd1 = dragon[worm1].origin;
	dd2 = dragon[worm2].origin;
	if (dd1 == dd2)
	  dd2 = NO_MOVE;
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
	    && !owl_attack_move_reason_known(pos, dd)) {
	  int kworm = NO_MOVE;
	  int acode = owl_does_attack(pos, dd, &kworm);
	  if (acode >= dragon[dd].owl_attack_code) {
	    if (acode == GAIN)
	      add_gain_move(pos, dd, kworm);
	    else
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
	    && !owl_defense_move_reason_known(pos, dd)) {
	  int kworm = NO_MOVE;
	  /* FIXME: Better use owl_connection_defend() for CONNECT_MOVE ? */
	  int dcode = owl_does_defend(pos, dd, &kworm);
	  if (dcode >= dragon[dd].owl_defense_code) {
	    if (dcode == LOSS)
	      add_loss_move(pos, dd, kworm);
	    else
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
	    dd = move_reasons[r].what;
	    if (are_neighbor_dragons(dd, pos)) {
	      worth_trying = 1;
	      break;
	    }
	  } 
	  /* else ...
	     FIXME: what about the new OWL_ATTACK_MOVE_GAIN codes ?
	   */
	}

	if (worth_trying) {
	  if (board[pos] == color
	      && !owl_defense_move_reason_known(pos2, pos)) {
	    int kworm = NO_MOVE;
	    int dcode = owl_does_defend(pos2, pos, &kworm);
	    if (dcode >= dragon[pos].owl_defense_code) {
	      if (dcode == LOSS)
		add_loss_move(pos2, pos, kworm);
	      else
		add_owl_defense_move(pos2, pos, dcode);
	    }

	  }
	  else if (board[pos] != color
		   && !owl_attack_move_reason_known(pos2, pos)) {
	    int kworm = NO_MOVE;
	    int acode = owl_does_attack(pos2, pos, &kworm);
	    if (acode >= dragon[pos].owl_attack_code) {
	      if (acode == GAIN)
		add_gain_move(pos2, pos, kworm);
	      else
		add_owl_attack_move(pos2, pos, acode);
	    }
	  }
	}
      }
    }
  }
}



static int 
bdist(int pos1, int pos2)
{
  int idist = I(pos1) - I(pos2);
  int jdist = J(pos1) - J(pos2);
  return idist*idist + jdist*jdist;
}

/*
 * Any move that captures or defends a worm also potentially connects
 * or cuts the surrounding strings. Find these secondary move reasons
 * and verify them by connection reading.
 *
 * We also let an owl attack count as a strategical defense of our
 * neighbors of the owl attacked dragon. We only do this for
 * tactically safe dragons, however, because otherwise the effects of
 * capturing have already been taken into account elsewhere.
 *
 * Also, connecting moves played on inhibited points possibly remove
 * nearby connection inhibitions like in following example :
 * 
 * .OX.   The * move connects _all_ O stones together, not only
 * O...   the 2 lower ones.
 * XO*O
 * X.X.
 *
 */

static void
induce_secondary_move_reasons(int color)
{
  int pos;
  int k;
  int i, j;
  int aa;
  
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (!ON_BOARD(pos))
      continue;
    
    for (k = 0; k < MAX_REASONS; k++) {
      int r = move[pos].reason[k];
      
      if (r < 0)
	break;
      
      if (move_reasons[r].type == ATTACK_MOVE
	  || move_reasons[r].type == DEFEND_MOVE) {
	int attack_move;
	int color_to_move;
	int num_adj, adjs[MAXCHAIN];
	
	aa = move_reasons[r].what;
	
	if (move_reasons[r].type == ATTACK_MOVE) {
	  attack_move = 1;
	  color_to_move = OTHER_COLOR(board[aa]);
	}
	else {
	  attack_move = 0;
	  color_to_move = board[aa];
	}
		
	if (worm[aa].defense_codes[0] == 0)
	  continue; /* No defense. */
	
	/* Don't care about inessential dragons. */
	if (DRAGON2(aa).safety == INESSENTIAL)
	  continue;
	
	/*
	 * If this is a defense move and the defense is futile for
	 * strategical reasons, we shouldn't induce a cutting move
	 * reason.
	 *
	 * FIXME: We may want to revise this policy.
	 */
	if (!attack_move && !move[pos].move_safety)
	  continue;
	
	num_adj = extended_chainlinks(aa, adjs, 1);
	
	for (i = 0; i < num_adj; i++) {
	  for (j = i+1; j < num_adj; j++) {
	    int adj1 = adjs[i];
	    int adj2 = adjs[j];
	    
	    if (board[adj1] != board[adj2])
	      continue;
	    if (attack_move
		&& board[adj1] != board[aa]
		&& !disconnect(adj1, adj2, NULL))
	      continue;
	    if (!attack_move
		&& board[adj1] != board[aa]
		&& !string_connect(adj1, adj2, NULL))
	      continue;
	    if (attack_move
		&& board[adj1] == board[aa])
	      continue;
	    if (!attack_move
		&& board[adj1] == board[aa]
		&& !disconnect(adj1, adj2, NULL))
	      continue;

	    if (trymove(pos, color_to_move, "induce_secondary_move_reasons",
			aa, EMPTY, NO_MOVE)) {
	      if (attack_move
		  && board[adj1] != board[aa]
		  && !disconnect(adj1, adj2, NULL)) {
		TRACE_MOVE_REASONS(
		      "Connection move at %1m induced for %1m/%1m due to attack of %1m\n",
		      pos, adj1, adj2, aa);
		add_connection_move(pos, adj1, adj2);
	      }
		  
	      if (!attack_move
		  && board[adj1] != board[aa]
		  && !string_connect(adj1, adj2, NULL)) {
		TRACE_MOVE_REASONS(
		      "Cut move at %1m induced for %1m/%1m due to defense of %1m\n",
		      pos, adj1, adj2, aa);
		add_cut_move(pos, adj1, adj2);
	      }

	      if (!attack_move
		  && board[adj1] == board[aa]
		  && !disconnect(adj1, adj2, NULL)) {
		TRACE_MOVE_REASONS(
		      "Connection move at %1m induced for %1m/%1m due to defense of %1m\n",
		      pos, adj1, adj2, aa);
		add_connection_move(pos, adj1, adj2);
	      }
		  
	      popgo();
	    }
	  }
	}  
      }
      else if (move_reasons[r].type == OWL_ATTACK_MOVE) {
	aa = move_reasons[r].what;
	for (i = 0; i < DRAGON2(aa).neighbors; i++) {
	  int bb = dragon2[DRAGON2(aa).adjacent[i]].origin;
	  if (dragon[bb].color == color && worm[bb].attack_codes[0] == 0)
	    add_strategical_defense_move(pos, bb);
	}
      }
      else if (move_reasons[r].type == CONNECT_MOVE
	      && cut_possible(pos, OTHER_COLOR(color))) {
	int worm1 = conn_worm1[move_reasons[r].what];
	int worm2 = conn_worm2[move_reasons[r].what];
	int pos2;
	for (pos2 = BOARDMIN; pos2 < BOARDMAX; pos2++)
	  if (ON_BOARD(pos2) && board[pos2] == EMPTY
	      && cut_possible(pos2, OTHER_COLOR(color))
	      && bdist(pos, pos2) <= 5)
	    for (j = 0; j < 8; j++) {
	      int pos3 = pos2 + delta[j];
	      if (ON_BOARD(pos3) && board[pos3] == color
		  && !is_same_worm(pos3, worm1)
		  && !is_same_worm(pos3, worm2)) {
		if (trymove(pos, color, "induce_secondary_move_reasons-B",
			    worm1, EMPTY, NO_MOVE)) {
		  if (!disconnect(pos3, worm1, NULL))
		    add_connection_move(pos, pos3, worm1);
		  if (!disconnect(pos3, worm2, NULL))
		    add_connection_move(pos, pos3, worm2);
		  popgo();
		}
	      }
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
  int pos;
  int k;
  
  start_timer(3);
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    int safety = 0;
    int tactical_safety = 0;
    if (!ON_BOARD(pos))
      continue;
      
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
      case OWL_DEFEND_MOVE:
      case OWL_DEFEND_MOVE_GOOD_KO:
      case OWL_DEFEND_MOVE_BAD_KO:
      case OWL_DEFEND_MOVE_LOSS:
       	/* FIXME: The above imply not necessarily a safe move, if the
	 * defending move is not connected to the dragon defended.
	 */
      case MY_ATARI_ATARI_MOVE:
      case YOUR_ATARI_ATARI_MOVE:
      case EITHER_MOVE:         /* FIXME: More advanced handling? */
      case ALL_MOVE:            /* FIXME: More advanced handling? */
	tactical_safety = 1;
	safety = 1;
	break;
      case EXPAND_TERRITORY_MOVE:
      case EXPAND_MOYO_MOVE:
      case INVASION_MOVE:   /* A real invasion should be safe.
			       A sacrifice is something else.*/
        safety = 1;
	break;
      case ATTACK_MOVE:
      case ATTACK_MOVE_GOOD_KO:
      case ATTACK_MOVE_BAD_KO:
      case OWL_ATTACK_MOVE:
      case OWL_ATTACK_MOVE_GOOD_KO:
      case OWL_ATTACK_MOVE_BAD_KO:
      case OWL_ATTACK_MOVE_GAIN:
        {
	  int aa = NO_MOVE;
	  int bb = NO_MOVE;
	  int size;
	  int m;
	  int our_color_neighbors;
	  
	  if (type == ATTACK_MOVE
	      || type == ATTACK_MOVE_GOOD_KO
	      || type == ATTACK_MOVE_BAD_KO) {
	    aa = what;
	    size = worm[aa].effective_size;
	  } 
	  else if (type == OWL_ATTACK_MOVE_GAIN) {
	    aa = either_data[what].what2;
	    size = worm[aa].effective_size;
	  } 
	  else {
	    aa = what;
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
	    for (m = 0; m < num_adj; m++) {
	      int adj = adjs[m];
	      
	      if (board[adj] == color) {
		/* Check whether this string is part of the same
		 * dragon as an earlier string. We only want to
		 * count distinct neighbor dragons.
		 */
		int n;
		
		for (n = 0; n < m; n++)
		  if (dragon[adjs[n]].id == dragon[adj].id)
		    break;
		if (n == m) {
		  /* New dragon. */
		  our_color_neighbors++;
		  bb = adj;
		}
	      }
	    }
	  }
	  else {
	    for (m = 0; m < DRAGON2(aa).neighbors; m++)
	      if (DRAGON(DRAGON2(aa).adjacent[m]).color == color) {
		our_color_neighbors++;
		bb = dragon2[DRAGON2(aa).adjacent[m]].origin;
		if (dragon[bb].status == ALIVE) {
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
	  
	  for (m = 0; m < 4; m++) {
	    int d = delta[m];
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
	  if (dragon[bb].status == ALIVE) {
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
	  if (owl_does_defend(pos, bb, NULL)) {
	    tactical_safety = 1;
	    safety = 1;
	  }
	  break;
	}
      case DEFEND_MOVE:
      case DEFEND_MOVE_GOOD_KO:
      case DEFEND_MOVE_BAD_KO:
	{
	  int aa = what;
	  
	  if (dragon[aa].status == ALIVE)
	    /* It would be better if this never happened, but it does
	     * sometimes. The owl reading can be very slow then.
	     */
	    safety = 1;
	  
	  else if (owl_does_defend(pos, aa, NULL))
	    safety = 1;
	  break;
	}
	
      case ATTACK_THREAT:
      case DEFEND_THREAT:
	break;

      case CONNECT_MOVE:
        {
	  int worm1 = conn_worm1[move_reasons[r].what];
	  int worm2 = conn_worm2[move_reasons[r].what];
	  int aa = dragon[worm1].origin;
	  int bb = dragon[worm2].origin;

	  if (aa == bb)
	    continue;
	  
	  if (dragon[aa].owl_status == ALIVE
	      || dragon[bb].owl_status == ALIVE) {
	    tactical_safety = 1;
	    safety = 1;
	  }
	  else if ((dragon[aa].owl_status == UNCHECKED
		    && dragon[aa].crude_status == ALIVE)
		   || (dragon[bb].owl_status == UNCHECKED
		       && dragon[bb].crude_status == ALIVE)) {
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
 * Returns the pre-computed weakness of a dragon, with corrections
 * according to ignore_dead_dragons.
 *
 * FIXME: Important to test more exactly how effective a strategical
 *        attack or defense of a weak dragon is. This can be done by
 *        measuring escape factor and moyo size after the move and
 *        compare with the old values. Also necessary to test whether
 *        an attack or defense of a critical dragon is effective.
 *        Notice that this wouldn't exactly go into this function but
 *        rather where it's called.
 */

static float
dragon_weakness(int dr, int ignore_dead_dragons)
{
  int dragon_safety = DRAGON2(dr).safety;

  /* Kludge: If a dragon is dead, we return 1.0 in order not
   * to try to run away.
   */
  if (ignore_dead_dragons
      && (dragon_safety == DEAD
	  || dragon_safety == INESSENTIAL
	  || dragon_safety == TACTICALLY_DEAD))
    return 0.0;

  /* When scoring, we don't want to reinforce ALIVE dragons. */
  if (doing_scoring && dragon_safety == ALIVE)
    return 0.0;
  
  return DRAGON2(dr).weakness;
}

/*
 * Strategical value of connecting (or cutting) the dragon at (dragona)
 * to the dragon at (dragonb). Notice that this function is asymmetric.
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
 * If this exceeds 20 points we value connections more.  This is because
 * we can afford to waste a move making sure of safety.
 */

static float
connection_value(int dragona, int dragonb, int tt, float margin)
{
  struct dragon_data2* da = &DRAGON2(dragona);
  struct dragon_data2* db = &DRAGON2(dragonb);
  float sizea = dragon[dragona].effective_size;
  float sizeb = dragon[dragonb].effective_size;
  int safetya = da->safety;
  int safetyb = db->safety;
  float crude_weakness_a
    = crude_dragon_weakness(da->safety, &da->genus, da->lunch != NO_MOVE, 
			    da->moyo_territorial_value,
			    (float) da->escape_route);
  float crude_weakness_sum;
  struct eyevalue genus_sum;
  float terr_val = move[tt].territorial_value;
  float return_value;

  if (margin > 20.0)
    margin = 20.0;

  /* When scoring, we want to be restrictive with reinforcement moves
   * inside own territory. Thus if both dragons are weakly_alive,
   * alive, strongly alive, or invincible, no bonus is awarded.
   *
   * Notice that this requires that the territorial value is computed
   * before the strategical value.
   *
   * FIXME: Shouldn't it be sufficient to check this for dragon a?
   */
  if (doing_scoring && terr_val < 0.0) {
    if ((safetya == ALIVE
	 || safetya == STRONGLY_ALIVE
	 || safetya == INVINCIBLE)
	&& (safetyb == ALIVE
	    || safetyb == STRONGLY_ALIVE
	    || safetyb == INVINCIBLE))
      return 0.0;
  }

  if (crude_weakness_a == 0.0
      || dragon[dragona].status == DEAD)
    return 0.0;
  if (terr_val < 0.0)
    terr_val = 0.0;

  add_eyevalues(&da->genus, &db->genus, &genus_sum);
  /* FIXME: There is currently no sane way to take the escape values
   * into account. Hence we simply pretend they do not change.
   *
   * FIXME: terr_val is a very crude approximation to the expected
   * increase in moyo size. It's especially way off if the move at (tt)
   * (owl) defends some stones.
   */
  crude_weakness_sum
    = crude_dragon_weakness(safetyb, &genus_sum,
			    (da->lunch != NO_MOVE || db->lunch != NO_MOVE), 
			    da->moyo_territorial_value
			    + db->moyo_territorial_value
			    + terr_val,
			    (float) da->escape_route);

  /* Kludge: For a CRITICAL dragon, we use the usual effective
   * size and give a strategic effect bigger than 2.0 * effective size.
   * This is to match the "strategic bonus computation" in
   * estimate_strategical_value(). This prefers connection moves that
   * owl defend a dragon to other owl defense move.
   */
  if (dragon[dragona].status == CRITICAL) {
    float bonus = (0.2 - 0.3 * crude_weakness_sum) * sizea;
    /* If ahead, give extra bonus to connections. */
    if (margin > 0.0 && bonus > 0.0)
      bonus *= 1.0 + 0.05 * margin;
    return_value = 2.0 * sizea + bonus;
  }
  else {
    float old_burden = 2.0 * crude_weakness_a * soft_cap(sizea, 15.0);

    /* The new burden is the burden of defending new joint dragon; but
     * we share this burden proportionally with the other dragon.
     */
    float new_burden = 2.0 * crude_weakness_sum * soft_cap(sizea + sizeb, 15.0)
		       * sizea / (sizea + sizeb);

    return_value = 1.05 * (old_burden - new_burden);
    /* If ahead, give extra bonus to connections. */
    if (margin > 0.0)
      return_value *= 1.0 + 0.02 * margin;
  }

  if (return_value < 0.0)
    return_value = 0.0;

  return return_value;
}


/* 
 * This function computes the shape factor, which multiplies
 * the score of a move. We take the largest positive contribution
 * to shape and add 1 for each additional positive contribution found.
 * Then we take the largest negative contribution to shape, and
 * add 1 for each additional negative contribution. The resulting
 * number is raised to the power 1.05.
 *
 * The rationale behind this complicated scheme is that every
 * shape point is very significant. If two shape contributions
 * with values (say) 5 and 3 are found, the second contribution
 * should be devalued to 1. Otherwise the engine is too difficult to
 * tune since finding multiple contributions to shape can cause
 * significant overvaluing of a move.
 */

static float
compute_shape_factor(int pos)
{
  float exponent = move[pos].maxpos_shape - move[pos].maxneg_shape;

  ASSERT_ON_BOARD1(pos);
  if (move[pos].numpos_shape > 1)
    exponent += move[pos].numpos_shape - 1;
  if (move[pos].numneg_shape > 1)
    exponent -= move[pos].numneg_shape - 1;
  return pow(1.05, exponent);
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
  int num_adj;
  int adjs[MAXCHAIN];
  int has_live_neighbor = 0;
  float adjusted_value = 2 * worm[ww].effective_size;
  float adjustment_up = 0.0;
  float adjustment_down = 0.0;
  int s;

  num_adj = chainlinks(ww, adjs);
  for (s = 0; s < num_adj; s++) {
    int adj = adjs[s];

    if (dragon[adj].status != DEAD)
      has_live_neighbor = 1;

    if (dragon[adj].status == DEAD
	&& 2*dragon[adj].effective_size > adjustment_up)
      adjustment_up = 2*dragon[adj].effective_size;

    if (worm[adj].attack_codes[0] != 0
	&& !does_defend(pos, adj)
	&& 2*worm[adj].effective_size > adjustment_down)
      adjustment_down = 2*worm[adj].effective_size;
  }

  if (has_live_neighbor)
    adjusted_value += adjustment_up;
  adjusted_value -= adjustment_down;

  /* It can happen that the adjustment down was larger than the effective
   * size we started with. In this case we simply return 0.0. (This means
   * we ignore the respective EITHER_MOVE reason.)
   */
  if (adjusted_value > 0.0)
    return adjusted_value;
  else
    return 0.0;
}


/* The new (3.2) territorial evaluation overvalues moves creating a new
 * group in the opponent's sphere of influence. The influence module cannot
 * see that the opponent will gain by attacking the new (probably weak)
 * group.
 * This function uses some heuristics to estimate the strategic penalty
 * of invasion moves, and moves that try to run away with a group of size
 * 1 in front of opponent's strength.
 */
static float
strategic_penalty(int pos, int color)
{
  int k;
  float ret_val;
  
  /* We try to detect support from an alive friendly stone by checking
   * whether all neighboring intersections belong to the opponent's moyo.
   */
  for (k = 0; k < 4; k++)
    if (board[pos + delta[k]] == EMPTY
        && whose_area(OPPOSITE_INFLUENCE(color), pos + delta[k])
	   != OTHER_COLOR(color))
      return 0.0;
  if (whose_area(OPPOSITE_INFLUENCE(color), pos) != OTHER_COLOR(color))
    return 0.0;

  for (k = 0; k < MAX_REASONS; k++) {
    int r = move[pos].reason[k];
    if (r < 0)
      break;
    /* We assume that invasion moves can only have the move reasons listed
     * below.
     *
     * FIXME: EXPAND_TERRITORY should always be connected to our own
     *        stones.  Remove later when that change is done.
     */
    switch (move_reasons[r].type) {
    case EXPAND_MOYO_MOVE:
    case STRATEGIC_ATTACK_MOVE:
    case INVASION_MOVE:
      continue;
    /* If we find a tactical defense move, we just test whether it concerns
     * a single-stone-dragon; if not, we stop, if yes, we let the necessary
     * tests be made in the OWL_DEFEND_MOVE case.
     */
    case DEFEND_MOVE:
      {
	int target = move_reasons[r].what;
	if (dragon[target].size > 1)
	  return 0.0;
	continue;
      }
    /* An owl defense of a single stone might be a stupid attempt to run
     * away with an unimportant (kikashi like) stone. We assume this is the
     * case if this single stone has a strong hostile direct neighbor.
     */
    case OWL_DEFEND_MOVE:
      {
	int target = move_reasons[r].what;
	int has_strong_neighbor = 0;
	int has_weak_neighbor = 0;
	int i;
	/* We award no penalty for running away with a cutting stone. */
	if (dragon[target].size > 1
	    || worm[target].cutstone > 0
	    || worm[target].cutstone2 > 0)
	  return 0.0;
	/* Third line moves (or lower) are ok -- they try to live, not run
         * away.
	 *
	 * FIXME: Add an "edge_distance()" function in board.c which can
	 *        be used here.
         */
        if (gg_min(gg_min(I(pos), board_size-1 - I(pos)),
                   gg_min(J(pos), board_size-1 - J(pos)))
            < 3)
	  return 0.0;
	
	for (i = 0; i < 4; i++)
	  if (board[target + delta[i]] == OTHER_COLOR(color)) {
	    if (dragon[target + delta[i]].size == 1) {
	      has_weak_neighbor = 1;
	      break;
	    }
	    switch (DRAGON2(target + delta[i]).safety) {
	    case INVINCIBLE:
	    case STRONGLY_ALIVE:
	      has_strong_neighbor = 1;
	      break;
	    case ALIVE:
	      if (DRAGON2(target + delta[i]).weakness > 0.4)
		has_weak_neighbor = 1;
	      break;
	    default:
	      has_weak_neighbor = 1;
	    }
	  }
	if (has_weak_neighbor || (!has_strong_neighbor))
	  return 0.0;
	else
	  continue;
      }
    default:
      return 0.0;
    }  
  }

  /* We have to make a guess how much the point where we want to play
   * is dominated by the opponent. The territorial valuation is a
   * good try here.
   */
  ret_val = influence_territory(INITIAL_INFLUENCE(OTHER_COLOR(color)),
      				pos, OTHER_COLOR(color));
  ret_val *= 12.0;
  ret_val = gg_max(0.0, ret_val);
  return ret_val;
}


/* True if pos is adjacent to a nondead stone of the given color. This
 * function can be called when stackp>0 but the result is given for the
 * position when stackp==0.
 *
 * FIXME: Move this somewhere more generally accessible, probably
 *        utils.c
 */
static int
adjacent_to_nondead_stone(int pos, int color)
{
  int k;
  for (k = 0; k < 4; k++)
    if (ON_BOARD(pos + delta[k])
	&& worm[pos + delta[k]].color == color
	&& dragon[pos + delta[k]].status != DEAD)
      return 1;
  
  return 0;
}


/*
 * Estimate the direct territorial value of a move at (pos).
 */
static void
estimate_territorial_value(int pos, int color, float score)
{
  int other = OTHER_COLOR(color);
  int k;
  int aa = NO_MOVE;
  int bb = NO_MOVE;
  
  float this_value = 0.0;
  float tot_value = 0.0;
  float secondary_value = 0.0;

  int does_block = 0;
  char safe_stones[BOARDMAX];
  float strength[BOARDMAX];

  set_strength_data(OTHER_COLOR(color), safe_stones, strength);
  
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
      aa = move_reasons[r].what;
      
      gg_assert(board[aa] != color);
      
      /* Defenseless stone. */
      if (worm[aa].defense_codes[0] == 0) {
	TRACE_MOVE_REASONS(
	      "  %1m:   %f (secondary) - attack on %1m (defenseless)\n",
	      pos, worm[aa].effective_size, aa);
	secondary_value += worm[aa].effective_size;
	does_block = 1;
	break;
      }

      this_value = 2 * worm[aa].effective_size;

      /* If the stones are dead, there is only a secondary value in
       * capturing them tactically as well.
       */
      if (dragon[aa].status == DEAD) {
	TRACE_MOVE_REASONS(
	      "  %1m:   %f (secondary) - attack on %1m (dead)\n",
	      pos, 0.2 * this_value, aa);
	secondary_value += 0.2 * this_value;
	does_block = 1;
	break;
      }

      /* Mark the string as captured, for evaluation in the influence code. */
      mark_changed_string(aa, safe_stones, strength, 0);
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
      aa = move_reasons[r].what;
      
      gg_assert(board[aa] == color);
      
      /* 
       * Estimate value 
       */
      this_value = 2 * worm[aa].effective_size;

      /* If the stones are dead, we use the convention that
       * defending them has a strategical value rather than
       * territorial. Admittedly this make more sense for attacks on
       * dead stones.
       */
      if (dragon[aa].status == DEAD) {
	TRACE_MOVE_REASONS(
	      "  %1m:   %f (secondary) - defense of %1m (dead)\n",
	      pos, 0.2 * this_value, aa);
	secondary_value += 0.2 * this_value;
	break;
      }

      /* Mark the string as saved, for evaluation in the influence code. */
      mark_changed_string(aa, safe_stones, strength, INFLUENCE_SAVED_STONE);
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

      /* If a move tactically defends an owl critical string, but
       * this move is not listed as an owl defense, it probably is
       * ineffective. The 0.45 factor is chosen so that even in
       * combination with bad ko it still has a positive net impact.
       */
      if (dragon[aa].owl_status == CRITICAL
	  && (owl_defense_move_reason_known(pos, aa)
	      < defense_move_reason_known(pos, aa))) {
	this_value = 0.45 * (2 * worm[aa].effective_size);
	TRACE("  %1m: -%f - suspected ineffective defense of worm %1m\n",
	      pos, this_value, aa);
	tot_value -= this_value;
      }

      does_block = 1;
      break;

    case ATTACK_THREAT:
      aa = move_reasons[r].what;

      /* Make sure this is a threat to attack opponent stones. */
      ASSERT1(board[aa] == other, aa);
      
      if (dragon[aa].status == DEAD) {
	TRACE_MOVE_REASONS(
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
       * back on our move because we're likely to end in gote then,
       * unless the move is unsafe anyway and played as a ko threat.
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
         * against our move. An exception to this is when our move
         * isn't safe anyway and we play this only for the followup
         * value, typically as a ko threat.
	 *
	 * FIXME: This is somewhat halfhearted since only one defense
	 * move is tested.
	 */
	if (board[aa] != EMPTY
	    && (move[pos].move_safety == 1
		|| adjacent_to_nondead_stone(pos, color))
	    && find_defense(aa, &defense_move) == WIN
	    && defense_move != NO_MOVE) {
	  if (trymove(defense_move, other,
		      "estimate_territorial_value-b", NO_MOVE,
		      EMPTY, NO_MOVE)) {
	    if (board[pos] == EMPTY || attack(pos, NULL) == WIN) {
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
	      && dragon[adj].status == DEAD
	      && 2*dragon[adj].effective_size > adjustment_up)
	    adjustment_up = 2*dragon[adj].effective_size;
	  if (dragon[adj].color == color
	      && attack(adj, NULL)
	      && 2*worm[adj].effective_size > adjustment_down)
	    adjustment_down = 2*worm[adj].effective_size;
	}

	popgo();

	/* No followup if the string is not substantial. */
	{
	  int save_verbose = verbose;
	  if (verbose > 0)
	    verbose --;
	  if (move[pos].move_safety == 0
	      && !owl_substantial(aa)) {
	    verbose = save_verbose;
	    break;
	  }
	  verbose = save_verbose;
	}
	
	adjusted_value += adjustment_up;
	adjusted_value -= adjustment_down;
	if (adjusted_value > 0.0) {
	  add_followup_value(pos, adjusted_value);
	  /* Inside trymove, so don't re-indented.*/
	  TRACE("%1m:   %f (followup) - threatens to capture %1m\n",
		pos, adjusted_value, aa);
	}
      }
      break;

    case DEFEND_THREAT:
      aa = move_reasons[r].what;

      /* Make sure this is a threat to defend our stones. */
      ASSERT1(board[aa] == color, aa);
      
      /* No followup value if string can be attacked with threat
       * against our move. An exception to this is when our move
       * isn't safe anyway and we play this only for the followup
       * value, typically as a ko threat.
       *
       * FIXME: This is somewhat halfhearted since only one attack
       * move is tested.
       */
      if (trymove(pos, color, "estimate_territorial_value-A",
		  NO_MOVE, EMPTY, NO_MOVE)) {
	int attack_move;
	if (move[pos].move_safety == 1
	    && attack(aa, &attack_move) == WIN
	    && attack_move != NO_MOVE) {
	  if (trymove(attack_move, other,
		      "estimate_territorial_value-b", NO_MOVE,
		      EMPTY, NO_MOVE)) {
	    if (board[pos] == EMPTY || attack(pos, NULL) == WIN) {
	      popgo();
	      popgo();
	      break;
	    }
	    popgo();
	  }
	}
	popgo();
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
    case EXPAND_MOYO_MOVE:
    case EXPAND_TERRITORY_MOVE:
    case INVASION_MOVE:
      does_block = 1;
      break;
      
    case SEMEAI_MOVE:
      aa = move_reasons[r].what;
      
      this_value = 2 * dragon[aa].effective_size;
      TRACE("  %1m: %f - semeai involving %1m\n", pos, this_value, aa);
      tot_value += this_value;
      break;
      
    case SEMEAI_THREAT:
      aa = move_reasons[r].what;

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
    case OWL_ATTACK_MOVE_GAIN:
    case OWL_DEFEND_MOVE:
    case OWL_DEFEND_MOVE_GOOD_KO:
    case OWL_DEFEND_MOVE_BAD_KO:
    case OWL_DEFEND_MOVE_LOSS:

      if (move_reasons[r].type == OWL_ATTACK_MOVE_GAIN
	  || move_reasons[r].type == OWL_DEFEND_MOVE_LOSS) {
        aa = either_data[move_reasons[r].what].what1;
        bb = either_data[move_reasons[r].what].what2;
      } 
      else {
	aa = move_reasons[r].what;
	bb = NO_MOVE;
      }

      /* If the dragon is a single ko stone, the owl code currently
       * won't detect that the owl attack is conditional. As a
       * workaround we deduct 0.5 points for the move here, but only
       * if the move is a liberty of the string.
       */
      if (dragon[aa].size == 1
	  && is_ko_point(aa)
	  && liberty_of_string(pos, aa)) {
	TRACE("  %1m: -0.5 - penalty for ko stone %1m (workaround)\n",
	      pos, aa);
	tot_value -= 0.5;
      }

      /* Mark the affected dragon for use in the territory analysis. */
      mark_changed_dragon(pos, color, aa, bb, move_reasons[r].type,
	  		  safe_stones, strength, &this_value);
      this_value *= 2.0;

      TRACE("  %1m: owl attack/defend for %1m\n", pos, aa);
      
      /* FIXME: How much should we reduce the value for ko attacks? */
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
      else if (move_reasons[r].type == OWL_ATTACK_MOVE_GAIN
	       || move_reasons[r].type == OWL_DEFEND_MOVE_LOSS) {
	this_value = 0.0;
      }
      
      tot_value -= this_value;

      /* If the dragon is a single string which can be tactically
       * attacked, but this owl attack does not attack tactically, it
       * can be suspected to leave some unnecessary aji or even be an
       * owl misread. Therefore we give it a small penalty to favor
       * the moves which do attack tactically as well.
       *
       * One example is manyfaces:2 where the single stone S15 can be
       * tactically attacked at S16 but where 3.3.2 finds additional
       * owl attacks at R14 (clearly ineffective) and T15 (might work,
       * but leaves huge amounts of aji).
       */
      if ((move_reasons[r].type == OWL_ATTACK_MOVE
	   || move_reasons[r].type == OWL_ATTACK_MOVE_GOOD_KO
	   || move_reasons[r].type == OWL_ATTACK_MOVE_BAD_KO)
	  && dragon[aa].size == worm[aa].size
	  && worm[aa].attack_codes[0] == WIN
	  && attack_move_reason_known(pos, aa) != WIN) {
	this_value = 0.05 * (2 * worm[aa].effective_size);
	TRACE("  %1m: -%f - suspected ineffective owl attack of worm %1m\n",
	      pos, this_value, aa);
	tot_value -= this_value;
      }
      
      does_block = 1;
      break;

    case OWL_ATTACK_THREAT:
      aa = move_reasons[r].what;

      if (dragon[aa].status == DEAD) {
	TRACE_MOVE_REASONS(
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
	      && dragon[adj].status == CRITICAL
	      && dragon2[d].safety != INESSENTIAL
	      && !owl_defense_move_reason_known(pos, adj))
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
      aa = move_reasons[r].what;

      add_followup_value(pos, 2 * dragon[aa].effective_size);
      TRACE("  %1m: %f (followup) - threatens to owl defend %1m\n",
	    pos, 2 * dragon[aa].effective_size, aa);
      break;

    case OWL_PREVENT_THREAT:
      /* A move attacking a dragon whose defense can be threatened.
       */
      aa = move_reasons[r].what;

      /* If the opponent just added a stone to a dead dragon, then
       * attack it. If we are ahead, add a safety move here, at most
       * half the margin of victory.
       *
       * This does not apply if we are doing scoring.
       */
      if (!doing_scoring
	  && is_same_dragon(get_last_opponent_move(color), aa)) {
	this_value = 1.5 * dragon[aa].effective_size;
	TRACE("  %1m: %f - attack last move played, although it seems dead\n",
	      pos, this_value);
	tot_value += this_value;
      }
      else if (!doing_scoring && ((color == BLACK && score < 0.0)
				  || (color == WHITE && score > 0.0))) {
	/* tm - devalued this bonus (3.1.17) */
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
      /* Add 1.0 to compensate for -1.0 penalty because the move is
       * thought to be a sacrifice.
       */
      this_value = move_reasons[r].what + 1.0;
      tot_value += this_value;
      TRACE("  %1m: %f - combination attack kills one of several worms\n",
	    pos, this_value);
      break;
      
    case YOUR_ATARI_ATARI_MOVE:
      this_value = move_reasons[r].what;
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

  if (move[pos].move_safety == 1 && safe_move(pos, color) == WIN) {
    safe_stones[pos] = INFLUENCE_SAVED_STONE;
    strength[pos] = DEFAULT_STRENGTH;
  }
  else {
    safe_stones[pos] = 0;
    strength[pos] = 0.0;
  }
  
  /* tm added move_safety check (3.1.22) (see trevorc:880) */
  if (does_block && move[pos].move_safety
      && tryko(pos, color, "estimate_territorial_value", EMPTY, NO_MOVE)) {
    if (!retrieve_delta_territory_cache(pos, color, &this_value, 
	  				&move[pos].influence_followup_value)) {
      compute_influence(OTHER_COLOR(color), safe_stones, strength, 
	  		&move_influence, pos, "after move");
      compute_followup_influence(&move_influence, &followup_influence,
	  			 pos, "followup");
      this_value = influence_delta_territory(OPPOSITE_INFLUENCE(color),
	   				     &move_influence, color, pos);
      move[pos].influence_followup_value
	= influence_delta_territory(&move_influence, &followup_influence,
	    			    color, pos);
      store_delta_territory_cache(pos, color, this_value,
	 			  move[pos].influence_followup_value);	
    }
    popgo();
					   
    if (this_value != 0.0)
      TRACE("  %1m: %f - change in territory\n", pos, this_value);
    else
      TRACE_MOVE_REASONS("  %1m: 0.00 - change in territory\n", 
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
  
  float this_value = 0.0;
  float tot_value = 0.0;

  /* Strategical value of connecting or cutting dragons. */
  float dragon_value[BOARDMAX];

  for (aa = BOARDMIN; aa < BOARDMAX; aa++)
    dragon_value[aa] = 0.0;
  
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
	aa = move_reasons[r].what;
      
	/* Defenseless stone */
	if (worm[aa].defense_codes[0] == 0)
	  break;

	/* FIXME: This is totally ad hoc, just guessing the value of
         *        potential cutting points.
	 * FIXME: When worm[aa].cutstone2 == 1 we should probably add
	 *        a followup value.
	 */
	if (worm[aa].cutstone2 > 1) {
	  double ko_factor = 1;
	  if (move_reasons[r].type == ATTACK_MOVE_GOOD_KO
	      || move_reasons[r].type == DEFEND_MOVE_GOOD_KO) {
	    ko_factor = 0.6;
	  }
	  else if (move_reasons[r].type == ATTACK_MOVE_BAD_KO
		   || move_reasons[r].type == DEFEND_MOVE_BAD_KO) {
	    ko_factor = 0.4;
	  }
	  this_value = 10.0 * (worm[aa].cutstone2 - 1) * ko_factor;
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
	  if (lunch_worm[l] == aa) {
	    bb = lunch_dragon[l];

	    /* FIXME: This value cannot be computed without some measurement
	     * of how the actual move affects the dragon.  The dragon safety
	     * alone is not enough. The question is whether the dragon is
	     * threatened or defended by the move or not.  
	     */
	    this_value = 1.8 * soft_cap(dragon[bb].effective_size, 15.0)
			 * dragon_weakness(bb, 0);

	    /* If this dragon consists of only one worm and that worm
	     * can be tactically captured or defended by this move, we
	     * have already counted the points as territorial value,
	     * unless it's assumed to be dead.
	     */
	    if (dragon[bb].status != DEAD
		&& dragon[bb].size == worm[bb].size
		&& (attack_move_reason_known(pos, bb)
		    || defense_move_reason_known(pos, bb)))
	      this_value = 0.0;

	    /* If this dragon can be tactically attacked and the move
             * does not defend or attack, no points.
	     */
	    if (worm[bb].attack_codes[0] != 0
		&& ((color == board[bb] && !does_defend(pos, bb))
		    || (color == OTHER_COLOR(board[bb])
			&& !does_attack(pos, bb))))
	      this_value = 0.0;

	    /* If we are doing scoring, are alive, and the move loses
             * territory, no points.
	     */
	    if (doing_scoring
		&& move[pos].territorial_value < 0.0
		&& (DRAGON2(bb).safety == ALIVE
		    || DRAGON2(bb).safety == STRONGLY_ALIVE
		    || DRAGON2(bb).safety == INVINCIBLE))
	      this_value = 0.0;
	    
	    if (this_value > dragon_value[bb]) {
	      TRACE_MOVE_REASONS(
		    "  %1m:   %f - %1m attacked/defended\n",
		    pos, this_value, bb);
	      dragon_value[bb] = this_value;
	    }
	  }

	break;
	
      case ATTACK_THREAT:
      case DEFEND_THREAT:
        break;

      case EITHER_MOVE:
	/* FIXME: Generalize this to more types of threats. */
	/* FIXME: We need a policy if a move has several EITHER_MOVE
	 * reasons. Most likely not all of them can be achieved.
	 */
	aa = either_data[move_reasons[r].what].what1;
	bb = either_data[move_reasons[r].what].what2;

	/* If both worms are dead, this move reason has no value. */
	if (dragon[aa].status == DEAD 
	    && dragon[bb].status == DEAD)
	  break;

	/* Also if there is a combination attack, we assume it covers
	 * the same thing.
	 * FIXME: This is only applicable as long as the only moves
	 *        handled by EITHER_MOVE are attacks.
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
	
      case ALL_MOVE:
	/* FIXME: Generalize this to more types of threats. */
	aa = all_data[move_reasons[r].what].what1;
	bb = all_data[move_reasons[r].what].what2;

	/* If both worms are dead, this move reason has no value. */
	if (dragon[aa].status == DEAD 
	    && dragon[bb].status == DEAD)
	  break;

	/* Also if there is a combination attack, we assume it covers
	 * the same thing.
	 */
	if (move_reason_known(pos, YOUR_ATARI_ATARI_MOVE, -1))
	  break;

	aa_value = worm[aa].effective_size;
	bb_value = worm[bb].effective_size;
	this_value = 2 * gg_min(aa_value, bb_value);

	TRACE("  %1m: %f - both defends %1m (%f) and defends %1m (%f)\n",
	      pos, this_value, aa, aa_value, bb, bb_value);

	tot_value += this_value;
	break;
	
      case CONNECT_MOVE:

	/* If the opponent just added a stone to a dead dragon, which is
	 * adjacent to both dragons being connected, then the connection
	 * is probably a good way to make sure the thrashing dragon
	 * stays dead. If we are ahead, add a safety move here, at most
	 * half the margin of victory.
	 *
	 * This does not apply if we are doing scoring.
	 *
	 * FIXME: The margin of victory limit is not implemented.
	 */
      
	if (!doing_scoring) {
	  int cc;
	  aa = dragon[conn_worm1[move_reasons[r].what]].origin;
	  bb = dragon[conn_worm2[move_reasons[r].what]].origin;
	  cc = get_last_opponent_move(color);
	  
	  if (cc != NO_MOVE
	      && dragon[cc].status == DEAD
	      && are_neighbor_dragons(aa, cc)
	      && are_neighbor_dragons(bb, cc)) {
	    if (aa == bb)
	      this_value = 1.6 * dragon[cc].effective_size;
	    else if (DRAGON2(aa).safety == INESSENTIAL
		     || DRAGON2(bb).safety == INESSENTIAL)
	      this_value = 0.8 * dragon[cc].effective_size;
	    else
	      this_value = 1.7 * dragon[cc].effective_size;
	    
	    if (this_value > dragon_value[dragon[cc].origin]) {
	      dragon_value[dragon[cc].origin] = this_value;
	      TRACE_MOVE_REASONS(
		    "  %1m:   %f - connect %1m and %1m to attack thrashing dragon %1m\n",
		    pos, this_value, aa, bb, cc);
	    }
	  }
	}

	if (!move[pos].move_safety)
	  break;
	/* Otherwise fall through. */
      case CUT_MOVE:
	if (doing_scoring && !move[pos].move_safety)
	  break;

	aa = dragon[conn_worm1[move_reasons[r].what]].origin;
	bb = dragon[conn_worm2[move_reasons[r].what]].origin;

	if (aa == bb)
	  continue;

	/* If we are ahead by more than 20, value connections more strongly */
	if ((color == WHITE && score > 20.0)
	    || (color == BLACK && score < -20.0))
	  this_value = connection_value(aa, bb, pos, gg_abs(score));
	else
	  this_value = connection_value(aa, bb, pos, 0);
	if (this_value > dragon_value[aa]) {
	  dragon_value[aa] = this_value;
          TRACE_MOVE_REASONS(
		"  %1m:   %f - %1m cut/connect strategic value\n",
		pos, this_value, aa);
	}

	
	if ((color == WHITE && score > 20.0)
	    || (color == BLACK && score < -20.0))
	  this_value = connection_value(bb, aa, pos, gg_abs(score));
	else
	  this_value = connection_value(bb, aa, pos, 0);
	if (this_value > dragon_value[bb]) {
	  dragon_value[bb] = this_value;
          TRACE_MOVE_REASONS(
		"  %1m:   %f - %1m cut/connect strategic value\n",
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
	aa = move_reasons[r].what;

	/* FIXME: This value cannot be computed without some
	 * measurement of how the actual move affects the dragon. The
	 * dragon safety alone is not enough. The question is whether
	 * the dragon is threatened by the move or not.
	 */
	this_value = 1.8 * soft_cap(dragon[aa].effective_size, 15.0)
		     * dragon_weakness(aa, 1);

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
		&& dragon[adj].status == CRITICAL
		&& dragon2[d].safety != INESSENTIAL
		&& !owl_defense_move_reason_known(pos, adj))
	      this_value = 0.0;
	  }
	}
		
	if (this_value > dragon_value[aa]) {
	  dragon_value[aa] = this_value;
          TRACE_MOVE_REASONS(
		"  %1m:   %f - %1m strategic attack/defend\n",
		pos, this_value, aa);

	}
	break;

      case UNCERTAIN_OWL_DEFENSE:
	aa = move_reasons[r].what;
	
	/* If there is an adjacent dragon which is critical we should
	 * skip this type of move reason, since attacking or defending
	 * the critical dragon is more urgent.
	 */
	{
	  int d;
	  int found_one = 0;
	  
	  for (d = 0; d < DRAGON2(aa).neighbors; d++)
	    if (DRAGON(DRAGON2(aa).adjacent[d]).status == CRITICAL)
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
	  this_value = gg_min(2*dragon[aa].effective_size, gg_abs(0.65*score));
	
	if (this_value > dragon_value[aa]) {
	  dragon_value[aa] = this_value;
	  TRACE_MOVE_REASONS(
		"  %1m:   %f - %1m uncertain owl defense bonus\n",
		pos, this_value, aa);
	}

	break;
    }
  }
  
  for (aa = BOARDMIN; aa < BOARDMAX; aa++) {
    if (dragon_value[aa] == 0.0)
      continue;

    ASSERT1(dragon[aa].origin == aa, aa);

    /* If this dragon is critical but not attacked/defended by this
     * move, we ignore the strategic effect.
     */
    if (dragon[aa].status == CRITICAL
	&& !owl_move_reason_known(pos, aa)) {
      TRACE_MOVE_REASONS("  %1m: 0.0 - disregarding strategic effect on %1m (critical dragon)\n",
	    pos, aa);
      continue;
    }
    
    /* If this dragon consists of only one worm and that worm can
     * be tactically captured or defended by this move, we have
     * already counted the points as territorial value, unless
     * it's assumed to be dead.
     */
    if (dragon[aa].status != DEAD
	&& dragon[aa].size == worm[aa].size
	&& (attack_move_reason_known(pos, aa)
	    || defense_move_reason_known(pos, aa))) {
      TRACE("  %1m:   %f - %1m strategic value already counted - A.\n",
	    pos, dragon_value[aa], aa);
      continue;
    }
    /* If the dragon has been owl captured, owl defended, or involved
     * in a semeai, we have likewise already counted the points as
     * territorial value.
     */
    if (owl_attack_move_reason_known(pos, aa)
	|| owl_defense_move_reason_known(pos, aa)
	|| move_reason_known(pos, SEMEAI_MOVE, aa)) {
      /* But if the strategical value was larger than the territorial
       * value (e.g. because connecting to strong dragon) we award the
       * excess value as a bonus.
       */
      float excess_value = (dragon_value[aa] - 
			    2 * dragon[aa].effective_size);
      if (excess_value > 0.0) {
	TRACE("  %1m: %f - strategic bonus for %1m\n", pos, excess_value, aa);
	tot_value += excess_value;
      }
      else {
	TRACE("  %1m:   %f - %1m strategic value already counted - B.\n",
	      pos, dragon_value[aa], aa);
      }
      
      continue;
    }

    TRACE("  %1m: %f - strategic effect on %1m\n",
	  pos, dragon_value[aa], aa);
    tot_value += dragon_value[aa];
  }

  /* Finally, subtract penalty for invasion type moves. */
  this_value = strategic_penalty(pos, color);
  if (this_value > 0.0) {
    TRACE("  %1m: %f - strategic penalty, considered as invasion.\n",
	  pos, -this_value);
    tot_value -= this_value;
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
    while (move[pos].reason[num_reasons] >= 0 && num_reasons < MAX_REASONS)
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

  /* Avoid rounding anomalies from the territorial valuation. */
  if (tot_value < 0.001 && tot_value > -0.001)
    tot_value = 0.0;
  
  if (tot_value > 0.0) {
    int c;
    float followup_value;

    followup_value = move[pos].followup_value
		     + move[pos].influence_followup_value;
    TRACE("  %1m:   %f - total followup value, added %f as territorial followup\n",
	  pos, followup_value, move[pos].influence_followup_value);

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
      contribution = gg_min(gg_min(0.5 * followup_value
				   + 0.5 * move[pos].reverse_followup_value,
				   1.0 * tot_value
				   + followup_value),
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
	      pos, contribution, followup_value,
	      move[pos].reverse_followup_value);
      }

      /* If a ko fight is going on, we should use the full followup
       * and reverse followup values in the total value. We save the
       * additional contribution for later access.
       */
      move[pos].additional_ko_value =
	followup_value 
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

    /* Add a special shape bonus for moves which connect own strings
     * or cut opponent strings.
     */
    c = (move_connects_strings(pos, color)
	 + move_connects_strings(pos, OTHER_COLOR(color)));
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
      && value_moves_confirm_safety(pos, color)) {
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
  int m, n;
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
	move[pos].value += 
	  0.01 * move[pos].random_number * move[pos].randomness_scaling;
      }
      else {
	move[pos].value = 0.0;
	TRACE("Move at %1m wasn't legal.\n", pos);
      }
    }
}


/* Search through all board positions for the 10 highest valued
 * moves and print them.
 */

static void
print_top_moves(void)
{
  int k;
  int pos;
  float tval;
  
  for (k = 0; k < 10; k++) {
    best_moves[k] = NO_MOVE;
    best_move_values[k] = 0.0;
  }
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (!ON_BOARD(pos) || move[pos].final_value <= 0.0)
      continue;
      
    tval = move[pos].final_value;
    record_top_move(pos, tval);
  }
  
  if (verbose > 0 || (debug & DEBUG_TOP_MOVES)) {
    gprintf("\nTop moves:\n");
    for (k = 0; k < 10 && best_move_values[k] > 0.0; k++)
      gprintf("%d. %1M %f\n", k+1, best_moves[k], best_move_values[k]);
  }
}

/* Add a move to the list of top moves (if it is among the top ten) */

void
record_top_move(int move, float val)
{
  int k;
  for (k = 9; k >= 0; k--)
    if (val > best_move_values[k]) {
      if (k < 9) {
	best_move_values[k+1] = best_move_values[k];
	best_moves[k+1] = best_moves[k];
      }
      best_move_values[k] = val;
      best_moves[k] = move;
    }
}

/* remove a rejected move from the list of top moves */

void
remove_top_move(int move)
{
  int k;
  for (k = 0; k < 10; k++) {
    if (best_moves[k] == move) {
      int l;
      for (l = k; l < 9; l++) {
	best_moves[l] = best_moves[l+1];
	best_move_values[l] = best_move_values[l+1];
      }
      best_moves[9] = NO_MOVE;
      best_move_values[9] = 0.0;
    }
  }
}

/* This function is called if the biggest move on board was an illegal
 * ko capture.
 * FIXME: We need a check here whether the threat still works after
 * the opponent fills in the ko (or resolves it in another way.)
 */
static void
reevaluate_ko_threats(int ko_move, int color)
{
  int ko_stone = NO_MOVE;
  int opp_ko_move;
  int m, n;
  int pos;
  int k;
  int type, what;
  int threat_does_work = 0;
  int ko_move_target;
  float threat_size;

  ko_move_target = get_biggest_owl_target(ko_move);
  
  /* Find the ko stone. */
  for (k = 0; k <= 3; k++) {
    ko_stone = ko_move + delta[k];
    if (ON_BOARD(ko_stone) && countlib(ko_stone) == 1)
      break;
  }
  ASSERT_ON_BOARD1(ko_stone);
  
  TRACE("Reevaluating ko threats.\n");
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      pos = POS(m, n);
      if (pos == ko_move)
        continue;
      if (move[pos].additional_ko_value <= 0.0) 
        continue;

      /* Otherwise we look for the biggest threat, and then check whether
       * it still works after ko has been resolved.
       */
      threat_size = 0.0;
      type = -1;
      what = -1;
      for (k = 0; k < MAX_REASONS; k++) {
        int r = move[pos].reason[k];
        if (r < 0)
          break;
        if (!(move_reasons[r].type & THREAT_BIT))
          continue;
        switch (move_reasons[r].type) {
        case ATTACK_THREAT:
        case DEFEND_THREAT:
          if (worm[move_reasons[r].what].effective_size
              > threat_size) {
            threat_size = worm[move_reasons[r].what].effective_size;
            type = move_reasons[r].type;
            what = move_reasons[r].what;
          }
          break;
        case OWL_ATTACK_THREAT:
        case OWL_DEFEND_THREAT:   
        case SEMEAI_THREAT:
          if (dragon[move_reasons[r].what].effective_size
              > threat_size) {
            threat_size = dragon[move_reasons[r].what]\
                          .effective_size;
            type = move_reasons[r].type;
            what = move_reasons[r].what;
          }
          break;
        default:
          /* This means probably someone has introduced a new threat type
           * without adding the corresponding case above.
           */
          gg_assert(0);
          break;
        }
      } 
      /* If there is no threat recorded, the followup value is probably
       * contributed by a pattern. We can do nothing but accept this value.
       * (although this does cause problems).
       */
      if (type == -1)
        threat_does_work = 1;
      else
        if (trymove(pos, color, "reevaluate_ko_threats", ko_move,
                    EMPTY, ko_move)) {
	  ASSERT_ON_BOARD1(ko_stone);
          if (!find_defense(ko_stone, &opp_ko_move))
            threat_does_work = 1;
          else {
            if (trymove(opp_ko_move, OTHER_COLOR(color),
                        "reevaluate_ko_threats", ko_move, EMPTY, NO_MOVE)) {
              switch (type) {
              case ATTACK_THREAT:
                threat_does_work = attack(what, NULL);
                break;
              case DEFEND_THREAT:
                threat_does_work = (board[what] != EMPTY
                                    && find_defense(what, NULL));
                break;
              case OWL_ATTACK_THREAT:
              case OWL_DEFEND_THREAT:
                /* Should we call do_owl_attack/defense here?
                 * Maybe too expensive? For the moment we just assume
                 * that the attack does not work if it concerns the
                 * same dragon as ko_move. (Can this really happen?)
                 */
                threat_does_work = (ko_move_target != what);
              }
              popgo();
            }
          }
          popgo();
        }
 
      if (threat_does_work) {
	TRACE("%1m: %f + %f = %f\n", pos, move[pos].value,
	      move[pos].additional_ko_value,
	      move[pos].value + move[pos].additional_ko_value);
	move[pos].value += move[pos].additional_ko_value;
      }
      else
        TRACE_MOVE_REASONS(
              "%1m: no additional ko value (threat does not work as ko threat)\n", pos);
    }
}


/* Redistribute points. When one move is declared a replacement for
 * another by a replacement move reason, the move values for the
 * inferior move are transferred to the replacement.
 */
static void
redistribute_points(void)
{
  int source;
  int target;

  for (target = BOARDMIN; target < BOARDMAX; target++)
    if (ON_BOARD(target))
      move[target].final_value = move[target].value;
  
  for (source = BOARDMIN; source < BOARDMAX; source++) {
    if (!ON_BOARD(source))
      continue;
    target = replacement_map[source];
    if (target == NO_MOVE)
      continue;

    TRACE("Redistributing points from %1m to %1m.\n", source, target);
    if (move[target].final_value < move[source].final_value) {
      TRACE("%1m is now valued %f.\n", target, move[source].final_value);
      move[target].final_value = move[source].final_value;
    }
    TRACE("%1m is now valued 0.\n", source);
    move[source].final_value = 0.0;
  }
}

/* This selects the best move available according to their valuations.
 * If the best move is an illegal ko capture, we add ko threat values.
 * If the best move is a blunder, it gets devalued and continue to look
 * for the best move.
 */
static int
find_best_move(int *the_move, float *val, int color,
	       int allowed_moves[BOARDMAX])
{
  int good_move_found = 0;
  int ko_values_have_been_added = 0;
  char blunder_tested[BOARDMAX];
  float bestval = 0.0;
  int best_move = NO_MOVE;
  int pos;

  memset(blunder_tested, 0, sizeof(blunder_tested));

  while (!good_move_found) {
    bestval = 0.0;
    best_move = NO_MOVE;

    /* Search through all board positions for the highest valued move. */
    for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
      float tval = move[pos].final_value;
      if (allowed_moves && !allowed_moves[pos])
	continue;
      if (!ON_BOARD(pos) || move[pos].final_value == 0.0)
	continue;
	
      if (tval > bestval) {
	if (is_legal(pos, color) || is_illegal_ko_capture(pos, color)) {
	  bestval = tval;
	  best_move = pos;
	}
	else {
	  TRACE("Move at %1m would be suicide.\n", pos);
	  remove_top_move(pos);
	  move[pos].value = 0.0;
	  move[pos].final_value = 0.0;
	}
      }
    }
    
    /* If the best move is an illegal ko capture, reevaluate ko
     * threats and search again.
     */
    if (bestval > 0.0 && is_illegal_ko_capture(best_move, color)) {
      TRACE("Move at %1m would be an illegal ko capture.\n", best_move);
      reevaluate_ko_threats(best_move, color);
      redistribute_points();
      time_report(2, "  reevaluate_ko_threats", NO_MOVE, 1.0);
      ko_values_have_been_added = 1;
      remove_top_move(best_move);
      move[best_move].value = 0.0;
      move[best_move].final_value = 0.0;
      print_top_moves();
      good_move_found = 0;
    }
    /* Call blunder_size() to check that we're not about to make a
     * blunder. Otherwise devalue this move and scan through all move
     * values once more.
     */
    else if (bestval > 0.0) {
      if (!blunder_tested[best_move]) {
	float blunder_size = value_moves_get_blunder_size(best_move, color);
	if (blunder_size > 0.0) {
	  TRACE("Move at %1m is a blunder, subtracting %f.\n", best_move,
		blunder_size);
	  remove_top_move(best_move);
	  move[best_move].value -= blunder_size;
	  move[best_move].final_value -= blunder_size;
	  TRACE("Move at %1m is now valued %f.\n", best_move,
		move[best_move].final_value);
	  record_top_move(best_move, move[best_move].final_value);
	  good_move_found = 0;
	  blunder_tested[best_move] = 1;
	}
	else
	  good_move_found = 1; /* Best move was not a blunder. */
      }
      else /* The move apparently was a blunder, but still the best move. */
	good_move_found = 1;
    }
    else
      good_move_found = 1; /* It's best to pass. */
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
 * Review the move reasons to find which (if any) move we want to play.
 *
 * The parameter pure_threat_value is the value assigned to a move
 * which only threatens to capture or kill something. The reason for
 * playing these is that the move may be effective because we have
 * misevaluated the dangers or because the opponent misplays.
 *
 * The array allowed_moves restricts which moves may be considered. If
 * NULL any move is allowed.
 */
int
review_move_reasons(int *the_move, float *val, int color,
		    float pure_threat_value, float score,
		    int allowed_moves[BOARDMAX])
{
  int save_verbose;
  
  start_timer(2);
  if (!urgent || allpats) {
    find_more_attack_and_defense_moves(color);
    time_report(2, "  find_more_attack_and_defense_moves", NO_MOVE, 1.0);
  }

  save_verbose = verbose;
  if (verbose > 0)
    verbose--;
  if (level >= 6) {
    find_more_owl_attack_and_defense_moves(color);
    time_report(2, "  find_more_owl_attack_and_defense_moves", NO_MOVE, 1.0);
  }
  verbose = save_verbose;

  if (verbose > 0)
    verbose--;
  examine_move_safety(color);
  time_report(2, "  examine_move_safety", NO_MOVE, 1.0);
  verbose = save_verbose;

  /* We can't do this until move_safety is known. */
  induce_secondary_move_reasons(color);
  time_report(2, "  induce_secondary_move_reasons", NO_MOVE, 1.0);
    
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

  /* Select the highest valued move and return it. */
  return find_best_move(the_move, val, color, allowed_moves);
}


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
