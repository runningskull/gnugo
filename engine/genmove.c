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
#include "sgftree.h"

/* Return one if x doesn't equal position_number and 0 otherwise.
 * After using this macro x will always have the value
 * position_number.
 */
#define NEEDS_UPDATE(x) (x != position_number ? (x = position_number, 1) : 0)

static int get_level(int *level);
static int do_genmove(int *i, int *j, int color, float pure_threat_value);

static double slowest_time = 0.;
static int slowest_i = -1;
static int slowest_j = -1;
static int slowest_movenum = 0;
static double total_time = 0.;

void sgfShowConsideredMoves(void);


/* Reset some things in the engine. 
 *
 * This prepares the hash table for the reading code for use.  It
 * should be called when we start examine a new position.  
 */

void
reset_engine()
{
  /* Initialize things for hashing of positions. */
  reading_cache_clear();
  hashdata_recalc(&hashdata, p, board_ko_i, board_ko_j);

  /* Prepare our table of move reasons. */
  clear_move_reasons();

  /* Set up depth values. Level 10 should be more or less the same as level 9
   * except that the superstring technology is used in the reading code. This
   * makes us about 10% slower.
   */
  set_depth_values(level);
}

/*
 * Examine the position and try to gather as much information as possible.
 * This is used mainly for move generation, but could also be called
 * for debugging purposes (decidestring, etc).
 *
 * The parameter how_much tells us how much of the work we have to do.
 * For move generation we have to do it all.  For debugging we can 
 * sometimes stop a little earlier.
 */

void
examine_position(int color, int how_much)
{
  int save_verbose;
  int m, n;
  double t1 = 0., t2 = 0.;

  /* Position numbers for which various examinations were last made. */
  static int worms_examined = -1;
  static int initial_influence_examined = -1;
  static int dragons_examined_without_owl = -1;
  static int dragons_examined = -1;
  static int owl_reasons_examined = -1;
  static int initial_influence2_examined = -1;

  purge_persistent_reading_cache();
  
  /* Don't print reading traces during make_worms and make_dragons unless 
   * the user really wants it (verbose == 3). 
   */
  save_verbose = verbose;
  if ((verbose == 1) || (verbose == 2))
    --verbose;

  if (NEEDS_UPDATE(worms_examined)) {
    if (showtime)
      t1 = gg_gettimeofday();
    make_worms();
    if (showtime) {
      t2 = gg_gettimeofday();
      fprintf(stderr, "  make worms: %.2f sec\n", t2-t1);
    }
  }
  if (how_much == EXAMINE_WORMS) {
    verbose = save_verbose;
    return;
  }

  if (stones_on_board(BLACK | WHITE) != 0) {
    if (NEEDS_UPDATE(initial_influence_examined))
      compute_initial_influence(color, 0);
    if (how_much == EXAMINE_INITIAL_INFLUENCE) {
      verbose = save_verbose;
      return;
    }

    if (how_much == EXAMINE_DRAGONS_WITHOUT_OWL) {
      if (NEEDS_UPDATE(dragons_examined_without_owl))
	make_dragons(color, 1);
      verbose = save_verbose;
      return;
    }
    
    if (NEEDS_UPDATE(dragons_examined)) {
      make_dragons(color, 0);
      /* We have automatically done a partial dragon analysis as well. */
      dragons_examined_without_owl = position_number;
    }
    if (how_much == EXAMINE_DRAGONS) {
      verbose = save_verbose;
      return;
    }

    if (NEEDS_UPDATE(owl_reasons_examined))
      owl_reasons(color);
    if (how_much == EXAMINE_OWL_REASONS) {
      verbose = save_verbose;
      return;
    }
  }
  else if (how_much == EXAMINE_INITIAL_INFLUENCE
	   || how_much == EXAMINE_DRAGONS
	   || how_much == EXAMINE_OWL_REASONS) {
    verbose = save_verbose;
    return;
  }
  
  verbose = save_verbose;

  if (printworms) {
    show_dragons();
  }
  else if (verbose) {
    for (m = 0; m < board_size; m++)
      for (n = 0; n < board_size; n++)
	if (p[m][n]
	    && worm[m][n].origini == m
	    && worm[m][n].originj == n
	    && worm[m][n].cutstone) {
	  TRACE("%scutting stones found at %m\n",
		worm[m][n].cutstone==2 ? "potential " : "", m,n); 
	}
  }	  

  if (NEEDS_UPDATE(initial_influence2_examined))
    compute_initial_influence(color, 1);
  if (how_much == EXAMINE_INITIAL_INFLUENCE2)
    return;
}


/*
 * Estimate the current score using the influence function. The value
 * is positive if white is thought to be ahead and negative otherwise.
 *
 * The color is the color to move. We only need this for ugly
 * technical reasons and it shouldn't have any effect on the score
 * estimate.
 */
void
old_estimate_score(int color, float *lower_bound, float *upper_bound)
{
  static int last_scored_position = -1;
  static float lower = 0.0;
  static float upper = 0.0;
  float score1;
  float score2;

  if (position_number != last_scored_position) {
    /* Find out information about the worms and dragons. */
    examine_position(color, EXAMINE_ALL);
    compute_initial_influence(OTHER_COLOR(color), 1);
    /* The coefficients should match those close to the end of
     * estimate_influence_value() in move_reasons.c.
     */
    score1 = influence_estimate_score(0.35, 0.13);
    compute_initial_influence(color, 1);
    score2 = influence_estimate_score(0.35, 0.13);
    
    DEBUG(DEBUG_SCORING, "%f %f %d %d\n", score1, score2,
	  white_captured, black_captured);

    if (score1 > score2) {
      lower = score2;
      upper = score1;
    }
    else {
      lower = score1;
      upper = score2;
    }

    lower += black_captured - white_captured;
    upper += black_captured - white_captured;
 
    last_scored_position = position_number;
  }

  *lower_bound = lower + komi;
  *upper_bound = upper + komi;
}



/* 
 * Generate computer move for COLOR.
 *
 * Return the generated move in (*i, *j).
 */

int
genmove(int *i, int *j, int color)
{
  return do_genmove(i, j, color, 0.4);
}


/* 
 * Same as above but doesn't generate pure threat moves. Useful when
 * trying to score a game.
 */

int
genmove_conservative(int *i, int *j, int color)
{
  return do_genmove(i, j, color, 0.0);
}



/* 
 * Perform the actual move generation. 
 */
  
static int
do_genmove(int *i, int *j, int color, float pure_threat_value)
{
  float val;
  double t1 = 0., t2 = 0.;
  double t3 = 0., t4 = 0.;

  if (showtime)
    t1 = gg_gettimeofday();
  
  /* Prepare our table of moves considered. */
  memset(potential_moves, 0, sizeof(potential_moves));
  
  /* Reset all the statistics for each move. */
  stats.nodes = 0;
  stats.position_entered    = 0;
  stats.position_hits       = 0;
  stats.read_result_entered = 0;
  stats.read_result_hits    = 0;
  stats.hash_collisions     = 0;
  
  /* no move is found yet. */
  *i = -1;  
  *j = -1;  
  val = -1; 
  if (get_level(&level))
    fprintf(stderr, "level = %d\n", level);
  
  /* experimental level adapter */
  clock_adapt_level(&level, color);

  /* Prepare pattern matcher and reading code. */
  reset_engine();

  /* Find out information about the worms and dragons. */
  if (showtime)
    t3 = gg_gettimeofday();
  examine_position(color, EXAMINE_ALL);
  if (showtime) {
    t4 = gg_gettimeofday();
    fprintf(stderr, "EXAMINE POSITION took %.2f sec\n", t4-t3);
    t3 = t4;
  }
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
    }
    if (showtime) {
      t4 = gg_gettimeofday();
      fprintf(stderr, "ESTIMATE SCORE took %.2f sec\n", t4-t3);
      t3 = t4;
    }
    /* The score will be used to determine when we are safely
     * ahead. So we want the most conservative score.
     */
    if (color == WHITE)
      score = lower_bound;
    else
      score = upper_bound;
  }
  else score = 0.;

  /*
   * Print some of the information if the user wants to.
   */
  /* compute_influence(); */
  if (printmoyo)
    print_moyo();
  
  if (printboard) {
    if (printboard == 1)
      fprintf(stderr,"\n          dragon_status display:\n\n");
    if (printboard == 2)
      fprintf(stderr,"\n          eye display:\n\n");
    showboard(printboard); 
    if (printboard == 1) {
      fprintf(stderr,"\n           owl_status display:\n\n");      
      showboard(3);
    }
  }
  
  gg_assert(stackp == 0);
  
  /*
   * Ok, information gathering is complete. Now start to find some moves!
   */
  
  /* Try to find empty corner moves. */
  fuseki(color);
  gg_assert(stackp == 0);

  /* Pattern matcher. */
  if (showtime)
    t3 = gg_gettimeofday();
  shapes(color);
  if (showtime) {
    t4 = gg_gettimeofday();
    fprintf(stderr, "SHAPES took %.2f sec\n", t4-t3);
    t3 = t4;
  }
  gg_assert(stackp == 0);

  /* Look for combination attacks. */
  {
    int ai, aj;
    int other = OTHER_COLOR(color);
    int aa_val;

    int save_verbose = verbose;
    if (verbose > 0)
      verbose--;

    if (save_verbose)
      gprintf("\nlooking for combination attacks ...\n");
    aa_val = atari_atari(color, &ai, &aj, save_verbose);
    if (aa_val)
      add_my_atari_atari_move(ai, aj, aa_val);
    aa_val = atari_atari(other, &ai, &aj, save_verbose);
    if (aa_val && safe_move(ai, aj, color))
      add_your_atari_atari_move(ai, aj, aa_val);
    verbose = save_verbose;
  }
  if (showtime) {
    t4 = gg_gettimeofday();
    fprintf(stderr, "ATARI ATARI took %.2f sec\n", t4-t3);
    t3 = t4;
  }

  /* Review the move reasons and estimate move values. */
  if (review_move_reasons(i, j, &val, color, 
			  pure_threat_value, lower_bound))
    TRACE("Move generation likes %m with value %f\n", *i, *j, val);
  gg_assert(stackp == 0);
  if (showtime) {
    t4 = gg_gettimeofday();
    fprintf(stderr, "REVIEW MOVE REASONS took %.2f sec\n", t4-t3);
    t3 = t4;
  }

  /* If the move value is 6 or lower, we look for endgame patterns too. */
  if (val <= 6.0 && !disable_endgame_patterns) {
    endgame_shapes(color);
    gg_assert(stackp == 0);
    if (review_move_reasons(i, j, &val, color, pure_threat_value, score))
      TRACE("Move generation likes %m with value %f\n", *i, *j, val);
    gg_assert(stackp == 0);
    if (showtime) {
      t4 = gg_gettimeofday();
      if (t4-t3 > 1.)
	fprintf(stderr, "ENDGAME took %.2f sec\n", t4-t3);
      t3 = t4;
    }
  }
  
  /* If no move found yet, revisit any semeai and change the
   * status of the opponent group from DEAD to UNKNOWN, then 
   * run shapes and endgame_shapes again. This may turn up a move.
   */
  if (val < 0.0) {
    if (revise_semeai(color)) {
      shapes(color);
      endgame_shapes(color);
      if (review_move_reasons(i, j, &val, color, pure_threat_value, score)) {
	TRACE("Upon reconsideration move generation likes %m with value %f\n",
	      *i, *j, val); 
      }
    }
    if (showtime) {
      t4 = gg_gettimeofday();
      if (t4-t3 > 1.)
	fprintf(stderr, 
		"MOVE REASONS WITH REVISED SEMEAI STATUS took %.2f sec\n", 
		t4-t3);
      t3 = t4;
    }
  }

  /* If still no move, fill a remaining liberty. This should pick up
   * all missing dame points.
   */
  if (val < 0.0 
      && fill_liberty(i, j, color)) {
    val = 1.0;
    TRACE("Filling a liberty at %m\n", *i, *j);
    move_considered(*i, *j, val);
    if (showtime) {
      t4 = gg_gettimeofday();
      fprintf(stderr, "FILL LIBERTY took %.2f sec\n", t4-t3);
      t3 = t4;
    }
  }

  /* If we're instructed to play out the aftermath or capture all dead
   * opponent stones, generate an aftermath move.
   */
  if (val < 0.0
      && !doing_scoring
      && (play_out_aftermath || capture_all_dead)
      && aftermath_genmove(i, j, color, NULL, 0) > 0) {
    val = 1.0;
    TRACE("Aftermath move at %m\n", *i, *j);
    move_considered(*i, *j, val);
    if (showtime) {
      t4 = gg_gettimeofday();
      fprintf(stderr, "AFTERMATH_GENMOVE took %.2f sec\n", t4-t3);
      t3 = t4;
    }
  }

  /* If we're instructed to capture all dead opponent stones, generate
   * a capturing move.
   */
  if (val < 0.0
      && !doing_scoring
      && capture_all_dead
      && aftermath_genmove(i, j, color, NULL, 1) > 0) {
    val = 1.0;
    TRACE("Aftermath move at %m\n", *i, *j);
    move_considered(*i, *j, val);
    if (showtime) {
      t4 = gg_gettimeofday();
      fprintf(stderr, "AFTERMATH_GENMOVE took %.2f sec\n", t4-t3);
      t3 = t4;
    }
  }

  /* If no move is found then pass. */
  if (val < 0.0) {
    TRACE("I pass.\n");
    *i = -1;
    *j = -1;
  }
  else
    TRACE("genmove() recommends %m with value %f\n", *i, *j, val);
  
  /* If statistics is turned on, this is the place to show it. */
  if (showstatistics) {
    printf("Nodes:                %d\n", stats.nodes);
    printf("Positions entered:    %d\n", stats.position_entered);
    printf("Position hits:        %d\n", stats.position_hits);
    printf("Read results entered: %d\n", stats.read_result_entered);
    printf("Read result hits:     %d\n", stats.read_result_hits);
    printf("Hash collisions:      %d\n", stats.hash_collisions);
  }

  if (showtime) {
    t2 = gg_gettimeofday();
    gprintf("*** TIME to generate move at %m: ", *i, *j);
    fprintf(stderr, "%.2f sec\n", t2-t1);
    total_time += t2-t1;
    if (t2-t1 > slowest_time) {
      slowest_time = t2-t1;
      slowest_i = *i;
      slowest_j = *j;
      slowest_movenum = movenum+1;
    }
    if (*i == -1) {
      gprintf("\nSLOWEST MOVE: %d at %m ", slowest_movenum,
	      slowest_i, slowest_j);
      fprintf(stderr, "(%.2f seconds)\n", slowest_time);
      fprintf(stderr, "\nAVERAGE TIME: %.2f seconds per move\n",
	      total_time/movenum);
      fprintf(stderr, "\nTOTAL TIME: %.2f seconds\n",
	      total_time);

    }
  }
  
  return val;
  
}  /* end genmove */


/* This is called for each move which has been considered. For
 * debugging purposes, we keep a table of all the moves we
 * have considered.
 */

void 
move_considered(int i, int j, float val)
{
  if (val > potential_moves[i][j]) {
    potential_moves[i][j] = val;
  }
}


/* If there is a file with the name "level", reads it
 * each move and corrects the value of level.
 */

static int
get_level(int *level)
{
  char buffer[128];
  FILE *fp;

  const char filename[]="level";

  if ((fp=fopen(filename,"r"))==NULL)
    return 0;

  if (fgets(buffer, 128, fp)) {
    if (sscanf(buffer, "%d", level))
      return 1;
    else
      return 0;
  }
  else
    return 0;
}
  

/* ================================================================ */
/*       Set up fixed placement handicap pieces for black side      */
/* ================================================================ */


/* Handicap pieces are set up according to the following diagrams:
 *  
 * 2 stones:                    3 stones:           
 *
 *   A B C D E F G H J	  	  A B C D E F G H J  
 * 9 . . . . . . . . . 9  	9 . . . . . . . . . 9
 * 8 . . . . . . . . . 8  	8 . . . . . . . . . 8
 * 7 . . + . . . X . . 7  	7 . . X . . . X . . 7
 * 6 . . . . . . . . . 6  	6 . . . . . . . . . 6
 * 5 . . . . + . . . . 5  	5 . . . . + . . . . 5
 * 4 . . . . . . . . . 4  	4 . . . . . . . . . 4
 * 3 . . X . . . + . . 3  	3 . . X . . . + . . 3
 * 2 . . . . . . . . . 2  	2 . . . . . . . . . 2
 * 1 . . . . . . . . . 1  	1 . . . . . . . . . 1
 *   A B C D E F G H J	  	  A B C D E F G H J  
 *   
 * 4 stones:                    5 stones:           
 *						     
 *   A B C D E F G H J	          A B C D E F G H J  
 * 9 . . . . . . . . . 9 	9 . . . . . . . . . 9
 * 8 . . . . . . . . . 8 	8 . . . . . . . . . 8
 * 7 . . X . . . X . . 7 	7 . . X . . . X . . 7
 * 6 . . . . . . . . . 6 	6 . . . . . . . . . 6
 * 5 . . . . + . . . . 5 	5 . . . . X . . . . 5
 * 4 . . . . . . . . . 4 	4 . . . . . . . . . 4
 * 3 . . X . . . X . . 3 	3 . . X . . . X . . 3
 * 2 . . . . . . . . . 2 	2 . . . . . . . . . 2
 * 1 . . . . . . . . . 1 	1 . . . . . . . . . 1
 *   A B C D E F G H J	          A B C D E F G H J  
 *  
 * 6 stones:                    7 stones:           
 *						     
 *   A B C D E F G H J	          A B C D E F G H J  
 * 9 . . . . . . . . . 9 	9 . . . . . . . . . 9
 * 8 . . . . . . . . . 8 	8 . . . . . . . . . 8
 * 7 . . X . . . X . . 7 	7 . . X . . . X . . 7
 * 6 . . . . . . . . . 6 	6 . . . . . . . . . 6
 * 5 . . X . + . X . . 5 	5 . . X . X . X . . 5
 * 4 . . . . . . . . . 4 	4 . . . . . . . . . 4
 * 3 . . X . . . X . . 3 	3 . . X . . . X . . 3
 * 2 . . . . . . . . . 2 	2 . . . . . . . . . 2
 * 1 . . . . . . . . . 1 	1 . . . . . . . . . 1
 *   A B C D E F G H J	          A B C D E F G H J  
 *  
 * 8 stones:                    9 stones:           
 *						     
 *   A B C D E F G H J	          A B C D E F G H J  
 * 9 . . . . . . . . . 9   	9 . . . . . . . . . 9
 * 8 . . . . . . . . . 8   	8 . . . . . . . . . 8
 * 7 . . X . X . X . . 7   	7 . . X . X . X . . 7
 * 6 . . . . . . . . . 6   	6 . . . . . . . . . 6
 * 5 . . X . + . X . . 5   	5 . . X . X . X . . 5
 * 4 . . . . . . . . . 4   	4 . . . . . . . . . 4
 * 3 . . X . X . X . . 3   	3 . . X . X . X . . 3
 * 2 . . . . . . . . . 2   	2 . . . . . . . . . 2
 * 1 . . . . . . . . . 1   	1 . . . . . . . . . 1
 *   A B C D E F G H J	          A B C D E F G H J  
 *  
 * For odd-sized boards larger than 9x9, the same pattern is followed,
 * except that the edge stones are moved to the fourth line for 13x13
 * boards and larger.
 *
 * For even-sized boards at least 8x8, only the four first diagrams
 * are used, because there is no way to place the center stones
 * symmetrically. As for odd-sized boards, the edge stones are moved
 * to the fourth line for boards larger than 11x11.
 *
 * At most four stones are placed on 7x7 boards too (this size may or
 * may not be supported by the rest of the engine). No handicap stones
 * are ever placed on smaller boards.
 *
 * Notice that this function only deals with fixed handicap placement.
 * Larger handicaps can be added by free placement if the used
 * interface supports it. */


/* This table contains the (coded) positions of the stones.
 *  2 maps to 2 or 3, depending on board size
 *  0 maps to center
 * -ve numbers map to  board_size - number
 *
 * The stones are placed in this order, *except* if there are
 * 5 or 7 stones, in which case center ( {0,0} ) is placed, and
 * then as for 4 or 6.
 */

static const int places[][2] = {

  {2,-2}, {-2,2}, {2,2}, {-2,-2},  /* first 4 are easy */
                                   /* for 5, {0,0} is explicitly placed */
  
  {0,2},  {0,-2},                  /* for 6 these two are placed */
                                   /* for 7, {0,0} is explicitly placed */
  
  {2,0}, {-2,0},                   /* for 8, these two are placed */

  {0,0},                           /* finally tengen for 9 */
};


/*
 * Sets up handicap stones, returning the number of placed handicap stones.
 */

int
placehand(int handicap)
{
  int x;
  int maxhand;
  int three = board_size > 11 ? 3 : 2;
  int mid = board_size/2;
  int retval = handicap;

  /* A handicap of 1 just means that B plays first, no komi.
   * Black is not told where to play the first stone so no handicap
   * is set. 
   */
  if (handicap < 2)
    return 0;
  if ((board_size % 2 == 1) && (board_size >= 9))
    maxhand = 9;
  else if (board_size >= 7)
    maxhand = 4;
  else
    maxhand = 0;

  /* It's up to the caller of this function to notice if the handicap
   * was too large for fixed placement and act upon that.
   */
  if (handicap > maxhand) {
    handicap = maxhand;
    retval = maxhand;
  }

  /* special cases: 5 and 7 */
  if (handicap == 5 || handicap == 7) {
    add_stone(mid, mid, BLACK);
    handicap--;
  }

  for (x = 0; x < handicap; ++x) {
    int i = places[x][0];
    int j = places[x][1];

    /* translate the encoded values to board co-ordinates */
    if (i == 2)  i = three;	/* 2 or 3 */
    if (i == -2) i = -three;

    if (j == 2)  j = three;
    if (j == -2) j = -three;

    if (i == 0) i = mid;
    if (j == 0) j = mid;

    if ( i < 0) i += board_size-1;
    if ( j < 0) j += board_size-1;

    add_stone(i, j, BLACK);
  }

  return retval;
}


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
