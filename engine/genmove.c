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
#include "sgftree.h"
#include "random.h"

/* Return one if x doesn't equal position_number and 0 otherwise.
 * After using this macro x will always have the value
 * position_number.
 */
#define NEEDS_UPDATE(x) (x != position_number ? (x = position_number, 1) : 0)

static int get_level(int *level);
static int do_genmove(int *move, int color, float pure_threat_value,
		      int allowed_moves[BOARDMAX]);

static double slowest_time = 0.0;
static int    slowest_move = NO_MOVE;
static int    slowest_movenum = 0;
static double total_time = 0.0;

/* Position numbers for which various examinations were last made. */
static int worms_examined = -1;
static int initial_influence_examined = -1;
static int dragons_examined_without_owl = -1;
static int dragons_examined = -1;
static int initial_influence2_examined = -1;
static int dragons_refinedly_examined = -1;

static int revise_semeai(int color);
static int revise_thrashing_dragon(int color, float advantage);

static int find_mirror_move(int *move, int color);
static int test_symmetry_after_move(int move, int color);

void sgfShowConsideredMoves(void);


/* Reset some things in the engine. 
 *
 * This prepares the hash table for the reading code for use.  It
 * should be called when we start examine a new position.  
 */

void
reset_engine()
{
  /* To improve the reproducability of games, we restart the random
   * number generator with the same seed for each move. Thus we don't
   * have to know how many previous moves have been played, nor
   * actually play through them, in order to get the right random
   * numbers.
   */
  gg_srand(random_seed);

  /* Initialize things for hashing of positions. */
  reading_cache_clear();
  hashdata_recalc(&hashdata, board, board_ko_pos);

  worms_examined = -1;
  initial_influence_examined = -1;
  dragons_examined_without_owl = -1;
  dragons_examined = -1;
  initial_influence2_examined = -1;
  dragons_refinedly_examined = -1;

  /* Prepare our table of move reasons. */
  clear_move_reasons();

  /* Set up depth values (see comments there for details). */
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
  int save_verbose = verbose;

  purge_persistent_reading_cache();
  
  /* Don't print reading traces during make_worms and make_dragons unless 
   * the user really wants it (verbose == 3). 
   */
  if (verbose == 1 || verbose == 2)
    --verbose;

  if (NEEDS_UPDATE(worms_examined)) {
    start_timer(0);
    make_worms();
    time_report(0, "  make worms", NO_MOVE, 1.0);
  }
  if (how_much == EXAMINE_WORMS) {
    verbose = save_verbose;
    gg_assert(test_gray_border() < 0);
    return;
  }

  if (stones_on_board(BLACK | WHITE) != 0) {
    if (NEEDS_UPDATE(initial_influence_examined))
      compute_worm_influence();
    if (how_much == EXAMINE_INITIAL_INFLUENCE) {
      verbose = save_verbose;
      gg_assert(test_gray_border() < 0);
      return;
    }

    if (how_much == EXAMINE_DRAGONS_WITHOUT_OWL) {
      if (NEEDS_UPDATE(dragons_examined_without_owl))
	make_dragons(color, 1, save_verbose);
      verbose = save_verbose;
      gg_assert(test_gray_border() < 0);
      return;
    }
    
    if (NEEDS_UPDATE(dragons_examined)) {
      make_dragons(color, 0, save_verbose);
      /* We have automatically done a partial dragon analysis as well. */
      dragons_examined_without_owl = position_number;
    }
    if (how_much == EXAMINE_DRAGONS) {
      verbose = save_verbose;
      gg_assert(test_gray_border() < 0);
      return;
    }
  }
  else if (how_much == EXAMINE_INITIAL_INFLUENCE
	   || how_much == EXAMINE_DRAGONS
	   || how_much == EXAMINE_ALL) {
    initialize_dragon_data();
    verbose = save_verbose;
    gg_assert(test_gray_border() < 0);
    return;
  }
  
  verbose = save_verbose;

  if (NEEDS_UPDATE(initial_influence2_examined)) {
    compute_dragon_influence();
  }
  if (how_much == EXAMINE_INITIAL_INFLUENCE2) {
    gg_assert(test_gray_border() < 0);
    return;
  }

  if (NEEDS_UPDATE(dragons_refinedly_examined)) {
    compute_refined_dragon_weaknesses();
  }
  if (how_much == FULL_EXAMINE_DRAGONS) {
    gg_assert(test_gray_border() < 0);
    return;
  }

  if (printworms)
    show_dragons();
}


/* The same as examine_position(), except that all traces, debug
 * output, and sgf traces are turned off.
 */
void
silent_examine_position(int color, int how_much)
{
  int save_verbose = verbose;
  SGFTree *save_sgf_dumptree = sgf_dumptree;
  int save_count_variations = count_variations;
  int save_debug = debug;
  int save_printmoyo = printmoyo;
  
  verbose = 0;
  sgf_dumptree = NULL;
  count_variations = 0;
  debug = 0;
  printmoyo = 0;
  
  examine_position(color, how_much);

  verbose = save_verbose;
  sgf_dumptree = save_sgf_dumptree;
  count_variations = save_count_variations;
  debug = save_debug;
  printmoyo = save_printmoyo;
}


/* 
 * Generate computer move for COLOR.
 *
 * Return the generated move in (*i, *j).
 */

int
genmove(int *i, int *j, int color)
{
  int move;
  int retval;

  retval = do_genmove(&move, color, 0.4, NULL);
  gg_assert(retval < 0 || ON_BOARD1(move));

  if (i) *i = I(move);
  if (j) *j = J(move);

  return retval;
}


/* 
 * Same as above but doesn't generate pure threat moves. Useful when
 * trying to score a game.
 */

int
genmove_conservative(int *i, int *j, int color)
{
  int move;
  int retval;

  retval = do_genmove(&move, color, 0.0, NULL);

  if (i) *i = I(move);
  if (j) *j = J(move);

  return retval;
}


int
genmove_restricted(int *i, int *j, int color, int allowed_moves[BOARDMAX])
{
  int move;
  int retval;

  retval = do_genmove(&move, color, 0.0, allowed_moves);

  if (i) *i = I(move);
  if (j) *j = J(move);

  return retval;
}


/* 
 * Perform the actual move generation.
 *
 * The array allowed_moves restricts which moves may be considered. If
 * NULL any move is allowed. Pass is always allowed and will be chosen
 * if the move generation doesn't like any of the allowed moves (or
 * overlooks them).
 */
  
static int
do_genmove(int *move, int color, float pure_threat_value,
	   int allowed_moves[BOARDMAX])
{
  float val;
  int save_verbose;
  int save_depth;

  start_timer(0);
  
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
  *move = NO_MOVE;  
  val = -1; 
  if (get_level(&level))
    fprintf(stderr, "level = %d\n", level);
  
  /* Prepare pattern matcher and reading code. */
  reset_engine();

  /* Store the depth value so we can check that it hasn't changed when
   * we leave this function.
   */
  save_depth = depth;

  /* If in mirror mode, try to find a mirror move. */
  if (play_mirror_go
      && (mirror_stones_limit < 0
	  || stones_on_board(WHITE | BLACK) <= mirror_stones_limit)
      && find_mirror_move(move, color)) {
    TRACE("genmove() recommends mirror move at %1m\n", *move);
    return 1.0;
  }

  /* Find out information about the worms and dragons. */
  start_timer(1);
  examine_position(color, EXAMINE_ALL);
  time_report(1, "examine position", NO_MOVE, 1.0);

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
    time_report(1, "estimate score", NO_MOVE, 1.0);

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

  /*
   * Print some of the information if the user wants to.
   */
  if (printmoyo)
    print_moyo();
  
  if (printboard) {
    if (printboard == 1)
      fprintf(stderr, "\n          dragon_status display:\n\n");
    if (printboard == 2)
      fprintf(stderr, "\n          eye display:\n\n");
    showboard(printboard); 
    if (printboard == 1) {
      fprintf(stderr, "\n           owl_status display:\n\n");      
      showboard(3);
      fprintf(stderr, "\n           matcher_status display:\n\n");      
      showboard(4);
    }
  }
  
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
  
  /* Try to find empty corner moves. */
  fuseki(color);
  gg_assert(stackp == 0);

  /* The general pattern database. */
  start_timer(1);
  shapes(color);
  time_report(1, "shapes", NO_MOVE, 1.0);
  gg_assert(stackp == 0);

  /* Look for combination attacks and defenses against them. */
  combinations(color);
  time_report(1, "combinations", NO_MOVE, 1.0);
  gg_assert(stackp == 0);

  /* Review the move reasons and estimate move values. */
  if (review_move_reasons(move, &val, color, 
			  pure_threat_value, lower_bound, allowed_moves))
    TRACE("Move generation likes %1m with value %f\n", *move, val);
  gg_assert(stackp == 0);
  time_report(1, "review move reasons", NO_MOVE, 1.0);

  /* If we are ahead by 15 points or more, consider a thrashing
   * dragon dangerous and change its status from DEAD to
   * UNKNOWN. This may generate a move.
   */
  if (val < 10.0 && !doing_scoring) {
    if (revise_thrashing_dragon(color, 15.0)) {
      shapes(color);
      if (!disable_endgame_patterns)
	endgame_shapes(color);
      if (review_move_reasons(move, &val, color, pure_threat_value, score,
			      allowed_moves)) {
	TRACE("Upon reconsideration move generation likes %1m with value %f\n",
	      *move, val); 
      }
    }
    time_report(1, "move reasons with revised semeai status", NO_MOVE, 1.0);
  }

  /* If the move value is 6 or lower, we look for endgame patterns too. */
  if (val <= 6.0 && !disable_endgame_patterns) {
    endgame_shapes(color);
    gg_assert(stackp == 0);
    if (review_move_reasons(move, &val, color, pure_threat_value, score,
			    allowed_moves))
      TRACE("Move generation likes %1m with value %f\n", *move, val);
    gg_assert(stackp == 0);
    time_report(1, "endgame", NO_MOVE, 1.0);
  }
  
  /* If no move found yet, revisit any semeai and change the
   * status of the opponent group from DEAD to UNKNOWN, then 
   * run shapes and endgame_shapes again. This may turn up a move.
   */
  if (val < 0.0) {
    if (revise_thrashing_dragon(color, 0.0)
	|| revise_semeai(color)) {
      shapes(color);
      endgame_shapes(color);
      if (review_move_reasons(move, &val, color, pure_threat_value, score,
			      allowed_moves)) {
	TRACE("Upon reconsideration move generation likes %1m with value %f\n",
	      *move, val); 
      }
    }
    time_report(1, "move reasons with revised semeai status", NO_MOVE, 1.0);
  }

  /* If still no move, fill a remaining liberty. This should pick up
   * all missing dame points.
   */
  if (val < 0.0 
      && fill_liberty(move, color)
      && (!allowed_moves || allowed_moves[*move])) {
    val = 1.0;
    TRACE("Filling a liberty at %1m\n", *move);
    record_top_move(*move, val);
    move_considered(*move, val);
    time_report(1, "fill liberty", NO_MOVE, 1.0);
  }

  /* If we're instructed to play out the aftermath or capture all dead
   * opponent stones, or if the opponent is trying to live inside
   * our territory and we are clearly ahead, generate an aftermath move.
   */

  if (val < 0.0
      && !doing_scoring
      && (play_out_aftermath 
	  || capture_all_dead 
	  || (thrashing_dragon
	      && ((color == BLACK && score < -15.0)
		  || (color == WHITE && score > 15.0))))
      && aftermath_genmove(move, color, NULL, 0) > 0
      && (!allowed_moves || allowed_moves[*move])) {
    ASSERT1(is_legal(*move, color), *move);
    val = 1.0;
    TRACE("Aftermath move at %1m\n", *move);
    record_top_move(*move, val);
    move_considered(*move, val);
    time_report(1, "aftermath_genmove", NO_MOVE, 1.0);
  }

  /* If we're instructed to capture all dead opponent stones, generate
   * a capturing move.
   */
  if (val < 0.0
      && !doing_scoring
      && capture_all_dead 
      && aftermath_genmove(move, color, NULL, 1) > 0
      && (!allowed_moves || allowed_moves[*move])) {
    ASSERT1(is_legal(*move, color), *move);
    val = 1.0;
    TRACE("Aftermath move at %1m\n", *move);
    move_considered(*move, val);
    time_report(1, "aftermath_genmove", NO_MOVE, 1.0);
  }

  /* If no move is found then pass. */
  if (val < 0.0) {
    TRACE("I pass.\n");
    *move = NO_MOVE;
  }
  else
    TRACE("genmove() recommends %1m with value %f\n", *move, val);
  
  /* If statistics is turned on, this is the place to show it. */
  if (showstatistics) {
    gprintf("Nodes:                %d\n", stats.nodes);
    gprintf("Positions entered:    %d\n", stats.position_entered);
    gprintf("Position hits:        %d\n", stats.position_hits);
    gprintf("Read results entered: %d\n", stats.read_result_entered);
    gprintf("Read result hits:     %d\n", stats.read_result_hits);
    gprintf("Hash collisions:      %d\n", stats.hash_collisions);
  }
 
  if (showtime) {
    double spent = time_report(0, "TIME to generate move at ", *move, 1.0);
    total_time += spent;
    if (spent > slowest_time) {
      slowest_time = spent;
      slowest_move = *move;
      slowest_movenum = movenum + 1;
    }
    if (*move == NO_MOVE) {
      gprintf("\nSLOWEST MOVE: %d at %1m ", slowest_movenum, slowest_move);
      fprintf(stderr, "(%.2f seconds)\n", slowest_time);
      fprintf(stderr, "\nAVERAGE TIME: %.2f seconds per move\n",
	      total_time / movenum);
      fprintf(stderr, "\nTOTAL TIME: %.2f seconds\n",
	      total_time);

    }
  }

  /* Some consistency checks to verify that things are properly
   * restored and/or have not been corrupted.
   */
  gg_assert(stackp == 0);
  gg_assert(test_gray_border() < 0);
  gg_assert(depth == save_depth);

  return val;
}



/* This is called for each move which has been considered. For
 * debugging purposes, we keep a table of all the moves we
 * have considered.
 */

void 
move_considered(int move, float val)
{
  int i = I(move);
  int j = J(move);
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

  const char filename[] = "level";

  if ((fp = fopen(filename, "r")) == NULL)
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
  

/* revise_semeai(color) changes the status of any DEAD dragon of
 * OPPOSITE_COLOR(color) which occurs in a semeai to UNKNOWN.
 * It returns true if such a dragon is found.
 */

static int
revise_semeai(int color)
{
  int pos;
  int found_one = 0;
  int other = OTHER_COLOR(color);

  gg_assert(dragon2 != NULL);

  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (ON_BOARD(pos)
	&& dragon[pos].color == other
	&& DRAGON2(pos).semeai
	&& dragon[pos].status == DEAD) {
      found_one = 1;
      dragon[pos].status = UNKNOWN;
      if (dragon[pos].origin == pos)
	TRACE("revise_semeai: changed status of dragon %1m from DEAD to UNKNOWN\n",
	      pos);
    }
  }
  
  return found_one;
}


/* If the opponent's last move added a stone to a dead dragon,
 * revise it's status to UNKNOWN. This will cause genmove to
 * generate moves restraining the dragon. We only do this if
 * we are ahead by 'advantage', and no owl threat has been found.
 */

static int
revise_thrashing_dragon(int color, float advantage)
{
  int pos;
  char safe_stones[BOARDMAX];
  float strength[BOARDMAX];

  /* Trust the owl code's opinion if we are behind. */
  if ((color == BLACK && score > -advantage)
      || (color == WHITE && score < advantage))
    return 0;

  /* If an owl threat has been found, a move reason has
   * probably been generated, so we skip this step. */
  if (dragon[thrashing_dragon].owl_threat_status == CAN_THREATEN_DEFENSE)
    return 0;

  if (disable_threat_computation
      || !thrashing_dragon 
      || dragon[thrashing_dragon].status != DEAD)
    return 0;
  
  for (pos = BOARDMIN; pos < BOARDMAX; pos++)
    if (ON_BOARD(pos)
	&& is_same_dragon(pos, thrashing_dragon))
      dragon[pos].status = UNKNOWN;

  DRAGON2(thrashing_dragon).safety = ALIVE;

  set_strength_data(OTHER_COLOR(color), safe_stones, strength);
  compute_influence(OTHER_COLOR(color), safe_stones, strength,
      		    OPPOSITE_INFLUENCE(color),
		    NO_MOVE, "revised thrashing dragon");
  compute_refined_dragon_weaknesses();
  
  return 1;
}


/* Look for a mirror move. First try the mirror of the last move. If
 * none is available, test all moves to see if one makes the board
 * symmetric.
 *
 * To be able to deal with handicap stones we use a somewhat weak
 * definition of symmetry.
 */
#define MIRROR_MOVE(pos) POS(board_size - 1 - I(pos), board_size - 1 - J(pos))

static int
find_mirror_move(int *move, int color)
{
  int last_move = get_last_move();
  int mirror_move;
  if (last_move != NO_MOVE) {
    mirror_move = MIRROR_MOVE(last_move);
    if (test_symmetry_after_move(mirror_move, color)) {
      *move = mirror_move;
      return 1;
    }
  }
  else {
    for (mirror_move = BOARDMIN; mirror_move < BOARDMAX; mirror_move++) {
      if (ON_BOARD(mirror_move)
	  && test_symmetry_after_move(mirror_move, color)) {
	*move = mirror_move;
	return 1;
      }
    }
  }
  
  return 0;
}

static int
test_symmetry_after_move(int move, int color)
{
  int pos;
  int result = 1;

  if (board[move] != EMPTY)
    return 0;
  if (!trymove(move, color, "find_mirror_move", NO_MOVE, EMPTY, NO_MOVE))
    return 0;
  
  for (pos = BOARDMIN; pos <= MIRROR_MOVE(pos); pos++) {
    if ((board[pos] == EMPTY) ^ (board[MIRROR_MOVE(pos)] == EMPTY)) {
      result = 0;
      break;
    }
  }
  
  popgo();

  return result;
}


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
