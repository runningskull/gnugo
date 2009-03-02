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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "liberty.h"
#include "sgftree.h"
#include "gg_utils.h"

/* Return one if x doesn't equal position_number and 0 otherwise.
 * After using this macro x will always have the value
 * position_number.
 */
#define NEEDS_UPDATE(x) (x != position_number ? (x = position_number, 1) : 0)

/* Mark a limited search area. If limit_search != 1, genmove
 * will only return moves within the area marked by the array
 * search_mask.
 */
static int limit_search = 0;
static int search_mask[BOARDMAX];

static int do_genmove(int color, float pure_threat_value,
		      int allowed_moves[BOARDMAX], float *value, int *resign);

/* Position numbers for which various examinations were last made. */
static int worms_examined = -1;
static int initial_influence_examined = -1;
static int dragons_examined_without_owl = -1;
static int dragons_examined = -1;
static int initial_influence2_examined = -1;
static int dragons_refinedly_examined = -1;

static int revise_semeai(int color);
static int revise_thrashing_dragon(int color, float our_score,
    			 	   float advantage);

static void break_mirror_go(int color);
static int find_mirror_move(int *move, int color);
static int should_resign(int color, float optimistic_score, int move);
static void compute_scores(int use_chinese_rules);


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
  reuse_random_seed();

  /* Initialize things for hashing of positions. */
  reading_cache_clear();

  hashdata_recalc(&board_hash, board, board_ko_pos);

  worms_examined = -1;
  initial_influence_examined = -1;
  dragons_examined_without_owl = -1;
  dragons_examined = -1;
  initial_influence2_examined = -1;
  dragons_refinedly_examined = -1;

  /* Prepare our table of move reasons. */
  clear_move_reasons();
  clear_break_in_list();

  /* Set up depth values (see comments there for details). */
  set_depth_values(get_level(), 0);

  /* Initialize arrays of moves which are meaningless due to
   * static analysis of unconditional status.
   */
  clear_unconditionally_meaningless_moves();
}

/*
 * Examine the position and try to gather as much information as possible.
 * This is used mainly for move generation, but could also be called
 * for debugging purposes (decidestring, etc).
 *
 * The parameter how_much tells us how much of the work we have to do.
 * For move generation we have to do it all.  For debugging we can 
 * sometimes stop a little earlier.
 *
 * aftermath_play indicates we are in aftermath playout phase. It's only
 * effect is to always use chinese rules for the score estimates.
 */

void
examine_position(int how_much, int aftermath_play)
{
  int save_verbose = verbose;

  purge_persistent_caches();
  
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
	make_dragons(1);
      verbose = save_verbose;
      gg_assert(test_gray_border() < 0);
      return;
    }
    
    if (NEEDS_UPDATE(dragons_examined)) {
      make_dragons(0);
      compute_scores(chinese_rules || aftermath_play);
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
    compute_scores(chinese_rules || aftermath_play);
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
    compute_strategic_sizes();
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
silent_examine_position(int how_much)
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
  
  examine_position(how_much, 0);

  verbose = save_verbose;
  sgf_dumptree = save_sgf_dumptree;
  count_variations = save_count_variations;
  debug = save_debug;
  printmoyo = save_printmoyo;
}


/* 
 * Generate computer move for color.
 *
 * Return the generated move.
 */

int
genmove(int color, float *value, int *resign)
{
  int move = PASS_MOVE;
  if (resign)
    *resign = 0;

#if ORACLE
  if (metamachine) {
    move = metamachine_genmove(color, value, limit_search);
    gg_assert(stackp == 0);
    if (move != PASS_MOVE)
      return move;
  }
#endif

  if (limit_search)
    move = do_genmove(color, 0.4, search_mask, value, resign);
  else
    move = do_genmove(color, 0.4, NULL, value, resign);
  gg_assert(move == PASS_MOVE || ON_BOARD(move));

  return move;
}


/* 
 * Same as above but doesn't generate pure threat moves. Useful when
 * trying to score a game.
 */

int
genmove_conservative(int color, float *value)
{
  return do_genmove(color, 0.0, NULL, value, NULL);
}


int
genmove_restricted(int color, int allowed_moves[BOARDMAX])
{
  return do_genmove(color, 0.0, allowed_moves, NULL, NULL);
}

/* This function collects move reasons can be generated immediately from
 * the data gathered in the examine_position() phase.
 */
void
collect_move_reasons(int color)
{
  worm_reasons(color);
  semeai_move_reasons(color);
  owl_reasons(color);
  cut_reasons(color);
  break_in_move_reasons(color);
  unconditional_move_reasons(color);
}

/* Call Monte Carlo module to generate a move. */
static int
monte_carlo_genmove(int color, int allowed_moves[BOARDMAX],
		    float *value, int *resign)
{
  int pos;
  int best_move = PASS_MOVE;
  int best_uct_move = PASS_MOVE;
  int unconditional_territory_black[BOARDMAX];
  int unconditional_territory_white[BOARDMAX];
  int forbidden_move[BOARDMAX];
  float move_values[BOARDMAX];
  int move_frequencies[BOARDMAX];
  float best_value;
  int frequency_cutoff;
  int frequency_cutoff2;
  int number_of_simulations;

  memset(move_values, 0, sizeof(move_values));
  memset(move_frequencies, 0, sizeof(move_frequencies));
  
  if (0) {
    simple_showboard(stderr);
    gprintf("\n");
  }

  if (resign)
    *resign = 0;
  
  unconditional_life(unconditional_territory_black, BLACK);
  unconditional_life(unconditional_territory_white, WHITE);

  for (pos = BOARDMIN; pos < BOARDMAX; pos++)
    if (!ON_BOARD(pos))
      continue;
    else if (unconditional_territory_black[pos])
      forbidden_move[pos] = BLACK;
    else if (unconditional_territory_white[pos])
      forbidden_move[pos] = WHITE;
    else
      forbidden_move[pos] = 0;

  number_of_simulations = mc_games_per_level * gg_max(get_level(), 1);
  
  uct_genmove(color, &best_uct_move, forbidden_move, allowed_moves,
	      number_of_simulations, move_values, move_frequencies);

  best_move = best_uct_move;
  best_value = 0.0;
  frequency_cutoff = move_frequencies[best_uct_move] / 2;
  frequency_cutoff2 = move_frequencies[best_uct_move] / 10;
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (ON_BOARD(pos)
	&& (move_frequencies[pos] > frequency_cutoff
	    || (move_values[pos] > 0.9
		&& move_frequencies[pos] > frequency_cutoff2)
	    || move_values[best_uct_move] < 0.1)
	&& (!allowed_moves || allowed_moves[pos])
	&& potential_moves[pos] > best_value) {
      best_move = pos;
      best_value = potential_moves[pos];
    }
  }

  unconditionally_meaningless_move(best_move, color, &best_move);

  *value = 1.0;
  
  return best_move;
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
do_genmove(int color, float pure_threat_value,
	   int allowed_moves[BOARDMAX], float *value, int *resign)
{
  float average_score, pessimistic_score, optimistic_score;
  int save_verbose;
  int save_depth;
  int move;
  float dummy_value;
  int use_thrashing_dragon_heuristics = 0;

  if (!value)
    value = &dummy_value;

  start_timer(0);
  clearstats();

  /* Usually we would not recommend resignation. */
  if (resign)
    *resign = 0;
  
  /* Prepare our table of moves considered. */
  memset(potential_moves, 0, sizeof(potential_moves));
  
  /* no move is found yet. */
  move = PASS_MOVE;  
  *value = 0.0; 
  
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
      && find_mirror_move(&move, color)) {
    TRACE("genmove() recommends mirror move at %1m\n", move);
    *value = 1.0;
    return move;
  }

  /* Find out information about the worms and dragons. */
  start_timer(1);
  examine_position(EXAMINE_ALL, 0);
  time_report(1, "examine position", NO_MOVE, 1.0);


  /* The score will be used to determine when we are safely
   * ahead. So we want the most conservative score.
   *
   * We always want to have the score from our point of view. So
   * negate it if we are black.
   */
  if (color == WHITE) {
    pessimistic_score = black_score;
    optimistic_score = white_score;
  }
  else {
    pessimistic_score = -white_score;
    optimistic_score = -black_score;
  }

  if (color == WHITE)
    average_score = (white_score + black_score)/2.0;
  else
    average_score = -(white_score + black_score)/2.0;
  choose_strategy(color, average_score, game_status(color));

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

  
  /* Pick up moves that we know of already. */
  save_verbose = verbose;
  if (verbose > 0)
    verbose--;
  collect_move_reasons(color);
  verbose = save_verbose;
  time_report(1, "generate move reasons", NO_MOVE, 1.0);
  
  /* Try to find empty corner moves. */
  fuseki(color);
  gg_assert(stackp == 0);

  /* Look for moves to break mirror play by the opponent. */
  break_mirror_go(color);

  /* If we are ahead by 5 points or more, consider a thrashing
   * dragon dangerous and change its status from DEAD to
   * UNKNOWN. Otherwise, pretend there is no thrashing dragon.
   */
  if (!doing_scoring)
    use_thrashing_dragon_heuristics
      = revise_thrashing_dragon(color, pessimistic_score, 5.0);
  
  /* The general pattern database. */
  shapes(color);
  time_report(1, "shapes", NO_MOVE, 1.0);
  gg_assert(stackp == 0);

  /* Look for combination attacks and defenses against them. */
  combinations(color);
  time_report(1, "combinations", NO_MOVE, 1.0);
  gg_assert(stackp == 0);

  /* Review the move reasons and estimate move values. */
  if (review_move_reasons(&move, value, color, 
			  pure_threat_value, pessimistic_score, allowed_moves,
			  use_thrashing_dragon_heuristics))
    TRACE("Move generation likes %1m with value %f\n", move, *value);
  gg_assert(stackp == 0);
  time_report(1, "review move reasons", NO_MOVE, 1.0);


  /* If the move value is 6 or lower, we look for endgame patterns too. */
  if (*value <= 6.0 && !disable_endgame_patterns) {
    endgame_shapes(color);
    endgame(color);
    gg_assert(stackp == 0);
    if (review_move_reasons(&move, value, color, pure_threat_value,
	  		    pessimistic_score, allowed_moves,
			    use_thrashing_dragon_heuristics))
      TRACE("Move generation likes %1m with value %f\n", move, *value);
    gg_assert(stackp == 0);
    time_report(1, "endgame", NO_MOVE, 1.0);
  }
  
  /* If no move found yet, revisit any semeai and change the
   * status of the opponent group from DEAD to UNKNOWN, then 
   * run shapes and endgame_shapes again. This may turn up a move.
   */
  if (move == PASS_MOVE) {
    if (revise_semeai(color)) {
      shapes(color);
      endgame_shapes(color);
      if (review_move_reasons(&move, value, color, pure_threat_value,
			      pessimistic_score, allowed_moves,
			      use_thrashing_dragon_heuristics)) {
	TRACE("Upon reconsideration move generation likes %1m with value %f\n",
	      move, *value); 
      }
    }
    time_report(1, "move reasons with revised semeai status",
		NO_MOVE, 1.0);
  }

  /* If Monte Carlo move generation is enabled, call it now. Do not
   * override a fuseki move.
   *
   * FIXME: Identifying fuseki moves by checking the move value is
   * very ugly and fragile.
   */
  if (use_monte_carlo_genmove && move != PASS_MOVE
      && (*value < 75.0 || *value > 75.01) && !doing_scoring) {
    int allowed_moves2[BOARDMAX];
    int num_allowed_moves2 = 0;
    int pos;
    for (pos = BOARDMIN; pos < BOARDMAX; pos++)
      if (ON_BOARD(pos)
	  && (!allowed_moves || allowed_moves[pos])
	  && is_allowed_move(pos, color)) {
	allowed_moves2[pos] = 1;
	num_allowed_moves2++;
      }
      else
	allowed_moves2[pos] = 0;
    
    if (num_allowed_moves2 > 1)
      move = monte_carlo_genmove(color, allowed_moves2, value, resign);
  }
  
  /* If still no move, fill a remaining liberty. This should pick up
   * all missing dame points.
   */
  if (move == PASS_MOVE
      && fill_liberty(&move, color)) {
    if (!allowed_moves || allowed_moves[move]) {
      *value = 1.0;
      TRACE("Filling a liberty at %1m\n", move);
      record_top_move(move, *value);
      move_considered(move, *value);
      time_report(1, "fill liberty", NO_MOVE, 1.0);
    }
    else
      move = PASS_MOVE;
  }

  /* If we're instructed to play out the aftermath or capture all dead
   * opponent stones, or if the opponent is trying to live inside
   * our territory and we are clearly ahead, generate an aftermath move.
   */
  if (move == PASS_MOVE) {
    if (play_out_aftermath 
	|| capture_all_dead 
	|| (!doing_scoring && thrashing_dragon && pessimistic_score > 15.0))
      move = aftermath_genmove(color, capture_all_dead, allowed_moves);

    if (move != PASS_MOVE) {
      ASSERT1(is_legal(move, color), move);
      *value = 1.0;
      TRACE("Aftermath move at %1m\n", move);
      record_top_move(move, *value);
      move_considered(move, *value);
      time_report(1, "aftermath_genmove", NO_MOVE, 1.0);
    }
  }

  /* If we somehow have managed to generate an illegal move, pass instead. */
  if (!is_allowed_move(move, color)) {
    TRACE("ILLEGAL MOVE GENERATED. Passing instead.\n");
    move = PASS_MOVE;
    *value = -1.0;
  }
  
  /* If no move is found then pass. */
  if (move == PASS_MOVE) {
    TRACE("I pass.\n");
  }
  else {
    TRACE("genmove() recommends %1m with value %f\n", move, *value);
  }
  
  /* Maybe time to resign...
   */
  if (resign && resign_allowed
      && *value < 10.0 && should_resign(color, optimistic_score, move)) {
    TRACE("... though, genmove() thinks the position is hopeless\n");
    *resign = 1;
  }
  
  /* If statistics is turned on, this is the place to show it. */
  if (showstatistics)
    showstats();

  if (showtime) {
    double spent = time_report(0, "TIME to generate move at ", move, 1.0);
    total_time += spent;
    if (spent > slowest_time) {
      slowest_time = spent;
      slowest_move = move;
      slowest_movenum = movenum + 1;
    }
  }

  /* Some consistency checks to verify that things are properly
   * restored and/or have not been corrupted.
   */
  gg_assert(stackp == 0);
  gg_assert(test_gray_border() < 0);
  gg_assert(depth == save_depth);

  return move;
}



/* This is called for each move which has been considered. For
 * debugging purposes, we keep a table of all the moves we
 * have considered.
 */

void 
move_considered(int move, float value)
{
  if (value > potential_moves[move])
    potential_moves[move] = value;
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

  if (stones_on_board(BLACK | WHITE) == 0)
    return 0;

  if (doing_scoring)
    return 0;
  
  gg_assert(dragon2 != NULL);

  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (ON_BOARD(pos)
	&& dragon[pos].color == other
	&& DRAGON2(pos).semeais
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
revise_thrashing_dragon(int color, float our_score, float advantage)
{
  int pos;
  signed char safe_stones[BOARDMAX];
  float strength[BOARDMAX];

  /* Trust the owl code's opinion if we are behind. */
  if (our_score < advantage)
    return 0;

  if (disable_threat_computation
      || !thrashing_dragon 
      || dragon[thrashing_dragon].status != DEAD)
    return 0;
  
  for (pos = BOARDMIN; pos < BOARDMAX; pos++)
    if (ON_BOARD(pos) && thrashing_stone[pos]
	&& worm[pos].unconditional_status != DEAD) {
      dragon[pos].status = UNKNOWN;
      DRAGON2(pos).safety = ALIVE;
    }

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

static int
find_mirror_move(int *move, int color)
{
  int last_move = get_last_move();
  int mirror_move;
  if (last_move != NO_MOVE) {
    mirror_move = MIRROR_MOVE(last_move);
    if (test_symmetry_after_move(mirror_move, color, 0)) {
      *move = mirror_move;
      return 1;
    }
  }
  else {
    for (mirror_move = BOARDMIN; mirror_move < BOARDMAX; mirror_move++) {
      if (ON_BOARD(mirror_move)
	  && test_symmetry_after_move(mirror_move, color, 0)) {
	*move = mirror_move;
	return 1;
      }
    }
  }
  
  return 0;
}

/* Computer two territory estimates: for *upper, the status of all
 * cricital stones gets resolved in White's favor; vice verso for
 * black.
 */
static void
compute_scores(int use_chinese_rules)
{
  signed char safe_stones[BOARDMAX];
  float strength[BOARDMAX];

  set_strength_data(WHITE, safe_stones, strength);
  compute_influence(EMPTY, safe_stones, strength, &move_influence,
      		    NO_MOVE, "White territory estimate");
  white_score = influence_score(&move_influence, use_chinese_rules);
  set_strength_data(BLACK, safe_stones, strength);
  compute_influence(EMPTY, safe_stones, strength, &move_influence,
      		    NO_MOVE, "White territory estimate");
  black_score = influence_score(&move_influence, use_chinese_rules);

  if (verbose || showscore) {
    if (white_score == black_score)
      gprintf("Score estimate: %s %f\n",
	      black_score > 0 ? "W " : "B ", gg_abs(black_score));
    else
      gprintf("Score estimate: %s %f to %s %f\n",
	      black_score > 0 ? "W " : "B ", gg_abs(black_score),
	      white_score > 0 ? "W " : "B ", gg_abs(white_score));
    fflush(stderr);
  }
}


/* Detect if a white opponent has played mirror go for at least 10
 * moves and if so play on tengen.
 *
 * Mirror breaking moves in other situations are handled by patterns
 * in patterns.db.
 */
static void
break_mirror_go(int color)
{
  int tengen = POS((board_size - 1) / 2, (board_size - 1) / 2);
  if (board[tengen] == EMPTY
      && color == BLACK
      && stones_on_board(BLACK | WHITE) > 10
      && test_symmetry_after_move(tengen, color, 1)) {
    set_minimum_move_value(tengen, 30.0);
    TRACE("Play %1m to break mirror go, value 30.\n", tengen);
  }
}


/* Helper to decide whether GG should resign a game
 */
static int
should_resign(int color, float optimistic_score, int move)
{
  float status;
  int d;
  /* We resign 19x19 games only, smaller board games are fast enough.
   * We resign only if the margin is bigger than 45 pts and if we are
   * behind (of course).
   *
   * As an exception to this rule, we resign on any board size if
   * it looks like all our dragons are dead and the generated move
   * is a pass.
   */
  if (board_size > 2 && move == PASS_MOVE && !lively_dragon_exists(color))
    return 1;
  
  if (move == PASS_MOVE
      || board_size < 19
      || optimistic_score > -45.0)
    return 0;
  /* Check dragon statuses. If a friendly dragon is critical, we are
   * possibly not that much behind after we save it. If some hostile
   * dragon is at least weak, we possibly have a chance to come back
   * if we can kill it.
   */
  for (d = 0; d < number_of_dragons; d++) {
    /* any friendly critical dragon ? */
    if (board[dragon2[d].origin] == color
	&& DRAGON(d).status == CRITICAL)
      return 0;
    /* any weak opponent dragon ? */
    if (board[dragon2[d].origin] == OTHER_COLOR(color)
	&& DRAGON(d).status != DEAD
	&& DRAGON(d).effective_size >= 10
	&& dragon_weak(dragon2[d].origin))
      return 0;
  }
  /* Is it already too late to try something ? */
  status = game_status(color);
  if (status < 0.8)
    /* Still "too early".
     * Note: the 0.8 constant is very conservative, we actually could give
     * up a bit earlier.
     */
    return 0;

  /* well, it is probably reasonable and polite to give up this game */
  return 1;
}


/*********************************************************************\
 *                Mark a limited search area                         *
\*********************************************************************/

/* Activate or deactivate search limit. */
void
set_limit_search(int value)
{
  limit_search = value;
}

/* The following function marks a diamond of radius 6 with center pos.  */

void
set_search_diamond(int pos)
{
  int i = I(pos);
  int j = J(pos);
  int m, n;
  
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      if (gg_abs(m - i) + gg_abs(n - j) <= 6)
	search_mask[POS(m, n)] = 1;
      else
	search_mask[POS(m, n)] = 0;      
    }
  limit_search = pos;
  if (0)
    draw_search_area();
}

/* unmarks the entire board */

void
reset_search_mask()
{
  memset(search_mask, 0, sizeof(search_mask));
}

/* marks a single vertex */

void
set_search_mask(int pos, int value)
{
  search_mask[pos] = value;
}

/* displays the search area */

void
draw_search_area(void)
{
  int m, n;

  start_draw_board();
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      int col, c;
	
      if (search_mask[POS(m, n)])
	col = GG_COLOR_RED;
      else
	col = GG_COLOR_BLACK;
      
      if (board[POS(m, n)] == BLACK)
	c = 'X';
      else if (board[POS(m, n)] == WHITE)
	c = 'O';
      else if (search_mask[POS(m, n)])
	c = '*';
      else
	c = '.';
      draw_color_char(m, n, c, col);
    }
  end_draw_board();
}

/* returns true if the position is within the search area */
int
within_search_area(int pos)
{
  if (!limit_search)
    return 1;
  return search_mask[pos];
}

/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
