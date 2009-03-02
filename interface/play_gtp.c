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
#include <assert.h>
#include <ctype.h>
#include <string.h>
#include <math.h>

#include "interface.h"
#include "liberty.h"
#include "gtp.h"
#include "gg_utils.h"

/* Internal state that's not part of the engine. */
static int report_uncertainty = 0;
static int gtp_orientation = 0;

static void gtp_print_code(int c);
static void gtp_print_vertices2(int n, int *moves);
static void rotate_on_input(int ai, int aj, int *bi, int *bj);
static void rotate_on_output(int ai, int aj, int *bi, int *bj);


#define DECLARE(func) static int func(char *s)

DECLARE(gtp_aa_confirm_safety);
DECLARE(gtp_accurate_approxlib);
DECLARE(gtp_accuratelib);
DECLARE(gtp_advance_random_seed);
DECLARE(gtp_all_legal);
DECLARE(gtp_all_move_values);
DECLARE(gtp_analyze_eyegraph);
DECLARE(gtp_analyze_semeai);
DECLARE(gtp_analyze_semeai_after_move);
DECLARE(gtp_attack);
DECLARE(gtp_attack_either);
DECLARE(gtp_block_off);
DECLARE(gtp_break_in);
DECLARE(gtp_captures);
DECLARE(gtp_clear_board);
DECLARE(gtp_clear_cache);
DECLARE(gtp_combination_attack);
DECLARE(gtp_combination_defend);
DECLARE(gtp_connect);
DECLARE(gtp_countlib);
DECLARE(gtp_cputime);
DECLARE(gtp_decrease_depths);
DECLARE(gtp_defend);
DECLARE(gtp_defend_both);
DECLARE(gtp_disconnect);
DECLARE(gtp_does_attack);
DECLARE(gtp_does_defend);
DECLARE(gtp_does_surround);
DECLARE(gtp_dragon_data);
DECLARE(gtp_dragon_status);
DECLARE(gtp_dragon_stones);
DECLARE(gtp_draw_search_area);
DECLARE(gtp_dump_stack);
DECLARE(gtp_echo);
DECLARE(gtp_echo_err);
DECLARE(gtp_estimate_score);
DECLARE(gtp_eval_eye);
DECLARE(gtp_experimental_score);
DECLARE(gtp_eye_data);
DECLARE(gtp_final_score);
DECLARE(gtp_final_status);
DECLARE(gtp_final_status_list);
DECLARE(gtp_findlib);
DECLARE(gtp_finish_sgftrace);
DECLARE(gtp_fixed_handicap);
DECLARE(gtp_followup_influence);
DECLARE(gtp_genmove);
DECLARE(gtp_genmove_black);
DECLARE(gtp_genmove_white);
DECLARE(gtp_get_connection_node_counter);
DECLARE(gtp_get_handicap);
DECLARE(gtp_get_komi);
DECLARE(gtp_get_life_node_counter);
DECLARE(gtp_get_owl_node_counter);
DECLARE(gtp_get_random_seed);
DECLARE(gtp_get_reading_node_counter);
DECLARE(gtp_get_trymove_counter);
DECLARE(gtp_gg_genmove);
DECLARE(gtp_gg_undo);
DECLARE(gtp_half_eye_data);
DECLARE(gtp_increase_depths);
DECLARE(gtp_initial_influence);
DECLARE(gtp_invariant_hash);
DECLARE(gtp_invariant_hash_for_moves);
DECLARE(gtp_is_legal);
DECLARE(gtp_is_surrounded);
DECLARE(gtp_kgs_genmove_cleanup);
DECLARE(gtp_known_command);
DECLARE(gtp_ladder_attack);
DECLARE(gtp_last_move);
DECLARE(gtp_limit_search);
DECLARE(gtp_list_commands);
DECLARE(gtp_list_stones);
DECLARE(gtp_loadsgf);
DECLARE(gtp_move_influence);
DECLARE(gtp_move_probabilities);
DECLARE(gtp_move_reasons);
DECLARE(gtp_move_uncertainty);
DECLARE(gtp_move_history);
DECLARE(gtp_name);
DECLARE(gtp_owl_attack);
DECLARE(gtp_owl_connection_defends);
DECLARE(gtp_owl_defend);
DECLARE(gtp_owl_does_attack);
DECLARE(gtp_owl_does_defend);
DECLARE(gtp_owl_substantial);
DECLARE(gtp_owl_threaten_attack);
DECLARE(gtp_owl_threaten_defense);
DECLARE(gtp_place_free_handicap);
DECLARE(gtp_play);
DECLARE(gtp_playblack);
DECLARE(gtp_playwhite);
DECLARE(gtp_popgo);
DECLARE(gtp_printsgf);
DECLARE(gtp_program_version);
DECLARE(gtp_protocol_version);
DECLARE(gtp_query_boardsize);
DECLARE(gtp_query_orientation);
DECLARE(gtp_quit);
DECLARE(gtp_reg_genmove);
DECLARE(gtp_report_uncertainty);
DECLARE(gtp_reset_connection_node_counter);
DECLARE(gtp_reset_life_node_counter);
DECLARE(gtp_reset_owl_node_counter);
DECLARE(gtp_reset_reading_node_counter);
DECLARE(gtp_reset_search_mask);
DECLARE(gtp_reset_trymove_counter);
DECLARE(gtp_restricted_genmove);
DECLARE(gtp_same_dragon);
DECLARE(gtp_set_boardsize);
DECLARE(gtp_set_free_handicap);
DECLARE(gtp_set_komi);
DECLARE(gtp_set_level);
DECLARE(gtp_set_orientation);
DECLARE(gtp_set_random_seed);
DECLARE(gtp_set_search_diamond);
DECLARE(gtp_set_search_limit);
DECLARE(gtp_showboard);
DECLARE(gtp_start_sgftrace);
DECLARE(gtp_surround_map);
DECLARE(gtp_tactical_analyze_semeai);
DECLARE(gtp_test_eyeshape);
DECLARE(gtp_time_left);
DECLARE(gtp_time_settings);
DECLARE(gtp_top_moves);
DECLARE(gtp_top_moves_black);
DECLARE(gtp_top_moves_white);
DECLARE(gtp_tryko);
DECLARE(gtp_trymove);
DECLARE(gtp_tune_move_ordering);
DECLARE(gtp_unconditional_status);
DECLARE(gtp_undo);
DECLARE(gtp_what_color);
DECLARE(gtp_worm_cutstone);
DECLARE(gtp_worm_data);
DECLARE(gtp_worm_stones);

/* List of known commands. */
static struct gtp_command commands[] = {
  {"aa_confirm_safety",       gtp_aa_confirm_safety},
  {"accurate_approxlib",      gtp_accurate_approxlib},
  {"accuratelib",             gtp_accuratelib},
  {"advance_random_seed",     gtp_advance_random_seed},
  {"all_legal",        	      gtp_all_legal},
  {"all_move_values",         gtp_all_move_values},
  {"analyze_eyegraph", 	      gtp_analyze_eyegraph},
  {"analyze_semeai",          gtp_analyze_semeai},
  {"analyze_semeai_after_move", gtp_analyze_semeai_after_move},
  {"attack",           	      gtp_attack},
  {"attack_either",           gtp_attack_either},
  {"black",            	      gtp_playblack},
  {"block_off",		      gtp_block_off},
  {"boardsize",        	      gtp_set_boardsize},
  {"break_in",		      gtp_break_in},
  {"captures",        	      gtp_captures},
  {"clear_board",      	      gtp_clear_board},
  {"clear_cache",	      gtp_clear_cache},
  {"color",            	      gtp_what_color},
  {"combination_attack",      gtp_combination_attack},
  {"combination_defend",      gtp_combination_defend},
  {"connect",         	      gtp_connect},
  {"countlib",         	      gtp_countlib},
  {"cputime",		      gtp_cputime},
  {"decrease_depths",  	      gtp_decrease_depths},
  {"defend",           	      gtp_defend},
  {"defend_both",	      gtp_defend_both},
  {"disconnect",       	      gtp_disconnect},
  {"does_attack",             gtp_does_attack},
  {"does_defend",             gtp_does_defend},
  {"does_surround",           gtp_does_surround},
  {"dragon_data",             gtp_dragon_data},
  {"dragon_status",    	      gtp_dragon_status},
  {"dragon_stones",           gtp_dragon_stones},
  {"draw_search_area",        gtp_draw_search_area},
  {"dump_stack",       	      gtp_dump_stack},
  {"echo" ,                   gtp_echo},
  {"echo_err" ,               gtp_echo_err},
  {"estimate_score",          gtp_estimate_score},
  {"eval_eye",         	      gtp_eval_eye},
  {"experimental_score",      gtp_experimental_score},
  {"eye_data",                gtp_eye_data},
  {"final_score",             gtp_final_score},
  {"final_status",            gtp_final_status},
  {"final_status_list",       gtp_final_status_list},
  {"findlib",          	      gtp_findlib},
  {"finish_sgftrace",  	      gtp_finish_sgftrace},
  {"fixed_handicap",   	      gtp_fixed_handicap},
  {"followup_influence",      gtp_followup_influence},
  {"genmove",                 gtp_genmove},
  {"genmove_black",           gtp_genmove_black},
  {"genmove_white",           gtp_genmove_white},
  {"get_connection_node_counter", gtp_get_connection_node_counter},
  {"get_handicap",   	      gtp_get_handicap},
  {"get_komi",        	      gtp_get_komi},
  {"get_life_node_counter",   gtp_get_life_node_counter},
  {"get_owl_node_counter",    gtp_get_owl_node_counter},
  {"get_random_seed",  	      gtp_get_random_seed},
  {"get_reading_node_counter", gtp_get_reading_node_counter},
  {"get_trymove_counter",     gtp_get_trymove_counter},
  {"gg-undo",                 gtp_gg_undo},
  {"gg_genmove",              gtp_gg_genmove},
  {"half_eye_data",           gtp_half_eye_data},
  {"help",                    gtp_list_commands},
  {"increase_depths",  	      gtp_increase_depths},
  {"initial_influence",       gtp_initial_influence},
  {"invariant_hash_for_moves",gtp_invariant_hash_for_moves},
  {"invariant_hash",   	      gtp_invariant_hash},
  {"is_legal",         	      gtp_is_legal},
  {"is_surrounded",           gtp_is_surrounded},
  {"kgs-genmove_cleanup",     gtp_kgs_genmove_cleanup},
  {"known_command",    	      gtp_known_command},
  {"komi",        	      gtp_set_komi},
  {"ladder_attack",    	      gtp_ladder_attack},
  {"last_move",    	      gtp_last_move},
  {"level",        	      gtp_set_level},
  {"limit_search",     	      gtp_limit_search},
  {"list_commands",    	      gtp_list_commands},
  {"list_stones",    	      gtp_list_stones},
  {"loadsgf",          	      gtp_loadsgf},
  {"move_influence",          gtp_move_influence},
  {"move_probabilities",      gtp_move_probabilities},
  {"move_reasons",            gtp_move_reasons},
  {"move_uncertainty",	      gtp_move_uncertainty},
  {"move_history",	      gtp_move_history},
  {"name",                    gtp_name},
  {"new_score",               gtp_estimate_score},
  {"orientation",     	      gtp_set_orientation},
  {"owl_attack",     	      gtp_owl_attack},
  {"owl_connection_defends",  gtp_owl_connection_defends},
  {"owl_defend",     	      gtp_owl_defend},
  {"owl_does_attack", 	      gtp_owl_does_attack},
  {"owl_does_defend", 	      gtp_owl_does_defend},
  {"owl_substantial", 	      gtp_owl_substantial},
  {"owl_threaten_attack",     gtp_owl_threaten_attack},
  {"owl_threaten_defense",    gtp_owl_threaten_defense},
  {"place_free_handicap",     gtp_place_free_handicap},
  {"play",            	      gtp_play},
  {"popgo",            	      gtp_popgo},
  {"printsgf",         	      gtp_printsgf},
  {"protocol_version",        gtp_protocol_version},
  {"query_boardsize",         gtp_query_boardsize},
  {"query_orientation",       gtp_query_orientation},
  {"quit",             	      gtp_quit},
  {"reg_genmove",             gtp_reg_genmove},
  {"report_uncertainty",      gtp_report_uncertainty},
  {"reset_connection_node_counter", gtp_reset_connection_node_counter},
  {"reset_life_node_counter", gtp_reset_life_node_counter},
  {"reset_owl_node_counter",  gtp_reset_owl_node_counter},
  {"reset_reading_node_counter", gtp_reset_reading_node_counter},
  {"reset_search_mask",       gtp_reset_search_mask},
  {"reset_trymove_counter",   gtp_reset_trymove_counter},
  {"restricted_genmove",      gtp_restricted_genmove},
  {"same_dragon",    	      gtp_same_dragon},
  {"set_free_handicap",       gtp_set_free_handicap},
  {"set_random_seed",  	      gtp_set_random_seed},
  {"set_search_diamond",      gtp_set_search_diamond},
  {"set_search_limit",        gtp_set_search_limit},
  {"showboard",        	      gtp_showboard},
  {"start_sgftrace",  	      gtp_start_sgftrace},
  {"surround_map",            gtp_surround_map},
  {"tactical_analyze_semeai", gtp_tactical_analyze_semeai},
  {"test_eyeshape",           gtp_test_eyeshape},
  {"time_left",               gtp_time_left},
  {"time_settings",           gtp_time_settings},
  {"top_moves",               gtp_top_moves},
  {"top_moves_black",         gtp_top_moves_black},
  {"top_moves_white",         gtp_top_moves_white},
  {"tryko",          	      gtp_tryko},
  {"trymove",          	      gtp_trymove},
  {"tune_move_ordering",      gtp_tune_move_ordering},
  {"unconditional_status",    gtp_unconditional_status},
  {"undo",                    gtp_undo},
  {"version",                 gtp_program_version},
  {"white",            	      gtp_playwhite},
  {"worm_cutstone",           gtp_worm_cutstone},
  {"worm_data",               gtp_worm_data},
  {"worm_stones",             gtp_worm_stones},
  {NULL,                      NULL}
};


/* Start playing using the Go Text Protocol. */
void
play_gtp(FILE *gtp_input, FILE *gtp_output, FILE *gtp_dump_commands,
	 int gtp_initial_orientation)
{
  /* Make sure `gtp_output' is unbuffered. (Line buffering is also
   * okay but not necessary. Block buffering breaks GTP mode.)
   *
   * FIXME: Maybe should go to `gtp.c'?
   */
  setbuf(gtp_output, NULL);

  /* Inform the GTP utility functions about the board size. */
  gtp_internal_set_boardsize(board_size);
  gtp_orientation = gtp_initial_orientation;
  gtp_set_vertex_transform_hooks(rotate_on_input, rotate_on_output);

  /* Initialize time handling. */
  init_timers();
  
  /* Prepare pattern matcher and reading code. */
  reset_engine();
  clearstats();
  gtp_main_loop(commands, gtp_input, gtp_output, gtp_dump_commands);
  if (showstatistics)
    showstats();
}


/****************************
 * Administrative commands. *
 ****************************/

/* Function:  Quit
 * Arguments: none
 * Fails:     never
 * Returns:   nothing
 *
 * Status:    GTP version 2 standard command.
 */
static int
gtp_quit(char *s)
{
  UNUSED(s);
  gtp_success("");
  return GTP_QUIT;
}


/* Function:  Report protocol version.
 * Arguments: none
 * Fails:     never
 * Returns:   protocol version number
 *
 * Status:    GTP version 2 standard command.
 */
static int
gtp_protocol_version(char *s)
{
  UNUSED(s);
  return gtp_success("%d", gtp_version);
}


/****************************
 * Program identity.        *
 ****************************/

/* Function:  Report the name of the program.
 * Arguments: none
 * Fails:     never
 * Returns:   program name
 *
 * Status:    GTP version 2 standard command.
 */
static int
gtp_name(char *s)
{
  UNUSED(s);
  return gtp_success("GNU Go");
}




/* Function:  Report the version number of the program.
 * Arguments: none
 * Fails:     never
 * Returns:   version number
 *
 * Status:    GTP version 2 standard command.
 */
static int
gtp_program_version(char *s)
{
  UNUSED(s);
  return gtp_success(VERSION);
}


/***************************
 * Setting the board size. *
 ***************************/

/* Function:  Set the board size to NxN and clear the board.
 * Arguments: integer
 * Fails:     board size outside engine's limits
 * Returns:   nothing
 *
 * Status:    GTP version 2 standard command.
 */
static int
gtp_set_boardsize(char *s)
{
  int boardsize;

  if (sscanf(s, "%d", &boardsize) < 1)
    return gtp_failure("boardsize not an integer");
  
  if (!check_boardsize(boardsize, NULL)) {
    if (gtp_version == 1)
      return gtp_failure("unacceptable boardsize");
    else
      return gtp_failure("unacceptable size");
  }

  /* If this is called with a non-empty board, we assume that a new
   * game will be started, for which we want a new random seed.
   */
  if (stones_on_board(BLACK | WHITE) > 0)
    update_random_seed();

  board_size = boardsize;
  clear_board();
  gtp_internal_set_boardsize(boardsize);
  reset_engine();
  return gtp_success("");
}

/* Function:  Find the current boardsize
 * Arguments: none
 * Fails:     never
 * Returns:   board_size
 */
static int
gtp_query_boardsize(char *s)
{
  UNUSED(s);

  return gtp_success("%d", board_size);
}

/***********************
 * Clearing the board. *
 ***********************/

/* Function:  Clear the board.
 * Arguments: none
 * Fails:     never
 * Returns:   nothing
 *
 * Status:    GTP version 2 standard command.
 */
static int
gtp_clear_board(char *s)
{
  UNUSED(s);

  /* If this is called with a non-empty board, we assume that a new
   * game will be started, for which we want a new random seed.
   */
  if (stones_on_board(BLACK | WHITE) > 0)
    update_random_seed();

  clear_board();
  init_timers();
  
  return gtp_success("");
}

/****************************
 * Setting the orientation. *
 ****************************/

/* Function:  Set the orienation to N and clear the board
 * Arguments: integer
 * Fails:     illegal orientation
 * Returns:   nothing
 */
static int
gtp_set_orientation(char *s)
{
  int orientation;
  if (sscanf(s, "%d", &orientation) < 1)
    return gtp_failure("orientation not an integer");
  
  if (orientation < 0 || orientation > 7)
    return gtp_failure("unacceptable orientation");

  clear_board();
  gtp_orientation = orientation;
  gtp_set_vertex_transform_hooks(rotate_on_input, rotate_on_output);
  return gtp_success("");
}

/* Function:  Find the current orientation
 * Arguments: none
 * Fails:     never
 * Returns:   orientation
 */
static int
gtp_query_orientation(char *s)
{
  UNUSED(s);

  return gtp_success("%d", gtp_orientation);
}

/***************************
 * Setting komi.           *
 ***************************/

/* Function:  Set the komi.
 * Arguments: float
 * Fails:     incorrect argument
 * Returns:   nothing
 *
 * Status:    GTP version 2 standard command.
 */
static int
gtp_set_komi(char *s)
{
  if (sscanf(s, "%f", &komi) < 1)
    return gtp_failure("komi not a float");
  
  return gtp_success("");
}


/***************************
 * Getting komi            *
 ***************************/

/* Function:  Get the komi
 * Arguments: none
 * Fails:     never
 * Returns:   Komi 
 */
static int
gtp_get_komi(char *s)
{
  UNUSED(s);
  return gtp_success("%4.1f", komi);
}


/******************
 * Playing moves. *
 ******************/

/* Function:  Play a black stone at the given vertex.
 * Arguments: vertex
 * Fails:     invalid vertex, illegal move
 * Returns:   nothing
 *
 * Status:    Obsolete GTP version 1 command.
 */
static int
gtp_playblack(char *s)
{
  int i, j;
  char *c;

  for (c = s; *c; c++)
    *c = tolower((int)*c);

  if (strncmp(s, "pass", 4) == 0) {
    i = -1;
    j = -1;
  }
  else if (!gtp_decode_coord(s, &i, &j))
    return gtp_failure("invalid coordinate");

  if (!is_allowed_move(POS(i, j), BLACK))
    return gtp_failure("illegal move");

  gnugo_play_move(POS(i, j), BLACK);
  return gtp_success("");
}


/* Function:  Play a white stone at the given vertex.
 * Arguments: vertex
 * Fails:     invalid vertex, illegal move
 * Returns:   nothing
 *
 * Status:    Obsolete GTP version 1 command.
 */
static int
gtp_playwhite(char *s)
{
  int i, j;
  char *c;

  for (c = s; *c; c++)
    *c = tolower((int)*c);

  if (strncmp(s, "pass", 4) == 0) {
    i = -1;
    j = -1;
  }
  else if (!gtp_decode_coord(s, &i, &j))
    return gtp_failure("invalid coordinate");
  
  if (!is_allowed_move(POS(i, j), WHITE))
    return gtp_failure("illegal move");

  gnugo_play_move(POS(i, j), WHITE);
  return gtp_success("");
}


/* Function:  Play a stone of the given color at the given vertex.
 * Arguments: color, vertex
 * Fails:     invalid vertex, illegal move
 * Returns:   nothing
 *
 * Status:    GTP version 2 standard command.
 */
static int
gtp_play(char *s)
{
  int i, j;
  int color;

  if (!gtp_decode_move(s, &color, &i, &j))
    return gtp_failure("invalid color or coordinate");

  if (!is_allowed_move(POS(i, j), color))
    return gtp_failure("illegal move");

  gnugo_play_move(POS(i, j), color);
  return gtp_success("");
}


/* Function:  Set up fixed placement handicap stones.
 * Arguments: number of handicap stones
 * Fails:     invalid number of stones for the current boardsize
 * Returns:   list of vertices with handicap stones
 *
 * Status:    GTP version 2 standard command.
 */
static int
gtp_fixed_handicap(char *s)
{
  int m, n;
  int first = 1;
  int this_handicap;

  if (gtp_version == 1)
    clear_board();
  else if (stones_on_board(BLACK | WHITE) > 0)
    return gtp_failure("board not empty");

  if (sscanf(s, "%d", &this_handicap) < 1)
    return gtp_failure("handicap not an integer");
  
  if (this_handicap < 2 && (gtp_version > 1 || this_handicap != 0))
    return gtp_failure("invalid handicap");

  if (place_fixed_handicap(this_handicap) != this_handicap) {
    clear_board();
    return gtp_failure("invalid handicap");
  }

  handicap = this_handicap;

  gtp_start_response(GTP_SUCCESS);

  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++)
      if (BOARD(m, n) != EMPTY) {
	if (!first)
	  gtp_printf(" ");
	else
	  first = 0;
	gtp_mprintf("%m", m, n);
      }
  
  return gtp_finish_response();
}


/* Function:  Choose free placement handicap stones and put them on the board.
 * Arguments: number of handicap stones
 * Fails:     invalid number of stones
 * Returns:   list of vertices with handicap stones
 *
 * Status:    GTP version 2 standard command.
 */
static int
gtp_place_free_handicap(char *s)
{
  int m, n;
  int first = 1;
  int this_handicap;
  if (sscanf(s, "%d", &this_handicap) < 1)
    return gtp_failure("handicap not an integer");
  
  if (stones_on_board(BLACK | WHITE) > 0)
    return gtp_failure("board not empty");

  if (this_handicap < 2)
    return gtp_failure("invalid handicap");

  handicap = place_free_handicap(this_handicap);

  gtp_start_response(GTP_SUCCESS);

  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++)
      if (BOARD(m, n) != EMPTY) {
	if (!first)
	  gtp_printf(" ");
	else
	  first = 0;
	gtp_mprintf("%m", m, n);
      }
  
  return gtp_finish_response();
}


/* Function:  Put free placement handicap stones on the board.
 * Arguments: list of vertices with handicap stones
 * Fails:     board not empty, bad list of vertices
 * Returns:   nothing
 *
 * Status:    GTP version 2 standard command.
 */
static int
gtp_set_free_handicap(char *s)
{
  int n;
  int i, j;
  int k;
  
  if (stones_on_board(BLACK | WHITE) > 0)
    return gtp_failure("board not empty");

  for (k = 0; k < MAX_BOARD * MAX_BOARD; k++) {
    n = gtp_decode_coord(s, &i, &j);
    if (n > 0) {
      if (board[POS(i, j)] != EMPTY) {
	clear_board();
	return gtp_failure("repeated vertex");
      }
      add_stone(POS(i, j), BLACK);
      s += n;
    }
    else if (sscanf(s, "%*s") != EOF)
      return gtp_failure("invalid coordinate");
    else
      break;
  }

  if (k < 2) {
    clear_board();
    return gtp_failure("invalid handicap");
  }

  handicap = k;
  
  return gtp_success("");
}


/* Function:  Get the handicap
 * Arguments: none
 * Fails:     never
 * Returns:   handicap
 */
static int
gtp_get_handicap(char *s)
{
  UNUSED(s);
  return gtp_success("%d", handicap);
}


/* Function:  Load an sgf file, possibly up to a move number or the first
 *            occurence of a move.           
 * Arguments: filename + move number, vertex, or nothing
 * Fails:     missing filename or failure to open or parse file
 * Returns:   color to play
 *
 * Status:    GTP version 2 standard command.
 */
static int
gtp_loadsgf(char *s)
{
  char filename[GTP_BUFSIZE];
  char untilstring[GTP_BUFSIZE];
  SGFTree sgftree;
  Gameinfo gameinfo;
  int nread;
  int color_to_move;
  
  nread = sscanf(s, "%s %s", filename, untilstring);
  if (nread < 1)
    return gtp_failure("missing filename");

  sgftree_clear(&sgftree);
  if (!sgftree_readfile(&sgftree, filename))
    return gtp_failure("cannot open or parse '%s'", filename);

  if (nread == 1)
    color_to_move = gameinfo_play_sgftree_rot(&gameinfo, &sgftree, NULL,
					      gtp_orientation);
  else
    color_to_move = gameinfo_play_sgftree_rot(&gameinfo, &sgftree, untilstring,
                                              gtp_orientation);

  if (color_to_move == EMPTY)
    return gtp_failure("cannot load '%s'", filename);
  
  gtp_internal_set_boardsize(board_size);
  reset_engine();
  init_timers();

  sgfFreeNode(sgftree.root);

  gtp_start_response(GTP_SUCCESS);
  gtp_mprintf("%C", color_to_move);
  return gtp_finish_response();
}


/*****************
 * Board status. *
 *****************/

/* Function:  Return the color at a vertex.
 * Arguments: vertex
 * Fails:     invalid vertex
 * Returns:   "black", "white", or "empty"
 */
static int
gtp_what_color(char *s)
{
  int i, j;
  if (!gtp_decode_coord(s, &i, &j))
    return gtp_failure("invalid coordinate");
  
  return gtp_success(color_to_string(BOARD(i, j)));
}


/* Function:  List vertices with either black or white stones.
 * Arguments: color
 * Fails:     invalid color
 * Returns:   list of vertices
 */
static int
gtp_list_stones(char *s)
{
  int i, j;
  int color = EMPTY;
  int vertexi[MAX_BOARD * MAX_BOARD];
  int vertexj[MAX_BOARD * MAX_BOARD];
  int vertices = 0;
  
  if (!gtp_decode_color(s, &color))
    return gtp_failure("invalid color");

  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++)
      if (BOARD(i, j) == color) {
	vertexi[vertices] = i;
	vertexj[vertices++] = j;
      }

  gtp_start_response(GTP_SUCCESS);
  gtp_print_vertices(vertices, vertexi, vertexj);
  return gtp_finish_response();
}


/* Function:  Count number of liberties for the string at a vertex.
 * Arguments: vertex
 * Fails:     invalid vertex, empty vertex
 * Returns:   Number of liberties.
 */
static int
gtp_countlib(char *s)
{
  int i, j;
  if (!gtp_decode_coord(s, &i, &j))
    return gtp_failure("invalid coordinate");

  if (BOARD(i, j) == EMPTY)
    return gtp_failure("vertex must not be empty");

  return gtp_success("%d", countlib(POS(i, j)));
}


/* Function:  Return the positions of the liberties for the string at a vertex.
 * Arguments: vertex
 * Fails:     invalid vertex, empty vertex
 * Returns:   Sorted space separated list of vertices.
 */
static int
gtp_findlib(char *s)
{
  int i, j;
  int libs[MAXLIBS];
  int liberties;
  
  if (!gtp_decode_coord(s, &i, &j))
    return gtp_failure("invalid coordinate");

  if (BOARD(i, j) == EMPTY)
    return gtp_failure("vertex must not be empty");

  liberties = findlib(POS(i, j), MAXLIBS, libs);
  gtp_start_response(GTP_SUCCESS);
  gtp_print_vertices2(liberties, libs);
  return gtp_finish_response();
}


/* Function:  Determine which liberties a stone of given color
 *            will get if played at given vertex.
 * Arguments: move (color + vertex)
 * Fails:     invalid color, invalid vertex, occupied vertex
 * Returns:   Sorted space separated list of liberties
 */
static int
gtp_accuratelib(char *s)
{
  int i, j;
  int color;
  int libs[MAXLIBS];
  int liberties;

  if (!gtp_decode_move(s, &color, &i, &j))
    return gtp_failure("invalid color or coordinate");

  if (BOARD(i, j) != EMPTY)
    return gtp_failure("vertex must be empty");

  liberties = accuratelib(POS(i, j), color, MAXLIBS, libs);

  gtp_start_response(GTP_SUCCESS);
  gtp_print_vertices2(liberties, libs);
  return gtp_finish_response();
}


/* Function:  Determine which liberties a stone of given color
 *            will get if played at given vertex.
 * Arguments: move (color + vertex)
 * Fails:     invalid color, invalid vertex, occupied vertex
 * Returns:   Sorted space separated list of liberties
 *
 * Supposedly identical in behavior to the above function and
 * can be retired when this is confirmed.
 */
static int
gtp_accurate_approxlib(char *s)
{
  int i, j;
  int color;
  int libs[MAXLIBS];
  int liberties;

  if (!gtp_decode_move(s, &color, &i, &j))
    return gtp_failure("invalid color or coordinate");

  if (BOARD(i, j) != EMPTY)
    return gtp_failure("vertex must be empty");

  liberties = accuratelib(POS(i, j), color, MAXLIBS, libs);

  gtp_start_response(GTP_SUCCESS);
  gtp_print_vertices2(liberties, libs);
  return gtp_finish_response();
}


/* Function:  Tell whether a move is legal.
 * Arguments: move
 * Fails:     invalid move
 * Returns:   1 if the move is legal, 0 if it is not.
 */
static int
gtp_is_legal(char *s)
{
  int i, j;
  int color;
  
  if (!gtp_decode_move(s, &color, &i, &j))
    return gtp_failure("invalid color or coordinate");

  return gtp_success("%d", is_allowed_move(POS(i, j), color));
}


/* Function:  List all legal moves for either color.
 * Arguments: color
 * Fails:     invalid color
 * Returns:   Sorted space separated list of vertices.
 */
static int
gtp_all_legal(char *s)
{
  int i, j;
  int color;
  int movei[MAX_BOARD * MAX_BOARD];
  int movej[MAX_BOARD * MAX_BOARD];
  int moves = 0;
  
  if (!gtp_decode_color(s, &color))
    return gtp_failure("invalid color");

  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++)
      if (BOARD(i, j) == EMPTY && is_allowed_move(POS(i, j), color)) {
	movei[moves] = i;
	movej[moves++] = j;
      }

  gtp_start_response(GTP_SUCCESS);
  gtp_print_vertices(moves, movei, movej);
  return gtp_finish_response();
}


/* Function:  List the number of captures taken by either color.
 * Arguments: color
 * Fails:     invalid color
 * Returns:   Number of captures.
 */
static int
gtp_captures(char *s)
{
  int color;
  
  if (!gtp_decode_color(s, &color))
    return gtp_failure("invalid color");

  if (color == BLACK)
    return gtp_success("%d", white_captured);
  else
    return gtp_success("%d", black_captured);
}


/* Function:  Return the last move.
 * Arguments: none
 * Fails:     no previous move known
 * Returns:   Color and vertex of last move.
 */
static int
gtp_last_move(char *s)
{
  int pos;
  int color;
  UNUSED(s);
  
  if (move_history_pointer <= 0)
    return gtp_failure("no previous move known");
  
  pos = move_history_pos[move_history_pointer - 1];
  color = move_history_color[move_history_pointer - 1];
  
  gtp_start_response(GTP_SUCCESS);
  gtp_mprintf("%C %m", color, I(pos), J(pos));
  return gtp_finish_response();
}

/* Function:  Print the move history in reverse order
 * Arguments: none
 * Fails:     never
 * Returns:   List of moves played in reverse order in format: 
 *            color move (one move per line)
 */
static int
gtp_move_history(char *s)
{
  int k, pos, color;
  UNUSED(s);
  
  gtp_start_response(GTP_SUCCESS);
  if (move_history_pointer > 0)
    for (k = move_history_pointer-1; k >= 0; k--) {
      color = move_history_color[k];
      pos = move_history_pos[k];
      gtp_mprintf("%C %m\n", color, I(pos), J(pos));
    }
  else
    gtp_printf("\n");
  gtp_printf("\n");
  return GTP_OK;
}


/* Function:  Return the rotation/reflection invariant board hash.
 * Arguments: none
 * Fails:     never
 * Returns:   Invariant hash for the board as a hexadecimal number.
 */
static int
gtp_invariant_hash(char *s)
{
  Hash_data hash;
  UNUSED(s);
  hashdata_calc_orientation_invariant(&hash, board, board_ko_pos);
  return gtp_success("%s", hashdata_to_string(&hash));
}


/* Function:  Return the rotation/reflection invariant board hash
 *            obtained by playing all the possible moves for the
 *            given color.
 * Arguments: color
 * Fails:     invalid color
 * Returns:   List of moves + invariant hash as a hexadecimal number,
 *            one pair of move + hash per line.
 */
static int
gtp_invariant_hash_for_moves(char *s)
{
  Hash_data hash;
  int color;
  int pos;
  int move_found = 0;
  
  if (!gtp_decode_color(s, &color))
    return gtp_failure("invalid color");

  gtp_start_response(GTP_SUCCESS);

  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (board[pos] == EMPTY
	&& trymove(pos, color, "gtp_invariant_hash_for_moves", NO_MOVE)) {
      hashdata_calc_orientation_invariant(&hash, board, board_ko_pos);
      gtp_mprintf("%m %s\n", I(pos), J(pos), hashdata_to_string(&hash));
      popgo();
      move_found = 1;
    }
  }

  if (!move_found)
    gtp_printf("\n");
  
  gtp_printf("\n");
  return GTP_OK;
}



/**********************
 * Retractable moves. *
 **********************/

/* Function:  Play a stone of the given color at the given vertex.
 * Arguments: move (color + vertex)
 * Fails:     invalid color, invalid vertex, illegal move
 * Returns:   nothing
 */
static int
gtp_trymove(char *s)
{
  int i, j;
  int color;
  if (!gtp_decode_move(s, &color, &i, &j))
    return gtp_failure("invalid color or coordinate");

  if (!trymove(POS(i, j), color, "gtp_trymove", NO_MOVE))
    return gtp_failure("illegal move");

  return gtp_success("");
}

/* Function:  Play a stone of the given color at the given vertex, 
 *            allowing illegal ko capture.
 * Arguments: move (color + vertex)
 * Fails:     invalid color, invalid vertex, illegal move
 * Returns:   nothing
 */
static int
gtp_tryko(char *s)
{
  int i, j;
  int color;
  if (!gtp_decode_move(s, &color, &i, &j) || POS(i, j) == PASS_MOVE)
    return gtp_failure("invalid color or coordinate");

  if (!tryko(POS(i, j), color, "gtp_tryko"))
    return gtp_failure("illegal move");

  return gtp_success("");
}


/* Function:  Undo a trymove or tryko.
 * Arguments: none
 * Fails:     stack empty
 * Returns:   nothing
 */
static int
gtp_popgo(char *s)
{
  UNUSED(s);

  if (stackp == 0)
    return gtp_failure("Stack empty.");

  popgo();
  return gtp_success("");
}

/*********************
 * Caching	     *
 *********************/

/* Function:  clear the caches.
 * Arguments: none.
 * Fails:     never.
 * Returns:   nothing.
 */

static int
gtp_clear_cache(char *s)
{
  UNUSED(s);
  clear_persistent_caches();
  reading_cache_clear();
  return gtp_success("");
}

/*********************
 * Tactical reading. *
 *********************/

/* Function:  Try to attack a string.
 * Arguments: vertex
 * Fails:     invalid vertex, empty vertex
 * Returns:   attack code followed by attack point if attack code nonzero.
 */
static int
gtp_attack(char *s)
{
  int i, j;
  int apos;
  int attack_code;
  
  if (!gtp_decode_coord(s, &i, &j))
    return gtp_failure("invalid coordinate");

  if (BOARD(i, j) == EMPTY)
    return gtp_failure("vertex must not be empty");

  attack_code = attack(POS(i, j), &apos);
  gtp_start_response(GTP_SUCCESS);
  gtp_print_code(attack_code);
  if (attack_code > 0) {
    gtp_printf(" ");
    gtp_print_vertex(I(apos), J(apos));
  }
  return gtp_finish_response();
}  


/* Function:  Try to attack either of two strings
 * Arguments: two vertices
 * Fails:     invalid vertex, empty vertex
 * Returns:   attack code against the strings.  Guarantees there
 *            exists a move which will attack one of the two
 *            with attack_code, but does not return the move.
 */
static int
gtp_attack_either(char *s)
{
  int ai, aj;
  int bi, bj;
  int n;
  int acode;

  n = gtp_decode_coord(s, &ai, &aj);
  if (n == 0)
    return gtp_failure("invalid coordinate");

  if (BOARD(ai, aj) == EMPTY)
    return gtp_failure("string vertex must be empty");

  n = gtp_decode_coord(s + n, &bi, &bj);
  if (n == 0)
    return gtp_failure("invalid coordinate");

  if (BOARD(bi, bj) == EMPTY)
    return gtp_failure("string vertex must not be empty");

  acode = attack_either(POS(ai, aj), POS(bi, bj));

  gtp_start_response(GTP_SUCCESS);
  gtp_print_code(acode);
  return gtp_finish_response();
}


/* Function:  Try to defend a string.
 * Arguments: vertex
 * Fails:     invalid vertex, empty vertex
 * Returns:   defense code followed by defense point if defense code nonzero.
 */
static int
gtp_defend(char *s)
{
  int i, j;
  int dpos;
  int defend_code;
  
  if (!gtp_decode_coord(s, &i, &j))
    return gtp_failure("invalid coordinate");

  if (BOARD(i, j) == EMPTY)
    return gtp_failure("vertex must not be empty");

  defend_code = find_defense(POS(i, j), &dpos);
  gtp_start_response(GTP_SUCCESS);
  gtp_print_code(defend_code);
  if (defend_code > 0) {
    gtp_printf(" ");
    gtp_print_vertex(I(dpos), J(dpos));
  }
  return gtp_finish_response();
}  


/* Function:  Examine whether a specific move attacks a string tactically.
 * Arguments: vertex (move), vertex (dragon)
 * Fails:     invalid vertex, empty vertex
 * Returns:   attack code
 */
static int
gtp_does_attack(char *s)
{
  int i, j;
  int ti, tj;
  int attack_code;
  int n;

  n = gtp_decode_coord(s, &ti, &tj);
  if (n == 0)
    return gtp_failure("invalid coordinate");

  if (BOARD(ti, tj) != EMPTY)
    return gtp_failure("move vertex must be empty");

  n = gtp_decode_coord(s + n, &i, &j);
  if (n == 0)
    return gtp_failure("invalid coordinate");

  if (BOARD(i, j) == EMPTY)
    return gtp_failure("string vertex must not be empty");

  /* to get the variations into the sgf file, clear the reading cache */
  if (sgf_dumptree)
    reading_cache_clear();
  
  attack_code = does_attack(POS(ti, tj), POS(i, j));
  gtp_start_response(GTP_SUCCESS);
  gtp_print_code(attack_code);
  return gtp_finish_response();
}  


/* Function:  Examine whether a specific move defends a string tactically.
 * Arguments: vertex (move), vertex (dragon)
 * Fails:     invalid vertex, empty vertex
 * Returns:   attack code
 */
static int
gtp_does_defend(char *s)
{
  int i, j;
  int ti, tj;
  int defense_code;
  int n;

  n = gtp_decode_coord(s, &ti, &tj);
  if (n == 0)
    return gtp_failure("invalid coordinate");

  if (BOARD(ti, tj) != EMPTY)
    return gtp_failure("move vertex must be empty");

  n = gtp_decode_coord(s + n, &i, &j);
  if (n == 0)
    return gtp_failure("invalid coordinate");

  if (BOARD(i, j) == EMPTY)
    return gtp_failure("string vertex must not be empty");

  /* to get the variations into the sgf file, clear the reading cache */
  if (sgf_dumptree)
    reading_cache_clear();
  
  defense_code = does_defend(POS(ti, tj), POS(i, j));
  gtp_start_response(GTP_SUCCESS);
  gtp_print_code(defense_code);
  return gtp_finish_response();
}  


/* Function:  Try to attack a string strictly in a ladder.
 * Arguments: vertex
 * Fails:     invalid vertex, empty vertex
 * Returns:   attack code followed by attack point if attack code nonzero.
 */
static int
gtp_ladder_attack(char *s)
{
  int i, j;
  int apos;
  int attack_code;
  
  if (!gtp_decode_coord(s, &i, &j))
    return gtp_failure("invalid coordinate");

  if (BOARD(i, j) == EMPTY)
    return gtp_failure("vertex must not be empty");

  if (countlib(POS(i, j)) != 2)
    return gtp_failure("string must have exactly 2 liberties");

  attack_code = simple_ladder(POS(i, j), &apos);
  gtp_start_response(GTP_SUCCESS);
  gtp_print_code(attack_code);
  if (attack_code > 0) {
    gtp_printf(" ");
    gtp_print_vertex(I(apos), J(apos));
  }
  return gtp_finish_response();
}  


/* Function:  Increase depth values by one.
 * Arguments: none
 * Fails:     never
 * Returns:   nothing
 */
static int
gtp_increase_depths(char *s)
{
  UNUSED(s);
  increase_depth_values();
  return gtp_success("");
}  


/* Function:  Decrease depth values by one.
 * Arguments: none
 * Fails:     never
 * Returns:   nothing
 */
static int
gtp_decrease_depths(char *s)
{
  UNUSED(s);
  decrease_depth_values();
  return gtp_success("");
}  


/******************
 * owl reading. *
 ******************/

/* Function:  Try to attack a dragon.
 * Arguments: vertex
 * Fails:     invalid vertex, empty vertex
 * Returns:   attack code followed by attack point if attack code nonzero.
 */
static int
gtp_owl_attack(char *s)
{
  int i, j;
  int attack_point;
  int attack_code;
  int result_certain;
  int kworm;

  if (!gtp_decode_coord(s, &i, &j))
    return gtp_failure("invalid coordinate");

  if (BOARD(i, j) == EMPTY)
    return gtp_failure("vertex must not be empty");

  silent_examine_position(EXAMINE_DRAGONS_WITHOUT_OWL);
  
  /* to get the variations into the sgf file, clear the reading cache */
  if (sgf_dumptree)
    reading_cache_clear();
  
  attack_code = owl_attack(POS(i, j), &attack_point, &result_certain, &kworm);
  gtp_start_response(GTP_SUCCESS);
  gtp_print_code(attack_code);
  if (attack_code > 0) {
    gtp_printf(" ");
    gtp_print_vertex(I(attack_point), J(attack_point));
  }
  if (!result_certain && report_uncertainty)
    gtp_printf(" uncertain");
  return gtp_finish_response();
}  


/* Function:  Try to defend a dragon.
 * Arguments: vertex
 * Fails:     invalid vertex, empty vertex
 * Returns:   defense code followed by defense point if defense code nonzero.
 */
static int
gtp_owl_defend(char *s)
{
  int i, j;
  int defense_point;
  int defend_code;
  int result_certain;
  int kworm;
  
  if (!gtp_decode_coord(s, &i, &j))
    return gtp_failure("invalid coordinate");

  if (BOARD(i, j) == EMPTY)
    return gtp_failure("vertex must not be empty");

  silent_examine_position(EXAMINE_DRAGONS_WITHOUT_OWL);
  
  /* to get the variations into the sgf file, clear the reading cache */
  if (sgf_dumptree)
    reading_cache_clear();

  defend_code = owl_defend(POS(i, j), &defense_point, &result_certain, &kworm);
  gtp_start_response(GTP_SUCCESS);
  gtp_print_code(defend_code);
  if (defend_code > 0) {
    gtp_printf(" ");
    gtp_print_vertex(I(defense_point), J(defense_point));
  }
  if (!result_certain && report_uncertainty)
    gtp_printf(" uncertain");
  return gtp_finish_response();
}  

/* Function:  Try to attack a dragon in 2 moves.
 * Arguments: vertex
 * Fails:     invalid vertex, empty vertex
 * Returns:   attack code followed by the two attack points if
 *            attack code nonzero.
 */
static int
gtp_owl_threaten_attack(char *s)
{
  int i, j;
  int attack_point1;
  int attack_point2;
  int attack_code;

  if (!gtp_decode_coord(s, &i, &j))
    return gtp_failure("invalid coordinate");

  if (BOARD(i, j) == EMPTY)
    return gtp_failure("vertex must not be empty");

  silent_examine_position(EXAMINE_DRAGONS_WITHOUT_OWL);
  
  /* to get the variations into the sgf file, clear the reading cache */
  if (sgf_dumptree)
    reading_cache_clear();
  
  attack_code = owl_threaten_attack(POS(i, j), &attack_point1, &attack_point2);
  gtp_start_response(GTP_SUCCESS);
  gtp_print_code(attack_code);
  if (attack_code > 0) {
    gtp_printf(" ");
    gtp_print_vertex(I(attack_point1), J(attack_point1));
    gtp_printf(" ");
    gtp_print_vertex(I(attack_point2), J(attack_point2));
  }
  return gtp_finish_response();
}  


/* Function:  Try to defend a dragon with 2 moves.
 * Arguments: vertex
 * Fails:     invalid vertex, empty vertex
 * Returns:   defense code followed by the 2 defense points if
 *            defense code nonzero.
 */
static int
gtp_owl_threaten_defense(char *s)
{
  int i, j;
  int defense_point1;
  int defense_point2;
  int defend_code;
  
  if (!gtp_decode_coord(s, &i, &j))
    return gtp_failure("invalid coordinate");

  if (BOARD(i, j) == EMPTY)
    return gtp_failure("vertex must not be empty");

  silent_examine_position(EXAMINE_DRAGONS_WITHOUT_OWL);
  
  /* to get the variations into the sgf file, clear the reading cache */
  if (sgf_dumptree)
    reading_cache_clear();

  defend_code = owl_threaten_defense(POS(i, j), &defense_point1,
				     &defense_point2);
  gtp_start_response(GTP_SUCCESS);
  gtp_print_code(defend_code);
  if (defend_code > 0) {
    gtp_printf(" ");
    gtp_print_vertex(I(defense_point1), J(defense_point1));
    gtp_printf(" ");
    gtp_print_vertex(I(defense_point2), J(defense_point2));
  }
  return gtp_finish_response();
}  


/* Function:  Examine whether a specific move attacks a dragon.
 * Arguments: vertex (move), vertex (dragon)
 * Fails:     invalid vertex, empty vertex
 * Returns:   attack code
 */
static int
gtp_owl_does_attack(char *s)
{
  int i, j;
  int ti, tj;
  int attack_code;
  int kworm;
  int n;

  n = gtp_decode_coord(s, &ti, &tj);
  if (n == 0)
    return gtp_failure("invalid coordinate");

  if (BOARD(ti, tj) != EMPTY)
    return gtp_failure("move vertex must be empty");

  n = gtp_decode_coord(s + n, &i, &j);
  if (n == 0)
    return gtp_failure("invalid coordinate");

  if (BOARD(i, j) == EMPTY)
    return gtp_failure("dragon vertex must not be empty");

  silent_examine_position(EXAMINE_DRAGONS_WITHOUT_OWL);
  
  /* to get the variations into the sgf file, clear the reading cache */
  if (sgf_dumptree)
    reading_cache_clear();
  
  attack_code = owl_does_attack(POS(ti, tj), POS(i, j), &kworm);
  gtp_start_response(GTP_SUCCESS);
  gtp_print_code(attack_code);
  return gtp_finish_response();
}  


/* Function:  Examine whether a specific move defends a dragon.
 * Arguments: vertex (move), vertex (dragon)
 * Fails:     invalid vertex, empty vertex
 * Returns:   defense code
 */
static int
gtp_owl_does_defend(char *s)
{
  int i, j;
  int ti, tj;
  int defense_code;
  int kworm;
  int n;

  n = gtp_decode_coord(s, &ti, &tj);
  if (n == 0)
    return gtp_failure("invalid coordinate");

  if (BOARD(ti, tj) != EMPTY)
    return gtp_failure("move vertex must be empty");

  n = gtp_decode_coord(s + n, &i, &j);
  if (n == 0)
    return gtp_failure("invalid coordinate");

  if (BOARD(i, j) == EMPTY)
    return gtp_failure("dragon vertex must not be empty");

  silent_examine_position(EXAMINE_DRAGONS_WITHOUT_OWL);
  
  /* to get the variations into the sgf file, clear the reading cache */
  if (sgf_dumptree)
    reading_cache_clear();
  
  defense_code = owl_does_defend(POS(ti, tj), POS(i, j), &kworm);
  gtp_start_response(GTP_SUCCESS);
  gtp_print_code(defense_code);
  return gtp_finish_response();
}  


/* Function:  Examine whether a connection defends involved dragons.
 * Arguments: vertex (move), vertex (dragon1), vertex (dragon2)
 * Fails:     invalid vertex, empty vertex
 * Returns:   defense code
 */
static int
gtp_owl_connection_defends(char *s)
{
  int ai, aj;
  int bi, bj;
  int ti, tj;
  int defense_code;
  int n;

  n = gtp_decode_coord(s, &ti, &tj);
  if (n == 0)
    return gtp_failure("invalid coordinate");

  if (BOARD(ti, tj) != EMPTY)
    return gtp_failure("move vertex must be empty");

  s += n;
  n = gtp_decode_coord(s, &ai, &aj);
  if (n == 0)
    return gtp_failure("invalid coordinate");

  s += n;
  n = gtp_decode_coord(s, &bi, &bj);
  if (n == 0)
    return gtp_failure("invalid coordinate");

  if (BOARD(ai, aj) == EMPTY || BOARD(bi, bj) == EMPTY)
    return gtp_failure("dragon vertex must not be empty");

  if (BOARD(ai, aj) != BOARD(bi, bj))
    return gtp_failure("dragon vertices must have the same color");

  silent_examine_position(EXAMINE_DRAGONS_WITHOUT_OWL);
  
  /* to get the variations into the sgf file, clear the reading cache */
  if (sgf_dumptree)
    reading_cache_clear();
  
  defense_code = owl_connection_defends(POS(ti, tj), POS(ai, aj), POS(bi, bj));
  gtp_start_response(GTP_SUCCESS);
  gtp_print_code(defense_code);
  return gtp_finish_response();
}


/* Function:  Try to defend both of two strings
 * Arguments: two vertices
 * Fails:     invalid vertex, empty vertex
 * Returns:   defend code for the strings.  Guarantees there
 *            exists a move which will defend both of the two
 *            with defend_code, but does not return the move.
 */
static int
gtp_defend_both(char *s)
{
  int ai, aj;
  int bi, bj;
  int n;
  int dcode;

  n = gtp_decode_coord(s, &ai, &aj);
  if (n == 0)
    return gtp_failure("invalid coordinate");

  if (BOARD(ai, aj) == EMPTY)
    return gtp_failure("string vertex must be empty");

  n = gtp_decode_coord(s + n, &bi, &bj);
  if (n == 0)
    return gtp_failure("invalid coordinate");

  if (BOARD(bi, bj) == EMPTY)
    return gtp_failure("string vertex must not be empty");

  dcode = defend_both(POS(ai, aj), POS(bi, bj));

  gtp_start_response(GTP_SUCCESS);
  gtp_print_code(dcode);
  return gtp_finish_response();
}



/* Function:  Determine whether capturing a string gives a living dragon
 * Arguments: vertex
 * Fails:     invalid vertex, empty vertex
 * Returns:   1 if dragon can live, 0 otherwise
 */
static int
gtp_owl_substantial(char *s)
{
  int i, j;
  int result;
  
  if (!gtp_decode_coord(s, &i, &j))
    return gtp_failure("invalid coordinate");

  if (BOARD(i, j) == EMPTY)
    return gtp_failure("vertex must not be empty");

  silent_examine_position(EXAMINE_DRAGONS_WITHOUT_OWL);
  
  /* to get the variations into the sgf file, clear the reading cache */
  if (sgf_dumptree)
    reading_cache_clear();

  result = owl_substantial(POS(i, j));
  return gtp_success("%d", result);
}  


/* Function:  Analyze a semeai
 * Arguments: dragona, dragonb
 * Fails:     invalid vertices, empty vertices
 * Returns:   semeai defense result, semeai attack result, semeai move
 */
static int
gtp_analyze_semeai(char *s)
{
  int i, j;
  int k;
  int dragona, dragonb;
  int resulta, resultb, move, result_certain;
  
  k = gtp_decode_coord(s, &i, &j);

  if (k == 0)
    return gtp_failure("invalid coordinate");
  dragona = POS(i, j);
  if (BOARD(i, j) == EMPTY)
    return gtp_failure("vertex must not be empty");

  if (!gtp_decode_coord(s+k, &i, &j))
    return gtp_failure("invalid coordinate");
  dragonb = POS(i, j);
  if (BOARD(i, j) == EMPTY)
    return gtp_failure("vertex must not be empty");

  silent_examine_position(EXAMINE_DRAGONS_WITHOUT_OWL);
  /* to get the variations into the sgf file, clear the reading cache */
  if (sgf_dumptree)
    reading_cache_clear();

  owl_analyze_semeai(dragona, dragonb, &resulta, &resultb, &move, 1,
  		     &result_certain);
  gtp_start_response(GTP_SUCCESS);
  gtp_print_code(resulta);
  gtp_printf(" ");
  gtp_print_code(resultb);
  gtp_mprintf(" %m", I(move), J(move));
  if (!result_certain && report_uncertainty)
    gtp_printf(" uncertain");

  return gtp_finish_response();
}  


/* Function:  Analyze a semeai after a move have been made.
 * Arguments: color, vertex, dragona, dragonb
 * Fails:     invalid vertices
 * Returns:   semeai defense result, semeai attack result, semeai move
 */
static int
gtp_analyze_semeai_after_move(char *s)
{
  int i, j;
  int color;
  int move;
  int k;
  int dragona, dragonb;
  int resulta, resultb, semeai_move, result_certain;
  
  k = gtp_decode_move(s, &color, &i, &j);
  move = POS(i, j);
  if (k == 0 || move == NO_MOVE)
    return gtp_failure("invalid color or coordinate");
  if (board[move] != EMPTY)
    return gtp_failure("move vertex is not empty");
  s += k;
  
  k = gtp_decode_coord(s, &i, &j);
  if (k == 0)
    return gtp_failure("invalid coordinate");
  dragona = POS(i, j);
  if (board[dragona] == EMPTY)
    return gtp_failure("dragon vertex must not be empty");
  s += k;

  if (!gtp_decode_coord(s, &i, &j))
    return gtp_failure("invalid coordinate");
  dragonb = POS(i, j);
  if (board[dragonb] == EMPTY)
    return gtp_failure("dragon vertex must not be empty");

  silent_examine_position(EXAMINE_DRAGONS_WITHOUT_OWL);
  /* to get the variations into the sgf file, clear the reading cache */
  if (sgf_dumptree)
    reading_cache_clear();

  owl_analyze_semeai_after_move(move, color, dragona, dragonb,
				&resulta, &resultb, &semeai_move, 1,
				&result_certain, 0);
  gtp_start_response(GTP_SUCCESS);
  gtp_print_code(resulta);
  gtp_printf(" ");
  gtp_print_code(resultb);
  gtp_mprintf(" %m", I(semeai_move), J(semeai_move));
  if (!result_certain && report_uncertainty)
    gtp_printf(" uncertain");

  return gtp_finish_response();
}  


/* Function:  Analyze a semeai, not using owl
 * Arguments: dragona, dragonb
 * Fails:     invalid vertices, empty vertices
 * Returns:   status of dragona, dragonb assuming dragona moves first
 */
static int
gtp_tactical_analyze_semeai(char *s)
{
  int i, j;
  int k;
  int dragona, dragonb;
  int resulta, resultb, move, result_certain;
  
  k = gtp_decode_coord(s, &i, &j);

  if (k == 0)
    return gtp_failure("invalid coordinate");
  dragona = POS(i, j);
  if (BOARD(i, j) == EMPTY)
    return gtp_failure("vertex must not be empty");

  if (!gtp_decode_coord(s+k, &i, &j))
    return gtp_failure("invalid coordinate");
  dragonb = POS(i, j);
  if (BOARD(i, j) == EMPTY)
    return gtp_failure("vertex must not be empty");

  silent_examine_position(EXAMINE_DRAGONS_WITHOUT_OWL);
  /* to get the variations into the sgf file, clear the reading cache */
  if (sgf_dumptree)
    reading_cache_clear();

  owl_analyze_semeai(dragona, dragonb, &resulta, &resultb, &move, 0,
                     &result_certain);
  gtp_start_response(GTP_SUCCESS);
  gtp_print_code(resulta);
  gtp_printf(" ");
  gtp_print_code(resultb);
  gtp_mprintf(" %m", I(move), J(move));
  if (!result_certain && report_uncertainty)
    gtp_printf(" uncertain");

  return gtp_finish_response();
}  


/***********************
 * Connection reading. *
 ***********************/

/* Function:  Try to connect two strings.
 * Arguments: vertex, vertex
 * Fails:     invalid vertex, empty vertex, vertices of different colors
 * Returns:   connect result followed by connect point if successful.
 */
static int
gtp_connect(char *s)
{
  int ai, aj;
  int bi, bj;
  int connect_move = PASS_MOVE;
  int result;
  int n;
  
  n = gtp_decode_coord(s, &ai, &aj);
  if (n == 0)
    return gtp_failure("invalid coordinate");

  if (!gtp_decode_coord(s + n, &bi, &bj))
    return gtp_failure("invalid coordinate");

  if (BOARD(ai, aj) == EMPTY || BOARD(bi, bj) == EMPTY)
    return gtp_failure("vertex must not be empty");

  if (BOARD(ai, aj) != BOARD(bi, bj))
    return gtp_failure("vertices must have same color");

  result = string_connect(POS(ai, aj), POS(bi, bj), &connect_move);
  gtp_start_response(GTP_SUCCESS);
  gtp_print_code(result);
  if (result != 0)
    gtp_mprintf(" %m", I(connect_move), J(connect_move));

  return gtp_finish_response();
}  


/* Function:  Try to disconnect two strings.
 * Arguments: vertex, vertex
 * Fails:     invalid vertex, empty vertex, vertices of different colors
 * Returns:   disconnect result followed by disconnect point if successful.
 */
static int
gtp_disconnect(char *s)
{
  int ai, aj;
  int bi, bj;
  int disconnect_move = PASS_MOVE;
  int result;
  int n;
  
  n = gtp_decode_coord(s, &ai, &aj);
  if (n == 0)
    return gtp_failure("invalid coordinate");

  if (!gtp_decode_coord(s + n, &bi, &bj))
    return gtp_failure("invalid coordinate");

  if (BOARD(ai, aj) == EMPTY || BOARD(bi, bj) == EMPTY)
    return gtp_failure("vertex must not be empty");

  if (BOARD(ai, aj) != BOARD(bi, bj))
    return gtp_failure("vertices must have same color");

  result = disconnect(POS(ai, aj), POS(bi, bj), &disconnect_move);
  gtp_start_response(GTP_SUCCESS);
  gtp_print_code(result);
  if (result != 0)
    gtp_mprintf(" %m", I(disconnect_move), J(disconnect_move));

  return gtp_finish_response();
}  


/* Function:  Try to break from string into area.
 * Arguments: vertex, vertices
 * Fails:     invalid vertex, empty vertex.
 * Returns:   result followed by break in point if successful.
 */
static int
gtp_break_in(char *s)
{
  int ai, aj;
  int i, j;
  signed char goal[BOARDMAX];
  int break_move = PASS_MOVE;
  int result;
  int n;
  int k;
  
  n = gtp_decode_coord(s, &ai, &aj);
  if (n == 0)
    return gtp_failure("invalid coordinate");

  memset(goal, 0, BOARDMAX);
  s += n;

  for (k = 0; k < MAX_BOARD * MAX_BOARD; k++) {
    n = gtp_decode_coord(s, &i, &j);
    if (n > 0) {
      goal[POS(i, j)] = 1;
      s += n;
    }
    else if (sscanf(s, "%*s") != EOF)
      return gtp_failure("invalid coordinate");
    else
      break;
  }

  if (BOARD(ai, aj) == EMPTY)
    return gtp_failure("vertex must not be empty");

  result = break_in(POS(ai, aj), goal, &break_move);
  gtp_start_response(GTP_SUCCESS);
  gtp_print_code(result);
  if (result != 0)
    gtp_mprintf(" %m", I(break_move), J(break_move));

  return gtp_finish_response();
}

/* Function:  Try to block string from area.
 * Arguments: vertex, vertices
 * Fails:     invalid vertex, empty vertex.
 * Returns:   result followed by block point if successful.
 */
static int
gtp_block_off(char *s)
{
  int ai, aj;
  int i, j;
  signed char goal[BOARDMAX];
  int block_move = PASS_MOVE;
  int result;
  int n;
  int k;
  
  n = gtp_decode_coord(s, &ai, &aj);
  if (n == 0)
    return gtp_failure("invalid coordinate");

  memset(goal, 0, BOARDMAX);
  s += n;

  for (k = 0; k < MAX_BOARD * MAX_BOARD; k++) {
    n = gtp_decode_coord(s, &i, &j);
    if (n > 0) {
      goal[POS(i, j)] = 1;
      s += n;
    }
    else if (sscanf(s, "%*s") != EOF)
      return gtp_failure("invalid coordinate");
    else
      break;
  }

  if (BOARD(ai, aj) == EMPTY)
    return gtp_failure("vertex must not be empty");

  result = block_off(POS(ai, aj), goal, &block_move);
  gtp_start_response(GTP_SUCCESS);
  gtp_print_code(result);
  if (result != 0)
    gtp_mprintf(" %m", I(block_move), J(block_move));

  return gtp_finish_response();
}



/********
 * eyes *
 ********/

/* Function:  Evaluate an eye space
 * Arguments: vertex
 * Fails:     invalid vertex
 * Returns:   Minimum and maximum number of eyes. If these differ an
 *            attack and a defense point are additionally returned.
 *            If the vertex is not an eye space or not of unique color,
 *            a single -1 is returned.
 */

static int
gtp_eval_eye(char *s)
{
  int m, n;
  struct eyevalue value;
  int attack_point;
  int defense_point;
  int pos;

  if (!gtp_decode_coord(s, &m, &n))
    return gtp_failure("invalid coordinate");

  silent_examine_position(EXAMINE_DRAGONS_WITHOUT_OWL);
  
  if (black_eye[POS(m, n)].color == BLACK) {
    pos = black_eye[POS(m, n)].origin;
    compute_eyes(pos, &value, &attack_point, &defense_point,
		 black_eye, half_eye, 0);
  }
  else if (white_eye[POS(m, n)].color == WHITE) {
    pos = white_eye[POS(m, n)].origin;
    compute_eyes(pos, &value, &attack_point, &defense_point,
		 white_eye, half_eye, 0);
  }
  else
    /* Not an eye or not of unique color. */
    return gtp_success("-1");

  gtp_start_response(GTP_SUCCESS);
  gtp_printf("%d %d", min_eyes(&value), max_eyes(&value));
  if (eye_move_urgency(&value) > 0) {
    gtp_printf(" ");
    gtp_print_vertex(I(attack_point), J(attack_point));
    gtp_printf(" ");
    gtp_print_vertex(I(defense_point), J(defense_point));
  }
  return gtp_finish_response();
}


/*****************
 * dragon status *
 *****************/

/* Function:  Determine status of a dragon.
 * Arguments: optional vertex
 * Fails:     invalid vertex, empty vertex
 * Returns:   status ("alive", "critical", "dead", or "unknown"),
 *            attack point, defense point. Points of attack and
 *            defense are only given if the status is critical.
 *            If no vertex is given, the status is listed for all
 *            dragons, one per row in the format "A4: alive".
 *
 * FIXME: Should be able to distinguish between life in seki
 *        and independent life. Should also be able to identify ko.
 */

static int
gtp_dragon_status(char *s)
{
  int i, j;
  int str = NO_MOVE;
  int pos;
  int empty_response = 1;

  if (gtp_decode_coord(s, &i, &j)) {
    str = POS(i, j);
    if (board[str] == EMPTY)
      return gtp_failure("vertex must not be empty");
  }
  else if (sscanf(s, "%*s") != EOF)
    return gtp_failure("invalid coordinate");

  silent_examine_position(EXAMINE_DRAGONS);
  
  gtp_start_response(GTP_SUCCESS);

  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (ON_BOARD(pos)
	&& (pos == str
	    || (str == NO_MOVE
		&& board[pos] != EMPTY
		&& dragon[pos].origin == pos))) {
      if (str == NO_MOVE)
	gtp_mprintf("%m: ", I(pos), J(pos));
      
      if (dragon[pos].status == ALIVE)
	gtp_printf("alive\n");
      else if (dragon[pos].status == DEAD)
	gtp_printf("dead\n");
      else if (dragon[pos].status == UNKNOWN)
	gtp_printf("unknown\n");
      else {
	/* Only remaining possibility. */
	assert(dragon[pos].status == CRITICAL); 
	/* Status critical, need to return attack and defense point as well. */
	gtp_mprintf("critical %m %m\n", 
		    I(DRAGON2(pos).owl_attack_point),
		    J(DRAGON2(pos).owl_attack_point),
		    I(DRAGON2(pos).owl_defense_point),
		    J(DRAGON2(pos).owl_defense_point));
      }
      empty_response = 0;
    }
  }

  if (empty_response)
    gtp_printf("\n");

  gtp_printf("\n");
  return GTP_OK;
}


/* Function:  Determine whether two stones belong to the same dragon.
 * Arguments: vertex, vertex
 * Fails:     invalid vertex, empty vertex
 * Returns:   1 if the vertices belong to the same dragon, 0 otherwise
 */

static int
gtp_same_dragon(char *s)
{
  int ai, aj;
  int bi, bj;
  int n;

  n = gtp_decode_coord(s, &ai, &aj);
  if (n == 0)
    return gtp_failure("invalid coordinate");

  if (!gtp_decode_coord(s + n, &bi, &bj))
    return gtp_failure("invalid coordinate");

  if (BOARD(ai, aj) == EMPTY || BOARD(bi, bj) == EMPTY)
    return gtp_failure("vertex must not be empty");

  silent_examine_position(EXAMINE_DRAGONS_WITHOUT_OWL);
  
  return gtp_success("%d", dragon[POS(ai, aj)].id == dragon[POS(bi, bj)].id);
}


/************************
 * Unconditional status *
 ************************/

/* Function:  Determine the unconditional status of a vertex.
 * Arguments: vertex
 * Fails:     invalid vertex
 * Returns:   unconditional status ("undecided", "alive", "dead",
 *            "white_territory", "black_territory"). Occupied vertices can
 *            be undecided, alive, or dead. Empty vertices can be
 *            undecided, white territory, or black territory.
 */

static int
gtp_unconditional_status(char *s)
{
  int i, j;
  enum dragon_status status;

  if (!gtp_decode_coord(s, &i, &j))
    return gtp_failure("invalid coordinate");

  silent_examine_position(EXAMINE_WORMS);
  
  status = worm[POS(i, j)].unconditional_status;
  if (status == UNKNOWN)
    return gtp_success("undecided");
  return gtp_success("%s", status_to_string(status));
}


/***********************
 * combination attacks *
 ***********************/

/* Function:  Find a move by color capturing something through a
 *            combination attack.
 * Arguments: color
 * Fails:     invalid color
 * Returns:   Recommended move, PASS if no move found
 */

static int
gtp_combination_attack(char *s)
{
  int color;
  int attack_point;
  int n;

  n = gtp_decode_color(s, &color);
  if (!n)
    return gtp_failure("invalid color");

  silent_examine_position(EXAMINE_ALL);

  if (!atari_atari(color, &attack_point, NULL, verbose))
    attack_point = NO_MOVE;
  
  gtp_start_response(GTP_SUCCESS);
  gtp_print_vertex(I(attack_point), J(attack_point));
  return gtp_finish_response();
}

/* Function:  If color can capture something through a
 *            combination attack, list moves by the opponent of color
 *            to defend against this attack.
 * Arguments: color
 * Fails:     invalid color
 * Returns:   Recommended moves, PASS if no combination attack found.
 */

static int
gtp_combination_defend(char *s)
{
  int color;
  signed char defense_points[BOARDMAX];
  int pos;
  int first = 1;
  int n;

  n = gtp_decode_color(s, &color);
  if (!n)
    return gtp_failure("invalid color");

  silent_examine_position(EXAMINE_ALL);

  memset(defense_points, 0, sizeof(defense_points));
  if (!atari_atari(color, NULL, defense_points, verbose))
    return gtp_success("PASS");
  
  gtp_start_response(GTP_SUCCESS);
  for (pos = BOARDMIN; pos < BOARDMAX; pos++)
    if (ON_BOARD(pos) && defense_points[pos]) {
      if (!first)
	gtp_printf(" ");
      else
	first = 0;
      gtp_print_vertex(I(pos), J(pos));
    }
  
  return gtp_finish_response();
}

/* Function:  Run atari_atari_confirm_safety().
 * Arguments: move, optional int
 * Fails:     invalid move
 * Returns:   success code, if failure also defending move
 */

static int
gtp_aa_confirm_safety(char *s)
{
  int color;
  int i, j;
  int n;
  int minsize = 0;
  int result;
  int defense_point = NO_MOVE;
  signed char saved_dragons[BOARDMAX];
  signed char saved_worms[BOARDMAX];

  n = gtp_decode_move(s, &color, &i, &j);
  if (n == 0 || POS(i, j) == NO_MOVE)
    return gtp_failure("invalid color or coordinate");

  sscanf(s + n, "%d", &minsize);

  genmove(color, NULL, NULL);
  get_saved_dragons(POS(i, j), saved_dragons);
  get_saved_worms(POS(i, j), saved_worms);
  
  result = atari_atari_confirm_safety(color, POS(i, j),
				      &defense_point, minsize,
				      saved_dragons, saved_worms);
  
  gtp_start_response(GTP_SUCCESS);
  gtp_mprintf("%d", result);
  if (result == 0)
    gtp_mprintf(" %m", I(defense_point), J(defense_point));
  
  return gtp_finish_response();
}


/********************
 * generating moves *
 ********************/

/* Function:  Generate and play the supposedly best black move.
 * Arguments: none
 * Fails:     never
 * Returns:   a move coordinate or "PASS"
 *
 * Status:    Obsolete GTP version 1 command.
 */
static int
gtp_genmove_black(char *s)
{
  int move;
  UNUSED(s);

  if (stackp > 0)
    return gtp_failure("genmove cannot be called when stackp > 0");

  move = genmove(BLACK, NULL, NULL);

  gnugo_play_move(move, BLACK);

  gtp_start_response(GTP_SUCCESS);
  gtp_print_vertex(I(move), J(move));
  return gtp_finish_response();
}

/* Function:  Generate and play the supposedly best white move.
 * Arguments: none
 * Fails:     never
 * Returns:   a move coordinate or "PASS"
 *
 * Status:    Obsolete GTP version 1 command.
 */
static int
gtp_genmove_white(char *s)
{
  int move;
  UNUSED(s);

  if (stackp > 0)
    return gtp_failure("genmove cannot be called when stackp > 0");

  move = genmove(WHITE, NULL, NULL);

  gnugo_play_move(move, WHITE);

  gtp_start_response(GTP_SUCCESS);
  gtp_print_vertex(I(move), J(move));
  return gtp_finish_response();
}

/* Function:  Generate and play the supposedly best move for either color.
 * Arguments: color to move
 * Fails:     invalid color
 * Returns:   a move coordinate or "PASS" (or "resign" if resignation_allowed)
 *
 * Status:    GTP version 2 standard command.
 */
static int
gtp_genmove(char *s)
{
  int move;
  int resign;
  int color;
  int n;

  n = gtp_decode_color(s, &color);
  if (!n)
    return gtp_failure("invalid color");

  if (stackp > 0)
    return gtp_failure("genmove cannot be called when stackp > 0");

  adjust_level_offset(color);
  move = genmove(color, NULL, &resign);

  if (resign)
    return gtp_success("resign");

  gnugo_play_move(move, color);

  gtp_start_response(GTP_SUCCESS);
  gtp_print_vertex(I(move), J(move));
  return gtp_finish_response();
}


/* Function:  Generate the supposedly best move for either color.
 * Arguments: color to move
 * Fails:     invalid color
 * Returns:   a move coordinate (or "PASS")
 *
 * Status:    GTP version 2 standard command.
 */
static int
gtp_reg_genmove(char *s)
{
  int move;
  int color;
  int n;
  unsigned int saved_random_seed = get_random_seed();

  n = gtp_decode_color(s, &color);
  if (!n)
    return gtp_failure("invalid color");

  if (stackp > 0)
    return gtp_failure("genmove cannot be called when stackp > 0");

  /* This is intended for regression purposes and should therefore be
   * deterministic. The best way to ensure this is to reset the random
   * number generator before calling genmove(). It is always seeded by
   * 0.
   */
  set_random_seed(0);
  
  move = genmove_conservative(color, NULL);

  set_random_seed(saved_random_seed);
  gtp_start_response(GTP_SUCCESS);
  gtp_print_vertex(I(move), J(move));
  return gtp_finish_response();
}

/* Function:  Generate the supposedly best move for either color.
 * Arguments: color to move, optionally a random seed
 * Fails:     invalid color
 * Returns:   a move coordinate (or "PASS")
 *
 * This differs from reg_genmove in the optional random seed.
 */
static int
gtp_gg_genmove(char *s)
{
  int move;
  int color;
  int n;
  unsigned int saved_random_seed = get_random_seed();
  unsigned int seed;

  n = gtp_decode_color(s, &color);
  if (!n)
    return gtp_failure("invalid color");

  if (stackp > 0)
    return gtp_failure("genmove cannot be called when stackp > 0");

  /* This is intended for regression purposes and should therefore be
   * deterministic. The best way to ensure this is to reset the random
   * number generator before calling genmove(). By default it is
   * seeded with 0, but if an optional unsigned integer is given in
   * the command after the color, this is used as seed instead.
   */
  seed = 0;
  sscanf(s+n, "%u", &seed);
  set_random_seed(seed);
  
  move = genmove_conservative(color, NULL);
  set_random_seed(saved_random_seed);
  gtp_start_response(GTP_SUCCESS);
  gtp_print_vertex(I(move), J(move));
  return gtp_finish_response();
}


/* Function:  Generate the supposedly best move for either color from a
 *            choice of allowed vertices.
 * Arguments: color to move, allowed vertices
 * Fails:     invalid color, invalid vertex, no vertex listed
 * Returns:   a move coordinate (or "PASS")
 */
static int
gtp_restricted_genmove(char *s)
{
  int move;
  int i, j;
  int color;
  int n;
  unsigned int saved_random_seed = get_random_seed();
  int allowed_moves[BOARDMAX];
  int number_allowed_moves = 0;
  memset(allowed_moves, 0, sizeof(allowed_moves));

  n = gtp_decode_color(s, &color);
  if (!n)
    return gtp_failure("invalid color");

  s += n;
  while (1) {
    n = gtp_decode_coord(s, &i, &j);
    if (n > 0) {
      allowed_moves[POS(i, j)] = 1;
      number_allowed_moves++;
      s += n;
    }
    else if (sscanf(s, "%*s") != EOF)
      return gtp_failure("invalid coordinate");
    else
      break;
  }

  if (number_allowed_moves == 0)
    return gtp_failure("no allowed vertex");

  if (stackp > 0)
    return gtp_failure("genmove cannot be called when stackp > 0");

  /* This is intended for regression purposes and should therefore be
   * deterministic. The best way to ensure this is to reset the random
   * number generator before calling genmove(). It is always seeded by
   * 0.
   */
  set_random_seed(0);
  
  move = genmove_restricted(color, allowed_moves);
  set_random_seed(saved_random_seed);
  gtp_start_response(GTP_SUCCESS);
  gtp_print_vertex(I(move), J(move));
  return gtp_finish_response();
}


/* Function:  Generate and play the supposedly best move for either color,
 *            not passing until all dead opponent stones have been removed.
 * Arguments: color to move
 * Fails:     invalid color
 * Returns:   a move coordinate (or "PASS")
 *
 * Status:    KGS specific command.
 *
 * A similar command, but possibly somewhat different, will likely be added
 * to GTP version 3 at a later time.
 */
static int
gtp_kgs_genmove_cleanup(char *s)
{
  int move;
  int color;
  int n;
  int save_capture_all_dead = capture_all_dead;

  n = gtp_decode_color(s, &color);
  if (!n)
    return gtp_failure("invalid color");

  if (stackp > 0)
    return gtp_failure("kgs-genmove_cleanup cannot be called when stackp > 0");

  /* Turn on the capture_all_dead option to force removal of dead
   * opponent stones.
   */
  capture_all_dead = 1;
  
  adjust_level_offset(color);
  move = genmove(color, NULL, NULL);

  capture_all_dead = save_capture_all_dead;
  
  gnugo_play_move(move, color);

  gtp_start_response(GTP_SUCCESS);
  gtp_print_vertex(I(move), J(move));
  return gtp_finish_response();
}


/* Function : List the move reasons for a move.
 * Arguments: vertex
 * Fails:   : invalid vertex, occupied vertex
 * Returns  : list of move reasons (may be empty)
 */

static int
gtp_move_reasons(char *s)
{
  int i, j;
  if (!gtp_decode_coord(s, &i, &j))
    return gtp_failure("invalid coordinate");

  if (BOARD(i, j) != EMPTY)
    return gtp_failure("vertex must not be occupied");

  gtp_start_response(GTP_SUCCESS);
  if (list_move_reasons(gtp_output_file, POS(i, j)) == 0)
    gtp_printf("\n");
  gtp_printf("\n");
  return GTP_OK;
}

/* Function : Generate a list of all moves with values larger than zero in
 *            the previous genmove command.
 *            If no previous genmove command has been issued, the result
 *            of this command will be meaningless.
 * Arguments: none
 * Fails:   : never
 * Returns  : list of moves with values
 */

static int
gtp_all_move_values(char *s)
{
  UNUSED(s);
  gtp_start_response(GTP_SUCCESS);
  print_all_move_values(gtp_output_file);
  gtp_printf("\n");
  return GTP_OK;
}

/* Function : Generate a sorted list of the best moves in the previous genmove
 *            command.
 *            If no previous genmove command has been issued, the result
 *            of this command will be meaningless.
 * Arguments: none
 * Fails:   : never
 * Returns  : list of moves with weights
 */

/* FIXME: Don't we want the moves one per row? */
static int
gtp_top_moves(char *s)
{
  int k;
  UNUSED(s);
  gtp_start_response(GTP_SUCCESS);
  for (k = 0; k < 10; k++)
    if (best_move_values[k] > 0.0) {
      gtp_print_vertex(I(best_moves[k]), J(best_moves[k]));
      gtp_printf(" %.2f ", best_move_values[k]);
    }
  gtp_printf("\n\n");
  return GTP_OK;
}

/* Function : Generate a list of the best moves for white with weights
 * Arguments: none
 * Fails:   : never
 * Returns  : list of moves with weights
 */

static int
gtp_top_moves_white(char *s)
{
  int k;
  UNUSED(s);
  genmove(WHITE, NULL, NULL);
  gtp_start_response(GTP_SUCCESS);
  for (k = 0; k < 10; k++)
    if (best_move_values[k] > 0.0) {
      gtp_print_vertex(I(best_moves[k]), J(best_moves[k]));
      gtp_printf(" %.2f ", best_move_values[k]);
    }
  return gtp_finish_response();
}

/* Function : Generate a list of the best moves for black with weights
 * Arguments: none
 * Fails:   : never
 * Returns  : list of moves with weights
 */

static int
gtp_top_moves_black(char *s)
{
  int k;
  UNUSED(s);
  genmove(BLACK, NULL, NULL);
  gtp_start_response(GTP_SUCCESS);
  for (k = 0; k < 10; k++)
    if (best_move_values[k] > 0.0) {
      gtp_print_vertex(I(best_moves[k]), J(best_moves[k]));
      gtp_printf(" %.2f ", best_move_values[k]);
    }
  return gtp_finish_response();
}



/* Function:  Set the playing level.
 * Arguments: int
 * Fails:     incorrect argument
 * Returns:   nothing
 */
static int
gtp_set_level(char *s)
{
  int new_level;
  if (sscanf(s, "%d", &new_level) < 1)
    return gtp_failure("level not an integer");
  
  set_level(new_level);
  return gtp_success("");
}

/* Function:  Undo one move
 * Arguments: none
 * Fails:     If move history is too short.
 * Returns:   nothing
 *
 * Status:    GTP version 2 standard command.
 */

static int
gtp_undo(char *s)
{
  UNUSED(s);

  if (stackp > 0 || !undo_move(1))
    return gtp_failure("cannot undo");

  reset_engine();
  
  return gtp_success("");
}


/* Function:  Undo a number of moves
 * Arguments: optional int
 * Fails:     If move history is too short.
 * Returns:   nothing
 */

static int
gtp_gg_undo(char *s)
{
  int number_moves = 1;

  sscanf(s, "%d", &number_moves);

  if (number_moves < 0)
    return gtp_failure("can't undo a negative number of moves");

  if (stackp > 0 || !undo_move(number_moves))
    return gtp_failure("cannot undo");

  reset_engine();
  
  return gtp_success("");
}


/*****************
 * time handling *
 *****************/

/* Function:  Set time allowance
 * Arguments: int main_time, int byo_yomi_time, int byo_yomi_stones
 * Fails:     syntax error
 * Returns:   nothing
 *
 * Status:    GTP version 2 standard command.
 */

static int
gtp_time_settings(char *s)
{
  int main_time, byoyomi_time, byoyomi_stones;
  
  if (sscanf(s, "%d %d %d", &main_time, &byoyomi_time, &byoyomi_stones) < 3)
    return gtp_failure("not three integers");

  clock_settings(main_time, byoyomi_time, byoyomi_stones);
  return gtp_success("");
}


/* Function:  Report remaining time
 * Arguments: color color, int time, int stones
 * Fails:     syntax error
 * Returns:   nothing
 *
 * Status:    GTP version 2 standard command.
 */

static int
gtp_time_left(char *s)
{
  int color;
  int time;
  int stones;
  int n;

  n = gtp_decode_color(s, &color);
  if (!n)
    return gtp_failure("invalid color");
  
  if (sscanf(s+n, "%d %d", &time, &stones) < 2)
    return gtp_failure("time and stones not two integers");

  update_time_left(color, time, stones);
  
  return gtp_success("");
}


/***********
 * scoring *
 ***********/

static float final_score;
static enum dragon_status final_status[MAX_BOARD][MAX_BOARD];
static enum dragon_status status_numbers[6] = {ALIVE, DEAD, ALIVE_IN_SEKI,
					       WHITE_TERRITORY,
					       BLACK_TERRITORY, DAME};
static const char *status_names[6] = {"alive", "dead", "seki",
				      "white_territory", "black_territory",
				      "dame"};

/* Helper function. */
static void
finish_and_score_game(int seed)
{
  int move;
  int i, j;
  int next;
  int pass = 0;
  int moves = 0;
  int saved_board[MAX_BOARD][MAX_BOARD];
  struct board_state saved_pos;
  static int current_board[MAX_BOARD][MAX_BOARD];
  static int current_seed = -1;
  int cached_board = 1;

  if (current_seed != seed) {
    current_seed = seed;
    cached_board = 0;
  }

  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++)
      if (BOARD(i, j) != current_board[i][j]) {
	current_board[i][j] = BOARD(i, j);
	cached_board = 0;
      }

  /* If this is exactly the same position as the one we analyzed the
   * last time, the contents of final_score and final_status are up to date.
   */
  if (cached_board)
    return;

  doing_scoring = 1;
  store_board(&saved_pos);

  /* Let black start if we have no move history. Otherwise continue
   * alternation.
   */
  if (get_last_player() == EMPTY)
    next = BLACK;
  else
    next = OTHER_COLOR(get_last_player());

  do {
    move = genmove_conservative(next, NULL);
    gnugo_play_move(move, next);
    if (move != PASS_MOVE) {
      pass = 0;
      moves++;
    }
    else
      pass++;

    next = OTHER_COLOR(next);
  } while (pass < 2 && moves < board_size * board_size);

  final_score = aftermath_compute_score(next, NULL);
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++) {
      final_status[i][j] = aftermath_final_status(next, POS(i, j));
      saved_board[i][j] = BOARD(i, j);
    }

  restore_board(&saved_pos);
  doing_scoring = 0;

  /* Update the status for vertices which were changed while finishing
   * the game, up to filling dame.
   */
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++) {
      if (BOARD(i, j) == saved_board[i][j])
	continue;

      if (BOARD(i, j) == EMPTY) {
	if (final_status[i][j] == ALIVE
	    || final_status[i][j] == ALIVE_IN_SEKI)
	  final_status[i][j] = DAME;
	else if (final_status[i][j] == DEAD) {
	  if (saved_board[i][j] == BLACK)
	    final_status[i][j] = WHITE_TERRITORY;
	  else
	    final_status[i][j] = BLACK_TERRITORY;
	}
      }
      else if (BOARD(i, j) == BLACK) {
	if (final_status[i][j] == WHITE_TERRITORY)
	  final_status[i][j] = DEAD;
	else if (final_status[i][j] == DAME)
	  final_status[i][j] = ALIVE_IN_SEKI;
	else if (final_status[i][j] == BLACK_TERRITORY)
	  final_status[i][j] = ALIVE;
	else
	  final_status[i][j] = DEAD;
      }
      else if (BOARD(i, j) == WHITE) {
	if (final_status[i][j] == BLACK_TERRITORY)
	  final_status[i][j] = DEAD;
	else if (final_status[i][j] == DAME)
	  final_status[i][j] = ALIVE_IN_SEKI;
	else if (final_status[i][j] == WHITE_TERRITORY)
	  final_status[i][j] = ALIVE;
	else
	  final_status[i][j] = DEAD;
      }
    }
}


/* Function:  Compute the score of a finished game.
 * Arguments: Optional random seed
 * Fails:     never
 * Returns:   Score in SGF format (RE property).
 *
 * Status:    GTP version 2 standard command.
 */
static int
gtp_final_score(char *s)
{
  unsigned int saved_random_seed = get_random_seed();
  int seed;
  /* This is intended for regression purposes and should therefore be
   * deterministic. The best way to ensure this is to reset the random
   * number generator before calling genmove(). By default it is
   * seeded with 0, but if an optional unsigned integer is given in
   * the command after the color, this is used as seed instead.
   */
  seed = 0;
  sscanf(s, "%d", &seed);
  set_random_seed(seed);

  finish_and_score_game(seed);

  set_random_seed(saved_random_seed);

  gtp_start_response(GTP_SUCCESS);
  if (final_score > 0.0)
    gtp_printf("W+%3.1f", final_score);
  else if (final_score < 0.0)
    gtp_printf("B+%3.1f", -final_score);
  else
    gtp_printf("0");
  return gtp_finish_response();
}


/* Function:  Report the final status of a vertex in a finished game.
 * Arguments: Vertex, optional random seed
 * Fails:     invalid vertex
 * Returns:   Status in the form of one of the strings "alive", "dead",
 *            "seki", "white_territory", "black_territory", or "dame".
 */
static int
gtp_final_status(char *s)
{
  int seed;
  int n;
  int ai, aj;
  int k;
  unsigned int saved_random_seed = get_random_seed();
  const char *result = NULL;

  n = gtp_decode_coord(s, &ai, &aj);
  if (n == 0)
    return gtp_failure("invalid coordinate");

  /* This is intended for regression purposes and should therefore be
   * deterministic. The best way to ensure this is to reset the random
   * number generator before calling genmove(). By default it is
   * seeded with 0, but if an optional unsigned integer is given in
   * the command after the color, this is used as seed instead.
   */
  seed = 0;
  sscanf(s + n, "%d", &seed);
  set_random_seed(seed);

  finish_and_score_game(seed);

  set_random_seed(saved_random_seed);
  for (k = 0; k < 6; k++)
    if (final_status[ai][aj] == status_numbers[k]) {
      result = status_names[k];
      break;
    }
  assert(result != NULL);

  return gtp_success(result);
}


/* Function:  Report vertices with a specific final status in a finished game.
 * Arguments: Status in the form of one of the strings "alive", "dead",
 *            "seki", "white_territory", "black_territory", or "dame".
 *            An optional random seed can be added.
 * Fails:     missing or invalid status string
 * Returns:   Vertices having the specified status. These are split with
 *            one string on each line if the vertices are nonempty (i.e.
 *            for "alive", "dead", and "seki").
 *
 * Status:    GTP version 2 standard command.
 *            However, "dame", "white_territory", and "black_territory"
 *            are private extensions.
 */
static int
gtp_final_status_list(char *s)
{
  int seed;
  int n;
  int i, j;
  enum dragon_status status = UNKNOWN;
  int k;
  char status_string[GTP_BUFSIZE];
  int first;
  unsigned int saved_random_seed = get_random_seed();

  if (sscanf(s, "%s %n", status_string, &n) != 1)
    return gtp_failure("missing status");
  
  for (k = 0; k < 6; k++) {
    if (strcmp(status_string, status_names[k]) == 0)
      status = status_numbers[k];
  }

  if (status == UNKNOWN)
    return gtp_failure("invalid status");

  /* This is intended for regression purposes and should therefore be
   * deterministic. The best way to ensure this is to reset the random
   * number generator before calling genmove(). By default it is
   * seeded with 0, but if an optional unsigned integer is given in
   * the command after the color, this is used as seed instead.
   */
  seed = 0;
  sscanf(s + n, "%d", &seed);
  set_random_seed(seed);

  finish_and_score_game(seed);

  set_random_seed(saved_random_seed);

  gtp_start_response(GTP_SUCCESS);

  first = 1;
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++) {
      if (final_status[i][j] != status)
	continue;
      if (BOARD(i, j) == EMPTY) {
	if (!first)
	  gtp_printf(" ");
	else
	  first = 0;
	gtp_print_vertex(i, j);
      }
      else {
	int num_stones;
	int stones[MAX_BOARD * MAX_BOARD];
	if (find_origin(POS(i, j)) != POS(i, j))
	  continue;
	if (!first)
	  gtp_printf("\n");
	else
	  first = 0;
	num_stones = findstones(POS(i, j), board_size * board_size, stones);
	gtp_print_vertices2(num_stones, stones);
      }
    }

  return gtp_finish_response();
}

/* Function:  Estimate the score
 * Arguments: None
 * Fails:     never
 * Returns:   upper and lower bounds for the score
 */

static int
gtp_estimate_score(char *s)
{
  float score;
  float upper_bound, lower_bound;
  UNUSED(s);

  score = gnugo_estimate_score(&upper_bound, &lower_bound);
  gtp_start_response(GTP_SUCCESS);
  /* Traditionally W wins jigo */
  if (score >= 0.0) 
    gtp_printf("W+%3.1f (upper bound: %3.1f, lower: %3.1f)", 
	       score, upper_bound, lower_bound);
  else if (score < 0.0)
    gtp_printf("B+%3.1f (upper bound: %3.1f, lower: %3.1f)", 
	       -score, upper_bound, lower_bound);
  return gtp_finish_response();
}  

/* Function:  Estimate the score, taking into account which player moves next
 * Arguments: Color to play
 * Fails:     Invalid color
 * Returns:   Score.
 *
 * This function generates a move for color, then adds the
 * value of the move generated to the value of the position.
 * Critical dragons are awarded to the opponent since the
 * value of rescuing a critical dragon is taken into account
 * in the value of the move generated.
 */

static int
gtp_experimental_score(char *s)
{
  float upper_bound, lower_bound, score;
  int color;

  if (!gtp_decode_color(s, &color)
      || (color != BLACK && color != WHITE))
    return gtp_failure("invalid color");

  genmove_conservative(color, NULL);
  gnugo_estimate_score(&upper_bound, &lower_bound);

  if (debug & DEBUG_SCORING)
    fprintf(stderr, "upper = %3.1f, lower = %3.1f, best = %3.1f\n",
	    upper_bound, lower_bound, best_move_values[0]);
  if (color == WHITE)
    score = lower_bound + best_move_values[0];
  else
    score = upper_bound - best_move_values[0];

  return gtp_success("%3.1f", score);
}  


/**************
 * statistics *
 **************/

/* Function:  Reset the count of life nodes.
 * Arguments: none
 * Fails:     never
 * Returns:   nothing
 *
 * Note: This function is obsolete and only remains for backwards
 * compatibility.
 */
static int
gtp_reset_life_node_counter(char *s)
{
  UNUSED(s);
  return gtp_success("");
}


/* Function:  Retrieve the count of life nodes.
 * Arguments: none
 * Fails:     never
 * Returns:   number of life nodes
 *
 * Note: This function is obsolete and only remains for backwards
 * compatibility.
 */
static int
gtp_get_life_node_counter(char *s)
{
  UNUSED(s);
  return gtp_success("0");
}


/* Function:  Reset the count of owl nodes.
 * Arguments: none
 * Fails:     never
 * Returns:   nothing
 */
static int
gtp_reset_owl_node_counter(char *s)
{
  UNUSED(s);
  reset_owl_node_counter();
  return gtp_success("");
}


/* Function:  Retrieve the count of owl nodes.
 * Arguments: none
 * Fails:     never
 * Returns:   number of owl nodes
 */
static int
gtp_get_owl_node_counter(char *s)
{
  int nodes = get_owl_node_counter();
  UNUSED(s);
  return gtp_success("%d", nodes);
}


/* Function:  Reset the count of reading nodes.
 * Arguments: none
 * Fails:     never
 * Returns:   nothing
 */
static int
gtp_reset_reading_node_counter(char *s)
{
  UNUSED(s);
  reset_reading_node_counter();
  return gtp_success("");
}


/* Function:  Retrieve the count of reading nodes.
 * Arguments: none
 * Fails:     never
 * Returns:   number of reading nodes
 */
static int
gtp_get_reading_node_counter(char *s)
{
  int nodes = get_reading_node_counter();
  UNUSED(s);
  return gtp_success("%d", nodes);
}


/* Function:  Reset the count of trymoves/trykos.
 * Arguments: none
 * Fails:     never
 * Returns:   nothing
 */
static int
gtp_reset_trymove_counter(char *s)
{
  UNUSED(s);
  reset_trymove_counter();
  return gtp_success("");
}


/* Function:  Retrieve the count of trymoves/trykos.
 * Arguments: none
 * Fails:     never
 * Returns:   number of trymoves/trykos
 */
static int
gtp_get_trymove_counter(char *s)
{
  int nodes = get_trymove_counter();
  UNUSED(s);
  return gtp_success("%d", nodes);
}


/* Function:  Reset the count of connection nodes.
 * Arguments: none
 * Fails:     never
 * Returns:   nothing
 */
static int
gtp_reset_connection_node_counter(char *s)
{
  UNUSED(s);
  reset_connection_node_counter();
  return gtp_success("");
}


/* Function:  Retrieve the count of connection nodes.
 * Arguments: none
 * Fails:     never
 * Returns:   number of connection nodes
 */
static int
gtp_get_connection_node_counter(char *s)
{
  int nodes = get_connection_node_counter();
  UNUSED(s);
  return gtp_success("%d", nodes);
}



/*********
 * debug *
 *********/


/* Function:  Test an eyeshape for inconsistent evaluations
 * Arguments: Eyeshape vertices
 * Fails:     Bad vertices
 * Returns:   Failure reports on stderr.
 */
static int
gtp_test_eyeshape(char *s)
{
  int n;
  int i, j;
  int eye_vertices[MAX_BOARD * MAX_BOARD];
  int eyesize = 0;

  n = gtp_decode_coord(s, &i, &j);
  while (n > 0) {
    eye_vertices[eyesize] = POS(i, j);
    eyesize++;
    s += n;
    n = gtp_decode_coord(s, &i, &j);
  }
  
  if (eyesize == 0)
    return gtp_failure("invalid coordinate");

  test_eyeshape(eyesize, eye_vertices);

  return gtp_success("");
}


/* Function:  Compute an eyevalue and vital points for an eye graph
 * Arguments: Eyeshape encoded in string
 * Fails:     Bad eyeshape, analysis failed
 * Returns:   Eyevalue, vital points
 */
static int
gtp_analyze_eyegraph(char *s)
{
  struct eyevalue value;
  char analyzed_eyegraph[1024];
  int result = analyze_eyegraph(s, &value, analyzed_eyegraph);

  if (result == 0)
    return gtp_failure("failed to analyze");

  return gtp_success("%s\n%s", eyevalue_to_string(&value), analyzed_eyegraph);
}



/* Function:  Returns elapsed CPU time in seconds.
 * Arguments: none
 * Fails:     never
 * Returns:   Total elapsed (user + system) CPU time in seconds.
 */
static int
gtp_cputime(char *s)
{
  UNUSED(s);
  return gtp_success("%.3f", gg_cputime());
}



/* Function:  Write the position to stdout.
 * Arguments: none
 * Fails:     never
 * Returns:   nothing
 *
 * Status:    GTP version 2 standard command.
 */
static int
gtp_showboard(char *s)
{
  UNUSED(s);
  
  gtp_start_response(GTP_SUCCESS);
  gtp_printf("\n");
  simple_showboard(gtp_output_file);
  return gtp_finish_response();
}


/* Function:  Dump stack to stderr.
 * Arguments: none
 * Fails:     never
 * Returns:   nothing
 */
static int
gtp_dump_stack(char *s)
{
  UNUSED(s);
  dump_stack();
  return gtp_success("");
}

/* Determine whether a string starts with a specific substring. */
static int
has_prefix(const char *s, const char *prefix)
{
  return strncmp(s, prefix, strlen(prefix)) == 0;
}

static int
print_influence_data(struct influence_data *q, char *what_data)
{
  float white_influence[BOARDMAX];
  float black_influence[BOARDMAX];
  float white_strength[BOARDMAX];
  float black_strength[BOARDMAX];
  float white_attenuation[BOARDMAX]; 
  float black_attenuation[BOARDMAX];
  float white_permeability[BOARDMAX];
  float black_permeability[BOARDMAX];
  float territory_value[BOARDMAX];
  int influence_regions[BOARDMAX];
  int non_territory[BOARDMAX];
  int m, n;
  
  float *float_pointer = NULL;
  int *int_pointer = NULL;
  
  while (*what_data == ' ')
    what_data++;

  get_influence(q, white_influence, black_influence,
		white_strength, black_strength,
		white_attenuation, black_attenuation,
		white_permeability, black_permeability,
		territory_value, influence_regions, non_territory);

  if (has_prefix(what_data, "white_influence"))
    float_pointer = white_influence;
  else if (has_prefix(what_data, "black_influence"))
    float_pointer = black_influence;
  else if (has_prefix(what_data, "white_strength"))
    float_pointer = white_strength;
  else if (has_prefix(what_data, "black_strength"))
    float_pointer = black_strength;
  else if (has_prefix(what_data, "white_attenuation"))
    float_pointer = white_attenuation;
  else if (has_prefix(what_data, "black_attenuation"))
    float_pointer = black_attenuation;
  else if (has_prefix(what_data, "white_permeability"))
    float_pointer = white_permeability;
  else if (has_prefix(what_data, "black_permeability"))
    float_pointer = black_permeability;
  else if (has_prefix(what_data, "territory_value"))
    float_pointer = territory_value;
  else if (has_prefix(what_data, "influence_regions"))
    int_pointer = influence_regions;
  else if (has_prefix(what_data, "non_territory"))
    int_pointer = non_territory;
  else
    return gtp_failure("unknown influence data");
  
  gtp_start_response(GTP_SUCCESS);
  for (m = 0; m < board_size; m++) {
    for (n = 0; n < board_size; n++) {
      if (float_pointer)
	gtp_printf("%6.2f ", float_pointer[POS(m, n)]);
      else
	gtp_printf("%2d ", int_pointer[POS(m, n)]);
    }
    gtp_printf("\n");
  }
  
  /* We already have one newline and thus can't use gtp_finish_response(). */
  gtp_printf("\n");
  return GTP_OK;
}

/* Function:  Return information about the initial influence function.
 * Arguments: color to move, what information
 * Fails:     never
 * Returns:   Influence data formatted like:
 *
 *   0.51   1.34   3.20   6.60   9.09   8.06   1.96   0.00   0.00 
 *   0.45   1.65   4.92  12.19  17.47  15.92   4.03   0.00   0.00 
 *                   .
 *                   .
 *                   .
 *   0.00   0.00   0.00   0.00   0.00 100.00  75.53  41.47  23.41
 *
 * The available choices of information are:
 * 
 * white_influence (float)
 * black_influence (float)
 * white_strength (float)
 * black_strength (float)
 * white_attenuation (float)
 * black_attenuation (float)
 * white_permeability (float)
 * black_permeability (float)
 * territory_value (float)
 * influence_regions (int)
 * non_territory (int)
 *
 * The encoding of influence_regions is as follows:
 *  4 white stone
 *  3 white territory
 *  2 white moyo
 *  1 white area
 *  0 neutral
 * -1 black area
 * -2 black moyo
 * -3 black territory
 * -4 black stone
 */
static int
gtp_initial_influence(char *s)
{
  int color;
  struct influence_data *q;
  int n;

  n = gtp_decode_color(s, &color);
  if (n == 0)
    return gtp_failure("invalid color");

  q = INITIAL_INFLUENCE(color);
  
  silent_examine_position(EXAMINE_ALL);

  return print_influence_data(q, s + n);
}


/* Function:  Return information about the influence function after a move.
 * Arguments: move, what information
 * Fails:     never
 * Returns:   Influence data formatted like for initial_influence.
 */
static int
gtp_move_influence(char *s)
{
  int color;
  int i, j;
  int n;

  n = gtp_decode_move(s, &color, &i, &j);
  if (n == 0)
    return gtp_failure("invalid move");

  prepare_move_influence_debugging(POS(i, j), color);
  
  return print_influence_data(&move_influence, s + n);
}


/* Function:  List probabilities of each move being played (when non-zero).
 *            If no previous genmove command has been issued, the result
 *            of this command will be meaningless.
 * Arguments: none
 * Fails:     never
 * Returns:   Move, probabilty pairs, one per row.
 */
static int
gtp_move_probabilities(char *s)
{
  float probabilities[BOARDMAX];
  int pos;
  int any_moves_printed = 0;

  UNUSED(s);

  compute_move_probabilities(probabilities);

  gtp_start_response(GTP_SUCCESS);
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (ON_BOARD(pos) && probabilities[pos] != 0.0) {
      gtp_mprintf("%m ", I(pos), J(pos));
      gtp_printf("%.4f\n", probabilities[pos]);
      any_moves_printed = 1;
    }
  }

  if (!any_moves_printed)
    gtp_printf("\n");
  gtp_printf("\n");

  return GTP_OK;
}


/* Function:  Return the number of bits of uncertainty in the move.
 *            If no previous genmove command has been issued, the result
 *            of this command will be meaningless.
 * Arguments: none
 * Fails:     never
 * Returns:   bits of uncertainty
 */
static int
gtp_move_uncertainty(char *s)
{
  float probabilities[BOARDMAX];
  int pos;
  double uncertainty = 0.0;

  UNUSED(s);

  compute_move_probabilities(probabilities);

  gtp_start_response(GTP_SUCCESS);
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (ON_BOARD(pos) && probabilities[pos] > 0.0) {
      /* Shannon's formula */
      uncertainty += -1 * ((double)probabilities[pos]) *
	log((double)probabilities[pos]) / log(2.0);
    }
  }

  gtp_printf("%.4f\n\n", uncertainty);

  return GTP_OK;
}



/* Function:  Return information about the followup influence after a move.
 * Arguments: move, what information
 * Fails:     never
 * Returns:   Influence data formatted like for initial_influence.
 */
static int
gtp_followup_influence(char *s)
{
  int color;
  int i, j;
  int n;

  n = gtp_decode_move(s, &color, &i, &j);
  if (n == 0)
    return gtp_failure("invalid move");

  prepare_move_influence_debugging(POS(i, j), color);
  
  return print_influence_data(&followup_influence, s + n);
}


/* Function:  Return the information in the worm data structure.
 * Arguments: optional vertex
 * Fails:     never
 * Returns:   Worm data formatted like:
 *
 * A19:
 * color           black
 * size            10
 * effective_size  17.83
 * origin          A19
 * liberties       8
 * liberties2      15
 * liberties3      10
 * liberties4      8
 * attack          PASS
 * attack_code     0
 * lunch           B19
 * defend          PASS
 * defend_code     0
 * cutstone        2
 * cutstone2       0
 * genus           0
 * inessential     0
 * B19:
 * color           white
 * .
 * .
 * .
 * inessential     0
 * C19:
 * ...
 *
 * If an intersection is specified, only data for this one will be returned.
 */
static int
gtp_worm_data(char *s)
{
  int i = -1;
  int j = -1;
  int m, n;

  if (sscanf(s, "%*c") >= 0 && !gtp_decode_coord(s, &i, &j))
    return gtp_failure("invalid color or coordinate");

  silent_examine_position(EXAMINE_WORMS);

  gtp_start_response(GTP_SUCCESS);
  
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++)
      if (i == -1 || (m == i && n == j)) {
	struct worm_data *w = &worm[POS(m, n)];
	gtp_print_vertex(m, n);
	gtp_printf(":\n");
	gtp_mprintf("origin               %m\n",  I(w->origin), J(w->origin));
	gtp_mprintf("color                %C\n",  w->color);
	gtp_printf("size                 %d\n",   w->size);
	gtp_printf("effective_size       %.2f\n", w->effective_size);
	gtp_printf("liberties            %d\n",   w->liberties);
	gtp_printf("liberties2           %d\n",   w->liberties2);
	gtp_printf("liberties3           %d\n",   w->liberties3);
	gtp_printf("liberties4           %d\n",   w->liberties4);
	gtp_printf("attack_code          %d\n",   w->attack_codes[0]);
	gtp_mprintf("attack_point         %m\n",  
		    I(w->attack_points[0]), J(w->attack_points[0]));
	gtp_printf("defense_code         %d\n",   w->defense_codes[0]);
	gtp_mprintf("defense_point        %m\n",  
		    I(w->defense_points[0]), J(w->defense_points[0]));
	gtp_mprintf("lunch                %m\n",  
		    I(w->lunch), J(w->lunch));
	gtp_printf("cutstone             %d\n",   w->cutstone);
	gtp_printf("cutstone2            %d\n",   w->cutstone2);
	gtp_printf("genus                %d\n",   w->genus);
	gtp_printf("inessential          %d\n",   w->inessential);
	gtp_printf("invincible           %d\n",   w->invincible);
	gtp_printf("unconditional_status %s\n",
		   status_to_string(w->unconditional_status));
      }
  
  gtp_printf("\n");
  return GTP_OK;
}

/* Function:  List the stones of a worm
 * Arguments: the location, "BLACK" or "WHITE"
 * Fails:     if called on an empty or off-board location
 * Returns:   list of stones
 */
static int
gtp_worm_stones(char *s)
{
  int i = -1;
  int j = -1;
  int color = EMPTY;
  int m, n;
  int u, v;
  int board_empty = 1;

  if (sscanf(s, "%*c") >= 0) {
    if (!gtp_decode_coord(s, &i, &j)
	&& !gtp_decode_color(s, &color))
      return gtp_failure("invalid coordinate");
  }
    
  if (BOARD(i, j) == EMPTY)
    return gtp_failure("worm_stones called on an empty vertex");

  gtp_start_response(GTP_SUCCESS);
  
  for (u = 0; u < board_size; u++)
    for (v = 0; v < board_size; v++) {
      if (BOARD(u, v) == EMPTY
	  || (color != EMPTY && BOARD(u, v) != color))
	continue;
      board_empty = 0;
      if (find_origin(POS(u, v)) != POS(u, v))
	continue;
      if (ON_BOARD2(i, j) 
	  && !same_string(POS(u, v), POS(i, j)))
	continue;
      for (m = 0; m < board_size; m++)
	for (n = 0; n < board_size; n++)
	  if (BOARD(m, n) != EMPTY
	      && same_string(POS(m, n), POS(u, v)))
	    gtp_mprintf("%m ", m, n);
      gtp_printf("\n");
    }
  
  if (board_empty) 
    gtp_printf("\n"); /* in case no stones have been printed */
  gtp_printf("\n");
  return GTP_OK;
}



/* Function:  Return the cutstone field in the worm data structure.
 * Arguments: non-empty vertex
 * Fails:     never
 * Returns:   cutstone
 */
static int
gtp_worm_cutstone(char *s)
{

  int i, j;
  if (!gtp_decode_coord(s, &i, &j))
    return gtp_failure("invalid coordinate");

  if (BOARD(i, j) == EMPTY)
    return gtp_failure("vertex must not be empty");

  silent_examine_position(EXAMINE_WORMS);

  return gtp_success(" %d", worm[POS(i, j)].cutstone);
}

/* Function:  Return the information in the dragon data structure.
 * Arguments: optional intersection
 * Fails:     never
 * Returns:   Dragon data formatted in the corresponding way to gtp_worm_data.
 */
static int
gtp_dragon_data(char *s)
{
  int i = -1;
  int j = -1;
  int m, n;
  int newline_needed = 0;

  if (sscanf(s, "%*c") >= 0 && !gtp_decode_coord(s, &i, &j))
    return gtp_failure("invalid coordinate");

  if (stackp > 0)
    return gtp_failure("dragon data unavailable when stackp > 0");

  silent_examine_position(FULL_EXAMINE_DRAGONS);

  gtp_start_response(GTP_SUCCESS);

  if (ON_BOARD2(i, j) && BOARD(i, j) == EMPTY)
    gtp_mprintf("%m empty\n", i, j);
  else {
    newline_needed = 1;
    for (m = 0; m < board_size; m++)
      for (n = 0; n < board_size; n++)
	if ((m == i && n == j)
	    || (i == -1
		&& BOARD(m, n) != EMPTY
		&& dragon[POS(m, n)].origin == POS(m, n))) {
	  gtp_print_vertex(m, n);
	  gtp_printf(":\n");
	  report_dragon(gtp_output_file, POS(m, n));
	  newline_needed = 0;
	}
  }
  if (newline_needed)
    gtp_printf("\n");
  gtp_printf("\n");
  return GTP_OK;
}

/* Function:  List the stones of a dragon
 * Arguments: the location
 * Fails:     if called on an empty or off-board location
 * Returns:   list of stones
 */
static int
gtp_dragon_stones(char *s)
{
  int i = -1;
  int j = -1;
  int color = EMPTY;
  int m, n;
  int u, v;

  if (sscanf(s, "%*c") >= 0) {
    if (!gtp_decode_coord(s, &i, &j)
	&& !gtp_decode_color(s, &color))
    return gtp_failure("invalid coordinate");
  }

  if (BOARD(i, j) == EMPTY)
    return gtp_failure("dragon_stones called on an empty vertex");

  silent_examine_position(EXAMINE_DRAGONS);

  gtp_start_response(GTP_SUCCESS);

  
  for (u = 0; u < board_size; u++)
    for (v = 0; v < board_size; v++) {
      if (BOARD(u, v) == EMPTY
	  || (color != EMPTY && BOARD(u, v) != color))
	continue;
      if (dragon[POS(u, v)].origin != POS(u, v))
	continue;
      if (ON_BOARD2(i, j) && dragon[POS(i, j)].origin != POS(u, v))
	continue;
      for (m = 0; m < board_size; m++)
	for (n = 0; n < board_size; n++)
	  if (dragon[POS(m, n)].origin == POS(u, v))
	    gtp_mprintf("%m ", m, n);
      gtp_printf("\n");
    }
  
  gtp_printf("\n");
  return GTP_OK;
}

/* Function:  Return the information in the eye data structure.
 * Arguments: color, vertex
 * Fails:     never
 * Returns:   eye data fields and values, one pair per row
 */
static int
gtp_eye_data(char *s)
{
  int color = EMPTY;
  int i = -1;
  int j = -1;
  struct eye_data *e;

  if (!gtp_decode_move(s, &color, &i, &j))
    return gtp_failure("invalid color or coordinate");

  if (stackp > 0)
    return gtp_failure("eye data unavailable when stackp > 0");

  silent_examine_position(EXAMINE_DRAGONS_WITHOUT_OWL);

  gtp_start_response(GTP_SUCCESS);

  if (color == BLACK)
    e = &black_eye[POS(i, j)];
  else
    e = &white_eye[POS(i, j)];
  
  gtp_mprintf("origin               %m\n", I(e->origin), J(e->origin));
  gtp_mprintf("color                %C\n", e->color);
  gtp_printf("esize                %d\n", e->esize);
  gtp_printf("msize                %d\n", e->msize);
  gtp_printf("value                %s\n", eyevalue_to_string(&e->value));
  gtp_printf("marginal             %d\n", e->marginal);
  gtp_printf("neighbors            %d\n", e->neighbors);
  gtp_printf("marginal_neighbors   %d\n", e->marginal_neighbors);
  
  gtp_printf("\n");
  return GTP_OK;
}


/* Function:  Return the information in the half eye data structure.
 * Arguments: vertex
 * Fails:     never
 * Returns:   half eye data fields and values, one pair per row
 */
static int
gtp_half_eye_data(char *s)
{
  int i = -1;
  int j = -1;
  struct half_eye_data *h;
  int k;

  if (!gtp_decode_coord(s, &i, &j))
    return gtp_failure("invalid coordinate");

  if (stackp > 0)
    return gtp_failure("half eye data unavailable when stackp > 0");

  silent_examine_position(EXAMINE_DRAGONS_WITHOUT_OWL);

  gtp_start_response(GTP_SUCCESS);

  h = &half_eye[POS(i, j)];
  
  gtp_printf("value                %.2f\n", h->value);
  if (h->type == HALF_EYE)
    gtp_printf("type                 HALF_EYE\n");
  else if (h->type == FALSE_EYE)
    gtp_printf("type                 FALSE_EYE\n");
  else
    gtp_printf("type                 %d\n", h->type);
  gtp_printf("num_attacks          %d\n", h->num_attacks);
  for (k = 0; k < h->num_attacks; k++)
    gtp_mprintf("attack_point[%d]      %m\n", k, I(h->attack_point[k]),
		J(h->attack_point[k]));
  gtp_printf("num_defenses         %d\n", h->num_defenses);
  for (k = 0; k < h->num_defenses; k++)
    gtp_mprintf("defense_point[%d]     %m\n", k, I(h->defense_point[k]),
		J(h->defense_point[k]));
  
  gtp_printf("\n");
  return GTP_OK;
}


static SGFTree gtp_sgftree;

/* Function:  Start storing moves executed during reading in an sgf
 *            tree in memory. 
 * Arguments: none
 * Fails:     never
 * Returns:   nothing
 *
 * Warning: You had better know what you're doing if you try to use this
 *          command.
 */
static int
gtp_start_sgftrace(char *s)
{
  UNUSED(s);
  sgffile_begindump(&gtp_sgftree);
  count_variations = 1;
  return gtp_success("");
}


/* Function:  Finish storing moves in an sgf tree and write it to file. 
 * Arguments: filename
 * Fails:     never
 * Returns:   nothing
 *
 * Warning: You had better know what you're doing if you try to use this
 *          command.
 */
static int
gtp_finish_sgftrace(char *s)
{
  char filename[GTP_BUFSIZE];
  int nread;
  
  nread = sscanf(s, "%s", filename);
  if (nread < 1)
    return gtp_failure("missing filename");

  sgffile_enddump(filename);
  count_variations = 0;
  return gtp_success("");
}


/* Function:  Dump the current position as a static sgf file to filename,
 *            or as output if filename is missing or "-" 
 * Arguments: optional filename
 * Fails:     never
 * Returns:   nothing if filename, otherwise the sgf
 */
static int
gtp_printsgf(char *s)
{
  char filename[GTP_BUFSIZE];
  int nread;
  int next;
  
  if (get_last_player() == EMPTY)
    next = BLACK;
  else
    next = OTHER_COLOR(get_last_player());

  nread = sscanf(s, "%s", filename);

  if (nread < 1)
    gg_snprintf(filename, GTP_BUFSIZE, "%s", "-");

  if (strcmp(filename, "-") == 0) {
    gtp_start_response(GTP_SUCCESS);
    sgffile_printsgf(next, filename);
    gtp_printf("\n");
    return GTP_OK;
  }
  else {
    sgffile_printsgf(next, filename);
    return gtp_success("");
  }
}


/* Function:  Tune the parameters for the move ordering in the tactical
 *            reading.
 * Arguments: MOVE_ORDERING_PARAMETERS integers
 * Fails:     incorrect arguments
 * Returns:   nothing
 */
static int
gtp_tune_move_ordering(char *s)
{
  int params[MOVE_ORDERING_PARAMETERS];
  int k;
  int p;
  int n;

  for (k = 0; k < MOVE_ORDERING_PARAMETERS; k++) {
    if (sscanf(s, "%d%n", &p, &n) == 0)
      return gtp_failure("incorrect arguments, expected %d integers",
			 MOVE_ORDERING_PARAMETERS);
    params[k] = p;
    s += n;
  }

  tune_move_ordering(params);
  return gtp_success("");
}

/* Function:  Echo the parameter
 * Arguments: string
 * Fails:     never
 * Returns:   nothing
 */
static int
gtp_echo(char *s)
{
  return gtp_success("%s", s);
}


/* Function:  Echo the parameter to stdout AND stderr
 * Arguments: string
 * Fails:     never
 * Returns:   nothing
 */
static int
gtp_echo_err(char *s)
{
  fprintf(stderr, "%s", s);
  fflush(gtp_output_file);
  fflush(stderr);
  return gtp_success("%s", s);
}

/* Function:  List all known commands
 * Arguments: none
 * Fails:     never
 * Returns:   list of known commands, one per line
 *
 * Status:    GTP version 2 standard command.
 */
static int
gtp_list_commands(char *s)
{
  int k;
  UNUSED(s);

  gtp_start_response(GTP_SUCCESS);

  for (k = 0; commands[k].name != NULL; k++)
    gtp_printf("%s\n", commands[k].name);

  gtp_printf("\n");
  return GTP_OK;
}


/* Function:  Tell whether a command is known.
 * Arguments: command name
 * Fails:     never
 * Returns:   "true" if command exists, "false" if not
 *
 * Status:    GTP version 2 standard command.
 */
static int
gtp_known_command(char *s)
{
  int k;
  char command[GTP_BUFSIZE];

  if (sscanf(s, "%s", command) == 1) {
    for (k = 0; commands[k].name != NULL; k++)
      if (strcmp(command, commands[k].name) == 0)
	return gtp_success("true");
  }

  return gtp_success("false");
}


/* Function:  Turn uncertainty reports from owl_attack
 *            and owl_defend on or off.
 * Arguments: "on" or "off"
 * Fails:     invalid argument
 * Returns:   nothing
 */
static int
gtp_report_uncertainty(char *s)
{
  if (!strncmp(s, "on", 2)) {
    report_uncertainty = 1;
    return gtp_success("");
  }
  if (!strncmp(s, "off", 3)) {
    report_uncertainty = 0;
    return gtp_success("");
  }
  return gtp_failure("invalid argument");
}
    

static void
gtp_print_code(int c)
{
  static int conversion[6] = { 
    0, /* LOSE */
    3, /* KO_B */
    5, /* LOSS */
    4, /* GAIN */
    2, /* KO_A */
    1, /* WIN  */
  };
  gtp_printf("%d", conversion[c]);
}

static void
gtp_print_vertices2(int n, int *moves)
{
  int movei[MAX_BOARD * MAX_BOARD];
  int movej[MAX_BOARD * MAX_BOARD];
  int k;

  for (k = 0; k < n; k++) {
    movei[k] = I(moves[k]);
    movej[k] = J(moves[k]);
  }
  
  gtp_print_vertices(n, movei, movej);
}

/*************
 * transform *
 *************/

static void
rotate_on_input(int ai, int aj, int *bi, int *bj)
{
  rotate(ai, aj, bi, bj, board_size, gtp_orientation);
}

static void
rotate_on_output(int ai, int aj, int *bi, int *bj)
{
  inv_rotate(ai, aj, bi, bj, board_size, gtp_orientation);
}


/***************
 * random seed *
 ***************/

/* Function:  Get the random seed
 * Arguments: none
 * Fails:     never
 * Returns:   random seed
 */
static int
gtp_get_random_seed(char *s)
{
  UNUSED(s);
  return gtp_success("%d", get_random_seed());
}

/* Function:  Set the random seed
 * Arguments: integer
 * Fails:     invalid data
 * Returns:   nothing
 */
static int
gtp_set_random_seed(char *s)
{
  int seed;
  if (sscanf(s, "%d", &seed) < 1)
    return gtp_failure("invalid seed");
  
  set_random_seed(seed);
  return gtp_success("");
}


/* Function:  Advance the random seed by a number of games.
 * Arguments: integer
 * Fails:     invalid data
 * Returns:   New random seed.
 */
static int
gtp_advance_random_seed(char *s)
{
  int i;
  int games;
  if (sscanf(s, "%d", &games) < 1
      || games < 0)
    return gtp_failure("invalid number of games");
  
  for (i = 0; i < games; i++)
    update_random_seed();

  return gtp_success("%d", get_random_seed());
}

/***************
 * surrounding *
 ***************/

/* Function:  Determine if a dragon is surrounded
 * Arguments: vertex (dragon)
 * Fails:     invalid vertex, empty vertex
 * Returns:   1 if surrounded, 2 if weakly surrounded, 0 if not
 */
static int
gtp_is_surrounded(char *s)
{
  int i, j;
  int n;

  n = gtp_decode_coord(s, &i, &j);
  if (n == 0)
    return gtp_failure("invalid coordinate");

  if (BOARD(i, j) == EMPTY)
    return gtp_failure("dragon vertex must be nonempty");

  silent_examine_position(EXAMINE_DRAGONS);
  return gtp_success("%d", DRAGON2(POS(i, j)).surround_status);
}

/* Function:  Determine if a move surrounds a dragon
 * Arguments: vertex (move), vertex (dragon)
 * Fails:     invalid vertex, empty (dragon, nonempty (move)
 * Returns:   1 if (move) surrounds (dragon)
 */
static int
gtp_does_surround(char *s)
{
  int si, sj, di, dj;
  int n;

  n = gtp_decode_coord(s, &si, &sj);
  if (n == 0)
    return gtp_failure("invalid coordinate");

  if (BOARD(si, sj) != EMPTY)
    return gtp_failure("move vertex must be empty");

  n = gtp_decode_coord(s + n, &di, &dj);
  if (n == 0)
    return gtp_failure("invalid coordinate");

  if (BOARD(di, dj) == EMPTY)
    return gtp_failure("dragon vertex must be nonempty");

  silent_examine_position(EXAMINE_DRAGONS);
  return gtp_success("%d", does_surround(POS(si, sj), POS(di, dj)));
}

/* Function:  Report the surround map for dragon at a vertex
 * Arguments: vertex (dragon), vertex (mapped location)
 * Fails:     invalid vertex, empty dragon
 * Returns:   value of surround map at (mapped location), or -1 if
 *            dragon not surrounded.
 */

static int
gtp_surround_map(char *s)
{
  int di, dj, mi, mj;
  int n;

  n = gtp_decode_coord(s, &di, &dj);
  if (n == 0)
    return gtp_failure("invalid coordinate");

  if (BOARD(di, dj) == EMPTY)
    return gtp_failure("dragon vertex must not be empty");

  n = gtp_decode_coord(s + n, &mi, &mj);
  if (n == 0)
    return gtp_failure("invalid coordinate");

  silent_examine_position(EXAMINE_DRAGONS);
  return gtp_success("%d", surround_map(POS(di, dj), POS(mi, mj)));
}

/***************
 * search area *
 ***************/

/* Function:  limit search, and establish a search diamond
 * Arguments: pos
 * Fails:     invalid value
 * Returns:   nothing
 */
static int
gtp_set_search_diamond(char *s)
{
  int i, j;

  if (!gtp_decode_coord(s, &i, &j))
    return gtp_failure("invalid coordinate");
  
  set_limit_search(1);
  set_search_diamond(POS(i, j));
  return gtp_success("");
}

/* Function:  unmark the entire board for limited search
 * Arguments: none
 * Fails:     never
 * Returns:   nothing
 */
static int
gtp_reset_search_mask(char *s)
{
  UNUSED(s);

  reset_search_mask();
  return gtp_success("");
}
  
/* Function:  sets the global variable limit_search
 * Arguments: value
 * Fails:     invalid arguments
 * Returns:   nothing
 */
static int
gtp_limit_search(char *s)
{
  int value;

  if (sscanf(s, "%d", &value) < 1)
    return gtp_failure("invalid value for search limit");
  set_limit_search(value);
  return gtp_success("");
}

/* Function:  mark a vertex for limited search
 * Arguments: position
 * Fails:     invalid arguments
 * Returns:   nothing
 */
static int
gtp_set_search_limit(char *s)
{
  int i, j;

  gtp_decode_coord(s, &i, &j);
  set_search_mask(POS(i, j), 1);
  return gtp_success("");
}
  
/* Function:  Draw search area. Writes to stderr.
 * Arguments: none
 * Fails:     never
 * Returns:   nothing
 */
static int
gtp_draw_search_area(char *s)
{
  UNUSED(s);

  gtp_start_response(GTP_SUCCESS);
  gtp_printf("\n");
  draw_search_area();
  return gtp_finish_response();
}



/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
