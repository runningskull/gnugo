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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <assert.h>
#include <ctype.h>
#include <string.h>

#include "gnugo.h"
#include "interface.h"
#include "liberty.h"
#include "gtp.h"
#include "random.h"

/* Internal state that's not part of the engine. */
int color_to_move;
float komi;
int handicap;

static void
print_influence(float white_influence[MAX_BOARD][MAX_BOARD],
		float black_influence[MAX_BOARD][MAX_BOARD],
		int influence_regions[MAX_BOARD][MAX_BOARD]);
static void gtp_print_code(int c);
static int report_uncertainty = 0;

/* For undo, we keep the starting position, and a stack
 * listing all moves made.
 */

#define MAX_MOVES 1000
static Position starting_position;

struct game_move_data {
  int i;
  int j;
  int color;
};
static struct game_move_data game_move[MAX_MOVES];
static int move_stack_pointer = 0;

#define DECLARE(func) static int func(char *s, int id)

DECLARE(gtp_all_legal);
DECLARE(gtp_attack);
DECLARE(gtp_combination_attack);
DECLARE(gtp_countlib);
DECLARE(gtp_debug_influence);
DECLARE(gtp_debug_move_influence);
DECLARE(gtp_decrease_depths);
DECLARE(gtp_defend);
DECLARE(gtp_dragon_data);
DECLARE(gtp_dragon_status);
DECLARE(gtp_dump_stack);
DECLARE(gtp_echo);
DECLARE(gtp_eval_eye);
DECLARE(gtp_final_score);
DECLARE(gtp_final_status);
DECLARE(gtp_final_status_list);
DECLARE(gtp_findlib);
DECLARE(gtp_fixed_handicap);
DECLARE(gtp_genmove);
DECLARE(gtp_genmove_black);
DECLARE(gtp_genmove_white);
DECLARE(gtp_get_life_node_counter);
DECLARE(gtp_get_owl_node_counter);
DECLARE(gtp_get_reading_node_counter);
DECLARE(gtp_get_trymove_counter);
DECLARE(gtp_help);
DECLARE(gtp_increase_depths);
DECLARE(gtp_influence);
DECLARE(gtp_is_legal);
DECLARE(gtp_loadsgf);
DECLARE(gtp_move_influence);
DECLARE(gtp_name);
DECLARE(gtp_estimate_score);
DECLARE(gtp_owl_attack);
DECLARE(gtp_owl_defend);
DECLARE(gtp_playblack);
DECLARE(gtp_playwhite);
DECLARE(gtp_popgo);
DECLARE(gtp_captures);
DECLARE(gtp_protocol_version);
DECLARE(gtp_quit);
DECLARE(gtp_report_uncertainty);
DECLARE(gtp_reset_life_node_counter);
DECLARE(gtp_reset_owl_node_counter);
DECLARE(gtp_reset_reading_node_counter);
DECLARE(gtp_reset_trymove_counter);
DECLARE(gtp_same_dragon);
DECLARE(gtp_set_boardsize);
DECLARE(gtp_set_komi);
DECLARE(gtp_set_level);
DECLARE(gtp_showboard);
DECLARE(gtp_top_moves_white);
DECLARE(gtp_top_moves_black);
DECLARE(gtp_trymove);
DECLARE(gtp_tune_move_ordering);
DECLARE(gtp_undo);
DECLARE(gtp_version);
DECLARE(gtp_what_color);
DECLARE(gtp_worm_cutstone);
DECLARE(gtp_worm_data);

/* List of known commands. */
static struct gtp_command commands[] = {
  {"all_legal",        	      gtp_all_legal},
  {"attack",           	      gtp_attack},
  {"black",            	      gtp_playblack},
  {"boardsize",        	      gtp_set_boardsize},
  {"color",            	      gtp_what_color},
  {"combination_attack",      gtp_combination_attack},
  {"countlib",         	      gtp_countlib},
  {"debug_influence",         gtp_debug_influence},
  {"debug_move_influence",    gtp_debug_move_influence},
  {"decrease_depths",  	      gtp_decrease_depths},
  {"defend",           	      gtp_defend},
  {"dragon_data",             gtp_dragon_data},
  {"dragon_status",    	      gtp_dragon_status},
  {"dump_stack",       	      gtp_dump_stack},
  {"echo" ,                   gtp_echo},
  {"estimate_score",          gtp_estimate_score},
  {"eval_eye",         	      gtp_eval_eye},
  {"final_score",             gtp_final_score},
  {"final_status",            gtp_final_status},
  {"final_status_list",       gtp_final_status_list},
  {"findlib",          	      gtp_findlib},
  {"fixed_handicap",   	      gtp_fixed_handicap},
  {"genmove_black",           gtp_genmove_black},
  {"genmove_white",           gtp_genmove_white},
  {"get_life_node_counter",   gtp_get_life_node_counter},
  {"get_owl_node_counter",    gtp_get_owl_node_counter},
  {"get_reading_node_counter",gtp_get_reading_node_counter},
  {"get_trymove_counter",     gtp_get_trymove_counter},
  {"gg_genmove",              gtp_genmove},
  {"help",                    gtp_help},
  {"increase_depths",  	      gtp_increase_depths},
  {"influence",               gtp_influence},
  {"is_legal",         	      gtp_is_legal},
  {"komi",        	      gtp_set_komi},
  {"level",        	      gtp_set_level},
  {"loadsgf",          	      gtp_loadsgf},
  {"move_influence",          gtp_move_influence},
  {"name",                    gtp_name},
  {"new_score",               gtp_estimate_score},
  {"owl_attack",     	      gtp_owl_attack},
  {"owl_defend",     	      gtp_owl_defend},
  {"popgo",            	      gtp_popgo},
  {"captures",        	      gtp_captures},
  {"protocol_version",        gtp_protocol_version},
  {"quit",             	      gtp_quit},
  {"report_uncertainty",      gtp_report_uncertainty},
  {"reset_life_node_counter", gtp_reset_life_node_counter},
  {"reset_owl_node_counter",  gtp_reset_owl_node_counter},
  {"reset_reading_node_counter", gtp_reset_reading_node_counter},
  {"reset_trymove_counter",   gtp_reset_trymove_counter},
  {"same_dragon",    	      gtp_same_dragon},
  {"showboard",        	      gtp_showboard},
  {"top_moves_black",         gtp_top_moves_black},
  {"top_moves_white",         gtp_top_moves_white},
  {"trymove",          	      gtp_trymove},
  {"tune_move_ordering",      gtp_tune_move_ordering},
  {"undo",                    gtp_undo},
  {"version",                 gtp_version},
  {"white",            	      gtp_playwhite},
  {"worm_cutstone",           gtp_worm_cutstone},
  {"worm_data",               gtp_worm_data},
  {NULL,                      NULL}
};


/* Start playing using the Go Text Protocol. */
void
play_gtp()
{
  /* Try to make sure that we have a useful level of buffering of stdout. */
#ifdef HAVE_SETLINEBUF
  setlinebuf(stdout);
#else
  setbuf(stdout, NULL);
#endif

  /* Inform the GTP utility functions about the board size. */
  board_size = 19;
  gtp_internal_set_boardsize(19);
  
  /* Prepare pattern matcher and reading code. */
  reset_engine();
  move_stack_pointer = 0;
  store_position(&starting_position);
  gtp_main_loop(commands);
}


/****************************
 * Administrative commands. *
 ****************************/

/* Function:  Quit
 * Arguments: none
 * Fails:     never
 * Returns:   nothing
 */
static int
gtp_quit(char *s, int id)
{
  UNUSED(s);
  gtp_success(id, "");
  return GTP_QUIT;
}


/* Function:  Report protocol version.
 * Arguments: none
 * Fails:     never
 * Returns:   protocol version number
 */
static int
gtp_protocol_version(char *s, int id)
{
  UNUSED(s);
  return gtp_success(id, "1");
}


/****************************
 * Program identity.        *
 ****************************/

/* Function:  Report the name of the program.
 * Arguments: none
 * Fails:     never
 * Returns:   program name
 */
static int
gtp_name(char *s, int id)
{
  UNUSED(s);
  return gtp_success(id, "GNU Go");
}


/* Function:  Report the version number of the program.
 * Arguments: none
 * Fails:     never
 * Returns:   version number
 */
static int
gtp_version(char *s, int id)
{
  UNUSED(s);
  return gtp_success(id, VERSION);
}


/***************************
 * Setting the board size. *
 ***************************/

/* Function:  Set the board size to NxN and clear the board.
 * Arguments: integer
 * Fails:     board size outside engine's limits
 * Returns:   nothing
 */
static int
gtp_set_boardsize(char *s, int id)
{
  int boardsize;
  if (sscanf(s, "%d", &boardsize) < 1)
    return gtp_failure(id, "boardsize not an integer");
  
  if (boardsize < MIN_BOARD || boardsize > MAX_BOARD)
    return gtp_failure(id, "unacceptable boardsize");

  board_size = boardsize;
  clear_board();
  gtp_internal_set_boardsize(boardsize);
  store_position(&starting_position);
  return gtp_success(id, "");
}


/***************************
 * Setting komi.           *
 ***************************/

/* Function:  Set the komi.
 * Arguments: float
 * Fails:     incorrect argument
 * Returns:   nothing
 */
static int
gtp_set_komi(char *s, int id)
{
  if (sscanf(s, "%f", &komi) < 1)
    return gtp_failure(id, "komi not a float");
  
  return gtp_success(id, "");
}


/******************
 * Playing moves. *
 ******************/

/* Function:  Play a black stone at the given vertex.
 * Arguments: vertex
 * Fails:     invalid vertex, illegal move
 * Returns:   nothing
 */
static int
gtp_playblack(char *s, int id)
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
    return gtp_failure(id, "invalid coordinate");

  if (!is_legal(i, j, BLACK))
    return gtp_failure(id, "illegal move");

  play_move(i, j, BLACK);
  game_move[move_stack_pointer].i = i;
  game_move[move_stack_pointer].j = j;
  game_move[move_stack_pointer].color = BLACK;
  if (move_stack_pointer < MAX_MOVES)
    move_stack_pointer++;
  return gtp_success(id, "");
}


/* Function:  Play a white stone at the given vertex.
 * Arguments: vertex
 * Fails:     invalid vertex, illegal move
 * Returns:   nothing
 */
static int
gtp_playwhite(char *s, int id)
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
    return gtp_failure(id, "invalid coordinate");
  
  if (!is_legal(i, j, WHITE))
    return gtp_failure(id, "illegal move");

  play_move(i, j, WHITE);
  game_move[move_stack_pointer].i = i;
  game_move[move_stack_pointer].j = j;
  game_move[move_stack_pointer].color = WHITE;
  if (move_stack_pointer < MAX_MOVES)
    move_stack_pointer++;
  return gtp_success(id, "");
}


/* Function:  Set up fixed placement handicap stones.
 * Arguments: number of handicap stones
 * Fails:     invalid number of stones for the current boardsize
 * Returns:   list of vertices with handicap stones
 */
static int
gtp_fixed_handicap(char *s, int id)
{
  int m, n;
  int first = 1;
  int handicap;
  if (sscanf(s, "%d", &handicap) < 1)
    return gtp_failure(id, "handicap not an integer");
  
  clear_board();
  if (placehand(handicap) != handicap)
    return gtp_failure(id, "invalid handicap");

  gtp_printid(id, GTP_SUCCESS);

  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++)
      if (p[m][n] != EMPTY) {
	if (!first)
	  gtp_printf(" ");
	else
	  first = 0;
	gtp_mprintf("%m", m, n);
      }
  store_position(&starting_position);
  move_stack_pointer = 0;
  return gtp_finish_response();
}


/* Function:  Load an sgf file, possibly up to a move number or the first
 *            occurence of a move.           
 * Arguments: filename + move number, vertex, or nothing
 * Fails:     missing filename or failure to open or parse file
 * Returns:   color to play
 */
static int
gtp_loadsgf(char *s, int id)
{
  char filename[GTP_BUFSIZE];
  char untilstring[GTP_BUFSIZE];
  SGFNode *sgf;
  Gameinfo gameinfo;
  int nread;
  
  nread = sscanf(s, "%s %s", filename, untilstring);
  if (nread == 0)
    return gtp_failure(id, "missing filename");
  
  if ((sgf = readsgffile(filename)) == NULL)
    return gtp_failure(id, "cannot open or parse '%s'", filename);

  gameinfo_clear(&gameinfo, 19, 5.5); /* Probably unnecessary. */
  gameinfo_load_sgfheader(&gameinfo, sgf);
  if (nread == 1) {
    color_to_move = gameinfo_play_sgftree(&gameinfo, sgf, NULL);
  } else {
    color_to_move = gameinfo_play_sgftree(&gameinfo, sgf, untilstring);
  }
  gnugo_force_to_globals(&gameinfo.position);
  movenum = gameinfo.move_number;
  komi = gameinfo.position.komi;
  handicap = gameinfo.handicap;
  gtp_internal_set_boardsize(gameinfo.position.boardsize);
  reset_engine();
  store_position(&starting_position);

  move_stack_pointer = 0;

  gtp_printid(id, GTP_SUCCESS);
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
gtp_what_color(char *s, int id)
{
  int i, j;
  if (!gtp_decode_coord(s, &i, &j))
    return gtp_failure(id, "invalid coordinate");
  
  return gtp_success(id, color_to_string(p[i][j]));
}


/* Function:  Count number of liberties for the string at a vertex.
 * Arguments: vertex
 * Fails:     invalid vertex, empty vertex
 * Returns:   Number of liberties.
 */
static int
gtp_countlib(char *s, int id)
{
  int i, j;
  if (!gtp_decode_coord(s, &i, &j))
    return gtp_failure(id, "invalid coordinate");

  if (p[i][j] == EMPTY)
    return gtp_failure(id, "vertex must not be empty");

  return gtp_success(id, "%d", countlib(i, j));
}


/* Function:  Return the positions of the liberties for the string at a vertex.
 * Arguments: vertex
 * Fails:     invalid vertex, empty vertex
 * Returns:   Sorted space separated list of vertices.
 */
static int
gtp_findlib(char *s, int id)
{
  int i, j;
  int libi[MAXLIBS];
  int libj[MAXLIBS];
  int libs;
  
  if (!gtp_decode_coord(s, &i, &j))
    return gtp_failure(id, "invalid coordinate");

  if (p[i][j] == EMPTY)
    return gtp_failure(id, "vertex must not be empty");

  libs = findlib(i, j, MAXLIBS, libi, libj);
  gtp_printid(id, GTP_SUCCESS);
  gtp_print_vertices(libs, libi, libj);
  return gtp_finish_response();
}


/* Function:  Tell whether a move is legal.
 * Arguments: move
 * Fails:     invalid move
 * Returns:   1 if the move is legal, 0 if it is not.
 */
static int
gtp_is_legal(char *s, int id)
{
  int i, j;
  int color;
  
  if (!gtp_decode_move(s, &color, &i, &j))
    return gtp_failure(id, "invalid color or coordinate");

  return gtp_success(id, "%d", is_legal(i, j, color));
}


/* Function:  List all legal moves for either color.
 * Arguments: color
 * Fails:     invalid color
 * Returns:   Sorted space separated list of vertices.
 */
static int
gtp_all_legal(char *s, int id)
{
  int i, j;
  int color;
  int movei[MAX_BOARD * MAX_BOARD];
  int movej[MAX_BOARD * MAX_BOARD];
  int moves = 0;
  
  if (!gtp_decode_color(s, &color))
    return gtp_failure(id, "invalid color");

  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++)
      if (p[i][j] == EMPTY && is_legal(i, j, color)) {
	movei[moves] = i;
	movej[moves++] = j;
      }

  gtp_printid(id, GTP_SUCCESS);
  gtp_print_vertices(moves, movei, movej);
  return gtp_finish_response();
}


/* Function:  List the number of captures taken by either color.
 * Arguments: color
 * Fails:     invalid color
 * Returns:   Number of captures.
 */
static int
gtp_captures(char *s, int id)
{
  int color;
  
  if (!gtp_decode_color(s, &color))
    return gtp_failure(id, "invalid color");

  if (color == BLACK)
    return gtp_success(id, "%d", white_captured);
  else
    return gtp_success(id, "%d", black_captured);
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
gtp_trymove(char *s, int id)
{
  int i, j;
  int color;
  if (!gtp_decode_move(s, &color, &i, &j))
    return gtp_failure(id, "invalid color or coordinate");

  if (!trymove(i, j, color, "gtp_trymove", -1, -1, EMPTY, -1, -1))
    return gtp_failure(id, "illegal move");

  return gtp_success(id, "");
}


/* Function:  Undo a trymove.
 * Arguments: none
 * Fails:     stack empty
 * Returns:   nothing
 */
static int
gtp_popgo(char *s, int id)
{
  UNUSED(s);

  if (stackp == 0)
    return gtp_failure(id, "Stack empty.\n");

  popgo();
  return gtp_success(id, "");
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
gtp_attack(char *s, int id)
{
  int i, j;
  int ai, aj;
  int attack_code;
  
  if (!gtp_decode_coord(s, &i, &j))
    return gtp_failure(id, "invalid coordinate");

  if (p[i][j] == EMPTY)
    return gtp_failure(id, "vertex must not be empty");

  attack_code = attack(i, j, &ai, &aj);
  gtp_printid(id, GTP_SUCCESS);
  gtp_print_code(attack_code);
  if (attack_code > 0) {
    gtp_printf(" ");
    gtp_print_vertex(ai, aj);
  }
  return gtp_finish_response();
}  


/* Function:  Try to defend a string.
 * Arguments: vertex
 * Fails:     invalid vertex, empty vertex
 * Returns:   defense code followed by defense point if defense code nonzero.
 */
static int
gtp_defend(char *s, int id)
{
  int i, j;
  int di, dj;
  int defend_code;
  
  if (!gtp_decode_coord(s, &i, &j))
    return gtp_failure(id, "invalid coordinate");

  if (p[i][j] == EMPTY)
    return gtp_failure(id, "vertex must not be empty");

  defend_code = find_defense(i, j, &di, &dj);
  gtp_printid(id, GTP_SUCCESS);
  gtp_print_code(defend_code);
  if (defend_code > 0) {
    gtp_printf(" ");
    gtp_print_vertex(di, dj);
  }
  return gtp_finish_response();
}  


/* Function:  Increase depth values by one.
 * Arguments: none
 * Fails:     never
 * Returns:   nothing
 */
static int
gtp_increase_depths(char *s, int id)
{
  UNUSED(s);
  increase_depth_values();
  return gtp_success(id, "");
}  


/* Function:  Decrease depth values by one.
 * Arguments: none
 * Fails:     never
 * Returns:   nothing
 */
static int
gtp_decrease_depths(char *s, int id)
{
  UNUSED(s);
  decrease_depth_values();
  return gtp_success(id, "");
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
gtp_owl_attack(char *s, int id)
{
  int i, j;
  int ai, aj;
  int attack_code;
  int save_verbose = verbose;
  int result_certain;

  if (!gtp_decode_coord(s, &i, &j))
    return gtp_failure(id, "invalid coordinate");

  if (p[i][j] == EMPTY)
    return gtp_failure(id, "vertex must not be empty");

  verbose = 0;
  examine_position(p[i][j], EXAMINE_DRAGONS_WITHOUT_OWL);
  verbose = save_verbose;
  
  attack_code = owl_attack(i, j, &ai, &aj, &result_certain);
  gtp_printid(id, GTP_SUCCESS);
  gtp_print_code(attack_code);
  if (attack_code > 0) {
    gtp_printf(" ");
    gtp_print_vertex(ai, aj);
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
gtp_owl_defend(char *s, int id)
{
  int i, j;
  int di, dj;
  int defend_code;
  int save_verbose = verbose;
  int result_certain;
  
  if (!gtp_decode_coord(s, &i, &j))
    return gtp_failure(id, "invalid coordinate");

  if (p[i][j] == EMPTY)
    return gtp_failure(id, "vertex must not be empty");

  verbose = 0;
  examine_position(p[i][j], EXAMINE_DRAGONS_WITHOUT_OWL);
  verbose = save_verbose;

  defend_code = owl_defend(i, j, &di, &dj, &result_certain);
  gtp_printid(id, GTP_SUCCESS);
  gtp_print_code(defend_code);
  if (defend_code > 0) {
    gtp_printf(" ");
    gtp_print_vertex(di, dj);
  }
  if (!result_certain && report_uncertainty)
    gtp_printf(" uncertain");
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
gtp_eval_eye(char *s, int id)
{
  int m, n;
  int max, min;
  int attacki, attackj;
  int defendi, defendj;
  int i, j;
  int save_verbose = verbose;

  if (!gtp_decode_coord(s, &m, &n))
    return gtp_failure(id, "invalid coordinate");

  verbose = 0;
  examine_position(BLACK, EXAMINE_DRAGONS_WITHOUT_OWL);
  verbose = save_verbose;
  
  if (black_eye[m][n].color == BLACK_BORDER) {
    i = black_eye[m][n].origini;
    j = black_eye[m][n].originj;
    compute_eyes(i, j, &max, &min, &attacki, &attackj, &defendi, &defendj,
		 black_eye, half_eye, 0, EMPTY);
  }
  else if (white_eye[m][n].color == WHITE_BORDER) {
    i = white_eye[m][n].origini;
    j = white_eye[m][n].originj;
    compute_eyes(i, j, &max, &min, &attacki, &attackj, &defendi, &defendj,
		 white_eye, half_eye, 0, EMPTY);
  }
  else
    /* Not an eye or not of unique color. */
    return gtp_success(id, "-1");

  gtp_printid(id, GTP_SUCCESS);
  gtp_printf("%d %d", min, max);
  if (max != min) {
    gtp_printf(" ");
    gtp_print_vertex(attacki, attackj);
    gtp_printf(" ");
    gtp_print_vertex(defendi, defendj);
  }
  return gtp_finish_response();
}


/*****************
 * dragon status *
 *****************/

/* Function:  Determine status of a dragon.
 * Arguments: vertex
 * Fails:     invalid vertex, empty vertex
 * Returns:   status ("alive", "critical", "dead", or "unknown"),
 *            attack point, defense point. Points of attack and
 *            defense are only given if the status is critical and the
 *            owl code is enabled.
 *
 * FIXME: Should be able to distinguish between life in seki
 *        and life with territory. Should also be able to identify ko.
 */

static int
gtp_dragon_status(char *s, int id)
{
  int i, j;
  int save_verbose = verbose;

  if (!gtp_decode_coord(s, &i, &j))
    return gtp_failure(id, "invalid coordinate");

  if (p[i][j] == EMPTY)
    return gtp_failure(id, "vertex must not be empty");

  verbose = 0;
  examine_position(BLACK, EXAMINE_DRAGONS);
  verbose = save_verbose;
  
  /* FIXME: We should also call the semeai module. */

  if (dragon[i][j].owl_status == UNCHECKED) {
    if (dragon[i][j].status == ALIVE)
      return gtp_success(id, "alive");
  
    if (dragon[i][j].status == DEAD)
      return gtp_success(id, "dead");
  
    if (dragon[i][j].status == UNKNOWN)
      return gtp_success(id, "unknown");

    assert(dragon[i][j].status == CRITICAL); /* Only remaining possibility. */
    return gtp_success(id, "critical");
  }

  /* Owl code active. */
  if (dragon[i][j].owl_status == ALIVE)
    return gtp_success(id, "alive");
  
  if (dragon[i][j].owl_status == DEAD)
    return gtp_success(id, "dead");
  
  if (dragon[i][j].owl_status == UNKNOWN)
    return gtp_success(id, "unknown");
  
  assert(dragon[i][j].owl_status == CRITICAL);
  /* Status critical, need to return attack and defense point as well. */
  gtp_printid(id, GTP_SUCCESS);
  gtp_printf("critical ");
  gtp_print_vertex(dragon[i][j].owl_attacki, dragon[i][j].owl_attackj);
  gtp_printf(" ");
  gtp_print_vertex(dragon[i][j].owl_defendi, dragon[i][j].owl_defendj);
  return gtp_finish_response();
}


/* Function:  Determine whether two stones belong to the same dragon.
 * Arguments: vertex, vertex
 * Fails:     invalid vertex, empty vertex
 * Returns:   1 if the vertices belong to the same dragon, 0 otherwise
 */

static int
gtp_same_dragon(char *s, int id)
{
  int ai, aj;
  int bi, bj;
  int save_verbose = verbose;
  int n;

  n = gtp_decode_coord(s, &ai, &aj);
  if (n == 0)
    return gtp_failure(id, "invalid coordinate");

  if (!gtp_decode_coord(s + n, &bi, &bj))
    return gtp_failure(id, "invalid coordinate");

  if (p[ai][aj] == EMPTY || p[bi][bj] == EMPTY)
    return gtp_failure(id, "vertex must not be empty");

  verbose = 0;
  examine_position(BLACK, EXAMINE_DRAGONS);
  verbose = save_verbose;
  
  return gtp_success(id, "%d", dragon[ai][aj].id == dragon[bi][bj].id);
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
gtp_combination_attack(char *s, int id)
{
  int color;
  int i, j;
  int save_verbose = verbose;
  int n;

  n = gtp_decode_color(s, &color);
  if (!n)
    return gtp_failure(id, "invalid color");

  verbose = 0;
  examine_position(BLACK, EXAMINE_ALL);
  verbose = save_verbose;

  if (!atari_atari(color, &i, &j, verbose)) {
    i = -1;
    j = -1;
  }
  
  gtp_printid(id, GTP_SUCCESS);
  gtp_print_vertex(i, j);
  return gtp_finish_response();
}


/********************
 * generating moves *
 ********************/

/* Function:  Generate and play the supposedly best black move.
 * Arguments: none
 * Fails:     never
 * Returns:   a move coordinate (or "PASS")
 */
static int
gtp_genmove_black(char *s, int id)
{
  int i, j;
  UNUSED(s);
  
  if (genmove(&i, &j, BLACK) >= 0)
    play_move(i, j, BLACK);

  game_move[move_stack_pointer].i = i;
  game_move[move_stack_pointer].j = j;
  game_move[move_stack_pointer].color = BLACK;
  if (move_stack_pointer < MAX_MOVES)
    move_stack_pointer++;

  gtp_printid(id, GTP_SUCCESS);
  gtp_print_vertex(i, j);
  return gtp_finish_response();
}

/* Function:  Generate and play the supposedly best white move.
 * Arguments: none
 * Fails:     never
 * Returns:   a move coordinate (or "PASS")
 */
static int
gtp_genmove_white(char *s, int id)
{
  int i, j;
  UNUSED(s);
  if (genmove(&i, &j, WHITE) >= 0)
    play_move(i, j, WHITE);

  game_move[move_stack_pointer].i = i;
  game_move[move_stack_pointer].j = j;
  game_move[move_stack_pointer].color = WHITE;
  if (move_stack_pointer < MAX_MOVES)
    move_stack_pointer++;

  gtp_printid(id, GTP_SUCCESS);
  gtp_print_vertex(i, j);
  return gtp_finish_response();
}

/* Function:  Generate the supposedly best move for either color.
 * Arguments: color to move, optionally a random seed
 * Fails:     invalid color
 * Returns:   a move coordinate (or "PASS")
 */
static int
gtp_genmove(char *s, int id)
{
  int i, j;
  int color;
  int n;
  unsigned int seed;

  n = gtp_decode_color(s, &color);
  if (!n)
    return gtp_failure(id, "invalid color");

  /* This is intended for regression purposes and should therefore be
   * deterministic. The best way to ensure this is to reset the random
   * number generator before calling genmove(). By default it is
   * seeded with 0, but if an optional unsigned integer is given in
   * the command after the color, this is used as seed instead.
   */
  seed = 0;
  sscanf(s+n, "%u", &seed);
  gg_srand(seed);
  
  genmove_conservative(&i, &j, color);
  game_move[move_stack_pointer].i = i;
  game_move[move_stack_pointer].j = j;
  game_move[move_stack_pointer].color = color;
  if (move_stack_pointer < MAX_MOVES)
    move_stack_pointer++;

  gtp_printid(id, GTP_SUCCESS);
  gtp_print_vertex(i, j);
  return gtp_finish_response();
}

/* Function : Generate a list of the best moves for White with weights
 * Arguments: none
 * Fails:   : never
 * Returns  : list of moves with weights
 */

static int
gtp_top_moves_white(char *s, int id)
{
  int i, j, k;
  UNUSED(s);
  genmove(&i, &j, WHITE);
  gtp_printid(id, GTP_SUCCESS);
  for (k = 0; k < 10; k++)
    if (best_move_values[k] > 0.0) {
      gtp_print_vertex(best_movei[k], best_movej[k]);
      gtp_printf(" %.2f ", best_move_values[k]);
    }
  gtp_printf("\n");
  return GTP_OK;
}

static int
gtp_top_moves_black(char *s, int id)
{
  int i, j, k;
  UNUSED(s);
  genmove(&i, &j, BLACK);
  gtp_printid(id, GTP_SUCCESS);
  for (k = 0; k < 10; k++)
    if (best_move_values[k] > 0.0) {
      gtp_print_vertex(best_movei[k], best_movej[k]);
      gtp_printf(" %.2f ", best_move_values[k]);
    }
  gtp_printf("\n");
  return GTP_OK;
}



/* Function:  Set the playing level.
 * Arguments: int
 * Fails:     incorrect argument
 * Returns:   nothing
 */
static int
gtp_set_level(char *s, int id)
{
  int new_level;
  if (sscanf(s, "%d", &new_level) < 1)
    return gtp_failure(id, "level not an integer");
  
  level = new_level;
  return gtp_success(id, "");
}

/* Function:  Undo last move
 * Arguments: int
 * Fails:     If move pointer is 0
 * Returns:   nothing
 */


static int
gtp_undo(char *s, int id)
{
  int k;
  UNUSED(s);

  if (move_stack_pointer == 0)
    return gtp_failure(id, "no moves to take back");
  if (move_stack_pointer > MAX_MOVES)
    return gtp_failure(id, "move stack overflow");
  restore_position(&starting_position);

  move_stack_pointer--;
  for (k = 0; k < move_stack_pointer; k++)
    play_move(game_move[k].i, game_move[k].j, game_move[k].color);

  return gtp_success(id, "");
}
  

/***********
 * scoring *
 ***********/

static float final_score;
static int final_status[MAX_BOARD][MAX_BOARD];
static int status_numbers[6] = {ALIVE, DEAD, ALIVE_IN_SEKI, WHITE_TERRITORY,
				BLACK_TERRITORY, DAME};
static const char *status_names[6] = {"alive", "dead", "seki",
				      "white_territory", "black_territory",
				      "dame"};

/* Helper function. */
static void
finish_and_score_game(int seed)
{
  int move_val;
  int i, j;
  int next;
  int pass = 0;
  int moves = 0;
  int saved_board[MAX_BOARD][MAX_BOARD];
  Position saved_pos;
  static int current_board[MAX_BOARD][MAX_BOARD];
  static int current_seed = -1;
  int cached_board = 1;

  if (current_seed != seed) {
    current_seed = seed;
    cached_board = 0;
  }

  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++)
      if (p[i][j] != current_board[i][j]) {
	current_board[i][j] = p[i][j];
	cached_board = 0;
      }

  /* If this is exactly the same position as the one we analyzed the
   * last time, the contents of final_score and final_status are up to date.
   */
  if (cached_board)
    return;

  doing_scoring = 1;
  store_position(&saved_pos);

  /* FIXME: Letting black always start is a preliminary solution. */
  next = BLACK;
  do {
    move_val = genmove_conservative(&i, &j, next);
    play_move(i, j, next);
    if (move_val >= 0) {
      pass = 0;
      moves++;
    }
    else
      pass++;

    next = OTHER_COLOR(next);
  } while (pass < 2 && moves < board_size * board_size);

  final_score = aftermath_compute_score(next, komi);
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++) {
      final_status[i][j] = aftermath_final_status(next, i, j);
      saved_board[i][j] = p[i][j];
    }

  restore_position(&saved_pos);
  doing_scoring = 0;

  /* Update the status for vertices which were changed while finishing
   * the game, up to filling dame.
   */
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++) {
      if (p[i][j] == saved_board[i][j])
	continue;

      if (p[i][j] == EMPTY) {
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
      else if (p[i][j] == BLACK) {
	if (final_status[i][j] == WHITE_TERRITORY)
	  final_status[i][j] = DEAD;
	else if (final_status[i][j] == DAME)
	  final_status[i][j] = ALIVE_IN_SEKI;
	else if (final_status[i][j] == BLACK_TERRITORY)
	  final_status[i][j] = ALIVE;
	else
	  final_status[i][j] = DEAD;
      }
      else if (p[i][j] == WHITE) {
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
 */
static int
gtp_final_score(char *s, int id)
{
  int seed;
  /* This is intended for regression purposes and should therefore be
   * deterministic. The best way to ensure this is to reset the random
   * number generator before calling genmove(). By default it is
   * seeded with 0, but if an optional unsigned integer is given in
   * the command after the color, this is used as seed instead.
   */
  seed = 0;
  sscanf(s, "%u", &seed);
  gg_srand(seed);

  finish_and_score_game(seed);

  gtp_printid(id, GTP_SUCCESS);
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
gtp_final_status(char *s, int id)
{
  int seed;
  int n;
  int ai, aj;
  int k;
  const char *result = NULL;

  n = gtp_decode_coord(s, &ai, &aj);
  if (n == 0)
    return gtp_failure(id, "invalid coordinate");

  /* This is intended for regression purposes and should therefore be
   * deterministic. The best way to ensure this is to reset the random
   * number generator before calling genmove(). By default it is
   * seeded with 0, but if an optional unsigned integer is given in
   * the command after the color, this is used as seed instead.
   */
  seed = 0;
  sscanf(s + n, "%u", &seed);
  gg_srand(seed);

  finish_and_score_game(seed);

  for (k = 0; k < 6; k++)
    if (final_status[ai][aj] == status_numbers[k]) {
      result = status_names[k];
      break;
    }
  assert(result != NULL);

  return gtp_success(id, result);
}


/* Function:  Report vertices with a specific final status in a finished game.
 * Arguments: Status in the form of one of the strings "alive", "dead",
 *            "seki", "white_territory", "black_territory", or "dame".
 *            An optional random seed can be added.
 * Fails:     missing or invalid status string
 * Returns:   Vertices having the specified status. These are split with
 *            one string on each line if the vertices are nonempty (i.e.
 *            for "alive", "dead", and "seki").
 */
static int
gtp_final_status_list(char *s, int id)
{
  int seed;
  int n;
  int i, j;
  int status = UNKNOWN;
  int k;
  char status_string[GTP_BUFSIZE];
  int first;

  if (sscanf(s, "%s %n", status_string, &n) != 1)
    return gtp_failure(id, "missing status");
  
  for (k = 0; k < 6; k++) {
    if (strcmp(status_string, status_names[k]) == 0)
      status = status_numbers[k];
  }

  if (status == UNKNOWN)
    return gtp_failure(id, "invalid status");

  /* This is intended for regression purposes and should therefore be
   * deterministic. The best way to ensure this is to reset the random
   * number generator before calling genmove(). By default it is
   * seeded with 0, but if an optional unsigned integer is given in
   * the command after the color, this is used as seed instead.
   */
  seed = 0;
  sscanf(s + n, "%u", &seed);
  gg_srand(seed);

  finish_and_score_game(seed);

  gtp_printid(id, GTP_SUCCESS);

  first = 1;
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++) {
      if (final_status[i][j] != status)
	continue;
      if (p[i][j] == EMPTY) {
	if (!first)
	  gtp_printf(" ");
	else
	  first = 0;
	gtp_print_vertex(i, j);
      }
      else {
	int stones;
	int stonei[MAX_BOARD * MAX_BOARD];
	int stonej[MAX_BOARD * MAX_BOARD];
	int oi, oj;
	find_origin(i, j, &oi, &oj);
	if (i != oi || j != oj)
	  continue;
	if (!first)
	  gtp_printf("\n");
	else
	  first = 0;
	stones = findstones(i, j, board_size * board_size, stonei, stonej);
	gtp_print_vertices(stones, stonei, stonej);
      }
    }

  return gtp_finish_response();
}


static int
gtp_estimate_score(char *s, int id)
{
  int save_verbose = verbose;
  UNUSED(s);

  verbose = 0;
  examine_position(WHITE, EXAMINE_DRAGONS);
  verbose = save_verbose;
  
  score = estimate_score(&upper_bound, &lower_bound);
  gtp_printid(id, GTP_SUCCESS);
  if (score > 0.0) 
    gtp_printf("W+%3.1f (upper bound: %3.1f, lower: %3.1f)", 
	       score, upper_bound, lower_bound);
  else if (score < 0.0)
    gtp_printf("B+%3.1f (upper bound: %3.1f, lower: %3.1f)", 
	       -score, upper_bound, lower_bound);
  else
    gtp_printf("0");
  return gtp_finish_response();
}  


/**************
 * statistics *
 **************/

/* Function:  Reset the count of life nodes.
 * Arguments: none
 * Fails:     never
 * Returns:   nothing
 */
static int
gtp_reset_life_node_counter(char *s, int id)
{
  UNUSED(s);
  reset_life_node_counter();
  return gtp_success(id, "");
}


/* Function:  Retrieve the count of life nodes.
 * Arguments: none
 * Fails:     never
 * Returns:   number of life nodes
 */
static int
gtp_get_life_node_counter(char *s, int id)
{
  int nodes = get_life_node_counter();
  UNUSED(s);
  return gtp_success(id, "%d", nodes);
}


/* Function:  Reset the count of owl nodes.
 * Arguments: none
 * Fails:     never
 * Returns:   nothing
 */
static int
gtp_reset_owl_node_counter(char *s, int id)
{
  UNUSED(s);
  reset_owl_node_counter();
  return gtp_success(id, "");
}


/* Function:  Retrieve the count of owl nodes.
 * Arguments: none
 * Fails:     never
 * Returns:   number of owl nodes
 */
static int
gtp_get_owl_node_counter(char *s, int id)
{
  int nodes = get_owl_node_counter();
  UNUSED(s);
  return gtp_success(id, "%d", nodes);
}


/* Function:  Reset the count of reading nodes.
 * Arguments: none
 * Fails:     never
 * Returns:   nothing
 */
static int
gtp_reset_reading_node_counter(char *s, int id)
{
  UNUSED(s);
  reset_reading_node_counter();
  return gtp_success(id, "");
}


/* Function:  Retrieve the count of reading nodes.
 * Arguments: none
 * Fails:     never
 * Returns:   number of reading nodes
 */
static int
gtp_get_reading_node_counter(char *s, int id)
{
  int nodes = get_reading_node_counter();
  UNUSED(s);
  return gtp_success(id, "%d", nodes);
}


/* Function:  Reset the count of trymoves/trykos.
 * Arguments: none
 * Fails:     never
 * Returns:   nothing
 */
static int
gtp_reset_trymove_counter(char *s, int id)
{
  UNUSED(s);
  reset_trymove_counter();
  return gtp_success(id, "");
}


/* Function:  Retrieve the count of trymoves/trykos.
 * Arguments: none
 * Fails:     never
 * Returns:   number of trymoves/trykos
 */
static int
gtp_get_trymove_counter(char *s, int id)
{
  int nodes = get_trymove_counter();
  UNUSED(s);
  return gtp_success(id, "%d", nodes);
}



/*********
 * debug *
 *********/

/* Function:  Write the position to stderr.
 * Arguments: none
 * Fails:     never
 * Returns:   nothing
 */
static int
gtp_showboard(char *s, int id)
{
  UNUSED(s);
  showboard(0);
  return gtp_success(id, "");
}


/* Function:  Dump stack to stderr.
 * Arguments: none
 * Fails:     never
 * Returns:   nothing
 */
static int
gtp_dump_stack(char *s, int id)
{
  UNUSED(s);
  dump_stack();
  return gtp_success(id, "");
}


/* Function:  Write information about the influence function to stderr.
 * Arguments: color to move, optionally a list of what to show
 * Fails:     invalid color
 * Returns:   nothing
 */
static int
gtp_debug_influence(char *s, int id)
{
  int save_verbose   = verbose;
  int save_debug     = debug;
  int save_printmoyo = printmoyo;
  int color;

  if (!gtp_decode_color(s, &color))
    return gtp_failure(id, "invalid color");

  verbose   = 0;
  debug     = 0;
  printmoyo = 0;
  examine_position(color, EXAMINE_ALL);
  verbose   = save_verbose;
  debug     = save_debug;
  printmoyo = save_printmoyo;

  /* FIXME: Decide the choice of information from the command. */
  printmoyo |= (PRINTMOYO_NUMERIC_INFLUENCE | PRINTMOYO_PRINT_INFLUENCE
		| PRINTMOYO_PERMEABILITY | PRINTMOYO_STRENGTH
		| PRINTMOYO_ATTENUATION);
  print_initial_influence(color, 1);
  printmoyo = save_printmoyo;
  
  return gtp_success(id, "");
}


/* Function:  Write information about the influence function after making
 *            a move to stderr.
 * Arguments: move, optionally a list of what to show
 * Fails:     never
 * Returns:   nothing
 */
static int
gtp_debug_move_influence(char *s, int id)
{
  int save_verbose   = verbose;
  int save_debug     = debug;
  int save_printmoyo = printmoyo;
  int color;
  int i, j;
  char saved_stones[MAX_BOARD][MAX_BOARD];
  
  if (!gtp_decode_move(s, &color, &i, &j))
    return gtp_failure(id, "invalid color or coordinate");

  verbose   = 0;
  debug     = 0;
  printmoyo = 0;
  examine_position(color, EXAMINE_ALL);
  verbose   = save_verbose;
  debug     = save_debug;
  printmoyo = save_printmoyo;

  find_stones_saved_by_move(i, j, color, saved_stones);
  
  /* FIXME: Decide the choice of information from the command. */
  printmoyo |= (PRINTMOYO_NUMERIC_INFLUENCE | PRINTMOYO_PRINT_INFLUENCE
		| PRINTMOYO_PERMEABILITY | PRINTMOYO_STRENGTH);
  print_move_influence(i, j, color, saved_stones);
  printmoyo = save_printmoyo;

  return gtp_success(id, "");
}

/* Function:  Return information about the influence function.
 * Arguments: color to move
 * Fails:     never
 * Returns:   Influence data formatted like:
 *
 * white:
 *   0.51   1.34   3.20   6.60   9.09   8.06   1.96   0.00   0.00 
 *   0.45   1.65   4.92  12.19  17.47  15.92   4.03   0.00   0.00 
 *                   .
 *                   .
 *                   .
 *   0.00   0.00   0.00   0.00   0.00 100.00  75.53  41.47  23.41
 * black:
 *   1.57   2.51   4.10   3.10   3.60   4.54   8.32   4.15   2.71 
 *   2.96   4.62   9.18   5.47   5.89  10.88  20.54  10.19   4.08 
 *                   .
 *                   .
 *                   .
 * 100.00 139.39 100.00 139.39 100.00   0.00   0.00   0.00   0.00
 * regions:
 * -1  0  0  1  1  0 -1 -3 -3
 *              .
 *              .
 *              .
 * -3 -3 -3 -3 -3  3  3  3  3
 *
 * The encoding of the regions is as follows:
 *  3 white territory
 *  2 white moyo
 *  1 white area
 *  0 neutral
 * -1 black area
 * -2 black moyo
 * -3 black territory
 */
static int
gtp_influence(char *s, int id)
{
  int save_verbose   = verbose;
  int save_debug     = debug;
  int save_printmoyo = printmoyo;
  int color;
  float white_influence[MAX_BOARD][MAX_BOARD];
  float black_influence[MAX_BOARD][MAX_BOARD];
  int influence_regions[MAX_BOARD][MAX_BOARD];


  
  if (!gtp_decode_color(s, &color))
    return gtp_failure(id, "invalid color");

  verbose   = 0;
  debug     = 0;
  printmoyo = 0;
  examine_position(color, EXAMINE_ALL);
  verbose   = save_verbose;
  debug     = save_debug;
  printmoyo = save_printmoyo;

  gtp_printid(id, GTP_SUCCESS);
  get_initial_influence(color, 1, white_influence,
			black_influence, influence_regions);
  print_influence(white_influence, black_influence, influence_regions);
  /* We already have one newline and thus can't use gtp_finish_response(). */
  gtp_printf("\n");
  return GTP_OK;
}

static void
print_influence(float white_influence[MAX_BOARD][MAX_BOARD],
		float black_influence[MAX_BOARD][MAX_BOARD],
		int influence_regions[MAX_BOARD][MAX_BOARD])
{
  int m, n;
  gtp_printf("white:\n");
  for (m = 0; m < board_size; m++) {
    for (n = 0; n < board_size; n++) {
      gtp_printf("%6.2f ", white_influence[m][n]);
    }
    gtp_printf("\n");
  }

  gtp_printf("black:\n");
  for (m = 0; m < board_size; m++) {
    for (n = 0; n < board_size; n++) {
      gtp_printf("%6.2f ", black_influence[m][n]);
    }
    gtp_printf("\n");
  }

  gtp_printf("regions:\n");
  for (m = 0; m < board_size; m++) {
    for (n = 0; n < board_size; n++) {
      gtp_printf("%2d ", influence_regions[m][n]);
    }
    gtp_printf("\n");
  }
}


/* Function:  Return information about the influence function after a move
 * Arguments: move
 * Fails:     never
 * Returns:   Influence data formatted in the same way as for gtp_influence.
 */
static int
gtp_move_influence(char *s, int id)
{
  int save_verbose   = verbose;
  int save_debug     = debug;
  int save_printmoyo = printmoyo;
  int color;
  int i, j;
  char saved_stones[MAX_BOARD][MAX_BOARD];
  float white_influence[MAX_BOARD][MAX_BOARD];
  float black_influence[MAX_BOARD][MAX_BOARD];
  int influence_regions[MAX_BOARD][MAX_BOARD];

  if (!gtp_decode_move(s, &color, &i, &j))
    return gtp_failure(id, "invalid color or coordinate");

  verbose   = 0;
  debug     = 0;
  printmoyo = 0;
  examine_position(color, EXAMINE_ALL);
  verbose   = save_verbose;
  debug     = save_debug;
  printmoyo = save_printmoyo;

  find_stones_saved_by_move(i, j, color, saved_stones);
  
  gtp_printid(id, GTP_SUCCESS);
  get_move_influence(i, j, color, saved_stones, white_influence,
		     black_influence, influence_regions);
  print_influence(white_influence, black_influence, influence_regions);
  /* We already have one newline and thus can't use gtp_finish_response(). */
  gtp_printf("\n");
  return GTP_OK;
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
 * ko              0
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
gtp_worm_data(char *s, int id)
{
  int i = -1;
  int j = -1;
  int m, n;

  if (sscanf(s, " %*c") >= 0 && !gtp_decode_coord(s, &i, &j))
    return gtp_failure(id, "invalid color or coordinate");

  examine_position(EMPTY, EXAMINE_WORMS);

  gtp_printid(id, GTP_SUCCESS);
  
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++)
      if (i == -1 || (m == i && n == j)) {
	struct worm_data *w = &worm[m][n];
	gtp_print_vertex(m, n);
	gtp_printf(":\n");
	gtp_mprintf("color           %C\n",  w->color);
	gtp_printf("size            %d\n",   w->size);
	gtp_printf("effective_size  %.2f\n", w->effective_size);
	gtp_mprintf("origin          %m\n",  w->origini, w->originj);
	gtp_printf("liberties       %d\n",   w->liberties);
	gtp_printf("liberties2      %d\n",   w->liberties2);
	gtp_printf("liberties3      %d\n",   w->liberties3);
	gtp_printf("liberties4      %d\n",   w->liberties4);
	gtp_mprintf("attack          %m\n",  w->attacki, w->attackj);
	gtp_printf("attack_code     %d\n",   w->attack_code);
	gtp_mprintf("lunch           %m\n",  w->lunchi, w->lunchj);
	gtp_mprintf("defend          %m\n",  w->defendi, w->defendj);
	gtp_printf("defend_code     %d\n",   w->defend_code);
	gtp_printf("cutstone        %d\n",   w->cutstone);
	gtp_printf("cutstone2       %d\n",   w->cutstone2);
	gtp_printf("genus           %d\n",   w->genus);
	gtp_printf("ko              %d\n",   w->ko);
	gtp_printf("inessential     %d\n",   w->inessential);
      }
  
  gtp_printf("\n");
  return GTP_OK;
}

/* Function:  Return the cutstone field in the worm data structure.
 * Arguments: non-empty vertex
 * Fails:     never
 * Returns:   cutstone
 */
static int
gtp_worm_cutstone(char *s, int id)
{

  int i, j;
  if (!gtp_decode_coord(s, &i, &j))
    return gtp_failure(id, "invalid coordinate");

  if (p[i][j] == EMPTY)
    return gtp_failure(id, "vertex must not be empty");

  examine_position(EMPTY, EXAMINE_WORMS);

  return gtp_success(id, " %d", worm[i][j].cutstone);
}

/* Function:  Return the information in the dragon data structure.
 * Arguments: optional intersection
 * Fails:     never
 * Returns:   Dragon data formatted in the corresponding way to gtp_worm_data.
 */
static int
gtp_dragon_data(char *s, int id)
{
  int i = -1;
  int j = -1;
  int m, n;

  if (sscanf(s, " %*c") >= 0 && !gtp_decode_coord(s, &i, &j))
    return gtp_failure(id, "invalid color or coordinate");

  examine_position(EMPTY, EXAMINE_DRAGONS);

  gtp_printid(id, GTP_SUCCESS);
  
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++)
      if (i == -1 || (m == i && n == j)) {
	int k;
	struct dragon_data *d = &dragon[m][n];
	gtp_print_vertex(m, n);
	gtp_printf(":\n");
	gtp_printf("color                   %s\n",   
		   color_to_string(d->color));
	gtp_mprintf("origin                  %m\n",  d->origini, d->originj);
	gtp_mprintf("border                  %m\n",  d->borderi, d->borderj);
	gtp_printf("size                    %d\n",   d->size);
	gtp_printf("effective_size          %.2f\n", d->effective_size);
	gtp_printf("heyes                   %d\n",   d->heyes);
	gtp_mprintf("heye                    %m\n",  d->heyei, d->heyej);
	gtp_printf("genus                   %d\n",   d->genus);
	gtp_printf("escape_route            %d\n",   d->escape_route);
	gtp_mprintf("lunch                   %m\n",  d->lunchi, d->lunchj);
	gtp_printf("status                  %s\n",   
		   status_to_string(d->status));
	gtp_printf("owl_status              %s\n",   
		   status_to_string(d->owl_status));
	gtp_printf("matcher_status          %s\n",   
		   status_to_string(d->matcher_status));
	gtp_printf("owl_threat_status       %s\n",   
		   status_to_string(d->owl_threat_status));
	gtp_mprintf("owl_attack              %m\n",  
		    d->owl_attacki, d->owl_attackj);
	gtp_printf("owl_attack_certain:     %s\n",   
		   (d->owl_attack_certain) ? "YES" : "NO");
	gtp_mprintf("owl_2nd_attack          %m\n",  
		    d->owl_second_attacki, d->owl_second_attackj);
	gtp_mprintf("owl_defend              %m\n",  
		    d->owl_defendi, d->owl_defendj);
	gtp_printf("owl_defend_certain:     %s\n",   
		   (d->owl_defend_certain) ? "YES" : "NO");
	gtp_mprintf("owl_2nd_defend          %m\n",  
		    d->owl_second_defendi, d->owl_second_defendj);
	gtp_printf("semeai                  %d\n",   
		   d->semeai);
	gtp_printf("semeai_margin_of_safety %d\n",   
		   d->semeai_margin_of_safety);
	gtp_printf("neighbors:              ");
	for (k = 0; k < DRAGON2(m, n).neighbors; k++)
	  gtp_mprintf("%m ", 
		      DRAGON(DRAGON2(m, n).adjacent[k]).origini,
		      DRAGON(DRAGON2(m, n).adjacent[k]).originj);
	gtp_printf("\n");
	gtp_printf("moyo:                   %d\n", DRAGON2(m, n).moyo);
	gtp_printf("safety:                 %s\n", 
		   safety_to_string(DRAGON2(m, n).safety));
      }
  gtp_printf("\n");
  return GTP_OK;
}

/* Function:  Tune the parameters for the move ordering in the tactical
 *            reading.
 * Arguments: MOVE_ORDERING_PARAMETERS integers
 * Fails:     incorrect arguments
 * Returns:   nothing
 */
static int
gtp_tune_move_ordering(char *s, int id)
{
  int params[MOVE_ORDERING_PARAMETERS];
  int k;
  int p;
  int n;

  for (k = 0; k < MOVE_ORDERING_PARAMETERS; k++) {
    if (sscanf(s, "%d%n", &p, &n) == 0)
      return gtp_failure(id, "incorrect arguments, expected %d integers",
			 MOVE_ORDERING_PARAMETERS);
    params[k] = p;
    s += n;
  }

  tune_move_ordering(params);
  return gtp_success(id, "");
}

/* Function:  Echo the parameter
 * Arguments: string
 * Fails:     never
 * Returns:   nothing
 */
static int
gtp_echo(char *s, int id)
{
  return gtp_success(id, "%s", s);
}

/* Function:  List all known commands
 * Arguments: none
 * Fails:     never
 * Returns:   list of known commands, one per line
 */
static int
gtp_help(char *s, int id)
{
  int k;
  UNUSED(s);

  gtp_printid(id, GTP_SUCCESS);

  for (k = 0; commands[k].name != NULL; k++)
    gtp_printf("%s\n", commands[k].name);

  gtp_printf("\n");
  return GTP_OK;
}


/* Function:  Turn uncertainty reports from owl_attack
 *            and owl_defend on or off.
 * Arguments: "on" or "off"
 * Fails:     invalid argument
 * Returns:   nothing
 */
static int
gtp_report_uncertainty(char *s, int id)
{
  if (!strncmp(s, "on", 2)) {
    report_uncertainty = 1;
    return gtp_success(id, "");
  }
  if (!strncmp(s, "off", 3)) {
    report_uncertainty = 0;
    return gtp_success(id, "");
  }
  return gtp_failure(id, "invalid argument");
}
    

static void
gtp_print_code(int c)
{
  gtp_printf("%d", c ? 4-c : c);
}




/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
