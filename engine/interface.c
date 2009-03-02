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

#include <stdlib.h>
#include <string.h>

#include "sgftree.h"
#include "liberty.h"
#include "clock.h"

#include "gg_utils.h"

/*
 * Initialize the gnugo engine. This needs to be called 
 * once only.
 */

void
init_gnugo(float memory, unsigned int seed)
{
  /* We need a fixed seed when initializing the Zobrist hashing to get
   * reproducable results.
   * FIXME: Test the quality of the seed.
   */
  set_random_seed(HASH_RANDOM_SEED);
  reading_cache_init(memory * 1024 * 1024);
  set_random_seed(seed);
  persistent_cache_init();
  clear_board();

  transformation_init();
  dfa_match_init();
  choose_mc_patterns(NULL);

  clear_approxlib_cache();
  clear_accuratelib_cache();
}


/* ---------------------------------------------------------------- */

/* Check whether we can accept a certain boardsize. Set out to NULL to
 * suppress informative messages. Return 1 for an acceptable
 * boardsize, 0 otherwise.
 */
int check_boardsize(int boardsize, FILE *out)
{
  int max_board = MAX_BOARD;
  if (use_monte_carlo_genmove && max_board > 9)
    max_board = 9;
  
  if (boardsize < MIN_BOARD || boardsize > max_board) {
    if (out) {
      fprintf(out, "Unsupported board size: %d. ", boardsize);
      if (boardsize < MIN_BOARD)
	fprintf(out, "Min size is %d.\n", MIN_BOARD);
      else {
	fprintf(out, "Max size is %d", max_board);
	if (max_board < MAX_BOARD)
	  fprintf(out, " (%d without --monte-carlo)", MAX_BOARD);
	fprintf(out, ".\n");
      }
      fprintf(out, "Try `gnugo --help' for more information.\n");
    }
    return 0;
  }

  return 1;
}

/*
 * Clear the board.
 */
void
gnugo_clear_board(int boardsize)
{
  board_size = boardsize;
  clear_board();
  init_timers();
#if 0
  if (metamachine && oracle_exists)
    oracle_clear_board(boardsize);
#endif
}

/* Play a move and start the clock */

void
gnugo_play_move(int move, int color)
{
#if ORACLE
  if (oracle_exists)
    oracle_play_move(move, color);
  else
    play_move(move, color);
#else
  play_move(move, color);
#endif
  clock_push_button(color);
}


/*
 * Perform the moves and place the stones from the SGF node on the 
 * board. Return the color of the player whose turn it is to move.
 */

int
gnugo_play_sgfnode(SGFNode *node, int to_move)
{
  SGFProperty *prop;

  for (prop = node->props; prop; prop = prop->next) {
    switch (prop->name) {
    case SGFAB:
      /* A black stone. */
      add_stone(get_sgfmove(prop), BLACK);
      break;

    case SGFAW:
      /* A white stone. */
      add_stone(get_sgfmove(prop), WHITE);
      break;

    case SGFPL:
      /* Player property - who is next to move? */
      if (prop->value[0] == 'w' || prop->value[0] == 'W')
	to_move = WHITE;
      else
	to_move = BLACK;
      break;

    case SGFW:
    case SGFB:
      /* An ordinary move. */
      to_move = (prop->name == SGFW) ? WHITE : BLACK;
      gnugo_play_move(get_sgfmove(prop), to_move);
      to_move = OTHER_COLOR(to_move);
      break;
    }
  }

  return to_move;
}


/* Interface to place_fixed_handicap. Sets up handicap stones and
 * updates the sgf file.
 */
int
gnugo_sethand(int desired_handicap, SGFNode *node)
{
  place_fixed_handicap(desired_handicap);
  sgffile_recordboard(node);
  return handicap;
}


/* Put upper and lower score estimates into *upper, *lower and
 * return the average. A positive score favors white. In computing
 * the upper bound, CRITICAL dragons are awarded to white; in
 * computing the lower bound, they are awarded to black.
 */

float
gnugo_estimate_score(float *upper, float *lower)
{
  silent_examine_position(EXAMINE_DRAGONS);
  if (upper != NULL)
    *upper = white_score;
  if (lower != NULL)
    *lower = black_score;
  return ((white_score + black_score) / 2.0);
}


/* ================================================================ */
/*                             Gameinfo                             */
/* ================================================================ */


/*
 * Initialize the structure.
 */

void
gameinfo_clear(Gameinfo *gameinfo)
{
  gnugo_clear_board(board_size);
  gameinfo->handicap = 0;
  gameinfo->to_move = BLACK;
  sgftree_clear(&gameinfo->game_record);

  /* Info relevant to the computer player. */
  gameinfo->computer_player = WHITE; /* Make an assumption. */
}


/*
 * Print a gameinfo.
 */

void
gameinfo_print(Gameinfo *gameinfo)
{
  printf("Board Size:   %d\n", board_size);
  printf("Handicap      %d\n", gameinfo->handicap);
  printf("Komi:         %.1f\n", komi);
  printf("Move Number:  %d\n", movenum);
  printf("To Move:      %s\n", color_to_string(gameinfo->to_move));

  printf("Computer player: ");
  if (gameinfo->computer_player == WHITE)
    printf("White\n");
  else if (gameinfo->computer_player == BLACK)
    printf("Black\n");
  else if (gameinfo->computer_player == EMPTY)
    printf("Both (solo)\n");
  else
    printf("Nobody\n");
}

/*
 * Play the moves in an SGF tree. Walk the main variation, actioning
 * the properties into the playing board.
 *
 * Returns the color of the next move to be made. The returned color
 * being EMPTY signals a failure to load the file.
 *
 * Head is an sgf tree. 
 * Untilstr is an optional string of the form either 'L12' or '120'
 * which tells it to stop playing at that move or move-number.
 * When debugging, this is the location of the move being examined.
 */

int
gameinfo_play_sgftree_rot(Gameinfo *gameinfo, SGFTree *tree,
			  const char *untilstr, int orientation)
{
  int bs;
  int next = BLACK;
  int untilmove = -1; /* Neither a valid move nor pass. */
  int until = 9999;
  
  if (!sgfGetIntProperty(tree->root, "SZ", &bs))
    bs = 19;

  if (!check_boardsize(bs, stderr))
    return EMPTY;
  
  handicap = 0;
  if (sgfGetIntProperty(tree->root, "HA", &handicap) && handicap > 1)
    next = WHITE;
  gameinfo->handicap = handicap;
  
  if (handicap > bs * bs - 1 || handicap < 0) {
    gprintf(" Handicap HA[%d] is unreasonable.\n Modify SGF file.\n",
	    handicap);
    return EMPTY;
  }
  
  gnugo_clear_board(bs);

  if (!sgfGetFloatProperty(tree->root, "KM", &komi)) {
    if (gameinfo->handicap == 0)
      komi = 5.5;
    else
      komi = 0.5;
  }

  /* Now we can safely parse the until string (which depends on board size). */
  if (untilstr) {
    if (*untilstr > '0' && *untilstr <= '9') {
      until = atoi(untilstr);
      DEBUG(DEBUG_LOADSGF, "Loading until move %d\n", until);
    }
    else {
      untilmove = string_to_location(board_size, untilstr);
      DEBUG(DEBUG_LOADSGF, "Loading until move at %1m\n", untilmove);
    }
  }
  
  /* Finally, we iterate over all the properties of all the
   * nodes, actioning them. We follow only the 'child' pointers,
   * as we have no interest in variations.
   *
   * The sgf routines map AB[aa][bb][cc] into AB[aa]AB[bb]AB[cc]
   */
  for (tree->lastnode = NULL; sgftreeForward(tree);) {
    SGFProperty *prop;
    int move;
      
    for (prop = tree->lastnode->props; prop; prop = prop->next) {
      DEBUG(DEBUG_LOADSGF, "%c%c[%s]\n", 
	    prop->name & 0xff, (prop->name >> 8), prop->value);
      switch (prop->name) {
      case SGFAB:
      case SGFAW:
	/* Generally the last move is unknown when the AB or AW
	 * properties are encountered. These are used to set up
	 * a board position (diagram) or to place handicap stones
	 * without reference to the order in which the stones are
	 * placed on the board.
	 */
	move = rotate1(get_sgfmove(prop), orientation);
	if (board[move] != EMPTY)
	  gprintf("Illegal SGF! attempt to add a stone at occupied point %1m\n",
		  move);
	else
	  add_stone(move, prop->name == SGFAB ? BLACK : WHITE);
	break;
	      
      case SGFPL:
	/* Due to a bad comment in the SGF FF3 definition (in the
         * "Alphabetical list of properties" section) some
         * applications encode the colors with 1 for black and 2 for
         * white.
	 */
	if (prop->value[0] == 'w'
	    || prop->value[0] == 'W'
	    || prop->value[0] == '2')
	  next = WHITE;
	else
	  next = BLACK;
	/* following really should not be needed for proper sgf file */
	if (stones_on_board(GRAY) == 0 && next == WHITE) {
	  place_fixed_handicap(gameinfo->handicap);
	  sgfOverwritePropertyInt(tree->root, "HA", handicap);
	}
	break;
	      
      case SGFW:
      case SGFB:
	next = prop->name == SGFW ? WHITE : BLACK;
	/* following really should not be needed for proper sgf file */
	if (stones_on_board(GRAY) == 0 && next == WHITE) {
	  place_fixed_handicap(gameinfo->handicap);
	  sgfOverwritePropertyInt(tree->root, "HA", handicap);
	}

	move = get_sgfmove(prop);
	if (move == untilmove || movenum == until - 1) {
	  gameinfo->to_move = next;
	  /* go back so that variant will be added to the proper node */
	  sgftreeBack(tree);
	  return next;
	}

	move = rotate1(move, orientation);
	if (move == PASS_MOVE || board[move] == EMPTY) {
	  gnugo_play_move(move, next);
	  next = OTHER_COLOR(next);
	}
	else {
	  gprintf("WARNING: Move off board or on occupied position found in sgf-file.\n");
	  gprintf("Move at %1m ignored, trying to proceed.\n", move);
	  gameinfo->to_move = next;
	  return next;
	}

	break;

      case SGFIL:
	/* The IL property is not a standard SGF property but
	 * is used by GNU Go to mark illegal moves. If a move
	 * is found marked with the IL property which is a ko
	 * capture then that ko capture is deemed illegal and
	 * (board_ko_i, board_ko_j) is set to the location of
	 * the ko.
	 */
	move = rotate1(get_sgfmove(prop), orientation);

	if (board_size > 1)
	{
	  int move_color;

	  if (ON_BOARD(NORTH(move)))
	    move_color = OTHER_COLOR(board[NORTH(move)]);
	  else 
	    move_color = OTHER_COLOR(board[SOUTH(move)]);
	  if (is_ko(move, move_color, NULL))
	    board_ko_pos = move;
	}
	break;
      }
    }
  }

  gameinfo->to_move = next;
  return next;
}

/* Same as previous function, using standard orientation */

int
gameinfo_play_sgftree(Gameinfo *gameinfo, SGFTree *tree, const char *untilstr)
{
  return gameinfo_play_sgftree_rot(gameinfo, tree, untilstr, 0);
}



/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
