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


#include "gnugo.h"

#include <stdlib.h>
#include <string.h>

#include "sgftree.h"
#include "liberty.h"
#include "clock.h"

#include <gg_utils.h>

/*
 * Initialize the gnugo engine. This needs to be called 
 * once only.
 */

void
init_gnugo(float memory)
{
  reading_cache_init(memory * 1024 * 1024);
  clear_board();

  dfa_match_init();
  tree_match_init();
}


/*
 * Put the values in POS into the global variables which is the
 * equivalent of the position.  
 */

static void
position_to_globals(Position *pos)
{
  board_size = pos->boardsize;
  setup_board(pos->board, pos->ko_pos, pos->last, pos->komi,
	      pos->white_captured, pos->black_captured);
}


/*
 * Get the values in POS from the global variables which is the
 * equivalent of the position.
 */

static void
globals_to_position(Position *pos)
{
  int k;
  int m, n;

  pos->boardsize = board_size;
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++)
      pos->board[m][n] = BOARD(m, n);

  pos->ko_pos = board_ko_pos;
  for (k = 0; k < 2; k++)
    pos->last[k] = last_moves[k];

  pos->komi = komi;
  pos->white_captured = white_captured;
  pos->black_captured = black_captured;
}


/* ---------------------------------------------------------------- */

void
store_position(Position *pos)
{
  globals_to_position(pos);
}

void
restore_position(Position *pos)
{
  position_to_globals(pos);
}

/* ---------------------------------------------------------------- */

/*
 * Clear a position.
 */
/* FIXME: komi is already included among the globals? */
void
gnugo_clear_position(Position *pos, int boardsize, float komi)
{
  gg_assert(MIN_BOARD <= boardsize && boardsize <= MAX_BOARD);
  board_size = boardsize;
  clear_board();
  globals_to_position(pos);
  pos->komi = komi;
}


/* Copy a position */
void
gnugo_copy_position(Position *to, Position *from)
{
  memcpy(to, from, sizeof(Position));
}

/* Wrapper for position_to_globals */
void
gnugo_force_to_globals(Position *pos)
{
  position_to_globals(pos);
}

/* Interface to add_stone */
void
gnugo_add_stone(Position *pos, int i, int j, int color)
{
  position_to_globals(pos);
  add_stone(POS(i, j), color);
  globals_to_position(pos);
}


/* Interface to remove_stone */
void
gnugo_remove_stone(Position *pos, int i, int j)
{
  position_to_globals(pos);
  remove_stone(POS(i, j));
  globals_to_position(pos);
}

int
gnugo_is_pass(int i, int j)
{
  return is_pass(POS(i, j));
}

/* Interface to play_move */
void
gnugo_play_move(Position *pos, int i, int j, int color)
{
  position_to_globals(pos);

  play_move(POS(i, j), color);
  clock_push_button(color);

  globals_to_position(pos);
}


/*
 * Perform the moves and place the stones from the SGF node on the 
 * board.  Return whose turn it is to move (WHITE or BLACK).
 */

int
gnugo_play_sgfnode(Position *pos, SGFNode *node, int to_move)
{
  int i, j;
  SGFProperty *prop;

  for (prop = node->props; prop; prop = prop->next) {
    switch (prop->name) {
    case SGFAB:
      /* A black stone. */
      get_moveXY(prop, &i, &j, pos->boardsize);
      gnugo_add_stone(pos, i, j, BLACK);
      break;

    case SGFAW:
      /* A white stone. */
      get_moveXY(prop, &i, &j, pos->boardsize);
      gnugo_add_stone(pos, i, j, WHITE);
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
      get_moveXY(prop, &i, &j, pos->boardsize);
      gnugo_play_move(pos, i, j, to_move);
      to_move = OTHER_COLOR(to_move);
      break;
    }
  }

  return to_move;
}


/*
 * Play the moves in ROOT UNTIL movenumber is reached.
 * Return whose turn it is to move (WHITE or BLACK).
 */
int
gnugo_play_sgftree(Position *position, SGFNode *root, int *until, 
		   SGFNode **curnode)
{
  SGFNode *node;
  int next = BLACK;
  SGFProperty *prop;
  int i, j;
  int movenumber = 0;

  /* Start from the empty board. */
  gnugo_clear_position(position, position->boardsize, komi);

  for (node = root; node; node = node->child) {
    for (prop = node->props; prop; prop = prop->next) {
      switch (prop->name) {
      case SGFAB:
	get_moveXY(prop, &i, &j, position->boardsize);
	gnugo_add_stone(position, i, j, BLACK);
	break;

      case SGFAW:
	get_moveXY(prop, &i, &j, position->boardsize);
	gnugo_add_stone(position, i, j, WHITE);
	break;

      case SGFPL:
	if (prop->value[0] == 'w' || prop->value[0] == 'W')
	  next = WHITE;
	else
	  next = BLACK;
	break;

      case SGFW:
      case SGFB:
	next = prop->name == SGFW ? WHITE : BLACK;
	get_moveXY(prop, &i, &j, position->boardsize);
	movenumber++;
	if (movenumber == *until) {
	  *curnode = node->parent;
	  return next;
	}
	gnugo_play_move(position, i, j, next);
	next = OTHER_COLOR(next);
	break;
      }
    }
  }
  *until = movenumber;

  return next;
}


/* Interface to is_legal. */
int
gnugo_is_legal(Position *pos, int i, int j, int color)
{
  int retval;

  position_to_globals(pos);
  retval = is_legal(POS(i, j), color);

  return retval;
}


/* Interface to is_suicide. */
int
gnugo_is_suicide(Position *pos, int i, int j, int color)
{
  int retval;

  position_to_globals(pos);
  retval = is_suicide(POS(i, j), color);

  return retval;
}


/* Interface to placehand. Sets up handicap pieces and
 * returns the number of placed handicap stones. */
int
gnugo_placehand(Position *pos, int handicap)
{
  int retval;

  position_to_globals(pos);
  retval = placehand(handicap);
  globals_to_position(pos);

  return retval;
}


/* Interface to sgffile_recordboard */
void
gnugo_recordboard(Position *pos, SGFNode *root)
{
  position_to_globals(pos);
  sgffile_recordboard(root);
}

/* Interface to placehand. Sets up handicap pieces and
 * returns the number of placed handicap stones, updating the sgf file
 */
int
gnugo_sethand(Position *pos, int handicap, SGFNode *node)
{
  int stones;

  position_to_globals(pos);
  stones = placehand(handicap);
  sgffile_recordboard(node);
  globals_to_position(pos);

  return stones;
}


/* Interface to genmove */
int
gnugo_genmove(Position *pos, int *i, int *j, int color, int move_number)
{
  position_to_globals(pos);
  movenum = move_number;	/* Another global */
  return genmove(i, j, color);
}

/* Interface to attack*/
int
gnugo_attack(Position *pos, int m, int n, int *i, int *j)
{
  int retval;
  int move;

  position_to_globals(pos);
  retval = attack(POS(m, n), &move);

  if (i) *i = I(move);
  if (j) *j = J(move);
  return retval;
}


/* Interface to find_defense */
int
gnugo_find_defense(Position *pos, int m, int n, int *i, int *j)
{
  int retval;
  int move;

  position_to_globals(pos);
  retval = find_defense(POS(m, n), &move);

  if (i) *i = I(move);
  if (j) *j = J(move);
  return retval;
}


/* Interface to who_wins */
void
gnugo_who_wins(Position *pos, int color, FILE *outfile)
{
  position_to_globals(pos);
  who_wins(color, outfile);
}


/* Put upper and lower score estimates into *upper, *lower and
 * return the average. A positive score favors white. In computing
 * the upper bound, CRITICAL dragons are awarded to white; in
 * computing the lower bound, they are awarded to black.
 */

float
gnugo_estimate_score(Position *pos, float *upper, float *lower)
{
  position_to_globals(pos);
  return estimate_score(lower, upper);
}


void
gnugo_examine_position(Position *pos, int color, int how_much)
{
  position_to_globals(pos);
  examine_position(color, how_much);
}


/* ================================================================ */
/*                             Gameinfo                             */
/* ================================================================ */


/*
 * Initialize the structure.
 */

void
gameinfo_clear(Gameinfo *ginfo, int boardsize, float komi)
{
  ginfo->handicap = 0;
  
  ginfo->move_number = 0;
  gnugo_clear_position(&(ginfo->position), boardsize, komi);
  ginfo->to_move = BLACK;
  sgftree_clear(&ginfo->moves);

  /* Info relevant to the computer player. */
  ginfo->seed = 0.0;
  ginfo->computer_player = WHITE; /* Make an assumtion. */

  ginfo->outfilename[0] = '\0';
  ginfo->outfile = NULL;
}


/*
 * Print a gameinfo.
 */

void
gameinfo_print(Gameinfo *ginfo)
{
  printf("Board Size:   %d\n", ginfo->position.boardsize);
  printf("Handicap      %d\n", ginfo->handicap);
  printf("Komi:         %.1f\n", ginfo->position.komi);
  printf("Move Number:  %d\n", ginfo->move_number);

  printf("To Move:      %s\n", color_to_string(ginfo->to_move));

  printf("Computer player: ");
  if (ginfo->computer_player == WHITE)
    printf("White\n");
  else if (ginfo->computer_player == BLACK)
    printf("Black\n");
  else if (ginfo->computer_player == EMPTY)
    printf("Both (solo)\n");
  else
    printf("Nobody\n");
}


/*
 * Reads header info from sgf structure and sets the appropriate variables.
 */

void
gameinfo_load_sgfheader(Gameinfo *gameinfo, SGFNode *head)
{
  int bsize;
  int handicap;
  float komi;
  
  if (!sgfGetIntProperty(head, "SZ", &bsize))
    bsize = 19;
  if (!sgfGetFloatProperty(head, "KM", &komi))
    komi = 5.5;
  gnugo_clear_position(&gameinfo->position, bsize, komi);
  
  if (!sgfGetIntProperty(head, "HA", &handicap) || handicap < 0)
    /* Handicap stones should appear as AW, AB properties in the sgf file. */
    handicap = 0;
  gameinfo->handicap = handicap;

  if (handicap > bsize * bsize - 1 || handicap < 0) {
    fprintf(stderr, " Handicap HA[%d] is unreasonable.\n", handicap);
    fprintf(stderr, " Modify SGF file.\n");
    exit (1);
  }
}


/*
 * Make a move in the game.  Return 1 if the move was legal. In that
 * case the move is actually done. Otherwise return 0.
 */

void
gameinfo_play_move(Gameinfo *ginfo, int i, int j, int color)
{
  gnugo_play_move(&ginfo->position, i, j, color);
  sgftreeAddPlay(&ginfo->moves, 0, color, i, j);

  sgffile_move_made(i, j, color, 0);

  ginfo->move_number++;
  ginfo->to_move = OTHER_COLOR(color);
}


/*
 * Play the moves in an SGF tree. Walk the main variation, actioning
 * the properties into the playing board.
 *
 * Returns the color of the next move to be made.
 *
 * Head is a sgf tree. 
 * Untilstr is an optional string of the form either 'L12' or '120'
 * which tells it to stop playing at that move or move-number.
 * When debugging, this will probably be a particularly bad move,
 * and we want to know why.
 */

int
gameinfo_play_sgftree_rot(Gameinfo *gameinfo, SGFNode *head,
			  const char *untilstr, int orientation)
{
  int bs, handicap;
  float komi;
  int next = BLACK;
  
  int untilm = -1, untiln = -1;
  int until = 9999;
  int addstone = 0;          /* handicap stone detector */
  
  SGFNode *node;
  
  if (!sgfGetFloatProperty(head, "KM", &komi)) {
    if (gameinfo->handicap == 0)
      komi = 5.5;
    else
      komi = 0.5;
  }
  if (!sgfGetIntProperty(head, "SZ", &bs))
    bs = 19;
  gnugo_clear_position(&gameinfo->position, bs, komi);

  /* Now we can safely parse the until string (which depends on board size). */
  if (untilstr) {
    if (*untilstr > '0' && *untilstr <= '9') {
      until = atoi(untilstr);
      DEBUG(DEBUG_LOADSGF, "Loading until move %d\n", until);
    }
    else {
      untiln = *untilstr - 'A';
      if (*untilstr >= 'I')
	--untiln;
	  
      untilm = gameinfo->position.boardsize - atoi(untilstr+1);
      DEBUG(DEBUG_LOADSGF, "Loading until move at %d,%d (%m)\n", 
	    untilm, untiln, untilm, untiln);
    }
  }
  
  if (sgfGetIntProperty(head, "HA", &handicap) && handicap > 1) {
    gameinfo->handicap = handicap;
    next = WHITE;
  }
  
  /* Finally, we iterate over all the properties of all the
   * nodes, actioning them. We follow only the 'child' pointers,
   * as we have no interest in variations.
   *
   * The sgf routines map AB[aa][bb][cc] into AB[aa]AB[bb]AB[cc]
   */
  for (node = head; node; node = node->child) {
    SGFProperty *prop;
    int i, j;
      
    for (prop = node->props; prop; prop = prop->next) {
      DEBUG(DEBUG_LOADSGF, "%c%c[%s]\n", 
	    prop->name & 0xff, (prop->name >> 8), prop->value);
      switch (prop->name) {
      case SGFAB:
	get_moveXY(prop, &i, &j, gameinfo->position.boardsize);
	/* Generally the last move is unknown when the AB or AW
	 * properties are encountered. These are used to set up
	 * a board position (diagram) or to place handicap stones
	 * without reference to the order in which the stones are
	 * placed on the board.
	 */
	last_moves[0] = 0;
	last_moves[1] = 0;
	rotate(i, j, &i, &j, gameinfo->position.boardsize, orientation);
	gnugo_add_stone(&gameinfo->position, i, j, BLACK);
	sgffile_put_stone(i, j, BLACK);
	addstone = 1;
	break;
	      
      case SGFAW:
	get_moveXY(prop, &i, &j, gameinfo->position.boardsize);
	last_moves[0] = 0;
	last_moves[1] = 0;
	rotate(i, j, &i, &j, gameinfo->position.boardsize, orientation);
	gnugo_add_stone(&gameinfo->position, i, j, WHITE);
	sgffile_put_stone(i, j, WHITE);
	addstone = 1;
	break;
	      
      case SGFPL:
	/* following really should not be needed for proper sgf file */
	if (!gameinfo->move_number && !addstone) {
	  gnugo_sethand(&gameinfo->position, gameinfo->handicap, 0);
	  sgfOverwritePropertyInt(head, "HA", handicap);
	}

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
	break;
	      
      case SGFW:
      case SGFB:
	next = prop->name == SGFW ? WHITE : BLACK;
	/* following really should not be needed for proper sgf file */
	if (!gameinfo->move_number && !addstone) {
	  gnugo_sethand(&gameinfo->position, gameinfo->handicap, 0);
	  sgfOverwritePropertyInt(head, "HA", handicap);
	}

	gameinfo->move_number++;
	if (gameinfo->move_number == until)
	  return next;
	      
	if (get_moveXY(prop, &i, &j, gameinfo->position.boardsize))
	  if (i == untilm && j == untiln)
	    return next;

	rotate(i, j, &i, &j, gameinfo->position.boardsize, orientation);
	gnugo_play_move(&gameinfo->position, i, j, next);
	sgffile_move_made(i, j, next, 0);
	next = OTHER_COLOR(next);
	      
	break;

      case SGFIL:
	/* The IL property is not a standard SGF property but
	 * is used by GNU Go to mark illegal moves. If a move
	 * is found marked with the IL property which is a ko
	 * capture then that ko capture is deemed illegal and
	 * (board_ko_i, board_ko_j) is set to the location of
	 * the ko.
	 */
	get_moveXY(prop, &i, &j, gameinfo->position.boardsize);
	rotate(i, j, &i, &j, gameinfo->position.boardsize, orientation);
	{
	  int move_color;

	  if (!ON_BOARD2(i, j))
	    break;
	  if (i > 0)
	    move_color = OTHER_COLOR(BOARD(i-1, j));
	  else 
	    move_color = OTHER_COLOR(BOARD(i+1, j));
	  if (is_ko(POS(i, j), move_color, NULL))
	    board_ko_pos = POS(i, j);
	}
	break;
      }
    }
  }

  gameinfo->to_move = next;
  return next;
}

int
gameinfo_play_sgftree(Gameinfo *gameinfo, SGFNode *head, const char *untilstr)
{
  return gameinfo_play_sgftree_rot(gameinfo, head, untilstr, 0);
}



/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
