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

#include <stdlib.h>
#include <string.h>

#include "sgftree.h"
#include "liberty.h"

#include "gg_utils.h"

/*
 * Initialize the gnugo engine. This needs to be called 
 * once only.
 */

void
init_gnugo()
{
  reading_cache_init();
  clear_board();

  transformation_init();
  dfa_match_init();
}


/* ---------------------------------------------------------------- */

/*
 * Clear the board.
 */
void
gnugo_clear_board(int boardsize)
{
  gg_assert(MIN_BOARD <= boardsize && boardsize <= MAX_BOARD);
  board_size = boardsize;
  clear_board();
}

/* Set the komi */

void
gnugo_set_komi(float new_komi)
{
  komi = new_komi;
}

/* Place a stone on the board */

void
gnugo_add_stone(int i, int j, int color)
{
  add_stone(POS(i, j), color);
}

/* Remove a stone from the board */

void
gnugo_remove_stone(int i, int j)
{
  remove_stone(POS(i, j));
}

/* Return true if (i,j) is PASS_MOVE */

int
gnugo_is_pass(int i, int j)
{
  return is_pass(POS(i, j));
}

/* Play a move and start the clock */

void
gnugo_play_move(int i, int j, int color)
{
  play_move(POS(i, j), color);
}

/* Undo n permanent moves. Returns 1 if successful and 0 if it fails.
 * If n moves cannot be undone, no move is undone.
 */

int
gnugo_undo_move(int n)
{
  return undo_move(n);
}


/*
 * Perform the moves and place the stones from the SGF node on the 
 * board. Return the color of the player whose turn it is to move.
 */

int
gnugo_play_sgfnode(SGFNode *node, int to_move)
{
  int i, j;
  SGFProperty *prop;

  for (prop = node->props; prop; prop = prop->next) {
    switch (prop->name) {
    case SGFAB:
      /* A black stone. */
      get_moveXY(prop, &i, &j, board_size);
      gnugo_add_stone(i, j, BLACK);
      break;

    case SGFAW:
      /* A white stone. */
      get_moveXY(prop, &i, &j, board_size);
      gnugo_add_stone(i, j, WHITE);
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
      get_moveXY(prop, &i, &j, board_size);
      gnugo_play_move(i, j, to_move);
      to_move = OTHER_COLOR(to_move);
      break;
    }
  }

  return to_move;
}


/*
 * Play the moves in ROOT UNTIL movenumber is reached.
 * Return the color of the player whose turn it is to move.
 */
int
gnugo_play_sgftree(SGFNode *root, int *until, SGFNode **curnode)
{
  SGFNode *node;
  int next = BLACK;
  SGFProperty *prop;
  int i, j;
  int movenumber = 0;

  /* Start from the empty board. */
  gnugo_clear_board(board_size);

  for (node = root; node; node = node->child) {
    for (prop = node->props; prop; prop = prop->next) {
      switch (prop->name) {
      case SGFAB:
	get_moveXY(prop, &i, &j, board_size);
	gnugo_add_stone(i, j, BLACK);
	break;

      case SGFAW:
	get_moveXY(prop, &i, &j, board_size);
	gnugo_add_stone(i, j, WHITE);
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
	get_moveXY(prop, &i, &j, board_size);
	movenumber++;
	if (movenumber == *until) {
	  *curnode = node->parent;
	  return next;
	}
	gnugo_play_move(i, j, next);
	next = OTHER_COLOR(next);
	break;
      }
    }
  }
  *until = movenumber;

  return next;
}


/* Interface to is_legal(). */
int
gnugo_is_legal(int i, int j, int color)
{
  return is_legal(POS(i, j), color);
}


/* Interface to is_suicide(). */
int
gnugo_is_suicide(int i, int j, int color)
{
  return is_suicide(POS(i, j), color);
}


/* Interface to placehand. Sets up handicap stones and
 * returns the number of placed handicap stones. */
int
gnugo_placehand(int handicap)
{
  return place_fixed_handicap(handicap);
}


/* Interface to sgffile_recordboard */
void
gnugo_recordboard(SGFNode *root)
{
  sgffile_recordboard(root);
}

/* Interface to placehand. Sets up handicap stones and
 * returns the number of placed handicap stones, updating the sgf file.
 */
int
gnugo_sethand(int handicap, SGFNode *node)
{
  int stones = place_fixed_handicap(handicap);
  sgffile_recordboard(node);
  return stones;
}


/* Interface to genmove() */
int
gnugo_genmove(int *i, int *j, int color)
{
  return genmove(i, j, color);
}

/* Interface to attack() */
int
gnugo_attack(int m, int n, int *i, int *j)
{
  int retval;
  int move;

  retval = attack(POS(m, n), &move);

  if (i)
    *i = I(move);
  if (j)
    *j = J(move);
  
  return retval;
}


/* Interface to find_defense() */
int
gnugo_find_defense(int m, int n, int *i, int *j)
{
  int retval;
  int move;

  retval = find_defense(POS(m, n), &move);

  if (i)
    *i = I(move);
  if (j)
    *j = J(move);

  return retval;
}


/* Interface to who_wins */
void
gnugo_who_wins(int color, FILE *outfile)
{
  who_wins(color, outfile);
}


/* Put upper and lower score estimates into *upper, *lower and
 * return the average. A positive score favors white. In computing
 * the upper bound, CRITICAL dragons are awarded to white; in
 * computing the lower bound, they are awarded to black.
 */

float
gnugo_estimate_score(float *upper, float *lower)
{
  return estimate_score(lower, upper);
}


/* Interface to examine_position(). */

void
gnugo_examine_position(int color, int how_much)
{
  examine_position(color, how_much);
}

/* Accessor functions for internal board state. */

/* Report the komi. */

float
gnugo_get_komi()
{
  return komi;
}

/* Place the board into the b array */

void
gnugo_get_board(int b[MAX_BOARD][MAX_BOARD])
{
  int i, j;
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++)
      b[i][j] = BOARD(i, j);
}

int
gnugo_get_boardsize()
{
  return board_size;
}

int
gnugo_get_move_number()
{
  return movenum;
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
  
  gnugo_clear_board(boardsize);
  gnugo_set_komi(komi);
  ginfo->to_move = BLACK;
  sgftree_clear(&ginfo->game_record);

  /* Info relevant to the computer player. */
  ginfo->computer_player = WHITE; /* Make an assumption. */
}


/*
 * Print a gameinfo.
 */

void
gameinfo_print(Gameinfo *ginfo)
{
  printf("Board Size:   %d\n", board_size);
  printf("Handicap      %d\n", ginfo->handicap);
  printf("Komi:         %.1f\n", komi);
  printf("Move Number:  %d\n", movenum);

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
  
  gnugo_clear_board(bsize);
  gnugo_set_komi(komi);
  
  if (!sgfGetIntProperty(head, "HA", &handicap) || handicap < 0)
    /* Handicap stones should appear as AW, AB properties in the sgf file. */
    handicap = 0;
  gameinfo->handicap = handicap;

  if (handicap > bsize * bsize - 1 || handicap < 0) {
    fprintf(stderr, " Handicap HA[%d] is unreasonable.\n", handicap);
    fprintf(stderr, " Modify SGF file.\n");
    exit(EXIT_FAILURE);
  }
}


/*
 * Make a move in the game.  Return 1 if the move was legal. In that
 * case the move is actually done. Otherwise return 0.
 */

void
gameinfo_play_move(Gameinfo *ginfo, int i, int j, int color)
{
  gnugo_play_move(i, j, color);
  sgftreeAddPlay(&ginfo->game_record, color, i, j);

  ginfo->to_move = OTHER_COLOR(color);
}


/*
 * Play the moves in an SGF tree. Walk the main variation, actioning
 * the properties into the playing board.
 *
 * Returns the color of the next move to be made.
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
  int bs, handicap;
  float komi;
  int next = BLACK;
  
  int untilm = -1, untiln = -1;
  int until = 9999;
  int addstone = 0;          /* handicap stone detector */
  
  if (!sgfGetFloatProperty(tree->root, "KM", &komi)) {
    if (gameinfo->handicap == 0)
      komi = 5.5;
    else
      komi = 0.5;
  }
  if (!sgfGetIntProperty(tree->root, "SZ", &bs))
    bs = 19;
  gnugo_clear_board(bs);
  gnugo_set_komi(komi);

  /* Now we can safely parse the until string (which depends on board size). */
  if (untilstr) {
    if (*untilstr > '0' && *untilstr <= '9') {
      until = atoi(untilstr);
      TRACE_LOADSGF("Loading until move %d\n", until);
    }
    else {
      untiln = *untilstr - 'A';
      if (*untilstr >= 'I')
	--untiln;
	  
      untilm = board_size - atoi(untilstr+1);
      TRACE_LOADSGF("Loading until move at %d,%d (%m)\n", 
	    untilm, untiln, untilm, untiln);
    }
  }
  
  if (sgfGetIntProperty(tree->root, "HA", &handicap) && handicap > 1) {
    gameinfo->handicap = handicap;
    next = WHITE;
  }
  
  /* Finally, we iterate over all the properties of all the
   * nodes, actioning them. We follow only the 'child' pointers,
   * as we have no interest in variations.
   *
   * The sgf routines map AB[aa][bb][cc] into AB[aa]AB[bb]AB[cc]
   */
  for (tree->lastnode = NULL; sgftreeForward(tree);) {
    SGFProperty *prop;
    int i, j;
      
    for (prop = tree->lastnode->props; prop; prop = prop->next) {
      TRACE_LOADSGF("%c%c[%s]\n", 
	    prop->name & 0xff, (prop->name >> 8), prop->value);
      switch (prop->name) {
      case SGFAB:
	get_moveXY(prop, &i, &j, board_size);
	/* Generally the last move is unknown when the AB or AW
	 * properties are encountered. These are used to set up
	 * a board position (diagram) or to place handicap stones
	 * without reference to the order in which the stones are
	 * placed on the board.
	 */
	rotate(i, j, &i, &j, board_size, orientation);
	gnugo_add_stone(i, j, BLACK);
	addstone = 1;
	break;
	      
      case SGFAW:
	get_moveXY(prop, &i, &j, board_size);
	rotate(i, j, &i, &j, board_size, orientation);
	gnugo_add_stone(i, j, WHITE);
	addstone = 1;
	break;
	      
      case SGFPL:
	/* following really should not be needed for proper sgf file */
	if (movenum != 0 && !addstone) {
	  gnugo_sethand(gameinfo->handicap, 0);
         sgfOverwritePropertyInt(tree->root, "HA", gameinfo->handicap);
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
	if (movenum != 0 && !addstone) {
	  gnugo_sethand(gameinfo->handicap, 0);
         sgfOverwritePropertyInt(tree->root, "HA", gameinfo->handicap);
	}

	if (movenum == until - 1) {
	  gameinfo->to_move = next;
         /* go back so that variant will be added to the proper node */
         sgftreeBack(tree);
	  return next;
	}
	      
	if (get_moveXY(prop, &i, &j, board_size))
	  if (i == untilm && j == untiln) {
	    gameinfo->to_move = next;
            /* go back so that variant will be added to the proper node */
           sgftreeBack(tree);
	    return next;
	  }

	rotate(i, j, &i, &j, board_size, orientation);
	if ((ON_BOARD2(i, j) && board[POS(i, j)] == EMPTY)
	    || (i == -1 && j == -1)) {
	  gnugo_play_move(i, j, next);
	  next = OTHER_COLOR(next);
	}
	else {
	  gprintf("WARNING: Move off board or on occupied position found in sgf-file.\n");
	  gprintf("Move at %m ignored, trying to proceed.\n", i, j);
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
	get_moveXY(prop, &i, &j, board_size);
	rotate(i, j, &i, &j, board_size, orientation);
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
