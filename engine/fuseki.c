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

#include "liberty.h"
#include "patterns.h"
#include "random.h"

#include "sgftree.h"


/* Pointless to do fuseki database pattern matching after this number
 * of stones have been placed on the board.
 *
 * Notice that we are not talking of the move number here but the
 * number of stones actually residing on the board. This does in
 * particular include handicap stones.
 */
#define MAX_FUSEKI_DATABASE_STONES 19


#define UPPER_LEFT  0
#define UPPER_RIGHT 1
#define LOWER_LEFT  2
#define LOWER_RIGHT 3

/* Global variables remembering which symmetries the position has. */
static int horizontally_symmetric; /* symmetry with respect to K column */
static int vertically_symmetric;   /* symmetry with respect to 10 row */
static int diagonally_symmetric;   /* with respect to diagonal from UR to LL */

/* This value must be lower than the value for an ongoing joseki. 
 * (Gets multiplied with board_size / 19.) 
 */
#define EMPTY_CORNER_VALUE 25

/* check if region from i1, j1 to i2, j2 is open */

static int 
openregion(int i1, int i2, int j1, int j2)
{
  int x, y;

  if (i1 > i2)
    return openregion(i2, i1, j1, j2);
  if (j1 > j2)
    return openregion(i1, i2, j2, j1);

  for (x = i1; x <= i2; x++)
    for (y = j1; y <= j2; y++)
      if (BOARD(x, y) != EMPTY)
	return 0;
  return 1;
}

/* This function sets the global variables indicating symmetries of the
 * position. (Important for etiquette.)
 */
static void
set_symmetries(void)
{
  int i, j;
  horizontally_symmetric = 1;
  vertically_symmetric = 1; 
  diagonally_symmetric = 1;
  for (i = 0; i < board_size
              && (vertically_symmetric || horizontally_symmetric
		  || diagonally_symmetric); i++)
    for (j = 0; j < board_size; j++) {
      if (board[POS(i, j)] != board[POS(i, board_size - 1 - j)])
	horizontally_symmetric = 0;
      if (board[POS(i, j)] != board[POS(board_size - 1 - i, j)])
	vertically_symmetric = 0;
      if (board[POS(i, j)]
	  != board[POS(board_size - 1 - j, board_size - 1 - i)])
	diagonally_symmetric = 0;
    }
}

/* The corner moves. */

static int corners[][2] =
{
  {3, 3},
  {3, 4},
  {4, 3},
  {4, 4},
  {5, 3},
  {3, 5},
  {5, 4},
  {4, 5},
};

/* Relative weights for different corner moves at different board
   sizes. */

/* up to 11x11 */
static int small_board[] =
{
  50,       /* 3-3 */
  18,       /* 3-4 */
  17,       /* 4-3 */
  15,       /* 4-4 */
  0,        /* 5-3 */
  0,        /* 3-5 */
  0,        /* 5-4 */
  0,        /* 4-5 */
};

/* 12x12 to 15x15 */
static int medium_board[] =
{
  30,       /* 3-3 */
  20,       /* 3-4 */
  20,       /* 4-3 */
  22,       /* 4-4 */
  2,        /* 5-3 */
  2,        /* 3-5 */
  2,        /* 5-4 */
  2,        /* 4-5 */
};

/* 16x16 and larger */
static int large_board[] =
{
  15,       /* 3-3 */
  15,       /* 3-4 */
  15,       /* 4-3 */
  35,       /* 4-4 */
  5,        /* 5-3 */
  5,        /* 3-5 */
  5,        /* 5-4 */
  5,        /* 4-5 */
};

static void
choose_corner_move(int corner, int *m, int *n)
{
  int *table = 0;
  int sum_of_weights = 0;
  int i;
  int q;
  
  if (board_size <= 11)
    table = small_board;
  else if (board_size <= 15)
    table = medium_board;
  else 
    table = large_board;

  for (i = 0; i < 8 ;i++)
    sum_of_weights += table[i];

  q = gg_rand() % sum_of_weights;
  for (i = 0; i < 8; i++) {
    q -= table[i];
    if (q < 0)
      break;
  }
  
  *m = corners[i][0];
  *n = corners[i][1];

  switch (corner) {
  case UPPER_LEFT:
    *m = *m - 1;
    *n = *n - 1;
    break;
  case UPPER_RIGHT:
    *m = *m - 1;
    *n = board_size - *n;
    break;
  case LOWER_LEFT:
    *m = board_size - *m;
    *n = *n - 1;
    break;
  case LOWER_RIGHT:
    *m = board_size - *m;
    *n = board_size - *n;
    break;
  }
}


/* Announce move, but check for politeness first. */
static void
announce_move(int move, int val, int color)
{
  int i, j;
  /* This shouldn't happen. */
  if (board[move] != EMPTY)
    return;
  
  /* Politeness: Black plays in lower right half of upper right corner first.
   * White plays in upper left half of lower left corner first.
   * (Not sure whether this is correct for handicap games. Is this an
   * urgent FIXME? :-) )
   */
  if (horizontally_symmetric) {
    i = I(move);
    j = J(move);
    if ((2 * j < board_size - 1) ^ (color == WHITE))
      move = POS(i, board_size - 1 - j);
  }
  if (vertically_symmetric) {
    i = I(move);
    j = J(move);
    if ((2 * i > board_size - 1) ^ (color == WHITE))
      move = POS(board_size - 1 - i, j);
  }
  if (diagonally_symmetric) {
    i = I(move);
    j = J(move);
    if ((board_size - 1 - j > i) ^ (color == WHITE))
      move = POS(board_size - 1 - j, board_size - 1 - i);
  }
  
  if (set_minimum_move_value(move, val))
    TRACE("Fuseki Player suggests %1m with value %d\n", move, val);
}


/* Storage for values collected during pattern matching. */
static int fuseki_moves[MAX_BOARD * MAX_BOARD];
static int fuseki_value[MAX_BOARD * MAX_BOARD];
static int num_fuseki_moves;
static int fuseki_total_value;

/* Callback for fuseki database pattern matching. */
static void
fuseki_callback(int move, struct fullboard_pattern *pattern, int ll)
{
  TRACE("Fuseki database move at %1m with relative weight %d, pattern %s+%d\n",
	move, (int) pattern->value, pattern->name, ll);

  /* Store coordinates and relative weight for the found move. */
  fuseki_moves[num_fuseki_moves] = move;
  fuseki_value[num_fuseki_moves] = pattern->value;
  fuseki_total_value += pattern->value;
  num_fuseki_moves++;
}

/* Full board matching in database for fuseki moves. Return 1 if any
 * pattern found.
 */
static int
search_fuseki_database(int color)
{
  struct fullboard_pattern *database;
  int q;
  int k;
  int best_fuseki_value;

  /* Disable matching after a certain number of stones are placed on
   * the board.
   */
  if (stones_on_board(BLACK | WHITE) > MAX_FUSEKI_DATABASE_STONES)
    return 0;

  /* We only have databases for 9x9, 13x13 and 19x19. */
  if (board_size == 9)
    database = fuseki9;
  else if (board_size == 13)
    database = fuseki13;
  else if (board_size == 19)
    database = fuseki19;
  else
    return 0;

  /* Do the matching. */
  num_fuseki_moves = 0;
  fuseki_total_value = 0;
  fullboard_matchpat(fuseki_callback, color, database);

  /* No match. */
  if (num_fuseki_moves == 0)
    return 0;

  /* Choose randomly with respect to relative weights for matched moves. */
  /* Do not choose moves with less value than 20% of the best move */
  best_fuseki_value = fuseki_value[0];
  q = gg_rand() % fuseki_total_value;
  for (k = 0; k < num_fuseki_moves; k++) {
    if (fuseki_value[k] < (best_fuseki_value / 5))
      break;
    q -= fuseki_value[k];
    if (q < 0)
      break;
  }

  gg_assert(k < num_fuseki_moves);
  /* Give this move an arbitrary value of 75. The actual value doesn't
   * matter much since the intention is that we should play this move
   * whatever the rest of the analysis thinks.
   */
  announce_move(fuseki_moves[k], 75, color);

  /* Also make sure the other considered moves can be seen in the
   * traces and in the output file.
   */
  for (k = 0; k < num_fuseki_moves; k++)
    set_minimum_move_value(fuseki_moves[k], 74);

  return 1;
}

/* Generate move in empty corner or in middle of small board.*/
void
fuseki(int color)
{
  int i = -1;
  int j = -1;
  int width;  /* Side of the open region required in the corner. */
  int empty_corner_value = EMPTY_CORNER_VALUE * board_size/19;

  /* Return immediately if --disable_fuseki option used. */
  if (disable_fuseki)
    return;
  
  set_symmetries();

  /* Search in fuseki database unless disabled by --nofusekidb option. */
  if (fusekidb && search_fuseki_database(color))
    return;

  /* On 9x9, only play open corners after the first move if nothing
   * else useful is found.
   */
  if (board_size == 9 && stones_on_board(color) > 0)
    empty_corner_value = 5;
  
  if (board_size <= 11) {
    /* For boards of size 11x11 or smaller we first go for the center point. */
    int middle = board_size/2;
    if (openregion(middle-2, middle+2, middle-2, middle+2)) {
      announce_move(POS(middle, middle), 45, color);
    }
  }

  if (board_size < 9)
    return;

  if (board_size >= 18)
    width = 8;
  else if (board_size == 9)
    width = 5;
  else
    width = board_size/2;
  
  if (openregion(0, width-1, board_size-width, board_size-1)) {
    choose_corner_move(UPPER_RIGHT, &i, &j);
    announce_move(POS(i, j), empty_corner_value, color);
  }
  
  if (openregion(board_size-width, board_size-1, 0, width-1)) {
    choose_corner_move(LOWER_LEFT, &i, &j);
    announce_move(POS(i, j), empty_corner_value, color);
  }
  if (openregion(board_size-width, board_size-1,
		 board_size-width, board_size-1)) {
    choose_corner_move(LOWER_RIGHT, &i, &j);
    announce_move(POS(i, j), empty_corner_value, color);
  }
  
  if (openregion(0, width-1, 0, width-1)) {
    choose_corner_move(UPPER_LEFT, &i, &j);
    announce_move(POS(i, j), empty_corner_value, color);
  }
}

/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */

