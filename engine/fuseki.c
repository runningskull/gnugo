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

/* This value must be lower than the value for an ongoing joseki. */
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

/* The corner moves. */

static int corners[][2] =
{
  {3,3},
  {3,4},
  {4,3},
  {4,4},
  {5,3},
  {3,5},
  {5,4},
  {4,5},
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

  switch(corner) {
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


static void
announce_move(int i, int j, int val)
{
  /* This shouldn't happen. */
  if (BOARD(i, j))
    return;

  TRACE("Fuseki Player suggests %m with value %d\n", i, j, val);
  set_minimum_move_value(i, j, val);
}


/* Storage for values collected during pattern matching. */
static int fuseki_movei[MAX_BOARD * MAX_BOARD];
static int fuseki_movej[MAX_BOARD * MAX_BOARD];
static int fuseki_value[MAX_BOARD * MAX_BOARD];
static int fuseki_moves;
static int fuseki_total_value;

/* Callback for fuseki database pattern matching. */
static void
fuseki_callback(int ti, int tj, struct fullboard_pattern *pattern, int ll)
{
  TRACE("Fuseki database move at %m with relative weight %d, pattern %s+%d\n",
	ti, tj, (int) pattern->value, pattern->name, ll);

  /* Store coordinates and relative weight for the found move. */
  fuseki_movei[fuseki_moves] = ti;
  fuseki_movej[fuseki_moves] = tj;
  fuseki_value[fuseki_moves] = pattern->value;
  fuseki_total_value += pattern->value;
  fuseki_moves++;
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
  fuseki_moves = 0;
  fuseki_total_value = 0;
  fullboard_matchpat(fuseki_callback, color, database);

  /* No match. */
  if (fuseki_moves == 0)
    return 0;

  /* Choose randomly with respect to relative weights for matched moves. */
  /* do not choose moves with less value than 20% of the best move */
  best_fuseki_value = fuseki_value[0];
  q = gg_rand() % fuseki_total_value;
  for (k = 0; k < fuseki_moves; k++) {
    if (fuseki_value[k] < (best_fuseki_value / 5))
      break;
    q -= fuseki_value[k];
    if (q < 0)
      break;
  }

  gg_assert(k < fuseki_moves);
  /* Give this move an arbitrary value of 75. The actual value doesn't
   * matter much since the intention is that we should play this move
   * whatever the rest of the analysis thinks.
   */
  announce_move(fuseki_movei[k], fuseki_movej[k], 75);

  /* Also make sure the other considered moves can be seen in the
   * traces and in the output file.
   */
  for (k = 0; k < fuseki_moves; k++)
    set_minimum_move_value(fuseki_movei[k], fuseki_movej[k], 74);

  return 1;
}

/* Generate move in empty corner or in middle of small board.*/
void
fuseki(int color)
{
  int i = -1;
  int j = -1;
  int width;  /* Side of the open region required in the corner. */
  int empty_corner_value = EMPTY_CORNER_VALUE;

  /* Return immediately if --disable_fuseki option used. */
  if (disable_fuseki)
    return;

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
      announce_move(middle, middle, 45);
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
    announce_move(i, j, empty_corner_value);
  }
  
  if (openregion(board_size-width, board_size-1, 0, width-1)) {
    choose_corner_move(LOWER_LEFT, &i, &j);
    announce_move(i, j, empty_corner_value);
  }
  if (openregion(board_size-width, board_size-1,
		 board_size-width, board_size-1)) {
    choose_corner_move(LOWER_RIGHT, &i, &j);
    announce_move(i, j, empty_corner_value);
  }
  
  if (openregion(0, width-1, 0, width-1)) {
    choose_corner_move(UPPER_LEFT, &i, &j);
    announce_move(i, j, empty_corner_value);
  }
}

/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */

