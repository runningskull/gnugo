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

#include <string.h>
#include "liberty.h"
#include "gg_utils.h"


static int dilate_erode(int dilations, int erosions, 
			int gb[BOARDMAX], int color);
static void print_new_moyo(int dilations, int erosions);
static void close_bubbles(int gb[BOARDMAX]);
static int captured_territory(int pos, int color);


#define ARRAYSIZE MAX_BOARD*MAX_BOARD


/* As explained in the Texinfo documentation, this function
 * takes the characteristic function of the live groups,
 * dilates and erodes the required number of times. If
 * erosions = dilations*(dilations-1)+1, an isolated stone
 * does not get any area, suitable for computing territory.
 *
 * If dragons are CRITICAL, these are awarded to the player
 * 'color' for the purpose of determining an upper and lower
 * bound. The function returns 1 if critical dragons are found,
 * otherwise 0.
 */

static int
dilate_erode(int dilations, int erosions, int gb[BOARDMAX], int color)
{
  int ii;
  int work[BOARDMAX];
  int n;
  int critical_found = 0;
  
  for (ii = BOARDMIN; ii < BOARDMAX; ii++) {
    if (!ON_BOARD(ii))
      continue;

    if (board[ii] && dragon[ii].status == CRITICAL)
      critical_found = 1;
    if (board[ii] == WHITE && !captured_territory(ii, color))
      gb[ii] = 128;
    else if (board[ii] == BLACK && !captured_territory(ii, color))      
      gb[ii] = -128;
    else
      gb[ii] = 0;
  }
  
  /* dilate */
  memcpy(work, gb, sizeof(work));
  for (n = 0; n < dilations; n++) {
    for (ii = BOARDMIN; ii < BOARDMAX; ii++) {
      if (!ON_BOARD(ii))
	continue;
      
      if (gb[ii] >= 0
	  && (!ON_BOARD(SOUTH(ii)) || gb[SOUTH(ii)] >= 0)
	  && (!ON_BOARD(WEST(ii))  || gb[WEST(ii)]  >= 0)
	  && (!ON_BOARD(NORTH(ii)) || gb[NORTH(ii)] >= 0)
	  && (!ON_BOARD(EAST(ii))  || gb[EAST(ii)]  >= 0)) {
	if (ON_BOARD(SOUTH(ii)) && gb[SOUTH(ii)] > 0)
	  work[ii]++;
	if (ON_BOARD(WEST(ii))  && gb[WEST(ii)]  > 0)
	  work[ii]++;
	if (ON_BOARD(NORTH(ii)) && gb[NORTH(ii)] > 0)
	  work[ii]++;
	if (ON_BOARD(EAST(ii))  && gb[EAST(ii)]  > 0)
	  work[ii]++;
      }
	
      if (gb[ii] <= 0
	  && (!ON_BOARD(SOUTH(ii)) || gb[SOUTH(ii)] <= 0)
	  && (!ON_BOARD(WEST(ii))  || gb[WEST(ii)]  <= 0)
	  && (!ON_BOARD(NORTH(ii)) || gb[NORTH(ii)] <= 0)
	  && (!ON_BOARD(EAST(ii))  || gb[EAST(ii)]  <= 0)) {
	if (ON_BOARD(SOUTH(ii)) && gb[SOUTH(ii)] < 0)
	  work[ii]--;
	if (ON_BOARD(WEST(ii))  && gb[WEST(ii)]  < 0)
	  work[ii]--;
	if (ON_BOARD(NORTH(ii)) && gb[NORTH(ii)] < 0)
	  work[ii]--;
	if (ON_BOARD(EAST(ii))  && gb[EAST(ii)]  < 0)
	  work[ii]--;
      }
    }
    memcpy(gb, work, sizeof(work));
  }

  /* erode */
  for (n = 0; n < erosions; n++) {
    for (ii = BOARDMIN; ii < BOARDMAX; ii++) {
      if (!ON_BOARD(ii))
	continue;

      if (work[ii] > 0) {
	if (ON_BOARD(SOUTH(ii)) && gb[SOUTH(ii)] <= 0)
	  work[ii]--;
	if (ON_BOARD(WEST(ii))  && gb[WEST(ii)]  <= 0 && work[ii] > 0)
	  work[ii]--;
	if (ON_BOARD(NORTH(ii)) && gb[NORTH(ii)] <= 0 && work[ii] > 0)
	  work[ii]--;
	if (ON_BOARD(EAST(ii))  && gb[EAST(ii)]  <= 0 && work[ii] > 0)
	  work[ii]--;
      }
      
      if (work[ii] < 0) {
	if (ON_BOARD(SOUTH(ii)) && gb[SOUTH(ii)] >= 0)
	  work[ii]++;
	if (ON_BOARD(WEST(ii))  && gb[WEST(ii)]  >= 0 && work[ii] < 0)
	  work[ii]++;
	if (ON_BOARD(NORTH(ii)) && gb[NORTH(ii)] >= 0 && work[ii] < 0)
	  work[ii]++;
	if (ON_BOARD(EAST(ii))  && gb[EAST(ii)]  >= 0 && work[ii] < 0)
	  work[ii]++;
      }
    }
    memcpy(gb, work, sizeof(work));
  }
  
  return critical_found;
}


/* For scoring, it is assumed that any region completely
 * surrounded by territory is territory. This assumption
 * is particularly valid at the end of the game when
 * genmove produces a PASS. This function identifies
 * regions completely surrounded by territory and sets
 * them equal to 1 or -1 for WHITE or BLACK.
 * 
 * We define an *bubble* to be a region of the complement
 * of the region marked in gb. The border color of the
 * bubble may be BLACK, WHITE or GRAY depending on whether
 * it is only adjacent to BLACK or WHITE marked regions,
 * or to both.
 */

static void
close_bubbles(int gb[BOARDMAX])
{
  int bubbles[BOARDMAX];
  int ii;
  int found_one = 1;

  memset(bubbles, 0, sizeof(bubbles));
  while (found_one) {
    found_one = 0;
    for (ii = BOARDMIN; ii < BOARDMAX; ii++) {
      int white_neighbor = 0;
      int black_neighbor = 0;
      int new_color = 0;
      
      if (!ON_BOARD(ii) || gb[ii] || bubbles[ii] == GRAY)
	continue;
	
      /* If any neighbor is gray, mark the spot gray */
      if ((ON_BOARD(SOUTH(ii)) && bubbles[SOUTH(ii)] == GRAY)
	  || (ON_BOARD(WEST(ii)) && bubbles[WEST(ii)] == GRAY)
	  || (ON_BOARD(NORTH(ii)) && bubbles[NORTH(ii)] == GRAY)
	  || (ON_BOARD(EAST(ii)) && bubbles[EAST(ii)] == GRAY)) {
	found_one = 1;
	bubbles[ii] = GRAY;
      }
      else {
	/* Look for white neighbors, including the spot itself */
	if (bubbles[ii] == WHITE
	    || (ON_BOARD(SOUTH(ii))
		&& (gb[SOUTH(ii)] > 0 || bubbles[SOUTH(ii)] == WHITE))
	    || (ON_BOARD(WEST(ii))
		&& (gb[WEST(ii)] > 0 || bubbles[WEST(ii)] == WHITE))
	    || (ON_BOARD(NORTH(ii))
		&& (gb[NORTH(ii)] > 0 || bubbles[NORTH(ii)] == WHITE))
	    || (ON_BOARD(EAST(ii))
		&& (gb[EAST(ii)] > 0 || bubbles[EAST(ii)] == WHITE)))
	  white_neighbor = 1;
	
	if (bubbles[ii] == BLACK
	    || (ON_BOARD(SOUTH(ii))
		&& (gb[SOUTH(ii)] < 0 || bubbles[SOUTH(ii)] == BLACK))
	    || (ON_BOARD(WEST(ii))
		&& (gb[WEST(ii)] < 0 || bubbles[WEST(ii)] == BLACK))
	    || (ON_BOARD(NORTH(ii))
		&& (gb[NORTH(ii)] < 0 || bubbles[NORTH(ii)] == BLACK))
	    || (ON_BOARD(EAST(ii))
		&& (gb[EAST(ii)] < 0 || bubbles[EAST(ii)] == BLACK)))
	  black_neighbor = 1;
	
	if (white_neighbor) {
	  if (black_neighbor)
	    new_color = GRAY;
	  else
	    new_color = WHITE;
	}
	else if (black_neighbor)
	  new_color = BLACK;
	
	if (new_color && new_color != bubbles[ii]) {
	  found_one = 1;
	  bubbles[ii] = new_color;
	}
      }
    }
  }
  /* The bubbles are found and classified. Now adjoin them. */

  for (ii = BOARDMIN; ii < BOARDMAX; ii++) {
    if (!ON_BOARD(ii) || gb[ii])
      continue;
    
    if (bubbles[ii] == WHITE)
      gb[ii] = 1;
    
    if (bubbles[ii] == BLACK)
      gb[ii] = -1;
  }
}


/* Generic function to print the regions in gb, rendering
 * positive as white territory, negative as black. Stones
 * with status == DEAD are invisible and appear as
 * territory.
 */

static void
print_regions(int gb[BOARDMAX])
{
  int i, j, k;
  int ii;

  start_draw_board();
  for (i = 0; i < board_size; i++) {
    for (j = 0; j < board_size; j++) {
      ii = POS(i, j);

      if (board[ii] && dragon[ii].status != DEAD)
	k = board[ii];
      else
	k = EMPTY;

      switch (k) {
      case EMPTY:
	if (gb[ii] > 0)
	  draw_color_char(i, j, 'w', GG_COLOR_GREEN);
	else if (gb[ii] < 0)
	  draw_color_char(i, j, 'b', GG_COLOR_MAGENTA);
	else
	  draw_color_char(i, j, '.', GG_COLOR_BLACK);
	break;

      case BLACK:
	if (dragon[ii].status == CRITICAL)
	  draw_color_char(i, j, 'X', GG_COLOR_RED);
	else
	  draw_color_char(i, j, 'X', GG_COLOR_BLACK);
	break;

      case WHITE:
	if (dragon[ii].status == CRITICAL)
	  draw_color_char(i, j, 'O', GG_COLOR_RED);
	else
	  draw_color_char(i, j, 'O', GG_COLOR_BLACK);
	break;
      }
    }
  }
  end_draw_board();
}


/* Print the moyo regions after a specified number
 * of dilations and erosions.
 */

void
print_moyo(void)
{
  if (printmoyo & PRINTMOYO_TERRITORY)
    print_new_moyo(5, 21);
  
  if (printmoyo & PRINTMOYO_MOYO)
    print_new_moyo(5, 10);
  
  if (printmoyo & PRINTMOYO_AREA) {
    print_new_moyo(4, 0);
  }
}


static void
print_new_moyo(int dilations, int erosions)
{
  int gb[BOARDMAX];

  dilate_erode(dilations, erosions, gb, WHITE);
  close_bubbles(gb);
  print_regions(gb);
}


/* Put upper and lower score estimates into *upper, *lower and
 * return the average. A positive score favors white. In computing
 * the upper bound, CRITICAL dragons are awarded to white; in
 * computing the lower bound, they are awarded to black.
 */

float
estimate_score(float *upper, float *lower)
{
  int gb[BOARDMAX];

  float white_territory = 0.0;
  float black_territory = 0.0;
  float white_area = 0.0;
  float black_area = 0.0;
  int i, j;
  int ii;
  float u, l;
  int critical;

  critical = dilate_erode(5, 21, gb, WHITE);
  close_bubbles(gb);
  if (debug & DEBUG_SCORING)
    print_regions(gb);

  for (i = 0; i < board_size; i++) {
    int black_territory_in_row = 0;
    int white_territory_in_row = 0;
    int white_area_in_row = 0;
    int black_area_in_row = 0;
    for (j = 0; j < board_size; j++) {
      ii = POS(i, j);
      
      if (board[ii] == BLACK) {
	if (captured_territory(ii, WHITE)) {
	  white_territory += 2;
	  white_area++;
	  white_territory_in_row += 2;
	  white_area_in_row++;
	}
	else {
	  black_area++;
	  black_area_in_row++;
	}
      }
      else if (board[ii] == WHITE) {
	if (captured_territory(ii, WHITE)) {
	  black_territory += 2;
	  black_area++;
	  black_territory_in_row += 2;
	  black_area_in_row++;
	} 
	else {
	  white_area++;
	  white_area_in_row++;
	}
      }
      else {
	if (gb[ii] > 0.0) {
	  white_territory++;
	  white_area++;
	  white_territory_in_row++;
	  white_area_in_row++;
	}
	else if (gb[ii] < 0.0) {
	  black_territory++;
	  black_area++;
	  black_territory_in_row++;
	  black_area_in_row++;
	}
      }
    }
    if (chinese_rules) {
      TRACE_SCORING(
	    "in row %d, white area=%d, black=%d\n",
	  board_size - i, white_area_in_row, black_area_in_row);
    }
    else {
      TRACE_SCORING(
	    "in row %d, white territory=%d, black=%d\n",
	    board_size - i, white_territory_in_row, black_territory_in_row);
    }
  }

  if (chinese_rules)
    u = white_area - black_area + komi;
  else {
    TRACE_SCORING(
	  "black captured: %d\nwhite captured: %d\nkomi: %f\n",
	  black_captured, white_captured, komi);
    u = white_territory 
      + black_captured - black_territory - white_captured + komi;
  }
  if (critical) {
    white_territory = 0.0;
    black_territory = 0.0;
    white_area = 0.0;
    black_area = 0.0;
    
    dilate_erode(5, 21, gb, BLACK);
    close_bubbles(gb);
    for (i = 0; i < board_size; i++) {
      int black_territory_in_row = 0;
      int white_territory_in_row = 0;
      int white_area_in_row = 0;
      int black_area_in_row = 0;
      for (j = 0; j < board_size; j++) {
	ii = POS(i, j);

	if (board[ii] == BLACK) {
	  if (captured_territory(ii, BLACK)) {
	    white_territory += 2;
	    white_territory_in_row += 2;
	    white_area++;
	    white_area_in_row++;
	  }
	  else {
	    black_area++;
	    black_area_in_row++;
	  }
	}
	else if (board[ii] == WHITE) {
	  if (captured_territory(ii, BLACK)) {
	    black_territory += 2;
	    black_area++;
	    black_territory_in_row += 2;
	    black_area++;
	  }
	}
	else {
	  if (gb[ii] > 0.0) {
	    white_territory++;
	    white_area++;
	    white_territory_in_row++;
	    white_area++;
	  }
	  else if (gb[ii] < 0.0) {
	    black_territory++;
	    black_area++;
	    black_territory_in_row++;
	    black_area_in_row++;
	  }
	}
      }
      if (chinese_rules) {
	TRACE_SCORING("in row %d, white area=%d, black=%d\n",
	      board_size-i, white_area_in_row, black_area_in_row);
      }
      else {
	TRACE_SCORING(
	      "in row %d, white territory=%d, black=%d\n",
	      board_size-i, white_territory_in_row, black_territory_in_row);
      }
    }

    if (chinese_rules)
      l = white_area - black_area + komi;
    else {
      TRACE_SCORING(
	  "black captured: %d\nwhite captured: %d\nkomi: %f\n",
	    black_captured, white_captured, komi);
      l = white_territory 
	+ black_captured - black_territory - white_captured + komi;
    }
  }
  else
    l = u;

  if (upper)
    *upper = u;
  if (lower)
    *lower = l;

  return (u + l) / 2;
}


/* We do not count dead stones inside the eyespace as territory. Such a
 * stone is characterized as having status DEAD yet having only
 * DEAD dragons as neighbors.
 *
 * Thus in this situation:
 * 
 * XXXXXXX  We do not count the X stone in the center as
 * XOOOOOX  territory for O since the surrounding O dragon is
 * XO.X.OX  dead.
 * XOOOOOX
 * XXXXXXX
 * 
 * If (pos) is the location of a stone which is DEAD and which is
 * not an exception of this type then it is safe to count it as
 * two points territory for the opponent. This function tests for
 * this condition.
 *
 * CRITICAL dragons are awarded to 'color'.
 */

static int
captured_territory(int pos, int color)
{
  int d;

  if (board[pos] == EMPTY 
      || dragon[pos].status == ALIVE
      || dragon[pos].status == UNKNOWN
      || (board[pos] == color && dragon[pos].status == CRITICAL))
    return 0;

  if (DRAGON2(pos).neighbors == 0
      && dragon[pos].status == DEAD)
    return 1;

  for (d = 0; d < DRAGON2(pos).neighbors; d++)
    if (DRAGON(DRAGON2(pos).adjacent[d]).color == OTHER_COLOR(board[pos])
	&& (DRAGON(DRAGON2(pos).adjacent[d]).status == ALIVE
	|| (board[pos] != color
	    && DRAGON(DRAGON2(pos).adjacent[d]).status == CRITICAL)))
      return 1;

  return 0;
}



/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */

