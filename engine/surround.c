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
#include <string.h>
#include <math.h>

#include "liberty.h"

/* Returns true if a dragon is enclosed within the convex hull of
 * its hostile neighbor dragons. This is an indication that the dragon is
 * in danger. Stones on the second and first lines are not tested.
 *
 * Normally NULL will be passed to the parameter apos. It can be
 * an empty board location. If apos is non NULL it is marked and
 * added to the the hull. Thus we can ask if adding a single stone
 * to the board surrounds the dragon.
 *
 * A CORNER is a vertex of the polygon which comprises this convex
 * hull. The algorithm proceeds by first finding the sequence of
 * corners on the left side of the polyhedron, then the sequence
 * of corners on the right side.
 *
 * The hull is marked in the array mn with the number 1.  A slight
 * expansion is marked with the number 2. Return code is SURROUNDED if
 * the friendly dragon lies within the area marked 1,
 * WEAKLY_SURROUNDED if it lies in the slightly larger area marked 1
 * and 2, and 0 otherwise.
 *
 * The notion of weak surroundedness seems to be much less indicative
 * of a dragon's immanent danger than surroundedness.
 * 
 * An exception: if the larger area contains any stone of a different
 * friendly dragon (which is not DEAD) the return code is 0, unless
 * that allied dragon is ENTIRELY contained within the hull.
 *
 * Another exception: an ikken tobi (one space jump) is generally not
 * a connection but in practice may be almost as good. If there is an
 * ikken tobi out of the hull, then the dragon is not surrounded.
 *
 * If the parameter showboard is 1, the figure is drawn. If showboard
 * is 2, the figure is only drawn if the region is surrounded.  
 *
 * If (apos) is NULL, the result is saved in the surround_data cache. 
 * The assumption is that the function will only be called once
 * with (apos) null, during make_dragons; thereafter the surroundedness
 * will be accessed using the function is_surrounded().
 *
 * If not *surround_size is not a NULL pointer, then surround_size
 * returns the 
 */

int
compute_surroundings(int pos, int apos, int showboard, int *surround_size)
{
  int i, j;
  int m, n;
  int k;
  int dpos;
  int surrounded;
  
  int left_corner[MAX_BOARD];
  int right_corner[MAX_BOARD];
  int left_corners = 0, right_corners = 0;
  int top_row, bottom_row;
  int color = board[pos];
  int other = OTHER_COLOR(color);
  
  char mf[BOARDMAX]; /* friendly dragon  */
  char mn[BOARDMAX]; /* neighbor dragons */
  
  memset(mf, 0, sizeof(mf));
  memset(mn, 0, sizeof(mn));
  
  /* mark dragon */
  for (dpos = BOARDMIN; dpos < BOARDMAX; dpos++)
    if (ON_BOARD(dpos)
	&& worm[dpos].origin == dpos
	&& is_same_dragon(dpos, pos))
      mark_string(dpos, mf, 1);
  
  if (DRAGON2(pos).hostile_neighbors == 0)
    return(0);
  
  /* mark hostile neighbors */
  for (k = 0; k < DRAGON2(pos).neighbors; k++) {
    int nd = DRAGON(DRAGON2(pos).adjacent[k]).origin;
    
    if (board[nd] != color) {
      if (0)
	gprintf("neighbor: %1m\n", nd);
      for (dpos = BOARDMIN; dpos < BOARDMAX; dpos++)
	if (ON_BOARD(dpos)
	    && worm[dpos].origin == dpos
	    && is_same_dragon(dpos, nd))
	  mark_string(dpos, mn, 1);
    }
  }

  /* if apos is non NULL, mark it. */

  if (apos) {
    gg_assert(ON_BOARD(apos));
    mn[apos] =1;
  }
  
  /* find top row of surrounding polyhedron */
  
  top_row = -1;
  for (m = 0; m < board_size; m++) {
    if (top_row != -1)
      break;
    for (n = 0; n < board_size; n++)
      if (mn[POS(m, n)]) {
	left_corner[0] = POS(m, n);
	top_row = m;
	break;
      }
  }
  /* find bottom row */
  
  bottom_row = -1;
  for (m = board_size - 1; m >= 0; m--) {
    if (bottom_row != -1)
      break;
    for (n = 0; n < board_size; n++)
      if (mn[POS(m, n)]) {
	bottom_row = m;
	break;
      }
  }
  
  /* find the corners on the left side */
  
  for (left_corners = 1; I(left_corner[left_corners-1]) < bottom_row; 
       left_corners++) {
    int best_found = 0;
    float best_slope = 0.;
    int m = I(left_corner[left_corners-1]);
    int n = J(left_corner[left_corners-1]);
    
    for (i = m + 1; i <= bottom_row; i++)
      for (j = 0; j < board_size; j++)
	if (mn[POS(i, j)]) {
	  float slope = ((float) (j - n))/((float) (i - m));
	  if (0)
	    gprintf("(left) at %m, last %m, slope=%f\n", i, j, m, n, slope);
	  
	  if (!best_found || slope < best_slope) {
	    best_found = POS(i, j);
	    best_slope = slope;
	  }
	}
    gg_assert(ON_BOARD(best_found));
    left_corner[left_corners] = best_found;
  }
  
  for (n = board_size-1; n >= 0; n--)
    if (mn[POS(top_row, n)]) {
      right_corner[0] = POS(top_row, n);
      break;
    }
  
  /* find the corners on the left side */
  
  for (right_corners = 1; I(right_corner[right_corners-1]) < bottom_row; 
       right_corners++) {
    int best_found = 0;
    float best_slope = 0.;
    int m = I(right_corner[right_corners-1]);
    int n = J(right_corner[right_corners-1]);
    
    for (i = m + 1; i <= bottom_row; i++) {
      for (j = board_size - 1; j >= 0; j--) {
	if (mn[POS(i, j)]) {
	  float slope = ((float) (j - n))/((float) (i - m));
	  if (0)
	    gprintf("(right) at %m, last %m, slope=%f\n", i, j, m, n, slope);
	  if (!best_found || slope > best_slope) {
	    best_found = POS(i, j);
	    best_slope = slope;
	  }
	}
      }
    }
    gg_assert(ON_BOARD(best_found));
    right_corner[right_corners] = best_found;
  }
  
  if (0) {
    for (k = 0; k < left_corners; k++)
      gprintf("left corner %d: %1m\n", k, left_corner[k]);
    
    for (k = 0; k < right_corners; k++)
      gprintf("right corner %d: %1m\n", k, right_corner[k]);
  }

  /* Now mark the interior of the convex hull */
  
  for (n = J(left_corner[0]); n <= J(right_corner[0]); n++)
    mn[POS(top_row, n)] = 1;
  for (n = J(left_corner[left_corners-1]); 
       n <= J(right_corner[right_corners-1]); n++)
    mn[POS(bottom_row, n)] = 1;
  for (m = top_row+1; m < bottom_row; m++) {
    int left_boundary = -1, right_boundary = -1;
    for (k = 1; k < left_corners; k++) {
      if (I(left_corner[k]) > m) {
	float ti = I(left_corner[k-1]);
	float tj = J(left_corner[k-1]);
	float bi = I(left_corner[k]);
	float bj = J(left_corner[k]);
	
	if (0)
	  gprintf("(left) %d: %1m %1m\n", 
		  m, left_corner[k-1], left_corner[k]);
	/* left edge in this row is on segment (ti,tj) -> (bi, bj) */
	
	/* FIXME: Rewrite this to avoid floating point arithmetic */
	left_boundary = ceil(-0.001 + tj + (m - ti) * (bj - tj) / (bi - ti));
	break;
      }
    }
    for (k = 1; k < right_corners; k++) {
      if (I(right_corner[k]) > m) {
	float ti = I(right_corner[k-1]);
	float tj = J(right_corner[k-1]);
	float bi = I(right_corner[k]);
	float bj = J(right_corner[k]);
	
	if (0)
	  gprintf("(right) %d: %1m %1m\n", 
		  m, right_corner[k-1], right_corner[k]);

	/* FIXME: Rewrite this to avoid floating point arithmetic */
	right_boundary = floor(0.001 + tj + (m - ti) * (bj - tj) / (bi - ti));
	break;
      }
    }
    for (n = left_boundary; n <= right_boundary; n++)
      mn[POS(m, n)] = 1;
  }
  
  /* mark the expanded region */

  for (dpos = BOARDMIN; dpos < BOARDMAX; dpos++)
    if (ON_BOARD(dpos) && mn[dpos] == 1)
      for (k = 0; k < 4; k++)
	if (ON_BOARD(dpos + delta[k]) && !mn[dpos + delta[k]])
	  mn[dpos + delta[k]] = 2;
      
  /* Mark allied dragons that intersect the (unexpanded) hull.
   * These must all lie entirely within the hull for the
   * dragon to be considered surrounded. 
   *
   * Only neighbor dragons are considered since dragons that
   * are not neighbors are less likely to be helpful.
   */

  for (dpos = BOARDMIN; dpos < BOARDMAX; dpos++)
    if (ON_BOARD(dpos) 
	&& mn[dpos] == 1
	&& board[dpos] == color
	&& are_neighbor_dragons(pos, dpos)
	&& !mf[dpos]) {
      int mpos;

      for (mpos = BOARDMIN; mpos < BOARDMAX; mpos++)
	if (ON_BOARD(mpos) && is_same_dragon(mpos, dpos))
	  mf[mpos] = 2;
    }

  /* determine the surround status of the dragon */

  surrounded = SURROUNDED;

  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      if (mf[POS(m, n)]) {
	if (mn[POS(m, n)] == 0) {
	  surrounded = 0;
	  break;
	}
	else if (mn[POS(m, n)] == 2)
	  surrounded = WEAKLY_SURROUNDED;
      }
    }
      
  /* revise the status if an ikken tobi jumps out. */

  if (surrounded) {
    for (dpos = BOARDMIN; dpos < BOARDMAX && surrounded; dpos++)
      if (ON_BOARD(dpos) && mf[dpos]) {
	if ((ON_BOARD(NORTH(dpos)) 
	     && board[NORTH(dpos)] == EMPTY
	     && ON_BOARD(NORTH(NORTH(dpos)))
	     && board[NORTH(NORTH(dpos))] == color
	     && mn[NORTH(NORTH(dpos))] != 1
	     && ON_BOARD(EAST(NORTH(dpos)))
	     && board[EAST(NORTH(dpos))] != other
	     && ON_BOARD(WEST(NORTH(dpos)))
	     && board[WEST(NORTH(dpos))] != other)
	    || (ON_BOARD(SOUTH(dpos))
		&& board[SOUTH(dpos)] == EMPTY
		&& ON_BOARD(SOUTH(SOUTH(dpos)))
		&& board[SOUTH(SOUTH(dpos))] == color
		&& mn[SOUTH(SOUTH(dpos))] != 1
		&& ON_BOARD(EAST(SOUTH(dpos)))
		&& board[EAST(SOUTH(dpos))] != other
		&& ON_BOARD(WEST(SOUTH(dpos)))
		&& board[WEST(SOUTH(dpos))] != other)
	    || (ON_BOARD(EAST(dpos)) 
		&& board[EAST(dpos)] == EMPTY
		&& ON_BOARD(EAST(EAST(dpos)))
		&& board[EAST(EAST(dpos))] == color
		&& mn[EAST(EAST(dpos))] != 1
		&& ON_BOARD(NORTH(EAST(dpos)))
		&& board[NORTH(EAST(dpos))] != other
		&& ON_BOARD(SOUTH(EAST(dpos)))
		&& board[SOUTH(EAST(dpos))] != other)
	    || (ON_BOARD(WEST(dpos)) 
		&& board[WEST(dpos)] == EMPTY
		&& ON_BOARD(WEST(WEST(dpos)))
		&& board[WEST(WEST(dpos))] == color
		&& mn[WEST(WEST(dpos))] != 1
		&& ON_BOARD(NORTH(WEST(dpos)))
		&& board[NORTH(WEST(dpos))] != other
		&& ON_BOARD(SOUTH(WEST(dpos)))
		&& board[SOUTH(WEST(dpos))] != other))
	  surrounded = 0;
      }
  }
  if (showboard == 1 || (showboard == 2 && surrounded)) {
    start_draw_board();
    for (m = 0; m < board_size; m++)
      for (n = 0; n < board_size; n++) {
	int col, c;
	
	if (mf[POS(m,n)]) {
	  if (mn[POS(m,n)] ==1 )
	    col = GG_COLOR_RED;
	  else if (mn[POS(m,n)] == 2)
	    col = GG_COLOR_YELLOW;
	  else
	    col = GG_COLOR_GREEN;
	}
	else if (mn[POS(m,n)] == 1)
	  col = GG_COLOR_BLUE;
	else if (mn[POS(m,n)] == 2)
	  col = GG_COLOR_CYAN;
	else
	  col = GG_COLOR_BLACK;
	if (board[POS(m, n)] == BLACK)
	  c = 'X';
	else if (board[POS(m, n)] == WHITE)
	  c = 'O';
	else if (mn[POS(m, n)])
	  c = '*';
	else
	  c = '.';
	draw_color_char(m, n, c, col);
      }
    end_draw_board();
  }
  if (!apos && surrounded && surround_pointer < MAX_SURROUND) {
    memcpy(surroundings[surround_pointer].surround_map, mn, sizeof(mn));
    surroundings[surround_pointer].dragon_number = dragon[pos].id;
    surround_pointer++;
  }
  if (surround_size) {
    int pos;

    *surround_size = 0;
    for (pos = BOARDMIN; pos < BOARDMAX; pos++)
      if (ON_BOARD(pos) && mn[pos] == 1)
	(*surround_size)++;
  }
  return surrounded;
}

int
is_surrounded(int dr)
{
  return(DRAGON2(dr).surround_status);
}

/* Returns true if (dragon) is not surrounded, but (move) surrounds it.
 */

int
does_surround(int move, int dr)
{
  if (DRAGON2(dr).surround_status)
    return 0;
  return compute_surroundings(dr, move, 0, NULL);
}


/* Should be run once per genmove, before make_dragons. */

void
reset_surround_data(void)
{
  surround_pointer = 0;
}


/* Returns 1 (respectively 2) if pos is in the convex hull
 * (respectively expanded hull boundary) of the surrounding
 * dragons. Returns -1 if the dragon is not found.
 */
int
surround_map(int dr, int pos)
{
  int k;

  for (k = 0; k < surround_pointer; k++)
    if (surroundings[k].dragon_number == dragon[dr].id)
      return surroundings[k].surround_map[pos];
  return -1;
}


  


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
