/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * This is GNU Go, a Go program. Contact gnugo@gnu.org, or see       *
 * http://www.gnu.org/software/gnugo/ for more information.          *
 *                                                                   *
 * Copyright 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007,   *
 * 2008, 2009 and 2010 by the Free Software Foundation.              *
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
#include <string.h>
#include <math.h>

#include "liberty.h"
#include "gg_utils.h"

/* Forward declarations */
static int goal_dist(int pos, signed char goal[BOARDMAX]);
static int compare_angles(const void *a, const void *b);
static void show_surround_map(signed char mf[BOARDMAX],
			      signed char mn[BOARDMAX]);

/* Globals */
static int gg;      /* stores the gravity center of the goal */


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
 * If *surround_size is not a NULL pointer, then surround_size
 * returns the size of the surroundings.
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
  int corner[BOARDMAX];
  int left_corners = 0, right_corners = 0;
  int corners = 0;
  int top_row, bottom_row;
  int color = board[pos];
  int other = OTHER_COLOR(color);
  int gi = 0;
  int gj = 0;
  int stones = 0;
  int found_some;
  
  signed char mf[BOARDMAX]; /* friendly dragon  */
  signed char mn[BOARDMAX]; /* neighbor dragons */
  int  sd[BOARDMAX]; /* distances to the goal */
  
  if (DRAGON2(pos).hostile_neighbors == 0)
    return(0);
  
  memset(mf, 0, sizeof(mf));
  memset(mn, 0, sizeof(mn));
  memset(sd, 0, sizeof(sd));

  /* These should not be strictly necessary to clear but doing so
   * avoids compiler warnings about possibly using uninitialized
   * variables.
   */
  memset(left_corner, 0, sizeof(left_corner));
  memset(right_corner, 0, sizeof(right_corner));
  
  mark_dragon(pos, mf, 1);

  /* mark hostile neighbors */

  for (k = 0; k < DRAGON2(pos).neighbors; k++) {
    int nd = DRAGON(DRAGON2(pos).adjacent[k]).origin;
    
    if (board[nd] != color) {
      if (0)
	gprintf("neighbor: %1m\n", nd);
      mark_dragon(nd, mn, 1);
    }
  }

  /* descend markings from stones lying on the 2nd and third lines */

  for (dpos = BOARDMIN; dpos < BOARDMAX; dpos++)
    if (ON_BOARD(dpos) && mn[dpos]) {
      for (k = 0; k < 4; k++) {
        int d = delta[k];
        if (!ON_BOARD(dpos + d))
          continue;
        if (!ON_BOARD(dpos + 2*d)) {
          if (board[dpos + d] == EMPTY)
            mn[dpos + d] = 1;
        }
        else if (!ON_BOARD(dpos + 3*d)) {
          if (board[dpos + d] == EMPTY
              && board[dpos + 2*d] == EMPTY)
            mn[dpos + 2*d] = 1;
        }
      }
    }

  /* compute minimum distances to the goal */

  for (dpos = BOARDMIN; dpos < BOARDMAX; dpos++)
    if (ON_BOARD(dpos) && mn[dpos]) 
      sd[dpos] = goal_dist(dpos, mf);

  /* revise markings */

  do {
    found_some = 0;
    for (dpos = BOARDMIN; dpos < BOARDMAX; dpos++)
      if (ON_BOARD(dpos) && mn[dpos] && sd[dpos] > 8) {
        /* discard markings if we can find 2 stones
         * that verify :
         * - it is closer to the goal than we are
         * - it is closer to us than the goal is
         * - they are closer to each other than we are to the goal
         */
        for (i = BOARDMIN; i < BOARDMAX; i++)
	  if (ON_BOARD(i) && mn[i] && i != dpos
              && sd[i] < sd[dpos]
              && square_dist(i, dpos) < sd[dpos]) {
            for (j = i + 1; j < BOARDMAX; j++)
	      if (ON_BOARD(j) && mn[j] && j != dpos
                  && sd[j] < sd[dpos]
                  && square_dist(j, dpos) < sd[dpos]
                  && square_dist(i, j) < sd[dpos]) {
	        mn[dpos] = 0;
                found_some = 1;
                break;
              }
            if (mn[dpos] == 0)
              break;
          }
      }
  } while (found_some);

  /* prepare corner array */

  for (dpos = BOARDMIN; dpos < BOARDMAX; dpos++)
    if (ON_BOARD(dpos) && mn[dpos])
      corner[corners++] = dpos;

  /* compute gravity center of the goal */

  for (dpos = BOARDMIN; dpos < BOARDMAX; dpos++)
    if (ON_BOARD(dpos) && mf[dpos]) {
      gi += I(dpos);
      gj += J(dpos);
      stones++;
    }
  gi /= stones;
  gj /= stones;
  gg = POS(gi, gj);

  /* sort the corner array */

  gg_sort(corner, corners, sizeof(int), compare_angles);

  /* if apos is not NO_MOVE, mark it. */

  if (apos != NO_MOVE) {
    ASSERT_ON_BOARD1(apos);
    mn[apos] = 1;
  }
  
  if (showboard == 1) {
    show_surround_map(mf, mn);
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
    ASSERT_ON_BOARD1(best_found);
    left_corner[left_corners] = best_found;
  }
  
  for (n = board_size-1; n >= 0; n--)
    if (mn[POS(top_row, n)]) {
      right_corner[0] = POS(top_row, n);
      break;
    }
  
  /* find the corners on the right side */
  
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
    ASSERT_ON_BOARD1(best_found);
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
	left_boundary = ceil(tj + (m - ti) * (bj - tj) / (bi - ti));
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
	right_boundary = floor(tj + (m - ti) * (bj - tj) / (bi - ti));
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

  for (dpos = BOARDMIN; dpos < BOARDMAX; dpos++) {
    int mpos;
    if (ON_BOARD(dpos) 
	&& mn[dpos] == 1
	&& board[dpos] == color
	&& are_neighbor_dragons(pos, dpos)
	&& !mf[dpos]) {

      for (mpos = BOARDMIN; mpos < BOARDMAX; mpos++)
	if (ON_BOARD(mpos) && is_same_dragon(mpos, dpos))
	  mf[mpos] = 2;
    }
    /* A special case
     *
     *  . X X .
     *  X O . X
     *  X . O O
     *  . O . .
     *
     * The O stone hasn't been amalgamated and the surround computations
     * might think this single stone dragon is surrounded, which in turn
     * can generate overvaluation of moves around this stone.
     * Consequently, we allow inclusion of the stones at kosumi distance
     * in the mf (friendly) array.
     */
    if (ON_BOARD(dpos) 
	&& mn[dpos] == 2
	&& board[dpos] == color
	&& are_neighbor_dragons(pos, dpos)
	&& !mf[dpos]) {
      for (k = 4; k < 8; k++)
	if (ON_BOARD(dpos + delta[k]) && board[dpos + delta[k]] == color
	    && mn[dpos + delta[k]] == 1
	    && board[dpos + delta[k-4]] == EMPTY
	    && board[dpos + delta[(k-3)%4]] == EMPTY) {
	  for (mpos = BOARDMIN; mpos < BOARDMAX; mpos++)
	    if (ON_BOARD(mpos) && is_same_dragon(mpos, dpos))
	      mf[mpos] = 2;
	}
    }
  }

  /* determine the surround status of the dragon */

  surrounded = SURROUNDED;

  /* Compute the maximum surround status awarded
   * If distances between enclosing stones are large, reduce to
   * WEAKLY_SURROUNDED. If (really) too large, then reduce to 0
   * FIXME: constants chosen completely ad hoc. Possibly better tunings
   *        can be found.
   */

  for (k = 0; k < corners - 1; k++) {
    if (is_edge_vertex(corner[k])
        && is_edge_vertex(corner[k+1]))
      continue;
    if (square_dist(corner[k], corner[k+1]) > 60) {
      surrounded = 0;
      break;
    }
    else if (square_dist(corner[k], corner[k+1]) > 27)
      surrounded = WEAKLY_SURROUNDED;
  }
  if (surrounded
      && (!is_edge_vertex(corner[0])
          || !is_edge_vertex(corner[corners-1]))) {
    if (square_dist(corner[0], corner[corners-1]) > 60)
      surrounded = 0;
    else if (square_dist(corner[0], corner[corners-1]) > 27)
      surrounded = WEAKLY_SURROUNDED;
  }

  if (surrounded)
    for (dpos = BOARDMIN; dpos < BOARDMAX; dpos++)
      if (mf[dpos]) {
	if (mn[dpos] == 0) {
	  surrounded = 0;
	  break;
	}
	else if (mn[dpos] == 2)
	  surrounded = WEAKLY_SURROUNDED;
      }

  /* revise the status for single stone dragons. */

  if (stones == 1
      && surrounded == WEAKLY_SURROUNDED
      && mn[pos] == 2)
    surrounded = 0;
      
  /* revise the status if an ikken tobi jumps out. */

  if (surrounded) {
    for (dpos = BOARDMIN; dpos < BOARDMAX && surrounded; dpos++) {
      if (!ON_BOARD(dpos) || !mf[dpos])
	continue;

      for (k = 0; k < 4; k++) {
	int up = delta[k];
	int right = delta[(k + 1) % 4];
	if (board[dpos + up] == EMPTY
	    && board[dpos + 2*up] == color
	    && mn[dpos + 2*up] != 1
	    && ON_BOARD(dpos + up + right)
	    && board[dpos + up + right] != other
	    && ON_BOARD(dpos + up - right)
	    && board[dpos + up - right] != other) {
	  surrounded = 0;
	  break;
	}
      }
    }
  }

  if (showboard == 1 || (showboard == 2 && surrounded)) {
    show_surround_map(mf, mn);
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


/* Computes the minimum distance to the goal
 */

static int
goal_dist(int pos, signed char goal[BOARDMAX])
{
  int dist = 10000;
  int ii;

  for (ii = BOARDMIN; ii < BOARDMAX; ii++)
    if (ON_BOARD(ii) && goal[ii])
      dist = gg_min(dist, square_dist(ii, pos));

  return dist;
}

/* Compares angles. Chosen convention:
 * - SOUTH is "lowest"
 * - ascending order is done clock-wise (WEST, NORTH, EAST)
 */
static int
compare_angles(const void *a, const void *b)
{
  int aa = *((const int *)a);
  int bb = *((const int *)b);

  int di_a = I(aa) - I(gg);
  int dj_a = J(aa) - J(gg);
  int di_b = I(bb) - I(gg);
  int dj_b = J(bb) - J(gg);

  float sin_a, sin_b;

  if (aa == gg)
    return 1;
  if (bb == gg)
    return -1;

  if (dj_a == 0) {
    if (di_a > 0) {
      if (dj_b != 0 || di_b <= 0)
        return -1;
      return 0;
    }
    else {
      if (dj_b > 0)
        return -1;
      else if (dj_b < 0 || di_b > 0)
        return 1;
      else
        return 0;
    }
  }

  sin_a = (float)di_a / sqrt(di_a*di_a + dj_a*dj_a);
  sin_b = (float)di_b / sqrt(di_b*di_b + dj_b*dj_b);

  if (dj_a > 0) {
    if (dj_b <= 0)
      return 1;
    if (sin_a > sin_b)
      return 1;
    else if (sin_a < sin_b)
      return -1;
    else
      return 0;
  }
  else { /* if (dj_a < 0) */
    if (dj_b > 0)
      return -1;
    if (sin_a < sin_b)
      return 1;
    else if (sin_a > sin_b)
      return -1;
    else
      return 0;
  }
}


static void
show_surround_map(signed char mf[BOARDMAX], signed char mn[BOARDMAX])
{
  int m, n;

  start_draw_board();
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      int col, c;
      
      if (mf[POS(m, n)]) {
	if (mn[POS(m, n)] == 1)
	  col = GG_COLOR_RED;
	else if (mn[POS(m, n)] == 2)
	  col = GG_COLOR_YELLOW;
	else
	  col = GG_COLOR_GREEN;
      }
      else if (mn[POS(m, n)] == 1)
	col = GG_COLOR_BLUE;
      else if (mn[POS(m, n)] == 2)
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
