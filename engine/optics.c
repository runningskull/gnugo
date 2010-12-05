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
#include <stdlib.h>
#include <string.h>
#include "liberty.h"
#include "eyes.h"
#include "gg_utils.h"

#define MAXEYE 20


/* This structure is used in communication between read_eye() and
 * recognize_eye().
 */
struct vital_points {
  int attacks[4 * MAXEYE];
  int defenses[4 * MAXEYE];
  int num_attacks;
  int num_defenses;
};


static void
compute_primary_domains(int color, int domain[BOARDMAX],
			int lively[BOARDMAX],
			int false_margins[BOARDMAX],
			int first_time);
static void count_neighbours(struct eye_data eyedata[BOARDMAX]);
static int is_lively(int owl_call, int pos);
static int false_margin(int pos, int color, int lively[BOARDMAX]);
static void originate_eye(int origin, int pos,
			  int *esize, int *msize,
			  struct eye_data eye[BOARDMAX]);
static int read_eye(int pos, int *attack_point, int *defense_point,
		    struct eyevalue *value,
		    struct eye_data eye[BOARDMAX],
		    struct half_eye_data heye[BOARDMAX],
		    int eyefilling_points[BOARDMAX],
		    int add_moves);
static int recognize_eye(int pos, int *attack_point, int *defense_point,
			 struct eyevalue *value,
			 struct eye_data eye[BOARDMAX],
			 struct half_eye_data heye[BOARDMAX],
			 struct vital_points *vp,
			 int eyefilling_points[BOARDMAX]);
static void guess_eye_space(int pos, int effective_eyesize, int margins,
			    int bulk_score, struct eye_data eye[BOARDMAX],
			    struct eyevalue *value, int *pessimistic_min);
static struct eye_graph *optical_graph_matcher(int *vpos,
					       signed char *marginal,
					       signed char *edge,
					       signed char *neighbors,
					       signed char *stone,
					       int eye_size, int num_marginals,
					       int *map,
					       struct half_eye_data *heye);
static void reset_map(int size);
static void first_map(int *map_value);
static int next_map(int *q, int map[MAXEYE]);
static void print_eye(struct eye_data eye[BOARDMAX],
		      struct half_eye_data heye[BOARDMAX], int pos);
static void add_false_eye(int pos, struct eye_data eye[BOARDMAX], 
			  struct half_eye_data heye[BOARDMAX]);
static float topological_eye(int pos, int color,
			     struct eye_data my_eye[BOARDMAX],
			     struct half_eye_data heye[BOARDMAX]);
static float evaluate_diagonal_intersection(int m, int n, int color,
					    int *attack_point,
					    int *defense_point,
					    struct eye_data my_eye[BOARDMAX]);


/* These are used during the calculations of eye spaces. */
static int black_domain[BOARDMAX];
static int white_domain[BOARDMAX];

/* Used internally by mapping functions. */
static int map_size;
static signed char used_index[MAXEYE];


/*
 * make_domains() is called from make_dragons() and from
 * owl_determine_life(). It marks the black and white domains
 * (eyeshape regions) and collects some statistics about each one.
 */

void
make_domains(struct eye_data b_eye[BOARDMAX],
	     struct eye_data w_eye[BOARDMAX],
	     int owl_call)
{
  int k;
  int pos;
  int lively[BOARDMAX];
  int false_margins[BOARDMAX];
  
  memset(black_domain, 0, sizeof(black_domain));
  memset(white_domain, 0, sizeof(white_domain));
  memset(false_margins, 0, sizeof(false_margins));

  if (b_eye)
    memset(b_eye, 0, BOARDMAX * sizeof(b_eye[0]));
  if (w_eye)
    memset(w_eye, 0, BOARDMAX * sizeof(w_eye[0]));

  /* Initialize eye data and compute the lively array. */
  for (pos = BOARDMIN; pos < BOARDMAX; pos++)
    if (ON_BOARD(pos))
      lively[pos] = is_lively(owl_call, pos);

  /* Compute the domains of influence of each color. */
  compute_primary_domains(BLACK, black_domain, lively, false_margins, 1);
  compute_primary_domains(WHITE, white_domain, lively, false_margins, 0);

  /* Now we fill out the arrays b_eye and w_eye with data describing
   * each eye shape.
   */

  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (!ON_BOARD(pos))
      continue;
    
    if (board[pos] == EMPTY || !lively[pos]) {
      if (black_domain[pos] == 0 && white_domain[pos] == 0) {
	if (w_eye)
	  w_eye[pos].color = GRAY;
	if (b_eye)
	  b_eye[pos].color = GRAY;
      }
      else if (black_domain[pos] == 1 && white_domain[pos] == 0 && b_eye) {
	b_eye[pos].color = BLACK;
	for (k = 0; k < 4; k++) {
	  int apos = pos + delta[k];
	  if (ON_BOARD(apos) && white_domain[apos] && !black_domain[apos]) {
	    b_eye[pos].marginal = 1;
	    break;
	  }
	}
      }
      else if (black_domain[pos] == 0 && white_domain[pos] == 1 && w_eye) {
	w_eye[pos].color = WHITE;
	for (k = 0; k < 4; k++) {
	  int apos = pos + delta[k];
	  if (ON_BOARD(apos) && black_domain[apos] && !white_domain[apos]) {
	    w_eye[pos].marginal = 1;
	    break;
	  }
	}
      }
      else if (black_domain[pos] == 1 && white_domain[pos] == 1) {
	if (b_eye) {
	  for (k = 0; k < 4; k++) {
	    int apos = pos + delta[k];
	    if (ON_BOARD(apos) && black_domain[apos]
		&& !white_domain[apos]) {
	      b_eye[pos].marginal = 1;
	      b_eye[pos].color = BLACK;
	      break;
	    }
	  }
	  if (k == 4)
	    b_eye[pos].color = GRAY;
	}
	
	if (w_eye) {
	  for (k = 0; k < 4; k++) {
	    int apos = pos + delta[k];
	    if (ON_BOARD(apos) && white_domain[apos]
		&& !black_domain[apos]) {
	      w_eye[pos].marginal = 1;
	      w_eye[pos].color = WHITE;
	      break;
	    }
	  }
	  if (k == 4)
	    w_eye[pos].color = GRAY;
	}
      }
    }
  }
  
  /* The eye spaces are all found. Now we need to find the origins. */
  partition_eyespaces(b_eye, BLACK);
  partition_eyespaces(w_eye, WHITE);
}

/* Find connected eyespace components and compute relevant statistics. */
void
partition_eyespaces(struct eye_data eye[BOARDMAX], int color)
{
  int pos;

  if (!eye)
    return;

  for (pos = BOARDMIN; pos < BOARDMAX; pos++)
    if (ON_BOARD(pos))
      eye[pos].origin = NO_MOVE;

  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (!ON_BOARD(pos))
      continue;
    if (eye[pos].origin == NO_MOVE && eye[pos].color == color) {
      int esize = 0;
      int msize = 0;
      
      originate_eye(pos, pos, &esize, &msize, eye);
      eye[pos].esize = esize;
      eye[pos].msize = msize;
    }
  }

  /* Now we count the number of neighbors and marginal neighbors
   * of each vertex.
   */
  count_neighbours(eye);
}


/* Compute the domains of influence of each color, used in determining
 * eye shapes. NOTE: the term influence as used here is distinct from the
 * influence in influence.c.
 * 
 * For this algorithm the strings which are not lively are invisible. Ignoring
 * these, the algorithm assigns friendly influence to:
 *
 * (1) every vertex which is occupied by a (lively) friendly stone, 
 * (2) every empty vertex adjoining a (lively) friendly stone,
 * (3) every empty vertex for which two adjoining vertices (not
 *     on the first line) in the (usually 8) surrounding ones have friendly
 *     influence, with two CAVEATS explained below.
 *
 * Thus in the following diagram, e would be assigned friendly influence
 * if a and b have friendly influence, or a and d. It is not sufficent
 * for b and d to have friendly influence, because they are not adjoining.
 * 
 *        uabc
 *         def
 *         ghi
 * 
 * The constraint that the two adjoining vertices not lie on the first
 * line prevents influence from leaking under a stone on the third line.
 * 
 * The first CAVEAT alluded to above is that even if a and b have friendly
 * influence, this does not cause e to have friendly influence if there
 * is a lively opponent stone at d. This constraint prevents 
 * influence from leaking past knight's move extensions.
 *
 * The second CAVEAT is that even if a and b have friendly influence
 * this does not cause e to have influence if there are lively opponent
 * stones at u and at c. This prevents influence from leaking past
 * nikken tobis (two space jumps).  
 *
 * The corner vertices are handled slightly different.
 * 
 *    +---
 *    |ab
 *    |cd
 * 
 * We get friendly influence at a if we have friendly influence
 * at b or c and no lively unfriendly stone at b, c or d. 
 *
 */

#define sufficient_influence(pos, apos, bpos) \
  (ON_BOARD(bpos) && influence[bpos] > threshold[pos] - influence[apos])

static void
compute_primary_domains(int color, int domain[BOARDMAX],
			int lively[BOARDMAX],
			int false_margins[BOARDMAX],
			int first_time)
{
  int other = OTHER_COLOR(color);
  int i, j, k;
  int pos, pos2;
  int own, enemy;
  signed char threshold[BOARDMAX];
  signed char influence[BOARDMAX];
  int list[BOARDMAX];
  int size = 0, lastchange = 0;

  memset(threshold, 0, sizeof(threshold));
  memset(influence, 0, sizeof(influence));
  
  /* In the first pass we
   * 1. Give influence to lively own stones and their neighbors.
   *    (Cases (1) and (2) above.)
   * 2. Fill influence[] and threshold[] arrays with initial values.
   */
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (!ON_BOARD(pos))
      continue;
   
    if (lively[pos]) {
      if (board[pos] == color) {
        domain[pos] = 1; /* Case (1) above. */
        influence[pos] = 1;
      }
      else
        influence[pos] = -1;
      continue;
    }
    
    own = enemy = 0;
    for (k = 0; k < 4; k++) {
      pos2 = pos + delta[k];
      if (ON_BOARD(pos2) && lively[pos2]) {
        if (board[pos2] == color)
          own = 1;
        else
          enemy = 1;
      }
    }
    
    if (own) {
      /* To explain the asymmetry between the first time around
       * this loop and subsequent ones, a false margin is adjacent
       * to both B and W lively stones, so it's found on the first
       * pass through the loop.
       */
      if (first_time) {
        if (board[pos] == EMPTY && (false_margin(pos, color, lively)
				    || false_margin(pos, other, lively)))
          false_margins[pos] = 1;
        else {
          domain[pos] = 1;
          influence[pos] = 1;
        }
      }
      else if (board[pos] != EMPTY || !false_margins[pos]) {
        domain[pos] = 1;
        influence[pos] = 1;
      }
    }
    else
      list[size++] = pos;
    
    if (enemy) {
      threshold[pos] = 1;
      influence[pos]--;
    }
    else if (is_edge_vertex(pos))
      influence[pos]--;
  }
  
  /* Now we loop over the board until no more vertices can be added to
   * the domain through case (3) above.
   */
  if (size) {
    k = size;
    while (1) {
      if (!k)
        k = size;
      pos = list[--k];
   
      /* Case (3) above. */
      if (sufficient_influence(pos, SOUTH(pos), SE(pos))
          || sufficient_influence(pos, SOUTH(pos), SW(pos))
          || sufficient_influence(pos, EAST(pos), SE(pos))
          || sufficient_influence(pos, EAST(pos), NE(pos))
          || sufficient_influence(pos, WEST(pos), SW(pos))
          || sufficient_influence(pos, WEST(pos), NW(pos))
          || sufficient_influence(pos, NORTH(pos), NW(pos))
          || sufficient_influence(pos, NORTH(pos), NE(pos))) {
        domain[pos] = 1;
        influence[pos]++;
	
        if (!--size)
          break;
        if (k < size)
          list[k] = list[size];
        else
          k--;
        lastchange = k;
      }
      else if (k == lastchange)
        break; /* Looped the whole list and found nothing new */
    }
  }
  
  if (0 && (debug & DEBUG_EYES)) {
    start_draw_board();
    for (i = 0; i < board_size; i++)
      for (j = 0; j < board_size; j++) {
	draw_color_char(i, j, domain[POS(i, j)] ? '1' : '0', GG_COLOR_BLACK);
      }
    end_draw_board();
  }
}


static void
count_neighbours(struct eye_data eyedata[BOARDMAX])
{
  int pos;
  int k;

  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (!ON_BOARD(pos) || eyedata[pos].origin == NO_MOVE) 
      continue;

    eyedata[pos].esize = eyedata[eyedata[pos].origin].esize;
    eyedata[pos].msize = eyedata[eyedata[pos].origin].msize;
    eyedata[pos].neighbors = 0;
    eyedata[pos].marginal_neighbors = 0;
    
    for (k = 0; k < 4; k++) {
      int pos2 = pos + delta[k];
      if (ON_BOARD(pos2) && eyedata[pos2].origin == eyedata[pos].origin) {
	eyedata[pos].neighbors++;
	if (eyedata[pos2].marginal)
	  eyedata[pos].marginal_neighbors++;
      }
    }
  }
}


static int
is_lively(int owl_call, int pos)
{
  if (board[pos] == EMPTY)
    return 0;

  if (owl_call)
    return owl_lively(pos);
  else
    return (!worm[pos].inessential
	    && (worm[pos].attack_codes[0] == 0
		|| worm[pos].defense_codes[0] != 0));
}


/* In the following situation, we do not wish the vertex at 'a'
 * included in the O eye space:
 * 
 * OOOOXX
 * OXaX..
 * ------
 *
 * This eyespace should parse as (X), not (X!). Thus the vertex
 * should not be included in the eyespace if it is adjacent to
 * an X stone which is alive, yet X cannot play safely at a.
 * The function returns 1 if this situation is found at 
 * (pos) for color O.
 *
 * The condition above is true, curiously enough, also for the
 * following case:
 *   A group has two eyes, one of size 1 and one which is critical 1/2.
 *   It also has to have less than 4 external liberties, since the
 *   reading has to be able to capture the group tactically. In that 
 *   case, the eye of size one will be treated as a false marginal.
 * Thus we have to exclude this case, which is done by requiring (pos)
 * to be adjacent to both white and black stones. Since this test is
 * least expensive, we start with it.
 *
 * As a second optimization we require that one of the other colored
 * neighbors is not lively. This should cut down on the number of
 * calls to attack() and safe_move().
 */

static int
false_margin(int pos, int color, int lively[BOARDMAX])
{
  int other = OTHER_COLOR(color);
  int neighbors = 0;
  int k;
  int all_lively;
  int potential_false_margin;
  
  /* Require neighbors of both colors. */
  for (k = 0; k < 4; k++)
    if (ON_BOARD(pos + delta[k]))
	neighbors |= board[pos + delta[k]];	

  if (neighbors != (WHITE | BLACK))
    return 0;

  /* At least one opponent neighbor should be not lively. */
  all_lively = 1;
  for (k = 0; k < 4; k++)
    if (board[pos + delta[k]] == other && !lively[pos + delta[k]])
      all_lively = 0;

  if (all_lively)
    return 0;

  potential_false_margin = 0;
  for (k = 0; k < 4; k++) {
    int apos = pos + delta[k];
    if (board[apos] != other || !lively[apos])
      continue;
    
    if (stackp == 0 && worm[apos].attack_codes[0] == 0)
      potential_false_margin = 1;
    
    if (stackp > 0 && !attack(apos, NULL))
      potential_false_margin = 1;
  }
  
  if (potential_false_margin && safe_move(pos, other) == 0) {
    DEBUG(DEBUG_EYES, "False margin for %C at %1m.\n", color, pos);
    return 1;
  }

  return 0;
}


/*
 * originate_eye(pos, pos, *esize, *msize, eye) creates an eyeshape
 * with origin pos. esize and msize return the size and the number of
 * marginal vertices. The repeated variables (pos) are due to the
 * recursive definition of the function.
 */
static void
originate_eye(int origin, int pos,
	      int *esize, int *msize, 
	      struct eye_data eye[BOARDMAX])
{
  int k;
  ASSERT_ON_BOARD1(origin);
  ASSERT_ON_BOARD1(pos);
  gg_assert(esize != NULL);
  gg_assert(msize != NULL);
  
  eye[pos].origin = origin;
  (*esize)++;
  if (eye[pos].marginal)
    (*msize)++;

  for (k = 0; k < 4; k++) {
    int pos2 = pos + delta[k];
    if (ON_BOARD(pos2)
	&& eye[pos2].color == eye[pos].color
	&& eye[pos2].origin == NO_MOVE
	&& (!eye[pos2].marginal || !eye[pos].marginal))
      originate_eye(origin, pos2, esize, msize, eye);
  }
}


/* 
 * propagate_eye(origin) copies the data at the (origin) to the
 * rest of the eye (invariant fields only).
 */

void
propagate_eye(int origin, struct eye_data eye[BOARDMAX])
{
  int pos;

  for (pos = BOARDMIN; pos < BOARDMAX; pos++)
    if (ON_BOARD(pos) && eye[pos].origin == origin) {
      eye[pos].color         = eye[origin].color;
      eye[pos].esize         = eye[origin].esize;
      eye[pos].msize         = eye[origin].msize;
      eye[pos].origin        = eye[origin].origin;
      eye[pos].value         = eye[origin].value;
    }
}


/* Find the dragon or dragons surrounding an eye space. Up to
 * max_dragons dragons adjacent to the eye space are added to
 * the dragon array, and the number of dragons found is returned.
 */

int
find_eye_dragons(int origin, struct eye_data eye[BOARDMAX], int eye_color,
		 int dragons[], int max_dragons)
{
  int mx[BOARDMAX];
  int num_dragons = 0;
  int pos;

  memset(mx, 0, sizeof(mx));
  DEBUG(DEBUG_MISCELLANEOUS, "find_eye_dragons: %1m %C\n", origin, eye_color);
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (board[pos] == eye_color
	&& mx[dragon[pos].origin] == 0
	&& ((ON_BOARD(SOUTH(pos))
	     && eye[SOUTH(pos)].origin == origin
	     && !eye[SOUTH(pos)].marginal)
	    || (ON_BOARD(WEST(pos))
		&& eye[WEST(pos)].origin == origin
		&& !eye[WEST(pos)].marginal)
	    || (ON_BOARD(NORTH(pos))
		&& eye[NORTH(pos)].origin == origin
		&& !eye[NORTH(pos)].marginal)
	    || (ON_BOARD(EAST(pos))
		&& eye[EAST(pos)].origin == origin
		&& !eye[EAST(pos)].marginal))) {
      DEBUG(DEBUG_MISCELLANEOUS, 
	    "  dragon: %1m %1m\n", pos, dragon[pos].origin);
      mx[dragon[pos].origin] = 1;
      if (dragons != NULL && num_dragons < max_dragons)
	dragons[num_dragons] = dragon[pos].origin;
      num_dragons++;
    }
  }
  
  return num_dragons;
}

/* Print debugging data for the eyeshape at (i,j). Useful with GDB.
 */

static void
print_eye(struct eye_data eye[BOARDMAX], struct half_eye_data heye[BOARDMAX],
	  int pos)
{
  int m, n;
  int pos2;
  int mini, maxi;
  int minj, maxj;
  int origin = eye[pos].origin;

  gprintf("Eyespace at %1m: color=%C, esize=%d, msize=%d\n",
	  pos, eye[pos].color, eye[pos].esize, eye[pos].msize);
  
  for (pos2 = BOARDMIN; pos2 < BOARDMAX; pos2++) {
    if (!ON_BOARD(pos2))
      continue;
    
    if (eye[pos2].origin != pos) 
      continue;
    
    if (eye[pos2].marginal && IS_STONE(board[pos2]))
      gprintf("%1m (X!)\n", pos2);
    else if (is_halfeye(heye, pos2) && IS_STONE(board[pos2])) {
      if (heye[pos2].value == 3.0)
	gprintf("%1m (XH)\n", pos2);
      else
	gprintf("%1m (XH) (topological eye value = %f)\n", pos2,
		heye[pos2].value);
    }
    else if (!eye[pos2].marginal && IS_STONE(board[pos2]))
      gprintf("%1m (X)\n", pos2);
    else if (eye[pos2].marginal && board[pos2] == EMPTY)
      gprintf("%1m (!)\n", pos2);
    else if (is_halfeye(heye, pos2) && board[pos2] == EMPTY) {
      if (heye[pos2].value == 3.0)
	gprintf("%1m (H)\n", pos2);
      else
	gprintf("%1m (H) (topological eye value = %f)\n", pos2,
		heye[pos2].value);
    }
    else
      gprintf("%1m\n", pos2);
  }
  gprintf("\n");
  
  /* Determine the size of the eye. */
  mini = board_size;
  maxi = -1;
  minj = board_size;
  maxj = -1;
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      if (eye[POS(m, n)].origin != origin)
	continue;

      if (m < mini) mini = m;
      if (m > maxi) maxi = m;
      if (n < minj) minj = n;
      if (n > maxj) maxj = n;
    }

  /* Prints the eye shape. A half eye is shown by h, if empty or H, if an
   * enemy is present. Note that each half eye has a marginal point which is 
   * not printed, so the representation here may have less points than the 
   * matching eye pattern in eyes.db. Printing a marginal for the half eye
   * would be nice, but difficult to implement.
   */
  for (m = mini; m <= maxi; m++) {
    gprintf(""); /* Get the indentation right. */
    for (n = minj; n <= maxj; n++) {
      int pos2 = POS(m, n);
      if (eye[pos2].origin == origin) {
	if (board[pos2] == EMPTY) {
	  if (eye[pos2].marginal)
	    gprintf("%o!");
	  else if (is_halfeye(heye, pos2))
	    gprintf("%oh");
	  else
	    gprintf("%o.");
	}
	else if (is_halfeye(heye, pos2))
	  gprintf("%oH");
	else
	  gprintf("%oX");
      }
      else
	gprintf("%o ");
    }
    gprintf("\n");
  }
}


/* 
 * Given an eyespace with origin (pos), this function computes the
 * minimum and maximum numbers of eyes the space can yield. If max and
 * min are different, then vital points of attack and defense are also
 * generated.
 * 
 * If add_moves == 1, this function may add a move_reason for (color) at
 * a vital point which is found by the function. If add_moves == 0,
 * set color == EMPTY.
 */

void
compute_eyes(int pos, struct eyevalue *value,
	     int *attack_point, int *defense_point,
	     struct eye_data eye[BOARDMAX],
	     struct half_eye_data heye[BOARDMAX], int add_moves)
{
  if (attack_point)
    *attack_point = NO_MOVE;
  if (defense_point)
    *defense_point = NO_MOVE;

  if (debug & DEBUG_EYES) {
    print_eye(eye, heye, pos);
    DEBUG(DEBUG_EYES, "\n");
  }
  
  /* Look up the eye space in the graphs database. */
  if (read_eye(pos, attack_point, defense_point, value, eye, heye,
	       NULL, add_moves))
    return;

  /* Ideally any eye space that hasn't been matched yet should be two
   * secure eyes. Until the database becomes more complete we have
   * some additional heuristics to guess the values of unknown
   * eyespaces.
   */
  if (eye[pos].esize - 2*eye[pos].msize > 3)
    set_eyevalue(value, 2, 2, 2, 2);
  else if (eye[pos].esize - 2*eye[pos].msize > 0)
    set_eyevalue(value, 1, 1, 1, 1);
  else
    set_eyevalue(value, 0, 0, 0, 0);
}


/*
 * This function works like compute_eyes(), except that it also gives
 * a pessimistic view of the chances to make eyes. Since it is intended
 * to be used from the owl code, the option to add move reasons has
 * been removed.
 */
void
compute_eyes_pessimistic(int pos, struct eyevalue *value,
			 int *pessimistic_min,
			 int *attack_point, int *defense_point,
			 struct eye_data eye[BOARDMAX],
			 struct half_eye_data heye[BOARDMAX],
			 int eyefilling_points[BOARDMAX])
{
  static int bulk_coefficients[5] = {-1, -1, 1, 4, 12};

  int pos2;
  int margins = 0;
  int halfeyes = 0;
  int margins_adjacent_to_margin = 0;
  int effective_eyesize;
  int bulk_score = 0;
  signed char chainlinks[BOARDMAX];
  int contains_inset = 0;

  /* Stones inside eyespace which do not coincide with a false eye or
   * a halfeye.
   */
  int interior_stones = 0;

  memset(chainlinks, 0, BOARDMAX);

  for (pos2 = BOARDMIN; pos2 < BOARDMAX; pos2++) {
    int k;

    if (!ON_BOARD(pos2) || eye[pos2].origin != pos)
      continue;

    if (eye[pos2].marginal || is_halfeye(heye, pos2)) {
      margins++;
      if (eye[pos2].marginal && eye[pos2].marginal_neighbors > 0)
	margins_adjacent_to_margin++;
      if (is_halfeye(heye, pos2))
	halfeyes++;
    }
    else if (IS_STONE(board[pos2]))
      interior_stones++;

    bulk_score += bulk_coefficients[(int) eye[pos2].neighbors];

    for (k = 0; k < 4; k++) {
      int neighbor = pos2 + delta[k];

      if (board[neighbor] == eye[pos].color) {
	if (!chainlinks[neighbor]) {
	  bulk_score += 4;
	  mark_string(neighbor, chainlinks, 1);
	  /* In a position like this
	   *
	   * |OOO
	   * |X.O
	   * |OXO
	   * |.OO
	   * |.O
	   * |OO
	   *
	   * it would make sense to treat this as a single eyespace
	   * but the stone in atari on the edge splits it up in two
	   * separate eyespaces which are evaluated as 2222 and 1111
	   * respectively. Since they together in fact are 1122 this
	   * is way off. The best solution would be to to merge the
	   * eyespaces but since support for evaluating such eyespaces
	   * is missing we try to workaround it by setting pessimistic
	   * min to zero for both detected eyespaces.
	   *
	   * The code below does the detection of such stones called
	   * inset. Modification of pessimistic min is done later in
	   * the function.
	   */
	  if (countlib(neighbor) == 1
	      && attack(neighbor, NULL) != 0) {
	    int is_inset = 1;
	    int splits_eyespace = 0;
	    int m;

	    for (m = 0; m < 4; m++) {
	      if (ON_BOARD(neighbor + delta[m])) {
		if (eye[neighbor + delta[m]].color == eye[pos].color) {
		  if (eye[neighbor + delta[m]].origin != pos)
		    splits_eyespace = 1;
		}
		else
		  is_inset = 0;
	      }
	    }
	    contains_inset |= (is_inset & splits_eyespace);
	  }
	}
      }
      else if (!ON_BOARD(neighbor))
	bulk_score += 2;
    }
  }

  /* This is a measure based on the simplified assumption that both
   * players only cares about playing the marginal eye spaces. It is
   * used later to guess the eye value for unidentified eye shapes.
   */
  effective_eyesize = (eye[pos].esize + halfeyes - 2*margins
		       - margins_adjacent_to_margin);

  if (attack_point)
    *attack_point = NO_MOVE;
  if (defense_point)
    *defense_point = NO_MOVE;

  if (debug & DEBUG_EYES) {
    print_eye(eye, heye, pos);
    DEBUG(DEBUG_EYES, "\n");
  }
  
  /* Look up the eye space in the graphs database. */
  if (read_eye(pos, attack_point, defense_point, value,
	       eye, heye, eyefilling_points, 0)) {
    *pessimistic_min = min_eyes(value) - margins;

    /* A single point eye which is part of a ko can't be trusted. */
    if (eye[pos].esize == 1
	&& is_ko(pos, OTHER_COLOR(eye[pos].color), NULL))
      *pessimistic_min = 0;

    DEBUG(DEBUG_EYES, "  graph matching - %s, pessimistic_min=%d\n",
	  eyevalue_to_string(value), *pessimistic_min);
  }
  
  /* Ideally any eye space that hasn't been matched yet should be two
   * secure eyes. Until the database becomes more complete we have
   * some additional heuristics to guess the values of unknown
   * eyespaces.
   */
  else {
    guess_eye_space(pos, effective_eyesize, margins, bulk_score, eye,
		    value, pessimistic_min); 
    DEBUG(DEBUG_EYES, "  guess_eye - %s, pessimistic_min=%d\n",
	  eyevalue_to_string(value), *pessimistic_min);
  }

  if (*pessimistic_min < 0) {
    *pessimistic_min = 0;
    DEBUG(DEBUG_EYES, "  pessimistic min revised to 0\n");
  }

  /* An eyespace with at least two interior stones is assumed to be
   * worth at least one eye, regardless of previous considerations.
   */
  if (*pessimistic_min < 1 && interior_stones >= 2) {
    *pessimistic_min = 1;
    DEBUG(DEBUG_EYES, "  pessimistic min revised to 1 (interior stones)\n");
  }

  if (contains_inset) {
    *pessimistic_min = 0;
    DEBUG(DEBUG_EYES, "  pessimistic min revised to 0 (contains inset)\n");
  }

  if (attack_point
      && *attack_point == NO_MOVE
      && max_eyes(value) != *pessimistic_min) {
    /* Find one marginal vertex and set as attack and defense point.
     *
     * We make some effort to find the best marginal vertex by giving
     * priority to ones with more than one neighbor in the eyespace.
     * We prefer non-halfeye margins and ones which are not self-atari
     * for the opponent. Margins not on the edge are also favored.
     */
    int best_attack_point = NO_MOVE;
    int best_defense_point = NO_MOVE;
    float score = 0.0;
    
    for (pos2 = BOARDMIN; pos2 < BOARDMAX; pos2++) {
      if (ON_BOARD(pos2) && eye[pos2].origin == pos) {
	float this_score = 0.0;
	int this_attack_point = NO_MOVE;
	int this_defense_point = NO_MOVE;
	if (eye[pos2].marginal && board[pos2] == EMPTY) {
	  this_score = eye[pos2].neighbors;
	  this_attack_point = pos2;
	  this_defense_point = pos2;

	  if (is_self_atari(pos2, OTHER_COLOR(eye[pos].color)))
	    this_score -= 0.5;
	  
	  if (is_edge_vertex(pos2))
	    this_score -= 0.1;
	}
	else if (is_halfeye(heye, pos2)) {
	  this_score = 0.75;
	  this_defense_point = heye[pos2].defense_point[0];
	  this_attack_point = heye[pos2].attack_point[0];
	}
	else
	  continue;
	
	if (gg_normalize_float2int(this_score, 0.01)
	    > gg_normalize_float2int(score, 0.01)) {
	  best_attack_point = this_attack_point;
	  best_defense_point = this_defense_point;
	  score = this_score;
	}
      }
    }
    
    if (score > 0.0) {
      if (defense_point)
	*defense_point = best_defense_point;
      if (attack_point)
	*attack_point = best_attack_point;
    }
  }

  if (defense_point && *defense_point != NO_MOVE) {
    ASSERT_ON_BOARD1(*defense_point);
  }
  if (attack_point && *attack_point != NO_MOVE) {
    ASSERT_ON_BOARD1(*attack_point);
  }
}


static void
guess_eye_space(int pos, int effective_eyesize, int margins,
		int bulk_score, struct eye_data eye[BOARDMAX],
		struct eyevalue *value, int *pessimistic_min)
{
  if (effective_eyesize > 3) {
    set_eyevalue(value, 2, 2, 2, 2);
    if (margins > 0)
      *pessimistic_min = 1;
    else
      *pessimistic_min = 2;

    if ((margins == 0 && effective_eyesize > 7)
	|| (margins > 0 && effective_eyesize > 9)) {
      int eyes = 2 + (effective_eyesize - 2 * (margins > 0) - 8) / 2;
      int threshold = (4 * (eye[pos].esize - 2)
		       + (effective_eyesize - 8) * (effective_eyesize - 9));

      DEBUG(DEBUG_EYES, "size: %d(%d), threshold: %d, bulk score: %d\n",
	    eye[pos].esize, effective_eyesize, threshold, bulk_score);

      if (bulk_score > threshold && effective_eyesize < 15)
	eyes = gg_max(2, eyes - ((bulk_score - threshold) / eye[pos].esize));

      if (bulk_score < threshold + eye[pos].esize || effective_eyesize >= 15)
	*pessimistic_min = eyes;

      set_eyevalue(value, eyes, eyes, eyes, eyes);
    }
  }
  else if (effective_eyesize > 0) {
    set_eyevalue(value, 1, 1, 1, 1);
    if (margins > 0)
      *pessimistic_min = 0;
    else
      *pessimistic_min = 1;
  }
  else {
    if (eye[pos].esize - margins > 2)
      set_eyevalue(value, 0, 0, 1, 1);
    else
      set_eyevalue(value, 0, 0, 0, 0);
    *pessimistic_min = 0;
  }
}


/* This function does some minor reading to improve the results of
 * recognize_eye(). Currently, it has two duties. One is to read
 * positions like this:
 *
 *     .XXXX|        with half eye         with proper eye
 *     XXOOO|
 *     XO.O.|           .   (1 eye)           .   (2 eyes)
 *     XXOa.|         !..                    .*
 *     -----+
 *
 * recognize_eye() sees the eyespace of the white dragon as shown
 * (there's a half eye at a and it is considered the same as '!.' by
 * the optics code). Normally, that eye shape gives only one secure
 * eye, and owl thinks that the white dragon is dead unconditionally.
 * This function tries to turn such ko-dependent half eyes into proper
 * eyes and chooses the best alternative. Note that we don't have any
 * attack/defense codes here, since owl will determine them itself.
 *
 * Another one is related to some cases when replacing half eyes with
 * '!.' doesn't work. E.g. consider this eye (optics:328):
 *
 *     XXXOO         eye graph is 310:
 *     X..X.
 *     XOXX.             !.!  (second '!' is due to the halfeye)
 *     OXO..
 *     O.O..
 *
 * When this function detects such a half eye that can be attacked
 * and/or defended inside its eyespace, it tries to turn it into a
 * proper eye and see what happens. In case it gives an improvement
 * for attacker and/or defender, the function keeps new result but
 * only if new vital points are also vital points for the half eye.
 * The heuristics used here might need improvements since they are
 * based on a single game position.
 *
 * If add_moves != 0, this function may add move reasons for (color)
 * at the vital points which are found by recognize_eye(). If add_moves 
 * == 0, set color to be EMPTY.
 */
static int
read_eye(int pos, int *attack_point, int *defense_point,
	 struct eyevalue *value, struct eye_data eye[BOARDMAX], 
	 struct half_eye_data heye[BOARDMAX], int eyefilling_points[BOARDMAX],
	 int add_moves)
{
  int eye_color;
  int k;
  int pos2;
  int combination_halfeye = NO_MOVE;
  int combination_attack = NO_MOVE;
  int combination_defense = NO_MOVE;
  int num_ko_halfeyes = 0;
  int ko_halfeye = NO_MOVE;
  struct vital_points vp;
  struct vital_points ko_vp;
  struct vital_points *best_vp = &vp;

  eye_color = recognize_eye(pos, attack_point, defense_point, value,
			    eye, heye, &vp, eyefilling_points);
  if (!eye_color)
    return 0;

  /* Find ko half eyes and "combination" half eyes if any. */
  for (pos2 = BOARDMIN; pos2 < BOARDMAX; pos2++) {
    if (ON_BOARD(pos2)
	&& eye[pos2].origin == pos
	&& heye[pos2].type == HALF_EYE) {
      if (combination_halfeye == NO_MOVE) {
	int apos = NO_MOVE;
	int dpos = NO_MOVE;
 
	for (k = 0; k < heye[pos2].num_attacks; k++) {
	  if (eye[heye[pos2].attack_point[k]].origin == pos) {
	    apos = heye[pos2].attack_point[k];
	    break;
	  }
	}
  
	for (k = 0; k < heye[pos2].num_defenses; k++) {
	  if (eye[heye[pos2].defense_point[k]].origin == pos) {
	    dpos = heye[pos2].defense_point[k];
	    break;
	  }
	}
 
	if (apos || dpos) {
	  combination_halfeye = pos2;
	  combination_attack = apos;
	  combination_defense = dpos;
	}
      }
 
      if (heye[pos2].value < 3.0) {
	num_ko_halfeyes++;
	ko_halfeye = pos2;
      }
    }
  }

  /* In case we have a "combination" half eye, turn it into a proper eye
   * vertex for a while and see what happens.
   */
  if (combination_halfeye != NO_MOVE) {
    int result;
    int apos = NO_MOVE;
    int dpos = NO_MOVE;
    struct eyevalue combination_value;
    struct vital_points combination_vp;

    heye[combination_halfeye].type = 0;
    result = recognize_eye(pos, &apos, &dpos, &combination_value, eye,
			   heye, &combination_vp, NULL);
    heye[combination_halfeye].type = HALF_EYE;

    if (result) {
      if (combination_attack
	  && min_eyes(value) > min_eyes(&combination_value)) {
	/* FIXME: I'm not sure we can ever get here. */
	for (k = 0; k < combination_vp.num_attacks; k++) {
	  if (combination_vp.attacks[k] == combination_attack) {
	    value->a = combination_value.a;
	    value->b = combination_value.b;
	    *attack_point = apos;
	    best_vp->num_attacks = 1;
	    best_vp->attacks[0] = combination_attack;
	    break;
	  }
	}
      }

      if (combination_defense
	  && max_eyes(value) < max_eyes(&combination_value)) {
	/* Turning the half eye into a proper eye gives an improvement.
	 * However, we can only accept this result if there is a vital
	 * point that defends both the half eye and the whole eyespace.
	 */
	for (k = 0; k < combination_vp.num_defenses; k++) {
	  if (combination_vp.defenses[k] == combination_defense) {
	    value->c = combination_value.c;
	    value->d = combination_value.d;
	    *defense_point = dpos;
	    best_vp->num_defenses = 1;
	    best_vp->defenses[0] = combination_defense;
	    break;
	  }
	}
      }

      if (min_eyes(value) != max_eyes(value)) {
	ASSERT1(combination_attack || combination_defense, combination_halfeye);
	if (*attack_point == NO_MOVE) {
	  *attack_point = combination_attack;
	  if (*attack_point == NO_MOVE)
	    *attack_point = combination_defense;
	}

	if (*defense_point == NO_MOVE) {
	  *defense_point = combination_defense;
	  if (*defense_point == NO_MOVE)
	    *defense_point = combination_defense;
	}
      }
    }
  }

  /* The same with ko half eye (we cannot win two kos at once, therefore we
   * give up if there is more than one ko half eye).
   */
  if (num_ko_halfeyes == 1) {
    int result;
    int apos = NO_MOVE;
    int dpos = NO_MOVE;
    struct eyevalue ko_value;

    heye[ko_halfeye].type = 0;
    result = recognize_eye(pos, &apos, &dpos, &ko_value, eye,
			   heye, &ko_vp, NULL);
    heye[ko_halfeye].type = HALF_EYE;

    if (result && max_eyes(value) < max_eyes(&ko_value)) {
      /* It is worthy to win the ko. */
      *value = ko_value;
      *attack_point = apos;
      *defense_point = dpos;
      best_vp = &ko_vp;
    }
  }

  if (add_moves) {
    struct vital_eye_points *vital;
    if (eye_color == WHITE)
      vital = white_vital_points;
    else
      vital = black_vital_points;
    for (k = 0; k < best_vp->num_defenses && k < MAX_EYE_ATTACKS; k++)
      vital[pos].defense_points[k] = best_vp->defenses[k];
    for (k = 0; k < best_vp->num_attacks && k < MAX_EYE_ATTACKS; k++)
      vital[pos].attack_points[k] = best_vp->attacks[k];
  }

  return 1;
}


/* recognize_eye(pos, *attack_point, *defense_point, *max, *min, eye_data,
 * half_eye_data, color, vp), where pos is the origin of an eyespace, returns
 * owner of eye (his color) if there is a pattern in eyes.db matching the
 * eyespace, or 0 if no match is found. If there is a key point for attack,
 * (*attack_point) is set to its location, or NO_MOVE if there is none.
 * Similarly (*defense_point) is the location of a vital defense point.
 * *value is set according to the pattern found. Vital attack/defense points
 * exist if and only if min_eyes(value) != max_eyes(value).
 */

static int
recognize_eye(int pos, int *attack_point, int *defense_point,
	      struct eyevalue *value,
	      struct eye_data eye[BOARDMAX], 
	      struct half_eye_data heye[BOARDMAX], 
	      struct vital_points *vp,
	      int eyefilling_points[BOARDMAX])
{
  int pos2;
  int eye_color;
  int eye_size = 0;
  int num_marginals = 0;
  int vpos[MAXEYE];
  signed char marginal[MAXEYE];
  signed char edge[MAXEYE];
  signed char neighbors[MAXEYE];
  signed char stone[MAXEYE];
  struct eye_graph *graph;
  int map[MAXEYE];
  int best_score;
  int r;

  gg_assert(attack_point != NULL);
  gg_assert(defense_point != NULL);

  /* Set `eye_color' to the owner of the eye. */
  eye_color = eye[pos].color;

  if (eye[pos].esize-eye[pos].msize > 8)
    return 0;

  if (eye[pos].msize > MAXEYE)
    return 0;

  /* Create list of eye vertices */
  for (pos2 = BOARDMIN; pos2 < BOARDMAX; pos2++) {
    if (!ON_BOARD(pos2))
      continue;
    if (eye[pos2].origin == pos) {
      vpos[eye_size] = pos2;
      marginal[eye_size] = eye[pos2].marginal;
      if (marginal[eye_size])
	num_marginals++;
      neighbors[eye_size] = eye[pos2].neighbors;
      stone[eye_size] = IS_STONE(board[pos2]);
      if (0) {
	if (marginal[eye_size])
	  TRACE("(%1m)", vpos[eye_size]);
	else
	  TRACE(" %1m ", vpos[eye_size]);
	TRACE("\n");
      }
      
      if (is_corner_vertex(pos2))
	edge[eye_size] = 2;
      else if (is_edge_vertex(pos2))
	edge[eye_size] = 1;
      else 
	edge[eye_size] = 0;
      
      if (is_halfeye(heye, pos2)) {
	neighbors[eye_size]++;      /* Increase neighbors of half eye. */
	eye_size++;
	/* Use a virtual marginal vertex for mapping purposes. We set it
	 * to be at NO_MOVE so it won't accidentally count as a
	 * neighbor for another vertex. Note that the half eye precedes
	 * the virtual marginal vertex in the list.
	 */
	vpos[eye_size] = NO_MOVE;
	marginal[eye_size] = 1;
	num_marginals++;
	edge[eye_size] = 0;
	neighbors[eye_size] = 1;
	stone[eye_size] = 0;
      }
      
      eye_size++;
    }
  }

  graph = optical_graph_matcher(vpos, marginal, edge, neighbors, stone,
				eye_size, num_marginals, map, heye);

  if (!graph)
    return 0;

  /* We have found a match! Now sort out the vital moves. */
  *value = graph->value;
  vp->num_attacks = 0;
  vp->num_defenses = 0;

  if (eye_move_urgency(value) > 0) {
    /* Collect all attack and defense points in the pattern. */
    int k;

    for (k = 0; k < eye_size; k++) {
      struct eye_vertex *ev = &graph->vertex[k];

      if (ev->flags & EYE_ATTACK_POINT) {
	/* Check for a marginal vertex matching a half eye virtual
	 * marginal. This is the case if a half eye preceeds the
	 * current vertex in the list.
	 */
	if (ev->marginal
	    && map[k] > 0
	    && vpos[map[k] - 1] != NO_MOVE
	    && is_halfeye(heye, vpos[map[k] - 1])) {
	  /* Add all diagonals as vital. */
	  int ix;
	  struct half_eye_data *he = &heye[vpos[map[k] - 1]];

	  for (ix = 0; ix < he->num_attacks; ix++)
	    vp->attacks[vp->num_attacks++] = he->attack_point[ix];
	}
	else
	  vp->attacks[vp->num_attacks++] = vpos[map[k]];
      }

      if (ev->flags & EYE_DEFENSE_POINT) {
	/* Check for a half eye virtual marginal vertex. */
	if (ev->marginal
	    && map[k] > 0
	    && vpos[map[k] - 1] != NO_MOVE
	    && is_halfeye(heye, vpos[map[k] - 1])) {
	  /* Add all diagonals as vital. */
	  int ix;
	  struct half_eye_data *he = &heye[vpos[map[k] - 1]];

	  for (ix = 0; ix < he->num_defenses; ix++)
	    vp->defenses[vp->num_defenses++] = he->defense_point[ix];
	}
	else
	  vp->defenses[vp->num_defenses++] = vpos[map[k]];
      }
    }

    gg_assert(vp->num_attacks > 0 && vp->num_defenses > 0);

    /* We now have all vital attack and defense points listed but
     * we are also expected to single out of one of each to return
     * in *attack_point and *defense_point. Since sometimes those
     * are the only vital points considered, we want to choose the
     * best ones, in the sense that they minimize the risk for
     * error in the eye space analysis.
     *
     * One example is this position
     *
     * |..XXXX
     * |XXX..X
     * |..!O.X
     * |OO.O.X
     * |.O.!XX
     * +------
     *
     * where O has an eyespace of the !..! type. The graph
     * matching finds that both marginal vertices are vital points
     * but here the one at 3-3 fails to defend. (For attack both
     * points work but the 3-3 one is still worse since it leaves
     * a ko threat.)
     *
     * In order to differentiate between the marginal points we
     * count the number of straight and diagonal neighbors within
     * the eye space. In the example above both have one straight
     * neighbor each but the edge margin wins because it also has
     * a diagonal margin.
     */

    best_score = -10;
    for (k = 0; k < vp->num_attacks; k++) {
      int apos = vp->attacks[k];
      int score = 0;
      for (r = 0; r < 8; r++)
	if (ON_BOARD(apos + delta[r])
	    && eye[apos + delta[r]].color == eye[pos].color
	    && !eye[apos + delta[r]].marginal) {
	  score++;
	  if (r < 4) {
	    score++;
	    if (board[apos + delta[r]] != EMPTY)
	      score++;
	  }
	}

      /* If a vital point is not adjacent to any point in the eye
       * space, it must be a move to capture or defend a string
       * related to a halfeye, e.g. the move * in this position,
       *
       * ......|
       * .XXXX.|
       * .X.O..|
       * .XO.OO|
       * .*XO..|
       * ------+
       *
       * Playing this is probably a good idea.
       */
      if (score == 0)
	score += 2;

      if (0)
	gprintf("attack point %1m score %d\n", apos, score);

      if (score > best_score) {
	*attack_point = apos;
	best_score = score;
      }
    }

    best_score = -10;
    for (k = 0; k < vp->num_defenses; k++) {
      int dpos = vp->defenses[k];
      int score = 0;
      for (r = 0; r < 8; r++)
	if (ON_BOARD(dpos + delta[r])
	    && eye[dpos + delta[r]].color == eye[pos].color
	    && !eye[dpos + delta[r]].marginal) {
	  score++;
	  if (r < 4) {
	    score++;
	    if (board[dpos + delta[r]] != EMPTY)
	      score++;
	  }
	}

      /* If possible, choose a non-sacrificial defense point.
       * Compare white T8 and T6 in lazarus:21.
       */
      if (safe_move(dpos, eye_color) != WIN)
	score -= 5;

      /* See comment to the same code for attack points. */
      if (score == 0)
	score += 2;

      if (0)
	gprintf("defense point %1m score %d\n", dpos, score);

      if (score > best_score) {
	*defense_point = dpos;
	best_score = score;
      }
    }

    DEBUG(DEBUG_EYES, "  vital points: %1m (attack) %1m (defense)\n",
	  *attack_point, *defense_point);
    DEBUG(DEBUG_EYES, "  pattern matched:  %d\n", graph->patnum);

  }

  /* In certain types of semeais it is necessary to fill in nakade
   * shapes, e.g. in a position like this:
   *
   * OOOOOXXXXXX
   * OXXXXOOOOOX
   * OX.OXXO..OX
   * OX.OOXO..OX
   * -----------
   *
   * However, only one of the lower left liberties leaves a nakade
   * shape when O plays there. The code below tries adding a stone at
   * each empty vertex in the eyespace and do graph matching to find
   * the eyevalue of the resulting eyespace. If it's still only worth
   * one eye it is a valid eyefilling point.
   *
   * In some positions a subset of the eyefilling points give the
   * opponent a ko threat. If this can be avoided we prefer the points
   * which don't. One example is
   *
   * XXXX
   * XabXX
   * XcOdX
   * -----
   *
   * where playing at the empty vertices a and d leaves a ko threat to
   * make two eyes, whereas b and c don't.
   *
   * We only do this analysis if the information is requested (when
   * called from semeai reading), the eyeshape is worth exactly one
   * eye, and there are no marginal points (then generally better to
   * play those first).
   */
  if (eyefilling_points
      && min_eyes(value) == 1 && max_eyes(value) == 1 && num_marginals == 0) {
    int k;
    int num_empty = 0;
    /* Count the number of empty points in the eyespace. */
    for (k = 0; k < eye_size; k++) {
      if (!stone[k])
	num_empty++;
    }

    /* We can't fill in an eyespace with only one liberty. It would
     * either be suicide or capture the surrounding stones.
     */
    if (num_empty > 1) {
      int valid_point[MAXEYE];
      int threat[MAXEYE];
      int min_threat = 2;
      /* For each empty point, add a stone and do the graph matching
       * again.
       */
      for (k = 0; k < eye_size; k++) {
	if (!stone[k]) {
	  struct eye_graph *graph2;
	  stone[k] = 1;
	  valid_point[k] = 0;
	  graph2 = optical_graph_matcher(vpos, marginal, edge, neighbors, stone,
					 eye_size, num_marginals, map, heye);
	  if (graph2 && max_eyes(&graph2->value) < 2) {
	    /* This is an effective eyefilling point. Also check
	     * whether it leaves a ko threat.
	     */
	    valid_point[k] = 1;
	    threat[k] = max_eye_threat(&graph2->value);
	    if (threat[k] < min_threat)
	      min_threat = threat[k];
	  }
	  stone[k] = 0;
	}
      }
      /* Loop through the points once more to record those which are
       * both effective eyefilling points and don't leave a ko threat
       * if it can be avoided.
       */
      for (k = 0; k < eye_size; k++) {
	if (!stone[k] && valid_point[k]) {
	  int pos = vpos[k];
	  if (threat[k] == min_threat) {
	    eyefilling_points[pos] = 1;
	    TRACE("Eyefilling point at %1m\n", pos);
	  }
	}
      }
    }
  }

  TRACE("eye space at %1m of type %d\n", pos, graph->patnum);
  return eye_color;
}


/* Perform the actual matching of the graphs in eyes.db. */
static struct eye_graph *
optical_graph_matcher(int *vpos, signed char *marginal, signed char *edge,
		      signed char *neighbors, signed char *stone, int eye_size,
		      int num_marginals, int *map,
		      struct half_eye_data *heye)
{
  /* We attempt to construct a map from the graph to the eyespace
   * preserving the adjacency structure. If this can be done, we've
   * identified the eyeshape.
   */
  int n;

  for (n = 0; graphs[n].vertex != NULL; n++) {
    int q;

    if (graphs[n].esize != eye_size
	|| graphs[n].msize != num_marginals)
      continue;

    reset_map(eye_size);
    q = 0;
    first_map(&map[0]);

    while (1) {
      struct eye_vertex *gv = &graphs[n].vertex[q];
      int mv = map[q];
      int ok = 1;

      if (0)
	TRACE("q=%d: %d %d %d %d %d %d\n", 
	      q, map[0], map[1], map[2], map[3], map[4], map[5]);

      if (neighbors[mv] != gv->neighbors
	  || marginal[mv] != gv->marginal
	  || edge[mv] < gv->edge)
	ok = 0;

      if (ok) {
        if (stone[mv]) {
	  if (!(gv->flags & CAN_CONTAIN_STONE))
	    ok = 0;
	}
	/* Virtual half eye marginals also fall here since they are off
	 * board.
	 */
	else if (!(gv->flags & CAN_BE_EMPTY))
	  ok = 0;
      }

      if (ok) {
	int k;

	for (k = 0; k < gv->neighbors; k++) {
	  if (gv->n[k] < q) {
	    int mn = map[gv->n[k]];

	    /* Two eye vertices are neighbours if they are adjacent on the
	     * board or one of them is a half eye and the other is its
	     * virtual marginal vertex (and follows it in vpos[] array).
	     */
	    if (vpos[mv] != SOUTH(vpos[mn])
		&& vpos[mv] != WEST(vpos[mn])
		&& vpos[mv] != NORTH(vpos[mn])
		&& vpos[mv] != EAST(vpos[mn])
		&& (mv != mn - 1
		    || vpos[mv] == NO_MOVE
		    || heye[vpos[mv]].type != HALF_EYE)
		&& (mn != mv - 1
		    || vpos[mn] == NO_MOVE
		    || heye[vpos[mn]].type != HALF_EYE)) {
	      ok = 0;
	      break;
	    }
	  }
	}
      }

      if (!ok) {
	if (!next_map(&q, map))
	  break;

	if (0)
	  gprintf("  q=%d, esize=%d: %d %d %d %d %d\n",
		  q, eye_size, 
		  map[0], map[1], map[2], map[3], map[4]);
      }
      else {
	q++;
	if (q == eye_size)
	  break;			/* A match! */
	
	first_map(&map[q]);
      }
    }

    /* Successful match. */
    if (q == eye_size)
      return &graphs[n];
  }

  return NULL;
}


/* a MAP is a map of the integers 0,1,2, ... ,q into 
 * 0,1, ... , esize-1 where q < esize. This determines a 
 * bijection of the first q+1 elements of the graph into the 
 * eyespace. The following three functions work with maps.
 */

/* Reset internal data structure used by first_map() and
 * next_map() functions.
 */
static void
reset_map(int size)
{
  map_size = size;
  memset(used_index, 0, size * sizeof(used_index[0]));
}


/* The function first_map finds the smallest valid
 * value of a map element.
 */
static void
first_map(int *map_value)
{
  int k = 0;

  while (used_index[k])
    k++;

  used_index[k] = 1;
  *map_value = k;
}     


/* The function next_map produces the next map in lexicographical
 * order. If no next map can be found, q is decremented, then we
 * try again. If the entire map is lexicographically last, the
 * function returns false.
 */
static int
next_map(int *q, int map[MAXEYE])
{
  int k;

  do {
    used_index[map[*q]] = 0;
    for (k = map[*q]; ++k < map_size;) {
      if (!used_index[k]) {
	used_index[k] = 1;
	map[*q] = k;
	return 1;
      }
    }

    (*q)--;
  } while (*q >= 0);
  
  return 0;
}     


/* add_false_eye() turns a proper eyespace into a margin. */

static void
add_false_eye(int pos, struct eye_data eye[BOARDMAX],
	      struct half_eye_data heye[BOARDMAX])
{
  int k;
  ASSERT1(heye[pos].type == FALSE_EYE, pos);
  DEBUG(DEBUG_EYES, "false eye found at %1m\n", pos);

  if (eye[pos].color == GRAY || eye[pos].marginal != 0)
    return;
  
  eye[pos].marginal = 1;
  eye[eye[pos].origin].msize++;
  for (k = 0; k < 4; k++)
    if (ON_BOARD(pos + delta[k])
	&& eye[pos + delta[k]].origin == eye[pos].origin)
      eye[pos + delta[k]].marginal_neighbors++;
  propagate_eye(eye[pos].origin, eye);
}


/* These functions are used from constraints to identify eye spaces,
 * primarily for late endgame moves.
 */
int
is_eye_space(int pos)
{
  return (white_eye[pos].color == WHITE
	  || black_eye[pos].color == BLACK);
}

int
is_proper_eye_space(int pos)
{
  return ((white_eye[pos].color == WHITE && !white_eye[pos].marginal)
	  || (black_eye[pos].color == BLACK && !black_eye[pos].marginal));
}

/* Return the maximum number of eyes that can be obtained from the
 * eyespace at (i, j). This is most useful in order to determine
 * whether the eyespace can be assumed to produce any territory at
 * all.
 */
int
max_eye_value(int pos)
{
  int max_white = 0;
  int max_black = 0;
  
  if (white_eye[pos].color == WHITE)
    max_white = max_eyes(&white_eye[pos].value);

  if (black_eye[pos].color == BLACK)
    max_black = max_eyes(&black_eye[pos].value);

  return gg_max(max_white, max_black);
}

int
is_marginal_eye_space(int pos)
{
  return (white_eye[pos].marginal || black_eye[pos].marginal);
}

int
is_halfeye(struct half_eye_data heye[BOARDMAX], int pos)
{
  return heye[pos].type == HALF_EYE;
}

int
is_false_eye(struct half_eye_data heye[BOARDMAX], int pos)
{
  return heye[pos].type == FALSE_EYE;
}


/* Find topological half eyes and false eyes by analyzing the
 * diagonal intersections, as described in the Texinfo
 * documentation (Eyes/Eye Topology).
 */
void
find_half_and_false_eyes(int color, struct eye_data eye[BOARDMAX],
			 struct half_eye_data heye[BOARDMAX],
			 int find_mask[BOARDMAX])
{
  int eye_color = color;
  int pos;
  float sum;
  
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    /* skip eyespaces which owl doesn't want to be searched */
    if (!ON_BOARD(pos) || (find_mask && find_mask[eye[pos].origin] <= 1))
      continue;
    
    /* skip every vertex which can't be a false or half eye */
    if (eye[pos].color != eye_color
        || eye[pos].marginal
        || eye[pos].neighbors > 1)
      continue;
    
    sum = topological_eye(pos, color, eye, heye);
    if (sum >= 4.0) {
      /* false eye */
      heye[pos].type = FALSE_EYE;
      if (eye[pos].esize == 1
          || is_legal(pos, OTHER_COLOR(color))
          || board[pos] == OTHER_COLOR(color))
        add_false_eye(pos, eye, heye);
    }
    else if (sum > 2.0) {
      /* half eye */
      heye[pos].type = HALF_EYE;
      ASSERT1(heye[pos].num_attacks > 0, pos);
      ASSERT_ON_BOARD1(heye[pos].attack_point[0]);
      ASSERT1(heye[pos].num_defenses > 0, pos);
      ASSERT_ON_BOARD1(heye[pos].defense_point[0]);
    }
  }
}


/* See Texinfo documentation (Eyes:Eye Topology). Returns:
 * - 2 or less if (pos) is a proper eye for (color);
 * - between 2 and 3 if the eye can be made false only by ko
 * - 3 if (pos) is a half eye;
 * - between 3 and 4 if the eye can be made real only by ko
 * - 4 or more if (pos) is a false eye.
 *
 * Attack and defense points for control of the diagonals are stored
 * in the heye[] array.
 *
 * my_eye is the eye space information with respect to (color).
 */

static float
topological_eye(int pos, int color,
		struct eye_data my_eye[BOARDMAX],
		struct half_eye_data heye[BOARDMAX])
{
  float sum = 0.0;
  float val;
  int num_attacks = 0;
  int num_defenses = 0;
  int attack_values[4];
  int defense_values[4];
  int k;
  int r;
  int attack_point;
  int defense_point;
  int attack_value;
  int defense_value;

  memset(attack_values, 0, sizeof(attack_values));
  memset(defense_values, 0, sizeof(defense_values));
  
  /* Loop over the diagonal directions. */
  for (k = 4; k < 8; k++) {
    int diag = pos + delta[k];
    val = evaluate_diagonal_intersection(I(pos) + deltai[k],
					 J(pos) + deltaj[k], color,
					 &attack_point, &defense_point, 
					 my_eye);

    /*
     * Eyespaces with cutting points are problematic. In this position
     * 
     * .....XXXXX
     * XXXXX.OO.X
     * X.OOOO.O.X
     * X.O.XXXO.X
     * ----------
     * 
     * the eyespace will be .XXX. which evaluates to two eyes (seki)
     * unless countermeasures are taken.
     *
     * This can be worked around in the topological analysis by
     * sometimes setting the diagonal value to 2.0 for vertices inside
     * the eyespace which are occupied by opponent stones. More
     * precisely all of the following conditions must hold:
     *
     * a) The value is not already 2.0.
     * a) The (potential) eyepoint is empty.
     * b) The diagonal is occupied by an opponent string,
     * c) which is also adjacent to the (potential) eye and 
     * d) at least three stones long.
     * e) The (potential) eye is not on the edge (to steer clear of all the
     *    hairy cases that are handled by eyes.db anyway).
     * f) At least two own strings are adjacent to the (potential) eye.
     * g) At least one of the own strings adjacent to the (potential) eye has
     *    only one liberty which is an eye space and not decided false, yet.
     *
     * With this revision the eyespace above becomes .XXXh or
     * equivalently .XXX.! which is almost evaluated correctly, eye
     * value 0122 instead of the correct 1122. Compared to the
     * previous value 2222 it's a major improvement.
     *
     * FIXME: This approach has a number of shortcomings.
     *
     *        1. d) is kind of arbitrary and there may be exceptional
     *           cases.
     *
     *        2. This diagonal value modification should not apply to
     *           two diagonals of the same strings inside the eyespace.
     *           E.g. if we have a partial eyespace looking like
     *
     *           .OOO.
     *           OO.OO
     *           OXXXO
     *
     *           it doesn't make sense to mark the middle vertex as a
     *           false eye. Possibly this doesn't make any difference
     *           in practice but it's at the very least confusing.
     *
     *        3. Actually it doesn't make sense to mark vertices as
     *           false otherwise either due to these revisions (half
     *           eyes make good sense though) as can be seen if a
     *           stone is added to the initial diagram,
     * 
     *           .....XXXXX
     *           XXXXXXOO.X
     *           X.OOOO.O.X
     *           X.O.XXXO.X
     *           ----------
     *
     *           Now the eyespace instead becomes .XXX! which has the
     *           eye value 0011 but if X tries to attack the eye O
     *           suddenly gets two solid eyes!
     *
     *           The correct analysis would be to remove the vertex
     *           from the eyespace rather than turning it into a false
     *           eye. Then we would have the eyespace .XXX which is
     *           correctly evaluated to one eye (eye value 1112).
     *
     *           The problem with this is that removing eye points is
     *           messy. It can surely be done but currently there is
     *           no support in the code for doing that. It has existed
     *           at an earlier time but was removed because the
     *           implementation was not robust enough and there was no
     *           longer any apparent need for it. To correct this
     *           problem is sufficient reason to reimplement that
     *           functionality.
     *
     *        4. The test of condition g) has a result which
     *           potentially depends on the ordering of the eyespaces
     *           and thus presumably on the orientation of the board.
     *           It might make more sense to examine whether the
     *           string neighbors more than one empty vertex in the
     *           same eyespace.
     */
    if (val < 2.0 && board[pos] == EMPTY && board[diag] == OTHER_COLOR(color)
	&& !is_edge_vertex(pos) && neighbor_of_string(pos, diag)
	&& countstones(diag) >= 3) {
      int strings[3];
      int string_count;
      int s;
      string_count = 0;
      for (r = 0; r < 4; r++) {
	int str;
	str = pos + delta[r];

	if (board[str] != color)
	  continue;

	ASSERT1(string_count < 3, pos);
	for (s = 0; s < string_count; s++)
	  if (same_string(str, strings[s]))
	    break;
	if (s != string_count)
	  continue;

	strings[string_count++] = str;
      }
      if (string_count > 1) {
	for (s = 0; s < string_count; s++) {
	  int libs[MAX_LIBERTIES];
	  int adj_eye_count;
	  int lib_count;
	  adj_eye_count = 0;
	  lib_count = findlib(strings[s], MAX_LIBERTIES, libs);
	  if (lib_count > MAX_LIBERTIES)
	    continue;

	  for (r = 0; r < lib_count && adj_eye_count < 2; r++)
	    if (my_eye[libs[r]].color == OTHER_COLOR(color)
		&& !my_eye[libs[r]].marginal)
	      adj_eye_count++;
	  if (adj_eye_count < 2) {
	    val = 2.0;
	    break;
	  }
	}
      }
    }

    sum += val;

    if (val > 0.0 && val < 2.0) {
      /* Diagonals off the edge has value 1.0 but no attack or defense
       * point.
       */
      if (attack_point != NO_MOVE && defense_point != NO_MOVE) {
	ASSERT_ON_BOARD1(attack_point);
	ASSERT_ON_BOARD1(defense_point);
	/* Store these in sorted (descending) order. We remap val
         * differently for attack and defense points according to:
	 *
	 * val    attack_value     defense_value
	 * ---    ------------     -------------
	 * 1.0    3                3
	 * <1.0   2                1
	 * >1.0   1                2
	 *
	 * This means that we primarily want to take control of
	 * diagonals without ko and secondarily of diagonals we can
	 * take unconditionally but not the opponent.
	 */
	if (val == 1.0) {
	  attack_value = 3;
	  defense_value = 3;
	}
	else if (val < 1.0) {
	  attack_value = 2;
	  defense_value = 1;
	}
	else {
	  attack_value = 1;
	  defense_value = 2;
	}

	for (r = 0; r < 4; r++) {
	  if (attack_values[r] < attack_value) {
	    int tmp_value = attack_values[r];
	    int tmp_point;
	    if (tmp_value)
	      tmp_point = heye[pos].attack_point[r];
	    else
	      tmp_point = 0;
	    attack_values[r] = attack_value;
	    heye[pos].attack_point[r] = attack_point;
	    attack_value = tmp_value;
	    attack_point = tmp_point;
	  }
	  
	  if (defense_values[r] < defense_value) {
	    int tmp_value = defense_values[r];
	    int tmp_point;
	    if (tmp_value)
	      tmp_point = heye[pos].defense_point[r];
	    else
	      tmp_point = 0;
	    defense_values[r] = defense_value;
	    heye[pos].defense_point[r] = defense_point;
	    defense_value = tmp_value;
	    defense_point = tmp_point;
	  }
	}
	
	num_attacks++;
	num_defenses++;
      }
    }
  }

  /* Remove attacks and defenses with smaller value than the best
   * ones. (These might be useful to save as well, but not unless we
   * also store the attack/defense values in the half_eye_data.)
   */
  for (r = 0; r < num_attacks; r++) {
    if (attack_values[r] < attack_values[0]) {
      num_attacks = r;
      break;
    }
  }
  
  for (r = 0; r < num_defenses; r++) {
    if (defense_values[r] < defense_values[0]) {
      num_defenses = r;
      break;
    }
  }

  heye[pos].num_attacks = num_attacks;
  heye[pos].num_defenses = num_defenses;
  heye[pos].value = sum;

  return sum;
}



/* Evaluate an intersection (m, n) which is diagonal to an eye space,
 * as described in the Texinfo documentation (Eyes/Eye Topology).
 *
 * Returns:
 *
 * 0 if both coordinates are off the board
 * 1 if one coordinate is off the board
 *
 * 0    if (color) has control over the vertex
 * a    if (color) can take control over the vertex unconditionally and
 *      the opponent can take control by winning a ko.
 * 1    if both (color) and the opponent can take control of the vertex
 *      unconditionally
 * b    if (color) can take control over the vertex by winning a ko and
 *      the opponent can take control unconditionally.
 * 2    if the opponent has control over the vertex
 *
 * The values a and b are discussed in the documentation. We are
 * currently using a = 0.75 and b = 1.25.
 *
 * Notice that it's necessary to pass the coordinates separately
 * instead of as a 1D coordinate. The reason is that the 1D mapping
 * can't uniquely identify "off the corner" points.
 *
 * my_eye has to be the eye_data with respect to color.
 */
static float
evaluate_diagonal_intersection(int m, int n, int color,
			       int *attack_point, int *defense_point,
			       struct eye_data my_eye[BOARDMAX])
{
  float value = 0;
  int other = OTHER_COLOR(color);
  int pos = POS(m, n);
  int acode = 0;
  int apos = NO_MOVE;
  int dcode = 0;
  int dpos = NO_MOVE;
  int off_edge = 0;
  const float a = 0.75;
  const float b = 2 - a;

  *attack_point = NO_MOVE;
  *defense_point = NO_MOVE;
  
  /* Check whether intersection is off the board. We must do this for
   * each board coordinate separately because points "off the corner"
   * are special cases.
   */
  if (m < 0 || m >= board_size)
    off_edge++;

  if (n < 0 || n >= board_size)
    off_edge++;

  /* Must return 0 if both coordinates out of bounds. */
  if (off_edge > 0)
    return (float) (off_edge % 2);

  /* Discard points within own eyespace, unless marginal or ko point.
   *
   * Comment: For some time discardment of points within own eyespace
   * was contingent on this being the same eyespace as that of the
   * examined vertex. This caused problems, e.g. in this position,
   *
   * |........
   * |XXXXX...
   * |OOOOX...
   * |aO.OX...
   * |OXXOX...
   * |.XXOX...
   * +--------
   *
   * where the empty vertex at a was evaluated as a false eye and the
   * whole group as dead (instead of living in seki).
   *
   * The reason for the requirement of less than two marginal
   * neighbors is this position:
   *
   * |.XXXX...
   * |.OOOX...
   * |O..OX...
   * |aOO.X...
   * |O..XX...
   * |..O.X...
   * |.X..X...
   * |..XXX...
   *
   * where the empty vertex at a should not count as a solid eye.
   * (The eyespace diagonally below a looks like this:
   *   .!
   *   !
   * so we can clearly see why having two marginal vertices makes a
   * difference.)
   */
   if (my_eye[pos].color == color
       && !my_eye[pos].marginal
       && my_eye[pos].marginal_neighbors < 2
       && !(board[pos] == EMPTY && does_capture_something(pos, other)))
    return 0.0;

  if (board[pos] == EMPTY) {
    int your_safety = safe_move(pos, other);

    apos = pos;
    dpos = pos;

    /* We should normally have a safe move, but occasionally it may
     * happen that it's not safe. There are complications, however,
     * with a position like this:
     *
     * .XXXX|
     * XXOO.|
     * XO.O.|
     * XXO.O|
     * -----+
     *
     * Therefore we ignore our own safety if opponent's safety depends
     * on ko.
     */
    if (your_safety == 0)
      value = 0.0;
    else if (your_safety != WIN)
      value = a;
    else {                           /* So your_safety == WIN. */
      int our_safety = safe_move(pos, color);
      
      if (our_safety == 0) {
	int k;

	value = 2.0;

	/* This check is intended to fix a certain special case, but might
	 * be helpful in other situations as well. Consider this position,
	 * happened in owl reading deep enough:
	 *
	 * |XXXXX
	 * |XOOXX
	 * |O.OOX
	 * |.OXX.
	 * +-----
	 *
	 * Without this check, the corner eye is considered false, not half-
	 * eye. Thus, owl thinks that the capture gains at most one eye and
	 * gives up.
	 */
	for (k = 4; k < 8; k++) {
	  int diagonal = pos + delta[k];
	  int lib;

	  if (board[diagonal] == other && findlib(diagonal, 1, &lib) == 1) {
	    if (lib != pos && does_secure(color, lib, pos)) {
	      value = 1.0;
	      apos = lib;
	      break;
	    }
	  }
	}
      }
      else if (our_safety == WIN)
        value = 1.0;
      else                           /* our_safety depends on ko. */
        value = b;
    }
  }
  else if (board[pos] == color) {
    /* This stone had better be safe, otherwise we wouldn't have an
     * eyespace in the first place.
     */
    value = 0.0;
  }
  else if (board[pos] == other) {
    if (stackp == 0) {
      acode = worm[pos].attack_codes[0];
      apos  = worm[pos].attack_points[0];
      dcode = worm[pos].defense_codes[0];
      dpos  = worm[pos].defense_points[0];
    }
    else
      attack_and_defend(pos, &acode, &apos, &dcode, &dpos);

    /* Must test acode first since dcode only is reliable if acode is
     * non-zero.
     */
    if (acode == 0)
      value = 2.0;
    else if (dcode == 0)
      value = 0.0;
    else if (acode == WIN && dcode == WIN)
      value = 1.0;
    else if (acode == WIN && dcode != WIN)
      value = a;
    else if (acode != WIN && dcode == WIN)
      value = b;
    else if (acode != WIN && dcode != WIN)
      value = 1.0; /* Both contingent on ko. Probably can't happen. */
  }
  
  if (value > 0.0 && value < 2.0) {
    /* FIXME:
     * Usually there are several attack and defense moves that would
     * be equally valid. It's not good that we make an arbitrary
     * choice at this point.
     */
    ASSERT_ON_BOARD1(apos);
    ASSERT_ON_BOARD1(dpos);
    /* Notice:
     * The point to ATTACK the half eye is the point which DEFENDS
     * the stones on the diagonal intersection and vice versa. Thus
     * we must switch attack and defense points here.
     * If the vertex is empty, dpos == apos and it doesn't matter
     * whether we switch.
     */
    *attack_point = dpos;
    *defense_point = apos;
  }

  return value;
}


/* Conservative relative of topological_eye(). Essentially the same
 * algorithm is used, but only tactically safe opponent strings on
 * diagonals are considered. This may underestimate the false/half eye
 * status, but it should never be overestimated.
 */
int
obvious_false_eye(int pos, int color)
{
  int i = I(pos);
  int j = J(pos);
  int k;
  int diagonal_sum = 0;
  for (k = 4; k < 8; k++) {
    int di = deltai[k];
    int dj = deltaj[k];
    
    if (!ON_BOARD2(i+di, j) && !ON_BOARD2(i, j+dj))
      diagonal_sum--;
    
    if (!ON_BOARD2(i+di, j+dj))
      diagonal_sum++;
    else if (BOARD(i+di, j+dj) == OTHER_COLOR(color)
	     && !attack(POS(i+di, j+dj), NULL))
      diagonal_sum += 2;
  }
  
  return diagonal_sum >= 4;
}


/* Set the parameters into struct eyevalue as follows:
a = number of eyes if attacker plays first twice
b = number of eyes if attacker plays first
c = number of eyes if defender plays first
d =number of eyes if defender plays first twice
*/

void
set_eyevalue(struct eyevalue *e, int a, int b, int c, int d)
{
  e->a = a;
  e->b = b;
  e->c = c;
  e->d = d;
}

/* Number of eyes if attacker plays first twice (the threat of the first
 * move by attacker).
 */
int
min_eye_threat(struct eyevalue *e)
{
  return e->a;
}

/* Number of eyes if attacker plays first followed by alternating play. */
int
min_eyes(struct eyevalue *e)
{
  return e->b;
}

/* Number of eyes if defender plays first followed by alternating play. */
int
max_eyes(struct eyevalue *e)
{
  return e->c;
}

/* Number of eyes if defender plays first twice (the threat of the first
 * move by defender).
 */
int
max_eye_threat(struct eyevalue *e)
{
  return e->d;
}

/* Add the eyevalues *e1 and *e2, leaving the result in *sum. It is
 * safe to let sum be the same as e1 or e2.
 */
void
add_eyevalues(struct eyevalue *e1, struct eyevalue *e2, struct eyevalue *sum)
{
  struct eyevalue res;
  res.a = gg_min(gg_min(e1->a + e2->c, e1->c + e2->a),
		 gg_max(e1->a + e2->b, e1->b + e2->a));
  res.b = gg_min(gg_max(e1->b + e2->b, gg_min(e1->a + e2->d, e1->b + e2->c)),
		 gg_max(e1->b + e2->b, gg_min(e1->d + e2->a, e1->c + e2->b)));
  res.c = gg_max(gg_min(e1->c + e2->c, gg_max(e1->d + e2->a, e1->c + e2->b)),
		 gg_min(e1->c + e2->c, gg_max(e1->a + e2->d, e1->b + e2->c)));
  res.d = gg_max(gg_max(e1->d + e2->b, e1->b + e2->d),
		 gg_min(e1->d + e2->c, e1->c + e2->d));

  /* The rules above give 0011 + 0002 = 0012, which is incorrect. Thus
   * we need this annoying exception.
   */
  if ((e1->d - e1->c == 2 && e2->c - e2->b == 1)
      || (e1->c - e1->b == 1 && e2->d - e2->c == 2)) {
    res.d = gg_max(gg_min(e1->c + e2->d, e1->d + e2->b),
		   gg_min(e1->d + e2->c, e1->b + e2->d));
  }

  /* The temporary storage in res is necessary if sum is the same as
   * e1 or e2.
   */
  sum->a = res.a;
  sum->b = res.b;
  sum->c = res.c;
  sum->d = res.d;
}

/* The impact on the number of eyes (counting up to two) if a vital
 * move is made. The possible values are
 * 0 - settled eye, no vital move
 * 2 - 1/2 eye or 3/2 eyes
 * 3 - 3/4 eyes or 5/4 eyes
 * 4 - 1* eyes (a chimera)
 */
int
eye_move_urgency(struct eyevalue *e)
{
  int a = gg_min(e->a, 2);
  int b = gg_min(e->b, 2);
  int c = gg_min(e->c, 2);
  int d = gg_min(e->d, 2);
  if (b == c)
    return 0;
  else
    return d + c - b - a;
}

/* Produces a string representing the eyevalue.
 * 
 * Note: the result string is stored in a statically allocated buffer
 * which will be overwritten the next time this function is called.
 */
char *
eyevalue_to_string(struct eyevalue *e)
{
  static char result[30];
  if (e->a < 10 && e->b < 10 && e->c < 10 && e->d < 10)
    gg_snprintf(result, 29, "%d%d%d%d", e->a, e->b, e->c, e->d);
  else
    gg_snprintf(result, 29, "[%d,%d,%d,%d]", e->a, e->b, e->c, e->d);
  return result;
}



/* Test whether the optics code evaluates an eyeshape consistently. */
void
test_eyeshape(int eyesize, int *eye_vertices)
{
  int k;
  int n, N;
  int mx[BOARDMAX];
  int pos;
  int str = NO_MOVE;
  int attack_code;
  int attack_point;
  int defense_code;
  int defense_point;
  int save_verbose;
  struct board_state starting_position;

  /* Clear the board and initialize the engine properly. */
  clear_board();
  reset_engine();

  /* Mark the eyespace in the mx array. */
  memset(mx, 0, sizeof(mx));
  for (k = 0; k < eyesize; k++) {
    ASSERT_ON_BOARD1(eye_vertices[k]);
    mx[eye_vertices[k]] = 1;
  }

  /* Play white stones surrounding the eyespace, including diagonals. */
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (!ON_BOARD(pos) || mx[pos] == 1)
      continue;
    for (k = 0; k < 8; k++) {
      if (ON_BOARD(pos + delta[k]) && mx[pos + delta[k]] == 1) {
	play_move(pos, WHITE);
	str = pos;
	break;
      }
    }
  }

  /* Play black stones surrounding the white group, but leaving all
   * liberties empty.
   */
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (mx[pos] == 1 || board[pos] != EMPTY || liberty_of_string(pos, str))
      continue;
    for (k = 0; k < 8; k++) {
      if (ON_BOARD(pos + delta[k])
	  && liberty_of_string(pos + delta[k], str)) {
	play_move(pos, BLACK);
	break;
      }
    }
  }

  /* Show the board if verbose is on. Then turn off traces so we don't
   * get any from make_worms(), make_dragons(), or the owl reading.
   */
  if (verbose)
    showboard(0);
  save_verbose = verbose;
  verbose = 0;
  
  
  /* Store this position so we can come back to it. */
  store_board(&starting_position);

  /* Loop over all configurations of black stones inserted in the
   * eyeshape. There are N = 2^(eyesize) configurations and we can
   * straightforwardly use binary representation to enumerate them.
   */
  N = 1 << eyesize;
  for (n = 0; n < N; n++) {
    int valid = 1;
    int internal_stones = 0;
    
    restore_board(&starting_position);
    /* Play the stones for this configuration. */
    for (k = 0; k < eyesize; k++) {
      if (n & (1 << k)) {
	if (!is_legal(eye_vertices[k], BLACK)) {
	  valid = 0;
	  break;
	}
	play_move(eye_vertices[k], BLACK);
	internal_stones++;
      }
    }

    if (!valid)
      continue;

    if (save_verbose > 1)
      showboard(0);

    /* Now we are ready to test the consistency. This is most easily
     * done with help from the owl code. First we must prepare for
     * this though.
     */
    examine_position(EXAMINE_DRAGONS_WITHOUT_OWL, 0);

    attack_code = owl_attack(str, &attack_point, NULL, NULL);

    if (attack_code == 0) {
      /* The owl code claims there is no attack. We test this by
       * trying to attack on all empty spaces in the eyeshape.
       */
      for (k = 0; k < eyesize; k++) {
	if (board[eye_vertices[k]] == EMPTY
	    && is_legal(eye_vertices[k], BLACK)
	    && owl_does_attack(eye_vertices[k], str, NULL)) {
	  gprintf("%1m alive, but %1m attacks:\n", str, eye_vertices[k]);
	  showboard(0);
	  gprintf("\n");
	}
      }

      /* Furthermore, if the eyespace is almost filled, white should
       * be able to play on the remaining eyespace point and still be
       * alive.
       */
      if (internal_stones == eyesize - 1) {
	for (k = 0; k < eyesize; k++) {
	  if (board[eye_vertices[k]] == EMPTY
	      && !owl_does_defend(eye_vertices[k], str, NULL)) {
	    gprintf("%1m alive, but almost filled with nakade:\n", str);
	    showboard(0);
	  }
	}
      }
    }
    else {
      defense_code = owl_defend(str, &defense_point, NULL, NULL);
      if (defense_code == 0) {
	/* The owl code claims there is no defense. We test this by
	 * trying to defend on all empty spaces in the eyeshape.
	 */
	for (k = 0; k < eyesize; k++) {
	  if (board[eye_vertices[k]] == EMPTY
	      && is_legal(eye_vertices[k], WHITE)
	      && owl_does_defend(eye_vertices[k], str, NULL)) {
	    gprintf("%1m dead, but %1m defends:\n", str, eye_vertices[k]);
	    showboard(0);
	    gprintf("\n");
	  }
	}
      }
      else {
	/* The owl code claims the dragon is critical. Verify the
         * attack and defense points.
	 */
	if (board[attack_point] != EMPTY
	    || !is_legal(attack_point, BLACK)) {
	  gprintf("Bad attack point %1m:\n", attack_point);
	  showboard(0);
	}
	else if (!owl_does_attack(attack_point, str, NULL)) {
	  gprintf("Attack point %1m failed:\n", attack_point);
	  showboard(0);
	}

	if (board[defense_point] != EMPTY
	    || !is_legal(defense_point, WHITE)) {
	  gprintf("Bad defense point %1m:\n", defense_point);
	  showboard(0);
	}
	else if (!owl_does_defend(defense_point, str, NULL)) {
	  gprintf("Defense point %1m failed:\n", defense_point);
	  showboard(0);
	}
      }
    }
  }
  verbose = save_verbose;
}

/********************************************************************
 * The following static functions are helpers for analyze_eyegraph()
 * further down. The purpose is to evaluate eye graphs according to
 * the rules for local games, as described in doc/eyes.texi.
 *
 * The technique to do this is to convert the eye evaluation problem
 * into a tactical style life and death reading problem. Tactical in
 * the sense of needing to decide whether certain stones can be
 * captured, but not in the sense of the tactical reading that five
 * liberties are considered safe.
 *
 * We illustrate how this works with an example. Consider the eye shape
 *
 *   !
 *  .X
 * !...
 *
 * The basic idea is to embed the eyespace in a perfectly connected
 * group without additional eyes or eye potential. This is most easily
 * done by the somewhat brutal trick to fill the entire board with
 * stones. We let the group consist of white stones (O) and get this
 * result, disregarding the two marginal eye vertices:
 *
 *    A B C D E F G H J K L M N O P Q R S T
 * 19 O O O O O O O O O O O O O O O O O O O 19
 * 18 O O O O O O O O O O O O O O O O O O O 18
 * 17 O O O O O O O O O O O O O O O O O O O 17
 * 16 O O O O O O O O O O O O O O O O O O O 16
 * 15 O O O O O O O O O O O O O O O O O O O 15
 * 14 O O O O O O O O O O O O O O O O O O O 14
 * 13 O O O O O O O O O O O O O O O O O O O 13
 * 12 O O O O O O O O . O O O O O O O O O O 12
 * 11 O O O O O O O . X O O O O O O O O O O 11
 * 10 O O O O O O . . . . O O O O O O O O O 10
 *  9 O O O O O O O O O O O O O O O O O O O 9
 *  8 O O O O O O O O O O O O O O O O O O O 8
 *  7 O O O O O O O O O O O O O O O O O O O 7
 *  6 O O O O O O O O O O O O O O O O O O O 6
 *  5 O O O O O O O O O O O O O O O O O O O 5
 *  4 O O O O O O O O O O O O O O O O O O O 4
 *  3 O O O O O O O O O O O O O O O O O O O 3
 *  2 O O O O O O O O O O O O O O O O O O O 2
 *  1 O O O O O O O O O O O O O O O O O O O 1
 *    A B C D E F G H J K L M N O P Q R S T
 *
 * The question now is whether black can capture all the white stones
 * under alternating play where only white may pass. However, first we
 * need to make the top and leftmost eye vertices marginal. This is
 * done by inserting small invincible black groups in the sea of white
 * stones, in contact with the marginal vertices.
 *
 *    A B C D E F G H J K L M N O P Q R S T
 * 19 . O O O O O O O O O O O O O O O O O O 19
 * 18 O O O O O O O O X X X O O O O O O O O 18
 * 17 O O O O O O O O X . X O O O O O O O O 17
 * 16 O O O O O O O O X X X O O O O O O O O 16
 * 15 O O O O O O O O X . X O O O O O O O O 15
 * 14 O O O O O O O O X X X O O O O O O O O 14
 * 13 O O O O O O O O X O O O O O O O O O O 13
 * 12 O O O O O O O O . O O O O O O O O O O 12
 * 11 O O O O O O O . X O O O O O O O O O O 11
 * 10 O O O O O O . . . . O O O O O O O O O 10
 *  9 O O O O O O X O O O O O O O O O O O O 9
 *  8 O O O O X X X O O O O O O O O O O O O 8
 *  7 O O O O X . X O O O O O O O O O O O O 7
 *  6 O O O O X X X O O O O O O O O O O O O 6
 *  5 O O O O X . X O O O O O O O O O O O O 5
 *  4 . O O O X X X O O O O O O O O O O O O 4
 *  3 X X . O O O O O O O O O O O O O O O O 3
 *  2 X . X O O O O O O O O O O O O O O O O 2
 *  1 . X X O O O O O O O O O O O O O O O O 1
 *    A B C D E F G H J K L M N O P Q R S T
 *
 * In this diagram we have also added an invincible black group in the
 * lower left corner in order to add two outer liberties (at A4 and
 * C3) for the white group (this is sometimes needed for the tactical
 * life and death reading to make sense). Furthermore there is an
 * extra eye at A19. This is used when we want to distinguish between
 * 0 and 1 (or 2) eyes since the tactical life and death reading by
 * itself only cares about two eyes or not. When trying to distinguish
 * between 1 (or 0) and 2 eyes we first fill in A19 again.
 *
 * Depending on the tactical life and death status with or without the
 * extra eye we can determine the number of eyes. By evaluating
 * tactical life and death status after having made a move we can also
 * identify ko threats and critical moves.
 *
 * This code is organized as follows:
 *
 * analyze_eyegraph() converts the eyegraph into the tactical board
 * position as demonstrated, then calls evaluate_eyespace() to its eye
 * value.
 *
 * white_area() is a helper to add a small invincible black group on
 * the board.
 *
 * evaluate_eyespace() calls tactical_life() and itself recursively to
 * determine the eye value and the critical points.
 *
 * tactical_life() determines whether the white stones on the board
 * (assumed to be a single string) can be captured under alternating
 * play.
 *
 * tactical_life_attack() and tactical_life_defend() are two mutually
 * recursive functions which perform the actual reading for
 * tactical_life().
 *
 * Worth to mention in this overview is also the cache used for
 * tactical_life_attack() and tactical_life_defend(). Since we have a
 * limited number of vertices (eye space points + two outer liberties
 * + possibly an extra eye) to play on we use a complete cache with a
 * unique entry for every possible configuration of stones on the
 * considered vertices.
 *
 * For each cache entry four bits are used, two for attack results and
 * two four defense results. Each of these can take the values 0-3
 * with the following interpretations:
 * 0 - not yet considered
 * 1 - result is being computed
 * 2 - result has been computed and was a failure (0)
 * 3 - result has been computed and was a success (1)
 */

/* Types of vertices under consideration. */
#define REGULAR       0
#define OUTER_LIBERTY 1
#define EXTRA_EYE     2
#define KO_THREAT     3

#define MAX_OUTER_LIBERTIES 4
#define MAX_KO_THREATS      4

struct vertex_data {
  int pos[MAX_BOARD];
  int type[MAX_BOARD];
  int num;
  int ko_threat_pos[MAX_KO_THREATS];
  int num_ko_threats;
  int num_ordinary;
  int komaster;
  int extra_eye_pos;
  int string;
};

/* Like trymove() except that it does a superko check. This does,
 * however, only disallow repetition (besides simple ko) for the
 * attacker.
 *
 * In order to cope with bent four in the corner, white is given one
 * free virtual ko threat, which black can eliminate by spending a
 * move playing on the "ko threat vertex". This has the side effect
 * that white wins all direct kos, which is mostly okay until ko
 * dependent eye values are implemented. It can, however, cause attack
 * and defense points dependent on ko to be recommended together with
 * non-ko vital points.
 */

static int forced_ko_threat_stackp[MAX_KO_THREATS];
static int num_active_ko_threats = 0;

static int
eyegraph_trymove(int pos, int color, const char *message, int str,
		 struct vertex_data *vertices)
{
  static Hash_data remembered_board_hashes[MAXSTACK];
  int k;
  
  remembered_board_hashes[stackp] = board_hash;

  if (!trymove(pos, color, message, str)) {
    int ko_threat_pos = NO_MOVE;

    if (!vertices)
      return 0;

    if (color != vertices->komaster
	|| is_suicide(pos, color))
      return 0;

    for (k = 0; k < vertices->num_ko_threats; k++)
      if (board[vertices->ko_threat_pos[k]] == EMPTY) {
	ko_threat_pos = vertices->ko_threat_pos[k];
	break;
      }

    if (ko_threat_pos != NO_MOVE) {
      trymove(ko_threat_pos, BLACK, "making/answering virtual ko threat", str);
      forced_ko_threat_stackp[num_active_ko_threats++] = stackp;
      trymove(pos, color, message, str);
    }
    else
      return 0;
  }

  if (color == WHITE)
    return 1;

  for (k = 0; k < stackp; k++)
    if (hashdata_is_equal(board_hash, remembered_board_hashes[k])) {
      popgo();
      return 0;
    }

  return 1;
}

static void
eyegraph_popgo(void)
{
  popgo();

  if (num_active_ko_threats > 0
      && forced_ko_threat_stackp[num_active_ko_threats - 1] == stackp) {
    popgo();
    num_active_ko_threats--;
  }
}

/* Has the vertex a neighbor which is connected to an invincible black
 * string?
 */
static int
eyegraph_is_margin_or_outer_liberty(int vertex)
{
  int k;
  int r;
  int num_libs;
  int libs[MAXLIBS];
  int eyes;
  
  for (k = 0; k < 4; k++) {
    if (board[vertex + delta[k]] == BLACK) {
      eyes = 0;
      num_libs = findlib(vertex + delta[k], MAXLIBS, libs);
      
      for (r = 0; r < num_libs; r++)
	if (is_suicide(libs[r], WHITE))
	  eyes++;
      
      if (eyes >= 2)
	return 1;
    }
  }
  return 0;
}

static int
eyegraph_order_moves(struct vertex_data *vertices, int color_to_move,
		     int *moves)
{
  int num_moves = 0;
  int scores[BOARDMAX];
  int move;
  int score;
  int k;
  int r;

  for (k = 0; k < vertices->num; k++) {
    if (vertices->type[k] != REGULAR) {
      /* Never useful for white to fill in outer liberties or a second eye. */
      if (color_to_move == WHITE)
	break;
      /* No use playing a later outer liberty before the previous one. */
      if (vertices->type[k] == OUTER_LIBERTY
	  && vertices->type[k - 1] == OUTER_LIBERTY
	  && board[vertices->pos[k - 1]] == EMPTY)
	continue;
    }
    
    move = vertices->pos[k];
    score = 0;

    if (board[move] != EMPTY)
      continue;
    
    if (eyegraph_is_margin_or_outer_liberty(move))
      if (vertices->type[k] == REGULAR)
	score = 5; /* margin */

    if (accuratelib(move, color_to_move, 2, NULL) == 1)
      score -= 3;
    
    for (r = 0; r < 4; r++) {
      if (board[move + delta[r]] == EMPTY)
	score += 2;
      else if (board[move + delta[r]] == BLACK)
	score += 3;
    }
    
    moves[num_moves] = move;
    scores[num_moves] = score;
    num_moves++;
  }

  for (k = 0; k < num_moves; k++) {
    int maxscore = scores[k];
    int max_at = 0;

    /* Find the move with the biggest score. */
    for (r = k + 1; r < num_moves; r++) {
      if (scores[r] > maxscore) {
	maxscore = scores[r];
	max_at = r;
      }
    }

    /* Now exchange the move at k with the move at max_at.
     * Don't forget to exchange the scores as well.
     */
    if (max_at != 0) {
      int temp = moves[max_at];
      moves[max_at] = moves[k];
      moves[k] = temp;
      temp = scores[max_at];
      scores[max_at] = scores[k];
      scores[k] = temp;
    }
  }
  
  return num_moves;
}

/* Place a small invincible black group on the board.
 * It is required that previously there were white stones at all
 * involved vertices and on the surrounding vertices.
 *
 * Returns 1 if a group was placed, 0 otherwise.
 */
static int
white_area(int mx[BOARDMAX], int pos, int up, int right, int marginpos,
	   int distance)
{
  int u, v;
  int k;
  int edge = is_edge_vertex(marginpos);

  for (k = 1; k < distance; k++)
    if (!ON_BOARD(marginpos + k * up)
	|| mx[marginpos + k * up] != WHITE)
      return 0;

  for (u = -1; u <= 4; u++)
    for (v = -1; v <= 4; v++) {
      int pos2 = pos + u * up + v * right;
      if (!ON_BOARD(pos2)) {
	if (!edge)
	  return 0;
	else if (u >= 0 && u <= 3 && v >= 0 && v <= 3)
	  return 0;
	else if (I(pos2) != I(NORTH(marginpos))
		 && I(pos2) != I(SOUTH(marginpos))
		 && J(pos2) != J(WEST(marginpos))
		 && J(pos2) != J(EAST(marginpos)))
	  return 0;
      }
      else if (mx[pos2] != WHITE)
	return 0;
    }
  
  for (u = 0; u <= 3; u++)
    for (v = 0; v <= 3; v++) {
      int pos2 = pos + u * up + v * right;
      mx[pos2] = BLACK;
    }

  mx[pos + up + right] = EMPTY;
  mx[pos + 2 * up + 2 * right] = EMPTY;

  return 1;
}


#define EYEGRAPH_RETURN(result, trace) \
  do { \
    if (sgf_dumptree) \
      sgftreeAddComment(sgf_dumptree, (trace)); \
    return (result); \
  } while (0);

static int tactical_life_defend(int str, struct vertex_data *vertices,
				unsigned char *results);

/* Determine whether black can capture all white stones. */
static int
tactical_life_attack(int str, struct vertex_data *vertices,
		     unsigned char *results)
{
  int k;
  int hash = 0;
  int cached_result;
  int result;
  int num_moves;
  int moves[BOARDMAX];

  /* Compute hash value to index the result cache with. */
  for (k = 0; k < vertices->num; k++) {
    hash *= 3;
    hash += board[vertices->pos[k]];
  }
  hash *= 2;
  hash += (board_ko_pos != NO_MOVE);

  /* Is the result known from the cache? */
  cached_result = results[hash] & 3;

  if (0) {
    showboard(0);
    gprintf("%d %d (%d)\n", hash, cached_result, results[hash]);
  }
  
  if (cached_result == 2)
    EYEGRAPH_RETURN(0, "tactical_life_attack: 0 (cached)");
  if (cached_result == 3)
    EYEGRAPH_RETURN(1, "tactical_life_attack: win (cached)");
  if (cached_result == 1)
    EYEGRAPH_RETURN(1, "tactical_life_attack: win (open node in cache)");

  /* Mark this entry in the cache as currently being computed. */
  results[hash] |= 1;

  /* Try to play on all relevant vertices. */
  num_moves = eyegraph_order_moves(vertices, OTHER_COLOR(board[str]), moves);
  for (k = 0; k < num_moves; k++) {
    int move = moves[k];
    if (eyegraph_trymove(move, OTHER_COLOR(board[str]),
			 "tactical_life_attack", str,
			 vertices)) {
      /* We were successful if the white stones were captured or if no
       * defense can be found.
       */
      if (board[str] == EMPTY)
	result = 1;
      else
	result = !tactical_life_defend(str, vertices, results);
      
      eyegraph_popgo();

      if (result == 1) {
	/* Store the result (success) in the cache. */
	results[hash] = (results[hash] & (~3)) | 3;
	EYEGRAPH_RETURN(1, "tactical_life_attack: win");
      }
    }
  }
  
  /* Store the result (failure) in the cache. */
  results[hash] = (results[hash] & (~3)) | 2;
  EYEGRAPH_RETURN(0, "tactical_life_attack: 0");
}

/* Determine whether white can live with all stones. */
static int
tactical_life_defend(int str, struct vertex_data *vertices,
		     unsigned char *results)
{
  int k;
  int hash = 0;
  int cached_result;
  int result;
  int num_moves;
  int moves[BOARDMAX];
  
  /* Compute hash value to index the result cache with. */
  for (k = 0; k < vertices->num; k++) {
    hash *= 3;
    ASSERT1(board[vertices->pos[k]] <= 2, vertices->pos[k]);
    hash += board[vertices->pos[k]];
  }
  hash *= 2;
  hash += (board_ko_pos != NO_MOVE);
  
  /* Is the result known from the cache? */
  cached_result = (results[hash] >> 2) & 3;

  if (0) {
    showboard(0);
    gprintf("%d %d (%d)\n", hash, cached_result, results[hash]);
  }

  if (cached_result == 2)
    EYEGRAPH_RETURN(0, "tactical_life_defend: 0 (cached)");
  if (cached_result == 3)
    EYEGRAPH_RETURN(1, "tactical_life_defend: win (cached)");
  if (cached_result == 1)
    EYEGRAPH_RETURN(1, "tactical_life_defend: win (node open in cache)");

  /* Mark this entry in the cache as currently being computed. */
  results[hash] |= (1 << 2);

  /* Try to play on all relevant vertices. */
  num_moves = eyegraph_order_moves(vertices, board[str], moves);
  for (k = 0; k < num_moves; k++) {
    int move = moves[k];
    if ((!is_suicide(move, OTHER_COLOR(board[str]))
	 || does_capture_something(move, board[str]))
	&& eyegraph_trymove(move, board[str], "tactical_life_defend", str,
			    vertices)) {
      /* We were successful if no attack can be found. */
      result = !tactical_life_attack(str, vertices, results);
      
      eyegraph_popgo();

      if (result == 1) {
	/* Store the result (success) in the cache. */
	results[hash] = (results[hash] & (~12)) | (3 << 2);
	EYEGRAPH_RETURN(1, "tactical_life_defend: win");
      }
    }
  }

  /* If no move worked, also try passing. */
  if (!tactical_life_attack(str, vertices, results)) {
    /* Store the result (success) in the cache. */
    results[hash] = (results[hash] & (~12)) | (3 << 2);
    EYEGRAPH_RETURN(1, "tactical_life_defend: win");
  }
  
  /* Store the result (failure) in the cache. */
  results[hash] = (results[hash] & (~12)) | (2 << 2);
  EYEGRAPH_RETURN(0, "tactical_life_defend: 0");
}

/* Determine the tactical life and death status of all white stones.
 * Also find all attack and defense moves. The parameter have_eye
 * determines whether the extra eye in the upper left corner should be
 * used or filled in before starting reading.
 */
static void
tactical_life(int have_eye, struct vertex_data *vertices,
	      int *attack_code, int *num_attacks, int *attack_points,
	      int *defense_code, int *num_defenses, int *defense_points,
	      unsigned char *results)
{
  int k;
  int str;
  int num_moves;
  int moves[BOARDMAX];

  gg_assert(attack_code != NULL && defense_code != NULL);

  /* We know that the large white group includes A18. This is the
   * vertex we test to determine whether the white stones have been
   * captured.
   */
  str = vertices->string;

  if (board[str] == EMPTY) {
    /* The stones have already been captured, too late to defend. */
    *attack_code = WIN;
    *defense_code = 0;
    return;
  }

  /* Fill in the extra eye if have_eye is 0. If filling in would be
   * suicide the white stones can be considered dead.
   */
  if (!have_eye) {
    if (!eyegraph_trymove(vertices->extra_eye_pos, WHITE, "tactical_life-A",
			  NO_MOVE, NULL)) {
      *attack_code = WIN;
      *defense_code = 0;
      return;
    }
  }
  
  *attack_code = 0;
  *defense_code = 0;

  /* Call tactical_life_attack() and tactical_life_defend() to
   * determine status.
   */
  if (tactical_life_attack(str, vertices, results)) {
    *attack_code = WIN;
    if (tactical_life_defend(str, vertices, results))
      *defense_code = WIN;
  }
  else
    *defense_code = WIN;


  /* If the status is critical, try to play at each relevant vertex
   * and call tactical_life_defend() or tactical_life_attack() to
   * determine whether the move works as attack or defense.
   */
  if (*attack_code != 0 && *defense_code != 0) {
    if (num_attacks != NULL && attack_points != NULL) {
      *num_attacks = 0;
      num_moves = eyegraph_order_moves(vertices,
				       OTHER_COLOR(board[str]), moves);
      for (k = 0; k < num_moves; k++) {
	int move = moves[k];
	if (eyegraph_trymove(move, OTHER_COLOR(board[str]), "tactical_life-B",
			     str, vertices)) {
	  if (board[str] == EMPTY
	      || !tactical_life_defend(str, vertices, results))
	    attack_points[(*num_attacks)++] = move;
	  eyegraph_popgo();
	}
      }
    }

    if (num_defenses != NULL && defense_points != NULL) {
      *num_defenses = 0;
      num_moves = eyegraph_order_moves(vertices, board[str], moves);
      for (k = 0; k < num_moves; k++) {
	int move = moves[k];
	if (eyegraph_trymove(move, board[str], "tactical_life-C", str,
			     vertices)) {
	  if (!tactical_life_attack(str, vertices, results))
	    defense_points[(*num_defenses)++] = move;
	  eyegraph_popgo();
	}
      }
    }
  }

  /* Unfill the extra eye if we didn't use it. */
  if (!have_eye)
    eyegraph_popgo();
}

/* Determine the eye value of the eyespace for the big white group on
 * the board and vital moves. The possible eye values are documented
 * in the preamble to eyes.db. By calling tactical_life() multiple
 * times, with and without using an extra eye, we can compute the eye
 * values. To determine ko threats and vital moves, tactical_life() is
 * called again after trying to play on one of the relevant vertices.
 * In order to find out whether ko threats really are effective and to
 * distinguish between 0122/1122 and 0012/0011 eye values (see
 * discussion on pattern 6141 in the preamble of eyes.db), we may also
 * need to recursively call ourselves after a move has been made.
 */
static void
evaluate_eyespace(struct eyevalue *result, struct vertex_data *vertices,
		  int *num_vital_attacks, int *vital_attacks,
		  int *num_vital_defenses, int *vital_defenses,
		  unsigned char *tactical_life_results)
{
  int k;
  int attack_code;
  int num_attacks;
  int attack_points[BOARDMAX];
  int defense_code;
  int num_defenses;
  int defense_points[BOARDMAX];
  int attack_code2;
  int num_attacks2;
  int attack_points2[BOARDMAX];
  int defense_code2;
  struct eyevalue result2;
  int num_vital_attacks2;
  int vital_attacks2[BOARDMAX];
  int num_vital_defenses2;
  int vital_defenses2[BOARDMAX];
  int num_moves;
  int moves[BOARDMAX];

  *num_vital_attacks = 0;
  *num_vital_defenses = 0;

  /* Determine tactical life without an extra eye. */
  tactical_life(0, vertices,
		&attack_code, &num_attacks, attack_points,
		&defense_code, &num_defenses, defense_points,
		tactical_life_results);

  if (attack_code == 0) {
    /* Alive without extra eye.
     * Possible results: 0222, 1222, 2222
     *
     * Determine whether there are ko threats and how serious.
     */
    int a = 2;

    if (sgf_dumptree)
      sgftreeAddComment(sgf_dumptree, "Alive without extra eye.\n");
    
    num_moves = eyegraph_order_moves(vertices, BLACK, moves);
    for (k = 0; k < num_moves; k++) {
      int acode, dcode;
      int move = moves[k];
      if (eyegraph_trymove(move, BLACK, "evaluate_eyespace-A", NO_MOVE,
			   vertices)) {
	tactical_life(0, vertices, &acode, NULL, NULL,
		      &dcode, NULL, NULL, tactical_life_results);
	if (acode != 0) {
	  tactical_life(1, vertices, &acode, NULL, NULL,
			&dcode, NULL, NULL, tactical_life_results);
	  if (acode != 0) {
	    if (a == 1)
	      *num_vital_attacks = 0;
	    a = 0;
	    vital_attacks[(*num_vital_attacks)++] = move;
	    if (sgf_dumptree)
	      sgftreeAddComment(sgf_dumptree,
				"Ko threat to remove both eyes.\n");
	  }
	  else {
	    if (a != 0) {
	      vital_attacks[(*num_vital_attacks)++] = move;
	      a = 1;
	    }
	    if (sgf_dumptree)
	      sgftreeAddComment(sgf_dumptree, "Ko threat to remove one eye.\n");
	  }
	}
	eyegraph_popgo();
      }
    }
    set_eyevalue(result, a, 2, 2, 2);
    if (sgf_dumptree) {
      if (a == 0)
	sgftreeAddComment(sgf_dumptree, "Eyevalue 0222.\n");
      else if (a == 1)
	sgftreeAddComment(sgf_dumptree, "Eyevalue 1222.\n");
      else
	sgftreeAddComment(sgf_dumptree, "Eyevalue 2222.\n");
    }
  }
  else if (defense_code != 0) {
    /* Critical without extra eye.
     * Possible results: 0022, 0122, 1122
     */
    if (sgf_dumptree)
      sgftreeAddComment(sgf_dumptree, "Critical without extra eye.\n");
    tactical_life(1, vertices,
		  &attack_code2, &num_attacks2, attack_points2,
		  &defense_code2, NULL, NULL, tactical_life_results);
    for (k = 0; k < num_defenses; k++)
      vital_defenses[(*num_vital_defenses)++] = defense_points[k];
    if (attack_code2 == WIN) {
      /* A chimera. 0022. */
      set_eyevalue(result, 0, 0, 2, 2);
      for (k = 0; k < num_attacks2; k++)
	vital_attacks[(*num_vital_attacks)++] = attack_points2[k];
      if (sgf_dumptree)
	sgftreeAddComment(sgf_dumptree, "Eyevalue: 0022.\n");
    }
    else {
      int a = 1;
      for (k = 0; k < num_attacks; k++) {
	int move = attack_points[k];
	if (eyegraph_trymove(move, BLACK, "evaluate_eyespace-B", NO_MOVE,
			     vertices)) {
	  evaluate_eyespace(&result2, vertices,
			    &num_vital_attacks2, vital_attacks2,
			    &num_vital_defenses2, vital_defenses2,
			    tactical_life_results);
	  /* If result2 is 0011 for some move we have 0122 as final
           * result, otherwise 1122.
	   */
	  if (min_eyes(&result2) == 0
	      && max_eyes(&result2) == 1
	      && max_eye_threat(&result2) == 1) {
	    if (a == 1)
	      *num_vital_attacks = 0;
	    a = 0;
	    vital_attacks[(*num_vital_attacks)++] = move;
	  }
	  else if (a == 1)
	    vital_attacks[(*num_vital_attacks)++] = move;
	  eyegraph_popgo();
	}
      }
      set_eyevalue(result, a, 1, 2, 2);
      if (sgf_dumptree) {
	if (a == 0)
	  sgftreeAddComment(sgf_dumptree, "Eyevalue: 0122.\n");
	else
	  sgftreeAddComment(sgf_dumptree, "Eyevalue: 1122.\n");
      }	  
    }
  }
  else {
    /* Dead without extra eye.
     * Possible results: 0000, 0001, 0002, 0011, 0012, 0111, 0112, 1111, 1112
     *
     * Now determine tactical life with an extra eye.
     */
    if (sgf_dumptree)
      sgftreeAddComment(sgf_dumptree, "Dead without extra eye.\n");
    tactical_life(1, vertices,
		  &attack_code, &num_attacks, attack_points,
		  &defense_code, &num_defenses, defense_points,
		  tactical_life_results);
    if (attack_code == 0) {
      /* Alive with extra eye.
       * Possible results: 0111, 0112, 1111, 1112
       */
      int a = 1;
      int d = 1;
      if (sgf_dumptree)
	sgftreeAddComment(sgf_dumptree, "Alive with extra eye.\n");
      num_moves = eyegraph_order_moves(vertices, BLACK, moves);
      for (k = 0; k < num_moves; k++) {
	int acode, dcode;
	int move = moves[k];
	if (eyegraph_trymove(move, BLACK, "evaluate_eyespace-C", NO_MOVE,
			     vertices)) {
	  tactical_life(1, vertices, &acode, NULL, NULL,
			&dcode, NULL, NULL, tactical_life_results);
	  if (acode != 0) {
	    evaluate_eyespace(&result2, vertices,
			      &num_vital_attacks2, vital_attacks2,
			      &num_vital_defenses2, vital_defenses2,
			      tactical_life_results);
	    /* This is either 0011 or 0012. Only the first is acceptable. */
	    if (max_eye_threat(&result2) == 1) {
	      vital_attacks[(*num_vital_attacks)++] = move;
	      a = 0;
	      if (sgf_dumptree)
		sgftreeAddComment(sgf_dumptree, "Attacking ko threat.\n");
	    }
	  }
	  eyegraph_popgo();
	}
      }
      
      num_moves = eyegraph_order_moves(vertices, WHITE, moves);
      for (k = 0; k < num_moves; k++) {
	int acode, dcode;
	int move = moves[k];
	if (eyegraph_trymove(move, WHITE, "evaluate_eyespace-D", NO_MOVE,
			     vertices)) {
	  tactical_life(0, vertices, &acode, NULL, NULL,
			&dcode, NULL, NULL, tactical_life_results);
	  if (dcode != 0) {
	    evaluate_eyespace(&result2, vertices,
			      &num_vital_attacks2, vital_attacks2,
			      &num_vital_defenses2, vital_defenses2,
			      tactical_life_results);
	    /* This is either 1122 or 0122. Only the first is acceptable. */
	    if (min_eye_threat(&result2) == 1) {
	      vital_defenses[(*num_vital_defenses)++] = move;
	      d = 2;
	      if (sgf_dumptree)
		sgftreeAddComment(sgf_dumptree, "Defending ko threat.\n");
	    }
	  }
	  eyegraph_popgo();
	}
      }
      set_eyevalue(result, a, 1, 1, d);
      if (sgf_dumptree) {
	if (a == 0 && d == 1)
	  sgftreeAddComment(sgf_dumptree, "Eyevalue 0111.\n");
	else if (a == 0 && d == 2)
	  sgftreeAddComment(sgf_dumptree, "Eyevalue 0112.\n");
	else if (a == 1 && d == 1)
	  sgftreeAddComment(sgf_dumptree, "Eyevalue 1111.\n");
	else
	  sgftreeAddComment(sgf_dumptree, "Eyevalue 1112.\n");
      }
    }
    else if (defense_code != 0) {
      /* Critical with extra eye.
       * Possible results: 0011, 0012
       */
      int d = 1;
      if (sgf_dumptree)
	sgftreeAddComment(sgf_dumptree, "Critical with extra eye.\n");
      for (k = 0; k < num_attacks; k++)
	vital_attacks[(*num_vital_attacks)++] = attack_points[k];
      for (k = 0; k < num_defenses; k++) {
	int move = defense_points[k];
	if (eyegraph_trymove(move, WHITE, "evaluate_eyespace-E", NO_MOVE,
			     vertices)) {
	  evaluate_eyespace(&result2, vertices,
			    &num_vital_attacks2, vital_attacks2,
			    &num_vital_defenses2, vital_defenses2,
			    tactical_life_results);
	  /* If result2 is 1122 for some move we have 0012 as final
           * result, otherwise 0011.
	   */
	  if (min_eye_threat(&result2) == 1
	      && min_eyes(&result2) == 1
	      && max_eyes(&result2) == 2) {
	    if (d == 1)
	      *num_vital_defenses = 0;
	    d = 2;
	    vital_defenses[(*num_vital_defenses)++] = move;
	  }
	  else if (d == 1)
	    vital_defenses[(*num_vital_defenses)++] = move;
	  eyegraph_popgo();
	}
      }
      set_eyevalue(result, 0, 0, 1, d);
      if (sgf_dumptree) {
	if (d == 1)
	  sgftreeAddComment(sgf_dumptree, "Eyevalue: 0011.\n");
	else
	  sgftreeAddComment(sgf_dumptree, "Eyevalue: 0012.\n");
      }	  
    }
    else {
      /* Dead with extra eye.
       * Possible results: 0000, 0001, 0002
       *
       * Determine whether there are ko threats and how serious.
       */
      int d = 0;
      if (sgf_dumptree)
	sgftreeAddComment(sgf_dumptree, "Dead with extra eye.\n");
      num_moves = eyegraph_order_moves(vertices, WHITE, moves);
      for (k = 0; k < num_moves; k++) {
	int acode, dcode;
	int move = moves[k];
	if (eyegraph_trymove(move, WHITE, "evaluate_eyespace-F", NO_MOVE,
			     vertices)) {
	  tactical_life(1, vertices, &acode, NULL, NULL,
			&dcode, NULL, NULL, tactical_life_results);
	  if (dcode != 0) {
	    tactical_life(0, vertices, &acode, NULL, NULL,
			  &dcode, NULL, NULL, tactical_life_results);
	    if (dcode != 0) {
	      if (d == 1)
		*num_vital_defenses = 0;
	      d = 2;
	      vital_defenses[(*num_vital_defenses)++] = move;
	      if (sgf_dumptree)
		sgftreeAddComment(sgf_dumptree,
				  "Ko threat to make two eyes.\n");
	    }
	    else {
	      if (d != 2) {
		vital_defenses[(*num_vital_defenses)++] = move;
		d = 1;
	      }
	      if (sgf_dumptree)
		sgftreeAddComment(sgf_dumptree,
				  "Ko threat to make one eye.\n");
	    }
	  }
	  eyegraph_popgo();
	}
      }
      set_eyevalue(result, 0, 0, 0, d);
      if (sgf_dumptree) {
	if (d == 0)
	  sgftreeAddComment(sgf_dumptree, "Eyevalue 0000.\n");
	else if (d == 1)
	  sgftreeAddComment(sgf_dumptree, "Eyevalue 0001.\n");
	else
	  sgftreeAddComment(sgf_dumptree, "Eyevalue 0002.\n");
      }
    }
  }
}

/* Add small invincible black groups in contact with the marginal
 * vertices, without destroying the connectivity of the white stones.
 *
 */
static int
add_margins(int num_margins, int *margins, int mx[BOARDMAX])
{
  int k;
  int i, j;
  int old_mx[BOARDMAX];
  int pos;

  if (num_margins == 0)
    return 1;
  
  memcpy(old_mx, mx, sizeof(old_mx));
  
  pos = margins[num_margins - 1];

  for (k = 0; k < 4; k++) {
    int up = delta[k];
    int right = delta[(k + 1) % 4];
    
    if (!ON_BOARD(pos + up))
      continue;
    
    if (mx[pos + up] == WHITE
	&& (!ON_BOARD(pos + up + right) || mx[pos + up + right] == WHITE)
	&& (!ON_BOARD(pos + up - right) || mx[pos + up - right] == WHITE)) {
      for (i = -3; i <= 0; i++) {
	for (j = 2; j < 6; j++) {
	  if (white_area(mx, pos + j * up + i * right, up, right, pos, j)) {
	    int s = 1;
	    while (mx[pos + s * up] == WHITE) {
	      mx[pos + s * up] = BLACK;
	      s++;
	    }
	    if (add_margins(num_margins - 1, margins, mx))
	      return 1;
	    else
	      memcpy(mx, old_mx, sizeof(old_mx));
	  }
	}
      }
    }
  }

  return 0;
}

/* Analyze an eye graph to determine the eye value and vital moves.
 *
 * The eye graph is given by a string which is encoded with "%" for
 * newlines and "O" for spaces. E.g., the eye graph
 *
 *   !
 *  .X
 * !...
 *
 * is encoded as "OO!%O.X%!...". (The encoding is needed for the GTP
 * interface to this function.)
 *
 * The result is an eye value and a (nonencoded) pattern showing the
 * vital moves, using the same notation as eyes.db. In the example above
 * we would get the eye value 0112 and the graph (showing ko threat moves)
 *
 *   @  
 *  .X  
 * !.*.
 *
 * If the eye graph cannot be realized, 0 is returned, 1 otherwise.
 *
 * The parameter outer_liberties determines how many external
 * liberties will be attached, excluding the one which is sometimes
 * provided by a second eye. Up to five outer liberties can be provided.
 *
 * The parameter ko_threats tells how many ko_threats one of the
 * players is allowed to ignore. Positive values mean that the
 * defender is allowed to ignore ko threats and negative that the
 * attacker can do so. The other player is allowed to spend a move to
 * reduce this number, which is usually not the best thing to do with
 * a move but allows a correct understanding of the bent four in the
 * corner shapes. At most 4 ignored ko threats are considered.
 */
int
analyze_eyegraph(const char *coded_eyegraph, struct eyevalue *value,
		 char *analyzed_eyegraph, int outer_liberties,
		 int ko_threats)
{
  int k;
  int i, j;
  int mini, minj;
  int mx[BOARDMAX];
  char mg[BOARDMAX];
  int pos;
  int corner;
  int up;
  int right;

  int num_vital_attacks;
  int vital_attacks[BOARDMAX]; /* Way larger than necessary. */
  int num_vital_defenses;
  int vital_defenses[BOARDMAX]; /* Way larger than necessary. */
  
  int maxwidth;
  int current_width;
  int num_rows;
  int horizontal_edge;
  int vertical_edge;

  int num_margins;
  int margins[BOARDMAX]; /* Way larger than necessary. */

  struct vertex_data vertices;

  int table_size;
  unsigned char *tactical_life_results;

  if (0)
    gprintf("Analyze eyegraph %s\n", coded_eyegraph);

  gg_assert(outer_liberties >= 0 && outer_liberties <= 5);
  gg_assert(ko_threats >= -4 && ko_threats <= 4);

  if (ko_threats > 0)
    vertices.komaster = WHITE;
  else if (ko_threats == 0)
    vertices.komaster = EMPTY;
  else {
    vertices.komaster = BLACK;
    ko_threats = -ko_threats;
  }
  
  /* Mark the eyespace in the mx array. We construct the position in
   * the mx array and copy it to the actual board later.
   */
  for (pos = BOARDMIN; pos < BOARDMAX; pos++)
    if (ON_BOARD(pos))
      mx[pos] = WHITE;

  /* Find out the size of the eye graph pattern so that we can center
   * it properly.
   */
  maxwidth = 0;
  current_width = 0;
  num_rows = 1;
  horizontal_edge = -1;
  vertical_edge = -1;
  for (k = 0; k < (int) strlen(coded_eyegraph); k++) {
    if (coded_eyegraph[k] == '\n')
      continue;
    if (coded_eyegraph[k] == ' ')
      continue;
    if (coded_eyegraph[k] == '\t')
      continue;
    if (coded_eyegraph[k] == '%') {
      num_rows++;
      if (current_width > maxwidth)
	maxwidth = current_width;
      current_width = 0;
    }
    else {
      if (coded_eyegraph[k] == '-')
	horizontal_edge = num_rows - 1;
      else if (coded_eyegraph[k] == '|')
	vertical_edge = current_width;
      current_width++;
    }
  }
  if (current_width > maxwidth)
    maxwidth = current_width;

  /* Cut out the eyespace from the solid white string. */
  num_margins = 0;
  vertices.num = 0;
  
  if (horizontal_edge == 0)
    mini = -1;
  else if (horizontal_edge > 0)
    mini = board_size - num_rows + 1;
  else
    mini = (board_size - num_rows) / 2;

  if (vertical_edge == 0)
    minj = -1;
  else if (vertical_edge > 0)
    minj = board_size - maxwidth + 1;
  else
    minj = (board_size - maxwidth) / 2;
  
  i = mini;
  j = minj;
  for (k = 0; k < (int) strlen(coded_eyegraph); k++) {
    char c = coded_eyegraph[k];
    if (c == '\n')
      continue;
    if (c == ' ')
      continue;
    if (c == '\t')
      continue;
    if (c == '%') {
      i++;
      j = minj - 1;
    }
    else if (c == 'X' || c == '$')
      mx[POS(i, j)] = BLACK;
    else if (c == '.' || c == '*' || c == '<' || c == '>'
	     || c == '!' || c == '@' || c == '(' || c == ')')
      mx[POS(i, j)] = EMPTY;
    if (c == '!' || c == '@' || c == '(' || c == ')' || c == '$')
      margins[num_margins++] = POS(i, j);
    if (c != '|' && c != '-' && c != '+' && c != '%'
	&& ON_BOARD(POS(i, j)) && mx[POS(i, j)] != WHITE) {
      vertices.type[vertices.num] = REGULAR;
      vertices.pos[vertices.num++] = POS(i, j);
    }
    j++;
  }

  vertices.num_ordinary = vertices.num;

  /* Add an invincible black group in the lower left corner plus outer
   * liberties for the white string and space for virtual ko threats.
   * However, if the eyespace is placed in or near the lower left
   * corner, we put this group in the upper right instead.
   *
   * |aaaaa
   * |XXXXX
   * |.X.X
   * |XXXXX
   * |bbbbX
   * +-----
   *
   * a - up to five outer liberties (empty if active, otherwise O)
   * b - up to four virtual ko threats (always empty)
   */
  corner = POS(board_size - 1, 0);
  up = DELTA(-1, 0);
  right = DELTA(0, 1);
  if ((vertical_edge == 0 && horizontal_edge != 0)
      || (horizontal_edge > 0 && vertical_edge <= 0)) {
    corner = POS(0, board_size - 1);
    up = DELTA(1, 0);
    right = DELTA(0, -1);
  }

#define LOCAL_COORD(i, j) corner + i * up + j * right

  mx[LOCAL_COORD(0, 4)] = BLACK;
  mx[LOCAL_COORD(1, 0)] = BLACK;
  mx[LOCAL_COORD(1, 1)] = BLACK;
  mx[LOCAL_COORD(1, 2)] = BLACK;
  mx[LOCAL_COORD(1, 3)] = BLACK;
  mx[LOCAL_COORD(1, 4)] = BLACK;
  mx[LOCAL_COORD(2, 1)] = BLACK;
  mx[LOCAL_COORD(2, 3)] = BLACK;
  mx[LOCAL_COORD(3, 0)] = BLACK;
  mx[LOCAL_COORD(3, 1)] = BLACK;
  mx[LOCAL_COORD(3, 2)] = BLACK;
  mx[LOCAL_COORD(3, 3)] = BLACK;
  mx[LOCAL_COORD(3, 4)] = BLACK;
  mx[LOCAL_COORD(2, 0)] = EMPTY;
  mx[LOCAL_COORD(2, 2)] = EMPTY;

  /* Virtual ko threat points. */
  mx[LOCAL_COORD(0, 0)] = EMPTY;
  mx[LOCAL_COORD(0, 1)] = EMPTY;
  mx[LOCAL_COORD(0, 2)] = EMPTY;
  mx[LOCAL_COORD(0, 3)] = EMPTY;

  vertices.num_ko_threats = 0;
  for (k = 0; k < ko_threats; k++) {
    vertices.type[vertices.num] = KO_THREAT;
    vertices.pos[vertices.num++] = LOCAL_COORD(0, k);
    vertices.ko_threat_pos[vertices.num_ko_threats++] = LOCAL_COORD(0, k);
  }

  /* Outer liberties. */
  for (k = 0; k < outer_liberties; k++) {
    vertices.type[vertices.num] = OUTER_LIBERTY;
    vertices.pos[vertices.num++] = LOCAL_COORD(4, k);
    mx[LOCAL_COORD(4, k)] = EMPTY;
  }

  /* Add an extra eye in the upper left corner unless it is occupied.
   * In that case place the extra eye in the lower right instead.
   * Also pick up a vertex belonging to the large white string.
   */
  if (vertical_edge != 0 || horizontal_edge != 0) {
    vertices.extra_eye_pos = POS(0, 0);
    vertices.string = POS(1, 0);
  }
  else {
    vertices.extra_eye_pos = POS(board_size - 1, board_size - 1);
    vertices.string = POS(board_size - 2, board_size - 1);
  }

  mx[vertices.extra_eye_pos] = EMPTY;
  vertices.type[vertices.num] = EXTRA_EYE;
  vertices.pos[vertices.num++] = vertices.extra_eye_pos;

  if (!add_margins(num_margins, margins, mx))
    return 0;

  /* Copy the mx array over to the board. */
  clear_board();
  for (pos = BOARDMIN; pos < BOARDMAX; pos++)
    if (ON_BOARD(pos)) {
      if (mx[pos] == WHITE)
	add_stone(pos, WHITE);
      else if (mx[pos] == BLACK)
	add_stone(pos, BLACK);
    }

  if (verbose)
    showboard(0);

  /* If there are any isolated O stones, those should also be added to
   * the playable vertices.
   */
  for (pos = BOARDMIN; pos < BOARDMAX; pos++)
    if (board[pos] == WHITE && !same_string(pos, vertices.string)) {
      int k;
      for (k = vertices.num; vertices.type[k - 1] != REGULAR; k--) {
	vertices.type[k] = vertices.type[k - 1];
	vertices.pos[k] = vertices.pos[k - 1];
      }
      vertices.type[k] = REGULAR;
      vertices.pos[k] = pos;
      vertices.num++;
    }

  if (verbose) {
    int k;
    gprintf("\nPlayable vertices:\n");
    for (k = 0; k < vertices.num; k++)
      gprintf("%1m(%d) ", vertices.pos[k], vertices.type[k]);
    gprintf("\n\n");
  }
  
  /* Disable this test if you need to evaluate larger eyespaces, have
   * no shortage of memory, and know what you're doing.
   */
  if (vertices.num > 17) {
    gprintf("analyze_eyegraph: too large eyespace, %d vertices\n",
	    vertices.num);
    gg_assert(vertices.num <= 17);
  }

  /* The cache must have 2*3^vertices.num entries. */
  table_size = 2;
  for (k = 0; k < vertices.num; k++)
    table_size *= 3;

  /* Allocate memory for the cache. */
  tactical_life_results = malloc(table_size);
  if (!tactical_life_results) {
    gprintf("analyze_eyegraph: failed to allocate %d bytes\n", table_size);
    gg_assert(tactical_life_results != NULL);
  }
  memset(tactical_life_results, 0, table_size);

  if (sgf_dumptree)
    sgffile_printboard(sgf_dumptree);
  
  /* Evaluate the eyespace on the board. */
  evaluate_eyespace(value, &vertices,
		    &num_vital_attacks, vital_attacks,
		    &num_vital_defenses, vital_defenses,
		    tactical_life_results);

  /* Return the cache memory. */
  free(tactical_life_results);

  if (verbose) {
    gprintf("Eyevalue: %s\n", eyevalue_to_string(value));
    for (k = 0; k < num_vital_attacks; k++)
      gprintf("  vital attack point %1m\n", vital_attacks[k]);
    for (k = 0; k < num_vital_defenses; k++)
      gprintf("  vital defense point %1m\n", vital_defenses[k]);
  }

  /* Encode the attack and defense points with symbols in the mg[] array. */
  memset(mg, ' ', sizeof(mg));

  for (k = 0; k < vertices.num_ordinary; k++)
    mg[vertices.pos[k]] = (board[vertices.pos[k]] == BLACK ? 'X' : '.');

  for (k = 0; k < num_margins; k++)
    mg[margins[k]] = (mg[margins[k]] == 'X' ? '$' : '!');

  for (k = 0; k < num_vital_attacks; k++)
    mg[vital_attacks[k]] = (mg[vital_attacks[k]] == '!' ? '(' : '<');

  for (k = 0; k < num_vital_defenses; k++) {
    int pos = vital_defenses[k];
    if (mg[pos] == '.')
      mg[pos] = '>';
    else if (mg[pos] == '!')
      mg[pos] = ')';
    else if (mg[pos] == '<')
      mg[pos] = '*';
    else if (mg[pos] == '(')
      mg[pos] = '@';
  }

  /* Return the central part of the mg[] array (corresponding to the
   * input eye graph).
   */
  k = 0;
  for (i = mini; i < mini + num_rows; i++) {
    for (j = minj; j < minj + maxwidth; j++) {
      if ((i < 0 || i >= board_size) && (j < 0 || j >= board_size))
	analyzed_eyegraph[k++] = '+';
      else if (i < 0 || i >= board_size)
	analyzed_eyegraph[k++] = '-';
      else if (j < 0 || j >= board_size)
	analyzed_eyegraph[k++] = '|';
      else
	analyzed_eyegraph[k++] = mg[POS(i, j)];
    }
    analyzed_eyegraph[k++] = '\n';
  }
  analyzed_eyegraph[k - 1] = 0;
  
  return 1;
}


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
