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
		    int add_moves, int color);
static int recognize_eye(int pos, int *attack_point, int *defense_point,
			 struct eyevalue *value,
			 struct eye_data eye[BOARDMAX],
			 struct half_eye_data heye[BOARDMAX],
			 struct vital_points *vp);
static void guess_eye_space(int pos, int effective_eyesize, int margins,
			    struct eye_data eye[BOARDMAX],
			    struct eyevalue *value, int *pessimistic_min);
static void reset_map(int size);
static void first_map(int* map_value);
static int next_map(int *q, int map[MAXEYE]);
static void print_eye(struct eye_data eye[BOARDMAX],
		      struct half_eye_data heye[BOARDMAX], int pos);
static float 
evaluate_diagonal_intersection(int m, int n, int color,
			       int *attack_point, int *defense_point,
			       struct eye_data my_eye[BOARDMAX]);


/* These are used during the calculations of eye spaces. */
static int black_domain[BOARDMAX];
static int white_domain[BOARDMAX];

/* Used internally by mapping functions. */
static int map_size;
static char used_index[MAXEYE];


/*
 * Clear a struct eye_data.
 */

static void
clear_eye(struct eye_data *eye)
{
  eye->color = 0;
  eye->esize = 0;
  eye->msize = 0;
  eye->origin = NO_MOVE;
  set_eyevalue(&eye->value, 0, 0, 0, 0);
  eye->attack_point = NO_MOVE;
  eye->defense_point = NO_MOVE;
  eye->marginal = 0;
  eye->type = 0;
  eye->neighbors = 0;
  eye->marginal_neighbors = 0;
  eye->cut = 0;
}


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
  int i, j;
  int k;
  int pos;
  int lively[BOARDMAX];
  int false_margins[BOARDMAX];
  
  memset(black_domain, 0, sizeof(black_domain));
  memset(white_domain, 0, sizeof(white_domain));
  memset(false_margins, 0, sizeof(false_margins));

  /* Initialize eye data and compute the lively array. */
  for (pos = BOARDMIN; pos < BOARDMAX; pos++)
    if (ON_BOARD(pos)) {
      if (b_eye)
        clear_eye(&(b_eye[pos]));
      if (w_eye)
        clear_eye(&(w_eye[pos]));
      lively[pos] = is_lively(owl_call, pos);
    }

  /* Compute the domains of influence of each color. */
  compute_primary_domains(BLACK, black_domain, lively, false_margins, 1);
  compute_primary_domains(WHITE, white_domain, lively, false_margins, 0);

  /* Now we fill out the arrays b_eye and w_eye with data describing
   * each eye shape.
   */

  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++) {
      pos = POS(i, j);
      if (board[pos] == EMPTY || !lively[pos]) {
	if (black_domain[pos] == 0 && white_domain[pos] == 0) {
	  if (w_eye)
	    w_eye[pos].color = GRAY;
	  if (b_eye)
	    b_eye[pos].color = GRAY;
	}
	else if (black_domain[pos] == 1 && white_domain[pos] == 0 && b_eye) {
	  b_eye[pos].color = BLACK_BORDER;
	  for (k = 0; k < 4; k++) {
	    int apos = pos + delta[k];
	    if (ON_BOARD(apos) && white_domain[apos] && !black_domain[apos]) {
	      b_eye[pos].marginal = 1;
	      break;
	    }
	  }
	}
	else if (black_domain[pos] == 0 && white_domain[pos] == 1 && w_eye) {
	  w_eye[pos].color = WHITE_BORDER;
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
		b_eye[pos].color = BLACK_BORDER;
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
		w_eye[pos].color = WHITE_BORDER;
		break;
	      }
	    }
	    if (k == 4)
	      w_eye[pos].color = GRAY;
	  }
	}
      }
    }

  /* 
   * If called from make_dragons, search connection database for cutting
   * points, which may modify the eyespace in order to avoid amalgamation and
   * reflect the weakness in the position. The following test fails
   * if called from the owl code.
   */
  if (!owl_call)
    find_cuts();
  
 /* The eye spaces are all found. Now we need to find the origins. */
  if (b_eye)
    for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
      if (!ON_BOARD(pos))
	continue;
      if (b_eye[pos].origin == NO_MOVE 
	  && b_eye[pos].color == BLACK_BORDER)
      {
	int esize = 0;
	int msize = 0;

	originate_eye(pos, pos, &esize, &msize, b_eye);
	b_eye[pos].esize = esize;
	b_eye[pos].msize = msize;
      }
    }

  if (w_eye)
    for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
      if (!ON_BOARD(pos))
	continue;
      if (w_eye[pos].origin == NO_MOVE
	  && w_eye[pos].color == WHITE_BORDER)
      {
	int esize = 0;
	int msize = 0;

	originate_eye(pos, pos, &esize, &msize, w_eye);
	w_eye[pos].esize = esize;
	w_eye[pos].msize = msize;
      }
    }

  /* Now we count the number of neighbors and marginal neighbors
   * of each vertex.
   */
  if (b_eye)
    count_neighbours(b_eye);
  if (w_eye)
    count_neighbours(w_eye);
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
  char threshold[BOARDMAX];
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
      ||  worm[pos].defense_codes[0] != 0));
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
    TRACE_EYES("False margin for %C at %1m.\n", color, pos);
    return 1;
  }

  return 0;
}


/*
 * originate_eye(pos, pos, *size) creates an eyeshape with origin (pos).
 * the last variable returns the size. The repeated variables (pos) are due
 * to the recursive definition of the function.
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

  if (!experimental_connections)
    if (eye[pos].type & INHIBIT_CONNECTION)
      return;
  
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
      eye[pos].attack_point  = eye[origin].attack_point;
      eye[pos].defense_point = eye[origin].defense_point;
    }
}


/* Find the dragon or dragons surrounding an eye space. Up to
   max_dragons dragons adjacent to the eye space are added to
   the dragon array, and the number of dragons found is returned.
 */

int
find_eye_dragons(int origin, struct eye_data eye[BOARDMAX], int eye_color,
		 int dragons[], int max_dragons)
{
  int mx[BOARDMAX];
  int num_dragons = 0;
  int pos;

  memset(mx, 0, sizeof(mx));
  TRACE_MISCELLANEOUS("find_eye_dragons: %1m %C\n", origin, eye_color);
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
      TRACE_MISCELLANEOUS(
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
  int mini, maxi;
  int minj, maxj;
  int origin = eye[pos].origin;

  gprintf("Eyespace at %1m: color=%C, esize=%d, msize=%d\n",
	  pos, eye[pos].color, eye[pos].esize, eye[pos].msize);
  
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      int pos2 = POS(m, n);
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
	     struct half_eye_data heye[BOARDMAX],
	     int add_moves, int color)
{
  if (attack_point)
    *attack_point = NO_MOVE;
  if (defense_point)
    *defense_point = NO_MOVE;

  if (debug & DEBUG_EYES) {
    print_eye(eye, heye, pos);
    TRACE_EYES("\n");
  }
  
  /* Look up the eye space in the graphs database. */
  if (read_eye(pos, attack_point, defense_point, value,
	       eye, heye, add_moves, color))
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
			 struct half_eye_data heye[BOARDMAX])
{
  int m, n;
  int margins = 0;
  int halfeyes = 0;
  int margins_adjacent_to_margin = 0;
  int effective_eyesize;

  /* Stones inside eyespace which do not coincide with a false eye or
   * a halfeye.
   */
  int interior_stones = 0;

  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      int pos2 = POS(m, n);
      if (eye[pos2].origin != pos)
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
    TRACE_EYES("\n");
  }
  
  /* Look up the eye space in the graphs database. */
  if (read_eye(pos, attack_point, defense_point, value,
	       eye, heye, 0, EMPTY)) {
    *pessimistic_min = min_eyes(value) - margins;

    /* A single point eye which is part of a ko can't be trusted. */
    if (eye[pos].esize == 1
	&& is_ko(pos, eye[pos].color == WHITE_BORDER ? BLACK : WHITE, NULL))
      *pessimistic_min = 0;

    TRACE_EYES("  graph matching - %s, pessimistic_min=%d\n",
	  eyevalue_to_string(value), *pessimistic_min);
  }
  
  /* Ideally any eye space that hasn't been matched yet should be two
   * secure eyes. Until the database becomes more complete we have
   * some additional heuristics to guess the values of unknown
   * eyespaces.
   */
  else {
    guess_eye_space(pos, effective_eyesize, margins, eye,
		    value, pessimistic_min); 
    TRACE_EYES("  guess_eye - %s, pessimistic_min=%d\n",
	  eyevalue_to_string(value), *pessimistic_min);
  }

  if (*pessimistic_min < 0) {
    *pessimistic_min = 0;
    TRACE_EYES("  pessimistic min revised to 0\n");
  }
  
  /* An eyespace with at least two interior stones is assumed to be
   * worth at least one eye, regardless of previous considerations.
   */
  if (*pessimistic_min < 1 && interior_stones >= 2) {
    *pessimistic_min = 1;
    TRACE_EYES("  pessimistic min revised to 1 (interior stones)\n");
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
    int pos2;
    
    for (pos2 = BOARDMIN; pos2 < BOARDMAX; pos2++) {
      if (ON_BOARD(pos2) && eye[pos2].origin == pos) {
	float this_score = 0.0;
	int this_attack_point = NO_MOVE;
	int this_defense_point = NO_MOVE;
	if (eye[pos2].marginal && board[pos2] == EMPTY) {
	  this_score = eye[pos2].neighbors;
	  this_attack_point = pos2;
	  this_defense_point = pos2;

	  if (is_self_atari(pos2,
			    eye[pos].color == WHITE_BORDER ? BLACK : WHITE))
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
		struct eye_data eye[BOARDMAX],
		struct eyevalue *value, int *pessimistic_min)
{
  if (effective_eyesize > 3) {
    set_eyevalue(value, 2, 2, 2, 2);
    if ((margins == 0 && effective_eyesize > 7)
	|| (margins > 0 && effective_eyesize > 9)) {
      int eyes = 2 + (effective_eyesize - 2 * (margins > 0) - 8) / 2;
      *pessimistic_min = eyes;
      set_eyevalue(value, eyes, eyes, eyes, eyes);
    }
    else
      *pessimistic_min = 1;
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
 * recognize_eye(). Currently, its only purpose is to read positions
 * like this:
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
 * This function tries to turn such ko-depended half eyes into proper
 * eyes and chooses the best alternative. Note that we don't have any
 * attack/defense codes here, since owl will determine them itself.
 *
 * If add_moves != 0, this function may add move reasons for (color)
 * at the vital points which are found by recognize_eye(). If add_moves 
 * == 0, set color to be EMPTY.
 */
static int
read_eye(int pos, int *attack_point, int *defense_point,
	 struct eyevalue *value, struct eye_data eye[BOARDMAX], 
	 struct half_eye_data heye[BOARDMAX], 
	 int add_moves, int color)
{
  int eye_color;
  int k;
  int pos2;
  int ko_halfeye = NO_MOVE;
  int apos = NO_MOVE, dpos = NO_MOVE;
  struct eyevalue ko_value;
  struct vital_points vp;
  struct vital_points ko_vp;
  struct vital_points *best_vp = &vp;
  
  eye_color = recognize_eye(pos, attack_point, defense_point, value,
                            eye, heye, &vp);
  if (!eye_color)
    return 0;

  for (pos2 = BOARDMIN; pos2 < BOARDMAX; pos2++)
    if (ON_BOARD(pos2)
	&& eye[pos2].origin == pos
        && heye[pos2].type == HALF_EYE && heye[pos2].value < 3.0) {
      if (ko_halfeye != NO_MOVE) {
	ko_halfeye = NO_MOVE;   /* We can't win two kos at once. */
	break;            
      }
      
      ko_halfeye = pos2;
    }

  if (ko_halfeye != NO_MOVE) {
    int result;

    heye[ko_halfeye].type = 0;
    result = recognize_eye(pos, &apos, &dpos, &ko_value, eye,
			   heye, &ko_vp);
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
    if (eye_color == color) {
      for (k = 0; k < best_vp->num_defenses; k++)
	add_vital_eye_move(best_vp->defenses[k], pos, eye_color);
    }
    else {
      for (k = 0; k < best_vp->num_attacks; k++)
	add_vital_eye_move(best_vp->attacks[k], pos, eye_color);
    }
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
	      struct vital_points *vp)
{
  int m, n;
  int eye_color;
  int eye_size = 0;
  int num_marginals = 0;
  int vpos[MAXEYE];
  char marginal[MAXEYE], edge[MAXEYE], neighbors[MAXEYE];
  int graph;
  int map[MAXEYE];

  gg_assert(attack_point != NULL);
  gg_assert(defense_point != NULL);
    
  /* Set `eye_color' to the owner of the eye. */
  eye_color = eye[pos].color;
  if (eye_color == BLACK_BORDER)
    eye_color = BLACK;
  if (eye_color == WHITE_BORDER)
    eye_color = WHITE;

  if (eye[pos].esize-eye[pos].msize > 7)
    return 0;

  if (eye[pos].msize > MAXEYE)
    return 0;

  /* Create list of eye vertices */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      int pos2 = POS(m, n);
      if (eye[pos2].origin == pos) {
	vpos[eye_size] = pos2;
	marginal[eye_size] = eye[pos2].marginal;
	if (marginal[eye_size])
	  num_marginals++;
	neighbors[eye_size] = eye[pos2].neighbors;
	if (0) {
	  if (marginal[eye_size])
	    TRACE("(%1m)", vpos[eye_size]);
	  else
	    TRACE(" %1m ", vpos[eye_size]);
	  TRACE("\n");
	}

	edge[eye_size] = 0;
	if (m == 0 || m == board_size-1)
	  edge[eye_size]++;
	if (n == 0 || n == board_size-1)
	  edge[eye_size]++;
	
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
	}

	eye_size++;
      }
    }

  /* We attempt to construct a map from the graph to the eyespace
   * preserving the adjacency structure. If this can be done, we've
   * identified the eyeshape.
   */

  for (graph = 0; graphs[graph].vertex != NULL; graph++) {
    int q;

    if (graphs[graph].esize != eye_size
	|| graphs[graph].msize != num_marginals)
      continue;

    reset_map(eye_size);
    q = 0;
    first_map(&map[0]);

    while (1) {
      struct eye_vertex *gv = &graphs[graph].vertex[q];
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
        if (IS_STONE(board[vpos[mv]])) {
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

    if (q == eye_size) {
      /* We have found a match! Now sort out the vital moves. */
      *value = graphs[graph].value;
      vp->num_attacks = 0;
      vp->num_defenses = 0;
      
      if (eye_move_urgency(value) > 0) {
	/* Collect all attack and defense points in the pattern. */
	int k;

	for (k = 0; k < eye_size; k++) {
	  struct eye_vertex *ev = &graphs[graph].vertex[k];

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
	      
	      for (ix = 0; ix < he->num_defends; ix++)
		vp->defenses[vp->num_defenses++] = he->defense_point[ix];
	    }
	    else
	      vp->defenses[vp->num_defenses++] = vpos[map[k]];
	  }
	}
	
	gg_assert(vp->num_attacks > 0 && vp->num_defenses > 0);

	*attack_point = vp->attacks[0];
	/* If possible, choose a non-sacrificial defense point.
         * Compare white T8 and T6 in lazarus:21.
	 */
	*defense_point = vp->defenses[0];
	for (k = 0; k < vp->num_defenses; k++) {
	  if (safe_move(vp->defenses[k], eye_color) == WIN) {
	    *defense_point = vp->defenses[k];
	    break;
	  }
	}
	
	TRACE_EYES("  vital points: %1m (attack) %1m (defense)\n",
	      *attack_point, *defense_point);
	TRACE_EYES("  pattern matched:  %d\n", graphs[graph].patnum);
	
      }
      TRACE("eye space at %1m of type %d\n", pos, graphs[graph].patnum);
      
      return eye_color;
    }
  }

  return 0;
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

void
add_false_eye(int pos, struct eye_data eye[BOARDMAX],
	      struct half_eye_data heye[BOARDMAX])
{
  int k;
  ASSERT1(heye[pos].type == FALSE_EYE, pos);
  TRACE_EYES("false eye found at %1m\n", pos);

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
  return (white_eye[pos].color == WHITE_BORDER
	  || black_eye[pos].color == BLACK_BORDER);
}

int
is_proper_eye_space(int pos)
{
  return ((white_eye[pos].color == WHITE_BORDER
	   && !white_eye[pos].marginal)
	  || (black_eye[pos].color == BLACK_BORDER
	      && !black_eye[pos].marginal));
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
  
  if (white_eye[pos].color == WHITE_BORDER)
    max_white = max_eyes(&white_eye[pos].value);

  if (black_eye[pos].color == BLACK_BORDER)
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

float
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
    val = evaluate_diagonal_intersection(I(pos) + deltai[k],
					 J(pos) + deltaj[k], color,
					 &attack_point, &defense_point, 
					 my_eye);
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
	    int tmp_point = heye[pos].attack_point[r];
	    attack_values[r] = attack_value;
	    heye[pos].attack_point[r] = attack_point;
	    attack_value = tmp_value;
	    attack_point = tmp_point;
	  }
	
	  if (defense_values[r] < defense_value) {
	    int tmp_value = defense_values[r];
	    int tmp_point = heye[pos].defense_point[r];
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

  heye[pos].num_attacks = num_attacks;
  heye[pos].num_defends = num_defenses;
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
   if (my_eye[pos].color == BORDER_COLOR(color)
       && !my_eye[pos].marginal
       && my_eye[pos].marginal_neighbors < 2
       && !(board[pos] == EMPTY && does_capture_something(pos, other)))
    return 0.0;

  if (board[pos] == EMPTY) {
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

    int your_safety = safe_move(pos, other);
    
    if (your_safety == 0)
      value = 0.0;
    else if (your_safety != WIN)
      value = a;
    else {                           /* So your_safety == WIN. */
      int our_safety = safe_move(pos, color);
      
      if (our_safety == 0)
        value = 2.0;
      else if (our_safety == WIN)
        value = 1.0;
      else                           /* our_safety depends on ko. */
        value = b;
    }

    apos = pos;
    dpos = pos;
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

/* Note: the result string is stored in a statically allocated buffer
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
    examine_position(WHITE, EXAMINE_DRAGONS_WITHOUT_OWL);

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


/* Find topological half eyes and false eyes by analyzing the
 * diagonal intersections, as described in the Texinfo
 * documentation (Eyes/Eye Topology).
 */
void
find_half_and_false_eyes(int color, struct eye_data eye[BOARDMAX],
			 struct half_eye_data heye[BOARDMAX],
			 char find_mask[BOARDMAX])
{
  int eye_color = (color == WHITE ? WHITE_BORDER : BLACK_BORDER);
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
      ASSERT1(heye[pos].num_defends > 0, pos);
      ASSERT_ON_BOARD1(heye[pos].defense_point[0]);
    }
  }
}


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
