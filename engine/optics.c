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
#include <string.h>
#include "liberty.h"
#include "eyes.h"


/* This macro is not fully generalized. It works because it is used only where
 * c, d match the first vital attack/defend point of the the half eye or none
 * at all.
 */
#define hadj(heye, apos, bpos) \
     (heye[apos].type == HALF_EYE \
      && (heye[apos].attack_point[0] == (bpos) \
          || (heye[apos].defense_point[0] == (bpos))))

/*
 * Two eye points are defined to be adjacent if they are either
 * next to each other or if one vertex is a half eye and the
 * other one is the point making it a real eye.
 */

#define adjacent(heye, apos, bpos) (   (apos) == SOUTH(bpos) \
				    || (apos) == WEST(bpos) \
				    || (apos) == NORTH(bpos) \
				    || (apos) == EAST(bpos) \
 				    || hadj(heye, apos, bpos) \
				    || hadj(heye, bpos, apos))

#define MAXEYE 20

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
static int recognize_eye(int pos, int *attack_point, int *defense_point,
			 int *max, int *min, 
			 struct eye_data eye[BOARDMAX],
			 struct half_eye_data heye[BOARDMAX],
			 int add_moves, int color);
static void guess_eye_space(int pos, int effective_eyesize, int margins,
			    struct eye_data eye[BOARDMAX],
			    int *max, int *min, int *pessimistic_min);
static void first_map(int q, int map[MAXEYE]);
static int next_map(int *q, int map[MAXEYE], int esize);
static void print_eye(struct eye_data eye[BOARDMAX],
		      struct half_eye_data heye[BOARDMAX], int pos);
static float 
evaluate_diagonal_intersection(int m, int n, int color,
			       int *attack_point, int *defense_point,
			       struct eye_data b_eye[BOARDMAX],
			       struct eye_data w_eye[BOARDMAX]);


/* These are used during the calculations of eye spaces. */
static int black_domain[BOARDMAX];
static int white_domain[BOARDMAX];


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
  eye->maxeye = 0;
  eye->mineye = 0;
  eye->attack_point = NO_MOVE;
  eye->defense_point = NO_MOVE;
  eye->dragon = NO_MOVE;
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
      clear_eye(&(b_eye[pos]));
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
	  w_eye[pos].color = GRAY;
	  b_eye[pos].color = GRAY;
	}
	else if (black_domain[pos] == 1 && white_domain[pos] == 0) {
	  b_eye[pos].color = BLACK_BORDER;
	  for (k = 0; k < 4; k++) {
	    int apos = pos + delta[k];
	    if (ON_BOARD(apos) && white_domain[apos] && !black_domain[apos]) {
	      b_eye[pos].marginal = 1;
	      break;
	    }
	  }
	}
	else if (black_domain[pos] == 0 && white_domain[pos] == 1) {
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
	  for (k = 0; k < 4; k++) {
	    int apos = pos + delta[k];
	    if (ON_BOARD(apos) && black_domain[apos] && !white_domain[apos]) {
	      b_eye[pos].marginal = 1;
	      b_eye[pos].color = BLACK_BORDER;
	      break;
	    }
	  }
	  if (k == 4)
	    b_eye[pos].color = GRAY;
	  
	  for (k = 0; k < 4; k++) {
	    int apos = pos + delta[k];
	    if (ON_BOARD(apos) && white_domain[apos] && !black_domain[apos]) {
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

  /* 
   * If called from make_dragons, search connection database for cutting
   * points, which may modify the eyespace in order to avoid amalgamation and
   * reflect the weakness in the position. The following test fails
   * if called from the owl code.
   */
  if (b_eye == black_eye)
    find_cuts();
  
 /* The eye spaces are all found. Now we need to find the origins. */
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++) {
      pos = POS(i, j);
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

  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++) {
      pos = POS(i, j);
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
  count_neighbours(b_eye);
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

#define lively_stone(pos, color) (board[pos] == color && lively[pos])
#define has_inf(color, pos) (domain[pos] || lively_stone(pos, color))
#define sufficient_influence(pos, apos, bpos) \
 (ON_BOARD(bpos) \
  && (domain[apos] + domain[bpos]) \
      > (inhibit[pos] > 1) + (inhibit[apos] > 0) + (inhibit[bpos] > 0))

static void
compute_primary_domains(int color, int domain[BOARDMAX],
			int lively[BOARDMAX],
			int false_margins[BOARDMAX],
			int first_time)
{
  int other = OTHER_COLOR(color);
  int found_one;
  int i, j;
  int pos;
  int inhibit[BOARDMAX];
  memset(inhibit, 0, sizeof(inhibit));

  /* In the first pass we
   * 1. Give influence to lively own stones and their neighbors.
   *    (Cases (1) and (2) above.)
   * 2. Set inhibit for lively opponent stones and their neighbors.
   */
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++) {
      pos = POS(i, j);
      if (lively_stone(pos, color))
	domain[pos] = 1; /* Case (1) above. */
      else if (lively_stone(pos, other))
	inhibit[pos] = 1;
      else {
	if (lively_stone(SOUTH(pos), color)
	    || lively_stone(WEST(pos), color)
	    || lively_stone(NORTH(pos), color)
	    || lively_stone(EAST(pos), color)) {
	  /* Case (2) above.
	   *
	   * To explain the asymmetry between the first time around
	   * this loop and subsequent ones, a false margin is adjacent
	   * to both B and W lively stones, so it's found on the first
	   * pass through the loop.
	   */
	  if (first_time) {
	    if (board[pos] == EMPTY && false_margin(pos, color, lively))
	      false_margins[pos] = 1;
	    else if (board[pos] == EMPTY
		     && false_margin(pos, other, lively))
	      false_margins[pos] = 1;
	    else
	      domain[pos] = 1;
	  }
	  else {
	    if (IS_STONE(board[pos]) || false_margins[pos] != 1)
	      domain[pos] = 1;
	  }
	}
	
	if (lively_stone(SOUTH(pos), other)
	    || lively_stone(WEST(pos), other)
	    || lively_stone(NORTH(pos), other)
	    || lively_stone(EAST(pos), other))
	  inhibit[pos] = 2;
	else if (is_edge_vertex(pos))
	  inhibit[pos] = 1;
      }
    }

  /* Now we loop over the board until no more vertices can be added to
   * the domain through case (3) above.
   */
  do {
    found_one = 0;
    for (i = 0; i < board_size; i++)
      for (j = 0; j < board_size; j++) {
	pos = POS(i, j);
	
	/* First we handle the trivial cases. */
	if (domain[pos] || lively_stone(pos, other) || false_margins[pos])
	  continue;

	/* Case (3) above. */
	if (sufficient_influence(pos, SOUTH(pos), SE(pos))
	    || sufficient_influence(pos, SOUTH(pos), SW(pos))
	    || sufficient_influence(pos, WEST(pos), SW(pos))
	    || sufficient_influence(pos, WEST(pos), NW(pos))
	    || sufficient_influence(pos, NORTH(pos), NW(pos))
	    || sufficient_influence(pos, NORTH(pos), NE(pos))
	    || sufficient_influence(pos, EAST(pos), NE(pos))
	    || sufficient_influence(pos, EAST(pos), SE(pos))) {
	  domain[pos] = 1;
	  found_one = 1;
	}
      }
  } while (found_one);
  
  if (0 && (debug & DEBUG_EYES)) {
    int i, j;
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
  int result;

  if (owl_call)
    result = owl_lively(pos);
  else
    result = (!worm[pos].inessential
	      && (worm[pos].attack_codes[0] == 0
		  || worm[pos].defend_codes[0] != 0));

  return result;
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
  
  /* The life code needs the false margins to remain in the eyespace. */
  if (life)
    return 0;
  
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
      eye[pos].maxeye        = eye[origin].maxeye;
      eye[pos].mineye        = eye[origin].mineye;
      eye[pos].attack_point  = eye[origin].attack_point;
      eye[pos].defense_point = eye[origin].defense_point;
      eye[pos].dragon        = eye[origin].dragon;
    }
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
	  gprintf("%1m (XH) (topological eye value = %f\n", pos2,
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
	  gprintf("%1m (H) (topological eye value = %f\n", pos2,
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
compute_eyes(int pos, int *max, int *min,
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
    DEBUG(DEBUG_EYES, "\n");
  }
  
  /* First we try to let the life code evaluate the eye space. */
  if (life && eye[pos].esize <= life_eyesize) {
    int max1, min1;
    int attack_point1;
    int defense_point1;
    int status;

    if (recognize_eye2(pos, attack_point, defense_point, max, min,
		       eye, heye, add_moves, color)) {

      /* made these printouts contingent on DEBUG_EYES /gf */
      if (debug & DEBUG_EYES) {
	fprintf(stderr, "\n");
	showboard(2);
	
	status = recognize_eye(pos, &attack_point1, &defense_point1,
			       &max1, &min1, eye, heye, 0, EMPTY);
	
	if (status) {
	  gprintf("Number of eyes:  --life: (%d, %d)  old: (%d, %d) at %1m\n", 
		  *max, *min, max1, min1, pos);
	  if (*min != *max) {
	    gprintf("  vital point:     attack: %1m   defense: %1m\n",
		    *attack_point, *defense_point);
	    gprintf("  old vital point: attack: %1m   defense: %1m\n",
		    attack_point1, defense_point1);
	  }
	}
	else {
	  gprintf("Number of eyes:  new: (%d, %d) at %1m\n", *max, *min, pos);
	  if (*min != *max)
	    gprintf("  vital point:   attack: %1m   defense: %1m\n",
		    *attack_point, *defense_point);
	}
      }
      
      return;
    }
  }

  /* Fall back on the graphs database if the eye is too big or the
   * life code is disabled.
   */
  if (recognize_eye(pos, attack_point, defense_point, max, min,
		    eye, heye, add_moves, color))
    return;

  if (eye[pos].esize < 6) {
    /* made these printouts contingent on DEBUG_EYES /gf */
    if (debug & DEBUG_EYES) {
      gprintf("===========================================================\n");
      gprintf("Unrecognized eye of size %d shape at %1m\n", 
	      eye[pos].esize, pos);
      print_eye(eye, heye, pos);
    }
  }

  /* Ideally any eye space that hasn't been matched yet should be two
   * secure eyes. Until the database becomes more complete we have
   * some additional heuristics to guess the values of unknown
   * eyespaces.
   */
  if (eye[pos].esize-2*eye[pos].msize > 3) {
    *min = 2;
    *max = 2;
  }
  else if (eye[pos].esize-2*eye[pos].msize > 0) {
    *min = 1;
    *max = 1;
  }
  else {
    *min = 0;
    *max = 0;
  }
}


/*
 * This function works like compute_eyes(), except that it also gives
 * a pessimistic view of the chances to make eyes. Since it is intended
 * to be used from the owl code, the option to add move reasons has
 * been removed.
 */
void
compute_eyes_pessimistic(int pos, int *max, int *min,
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
    DEBUG(DEBUG_EYES, "\n");
  }
  
  /* First we try to let the life code evaluate the eye space. */
  if (life
      && eye[pos].esize <= life_eyesize
      && recognize_eye2(pos, attack_point, defense_point, max, min,
			eye, heye, 0, EMPTY)) {
    *pessimistic_min = *min - margins;

    DEBUG(DEBUG_EYES, "  life - max=%d, min=%d, pessimistic_min=%d\n",
	  *max, *min, *pessimistic_min);
  }
  /* Fall back on the graphs database if the eye is too big or the
   * life code is disabled.
   */
  else if (recognize_eye(pos, attack_point, defense_point, max, min,
			 eye, heye, 0, EMPTY)) {
    *pessimistic_min = *min - margins;

    /* A single point eye which is part of a ko can't be trusted. */
    if (eye[pos].esize == 1
	&& is_ko(pos, eye[pos].color == WHITE_BORDER ? BLACK : WHITE, NULL))
      *pessimistic_min = 0;

    DEBUG(DEBUG_EYES, "  graph matching - max=%d, min=%d, pessimistic_min=%d\n",
	  *max, *min, *pessimistic_min);
  }
  
  /* Ideally any eye space that hasn't been matched yet should be two
   * secure eyes. Until the database becomes more complete we have
   * some additional heuristics to guess the values of unknown
   * eyespaces.
   */
  else {
    guess_eye_space(pos, effective_eyesize, margins, eye,
		    max, min, pessimistic_min); 
    DEBUG(DEBUG_EYES, "  guess_eye - max=%d, min=%d, pessimistic_min=%d\n",
	  *max, *min, *pessimistic_min);
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
  
  if (attack_point
      && *attack_point == NO_MOVE
      && *max != *pessimistic_min) {
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
	
	if (this_score > score) {
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
		int *max, int *min, int *pessimistic_min)
{
  if (effective_eyesize > 3) {
    *min = 2;
    *max = 2;
    if ((margins == 0 && effective_eyesize > 7)
	|| (margins > 0 && effective_eyesize > 9))
      *pessimistic_min = 2;
    else
      *pessimistic_min = 1;
  }
  else if (effective_eyesize > 0) {
    *min = 1;
    *max = 1;
    if (margins > 0)
      *pessimistic_min = 0;
    else
      *pessimistic_min = 1;
  }
  else {
    *min = 0;
    if (eye[pos].esize - margins > 2)
      *max = 1;
    else
      *max = 0;
    *pessimistic_min = 0;
  }
}


/* recognize_eye(pos, *attack_point, *defense_point, *max, *min, eye_data, 
 * half_eye_data, add_moves, color), where pos is the origin of an eyespace,
 * returns 1 if there is a pattern in eyes.db matching the eyespace, or
 * 0 if no match is found. If there is a key point for attack, (*attack_point)
 * is set to its location, or NO_MOVE if there is none.
 * Similarly (*defense_point) is the location of a vital defense point. *min
 * and *max are the minimum and maximum number of eyes that can be
 * made in this eyespace respectively. Vital attack/defense points
 * exist if and only if *min != *max.
 *
 * If add_moves==1, this function may add a move_reason for (color) at
 * a vital point which is found by the function. If add_moves==0,
 * set color==EMPTY.
 */

static int
recognize_eye(int pos, int *attack_point, int *defense_point,
	      int *max, int *min, 
	      struct eye_data eye[BOARDMAX], 
	      struct half_eye_data heye[BOARDMAX], 
	      int add_moves, int color)
{
  int m, n;
  int k;
  int eye_size = 0;
  int vpos[MAXEYE], marginal[MAXEYE], neighbors[MAXEYE];
  int edge[MAXEYE];
  int graph;
  int q;
  int map[MAXEYE];
  int ok, contin;
  int eye_color;
  int kpos;
  int num_marginals = 0;

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
	
	eye_size++;
	if (is_halfeye(heye, pos2)) {

	  /* Use one of the diagonals as a marginal for mapping purposes.
	   * The whole set of diagonals is isomorphic to a marginal.
	   * Note that the half eye preceedes the diagonal in the list.
	   */
	  neighbors[eye_size-1]++;       /* increase neighbors of half eye */
	  if (eye_color == color)
	    kpos = heye[pos2].defense_point[0];
	  else
	    kpos = heye[pos2].attack_point[0];
	  ASSERT_ON_BOARD1(kpos);
	  vpos[eye_size] = kpos;
	  marginal[eye_size] = 1;
	  edge[eye_size] = 0;
	  num_marginals++;
	  neighbors[eye_size] = 1;
	  eye_size++;
	}
      }
    }

  /* We attempt to construct a map from the graph to the eyespace
   * preserving the adjacency structure. If this can be done, we've
   * identified the eyeshape.
   */

  for (graph = 0; graphs[graph].vertex != NULL; graph++) {
    if (graphs[graph].esize != eye_size
	|| graphs[graph].msize != num_marginals)
      continue;


    q = 0;
    first_map(q, map);

    contin = 1;
    while (contin && q >= 0 && q < eye_size) {
      ok = 1;

      if (0)
	TRACE("q=%d: %d %d %d %d %d %d\n", 
	      q, map[0], map[1], map[2], map[3], map[4], map[5]);

      if (neighbors[map[q]] != graphs[graph].vertex[q].neighbors)
	ok = 0;

      if (ok && marginal[map[q]]
	  && graphs[graph].vertex[q].type != '!'
	  && graphs[graph].vertex[q].type != '@'
	  && graphs[graph].vertex[q].type != '$'
	  && graphs[graph].vertex[q].type != ')'
	  && graphs[graph].vertex[q].type != '(')
	ok = 0;
      
      if (ok && !marginal[map[q]]
	  && (graphs[graph].vertex[q].type == '!'
	      || graphs[graph].vertex[q].type == '('
	      || graphs[graph].vertex[q].type == ')'
	      || graphs[graph].vertex[q].type == '@'
	      || graphs[graph].vertex[q].type == '$'))
	ok = 0;
      
      if (ok && IS_STONE(board[vpos[map[q]]])
	  && graphs[graph].vertex[q].type != 'X'
	  && graphs[graph].vertex[q].type != 'x'
	  && graphs[graph].vertex[q].type != '$')
	ok = 0;
      
      if (ok && board[vpos[map[q]]] == EMPTY
	  && (graphs[graph].vertex[q].type == 'X'
	      || graphs[graph].vertex[q].type == '$'))
	ok = 0;

      if (ok && edge[map[q]] < graphs[graph].vertex[q].edge)
	ok = 0;
      
      if (ok && graphs[graph].vertex[q].n1 < q
	  && graphs[graph].vertex[q].n1 != -1)
	{
	  if (!adjacent(heye, vpos[map[q]], 
			vpos[map[graphs[graph].vertex[q].n1]]))
	    ok = 0;
	}
      if (ok && graphs[graph].vertex[q].n2 < q
	  && graphs[graph].vertex[q].n2 != -1)
	{
	  if (!adjacent(heye, vpos[map[q]], 
			vpos[map[graphs[graph].vertex[q].n2]]))
	    ok = 0;
	}
      if (ok && graphs[graph].vertex[q].n3 < q
	  && graphs[graph].vertex[q].n3 != -1)
	{
	  if (!adjacent(heye, vpos[map[q]],
			vpos[map[graphs[graph].vertex[q].n3]]))
	    ok = 0;
	}
      if (ok && graphs[graph].vertex[q].n4 < q
	  && graphs[graph].vertex[q].n4 != -1)
	{
	  if (!adjacent(heye, vpos[map[q]],
			vpos[map[graphs[graph].vertex[q].n4]]))
	    ok = 0;
	}

      if (!ok) {
	contin = next_map(&q, map, eye_size);
	if (0)
	  gprintf("  q=%d, esize=%d: %d %d %d %d %d\n",
		  q, eye_size, 
		  map[0], map[1], map[2], map[3], map[4]);
      }
      else {
	q++;
	first_map(q, map);
      }
    }

    /* We have found a match! Now sort out the vital moves. */
    if (q == eye_size) {
      *max = graphs[graph].max;
      *min = graphs[graph].min;
      if (*max != *min) {
	/* Collect all attack and defense points in the pattern. */
	int attack_points[4 * MAXEYE];
	int defense_points[4 * MAXEYE];
	int num_attacks = 0;
	int num_defenses = 0;

	for (k = 0; k < graphs[graph].esize; k++) {
	  if (graphs[graph].vertex[k].type == '*'
	      || graphs[graph].vertex[k].type == '<')
	    attack_points[num_attacks++] = vpos[map[k]];
	  else if (graphs[graph].vertex[k].type == '@'
		   || graphs[graph].vertex[k].type == '(') {
	    /* check for marginal matching half eye diagonal
	     * If it is a half eye diagonal, the half eye preceeds
	     * the diagonal in the list of vertices
	     */
	    if (map[k] > 0 && is_halfeye(heye, vpos[map[k]-1])) {
	      /* Add all diagonals as vital. */
	      int ix;
	      struct half_eye_data *this_half_eye = &heye[vpos[map[k]-1]];
	      
	      for (ix = 0; ix < this_half_eye->num_attacks; ix++)
		attack_points[num_attacks++] = this_half_eye->attack_point[ix];
	    }
	    else
	      attack_points[num_attacks++] = vpos[map[k]];
	  }
	  
	  if (graphs[graph].vertex[k].type == '*'
	      || graphs[graph].vertex[k].type == '>')
	    defense_points[num_defenses++] = vpos[map[k]];
	  else if (graphs[graph].vertex[k].type == '@'
		   || graphs[graph].vertex[k].type == ')') {
	    /* Check for marginal matching half eye diagonal. */
	    if (map[k] > 0 && is_halfeye(heye, vpos[map[k]-1])) {
	      /* Add all diagonals as vital. */
	      int ix;
	      struct half_eye_data *this_half_eye = &heye[vpos[map[k]-1]];

	      for (ix = 0; ix < this_half_eye->num_defends; ix++)
		defense_points[num_defenses++] = this_half_eye->defense_point[ix];
	    }
	    else
	      defense_points[num_defenses++] = vpos[map[k]];
	  }
	}
	
	gg_assert(num_attacks > 0 && num_defenses > 0);
	*attack_point = attack_points[0];
	*defense_point = defense_points[0];
	DEBUG(DEBUG_EYES, "  vital points: %1m (attack) %1m (defense)\n",
	      *attack_point, *defense_point);
	DEBUG(DEBUG_EYES, "  pattern matched:  %s\n", graphs[graph].patname);

	if (add_moves) {
	  if (eye_color != color) {
	    for (k = 0; k < num_attacks; k++)
	      add_vital_eye_move(attack_points[k], pos, eye_color);
	  }
	  else {
	    for (k = 0; k < num_defenses; k++)
	      add_vital_eye_move(defense_points[k], pos, eye_color);
	  }
	}
      }
      TRACE("eye space at %1m of type %s\n", pos, graphs[graph].patname);

      return 1;
    }
  }

  return 0;
}


/* a MAP is a map of the integers 0,1,2, ... ,q into 
 * 0,1, ... , esize-1 where q < esize. This determines a 
 * bijection of the first q+1 elements of the graph into the 
 * eyespace. The function first_map finds the smallest valid
 * value of element q, assuming the previous elements are ok.
 */

static void
first_map(int q, int map[MAXEYE])
{
  int k;
  int r;
  
  for (k = 0; k <= q; k++) {
    for (r = 0; r < q; r++)
      if (map[r] == k)
	break;

    if (r == q) {
      map[q] = k;
      break;
    }
  }
}     


/* a MAP is a map of the integers 0,1,2, ... ,q into 
 * 0,1, ... , esize-1 where q < esize. This determines a 
 * bijection of the first q+1 elements of the graph into the 
 * eyespace. The function next_map produces the next map when
 * these are ordered lexicographically. If no next map can
 * be found, q is decremented, then we try again. If q==0
 * and no next map can be found, the function returns false.
 */

static int
next_map(int *q, int map[MAXEYE], int esize)
{
  int mapok = 0;
  int r;

  if (0)
    gprintf("  q=%d, esize=%d: %d %d %d %d %d\n",
	    *q, esize, map[0], map[1], map[2], map[3], map[4]);

  if (*q == 0 && map[*q] == esize - 1)
    return 0;

  map[*q]++;
  while (!mapok) {
    mapok = 1;
    for (r = 0; r < *q; r++) {
      if (map[r] == map[*q]) {
	map[*q]++;
	mapok = 0;
      }
    }
  }

  if (map[*q] >= esize) {
    map[*q] = 0;
    (*q)--;
    return next_map(q, map, esize);
  }
  else
    return 1;
}     


/* add_false_eye() turns a proper eyespace into a margin. */

void
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
    max_white = white_eye[pos].maxeye;

  if (black_eye[pos].color == BLACK_BORDER)
    max_black = black_eye[pos].maxeye;

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

/* See Texinfo documentation (Eyes:Eye Topology). Returns:
 * - 2 or less if (pos) is a proper eye for (color);
 * - between 2 and 3 if the eye can be made false only by ko
 * - 3 if (pos) is a half eye;
 * - between 3 and 4 if the eye can be made real only by ko
 * - 4 or more if (pos) is a false eye.
 *
 * Attack and defense points for control of the diagonals are stored
 * in the heye[] array.
 */

float
topological_eye(int pos, int color,
		struct eye_data b_eye[BOARDMAX],
		struct eye_data w_eye[BOARDMAX],
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
					 b_eye, w_eye);
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
	  defense_value = 3;
	}
	else {
	  attack_value = 3;
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
 */
static float
evaluate_diagonal_intersection(int m, int n, int color,
			       int *attack_point, int *defense_point,
			       struct eye_data b_eye[BOARDMAX],
			       struct eye_data w_eye[BOARDMAX])
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
  if (color == BLACK
      && b_eye[pos].color == BLACK_BORDER
      && !b_eye[pos].marginal
      && b_eye[pos].marginal_neighbors < 2
      && !(board[pos] == EMPTY && does_capture_something(pos, WHITE)))
    return 0.0;
  if (color == WHITE
      && w_eye[pos].color == WHITE_BORDER
      && !w_eye[pos].marginal
      && w_eye[pos].marginal_neighbors < 2
      && !(board[pos] == EMPTY && does_capture_something(pos, BLACK)))
    return 0.0;

  if (board[pos] == EMPTY) {
    /* We should normally have a safe move, but occasionally it may
     * happen that it's not safe. There are complications, however,
     * with a position like this
     *
     * .XXXX|
     * XXOO.|
     * XO.O.|
     * XXO.O|
     * -----+
     *
     */

    int our_safety = safe_move(pos, color);
    int your_safety = safe_move(pos, other);
    
    if (your_safety == 0)
      value = 0.0;
    else if (our_safety == 0 && your_safety == WIN)
      value = 2.0;
    else if (our_safety == WIN && your_safety == WIN)
      value = 1.0;
    else if (our_safety == WIN && your_safety != WIN)
      value = a;
    else if (our_safety != WIN && your_safety == WIN)
      value = b;
    else
      value = 1.0; /* Both contingent on ko. Probably can't happen. */

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
      dcode = worm[pos].defend_codes[0];
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
  Position starting_position;

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
  store_position(&starting_position);

  /* Loop over all configurations of black stones inserted in the
   * eyeshape. There are N = 2^(eyesize) configurations and we can
   * straightforwardly use binary representation to enumerate them.
   */
  N = 1 << eyesize;
  for (n = 0; n < N; n++) {
    int valid = 1;
    int internal_stones = 0;
    
    restore_position(&starting_position);
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

    attack_code = owl_attack(str, &attack_point, NULL);

    if (attack_code == 0) {
      /* The owl code claims there is no attack. We test this by
       * trying to attack on all empty spaces in the eyeshape.
       */
      for (k = 0; k < eyesize; k++) {
	if (board[eye_vertices[k]] == EMPTY
	    && is_legal(eye_vertices[k], BLACK)
	    && owl_does_attack(eye_vertices[k], str)) {
	  gprintf("%1m alive, but %1m attacks:\n",
		  str, eye_vertices[k]);
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
	      && !owl_does_defend(eye_vertices[k], str)) {
	    gprintf("%1m alive, but almost filled with nakade:\n",
		    str);
	    showboard(0);
	  }
	}
      }
    }
    else {
      defense_code = owl_defend(str, &defense_point, NULL);
      if (defense_code == 0) {
	/* The owl code claims there is no defense. We test this by
	 * trying to defend on all empty spaces in the eyeshape.
	 */
	for (k = 0; k < eyesize; k++) {
	  if (board[eye_vertices[k]] == EMPTY
	      && is_legal(eye_vertices[k], WHITE)
	      && owl_does_defend(eye_vertices[k], str)) {
	    gprintf("%1m dead, but %1m defends:\n",
		    str, eye_vertices[k]);
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
	else if (!owl_does_attack(attack_point, str)) {
	  gprintf("Attack point %1m failed:\n", attack_point);
	  showboard(0);
	}

	if (board[defense_point] != EMPTY
		 || !is_legal(defense_point, WHITE)) {
	  gprintf("Bad defense point %1m:\n", defense_point);
	  showboard(0);
	}
	else if (!owl_does_defend(defense_point, str)) {
	  gprintf("Defense point %1m failed:\n", defense_point);
	  showboard(0);
	}
      }
    }
  }
  verbose = save_verbose;
}


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
