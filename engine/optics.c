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
static int is_lively(int owl_call, int i, int j);
static int false_margin(int pos, int color, int lively[BOARDMAX]);

static int recognize_eye(int i, int j, int *ai, int *aj, int *di, int *dj,
			 int *max, int *min, 
			 struct eye_data eye[BOARDMAX],
			 struct half_eye_data heye[BOARDMAX],
			 int add_moves, int color);
static int linear_eye_space(int i, int j, int *attacki, int *attackj,
			    int *max, int *min,
			    struct eye_data eye[BOARDMAX]);
static void first_map(int q, int map[MAXEYE]);
static int next_map(int *q, int map[MAXEYE], int esize);
static void print_eye(struct eye_data eye[BOARDMAX],
		      struct half_eye_data heye[BOARDMAX], int i, int j);
static int 
evaluate_diagonal_intersection(int m, int n, int color,
			       int *attacki, int *attackj,
			       int *defendi, int *defendj,
			       struct eye_data b_eye[BOARDMAX],
			       struct eye_data w_eye[BOARDMAX]);


/* These are used during the calculations of eye spaces. */
static int  black_domain[BOARDMAX];
static int  white_domain[BOARDMAX];


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
 * make_domains() is called from make_dragons(). It marks the black
 * and white domains (eyeshape regions) and collects some statistics
 * about each one.
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
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++) {
      clear_eye(&(b_eye[POS(i, j)]));
      clear_eye(&(w_eye[POS(i, j)]));
      lively[POS(i, j)] = is_lively(owl_call, i, j);
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
	  w_eye[POS(i, j)].color = GRAY;
	  b_eye[POS(i, j)].color = GRAY;
	}
	else if (black_domain[pos] == 1 && white_domain[pos] == 0) {
	  b_eye[POS(i, j)].color = BLACK_BORDER;
	  for (k = 0; k < 4; k++) {
	    int apos = pos + delta[k];
	    if (ON_BOARD(apos) && white_domain[apos] && !black_domain[apos]) {
	      b_eye[POS(i, j)].marginal = 1;
	      break;
	    }
	  }
	}
	else if (black_domain[pos] == 0 && white_domain[pos] == 1) {
	  w_eye[POS(i, j)].color = WHITE_BORDER;
	  for (k = 0; k < 4; k++) {
	    int apos = pos + delta[k];
	    if (ON_BOARD(apos) && black_domain[apos] && !white_domain[apos]) {
	      w_eye[POS(i, j)].marginal = 1;
	      break;
	    }
	  }
	}
	else if (black_domain[pos] == 1 && white_domain[pos] == 1) {
	  for (k = 0; k < 4; k++) {
	    int apos = pos + delta[k];
	    if (ON_BOARD(apos) && black_domain[apos] && !white_domain[apos]) {
	      b_eye[POS(i, j)].marginal = 1;
	      b_eye[POS(i, j)].color = BLACK_BORDER;
	      break;
	    }
	  }
	  if (k == 4)
	    b_eye[POS(i, j)].color = GRAY;
	  
	  for (k = 0; k < 4; k++) {
	    int apos = pos + delta[k];
	    if (ON_BOARD(apos) && white_domain[apos] && !black_domain[apos]) {
	      w_eye[POS(i, j)].marginal = 1;
	      w_eye[POS(i, j)].color = WHITE_BORDER;
	      break;
	    }
	  }
	  if (k == 4)
	    w_eye[POS(i, j)].color = GRAY;
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
      if (b_eye[POS(i, j)].origin == NO_MOVE 
	  && b_eye[POS(i, j)].color == BLACK_BORDER)
      {
	int esize = 0;
	int msize = 0;

	originate_eye(i, j, i, j, &esize, &msize, b_eye);
	b_eye[POS(i, j)].esize = esize;
	b_eye[POS(i, j)].msize = msize;
      }
    }

  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++) {
      if (w_eye[POS(i, j)].origin == NO_MOVE
	  && w_eye[POS(i, j)].color == WHITE_BORDER)
      {
	int esize = 0;
	int msize = 0;

	originate_eye(i, j, i, j, &esize, &msize, w_eye);
	w_eye[POS(i, j)].esize = esize;
	w_eye[POS(i, j)].msize = msize;
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
#if 1
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
	    if (board[pos] != EMPTY || false_margins[pos] != 1)
	      domain[pos] = 1;
	  }
	}
	
	if (lively_stone(SOUTH(pos), other)
	    || lively_stone(WEST(pos), other)
	    || lively_stone(NORTH(pos), other)
	    || lively_stone(EAST(pos), other))
	  inhibit[pos] = 2;
	else if (!ON_BOARD(SOUTH(pos))
		 || !ON_BOARD(WEST(pos))
		 || !ON_BOARD(NORTH(pos))
		 || !ON_BOARD(EAST(pos)))
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
#else
  int other = OTHER_COLOR(color);
  int found_one;
  int i, j;
  int pos;

  do {
    found_one = 0;
    for (i = 0; i < board_size; i++)
      for (j = 0; j < board_size; j++) {
	pos = POS(i, j);
	
	/* First we handle the trivial cases. */
	if (domain[pos])
	  continue;
	if (!(board[pos] == EMPTY || (board[pos] == other && !lively[pos])))
	  continue;

	/* Case (1) above. */
	if (lively_stone(pos, color)) {
	  /* Comment: This code is never reached due to the previous
           * continue statement.
	   */
	  domain[pos] = 1;
	  found_one = 1;
	  continue;
	}

	/* Case (2) above. */
	if (lively_stone(NORTH(pos), color)
	    || lively_stone(SOUTH(pos), color)
	    || lively_stone(WEST(pos), color)
	    || lively_stone(EAST(pos), color))
	{
	  /* To explain the asymmetry between the first time around
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
	    else {
	      domain[pos] = 1;
	      found_one = 1;
	    }
	  }
	  else {
	    if (board[pos] != EMPTY || false_margins[pos] != 1) {
	      found_one = 1;
	      domain[pos] = 1;
	    }
	  }
	  continue;
	}

	/* Case (3) above. */

	/* First test the corners. */
	if ((i == 0 && j == 0
	     && (has_inf(color, POS(1, 0))
		 || has_inf(color, POS(0, 1)))
	     && !lively_stone(POS(1, 0), other)
	     && !lively_stone(POS(1, 1), other)
	     && !lively_stone(POS(0, 1), other))
	    || (i == board_size-1 && j == 0
		&& (has_inf(color, POS(board_size-2, 0))
		    || has_inf(color, POS(board_size-1, 1)))
		&& !lively_stone(POS(board_size-2, 0), other)
		&& !lively_stone(POS(board_size-2, 1), other)
		&& !lively_stone(POS(board_size-1, 1), other))
	    || (i == 0 && j == board_size-1
		&& (has_inf(color, POS(1, board_size-1))
		    || has_inf(color, POS(0, board_size-2)))
		&& !lively_stone(POS(1, board_size-1), other)
		&& !lively_stone(POS(1, board_size-2), other)
		&& !lively_stone(POS(0, board_size-2), other))
	    || (i == board_size-1 && j == board_size-1
		&& (has_inf(color, POS(board_size-2, board_size-1))
		    || has_inf(color, POS(board_size-1, board_size-2)))
		&& !lively_stone(POS(board_size-2, board_size-1), other)
		&& !lively_stone(POS(board_size-2, board_size-2), other)
		&& !lively_stone(POS(board_size-1, board_size-2), other)))
	{
	  domain[pos] = 1;
	  found_one = 1;
	} 
	else /* Then the rest of the board. */
	  if (((i > 1 && j > 0 && j < board_size-1
		&& has_inf(color, NORTH(pos)))
	       && ((j > 1 && has_inf(color, POS(i-1, j-1))
		    && !lively_stone(WEST(pos), other) /* 1st CAVEAT */
		    && (j > board_size-2 
			|| !lively_stone(POS(i-1, j+1), other)
			|| j < 2
			|| !lively_stone(POS(i-1, j-2), other))) /* 2nd CAVEAT */
		   || (j < board_size-2
		       && has_inf(color, POS(i-1, j+1))
		       && !lively_stone(EAST(pos), other)
		       && (j > board_size-3
			   || !lively_stone(POS(i-1, j+2), other)
			   || j < 1
			   || !lively_stone(POS(i-1, j-1), other)))))
	      ||
	      ((i < board_size-2 && j > 0 && j < board_size-1
		&& has_inf(color, SOUTH(pos)))
	       && ((j > 1 && has_inf(color, POS(i+1, j-1))
		    && !lively_stone(WEST(pos), other)
		    && (j > board_size-2 
			|| !lively_stone(POS(i+1, j+1), other)
			|| j < 2
			|| !lively_stone(POS(i+1, j-2), other)))
		   || (j < board_size-2
		       && has_inf(color, POS(i+1, j+1))
		       && !lively_stone(EAST(pos), other)
		       && (j > board_size-3
			   || !lively_stone(POS(i+1, j+2), other)
			   || j < 1
			   || !lively_stone(POS(i+1, j-1), other)))))
	      ||
	      ((j > 1 && i > 0 && i < board_size-1
		&& has_inf(color, WEST(pos)))
	       && ((i > 1 && has_inf(color, POS(i-1, j-1))
		    && !lively_stone(NORTH(pos), other)
		    && (i > board_size-2 
			|| !lively_stone(POS(i+1, j-1), other)
			|| i < 2
			|| !lively_stone(POS(i-2, j-1), other)))
		   || (i < board_size-2
		       && has_inf(color, POS(i+1, j-1))
		       && !lively_stone(SOUTH(pos), other)
		       && (i > board_size-3
			   || !lively_stone(POS(i+2, j-1), other)
			   || i < 1
			   || !lively_stone(POS(i-1, j-1), other)))))
	      ||
	      ((j < board_size-2 && i > 0 && i < board_size-1
		&& has_inf(color, EAST(pos)))
	       && ((i > 1 && has_inf(color, POS(i-1, j+1))
		    && !lively_stone(NORTH(pos), other)
		    && (i > board_size-2 
			|| !lively_stone(POS(i+1, j+1), other)
			|| i < 2
			|| !lively_stone(POS(i-2, j+1), other)))
		   || (i < board_size-2
		       && has_inf(color, POS(i+1, j+1))
		       && !lively_stone(SOUTH(pos), other)
		       && (j > board_size-3
			   || !lively_stone(POS(i+2, j+1), other)
			   || i < 1
			   || !lively_stone(POS(i-1, j+1), other))))))
	  {
	    domain[pos] = 1;
	    found_one = 1;
	  }
      }
  } while (found_one);
#endif
  
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
  int i, j;
  int k;

  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++) {
      if (eyedata[POS(i, j)].origin == NO_MOVE) 
	continue;

      eyedata[POS(i, j)].esize = eyedata[eyedata[POS(i, j)].origin].esize;
      eyedata[POS(i, j)].msize = eyedata[eyedata[POS(i, j)].origin].msize;
      eyedata[POS(i, j)].neighbors = 0;
      eyedata[POS(i, j)].marginal_neighbors = 0;

      for (k = 0; k < 4; k++) {
	int ai = i + deltai[k];
	int aj = j + deltaj[k];
	if (ON_BOARD2(ai, aj)
	    && eyedata[POS(ai, aj)].origin == eyedata[POS(i, j)].origin) {
	  eyedata[POS(i, j)].neighbors++;
	  if (eyedata[POS(ai, aj)].marginal)
	    eyedata[POS(i, j)].marginal_neighbors++;
	}
      }
    }
}


static int
is_lively(int owl_call, int i, int j)
{
  int result;

  if (owl_call)
    result = owl_lively(i, j);
  else
    result = (!worm[POS(i, j)].inessential
	      && (worm[POS(i, j)].attack_code == 0
		  || worm[POS(i, j)].defend_code != 0));

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
    
    if (stackp == 0 && worm[apos].attack_code == 0)
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
 * originate_eye(i, j, i, j, *size) creates an eyeshape with origin (i, j).
 * the last variable returns the size. The repeated variables (i, j) are due
 * to the recursive definition of the function.
 */
void
originate_eye(int i, int j, int m, int n,
	      int *esize, int *msize, 
	      struct eye_data eye[BOARDMAX])
{
  int k;
  ASSERT_ON_BOARD2(i, j);
  ASSERT_ON_BOARD2(m, n);
  gg_assert(esize != NULL);
  gg_assert(msize != NULL);
  
  eye[POS(m, n)].origin = POS(i, j);
  (*esize)++;
  if (eye[POS(m, n)].marginal)
    (*msize)++;
  if (eye[POS(m, n)].type & INHIBIT_CONNECTION)
    return;

  for (k = 0; k < 4; k++) {
    int ai = m + deltai[k];
    int aj = n + deltaj[k];
    if (ON_BOARD2(ai, aj)
	&& eye[POS(ai, aj)].color == eye[POS(m, n)].color
	&& eye[POS(ai, aj)].origin == NO_MOVE
	&& (!eye[POS(ai, aj)].marginal || !eye[POS(m, n)].marginal))
      originate_eye(i, j, ai, aj, esize, msize, eye);
  }
}


/* Print debugging data for the eyeshape at (i,j). Useful with GDB.
 */

static void
print_eye(struct eye_data eye[BOARDMAX],
	  struct half_eye_data heye[BOARDMAX],
	  int i, int j)
{
  int m,n;
  int mini, maxi;
  int minj, maxj;
  int origin = eye[POS(i, j)].origin;
  
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
      if (eye[POS(m, n)].origin == origin) {
	if (BOARD(m, n) == EMPTY) {
	  if (eye[POS(m, n)].marginal)
	    gprintf("%o!");
	  else if (is_halfeye(heye, POS(m, n)))
	    gprintf("%oh");
	  else
	    gprintf("%o.");
	}
	else if (is_halfeye(heye, POS(m, n)))
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
 * Given an eyespace with origin (i,j), this function computes the
 * minimum and maximum numbers of eyes the space can yield. If max and
 * min are different, then vital points of attack and defense are also
 * generated.
 * 
 * If add_moves==1, this function may add a move_reason for (color) at
 * a vital point which is found by the function. If add_moves==0,
 * set color==EMPTY.
 */

void
compute_eyes(int i, int  j, int *max, int *min, int *attacki, int *attackj,
	     int *defendi, int *defendj,
	     struct eye_data eye[BOARDMAX],
	     struct half_eye_data heye[BOARDMAX],
	     int add_moves, int color)
{
  int m, n;

  if (attacki) *attacki = -1;
  if (attackj) *attackj = -1;
  if (defendi) *defendi = -1;
  if (defendj) *defendj = -1;

  if (debug & DEBUG_EYES) {
    DEBUG(DEBUG_EYES, "Eyespace at %m: color=%C, esize=%d, msize=%d\n",
	  i, j, eye[POS(i, j)].color, eye[POS(i, j)].esize, 
	  eye[POS(i, j)].msize);

    for (m = 0; m < board_size; m++)
      for (n = 0; n < board_size; n++) {
	if (eye[POS(m, n)].origin != POS(i, j)) 
	  continue;

	if (eye[POS(m, n)].marginal && BOARD(m, n) != EMPTY)
	  DEBUG(DEBUG_EYES, "%m (X!)\n", m, n);
	else if (eye[POS(m, n)].marginal && BOARD(m, n) == EMPTY)
	  DEBUG(DEBUG_EYES, "%m (!)\n", m, n);
	else if (!eye[POS(m, n)].marginal && BOARD(m, n) != EMPTY)
	  DEBUG(DEBUG_EYES, "%m (X)\n", m, n);
	else if (is_halfeye(heye, POS(m, n)) && BOARD(m, n) == EMPTY)
	  DEBUG(DEBUG_EYES, "%m (H)\n", m, n);
	else if (is_halfeye(heye, POS(m, n)) && BOARD(m, n) != EMPTY)
	  DEBUG(DEBUG_EYES, "%m (XH)\n", m, n);
	else
	  DEBUG(DEBUG_EYES, "%m\n", m, n);
      }
    DEBUG(DEBUG_EYES, "\n");
    print_eye(eye, heye, i, j);
    DEBUG(DEBUG_EYES, "\n");
  }
  
  /* First we try to let the life code evaluate the eye space. */
  if (life && eye[POS(i, j)].esize <= life_eyesize) {
    int  max1, min1;
    int  attacki1, attackj1;
    int  defendi1, defendj1;
    int  status;

    if (recognize_eye2(i, j, attacki, attackj, defendi, defendj, max, min,
		       eye, heye, add_moves, color)) {

      /* made these printouts contingent on DEBUG_EYES /gf */
      if (debug & DEBUG_EYES) {
	fprintf(stderr, "\n");
	showboard(2);
	
	status = recognize_eye(i, j, &attacki1, &attackj1,
			       &defendi1, &defendj1,
			       &max1, &min1, eye, heye, 0, EMPTY);
	
	if (status) {
	  gprintf("Number of eyes:  --life: (%d, %d)  old: (%d, %d) at %m\n", 
		  *max, *min, max1, min1, i, j);
	  if (*min != *max) {
	    gprintf("  vital point:     attack: %m   defense: %m\n",
		    *attacki, *attackj, *defendi, *defendj);
	    gprintf("  old vital point: attack: %m   defense: %m\n",
		    attacki1, attackj1, defendi1, defendj1);
	  }
	}
	else {
	  gprintf("Number of eyes:  new: (%d, %d) at %m\n", *max, *min, i, j);
	  if (*min != *max)
	    gprintf("  vital point:   attack: %m   defense: %m\n",
		    *attacki, *attackj, *defendi, *defendj);
	}
      }
      
      return;
    }
  }

  /* Fall back on the graphs database if the eye is too big or the
   * life code is disabled.
   */
  if (recognize_eye(i, j, attacki, attackj, defendi, defendj, max, min,
		    eye, heye, add_moves, color))
    return;

  if (eye[POS(i, j)].esize < 6) {
    /* made these printouts contingent on DEBUG_EYES /gf */
    if (debug & DEBUG_EYES) {
      gprintf("===========================================================\n");
      gprintf("Unrecognized eye of size %d shape at %m\n", 
	      eye[POS(i, j)].esize, i, j);
      print_eye(eye, heye, i, j);
    }
  }
  
  /* If not found we examine whether we have a linear eye space. */
  if (linear_eye_space(i, j, attacki, attackj, max, min, eye)) {
    *defendi = *attacki; /* Duplicate attack point to defense point. */
    *defendj = *attackj;
    if (debug & DEBUG_EYES)
      gprintf("Linear eye shape at %m\n", i, j);
    return;
  }

  /* Ideally any eye space that hasn't been matched yet should be two
   * secure eyes. Until the database becomes more complete we have
   * some additional heuristics to guess the values of unknown
   * eyespaces.
   */
  if (eye[POS(i, j)].esize-2*eye[POS(i, j)].msize > 3) {
    *min = 2;
    *max = 2;
  }
  else if (eye[POS(i, j)].esize-2*eye[POS(i, j)].msize > 0) {
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
compute_eyes_pessimistic(int i, int  j, int *max, int *min,
			 int *pessimistic_min,
			 int *attacki, int *attackj,
			 int *defendi, int *defendj,
			 struct eye_data eye[BOARDMAX],
			 struct half_eye_data heye[BOARDMAX])
{
  int m, n;
  int margins = 0;
  int halfeyes = 0;
  int margins_adjacent_to_margin = 0;
  int effective_eyesize;

  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      if (eye[POS(m, n)].origin == POS(i, j)
	  && (eye[POS(m, n)].marginal
	      || is_halfeye(heye, POS(m, n)))) {
	margins++;
	if (eye[POS(m, n)].marginal && eye[POS(m, n)].marginal_neighbors > 0)
	  margins_adjacent_to_margin++;
	if (is_halfeye(heye, POS(m, n)))
	  halfeyes++;
      }
    }

  /* This is a measure based on the simplified assumption that both
   * players only cares about playing the marginal eye spaces. It is
   * used later to guess the eye value for unidentified eye shapes.
   */
  effective_eyesize = (eye[POS(i, j)].esize + halfeyes - 2*margins
		       - margins_adjacent_to_margin);

  if (attacki) *attacki = -1;
  if (attackj) *attackj = -1;
  if (defendi) *defendi = -1;
  if (defendj) *defendj = -1;

  if (debug & DEBUG_EYES) {
    DEBUG(DEBUG_EYES, "Eyespace at %m: color=%C, esize=%d, msize=%d\n",
	  i, j, eye[POS(i, j)].color, eye[POS(i, j)].esize, 
	  eye[POS(i, j)].msize);

    for (m = 0; m < board_size; m++)
      for (n = 0; n < board_size; n++) {
	if (eye[POS(m, n)].origin != POS(i, j)) 
	  continue;

	if (eye[POS(m, n)].marginal && BOARD(m, n) != EMPTY)
	  DEBUG(DEBUG_EYES, "%m (X!)\n", m, n);
	else if (eye[POS(m, n)].marginal && BOARD(m, n) == EMPTY)
	  DEBUG(DEBUG_EYES, "%m (!)\n", m, n);
	else if (!eye[POS(m, n)].marginal && BOARD(m, n) != EMPTY)
	  DEBUG(DEBUG_EYES, "%m (X)\n", m, n);
	else if (is_halfeye(heye, POS(m, n)) && BOARD(m, n) == EMPTY)
	  DEBUG(DEBUG_EYES, "%m (H)\n", m, n);
	else if (is_halfeye(heye, POS(m, n)) && BOARD(m, n) != EMPTY)
	  DEBUG(DEBUG_EYES, "%m (XH)\n", m, n);
	else
	  DEBUG(DEBUG_EYES, "%m\n", m, n);
      }
    DEBUG(DEBUG_EYES, "\n");
    print_eye(eye, heye, i, j);
    DEBUG(DEBUG_EYES, "\n");
  }
  
  /* First we try to let the life code evaluate the eye space. */
  if (life
      && eye[POS(i, j)].esize <= life_eyesize
      && recognize_eye2(i, j, attacki, attackj, defendi, defendj, max, min,
			eye, heye, 0, EMPTY)) {
    *pessimistic_min = *min - margins;
  }
  /* Fall back on the graphs database if the eye is too big or the
   * life code is disabled.
   */
  else if (recognize_eye(i, j, attacki, attackj, defendi, defendj, max, min,
			 eye, heye, 0, EMPTY)) {
    *pessimistic_min = *min - margins;

    /* A single point eye which is part of a ko can't be trusted. */
    if (eye[POS(i, j)].esize == 1
	&& is_ko(POS(i, j), 
		 eye[POS(i, j)].color == WHITE_BORDER ? BLACK : WHITE, NULL))
      *pessimistic_min = 0;
  }
  
  /* If not found we examine whether we have a linear eye space. */
  else if (linear_eye_space(i, j, attacki, attackj, max, min, eye)) {
    /* Duplicate attack point to defense point. */
    if (defendi) *defendi = *attacki;
    if (defendj) *defendj = *attackj;
    if (debug & DEBUG_EYES)
      gprintf("Linear eye shape at %m\n", i, j);
    *pessimistic_min = *min - margins;
    if (*pessimistic_min == 2)
      *pessimistic_min = 1;
  }

  /* Ideally any eye space that hasn't been matched yet should be two
   * secure eyes. Until the database becomes more complete we have
   * some additional heuristics to guess the values of unknown
   * eyespaces.
   */
  else if (effective_eyesize > 3) {
    *min = 2;
    *max = 2;
    if (margins == 0 || effective_eyesize > 7)
      *pessimistic_min = 1;
    else
      *pessimistic_min = 0;
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
    if (eye[POS(i, j)].esize - margins > 2)
      *max = 1;
    else
      *max = 0;
    *pessimistic_min = 0;
  }

  if (*pessimistic_min < 0)
    *pessimistic_min = 0;

  if (*max == *min && *max != *pessimistic_min) {
    /* Find one marginal vertex and set as attack and defense point. */
    for (m = 0; m < board_size; m++)
      for (n = 0; n < board_size; n++) {
	if (eye[POS(m, n)].origin == POS(i, j)) {
	  if (eye[POS(m, n)].marginal
	      && BOARD(m, n) == EMPTY) {
	    if (defendi) *defendi = m;
	    if (defendj) *defendj = n;
	    if (attacki) *attacki = m;
	    if (attackj) *attackj = n;
	    ASSERT_ON_BOARD2(m, n);
	    return;
	  }
	  else if (is_halfeye(heye, POS(m, n))) {
	    ASSERT_ON_BOARD1(heye[POS(m, n)].defense_point[0]);
	    ASSERT_ON_BOARD1(heye[POS(m, n)].attack_point[0]);
	    if (defendi) *defendi = I(heye[POS(m, n)].defense_point[0]);
	    if (defendj) *defendj = J(heye[POS(m, n)].defense_point[0]);
	    if (attacki) *attacki = I(heye[POS(m, n)].attack_point[0]);
	    if (attackj) *attackj = J(heye[POS(m, n)].attack_point[0]);
	    return;
	  }
	}
      }
  }

  if (defendi && defendj && *defendi != -1) {
    ASSERT_ON_BOARD2(*defendi, *defendj);
  }
  if (attacki && attackj && *attacki != -1) {
    ASSERT_ON_BOARD2(*attacki, *attackj);
  }
}


/* 
 * propagate_eye(i, j) copies the data at the origin (i, j) to the
 * rest of the eye (certain fields only).
 */

void
propagate_eye (int pos, struct eye_data eye[BOARDMAX])
{
  int m, n;

  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      if (eye[POS(m, n)].origin == pos) {
	eye[POS(m, n)].color         = eye[pos].color;
	eye[POS(m, n)].esize         = eye[pos].esize;
	eye[POS(m, n)].msize         = eye[pos].msize;
	eye[POS(m, n)].origin        = eye[pos].origin;
	eye[POS(m, n)].maxeye        = eye[pos].maxeye;
	eye[POS(m, n)].mineye        = eye[pos].mineye;
	eye[POS(m, n)].attack_point  = eye[pos].attack_point;
	eye[POS(m, n)].defense_point = eye[pos].defense_point;
	eye[POS(m, n)].dragon        = eye[pos].dragon;
      }
    }
}


/*
 * A linear eyespace is one in which each vertex has 2 neighbors, 
 * except for two vertices on the end. Only the end vertices can
 * be marginal.
 *
 * This function returns true if the eyespace is linear, and if so, 
 * returns in the variables @code{*max} and @code{*min} the number of
 * eyes it yields if the defender plays first and if the attacker
 * plays first. If these are different, then (*attacki, *attackj)
 * is the vital point of attack.
 */
static int
linear_eye_space (int i, int j, int *attacki, int *attackj, int *max, int *min,
		  struct eye_data eye[BOARDMAX])
{
  int m, n;
  int end1i = -1, end1j = -1;
  int end2i = -1, end2j = -1;
  int centers = 0;
  int centeri = -1, centerj = -1;
  int middlei = -1, middlej = -1;
  int is_line = 1;
  int msize = eye[POS(i, j)].msize;
  int esize = eye[POS(i, j)].esize;

  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      if (eye[POS(m, n)].origin == POS(i, j)) {
	if (eye[POS(m, n)].neighbors > 2) {
	  if (centeri == -1) {
	    centeri = m;
	    centerj = n;
	  }
	  centers++;
	  is_line = 0;
	}
	if (eye[POS(m, n)].neighbors == 2) {
	  middlei = m;
	  middlej = n;
	  if (eye[POS(m, n)].marginal)
	    is_line = 0;
	}
	if (eye[POS(m, n)].neighbors == 1) {
	  if (end1i == -1) {
	    end1i = m;
	    end1j = n;
	  }
	  else if (end2i == -1) {
	    end2i = m;
	    end2j = n;
	  }
	}
      }
    }
  
  if (!is_line)
    return 0;

  /* Come here --> Indeed a linear eye space. 
   * Now check how many eyes we can get. 
   */

  /* 1. Check eye spaces with no marginal vertices. */
  if (msize == 0) {
    if (esize == 1 || esize == 2) {
      *max = 1;
      *min = 1;
      return 1;
    }
    if (esize == 3) {
      gg_assert (middlei != -1);
      if (BOARD(middlei, middlej) == EMPTY) {
	*max = 2;
	*min = 1;
	*attacki = middlei;
	*attackj = middlej;
	return 1;
      }
      else {
	*max = 1;
	*min = 1;
	return 1;
      }
    }
    if (esize == 4) {
      int farmiddlei;
      int farmiddlej;
      if (middlei > 0 
	  && eye[POS(middlei-1, middlej)].origin == POS(i, j)
	  && eye[POS(middlei-1, middlej)].neighbors == 2) 
      {
	farmiddlei = middlei-1;
	farmiddlej = middlej;
      }
      else if (middlei < board_size-1
	       && eye[POS(middlei+1, middlej)].origin == POS(i, j)
	       && eye[POS(middlei+1, middlej)].neighbors == 2) 
      {
	farmiddlei = middlei+1;
	farmiddlej = middlej;
      }
      else if (middlej > 0
	       && eye[POS(middlei, middlej-1)].origin == POS(i, j)
	       && eye[POS(middlei, middlej-1)].neighbors == 2) 
      {
	farmiddlei = middlei;
	farmiddlej = middlej-1;
      }
      else if (middlej < board_size-1
	       && eye[POS(middlei, middlej+1)].origin == POS(i, j)
	       && eye[POS(middlei, middlej+1)].neighbors == 2) 
      {
	farmiddlei = middlei;
	farmiddlej = middlej+1;
      }
      else {
	farmiddlei = -1; /* to prevent compiler warning */
	farmiddlej = -1;
	abort();
      }

      if (BOARD(middlei, middlej) == EMPTY) {
	if (BOARD(farmiddlei, farmiddlej) == EMPTY) {
	  *max = 2;
	  *min = 2;
	  return 1;
	}
	else {
	  *max = 2;
	  *min = 1;
	  *attacki = middlei;
	  *attackj = middlej;
	  return 1;
	}
      }
      else
	if (BOARD(farmiddlei, farmiddlej) == EMPTY) {
	  *max = 2;
	  *min = 1;
	  *attacki = farmiddlei;
	  *attackj = farmiddlej;
	  return 1;
	}
	else {
	  *max = 1;
	  *min = 1;
	  return 1;
	}
    }
    if (esize > 4) {
      *max = 2;
      *min = 2;
      return 1;
    }
  }
  /* 2. Check eye spaces with one marginal vertex. */
  if (msize == 1) {
    if (esize == 1) {
      *max = 0;
      *min = 0;
      return 1;
    }
    if (esize == 2) {
      *max = 1;
      *min = 0;
      if (eye[POS(end1i, end1j)].marginal) {
	*attacki = end1i;
	*attackj = end1j;
      }
      else {
	*attacki = end2i;
	*attackj = end2j;
      }
      
      /* We need to make an exception for cases like this:
       * XXOOO
       * .OX.O
       * -----
       */
      if (BOARD(*attacki, *attackj) != EMPTY) {
	*max = 0;
	*attacki = -1;
	*attackj = -1;
      }
      return 1;
    }

    if (esize == 3) {
      if (BOARD(middlei, middlej) == EMPTY) {
	*max = 1;
	*min = 1;

	/* Exceptional cases. (eyes.tst:312) */
	if ((eye[POS(end1i, end1j)].marginal
	     && BOARD(end1i, end1j) != EMPTY
	     && BOARD(end2i, end2j) == EMPTY)
	    || (eye[POS(end2i, end2j)].marginal
		&& BOARD(end2i, end2j) != EMPTY
		&& BOARD(end1i, end1j) == EMPTY)) {
	  *min = 0;
	  *attacki = middlei;
	  *attackj = middlej;
	}
	return 1;
      }
      else {
	*max = 1;
	*min = 0;
	if (eye[POS(end1i, end1j)].marginal) {
	  if (BOARD(end1i, end1j) == EMPTY) {
	    *attacki = end1i;
	    *attackj = end1j;
	  }
	  else {
	    if (BOARD(end2i, end2j) != EMPTY)
	      *min = 1; /* three tactically dead stones in a row. */
	    else
	      *max = 0;
	  }
	}
	else {
	  if (BOARD(end2i, end2j) == EMPTY) {
	    *attacki = end2i;
	    *attackj = end2j;
	  }
	  else {
	    if (BOARD(end1i, end1j) != EMPTY)
	      *min = 1; /* three tactically dead stones in a row. */
	    else
	      *max = 0;
	  }
	}
	return 1;
      }
    }

    if (esize == 4) {
      if (BOARD(middlei, middlej) == EMPTY) {
	*max=1;
	*min=1;
	return 1;
      }
      else {
	int farmiddlei;
	int farmiddlej;
	if (middlei > 0
	    && eye[POS(middlei-1, middlej)].origin == POS(i, j)
	    && eye[POS(middlei-1, middlej)].neighbors == 2) 
	{
	  farmiddlei = middlei-1;
	  farmiddlej = middlej;
	}
	else if (middlei < board_size-1
		 && eye[POS(middlei+1, middlej)].origin == POS(i, j)
		 && eye[POS(middlei+1, middlej)].neighbors == 2)
	{
	  farmiddlei = middlei+1;
	  farmiddlej = middlej;
	}
	else if (middlej > 0
		 && eye[POS(middlei, middlej-1)].origin == POS(i, j)
		 && eye[POS(middlei, middlej-1)].neighbors == 2) 
	{
	  farmiddlei = middlei;
	  farmiddlej = middlej-1;
	}
	else if (middlej < board_size-1
		 && eye[POS(middlei, middlej+1)].origin == POS(i, j)
		 && eye[POS(middlei, middlej+1)].neighbors == 2)
	{
	  farmiddlei = middlei;
	  farmiddlej = middlej+1;
	}
	else {
	  farmiddlei = -1; /* to prevent compiler warning */
	  farmiddlej = -1;
	  abort();
	}

	if (BOARD(farmiddlei, farmiddlej) == EMPTY) {
	  *max = 1;
	  *min = 1;
	  return 1;
	}
	else {
	  *max = 1;
	  *min = 0;
	  if (eye[POS(end1i, end1j)].marginal) {
	    *attacki = end1i;
	    *attackj = end1j;
	  }
	  else {
	    *attacki = end2i;
	    *attackj = end2j;
	  }
	  return 1;
	}
      }
    }

    if (esize == 5) {
      *max=2;
      *min=1;
      if (eye[POS(end1i, end1j)].marginal) {
	*attacki = end1i;
	*attackj = end1j;
      }
      else {
	*attacki = end2i;
	*attackj = end2j;
      }
      return 1;
    }
    if (esize == 6) {
      *max = 2;
      *min = 2;
      return 1;
    }
  }
  
  if (msize == 2) {
    if (esize < 4) {
      *max = 0;
      *min = 0;
      return 1;
    }
    if (esize == 4) {
      *max = 1;
      *min = 0;
      *attacki = end1i;
      *attackj = end1j;
      return 1;
    }
    if (esize == 5 || esize==6) {
      *max = 1;
      *min = 1;
      return 1;
    }
    if (esize == 7) {
      *max = 2;
      *min = 1;
      *attacki = end1i;
      *attackj = end1j;
      return 1;
    }
    if (esize > 7) {
      *max = 2;
      *min = 2;
      return 1;
    }
  }

  return 0;
}

/* recognize_eye(i, j, *ai, *aj, *di, *dj, *max, *min, eye, heye,
 * add_moves, color), where (i,j) is the origin of an eyespace,
 * returns 1 if there is a pattern in eyes.c matching the eyespace, or
 * 0 if no match is found. If there is a key point for attack, (*ai,
 * *aj) are set to its location, or (-1, -1) if there is none.
 * Similarly (*di, *dj) is the location of a vital defense point. *min
 * and *max are the minimum and maximum number of eyes that can be
 * made in this eyespace respectively. Vital attack/defense points
 * exist if and only if *min != *max.
 *
 * If add_moves==1, this function may add a move_reason for (color) at
 * a vital point which is found by the function. If add_moves==0,
 * set color==EMPTY.
 */

static int
recognize_eye(int i, int j, int *ai, int *aj, int *di, int *dj,
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

  /* Set `eye_color' to the owner of the eye. */
  eye_color = eye[POS(i, j)].color;
  if (eye_color == BLACK_BORDER)
    eye_color = BLACK;
  if (eye_color == WHITE_BORDER)
    eye_color = WHITE;


  if (eye[POS(i, j)].esize-eye[POS(i, j)].msize > 7)
    return 0;

  if (eye[POS(i, j)].msize > MAXEYE)
    return 0;

  /* Create list of eye vertices */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      if (eye[POS(m, n)].origin == POS(i, j)) {
	vpos[eye_size] = POS(m, n);
	marginal[eye_size] = eye[POS(m, n)].marginal;
	if (marginal[eye_size])
	  num_marginals++;
	neighbors[eye_size] = eye[POS(m, n)].neighbors;
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
	if (is_halfeye(heye, POS(m, n))) {

	  /* Use one of the diagonals as a marginal for mapping purposes.
	   * The whole set of diagonals is isomorphic to a marginal.
	   * Note that the half eye preceedes the diagonal in the list.
	   */
	  neighbors[eye_size-1]++;       /* increase neighbors of half eye */
	  if (eye_color == color)
	    kpos = heye[POS(m, n)].defense_point[0];
	  else
	    kpos = heye[POS(m, n)].attack_point[0];
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
    if ((graphs[graph].esize != eye_size) 
	|| (graphs[graph].msize != num_marginals)) 
      continue;


    q = 0;
    first_map(q, map);

    contin=1;
    while (contin && q >= 0 && q < eye_size) {
      ok=1;

      if (0)
	TRACE("q=%d: %d %d %d %d %d %d\n", 
	      q, map[0], map[1], map[2], map[3], map[4], map[5]);

      if (neighbors[map[q]] != graphs[graph].vertex[q].neighbors)
	ok = 0;

      if (ok && marginal[map[q]]
	  && (graphs[graph].vertex[q].type != '!')
	  && (graphs[graph].vertex[q].type != '@')
	  && (graphs[graph].vertex[q].type != ')')
	  && (graphs[graph].vertex[q].type != '('))
	ok = 0;
      
      if (ok && !marginal[map[q]]
	  && ((graphs[graph].vertex[q].type == '!') ||
	      (graphs[graph].vertex[q].type == '(') ||
	      (graphs[graph].vertex[q].type == ')') ||
	      (graphs[graph].vertex[q].type == '@')))
	ok = 0;
      
      if (ok && (board[vpos[map[q]]] != EMPTY) 
	  && (graphs[graph].vertex[q].type != 'X')
	  && (graphs[graph].vertex[q].type != 'x'))
	ok = 0;
      
      if (ok && (board[vpos[map[q]]] == EMPTY) 
	  && (graphs[graph].vertex[q].type == 'X'))
	ok = 0;

      if (ok && edge[map[q]] < graphs[graph].vertex[q].edge)
	ok = 0;
      
      if (ok && graphs[graph].vertex[q].n1 < q
	  && (graphs[graph].vertex[q].n1 != -1)) 
	{
	  if (!adjacent(heye, vpos[map[q]], 
			vpos[map[graphs[graph].vertex[q].n1]]))
	    ok = 0;
	}
      if (ok && graphs[graph].vertex[q].n2 < q
	  && (graphs[graph].vertex[q].n2 != -1)) 
	{
	  if (!adjacent(heye, vpos[map[q]], 
			vpos[map[graphs[graph].vertex[q].n2]]))
	    ok = 0;
	}
      if (ok && graphs[graph].vertex[q].n3 < q
	  && (graphs[graph].vertex[q].n3 != -1)) 
	{
	  if (!adjacent(heye, vpos[map[q]],
			vpos[map[graphs[graph].vertex[q].n3]]))
	    ok = 0;
	}
      if (ok && graphs[graph].vertex[q].n4 < q
	  && (graphs[graph].vertex[q].n4 != -1)) 
	{
	  if (!adjacent(heye, vpos[map[q]],
			vpos[map[graphs[graph].vertex[q].n4]]))
	    ok = 0;
	}

      if (!ok) {
	contin=next_map(&q, map, eye_size);
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

    if (q==eye_size) {
      *max = graphs[graph].max;
      *min = graphs[graph].min;
      if (*max != *min) {
	gg_assert(graphs[graph].vital >= 0);
	if (ai) *ai = I(vpos[map[graphs[graph].vital]]);
	if (aj) *aj = J(vpos[map[graphs[graph].vital]]);
	if (di) *di = I(vpos[map[graphs[graph].vital]]);
	if (dj) *dj = J(vpos[map[graphs[graph].vital]]);
	DEBUG(DEBUG_EYES, "  vital points: %m (attack) %m (defense)\n",
	      *ai, *aj, *di, *dj);
	DEBUG(DEBUG_EYES, "  pattern matched:  %d\n", graphs[graph].id);

	if (add_moves) {
	  for (k = graphs[graph].vital; k < graphs[graph].esize; k++) {
	    if (eye_color != color) {
	      if (graphs[graph].vertex[k].type == '*' ||
		  graphs[graph].vertex[k].type == '<') {
		/* add attack vital move */
		add_vital_eye_move(vpos[map[k]], POS(i, j), eye_color);
	      }
	      else if (graphs[graph].vertex[k].type == '@' ||
		       graphs[graph].vertex[k].type == '(') {
 
 		/* check for marginal matching half eye diagonal
 		 * If it is a half eye diagonal, the half eye preceeds
 		 * the diagonal in the list of vertices
 		 */
		if (map[k] > 0 && is_halfeye(heye, vpos[map[k]-1])) {
		  /* add all diagonals as vital */
		  int ix;
		  struct half_eye_data *this_half_eye = &heye[vpos[map[k]-1]];

		  for (ix = 0; ix < this_half_eye->num_attacks; ix++) {
		    add_vital_eye_move(this_half_eye->attack_point[ix],
				       POS(i, j), eye_color);
		  }
		}
		else {
		  add_vital_eye_move(vpos[map[k]], POS(i, j), eye_color);
		}
	      }
	    }
	    else {
	      if (graphs[graph].vertex[k].type == '*' ||
		  graphs[graph].vertex[k].type == '>')
		/* add defense vital move */
		add_vital_eye_move(vpos[map[k]], POS(i, j), eye_color);
	      else if (graphs[graph].vertex[k].type == '@' ||
		       graphs[graph].vertex[k].type == ')') {
		/* check for marginal matching half eye diagonal */
		if (map[k] > 0 && is_halfeye(heye, vpos[map[k]-1])) {
		  /* add all diagonals as vital */
		  int ix;
		  struct half_eye_data *this_half_eye = &heye[vpos[map[k]-1]];

		  for (ix = 0; ix < this_half_eye->num_defends; ix++) {
		    add_vital_eye_move(this_half_eye->defense_point[ix],
				       POS(i, j), eye_color);
		  }
		}
		else {
		  add_vital_eye_move(vpos[map[k]], POS(i, j), eye_color);
		}
	      }
	    }
	  }
	}
      }
      TRACE("eye space at %m of type %d\n", i, j, graphs[graph].id);

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

  if (((*q) == 0) && (map[*q] == esize-1))
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


/* add_half_eye adds a half eye or false eye to an eye shape. */

void
add_half_eye(int m, int n, struct eye_data eye[BOARDMAX],
	     struct half_eye_data heye[BOARDMAX])
{
  if (heye[POS(m, n)].type)
    DEBUG(DEBUG_EYES, "half or false eye found at %m\n", m, n);

  if (heye[POS(m, n)].type == FALSE_EYE) {
    DEBUG(DEBUG_EYES, "false eye at %m for dragon at %1m\n",
	  m, n, eye[POS(m, n)].dragon);
    if (eye[POS(m, n)].color != GRAY) {
      if (eye[POS(m, n)].marginal == 0) {
	eye[POS(m, n)].marginal=1;
	(eye[eye[POS(m, n)].origin].msize)++;
	if ((m > 0) 
	    && (eye[POS(m-1, n)].origin == eye[POS(m, n)].origin))
	  eye[POS(m-1, n)].marginal_neighbors++;
	if ((m < board_size-1) 
	    && (eye[POS(m+1, n)].origin == eye[POS(m, n)].origin))
	  eye[POS(m+1, n)].marginal_neighbors++;
	if ((n > 0)
	    && (eye[POS(m, n-1)].origin == eye[POS(m, n)].origin))
	  eye[POS(m, n-1)].marginal_neighbors++;
	if ((n < board_size-1)
	    && (eye[POS(m, n+1)].origin == eye[POS(m, n)].origin))
	  eye[POS(m, n+1)].marginal_neighbors++;
	propagate_eye(eye[POS(m, n)].origin, eye);
      }
    }
  }
}


/* These functions are used from constraints to identify eye spaces,
 * primarily for late endgame moves.
 */
int
is_eye_space(int pos)
{
  return ((white_eye[pos].color == WHITE_BORDER)
	  || (black_eye[pos].color == BLACK_BORDER));
}

int
is_proper_eye_space(int pos)
{
  return ((   (white_eye[pos].color == WHITE_BORDER)
	   && !white_eye[pos].marginal)
	  || ((black_eye[pos].color == BLACK_BORDER)
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

/* Turn a marginal eye space into a proper eye space. */
void
make_proper_eye_space(int pos, int color)
{
  struct eye_data  *eye;
  int i = I(pos);
  int j = J(pos);

  if (color == WHITE)
    eye = white_eye;
  else
    eye = black_eye;

  gg_assert(eye[pos].color != GRAY_BORDER);
  gg_assert(eye[pos].marginal == 1);
  
  eye[pos].marginal = 0;
  
  (eye[eye[pos].origin].msize)--;
  if ((i > 0) 
      && (eye[pos-NS].origin == eye[pos].origin))
    eye[pos-NS].marginal_neighbors--;
  if ((i < board_size-1) 
      && (eye[pos+NS].origin == eye[pos].origin))
    eye[pos+NS].marginal_neighbors--;
  if ((j > 0)
      && (eye[pos-1].origin == eye[pos].origin))
    eye[pos-1].marginal_neighbors--;
  if ((j < board_size-1)
      && (eye[pos+1].origin == eye[pos].origin))
    eye[pos+1].marginal_neighbors--;

  propagate_eye(eye[pos].origin, eye);
}

/* remove a halfeye from an eye shape. */
void
remove_half_eye(struct half_eye_data heye[BOARDMAX],
		int m, int n, int color)
{
  /* This function is unreliable, not up to date with eye_data and
   * half_eye_data, and should not be used until it's fixed.
   */
  UNUSED(heye); UNUSED(m); UNUSED(n); UNUSED(color); abort();
#if 0
  int ei, ej, ki, kj;
  struct eye_data (*eye)[MAX_BOARD];
  if (color == WHITE)
    eye = white_eye;
  else
    eye = black_eye;

  gg_assert(heye[m][n].type == HALF_EYE);
  
  ki=heye[m][n].ki;
  kj=heye[m][n].kj;
  ei=eye[m][n].origini;
  ej=eye[m][n].originj;

  /* FIXME: If (ki, kj) was part of another eye space before it was
   * merged with (m,n), we won't be able to undo the merge. The
   * failure may be spectacular.
   */
  eye[ei][ej].esize--;
  eye[ei][ej].msize--;
  eye[m][n].neighbors--;
  eye[m][n].marginal_neighbors--;
  eye[ki][kj].origini = -1;
  eye[ki][kj].originj = -1;
  eye[ki][kj].marginal = 0;
  eye[ki][kj].neighbors = 0;
  eye[ki][kj].marginal_neighbors = 0;
  propagate_eye(ei, ej, eye);

  heye[m][n].type = 0;
  heye[m][n].ki = -1;
  heye[m][n].kj = -1;
#endif
}

/* Remove an eye point. This function can only be used before the
 * segmentation into eyespaces.
 */
void
remove_eyepoint(int pos, int color)
{
  if (color == WHITE)
    white_eye[pos].color = GRAY_BORDER;
  else
    black_eye[pos].color = GRAY_BORDER;
}


/* See Texinfo documentation (Eyes:Eye Topology). Returns:
 * 2 or less if (m, n) is a proper eye for (color);
 * 3 if (m, n) is a half eye;
 * 4 if (m, n) is a false eye.
 *
 * (*ai, *aj) and (*di, *dj) returns the coordinates of an empty
 * unsettled diagonal intersection, or an attack and defense point
 * respectively of an unsettled diagonal opponent worm.
 */

int
topological_eye(int m, int n, int color, 
		int *ai, int *aj, int *di, int *dj, 
		struct eye_data b_eye[BOARDMAX],
		struct eye_data w_eye[BOARDMAX],
		struct half_eye_data heye[BOARDMAX])
{
  int sum = 0;
  int val;
  int ax = 0;
  int dx = 0;

  val = evaluate_diagonal_intersection(m+1, n+1, color, ai, aj, di, dj, 
				       b_eye, w_eye);
  sum += val;
  if (val == 1) {
    if (ai != NULL && *ai >= 0) {
      heye[POS(m, n)].attack_point[ax] = POS(*ai, *aj);
      ax++;
    }
    if (di != NULL && *di >= 0) {
      heye[POS(m, n)].defense_point[dx] = POS(*di, *dj);
      dx++;
    }
  }
  
  val = evaluate_diagonal_intersection(m+1, n-1, color, ai, aj, di, dj, 
				       b_eye, w_eye);
  sum += val;
  if (val == 1) {
    if (ai != NULL && *ai >= 0) {
      heye[POS(m, n)].attack_point[ax] = POS(*ai, *aj);
      ax++;
    }
    if (di != NULL && *di >= 0) {
      heye[POS(m, n)].defense_point[dx] = POS(*di, *dj);
      dx++;
    }
  }

  val = evaluate_diagonal_intersection(m-1, n+1, color, ai, aj, di, dj, 
				       b_eye, w_eye);
  sum += val;
  if (val == 1) {
    if (ai != NULL && *ai >= 0) {
      heye[POS(m, n)].attack_point[ax] = POS(*ai, *aj);
      ax++;
    }
    if (di != NULL && *di >= 0) {
      heye[POS(m, n)].defense_point[dx] = POS(*di, *dj);
      dx++;
    }
  }

  val = evaluate_diagonal_intersection(m-1, n-1, color, ai, aj, di, dj, 
				       b_eye, w_eye);
  sum += val;
  if (val == 1) {
    if (ai != NULL && *ai >= 0) {
      heye[POS(m, n)].attack_point[ax] = POS(*ai, *aj);
      ax++;
    }
    if (di != NULL && *di >= 0) {
      heye[POS(m, n)].defense_point[dx] = POS(*di, *dj);
      dx++;
    }
  }

  heye[POS(m, n)].num_attacks = ax;
  heye[POS(m, n)].num_defends = dx;
  return sum;
}



/* Evaluate an intersection (m, n) which is diagonal to an eye space
 * (i, j), as described in the Texinfo documentation (Eyes/Eye
 * Topology).
 * Returns 0 if the opponent cannot safely play at the vertex;
 * Returns 1 if empty and the opponent can safely play on it;
 * Returns 2 if safely occupied by the opponent.
 *
 * Exception: if one coordinate is off the board, returns 1;
 * if both are off the board, returns 0. This guarantees
 * correct behavior for diagonal intersections of points
 * on the edge or in the corner.
 */
static int 
evaluate_diagonal_intersection(int m, int n, int color,
			       int *attacki, int *attackj,
			       int *defendi, int *defendj,
			       struct eye_data b_eye[BOARDMAX],
			       struct eye_data w_eye[BOARDMAX])
{
  int value = 0;
  int other = OTHER_COLOR(color);
  int acode = 0;
  int apos = NO_MOVE;
  int dcode = 0;
  int dpos = NO_MOVE;

  /* Check whether intersection is off the board.*/
  if (m < 0 || m >= board_size)
    value += 1;

  if (n < 0 || n >= board_size)
    value += 1;

  if (value > 0)
    return value % 2; /* Must return 0 if both coordinates out of bounds. */

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
   */
  if (color == BLACK
      && b_eye[POS(m, n)].color == BLACK_BORDER
      && !b_eye[POS(m, n)].marginal
      && !(BOARD(m, n) == EMPTY && does_capture_something2(m, n, WHITE)))
    return 0;
  if (color == WHITE
      && w_eye[POS(m, n)].color == WHITE_BORDER
      && !w_eye[POS(m, n)].marginal
      && !(BOARD(m, n) == EMPTY && does_capture_something2(m, n, BLACK)))
    return 0;

  if (BOARD(m, n) == EMPTY && safe_move2(m, n, other) != 0)
    value = 1;
  else {
    if (stackp == 0) {
      if (BOARD(m, n) == other) {
	if (worm[POS(m, n)].attack_code == 0)
	  value = 2;
	else if (worm[POS(m, n)].defend_code != 0) {
	  value = 1;
	  apos = worm[POS(m, n)].attack_point;
	  dpos = worm[POS(m, n)].defense_point;
	}
      }
    }
    else {
      if (BOARD(m, n) == other) {
	attack_and_defend(POS(m, n), &acode, &apos, &dcode, &dpos);
	if (acode == 0)
	  value = 2;
	else if (dcode != 0)
	  value = 1;
      }
    }   
  }

  if (value == 1) {
    if (BOARD(m, n) == EMPTY) {
      if (attacki) *attacki = m;
      if (attackj) *attackj = n;
      if (defendi) *defendi = m;
      if (defendj) *defendj = n;
    }
    else {
      /* FIXME:
       * Usually there are several attack and defense moves that would
       * be equally valid. It's not good that we make an arbitrary
       * choice at this point.
       */
      if (attacki) *attacki = I(apos);
      if (attackj) *attackj = J(apos);
      if (defendi) *defendi = I(dpos);
      if (defendj) *defendj = J(dpos);
      ASSERT_ON_BOARD1(apos);
      ASSERT_ON_BOARD1(dpos);
    }
  }

  return value;
}


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
