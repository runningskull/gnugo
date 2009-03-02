/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * This is GNU Go, a Go program. Contact gnugo@gnu.org, or see       *
 * http://www.gnu.org/software/gnugo/ for more information.          *
 *                                                                   *
 * Copyright 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007,   *
 * 2008 and 2009 by the Free Software Foundation.                    *
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

#include "liberty.h"
#include "dfa.h"

#include <memory.h>

/* Array for use by TRANSFORM() macro. */
int transformation[MAX_OFFSET][8];

/* Matrix array for use by TRANSFORM2() macro. */
const int transformation2[8][2][2] = {
  { { 1,  0}, 
    { 0,  1}}, /* a - identity transformation matrix */

  { { 0,  1}, 
    {-1,  0}}, /* g - rotate 90 clockwise */

  { {-1,  0}, 
    { 0, -1}}, /* d - rotate 180 */
  
  { { 0, -1}, 
    { 1,  0}}, /* f - rotate 90 counter-clockwise */
  
  { { 0, -1}, 
    {-1,  0}}, /* h - rotate 90 clockwise and flip on x axis */
  
  { {-1,  0}, 
    { 0,  1}}, /* b - flip on x axis */
  
  { { 0,  1}, 
    { 1,  0}}, /* e - rotate 90 counter-clockwise and flip on x axis */
  
  { { 1,  0}, 
    { 0, -1}}  /* c - flip on y axis */
};


/* Initialize transformation[][] array. */
void
transformation_init(void)
{
  int k;
  int dx;
  int dy;

  for (k = 0; k < 8; k++) {
    for (dy = -MAX_BOARD+1; dy <= MAX_BOARD-1; dy++) {
      for (dx = -MAX_BOARD+1; dx <= MAX_BOARD-1; dx++) {
	int tx;
	int ty;

	TRANSFORM2(dx, dy, &tx, &ty, k);
	transformation[OFFSET(dx, dy)][k] = DELTA(tx, ty);
      }
    }
  }
}
/* Spiral orders for DFA matching and building. */
int spiral[DFA_MAX_ORDER][8];

/* The spiral order is the way we scan the board, we begin on the
 * anchor and we progressively scan all its neigbouring intersections,
 * collecting all the known patterns we meet on our way:
 *
 *		    4	   4	  4
 * 1	1     13    13	  513	 513  ... and so on until we reach a
 *	2     2	    2	   2	 827	  stopping state in the DFA.
 *				  6
 *
 * Build the spiral order for each transformation: instead of changing
 * the board or changing the patterns, we only change the order.  For
 * e.g. the same DFA can perform the pattern matching
 *
 * That way for identity:
 *
 *	40					      04
 *     5139	 and this way for mirror symetry:    9315
 *     827					      728
 *	6					       6
 *
 * Anther possibility is to generate one string by pattern and by
 * transformation in `mkpat' to avoid any runtime transformation but
 * it drastically increases the size of DFAs.
 */
void
build_spiral_order(void)
{
  int i;
  int j;
  int k;
  char mark[2 * DFA_MAX_BOARD + 1][2 * DFA_MAX_BOARD + 1];
  int queue_i[DFA_MAX_ORDER];
  int queue_j[DFA_MAX_ORDER];
  int queue_start = 0;
  int queue_end = 1;

  static const int delta_i[4] = { 1,  0, -1,  0};
  static const int delta_j[4] = { 0, 1,  0,  -1};

  /* Initialization. */
  memset(mark, 1, sizeof(mark));
  for (i = 1; i < 2 * DFA_MAX_BOARD; i++) {
    for (j = 1; j < 2 * DFA_MAX_BOARD; j++)
      mark[i][j] = 0;
  }

  queue_i[0] = DFA_MAX_BOARD;
  queue_j[0] = DFA_MAX_BOARD;
  mark[DFA_MAX_BOARD][DFA_MAX_BOARD] = 1;

  do {
    int transformation;

    /* Transform queued coordinates and store DFA offsets in spiral[][]. */
    for (transformation = 0; transformation < 8; transformation++) {
      TRANSFORM2(queue_i[queue_start] - DFA_MAX_BOARD,
		 queue_j[queue_start] - DFA_MAX_BOARD,
		 &i, &j, transformation);
      spiral[queue_start][transformation] = DFA_BASE * i + j;
    }

    for (k = 0; k < 4; k++) {
      i = queue_i[queue_start] + delta_i[k];
      j = queue_j[queue_start] + delta_j[k];

      if (!mark[i][j]) {
	queue_i[queue_end] = i;
	queue_j[queue_end++] = j;
	mark[i][j] = 1;
      }
    }
  } while (++queue_start < queue_end);

  if (0) {
    int transformation;
    for (transformation = 0; transformation < 8; transformation++) {
      fprintf(stderr, "Transformation %d:\n", transformation);
      for (k = 0; k < 16; k++) {
	fprintf(stderr, "\t%d(%c); %d\n", k, 'A' + k,
		spiral[k][transformation]);
      }
    }
  }
}


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
