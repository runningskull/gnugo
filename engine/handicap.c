/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * This is GNU GO, a Go program. Contact gnugo@gnu.org, or see       *
 * http://www.gnu.org/software/gnugo/ for more information.          *
 *                                                                   *
 * Copyright 1999, 2000, 2001, 2002 by the Free Software Foundation. *
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

#include "liberty.h"
#include "random.h"

/* ================================================================ */
/*       Set up fixed placement handicap stones for black side      */
/* ================================================================ */


/* Handicap stones are set up according to the following diagrams:
 *  
 * 2 stones:                    3 stones:           
 *
 *   A B C D E F G H J	  	  A B C D E F G H J  
 * 9 . . . . . . . . . 9  	9 . . . . . . . . . 9
 * 8 . . . . . . . . . 8  	8 . . . . . . . . . 8
 * 7 . . + . . . X . . 7  	7 . . X . . . X . . 7
 * 6 . . . . . . . . . 6  	6 . . . . . . . . . 6
 * 5 . . . . + . . . . 5  	5 . . . . + . . . . 5
 * 4 . . . . . . . . . 4  	4 . . . . . . . . . 4
 * 3 . . X . . . + . . 3  	3 . . X . . . + . . 3
 * 2 . . . . . . . . . 2  	2 . . . . . . . . . 2
 * 1 . . . . . . . . . 1  	1 . . . . . . . . . 1
 *   A B C D E F G H J	  	  A B C D E F G H J  
 *   
 * 4 stones:                    5 stones:           
 *						     
 *   A B C D E F G H J	          A B C D E F G H J  
 * 9 . . . . . . . . . 9 	9 . . . . . . . . . 9
 * 8 . . . . . . . . . 8 	8 . . . . . . . . . 8
 * 7 . . X . . . X . . 7 	7 . . X . . . X . . 7
 * 6 . . . . . . . . . 6 	6 . . . . . . . . . 6
 * 5 . . . . + . . . . 5 	5 . . . . X . . . . 5
 * 4 . . . . . . . . . 4 	4 . . . . . . . . . 4
 * 3 . . X . . . X . . 3 	3 . . X . . . X . . 3
 * 2 . . . . . . . . . 2 	2 . . . . . . . . . 2
 * 1 . . . . . . . . . 1 	1 . . . . . . . . . 1
 *   A B C D E F G H J	          A B C D E F G H J  
 *  
 * 6 stones:                    7 stones:           
 *						     
 *   A B C D E F G H J	          A B C D E F G H J  
 * 9 . . . . . . . . . 9 	9 . . . . . . . . . 9
 * 8 . . . . . . . . . 8 	8 . . . . . . . . . 8
 * 7 . . X . . . X . . 7 	7 . . X . . . X . . 7
 * 6 . . . . . . . . . 6 	6 . . . . . . . . . 6
 * 5 . . X . + . X . . 5 	5 . . X . X . X . . 5
 * 4 . . . . . . . . . 4 	4 . . . . . . . . . 4
 * 3 . . X . . . X . . 3 	3 . . X . . . X . . 3
 * 2 . . . . . . . . . 2 	2 . . . . . . . . . 2
 * 1 . . . . . . . . . 1 	1 . . . . . . . . . 1
 *   A B C D E F G H J	          A B C D E F G H J  
 *  
 * 8 stones:                    9 stones:           
 *						     
 *   A B C D E F G H J	          A B C D E F G H J  
 * 9 . . . . . . . . . 9   	9 . . . . . . . . . 9
 * 8 . . . . . . . . . 8   	8 . . . . . . . . . 8
 * 7 . . X . X . X . . 7   	7 . . X . X . X . . 7
 * 6 . . . . . . . . . 6   	6 . . . . . . . . . 6
 * 5 . . X . + . X . . 5   	5 . . X . X . X . . 5
 * 4 . . . . . . . . . 4   	4 . . . . . . . . . 4
 * 3 . . X . X . X . . 3   	3 . . X . X . X . . 3
 * 2 . . . . . . . . . 2   	2 . . . . . . . . . 2
 * 1 . . . . . . . . . 1   	1 . . . . . . . . . 1
 *   A B C D E F G H J	          A B C D E F G H J  
 *  
 * For odd-sized boards larger than 9x9, the same pattern is followed,
 * except that the edge stones are moved to the fourth line for 13x13
 * boards and larger.
 *
 * For even-sized boards at least 8x8, only the four first diagrams
 * are used, because there is no way to place the center stones
 * symmetrically. As for odd-sized boards, the edge stones are moved
 * to the fourth line for boards larger than 11x11.
 *
 * At most four stones are placed on 7x7 boards too (this size may or
 * may not be supported by the rest of the engine). No handicap stones
 * are ever placed on smaller boards.
 *
 * Notice that this function only deals with fixed handicap placement.
 * Larger handicaps can be added by free placement if the used
 * interface supports it.
 */


/* This table contains the (coded) positions of the stones.
 *  2 maps to 2 or 3, depending on board size
 *  0 maps to center
 * -ve numbers map to  board_size - number
 *
 * The stones are placed in this order, *except* if there are
 * 5 or 7 stones, in which case center ({0, 0}) is placed, and
 * then as for 4 or 6.
 */

static const int places[][2] = {

  {2, -2}, {-2, 2}, {2, 2}, {-2, -2}, /* first 4 are easy */
                                      /* for 5, {0,0} is explicitly placed */
  
  {0, 2}, {0, -2},                    /* for 6 these two are placed */
                                      /* for 7, {0,0} is explicitly placed */
  
  {2, 0}, {-2, 0},                    /* for 8, these two are placed */

  {0, 0},                             /* finally tengen for 9 */
};


/*
 * Sets up handicap stones, returning the number of placed handicap stones.
 */

int
placehand(int handicap)
{
  int x;
  int maxhand;
  int three = board_size > 11 ? 3 : 2;
  int mid = board_size/2;
  int retval = handicap;

  /* A handicap of 1 just means that B plays first, no komi.
   * Black is not told where to play the first stone so no handicap
   * is set. 
   */
  if (handicap < 2)
    return 0;
  if ((board_size % 2 == 1) && (board_size >= 9))
    maxhand = 9;
  else if (board_size >= 7)
    maxhand = 4;
  else
    maxhand = 0;

  /* It's up to the caller of this function to notice if the handicap
   * was too large for fixed placement and act upon that.
   */
  if (handicap > maxhand) {
    handicap = maxhand;
    retval = maxhand;
  }

  /* special cases: 5 and 7 */
  if (handicap == 5 || handicap == 7) {
    add_stone(POS(mid, mid), BLACK);
    handicap--;
  }

  for (x = 0; x < handicap; ++x) {
    int i = places[x][0];
    int j = places[x][1];

    /* translate the encoded values to board co-ordinates */
    if (i == 2)  i = three;	/* 2 or 3 */
    if (i == -2) i = -three;

    if (j == 2)  j = three;
    if (j == -2) j = -three;

    if (i == 0) i = mid;
    if (j == 0) j = mid;

    if (i < 0) i += board_size-1;
    if (j < 0) j += board_size-1;

    add_stone(POS(i, j), BLACK);
  }

  return retval;
}


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
