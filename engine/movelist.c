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


static void movelist_sort_points(int max_points, int points[], int codes[]);
static void swap_points_and_codes(int points[], int codes[], int m, int n);


/* Return the code for the move if it is known.
 */
int
movelist_move_known(int move, int max_points, int points[], int codes[])
{
  int k;

  for (k = 0; k < max_points; k++) {
    if (codes[k] == 0)
      return 0;
    if (points[k] == move)
      return codes[k];
  }
  return 0;
}


/*
 * This function does the real work for change_attack(),
 * change_defense(), change_attack_threat(), and
 * change_defense_threat().
 */

void
movelist_change_point(int move, int code, int max_points,
		      int points[], int codes[])
{
  int k;

  /* First see if we already know about this point. */
  for (k = 0; k < max_points; k++)
    if (points[k] == move)
      break;

  /* Yes, we do. */
  if (k < max_points) {
    if (codes[k] <= code)
      return; /* Old news. */

    codes[k] = code;
    movelist_sort_points(max_points, points, codes);
    return;
  }

  /* This tactical point is new to us. */
  if (code > codes[max_points - 1]) {
    points[max_points - 1] = move;
    codes[max_points - 1] = code;
    movelist_sort_points(max_points, points, codes);
  }
}


/* Sort the tactical points so we have it sorted in falling order on
 * the code values.
 *
 * We use shaker sort because we prefer a stable sort and in all use
 * cases we can expect it to suffice with one turn through the outer
 * loop.
 */

static void
movelist_sort_points(int max_points, int points[], int codes[])
{
  int start = 0;
  int end = max_points - 1;
  int new_start;
  int new_end;
  int k;
  
  while (start < end) {
    new_start = end;
    for (k = end; k > start; k--)
      if (codes[k] > codes[k-1]) {
	swap_points_and_codes(points, codes, k, k-1);
	new_start = k;
      }
    start = new_start;
    new_end = start;
    for (k = start; k < end - 1; k++)
      if (codes[k] < codes[k+1]) {
	swap_points_and_codes(points, codes, k, k+1);
	new_end = k;
      }
    end = new_end;
  }
}

static void
swap_points_and_codes(int points[], int codes[], int m, int n)
{
  int tmp = points[m];
  points[m] = points[n];
  points[n] = tmp;
  tmp = codes[m];
  codes[m] = codes[n];
  codes[n] = tmp;
}



/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
