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

#include "liberty.h"

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
