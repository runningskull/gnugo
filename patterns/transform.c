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
 * Boston, MA 02111, USA                                         *
\* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */



/* table for use by TRANSFORM() (patterns.h) */

const int transformations[8][2][2] = {
  { { 1,  0}, 
    { 0,  1}}, /* a - identity transformation matrix */

  { { 0,  1}, 
    {-1,  0}}, /* g - rotate 270 counter-clockwise */

  { {-1,  0}, 
    { 0, -1}}, /* d - rotate 180 */
  
  { { 0, -1}, 
    { 1,  0}}, /* f - rotate 90 counter-clockwise */
  
  { { 0, -1}, 
    {-1,  0}}, /* h - rotate 90 and invert */
  
  { {-1,  0}, 
    { 0,  1}}, /* b - flip left */
  
  { { 0,  1}, 
    { 1,  0}}, /* e - rotate 90 and flip left */
  
  { { 1,  0}, 
    { 0, -1}}  /* c - invert */
};

