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


#define CAN_BE_EMPTY		1
#define CAN_CONTAIN_STONE	2
#define EYE_DEFENSE_POINT	4
#define EYE_ATTACK_POINT	8

/*
 * The vertices of each eye are defined by an array of struct eye_vertex.
 */

struct eye_vertex {
  signed char marginal;		  /* 1 if marginal vertex, 0 otherwise     */
  signed char edge;		  /* 0 = center, 1 = edge, 2 = corner	   */
  /* A corner vertex may only be matched at the corner.
   * An edge vertex may be matched at the corner or on the edge.
   * A center vertex may be matched anywhere.
   */
  signed char flags;		  /* see the #defines above		   */

  signed char neighbors;	  /* number of neighbors                   */
  signed char n[4];		  /* position in array of vertex neighbors */
};


/*
 * Each eye is described by one struct eye_graph and the vertices
 * in the struct eye_vertex array.
 */

struct eye_graph {
  struct eye_vertex *vertex;
  int patnum;			  /* number of pattern			   */
  int esize;                      /* number of vertices                    */
  int msize;                      /* number of marginal vertices           */
  int ends;                       /* number of vertices with one neighbor  */
  int two_neighbors;              /* number of vertices with 2 neighbors   */
  int three_neighbors;            /* number of vertices with 3 neighbors   */
  struct eyevalue value;          /* eye value                             */
};

extern struct eye_graph graphs[];

/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
