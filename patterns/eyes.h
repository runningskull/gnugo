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


/*
 * The vertices of each eye is defined by an array of struct eye_vertex.
 */

struct eye_vertex {
  int i;                          /* coordinates of the vertex             */
  int j; 
  char type;                      /* . x or X                              */

  int neighbors;                  /* number of neighbors                   */
  int n1;                         /* first neighbor (position in array)    */
  int n2;                         /* second neighbor                       */
  int n3;                         /* third neighbor                        */
  int n4;                         /* fourth neighbor                       */
  int edge;                       /* 0=center, 1=edge, 2=corner            */
  /* A corner vertex may only be matched at the corner.
   * An edge vertex may be matched at the corner or on the edge.
   * A center vertex may be matched anywhere.
   */
};


/*
 * Each eye is described by one struct eye_graph and the vertices
 * in the struct eye_vertex array.
 */

struct eye_graph {
  struct eye_vertex *vertex;
  int id;                         /* serial number of pattern              */
  int esize;                      /* number of vertices                    */
  int msize;                      /* number of marginal vertices           */
  int ends;                       /* number of vertices with one neighbor  */
  int two_neighbors;              /* number of vertices with 2 neighbors   */
  int three_neighbors;            /* number of vertices with 3 neighbors   */
  int max;                        /* number of eyes if O plays first       */
  int min;                        /* number of eyes if X plays first       */
};

extern struct eye_graph graphs[];

/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
