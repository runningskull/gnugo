/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * This is GNU Go, a Go program. Contact gnugo@gnu.org, or see       *
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


struct connection_data {
  float distances[BOARDMAX];
  float deltas[BOARDMAX];
  int coming_from[BOARDMAX];
  int vulnerable1[BOARDMAX];
  int vulnerable2[BOARDMAX];
  int queue[BOARDMAX];
  int queue_start;
  int queue_end;
};

void compute_connection_distances(int str, int target, float cutoff,
				  struct connection_data *conn);
void init_connection_data(int color, const char goal[BOARDMAX],
		                     struct connection_data *conn);
void spread_connection_distances(int color, int target,
				 struct connection_data *conn,
    			    	 float cutoff_distance,
				 int speculative);
void print_connection_distances(struct connection_data *conn);


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
