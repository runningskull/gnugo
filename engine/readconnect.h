/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * This is GNU Go, a Go program. Contact gnugo@gnu.org, or see       *
 * http://www.gnu.org/software/gnugo/ for more information.          *
 *                                                                   *
 * Copyright 1999, 2000, 2001, 2002, 2003 and 2004                   *
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


struct heap_entry;
struct connection_data;

/* Expensive functions that we try to evaluate as late as possible
 * when spreading connection distances.
 */
typedef void (*connection_helper_fn_ptr) (struct connection_data *conn,
					  int color);

/* This heap contains a list of positions where we have delayed a
 * decision whether to "spread a connection distance". The function
 * helper() will be called when we finally need the decision. See
 * push_connection_heap_entry() for organization of the heap.
 */
struct heap_entry {
  float distance;
  int coming_from;
  int target;
  connection_helper_fn_ptr helper;
};

struct connection_data {
  float distances[BOARDMAX];
  float deltas[BOARDMAX];
  int coming_from[BOARDMAX];
  int vulnerable1[BOARDMAX];
  int vulnerable2[BOARDMAX];
  int queue[BOARDMAX];
  int queue_start;
  int queue_end;

  int heap_data_size;
  int heap_size;
  struct heap_entry heap_data[4 * BOARDMAX];
  struct heap_entry *heap[BOARDMAX];

  int target;
  float cutoff_distance;
  int speculative;
};


void compute_connection_distances(int str, int target, float cutoff,
				  struct connection_data *conn,
				  int speculative);
void init_connection_data(int color, const char goal[BOARDMAX],
			  int target, float cutoff,
			  struct connection_data *conn, int speculative);
void spread_connection_distances(int color, struct connection_data *conn);
void sort_connection_queue_tail(struct connection_data *conn);
void expand_connection_queue(struct connection_data *conn);
void print_connection_distances(struct connection_data *conn);


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
