/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * This is GNU Go, a Go program. Contact gnugo@gnu.org, or see       *
 * http://www.gnu.org/software/gnugo/ for more information.          *
 *                                                                   *
 * Copyright 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007,   *
 * 2008, 2009 and 2010 by the Free Software Foundation.              *
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

#include "gnugo.h"
#include "liberty.h"
#include "readconnect.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>


/* This module looks for break-ins into territories that require
 * deeper tactical reading and are thus impossible to detect for the
 * influence module. It gets run after the influence module and revises
 * its territory valuations.
 *
 * The procedure is as follows: We look at all big (>= 10) territory regions
 * as detected by the influence code. Using the computation of
 * connection distances from readconnect.c, we compute all nearby vertices
 * of this territory. We look for the closest safe stones belonging to
 * the opponent.
 * For each such string (str) we call
 * - break_in(str, territory) if the opponent is assumed to be next to move,
 *   or
 * - block_off(str, territory) if the territory owner is next.
 * If the break in is successful resp. the blocking unsuccessful, we
 * shrink the territory, and see whether the opponent can still break in.
 * We repeat this until the territory is shrunk so much that the opponent
 * can no longer reach it.
 */


/* Store possible break-ins in initial position to generate move reasons
 * later.
 */
struct break_in_data {
  int str;
  int move;
};

#define MAX_BREAK_INS 50
static struct break_in_data break_in_list[MAX_BREAK_INS];
static int num_break_ins;


/* Adds all empty intersections that have two goal neighbors to the goal. */
static void
enlarge_goal(signed char goal[BOARDMAX])
{
  int pos;
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (board[pos] == EMPTY && !goal[pos]) {
      int k;
      int goal_neighbors = 0;
      for (k = 0; k < 4; k++)
	if (board[pos + delta[k]] == EMPTY && goal[pos + delta[k]] == 1)
	  goal_neighbors++;
      if (goal_neighbors >= 2)
	goal[pos] = 2;
    }
  }
}


/* The "smaller goal" is the intersection of the goal with what is
 * stored in the queue of the connection_data conn.
 * Plus we need a couple of extra careful modifications in the case
 * of "blocking off", i.e. when color_to_move == owner.
 */
static void
compute_smaller_goal(int owner, int color_to_move,
    		     const struct connection_data *conn,
    		     const signed char goal[BOARDMAX],
		     signed char smaller_goal[BOARDMAX])
{
  int k, j;
  int own_stones_visited[BOARDMAX];
  memset(smaller_goal, 0, BOARDMAX);
  for (k = 0; k < conn->queue_end; k++) {
    int pos = conn->queue[k];
    int goal_neighbors = 0;
    /* If we are trying to block-off, we need to be extra careful: We only
     * can block intrusions coming directly from the string in question.
     * Therefore, we discard the area if we have traversed more than two
     * stones of the color breaking in on the way to the goal.
     */
    if (owner == color_to_move) {
      int coming_from = conn->coming_from[pos];
      if (coming_from == NO_MOVE)
	own_stones_visited[pos] = 0;
      else {
	own_stones_visited[pos] = own_stones_visited[coming_from];
	/* How many stones have we used to jump from coming_from to pos?
	 * Use Manhattan metric as a guess.
	 */
	if (!goal[pos] && board[pos] == OTHER_COLOR(owner)) {
	  int i;
	  int stones[MAX_BOARD * MAX_BOARD];
	  int num_stones = findstones(pos, MAX_BOARD * MAX_BOARD, stones);
	  int smallest_distance = 3;

	  for (i = 0; i < num_stones; i++) {
	    int distance = (gg_abs(I(stones[i]) - I(coming_from))
			    + gg_abs(J(stones[i]) - J(coming_from)));

	    if (distance < smallest_distance)
	      smallest_distance = distance;
	  }

	  own_stones_visited[pos] += smallest_distance;
	}

	if (own_stones_visited[pos] > 2)
	  continue;
      }
    }

    if (!goal[pos])
      continue;

    /* We don't want vertices that are at the border of the territory, and
     * from which a break-in is unlikely; these often lead to false
     * positives.
     * So we throw out every vertex that has only one neighbor in the goal,
     * or that is on an edge and has only two goal neighbors.
     */
    for (j = 0; j < 4; j++)
      if (ON_BOARD(pos + delta[j])
	  && goal[pos + delta[j]]
	  && (board[pos] == EMPTY || goal[pos] == OTHER_COLOR(owner)))
	goal_neighbors++;
#if 0
    if (goal_neighbors > 2
	|| goal_neighbors == 2 && !is_edge_vertex(pos))
#else
    if (goal_neighbors >= 2)
      smaller_goal[pos] = 1;
#endif
  }

  /* Finally, in the case of blocking off, we only want one connected
   * component.
   */
  if (owner == color_to_move) {
    signed char marked[BOARDMAX];
    int sizes[BOARDMAX / 2];
    signed char mark = 0;
    int biggest_region = 1;
    memset(marked, 0, BOARDMAX);
    for (k = 0; k < conn->queue_end; k++) {
      int pos = conn->queue[k];
      if (ON_BOARD(pos) && smaller_goal[pos] && !marked[pos]) {
	/* Floodfill the connected component of (pos) in the goal. */
	int queue_start = 0;
	int queue_end = 1;
	int queue[BOARDMAX];
	mark++;
	sizes[(int) mark] = 1;
	marked[pos] = mark;
	queue[0] = pos;
	while (queue_start < queue_end) {
	  test_gray_border();
	  for (j = 0; j < 4; j++) {
	    int pos2 = queue[queue_start] + delta[j];
	    if (!ON_BOARD(pos2))
	      continue;
	    ASSERT1(marked[pos2] == 0 || marked[pos2] == mark, pos2);
	    if (smaller_goal[pos2]
		&& !marked[pos2]) {
	      sizes[(int) mark]++;
	      marked[pos2] = mark;
	      queue[queue_end++] = pos2;
	    }
	  }
	  queue_start++;
	}
      }
    }
    /* Now selected the biggest connected component. (In case of
     * equality, take the first one.
     */
    for (k = 1; k <= mark; k++) {
      if (sizes[k] > sizes[biggest_region])
	biggest_region = k;
    }
    memset(smaller_goal, 0, BOARDMAX);
    for (k = 0; k < conn->queue_end; k++) {
      int pos = conn->queue[k];
      if (marked[pos] == biggest_region)
	smaller_goal[pos] = 1;
    }
  }
}


/* Try to intrude from str into goal. If successful, we shrink the goal,
 * store the non-territory fields in the non_territory array, and
 * try again.
 */
static int
break_in_goal_from_str(int str, signed char goal[BOARDMAX],
    		      int *num_non_territory, int non_territory[BOARDMAX],
    		      int color_to_move, int info_pos)
{
  int move = NO_MOVE;
  int saved_move = NO_MOVE;
  signed char smaller_goal[BOARDMAX];
  struct connection_data conn;

  /* When blocking off, we use a somewhat smaller goal area. */
  if (color_to_move == board[str])
    compute_connection_distances(str, NO_MOVE, FP(3.01), &conn, 1);
  else
    compute_connection_distances(str, NO_MOVE, FP(2.81), &conn, 1);

  sort_connection_queue_tail(&conn);
  expand_connection_queue(&conn);
  compute_smaller_goal(OTHER_COLOR(board[str]), color_to_move,
      		       &conn, goal, smaller_goal);
  if (0 && (debug & DEBUG_BREAKIN))
    print_connection_distances(&conn);
  DEBUG(DEBUG_BREAKIN, "Trying to break in from %1m to:\n", str);
  if (debug & DEBUG_BREAKIN)
    goaldump(smaller_goal);
  while ((color_to_move == board[str]
          && break_in(str, smaller_goal, &move))
         || (color_to_move == OTHER_COLOR(board[str])
	     && !block_off(str, smaller_goal, NULL))) { 
    /* Successful break-in/unsuccessful block. Now where exactly can we
     * erase territory? This is difficult, and the method here is very
     * crude: Wherever we enter the territory when computing the closest
     * neighbors of (str). Plus at the location of the break-in move.
     * FIXME: This needs improvement.
     */
    int k;
    int save_num = *num_non_territory;
    int affected_size = 0;
    int cut_off_distance = FP(3.5);
    if (ON_BOARD(move) && goal[move]) {
      non_territory[(*num_non_territory)++] = move;
      if (info_pos)
	DEBUG(DEBUG_TERRITORY | DEBUG_BREAKIN,
	      "%1m: Erasing territory at %1m -a.\n", info_pos, move);
      else
	DEBUG(DEBUG_TERRITORY | DEBUG_BREAKIN,
	      "Erasing territory at %1m -a.\n", move);
    }

    for (k = 0; k < conn.queue_end; k++) {
      int pos = conn.queue[k];
      if (conn.distances[pos] > cut_off_distance + FP(0.31))
	break;
      if (goal[pos]
	  && (!ON_BOARD(conn.coming_from[pos])
	      || !goal[conn.coming_from[pos]])) {
	non_territory[(*num_non_territory)++] = pos;
	if (info_pos)
	  DEBUG(DEBUG_TERRITORY | DEBUG_BREAKIN,
		"%1m: Erasing territory at %1m -b.\n", info_pos, pos);
	else
	  DEBUG(DEBUG_TERRITORY | DEBUG_BREAKIN,
	        "Erasing territory at %1m -b.\n", pos);
	if (conn.distances[pos] < cut_off_distance)
	  cut_off_distance = conn.distances[pos];
      }
      if (*num_non_territory >= save_num + 4)
	break;
    }

    /* Shouldn't happen, but it does. */
    if (*num_non_territory == save_num)
      break;

    for (k = save_num; k < *num_non_territory; k++) {
      int j;
      int pos = non_territory[k];
      if (goal[pos]) {
	affected_size++;
	goal[pos] = 0;
      }
      for (j = 0; j < 4; j++)
	if (ON_BOARD(pos + delta[j]) && goal[pos + delta[j]])
	  affected_size++;
      /* Don't kill too much territory at a time. */
      if (affected_size >= 5) {
	*num_non_territory = k;
	break;
      }
    }

    compute_smaller_goal(OTHER_COLOR(board[str]), color_to_move,
			 &conn, goal, smaller_goal);
    DEBUG(DEBUG_BREAKIN, "Now trying to break to smaller goal:\n", str);
    if (debug & DEBUG_BREAKIN)
      goaldump(smaller_goal);

    if (saved_move == NO_MOVE)
      saved_move = move;
  }
  return saved_move;
}

#define MAX_TRIES 10

static void
break_in_goal(int color_to_move, int owner, signed char goal[BOARDMAX],
    	      struct influence_data *q, int store, int info_pos)
{
  struct connection_data conn;
  int k;
  int intruder = OTHER_COLOR(owner);
  signed char used[BOARDMAX];
  int non_territory[BOARDMAX];
  int num_non_territory = 0;
  int candidate_strings[MAX_TRIES];
  int candidates = 0;
  int min_distance = FP(5.0);

  DEBUG(DEBUG_BREAKIN,
        "Trying to break (%C to move) %C's territory ", color_to_move, owner);
  if (debug & DEBUG_BREAKIN)
    goaldump(goal);
  /* Compute nearby fields of goal. */
  init_connection_data(intruder, goal, NO_MOVE, FP(3.01), &conn, 1);
  k = conn.queue_end;
  spread_connection_distances(intruder, &conn);
  sort_connection_queue_tail(&conn);
  if (0 && (debug & DEBUG_BREAKIN))
    print_connection_distances(&conn);

  /* Look for nearby stones. */
  memset(used, 0, BOARDMAX);
  for (; k < conn.queue_end; k++) {
    int pos = conn.queue[k];
    if (conn.distances[pos] > min_distance + FP(1.001))
      break;
    if (board[pos] == intruder
	&& influence_considered_lively(q, pos)) {
      /* Discard this string in case the shortest path goes via a string
       * that we have in the candidate list already.
       */
      int pos2 = pos;
      while (ON_BOARD(pos2)) {
        pos2 = conn.coming_from[pos2];
	if (IS_STONE(board[pos2]))
	  pos2 = find_origin(pos2);

	if (used[pos2])
	  break;
      }

      used[pos] = 1;
      if (ON_BOARD(pos2))
	continue;
      if (candidates == 0)
	min_distance = conn.distances[pos];
      candidate_strings[candidates++] = pos;
      if (candidates == MAX_TRIES)
	break;
    }
  }

  /* Finally, try the break-ins. */
  memset(non_territory, 0, BOARDMAX);
  for (k = 0; k < candidates; k++) {
    int move = break_in_goal_from_str(candidate_strings[k], goal,
  		                     &num_non_territory, non_territory,
				     color_to_move, info_pos);
    if (store && ON_BOARD(move) && num_break_ins < MAX_BREAK_INS) {
      /* Remember the move as a possible move candidate for later. */
      break_in_list[num_break_ins].str = candidate_strings[k];
      break_in_list[num_break_ins].move = move;
      num_break_ins++;
    }
  }

  for (k = 0; k < num_non_territory; k++)
    influence_erase_territory(q, non_territory[k], owner);
  if (0 && num_non_territory > 0 && (debug & DEBUG_BREAKIN))
    showboard(0);
}


/* The main function of this module. color_to_move is self-explanatory,
 * and the influence_data refers to the influence territory evaluation that
 * we are analyzing (and will be correcting). store indicates whether
 * the successful break-ins should be stored in the break_in_list[] (which
 * later gets used to generate move reasons).
 */
void
break_territories(int color_to_move, struct influence_data *q, int store,
    		  int info_pos)
{
  struct moyo_data territories;
  int k;

  if (!experimental_break_in || get_level() < 10)
    return;

  influence_get_territory_segmentation(q, &territories);
  for (k = 1; k <= territories.number; k++) {
    signed char goal[BOARDMAX];
    int pos;
    int size = 0;

    memset(goal, 0, BOARDMAX);
    for (pos = BOARDMIN; pos < BOARDMAX; pos++)
      if (ON_BOARD(pos) && territories.segmentation[pos] == k) {
	goal[pos] = 1;
	if (board[pos] != territories.owner[k])
	  size++;
      }
    if (size < 10)
      continue;

    if (color_to_move == OTHER_COLOR(territories.owner[k]))
      enlarge_goal(goal);
    break_in_goal(color_to_move, territories.owner[k], goal, q, store,
		  info_pos);
  }
}

void
clear_break_in_list()
{
  num_break_ins = 0;
}

/* The blocking moves should usually already have a move reason.
 *
 * The EXPAND_TERRITORY move reason ensures a territory evaluation of
 * this move, without setting the move.safety field. (I.e. the move will
 * be treated as a sacrifice move unless another move reasons tells us
 * otherwise.)
 */
void
break_in_move_reasons(int color)
{
  int k;
  for (k = 0; k < num_break_ins; k++)
    if (board[break_in_list[k].str] == color)
      add_expand_territory_move(break_in_list[k].move);
}
