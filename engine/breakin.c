/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * This is GNU GO, a Go program. Contact gnugo@gnu.org, or see       *
 * http://www.gnu.org/software/gnugo/ for more information.          *
 *                                                                   *
 * Copyright 2003 by the Free Software Foundation.                   *
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
 * If the break in is successful resp. the blocking insuccessful, we
 * shrink the territory, and see whether the opponent can still break in.
 * We repeat this until the territory is shrinked so much that the opponent
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
struct break_in_data break_in_list[MAX_BREAK_INS];
int num_break_ins;


/* Adds all empty intersections that have two goal neighbors to the goal. */
static void
enlarge_goal(char goal[BOARDMAX])
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

/* Try to intrude from str into goal. If successful, we shrink the goal,
 * store the non-territory fields in the non_territory array, and
 * try again.
 */
static int
break_in_goal_from_str(int str, char goal[BOARDMAX],
    		      int *num_non_territory, int non_territory[BOARDMAX],
    		      int color_to_move)
{
  int move = NO_MOVE;

  DEBUG(DEBUG_TERRITORY, "Trying to break in from %1m\n", str);
  while ((color_to_move == board[str]
          && break_in(str, goal, &move))
         || (color_to_move == OTHER_COLOR(board[str])
	     && !block_off(str, goal, NULL))) { 
    /* Successful break-in/unsuccessful block. Now where exactly can we
     * erase territory? This is difficult, and the method here is very
     * crude: Wherever we enter the territory when computing the closest
     * neighbors of (str). Plus at the locaction of the break-in move.
     * FIXME: This needs improvement.
     */
    struct connection_data conn;
    int k;
    int save_num = *num_non_territory;
    float min_distance = 5.0;
    if (ON_BOARD(move) && goal[move]) {
      non_territory[(*num_non_territory)++] = move;
      DEBUG(DEBUG_TERRITORY, "Erasing territory at %1m -a.\n", move);
    }

    compute_connection_distances(str, NO_MOVE, 3.01, &conn);
    for (k = 0; k < conn.queue_end; k++) {
      int pos = conn.queue[k];
      if (conn.distances[pos] > min_distance + 0.31)
	break;
      if (goal[pos]
	  && (!ON_BOARD(conn.coming_from[pos])
	      || !goal[conn.coming_from[pos]])) {
	non_territory[(*num_non_territory)++] = pos;
	DEBUG(DEBUG_TERRITORY, "Erasing territory at %1m -b.\n", pos);
	if (conn.distances[pos] < min_distance)
	  min_distance = conn.distances[pos];
      }
      if (*num_non_territory >= save_num + 5)
	break;
    }
    /* Shouldn't happen, but it does. */
    if (*num_non_territory == save_num)
      break;
    for (k = save_num; k < *num_non_territory; k++)
      goal[non_territory[k]] = 0;
  }
  return move;
}

#define MAX_TRIES 10

static void
break_in_goal(int color_to_move, int owner, char goal[BOARDMAX],
    	      struct influence_data *q, int store)
{
  struct connection_data conn;
  int k;
  int intruder = OTHER_COLOR(owner);
  char used[BOARDMAX];
  int non_territory[BOARDMAX];
  int num_non_territory = 0;
  int candidate_strings[MAX_TRIES];
  int candidates = 0;
  float min_distance = 5.0;

  DEBUG(DEBUG_TERRITORY,
        "Trying to break (%C to move) %C's territory ", color_to_move, owner);
  if (debug & DEBUG_TERRITORY)
    goaldump(goal);
  /* Compute nearby fields of goal. */
  init_connection_data(owner, goal, &conn);
  k = conn.queue_end;
  spread_connection_distances(intruder, NO_MOVE, &conn, 3.01, 1);
  if (debug & DEBUG_TERRITORY)
    print_connection_distances(&conn);

  /* Look for nearby stones. */
  memset(used, 0, BOARDMAX);
  for (; k < conn.queue_end; k++) {
    int pos = conn.queue[k];
    if (conn.distances[pos] > min_distance + 1.001)
      break;
    if (board[pos] == intruder
	&& influence_considered_safe(q, pos)
	&& !used[pos]) {
      /* Discard this string in case the shortest path goes via a string
       * that we have in the candidate list already.
       */
      int pos2 = pos;
      while (ON_BOARD(pos2)) {
        pos2 = conn.coming_from[pos2];
	if (used[pos2])
	  break;
      }
      mark_string(pos, used, 1);
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
				     color_to_move);
    if (store && ON_BOARD(move) && num_break_ins < MAX_BREAK_INS) {
      /* Remember the move as a possible move candidate for later. */
      break_in_list[num_break_ins].str = candidate_strings[k];
      break_in_list[num_break_ins].move = move;
      num_break_ins++;
    }
  }

  for (k = 0; k < num_non_territory; k++)
    influence_erase_territory(q, non_territory[k], owner);
  if (1 && num_non_territory > 0 && (debug & DEBUG_TERRITORY))
    showboard(0);
}


/* The main function of this module. color_to_move is self-explanatory,
 * and the influence_data refers to the influence territory evaluation that
 * we are analyzing (and will be correcting). store indicates whether
 * the successful break-ins should be stored in the break_in_list[] (which
 * later gets used to generate move reasons).
 */
void
break_territories(int color_to_move, struct influence_data *q, int store)
{
  struct moyo_data territories;
  int k;

  if (!experimental_break_in)
    return;

  influence_get_territory_segmentation(q, &territories);
  for (k = 1; k <= territories.number; k++) {
    char goal[BOARDMAX];
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
      return;

    if (color_to_move == OTHER_COLOR(territories.owner[k]))
      enlarge_goal(goal);
    break_in_goal(color_to_move, territories.owner[k], goal, q, store);
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
 * this move, without setting the move.safety field.
 */
void
break_in_move_reasons(int color)
{
  int k;
  for (k = 0; k < num_break_ins; k++)
    if (board[break_in_list[k].str] == color)
      add_expand_territory_move(break_in_list[k].move);
}
