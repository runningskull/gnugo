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
#include <stdarg.h>
#include <string.h>

#include "liberty.h"
#include "cache.h"
#include "gg_utils.h"

/* Size of array where candidate moves are stored. */
#define MAX_MOVES 362

static int recursive_connect2(int str1, int str2, int *move,
			      int komaster, int kom_pos, int has_passed);
static int recursive_disconnect2(int str1, int str2, int *move,
				 int komaster, int kom_pos, int has_passed);

int nodes_connect = 0;
int max_nodes_connect = 2000;
int max_connect_depth2 = 20;

/* Statistics. */
static int global_connection_node_counter = 0;


/* Externally callable frontend to recursive_connect(). */

int
string_connect(int str1, int str2, int *move)
{
  int dummy_move;
  int save_verbose;
  int result;

  if (move == NULL)
    move = &dummy_move;
  
  nodes_connect = 0;
  *move = PASS_MOVE;
  
  save_verbose = verbose;
  if (verbose > 0)
    verbose--;
  result = recursive_connect2(str1, str2, move, EMPTY, NO_MOVE, 0);
  verbose = save_verbose;

  return result;
}


/* Externally callable frontend to recursive_disconnect(). */

int
disconnect(int str1, int str2, int *move)
{
  int dummy_move;
  int result;
  int save_verbose;
  
  if (move == NULL)
    move = &dummy_move;
  
  nodes_connect = 0;
  *move = PASS_MOVE;
  
  save_verbose = verbose;
  if (verbose > 0)
    verbose--;
  result = recursive_disconnect2(str1, str2, move, EMPTY, NO_MOVE, 0);
  verbose = save_verbose;

  return result;
}


/* Clear statistics. */
void
reset_connection_node_counter()
{
  global_connection_node_counter = 0;
}


/* Retrieve statistics. */
int
get_connection_node_counter()
{
  return global_connection_node_counter;
}


/*********************************************************
 *
 * Alternate connection reading algorithm.
 *
 *********************************************************/

/* This has been copied from reading.c and modified.
 */

#define ADD_CANDIDATE_MOVE(move, distance, moves, distances, num_moves)\
  do {\
    int l;\
    for (l = 0; l < num_moves; l++)\
      if (moves[l] == (move)) {\
        if (distances[l] > distance)\
          distances[l] = distance;\
	break;\
      }\
    if ((l == num_moves) && (num_moves < MAX_MOVES)) {\
      moves[num_moves] = move;\
      distances[num_moves] = distance;\
      (num_moves)++;\
    }\
  } while (0)


struct connection_data {
  int distances[BOARDMAX];
  int deltas[BOARDMAX];
  int coming_from[BOARDMAX];
  int vulnerable1[BOARDMAX];
  int vulnerable2[BOARDMAX];
  int queue[BOARDMAX];
  int queue_start;
  int queue_end;
};

#define HUGE_CONNECTION_DISTANCE 100000

static int find_connection_moves(int str1, int str2, int color_to_move,
				 int moves[MAX_MOVES], int *total_distance);
static void compute_connection_distances(int str, int target,
					 struct connection_data *conn);
static void print_connection_distances(struct connection_data *conn);
static int trivial_connection(int str1, int str2, int *move);
static int does_secure(int color, int move, int pos);
static int does_secure_through_ladder(int color, int move, int pos);
static int ladder_capture(int str, int *move);
static int ladder_capturable(int pos, int color);
static int no_escape_from_atari(int str);
static int no_escape_from_ladder(int str);
static int check_self_atari(int pos, int color_to_move);
static int common_vulnerabilities(int a1, int a2, int b1, int b2, int color);
static int common_vulnerability(int apos, int bpos, int color);


/* Try to connect two strings. This function is called in a mutual
 * recursion with recursive_disconnect2(). Return codes and the
 * meaning of komaster and kom_pos is identical to the tactical
 * reading functions. For the has_passed parameter, see the
 * documentation of recursive_disconnect2().
 *
 * The algorithm is
 * 1. Check if the strings are trivially connected or disconnected or
 *    the result is already cached.
 * 2. Find connection moves.
 * 3. Try one move at a time and call recursive_disconnect2() to see
 *    whether we were successful.
 * 4. If no move was found we assume success if the connection
 *    distance was small and failure otherwise.
 */
static int
recursive_connect2(int str1, int str2, int *move, int komaster, int kom_pos,
		   int has_passed)
{
  int color = board[str1];
  int moves[MAX_MOVES];
  int num_moves;
  int distance = 0;
  int k;
  int xpos;
  int savemove = NO_MOVE;
  int savecode = 0;
  int found_read_result;
  Read_result *read_result = NULL;
  
  SETUP_TRACE_INFO2("recursive_connect2", str1, str2);

  if (move)
    *move = NO_MOVE;

  nodes_connect++;
  global_connection_node_counter++;
  
  if (board[str1] == EMPTY || board[str2] == EMPTY) {
    SGFTRACE2(PASS_MOVE, 0, "one string already captured");
    return 0;
  }
  
  if (same_string(str1, str2)) {
    SGFTRACE2(PASS_MOVE, WIN, "already connected");
    return WIN;
  }

  if (nodes_connect > max_nodes_connect) {
    SGFTRACE2(PASS_MOVE, 0, "connection node limit reached");
    return 0;
  }
  
  if (stackp > max_connect_depth2) {
    SGFTRACE2(PASS_MOVE, 0, "connection depth limit reached");
    return 0;
  }

  if (stackp <= depth
      && (hashflags & HASH_CONNECT)
      && !has_passed) {
    found_read_result = get_read_result2(CONNECT, komaster, kom_pos, 
					 &str1, &str2, &read_result);
    if (found_read_result) {
      TRACE_CACHED_RESULT2(*read_result);
      if (rr_get_result(*read_result) != 0)
	if (move)
	  *move = rr_get_move(*read_result);

      SGFTRACE2(rr_get_move(*read_result),
		rr_get_result(*read_result), "cached");
      return rr_get_result(*read_result);
    }
  }
  
  if (trivial_connection(str1, str2, &xpos) == WIN) {
    SGFTRACE2(xpos, WIN, "trivial connection");
    READ_RETURN_CONN(read_result, move, xpos, WIN);
  }
  
  num_moves = find_connection_moves(str1, str2, color, moves, &distance);
  
  for (k = 0; k < num_moves; k++) {
    int new_komaster;
    int new_kom_pos;
    int ko_move;

    xpos = moves[k];
    
    if (komaster_trymove(xpos, color, "recursive_connect2", str1,
			 komaster, kom_pos, &new_komaster, &new_kom_pos,
			 &ko_move, stackp <= ko_depth && savecode == 0)) {
      if (!ko_move) {
	int acode = recursive_disconnect2(str1, str2, NULL,
					  new_komaster, new_kom_pos,
					  has_passed);
	popgo();
	if (acode == 0) {
	  SGFTRACE2(xpos, WIN, "connection effective");
	  READ_RETURN_CONN(read_result, move, xpos, WIN);
	}
	/* if the move works with ko we save it, then look for something
	 * better.
	 */
	UPDATE_SAVED_KO_RESULT(savecode, savemove, acode, xpos);
      }
      else {
	if (recursive_disconnect2(str1, str2, NULL,
				  new_komaster, new_kom_pos,
				  has_passed) != WIN) {
	  savemove = xpos;
	  savecode = KO_B;
	}
	popgo();
      }
    }
  }

  if (num_moves == 0 && distance < 1000) {
    SGFTRACE2(NO_MOVE, WIN, "no move, probably connected");
    READ_RETURN_CONN(read_result, move, NO_MOVE, WIN);
  }
  
  if (savecode != 0) {
    SGFTRACE2(savemove, savecode, "saved move");
    READ_RETURN_CONN(read_result, move, savemove, savecode);
  }

  SGFTRACE2(0, 0, NULL);
  READ_RETURN_CONN(read_result, move, NO_MOVE, 0);
}


/* Try to disconnect two strings. This function is called in a mutual
 * recursion with recursive_connect2(). Return codes and the meaning
 * of komaster and kom_pos is identical to the tactical reading
 * functions.
 *
 * The algorithm is
 * 1. Check if the strings are trivially connected or disconnected or
 *    the result is already cached.
 * 2. Find disconnection moves.
 * 3. Try one move at a time and call recursive_connect2() to see
 *    whether we were successful.
 * 4. If no move was found we assume failure if the connection
 *    distance was small. Otherwise we pass and let
 *    recursive_connect2() try to connect. However, if we already have
 *    passed once we just declare success. Whether a pass already has
 *    been made is indicated by the has_passed parameter.
 */
static int
recursive_disconnect2(int str1, int str2, int *move, int komaster, int kom_pos,
		      int has_passed)
{
  int color = board[str1];
  int other = OTHER_COLOR(color);
  int moves[MAX_MOVES];
  int num_moves;
  int distance = 0;
  int k;
  int xpos;
  int savemove = NO_MOVE;
  int savecode = 0;
  int found_read_result;
  Read_result *read_result = NULL;

  SETUP_TRACE_INFO2("recursive_disconnect2", str1, str2);
  
  nodes_connect++;
  global_connection_node_counter++;

  if (move)
    *move = NO_MOVE;
  
  if (board[str1] == EMPTY || board[str2] == EMPTY) {
    SGFTRACE2(PASS_MOVE, WIN, "one string already captured");
    return WIN;
  }

  if (same_string(str1, str2)) {
    SGFTRACE2(PASS_MOVE, 0, "already connected");
    return 0;
  }
  
  if (nodes_connect > max_nodes_connect) {
    SGFTRACE2(PASS_MOVE, WIN, "connection node limit reached");
    return WIN;
  }
  
  if (stackp > max_connect_depth2) {
    SGFTRACE2(PASS_MOVE, WIN, "connection depth limit reached");
    return WIN;
  }
  
  if ((stackp <= depth) && (hashflags & HASH_DISCONNECT)) {
    found_read_result = get_read_result2(DISCONNECT, komaster, kom_pos, 
					 &str1, &str2, &read_result);
    if (found_read_result) {
      TRACE_CACHED_RESULT2(*read_result);
      if (rr_get_result(*read_result) != 0)
	if (move)
	  *move = rr_get_move(*read_result);

      SGFTRACE2(rr_get_move(*read_result),
		rr_get_result(*read_result), "cached");
      return rr_get_result(*read_result);
    }
  }

  if (ladder_capture(str1, &xpos) == WIN) {
    SGFTRACE2(xpos, WIN, "first string capturable");
    READ_RETURN_CONN(read_result, move, xpos, WIN);
  }
  
  if (ladder_capture(str2, &xpos) == WIN) {
    SGFTRACE2(xpos, WIN, "second string capturable");
    READ_RETURN_CONN(read_result, move, xpos, WIN);
  }

  num_moves = find_connection_moves(str1, str2, other, moves, &distance);
  
  for (k = 0; k < num_moves; k++) {
    int new_komaster;
    int new_kom_pos;
    int ko_move;

    xpos = moves[k];
    
    if (komaster_trymove(xpos, other, "recursive_disconnect2", str1,
			 komaster, kom_pos, &new_komaster, &new_kom_pos,
			 &ko_move, stackp <= ko_depth && savecode == 0)) {
      if (!ko_move) {
	int dcode = recursive_connect2(str1, str2, NULL,
				       new_komaster, new_kom_pos, has_passed);
	popgo();
	if (dcode == 0) {
	  SGFTRACE2(xpos, WIN, "disconnection effective");
	  READ_RETURN_CONN(read_result, move, xpos, WIN);
	}
	/* if the move works with ko we save it, then look for something
	 * better.
	 */
	UPDATE_SAVED_KO_RESULT(savecode, savemove, dcode, xpos);
      }
      else {
	if (recursive_connect2(str1, str2, NULL,
			       new_komaster, new_kom_pos,
			       has_passed) != WIN) {
	  savemove = xpos;
	  savecode = KO_B;
	}
	popgo();
      }
    }
  }

  if (num_moves == 0
      && distance >= 1000
      && (has_passed
	  || !recursive_connect2(str1, str2, NULL, komaster, kom_pos, 1))) {
    SGFTRACE2(NO_MOVE, WIN, "no move, probably disconnected");
    READ_RETURN_CONN(read_result, move, NO_MOVE, WIN);
  }
  
  if (savecode != 0) {
    SGFTRACE2(savemove, savecode, "saved move");
    READ_RETURN_CONN(read_result, move, savemove, savecode);
  }

  SGFTRACE2(0, 0, NULL);
  READ_RETURN_CONN(read_result, move, NO_MOVE, 0);
}


/* Find moves to connect or disconnect the two strings str1 and str2.
 * If color_to_move equals the color of the strings we search for
 * connecting moves and otherwise disconnecting moves. The moves are
 * returned in the moves[] array and the number of moves is the return
 * value of the function. The parameter *total_distance is set to the
 * approximated connection distance between the two strings. This is
 * most useful when no moves are found. If *total_distance is small
 * they are probably already effectively connected and if it is huge
 * they are probably disconnected.
 *
 * The algorithm is to compute connection distances around each string
 * and find points where the sum of the distances is small, or more
 * exactly where the sum of the distances after the move would be
 * small. This can be done with help of delta values returned together
 * with distance values from the function
 * compute_connection_distances(). This "distance after move" measure
 * is modified with various bonuses and then used to order the found
 * moves.
 */
static int
find_connection_moves(int str1, int str2, int color_to_move,
		      int moves[MAX_MOVES], int *total_distance)
{
  int color = board[str1];
  int other = OTHER_COLOR(color);
  int connect_move = (color_to_move == color);
  int r;
  struct connection_data conn1;
  struct connection_data conn2;
  int distances[MAX_MOVES];
  int num_moves = 0;
  SGFTree *save_sgf_dumptree = sgf_dumptree;
  int save_count_variations = count_variations;
  int acode = 0;
  int attack_move = NO_MOVE;
  int dcode = 0;
  int defense_move = NO_MOVE;
  int max_dist1;
  int max_dist2;
  int lib;
  int k;
  int i, j;

  /* We turn off the sgf traces here to avoid cluttering them up with
   * tactical reading moves.
   */
  sgf_dumptree = NULL;
  count_variations = 0;

  compute_connection_distances(str1, str2, &conn1);
  compute_connection_distances(str2, str1, &conn2);

  if (findlib(str1, 1, &lib) == 1) {
    conn1.distances[lib] = 0;
    conn1.coming_from[lib] = NO_MOVE;
    conn2.distances[lib] = conn2.distances[str1];
    conn2.coming_from[lib] = conn1.coming_from[str1];
  }

  if (findlib(str2, 1, &lib) == 1) {
    conn2.distances[lib] = 0;
    conn1.distances[lib] = conn1.distances[str2];
  }


  max_dist1 = conn1.distances[str2];
  max_dist2 = conn2.distances[str1];

  *total_distance = gg_min(max_dist1, max_dist2);

  if (verbose > 0) {
    gprintf("%oVariation %d\n", save_count_variations);
    dump_stack();
    showboard(0);
    print_connection_distances(&conn1);
    print_connection_distances(&conn2);
  }

  /* Loop through the points with smallish distance from str1 and look
   * for ones also having a small distance to str2.
   */
  for (r = 0; r < conn1.queue_end; r++) {
    int pos = conn1.queue[r];
    int dist1 = conn1.distances[pos];
    int deltadist1 = conn1.deltas[pos];
    int dist2 = conn2.distances[pos];
    int deltadist2 = conn2.deltas[pos];
    int d1;
    int d2;
    int distance;
    
    if (dist1 - deltadist1 + dist2 - deltadist2 > 2500
	|| dist1 > max_dist1 + 200
	|| dist2 > max_dist2 + 200)
      continue;

    if (IS_STONE(board[pos]) && find_origin(pos) != pos)
      continue;

    if (verbose > 0)
      gprintf("%oMove %1m, (%d, %d, %d, %d)\n",
	      pos, dist1, deltadist1, dist2, deltadist2);

    /* The basic quality of the move is the sum of the distances to
     * each string minus the two delta values. This distance value
     * will subsequently be modified to take other factors into
     * account.
     */
    d1 = dist1 - deltadist1;
    d2 = dist2 - deltadist2;
    distance = d1 + d2;
    if (verbose > 0)
      gprintf("%o  %d, primary distance\n", distance);
    
    /* Bonus if d1 and d2 are well balanced. */
    if (3 * d1 > 2 * d2 && 3 * d2 > 2 * d1) {
      distance -= 100;
      if (verbose > 0)
	gprintf("%o  -100, well balanced\n");
    }

    /* Check whether the move is "between" the two strings. */
    if (conn1.coming_from[pos] != NO_MOVE
	&& conn1.coming_from[pos] == conn2.coming_from[pos]) {
      if (verbose > 0)
	gprintf("%o  discarded, not between strings\n");
      continue;
    }
    
    if (board[pos] == EMPTY) {
      if (check_self_atari(pos, color_to_move)) {
	ADD_CANDIDATE_MOVE(pos, distance, moves, distances, num_moves);
      }
      else {
	if (verbose > 0)
	  gprintf("%o  discarded, self atari\n");
      }
    }
    else if (board[pos] == other) {
      attack_and_defend(pos, &acode, &attack_move, &dcode, &defense_move);
      if (verbose > 0)
	gprintf("%o  attack with code %d at %1m, defense with code %d at %1m\n",
		acode, attack_move, dcode, defense_move);
      
      if (connect_move && acode != 0) {
	if (dcode == 0) {
	  distance += 500;
	  if (verbose > 0)
	    gprintf("%o  +500, no defense\n");
	}
	else {
	  if (conn1.distances[attack_move]
	      + conn2.distances[attack_move] > dist1 + dist2) {
	    distance += 500;
	    if (verbose > 0)
	      gprintf("%o  +500, attack point not on shortest path\n");
	  }
	}
	ADD_CANDIDATE_MOVE(attack_move, distance - 150, moves, distances,
			   num_moves);
	if (verbose > 0)
	  gprintf("%o  -150 at %1m, capturing a string\n", attack_move);
      }
      else if (!connect_move && acode != 0 && dcode != 0) {
	ADD_CANDIDATE_MOVE(defense_move, distance - 500, moves, distances,
			   num_moves);
	if (verbose > 0)
	  gprintf("%o  -500 at %1m, defending a string\n", defense_move);
      }
    }
    else if (board[pos] == color) {
      /* Check whether there are common vulnerable points. */
      for (k = 0; k < 4; k++) {
	int apos, bpos;
	if (k & 1)
	  apos = conn1.vulnerable1[pos];
	else 
	  apos = conn1.vulnerable2[pos];

	if (k & 2)
	  bpos = conn2.vulnerable1[pos];
	else 
	  bpos = conn2.vulnerable2[pos];

	if (common_vulnerability(apos, bpos, color)) {
	  if (check_self_atari(apos, color_to_move)) {
	    ADD_CANDIDATE_MOVE(apos, distance, moves, distances, num_moves);
	    if (verbose > 0)
	      gprintf("%o  +0 at %1m, vulnerability\n", apos);
	  }

	  if (bpos != apos
	      && check_self_atari(bpos, color_to_move)) {
	    ADD_CANDIDATE_MOVE(bpos, distance, moves, distances, num_moves);
	    if (verbose > 0)
	      gprintf("%o  +0 at %1m, vulnerability\n", bpos);
	  }
	}
      } 
    }
  }

  /* Turn the sgf traces back on. */
  sgf_dumptree = save_sgf_dumptree;
  count_variations = save_count_variations;

  /* Modify the distance values for the moves with various bonuses. */
  for (r = 0; r < num_moves; r++) {
    int move = moves[r];
    int adjacent_to_attacker = 0;

    for (k = 0; k < 4; k++) {
      int pos = move + delta[k];
      if (board[pos] == other) {
	adjacent_to_attacker = 1;
	distances[r] -= 150;
	if (verbose > 0)
	  gprintf("%o%1M -150, adjacent to attacker string\n", move);
	if (countlib(pos) <= 2) {
	  distances[r] -= 200;
	  if (verbose > 0)
	    gprintf("%o%1M -200, adjacent to attacker string with at most two liberties\n", move);
	  if ((conn1.distances[move] - conn1.deltas[move] <= 500
	       || conn1.distances[pos] - conn1.deltas[pos] <= 500)
	      && (conn2.distances[move] - conn2.deltas[move] <= 500
		  || conn2.distances[pos] - conn2.deltas[pos] <= 500)
	      && conn1.distances[pos] < *total_distance
	      && conn2.distances[pos] < *total_distance) {
	    distances[r] -= 700;
	    if (verbose > 0)
	      gprintf("%o%1M -700, capture or atari of immediately connecting string\n", move);
	  }
	}
      }
      else if (board[pos] == color) {
	if (countlib(pos) <= 2) {
	  distances[r] -= 200;
	  if (verbose > 0)
	    gprintf("%o%1M -200, adjacent to defender string with at most two liberties\n", move);
	}
      }
    }
    if (adjacent_to_attacker
	&& color != color_to_move
	&& is_edge_vertex(move)) {
      distances[r] -= 100;
      if (verbose > 0)
	gprintf("%o%1M -100, disconnect move on edge\n", move);
    }

    /* Bonus for moves adjacent to endpoint strings with 3 liberties.
     * Neighbor strings with less than 3 liberties have already
     * generated a bonus above.
     */
    if ((liberty_of_string(move, str1)
	 && countlib(str1) == 3)
	|| (liberty_of_string(move, str2)
	    && countlib(str2) == 3)) {
      distances[r] -= 100;
      if (verbose > 0)
	gprintf("%o%1M -100, liberty of endpoint string with 3 libs\n", move);
    }
  }

  /* Now sort the moves.  We use selection sort since this array will
   * probably never be more than 10 moves long.  In this case, the
   * overhead imposed by quicksort will probably overshadow the gains
   * given by the O(n*log(n)) behaviour over the O(n^2) behaviour of
   * selection sort.
   */
  for (i = 0; i < num_moves; i++) {
    /* Find the move with the smallest distance. */
    int mindistance = distances[i];
    int min_at = i;
    for (j = i + 1; j < num_moves; j++) {
      if (distances[j] < mindistance) {
	mindistance = distances[j];
	min_at = j;
      }
    }

    /* Now exchange the move at i with the move at min_at.
     * Don't forget to exchange the distances as well.
     */
    if (min_at != i) {
      int temp = moves[i];
      int tempmin = distances[i];

      moves[i] = moves[min_at];
      distances[i] = distances[min_at];

      moves[min_at] = temp;
      distances[min_at] = tempmin;
    }
  }

  if (verbose > 0) {
    gprintf("%oSorted moves:\n");
    for (i = 0; i < num_moves; i++)
      gprintf("%o%1M %d\n", moves[i], distances[i]);
  }

  if (sgf_dumptree) {
    char buf[500];
    char *pos;
    int chars;
    sprintf(buf, "Move order for %sconnect: %n",
	    color_to_move == color ? "" : "dis", &chars);
    pos = buf + chars;
    for (i = 0; i < num_moves; i++) {
      sprintf(pos, "%c%d (%d) %n", J(moves[i]) + 'A' + (J(moves[i]) >= 8),
	      board_size - I(moves[i]), distances[i], &chars);
      pos += chars;
    }
    sgftreeAddComment(sgf_dumptree, buf);
  }


  /* Filter out moves with distance at least 1500 more than the best
   * move.
   */
  for (r = 0; r < num_moves; r++)
    if (distances[r] > distances[0] + 1500)
      break;
  num_moves = r;

  return num_moves;
}


/* Helper macro for the function below. */
#define ENQUEUE(conn, from, pos, dist, delta, v1, v2) \
  do { \
    if (dist < conn->distances[pos]) { \
      if (board[pos] == EMPTY) { \
        if (conn->distances[pos] == HUGE_CONNECTION_DISTANCE) \
          conn->queue[conn->queue_end++] = pos; \
        conn->distances[pos] = dist; \
        conn->deltas[pos] = delta; \
        conn->coming_from[pos] = from; \
        conn->vulnerable1[pos] = v1; \
        conn->vulnerable2[pos] = v2; \
      } \
      else { \
        int r; \
        num_stones = findstones(pos, MAX_BOARD * MAX_BOARD, stones); \
        for (r = 0; r < num_stones; r++) { \
          if (conn->distances[stones[r]] == HUGE_CONNECTION_DISTANCE) \
            conn->queue[conn->queue_end++] = stones[r]; \
          conn->distances[stones[r]] = dist; \
          conn->deltas[stones[r]] = delta; \
          conn->coming_from[stones[r]] = from; \
          conn->vulnerable1[stones[r]] = v1; \
          conn->vulnerable2[stones[r]] = v2; \
	  if (stones[r] == target && dist < cutoff_distance) \
	    cutoff_distance = dist - 1; \
	} \
      } \
    } \
  } while (0);

/* Compute connection distances from the string str to nearby
 * vertices. This is a rough approximation of the number of moves
 * required to secure a connection from str to another vertex of the
 * board. We also compute delta values which are intended to tell how
 * big difference a particular move locally has on the connection
 * distance. However, remember that this is only a heuristic with the
 * sole purpose of helping to find relevant moves for connection
 * problems.
 *
 * The algorithm is to propagate connection values outwards using a
 * breadth-first searching strategy, implemented through an implicitly
 * sorted queue. The propagation to new vertices depends on
 * geometrical features with significance for connections. E.g. a
 * bamboo joint is recognized and the distance added when passing
 * through it is small. New points are added to the queue through the
 * ENQUEUE macro above. This checks whether the point has already been
 * entered on the queue and updates the distance and delta values if
 * the previous ones were worse. When a stone is entered, all stones
 * of the string are added to the queue simultaneously.
 *
 * (target) is the other string when called from find_connection_moves().
 * (It can be set to NO_MOVE otherwise.)
 *
 * The propagation is inhibited when the distance becomes too large,
 * or larger than the shortest path found to the target so far.
 *
 *
 * The purpose of the fields called vulnerable is to keep track of
 * points where the attacker can threaten an individual
 * connection. For example the diagonal formation
 *
 * .O
 * O.
 *
 * is considered a small distance link but both the empty vertices are
 * marked as vulnerable. Thus if we are computing connection distance
 * from the lower left O in this diagram,
 *
 * XXX     XXX
 * .O.     .O.
 * O.O     OaO
 * .X.     .X.
 *
 * the distance to the middle O is small but the second diagonal link
 * to the lower right O stone is not given a small distance since a
 * had already been marked as vulnerable.
 *
 * It should also be pointed out that this reasoning is not relevant
 * in this position where X has no cutting potential,
 *
 * XXX     XXX
 * .O.     .O.
 * O.O     OaO
 * ...     ...
 *
 * That is because there is a pattern directly recognizing the safe
 * link between the two lower stones, without taking the longer road
 * over the two diagonal links.
 */

static void
compute_connection_distances(int str, int target, struct connection_data *conn)
{
  int color = board[str];
  int other = OTHER_COLOR(color);
  int pos;
  int k;
  int distance;
  int stones[MAX_BOARD * MAX_BOARD];
  int num_stones = findstones(str, MAX_BOARD * MAX_BOARD, stones);
  int cutoff_distance = 3001;
  
  conn->queue_start = 0;
  conn->queue_end = 0;

  /* Initialize distance and delta values so that the former are
   * everywhere huge and the latter everywhere zero.
   */
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    conn->distances[pos] = HUGE_CONNECTION_DISTANCE;
    conn->deltas[pos] = 0;
    conn->coming_from[pos] = NO_MOVE;
    conn->vulnerable1[pos] = NO_MOVE;
    conn->vulnerable2[pos] = NO_MOVE;
  }

  /* Add all stones in the initial string to the queue. */
  for (k = 0; k < num_stones; k++) {
    ENQUEUE(conn, NO_MOVE, stones[k], 0, 0, NO_MOVE, NO_MOVE);
  }

  /* Loop until we reach the end of the queue. */
  for (; conn->queue_start < conn->queue_end; conn->queue_start++) {
    int smallest_dist = HUGE_CONNECTION_DISTANCE;
    int best_index = -1;

    gg_assert(conn->queue_end <= MAX_BOARD * MAX_BOARD);

    /* Find the smallest distance among the queued points. */
    for (k = conn->queue_start; k < conn->queue_end; k++) {
      if (conn->distances[conn->queue[k]] < smallest_dist) {
	smallest_dist = conn->distances[conn->queue[k]];
	best_index = k;
      }
    }

    /* Exchange the best point with the first element in the queue. */
    if (best_index != conn->queue_start) {
      int tmp = conn->queue[conn->queue_start];
      conn->queue[conn->queue_start] = conn->queue[best_index];
      conn->queue[best_index] = tmp;
    }

    /* Now we are ready to pick the first element in the queue and
     * process it.
     */
    pos = conn->queue[conn->queue_start];
    distance = conn->distances[pos];

    /* No further propagation if the distance is too large. */
    if (distance > cutoff_distance)
      break;

    /* Search for new vertices to propagate to. */
    if (board[pos] == color) {
      for (k = 0; k < 4; k++) {
	/* List of relative coordinates. (pos) is marked by *.
	 *
	 *  jef.
	 *  igb.
	 * kh*ac
	 *  ....
	 *
	 */
	int right = delta[k];
	int up = delta[(k+1)%4];

	/* FIXME: Compactify this list. */
	int apos = pos + right;
	int bpos = pos + right + up;
	int cpos = pos + 2 * right;
	int epos = pos + 2*up;
	int fpos = pos + right + 2*up;
	int gpos = pos + up;
	int hpos = pos - right;
	int ipos = pos - right + up;
	int jpos = pos - right + 2 * up;
	int kpos = pos - 2 * right;
	
	/* Case 1. "a" is empty and would be suicide for the opponent. */
	if (board[apos] == EMPTY && is_suicide(apos, other)) {
	  ENQUEUE(conn, pos, apos, distance, 0, apos, NO_MOVE);
	}
	
	/* Case 2. "a" is empty and would be self atari for the opponent. */
	if (board[apos] == EMPTY
	    && conn->distances[apos] > distance + 100
	    && is_self_atari(apos, other)) {
	  int lib;
	  int vulnerable1 = NO_MOVE;
	  int vulnerable2 = NO_MOVE;
	  if (approxlib(apos, other, 1, &lib) >= 1) {
	    if (approxlib(lib, other, 2, NULL) > 2)
	      vulnerable1 = lib;
	    if (countlib(pos) == 2) {
	      int i;
	      for (i = 0; i < 4; i++) {
		if (board[lib + delta[i]] == EMPTY
		    && lib + delta[i] != apos
		    && trymove(lib + delta[i], other,
			       "compute_connection_distances", pos,
			       EMPTY, NO_MOVE)) {
		  if (ladder_capture(pos, NULL)) {
		    vulnerable2 = lib + delta[i];
		    popgo();
		    break;
		  }
		  popgo();
		}
	      }
	    }
	  }
	  
	  if (!common_vulnerabilities(conn->vulnerable1[pos],
				      conn->vulnerable2[pos],
				      vulnerable1, vulnerable2, color)) {
	    ENQUEUE(conn, pos, apos, distance + 100, 100,
		    vulnerable1, vulnerable2);
	  }
	}
	
	/* Case 3. Bamboo joint of "*" + "a" to "e" + "f" through "b" and "g".
	 * Notice that the order of these tests is significant. We must
	 * check bpos before fpos and epos to avoid accessing memory
	 * outside the board array. (Notice that fpos is two steps away
	 * from pos, which we know is on the board.)
	 */
	if (board[apos] == color && board[bpos] == EMPTY
	    && board[fpos] == color && board[epos] == color
	    && board[gpos] == EMPTY) {
	  ENQUEUE(conn, pos, bpos, distance + 100, 100, NO_MOVE, NO_MOVE);
	  ENQUEUE(conn, pos, gpos, distance + 100, 100, NO_MOVE, NO_MOVE);
	}
	   
	/* Case 4. Diagonal connection to another stone "b" through
	 * empty vertices "a" and "g".
	 */
	if (board[bpos] == color
	    && board[apos] == EMPTY
	    && board[gpos] == EMPTY
	    && !common_vulnerabilities(conn->vulnerable1[pos],
				       conn->vulnerable2[pos],
				       apos, gpos, color)
	    && conn->distances[bpos] > distance + 100) {
#if 0
	  ENQUEUE(conn, pos, apos, distance + 200, 200, NO_MOVE, NO_MOVE);
	  ENQUEUE(conn, pos, gpos, distance + 200, 200, NO_MOVE, NO_MOVE);
#endif
	  ENQUEUE(conn, pos, bpos, distance + 100, 100, apos, gpos);
	}
	   
	/* Case 5. Almost bamboo joint.
	 * 
	 */
	if (board[gpos] == EMPTY
	    && board[epos] == color
            && conn->distances[epos] > distance + 200
	    && approxlib(gpos, other, 3, NULL) <= 2) {
	  if (board[bpos] == EMPTY
	      && approxlib(bpos, color, 3, NULL) >= 3
	      && (board[apos] == color
		  || (board[apos] == EMPTY
		      && !common_vulnerabilities(conn->vulnerable1[pos],
						 conn->vulnerable2[pos],
						 apos, gpos, color)
		      && approxlib(apos, other, 3, NULL) <= 2))
	      && (board[fpos] == color
		  || (board[fpos] == EMPTY
		      && !common_vulnerabilities(conn->vulnerable1[pos],
						 conn->vulnerable2[pos],
						 fpos, gpos, color)
		      && approxlib(fpos, other, 3, NULL) <= 2))) {
	    if (board[apos] == EMPTY && board[fpos] == EMPTY) {
	      ENQUEUE(conn, pos, epos, distance + 200, 200, apos, fpos);
	    }
	    else if (board[apos] == EMPTY && board[fpos] != EMPTY) {
	      ENQUEUE(conn, pos, epos, distance + 200, 200, apos, NO_MOVE);
	    }
	    else if (board[apos] != EMPTY && board[fpos] == EMPTY) {
	      ENQUEUE(conn, pos, epos, distance + 200, 200, fpos, NO_MOVE);
	    }
	    else if (board[apos] != EMPTY && board[fpos] != EMPTY) {
	      ENQUEUE(conn, pos, epos, distance + 200, 200, NO_MOVE, NO_MOVE);
	    }
	  }
	  if (board[ipos] == EMPTY
	      && approxlib(ipos, color, 3, NULL) >= 3
	      && (board[hpos] == color
		  || (board[hpos] == EMPTY
		      && !common_vulnerabilities(conn->vulnerable1[pos],
						 conn->vulnerable2[pos],
						 hpos, gpos, color)
		      && approxlib(hpos, other, 3, NULL) <= 2))
	      && (board[jpos] == color
		  || (board[jpos] == EMPTY
		      && !common_vulnerabilities(conn->vulnerable1[pos],
						 conn->vulnerable2[pos],
						 jpos, gpos, color)
		      && approxlib(jpos, other, 3, NULL) <= 2))) {
	    if (board[hpos] == EMPTY && board[jpos] == EMPTY) {
	      ENQUEUE(conn, pos, epos, distance + 200, 200, hpos, jpos);
	    }
	    else if (board[hpos] == EMPTY && board[jpos] != EMPTY) {
	      ENQUEUE(conn, pos, epos, distance + 200, 200, hpos, NO_MOVE);
	    }
	    else if (board[hpos] != EMPTY && board[jpos] == EMPTY) {
	      ENQUEUE(conn, pos, epos, distance + 200, 200, jpos, NO_MOVE);
	    }
	    else if (board[hpos] != EMPTY && board[jpos] != EMPTY) {
	      ENQUEUE(conn, pos, epos, distance + 200, 200, NO_MOVE, NO_MOVE);
	    }
	  }
	}
	   	   
	/* Case 6. "a" is empty and an opponent move can be captured in
	 * a ladder.
	 */
	if (board[apos] == EMPTY
	    && conn->distances[apos] > distance + 700
	    && ladder_capturable(apos, other)) {
	  ENQUEUE(conn, pos, apos, distance + 700, 700, apos, NO_MOVE);
	}

	/* Case 7. "a" is empty or occupied by opponent.
	 */
	if ((board[apos] == EMPTY || board[apos] == other)
	    && conn->distances[apos] > distance + 1000) {
	  ENQUEUE(conn, pos, apos, distance + 1000, 1000, NO_MOVE, NO_MOVE);
	}

	/* Case 8. Diagonal connection to empty vertex "b" through
	 * empty vertex "a" or "g", which makes "a" or "g" self_atari
	 * for opponent.
	 */
	if (board[bpos] == EMPTY
	    && board[apos] == EMPTY
	    && conn->distances[bpos] > distance + 1100
	    && does_secure(color, bpos, apos)) {
	  ENQUEUE(conn, pos, bpos, distance + 1100, 1000, apos, NO_MOVE);
	}

	if (board[bpos] == EMPTY
	    && board[gpos] == EMPTY
	    && conn->distances[bpos] > distance + 1100
	    && does_secure(color, bpos, gpos)) {
	  ENQUEUE(conn, pos, bpos, distance + 1100, 1000, gpos, NO_MOVE);
	}

	/* Case 9. One-space jump to empty vertex "e" through empty
	 * vertex "g", which makes "g" self_atari for opponent.
	 */
	if (board[gpos] == EMPTY
	    && board[epos] == EMPTY
	    && conn->distances[epos] > distance + 1100
	    && does_secure(color, epos, gpos)) {
	  ENQUEUE(conn, pos, epos, distance + 1100, 1000, gpos, NO_MOVE);
	}

	/* Case 10. One-space jump to empty vertex "e" through empty
	 * vertex "g", making a bamboo joint.
	 */
	if (board[gpos] == EMPTY
	    && board[epos] == EMPTY
	    && conn->distances[epos] > distance + 1100
	    && ((board[apos] == color && board[fpos] == color
		 && board[bpos] == EMPTY)
		|| (board[hpos] == color && board[jpos] == color
		    && board[ipos] == EMPTY))){
	  ENQUEUE(conn, pos, epos, distance + 1100, 1000, gpos, NO_MOVE);
	}

	/* Case 11. Diagonal connection to empty vertex "b" through
	 * empty vertices "a" and "g".
	 */
	if (board[bpos] == EMPTY
	    && board[apos] == EMPTY && board[gpos] == EMPTY
            && conn->distances[bpos] > distance + 1300) {
	  ENQUEUE(conn, pos, bpos, distance + 1300, 1000, apos, gpos);
	}

	/* Case 12. Keima to f or j on edge and one space jump on
	 * first or second line.
	 */
	if (board[apos] == EMPTY
	    && board[bpos] == EMPTY
	    && board[gpos] == EMPTY
	    && board[epos] == EMPTY
	    && board[fpos] == EMPTY
	    && (conn->distances[fpos] > distance + 1300
		|| conn->distances[epos] > distance + 1300)
	    && countlib(pos) >= 3
	    && (!ON_BOARD(cpos) || !ON_BOARD(hpos))) {
	  ENQUEUE(conn, pos, fpos, distance + 1300, 1000, NO_MOVE, NO_MOVE);
	  ENQUEUE(conn, pos, epos, distance + 1300, 1000, NO_MOVE, NO_MOVE);
	}

	if (countlib(pos) >= 3
	    && board[hpos] == EMPTY
	    && board[ipos] == EMPTY
	    && board[gpos] == EMPTY
	    && board[epos] == EMPTY
	    && board[jpos] == EMPTY
	    && (conn->distances[jpos] > distance + 1300
		|| conn->distances[epos] > distance + 1300)
	    && (!ON_BOARD(apos) || !ON_BOARD(kpos))) {
	  ENQUEUE(conn, pos, jpos, distance + 1300, 1000, NO_MOVE, NO_MOVE);
	  ENQUEUE(conn, pos, epos, distance + 1300, 1000, NO_MOVE, NO_MOVE);
	}

	/* Case 13. Diagonal connection to empty vertex "b" through
	 * empty vertex "a" or "g", which allows opponent move at "a"
	 * or "g" to be captured in a ladder.
	 */
	if (board[bpos] == EMPTY
	    && board[apos] == EMPTY
	    && conn->distances[bpos] > distance + 1200
	    && does_secure_through_ladder(color, bpos, apos)) {
	  ENQUEUE(conn, pos, bpos, distance + 1200, 1000, apos, NO_MOVE);
	}

	if (board[bpos] == EMPTY
	    && board[gpos] == EMPTY
	    && conn->distances[bpos] > distance + 1200
	    && does_secure_through_ladder(color, bpos, gpos)) {
	  ENQUEUE(conn, pos, bpos, distance + 1200, 1000, gpos, NO_MOVE);
	}

	/* Case 13b. Diagonal connection to empty vertex "b" through
	 * one empty and one opponent vertex "a" and "g", where
	 * the opponent stone is short of liberties.
	 */
	if (board[bpos] == EMPTY
	    && board[apos] == EMPTY
	    && board[gpos] == other
	    && countlib(gpos) <= 3
	    && conn->distances[bpos] > distance + 1500) {
	  ENQUEUE(conn, pos, bpos, distance + 1500, 1000, apos, NO_MOVE);
	}

	if (board[bpos] == EMPTY
	    && board[gpos] == EMPTY
	    && board[apos] == other
	    && countlib(apos) <= 3
	    && conn->distances[bpos] > distance + 1500) {
	  ENQUEUE(conn, pos, bpos, distance + 1500, 1000, gpos, NO_MOVE);
	}

	/* Case 14. Diagonal connection to empty vertex "b" through
	 * empty vertex "a" or "g", with no particular properties.
	 */
	if (board[bpos] == EMPTY
	    && board[apos] == EMPTY
	    && conn->distances[bpos] > distance + 1800) {
	  ENQUEUE(conn, pos, bpos, distance + 1800, 900, NO_MOVE, NO_MOVE);
	}

	if (board[bpos] == EMPTY
	    && board[gpos] == EMPTY
	    && conn->distances[bpos] > distance + 1800) {
	  ENQUEUE(conn, pos, bpos, distance + 1800, 900, NO_MOVE, NO_MOVE);
	}

	/* Case 15. Clamp at "e" of single stone at "g".
	 */
	if (board[gpos] == other
	    && board[epos] == EMPTY
	    && conn->distances[epos] > distance + 2000
	    && countstones(gpos) == 1) {
	  ENQUEUE(conn, pos, epos, distance + 2000, 1000, NO_MOVE, NO_MOVE);
	}

	if (board[bpos] == EMPTY
	    && board[gpos] == EMPTY
	    && conn->distances[bpos] > distance + 1800
	    && does_secure_through_ladder(color, bpos, gpos)) {
	  ENQUEUE(conn, pos, bpos, distance + 1800, 900, NO_MOVE, NO_MOVE);
	}

	/* Case 16. Diagonal connection to empty vertex "b" through
	 * opponent stones "a" or "g" with few liberties.
	 */
	if (board[bpos] == EMPTY
	    && board[apos] == other
	    && board[gpos] == other
	    && conn->distances[bpos] > distance + 2000
	    && (countlib(apos) + countlib(gpos) <= 6)) {
	  ENQUEUE(conn, pos, bpos, distance + 2000, 1000, NO_MOVE, NO_MOVE);
	}

	/* Case 17. Diagonal connection to own stone "b" through
	 * opponent stones "a" or "g" with few liberties.
	 */
	if (board[bpos] == color
	    && board[apos] == other
	    && board[gpos] == other
	    && conn->distances[bpos] > distance + 2000
	    && (countlib(apos) + countlib(gpos) <= 5)) {
	  ENQUEUE(conn, pos, bpos, distance + 2000, 1000, NO_MOVE, NO_MOVE);
	}

	/* Case 18. Adjacent opponent stone at "a" which can't avoid atari.
	 */
	if (board[apos] == other
	    && conn->distances[apos] > distance + 100
	    && no_escape_from_atari(apos)) {
	  ENQUEUE(conn, pos, apos, distance + 100, 100, NO_MOVE, NO_MOVE);
	}

	/* Case 19. Adjacent opponent stone at "a" which can't avoid
	 * ladder capture.
	 */
	if (board[apos] == other
	    && conn->distances[apos] > distance + 300
	    && no_escape_from_ladder(apos)) {
	  ENQUEUE(conn, pos, apos, distance + 300, 300, NO_MOVE, NO_MOVE);
	}
      }
    }
    else if (board[pos] == EMPTY
	     || (board[pos] == other
		 && (no_escape_from_ladder(pos))
		 && countlib(pos) <= 2)) {
      for (k = 0; k < 4; k++) {
	/* List of relative coordinates. (pos) is marked by *.
	 *
	 *  jef.
	 *  igb.
	 * kh*ac
	 *   .d.
	 *
	 */
	int right = delta[k];
	int up = delta[(k+1)%4];

	/* FIXME: Compactify this list. */
	int apos = pos + right;
	int bpos = pos + right + up;
#if 0
	int cpos = pos + 2 * right;
	int epos = pos + 2*up;
	int fpos = pos + right + 2*up;
#endif
	int gpos = pos + up;
#if 0
	int hpos = pos - right;
	int ipos = pos - right + up;
	int jpos = pos - right + 2 * up;
	int kpos = pos - 2 * right;
#endif
	
	if (board[apos] == color) {
	  ENQUEUE(conn, pos, apos, distance, 0,
		  conn->vulnerable1[pos], conn->vulnerable2[pos]);
	}
	else if (ON_BOARD(apos)) {
	  ENQUEUE(conn, pos, apos, distance + 1000, 1000, NO_MOVE, NO_MOVE);
	}

	/* Case 1. Diagonal connection to empty vertex "b" through
	 * empty vertices "a" and "g".
	 */
	if (board[bpos] == EMPTY
	    && board[apos] == EMPTY
	    && board[gpos] == EMPTY
            && conn->distances[bpos] > distance + 1500) {
	  ENQUEUE(conn, pos, bpos, distance + 1500, 1000, NO_MOVE, NO_MOVE);
	}
	
	/* Case 2. Diagonal connection to friendly stone at "b" through
	 * empty vertices "a" and "g".
	 */
	if (board[bpos] == color
	    && board[apos] == EMPTY
	    && board[gpos] == EMPTY
	    && conn->distances[bpos] > distance + 1300) {
	  ENQUEUE(conn, pos, bpos, distance + 1300, 1000, NO_MOVE, NO_MOVE);
	}
      }
    }
  }
}


/* Print the connection distances in a struct connection_data. */
static void
print_connection_distances(struct connection_data *conn)
{
  int i, j;
  int ch;
  int pos;
  
  fprintf(stderr, "  ");
  for (j = 0, ch = 'A'; j < board_size; j++, ch++) {
    if (ch == 'I')
      ch++;
    fprintf(stderr, "  %c ", ch);
  }
  fprintf(stderr, "\n");

  for (i = 0; i < board_size; i++) {
    fprintf(stderr, "%2d ", board_size - i);
    for (j = 0; j < board_size; j++) {
      pos = POS(i, j);
      if (conn->distances[pos] == HUGE_CONNECTION_DISTANCE) {
	if (board[pos] == WHITE)
	  fprintf(stderr, " O  ");
	if (board[pos] == BLACK)
	  fprintf(stderr, " X  ");
	if (board[pos] == EMPTY)
	  fprintf(stderr, " .  ");
      }
      else {
	fprintf(stderr, "%d ", conn->distances[pos]);
      }
    }
    fprintf(stderr, "\n");
  }
  fprintf(stderr, "\n");

  fprintf(stderr, "Vulnerable:\n");
  for (pos = BOARDMIN; pos < BOARDMAX; pos++)
    if (conn->distances[pos] < HUGE_CONNECTION_DISTANCE
	&& (conn->vulnerable1[pos] != NO_MOVE
	    || conn->vulnerable2[pos] != NO_MOVE)) {
      gprintf(" %1m:", pos);
      if (conn->vulnerable1[pos] != NO_MOVE)
	gprintf(" %1m", conn->vulnerable1[pos]);
      if (conn->vulnerable2[pos] != NO_MOVE)
	gprintf(" %1m", conn->vulnerable2[pos]);
      gprintf("\n", pos);
    }
}


/* Test whether there is a trivial connection between str1 and str2
 * and if so return the connecting move in *move. By trivial
 * connection we mean that they either have a common liberty or a
 * common neighbor which can be tactically attacked.
 */
static int
trivial_connection(int str1, int str2, int *move)
{
  SGFTree *save_sgf_dumptree = sgf_dumptree;
  int save_count_variations = count_variations;
  int adj, adjs[MAXCHAIN];
  int r;
  int result = 0;
  
  if (have_common_lib(str1, str2, move))
    return WIN;

  adj = chainlinks(str1, adjs);

  /* We turn off the sgf traces here to avoid cluttering them up with
   * tactical reading moves.
   */
  sgf_dumptree = NULL;
  count_variations = 0;
  
  for (r = 0; r < adj; r++)
    if (adjacent_strings(adjs[r], str2) && attack(adjs[r], move) == WIN) {
      result = WIN;
      break;
    }

  /* Turn the sgf traces back on. */
  sgf_dumptree = save_sgf_dumptree;
  count_variations = save_count_variations;
  
  return result;
}


/* True if a move by color makes an opponent move at pos a self atari.
 */
static int
does_secure(int color, int move, int pos)
{
  int result = 0;
  if (trymove(move, color, NULL, NO_MOVE, EMPTY, NO_MOVE)) {
    if (is_self_atari(pos, OTHER_COLOR(color)))
      result = 1;
    popgo();
  }
  
  return result;
}


/* True if a move by color makes an opponent move at pos a self atari
 * or possible to capture in a ladder.
 */
static int
does_secure_through_ladder(int color, int move, int pos)
{
  int result = 0;
  
  if (trymove(move, color, NULL, NO_MOVE, EMPTY, NO_MOVE)) {
    if (ladder_capturable(pos, OTHER_COLOR(color)))
      result = 1;
    popgo();
  }
  
  return result;
}

/* Test whether the string str can be immediately taken off the board
 * or captured in a ladder. If so the capturing move is returned in
 * *move.
 */
static int
ladder_capture(int str, int *move)
{
  int result;
  SGFTree *save_sgf_dumptree = sgf_dumptree;
  int save_count_variations = count_variations;
  int liberties = countlib(str);
  
  /* We turn off the sgf traces here to avoid cluttering them up with
   * tactical reading moves.
   */
  sgf_dumptree = NULL;
  count_variations = 0;

  if (liberties == 1)
    result = attack(str, move);
  else if (liberties == 2)
    result = simple_ladder(str, move);
  else
    result = 0;
  
  /* Turn the sgf traces back on. */
  sgf_dumptree = save_sgf_dumptree;
  count_variations = save_count_variations;

  return result;
}

/* Test whether a move at pos by color can be captured in a ladder. */
static int
ladder_capturable(int pos, int color)
{
  int result = 0;
  
  if (trymove(pos, color, NULL, NO_MOVE, EMPTY, NO_MOVE)) {
    int liberties = countlib(pos);
    if (liberties == 1 && attack(pos, NULL) == WIN)
      result = 1;
    else if (liberties == 2 && simple_ladder(pos, NULL) == WIN)
      result = 1;
    popgo();
  }
  else
    result = 1;
  
  return result;
}


/* Test whether the string str with one liberty is stuck with at most
 * one liberty. This function trivially returns false if the string
 * has more than one liberty to start with.
 */
static int
no_escape_from_atari(int str)
{
  int lib;
  int adj[MAXCHAIN];

  if (findlib(str, 1, &lib) > 1)
    return 0;

  if (accuratelib(lib, board[str], 2, NULL) > 1)
    return 0;

  /* FIXME: Should exclude snapback. */
  if (chainlinks2(str, adj, 1) > 0)
    return 0;

  return 1;
}


/* Test whether the string str with one liberty is captured in a
 * ladder. This function trivially returns false if the string has
 * more than one liberty to start with, except for one special case.
 * FIXME: Needs a simple_ladder_defense().
 */
static int
no_escape_from_ladder(int str)
{
  int result = 0;
  SGFTree *save_sgf_dumptree = sgf_dumptree;
  int save_count_variations = count_variations;
  int adj[MAXCHAIN];
  int libs[2];
  
  /* We turn off the sgf traces here to avoid cluttering them up with
   * tactical reading moves.
   */
  sgf_dumptree = NULL;
  count_variations = 0;
  
  if (countlib(str) == 1 && find_defense(str, NULL) == 0)
    result = 1;

  if (countlib(str) == 2
      && chainlinks2(str, adj, 1) == 0
      && findlib(str, 2, libs) == 2
      && approxlib(libs[0], board[str], 2, NULL) == 1
      && approxlib(libs[1], board[str], 2, NULL) == 1
      && ladder_capture(str, NULL)
      && !find_defense(str, NULL))
    result = 1;
      
  
  /* Turn the sgf traces back on. */
  sgf_dumptree = save_sgf_dumptree;
  count_variations = save_count_variations;

  return result;
}

/* We usually don't want to spend time with moves which are
 * self-atari, unless the stone is involved in a ko.
 */
static int
check_self_atari(int pos, int color_to_move)
{
#if 0
  int lib;
#endif
  
  if (!is_self_atari(pos, color_to_move))
    return 1;

  if (is_ko(pos, color_to_move, NULL))
    return 1;

#if 0
  /* FIXME: At some time I added this exceptional case but I can no
   * longer see how it would be useful. It might still be, however, so
   * I leave the code in for a while. /gf
   */
  if (approxlib(pos, color_to_move, 1, &lib) >= 1
      && approxlib(lib, OTHER_COLOR(color_to_move), 3, NULL) <= 2
      && ladder_capturable(lib, OTHER_COLOR(color_to_move)))
    return 1;
#endif

  return 0;
}

/* Check for overlap between (a1, a2) and (b1, b2). */
static int
common_vulnerabilities(int a1, int a2, int b1, int b2, int color)
{
  return (common_vulnerability(a1, b1, color)
	  || common_vulnerability(a1, b2, color)
	  || common_vulnerability(a2, b1, color)
	  || common_vulnerability(a2, b2, color));
}

/* Check if apos and bpos are the same or if they are both liberties
 * of a string of the given color with at most three liberties.
 */
static int
common_vulnerability(int apos, int bpos, int color)
{
  int k;
  
  if (apos == NO_MOVE || bpos == NO_MOVE)
    return 0;
  
  if (apos == bpos)
    return 1;

  for (k = 0; k < 4; k++)
    if (board[apos + delta[k]] == color
	&& countlib(apos + delta[k]) <= 3
	&& liberty_of_string(bpos, apos + delta[k]))
      return 1;

  return 0;
}

/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
