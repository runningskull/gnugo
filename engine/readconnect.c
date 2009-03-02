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

#include "gnugo.h"

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <math.h>

#include "liberty.h"
#include "cache.h"
#include "gg_utils.h"
#include "readconnect.h"

/* Size of array where candidate moves are stored. */
#define MAX_MOVES 362

/* trace of a search */

typedef struct _zone {
  int array[BOARDMAX];
  unsigned int bits[1+BOARDMAX/32];
  int i;
} zone;

static int recursive_connect2(int str1, int str2, int *move,
			        int has_passed);
static int recursive_disconnect2(int str1, int str2, int *move,
				   int has_passed);
static int recursive_break(int str, const signed char goal[BOARDMAX],
			   int *move, int has_passed, Hash_data *goal_hash);
static int recursive_block(int str, const signed char goal[BOARDMAX],
			   int *move, int has_passed, Hash_data *goal_hash);

static int add_array(int *array, int elt);
static int element_array(int *array, int elt);
static void intersection_array(int *array1, int *array2);
static int snapback(int str);
static int connection_one_move(int str1, int str2);
static int prevent_connection_one_move(int *moves, int str1, int str2);
static int connected_one_move(int str1, int str2);
static int moves_to_connect_in_two_moves(int *moves, int str1, int str2);
static int connection_two_moves(int str1, int str2);
static int prevent_connection_two_moves(int *moves, int str1, int str2);
#if 0
static int connected_two_moves(int str1, int str2);
#endif
static int moves_to_connect_in_three_moves(int *moves, int str1, int str2,
					   int does_connect);
#if 0
static int simple_connection_three_moves(int str1, int str2);
static int prevent_simple_connection_three_moves(int *moves,
						 int str1, int str2);
#endif

static int recursive_connect(int str1, int str2, int *move);
static int recursive_disconnect(int str1, int str2, int *move);

static int quiescence_connect(int str1, int str2, int *move);
static int quiescence_capture(int str, int *move);
/* static int capture_one_move(int str); */
static int prevent_capture_one_move(int *moves, int str1);
static int recursive_transitivity(int str1, int str2, int str3, int *move);
static int recursive_non_transitivity(int str1, int str2, int str3, int *move);
static void order_connection_moves(int *moves, int str1, int str2,
				   int color_to_move, const char *funcname);

static int nodes_connect = 0;

/* Used by alternate connections. */
static signed char connection_shadow[BOARDMAX];

static signed char breakin_shadow[BOARDMAX]; 

/* Statistics. */
static int global_connection_node_counter = 0;

static void
init_zone(zone *zn)
{
  zn->array[0] = 0;
  memset(zn->bits, 0, 1 + BOARDMAX / 8);
}

/* send back 1 if the intersection is in the zone
 */

#if 0
static int
elt_zone(zone *zn, int elt)
{
  if ((zn->bits[elt >> 5] >> (elt & 31)) & 1)
    return 1;
  return 0;
}
#endif

/* Adds an intersection to a zone
 */

static void
add_zone(zone *zn, int elt)
{
  if (((zn->bits[elt >> 5] >> (elt & 31)) & 1) == 0) {
    zn->bits[elt >> 5] |= (1 << (elt & 31));
    zn->array[0]++;
    zn->array[zn->array[0]] = elt;
  }
}

/* start to loop over a zone
 */

#if 0
static int
start_zone(zone *zn)
{
  if (zn->array[0] < 1)
    return -1;
  zn->i = 1;
  return zn->array[1];
}
#endif

/* continue to loop over a zone
 */

#if 0
static int
next_zone(zone *zn)
{
  zn->i++;
  if (zn->i > zn->array[0])
    return -1;
  return zn->array[zn->i];
}
#endif

/* only keep the elements of zn1 which are also in zn2 */

#if 0
static void
intersection_zone(zone *zn1, zone *zn2)
{
  int r, s;
  
  for (r = start_zone(zn1); r > -1; r = next_zone(zn1))
    if (!elt_zone(zn2, r)) {
      for (s = r; s < zn1->array[0]; s++)
	zn1->array[s] = zn1->array[s+1];
      zn1->bits[r >> 5] &= ~ (1 << (r & 31));
      zn1->array[0]--;
      zn1->i--;
    }
}
#endif

/* Adds an integer to an array of integers if it is not already there.
 * The number of elements of the array is in array[0].
 */

static int
add_array(int *array, int elt)
{
  int r;
  
  for (r = 1; r < array[0] + 1; r++)
    if (array[r] == elt)
      return 0;

  array[0]++;
  array[array[0]] = elt;
  return 1;
}

/* test if an element is part of an array */

static int
element_array(int *array, int elt)
{
  int r;
  for (r = 1; r < array[0] + 1; r++)
    if (array[r] == elt)
      return 1;
  return 0;
}

/* only keep the elements of array1 which are also in array2 */

static void
intersection_array(int *array1, int *array2)
{
  int r, s;
  
  for (r = 1; r < array1[0] + 1; r++)
    if (!element_array(array2, array1[r])) {
      for (s = r; s < array1[0]; s++)
	array1[s] = array1[s+1];
      array1[0]--;
      r--;
    }
}

/* verifies that capturing the stone at str is not a snapback */

static int
snapback(int str)
{
  int stones, liberties, lib;
  SGFTree *save_sgf_dumptree = sgf_dumptree;

  /* if more than one stone captured, not a snapback */
  stones = countstones(str);
  if (stones > 1)
    return 0;

  /* if more than one liberty, not a snapback */
  liberties = findlib(str, 1, &lib);
  if (liberties > 1)
    return 0;

  /* turn off the sgf traces
   */
  sgf_dumptree = NULL;
  
  /* if only one liberty after capture */
  if (trymove(lib, OTHER_COLOR(board[str]), "snapback", str)) {
    liberties = 0;
    if (IS_STONE(board[lib]))
      liberties = countlib(lib);
    popgo();
    sgf_dumptree = save_sgf_dumptree;
    if (liberties > 1)
      return 0;
    return WIN;
  }
  
  /* Turn the sgf traces back on. */
  sgf_dumptree = save_sgf_dumptree;
  
  return 0;
}

/* connection by playing and finding a ponnuki after play */

static int
ponnuki_connect(int *moves, int str1, int str2, zone *zn)
{
  int r, s, k, res = 0;
  int liberties, libs[MAXLIBS];
  int adj, adjs[MAXCHAIN];
  int neighb, neighbs[MAXCHAIN];

  /* finds connection through two forbidden liberties for
   * the opponent
   * + + + + + + +
   * + + @ O O @ +
   * + @ + @ @ x +
   * + + @ + + + +
   * - - - - - - -
   *
   * + + + + + + +
   * + + @ O O @ +
   * + @ + @ @ O @
   * + + @ + + x +
   * - - - - - - -
   */
  liberties = findlib(str1, MAXLIBS, libs);
  for (r = 0; r < liberties; r++)
    if (is_self_atari(libs[r], OTHER_COLOR(board[str1]))) 
      for (k = 0; k < 4; k++) {
	int pos = libs[r] + delta[k];
	if (board[pos] == board[str1]
	    && !same_string(pos, str1)
	    && !same_string(pos, str2)) {
	  /* try to connect pos to str2 in one move */
	  /* play a common liberty */
	  neighb = findlib(pos, MAXLIBS, neighbs);
	  for (s = 0; s < neighb; s++)
	    if (liberty_of_string(neighbs[s], str2)) {
	      res = 1;
	      add_zone(zn, libs[r]);
	      add_zone(zn, neighbs[s]);
	      add_array(moves, neighbs[s]);
	    }
	  /* or capture a common adjacent string */
	  adj = chainlinks2(pos, adjs, 1);
	  for (s = 0; s < adj; s++)
	    if (adjacent_strings(adjs[s], str2)
		&& !snapback(adjs[s])) {
	      res = 1;
	      neighb = findlib(adjs[s], 1, neighbs);
	      add_zone(zn, libs[r]);
	      add_zone(zn, neighbs[0]);
	      add_array(moves, neighbs[0]);
	    }
	}
      }
  
  return res;
}

/* connection in one move, finds all moves and memorizes intersections
 * involved in the connection.
 */

static int
moves_connection_one_move(int *moves, int str1, int str2, zone *zn)
{
  int r;
  int adj, adjs[MAXCHAIN];

  /* If one string is missing we have already failed. */
  if (board[str1] == EMPTY || board[str2] == EMPTY)
    return 0;

  /* Common liberties. */
  if (have_common_lib(str1, str2, NULL))
    return WIN;

  /* Common adjacent string in atari, more than one stone, no snapback. */
  adj = chainlinks2(str1, adjs, 1);
  for (r = 0; r < adj; r++)
    if (adjacent_strings(adjs[r], str2)
        && !snapback(adjs[r]))
      return WIN;
  
  /* Connections through a ponnuki */
  if (ponnuki_connect(moves, str1, str2, zn))
    return WIN;
  if (ponnuki_connect(moves, str2, str1, zn))
    return WIN;

  return 0;
}

/* Verifies that the strings str1 and str2 can be connected
 * directly by playing one move, either by playing a common liberty
 * of the two strings, or by capturing a common adjacent string.
 *
 * This is the gi1 game function.
 */

static int
connection_one_move(int str1, int str2)
{
  int moves[BOARDMAX];
  zone zn;
  init_zone(&zn);
  moves[0] = 0;
  return moves_connection_one_move(moves, str1, str2, &zn);
}

/* If the two strings str1 and str2 can be connected sends back WIN fill the
 * array moves with the only move that can prevent a connection in one move
 * (common liberties, liberties of common adjacent strings in atari).
 *
 * This is the ip1 game function. */

static int
prevent_connection_one_move(int *moves, int str1, int str2)
{
  int r, s;
  int libs[MAXLIBS];
  int adj, adjs[MAXCHAIN];
  int adjadj, adjadjs[MAXCHAIN];
  
  /* Common liberties. */
  if (have_common_lib(str1, str2, libs)) {
    add_array(moves, libs[0]);
    return WIN;
  }
  
  /* Save a common adjacent string in atari, more than one stone, no snapback.
   */
  adj = chainlinks2(str1, adjs, 1);
  for (r = 0; r < adj; r++)
    if (adjacent_strings(adjs[r], str2)
        && !snapback(adjs[r])) {
      findlib(adjs[r], MAXLIBS, libs);
      add_array(moves, libs[0]);
      adjadj = chainlinks2(adjs[r], adjadjs, 1);
      for (s = 0; s < adjadj; s++) {
	findlib(adjadjs[s], MAXLIBS, libs);
	add_array(moves, libs[0]);
      }
      return WIN;
    }
  
  return 0;
}

/* Returns WIN if str1 and str2 are connected in at most
 * one move even if the opponent plays first.
 * Verify that the strings are connectable in one move
 * and find the only possible moves that can prevent
 * using prevent_connection_one_move. If none of these
 * moves works, the two strings are connected.
 *
 * This is the g1 game function.
 */

static int
connected_one_move(int str1, int str2)
{
  int r, res = 0;
  int moves[MAX_MOVES];
  SGFTree *save_sgf_dumptree = sgf_dumptree;

  /* turn off the sgf traces
   */
  sgf_dumptree = NULL;
  
  moves[0] = 0;
  if (prevent_connection_one_move(moves, str1, str2)) {
    order_connection_moves(moves, str1, str2, OTHER_COLOR(board[str1]),
			   "connected_one_move");
    res = WIN;
    for (r = 1; ((r < moves[0] + 1) && res); r++) {
      if (trymove(moves[r], OTHER_COLOR(board[str1]),
		  "connected_one_move", str1)) {
	if (!connection_one_move(str1, str2))
	  res = 0;
	popgo();
      }
    }
  }

  /* Turn the sgf traces back on. */
  sgf_dumptree = save_sgf_dumptree;
  
  return res;
}

/* Find the moves that might be able to connect in less than three plies.
 * That is moves that can connect the strings if another move of the same
 * color is played just after:
 * - common liberties of the two strings;
 * - moves on the liberties of an opponent string with less than two
 *   liberties adjacent to both strings, or adjacent to one string and
 *   that has a common liberty with the second string;
 * - liberties of one string that are second order liberties of the
 *   other string.
 *
 * Returns WIN if a direct connection has been found. Returns 0
 * otherwise.
 */

static int
moves_to_connect_in_two_moves(int *moves, int str1, int str2)
{
  int r, s, common_adj_liberty;
  int liberties, libs[MAXLIBS];
  int adj, adjs[MAXCHAIN];
  int adjadj, adjadjs[MAXCHAIN];
  int k;
  int color = board[str1];
  int move;
  
  /* Common liberties. */
  if (have_common_lib(str1, str2, libs)) {
    add_array(moves, libs[0]);
    return 1;
  }
  
  /* Capture a common adjacent string or an adjacent liberty of str1
   * that has a common liberty with str2...
   */
  adj = chainlinks3(str1, adjs, 2);
  for (r = 0; r < adj; r++) {
    liberties = findlib(adjs[r], MAXLIBS, libs);
    common_adj_liberty = 0;
    for (s = 0; s < liberties; s++)
      if (liberty_of_string(libs[s], str2))
	common_adj_liberty = 1;
    if (common_adj_liberty || adjacent_strings(adjs[r], str2)) {
      for (s = 0; s < liberties; s++)
	add_array(moves, libs[s]);
      adjadj = chainlinks2(adjs[r], adjadjs, 1);
      for (s = 0; s < adjadj; s++) {
	findlib(adjadjs[s], MAXLIBS, libs);
	add_array(moves, libs[0]);
      }
    }
  }

  /* ...and vice versa. */
  adj = chainlinks3(str2, adjs, 2);
  for (r = 0; r < adj; r++) {
    liberties = findlib(adjs[r], MAXLIBS, libs);
    common_adj_liberty = 0;
    for (s = 0; s < liberties; s++)
      if (liberty_of_string(libs[s], str1))
	common_adj_liberty = 1;
    if (common_adj_liberty || adjacent_strings(adjs[r], str1)) {
      for (s = 0; s < liberties; s++)
	add_array(moves, libs[s]);
      adjadj = chainlinks2(adjs[r], adjadjs, 1);
      for (s = 0; s < adjadj; s++) {
	findlib(adjadjs[s], MAXLIBS, libs);
	add_array(moves, libs[0]);
      }
    }
  }
  
  /* Liberties of str1 that are second order liberties of str2 and
   * vice versa.
   */
  liberties = findlib(str1, MAXLIBS, libs);
  for (r = 0; r < liberties; r++) {
    if (board[SOUTH(libs[r])] == EMPTY) {
      if (liberty_of_string(SOUTH(libs[r]), str2)) {
	add_array(moves, libs[r]);
	add_array(moves, SOUTH(libs[r]));
      }
    }
    
    if (board[WEST(libs[r])] == EMPTY) {
      if (liberty_of_string(WEST(libs[r]), str2)) {
	add_array(moves, libs[r]);
	add_array(moves, WEST(libs[r]));
      }
    }

    if (board[NORTH(libs[r])] == EMPTY) {
      if (liberty_of_string(NORTH(libs[r]), str2)) {
	add_array(moves, libs[r]);
	add_array(moves, NORTH(libs[r]));
      }
    }

    if (board[EAST(libs[r])] == EMPTY) {
      if (liberty_of_string(EAST(libs[r]), str2)) {
	add_array(moves, libs[r]);
	add_array(moves, EAST(libs[r]));
      }
    }    
  }

  /* Liberties of str1 which are adjacent to a friendly string with
   * common liberty with str2.
   */
  liberties = findlib(str1, MAXLIBS, libs);
  for (r = 0; r < liberties; r++) {
    for (k = 0; k < 4; k++) {
      int pos = libs[r] + delta[k];
      if (board[pos] == color
	  && !same_string(pos, str1)
	  && quiescence_connect(pos, str2, &move)) {
	add_array(moves, libs[r]);
	add_array(moves, move);
      }
    }
  }

  /* And vice versa. */
  liberties = findlib(str2, MAXLIBS, libs);
  for (r = 0; r < liberties; r++) {
    for (k = 0; k < 4; k++) {
      int pos = libs[r] + delta[k];
      if (board[pos] == color
	  && !same_string(pos, str2)
	  && quiescence_connect(pos, str1, &move)) {
	add_array(moves, libs[r]);
	add_array(moves, move);
      }
    }
  }

  return 0;
}
  
/* Tests if the strings can be connected in three plies starts
 * with finding the possible moves that can connect.  If two
 * moves in a row are played, then try them and stops at the
 * first working move.  The strings are connected in two moves
 * if the function connected_one_move is verified after a move.
 *
 * This is the gi2 game function.
 */

static int
connection_two_moves(int str1, int str2)
{
  int r, res = 0, moves[MAX_MOVES];
  SGFTree *save_sgf_dumptree = sgf_dumptree;
 
  /* If one string is missing we have already failed. */
  if (board[str1] == EMPTY || board[str2] == EMPTY)
    return 0;
  
  moves[0] = 0;
  if (moves_to_connect_in_two_moves(moves, str1, str2))
    return WIN;
  order_connection_moves(moves, str1, str2, board[str1],
			 "connection_two_moves");
  
  /* turn off the sgf traces
   */
  sgf_dumptree = NULL;
  
  for (r = 1; ((r < moves[0] + 1) && !res); r++) {
    if (trymove(moves[r], board[str1], "connection_two_moves", str1)) {
      if (connected_one_move(str1, str2))
	res = WIN;
      popgo();
    }
  }
  
  sgf_dumptree = save_sgf_dumptree;
  
  return res;
}

/* Find the complete set of possible moves that can prevent
 * a connection in three plies.
 *
 * The function is not yet written, but moves_to_connect_in_two_moves does
 * a similar job, so it is called temporarly.
 */

static int
moves_to_prevent_connection_in_two_moves(int *moves, int str1, int str2)
{
  if (moves_to_connect_in_two_moves(moves, str1, str2))
    return 1;
  return 0;
}

/* Find all the moves that prevent to connect in a three plies
 * deep search and put them in the moves array.  Returns 0 if
 * there is no three plies connection, or else it tries all the
 * possible preventing moves.  If after a possible preventing
 * moves, there no connection in one move and no connection in
 * two moves, then the moves prevents a three plies deep
 * connection, and it is added to the moves array.
 *
 * this is the ip2 game function */

static int
prevent_connection_two_moves(int *moves, int str1, int str2)
{
  int r, res = 0;
  int possible_moves[MAX_MOVES];
  SGFTree *save_sgf_dumptree = sgf_dumptree;

  /* turn off the sgf traces
   */
  sgf_dumptree = NULL;
    
  if (connection_two_moves(str1, str2)) {
    res = WIN;
    possible_moves[0] = 0;
    moves_to_prevent_connection_in_two_moves(possible_moves, str1, str2);
    order_connection_moves(possible_moves, str1, str2,
			   OTHER_COLOR(board[str1]),
			   "prevent_connection_two_moves");
    for (r = 1; r < possible_moves[0] + 1; r++) {
      if (trymove(possible_moves[r], OTHER_COLOR(board[str1]), 
		  "prevent_connection_two_moves", str1)) {
	if (!connection_one_move(str1, str2))
	  if (!connection_two_moves(str1, str2))
	    add_array(moves, possible_moves[r]);
	popgo();
      }
    }
  }

  sgf_dumptree = save_sgf_dumptree;  

  return res;
}

/* Only partially written.
 *
 * Find all the moves than can connect if two subsequent
 * moves of the same color are played after
 * - common liberties;
 * - liberties of common adjacent strings with 3 liberties or less;
 * - liberties of adjacent strings with 2 liberties or less that have
 *   liberties that are second order liberties of the other string;
 * - liberties of one string that are second order liberties of the
 *   other string;
 * - second order liberties of the first string that are second order 
 *   liberties of the other string;
 *
 * A function that computes the second order liberties of a string is
 * needed as well as a function that checks efficiently if an
 * intersection is a second order liberty of a given string.
 *
 * If does_connect is 1, generate moves to connect, otherwise generate
 * moves to disconnect.
 */

static int
moves_to_connect_in_three_moves(int *moves, int str1, int str2,
				int does_connect)
{
  int r, s;
  int liberties, libs[MAXLIBS];
  int liberties2, libs2[MAXLIBS];
  int adj, adjs[MAXCHAIN];
  int adjadj, adjadjs[MAXCHAIN];
  int move;
  int k;
  int pos;
  int secondlib1[BOARDMAX];
  int secondlib2[BOARDMAX];
  
  if (moves_to_connect_in_two_moves(moves, str1, str2))
    return 1;

  /* Find second order liberties of str1. */
  memset(secondlib1, 0, sizeof(secondlib1));
  liberties = findlib(str1, MAXLIBS, libs);
  for (r = 0; r < liberties; r++)
    for (k = 0; k < 4; k++) {
      pos = libs[r] + delta[k];
      if (board[pos] == EMPTY)
	secondlib1[pos] = 1;
      else if (board[pos] == board[str1]) {
	liberties2 = findlib(pos, MAXLIBS, libs2);
	for (s = 0; s < liberties2; s++)
	  secondlib1[libs2[s]] = 1;
      }
    }
  
  /* Find second order liberties of str2.
   */
  memset(secondlib2, 0, sizeof(secondlib2));
  liberties = findlib(str2, MAXLIBS, libs);
  for (r = 0; r < liberties; r++)
    for (k = 0; k < 4; k++) {
      pos = libs[r] + delta[k];
      if (board[pos] == EMPTY)
	secondlib2[pos] = 1;
      else if (board[pos] == board[str2]) {
	liberties2 = findlib(pos, MAXLIBS, libs2);
	for (s = 0; s < liberties2; s++)
	  secondlib2[libs2[s]] = 1;
      }
    }
  
  /* Second order liberties of str1 that are second order liberties
   * of str2 and vice versa.
   */
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (secondlib1[pos] && secondlib2[pos])
      add_array(moves, pos);
  }

  /* Capture a neighbor of str1 which is in atari. The captured string
   * must in turn have a neighbor which can connect to str2 easily.
   */
  adj = chainlinks2(str1, adjs, 1);
  for (r = 0; r < adj; r++) {
    adjadj = chainlinks(adjs[r], adjadjs);
    for (s = 0; s < adjadj; s++) {
      if (!same_string(adjadjs[s], str1)
	  && quiescence_connect(adjadjs[s], str2, &move)) {
	findlib(adjs[r], 1, libs);
	add_array(moves, libs[0]);
	add_array(moves, move);
      }
    }
  }

  /* And vice versa. */
  adj = chainlinks2(str2, adjs, 1);
  for (r = 0; r < adj; r++) {
    adjadj = chainlinks(adjs[r], adjadjs);
    for (s = 0; s < adjadj; s++) {
      if (!same_string(adjadjs[s], str2)
	  && quiescence_connect(adjadjs[s], str1, &move)) {
	findlib(adjs[r], 1, libs);
	add_array(moves, libs[0]);
	add_array(moves, move);
      }
    }
  }

  /* Liberties of neighbor of str1 with at most two liberties, which
   * are second order liberties of str2.
   */
  adj = chainlinks3(str1, adjs, 2);
  for (r = 0; r < adj; r++) {
    liberties = findlib(adjs[r], 2, libs);
    for (s = 0; s < liberties; s++)
      if (second_order_liberty_of_string(libs[s], str2))
	add_array(moves, libs[s]);
  }

  /* And vice versa. */
  adj = chainlinks3(str2, adjs, 2);
  for (r = 0; r < adj; r++) {
    liberties = findlib(adjs[r], 2, libs);
    for (s = 0; s < liberties; s++)
      if (second_order_liberty_of_string(libs[s], str1))
	add_array(moves, libs[s]);
  }
  
  /* Move in on a three liberty opponent string which is adjacent to
   * str1 and has a liberty in common with str2.
   */
  adj = chainlinks2(str1, adjs, 3);
  for (r = 0; r < adj; r++) {
    if (have_common_lib(adjs[r], str2, NULL)) {
      liberties = findlib(adjs[r], 3, libs);
      for (s = 0; s < liberties; s++) {
	/* If generating a connecting move, require the liberty to be
         * no further than diagonal to a second order liberty of one
         * of the strings.
	 */
	for (k = 0; k < 8; k++) {
	  if (!does_connect
	      || (ON_BOARD(libs[s] + delta[k])
		  && (secondlib1[libs[s] + delta[k]]
		      || secondlib2[libs[s] + delta[k]]))) {
	    add_array(moves, libs[s]);
	    break;
	  }
	}
      }
    }
  }

  /* And vice versa. */
  adj = chainlinks2(str2, adjs, 3);
  for (r = 0; r < adj; r++) {
    if (have_common_lib(adjs[r], str1, NULL)) {
      liberties = findlib(adjs[r], 3, libs);
      for (s = 0; s < liberties; s++) {
	/* If generating a connecting move, require the liberty to be
         * no further than diagonal to a second order liberty of one
         * of the strings.
	 */
	for (k = 0; k < 8; k++) {
	  if (!does_connect
             || (ON_BOARD(libs[s] + delta[k])
                 && (secondlib1[libs[s] + delta[k]]
                     || secondlib2[libs[s] + delta[k]]))) {
	    add_array(moves, libs[s]);
	    break;
	  }
	}
      }
    }
  }
  
  return 0;
}


/* Not yet written.
 *
 * Find the complete set of possible moves that can prevent
 * a connection in 5 plies.
 */

static int
moves_to_prevent_connection_in_three_moves(int *moves, int str1, int str2)
{
  if (moves_to_connect_in_three_moves(moves, str1, str2, 0))
    return 1;
  return 0;
}

/* 
 * The simplest depth 4 connection:
 *
 * If there are forced moves to prevent connection in one move,
 * try them, and verify that they all lead to a depth 1 or
 * depth 3 connection.
 * 
 * This is the g211 game function.
 */

static int
simply_connected_two_moves(int str1, int str2)
{
  int r, res = 0;
  int moves[MAX_MOVES];
  SGFTree *save_sgf_dumptree = sgf_dumptree;

  /* turn off the sgf traces
   */
  sgf_dumptree = NULL;
  
  
  /* If one string is missing we have already failed. */
  if (board[str1] == EMPTY || board[str2] == EMPTY)
    return 0;
  
  moves[0] = 0;
  if (prevent_connection_one_move(moves, str1, str2)) {
    res = WIN;
    order_connection_moves(moves, str1, str2, OTHER_COLOR(board[str1]),
			   "simply_connected_two_moves");
    for (r = 1; ((r < moves[0] + 1) && res); r++) {
      if (trymove(moves[r], OTHER_COLOR(board[str1]),
		  "simply_connected_two_moves", str1)) {
	if (!connection_one_move(str1, str2))
	  if (!connection_two_moves(str1, str2))
	    res = 0;
	popgo();
      }
    }
  }
  
  sgf_dumptree = save_sgf_dumptree;
  
  return res;
}

/* Test if a move is a simple depth 5 connection.
 *
 * This is the gi311 game function.
 */

static int
simple_connection_three_moves(int str1, int str2)
{
  int r, res = 0, moves[MAX_MOVES];
  SGFTree *save_sgf_dumptree = sgf_dumptree;

  /* turn off the sgf traces
   */
  sgf_dumptree = NULL;
    
  
  moves[0] = 0;
  if (moves_to_connect_in_two_moves(moves, str1, str2))
    return WIN;
  order_connection_moves(moves, str1, str2, board[str1],
			 "simple_connection_three_moves");
  for (r = 1; ((r < moves[0] + 1) && !res); r++) {
    if (trymove(moves[r], board[str1],
		"simple_connection_three_moves", str1)) {
      if (simply_connected_two_moves(str1, str2))
	res = WIN;
      popgo();
    }
  }
  
  sgf_dumptree = save_sgf_dumptree;
  
  return res;
}

/* Find the forced moves that prevent a simple depth 5 connection.
 * Fills the array moves with the forced moves.
 *
 * This is the ip311 game function.
 *
 * It finds moves in very important situations such as:
 *
 * + + + O + +
 * + @ @ O + +
 * + @ O @ @ +
 * + @ O + + +
 * + + + + + +
 * - - - - - -
 *
 * and enables recursive_disconnect to prove the two black
 * strings are connected in these situations.
 */

static int
prevent_simple_connection_three_moves(int *moves, int str1, int str2)
{
  int r, res = 0;
  int possible_moves[MAX_MOVES];
  SGFTree *save_sgf_dumptree = sgf_dumptree;

  /* turn off the sgf traces
   */
  sgf_dumptree = NULL;
    
  
  if (simple_connection_three_moves(str1, str2)) {
    res = WIN;
    possible_moves[0] = 0;
    moves_to_prevent_connection_in_three_moves(possible_moves, str1, str2);
    order_connection_moves(moves, str1, str2, OTHER_COLOR(board[str1]),
			   "prevent_simple_connection_three_moves");
    for (r = 1; r < possible_moves[0] + 1; r++) {
      if (trymove(possible_moves[r], OTHER_COLOR(board[str1]), 
		  "prevent_simple_connection_three_moves", str1)) {
	if (!connection_one_move(str1, str2))
	  if (!connection_two_moves(str1, str2))
	    if (!simple_connection_three_moves(str1, str2))
	      add_array(moves, possible_moves[r]);
	popgo();
      }
    }
  }
  
  sgf_dumptree = save_sgf_dumptree;
  
  return res;
}

/* Find simple connections by looking at common liberties
 * or directly capturing a common adjacent string without a snapback
 * or looking at a ladder for a common adjacent string.
 */

static int
quiescence_connect(int str1, int str2, int *move)
{
  int r;
  int lib;
  int adj, adjs[MAXCHAIN];

  /* Common liberties. */
  if (have_common_lib(str1, str2, &lib)) {
    *move = lib;
    return WIN;
  }

  /* Common adjacent string in atari, more than one stone, no snapback. */
  adj = chainlinks2(str1, adjs, 1);
  for (r = 0; r < adj; r++)
    if (adjacent_strings(adjs[r], str2)
        && !snapback(adjs[r])) {
      findlib(adjs[r], 1, move);
      return WIN;
    }
  
  /* Common adjacent string two liberties, read ladder. */
  adj = chainlinks2(str1, adjs, 2);
  for (r = 0; r < adj; r++)
    if (adjacent_strings(adjs[r], str2))
      if (quiescence_capture(adjs[r], move))
	return WIN;
  
  return 0;
}


/* A persistent connection cache has been implemented, but currently
 * (3.3.15) it does not have much impact on performance. Possible
 * explanations for this include:
 * 1. The active area is too often unnecessarily large.
 * 2. Between the persistent caches of tactical reading and owl
 *    reading, there is not much to gain from also caching the
 *    connection results.
 * 3. There is some bug in the implementation.
 *
 * In order to simplify testing of code modifications, the caching
 * code has been made conditional. Setting
 * USE_PERSISTENT_CONNECTION_CACHE to 0, 1, or 2 has the following
 * effects.
 * 0 - Completely turned off.
 * 1 - Results are stored in the cache but retrieved results are only
 *     compared to the non-cached result. Deviations are reported.
 * 2 - Fully turned on.
 */
#define USE_PERSISTENT_CONNECTION_CACHE 0


/* Externally callable frontend to recursive_connect().
 * Returns WIN if str1 and str2 can be connected. 
 */
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
  
  if (alternate_connections) {
    int reading_nodes_when_called = get_reading_node_counter();
    double start = 0;
    int tactical_nodes;
    int save_connection_node_limit = connection_node_limit;
#if USE_PERSISTENT_CONNECTION_CACHE == 1
    int result2 = -1;
    int move2;
#endif
    if (board[str1] == EMPTY || board[str2] == EMPTY)
      return 0;
    str1 = find_origin(str1);
    str2 = find_origin(str2);
    if (str1 > str2) {
      int tmp = str1;
      str1 = str2;
      str2 = tmp;
    }


#if USE_PERSISTENT_CONNECTION_CACHE == 1
    if (!search_persistent_connection_cache(CONNECT, str1, str2,
					    &result2, &move2))
      result2 = -1;
    else if (0)
      gprintf("Persistent cache found connect %1m %1m: %d %1m\n",
	      str1, str2, result2, move2);
#endif
#if USE_PERSISTENT_CONNECTION_CACHE == 2
    if (search_persistent_connection_cache(CONNECT, str1, str2, &result, move))
      return result;
#endif

    connection_node_limit *= pow(1.45, -stackp + get_depth_modification());
    save_verbose = verbose;
    if (verbose > 0)
      verbose--;
    start = gg_cputime();
    memset(connection_shadow, 0, sizeof(connection_shadow));
    result = recursive_connect2(str1, str2, move, 0);
    verbose = save_verbose;
    tactical_nodes = get_reading_node_counter() - reading_nodes_when_called;
    connection_node_limit = save_connection_node_limit;

#if USE_PERSISTENT_CONNECTION_CACHE == 1
    if (result2 != -1
	&& result2 != result
	&& *move != move2)
      gprintf("Persistent cache failure connect %1m %1m: %d %1m != %d %1m\n",
	      str1, str2, result, *move, result2, move2);
#endif

    if (0) {
      gprintf("%oconnect    %1M %1M, result %d %1M (%d, %d nodes, %f seconds)\n",
	      str1, str2, result, *move,
	      nodes_connect, tactical_nodes, gg_cputime() - start);
      dump_stack();
    }
    if (0) {
      gprintf("%oconnect %1m %1m %d %1m ", str1, str2, result, *move);
      dump_stack();
    }

#if USE_PERSISTENT_CONNECTION_CACHE > 0
    store_persistent_connection_cache(CONNECT, str1, str2, result, *move,
				      tactical_nodes, connection_shadow);
#endif
  
    return result;
  }

  return recursive_connect(str1, str2, move);
}


/* returns WIN if str1 and str2 can be connected. */

static int
recursive_connect(int str1, int str2, int *move)
{
  int i, res = 0, Moves[MAX_MOVES], ForcedMoves[MAX_MOVES];
  SETUP_TRACE_INFO2("recursive_connect", str1, str2);
  
  if (board[str1] == EMPTY || board[str2] == EMPTY) {
    SGFTRACE2(PASS_MOVE, 0, "one string already captured");
    return 0;
  }
  
  if (same_string(str1, str2)) {
    SGFTRACE2(PASS_MOVE, WIN, "already connected");
    return WIN;
  }

  if (nodes_connect > connection_node_limit) {
    SGFTRACE2(PASS_MOVE, 0, "connection node limit reached");
    return 0;
  }
  
  if (stackp == connect_depth) {
    SGFTRACE2(PASS_MOVE, 0, "connection depth limit reached");
    return 0;
  }

  nodes_connect++;
  global_connection_node_counter++;
  
  if (quiescence_connect (str1, str2, move)) {
    SGFTRACE2(*move, WIN, "quiescence_connect");
    return WIN;
  }

  ForcedMoves[0] = 0;
  Moves[0] = 0;
  /* if one of the strings to connect can be captured
   * and there are forced moves to prevent the capture
   * then the only moves to try are the moves that
   * defend the string: all the other moves will
   * lead to the capture of the string
   */
  if (!prevent_capture_one_move(ForcedMoves, str1))
    prevent_capture_one_move(ForcedMoves, str2);
#if 0
  else if (prevent_capture_two_moves(ForcedMoves, str1))
     ; 
  else if (prevent_capture_two_moves(ForcedMoves, str2))
     ; 
#endif
  
  /* We are at a max node, so any move we can find
   * is ok. Try moves that can connect in three moves
   * because the function that prevent connection in one
   * and two moves are called at AND nodes.
   */
  moves_to_connect_in_three_moves(Moves, str1, str2, 1);

  /* if there are some forced moves to prevent the capture
   * of one of the two strings, then we only look at
   * the moves that prevent capture and that might also
   * connect
   */
  if (ForcedMoves[0] != 0 && Moves[0] != 0)
    intersection_array(Moves, ForcedMoves);

  order_connection_moves(Moves, str1, str2, board[str1],
			 "recursive_connect");
  for (i = 1; ((i < Moves[0] + 1) && (res == 0)); i++) {
    if (trymove(Moves[i], board[str1], "recursive_connect", str1)) {
      if (!recursive_disconnect(str1, str2, move)) {
	*move = Moves[i];
	res = WIN;
      }
      popgo();
    }
  }

  if (res == WIN) {
    SGFTRACE2(*move, WIN, "success");
  }
  else {
    SGFTRACE2(PASS_MOVE, 0, "failure");
  }
  
  return res;
}
  

/* Externally callable frontend to recursive_disconnect().
 * Returns WIN if str1 and str2 can be disconnected. 
 */
int
disconnect(int str1, int str2, int *move)
{
  int i;
  int res = WIN;
  int Moves[MAX_MOVES];
  int dummy_move;
  int result;
  int save_verbose;
  
  if (move == NULL)
    move = &dummy_move;
  
  nodes_connect = 0;
  *move = PASS_MOVE;
  
  if (alternate_connections) {
    int reading_nodes_when_called = get_reading_node_counter();
    int save_connection_node_limit = connection_node_limit;
    double start = 0;
    int tactical_nodes;
#if USE_PERSISTENT_CONNECTION_CACHE == 1
    int result2 = -1;
    int move2;
#endif
    if (board[str1] == EMPTY || board[str2] == EMPTY)
      return WIN;
    str1 = find_origin(str1);
    str2 = find_origin(str2);
    if (str1 > str2) {
      int tmp = str1;
      str1 = str2;
      str2 = tmp;
    }

#if USE_PERSISTENT_CONNECTION_CACHE == 1
    if (!search_persistent_connection_cache(DISCONNECT, str1, str2,
					    &result2, &move2))
      result2 = -1;
    else if (0)
      gprintf("Persistent cache found disconnect %1m %1m: %d %1m\n",
	      str1, str2, result2, move2);
#endif
#if USE_PERSISTENT_CONNECTION_CACHE == 2
    if (search_persistent_connection_cache(DISCONNECT, str1, str2,
					   &result, move))
      return result;
#endif

    connection_node_limit *= pow(1.5, -stackp + get_depth_modification());
    save_verbose = verbose;
    if (verbose > 0)
      verbose--;
    start = gg_cputime();
    memset(connection_shadow, 0, sizeof(connection_shadow));
    result = recursive_disconnect2(str1, str2, move, 0);
    verbose = save_verbose;
    tactical_nodes = get_reading_node_counter() - reading_nodes_when_called;
    connection_node_limit = save_connection_node_limit;

#if USE_PERSISTENT_CONNECTION_CACHE == 1
    if (result2 != -1
	&& result2 != result
	&& *move != move2)
      gprintf("Persistent cache failure disconnect %1m %1m: %d %1m != %d %1m\n",
	      str1, str2, result, *move, result2, move2);
#endif

    if (0) {
      gprintf("%odisconnect %1m %1m, result %d %1m (%d, %d nodes, %f seconds)\n",
	      str1, str2, result, *move,
	      nodes_connect, tactical_nodes, gg_cputime() - start);
      dump_stack();
    }
    if (0) {
      gprintf("%odisconnect %1m %1m %d %1m ", str1, str2, result, *move);
      dump_stack();
    }

#if USE_PERSISTENT_CONNECTION_CACHE > 0
    store_persistent_connection_cache(DISCONNECT, str1, str2, result, *move,
				      tactical_nodes, connection_shadow);
#endif

    return result;
  }

  Moves[0] = 0;
  moves_to_prevent_connection_in_three_moves(Moves, str1, str2);
  if (Moves[0] > 0)
    res = 0;
  order_connection_moves(Moves, str1, str2, OTHER_COLOR(board[str1]),
			 "disconnect");
  for (i = 1; ((i < Moves[0] + 1) && (res == 0)); i++)
    if (trymove(Moves[i], OTHER_COLOR(board[str1]),
		"disconnect", str1)) {
      if (!recursive_connect(str1, str2, move)) {
	*move = Moves[i];
	res = WIN;
      }
      popgo();
    }
  return res;
}

/* Externally callable frontend to recursive_disconnect().
 * Returns WIN if str1 and str2 can be disconnected. 
 *
 * Uses much lower node and depths limits.
 */
int
fast_disconnect(int str1, int str2, int *move)
{
  int result;
  int save_limit = connection_node_limit;
  int save_verbose = verbose;

  if (board[str1] == EMPTY || board[str2] == EMPTY)
    return WIN;
  str1 = find_origin(str1);
  str2 = find_origin(str2);
  if (str1 > str2) {
    int tmp = str1;
    str1 = str2;
    str2 = tmp;
  }

  modify_depth_values(-3);
  connection_node_limit /= 4;

  if (verbose > 0)
    verbose--;
  result = recursive_disconnect2(str1, str2, move, 0);
  verbose = save_verbose;

  connection_node_limit = save_limit;
  modify_depth_values(3);

  return result;
}



/* Returns WIN if str1 and str2 can be disconnected. */

static int
recursive_disconnect(int str1, int str2, int *move)
{
  int i, res = WIN, Moves[MAX_MOVES];
  
  SETUP_TRACE_INFO2("recursive_disconnect", str1, str2);
  
  if (board[str1] == EMPTY || board[str2] == EMPTY) {
    SGFTRACE2(PASS_MOVE, WIN, "one string already captured");
    return WIN;
  }

  if (quiescence_capture(str1, move)) {
    SGFTRACE2(*move, WIN, "first string capturable");
    return WIN;
  }
  if (quiescence_capture(str2, move)) {
    SGFTRACE2(*move, WIN, "second string capturable");
    return WIN;
  }

  if (same_string(str1, str2)) {
    SGFTRACE2(PASS_MOVE, 0, "already connected");
    return 0;
  }
  
  if (nodes_connect > connection_node_limit) {
    SGFTRACE2(PASS_MOVE, WIN, "connection node limit reached");
    return WIN;
  }
  
  if (stackp == connect_depth) {
    SGFTRACE2(PASS_MOVE, WIN, "connection depth limit reached");
    return WIN;
  }
  
  nodes_connect++;
  global_connection_node_counter++;
  
  /* we are at an and node
   * only look at forced moves here,
   * it ensures that the result of recursive_disconnect
   * is proved if it returns 0 (that is connections are proved)
   */
  Moves[0] = 0;

  if (prevent_connection_one_move(Moves, str1, str2))
    res = 0;
  else if (prevent_connection_two_moves(Moves, str1, str2))
    res = 0;
  else if (prevent_simple_connection_three_moves(Moves, str1, str2))
    res = 0;
  
  if (res == 0)
    order_connection_moves(Moves, str1, str2, OTHER_COLOR(board[str1]),
			   "recursive_disconnect");
    for (i = 1; ((i < Moves[0] + 1) && (res == 0)); i++)
      if (trymove(Moves[i], OTHER_COLOR(board[str1]),
		  "recursive_disconnect", str1)) {
	if (!recursive_connect(str1, str2, move)) {
	  *move = Moves[i];
	  res = WIN;
	}
	popgo();
      }

  if (res == WIN) {
    SGFTRACE2(*move, WIN, "success");
  }
  else {
    SGFTRACE2(PASS_MOVE, 0, "failure");
  }
  
  return res;
}
 
/* Reads simple ladders.
 */

static int
quiescence_capture(int str, int *move)
{
  SGFTree *save_sgf_dumptree = sgf_dumptree;
  int save_count_variations = count_variations;
  int result = 0;

  /* We turn off the sgf traces here to avoid cluttering them up with
   * naive_ladder moves.
   */
  sgf_dumptree = NULL;
  count_variations = 0;

  if (countlib(str) == 1) {
    findlib(str, 1, move);
    result = WIN;
  }
  else if (countlib(str) == 2)
    result = simple_ladder(str, move);

  /* Turn the sgf traces back on. */
  sgf_dumptree = save_sgf_dumptree;
  count_variations = save_count_variations;

  return result;
}

#if 0
static int
capture_one_move(int str) 
{
  if (countlib(str) == 1)
    return 1;
  return 0;
}
#endif

/* Find all the possible moves that can prevent the capture
 * of a string in atari.
 *
 * The ip1 game function.
 */

static int
prevent_capture_one_move(int *moves, int str1)
{
  int r, res = 0;
  int liberties, libs[MAXLIBS];
  int adj, adjs[MAXCHAIN];
  
  liberties = findlib(str1, MAXLIBS, libs);
  if (liberties == 1) {
    add_array(moves, libs[0]);
    res = WIN;
    adj = chainlinks2(str1, adjs, 1);
    for (r = 0; r < adj; r++) {
      findlib(adjs[r], 1, libs);
      add_array(moves, libs[0]);
    }
  }
  return res;
}


/* Returns WIN if str1, str2 and str3 can be connected. */

static int
recursive_transitivity(int str1, int str2, int str3, int *move)
{
  int i, res = 0, Moves[MAX_MOVES], ForcedMoves[MAX_MOVES];

  SETUP_TRACE_INFO2("recursive_transitivity", str1, str3);
  
  if (board[str1] == EMPTY || board[str2] == EMPTY || board[str3] == EMPTY) {
    SGFTRACE2(PASS_MOVE, 0, "one string already captured");
    return 0;
  }
  
  if (same_string(str1, str2) && same_string(str1, str3)) {
    SGFTRACE2(PASS_MOVE, WIN, "already connected");
    return WIN;
  }

  if (nodes_connect > connection_node_limit) {
    SGFTRACE2(PASS_MOVE, 0, "connection node limit reached");
    return 0;
  }
  
  if (stackp == connect_depth) {
    SGFTRACE2(PASS_MOVE, 0, "connection depth limit reached");
    return 0;
  }

  nodes_connect++;
  global_connection_node_counter++;
  
  if (same_string(str1, str2))
    if (quiescence_connect (str1, str3, move)) {
      SGFTRACE2(*move, WIN, "quiescence_connect");
      return WIN;
    }

  if (same_string(str2, str3))
    if (quiescence_connect (str1, str2, move)) {
      SGFTRACE2(*move, WIN, "quiescence_connect");
      return WIN;
    }

  ForcedMoves[0] = 0;
  Moves[0] = 0;
  /* If one of the strings to connect can be captured
   * and there are forced moves to prevent the capture
   * then the only moves to try are the moves that
   * defend the string. All the other moves will
   * lead to the capture of the string.
   */
  if (!prevent_capture_one_move(ForcedMoves, str1))
    if (!prevent_capture_one_move(ForcedMoves, str2))
      prevent_capture_one_move(ForcedMoves, str3);
  
  /* We are at a max node, so any move we can find
   * is ok. Try moves that can connect in two moves
   * because the function that prevents connection in one
   * move is called at and nodes.
   */
  moves_to_connect_in_two_moves(Moves, str1, str2);
  moves_to_connect_in_two_moves(Moves, str2, str3);

  /* If there are some forced moves to prevent the capture
   * of one of the two strings, then we only look at
   * the moves that prevent capture and that might also
   * connect.
   */
  if ((ForcedMoves[0] != 0) && (Moves[0] != 0))
    intersection_array(Moves, ForcedMoves);

  order_connection_moves(Moves, str1, str2, board[str1],
			 "recursive_transitivity");
  for (i = 1; ((i < Moves[0] + 1) && (res == 0)); i++) {
    if (trymove(Moves[i], board[str1], "recursive_transitivity", str1)) {
      if (!recursive_non_transitivity(str1, str2, str3, move)) {
	*move = Moves[i];
	res = WIN;
      }
      popgo();
    }
  }

  if (res == WIN) {
    SGFTRACE2(*move, WIN, "success");
  }
  else {
    SGFTRACE2(PASS_MOVE, 0, "failure");
  }
  
  return res;
}
  
/* It is often assumed that if str1 connects to str2 and str2 
 * connects to str3 then str1 connects to str3. This is called
 * TRANSITIVITY. However there are exceptions such as this
 * situation:
 * 
 *     XXXXX            XXXXX
 *     OO.OO            AA*CC
 *     ..O..            ..B..
 *     XXXXX            XXXXX
 *
 * Although strings A and B are connected, and strings B and C
 * are connected, a move at * disconnects strings A and C.
 *
 * This function is a public frontend to recursive_non_transitivity(). 
 * Returns WIN if str1, str2 and str3 can be disconnected.
*/

int
non_transitivity(int str1, int str2, int str3, int *move)
{
  int i, res = WIN, Moves[MAX_MOVES];
  
  nodes_connect = 0;
  *move = PASS_MOVE;
  moves_to_prevent_connection_in_three_moves(Moves, str1, str3);
  if (Moves[0] > 0)
    res = 0;
  order_connection_moves(Moves, str1, str2, OTHER_COLOR(board[str1]),
			 "non_transitivity");
  for (i = 1; ((i < Moves[0] + 1) && (res == 0)); i++)
    if (trymove(Moves[i], OTHER_COLOR(board[str1]),
		"non_transitivity", str1)) {
      if (!recursive_transitivity(str1, str2, str3, move)) {
	*move = Moves[i];
	res = WIN;
      }
      popgo();
    }
  return res;
}

/* Returns WIN if str1, str2 and str3 can be disconnected. */

static int
recursive_non_transitivity(int str1, int str2, int str3, int *move)
{
  int i, res = WIN, Moves[MAX_MOVES];
  
  SETUP_TRACE_INFO2("recursive_non_transitivity", str1, str3);
  
  if (board[str1] == EMPTY || board[str2] == EMPTY
      || board[str3] == EMPTY) {
    SGFTRACE2(PASS_MOVE, WIN, "one string already captured");
    return WIN;
  }

  if (quiescence_capture(str1, move)) {
    SGFTRACE2(*move, WIN, "first string capturable");
    return WIN;
  }
  if (quiescence_capture(str2, move)) {
    SGFTRACE2(*move, WIN, "second string capturable");
    return WIN;
  }
  if (quiescence_capture(str3, move)) {
    SGFTRACE2(*move, WIN, "third string capturable");
    return WIN;
  }

  if (same_string(str1, str2) && same_string(str1, str3)) {
    SGFTRACE2(PASS_MOVE, 0, "already connected");
    return 0;
  }
  
  if (nodes_connect > connection_node_limit) {
    SGFTRACE2(PASS_MOVE, WIN, "connection node limit reached");
    return WIN;
  }
  
  if (stackp == connect_depth) {
    SGFTRACE2(PASS_MOVE, WIN, "connection depth limit reached");
    return WIN;
  }
  
  nodes_connect++;
  global_connection_node_counter++;
  
  /* We are at an and node. Only look at forced moves. */
  Moves[0] = 0;
  if (prevent_connection_one_move(Moves, str1, str3))
    res = 0;
  else if (prevent_connection_two_moves(Moves, str1, str3))
    res = 0;
  else if (prevent_simple_connection_three_moves(Moves, str1, str3))
    res = 0;
  
  if (res == 0)
    order_connection_moves(Moves, str1, str2, OTHER_COLOR(board[str1]),
			   "recursive_non_transitivity");
    for (i = 1; ((i < Moves[0] + 1) && (res == 0)); i++)
      if (trymove(Moves[i], OTHER_COLOR(board[str1]),
		  "recursive_non_transitivity", str1)) {
	if (!recursive_transitivity(str1, str2, str3, move)) {
	  *move = Moves[i];
	  res = WIN;
	}
	popgo();
      }

  if (res == WIN) {
    SGFTRACE2(*move, WIN, "success");
  }
  else {
    SGFTRACE2(PASS_MOVE, 0, "failure");
  }
  
  return res;
}


/* Order the moves so that we try the ones likely to succeed early. */
static void
order_connection_moves(int *moves, int str1, int str2, int color_to_move,
		       const char *funcname)
{
  int scores[MAX_MOVES];
  int r;
  int i, j;
  UNUSED(str2);
  UNUSED(color_to_move);

  for (r = 1; r <= moves[0]; r++) {
    int move = moves[r];

    /* Look at the neighbors of this move and count the things we
     * find. Friendly and opponent stones are related to color, i.e.
     * the player to move, not to the color of the string.
     *
     * We don't use all these values. They are only here so we can
     * reuse incremental_order_moves() which was developed for the
     * tactical reading.
     */
    int number_edges       = 0; /* outside board */
    int number_same_string = 0; /* the string being attacked */
    int number_own         = 0; /* friendly stone */
    int number_opponent    = 0; /* opponent stone */
    int captured_stones    = 0; /* number of stones captured by this move */
    int threatened_stones  = 0; /* number of stones threatened by this move */
    int saved_stones       = 0; /* number of stones in atari saved */
    int number_open        = 0; /* empty intersection */
    int libs;

    /* We let the incremental board code do the heavy work. */
    incremental_order_moves(move, color_to_move, str1, &number_edges,
			    &number_same_string, &number_own,
			    &number_opponent, &captured_stones,
			    &threatened_stones, &saved_stones, &number_open);

    if (0)
      gprintf("%o %1m values: %d %d %d %d %d %d %d %d\n", move, number_edges,
	      number_same_string, number_own, number_opponent,
	      captured_stones, threatened_stones, saved_stones, number_open);

    scores[r] = 0;
    libs = approxlib(move, color_to_move, 10, NULL);

    /* Avoid self atari. */
    if (libs == 1 && captured_stones == 0)
      scores[r] -= 10;

    /* Good to get many liberties. */
    if (libs < 4)
      scores[r] += libs;
    else
      scores[r] += 4;

    /* Very good to capture opponent stones. */
    if (captured_stones > 0)
      scores[r] += 5 + captured_stones;

    /* Good to threaten opponent stones. */
    if (threatened_stones > 0)
      scores[r] += 3;

    /* Extremely good to save own stones. */
    if (saved_stones > 0)
      scores[r] += 10 + saved_stones;
  }
  
  /* Now sort the moves.  We use selection sort since this array will
   * probably never be more than 10 moves long.  In this case, the
   * overhead imposed by quicksort will probably overshadow the gains
   * given by the O(n*log(n)) behaviour over the O(n^2) behaviour of
   * selection sort.
   */
  for (i = 1; i <= moves[0]; i++) {
    /* Find the move with the biggest score. */
    int maxscore = scores[i];
    int max_at = i;
    for (j = i+1; j <= moves[0]; j++) {
      if (scores[j] > maxscore) {
	maxscore = scores[j];
	max_at = j;
      }
    }

    /* Now exchange the move at i with the move at max_at.
     * Don't forget to exchange the scores as well.
     */
    if (max_at != i) {
      int temp = moves[i];
      int tempmax = scores[i];

      moves[i] = moves[max_at];
      scores[i] = scores[max_at];

      moves[max_at] = temp;
      scores[max_at] = tempmax;
    }
  }

  if (0) {
    gprintf("%oVariation %d:\n", count_variations);
    for (i = 1; i <= moves[0]; i++)
      gprintf("%o  %1M %d\n", moves[i], scores[i]);
  }

  if (sgf_dumptree) {
    char buf[500];
    char *pos;
    int chars;
    sprintf(buf, "Move order for %s: %n", funcname, &chars);
    pos = buf + chars;
    for (i = 1; i <= moves[0]; i++) {
      sprintf(pos, "%c%d (%d) %n", J(moves[i]) + 'A' + (J(moves[i]) >= 8),
	      board_size - I(moves[i]), scores[i], &chars);
      pos += chars;
    }
    sgftreeAddComment(sgf_dumptree, buf);
  }
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
 * This code is enabled with the --enable-alternate-connections
 * configure flag at build time or toggled with the
 * --alternate-connections option at run time.
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


static int find_string_connection_moves(int str1, int str2, int color_to_move,
				        int moves[MAX_MOVES],
					int *total_distance);
static void clear_connection_data(struct connection_data *conn);
static int trivial_connection(int str1, int str2, int *move);
static int does_secure_through_ladder(int color, int move, int pos);
static int ladder_capture(int str, int *move);
static int ladder_capturable(int pos, int color);
static int no_escape_from_atari(int str);
static int no_escape_from_ladder(int str);
static int check_self_atari(int pos, int color_to_move);
static int common_vulnerabilities(int a1, int a2, int b1, int b2, int color);
static int common_vulnerability(int apos, int bpos, int color);

/* Try to connect two strings. This function is called in a mutual
 * recursion with recursive_disconnect2(). Return codes is identical to
 * the tactical reading functions. For the has_passed parameter, see the
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
recursive_connect2(int str1, int str2, int *move, int has_passed)
{
  int color = board[str1];
  int moves[MAX_MOVES];
  int num_moves;
  int distance = FP(0.0);
  int k;
  int xpos;
  int savemove = NO_MOVE;
  int savecode = 0;
  int tried_moves = 0;
  int value;
  
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

  if (nodes_connect > connection_node_limit) {
    SGFTRACE2(PASS_MOVE, 0, "connection node limit reached");
    return 0;
  }
  
  if (stackp > connect_depth2) {
    SGFTRACE2(PASS_MOVE, 0, "connection depth limit reached");
    return 0;
  }

  str1 = find_origin(str1);
  str2 = find_origin(str2);

  if (stackp <= depth && !has_passed
      && tt_get(&ttable, CONNECT, str1, str2, depth - stackp, NULL,
		&value, NULL, &xpos) == 2) {
    TRACE_CACHED_RESULT2(value, value, xpos);
    if (value != 0)
      if (move)
	*move = xpos;

    SGFTRACE2(xpos, value, "cached");
    return value;
  }
  
  if (trivial_connection(str1, str2, &xpos) == WIN) {
    SGFTRACE2(xpos, WIN, "trivial connection");
    READ_RETURN_CONN(CONNECT, str1, str2, depth - stackp, move, xpos, WIN);
  }
  
  num_moves = find_string_connection_moves(str1, str2, color,
      					   moves, &distance);
  
  for (k = 0; k < num_moves; k++) {
    int ko_move;
    xpos = moves[k];
    
    if (komaster_trymove(xpos, color, "recursive_connect2", str1,
			 &ko_move, stackp <= ko_depth && savecode == 0)) {
      tried_moves++;
      if (!ko_move) {
	int acode = recursive_disconnect2(str1, str2, NULL,
					 
					  has_passed);
	popgo();
	if (acode == 0) {
	  SGFTRACE2(xpos, WIN, "connection effective");
	  READ_RETURN_CONN(CONNECT, str1, str2, depth - stackp,
			   move, xpos, WIN);
	}
	/* if the move works with ko we save it, then look for something
	 * better.
	 */
	UPDATE_SAVED_KO_RESULT(savecode, savemove, acode, xpos);
      }
      else {
	if (recursive_disconnect2(str1, str2, NULL,
				 
				  has_passed) != WIN) {
	  savemove = xpos;
	  savecode = KO_B;
	}
	popgo();
      }
    }
  }

  if (tried_moves == 0 && distance < FP(1.0)) {
    SGFTRACE2(NO_MOVE, WIN, "no move, probably connected");
    READ_RETURN_CONN(CONNECT, str1, str2, depth - stackp, move, NO_MOVE, WIN);
  }
  
  if (savecode != 0) {
    SGFTRACE2(savemove, savecode, "saved move");
    READ_RETURN_CONN(CONNECT, str1, str2, depth - stackp,
		     move, savemove, savecode);
  }

  SGFTRACE2(0, 0, NULL);
  READ_RETURN_CONN(CONNECT, str1, str2, depth - stackp, move, NO_MOVE, 0);
}


/* Try to disconnect two strings. This function is called in a mutual
 * recursion with recursive_connect2(). Return codes is identical to
 * the tactical reading functions.
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
recursive_disconnect2(int str1, int str2, int *move, int has_passed)
{
  int color = board[str1];
  int other = OTHER_COLOR(color);
  int moves[MAX_MOVES];
  int num_moves;
  int distance = FP(0.0);
  int k;
  int xpos;
  int savemove = NO_MOVE;
  int savecode = 0;
  int tried_moves = 0;
  int attack_code1;
  int attack_pos1;
  int attack_code2;
  int attack_pos2;
  SGFTree *save_sgf_dumptree = sgf_dumptree;
  int save_count_variations = count_variations;
  int value;

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
  
  if (nodes_connect > connection_node_limit) {
    SGFTRACE2(PASS_MOVE, WIN, "connection node limit reached");
    return WIN;
  }
  
  if (stackp > connect_depth2) {
    SGFTRACE2(PASS_MOVE, WIN, "connection depth limit reached");
    return WIN;
  }

  sgf_dumptree = NULL;
  count_variations = 0;

  str1 = find_origin(str1);
  str2 = find_origin(str2);

  attack_code1 = attack(str1, &attack_pos1);
  if (attack_code1 == WIN) {
    sgf_dumptree = save_sgf_dumptree;
    count_variations = save_count_variations;

    SGFTRACE2(attack_pos1, WIN, "one string is capturable");
    if (move)
      *move = attack_pos1;

    return WIN;
  }

  attack_code2 = attack(str2, &attack_pos2);
  if (attack_code2 == WIN) {
    sgf_dumptree = save_sgf_dumptree;
    count_variations = save_count_variations;

    SGFTRACE2(attack_pos2, WIN, "one string is capturable");
    if (move)
      *move = attack_pos2;

    return WIN;
  }

  sgf_dumptree = save_sgf_dumptree;
  count_variations = save_count_variations;

  if (stackp <= depth
      && tt_get(&ttable, DISCONNECT, str1, str2,
		depth - stackp, NULL,
		&value, NULL, &xpos) == 2) {
    TRACE_CACHED_RESULT2(value, value, xpos);
    if (value != 0)
      if (move)
	*move = xpos;

    SGFTRACE2(xpos, value, "cached");
    return value;
  }

  if (ladder_capture(str1, &xpos) == WIN) {
    SGFTRACE2(xpos, WIN, "first string capturable");
    READ_RETURN_CONN(DISCONNECT, str1, str2, depth - stackp, move, xpos, WIN);
  }
  
  if (ladder_capture(str2, &xpos) == WIN) {
    SGFTRACE2(xpos, WIN, "second string capturable");
    READ_RETURN_CONN(DISCONNECT, str1, str2, depth - stackp, move, xpos, WIN);
  }

  num_moves = find_string_connection_moves(str1, str2, other,
      					   moves, &distance);

  if (attack_code1 != 0 && num_moves < MAX_MOVES) {
    for (k = 0; k < num_moves; k++) {
      if (moves[k] == attack_pos1)
	break;
    }

    if (k == num_moves)
      moves[num_moves++] = attack_pos1;
  }

  if (attack_code2 != 0 && num_moves < MAX_MOVES) {
    for (k = 0; k < num_moves; k++) {
      if (moves[k] == attack_pos2)
	break;
    }

    if (k == num_moves)
      moves[num_moves++] = attack_pos2;
  }

  for (k = 0; k < num_moves; k++) {
    int ko_move;
    xpos = moves[k];
    
    if (komaster_trymove(xpos, other, "recursive_disconnect2", str1,
			 &ko_move, stackp <= ko_depth && savecode == 0)) {
      tried_moves++;
      if (!ko_move) {
	int dcode = recursive_connect2(str1, str2, NULL,
				       has_passed);
	popgo();
	if (dcode == 0) {
	  SGFTRACE2(xpos, WIN, "disconnection effective");
	  READ_RETURN_CONN(DISCONNECT, str1, str2, depth - stackp,
			   move, xpos, WIN);
	}
	/* if the move works with ko we save it, then look for something
	 * better.
	 */
	UPDATE_SAVED_KO_RESULT(savecode, savemove, dcode, xpos);
      }
      else {
	if (recursive_connect2(str1, str2, NULL,
			      
			       has_passed) != WIN) {
	  savemove = xpos;
	  savecode = KO_B;
	}
	popgo();
      }
    }
  }

  if (tried_moves == 0
      && distance >= FP(1.0)
      && (has_passed
	  || !recursive_connect2(str1, str2, NULL, 1))) {
    SGFTRACE2(NO_MOVE, WIN, "no move, probably disconnected");
    READ_RETURN_CONN(DISCONNECT, str1, str2, depth - stackp,
		     move, NO_MOVE, WIN);
  }
  
  if (savecode != 0) {
    SGFTRACE2(savemove, savecode, "saved move");
    READ_RETURN_CONN(DISCONNECT, str1, str2, depth - stackp, 
		     move, savemove, savecode);
  }

  SGFTRACE2(0, 0, NULL);
  READ_RETURN_CONN(DISCONNECT, str1, str2, depth - stackp, move, NO_MOVE, 0);
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
    		      struct connection_data *conn1,
    		      struct connection_data *conn2,
		      int max_dist1, int max_dist2,
		      int moves[MAX_MOVES], int total_distance,
		      int cutoff)
{
  int color = board[str1];
  int other = OTHER_COLOR(color);
  int connect_move = (color_to_move == color);
  int r;
  int distances[MAX_MOVES];
  int num_moves = 0;
  int acode = 0;
  int attack_move = NO_MOVE;
  int dcode = 0;
  int defense_move = NO_MOVE;
  int k;
  int i, j;
  SGFTree *save_sgf_dumptree = sgf_dumptree;
  int save_count_variations = count_variations;
  int distance_limit;

  /* We turn off the sgf traces here to avoid cluttering them up with
   * tactical reading moves.
   */
  sgf_dumptree = NULL;
  count_variations = 0;

  /* Loop through the points with smallish distance from str1 and look
   * for ones also having a small distance to str2.
   */
  for (r = 0; r < conn1->queue_end; r++) {
    int pos = conn1->queue[r];
    int dist1 = conn1->distances[pos];
    int deltadist1 = conn1->deltas[pos];
    int dist2 = conn2->distances[pos];
    int deltadist2 = conn2->deltas[pos];
    int d1;
    int d2;
    int distance;
    
    if (dist1 - deltadist1 + dist2 - deltadist2 > FP(2.5)
	|| dist1 > max_dist1 + FP(0.2)
	|| dist2 > max_dist2 + FP(0.2))
      continue;

    if (verbose > 0)
      gprintf("%oMove %1m, (%f, %f, %f, %f)\n", pos, 
	      FIXED_TO_FLOAT(dist1), FIXED_TO_FLOAT(deltadist1), 
	      FIXED_TO_FLOAT(dist2), FIXED_TO_FLOAT(deltadist2));

    /* The basic quality of the move is the sum of the distances to
     * each string minus the two delta values. This distance value
     * will subsequently be modified to take other factors into
     * account.
     */
    d1 = dist1 - deltadist1;
    d2 = dist2 - deltadist2;
    distance = d1 + d2;
    if (verbose > 0)
      gprintf("%o  %f, primary distance\n", FIXED_TO_FLOAT(distance));
    
    /* Bonus if d1 and d2 are well balanced. */
    if ((3 * d1) / 2 > d2 && (3 * d2) / 2 > d1) {
      distance -= FP(0.1);
      if (verbose > 0)
	gprintf("%o  -0.1, well balanced\n");
    }

    /* Check whether the move is "between" the two strings. */
    if (conn1->coming_from[pos] != NO_MOVE
	&& conn1->coming_from[pos] == conn2->coming_from[pos]) {
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
	  distance += FP(0.5);
	  if (verbose > 0)
	    gprintf("%o  +0.5, no defense\n");
	}
	else {
	  if (conn1->distances[attack_move]
	      + conn2->distances[attack_move] > dist1 + dist2) {
	    distance += FP(0.5);
	    if (verbose > 0)
	      gprintf("%o  +0.5, attack point not on shortest path\n");
	  }
	}
	ADD_CANDIDATE_MOVE(attack_move, distance - FP(0.15), moves, distances,
			   num_moves);
	if (verbose > 0)
	  gprintf("%o  -0.15 at %1m, capturing a string\n", attack_move);
      }
      else if (!connect_move && acode != 0 && dcode != 0) {
	ADD_CANDIDATE_MOVE(defense_move, distance - FP(0.5), moves, distances,
			   num_moves);
	if (verbose > 0)
	  gprintf("%o  -0.5 at %1m, defending a string\n", defense_move);
      }
    }
    else if (board[pos] == color) {
      /* Check whether there are common vulnerable points. */
      for (k = 0; k < 4; k++) {
	int apos, bpos;
	if (k & 1)
	  apos = conn1->vulnerable1[pos];
	else 
	  apos = conn1->vulnerable2[pos];

	if (k & 2)
	  bpos = conn2->vulnerable1[pos];
	else 
	  bpos = conn2->vulnerable2[pos];

	if (common_vulnerability(apos, bpos, color)) {
	  if (check_self_atari(apos, color_to_move)) {
	    ADD_CANDIDATE_MOVE(apos, distance, moves, distances, num_moves);
	    if (verbose > 0)
	      gprintf("%o  +0.0 at %1m, vulnerability\n", apos);
	  }

	  if (bpos != apos
	      && check_self_atari(bpos, color_to_move)) {
	    ADD_CANDIDATE_MOVE(bpos, distance, moves, distances, num_moves);
	    if (verbose > 0)
	      gprintf("%o  +0.0 at %1m, vulnerability\n", bpos);
	  }
	}
      } 
    }
  }

  /* Modify the distance values for the moves with various bonuses. */
  for (r = 0; r < num_moves; r++) {
    int move = moves[r];
    int adjacent_to_attacker = 0;
    int bonus_given = 0;

    for (k = 0; k < 4; k++) {
      int pos = move + delta[k];
      if (board[pos] == other) {
	adjacent_to_attacker = 1;
	distances[r] -= FP(0.15);
	if (verbose > 0)
	  gprintf("%o%1M -0.15, adjacent to attacker string\n", move);
	if (countlib(pos) <= 2) {
	  distances[r] -= FP(0.2);
	  if (verbose > 0)
	    gprintf("%o%1M -0.2, adjacent to attacker string with at most two liberties\n", move);
	  if ((connect_move || !bonus_given)
	      && (conn1->distances[move] - conn1->deltas[move] <= FP(0.5)
		  || conn1->distances[pos] - conn1->deltas[pos] <= FP(0.5))
	      && (conn2->distances[move] - conn2->deltas[move] <= FP(0.5)
		  || conn2->distances[pos] - conn2->deltas[pos] <= FP(0.5))
	      && conn1->distances[pos] < total_distance
	      && conn2->distances[pos] < total_distance) {
	    bonus_given = 1;
	    distances[r] -= FP(0.7);
	    if (verbose > 0)
	      gprintf("%o%1M -0.7, capture or atari of immediately connecting string\n", move);
	  }
	}
      }
      else if (board[pos] == color) {
	if (countlib(pos) <= 2) {
	  distances[r] -= FP(0.2);
	  if (verbose > 0)
	    gprintf("%o%1M -0.2, adjacent to defender string with at most two liberties\n", move);
	}
	/* The code above (in the 'board[pos] == other' branch) makes
	 * perfect sense for the defender, but has a tendency to
	 * overestimate solid connection defenses when the attacker's
	 * stones happen to be in atari, specially when capturing some
	 * defender stones instead would help just as well, if not better.
	 * The following code compensates in such kind of situations.
	 * See connection:111 and gunnar:53 for example.
	 */
	if (!connect_move && countlib(pos) == 1
	    /* let's avoid ko and snapbacks */
	    && accuratelib(move, other, 2, NULL) > 1) {
	  int adjs[MAXCHAIN];
	  int bonus;
	  bonus = FP(0.1) * chainlinks2(pos, adjs, 2);
	  bonus += FP(0.5) * chainlinks2(pos, adjs, 1);
	  distances[r] -= bonus;
	  if (verbose > 0)
	    gprintf("%o%1M -%f, capture of defender string\n",
		    move, FIXED_TO_FLOAT(bonus));
	}
      }
    }
    if (adjacent_to_attacker
	&& !connect_move
	&& is_edge_vertex(move)) {
      distances[r] -= FP(0.1);
      if (verbose > 0)
	gprintf("%o%1M -0.1, disconnect move on edge\n", move);
    }

    if (ladder_capturable(move, color_to_move)) {
      distances[r] += FP(0.3);
      if (verbose > 0)
	gprintf("%o%1M +0.3, can be captured in a ladder\n", move);
    }

    /* Bonus for moves adjacent to endpoint strings with 3 liberties.
     * Neighbor strings with less than 3 liberties have already
     * generated a bonus above.
     */
    if ((liberty_of_string(move, str1)
	 && countlib(str1) == 3)
	|| (ON_BOARD(str2) && liberty_of_string(move, str2)
	    && countlib(str2) == 3)) {
      distances[r] -= FP(0.1);
      if (verbose > 0)
	gprintf("%o%1M -0.1, liberty of endpoint string with 3 libs\n", move);
    }
  }

  /* Turn the sgf traces back on. */
  sgf_dumptree = save_sgf_dumptree;
  count_variations = save_count_variations;

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
      gprintf("%o%1M %f\n", moves[i], FIXED_TO_FLOAT(distances[i]));
  }

  if (sgf_dumptree) {
    char buf[500];
    char *pos;
    int chars;
    sprintf(buf, "Move order for %sconnect: %n",
	    connect_move ? "" : "dis", &chars);
    pos = buf + chars;
    for (i = 0; i < num_moves; i++) {
      sprintf(pos, "%c%d (%4.2f) %n", J(moves[i]) + 'A' + (J(moves[i]) >= 8),
	      board_size - I(moves[i]), FIXED_TO_FLOAT(distances[i]),
	      &chars);
      pos += chars;
    }
    if (cutoff < HUGE_CONNECTION_DISTANCE) {
      sprintf(pos, "(cutoff %f)%n", FIXED_TO_FLOAT(cutoff), &chars);
      pos += chars;
    }
    sgftreeAddComment(sgf_dumptree, buf);
  }

  if (num_moves == 0)
    return num_moves;

  /* Filter out moves with distance at least 1.5 more than the best
   * move, or with distance higher than the cutoff specified.
   *
   * In order to further reduce the branching factor, a decreasing
   * cutoff is applied between candidates. For instance, in this case
   *   1. d    2. d+0.5   3. d+1.0   4. d+1.5
   * the 4th candidate will be tested, while in following one
   *   1. d    2. d+0.1   3. d+0.2   4. d+1.5
   * it will be discarded.
   */
  if (num_moves <= 1 || !is_ko(moves[0], color_to_move, NULL))
    distance_limit = distances[0] + FP(1.5);
  else
    distance_limit = distances[1] + FP(1.5);

  /* Special case: If the second best move has a distance less than 1,
   * include it if even if the best move has a very low distance.
   */
  if (num_moves > 1
      && distances[1] < FP(1.0)
      && distances[1] > distance_limit)
    distance_limit = distances[1];
  
  for (r = 0; r < num_moves; r++) {
    if (r > 1
	&& distances[r] > distances[r-1]
	&& distances[r] - distances[r-1] > (8 - r) * FP(0.2))
      break;
    if (distances[r] > distance_limit
	|| distances[r] > cutoff)
      break;
  }
  num_moves = r;

  return num_moves;
}

static int
find_string_connection_moves(int str1, int str2, int color_to_move,
		             int moves[MAX_MOVES], int *total_distance)
{
  struct connection_data conn1;
  struct connection_data conn2;
  int max_dist1;
  int max_dist2;
  int num_moves;
  int lib;
  SGFTree *save_sgf_dumptree = sgf_dumptree;
  int save_count_variations = count_variations;

  /* We turn off the sgf traces here to avoid cluttering them up with
   * tactical reading moves.
   */
  sgf_dumptree = NULL;
  count_variations = 0;

  compute_connection_distances(str1, str2, FP(3.051), &conn1, 1);
  compute_connection_distances(str2, str1, FP(3.051), &conn2, 1);

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

  sgf_dumptree = save_sgf_dumptree;
  count_variations = save_count_variations;

  num_moves = find_connection_moves(str1, str2, color_to_move,
      				    &conn1, &conn2, max_dist1, max_dist2,
      			   	    moves, *total_distance,
				    HUGE_CONNECTION_DISTANCE);
  return num_moves;
}


static void
add_to_start_queue(int pos, int dist, struct connection_data *conn)
{
  conn->queue[conn->queue_end++] = pos;
  conn->distances[pos] = dist;
  conn->deltas[pos] = dist;
  conn->coming_from[pos] = NO_MOVE;
  conn->vulnerable1[pos] = NO_MOVE;
  conn->vulnerable2[pos] = NO_MOVE;
}


void
init_connection_data(int color, const signed char goal[BOARDMAX],
    		     int target, int cutoff,
		     struct connection_data *conn, int speculative)
{
  int pos;
  signed char mark[BOARDMAX];

  memset(mark, 0, BOARDMAX);
  VALGRIND_MAKE_WRITABLE(conn, sizeof(conn));
  clear_connection_data(conn);

  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (goal[pos]) {
      if (board[pos] == color) {
	int origin = find_origin(pos);

	if (!mark[origin]) {
	  add_to_start_queue(origin, FP(0.0), conn);
	  mark[origin] = 1;
	}
      }
      else if (board[pos] == EMPTY)
        add_to_start_queue(pos, FP(1.0), conn);
    }
  }

  conn->target = target;
  conn->cutoff_distance = cutoff;
  conn->speculative = speculative;
}

static int
find_break_moves(int str, const signed char goal[BOARDMAX], int color_to_move,
		 int moves[MAX_MOVES], int *total_distance)
{
  struct connection_data conn1;
  struct connection_data conn2;
  int max_dist1 = HUGE_CONNECTION_DISTANCE;
  int max_dist2;
  int num_moves;
  int str2 = NO_MOVE;
  int color = board[str];
  int lib;
  int k;
  SGFTree *save_sgf_dumptree = sgf_dumptree;
  int save_count_variations = count_variations;

  /* We turn off the sgf traces here to avoid cluttering them up with
   * tactical reading moves.
   */
  sgf_dumptree = NULL;
  count_variations = 0;

  compute_connection_distances(str, NO_MOVE, FP(2.501), &conn1, 1);
  for (k = 0; k < conn1.queue_end; k++)
    if (board[conn1.queue[k]] == color) {
      int stones[MAX_BOARD * MAX_BOARD];
      int num_stones = findstones(conn1.queue[k],
	  			  MAX_BOARD * MAX_BOARD, stones);
      int i;
      for (i = 0; i < num_stones; i++) {
	if (goal[stones[i]]) {
	  str2 = find_origin(stones[i]);
	  TRACE("%oUsing %1m as secondary target.\n", str2);
	  mark_string(str2, breakin_shadow, 1);
	  break;
	}
      }
      if (i < num_stones)
	break;
    }

  /* Add all stones in the goal to the queue. */
  init_connection_data(color, goal, str, FP(2.501), &conn2, 1);

  for (k = 0; k < conn2.queue_end; k++) {
    if (max_dist1 > conn1.distances[conn2.queue[k]])
      max_dist1 = conn1.distances[conn2.queue[k]];
  }

  spread_connection_distances(color, &conn2);

  if (findlib(str, 1, &lib) == 1) {
    conn1.distances[lib] = 0;
    conn1.coming_from[lib] = NO_MOVE;
    conn2.distances[lib] = conn2.distances[str];
    conn2.coming_from[lib] = conn1.coming_from[str];
  }

  max_dist2 = conn2.distances[str];
  *total_distance = gg_min(max_dist1, max_dist2);

  /* Turn the sgf traces back on. */
  sgf_dumptree = save_sgf_dumptree;
  count_variations = save_count_variations;

  if (verbose > 0) {
    gprintf("%oVariation %d\n", save_count_variations);
    dump_stack();
    showboard(0);
    print_connection_distances(&conn1);
    print_connection_distances(&conn2);
  }

  {
    int cutoff = HUGE_CONNECTION_DISTANCE;
    if (breakin_depth - stackp <= 5)
      cutoff = FP(1.101) + (breakin_depth - stackp) * FP(0.15);
    num_moves = find_connection_moves(str, str2, color_to_move,
				      &conn1, &conn2, max_dist1, max_dist2,
				      moves, *total_distance, cutoff);
  }

  if (color_to_move != board[str]) {
    int move;
    if (num_moves < MAX_MOVES
	&& ON_BOARD(str2)
	&& ladder_capture(str2, &move)) {
      moves[num_moves++] = move;
    }
  }

  for (k = 0; k < num_moves; k++)
    breakin_shadow[moves[k]] = 1;

  return num_moves;
}


/* Can (str) connect to goal[] if the other color moves first? */
static int
recursive_break(int str, const signed char goal[BOARDMAX], int *move,
    		  int has_passed,
		Hash_data *goal_hash)
{
  int color = board[str];
  int moves[MAX_MOVES];
  int num_moves;
  int distance = FP(0.0);
  int k;
  int xpos;
  int savemove = NO_MOVE;
  int savecode = 0;
  int tried_moves = 0;
  int retval;
  
  SETUP_TRACE_INFO("recursive_break", str);

  if (move)
    *move = NO_MOVE;

  nodes_connect++;
  global_connection_node_counter++;
  
  if (board[str] == EMPTY) {
    SGFTRACE(PASS_MOVE, 0, "one string already captured");
    return 0;
  }
  
  if (nodes_connect > breakin_node_limit) {
    SGFTRACE(PASS_MOVE, 0, "connection node limit reached");
    return 0;
  }
  
  if (stackp > breakin_depth) {
    SGFTRACE(PASS_MOVE, 0, "connection depth limit reached");
    return 0;
  }

  str = find_origin(str);
  if (stackp <= depth && !has_passed
      && tt_get(&ttable, BREAK_IN, str, NO_MOVE, depth - stackp, goal_hash,
		&retval, NULL, &xpos) == 2) {
    /* FIXME: Use move for move ordering if tt_get() returned 1 */
    TRACE_CACHED_RESULT(retval, xpos);
    SGFTRACE(xpos, retval, "cached");
    if (move)
      *move = xpos;
    return retval;
  }
  
#if 0
  if (trivial_connection(str1, str2, &xpos) == WIN) {
    SGFTRACE2(xpos, WIN, "trivial connection");
    READ_RETURN_HASH(BREAK_IN, str, depth - stackp, goal_hash,
		     move, xpos, WIN);
  }
#endif
  
  num_moves = find_break_moves(str, goal, color, moves, &distance);
  
  for (k = 0; k < num_moves; k++) {
    int ko_move;
    xpos = moves[k];
    
    if (komaster_trymove(xpos, color, "recursive_break", str,
			 &ko_move, stackp <= ko_depth && savecode == 0)) {
      tried_moves++;
      if (!ko_move) {
	int acode = recursive_block(str, goal, NULL, has_passed, goal_hash);
	popgo();
	if (acode == 0) {
	  SGFTRACE(xpos, WIN, "break effective");
	  READ_RETURN_HASH(BREAK_IN, str, depth - stackp, goal_hash,
			   move, xpos, WIN);
	}
	/* if the move works with ko we save it, then look for something
	 * better.
	 */
	UPDATE_SAVED_KO_RESULT(savecode, savemove, acode, xpos);
      }
      else {
	if (recursive_block(str, goal, NULL, has_passed, goal_hash) != WIN) {
	  savemove = xpos;
	  savecode = KO_B;
	}
	popgo();
      }
    }
  }

  /* Because of a couple differences between the break-in and the
   * connection reading code, we can't afford to be as optimistic
   * as in recursive_connect2() here. See nando:32
   */
  if (tried_moves == 0 && distance < FP(0.89)) {
    SGFTRACE(NO_MOVE, WIN, "no move, probably connected");
    READ_RETURN_HASH(BREAK_IN, str, depth - stackp, goal_hash,
		     move, NO_MOVE, WIN);
  }
  
  if (savecode != 0) {
    SGFTRACE(savemove, savecode, "saved move");
    READ_RETURN_HASH(BREAK_IN, str, depth - stackp, goal_hash,
		     move, savemove, savecode);
  }

  SGFTRACE(0, 0, NULL);
  READ_RETURN_HASH(BREAK_IN, str, depth - stackp, goal_hash, move, NO_MOVE, 0);
}


/* Can (str) connect to goal[] if the other color moves first? */
static int
recursive_block(int str, const signed char goal[BOARDMAX], int *move,
		int has_passed,	Hash_data *goal_hash)
{
  int color = board[str];
  int other = OTHER_COLOR(color);
  int moves[MAX_MOVES];
  int num_moves;
  int distance = FP(0.0);
  int k;
  int xpos;
  int savemove = NO_MOVE;
  int savecode = 0;
  int tried_moves = 0;
  int retval;
  SETUP_TRACE_INFO("recursive_block", str);
  
  nodes_connect++;
  global_connection_node_counter++;

  if (move)
    *move = NO_MOVE;
  
  if (board[str] == EMPTY) {
    SGFTRACE(PASS_MOVE, WIN, "string already captured");
    return WIN;
  }

#if 0
  if (same_string(str1, str2)) {
    SGFTRACE(PASS_MOVE, 0, "already connected");
    return 0;
  }
#endif
  
  if (nodes_connect > breakin_node_limit) {
    SGFTRACE(PASS_MOVE, WIN, "connection node limit reached");
    return WIN;
  }
  
  if (stackp > breakin_depth) {
    SGFTRACE(PASS_MOVE, WIN, "connection depth limit reached");
    return WIN;
  }
  
  str = find_origin(str);
  if (stackp <= depth
      && tt_get(&ttable, BLOCK_OFF, str, NO_MOVE,
		depth - stackp, goal_hash, &retval, NULL, &xpos) == 2) {
    TRACE_CACHED_RESULT(retval, xpos);
    SGFTRACE(xpos, retval, "cached");
    if (move)
      *move = xpos;
    return retval;
  }

  if (ladder_capture(str, &xpos) == WIN) {
    SGFTRACE(xpos, WIN, "string capturable");
    READ_RETURN_HASH(BLOCK_OFF, str, depth - stackp, goal_hash,
		     move, xpos, WIN);
  }
  
  num_moves = find_break_moves(str, goal, other, moves, &distance);
  
  for (k = 0; k < num_moves; k++) {
    int ko_move;
    xpos = moves[k];
    
    if (komaster_trymove(xpos, other, "recursive_block", str,
			 &ko_move, stackp <= ko_depth && savecode == 0)) {
      tried_moves++;
      if (!ko_move) {
	int dcode = recursive_break(str, goal, NULL, has_passed, goal_hash);
	popgo();
	if (dcode == 0) {
	  SGFTRACE(xpos, WIN, "block effective");
	  READ_RETURN_HASH(BLOCK_OFF, str, depth - stackp, goal_hash,
			   move, xpos, WIN);
	}
	/* if the move works with ko we save it, then look for something
	 * better.
	 */
	UPDATE_SAVED_KO_RESULT(savecode, savemove, dcode, xpos);
      }
      else {
	if (recursive_break(str, goal, NULL,
			    has_passed, goal_hash) != WIN) {
	  savemove = xpos;
	  savecode = KO_B;
	}
	popgo();
      }
    }
  }

  if (tried_moves == 0
      && distance >= FP(1.0)
      && (has_passed
	  || !recursive_break(str, goal, NULL, 1,
	                      goal_hash))) {
    SGFTRACE(NO_MOVE, WIN, "no move, probably disconnected");
    READ_RETURN_HASH(BLOCK_OFF, str, depth - stackp, goal_hash,
		     move, NO_MOVE, WIN);
  }
  
  if (savecode != 0) {
    SGFTRACE(savemove, savecode, "saved move");
    READ_RETURN_HASH(BLOCK_OFF, str, depth - stackp, goal_hash,
		     move, savemove, savecode);
  }

  SGFTRACE(0, 0, NULL);
  READ_RETURN_HASH(BLOCK_OFF, str, depth - stackp, goal_hash,
		   move, NO_MOVE, 0);
}



/* Externally callable frontend to recursive_break.
 * Returns WIN if (str) can connect to the area goal[] (which may or may
 * not contain stones), if he gets the first move.
 */
int
break_in(int str, const signed char goal[BOARDMAX], int *move)
{
  int dummy_move;
  int save_verbose;
  int result;
  int reading_nodes_when_called = get_reading_node_counter();
  double start = 0;
  int tactical_nodes;
  Hash_data goal_hash = goal_to_hashvalue(goal);

  if (move == NULL)
    move = &dummy_move;
  
  nodes_connect = 0;
  *move = PASS_MOVE;
  
  if (board[str] == EMPTY)
    return 0;
  str = find_origin(str);

  if (search_persistent_breakin_cache(BREAK_IN, str, &goal_hash,
				      breakin_node_limit, &result, move)) {
    if (debug & DEBUG_BREAKIN) {
      gprintf("Break-in from %1m to:\n", str);
      goaldump(goal);
      gprintf("Result cached: %s %1m\n", result_to_string(result), *move);
    }
    return result;
  }

  save_verbose = verbose;
  if (verbose > 0)
    verbose--;
  start = gg_cputime();
  memcpy(breakin_shadow, goal, sizeof(breakin_shadow));
  result = recursive_break(str, goal, move, 0, &goal_hash);
  verbose = save_verbose;
  tactical_nodes = get_reading_node_counter() - reading_nodes_when_called;
  if (debug & DEBUG_BREAKIN) {
    gprintf("%obreak_in    %1M, result %s %1M (%d, %d nodes, %f seconds)\n",
	    str, result_to_string(result), *move,
	    nodes_connect, tactical_nodes, gg_cputime() - start);
    goaldump(goal);
    dump_stack();
  }
  if (0) {
    gprintf("%obreak_in %1m %d %1m ", str, result, *move);
    dump_stack();
    goaldump(goal);
  }
  store_persistent_breakin_cache(BREAK_IN, str, &goal_hash, result, *move,
      				 tactical_nodes, breakin_node_limit,
				 breakin_shadow);

  return result;
}


/* Externably callable frontend to recursive_block_off.
 * Returns WIN if (str) cannot connect to the area goal[] (which may or may
 * not contain stones), if the other color moves first.
 */
int
block_off(int str, const signed char goal[BOARDMAX], int *move)
{
  int dummy_move;
  int result;
  int save_verbose;
  int reading_nodes_when_called = get_reading_node_counter();
  double start = 0;
  int tactical_nodes;
  Hash_data goal_hash = goal_to_hashvalue(goal);
  
  if (move == NULL)
    move = &dummy_move;
  
  nodes_connect = 0;
  *move = PASS_MOVE;
  
  str = find_origin(str);
  if (search_persistent_breakin_cache(BLOCK_OFF, str, &goal_hash,
				      breakin_node_limit, &result, move)) {
    if (debug & DEBUG_BREAKIN) {
      gprintf("Blocking off %1m from:\n", str);
      goaldump(goal);
      gprintf("Result cached: %s %1m\n", result_to_string(result), *move);
    }
    return result;
  }

  save_verbose = verbose;
  if (verbose > 0)
    verbose--;
  start = gg_cputime();
  memcpy(breakin_shadow, goal, sizeof(breakin_shadow));
  result = recursive_block(str, goal, move, 0, &goal_hash);
  verbose = save_verbose;
  tactical_nodes = get_reading_node_counter() - reading_nodes_when_called;

  if (debug & DEBUG_BREAKIN) {
    gprintf("%oblock_off %1m, result %s %1m (%d, %d nodes, %f seconds)\n",
	    str, result_to_string(result), *move,
	    nodes_connect, tactical_nodes, gg_cputime() - start);
    goaldump(goal);
    dump_stack();
  }
  if (0) {
    gprintf("%oblock_off %1m %d %1m ", str, result, *move);
    goaldump(goal);
    dump_stack();
  }
  store_persistent_breakin_cache(BLOCK_OFF, str, &goal_hash, result, *move,
				 tactical_nodes, breakin_node_limit,
				 breakin_shadow);

  return result;
}


/* Store a possibly expensive decision for later evaluation. The
 * data getting stored should be self-explanatory.
 * The job of the helper function is to
 * - decide whether the spreading step will be allowed (typically
 *   depending on a latter)
 * - add the relevant positions to the connection queue in case the test
 *   was successful.
 *
 * Elements in the heap are kept sorted according to smallest distance.
 */
static void
push_connection_heap_entry(struct connection_data *conn, int distance,
			   int coming_from, int target,
			   connection_helper_fn_ptr helper)
{
  int k;
  int parent;
  struct heap_entry *new_entry = &conn->heap_data[conn->heap_data_size];

  gg_assert(conn->heap_data_size < 4 * BOARDMAX);
  gg_assert(conn->heap_size < BOARDMAX);

  /* Create new heap entry. */
  new_entry->distance	 = distance;
  new_entry->coming_from = coming_from;
  new_entry->target	 = target;
  new_entry->helper	 = helper;

  /* And insert it into the heap. */
  conn->heap_data_size++;

  for (k = conn->heap_size++; k > 0; k = parent) {
    parent = (k - 1) / 2;
    if (conn->heap[parent]->distance <= distance)
      break;

    conn->heap[k] = conn->heap[parent];
  }

  conn->heap[k] = new_entry;
}


/* Delete the first entry from the heap. */
static void
pop_connection_heap_entry(struct connection_data *conn)
{
  int k;
  int child;

  conn->heap_size--;
  for (k = 0; 2 * k + 1 < conn->heap_size; k = child) {
    child = 2 * k + 1;
    if (conn->heap[child]->distance > conn->heap[child + 1]->distance)
      child++;
    if (conn->heap[child]->distance >= conn->heap[conn->heap_size]->distance)
      break;

    conn->heap[k] = conn->heap[child];
  }

  conn->heap[k] = conn->heap[conn->heap_size];
}


#define ENQUEUE(conn, from, pos, dist, delta, v1, v2)			\
  do {									\
    if (dist < conn->distances[pos]) {					\
      if (conn->distances[pos] == HUGE_CONNECTION_DISTANCE)		\
	conn->queue[conn->queue_end++] = pos;				\
      conn->distances[pos]   = dist;					\
      conn->deltas[pos]      = delta;					\
      conn->coming_from[pos] = from;					\
      conn->vulnerable1[pos] = v1;					\
      conn->vulnerable2[pos] = v2;					\
    }									\
  } while(0)

#define ENQUEUE_STONE(conn, from, pos, dist, delta, v1, v2)		\
  do {									\
    int origin = find_origin(pos);					\
    if (dist < conn->distances[origin]) {				\
      if (conn->distances[origin] == HUGE_CONNECTION_DISTANCE)		\
	conn->queue[conn->queue_end++] = origin;			\
      conn->distances[origin]	= dist;					\
      conn->deltas[origin]	= delta;				\
      conn->coming_from[origin] = from;					\
      conn->vulnerable1[origin] = v1;					\
      conn->vulnerable2[origin] = v2;					\
      if (origin == conn->target && dist < conn->cutoff_distance)	\
	conn->cutoff_distance = dist - FP(0.0001);			\
    }									\
  } while(0)


static void
case_6_7_helper(struct connection_data *conn, int color)
{
  struct heap_entry *data = conn->heap[0];
  int pos = data->coming_from;
  int apos = data->target;
  int other = OTHER_COLOR(color);

  if (ladder_capturable(apos, other))
    ENQUEUE(conn, pos, apos, data->distance, FP(0.6), apos, NO_MOVE);
  else {
    int this_delta 
      = FP(0.85) + FP(0.05) * gg_min(approxlib(apos, other, 5, NULL), 5);
    ENQUEUE(conn, pos, apos, data->distance + this_delta - FP(0.6), this_delta,
	    NO_MOVE, NO_MOVE);
  }
}


static void
case_9_10_helper(struct connection_data *conn, int color)
{
  struct heap_entry *data = conn->heap[0];
  int pos = data->coming_from;
  int apos = data->target;

  UNUSED(color);

  if (no_escape_from_ladder(apos))
    ENQUEUE_STONE(conn, pos, apos, data->distance, FP(0.3), NO_MOVE, NO_MOVE);
  else {
    if (conn->speculative) {
      ENQUEUE_STONE(conn, pos, apos, data->distance + FP(0.7), FP(1.0),
		    NO_MOVE, NO_MOVE);
    }
    else {
      ENQUEUE_STONE(conn, pos, apos, data->distance + FP(0.8), FP(1.1),
		    NO_MOVE, NO_MOVE);
    }
  }
}


static void
case_16_17_18_helper(struct connection_data *conn, int color)
{
  struct heap_entry *data = conn->heap[0];
  int pos = data->coming_from;
  int bpos = data->target;
  int apos = SOUTH(gg_min(pos, bpos));
  int gpos = NORTH(gg_max(pos, bpos));
  int other = OTHER_COLOR(color);

  if (board[apos] == EMPTY
      && does_secure_through_ladder(color, bpos, apos))
    ENQUEUE(conn, pos, bpos, data->distance, FP(1.0), apos, NO_MOVE);
  else if (board[gpos] == EMPTY
	   && does_secure_through_ladder(color, bpos, gpos))
    ENQUEUE(conn, pos, bpos, data->distance, FP(1.0), gpos, NO_MOVE);
  else if (conn->distances[bpos] > data->distance + FP(0.3)) {
    if (board[apos] == EMPTY
	&& board[gpos] == other
	&& countlib(gpos) <= 3)
      ENQUEUE(conn, pos, bpos, data->distance + FP(0.3), FP(1.0),
	      apos, NO_MOVE);
    else if (board[gpos] == EMPTY
	     && board[apos] == other
	     && countlib(apos) <= 3)
      ENQUEUE(conn, pos, bpos, data->distance + FP(0.3), FP(1.0),
	      gpos, NO_MOVE);
    else
      ENQUEUE(conn, pos, bpos, data->distance + FP(0.6), FP(0.9),
	      NO_MOVE, NO_MOVE);
  }
}


/* Do the real work of computing connection distances.
 * This is a rough approximation of the number of moves required to secure
 * a connection. We also compute delta values which are intended to tell how
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
 *
 * (color) is the color for which we are computing connection distances,
 * (target) the position we want to reach (can be set to NO_MOVE),
 * (*conn) has to have the queue initialized with the positions
 * from which we want to know the distances,
 * (cutoff_distance) is the highest distance before we give up,
 * (speculative) controls some special cases in the propagation rules
 * below.
 *
 * As an optimization, new points are either added directly via the ENQUEUE
 * macro if the necessary test is an immediate (usually purely geometric)
 * check, or if the decision is more expensive (usually depending on a
 * ladder), it gets postponed and stored via push_connection_heap_entry()
 * for later evaluation.
 */

void
spread_connection_distances(int color, struct connection_data *conn)
{
  int other = OTHER_COLOR(color);
  int stones[MAX_BOARD * MAX_BOARD];
  int num_stones = 0;
  int stone = 0;

  /* Loop until we reach the end of the queue. */
  while (conn->queue_start < conn->queue_end || conn->heap_size > 0) {
    int k;
    int pos;
    int distance;

    /* Delete heap entries for positions that have already been reached
     * with smaller distance.
     */
    while (conn->heap_size > 0
	   && conn->heap[0]->distance >= conn->distances[conn->heap[0]->target])
      pop_connection_heap_entry(conn);

    if (stone == num_stones) {
      int best_index = -1;
      int smallest_dist = HUGE_CONNECTION_DISTANCE;

      if (conn->queue_start == conn->queue_end) {
	if (conn->heap_size > 0) {
	  conn->heap[0]->helper(conn, color);
	  pop_connection_heap_entry(conn);
	}

	continue;
      }

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
	int temp = conn->queue[conn->queue_start];
	conn->queue[conn->queue_start] = conn->queue[best_index];
	conn->queue[best_index] = temp;
      }

      /* If the first element in heap has smaller distance than the
       * smallest we have found so far, call the relevant helper function
       * now, and delete the heap entry.
       */
      if (conn->heap_size > 0 && conn->heap[0]->distance < smallest_dist) {
	conn->heap[0]->helper(conn, color);
	pop_connection_heap_entry(conn);
	continue;
      }

      /* Now we are ready to pick the first element in the queue and
       * process it.
       */
      pos = conn->queue[conn->queue_start++];
      if (board[pos] != EMPTY) {
	num_stones = findstones(pos, MAX_BOARD * MAX_BOARD, stones);
	pos = stones[0];
	stone = 1;
      }
    }
    else {
      pos = stones[stone++];
      conn->distances[pos]   = conn->distances[stones[0]];
      conn->deltas[pos]      = conn->deltas[stones[0]];
      conn->coming_from[pos] = conn->coming_from[stones[0]];
      conn->vulnerable1[pos] = conn->vulnerable1[stones[0]];
      conn->vulnerable2[pos] = conn->vulnerable2[stones[0]];
    }

    /* No further propagation if the distance is too large. */
    distance = conn->distances[pos];
    if (distance > conn->cutoff_distance)
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
	if (board[apos] == EMPTY && is_suicide(apos, other))
	  ENQUEUE(conn, pos, apos, distance, FP(0.0), apos, NO_MOVE);
	
	/* Case 2. "a" is empty and would be self atari for the opponent. */
	if (board[apos] == EMPTY
	    && conn->distances[apos] > distance + FP(0.1)
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
			       "compute_connection_distances", pos)) {
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
	    ENQUEUE(conn, pos, apos, distance + FP(0.1), FP(0.1),
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
	  ENQUEUE(conn, pos, bpos, distance + FP(0.1), FP(0.1),
		  NO_MOVE, NO_MOVE);
	  ENQUEUE(conn, pos, gpos, distance + FP(0.1), FP(0.1),
		  NO_MOVE, NO_MOVE);
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
	    && conn->distances[bpos] > distance + FP(0.1)) {
#if 0
	  ENQUEUE(conn, pos, apos, distance + FP(0.2), FP(0.2),
		  NO_MOVE, NO_MOVE);
	  ENQUEUE(conn, pos, gpos, distance + FP(0.2), FP(0.2),
		  NO_MOVE, NO_MOVE);
#endif
	  ENQUEUE_STONE(conn, pos, bpos, distance + FP(0.1), FP(0.1),
			apos, gpos);
	}

	/* Case 5. Almost bamboo joint.
	 * 
	 */
	if (board[gpos] == EMPTY
	    && board[epos] == color
            && conn->distances[epos] > distance + FP(0.2)
	    && approxlib(gpos, other, 3, NULL) <= 2) {
	  if (board[bpos] == EMPTY
	      && approxlib(bpos, color, 3, NULL) >= 3
	      && (board[apos] == color
		  || (board[apos] == EMPTY
		      && countlib(pos) > 2
		      && !common_vulnerabilities(conn->vulnerable1[pos],
						 conn->vulnerable2[pos],
						 apos, gpos, color)
		      && approxlib(apos, other, 3, NULL) <= 2))
	      && (board[fpos] == color
		  || (board[fpos] == EMPTY
		      && countlib(epos) > 2
		      && !common_vulnerabilities(conn->vulnerable1[pos],
						 conn->vulnerable2[pos],
						 fpos, gpos, color)
		      && approxlib(fpos, other, 3, NULL) <= 2))) {
	    if (board[apos] == EMPTY && board[fpos] == EMPTY) {
	      ENQUEUE_STONE(conn, pos, epos, distance + FP(0.2), FP(0.2),
			    apos, fpos);
	    }
	    else if (board[apos] == EMPTY && board[fpos] != EMPTY) {
	      ENQUEUE_STONE(conn, pos, epos, distance + FP(0.2), FP(0.2),
			    apos, NO_MOVE);
	    }
	    else if (board[apos] != EMPTY && board[fpos] == EMPTY) {
	      ENQUEUE_STONE(conn, pos, epos, distance + FP(0.2), FP(0.2),
			    fpos, NO_MOVE);
	    }
	    else if (board[apos] != EMPTY && board[fpos] != EMPTY) {
	      ENQUEUE_STONE(conn, pos, epos, distance + FP(0.2), FP(0.2),
			    NO_MOVE, NO_MOVE);
	    }
	  }

	  if (board[ipos] == EMPTY
	      && approxlib(ipos, color, 3, NULL) >= 3
	      && (board[hpos] == color
		  || (board[hpos] == EMPTY
		      && countlib(pos) > 2
		      && !common_vulnerabilities(conn->vulnerable1[pos],
						 conn->vulnerable2[pos],
						 hpos, gpos, color)
		      && approxlib(hpos, other, 3, NULL) <= 2))
	      && (board[jpos] == color
		  || (board[jpos] == EMPTY
		      && countlib(epos) > 2
		      && !common_vulnerabilities(conn->vulnerable1[pos],
						 conn->vulnerable2[pos],
						 jpos, gpos, color)
		      && approxlib(jpos, other, 3, NULL) <= 2))) {
	    if (board[hpos] == EMPTY && board[jpos] == EMPTY) {
	      ENQUEUE_STONE(conn, pos, epos, distance + FP(0.2), FP(0.2),
			    hpos, jpos);
	    }
	    else if (board[hpos] == EMPTY && board[jpos] != EMPTY) {
	      ENQUEUE_STONE(conn, pos, epos, distance + FP(0.2), FP(0.2),
			    hpos, NO_MOVE);
	    }
	    else if (board[hpos] != EMPTY && board[jpos] == EMPTY) {
	      ENQUEUE_STONE(conn, pos, epos, distance + FP(0.2), FP(0.2),
			    jpos, NO_MOVE);
	    }
	    else if (board[hpos] != EMPTY && board[jpos] != EMPTY) {
	      ENQUEUE_STONE(conn, pos, epos, distance + FP(0.2), FP(0.2),
			    NO_MOVE, NO_MOVE);
	    }
	  }
	}

	/* Case 6. "a" is empty and an opponent move can be captured
	 * in a ladder.
	 *
	 * Case 7. "a" is empty.
	 */
	if (board[apos] == EMPTY && conn->distances[apos] > distance + FP(0.6)) {
	  push_connection_heap_entry(conn, distance + FP(0.6), pos, apos,
				     case_6_7_helper);
	}

	/* Case 8. Adjacent opponent stone at "a" which can't avoid atari.
	 */
	if (board[apos] == other
	    && conn->distances[apos] > distance + FP(0.1)
	    && no_escape_from_atari(apos)) {
	  ENQUEUE_STONE(conn, pos, apos, distance + FP(0.1), FP(0.1),
			NO_MOVE, NO_MOVE);
	}

	/* Case 9. Adjacent opponent stone at "a" which can't avoid
	 * ladder capture.
	 *
	 * Case 10. "a" is occupied by opponent.
	 */
	if (board[apos] == other && conn->distances[apos] > distance + FP(0.3)) {
	  push_connection_heap_entry(conn, distance + FP(0.3), pos, apos,
				     case_9_10_helper);
	}

	/* Case 11. Diagonal connection to empty vertex "b" through
	 * empty vertex "a" or "g", which makes "a" or "g" self-atari
	 * for opponent.
	 */
	if (board[bpos] == EMPTY
	    && board[apos] == EMPTY
	    && conn->distances[bpos] > distance + FP(1.1)
	    && does_secure(color, bpos, apos)) {
	  ENQUEUE(conn, pos, bpos, distance + FP(1.1), FP(1.0), apos, NO_MOVE);
	}

	if (board[bpos] == EMPTY
	    && board[gpos] == EMPTY
	    && conn->distances[bpos] > distance + FP(1.1)
	    && does_secure(color, bpos, gpos)) {
	  ENQUEUE(conn, pos, bpos, distance + FP(1.1), FP(1.0), gpos, NO_MOVE);
	}

	/* Case 12. One-space jump to empty vertex "e" through empty
	 * vertex "g", which makes "g" self-atari for opponent.
	 */
	if (board[gpos] == EMPTY
	    && board[epos] == EMPTY
	    && conn->distances[epos] > distance + FP(1.1)
	    && does_secure(color, epos, gpos)) {
	  ENQUEUE(conn, pos, epos, distance + FP(1.1), FP(1.0), gpos, NO_MOVE);
	}

	/* Case 13. One-space jump to empty vertex "e" through empty
	 * vertex "g", making a bamboo joint.
	 */
	if (board[gpos] == EMPTY
	    && board[epos] == EMPTY
	    && conn->distances[epos] > distance + FP(1.1)
	    && ((board[apos] == color && board[fpos] == color
		 && board[bpos] == EMPTY)
		|| (board[hpos] == color && board[jpos] == color
		    && board[ipos] == EMPTY))) {
	  ENQUEUE(conn, pos, epos, distance + FP(1.1), FP(1.0), gpos, NO_MOVE);
	}

	/* Case 14. Diagonal connection to empty vertex "b" through
	 * empty vertices "a" and "g".
	 */
	if (board[bpos] == EMPTY
	    && board[apos] == EMPTY && board[gpos] == EMPTY
            && conn->distances[bpos] > distance + FP(1.3)) {
	  ENQUEUE(conn, pos, bpos, distance + FP(1.3), FP(1.0), apos, gpos);
	}

	/* Case 15. Keima to "f" or "j" on edge. and one space jump on
	 * first or second line.
	 */
	if (board[apos] == EMPTY
	    && board[bpos] == EMPTY
	    && board[gpos] == EMPTY
	    && board[epos] == EMPTY
	    && board[fpos] == EMPTY
	    && (conn->distances[fpos] > distance + FP(1.3)
		|| conn->distances[epos] > distance + FP(1.3))
	    && countlib(pos) >= 3
	    && (!ON_BOARD(cpos) || !ON_BOARD(hpos))) {
	  ENQUEUE(conn, pos, fpos, distance + FP(1.3), FP(1.0),
		  NO_MOVE, NO_MOVE);
	  ENQUEUE(conn, pos, epos, distance + FP(1.3), FP(1.0),
		  NO_MOVE, NO_MOVE);
	}

	if (board[hpos] == EMPTY
	    && board[ipos] == EMPTY
	    && board[gpos] == EMPTY
	    && board[epos] == EMPTY
	    && board[jpos] == EMPTY
	    && (conn->distances[jpos] > distance + FP(1.3)
		|| conn->distances[epos] > distance + FP(1.3))
	    && countlib(pos) >= 3
	    && (!ON_BOARD(apos) || !ON_BOARD(kpos))) {
	  ENQUEUE(conn, pos, jpos, distance + FP(1.3), FP(1.0),
		  NO_MOVE, NO_MOVE);
	  ENQUEUE(conn, pos, epos, distance + FP(1.3), FP(1.0),
		  NO_MOVE, NO_MOVE);
	}

	/* Case 16. Diagonal connection to empty vertex "b" through
	 * empty vertex "a" or "g", which allows opponent move at "a"
	 * or "g" to be captured in a ladder.
	 *
	 * Case 17. Diagonal connection to empty vertex "b" through
	 * one empty and one opponent vertex "a" and "g", where
	 * the opponent stone is short of liberties.
	 *
	 * Case 18. Diagonal connection to empty vertex "b" through
	 * empty vertex "a" or "g", with no particular properties.
	 */
	if (board[bpos] == EMPTY
	    && (board[apos] == EMPTY || board[gpos] == EMPTY)
	    && conn->distances[bpos] > distance + FP(1.2)) {
	  push_connection_heap_entry(conn, distance + FP(1.2), pos, bpos,
				     case_16_17_18_helper);
	}

	/* Case 19. Clamp at "e" of single stone at "g". */
	if (board[gpos] == other
	    && board[epos] == EMPTY
	    && conn->distances[epos] > distance + FP(2.0)
	    && countstones(gpos) == 1) {
	  ENQUEUE(conn, pos, epos, distance + FP(2.0), FP(1.0),
		  NO_MOVE, NO_MOVE);
	}

	/* Case 20. Diagonal connection to empty vertex "b" through
	 * opponent stones "a" or "g" with few liberties.
	 */
	if (board[bpos] == EMPTY
	    && board[apos] == other
	    && board[gpos] == other
	    && conn->distances[bpos] > distance + FP(2.0)
	    && (countlib(apos) + countlib(gpos) <= 6)) {
	  ENQUEUE(conn, pos, bpos, distance + FP(2.0), FP(1.0),
		  NO_MOVE, NO_MOVE);
	}

	/* Case 21. Diagonal connection to own stone "b" through
	 * opponent stones "a" or "g" with few liberties.
	 */
	if (board[bpos] == color
	    && board[apos] == other
	    && board[gpos] == other
	    && conn->distances[bpos] > distance + FP(2.0)
	    && (countlib(apos) + countlib(gpos) <= 5)) {
	  ENQUEUE_STONE(conn, pos, bpos, distance + FP(2.0), FP(1.0),
			NO_MOVE, NO_MOVE);
	}
      }
    }
    else if (board[pos] == EMPTY
	     || (board[pos] == other
		 && countlib(pos) <= 2
		 && no_escape_from_ladder(pos))) {
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
	  ENQUEUE_STONE(conn, pos, apos, distance, FP(0.0),
			conn->vulnerable1[pos], conn->vulnerable2[pos]);
	}
	else if (board[apos] == EMPTY) {
	  int this_delta
	    = FP(0.8) + FP(0.05) * gg_min(approxlib(apos, other, 6, NULL), 6);
	  ENQUEUE(conn, pos, apos, distance + this_delta, this_delta,
		  NO_MOVE, NO_MOVE);
	}
	else if (board[apos] == other) {
	  ENQUEUE_STONE(conn, pos, apos, distance + FP(1.0), FP(1.0),
			NO_MOVE, NO_MOVE);
	}

	/* Case 1. Diagonal connection to empty vertex "b" through
	 * empty vertices "a" and "g".
	 */
	if (board[bpos] == EMPTY
	    && board[apos] == EMPTY
	    && board[gpos] == EMPTY
            && conn->distances[bpos] > distance + FP(1.5)) {
	  ENQUEUE(conn, pos, bpos, distance + FP(1.5), FP(1.0),
		  NO_MOVE, NO_MOVE);
	}
	
	/* Case 2. Diagonal connection to friendly stone at "b" through
	 * empty vertices "a" and "g".
	 */
	if (board[bpos] == color
	    && board[apos] == EMPTY
	    && board[gpos] == EMPTY
	    && conn->distances[bpos] > distance + FP(1.3)) {
	  ENQUEUE_STONE(conn, pos, bpos, distance + FP(1.3), FP(1.0),
			NO_MOVE, NO_MOVE);
	}
      }
    }
  }
}


void
sort_connection_queue_tail(struct connection_data *conn)
{
  int k;

  for (k = conn->queue_start; k < conn->queue_end - 1; k++) {
    int i;
    int best_index = k;
    int smallest_dist = conn->distances[conn->queue[k]];

    for (i = k + 1; i < conn->queue_end; i++) {
      if (conn->distances[conn->queue[i]] < smallest_dist) {
	best_index = i;
	smallest_dist = conn->distances[conn->queue[i]];
      }
    }

    if (best_index != k) {
      int temp = conn->queue[k];
      conn->queue[k] = conn->queue[best_index];
      conn->queue[best_index] = temp;
    }
  }
}


/* Replace string origins in a connection queue with complete sets of
 * corresponding string stones.
 */
void
expand_connection_queue(struct connection_data *conn)
{
  int k;
  int full_queue[BOARDMAX];
  int full_queue_position = 0;
  int full_queue_start = 0;

  for (k = 0; k < conn->queue_end; k++) {
    if (k == conn->queue_start)
      full_queue_start = full_queue_position;

    if (board[conn->queue[k]] == EMPTY)
      full_queue[full_queue_position++] = conn->queue[k];
    else {
      full_queue_position += findstones(conn->queue[k],
					MAX_BOARD * MAX_BOARD,
					full_queue + full_queue_position);
    }
  }

  conn->queue_start = full_queue_start;
  conn->queue_end   = full_queue_position;
  memcpy(conn->queue, full_queue, conn->queue_end * sizeof(int));
}


/* Initialize distance and delta values so that the former are
 * everywhere huge and the latter everywhere zero.
 */
static void
clear_connection_data(struct connection_data *conn)
{
  int pos;

  conn->queue_start = 0;
  conn->queue_end = 0;
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    conn->distances[pos] = HUGE_CONNECTION_DISTANCE;
    conn->deltas[pos] = FP(0.0);
    conn->coming_from[pos] = NO_MOVE;
    conn->vulnerable1[pos] = NO_MOVE;
    conn->vulnerable2[pos] = NO_MOVE;
  }

  conn->heap_data_size = 0;
  conn->heap_size = 0;
}


/* Compute the connection distances from string (str) to nearby
 * vertices, until we reach target or the distance gets too high.
 */
void
compute_connection_distances(int str, int target, int cutoff,
			     struct connection_data *conn,
			     int speculative)
{
  int color = board[str];
  
  clear_connection_data(conn);

  /* Add the origin of the initial string to the queue. */
  add_to_start_queue(find_origin(str), FP(0.0), conn);

  conn->target = target;
  conn->cutoff_distance = cutoff;
  conn->speculative = speculative;

  spread_connection_distances(color, conn);
}


/* Print the connection distances in a struct connection_data. */
void
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
	fprintf(stderr, "%3.1f ", FIXED_TO_FLOAT(conn->distances[pos]));
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


/* True if a move by color makes an opponent move at pos a self atari
 * or possible to capture in a ladder.
 */
static int
does_secure_through_ladder(int color, int move, int pos)
{
  int result = 0;
  
  if (trymove(move, color, NULL, NO_MOVE)) {
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
  
  if (trymove(pos, color, NULL, NO_MOVE)) {
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
#if 1
  int lib;
#endif
  
  if (!is_self_atari(pos, color_to_move))
    return 1;

  if (is_ko(pos, color_to_move, NULL))
    return 1;

#if 1
  /* FIXME: At some time I added this exceptional case but I can no
   * longer see how it would be useful. It might still be, however, so
   * I leave the code in for a while. /gf
   *
   * Code reactivated, see nando:31. /nn
   *
   * Added requirement that no additional stones are sacrificed in the
   * self atari. /gf
   *
   * FIXME: Add a function in board.c to check how big the string
   *        becomes when playing a move and use for the isolated stone
   *        test below.
   */
  if (approxlib(pos, color_to_move, 1, &lib) >= 1
      && approxlib(lib, OTHER_COLOR(color_to_move), 3, NULL) <= 2
      && ladder_capturable(lib, OTHER_COLOR(color_to_move))) {
    int k;
    for (k = 0; k < 4; k++) {
      if (board[pos + delta[k]] == color_to_move)
	break;
    }
    if (k == 4)
      return 1;
  }
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
