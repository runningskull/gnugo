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


#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>

#include "liberty.h"
#include "cache.h"

/* Size of array where candidate moves are stored. */
#define MAX_MOVES 362

static int add_array(int *array, int elt);
static int element_array (int *array,int elt);
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
static int moves_to_connect_in_three_moves(int *moves, int str1, int str2);
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
static int recursive_transitivity (int str1, int str2, int str3, int *move);
static int recursive_non_transitivity (int str1, int str2, int str3, int *move);

int nodes_connect=0,max_nodes_connect=2000,max_connect_depth=64;

/* adds an integer to an array of integers if it is not already there
 * the number of elements of the array is in array[0]
 */

static int add_array (int *array, int elt) {
  int r;
  
  for (r = 1; r < array[0] + 1; r++)
    if (array[r] == elt)
      return 0;

  array[0]++;
  array[array[0]] = elt;
  return 1;
}

/* test if an element is part of an array */

static int element_array (int *array,int elt) {
  int r;
  for (r = 1; r < array[0] + 1; r++)
    if (array[r] == elt)
      return 1;
  return 0;
}

/* only keep the elements of array1 which are also in array2 */

static void intersection_array(int *array1, int *array2) {
  int r, s;
  
  for (r = 1; r < array1[0] + 1; r++)
    if (!element_array(array2, array1[r])) {
      for (s = r; s< array1[0]; s++)
	array1[s]=array1[s+1];
      array1[0]--;
      r--;
    }
}

/* verifies that capturing the stone at str is not a snapback */

static int snapback (int str) {
  int stones, liberties, lib;

  /* if more than one stone captured, not a snapback */
  stones = countstones(str);
  if (stones > 1)
    return 0;

  /* if more than one liberty, not a snapback */
  liberties = findlib(str, 1, &lib);
  if (liberties > 1)
    return 0;

  /* if only one liberty after capture */
  if (trymove(lib, OTHER_COLOR(board[str]),
	      "snapback", str, EMPTY, 0)) {
    liberties=0;
    if (IS_STONE(board[lib]))
      liberties = countlib(lib);
    popgo();
    if (liberties > 1)
      return 0;
    return WIN;
  }
  return 0;
}

/* Verifies that the strings str1 and str2 can be connected
 * directly by playing one move, either by playing a common liberty
 * of the two strings, or by capturing a common adjacent string.
 *
 * This is the gi1 game function.
 */

static int connection_one_move(int str1, int str2) {
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
  
  return 0;
}

/* If the two strings str1 and str2 can be connected sends back WIN fill the
 * array moves with the only move that can prevent a connection in one move
 * (common liberties, liberties of common adjacent strings in atari).
 *
 * This is the ip1 game function. */

static int prevent_connection_one_move (int *moves, int str1, int str2) {
  int r, s;
  int liberties, libs[MAXLIBS];
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
      liberties = findlib(adjs[r], MAXLIBS, libs);
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

static int connected_one_move (int str1, int str2) {
  int r, res=0;
  int moves[MAX_MOVES];
  
  moves[0] = 0;
  if (prevent_connection_one_move(moves, str1, str2)) {
    res = WIN;
    for (r = 1; ((r < moves[0] + 1) && res); r++) {
      if (trymove(moves[r], OTHER_COLOR(board[str1]),
		  "connected_one_move", str1, EMPTY, 0)) {
	if (!connection_one_move(str1, str2))
	  res = 0;
	popgo();
      }
    }
  }
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

static int moves_to_connect_in_two_moves (int *moves, int str1, int str2) {
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
    common_adj_liberty=0;
    for (s = 0; s < liberties; s++)
      if (liberty_of_string(libs[s], str2))
	common_adj_liberty=1;
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
    common_adj_liberty=0;
    for (s = 0; s < liberties; s++)
      if (liberty_of_string(libs[s], str1))
	common_adj_liberty=1;
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
    if (board[WEST(libs[r])] == EMPTY) {
      if (liberty_of_string(WEST(libs[r]), str2)) {
	add_array(moves, libs[r]);
	add_array(moves, WEST(libs[r]));
      }
    }
    if (board[EAST(libs[r])] == EMPTY) {
      if (liberty_of_string(EAST(libs[r]), str2)) {
	add_array(moves, libs[r]);
	add_array(moves, EAST(libs[r]));
      }
    }
    if (board[SOUTH(libs[r])] == EMPTY) {
      if (liberty_of_string(SOUTH(libs[r]), str2)) {
	add_array(moves, libs[r]);
	add_array(moves, SOUTH(libs[r]));
      }
    }
    if (board[NORTH(libs[r])] == EMPTY) {
      if (liberty_of_string(NORTH(libs[r]), str2)) {
	add_array(moves, libs[r]);
	add_array(moves, NORTH(libs[r]));
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


  /* Move in on a three liberty opponent string which is adjacent to
   * str1 and has a liberty in common with str2.
   */
  adj = chainlinks2(str1, adjs, 3);
  for (r = 0; r < adj; r++) {
    liberties = find_common_libs(adjs[r], str2, MAXLIBS, libs);
    for (s = 0; s < liberties; s++)
      add_array(moves, libs[s]);
  }

  /* And vice versa. */
  adj = chainlinks2(str2, adjs, 3);
  for (r = 0; r < adj; r++) {
    liberties = find_common_libs(adjs[r], str1, MAXLIBS, libs);
    for (s = 0; s < liberties; s++)
      add_array(moves, libs[s]);
  }
  
  return 0;
}
  
/* Tests if the strings can be connected in three plies starts
 * with finding the possible moves that can connect.  If two
 * moves in a row are played, then try them and stops at the
 * first working move.  The strings are connected in two moves
 * if the function connected_one_move is verified after a move.
 *
 * This is the gi2 game function. */

static int connection_two_moves (int str1, int str2) {
  int r, res = 0, moves[MAX_MOVES];
  
  /* If one string is missing we have already failed. */
  if (board[str1] == EMPTY || board[str2] == EMPTY)
    return 0;
  
  moves[0]=0;
  if (moves_to_connect_in_two_moves(moves, str1, str2))
    return WIN;
  for (r = 1; ((r < moves[0] + 1) && !res); r++) {
    if (trymove(moves[r], board[str1],
		"connection_two_moves", str1, EMPTY, 0)) {
      if (connected_one_move(str1, str2))
	res = WIN;
      popgo();
    }
  }
  return res;
}

/* Find the complete set of possible moves that can prevent
 * a connection in three plies.
 *
 * The function is not yet written, but moves_to_connect_in_two_moves does
 * a similar job, so it is called temporarly.
 */

static int moves_to_prevent_connection_in_two_moves (int *moves,
						     int str1, int str2) {
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

static int prevent_connection_two_moves (int *moves, int str1, int str2) {
  int r, res=0;
  int possible_moves[MAX_MOVES];
  
  if (connection_two_moves(str1, str2)) {
    res = WIN;
    possible_moves[0]=0;
    moves_to_prevent_connection_in_two_moves(possible_moves, str1, str2);
    for (r = 1; r < possible_moves[0] + 1; r++) {
      if (trymove(possible_moves[r], OTHER_COLOR(board[str1]), 
		  "prevent_connection_two_moves", str1, EMPTY, 0)) {
	if (!connection_one_move(str1, str2))
	  if (!connection_two_moves(str1, str2))
	    add_array(moves, possible_moves[r]);
	popgo();
      }
    }
  }
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
 */

static int moves_to_connect_in_three_moves (int *moves, int str1, int str2) {
  int r;
  int liberties, libs[MAXLIBS];
  int k;
  int mx[BOARDMAX];
  
  if (moves_to_connect_in_two_moves(moves, str1, str2))
    return 1;

  /* Second order liberties of str1 that are second order liberties
   * of str2 and vice versa.
   */
  memset(mx, 0, sizeof(mx));
  liberties = findlib(str1, MAXLIBS, libs);
  for (r = 0; r < liberties; r++)
    for (k = 0; k < 4; k++)
      if (board[libs[r] + delta[k]] == EMPTY)
	mx[libs[r] + delta[k]] = 1;
  
  liberties = findlib(str2, MAXLIBS, libs);
  for (r = 0; r < liberties; r++)
    for (k = 0; k < 4; k++)
      if (ON_BOARD(libs[r] + delta[k]) && mx[libs[r] + delta[k]])
	add_array(moves, libs[r] + delta[k]);

  return 0;
}

/* Not yet written.
 *
 * Find the complete set of possible moves that can prevent
 * a connection in 5 plies.
 */

static int moves_to_prevent_connection_in_three_moves (int *moves,
						       int str1, int str2) {
  if (moves_to_prevent_connection_in_two_moves(moves, str1, str2))
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

static int simply_connected_two_moves (int str1, int str2) {
  int r, res=0;
  int moves[MAX_MOVES];
  
  /* If one string is missing we have already failed. */
  if (board[str1] == EMPTY || board[str2] == EMPTY)
    return 0;
  
  moves[0] = 0;
  if (prevent_connection_one_move(moves, str1, str2)) {
    res = WIN;
    for (r = 1; ((r < moves[0] + 1) && res); r++) {
      if (trymove(moves[r], OTHER_COLOR(board[str1]),
		  "connected_one_move", str1, EMPTY, 0)) {
	if (!connection_one_move(str1, str2))
	  if (!connection_two_moves(str1, str2))
	    res = 0;
	popgo();
      }
    }
  }
  return res;
}

/* Test if a move is a simple depth 5 connection.
 *
 * This is the gi311 game function.
 */

static int simple_connection_three_moves (int str1, int str2) {
  int r, res = 0, moves[MAX_MOVES];
  
  moves[0]=0;
  if (moves_to_connect_in_two_moves(moves, str1, str2))
    return WIN;
  for (r = 1; ((r < moves[0] + 1) && !res); r++) {
    if (trymove(moves[r], board[str1],
		"connection_two_moves", str1, EMPTY, 0)) {
      if (simply_connected_two_moves(str1, str2))
	res = WIN;
      popgo();
    }
  }
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

static int prevent_simple_connection_three_moves (int *moves, int str1, int str2) {
  int r, res=0;
  int possible_moves[MAX_MOVES];
  
  if (simple_connection_three_moves(str1, str2)) {
    res = WIN;
    possible_moves[0]=0;
    moves_to_prevent_connection_in_three_moves(possible_moves, str1, str2);
    for (r = 1; r < possible_moves[0] + 1; r++) {
      if (trymove(possible_moves[r], OTHER_COLOR(board[str1]), 
		  "prevent_connection_two_moves", str1, EMPTY, 0)) {
	if (!connection_one_move(str1, str2))
	  if (!connection_two_moves(str1, str2))
	    if (!simple_connection_three_moves(str1, str2))
	      add_array(moves, possible_moves[r]);
	popgo();
      }
    }
  }
  return res;
}

/* Find simple connections by looking at common liberties
 * or directly capturing a common adjacent string without a snapback
 * or looking at a ladder for a common adjacent string.
 */

static int quiescence_connect(int str1, int str2, int *move) {
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


/* Externally callable frontend to recursive_connect(). */

int
string_connect(int str1, int str2, int *move)
{
  nodes_connect = 0;
  *move = PASS_MOVE;
  return recursive_connect(str1, str2, move);
}


/* returns WIN if str1 and str2 can be connected. */

static int recursive_connect (int str1, int str2, int *move) {
  int i, res = 0, Moves[MAX_MOVES], ForcedMoves[MAX_MOVES];
  SETUP_TRACE_INFO2("recursive_connect", str1, str2);
  
  if ( (board[str1] == EMPTY) || (board[str2] == EMPTY) ) {
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
  
  if (stackp == max_connect_depth) {
    SGFTRACE2(PASS_MOVE, 0, "connection depth limit reached");
    return 0;
  }

  nodes_connect++;

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
  moves_to_connect_in_three_moves (Moves, str1, str2);

  /* if there are some forced moves to prevent the capture
   * of one of the two strings, then we only look at
   * the moves that prevent capture and that might also
   * connect
   */
  if ( (ForcedMoves[0] != 0) && (Moves[0] != 0) )
    intersection_array(Moves, ForcedMoves);

  for (i = 1; ((i < Moves[0] + 1) && (res == 0)); i++) {
    if (trymove(Moves[i], board[str1], "recursive_connect", str1, EMPTY, 0)) {
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
  

/* Externally callable frontend to recursive_disconnect(). */

int disconnect(int str1, int str2, int *move) {
  int i, res = WIN, Moves[MAX_MOVES];
  
  nodes_connect = 0;
  *move = PASS_MOVE;
  Moves[0]=0;
  moves_to_prevent_connection_in_three_moves (Moves, str1, str2);
  if (Moves[0] > 0)
    res = 0;
  for (i = 1; ((i < Moves[0] + 1) && (res == 0)); i++)
    if (trymove(Moves[i], OTHER_COLOR(board[str1]),
		"disconnect", str1, EMPTY, 0)) {
      if (!recursive_connect(str1, str2, move)) {
	*move = Moves[i];
	res = WIN;
      }
      popgo();
    }
  return res;
}


/* Returns WIN if str1 and str2 can be disconnected. */

static int recursive_disconnect (int str1, int str2, int *move) {
  int i, res = WIN, Moves[MAX_MOVES];
  
  SETUP_TRACE_INFO2("recursive_disconnect", str1, str2);
  
  if ((board[str1] == EMPTY) || (board[str2] == EMPTY)) {
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
  
  if (nodes_connect > max_nodes_connect) {
    SGFTRACE2(PASS_MOVE, WIN, "connection node limit reached");
    return WIN;
  }
  
  if (stackp == max_connect_depth) {
    SGFTRACE2(PASS_MOVE, WIN, "connection depth limit reached");
    return WIN;
  }
  
  nodes_connect++;

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
    for (i = 1; ((i < Moves[0] + 1) && (res == 0)); i++)
      if (trymove(Moves[i], OTHER_COLOR(board[str1]),
		  "recursive_disconnect", str1, EMPTY, 0)) {
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

static int quiescence_capture (int str, int *move) {
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
    result = naive_ladder(str, move);

  /* Turn the sgf traces back on. */
  sgf_dumptree = save_sgf_dumptree;
  count_variations = save_count_variations;

  return result;
}

#if 0
static int capture_one_move (int str) 
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

static int prevent_capture_one_move(int *moves, int str1) {
  int r, res=0;
  int liberties, libs[MAXLIBS];
  int adj, adjs[MAXCHAIN];
  
  liberties = findlib(str1, MAXLIBS, libs);
  if (liberties==1) {
    res = WIN;
    adj = chainlinks2(str1, adjs, 1);
    for (r = 0; r < adj; r++)
      add_array(moves, adjs[r]);
    add_array(moves, libs[0]);
  }
  return res;
}


/* Returns WIN if str1, str2 and str3 can be connected. */

static int recursive_transitivity (int str1, int str2, int str3, int *move) {
  int i, res = 0, Moves[MAX_MOVES], ForcedMoves[MAX_MOVES];

  SETUP_TRACE_INFO2("recursive_transitivity", str1, str3);
  
  if ( (board[str1] == EMPTY) || (board[str2] == EMPTY) || (board[str3] == EMPTY) ) {
    SGFTRACE2(PASS_MOVE, 0, "one string already captured");
    return 0;
  }
  
  if (same_string(str1, str2) && same_string(str1, str3)) {
    SGFTRACE2(PASS_MOVE, WIN, "already connected");
    return WIN;
  }

  if (nodes_connect > max_nodes_connect) {
    SGFTRACE2(PASS_MOVE, 0, "connection node limit reached");
    return 0;
  }
  
  if (stackp == max_connect_depth) {
    SGFTRACE2(PASS_MOVE, 0, "connection depth limit reached");
    return 0;
  }

  nodes_connect++;

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
  moves_to_connect_in_two_moves (Moves, str1, str2);
  moves_to_connect_in_two_moves (Moves, str2, str3);

  /* If there are some forced moves to prevent the capture
   * of one of the two strings, then we only look at
   * the moves that prevent capture and that might also
   * connect.
   */
  if ( (ForcedMoves[0] != 0) && (Moves[0] != 0) )
    intersection_array(Moves, ForcedMoves);

  for (i = 1; ((i < Moves[0] + 1) && (res == 0)); i++) {
    if (trymove(Moves[i], board[str1], "recursive_transitivity", 
		str1, EMPTY, 0)) {
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
  
/* Externally callable frontend to recursive_non_transitivity(). */

int non_transitivity(int str1, int str2, int str3, int *move) {
  int i, res = WIN, Moves[MAX_MOVES];
  
  nodes_connect = 0;
  *move = PASS_MOVE;
  moves_to_prevent_connection_in_three_moves (Moves, str1, str3);
  if (Moves[0] > 0)
    res = 0;
  for (i = 1; ((i < Moves[0] + 1) && (res == 0)); i++)
    if (trymove(Moves[i], OTHER_COLOR(board[str1]),
		"non_transitivity", str1, EMPTY, 0)) {
      if (!recursive_transitivity(str1, str2, str3, move)) {
	*move = Moves[i];
	res = WIN;
      }
      popgo();
    }
  return res;
}

/* Returns WIN if str1, str2 and str3 can be disconnected. */

static int recursive_non_transitivity (int str1, int str2, int str3, 
				       int *move) {
  int i, res = WIN, Moves[MAX_MOVES];
  
  SETUP_TRACE_INFO2("recursive_non_transitivity", str1, str3);
  
  if ((board[str1] == EMPTY) || (board[str2] == EMPTY) 
      || (board[str3] == EMPTY)) {
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
  
  if (nodes_connect > max_nodes_connect) {
    SGFTRACE2(PASS_MOVE, WIN, "connection node limit reached");
    return WIN;
  }
  
  if (stackp == max_connect_depth) {
    SGFTRACE2(PASS_MOVE, WIN, "connection depth limit reached");
    return WIN;
  }
  
  nodes_connect++;

  /* We are at an and node. Only look at forced moves. */
  Moves[0] = 0;
  if (prevent_connection_one_move(Moves, str1, str3))
    res = 0;
  else if (prevent_connection_two_moves(Moves, str1, str3))
    res = 0;
  else if (prevent_simple_connection_three_moves(Moves, str1, str3))
    res = 0;
  
  if (res == 0)
    for (i = 1; ((i < Moves[0] + 1) && (res == 0)); i++)
      if (trymove(Moves[i], OTHER_COLOR(board[str1]),
		  "recursive_non_transitivity", str1, EMPTY, 0)) {
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

/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
