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

int nodes_connect=0,max_nodes_connect=2000,max_connect_depth=64;

static int add_array (int *array, int elt) {
  int r, add = 1;
  
  for (r = 1; ((r < array[0] + 1) && add); r++)
    if (array[r] == elt)
      add = 0;
  if (add) {
    array[0]++;
    array[array[0]] = elt;
  }
  return add;
}

static int element_array (int *array,int elt) {
  int r;
  for (r = 1; r < array[0] + 1; r++)
    if (array[r] == elt)
      return 1;
  return 0;
}

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
  int stones, liberties, libs[MAXLIBS];

  /* if more than one stone captured, not a snapback */
  stones = countstones(str);
  if (stones > 1)
    return 0;

  /* if more than one liberty, not a snapback */
  liberties = findlib(str, MAXLIBS, libs);
  if (liberties > 1)
    return 0;

  /* if only one liberty after capture */
  if (trymove(libs[0], OTHER_COLOR(board[str]),
	      "snapback", str, EMPTY, 0)) {
    liberties=0;
    if (board[libs[0]] != EMPTY)
      liberties = countlib(libs[0]);
    popgo();
    if (liberties > 1)
      return 0;
    return WIN;
  }
  return 0;
}

static int connection_one_move(int str1, int str2) {
  int r;
  int adj, adjs[MAXCHAIN];
  
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

static int moves_to_connect_in_two_moves (int *moves, int str1, int str2) {
  int r, s, common_adj_liberty;
  int liberties, libs[MAXLIBS];
  int adj, adjs[MAXCHAIN];
  int adjadj, adjadjs[MAXCHAIN];
  int k;
  int color = board[str1];
  
  moves[0] = 0;

  /* Common liberties. */
  if (have_common_lib(str1, str2, libs)) {
    add_array(moves, libs[0]);
    return 1;
  }
  
  /* Capture a common adjacent string or an adjacent liberty of str1
   * that has a common liberty with str2...
   */
  adj = chainlinks2(str1, adjs, 2);
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
  adj = chainlinks2(str2, adjs, 2);
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
    else if (board[EAST(libs[r])] == EMPTY) {
      if (liberty_of_string(EAST(libs[r]), str2)) {
	add_array(moves, libs[r]);
	add_array(moves, EAST(libs[r]));
      }
    }
    else if (board[SOUTH(libs[r])] == EMPTY) {
      if (liberty_of_string(SOUTH(libs[r]), str2)) {
	add_array(moves, libs[r]);
	add_array(moves, SOUTH(libs[r]));
      }
    }
    else if (board[NORTH(libs[r])] == EMPTY) {
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
      if (board[pos] == color && have_common_lib(pos, str2, NULL))
	add_array(moves, libs[r]);
    }
  }

  /* And vice versa. */
  liberties = findlib(str2, MAXLIBS, libs);
  for (r = 0; r < liberties; r++) {
    for (k = 0; k < 4; k++) {
      int pos = libs[r] + delta[k];
      if (board[pos] == color && have_common_lib(pos, str1, NULL))
	add_array(moves, libs[r]);
    }
  }

  return 0;
}
  
static int connection_two_moves (int str1, int str2) {
  int r, res = 0, moves[MAX_MOVES];
  
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

static int moves_to_prevent_connection_in_two_moves (int *moves,
						     int str1, int str2) {
  if (moves_to_connect_in_two_moves(moves, str1, str2))
    return 1;
  return 0;
}

static int prevent_connection_two_moves (int *moves, int str1, int str2) {
  int r, res=0;
  int possible_moves[MAX_MOVES];
  
  if (connection_two_moves(str1, str2)) {
    res = WIN;
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

static int moves_to_connect_in_three_moves (int *moves, int str1, int str2) {
  if (moves_to_connect_in_two_moves(moves, str1, str2))
    return 1;
  return 0;
}

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
connect(int str1, int str2, int *move)
{
  nodes_connect = 0;
  *move = PASS_MOVE;
  return recursive_connect(str1, str2, move);
}


/* returns WIN if str1 and str2 can be connected */

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
  if (!prevent_capture_one_move(ForcedMoves, str1))
    prevent_capture_one_move(ForcedMoves, str2);
/*   else if (prevent_capture_two_moves(ForcedMoves, str1)) */
/*     ;  */
/*   else if (prevent_capture_two_moves(ForcedMoves, str2)) */
/*     ;  */
  
  moves_to_connect_in_three_moves (Moves, str1, str2);

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

int
disconnect(int str1, int str2, int *move)
{
  nodes_connect = 0;
  *move = PASS_MOVE;
  return recursive_disconnect(str1, str2, move);
}


/* returns WIN if str1 and str2 can be disconnected */

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

  Moves[0] = 0;
  if (prevent_connection_one_move(Moves, str1, str2))
    res = 0;
  else if (prevent_connection_two_moves(Moves, str1, str2))
    res = 0;
  /* to do */
  /* else if (prevent_simple_connection_three_moves(Moves, str1, str2)) */
  /*  res = 0; */
  
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
 
static int quiescence_capture (int str, int *move) {
  if (countlib(str) == 1) {
    findlib(str, 1, move);
    return WIN;
  }
  else if (countlib(str) == 2)
    return naive_ladder(str, move);
  return 0;
}

/* static int capture_one_move (int str) { */
/*   if (countlib(str) == 1) */
/*     return 1; */
/*   return 0; */
/* } */

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




/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
