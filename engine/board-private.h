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


#ifndef _BOARD_PRIVATE_H_
#define _BOARD_PRIVATE_H_


/* Experimental results show that the average number of change stack
 * entries per move usually is in the 20-30 range and very seldom
 * exceeds 40. But since we have no way to recover from running out of
 * stack space, we allocate with a substantial safety margin.
 */
#define STACK_SIZE 80 * MAXSTACK


/* FIXME: Investigate if they are worth it now when we can use
 *	   multiple boards.
 */
#define USE_BOARD_CACHES	(NUM_HASHVALUES <= 4)


/* We keep the address and the old value for each change. */
typedef struct _Change_stack_entry	Change_stack_entry;

struct _Change_stack_entry {
  int	       *address;
  int		value;
};


/* We keep the address and the old value for each change. */
typedef struct _Vertex_stack_entry	Vertex_stack_entry;

struct _Vertex_stack_entry {
  Intersection  *address;
  int		 value;
};


/* Used by approxlib() and accuratelib() to cache their results. */
typedef struct _Board_cache_entry	Board_cache_entry;

struct _Board_cache_entry {
  int		threshold;
  int		liberties;
  Hash_data	position_hash;
};


typedef struct _String_data	String_data;

struct _String_data {
  /* Color of string, BLACK or WHITE. */
  int		color;

  /* Number of stones in string. */
  int		size;

  /* Coordinates of "origin", i.e.  "upper left" stone. */
  int		origin;

  int		num_liberties;
  int		num_neighbors;

  /* General purpose mark. */
  int		mark;
};


typedef int	String_liberty_list[MAX_LIBERTIES];
typedef int	String_neighbor_list[MAXCHAIN];


/* The private data of goban. */
struct _Goban_private_data {
  /* Komaster data. */
  int		komaster;
  int		kom_pos;

  /* For liberty and string marking purposes. */
  int		liberty_mark;
  int		string_mark;
  int		last_liberty_marks[BOARDMAX];

  /* Index into list of strings.  The index is only valid if there is
   * a stone at the vertex.
   */
  int		string_index[BOARDMAX];

  /* Main array of string information. */
  String_data	strings[MAX_STRINGS];

  /* The stones in a string are linked together in a cyclic list.
   * These are the coordinates to the next stone in the string.
   */
  int		next_stone[BOARDMAX];

  /* Number of the next free string. */
  int		next_string;


  /* Stacks and stack pointers. */
  Change_stack_entry   change_stack[STACK_SIZE];
  Change_stack_entry  *change_stack_pointer;
  Vertex_stack_entry   vertex_stack[STACK_SIZE];
  Vertex_stack_entry  *vertex_stack_pointer;

  /* Stack of trial moves to get to current position and which color
   * made them.  Perhaps this should be one array of a structure.  The
   * associated stack pointer is public `goban->stackp'.
   */
  int		stack[MAXSTACK];
  int		move_color[MAXSTACK];

  /* A stack of hashes.  With this, we can just pop the previous hash
   * from the stack in popgo() instead of recomputing it.
   */
  Hash_data	board_hash_stack[MAXSTACK];


#if USE_BOARD_CACHES

  Board_cache_entry   approxlib_cache[BOARDMAX][2];
  Board_cache_entry   accuratelib_cache[BOARDMAX][2];

#endif

  /* Placed last of frequently accessed fields because of the gigantic
   * size.
   */
  String_liberty_list   string_liberties[MAX_STRINGS];
  String_neighbor_list  string_neighbors[MAX_STRINGS];


  /* Statistics.  It is more logical to keep one counter per `Goban'.
   * We can always sum them up later, if needed.  The opposite is not
   * true: if the counter is global, there is no way to know from
   * which `Goban' most ticks has come.
   */
  int		trymove_counter;


  /* Data cached by stones_on_board(). */
  int		stone_count_for_position;
  int		white_stones_on_board;
  int		black_stones_on_board;


  /* The board from which move history originates. */
  int		initial_board_ko_pos;
  int		initial_black_captured;
  int		initial_white_captured;
  Intersection	initial_board[BOARDSIZE];

  /* The move history. */
  int		move_history_pointer;
  int		move_history_color[MAX_MOVE_HISTORY];
  int		move_history_pos[MAX_MOVE_HISTORY];
};


#endif  /* _BOARD_PRIVATE_H_ */


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
