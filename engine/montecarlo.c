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
#include <string.h>

#include "liberty.h"
#include "sgftree.h"
#include "gg_utils.h"

#include "random.h"
#include <math.h>

/* FIXME: Replace with a DEBUG_MC symbol for use with -d. */
static int mc_debug = 0;

#define TURN_OFF_ASSERTIONS 1


/* Special board code for Monte Carlo simulations.
 *
 * A liberty edge is the combination of the position of the liberty
 * and the direction to a string given by the index into the delta[]
 * array. A liberty edge at lib with corresponding string in direction
 * delta[dir] is encoded as (lib << 2 | dir).
 *
 * The stones of a string are linked in a cyclic list through the
 * next_stone field, just like the global board does.
 *
 * Likewise the liberty edges corresponding to each string are
 * connected in a doubly linked cyclic list through the
 * previous_liberty_edge and next_liberty_edge fields.
 *
 * The reference stone has to be the same for every stone in a string
 * but it doesn't have to be a predictable one, in contrast to the
 * origin in the global board. The reference stone is the only one
 * which is guaranteed to have a valid pointer to the "first" liberty
 * edge (just an arbitrary element in the circular list).
 *
 * The local_context field contains information about the surrounding
 * 8 points. The bit layout is
 *
 * 23   : Black suicide.
 * 22   : White suicide.
 * 21   : Black self-atari.
 * 20   : White self-atari.
 * 19,18: Number of stones captured by black move.
 * 17,16: Number of stones captured by white move.
 * 15,14: Color to the southeast.
 * 13,12: Color to the northeast.
 * 11,10: Color to the northwest.
 *  9, 8: Color to the southwest.
 *  7, 6: Color to the east.
 *  5, 4: Color to the north.
 *  3, 2: Color to the west.
 *  1, 0: Color to the south.
 *
 * The number of stones in atari is 0 if empty or not atari, 1 if one
 * stone in atari, 2 if two stones in atari, and 3 if three or more stones
 * in atari.
 *
 * The queue array is used to form a linked single list data structure
 * with O(1) add, O(1) lookup, and O(n) delete operations. The
 * assumption is that v1 = queue[0] points to the first board vertex
 * in the list, v2 = queue[v1] points to the second vertex and so on.
 * The list ends when the next vertex is the off-board point 1. Board
 * vertices not included in the list have queue[v1] = 0. Thus an empty
 * list is characterized by queue[0] = 1 and queue[v] = 0 for all
 * vertices on the board. Generally queue[v] can be used to test for
 * membership in the list. The list is used to keep track of points
 * needing updated local context or value information.
 *
 * The move_values_*, partitioned_move_value_sums_*,
 * move_partition_lists_*, and move_value_sum_* fields are together
 * used to track move values and quickly sample according the
 * distribution determined by the move values.
 *
 * The move_values_* arrays naturally hold the values of each move.
 *
 * The move_partition_lists_* arrays form a two-level access to the
 * legal moves of respective color. Depending on the MAX_BOARD size,
 * the vertices are split into 2, 4, 8, or 16 partitions (see below)
 * where the partition number of each vertex is given by the 1, 2, 3,
 * or 4 least significant bits of the vertex index respectively. The
 * legal moves in each partition are linked together just like the
 * queue array described above. The only difference is that multiple
 * partition linked lists are represented in the same array by
 * starting from out of board indices 0..1, 0..3, 0..7, and 0..15
 * respectively.
 *
 * The partitioned_move_value_sums_* arrays are simply the sums of
 * move values in each partition and the move_value_sum_white_* fields
 * are the sum of the values of all legal moves.
 */

#if MAX_BOARD < 4
#define NUM_MOVE_PARTITIONS 2
#elif MAX_BOARD < 8
#define NUM_MOVE_PARTITIONS 4
#elif MAX_BOARD < 16
#define NUM_MOVE_PARTITIONS 8
#else
#define NUM_MOVE_PARTITIONS 16
#endif

struct mc_board {
  Intersection board[BOARDSIZE];
  int local_context[BOARDSIZE];
  int queue[BOARDMAX];
  unsigned int move_values_white[BOARDMAX];
  unsigned int move_values_black[BOARDMAX];
  unsigned int partitioned_move_value_sums_white[NUM_MOVE_PARTITIONS];
  unsigned int partitioned_move_value_sums_black[NUM_MOVE_PARTITIONS];
  int move_partition_lists_white[BOARDMAX];
  int move_partition_lists_black[BOARDMAX];
  unsigned int move_value_sum_white;
  unsigned int move_value_sum_black;
  int board_ko_pos;
  int reference_stone[BOARDMAX];
  int next_stone[BOARDMAX];
  int first_liberty_edge[BOARDMAX];
  int previous_liberty_edge[4 * BOARDMAX];
  int next_liberty_edge[4 * BOARDMAX];
  Hash_data hash;
};

#define MC_ADD_TO_UPDATE_QUEUE(mc, pos) \
  do {					\
    if (!mc->queue[pos]) {		\
      mc->queue[pos] = mc->queue[0];	\
      mc->queue[0] = pos;		\
    }					\
  } while (0)

#define MC_ON_BOARD(pos) (mc->board[pos] != GRAY)

/* Add a liberty edge for a string at pos with liberty at lib and
 * direction dir.
 */
static void
mc_add_liberty_edge(struct mc_board *mc, int pos, int lib, int dir)
{
  int this_liberty_edge = (lib << 2) | dir;
  int reference = mc->reference_stone[pos];
  int first_liberty_edge = mc->first_liberty_edge[reference];

#if !TURN_OFF_ASSERTIONS
  gg_assert(lib + delta[dir] == pos);
#endif
  
  if (first_liberty_edge) {
    int second_liberty_edge = mc->next_liberty_edge[first_liberty_edge];
    mc->previous_liberty_edge[this_liberty_edge] = first_liberty_edge;
    mc->next_liberty_edge[this_liberty_edge] = second_liberty_edge;
    mc->next_liberty_edge[first_liberty_edge] = this_liberty_edge;
    mc->previous_liberty_edge[second_liberty_edge] = this_liberty_edge;
  }
  else {
    mc->first_liberty_edge[reference] = this_liberty_edge;
    mc->next_liberty_edge[this_liberty_edge] = this_liberty_edge;
    mc->previous_liberty_edge[this_liberty_edge] = this_liberty_edge;
  }
}


/* Remove a liberty edge for a string at pos with liberty at lib and
 * direction dir.
 */
static int
mc_remove_liberty_edge(struct mc_board *mc, int pos, int lib, int dir)
{
  int reference = mc->reference_stone[pos];
  int this_liberty_edge = (lib << 2) | dir;
  int next = mc->next_liberty_edge[this_liberty_edge];
  int previous = mc->previous_liberty_edge[this_liberty_edge];
  
#if !TURN_OFF_ASSERTIONS
  gg_assert(lib + delta[dir] == pos);
#endif
  
  if (next == this_liberty_edge) {
    mc->first_liberty_edge[reference] = 0;
    return 0;
  }

  mc->next_liberty_edge[previous] = next;
  mc->previous_liberty_edge[next] = previous;
  if (mc->first_liberty_edge[reference] == this_liberty_edge)
    mc->first_liberty_edge[reference] = next;

  return next;
}


/* Join the strings at str1 and str2. It is assumed that str1 is a
 * newly placed stone (possibly already joined with other strings) and
 * that the liberty edge corresponding to the liberty at the newly
 * placed stone has not yet been removed.
 */
static void
mc_join_strings(struct mc_board *mc, int str1, int str2)
{
  int reference = mc->reference_stone[str2];
  int liberty_edge2 = mc->first_liberty_edge[reference];
  int liberty_edge1 = mc->first_liberty_edge[mc->reference_stone[str1]];
  int next1;
  int next2;
  int pos = str1;

  /* Update the reference stone for str1. */
  do {
    mc->reference_stone[pos] = reference;
    pos = mc->next_stone[pos];
  } while (pos != str1);

  /* Switch next_stone pointers to join the strings. */
  next1 = mc->next_stone[str1];
  mc->next_stone[str1] = mc->next_stone[str2];
  mc->next_stone[str2] = next1;

  /* Join the circular liberty_edge structures. We know that str2
   * still has a liberty listed at the newly added stone so
   * liberty_edge2 is guaranteed to be non-zero.
   */
  if (liberty_edge1 != 0) {
    next1 = mc->next_liberty_edge[liberty_edge1];
    next2 = mc->next_liberty_edge[liberty_edge2];
    mc->next_liberty_edge[liberty_edge1] = next2;
    mc->next_liberty_edge[liberty_edge2] = next1;
    mc->previous_liberty_edge[next1] = liberty_edge2;
    mc->previous_liberty_edge[next2] = liberty_edge1;
  }
}


/* Does the string at str have at most two liberties? In that case,
 * add them to the update queue.
 */
static void
mc_queue_max_two_liberties(struct mc_board *mc, int str)
{
  int reference = mc->reference_stone[str];
  int first_liberty_edge = mc->first_liberty_edge[reference];
  int first_liberty = first_liberty_edge >> 2;
  int liberty_edge = mc->next_liberty_edge[first_liberty_edge];
  int second_liberty;
#if !TURN_OFF_ASSERTIONS
  ASSERT1(IS_STONE(mc->board[str]), str);
#endif
  if (first_liberty == NO_MOVE)
    return;
  while (liberty_edge != first_liberty_edge) {
    if ((liberty_edge >> 2) != first_liberty) {
      second_liberty = liberty_edge >> 2;
      while (liberty_edge != first_liberty_edge) {
	if ((liberty_edge >> 2) != first_liberty
	    && (liberty_edge >> 2) != second_liberty)
	  return;
	liberty_edge = mc->next_liberty_edge[liberty_edge];
      }
      MC_ADD_TO_UPDATE_QUEUE(mc, first_liberty);
      MC_ADD_TO_UPDATE_QUEUE(mc, second_liberty);
      return;
    }
    liberty_edge = mc->next_liberty_edge[liberty_edge];
  }

  MC_ADD_TO_UPDATE_QUEUE(mc, first_liberty);
}


/* Remove the string at str from the board. */
static int
mc_remove_string(struct mc_board *mc, int str)
{
  int color = mc->board[str];
  int other = OTHER_COLOR(color);
  int pos = str;
  int num_removed_stones = 0;
  int k;
  
  do {
    for (k = 0; k < 8; k++) {
      if (k < 4 && mc->board[pos + delta[k]] == other) {
	mc_queue_max_two_liberties(mc, pos + delta[k]);
	mc_add_liberty_edge(mc, pos + delta[k], pos, k);
      }
      if (mc->board[pos + delta[k]] == EMPTY)
	MC_ADD_TO_UPDATE_QUEUE(mc, pos + delta[k]);
    }
    mc->board[pos] = EMPTY;
    mc->local_context[NW(pos)] ^= color << 14;
    mc->local_context[SW(pos)] ^= color << 12;
    mc->local_context[SE(pos)] ^= color << 10;
    mc->local_context[NE(pos)] ^= color << 8;
    mc->local_context[WEST(pos)] ^= color << 6;
    mc->local_context[SOUTH(pos)] ^= color << 4;
    mc->local_context[EAST(pos)] ^= color << 2;
    mc->local_context[NORTH(pos)] ^= color;
    hashdata_invert_stone(&(mc->hash), pos, color);
    MC_ADD_TO_UPDATE_QUEUE(mc, pos);
    num_removed_stones++;
    pos = mc->next_stone[pos];
  } while (pos != str);

  return num_removed_stones;
}


/* Initialize a Monte Carlo board struct from the global board. */
static void
mc_init_board_from_global_board(struct mc_board *mc)
{
  int stones[BOARDMAX];
  int num_stones;
  int pos;
  int k;
  int r;
  
  memcpy(mc->board, board, sizeof(mc->board));
  mc->board_ko_pos = board_ko_pos;
  mc->hash = board_hash;
  memset(mc->queue, 0, sizeof(mc->queue));
  mc->queue[0] = 1;

  memset(mc->next_stone, 0, sizeof(mc->next_stone));
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    int geometry = ((mc->board[SE(pos)] << 14)
		    | (mc->board[NE(pos)] << 12)
		    | (mc->board[NW(pos)] << 10)
		    | (mc->board[SW(pos)] << 8)
		    | (mc->board[EAST(pos)] << 6)
		    | (mc->board[NORTH(pos)] << 4)
		    | (mc->board[WEST(pos)] << 2)
		    | mc->board[SOUTH(pos)]);
    mc->local_context[pos] = geometry;
    if (board[pos] == EMPTY) {
      int s;
      int captured_black_stones = 0;
      int captured_white_stones = 0;
      if (is_self_atari(pos, WHITE))
	mc->local_context[pos] |= 1 << 20;
      if (is_self_atari(pos, BLACK))
	mc->local_context[pos] |= 1 << 21;
      if (is_suicide(pos, WHITE))
	mc->local_context[pos] |= 1 << 22;
      if (is_suicide(pos, BLACK))
	mc->local_context[pos] |= 1 << 23;
      for (s = 0; s < 4; s++) {
	if (board[pos + delta[s]] == BLACK
	    && countlib(pos + delta[s]) == 1)
	  captured_black_stones += countstones(pos + delta[s]);
	else if (board[pos + delta[s]] == WHITE
		 && countlib(pos + delta[s]) == 1)
	  captured_white_stones += countstones(pos + delta[s]);
      }
      if (captured_black_stones > 3)
	captured_black_stones = 3;
      if (captured_white_stones > 3)
	captured_white_stones = 3;
      mc->local_context[pos] |= captured_black_stones << 16;
      mc->local_context[pos] |= captured_white_stones << 18;
    }
    
    if (IS_STONE(board[pos]) && mc->next_stone[pos] == 0) {
      num_stones = findstones(pos, BOARDMAX, stones);
      mc->first_liberty_edge[pos] = 0;
      for (r = 0; r < num_stones; r++) {
	mc->next_stone[stones[r]] = stones[(r + 1) % num_stones];
	mc->reference_stone[stones[r]] = pos;
	for (k = 0; k < 4; k++) {
	  if (board[stones[r] + delta[k]] == EMPTY)
	    mc_add_liberty_edge(mc, stones[r], stones[r] + delta[k],
				(k + 2) % 4);
	}
      }
    }
  }
}


#if 0
/* Debug tool. */
static void
mc_check_consistency_with_global_board(struct mc_board *mc)
{
  int pos;

  ASSERT1(board_ko_pos == mc->board_ko_pos, mc->board_ko_pos);
  for (pos = 0; pos < BOARDSIZE; pos++) {
    ASSERT1(board[pos] == mc->board[pos], pos);
    if (IS_STONE(board[pos])) {
      ASSERT1(same_string(pos, mc->reference_stone[pos]), pos);
      if (find_origin(pos) == pos) {
	int reference = mc->reference_stone[pos];
	int pos2 = pos;
	int num_stones = 0;
	int first_liberty_edge;
	int liberty_edge;
	int num_liberty_edges = 0;
	int k;
	int ml[4 * BOARDMAX];
	memset(ml, 0, sizeof(ml));
	
	do {
	  ASSERT1(mc->reference_stone[pos2] == reference, pos2);
	  ASSERT1(num_stones < countstones(pos), pos);
	  num_stones++;
	  for (k = 0; k < 4; k++)
	    if (board[pos2 + delta[k]] == EMPTY) {
	      ml[(pos2 + delta[k]) << 2 | (k + 2) % 4] = 1;
	      num_liberty_edges++;
	    }
	  pos2 = mc->next_stone[pos2];
	} while (pos2 != pos);
	ASSERT1(num_stones == countstones(pos), pos);

	first_liberty_edge = mc->first_liberty_edge[reference];
	liberty_edge = first_liberty_edge;
	do {
	  int previous = mc->previous_liberty_edge[liberty_edge];
	  int next = mc->next_liberty_edge[liberty_edge];
	  ASSERT1(ml[liberty_edge] == 1, pos);
	  ml[liberty_edge] = 0;
	  num_liberty_edges--;
	  ASSERT1(mc->next_liberty_edge[previous] == liberty_edge, pos);
	  ASSERT1(mc->previous_liberty_edge[next] == liberty_edge, pos);
	  ASSERT1(liberty_of_string(liberty_edge >> 2, pos), pos);
	  liberty_edge = mc->next_liberty_edge[liberty_edge];
	} while (liberty_edge != first_liberty_edge);
	ASSERT1(num_liberty_edges == 0, pos);
      }
    }
  }
}
#endif


/* Write the Monte Carlo board to outfile. */
static void
mc_showboard(struct mc_board *mc, FILE *outfile)
{
  int i, j;

  draw_letter_coordinates(outfile);
  
  for (i = 0; i < board_size; i++) {
    fprintf(outfile, "\n%2d", board_size - i);
    
    for (j = 0; j < board_size; j++) {
      if (mc->board[POS(i, j)] == EMPTY)
	fprintf(outfile, " %c", is_hoshi_point(i, j) ? '+' : '.');
      else
	fprintf(outfile, " %c", mc->board[POS(i, j)] == BLACK ? 'X' : 'O');
    }
  }
  
  fprintf(outfile, "\n");
  draw_letter_coordinates(outfile);
}


/* Count the number of stones in the string at str. Stop counting if
 * maxstones is reached.
 */
static int
mc_countstones(struct mc_board *mc, int str, int maxstones)
{
  int stone = str;
  int num_stones = 0;
  do {
    num_stones++;
    stone = mc->next_stone[stone];
  } while (stone != str && num_stones < maxstones);

  return num_stones;
}


/* Suicide is incrementally tracked by the local context information. */
#define mc_is_suicide(mc, pos, color) \
  ((mc->local_context[pos] >> (21 + color)) & 1)


#if !TURN_OFF_ASSERTIONS
/* Is a move at pos by color legal? */
static int
mc_is_legal(struct mc_board *mc, int pos, int color)
{
  if (pos == PASS_MOVE)
    return 1;

  if (mc->board[pos] != EMPTY)
    return 0;

  if (pos == mc->board_ko_pos) {
    if (mc->board[WEST(pos)] == OTHER_COLOR(color)
	|| mc->board[EAST(pos)] == OTHER_COLOR(color)) {
      return 0;
    }
  }

  return !mc_is_suicide(mc, pos, color);
}
#endif


/* Is the string at str in atari? Always place one liberty of the
 * string in lib, unless it's a null pointer.
 */
static int
mc_is_in_atari(struct mc_board *mc, int str, int *lib)
{
  int reference = mc->reference_stone[str];
  int first_liberty_edge = mc->first_liberty_edge[reference];
  int liberty = first_liberty_edge >> 2;
  int liberty_edge = mc->next_liberty_edge[first_liberty_edge];
#if !TURN_OFF_ASSERTIONS
  ASSERT1(IS_STONE(mc->board[str]), str);
#endif
  if (lib)
    *lib = liberty;
  while (liberty_edge != first_liberty_edge) {
    if ((liberty_edge >> 2) != liberty)
      return 0;
    liberty_edge = mc->next_liberty_edge[liberty_edge];
  }

  return 1;
}


/* Does the liberty edge chain at first_liberty_edge contain more than
 * one distinct liberty?
 */
static int
mc_is_in_atari2(struct mc_board *mc, int first_liberty, int first_liberty_edge)
{
  int liberty_edge = mc->next_liberty_edge[first_liberty_edge];
  while (liberty_edge != first_liberty_edge) {
    if ((liberty_edge >> 2) != first_liberty)
      return 0;
    liberty_edge = mc->next_liberty_edge[liberty_edge];
  }

  return 1;
}


/* Count the number of stones that would be captured if color played at move.
 * Return at most the number given by maxstones.
 */
static int
mc_stones_in_atari(struct mc_board *mc, int move, int color, int maxstones)
{
  int k;
  int stones_in_atari = 0;
  for (k = 0; k < 4 && stones_in_atari < maxstones; k++) {
    int pos = move + delta[k];
    if (mc->board[pos] == OTHER_COLOR(color)
	&& mc_is_in_atari(mc, pos, NULL))
      stones_in_atari += mc_countstones(mc, pos, maxstones - stones_in_atari);
  }
  
  return stones_in_atari;
}


/* Does the string at str have exactly two liberties? One liberty is
 * assumed to be known and passed in first_liberty, whereas the second
 * is placed in second_liberty.
 */
static int
mc_has_two_liberties_one_given(struct mc_board *mc, int str,
			       int first_liberty, int *second_liberty)
{
  int reference = mc->reference_stone[str];
  int first_liberty_edge = mc->first_liberty_edge[reference];
  int liberty_edge = first_liberty_edge;
  *second_liberty = NO_MOVE;
  do {
    int liberty = liberty_edge >> 2;
    if (liberty != first_liberty) {
      if (*second_liberty == NO_MOVE)
	*second_liberty = liberty;
      else if (liberty != *second_liberty)
	return 0;
    }
    liberty_edge = mc->next_liberty_edge[liberty_edge];
  } while (liberty_edge != first_liberty_edge);

  return (*second_liberty != NO_MOVE);
}


/* Is a move at pos by color a self atari? */
static int
mc_is_self_atari(struct mc_board *mc, int pos, int color)
{
  int k;
  int captured = NO_MOVE;
  int liberty = NO_MOVE;
  int reference;
  int other;

  /* Quick test which is often effective. */
  if (((mc->board[SOUTH(pos)] == EMPTY)
       + (mc->board[WEST(pos)] == EMPTY)
       + (mc->board[NORTH(pos)] == EMPTY)
       + (mc->board[EAST(pos)] == EMPTY)) > 1)
    return 0;

  /* Otherwise look closer. */
  for (k = 0; k < 4; k++) {
    int first_liberty_edge;
    int liberty_edge;
    int additional_liberty = 0;
    int pos2 = pos + delta[k];
    if (mc->board[pos2] == EMPTY) {
      if (pos2 != liberty) {
	if (liberty != NO_MOVE)
	  return 0;
	else
	  liberty = pos2;
      }
    }
    else if (IS_STONE(mc->board[pos2])) {
      first_liberty_edge = (pos << 2) | k;
      liberty_edge = mc->next_liberty_edge[first_liberty_edge];
      while (liberty_edge != first_liberty_edge) {
	int lib = liberty_edge >> 2;
	if (lib != pos) {
	  additional_liberty = 1;
	  if (mc->board[pos2] == color) {
	    if (lib != liberty) {
	      if (liberty != NO_MOVE)
		return 0;
	      else
		liberty = lib;
	    }
	  }
	  else
	    break;
	}
	liberty_edge = mc->next_liberty_edge[liberty_edge];
      }

      if (mc->board[pos2] != color && additional_liberty == 0) {
	captured = pos2;
	if (pos2 != liberty) {
	  if (liberty != NO_MOVE)
	    return 0;
	  else
	    liberty = pos2;
	}
      }
    }
  }

  if (liberty == NO_MOVE || captured == NO_MOVE)
    return 1;

  /* Now only the difficult case remains where there was no adjacent
   * empty stone, no adjacent friendly stone with an extra liberty,
   * and exactly one neighbor was captured. Then the question is
   * whether the capture produced a second liberty elsewhere.
   */
  reference = mc->reference_stone[captured];
  other = OTHER_COLOR(color);
  for (k = 0; k < 4; k++) {
    if (mc->board[pos + delta[k]] == color) {
      int stone = pos + delta[k];
      do {
	int m;
	for (m = 0; m < 4; m++) {
	  int pos2 = stone + delta[m];
	  if (mc->board[pos2] == other
	      && pos2 != captured
	      && mc->reference_stone[pos2] == reference)
	    return 0;
	}
	stone = mc->next_stone[stone];
      } while (stone != pos + delta[k]);
    }
  }

  return 1;
}


/* Update the local context information at pos, except the geometric
 * information, by recomputing it. Most of the information is obtained
 * by analyzing the presence of empty vertices or stones in atari
 * adjacent to pos.
 *
 * FIXME: There's some computations wasted by calling the full
 * mc_is_self_atari() and mc_stones_in_atari() functions when parts of
 * the relevant information is already available.
 */
static void
mc_update_local_context(struct mc_board *mc, int pos)
{
  int min_white_liberties = 0;
  int min_black_liberties = 0;
  int white_liberty_through_stones = 0;
  int black_liberty_through_stones = 0;
  int min_white_captured_stones = 0;
  int min_black_captured_stones = 0;
  int white_suicide = 0;
  int black_suicide = 0;
  int white_self_atari = 0;
  int black_self_atari = 0;
  int white_captured_stones = 0;
  int black_captured_stones = 0;
  int k;
  for (k = 0; k < 4; k++) {
    int pos2 = pos + delta[k];
    switch (mc->board[pos2]) {
      case EMPTY:
	min_white_liberties++;
	min_black_liberties++;
	break;
      case WHITE:
	if (mc_is_in_atari2(mc, pos, (pos << 2) | k)) {
	  min_black_liberties++;
	  min_white_captured_stones++;
	}
	else
	  white_liberty_through_stones = 1;
	break;
      case BLACK:
	if (mc_is_in_atari2(mc, pos, (pos << 2) | k)) {
	  min_white_liberties++;
	  min_black_captured_stones++;
	}
	else
	  black_liberty_through_stones = 1;
	break;
    }
  }

  if (min_white_liberties + white_liberty_through_stones == 0) {
    white_suicide = 1;
    white_self_atari = 1;
  }
  else if (min_white_liberties <= 1)
    white_self_atari = mc_is_self_atari(mc, pos, WHITE);

  if (min_black_liberties + black_liberty_through_stones == 0) {
    black_suicide = 1;
    black_self_atari = 1;
  }
  else if (min_black_liberties <= 1)
    black_self_atari = mc_is_self_atari(mc, pos, BLACK);

  if (min_white_captured_stones >= 3)
    white_captured_stones = 3;
  else if (min_white_captured_stones > 0)
    white_captured_stones = mc_stones_in_atari(mc, pos, BLACK, 3);

  if (min_black_captured_stones >= 3)
    black_captured_stones = 3;
  else if (min_black_captured_stones > 0)
    black_captured_stones = mc_stones_in_atari(mc, pos, WHITE, 3);

  mc->local_context[pos] &= 0xffff;
  mc->local_context[pos] |= black_captured_stones << 16;
  mc->local_context[pos] |= white_captured_stones << 18;
  mc->local_context[pos] |= white_self_atari << 20;
  mc->local_context[pos] |= black_self_atari << 21;
  mc->local_context[pos] |= white_suicide << 22;
  mc->local_context[pos] |= black_suicide << 23;
}


/* Play the move at pos by color. */
static int
mc_play_move(struct mc_board *mc, int pos, int color)
{
  int k;
  int captured_stones = 0;
  int num_direct_liberties = 0;
  int pos2;

  /* Clear the update queue. */
  while (mc->queue[0] != 1) {
    pos2 = mc->queue[0];
    mc->queue[0] = mc->queue[pos2];
    mc->queue[pos2] = 0;
  }

  if (pos == PASS_MOVE) {
    if (mc->board_ko_pos != NO_MOVE)
      hashdata_invert_ko(&mc->hash, mc->board_ko_pos);
    mc->board_ko_pos = NO_MOVE;
    return 1;
  }
  
  /* The move must not be the ko point. */
  if (pos == mc->board_ko_pos) {
    if (mc->board[WEST(pos)] == OTHER_COLOR(color)
	|| mc->board[EAST(pos)] == OTHER_COLOR(color)) {
      return 0;
    }
  }
  
  /* Test for suicide. */
  if (mc_is_suicide(mc, pos, color))
    return 0;

  if (mc->board_ko_pos != NO_MOVE)
    hashdata_invert_ko(&mc->hash, mc->board_ko_pos);
  mc->board_ko_pos = NO_MOVE;

#if !TURN_OFF_ASSERTIONS
  ASSERT1(mc->board[pos] == EMPTY, pos);
#endif
  mc->board[pos] = color;
  hashdata_invert_stone(&mc->hash, pos, color);
  mc->next_stone[pos] = pos;
  
  /* Update the geometry part of the local context. */
  mc->local_context[NW(pos)] |= color << 14;
  mc->local_context[SW(pos)] |= color << 12;
  mc->local_context[SE(pos)] |= color << 10;
  mc->local_context[NE(pos)] |= color << 8;
  mc->local_context[WEST(pos)] |= color << 6;
  mc->local_context[SOUTH(pos)] |= color << 4;
  mc->local_context[EAST(pos)] |= color << 2;
  mc->local_context[NORTH(pos)] |= color;
  
  mc->reference_stone[pos] = pos;
  mc->first_liberty_edge[pos] = 0;

  for (k = 0; k < 4; k++) {
    pos2 = pos + delta[k];
    if (mc->board[pos2] == EMPTY) {
      mc_add_liberty_edge(mc, pos, pos2, (k + 2) % 4);
      num_direct_liberties++;
      MC_ADD_TO_UPDATE_QUEUE(mc, pos2);
    }
  }
  
  for (k = 0; k < 4; k++) {
    int liberty;
    pos2 = pos + delta[k];
    if (mc->board[pos2] == color) {
      if (mc->reference_stone[pos] != mc->reference_stone[pos2]) {
	if (mc_has_two_liberties_one_given(mc, pos2, pos, &liberty))
	  MC_ADD_TO_UPDATE_QUEUE(mc, liberty);
	mc_join_strings(mc, pos, pos2);
      }
      mc_remove_liberty_edge(mc, pos2, pos, k);
    }
  }

  for (k = 0; k < 4; k++) {
    pos2 = pos + delta[k];
    if (mc->board[pos2] == OTHER_COLOR(color)) {
      if (mc_remove_liberty_edge(mc, pos2, pos, k) == 0)
	captured_stones += mc_remove_string(mc, pos2);
      else
	mc_queue_max_two_liberties(mc, pos2);
    }
  }

  if (captured_stones == 1
      && mc->next_stone[pos] == pos
      && num_direct_liberties == 0) {
    mc->board_ko_pos = mc->first_liberty_edge[pos] >> 2;
    hashdata_invert_ko(&mc->hash, mc->board_ko_pos);
  }

  mc_queue_max_two_liberties(mc, pos);

  /* Traverse the update queue and update the local context for queued
   * points.
   */
  for (pos2 = mc->queue[0]; pos2 != 1; pos2 = mc->queue[pos2])
    if (pos2 != pos)
      mc_update_local_context(mc, pos2);

  /* Add the immediate neighborhood of the move to the update queue
   * for recomputation of move values later on.
   */
  MC_ADD_TO_UPDATE_QUEUE(mc, pos);
  for (k = 0; k < 8; k++)
    if (mc->board[pos + delta[k]] == EMPTY)
      MC_ADD_TO_UPDATE_QUEUE(mc, pos + delta[k]);
  
  return 1;
}


/***************************************************/

#define NUM_GEOMETRIES 1107
#define NUM_PROPERTIES 256

struct mc_pattern_table
{
  unsigned short geometry_table[65536];
  unsigned int values[(NUM_GEOMETRIES + 1) * NUM_PROPERTIES];
};

static struct mc_pattern_table mc_patterns;

/* The pattern number is determined by the following bit layout:
 * 18-8: Geometry number (range 1..1107)
 * 7   : Opponent suicide
 * 6   : Our self-atari
 * 5   : Opponent self-atari
 * 4,3 : Our captures
 * 2,1 : Opponent captures
 * 0   : Near
 */
static int
mc_find_pattern_number(struct mc_board *mc, int move, int color,
		       int near_previous_move)
{
  int local_context = mc->local_context[move];
  int properties;
  int geometry;

  if (color == WHITE) {
    properties = (((local_context >> 16) & 0xa0)
		  | ((local_context >> 14) & 0x40)
		  | ((local_context >> 17) & 0x06)
		  | ((local_context >> 13) & 0x18));
    geometry = local_context & 0xffff;
  }
  else {
    properties = (local_context >> 15) & 0xfe;
    geometry = (((local_context & 0x5555) << 1)
		| ((local_context & 0xaaaa) >> 1));
  }

  return ((mc_patterns.geometry_table[geometry] << 8)
	  | properties
	  | near_previous_move);
}


/* Geometry patterns have the neighborhood defined by the order
 *
 * 637
 * 2*4
 * 518
 *
 * where * is the point to play. The reason for this seemingly
 * arbitrary order is to be consistent with the delta[] array
 * of point offsets.
 *
 * The 8 rotation/mirror transformations are given by reordering the
 * points like this:
 * 12345678 no transform
 * 41238567 rotation 90
 * 34127856 rotation 180
 * 23416785 rotation 270
 * 14328765 mirror
 * 21435876 mirror + rotation 90
 * 32146587 mirror + rotation 180
 * 43217658 mirror + rotation 270
 *
 * The geometry is encoded by a 16-bit integer where point 1 goes into
 * the 2 least significant bits and point 8 into the 2 most
 * significant bits. Each pair of bits contain the corresponding
 * EMPTY, WHITE, BLACK, GRAY (off board) values.
 */
static unsigned short
mc_register_geometry_pattern(unsigned int pattern, unsigned short n)
{
  int k;
  int j;
  unsigned int transformed_pattern;
  
  if (mc_patterns.geometry_table[pattern] != 0)
    return 0;

  for (k = 0; k < 8; k++) {
    transformed_pattern = pattern;
    if (k >= 4) {
      /* Mirror pattern. */
      transformed_pattern = (((pattern & 0x0300) << 6)
			     | ((pattern & 0x000c) << 4)
			     | ((pattern & 0x0c00) << 2)
			     | (pattern & 0x0033)
			     | ((pattern & 0x3000) >> 2)
			     | ((pattern & 0x00c0) >> 4)
			     | ((pattern & 0xc000) >> 6));
    }

    /* Rotate pattern. */
    for (j = 0; j < k % 4; j++) {
      transformed_pattern = (((transformed_pattern & 0xc0c0) >> 6)
			     | ((transformed_pattern & 0x3f3f) << 2));
    }
    mc_patterns.geometry_table[transformed_pattern] = n;
  }

  return 1;
}


/* Compute the mapping from 8-point local neighborhoods to rotation
 * invariant geometry numbers.
 */
static void
mc_init_pattern_geometries(void)
{
  unsigned int pattern;
  unsigned short n = 1;

  static int initialized = 0;
  if (initialized)
    return;
  initialized = 1;

  memset(mc_patterns.geometry_table, 0, sizeof(mc_patterns.geometry_table));
  
  for (pattern = 0; pattern < 65536; pattern++) {
    unsigned int off_board = (pattern & (pattern >> 1)) & 0x5555;
    if (off_board == 0x0 || off_board == 0x1410 || off_board == 0x5450)
      n += mc_register_geometry_pattern(pattern, n);
  }

  gg_assert(n == NUM_GEOMETRIES + 1);
}


/* Determine which geometry numbers are matched by a pattern with
 * possible wildcards, for use when loading pattern databases.
 *
 * This function is recursive with the argument n determining which
 * point in the neighborhood is expanded for wildcards.
 */
static void
mc_match_geometries(int pattern[8], int *matching_geometries, int n)
{
  int k;
  int geometry = 0;
  if (n == 8) {
    /* The pattern has been fully expanded. Find the geometry number. */
    for (k = 0; k < 8; k++) {
      if (pattern[k] == 'O')
	geometry |= WHITE << (2 * k);
      else if (pattern[k] == 'X')
	geometry |= BLACK << (2 * k);
      else if (pattern[k] == '+' || pattern[k] == '|' || pattern[k] == '-')
	geometry |= (WHITE | BLACK) << (2 * k);
    }
    if (mc_patterns.geometry_table[geometry] != 0) {
      matching_geometries[mc_patterns.geometry_table[geometry]] = 1;
    }
  }
  else {
    /* Recurse with all possible expansions of the current
     * neighborhood point.
     */
    int new_pattern[8];
    memcpy(new_pattern, pattern, sizeof(new_pattern));
    switch (pattern[n]) {
      case '.':
      case 'O':
      case 'X':
      case '|':
      case '-':
      case '+':
	mc_match_geometries(new_pattern, matching_geometries, n + 1);
	break;
      case 'o':
	new_pattern[n] = '.';
	mc_match_geometries(new_pattern, matching_geometries, n + 1);
	new_pattern[n] = 'O';
	mc_match_geometries(new_pattern, matching_geometries, n + 1);
	break;
      case 'x':
	new_pattern[n] = '.';
	mc_match_geometries(new_pattern, matching_geometries, n + 1);
	new_pattern[n] = 'X';
	mc_match_geometries(new_pattern, matching_geometries, n + 1);
	break;
      case '?':
	new_pattern[n] = '.';
	mc_match_geometries(new_pattern, matching_geometries, n + 1);
	new_pattern[n] = 'O';
	mc_match_geometries(new_pattern, matching_geometries, n + 1);
	new_pattern[n] = 'X';
	mc_match_geometries(new_pattern, matching_geometries, n + 1);
	break;
      case '%':
	new_pattern[n] = '.';
	mc_match_geometries(new_pattern, matching_geometries, n + 1);
	new_pattern[n] = 'O';
	mc_match_geometries(new_pattern, matching_geometries, n + 1);
	new_pattern[n] = 'X';
	mc_match_geometries(new_pattern, matching_geometries, n + 1);
	new_pattern[n] = '+';
	mc_match_geometries(new_pattern, matching_geometries, n + 1);
	break;
    }
  }
}


/* Clear a subset of the property combinations determined by shift,
 * mask, and value.
 */
static void
mc_clear_properties(int *properties, int shift, int mask, int value)
{
  int k;
  for (k = 0; k < NUM_PROPERTIES; k++)
    if (((k >> shift) & mask) == value)
      properties[k] = 0;
}


/* Find which property combinations are consistent with the rules
 * given in buf.
 */
static void
mc_analyze_properties(char *buf, int *properties)
{
  int k;

  /* First set all properties. */
  for (k = 0; k < NUM_PROPERTIES; k++)
    properties[k] = 1;

  /* Then reset the ones which are inconsistent. */
  if (strstr(buf, "near"))
    mc_clear_properties(properties, 0, 1, 0);
  if (strstr(buf, "far"))
    mc_clear_properties(properties, 0, 1, 1);
  if (strstr(buf, "xcap0")) {
    mc_clear_properties(properties, 1, 3, 1);
    mc_clear_properties(properties, 1, 3, 2);
    mc_clear_properties(properties, 1, 3, 3);
  }
  if (strstr(buf, "xcap1+"))
    mc_clear_properties(properties, 1, 3, 0);
  else if (strstr(buf, "xcap1-")) {
    mc_clear_properties(properties, 1, 3, 2);
    mc_clear_properties(properties, 1, 3, 3);
  }
  else if (strstr(buf, "xcap1")) {
    mc_clear_properties(properties, 1, 3, 0);
    mc_clear_properties(properties, 1, 3, 2);
    mc_clear_properties(properties, 1, 3, 3);
  }
  if (strstr(buf, "xcap2+")) {
    mc_clear_properties(properties, 1, 3, 0);
    mc_clear_properties(properties, 1, 3, 1);
  }
  else if (strstr(buf, "xcap2-"))
    mc_clear_properties(properties, 1, 3, 3);
  else if (strstr(buf, "xcap2")) {
    mc_clear_properties(properties, 1, 3, 0);
    mc_clear_properties(properties, 1, 3, 1);
    mc_clear_properties(properties, 1, 3, 3);
  }
  if (strstr(buf, "xcap3")) {
    mc_clear_properties(properties, 1, 3, 0);
    mc_clear_properties(properties, 1, 3, 1);
    mc_clear_properties(properties, 1, 3, 2);
  }
  if (strstr(buf, "ocap0")) {
    mc_clear_properties(properties, 3, 3, 1);
    mc_clear_properties(properties, 3, 3, 2);
    mc_clear_properties(properties, 3, 3, 3);
  }
  if (strstr(buf, "ocap1+"))
    mc_clear_properties(properties, 3, 3, 0);
  else if (strstr(buf, "ocap1-")) {
    mc_clear_properties(properties, 3, 3, 2);
    mc_clear_properties(properties, 3, 3, 3);
  }
  else if (strstr(buf, "ocap1")) {
    mc_clear_properties(properties, 3, 3, 0);
    mc_clear_properties(properties, 3, 3, 2);
    mc_clear_properties(properties, 3, 3, 3);
  }
  if (strstr(buf, "ocap2+")) {
    mc_clear_properties(properties, 3, 3, 0);
    mc_clear_properties(properties, 3, 3, 1);
  }
  else if (strstr(buf, "ocap2-"))
    mc_clear_properties(properties, 3, 3, 3);
  else if (strstr(buf, "ocap2")) {
    mc_clear_properties(properties, 3, 3, 0);
    mc_clear_properties(properties, 3, 3, 1);
    mc_clear_properties(properties, 3, 3, 3);
  }
  if (strstr(buf, "ocap3")) {
    mc_clear_properties(properties, 3, 3, 0);
    mc_clear_properties(properties, 3, 3, 1);
    mc_clear_properties(properties, 3, 3, 2);
  }
  if (strstr(buf, "xsafe"))
    mc_clear_properties(properties, 5, 1, 1);
  if (strstr(buf, "xunsafe"))
    mc_clear_properties(properties, 5, 1, 0);
  if (strstr(buf, "osafe"))
    mc_clear_properties(properties, 6, 1, 1);
  if (strstr(buf, "ounsafe"))
    mc_clear_properties(properties, 6, 1, 0);
  if (strstr(buf, "xsuicide"))
    mc_clear_properties(properties, 7, 1, 0);
  if (strstr(buf, "xnosuicide"))
    mc_clear_properties(properties, 7, 1, 1);
}


/* Export the size of the array mc_patterns.values so that external
 * callers of mc_load_patterns_from_db() know how big arrays to
 * allocate.
 */
int
mc_get_size_of_pattern_values_table(void)
{
  return (NUM_GEOMETRIES + 1) * NUM_PROPERTIES;
}


/* Load Monte Carlo patterns from file in .db format. If values is
 * NULL, load directly into mc_patterns.values.
 */
int
mc_load_patterns_from_db(const char *filename, unsigned int *values)
{
  FILE *pattern_file;
  char buf[80];
  unsigned int value;
  int pattern_line = 0;
  int current_pattern[8];
  int patterns_expanded = 0;
  int *matching_geometries;
  int properties[NUM_PROPERTIES];
  int k;
  int m;

  if (!values)
    values = mc_patterns.values;

  mc_init_pattern_geometries();
  
  pattern_file = fopen(filename, "r");
  if (!pattern_file) {
    gprintf("Failed to open %s file.\n", filename);
    return 0;
  }

  matching_geometries = malloc((NUM_GEOMETRIES + 1)
			       * sizeof(*matching_geometries));

  /* Set unloaded patterns to a "-1" value. */
  for (k = 1; k <= NUM_GEOMETRIES; k++)
    for (m = 0; m < NUM_PROPERTIES; m++)
      values[k * NUM_PROPERTIES + m] = 0xffffffffU;

  /* Loop over the rows of the pattern database. */
  while (fgets(buf, 80, pattern_file)) {
    if (strchr(".xXoO|+-?%", buf[0])) {
      /* Pattern line found */
      patterns_expanded = 0;
      if (pattern_line == 0) {
	current_pattern[5] = buf[0];
	current_pattern[2] = buf[1];
	current_pattern[6] = buf[2];
      }
      else if (pattern_line == 1) {
	current_pattern[1] = buf[0];
	current_pattern[3] = buf[2];
      }
      else if (pattern_line == 2) {
	current_pattern[4] = buf[0];
	current_pattern[0] = buf[1];
	current_pattern[7] = buf[2];
      }
      pattern_line++;
    }
    else if (sscanf(buf, ":%u", &value) == 1) {
      /* Colon line found. */
      if (value > 10000000)
	fprintf(stderr, "Warning: pattern values should be at most 10000000.");

      if (!patterns_expanded) {
	/* Find the set of rotation invariant geometries matching the
	 * pattern.
	 */
	memset(matching_geometries, 0,
	       (NUM_GEOMETRIES + 1) * sizeof(*matching_geometries));
	mc_match_geometries(current_pattern, matching_geometries, 0);
	patterns_expanded = 1;
      }

      /* Find the set of matching property values. */
      mc_analyze_properties(buf, properties);

      /* Set the value for the combinations of matched geometries and
       * properties, except those which have already been matched by a
       * previous pattern.
       */
      for (k = 1; k <= NUM_GEOMETRIES; k++)
	if (matching_geometries[k])
	  for (m = 0; m < NUM_PROPERTIES; m++)
	    if (properties[m] && values[k * NUM_PROPERTIES + m] == 0xffffffffU)
	      values[k * NUM_PROPERTIES + m] = value;

      pattern_line = 0;
    }
  }

  fclose(pattern_file);

  /* Set unmatched patterns/properties to a value of 1. */
  for (k = 1; k <= NUM_GEOMETRIES; k++)
    for (m = 0; m < NUM_PROPERTIES; m++)
      if (values[k * NUM_PROPERTIES + m] == 0xffffffffU)
	values[k * NUM_PROPERTIES + m] = 1;

  free(matching_geometries);
  return 1;
}


/* Set up local pattern values. */
void
mc_init_patterns(const unsigned int *values)
{
  mc_init_pattern_geometries();
  memcpy(mc_patterns.values, values, sizeof(mc_patterns.values));
}


/* Initialize the data structures used to keep track of the local
 * pattern values.
 */
static void
mc_init_move_values(struct mc_board *mc)
{
  int pos;
  int k;

  memset(mc->move_values_white, 0, sizeof(mc->move_values_white));
  memset(mc->move_values_black, 0, sizeof(mc->move_values_black));
  memset(mc->partitioned_move_value_sums_white, 0,
	 sizeof(mc->partitioned_move_value_sums_white));
  memset(mc->partitioned_move_value_sums_black, 0,
	 sizeof(mc->partitioned_move_value_sums_black));
  memset(mc->move_partition_lists_white, 0,
	 sizeof(mc->move_partition_lists_white));
  memset(mc->move_partition_lists_black, 0,
	 sizeof(mc->move_partition_lists_black));

  mc->move_value_sum_white = 0.0;
  mc->move_value_sum_black = 0.0;

  for (k = 0; k < NUM_MOVE_PARTITIONS; k++) {
    mc->move_partition_lists_white[k] = 1;
    mc->move_partition_lists_black[k] = 1;
  }
  
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (mc->board[pos] == EMPTY) {
      int partition = pos & (NUM_MOVE_PARTITIONS - 1);
      if (!mc_is_suicide(mc, pos, WHITE)) {
	int pattern = mc_find_pattern_number(mc, pos, WHITE, 0);
	unsigned int value = mc_patterns.values[pattern];
	mc->move_values_white[pos] = value;
	mc->partitioned_move_value_sums_white[partition] += value;
	mc->move_value_sum_white += value;
	mc->move_partition_lists_white[pos] = mc->move_partition_lists_white[partition];
	mc->move_partition_lists_white[partition] = pos;
      }
      if (!mc_is_suicide(mc, pos, BLACK)) {
	int pattern = mc_find_pattern_number(mc, pos, BLACK, 0);
	unsigned int value = mc_patterns.values[pattern];
	mc->move_values_black[pos] = value;
	mc->partitioned_move_value_sums_black[partition] += value;
	mc->move_value_sum_black += value;
	mc->move_partition_lists_black[pos] = mc->move_partition_lists_black[partition];
	mc->move_partition_lists_black[partition] = pos;
      }
    }
  }
}


/* Add a move at a vertex which was previously not a legal move. */
static void
mc_add_move(struct mc_board *mc, int pos, int color, int partition,
	    unsigned int *move_values, int *partition_lists,
	    unsigned int *partition_sums, unsigned int *move_value_sum)
{
  int pattern = mc_find_pattern_number(mc, pos, color, 0);
  unsigned int value = mc_patterns.values[pattern];
  partition_lists[pos] = partition_lists[partition];
  partition_lists[partition] = pos;
  move_values[pos] = value;
  partition_sums[partition] += value;
  *move_value_sum += value;
}


/* Update a move value. */
static void
mc_update_move(struct mc_board *mc, int pos, int color, int partition,
	       unsigned int *move_values, unsigned int *partition_sums,
	       unsigned int *move_value_sum)
{
  int pattern = mc_find_pattern_number(mc, pos, color, 0);
  unsigned int value = mc_patterns.values[pattern];
  partition_sums[partition] += value - move_values[pos];
  *move_value_sum += value - move_values[pos];
  move_values[pos] = value;
}


/* Remove a move because it has been played or has become suicide. */
static void
mc_remove_move(int pos, int partition, unsigned int *move_values,
	       int *partition_lists, unsigned int *partition_sums,
	       unsigned int *move_value_sum)
{
  int pos2;
  int pos3;
  for (pos2 = partition; partition_lists[pos2] != 1; pos2 = partition_lists[pos2]) {
    if (partition_lists[pos2] == pos)
      break;
  }
  pos3 = partition_lists[pos2];
  partition_lists[pos2] = partition_lists[pos3];
  partition_lists[pos3] = 0;
  partition_sums[partition] -= move_values[pos];
  *move_value_sum -= move_values[pos];
  move_values[pos] = 0.0;
}


/* Update move values for the moves listed in the update queue. */
static void
mc_update_move_values(struct mc_board *mc)
{
  int pos;
  int partition;
  for (pos = mc->queue[0]; pos != 1; pos = mc->queue[pos]) {
    partition = pos & (NUM_MOVE_PARTITIONS - 1);
    if ((mc->board[pos] != EMPTY || mc_is_suicide(mc, pos, WHITE))) {
      if (mc->move_partition_lists_white[pos] != 0) {
	mc_remove_move(pos, partition, mc->move_values_white,
		       mc->move_partition_lists_white,
		       mc->partitioned_move_value_sums_white,
		       &mc->move_value_sum_white);
      }
    }
    else {
      if (mc->move_partition_lists_white[pos] == 0)
	mc_add_move(mc, pos, WHITE, partition, mc->move_values_white,
		    mc->move_partition_lists_white,
		    mc->partitioned_move_value_sums_white,
		    &mc->move_value_sum_white);
      else
	mc_update_move(mc, pos, WHITE, partition, mc->move_values_white,
		       mc->partitioned_move_value_sums_white,
		       &mc->move_value_sum_white);
    }

    if ((mc->board[pos] != EMPTY || mc_is_suicide(mc, pos, BLACK))) {
      if (mc->move_partition_lists_black[pos] != 0) {
	mc_remove_move(pos, partition, mc->move_values_black,
		       mc->move_partition_lists_black,
		       mc->partitioned_move_value_sums_black,
		       &mc->move_value_sum_black);
      }
    }
    else {
      if (mc->move_partition_lists_black[pos] == 0)
	mc_add_move(mc, pos, BLACK, partition, mc->move_values_black,
		    mc->move_partition_lists_black,
		    mc->partitioned_move_value_sums_black,
		    &mc->move_value_sum_black);
      else
	mc_update_move(mc, pos, BLACK, partition, mc->move_values_black,
		       mc->partitioned_move_value_sums_black,
		       &mc->move_value_sum_black);
    }
  }
}


/***************************************************/

#define ASSERT_LEGAL 1

struct mc_game {
  struct mc_board mc;
  int move_history[600];
  unsigned char settled[BOARDMAX];
  int color_to_move;
  int last_move;
  int consecutive_passes;
  int consecutive_ko_captures;
  int depth;
};


/* Generate a random move. */
static int
mc_generate_random_move(struct mc_game *game)
{
  struct mc_board *mc = &game->mc;
  int last_move = game->last_move;
  int color = game->color_to_move;
  int depth = game->depth;
  
  int pos;

  int near_moves[BOARDMAX];
  unsigned int saved_near_move_values[BOARDMAX];
  int num_near_moves;
  unsigned int *move_values;
  unsigned int *partition_sums;
  int *partition_lists;
  unsigned int *move_value_sum;
  unsigned int saved_ko_value = 0;
  int partition;
  int move;
  int k;
  int x;

  /* If we get this deep we are almost certainly playing triple ko
   * without alternative options, so just give up and score as is.
   *
   * FIXME: Handle this in some proper way.
   */
  if (depth > 600) {
    if (mc_debug) {
      int pos;
      fprintf(stderr, "Reached 600 iterations.\n");
      mc_showboard(mc, stderr);
      for (k = 0; k < game->depth; k++)
	gprintf("%1m ", game->move_history[k]);
      gprintf("\n");
      for (pos = BOARDMIN; pos < BOARDMAX; pos++)
	if (mc->board[pos] == EMPTY) {
	  gprintf("%1m ", pos);
	  fprintf(stderr, "white %7d black %7d white near %7d black near %7d\n",
		  (int) mc->move_values_white[pos],
		  (int) mc->move_values_black[pos],
		  mc_patterns.values[mc_find_pattern_number(mc, pos, WHITE, 1)],
		  mc_patterns.values[mc_find_pattern_number(mc, pos, BLACK, 1)]);
	}
    }
    return PASS_MOVE;
  }

  if (color == WHITE) {
    move_values = mc->move_values_white;
    partition_sums = mc->partitioned_move_value_sums_white;
    partition_lists = mc->move_partition_lists_white;
    move_value_sum = &mc->move_value_sum_white;
  }
  else {
    move_values = mc->move_values_black;
    partition_sums = mc->partitioned_move_value_sums_black;
    partition_lists = mc->move_partition_lists_black;
    move_value_sum = &mc->move_value_sum_black;
  }
  
  /* Temporarily update pattern values for NEAR moves. We define
   * NEAR moves as those which had their values updated during the
   * previous mc_play_move() call and can be found by traversing the
   * update queue.
   */
  num_near_moves = 0;
  if (last_move != PASS_MOVE) {
    for (pos = mc->queue[0]; pos != 1; pos = mc->queue[pos]) {
      if (mc->board[pos] == EMPTY && partition_lists[pos] != 0) {
	unsigned int old_value = move_values[pos];
	int pattern = mc_find_pattern_number(mc, pos, color, 1);
	unsigned int new_value = mc_patterns.values[pattern];
	partition = pos & (NUM_MOVE_PARTITIONS - 1);
	saved_near_move_values[num_near_moves] = old_value;
	near_moves[num_near_moves++] = pos;
	move_values[pos] = new_value;
	partition_sums[partition] += new_value - old_value;
	  *move_value_sum += new_value - old_value;
      }
    }
  }
  
  /* Temporarily clear the move value of an illegal ko capture. */
  if (mc->board_ko_pos != NO_MOVE) {
    if (mc->board[WEST(mc->board_ko_pos)] == OTHER_COLOR(color)
	|| mc->board[EAST(mc->board_ko_pos)] == OTHER_COLOR(color)) {
      partition = mc->board_ko_pos & (NUM_MOVE_PARTITIONS - 1);
      saved_ko_value = move_values[mc->board_ko_pos];
      move_values[mc->board_ko_pos] = 0;
      partition_sums[partition] -= saved_ko_value;
      *move_value_sum -= saved_ko_value;
    }
  }
  
  /* Sample a move randomly according to the distribution given by
   * the move values.
   */
  if (*move_value_sum == 0)
    move = PASS_MOVE;
  else {
    /* First choose a partition. */
    x = (int) (gg_drand() * *move_value_sum);
    for (k = 0; k < NUM_MOVE_PARTITIONS; k++) {
      x -= partition_sums[k];
      if (x < 0)
	break;
    }

    /* Then choose a move in that partition. */
    x = (unsigned int) (gg_drand() * partition_sums[k]);
    for (pos = partition_lists[k]; pos != 1; pos = partition_lists[pos]) {
      x -= move_values[pos];
      if (x < 0)
	break;
    }

    move = pos;
#if !TURN_OFF_ASSERTIONS
    ASSERT1(move == PASS_MOVE || move_values[move] > 0, move);
    ASSERT1(move == PASS_MOVE || mc->board[move] == EMPTY, move);
    ASSERT1(mc_is_legal(mc, move, color), move);
#endif
  }

  /* Reset the value of an illegal ko capture. */
  if (saved_ko_value > 0) {
    partition = mc->board_ko_pos & (NUM_MOVE_PARTITIONS - 1);
    partition_sums[partition] += saved_ko_value - move_values[mc->board_ko_pos];
    *move_value_sum += saved_ko_value - move_values[mc->board_ko_pos];
    move_values[mc->board_ko_pos] = saved_ko_value;
  }
  
  /* Reset move values for NEAR moves. */
  for (k = 0; k < num_near_moves; k++) {
    unsigned int old_value;
    unsigned int new_value;
    pos = near_moves[k];
    partition = pos & (NUM_MOVE_PARTITIONS - 1);
    old_value = move_values[pos];
    new_value = saved_near_move_values[k];
    move_values[pos] = new_value;
    partition_sums[partition] += new_value - old_value;
    *move_value_sum += new_value - old_value;
  }

  return move;
}


static int mc_play_random_move(struct mc_game *game, int move)
{
  int result = mc_play_move(&game->mc, move, game->color_to_move);
  mc_update_move_values(&game->mc);
  
  if (result) {
    if (is_pass(move))
      game->consecutive_passes++;
    else {
      game->consecutive_passes = 0;
    }
      
    if (game->mc.board_ko_pos != NO_MOVE)
      game->consecutive_ko_captures++;
    else
      game->consecutive_ko_captures = 0;

    game->move_history[game->depth] = move;
    game->last_move = move;
    game->color_to_move = OTHER_COLOR(game->color_to_move);
    game->depth++;
  }
    
  return result;
}

static int mc_play_random_game(struct mc_game *game)
{
  struct mc_board *mc = &game->mc;
  
  int score = 0;
  int pos;
  int k;
  int result;
  int move;

  /* First finish the game, if it isn't already. */
  while (game->consecutive_passes < 3) {
    move = mc_generate_random_move(game);
    result = mc_play_random_move(game, move);
    ASSERT1(result, move);
  }

  for (pos = BOARDMIN; pos < BOARDMAX; pos++)
    if (MC_ON_BOARD(pos)) {
      if (game->settled[pos] == WHITE)
	score++;
      else if (game->settled[pos] == BLACK)
	score--;
      else {
	int pos2 = pos;
	if (mc->board[pos] == EMPTY)
	  for (k = 0; k < 4; k++) {
	    pos2 = pos + delta[k];
	    if (IS_STONE(mc->board[pos2]))
	      break;
	  }
	
	score += 2 * (mc->board[pos2] == WHITE) - 1;
      }
    }

  return score;
}

/******************* UCT search ***********************/

#define UCT_MAX_SEARCH_DEPTH BOARDMAX

struct bitboard {
  /* FIXME: Do this properly. */
  unsigned int bits[1 + BOARDMAX / 32];
};

struct uct_arc {
  int move;
  struct uct_node *node;
  struct uct_arc *next;
};

struct uct_node {
  int wins;
  int games;
  float sum_scores;
  float sum_scores2;
  struct uct_arc *child;
  struct bitboard untested;
  Hash_data boardhash;
};

struct uct_tree {
  struct uct_node *nodes;
  struct uct_arc *arcs;
  unsigned int *hashtable_odd;
  unsigned int *hashtable_even;
  unsigned int hashtable_size;
  int num_nodes;
  int num_used_nodes;
  int num_arcs;
  int num_used_arcs;
  int *forbidden_moves;
  struct mc_game game;
  int move_score[BOARDSIZE];
  int move_ordering[BOARDSIZE];
  int inverse_move_ordering[BOARDSIZE];
  int num_ordered_moves;
};


static struct uct_node *
uct_init_node(struct uct_tree *tree, int *allowed_moves)
{
  int pos;
  struct uct_node *node = &tree->nodes[tree->num_used_nodes++];

  node->wins = 0;
  node->games = 0;
  node->sum_scores = 0.0;
  node->sum_scores2 = 0.0;
  node->child = NULL;
  memset(node->untested.bits, 0, sizeof(node->untested.bits));
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (tree->game.mc.board[pos] == EMPTY
	&& !tree->forbidden_moves[pos]
	&& (!allowed_moves || allowed_moves[pos])) {
      node->untested.bits[pos / 32] |= 1 << pos % 32;
    }
  }
  node->boardhash = tree->game.mc.hash;

  return node;
}

static struct uct_node *
uct_find_node(struct uct_tree *tree, struct uct_node *parent, int move)
{
  struct uct_node *node = NULL;
  Hash_data *boardhash = &tree->game.mc.hash;
  unsigned int hash_index = hashdata_remainder(*boardhash,
					       tree->hashtable_size);
  unsigned int *hashtable = tree->hashtable_even;
  if (tree->game.depth & 1)
    hashtable = tree->hashtable_odd;

  while (hashtable[hash_index] != 0) {
    int node_index = hashtable[hash_index];
    gg_assert(node_index > 0 && node_index < tree->num_nodes);
    if (hashdata_is_equal(tree->nodes[node_index].boardhash, *boardhash)) {
      node = &tree->nodes[node_index];
      break;
    }
    hash_index++;
    if (hash_index >= tree->hashtable_size)
      hash_index = 0;
  }

  if (!node) {
    node = uct_init_node(tree, NULL);
    gg_assert(hash_index < tree->hashtable_size);
    hashtable[hash_index] = node - tree->nodes;
  }
  
  /* Add the node as the first of the siblings. */
  if (parent) {
    struct uct_arc *arc = &tree->arcs[tree->num_used_arcs++];
    gg_assert(tree->num_used_arcs < tree->num_arcs);
    arc->move = move;
    arc->node = node;
    if (parent->child)
      arc->next = parent->child;
    else
      arc->next = NULL;
    parent->child = arc;
  }

  return node;
}


static void
uct_update_move_ordering(struct uct_tree *tree, int move)
{
  int score = ++tree->move_score[move];
  while (1) {
    int n = tree->inverse_move_ordering[move];
    int preceding_move;
    if (n == 0)
      return;
    preceding_move = tree->move_ordering[n - 1];
    if (tree->move_score[preceding_move] >= score)
      return;

    /* Swap move ordering. */
    tree->move_ordering[n - 1] = move;
    tree->move_ordering[n] = preceding_move;
    tree->inverse_move_ordering[move] = n - 1;
    tree->inverse_move_ordering[preceding_move] = n;
  }
}


static void
uct_init_move_ordering(struct uct_tree *tree)
{
  int pos;
  int k = 0;
  /* FIXME: Exclude forbidden moves. */
  memset(tree->move_score, 0, sizeof(tree->move_score));
  for (pos = BOARDMIN; pos < BOARDMAX; pos++)
    if (ON_BOARD(pos)) {
      tree->move_ordering[k] = pos;
      tree->inverse_move_ordering[pos] = k;
      k++;
    }
  
  tree->num_ordered_moves = k;

  /* FIXME: Quick and dirty experiment. */
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (ON_BOARD(pos)) {
      tree->move_score[pos] = (int) (10 * potential_moves[pos]) - 1;
      uct_update_move_ordering(tree, pos);
    }
  }
}


static float
uct_finish_and_score_game(struct mc_game *game)
{
  return komi + mc_play_random_game(game);
}

static struct uct_node *
uct_play_move(struct uct_tree *tree, struct uct_node *node, float alpha,
	      float *gamma, int *move)
{
  struct uct_arc *child_arc;
  int pos;
  struct uct_arc *next_arc = NULL;
  struct uct_arc *best_winrate_arc = NULL;
  float best_uct_value = 0.0;
  float best_winrate = 0.0;
  
  for (child_arc = node->child; child_arc; child_arc = child_arc->next) {
    struct uct_node *child_node = child_arc->node;
    float winrate = (float) child_node->wins / child_node->games;
    float uct_value;
    float log_games_ratio = log(node->games) / child_node->games;
    float x = winrate * (1.0 - winrate) + sqrt(2.0 * log_games_ratio);
    if (x < 0.25)
      x = 0.25;
    uct_value = winrate + sqrt(2 * log_games_ratio * x / (1 + tree->game.depth));
    if (uct_value > best_uct_value) {
      next_arc = child_arc;
      best_uct_value = uct_value;
    }

    if (winrate > best_winrate) {
      best_winrate_arc = child_arc;
      best_winrate = winrate;
    }
  }

  *gamma = best_winrate;
  if (best_winrate > alpha)
    next_arc = best_winrate_arc;
  else {
    /* First play a random previously unplayed move, if any. */
    int k;
    for (k = -1; k < tree->num_ordered_moves; k++) {
      if (k == -1 && best_uct_value > 0.0)
	continue;
      else if (k == -1)
	pos = mc_generate_random_move(&tree->game);
      else
	pos = tree->move_ordering[k];
      
      if (node->untested.bits[pos / 32] & (1 << (pos % 32))) {
	int r;
	int proper_small_eye = 1;
	struct mc_board *mc = &tree->game.mc;
	*move = pos;
	node->untested.bits[*move / 32] &= ~(1 << *move % 32);

	for (r = 0; r < 4; r++) {
	  if (mc->board[pos + delta[r]] == EMPTY
	      || mc->board[pos + delta[r]] == OTHER_COLOR(tree->game.color_to_move)) {
	    proper_small_eye = 0;
	    break;
	  }
	}
	
	if (proper_small_eye) {
	  int diagonal_value = 0;
	  for (r = 4; r < 8; r++) {
	    int pos2 = pos + delta[r];
	    if (!MC_ON_BOARD(pos2))
	      diagonal_value++;
	    else if (mc->board[pos2] == OTHER_COLOR(tree->game.color_to_move))
	      diagonal_value += 2;
	  }
	  if (diagonal_value > 3)
	    proper_small_eye = 0;
	}
	
	if (!proper_small_eye && mc_play_random_move(&tree->game, *move))
	  return uct_find_node(tree, node, *move);
      }
    }
  }
  
  if (!next_arc) {
    mc_play_random_move(&tree->game, PASS_MOVE);
    *move = PASS_MOVE;
    return uct_find_node(tree, node, PASS_MOVE);
  }

  *move = next_arc->move;
  mc_play_random_move(&tree->game, next_arc->move);
  
  return next_arc->node;
}

static float
uct_traverse_tree(struct uct_tree *tree, struct uct_node *node,
		  float alpha, float beta)
{
  int color = tree->game.color_to_move;
  int num_passes = tree->game.consecutive_passes;
  float result;
  float gamma;
  int move = PASS_MOVE;
  
  /* FIXME: Unify these. */
  if (num_passes == 3 || tree->game.depth >= UCT_MAX_SEARCH_DEPTH
      || (node->games == 0 && node != tree->nodes))
    result = uct_finish_and_score_game(&tree->game);
  else {
    struct uct_node *next_node;
    next_node = uct_play_move(tree, node, alpha, &gamma, &move);
    
    gamma += 0.00;
    if (gamma > 0.8)
      gamma = 0.8;
    result = uct_traverse_tree(tree, next_node, beta, gamma);
  }

  node->games++;
  if ((result > 0) ^ (color == WHITE)) {
    node->wins++;
    if (move != PASS_MOVE)
      uct_update_move_ordering(tree, move);
  }

  node->sum_scores += result;
  node->sum_scores2 += result * result;
  
  return result;
}

static int
uct_find_best_children(struct uct_node *node, struct uct_arc **children,
		       int n)
{
  struct uct_arc *child_arc;
  float best_score;
  struct uct_arc *best_child;
  int found_moves[BOARDMAX];
  int k;

  memset(found_moves, 0, sizeof(found_moves));
  for (k = 0; k < n; k++) {
    best_score = 0.0;
    best_child = NULL;
    for (child_arc = node->child; child_arc; child_arc = child_arc->next) {
      struct uct_node *child_node = child_arc->node;
      if (!found_moves[child_arc->move]
	  && best_score * child_node->games < child_node->wins) {
	best_child = child_arc;
	best_score = (float) child_node->wins / child_node->games;
      }
    }
    if (!best_child)
      break;
    children[k] = best_child;
    found_moves[best_child->move] = 1;
  }

  return k;
}

static void
uct_dump_tree_recursive(struct uct_node *node, SGFTree *sgf_tree, int color,
			int cutoff, int depth)
{
  struct uct_arc *child_arc;
  char buf[100];
  if (depth > 50)
    return;
  for (child_arc = node->child; child_arc; child_arc = child_arc->next) {
    struct uct_node *child_node = child_arc->node;
    sgftreeAddPlayLast(sgf_tree, color,
		       I(child_arc->move), J(child_arc->move));
    gg_snprintf(buf, 100, "%d/%d (%5.3f)", child_node->wins, child_node->games,
		(float) child_node->wins / child_node->games);
    sgftreeAddComment(sgf_tree, buf);
    if (child_node->games >= cutoff)
      uct_dump_tree_recursive(child_node, sgf_tree, OTHER_COLOR(color), cutoff, depth + 1);
    sgf_tree->lastnode = sgf_tree->lastnode->parent;
  }
}


static void
uct_dump_tree(struct uct_tree *tree, const char *filename, int color,
	      int cutoff)
{
  SGFTree sgf_tree;
  sgftree_clear(&sgf_tree);
  sgftreeCreateHeaderNode(&sgf_tree, board_size, komi, 0);
  sgffile_printboard(&sgf_tree);

  uct_dump_tree_recursive(&tree->nodes[0], &sgf_tree, color, cutoff, 0);
  
  writesgf(sgf_tree.root, filename);
  sgfFreeNode(sgf_tree.root);
}


void
uct_genmove(int color, int *move, int *forbidden_moves, int *allowed_moves,
	    int nodes, float *move_values, int *move_frequencies)
{
  struct uct_tree tree;
  float best_score;
  struct uct_arc *arc;
  struct uct_node *node;
  struct mc_game starting_position;
  int most_games;
  struct uct_node *most_games_node;
  struct uct_arc *most_games_arc;
  int pos;

  mc_init_board_from_global_board(&starting_position.mc);
  mc_init_move_values(&starting_position.mc);
  starting_position.color_to_move = color;
  /* FIXME: Fill in correct information. */
  starting_position.consecutive_passes = 0;
  starting_position.consecutive_ko_captures = 0;
  starting_position.last_move = get_last_move();
  starting_position.depth = 0;
  for (pos = BOARDMIN; pos < BOARDMAX; pos++)
    starting_position.settled[pos] = forbidden_moves[pos];

  tree.game = starting_position;
  /* FIXME: Don't reallocate between moves. */
  tree.nodes = malloc(nodes * sizeof(*tree.nodes));
  gg_assert(tree.nodes);
  tree.arcs = malloc(nodes * sizeof(*tree.arcs));
  gg_assert(tree.arcs);
  tree.hashtable_size = nodes;
  tree.hashtable_odd = calloc(tree.hashtable_size,
			      sizeof(*tree.hashtable_odd));
  tree.hashtable_even = calloc(tree.hashtable_size,
			       sizeof(*tree.hashtable_even));
  gg_assert(tree.hashtable_odd);
  gg_assert(tree.hashtable_even);
  tree.num_nodes = nodes;
  tree.num_arcs = nodes;
  tree.num_used_nodes = 0;
  tree.num_used_arcs = 0;
  tree.forbidden_moves = forbidden_moves;
  uct_init_node(&tree, allowed_moves);
  uct_init_move_ordering(&tree);

  /* Play simulations. FIXME: Terribly dirty fix. */
  while (tree.num_used_arcs < tree.num_arcs - 10) {
    int last_used_arcs = tree.num_used_arcs;
    tree.game = starting_position;
    uct_traverse_tree(&tree, &tree.nodes[0], 1.0, 0.9);
    /* FIXME: Ugly workaround for solved positions before running out
     * of nodes.
     */
    if (tree.num_used_arcs == last_used_arcs)
      break;
  }

  /* Identify the best move on the top level. */
  best_score = 0.0;
  *move = PASS_MOVE;
  for (arc = tree.nodes[0].child; arc; arc = arc->next) {
    node = arc->node;
    move_frequencies[arc->move] = node->games;
    move_values[arc->move] = (float) node->wins / node->games;
    if (best_score * node->games < node->wins) {
      *move = arc->move;
      best_score = (float) node->wins / node->games;
    }
  }

  /* Dump sgf tree of the significant part of the search tree. */
  if (0)
    uct_dump_tree(&tree, "/tmp/ucttree.sgf", color, 50);
    
  /* Print information about the search tree. */
  if (mc_debug) {
    while (1) {
      float mean;
      float std;
      
      most_games = 0;
      most_games_node = NULL;
      most_games_arc = NULL;
      
      for (arc = tree.nodes[0].child; arc; arc = arc->next) {
	node = arc->node;
	if (most_games < node->games) {
	  most_games = node->games;
	  most_games_node = node;
	  most_games_arc = arc;
	}
      }
      
      if (most_games == 0)
	break;
      
      mean = most_games_node->sum_scores / most_games_node->games;
      std = sqrt((most_games_node->sum_scores2 - most_games_node->sum_scores * mean) / (most_games_node->games - 1));
      gprintf("%1m ", most_games_arc->move);
      fprintf(stderr, "%6d %6d %5.3f %5.3f %5.3f %5.3f\n",
	      most_games_node->wins, most_games_node->games,
	      (float) most_games_node->wins / most_games_node->games,
	      mean, std, mean / (std + 0.001));
      most_games_node->games = -most_games_node->games;
    }
    for (arc = tree.nodes[0].child; arc; arc = arc->next)
      arc->node->games = -arc->node->games;
    
    {
      int n;
      struct uct_arc *arcs[7];
      int depth = 0;
      n = uct_find_best_children(&tree.nodes[0], arcs, 7);
      gprintf("Principal variation:\n");
      while (n > 0 && depth < 80) {
	int k;
	gprintf("%C ", color);
	for (k = 0; k < n; k++) {
	  node = arcs[k]->node;
	  gprintf("%1m ", arcs[k]->move);
	  fprintf(stderr, "%5.3f", (float) node->wins / node->games);
	  if (k == 0)
	    gprintf(" (%d games)", node->games);
	  if (k < n - 1)
	    gprintf(", ");
	}
	gprintf("\n");
	color = OTHER_COLOR(color);
	n = uct_find_best_children(arcs[0]->node, arcs, 7);
	depth++;
      }
      gprintf("\n");
    }
  }
  
  free(tree.nodes);
  free(tree.arcs);
  free(tree.hashtable_odd);
  free(tree.hashtable_even);
}


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
