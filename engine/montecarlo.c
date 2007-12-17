/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * This is GNU Go, a Go program. Contact gnugo@gnu.org, or see       *
 * http://www.gnu.org/software/gnugo/ for more information.          *
 *                                                                   *
 * Copyright 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006 and 2007 *
 * by the Free Software Foundation.                                  *
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

#define CACHE_CAPTURE_MOVES 1

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
 */


struct mc_board {
  Intersection board[BOARDSIZE];
  int board_ko_pos;
  int reference_stone[BOARDMAX];
  int next_stone[BOARDMAX];
  int first_liberty_edge[BOARDMAX];
  int previous_liberty_edge[4 * BOARDMAX];
  int next_liberty_edge[4 * BOARDMAX];
  Hash_data hash;
};

#define MC_ON_BOARD(pos) (mc->board[pos] != GRAY)

/* FIXME: In principle same a MAXCHAIN but we don't need to care about
 * such high numbers.
 */
#define MAX_NEIGHBORS 20

/* Add a liberty edge for a string at pos with liberty at lib and
 * direction dir.
 */
static void
mc_add_liberty_edge(struct mc_board *mc, int pos, int lib, int dir)
{
  int this_liberty_edge = (lib << 2) | dir;
  int reference = mc->reference_stone[pos];
  int first_liberty_edge = mc->first_liberty_edge[reference];

  gg_assert(lib + delta[dir] == pos);
  
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
  
  gg_assert(lib + delta[dir] == pos);
  
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
    for (k = 0; k < 4; k++)
      if (mc->board[pos + delta[k]] == other)
	mc_add_liberty_edge(mc, pos + delta[k], pos, k);
    mc->board[pos] = EMPTY;
    hashdata_invert_stone(&(mc->hash), pos, color);
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

  memset(mc->next_stone, 0, sizeof(mc->next_stone));
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
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


/* Write the Monte Carlo board to outfile.
 */
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

/* Is a move at pos by color suicide? */
static int
mc_is_suicide(struct mc_board *mc, int pos, int color)
{
  int k;
  
  if (mc->board[SOUTH(pos)] == EMPTY
      || mc->board[WEST(pos)] == EMPTY
      || mc->board[NORTH(pos)] == EMPTY
      || mc->board[EAST(pos)] == EMPTY)
    return 0;

  for (k = 0; k < 4; k++) {
    int first_liberty_edge;
    int liberty_edge;
    int additional_liberty = 0;
    if (!ON_BOARD(pos + delta[k]))
      continue;

    first_liberty_edge = (pos << 2) | k;
    liberty_edge = mc->next_liberty_edge[first_liberty_edge];
    while (liberty_edge != first_liberty_edge) {
      if ((liberty_edge >> 2) != pos) {
	additional_liberty = 1;
	break;
      }
      liberty_edge = mc->next_liberty_edge[liberty_edge];
    }

    if ((mc->board[pos + delta[k]] != color) ^ additional_liberty)
      return 0;
  }

  return 1;
}


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
  ASSERT1(IS_STONE(mc->board[str]), str);
  if (lib)
    *lib = liberty;
  while (liberty_edge != first_liberty_edge) {
    if ((liberty_edge >> 2) != liberty)
      return 0;
    liberty_edge = mc->next_liberty_edge[liberty_edge];
  }

  return 1;
}


/* Does the string at str have exactly two liberties? Place those in
 * lib[0] and lib[1] unless lib is a NULL pointer. 
 */
static int
mc_has_two_liberties(struct mc_board *mc, int str, int *lib)
{
  int reference = mc->reference_stone[str];
  int first_liberty_edge = mc->first_liberty_edge[reference];
  int first_liberty = first_liberty_edge >> 2;
  int liberty_edge = mc->next_liberty_edge[first_liberty_edge];
  int second_liberty;
  ASSERT1(IS_STONE(mc->board[str]), str);
  if (lib)
    lib[0] = first_liberty;
  while (liberty_edge != first_liberty_edge) {
    if ((liberty_edge >> 2) != first_liberty) {
      second_liberty = liberty_edge >> 2;
      if (lib)
	lib[1] = second_liberty;
      while (liberty_edge != first_liberty_edge) {
	if ((liberty_edge >> 2) != first_liberty
	    && (liberty_edge >> 2) != second_liberty)
	  return 0;
	liberty_edge = mc->next_liberty_edge[liberty_edge];
      }
      return 1;
    }
    liberty_edge = mc->next_liberty_edge[liberty_edge];
  }

  return 0;
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


/* Does the string at str have one or more neighbors in atari? If so,
 * return moves to capture a neighbor.
 * FIXME: Does it pay off the keep a string mark array in the mc_board
 *        struct?
 */
static int
mc_break_chain(struct mc_board *mc, int str, int *moves, int max_moves,
	       int include_ko_recapture)
{
  unsigned char found_neighbors[BOARDMAX];
  int pos = str;
  int other = OTHER_COLOR(mc->board[str]);
  int num_moves = 0;
  int move;
  ASSERT1(IS_STONE(mc->board[str]), str);
  memset(found_neighbors, 0, sizeof(found_neighbors));
  do {
    int k;
    for (k = 0; k < 4; k++) {
      if (mc->board[pos + delta[k]] == other) {
	int reference = mc->reference_stone[pos + delta[k]];
	if (!found_neighbors[reference]) {
	  found_neighbors[reference] = 1;
	  if (mc_is_in_atari(mc, pos + delta[k], &move)
	      && (include_ko_recapture
		  || move != mc->board_ko_pos)) {
	    moves[num_moves++] = move;
	    if (num_moves == max_moves)
	      return num_moves;
	  }
	}
      }
    }
    pos = mc->next_stone[pos];
  } while (pos != str);

  return num_moves;
}


/* Play the move at pos by color. */
static int
mc_play_move(struct mc_board *mc, int pos, int color)
{
  int k;
  int captured_stones = 0;
  int num_direct_liberties = 0;
  
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

  ASSERT1(mc->board[pos] == EMPTY, pos);
  mc->board[pos] = color;
  hashdata_invert_stone(&mc->hash, pos, color);
  mc->next_stone[pos] = pos;
  mc->reference_stone[pos] = pos;
  mc->first_liberty_edge[pos] = 0;

  for (k = 0; k < 4; k++) {
    int pos2 = pos + delta[k];
    if (mc->board[pos2] == EMPTY) {
      mc_add_liberty_edge(mc, pos, pos2, (k + 2) % 4);
      num_direct_liberties++;
    }
  }
  
  for (k = 0; k < 4; k++) {
    int pos2 = pos + delta[k];
    if (mc->board[pos2] == OTHER_COLOR(color)) {
      if (mc_remove_liberty_edge(mc, pos2, pos, k) == 0)
	captured_stones += mc_remove_string(mc, pos2);
    }
    else if (mc->board[pos2] == color) {
      if (mc->reference_stone[pos] != mc->reference_stone[pos2])
	mc_join_strings(mc, pos, pos2);
      mc_remove_liberty_edge(mc, pos2, pos, k);
    }
  }

  if (captured_stones == 1
      && mc->next_stone[pos] == pos
      && num_direct_liberties == 0) {
    mc->board_ko_pos = mc->first_liberty_edge[pos] >> 2;
    hashdata_invert_ko(&mc->hash, mc->board_ko_pos);
  }
  
  return 1;
}


/***************************************************/

#define ASSERT_LEGAL 1

struct mc_game {
  struct mc_board mc;
  unsigned char settled[BOARDMAX];
  int color_to_move;
  int last_move;
  int consecutive_passes;
  int consecutive_ko_captures;
  int depth;
};


/* Generate a random move. */
static int
mc_generate_random_move(struct mc_game *game, int only_reactive_moves)
{
  struct mc_board *mc = &game->mc;
  int last_move = game->last_move;
  int color = game->color_to_move;
  int depth = game->depth;
  
  int moves[MAX_BOARD * MAX_BOARD];
  int num_moves = 0;
  int pos;
  int k;
  int offset;
  int move;
  int liberties[2];

  /* If we get this deep we are almost certainly playing triple ko
   * without alternative options, so just give up and score as is.
   *
   * FIXME: Handle this in some proper way.
   */
  if (depth > 600) {
    if (mc_debug) {
      fprintf(stderr, "Reached 600 iterations.\n");
      mc_showboard(mc, stderr);
    }
    return PASS_MOVE;
  }

  /* Play reactive moves, aiming to reduce the Monte-Carlo tendency to
   * fear cuts.
   */
  if (last_move != PASS_MOVE && game->settled[last_move] == EMPTY) {
    /* If the opponent played self-atari, with high probability capture. */
    if (mc_is_in_atari(mc, last_move, &move)
	&& move != mc->board_ko_pos
	&& gg_drand() < 0.75 + 0.1 * mc_countstones(mc, last_move, 4)) {
#if ASSERT_LEGAL
      ASSERT1(mc_is_legal(mc, move, color), move);
#endif
      return move;
    }
    
    /* If the opponent played an atari, try to run away or capture out
     * of atari.
     */
    offset = gg_urand() % 4;
    for (k = 0; k < 4; k++) {
      int neighbor = last_move + delta[(k + offset) % 4];
      if (mc->board[neighbor] == color
	  && mc_is_in_atari(mc, neighbor, &move) == 1
	  && gg_drand() < (0.7 + 0.1 * (mc_countstones(mc, neighbor, 4) - 1))) {
	if (!mc_is_self_atari(mc, move, color)) {
#if ASSERT_LEGAL
	  ASSERT1(mc_is_legal(mc, move, color), move);
#endif
	  return move;
	}
	else if (mc_break_chain(mc, neighbor, &move, 1, 0)) {
#if ASSERT_LEGAL
	  ASSERT1(mc_is_legal(mc, move, color), move);
#endif
	  return move;
	}
      }
    }

    /* If the opponent played a string with two liberties, try to
     * capture with a ladder. (Don't actually check this, just play as
     * if it works.)
     */
    if (mc_has_two_liberties(mc, last_move, liberties)
      	&& gg_drand() < 0.77 + 0.1 * mc_countstones(mc, last_move, 4)) {
      int first_liberty_score = 0;
      int second_liberty_score = 0;
      
      for (k = 0; k < 4; k++) {
	int neighbor = mc->board[liberties[0] + delta[k]];
	if (neighbor == EMPTY)
	  first_liberty_score += 2;
	else if (neighbor == OTHER_COLOR(color))
	  first_liberty_score += 2;
	else if (neighbor != color)
	  first_liberty_score -= 1;
      }
      
      for (k = 0; k < 4; k++) {
	int neighbor = mc->board[liberties[1] + delta[k]];
	if (neighbor == EMPTY)
	  second_liberty_score += 2;
	else if (neighbor == OTHER_COLOR(color))
	  second_liberty_score += 2;
	else if (neighbor != color)
	  second_liberty_score -= 1;
      }

      if (first_liberty_score < 3
	  && liberties[1] != mc->board_ko_pos
	  && ((first_liberty_score < second_liberty_score)
	      || (first_liberty_score == second_liberty_score
		  && gg_drand() < 0.5))
	  && !mc_is_self_atari(mc, liberties[1], color)) {
#if ASSERT_LEGAL
	ASSERT1(mc_is_legal(mc, liberties[1], color), liberties[1]);
#endif
	return liberties[1];
      }

      if (second_liberty_score < 3
	  && liberties[0] != mc->board_ko_pos
	  && second_liberty_score <= first_liberty_score
	  && !mc_is_self_atari(mc, liberties[0], color)) {
#if ASSERT_LEGAL
	ASSERT1(mc_is_legal(mc, liberties[0], color), liberties[0]);
#endif
	return liberties[0];
      }
    }

#if 0
    /* Connect and cut.
     *
     *  ?Y?
     *  O*O
     *  ?X?
     */
    for (k = 0; k < 4; k++) {
      int down = delta[(k + offset) % 4];
      int right = delta[(k + 1 + offset) % 4];
      if (mc->board[last_move + down] == EMPTY
	  && (mc->board[last_move + down + right] == color
	      || !MC_ON_BOARD(last_move + down + right))
	  && (mc->board[last_move + down - right] == color
	      || !MC_ON_BOARD(last_move + down - right))) {
	if ((mc->board[last_move + down + down] == OTHER_COLOR(color)
	     || !MC_ON_BOARD(last_move + down + down)
	     || (mc->board[last_move + down + down] == color
		 && mc_has_two_liberties(mc, last_move + down + down, NULL)))
	    && gg_drand() < 0.9
	    && !mc_is_self_atari(mc, last_move + down, color)) {
#if ASSERT_LEGAL
	  if (!mc_is_legal(mc, last_move + down, color))
	    mc_showboard(mc, stderr);
	  ASSERT1(mc_is_legal(mc, last_move + down, color), last_move + down);
#endif
	  return last_move + down;
	}
      }
    }
#endif

    /* If the opponent might cut, connect or at least try to connect.
     *
     *  oYo
     *  o*o
     */
    for (k = 0; k < 4; k++) {
      int down = delta[(k + offset) % 4];
      int right = delta[(k + 1 + offset) % 4];
      if (mc->board[last_move + down] == EMPTY) {
	int a = 0;
	int b = 0;
	if ((mc->board[last_move + down + right] == color
	     || !MC_ON_BOARD(last_move + down + right)))
	  a = 1;
	else if (mc->board[last_move + right] == color
		 || !MC_ON_BOARD(last_move + right))
	  a = 2;
	if (mc->board[last_move + down - right] == color
	    || !MC_ON_BOARD(last_move + down - right))
	  b = 1;
	else if (mc->board[last_move - right] == color
		 || !MC_ON_BOARD(last_move - right))
	  b = 2;

	if (a > 0 && b > 0 && a + b < 4
	    && gg_drand() < 0.6
	    && last_move + down != mc->board_ko_pos
	    && !mc_is_self_atari(mc, last_move + down, color)) {
#if ASSERT_LEGAL
	  ASSERT1(mc_is_legal(mc, last_move + down, color), last_move + down);
#endif
	  return last_move + down;
	}
      }
    }

    /* If the opponent invited a cut, do cut.
     *
     *   Y
     *  o*O
     *   X
     */
    for (k = 0; k < 4; k++) {
      int down = delta[(k + offset) % 4];
      int right = delta[(k + 1 + offset) % 4];
      int other = OTHER_COLOR(color);
      if (mc->board[last_move + down] == EMPTY
	  && mc->board[last_move + down + down] == other) {
	if ((mc->board[last_move + down + right] == color
	     || mc->board[last_move + down - right] == color)
	    && mc->board[last_move + down + right] != other
	    && mc->board[last_move + down - right] != other
	    && gg_drand() < 0.5
	    && !mc_is_self_atari(mc, last_move + down, color)) {
#if ASSERT_LEGAL
	  ASSERT1(mc_is_legal(mc, last_move + down, color), last_move + down);
#endif
	  return last_move + down;
	}
      }
    }

    /* Crosscut.
     *
     *  YO
     * o*X
     *  o
     */
    for (k = 0; k < 4; k++) {
      int down = delta[(k + offset) % 4];
      int right = delta[(k + 1 + offset) % 4];
      int other = OTHER_COLOR(color);
      if (mc->board[last_move + down] == EMPTY
	  && mc->board[last_move + down + down] != other) {
	if (((mc->board[last_move + right] == color
	      && mc->board[last_move + down + right] == other
	      && mc->board[last_move + down - right] != other)
	     || (mc->board[last_move - right] == color
	      && mc->board[last_move + down - right] == other
		 && mc->board[last_move + down + right] != other))
	    && last_move + down != mc->board_ko_pos
	    && gg_drand() < 0.3
	    && !mc_is_self_atari(mc, last_move + down, color)) {
#if ASSERT_LEGAL
	  ASSERT1(mc_is_legal(mc, last_move + down, color), last_move + down);
#endif
	  return last_move + down;
	}
      }
    }

#if 0
    /* Block.
     *
     *  Y?
     *  *O
     *  ..
     */
    for (k = 0; k < 4; k++) {
      int down = delta[(k + offset) % 4];
      int right = delta[(k + 1 + offset) % 4];
      int other = OTHER_COLOR(color);
      if (mc->board[last_move + down] == EMPTY
	  && mc->board[last_move + down + down] == EMPTY
	  && mc->board[last_move + down + down + right] == EMPTY
	  && mc->board[last_move + down + right] == color
	  && gg_drand() < 0.3) {
#if ASSERT_LEGAL
	ASSERT1(mc_is_legal(mc, last_move + down, color), last_move + down);
#endif
	return last_move + down;
      }
    }
#endif
  }
  

  /* Capturing stones is always fun. */
  if (1) {
    pos = BOARDMIN + gg_urand() % (BOARDMAX - BOARDMIN);
    for (k = 0; k < BOARDMAX - BOARDMIN; k++, pos++) {
      if (pos == BOARDMAX)
	pos = BOARDMIN;
      if (mc->board[pos] == OTHER_COLOR(color)
	  && mc->reference_stone[pos] == pos
	  && mc_is_in_atari(mc, pos, &move)
	  && mc->board_ko_pos != move
	  && gg_drand() < 0.15 + 0.1 * (mc_countstones(mc, pos, 5) - 1)) {
#if ASSERT_LEGAL
	ASSERT1(mc_is_legal(mc, move, color), move);
#endif
	return move;
      }
    }
  }
  else {
    pos = BOARDMIN + gg_urand() % (BOARDMAX - BOARDMIN);
    for (k = 0; k < BOARDMAX - BOARDMIN; k++, pos++) {
      if (pos == BOARDMAX)
	pos = BOARDMIN;
      if (mc->board[pos] == OTHER_COLOR(color)
	  && mc->reference_stone[pos] == pos
	  && mc_is_in_atari(mc, pos, &move)
	  && mc->board_ko_pos != move
	  && gg_drand() < 0.15 + 0.1 * (mc_countstones(mc, pos, 5) - 1)) {
#if ASSERT_LEGAL
	ASSERT1(mc_is_legal(mc, move, color), move);
#endif
	return move;
      }
      else if (mc->board[pos] == color
	       && mc->reference_stone[pos] == pos
	       && mc_is_in_atari(mc, pos, &move)
	       && gg_drand() < 0.15 + 0.1 * (mc_countstones(mc, pos, 5) - 1)
	       && !mc_is_self_atari(mc, move, color)) {
#if ASSERT_LEGAL
	ASSERT1(mc_is_legal(mc, move, color), move);
#endif
	return move;
      }
    }
  }      
    

  if (only_reactive_moves)
    return PASS_MOVE;
  
  /* Make ten attempts to find a move nearby the previous one. */
  if (last_move != PASS_MOVE && game->settled[last_move] == EMPTY) {
    for (k = 0; k < 10; k++) {
      int di = gg_rand() % 5 - 2;
      int dj = gg_rand() % 5 - 2;
      int i = I(last_move) + di;
      int j = J(last_move) + dj;
      int move = POS(i, j);
      if (ON_BOARD2(i, j)
	  && mc->board[move] == EMPTY
	  && !game->settled[move]
	  && mc_is_legal(mc, move, color)
	  && !mc_is_self_atari(mc, move, color)
	  && !mc_is_suicide(mc, move, OTHER_COLOR(color)))
	return move;
    }
  }

  /* Brown move generation follows below. */
  num_moves = 0;
  for (pos = BOARDMIN; pos < BOARDMAX; pos++)
    if (mc->board[pos] == EMPTY)
      moves[num_moves++] = pos;

  move = PASS_MOVE;
  while (num_moves > 0 && move == PASS_MOVE) {
    int index = gg_rand() % num_moves;
    pos = moves[index];
    /* Consider moving at pos if it is legal and not suicide. */
    if (game->settled[pos] == EMPTY
	&& mc_is_legal(mc, pos, color)
	&& !mc_is_suicide(mc, pos, color)) {
      /* Further require the move not to be suicide for the opponent... */
      if (!mc_is_suicide(mc, pos, OTHER_COLOR(color))) {
	move = pos;
	break;
      }
      else {
	/* ...however, if the move captures at least one stone,
	 * consider it anyway.
	 */
	for (k = 0; k < 4; k++) {
	  int pos2 = pos + delta[k];
	  if (mc->board[pos2] == OTHER_COLOR(color)) {
#if ASSERT_LEGAL
	    ASSERT1(mc_is_legal(mc, pos, color), pos);
#endif
	    move = pos;
	    break;
	  }
	}
      }
    }
    moves[index] = moves[num_moves - 1];
    num_moves--;
  }

  /* If the move seems to use eyespace inefficiently, try to move it
   * to a better neighbor.
   */
  if (1 && move != PASS_MOVE) {
    int not_own_neighbors = 0;
    for (k = 0; k < 4; k++) {
      pos = move + delta[k];
      if (mc->board[pos] == EMPTY
	  || mc->board[pos] == OTHER_COLOR(color))
	not_own_neighbors++;
    }

    if (not_own_neighbors == 1) {
      int offset = gg_rand() % 4;
      for (k = 0; k < 4; k++) {
	int r;
	int not_own_neighbors2 = 0;
	int own_neighbors2 = 0;
	pos = move + delta[(k + offset) % 4];
	if (mc->board[pos] == EMPTY) {
	  for (r = 0; r < 4; r++) {
	    int pos2 = pos + delta[r];
	    if (mc->board[pos2] == EMPTY
		|| mc->board[pos2] == OTHER_COLOR(color))
	      not_own_neighbors2++;
	    else if (mc->board[pos2] == color)
	      own_neighbors2++;
	  }
	  if (not_own_neighbors2 > 1
	      && own_neighbors2 > 0
	      && !mc_is_self_atari(mc, pos, color)) {
	    move = pos;
#if ASSERT_LEGAL
	    ASSERT1(mc_is_legal(mc, move, color), move);
#endif
	    break;
	  }
	}
      }
    }
  }

  /* If the move is a self atari, with high probability try to make a
   * backfilling move first.
   */
  if (move != PASS_MOVE) {
    if (mc_is_self_atari(mc, move, color)
	&& gg_drand() < 0.8) {
      int offset = gg_rand() % 4;
      for (k = 0; k < 4; k++) {
	int pos = move + delta[(k + offset) % 4];
	if (mc->board[pos] == EMPTY) {
	  move = pos;
#if ASSERT_LEGAL
	  ASSERT1(mc_is_legal(mc, move, color), move);
#endif
	  break;
	}
	else if (mc->board[pos] == color
		 && mc_has_two_liberties(mc, pos, liberties)) {
	  if (liberties[0] == move)
	    move = liberties[1];
	  else
	    move = liberties[0];
#if ASSERT_LEGAL
	  ASSERT1(mc_is_legal(mc, move, color), move);
#endif
	  break;
	}
      }
    }
  }
  
  return move;
}


static int mc_play_random_move(struct mc_game *game, int move)
{
  int result = mc_play_move(&game->mc, move, game->color_to_move);

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
    move = mc_generate_random_move(game, 0);
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
    float log_games_ratio = logf(node->games) / child_node->games;
    float x = winrate * (1.0 - winrate) + sqrtf(2.0 * log_games_ratio);
    if (x < 0.25)
      x = 0.25;
    uct_value = winrate + sqrtf(2 * log_games_ratio * x / (1 + tree->game.depth));
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
	pos = mc_generate_random_move(&tree->game, 1);
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
      std = sqrtf((most_games_node->sum_scores2 - most_games_node->sum_scores * mean) / (most_games_node->games - 1));
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
