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


/* The functions in this file implements a go board with incremental
 * update of strings and liberties.
 *
 * See the Texinfo documentation (Utility Functions: Incremental
 * Board) for an introduction.
 */


#include "board.h"
#include "board-private.h"
#include "hash.h"
#include "sgftree.h"
#include "gg_utils.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>



/* Macros declaring constant local pointers to various parts of
 * `Goban' structure.  The purpose is to save typing.
 */

#define ACCESS_PRIVATE_DATA						\
  Goban_private_data *const private = goban->private

#define ACCESS_PRIVATE_DATA_CONST					\
  const Goban_private_data *const private = goban->private

#define ACCESS_BOARD							\
  Intersection *const board = goban->board

#define ACCESS_BOARD_CONST						\
  const Intersection *const board = goban->board



/* Various stack accessory macros. */

#define CLEAR_STACKS()							\
  do {									\
    private->change_stack_pointer = private->change_stack;		\
    private->vertex_stack_pointer = private->vertex_stack;		\
    VALGRIND_MAKE_WRITABLE(private->change_stack,			\
			   sizeof private->change_stack);		\
    VALGRIND_MAKE_WRITABLE(private->vertex_stack,			\
			   sizeof private->vertex_stack);		\
  } while (0)

/* Begin a record: use `address == NULL' as mark. */
#define BEGIN_CHANGE_RECORD()						\
  ((private->change_stack_pointer++)->address = NULL,			\
   (private->vertex_stack_pointer++)->address = NULL)


/* Save a value: store the address and the value in the stack. */
#define PUSH_VALUE(the_value)						\
  (private->change_stack_pointer->address = &(the_value),		\
   (private->change_stack_pointer++)->value = (the_value))

/* Save a board value: store the address and the value in the
 * stack.
 */
#define PUSH_VERTEX(the_value)						\
  (private->vertex_stack_pointer->address = &(the_value),		\
   (private->vertex_stack_pointer++)->value = (the_value))


#define POP_MOVE()							\
  do {									\
    while ((--private->change_stack_pointer)->address)			\
      *(private->change_stack_pointer->address)				\
	= private->change_stack_pointer->value;				\
  } while (0)

#define POP_VERTICES()							\
  do {									\
    while ((--private->vertex_stack_pointer)->address)			\
      *(private->vertex_stack_pointer->address)				\
	= private->vertex_stack_pointer->value;				\
  } while (0)



/* The string index at `pos'.  There must be a stone for this macro to
 * give meaningful results.
 */
#define STRING_INDEX(pos)	(private->string_index[pos])

/* Get a pointer to `String_data' structure for the string at `pos'. */
#define STRING_DATA(pos)						\
  STRING_DATA_BY_INDEX(STRING_INDEX(pos))

/* Likewise for a strinng with known index. */
#define STRING_DATA_BY_INDEX(string_index)				\
  (&private->strings[string_index])


/* Do the stones at `pos1' and `pos2' belong to the same string? */
#define SAME_STRING(pos1, pos2) (STRING_INDEX(pos1) == STRING_INDEX(pos2))


/* Get string origin. */
#define ORIGIN(pos)							\
  (STRING_DATA(pos)->origin)
#define ORIGIN_BY_INDEX(string_index)					\
  (STRING_DATA_BY_INDEX(string_index)->origin)

/* Get the number of string liberties. */
#define LIBERTIES(pos)							\
  (STRING_DATA(pos)->num_liberties)
#define LIBERTIES_BY_INDEX(string_index)				\
  (STRING_DATA_BY_INDEX(string_index)->num_liberties)

/* Get the size (number of stones) of the string. */
#define COUNT_STONES(pos)						\
  (STRING_DATA(pos)->size)
#define COUNT_STONES_BY_INDEX(string_index)				\
  (STRING_DATA_BY_INDEX(string_index)->size)

/* Get the number of string neighbors. */
#define COUNT_NEIGHBORS(pos)						\
  (STRING_DATA(pos)->num_neighbors)
#define COUNT_NEIGHBORS_BY_INDEX(string_index)				\
  (STRING_DATA_BY_INDEX(string_index)->num_neighbors)


#define STRING_LIBERTIES(string_index)					\
  (private->string_liberties[string_index])

#define STRING_NEIGHBORS(string_index)					\
  (private->string_neighbors[string_index])


/* Macros to traverse the stones of a string.
 *
 * Usage:
 *
 *	int s, pos;
 *	s = find_the_string()
 *	pos = FIRST_STONE(goban, s);
 *	do {
 *	  use_stone(pos);
 *	  pos = NEXT_STONE(goban, pos);
 *	} while (!BACK_TO_FIRST_STONE(goban, s, pos));
 */
#define FIRST_STONE(string_index)					\
  (ORIGIN_BY_INDEX(string_index))

#define NEXT_STONE(pos)		(private->next_stone[pos])

#define BACK_TO_FIRST_STONE(string_index, pos)				\
  ((pos) == FIRST_STONE(string_index))


/* Is `pos' empty? */
#define LIBERTY(pos)		(board[pos] == EMPTY)

/* Is `pos' empty and not marked? */
#define UNMARKED_LIBERTY(pos)						\
  (board[pos] == EMPTY &&						\
   private->last_liberty_marks[pos] != private->liberty_mark)

/* Mark `pos'.  Use together with UNMARKED_LIBERTY(). */
#define MARK_LIBERTY(pos)						\
  (private->last_liberty_marks[pos] = private->liberty_mark)

#define UNMARKED_STRING(pos)						\
  (STRING_DATA(pos)->mark != private->string_mark)

/* Note that these two macros are not complementary. Both return
 * false if board[pos] != color.
 */
#define UNMARKED_COLOR_STRING(pos, color)				\
  (board[pos] == (color) && UNMARKED_STRING(pos))

#define MARKED_COLOR_STRING(pos, color)					\
  (board[pos] == (color) && !UNMARKED_STRING(pos))

#define MARK_STRING(pos)						\
  (STRING_DATA(pos)->mark = private->string_mark)

#define STRING_AT_VERTEX(pos, string_index, color)			\
  (board[pos] == (color) && STRING_INDEX(pos) == (string_index))

#define NEIGHBOR_OF_STRING(pos, string_index, color)			\
  (STRING_AT_VERTEX(SOUTH(pos), (string_index), (color))		\
   || STRING_AT_VERTEX(WEST(pos), (string_index), (color))		\
   || STRING_AT_VERTEX(NORTH(pos), (string_index), (color))		\
   || STRING_AT_VERTEX(EAST(pos), (string_index), (color)))

/* These four macros have rather confusing names. It should be read as:
 * "(pos) is a neighbor of string (s) of (color) in any direction except
 * the specified one".
 */
#define NON_SOUTH_NEIGHBOR_OF_STRING(pos, string_index, color)		\
  (STRING_AT_VERTEX(SOUTH(pos), (string_index), (color))		\
   || STRING_AT_VERTEX(WEST(pos), (string_index), (color))		\
   || STRING_AT_VERTEX(EAST(pos), (string_index), (color)))

#define NON_WEST_NEIGHBOR_OF_STRING(pos, string_index, color)		\
  (STRING_AT_VERTEX(WEST(pos), (string_index), (color))			\
   || STRING_AT_VERTEX(NORTH(pos), (string_index), (color))		\
   || STRING_AT_VERTEX(SOUTH(pos), (string_index), (color)))

#define NON_NORTH_NEIGHBOR_OF_STRING(pos, string_index, color)		\
  (STRING_AT_VERTEX(NORTH(pos), (string_index), (color))		\
   || STRING_AT_VERTEX(EAST(pos), (string_index), (color))		\
   || STRING_AT_VERTEX(WEST(pos), (string_index), (color)))

#define NON_EAST_NEIGHBOR_OF_STRING(pos, string_index, color)		\
  (STRING_AT_VERTEX(EAST(pos), (string_index), (color))			\
   || STRING_AT_VERTEX(SOUTH(pos), (string_index), (color))		\
   || STRING_AT_VERTEX(NORTH(pos), (string_index), (color)))


#define ADD_LIBERTY(string_index, pos)					\
  do {									\
    if (LIBERTIES_BY_INDEX(string_index) < MAX_LIBERTIES) {		\
      STRING_LIBERTIES(string_index)[LIBERTIES_BY_INDEX(string_index)]	\
	= (pos);							\
    }									\
    LIBERTIES_BY_INDEX(string_index)++;					\
  } while (0)

#define ADD_AND_MARK_LIBERTY(string_index, pos)				\
  do {									\
    ADD_LIBERTY((string_index), (pos));					\
    MARK_LIBERTY(pos);							\
  } while (0)

#define ADD_NEIGHBOR(string_index, pos)					\
  (STRING_NEIGHBORS(string_index)					\
   [COUNT_NEIGHBORS_BY_INDEX(string_index)++] = STRING_INDEX(pos))

#define DO_ADD_STONE(pos, color)					\
  do {									\
    PUSH_VERTEX(board[pos]);						\
    board[pos] = color;							\
    hashdata_invert_stone(&goban->board_hash, pos, color);		\
  } while (0)

#define DO_REMOVE_STONE(pos)						\
  do {									\
    PUSH_VERTEX(board[pos]);						\
    hashdata_invert_stone(&goban->board_hash, pos, board[pos]);		\
    board[pos] = EMPTY;							\
  } while (0)


#define STORE_LIBERTY(new_liberty, liberty_list, num_liberties)		\
  do {									\
    if (liberty_list)							\
      (liberty_list) [num_liberties] = (new_liberty);			\
    (num_liberties)++;							\
  } while (0)

#define STORE_LIBERTY_WITH_LIMIT(new_liberty,				\
				 liberty_list, num_liberties, limit)	\
  do {									\
    STORE_LIBERTY((new_liberty), (liberty_list), (num_liberties));	\
    if ((num_liberties) >= (limit))					\
      return num_liberties;						\
  } while (0)


/* ---------------------------------------------------------------- */


/* Forward declarations. */
static int  do_trymove(Goban *goban, int pos, int color, int ignore_ko);
static void undo_trymove(Goban *goban);

static int  do_approxlib(const Goban *goban, int pos, int color,
			 int maxlib, int *libs);
static int  slow_approxlib(const Goban *goban, int pos, int color,
			   int maxlib, int *libs);
static int  do_accuratelib(const Goban *goban, int pos, int color,
			   int maxlib, int *libs);

static void new_position(Goban *goban);
static int  propagate_string(const Goban *goban, int stone, int str);
static void find_liberties_and_neighbors(const Goban *goban, int s);
static int  do_remove_string(Goban *goban, int s);
static void do_commit_suicide(Goban *goban, int pos, int color);
static void do_play_move(Goban *goban, int pos, int color);


/* Coordinates for the eight directions, ordered
 * south, west, north, east, southwest, northwest, northeast, southeast.
 */
int deltai[8] = {  1,  0,  -1, 0,    1,    -1,    -1,    1 };
int deltaj[8] = {  0, -1,   0, 1,   -1,    -1,     1,    1 };
int delta[8]  = { NS, -1, -NS, 1, NS-1, -NS-1, -NS+1, NS+1 };


/* ================================================================ */
/*                    Board initialization                          */
/* ================================================================ */


Goban *
create_goban(int board_size)
{
  Goban *goban = malloc(sizeof(Goban));
  gg_assert(NULL, goban);

  goban->private = malloc(sizeof(Goban_private_data));
  gg_assert(NULL, goban->private);

  goban->variations_counter = 0;
  goban->sgf_dumptree = NULL;

  goban->allow_suicide = 0;
  goban->chinese_rules = 0;

  goban->position_number		   = 0;
  goban->private->trymove_counter	   = 0;
  goban->private->stone_count_for_position = 0;
  goban->private->white_stones_on_board	   = 0;
  goban->private->black_stones_on_board	   = 0;

  clear_internal_caches(goban);

  /* clear_board() also initializes all the rest fields. */
  goban->board_size = board_size;
  clear_board(goban);

  return goban;
}


/* Save board state. */
void
store_board(const Goban *goban, Board_state *state)
{
  ACCESS_PRIVATE_DATA_CONST;

  int k;

  gg_assert(goban, goban->stackp == 0);

  state->board_size	= goban->board_size;
  state->board_ko_pos	= goban->board_ko_pos;
  state->white_captured = goban->white_captured;
  state->black_captured = goban->black_captured;

  memcpy(state->board, goban->board, sizeof goban->board);

  state->initial_board_ko_pos	= private->initial_board_ko_pos;
  state->initial_white_captured = private->initial_white_captured;
  state->initial_black_captured = private->initial_black_captured;

  memcpy(state->initial_board, private->initial_board,
	 sizeof private->initial_board);

  state->move_history_pointer = private->move_history_pointer;
  for (k = 0; k < private->move_history_pointer; k++) {
    state->move_history_color[k] = private->move_history_color[k];
    state->move_history_pos[k]	 = private->move_history_pos[k];
  }

  state->komi	     = goban->komi;
  state->move_number = goban->move_number;
}


/* Restore a saved board state. */
void
restore_board(Goban *goban, const Board_state *state)
{
  ACCESS_PRIVATE_DATA;

  int k;

  gg_assert(goban, goban->stackp == 0);

  goban->board_size     = state->board_size;
  goban->board_ko_pos   = state->board_ko_pos;
  goban->white_captured = state->white_captured;
  goban->black_captured = state->black_captured;

  memcpy(goban->board, state->board, sizeof goban->board);

  private->initial_board_ko_pos   = state->initial_board_ko_pos;
  private->initial_white_captured = state->initial_white_captured;
  private->initial_black_captured = state->initial_black_captured;

  memcpy(private->initial_board, state->initial_board,
	 sizeof private->initial_board);

  private->move_history_pointer = state->move_history_pointer;
  for (k = 0; k < private->move_history_pointer; k++) {
    private->move_history_color[k] = state->move_history_color[k];
    private->move_history_pos[k]   = state->move_history_pos[k];
  }

  goban->komi	     = state->komi;
  goban->move_number = state->move_number;

  hashdata_recalc(&goban->board_hash, goban->board, goban->board_ko_pos);
  new_position(goban);
}


/* Clear the internal board. */
void
clear_board(Goban *goban)
{
  ACCESS_PRIVATE_DATA;

  int pos;

  gg_assert(goban, goban->board_size > 0 && goban->board_size <= MAX_BOARD);

  memset(goban->board, EMPTY, sizeof goban->board);
  memset(private->initial_board, EMPTY, sizeof private->initial_board);

  for (pos = 0; pos < BOARDSIZE; pos++) {
    if (!ON_BOARD2(goban, I(pos), J(pos))) {
      goban->board[pos]		  = GRAY;
      private->initial_board[pos] = GRAY;
    }
  }

  goban->board_ko_pos   = NO_MOVE;
  goban->white_captured = 0;
  goban->black_captured = 0;

  goban->stackp = 0;

  private->komaster = EMPTY;
  private->kom_pos  = NO_MOVE;

  private->initial_board_ko_pos   = NO_MOVE;
  private->initial_white_captured = 0;
  private->initial_black_captured = 0;

  private->move_history_pointer = 0;
  goban->move_number		= 0;

  hashdata_recalc(&goban->board_hash, goban->board, goban->board_ko_pos);
  new_position(goban);
}


void
clear_internal_caches(const Goban *goban)
{
#if USE_BOARD_CACHES

  ACCESS_PRIVATE_DATA;

  int pos;

  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    /* Set thresholds to zero, so that the "cached" values are not
     * used even if we have an accidental hash value match.
     */
    private->approxlib_cache[pos][0].threshold	 = 0;
    private->approxlib_cache[pos][1].threshold	 = 0;
    private->accuratelib_cache[pos][0].threshold = 0;
    private->accuratelib_cache[pos][1].threshold = 0;
  }

#endif
}


/* Test the integrity of the gray border. */
int
test_gray_border(const Goban *goban)
{
  int pos;

  gg_assert(goban, goban->board_size > 0 && goban->board_size <= MAX_BOARD);

  for (pos = 0; pos < BOARDSIZE; pos++) {
    if (!ON_BOARD2(goban, I(pos), J(pos)) && goban->board[pos] != GRAY)
      return pos;
  }

  return -1;
}


/* ================================================================ */
/*                      Temporary moves                             */
/* ================================================================ */

/*
 * trymove pushes the position onto the stack, and makes a move
 * at pos of color. Returns one if the move is legal. The
 * stack pointer is only incremented if the move is legal.
 *
 * The way to use this is:
 *
 *   if (trymove(...)) {
 *      ...
 *      popgo();
 *   }
 *
 * The message can be written as a comment to an sgf file using
 * sgfdump(). str can be NO_MOVE if it is not needed but otherwise
 * the location of str is included in the comment.
 */

int
trymove(Goban *goban, int pos, int color, const char *message, int str)
{
  /* FIXME: Why do we pass it around then? */
  UNUSED(str);

  /* Do the real work elsewhere. */
  if (!do_trymove(goban, pos, color, 0))
    return 0;

  /* Store the move in an sgf tree if one is available. */
  if (goban->sgf_dumptree) {
    ACCESS_PRIVATE_DATA_CONST;
    char buffer[100];

    if (message == NULL)
      message = "UNKNOWN";

    if (pos == NO_MOVE) {
      if (private->komaster != EMPTY)
	gg_snprintf(buffer, sizeof buffer,
		    "%s (variation %d, hash %s, komaster %s:%s)",
		    message, goban->variations_counter,
		    hashdata_to_string(&goban->board_hash),
		    color_to_string(private->komaster),
		    location_to_string(goban->board_size, private->kom_pos));
      else
	gg_snprintf(buffer, sizeof buffer,
		    "%s (variation %d, hash %s)", message,
		    goban->variations_counter,
		    hashdata_to_string(&goban->board_hash));
    }
    else {
      if (private->komaster != EMPTY)
	gg_snprintf(buffer, sizeof buffer,
		    "%s at %s (variation %d, hash %s, komaster %s:%s)",
		    message, location_to_string(goban->board_size, pos),
		    goban->variations_counter,
		    hashdata_to_string(&goban->board_hash),
		    color_to_string(private->komaster),
		    location_to_string(goban->board_size, private->kom_pos));
      else
	gg_snprintf(buffer, sizeof buffer, "%s at %s (variation %d, hash %s)",
		    message, location_to_string(goban->board_size, pos),
		    goban->variations_counter,
		    hashdata_to_string(&goban->board_hash));
    }

    sgftreeAddPlayLast(goban->sgf_dumptree, color, I(pos), J(pos));
    sgftreeAddComment(goban->sgf_dumptree, buffer);
  }

  if (goban->variations_counter)
    goban->variations_counter++;
  stats.nodes++;

  return 1;
}


/*
 * tryko pushes the position onto the stack, and makes a move
 * at (pos) of (color). The move is allowed even if it is an
 * illegal ko capture. It is to be imagined that (color) has
 * made an intervening ko threat which was answered and now
 * the continuation is to be explored.
 *
 * Return 1 if the move is legal with the above caveat. Returns
 * zero if it is not legal because of suicide.
 */

int
tryko(Goban *goban, int pos, int color, const char *message)
{
  /* Do the real work elsewhere. */
  if (!do_trymove(goban, pos, color, 1))
    return 0;

  if (goban->sgf_dumptree) {
    ACCESS_PRIVATE_DATA_CONST;
    char buffer[100];

    if (message == NULL)
      message = "UNKNOWN";

    if (private->komaster != EMPTY)
      gg_snprintf(buffer, sizeof buffer,
		  "tryko: %s (variation %d, %s, komaster %s:%s)",
		  message, goban->variations_counter,
		  hashdata_to_string(&goban->board_hash),
		  color_to_string(private->komaster),
		  location_to_string(goban->board_size, private->kom_pos));
    else
      gg_snprintf(buffer, sizeof buffer, "tryko: %s (variation %d, %s)",
		  message, goban->variations_counter,
		  hashdata_to_string(&goban->board_hash));

    /* Add two pass moves to the SGF output to simulate the ko threat
     * and the answer.
     *
     * The reason we add these is that certain SGF viewers, including
     * Cgoban 1, won't properly display variations with illegal ko
     * captures. SGF FF[4] compliant browsers should have no problem
     * with this, though.
     */
    sgftreeAddPlayLast(goban->sgf_dumptree, color, -1, -1);
    sgftreeAddComment(goban->sgf_dumptree, "tenuki (ko threat)");
    sgftreeAddPlayLast(goban->sgf_dumptree, OTHER_COLOR(color), -1, -1);
    sgftreeAddComment(goban->sgf_dumptree, "tenuki (answers ko threat)");

    sgftreeAddPlayLast(goban->sgf_dumptree, color, I(pos), J(pos));
    sgftreeAddComment(goban->sgf_dumptree, buffer);
  }

  if (goban->variations_counter)
    goban->variations_counter++;
  stats.nodes++;

  return 1;
}


/*
 * Do the main work of trymove() and tryko(), i.e. the common parts.
 * The ignore_ko flag tells whether an illegal ko capture may be done.
 * Return 1 if the move was valid, otherwise 0.
 */

static int
do_trymove(Goban *goban, int pos, int color, int ignore_ko)
{
  ACCESS_PRIVATE_DATA;

  /* 1. The color must be BLACK or WHITE. */
  gg_assert(goban, color == BLACK || color == WHITE);

  if (pos != PASS_MOVE) {
    ACCESS_BOARD;

    /* 2. Unless pass, the move must be inside the board. */
    ASSERT_ON_BOARD1(goban, pos);

    /* Update the reading tree shadow. */
    goban->shadow[pos] = 1;

    /* 3. The location must be empty. */
    if (board[pos] != EMPTY)
      return 0;

    /* 4. The location must not be the ko point, unless ignore_ko == 1. */
    if (!ignore_ko && pos == goban->board_ko_pos) {
      if (board[WEST(pos)] == OTHER_COLOR(color)
	  || board[EAST(pos)] == OTHER_COLOR(color)) {
	return 0;
      }
    }

    /* 5. Test for suicide. */
    if (is_suicide(goban, pos, color))
      return 0;
  }

  /* Check for stack overflow. */
  if (goban->stackp >= MAXSTACK - 2) {
    fprintf(stderr,
	    "gnugo: Truncating search. This is beyond my reading ability!\n");
    /* FIXME: Perhaps it's best to just assert here and be done with it? */
    if (0)
      ASSERT1(goban, 0 && "trymove stack overflow", pos);

#if 0
    if (verbose > 0) {
      showboard(0);
      dump_stack();
    }
#endif
    fflush(stderr);
    return 0;
  }


  /* Only count trymove when we do create a new position. */
  private->trymove_counter++;

  /* So far, so good. Now push the move on the move stack. These are
   * needed for dump_stack().
   */
  private->stack[goban->stackp]	     = pos;
  private->move_color[goban->stackp] = color;

  /*
   * FIXME: Do we really have to store board_hash in a stack?
   *
   * Answer: No, we don't.  But for every stone that we add
   *         or remove, we must call hashdata_invert_stone(). This is
   *         not difficult per se, but the whole board.c
   *         will have to be checked, and there is lots of room
   *         for mistakes.
   *
   *         At the same time, profiling shows that storing the
   *         hashdata in a stack doesn't take a lot of time, so
   *         this is not an urgent FIXME.
   */
  BEGIN_CHANGE_RECORD();
  PUSH_VALUE(goban->board_ko_pos);
  memcpy(&private->board_hash_stack[goban->stackp], &goban->board_hash,
	 sizeof goban->board_hash);

  if (goban->board_ko_pos != NO_MOVE)
    hashdata_invert_ko(&goban->board_hash, goban->board_ko_pos);

  goban->board_ko_pos = NO_MOVE;

  goban->stackp++;

  if (pos != PASS_MOVE) {
    PUSH_VALUE(goban->black_captured);
    PUSH_VALUE(goban->white_captured);
    do_play_move(goban, pos, color);
  }

  return 1;
}


/*
 * popgo pops the position from the stack.
 */

void
popgo(Goban *goban)
{
  ACCESS_PRIVATE_DATA;

  goban->stackp--;
  undo_trymove(goban);

  memcpy(&goban->board_hash, &private->board_hash_stack[goban->stackp],
	 sizeof goban->board_hash);

  if (goban->sgf_dumptree) {
    char buffer[100];
    int is_tryko = 0;
    char *sgf_comment;

    /* FIXME: Change the sgfGet*Property() interface so that either
     * "C" instead of "C " works or the SGFXX symbols are used.
     */
    if (sgfGetCharProperty(goban->sgf_dumptree->lastnode, "C ", &sgf_comment)
	&& strncmp(sgf_comment, "tryko:", 6) == 0)
      is_tryko = 1;

    gg_snprintf(buffer, sizeof buffer, "(next variation: %d)",
		goban->variations_counter);
    sgftreeAddComment(goban->sgf_dumptree, buffer);
    goban->sgf_dumptree->lastnode = goban->sgf_dumptree->lastnode->parent;

    /* After tryko() we need to undo two pass nodes too. */
    if (is_tryko) {
      goban->sgf_dumptree->lastnode
	= goban->sgf_dumptree->lastnode->parent->parent;
    }
  }
}


#if 0

/* Silent version of popgo(), suitable for use if you have called
 * do_trymove() without passing through trymove() or tryko().
 */

static void
silent_popgo(Goban *goban)
{
  goban->stackp--;
  undo_trymove(goban);
  memcpy(&goban->board_hash, &private->board_hash_stack[goban->stackp],
	 sizeof goban->board_hash);
}

#endif

/* Restore board state to the position before the last move. This is
 * accomplished by popping everything that was stored on the stacks
 * since the last BEGIN_CHANGE_RECORD().
 */

static void
undo_trymove(Goban *goban)
{
  ACCESS_PRIVATE_DATA;

  gg_assert(goban,
	    private->change_stack_pointer - private->change_stack
	    <= STACK_SIZE);

  if (0) {
    gprintf(goban, "Change stack size = %d\n",
	    private->change_stack_pointer - private->change_stack);
    gprintf(goban, "Vertex stack size = %d\n",
	    private->vertex_stack_pointer - private->vertex_stack);
  }

  POP_MOVE();
  POP_VERTICES();
}



/*
 * dump_stack() for use under gdb prints the move stack.
 */

void
dump_stack(const Goban *goban)
{
  do_dump_stack(goban);

#if !TRACE_READ_RESULTS
  if (goban->variations_counter)
    gprintf(goban, "%o (variation %d)", goban->variations_counter - 1);
#else
  gprintf(goban, "%o (%s)", hashdata_to_string(&goban->board_hash));
#endif

  gprintf(goban, "%o\n");
  fflush(stderr);
}

/* Bare bones of dump_stack(). */
void
do_dump_stack(const Goban *goban)
{
  ACCESS_PRIVATE_DATA_CONST;

  int n;

  for (n = 0; n < goban->stackp; n++) {
    gprintf(goban, "%o%s:%1m ",
	    private->move_color[n] == BLACK ? "B" : "W",
	    private->stack[n]);
  }
}

/* ================================================================ */
/*                     Permanent moves                              */
/* ================================================================ */


static void
reset_move_history(Goban *goban)
{
  ACCESS_PRIVATE_DATA;

  memcpy(private->initial_board, goban->board, sizeof goban->board);
  private->initial_board_ko_pos	  = goban->board_ko_pos;
  private->initial_white_captured = goban->white_captured;
  private->initial_black_captured = goban->black_captured;

  private->move_history_pointer = 0;
}

/* Place a stone on the board and update the board_hash. This operation
 * destroys all move history.
 */

void
add_stone(Goban *goban, int pos, int color)
{
  ACCESS_BOARD;

  ASSERT1(goban, goban->stackp == 0, pos);
  ASSERT_ON_BOARD1(goban, pos);
  ASSERT1(goban, board[pos] == EMPTY, pos);

  board[pos] = color;
  hashdata_invert_stone(&goban->board_hash, pos, color);
  reset_move_history(goban);

  new_position(goban);
}


/* Remove a stone from the board and update the board_hash. This
 * operation destroys the move history.
 */

void
remove_stone(Goban *goban, int pos)
{
  ACCESS_BOARD;

  ASSERT1(goban, goban->stackp == 0, pos);
  ASSERT_ON_BOARD1(goban, pos);
  ASSERT1(goban, IS_STONE(board[pos]), pos);

  hashdata_invert_stone(&goban->board_hash, pos, board[pos]);
  board[pos] = EMPTY;
  reset_move_history(goban);

  new_position(goban);
}


/* Play a move. Basically the same as play_move() below, but doesn't store
 * the move in history list.
 *
 * Set `update_internals' to zero if you want to play several moves in a
 * row to avoid overhead caused by new_position(). Don't forget to call
 * it yourself after all the moves have been played.
 */
static void
play_move_no_history(Goban *goban, int pos, int color, int update_internals)
{
  ACCESS_PRIVATE_DATA;

#if CHECK_HASHING
  Hash_data oldkey;

  /* Check the hash table to see if it corresponds to the cumulative one. */
  hashdata_recalc(&oldkey, goban->board, goban->board_ko_pos);
  gg_assert(goban, hashdata_is_equal(oldkey, goban->board_hash));
#endif

  if (goban->board_ko_pos != NO_MOVE)
    hashdata_invert_ko(&goban->board_hash, goban->board_ko_pos);
  goban->board_ko_pos = NO_MOVE;

  /* If the move is a pass, we can skip some steps. */
  if (pos != PASS_MOVE) {
    ASSERT_ON_BOARD1(goban, pos);
    ASSERT1(goban, goban->board[pos] == EMPTY, pos);

    /* Do play the move. */
    if (!is_suicide(goban, pos, color))
      do_play_move(goban, pos, color);
    else
      do_commit_suicide(goban, pos, color);

#if CHECK_HASHING
    /* Check the hash table to see if it equals the previous one. */
    hashdata_recalc(&oldkey, goban->board, goban->board_ko_pos);
    gg_assert(goban, hashdata_is_equal(oldkey, goban->board_hash));
#endif
  }

  if (update_internals || private->next_string == MAX_STRINGS)
    new_position(goban);
  else
    CLEAR_STACKS();
}

/* Load the initial position and replay the first n moves. */
static void
replay_move_history(Goban *goban, int n)
{
  ACCESS_PRIVATE_DATA_CONST;

  int k;

  memcpy(goban->board, private->initial_board, sizeof goban->board);
  goban->board_ko_pos	= private->initial_board_ko_pos;
  goban->white_captured = private->initial_white_captured;
  goban->black_captured = private->initial_black_captured;

  new_position(goban);

  for (k = 0; k < n; k++) {
    play_move_no_history(goban, private->move_history_pos[k],
			 private->move_history_color[k], 0);
  }

  new_position(goban);
}

/* Play a move. If you want to test for legality you should first call
 * is_legal(). This function strictly follows the algorithm:
 * 1. Place a stone of given color on the board.
 * 2. If there are any adjacent opponent strings without liberties,
 *    remove them and increase the prisoner count.
 * 3. If the newly placed stone is part of a string without liberties,
 *    remove it and increase the prisoner count.
 *
 * In spite of the name "permanent move", this move can (usually) be
 * unplayed by undo_move(), but it is significantly more costly than
 * unplaying a temporary move. There are limitations on the available
 * move history, so under certain circumstances the move may not be
 * possible to unplay at a later time.
 */
void
play_move(Goban *goban, int pos, int color)
{
  ACCESS_PRIVATE_DATA;
  ACCESS_BOARD;

  ASSERT1(goban, goban->stackp == 0, pos);
  ASSERT1(goban, color == WHITE || color == BLACK, pos);
  ASSERT1(goban, pos == PASS_MOVE || ON_BOARD1(goban, pos), pos);
  ASSERT1(goban, pos == PASS_MOVE || board[pos] == EMPTY, pos);
  ASSERT1(goban, private->komaster == EMPTY && private->kom_pos == NO_MOVE,
	  pos);

  if (private->move_history_pointer >= MAX_MOVE_HISTORY) {
    /* The move history is full. We resolve this by collapsing the
     * first about 10% of the moves into the initial position.
     */
    int number_collapsed_moves = 1 + MAX_MOVE_HISTORY / 10;
    int k;
    Intersection saved_board[BOARDSIZE];
    int saved_board_ko_pos   = goban->board_ko_pos;
    int saved_white_captured = goban->white_captured;
    int saved_black_captured = goban->black_captured;

    memcpy(saved_board, board, sizeof goban->board);

    replay_move_history(goban, number_collapsed_moves);

    memcpy(private->initial_board, board, sizeof goban->board);
    private->initial_board_ko_pos   = goban->board_ko_pos;
    private->initial_white_captured = goban->white_captured;
    private->initial_black_captured = goban->black_captured;

    for (k = number_collapsed_moves; k < private->move_history_pointer; k++) {
      private->move_history_color[k - number_collapsed_moves]
	= private->move_history_color[k];
      private->move_history_pos[k - number_collapsed_moves]
	= private->move_history_pos[k];
    }

    private->move_history_pointer -= number_collapsed_moves;

    memcpy(board, saved_board, sizeof goban->board);
    goban->board_ko_pos   = saved_board_ko_pos;
    goban->white_captured = saved_white_captured;
    goban->black_captured = saved_black_captured;

    new_position(goban);
  }

  private->move_history_color[private->move_history_pointer] = color;
  private->move_history_pos[private->move_history_pointer]   = pos;
  private->move_history_pointer++;

  play_move_no_history(goban, pos, color, 1);

  goban->move_number++;
}


/* Undo n permanent moves. Returns 1 if successful and 0 if it fails.
 * If n moves cannot be undone, no move is undone.
 */
int
undo_moves(Goban *goban, int n)
{
  ACCESS_PRIVATE_DATA;

  gg_assert(goban, goban->stackp == 0);

  /* Fail if and only if the move history is too short. */
  if (private->move_history_pointer < n)
    return 0;

  replay_move_history(goban, private->move_history_pointer - n);
  private->move_history_pointer -= n;
  goban->move_number		-= n;

  return 1;
}


/* Return the last move done by anyone. Both if no move was found or
 * if the last move was a pass, PASS_MOVE is returned.
 */
int
get_last_move(const Goban *goban)
{
  ACCESS_PRIVATE_DATA;
  if (private->move_history_pointer == 0)
    return PASS_MOVE;

  return private->move_history_pos[private->move_history_pointer - 1];
}

/* Return the color of the player doing the last move. If no move was
 * found, EMPTY is returned.
 */
int
get_last_player(const Goban *goban)
{
  ACCESS_PRIVATE_DATA;
  if (private->move_history_pointer == 0)
    return EMPTY;

  return private->move_history_color[private->move_history_pointer - 1];
}


/* Return the last move done by the opponent to color. Both if no move
 * was found or if the last move was a pass, PASS_MOVE is returned.
 */
int
get_last_opponent_move(const Goban *goban, int color)
{
  ACCESS_PRIVATE_DATA;
  int k;

  for (k = private->move_history_pointer - 1; k >= 0; k--) {
    if (private->move_history_color[k] == OTHER_COLOR(color))
      return private->move_history_pos[k];
  }

  return PASS_MOVE;
}


/* ================================================================ */
/*                        Utility functions                         */
/* ================================================================ */


/*
 * Test if the move is a pass or not.  Return 1 if it is.
 */

int
is_pass(int pos)
{
  return pos == 0;
}


/*
 * is_legal(pos, color) determines whether the move (color) at
 * pos is legal.
 */

int
is_legal(const Goban *goban, int pos, int color)
{
  ACCESS_BOARD_CONST;

  /* 0. A pass move is always legal. */
  if (pos == 0)
    return 1;

  /* 1. The move must be inside the board. */
  ASSERT_ON_BOARD1(goban, pos);

  /* 2. The location must be empty. */
  if (board[pos] != EMPTY)
    return 0;

  /* 3. The location must not be the ko point. */
  if (pos == goban->board_ko_pos
      && (board[WEST(pos)] == OTHER_COLOR(color)
	  || board[EAST(pos)] == OTHER_COLOR(color)))
    return 0;

  /* Check for stack overflow. */
  if (goban->stackp >= MAXSTACK - 2) {
    fprintf(stderr,
	    "gnugo: Truncating search. This is beyond my reading ability!\n");
    /* FIXME: Perhaps it's best to just assert here and be done with it? */
    if (0)
      ASSERT1(goban, 0 && "is_legal stack overflow", pos);

    return 0;
  }

  /* Check for suicide. */
  if (!goban->allow_suicide && is_suicide(goban, pos, color))
    return 0;

  return 1;
}


/*
 * is_suicide(pos, color) determines whether the move (color) at
 * (pos) would be a suicide.
 *
 * This is the case if
 * 1. There is no neighboring empty intersection.
 * 2. There is no neighboring opponent string with exactly one liberty.
 * 3. There is no neighboring friendly string with more than one liberty.
 */
int
is_suicide(const Goban *goban, int pos, int color)
{
  ACCESS_PRIVATE_DATA_CONST;
  ACCESS_BOARD_CONST;

  ASSERT_ON_BOARD1(goban, pos);
  ASSERT1(goban, board[pos] == EMPTY, pos);

  /* Check for suicide. */
  if (LIBERTY(SOUTH(pos))
      || (ON_BOARD(goban, SOUTH(pos))
	  && ((board[SOUTH(pos)] == color) ^ (LIBERTIES(SOUTH(pos)) == 1))))
    return 0;

  if (LIBERTY(WEST(pos))
      || (ON_BOARD(goban, WEST(pos))
	  && ((board[WEST(pos)] == color) ^ (LIBERTIES(WEST(pos)) == 1))))
    return 0;

  if (LIBERTY(NORTH(pos))
      || (ON_BOARD(goban, NORTH(pos))
	  && ((board[NORTH(pos)] == color) ^ (LIBERTIES(NORTH(pos)) == 1))))
    return 0;

  if (LIBERTY(EAST(pos))
      || (ON_BOARD(goban, EAST(pos))
	  && ((board[EAST(pos)] == color) ^ (LIBERTIES(EAST(pos)) == 1))))
    return 0;

  return 1;
}


/*
 * is_illegal_ko_capture(pos, color) determines whether the move
 * (color) at (pos) would be an illegal ko capture.
 */
int
is_illegal_ko_capture(const Goban *goban, int pos, int color)
{
  ACCESS_BOARD_CONST;

  ASSERT_ON_BOARD1(goban, pos);
  ASSERT1(goban, board[pos] == EMPTY, pos);

  return (pos == goban->board_ko_pos
	  && (board[WEST(pos)] == OTHER_COLOR(color)
	      || board[EAST(pos)] == OTHER_COLOR(color)));
}


/* Necessary work to set the new komaster state. */
static void
set_new_komaster(Goban *goban, int new_komaster)
{
  ACCESS_PRIVATE_DATA;

  PUSH_VALUE(private->komaster);
  hashdata_invert_komaster(&goban->board_hash, private->komaster);
  private->komaster = new_komaster;
  hashdata_invert_komaster(&goban->board_hash, private->komaster);
}


/* Necessary work to set the new komaster position. */
static void
set_new_kom_pos(Goban *goban, int new_kom_pos)
{
  ACCESS_PRIVATE_DATA;

  PUSH_VALUE(private->kom_pos);
  hashdata_invert_kom_pos(&goban->board_hash, private->kom_pos);
  private->kom_pos = new_kom_pos;
  hashdata_invert_kom_pos(&goban->board_hash, private->kom_pos);
}


/* Variation of trymove()/tryko() where ko captures (both conditional
 * and unconditional) must follow a komaster scheme.
 *
 * Historical note: Up to GNU Go 3.4 five different komaster schemes
 * were implemented and could easily be switched between. In GNU Go
 * 3.5.1 four of them were removed to simplify the code and because it
 * no longer seemed interesting to be able to switch. The remaining
 * komaster scheme was previously known as komaster scheme 5 (or V).
 *
 * FIXME: This function could be optimized by integrating the
 * trymove()/tryko() code.
 */

/* V. Complex scheme, O to move.
 *
 * 1. Komaster is EMPTY.
 * 1a) Unconditional ko capture is allowed.
 *       Komaster remains EMPTY if previous move was not a ko capture.
 *       Komaster is set to WEAK_KO if previous move was a ko capture
 *       and kom_pos is set to the old value of board_ko_pos.
 * 1b) Conditional ko capture is allowed. Komaster is set to O and
 *     kom_pos to the location of the ko, where a stone was
 *     just removed.
 *
 * 2. Komaster is O:
 * 2a) Only nested ko captures are allowed. Kom_pos is moved to the
 *     new removed stone.
 * 2b) If komaster fills the ko at kom_pos then komaster reverts to
 *     EMPTY.
 *
 * 3. Komaster is X:
 *    Play at kom_pos is not allowed. Any other ko capture
 *    is allowed. If O takes another ko, komaster becomes GRAY_X.
 *
 * 4. Komaster is GRAY_O or GRAY_X:
 *    Ko captures are not allowed. If the ko at kom_pos is
 *    filled then the komaster reverts to EMPTY.
 *
 * 5. Komaster is WEAK_KO:
 * 5a) After a non-ko move komaster reverts to EMPTY.
 * 5b) Unconditional ko capture is only allowed if it is nested ko capture.
 *     Komaster is changed to WEAK_X and kom_pos to the old value of
 *     board_ko_pos.
 * 5c) Conditional ko capture is allowed according to the rules of 1b.
 */
int
komaster_trymove(Goban *goban, int pos, int color,
		 const char *message, int str,
		 int *is_conditional_ko, int consider_conditional_ko)
{
  ACCESS_PRIVATE_DATA_CONST;
  ACCESS_BOARD_CONST;

  int other = OTHER_COLOR(color);
  int ko_move;
  int kpos;
  int previous_board_ko_pos = goban->board_ko_pos;

  /* First we check whether the ko claimed by komaster has been
   * resolved. If that is the case, we revert komaster to EMPTY.
   *
   * The ko has been resolved in favor of the komaster if it has
   * been filled, or if it is no longer a ko and an opponent move
   * there is suicide.
   */
  if (((private->komaster == WHITE || private->komaster == GRAY_WHITE)
       && (IS_STONE(board[private->kom_pos])
	   || (!is_ko(goban, private->kom_pos, BLACK, NULL)
	       && is_suicide(goban, private->kom_pos, BLACK))))
      || ((private->komaster == BLACK || private->komaster == GRAY_BLACK)
	  && (IS_STONE(board[private->kom_pos])
	      || (!is_ko(goban, private->kom_pos, WHITE, NULL)
		  && is_suicide(goban, private->kom_pos, WHITE))))) {
    set_new_komaster(goban, EMPTY);
    set_new_kom_pos(goban, NO_MOVE);
  }

  *is_conditional_ko = 0;
  ko_move = is_ko(goban, pos, color, &kpos);

  if (!ko_move) {
    if (private->komaster == WEAK_KO) {
      set_new_komaster(goban, EMPTY);
      set_new_kom_pos(goban, NO_MOVE);
    }
  }
  else {
    /* If opponent is komaster we may not capture his ko. */
    if (private->komaster == other && pos == private->kom_pos)
      return 0;

    /* If komaster is gray we may not capture ko at all. */
    if (private->komaster == GRAY_WHITE || private->komaster == GRAY_BLACK)
      return 0;

    /* If we are komaster, we may only do nested captures. */
    if (private->komaster == color
	&& !DIAGONAL_NEIGHBORS(kpos, private->kom_pos))
      return 0;

    /* If komaster is WEAK_KO, we may only do nested ko capture or
     * conditional ko capture.
     */
    if (private->komaster == WEAK_KO) {
      if (pos != goban->board_ko_pos
	  && !DIAGONAL_NEIGHBORS(kpos, private->kom_pos))
	return 0;
    }
  }

  if (!trymove(goban, pos, color, message, str)) {
    if (!consider_conditional_ko)
      return 0;

    if (!tryko(goban, pos, color, message))
      return 0; /* Suicide. */

    *is_conditional_ko = 1;

    /* Conditional ko capture, set komaster parameters. */
    if (private->komaster == EMPTY || private->komaster == WEAK_KO) {
      set_new_komaster(goban, color);
      set_new_kom_pos(goban, kpos);
      return 1;
    }
  }

  if (!ko_move)
    return 1;

  if (private->komaster == other) {
    if (color == WHITE)
      set_new_komaster(goban, GRAY_BLACK);
    else
      set_new_komaster(goban, GRAY_WHITE);
  }
  else if (private->komaster == color) {
    /* This is where we update kom_pos after a nested capture. */
    set_new_kom_pos(goban, kpos);
  }
  else {
    /* We can reach here when komaster is EMPTY or WEAK_KO. If previous
     * move was also a ko capture, we now set komaster to WEAK_KO.
     */
    if (previous_board_ko_pos != NO_MOVE) {
      set_new_komaster(goban, WEAK_KO);
      set_new_kom_pos(goban, previous_board_ko_pos);
    }
  }

  return 1;
}


int
get_komaster(const Goban *goban)
{
  return goban->private->komaster;
}


int
get_kom_pos(const Goban *goban)
{
  return goban->private->kom_pos;
}


/* Determine whether vertex is on the edge. */
int
is_edge_vertex(const Goban *goban, int pos)
{
  ASSERT_ON_BOARD1(goban, pos);
  if (!ON_BOARD(goban, SW(pos))
      || !ON_BOARD(goban, NE(pos)))
    return 1;

  return 0;
}


/* Calculate the distance to the edge. */
int
edge_distance(const Goban *goban, int pos)
{
  int i = I(pos);
  int j = J(pos);
  ASSERT_ON_BOARD1(goban, pos);
  return gg_min(gg_min(i, (goban->board_size - 1) - i),
		gg_min(j, (goban->board_size - 1) - j));
}


/* Determine whether vertex is a corner. */
int
is_corner_vertex(const Goban *goban, int pos)
{
  ASSERT_ON_BOARD1(goban, pos);
  if ((!ON_BOARD(goban, WEST(pos)) || !ON_BOARD(goban, EAST(pos)))
      && (!ON_BOARD(goban, SOUTH(pos)) || !ON_BOARD(goban, NORTH(pos))))
    return 1;

  return 0;
}


/* Returns true if the empty vertex respectively the string at pos1 is
 * adjacent to the empty vertex respectively the string at pos2.
 */
int
are_neighbors(const Goban *goban, int pos1, int pos2)
{
  ACCESS_BOARD_CONST;

  if (board[pos1] == EMPTY) {
    if (board[pos2] == EMPTY)
      return (gg_abs(pos1 - pos2) == NS || gg_abs(pos1 - pos2) == WE);
    else
      return neighbor_of_string(goban, pos1, pos2);
  }
  else {
    if (board[pos2] == EMPTY)
      return neighbor_of_string(goban, pos2, pos1);
    else
      return adjacent_strings(goban, pos1, pos2);
  }
}


/* Count the number of liberties of the string at pos. pos must not be
 * empty.
 */
int
countlib(const Goban *goban, int str)
{
  ACCESS_PRIVATE_DATA_CONST;

  ASSERT1(goban, IS_STONE(goban->board[str]), str);

  /* We already know the number of liberties. Just look it up. */
  return LIBERTIES(str);
}


/* Find the liberties of the string at str. str must not be
 * empty. The locations of up to maxlib liberties are written into
 * libs[]. The full number of liberties is returned.
 *
 * If you want the locations of all liberties, whatever their number,
 * you should pass MAXLIBS as the value for maxlib and allocate space
 * for libs[] accordingly.
 */

int
findlib(const Goban *goban, int str, int maxlib, int *libs)
{
  ACCESS_PRIVATE_DATA;
  ACCESS_BOARD_CONST;

  int k;
  int liberties;
  int s;

  ASSERT1(goban, IS_STONE(board[str]), str);
  ASSERT1(goban, libs != NULL, str);

  /* We already have the list of liberties and only need to copy it to
   * libs[].
   *
   * However, if the string has more than MAX_LIBERTIES liberties the
   * list is truncated and if maxlib is also larger than MAX_LIBERTIES
   * we have to traverse the stones in the string in order to find
   * where the liberties are.
   */
  s = STRING_INDEX(str);
  liberties = LIBERTIES_BY_INDEX(s);

  if (liberties <= MAX_LIBERTIES || maxlib <= MAX_LIBERTIES) {
    /* The easy case, it suffices to copy liberty locations from the
     * incrementally updated list.
     */
    for (k = 0; k < maxlib && k < liberties; k++)
      libs[k] = STRING_LIBERTIES(s)[k];
  }
  else {
    /* The harder case, where we have to traverse the stones in the
     * string. We don't have to check explicitly if we are back to
     * the start of the chain since we will run out of liberties
     * before that happens.
     */
    int pos;
    private->liberty_mark++;

    for (k = 0, pos = FIRST_STONE(s);
	 k < maxlib && k < liberties;
	 pos = NEXT_STONE(pos)) {
      if (UNMARKED_LIBERTY(SOUTH(pos))) {
	libs[k++] = SOUTH(pos);
	MARK_LIBERTY(SOUTH(pos));
	if (k >= maxlib)
	  break;
      }

      if (UNMARKED_LIBERTY(WEST(pos))) {
	libs[k++] = WEST(pos);
	MARK_LIBERTY(WEST(pos));
	if (k >= maxlib)
	  break;
      }

      if (UNMARKED_LIBERTY(NORTH(pos))) {
	libs[k++] = NORTH(pos);
	MARK_LIBERTY(NORTH(pos));
	if (k >= maxlib)
	  break;
      }

      if (UNMARKED_LIBERTY(EAST(pos))) {
	libs[k++] = EAST(pos);
	MARK_LIBERTY(EAST(pos));
	if (k >= maxlib)
	  break;
      }
    }
  }

  return liberties;
}

/* Count the liberties a stone of the given color would get if played
 * at (pos). The location (pos) must be empty.
 *
 * The intent of this function is to be as fast as possible, not
 * necessarily complete. But if it returns a positive value (meaning
 * it has succeeded), the value is guaranteed to be correct.
 *
 * Captures are ignored based on the ignore_capture flag.  The function
 * fails if there are more than two neighbor strings of the same
 * color.  In this case, the return value is -1.  Captures are handled
 * in a very limited way, so if ignore_capture is 0, and a capture is
 * required, it will often return -1.
 *
 * Note well, that it relies on incremental data.
 */

int
fastlib(const Goban *goban, int pos, int color, int ignore_captures)
{
  ACCESS_PRIVATE_DATA;
  ACCESS_BOARD_CONST;

  int ally1 = -1;
  int ally2 = -1;
  int fast_liberties = 0;

  ASSERT1(goban, board[pos] == EMPTY, pos);
  ASSERT1(goban, IS_STONE(color), pos);

  /* Find neighboring strings of the same color. If there are more than two of
   * them, we give up (it's too difficult to count their common liberties).
   */
  if (board[SOUTH(pos)] == color) {
    ally1 = STRING_INDEX(SOUTH(pos));

    if (board[WEST(pos)] == color
	&& STRING_INDEX(WEST(pos)) != ally1) {
      ally2 = STRING_INDEX(WEST(pos));

      if (board[NORTH(pos)] == color
	  && STRING_INDEX(NORTH(pos)) != ally1
	  && STRING_INDEX(NORTH(pos)) != ally2)
	return -1;
    }
    else if (board[NORTH(pos)] == color
	     && STRING_INDEX(NORTH(pos)) != ally1)
      ally2 = STRING_INDEX(NORTH(pos));

    if (board[EAST(pos)] == color
	&& STRING_INDEX(EAST(pos)) != ally1) {
      if (ally2 < 0)
	ally2 = STRING_INDEX(EAST(pos));
      else if (STRING_INDEX(EAST(pos)) != ally2)
	return -1;
    }
  }
  else if (board[WEST(pos)] == color) {
    ally1 = STRING_INDEX(WEST(pos));

    if (board[NORTH(pos)] == color
	&& STRING_INDEX(NORTH(pos)) != ally1) {
      ally2 = STRING_INDEX(NORTH(pos));

      if (board[EAST(pos)] == color
	  && STRING_INDEX(EAST(pos)) != ally1
	  && STRING_INDEX(EAST(pos)) != ally2)
	return -1;
    }
    else if (board[EAST(pos)] == color
	     && STRING_INDEX(EAST(pos)) != ally1)
      ally2 = STRING_INDEX(EAST(pos));
  }
  else if (board[NORTH(pos)] == color) {
    ally1 = STRING_INDEX(NORTH(pos));

    if (board[EAST(pos)] == color
	&& STRING_INDEX(EAST(pos)) != ally1)
      ally2 = STRING_INDEX(EAST(pos));
  }
  else if (board[EAST(pos)] == color)
    ally1 = STRING_INDEX(EAST(pos));

  /* If we are to ignore captures, the things are very easy. */
  if (ignore_captures) {
    if (ally1 < 0) {
      /* No allies. */
      if (LIBERTY(SOUTH(pos)))
	fast_liberties++;
      if (LIBERTY(WEST(pos)))
	fast_liberties++;
      if (LIBERTY(NORTH(pos)))
	fast_liberties++;
      if (LIBERTY(EAST(pos)))
	fast_liberties++;
    }
    else if (ally2 < 0) {
      /* One ally. */
      if (LIBERTY(SOUTH(pos))
	  && !NON_SOUTH_NEIGHBOR_OF_STRING(SOUTH(pos), ally1, color))
	fast_liberties++;
      if (LIBERTY(WEST(pos))
	  && !NON_WEST_NEIGHBOR_OF_STRING(WEST(pos), ally1, color))
	fast_liberties++;
      if (LIBERTY(NORTH(pos))
	  && !NON_NORTH_NEIGHBOR_OF_STRING(NORTH(pos), ally1, color))
	fast_liberties++;
      if (LIBERTY(EAST(pos))
	  && !NON_EAST_NEIGHBOR_OF_STRING(EAST(pos), ally1, color))
	fast_liberties++;

      fast_liberties += LIBERTIES_BY_INDEX(ally1) - 1;
    }
    else {
      /* Two allies. */
      if (LIBERTY(SOUTH(pos))
	  && !NON_SOUTH_NEIGHBOR_OF_STRING(SOUTH(pos), ally1, color)
	  && !NON_SOUTH_NEIGHBOR_OF_STRING(SOUTH(pos), ally2, color))
	fast_liberties++;
      if (LIBERTY(WEST(pos))
	  && !NON_WEST_NEIGHBOR_OF_STRING(WEST(pos), ally1, color)
	  && !NON_WEST_NEIGHBOR_OF_STRING(WEST(pos), ally2, color))
	fast_liberties++;
      if (LIBERTY(NORTH(pos))
	  && !NON_NORTH_NEIGHBOR_OF_STRING(NORTH(pos), ally1, color)
	  && !NON_NORTH_NEIGHBOR_OF_STRING(NORTH(pos), ally2, color))
	fast_liberties++;
      if (LIBERTY(EAST(pos))
	  && !NON_EAST_NEIGHBOR_OF_STRING(EAST(pos), ally1, color)
	  && !NON_EAST_NEIGHBOR_OF_STRING(EAST(pos), ally2, color))
	fast_liberties++;

      fast_liberties += (LIBERTIES_BY_INDEX(ally1)
			 + LIBERTIES_BY_INDEX(ally2)
			 - count_common_libs(goban,
					     ORIGIN_BY_INDEX(ally1),
					     ORIGIN_BY_INDEX(ally2))
			 - 1);
    }
  }
  /* We are to take captures into account. This case is much more rare, so
   * it is not optimized much.
   */
  else {
    int k;

    for (k = 0; k < 4; k++) {
      int neighbor = pos + delta[k];

      if (LIBERTY(neighbor)
	  && (ally1 < 0 || !NEIGHBOR_OF_STRING(neighbor, ally1, color))
	  && (ally2 < 0 || !NEIGHBOR_OF_STRING(neighbor, ally2, color)))
	fast_liberties++;
      else if (board[neighbor] == OTHER_COLOR(color)
	       && LIBERTIES(neighbor) == 1) {
	/* A capture. */
	int neighbor_size = COUNT_STONES(neighbor);

	if (neighbor_size == 1 || (neighbor_size == 2 && ally1 < 0))
	  fast_liberties++;
	else
	  return -1;
      }
    }

    if (ally1 >= 0) {
      fast_liberties += LIBERTIES_BY_INDEX(ally1) - 1;
      if (ally2 >= 0) {
	fast_liberties += (LIBERTIES_BY_INDEX(ally2)
			   - count_common_libs(goban,
					       ORIGIN_BY_INDEX(ally1),
					       ORIGIN_BY_INDEX(ally2)));
      }
    }
  }

  return fast_liberties;
}


/* Find the liberties a stone of the given color would get if played
 * at (pos), ignoring possible captures of opponent stones. (pos)
 * must be empty. If libs != NULL, the locations of up to maxlib
 * liberties are written into libs[]. The counting of liberties may
 * or may not be halted when maxlib is reached. The number of liberties
 * found is returned.
 *
 * If you want the number or the locations of all liberties, however
 * many they are, you should pass MAXLIBS as the value for maxlib and
 * allocate space for libs[] accordingly.
 */
int
approxlib(const Goban *goban, int pos, int color, int maxlib, int *libs)
{
  int liberties;

#ifdef USE_BOARD_CACHES

  ACCESS_PRIVATE_DATA;

  Board_cache_entry *entry = &private->approxlib_cache[pos][color - 1];

  ASSERT1(goban, goban->board[pos] == EMPTY, pos);
  ASSERT1(goban, IS_STONE(color), pos);

  if (!libs) {
    /* First see if this result is cached. */
    if (hashdata_is_equal(goban->board_hash, entry->position_hash)
	&& maxlib <= entry->threshold)
      return entry->liberties;

    liberties = fastlib(goban, pos, color, 1);
    if (liberties >= 0) {
      /* Since fastlib() always returns precise result and doesn't take
       * `maxlib' into account, we set threshold to MAXLIBS so that this
       * result is used regardless of any `maxlib' passed.
       */
      entry->threshold	   = MAXLIBS;
      entry->liberties	   = liberties;
      entry->position_hash = goban->board_hash;

      return liberties;
    }
  }

  /* We initialize the cache entry threshold to `maxlib'. If do_approxlib()
   * or slow_approxlib() finds all the liberties (that is, they don't use
   * `maxlib' value for an early return), they will set threshold to
   * MAXLIBS themselves.
   */
  entry->threshold = maxlib;

  if (maxlib <= MAX_LIBERTIES)
    liberties = do_approxlib(goban, pos, color, maxlib, libs);
  else
    liberties = slow_approxlib(goban, pos, color, maxlib, libs);

  entry->liberties     = liberties;
  entry->position_hash = goban->board_hash;

#else /* not USE_BOARD_CACHES */

  ASSERT1(goban, goban->board[pos] == EMPTY, pos);
  ASSERT1(goban, IS_STONE(color), pos);

  if (!libs) {
    liberties = fastlib(goban, pos, color, 1);
    if (liberties >= 0)
      return liberties;
  }

  if (maxlib <= MAX_LIBERTIES)
    liberties = do_approxlib(goban, pos, color, maxlib, libs);
  else
    liberties = slow_approxlib(goban, pos, color, maxlib, libs);

#endif /* not USE_BOARD_CACHES */

  return liberties;
}


/* Does the real work of approxlib(). */
static int
do_approxlib(const Goban *goban, int pos, int color, int maxlib, int *libs)
{
  ACCESS_PRIVATE_DATA;
  ACCESS_BOARD_CONST;

  int k;
  int liberties = 0;

  /* Look for empty neighbors and the liberties of the adjacent
   * strings of the given color. The algorithm below won't work
   * correctly if any of the adjacent strings have more than
   * MAX_LIBERTIES liberties AND maxlib is larger than MAX_LIBERTIES.
   * therefore approxlib() calls more robust slow_approxlib() if
   * this might be the case.
   */

  /* Start by marking pos itself so it isn't counted among its own
   * liberties.
   */
  private->liberty_mark++;
  MARK_LIBERTY(pos);

  if (UNMARKED_LIBERTY(SOUTH(pos))) {
    STORE_LIBERTY_WITH_LIMIT(SOUTH(pos), libs, liberties, maxlib);
    MARK_LIBERTY(SOUTH(pos));
  }
  else if (board[SOUTH(pos)] == color) {
    int s = STRING_INDEX(SOUTH(pos));
    for (k = 0; k < LIBERTIES_BY_INDEX(s); k++) {
      int lib = STRING_LIBERTIES(s)[k];
      if (UNMARKED_LIBERTY(lib)) {
	STORE_LIBERTY_WITH_LIMIT(lib, libs, liberties, maxlib);
	MARK_LIBERTY(lib);
      }
    }
  }

  if (UNMARKED_LIBERTY(WEST(pos))) {
    STORE_LIBERTY_WITH_LIMIT(WEST(pos), libs, liberties, maxlib);
    MARK_LIBERTY(WEST(pos));
  }
  else if (board[WEST(pos)] == color) {
    int s = STRING_INDEX(WEST(pos));
    for (k = 0; k < LIBERTIES_BY_INDEX(s); k++) {
      int lib = STRING_LIBERTIES(s)[k];
      if (UNMARKED_LIBERTY(lib)) {
	STORE_LIBERTY_WITH_LIMIT(lib, libs, liberties, maxlib);
	MARK_LIBERTY(lib);
      }
    }
  }

  if (UNMARKED_LIBERTY(NORTH(pos))) {
    STORE_LIBERTY_WITH_LIMIT(NORTH(pos), libs, liberties, maxlib);
    MARK_LIBERTY(NORTH(pos));
  }
  else if (board[NORTH(pos)] == color) {
    int s = STRING_INDEX(NORTH(pos));
    for (k = 0; k < LIBERTIES_BY_INDEX(s); k++) {
      int lib = STRING_LIBERTIES(s)[k];
      if (UNMARKED_LIBERTY(lib)) {
	STORE_LIBERTY_WITH_LIMIT(lib, libs, liberties, maxlib);
	MARK_LIBERTY(lib);
      }
    }
  }

  if (UNMARKED_LIBERTY(EAST(pos))) {
#if 0
    STORE_LIBERTY_WITH_LIMIT(EAST(pos), libs, liberties, maxlib);
    MARK_LIBERTY(EAST(pos));
#else
    /* No limit checking and liberty marking since we are about to
     * leave.
     */
    STORE_LIBERTY(EAST(pos), libs, liberties);
#endif
  }
  else if (board[EAST(pos)] == color) {
    int s = STRING_INDEX(EAST(pos));
    for (k = 0; k < LIBERTIES_BY_INDEX(s); k++) {
      int lib = STRING_LIBERTIES(s)[k];
      if (UNMARKED_LIBERTY(lib)) {
	STORE_LIBERTY_WITH_LIMIT(lib, libs, liberties, maxlib);
	MARK_LIBERTY(lib);
      }
    }
  }

#if USE_BOARD_CACHES
  /* If we reach here, then we have counted _all_ the liberties, so
   * we set threshold to MAXLIBS (the result is the same regardless
   * of `maxlib' value).
   */
  if (!libs)
    private->approxlib_cache[pos][color - 1].threshold = MAXLIBS;
#endif

  return liberties;
}


/* Find the liberties a move of the given color at pos would have,
 * excluding possible captures, by traversing all adjacent friendly
 * strings. This is a fallback used by approxlib() when a faster
 * algorithm can't be used.
 */
static int
slow_approxlib(const Goban *goban, int pos, int color, int maxlib, int *libs)
{
  ACCESS_PRIVATE_DATA;
  ACCESS_BOARD_CONST;

  int k;
  int liberties = 0;

  private->liberty_mark++;
  private->string_mark++;
  MARK_LIBERTY(pos);

  for (k = 0; k < 4; k++) {
    int neighbor = pos + delta[k];

    if (UNMARKED_LIBERTY(neighbor)) {
      STORE_LIBERTY_WITH_LIMIT(neighbor, libs, liberties, maxlib);
      MARK_LIBERTY(neighbor);
    }
    else if (board[neighbor] == color && UNMARKED_STRING(neighbor)) {
      int s = STRING_INDEX(neighbor);
      int pos2 = FIRST_STONE(s);
      do {
	int l;
	for (l = 0; l < 4; l++) {
	  int neighbor2 = pos2 + delta[l];
	  if (UNMARKED_LIBERTY(neighbor2)) {
	    STORE_LIBERTY_WITH_LIMIT(neighbor2, libs, liberties, maxlib);
	    MARK_LIBERTY(neighbor2);
	  }
	}

	pos2 = NEXT_STONE(pos2);
      } while (!BACK_TO_FIRST_STONE(s, pos2));
      MARK_STRING(neighbor);
    }
  }

#if USE_BOARD_CACHES
  /* If we reach here, then we have counted _all_ the liberties, so
   * we set threshold to MAXLIBS (the result is the same regardless
   * of `maxlib' value).
   */
  if (!libs)
    goban->private->approxlib_cache[pos][color - 1].threshold = MAXLIBS;
#endif

  return liberties;
}


/* Find the liberties a stone of the given color would get if played
 * at (pos). This function takes into consideration all captures. Its
 * return value is exact in that sense it counts all the liberties,
 * unless (maxlib) allows it to stop earlier. (pos) must be empty. If
 * libs != NULL, the locations of up to maxlib liberties are written
 * into libs[]. The counting of liberties may or may not be halted
 * when maxlib is reached. The number of found liberties is returned.
 *
 * This function guarantees that liberties which are not results of
 * captures come first in libs[] array. To find whether all the
 * liberties starting from a given one are results of captures, one
 * may use  if (board[libs[k]] != EMPTY)  construction.
 *
 * If you want the number or the locations of all liberties, however
 * many they are, you should pass MAXLIBS as the value for maxlib and
 * allocate space for libs[] accordingly.
 */
int
accuratelib(const Goban *goban, int pos, int color, int maxlib, int *libs)
{
  int liberties;

#ifdef USE_BOARD_CACHES

  Board_cache_entry *entry
    = &goban->private->accuratelib_cache[pos][color - 1];

  ASSERT1(goban, goban->board[pos] == EMPTY, pos);
  ASSERT1(goban, IS_STONE(color), pos);

  if (!libs) {
    /* First see if this result is cached. */
    if (hashdata_is_equal(goban->board_hash, entry->position_hash)
	&& maxlib <= entry->threshold) {
      return entry->liberties;
    }

    liberties = fastlib(goban, pos, color, 0);
    if (liberties >= 0) {
      /* Since fastlib() always returns precise result and doesn't take
       * `maxlib' into account, we set threshold to MAXLIBS so that this
       * result is used regardless of any `maxlib' passed.
       */
      entry->threshold	   = MAXLIBS;
      entry->liberties	   = liberties;
      entry->position_hash = goban->board_hash;

      return liberties;
    }
  }

  liberties = do_accuratelib(goban, pos, color, maxlib, libs);

  /* If accuratelib() found less than `maxlib' liberties, then its
   * result is certainly independent of `maxlib' and we set threshold
   * to MAXLIBS.
   */
  entry->threshold     = liberties < maxlib ? MAXLIBS : maxlib;
  entry->liberties     = liberties;
  entry->position_hash = goban->board_hash;

#else /* not USE_BOARD_CACHES */

  ASSERT1(goban, goban->board[pos] == EMPTY, pos);
  ASSERT1(goban, IS_STONE(color), pos);

  if (!libs) {
    liberties = fastlib(goban, pos, color, 0);
    if (liberties >= 0)
      return liberties;
  }

  liberties = do_accuratelib(goban, pos, color, maxlib, libs);

#endif /* not USE_BOARD_CACHES */

  return liberties;
}


/* Does the real work of accuratelib(). */
static int
do_accuratelib(const Goban *goban, int pos, int color, int maxlib, int *libs)
{
  ACCESS_PRIVATE_DATA;
  ACCESS_BOARD_CONST;

  int k, l;
  int liberties = 0;
  int lib;
  int captured[4];
  int captures = 0;

  private->string_mark++;
  private->liberty_mark++;
  MARK_LIBERTY(pos);

  for (k = 0; k < 4; k++) {
    int pos2 = pos + delta[k];
    if (UNMARKED_LIBERTY(pos2)) {
      /* A trivial liberty */
      STORE_LIBERTY_WITH_LIMIT(pos2, libs, liberties, maxlib);
      MARK_LIBERTY(pos2);
    }
    else if (UNMARKED_COLOR_STRING(pos2, color)) {
      /* An own neighbor string */
      int string_index = STRING_INDEX(pos2);

      if (LIBERTIES_BY_INDEX(string_index) <= MAX_LIBERTIES
	  || maxlib <= MAX_LIBERTIES - 1) {
	/* The easy case - we already have all (necessary) liberties of
	 * the string listed
	 */
	for (l = 0; l < LIBERTIES_BY_INDEX(string_index); l++) {
	  lib = STRING_LIBERTIES(string_index)[l];
	  if (UNMARKED_LIBERTY(lib)) {
	    STORE_LIBERTY_WITH_LIMIT(lib, libs, liberties, maxlib);
	    MARK_LIBERTY(lib);
	  }
	}
      }
      else {
	/* The harder case - we need to find all the liberties of the
	 * string by traversing its stones. We stop as soon as we have
	 * traversed all the stones or have reached maxlib. Unfortunately,
	 * we cannot use the trick from findlib() since some of the
	 * liberties may already have been marked.
	 */
	int stone = pos2;
	do {
	  if (UNMARKED_LIBERTY(SOUTH(stone))) {
	    STORE_LIBERTY_WITH_LIMIT(SOUTH(stone), libs, liberties, maxlib);
	    MARK_LIBERTY(SOUTH(stone));
	  }

	  if (UNMARKED_LIBERTY(WEST(stone))) {
	    STORE_LIBERTY_WITH_LIMIT(WEST(stone), libs, liberties, maxlib);
	    MARK_LIBERTY(WEST(stone));
	  }

	  if (UNMARKED_LIBERTY(NORTH(stone))) {
	    STORE_LIBERTY_WITH_LIMIT(NORTH(stone), libs, liberties, maxlib);
	    MARK_LIBERTY(NORTH(stone));
	  }

	  if (UNMARKED_LIBERTY(EAST(stone))) {
	    STORE_LIBERTY_WITH_LIMIT(EAST(stone), libs, liberties, maxlib);
	    MARK_LIBERTY(EAST(stone));
	  }

	  stone = NEXT_STONE(stone);
	} while (stone != pos2);
      }

      MARK_STRING(pos2);
    }
    else if (board[pos2] == OTHER_COLOR(color) && LIBERTIES(pos2) == 1) {
      /* A capture. */
      captured[captures++] = pos2;
    }
  }

  /* Now we look at all the captures found in the previous step */
  for (k = 0; k < captures; k++) {
    lib = captured[k];

    /* Add the stone adjacent to (pos) to the list of liberties if
     * it is not also adjacent to an own marked string (otherwise,
     * it will be added later).
     */
    if (!MARKED_COLOR_STRING(SOUTH(lib), color)
	&& !MARKED_COLOR_STRING(WEST(lib), color)
	&& !MARKED_COLOR_STRING(NORTH(lib), color)
	&& !MARKED_COLOR_STRING(EAST(lib), color))
      STORE_LIBERTY_WITH_LIMIT(lib, libs, liberties, maxlib);

    /* Check if we already know of this capture. */
    for (l = 0; l < k; l++) {
      if (SAME_STRING(captured[l], lib))
	break;
    }

    if (l == k) {
      /* Traverse all the stones of the capture and add to the list
       * of liberties those, which are adjacent to at least one own
       * marked string.
       */
      do {
	if (MARKED_COLOR_STRING(SOUTH(lib), color)
	    || MARKED_COLOR_STRING(WEST(lib), color)
	    || MARKED_COLOR_STRING(NORTH(lib), color)
	    || MARKED_COLOR_STRING(EAST(lib), color))
	  STORE_LIBERTY_WITH_LIMIT(lib, libs, liberties, maxlib);

	lib = NEXT_STONE(lib);
      } while (lib != captured[k]);
    }
  }

  return liberties;
}


/* Find the number of common liberties of the two strings at str1 and str2.
 */

int
count_common_libs(const Goban *goban, int str1, int str2)
{
  ACCESS_PRIVATE_DATA;
  ACCESS_BOARD_CONST;

  int all_libs1[MAXLIBS], *libs1;
  int liberties1, liberties2;
  int commonlibs = 0;
  int k, n;

  ASSERT_ON_BOARD1(goban, str1);
  ASSERT_ON_BOARD1(goban, str2);
  ASSERT1(goban, IS_STONE(board[str1]), str1);
  ASSERT1(goban, IS_STONE(board[str2]), str2);

  n = STRING_INDEX(str1);
  liberties1 = LIBERTIES_BY_INDEX(n);

  if (liberties1 > LIBERTIES(str2)) {
    int tmp = str1;
    str1    = str2;
    str2    = tmp;

    /* The strings have been swapped.  Update variables. */
    n = STRING_INDEX(str1);
    liberties1 = LIBERTIES_BY_INDEX(n);
  }

  if (liberties1 <= MAX_LIBERTIES) {
    /* Speed optimization: don't copy liberties with findlib(). */
    libs1 = STRING_LIBERTIES(n);
    n = STRING_INDEX(str2);
    liberties2 = LIBERTIES_BY_INDEX(n);

    if (liberties2 <= MAX_LIBERTIES) {
      /* Speed optimization: NEIGHBOR_OF_STRING() is quite expensive. */
      private->liberty_mark++;
      for (k = 0; k < liberties1; k++)
	MARK_LIBERTY(libs1[k]);

      libs1 = STRING_LIBERTIES(n);
      for (k = 0; k < liberties2; k++)
	if (!UNMARKED_LIBERTY(libs1[k]))
	  commonlibs++;

      return commonlibs;
    }
  }
  else {
    findlib(goban, str1, MAXLIBS, all_libs1);
    libs1 = all_libs1;
  }

  for (k = 0; k < liberties1; k++) {
    if (NEIGHBOR_OF_STRING(libs1[k], STRING_INDEX(str2), board[str2]))
      commonlibs++;
  }

  return commonlibs;
}


/* Find the common liberties of the two strings at str1 and str2. The
 * locations of up to maxlib common liberties are written into libs[].
 * The full number of common liberties is returned.
 *
 * If you want the locations of all common liberties, whatever their
 * number, you should pass MAXLIBS as the value for maxlib and
 * allocate space for libs[] accordingly.
 */

int
find_common_libs(const Goban *goban, int str1, int str2, int maxlib, int *libs)
{
  ACCESS_PRIVATE_DATA;
  ACCESS_BOARD_CONST;

  int all_libs1[MAXLIBS], *libs1;
  int liberties1, liberties2;
  int commonlibs = 0;
  int k, n;

  ASSERT_ON_BOARD1(goban, str1);
  ASSERT_ON_BOARD1(goban, str2);
  ASSERT1(goban, IS_STONE(board[str1]), str1);
  ASSERT1(goban, IS_STONE(board[str2]), str2);
  ASSERT1(goban, libs != NULL, str1);

  n = STRING_INDEX(str1);
  liberties1 = LIBERTIES_BY_INDEX(n);

  if (liberties1 > LIBERTIES(str2)) {
    int tmp = str1;
    str1    = str2;
    str2    = tmp;

    /* The strings have been swapped.  Update variables. */
    n = STRING_INDEX(str1);
    liberties1 = LIBERTIES_BY_INDEX(n);
  }

  if (liberties1 <= MAX_LIBERTIES) {
    /* Speed optimization: don't copy liberties with findlib(). */
    libs1 = STRING_LIBERTIES(n);
    n = STRING_INDEX(str2);
    liberties2 = LIBERTIES_BY_INDEX(n);

    if (liberties2 <= MAX_LIBERTIES) {
      /* Speed optimization: NEIGHBOR_OF_STRING() is quite expensive. */
      private->liberty_mark++;
      for (k = 0; k < liberties1; k++)
	MARK_LIBERTY(libs1[k]);

      libs1 = STRING_LIBERTIES(n);
      for (k = 0; k < liberties2; k++)
	if (!UNMARKED_LIBERTY(libs1[k])) {
	  if (commonlibs < maxlib)
	    libs[commonlibs] = libs1[k];
	  commonlibs++;
	}

      return commonlibs;
    }
  }
  else {
    findlib(goban, str1, MAXLIBS, all_libs1);
    libs1 = all_libs1;
  }

  for (k = 0; k < liberties1; k++) {
    if (NEIGHBOR_OF_STRING(libs1[k], STRING_INDEX(str2), board[str2])) {
      if (commonlibs < maxlib)
	libs[commonlibs] = libs1[k];
      commonlibs++;
    }
  }

  return commonlibs;
}


/* Determine whether two strings have at least one common liberty.
 * If they do and lib != NULL, one common liberty is returned in *lib.
 */
int
have_common_lib(const Goban *goban, int str1, int str2, int *lib)
{
  ACCESS_PRIVATE_DATA;
  ACCESS_BOARD_CONST;

  int all_libs1[MAXLIBS], *libs1;
  int liberties1;
  int k, n;

  ASSERT_ON_BOARD1(goban, str1);
  ASSERT_ON_BOARD1(goban, str2);
  ASSERT1(goban, IS_STONE(board[str1]), str1);
  ASSERT1(goban, IS_STONE(board[str2]), str2);

  n = STRING_INDEX(str1);
  liberties1 = LIBERTIES_BY_INDEX(n);

  if (liberties1 > LIBERTIES(str2)) {
    int tmp = str1;
    str1    = str2;
    str2    = tmp;

    /* The strings have been swapped.  Update variables. */
    n = STRING_INDEX(str1);
    liberties1 = LIBERTIES_BY_INDEX(n);
  }

  if (liberties1 <= MAX_LIBERTIES)
    /* Speed optimization: don't copy liberties with findlib(). */
    libs1 = STRING_LIBERTIES(n);
  else {
    findlib(goban, str1, MAXLIBS, all_libs1);
    libs1 = all_libs1;
  }

  for (k = 0; k < liberties1; k++) {
    if (NEIGHBOR_OF_STRING(libs1[k], STRING_INDEX(str2), board[str2])) {
      if (lib)
	*lib = libs1[k];
      return 1;
    }
  }

  return 0;
}



/*
 * Report the number of stones in a string.
 */

int
countstones(const Goban *goban, int str)
{
  ACCESS_PRIVATE_DATA_CONST;

  ASSERT_ON_BOARD1(goban, str);
  ASSERT1(goban, IS_STONE(goban->board[str]), str);

  return COUNT_STONES(str);
}


/* Find the stones of the string at str. str must not be
 * empty. The locations of up to maxstones stones are written into
 * stones[]. The full number of stones is returned.
 */

int
findstones(const Goban *goban, int str, int maxstones, int *stones)
{
  ACCESS_PRIVATE_DATA_CONST;

  int s;
  int size;
  int pos;
  int k;

  ASSERT_ON_BOARD1(goban, str);
  ASSERT1(goban, IS_STONE(goban->board[str]), str);

  s = STRING_INDEX(str);
  size = COUNT_STONES_BY_INDEX(s);

  /* Traverse the stones of the string, by following the cyclic chain. */
  pos = FIRST_STONE(s);
  for (k = 0; k < maxstones && k < size; k++) {
    stones[k] = pos;
    pos = NEXT_STONE(pos);
  }

  return size;
}


/* Counts how many stones in str1 are directly adjacent to str2.
 * A limit can be given in the maxstones parameter so that the
 * function returns immediately. See fast_defense() in reading.c
 */

int
count_adjacent_stones(const Goban *goban, int str1, int str2, int maxstones)
{
  ACCESS_PRIVATE_DATA_CONST;
  ACCESS_BOARD_CONST;

  int s1, s2;
  int size;
  int pos;
  int k;
  int count = 0;

  ASSERT_ON_BOARD1(goban, str1);
  ASSERT1(goban, IS_STONE(board[str1]), str1);
  ASSERT_ON_BOARD1(goban, str2);
  ASSERT1(goban, IS_STONE(board[str2]), str2);

  s1 = STRING_INDEX(str1);
  s2 = STRING_INDEX(str2);
  size = COUNT_STONES_BY_INDEX(s1);

  /* Traverse the stones of the string, by following the cyclic chain. */
  pos = FIRST_STONE(s1);
  for (k = 0; k < size && count < maxstones; k++) {
    if (NEIGHBOR_OF_STRING(pos, s2, board[str2]))
      count++;
    pos = NEXT_STONE(pos);
  }

  return count;
}


/* chainlinks returns (in the (adj) array) the chains surrounding
 * the string at (str). The number of chains is returned.
 */

int
chainlinks(const Goban *goban, int str, int adj[MAXCHAIN])
{
  ACCESS_PRIVATE_DATA_CONST;

  int num_neighbors;
  const int *neighbors;
  int k;

  ASSERT1(goban, IS_STONE(goban->board[str]), str);

  /* We already have the list ready, just copy it and fill in the
   * desired information.
   */
  num_neighbors = COUNT_NEIGHBORS(str);
  neighbors	= STRING_NEIGHBORS(STRING_INDEX(str));
  for (k = 0; k < num_neighbors; k++)
    adj[k] = ORIGIN_BY_INDEX(neighbors[k]);

  return num_neighbors;
}


/* chainlinks2 returns (in adj array) those chains surrounding
 * the string at str which have exactly lib liberties. The number
 * of such chains is returned.
 */

int
chainlinks2(const Goban *goban, int str, int adj[MAXCHAIN], int lib)
{
  ACCESS_PRIVATE_DATA_CONST;

  int num_neighbors;
  int num_all_neighbors;
  const int *neighbors;
  int k;

  ASSERT1(goban, IS_STONE(goban->board[str]), str);

  /* We already have the list ready, just copy the strings with the
   * right number of liberties.
   */
  num_neighbors	    = 0;
  num_all_neighbors = COUNT_NEIGHBORS(str);
  neighbors	    = STRING_NEIGHBORS(STRING_INDEX(str));

  for (k = 0; k < num_all_neighbors; k++) {
    if (LIBERTIES_BY_INDEX(neighbors[k]) == lib)
      adj[num_neighbors++] = ORIGIN_BY_INDEX(neighbors[k]);
  }

  return num_neighbors;
}


/* chainlinks3 returns (in adj array) those chains surrounding
 * the string at str, which have less or equal lib liberties.
 * The number of such chains is returned.
 */

int
chainlinks3(const Goban *goban, int str, int adj[MAXCHAIN], int lib)
{
  ACCESS_PRIVATE_DATA_CONST;

  int num_neighbors;
  int num_all_neighbors;
  const int *neighbors;
  int k;

  ASSERT1(goban, IS_STONE(goban->board[str]), str);

  /* We already have the list ready, just copy the strings with the
   * right number of liberties.
   */
  num_neighbors	    = 0;
  num_all_neighbors = COUNT_NEIGHBORS(str);
  neighbors	    = STRING_NEIGHBORS(STRING_INDEX(str));

  for (k = 0; k < num_all_neighbors; k++) {
    if (LIBERTIES_BY_INDEX(neighbors[k]) <= lib)
      adj[num_neighbors++] = ORIGIN_BY_INDEX(neighbors[k]);
  }

  return num_neighbors;
}


/* extended_chainlinks() returns (in the (adj) array) the opponent
 * strings being directly adjacent to (str) or having a common liberty
 * with (str). The number of such strings is returned.
 *
 * If the both_colors parameter is true, also own strings sharing a
 * liberty are returned.
 */

int
extended_chainlinks(const Goban *goban, int str, int adj[MAXCHAIN],
		    int both_colors)
{
  ACCESS_PRIVATE_DATA;
  ACCESS_BOARD_CONST;

  int string_index;
  int n;
  int k;
  int r;
  int libs[MAXLIBS];
  int liberties;

  ASSERT1(goban, IS_STONE(board[str]), str);

  string_index = STRING_INDEX(str);

  /* We already have the list of directly adjacent strings ready, just
   * copy it and mark the strings.
   */
  private->string_mark++;
  for (n = 0; n < COUNT_NEIGHBORS_BY_INDEX(string_index); n++) {
    adj[n] = ORIGIN_BY_INDEX(STRING_NEIGHBORS(string_index)[n]);
    MARK_STRING(adj[n]);
  }

  /* Get the liberties. */
  liberties = findlib(goban, str, MAXLIBS, libs);

  /* Look for unmarked opponent strings next to a liberty and add the
   * ones which are found to the output.
   */
  for (r = 0; r < liberties; r++) {
    for (k = 0; k < 4; k++) {
      int neighbor = libs[r] + delta[k];
      if ((board[neighbor] == OTHER_COLOR(board[str])
	   || (both_colors && board[neighbor] == board[str]))
	  && UNMARKED_STRING(neighbor)) {
	adj[n] = ORIGIN(neighbor);
	MARK_STRING(adj[n]);
	n++;
      }
    }
  }

  return n;
}


/*
 * Find the origin of a worm, i.e. the point with the
 * smallest 1D board coordinate. The idea is to have a canonical
 * reference point for a string.
 */

int
find_origin(const Goban *goban, int str)
{
  ACCESS_PRIVATE_DATA_CONST;

  ASSERT1(goban, IS_STONE(goban->board[str]), str);

  return ORIGIN(str);
}


/* Determine whether a move by color at (pos) would be a self atari,
 * i.e. whether it would get more than one liberty. This function
 * returns true also for the case of a suicide move.
 */

int
is_self_atari(const Goban *goban, int pos, int color)
{
  ACCESS_PRIVATE_DATA;
  ACCESS_BOARD_CONST;

  int other = OTHER_COLOR(color);

  /* Number of empty neighbors. */
  int trivial_liberties = 0;

  /* Number of captured opponent strings. */
  int captures = 0;

  /* Whether there is a friendly neighbor with a spare liberty. If it
   * has more than one spare liberty we immediately return 0.
   */
  int far_liberties = 0;

  ASSERT_ON_BOARD1(goban, pos);
  ASSERT1(goban, board[pos] == EMPTY, pos);
  ASSERT1(goban, IS_STONE(color), pos);

  /* 1. Try first to solve the problem without much work. */
  private->string_mark++;

  if (LIBERTY(SOUTH(pos)))
    trivial_liberties++;
  else if (board[SOUTH(pos)] == color) {
    if (LIBERTIES(SOUTH(pos)) > 2)
      return 0;
    if (LIBERTIES(SOUTH(pos)) == 2)
      far_liberties++;
  }
  else if (board[SOUTH(pos)] == other
	  && LIBERTIES(SOUTH(pos)) == 1 && UNMARKED_STRING(SOUTH(pos))) {
    captures++;
    MARK_STRING(SOUTH(pos));
  }

  if (LIBERTY(WEST(pos)))
    trivial_liberties++;
  else if (board[WEST(pos)] == color) {
    if (LIBERTIES(WEST(pos)) > 2)
      return 0;
    if (LIBERTIES(WEST(pos)) == 2)
      far_liberties++;
  }
  else if (board[WEST(pos)] == other
	  && LIBERTIES(WEST(pos)) == 1 && UNMARKED_STRING(WEST(pos))) {
    captures++;
    MARK_STRING(WEST(pos));
  }

  if (LIBERTY(NORTH(pos)))
    trivial_liberties++;
  else if (board[NORTH(pos)] == color) {
    if (LIBERTIES(NORTH(pos)) > 2)
      return 0;
    if (LIBERTIES(NORTH(pos)) == 2)
      far_liberties++;
  }
  else if (board[NORTH(pos)] == other
	  && LIBERTIES(NORTH(pos)) == 1 && UNMARKED_STRING(NORTH(pos))) {
    captures++;
    MARK_STRING(NORTH(pos));
  }

  if (LIBERTY(EAST(pos)))
    trivial_liberties++;
  else if (board[EAST(pos)] == color) {
    if (LIBERTIES(EAST(pos)) > 2)
      return 0;
    if (LIBERTIES(EAST(pos)) == 2)
      far_liberties++;
  }
  else if (board[EAST(pos)] == other
	  && LIBERTIES(EAST(pos)) == 1 && UNMARKED_STRING(EAST(pos))) {
    captures++;
#if 0
    MARK_STRING(EAST(pos));
#endif
  }

  /* Each captured string is guaranteed to produce at least one
   * liberty. These are disjoint from both trivial liberties and far
   * liberties. The two latter may however coincide.
   */
  if (trivial_liberties + captures >= 2)
    return 0;

  if ((far_liberties > 0) + captures >= 2)
    return 0;

  if (captures == 0 && far_liberties + trivial_liberties <= 1)
    return 1;

  /* 2. It was not so easy.  We use accuratelib() in this case. */
  return accuratelib(goban, pos, color, 2, NULL) <= 1;
}


/*
 * Returns true if pos is a liberty of the string at str.
 */

int
liberty_of_string(const Goban *goban, int pos, int str)
{
  ACCESS_PRIVATE_DATA_CONST;
  ACCESS_BOARD_CONST;

  ASSERT_ON_BOARD1(goban, pos);
  ASSERT_ON_BOARD1(goban, str);
  if (IS_STONE(board[pos]))
    return 0;

  return NEIGHBOR_OF_STRING(pos, STRING_INDEX(str), board[str]);
}


/*
 * Returns true if pos is a second order liberty of the string at str.
 */
int
second_order_liberty_of_string(const Goban *goban, int pos, int str)
{
  ACCESS_PRIVATE_DATA_CONST;
  ACCESS_BOARD_CONST;

  int k;
  ASSERT_ON_BOARD1(goban, pos);
  ASSERT_ON_BOARD1(goban, str);

  for (k = 0; k < 4; k++)
    if (board[pos + delta[k]] == EMPTY
	&& NEIGHBOR_OF_STRING(pos + delta[k], STRING_INDEX(str), board[str]))
      return 1;

  return 0;
}


/*
 * Returns true if pos is adjacent to the string at str.
 */

int
neighbor_of_string(const Goban *goban, int pos, int str)
{
  ACCESS_PRIVATE_DATA_CONST;
  ACCESS_BOARD_CONST;

  int color = board[str];

  ASSERT1(goban, IS_STONE(color), str);
  ASSERT_ON_BOARD1(goban, pos);

  return NEIGHBOR_OF_STRING(pos, STRING_INDEX(str), color);
}

/*
 * Returns true if (pos) has a neighbor of color (color).
 */

int
has_neighbor(const Goban *goban, int pos, int color)
{
  ACCESS_BOARD_CONST;

  ASSERT_ON_BOARD1(goban, pos);
  ASSERT1(goban, IS_STONE(color), pos);

  return (board[SOUTH(pos)] == color
	  || board[WEST(pos)] == color
	  || board[NORTH(pos)] == color
	  || board[EAST(pos)] == color);
}

/*
 * Returns true if str1 and str2 belong to the same string.
 */

int
same_string(const Goban *goban, int str1, int str2)
{
  ACCESS_PRIVATE_DATA_CONST;

  ASSERT_ON_BOARD1(goban, str1);
  ASSERT_ON_BOARD1(goban, str2);
  ASSERT1(goban, IS_STONE(goban->board[str1]), str1);
  ASSERT1(goban, IS_STONE(goban->board[str2]), str2);

  return SAME_STRING(str1, str2);
}


/*
 * Returns true if the strings at str1 and str2 are adjacent.
 */

int
adjacent_strings(const Goban *goban, int str1, int str2)
{
  ACCESS_PRIVATE_DATA_CONST;

  int s1, s2;
  int k;

  ASSERT_ON_BOARD1(goban, str1);
  ASSERT_ON_BOARD1(goban, str2);
  ASSERT1(goban, IS_STONE(goban->board[str1]), str1);
  ASSERT1(goban, IS_STONE(goban->board[str2]), str2);

  s1 = STRING_INDEX(str1);
  s2 = STRING_INDEX(str2);

  for (k = 0; k < COUNT_NEIGHBORS_BY_INDEX(s1); k++) {
    if (STRING_NEIGHBORS(s1)[k] == s2)
      return 1;
  }

  return 0;
}


/*
 * Return true if the move (pos) by (color) is a ko capture
 * (whether capture is legal on this move or not). If so,
 * and if ko_pos is not a NULL pointer, then
 * *ko_pos returns the location of the captured ko stone.
 * If the move is not a ko capture, *ko_pos is set to 0.
 *
 * A move is a ko capture if and only if
 *    1. All neighbors are opponent stones.
 *    2. The number of captured stones is exactly one.
 */

int
is_ko(const Goban *goban, int pos, int color, int *ko_pos)
{
  ACCESS_PRIVATE_DATA_CONST;
  ACCESS_BOARD_CONST;

  int other = OTHER_COLOR(color);
  int captures = 0;
  int kpos = 0;

  ASSERT_ON_BOARD1(goban, pos);
  ASSERT1(goban, color == WHITE || color == BLACK, pos);

  if (ON_BOARD(goban, SOUTH(pos))) {
    if (board[SOUTH(pos)] != other)
      return 0;
    else if (LIBERTIES(SOUTH(pos)) == 1) {
      kpos = SOUTH(pos);
      captures += COUNT_STONES(SOUTH(pos));
      if (captures > 1)
	return 0;
    }
  }

  if (ON_BOARD(goban, WEST(pos))) {
    if (board[WEST(pos)] != other)
      return 0;
    else if (LIBERTIES(WEST(pos)) == 1) {
      kpos = WEST(pos);
      captures += COUNT_STONES(WEST(pos));
      if (captures > 1)
	return 0;
    }
  }

  if (ON_BOARD(goban, NORTH(pos))) {
    if (board[NORTH(pos)] != other)
      return 0;
    else if (LIBERTIES(NORTH(pos)) == 1) {
      kpos = NORTH(pos);
      captures += COUNT_STONES(NORTH(pos));
      if (captures > 1)
	return 0;
    }
  }

  if (ON_BOARD(goban, EAST(pos))) {
    if (board[EAST(pos)] != other)
      return 0;
    else if (LIBERTIES(EAST(pos)) == 1) {
      kpos = EAST(pos);
      captures += COUNT_STONES(EAST(pos));
      if (captures > 1)
	return 0;
    }
  }

  if (captures == 1) {
    if (ko_pos)
      *ko_pos = kpos;
    return 1;
  }
  return 0;
}


/* Return true if pos is either a stone, which if captured would give
 * ko, or if pos is an empty intersection adjacent to a ko stone.
 */
int
is_ko_point(const Goban *goban, int pos)
{
  ACCESS_BOARD_CONST;

  ASSERT_ON_BOARD1(goban, pos);

  if (board[pos] == EMPTY) {
    int color;
    if (ON_BOARD(goban, SOUTH(pos)))
      color = board[SOUTH(pos)];
    else
      color = board[NORTH(pos)];
    if (IS_STONE(color) && is_ko(goban, pos, OTHER_COLOR(color), NULL))
      return 1;
  }
  else {
    ACCESS_PRIVATE_DATA_CONST;

    int string_index = STRING_INDEX(pos);
    if (LIBERTIES_BY_INDEX(string_index)
	&& COUNT_STONES_BY_INDEX(string_index) == 1
	&& is_ko(goban, STRING_LIBERTIES(string_index)[0],
		 OTHER_COLOR(STRING_DATA_BY_INDEX(string_index)->color), NULL))
      return 1;
  }

  return 0;
}


/* Returns 1 if at least one string is captured when color plays at pos.
 */
int
does_capture_something(const Goban *goban, int pos, int color)
{
  ACCESS_PRIVATE_DATA_CONST;
  ACCESS_BOARD_CONST;

  int other = OTHER_COLOR(color);

  ASSERT1(goban, board[pos] == EMPTY, pos);

  if (board[SOUTH(pos)] == other && LIBERTIES(SOUTH(pos)) == 1)
    return 1;

  if (board[WEST(pos)] == other && LIBERTIES(WEST(pos)) == 1)
    return 1;

  if (board[NORTH(pos)] == other && LIBERTIES(NORTH(pos)) == 1)
    return 1;

  if (board[EAST(pos)] == other && LIBERTIES(EAST(pos)) == 1)
    return 1;

  return 0;
}


/* For each stone in the string at pos, set mx to value mark. */
void
mark_string(const Goban *goban, int str, char mx[BOARDMAX], char mark)
{
  ACCESS_PRIVATE_DATA_CONST;

  int pos = str;

  ASSERT1(goban, IS_STONE(goban->board[str]), str);

  do {
    mx[pos] = mark;
    pos = NEXT_STONE(pos);
  } while (pos != str);
}


/* Signed char variant of the function above.
 * FIXME: Do we want to convert all mark_string() to signed char?
 */
void
signed_mark_string(const Goban *goban, int str,
		   signed char mx[BOARDMAX], signed char mark)
{
  ACCESS_PRIVATE_DATA_CONST;

  int pos = str;

  ASSERT1(goban, IS_STONE(goban->board[str]), str);

  do {
    mx[pos] = mark;
    pos = NEXT_STONE(pos);
  } while (pos != str);
}


/* Returns true if at least one move has been played at pos
 * at deeper than level 'cutoff' in the reading tree.
 */
int
move_in_stack(const Goban *goban, int pos, int cutoff)
{
  int k;
  for (k = cutoff; k < goban->stackp; k++)
    if (goban->private->stack[k] == pos)
      return 1;

  return 0;
}


/* Retrieve a move from the move stack. */
void
get_move_from_stack(const Goban *goban, int k, int *move, int *color)
{
  gg_assert(goban, k < goban->stackp);
  *move	 = goban->private->stack[k];
  *color = goban->private->move_color[k];
}


/* Return the number of stones of the indicated color(s) on the board.
 * This only counts stones in the permanent position, not stones placed
 * by trymove() or tryko(). Use stones_on_board(BLACK | WHITE) to get
 * the total number of stones on the board.
 *
 * FIXME: This seems wrong, it uses the modified board, not the permanent
 * one. /ab
 */
int
stones_on_board(const Goban *goban, int color)
{
  ACCESS_PRIVATE_DATA;

  gg_assert(goban, goban->stackp == 0);

  if (private->stone_count_for_position != goban->position_number) {
    int pos;

    private->white_stones_on_board = 0;
    private->black_stones_on_board = 0;

    for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
      if (goban->board[pos] == BLACK)
	private->black_stones_on_board++;
      else if (goban->board[pos] == WHITE)
	private->white_stones_on_board++;
    }

    private->stone_count_for_position = goban->position_number;
  }

  return ((color & WHITE ? private->white_stones_on_board : 0)
	  + (color & BLACK ? private->black_stones_on_board : 0));
}


/* ===================== Statistics  ============================= */


/* Clear statistics. */
void
reset_trymove_counter(const Goban *goban)
{
  goban->private->trymove_counter = 0;
}


/* Retrieve statistics. */
int
get_trymove_counter(const Goban *goban)
{
  return goban->private->trymove_counter;
}


/* ================================================================ */
/*                      Lower level functions                       */
/* ================================================================ */


/* This function should be called if the board is modified by other
 * means than do_play_move() or undo_trymove().
 *
 * We have reached a new position. Increase the position counter and
 * re-initialize the incremental strings.
 *
 * Set up incremental board structures and populate them with the
 * strings available in the position given by board[]. Clear the stacks
 * and start the mark numbers from zero. All undo information is lost
 * by calling this function.
 */

static void
new_position(Goban *goban)
{
  ACCESS_PRIVATE_DATA;
  ACCESS_BOARD_CONST;

  int pos;
  int s;

  goban->position_number++;
  private->next_string	= 0;
  private->liberty_mark = 0;
  private->string_mark	= 0;
  CLEAR_STACKS();

  /* FIXME: Probably better to explicitly reset necessary fields. */
  memset(private->strings, 0, sizeof private->strings);

  memset(private->string_liberties, 0, sizeof private->string_liberties);
  memset(private->string_neighbors, 0, sizeof private->string_neighbors);
  memset(private->last_liberty_marks, 0, sizeof private->last_liberty_marks);

  VALGRIND_MAKE_WRITABLE(private->next_stone, sizeof private->next_stone);

  /* propagate_string() relies on non-assigned stones to have
   * string_index of -1.
   */
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (ON_BOARD(goban, pos))
      STRING_INDEX(pos) = -1;
  }

  /* Find the existing strings. */
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (!ON_BOARD(goban, pos))
      continue;

    if (IS_STONE(board[pos]) && STRING_INDEX(pos) == -1) {
      String_data *string_data;

      STRING_INDEX(pos) = private->next_string++;
      string_data	= STRING_DATA(pos);

      string_data->size   = propagate_string(goban, pos, pos);
      string_data->color  = board[pos];
      string_data->origin = pos;
      string_data->mark   = 0;
    }
  }

  /* Fill in liberty and neighbor info. */
  for (s = 0; s < private->next_string; s++)
    find_liberties_and_neighbors(goban, s);
}


#if 0

/*
 * Debug function. Dump all string information.
 */

static void
dump_incremental_board(const Goban *goban)
{
  ACCESS_PRIVATE_DATA_CONST;

  int pos;
  int s;
  int i;

  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (!ON_BOARD(goban, pos))
      continue;
    if (goban->board[pos] == EMPTY)
      fprintf(stderr, " . ");
    else
      fprintf(stderr, "%2d ", STRING_INDEX(pos));
    fprintf(stderr, "\n");
  }

  for (s = 0; s < private->next_string; s++) {
    const String_data *string_data = STRING_DATA_BY_INDEX(s);
    if (goban->board[string_data->origin] == EMPTY)
      continue;

    gprintf(goban, "%o%d %s %1m size %d, %d liberties, %d neighbors\n",
	    s, color_to_string(string_data->color),
	    string_data->origin, string_data->size,
	    string_data->num_liberties, string_data->num_neighbors);
    gprintf(goban, "%ostones:");

    pos = FIRST_STONE(s);
    do {
      gprintf(goban, "%o %1m", pos);
      pos = NEXT_STONE(pos);
    } while (!BACK_TO_FIRST_STONE(s, pos));

    gprintf(goban, "%o\nliberties:");
    for (i = 0; i < string_data->num_liberties; i++)
      gprintf(goban, "%o %1m", string[s].libs[i]);

    gprintf(goban, "%o\nneighbors:");
    for (i = 0; i < string_data->num_neighbors; i++)
      gprintf(goban, "%o %d(%1m)", string[s].neighborlist[i],
	      string[string[s].neighborlist[i]].origin);
    gprintf(goban, "%o\n\n");
  }
}
#endif


/* Build a string and its cyclic list representation from scratch.
 * propagate_string(stone, str) adds the stone (stone) to the string
 * (str) and recursively continues with not already included friendly
 * neighbors. To start a new string at (stone), use
 * propagate_string(stone, stone). The size of the string is returned.
 */

static int
propagate_string(const Goban *goban, int stone, int str)
{
  ACCESS_PRIVATE_DATA;

  int size = 1;
  int k;

  if (stone == str) {
    /* Start a new string. */
    NEXT_STONE(stone) = stone;
  }
  else {
    /* Link the stone at (stone) to the string including (str) */
    STRING_INDEX(stone) = STRING_INDEX(str);
    NEXT_STONE(stone)	= NEXT_STONE(str);
    NEXT_STONE(str)	= stone;
  }

  /* Look in all four directions for more stones to add. */
  for (k = 0; k < 4; k++) {
    int neighbor = stone + delta[k];
    if (ON_BOARD(goban, neighbor)
	&& goban->board[neighbor] == goban->board[stone]
	&& STRING_INDEX(neighbor) == -1)
      size += propagate_string(goban, neighbor, str);
  }

  return size;
}


/* Build the lists of liberties and neighbors of a string from
 * scratch. No information is pushed onto the stack by this function.
 */

static void
find_liberties_and_neighbors(const Goban *goban, int string_index)
{
  ACCESS_PRIVATE_DATA;
  ACCESS_BOARD_CONST;

  int pos;
  int other = OTHER_COLOR(STRING_DATA_BY_INDEX(string_index)->color);

  /* Clear the marks. */
  private->liberty_mark++;
  private->string_mark++;

  /* Traverse the stones of the string, by following the cyclic chain. */
  pos = FIRST_STONE(string_index);
  do {
    /* Look in each direction for new liberties or new neighbors. Mark
     * already visited liberties and neighbors.
     */
    if (UNMARKED_LIBERTY(SOUTH(pos)))
      ADD_AND_MARK_LIBERTY(string_index, SOUTH(pos));
    else if (UNMARKED_COLOR_STRING(SOUTH(pos), other)) {
      ADD_NEIGHBOR(string_index, SOUTH(pos));
      MARK_STRING(SOUTH(pos));
    }

    if (UNMARKED_LIBERTY(WEST(pos)))
      ADD_AND_MARK_LIBERTY(string_index, WEST(pos));
    else if (UNMARKED_COLOR_STRING(WEST(pos), other)) {
      ADD_NEIGHBOR(string_index, WEST(pos));
      MARK_STRING(WEST(pos));
    }

    if (UNMARKED_LIBERTY(NORTH(pos)))
      ADD_AND_MARK_LIBERTY(string_index, NORTH(pos));
    else if (UNMARKED_COLOR_STRING(NORTH(pos), other)) {
      ADD_NEIGHBOR(string_index, NORTH(pos));
      MARK_STRING(NORTH(pos));
    }

    if (UNMARKED_LIBERTY(EAST(pos)))
      ADD_AND_MARK_LIBERTY(string_index, EAST(pos));
    else if (UNMARKED_COLOR_STRING(EAST(pos), other)) {
      ADD_NEIGHBOR(string_index, EAST(pos));
      MARK_STRING(EAST(pos));
    }

    pos = NEXT_STONE(pos);
  } while (!BACK_TO_FIRST_STONE(string_index, pos));
}


/* Update the liberties of a string from scratch, first pushing the
 * old information.
 */

static void
update_liberties(const Goban *goban, int s)
{
  ACCESS_PRIVATE_DATA;
  ACCESS_BOARD_CONST;

  int pos;
  int k;

  /* Push the old information. */
  PUSH_VALUE(LIBERTIES_BY_INDEX(s));
  for (k = 0; k < LIBERTIES_BY_INDEX(s) && k < MAX_LIBERTIES; k++)
    PUSH_VALUE(STRING_LIBERTIES(s)[k]);

  LIBERTIES_BY_INDEX(s) = 0;

  /* Clear the liberty mark. */
  private->liberty_mark++;

  /* Traverse the stones of the string, by following the cyclic chain. */
  pos = FIRST_STONE(s);
  do {
    /* Look in each direction for new liberties. Mark already visited
     * liberties.
     */
    if (UNMARKED_LIBERTY(SOUTH(pos)))
      ADD_AND_MARK_LIBERTY(s, SOUTH(pos));

    if (UNMARKED_LIBERTY(WEST(pos)))
      ADD_AND_MARK_LIBERTY(s, WEST(pos));

    if (UNMARKED_LIBERTY(NORTH(pos)))
      ADD_AND_MARK_LIBERTY(s, NORTH(pos));

    if (UNMARKED_LIBERTY(EAST(pos)))
      ADD_AND_MARK_LIBERTY(s, EAST(pos));

    pos = NEXT_STONE(pos);
  } while (!BACK_TO_FIRST_STONE(s, pos));
}


/* Remove a string from the list of neighbors and push the changed
 * information.
 */

static void
remove_neighbor(const Goban *goban, int string_index, int n)
{
  ACCESS_PRIVATE_DATA;

  int k;
  int *neighbors = STRING_NEIGHBORS(string_index);
  String_data *string_data = STRING_DATA_BY_INDEX(string_index);

  for (k = 0; k < string_data->num_neighbors; k++) {
    if (neighbors[k] == n) {
      /* We need to push the last entry too because it may become
       * destroyed later.
       */
      PUSH_VALUE(neighbors[string_data->num_neighbors - 1]);
      PUSH_VALUE(neighbors[k]);
      PUSH_VALUE(string_data->num_neighbors);
      neighbors[k] = neighbors[--string_data->num_neighbors];

      return;
    }
  }

  gg_assert(goban, 0);
}


/* Remove one liberty from the list of liberties, pushing changed
 * information. If the string had more liberties than the size of the
 * list, rebuild the list from scratch.
 */

static void
remove_liberty(const Goban *goban, int string_index, int pos)
{
  ACCESS_PRIVATE_DATA;

  int k;
  String_data *string_data = STRING_DATA_BY_INDEX(string_index);

  if (string_data->num_liberties > MAX_LIBERTIES)
    update_liberties(goban, string_index);
  else {
    int *liberties = STRING_LIBERTIES(string_index);

    for (k = 0; k < string_data->num_liberties; k++) {
      if (liberties[k] == pos) {
	/* We need to push the last entry too because it may become
	 * destroyed later.
	 */
	PUSH_VALUE(liberties[string_data->num_liberties - 1]);
	PUSH_VALUE(liberties[k]);
	PUSH_VALUE(string_data->num_liberties);
	liberties[k] = liberties[--string_data->num_liberties];

	break;
      }
    }
  }
}


/* Remove a string from the board, pushing necessary information to
 * restore it. Return the number of removed stones.
 */

static int
do_remove_string(Goban *goban, int s)
{
  ACCESS_PRIVATE_DATA;
  ACCESS_BOARD;

  int pos;
  int k;
  int size = COUNT_STONES_BY_INDEX(s);

  /* Traverse the stones of the string, by following the cyclic chain. */
  pos = FIRST_STONE(s);
  do {
    /* Push color, string number and cyclic chain pointers. */
    PUSH_VALUE(STRING_INDEX(pos));
    PUSH_VALUE(NEXT_STONE(pos));
    DO_REMOVE_STONE(pos);
    pos = NEXT_STONE(pos);
  } while (!BACK_TO_FIRST_STONE(s, pos));

  /* The neighboring strings have obtained some new liberties and lost
   * a neighbor.  For speed reasons we handle two most common cases
   * when string size is 1 or 2 stones here instead of calling
   * update_liberties().
   */
  if (size == 1) {
    for (k = 0; k < COUNT_NEIGHBORS_BY_INDEX(s); k++) {
      int neighbor = STRING_NEIGHBORS(s)[k];

      remove_neighbor(goban, neighbor, s);
      PUSH_VALUE(LIBERTIES_BY_INDEX(neighbor));

      if (LIBERTIES_BY_INDEX(neighbor) < MAX_LIBERTIES)
	STRING_LIBERTIES(neighbor)[LIBERTIES_BY_INDEX(neighbor)] = pos;

      LIBERTIES_BY_INDEX(neighbor)++;
    }
  }
  else if (size == 2) {
    int other = OTHER_COLOR(STRING_DATA_BY_INDEX(s)->color);
    int pos2 = NEXT_STONE(pos);

    for (k = 0; k < COUNT_NEIGHBORS_BY_INDEX(s); k++) {
      int neighbor = STRING_NEIGHBORS(s)[k];

      remove_neighbor(goban, neighbor, s);
      PUSH_VALUE(LIBERTIES_BY_INDEX(neighbor));

      if (NEIGHBOR_OF_STRING(pos, neighbor, other)) {
	if (LIBERTIES_BY_INDEX(neighbor) < MAX_LIBERTIES)
	  STRING_LIBERTIES(neighbor)[LIBERTIES_BY_INDEX(neighbor)] = pos;

	LIBERTIES_BY_INDEX(neighbor)++;
      }

      if (NEIGHBOR_OF_STRING(pos2, neighbor, other)) {
	if (LIBERTIES_BY_INDEX(neighbor) < MAX_LIBERTIES)
	  STRING_LIBERTIES(neighbor)[LIBERTIES_BY_INDEX(neighbor)] = pos2;

	LIBERTIES_BY_INDEX(neighbor)++;
      }
    }
  }
  else {
    for (k = 0; k < COUNT_NEIGHBORS_BY_INDEX(s); k++) {
      remove_neighbor(goban, STRING_NEIGHBORS(s)[k], s);
      update_liberties(goban, STRING_NEIGHBORS(s)[k]);
    }
  }

  /* Update the number of captured stones. These are assumed to
   * already have been pushed.
   */
  if (STRING_DATA_BY_INDEX(s)->color == WHITE)
    goban->white_captured += size;
  else
    goban->black_captured += size;

  return size;
}


/* We have played an isolated new stone and need to create a new
 * string for it.
 */
static void
create_new_string(const Goban *goban, int pos)
{
  ACCESS_PRIVATE_DATA;
  ACCESS_BOARD_CONST;

  int s;
  String_data *string_data;
  int color = board[pos];
  int other = OTHER_COLOR(color);

  /* Get the next free string number. */
  PUSH_VALUE(private->next_string);
  s = private->next_string++;
  STRING_INDEX(pos) = s;
  string_data = STRING_DATA_BY_INDEX(s);

  /* Set up a size one cycle for the string. */
  NEXT_STONE(pos) = pos;

  /* Set trivially known values and initialize the rest to zero. */
  string_data->color	     = color;
  string_data->size	     = 1;
  string_data->origin	     = pos;
  string_data->num_liberties = 0;
  string_data->num_neighbors = 0;
  string_data->mark	     = 0;

  /* Clear the string mark. */
  private->string_mark++;

  /* In each direction, look for a liberty or a nonmarked opponent
   * neighbor. Mark visited neighbors. There is no need to mark the
   * liberties since we can't find them twice. */
  if (LIBERTY(SOUTH(pos)))
    ADD_LIBERTY(s, SOUTH(pos));
  else if (UNMARKED_COLOR_STRING(SOUTH(pos), other)) {
    int s2 = STRING_INDEX(SOUTH(pos));

    /* Add the neighbor to our list. */
    ADD_NEIGHBOR(s, SOUTH(pos));

    /* Add us to our neighbor's list. */
    PUSH_VALUE(COUNT_NEIGHBORS_BY_INDEX(s2));
    ADD_NEIGHBOR(s2, pos);
    MARK_STRING(SOUTH(pos));
  }

  if (LIBERTY(WEST(pos)))
    ADD_LIBERTY(s, WEST(pos));
  else if (UNMARKED_COLOR_STRING(WEST(pos), other)) {
    int s2 = STRING_INDEX(WEST(pos));

    /* Add the neighbor to our list. */
    ADD_NEIGHBOR(s, WEST(pos));

    /* Add us to our neighbor's list. */
    PUSH_VALUE(COUNT_NEIGHBORS_BY_INDEX(s2));
    ADD_NEIGHBOR(s2, pos);
    MARK_STRING(WEST(pos));
  }

  if (LIBERTY(NORTH(pos)))
    ADD_LIBERTY(s, NORTH(pos));
  else if (UNMARKED_COLOR_STRING(NORTH(pos), other)) {
    int s2 = STRING_INDEX(NORTH(pos));

    /* Add the neighbor to our list. */
    ADD_NEIGHBOR(s, NORTH(pos));

    /* Add us to our neighbor's list. */
    PUSH_VALUE(COUNT_NEIGHBORS_BY_INDEX(s2));
    ADD_NEIGHBOR(s2, pos);
    MARK_STRING(NORTH(pos));
  }

  if (LIBERTY(EAST(pos)))
    ADD_LIBERTY(s, EAST(pos));
  else if (UNMARKED_COLOR_STRING(EAST(pos), other)) {
    int s2 = STRING_INDEX(EAST(pos));

    /* Add the neighbor to our list. */
    ADD_NEIGHBOR(s, EAST(pos));

    /* Add us to our neighbor's list. */
    PUSH_VALUE(COUNT_NEIGHBORS_BY_INDEX(s2));
    ADD_NEIGHBOR(s2, pos);

    /* No need to mark since no visits left. */
#if 0
    MARK_STRING(EAST(pos));
#endif
  }
}


/* We have played a stone with exactly one friendly neighbor. Add the
 * new stone to that string.
 */
static void
extend_neighbor_string(const Goban *goban, int pos, int s)
{
  ACCESS_PRIVATE_DATA;
  ACCESS_BOARD_CONST;

  int k;
  int liberties_updated = 0;
  int color = board[pos];
  int other = OTHER_COLOR(color);

  /* Link in the stone in the cyclic list. */
  int	pos2	   = ORIGIN_BY_INDEX(s);
  NEXT_STONE(pos)  = NEXT_STONE(pos2);
  PUSH_VALUE(NEXT_STONE(pos2));
  NEXT_STONE(pos2) = pos;

  /* Do we need to update the origin? */
  if (pos < pos2) {
    PUSH_VALUE(ORIGIN_BY_INDEX(s));
    ORIGIN_BY_INDEX(s) = pos;
  }

  STRING_INDEX(pos) = s;

  /* The size of the string has increased by one. */
  PUSH_VALUE(COUNT_STONES_BY_INDEX(s));
  COUNT_STONES_BY_INDEX(s)++;

  /* If s has too many liberties, we don't know where they all are and
   * can't update the liberties with the algorithm we otherwise
   * use. In that case we can only recompute the liberties from
   * scratch.
   */
  if (LIBERTIES_BY_INDEX(s) > MAX_LIBERTIES) {
    update_liberties(goban, s);
    liberties_updated = 1;
  }
  else {
    /* The place of the new stone is no longer a liberty. */
    remove_liberty(goban, s, pos);
  }

  /* Mark old neighbors of the string. */
  private->string_mark++;
  for (k = 0; k < COUNT_NEIGHBORS_BY_INDEX(s); k++) {
    STRING_DATA_BY_INDEX(STRING_NEIGHBORS(s)[k])->mark
      = private->string_mark;
  }

  /* Look at the neighbor locations of pos for new liberties and/or
   * neighbor strings.
   */

  /* If we find a liberty, look two steps away to determine whether
   * this already is a liberty of s.
   */
  if (LIBERTY(SOUTH(pos))) {
    if (!liberties_updated
	&& !NON_SOUTH_NEIGHBOR_OF_STRING(SOUTH(pos), s, color))
      ADD_LIBERTY(s, SOUTH(pos));
  }
  else if (UNMARKED_COLOR_STRING(SOUTH(pos), other)) {
    int s2 = STRING_INDEX(SOUTH(pos));
    PUSH_VALUE(COUNT_NEIGHBORS_BY_INDEX(s));
    ADD_NEIGHBOR(s, SOUTH(pos));
    PUSH_VALUE(COUNT_NEIGHBORS_BY_INDEX(s2));
    ADD_NEIGHBOR(s2, pos);
    MARK_STRING(SOUTH(pos));
  }

  if (LIBERTY(WEST(pos))) {
    if (!liberties_updated
	&& !NON_WEST_NEIGHBOR_OF_STRING(WEST(pos), s, color))
      ADD_LIBERTY(s, WEST(pos));
  }
  else if (UNMARKED_COLOR_STRING(WEST(pos), other)) {
    int s2 = STRING_INDEX(WEST(pos));
    PUSH_VALUE(COUNT_NEIGHBORS_BY_INDEX(s));
    ADD_NEIGHBOR(s, WEST(pos));
    PUSH_VALUE(COUNT_NEIGHBORS_BY_INDEX(s2));
    ADD_NEIGHBOR(s2, pos);
    MARK_STRING(WEST(pos));
  }

  if (LIBERTY(NORTH(pos))) {
    if (!liberties_updated
	&& !NON_NORTH_NEIGHBOR_OF_STRING(NORTH(pos), s, color))
      ADD_LIBERTY(s, NORTH(pos));
  }
  else if (UNMARKED_COLOR_STRING(NORTH(pos), other)) {
    int s2 = STRING_INDEX(NORTH(pos));
    PUSH_VALUE(COUNT_NEIGHBORS_BY_INDEX(s));
    ADD_NEIGHBOR(s, NORTH(pos));
    PUSH_VALUE(COUNT_NEIGHBORS_BY_INDEX(s2));
    ADD_NEIGHBOR(s2, pos);
    MARK_STRING(NORTH(pos));
  }

  if (LIBERTY(EAST(pos))) {
    if (!liberties_updated
	&& !NON_EAST_NEIGHBOR_OF_STRING(EAST(pos), s, color))
      ADD_LIBERTY(s, EAST(pos));
  }
  else if (UNMARKED_COLOR_STRING(EAST(pos), other)) {
    int s2 = STRING_INDEX(EAST(pos));
    PUSH_VALUE(COUNT_NEIGHBORS_BY_INDEX(s));
    ADD_NEIGHBOR(s, EAST(pos));
    PUSH_VALUE(COUNT_NEIGHBORS_BY_INDEX(s2));
    ADD_NEIGHBOR(s2, pos);
#if 0
    MARK_STRING(EAST(pos));
#endif
  }

}


/* Incorporate the string at pos with the string s.
 */

static void
assimilate_string(const Goban *goban, int s, int pos)
{
  ACCESS_PRIVATE_DATA;
  ACCESS_BOARD_CONST;

  int k;
  int last;
  int s2 = STRING_INDEX(pos);
  COUNT_STONES_BY_INDEX(s) += COUNT_STONES_BY_INDEX(s2);

  /* Walk through the s2 stones and change string number. Also pick up
   * the last stone in the cycle for later use.
   */
  pos = FIRST_STONE(s2);
  do {
    PUSH_VALUE(STRING_INDEX(pos));
    STRING_INDEX(pos) = s;
    last = pos;
    pos = NEXT_STONE(pos);
  } while (!BACK_TO_FIRST_STONE(s2, pos));

  /* Link the two cycles together. */
  {
    int pos2 = ORIGIN_BY_INDEX(s);
    PUSH_VALUE(NEXT_STONE(last));
    PUSH_VALUE(NEXT_STONE(pos2));
    NEXT_STONE(last) = NEXT_STONE(pos2);
    NEXT_STONE(pos2) = ORIGIN_BY_INDEX(s2);

    /* Do we need to update the origin? */
    if (ORIGIN_BY_INDEX(s2) < pos2)
      ORIGIN_BY_INDEX(s) = ORIGIN_BY_INDEX(s2);
  }

  /* Pick up the liberties of s2 that we don't already have.
   * It is assumed that the liberties of s have been marked before
   * this function is called.
   */
  if (LIBERTIES_BY_INDEX(s2) <= MAX_LIBERTIES) {
    for (k = 0; k < LIBERTIES_BY_INDEX(s2); k++) {
      int pos2 = STRING_LIBERTIES(s2)[k];
      if (UNMARKED_LIBERTY(pos2))
	ADD_AND_MARK_LIBERTY(s, pos2);
    }
  }
  else {
    /* If s2 had too many liberties the above strategy wouldn't be
     * effective, since not all liberties are listed in
     * libs[] the chain of stones for s2 is no
     * longer available (it has already been merged with s) so we
     * can't reconstruct the s2 liberties. Instead we capitulate and
     * rebuild the list of liberties for s (including the neighbor
     * strings assimilated so far) from scratch.
     */

    /* Reset the mark. */
    private->liberty_mark++;

    /* To avoid pushing the current list. */
    LIBERTIES_BY_INDEX(s) = 0;

    update_liberties(goban, s);
  }

  /* Remove s2 as neighbor to the neighbors of s2 and instead add s if
   * they don't already have added it. Also add the neighbors of s2 as
   * neighbors of s, unless they already have been added. The already
   * known neighbors of s are assumed to have been marked before this
   * function is called.
   */
  for (k = 0; k < COUNT_NEIGHBORS_BY_INDEX(s2); k++) {
    int t = STRING_NEIGHBORS(s2)[k];
    remove_neighbor(goban, t, s2);
    if (STRING_DATA_BY_INDEX(t)->mark != private->string_mark) {
      PUSH_VALUE(COUNT_NEIGHBORS_BY_INDEX(t));
      STRING_NEIGHBORS(t)[COUNT_NEIGHBORS_BY_INDEX(t)++] = s;
      STRING_NEIGHBORS(s)[COUNT_NEIGHBORS_BY_INDEX(s)++] = t;
      STRING_DATA_BY_INDEX(t)->mark = private->string_mark;
    }
  }
}


/* Create a new string for the stone at pos and assimilate all
 * friendly neighbor strings.
 */

static void
assimilate_neighbor_strings(const Goban *goban, int pos)
{
  ACCESS_PRIVATE_DATA;
  ACCESS_BOARD_CONST;

  int s;
  String_data *string_data;
  int color = board[pos];
  int other = OTHER_COLOR(color);

  /* Get the next free string number. */
  PUSH_VALUE(private->next_string);
  s = private->next_string++;
  ASSERT1(goban, s < MAX_STRINGS, pos);
  STRING_INDEX(pos) = s;
  string_data = STRING_DATA_BY_INDEX(s);

  /* Set up a size one cycle for the string. */
  NEXT_STONE(pos) = pos;

  /* Set trivially known values and initialize the rest to zero. */
  string_data->color	     = color;
  string_data->size	     = 1;
  string_data->origin	     = pos;
  string_data->num_liberties = 0;
  string_data->num_neighbors = 0;

  /* Clear the marks. */
  private->liberty_mark++;
  private->string_mark++;

  /* Mark ourselves. */
  string_data->mark = private->string_mark;

  /* Look in each direction for
   *
   * 1. liberty: Add if not already visited.
   * 2. opponent string: Add it among our neighbors and us among its
   *    neighbors, unless already visited.
   * 3. friendly string: Assimilate.
   */
  if (UNMARKED_LIBERTY(SOUTH(pos)))
    ADD_AND_MARK_LIBERTY(s, SOUTH(pos));
  else if (UNMARKED_COLOR_STRING(SOUTH(pos), other)) {
    ADD_NEIGHBOR(s, SOUTH(pos));
    PUSH_VALUE(COUNT_NEIGHBORS(SOUTH(pos)));
    ADD_NEIGHBOR(STRING_INDEX(SOUTH(pos)), pos);
    MARK_STRING(SOUTH(pos));
  }
  else if (UNMARKED_COLOR_STRING(SOUTH(pos), color))
    assimilate_string(goban, s, SOUTH(pos));

  if (UNMARKED_LIBERTY(WEST(pos)))
    ADD_AND_MARK_LIBERTY(s, WEST(pos));
  else if (UNMARKED_COLOR_STRING(WEST(pos), other)) {
    ADD_NEIGHBOR(s, WEST(pos));
    PUSH_VALUE(COUNT_NEIGHBORS(WEST(pos)));
    ADD_NEIGHBOR(STRING_INDEX(WEST(pos)), pos);
    MARK_STRING(WEST(pos));
  }
  else if (UNMARKED_COLOR_STRING(WEST(pos), color))
    assimilate_string(goban, s, WEST(pos));

  if (UNMARKED_LIBERTY(NORTH(pos)))
    ADD_AND_MARK_LIBERTY(s, NORTH(pos));
  else if (UNMARKED_COLOR_STRING(NORTH(pos), other)) {
    ADD_NEIGHBOR(s, NORTH(pos));
    PUSH_VALUE(COUNT_NEIGHBORS(NORTH(pos)));
    ADD_NEIGHBOR(STRING_INDEX(NORTH(pos)), pos);
    MARK_STRING(NORTH(pos));
  }
  else if (UNMARKED_COLOR_STRING(NORTH(pos), color))
    assimilate_string(goban, s, NORTH(pos));

  if (UNMARKED_LIBERTY(EAST(pos))) {
#if 0
    ADD_AND_MARK_LIBERTY(s, EAST(pos));
#else
    ADD_LIBERTY(s, EAST(pos));
#endif
  }
  else if (UNMARKED_COLOR_STRING(EAST(pos), other)) {
    ADD_NEIGHBOR(s, EAST(pos));
    PUSH_VALUE(COUNT_NEIGHBORS(EAST(pos)));
    ADD_NEIGHBOR(STRING_INDEX(EAST(pos)), pos);
#if 0
    MARK_STRING(EAST(pos));
#endif
  }
  else if (UNMARKED_COLOR_STRING(EAST(pos), color))
    assimilate_string(goban, s, EAST(pos));
}


/* Suicide at `pos' (the function assumes that the move is indeed suicidal).
 * Remove the neighboring friendly strings.
 */

static void
do_commit_suicide(Goban *goban, int pos, int color)
{
  ACCESS_PRIVATE_DATA_CONST;
  ACCESS_BOARD_CONST;

  if (board[SOUTH(pos)] == color)
    do_remove_string(goban, STRING_INDEX(SOUTH(pos)));

  if (board[WEST(pos)] == color)
    do_remove_string(goban, STRING_INDEX(WEST(pos)));

  if (board[NORTH(pos)] == color)
    do_remove_string(goban, STRING_INDEX(NORTH(pos)));

  if (board[EAST(pos)] == color)
    do_remove_string(goban, STRING_INDEX(EAST(pos)));

  /* Count the stone we "played" as captured. */
  if (color == WHITE)
    goban->white_captured++;
  else
    goban->black_captured++;
}


/* Play a move without legality checking. This is a low-level function,
 * it assumes that the move is not a suicide. Such cases must be handled
 * where the function is called.
 */

static void
do_play_move(Goban *goban, int pos, int color)
{
  ACCESS_PRIVATE_DATA;
  ACCESS_BOARD;

  int other = OTHER_COLOR(color);
  int captured_stones = 0;
  int neighbor_allies = 0;
  int s = -1;

  /* Clear string mark. */
  private->string_mark++;

  /* Put down the stone.  We also set its string number to -1 for a while
   * so that NEIGHBOR_OF_STRING() and friends don't get confused with the
   * stone.
   */
  DO_ADD_STONE(pos, color);
  STRING_INDEX(pos) = -1;

  /* Look in all directions. Count the number of neighbor strings of the same
   * color, remove captured strings and remove `pos' as liberty for opponent
   * strings that are not captured.
   */
  if (board[SOUTH(pos)] == color) {
    neighbor_allies++;
    s = STRING_INDEX(SOUTH(pos));
    MARK_STRING(SOUTH(pos));
  }
  else if (board[SOUTH(pos)] == other) {
    if (LIBERTIES(SOUTH(pos)) > 1) {
      remove_liberty(goban, STRING_INDEX(SOUTH(pos)), pos);
      MARK_STRING(SOUTH(pos));
    }
    else
      captured_stones += do_remove_string(goban, STRING_INDEX(SOUTH(pos)));
  }

  if (UNMARKED_COLOR_STRING(WEST(pos), color)) {
    neighbor_allies++;
    s = STRING_INDEX(WEST(pos));
    MARK_STRING(WEST(pos));
  }
  else if (UNMARKED_COLOR_STRING(WEST(pos), other)) {
    if (LIBERTIES(WEST(pos)) > 1) {
      remove_liberty(goban, STRING_INDEX(WEST(pos)), pos);
      MARK_STRING(WEST(pos));
    }
    else
      captured_stones += do_remove_string(goban, STRING_INDEX(WEST(pos)));
  }

  if (UNMARKED_COLOR_STRING(NORTH(pos), color)) {
    neighbor_allies++;
    s = STRING_INDEX(NORTH(pos));
    MARK_STRING(NORTH(pos));
  }
  else if (UNMARKED_COLOR_STRING(NORTH(pos), other)) {
    if (LIBERTIES(NORTH(pos)) > 1) {
      remove_liberty(goban, STRING_INDEX(NORTH(pos)), pos);
      MARK_STRING(NORTH(pos));
    }
    else
      captured_stones += do_remove_string(goban, STRING_INDEX(NORTH(pos)));
  }

  if (UNMARKED_COLOR_STRING(EAST(pos), color)) {
    neighbor_allies++;
    s = STRING_INDEX(EAST(pos));
#if 0
    MARK_STRING(EAST(pos));
#endif
  }
  else if (UNMARKED_COLOR_STRING(EAST(pos), other)) {
    if (LIBERTIES(EAST(pos)) > 1) {
      remove_liberty(goban, STRING_INDEX(EAST(pos)), pos);
#if 0
      MARK_STRING(EAST(pos));
#endif
    }
    else
      captured_stones += do_remove_string(goban, STRING_INDEX(EAST(pos)));
  }

  /* Choose strategy depending on the number of friendly neighbors. */
  if (neighbor_allies == 0)
    create_new_string(goban, pos);
  else if (neighbor_allies == 1) {
    gg_assert(goban, s >= 0);
    extend_neighbor_string(goban, pos, s);
    return; /* can't be a ko, we're done */
  }
  else {
    assimilate_neighbor_strings(goban, pos);
    return; /* can't be a ko, we're done */
  }

  /* Check whether this move was a ko capture and if so set
   * board_ko_pos.
   *
   * No need to push board_ko_pos on the stack,
   * because this has been done earlier.
   */
  s = STRING_INDEX(pos);
  if (LIBERTIES_BY_INDEX(s) == 1
      && COUNT_STONES_BY_INDEX(s) == 1
      && captured_stones == 1) {
    /* In case of a double ko: clear old ko position first. */
    if (goban->board_ko_pos != NO_MOVE)
      hashdata_invert_ko(&goban->board_hash, goban->board_ko_pos);
    goban->board_ko_pos = STRING_LIBERTIES(s)[0];
    hashdata_invert_ko(&goban->board_hash, goban->board_ko_pos);
  }
}



/* ================================================================ *
 * The following functions don't actually belong here. They are
 * only here because they are faster here where they have access to
 * the incremental data structures.
 * ================================================================ */


/* Help collect the data needed by order_moves() in reading.c.
 * It's the caller's responsibility to initialize the result parameters.
 */

#define NO_UNROLL 0

void
incremental_order_moves(const Goban *goban, int move, int color, int str,
			int *number_edges, int *number_same_string,
			int *number_own, int *number_opponent,
			int *captured_stones, int *threatened_stones,
			int *saved_stones, int *number_open)
{
  ACCESS_PRIVATE_DATA;
  ACCESS_BOARD_CONST;

#if NO_UNROLL == 1

  int pos;
  int k;

  /* Clear the string mark. */
  private->string_mark++;

  for (k = 0; k < 4; k++) {
    pos = move + delta[k];
    if (!ON_BOARD(goban, pos))
      (*number_edges)++;
    else if (board[pos] == EMPTY)
      (*number_open)++;
    else {
      int s = STRING_INDEX(pos);
      if (STRING_INDEX(str) == s)
	(*number_same_string)++;

      if (board[pos] == color) {
	(*number_own)++;
	if (LIBERTIES_BY_INDEX(s) == 1)
	  (*saved_stones) += COUNT_STONES_BY_INDEX(s);
      }
      else {
	(*number_opponent)++;
	if (LIBERTIES_BY_INDEX(s) == 1) {
	  int r;
	  const String_data *t;
	  (*captured_stones) += COUNT_STONES_BY_INDEX(s);
	  for (r = 0; r < COUNT_NEIGHBORS_BY_INDEX(s); r++) {
	    t = STRING_DATA_BY_INDEX(STRING_NEIGHBORS(s([r]);
	    if (t->num_liberties == 1)
	      (*saved_stones) += t->size;
	  }
	}
	else if (LIBERTIES_BY_INDEX(s) == 2 && UNMARKED_STRING(pos)) {
	  (*threatened_stones) += COUNT_STONES_BY_INDEX(s);
	  MARK_STRING(pos);
	}
      }
    }
  }

#else /* NO_UNROLL != 1 */

#define code1(arg)							\
  if (!ON_BOARD(goban, arg))						\
    (*number_edges)++;							\
  else if (board[arg] == EMPTY)						\
    (*number_open)++;							\
  else {								\
    int s = STRING_INDEX(arg);						\
    if (STRING_INDEX(str) == s)						\
      (*number_same_string)++;						\
    if (board[arg] == color) {						\
      (*number_own)++;							\
      if (LIBERTIES_BY_INDEX(s) == 1)					\
	(*saved_stones) += COUNT_STONES_BY_INDEX(s);			\
    }									\
    else {								\
      (*number_opponent)++;						\
      if (LIBERTIES_BY_INDEX(s) == 1) {					\
	int r;								\
	const String_data *t;						\
	(*captured_stones) += COUNT_STONES_BY_INDEX(s);			\
	for (r = 0; r < COUNT_NEIGHBORS_BY_INDEX(s); r++) {		\
	  t = STRING_DATA_BY_INDEX(STRING_NEIGHBORS(s)[r]);		\
	  if (t->num_liberties == 1)					\
	    (*saved_stones) += t->size;					\
	}								\
      }									\
      else if (LIBERTIES_BY_INDEX(s) == 2 && UNMARKED_STRING(arg)) {	\
	(*threatened_stones) += COUNT_STONES_BY_INDEX(s);		\
	MARK_STRING(arg);						\
      }									\
    }									\
  }

  /* Clear the string mark. */
  private->string_mark++;

  code1(SOUTH(move));
  code1(WEST(move));
  code1(NORTH(move));
  code1(EAST(move));

#endif /* NO_UNROLL != 1 */
}


int
square_dist(int pos1, int pos2)
{
  int idist = I(pos1) - I(pos2);
  int jdist = J(pos1) - J(pos2);
  return idist*idist + jdist*jdist;
}


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
