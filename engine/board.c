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


/* The functions in this file implements a go board with incremental
 * update of strings and liberties.
 * 
 * See the Texinfo documentation (Utility Functions: Incremental Board)
 * for an introduction.
 */

#include "gnugo.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>

#include "liberty.h"
#include "hash.h"
#include "sgftree.h"
#include "gg_utils.h"


/* This can be used for internal checks w/in board.c that should
 * typically not be necessary (for speed). */
#if 1
#define PARANOID1(x, pos) ASSERT1(x, pos)
#else
#define PARANOID1(x, pos)
#endif

/* ================================================================ */
/*                          data structures                         */
/* ================================================================ */


/* Incremental string data. */
struct string_data {
  int color;                       /* Color of string, BLACK or WHITE */
  int size;                        /* Number of stones in string. */
  int origin;                      /* Coordinates of "origin", i.e. */
                                   /* "upper left" stone. */
  int liberties;                   /* Number of liberties. */
  int libs[MAX_LIBERTIES];         /* Coordinates of liberties. */
  int neighbors;                   /* Number of neighbor strings */
  int neighborlist[MAXCHAIN];      /* List of neighbor string numbers. */
  int mark;                        /* General purpose mark. */
};


/* we keep the adress and the old value */
struct change_stack_entry {
  int *address;
  int value;
};

/* we keep the adress and the old value */
struct vertex_stack_entry {
  Intersection *address;
  int value;
};


/* Experimental results show that the average number of change stack
 * entries per move usually is in the 20-30 range and very seldom
 * exceeds 40. But since we have no way to recover from running out of
 * stack space, we allocate with a substantial safety margin.
 */
#define STACK_SIZE 80 * MAXSTACK


#define CLEAR_STACKS()\
(change_stack_pointer = change_stack, \
 vertex_stack_pointer = vertex_stack)

/* Begin a record : adress == NULL */
#define BEGIN_CHANGE_RECORD()\
((change_stack_pointer++)->address = NULL,\
 (vertex_stack_pointer++)->address = NULL)

/* Save a value : store the adress and the value in the stack */
#define PUSH_VALUE(v)\
(change_stack_pointer->address = &(v),\
 (change_stack_pointer++)->value = (v))

/* Save a board value : store the adress and the value in the stack */
#define PUSH_VERTEX(v)\
(vertex_stack_pointer->address = &(v),\
 (vertex_stack_pointer++)->value = (v))

#define POP_MOVE()\
  while ((--change_stack_pointer)->address)\
  *(change_stack_pointer->address) =\
  change_stack_pointer->value


#define POP_VERTICES()\
  while ((--vertex_stack_pointer)->address)\
  *(vertex_stack_pointer->address) =\
  vertex_stack_pointer->value


/* ================================================================ */
/*                      static data structures                      */
/* ================================================================ */


/* Main array of string information. */
static struct string_data  string[MAX_STRINGS];

/* Stacks and stack pointers. */
static struct change_stack_entry change_stack[STACK_SIZE];
static struct change_stack_entry *change_stack_pointer;

static struct vertex_stack_entry vertex_stack[STACK_SIZE];
static struct vertex_stack_entry *vertex_stack_pointer;


/* Index into list of strings. The index is only valid if there is a
 * stone at the vertex.
 */
static int string_number[BOARDMAX];


/* The stones in a string are linked together in a cyclic list. 
 * These are the coordinates to the next stone in the string.
 */
static int next_stone[BOARDMAX];


/* ---------------------------------------------------------------- */


/* Macros to traverse the stones of a string.
 *
 * Usage:
 * int s, pos;
 * s = find_the_string()
 * pos = FIRST_STONE(s);
 *   do {
 *    use_stone(pos);
 *    pos = NEXT_STONE(pos);
 *  } while (!BACK_TO_FIRST_STONE(s, pos));
 */
#define FIRST_STONE(s) \
  (string[s].origin)

#define NEXT_STONE(pos) \
  (next_stone[pos])

#define BACK_TO_FIRST_STONE(s, pos) \
  ((pos) == string[s].origin)


/* Assorted useful macros.
 *
 * Some of them could have been functions but are implemented as
 * macros for speed.
 */

#define LIBERTY(pos) \
  (board[pos] == EMPTY)

#define UNMARKED_LIBERTY(pos) \
  (board[pos] == EMPTY && ml[pos] != liberty_mark)

#define MARK_LIBERTY(pos) \
  ml[pos] = liberty_mark

#define UNMARKED_STRING(pos) \
  (string[string_number[pos]].mark != string_mark)

/* Note that these two macros are not complementary. Both return
 * false if board[pos] != color.
 */
#define UNMARKED_COLOR_STRING(pos, color)\
  (board[pos] == color\
   && string[string_number[pos]].mark != string_mark)

#define MARKED_COLOR_STRING(pos, color)\
  (board[pos] == color\
   && string[string_number[pos]].mark == string_mark)

#define MARK_STRING(pos) string[string_number[pos]].mark = string_mark

#define STRING_AT_VERTEX(pos, s, color)\
  ((board[pos] == color) && string_number[pos] == (s))

#define NEIGHBOR_OF_STRING(pos, s, color)\
  (STRING_AT_VERTEX(SOUTH(pos), s, color)\
   || STRING_AT_VERTEX(WEST(pos), s, color)\
   || STRING_AT_VERTEX(NORTH(pos), s, color)\
   || STRING_AT_VERTEX(EAST(pos), s, color))

/* These four macros have rather confusing names. It should be read as:
 * "(pos) is a neighbor of string (s) of (color) in any direction except
 * the specified one".
 */
#define NON_SOUTH_NEIGHBOR_OF_STRING(pos, s, color)\
  (STRING_AT_VERTEX(SOUTH(pos), s, color)\
   || STRING_AT_VERTEX(WEST(pos), s, color)\
   || STRING_AT_VERTEX(EAST(pos), s, color))
  
#define NON_WEST_NEIGHBOR_OF_STRING(pos, s, color)\
  (STRING_AT_VERTEX(WEST(pos), s, color)\
   || STRING_AT_VERTEX(NORTH(pos), s, color)\
   || STRING_AT_VERTEX(SOUTH(pos), s, color))
  
#define NON_NORTH_NEIGHBOR_OF_STRING(pos, s, color)\
  (STRING_AT_VERTEX(NORTH(pos), s, color)\
   || STRING_AT_VERTEX(EAST(pos), s, color)\
   || STRING_AT_VERTEX(WEST(pos), s, color))
  
#define NON_EAST_NEIGHBOR_OF_STRING(pos, s, color)\
  (STRING_AT_VERTEX(EAST(pos), s, color)\
   || STRING_AT_VERTEX(SOUTH(pos), s, color)\
   || STRING_AT_VERTEX(NORTH(pos), s, color))
  
#define LIBERTIES(pos)\
  string[string_number[pos]].liberties

#define COUNTSTONES(pos) \
  string[string_number[pos]].size

#define ADD_LIBERTY(s, pos)\
  do {\
    if (string[s].liberties < MAX_LIBERTIES)\
      string[s].libs[string[s].liberties] = pos;\
    string[s].liberties++;\
  } while (0)

#define ADD_AND_MARK_LIBERTY(s, pos)\
  do {\
    if (string[s].liberties < MAX_LIBERTIES)\
      string[s].libs[string[s].liberties] = pos;\
    string[s].liberties++;\
    ml[pos] = liberty_mark;\
  } while (0)

#define ADD_NEIGHBOR(s, pos)\
  string[s].neighborlist[string[s].neighbors++] = string_number[pos]

#define DO_ADD_STONE(pos, color)\
  do {\
    PUSH_VERTEX(board[pos]);\
    board[pos] = color;\
    hashdata_invert_stone(&hashdata, pos, color);\
  } while (0)

#define DO_REMOVE_STONE(pos)\
  do {\
    PUSH_VERTEX(board[pos]);\
    hashdata_invert_stone(&hashdata, pos, board[pos]);\
    board[pos] = EMPTY;\
  } while (0)


/* ---------------------------------------------------------------- */



/* Number of the next free string. */
static int next_string;


/* For marking purposes. */
static int ml[BOARDMAX];
static int liberty_mark;
static int string_mark;


/* Forward declarations. */
static int do_trymove(int pos, int color, int ignore_ko);
static void undo_trymove(void);
static void new_position(void);
static int propagate_string(int stone, int str);
static void find_liberties_and_neighbors(int s);
static int do_remove_string(int s);
static void do_play_move(int pos, int color);
static int slow_approxlib(int pos, int color, int maxlib, int *libs);


/* Statistics. */
static int trymove_counter = 0;

/* Coordinates for the eight directions, ordered
 * south, west, north, east, southwest, northwest, northeast, southeast.
 */
int deltai[8] = { 1,  0, -1,  0,  1, -1, -1, 1};
int deltaj[8] = { 0, -1,  0,  1, -1, -1,  1, 1};
int delta[8]  = { NS, -1, -NS, 1, NS-1, -NS-1, -NS+1, NS+1};


/* ================================================================ */
/*                    Board initialization                          */
/* ================================================================ */

/*
 * Save board state.
 */

void
store_board(struct board_state *state)
{
  int k;

  gg_assert(stackp == 0);

  state->board_size = board_size;

  memcpy(state->board, board, sizeof(board));
  memcpy(state->initial_board, initial_board, sizeof(initial_board));

  state->board_ko_pos = board_ko_pos;
  state->white_captured = white_captured;
  state->black_captured = black_captured;
  
  state->initial_board_ko_pos = initial_board_ko_pos;
  state->initial_white_captured = initial_white_captured;
  state->initial_black_captured = initial_black_captured;
  
  state->move_history_pointer = move_history_pointer;
  for (k = 0; k < move_history_pointer; k++) {
    state->move_history_color[k] = move_history_color[k];
    state->move_history_pos[k] = move_history_pos[k];
  }

  state->komi = komi;
  state->move_number = movenum;
}


/*
 * Restore a saved board state.
 */

void
restore_board(struct board_state *state)
{
  int k;

  gg_assert(stackp == 0);

  board_size = state->board_size;

  memcpy(board, state->board, sizeof(board));
  memcpy(initial_board, state->initial_board, sizeof(initial_board));

  board_ko_pos = state->board_ko_pos;
  white_captured = state->white_captured;
  black_captured = state->black_captured;
  
  initial_board_ko_pos = state->initial_board_ko_pos;
  initial_white_captured = state->initial_white_captured;
  initial_black_captured = state->initial_black_captured;
  
  move_history_pointer = state->move_history_pointer;
  for (k = 0; k < move_history_pointer; k++) {
    move_history_color[k] = state->move_history_color[k];
    move_history_pos[k] = state->move_history_pos[k];
  }

  komi = state->komi;
  movenum = state->move_number;
  
  hashdata_recalc(&hashdata, board, board_ko_pos);
  new_position();
}


/*
 * Clear the internal board.
 */

void
clear_board(void)
{
  int k;

  gg_assert(board_size > 0 && board_size <= MAX_BOARD);
  
  memset(board, EMPTY, sizeof(board));
  memset(initial_board, EMPTY, sizeof(initial_board));
  for (k = 0; k < BOARDSIZE; k++) {
    if (!ON_BOARD2(I(k), J(k))) {
      board[k] = GRAY;
      initial_board[k] = GRAY;
    }
  }

  board_ko_pos = NO_MOVE;
  white_captured = 0;
  black_captured = 0;

  initial_board_ko_pos = NO_MOVE;
  initial_white_captured = 0;
  initial_black_captured = 0;

  move_history_pointer = 0;
  movenum = 0;
  
  hashdata_recalc(&hashdata, board, board_ko_pos);
  new_position();
}

/* Test the integrity of the gray border. */
int
test_gray_border(void)
{
  int k;

  gg_assert(board_size > 0 && board_size <= MAX_BOARD);
  
  for (k = 0; k < BOARDSIZE; k++)
    if (!ON_BOARD2(I(k), J(k)))
      if (board[k] != GRAY)
      	return k;
  
  return -1;
}


/* ================================================================ */
/*                      Temporary moves                             */
/* ================================================================ */


/* Stack of trial moves to get to current
 * position and which color made them. Perhaps 
 * this should be one array of a structure 
 */
static int stack[MAXSTACK];
static int move_color[MAXSTACK];

static Hash_data hashdata_stack[MAXSTACK];

static const char *
komaster_to_string(int komaster)
{
  const char *b[7] = {"empty", "white", "black", "gray", "gray_white",
		      "gray_black", "weak_ko"};
  gg_assert(komaster >= 0 && komaster <= 6);
  return b[komaster];
}

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
trymove(int pos, int color, const char *message, int str,
	int komaster, int kom_pos)
{
  /* Do the real work elsewhere. */
  if (!do_trymove(pos, color, 0))
    return 0;

  /* Store the move in an sgf tree if one is available. */
  if (sgf_dumptree) {
    char buf[100];

    if (message == NULL)
      message = "UNKNOWN";

    if (str == NO_MOVE) {
      if (komaster != EMPTY)
	gg_snprintf(buf, 100, "%s (variation %d, hash %lx, komaster %s:%s)", 
		    message, count_variations, hashdata.hashval[0],
		    komaster_to_string(komaster),
		    location_to_string(kom_pos));
      else
	gg_snprintf(buf, 100, "%s (variation %d, hash %lx)", 
		    message, count_variations, hashdata.hashval[0]);
    }
    else {
      if (komaster != EMPTY)
	gg_snprintf(buf, 100, 
		    "%s at %s (variation %d, hash %lx, komaster %s:%s)", 
		    message, location_to_string(str), count_variations,
		    hashdata.hashval[0], 
                    komaster_to_string(komaster),
		    location_to_string(kom_pos));
      else
	gg_snprintf(buf, 100, "%s at %s (variation %d, hash %lx)", 
		    message, location_to_string(str), count_variations,
		    hashdata.hashval[0]);
    }
    sgftreeAddPlayLast(sgf_dumptree, color, I(pos), J(pos));
    sgftreeAddComment(sgf_dumptree, buf);
  }
  
  if (count_variations)
    count_variations++;
  stats.nodes++;

  return 1;
}


/*
 * Special version for semeai reading.
 */

int 
semeai_trymove(int pos, int color, const char *message, int str1, int str2,
	       int owl_phase, int value)
{
  /* Do the real work elsewhere. */
  if (!do_trymove(pos, color, 0))
    return 0;

  if (message == NULL)
    message = "UNKNOWN";
  
  /* Store the move in an sgf tree if one is available. */
  if (sgf_dumptree) {
    char buf[100];
    char sbuf1[5], sbuf2[5];
    /* Simply calling location_to_string three times doesn't work
     * because the internal buffer of that function gets overwritten
     * too soon. So we allocate our own char buffers.
     */
    location_to_buffer(str1, sbuf1);
    location_to_buffer(str2, sbuf2);

    if (owl_phase)
      gg_snprintf(buf, 100, 
		  "%s in semeai %s-%s (variation %d, value %d, owl_phase)",
		  message, sbuf1, sbuf2, count_variations, value);
    else
      gg_snprintf(buf, 100, 
		  message, sbuf1, sbuf2, count_variations, value);
    sgftreeAddPlayLast(sgf_dumptree, color, I(pos), J(pos));
    sgftreeAddComment(sgf_dumptree, buf);
  }
  
  if (count_variations)
    count_variations++;
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
tryko(int pos, int color, const char *message, int komaster, int kom_pos)
{
  /* Do the real work elsewhere. */
  if (!do_trymove(pos, color, 1))
    return 0;

  if (sgf_dumptree) {
    char buf[100];
    if (message == NULL)
      message = "UNKNOWN";
    if (komaster != EMPTY)
      gg_snprintf(buf, 100, "tryko: %s (variation %d, %lx, komaster %s:%s)", 
		  message, count_variations, hashdata.hashval[0],
		  komaster_to_string(komaster), 
                  location_to_string(kom_pos));
    else
      gg_snprintf(buf, 100, "tryko: %s (variation %d, %lx)", 
		  message, count_variations, hashdata.hashval[0]);

    /* Add two pass moves to the SGF output to simulate the ko threat
     * and the answer.
     *
     * The reason we add these is that certain SGF viewers, including
     * Cgoban 1, won't properly display variations with illegal ko
     * captures. SGF FF[4] compliant browsers should have no problem
     * with this, though.
     */
    sgftreeAddPlayLast(sgf_dumptree, color, -1, -1);
    sgftreeAddComment(sgf_dumptree, "tenuki (ko threat)");
    sgftreeAddPlayLast(sgf_dumptree, OTHER_COLOR(color), -1, -1);
    sgftreeAddComment(sgf_dumptree, "tenuki (answers ko threat)");

    sgftreeAddPlayLast(sgf_dumptree, color, I(pos), J(pos));
    sgftreeAddComment(sgf_dumptree, buf);
  }
  
  if (count_variations)
    count_variations++;
  stats.nodes++;

  return 1;
}


/*
 * Do the main work of trymove() and tryko(), i.e. the common parts.
 * The ignore_ko flag tells whether an illegal ko capture may be done.
 * Return 1 if the move was valid, otherwise 0.
 */

static int 
do_trymove(int pos, int color, int ignore_ko)
{
  /* 1. The move must be inside the board and the color must be BLACK
   * or WHITE.
   */
  ASSERT_ON_BOARD1(pos);
  gg_assert(color == BLACK || color == WHITE);
 
  /* Update the reading tree shadow. */
  shadow[pos] = 1;

  /* 2. The location must be empty. */
  if (board[pos] != EMPTY)
    return 0;

  /* 3. The location must not be the ko point, unless ignore_ko == 1. */
  if (!ignore_ko && pos == board_ko_pos) {
    if (board[WEST(pos)] == OTHER_COLOR(color)
	|| board[EAST(pos)] == OTHER_COLOR(color)) {
      return 0;
    }
  }

  /* 4. Test for suicide. */
  if (is_suicide(pos, color)) {
    return 0;
  }
  
  /* Check for stack overflow. */
  if (stackp >= MAXSTACK-2) {
    fprintf(stderr, 
	    "gnugo: Truncating search. This is beyond my reading ability!\n");
    /* FIXME: Perhaps it's best to just assert here and be done with it? */
    if (0) {
      ASSERT1(0 && "trymove stack overflow", pos);
    }
    if (verbose > 0) {
      showboard(0);
      dump_stack();
    }
    fflush(stdout);
    fflush(stderr);
    return 0;
  }


  /* Only count trymove when we do create a new position. */
  trymove_counter++;
  
  /* So far, so good. Now push the move on the move stack. These are
   * needed for dump_stack().
   */
  stack[stackp] = pos;
  move_color[stackp] = color;

  /*
   * FIXME: Do we really have to store hashdata in a stack?
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
  PUSH_VALUE(board_ko_pos);
  memcpy(&hashdata_stack[stackp], &hashdata, sizeof(hashdata));

  if (board_ko_pos != NO_MOVE)
    hashdata_invert_ko(&hashdata, board_ko_pos);
  board_ko_pos = NO_MOVE;
  
  PUSH_VALUE(black_captured);
  PUSH_VALUE(white_captured);

  ++stackp;

  if (verbose == 4)
    dump_stack();

  do_play_move(pos, color);

  return 1;
}


/*
 * popgo pops the position from the stack.
 */

void
popgo()
{
  stackp--;
  
  undo_trymove();
  
  memcpy(&hashdata, &(hashdata_stack[stackp]), sizeof(hashdata));
  if (sgf_dumptree) {
    char buf[100];
    gg_snprintf(buf, 100, "(next variation: %d)", count_variations);
    sgftreeAddComment(sgf_dumptree, buf);
    sgf_dumptree->lastnode = sgf_dumptree->lastnode->parent;
    /* After tryko() we need to undo two pass nodes too. Since we have
     * no other way to identify ko moves, we skip all pass nodes.
     */
    while (is_pass_node(sgf_dumptree->lastnode, board_size))
      sgf_dumptree->lastnode = sgf_dumptree->lastnode->parent;
  }
}



/* Restore board state to the position before the last move. This is
 * accomplished by popping everything that was stored on the stacks
 * since the last BEGIN_CHANGE_RECORD().
 */

static void
undo_trymove()
{
  gg_assert(change_stack_pointer - change_stack <= STACK_SIZE);

  if (0) {
    gprintf("Change stack size = %d\n", change_stack_pointer - change_stack);
    gprintf("Vertex stack size = %d\n", vertex_stack_pointer - vertex_stack);
  }

  POP_MOVE();
  POP_VERTICES();
}




/*
 * dump_stack() for use under gdb prints the move stack. 
 */

void
dump_stack(void)
{
  int n;

  for (n = 0; n < stackp; n++)
    gprintf("%o%s:%1m ", move_color[n] == BLACK ? "B" : "W", stack[n]);
  
  if (count_variations)
    gprintf("%o (variation %d)", count_variations-1);

  gprintf("%o\n");
  fflush(stdout);
  fflush(stderr);
}


/* ================================================================ */
/*                     Permanent moves                              */
/* ================================================================ */


static void
reset_move_history(void)
{
  memcpy(initial_board, board, sizeof(board));
  initial_board_ko_pos = board_ko_pos;
  initial_white_captured = white_captured;
  initial_black_captured = black_captured;
  move_history_pointer = 0;
}

/* Place a stone on the board and update the hashdata. This operation
 * destroys all move history.
 */

void
add_stone(int pos, int color)
{
  ASSERT1(stackp == 0, pos);
  ASSERT_ON_BOARD1(pos);
  ASSERT1(board[pos] == EMPTY, pos);

  board[pos] = color;
  hashdata_invert_stone(&hashdata, pos, color);
  reset_move_history();
  new_position();
}


/* Remove a stone from the board and update the hashdata. This
 * operation destroys the move history.
 */

void
remove_stone(int pos)
{
  ASSERT1(stackp == 0, pos);
  ASSERT_ON_BOARD1(pos);
  ASSERT1(IS_STONE(board[pos]), pos);

  hashdata_invert_stone(&hashdata, pos, board[pos]);
  board[pos] = EMPTY;
  reset_move_history();
  new_position();
}

static void
play_move_no_history(int pos, int color)
{
#if CHECK_HASHING
  Hash_data oldkey;

  /* Check the hash table to see if it corresponds to the cumulative one. */
  hashdata_recalc(&oldkey, board, board_ko_pos);
#if FULL_POSITION_IN_HASH
  gg_assert(hashdata_diff_dump(&oldkey, &hashdata) == 0);
#else
  gg_assert(hashdata_compare(&oldkey, &hashdata) == 0);
#endif
#endif

  if (board_ko_pos != NO_MOVE)
    hashdata_invert_ko(&hashdata, board_ko_pos);
  board_ko_pos = NO_MOVE;

  /* If the move is a pass, we can skip some steps. */
  if (pos != PASS_MOVE) {
    ASSERT_ON_BOARD1(pos);
    ASSERT1(board[pos] == EMPTY, pos);

    /* Do play the move. */
    do_play_move(pos, color);

#if CHECK_HASHING
    /* Check the hash table to see if it equals the previous one. */
    hashdata_recalc(&oldkey, board, board_ko_pos);
#if FULL_POSITION_IN_HASH
    gg_assert(hashdata_diff_dump(&oldkey, &hashdata) == 0);
#else
    gg_assert(hashdata_compare(&oldkey, &hashdata) == 0);
#endif
#endif
  }
  new_position();
}

/* Load the initial position and replay the first n moves. */
static void
replay_move_history(int n)
{
  int k;
  
  memcpy(board, initial_board, sizeof(board));
  board_ko_pos = initial_board_ko_pos;
  white_captured = initial_white_captured;
  black_captured = initial_black_captured;
  new_position();

  for (k = 0; k < n; k++)
    play_move_no_history(move_history_pos[k], move_history_color[k]);
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
play_move(int pos, int color)
{
  ASSERT1(stackp == 0, pos);
  ASSERT1(color == WHITE || color == BLACK, pos);
  ASSERT1(pos == PASS_MOVE || ON_BOARD1(pos), pos);
  ASSERT1(pos == PASS_MOVE || board[pos] == EMPTY, pos);

  if (move_history_pointer >= MAX_MOVE_HISTORY) {
    /* The move history is full. We resolve this by collapsing the
     * first about 10% of the moves into the initial position.
     */
    int number_collapsed_moves = 1 + MAX_MOVE_HISTORY / 10;
    int k;
    Intersection saved_board[BOARDSIZE];
    int saved_board_ko_pos = board_ko_pos;
    int saved_white_captured = white_captured;
    int saved_black_captured = black_captured;
    memcpy(saved_board, board, sizeof(board));

    replay_move_history(number_collapsed_moves);

    memcpy(initial_board, board, sizeof(board));
    initial_board_ko_pos = board_ko_pos;
    initial_white_captured = white_captured;
    initial_black_captured = black_captured;

    for (k = number_collapsed_moves; k < move_history_pointer; k++) {
      move_history_color[k - number_collapsed_moves] = move_history_color[k];
      move_history_pos[k - number_collapsed_moves] = move_history_pos[k];
    }
    move_history_pointer -= number_collapsed_moves;

    memcpy(board, saved_board, sizeof(board));
    board_ko_pos = saved_board_ko_pos;
    white_captured = saved_white_captured;
    black_captured = saved_black_captured;
  }

  move_history_color[move_history_pointer] = color;
  move_history_pos[move_history_pointer] = pos;
  move_history_pointer++;
  
  play_move_no_history(pos, color);
  
  movenum++;
}


/* Undo n permanent moves. Returns 1 if successful and 0 if it fails.
 * If n moves cannot be undone, no move is undone.
 */
int
undo_move(int n)
{
  gg_assert(stackp == 0);
  
  /* Fail if and only if the move history is too short. */
  if (move_history_pointer < n)
    return 0;

  replay_move_history(move_history_pointer - n);
  move_history_pointer -= n;
  movenum -= n;

  return 1;
}


/* Return the last move done by the opponent to color. Both if no move
 * was found or if the last move was a pass, PASS_MOVE is returned.
 */
int
get_last_opponent_move(int color)
{
  int k;
  
  for (k = move_history_pointer - 1; k >= 0; k--)
    if (move_history_color[k] == OTHER_COLOR(color))
      return move_history_pos[k];

  return PASS_MOVE;
}

/* Return the last move done by anyone. Both if no move was found or
 * if the last move was a pass, PASS_MOVE is returned.
 */
int
get_last_move()
{
  if (move_history_pointer == 0)
    return PASS_MOVE;

  return move_history_pos[move_history_pointer - 1];
}

/* Return the color of the player doing the last move. If no move was
 * found, EMPTY is returned.
 */
int
get_last_player()
{
  if (move_history_pointer == 0)
    return EMPTY;

  return move_history_color[move_history_pointer - 1];
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
is_legal(int pos, int color)
{
  /* 0. A pass move is always legal. */
  if (pos == 0)
    return 1;

  /* 1. The move must be inside the board. */
  ASSERT_ON_BOARD1(pos);

  /* 2. The location must be empty. */
  if (board[pos] != EMPTY) 
    return 0;

  /* 3. The location must not be the ko point. */
  if (pos == board_ko_pos)
    if (board[WEST(pos)] == OTHER_COLOR(color)
	|| board[EAST(pos)] == OTHER_COLOR(color)) {
      return 0;
    }

  /* Check for stack overflow. */
  if (stackp >= MAXSTACK-2) {
    fprintf(stderr, 
	    "gnugo: Truncating search. This is beyond my reading ability!\n");
    /* FIXME: Perhaps it's best to just assert here and be done with it? */
    if (0) {
      ASSERT1(0 && "is_legal stack overflow", pos);
    }
    return 0;
  }

  /* Check for suicide. */
  if (!allow_suicide && is_suicide(pos, color)) {
    return 0;
  }
  
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
is_suicide(int pos, int color)
{
  ASSERT_ON_BOARD1(pos);
  ASSERT1(board[pos] == EMPTY, pos);

  /* Check for suicide. */
  if (LIBERTY(SOUTH(pos))
      || (ON_BOARD(SOUTH(pos))
	  && ((board[SOUTH(pos)] == color) ^ (LIBERTIES(SOUTH(pos)) == 1))))
    return 0;

  if (LIBERTY(WEST(pos))
      || (ON_BOARD(WEST(pos))
	  && ((board[WEST(pos)] == color) ^ (LIBERTIES(WEST(pos)) == 1))))
    return 0;

  if (LIBERTY(NORTH(pos))
      || (ON_BOARD(NORTH(pos))
	  && ((board[NORTH(pos)] == color) ^ (LIBERTIES(NORTH(pos)) == 1))))
    return 0;

  if (LIBERTY(EAST(pos))
      || (ON_BOARD(EAST(pos))
	  && ((board[EAST(pos)] == color) ^ (LIBERTIES(EAST(pos)) == 1))))
    return 0;

  return 1;
}


/*
 * is_illegal_ko_capture(pos, color) determines whether the move
 * (color) at (pos) would be an illegal ko capture.
 */
int 
is_illegal_ko_capture(int pos, int color)
{
  ASSERT_ON_BOARD1(pos);
  ASSERT1(board[pos] == EMPTY, pos);

  return (pos == board_ko_pos
	  && ((board[WEST(pos)] == OTHER_COLOR(color))
	      || (board[EAST(pos)] == OTHER_COLOR(color))));
}


/* Variation of trymove()/tryko() where ko captures (both conditional
 * and unconditional) must follow a komaster scheme.
 *
 * FIXME: This function could be optimized by integrating the
 * trymove()/tryko() code.
 */

#define GRAY_WHITE 4
#define GRAY_BLACK 5
#define WEAK_KO    6

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
komaster_trymove(int pos, int color, const char *message, int str,
		 int komaster, int kom_pos,
		 int *new_komaster, int *new_kom_pos,
		 int *is_conditional_ko, int consider_conditional_ko)
{
  int other = OTHER_COLOR(color);
  int ko_move;
  int kpos;
  int previous_board_ko_pos = board_ko_pos;

  /* First we check whether the ko claimed by komaster has been
   * resolved. If that is the case, we revert komaster to EMPTY.
   *
   * The ko has been resolved in favor of the komaster if it has
   * been filled, or if it is no longer a ko and an opponent move
   * there is suicide.
   */
  if (((komaster == WHITE || komaster == GRAY_WHITE)
       && (IS_STONE(board[kom_pos])
	   || (!is_ko(kom_pos, BLACK, NULL)
	       && is_suicide(kom_pos, BLACK))))
      || ((komaster == BLACK || komaster == GRAY_BLACK)
	  && (IS_STONE(board[kom_pos])
	      || (!is_ko(kom_pos, WHITE, NULL)
		  && is_suicide(kom_pos, WHITE))))) {
    komaster = EMPTY;
    kom_pos = NO_MOVE;
  }

  /* Usually the komaster parameters are unchanged. */
  *new_komaster = komaster;
  *new_kom_pos = kom_pos;

  *is_conditional_ko = 0;
  ko_move = is_ko(pos, color, &kpos);

  if (!ko_move) {
    if (komaster == WEAK_KO) {
      *new_komaster = EMPTY;
      *new_kom_pos = NO_MOVE;
    }
  }
  else {
    /* If opponent is komaster we may not capture his ko. */
    if (komaster == other && pos == kom_pos)
      return 0;

    /* If komaster is gray we may not capture ko at all. */
    if (komaster == GRAY_WHITE || komaster == GRAY_BLACK)
      return 0;

    /* If we are komaster, we may only do nested captures. */
    if (komaster == color
	&& !(kpos == SW(kom_pos) || kpos == NW(kom_pos)
	     || kpos == NE(kom_pos) || kpos == SE(kom_pos)))
      return 0;

    /* If komaster is WEAK_KO, we may only do nested ko capture or
     * conditional ko capture.
     */
    if (komaster == WEAK_KO) {
      if (pos != board_ko_pos
	  && !(kpos == SW(kom_pos) || kpos == NW(kom_pos)
	       || kpos == NE(kom_pos) || kpos == SE(kom_pos)))
	return 0;
    }
  }

  if (!trymove(pos, color, message, str, komaster, kom_pos)) {
    if (!consider_conditional_ko)
      return 0;

    if (!tryko(pos, color, message, komaster, kom_pos))
      return 0; /* Suicide. */
      
    *is_conditional_ko = 1;

    /* Conditional ko capture, set komaster parameters. */
    if (komaster == EMPTY || komaster == WEAK_KO) {
      *new_komaster = color;
      *new_kom_pos = kpos;
      return 1;
    }
  }

  if (!ko_move)
    return 1;

  if (komaster == other) {
    if (color == WHITE)
      *new_komaster = GRAY_BLACK;
    else
      *new_komaster = GRAY_WHITE;
  }
  else if (komaster == color) {
    /* This is where we update kom_pos after a nested capture. */
    *new_kom_pos = kpos;
  }
  else {
    /* We can reach here when komaster is EMPTY or WEAK_KO. If previous
     * move was also a ko capture, we now set komaster to WEAK_KO.
     */
    if (previous_board_ko_pos != NO_MOVE) {
      *new_komaster = WEAK_KO;
      *new_kom_pos = previous_board_ko_pos;
    }
  }
  
  return 1;
}



/* Determine whether vertex is on the edge. */
int
is_edge_vertex(int pos)
{
  ASSERT_ON_BOARD1(pos);
  if (!ON_BOARD(SW(pos))
      || !ON_BOARD(NE(pos)))
    return 1;

  return 0;
}



/* Count the number of liberties of the string at pos. pos must not be
 * empty.
 */
int
countlib(int str)
{
  ASSERT1(IS_STONE(board[str]), str);
  
  /* We already know the number of liberties. Just look it up. */
  return string[string_number[str]].liberties;
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
findlib(int str, int maxlib, int *libs)
{
  int k;
  int liberties;
  int s;
  
  ASSERT1(IS_STONE(board[str]), str);
  ASSERT1(libs != NULL, str);
  
  /* We already have the list of liberties and only need to copy it to
   * libs[].
   *
   * However, if the string has more than MAX_LIBERTIES liberties the
   * list is truncated and if maxlib is also larger than MAX_LIBERTIES
   * we have to traverse the stones in the string in order to find
   * where the liberties are.
   */
  s = string_number[str];
  liberties = string[s].liberties;

  if (liberties <= MAX_LIBERTIES || maxlib <= MAX_LIBERTIES) {
    /* The easy case, it suffices to copy liberty locations from the
     * incrementally updated list.
     */
    for (k = 0; k < maxlib && k < liberties; k++)
      libs[k] = string[s].libs[k];
  }
  else {
    /* The harder case, where we have to traverse the stones in the
     * string. We don't have to check explicitly if we are back to
     * the start of the chain since we will run out of liberties
     * before that happens.
     */
    int pos;
    liberty_mark++;
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
 * at (pos).  Captures are ignored based on the ignore_capture flag.
 * (pos) must be empty.  It will fail if there is more than two
 * string neighbor of the same color.  In this case, the return value
 * is -1.  Captures are handled in a very limited way, so if 
 * ignore_capture is 0, and a capture is required, it will often
 * return -1.
 *
 * The intent of this function is to be as fast as possible, not
 * necessarily complete. But if it returns a positive value (meaning
 * it has succeeded), the value is guaranteed to be correct.
 *
 * Note well, that it relies on incremental data.
 */

int
fastlib(int pos, int color, int ignore_captures)
{
  int ally1 = -1;
  int ally2 = -1;
  int fast_liberties = 0;

  ASSERT1(board[pos] == EMPTY, pos);
  ASSERT1(IS_STONE(color), pos);

  /* Find neighboring strings of the same color. If there are more than two of
   * them, we give up (it's too difficult to count their common liberties).
   */
  if (board[SOUTH(pos)] == color) {
    ally1 = string_number[SOUTH(pos)];

    if (board[WEST(pos)] == color
	&& string_number[WEST(pos)] != ally1) {
      ally2 = string_number[WEST(pos)];

      if (board[NORTH(pos)] == color
	  && string_number[NORTH(pos)] != ally1
	  && string_number[NORTH(pos)] != ally2)
	return -1;
    }
    else if (board[NORTH(pos)] == color
	     && string_number[NORTH(pos)] != ally1)
      ally2 = string_number[NORTH(pos)];

    if (board[EAST(pos)] == color
	&& string_number[EAST(pos)] != ally1) {
      if (ally2 < 0)
	ally2 = string_number[EAST(pos)];
      else if (string_number[EAST(pos)] != ally2)
	return -1;
    }
  }
  else if (board[WEST(pos)] == color) {
    ally1 = string_number[WEST(pos)];

    if (board[NORTH(pos)] == color
	&& string_number[NORTH(pos)] != ally1) {
      ally2 = string_number[NORTH(pos)];

      if (board[EAST(pos)] == color
	  && string_number[EAST(pos)] != ally1
	  && string_number[EAST(pos)] != ally2)
	return -1;
    }
    else if (board[EAST(pos)] == color
	&& string_number[EAST(pos)] != ally1)
      ally2 = string_number[EAST(pos)];
  }
  else if (board[NORTH(pos)] == color) {
    ally1 = string_number[NORTH(pos)];
    
    if (board[EAST(pos)] == color
	&& string_number[EAST(pos)] != ally1)
      ally2 = string_number[EAST(pos)];
  }
  else if (board[EAST(pos)] == color)
    ally1 = string_number[EAST(pos)];

  /* If we are to ignore captures, the things are very easy. */
  if (ignore_captures) {
    if (ally1 < 0) {			/* No allies */
      if (LIBERTY(SOUTH(pos)))
	fast_liberties++;
      if (LIBERTY(WEST(pos)))
	fast_liberties++;
      if (LIBERTY(NORTH(pos)))
	fast_liberties++;
      if (LIBERTY(EAST(pos)))
	fast_liberties++;
    }
    else if(ally2 < 0) {		/* One ally */
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

      fast_liberties += string[ally1].liberties - 1;
    }
    else {				/* Two allies */
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

      fast_liberties += string[ally1].liberties + string[ally2].liberties
	- count_common_libs(string[ally1].origin, string[ally2].origin) - 1;
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
      else if (board[neighbor] == OTHER_COLOR(color)	/* A capture */
	       && LIBERTIES(neighbor) == 1) {
	int neighbor_size = COUNTSTONES(neighbor);

	if (neighbor_size == 1 || (neighbor_size == 2 && ally1 < 0))
	  fast_liberties++;
	else
	  return -1;
      }
    }

    if (ally1 >= 0) {
      fast_liberties += string[ally1].liberties - 1;
      if (ally2 >= 0)
	fast_liberties += string[ally2].liberties
	  - count_common_libs(string[ally1].origin, string[ally2].origin);
    }
  }

  return fast_liberties;
}


/* Find the liberties a stone of the given color would get if played
 * at (pos), ignoring possible captures of opponent stones. (pos)
 * must be empty. If libs != NULL, the locations of up to maxlib
 * liberties are written into libs[]. The counting of
 * liberties may or may not be halted when maxlib is reached. The
 * number of liberties found is returned.
 *
 * If you want the number or the locations of all liberties, however
 * many they are, you should pass MAXLIBS as the value for maxlib and
 * allocate space for libs[] accordingly.
 */

int
approxlib(int pos, int color, int maxlib, int *libs)
{
  int k;
  int liberties = 0;

  ASSERT1(board[pos] == EMPTY, pos);
  ASSERT1(IS_STONE(color), pos);

  if (!libs) {
    int fast_liberties = fastlib(pos, color, 1);
    if (fast_liberties >= 0)
      return fast_liberties;
  }

  /* Look for empty neighbors and the liberties of the adjacent
   * strings of the given color. The algorithm below won't work
   * correctly if any of the adjacent strings have more than
   * MAX_LIBERTIES liberties AND maxlib is larger than MAX_LIBERTIES.
   * If this might be the case, we use a more robust fallback.
   */
  if (maxlib > MAX_LIBERTIES)
    return slow_approxlib(pos, color, maxlib, libs);
  
  /* Start by marking pos itself so it isn't counted among its own
   * liberties.
   */
  liberty_mark++;
  MARK_LIBERTY(pos);
    
  if (UNMARKED_LIBERTY(SOUTH(pos))) {
    if (libs != NULL)
      libs[liberties] = SOUTH(pos);
    liberties++;
    /* Stop counting if we reach maxlib. */
    if (liberties >= maxlib)
      return liberties;
    MARK_LIBERTY(SOUTH(pos));
  }
  else if (board[SOUTH(pos)] == color) {
    int s = string_number[SOUTH(pos)];
    for (k = 0; k < string[s].liberties; k++) {
      int lib = string[s].libs[k];
      if (UNMARKED_LIBERTY(lib)) {
	if (libs != NULL)
	  libs[liberties] = lib;
	liberties++;
	if (liberties >= maxlib)
	  return liberties;
	MARK_LIBERTY(lib);
      }
    }
  }
  
  if (UNMARKED_LIBERTY(WEST(pos))) {
    if (libs != NULL)
      libs[liberties] = WEST(pos);
    liberties++;
    /* Stop counting if we reach maxlib. */
    if (liberties >= maxlib)
      return liberties;
    MARK_LIBERTY(WEST(pos));
  }
  else if (board[WEST(pos)] == color) {
    int s = string_number[WEST(pos)];
    for (k = 0; k < string[s].liberties; k++) {
      int lib = string[s].libs[k];
      if (UNMARKED_LIBERTY(lib)) {
	if (libs != NULL)
	  libs[liberties] = lib;
	liberties++;
	if (liberties >= maxlib)
	  return liberties;
	MARK_LIBERTY(lib);
      }
    }
  }
  
  if (UNMARKED_LIBERTY(NORTH(pos))) {
    if (libs != NULL)
      libs[liberties] = NORTH(pos);
    liberties++;
    /* Stop counting if we reach maxlib. */
    if (liberties >= maxlib)
      return liberties;
    MARK_LIBERTY(NORTH(pos));
  }
  else if (board[NORTH(pos)] == color) {
    int s = string_number[NORTH(pos)];
    for (k = 0; k < string[s].liberties; k++) {
      int lib = string[s].libs[k];
      if (UNMARKED_LIBERTY(lib)) {
	if (libs != NULL)
	  libs[liberties] = lib;
	liberties++;
	if (liberties >= maxlib)
	  return liberties;
	MARK_LIBERTY(lib);
      }
    }
  }
  
  if (UNMARKED_LIBERTY(EAST(pos))) {
    if (libs != NULL)
      libs[liberties] = EAST(pos);
    liberties++;
    /* Stop counting if we reach maxlib. */
    if (liberties >= maxlib)
      return liberties;
  }
  else if (board[EAST(pos)] == color) {
    int s = string_number[EAST(pos)];
    for (k = 0; k < string[s].liberties; k++) {
      int lib = string[s].libs[k];
      if (UNMARKED_LIBERTY(lib)) {
	if (libs != NULL)
	  libs[liberties] = lib;
	liberties++;
	if (liberties >= maxlib)
	  return liberties;
	MARK_LIBERTY(lib);
      }
    }
  }  

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
accuratelib(int pos, int color, int maxlib, int *libs)
{
  int k, l;
  int liberties = 0;
  int lib;
  int captured[4];
  int captures = 0;

  ASSERT1(board[pos] == EMPTY, pos);
  ASSERT1(IS_STONE(color), pos);

  if (!libs) {
    int fast_liberties = fastlib(pos, color, 0);
    if (fast_liberties >= 0)
      return fast_liberties;
  }

  string_mark++;
  liberty_mark++;
  MARK_LIBERTY(pos);

  for (k = 0; k < 4; k++) {
    int pos2 = pos + delta[k];
    if (UNMARKED_LIBERTY(pos2)) {
      /* A trivial liberty */
      if (libs)
	libs[liberties] = pos2;
      liberties++;
      if (liberties >= maxlib)
	return liberties;

      MARK_LIBERTY(pos2);
    }
    else if (UNMARKED_COLOR_STRING(pos2, color)) {
      /* An own neighbor string */
      struct string_data *s = &string[string_number[pos2]];

      if (s->liberties <= MAX_LIBERTIES || maxlib <= MAX_LIBERTIES - 1) {
	/* The easy case - we already have all (necessary) liberties of
	 * the string listed
	 */
	for (l = 0; l < s->liberties; l++) {
	  lib = s->libs[l];
	  if (UNMARKED_LIBERTY(lib)) {
	    if (libs)
	      libs[liberties] = lib;
	    liberties++;
	    if (liberties >= maxlib)
	      return liberties;

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
	    if (libs)
	      libs[liberties] = SOUTH(stone);
	    liberties++;
	    if (liberties >= maxlib)
	      return liberties;

	    MARK_LIBERTY(SOUTH(stone));
	  }

	  if (UNMARKED_LIBERTY(WEST(stone))) {
	    if (libs)
	      libs[liberties] = WEST(stone);
	    liberties++;
	    if (liberties >= maxlib)
	      return liberties;

	    MARK_LIBERTY(WEST(stone));
	  }

	  if (UNMARKED_LIBERTY(NORTH(stone))) {
	    if (libs)
	      libs[liberties] = NORTH(stone);
	    liberties++;
	    if (liberties >= maxlib)
	      return liberties;

	    MARK_LIBERTY(NORTH(stone));
	  }

	  if (UNMARKED_LIBERTY(EAST(stone))) {
	    if (libs)
	      libs[liberties] = EAST(stone);
	    liberties++;
	    if (liberties >= maxlib)
	      return liberties;

	    MARK_LIBERTY(EAST(stone));
	  }

	  stone = NEXT_STONE(stone);
	} while (stone != pos2);
      }

      MARK_STRING(pos2);
    }
    else if (board[pos2] == OTHER_COLOR(color)
	     && string[string_number[pos2]].liberties == 1) {
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
	&& !MARKED_COLOR_STRING(EAST(lib), color)) {
      if (libs)
	libs[liberties] = lib;
      liberties++;
      if (liberties >= maxlib)
	return liberties;
    }

    /* Check if we already know of this capture. */
    for (l = 0; l < k; l++)
      if (string_number[captured[l]] == string_number[lib])
	break;

    if (l == k) {
      /* Traverse all the stones of the capture and add to the list
       * of liberties those, which are adjacent to at least one own
       * marked string.
       */
      do {
	if (MARKED_COLOR_STRING(SOUTH(lib), color)
	    || MARKED_COLOR_STRING(WEST(lib), color)
	    || MARKED_COLOR_STRING(NORTH(lib), color)
	    || MARKED_COLOR_STRING(EAST(lib), color)) {
	  if (libs)
	    libs[liberties] = lib;
	  liberties++;
	  if (liberties >= maxlib)
	    return liberties;
	}

	lib = NEXT_STONE(lib);
      } while (lib != captured[k]);
    }
  }

  return liberties;
}


/* Find the number of common liberties of the two strings at str1 and str2.
 */

int
count_common_libs(int str1, int str2)
{
  int all_libs1[MAXLIBS], *libs1;
  int liberties1, liberties2;
  int commonlibs = 0;
  int k, n, tmp;
  
  ASSERT_ON_BOARD1(str1);
  ASSERT_ON_BOARD1(str2);
  ASSERT1(IS_STONE(board[str1]), str1);
  ASSERT1(IS_STONE(board[str2]), str2);
  
  n = string_number[str1];
  liberties1 = string[n].liberties;
  
  if (liberties1 > string[string_number[str2]].liberties) {
    n = string_number[str2];
    liberties1 = string[n].liberties;
    tmp = str1;
    str1 = str2;
    str2 = tmp;
  }

  if (liberties1 <= MAX_LIBERTIES) {
    /* Speed optimization: don't copy liberties with findlib */
    libs1 = string[n].libs;
    n = string_number[str2];
    liberties2 = string[n].liberties;
    
    if (liberties2 <= MAX_LIBERTIES) {
      /* Speed optimization: NEIGHBOR_OF_STRING is quite expensive */
      liberty_mark++;
      
      for (k = 0; k < liberties1; k++)
	MARK_LIBERTY(libs1[k]);

      libs1 = string[n].libs;
      for (k = 0; k < liberties2; k++)
	if (!UNMARKED_LIBERTY(libs1[k]))
	  commonlibs++;

      return commonlibs;
    }
  }
  else {
    findlib(str1, MAXLIBS, all_libs1);
    libs1 = all_libs1;
  }
  
  for (k = 0; k < liberties1; k++)
    if (NEIGHBOR_OF_STRING(libs1[k], string_number[str2], board[str2]))
      commonlibs++;
  
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
find_common_libs(int str1, int str2, int maxlib, int *libs)
{
  int all_libs1[MAXLIBS], *libs1;
  int liberties1, liberties2;
  int commonlibs = 0;
  int k, n, tmp;
  
  ASSERT_ON_BOARD1(str1);
  ASSERT_ON_BOARD1(str2);
  ASSERT1(IS_STONE(board[str1]), str1);
  ASSERT1(IS_STONE(board[str2]), str2);
  ASSERT1(libs != NULL, str1);
  
  n = string_number[str1];
  liberties1 = string[n].liberties;
  
  if (liberties1 > string[string_number[str2]].liberties) {
    n = string_number[str2];
    liberties1 = string[n].liberties;
    tmp = str1;
    str1 = str2;
    str2 = tmp;
  }
  
  if (liberties1 <= MAX_LIBERTIES) {
    /* Speed optimization: don't copy liberties with findlib */
    libs1 = string[n].libs;
    n = string_number[str2];
    liberties2 = string[n].liberties;

    if (liberties2 <= MAX_LIBERTIES) {
      /* Speed optimization: NEIGHBOR_OF_STRING is quite expensive */
      liberty_mark++;

      for (k = 0; k < liberties1; k++)
	MARK_LIBERTY(libs1[k]);
      
      libs1 = string[n].libs;
      for (k = 0; k < liberties2; k++)
	if (!UNMARKED_LIBERTY(libs1[k])){
          if (commonlibs < maxlib)
	    libs[commonlibs] = libs1[k];
	  commonlibs++;
	}
      
      return commonlibs;
    }
  }
  else {
    findlib(str1, MAXLIBS, all_libs1);
    libs1 = all_libs1;
  }
  
  for (k = 0; k < liberties1; k++)
    if (NEIGHBOR_OF_STRING(libs1[k], string_number[str2], board[str2])) {
      if (commonlibs < maxlib)
	libs[commonlibs] = libs1[k];
      commonlibs++;
    }
  
  return commonlibs;
}


/* Determine whether two strings have at least one common liberty.
 * If they have and lib != NULL, one common liberty is returned in *lib.
 */

int
have_common_lib(int str1, int str2, int *lib)
{
  int all_libs1[MAXLIBS], *libs1;
  int liberties1;
  int k, n, tmp;
  
  ASSERT_ON_BOARD1(str1);
  ASSERT_ON_BOARD1(str2);
  ASSERT1(IS_STONE(board[str1]), str1);
  ASSERT1(IS_STONE(board[str2]), str2);
  
  n = string_number[str1];
  liberties1 = string[n].liberties;
  
  if (liberties1 > string[string_number[str2]].liberties) {
    n = string_number[str2];
    liberties1 = string[n].liberties;
    tmp = str1;
    str1 = str2;
    str2 = tmp;
  }
  
  if (liberties1 <= MAX_LIBERTIES)
    /* Speed optimization: don't copy liberties with findlib */
    libs1 = string[n].libs;
  else {
    findlib(str1, MAXLIBS, all_libs1);
    libs1 = all_libs1;
  }

  for (k = 0; k < liberties1; k++) {
    if (NEIGHBOR_OF_STRING(libs1[k], string_number[str2], board[str2])) {
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
countstones(int str)
{
  ASSERT_ON_BOARD1(str);
  ASSERT1(IS_STONE(board[str]), str);

  return COUNTSTONES(str);
}


/* Find the stones of the string at str. str must not be
 * empty. The locations of up to maxstones stones are written into
 * stones[]. The full number of stones is returned.
 */

int
findstones(int str, int maxstones, int *stones)
{
  int s;
  int size;
  int pos;
  int k;
  
  ASSERT_ON_BOARD1(str);
  ASSERT1(IS_STONE(board[str]), str);

  s = string_number[str];
  size = string[s].size;
  
  /* Traverse the stones of the string, by following the cyclic chain. */
  pos = FIRST_STONE(s);
  for (k = 0; k < maxstones && k < size; k++) {
    stones[k] = pos;
    pos = NEXT_STONE(pos);
  }

  return size;
}


/* chainlinks returns (in the (adj) array) the chains surrounding
 * the string at (str). The number of chains is returned.
 */

int 
chainlinks(int str, int adj[MAXCHAIN])
{
  struct string_data *s;
  int k;

  ASSERT1(IS_STONE(board[str]), str);

  /* We already have the list ready, just copy it and fill in the
   * desired information.
   */
  s = &string[string_number[str]];
  for (k = 0; k < s->neighbors; k++)
    adj[k] = string[s->neighborlist[k]].origin;

  return s->neighbors;
}


/* chainlinks2 returns (in adj array) the chains surrounding
 * the string at str, which has exactly lib liberties. The number
 * of such chains is returned.
 */

int
chainlinks2(int str, int adj[MAXCHAIN], int lib)
{
  struct string_data *s, *t;
  int k;
  int neighbors;

  ASSERT1(IS_STONE(board[str]), str);

  /* We already have the list ready, just copy the strings with the
   * right number of liberties.
   */
  neighbors = 0;
  s = &string[string_number[str]];
  for (k = 0; k < s->neighbors; k++) {
    t = &string[s->neighborlist[k]];
    if (t->liberties == lib)
      adj[neighbors++] = t->origin;
  }
  return neighbors;
}


/* chainlinks3 returns (in adj array) the chains surrounding
 * the string at str, which have less or equal lib liberties.
 * The number of such chains is returned.
 */

int
chainlinks3(int str, int adj[MAXCHAIN], int lib)
{
  struct string_data *s, *t;
  int k;
  int neighbors;

  ASSERT1(IS_STONE(board[str]), str);

  /* We already have the list ready, just copy the strings with the
   * right number of liberties.
   */
  neighbors = 0;
  s = &string[string_number[str]];
  for (k = 0; k < s->neighbors; k++) {
    t = &string[s->neighborlist[k]];
    if (t->liberties <= lib)
      adj[neighbors++] = t->origin;
  }
  return neighbors;
}


/* extended_chainlinks() returns (in the (adj) array) the opponent
 * strings being directly adjacent to (str) or having a common liberty
 * with (str). The number of such strings is returned.
 *
 * If the both_colors parameter is true, also own strings sharing a
 * liberty are returned.
 */

int 
extended_chainlinks(int str, int adj[MAXCHAIN], int both_colors)
{
  struct string_data *s;
  int n;
  int k;
  int r;
  int libs[MAXLIBS];
  int liberties;

  ASSERT1(IS_STONE(board[str]), str);

  /* We already have the list of directly adjacent strings ready, just
   * copy it and mark the strings.
   */
  s = &string[string_number[str]];
  string_mark++;
  for (n = 0; n < s->neighbors; n++) {
    adj[n] = string[s->neighborlist[n]].origin;
    MARK_STRING(adj[n]);
  }

  /* Get the liberties. */
  liberties = findlib(str, MAXLIBS, libs);

  /* Look for unmarked opponent strings next to a liberty and add the
   * ones which are found to the output.
   */
  for (r = 0; r < liberties; r++) {
    for (k = 0; k < 4; k++) {
      if ((board[libs[r] + delta[k]] == OTHER_COLOR(board[str])
	   || (both_colors && board[libs[r] + delta[k]] == board[str]))
	  && UNMARKED_STRING(libs[r] + delta[k])) {
	adj[n] = string[string_number[libs[r] + delta[k]]].origin;
	MARK_STRING(adj[n]);
	n++;
      }
    }
  }
  
  return n;
}


/*
 * Find the origin of a worm or a cavity, i.e. the point with the
 * smallest 1D board coordinate. The idea is to have a canonical
 * reference point for a string.
 */

int
find_origin(int str)
{
  ASSERT1(IS_STONE(board[str]), str);

  return string[string_number[str]].origin;
}


/* Determine whether a move by color at (pos) would be a self atari,
 * i.e. whether it would get more than one liberty. This function
 * returns true also for the case of a suicide move.
 */

int
is_self_atari(int pos, int color)
{
  int other = OTHER_COLOR(color);
  /* number of empty neighbors */
  int trivial_liberties = 0;
  /* number of captured opponent strings */
  int captures = 0;
  /* Whether there is a friendly neighbor with a spare liberty. If it
   * has more than one spare liberty we immediately return 0.
   */
  int far_liberties = 0;
  
  ASSERT_ON_BOARD1(pos);
  ASSERT1(board[pos] == EMPTY, pos);
  ASSERT1(IS_STONE(color), pos);

  /* 1. Try first to solve the problem without much work. */
  string_mark++;
  
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
  return accuratelib(pos, color, 2, NULL) <= 1;
}


/*
 * Returns true if pos is a liberty of the string at str.
 */

int
liberty_of_string(int pos, int str)
{
  ASSERT_ON_BOARD1(pos);
  ASSERT_ON_BOARD1(str);
  if (IS_STONE(board[pos]))
    return 0;

  return NEIGHBOR_OF_STRING(pos, string_number[str], board[str]);
}


/*
 * Returns true if pos is a second order liberty of the string at str.
 */
int
second_order_liberty_of_string(int pos, int str)
{
  int k;
  ASSERT_ON_BOARD1(pos);
  ASSERT_ON_BOARD1(str);

  for (k = 0; k < 4; k++)
    if (board[pos + delta[k]] == EMPTY
	&& NEIGHBOR_OF_STRING(pos + delta[k], string_number[str], board[str]))
      return 1;

  return 0;
}


/*
 * Returns true if pos is adjacent to the string at str.
 */

int
neighbor_of_string(int pos, int str)
{
  int color = board[str];

  ASSERT1(IS_STONE(color), str);
  ASSERT_ON_BOARD1(pos);

  return NEIGHBOR_OF_STRING(pos, string_number[str], color);
}

/*
 * Returns true if (pos) has a neighbor of color (color).
 */

int
has_neighbor(int pos, int color)
{
  ASSERT_ON_BOARD1(pos);
  ASSERT1(IS_STONE(color), pos);

  return (board[SOUTH(pos)] == color
          || board[WEST(pos)] == color
          || board[NORTH(pos)] == color
          || board[EAST(pos)] == color);
}

/*
 * Returns true if str1 and str2 belong to the same string.
 */

int
same_string(int str1, int str2)
{
  ASSERT_ON_BOARD1(str1);
  ASSERT_ON_BOARD1(str2);
  ASSERT1(IS_STONE(board[str1]), str1);
  ASSERT1(IS_STONE(board[str2]), str2);

  return string_number[str1] == string_number[str2];
}


/*
 * Returns true if the strings at str1 and str2 are adjacent.
 */

int
adjacent_strings(int str1, int str2)
{
  int s1, s2;
  int k;
  
  ASSERT_ON_BOARD1(str1);
  ASSERT_ON_BOARD1(str2);
  ASSERT1(IS_STONE(board[str1]), str1);
  ASSERT1(IS_STONE(board[str2]), str2);

  s1 = string_number[str1];
  s2 = string_number[str2];

  for (k = 0; k < string[s1].neighbors; k++)
    if (string[s1].neighborlist[k] == s2)
      return 1;

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
is_ko(int pos, int color, int *ko_pos)
{
  int other = OTHER_COLOR(color);
  int captures = 0;
  int kpos = 0;
  
  ASSERT_ON_BOARD1(pos);
  ASSERT1(color == WHITE || color == BLACK, pos);

  if (ON_BOARD(SOUTH(pos))) {
    if (board[SOUTH(pos)] != other)
      return 0;
    else if (LIBERTIES(SOUTH(pos)) == 1) {
      kpos = SOUTH(pos);
      captures += string[string_number[SOUTH(pos)]].size;
      if (captures > 1)
	return 0;
    }
  }
  
  if (ON_BOARD(WEST(pos))) {
    if (board[WEST(pos)] != other)
      return 0;
    else if (LIBERTIES(WEST(pos)) == 1) {
      kpos = WEST(pos);
      captures += string[string_number[WEST(pos)]].size;
      if (captures > 1)
	return 0;
    }
  }
  
  if (ON_BOARD(NORTH(pos))) {
    if (board[NORTH(pos)] != other)
      return 0;
    else if (LIBERTIES(NORTH(pos)) == 1) {
      kpos = NORTH(pos);
      captures += string[string_number[NORTH(pos)]].size;
      if (captures > 1)
	return 0;
    }
  }
  
  if (ON_BOARD(EAST(pos))) {
    if (board[EAST(pos)] != other)
      return 0;
    else if (LIBERTIES(EAST(pos)) == 1) {
      kpos = EAST(pos);
      captures += string[string_number[EAST(pos)]].size;
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
is_ko_point(int pos)
{
  ASSERT_ON_BOARD1(pos);

  if (board[pos] == EMPTY) {
    int color;
    if (ON_BOARD(SOUTH(pos)))
      color = board[SOUTH(pos)];
    else
      color = board[NORTH(pos)];
    if (IS_STONE(color) && is_ko(pos, OTHER_COLOR(color), NULL))
      return 1;
  }
  else {
    struct string_data *s = &string[string_number[pos]];
    if (s->liberties == 1 && s->size == 1
	&& is_ko(s->libs[0], OTHER_COLOR(s->color), NULL))
      return 1;
  }

  return 0;
}


/* Returns 1 if at least one string is captured when color plays at pos.
 */
int
does_capture_something(int pos, int color)
{
  int other = OTHER_COLOR(color);

  ASSERT1(board[pos] == EMPTY, pos);

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


/* For each stone in the string at pos, set mx to value mark. If
 * some of the stones in the string are marked prior to calling this
 * function, only the connected unmarked stones starting from pos
 * are guaranteed to become marked. The rest of the string may or may
 * not become marked. (In the current implementation, it will.)
 */
void
mark_string(int str, char mx[BOARDMAX], char mark)
{
  int pos = str;

  ASSERT1(IS_STONE(board[str]), str);

  do {
    mx[pos] = mark;
    pos = NEXT_STONE(pos);
  } while (pos != str);
}


/* Returns true if at least one move has been played at pos
 * at deeper than level 'cutoff' in the reading tree.
 */
int
move_in_stack(int pos, int cutoff)
{
  int k;
  for (k = cutoff; k < stackp; k++)
    if (stack[k] == pos)
      return 1;
  
  return 0;
}


/* Retrieve a move from the move stack. */
void
get_move_from_stack(int k, int *move, int *color)
{
  gg_assert(k < stackp);
  *move = stack[k];
  *color = move_color[k];
}

/* Return the number of stones of the indicated color(s) on the board.
 * This only counts stones in the permanent position, not stones placed
 * by trymove() or tryko(). Use stones_on_board(BLACK | WHITE) to get
 * the total number of stones on the board.
 */
int
stones_on_board(int color)
{
  static int stone_count_for_position = -1;
  static int white_stones = 0;
  static int black_stones = 0;

  gg_assert(stackp == 0);

  if (stone_count_for_position != position_number) {
    int pos;
    white_stones = 0;
    black_stones = 0;
    for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
      if (board[pos] == WHITE)
	white_stones++;
      else if (board[pos] == BLACK)
	black_stones++;
    }
    
    stone_count_for_position = position_number;
  }

  return ((color & BLACK ? black_stones : 0) +
	  (color & WHITE ? white_stones : 0));
}


/* ===================== Statistics  ============================= */


/* Clear statistics. */
void
reset_trymove_counter()
{
  trymove_counter = 0;
}


/* Retrieve statistics. */
int
get_trymove_counter()
{
  return trymove_counter;
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
new_position(void)
{
  int pos;
  int s;

  position_number++;
  next_string = 0;
  liberty_mark = 0;
  string_mark = 0;
  CLEAR_STACKS();

  memset(string, 0, sizeof(string));
  memset(ml, 0, sizeof(ml));

  /* propagate_string relies on non-assigned stones to have
   * string_number -1.
   */
  for (pos = BOARDMIN; pos < BOARDMAX; pos++)
    if (ON_BOARD(pos))
      string_number[pos] = -1;

  /* Find the existing strings. */
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (!ON_BOARD(pos))
      continue;
    if (IS_STONE(board[pos]) && string_number[pos] == -1) {
      string_number[pos] = next_string;
      string[next_string].size = propagate_string(pos, pos);
      string[next_string].color = board[pos];
      string[next_string].origin = pos;
      string[next_string].mark = 0;
      next_string++;
    }
  }
  
  /* Fill in liberty and neighbor info. */
  for (s = 0; s < next_string; s++) {
    find_liberties_and_neighbors(s);
  }
}


/* Build a string and its cyclic list representation from scratch.
 * propagate_string(stone, str) adds the stone (stone) to the string
 * (str) and recursively continues with not already included friendly
 * neighbors. To start a new string at (stone), use
 * propagate_string(stone, stone). The size of the string is returned.
 */

static int
propagate_string(int stone, int str)
{
  int size = 1;
  int k;
  
  if (stone == str) {
    /* Start a new string. */
    next_stone[stone] = stone;
  }
  else {
    /* Link the stone at (stone) to the string including (str) */
    string_number[stone] = string_number[str];
    next_stone[stone] = next_stone[str];
    next_stone[str] = stone;
  }

  /* Look in all four directions for more stones to add. */
  for (k = 0; k < 4; k++) {
    int d = delta[k];
    if (ON_BOARD(stone + d)
	&& board[stone + d] == board[stone]
	&& string_number[stone + d] == -1)
      size += propagate_string(stone + d, str);
  }
  
  return size;
}


/* Build the lists of liberties and neighbors of a string from
 * scratch. No information is pushed onto the stack by this function.
 */

static void
find_liberties_and_neighbors(int s)
{
  int pos;
  int other = OTHER_COLOR(string[s].color);

  /* Clear the marks. */
  liberty_mark++;
  string_mark++;

  /* Traverse the stones of the string, by following the cyclic chain. */
  pos = FIRST_STONE(s);
  do {
    /* Look in each direction for new liberties or new neighbors. Mark
     * already visited liberties and neighbors.
     */
    if (UNMARKED_LIBERTY(SOUTH(pos))) {
      ADD_AND_MARK_LIBERTY(s, SOUTH(pos));
    }
    else if (UNMARKED_COLOR_STRING(SOUTH(pos), other)) {
      ADD_NEIGHBOR(s, SOUTH(pos));
      MARK_STRING(SOUTH(pos));
    }
    
    if (UNMARKED_LIBERTY(WEST(pos))) {
      ADD_AND_MARK_LIBERTY(s, WEST(pos));
    }
    else if (UNMARKED_COLOR_STRING(WEST(pos), other)) {
      ADD_NEIGHBOR(s, WEST(pos));
      MARK_STRING(WEST(pos));
    }
    
    if (UNMARKED_LIBERTY(NORTH(pos))) {
      ADD_AND_MARK_LIBERTY(s, NORTH(pos));
    }
    else if (UNMARKED_COLOR_STRING(NORTH(pos), other)) {
      ADD_NEIGHBOR(s, NORTH(pos));
      MARK_STRING(NORTH(pos));
    }
    
    if (UNMARKED_LIBERTY(EAST(pos))) {
      ADD_AND_MARK_LIBERTY(s, EAST(pos));
    }
    else if (UNMARKED_COLOR_STRING(EAST(pos), other)) {
      ADD_NEIGHBOR(s, EAST(pos));
      MARK_STRING(EAST(pos));
    }
    
    pos = NEXT_STONE(pos);
  } while (!BACK_TO_FIRST_STONE(s, pos));
}


/* Update the liberties of a string from scratch, first pushing the
 * old information.
 */

static void
update_liberties(int s)
{
  int pos;
  int k;

  /* Push the old information. */
  PUSH_VALUE(string[s].liberties);
  for (k = 0; k < string[s].liberties && k < MAX_LIBERTIES; k++) {
    PUSH_VALUE(string[s].libs[k]);
  }
  string[s].liberties = 0;

  /* Clear the liberty mark. */
  liberty_mark++;

  /* Traverse the stones of the string, by following the cyclic chain. */
  pos = FIRST_STONE(s);
  do {
    /* Look in each direction for new liberties. Mark already visited
     * liberties. 
     */
    if (UNMARKED_LIBERTY(SOUTH(pos))) {
      ADD_AND_MARK_LIBERTY(s, SOUTH(pos));
    }
    
    if (UNMARKED_LIBERTY(WEST(pos))) {
      ADD_AND_MARK_LIBERTY(s, WEST(pos));
    }
    
    if (UNMARKED_LIBERTY(NORTH(pos))) {
      ADD_AND_MARK_LIBERTY(s, NORTH(pos));
    }
    
    if (UNMARKED_LIBERTY(EAST(pos))) {
      ADD_AND_MARK_LIBERTY(s, EAST(pos));
    }
    
    pos = NEXT_STONE(pos);
  } while (!BACK_TO_FIRST_STONE(s, pos));
}


/* Remove a string from the list of neighbors and push the changed
 * information.
 */

static void
remove_neighbor(int str_number, int n)
{
  int k;
  int done = 0;
  struct string_data *s = &string[str_number];
  for (k = 0; k < s->neighbors; k++)
    if (s->neighborlist[k] == n) {
      /* We need to push the last entry too because it may become
       * destroyed later.
       */
      PUSH_VALUE(s->neighborlist[s->neighbors - 1]);
      PUSH_VALUE(s->neighborlist[k]);
      PUSH_VALUE(s->neighbors);
      s->neighborlist[k] = s->neighborlist[s->neighbors - 1];
      s->neighbors--;
      done = 1;
      break;
    }
  gg_assert(done);
}


/* Remove one liberty from the list of liberties, pushing changed
 * information. If the string had more liberties than the size of the
 * list, rebuild the list from scratch.
 */

static void
remove_liberty(int str_number, int pos)
{
  int k;
  struct string_data *s = &string[str_number];
  
  if (s->liberties > MAX_LIBERTIES)
    update_liberties(str_number);
  else {
    for (k = 0; k < s->liberties; k++)
      if (s->libs[k] == pos) {
	/* We need to push the last entry too because it may become
	 * destroyed later.
	 */
	PUSH_VALUE(s->libs[s->liberties - 1]);
	PUSH_VALUE(s->libs[k]);
	PUSH_VALUE(s->liberties);
	s->libs[k] = s->libs[s->liberties - 1];
	s->liberties--;
	break;
      }
  }
}


/* Remove a string from the board, pushing necessary information to
 * restore it. Return the number of removed stones.
 */

static int
do_remove_string(int s)
{
  int pos;
  int k;

  /* Traverse the stones of the string, by following the cyclic chain. */
  pos = FIRST_STONE(s);
  do {
    /* Push color, string number and cyclic chain pointers. */
    PUSH_VALUE(string_number[pos]);
    PUSH_VALUE(next_stone[pos]);
    DO_REMOVE_STONE(pos);
    pos = NEXT_STONE(pos);
  } while (!BACK_TO_FIRST_STONE(s, pos));

  /* The neighboring strings have obtained some new liberties and lost
   * a neighbor.
   */
  for (k = 0; k < string[s].neighbors; k++) {
    remove_neighbor(string[s].neighborlist[k], s);
    update_liberties(string[s].neighborlist[k]);
  }

  /* Update the number of captured stones. These are assumed to
   * already have been pushed.
   */
  if (string[s].color == WHITE)
    white_captured += string[s].size;
  else
    black_captured += string[s].size;
    
  return string[s].size;
}


/* We have played an isolated new stone and need to create a new
 * string for it.
 */
static void
create_new_string(int pos)
{
  int s;
  int color = board[pos];
  int other = OTHER_COLOR(color);

  /* Get the next free string number. */
  PUSH_VALUE(next_string);
  s = next_string++;
  string_number[pos] = s;
  /* Set up a size one cycle for the string. */
  next_stone[pos] = pos;

  /* Set trivially known values and initialize the rest to zero. */
  string[s].color = color;
  string[s].size = 1;
  string[s].origin = pos;
  string[s].liberties = 0;
  string[s].neighbors = 0;
  string[s].mark = 0;

  /* Clear the string mark. */
  string_mark++;

  /* In each direction, look for a liberty or a nonmarked opponent
   * neighbor. Mark visited neighbors. There is no need to mark the
   * liberties since we can't find them twice. */
  if (LIBERTY(SOUTH(pos))) {
    ADD_LIBERTY(s, SOUTH(pos));
  }
  else if (UNMARKED_COLOR_STRING(SOUTH(pos), other)) {
    int s2 = string_number[SOUTH(pos)];
    /* Add the neighbor to our list. */
    ADD_NEIGHBOR(s, SOUTH(pos));
    /* Add us to our neighbor's list. */
    PUSH_VALUE(string[s2].neighbors);
    ADD_NEIGHBOR(s2, pos);
    MARK_STRING(SOUTH(pos));
  }
  
  if (LIBERTY(WEST(pos))) {
    ADD_LIBERTY(s, WEST(pos));
  }
  else if (UNMARKED_COLOR_STRING(WEST(pos), other)) {
    int s2 = string_number[WEST(pos)];
    /* Add the neighbor to our list. */
    ADD_NEIGHBOR(s, WEST(pos));
    /* Add us to our neighbor's list. */
    PUSH_VALUE(string[s2].neighbors);
    ADD_NEIGHBOR(s2, pos);
    MARK_STRING(WEST(pos));
  }
  
  if (LIBERTY(NORTH(pos))) {
    ADD_LIBERTY(s, NORTH(pos));
  }
  else if (UNMARKED_COLOR_STRING(NORTH(pos), other)) {
    int s2 = string_number[NORTH(pos)];
    /* Add the neighbor to our list. */
    ADD_NEIGHBOR(s, NORTH(pos));
    /* Add us to our neighbor's list. */
    PUSH_VALUE(string[s2].neighbors);
    ADD_NEIGHBOR(s2, pos);
    MARK_STRING(NORTH(pos));
  }
  
  if (LIBERTY(EAST(pos))) {
    ADD_LIBERTY(s, EAST(pos));
  }
  else if (UNMARKED_COLOR_STRING(EAST(pos), other)) {
    int s2 = string_number[EAST(pos)];
    /* Add the neighbor to our list. */
    ADD_NEIGHBOR(s, EAST(pos));
    /* Add us to our neighbor's list. */
    PUSH_VALUE(string[s2].neighbors);
    ADD_NEIGHBOR(s2, pos);
    /* No need to mark since no visits left. */
  }
}


/* We have played a stone with exactly one friendly neighbor. Add the
 * new stone to that string.
 */
static void
extend_neighbor_string(int pos, int s)
{
  int k;
  int liberties_updated = 0;
  int color = board[pos];
  int other = OTHER_COLOR(color);

  /* Link in the stone in the cyclic list. */
  int pos2 = string[s].origin;
  next_stone[pos] = next_stone[pos2];
  PUSH_VALUE(next_stone[pos2]);
  next_stone[pos2] = pos;
  
  /* Do we need to update the origin? */
  if (pos < pos2) {
    PUSH_VALUE(string[s].origin);
    string[s].origin = pos;
  }
  
  string_number[pos] = s;

  /* The size of the string has increased by one. */
  PUSH_VALUE(string[s].size);
  string[s].size++;

  /* If s has too many liberties, we don't know where they all are and
   * can't update the liberties with the algorithm we otherwise
   * use. In that case we can only recompute the liberties from
   * scratch.
   */
  if (string[s].liberties > MAX_LIBERTIES) {
    update_liberties(s);
    liberties_updated = 1;
  }
  else {
    /* The place of the new stone is no longer a liberty. */
    remove_liberty(s, pos);
  }

  /* Mark old neighbors of the string. */
  string_mark++;
  for (k = 0; k < string[s].neighbors; k++)
    string[string[s].neighborlist[k]].mark = string_mark;

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
    int s2 = string_number[SOUTH(pos)];
    PUSH_VALUE(string[s].neighbors);
    ADD_NEIGHBOR(s, SOUTH(pos));
    PUSH_VALUE(string[s2].neighbors);
    ADD_NEIGHBOR(s2, pos);
    MARK_STRING(SOUTH(pos));
  }
  
  if (LIBERTY(WEST(pos))) {
    if (!liberties_updated
	&& !NON_WEST_NEIGHBOR_OF_STRING(WEST(pos), s, color))
      ADD_LIBERTY(s, WEST(pos));
  }
  else if (UNMARKED_COLOR_STRING(WEST(pos), other)) {
    int s2 = string_number[WEST(pos)];
    PUSH_VALUE(string[s].neighbors);
    ADD_NEIGHBOR(s, WEST(pos));
    PUSH_VALUE(string[s2].neighbors);
    ADD_NEIGHBOR(s2, pos);
    MARK_STRING(WEST(pos));
  }
  
  if (LIBERTY(NORTH(pos))) {
    if (!liberties_updated
	&& !NON_NORTH_NEIGHBOR_OF_STRING(NORTH(pos), s, color))
      ADD_LIBERTY(s, NORTH(pos));
  }
  else if (UNMARKED_COLOR_STRING(NORTH(pos), other)) {
    int s2 = string_number[NORTH(pos)];
    PUSH_VALUE(string[s].neighbors);
    ADD_NEIGHBOR(s, NORTH(pos));
    PUSH_VALUE(string[s2].neighbors);
    ADD_NEIGHBOR(s2, pos);
    MARK_STRING(NORTH(pos));
  }
  
  if (LIBERTY(EAST(pos))) {
    if (!liberties_updated
	&& !NON_EAST_NEIGHBOR_OF_STRING(EAST(pos), s, color))
      ADD_LIBERTY(s, EAST(pos));
  }
  else if (UNMARKED_COLOR_STRING(EAST(pos), other)) {
    int s2 = string_number[EAST(pos)];
    PUSH_VALUE(string[s].neighbors);
    ADD_NEIGHBOR(s, EAST(pos));
    PUSH_VALUE(string[s2].neighbors);
    ADD_NEIGHBOR(s2, pos);
  }
  
}


/* Incorporate the string at pos with the string s.
 */

static void
assimilate_string(int s, int pos)
{
  int k;
  int last;
  int s2 = string_number[pos];
  string[s].size += string[s2].size;

  /* Walk through the s2 stones and change string number. Also pick up
   * the last stone in the cycle for later use.
   */
  pos = FIRST_STONE(s2);
  do {
    PUSH_VALUE(string_number[pos]);
    string_number[pos] = s;
    last = pos;
    pos = NEXT_STONE(pos);
  } while (!BACK_TO_FIRST_STONE(s2, pos));

  /* Link the two cycles together. */
  {
    int pos2 = string[s].origin;
    PUSH_VALUE(next_stone[last]);
    PUSH_VALUE(next_stone[pos2]);
    next_stone[last] = next_stone[pos2];
    next_stone[pos2] = string[s2].origin;
    
    /* Do we need to update the origin? */
    if (string[s2].origin < pos2)
      string[s].origin = string[s2].origin;
  }

  /* Pick up the liberties of s2 that we don't already have.
   * It is assumed that the liberties of s have been marked before
   * this function is called.
   */
  if (string[s2].liberties <= MAX_LIBERTIES) {
    for (k = 0; k < string[s2].liberties; k++) {
      int pos2 = string[s2].libs[k];
      if (UNMARKED_LIBERTY(pos2)) {
	ADD_AND_MARK_LIBERTY(s, pos2);
      }
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
    liberty_mark++;          /* Reset the mark. */
    string[s].liberties = 0; /* To avoid pushing the current list. */
    update_liberties(s);
  }

  /* Remove s2 as neighbor to the neighbors of s2 and instead add s if
   * they don't already have added it. Also add the neighbors of s2 as
   * neighbors of s, unless they already have been added. The already
   * known neighbors of s are assumed to have been marked before this
   * function is called.
   */
  for (k = 0; k < string[s2].neighbors; k++) {
    int t = string[s2].neighborlist[k];
    remove_neighbor(t, s2);
    if (string[t].mark != string_mark) {
      PUSH_VALUE(string[t].neighbors);
      string[t].neighborlist[string[t].neighbors++] = s;
      string[s].neighborlist[string[s].neighbors++] = t;
      string[t].mark = string_mark;
    }
  }
}


/* Create a new string for the stone at pos and assimilate all
 * friendly neighbor strings.
 */

static void
assimilate_neighbor_strings(int pos)
{
  int s;
  int color = board[pos];
  int other = OTHER_COLOR(color);

  /* Get the next free string number. */
  PUSH_VALUE(next_string);
  s = next_string++;
  PARANOID1(s < MAX_STRINGS, pos); 
  string_number[pos] = s;
  /* Set up a size one cycle for the string. */
  next_stone[pos] = pos;
  
  /* Set trivially known values and initialize the rest to zero. */
  string[s].color = color;
  string[s].size = 1;
  string[s].origin = pos;
  string[s].liberties = 0;
  string[s].neighbors = 0;

  /* Clear the marks. */
  liberty_mark++;
  string_mark++;

  /* Mark ourselves. */
  string[s].mark = string_mark;

  /* Look in each direction for
   *
   * 1. liberty: Add if not already visited.
   * 2. opponent string: Add it among our neighbors and us among its
   *    neighbors, unless already visited.
   * 3. friendly string: Assimilate.
   */
  if (UNMARKED_LIBERTY(SOUTH(pos))) {
    ADD_AND_MARK_LIBERTY(s, SOUTH(pos));
  }
  else if (UNMARKED_COLOR_STRING(SOUTH(pos), other)) {
    ADD_NEIGHBOR(s, SOUTH(pos));
    PUSH_VALUE(string[string_number[SOUTH(pos)]].neighbors);
    ADD_NEIGHBOR(string_number[SOUTH(pos)], pos);
    MARK_STRING(SOUTH(pos));
  }
  else if (UNMARKED_COLOR_STRING(SOUTH(pos), color)) {
    assimilate_string(s, SOUTH(pos));
  }

  if (UNMARKED_LIBERTY(WEST(pos))) {
    ADD_AND_MARK_LIBERTY(s, WEST(pos));
  }
  else if (UNMARKED_COLOR_STRING(WEST(pos), other)) {
    ADD_NEIGHBOR(s, WEST(pos));
    PUSH_VALUE(string[string_number[WEST(pos)]].neighbors);
    ADD_NEIGHBOR(string_number[WEST(pos)], pos);
    MARK_STRING(WEST(pos));
  }
  else if (UNMARKED_COLOR_STRING(WEST(pos), color)) {
    assimilate_string(s, WEST(pos));
  }
  
  if (UNMARKED_LIBERTY(NORTH(pos))) {
    ADD_AND_MARK_LIBERTY(s, NORTH(pos));
  }
  else if (UNMARKED_COLOR_STRING(NORTH(pos), other)) {
    ADD_NEIGHBOR(s, NORTH(pos));
    PUSH_VALUE(string[string_number[NORTH(pos)]].neighbors);
    ADD_NEIGHBOR(string_number[NORTH(pos)], pos);
    MARK_STRING(NORTH(pos));
  }
  else if (UNMARKED_COLOR_STRING(NORTH(pos), color)) {
    assimilate_string(s, NORTH(pos));
  }
  
  if (UNMARKED_LIBERTY(EAST(pos))) {
    ADD_LIBERTY(s, EAST(pos));
  }
  else if (UNMARKED_COLOR_STRING(EAST(pos), other)) {
    ADD_NEIGHBOR(s, EAST(pos));
    PUSH_VALUE(string[string_number[EAST(pos)]].neighbors);
    ADD_NEIGHBOR(string_number[EAST(pos)], pos);
  }
  else if (UNMARKED_COLOR_STRING(EAST(pos), color)) {
    assimilate_string(s, EAST(pos));
  }
}


/* Suicide at pos. Remove the neighboring friendly strings.
 */

static void
do_commit_suicide(int pos, int color)
{
  if (board[SOUTH(pos)] == color)
    do_remove_string(string_number[SOUTH(pos)]);

  if (board[WEST(pos)] == color)
    do_remove_string(string_number[WEST(pos)]);

  if (board[NORTH(pos)] == color)
    do_remove_string(string_number[NORTH(pos)]);

  if (board[EAST(pos)] == color)
    do_remove_string(string_number[EAST(pos)]);
}


/* Play a move without legality checking. Suicide is allowed.
 */

static void
do_play_move(int pos, int color)
{
  int other = OTHER_COLOR(color);
  int captured_stones = 0;
  int neighbor_allies = 0;
  int have_liberties = 0;
  int s = -1;
  int south = SOUTH(pos);
  int west = WEST(pos);
  int north = NORTH(pos);
  int east = EAST(pos);
  
  /* Remove captured stones and check for suicide.*/
  if (board[south] == other && LIBERTIES(south) == 1)
    captured_stones += do_remove_string(string_number[south]);

  if (board[west] == other && LIBERTIES(west) == 1)
    captured_stones += do_remove_string(string_number[west]);

  if (board[north] == other && LIBERTIES(north) == 1)
    captured_stones += do_remove_string(string_number[north]);

  if (board[east] == other && LIBERTIES(east) == 1)
    captured_stones += do_remove_string(string_number[east]);


  if (captured_stones == 0) {
    if (LIBERTY(south)
	|| (board[south] == color && LIBERTIES(south) > 1))
      have_liberties = 1;
    else if (LIBERTY(west)
	     || (board[west] == color && LIBERTIES(west) > 1))
      have_liberties = 1;
    else if (LIBERTY(north)
	     ||	(board[north] == color && LIBERTIES(north) > 1))
      have_liberties = 1;
    else if (LIBERTY(east)
	     ||(board[east] == color && LIBERTIES(east) > 1))
      have_liberties = 1;
  }
  else
      have_liberties = 1;


  /* No captures and no liberties -> suicide. */
  if (have_liberties == 0 && captured_stones == 0) {
    do_commit_suicide(pos, color);
    return;
  }
  
  /* Put down the stone. */
  DO_ADD_STONE(pos, color);

  /* Count the number of adjacent strings of my color and remove
   * pos as liberty for the adjacent opponent strings.
   */
  string_mark++;

  if (UNMARKED_COLOR_STRING(south, color)) {
    neighbor_allies++;
    s = string_number[south];
    MARK_STRING(south);
  }
  else if (UNMARKED_COLOR_STRING(south, other)) {
    remove_liberty(string_number[south], pos);
    MARK_STRING(south);
  }    
  
  if (UNMARKED_COLOR_STRING(west, color)) {
    neighbor_allies++;
    s = string_number[west];
    MARK_STRING(west);
  }
  else if (UNMARKED_COLOR_STRING(west, other)) {
    remove_liberty(string_number[west], pos);
    MARK_STRING(west);
  }    
  
  if (UNMARKED_COLOR_STRING(north, color)) {
    neighbor_allies++;
    s = string_number[north];
    MARK_STRING(north);
  }
  else if (UNMARKED_COLOR_STRING(north, other)) {
    remove_liberty(string_number[north], pos);
    MARK_STRING(north);
  }    
  
  if (UNMARKED_COLOR_STRING(east, color)) {
    neighbor_allies++;
    s = string_number[east];
  }
  else if (UNMARKED_COLOR_STRING(east, other))
    remove_liberty(string_number[east], pos);
  
  /* Choose strategy depending on the number of friendly neighbors. */
  if (neighbor_allies == 0)
    create_new_string(pos);
  else if (neighbor_allies == 1) {
    gg_assert(s >= 0);
    extend_neighbor_string(pos, s);
    return; /* can't be a ko, we're done */
  }
  else {
    assimilate_neighbor_strings(pos);
    return; /* can't be a ko, we're done */
  }

  /* Check whether this move was a ko capture and if so set 
   * board_ko_pos.
   *
   * No need to push board_ko_pos on the stack, 
   * because this has been done earlier.
   */
  s = string_number[pos];
  if (string[s].liberties == 1
      && string[s].size == 1
      && captured_stones == 1) {
    /* In case of a double ko: clear old ko position first. */
    if (board_ko_pos != NO_MOVE)
      hashdata_invert_ko(&hashdata, board_ko_pos);
    board_ko_pos = string[s].libs[0];
    hashdata_invert_ko(&hashdata, board_ko_pos);
  }
}


/* Find the liberties a move of the given color at pos would have,
 * excluding possible captures, by traversing all adjacent friendly
 * strings. This is a fallback used by approxlib() when a
 * faster algorithm can't be used.
 */

static int
slow_approxlib(int pos, int color, int maxlib, int *libs)
{
  int liberties = 0;
  int k;

  liberty_mark++;
  MARK_LIBERTY(pos);
  string_mark++;
  for (k = 0; k < 4; k++) {
    int d = delta[k];
    if (UNMARKED_LIBERTY(pos + d)) {
      if (libs)
	libs[liberties] = pos + d;
      liberties++;
      if (liberties == maxlib)
	return liberties;
      MARK_LIBERTY(pos + d);
    }
    else if (board[pos + d] == color
	     && UNMARKED_STRING(pos + d)) {
      int s = string_number[pos + d];
      int pos2;
      pos2 = FIRST_STONE(s);
      do {
	int l;
	for (l = 0; l < 4; l++) {
	  int d2 = delta[l];
	  if (UNMARKED_LIBERTY(pos2 + d2)) {
	    if (libs)
	      libs[liberties] = pos2 + d2;
	    liberties++;
	    if (liberties == maxlib)
	      return liberties;
	    MARK_LIBERTY(pos2 + d2);
	  }
	}
	
	pos2 = NEXT_STONE(pos2);
      } while (!BACK_TO_FIRST_STONE(s, pos2));
      MARK_STRING(pos + d);
    }
  }
  return liberties;
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
incremental_order_moves(int move, int color, int str,
			int *number_edges, int *number_same_string,
			int *number_own, int *number_opponent,
			int *captured_stones, int *threatened_stones,
			int *saved_stones, int *number_open)
{
#if NO_UNROLL == 1
  int pos;
  int k;

  /* Clear the string mark. */
  string_mark++;

  for (k = 0; k < 4; k++) {
    pos = move + delta[k];
    if (!ON_BOARD(pos))
      (*number_edges)++;
    else if (board[pos] == EMPTY)
      (*number_open)++;
    else {
      int s = string_number[pos];
      if (string_number[str] == s)
	(*number_same_string)++;
      
      if (board[pos] == color) {
	(*number_own)++;
	if (string[s].liberties == 1)
	  (*saved_stones) += string[s].size;
      }
      else {
	(*number_opponent)++;
	if (string[s].liberties == 1) {
	  int r;
	  struct string_data *t;
	  (*captured_stones) += string[s].size;
	  for (r = 0; r < string[s].neighbors; r++) {
	    t = &string[string[s].neighborlist[r]];
	    if (t->liberties == 1)
	      (*saved_stones) += t->size;
	  }
	}
	else if (string[s].liberties == 2 && UNMARKED_STRING(pos)) {
	  (*threatened_stones) += string[s].size;
	  MARK_STRING(pos);
	}
      }
    }
  }
  
#else
#define code1(arg) \
  if (!ON_BOARD(arg)) \
    (*number_edges)++; \
  else if (board[arg] == EMPTY) \
    (*number_open)++; \
  else { \
    int s = string_number[arg]; \
    if (string_number[str] == s) \
      (*number_same_string)++; \
    if (board[arg] == color) { \
      (*number_own)++; \
      if (string[s].liberties == 1) \
	(*saved_stones) += string[s].size; \
    } \
    else { \
      (*number_opponent)++; \
      if (string[s].liberties == 1) { \
	int r; \
	struct string_data *t; \
	(*captured_stones) += string[s].size; \
	for (r = 0; r < string[s].neighbors; r++) { \
	  t = &string[string[s].neighborlist[r]]; \
	  if (t->liberties == 1) \
	    (*saved_stones) += t->size; \
	} \
      } \
      else if (string[s].liberties == 2 && UNMARKED_STRING(arg)) { \
	(*threatened_stones) += string[s].size; \
        MARK_STRING(arg); \
      } \
    } \
  }

  /* Clear the string mark. */
  string_mark++;

  code1(SOUTH(move));
  code1(WEST(move));
  code1(NORTH(move));
  code1(EAST(move));
#endif
}


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
