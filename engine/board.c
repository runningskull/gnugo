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


/* The functions in this file implements a go board with incremental
 * update of strings and liberties.
 * 
 * See the Texinfo documentation (Utility Functions: Incremental Board)
 * for an introduction.
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>

#include "liberty.h"
#include "hash.h"
#include "sgftree.h"
#include "gg_utils.h"


/* ================================================================ */
/*                          data structures                         */
/* ================================================================ */


/* Incremental string data. */
struct string_data {
  int color;                       /* Color of string, BLACK or WHITE */
  int size;                        /* Number of stones in string. */
  int origini;                     /* Coordinates of "origin", i.e. */
  int originj;                     /* "upper left" stone. */
  int liberties;                   /* Number of liberties. */
  int libi[MAX_LIBERTIES];         /* Coordinates of liberties. */
  int libj[MAX_LIBERTIES];
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
(change_stack_pointer = change_stack, vertex_stack_pointer = vertex_stack)

/* Begin a record : adress==NULL */
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
static int string_number[MAX_BOARD][MAX_BOARD];


/* The stones in a string are linked together in a cyclic list. 
 * These are the coordinates to the next stone in the string.
 */
static int next_stonei[MAX_BOARD][MAX_BOARD];
static int next_stonej[MAX_BOARD][MAX_BOARD];


/* ---------------------------------------------------------------- */


/* Macros to traverse the stones of a string.
 *
 * Usage:
 * int s;
 * s=find_the_string()
 * FIRST_STONE(s, i, j);
 *   do {
 *    use_stone(i, j);
 *    NEXT_STONE(i, j);
 *  } while (!BACK_TO_FIRST_STONE(s, i, j));
 */
#define FIRST_STONE(s, i, j) \
  (i = string[s].origini, j = string[s].originj)

#define NEXT_STONE(i, j) \
  do {\
    int nexti = next_stonei[i][j];\
    j = next_stonej[i][j];\
    i = nexti;\
  } while (0)

#define BACK_TO_FIRST_STONE(s, i, j) \
  (i == string[s].origini &&\
   j == string[s].originj)


/* Assorted useful macros.
 *
 * Some of them could have been functions but are implemented as
 * macros for speed.
 */

#define LIBERTY(i, j) \
  (p[i][j] == EMPTY)

#define UNMARKED_LIBERTY(i, j) \
  (p[i][j] == EMPTY && ml[i][j] != liberty_mark)

#define MARK_LIBERTY(i, j) \
  ml[i][j] = liberty_mark

#define UNMARKED_STRING(i, j) \
  (string[string_number[i][j]].mark != string_mark)

#define UNMARKED_OPPONENT_STRING(s, i, j)\
  (p[i][j] == OTHER_COLOR(string[s].color)\
   && string[string_number[i][j]].mark != string_mark)

#define UNMARKED_OWN_STRING(s, i, j)\
  (p[i][j] == string[s].color\
   && string[string_number[i][j]].mark != string_mark)

#define MARK_STRING(i, j) string[string_number[i][j]].mark = string_mark

#define STRING_AT_VERTEX(i, j, s)\
  (p[i][j] != EMPTY && string_number[i][j] == (s))
  
#define LIBERTIES(i, j)\
  string[string_number[i][j]].liberties

#define ADD_LIBERTY(s, i, j)\
  do {\
    if (string[s].liberties < MAX_LIBERTIES) {\
      string[s].libi[string[s].liberties] = i;\
      string[s].libj[string[s].liberties] = j;\
    }\
    string[s].liberties++;\
  } while (0)

#define ADD_AND_MARK_LIBERTY(s, i, j)\
  do {\
    if (string[s].liberties < MAX_LIBERTIES) {\
      string[s].libi[string[s].liberties] = i;\
      string[s].libj[string[s].liberties] = j;\
    }\
    string[s].liberties++;\
    ml[i][j] = liberty_mark;\
  } while (0)

#define ADD_NEIGHBOR(s, i, j)\
  string[s].neighborlist[string[s].neighbors++] = string_number[i][j]

#define DO_ADD_STONE(i, j, color)\
  do {\
    PUSH_VERTEX(p[i][j]);\
    p[i][j] = color;\
    hashdata_invert_stone(&hashdata, i, j, color);\
  } while (0)

#define DO_REMOVE_STONE(i, j)\
  do {\
    PUSH_VERTEX(p[i][j]);\
    hashdata_invert_stone(&hashdata, i, j, p[i][j]);\
    p[i][j] = EMPTY;\
  } while (0)


/* ---------------------------------------------------------------- */


/* True if the data structures are up to date for the current 
 * board position.
 */
static int strings_initialized = 0;


/* Number of the next free string. */
static int next_string;


/* For marking purposes. */
static int ml[MAX_BOARD][MAX_BOARD];
static int liberty_mark;
static int string_mark;


/* Forward declaration. */
static int  do_trymove(int i, int j, int color, int ignore_ko);
static void undo_move(void);
static void new_position(void);
static void init_board(void);
static int  propagate_string(int i, int j, int m, int n);
static void find_liberties_and_neighbors(int s);
static int  do_remove_string(int s);
static void do_play_move(int i, int j, int color);
static int  slow_approxlib(int i, int j, int color, int maxlib, 
			   int *libi, int *libj);
static int  incremental_sloppy_self_atari(int m, int n, int color);


/* Statistics. */
static int trymove_counter = 0;


/* Coordinates for the eight directions, ordered
 * south, west, north, east, southwest, northwest, northeast, southeast.
 */
int deltai[8] = { 1,  0, -1,  0,  1, -1, -1, 1};
int deltaj[8] = { 0, -1,  0,  1, -1, -1,  1, 1};


/* ================================================================ */
/*                         External functions                       */
/* ================================================================ */


/*
 * Set up an entirely new position.
 */

void
setup_board(Intersection new_p[MAX_BOARD][MAX_BOARD], int koi, int koj,
	    int *last_i, int *last_j, 
	    float new_komi, int w_captured, int b_captured)
{
  int k;

  memcpy(p, new_p, sizeof(p));
  board_ko_i = koi;
  board_ko_j = koj;

  for (k = 0; k < 2; k++) {
    if (last_i[k] != -1) {
      last_moves_i[k] = last_i[k];
      last_moves_j[k] = last_j[k];
    }
  }

  komi = new_komi;
  white_captured = w_captured;
  black_captured = b_captured;

  hashdata_recalc(&hashdata, p, board_ko_i, board_ko_j);
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
  
  memset(p, EMPTY, sizeof(p));
  board_ko_i = -1;
  board_ko_j = -1;

  for (k = 0; k < 2; k++) {
    last_moves_i[k] = -1;
    last_moves_j[k] = -1;
  }

  white_captured = 0;
  black_captured = 0;

  hashdata_recalc(&hashdata, p, board_ko_i, board_ko_j);
  new_position();
  movenum = 0;
}


/* ================================================================ */
/*                 Pushing and popping of boards                    */
/* ================================================================ */


/* Stack of trial moves to get to current
 * position and which color made them. Perhaps 
 * this should be one array of a structure 
 */
static int      stacki[MAXSTACK];
static int      stackj[MAXSTACK];
static int      move_color[MAXSTACK];

static Hash_data  hashdata_stack[MAXSTACK];


/*
 * trymove pushes the position onto the stack, and makes a move
 * at (i, j) of color. Returns one if the move is legal. The
 * stack pointer is only incremented if the move is legal.
 *
 * The way to use this is:
 *
 *   if (trymove(i, j, color, [message], k, l)) {
 *      ...
 *      popgo();
 *   }   
 *
 * The message can be written as a comment to an sgf file using 
 * sgfdump().  (k, l) can be -1 if they are not needed but if they are 
 * not the location of (k, l) is included in the comment.
 */

int 
trymove(int i, int j, int color, const char *message, int k, int l,
	int komaster, int kom_i, int kom_j)
{
  /* Do the real work elsewhere. */
  if (!do_trymove(i, j, color, 0))
    return 0;
  
  /* Store the move in an sgf tree if one is available. */
  if (sgf_dumptree) {
    char buf[100];
    if (k == -1) {
      if (komaster != EMPTY)
	gg_snprintf(buf, 100, "%s (variation %d, hash %lx, komaster %s:%c%d)", 
		    message, count_variations, hashdata.hashval,
		    color_to_string(komaster),
		    kom_j + 'A' + (kom_j >= 8), board_size - kom_i);
      else
	gg_snprintf(buf, 100, "%s (variation %d, hash %lx)", 
		    message, count_variations, hashdata.hashval);
    }
    else {
      if (komaster != EMPTY)
	gg_snprintf(buf, 100, "%s at %c%d (variation %d, hash %lx, komaster %s:%c%d)", 
		    message,
		    l + 'A' + (l >= 8), board_size - k, count_variations,
		    hashdata.hashval, color_to_string(komaster),
		    kom_j + 'A' + (kom_j >= 8), board_size - kom_i);
      else
	gg_snprintf(buf, 100, "%s at %c%d (variation %d, hash %lx)", 
		    message,
		    l + 'A' + (l>=8), board_size - k, count_variations,
		    hashdata.hashval);
    }
    sgftreeAddPlayLast(sgf_dumptree, NULL, color, i, j);
    sgftreeAddComment(sgf_dumptree, NULL, buf);
  }
  
  if (count_variations)
    count_variations++;
  stats.nodes++;

  return 1;
}


/*
 * tryko pushes the position onto the stack, and makes a move
 * at (i, j) of color. The move is allowed even if it is an
 * illegal ko capture. It is to be imagined that (color) has
 * made an intervening ko threat which was answered and now
 * the continuation is to be explored.
 *
 * Return 1 if the move is legal with the above caveat. Returns
 * zero if it is not legal because of suicide.
 */

int 
tryko(int i, int j, int color, const char *message, 
      int komaster, int kom_i, int kom_j)
{
  /* Do the real work elsewhere. */
  if (!do_trymove(i, j, color, 1))
    return 0;

  if (sgf_dumptree) {
    char buf[100];
    if (!message)
      message = "???";
    if (komaster != EMPTY)
      gg_snprintf(buf, 100, "tryko: %s (variation %d, %lx, komaster %s:%c%d)", 
		  message,
		  count_variations,
		  hashdata.hashval,
		  color_to_string(komaster),
		  kom_i + 'A' + (kom_i >= 8), board_size - kom_j);
    else
      gg_snprintf(buf, 100, "tryko: %s (variation %d, %lx)", 
		  message,
		  count_variations,
		  hashdata.hashval);
    sgftreeAddPlayLast(sgf_dumptree, NULL, color, -1, -1);
    sgftreeAddComment(sgf_dumptree, NULL, "tenuki (ko threat)");
    sgftreeAddPlayLast(sgf_dumptree, NULL, OTHER_COLOR(color), -1, -1);
    sgftreeAddComment(sgf_dumptree, NULL, "tenuki (answers ko threat)");
    sgftreeAddPlayLast(sgf_dumptree, NULL, color, i, j);
    sgftreeAddComment(sgf_dumptree, NULL, buf);
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
do_trymove(int i, int j, int color, int ignore_ko)
{
  /* 1. The move must be inside the board and the color must be BLACK
   * or WHITE.
   */
  ASSERT_ON_BOARD(i, j);
  gg_assert(color == BLACK || color == WHITE);
  
  /* Update the reading tree shadow. */
  shadow[i][j] = 1;

  /* 2. The location must be empty. */
  if (p[i][j] != EMPTY)
    return 0;

  /* 3. The location must not be the ko point, unless ignore_ko==1. */
  if (!ignore_ko
      && i == board_ko_i && j == board_ko_j) {
    if (((i > 0) && (p[i-1][j] != color))
	|| ((i == 0) && (p[i+1][j] != color))) {
      RTRACE("%m would violate the ko rule\n", i, j);
      return 0;
    }
  }

  /* 4. Test for suicide. */
  if (is_suicide(i, j, color)) {
    RTRACE("%m would be suicide\n", i, j);
    return 0;
  }
  
  /* Check for stack overflow. */
  if (stackp >= MAXSTACK-2) {
    fprintf(stderr, 
	    "gnugo: Truncating search. This is beyond my reading ability!\n");
    if (verbose > 0) {
      showboard(0);
      dump_stack();
    }
    return 0;
  }

  /* Only count trymove when we do create a new position. */
  trymove_counter++;
  
  /* So far, so good. Now push the move on the move stack. These are
   * needed for dump_stack().
   */
  stacki[stackp] = i;
  stackj[stackp] = j;
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
  PUSH_VALUE(board_ko_i);
  PUSH_VALUE(board_ko_j);
  memcpy(&hashdata_stack[stackp], &hashdata, sizeof(hashdata));

  board_ko_i = -1;
  board_ko_j = -1;
  hashdata_remove_ko(&hashdata);
  
  PUSH_VALUE(black_captured);
  PUSH_VALUE(white_captured);

  if (showstack)
    gprintf("        *** STACK before push: %d\n", stackp);
  ++stackp;

  if (verbose == 4)
    dump_stack();

  do_play_move(i, j, color);

  return 1;
}


/*
 * popgo pops the position from the stack.
 */

void
popgo()
{
  stackp--;
  if (showstack)
    gprintf("<=    *** STACK  after pop: %d\n", stackp);
  
  undo_move();
  
  memcpy(&hashdata, &(hashdata_stack[stackp]), sizeof(hashdata));
  if (sgf_dumptree) {
    sgf_dumptree->lastnode = sgf_dumptree->lastnode->parent;
    /* After tryko() we need to undo two pass nodes too. Since we have
     * no other way to identify ko moves, we skip all pass nodes.
     */
    while (is_pass_node(sgf_dumptree->lastnode, board_size))
      sgf_dumptree->lastnode = sgf_dumptree->lastnode->parent;
  }
}


/* Silent version of popgo(), suitable for use if you have called
 * do_trymove() without passing through trymove() or tryko().
 */

static void
silent_popgo(void)
{
  stackp--;
  undo_move();
  memcpy(&hashdata, &(hashdata_stack[stackp]), sizeof(hashdata));
}


/* Restore board state to the position before the last move. This is
 * accomplished by popping everything that was stored on the stacks
 * since the last BEGIN_CHANGE_RECORD().
 */

static void
undo_move()
{
  gg_assert(strings_initialized);
  gg_assert(change_stack_pointer - change_stack <= STACK_SIZE);

  if (0)
    gprintf("Change stack size = %d\n", change_stack_pointer - change_stack);

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
    gprintf("%o%s:%m ", move_color[n] == BLACK ? "B" : "W",
	    stacki[n], stackj[n]);
  
#if !TRACE_READ_RESULTS
  if (count_variations)
    gprintf("%o (variation %d)", count_variations-1);
#else
  gprintf("%o (%d)", hashdata.hashval);
#endif

  gprintf("%o\n");
}


/* ================================================================ */
/*                Non-invertable board manipulation                 */
/* ================================================================ */


/*
 * do_add_stone and do_remove_stone are defined as macros for 
 * efficiency reasons.
 */

#define do_add_stone(i, j, color) \
  do { \
    p[i][j] = color; \
    hashdata_invert_stone(&hashdata, i, j, color); \
  } while (0)

#define do_remove_stone(i, j) \
  do { \
    hashdata_invert_stone(&hashdata, i, j, p[i][j]); \
    p[i][j] = EMPTY; \
  } while (0)


/*
 * And now the functions, accessible from outside this file.
 */

/* place a stone on the board and update the hashdata. */

void
add_stone(int i, int j, int color)
{
  ASSERT(stackp == 0, i, j);
  ASSERT_ON_BOARD(i, j);
  ASSERT(p[i][j] == EMPTY, i, j);

  do_add_stone(i, j, color);
#if 0
  new_position();
#else
  CLEAR_STACKS();
#endif
}


/* remove a stone from the board and update the hashdata. */

void
remove_stone(int i, int j)
{
  ASSERT(stackp == 0, i, j);
  ASSERT_ON_BOARD(i, j);
  ASSERT(p[i][j] != EMPTY, i, j);

  do_remove_stone(i, j);
#if 0
  new_position();
#else
  CLEAR_STACKS();
#endif
}


/* Play a move. If you want to test for legality you should first call
 * is_legal(). This function strictly follows the algorithm: 
 * 1. Place a stone of given color on the board.
 * 2. If there are any adjacent opponent strings without liberties,
 *    remove them and increase the prisoner count. 
 * 3. If the newly placed stone is part of a string without liberties,
 *    remove it and increase the prisoner count.
 *
 * In contrast to a move played by trymove() or tryko(), this move
 * can't be easily unplayed.
 */
void
play_move(int i, int j, int color)
{
#if CHECK_HASHING
  Hash_data oldkey;

  /* Check the hash table to see if it corresponds to the cumulative one. */
  hashdata_recalc(&oldkey, p, board_ko_i, board_ko_j);
  gg_assert(hashdata_diff_dump(&oldkey, &hashdata) == 0);
#endif
  
  gg_assert(stackp == 0);
  
  last_moves_i[1] = last_moves_i[0];
  last_moves_j[1] = last_moves_j[0];
  last_moves_i[0] = i;
  last_moves_j[0] = j;

  board_ko_i = -1;
  board_ko_j = -1;
  hashdata_remove_ko(&hashdata);

  /* If the move is a pass, we can skip some steps. */
  if (i != -1 || j != -1) {
    ASSERT_ON_BOARD(i, j);
    ASSERT(p[i][j] == EMPTY, i, j);

    /* Do play the move. */
    do_play_move(i, j, color);

#if CHECK_HASHING
    /* Check the hash table to see if it equals the previous one. */
    hashdata_recalc(&oldkey, p, board_ko_i, board_ko_j);
    gg_assert(hashdata_diff_dump(&oldkey, &hashdata) == 0);
#endif
  }
  
  movenum++;
#if 0
  new_position();
#else
  CLEAR_STACKS();
#endif
}



/* ================================================================ */
/*                        Utility functions                         */
/* ================================================================ */


/*
 * Test if the move is a pass or not.  Return 1 if it is.
 */

int
is_pass(int i, int j)
{
  if (i < 0) {
    gg_assert(j < 0);
    return 1;
  }

  return 0;
}


/*
 * is_legal(i, j, color) determines whether the move (color) at
 * (i, j) is legal.
 */

int 
is_legal(int i, int j, int color)
{
  /* 0. A pass move is always legal. */
  if (i == -1 && j == -1)
    return 1;

  /* 1. The move must be inside the board. */
  ASSERT_ON_BOARD(i, j);

  /* 2. The location must be empty. */
  if (p[i][j]!=EMPTY) 
    return 0;

  /* 3. The location must not be the ko point. */
  if (i == board_ko_i && j == board_ko_j)
    if ((i > 0 && p[i-1][j] != color)
	|| (i == 0 && p[i+1][j] != color)) {
      RTRACE("%m would violate the ko rule\n", i, j);
      return 0;
    }

  /* Check for stack overflow. */
  if (stackp >= MAXSTACK-2) {
    fprintf(stderr, 
	    "gnugo: Truncating search. This is beyond my reading ability!\n");
    return 0;
  }

  /* Check for suicide. */
  if (!allow_suicide && is_suicide(i, j, color)) {
    RTRACE("%m would be suicide\n", i, j);
    return 0;
  }
  
  return 1;
}


/*
 * is_suicide(i, j, color) determines whether the move (color) at
 * (i, j) would be a  suicide.
 *
 * This is the case if
 * 1. There is no neighboring empty intersection.
 * 2. There is no neighboring opponent string with exactly one liberty.
 * 3. There is no neighboring friendly string with more than one liberty.
 */
int 
is_suicide(int m, int n, int color)
{
  ASSERT_ON_BOARD(m, n);
  gg_assert(p[m][n] == EMPTY);

  if (!strings_initialized)
    init_board();
  
  /* Check for suicide. */
  if (m > 0
      && (LIBERTY(m-1, n)
	  || ((p[m-1][n] == color) ^ (LIBERTIES(m-1, n) == 1))))
    return 0;

  if (m < board_size-1
      && (LIBERTY(m+1, n)
	  || ((p[m+1][n] == color) ^ (LIBERTIES(m+1, n) == 1))))
    return 0;

  if (n > 0
      && (LIBERTY(m, n-1)
	  || ((p[m][n-1] == color) ^ (LIBERTIES(m, n-1) == 1))))
    return 0;

  if (n < board_size-1
      && (LIBERTY(m, n+1)
	  || ((p[m][n+1] == color) ^ (LIBERTIES(m, n+1) == 1))))
    return 0;

  return 1;
}


/*
 * is_illegal_ko_capture(i, j, color) determines whether the move
 * (color) at (i, j) would be an illegal ko capture.
 */
int 
is_illegal_ko_capture(int i, int j, int color)
{
  ASSERT_ON_BOARD(i, j);
  ASSERT(p[i][j] == EMPTY, i, j);

  return (i == board_ko_i && j == board_ko_j
	  && ((i > 0 && p[i-1][j] != color)
	      || (i == 0 && p[i+1][j] != color)));
}


/* Variation of trymove()/tryko() where ko captures (both conditional
 * and unconditional) must follow a komaster scheme.
 *
 * FIXME: This function could be optimized by integrating the
 * trymove()/tryko() code.
 */

#define KOMASTER_SCHEME 1

#if KOMASTER_SCHEME == 1

/* I. Dan's simple scheme, O to move.
 * 
 * 1. Komaster is EMPTY.
 * 1a) Unconditional ko capture is allowed. Komaster remains EMPTY.
 * 1b) Conditional ko capture is allowed. Komaster is set to O and
 *     (kom_i, kom_j) to the location of the ko, where a stone was
 *     just removed.
 * 
 * 2. Komaster is O:
 * 2a) Only nested ko captures are allowed.
 * 2b) If komaster fills the ko at (kom_i,kom_j) then komaster reverts to
 *     EMPTY.
 * 
 * 3. Komaster is X:
 *    Play at (kom_i,kom_j) is not allowed. Any other ko capture
 *    is allowed. If O takes another ko, komaster becomes GRAY.
 * 
 * 4. Komaster is GRAY:
 *    Ko captures are not allowed. If the ko at (kom_i,kom_j) is
 *    filled then the komaster reverts to EMPTY.
 * 
 */
int
komaster_trymove(int i, int j, int color,
		 const char *message, int si, int sj,
		 int komaster, int kom_i, int kom_j,
		 int *new_komaster, int *new_kom_i, int *new_kom_j,
		 int *is_conditional_ko, int consider_conditional_ko)
{
  int other = OTHER_COLOR(color);
  int ko_move;
  int ki, kj;

  /* First we check whether the ko claimed by komaster has been
   * resolved. If that is the case, we revert komaster to EMPTY.
   *
   * The ko has been resolved in favor of the komaster if it has
   * been filled, or if it is no longer a ko and an opponent move
   * there is suicide. If komaster == GRAY we don't remember who
   * owns the ko so we have to try both colors.
   */
  if (komaster != EMPTY 
      && (p[kom_i][kom_j] != EMPTY
	  || (komaster != GRAY
	      && !is_ko(kom_i, kom_j, OTHER_COLOR(komaster), NULL, NULL)
	      && is_suicide(kom_i, kom_j, OTHER_COLOR(komaster)))
	  || (komaster == GRAY
	      && !is_ko(kom_i, kom_j, BLACK, NULL, NULL)
	      && !is_ko(kom_i, kom_j, WHITE, NULL, NULL)
	      && (is_suicide(kom_i, kom_j, BLACK)
		  || is_suicide(kom_i, kom_j, WHITE))))) {
    komaster = EMPTY;
    kom_i = -1;
    kom_j = -1;
  }

  /* Usually the komaster parameters are unchanged. */
  *new_komaster = komaster;
  *new_kom_i = kom_i;
  *new_kom_j = kom_j;

  *is_conditional_ko = 0;
  ko_move = is_ko(i, j, color, &ki, &kj);

  if (ko_move) {
    /* If opponent is komaster we may not capture his ko. */
    if (komaster == other
	&& i == kom_i
	&& j == kom_j)
      return 0;

    /* If opponent is komaster we may not capture ko at all. */
    if (komaster == GRAY)
      return 0;

    /* If we are komaster, we may only do nested captures. */
    if (komaster == color
	&& !((ki == kom_i+1 || ki == kom_i-1)
	     && (kj == kom_j+1 || kj == kom_j-1)))
      return 0;
  }

  if (!trymove(i, j, color, message, si, sj, komaster, kom_i, kom_j)) {
    if (!consider_conditional_ko)
      return 0;

    if (!tryko(i, j, color, message, komaster, kom_i, kom_j))
      return 0; /* Suicide. */
      
    *is_conditional_ko = 1;

    /* Conditional ko capture, set komaster parameters. */
    if (komaster == EMPTY) {
      *new_komaster = color;
      *new_kom_i = ki;
      *new_kom_j = kj;
      return 1;
    }
  }

  if (!ko_move)
    return 1;

  if (komaster == other)
    *new_komaster = GRAY;
  else if (komaster == color) {
    *new_kom_i = ki;
    *new_kom_j = kj;
  }

  return 1;
}

#endif




#if KOMASTER_SCHEME == 2
/* Simple komaster scheme, equivalent to the one implemented in 2.7.232.
 *
 * (II) Original 2.7.232 scheme, O to move.
 * 
 * 1. Komaster is EMPTY.
 * 1a) Unconditional ko capture is allowed. Komaster remains EMPTY.
 * 1b) Conditional ko capture is allowed. Komaster is set to O and
 *     (kom_i, kom_j) to the location of the ko, where a stone was
 *     just removed.
 * 
 * 2. Komaster is O:
 * 2a) Conditional ko capture is not allowed.
 * 2b) Unconditional ko capture is allowed. Komaster parameters unchanged.
 * 
 * 3. Komaster is X:
 * 3a) Conditional ko capture is not allowed.
 * 3b) Unconditional ko capture is allowed except for a move at (kom_i,
 *     kom_j). Komaster parameters unchanged.
 * 
 * 4. Komaster is GRAY:
 *    Doesn't happen.
 */
int
komaster_trymove(int i, int j, int color,
		 const char *message, int si, int sj,
		 int komaster, int kom_i, int kom_j,
		 int *new_komaster, int *new_kom_i, int *new_kom_j,
		 int *is_conditional_ko, int consider_conditional_ko)
{
  int other = OTHER_COLOR(color);
  int ki, kj;

  /* Usually the komaster parameters are unchanged. */
  *new_komaster = komaster;
  *new_kom_i = kom_i;
  *new_kom_j = kom_j;

  *is_conditional_ko = 0;

  /* If opponent is komaster we may not capture his ko. */
  if (is_ko(i, j, color, &ki, &kj)) {
    if (komaster == other
	&& i == kom_i
	&& j == kom_j)
      return 0;
  }

  if (trymove(i, j, color, message, si, sj, komaster, kom_i, kom_j))
    return 1;

  /* Conditional ko captures are only allowed if the komaster is EMPTY. */
  if (!consider_conditional_ko || komaster != EMPTY)
    return 0;

  if (tryko(i, j, color, message, komaster, kom_i, kom_j)) {
    /* Conditional ko capture, set komaster parameters. */
    *new_komaster = color;
    *new_kom_i = ki;
    *new_kom_j = kj;
    *is_conditional_ko = 1;
    return 1;
  }
  
  /* If we come here, the move was a suicide. */
  return 0;
}

#endif



#if KOMASTER_SCHEME == 3

/* Slightly more complex komaster scheme.
 *
 * (III) Revised 2.7.232 version, O to move.
 * 
 * 1. Komaster is EMPTY.
 * 1a) Unconditional ko capture is allowed. Komaster remains EMPTY.
 * 1b) Conditional ko capture is allowed. Komaster is set to O and
 *     (kom_i, kom_j) to the location of the ko, where a stone was
 *     just removed.
 * 
 * 2. Komaster is O:
 *    Ko capture (both kinds) is allowed only if after playing the move,
 *    is_ko(kom_i, kom_j, X) returns false. In that case, (kom_i, kom_j)
 *    is updated to the new ko position, i.e. the stone captured by this
 *    move. 
 * 
 * 3. Komaster is X:
 * 3a) Conditional ko capture is not allowed.
 * 3b) Unconditional ko capture is allowed except for a move at (kom_i,
 *     kom_j). Komaster parameters unchanged.
 * 
 * 4. Komaster is GRAY:
 *    Doesn't happen.
 * 
 */
int
komaster_trymove(int i, int j, int color,
		 const char *message, int si, int sj,
		 int komaster, int kom_i, int kom_j,
		 int *new_komaster, int *new_kom_i, int *new_kom_j,
		 int *is_conditional_ko, int consider_conditional_ko)
{
  int other = OTHER_COLOR(color);
  int ko_move;
  int ki, kj;

  /* Usually the komaster parameters are unchanged. */
  *new_komaster = komaster;
  *new_kom_i = kom_i;
  *new_kom_j = kom_j;

  *is_conditional_ko = 0;

  /* If opponent is komaster we may not capture his ko. */
  ko_move = is_ko(i, j, color, &ki, &kj);
  if (ko_move
      && komaster == other
      && i == kom_i
      && j == kom_j)
    return 0;

  if (!trymove(i, j, color, message, si, sj, komaster, kom_i, kom_j)) {
    /* Conditional ko captures are allowed if komaster is EMPTY or our
     * color.
     */
    if (!consider_conditional_ko || komaster == other)
      return 0;

    if (!tryko(i, j, color, message, komaster, kom_i, kom_j))
      return 0; /* Suicide. */
      
    *is_conditional_ko = 1;

    /* Conditional ko capture, set komaster parameters. */
    if (komaster == EMPTY) {
      *new_komaster = color;
      *new_kom_i = ki;
      *new_kom_j = kj;
    }
  }

  if (!ko_move)
    return 1;

  /* Remains to check that if we are komaster, the old ko is gone. */
  if (komaster != color)
    return 1;
  
  if (!is_ko(kom_i, kom_j, other, NULL, NULL)) {
    *new_kom_i = ki;
    *new_kom_j = kj;
    return 1;
  }

  /* The old ko was still around, move not accepted. */
  popgo();
  return 0;
}

#endif


/* Count the number of liberties of the string at (m, n). (m, n) must
 * not be empty.
 */

int
countlib(int m, int n)
{
  ASSERT_ON_BOARD(m, n);
  ASSERT(p[m][n] != EMPTY, m, n);
  
  if (!strings_initialized)
    init_board();

  /* We already know the number of liberties. Just look it up. */
  return string[string_number[m][n]].liberties;
}


/* Find the liberties of the string at (m, n). (m, n) must not be
 * empty. The locations of up to maxlib liberties are written into
 * (libi[], libj[]). The full number of liberties is returned.
 *
 * If you want the locations of all liberties, whatever their number,
 * you should pass MAXLIBS as the value for maxlib and allocate space
 * for libi[], libj[] accordingly.
 */

int
findlib(int m, int n, int maxlib, int *libi, int *libj)
{
  int k;
  int libs;
  int s;
  
  ASSERT_ON_BOARD(m, n);
  ASSERT(p[m][n] != EMPTY, m, n);
  ASSERT(libi != NULL && libj != NULL, m, n);
  
  if (!strings_initialized)
    init_board();

  /* We already have the list of liberties and only need to copy it to
   * (libi[], libj[]).
   *
   * However, if the string has more than MAX_LIBERTIES liberties the
   * list is truncated and if maxlib is also larger than MAX_LIBERTIES
   * we have to traverse the stones in the string in order to find
   * where the liberties are.
   */
  s = string_number[m][n];
  libs = string[s].liberties;
  if (libs <= MAX_LIBERTIES || maxlib <= MAX_LIBERTIES) {
    /* The easy case, it suffices to copy liberty locations from the
     * incrementally updated list.
     */
    for (k = 0; k < maxlib && k < libs; k++) {
      libi[k] = string[s].libi[k];
      libj[k] = string[s].libj[k];
    }
  }
  else {
    /* The harder case, where we have to traverse the stones in the
     * string. We don't have to check explicitly if we are back to
     * the start of the chain since we will run out of liberties
     * before that happens.
     */
    int i, j;
    liberty_mark++;
    for (k = 0, FIRST_STONE(s, i, j); k < maxlib && k < libs; ) {
      if (i > 0 && UNMARKED_LIBERTY(i-1, j)) {
	libi[k] = i-1;
	libj[k++] = j;
	MARK_LIBERTY(i-1, j);
	if (k >= maxlib)
	  break;
      }
      
      if (i < board_size-1 && UNMARKED_LIBERTY(i+1, j)) {
	libi[k] = i+1;
	libj[k++] = j;
	MARK_LIBERTY(i+1, j);
	if (k >= maxlib)
	  break;
      }
      
      if (j > 0 && UNMARKED_LIBERTY(i, j-1)) {
	libi[k] = i;
	libj[k++] = j-1;
	MARK_LIBERTY(i, j-1);
	if (k >= maxlib)
	  break;
      }
      
      if (j < board_size-1 && UNMARKED_LIBERTY(i, j+1)) {
	libi[k] = i;
	libj[k++] = j+1;
	MARK_LIBERTY(i, j+1);
	if (k >= maxlib)
	  break;
      }
      /* We can't use this macro within the for statement, so we have
       * to put it at the end of the loop instead.
       */
      NEXT_STONE(i, j);
      ASSERT(k == libs || !BACK_TO_FIRST_STONE(s, i, j), i, j);
    }
  }
      
  return libs;
}


/* Find the liberties a stone of the given color would get if played
 * at (m, n), ignoring possible captures of opponent stones. (m, n)
 * must be empty. If libi!=NULL, the locations of up to maxlib
 * liberties are written into (libi[], libj[]). The counting of
 * liberties may or may not be halted when maxlib is reached. The
 * number of liberties found is returned.
 *
 * If you want the number or the locations of all liberties, however
 * many they are, you should pass MAXLIBS as the value for maxlib and
 * allocate space for libi[], libj[] accordingly.
 */

int
approxlib(int m, int n, int color, int maxlib, int *libi, int *libj)
{
  int k;
  int libs = 0;

  ASSERT_ON_BOARD(m, n);
  ASSERT(p[m][n] == EMPTY, m,n);
  ASSERT(color != EMPTY, m, n);

  /* Either both NULL or neither NULL. */
  ASSERT(((libi != NULL) ^ (libj != NULL)) == 0, m, n);
  
  if (!strings_initialized)
    init_board();

  /* Look for empty neighbors and the liberties of the adjacent
   * strings of the given color. The algorithm below won't work
   * correctly if any of the adjacent strings have more than
   * MAX_LIBERTIES liberties AND maxlib is larger than MAX_LIBERTIES.
   * If this might be the case, we use a more robust fallback.
   */
  if (maxlib > MAX_LIBERTIES)
    return slow_approxlib(m, n, color, maxlib, libi, libj);
  
  /* Start by marking (m, n) itself so it isn't counted among its own
   * liberties.
   */
  liberty_mark++;
  MARK_LIBERTY(m, n);
    
  if (m > 0) {
    if (UNMARKED_LIBERTY(m-1, n)) {
      if (libi != NULL && libs < maxlib) {
	libi[libs] = m-1;
	libj[libs] = n;
      }
      libs++;
      /* Stop counting if we reach maxlib. */
      if (libs >= maxlib)
	return libs;
      MARK_LIBERTY(m-1, n);
    }
    else if (p[m-1][n] == color) {
      int s = string_number[m-1][n];
      for (k = 0; k < string[s].liberties; k++) {
	int ai = string[s].libi[k];
	int aj = string[s].libj[k];
	if (UNMARKED_LIBERTY(ai, aj)) {
	  if (libi != NULL && libs < maxlib) {
	    libi[libs] = ai;
	    libj[libs] = aj;
	  }
	  libs++;
	  if (libs >= maxlib)
	    return libs;
	  MARK_LIBERTY(ai, aj);
	}
      }
    }
  }
  
  if (m < board_size-1) {
    if (UNMARKED_LIBERTY(m+1, n)) {
      if (libi != NULL && libs < maxlib) {
	libi[libs] = m+1;
	libj[libs] = n;
      }
      libs++;
      if (libs >= maxlib)
	return libs;
      MARK_LIBERTY(m+1, n);
    }
    else if (p[m+1][n] == color) {
      int s = string_number[m+1][n];
      for (k = 0; k < string[s].liberties; k++) {
	int ai = string[s].libi[k];
	int aj = string[s].libj[k];
	if (UNMARKED_LIBERTY(ai, aj)) {
	  if (libi != NULL && libs < maxlib) {
	    libi[libs] = ai;
	    libj[libs] = aj;
	  }
	  libs++;
	  if (libs >= maxlib)
	    return libs;
	  MARK_LIBERTY(ai, aj);
	}
      }
    }
  }
  
  if (n > 0) {
    if (UNMARKED_LIBERTY(m, n-1)) {
      if (libi != NULL && libs < maxlib) {
	libi[libs] = m;
	libj[libs] = n-1;
      }
      libs++;
      if (libs >= maxlib)
	return libs;
      MARK_LIBERTY(m, n-1);
    }
    else if (p[m][n-1] == color) {
      int s = string_number[m][n-1];
      for (k = 0; k < string[s].liberties; k++) {
	int ai = string[s].libi[k];
	int aj = string[s].libj[k];
	if (UNMARKED_LIBERTY(ai, aj)) {
	  if (libi != NULL && libs < maxlib) {
	    libi[libs] = ai;
	    libj[libs] = aj;
	  }
	  libs++;
	  if (libs >= maxlib)
	    return libs;
	  MARK_LIBERTY(ai, aj);
	}
      }
    }
  }
  
  if (n < board_size-1) {
    if (UNMARKED_LIBERTY(m, n+1)) {
      if (libi != NULL && libs < maxlib) {
	libi[libs] = m;
	libj[libs] = n+1;
      }
      libs++;
      if (libs >= maxlib)
	return libs;
      /* Unneeded since we're about to leave. */
#if 0
      MARK_LIBERTY(m, n+1);
#endif
    }
    else if (p[m][n+1] == color) {
      int s = string_number[m][n+1];
      for (k = 0; k < string[s].liberties; k++) {
	int ai = string[s].libi[k];
	int aj = string[s].libj[k];
	if (UNMARKED_LIBERTY(ai, aj)) {
	  if (libi != NULL && libs < maxlib) {
	    libi[libs] = ai;
	    libj[libs] = aj;
	  }
	  libs++;
	  if (libs >= maxlib)
	    return libs;
	  MARK_LIBERTY(ai, aj);
	}
      }
    }
  }

  return libs;
}


/*
 * Report the number of stones in a string.
 */

int
countstones(int m, int n)
{
  ASSERT_ON_BOARD(m, n);
  ASSERT(p[m][n] != EMPTY, m, n);

  if (!strings_initialized)
    init_board();

  return string[string_number[m][n]].size;
}


/* Find the stones of the string at (m, n). (m, n) must not be
 * empty. The locations of up to maxstones stones are written into
 * (stonei[], stonej[]). The full number of stones is returned.
 */

int
findstones(int m, int n, int maxstones, int *stonei, int *stonej)
{
  int s;
  int size;
  int i, j;
  int k;
  
  ASSERT_ON_BOARD(m, n);
  ASSERT(p[m][n] != EMPTY, m, n);

  if (!strings_initialized)
    init_board();

  s = string_number[m][n];
  size = string[s].size;
  
  /* Traverse the stones of the string, by following the cyclic chain. */
  FIRST_STONE(s, i, j);
  for (k = 0; k < maxstones && k < size; k++) {
    stonei[k] = i;
    stonej[k] = j;
    NEXT_STONE(i, j);
  }

  return size;
}


/* chainlinks returns (in adji, adjj arrays) the chains surrounding
 * the string at (m, n). The number of chains is returned.
 */

int 
chainlinks(int m, int n, int adji[MAXCHAIN], int adjj[MAXCHAIN])
{
  struct string_data *s, *t;
  int k;

  ASSERT(p[m][n] != EMPTY, m, n);

  if (!strings_initialized)
    init_board();

  /* We already have the list ready, just copy it and fill in the
   * desired information.
   */
  s = &string[string_number[m][n]];
  for (k = 0; k < s->neighbors; k++) {
    t = &string[s->neighborlist[k]];
    adji[k] = t->origini;
    adjj[k] = t->originj;
  }
  return s->neighbors;
}


/* chainlinks2 returns (in adji, adjj arrays) the chains surrounding
 * the string at (m, n), which have exactly lib liberties. The number
 * of such chains is returned.
 */

int
chainlinks2(int m, int n, int adji[MAXCHAIN], int adjj[MAXCHAIN], int lib)
{
  struct string_data *s, *t;
  int k;
  int adj;

  ASSERT(p[m][n] != EMPTY, m, n);

  if (!strings_initialized)
    init_board();

  /* We already have the list ready, just copy the strings with the
   * right number of liberties.
   */
  adj = 0;
  s = &string[string_number[m][n]];
  for (k = 0; k < s->neighbors; k++) {
    t = &string[s->neighborlist[k]];
    if (t->liberties == lib) {
      adji[adj] = t->origini;
      adjj[adj] = t->originj;
      adj++;
    }
  }
  return adj;
}


/*
 * Find the origin of a worm or a cavity, i.e. the point with smallest
 * i coordinate and in the case of a tie with smallest j coordinate.
 * The idea is to have a canonical reference point for a string.
 */

void
find_origin(int m, int n, int *origini, int *originj)
{
  struct string_data *s;

  gg_assert(p[m][n] != EMPTY);

  if (!strings_initialized)
    init_board();
  
  s = &string[string_number[m][n]];
  *origini = s->origini;
  *originj = s->originj;
}


/* Determine whether a move by color at (m, n) would be a self atari,
 * i.e. whether it would get more than one liberty. This function
 * returns true also for the case of a suicide move.
 */

int
is_self_atari(int m, int n, int color)
{
  int liberties;
  int result;
  
  ASSERT_ON_BOARD(m, n);
  ASSERT(p[m][n] == EMPTY, m, n);
  ASSERT(color != EMPTY, m, n);

  /* 1. Try first without really putting the stone on the board. */
  /* FIXME: Integrate incremental_sloppy_self_atari() here. */
  result = incremental_sloppy_self_atari(m, n, color);
  if (result != -1)
    return result;

  /* 2. It was not so easy.  Now see if we can put the stone on the board.
   *    If we can't, this is a self atari.*/
  if (!do_trymove(m, n, color, 1))
    return 1;
  liberties = countlib(m, n);
  silent_popgo();
  
  return liberties <= 1;
}


/*
 * Returns true if (ai, aj) is a liberty of the string at (si, sj).
 */

int
liberty_of_string(int ai, int aj, int si, int sj)
{
  if (p[ai][aj] != EMPTY)
    return 0;

  return neighbor_of_string(ai, aj, si, sj);
}


/*
 * Returns true if (ai, aj) is adjacent to the string at (si, sj).
 */

int
neighbor_of_string(int ai, int aj, int si, int sj)
{
  int s;

  gg_assert(p[si][sj] != EMPTY);

  if (!strings_initialized)
    init_board();

  s = string_number[si][sj];
  
  if (ai > 0
      && p[ai-1][aj] == p[si][sj]
      && string_number[ai-1][aj] == s)
    return 1;

  if (ai < board_size-1
      && p[ai+1][aj] == p[si][sj]
      && string_number[ai+1][aj] == s)
    return 1;

  if (aj > 0
      && p[ai][aj-1] == p[si][sj]
      && string_number[ai][aj-1] == s)
    return 1;

  if (aj < board_size-1
      && p[ai][aj+1] == p[si][sj]
      && string_number[ai][aj+1] == s)
    return 1;
  
  return 0;
}


/*
 * Returns true if (ai, aj) and (bi, bj) belong to the same string.
 */

int
same_string(int ai, int aj, int bi, int bj)
{
  ASSERT_ON_BOARD(ai, aj);
  ASSERT_ON_BOARD(bi, bj);
  ASSERT(p[ai][aj] != EMPTY, ai, aj);
  ASSERT(p[bi][bj] != EMPTY, bi, bj);
  return string_number[ai][aj] == string_number[bi][bj];
}


/*
 * Return true if the move (i,j) by (color) is a ko capture
 * (whether capture is legal on this move or not). If so,
 * and if (*ko_i,*ko_j) are not NULL pointers, then
 * (*ko_i,*ko_j) returns the location of the captured ko stone.
 * If the move is not a ko capture, (ko_i,ko_j) is set to (-1,-1).
 *
 * A move is a ko capture if and only if
 *    1. All neighbors are opponent stones.
 *    2. The number of captured stones is exactly one.
 */

int
is_ko(int m, int n, int color, int *ko_i, int *ko_j)
{
  int other = OTHER_COLOR(color);
  int captures = 0;
  int ki = -1, kj = -1;
  
  ASSERT_ON_BOARD(m, n);
  ASSERT(color == WHITE || color == BLACK, m, n);

  if (!strings_initialized)
    init_board();
  
  if (m > 0) {
    if (p[m-1][n] != other)
      return 0;
    else if (LIBERTIES(m-1, n) == 1) {
      ki = m-1;
      kj = n;
      captures += string[string_number[m-1][n]].size;
      if (captures > 1)
	return 0;
    }
  }
  
  if (m < board_size-1) {
    if (p[m+1][n] != other)
      return 0;
    else if (LIBERTIES(m+1, n) == 1) {
      ki = m+1;
      kj = n;
      captures += string[string_number[m+1][n]].size;
      if (captures > 1)
	return 0;
    }
  }
  
  if (n > 0) {
    if (p[m][n-1] != other)
      return 0;
    else if (LIBERTIES(m, n-1) == 1) {
      ki = m;
      kj = n-1;
      captures += string[string_number[m][n-1]].size;
      if (captures > 1)
	return 0;
    }
  }
  
  if (n < board_size-1) {
    if (p[m][n+1] != other)
      return 0;
    else if (LIBERTIES(m, n+1) == 1) {
      ki = m;
      kj = n+1;
      captures += string[string_number[m][n+1]].size;
      if (captures > 1)
	return 0;
    }
  }

  if (captures == 1) {
    if (ko_i) *ko_i = ki;
    if (ko_j) *ko_j = kj;
    return 1;
  }
  return 0;
}


/* Returns 1 if at least one string is captured when color plays at (m, n).
 */
int
does_capture_something(int m, int n, int color)
{
  int other = OTHER_COLOR(color);

  gg_assert(p[m][n] == EMPTY);

  if (!strings_initialized)
    init_board();

  if (m > 0              && p[m-1][n] == other && LIBERTIES(m-1, n) == 1)
    return 1;
  
  if (m < board_size - 1 && p[m+1][n] == other && LIBERTIES(m+1, n) == 1)
    return 1;
  
  if (n > 0              && p[m][n-1] == other && LIBERTIES(m, n-1) == 1)
    return 1;
  
  if (n < board_size - 1 && p[m][n+1] == other && LIBERTIES(m, n+1) == 1)
    return 1;

  return 0;
}


/* For each stone in the string at (i, j), set mx to value mark. If
 * some of the stones in the string are marked prior to calling this
 * function, only the connected unmarked stones starting from (i, j)
 * are guaranteed to become marked. The rest of the string may or may
 * not become marked. (In the current implementation, it will.)
 */
void
mark_string(int m, int n, char mx[MAX_BOARD][MAX_BOARD], char mark)
{
  int i = m;
  int j = n;

  gg_assert(p[m][n] != EMPTY);

  do {
    mx[i][j] = mark;
    NEXT_STONE(i, j);
  } while (i != m || j != n);
}


/* Returns true if at least one move has been played at @code{(m, n)}
 * at deeper than level 'cutoff' in the reading tree.
 */
int
move_in_stack(int m, int n, int cutoff)
{
  int k;
  for (k=cutoff; k<stackp; k++)
    if (stacki[k] == m && stackj[k] == n)
      return 1;
  
  return 0;
}


/* Retrieve a move from the move stack. */
void
get_move_from_stack(int k, int *i, int *j, int *color)
{
  gg_assert(k < stackp);
  *i = stacki[k];
  *j = stackj[k];
  *color = move_color[k];
}

/* Return the number of stones of the indicated color(s) on the board.
 * This only count stones in the permanent position, not stones placed
 * by trymove() or tryko(). Use stones_on_board(BLACK | WHITE) to get
 * the total number of stones on the board.
 */
int
stones_on_board(int color)
{
  static int stone_count_for_position = -1;
  static int white_stones = 0;
  static int black_stones = 0;
  
  if (stone_count_for_position != position_number) {
    int m, n;
    white_stones = 0;
    black_stones = 0;
    for (m = 0; m < board_size; m++)
      for (n = 0; n < board_size; n++) {
	if (p[m][n] == WHITE)
	  white_stones++;
	else if (p[m][n] == BLACK)
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


/* Don't trust the incremental string data until it's reinitialized.
 *
 * This function should be called if the board is modified by other
 * means than do_play_move() or undo_move().
 * It's also useful to force a recomputation of the strings if we
 * don't have any immediate plans to undo the move, because it recovers
 * undo stack space and holes in the 'string' array.
 */

/* We have reached a new position. Increase the position counter and
 * invalidate the incremental strings.
 */

static void
new_position(void)
{
  position_number++;
  strings_initialized = 0;
}


/* Set up incremental board structures and populate them with the
 * strings available in the position given by p[][]. Clear the stacks
 * and start the mark numbers from zero. All undo information is lost
 * by calling this function.
 */

static void
init_board()
{
  int i, j, s;
  next_string = 0;
  liberty_mark = 0;
  string_mark = 0;
  CLEAR_STACKS();

  memset(string, 0, sizeof(string));
  memset(ml, 0, sizeof(ml));

  /* propagate_string relies on non-assigned stones to have
   * string_number -1.
   */
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++)
      string_number[i][j] = -1;

  /* Find the existing strings. */
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++)
      if (p[i][j] != EMPTY && string_number[i][j] == -1) {
	string_number[i][j] = next_string;
	string[next_string].size = propagate_string(i, j, i, j);
	string[next_string].color = p[i][j];
	string[next_string].origini = i;
	string[next_string].originj = j;
	string[next_string].mark = 0;
	next_string++;
      }
  
  /* Fill in liberty and neighbor info. */
  for (s = 0; s < next_string; s++) {
    find_liberties_and_neighbors(s);
  }

  /* Now we can trust the information. */
  strings_initialized = 1;
}


#if 0

/*
 * Debug function. Dump all string information.
 */

static void
dump_incremental_board(void)
{
  int i, j, s;
  
  for (i = 0; i < board_size; i++) {
    for (j = 0; j < board_size; j++)
      if (p[i][j] == EMPTY)
	fprintf(stderr, " . ");
      else
	fprintf(stderr, "%2d ", string_number[i][j]);
    fprintf(stderr, "\n");
  }

  for (s = 0; s < next_string; s++) {
    if (p[string[s].origini][string[s].originj] == EMPTY)
      continue;
    
    gprintf("%o%d %s %m size %d, %d liberties, %d neighbors\n", s,
	    color_to_string(string[s].color),
	    string[s].origini, string[s].originj, string[s].size,
	    string[s].liberties, string[s].neighbors);
    gprintf("%ostones:");

    FIRST_STONE(s, i, j);
    do {
      gprintf("%o %m", i, j);
      NEXT_STONE(i, j);
    } while (!BACK_TO_FIRST_STONE(s, i, j));
    
    gprintf("%o\nliberties:");
    for (i = 0; i < string[s].liberties; i++)
      gprintf("%o %m", string[s].libi[i], string[s].libj[i]);
    
    gprintf("%o\nneighbors:");
    for (i = 0; i < string[s].neighbors; i++)
      gprintf("%o %d(%m)", string[s].neighborlist[i],
	      string[string[s].neighborlist[i]].origini,
	      string[string[s].neighborlist[i]].originj);
    gprintf("%o\n\n");
  }
}
#endif


/* Build a string and its cyclic list representation from scratch.
 * propagate_string(i, j, m, n) adds the stone (i, j) to the string
 * (m, n) and recursively continues with not already included friendly
 * neighbors. To start a new string at (i, j), use
 * propagate_string(i, j, i, j). The size of the string is returned.
 */

static int
propagate_string(int i, int j, int m, int n)
{
  int size = 1;
  int k;
  
  if (i == m && j == n) {
    /* Start a new string. */
    next_stonei[i][j] = i;
    next_stonej[i][j] = j;
  }
  else {
    /* Link the stone at (i, j) to the string including (m, n) */
    string_number[i][j] = string_number[m][n];
    next_stonei[i][j] = next_stonei[m][n];
    next_stonej[i][j] = next_stonej[m][n];
    next_stonei[m][n] = i;
    next_stonej[m][n] = j;
  }

  /* Look in all four directions for more stones to add. */
  for (k = 0; k < 4; k++) {
    int di = deltai[k];
    int dj = deltaj[k];
    if (ON_BOARD(i+di, j+dj)
	&& p[i+di][j+dj] == p[i][j]
	&& string_number[i+di][j+dj] == -1)
      size += propagate_string(i+di, j+dj, m, n);
  }
  
  return size;
}


/* Build the lists of liberties and neighbors of a string from
 * scratch. No information is pushed onto the stack by this function.
 */

static void
find_liberties_and_neighbors(int s)
{
  int i, j;

  /* Clear the marks. */
  liberty_mark++;
  string_mark++;

  /* Traverse the stones of the string, by following the cyclic chain. */
  FIRST_STONE(s, i, j);
  do {
    /* Look in each direction for new liberties or new neighbors. Mark
     * already visited liberties and neighbors.
     */
    if (i > 0) {
      if (UNMARKED_LIBERTY(i-1, j)) {
	ADD_AND_MARK_LIBERTY(s, i-1, j);
      }
      else if (UNMARKED_OPPONENT_STRING(s, i-1, j)) {
	ADD_NEIGHBOR(s, i-1, j);
	MARK_STRING(i-1, j);
      }
    }
    
    if (i < board_size-1) {
      if (UNMARKED_LIBERTY(i+1, j)) {
	ADD_AND_MARK_LIBERTY(s, i+1, j);
      }
      else if (UNMARKED_OPPONENT_STRING(s, i+1, j)) {
	ADD_NEIGHBOR(s, i+1, j);
	MARK_STRING(i+1, j);
      }
    }
    
    if (j > 0) {
      if (UNMARKED_LIBERTY(i, j-1)) {
	ADD_AND_MARK_LIBERTY(s, i, j-1);
      }
      else if (UNMARKED_OPPONENT_STRING(s, i, j-1)) {
	ADD_NEIGHBOR(s, i, j-1);
	MARK_STRING(i, j-1);
      }
    }
    
    if (j < board_size-1) {
      if (UNMARKED_LIBERTY(i, j+1)) {
	ADD_AND_MARK_LIBERTY(s, i, j+1);
      }
      else if (UNMARKED_OPPONENT_STRING(s, i, j+1)) {
	ADD_NEIGHBOR(s, i, j+1);
	MARK_STRING(i, j+1);
      }
    }

    NEXT_STONE(i, j);
  } while (!BACK_TO_FIRST_STONE(s, i, j));
}


/* Update the liberties of a string from scratch, first pushing the
 * old information.
 */

static void
update_liberties(int s)
{
  int i, j, k;

  /* Push the old information. */
  PUSH_VALUE(string[s].liberties);
  for (k = 0; k < string[s].liberties && k < MAX_LIBERTIES; k++) {
    PUSH_VALUE(string[s].libi[k]);
    PUSH_VALUE(string[s].libj[k]);
  }
  string[s].liberties = 0;

  /* Clear the liberty mark. */
  liberty_mark++;

  /* Traverse the stones of the string, by following the cyclic chain. */
  FIRST_STONE(s, i, j);
  do {
    /* Look in each direction for new liberties. Mark already visited
     * liberties. 
     */
    if (i > 0 && UNMARKED_LIBERTY(i-1, j)) {
      ADD_AND_MARK_LIBERTY(s, i-1, j);
    }
    
    if (i < board_size-1 && UNMARKED_LIBERTY(i+1, j)) {
      ADD_AND_MARK_LIBERTY(s, i+1, j);
    }
    
    if (j > 0 && UNMARKED_LIBERTY(i, j-1)) {
      ADD_AND_MARK_LIBERTY(s, i, j-1);
    }
    
    if (j < board_size-1 && UNMARKED_LIBERTY(i, j+1)) {
      ADD_AND_MARK_LIBERTY(s, i, j+1);
    }

    NEXT_STONE(i, j);
  } while (!BACK_TO_FIRST_STONE(s, i, j));
}


/* Remove a string from the list of neighbors and push the changed
 * information.
 */

static void
remove_neighbor(int str, int n)
{
  int k;
  int done = 0;
  struct string_data *s = &string[str];
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
remove_liberty(int str, int i, int j)
{
  int k;
  struct string_data *s = &string[str];
  
  if (s->liberties > MAX_LIBERTIES)
    update_liberties(str);
  else {
    for (k = 0; k < s->liberties; k++)
      if (s->libi[k] == i && s->libj[k] == j) {
	/* We need to push the last entry too because it may become
	 * destroyed later.
	 */
	PUSH_VALUE(s->libi[s->liberties - 1]);
	PUSH_VALUE(s->libj[s->liberties - 1]);
	PUSH_VALUE(s->libi[k]);
	PUSH_VALUE(s->libj[k]);
	PUSH_VALUE(s->liberties);
	s->libi[k] = s->libi[s->liberties - 1];
	s->libj[k] = s->libj[s->liberties - 1];
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
  int i, j, k;

  /* Traverse the stones of the string, by following the cyclic chain. */
  FIRST_STONE(s, i, j);
  do {
    /* Push color, string number and cyclic chain pointers. */
    PUSH_VALUE(string_number[i][j]);
    PUSH_VALUE(next_stonei[i][j]);
    PUSH_VALUE(next_stonej[i][j]);
    DO_REMOVE_STONE(i, j);
    NEXT_STONE(i, j);
  } while (!BACK_TO_FIRST_STONE(s, i, j));

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
create_new_string(int i, int j)
{
  int s;
  int color = p[i][j];

  /* Get the next free string number. */
  PUSH_VALUE(next_string);
  s = next_string++;
  string_number[i][j] = s;
  /* Set up a size one cycle for the string. */
  next_stonei[i][j] = i;
  next_stonej[i][j] = j;

  /* Set trivially known values and initialize the rest to zero. */
  string[s].color = color;
  string[s].size = 1;
  string[s].origini = i;
  string[s].originj = j;
  string[s].liberties = 0;
  string[s].neighbors = 0;
  string[s].mark = 0;

  /* Clear the string mark. */
  string_mark++;

  /* In each direction, look for a liberty or a nonmarked opponent
   * neighbor. Mark visited neighbors. There is no need to mark the
   * liberties since we can't find them twice. */
  if (i > 0) {
    if (LIBERTY(i-1, j)) {
      ADD_LIBERTY(s, i-1, j);
    }
    else if (UNMARKED_OPPONENT_STRING(s, i-1, j)) {
      int s2 = string_number[i-1][j];
      /* Add the neighbor to our list. */
      ADD_NEIGHBOR(s, i-1, j);
      /* Add us to our neighbor's list. */
      PUSH_VALUE(string[s2].neighbors);
      ADD_NEIGHBOR(s2, i, j);
      MARK_STRING(i-1, j);
    }
  }
  
  if (i < board_size-1) {
    if (LIBERTY(i+1, j)) {
      ADD_LIBERTY(s, i+1, j);
    }
    else if (UNMARKED_OPPONENT_STRING(s, i+1, j)) {
      int s2 = string_number[i+1][j];
      ADD_NEIGHBOR(s, i+1, j);
      PUSH_VALUE(string[s2].neighbors);
      ADD_NEIGHBOR(s2, i, j);
      MARK_STRING(i+1, j);
    }
  }
  
  if (j > 0) {
    if (LIBERTY(i, j-1)) {
      ADD_LIBERTY(s, i, j-1);
    }
    else if (UNMARKED_OPPONENT_STRING(s, i, j-1)) {
      int s2 = string_number[i][j-1];
      ADD_NEIGHBOR(s, i, j-1);
      PUSH_VALUE(string[s2].neighbors);
      ADD_NEIGHBOR(s2, i, j);
      MARK_STRING(i, j-1);
    }
  }
  
  if (j < board_size-1) {
    if (LIBERTY(i, j+1)) {
      ADD_LIBERTY(s, i, j+1);
    }
    else if (UNMARKED_OPPONENT_STRING(s, i, j+1)) {
      int s2 = string_number[i][j+1];
      ADD_NEIGHBOR(s, i, j+1);
      PUSH_VALUE(string[s2].neighbors);
      ADD_NEIGHBOR(s2, i, j);
      /* No need to mark since no visits left. */
/*      MARK_STRING(i, j+1);*/
    }
  }
}


/* We have played a stone with exactly one friendly neighbor. Add the
 * new stone to that string.
 */
static void
extend_neighbor_string(int i, int j, int s)
{
  int k;
  int liberties_updated = 0;

  /* Link in the stone in the cyclic list. */
  int m = string[s].origini;
  int n = string[s].originj;
  next_stonei[i][j] = next_stonei[m][n];
  next_stonej[i][j] = next_stonej[m][n];
  PUSH_VALUE(next_stonei[m][n]);
  PUSH_VALUE(next_stonej[m][n]);
  next_stonei[m][n] = i;
  next_stonej[m][n] = j;
  
  /* Do we need to update the origin? */
  if (i < m || (i == m && j < n)) {
    PUSH_VALUE(string[s].origini);
    PUSH_VALUE(string[s].originj);
    string[s].origini = i;
    string[s].originj = j;
  }
  
  string_number[i][j] = s;

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
    remove_liberty(s, i, j);
  }

  /* Mark old neighbors of the string. */
  string_mark++;
  for (k = 0; k < string[s].neighbors; k++)
    string[string[s].neighborlist[k]].mark = string_mark;

  /* Look at the neighbor locations of (i, j) for new liberties and/or
   * neighbor strings.
   */
  if (i > 0) {
    /* If we find a liberty, look two steps away to determine whether
     * this already is a liberty of s.
     */
    if (LIBERTY(i-1, j)) {
      if (!liberties_updated
	  && !((i > 1 && STRING_AT_VERTEX(i-2, j, s))
	       || (j > 0 && STRING_AT_VERTEX(i-1, j-1, s))
	       || (j < board_size-1 && STRING_AT_VERTEX(i-1, j+1, s))))
	ADD_LIBERTY(s, i-1, j);
    }
    else if (UNMARKED_OPPONENT_STRING(s, i-1, j)) {
      int s2 = string_number[i-1][j];
      PUSH_VALUE(string[s].neighbors);
      ADD_NEIGHBOR(s, i-1, j);
      PUSH_VALUE(string[s2].neighbors);
      ADD_NEIGHBOR(s2, i, j);
      MARK_STRING(i-1, j);
    }
  }
  
  if (i < board_size-1) {
    if (LIBERTY(i+1, j)) {
      if (!liberties_updated
	  && !((i < board_size-2 && STRING_AT_VERTEX(i+2, j, s))
	       || (j > 0 && STRING_AT_VERTEX(i+1, j-1, s))
	       || (j < board_size-1 && STRING_AT_VERTEX(i+1, j+1, s))))
	ADD_LIBERTY(s, i+1, j);
    }
    else if (UNMARKED_OPPONENT_STRING(s, i+1, j)) {
      int s2 = string_number[i+1][j];
      PUSH_VALUE(string[s].neighbors);
      ADD_NEIGHBOR(s, i+1, j);
      PUSH_VALUE(string[s2].neighbors);
      ADD_NEIGHBOR(s2, i, j);
      MARK_STRING(i+1, j);
    }
  }
  
  if (j > 0) {
    if (LIBERTY(i, j-1)) {
      if (!liberties_updated
	  && !((j > 1 && STRING_AT_VERTEX(i, j-2, s))
	       || (i > 0 && STRING_AT_VERTEX(i-1, j-1, s))
	       || (i < board_size-1 && STRING_AT_VERTEX(i+1, j-1, s))))
	ADD_LIBERTY(s, i, j-1);
    }
    else if (UNMARKED_OPPONENT_STRING(s, i, j-1)) {
      int s2 = string_number[i][j-1];
      PUSH_VALUE(string[s].neighbors);
      ADD_NEIGHBOR(s, i, j-1);
      PUSH_VALUE(string[s2].neighbors);
      ADD_NEIGHBOR(s2, i, j);
      MARK_STRING(i, j-1);
    }
  }
  
  if (j < board_size-1) {
    if (LIBERTY(i, j+1)) {
      if (!liberties_updated
	  && !((j < board_size-2 && STRING_AT_VERTEX(i, j+2, s))
	       || (i > 0 && STRING_AT_VERTEX(i-1, j+1, s))
	       || (i < board_size-1 && STRING_AT_VERTEX(i+1, j+1, s))))
	ADD_LIBERTY(s, i, j+1);
    }
    else if (UNMARKED_OPPONENT_STRING(s, i, j+1)) {
      int s2 = string_number[i][j+1];
      PUSH_VALUE(string[s].neighbors);
      ADD_NEIGHBOR(s, i, j+1);
      PUSH_VALUE(string[s2].neighbors);
      ADD_NEIGHBOR(s2, i, j);
/*      MARK_STRING(i, j+1);*/
    }
  }
}


/* Incorporate the string at (i, j) with the string s.
 */

static void
assimilate_string(int s, int i, int j)
{
  int k;
  int lasti, lastj;
  int s2 = string_number[i][j];
  string[s].size += string[s2].size;

  /* Walk through the s2 stones and change string number. Also pick up
   * the last stone in the cycle for later use.
   */
  FIRST_STONE(s2, i, j);
  do {
    PUSH_VALUE(string_number[i][j]);
    string_number[i][j] = s;
    lasti = i;
    lastj = j;
    NEXT_STONE(i, j);
  } while(!BACK_TO_FIRST_STONE(s2, i, j));

  /* Link the two cycles together. */
  {
    int m = string[s].origini;
    int n = string[s].originj;
    PUSH_VALUE(next_stonei[lasti][lastj]);
    PUSH_VALUE(next_stonej[lasti][lastj]);
    PUSH_VALUE(next_stonei[m][n]);
    PUSH_VALUE(next_stonej[m][n]);
    next_stonei[lasti][lastj] = next_stonei[m][n];
    next_stonej[lasti][lastj] = next_stonej[m][n];
    next_stonei[m][n] = string[s2].origini;
    next_stonej[m][n] = string[s2].originj;
    
    /* Do we need to update the origin? */
    if (string[s2].origini < m || (string[s2].origini == m
				   && string[s2].originj < n)) {
      string[s].origini = string[s2].origini;
      string[s].originj = string[s2].originj;
    }
  }

  /* Pick up the liberties of s2 that we don't already have.
   * It is assumed that the liberties of s have been marked before
   * this function is called.
   */
  if (string[s2].liberties <= MAX_LIBERTIES) {
    for (k = 0; k < string[s2].liberties; k++) {
      int m = string[s2].libi[k];
      int n = string[s2].libj[k];
      if (UNMARKED_LIBERTY(m, n)) {
	ADD_AND_MARK_LIBERTY(s, m, n);
      }
    }
  }
  else {
    /* If s2 had too many liberties the above strategy wouldn't be
     * effective, since not all liberties are listed in
     * (libi[], libj[]). Moreover, the chain of stones for s2 is no
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


/* Create a new string for the stone at (i, j) and assimilate all
 * friendly neighbor strings.
 */

static void
assimilate_neighbor_strings(int i, int j)
{
  int s;
  int color = p[i][j];

  /* Get the next free string number. */
  PUSH_VALUE(next_string);
  s = next_string++;
  string_number[i][j] = s;
  /* Set up a size one cycle for the string. */
  next_stonei[i][j] = i;
  next_stonej[i][j] = j;
  
  /* Set trivially known values and initialize the rest to zero. */
  string[s].color = color;
  string[s].size = 1;
  string[s].origini = i;
  string[s].originj = j;
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
  if (i > 0) {
    if (UNMARKED_LIBERTY(i-1, j)) {
      ADD_AND_MARK_LIBERTY(s, i-1, j);
    }
    else if (UNMARKED_OPPONENT_STRING(s, i-1, j)) {
      ADD_NEIGHBOR(s, i-1, j);
      PUSH_VALUE(string[string_number[i-1][j]].neighbors);
      ADD_NEIGHBOR(string_number[i-1][j], i, j);
      MARK_STRING(i-1, j);
    }
    else if (UNMARKED_OWN_STRING(s, i-1, j)) {
      assimilate_string(s, i-1, j);
    }
  }

  if (i < board_size-1) {
    if (UNMARKED_LIBERTY(i+1, j)) {
      ADD_AND_MARK_LIBERTY(s, i+1, j);
    }
    else if (UNMARKED_OPPONENT_STRING(s, i+1, j)) {
      ADD_NEIGHBOR(s, i+1, j);
      PUSH_VALUE(string[string_number[i+1][j]].neighbors);
      ADD_NEIGHBOR(string_number[i+1][j], i, j);
      MARK_STRING(i+1, j);
    }
    else if (UNMARKED_OWN_STRING(s, i+1, j)) {
      assimilate_string(s, i+1, j);
    }
  }

  if (j > 0) {
    if (UNMARKED_LIBERTY(i, j-1)) {
      ADD_AND_MARK_LIBERTY(s, i, j-1);
    }
    else if (UNMARKED_OPPONENT_STRING(s, i, j-1)) {
      ADD_NEIGHBOR(s, i, j-1);
      PUSH_VALUE(string[string_number[i][j-1]].neighbors);
      ADD_NEIGHBOR(string_number[i][j-1], i, j);
      MARK_STRING(i, j-1);
    }
    else if (UNMARKED_OWN_STRING(s, i, j-1)) {
      assimilate_string(s, i, j-1);
    }
  }

  if (j < board_size-1) {
    if (UNMARKED_LIBERTY(i, j+1)) {
      ADD_AND_MARK_LIBERTY(s, i, j+1);
    }
    else if (UNMARKED_OPPONENT_STRING(s, i, j+1)) {
      ADD_NEIGHBOR(s, i, j+1);
      PUSH_VALUE(string[string_number[i][j+1]].neighbors);
      ADD_NEIGHBOR(string_number[i][j+1], i, j);
      MARK_STRING(i, j+1);
    }
    else if (UNMARKED_OWN_STRING(s, i, j+1)) {
      assimilate_string(s, i, j+1);
    }
  }
}


/* Suicide at (i, j). Remove the neighboring friendly strings.
 */

static void
do_commit_suicide(int i, int j, int color)
{
  if (i > 0 && p[i-1][j] == color)
    do_remove_string(string_number[i-1][j]);

  if (i < board_size-1 && p[i+1][j] == color)
    do_remove_string(string_number[i+1][j]);

  if (j > 0 && p[i][j-1] == color)
    do_remove_string(string_number[i][j-1]);

  if (j < board_size-1 && p[i][j+1] == color)
    do_remove_string(string_number[i][j+1]);
}


/* Play a move without legality checking. Suicide is allowed.
 */

static void
do_play_move(int i, int j, int color)
{
  int other = OTHER_COLOR(color);
  int captured_stones = 0;
  int neighbor_allies = 0;
  int have_liberties = 0;
  int s = -1;
  
  if (!strings_initialized)
    init_board();
    
  /* Remove captured stones and check for suicide.*/
  if (i > 0) {
    if (p[i-1][j] == other && LIBERTIES(i-1, j) == 1)
      captured_stones += do_remove_string(string_number[i-1][j]);
    else if (LIBERTY(i-1, j) || (p[i-1][j] == color && LIBERTIES(i-1, j) > 1))
      have_liberties = 1;
  }

  if (i < board_size-1) {
    if (p[i+1][j] == other && LIBERTIES(i+1, j) == 1)
      captured_stones += do_remove_string(string_number[i+1][j]);
    else if (LIBERTY(i+1, j) || (p[i+1][j] == color && LIBERTIES(i+1, j) > 1))
      have_liberties = 1;
  }

  if (j > 0) {
    if (p[i][j-1] == other && LIBERTIES(i, j-1) == 1)
      captured_stones += do_remove_string(string_number[i][j-1]);
    else if (LIBERTY(i, j-1) || (p[i][j-1] == color && LIBERTIES(i, j-1) > 1))
      have_liberties = 1;
  }

  if (j < board_size-1) {
    if (p[i][j+1] == other && LIBERTIES(i, j+1) == 1)
      captured_stones += do_remove_string(string_number[i][j+1]);
    else if (LIBERTY(i, j+1) || (p[i][j+1] == color && LIBERTIES(i, j+1) > 1))
      have_liberties = 1;
  }

  /* No captures and no liberties -> suicide. */
  if (have_liberties == 0 && captured_stones == 0) {
    do_commit_suicide(i, j, color);
    return;
  }
  
  /* Put down the stone. */
  DO_ADD_STONE(i, j, color);

  /* Count the number of adjacent strings of my color and remove
   * (i, j) as liberty for the adjacent opponent strings.
   */
  string_mark++;
  if (i > 0 && p[i-1][j] != EMPTY && UNMARKED_STRING(i-1, j)) {
    if (p[i-1][j] == color) {
      neighbor_allies++;
      s = string_number[i-1][j];
    }
    else
      remove_liberty(string_number[i-1][j], i, j);
    MARK_STRING(i-1, j);
  }
  
  if (i < board_size-1 && p[i+1][j] != EMPTY && UNMARKED_STRING(i+1, j)) {
    if (p[i+1][j] == color) {
      neighbor_allies++;
      s = string_number[i+1][j];
    }
    else
      remove_liberty(string_number[i+1][j], i, j);
    MARK_STRING(i+1, j);
  }
  
  if (j > 0 && p[i][j-1] != EMPTY && UNMARKED_STRING(i, j-1)) {
    if (p[i][j-1] == color) {
      neighbor_allies++;
      s = string_number[i][j-1];
    }
    else
      remove_liberty(string_number[i][j-1], i, j);
    MARK_STRING(i, j-1);
  }
  
  if (j < board_size-1 && p[i][j+1] != EMPTY && UNMARKED_STRING(i, j+1)) {
    if (p[i][j+1] == color) {
      neighbor_allies++;
      s = string_number[i][j+1];
    }
    else
      remove_liberty(string_number[i][j+1], i, j);
/*    MARK_STRING(i, j+1);*/
  }

  /* Choose strategy depending on the number of friendly neighbors. */
  if (neighbor_allies == 0)
    create_new_string(i, j);
  else if (neighbor_allies == 1) {
    gg_assert(s >= 0);
    extend_neighbor_string(i, j, s);
  }
  else
    assimilate_neighbor_strings(i, j);

  /* Check whether this move was a ko capture and if so set 
   * (board_ko_i, board_ko_j).
   *
   * No need to push (board_ko_i, board_ko_j) on the stack, 
   * because this has been done earlier.
   */
  s = string_number[i][j];
  if (string[s].liberties == 1
      && string[s].size == 1
      && captured_stones == 1) {
    board_ko_i = string[s].libi[0];
    board_ko_j = string[s].libj[0];
    hashdata_set_ko(&hashdata, board_ko_i, board_ko_j);
  }
}


/* Find the liberties a move of the given color at (i, j) would have,
 * excluding possible captures, by traversing all adjacent friendly
 * strings. This is a fallback used by approxlib() when a
 * faster algorithm can't be used.
 */

static int
slow_approxlib(int i, int j, int color, int maxlib, int *libi, int *libj)
{
  int libs = 0;
  int k;

  liberty_mark++;
  MARK_LIBERTY(i, j);
  string_mark++;
  for (k = 0; k < 4; k++) {
    int di = deltai[k];
    int dj = deltaj[k];
    if (ON_BOARD(i+di, j+dj)) {
      if (UNMARKED_LIBERTY(i+di, j+dj)) {
	if (libi) {
	  libi[libs] = i+di;
	  libj[libs] = j+dj;
	}
	libs++;
	if (libs == maxlib)
	  return libs;
	MARK_LIBERTY(i+di, j+dj);
      }
      else if (p[i+di][j+dj] == color
	       && UNMARKED_STRING(i+di, j+dj)) {
	int s = string_number[i+di][j+dj];
	int m, n;
	FIRST_STONE(s, m, n);
	do {
	  int l;
	  for (l = 0; l < 4; l++) {
	    int dm = deltai[l];
	    int dn = deltaj[l];
	    if (ON_BOARD(m+dm, n+dn) && UNMARKED_LIBERTY(m+dm, n+dn)) {
	      if (libi) {
		libi[libs] = m+dm;
		libj[libs] = n+dn;
	      }
	      libs++;
	      if (libs == maxlib)
		return libs;
	      MARK_LIBERTY(m+dm, n+dn);
	    }
	  }
	  
	  NEXT_STONE(m, n);
	} while (!BACK_TO_FIRST_STONE(s, m, n));
	MARK_STRING(i+di, j+dj);
      }
    }
  }
  return libs;
}


/* Determine whether a move by color at (m, n) might be a self atari.
 * This function is sloppy in that it only does a quick check for two
 * liberties and might miss certain cases.
 * Return value 0 means it cannot be a self atari.
 * Return value 1 means it definitely is a self atari.
 * Return value -1 means uncertain.
 */

static int
incremental_sloppy_self_atari(int m, int n, int color)
{
  /* number of empty neighbors */
  int trivial_liberties = 0;
  /* number of captured opponent strings */
  int captures = 0;
  /* Whether there is a friendly neighbor with a spare liberty. If it
   * has more than one spare liberty we immediately return 0.
   */
  int far_liberties = 0;

  /* Clear string mark. */
  string_mark++;
  
  if (m > 0) {
    if (p[m-1][n] == EMPTY)
      trivial_liberties++;
    else if (p[m-1][n] == color) {
      if (LIBERTIES(m-1, n) > 2)
	return 0;
      if (LIBERTIES(m-1, n) == 2)
	far_liberties++;
    }
    else if (LIBERTIES(m-1, n) == 1 && UNMARKED_STRING(m-1, n)) {
      captures++;
      MARK_STRING(m-1, n);
    }
  }

  if (m < board_size-1) {
    if (p[m+1][n] == EMPTY)
      trivial_liberties++;
    else if (p[m+1][n] == color) {
      if (LIBERTIES(m+1, n) > 2)
	return 0;
      if (LIBERTIES(m+1, n) == 2)
	far_liberties++;
    }
    else if (LIBERTIES(m+1, n) == 1 && UNMARKED_STRING(m+1, n)) {
      captures++;
      MARK_STRING(m+1, n);
    }
  }

  if (n > 0) {
    if (p[m][n-1] == EMPTY)
      trivial_liberties++;
    else if (p[m][n-1] == color) {
      if (LIBERTIES(m, n-1) > 2)
	return 0;
      if (LIBERTIES(m, n-1) == 2)
	far_liberties++;
    }
    else if (LIBERTIES(m, n-1) == 1 && UNMARKED_STRING(m, n-1)) {
      captures++;
      MARK_STRING(m, n-1);
    }
  }

  if (n < board_size-1) {
    if (p[m][n+1] == EMPTY)
      trivial_liberties++;
    else if (p[m][n+1] == color) {
      if (LIBERTIES(m, n+1) > 2)
	return 0;
      if (LIBERTIES(m, n+1) == 2)
	far_liberties++;
    }
    else if (LIBERTIES(m, n+1) == 1 && UNMARKED_STRING(m, n+1)) {
      captures++;
      MARK_STRING(m, n+1);
    }
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

  return -1;
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
incremental_order_moves(int mi, int mj, int color, int si, int sj,
			int *number_edges, int *number_same_string,
			int *number_own, int *number_opponent,
			int *captured_stones, int *threatened_stones,
			int *saved_stones, int *number_open)
{
#if NO_UNROLL == 1
  int i, j;
  int  k;

  /* Clear the string mark. */
  string_mark++;

  for (k = 0; k < 4; k++) {
    i = mi + deltai[k];
    j = mj + deltaj[k];
    if (!ON_BOARD(i, j))
      (*number_edges)++;
    else if (p[i][j] == EMPTY)
      (*number_open)++;
    else {
      int s = string_number[i][j];
      if (string_number[si][sj] == s)
	(*number_same_string)++;
      
      if (p[i][j] == color) {
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
	else if (string[s].liberties == 2 && UNMARKED_STRING(i, j)) {
	  (*threatened_stones) += string[s].size;
	  MARK_STRING(i, j);
	}
      }
    }
  }
  
#else
#define code1(argi,argj) \
  if (p[argi][argj] == EMPTY) \
    (*number_open)++; \
  else { \
    int s = string_number[argi][argj]; \
    if (string_number[si][sj] == s) \
      (*number_same_string)++; \
    if (p[argi][argj] == color) { \
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
      else if (string[s].liberties == 2 && UNMARKED_STRING(argi, argj)) { \
	(*threatened_stones) += string[s].size; \
        MARK_STRING(argi, argj); \
      } \
    } \
  }

  /* Clear the string mark. */
  string_mark++;

  if (mi > 0) {
    code1(mi-1, mj);
  }
  else
    (*number_edges)++;

  if (mi < board_size-1) {
    code1(mi+1, mj);
  }
  else
    (*number_edges)++;

  if (mj > 0) {
    code1(mi, mj-1);
  }
  else
    (*number_edges)++;

  if (mj < board_size-1) {
    code1(mi, mj+1);
  }
  else
    (*number_edges)++;
#endif
}


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
