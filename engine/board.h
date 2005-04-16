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

#ifndef _BOARD_H_
#define _BOARD_H_


/* This type is used to store each intersection on the board.
 * Declared before includes, because it is used in `hash.h' too.
 *
 * On a 486, char is best, since the time taken to push and pop
 * becomes significant otherwise. On other platforms, an int may be
 * better, e.g. if memcpy() is particularly fast, or if character
 * access is very slow.
 */
typedef unsigned char Intersection;


#include "hash.h"
#include "sgftree.h"
#include "winsocket.h"
#include "config.h"
#include <stdarg.h>


/* Local versions of absolute value, min and max. */
#define gg_abs(x)	((x) < 0 ? -(x) : (x))
#define gg_min(a, b)	((a)<(b) ? (a) : (b))
#define gg_max(a, b)	((a)<(b) ? (b) : (a))

/* Avoid compiler warnings with unused parameters. */
#define UNUSED(x)	((void) x)


/* ================================================================ *
 *                         One-dimensional board                    *
 * ================================================================ */

/* Board sizes. */

#define DEFAULT_BOARD_SIZE  19

#define MIN_BOARD            1     /* Minimum supported board size.   */
#define MAX_BOARD           19     /* Maximum supported board size.   */
#define MAX_HANDICAP         9     /* Maximum supported handicap.     */
#define MAX_MOVE_HISTORY   500     /* Max number of moves remembered. */


/* Colors and komaster states. */
enum Colors {
  EMPTY,
  WHITE,
  BLACK,
  GRAY,
  GRAY_WHITE,
  GRAY_BLACK,
  WEAK_KO,
  NUM_KOMASTER_STATES
};

#define COLOR_NAMES				\
  "empty",					\
  "white",					\
  "black",					\
  "gray",					\
  "gray_white",					\
  "gray_black",					\
  "weak_ko"

const char *color_to_string(int color);


#define OTHER_COLOR(color)      (WHITE + BLACK - (color))
#define IS_STONE(arg)           ((arg) == WHITE || (arg) == BLACK)


/* Various limits. */


/* A string with n stones can have at most 2(n+1) liberties. From this
 * follows that an upper bound on the number of liberties of a string
 * on a board of size N^2 is 2/3 (N^2+1).
 */
#define MAXLIBS		(2*(MAX_BOARD*MAX_BOARD + 1)/3)

/* This is a smaller, practical number of liberties that we care to
 * keep track of.
 */
#define MAX_LIBERTIES	8


/* This is an upper bound of the number of strings that can exist on
 * the board simultaneously.
 *
 * FIXME: This is not sufficiently large; above stackp == 0, the
 *	  incremental board code doesn't necessarily reuse all
 *	  indices.  This is a problem only in very pathological cases,
 *	  and is extremely unlikely to occur in practice.
 */
#define MAX_STRINGS	(2 * MAX_BOARD * MAX_BOARD / 3)

/* Per gf: unconditional_life() can get very close to filling the
 * entire board under certain circumstances. This was discussed in
 * the list around August 21, 2001, in a thread with the subject
 * "gnugo bug logs".
 */
#define MAXSTACK	(MAX_BOARD * MAX_BOARD)

/* FIXME: Do we have a justification for 160 on 19x19?  I replace this
 *	  with a MAX_BOARD-dependent value that yields 160 for
 *	  MAX_BOARD == 19.  /pp
 */
#define MAXCHAIN	(((MAX_BOARD * MAX_BOARD) * 160) / 361)


/* Board topology. */

/* Note that POS(-1, -1) == 0.  DELTA() is defined so that
 *
 *	POS(i + di, j + dj) = POS(i, j) + DELTA(di, dj).
 */
#define BOARDSIZE     ((MAX_BOARD + 2) * (MAX_BOARD + 1) + 1)
#define BOARDMIN      (MAX_BOARD + 2)
#define BOARDMAX      (MAX_BOARD + 1) * (MAX_BOARD + 1)
#define POS(i, j)     ((MAX_BOARD + 2) + (i) * (MAX_BOARD + 1) + (j))
#define DELTA(di, dj) ((di) * (MAX_BOARD + 1) + (dj))
#define I(pos)        ((pos) / (MAX_BOARD + 1) - 1)
#define J(pos)        ((pos) % (MAX_BOARD + 1) - 1)
#define PASS_MOVE     0
#define NO_MOVE       PASS_MOVE
#define NS            (MAX_BOARD + 1)
#define WE            1
#define SOUTH(pos)    ((pos) + NS)
#define WEST(pos)     ((pos) - 1)
#define NORTH(pos)    ((pos) - NS)
#define EAST(pos)     ((pos) + 1)
#define SW(pos)       ((pos) + NS - 1)
#define NW(pos)       ((pos) - NS - 1)
#define NE(pos)       ((pos) - NS + 1)
#define SE(pos)       ((pos) + NS + 1)
#define SS(pos)       ((pos) + 2 * NS)
#define WW(pos)       ((pos) - 2)
#define NN(pos)       ((pos) - 2 * NS)
#define EE(pos)       ((pos) + 2)

#define DIRECT_NEIGHBORS(pos1, pos2)		\
  ((pos1) == SOUTH(pos2)			\
   || (pos1) == WEST(pos2)			\
   || (pos1) == NORTH(pos2)			\
   || (pos1) == EAST(pos2))

#define DIAGONAL_NEIGHBORS(pos1, pos2)		\
  ((pos1) == SW(pos2)				\
   || (pos1) == NW(pos2)			\
   || (pos1) == NE(pos2)			\
   || (pos1) == SE(pos2))

#define BOARD(goban, i, j)	((goban)->board[POS(i, j)])

#define MIRROR_MOVE(goban, pos)			\
  POS((goban)->board_size - 1 - I(pos),		\
      (goban)->board_size - 1 - J(pos))


/* The `Goban' structure that wraps up the board and related
 * variables.
 */

typedef struct _Goban			Goban;

/* This data is private to `Goban' structure and is not accessible
 * from outside the `board.c'.
 */
typedef struct _Goban_private_data	Goban_private_data;


struct _Goban {
  /* The private data. */
  Goban_private_data  *private;

  int		board_size;
  int		board_ko_pos;

  int		black_captured;
  int		white_captured;
  Hash_data	board_hash;
  Intersection  board[BOARDSIZE];

  /* Used for SGF dumping of reading trees. */
  int		variations_counter;
  SGFTree      *sgf_dumptree;

  /* This is incremented everytime a move is (permanently) played or
   * the board is cleared.
   */
  int		position_number;

  /* Stack pointer. */
  int		stackp;

  /* Reading tree shadow. */
  char		shadow[BOARDMAX];

  float		komi;
  int		move_number;
  int		chinese_rules;
  int		allow_suicide;
};


/* This structure holds the internal board state.  If we were using
 * C++, we could inherit `Goban' structure from this one.
 */

typedef struct _Board_state	Board_state;

struct _Board_state {
  int		board_size;
  int		board_ko_pos;
  int		black_captured;
  int		white_captured;
  Intersection  board[BOARDSIZE];

  int		initial_board_ko_pos;
  int		initial_black_captured;
  int		initial_white_captured;
  Intersection	initial_board[BOARDSIZE];

  int		move_history_pointer;
  int		move_history_color[MAX_MOVE_HISTORY];
  int		move_history_pos[MAX_MOVE_HISTORY];

  float		komi;
  int		move_number;
};


/* ================================================================ */
/*                        board.c functions                         */
/* ================================================================ */


Goban * create_goban(int board_size);


/* Functions handling the permanent board state. */
void store_board(const Goban *goban, Board_state *state);
void restore_board(Goban *goban, const Board_state *state);

void clear_board(Goban *goban);
void clear_internal_caches(const Goban *goban);
int  test_gray_border(const Goban *goban);
void setup_board(Goban *goban, Intersection new_board[MAX_BOARD][MAX_BOARD],
		 int ko_pos, int *last, float new_komi,
		 int white_captured, int black_captured);
void add_stone(Goban *goban, int pos, int color);
void remove_stone(Goban *goban, int pos);
void play_move(Goban *goban, int pos, int color);
int  undo_moves(Goban *goban, int n);


/* Information about the permanent board. */
int  get_last_move(const Goban *goban);
int  get_last_player(const Goban *goban);
int  get_last_opponent_move(const Goban *goban, int color);
int  stones_on_board(const Goban *goban, int color);

/* Functions handling the variable board state. */
int  trymove(Goban *goban, int pos, int color, const char *message, int str);
int  tryko(Goban *goban, int pos, int color, const char *message);
void popgo(Goban *goban);
int  komaster_trymove(Goban *goban, int pos, int color,
		      const char *message, int str,
		      int *is_conditional_ko, int consider_conditional_ko);
int  get_komaster(const Goban *goban);
int  get_kom_pos(const Goban *goban);

int  move_in_stack(const Goban *goban, int pos, int cutoff);
void get_move_from_stack(const Goban *goban, int k, int *move, int *color);
void dump_stack(const Goban *goban);
void do_dump_stack(const Goban *goban);

void reset_trymove_counter(const Goban *goban);
int  get_trymove_counter(const Goban *goban);

/* move properties */
int  is_pass(int pos);
int  is_legal(const Goban *goban, int pos, int color);
int  is_suicide(const Goban *goban, int pos, int color);
int  is_illegal_ko_capture(const Goban *goban, int pos, int color);
int  is_ko(const Goban *goban, int pos, int color, int *ko_pos);
int  is_ko_point(const Goban *goban, int pos);
int  does_capture_something(const Goban *goban, int pos, int color);
int  is_self_atari(const Goban *goban, int pos, int color);

/* Purely geometric functions. */
int  is_edge_vertex(const Goban *goban, int pos);
int  is_corner_vertex(const Goban *goban, int pos);
int  edge_distance(const Goban *goban, int pos);
int  square_dist(int pos1, int pos2);

/* Basic string information. */
int  find_origin(const Goban *goban, int str);
int  chainlinks(const Goban *goban, int str, int adj[MAXCHAIN]);
int  chainlinks2(const Goban *goban, int str, int adj[MAXCHAIN], int lib);
int  chainlinks3(const Goban *goban, int str, int adj[MAXCHAIN], int lib);
int  extended_chainlinks(const Goban *goban, int str, int adj[MAXCHAIN],
			 int both_colors);

int  liberty_of_string(const Goban *goban, int pos, int str);
int  second_order_liberty_of_string(const Goban *goban, int pos, int str);
int  neighbor_of_string(const Goban *goban, int pos, int str);
int  has_neighbor(const Goban *goban, int pos, int color);
int  same_string(const Goban *goban, int str1, int str2);
int  adjacent_strings(const Goban *goban, int str1, int str2);
void mark_string(const Goban *goban, int str, char mx[BOARDMAX], char mark);
void signed_mark_string(const Goban *goban, int str,
			signed char mx[BOARDMAX], signed char mark);
int  are_neighbors(const Goban *goban, int pos1, int pos2);

/* Count and/or find liberties at (pos). */
int  countlib(const Goban *goban, int str);
int  findlib(const Goban *goban, int str, int maxlib, int *libs);
int  fastlib(const Goban *goban, int pos, int color, int ignore_captures);
int  approxlib(const Goban *goban, int pos, int color, int maxlib, int *libs);
int  accuratelib(const Goban *goban, int pos, int color,
		 int maxlib, int *libs);
int  count_common_libs(const Goban *goban, int str1, int str2);
int  find_common_libs(const Goban *goban, int str1, int str2,
		      int maxlib, int *libs);
int  have_common_lib(const Goban *goban, int str1, int str2, int *lib);

/* Count the number of stones in a string. */
int  countstones(const Goban *goban, int str);
int  findstones(const Goban *goban, int str, int maxstones, int *stones);
int  count_adjacent_stones(const Goban *goban, int str1, int str2,
			   int maxstones);

/* Special function for reading.c */
void incremental_order_moves(const Goban *goban,
			     int move, int color, int string,
			     int *number_edges, int *number_same_string,
			     int *number_own, int *number_opponent,
			     int *captured_stones, int *threatened_stones,
			     int *saved_stones, int *number_open);

/* Board caches initialization functions. */
void clear_approxlib_cache(void);
void clear_accuratelib_cache(void);


/* Is this point inside the board? */
#if 0
#define ON_BOARD2(goban, i, j)						\
  ((i) >= 0 && (j) >= 0							\
   && (i) < (goban)->board_size && (j) < (goban)->board_size)
#else
/*
 * For the case when expr can only be slightly negative,
 *    if (expr < 0 || expr > something)
 * is equivalent to
 *    if ((unsigned) expr > something)
 *
 * (I think GCC knows this trick, but it does no harm to
 * encode it explicitly since it saves typing !)
 */
#define ON_BOARD2(goban, i, j)						\
  ((unsigned) (i)    < (unsigned) (goban)->board_size			\
   && (unsigned) (j) < (unsigned) (goban)->board_size)
#endif

#define ASSERT_ON_BOARD2(goban, i, j)					\
  ASSERT2((goban), ON_BOARD2((goban), (i), (j)), (i), (j))

#define ON_BOARD1(goban, pos)						\
  (((unsigned) (pos) < BOARDSIZE) && (goban)->board[pos] != GRAY)

#define ON_BOARD(goban, pos)	((goban)->board[pos] != GRAY)

#define ASSERT_ON_BOARD1(goban, pos)					\
  ASSERT1((goban), ON_BOARD1((goban), (pos)), (pos))

/* Coordinates for the eight directions, ordered
 * south, west, north, east, southwest, northwest, northeast, southeast.
 * Defined in board.c.
 */
extern int deltai[8]; /* = { 1,  0, -1,  0,  1, -1, -1, 1}; */
extern int deltaj[8]; /* = { 0, -1,  0,  1, -1, -1,  1, 1}; */
extern int delta[8];  /* = { NS, -1, -NS, 1, NS-1, -NS-1, -NS+1, NS+1}; */



/* ================================================================ */
/*                          Other functions                         */
/* ================================================================ */


/* SGF routines for debugging purposes in sgffile.c */
void sgffile_begindump(struct SGFTree_t *tree);
void sgffile_enddump(const char *filename);


/* Hashing and Caching statistics. */
struct stats_data {
  int nodes;                     /* Number of visited nodes while reading */
  int read_result_entered;       /* Number of read results entered. */
  int read_result_hits;          /* Number of hits of read results. */
  int trusted_read_result_hits;  /* Number of hits of read results   */
				 /* with sufficient remaining depth. */
};

extern struct stats_data stats;


/* printutils.c */
int gprintf(const Goban *goban, const char *fmt, ...);
void vgprintf(const Goban *goban, FILE *outputfile, const char *fmt,
	      va_list ap);
void mprintf(const Goban *goban, const char *fmt, ...);
void gfprintf(const Goban *goban, FILE *outfile, const char *fmt, ...);

const char *color_to_string(int color);
const char *location_to_string(int board_size, int pos);
void location_to_buffer(int board_size, int pos, char *buf);

int string_to_location(int boardsize, const char *str, int *m, int *n);

int is_hoshi_point(int board_size, int m, int n);
void draw_letter_coordinates(int board_size, FILE *outfile);
void simple_showboard(const Goban *goban, FILE *outfile);

void mark_goal_in_sgf(const Goban *goban, char goal[BOARDMAX]);

/* ================================================================ */
/*                         assertions                               */
/* ================================================================ */

/* Our own abort() which prints board state on the way out.
 * (pos) is a "relevant" board position for info.
 */
void abortgo(const Goban *goban, const char *file, int line,
	     const char *msg, int pos)
#ifdef __GNUC__
	__attribute__ ((noreturn))
#endif
	;

#ifdef GG_TURN_OFF_ASSERTS

#define ASSERT2(goban, x, i, j)
#define ASSERT1(goban, x, pos)

#else

#define ASSERT2(goban, x, i, j)						\
  do {									\
    if (!(x))								\
      abortgo((goban), __FILE__, __LINE__, #x, POS((i), (j)));		\
  } while (0)

#define ASSERT1(goban, x, pos)						\
  do {									\
    if (!(x))								\
      abortgo((goban), __FILE__, __LINE__, #x, (pos));			\
  } while (0)

#endif

#define gg_assert(goban, x)	ASSERT1((goban), (x), NO_MOVE)


/* Are we using valgrind memory checking? */
#if USE_VALGRIND
#include <valgrind/memcheck.h>
#else
#define VALGRIND_MAKE_WRITABLE(a, b)
#endif

#endif  /* _BOARD_H_ */


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
