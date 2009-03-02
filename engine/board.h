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

#ifndef _BOARD_H_
#define _BOARD_H_

#include <stdarg.h>
#include "config.h"
#include "sgftree.h"
#include "winsocket.h"

/* This type is used to store each intersection on the board.
 *
 * On a 486, char is best, since the time taken to push and pop
 * becomes significant otherwise. On other platforms, an int may
 * be better, e.g. if memcpy() is particularly fast, or if
 * character access is very slow.
 */

typedef unsigned char Intersection;

/* FIXME: This is very ugly but we can't include hash.h until we have
 * defined Intersection. And we do need to include it before using
 * Hash_data.
 */
#include "hash.h"

/* local versions of absolute value, min and max */

#define gg_abs(x) ((x) < 0 ? -(x) : (x))
#define gg_min(a, b) ((a)<(b) ? (a) : (b))
#define gg_max(a, b) ((a)<(b) ? (b) : (a))

/* Avoid compiler warnings with unused parameters */
#define UNUSED(x)  (void)x


/* A string with n stones can have at most 2(n+1) liberties. From this
 * follows that an upper bound on the number of liberties of a string
 * on a board of size N^2 is 2/3 (N^2+1).
 */
#define MAXLIBS   (2*(MAX_BOARD*MAX_BOARD + 1)/3)
/* This is a smaller, practical number of liberties that we care to keep track of. */
#define MAX_LIBERTIES 8


/* This is an upper bound on the number of strings that can exist on
 * the board simultaneously. Since each string must have at least one
 * liberty and each empty point can provide a liberty to at most four
 * strings, at least one out of five board points must be empty.
 *
 * FIXME: This is not sufficiently large. Above stackp==0, the
 *        incremental board code doesn't re-use the entries for
 *        removed or merged strings, while new strings require new
 *        entries. This is a problem only in very pathological cases,
 *        and is extremely unlikely to occur in practice.
 *
 *        Actually, in the not all that pathological case of a
 *        repeated triple ko cycle, each move creates a new string and
 *        thus makes use of one more string, which relatively quickly
 *        will exhaust the available strings. For a safe upper bound
 *        MAX_STRINGS should be set to
 *        MAX_STACK + 4 * MAX_BOARD * MAX_BOARD / 5.
 *        It's not clear that it's worth the extra memory, however.
 */
#define MAX_STRINGS (4 * MAX_BOARD * MAX_BOARD / 5)

/* Per gf: Unconditional_life() can get very close to filling the 
 * entire board under certain circumstances. This was discussed in 
 * the list around August 21, 2001, in a thread with the subject 
 * "gnugo bug logs".
 */
#define MAXSTACK  MAX_BOARD * MAX_BOARD
#define MAXCHAIN  160

#define HASH_RANDOM_SEED 12345

/* ================================================================ *
 *                         One-dimensional board                    *
 * ================================================================ */

/* Board sizes */


#define MIN_BOARD          1       /* Minimum supported board size.   */
#define MAX_BOARD         19       /* Maximum supported board size.   */
#define MAX_HANDICAP       9       /* Maximum supported handicap.     */
#define MAX_MOVE_HISTORY 500       /* Max number of moves remembered. */

#define DEFAULT_BOARD_SIZE MAX_BOARD

/* Colors and komaster states. */
enum colors {
  EMPTY,
  WHITE,
  BLACK,
  GRAY,
  GRAY_WHITE,
  GRAY_BLACK,
  WEAK_KO,
  NUM_KOMASTER_STATES
};

#define COLOR_NAMES \
  "empty", \
  "white", \
  "black", \
  "gray", \
  "gray_white", \
  "gray_black", \
  "weak_ko"

const char *color_to_string(int color);

#define OTHER_COLOR(color)      (WHITE+BLACK-(color))
#define IS_STONE(arg)           ((arg) == WHITE || (arg) == BLACK)

/* Note that POS(-1, -1) == 0
 * DELTA() is defined so that POS(i+di, j+dj) = POS(i, j) + DELTA(di, dj).
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

#define BOARD(i, j)   board[POS(i, j)]


#define MIRROR_MOVE(pos) POS(board_size - 1 - I(pos), board_size - 1 - J(pos))

/* ================================================================ */
/*                         global variables                         */
/* ================================================================ */

/* The board and the other parameters deciding the current position. */
extern int          board_size;             /* board size (usually 19) */
extern Intersection board[BOARDSIZE];       /* go board */
extern int          board_ko_pos;
extern int          black_captured;   /* num. of black stones captured */
extern int          white_captured;

extern Intersection initial_board[BOARDSIZE];
extern int          initial_board_ko_pos;
extern int          initial_white_captured;
extern int          initial_black_captured;
extern int          move_history_color[MAX_MOVE_HISTORY];
extern int          move_history_pos[MAX_MOVE_HISTORY];
extern Hash_data    move_history_hash[MAX_MOVE_HISTORY];
extern int          move_history_pointer;

extern float        komi;
extern int          handicap;     /* used internally in chinese scoring */
extern int          movenum;      /* movenumber - used for debug output */
		    
extern signed char  shadow[BOARDMAX];      /* reading tree shadow */

enum suicide_rules {
  FORBIDDEN,
  ALLOWED,
  ALL_ALLOWED
};
extern enum suicide_rules suicide_rule;

enum ko_rules {
  SIMPLE,
  NONE,
  PSK,
  SSK
};
extern enum ko_rules ko_rule;


extern int stackp;                /* stack pointer */
extern int count_variations;      /* count (decidestring) */
extern SGFTree *sgf_dumptree;


/* This struct holds the internal board state. */
struct board_state {
  int board_size;

  Intersection board[BOARDSIZE];
  int board_ko_pos;
  int black_captured;
  int white_captured;

  Intersection initial_board[BOARDSIZE];
  int initial_board_ko_pos;
  int initial_white_captured;
  int initial_black_captured;
  int move_history_color[MAX_MOVE_HISTORY];
  int move_history_pos[MAX_MOVE_HISTORY];
  Hash_data move_history_hash[MAX_MOVE_HISTORY];
  int move_history_pointer;

  float komi;
  int handicap;
  int move_number;
};

/* This is increased by one anytime a move is (permanently) played or
 * the board is cleared.
 */
extern int position_number;

/* ================================================================ */
/*                        board.c functions                         */
/* ================================================================ */


/* Functions handling the permanent board state. */
void clear_board(void);
int test_gray_border(void);
void setup_board(Intersection new_board[MAX_BOARD][MAX_BOARD], int ko_pos,
                 int *last, float new_komi, int w_captured, int b_captured);
void add_stone(int pos, int color);
void remove_stone(int pos);
void play_move(int pos, int color);
int undo_move(int n);

void store_board(struct board_state *state);
void restore_board(struct board_state *state);

/* Information about the permanent board. */
int get_last_move(void);
int get_last_player(void);
int get_last_opponent_move(int color);
int stones_on_board(int color);

/* Functions handling the variable board state. */
int trymove(int pos, int color, const char *message, int str);
int tryko(int pos, int color, const char *message);
void popgo(void);
int komaster_trymove(int pos, int color,
		     const char *message, int str,
		     int *is_conditional_ko, int consider_conditional_ko);
int get_komaster(void);
int get_kom_pos(void);

int move_in_stack(int pos, int cutoff);
void get_move_from_stack(int k, int *move, int *color);
void dump_stack(void);
void do_dump_stack(void);

void reset_trymove_counter(void);
int get_trymove_counter(void);

/* move properties */
int is_pass(int pos);
int is_legal(int pos, int color);
int is_suicide(int pos, int color);
int is_illegal_ko_capture(int pos, int color);
int is_allowed_move(int pos, int color);
int is_ko(int pos, int color, int *ko_pos);
int is_ko_point(int pos);
int does_capture_something(int pos, int color);
int is_self_atari(int pos, int color);

/* Purely geometric functions. */
int is_edge_vertex(int pos);
int is_corner_vertex(int pos);
int edge_distance(int pos);
int square_dist(int pos1, int pos2);
int rotate1(int pos, int rot);

/* Basic string information. */
int find_origin(int str);
int chainlinks(int str, int adj[MAXCHAIN]);
int chainlinks2(int str, int adj[MAXCHAIN], int lib);
int chainlinks3(int str, int adj[MAXCHAIN], int lib);
int extended_chainlinks(int str, int adj[MAXCHAIN], int both_colors);

int liberty_of_string(int pos, int str);
int second_order_liberty_of_string(int pos, int str);
int neighbor_of_string(int pos, int str);
int has_neighbor(int pos, int color);
int same_string(int str1, int str2);
int adjacent_strings(int str1, int str2);
void mark_string(int str, signed char mx[BOARDMAX], signed char mark);
int are_neighbors(int pos1, int pos2);

/* Count and/or find liberties at (pos). */
int countlib(int str);
int findlib(int str, int maxlib, int *libs);
int fastlib(int pos, int color, int ignore_captures);
int approxlib(int pos, int color, int maxlib, int *libs);
int accuratelib(int pos, int color, int maxlib, int *libs);
int count_common_libs(int str1, int str2);
int find_common_libs(int str1, int str2, int maxlib, int *libs);
int have_common_lib(int str1, int str2, int *lib);

/* Count the number of stones in a string. */
int countstones(int str);
int findstones(int str, int maxstones, int *stones);
int count_adjacent_stones(int str1, int str2, int maxstones);

/* Detect a special shape. */
int send_two_return_one(int move, int color);

/* Special function for reading.c */
void incremental_order_moves(int move, int color, int string,
			     int *number_edges, int *number_same_string,
			     int *number_own, int *number_opponent,
			     int *captured_stones, int *threatened_stones,
			     int *saved_stones, int *number_open);

/* Board caches initialization functions. */
void clear_approxlib_cache(void);
void clear_accuratelib_cache(void);
  

/* Is this point inside the board? */
#if 0
#define ON_BOARD2(i, j) ((i)>=0 && (j)>=0 && (i)<board_size && (j)<board_size)
#else
/*
 * For the case when expr can only be slightly negative,
 *    if (expr < 0 || expr > something)
 * is equivalent to
 *    if ((unsigned) expr > something)
 *
 * (I think gcc knows this trick, but it does no harm to
 *  encode it explicitly since it saves typing !)
 */
#define ON_BOARD2(i, j) ((unsigned) (i) < (unsigned) board_size &&\
		         (unsigned) (j) < (unsigned) board_size)
#endif

#define ASSERT_ON_BOARD2(i, j) ASSERT2(ON_BOARD2((i), (j)), (i), (j))

#define ON_BOARD1(pos) (((unsigned) (pos) < BOARDSIZE) && board[pos] != GRAY)
#define ON_BOARD(pos) (board[pos] != GRAY)
#define ASSERT_ON_BOARD1(pos) ASSERT1(ON_BOARD1(pos), (pos))

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
int gprintf(const char *fmt, ...);
void vgprintf(FILE *outputfile, const char *fmt, va_list ap);
void mprintf(const char *fmt, ...);
void gfprintf(FILE *outfile, const char *fmt, ...);

const char *color_to_string(int color); 
const char *location_to_string(int pos);
void location_to_buffer(int pos, char *buf);

int string_to_location(int boardsize, const char *str);

int is_hoshi_point(int m, int n);
void draw_letter_coordinates(FILE *outfile);
void simple_showboard(FILE *outfile);

void mark_goal_in_sgf(signed char goal[BOARDMAX]);

/* ================================================================ */
/*                         assertions                               */
/* ================================================================ */

/* Our own abort() which prints board state on the way out.
 * (pos) is a "relevant" board position for info.
 */
void abortgo(const char *file, int line, const char *msg, int pos)
#ifdef __GNUC__
	__attribute__ ((noreturn))
#endif
	;

#ifdef GG_TURN_OFF_ASSERTS
#define ASSERT2(x, i, j)
#define ASSERT1(x, pos)
#else
/* avoid dangling else */
/* FIXME: Should probably re-write these using do {...} while (0) idiom. */
#define ASSERT2(x, i, j) if (x) ; else abortgo(__FILE__, __LINE__, #x, POS(i, j))
#define ASSERT1(x, pos) if (x) ; else abortgo(__FILE__, __LINE__, #x, pos)
#endif

#define gg_assert(x) ASSERT1(x, NO_MOVE)

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
