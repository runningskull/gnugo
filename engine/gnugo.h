/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * This is GNU Go, a Go program. Contact gnugo@gnu.org, or see       *
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

/* ---------------------------------------------------------------- *
 * gnugo.h
 *	This file contains the public interface to the GNU Go engine.
 * ---------------------------------------------------------------- */


#ifndef _GNUGO_H_
#define _GNUGO_H_


#include <stdio.h>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef HAVE_VISUAL_C
# include <crtdbg.h>
#endif

#include "sgftree.h"
#include "clock.h"

/* interface.c */
/* Initialize the whole thing. Should be called once. */
void init_gnugo(float memory, unsigned int random_seed);


/* ================================================================ */
/*                some public macros used everywhere                */
/* ================================================================ */


/* Board size */

#define DEFAULT_BOARD_SIZE 19

/* Colors */
#define EMPTY        0
#define WHITE        1
#define BLACK        2
#define GRAY         3
#define GRAY_BORDER  3
#define WHITE_BORDER 4
#define BLACK_BORDER 5
#define ANCHOR_COLOR 6
#define ANCHOR_OTHER 7


#define OTHER_COLOR(color)  	(WHITE+BLACK-(color))
#define IS_STONE(arg)   	((arg) == WHITE || (arg) == BLACK)
#define BORDER_COLOR(color)	(color + WHITE_BORDER - WHITE)

/* Return codes for reading functions */

#define WIN  5
#define KO_A 4
#define GAIN 3
#define LOSS 2
#define KO_B 1
#define LOSE 0

/* Used by break_through(). Must be different from 0 and WIN. */
#define CUT  2

/* Group statuses */
#define DEAD        0
#define ALIVE       1
#define CRITICAL    2 
#define UNKNOWN     3
#define UNCHECKED   4
#define MAX_DRAGON_STATUS 4	/* used to size an array in matchpat.c */

/* Dragon safety values. DEAD, ALIVE, and CRITICAL are reused. */
#define INESSENTIAL     5
#define TACTICALLY_DEAD 6
#define ALIVE_IN_SEKI   7
#define STRONGLY_ALIVE  8
#define INVINCIBLE      9
#define INSUBSTANTIAL   10
#define CAN_THREATEN_ATTACK  11
#define CAN_THREATEN_DEFENSE 12

/* Surrounded */

#define SURROUNDED 1
#define WEAKLY_SURROUNDED 2

/* Final statuses for empty vertices. */
#define BLACK_TERRITORY 13
#define WHITE_TERRITORY 14
#define DAME            15

/* ================================================================ */
/*                        Board manipulation                        */
/* ================================================================ */


/* Board sizes */
#define MIN_BOARD          5	   /* Minimum supported board size.   */
#define MAX_BOARD         19       /* Maximum supported board size.   */
#define MAX_HANDICAP       9	   /* Maximum supported handicap.     */
#define MAX_MOVE_HISTORY 500       /* Max number of moves remembered. */

/* This type is used to store each intersection on the board.
 *
 * On a 486, char is best, since the time taken to push and pop
 * becomes significant otherwise. On other platforms, an int may
 * be better, e.g. if memcpy() is particularly fast, or if
 * character access is very slow.
 */

typedef unsigned char Intersection;


void gnugo_clear_board(int boardsize);
void gnugo_set_komi(float new_komi);
void gnugo_add_stone(int i, int j, int color);
void gnugo_remove_stone(int i, int j);
int  gnugo_is_pass(int i, int j);
void gnugo_play_move(int i, int j, int color);
int  gnugo_undo_move(int n);
int  gnugo_play_sgfnode(SGFNode *node, int to_move);
int  gnugo_play_sgftree(SGFNode *root, int *until, SGFNode **curnode);
int  gnugo_is_legal(int i, int j, int color);
int  gnugo_is_suicide(int i, int j, int color);

int  gnugo_placehand(int handicap);
int  gnugo_sethand(int handicap, SGFNode *root);
void gnugo_recordboard(SGFNode *node);

int  gnugo_genmove(int *i, int *j, int color);

int  gnugo_attack(int m, int n, int *i, int *j);
int  gnugo_find_defense(int m, int n, int *i, int *j);

void  gnugo_who_wins(int color, FILE *outfile);
float gnugo_estimate_score(float *upper, float *lower);
void  gnugo_examine_position(int color, int how_much);

float gnugo_get_komi(void);
void  gnugo_get_board(int b[MAX_BOARD][MAX_BOARD]);
int   gnugo_get_boardsize(void);
int   gnugo_get_move_number(void);

/* ================================================================ */
/*                           Game handling                          */
/* ================================================================ */


typedef struct {
  int       handicap;

  int       to_move;		/* whose move it currently is */
  SGFTree   game_record;	/* Game record in sgf format. */

  int       computer_player;	/* BLACK, WHITE, or EMPTY (used as BOTH) */
} Gameinfo;

void gameinfo_clear(Gameinfo *ginfo, int boardsize, float komi);
void gameinfo_print(Gameinfo *ginfo);
void gameinfo_load_sgfheader(Gameinfo *gameinfo, SGFNode *head);
void gameinfo_play_move(Gameinfo *ginfo, int i, int j, int color);
int  gameinfo_play_sgftree_rot(Gameinfo *gameinfo, SGFTree *tree,
			       const char *untilstr, int orientation);
int  gameinfo_play_sgftree(Gameinfo *gameinfo, SGFTree *tree,
			   const char *untilstr);


/* ================================================================ */
/*                           global variables                       */
/* ================================================================ */


/* Miscellaneous debug options. */
extern int quiet;		/* Minimal output. */
extern int verbose;		/* Bore the opponent. */
extern int allpats;		/* generate all patterns, even small ones */
extern int printworms;		/* print full data on each string */
extern int printmoyo;		/* print moyo board each move */
extern int printdragons;	/* print full data on each dragon */
extern int printboard;		/* print board each move */
extern int showstatistics;	/* print statistics */
extern int profile_patterns;	/* print statistics of pattern usage */
extern char outfilename[128];  /* output file (-o option) */
extern int output_flags;       /* amount of output to outfile */

/* output flag bits */
#define OUTPUT_MARKDRAGONS         0x0001  /* mark dead and critical dragons */
#define OUTPUT_MOVEVALUES          0x0002  /* output values of all moves in list */

#define OUTPUT_DEFAULT             0 /* no debug output  by default */

/* debug flag bits */
/* NOTE : can specify -d0x... */
#define DEBUG_INFLUENCE             0x0001
#define DEBUG_EYES                  0x0002
#define DEBUG_OWL                   0x0004
#define DEBUG_ESCAPE                0x0008
#define DEBUG_MATCHER               0x0010
#define DEBUG_DRAGONS               0x0020
#define DEBUG_SEMEAI                0x0040
#define DEBUG_LOADSGF               0x0080
#define DEBUG_HELPER                0x0100
#define DEBUG_READING               0x0200
#define DEBUG_WORMS                 0x0400
#define DEBUG_MOVE_REASONS          0x0800
#define DEBUG_OWL_PERFORMANCE       0x1000
#define DEBUG_BREAKIN		    0x2000
#define DEBUG_FILLLIB               0x4000
#define DEBUG_READING_PERFORMANCE   0x8000
#define DEBUG_SCORING               0x010000
#define DEBUG_AFTERMATH             0x020000
#define DEBUG_ATARI_ATARI           0x040000
#define DEBUG_READING_CACHE         0x080000
#define DEBUG_TERRITORY             0x100000
#define DEBUG_OWL_PERSISTENT_CACHE  0x200000
#define DEBUG_TOP_MOVES             0x400000
#define DEBUG_MISCELLANEOUS         0x800000
#define DEBUG_ORACLE_STREAM         0x1000000

/* hash flag bits 
 *
 * Regarding HASH_DEFAULT:
 * Hashing all functions saves time, but wastes table space, which is
 * bad when the reading is complicated. HASH_DEFAULT is a compromise. 
 */

#define HASH_FIND_DEFENSE 0x0001  /* NOTE : can specify -d0x... */
#define HASH_ATTACK       0x0020
#define HASH_OWL_ATTACK   0x0100
#define HASH_OWL_DEFEND   0x0200
#define HASH_SEMEAI       0x0400
#define HASH_CONNECT      0x0800
#define HASH_DISCONNECT   0x1000
#define HASH_BREAK_IN	  0x2000
#define HASH_BLOCK_OFF	  0x4000
#define HASH_NOTHING      0
#define HASH_ALL          0xffff
#define HASH_DEFAULT      (HASH_ATTACK | HASH_FIND_DEFENSE\
			   | HASH_OWL_ATTACK | HASH_OWL_DEFEND | HASH_SEMEAI\
                           | HASH_CONNECT | HASH_DISCONNECT\
			   | HASH_BREAK_IN | HASH_BLOCK_OFF)

extern int debug;		/* debug flags */
extern int hashflags;		/* hash flags */
extern int fusekidb;            /* use fuseki database */
extern int disable_fuseki;      /* do not generate fuseki moves */
extern int josekidb;            /* use joseki database */
extern int level;		/* controls depth of reading */
extern int semeai_variations;   /* max variations considered reading semeai */
extern int showtime;		/* print genmove time */
extern int showscore;		/* print score */
extern float score;
extern float lower_bound;
extern float upper_bound;
extern int chinese_rules;       /* use chinese (area) rules for counting */
extern int experimental_owl_ext;     /* use experimental owl (GAIN/LOSS) */
extern int experimental_semeai;      /* use experimental semeai module */
extern int experimental_connections; /* use experimental connection module */
extern int alternate_connections;    /* use alternate connection module */
extern int owl_threats;              /* compute owl threats */
extern int allow_suicide;            /* allow opponent to make suicide moves */
extern int capture_all_dead;         /* capture all dead opponent stones */
extern int play_out_aftermath; /* make everything unconditionally settled */
extern int resign_allowed;           /* allows GG to resign hopeless games */
extern int play_mirror_go;           /* try to play mirror go if possible */
extern int mirror_stones_limit;      /* but stop at this number of stones */
extern int gtp_version;              /* version of Go Text Protocol */

#if EXPERIMENTAL_READING
extern int defend_by_pattern;  /* use patterns for tactical reading defense */
extern int attack_by_pattern;  /* use patterns for tactical reading attack */
#endif

/* Mandatory values of reading parameters. Normally -1, if set at 
 * these override the values derived from the level. */
extern int mandated_depth;
extern int mandated_backfill_depth;
extern int mandated_backfill2_depth;
extern int mandated_break_chain_depth;
extern int mandated_superstring_depth;
extern int mandated_fourlib_depth;
extern int mandated_ko_depth;
extern int mandated_branch_depth;
extern int mandated_aa_depth;
extern int mandated_owl_distrust_depth;
extern int mandated_owl_branch_depth;
extern int mandated_owl_reading_depth;
extern int mandated_owl_node_limit; 

/* Keep this as 2D until we change the entire API. */
extern float potential_moves[MAX_BOARD][MAX_BOARD];

extern int limit_search;  /* limit move search to a portion of the board */
extern int oracle_exists; /* oracle is available for consultation        */
extern int metamachine;   /* use metamachine_genmove                     */

/* ================================================================ */
/*                 tracing and debugging functions                  */
/* ================================================================ */

/* Colors. */
#define GG_COLOR_BLACK   0
#define GG_COLOR_RED     1
#define GG_COLOR_GREEN   2
#define GG_COLOR_YELLOW  3
#define GG_COLOR_BLUE    4
#define GG_COLOR_MAGENTA 5
#define GG_COLOR_CYAN    6
#define GG_COLOR_WHITE   7

/* showbord.c */
void start_draw_board(void);
void draw_color_char(int m, int n, int c, int color);
void draw_char(int m, int n, int c);
void end_draw_board(void);
void showboard(int xo);  /* ascii rep. of board to stderr */
void simple_showboard(FILE *outfile);  /* ascii rep. of board to outfile */

/* printutils.c */
int gprintf(const char *fmt, ...);
void mprintf(const char *fmt, ...);
void gfprintf(FILE *outfile, const char *fmt, ...);
const char *color_to_string(int color);
const char *location_to_string(int pos);
void        location_to_buffer(int pos, char *buf);
const char *status_to_string(int status);
const char *safety_to_string(int status);
const char *result_to_string(int result);
const char *routine_to_string(int routine);
int string_to_location(int boardsize, char *str, int *m, int *n);
double gg_gettimeofday(void);


/* influence.c */
void debug_influence_move(int i, int j);


#define TRACE  (!(verbose)) ? (void)0 : (void)gprintf

#ifdef HAVE_VARIADIC_DEFINE

/* gnuc allows variadic macros, so the tests can be done inline */
#define DEBUG(level, fmt, args...) \
    do { if ((debug & (level))) gprintf(fmt, ##args); } while (0)

#else /*HAVE_VARIADIC_DEFINE*/

/* if debug == 0, then can skip the function call. */
#define DEBUG  (!(debug)) ? (void)0 : (void)DEBUG_func
int DEBUG_func(int level, const char *fmt, ...);

#endif  /*HAVE_VARIADIC_DEFINE*/


/* genmove.c */
#define EXAMINE_WORMS               1
#define EXAMINE_INITIAL_INFLUENCE   2
#define EXAMINE_DRAGONS_WITHOUT_OWL 3
#define EXAMINE_DRAGONS             4
#define EXAMINE_OWL_REASONS         5
#define EXAMINE_INITIAL_INFLUENCE2  6
#define FULL_EXAMINE_DRAGONS        7

#define EXAMINE_ALL                 99

void reset_engine(void);
void examine_position(int color, int how_much);
void silent_examine_position(int color, int how_much);


/* ================================================================ */
/*                         statistics functions                     */
/* ================================================================ */


/* These are mostly used for GTP examination. */
void reset_owl_node_counter(void);
int get_owl_node_counter(void);
void reset_reading_node_counter(void);
int get_reading_node_counter(void);
void reset_trymove_counter(void);
int get_trymove_counter(void);
void reset_connection_node_counter(void);
int get_connection_node_counter(void);


/* ================================================================ */
/*                         Low level functions                      */
/* ================================================================ */


/* board.c */
/* General board handling. */
void clear_board(void);
int test_gray_border(void);
void setup_board(Intersection new_board[MAX_BOARD][MAX_BOARD], int ko_pos,
		 int *last, float new_komi, int w_captured, int b_captured);


/* Putting stones on the board.. */
void add_stone(int pos, int color);
void remove_stone(int pos);
void play_move(int pos, int color);
int undo_move(int n);
int get_last_move(void);
int get_last_player(void);
int get_last_opponent_move(int color);
int is_pass(int pos);
int is_legal(int pos, int color);
int is_suicide(int pos, int color);
int is_illegal_ko_capture(int pos, int color);
int trymove(int pos, int color, const char *message, int str, 
	    int komaster, int kom_pos);
int tryko(int pos, int color, const char *message, 
	  int komaster, int kom_pos);
void popgo(void);

/* utils.c */
void change_dragon_status(int dr, int status);
void who_wins(int color, FILE *outfile);

/* high-level routine to generate the best move for the given color */
int genmove(int *i, int *j, int color);
int genmove_conservative(int *i, int *j, int color);

/* Play through the aftermath. */
float aftermath_compute_score(int color, float komi, SGFTree *tree);
int aftermath_final_status(int color, int pos);

/* Basic information gathering. */
/* worm.c */
void make_worms(void);
void compute_worm_influence(void);

/* dragon.c */
void make_dragons(int color, int stop_before_owl);
void initialize_dragon_data(void);
void show_dragons(void);
int crude_status(int pos);
int dragon_status(int pos);
int same_dragon(int dr1, int dr2);

/* moyo functions */
void print_moyo(void);

/* debugging functions */
void prepare_pattern_profiling(void);
void report_pattern_profiling(void);

/* sgffile.c */
void sgffile_add_debuginfo(SGFNode *node, int value);
void sgffile_output(SGFTree *tree);

void sgffile_printsgf(int color_to_play, const char *filename);
void sgffile_printboard(SGFTree *tree);
void sgffile_recordboard(SGFNode *node);

/* sgfdecide.c */
void decide_string(int pos);
void decide_connection(int apos, int bpos);
void decide_owl(int pos);
void decide_dragon_data(int pos);
void decide_semeai(int apos, int bpos);
void decide_tactical_semeai(int apos, int bpos);
void decide_position(int color);
void decide_eye(int pos);
void decide_combination(int color);
void decide_surrounded(int pos);
void decide_oracle(Gameinfo *gameinfo, char *infilename, char *untilstring);

/*oracle.c*/
void dismiss_oracle(void);
void oracle_clear_board(int boardsize);

#endif  /* _GNUGO_H_ */


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
