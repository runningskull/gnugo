/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * This is GNU Go, a Go program. Contact gnugo@gnu.org, or see       *
 * http://www.gnu.org/software/gnugo/ for more information.          *
 *                                                                   *
 * Copyright 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007,   *
 * 2008, 2009 and 2010 by the Free Software Foundation.              *
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

/* ---------------------------------------------------------------- *
 * gnugo.h
 *	This file contains the public interface to the GNU Go engine.
 * ---------------------------------------------------------------- */


#ifndef _GNUGO_H_
#define _GNUGO_H_

#include "board.h"

#include <stdio.h>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef HAVE_CRTDBG_H
#include <crtdbg.h>
#endif

#include "sgftree.h"
#include "clock.h"
#include "winsocket.h"

/* interface.c */
/* Initialize the whole thing. Should be called once. */
void init_gnugo(float memory, unsigned int random_seed);


/* ================================================================ */
/*                some public macros used everywhere                */
/* ================================================================ */


/* Used in matchpat.c. Have to be different from WHITE, BLACK. */
#define ANCHOR_COLOR 6
#define ANCHOR_OTHER 7

/* Return codes for reading functions */

#define WIN  5
#define KO_A 4
#define GAIN 3
#define LOSS 2
#define KO_B 1
#define LOSE 0

const char *result_to_string(int result);

/* Used by break_through(). Must be different from 0 and WIN. */
#define CUT  2


/* Surrounded */

#define SURROUNDED 1
#define WEAKLY_SURROUNDED 2

/* ================================================================ */
/*                        Board manipulation                        */
/* ================================================================ */


int check_boardsize(int boardsize, FILE *out);
void gnugo_clear_board(int boardsize);
void gnugo_play_move(int move, int color);
int gnugo_play_sgfnode(SGFNode *node, int to_move);
int gnugo_sethand(int desired_handicap, SGFNode *node);
float gnugo_estimate_score(float *upper, float *lower);

/* ================================================================ */
/*                           Game handling                          */
/* ================================================================ */


typedef struct {
  int handicap;
  int to_move;		/* whose move it currently is */
  SGFTree game_record;	/* Game record in sgf format. */
  int computer_player;	/* BLACK, WHITE, or EMPTY (used as BOTH) */
} Gameinfo;

void gameinfo_clear(Gameinfo *ginfo);
void gameinfo_print(Gameinfo *ginfo);
int gameinfo_play_sgftree_rot(Gameinfo *gameinfo, SGFTree *tree,
			      const char *untilstr, int orientation);
int gameinfo_play_sgftree(Gameinfo *gameinfo, SGFTree *tree,
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
extern char outfilename[128];	/* output file (-o option) */
extern int output_flags;	/* amount of output to outfile */

/* output flag bits */
#define OUTPUT_MARKDRAGONS         0x0001  /* mark dead and critical dragons */
#define OUTPUT_MOVEVALUES          0x0002  /* output values of all moves in list */

#define OUTPUT_DEFAULT             0 /* no debug output  by default */

/* debug flag bits */
/* NOTE : can specify -d0x... */
/* Please keep this list in sync with the DEBUG_FLAGS string below. */
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
#define DEBUG_PERSISTENT_CACHE	    0x200000
#define DEBUG_TOP_MOVES             0x400000
#define DEBUG_MISCELLANEOUS         0x800000
#define DEBUG_ORACLE_STREAM         0x1000000
#define DEBUG_LARGE_SCALE           0x1000000
#define DEBUG_SPLIT_OWL             0x2000000
#define DEBUG_TIME                  0x4000000


#define DEBUG_FLAGS "\
DEBUG_INFLUENCE             0x0001\n\
DEBUG_EYES                  0x0002\n\
DEBUG_OWL                   0x0004\n\
DEBUG_ESCAPE                0x0008\n\
DEBUG_MATCHER               0x0010\n\
DEBUG_DRAGONS               0x0020\n\
DEBUG_SEMEAI                0x0040\n\
DEBUG_LOADSGF               0x0080\n\
DEBUG_HELPER                0x0100\n\
DEBUG_READING               0x0200\n\
DEBUG_WORMS                 0x0400\n\
DEBUG_MOVE_REASONS          0x0800\n\
DEBUG_OWL_PERFORMANCE       0x1000\n\
DEBUG_BREAKIN               0x2000\n\
DEBUG_FILLLIB               0x4000\n\
DEBUG_READING_PERFORMANCE   0x8000\n\
DEBUG_SCORING               0x010000\n\
DEBUG_AFTERMATH             0x020000\n\
DEBUG_ATARI_ATARI           0x040000\n\
DEBUG_READING_CACHE         0x080000\n\
DEBUG_TERRITORY             0x100000\n\
DEBUG_PERSISTENT_CACHE      0x200000\n\
DEBUG_TOP_MOVES             0x400000\n\
DEBUG_MISCELLANEOUS         0x800000\n\
DEBUG_ORACLE_STREAM         0x1000000\n\
DEBUG_LARGE_SCALE           0x1000000\n\
DEBUG_SPLIT_OWL             0x2000000\n\
DEBUG_TIME                  0x4000000\n\
"


extern int debug;		/* debug flags */
extern int fusekidb;            /* use fuseki database */
extern int disable_fuseki;      /* do not generate fuseki moves */
extern int josekidb;            /* use joseki database */
extern int semeai_variations;   /* max variations considered reading semeai */
extern int showtime;		/* print genmove time */
extern int showscore;		/* print score */
extern int chinese_rules;       /* use chinese (area) rules for counting */
extern int experimental_owl_ext;     /* use experimental owl (GAIN/LOSS) */
extern int experimental_connections; /* use experimental connection module */
extern int alternate_connections;    /* use alternate connection module */
extern int owl_threats;              /* compute owl threats */
extern int capture_all_dead;         /* capture all dead opponent stones */
extern int play_out_aftermath; /* make everything unconditionally settled */
extern int resign_allowed;           /* allows GG to resign hopeless games */
extern int play_mirror_go;           /* try to play mirror go if possible */
extern int mirror_stones_limit;      /* but stop at this number of stones */
extern int gtp_version;              /* version of Go Text Protocol */
extern int use_monte_carlo_genmove;  /* use Monte Carlo move generation */
extern int mc_games_per_level;       /* number of Monte Carlo simulations per level */

/* Mandatory values of reading parameters. Normally -1, if set
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
extern int mandated_semeai_node_limit; 

extern int autolevel_on;

extern float potential_moves[BOARDMAX];

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


/* influence.c */
void debug_influence_move(int move);


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
void examine_position(int how_much, int aftermath_play);
void silent_examine_position(int how_much);


/* ================================================================ */
/*                         statistics functions                     */
/* ================================================================ */


/* These are mostly used for GTP examination. */
void reset_owl_node_counter(void);
int get_owl_node_counter(void);
void reset_reading_node_counter(void);
int get_reading_node_counter(void);
void reset_connection_node_counter(void);
int get_connection_node_counter(void);



/* ================================================================ */
/*                         Low level functions                      */
/* ================================================================ */

/* utils.c */
void who_wins(int color, FILE *outfile);

/* high-level routine to generate the best move for the given color */
int genmove(int color, float *value, int *resign);
int genmove_conservative(int color, float *value);

/* Play through the aftermath. */
float aftermath_compute_score(int color, SGFTree *tree);

/* Basic information gathering. */
/* worm.c */
void make_worms(void);
void compute_worm_influence(void);

/* dragon.c */
void make_dragons(int stop_before_owl);
void initialize_dragon_data(void);
void show_dragons(void);
enum dragon_status crude_status(int pos);
enum dragon_status dragon_status(int pos);
int same_dragon(int dr1, int dr2);

/* debugging functions */
void prepare_pattern_profiling(void);
void report_pattern_profiling(void);

/* sgffile.c */
void sgffile_add_debuginfo(SGFNode *node, float value);
void sgffile_output(SGFTree *tree);

void sgffile_printsgf(int color_to_play, const char *filename);
void sgffile_printboard(SGFTree *tree);
void sgffile_recordboard(SGFNode *node);
int get_sgfmove(SGFProperty *property);

/* sgfdecide.c */
void decide_string(int pos);
void decide_connection(int apos, int bpos);
void decide_owl(int pos);
void decide_dragon_data(int pos);
void decide_semeai(int apos, int bpos);
void decide_tactical_semeai(int apos, int bpos);
void decide_position(void);
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
