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

#include "gnugo.h"

#include <stdio.h>

#include "sgftree.h"
#include "liberty.h"
#include "config.h"

/* 
 * Define all global variables used within the engine.
 */


/* The go board and position. */
int          board_size = DEFAULT_BOARD_SIZE; /* board size */
Intersection board[BOARDSIZE];
int          board_ko_pos;
int          white_captured;    /* number of black and white stones captured */
int          black_captured;

Intersection initial_board[BOARDSIZE];
int          initial_board_ko_pos;
int          initial_white_captured;
int          initial_black_captured;
int          move_history_color[MAX_MOVE_HISTORY];
int          move_history_pos[MAX_MOVE_HISTORY];
int          move_history_pointer;

int thrashing_dragon = 0; /* Dead opponent's dragon trying to live */

float        komi;
int          movenum;

Intersection shadow[BOARDMAX];

/* Hashing of positions. */
Hash_data    hashdata;

int hashflags = HASH_DEFAULT;

float potential_moves[MAX_BOARD][MAX_BOARD];

/* Used by reading. */
int stackp;             /* stack pointer */
int position_number;    /* position number */
int depth;              /* deep reading cut off */
int backfill_depth;     /* deep reading cut off */
int backfill2_depth;    /* deep reading cut off */
int superstring_depth;  /* deep reading cut off */
int fourlib_depth;      /* deep reading cut off */
int ko_depth;           /* deep reading cut off */
int branch_depth;       /* deep reading cut off */
int aa_depth;
int owl_distrust_depth;   /* below this owl trusts the optics code */
int owl_branch_depth;     /* below this owl tries only one variation */
int owl_reading_depth;    /* owl does not read below this depth */
int owl_node_limit;       /* maximum number of nodes considered */
int mandated_depth;             /* deep reading cut off, mandated value */
int mandated_backfill_depth;    /* deep reading cut off, mandated value */
int mandated_backfill2_depth;   /* deep reading cut off, mandated value */
int mandated_superstring_depth; /* deep reading cut off, mandated value */
int mandated_fourlib_depth;     /* deep reading cut off, mandated value */
int mandated_ko_depth;          /* deep reading cut off, mandated value */
int mandated_branch_depth;      /* deep reading cut off, mandated value */
int mandated_aa_depth;
int mandated_owl_distrust_depth;  
int mandated_owl_branch_depth;  
int mandated_owl_reading_depth; 
int mandated_owl_node_limit;    

/* Miscellaneous. */
int quiet             = 0;  /* minimal output */
int showstatistics    = 0;  /* print statistics */
int allpats           = 0;  /* generate all patterns, even small ones */
int printworms        = 0;  /* print full data on each string */
int printmoyo         = 0;  /* print moyo board each move */
int printboard        = 0;  /* print board each move */
int count_variations  = 0;  /* used by decide_string */
int sgf_dump          = 0;  /* used by decide_string */
SGFTree *sgf_dumptree = NULL;
int random_seed       = 0;  /* random seed */
int loading           = 0;  /* TRUE if last loaded move comes from file */
int fusekidb          = 1;  /* use fuseki database */
int disable_fuseki    = 0;  /* do not generate fuseki moves */
int josekidb          = 1;  /* use fuseki database */
int showtime          = 0;  /* print time to find move */
int showscore         = 0;  /* print estimated score */
float score           = 0.0;
float lower_bound     = 0.0;
float upper_bound     = 0.0;
int level             = DEFAULT_LEVEL; /* strength; up to 10 supported */
int urgent            = 0;  /* urgent move on board */
int debug             = 0;  /* controls debug output */
int verbose           = 0;  /* trace level */
char outfilename[128] = ""; /* output file (-o option) */
int output_flags      = OUTPUT_DEFAULT; /* amount of output to outfile */

int disable_threat_computation = 0;
int disable_endgame_patterns   = 0;
int doing_scoring              = 0;

int chinese_rules       = 0;    /* ruleset choice for GMP connection */
/* use experimental semeai module */
int experimental_semeai = EXPERIMENTAL_SEMEAI;
int semeai_variations   = DEFAULT_SEMEAI_VARIATIONS;
/* use experimental connection module */
int experimental_connections = EXPERIMENTAL_CONNECTIONS;
/* use alternate connection reading algorithm */
int owl_threats = OWL_THREATS;          /* compute owl threats */
/* use experimental owl extension (GAIN/LOSS) */
int experimental_owl_ext = EXPERIMENTAL_OWL_EXT;

int allow_suicide       = 0;    /* allow opponent to make suicide moves */
int capture_all_dead    = 0;    /* capture all dead opponent stones */
int play_out_aftermath  = 0;    /* make everything unconditionally settled */

int play_mirror_go      = 0;    /* try to play mirror go if possible */
int mirror_stones_limit = -1;   /* but stop at this number of stones */

int gtp_version         = 2;    /* Use GTP version 2 by default. */

float best_move_values[10];
int   best_moves[10];

int close_worms[BOARDMAX][4];
int number_close_worms[BOARDMAX];
int close_black_worms[BOARDMAX][4];
int number_close_black_worms[BOARDMAX];
int close_white_worms[BOARDMAX][4];
int number_close_white_worms[BOARDMAX];

int false_eye_territory[BOARDMAX];

/* Various statistics are collected here. */
struct stats_data stats;

struct worm_data      worm[BOARDMAX];
struct dragon_data    dragon[BOARDMAX];
int                   number_of_dragons;
struct dragon_data2   *dragon2 = NULL;
struct half_eye_data  half_eye[BOARDMAX];
struct half_eye_data  owl_half_eye[BOARDMAX];
struct eye_data       black_eye[BOARDMAX];
struct eye_data       white_eye[BOARDMAX];
struct eye_data       owl_black_eye[BOARDMAX];
struct eye_data       owl_white_eye[BOARDMAX];
struct surround_data  surroundings[MAX_SURROUND];
int                   surround_pointer;
