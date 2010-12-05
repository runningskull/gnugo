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

#include "gnugo.h"

#include <stdio.h>

#include "sgftree.h"
#include "liberty.h"
#include "config.h"

/* 
 * Define all global variables used within the engine.
 */

int thrashing_dragon = NO_MOVE; /* Dead opponent's dragon trying to live. */
signed char thrashing_stone[BOARDMAX]; /* All thrashing stones. */

float potential_moves[BOARDMAX];

/* Used by reading. */
int depth;              /* deep reading cut off */
int backfill_depth;     /* deep reading cut off */
int backfill2_depth;    /* deep reading cut off */
int break_chain_depth;  /* deep reading cut off */
int superstring_depth;  /* deep reading cut off */
int fourlib_depth;      /* deep reading cut off */
int ko_depth;           /* deep reading cut off */
int branch_depth;       /* deep reading cut off */
int aa_depth;
int depth_offset;       /* keeps track of temporary depth changes */
int owl_distrust_depth;   /* below this owl trusts the optics code */
int owl_branch_depth;     /* below this owl tries only one variation */
int owl_reading_depth;    /* owl does not read below this depth */
int owl_node_limit;       /* maximum number of nodes considered */
int semeai_branch_depth;
int semeai_branch_depth2;
int semeai_node_limit;
int connect_depth;	/* Used by Tristan Cazenave's connection reader. */
int connect_depth2;     /* Used by alternater connection reader. */
int connection_node_limit; 
int breakin_node_limit; /* Reading limits for break_in/block_off reading */
int breakin_depth;    
/* Mandated values for deep reading cutoffs. */
int mandated_depth = -1;   
int mandated_backfill_depth = -1;
int mandated_backfill2_depth = -1;
int mandated_break_chain_depth = -1;
int mandated_superstring_depth = -1;
int mandated_fourlib_depth = -1;   
int mandated_ko_depth = -1;       
int mandated_branch_depth = -1;  
int mandated_aa_depth = -1;
int mandated_owl_distrust_depth = -1;  
int mandated_owl_branch_depth = -1;  
int mandated_owl_reading_depth = -1; 
int mandated_owl_node_limit = -1;    
int mandated_semeai_node_limit = -1;    


/* Miscellaneous. */
int quiet             = 0;  /* minimal output */
int showstatistics    = 0;  /* print statistics */
int profile_patterns  = 0;  /* print statistics of pattern usage */
int allpats           = 0;  /* generate all patterns, even small ones */
int printworms        = 0;  /* print full data on each string */
int printmoyo         = 0;  /* print moyo board each move */
int printboard        = 0;  /* print board each move */
int fusekidb          = 1;  /* use fuseki database */
int disable_fuseki    = 0;  /* do not generate fuseki moves */
int josekidb          = 1;  /* use joseki database */
int showtime          = 0;  /* print time to find move */
int showscore         = 0;  /* print estimated score */
int debug             = 0;  /* controls debug output */
int verbose           = 0;  /* trace level */
char outfilename[128] = ""; /* output file (-o option) */
int output_flags      = OUTPUT_DEFAULT; /* amount of output to outfile */
int metamachine       = 0;  /* use metamachine_genmove */
int oracle_exists     = 0;  /* oracle is available for consultation   */
int autolevel_on      = 0;  /* Adjust level in GMP or ASCII mode. */

int disable_threat_computation = 0;
int disable_endgame_patterns   = 0;
int doing_scoring              = 0;

int chinese_rules = CHINESE_RULES; /* ruleset choice for GMP connection */
/* use experimental connection module */
int experimental_connections = EXPERIMENTAL_CONNECTIONS;
/* use alternate connection reading algorithm */
int alternate_connections = ALTERNATE_CONNECTIONS;
/* compute owl threats */
int owl_threats = OWL_THREATS; 
/* use experimental owl extension (GAIN/LOSS) */
int experimental_owl_ext = EXPERIMENTAL_OWL_EXT;
/* use experimental territory break-in module */
int experimental_break_in = USE_BREAK_IN;
/* use central oriented influence */
int cosmic_gnugo = COSMIC_GNUGO;
/* search for large scale owl moves */
int large_scale = LARGE_SCALE;

int capture_all_dead    = 0;    /* capture all dead opponent stones */
int play_out_aftermath  = 0;    /* make everything unconditionally settled */
int resign_allowed = RESIGNATION_ALLOWED; /* resign hopeless games */

int play_mirror_go      = 0;    /* try to play mirror go if possible */
int mirror_stones_limit = -1;   /* but stop at this number of stones */

int gtp_version         = 2;    /* Use GTP version 2 by default. */
int use_monte_carlo_genmove = 0; /* Default is not to use Monte Carlo move
				  * generation.
				  */
int mc_games_per_level = 8000;  /* By default, use 8000 times the current
				 * level number of simulations
				 * for each mmove when Monte Carlo
				 * move generation is enabled.
				 */

float best_move_values[10];
int   best_moves[10];
float white_score;
float black_score;

int close_worms[BOARDMAX][4];
int number_close_worms[BOARDMAX];
int close_black_worms[BOARDMAX][4];
int number_close_black_worms[BOARDMAX];
int close_white_worms[BOARDMAX][4];
int number_close_white_worms[BOARDMAX];

int false_eye_territory[BOARDMAX];
int forced_backfilling_moves[BOARDMAX];

struct worm_data      worm[BOARDMAX];
struct dragon_data    dragon[BOARDMAX];
int                   number_of_dragons;
struct dragon_data2   *dragon2 = NULL;
struct half_eye_data  half_eye[BOARDMAX];
struct eye_data       black_eye[BOARDMAX];
struct eye_data       white_eye[BOARDMAX];
struct vital_eye_points black_vital_points[BOARDMAX];
struct vital_eye_points white_vital_points[BOARDMAX];
struct surround_data  surroundings[MAX_SURROUND];
int                   surround_pointer;

int cutting_points[BOARDMAX];

double slowest_time = 0.0;
int    slowest_move = NO_MOVE;
int    slowest_movenum = 0;
double total_time = 0.0;


float minimum_value_weight  = 1.0;
float maximum_value_weight  = 1.0;
float invasion_malus_weight = 1.0;
float territorial_weight    = 1.0;
float strategical_weight    = 1.0;
float attack_dragon_weight  = 1.0;
float followup_weight       = 1.0;
