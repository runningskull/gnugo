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
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "liberty.h"

#include "gg-getopt.h"
#include "gg_utils.h"

#include "interface.h"
#include "sgftree.h"
#include "random.h"

static void show_copyright(void);
static void show_version(void);
static void show_help(void);
static void show_debug_help(void);
static void show_debug_flags(void);

/* long options which have no short form */
enum {OPT_BOARDSIZE=127,
      OPT_HANDICAPSTONES,
      OPT_COLOR,
      OPT_KOMI,
      OPT_CLOCK_TIME,
      OPT_CLOCK_BYO_TIME,
      OPT_CLOCK_BYO_PERIOD,
      OPT_AUTOLEVEL,
      OPT_MODE,
      OPT_INFILE,
      OPT_OUTFILE, 
      OPT_QUIET,
      OPT_GTP_INPUT,
      OPT_GTP_INITIAL_ORIENTATION,
      OPT_GTP_VERSION,
      OPT_SHOWCOPYRIGHT,
      OPT_REPLAY_GAME,
      OPT_DECIDE_STRING,
      OPT_DECIDE_CONNECTION,
      OPT_DECIDE_OWL,
      OPT_DECIDE_DRAGON_DATA,
      OPT_DECIDE_SEMEAI,
      OPT_DECIDE_SURROUNDED,
      OPT_DECIDE_TACTICAL_SEMEAI,
      OPT_EXPERIMENTAL_SEMEAI,
      OPT_EXPERIMENTAL_OWL_EXT,
      OPT_SEMEAI_VARIATIONS,
      OPT_EXPERIMENTAL_CONNECTIONS,
      OPT_EXPERIMENTAL_INFLUENCE,
      OPT_OPTIONS,
      OPT_STANDARD_SEMEAI,
      OPT_STANDARD_CONNECTIONS,
      OPT_STANDARD_INFLUENCE,
      OPT_DECIDE_POSITION,
      OPT_DECIDE_EYE,
      OPT_DECIDE_COMBINATION,
      OPT_BRANCH_DEPTH,
      OPT_BACKFILL2_DEPTH,
      OPT_SUPERSTRING_DEPTH,
      OPT_AA_DEPTH,
      OPT_DEBUG_FLAGS,
      OPT_OWL_DISTRUST,
      OPT_OWL_BRANCH,
      OPT_OWL_READING,
      OPT_OWL_NODE_LIMIT,
      OPT_NOFUSEKIDB,
      OPT_NOFUSEKI,
      OPT_NOJOSEKIDB,
      OPT_LEVEL,
      OPT_SHOWTIME,
      OPT_SHOWSCORE,
      OPT_DEBUG_INFLUENCE,
      OPT_SCORE,
      OPT_PRINTSGF,
      OPT_CHINESE_RULES,
      OPT_OWL_THREATS,
      OPT_NO_OWL_THREATS,
      OPT_JAPANESE_RULES,
      OPT_ALLOW_SUICIDE,
      OPT_CAPTURE_ALL_DEAD,
      OPT_PLAY_OUT_AFTERMATH,
      OPT_MIRROR,
      OPT_MIRROR_LIMIT
};

/* names of playing modes */

enum mode {
  MODE_UNKNOWN=0,
  MODE_ASCII,
  MODE_ASCII_EMACS,
  MODE_GTP,
  MODE_SGF,
  MODE_LOAD_AND_ANALYZE,
  MODE_LOAD_AND_SCORE,
  MODE_LOAD_AND_PRINT,
  MODE_SOLO,
  MODE_REPLAY,
  MODE_DECIDE_STRING,
  MODE_DECIDE_CONNECTION,
  MODE_DECIDE_OWL,
  MODE_DECIDE_DRAGON_DATA,
  MODE_DECIDE_SEMEAI,
  MODE_DECIDE_TACTICAL_SEMEAI,
  MODE_DECIDE_POSITION,
  MODE_DECIDE_EYE,
  MODE_DECIDE_COMBINATION,
  MODE_DECIDE_SURROUNDED
};


/* Definitions of the --long options. Final column is
 * either an OPT_ as defined in the enum above, or it
 * is the equivalent single-letter option.
 * It is useful to keep them in the same order as the
 * help string, for maintenance purposes only.
 */

static struct gg_option const long_options[] =
{
  {"mode",           required_argument, 0, OPT_MODE},
  {"replay",         required_argument, 0, OPT_REPLAY_GAME},
  {"quiet",          no_argument,       0, OPT_QUIET},
  {"silent",         no_argument,       0, OPT_QUIET},
  {"gtp-input",      required_argument, 0, OPT_GTP_INPUT},
  {"orientation",    required_argument, 0, OPT_GTP_INITIAL_ORIENTATION},
  {"gtp-initial-orientation",
  		     required_argument, 0, OPT_GTP_INITIAL_ORIENTATION},
  {"gtp-version",    required_argument, 0, OPT_GTP_VERSION},
  {"infile",         required_argument, 0, 'l'},
  {"until",          required_argument, 0, 'L'},
  {"outfile",        required_argument, 0, 'o'},
  {"output-flags",   required_argument, 0, 'O'},
  {"boardsize",      required_argument, 0, OPT_BOARDSIZE},
  {"color",          required_argument, 0, OPT_COLOR},
  {"handicap",       required_argument, 0, OPT_HANDICAPSTONES},
  {"komi",           required_argument, 0, OPT_KOMI},
  {"help",           optional_argument, 0, 'h'},
  {"copyright",      no_argument,       0, OPT_SHOWCOPYRIGHT},
  {"version",        no_argument,       0, 'v'},
  {"allpats",        no_argument,       0, 'a'},
  {"printboard",     no_argument,       0, 'T'},
  {"printeyes",      no_argument,       0, 'E'},
  {"debug",          required_argument, 0, 'd'},
  {"debug-flags",    no_argument,       0,  OPT_DEBUG_FLAGS},
  {"depth",          required_argument, 0, 'D'},
  {"backfill-depth", required_argument, 0, 'B'},
  {"branch-depth",   required_argument, 0, OPT_BRANCH_DEPTH},
  {"backfill2-depth",   required_argument, 0, OPT_BACKFILL2_DEPTH},
  {"superstring-depth", required_argument, 0, OPT_SUPERSTRING_DEPTH},
  {"fourlib-depth",  required_argument, 0, 'F'},
  {"ko-depth",       required_argument, 0, 'K'},
  {"aa-depth",       required_argument, 0, OPT_AA_DEPTH},
  {"owl-distrust",   required_argument, 0, OPT_OWL_DISTRUST},
  {"owl-branch",     required_argument, 0, OPT_OWL_BRANCH},
  {"owl-reading",    required_argument, 0, OPT_OWL_READING},
  {"owl-node-limit", required_argument, 0, OPT_OWL_NODE_LIMIT},
  {"level",          required_argument, 0, OPT_LEVEL},
  {"byo-time",       required_argument, 0, OPT_CLOCK_BYO_TIME},
  {"byo-period",     required_argument, 0, OPT_CLOCK_BYO_PERIOD},
  {"autolevel",      no_argument,       0, OPT_AUTOLEVEL},
  {"chinese-rules",  no_argument,       0, OPT_CHINESE_RULES},
  {"japanese-rules", no_argument,       0, OPT_JAPANESE_RULES},
  {"experimental-semeai",  no_argument, 0, OPT_EXPERIMENTAL_SEMEAI},
  {"experimental-owl-ext",  no_argument, 0, OPT_EXPERIMENTAL_OWL_EXT},
  {"semeai-variations",   required_argument, 0, OPT_SEMEAI_VARIATIONS},
  {"experimental-connections",  no_argument, 0, OPT_EXPERIMENTAL_CONNECTIONS},
  {"owl-threats",     no_argument,      0, OPT_OWL_THREATS},
  {"no-owl-threats",  no_argument,      0, OPT_NO_OWL_THREATS},
  {"standard-connections",  no_argument, 0, OPT_STANDARD_CONNECTIONS},
  {"standard-semeai", no_argument,      0, OPT_STANDARD_SEMEAI},
  {"options",        no_argument, 0, OPT_OPTIONS},
  {"allow-suicide",  no_argument,       0, OPT_ALLOW_SUICIDE},
  {"capture-all-dead",   no_argument,   0, OPT_CAPTURE_ALL_DEAD},
  {"play-out-aftermath", no_argument,   0, OPT_PLAY_OUT_AFTERMATH},
  {"hash",           required_argument, 0, 'H'},
  {"worms",          no_argument,       0, 'w'},
  {"moyo",           required_argument, 0, 'm'},
  {"benchmark",      required_argument, 0, 'b'},
  {"statistics",     no_argument,       0, 'S'},
  {"trace",          no_argument,       0, 't'},
  {"seed",           required_argument, 0, 'r'},
  {"decide-string",  required_argument, 0, OPT_DECIDE_STRING},
  {"decide-connection", required_argument, 0, OPT_DECIDE_CONNECTION},
  {"decide-dragon",  required_argument, 0, OPT_DECIDE_OWL},
  {"decide-owl",     required_argument, 0, OPT_DECIDE_OWL},
  {"decide-dragon-data",  required_argument, 0, OPT_DECIDE_DRAGON_DATA},
  {"decide-semeai",  required_argument, 0, OPT_DECIDE_SEMEAI},
  {"decide-tactical-semeai", required_argument, 0, OPT_DECIDE_TACTICAL_SEMEAI},
  {"decide-position", no_argument,      0, OPT_DECIDE_POSITION},
  {"decide-surrounded",  required_argument, 0, OPT_DECIDE_SURROUNDED},
  {"decide-eye",     required_argument, 0, OPT_DECIDE_EYE},
  {"decide-combination", no_argument,   0, OPT_DECIDE_COMBINATION},
  {"nofusekidb",     no_argument,       0, OPT_NOFUSEKIDB},
  {"nofuseki",       no_argument,       0, OPT_NOFUSEKI},
  {"nojosekidb",     no_argument,       0, OPT_NOJOSEKIDB},
  {"debug-influence", required_argument, 0, OPT_DEBUG_INFLUENCE},
  {"showtime",       no_argument,       0, OPT_SHOWTIME},
  {"showscore",      no_argument,       0, OPT_SHOWSCORE},
  {"score",          required_argument, 0, OPT_SCORE},
  {"printsgf",       required_argument, 0, OPT_PRINTSGF},
  {"mirror",         no_argument,       0, OPT_MIRROR},
  {"mirror-limit",   required_argument, 0, OPT_MIRROR_LIMIT},
  {NULL, 0, NULL, 0}
};


int
main(int argc, char *argv[])
{
  Gameinfo gameinfo;
  SGFTree sgftree;

  int i;
  int mandated_color = EMPTY;
  enum mode playmode = MODE_UNKNOWN;
  int replay_color = EMPTY;
  
  char *infilename = NULL;
  char *untilstring = NULL;
  char *scoringmode = NULL;
  char *outfile = NULL;
  char *outflags = NULL;
  char *gtpfile = NULL;
  
  char *printsgffile = NULL;
  
  char decide_this[8];
  char *decide_that = NULL;
  char debuginfluence_move[4] = "\0";
  
  int benchmark = 0;  /* benchmarking mode (-b) */
  FILE *gtp_input_FILE, *output_check;
  int orientation = 0;

  /* If seed is zero, GNU Go will play a different game each time. If
   * it is set using -r, GNU Go will play the same game each time.
   * (Change seed to get a different game).
   */
  int seed = 0;

  komi = 0.0;
  
  level = DEFAULT_LEVEL;
  semeai_variations = DEFAULT_SEMEAI_VARIATIONS;

  mandated_depth               = -1;
  mandated_backfill_depth      = -1;
  mandated_backfill2_depth     = -1;
  mandated_superstring_depth   = -1;
  mandated_fourlib_depth       = -1;
  mandated_ko_depth            = -1;
  mandated_branch_depth        = -1;
  mandated_owl_distrust_depth  = -1;
  mandated_owl_branch_depth    = -1;
  mandated_owl_reading_depth   = -1;
  mandated_owl_node_limit      = -1;
  mandated_aa_depth            = -1;

  debug = 0;
  fusekidb = 1;
  disable_fuseki = 0;
  josekidb = 1;
  if (CHINESE_RULES)
    chinese_rules = 1;
  else 
    chinese_rules = 0;
  if (OWL_THREATS)
    owl_threats = 1;
  else
    owl_threats = 0;
  experimental_owl_ext = EXPERIMENTAL_OWL_EXT;
  experimental_semeai = EXPERIMENTAL_SEMEAI;
  experimental_connections = EXPERIMENTAL_CONNECTIONS;

  allow_suicide = 0;
  capture_all_dead = 0;
  play_out_aftermath = 0;

  sgftree_clear(&sgftree);
  gameinfo_clear(&gameinfo, board_size, komi);
  
  /* Weed through all of the command line options. */
  while ((i = gg_getopt_long(argc, argv, 
                            "-ab:B:d:D:EF:gh::H:K:l:L:M:m:o:O:p:r:fsStTvw",
			     long_options, NULL)) != EOF)
    {
      switch (i) {
      case 'T': printboard++; break;
      case 't': ++verbose; break;
      case 'a': allpats = 1; break;

      case  1 :
      case 'l': infilename = gg_optarg; 
	break;
	
      case 'b': benchmark = atoi(gg_optarg); playmode = MODE_SOLO; break;
      case 'r': seed = atoi(gg_optarg); break;
      case 'S': showstatistics = 1; break;
      case 'w': printworms = 1; break;
      case 'm': printmoyo = strtol(gg_optarg, NULL, 0);  /* allows 0x... */ break;
      case 'd': debug ^= strtol(gg_optarg, NULL, 0);  /* allows 0x... */ break;
      case 'D': mandated_depth = atoi(gg_optarg); break;

      case 'H': hashflags = strtol(gg_optarg, NULL, 0);  /* allows 0x... */ break;

      case 'E': printboard = 2; break;
      case 'B': mandated_backfill_depth = atoi(gg_optarg); break;
      case 'F': mandated_fourlib_depth = atoi(gg_optarg); break;
      case 'K': mandated_ko_depth = atoi(gg_optarg); break;

      case 'L':
	untilstring = gg_optarg;
	break;
	
      case 'o':
	outfile = gg_optarg;
	strcpy(outfilename, gg_optarg);
	break;

      case 'O':
	outflags = gg_optarg;
	output_flags = 0;
	if (outflags)
	  while (*outflags){
	    switch (*outflags) {
	    case 'd':
	      output_flags |= OUTPUT_MARKDRAGONS;
	      break;
	    case 'v':
	      output_flags |= OUTPUT_MOVEVALUES;
	      break;
	    }
	    outflags++;
	  }
	break;
	
      case OPT_QUIET:
	quiet = 1;
	break;
	
      case OPT_GTP_INPUT:
	gtpfile = gg_optarg;
	break;
	
      case OPT_GTP_INITIAL_ORIENTATION:
	orientation = atoi(gg_optarg);
	if (orientation < 0 || orientation > 7) {
	  fprintf(stderr, "Illegal orientation: %d.\n", orientation);
	  fprintf(stderr, "Try `gnugo --help' for more information.\n");
	  exit(EXIT_FAILURE);
	}
	break;
	
      case OPT_GTP_VERSION:
	gtp_version = atoi(gg_optarg);
	break;
	
      case OPT_OPTIONS:
	if (EXPERIMENTAL_CONNECTIONS)
	  fprintf(stderr,
		  "configure option enabled: experimental connections\n");
	if (EXPERIMENTAL_SEMEAI)
	  fprintf(stderr,
		  "configure option enabled: experimental semeai\n");
	if (EXPERIMENTAL_OWL_EXT)
	  fprintf(stderr,
		  "configure option enabled: experimental GAIN/LOSS codes\n");
	if (OWL_THREATS)
	  fprintf(stderr,
		  "configure option enabled: owl threats\n");
	fprintf(stderr,
		"Owl node limit: %d\n", OWL_NODE_LIMIT);

	return EXIT_SUCCESS;
	break;

      case OPT_SHOWTIME:
	showtime = 1;
	break;
	
      case OPT_SHOWSCORE:
	showscore = 1;
	break;
	
      case OPT_HANDICAPSTONES:
	{
	  int handicap = atoi(gg_optarg);
	  
	  if (handicap < 0 || handicap > MAX_HANDICAP) {
	    fprintf(stderr, "Illegal handicap: %d.\n", handicap);
	    fprintf(stderr, "Try `gnugo --help' for more information.\n");
	    exit(EXIT_FAILURE);
	  }
	  gameinfo.handicap = handicap;
	}
        break;
      
      case OPT_BOARDSIZE:
        {
	  int boardsize = atoi(gg_optarg);
	  
	  if (boardsize < MIN_BOARD || boardsize > MAX_BOARD) {
	    fprintf(stderr, "Illegal board size: %d.\n", boardsize);
	    fprintf(stderr, "Try `gnugo --help' for more information.\n");
	    exit(EXIT_FAILURE);
	  }
	  gnugo_clear_board(boardsize);
	  break;
	}
	
      case OPT_KOMI: 
	if (sscanf(gg_optarg, "%f", &komi) != 1) {
	  fprintf(stderr, "Invalid komi selection: %s\n", gg_optarg);
	  fprintf(stderr, "Try `gnugo --help' for more information.\n");
	  exit(EXIT_FAILURE);
	}
	break;
	
      case OPT_CHINESE_RULES: 
	chinese_rules = 1;
	break;

      case OPT_OWL_THREATS: 
	owl_threats = 1;
	break;

      case OPT_NO_OWL_THREATS: 
	owl_threats = 0;
	break;

      case OPT_JAPANESE_RULES: 
	chinese_rules = 0;
	break;

      case OPT_EXPERIMENTAL_OWL_EXT:
	experimental_owl_ext = 1;
	break;

      case OPT_EXPERIMENTAL_SEMEAI:
	experimental_semeai = 1;
	break;

      case OPT_SEMEAI_VARIATIONS:
	semeai_variations = atoi(gg_optarg);
	break;

      case OPT_STANDARD_SEMEAI: 
	experimental_semeai = 0;
	break;

      case OPT_EXPERIMENTAL_CONNECTIONS:
	experimental_connections = 1;
	break;

      case OPT_STANDARD_CONNECTIONS: 
	experimental_connections = 0;
	break;

      case OPT_ALLOW_SUICIDE:
	allow_suicide = 1;
	break;

      case OPT_CAPTURE_ALL_DEAD:
	capture_all_dead = 1;
	break;

      case OPT_PLAY_OUT_AFTERMATH:
	play_out_aftermath = 1;
	break;

      case OPT_MODE: 
	if (strcmp(gg_optarg, "ascii") == 0) 
	  playmode = MODE_ASCII;
	else if (strcmp(gg_optarg, "emacs") == 0)
	  playmode = MODE_ASCII_EMACS;
	else if (strcmp(gg_optarg, "gtp") == 0)
	  playmode = MODE_GTP;
	else {
	  fprintf(stderr, "Invalid mode selection: %s\n", gg_optarg);
	  fprintf(stderr, "Try `gnugo --help' for more information.\n");
	  
	  exit(EXIT_FAILURE);
	}
	break;
	
      case OPT_DECIDE_STRING:
	if (strlen(gg_optarg) > 3) {
	  fprintf(stderr, "Invalid board coordinate: %s\n", gg_optarg);
	  exit(EXIT_FAILURE);
	}
	strcpy(decide_this, gg_optarg);
	playmode = MODE_DECIDE_STRING;
	break;
	
      case OPT_DECIDE_CONNECTION:
	if (strlen(gg_optarg) > 7) {
	  fprintf(stderr, 
		  "usage: --decide-connection [first string]/[second string]\n");
	  return EXIT_FAILURE;
	}
	strcpy(decide_this, gg_optarg);
	strtok(decide_this, "/");
	decide_that = strtok(NULL, "/");
	if (!decide_that) {
	  fprintf(stderr, 
		  "usage: --decide-connection [first string]/[second string]\n");
	  return EXIT_FAILURE;
	}

	playmode = MODE_DECIDE_CONNECTION;
	break;
	
      case OPT_DECIDE_OWL:
	if (strlen(gg_optarg) > 3) {
	  fprintf(stderr, "Invalid board coordinate: %s\n", gg_optarg);
	  exit(EXIT_FAILURE);
	}
	strcpy(decide_this, gg_optarg);
	playmode = MODE_DECIDE_OWL;
	break;
	
      case OPT_DECIDE_DRAGON_DATA:
	if (strlen(gg_optarg) > 3) {
	  fprintf(stderr, "Invalid board coordinate: %s\n", gg_optarg);
	  exit(EXIT_FAILURE);
	}
	strcpy(decide_this, gg_optarg);
	playmode = MODE_DECIDE_DRAGON_DATA;
	break;
	
      case OPT_DECIDE_SEMEAI:
	if (strlen(gg_optarg) > 7) {
	  fprintf(stderr, 
		  "usage: --decide-semeai [first dragon]/[second dragon]\n");
	  return EXIT_FAILURE;
	}
	strcpy(decide_this, gg_optarg);
	strtok(decide_this, "/");
	decide_that = strtok(NULL, "/");
	if (!decide_that) {
	  fprintf(stderr, 
		  "usage: --decide-semeai [first dragon]/[second dragon]\n");
	  return EXIT_FAILURE;
	}

	playmode = MODE_DECIDE_SEMEAI;
	break;
	
      case OPT_DECIDE_TACTICAL_SEMEAI:
	if (strlen(gg_optarg) > 7) {
	  fprintf(stderr, 
		  "usage: --decide-tactical-semeai [first dragon]/[second dragon]\n");
	  return EXIT_FAILURE;
	}
	strcpy(decide_this, gg_optarg);
	strtok(decide_this, "/");
	decide_that = strtok(NULL, "/");
	if (!decide_that) {
	  fprintf(stderr, 
		  "usage: --decide-tactical-semeai [first dragon]/[second dragon]\n");
	  return EXIT_FAILURE;
	}
	playmode = MODE_DECIDE_TACTICAL_SEMEAI;
	break;
	
      case OPT_DECIDE_POSITION:
	playmode = MODE_DECIDE_POSITION;
	break;
	
      case OPT_DECIDE_EYE:
	if (strlen(gg_optarg) > 3) {
	  fprintf(stderr, "Invalid board coordinate: %s\n", gg_optarg);
	  exit(EXIT_FAILURE);
	}
	strcpy(decide_this, gg_optarg);
	playmode = MODE_DECIDE_EYE;
	break;
	
      case OPT_DECIDE_COMBINATION:
	playmode = MODE_DECIDE_COMBINATION;
	break;
	
      case OPT_DECIDE_SURROUNDED:
	if (strlen(gg_optarg) > 3) {
	  fprintf(stderr, "Invalid board coordinate: %s\n", gg_optarg);
	  exit(EXIT_FAILURE);
	}
	strcpy(decide_this, gg_optarg);
	playmode = MODE_DECIDE_SURROUNDED;
	break;
	
      case OPT_BRANCH_DEPTH:
	mandated_branch_depth = atoi(gg_optarg);
	break;
	
      case OPT_BACKFILL2_DEPTH:
	mandated_backfill2_depth = atoi(gg_optarg);
	break;
	
      case OPT_SUPERSTRING_DEPTH:
	mandated_superstring_depth = atoi(gg_optarg);
	break;
	
      case OPT_AA_DEPTH:
	mandated_aa_depth = atoi(gg_optarg);

      case OPT_OWL_DISTRUST:
	mandated_owl_distrust_depth = atoi(gg_optarg);
	break;
	
      case OPT_OWL_BRANCH:
	mandated_owl_branch_depth = atoi(gg_optarg);
	break;
	
      case OPT_OWL_READING:
	mandated_owl_reading_depth = atoi(gg_optarg);
	break;
	
      case OPT_OWL_NODE_LIMIT:
	mandated_owl_node_limit = atoi(gg_optarg);
	break;
	
      case OPT_NOFUSEKIDB:
	fusekidb = 0;
	break;
	
      case OPT_NOFUSEKI:
	disable_fuseki = 1;
	break;
	
      case OPT_NOJOSEKIDB:
	josekidb = 0;
	break;
	
      case OPT_LEVEL:
	level = atoi(gg_optarg);
	break;

      case OPT_DEBUG_INFLUENCE:
	if (strlen(gg_optarg) > 3) {
	  fprintf(stderr, "Invalid board coordinate: %s\n", gg_optarg);
	  exit(EXIT_FAILURE);
	}
	strcpy(debuginfluence_move, gg_optarg);
        break;
	
      case OPT_REPLAY_GAME: 
	playmode = MODE_REPLAY;
	if (strcmp(gg_optarg, "white") == 0)
	  replay_color = WHITE;
	else if (strcmp(gg_optarg, "black") == 0)
	  replay_color = BLACK;
	else if (strcmp(gg_optarg, "both") == 0)
	  replay_color = GRAY;
	else {
	  fprintf(stderr, "Invalid replay color: %s\n", gg_optarg);
	  fprintf(stderr, "Try `gnugo --help' for more information.\n");
	  exit(EXIT_FAILURE);
	}
	break;
	
      case OPT_SCORE:
	scoringmode = gg_optarg;
	if (playmode == MODE_UNKNOWN)
	  playmode = MODE_LOAD_AND_SCORE;
	break;
	
      case OPT_PRINTSGF:
	playmode = MODE_LOAD_AND_PRINT;
	printsgffile = gg_optarg;
	break;
	
      case OPT_COLOR: 
	if (strcmp(gg_optarg, "white") == 0)
	  mandated_color = WHITE;
	else if (strcmp(gg_optarg, "black") == 0)
	  mandated_color = BLACK;
	else {
	  fprintf(stderr, "Invalid color selection: %s\n", gg_optarg);
	  fprintf(stderr, "Try `gnugo --help' for more information.\n");
	  exit(EXIT_FAILURE);
	}
	break;
	
      case OPT_SHOWCOPYRIGHT: 
	show_copyright();
	return EXIT_SUCCESS;
	break;
	
      case OPT_MIRROR:
        play_mirror_go = 1;
        break;

      case OPT_MIRROR_LIMIT:
        mirror_stones_limit = atoi(gg_optarg);
        break;

      case 'v':
	show_version();
	return EXIT_SUCCESS;
	break;
	
      case 'h': 
	show_version();
	if (gg_optarg) {
	  /* In the default behavior of getopt_long with optional args
	   * you need to type "-hdebug"
	   * I can't get long options "--helpdebug" to work at all
	   */
	  if (strncmp(gg_optarg, "debug", 5) == 0)
	    show_debug_help();
	}
	else {
	  /* This is the trick to get "--help debug" and "-h debug" to work*/
	  if (gg_optind < argc) {
	    if (strncmp(argv[gg_optind], "debug", 5) == 0)
	      show_debug_help();
	  }
	  else
	    show_help();
	}
	return EXIT_SUCCESS;
	break;
	
      case OPT_DEBUG_FLAGS:
	show_debug_flags();
	return EXIT_SUCCESS;
	break;

	/* NOTE: getopt returns '?' if an illegal option is supplied. */
      case '?':
      default:
	fprintf(stderr, "Try `gnugo --help' for more information.\n");
	exit(EXIT_FAILURE);
      }
    }

  
  /* Figure out a default mode if there was no explicit one. */
  if (playmode == MODE_UNKNOWN) {
    if (infilename)
      playmode = MODE_LOAD_AND_ANALYZE;
    else
      playmode = MODE_GTP;
  }

  /* Display copyright message unless --quiet option used. */
  if (!quiet && playmode != MODE_GTP) {
    fprintf(stderr, "\n");
    show_version();
    show_copyright();
  }
  
  /* Start random number seed. */
  if (!seed)
    seed = 1;
  gg_srand(seed);
  random_seed = seed;

  
  /* Initialize the GNU Go engine. */
  init_gnugo();

  
  /* Read the infile if there is one. Also play up the position. */
  if (infilename) {
    if (!sgftree_readfile(&sgftree, infilename)) {
      fprintf(stderr, "Cannot open or parse '%s'\n", infilename);
      exit(EXIT_FAILURE);
    }
    
    gameinfo_play_sgftree_rot(&gameinfo, &sgftree, untilstring,
                             orientation);
  }
  else
  /* Initialize and empty sgf tree if there was no infile. */
    sgftreeCreateHeaderNode(&sgftree, board_size, komi);

  /* Set the game_record to be identical to the loaded one or the
   * newly created empty sgf tree.
   */
  gameinfo.game_record = sgftree;
  
  /* Notice that we need to know the board size before we can do this.
   */
  if (debuginfluence_move[0]) {
    int m, n;
    string_to_location(board_size, debuginfluence_move, &m, &n);
    debug_influence_move(m, n);
  }

  if (outfile && playmode != MODE_LOAD_AND_PRINT) {
    output_check = fopen(outfile, "w");
    if (!output_check) {
      fprintf(stderr, "Error: could not open '%s' for writing\n", outfile);
      exit(EXIT_FAILURE);
    }
    fclose(output_check);
  }
  
  switch (playmode) {
  case MODE_SOLO:
    play_solo(&gameinfo, benchmark);
    break;
    
  case MODE_REPLAY:    
    if (!infilename) {
      fprintf(stderr, "You must use -l infile with replay mode.\n");
      exit(EXIT_FAILURE);
    }
    play_replay(&gameinfo, replay_color);
    break;
    
  case MODE_LOAD_AND_ANALYZE:
    if (mandated_color != EMPTY)
      gameinfo.to_move = mandated_color;
    
    if (!infilename) {
      fprintf(stderr, "You must use -l infile with load and analyze mode.\n");
      exit(EXIT_FAILURE);
    }
    load_and_analyze_sgf_file(&gameinfo);
    break;
    
  case MODE_LOAD_AND_SCORE:
    if (mandated_color != EMPTY)
      gameinfo.to_move = mandated_color;

    if (!infilename) {
      fprintf(stderr, "gnugo: --score must be used with -l\n");
      exit(EXIT_FAILURE);
    }
    load_and_score_sgf_file(&sgftree, &gameinfo, scoringmode);
    break;
    
  case MODE_LOAD_AND_PRINT:
    if (!infilename) {
      fprintf(stderr, "gnugo: --printsgf must be used with -l\n");
      exit(EXIT_FAILURE);
    }

    else {
      if (mandated_color != EMPTY)
        gameinfo.to_move = mandated_color;

      sgffile_printsgf(gameinfo.to_move, printsgffile);
    }
    break;
    
  case MODE_DECIDE_STRING:
    {
      int m, n;
      
      if (!infilename) {
	fprintf(stderr, "gnugo: --decide-string must be used with -l\n");
	return EXIT_FAILURE;
      }
      
      if (!string_to_location(board_size, decide_this, &m, &n)) {
	fprintf(stderr, "gnugo: --decide-string: strange coordinate \n");
	return EXIT_FAILURE;
      }

      rotate(m, n, &m, &n, board_size, orientation);
      decide_string(POS(m, n));
    }
    break;
  
  case MODE_DECIDE_CONNECTION:
    {
      int ai, aj, bi, bj;
      
      if (!infilename) {
	fprintf(stderr, "gnugo: --decide-connection must be used with -l\n");
	return EXIT_FAILURE;
      }
      
      if (!string_to_location(board_size, decide_this, &ai, &aj)) {
	fprintf(stderr, "usage: --decide-connection [first string]/[second string]\n");
	return EXIT_FAILURE;
      }
      
      if (!string_to_location(board_size, decide_that, &bi, &bj)) {
	fprintf(stderr, "usage: --decide-connection [first string]/[second string]\n");
	return EXIT_FAILURE;
      }

      rotate(ai, aj, &ai, &aj, board_size, orientation);
      rotate(bi, bj, &bi, &bj, board_size, orientation);
      decide_connection(POS(ai, aj), POS(bi, bj));
    }
  break;
  
  case MODE_DECIDE_OWL:
    {
      int m, n;
      
      if (!infilename) {
	fprintf(stderr, "gnugo: --decide-dragon must be used with -l\n");
	return EXIT_FAILURE;
      }
      
      if (!string_to_location(board_size, decide_this, &m, &n)) {
	fprintf(stderr, "gnugo: --decide-dragon: strange coordinate \n");
	return EXIT_FAILURE;
      }

      rotate(m, n, &m, &n, board_size, orientation);
      decide_owl(POS(m, n));
    }
    break;
  
  case MODE_DECIDE_DRAGON_DATA:
    {
      int m, n;
      
      if (!infilename) {
	fprintf(stderr, "gnugo: --decide-dragon-data must be used with -l\n");
	return EXIT_FAILURE;
      }
      
      if (!string_to_location(board_size, decide_this, &m, &n)) {
	fprintf(stderr, "gnugo: --decide-dragon-data: strange coordinate \n");
	return EXIT_FAILURE;
      }

      rotate(m, n, &m, &n, board_size, orientation);
      decide_dragon_data(POS(m, n));
    }
    break;
  
  case MODE_DECIDE_SEMEAI:
    {
      int ai, aj, bi, bj;
      
      if (!infilename) {
	fprintf(stderr, "gnugo: --decide-semeai must be used with -l\n");
	return EXIT_FAILURE;
      }
      
      if (!string_to_location(board_size, decide_this, &ai, &aj)) {
	fprintf(stderr, 
		"usage: --decide-semeai [first dragon]/[second dragon]\n");
	return EXIT_FAILURE;
      }
      
      if (!string_to_location(board_size, decide_that, &bi, &bj)) {
	fprintf(stderr, 
		"usage: --decide-semeai [first dragon]/[second dragon]\n");
	return EXIT_FAILURE;
      }

      rotate(ai, aj, &ai, &aj, board_size, orientation);
      rotate(bi, bj, &bi, &bj, board_size, orientation);
      decide_semeai(POS(ai, aj), POS(bi, bj));
    }
    break;
    

  case MODE_DECIDE_TACTICAL_SEMEAI:
    {
      int ai, aj, bi, bj;
      
      if (!infilename) {
	fprintf(stderr, "gnugo: --decide-tactical-semeai must be used with -l\n");
	return EXIT_FAILURE;
      }
      
      if (!string_to_location(board_size, decide_this, &ai, &aj)) {
	fprintf(stderr, 
		"usage: --decide-tactical-semeai [first dragon]/[second dragon]\n");
	return EXIT_FAILURE;
      }
      
      if (!string_to_location(board_size, decide_that, &bi, &bj)) {
	fprintf(stderr, 
		"usage: --decide-tactical-semeai [first dragon]/[second dragon]\n");
	return EXIT_FAILURE;
      }

      rotate(ai, aj, &ai, &aj, board_size, orientation);
      rotate(bi, bj, &bi, &bj, board_size, orientation);
      decide_tactical_semeai(POS(ai, aj), POS(bi, bj));
    }
    break;
    

  case MODE_DECIDE_POSITION:
    {
      int color;
      if (!infilename) {
	fprintf(stderr, "gnugo: --decide-position must be used with -l\n");
	return EXIT_FAILURE;
      }
      color = gameinfo.to_move;
      if (mandated_color != EMPTY)
	color = mandated_color;
      decide_position(color);
    }
    break;
    
  case MODE_DECIDE_EYE:
    {
      int m, n;
      
      if (!infilename) {
	fprintf(stderr, "gnugo: --decide-eye must be used with -l\n");
	return EXIT_FAILURE;
      }
      
      if (!string_to_location(board_size, decide_this, &m, &n)) {
	fprintf(stderr, "gnugo: --decide-eye: strange coordinate \n");
	return EXIT_FAILURE;
      }
      
      rotate(m, n, &m, &n, board_size, orientation);
      decide_eye(POS(m, n));
    }
    break;
  
  case MODE_DECIDE_COMBINATION:
    {
      int color;
      if (!infilename) {
	fprintf(stderr, "gnugo: --decide-combination must be used with -l\n");
	return EXIT_FAILURE;
      }
      color = gameinfo.to_move;
      if (mandated_color != EMPTY)
	color = mandated_color;
      decide_combination(color);
    }
    break;
    
  case MODE_DECIDE_SURROUNDED:
    {
      int m, n;

      if (!string_to_location(board_size, decide_this, &m, &n)) {
	fprintf(stderr, 
		"usage: --decide-surrounded [pos]\n");
	return EXIT_FAILURE;
      }

      rotate(m, n, &m, &n, board_size, orientation);
      decide_surrounded(POS(m, n));
      break;
    }

  case MODE_GTP:  
    if (gtpfile != NULL) {
      gtp_input_FILE = fopen(gtpfile, "r");
      if (gtp_input_FILE == NULL) {
	fprintf(stderr, "gnugo: Cannot open file %s\n", gtpfile);
	return EXIT_FAILURE;
      } 
    }
    else
      gtp_input_FILE = stdin;

    play_gtp(gtp_input_FILE, orientation);
    break;

  case MODE_ASCII_EMACS:  
    if (mandated_color != EMPTY)
      gameinfo.computer_player = OTHER_COLOR(mandated_color);
    
    play_ascii_emacs(&sgftree, &gameinfo, infilename, untilstring);
    break;

  case MODE_ASCII:  
  default:     
    if (mandated_color != EMPTY)
      gameinfo.computer_player = OTHER_COLOR(mandated_color);
    
    play_ascii(&sgftree, &gameinfo, infilename, untilstring);
    break;
  }
  
  return 0;
}  /* end main */



static void
show_version(void)
{
  fprintf(stderr, "GNU Go Version %s\n", VERSION);
}


/* Set the parameters which determine the depth to which
 * the reading and owl code carries its calculations.
 */



/* 
 * This string is modelled after the GNU tar --help output.
 * Since the maximum string length is 2048 bytes in VC++ we
 * split the help string.
 */


#define USAGE "\n\
Usage: gnugo [-opts]\n\
\n\
Main Options:\n\
       --mode <mode>     Force the playing mode ('ascii' or 'gtp').\n\
                         Default is GTP.\n\
       --quiet           Don't print copyright and informational messages\n\
       --gtp-input <file>Read gtp commands from file instead of stdin\n\
   -l, --infile <file>   Load name sgf file\n\
   -L, --until <move>    Stop loading just before move is played. <move>\n\
                         can be the move number or location (eg L10).\n\
   -o, --outfile <file>  Write sgf output to file\n\
   --printsgf <file>     Write position as a diagram to file (use with -l)\n\
\n\
Options that affect strength (higher = stronger, slower):\n\
   -D, --depth <depth>          deep reading cutoff (default %d)\n\
   -B, --backfill-depth <depth> deep reading cutoff (default %d)\n\
   -F, --fourlib-depth <depth>  deep reading cutoff (default %d)\n\
   -K, --ko-depth <depth>       deep reading cutoff (default %d)\n\
   --branch-depth <depth>       deep reading cutoff (default %d)\n\
   --backfill2-depth <depth>    deep reading cutoff (default %d)\n\
   --superstring-depth <depth>  deep reading cutoff (default %d)\n\
   --aa-depth <depth>           deep reading cutoff (default %d)\n\
   --owl-distrust <depth>       owl distrust depth (default %d)\n\
   --owl-branch <depth>         owl branching depth (default %d)\n\
   --owl-reading <depth>        owl reading depth (default %d)\n\
   --owl-node-limit <limit>     max nodes for owl reading (default %d)\n\
   --level <amount>             strength (default %d, up to 10 supported)\n\
"

#define USAGE1 "\n\
Experimental options:\n\
   --nofusekidb            turn off fuseki database\n\
   --nofuseki              turn off fuseki moves entirely\n\
   --nojosekidb            turn off joseki database\n\
   --mirror                try to play mirror go\n\
   --mirror-limit <n>      stop mirroring when n stones on board\n\
Scoring:\n\
   --score estimate        estimate score at loaded position\n\
   --score finish          generate moves to finish game, then score\n\
   --score aftermath       generate moves to finish, use best algorithm\n\
   --score aftermath --capture-all-dead --chinese-rules   Tromp-Taylor score\n\
\n\
Game Options: (--mode ascii)\n\
       --boardsize num   Set the board size to use (%d--%d)\n\
       --color <color>   Choose your color ('black' or 'white')\n\
       --handicap <num>  Set the number of handicap stones (0--%d)\n\
       --komi <num>      Set the komi\n\
\n\
Informative Output:\n\
   -v, --version         Display the version of GNU Go\n\
   --options             Display configure options\n\
   -h, --help            Display this help message\n\
       --help debug      Display help about debugging options\n\
       --copyright       Display copyright notice\n\
\n\
"


#define COPYRIGHT "\n\n\
This is GNU GO, a Go program. Contact gnugo@gnu.org, or see\n\
http://www.gnu.org/software/gnugo/ for more information.\n\n\
Copyright 1999, 2000, 2001, 2002 by the Free Software Foundation.\n\n\
This program is free software; you can redistribute it and/or\n\
modify it under the terms of the GNU General Public License\n\
as published by the Free Software Foundation - version 2.\n\n\
This program is distributed in the hope that it will be\n\
useful, but WITHOUT ANY WARRANTY; without even the implied\n\
warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR\n\
PURPOSE.  See the GNU General Public License in file COPYING\n\
for more details.\n\n\
You should have received a copy of the GNU General Public\n\
License along with this program; if not, write to the Free\n\
Software Foundation, Inc., 59 Temple Place - Suite 330,\n\
Boston, MA 02111, USA.\n\n\
"

/* USAGE_DEBUG Split in half because of VC limit on constant string 
 * length of 2048 characters!*/
#define USAGE_DEBUG "\n\
Debugging Options:\n\
\n\
       --replay <color> replay the moves in a game for color.\n\
                        (requires -l)\n\
                     white: replay only white moves\n\
                     black: replay only black moves\n\
                     both:  replay all moves\n\
   -a, --allpats                 test all patterns\n\
   -T, --printboard              colored display of dragons\n\
   -E, --printeyes               colored display of eye spaces\n\
   -d, --debug <flags>           debugging output (see next item for bits)\n\
   --debug-flags                 print the debug flags for previous item\n\
   -H, --hash <level>            hash (see gnugo.h for bits)\n\
   -w, --worms                   print worm and dragon data and move reasons\n\
   -m, --moyo <level>            moyo debugging, show moyo board\n\
   -b, --benchmark num           benchmarking mode - can be used with -l\n\
   -S, --statistics              print statistics (for debugging purposes)\n\n\
   -t, --trace                   verbose tracing\n\
   -O, --output-flags <flags>    optional output (use with -o)\n\
                    d: mark dead and critical dragons\n\
                    v: show values of considered moves\n\
                    specify either 'd', 'v' or 'dv' (nothing by default)\n\
   --showtime                    print timing diagnostic\n\
   --replay <color>              replay game. Use with -o.\n\
   --showscore                   print estimated score\n\
   -r, --seed number             set random number seed\n\
"
#define USAGE_DEBUG2 "\
       --decide-string <string>  can this string live? (try with -o)\n\
       --decide-connection <str/str> can these strings connect? (try with -o)\n\
       --decide-dragon <dragon>  can this dragon live? (try with -o or -t)\n\
       --decide-position         evaluate all dragons (try with -o or -t)\n\
       --decide-eye <string>     evaluate the eye\n\
       --decide-combination      search for combination attack (try with -o)\n\
       --genmove <color>         generate a move for color\n\
       --nofusekidb              turn off fuseki database\n\
       --nofuseki                turn off fuseki moves entirely\n\
       --nojosekidb              turn off joseki database\n\
       --debuginfluence <move>   print influence map after making a move\n\
       --score <end|last|move>   count or estimate territory\n\
       --profile-patterns        print statistics for pattern usage\n\
       --attack-by-pattern       use pattern-based tactical reading for attack\n\
       --defend-by-pattern       use pattern-based tactical reading for defense\n\
"

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
DEBUG_LIFE                  0x2000\n\
DEBUG_FILLLIB               0x4000\n\
DEBUG_READING_PERFORMANCE   0x8000\n\
DEBUG_SCORING               0x010000\n\
DEBUG_AFTERMATH             0x020000\n\
DEBUG_ATARI_ATARI           0x040000\n\
DEBUG_READING_CACHE         0x080000\n\
DEBUG_TERRITORY             0x100000\n\
DEBUG_OWL_PERSISTENT_CACHE  0X200000\n\
DEBUG_TOP_MOVES             0x400000\n\
DEBUG_MISCELLANEOUS         0x800000\n\
"

/*
 * Since the maximum string length is 2048 bytes in VC++ we
 * split the help string.
 */
static void
show_help(void)
{
  set_depth_values(DEFAULT_LEVEL);
  fprintf(stderr, USAGE,
	  depth, backfill_depth, fourlib_depth, ko_depth, branch_depth,
	  backfill2_depth, superstring_depth, aa_depth, 
	  owl_distrust_depth, owl_branch_depth,
	  owl_reading_depth, owl_node_limit, DEFAULT_LEVEL);
  fprintf(stderr, USAGE1, MIN_BOARD, MAX_BOARD, MAX_HANDICAP);
}


static void
show_debug_help(void)
{
  fprintf(stderr, USAGE_DEBUG USAGE_DEBUG2);
}

static void 
show_debug_flags(void)
{
  fprintf(stderr, DEBUG_FLAGS);
}

static void
show_copyright(void)
{
  fputs(COPYRIGHT, stderr);
}




/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
