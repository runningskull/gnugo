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

#include "gnugo.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#ifdef HAVE_UNISTD_H
/* For isatty(). */
#include <unistd.h>
#else
#include <io.h>
#endif

#if TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# if HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  include <time.h>
# endif
#endif

#include "liberty.h"

#include "gg-getopt.h"
#include "gg_utils.h"
#include "winsocket.h"

#include "interface.h"
#include "sgftree.h"
#include "random.h"

static void show_copyright(void);
static void show_version(void);
static void show_help(void);
static void show_debug_help(void);
static void show_debug_flags(void);

static void socket_connect_to(const char *host_name, unsigned int port,
			      FILE **input_file, FILE **output_file);
static void socket_listen_at(const char *host_name, unsigned int port,
			     FILE **input_file, FILE **output_file);
static void socket_close_connection(FILE *input_file, FILE *output_file);
static void socket_stop_listening(FILE *input_file, FILE *output_file);


/* long options which have no short form */
enum {OPT_BOARDSIZE = 127,
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
      OPT_GTP_CONNECT,
      OPT_GTP_LISTEN,
      OPT_GTP_DUMP_COMMANDS,
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
      OPT_DECIDE_ORACLE,
      OPT_EXPERIMENTAL_SEMEAI,
      OPT_EXPERIMENTAL_OWL_EXT,
      OPT_SEMEAI_NODE_LIMIT,
      OPT_EXPERIMENTAL_CONNECTIONS,
      OPT_ALTERNATE_CONNECTIONS,
      OPT_WITH_BREAK_IN,
      OPT_WITHOUT_BREAK_IN,
      OPT_COSMIC_GNUGO,
      OPT_NO_COSMIC_GNUGO,
      OPT_LARGE_SCALE,
      OPT_NO_LARGE_SCALE,
      OPT_OPTIONS,
      OPT_STANDARD_SEMEAI,
      OPT_STANDARD_CONNECTIONS,
      OPT_PRINT_LEVELS,
      OPT_DECIDE_POSITION,
      OPT_DECIDE_EYE,
      OPT_DECIDE_COMBINATION,
      OPT_BRANCH_DEPTH,
      OPT_BACKFILL2_DEPTH,
      OPT_BREAK_CHAIN_DEPTH,
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
      OPT_MIN_LEVEL,
      OPT_MAX_LEVEL,
      OPT_LIMIT_SEARCH,
      OPT_SHOWTIME,
      OPT_SHOWSCORE,
      OPT_DEBUG_INFLUENCE,
      OPT_SCORE,
      OPT_PRINTSGF,
      OPT_PROFILE_PATTERNS,
      OPT_CHINESE_RULES,
      OPT_OWL_THREATS,
      OPT_NO_OWL_THREATS,
      OPT_JAPANESE_RULES,
      OPT_FORBID_SUICIDE,
      OPT_ALLOW_SUICIDE,
      OPT_ALLOW_ALL_SUICIDE,
      OPT_SIMPLE_KO,
      OPT_NO_KO,
      OPT_POSITIONAL_SUPERKO,
      OPT_SITUATIONAL_SUPERKO,
      OPT_CAPTURE_ALL_DEAD,
      OPT_PLAY_OUT_AFTERMATH,
      OPT_MIRROR,
      OPT_MIRROR_LIMIT,
      OPT_METAMACHINE,
      OPT_RESIGN_ALLOWED,
      OPT_NEVER_RESIGN,
      OPT_MONTE_CARLO,
      OPT_MC_GAMES_PER_LEVEL,
      OPT_MC_PATTERNS,
      OPT_MC_LIST_PATTERNS,
      OPT_MC_LOAD_PATTERNS
};

/* names of playing modes */

enum mode {
  MODE_UNKNOWN = 0,
  MODE_ASCII,
  MODE_GTP,
  MODE_GMP,
  MODE_SGMP,
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
  MODE_DECIDE_SURROUNDED,
  MODE_DECIDE_ORACLE
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
  {"gtp-connect",    required_argument, 0, OPT_GTP_CONNECT},
  {"gtp-listen",     required_argument, 0, OPT_GTP_LISTEN},
  {"gtp-dump-commands", required_argument, 0, OPT_GTP_DUMP_COMMANDS},
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
  {"break-chain-depth", required_argument, 0, OPT_BREAK_CHAIN_DEPTH},
  {"superstring-depth", required_argument, 0, OPT_SUPERSTRING_DEPTH},
  {"fourlib-depth",  required_argument, 0, 'F'},
  {"ko-depth",       required_argument, 0, 'K'},
  {"aa-depth",       required_argument, 0, OPT_AA_DEPTH},
  {"owl-distrust",   required_argument, 0, OPT_OWL_DISTRUST},
  {"owl-branch",     required_argument, 0, OPT_OWL_BRANCH},
  {"owl-reading",    required_argument, 0, OPT_OWL_READING},
  {"owl-node-limit", required_argument, 0, OPT_OWL_NODE_LIMIT},
  {"print-levels",   no_argument,       0, OPT_PRINT_LEVELS},
  {"level",          required_argument, 0, OPT_LEVEL},
  {"min-level",      required_argument, 0, OPT_MIN_LEVEL},
  {"max-level",      required_argument, 0, OPT_MAX_LEVEL},
  {"limit-search",   required_argument, 0, OPT_LIMIT_SEARCH},
  {"clock",          required_argument, 0, OPT_CLOCK_TIME},
  {"byo-time",       required_argument, 0, OPT_CLOCK_BYO_TIME},
  {"byo-period",     required_argument, 0, OPT_CLOCK_BYO_PERIOD},
  {"autolevel",      no_argument,       0, OPT_AUTOLEVEL},
  {"chinese-rules",  no_argument,       0, OPT_CHINESE_RULES},
  {"japanese-rules", no_argument,       0, OPT_JAPANESE_RULES},
  {"experimental-semeai",  no_argument, 0, OPT_EXPERIMENTAL_SEMEAI},
  {"experimental-owl-ext",  no_argument, 0, OPT_EXPERIMENTAL_OWL_EXT},
  {"semeai-node-limit",   required_argument, 0, OPT_SEMEAI_NODE_LIMIT},
  {"experimental-connections",  no_argument, 0, OPT_EXPERIMENTAL_CONNECTIONS},
  {"standard-connections",  no_argument, 0, OPT_STANDARD_CONNECTIONS},
  {"standard-semeai", no_argument,      0, OPT_STANDARD_SEMEAI},
  {"alternate-connections",  no_argument, 0, OPT_ALTERNATE_CONNECTIONS},
  {"with-break-in",  	   no_argument, 0, OPT_WITH_BREAK_IN},
  {"without-break-in",     no_argument, 0, OPT_WITHOUT_BREAK_IN},
  {"cosmic-gnugo",         no_argument, 0, OPT_COSMIC_GNUGO},
  {"no-cosmic-gnugo",      no_argument, 0, OPT_NO_COSMIC_GNUGO},
  {"large-scale",          no_argument, 0, OPT_LARGE_SCALE},
  {"no-large-scale",       no_argument, 0, OPT_NO_LARGE_SCALE},
  {"options",              no_argument, 0, OPT_OPTIONS},
  {"forbid-suicide",       no_argument, 0, OPT_FORBID_SUICIDE},
  {"allow-suicide",        no_argument, 0, OPT_ALLOW_SUICIDE},
  {"allow-all-suicide",    no_argument, 0, OPT_ALLOW_ALL_SUICIDE},
  {"simple-ko",            no_argument, 0, OPT_SIMPLE_KO},
  {"no-ko",                no_argument, 0, OPT_NO_KO},
  {"positional-superko",   no_argument, 0, OPT_POSITIONAL_SUPERKO},
  {"situational-superko",  no_argument, 0, OPT_SITUATIONAL_SUPERKO},
  {"capture-all-dead",     no_argument, 0, OPT_CAPTURE_ALL_DEAD},
  {"play-out-aftermath",   no_argument, 0, OPT_PLAY_OUT_AFTERMATH},
  {"cache-size",     required_argument, 0, 'M'},
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
  {"decide-oracle",  no_argument,       0, OPT_DECIDE_ORACLE},
  {"nofusekidb",     no_argument,       0, OPT_NOFUSEKIDB},
  {"nofuseki",       no_argument,       0, OPT_NOFUSEKI},
  {"nojosekidb",     no_argument,       0, OPT_NOJOSEKIDB},
  {"debug-influence", required_argument, 0, OPT_DEBUG_INFLUENCE},
  {"showtime",       no_argument,       0, OPT_SHOWTIME},
  {"showscore",      no_argument,       0, OPT_SHOWSCORE},
  {"score",          required_argument, 0, OPT_SCORE},
  {"printsgf",       required_argument, 0, OPT_PRINTSGF},
  {"profile-patterns", no_argument,     0, OPT_PROFILE_PATTERNS},
  {"mirror",         no_argument,       0, OPT_MIRROR},
  {"mirror-limit",   required_argument, 0, OPT_MIRROR_LIMIT},
  {"metamachine",    no_argument,       0, OPT_METAMACHINE},
  {"resign-allowed", no_argument,       0, OPT_RESIGN_ALLOWED},
  {"never-resign",   no_argument,       0, OPT_NEVER_RESIGN},
  {"monte-carlo",    no_argument,       0, OPT_MONTE_CARLO},
  {"mc-games-per-level", required_argument, 0, OPT_MC_GAMES_PER_LEVEL},
  {"mc-patterns",    required_argument, 0, OPT_MC_PATTERNS},
  {"mc-list-patterns", no_argument,     0, OPT_MC_LIST_PATTERNS},
  {"mc-load-patterns", required_argument, 0, OPT_MC_LOAD_PATTERNS},
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
  char *gtp_dump_commands_file = NULL;
  int gtp_tcp_ip_mode = 0;
  char *gtp_tcp_ip_address = NULL;
  
  char *printsgffile = NULL;
  
  char decide_this[8];
  char *decide_that = NULL;
  char debuginfluence_move[4] = "\0";
  
  int benchmark = 0;  /* benchmarking mode (-b) */
  FILE *output_check;
  int orientation = 0;

  char mc_pattern_name[40] = "";
  char mc_pattern_filename[320] = "";

  float memory = (float) DEFAULT_MEMORY; /* Megabytes used for hash table. */

  /* If seed is zero, GNU Go will play a different game each time. If
   * it is set using -r, GNU Go will play the same game each time.
   * (Change seed to get a different game).
   */
  int seed = 0;
  int seed_specified = 0;
  
  int requested_boardsize = -1;

  sgftree_clear(&sgftree);
  gameinfo_clear(&gameinfo);
  
  /* Weed through all of the command line options. */
  while ((i = gg_getopt_long(argc, argv, 
                            "-ab:B:d:D:EF:gh::K:l:L:M:m:o:O:p:r:fsStTvw",
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
      case 'r': seed = atoi(gg_optarg); seed_specified = 1; break;
      case 'S': showstatistics = 1; break;
      case 'w': printworms = 1; break;
      case 'm': printmoyo = strtol(gg_optarg, NULL, 0);  /* allows 0x... */ 
	break;
      case 'd': debug ^= strtol(gg_optarg, NULL, 0);  /* allows 0x... */ break;
      case 'D': mandated_depth = atoi(gg_optarg); break;
      case 'M': memory = atof(gg_optarg); break; /* floating point number */
      case 'E': printboard = 2; break;
      case 'B': mandated_backfill_depth = atoi(gg_optarg); break;
      case 'F': mandated_fourlib_depth = atoi(gg_optarg); break;
      case 'K': mandated_ko_depth = atoi(gg_optarg); break;

      case 'L':
	untilstring = gg_optarg;
	break;
	
      case 'o':
	if (strlen(gg_optarg) >= sizeof(outfilename)) {
	  fprintf(stderr, "Too long filename given as value to -o option.\n");
	  exit(EXIT_FAILURE);
	}
	outfile = gg_optarg;
	strcpy(outfilename, gg_optarg);
	break;

      case 'O':
	outflags = gg_optarg;
	output_flags = 0;
	if (outflags)
	  while (*outflags) {
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
      case OPT_GTP_CONNECT:
      case OPT_GTP_LISTEN:
	if (gtp_tcp_ip_mode != 0 || gtpfile != NULL) {
	  fprintf(stderr, ("Options `--gtp-input', `--gtp-connect' and `--gtp-listen' "
			   "are mutually-exclusive\n"));
	  exit(EXIT_FAILURE);
	}

	if (i == OPT_GTP_INPUT)
	  gtpfile = gg_optarg;
	else {
	  gtp_tcp_ip_mode = i;
	  gtp_tcp_ip_address = gg_optarg;
	}

	break;

      case OPT_GTP_DUMP_COMMANDS:
	gtp_dump_commands_file = gg_optarg;
	break;
	
      case OPT_GTP_INITIAL_ORIENTATION:
	orientation = atoi(gg_optarg);
	if (orientation < 0 || orientation > 7) {
	  fprintf(stderr, "Invalid orientation: %d.\n", orientation);
	  fprintf(stderr, "Try `gnugo --help' for more information.\n");
	  exit(EXIT_FAILURE);
	}
	break;
	
      case OPT_GTP_VERSION:
	gtp_version = atoi(gg_optarg);
	break;
	
      case OPT_OPTIONS:
	if (USE_BREAK_IN)
	  fprintf(stdout,
		  "configure option enabled: experimental break-ins\n");
	if (COSMIC_GNUGO)
	  fprintf(stdout,
		  "configure option enabled: cosmic GNU Go \n");
	if (LARGE_SCALE)
	  fprintf(stdout,
		  "configure option enabled: large scale captures \n");
	if (EXPERIMENTAL_CONNECTIONS)
	  fprintf(stdout,
		  "configure option enabled: experimental connections\n");
	if (ALTERNATE_CONNECTIONS)
	  fprintf(stdout,
		  "configure option enabled: alternate connections\n");
	if (EXPERIMENTAL_OWL_EXT)
	  fprintf(stdout,
		  "configure option enabled: experimental GAIN/LOSS codes\n");
	if (OWL_THREATS)
	  fprintf(stdout,
		  "configure option enabled: owl threats\n");
	if (RESIGNATION_ALLOWED)
	  fprintf(stdout,
		  "configure option enabled: resignation allowed\n");
	if (ORACLE)
	  fprintf(stdout,
		  "configure option enabled: oracle\n");
	fprintf(stdout,
		"Owl node limit: %d\n", OWL_NODE_LIMIT);
	fprintf(stdout,
		"Semeai node limit: %d\n", SEMEAI_NODE_LIMIT);
	if (DEFAULT_MEMORY == -1)
	  fprintf(stdout, "Cache size: %d MB (special default value)\n",
		  DEFAULT_MEMORY);
	else
	  fprintf(stdout, "Cache size: %d MB\n", DEFAULT_MEMORY);

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
	  int requested_handicap = atoi(gg_optarg);
	  
	  if (requested_handicap < 0 || requested_handicap > MAX_HANDICAP) {
	    fprintf(stderr, "Unsupported handicap: %d.\n", requested_handicap);
	    fprintf(stderr, "Try `gnugo --help' for more information.\n");
	    exit(EXIT_FAILURE);
	  }
	  gameinfo.handicap = requested_handicap;
	}
        break;
      
      case OPT_BOARDSIZE:
	requested_boardsize = atoi(gg_optarg);
	break;
	
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

      case OPT_METAMACHINE:
        metamachine = 1;
	break;

      case OPT_JAPANESE_RULES: 
	chinese_rules = 0;
	break;

      case OPT_EXPERIMENTAL_OWL_EXT:
	experimental_owl_ext = 1;
	break;

      case OPT_SEMEAI_NODE_LIMIT:
	mandated_semeai_node_limit = atoi(gg_optarg);
	break;

      case OPT_EXPERIMENTAL_CONNECTIONS:
	experimental_connections = 1;
	break;

      case OPT_STANDARD_CONNECTIONS: 
	experimental_connections = 0;
	break;

      case OPT_ALTERNATE_CONNECTIONS: 
	alternate_connections = !alternate_connections;
	break;

      case OPT_WITH_BREAK_IN:
	experimental_break_in = 1;
	break;

      case OPT_WITHOUT_BREAK_IN:
	experimental_break_in = 0;
	break;

      case OPT_COSMIC_GNUGO:
	cosmic_gnugo = 1;
	break;

      case OPT_NO_COSMIC_GNUGO:
	cosmic_gnugo = 0;
	break;

      case OPT_LARGE_SCALE:
	large_scale = 1;
	break;

      case OPT_NO_LARGE_SCALE:
	large_scale = 0;
	break;

      case OPT_FORBID_SUICIDE:
	suicide_rule = FORBIDDEN;
	break;

      case OPT_ALLOW_SUICIDE:
	suicide_rule = ALLOWED;
	break;

      case OPT_ALLOW_ALL_SUICIDE:
	suicide_rule = ALL_ALLOWED;
	break;

      case OPT_SIMPLE_KO:
	ko_rule = SIMPLE;
	break;

      case OPT_NO_KO:
	ko_rule = NONE;
	break;

      case OPT_POSITIONAL_SUPERKO:
	ko_rule = PSK;
	break;

      case OPT_SITUATIONAL_SUPERKO:
	ko_rule = SSK;
	break;

      case OPT_CAPTURE_ALL_DEAD:
	capture_all_dead = 1;
	break;

      case OPT_PLAY_OUT_AFTERMATH:
	play_out_aftermath = 1;
	break;

      case OPT_RESIGN_ALLOWED:
	resign_allowed = 1;
	break;

      case OPT_NEVER_RESIGN:
	resign_allowed = 0;
	break;

      case OPT_MONTE_CARLO:
	use_monte_carlo_genmove = 1;
	break;

      case OPT_MC_GAMES_PER_LEVEL:
	mc_games_per_level = atoi(gg_optarg);
	break;

      case OPT_MC_PATTERNS:
	if (strlen(gg_optarg) >= sizeof(mc_pattern_name)) {
	  fprintf(stderr, "Too long name given as value to --mc-patterns option.\n");
	  exit(EXIT_FAILURE);
	}
	strcpy(mc_pattern_name, gg_optarg);
	break;

      case OPT_MC_LIST_PATTERNS:
	list_mc_patterns();
	return EXIT_SUCCESS;
	break;

      case OPT_MC_LOAD_PATTERNS:
	if (strlen(gg_optarg) >= sizeof(mc_pattern_filename)) {
	  fprintf(stderr, "Too long name given as value to --mc-load-patterns option.\n");
	  exit(EXIT_FAILURE);
	}
	strcpy(mc_pattern_filename, gg_optarg);
	break;

      case OPT_MODE: 
	if (strcmp(gg_optarg, "ascii") == 0)
	  playmode = MODE_ASCII;
	else if (strcmp(gg_optarg, "gtp") == 0)
	  playmode = MODE_GTP;
	else if (strcmp(gg_optarg, "gmp") == 0)
	  playmode = MODE_GMP;
	else if (strcmp(gg_optarg, "sgmp") == 0)
	  playmode = MODE_SGMP;
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
	
      case OPT_DECIDE_ORACLE:
	playmode = MODE_DECIDE_ORACLE;
	break;
	
      case OPT_BRANCH_DEPTH:
	mandated_branch_depth = atoi(gg_optarg);
	break;
	
      case OPT_BACKFILL2_DEPTH:
	mandated_backfill2_depth = atoi(gg_optarg);
	break;
	
      case OPT_BREAK_CHAIN_DEPTH:
	mandated_break_chain_depth = atoi(gg_optarg);
	break;
	
      case OPT_SUPERSTRING_DEPTH:
	mandated_superstring_depth = atoi(gg_optarg);
	break;
	
      case OPT_AA_DEPTH:
	mandated_aa_depth = atoi(gg_optarg);
	break;

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
	set_level(atoi(gg_optarg));
	break;

      case OPT_MIN_LEVEL:
	set_min_level(atoi(gg_optarg));
	break;

      case OPT_MAX_LEVEL:
	set_max_level(atoi(gg_optarg));
	break;

      case OPT_LIMIT_SEARCH:
	{
	  int pos = string_to_location(board_size, gg_optarg);
	  if (pos == NO_MOVE) {
	    fprintf(stderr, "gnugo: use --limit-search <pos>\n");
	    return EXIT_FAILURE;
	  }
	  set_search_diamond(pos);
	}
	break;

      case OPT_CLOCK_TIME:
	clock_settings(atoi(gg_optarg), -1, -1);
	break;

      case OPT_CLOCK_BYO_TIME: 
	clock_settings(-1, atoi(gg_optarg), -1);
	break;

      case OPT_CLOCK_BYO_PERIOD:
	clock_settings(-1, -1, atoi(gg_optarg));
	break;

      case OPT_AUTOLEVEL:
	autolevel_on = 1;
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
	
      case OPT_PROFILE_PATTERNS:
	profile_patterns = 1;
	prepare_pattern_profiling();
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
	show_copyright();
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

      case OPT_PRINT_LEVELS:
	{
	  int lev;
	  for (lev = 12; lev >= 0; lev--)
	    set_depth_values(lev, 1);
	}
	return EXIT_SUCCESS;
	break;

      /* NOTE: getopt returns '?' if an illegal option is supplied. */

      case '?':
      default:
	fprintf(stderr, "Try `gnugo --help' for more information.\n");
	exit(EXIT_FAILURE);
      }
    }

  if (requested_boardsize != -1) {
    if (!check_boardsize(requested_boardsize, stderr))
      exit(EXIT_FAILURE);
    gnugo_clear_board(requested_boardsize);
  }
  
  /* Start random number seed. */
  if (!seed_specified)
    seed = time(0);
  
  /* Initialize the GNU Go engine. */
  init_gnugo(memory, seed);

  /* Load Monte Carlo patterns if one has been specified. Either
   * choose one of the compiled in ones or load directly from a
   * database file.
   */
  if (strlen(mc_pattern_filename) > 0) {
    if (!mc_load_patterns_from_db(mc_pattern_filename, NULL))
      return EXIT_FAILURE;
  }
  else if (strlen(mc_pattern_name) > 0) {
    if (!choose_mc_patterns(mc_pattern_name)) {
      fprintf(stderr, "Unknown Monte Carlo pattern database name %s.\n",
	      mc_pattern_name);
      fprintf(stderr, "Use \"--mc-list-patterns\" to list the available databases.\n");
      return EXIT_FAILURE;
    }
  }

  /* Read the infile if there is one. Also play up the position. */
  if (infilename) {
    if (!sgftree_readfile(&sgftree, infilename)) {
      fprintf(stderr, "Cannot open or parse '%s'\n", infilename);
      exit(EXIT_FAILURE);
    }
    
    if (gameinfo_play_sgftree_rot(&gameinfo, &sgftree, untilstring,
				  orientation) == EMPTY) {
      fprintf(stderr, "Cannot load '%s'\n", infilename);
      exit(EXIT_FAILURE);
    }
  }
  else
  /* Initialize and empty sgf tree if there was no infile. */
    sgftreeCreateHeaderNode(&sgftree, board_size, komi, handicap);

  /* Set the game_record to be identical to the loaded one or the
   * newly created empty sgf tree.
   */
  gameinfo.game_record = sgftree;
  
  /* Notice that we need to know the board size before we can do this.
   */
  if (debuginfluence_move[0]) {
    int pos = string_to_location(board_size, debuginfluence_move);
    debug_influence_move(pos);
  }
  
  /* Figure out a default mode if there was no explicit one. */
  if (playmode == MODE_UNKNOWN) {
    if (infilename)
      playmode = MODE_LOAD_AND_ANALYZE;
    else
      playmode = (isatty(0)) ? MODE_ASCII : MODE_GMP;
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
  case MODE_GMP:
  case MODE_SGMP:

    /* not supported by the protocol */
    resign_allowed = 0;

#if ORACLE
    if (metamachine)
      summon_oracle();
#endif

    /* EMPTY is valid for play_gmp.c. */
    gameinfo.computer_player = mandated_color;
    play_gmp(&gameinfo, playmode == MODE_SGMP);

#if ORACLE
    if (metamachine)
      dismiss_oracle();
#endif

    break;
    
  case MODE_SOLO:
    play_solo(&gameinfo, benchmark);
    break;
    
  case MODE_REPLAY:    
    if (!infilename) {
      fprintf(stderr, "You must use -l infile with replay mode.\n");
      exit(EXIT_FAILURE);
    }
    play_replay(&sgftree, replay_color);
    break;
    
  case MODE_LOAD_AND_ANALYZE:
    if (mandated_color != EMPTY)
      gameinfo.to_move = mandated_color;
    
    if (!infilename) {
      fprintf(stderr, "You must use -l infile with load and analyze mode.\n");
      exit(EXIT_FAILURE);
    }

#if ORACLE
    if (metamachine) {
      summon_oracle();
      oracle_loadsgf(infilename, untilstring);
    }
#endif

    load_and_analyze_sgf_file(&gameinfo);

#if ORACLE
    dismiss_oracle();
#endif

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
      int str;
      
      if (!infilename) {
	fprintf(stderr, "gnugo: --decide-string must be used with -l\n");
	return EXIT_FAILURE;
      }

      str = string_to_location(board_size, decide_this); 
      if (str == NO_MOVE) {
	fprintf(stderr, "gnugo: --decide-string: strange coordinate \n");
	return EXIT_FAILURE;
      }

      decide_string(str);
    }
    break;
  
  case MODE_DECIDE_CONNECTION:
    {
      int str1, str2;
      
      if (!infilename) {
	fprintf(stderr, "gnugo: --decide-connection must be used with -l\n");
	return EXIT_FAILURE;
      }

      str1 = string_to_location(board_size, decide_this);
      if (str1 == NO_MOVE) {
	fprintf(stderr,
		"usage: --decide-connection [first string]/[second string]\n");
	return EXIT_FAILURE;
      }

      str2 = string_to_location(board_size, decide_that);
      if (str2 == NO_MOVE) {
	fprintf(stderr,
		"usage: --decide-connection [first string]/[second string]\n");
	return EXIT_FAILURE;
      }

      decide_connection(str1, str2);
    }
    break;
  
  case MODE_DECIDE_OWL:
    {
      int pos;
      
      if (!infilename) {
	fprintf(stderr, "gnugo: --decide-dragon must be used with -l\n");
	return EXIT_FAILURE;
      }

      pos = string_to_location(board_size, decide_this);
      if (pos == NO_MOVE) {
	fprintf(stderr, "gnugo: --decide-dragon: strange coordinate \n");
	return EXIT_FAILURE;
      }

      decide_owl(pos);
    }
    break;
  
  case MODE_DECIDE_DRAGON_DATA:
    {
      int pos;
      
      if (!infilename) {
	fprintf(stderr, "gnugo: --decide-dragon-data must be used with -l\n");
	return EXIT_FAILURE;
      }

      pos = string_to_location(board_size, decide_this);
      if (pos == NO_MOVE) {
	fprintf(stderr, "gnugo: --decide-dragon-data: strange coordinate \n");
	return EXIT_FAILURE;
      }

      decide_dragon_data(pos);
    }
    break;
  
  case MODE_DECIDE_SEMEAI:
    {
      int pos1, pos2;
      
      if (!infilename) {
	fprintf(stderr, "gnugo: --decide-semeai must be used with -l\n");
	return EXIT_FAILURE;
      }

      pos1 = string_to_location(board_size, decide_this);
      if (pos1 == NO_MOVE) {
	fprintf(stderr, 
		"usage: --decide-semeai [first dragon]/[second dragon]\n");
	return EXIT_FAILURE;
      }

      pos2 = string_to_location(board_size, decide_that);
      if (pos2 == NO_MOVE) {
	fprintf(stderr, 
		"usage: --decide-semeai [first dragon]/[second dragon]\n");
	return EXIT_FAILURE;
      }

      decide_semeai(pos1, pos2);
    }
    break;
    

  case MODE_DECIDE_TACTICAL_SEMEAI:
    {
      int pos1, pos2;
      
      if (!infilename) {
	fprintf(stderr, "gnugo: --decide-tactical-semeai must be used with -l\n");
	return EXIT_FAILURE;
      }

      pos1 = string_to_location(board_size, decide_this);
      if (pos1 == NO_MOVE) {
	fprintf(stderr, 
		"usage: --decide-tactical-semeai [first dragon]/[second dragon]\n");
	return EXIT_FAILURE;
      }

      pos2 = string_to_location(board_size, decide_that);
      if (pos2 == NO_MOVE) {
	fprintf(stderr, 
		"usage: --decide-tactical-semeai [first dragon]/[second dragon]\n");
	return EXIT_FAILURE;
      }

      decide_tactical_semeai(pos1, pos2);
    }
    break;
    

  case MODE_DECIDE_POSITION:
    {
      if (!infilename) {
	fprintf(stderr, "gnugo: --decide-position must be used with -l\n");
	return EXIT_FAILURE;
      }
      decide_position();
    }
    break;
    
  case MODE_DECIDE_EYE:
    {
      int pos;
      
      if (!infilename) {
	fprintf(stderr, "gnugo: --decide-eye must be used with -l\n");
	return EXIT_FAILURE;
      }

      pos = string_to_location(board_size, decide_this);
      if (pos == NO_MOVE) {
	fprintf(stderr, "gnugo: --decide-eye: strange coordinate \n");
	return EXIT_FAILURE;
      }
      
      decide_eye(pos);
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
      int pos = string_to_location(board_size, decide_this);

      if (pos == NO_MOVE) {
	fprintf(stderr, 
		"usage: --decide-surrounded [pos]\n");
	return EXIT_FAILURE;
      }

      decide_surrounded(pos);
      break;
    }

#if ORACLE
  case MODE_DECIDE_ORACLE:
    {
      if (mandated_color != EMPTY)
	gameinfo.to_move = mandated_color;
      
      if (!infilename) {
	fprintf(stderr, "You must use -l infile with load and analyze mode.\n");
	exit(EXIT_FAILURE);
      }

      decide_oracle(&gameinfo, infilename, untilstring);
      break;
    }
#endif

  case MODE_GTP:
    {
      FILE *gtp_input_FILE = stdin;
      FILE *gtp_output_FILE = stdout;
      FILE *gtp_dump_commands_FILE = NULL;

      if (gtpfile != NULL) {
	gtp_input_FILE = fopen(gtpfile, "r");
	if (gtp_input_FILE == NULL) {
	  fprintf(stderr, "gnugo: Cannot open file %s\n", gtpfile);
	  return EXIT_FAILURE;
	}
      }
      else if (gtp_tcp_ip_mode != 0) {
	unsigned int port = 65536;
	char *port_string = strchr(gtp_tcp_ip_address, ':');
	const char *host_name = NULL;

	if (port_string) {
	  host_name = gtp_tcp_ip_address;

	  *port_string++ = 0;
	  sscanf(port_string, "%u", &port);
	}
	else
	  sscanf(gtp_tcp_ip_address, "%u", &port);

	if (port > 65535) {
	  fprintf(stderr, "A valid TCP/IP port number expected\n");
	  exit(EXIT_FAILURE);
	}

	if (gtp_tcp_ip_mode == OPT_GTP_CONNECT) {
	  socket_connect_to(host_name, port,
			    &gtp_input_FILE, &gtp_output_FILE);
	}
	else {
	  socket_listen_at(host_name, port,
			   &gtp_input_FILE, &gtp_output_FILE);
	}
      }

      if (gtp_dump_commands_file != NULL) {
	gtp_dump_commands_FILE = fopen(gtp_dump_commands_file, "w");
	if (gtp_dump_commands_FILE == NULL) {
	  fprintf(stderr, "gnugo: Cannot open file %s\n",
		  gtp_dump_commands_file);
	  return EXIT_FAILURE;
	}
      }

      play_gtp(gtp_input_FILE, gtp_output_FILE, gtp_dump_commands_FILE,
	       orientation);

      if (gtp_dump_commands_FILE)
	fclose(gtp_dump_commands_FILE);

      if (gtp_tcp_ip_mode == OPT_GTP_CONNECT)
	socket_close_connection(gtp_input_FILE, gtp_output_FILE);
      else if (gtp_tcp_ip_mode == OPT_GTP_LISTEN)
	socket_stop_listening(gtp_input_FILE, gtp_output_FILE);
    }

    break;

  case MODE_ASCII:  
  default:     
    if (mandated_color != EMPTY)
      gameinfo.computer_player = OTHER_COLOR(mandated_color);

  /* Display copyright message in ASCII mode unless --quiet option used. */
    if (!quiet) {
      show_version();
      show_copyright();
    }

#if ORACLE
    if (metamachine) {
      summon_oracle();
      oracle_loadsgf(infilename, untilstring);
    }
#endif

    play_ascii(&sgftree, &gameinfo, infilename, untilstring);
    break;
  }
  
  if (profile_patterns)
    report_pattern_profiling();

  sgfFreeNode(sgftree.root); 

  return 0;
}  /* end main */



static void
show_version(void)
{
  printf("GNU Go %s\n", VERSION);
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
Examples:\n\
  gnugo --mode gtp --level 5\n\
         To play against gnugo in level 5 from a GTP client\n\
  gnugo --mode ascii -l game.sgf -L 123\n\
         Resume game at move 123 in ASCII mode\n\
  gnugo --score estimate -l game.sgf\n\
         Give a rough score estimate of the end position in game.sgf\n\
\n\
Main Options:\n\
       --mode <mode>     Force the playing mode ('ascii', 'gmp', 'sgmp',\n\
                         or 'gtp'). Default is ASCII.\n\
                         If no terminal is detected GMP (Go Modem Protocol)\n\
                         will be assumed.\n\
       --quiet  --silent Don't print copyright and informational messages\n\
       --level <amount>  strength (default %d)\n\
       --never-resign    Forbid GNU Go to resign\n\
       --resign-allowed  Allow resignation (default)\n\
   -l, --infile <file>   Load name sgf file\n\
   -L, --until <move>    Stop loading just before move is played. <move>\n\
                         can be the move number or location (eg L10).\n\
   -o, --outfile <file>  Write sgf output to file\n\
       --printsgf <file>     Write position as a diagram to file (use with -l)\n\
\n\
Scoring:\n\
   --score estimate      estimate score at loaded position\n\
   --score finish        generate moves to finish game, then score\n\
   --score aftermath     generate moves to finish, use best algorithm\n\
\n\
"

#define USAGE1 "\
Game Options:\n\
Used with --mode ascii (or other modes for non-interactive settings)\n\
   --boardsize num   Set the board size to use (%d--%d)\n\
   --color <color>   Choose your color ('black' or 'white')\n\
   --handicap <num>  Set the number of handicap stones (0--%d)\n\
   --komi <num>      Set the komi\n\
   --clock <sec>     Initialize the timer.\n\
   --byo-time <sec>  Initialize the byo-yomi timer.\n\
   --byo-period <stones>  Initialize the byo-yomi period.\n\
\n\
   --japanese-rules     (default)\n\
   --chinese-rules\n\
   --forbid-suicide      Forbid suicide. (default)\n\
   --allow-suicide       Allow suicide except single-stone suicide.\n\
   --allow-all-suicide   Allow all suicide moves.\n\
   --simple-ko           Forbid simple ko recapture. (default)\n\
   --no-ko               Allow any ko recapture.\n\
   --positional-superko  Positional superko restrictions.\n\
   --situational-superko Situational superko restrictions.\n\
\n\
   --play-out-aftermath\n\
   --capture-all-dead\n\
\n\
   --min-level <amount>         minimum level for adjustment schemes\n\
   --max-level <amount>         maximum level for adjustment schemes\n\
   --autolevel                  adapt gnugo level during game to respect\n\
                                the time specified by --clock <sec>.\n\
\n\
Connection options\n\
   --gtp-input <file>Read gtp commands from file instead of stdin\n\
   --gtp-connect [HOST:]PORT\n\
                     Connect to given host (127.0.0.1 if omitted) and port\n\
                     and receive GTP commands on the established connection\n\
   --gtp-listen [HOST:]PORT\n\
                     Wait for the first TCP/IP connection on the given port\n\
                     (if HOST is specified, only to that host)\n\
   --gtp-version\n\
\n\
"

#define USAGE2 "\
Experimental options:\n\
   --with-break-in         use the break-in code (on at level 10 by default)\n\
   --without-break-in      do not use the break-in code\n\
   --cosmic-gnugo          use center oriented influence\n\
   --no-cosmic-gnugo       don't use center oriented influence (default)\n\
   --large-scale           look for large scale captures\n\
   --no-large-scale        don't seek large scale captures (default)\n\
   --nofusekidb            turn off fuseki database\n\
   --nofuseki              turn off fuseki moves entirely\n\
   --nojosekidb            turn off joseki database\n\
   --mirror                try to play mirror go\n\
   --mirror-limit <n>      stop mirroring when n stones on board\n\n\
   --monte-carlo           enable Monte Carlo move generation (9x9 or smaller)\n\
   --mc-games-per-level <n> number of Monte Carlo simulations per level\n\
   --mc-list-patterns      list names of builtin Monte Carlo patterns\n\
   --mc-patterns <name>    choose a built in Monte Carlo pattern database\n\
   --mc-load-patterns <filename> read Monte Carlo patterns from file\n\
   --alternate-connections\n\
   --experimental-connections\n\
   --experimental-owl-ext\n\
   --experimental-semeai\n\
   --standard-connections\n\
   --standard-semeai\n\
   --oracle                Read the documentation\n\
\n\
Cache size (higher=more memory usage, faster unless swapping occurs):\n\
   -M, --cache-size <megabytes>  RAM cache for read results (default %4.1f Mb)\n\
\n\
Informative Output:\n\
   -v, --version         Display the version and copyright of GNU Go\n\
   --options             Display configure options\n\
   -h, --help            Display this help message\n\
       --help debug      Display help about debugging options\n\
       --copyright       Display copyright notice\n\
\n\
"

#define COPYRIGHT \
"Copyright 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007\n\
2008 and 2009 by the Free Software Foundation, Inc.\n\
See http://www.gnu.org/software/gnugo/ or contact\n\
gnugo@gnu.org for information about GNU Go. GNU Go comes with NO WARRANTY to\n\
the extent permitted by law. This program is free software; you can\n\
redistribute it and/or modify it under the terms of the GNU General Public\n\
License as published by the Free Software Foundation - version 3 or\n\
(at your option) any later version. For more\n\
information about these matters, see the files named COPYING.\n"

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
       --debug-flags             print the debug flags for previous item\n\
   -w, --worms                   print worm and dragon data and move reasons\n\
   -m, --moyo <level>            moyo debugging, show moyo board\n\
       --debug-influence <move>   print influence map after making a move\n\
   -b, --benchmark num           benchmarking mode - can be used with -l\n\
   -S, --statistics              print statistics (for debugging purposes)\n\n\
       --profile-patterns        print statistics for pattern usage\n\
       --showtime                print timing diagnostic\n\
   -t, --trace                   verbose tracing\n\
   -O, --output-flags <flags>    optional output (use with -o)\n\
                    d: mark dead and critical dragons\n\
                    v: show values of considered moves\n\
                    specify either 'd', 'v' or 'dv' (nothing by default)\n\
       --showscore               print estimated score\n\
   -r, --seed number             set random number seed\n\
       --gtp-dump-commands <file>dump commands received in GTP mode\n\
       --gtp-initial-orientation\n\
       --orientation\n\
\n\
"

#define USAGE_DEBUG2 "\
Options affecting depth settings and playing strength:\n\
   --print-levels        shows all this values for levels 12 to 0\n\
\n\
   Default values for the default level (%d):\n\
   -D, --depth <depth>          deep reading cutoff (default %d)\n\
   -B, --backfill-depth <depth> deep reading cutoff (default %d)\n\
   -F, --fourlib-depth <depth>  deep reading cutoff (default %d)\n\
   -K, --ko-depth <depth>       deep reading cutoff (default %d)\n\
   --branch-depth <depth>       deep reading cutoff (default %d)\n\
   --backfill2-depth <depth>    deep reading cutoff (default %d)\n\
   --break_chain-depth <depth>  deep reading cutoff (default %d)\n\
   --superstring-depth <depth>  deep reading cutoff (default %d)\n\
   --aa-depth <depth>           deep reading cutoff (default %d)\n\
   --owl-distrust <depth>       owl distrust depth (default %d)\n\
   --owl-branch <depth>         owl branching depth (default %d)\n\
   --owl-reading <depth>        owl reading depth (default %d)\n\
   --owl-node-limit <limit>     max nodes for owl reading (default %d)\n\
   --semeai-node-limit <limit>  max nodes for semeai reading (default %d)\n\
\n\
Options providing detailed reading results etc.:\n\
   --decide-string <string>     can this string live? (try with -o)\n\
   --decide-connection <str/str> can these strings connect? (try with -o)\n\
   --decide-dragon <dragon>     can this dragon live? (try with -o or -t)\n\
   --decide-dragon-data\n\
   --decide-owl\n\
   --decide-position            evaluate all dragons (try with -o or -t)\n\
   --decide-eye <string>        evaluate the eye\n\
   --decide-combination         search for combination attack (try with -o)\n\
   --decide-oracle\n\
   --decide-semeai\n\
   --decide-tactical-semeai\n\
   --decide-surrounded\n\
   --limit-search\n\
\n\
"


/*
 * Since the maximum string length is 2048 bytes in VC++ we
 * split the help string.
 */
static void
show_help(void)
{
  printf(USAGE, DEFAULT_LEVEL);
  printf(USAGE1, MIN_BOARD, MAX_BOARD, MAX_HANDICAP);
  printf(USAGE2, DEFAULT_MEMORY <= 0 ? reading_cache_default_size() :
	 (float) DEFAULT_MEMORY);
}


static void
show_debug_help(void)
{
  set_depth_values(DEFAULT_LEVEL,0);
  printf(USAGE_DEBUG USAGE_DEBUG2, 
	 DEFAULT_LEVEL, depth, backfill_depth, fourlib_depth, ko_depth, branch_depth,
	 backfill2_depth, break_chain_depth, superstring_depth, aa_depth, 
	 owl_distrust_depth, owl_branch_depth,
	 owl_reading_depth, owl_node_limit, semeai_node_limit);
}

static void 
show_debug_flags(void)
{
  printf(DEBUG_FLAGS);
}

static void
show_copyright(void)
{
  printf(COPYRIGHT);
}


#ifdef ENABLE_SOCKET_SUPPORT


#if !defined(_WIN32) && !defined(_WIN32_WCE)

#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>

#define closesocket	close
#define init_sockets()

#else	/* on Windows */


#include <winsocket.h>


static void
init_sockets(void)
{
  WSADATA data;
  WORD version = MAKEWORD(1, 1);

  if (WSAStartup(version, &data) != NO_ERROR) {
    fprintf(stderr, "WSAStartup() failed with error %d\n", WSAGetLastError());
    exit(EXIT_FAILURE);
  }
}


#endif	/* on Windows */


static void
socket_connect_to(const char *host_name, unsigned int port,
		  FILE **input_file, FILE **output_file)
{
  struct sockaddr_in address;
  int connection_socket;
  struct hostent *host_data;
  char **address_pointer;

  init_sockets();

  if (!host_name)
    host_name = "127.0.0.1";

  host_data = gethostbyname(host_name);
  if (!host_data
      || host_data->h_addrtype != AF_INET
      || host_data->h_length != sizeof address.sin_addr) {
    fprintf(stderr, "Failed to resolve host name `%s'\n", host_name);
    exit(EXIT_FAILURE);
  }

  connection_socket = socket(PF_INET, SOCK_STREAM, 0);
  if (connection_socket == -1) {
    fprintf(stderr, "Unexpected error: failed to create a socket\n");
    exit(EXIT_FAILURE);
  }

  address.sin_family = AF_INET;
  address.sin_port = htons((unsigned short) port);

  for (address_pointer = host_data->h_addr_list; *address_pointer;
       address_pointer++) {
    memcpy(&address.sin_addr, *address_pointer, sizeof address.sin_addr);
    if (connect(connection_socket, (struct sockaddr *) &address,
		sizeof address) != -1)
      break;
  }

  if (! *address_pointer) {
    fprintf(stderr, "Failed to connect to %s:%u\n", host_data->h_name, port);
    closesocket(connection_socket);
    exit(EXIT_FAILURE);
  }

#if !USE_WINDOWS_SOCKET_CLUDGE

  *input_file  = fdopen(connection_socket, "r");
  *output_file = fdopen(dup(connection_socket), "w");

#else	/* USE_WINDOWS_SOCKET_CLUDGE */

  winsocket_activate(connection_socket);

  *input_file  = NULL;
  *output_file = NULL;

#endif	/* USE_WINDOWS_SOCKET_CLUDGE */
}


static void
socket_listen_at(const char *host_name, unsigned int port,
		 FILE **input_file, FILE **output_file)
{
  struct sockaddr_in address;
  int listening_socket;
  int connection_socket;

  init_sockets();

  if (host_name) {
    struct hostent *host_data;

    host_data = gethostbyname(host_name);
    if (!host_data
	|| host_data->h_addrtype != AF_INET
	|| host_data->h_length != sizeof address.sin_addr) {
      fprintf(stderr, "Failed to resolve host name `%s'\n", host_name);
      exit(EXIT_FAILURE);
    }

    host_name = host_data->h_name;
    memcpy(&address.sin_addr, host_data->h_addr_list[0],
	   sizeof address.sin_addr);
  }
  else
    address.sin_addr.s_addr = htonl(INADDR_ANY);

  listening_socket = socket(PF_INET, SOCK_STREAM, 0);
  if (listening_socket == -1) {
    fprintf(stderr, "Unexpected error: failed to create a socket\n");
    exit(EXIT_FAILURE);
  }

  address.sin_family = AF_INET;
  address.sin_port = htons((unsigned short) port);

  if (verbose) {
    if (host_name) {
      fprintf(stderr, "Waiting for a connection on %s:%u...\n",
	      host_name, port);
    }
    else
      fprintf(stderr, "Waiting for a connection on port %u...\n", port);
  }

  if (bind(listening_socket,
	   (struct sockaddr *) &address, sizeof address) == -1
      || listen(listening_socket, 0) == -1
      || (connection_socket = accept(listening_socket, NULL, NULL)) == -1) {
    if (host_name)
      fprintf(stderr, "Failed to listen on %s:%u\n", host_name, port);
    else
      fprintf(stderr, "Failed to listen on port %u\n", port);

    closesocket(listening_socket);
    exit(EXIT_FAILURE);
  }

  closesocket(listening_socket);

#if !USE_WINDOWS_SOCKET_CLUDGE

  *input_file  = fdopen(connection_socket, "r");
  *output_file = fdopen(dup(connection_socket), "w");

#else	/* USE_WINDOWS_SOCKET_CLUDGE */

  winsocket_activate(connection_socket);

  *input_file  = NULL;
  *output_file = NULL;

#endif	/* USE_WINDOWS_SOCKET_CLUDGE */
}


static void
socket_close_connection(FILE *input_file, FILE *output_file)
{
  /* When connecting, we close the socket first. */
  fclose(input_file);
  fclose(output_file);
}


static void
socket_stop_listening(FILE *input_file, FILE *output_file)
{
  int buffer[0x1000];

  if (verbose)
    fprintf(stderr, "Waiting for the client to disconnect...\n");

  /* When listening, we wait for the client to disconnect first.
   * Otherwise, socket doesn't get released properly.
   */
  do
    fread(buffer, sizeof buffer, 1, input_file);
  while (!feof(input_file));

  fclose(input_file);
  fclose(output_file);
}


#else  /* not ENABLE_SOCKET_SUPPORT */


static void
socket_connect_to(const char *host_name, unsigned int port,
		  FILE **input_file, FILE **output_file)
{
  UNUSED(host_name);
  UNUSED(port);
  UNUSED(input_file);
  UNUSED(output_file);

  fprintf(stderr, "GNU Go was compiled without socket support, unable to connect\n");
  exit(EXIT_FAILURE);
}


static void
socket_listen_at(const char *host_name, unsigned int port,
		 FILE **input_file, FILE **output_file)
{
  UNUSED(host_name);
  UNUSED(port);
  UNUSED(input_file);
  UNUSED(output_file);

  fprintf(stderr, "GNU Go was compiled without socket support, unable to listen\n");
  exit(EXIT_FAILURE);
}


static void
socket_close_connection(FILE *input_file, FILE *output_file)
{
  UNUSED(input_file);
  UNUSED(output_file);
}


static void
socket_stop_listening(FILE *input_file, FILE *output_file)
{
  UNUSED(input_file);
  UNUSED(output_file);
}


#endif	/* not ENABLE_SOCKET_SUPPORT */


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
