/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * This is GNU GO, a Go program. Contact gnugo@gnu.org, or see       *
 * http://www.gnu.org/software/gnugo/ for more information.          *
 *                                                                   *
 * Copyright 1999, 2000, 2001, 2002 by the Free Software Foundation. *
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

/* Compile one of the pattern databases. This takes a database file,
 * e.g. patterns.db, and produces a C code file, in this case
 * patterns.c.
 */

/* See also patterns.h, and the *.db files.
 */

/* Differences when compiling connections patterns (-c) :
 *  '*' means cutting point
 *  '!' is allowed (inhibit connection there), matches as '.'.
 *  '!' will always be written as the first elements
*/

/* FIXME: This file is a horrible mess, especially after pattern
 *	  matching went 1D. Cleaning it will make future work
 *	  with pattern mathing easier.
 */

/* As in the rest of GNU Go, co-ordinate convention (i,j) is 'i' down from
 * the top, then 'j' across from the left
 */

#define USAGE "\
Usage : mkpat [options] <prefix>\n\
  General options:\n\
	-i = one or more input files (typically *.db)\n\
	-o = output file (typically *.c)\n\
	-v = verbose\n\
	-V <level> = dfa verbiage level\n\
  Database type:\n\
	-p = compile general pattern database (the default)\n\
	-c = compile connections database\n\
	-f = compile a fullboard pattern database\n\
	-C = compile a corner pattern database\n\
	-D = compile a dfa database (allows fast matching)\n\
	-T = compile a tree based pattern database (even faster)\n\
  Pattern generation options:\n\
	-O = allow only O to be anchor (the default)\n\
	-X = allow only X to be anchor\n\
	-b = allow both colors to be anchor\n\
	-m = try to place the anchor in the center of the pattern\n\
	     (works best with dfa databases)\n\
	-a = require anchor in all patterns. Sets fixed_anchor flag in db\n\
	-r = pre-rotate patterns before storing in database\n\
If no input files specified, reads from stdin.\n\
If outpuf file is not specified, writes to stdout.\n\
"


#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

#include "liberty.h"
#include "patterns.h"
#include "gg-getopt.h"
#include "gg_utils.h"

#include "dfa.h"


#define DB_GENERAL	((int) 'p')
#define DB_CONNECTIONS	((int) 'c')
#define DB_FULLBOARD	((int) 'f')
#define DB_CORNER	((int) 'C')
#define DB_DFA		((int) 'D')
#define DB_TREE 	((int) 'T')

/* code assumes that ATT_O and ATT_X are 1 and 2 (in either order)
 * An attribute is a candidate for anchor if  (att & anchor) != 0
 */
#define ANCHOR_O    ATT_O
#define ANCHOR_X    ATT_X
#define ANCHOR_BOTH (ATT_O | ATT_X)

#define MAXLINE 500
#define MAXCONSTRAINT 10000
#define MAXACTION 10000
#define MAXPATNO 5000
#define MAXLABELS 20
#define MAXPARAMS 15
#define MAX_INPUT_FILE_NAMES 10

/* Avoid compiler warnings with unused parameters */
#define UNUSED(x)  (void)x

/* valid characters that can appear in a pattern
 * position in string is att value to store
 */
const char VALID_PATTERN_CHARS[]     = ".XOxo,a!*?QY";
const char VALID_EDGE_CHARS[]        = "+-|";
const char VALID_CONSTRAINT_LABELS[] = "abcdefghijklmnpqrstuvwyzABCDEFGHIJKLMNPRSTUVWZ";


/* the offsets into the list are the ATT_* defined in patterns.h
 * The following defns are for internal use only, and are not
 * written out to the compiled pattern database
 */

#define ATT_star  8
#define ATT_wild  9
#define ATT_Q    10
#define ATT_Y    11


/* stuff used in reading/parsing pattern rows */
int maxi, maxj;                 /* (i,j) offsets of largest element */
int mini, minj;                 /* offset of top-left element
				   (0,0) unless there are edge constraints */
int where;                      /* NORTH_EDGE | WEST_EDGE, etc */
int el;                         /* next element number in current pattern */
struct patval_b elements[MAX_BOARD*MAX_BOARD]; /* elements of current pattern */
int num_stars;

int ci = -1, cj = -1;           /* position of origin (first piece element)
				   relative to top-left */
int patno;		        /* current pattern */
int pats_with_constraints = 0;  /* just out of interest */
int label_coords[256][2];       /* Coordinates for labeled stones in the 
				   autohelper patterns. */
int current_i;		        /* Counter for the line number of the
				   diagram */ 
int current_c_i;		/* Counter for the line number of a 
				   constraint diagram. */
char constraint[MAXCONSTRAINT]; /* Store constraint lines. */
char action[MAXCONSTRAINT];     /* Store action lines. */
static char diagram[MAX_BOARD+2][MAX_BOARD+3];
				/* store pattern diagram*/
static char constraint_diagram[MAX_BOARD+2][MAX_BOARD+3];
				/* store pattern constraint diagram */

/* stuff to maintain info about patterns while reading */
char *prefix;
struct pattern pattern[MAXPATNO];  /* accumulate the patterns into here */
char pattern_names[MAXPATNO][80];  /* with optional names here, */
char helper_fn_names[MAXPATNO][80]; /* helper fn names here */
char autohelper_code[MAXPATNO*300]; /* code for automatically generated */
                                    /* helper functions here */
char *code_pos;                     /* current position in code buffer */
struct autohelper_func {
  const char *name;
  int params;
  int type;	/* 0 - just copy the parameters,
		 * 1 - add parameter count,
		 * 2 - add address of the current pattern.
		 */
  float cost;
  const char *code;
};


/*
 * current_* are useful for debugging broken patterns.
 */
const char *current_file = 0;
int current_line_number = 0;

/* ================================================================ */
/*                                                                  */
/*                Autohelper function definitions                   */
/*                                                                  */
/* ================================================================ */

/* Important notice:
 * If one function has a name which is a prefix of another name, the
 * shorter name must come later in the list. E.g. "lib" must be preceded
 * by "lib2", "lib3", and "lib4".
 */
static struct autohelper_func autohelper_functions[] = {
  {"lib2",			1, 0, 0.01, "worm[%s].liberties2"},
  {"lib3",			1, 0, 0.01, "worm[%s].liberties3"},
  {"lib4",			1, 0, 0.01, "worm[%s].liberties4"},
  {"goallib",			0, 0, 0.01, "goallib"},
  {"lib",			1, 0, 0.01, "countlib(%s)"},
  {"alive",			1, 0, 0.01,
		"(dragon[%s].status == ALIVE)"},
  {"unknown",			1, 0, 0.01,
		"(dragon[%s].status == UNKNOWN)"},
  {"critical",			1, 0, 0.01,
		"(dragon[%s].status == CRITICAL)"},
  {"dead",			1, 0, 0.01, "(dragon[%s].status == DEAD)"},
  {"status",			1, 0, 0.01, "dragon[%s].status"},
  {"ko", 			1, 0, 0.01, "is_ko_point(%s)"},
  {"xdefend_against",		2, 0, 1.00,
		"defend_against(%s, OTHER_COLOR(color), %s)"},
  {"odefend_against",		2, 0, 1.00, "defend_against(%s, color, %s)"},
  {"defend_against_atari",	1, 0, 1.00,
		"defend_against_atari_helper(move, %s)"},
  {"does_defend",		2, 0, 1.00, "does_defend(%s, %s)"},
  {"does_attack",		2, 0, 1.00, "does_attack(%s, %s)"},
  {"attack",			1, 0, 1.00, "ATTACK_MACRO(%s)"},
  {"defend",			1, 0, 1.00, "DEFEND_MACRO(%s)"},
  {"weak",			1, 0, 0.01, "DRAGON_WEAK(%s)"},
  {"safe_xmove", 		1, 0, 1.00, "safe_move(%s, OTHER_COLOR(color))"},
  {"safe_omove", 		1, 0, 1.00, "safe_move(%s, color)"},
  {"legal_xmove",		1, 0, 0.05, "is_legal(%s, OTHER_COLOR(color))"},
  {"legal_omove",		1, 0, 0.05, "is_legal(%s, color)"},
  {"x_suicide",			1, 0, 0.05, "is_suicide(%s, OTHER_COLOR(color))"},
  {"o_suicide",			1, 0, 0.05, "is_suicide(%s, color)"},
  {"x_alive_somewhere",		0, 1, 0.01,
		"somewhere(OTHER_COLOR(color), 1, %d"},
  {"o_alive_somewhere",		0, 1, 0.01, "somewhere(color, 1, %d"},
  {"x_somewhere",		0, 1, 0.01,
		"somewhere(OTHER_COLOR(color), 0, %d"},
  {"o_somewhere",		0, 1, 0.01, "somewhere(color, 0, %d"},
  {"xmoyo_opposite",		1, 0, 0.01,
      "(whose_moyo(&INITIAL_INFLUENCE(color), %s) == OTHER_COLOR(color))"},
  {"omoyo_opposite",		1, 0, 0.01,
      "(whose_moyo(&INITIAL_INFLUENCE(color), %s) == color)"},
  {"xmoyo",			1, 0, 0.01,
      "(whose_moyo(&OPPOSITE_INFLUENCE(color), %s) == OTHER_COLOR(color))"},
  {"omoyo",			1, 0, 0.01,
      "(whose_moyo(&OPPOSITE_INFLUENCE(color), %s) == color)"},
  {"xarea",			1, 0, 0.01,
      "(whose_area(&OPPOSITE_INFLUENCE(color), %s) == OTHER_COLOR(color))"},
  {"oarea",			1, 0, 0.01, 
      "(whose_area(&OPPOSITE_INFLUENCE(color), %s) == color)"},
  {"xterri",			1, 0, 0.01,
      "(whose_territory(&OPPOSITE_INFLUENCE(color), %s) == OTHER_COLOR(color))"},
  {"oterri",			1, 0, 0.01,
      "(whose_territory(&OPPOSITE_INFLUENCE(color), %s) == color)"},
  {"genus",			1, 0, 0.01, "dragon[%s].genus"},
  {"approx_xlib",		1, 0, 0.03,
		"approxlib(%s, OTHER_COLOR(color), MAX_LIBERTIES, NULL)"},
  {"approx_olib",		1, 0, 0.03,
		"approxlib(%s, color, MAX_LIBERTIES, NULL)"},
  {"xlib",			1, 0, 0.05,
	"accuratelib(%s, OTHER_COLOR(color), MAX_LIBERTIES, NULL)"},
  {"olib",			1, 0, 0.05,
	"accuratelib(%s, color, MAX_LIBERTIES, NULL)"},
  {"xcut",			1, 0, 0.01,
    	"cut_possible(%s, OTHER_COLOR(color))"},
  {"ocut",			1, 0, 0.05, "cut_possible(%s, color)"},
  {"edge_double_sente", 	4, 1, 3.00,
		"edge_double_sente_helper(%s, %s, %s, %s)"},
  {"xplay_defend_both",		2, 1, 3.00,
		"play_attack_defend2_n(OTHER_COLOR(color), 0, %d"},
  {"oplay_defend_both",		2, 1, 3.00, "play_attack_defend2_n(color, 0, %d"},
  {"xplay_attack_either",	2, 1, 3.00,
		"play_attack_defend2_n(OTHER_COLOR(color), 1, %d"},
  {"oplay_attack_either",	2, 1, 3.00, "play_attack_defend2_n(color, 1, %d"},
  {"xplay_defend",		1, 1, 1.00,
		"play_attack_defend_n(OTHER_COLOR(color), 0, %d"},
  {"oplay_defend",		1, 1, 1.00, "play_attack_defend_n(color, 0, %d"},
  {"xplay_attack",		1, 1, 1.00,
		"play_attack_defend_n(OTHER_COLOR(color), 1, %d"},
  {"oplay_attack",		1, 1, 1.00, "play_attack_defend_n(color, 1, %d"},
  {"xplay_break_through",	3, 1, 5.00,
		"play_break_through_n(OTHER_COLOR(color), %d"},
  {"oplay_break_through",	3, 1, 5.00, "play_break_through_n(color, %d"},
  {"oplay_connect",		2, 1,10.00, "play_connect_n(color, 1, %d"},
  {"xplay_connect",		2, 1,10.00,
		"play_connect_n(OTHER_COLOR(color), 1, %d"},
  {"oplay_disconnect",		2, 1,10.00, "play_connect_n(color, 0, %d"},
  {"xplay_disconnect",		2, 1,10.00,
		"play_connect_n(OTHER_COLOR(color), 0, %d"},
  {"seki_helper",		1, 0, 0.0, "seki_helper(%s)"},
  {"threaten_to_save",		1, 0, 0.0, "threaten_to_save_helper(move,%s)"},
  {"threaten_to_capture",	1, 0, 0.0, "threaten_to_capture_helper(move,%s)"},
  {"not_lunch",			2, 0, 0.0, "not_lunch_helper(%s, %s)"},
  {"eye",			1, 0, 0.01, "is_eye_space(%s)"},
  {"proper_eye", 		1, 0, 0.01, "is_proper_eye_space(%s)"},
  {"marginal_eye",		1, 0, 0.01, "is_marginal_eye_space(%s)"},
  {"halfeye",			1, 0, 0.01, "is_halfeye(half_eye,%s)"},
  {"max_eye_value",		1, 0, 0.01, "max_eye_value(%s)"},
  {"owl_topological_eye",	2, 0, 0.01, "owl_topological_eye(%s, board[%s])"},
  {"obvious_false_oeye",	1, 0, 0.01, "obvious_false_eye(%s, color)"},
  {"obvious_false_xeye",	1, 0, 0.01,
		"obvious_false_eye(%s, OTHER_COLOR(color))"},
  {"antisuji",			1, 0, 0.0, "add_antisuji_move(%s)"},
  {"add_connect_move",		2, 0, 0.0, "add_connection_move(move,%s, %s)"},
  {"add_cut_move",		2, 0, 0.0, "add_cut_move(move, %s, %s)"},
  {"test_attack_either_move",	2, 0, 0.0,
		"test_attack_either_move(move, color, %s, %s)"},
  {"add_defend_both_move",	2, 0, 0.0,
		"add_all_move(move, DEFEND_STRING, %s, DEFEND_STRING, %s)"},
  {"same_dragon",		2, 0, 0.01, "is_same_dragon(%s, %s)"},
  {"same_string",		2, 0, 0.01, "same_string(%s, %s)"},
  {"dragonsize", 		1, 0, 0.01, "dragon[%s].size"},
  {"wormsize",			1, 0, 0.01, "countstones(%s)"},
  {"effective_size",		1, 0, 0.01, "dragon[%s].effective_size"},
  {"vital_chain",		1, 0, 0.05, "vital_chain(%s)"},
  {"potential_cutstone",	1, 0, 0.01, "worm[%s].cutstone2 > 1"},
  {"amalgamate_most_valuable_helper", 3, 0, 0.0,
   		"amalgamate_most_valuable_helper(%s, %s, %s)"},
  {"amalgamate", 		2, 0, 0.0, "join_dragons(%s, %s)"},
  {"owl_escape_value",		1, 0, 0.01, "owl_escape_value(%s)"},
  {"owl_goal_dragon",		1, 0, 0.01, "owl_goal_dragon(%s)"},
  {"owl_eyespace",		1, 0, 0.01, "owl_eyespace(%s)"},
  {"owl_big_eyespace",		1, 0, 0.01, "owl_big_eyespace(%s)"},
  {"owl_proper_eye",		1, 0, 0.01, "owl_proper_eye(%s)"},
  {"owl_strong_dragon",		1, 0, 0.01, "owl_strong_dragon(%s)"},
  {"has_aji",			1, 0, 0.01,
		"(dragon[%s].owl_threat_status == CAN_THREATEN_DEFENSE)"},
  {"finish_ko_helper",		1, 0, 0.05, "finish_ko_helper(%s)"},
  {"squeeze_ko_helper",		1, 0, 0.03, "squeeze_ko_helper(%s)"},
  {"backfill_helper",		3, 0, 1.50, "backfill_helper(%s, %s, %s)"},
  {"connect_and_cut_helper2",	3, 0, 3.00,
		"connect_and_cut_helper2(%s, %s, %s, color)"},
  {"connect_and_cut_helper",	3, 0, 3.00, "connect_and_cut_helper(%s, %s, %s)"},
  {"owl_threatens",		2, 0, 0.01, "owl_threatens_attack(%s, %s)"},
  {"replace",			2, 0, 0.0,  "add_replacement_move(%s, %s)"},
  {"non_oterritory",		1, 0, 0.0,
		"influence_mark_non_territory(%s, color)"},
  {"non_xterritory",		1, 0, 0.0,
		"influence_mark_non_territory(%s, OTHER_COLOR(color))"},
  {"remaining_handicap_stones",	0, 0, 0.0,  "free_handicap_remaining_stones()"},
  {"total_handicap_stones",	0, 0, 0.0,  "free_handicap_total_stones()"},
  {"o_captures_something", 	1, 0, 0.02, "does_capture_something(%s, color)"},
  {"x_captures_something", 	1, 0, 0.02,
		"does_capture_something(%s, OTHER_COLOR(color))"},
  {"false_eye_territory",	1, 0, 0.0, "false_eye_territory[%s]"},
  {"false_eye",			1, 0, 0.01, "is_false_eye(half_eye, %s)"},
  {"is_surrounded",		1, 0, 0.01, "is_surrounded(%s)"},
  {"does_surround",		2, 0, 1.00, "does_surround(%s, %s)"},
  {"surround_map",		2, 0, 0.01, "surround_map(%s, %s)"},
  {"oracle_threatens",		2, 0, 0.01, "oracle_threatens(%s, %s)"},
  {"value",			0, 2, 0.0,  "(%s->value)"}
};


/* To get a valid function pointer different from NULL. */
static int
dummyhelper(int transformation, int move, int color, int action)
{
  UNUSED(transformation); UNUSED(move); UNUSED(color);
  UNUSED(action);
  return 0;
}


#define PREAMBLE "\
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n\
 * This is GNU GO, a Go program. Contact gnugo@gnu.org, or see       *\n\
 * http://www.gnu.org/software/gnugo/ for more information.          *\n\
 *                                                                   *\n\
 * Copyright 1999, 2000, 2001, 2002 by the Free Software Foundation. *\n\
 *                                                                   *\n\
 * This program is free software; you can redistribute it and/or     *\n\
 * modify it under the terms of the GNU General Public License as    *\n\
 * published by the Free Software Foundation - version 2             *\n\
 *                                                                   *\n\
 * This program is distributed in the hope that it will be useful,   *\n\
 * but WITHOUT ANY WARRANTY; without even the implied warranty of    *\n\
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     *\n\
 * GNU General Public License in file COPYING for more details.      *\n\
 *                                                                   *\n\
 * You should have received a copy of the GNU General Public         *\n\
 * License along with this program; if not, write to the Free        *\n\
 * Software Foundation, Inc., 59 Temple Place - Suite 330,           *\n\
 * Boston, MA 02111, USA.                                            *\n\
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */\n\n\
#include <stdio.h> /* for NULL */\n\
#include \"liberty.h\"\n\
#include \"patterns.h\"\n\n\
"

int fatal_errors = 0;

/* options */
int verbose = 0;	/* -v */
int database_type = 0;  /* -p (default), -c, -f, -C, -D or -T */
int anchor = 0; 	/* Whether both O and/or X may be anchors.
			* -b for both. -X for only X.
			*/

int choose_best_anchor = 0;  /* -m */
int fixed_anchor = 0;        /* -a */
int pre_rotate = 0;          /* -p */

dfa_t dfa;

/**************************
 *
 * stuff to check the constraint diagram
 *
 **************************/

#define CHECK_CHARS "xXoO"
static void
check_constraint_diagram(void)
{
  int i, j, ino = 0, iso = 0, jwo = 0, jeo = 0;

  int have_constraint = (pattern[patno].autohelper_flag & HAVE_CONSTRAINT);
  if (0)
    fprintf(stderr, "patno: %d\n", patno);
    
  if (where & NORTH_EDGE)
    ino = 1;
  if (where & SOUTH_EDGE)
    iso = 1;
  if (where & WEST_EDGE)
    jwo = 1;
  if (where & EAST_EDGE)
    jeo = 1;
    
  if (verbose) {
    for (i = ino; i <= maxi+ino+iso; i++)
      fprintf(stderr, "%02d %s\n", i, diagram[i]);
    for (i = ino; i <= maxi+ino+iso; i++)
      fprintf(stderr, "%02d %s\n", i, constraint_diagram[i]);
  }
  
  if (0)
    fprintf(stderr, "have_constraint: %d\n", have_constraint);
  if (have_constraint) {
    for (i = ino; i <= maxi+ino; i++)
      for (j = jwo; j <= maxj+jwo; j++) {
	if (0)
	  fprintf(stderr,"%2d %2d %c %c\n", i, j, constraint_diagram[i][j], 
		  diagram[i][j]);
	if (strchr(CHECK_CHARS, constraint_diagram[i][j])
	    && constraint_diagram[i][j] != diagram[i][j]) {
	  fprintf(stderr, "%s(%d) : Error : "
                  "xXoO not matched in constraint diagram of pattern %s\n",
		  current_file, current_line_number, pattern_names[patno]);
	  fatal_errors++;
	}
    }
  }
}

/**************************
 *
 * stuff to parse the input
 *
 **************************/

/* reset state of all pattern variables */
static void
reset_pattern(void)
{
  int i, j;

  maxi = 0;
  maxj = 0;
  ci = -1;
  cj = -1;
  where = 0;
  el = 0;
  num_stars = 0;
  strcpy(helper_fn_names[patno], "NULL");
  for (i = 0; i < 256; i++)
    label_coords[i][0] = -1;
  current_i = 0;
  current_c_i = 0;
  constraint[0] = 0;
  action[0] = 0;
  for (i = 0; i < MAX_BOARD+2; i++) {
    for (j = 0; j < MAX_BOARD+3; j++) {
      diagram[i][j] = '\0';
      constraint_diagram[i][j] = '\0';
    }
  }
}
  


/* this is called to compute the extents of the pattern, applying
 * edge constraints as necessary
 */

static void
find_extents(void)
{

  /* When this is called, elements go from (mini,minj) inclusive to
   * (maxi-1, maxj-1) [ie exclusive]. Make them inclusive.
   * Ie maximum element lies on (maxi,maxj).
   */
  
  --maxi;
  --maxj;

  /* apply edge constraints to the size of the pattern */

  if (where & (NORTH_EDGE|SOUTH_EDGE|EAST_EDGE|WEST_EDGE))
    ++pats_with_constraints;

  if (verbose)
    fprintf(stderr, "Pattern %s has constraints 0x%x\n",
	    pattern_names[patno], where);

  pattern[patno].edge_constraints = where;


  /* At this point, (mini,minj) -> (maxi,maxj) contain
   * the extent of the pattern, relative to top-left
   * of pattern, rather than (ci,cj).
   *
   * But we store them in the output file relative
   * to (ci,cj), so that we can transform the corners
   * of the pattern like any other relative co-ord.
   */

  pattern[patno].mini = mini - ci;
  pattern[patno].minj = minj - cj;
  pattern[patno].maxi = maxi - ci;
  pattern[patno].maxj = maxj - cj;
}


/*
 * Here we build the dfa.
 */

static void
write_to_dfa(int index)
{
  char str[MAX_ORDER+1];
  float ratio;
  int ll;
  int rot_start = 0;
  int rot_stop = 1;
  
  assert(ci != -1 && cj != -1);
#if 0
  pattern[index].patn = elements; /* a little modification : keep in touch! */
#endif
  pattern[index].name = &(pattern_names[index][0]); 

  if (verbose)
    fprintf(stderr, "Add   :%s\n", pattern[index].name);

  /* First we create the string from the actual pattern */
  pattern_2_string(pattern+index, elements, str, 0, ci, cj);
      
  if (pre_rotate) {
    if (pattern[index].trfno != 5) {
      rot_stop = pattern[index].trfno;
    } else {
      rot_start = 2;
      rot_stop = 6;
    }
  }
  for (ll = 0; ll < rot_stop; ll++) {
    /* Then We add this string to the DFA */
    ratio = (dfa_add_string(&dfa, str, index, ll) - 1)*100;
 
    /* Complain when there is more than 10% of increase */ 
    if (dfa_size(&dfa) > 100 && ratio > 10.0) {
      fprintf(stderr, "Pattern %s => %3.1f%% increase: ",
	      pattern[index].name, ratio);
      fprintf(stderr, "another orientation may save memory.\n");
    }
    if (dfa_verbose > 2)
      dump_dfa(stderr, &dfa);
  }
}


/* For good performance, we want to reject patterns as quickly as
 * possible. For each pattern, this combines 16 positions around
 * the anchor stone into a 32-bit mask and value. In the matcher,
 * the same 4x4 grid is precomputed, and then we can quickly
 * test 16 board positions with one test.
 * See matchpat.c for details of how this works - basically, if
 * we AND what is on the board with the and_mask, and get the
 * value in the val_mask, we have a match. This test can be
 * applied in parallel : 2 bits per posn x 16 posns = 32 bits.
 * "Don't care" has and_mask = val_mask = 0, which is handy !
 * FIXME: Looks like element "." (don't care) has and_mask 3, not 0,
 *    as indicated in the comments above.
 */

static void
compute_grids(void)
{
#if GRID_OPT
  /*                              element: .  X  O  x  o  ,  a  ! */
  static const unsigned int and_mask[] = { 3, 3, 3, 1, 2, 3, 3, 1 };
  static const unsigned int val_mask[] = { 0, 2, 1, 0, 0, 0, 0, 0 };

  int ll;  /* iterate over rotations */
  int k;   /* iterate over elements */

  for (ll = 0; ll < 8; ++ll) {
    for (k = 0; k < el; ++k) {
      int di, dj;

      TRANSFORM2(elements[k].x - ci, elements[k].y - cj, &di, &dj, ll);
      ++di;
      ++dj;
      if (di >= 0 && di < 4 && dj >= 0 && dj < 4) {
	pattern[patno].and_mask[ll]
	  |= and_mask[elements[k].att] << (30 - di * 8 - dj * 2);
	pattern[patno].val_mask[ll]
	  |= val_mask[elements[k].att] << (30 - di * 8 - dj * 2);
      }
    }
  }
#endif
}



/* We've just read a line that looks like a pattern line.
 * Now process it.
 */

static void
read_pattern_line(char *p)
{
  const char *char_offset;
  char *pcopy = p;
  int j;
  int width;
  int jwo = 0, jeo = 0;

  if (where & SOUTH_EDGE)
    /* something wrong here : pattern line after a SOUTH_EDGE constraint */
    goto fatal;


  if (*p == '+' || *p == '-') {
    /* must be a north/south constraint */

    if (maxi == 0)
      where |= NORTH_EDGE;
    else
      where |= SOUTH_EDGE;

    if (*p == '+') {
      if (maxi > 0 && !(where & WEST_EDGE))
	/* if this is end of pattern, must already know about west */
	goto fatal;

      where |= WEST_EDGE;
      ++p;
    }

    /* count the number of -'s */
    for (width = 0; *p == '-' ; ++p, ++width)
      ;

    if (width == 0)
      goto fatal;

    if (*p == '+') {
      if (maxi > 0 && !(where & EAST_EDGE))
	/* if this is end of pattern, must already know about west */
	goto fatal;
      where |= EAST_EDGE;
    }

    if (maxi > 0 && width != maxj)
      goto notrectangle;

    return;
  }

  /* get here => its a real pattern entry, 
   * rather than a north/south constraint 
   */

  /* we have a pattern line - add it into the current pattern */
  if (*p == '|') {
    /* if this is not the first line, or if there is a north
     * constraint, we should already know about it
     */
    if (!(where & WEST_EDGE) && ((where & NORTH_EDGE) || maxi > 0))
      /* we should already know about this constraint */
      goto fatal;

    where |= WEST_EDGE;
    ++p;
  }
  else if (where & WEST_EDGE)
    /* need a | if we are already constrained to west */
    goto fatal;


  for (j = 0; 
       (char_offset = strchr(VALID_PATTERN_CHARS, *p)) != NULL;
       ++j, ++p) {

    /* char_offset is a pointer within the VALID_PATTERN_CHARS string.
     * so  (char-VALID_PATTERN_CHARS) is the att (0 to 11) to write to the
     * pattern element
     */

    /* one of ATT_* - see above */
    int off = char_offset - VALID_PATTERN_CHARS;

    if (off == ATT_wild)
      continue;  /* boring - pad character */

    if (off == ATT_a) /* this were used by halfeye patterns */
      goto fatal;

    if (off == ATT_star) {
      /* '*' */
      pattern[patno].move_offset = OFFSET(maxi, j);
      ++num_stars;
      off = ATT_dot;  /* add a '.' to the pattern instead */
    }

    if (off == ATT_Q) {
      off = ATT_O;
      ci = maxi;
      cj = j;
      pattern[patno].anchored_at_X = (off == ATT_X) ? 3 : 0;
      /*FIXME: Make sure O is valid anchor*/
    }

    if (off == ATT_Y) {
      off = ATT_X;
      ci = maxi;
      cj = j;
      pattern[patno].anchored_at_X = (off == ATT_X) ? 3 : 0;
      /*FIXME: Make sure X is valid anchor*/
    }

    assert(off <= ATT_not);

	
    if ((ci == -1) && (off < 3) && ((off & anchor) != 0)) {
      /* Use this position as the pattern origin. */
      ci = maxi;
      cj = j;
      pattern[patno].anchored_at_X = (off == ATT_X) ? 3 : 0;
    }

    /* Special limitations for fullboard patterns. */
    if (database_type == DB_FULLBOARD) {
      if (off == ATT_dot)
	continue;
      assert(off == ATT_X || off == ATT_O);
    }
    
    /* Range checking. */
    assert(el < (int) (sizeof(elements) / sizeof(elements[0])));
    
    elements[el].x = maxi;
    elements[el].y = j;
    elements[el].att = off;  /* '*' mapped to '.' and 'Q' to 'O' above */
    ++el;
  }

  if (*p == '|') {

    /* if this is not the first line, or if there is a north
     * constraint, we should already know about it
     */
    if (!(where & EAST_EDGE) && ((where & NORTH_EDGE) || maxi > 0))
      goto fatal;  /* we should already know about this constraint */

    where |= EAST_EDGE;

  }
  else if (where & EAST_EDGE)
    goto fatal;  /* need a | if we are already constrained to east */


  if (maxi > 0 && j != maxj)
    goto notrectangle;

  if (j > maxj)
    maxj = j;


  if (where & WEST_EDGE)
    jwo = 1;
  if (where & EAST_EDGE)
    jeo = 1;
  strncpy(diagram[maxi], pcopy, maxj + jwo + jeo);
  maxi++;

  return;

fatal:
 fprintf(stderr, "%s(%d) : error : Illegal pattern %s\n", 
         current_file, current_line_number, pattern_names[patno]);
 fatal_errors = 1;
 return;

notrectangle:
 fprintf(stderr, "%s(%d) : error : Pattern %s not rectangular\n", 
	 current_file, current_line_number, pattern_names[patno]);
 fatal_errors++;
 return;
}


/*
 * We've just read a line that looks like a constraint pattern line.
 * Now process it.
 */

static void
read_constraint_diagram_line(char *p)
{
  int j;
  int jwo = 0, jeo = 0;
  const char *pcopy = p;

  /* North or south boundary, no letter to be found. */
  if (*p == '+' || *p == '-')
    return;

  /* Skip west boundary. */
  if (*p == '|')
    p++;
  
  for (j = 0; 
       strchr(VALID_PATTERN_CHARS, *p) || strchr(VALID_CONSTRAINT_LABELS, *p);
       ++j, ++p) {
    if (strchr(VALID_CONSTRAINT_LABELS, *p) 
	&& label_coords[(int)*p][0] == -1) {

      /* New constraint letter */
      label_coords[(int)*p][0] = current_c_i;
      label_coords[(int)*p][1] = j;
    }
  }

  /* Now j holds the width of this constraint diagram line. Require
   * this to match the main diagram width stored in maxj. However,
   * maxj was modified by find_extents() so we have to compensate for
   * this.
   */
  if (j != maxj + 1) {
    fprintf(stderr, "%s(%d) : error : Mismatching width of constraint line in pattern %s\n", 
	    current_file, current_line_number, pattern_names[patno]);
    fatal_errors++;
    return;
  }

  if (where & WEST_EDGE)
    jwo = 1;
  if (where & EAST_EDGE)
    jeo = 1;
  strncpy(constraint_diagram[current_c_i], pcopy, maxj+jwo+jeo+1);
  current_c_i++;

  return;
}

/* Check that the constraint diagram had the same number of rows as
 * the main diagram.
 */
static void
check_constraint_diagram_size(void)
{
  if (current_c_i != maxi + 1) {
    fprintf(stderr, "%s(%d) : error : Mismatching height of constraint diagram in pattern %s\n", 
	    current_file, current_line_number, pattern_names[patno]);
    fatal_errors++;
  }
}  

/* On reading a line starting ':', finish up and write
 * out the current pattern 
 */

static void
finish_pattern(char *line)
{

  /* end of pattern layout */
  char symmetry;		/* the symmetry character */
  
  mini = minj = 0; /* initially : can change with edge-constraints */

  if (num_stars > 1 || (database_type != DB_CONNECTIONS && num_stars == 0)) {
    fprintf(stderr, "%s(%d) : error : No or too many *'s in pattern %s\n",
	    current_file, current_line_number, pattern_names[patno]);
    fatal_errors = 1;
  }

  if (database_type == DB_FULLBOARD) {
    /* For fullboard patterns, the "anchor" is always at the mid point. */
    ci = (maxi-1)/2;
    cj = (maxj-1)/2;
  }
  else if (choose_best_anchor) { 

    /* Try to find a better anchor if
     * the -m option is set.
     */
    int mi, mj; /* middle */
    int d, min_d = 36100;
    int k, min_k = -1;
      
    /* We seek the element of suitable value minimizing
     * the distance to the middle.
     */
    mi = (maxi - 1) * 50;
    mj = (maxj - 1) * 50 - 1;
    for (k = 0; k != el; k++)
      if (elements[k].att < 3 && (elements[k].att & anchor) != 0) {
	d = gg_abs(100 * elements[k].x - mi)
	  + gg_abs(100 * elements[k].y - mj);
	if (d < min_d) {
	  min_k = k;
	  min_d = d;
	}
      }
    assert(min_k != -1);
    ci = elements[min_k].x;
    cj = elements[min_k].y;
    pattern[patno].anchored_at_X = (elements[min_k].att == ATT_X) ? 3 : 0;
    
  }
  else if (ci == -1 || cj == -1) {
    fprintf(stderr, "%s(%d) : No origin for pattern %s\n", 
            current_file, current_line_number, pattern_names[patno]);
    fatal_errors = 1;
    ci = 0;
    cj = 0;
  }

  /* translate posn of * (or Q) to relative co-ords
   */

  if (num_stars == 1) {
    pattern[patno].move_offset -= OFFSET_DELTA(ci, cj);
  }
  else if (num_stars == 0) {
    pattern[patno].move_offset = OFFSET(ci, cj);
  }

  find_extents();

  compute_grids();

  pattern[patno].patlen = el;

  /* Now parse the line. Only the symmetry character and the class
   * field are mandatory. The compiler guarantees that all the fields
   * are already initialized to 0.
   */

  {
    int s;
    char class[80];
    char entry[80];
    char *p = line;
    int n;
    float v = 0.0;
    
    class[0] = 0;  /* in case sscanf doesn't get that far */
    s = sscanf(p, ":%c,%[^,]%n", &symmetry, class, &n);
    p += n;
    
    if (s < 2) {
      fprintf(stderr, ": line must contain symmetry character and class\n");
      fatal_errors++;
    }

    while (sscanf(p, "%*[, ]%[^,]%n", entry, &n) > 0) {
      p += n;

      if (sscanf(entry, "%*[^(](%f)", &v) > 0) {
	if (strncmp(entry, "value", 5) == 0
	    || strncmp(entry, "minvalue", 8) == 0) {
	  pattern[patno].value = v;
	  pattern[patno].class |= VALUE_MINVAL;
	}
	else if (strncmp(entry, "maxvalue", 8) == 0) {
	  pattern[patno].maxvalue = v;
	  pattern[patno].class |= VALUE_MAXVAL;
	}
	else if (strncmp(entry, "terri", 5) == 0
		 || strncmp(entry, "minterri", 8) == 0) {
	  pattern[patno].minterritory = v;
	  pattern[patno].class |= VALUE_MINTERRITORY;
	}
	else if (strncmp(entry, "maxterri", 8) == 0) {
	  pattern[patno].maxterritory = v;
	  pattern[patno].class |= VALUE_MAXTERRITORY;
	}
	else if (strncmp(entry, "shape", 5) == 0) {
	  pattern[patno].shape = v;
	  pattern[patno].class |= VALUE_SHAPE;
	}
	else if (strncmp(entry, "followup", 8) == 0) {
	  pattern[patno].followup = v;
	  pattern[patno].class |= VALUE_FOLLOWUP;
	}
	else if (strncmp(entry, "reverse_followup", 16) == 0) {
	  pattern[patno].reverse_followup = v;
	  pattern[patno].class |= VALUE_REV_FOLLOWUP;
	}
	else {
	  fprintf(stderr, "%s(%d) : error : Unknown value field: %s\n", 
                  current_file, current_line_number, entry);
          fatal_errors++;
	  break;
	}
      }
      else {
	strncpy(helper_fn_names[patno], entry, 79);
	break;
      }
    }
      
    {
      char *p;
      for (p = class; *p; p++) {
	switch (*p) {
	  case 's': pattern[patno].class |= CLASS_s; break;
	  case 'O': pattern[patno].class |= CLASS_O; break;
	  case 'o': pattern[patno].class |= CLASS_o; break;
	  case 'X': pattern[patno].class |= CLASS_X; break;
	  case 'x': pattern[patno].class |= CLASS_x; break;
	  case 'D': pattern[patno].class |= CLASS_D; break;
	  case 'C': pattern[patno].class |= CLASS_C; break;
	  case 'c': pattern[patno].class |= CLASS_c; break;
	  case 'n': pattern[patno].class |= CLASS_n; break;
	  case 'B': pattern[patno].class |= CLASS_B; break;
	  case 'A': pattern[patno].class |= CLASS_A; break;
	  case 'b': pattern[patno].class |= CLASS_b; break;
	  case 'e': pattern[patno].class |= CLASS_e; break;
	  case 'E': pattern[patno].class |= CLASS_E; break;
	  case 'a': pattern[patno].class |= CLASS_a; break;
	  case 'd': pattern[patno].class |= CLASS_d; break;
	  case 'I': pattern[patno].class |= CLASS_I; break;
	  case 'J': pattern[patno].class |= CLASS_J; break;
	  case 'j': pattern[patno].class |= CLASS_j; break;
	  case 't': pattern[patno].class |= CLASS_t; break;
	  case 'T': pattern[patno].class |= CLASS_T; break;
	  case 'U': pattern[patno].class |= CLASS_U; break;
	  case 'W': pattern[patno].class |= CLASS_W; break;
	  case 'F': pattern[patno].class |= CLASS_F; break;
	  case 'Y': pattern[patno].class |= CLASS_Y; break;
	  case '-': break;
	  default:
	    if (!isgraph((int) *p))
	      break;
	    fprintf(stderr,
		    "%s(%d) : error : Unknown classification letter %c. (pattern %s).\n", 
		    current_file, current_line_number, *p,
		    pattern_names[patno]);
	    fatal_errors++;
	    break;
	}
      }
    }
  }

      
  /* Now get the symmetry. There are extra checks we can make to do with
   * square-ness and edges. We do this before we work out the edge constraints,
   * since that mangles the size info.
   */
  
  switch (symmetry) {
  case '+' :
    if (where & (NORTH_EDGE|EAST_EDGE|SOUTH_EDGE|WEST_EDGE))
      fprintf(stderr,
	      "%s(%d) : Warning : symmetry inconsistent with edge constraints (pattern %s)\n", 
	      current_file, current_line_number, pattern_names[patno]);
    pattern[patno].trfno = 2;
    break;

  case 'X' : 
    if (where & (NORTH_EDGE|EAST_EDGE|SOUTH_EDGE|WEST_EDGE))
      fprintf(stderr,
	      "%s(%d) : Warning : X symmetry inconsistent with edge constraints (pattern %s)\n", 
	      current_file, current_line_number, pattern_names[patno]);
    if (maxi != maxj)
      fprintf(stderr,
	      "%s(%d) : Warning : X symmetry requires a square pattern (pattern %s)\n",
	      current_file, current_line_number, pattern_names[patno]);
    pattern[patno].trfno = 2;
    break;

  case '-' :
    if (where & (NORTH_EDGE|SOUTH_EDGE))
      fprintf(stderr,
	      "%s(%d) : Warning : symmetry inconsistent with edge constraints (pattern %s)\n", 
	      current_file, current_line_number, pattern_names[patno]);
    pattern[patno].trfno = 4;
    break;
    
  case '|' :
    if (where & (EAST_EDGE|WEST_EDGE))
      fprintf(stderr,
	      "%s(%d) : Warning : symmetry inconsistent with edge constraints (pattern %s)\n", 
	      current_file, current_line_number, pattern_names[patno]);
    pattern[patno].trfno = 4;
    break;

  case '\\' :
  case '/' :
    /* FIXME: Can't be bothered putting in the edge tests.
    *         (What does this mean?)
    */
    if (maxi != maxj)
      fprintf(stderr,
	      "%s(%d) : Warning : \\ or / symmetry requires a square pattern (pattern %s)\n", 
	      current_file, current_line_number, pattern_names[patno]);

    pattern[patno].trfno = 4;
    break;

  case 'O' :
    if (where & (NORTH_EDGE|EAST_EDGE|SOUTH_EDGE|WEST_EDGE))
      fprintf(stderr,
	      "%s(%d) : Warning : symmetry inconsistent with edge constraints (pattern %s)\n", 
	      current_file, current_line_number, pattern_names[patno]);
    pattern[patno].trfno = 5;  /* Ugly special convention. */
    break;

  default:
    fprintf(stderr,
	    "%s(%d) : Warning : symmetry character '%c' not implemented - using '8' (pattern %s)\n", 
	    current_file, current_line_number, symmetry, pattern_names[patno]);
    /* FALLTHROUGH */
  case '8' :
    pattern[patno].trfno = 8;
    break;
  }

}
      

static void
read_constraint_line(char *line)
{
  /* Avoid buffer overrun. */
  assert(strlen(constraint) + strlen(line) < MAXCONSTRAINT);

  /* Append the new line. */
  strcat(constraint, line);

  pattern[patno].autohelper_flag |= HAVE_CONSTRAINT;
}


static void
read_action_line(char *line)
{
  /* Avoid buffer overrun. */
  assert(strlen(action) + strlen(line) < MAXACTION);

  /* Append the new line. */
  strcat(action, line);

  pattern[patno].autohelper_flag |= HAVE_ACTION;
}


static void
generate_autohelper_code(int funcno, int number_of_params, int *labels)
{
  int i;
  char varnames[MAXPARAMS][8];
  char pattern[MAXLINE];

  for (i = 0; i < number_of_params; i++) {
    if (labels[i] == (int) '*')
      sprintf(varnames[i], "move");
    /* The special label '?' represents a tenuki. We replace this
     * with NO_MOVE value.
     */
    else if (labels[i] == (int) '?')
      sprintf(varnames[i], "NO_MOVE");
    else
      sprintf(varnames[i], "%c", labels[i]);
  }

  switch (autohelper_functions[funcno].type) {
  case 0:
    /* A common case. Just use the labels as parameters. */
    switch (number_of_params) {
    case 0:
      code_pos += sprintf(code_pos, autohelper_functions[funcno].code);
      break;
    case 1:
      code_pos += sprintf(code_pos, autohelper_functions[funcno].code,
			  varnames[0]);
      break;
    case 2:
      code_pos += sprintf(code_pos, autohelper_functions[funcno].code,
			  varnames[0], varnames[1]);
      break;
    case 3:
      code_pos += sprintf(code_pos, autohelper_functions[funcno].code,
			  varnames[0], varnames[1], varnames[2]);
      break;
    case 4:
      code_pos += sprintf(code_pos, autohelper_functions[funcno].code,
			  varnames[0], varnames[1], varnames[2],
			  varnames[3]);
      break;
    default:
      fprintf(stderr, "%s(%d) : error : unknown number of parameters (pattern %s)",
	      current_file, current_line_number, pattern_names[patno]);
      fatal_errors++;
    }
    break;
  case 1:
    /* This is a very special case where there is assumed to be a
     * variable number of parameters and these constitute a series of
     * moves to make followed by a final attack or defense test.
     */
    if (number_of_params < autohelper_functions[funcno].params) {
      fprintf(stderr, "%s(%d) : error : too few parameters (pattern %s)",
	      current_file, current_line_number, pattern_names[patno]);
      fatal_errors++;
      return;
    }

    code_pos += sprintf(code_pos, autohelper_functions[funcno].code,
		number_of_params - autohelper_functions[funcno].params);

    for (i = 0; i < number_of_params; i++)
	code_pos += sprintf(code_pos, ", %s", varnames[i]);

    *code_pos++ = ')'; /* Close parenthesis. */
    break;

  default: /* 2 */
    /* A very special case. We add the address of the current pattern
     * before the actual parameters. So far, used only by `value'.
     */
    sprintf(pattern, "(%s + %d)", prefix, patno);

    switch(number_of_params) {
    case 0:
      code_pos += sprintf(code_pos, autohelper_functions[funcno].code,
			  pattern);
      break;
    case 1:
      code_pos += sprintf(code_pos, autohelper_functions[funcno].code,
			  pattern, varnames[0]);
      break;
    case 2:
      code_pos += sprintf(code_pos, autohelper_functions[funcno].code,
			  pattern, varnames[0], varnames[1]);
      break;
    case 3:
      code_pos += sprintf(code_pos, autohelper_functions[funcno].code,
			  pattern, varnames[0], varnames[1], varnames[2]);
      break;
    default:
      fprintf(stderr, "%s(%d) : error : unknown number of parameters (pattern %s)",
	      current_file, current_line_number, pattern_names[patno]);
      fatal_errors++;
    }
  }
}


/* Parse the constraint and generate the corresponding helper code.
 * We use a small state machine.
 */
static void
parse_constraint_or_action(char *line, float *cost)
{
  int state = 0;
  char *p;
  int n = 0;
  int label = 0;
  int labels[MAXLABELS];
  int N = sizeof(autohelper_functions)/sizeof(struct autohelper_func);
  int number_of_params = 0;
  float cost_factor = 1.0;

  *cost = 0.0;
  for (p = line; *p; p++)
  {
    switch (state) {
      case 0: /* Looking for a token, echoing other characters. */
	for (n = 0; n < N; n++) {
	  if (strncmp(p, autohelper_functions[n].name,
		      strlen(autohelper_functions[n].name)) == 0) {
	    state = 1;
	    p += strlen(autohelper_functions[n].name)-1;
	    *cost += autohelper_functions[n].cost * cost_factor;
	    cost_factor *= 0.6;
	    break;
	  }
	}
	if (state == 0 && *p != '\n')
	  *(code_pos++) = *p;
	break;
	
      case 1: /* Token found, now expect a '('. */
	if (*p != '(') {
	  if (autohelper_functions[n].params == 0) {
	    /* We allow to omit parenthesis when using functions which
	     * have no parameters. In any case, you may still place them,
	     * but having to write `value() = 50' is disgusting.
	     */
	    generate_autohelper_code(n, 0, NULL);
	    p--;
	    state = 0;
	    break;
	  }
	  fprintf(stderr,
		  "%s(%d) : error : Syntax error in constraint or action, '(' expected (pattern %s).\n", 
		  current_file, current_line_number, pattern_names[patno]);
	  fatal_errors++;
	  return;
	}
	else {
	  assert(autohelper_functions[n].params <= MAXPARAMS);
	  number_of_params = 0;
	  if (autohelper_functions[n].params != 0
	      || autohelper_functions[n].type == 1)
	    state = 2;
	  else
	    state = 3;
	}
	break;
	
      case 2: /* Time for a label. */
	if ((*p != '*') && (*p != '?') && !strchr(VALID_CONSTRAINT_LABELS, *p)) {
	  if (strchr("XxOo", *p))
	    fprintf(stderr,
		    "%s(%d) : error : '%c' is not allowed as a constraint label.\n",
		    current_file, current_line_number, *p);
	  else
	    fprintf(stderr,
		    "%s(%d) : error : Syntax error in constraint or action, label expected, found '%c'.\n", 
		    current_file, current_line_number, *p);
	  fatal_errors++;
	  return;
	}
	else {
	  if ((*p == '?') && (number_of_params == 0)) {
	    fprintf(stderr,
		    "mkpat: tenuki (?) cannot be the first label (pattern %s)\n", pattern_names[patno]);
	    return;
	  }

	  label = (int) *p;
	  /* The special label '?' represents a tenuki. */
	  if (*p != '*' && *p != '?' && label_coords[label][0] == -1) {
	    fprintf(stderr,
		    "%s(%d) : error : The constraint or action uses a label (%c) that wasn't specified in the diagram (pattern %s).\n", 
		    current_file, current_line_number, label, pattern_names[patno]);
	    fatal_errors++;
	    return;
	  }
	  labels[number_of_params] = label;
	  number_of_params++;
	  state = 3;
	}
	break;

      case 3: /* A ',' or a ')'. */
	if (*p == ',') {
	  if (autohelper_functions[n].type != 1
	      && number_of_params == autohelper_functions[n].params) {
	    fprintf(stderr,
		    "%s(%d) : error : Syntax error in constraint or action, ')' expected (pattern %s).\n",
		    current_file, current_line_number, pattern_names[patno]);
	    return;
	  }
	  if (number_of_params == MAXPARAMS) {
	    fprintf(stderr,
		    "Error in constraint or action, too many parameters. (pattern %s).\n", 
		    pattern_names[patno]);
	    return;
	  }
	  state = 2;
	  break;
	}
	else if (*p != ')') {
	  fprintf(stderr, 
		  "%s(%d) : error : Syntax error in constraint or action, ',' or ')' expected (pattern %s).\n", 
		  current_file, current_line_number, pattern_names[patno]);
	  return;
	}
	else { /* a closing parenthesis */
	  if ((autohelper_functions[n].type != 1)
	      && (number_of_params < autohelper_functions[n].params)) {
	    fprintf(stderr,
		    "%s(%d) : error : Syntax error in constraint or action, %s() requires %d parameters.\n",
		    current_file, current_line_number, autohelper_functions[n].name, autohelper_functions[n].params);
	    fatal_errors++;
	    return;
	  }
	  generate_autohelper_code(n, number_of_params, labels);
	  state = 0;
	}
	break;
	
      default:
	fprintf(stderr,
		"%s(%d) : error : Internal error in parse_constraint_or_action(), unknown state.\n",
		current_file, current_line_number);
        fatal_errors++;
	return;
	       
    }
  }
}

/* Finish up a constraint and/or action and generate the automatic
 * helper code. The constraint text is in the global variable
 * constraint. */

static void
finish_constraint_and_action(void)
{
  unsigned int i;
  float cost;
  int have_constraint = (pattern[patno].autohelper_flag & HAVE_CONSTRAINT);
  int have_action = (pattern[patno].autohelper_flag & HAVE_ACTION);
  int no_labels = 1;

  /* Mark that this pattern has an autohelper. */
  pattern[patno].autohelper = dummyhelper;
  
  /* Generate autohelper function declaration. */
  code_pos += sprintf(code_pos, 
		      "static int\nautohelper%s%d(int trans, int move, int color, int action)\n{\n  int",
		      prefix, patno);

  /* Generate variable declarations. */
  for (i = 0; i < sizeof(VALID_CONSTRAINT_LABELS); i++) {
    int c = (int) VALID_CONSTRAINT_LABELS[i];

    if (label_coords[c][0] != -1)
      code_pos += sprintf(code_pos, " %c,", c);
  }

  /* Replace the last ',' with ';' */
  if (*(code_pos-1) == ',')
    *(code_pos-1) = ';';
  else {
    code_pos -= 3; /* no variable, erase "int" */
    code_pos += sprintf(code_pos, "UNUSED(trans);");
  }

  /* Include UNUSED statements for two parameters */
  code_pos += sprintf(code_pos, "\n  UNUSED(color);\n");
  if (!have_constraint || !have_action)
    code_pos += sprintf(code_pos, "  UNUSED(action);\n");
  
  /* Generate coordinate transformations. */
  for (i = 0; i < sizeof(VALID_CONSTRAINT_LABELS); i++) {
    int c = (int) VALID_CONSTRAINT_LABELS[i];

    if (label_coords[c][0] != -1) {
      code_pos += sprintf(code_pos,
			  "\n  %c = AFFINE_TRANSFORM(%d, trans, move);",
			  c,
			  OFFSET(label_coords[c][0], label_coords[c][1])
			  - OFFSET_DELTA(ci, cj)
			  - CENTER_OFFSET(pattern[patno].move_offset));
      no_labels = 0;
    }
  }

  /* move might be unused. Add an UNUSED statement to avoid warnings. */
  if (no_labels)
    code_pos += sprintf(code_pos, "\n  UNUSED(move);");

  code_pos += sprintf(code_pos, "\n\n");
  if (have_constraint && have_action)
    code_pos += sprintf(code_pos, "  if (!action)\n  ");
  if (have_constraint) {
    code_pos += sprintf(code_pos, "  return ");
    parse_constraint_or_action(constraint, &cost);
    pattern[patno].constraint_cost = cost;
    code_pos += sprintf(code_pos, ";\n");
  }
  if (have_action) {
    code_pos += sprintf(code_pos, "  ");
    parse_constraint_or_action(action, &cost);
    code_pos += sprintf(code_pos, ";\n");
    code_pos += sprintf(code_pos, "\n  return 0;\n");
  }
  code_pos += sprintf(code_pos, "}\n\n");
  
  /* Check that we have not overrun our buffer. That would be really bad. */
  assert(code_pos <= autohelper_code + sizeof(autohelper_code));
}



/* ================================================================ */
/*           stuff to write the elements of a pattern               */
/* ================================================================ */

/* callback function used to sort the elements in a pattern. 
 * We want to sort the patterns by attribute.
 *
 *  RANK 01234567
 *  ATT  57126340
 *       ,!XOaxo.
 * 
 * so that cheaper / more specific tests are done early on
 * For connections mode, the inhibition points (7)
 * must be first.
 */

static int
compare_elements(const void *a, const void *b)
{
  static char order[] = {7,2,3,5,6,0,4,1};  /* score for each attribute */
  return  order[((const struct patval_b *)a)->att]
    - order[((const struct patval_b *)b)->att];
}

#if EXPERIMENTAL_READING

/* For the tree-based algorithm, it works best to sort the elements
 * by distance from the anchor, making the search tree smaller.
 * Similarly, it's best to match the most common elements first.
 * Note: This will therefore not work for the connections database
 */

static int
compare_elements_closest(const void *a, const void *b)
{
  const struct patval_b *pa = (const struct patval_b *)a;
  const struct patval_b *pb = (const struct patval_b *)b;

  int ax = pa->x - ci;
  int bx = pb->x - ci;
  int ay = pa->y - cj;
  int by = pb->y - cj;
  int metric = (ax*ax + ay*ay) - (bx*bx + by*by) ;
  if (metric == 0)
    return -compare_elements(a,b);
  else
    return metric;
}

static void tree_push_pattern(void);

#endif

struct element_node {
  struct patval_b e;
  struct element_node *next;
};

/* flush out the pattern stored in elements[]. Don't forget
 * that elements[].{x,y} and min/max{i,j} are still relative
 * to the top-left corner of the original ascii pattern, and
 * not relative to the anchor stone ci,cj
 */

static void
write_elements(FILE *outfile)
{
  int node;

  assert(ci != -1 && cj != -1);

  /* sort the elements so that least-likely elements are tested first. */
  gg_sort(elements, el, sizeof(struct patval_b), compare_elements);

  fprintf(outfile, "static struct patval %s%d[] = {\n", prefix, patno);

  /* This may happen for fullboard patterns. */
  if (el == 0) {
    fprintf(outfile, "  {0,-1}}; /* Dummy element, not used. */\n\n");
    return;
  }
  
  for (node = 0;node < el; node++) {
    assert(elements[node].x >= mini && elements[node].y >= minj);
    if (!(elements[node].x <= maxi && elements[node].y <= maxj)) {
      fprintf(stderr, 
	      "%s(%d) : error : Maximum number of elements exceeded in %s.\n",
	      current_file, current_line_number, prefix);
      fatal_errors++;
    }

    fprintf(outfile, "  {%d,%d}%s",
	    OFFSET(elements[node].x - ci, elements[node].y - cj),
	    elements[node].att,
            node < el-1 ?  ((node + 1) % 4 ? ",\t" : ",\n")  : "};\n\n");
  }

#if EXPERIMENTAL_READING
  if (database_type == DB_TREE)
    tree_push_pattern();
#endif
}

#if EXPERIMENTAL_READING

struct tree_node graph[2];
int tree_next = 0;

/* The elements of a "simplified" pattern, which has already been
 * copied in the following ways:
 *  1) All rotations
 *  2) All ATT_x & ATT_o elements reduced to ATT_dot, ATT_X, and ATT_O
 * 
 * This simplification allows for very simple searching of the
 * pre-digested patterns.  
 *
 * For example, a pattern with 8 symmetry and one ATT_x element will
 * have 16 copies located in various places throughout the tree.
 * This function adds only one of the 16 copies.
 */
static void
tree_push_elements(struct element_node *elist,
		   struct tree_node *tree_start, int ll)
{
  struct tree_node_list *grptr;
  struct element_node *elist_next;
  struct element_node *elist_prev;

   if (!tree_start->next_list) {
     tree_start->next_list = malloc(sizeof(struct tree_node_list));
     tree_start->next_list->next = 0;
   }

    elist_prev = elist_next = elist;
    while (elist_next->next) {
      elist_prev = elist_next;
      elist_next = elist_next->next;
      for (grptr = tree_start->next_list; grptr != 0; grptr = grptr->next) {
        if (elist_next->e.x == grptr->node.x
	    && elist_next->e.y == grptr->node.y
	    && elist_next->e.att == grptr->node.att)
        {
          if (verbose) 
            fprintf(stderr, "  element matched.\n");
          elist_prev->next = elist_next->next;
          tree_push_elements(elist, &(grptr->node), ll);
          return;
        }
      }
    }

    if (elist->next) {
      /* Still elements to add to tree */
      for (grptr = tree_start->next_list;
	   grptr->next != NULL;
	   grptr = grptr->next)
	;
      
      grptr->next = malloc(sizeof(struct tree_node_list));
      grptr = grptr->next;
      grptr->node.matches = 0;
      grptr->node.att = elist->next->e.att;
      if (!(grptr->node.att == ATT_dot
	    || grptr->node.att == ATT_X
	    || grptr->node.att == ATT_O)) {
        fprintf(stderr,
		"%s(%d) : error : Internal error; unexpected att = %d\n",
                current_file, current_line_number, grptr->node.att);
        fatal_errors++;
      }
      grptr->node.x = elist->next->e.x;
      grptr->node.y = elist->next->e.y;
      grptr->node.next_list= 0;
      grptr->next = 0;
      elist->next = elist->next->next;
      if (verbose) {
        fprintf(stderr, "  Added node %c, x=%2d, y=%2d\n", 
		*(VALID_PATTERN_CHARS + grptr->node.att),
		grptr->node.x, grptr->node.y);
      }
      tree_push_elements(elist, &grptr->node, ll);
    }
    else {
      /* patten matches here! */
      struct match_node *matches;
      if (verbose)
        fprintf(stderr, "  pattern complete.\n");
      matches = tree_start->matches;
      
      if (!matches) {
        matches = malloc(sizeof(struct match_node));
        matches->next = 0;
        tree_start->matches = matches;
      }
      
      while (matches->next != NULL)
        matches = matches->next;

      matches->next = malloc(sizeof(struct match_node));
      matches = matches->next;
      matches->patnum = patno;
      matches->next = 0;
      matches->orientation = ll;
    }
}


/* By this point, the pattern is rotated.  Now, recursively
 * copy the entries as necessary, expanding o & x to ., O, & X
 */
static void
tree_push_pattern_DOX(struct element_node *elist, int ll)
{
  struct element_node *elist_next = 0;
  struct element_node *elist_prev = 0;
  int need_copy = 0;

  elist_next = elist->next;
  while (elist_next) {
    if (elist_next->e.att == ATT_o 
        || elist_next->e.att == ATT_x) {
      need_copy = 1;
      break;
    }
    elist_next = elist_next->next;
  }
    
  if (need_copy) {
    struct element_node *elist_copy1 = malloc(sizeof(struct element_node));
    struct element_node *elist_copy2 = malloc(sizeof(struct element_node));
    struct element_node *elist1_next = elist_copy1;
    struct element_node *elist2_next = elist_copy2;
    int found_copy_element = 0;
    
    elist_next = elist->next;
    while (elist_next) {
      elist1_next->next = malloc(sizeof(struct element_node));
      elist1_next = elist1_next->next;
      elist1_next->e = elist_next->e;
      elist2_next->next = malloc(sizeof(struct element_node));
      elist2_next = elist2_next->next;
      elist2_next->e = elist_next->e;
      if (!found_copy_element) {
        if (elist_next->e.att == ATT_o
            || elist_next->e.att == ATT_x) {
          found_copy_element = 1;
          elist1_next->e.att = ATT_dot;
          elist2_next->e.att = (elist_next->e.att == ATT_o ? ATT_O : ATT_X);
        }
      }
      elist1_next->next = 0;
      elist2_next->next = 0;
      elist_next = elist_next->next;
    }
    assert(found_copy_element);
    tree_push_pattern_DOX(elist_copy1, ll);
    tree_push_pattern_DOX(elist_copy2, ll);
    return;
  }

  {
    if (verbose)
      fprintf(stderr, "P[%s %d]:\n", pattern_names[patno], ll);
    elist_prev = elist_next = elist;
    while (elist_next->next) {
      int i;
      elist_prev = elist_next;
      elist_next = elist_next->next;
      for (i = 0; i <= 1; i++) {
        if (elist_next->e.x == graph[i].x
            && elist_next->e.y == graph[i].y
            && elist_next->e.att == graph[i].att)
        {
          elist_prev->next = elist_next->next;
          tree_push_elements(elist, &graph[i], ll);
          assert(!elist->next);  /* Element list should get exhausted */
          return;
        }
      }
    }
  }
  assert(0 && "Anchor not matched.");
}

/* Rotate the pattern and push it onto the tree once for each
 * appropriate rotation.
 */
static void 
tree_push_pattern_rot(int ll)
{
  struct element_node *elist = 0;
  struct element_node *elist_next = 0;
  int i;
  
  elist = malloc(sizeof(struct element_node));
  elist_next = elist;
  for (i = 0; i < el; i++) {
    elist_next->next = malloc(sizeof(struct element_node)); /*or die*/
    elist_next = elist_next->next;
    elist_next->e.att = elements[i].att;
    /* or continue if we don't need this transformation */
    TRANSFORM2(elements[i].x - ci, elements[i].y - cj, 
               &(elist_next->e.x), &(elist_next->e.y), ll);
    elist_next->next = 0;
  }
  tree_push_pattern_DOX(elist, ll);
}

/* For each pattern, this is the entry point to add the pattern
 * to the tree-based pattern matching data structure.
 *
 * The pattern will be rotated and copied possibly many times,
 * as explained in tree_push_elements().
 * 
 * Conceptually, each node of the tree corresponds to either
 * ATT_dot, ATT_X, or ATT_O, with an x and y coordinate relative
 * to the anchor.  When the pattern matcher is run, if 
 * board[POS(node.x, node.y)] == node.att, then proceed to the
 * next node.  If you find a node with a match_node pointer,
 * then a pattern has matched (with a given orientation).
 */
static void
tree_push_pattern(void)
{
  static int init = 0;
  int ll;
  int start_transformation = 0;
  int end_transformation;

  if (!init) {
    memset(graph, 0, sizeof(struct tree_node));
    graph[0].att = ATT_X;
    graph[1].att = ATT_O;
    graph[0].next_list = 0;
    graph[1].next_list = 0;
    tree_next = 2;
    init = 1;
  }

  if (pattern->trfno == 5) {
    start_transformation = 2;
    end_transformation = 6;
  }

  end_transformation = pattern[patno].trfno;

  /* sort the elements so that MOST likely elements are tested first. */
  gg_sort(elements, el, sizeof(struct patval_b), compare_elements_closest);

  if (0) {
  int i;
    for (i = 0; i < el; i++) {
      fprintf(stderr, "E[%d, %d, %c]\n",
	      elements[i].x - ci, elements[i].y -cj, 
	      *(VALID_PATTERN_CHARS + elements[i].att));
    }
    fprintf(stderr, "\n");
  }

  for (ll = start_transformation; ll < end_transformation; ++ll)
    tree_push_pattern_rot(ll);
}


/* ================================================================ */
/*         stuff to write out the stored pattern structures         */
/* ================================================================ */

static int mn_count;
static int tnl_count;

struct tree_node_list *tnl_dump;
struct match_node *matches_dump;

#define PASS_COUNT 1
#define PASS_FILL 2


/* Note: Currently the tree_node_list & match_node lists contain
 * dummy headers in their lists, which isn't at all necessary when
 * being used by GNU Go, but are very handy when building them up 
 * in mkpat.  Probably can strip these out when writing out the
 * final data structure.
 *
 * Does the hard work to outputs the tree data structure data as C code 
 * for GNU Go.
 */
static void
dump_tree_node(FILE *outfile, struct tree_node *gn, int depth, int pass)
{
  static int as_text = 0;
  struct tree_node_list *gl;
  tnl_count++;

  if (as_text) {
    if (depth > 0)
      fprintf(stderr, "%.*s", depth*2, "                                             ");
    fprintf(stderr, "GN[att=%c, x=%d, y=%d",
	    *(VALID_PATTERN_CHARS + gn->att), gn->x, gn->y);
  }
  
  if (pass == PASS_FILL) {
    tnl_dump[tnl_count].node.att = gn->att;
    tnl_dump[tnl_count].node.x = gn->x;
    tnl_dump[tnl_count].node.y = gn->y;
  }

  if (gn->matches) {
    struct match_node *m = gn->matches->next;
    if (pass == PASS_FILL) {
      tnl_dump[tnl_count].node.matches = (void*)mn_count;
      matches_dump[mn_count].patnum = -1; /*Unused*/
      matches_dump[mn_count].orientation = 0; /*Unused*/
      matches_dump[mn_count].next = (void*)(mn_count + 1);
    }
    
    if (as_text)
      fprintf(stderr, ", matches[%d]: ", mn_count);

    mn_count++;
    while (m) {
      if (pass == PASS_FILL) {
        matches_dump[mn_count].patnum = m->patnum;
        matches_dump[mn_count].orientation = m->orientation;
        if (m->next)
          matches_dump[mn_count].next = (void*)(mn_count + 1);
	else
          matches_dump[mn_count].next = 0;
      }
      
      if (as_text)
        fprintf(stderr, "P[%s, %d]", pattern_names[m->patnum],
		m->orientation);

      mn_count++;
      m = m->next;
    }
  }
  else {
    if (pass == PASS_FILL)
      tnl_dump[tnl_count].node.matches = 0;
  }

  if (as_text)
    fprintf(stderr, "]\n");

  gl = gn->next_list;
  if (gl) {
    int prev_tnl_count = tnl_count;
    if (pass == PASS_FILL) {
      tnl_dump[tnl_count].node.next_list = (void*)(tnl_count+1);
      /*tnl_dump[tnl_count+1].node data is unused.*/
      tnl_dump[tnl_count+1].node.matches = 0;   /*Unused*/
      tnl_dump[tnl_count+1].node.att = -1;      /*Unused*/
      tnl_dump[tnl_count+1].node.x = -99;       /*Unused*/
      tnl_dump[tnl_count+1].node.y = -99;       /*Unused*/
      tnl_dump[tnl_count+1].node.next_list = 0; /*Unused*/

    }
    prev_tnl_count = tnl_count;
    tnl_count++;
    while (gl->next) {
      if (pass == PASS_FILL)
        tnl_dump[prev_tnl_count+1].next = (void*)(tnl_count+1);

      prev_tnl_count = tnl_count;
      dump_tree_node(outfile, &gl->next->node, depth+1, pass);
      gl = gl->next;
    }
    if (pass == PASS_FILL)
      tnl_dump[prev_tnl_count+1].next = 0;
  }
  else {
    if (pass == PASS_FILL) {
      assert(0 && "Strange bug here");
      /* This may be where we crash if we're missing an anchor color
       * in the database */
      if (0)
	fprintf(outfile, "  tnl[%d].node.next_list = 0;\n", tnl_count);
    }
  }


}
  
/*
 * Outputs the tree data structure data as C code for GNU Go.
 */
static void
tree_write_patterns(FILE *outfile)
{
  int oanchor_index = 0;
  int i;
  fprintf(outfile, "#include \"patterns.h\"\n\n");

  mn_count = 1;
  tnl_count = 0;
  dump_tree_node(outfile, &graph[0], 0, PASS_COUNT);

  oanchor_index = tnl_count+1;
  dump_tree_node(outfile, &graph[1], 0, PASS_COUNT);

  tnl_dump = malloc(sizeof(struct tree_node_list) * tnl_count);
  matches_dump = malloc(sizeof(struct match_node) * mn_count);

  tnl_dump[0].next = (void*)1;  /* X anchor node */
  tnl_dump[0].node.att = -1;    /* Not used */
  tnl_dump[0].node.matches = 0; /* Not used */
  tnl_dump[0].node.x = -99;     /* Not used */
  tnl_dump[0].node.y = -99;     /* Not used */
  tnl_dump[0].node.next_list = 0; /* Not used */
  tnl_dump[1].next = (void*) oanchor_index;
  tnl_dump[oanchor_index].next = 0;
  
  mn_count = 1;
  tnl_count = 0;
  dump_tree_node(outfile, &graph[0], 0, PASS_FILL);
  dump_tree_node(outfile, &graph[1], 0, PASS_FILL);

  fprintf(outfile, "struct tree_node_list tnl_%s[] =\n{\n", prefix);
  for (i = 0; i < tnl_count+1; i++) {
    fprintf(outfile,
	    "  { {(void*)%d, %d, %d, %d, (void*)%d}, (void*)%d}, /*#%d*/\n",
	    (int)tnl_dump[i].node.matches,
	    tnl_dump[i].node.att,
	    tnl_dump[i].node.x,
	    tnl_dump[i].node.y,
	    (int)tnl_dump[i].node.next_list,
	    (int)tnl_dump[i].next,
	    i);
  }
  fprintf(outfile, "};\n\n");

  fprintf(outfile, "struct match_node matches_%s[] = \n{\n", prefix);
  for (i = 0; i < mn_count; i++) {
    fprintf(outfile, "  {%d, %d, (void*)%d}, /*#%d*/\n",
	    matches_dump[i].patnum,
	    matches_dump[i].orientation,
	    (int) matches_dump[i].next,
	    i);
  }
  fprintf(outfile, "};\n\n");

  fprintf(outfile, "void\ninit_tree_%s(void)\n{\n", prefix);
  fprintf(outfile, "  gg_assert(sizeof(tnl_%s) / sizeof(struct tree_node_list) == %d);\n", 
	  prefix, tnl_count+1);
  fprintf(outfile, "  gg_assert(sizeof(matches_%s) / sizeof(struct match_node) == %d);\n",
	  prefix, mn_count);
  fprintf(outfile,
	  "  tree_initialize_pointers(tnl_%s, matches_%s, %d, %d);\n",
	  prefix, prefix, tnl_count+1, mn_count+1);
  fprintf(outfile, "}\n\n");
}

#endif

/* sort and write out the patterns */
static void
write_patterns(FILE *outfile)
{
  int j;

#if EXPERIMENTAL_READING
  if (database_type == DB_TREE)
    tree_write_patterns(outfile, prefix);
  else
    fprintf(outfile, "\nvoid\ninit_tree_%s(void)\n{\n"
	    "  /* nothing to do - tree option not compiled */\n"
	    "}\n\n", name);
#endif

  /* Write out the patterns. */
  if (database_type == DB_FULLBOARD)
    fprintf(outfile, "struct fullboard_pattern %s[] = {\n", prefix);
  else
    fprintf(outfile, "struct pattern %s[] = {\n", prefix);

  for (j = 0; j < patno; ++j) {
    struct pattern *p = pattern + j;

    if (database_type == DB_FULLBOARD) {
      fprintf(outfile, "  {%s%d,%d,\"%s\",%d,%f},\n", prefix, j, p->patlen,
	      pattern_names[j], p->move_offset, p->value);
      continue;
    }
    
    /* p->min{i,j} and p->max{i,j} are the maximum extent of the elements,
     * including any rows of '?' which do not feature in the elements list.
     * ie they are the positions of the topleft and bottomright corners of
     * the pattern, relative to the pattern origin. These just transform same
     * as the elements.
     */
    
    fprintf(outfile, "  {%s%d,%d,%d, \"%s\",%d,%d,%d,%d,%d,%d,0x%x,%d",
	    prefix, j,
	    p->patlen,
	    p->trfno,
	    pattern_names[j],
	    p->mini, p->minj,
	    p->maxi, p->maxj,
	    p->maxi - p->mini,   /* height */
	    p->maxj - p->minj,   /* width  */
	    p->edge_constraints,
	    p->move_offset);


#if GRID_OPT
    fprintf(outfile, ",\n    {");
    {
      int ll;

      for (ll = 0; ll < 8; ++ll)
	fprintf(outfile, " 0x%08x%s", p->and_mask[ll], ll<7 ? "," : "");
      fprintf(outfile, "},\n    {");
      for (ll = 0; ll < 8; ++ll)
	fprintf(outfile, " 0x%08x%s", p->val_mask[ll], ll<7 ? "," : "");
    }
    fprintf(outfile, "}\n   ");
#endif

    fprintf(outfile, ", 0x%x,%f,%f,%f,%f,%f,%f,%f,%d,%s,",
	    p->class,
	    p->value,
	    p->maxvalue,
	    p->minterritory,
	    p->maxterritory,
	    p->shape,
	    p->followup,
	    p->reverse_followup,
	    p->autohelper_flag,
	    helper_fn_names[j]);
    if (p->autohelper)
      fprintf(outfile, "autohelper%s%d", prefix, j);
    else
      fprintf(outfile, "NULL");
    fprintf(outfile, ",%d", p->anchored_at_X);
    fprintf(outfile, ",%f", p->constraint_cost);
#if PROFILE_PATTERNS
    fprintf(outfile, ",0,0");
    fprintf(outfile, ",0");
#endif

    fprintf(outfile, "},\n");
  }

  if (database_type == DB_FULLBOARD) {
    fprintf(outfile, "  {NULL,0,NULL,0,0.0}\n};\n");
    return;
  }
  
  /* Add a final empty entry. */
  fprintf(outfile, "  {NULL, 0,0,NULL,0,0,0,0,0,0,0,0");
#if GRID_OPT
  fprintf(outfile, ",{0,0,0,0,0,0,0,0},{0,0,0,0,0,0,0,0}");
#endif
  fprintf(outfile, ",0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0,NULL,NULL,0,0.0");
#if PROFILE_PATTERNS
  fprintf(outfile, ",0,0");
  fprintf(outfile, ",0");
#endif
  fprintf(outfile, "}\n};\n");
}

/* Write out the pattern db */
static void
write_pattern_db(FILE *outfile)
{
  /* Don't want this for fullboard patterns. */
  if (database_type == DB_FULLBOARD)
    return;
  
  /* Write out the pattern database. */
  fprintf(outfile, "\n");
  fprintf(outfile, "struct pattern_db %s_db = {\n", prefix);
  fprintf(outfile, "  -1,\n");  /* fixed_for_size */
  fprintf(outfile, "  %d,\n", fixed_anchor);
  fprintf(outfile, "  %s\n", prefix);
  if (database_type == DB_DFA)
    fprintf(outfile, " ,& dfa_%s\n", prefix); /* pointer to the wired dfa */
  else
    fprintf(outfile, " , NULL\n"); /* pointer to a possible dfa */
#if EXPERIMENTAL_READING
  if (database_type == DB_TREE)
    fprintf(outfile, " , tnl_%s", prefix);
  else
    fprintf(outfile, " , NULL\n");
#endif

  fprintf(outfile, "};\n");
}


int
main(int argc, char *argv[])
{
  static char stdin_name[] = "<stdin>";
  int input_files = 0;
  int ifc;
  char *input_file_names[MAX_INPUT_FILE_NAMES];
  char *output_file_name = NULL;
  FILE *input_FILE = stdin;
  FILE *output_FILE = stdout;
  int state = 0;

  transformation_init();

  {
    int i;
    int multiple_anchor_options = 0;

    /* Parse command-line options */
    while ((i = gg_getopt(argc, argv, "i:o:vV:pcfCDTOXbmar")) != EOF) {
      switch (i) {
      case 'i': 
	if (input_files == MAX_INPUT_FILE_NAMES) {
	  fprintf(stderr, "Error : Too many input files (maximum %d supported)\n",
	    MAX_INPUT_FILE_NAMES);
	  return 1;
        }
	input_file_names[input_files++] = gg_optarg;
	break;

      case 'o': output_file_name = gg_optarg; break;
      case 'v': verbose = 1; break;
      case 'V': dfa_verbose = strtol(gg_optarg, NULL, 10); break;

      case 'p':
      case 'c':
      case 'f':
      case 'C':
      case 'D':
      case 'T':
	if (database_type) {
	  fprintf(stderr, "Error : More than one database type specified (-%c and -%c)\n",
		  database_type, i);
	  return 1;
	}
	database_type = i;
#if EXPERIMENTAL_READING == 0
	if (i == 'T') {
	  fprintf(stderr, "Error : Tree based matcher is unsupported in this configuration\n");
	  fprintf(stderr, "        Try `configure --enable-experimental-reading'\n");
	  return 1;
	}
#endif
	break;

      case 'O':
	if (anchor)
	  multiple_anchor_options = 1;
	anchor = ANCHOR_O;
	break;
      case 'X':
	if (anchor)
	  multiple_anchor_options = 1;
	anchor = ANCHOR_X;
	break;
      case 'b':
	if (anchor)
	  multiple_anchor_options = 1;
	anchor = ANCHOR_BOTH;
	break;

      case 'm': 
        choose_best_anchor = 1;
        if (fixed_anchor)
          fprintf(stderr, "Warning : -m and -a are mutually exclusive.\n");
	break;
      case 'a': 
        fixed_anchor = 1; 
        if (choose_best_anchor)
          fprintf(stderr, "Warning : -m and -a are mutually exclusive.\n");
        break;

      case 'r': pre_rotate = 1; break;
      
      default:
	fprintf(stderr, "\b ; ignored\n");
      }
    }

    if (!database_type)
      database_type = DB_GENERAL;
    if (!anchor)
      anchor = ANCHOR_O;

    if (!input_files)
      input_file_names[input_files++] = stdin_name;
    if (output_file_name) {
      output_FILE = fopen(output_file_name, "wb");
      if (output_FILE == NULL) {
	fprintf(stderr, "Error : Cannot write to file %s\n", output_file_name);
	return 1;
      }
    }

    if (multiple_anchor_options)
      fprintf(stderr, "Warning : Multiple anchor options encountered. The last took precedence\n");
  }

  if (gg_optind >= argc) {
    fputs(USAGE, stderr);
    exit(EXIT_FAILURE);
  }

  prefix = argv[gg_optind];

  if (database_type == DB_DFA) {
    dfa_init();
    new_dfa(&dfa, "mkpat's dfa");
    dfa.pre_rotated = pre_rotate;
  }

  fprintf(output_FILE, PREAMBLE);

  /* Parse the input file, output pattern elements as as we find them,
   * and store pattern entries for later output.
   */

  /* Initialize pattern number and buffer for automatically generated
   * helper code.
   */
  patno = -1;
  autohelper_code[0] = 0;
  code_pos = autohelper_code;
  
  /* We do this parsing too with the help of a state machine.
   * state
   *   0     Waiting for a Pattern line.
   *   1     Pattern line found, waiting for a position diagram.
   *   2     Reading position diagram.
   *   3     Waiting for entry line (":" line).
   *   4     Waiting for optional constraint diagram.
   *   5     Reading constraint diagram.
   *   6     Waiting for constraint line (";" line).
   *   7     Reading a constraint
   *   8     Reading an action
   *
   * FIXME: This state machine should be possible to simplify.
   *
   */

  for (ifc = 0; ifc < input_files && !fatal_errors; ifc++) {
    char line[MAXLINE];  /* current line from file */

    if (input_file_names[ifc] != stdin_name) {
      input_FILE = fopen(input_file_names[ifc], "r");
      if (input_FILE == NULL) {
	fprintf(stderr, "Error: Cannot open file %s\n", input_file_names[ifc]);
	return 1;
      }
    }
 
    current_file = input_file_names[ifc];
    current_line_number = 0;

    while (fgets(line, MAXLINE, input_FILE)) {
      current_line_number++;
      if (line[strlen(line)-1] != '\n') {
	fprintf(stderr, "%s(%d) : Error : line truncated (longer than %d characters)\n",
		current_file, current_line_number, MAXLINE - 1);

	fatal_errors++;
      }
      
      /* Remove trailing white space from `line' */
      {
	int i = strlen(line) - 2;  /* Start removing backwards just before newline */
	while (i >= 0 && isspace(line[i]))
	  i--;
	line[i+1]   = '\n';
	line[i+2] = '\0';
      }
      
      /* FIXME: We risk overruning a buffer here. */
      if (sscanf(line, "Pattern %s", pattern_names[patno+1]) == 1) {
	char *p = strpbrk(pattern_names[patno+1], " \t");
	
	if (p)
	  *p = 0;
	if (patno >= 0) {
	  switch (state) {
	  case 1:
	    fprintf(stderr, "%s(%d) : Warning: empty pattern %s\n",
		    current_file, current_line_number, pattern_names[patno]);
	    break;
	  case 2:
	  case 3:
	    fprintf(stderr, "%s(%d) : Error : No entry line for pattern %s\n",
		    current_file, current_line_number, pattern_names[patno]);
	    fatal_errors++;
	    break;
	  case 5:
	  case 6:
	    fprintf(stderr,
		    "%s(%d) : Warning: constraint diagram but no constraint line for pattern %s\n",
		    current_file, current_line_number, pattern_names[patno]);
	    break;
	  case 7:
	  case 8:
	    finish_constraint_and_action();
							/* fall through */
	  case 0:
	  case 4:
	    check_constraint_diagram();
	    patno++;
	    reset_pattern();
	  }
	}
	else {
	  patno++;
	  reset_pattern();
	}
	state = 1;
      }
      else if (line[0] == '\n' || line[0] == '#') { 
	/* nothing */
	if (state == 2 || state == 5) {
	  if (state == 5)
	    check_constraint_diagram_size();
	  state++;
	}
      }
      else if (strchr(VALID_PATTERN_CHARS, line[0])
	       || strchr(VALID_EDGE_CHARS, line[0])
	       || strchr(VALID_CONSTRAINT_LABELS, line[0])) { 
	/* diagram line */
	switch (state) {
	case 0:
	case 3:
	case 6:
	case 7:
	case 8:
	  fprintf(stderr, 
		  "%s(%d) : error : What, another diagram here? (pattern %s)\n",
		  current_file, current_line_number, pattern_names[patno]);
	  fatal_errors++;
	  break;
	case 1:
	  state++; /* fall through */
	case 2:
	  read_pattern_line(line);
	  current_i++;
	  break;
	case 4:
	  state++; /* fall through */
	case 5:
	  read_constraint_diagram_line(line);
	  break;
	}	
      }
      else if (line[0] == ':') {
	if (state == 2 || state == 3) {
	  finish_pattern(line);
	  
	  write_elements(output_FILE);
	  if (database_type == DB_DFA)
	    write_to_dfa(patno);
	  state = 4;
	}
	else {
	  fprintf(stderr,
		  "%s(%d) : warning : Unexpected entry line in pattern %s\n",
		  current_file, current_line_number, pattern_names[patno]);
	}
      } 
      else if (line[0] == ';') {
	if (state == 5)
	  check_constraint_diagram_size();
	
	if (state == 5 || state == 6 || state == 7) {
	  read_constraint_line(line+1);
	  state = 7;
	}
	else {
	  fprintf(stderr, "%s(%d) : Warning: unexpected constraint line in pattern %s\n",
		  current_file, current_line_number, pattern_names[patno]);
	}
      } 
      else if (line[0] == '>') {
	if (state == 4 || state == 5 || state == 6 
	    || state == 7 || state == 8) {
	  if (state == 5)
	    check_constraint_diagram_size();
	  read_action_line(line+1);
	  state = 8;
	}
	else {
	  fprintf(stderr, "Warning: unexpected action line in pattern %s\n",
		  pattern_names[patno]);
	}
      } 
      else {
	int i = strlen(line);
	char c = line[i-1];
	line[i-1] = 0;  /* Chop off \n */
	fprintf(stderr, "%s(%d) : error : Malformed line \"%s\" in pattern %s\n",
		current_file, current_line_number, line, pattern_names[patno]);
	line[i-1] = c;  /* Put it back - maybe not necessary at this point. */
	fatal_errors++;
      }
    } /* while not EOF */
  } /* for each file */
  
  if (patno >= 0) {
    switch (state) {
    case 1:
      fprintf(stderr, "Warning: empty pattern %s\n",
	      pattern_names[patno]);
      break;
    case 2:
    case 3:
      fprintf(stderr, "%s(%d) : Error : no entry line for pattern %s\n",
	      current_file, current_line_number, pattern_names[patno]);
      fatal_errors++;
      break;
    case 5:
    case 6:
      fprintf(stderr,
	      "Warning: constraint diagram but no constraint line for pattern %s\n",
	      pattern_names[patno]);
      break;
    case 7:
    case 8:
      finish_constraint_and_action(); /* fall through */
    case 0:
    case 4:
      check_constraint_diagram();
      patno++;
      reset_pattern();
    }
  } 
 
  if (verbose)
    fprintf(stderr, "%d / %d patterns have edge-constraints\n",
	    pats_with_constraints, patno);

  /* Forward declaration, which autohelpers might need. */
  if (database_type != DB_FULLBOARD && database_type != DB_CORNER)
    fprintf(output_FILE, "struct pattern %s[];\n\n", prefix);

  /* Write the autohelper code. */
  fprintf(output_FILE, "%s", autohelper_code);

  write_patterns(output_FILE);

  if (database_type == DB_DFA) {
    fprintf(stderr, "---------------------------\n");

    dfa_finalize(&dfa);
    dfa_shuffle(&dfa);

    fprintf(stderr, "dfa for %s\n", prefix);
    fprintf(stderr, "size: %d kB for ", dfa_size(&dfa));
    fprintf(stderr, "%d patterns", patno);
    fprintf(stderr, "(%d states)\n", dfa.lastState);

    if (0 && dfa.pre_rotated)
      dump_dfa(stderr, &dfa);

    strcpy(dfa.name, prefix);
    print_c_dfa(output_FILE, prefix, &dfa);
    fprintf(stderr, "---------------------------\n");

    if (DFA_MAX_MATCHED/8 < patno)
      fprintf(stderr, "Warning: Increase DFA_MAX_MATCHED in 'dfa.h'.\n");

    kill_dfa(&dfa);
    dfa_end();
  }


  write_pattern_db(output_FILE);

  if (fatal_errors) {
    fprintf(output_FILE, "\n#error: One or more fatal errors compiling %s\n",
	    current_file);
  }

  return fatal_errors ? 1 : 0;
}


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */

