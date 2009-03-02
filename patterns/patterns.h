/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\\
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

/* This file describes the compiled form of the pattern database.
 * mkpat is used to compile various source files <name>.db into
 * intermediate files <name>.c which define data structures
 * describing the patterns.
 */

#ifndef _PATTERN_H_
#define _PATTERN_H_

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

/* local versions of absolute value, min and max */

#define gg_abs(x) ((x) < 0 ? -(x) : (x))
#define gg_min(a, b) ((a)<(b) ? (a) : (b))
#define gg_max(a, b) ((a)<(b) ? (b) : (a))

/* This tells Alpha OSF/1 not to define a getopt prototype in <stdio.h>.
 * Ditto for AIX 3.2 and <stdlib.h>. 
 */
#ifndef _NO_PROTO
#define _NO_PROTO
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#else
#define GRID_OPT 0
#endif

#ifndef GRID_OPT
#error GRID_OPT should be defined as 0, 1 or 2
#endif


/* Include support for pattern profiling. May be turned off in stable
 * releases to save some memory.
 *
 * FIXME: should probably be included in config.h
 */
#define PROFILE_PATTERNS 0

/* this trick forces a compile error if ints are not at least 32-bit */
struct _unused_patterns_h {
  int unused[sizeof(unsigned int) >= 4 ? 1 : -1];
};


#define ATTACK_MACRO(pos) ((stackp == 0) ? (worm[pos].attack_codes[0]) : attack(pos, NULL))
#define DEFEND_MACRO(pos) ((stackp == 0) ? (worm[pos].defense_codes[0]) : find_defense(pos, NULL))

struct pattern; /* forward reference to keep gcc happy */

/* this is the type of a function which the matcher can
 * call to evaluate the score of a move.
 * parameters:
 *   pattern and rotation are the current pattern being considered
 *   ti, tj: IN = posn of the 7, 8 or 9 marker
 *           OUT = recommended move
 * return value : weight of move, or 0 if match failed            
 */
 
typedef int (*pattern_helper_fn_ptr)(struct pattern *, int rotation,
				     int move, int color);

typedef int (*autohelper_fn_ptr)(int rotation, int move,
				 int color, int action);


/* each pattern is compiled into a sequence of these elements.
 * Each describes a relative x, y from the pattern origin,
 * and a description of what should be there.
 * Current attributes are
 *  0 = .
 *  1 = X
 *  2 = O
 *  3 = x
 *  4 = o
 *  5 = ,  (barriers only)
 *  6 = a  (half-eye only, OBSOLETE)
 *  7 = !  (connection and barriers only)
 */

#define ATT_dot   0
#define ATT_X     1
#define ATT_O     2
#define ATT_x     3
#define ATT_o     4
#define ATT_comma 5
#define ATT_a     6
#define ATT_not   7

/* Pattern classes. The semantics of these varies between different
 * databases. The descriptions here mostly relate to patterns in
 * patterns.db and other databases which are handled by shapes.c.
 */
#define CLASS_O     0x0001   /* O stones must be alive or unknown */
#define CLASS_o     0x0002   /* O stones must be dead or unknown */
#define CLASS_X     0x0004   /* X stones must be alive or unknown */
#define CLASS_x     0x0008   /* X stones must be dead or unknown */
#define CLASS_s     0x0010   /* move is a sacrifice */
#define CLASS_n     0x0020   /* X could also make this move if we do not */
#define CLASS_D     0x0040   /* defense pattern */
#define CLASS_C     0x0080   /* move connects two worms */
#define CLASS_c     0x0100   /* move weakly connects two worms */ 
			     /* for owl databases: combinable pattern */
#define CLASS_B     0x0200   /* move breaks connection between enemy worms */
#define CLASS_A     0x0400   /* attack pattern */
#define CLASS_b     0x0800   /* move is intended to block opponent */
#define CLASS_e     0x1000   /* move is intended to expand territory */
#define CLASS_E     0x2000   /* move is intended to expand moyo */
#define CLASS_a     0x4000   /* strategical level attack */
#define CLASS_d     0x8000   /* strategical level defense */
#define CLASS_I 0x00010000   /* invasions patterns (influence.db) */
#define CLASS_J 0x00020000   /* joseki standard move */
#define CLASS_j 0x00040000   /* joseki move, slightly less urgent */
#define CLASS_t 0x00080000   /* minor joseki move (tenuki OK) */
#define CLASS_U 0x00100000   /* very urgent joseki move */
#define CLASS_T 0x00200000   /* joseki trick move */
#define CLASS_W 0x00400000   /* worthwhile threat move */
#define CLASS_F 0x00800000   /* for joseki moves: a fuseki pattern */
#define CLASS_N 0x01000000   /* antisuji move (do _not_ play) */
#define CLASS_Y 0x80000000   /* used for experimental patterns */

/* Collection of the classes inducing move reasons. */
#define CLASS_MOVE_REASONS (CLASS_C | CLASS_B | CLASS_b | \
                            CLASS_e | CLASS_E | CLASS_I | CLASS_a | CLASS_d | \
			    CLASS_J | CLASS_j | CLASS_U | CLASS_T | CLASS_t | \
                            CLASS_W | CLASS_c | CLASS_F)

/* directions for applying edge-constraints */
#define NORTH_EDGE 1
#define SOUTH_EDGE 2
#define EAST_EDGE  4
#define WEST_EDGE  8

/* different kinds of autohelpers */
#define HAVE_CONSTRAINT 1
#define HAVE_ACTION     2

/* Values of the action parameter to indicate where an influence autohelper
 * is called from.
 */
#define INFLUENCE_CALLBACK 1
#define FOLLOWUP_INFLUENCE_CALLBACK 2


typedef struct patval {
  short offset;
  unsigned char att;
} Patval;

/* Build-time version of patval structure. */
typedef struct patval_b {
  int x;
  int y;
  int att;
} Patval_b;


enum attribute_type {
  MIN_VALUE,
  MAX_VALUE,
  MIN_TERRITORY,
  MAX_TERRITORY,
  SHAPE,
  FOLLOWUP,
  REVERSE_FOLLOWUP,

  /* For `mkpat'. */
  FIRST_OFFSET_ATTRIBUTE,

  THREATENS_TO_CAPTURE = FIRST_OFFSET_ATTRIBUTE,
  THREATENS_EYE,
  REVERSE_SENTE,

  NUM_ATTRIBUTES,
  LAST_ATTRIBUTE = NUM_ATTRIBUTES
};


#ifdef HAVE_TRANSPARENT_UNIONS

struct pattern_attribute {
  enum attribute_type type;

  /* GCC allows unnamed (and transparent) unions. */
  union {
    float value;
    int offset;
  };
};

#else

struct pattern_attribute {
  enum attribute_type type;
  float value;
  int offset;
};

#endif


/*
 * Each pattern as a whole is compiled to an instance of this structure.
 */
struct pattern {
  struct patval *patn;  /* array of elements */
  int patlen;           /* number of elements */
  int trfno;            /* number of transformations (rotations and reflections) */
  const char *name;     /* short description of pattern (optional) */

  int mini, minj;       /* min and max (relative to anchor) extent of ... */
  int maxi, maxj;       /* ...the pattern */
  int height, width;    /* differences between max and min extents */
  unsigned int edge_constraints; /* and combinations of NORTH, EAST etc.
				  * for edges */

  int move_offset;      /* offset of the suggested move (relative to anchor) */

#if GRID_OPT
  unsigned int and_mask[8]; /* for each rotation, masks for a */
  unsigned int val_mask[8]; /* 4x4 grid around anchor */
#endif

  unsigned int class;   /* classification of pattern */

  /* Value (owl-style, used for pattern sorting) is not stored as an
   * attribute, because it is very common.
   */
  float value;

  /* Pattern attributes like shape, followup etc. */
  struct pattern_attribute *attributes;

  int autohelper_flag;  /* whether autohelper has constraint and/or action */
  pattern_helper_fn_ptr helper;  /* helper function, or NULL */
  autohelper_fn_ptr autohelper;  /* automatically generated helper */
                                 /* function, or NULL */

  int anchored_at_X;    /* 3 if the pattern has 'X' at the anchor posn */

  float constraint_cost; /* mkpat's estimate of the constraint complexity.*/

#if PROFILE_PATTERNS
  int hits;
  int dfa_hits;
  int reading_nodes;
#endif
};


struct pattern_db {
  int fixed_for_size;
  const int fixed_anchor;
  struct pattern *patterns;
  struct dfa_rt *pdfa;
};


struct fullboard_pattern {
  Hash_data fullboard_hash;	/* Hash of the full board position. */	
  int number_of_stones;		/* Number of stones on board. */
  const char *name;		/* Pattern identifier. */
  int move_offset;      	/* position of the move relative to tengen */
  int value;			/* value for pattern, if matched */
};


/* Monte Carlo local patterns. */
struct mc_pattern_database {
  const char *name;
  const unsigned int *values;
};


/* helper functions */

#define DECLARE(x) int x(struct pattern *pattern, int transformation, int move, int color)

DECLARE(jump_out_helper);
DECLARE(jump_out_far_helper);
DECLARE(high_handicap_helper);
DECLARE(reinforce_helper);
DECLARE(throw_in_atari_helper);
DECLARE(cutstone2_helper);
DECLARE(thrash_around_helper);

/* autohelper fns */
int seki_helper(int str);
void threaten_to_save_helper(int move, int str);
void threaten_to_capture_helper(int move, int str);
void prevent_attack_threat_helper(int move, int str);
void defend_against_atari_helper(int move, int str);
void amalgamate_most_valuable_helper(int apos, int bpos, int cpos);
int finish_ko_helper(int apos);
int squeeze_ko_helper(int apos);
int backfill_helper(int apos, int bpos, int cpos);
int owl_threatens_attack(int apos, int bpos);
int connect_and_cut_helper(int Apos, int bpos, int cpos);
int connect_and_cut_helper2(int Apos, int bpos, int cpos, int color);
int edge_double_sente_helper(int move, int apos, int bpos, int cpos);
void test_attack_either_move(int move, int color, int worma, int wormb);
int adjacent_to_stone_in_atari(int str);
int adjacent_to_defendable_stone_in_atari(int str);
void backfill_replace(int move, int str);
int break_mirror_helper(int str, int color);
int distrust_tactics_helper(int str);
int disconnect_helper(int apos, int bpos);


/* pattern arrays themselves */
extern struct pattern_db pat_db;
extern struct pattern_db aa_attackpat_db;
extern struct pattern_db owl_attackpat_db;
extern struct pattern_db owl_vital_apat_db;
extern struct pattern_db owl_defendpat_db;
extern struct pattern_db conn_db;
extern struct pattern_db attpat_db;
extern struct pattern_db defpat_db;
extern struct pattern_db endpat_db;
extern struct pattern_db influencepat_db;
extern struct pattern_db barrierspat_db;
extern struct pattern_db fusekipat_db;
extern struct pattern_db handipat_db;
extern struct pattern_db oracle_db;

extern struct corner_db joseki_db;

extern struct fullboard_pattern fuseki19[];
extern struct fullboard_pattern fuseki13[];
extern struct fullboard_pattern fuseki9[];

extern struct mc_pattern_database mc_pattern_databases[];

struct corner_db;
struct corner_variation;
struct corner_pattern;

struct corner_db {
  int max_width;	/* Largest possible width and... */
  int max_height;	/* ... largest possible height of database patterns. */

  unsigned char num_top_variations; /* Number of top level variations. */
  struct corner_variation *top_variations;
};

struct corner_variation {
  int move_offset;	    /* Offset of the move in this variation. */
  signed char xor_att;      /* 0 - the same color as the first matched stone,
			     * 3 - the opposite color.
			     */
  unsigned char num_stones; /* Number of stones in the `move_offset' rectangle. */

  unsigned char num_variations; /* Number of subvariations. */
  struct corner_variation *variations; /* Pointer to subvariation array. */

  struct corner_pattern *pattern; /* Address of matched pattern (if any). */
};

struct corner_pattern {
  int second_corner_offset; /* Offset of pattern's second corner. */
  int symmetric;	/* If the pattern is symmetric ('/' symmetry). */

  unsigned int class;	/* Pattern class. */
  const char *name;	/* Pattern name (optional). */

  /* Pattern attributes like shape (the only one used currently). */
  struct pattern_attribute *attributes;

  int autohelper_flag;	/* Whether autohelper has constraint and/or action. */
  autohelper_fn_ptr autohelper; /* Automatically generated helper (or NULL). */
};

/* Build time version of corner_variation structure. */
struct corner_variation_b {
  int move_offset;
  signed char xor_att;
  unsigned char num_stones;

  unsigned char num_variations;
  struct corner_variation_b *next;
  struct corner_variation_b *child;
  int child_num;

  int pattern_num;
};


#endif /* _PATTERN_H_ */


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
