/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * This is GNU GO, a Go program. Contact gnugo@gnu.org, or see   *
 * http://www.gnu.org/software/gnugo/ for more information.      *
 *                                                               *
 * Copyright 1999, 2000, 2001 by the Free Software Foundation.   *
 *                                                               *
 * This program is free software; you can redistribute it and/or *
 * modify it under the terms of the GNU General Public License   *
 * as published by the Free Software Foundation - version 2.     *
 *                                                               *
 * This program is distributed in the hope that it will be       *
 * useful, but WITHOUT ANY WARRANTY; without even the implied    *
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR       *
 * PURPOSE.  See the GNU General Public License in file COPYING  *
 * for more details.                                             *
 *                                                               *
 * You should have received a copy of the GNU General Public     *
 * License along with this program; if not, write to the Free    *
 * Software Foundation, Inc., 59 Temple Place - Suite 330,       *
 * Boston, MA 02111, USA.                                        *
\* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */


#ifndef _LIBERTY_H_
#define _LIBERTY_H_

#include "gnugo.h"

/* local versions of absolute value, min and max */

#define gg_abs(x) ((x) < 0 ? -(x) : (x))
#define gg_min(a,b) ((a)<(b) ? (a) : (b))
#define gg_max(a,b) ((a)<(b) ? (b) : (a))

/* not sure if this is the best way of doing this, but... */
#define UNUSED(x)  x=x


/* ================================================================ */
/*                           public variables                       */
/* ================================================================ */


/* A few variables below are of types defined in hash.h. */
#include "hash.h"

/* Other modules get read-only access to this variable. */
extern Hash_data    hashdata;



/* ================================================================ */


#define FALSE_EYE          1
#define HALF_EYE           2
#define INHIBIT_CONNECTION 4


/* A string with n stones can have at most 2(n+1) liberties. From this
 * follows that an upper bound on the number of liberties of a string
 * on a board of size N^2 is 2/3 (N^2+1).
 */
#define MAXLIBS   (2*(MAX_BOARD*MAX_BOARD + 1)/3)
/* This is a smaller, practical number of liberties that we care to keep track of. */
#define MAX_LIBERTIES 20


/* This is an upper bound of the number of strings that can exist on
 * the board simultaneously.
 */
#define MAX_STRINGS (2 * MAX_BOARD * MAX_BOARD / 3)

#define MAXSTACK  MAX_BOARD * MAX_BOARD
#define MAXCHAIN  160

/* 1D board macros. */
/* Note that POS(-1, -1) == 0 */
#define BOARDSIZE    ((MAX_BOARD + 2) * (MAX_BOARD + 1) + 1)
#define BOARDMIN     (MAX_BOARD + 2)
#define BOARDMAX     (MAX_BOARD + 1) * (MAX_BOARD + 1)
#define POS(i, j)    ((MAX_BOARD + 2) + (i) * (MAX_BOARD + 1) + (j))
#define I(pos)       ((pos) / (MAX_BOARD + 1) - 1)
#define J(pos)       ((pos) % (MAX_BOARD + 1) - 1)
#define PASS_MOVE    0
#define NO_MOVE      PASS_MOVE
#define NS           (MAX_BOARD + 1)
#define WE           1
#define SOUTH(pos)   ((pos) + NS)
#define WEST(pos)    ((pos) - 1)
#define NORTH(pos)   ((pos) - NS)
#define EAST(pos)    ((pos) + 1)
#define SW(pos)      ((pos) + NS - 1)
#define NW(pos)      ((pos) - NS - 1)
#define NE(pos)      ((pos) - NS + 1)
#define SE(pos)      ((pos) + NS + 1)
#define SS(pos)      ((pos) + 2 * NS)
#define WW(pos)      ((pos) - 2)
#define NN(pos)      ((pos) - 2 * NS)
#define EE(pos)      ((pos) + 2)

#define BOARD(i, j)  board[POS(i, j)]

/* 2D workaround macros for 1D functions. */
#define countlib2(m, n) countlib(POS(m, n))
#define countstones2(m, n) countstones(POS(m, n))
#define same_string2(m, n, i, j) same_string(POS(m, n), POS(i, j))
#define safe_move2(m, n, color) safe_move(POS(m, n), color)
#define is_legal2(m, n, color) is_legal(POS(m, n), color)
#define is_suicide2(m, n, color) is_suicide(POS(m, n), color)
#define is_self_atari2(m, n, color) is_self_atari(POS(m, n), color)
#define is_illegal_ko_capture2(m, n, color) \
        is_illegal_ko_capture(POS(m, n), color)
#define trymove2(m, n, color, message, i, j, komaster, kom_i, kom_j) \
        trymove(POS(m, n), color, message, POS(i, j), \
                komaster, POS(kom_i, kom_j))
#define tryko2(m, n, color, message, komaster, kom_i, kom_j) \
        tryko(POS(m, n), color, message, komaster, POS(kom_i, kom_j))
#define neighbor_of_string2(m, n, i, j) \
        neighbor_of_string(POS(m, n), POS(i, j))
#define liberty_of_string2(m, n, i, j) \
        liberty_of_string(POS(m, n), POS(i, j))
#define is_ko_point2(m, n) is_ko_point(POS(m, n))
#define does_capture_something2(m, n, color) \
        does_capture_something(POS(m, n), color)
#define add_stone2(m, n, color) add_stone(POS(m, n), color)
#define is_worm_origin2(wi, wj, i, j) is_worm_origin(POS(wi, wj), POS(i, j))

/* board utility functions */

int find_origin(int str);
int chainlinks(int str, int adj[MAXCHAIN]);
int chainlinks2(int str, int adj[MAXCHAIN], int lib);

/* This is increased by one anytime a move is (permanently) played or
 * the board is cleared.
 */
extern int position_number;

/* Count and/or find liberties at (i, j). */
int countlib(int str);
int findlib(int str, int maxlib, int *libs);
int approxlib(int pos, int color, int maxlib, int *libs);
int count_common_libs(int str1, int str2);
int find_common_libs(int str1, int str2, int maxlib, int *libs);
int have_common_lib(int str1, int str2, int *lib);

void start_timer(int n);
double time_report(int n, const char *occupation, int i, int j);

/* Play at (m, n) and then count the liberties. */
int accurate_approxlib(int pos, int color, int maxlib, int *libs);

/* Check for self atari. */
int is_self_atari(int pos, int color);

/* Count the number of stones in a string. */
int countstones(int str);
int findstones(int str, int maxstones, int *stones);

/* Exported from incremental_board.c so that reading.c can use it. */
void incremental_order_moves(int move, int color, int string,
			     int *number_edges, int *number_same_string,
			     int *number_own, int *number_opponent,
			     int *captured_stones, int *threatened_stones,
			     int *saved_stones, int *number_open);

  
void dump_stack(void);
void report_worm(int m, int n);
void ascii_report_worm(char *string);
void report_dragon(int m, int n);
void ascii_report_dragon(char *string);

/* prototypes for reorientation functions */

void  rotate2(int i, int j, int *ri, int *rj, int rot);
void  inv_rotate2(int i, int j, int *ri, int *rj, int rot);
int  rotate1(int pos, int rot);
int  inv_rotate1(int pos, int rot);

/* Is this point inside the board? */
#if 0
#define ON_BOARD2(i, j) ((i)>=0 && (j)>=0 && (i)<board_size && (j)<board_size)
#else
/*
 * For the case when expr can only be slightly negative,
 *    if (expr < 0 || expr > something)
 * is equivalent to
 *    if ( (unsigned)expr > something)
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
 * Defined in utils.c.
 */
extern int deltai[8]; /* = { 1,  0, -1,  0,  1, -1, -1, 1}; */
extern int deltaj[8]; /* = { 0, -1,  0,  1, -1, -1,  1, 1}; */
extern int delta[8];  /* = { NS, -1, -NS, 1, NS-1, -NS-1, -NS+1, NS+1}; */

void store_position(Position *pos);
void restore_position(Position *pos);

/* Forward struct declarations. */
struct pattern;
struct pattern_db;
struct fullboard_pattern;
struct half_eye_data;

/*
 * Try to match a pattern in the database to the board. Callback for
 * each match
 */
typedef void (*matchpat_callback_fn_ptr)(int m, int n, int color,
                                         struct pattern *, int rotation,
                                         void *data);
typedef void (*fullboard_matchpat_callback_fn_ptr)(int ti, int tj,
                                                   struct fullboard_pattern *,
                                                   int rotation);
void global_matchpat(matchpat_callback_fn_ptr callback, int color,
		     struct pattern_db *pdb, void *callback_data,
		     char goal[BOARDMAX]);
void fullboard_matchpat(fullboard_matchpat_callback_fn_ptr callback,
			int color, struct fullboard_pattern *pattern);

void reading_cache_init(int bytes);
void reading_cache_clear(void);

/* reading.c */
int attack(int str, int *move);
int find_defense(int str, int *move);
int attack_and_defend(int str,
		      int *attack_code, int *attack_point,
		      int *defend_code, int *defense_point);
int attack_either(int astr, int bstr);
int defend_both(int astr, int bstr);
int break_through(int apos, int bpos, int cpos);
int naive_ladder(int str, int *move);
#define MOVE_ORDERING_PARAMETERS 67
void tune_move_ordering(int params[MOVE_ORDERING_PARAMETERS]);
void draw_reading_shadow(void);
void purge_persistent_reading_cache(void);
void reading_hotspots(float values[MAX_BOARD][MAX_BOARD]);

/* readconnect.c */
int connect(int str1, int str2, int *move);
int disconnect(int str1, int str2, int *move);


int liberty_of_string(int pos, int str);
int neighbor_of_string(int pos, int str);
int same_string(int str1, int str2);
int adjacent_strings(int str1, int str2);
int is_ko(int pos, int color, int *ko_pos);
int is_ko_point(int pos);
int komaster_trymove(int pos, int color,
		     const char *message, int str,
		     int komaster, int kom_pos,
		     int *new_komaster, int *new_kom_pos,
		     int *is_conditional_ko, int consider_conditional_ko);
int does_capture_something(int pos, int color);
void mark_string(int str, char mx[BOARDMAX], char mark);
void mark_string2(int m, int n, char mx[MAX_BOARD][MAX_BOARD], char mark);
int move_in_stack(int pos, int cutoff);
void get_move_from_stack(int k, int *move, int *color);
int stones_on_board(int color);
int obvious_false_eye(int i, int j, int color);
int owl_topological_eye(int i, int j, int color);
int vital_chain(int m, int n);
int confirm_safety(int m, int n, int color, int size, int *di, int *dj);
void set_depth_values(int level);
void modify_depth_values(int n);
void increase_depth_values(void);
void decrease_depth_values(void);
void set_temporary_depth_values(int d, int b, int f, int k, 
				int br, int b2, int ss);
void restore_depth_values(void);
int safe_move(int move, int color);
void join_dragons(int ai, int aj, int bi, int bj);
int dragon_escape(char goal[BOARDMAX], int color, int escape_value[BOARDMAX]);
int lively_dragon_exists(int color);
int is_same_worm(int w1, int w2);
int is_worm_origin(int w, int pos);
void propagate_worm(int pos);
void transform(int i, int j, int *ti, int *tj, int trans);
void offset(int i, int j, int basei, int basej, int *ti, int *tj, int trans);
void find_cuts(void);
void find_connections(void);
void modify_eye_spaces(void);

/* functions to add (or remove) move reasons */
void clear_move_reasons(void);
void add_lunch(int eater, int food);
void remove_lunch(int eater, int food);
void add_attack_move(int pos, int ww);
void add_defense_move(int pos, int ww);
void add_attack_threat_move(int pos, int ww);
void add_defense_threat_move(int pos, int ww);
void add_connection_move(int pos, int dr1, int dr2);
void add_cut_move(int pos, int dr1, int dr2);
void add_antisuji_move(int pos);
void add_semeai_move(int pos, int dr);
void add_semeai_threat(int pos, int dr);

void add_owl_attack_move(int pos, int dr);
void add_owl_defense_move(int pos, int dr);
void add_owl_attack_threat_move(int pos, int dr);
void add_owl_defense_threat_move(int pos, int dr);
void add_owl_prevent_threat_move(int pos, int dr);
void add_owl_uncertain_defense_move(int pos, int dr);
void add_owl_uncertain_attack_move(int pos, int dr);

void add_my_atari_atari_move(int pos, int size);
void add_your_atari_atari_move(int pos, int size);
void add_vital_eye_move(int pos, int eyespace, int color);
void add_attack_either_move(int pos, int str1, int str2);
void add_defend_both_move(int pos, int str1, int str2);
void add_block_territory_move(int pos);
void add_expand_territory_move(int pos);
void add_expand_moyo_move(int pos);
void add_strategical_attack_move(int pos, int dr);
void add_strategical_defense_move(int pos, int dr);
void add_worthwhile_threat_move(int pos);
void add_replacement_move(int from, int to);
void set_minimum_move_value(int pos, float value);
void set_maximum_move_value(int pos, float value);
void set_minimum_territorial_value(int pos, float value);
void set_maximum_territorial_value(int pos, float value);
void add_shape_value(int pos, float value);
void add_followup_value(int pos, float value);
void add_reverse_followup_value(int pos, float value);

void find_stones_saved_by_move(int i, int j, int color,
                               char saved_stones[MAX_BOARD][MAX_BOARD]);

int owl_lively(int i, int j);
int owl_escape_value(int i, int j);
int owl_goal_dragon(int i, int j);
int owl_eyespace(int ai, int aj, int bi, int bj);
int owl_big_eyespace(int ai, int aj, int bi, int bj);
void owl_reasons(int color);

void unconditional_life(int unconditional_territory[MAX_BOARD][MAX_BOARD],
			int color);
void find_superstring(int str, int *num_stones, int *stones);
void find_superstring_liberties(int str, int *liberties, int *libs,
                                int liberty_cap);
void find_proper_superstring_liberties(int str, int *liberties, int *libs, 
                                       int liberty_cap);
void find_superstring_stones_and_liberties(int str, int *num_stones,
					   int *stones, int *liberties,
					   int *libs, int liberty_cap);
void superstring_chainlinks(int str, int *num_adj, int adj[MAXCHAIN],
                            int liberty_cap);
void proper_superstring_chainlinks(int str, int *num_adj, 
                                   int adj[MAXCHAIN], int liberty_cap);

int placehand(int handicap);          /* place stones on board only */


/* Various different strategies for finding a move */
void fuseki(int color);
void semeai(int color);
void small_semeai(void);
void small_semeai_analyzer(int, int, int, int);
void shapes(int color);
void endgame_shapes(int color);

int atari_atari(int color, int *move, int save_verbose);
int atari_atari_confirm_safety(int color, int tpos, int *move,
			       int minsize);
int atari_atari_try_combination(int color, int apos, int bpos);

int review_move_reasons(int *i, int *j, float *val, int color,
			float pure_threat_value, float lower_bound);
int fill_liberty(int *i, int *j, int color);
int aftermath_genmove(int *i, int *j, int color,
		      int under_control[BOARDMAX],
		      int do_capture_dead_stones);
int revise_semeai(int color);

int owl_attack(int m, int n, int *ui, int *uj, int *certain);
int owl_defend(int m, int n, int *ui, int *uj, int *certain);
int owl_threaten_attack(int m, int n, int *ui, int *uj, int *vi, int *vj);
int owl_threaten_defense(int m, int n, int *ui, int *uj, int *vi, int *vj);
int owl_does_defend(int ti, int tj, int m, int n);
int owl_confirm_safety(int ti, int tj, int m, int n, int *di, int *dj);
int owl_does_attack(int ti, int tj, int m, int n);
int owl_connection_defends(int ti, int tj, int ai, int aj, int bi, int bj);
int owl_substantial(int i, int j);
void owl_analyze_semeai(int ai, int aj, int bi, int bj);
void purge_persistent_owl_cache(void);
void owl_hotspots(float values[MAX_BOARD][MAX_BOARD]);

void change_attack(int str, int move, int acode);
void change_defense(int str, int move, int dcode);
void change_attack_threat(int str, int move, int acode);
void change_defense_threat(int str, int move, int dcode);
int attack_move_known(int move, int str);
int defense_move_known(int move, int str);
int attack_threat_move_known(int move, int str);
int defense_threat_move_known(int move, int str);
void worm_reasons(int color);

int does_attack(int move, int str);
int does_defend(int move, int str);
int double_atari(int m, int n, int color);
int play_attack_defend_n(int color, int do_attack, int num_moves, ...);
int play_attack_defend2_n(int color, int do_attack, int num_moves, ...);
int play_break_through_n(int color, int num_moves, ...);
int cut_possible(int i, int j, int color);
int defend_against(int ti, int tj, int color, int ai, int aj);
int somewhere(int color, int num_moves, ...);

/* moyo.c */
int area_color(int m, int n);
int moyo_color(int m, int n);
int terri_color(int m, int n);
int delta_moyo(int ti, int tj, int color);
int delta_moyo_simple(int ti, int tj, int color);
int delta_terri(int ti, int tj, int color);
int diff_moyo(int ti, int tj, int color);
int diff_terri(int ti, int tj, int color);
int area_stone(int m, int n);
int area_space(int m, int n);

/* Printmoyo values, specified by -m flag. */
#define PRINTMOYO_TERRITORY         0x01
#define PRINTMOYO_MOYO              0x02
#define PRINTMOYO_AREA              0x04
/* The following have been borrowed by the influence functions below. */
#define PRINTMOYO_INITIAL_INFLUENCE 0x08
#define PRINTMOYO_PRINT_INFLUENCE   0x10
#define PRINTMOYO_NUMERIC_INFLUENCE 0x20
#define PRINTMOYO_PERMEABILITY      0x40
#define PRINTMOYO_STRENGTH          0x80
#define PRINTMOYO_ATTENUATION       0x100

/* Influence functions. */
void compute_initial_influence(int color, int dragons_known);
void resegment_initial_influence(void);
int influence_delta_territory(int m, int n, int color,
                              char saved_stones[MAX_BOARD][MAX_BOARD]);
int influence_delta_moyo(int m, int n, int color,
                         char saved_stones[MAX_BOARD][MAX_BOARD]);
int influence_delta_strict_moyo(int m, int n, int color,
                                char saved_stones[MAX_BOARD][MAX_BOARD]);
int influence_delta_area(int m, int n, int color,
                         char saved_stones[MAX_BOARD][MAX_BOARD]);
int influence_delta_strict_area(int m, int n, int color,
                                char saved_stones[MAX_BOARD][MAX_BOARD]);
int influence_territory_color(int m, int n);
int influence_moyo_color(int m, int n);
int influence_area_color(int m, int n);
int influence_get_moyo_size(int m, int n, int color, int initial);
float influence_estimate_score(float moyo_coeff, float area_coeff);

void  old_estimate_score(int color, float *lower_bound, float *upper_bound);
float estimate_score(float *upper, float *lower);

void print_initial_influence(int color, int dragons_known);
void print_move_influence(int m, int n, int color,
                          char saved_stones[MAX_BOARD][MAX_BOARD]);
void get_initial_influence(int color, int dragons_known,
                           float white_influence[MAX_BOARD][MAX_BOARD],
                           float black_influence[MAX_BOARD][MAX_BOARD],
                           int influence_regions[MAX_BOARD][MAX_BOARD]);
void get_move_influence(int i, int j, int color,
                        char saved_stones[MAX_BOARD][MAX_BOARD],
                        float white_influence[MAX_BOARD][MAX_BOARD],
                        float black_influence[MAX_BOARD][MAX_BOARD],
                        int influence_regions[MAX_BOARD][MAX_BOARD]);
void compute_escape_influence(char goal[BOARDMAX], int color,
                              int escape_value[BOARDMAX],
                              int dragons_known);

/* Eye space functions. */
int is_eye_space(int pos);
int is_proper_eye_space(int pos);
int is_marginal_eye_space(int pos);
int max_eye_value(int pos);
void make_proper_eye_space(int pos, int color);
void remove_eyepoint(int pos, int color);


/* debugging support */
void move_considered(int i, int j, float value);


/* SGF routines for debugging purposes in sgffile.c */
int  sgffile_write_line(const char *, ...);
void sgffile_dragon_status(int, int, int );
void goaldump(char goal[BOARDMAX]);
void begin_sgftreedump(struct SGFTree_t *tree);
void end_sgftreedump(const char *filename);



/* ================================================================ */
/*                         global variables                         */
/* ================================================================ */

/* The board and the other parameters deciding the current position. */
extern int           board_size;             /* board size (usually 19) */
extern Intersection  board[BOARDSIZE];       /* go board */
extern Intersection  shadow[BOARDMAX];      /* reading tree shadow */
extern int           board_ko_pos;

extern float         komi;
extern int           black_captured;   /* num. of black stones captured */
extern int           white_captured;

extern int           movenum;          /* movenumber - used for debug output */

extern int           disable_threat_computation;
extern int           disable_endgame_patterns;
extern int           doing_scoring;

/* Reading parameters */
extern int depth;               /* deep reading cutoff */
extern int backfill_depth;      /* deep reading cutoff */
extern int backfill2_depth;     /* deep reading cutoff */
extern int superstring_depth;   /* deep reading cutoff */
extern int branch_depth;        /* deep reading cutoff */
extern int fourlib_depth;       /* deep reading cutoff */
extern int ko_depth;            /* deep ko reading cutoff */
extern int aa_depth;            /* deep global reading cutoff */
extern int owl_distrust_depth;  /* below this owl trusts the optics code */
extern int owl_branch_depth;    /* below this owl tries only one variation */
extern int owl_reading_depth;   /* owl does not read below this depth */
extern int owl_node_limit;      /* maximum number of nodes considered */
extern int level;
extern float best_move_values[10];
extern int best_movei[10];
extern int best_movej[10];

extern int chinese_rules;

extern int stackp;                /* stack pointer */
extern int count_variations;      /* count (decidestring) */
extern SGFTree *sgf_dumptree;


struct stats_data {
  int  nodes;                     /* Number of visited nodes while reading */
  int  position_entered;          /* Number of Positions entered. */
  int  position_hits;             /* Number of hits of Positions. */
  int  read_result_entered;       /* Number of Read_results entered. */
  int  read_result_hits;          /* Number of hits of Read_results. */
  int  hash_collisions;           /* Number of hash collisions. */
};

extern struct stats_data stats;


struct half_eye_data {
  int type;         /* HALF_EYE or FALSE_EYE; */
  int num_attacks;  /* number of attacking points */
  int attack_point[4];  /* the move to attack a topological halfeye */
  int num_defends;      /* number of defending points */
  int defense_point[4]; /* the move to defend a topological halfeye */
};

/* array of half-eye data */
extern struct half_eye_data half_eye[BOARDMAX];

/*
 * data concerning a worm. A copy is kept at each vertex of the worm.
 */

#define MAX_TACTICAL_POINTS 10

struct worm_data {
  int color;         /* its color */
  int size;          /* its cardinality */
  float effective_size; /* stones and surrounding spaces */
  int origin;        /* the origin of the string. Two vertices are in */
                     /* the same worm iff they have same origin. */
  int liberties;     /* number of liberties */
  int liberties2;    /* number of second order liberties */
  int liberties3;    /* third order liberties (empty vertices at distance 3) */
  int liberties4;    /* fourth order liberties */
  int lunch;         /* if lunch != 0 then lunch points to a boundary */
                     /* worm which can be captured easily. */
  int cutstone;      /* 1=potential cutting stone; 2=cutting stone */
  int cutstone2;     /* Number of potential cuts involving the worm. */
  int genus;         /* number of connected components of the complement, less one */
  int inessential;   /* 1=inessential worm */
  int invincible;    /* 1=strongly unconditionally non-capturable */
  int unconditional_status; /* ALIVE, DEAD, WHITE_BORDER, BLACK_BORDER, UNKNOWN */

  /* The following arrays keeps track of up to MAX_TACTICAL_POINTS
   * different attack, defense, attack threat, and defense threat
   * points with corresponding result codes. (0 = loss, 1 = bad ko, 2
   * = good ko, 3 = win). The arrays are guaranteed to be sorted with
   * respect to the codes so that the first element contains the best
   * result.
   */
  int attack_codes[MAX_TACTICAL_POINTS];
  int attack_points[MAX_TACTICAL_POINTS];
  int defend_codes[MAX_TACTICAL_POINTS];
  int defense_points[MAX_TACTICAL_POINTS];
  int attack_threat_codes[MAX_TACTICAL_POINTS];
  int attack_threat_points[MAX_TACTICAL_POINTS];
  int defense_threat_codes[MAX_TACTICAL_POINTS];
  int defense_threat_points[MAX_TACTICAL_POINTS];
};

extern struct worm_data worm[BOARDMAX];


/*
 * data concerning a dragon. A copy is kept at each stone of the string.
 */

struct dragon_data {
  int color;    /* its color                                                 */
  int id;       /* the index into the dragon2 array                          */
  int origin;   /* the origin of the dragon. Two vertices are in the same    */
                /* dragon iff they have same origin.                         */
  int size;     /* size of the dragon                                        */
  float effective_size; /* stones and surrounding spaces                     */
  int status;   /* (ALIVE, DEAD, UNKNOWN, CRITICAL)                          */
  int owl_threat_status;   /* CAN_THREATEN_ATTACK or CAN_THREATEN_DEFENSE    */
  int owl_status;          /* (ALIVE, DEAD, UNKNOWN, CRITICAL, UNCHECKED)    */
  int owl_attack_point;    /* vital point for attack                         */
  int owl_attack_certain;  /* 0 if owl reading node limit is reached         */
  int owl_second_attack_point;/* if attacker gets both attack points, wins   */
  int owl_defense_point;   /* vital point for defense                        */
  int owl_defend_certain;  /* 0 if owl reading node limit is reached         */
  int owl_second_defense_point;/* if defender gets both attack points, wins  */
  int matcher_status;  /* status used by pattern matching                    */
};

extern struct dragon_data dragon[BOARDMAX];

/* Supplementary data concerning a dragon. Only one copy is stored per
 * dragon in the dragon2 array.
 */

#define MAX_NEIGHBOR_DRAGONS 10

struct dragon_data2 {
  int origin;                         /* the origin of the dragon            */
  int adjacent[MAX_NEIGHBOR_DRAGONS]; /* adjacent dragons                    */
  int neighbors;                      /* number of adjacent dragons          */
  int moyo;                           /* size of surrounding influence moyo  */
  int safety;                         /* a more detailed status estimate     */
  int escape_route; /* a measurement of likelihood of escape                 */
  int genus;    /* the number of eyes (approximately)                        */
  int heyes;    /* the number of half eyes                                   */
  int heye;     /* coordinates of a half eye                                 */
  int lunch;    /* if lunch != 0 then lunch points to a boundary worm which  */
                /* can be captured easily.                                   */
  int semeai;          /* true if a dragon is part of a semeai               */
  int semeai_margin_of_safety; /* if small, the semeai is close              */
};

/* dragon2 is dynamically allocated */
extern int number_of_dragons;
extern struct dragon_data2 *dragon2;

/* Macros for accessing the dragon2 data with board coordinates and
 * the dragon data with a dragon id.
 */
#define DRAGON2(m, n) dragon2[dragon[POS(m, n)].id]
#define DRAGON(d) dragon[dragon2[d].origin]

struct aftermath_data {
  int white_captured;
  int black_captured;
  int white_prisoners;
  int black_prisoners;
  int white_territory;
  int black_territory;
  int white_area;
  int black_area;
  int white_control[BOARDMAX];
  int black_control[BOARDMAX];
  int final_status[BOARDMAX];
};

struct eye_data {
  int color;/* BLACK, WHITE, BLACK_BORDERED, WHITE_BORDERED or GRAY_BORDERED */
  int esize;         /* size of the eyespace                                 */
  int msize;         /* number of marginal vertices                          */
  int origin;        /* The origin                                           */
  int maxeye;        /* number of eyes if defender plays first               */
  int mineye;        /* number of eyes if attacker plays first               */
  int attack_point;  /* vital point for attack                               */
  int defense_point; /* vital point for defense                              */
  int dragon;        /* origin of the surrounding dragon                     */

  /* The above fields are constant on the whole eyespace. */
  /* ---------------------------------------------------------------- */
  /* The below fields are not. */

  int marginal;             /* This vertex is marginal                    */
  int type;                 /* Various characteristics of the eyespace    */
  int neighbors;            /* number of neighbors in eyespace            */
  int marginal_neighbors;   /* number of marginal neighbors               */
  int cut;                  /* Opponent can cut at vertex.                */
};

typedef struct eye_data row_of_eye_data[BOARDMAX];

extern struct eye_data white_eye[BOARDMAX];
extern struct eye_data black_eye[BOARDMAX];

/* The following declarations have to be postponed until after the
 * definition of struct eye_data or struct half_eye_data.
 */

void compute_eyes(int i, int  j, int *max, int *min,
                  int *attacki, int *attackj, int *defendi, int *defendj,
                  struct eye_data eye[BOARDMAX],
                  struct half_eye_data heye[BOARDMAX],
                  int add_moves, int color);
void compute_eyes_pessimistic(int i, int  j, int *max, int *min,
                              int *pessimistic_min,
                              int *attacki, int *attackj,
                              int *defendi, int *defendj,
                              struct eye_data eye[BOARDMAX],
                              struct half_eye_data heye[BOARDMAX]);
int recognize_eye2(int m, int n, int *attacki, int *attackj,
                    int *defendi, int *defendj, int *max, int *min,
                    struct eye_data eye[BOARDMAX],
                    struct half_eye_data heye[BOARDMAX],
                    int add_moves, int color);
void propagate_eye (int pos, struct eye_data eye[BOARDMAX]);
void originate_eye(int i, int j, int m, int n,
		   int *esize, int *msize,
		   struct eye_data eye[BOARDMAX]);
int topological_eye(int m, int n, int color, int *ai, int *aj,
                    int *di, int *dj, 
                    struct eye_data b_eye[BOARDMAX],
                    struct eye_data w_eye[BOARDMAX],
                    struct half_eye_data heye[BOARDMAX]);
void add_half_eye(int m, int n, struct eye_data eye[BOARDMAX], 
                  struct half_eye_data heye[BOARDMAX]);
void make_domains(struct eye_data b_eye[BOARDMAX],
                  struct eye_data w_eye[BOARDMAX],
		  int owl_call);

int is_halfeye(struct half_eye_data heye[BOARDMAX], int pos);
void remove_half_eye(struct half_eye_data heye[BOARDMAX],
                     int i, int j, int color);

/* our own abort() which prints board state on the way out.
 * i,j is a "relevant" board position for info */
void abortgo(const char *file, int line, const char *msg, int i, int j);

#ifndef NDEBUG
/* avoid dangling else */
#define ASSERT2(x, i, j) if (x) ; else abortgo(__FILE__, __LINE__, #x, i, j)
#define ASSERT1(x, pos) if (x) ; else abortgo(__FILE__, __LINE__, #x, I(pos), J(pos))
#else
#define ASSERT2(x, i, j)
#define ASSERT1(x, pos)
#endif

#define gg_assert(x) ASSERT2(x, -1, -1);

#endif  /* _LIBERTY_H_ */


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
