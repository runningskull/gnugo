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

#ifndef _LIBERTY_H_
#define _LIBERTY_H_

#include "gnugo.h"

/* local versions of absolute value, min and max */

#define gg_abs(x) ((x) < 0 ? -(x) : (x))
#define gg_min(a, b) ((a)<(b) ? (a) : (b))
#define gg_max(a, b) ((a)<(b) ? (b) : (a))

/* Avoid compiler warnings with unused parameters */
#define UNUSED(x)  (void)x


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
 * FIXME: This is not sufficiently large;  above stackp==0, the incremental 
 *   board code doesn't necessarily re-use all indices.  This is a problem
 *   only in very pathological cases, and is extremely unlikely to occur in
 *   practice.
 */
#define MAX_STRINGS (2 * MAX_BOARD * MAX_BOARD / 3)

/* Per gf: Unconditional_life() can get very close to filling the 
 * entire board under certain circumstances. This was discussed in 
 * the list around August 21, 2001, in a thread with the subject 
 * "gnugo bug logs".
 */
#define MAXSTACK  MAX_BOARD * MAX_BOARD
#define MAXCHAIN  160

/* 1D board macros.
 * Note that POS(-1, -1) == 0
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

#define BOARD(i, j)   board[POS(i, j)]

#define REVERSE_RESULT(result)		(WIN - result)

/* Transformation stuff. */
#define MAX_OFFSET			(2*MAX_BOARD - 1) * (2*MAX_BOARD - 1)
#define OFFSET(dx, dy)\
  ((dy + MAX_BOARD - 1) * (2*MAX_BOARD - 1) + (dx + MAX_BOARD - 1))
#define OFFSET_DELTA(dx, dy)		(OFFSET(dx, dy) - OFFSET(0, 0))
#define CENTER_OFFSET(offset)		(offset - OFFSET(0, 0))
#define TRANSFORM(offset, trans)	(transformation[offset][trans])
#define AFFINE_TRANSFORM(offset, trans, delta)\
  (transformation[offset][trans] + delta)
#define TRANSFORM2(x, y, tx, ty, trans)\
  do {\
    *tx = transformation2[trans][0][0] * (x) + transformation2[trans][0][1] * (y);\
    *ty = transformation2[trans][1][0] * (x) + transformation2[trans][1][1] * (y);\
  } while (0)


/* This struct holds the internal board state.
 */
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
  int move_history_pointer;

  float komi;
  int move_number;
};


/* board utility functions */
int find_origin(int str);
int chainlinks(int str, int adj[MAXCHAIN]);
int chainlinks2(int str, int adj[MAXCHAIN], int lib);
int chainlinks3(int str, int adj[MAXCHAIN], int lib);
int extended_chainlinks(int str, int adj[MAXCHAIN], int both_colors);


/* This is increased by one anytime a move is (permanently) played or
 * the board is cleared.
 */
extern int position_number;


/* Detect vertex on edge. */
int is_edge_vertex(int pos);


/* Count and/or find liberties at (pos). */
int countlib(int str);
int findlib(int str, int maxlib, int *libs);
int fastlib(int pos, int color, int ignore_capture);
int approxlib(int pos, int color, int maxlib, int *libs);
int accuratelib(int pos, int color, int maxlib, int *libs);
int count_common_libs(int str1, int str2);
int find_common_libs(int str1, int str2, int maxlib, int *libs);
int have_common_lib(int str1, int str2, int *lib);


void start_timer(int n);
double time_report(int n, const char *occupation, int move, double mintime);

void update_random_seed(void);


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


void transformation_init(void);

  
void dump_stack(void);
void report_worm(int m, int n);
void ascii_report_worm(char *string);
void report_dragon(FILE *outfile, int pos);
void ascii_report_dragon(char *string);
struct dragon_data2 * dragon2_func(int pos);

/* prototypes for reorientation functions */

void rotate2(int i, int j, int *ri, int *rj, int rot);
void inv_rotate2(int i, int j, int *ri, int *rj, int rot);
int rotate1(int pos, int rot);
int inv_rotate1(int pos, int rot);

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
 * Defined in utils.c.
 */
extern int deltai[8]; /* = { 1,  0, -1,  0,  1, -1, -1, 1}; */
extern int deltaj[8]; /* = { 0, -1,  0,  1, -1, -1,  1, 1}; */
extern int delta[8];  /* = { NS, -1, -NS, 1, NS-1, -NS-1, -NS+1, NS+1}; */

void store_board(struct board_state *state);
void restore_board(struct board_state *state);

/* Forward struct declarations. */
struct pattern;
struct pattern_db;
struct fullboard_pattern;
struct corner_pattern;
struct corner_db;
struct half_eye_data;
struct movelist;
struct tree_node_list;
struct match_node;

/*
 * Try to match a pattern in the database to the board. Callback for
 * each match
 */
typedef void (*matchpat_callback_fn_ptr)(int anchor, int color,
                                         struct pattern *, int rotation,
                                         void *data);
typedef void (*fullboard_matchpat_callback_fn_ptr)(int move,
                                                   struct fullboard_pattern *,
                                                   int rotation);
typedef void (*corner_matchpat_callback_fn_ptr)(int move, int color,
						struct corner_pattern *pattern,
						int rotation);
void matchpat(matchpat_callback_fn_ptr callback, int color,
	      struct pattern_db *pdb, void *callback_data,
	      char goal[BOARDMAX]);
void matchpat_goal_anchor(matchpat_callback_fn_ptr callback, int color,
	      struct pattern_db *pdb, void *callback_data,
	      char goal[BOARDMAX], int anchor_in_goal);
void fullboard_matchpat(fullboard_matchpat_callback_fn_ptr callback,
			int color, struct fullboard_pattern *pattern);
void corner_matchpat(corner_matchpat_callback_fn_ptr callback, int color,
		     struct corner_db *database);
void dfa_match_init(void);
void tree_match_init(void);
void tree_initialize_pointers(struct tree_node_list *tnl,
                              struct match_node *matches,
                              int tnl_size,
                              int matches_size);

void reading_cache_init(void);
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
int attack_threats(int pos, int max_points, int moves[], int codes[]);

int restricted_defend1(int str, int *move, int komaster, int kom_pos,
		       int num_forbidden_moves, int *forbidden_moves);
int restricted_attack2(int str, int *move, int komaster, int kom_pos,
		       int num_forbidden_moves, int *forbidden_moves);

int simple_ladder(int str, int *move);
#define MOVE_ORDERING_PARAMETERS 67
void tune_move_ordering(int params[MOVE_ORDERING_PARAMETERS]);
void draw_reading_shadow(void);

/* persistent.c */
void purge_persistent_reading_cache(void);
void clear_persistent_reading_cache(void);
int search_persistent_reading_cache(int routine, int str, int *result,
				    int *move);
void store_persistent_reading_cache(int routine, int str, int result,
				    int move, int nodes);
void delete_persistent_reading_cache_entry(int routine, int str);
void reading_hotspots(float values[BOARDMAX]);
void purge_persistent_owl_cache(void);
void clear_persistent_owl_cache(void);
int search_persistent_owl_cache(int routine, int apos, int bpos, int cpos,
				int *result, int *move, int *move2,
				int *certain);
void store_persistent_owl_cache(int routine, int apos, int bpos, int cpos,
				int result, int move, int move2, int certain,
				int tactical_nodes, char goal[BOARDMAX],
				int goal_color);
void owl_hotspots(float values[BOARDMAX]);

/* readconnect.c */
int string_connect(int str1, int str2, int *move);
int disconnect(int str1, int str2, int *move);
int non_transitivity(int str1, int str2, int str3, int *move);

/* board.c */
int liberty_of_string(int pos, int str);
int second_order_liberty_of_string(int pos, int str);
int neighbor_of_string(int pos, int str);
int has_neighbor(int pos, int color);
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
int move_in_stack(int pos, int cutoff);
void get_move_from_stack(int k, int *move, int *color);
int stones_on_board(int color);

int obvious_false_eye(int pos, int color);
int owl_topological_eye(int pos, int color);
int vital_chain(int pos);
int confirm_safety(int move, int color, int *defense_point,
		   char safe_stones[BOARDMAX]);
float blunder_size(int move, int color, int *defense_point,
		   char safe_stones[BOARDMAX]);
void set_depth_values(int level);
void modify_depth_values(int n);
void increase_depth_values(void);
void decrease_depth_values(void);
void set_temporary_depth_values(int d, int b, int f, int k, 
				int br, int b2, int ss);
void restore_depth_values(void);

int safe_move(int move, int color);

void join_dragons(int d1, int d2);
int dragon_escape(char goal[BOARDMAX], int color, char escape_value[BOARDMAX]);
void compute_refined_dragon_weaknesses(void);
struct eyevalue;
float crude_dragon_weakness(int safety, struct eyevalue *genus, int has_lunch,
			    float moyo_value, float escape_route);
int is_same_dragon(int d1, int d2);
int are_neighbor_dragons(int d1, int d2);
int first_worm_in_dragon(int w);
int next_worm_in_dragon(int w);
int lively_dragon_exists(int color);
void compute_dragon_influence(void);
void set_strength_data(int color, char safe_stones[BOARDMAX],
		       float strength[BOARDMAX]);

void get_lively_stones(int color, char safe_stones[BOARDMAX]);
int is_same_worm(int w1, int w2);
int is_worm_origin(int w, int pos);
void propagate_worm(int pos);
void transform2(int i, int j, int *ti, int *tj, int trans);
void find_cuts(void);
void find_connections(void);
void modify_eye_spaces(void);

/* movelist.c */
int movelist_move_known(int move, int max_points, int points[], int codes[]);
void movelist_change_point(int move, int code, int max_points, 
			   int points[], int codes[]);

/* safety.c */
int compute_surroundings(int pos, int apos, int showboard,
			 int *surround_size);
int is_surrounded(int pos);
int does_surround(int move, int dragon);
void reset_surround_data(void);
int surround_map(int dr, int pos);

/* functions to add (or remove) move reasons */
void clear_move_reasons(void);
void add_lunch(int eater, int food);
void remove_lunch(int eater, int food);
void add_attack_move(int pos, int ww, int code);
void add_defense_move(int pos, int ww, int code);
void add_attack_threat_move(int pos, int ww, int code);
void remove_attack_threat_move(int pos, int ww);
void add_defense_threat_move(int pos, int ww, int code);
void add_connection_move(int pos, int dr1, int dr2);
void add_cut_move(int pos, int dr1, int dr2);
void add_antisuji_move(int pos);
void add_semeai_move(int pos, int dr);
void add_semeai_threat(int pos, int dr);

void add_owl_attack_move(int pos, int dr, int code);
void add_owl_defense_move(int pos, int dr, int code);
void add_owl_attack_threat_move(int pos, int dr, int code);
void add_owl_defense_threat_move(int pos, int dr, int code);
void add_owl_prevent_threat_move(int pos, int dr);
void add_owl_uncertain_defense_move(int pos, int dr);
void add_owl_uncertain_attack_move(int pos, int dr);

void add_my_atari_atari_move(int pos, int size);
void add_your_atari_atari_move(int pos, int size);
void add_vital_eye_move(int pos, int eyespace, int color);
void add_invasion_move(int pos);
void add_expand_territory_move(int pos);
void add_expand_moyo_move(int pos);
void add_strategical_attack_move(int pos, int dr);
void add_strategical_defense_move(int pos, int dr);
void add_worthwhile_threat_move(int pos);
void add_replacement_move(int from, int to);
int  set_minimum_move_value(int pos, float value);
void set_maximum_move_value(int pos, float value);
void set_minimum_territorial_value(int pos, float value);
void set_maximum_territorial_value(int pos, float value);
void add_shape_value(int pos, float value);
void add_followup_value(int pos, float value);
void add_reverse_followup_value(int pos, float value);
void record_top_move(int move, float val);
void remove_top_move(int move);
void scale_randomness(int pos, float scaling);

/* Parameters to add_either_move and add_all_move */
#define ATTACK_STRING  1
#define DEFEND_STRING  2
void add_either_move(int pos, int reason1, int target1,
		     int reason2, int target2);
void add_all_move(int pos, int reason1, int target1,
		  int reason2, int target2);

void add_gain_move(int pos, int target1, int target2);
void add_loss_move(int pos, int target1, int target2);


int get_attack_threats(int pos, int max_strings, int strings[]);
int get_defense_threats(int pos, int max_strings, int strings[]);
void get_saved_worms(int pos, char saved[BOARDMAX]);
void get_saved_dragons(int pos, char saved[BOARDMAX]);
void mark_safe_stones(int color, int move_pos,
		      const char saved_dragons[BOARDMAX],
		      const char saved_worms[BOARDMAX],
		      char safe_stones[BOARDMAX]);


int owl_lively(int pos);
int owl_escape_value(int pos);
int owl_goal_dragon(int pos);
int owl_eyespace(int pos);
int owl_big_eyespace(int pos);
int owl_proper_eye(int pos);
int owl_eye_size(int pos);
int owl_strong_dragon(int pos);
void owl_reasons(int color);

void unconditional_life(int unconditional_territory[BOARDMAX], int color);
void find_superstring(int str, int *num_stones, int *stones);
void find_superstring_conservative(int str, int *num_stones, int *stones);
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

int place_fixed_handicap(int handicap); /* place stones on board only */
int place_free_handicap(int handicap); /* place stones on board only */
int free_handicap_remaining_stones(void);
int free_handicap_total_stones(void);


/* Various different strategies for finding a move */
void fuseki(int color);
void semeai(int color);
void new_semeai(int color);
void small_semeai(int save_verbose);
void shapes(int color);
void endgame_shapes(int color);

void combinations(int color);
int atari_atari(int color, int *attack_move, char defense_moves[BOARDMAX],
		int save_verbose);
int atari_atari_confirm_safety(int color, int tpos, int *move, int minsize,
			       const char saved_dragons[BOARDMAX],
			       const char saved_worms[BOARDMAX]);

int atari_atari_blunder_size(int color, int tpos, int *move,
			     const char safe_stones[BOARDMAX]);

int review_move_reasons(int *move, float *val, int color,
			float pure_threat_value, float lower_bound,
			int allowed_moves[BOARDMAX]);
int fill_liberty(int *move, int color);
int aftermath_genmove(int *aftermath_move, int color,
		      int under_control[BOARDMAX],
		      int do_capture_dead_stones);

int owl_attack(int target, int *attack_point, int *certain, int *kworm);
int owl_defend(int target, int *defense_point, int *certain, int *kworm);
int owl_threaten_attack(int target, int *attack1, int *attack2);
int owl_threaten_defense(int target, int *defend1, int *defend2);
int owl_does_defend(int move, int target, int *kworm);
int owl_confirm_safety(int move, int target, int *defense_point, int *kworm);
int owl_does_attack(int move, int target, int *kworm);
int owl_connection_defends(int move, int target1, int target2);
int owl_substantial(int str);
void owl_analyze_semeai(int apos, int bpos, 
			int *resulta, int *resultb, int *move,
			int owl);

int genmove_restricted(int *i, int *j, int color, int allowed_moves[BOARDMAX]);

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
int double_atari(int move, int color, float *value,
		 char safe_stones[BOARDMAX]);
int play_attack_defend_n(int color, int do_attack, int num_moves, ...);
int play_attack_defend2_n(int color, int do_attack, int num_moves, ...);
int play_break_through_n(int color, int num_moves, ...);
int play_connect_n(int color, int do_connect, int num_moves, ...);
int cut_possible(int pos, int color);
int defend_against(int move, int color, int apos);
int somewhere(int color, int check_alive, int num_moves, ...);
int visible_along_edge(int color, int apos, int bpos);

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
#define PRINTMOYO_VALUE_TERRITORY   0x200

/* These values are used to communicate whether stones are safe or
 * have been saved, when computing influence.
 */
#define INFLUENCE_SAFE_STONE	1
#define INFLUENCE_SAVED_STONE	2

/* These values are used to communicate the status of stones when analyzing
 * a move for potentially being a blunder.
 */
/* 	dead		0 */
#define SAFE_STONE 	1
#define OWL_SAVED_STONE	2

/* This format is used when exporting the moyo segmentation. */
#define MAX_MOYOS MAX_BOARD*MAX_BOARD

struct moyo_data
{
  int number; /* Number of moyos. */
  int segmentation[BOARDMAX]; /* Numbers the moyos. */
  int size[MAX_MOYOS];
  int owner[MAX_MOYOS];
  float territorial_value[MAX_MOYOS];
};

/* We use a forward declaration of influence_data so that the rest
 * of the engine can reference influence data. It can only be accessed
 * in influence.c, however!
 */
struct influence_data;
extern struct influence_data initial_black_influence;
extern struct influence_data initial_white_influence;
extern struct influence_data move_influence;
extern struct influence_data followup_influence;

#define INITIAL_INFLUENCE(color) ((color) == WHITE ? \
				    &initial_white_influence \
				  : &initial_black_influence)
#define OPPOSITE_INFLUENCE(color) (INITIAL_INFLUENCE(OTHER_COLOR(color)))

#define DEFAULT_STRENGTH 100.0

/* Influence functions. */
void compute_influence(int color, const char safe_stones[BOARDMAX],
		       const float strength[BOARDMAX],
		       struct influence_data *q,
		       int move, const char *trace_message);
void compute_followup_influence(const struct influence_data *base,
			        struct influence_data *q, 
		                int move, const char *trace_message);
void compute_escape_influence(int color, const char safe_stones[BOARDMAX],
			      const float strength[BOARDMAX],
                              char escape_value[BOARDMAX]);

float influence_delta_territory(const struct influence_data *base,
	                        const struct influence_data *q, int color,
				int move);
int retrieve_delta_territory_cache(int pos, int color, float *move_value,
			           float *followup_value);
void store_delta_territory_cache(int pos, int color, float move_value,
				 float followup_value);

int whose_territory(const struct influence_data *q, int pos);
int whose_moyo(const struct influence_data *q, int pos);
int whose_area(const struct influence_data *q, int pos);
float influence_territory(const struct influence_data *q, int pos, int color);
void influence_get_moyo_segmentation(const struct influence_data *q,
	       			     struct moyo_data *moyo);
void influence_get_moyo_data(const struct influence_data *q,
			     int moyo_color[BOARDMAX],
			     float territory_value[BOARDMAX]);
void get_influence(const struct influence_data *q,
		   float white_influence[BOARDMAX],
		   float black_influence[BOARDMAX],
		   int regions[BOARDMAX]);
float influence_score(const struct influence_data *q);
void resegment_initial_influence(void);
void influence_mark_non_territory(int pos, int color);

float estimate_score(float *upper, float *lower);

/* Eye space functions. */
int is_eye_space(int pos);
int is_proper_eye_space(int pos);
int is_marginal_eye_space(int pos);
int max_eye_value(int pos);
void test_eyeshape(int eyesize, int *eye_vertices);


/* debugging support */
void goaldump(char goal[BOARDMAX]);
void move_considered(int move, float value);


/* SGF routines for debugging purposes in sgffile.c */
void sgffile_begindump(struct SGFTree_t *tree);
void sgffile_enddump(const char *filename);



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
extern int          move_history_pointer;

extern float        komi;
extern int          movenum;      /* movenumber - used for debug output */
		    
extern Intersection shadow[BOARDMAX];      /* reading tree shadow */

		    
extern int          disable_threat_computation;
extern int          disable_endgame_patterns;
extern int          doing_scoring;

/* Transformation arrays */
extern int	    transformation[MAX_OFFSET][8];
extern const int    transformation2[8][2][2];

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
extern int level;               /* controls the strength of play */
extern int semeai_variations;   /* max variations considered reading semeai */
extern float best_move_values[10];
extern int best_moves[10];

extern int chinese_rules;
extern int experimental_owl_ext;     /* use experimental owl (GAIN/LOSS) */
extern int experimental_semeai;      /* use experimental semeai module */
extern int experimental_connections; /* use experimental connection module */
extern int alternate_connections;    /* use alternate connection module */
extern int owl_threats;              /* compute owl threats */
extern int experimental_influence;   /* use experimental influence module */

extern int thrashing_dragon; /* Dead opponent's dragon trying to live */

/* Experimental reading */
extern char *rgoal;
extern int goallib;

extern int stackp;                /* stack pointer */
extern int count_variations;      /* count (decidestring) */
extern SGFTree *sgf_dumptree;

/* Arrays pointing out the closest worms from each vertex.  The first
 * one is the closest worms of either color, the last two ones ignore
 * worms of the other color.  Beyond a certain distance from any worm
 * no close worm is listed at all.  Only the closest worm is listed
 * and if more than one are equally close they are all listed. The
 * number of equally close worms is given in the number_*_worms
 * arrays. If more than MAX_CLOSE_WORMS are equally close, none is
 * listed.
 *
 * See compute_effective_worm_sizes() in worm.c for details.
 */
#define MAX_CLOSE_WORMS 4
extern int close_worms[BOARDMAX][MAX_CLOSE_WORMS];
extern int number_close_worms[BOARDMAX];
extern int close_black_worms[BOARDMAX][MAX_CLOSE_WORMS];
extern int number_close_black_worms[BOARDMAX];
extern int close_white_worms[BOARDMAX][MAX_CLOSE_WORMS];
extern int number_close_white_worms[BOARDMAX];

extern int false_eye_territory[BOARDMAX];

struct stats_data {
  int nodes;                     /* Number of visited nodes while reading */
  int position_entered;          /* Number of Positions entered. */
  int position_hits;             /* Number of hits of Positions. */
  int read_result_entered;       /* Number of Read_results entered. */
  int read_result_hits;          /* Number of hits of Read_results. */
  int hash_collisions;           /* Number of hash collisions. */
};

extern struct stats_data stats;


struct eyevalue {
#if 0
  char maxeye;       /* number of eyes if defender plays first               */
  char mineye;       /* number of eyes if attacker plays first               */
#else
  unsigned char a; /* number of eyes if attacker plays first twice */
  unsigned char b; /* number of eyes if attacker plays first */
  unsigned char c; /* number of eyes if defender plays first */
  unsigned char d; /* number of eyes if defender plays first twice */
#endif
};


struct half_eye_data {
  float value;          /* Topological eye value. */
  char type;            /* HALF_EYE or FALSE_EYE; */
  int num_attacks;      /* number of attacking points */
  int attack_point[4];  /* the moves to attack a topological halfeye */
  int num_defends;      /* number of defending points */
  int defense_point[4]; /* the moves to defend a topological halfeye */
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
  int attack_points[MAX_TACTICAL_POINTS];
  int attack_codes[MAX_TACTICAL_POINTS];
  int defense_points[MAX_TACTICAL_POINTS];
  int defense_codes[MAX_TACTICAL_POINTS];
  int attack_threat_points[MAX_TACTICAL_POINTS];
  int attack_threat_codes[MAX_TACTICAL_POINTS]; 
  int defense_threat_points[MAX_TACTICAL_POINTS];
  int defense_threat_codes[MAX_TACTICAL_POINTS];
};

extern struct worm_data worm[BOARDMAX];

/* Surround cache (see surround.c) */

#define MAX_SURROUND 10

struct surround_data {
  int dragon_number;           /* number of the (surrounded) beast */
  char surround_map[BOARDMAX]; /* surround map                     */
};

extern struct surround_data surroundings[MAX_SURROUND];
extern int surround_pointer;

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
  int crude_status;   /* (ALIVE, DEAD, UNKNOWN, CRITICAL)                    */
  int owl_threat_status;   /* CAN_THREATEN_ATTACK or CAN_THREATEN_DEFENSE    */
  int owl_status;          /* (ALIVE, DEAD, UNKNOWN, CRITICAL, UNCHECKED)    */
  int owl_attack_point;    /* vital point for attack                         */
  int owl_attack_code;     /* ko result code                                 */
#if 0
  int owl_attack_points[MAX_TACTICAL_POINTS];
  int owl_attack_codes[MAX_TACTICAL_POINTS];
#endif
  int owl_attack_certain;  /* 0 if owl reading node limit is reached         */
  int owl_second_attack_point;/* if attacker gets both attack points, wins   */
  int owl_defense_point;   /* vital point for defense                        */
  int owl_defense_code;    /* ko result code                                 */
  int owl_defense_certain; /* 0 if owl reading node limit is reached         */
  int owl_second_defense_point;/* if defender gets both attack points, wins  */
  int status;              /* best trusted status                            */
  int owl_attack_kworm;    /* only valid when owl_attack_code is GAIN        */
  int owl_defense_kworm;   /* only valid when owl_defense_code is LOSS       */
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
  int hostile_neighbors;              /* neighbors of opposite color         */
  int moyo_size;		      /* size of surrounding influence moyo, */
  float moyo_territorial_value;       /* ...and its territorial value */
  int safety;                         /* a more detailed status estimate     */
  float weakness; /* A new (3.3.x) continuos estimate of the dragon's safety */
  float weakness_pre_owl;     /* Dragon safety based on pre-owl computations */
  int escape_route; /* a measurement of likelihood of escape                 */
  struct eyevalue genus;    /* the number of eyes (approximately)            */
  int heye;     /* coordinates of a half eye                                 */
  int lunch;    /* if lunch != 0 then lunch points to a boundary worm which  */
                /* can be captured easily.                                   */
  int semeai;          /* true if a dragon is part of a semeai               */
  int semeai_margin_of_safety; /* if small, the semeai is close              */
  int surround_status;         /* Is it surrounded?                          */
  int surround_size;           /* Size of the surrounding area               */
};

/* dragon2 is dynamically allocated */
extern int number_of_dragons;
extern struct dragon_data2 *dragon2;

/* Macros for accessing the dragon2 data with board coordinates and
 * the dragon data with a dragon id.
 */
#if 1 /* Trust DRAGON2 accesses */
#define DRAGON2(pos) dragon2[dragon[pos].id]
#else
struct dragon_data2 * dragon2_func(int pos);
#define DRAGON2(pos) (*dragon2_func(pos))
#endif

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
  struct eyevalue value; /* Number of eyes.                                  */
  int attack_point;  /* vital point for attack                               */
  int defense_point; /* vital point for defense                              */

  /* The above fields are constant on the whole eyespace. */
  /* ---------------------------------------------------------------- */
  /* The below fields are not. */

  char marginal;             /* This vertex is marginal                    */
  char type;                 /* Various characteristics of the eyespace    */
  char neighbors;            /* number of neighbors in eyespace            */
  char marginal_neighbors;   /* number of marginal neighbors               */
  char cut;                  /* Opponent can cut at vertex.                */
};

extern struct eye_data white_eye[BOARDMAX];
extern struct eye_data black_eye[BOARDMAX];

/* The following declarations have to be postponed until after the
 * definition of struct eye_data or struct half_eye_data.
 */

void compute_eyes(int pos, struct eyevalue *value,
                  int *attack_point, int *defense_point,
                  struct eye_data eye[BOARDMAX],
                  struct half_eye_data heye[BOARDMAX],
                  int add_moves, int color);
void compute_eyes_pessimistic(int pos, struct eyevalue *value,
                              char *pessimistic_min,
                              int *attack_point, int *defense_point,
                              struct eye_data eye[BOARDMAX],
                              struct half_eye_data heye[BOARDMAX]);
void propagate_eye(int pos, struct eye_data eye[BOARDMAX]);
int find_eye_dragons(int origin, struct eye_data eye[BOARDMAX], int eye_color,
		     int dragons[], int max_dragons);
float topological_eye(int pos, int color,
		      struct eye_data my_eye[BOARDMAX],
		      struct half_eye_data heye[BOARDMAX]);
void add_false_eye(int pos, struct eye_data eye[BOARDMAX], 
		   struct half_eye_data heye[BOARDMAX]);
void make_domains(struct eye_data b_eye[BOARDMAX],
                  struct eye_data w_eye[BOARDMAX],
		  int owl_call);
void find_half_and_false_eyes(int color, struct eye_data eye[BOARDMAX],
			      struct half_eye_data heye[BOARDMAX],
			      char find_mask[BOARDMAX]);

void set_eyevalue(struct eyevalue *e, int a, int b, int c, int d);
int min_eye_threat(struct eyevalue *e);
int min_eyes(struct eyevalue *e);
int max_eyes(struct eyevalue *e);
int max_eye_threat(struct eyevalue *e);
void add_eyevalues(struct eyevalue *e1, struct eyevalue *e2,
		   struct eyevalue *sum);
int eye_move_urgency(struct eyevalue *e);
char *eyevalue_to_string(struct eyevalue *e);

int is_halfeye(struct half_eye_data heye[BOARDMAX], int pos);
int is_false_eye(struct half_eye_data heye[BOARDMAX], int pos);

/* Our own abort() which prints board state on the way out.
 * (i, j) is a "relevant" board position for info. */
void abortgo(const char *file, int line, const char *msg, int i, int j);

#if GG_TURN_OFF_ASSERTS
#define ASSERT2(x, i, j)
#define ASSERT1(x, pos)
#else
/* avoid dangling else */
/* FIXME: Should probably re-write these using do {...} while (0) idiom. */
#define ASSERT2(x, i, j) if (x) ; else abortgo(__FILE__, __LINE__, #x, i, j)
#define ASSERT1(x, pos) if (x) ; else abortgo(__FILE__, __LINE__, #x, I(pos), J(pos))
#endif

#define gg_assert(x) ASSERT2(x, -1, -1);

#endif  /* _LIBERTY_H_ */


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
