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

#ifndef _LIBERTY_H_
#define _LIBERTY_H_

#include "board.h"
#include "hash.h"
#include "gnugo.h"
#include "winsocket.h"

/* ================================================================ */
/*                           public variables                       */
/* ================================================================ */


/* ================================================================ */


#define FALSE_EYE          1
#define HALF_EYE           2


#define REVERSE_RESULT(result)		(WIN - result)


void start_timer(int n);
double time_report(int n, const char *occupation, int move, double mintime);
void showstats(void);
void clearstats(void);

void transformation_init(void);

void ascii_report_worm(char *string);
void report_dragon(FILE *outfile, int pos);
void ascii_report_dragon(char *string);
struct dragon_data2 *dragon2_func(int pos);

/* Routine names used by persistent and non-persistent caching schemes. */
enum routine_id {
  OWL_ATTACK,
  OWL_DEFEND,
  SEMEAI,
  FIND_DEFENSE,
  ATTACK,
  CONNECT,
  DISCONNECT,
  BREAK_IN,
  BLOCK_OFF,
  OWL_THREATEN_ATTACK,
  OWL_THREATEN_DEFENSE,
  OWL_DOES_DEFEND,
  OWL_DOES_ATTACK,
  OWL_CONNECTION_DEFENDS,
  OWL_SUBSTANTIAL,
  OWL_CONFIRM_SAFETY,
  ANALYZE_SEMEAI,
  NUM_CACHE_ROUTINES
};

#define ROUTINE_NAMES \
  "owl_attack", \
  "owl_defend", \
  "semeai", \
  "find_defense", \
  "attack", \
  "connect", \
  "disconnect", \
  "break_in", \
  "block_off", \
  "owl_threaten_attack", \
  "owl_threatend_defense", \
  "owl_does_defend", \
  "owl_does_attack", \
  "owl_connection_defends", \
  "owl_substantial", \
  "owl_confirm_safety", \
  "analyze_semeai"

/* To prioritize between different types of reading, we give a cost
 * ranking to each of the routines above:
 *
 * 4 semeai
 * 3 owl
 * 2 break-in
 * 1 connection
 * 0 tactical reading
 *
 * -1 is left at the end for a consistency check.
 */
#define ROUTINE_COSTS \
  3, 3, 4, 0, 0, 1, 1, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, -1
  

const char *routine_id_to_string(enum routine_id routine);


/* This is used for both the dragon status and safety fields.
 * Also used for unconditional status in struct worm_data and for the
 * final status computed by the aftermath code.
 */
enum dragon_status {
  DEAD,
  ALIVE,
  CRITICAL,
  UNKNOWN,
  UNCHECKED,
  CAN_THREATEN_ATTACK,
  CAN_THREATEN_DEFENSE, 
  INESSENTIAL,
  TACTICALLY_DEAD,
  ALIVE_IN_SEKI,
  STRONGLY_ALIVE,
  INVINCIBLE,
  INSUBSTANTIAL,
  WHITE_TERRITORY,
  BLACK_TERRITORY,
  DAME,
  NUM_DRAGON_STATUS
};

#define DRAGON_STATUS_NAMES \
  "dead", \
  "alive", \
  "critical", \
  "unknown", \
  "unchecked", \
  "can threaten attack", \
  "can threaten defense", \
  "inessential", \
  "tactically dead", \
  "alive in seki", \
  "strongly alive", \
  "invincible", \
  "insubstantial", \
  "white_territory", \
  "black_territory", \
  "dame"

const char *status_to_string(enum dragon_status status);


/* Forward struct declarations. */
struct pattern;
struct pattern_db;
struct fullboard_pattern;
struct corner_pattern;
struct corner_db;
struct half_eye_data;
struct movelist;

/*
 * Try to match a pattern in the database to the board. Callbacks for
 * each match.
 */
typedef void (*matchpat_callback_fn_ptr)(int anchor, int color,
                                         struct pattern *, int rotation,
                                         void *data);
typedef void (*fullboard_matchpat_callback_fn_ptr)(int move,
                                                   struct fullboard_pattern *,
                                                   int rotation);
typedef void (*corner_matchpat_callback_fn_ptr)(int move, int color,
						struct corner_pattern *pattern,
						int trans,
						int *stones, int num_stones);
void matchpat(matchpat_callback_fn_ptr callback, int color,
	      struct pattern_db *pdb, void *callback_data,
	      signed char goal[BOARDMAX]);
void matchpat_goal_anchor(matchpat_callback_fn_ptr callback, int color,
	      struct pattern_db *pdb, void *callback_data,
	      signed char goal[BOARDMAX], int anchor_in_goal);
void fullboard_matchpat(fullboard_matchpat_callback_fn_ptr callback,
			int color, struct fullboard_pattern *pattern);
void corner_matchpat(corner_matchpat_callback_fn_ptr callback, int color,
		     struct corner_db *database);
void dfa_match_init(void);

void reading_cache_init(int bytes);
void reading_cache_clear(void);
float reading_cache_default_size(void);

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

int restricted_defend1(int str, int *move,
		       int num_forbidden_moves, int *forbidden_moves);
int restricted_attack2(int str, int *move,
		       int num_forbidden_moves, int *forbidden_moves);

int simple_ladder(int str, int *move);
#define MOVE_ORDERING_PARAMETERS 67
void tune_move_ordering(int params[MOVE_ORDERING_PARAMETERS]);
void draw_reading_shadow(void);

/* persistent.c */
void persistent_cache_init(void);
void purge_persistent_caches(void);
void clear_persistent_caches(void);

int search_persistent_reading_cache(enum routine_id routine, int str,
				    int *result, int *move);
void store_persistent_reading_cache(enum routine_id routine, int str,
				    int result, int move, int nodes);
void reading_hotspots(float values[BOARDMAX]);
int search_persistent_connection_cache(enum routine_id routine,
				       int str1, int str2,
				       int *result, int *move);
void store_persistent_connection_cache(enum routine_id routine,
				       int str1, int str2,
				       int result, int move,
				       int tactical_nodes,
				       signed char connection_shadow[BOARDMAX]);
int search_persistent_breakin_cache(enum routine_id routine,
				    int str, Hash_data *goal_hash,
				    int breakin_node_limit,
				    int *result, int *move);
void store_persistent_breakin_cache(enum routine_id routine,
				    int str, Hash_data *goal_hash,
				    int result, int move,
				    int tactical_nodes,
				    int breakin_node_limit,
				    signed char breakin_shadow[BOARDMAX]);
int search_persistent_owl_cache(enum routine_id routine,
				int apos, int bpos, int cpos,
				int *result, int *move, int *move2,
				int *certain);
void store_persistent_owl_cache(enum routine_id routine,
				int apos, int bpos, int cpos,
				int result, int move, int move2, int certain,
				int tactical_nodes, signed char goal[BOARDMAX],
				int goal_color);
void owl_hotspots(float values[BOARDMAX]);
int search_persistent_semeai_cache(enum routine_id routine,
			           int apos, int bpos, int cpos, int color,
				   Hash_data *goal_hash,
				   int *resulta, int *resultb,
				   int *move, int *certain);
void store_persistent_semeai_cache(enum routine_id routine,
				   int apos, int bpos, int cpos, int color,
				   Hash_data *goal_hash,
				   int resulta, int resultb,
				   int move, int certain, int tactical_nodes,
				   signed char goala[BOARDMAX],
				   signed char goalb[BOARDMAX]);


/* readconnect.c */
int string_connect(int str1, int str2, int *move);
int disconnect(int str1, int str2, int *move);
int fast_disconnect(int str1, int str2, int *move);
int non_transitivity(int str1, int str2, int str3, int *move);

int break_in(int str, const signed char goal[BOARDMAX], int *move);
int block_off(int str1, const signed char goal[BOARDMAX], int *move);

int obvious_false_eye(int pos, int color);
void estimate_lunch_eye_value(int lunch, int *min, int *probable, int *max,
			      int appreciate_one_two_lunches);
int owl_topological_eye(int pos, int color);
int vital_chain(int pos);
int confirm_safety(int move, int color, int *defense_point,
		   signed char safe_stones[BOARDMAX]);
int dragon_weak(int pos);
float dragon_weakness(int pos, int ignore_dead_dragons);
int size_of_biggest_critical_dragon(void);
void change_dragon_status(int dr, enum dragon_status status);
float blunder_size(int move, int color, int *defense_point,
		   signed char safe_stones[BOARDMAX]);
void set_depth_values(int level, int report_levels);
void modify_depth_values(int n);
void increase_depth_values(void);
void decrease_depth_values(void);
int get_depth_modification(void);

int safe_move(int move, int color);
int does_secure(int color, int move, int pos);

void compute_new_dragons(int dragon_origins[BOARDMAX]);
void join_dragons(int d1, int d2);
int dragon_escape(signed char goal[BOARDMAX], int color,
		  signed char escape_value[BOARDMAX]);
void compute_refined_dragon_weaknesses(void);
void compute_strategic_sizes(void);

struct eyevalue;
void compute_dragon_genus(int d, struct eyevalue *genus, int eye_to_exclude);
float crude_dragon_weakness(int safety, struct eyevalue *genus, int has_lunch,
			    float moyo_value, float escape_route);

int is_same_dragon(int d1, int d2);
int are_neighbor_dragons(int d1, int d2);
void mark_dragon(int pos, signed char mx[BOARDMAX], signed char mark);
int first_worm_in_dragon(int d);
int next_worm_in_dragon(int w);
int lively_dragon_exists(int color);
void compute_dragon_influence(void);
void set_strength_data(int color, signed char safe_stones[BOARDMAX],
		       float strength[BOARDMAX]);
void mark_inessential_stones(int color, signed char safe_stones[BOARDMAX]);

void add_cut(int apos, int bpos, int move);
void cut_reasons(int color);

void get_lively_stones(int color, signed char safe_stones[BOARDMAX]);
int is_same_worm(int w1, int w2);
int is_worm_origin(int w, int pos);
void propagate_worm(int pos);
void find_cuts(void);
void find_connections(void);

/* movelist.c */
int movelist_move_known(int move, int max_points, int points[], int codes[]);
void movelist_change_point(int move, int code, int max_points, 
			   int points[], int codes[]);

/* surround.c */
int compute_surroundings(int pos, int apos, int showboard,
			 int *surround_size);
int is_surrounded(int pos);
int does_surround(int move, int dragon);
void reset_surround_data(void);
int surround_map(int dr, int pos);

/* functions to add (or remove) move reasons */
void collect_move_reasons(int color);

void clear_move_reasons(void);
void add_lunch(int eater, int food);
void add_attack_move(int pos, int ww, int code);
void add_defense_move(int pos, int ww, int code);
void add_attack_threat_move(int pos, int ww, int code);
void remove_attack_threat_move(int pos, int ww);
void add_defense_threat_move(int pos, int ww, int code);
void add_connection_move(int pos, int dr1, int dr2);
void add_cut_move(int pos, int dr1, int dr2);
void add_antisuji_move(int pos);
void add_semeai_move(int pos, int dr);
void add_potential_semeai_attack(int pos, int dr1, int dr2);
void add_potential_semeai_defense(int pos, int dr1, int dr2);
void add_semeai_threat(int pos, int dr);

void add_owl_attack_move(int pos, int dr, int kworm, int code);
void add_owl_defense_move(int pos, int dr, int code);
void add_owl_attack_threat_move(int pos, int dr, int code);
void add_owl_defense_threat_move(int pos, int dr, int code);
void add_owl_prevent_threat_move(int pos, int dr);
void add_owl_uncertain_defense_move(int pos, int dr);
void add_owl_uncertain_attack_move(int pos, int dr);
void add_gain_move(int pos, int target1, int target2);
void add_loss_move(int pos, int target1, int target2);

void add_my_atari_atari_move(int pos, int size);
void add_your_atari_atari_move(int pos, int size);
void add_vital_eye_move(int pos, int eyespace, int color);
void add_invasion_move(int pos);
void add_expand_territory_move(int pos);
void add_expand_moyo_move(int pos);
void add_strategical_attack_move(int pos, int dr);
void add_strategical_defense_move(int pos, int dr);
void add_worthwhile_threat_move(int pos);
void add_replacement_move(int from, int to, int color);

/* Parameters to add_either_move and add_all_move */
#define ATTACK_STRING  1
#define DEFEND_STRING  2
void add_either_move(int pos, int reason1, int target1,
		     int reason2, int target2);
void add_all_move(int pos, int reason1, int target1,
		  int reason2, int target2);

int set_minimum_move_value(int pos, float value);
void set_maximum_move_value(int pos, float value);
void set_minimum_territorial_value(int pos, float value);
void set_maximum_territorial_value(int pos, float value);
void add_shape_value(int pos, float value);
void add_followup_value(int pos, float value);
void add_reverse_followup_value(int pos, float value);
int list_move_reasons(FILE *out, int pos);
void print_all_move_values(FILE *output);
void record_top_move(int move, float val);
void remove_top_move(int move);
void scale_randomness(int pos, float scaling);
void compute_move_probabilities(float probabilities[BOARDMAX]);

void register_good_attack_threat(int move, int target);
int is_known_good_attack_threat(int move, int target);

void register_known_safe_move(int move);
int is_known_safe_move(int move);

int get_attack_threats(int pos, int max_strings, int strings[]);
int get_defense_threats(int pos, int max_strings, int strings[]);
void get_saved_worms(int pos, signed char saved[BOARDMAX]);
void get_saved_dragons(int pos, signed char saved[BOARDMAX]);
void mark_safe_stones(int color, int move_pos,
		      const signed char saved_dragons[BOARDMAX],
		      const signed char saved_worms[BOARDMAX],
		      signed char safe_stones[BOARDMAX]);


int owl_lively(int pos);
int owl_escape_value(int pos);
int owl_goal_dragon(int pos);
int owl_eyespace(int pos);
int owl_big_eyespace(int pos);
int owl_mineye(int pos);
int owl_maxeye(int pos);
int owl_proper_eye(int pos);
int owl_eye_size(int pos);
int owl_lunch(int str);
int owl_strong_dragon(int pos);
void owl_reasons(int color);

void unconditional_life(int unconditional_territory[BOARDMAX], int color);
void clear_unconditionally_meaningless_moves(void);
void find_unconditionally_meaningless_moves(int unconditional_territory[BOARDMAX],
					    int color);
int unconditionally_meaningless_move(int pos, int color,
				     int *replacement_move);
void unconditional_move_reasons(int color);

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
void semeai(void);
void semeai_move_reasons(int color);
void shapes(int color);
void endgame(int color);
void endgame_shapes(int color);

void combinations(int color);
int atari_atari(int color, int *attack_move,
		signed char defense_moves[BOARDMAX],
		int save_verbose);
int atari_atari_confirm_safety(int color, int tpos, int *move, int minsize,
			       const signed char saved_dragons[BOARDMAX],
			       const signed char saved_worms[BOARDMAX]);

int atari_atari_blunder_size(int color, int tpos,
			     signed char defense_moves[BOARDMAX],
			     const signed char safe_stones[BOARDMAX]);

int review_move_reasons(int *move, float *value, int color,
			float pure_threat_value, float our_score,
			int allowed_moves[BOARDMAX],
			int use_thrashing_dragon_heuristics);
void prepare_move_influence_debugging(int pos, int color);
int fill_liberty(int *move, int color);
int aftermath_genmove(int color, int do_capture_dead_stones,
		      int allowed_moves[BOARDMAX]);
enum dragon_status aftermath_final_status(int color, int pos);

int mc_get_size_of_pattern_values_table(void);
int mc_load_patterns_from_db(const char *filename, unsigned int *values);
void mc_init_patterns(const unsigned int *values);
int choose_mc_patterns(char *name);
void list_mc_patterns(void);

void uct_genmove(int color, int *move, int *forbidden_moves,
		 int *allowed_moves, int nodes, float *move_values,
		 int *move_frequencies);

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
			int *resulta, int *resultb, int *semeai_move,
			int owl, int *semeai_result_certain);
void owl_analyze_semeai_after_move(int move, int color, int apos, int bpos,
				   int *resulta, int *resultb,
				   int *semeai_move, int owl,
				   int *semeai_result_certain,
				   int recompute_dragons);

void set_limit_search(int value);
void set_search_diamond(int pos);
void reset_search_mask(void);
void set_search_mask(int pos, int value);
int oracle_play_move(int pos, int color);
void consult_oracle(int color);
void summon_oracle(void);
void oracle_loadsgf(char *infilename, char *untilstring);
int oracle_threatens(int move, int target);
int within_search_area(int pos);
int metamachine_genmove(int color, float *value);
void draw_search_area(void);

int genmove_restricted(int color, int allowed_moves[BOARDMAX]);

void change_attack(int str, int move, int acode);
void change_defense(int str, int move, int dcode);
void change_attack_threat(int str, int move, int acode);
void change_defense_threat(int str, int move, int dcode);
int attack_move_known(int move, int str);
int defense_move_known(int move, int str);
int attack_threat_move_known(int move, int str);
int defense_threat_move_known(int move, int str);
void worm_reasons(int color);

int semeai_move_reason_known(int move, int dr);

int does_attack(int move, int str);
int does_defend(int move, int str);
int double_atari(int move, int color, float *value,
		 signed char safe_stones[BOARDMAX]);
int playing_into_snapback(int move, int color);
int play_attack_defend_n(int color, int do_attack, int num_moves, ...);
int play_attack_defend2_n(int color, int do_attack, int num_moves, ...);
int play_break_through_n(int color, int num_moves, ...);
int play_connect_n(int color, int do_connect, int num_moves, ...);
int play_lib_n(int color, int num_moves, ...);
int cut_possible(int pos, int color);
int defend_against(int move, int color, int apos);
int somewhere(int color, int check_alive, int num_moves, ...);
int visible_along_edge(int color, int apos, int bpos);
int test_symmetry_after_move(int move, int color, int strict);

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
void compute_influence(int color, const signed char safe_stones[BOARDMAX],
		       const float strength[BOARDMAX],
		       struct influence_data *q,
		       int move, const char *trace_message);
void compute_followup_influence(const struct influence_data *base,
			        struct influence_data *q, 
		                int move, const char *trace_message);
void compute_escape_influence(int color,
			      const signed char safe_stones[BOARDMAX],
			      const signed char goal[BOARDMAX],
			      const float strength[BOARDMAX],
                              signed char escape_value[BOARDMAX]);

float influence_delta_territory(const struct influence_data *base,
	                        const struct influence_data *q, int color,
				int move);
int retrieve_delta_territory_cache(int pos, int color, float *move_value,
			           float *followup_value,
				   const struct influence_data *base,
				   Hash_data safety_hash);
void store_delta_territory_cache(int pos, int color, float move_value,
				 float followup_value,
				 const struct influence_data *base,
			         Hash_data safety_hash);

int whose_territory(const struct influence_data *q, int pos);
int whose_moyo(const struct influence_data *q, int pos);
int whose_moyo_restricted(const struct influence_data *q, int pos);
int whose_area(const struct influence_data *q, int pos);
float influence_territory(const struct influence_data *q, int pos, int color);
void influence_get_territory_segmentation(struct influence_data *q,
	       			          struct moyo_data *moyo);
void get_influence(const struct influence_data *q,
		   float white_influence[BOARDMAX],
		   float black_influence[BOARDMAX],
		   float white_strength[BOARDMAX],
		   float black_strength[BOARDMAX],
		   float white_attenuation[BOARDMAX], 
		   float black_attenuation[BOARDMAX],
		   float white_permeability[BOARDMAX],
		   float black_permeability[BOARDMAX],
		   float territory_value[BOARDMAX],
		   int influence_regions[BOARDMAX],
		   int non_territory[BOARDMAX]);
float influence_score(const struct influence_data *q, int chinese_rules);
float game_status(int color);
void influence_mark_non_territory(int pos, int color);
int influence_considered_lively(const struct influence_data *q, int pos);
void influence_erase_territory(struct influence_data *q, int pos, int color);

void break_territories(int color_to_move, struct influence_data *q,
		       int store, int pos);
void clear_break_in_list(void);
void break_in_move_reasons(int color);

void choose_strategy(int color, float our_score, float game_status);

/* Eye space functions. */
int is_eye_space(int pos);
int is_proper_eye_space(int pos);
int is_marginal_eye_space(int pos);
int max_eye_value(int pos);
void test_eyeshape(int eyesize, int *eye_vertices);
int analyze_eyegraph(const char *coded_eyegraph, struct eyevalue *value,
		     char *analyzed_eyegraph);


/* debugging support */
void goaldump(const signed char goal[BOARDMAX]);
void move_considered(int move, float value);


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


/* ================================================================ */
/*                         global variables                         */
/* ================================================================ */

extern int disable_threat_computation;
extern int disable_endgame_patterns;
extern int doing_scoring;

/* Reading parameters */
extern int depth;               /* deep reading cutoff */
extern int backfill_depth;      /* deep reading cutoff */
extern int backfill2_depth;     /* deep reading cutoff */
extern int break_chain_depth;   /* deep reading cutoff */
extern int superstring_depth;   /* deep reading cutoff */
extern int branch_depth;        /* deep reading cutoff */
extern int fourlib_depth;       /* deep reading cutoff */
extern int ko_depth;            /* deep ko reading cutoff */
extern int aa_depth;            /* deep global reading cutoff */
extern int depth_offset;        /* keeps track of temporary depth changes */
extern int owl_distrust_depth;  /* below this owl trusts the optics code */
extern int owl_branch_depth;    /* below this owl tries only one variation */
extern int owl_reading_depth;   /* owl does not read below this depth */
extern int owl_node_limit;      /* maximum number of nodes considered */
extern int semeai_branch_depth;
extern int semeai_branch_depth2;
extern int semeai_node_limit;
extern int connect_depth;
extern int connect_depth2;
extern int connection_node_limit;
extern int breakin_depth;
extern int breakin_node_limit;
extern int semeai_variations;   /* max variations considered reading semeai */
extern float best_move_values[10];
extern int best_moves[10];

extern int experimental_owl_ext;     /* use experimental owl (GAIN/LOSS) */
extern int experimental_semeai;      /* use experimental semeai module */
extern int experimental_connections; /* use experimental connection module */
extern int alternate_connections;    /* use alternate connection module */
extern int owl_threats;              /* compute owl threats */
extern int experimental_break_in;    /* use experimental module breakin.c */
extern int cosmic_gnugo;             /* use center oriented influence */
extern int large_scale;              /* seek large scale captures */

extern int thrashing_dragon;        /* Dead opponent's dragon trying to live */
extern signed char thrashing_stone[BOARDMAX];       /* All thrashing stones. */

extern int transformation[MAX_OFFSET][8];
extern const int transformation2[8][2][2];


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
extern int forced_backfilling_moves[BOARDMAX];

extern double slowest_time;      /* Timing statistics */
extern int slowest_move;
extern int slowest_movenum;
extern double total_time;


struct eyevalue {
  unsigned char a; /* number of eyes if attacker plays first twice */
  unsigned char b; /* number of eyes if attacker plays first */
  unsigned char c; /* number of eyes if defender plays first */
  unsigned char d; /* number of eyes if defender plays first twice */
};


struct half_eye_data {
  float value;          /* Topological eye value. */
  unsigned char type;   /* HALF_EYE or FALSE_EYE; */
  int num_attacks;      /* number of attacking points */
  int attack_point[4];  /* the moves to attack a topological halfeye */
  int num_defenses;     /* number of defending points */
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
  enum dragon_status unconditional_status; /* ALIVE, DEAD, WHITE_TERRITORY,
					      BLACK_TERRITORY, UNKNOWN */

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

/* Unconditionally meaningless moves. */
int meaningless_black_moves[BOARDMAX];
int meaningless_white_moves[BOARDMAX];

/* Surround cache (see surround.c) */

#define MAX_SURROUND 10

struct surround_data {
  int dragon_number;                  /* number of the (surrounded) beast */
  signed char surround_map[BOARDMAX]; /* surround map                     */
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
  enum dragon_status crude_status; /* (ALIVE, DEAD, UNKNOWN, CRITICAL)       */
  enum dragon_status status;       /* best trusted status                    */
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
  float moyo_territorial_value;       /* ...and its territorial value        */
  enum dragon_status safety;          /* a more detailed status estimate     */
  float weakness;           /* a continuous estimate of the dragon's safety  */
  float weakness_pre_owl;   /* dragon safety based on pre-owl computations   */
  float strategic_size; /* An effective size including weakness of neighbors */
  int escape_route;         /* a measurement of likelihood of escape         */
  struct eyevalue genus;    /* the number of eyes (approximately)            */
  int heye;     /* coordinates of a half eye                                 */
  int lunch;    /* if lunch != 0 then lunch points to a boundary worm which  */
                /* can be captured easily.                                   */
  int surround_status;         /* Is it surrounded?                          */
  int surround_size;           /* Size of the surrounding area               */

  int semeais;         /* number of semeais in which the dragon is involved  */
  int semeai_defense_code ;/* Result code for semeai defense.                */
  int semeai_defense_point;/* Move found by semeai code to rescue dragon     */
  int semeai_defense_certain;
  int semeai_defense_target; /* The opponent dragon involved in the semeai   */
  int semeai_attack_code ; /* Result code for semeai attack.                 */
  int semeai_attack_point; /* Move found by semeai code to kill dragon       */
  int semeai_attack_certain;
  int semeai_attack_target; /* The opponent dragon involved in the semeai    */
  enum dragon_status owl_threat_status; /* CAN_THREATEN_ATTACK/DEFENSE       */
  enum dragon_status owl_status; /* (ALIVE, DEAD, UNKNOWN, CRITICAL, UNCHECKED)    */
  int owl_attack_point;    /* vital point for attack                         */
  int owl_attack_code;     /* ko result code                                 */
  int owl_attack_certain;  /* 0 if owl reading node limit is reached         */
  int owl_attack_node_count;
  int owl_second_attack_point;/* if attacker gets both attack points, wins   */
  int owl_defense_point;   /* vital point for defense                        */
  int owl_defense_code;    /* ko result code                                 */
  int owl_defense_certain; /* 0 if owl reading node limit is reached         */
  int owl_second_defense_point;/* if defender gets both attack points, wins  */
  int owl_attack_kworm;    /* only valid when owl_attack_code is GAIN        */
  int owl_defense_kworm;   /* only valid when owl_defense_code is LOSS       */
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
struct dragon_data2 *dragon2_func(int pos);
#define DRAGON2(pos) (*dragon2_func(pos))
#endif

#define DRAGON(d) dragon[dragon2[d].origin]

extern float white_score, black_score;

/* Global variables to tune strategy. */

extern float minimum_value_weight;
extern float maximum_value_weight;
extern float invasion_malus_weight;
extern float strategical_weight;
extern float territorial_weight;
extern float attack_dragon_weight;
extern float followup_weight;

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
  enum dragon_status final_status[BOARDMAX];
};

#define MAX_EYE_ATTACKS 3

struct eye_data {
  int color;             /* BLACK, WHITE, or GRAY                     */
  int esize;             /* size of the eyespace                      */
  int msize;             /* number of marginal vertices               */
  int origin;            /* The origin                                */
  struct eyevalue value; /* Number of eyes.                           */

  /* The above fields are constant on the whole eyespace.             */
  /* ---------------------------------------------------------------- */
  /* The below fields are not.                                        */

  unsigned char marginal;             /* This vertex is marginal               */
  unsigned char neighbors;            /* number of neighbors in eyespace       */
  unsigned char marginal_neighbors;   /* number of marginal neighbors          */
};

struct vital_eye_points {
  int attack_points[MAX_EYE_ATTACKS];
  int defense_points[MAX_EYE_ATTACKS];
};

extern struct vital_eye_points black_vital_points[BOARDMAX];
extern struct vital_eye_points white_vital_points[BOARDMAX];

extern struct eye_data white_eye[BOARDMAX];
extern struct eye_data black_eye[BOARDMAX];

/* Array with the information which was previously stored in the cut
 * field and in the INHIBIT_CONNECTION bit of the type field in struct
 * eye_data.
 */
extern int cutting_points[BOARDMAX];

/* The following declarations have to be postponed until after the
 * definition of struct eye_data or struct half_eye_data.
 */

void compute_eyes(int pos, struct eyevalue *value,
                  int *attack_point, int *defense_point,
                  struct eye_data eye[BOARDMAX],
                  struct half_eye_data heye[BOARDMAX],
		  int add_moves);
void compute_eyes_pessimistic(int pos, struct eyevalue *value,
                              int *pessimistic_min,
                              int *attack_point, int *defense_point,
                              struct eye_data eye[BOARDMAX],
                              struct half_eye_data heye[BOARDMAX]);
void propagate_eye(int pos, struct eye_data eye[BOARDMAX]);
int find_eye_dragons(int origin, struct eye_data eye[BOARDMAX], int eye_color,
		     int dragons[], int max_dragons);
void make_domains(struct eye_data b_eye[BOARDMAX],
                  struct eye_data w_eye[BOARDMAX],
		  int owl_call);
void partition_eyespaces(struct eye_data eye[BOARDMAX], int color);
void find_half_and_false_eyes(int color, struct eye_data eye[BOARDMAX],
			      struct half_eye_data heye[BOARDMAX],
			      int find_mask[BOARDMAX]);

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

#endif  /* _LIBERTY_H_ */


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
