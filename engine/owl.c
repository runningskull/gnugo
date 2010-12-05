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

/*
 * The code in this file implements "Optics With Limit-negotiation (OWL)."
 *
 * The life and death code in optics.c, works reasonably well as long as the
 * position is in a *terminal position*, which we define to be one where there
 * are no moves left which can expand the eye space, or limit it. In
 * situations where the dragon is surrounded, yet has room to thrash around a
 * bit making eyes, a simple application of the graph-based analysis will not
 * work. Instead, a bit of reading is needed to reach a terminal position.
 * The defender tries to expand his eyespace, the attacker to limit it, and
 * when neither finds an effective move, the position is evaluated. We call
 * this type of life and death reading *Optics With Limit-negotiation* (OWL).
 *
 *                             (|__|)
 *                            (@)(@))
 *                            |:v:: |
 *                           (       )
 *                            \|   |/
 *                            =#===#=
 *                            /___/
 * 
 *                The owl is noted for its keen vision 
 *                       and (purported) wisdom.
 */

#include "gnugo.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "liberty.h"
#include "readconnect.h"
#include "patterns.h"
#include "cache.h"
#include "sgftree.h"
#include "gg_utils.h"

#define MAX_MOVES 3           /* maximum number of branches at each node */
#define MAX_SEMEAI_MOVES 6    /* semeai branch factor */
#define MAX_SEMEAI_DEPTH 100  /* Don't read below this depth */
#define MAX_LUNCHES 10
#define MAX_GOAL_WORMS 15  /* maximum number of worms in a dragon to be */
                           /*   cataloged.  NOTE: Must fit in value2 in hashnode! */
#define MAX_ESCAPE 3  /* After this many escape moves, owl_determine_life is */
                      /*    not called                                       */

struct local_owl_data {
  signed char goal[BOARDMAX];
  signed char boundary[BOARDMAX];
  /* Same as goal, except never anything is removed from it. */
  signed char cumulative_goal[BOARDMAX];

  /* FIXME: neighbors[] and escape_values[] are never recomputed.
   *	    Consider moving these arrays from stack to a static or
   *	    dynamic variable so it is not copied around in
   *	    do_push_owl().  Be aware of semeai code though.
   */
  signed char neighbors[BOARDMAX];

  signed char escape_values[BOARDMAX];
  int color;

  struct eye_data my_eye[BOARDMAX];
  /* array of half-eye data for use during owl reading */
  struct half_eye_data half_eye[BOARDMAX];
  
  int lunch[MAX_LUNCHES];
  int lunch_attack_code[MAX_LUNCHES];
  int lunch_attack_point[MAX_LUNCHES];
  int lunch_defend_code[MAX_LUNCHES];
  int lunch_defense_point[MAX_LUNCHES];
  signed char inessential[BOARDMAX];
  
  int lunches_are_current; /* If true, owl lunch data is current */  

  signed char safe_move_cache[BOARDMAX];

  /* This is used to organize the owl stack. */
  struct local_owl_data *restore_from;
};


static int result_certain;

/* Statistics. */
static int local_owl_node_counter;
/* Node limitation. */
static int global_owl_node_counter = 0;

static struct local_owl_data *current_owl_data;
static struct local_owl_data *other_owl_data;

static int goal_worms_computed = 0;
static int owl_goal_worm[MAX_GOAL_WORMS];


#define MAX_CUTS 5

enum same_dragon_value {
  SAME_DRAGON_NOT_CONNECTED,
  SAME_DRAGON_MAYBE_CONNECTED,
  SAME_DRAGON_CONNECTED,
  SAME_DRAGON_ALL_CONNECTED
};

struct matched_pattern_data;

struct owl_move_data {
  int pos;          /* move coordinate */
  int value;        /* value */
  const char *name; /* name of the pattern suggesting the move */
  /* whether the move extends the dragon or not */
  enum same_dragon_value same_dragon;
  int lunch;	    /* Position of a lunch, if applicable.*/
  int escape;       /* true if an escape pattern is matched */
  int defense_pos;  /* defense coordinate for vital owl attack patterns. */
  int cuts[MAX_CUTS]; /* strings of the goal that might get cut off */
  /* pointer to pattern data, used for SAME_DRAGON_ALL_CONNECTED */
  struct matched_pattern_data *pattern_data;
};

#define USE_BDIST 1

struct matched_pattern_data {
  int move;
  int value;
  int ll;
  int anchor;
#if USE_BDIST
  int bdist;
#endif
  struct pattern *pattern;

  /* To link combinable patterns in chains. */
  int next_pattern_index;
};
  
struct matched_patterns_list_data {
  int initialized;
  int counter; 		/* Number of patterns in the list. */
  int used;		/* How many patterns have already been used?*/
  int list_size;	
  struct matched_pattern_data *pattern_list;
  int first_pattern_index[BOARDMAX];

  int heap_num_patterns;
  struct matched_pattern_data **pattern_heap;
};

void dump_pattern_list(struct matched_patterns_list_data *list);


static int do_owl_attack(int str, int *move, int *wormid,
			 struct local_owl_data *owl, int escape);
static int do_owl_defend(int str, int *move, int *wormid,
			 struct local_owl_data *owl, int escape);
static void owl_shapes(struct matched_patterns_list_data *list,
                       struct owl_move_data moves[MAX_MOVES], int color,
		       struct local_owl_data *owl, struct pattern_db *type);
static void collect_owl_shapes_callbacks(int anchor, int color,
	  			         struct pattern *pattern_db,
				         int ll, void *data);

static void pattern_list_prepare(struct matched_patterns_list_data *list);
static void pattern_list_build_heap(struct matched_patterns_list_data *list);
static void pattern_list_pop_heap_once(struct matched_patterns_list_data *list);
static void pattern_list_sink_heap_top_element(struct matched_patterns_list_data
					       *list);

static int get_next_move_from_list(struct matched_patterns_list_data *list,
                                   int color, struct owl_move_data *moves,
				   int cutoff, struct local_owl_data *owl);
static void init_pattern_list(struct matched_patterns_list_data *list);
static void close_pattern_list(int color,
			       struct matched_patterns_list_data *list);
static void owl_shapes_callback(int anchor, int color,
				struct pattern *pattern_db,
				int ll, void *data);
static void owl_add_move(struct owl_move_data *moves, int move, int value,
			 const char *reason,
			 enum same_dragon_value same_dragon, int lunch,
			 int escape, int defense_pos, int max_moves,
			 struct matched_pattern_data *pattern_data);
static void owl_determine_life(struct local_owl_data *owl,
			       struct local_owl_data *second_owl,
			       int does_attack,
			       struct owl_move_data *moves,
			       struct eyevalue *probable_eyes,
			       int *eyemin, int *eyemax,
			       int eyefilling_points[BOARDMAX]);
static void owl_find_relevant_eyespaces(struct local_owl_data *owl,
					int mw[BOARDMAX], int mz[BOARDMAX]);
static int owl_estimate_life(struct local_owl_data *owl,
			     struct local_owl_data *second_owl,
    		  	     struct owl_move_data vital_moves[MAX_MOVES],
		  	     const char **live_reason,
			     int does_attack,
		  	     struct eyevalue *probable_eyes,
			     int *eyemin, int *eyemax,
			     int eyefilling_points[BOARDMAX]);
static int modify_stupid_eye_vital_point(struct local_owl_data *owl,
					 int *vital_point,
					 int is_attack_point);
static int modify_eyefilling_move(int *move, int color);
static int estimate_lunch_half_eye_bonus(int lunch,
			struct half_eye_data half_eye[BOARDMAX]);
static void owl_mark_dragon(int apos, int bpos,
			    struct local_owl_data *owl,
			    int new_dragons[BOARDMAX]);
static void owl_mark_worm(int apos, int bpos,
			  struct local_owl_data *owl);
static void owl_mark_boundary(struct local_owl_data *owl);
static void owl_update_goal(int pos, enum same_dragon_value same_dragon,
			    int lunch, struct local_owl_data *owl,
			    int semeai_call,
			    struct matched_pattern_data *pattern_data);
static void owl_test_cuts(signed char goal[BOARDMAX], int color,
		          int cuts[MAX_CUTS]);
static void componentdump(const signed char component[BOARDMAX]);
static void owl_update_boundary_marks(int pos, struct local_owl_data *owl);
static void owl_find_lunches(struct local_owl_data *owl);
static int test_lunch_essentiality(int lunch, signed char *goal, int color,
				   int *num_stones, int *stones);
static int test_unstable_lunch_essentiality(int lunch, int dcode, int apos,
					    int dpos, int color);
static int improve_lunch_attack(int lunch, int attack_point);
static int improve_lunch_defense(int lunch, int defense_point);
static void owl_make_domains(struct local_owl_data *owla,
			     struct local_owl_data *owlb);
static int owl_safe_move(int move, int color);
static void sniff_lunch(int lunch, int *min, int *probable, int *max,
			struct local_owl_data *owl);
static void eat_lunch_escape_bonus(int lunch, int *min, int *probable,
				   int *max, struct local_owl_data *owl);
static int select_new_goal_origin(int origin, struct local_owl_data *owl);
static void compute_owl_escape_values(struct local_owl_data *owl);
static int owl_escape_route(struct local_owl_data *owl);
static void do_owl_analyze_semeai(int apos, int bpos, 
				  struct local_owl_data *owla,
				  struct local_owl_data *owlb,
				  int *resulta, int *resultb,
				  int *move, int pass, int owl_phase);
static int semeai_trymove_and_recurse(int apos, int bpos,
				      struct local_owl_data *owla,
				      struct local_owl_data *owlb,
				      int owl_phase,
				      int move, int color, int ko_allowed,
				      int move_value, const char *move_name,
				      enum same_dragon_value same_dragon,
				      struct matched_pattern_data *pattern_data,
				      int lunch, int pass, int *semeai_move,
				      int *this_resulta, int *this_resultb);
static void semeai_add_sgf_comment(int value, int owl_phase);
static int semeai_trust_tactical_attack(int str);
static void semeai_review_owl_moves(struct owl_move_data owl_moves[MAX_MOVES],
				    struct local_owl_data *owla,
				    struct local_owl_data *owlb, int color,
				    int *safe_outside_liberty_found,
				    int *safe_common_liberty_found,
				    int *riskless_move_found,
				    signed char mw[BOARDMAX],
				    struct owl_move_data semeai_moves[MAX_SEMEAI_MOVES],
				    int guess_same_dragon, int value_bonus,
				    int *critical_semeai_worms);
static int semeai_move_value(int move, struct local_owl_data *owla,
			     struct local_owl_data *owlb, int raw_value,
			     int *critical_semeai_worms);
static int semeai_is_riskless_move(int move, struct local_owl_data *owla);
static void remove_eye_filling_moves(struct local_owl_data *our_owl,
				     struct owl_move_data *moves);
static int find_semeai_backfilling_move(int worm, int liberty);
static int liberty_of_goal(int pos, struct local_owl_data *owl);
static int second_liberty_of_goal(int pos, struct local_owl_data *owl);
static int matches_found;
static signed char found_matches[BOARDMAX];

static void reduced_init_owl(struct local_owl_data **owl,
    			     int at_bottom_of_stack);
static void init_owl(struct local_owl_data **owl, int target1, int target2,
		     int move, int use_stack, int new_dragons[BOARDMAX]);

static struct local_owl_data *owl_stack[2 * MAXSTACK];
static int owl_stack_size = 0;
static int owl_stack_pointer = 0;
static void check_owl_stack_size(void);
static void push_owl(struct local_owl_data **owl);
static void do_push_owl(struct local_owl_data **owl);
static void pop_owl(struct local_owl_data **owl);

#if 0
static int catalog_goal(struct local_owl_data *owl,
    			int goal_worm[MAX_GOAL_WORMS]);
#endif

static int list_goal_worms(struct local_owl_data *owl,
    			   int goal_worm[MAX_GOAL_WORMS]);

/* FIXME: taken from move_reasons.h */
#define MAX_DRAGONS       2 * MAX_BOARD * MAX_BOARD / 3

static int dragon_goal_worms[MAX_DRAGONS][MAX_GOAL_WORMS];

static void
prepare_goal_list(int str, struct local_owl_data *owl,
		  int list[MAX_GOAL_WORMS], int *flag, int *kworm,
		  int do_list);
static void
finish_goal_list(int *flag, int *wpos, int list[MAX_GOAL_WORMS], int index);


/* Semeai worms are worms whose capture wins the semeai. */

#define MAX_SEMEAI_WORMS 20
static int s_worms = 0;
static int semeai_worms[MAX_SEMEAI_WORMS];
static int important_semeai_worms[MAX_SEMEAI_WORMS];

/* Whether one color prefers to get a ko over a seki. */
static int prefer_ko;

/* Usually it's a bad idea to include the opponent worms involved in
 * the semeai in the eyespace. For some purposes (determining a
 * definite lack of eyespace, finding certain vital moves), however,
 * we want to do that anyway. Then set this variable to 1 before
 * calling owl_estimate_life() and reset it afterwards.
 *
 * FIXME: We should implement a nicer mechanism to propagate this
 *        information to owl_lively(), where it's used.
 */
static int include_semeai_worms_in_eyespace = 0;



static void
clear_cut_list(int cuts[MAX_CUTS])
{
  int i;
  for (i = 0; i < MAX_CUTS; i++)
    cuts[i] = NO_MOVE;
}



/* Called when (apos) and (bpos) point to adjacent dragons
 * of the opposite color, both with matcher_status DEAD or
 * CRITICAL, analyzes the semeai, assuming that the player
 * of the (apos) dragon moves first. The results returned
 * by *resulta and *resultb are the results of the defense 
 * of the apos dragon and the attack of the bpos dragon,
 * respectively. Thus if these results are 1 and 0,
 * respectively, the usual meaning is that a move by the
 * apos player produces seki.
 *
 * owl determines whether owl moves are being generated
 * or simple liberty filling is taking place.
 *
 */

void
owl_analyze_semeai(int apos, int bpos, int *resulta, int *resultb,
		   int *semeai_move, int owl, int *semeai_result_certain)
{
  owl_analyze_semeai_after_move(PASS_MOVE, EMPTY, apos, bpos, resulta, resultb,
				semeai_move, owl, semeai_result_certain, 0);
}

/* Same as the function above with the addition that an arbitrary move
 * may be made before the analysis is performed.
 */
void
owl_analyze_semeai_after_move(int move, int color, int apos, int bpos,
			      int *resulta, int *resultb, int *semeai_move, 
			      int owl, int *semeai_result_certain,
			      int recompute_dragons)
{
  signed char ms[BOARDMAX];
  int w1, w2;
  int str;
  SGFTree *save_sgf_dumptree = sgf_dumptree;
  int save_verbose = verbose;
  int dummy_resulta;
  int dummy_resultb;
  int dummy_semeai_move;
  double start = 0.0;
  int reading_nodes_when_called = get_reading_node_counter();
  int nodes_used;
  int new_dragons[BOARDMAX];
  
  struct local_owl_data *owla;
  struct local_owl_data *owlb;
  Hash_data goal_hash;
  
  if (!resulta)
    resulta = &dummy_resulta;
  if (!resultb)
    resultb = &dummy_resultb;
  if (!semeai_move)
    semeai_move = &dummy_semeai_move;

  if (debug & DEBUG_OWL_PERFORMANCE)
    start = gg_cputime();

  if (recompute_dragons) {
    if (tryko(move, color, "Recompute dragons for semeai.")) {
      compute_new_dragons(new_dragons);
      popgo();
    }
    else
      recompute_dragons = 0;
  }
  
  
  /* Look for owl substantial worms of either dragon adjoining
   * the other dragon. Capturing such a worm wins the semeai.
   * These are the semeai_worms. This code must come before
   * the owl_init() calls because the owl_substantial
   *
   * FIXME: The sentence above is unfinished.
   */
  s_worms = 0;
  memset(ms, 0, sizeof(ms));
  for (w1 = first_worm_in_dragon(apos);
       w1 != NO_MOVE;
       w1 = next_worm_in_dragon(w1)) {
    for (w2 = first_worm_in_dragon(bpos);
	 w2 != NO_MOVE;
	 w2 = next_worm_in_dragon(w2)) {
      if (adjacent_strings(w1, w2) || have_common_lib(w1, w2, NULL)) {
	mark_string(w1, ms, 1);
	mark_string(w2, ms, 1);
      }
    }
  }


  
  sgf_dumptree = NULL;
  if (verbose > 0)
    verbose--;
  for (str = BOARDMIN; str < BOARDMAX; str++) 
    if (ON_BOARD(str) && ms[str] && worm[str].origin == str) {
      int adj;
      int adjs[MAXCHAIN];
      int k;
      int adjacent_to_outside = 0;

      /* Is the string adjacent to a living dragon outside the semeai?
       * In that case it's important to attack/defend it for the life
       * of the opponent.
       *
       * FIXME: Checking crude_status here isn't quite appropriate but
       * owl_status is not always computed and status itself is unsafe
       * since it might change before later calls to this code, e.g.
       * when checking for blunders.
       *
       * Not checking for aliveness at all gives problems in e.g.
       * ld_owl:302 where S19 is a separate dragon and R19 should not
       * be considered critically important. What we really would like
       * to determine is whether it's outside the semeai, however.
       */
      adj = chainlinks(str, adjs);
      for (k = 0; k < adj; k++) {
	if (!is_same_dragon(adjs[k], apos)
	    && !is_same_dragon(adjs[k], bpos)
	    && dragon[adjs[k]].crude_status == ALIVE)
	  adjacent_to_outside = 1;
      }
      
      if ((adjacent_to_outside || countstones(str) > 6)
	  && s_worms < MAX_SEMEAI_WORMS) {
	important_semeai_worms[s_worms] = 1;
	semeai_worms[s_worms++] = str;
	DEBUG(DEBUG_SEMEAI, "important semeai worm: %1m\n", str);
      }
      else if (owl_substantial(str) && s_worms < MAX_SEMEAI_WORMS) {
	important_semeai_worms[s_worms] = 0;
	semeai_worms[s_worms++] = str;
	DEBUG(DEBUG_SEMEAI, "semeai worm: %1m\n", str);
      }
    }
  verbose = save_verbose;
  sgf_dumptree = save_sgf_dumptree;

  ASSERT1(board[apos] == OTHER_COLOR(board[bpos]), apos);
  count_variations = 1;
  if (move == PASS_MOVE)
    DEBUG(DEBUG_SEMEAI, "owl_analyze_semeai: %1m vs. %1m\n", apos, bpos);
  else
    DEBUG(DEBUG_SEMEAI, "owl_analyze_semeai_after_move %C %1m: %1m vs. %1m\n",
	  color, move, apos, bpos);
  
  if (owl) {
    if (recompute_dragons) {
      init_owl(&owla, apos, NO_MOVE, NO_MOVE, 1, new_dragons);
      init_owl(&owlb, bpos, NO_MOVE, NO_MOVE, 0, new_dragons);
    }
    else {
      init_owl(&owla, apos, NO_MOVE, NO_MOVE, 1, NULL);
      init_owl(&owlb, bpos, NO_MOVE, NO_MOVE, 0, NULL);
    }
    owl_make_domains(owla, owlb);
  }
  else {
    reduced_init_owl(&owla, 1);
    reduced_init_owl(&owlb, 0);
    local_owl_node_counter = 0;
    owl_mark_worm(apos, NO_MOVE, owla);
    owl_mark_worm(bpos, NO_MOVE, owlb);
  }

  result_certain = 1;

  {
    Hash_data temp = goal_to_hashvalue(owla->goal);
    goal_hash = goal_to_hashvalue(owlb->goal);
    hashdata_xor(goal_hash, temp);
  }
  if (owl
      && search_persistent_semeai_cache(ANALYZE_SEMEAI,
					apos, bpos, move, color, &goal_hash,
					resulta, resultb, semeai_move,
					semeai_result_certain)) {
    if (move == PASS_MOVE) {
      DEBUG(DEBUG_OWL_PERFORMANCE,
	    "analyze_semeai %1m vs. %1m, result %d %d %1m (cached)\n",
	    apos, bpos, *resulta, *resultb, *semeai_move);
    }
    else {
      DEBUG(DEBUG_OWL_PERFORMANCE,
	    "analyze_semeai_after_move %C %1m: %1m vs. %1m, result %d %d %1m (cached)\n",
	    color, move, apos, bpos, *resulta, *resultb, *semeai_move);
    }
    return;
  }

  /* In some semeai situations one or both players have the option to
   * choose between seki and ko for the life and death of both. In
   * general this choice depends on the ko threat situation, the
   * overall score, and the strategical effects on surrounding
   * dragons, but we don't try to correctly estimate this. Instead we
   * make the reasonable assumption that if one dragon is
   * substantially smaller than the other dragon, ko is to be
   * preferred for the smaller dragon and seki for the larger dragon.
   *
   * prefer_ko can be either WHITE, BLACK, or EMPTY and tells which
   * color, if any, prefers to get ko.
   */
  if (dragon[apos].size <= 5 && dragon[bpos].size > 3 * dragon[apos].size)
    prefer_ko = board[apos];
  else if (dragon[bpos].size <= 5 && dragon[apos].size > 3 * dragon[bpos].size)
    prefer_ko = board[bpos];
  else
    prefer_ko = EMPTY;
  
  if (move == PASS_MOVE)
    do_owl_analyze_semeai(apos, bpos, owla, owlb,
			  resulta, resultb, semeai_move, 0, owl);
  else {
    semeai_trymove_and_recurse(bpos, apos, owlb, owla, owl,
			       move, color, 1, 0, "mandatory move",
			       SAME_DRAGON_MAYBE_CONNECTED, NULL, NO_MOVE, 0,
			       semeai_move, resultb, resulta);
    *resulta = REVERSE_RESULT(*resulta);
    *resultb = REVERSE_RESULT(*resultb);
  }

  nodes_used = get_reading_node_counter() - reading_nodes_when_called;
  if (move == PASS_MOVE) {
    DEBUG(DEBUG_OWL_PERFORMANCE,
	  "analyze_semeai %1m vs. %1m, result %d %d %1m (%d, %d nodes, %f seconds)\n",
	  apos, bpos, *resulta, *resultb, *semeai_move, local_owl_node_counter,
	  nodes_used, gg_cputime() - start);
  }
  else {
    DEBUG(DEBUG_OWL_PERFORMANCE,
	  "analyze_semeai_after_move %C %1m: %1m vs. %1m, result %d %d %1m (%d, %d nodes, %f seconds)\n",
	  color, move, apos, bpos, *resulta, *resultb, *semeai_move,
	  local_owl_node_counter,
	  nodes_used, gg_cputime() - start);
  }
  
  if (semeai_result_certain)
    *semeai_result_certain = result_certain;

  if (owl)
    store_persistent_semeai_cache(ANALYZE_SEMEAI, apos, bpos, move, color,
				  &goal_hash,
				  *resulta, *resultb, *semeai_move,
				  result_certain, nodes_used,
				  owla->goal, owlb->goal);
}



/* It is assumed that the 'a' player moves first, and
 * determines the best result for both players. The
 * parameter "pass" is 1 if the opponent's last move is
 * pass. In this case, if no move is found but the genus
 * is less than 2, then the position is declared seki.
 *
 * If a move is needed to get this result, then (*move) is
 * the location, otherwise this field returns PASS.
 */

static void
do_owl_analyze_semeai(int apos, int bpos, 
		      struct local_owl_data *owla,
		      struct local_owl_data *owlb,
		      int *resulta, int *resultb,
		      int *move, int pass, int owl_phase)
{
  int color = board[apos];
  int other = OTHER_COLOR(color);
#if 0
  int wormsa, wormsb;
  int goal_wormsa[MAX_GOAL_WORMS], goal_wormsb[MAX_GOAL_WORMS];
#endif
  struct owl_move_data vital_defensive_moves[MAX_MOVES];
  struct owl_move_data vital_offensive_moves[MAX_MOVES];
  struct owl_move_data shape_defensive_moves[MAX_MOVES];
  struct owl_move_data shape_offensive_moves[MAX_MOVES];
  struct matched_patterns_list_data shape_offensive_patterns;
  struct matched_patterns_list_data shape_defensive_patterns;
  struct owl_move_data moves[MAX_SEMEAI_MOVES];
  struct owl_move_data outside_liberty;
  struct owl_move_data eyefilling_liberty;
  struct owl_move_data common_liberty;
  struct owl_move_data backfill_outside_liberty;
  struct owl_move_data backfill_common_liberty;
  int safe_outside_liberty_found = 0;
  int eyefilling_liberty_found = 0;
  int safe_common_liberty_found = 0;
  int riskless_move_found = 0;
  signed char mw[BOARDMAX];  
  int k;
  SGFTree *save_sgf_dumptree = sgf_dumptree;
  int save_count_variations = count_variations;
  int move_value;
  int best_resulta = 0;
  int best_resultb = 0;
  int best_move = 0;
  const char *best_move_name = NULL;
  int this_resulta = -1;
  int this_resultb = -1;
  int xpos;
  int value1;
  int value2;
  int this_variation_number = count_variations - 1;
  int you_look_alive = 0;
  int I_look_alive = 0;
  int dummy_move;
  int tested_moves;
  int critical_semeai_worms[MAX_SEMEAI_WORMS];
  int sworm;
  int we_might_be_inessential;
  struct eyevalue probable_eyes_a;
  struct eyevalue probable_eyes_b;
  struct eyevalue dummy_eyes;
  int I_have_more_eyes;
  int eyefilling_points[BOARDMAX];
  
  SETUP_TRACE_INFO2("do_owl_analyze_semeai", apos, bpos);

  if (!move)
    move = &dummy_move;
  
  ASSERT1(board[apos] == owla->color, apos);
  ASSERT1(board[bpos] == owlb->color, bpos);

  apos = find_origin(apos);
  bpos = find_origin(bpos);

  if (stackp <= semeai_branch_depth
      && owl_phase
      && tt_get(&ttable, SEMEAI, apos, bpos, depth - stackp, NULL,
		&value1, &value2, &xpos) == 2) {
    TRACE_CACHED_RESULT2(value1, value2, xpos);
    *move = xpos;
      
    *resulta = value1;
    *resultb = value2;

    TRACE("%oVariation %d: %1m %1m %s %s %1m (cached) ",
	  this_variation_number, apos, bpos,
	  result_to_string(*resulta),
	  result_to_string(*resultb),
	  *move);
    SGFTRACE_SEMEAI(xpos, *resulta, *resultb, "cached");
    return;
  }

  global_owl_node_counter++;
  local_owl_node_counter++;

  shape_offensive_patterns.initialized = 0;
  shape_defensive_patterns.initialized = 0;
  
#if 0
  wormsa = catalog_goal(owla, goal_wormsa);
  wormsb = catalog_goal(owlb, goal_wormsb);
#endif
  
  outside_liberty.pos = NO_MOVE;
  eyefilling_liberty.pos = NO_MOVE;
  common_liberty.pos = NO_MOVE;
  backfill_outside_liberty.pos = NO_MOVE;
  backfill_common_liberty.pos = NO_MOVE;
  for (k = 0; k < MAX_SEMEAI_MOVES; k++) {
    moves[k].pos = 0;
    moves[k].value = -1;
    moves[k].name = NULL;
    moves[k].same_dragon = SAME_DRAGON_CONNECTED;
    moves[k].lunch = NO_MOVE;
    clear_cut_list(moves[k].cuts);
  }
  ASSERT1(other == board[bpos], bpos);
  memset(mw, 0, sizeof(mw));

  /* Turn off the sgf file and variation counting. */
  sgf_dumptree = NULL;
  count_variations = 0;
  
  /* Look for a tactical attack. We seek a semeai worm of owlb which
   * can be attacked. If such exists and is considered critical, we
   * declare victory. If it's not considered critical we add the
   * attacking move as a high priority move to try.
   */

  {
    int upos;
    
    for (sworm = 0; sworm < s_worms; sworm++) {
      critical_semeai_worms[sworm] = 0;
      if (board[semeai_worms[sworm]] == other) {
	int acode = attack(semeai_worms[sworm], &upos);
	if (acode == WIN
	    && semeai_trust_tactical_attack(semeai_worms[sworm])
	    && important_semeai_worms[sworm]) {
	  *resulta = WIN;
	  *resultb = WIN;
	  *move = upos;
	  sgf_dumptree = save_sgf_dumptree;
	  count_variations = save_count_variations;
	  SGFTRACE_SEMEAI(upos, WIN, WIN, "tactical win found");
	  READ_RETURN_SEMEAI(SEMEAI, apos, bpos, depth - stackp,
			     move, upos, WIN, WIN);
	}
	else if (acode != 0
		 && find_defense(semeai_worms[sworm], NULL)) {
	  critical_semeai_worms[sworm] = 1;
	  owl_add_move(moves, upos, 105, "attack semeai worm",
		       SAME_DRAGON_MAYBE_CONNECTED,
		       NO_MOVE, 0, NO_MOVE, MAX_SEMEAI_MOVES, NULL);
	  TRACE("Added %1m %d (-1)\n", upos, 105);
	}
	else if (acode == WIN
		 && important_semeai_worms[sworm]) {
	  owl_add_move(moves, upos, 100, "attack semeai worm",
		       SAME_DRAGON_MAYBE_CONNECTED,
		       NO_MOVE, 0, NO_MOVE, MAX_SEMEAI_MOVES, NULL);
	  TRACE("Added %1m %d (-1)\n", upos, 100);
	}
      }
    }
    /* Look for a tactical rescue. If a semeai worm of owla is tactically
     * threatened, try to save it.
     */

    we_might_be_inessential = 1;
    for (sworm = 0; sworm < s_worms; sworm++)
      if (board[semeai_worms[sworm]] == color) {
	if (important_semeai_worms[sworm])
	  we_might_be_inessential = 0;
	
	if (attack(semeai_worms[sworm], NULL)
	    && find_defense(semeai_worms[sworm], &upos)) {
	  critical_semeai_worms[sworm] = 1;
	  owl_add_move(moves, upos, 85, "defend semeai worm",
		       SAME_DRAGON_MAYBE_CONNECTED, NO_MOVE,
	      	       0, NO_MOVE, MAX_SEMEAI_MOVES, NULL);
	  TRACE("Added %1m %d (0)\n", upos, 85);
	}
	else if (countlib(semeai_worms[sworm]) == 1) {
	  /* Overrule tactical reading if it looks like we are
	   * capturing a big nakade to get out of atari.
	   */
	  int lib;
	  int j;
	  findlib(semeai_worms[sworm], 1, &lib);
	  for (j = 0; j < 4; j++) {
	    int pos = lib + delta[j];
	    if (board[pos] == other
		&& countlib(pos) == 1
		&& countstones(pos) >= 4) {
	      critical_semeai_worms[sworm] = 1;
	      owl_add_move(moves, lib, 85, "defend semeai worm",
			   SAME_DRAGON_MAYBE_CONNECTED, NO_MOVE,
			   0, NO_MOVE, MAX_SEMEAI_MOVES, NULL);
	      TRACE("Added %1m %d (0)\n", lib, 85);
	    }
	  }
	}
      }
  }

  /* We generate the candidate moves. During the early stages of
   * the semeai, there may be moves to expand or shrink the
   * eyespaces of the two dragons. During the later stages, the
   * picture is simplified and reading the semeai is a matter 
   * of filling liberties until one of the dragons may be removed,
   * or a seki results. The first stage we call the owl phase.
   */
  if (!owl_phase) {
    set_eyevalue(&probable_eyes_a, 0, 0, 0, 0);
    set_eyevalue(&probable_eyes_b, 0, 0, 0, 0);
    I_have_more_eyes = 0;
  }
  else {
    /* First the vital moves. These include moves to attack or
     * defend the eyespace (e.g. nakade, or hane to reduce the
     * number of eyes) or moves to capture a lunch. 
     */
    int eyemin_a;
    int eyemin_b;
    int eyemax_a;
    int eyemax_b;
    const char *live_reasona;
    const char *live_reasonb;
    
    /* We do not wish for any string of the 'b' dragon to be 
     * counted as a lunch of the 'a' dragon since owl_determine_life 
     * can give a wrong result in the case of a semeai. So we eliminate 
     * such lunches.
     */
    
    owl_find_lunches(owla);
    owl_find_lunches(owlb);
    for (k = 0; k < MAX_LUNCHES; k++) {
      if (owla->lunch[k] != NO_MOVE 
	  && owlb->goal[owla->lunch[k]]) {
	owla->lunch[k] = NO_MOVE;
      }
    }
#if 1
    for (k = 0; k < MAX_LUNCHES; k++) {
      if (owlb->lunch[k] != NO_MOVE 
	  && owla->goal[owlb->lunch[k]]) {
	owlb->lunch[k] = NO_MOVE;
      }
    }
#endif

    if (owl_estimate_life(owla, owlb, vital_defensive_moves,
			  &live_reasona, 0, &probable_eyes_a,
			  &eyemin_a, &eyemax_a, NULL))
      I_look_alive = 1;
    else if (stackp > 2 && owl_escape_route(owla) >= 5) {
      live_reasona = "escaped";
      I_look_alive = 1;
    }

    memset(eyefilling_points, 0, sizeof(eyefilling_points));
    if (owl_estimate_life(owlb, owla, vital_offensive_moves,
			  &live_reasonb, 1, &probable_eyes_b,
			  &eyemin_b, &eyemax_b, eyefilling_points))
      you_look_alive = 1;
    else if (stackp > 2 && owl_escape_route(owlb) >= 5) {
      live_reasonb = "escaped";
      you_look_alive = 1;
    }
    
    if (verbose) {
      gprintf("probable_eyes_a: %s eyemin: %d eyemax: %d",
	      eyevalue_to_string(&probable_eyes_a), eyemin_a, eyemax_a);
      if (I_look_alive)
	gprintf("%o I look alive (%s)", live_reasona);
      gprintf("%o\n");
      gprintf("probable_eyes_b: %s eyemin: %d eyemax: %d",
	      eyevalue_to_string(&probable_eyes_b), eyemin_b, eyemax_b);
      if (you_look_alive)
	gprintf("%o you look alive(%s)", live_reasonb);
      gprintf("%o\n");
    }

    /* Stop here if both look certain to live. */
    if (I_look_alive && you_look_alive) {
      *resulta = WIN;
      *resultb = 0;
      *move = PASS_MOVE;
      sgf_dumptree = save_sgf_dumptree;
      count_variations = save_count_variations;
      TRACE("Both live\n");
      SGFTRACE_SEMEAI(PASS_MOVE, WIN, 0, "Both live");
      READ_RETURN_SEMEAI(SEMEAI, apos, bpos, depth - stackp,
			 move, PASS_MOVE, WIN, 0);
    }
    
    /* Next the shape moves. */
    if (!I_look_alive) {
      owl_shapes(&shape_defensive_patterns, shape_defensive_moves, color,
		 owla, &owl_defendpat_db);
      for (k = 0; k < MAX_MOVES-1; k++)
	if (!get_next_move_from_list(&shape_defensive_patterns, color,
				     shape_defensive_moves, 1, owla))
	  break;
    }
    else
      shape_defensive_moves[0].pos = NO_MOVE;

    if (!you_look_alive) {
      owl_shapes(&shape_offensive_patterns, shape_offensive_moves, color,
		 owlb, &owl_attackpat_db);
      for (k = 0; k < MAX_MOVES-1; k++)
	if (!get_next_move_from_list(&shape_offensive_patterns, color,
				     shape_offensive_moves, 1, owlb))
	  break;
    }
    else
      shape_offensive_moves[0].pos = NO_MOVE;

    /* Filter out moves, which fill our eye (and not split it). */
    if (eyemax_a > 0) {
      remove_eye_filling_moves(owla, vital_defensive_moves);
      remove_eye_filling_moves(owla, vital_offensive_moves);
      remove_eye_filling_moves(owla, shape_defensive_moves);
      remove_eye_filling_moves(owla, shape_offensive_moves);
    }

    /* Now we review the moves already considered, while collecting
     * them into a single list. 
     */

    if (!I_look_alive) {
      semeai_review_owl_moves(vital_defensive_moves, owla, owlb, color,
			      &safe_outside_liberty_found,
			      &safe_common_liberty_found,
			      &riskless_move_found,
			      mw, moves, 0, 30,
			      critical_semeai_worms);
      
      semeai_review_owl_moves(shape_defensive_moves, owla, owlb, color,
			      &safe_outside_liberty_found,
			      &safe_common_liberty_found,
			      &riskless_move_found,
			      mw, moves, 0, 0,
			      critical_semeai_worms);
    }

    if (!you_look_alive) {
      semeai_review_owl_moves(vital_offensive_moves, owla, owlb, color,
			      &safe_outside_liberty_found,
			      &safe_common_liberty_found,
			      &riskless_move_found,
			      mw, moves, 1, 30,
			      critical_semeai_worms);
      
      semeai_review_owl_moves(shape_offensive_moves, owla, owlb, color,
			      &safe_outside_liberty_found,
			      &safe_common_liberty_found,
			      &riskless_move_found,
			      mw, moves, 1, 0,
			      critical_semeai_worms);
    }

    /* If no moves were found so far, also check the eyespaces when
     * opponent semeai worms are allowed to be included for vital
     * moves.
     */
    if (moves[0].pos == NO_MOVE || we_might_be_inessential) {
      include_semeai_worms_in_eyespace = 1;
      if (!owl_estimate_life(owlb, owla, vital_offensive_moves,
			     &live_reasonb, 1, &dummy_eyes,
			     &eyemin_b, &eyemax_b, NULL))
	semeai_review_owl_moves(vital_offensive_moves, owla, owlb, color,
				&safe_outside_liberty_found,
				&safe_common_liberty_found,
				&riskless_move_found,
				mw, moves, 1, 30,
				critical_semeai_worms);
      include_semeai_worms_in_eyespace = 0;
    }

    if (eyemin_a == eyemax_a)
      /* We have stable number of eyes, so we can try to reduce
       * opponent eyes.
       */
      I_have_more_eyes = (eyemin_a > min_eyes(&probable_eyes_b));
    else {
      if (min_eyes(&probable_eyes_a) == max_eyes(&probable_eyes_a))
        /* If we can't increase our number of eyes, we try to reduce
	 * opponent eyes.
	 */
        I_have_more_eyes = (max_eyes(&probable_eyes_a) > min_eyes(&probable_eyes_b));
      else
        /* If we can increase our number of eyes, we do it and let
	 * opponent to increase his.
	 */
        I_have_more_eyes = (max_eyes(&probable_eyes_a) > max_eyes(&probable_eyes_b));
    }

    if (get_level() < 8) {
      /* If no owl moves were found on two consecutive moves,
       * turn off the owl phase.
       */
      if (moves[0].pos == NO_MOVE) {
	if (owl_phase == 1)
	  owl_phase = 2;
	else if (owl_phase == 2)
	  owl_phase = 0;
      }
      else
	owl_phase = 1;
    }
  }

  if (1 && verbose) {
    showboard(0);
    goaldump(owla->goal);
    goaldump(owlb->goal);
  }
  
  /* Now we look for a move to fill a liberty. This is only
   * interesting if the opponent doesn't already have two eyes.
   * If we have more eyes, always check for a backfilling move.
   */
  if ((!you_look_alive || we_might_be_inessential)
      && !safe_outside_liberty_found
      && (moves[0].value < 110 || I_have_more_eyes)) {
    int pos;
    for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
      if (!ON_BOARD(pos))
	continue;
      
      if (board[pos] == EMPTY && !mw[pos]) {
	if (liberty_of_goal(pos, owlb)) {
	  int origin = owlb->my_eye[pos].origin;
	  if (!liberty_of_goal(pos, owla)) {
	    if (!(owlb->my_eye[origin].color == owlb->color
		  && max_eyes(&owlb->my_eye[origin].value) > 0)) {
	      /* outside liberty */
	      if (safe_move(pos, color) == WIN) {
		safe_outside_liberty_found = 1;
		outside_liberty.pos = pos;
		break;
	      }
	      else if (backfill_outside_liberty.pos == NO_MOVE)
		backfill_outside_liberty.pos = find_semeai_backfilling_move(bpos,
									    pos);
	    }
	    else {
	      /* eye-filling liberty */
	      if (eyefilling_points[pos]) {
		eyefilling_liberty_found = 1;
		eyefilling_liberty.pos = pos;
	      }
	    }
	  }
	  else {
	    /* common liberty */
	    if (safe_move(pos, color) == WIN
		|| (!we_might_be_inessential
		    && accuratelib(pos, color, 3, NULL) >= 3)) {
	      safe_common_liberty_found = 1;
	      common_liberty.pos = pos;
	    }
	    else if (backfill_common_liberty.pos == NO_MOVE)
	      backfill_common_liberty.pos = find_semeai_backfilling_move(bpos,
									 pos);
	  }
	}
      }
    }
  }

  /* Add the best liberty filling move available. We first want to
   * play outer liberties, second backfilling moves required before
   * filling an outer liberty. If no such moves are available we try
   * to locate an eyefilling move. After that we try to fill a mutual
   * liberty or play a corresponding backfilling move.
   */
  if (!you_look_alive || we_might_be_inessential) {
    if (safe_outside_liberty_found
	&& outside_liberty.pos != NO_MOVE) {
      move_value = semeai_move_value(outside_liberty.pos,
				     owla, owlb, 50,
				     critical_semeai_worms);
      owl_add_move(moves, outside_liberty.pos, move_value,
		   "safe outside liberty", SAME_DRAGON_NOT_CONNECTED,
		   NO_MOVE, 0, NO_MOVE, MAX_SEMEAI_MOVES, NULL);
      riskless_move_found = 1;
      TRACE("Added %1m %d (5)\n", outside_liberty.pos, move_value);
    }
    else if (backfill_outside_liberty.pos != NO_MOVE) {
      move_value = semeai_move_value(backfill_outside_liberty.pos,
				     owla, owlb, 50,
				     critical_semeai_worms);
      owl_add_move(moves, backfill_outside_liberty.pos, move_value,
		   "backfilling move for outer liberty", SAME_DRAGON_NOT_CONNECTED, NO_MOVE, 0,
		   NO_MOVE, MAX_SEMEAI_MOVES, NULL);
      riskless_move_found = 1;
      TRACE("Added %1m %d (6)\n", backfill_outside_liberty.pos, move_value);
    }
    else if (eyefilling_liberty_found
	     && eyefilling_liberty.pos != NO_MOVE) {
      move_value = semeai_move_value(eyefilling_liberty.pos,
				     owla, owlb, 30,
				     critical_semeai_worms);
      owl_add_move(moves, eyefilling_liberty.pos, move_value,
		   "safe eyefilling liberty", SAME_DRAGON_NOT_CONNECTED,
		   NO_MOVE, 0, NO_MOVE, MAX_SEMEAI_MOVES, NULL);
      riskless_move_found = 1;
      TRACE("Added %1m %d (5)\n", eyefilling_liberty.pos, move_value);
    }
    else if (!(pass & color) && safe_common_liberty_found
	     && common_liberty.pos != NO_MOVE) {
      move_value = semeai_move_value(common_liberty.pos,
				     owla, owlb, 10,
				     critical_semeai_worms);
      owl_add_move(moves, common_liberty.pos, move_value,
		   "safe common liberty", SAME_DRAGON_MAYBE_CONNECTED,
		   NO_MOVE, 0, NO_MOVE, MAX_SEMEAI_MOVES, NULL);
      if (semeai_is_riskless_move(common_liberty.pos, owla))
	riskless_move_found = 1;
      TRACE("Added %1m %d (7)\n", common_liberty.pos, move_value);
    }
    else if (!(pass & color) && backfill_common_liberty.pos != NO_MOVE) {
      move_value = semeai_move_value(backfill_common_liberty.pos,
				     owla, owlb, 10,
				     critical_semeai_worms);
      owl_add_move(moves, backfill_common_liberty.pos, move_value,
		   "backfilling move for common liberty", SAME_DRAGON_NOT_CONNECTED, NO_MOVE, 0,
		   NO_MOVE, MAX_SEMEAI_MOVES, NULL);
      /* Playing a backfilling move for common liberties inside own
       * eyespace is not risk free, regardless of the tactical
       * analysis.
       */
      if (semeai_is_riskless_move(backfill_common_liberty.pos, owla)
	  && owla->my_eye[backfill_common_liberty.pos].color != owla->color)
	riskless_move_found = 1;
      TRACE("Added %1m %d (6)\n", backfill_common_liberty.pos, move_value);
    }
  }

  if (moves[0].pos == NO_MOVE)
    TRACE("No move found\n");

  /* Now we are ready to try moves. Turn on the sgf output ... */
  sgf_dumptree = save_sgf_dumptree;
  count_variations = save_count_variations;
  tested_moves = 0;
  for (k = 0; k < MAX_SEMEAI_MOVES; k++) {
    int mpos = moves[k].pos;
    if (mpos == NO_MOVE)
      break;

    if (moves[k].value == 0)
      continue;

    /* Do not try too many moves. */
    if (tested_moves > 2
	|| (stackp > semeai_branch_depth2 && tested_moves > 1)
	|| (stackp > semeai_branch_depth && tested_moves > 0)) {
      /* If allpats, try and pop to get the move in the sgf record. */
      if (!allpats)
	break;
      else if (trymove(mpos, color, moves[k].name, apos)) {
	semeai_add_sgf_comment(moves[k].value, owl_phase);
	popgo();
      }
      continue;
    }
    
    if (count_variations >= semeai_node_limit
	|| stackp >= MAX_SEMEAI_DEPTH)
      continue;

    /* Try playing the move at mpos and call ourselves recursively to
     * determine the result obtained by this move.
     */
    if (semeai_trymove_and_recurse(apos, bpos, owla, owlb,
				   owl_phase, mpos, color,
				   best_resulta == 0 || best_resultb == 0,
				   moves[k].value, moves[k].name,
				   moves[k].same_dragon, moves[k].pattern_data,
				   moves[k].lunch, pass, NULL,
				   &this_resulta, &this_resultb)) {
      tested_moves++;
      if (this_resultb == WIN && this_resulta == WIN) {
	/* Ideal result, no need to try any more moves. */
	*resulta = WIN;
	*resultb = WIN;
	*move = mpos;
	TRACE("After %1m I (%C) am alive, you are dead\n", mpos, color);
	SGFTRACE_SEMEAI(mpos, WIN, WIN, moves[k].name);
	close_pattern_list(color, &shape_defensive_patterns);
	close_pattern_list(color, &shape_offensive_patterns);
	READ_RETURN_SEMEAI(SEMEAI, apos, bpos, depth - stackp,
			   move, mpos, WIN, WIN);
      }
      /* When there is a choice between ko and seki, the prefer_ko
       * variable decides policy. Thus if prefer_ko == color we
       * consider attacking the opponent more important than defending
       * our dragon, and vise versa otherwise.
       */
      else if ((prefer_ko != color
		&& (this_resulta > best_resulta
		    || (this_resulta == best_resulta
			&& this_resultb > best_resultb)))
	       || (prefer_ko == color
		   && (this_resultb > best_resultb
		       || (this_resultb == best_resultb
			   && this_resulta > best_resulta)))) {
	best_resulta = this_resulta;
	best_resultb = this_resultb;
	best_move = mpos;
	best_move_name = moves[k].name;
      }
    }
  }

  close_pattern_list(color, &shape_defensive_patterns);
  close_pattern_list(color, &shape_offensive_patterns);

  /* If we can't find a move and the opponent looks alive, we have
   * lost.
   */
  if (best_resulta == 0 && best_resultb == 0 && you_look_alive) {
    *resulta = 0;
    *resultb = 0;
    *move = PASS_MOVE;
    SGFTRACE_SEMEAI(PASS_MOVE, 0, 0, "You live, I die");
    READ_RETURN_SEMEAI(SEMEAI, apos, bpos, depth - stackp,
		       move, PASS_MOVE, 0, 0);
  }

  /* If we didn't find a working move and we look dead even if including the
   * opponent stones in our eyespace, we have lost.
   */
  if (best_resulta == 0 && best_resultb == 0
      && !riskless_move_found) {
    const char *live_reasona;
    int eyemin_a;
    int eyemax_a;
    for (sworm = 0; sworm < s_worms; sworm++) {
      if (board[semeai_worms[sworm]] == other) {
	if (important_semeai_worms[sworm])
	  break;
      }
    }
    
    if (sworm == s_worms) {
      include_semeai_worms_in_eyespace = 1;
      if (!owl_estimate_life(owla, owlb, vital_defensive_moves,
			     &live_reasona, 0, &dummy_eyes,
			     &eyemin_a, &eyemax_a, NULL)
	  && eyemax_a < 2) {
	include_semeai_worms_in_eyespace = 0;
	*resulta = 0;
	*resultb = 0;
	*move = PASS_MOVE;
	SGFTRACE_SEMEAI(PASS_MOVE, 0, 0, "You live, I die - 2");
	READ_RETURN_SEMEAI(SEMEAI, apos, bpos, depth - stackp,
			   move, PASS_MOVE, 0, 0);
      }
      include_semeai_worms_in_eyespace = 0;
    }
  }

  /* If we can't find a useful move and opponent passed, it's seki, unless
   * one dragon has more eyes than the other.
   */
  if (best_resulta == 0 && best_resultb == 0
      && !riskless_move_found) {
    if (pass & OTHER_COLOR(color)) {
      if (max_eyes(&probable_eyes_a) < min_eyes(&probable_eyes_b)) {
	*resulta = 0;
	*resultb = 0;
	*move = PASS_MOVE;
	TRACE("You have more eyes.\n");
	SGFTRACE_SEMEAI(PASS_MOVE, 0, 0, "You have more eyes");
	READ_RETURN_SEMEAI(SEMEAI, apos, bpos, depth - stackp,
			   move, PASS_MOVE, 0, 0);
      }
      else if (max_eyes(&probable_eyes_b) < min_eyes(&probable_eyes_a)) {
	*resulta = WIN;
	*resultb = WIN;
	*move = PASS_MOVE;
	TRACE("I have more eyes\n");
	SGFTRACE_SEMEAI(PASS_MOVE, WIN, WIN, "I have more eyes");
	READ_RETURN_SEMEAI(SEMEAI, apos, bpos, depth - stackp,
			   move, PASS_MOVE, WIN, WIN);
      }
      else {
	*resulta = WIN;
	*resultb = 0;
	*move = PASS_MOVE;
	TRACE("Seki\n");
	SGFTRACE_SEMEAI(PASS_MOVE, WIN, 0, "Seki");
	READ_RETURN_SEMEAI(SEMEAI, apos, bpos, depth - stackp,
			   move, PASS_MOVE, WIN, 0);
      }
    }
    else {
    /* No working move was found, but opponent hasn't passed. Then we pass. */
      do_owl_analyze_semeai(bpos, apos, owlb, owla,
			    resultb, resulta, NULL, pass | color, owl_phase);
      *resulta = REVERSE_RESULT(*resulta);
      *resultb = REVERSE_RESULT(*resultb);
      TRACE("No move found\n");
      SGFTRACE_SEMEAI(PASS_MOVE, *resulta, *resultb, "No move found");
      *move = PASS_MOVE;
      READ_RETURN_SEMEAI(SEMEAI, apos, bpos, depth - stackp,
			 move, PASS_MOVE, *resulta, *resultb);
    }
  }

  /* There are a few selected cases where we should try to see if it
   * would be better to pass rather than playing any move in the semeai.
   *
   * A first simple example is the case of positions where there is nothing
   * left to play but common liberties. In case the above analysis concluded
   * the result is seki and if the best (and only) move happens to be a
   * common liberty, we attempt to pass, so that the engine considers tenuki
   * as a viable option in case it actually is.
   *
   * Another example is related to "disturbing" kos.
   * 
   * .OOOOOOOO.  In this position (similar to semeai:130), X has just taken
   * OOXXXXXXOO  the ko on the left. The semeai code finds the ko recapture
   * OX.XXOOXXO  as the only attacking move and concludes the result is KO_B.
   * OOXX.OO.XO
   * ----------
   *
   * In such cases too, we try to pass to see if it doesn't actually yield
   * a better result.
   *
   * FIXME: there might be more cases where passing would be valuable. 
   */
  if (!pass && k == 1) {
    if ((best_resulta == WIN && best_resultb == 0
	 && best_move != NO_MOVE
	 && (best_move == common_liberty.pos || best_move == eyefilling_liberty.pos)
	 && stackp == 0)
	|| (best_resulta == KO_B && best_resultb == KO_B
	    && is_ko(best_move, owla->color, NULL))) {
      do_owl_analyze_semeai(bpos, apos, owlb, owla, &this_resultb,
			    &this_resulta, NULL, pass | color, owl_phase);
      if (REVERSE_RESULT(this_resulta) >= best_resulta
	  && REVERSE_RESULT(this_resultb) >= best_resultb) {
	best_move = PASS_MOVE;
	best_resulta = REVERSE_RESULT(this_resulta);
	best_resultb = REVERSE_RESULT(this_resultb);
	best_move_name = "Pass";
      }
    }
  }

  *resulta = best_resulta;
  *resultb = best_resultb;
  if (best_resulta == 0)
    best_move = PASS_MOVE;
  *move = best_move;
  SGFTRACE_SEMEAI(best_move, best_resulta, best_resultb, best_move_name);
  READ_RETURN_SEMEAI(SEMEAI, apos, bpos, depth - stackp, 
		     move, best_move, best_resulta, best_resultb);
}

/* Play a move, update goal and boundaries appropriately, and call
 * do_owl_analyze_semeai() recursively to determine the result of this
 * move.
 */
static int
semeai_trymove_and_recurse(int apos, int bpos, struct local_owl_data *owla,
			   struct local_owl_data *owlb,
			   int owl_phase,
			   int move, int color, int ko_allowed,
			   int move_value, const char *move_name,
			   enum same_dragon_value same_dragon,
			   struct matched_pattern_data *pattern_data,
			   int lunch, int pass, int *semeai_move,
			   int *this_resulta, int *this_resultb)
{
  int ko_move = 0;
  
  gg_assert(this_resulta != NULL && this_resultb != NULL);
  *this_resulta = 0;
  *this_resultb = 0;

  if (!komaster_trymove(move, color, move_name, apos, &ko_move, ko_allowed)) {
    int kpos;
    if (is_ko(move, color, &kpos)) {
      /* Move was not allowed because of komaster. We want to check
       * if this situation is double ko and when it is, we won semeai.
       */
      int libs[MAX_LIBERTIES];
      int n;
      int nlib;
      int sworm;
      int worm_color;
      int other = OTHER_COLOR(color);

      for (sworm = 0; sworm < s_worms; sworm++) {
	worm_color = board[semeai_worms[sworm]];
	if (worm_color == color) {
	  /* We only check up to MAX_LIBERTIES, due to performance
	   * reasons. When we have more liberties we have some outside
	   * liberties to fill and these moves will be tried later
	   * (and double ko situation will be found).
	   */
	  nlib = findlib(semeai_worms[sworm], MAX_LIBERTIES, libs);
	  if (nlib > MAX_LIBERTIES)
	    return 0;

	  for (n = 0; n < nlib; n++)
	    if (is_ko(libs[n], other, NULL)) {
	      /* Check if situation is not a nested ko capture. */
	      if (DIAGONAL_NEIGHBORS(libs[n], kpos))
	        return 0;

	      /* Our dragon has double ko, but we have to check if
	       * opponent dragon doesn't have outside liberties or
	       * double ko.
	       */
	      *this_resulta = WIN;
	      *this_resultb = WIN;
	    }
	}
	else if (worm_color == other) {
	  if (countlib(semeai_worms[sworm]) > 2)
	    /* In double ko situation the opponent can have only a
	     * single eye and a ko outside liberty to be sure that we
	     * will always win double ko.
	     */
	    return 0;
	}
      }
      if (*this_resulta == WIN)
	return 1;
    }

    return 0;
  }
  
  semeai_add_sgf_comment(move_value, owl_phase);
  TRACE("Trying %C %1m. Current stack: ", color, move);
  if (verbose) {
    dump_stack();
    goaldump(owla->goal);
    gprintf("\n");
    goaldump(owlb->goal);
    gprintf("\n");
  }
  TRACE("%s, value %d, same_dragon %d\n", move_name, move_value, same_dragon);
    
  push_owl(&owla);
  push_owl(&owlb);

  if (owla->color == color) {
    owl_update_goal(move, same_dragon, lunch, owla, 1, pattern_data);
    owl_update_boundary_marks(move, owlb);
  }
  else {
    owl_update_goal(move, same_dragon, lunch, owlb, 1, pattern_data);
    owl_update_boundary_marks(move, owla);
  }
  mark_goal_in_sgf(owla->goal);
  mark_goal_in_sgf(owlb->goal);
    
  /* Do a recursive call to read the semeai after the move we just
   * tried. If dragon b was captured by the move, call
   * do_owl_attack() to see whether it sufficed for us to live.
   */
  if (board[bpos] == EMPTY) {
    /* FIXME: Are all owl_data fields and relevant static
     * variables properly set up for a call to do_owl_attack()?
     */
    *this_resulta = REVERSE_RESULT(do_owl_attack(apos, semeai_move, NULL, owla, 0));
    *this_resultb = *this_resulta;
  }
  else {
    do_owl_analyze_semeai(bpos, apos, owlb, owla,
			  this_resultb, this_resulta, semeai_move,
			  pass, owl_phase);
    *this_resulta = REVERSE_RESULT(*this_resulta);
    *this_resultb = REVERSE_RESULT(*this_resultb);
  }
    
  pop_owl(&owlb);
  pop_owl(&owla);
    
  popgo();
    
  /* Does success require ko? */
  if (ko_move) {
    if (*this_resulta != 0)
      *this_resulta = KO_B;
    if (*this_resultb != 0)
      *this_resultb = KO_B;
  }
    
  if (count_variations >= semeai_node_limit) {
    TRACE("Out of nodes, claiming win.\n");
    result_certain = 0;
    *this_resulta = WIN;
    *this_resultb = WIN;
  }
  return 1;
}

/* Add details in sgf file about move value and whether owl_phase is active. */
static void
semeai_add_sgf_comment(int value, int owl_phase)
{
  char buf[100];

  if (!sgf_dumptree)
    return;
  
  if (owl_phase)
    gg_snprintf(buf, 100, "value %d, owl_phase", value);
  else
    gg_snprintf(buf, 100, "value %d", value);
  sgftreeAddComment(sgf_dumptree, buf);
}


/* In semeai situations tactical attacks often cannot be trusted. This
 * in particular holds for strings with three or more liberties. Two
 * liberties can usually be trusted, but if neither liberty can be
 * played immediately, the need for backfilling moves gives an
 * effective liberty count of more than two, again making the attack
 * untrustworthy.
 *
 * This function decides whether an attack should be trusted. It does
 * not check whether there actually is an attack, though.
 */
static int
semeai_trust_tactical_attack(int str)
{
  int liberties;
  int libs[3];
  int other = OTHER_COLOR(board[str]);
  
  liberties = findlib(str, 3, libs);
  if (liberties > 2)
    return 0;

  if (liberties < 2)
    return 1;

  if (!is_self_atari(libs[0], other)
      || !is_self_atari(libs[1], other))
    return 1;

  return 0;
}


/* A move is deemed riskless (i.e., doesn't kill ourself in a seki situation)
 * if it doesn't decrease the liberty count of any goal string of our
 * dragon.
 */
static int
semeai_is_riskless_move(int move, struct local_owl_data *owla)
{
  int k;
  int liberties = accuratelib(move, owla->color, MAXLIBS, NULL);
  if (!liberty_of_goal(move, owla))
    return 1;
  for (k = 0; k < 4; k++) {
    int pos = move + delta[k];
    if (board[pos] == owla->color
	&& owla->goal[pos]
	&& countlib(pos) > liberties)
      return 0;
  }
  return 1;
}


/* Review the moves in owl_moves[] and add them into semeai_moves[].
 * This is used to merge multiple sets of owl moves into one move
 * list, while revising the values for use in semeai reading.
 *
 * We also record whether the moves include an outer or common liberty
 * in the semeai.
 */
static void
semeai_review_owl_moves(struct owl_move_data owl_moves[MAX_MOVES],
			struct local_owl_data *owla,
			struct local_owl_data *owlb, int color,
			int *safe_outside_liberty_found,
			int *safe_common_liberty_found,
			int *riskless_move_found,
			signed char mw[BOARDMAX],
			struct owl_move_data semeai_moves[MAX_SEMEAI_MOVES],
			int guess_same_dragon, int value_bonus,
			int *critical_semeai_worms)
{
  int move;
  int move_value;
  enum same_dragon_value same_dragon;
  struct matched_pattern_data *pattern_data = NULL;
  int k;
  
  for (k = 0; k < MAX_MOVES-1; k++) {
    move = owl_moves[k].pos;
    if (move == NO_MOVE)
      break;

    if (owl_moves[k].value == 0)
      continue;

    /* Does the move fill a liberty in the semeai? */
    if (liberty_of_goal(move, owlb)
	&& safe_move(move, color)) {
      if (!liberty_of_goal(move, owla))
	*safe_outside_liberty_found = 1;
      else
	*safe_common_liberty_found = 1;
    }
    if (is_legal(move, color) && !is_ko(move, color, NULL)
	&& semeai_is_riskless_move(move, owla))
      *riskless_move_found = 1;

    /* For some types of owl moves we don't have same_dragon
     * information recorded and need to guess.
     */
    if (guess_same_dragon) {
      if (liberty_of_goal(move, owla)
	  || second_liberty_of_goal(move, owla))
	same_dragon = SAME_DRAGON_MAYBE_CONNECTED;
      else
	same_dragon = SAME_DRAGON_NOT_CONNECTED;
    }
    else {
      same_dragon = owl_moves[k].same_dragon;
      pattern_data = owl_moves[k].pattern_data;
    }

    mw[move] = 1;
    move_value = (semeai_move_value(move, owla, owlb, owl_moves[k].value,
				    critical_semeai_worms)
		  + value_bonus);
    owl_add_move(semeai_moves, move, move_value, owl_moves[k].name, 
		 same_dragon, NO_MOVE, owl_moves[k].escape,
		 NO_MOVE, MAX_SEMEAI_MOVES, pattern_data);
    TRACE("Added %1m %d\n", move, move_value);
  }
}


/* Try to estimate the value of a semeai move. This has two
 * components. The first is the change in the total number of
 * liberties for strings involved in the semeai. The second is a bonus
 * for attacks and defenses of critical semeai worms.
 */

static int
semeai_move_value(int move, struct local_owl_data *owla,
		  struct local_owl_data *owlb,
		  int raw_value, int *critical_semeai_worms)
{
  int pos;
  int net = 0;
  int color = owla->color;
  int save_verbose = verbose;
  int k;
  int bonus = 0;

  ASSERT1(board[move] == EMPTY, move);
  verbose = 0;
  if (safe_move(move, color)) {
    for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
      if (IS_STONE(board[pos])
	  && pos == find_origin(pos)) {
	int count_lib = -1;
	if (owla->goal[pos]) {
	  count_lib = countlib(pos);
	  net -= 75 * count_lib;
	}
	if (owlb->goal[pos]) {
	  if (count_lib < 0)
	    count_lib = countlib(pos);
	  net += 100 * count_lib;
	}
      }
    }
    if (!trymove(move, color, NULL, 0)) {
      verbose = save_verbose;
      return 0;
    }
    for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
      if (IS_STONE(board[pos])
	  && pos == find_origin(pos)) {
	int count_lib = -1;
	if (owla->goal[pos]
	    || (pos == move && liberty_of_goal(move, owla))) {
	  count_lib = countlib(pos);
	  net += 75 * count_lib;
	}
	if (owlb->goal[pos]) {
	  if (count_lib < 0)
	    count_lib = countlib(pos);
	  net -= 100 * count_lib;
	}
      }
    }

    increase_depth_values();
    for (k = 0; k < s_worms; k++) {
      if (!critical_semeai_worms[k])
	continue;
      if (board[semeai_worms[k]] == color
	  && !attack(semeai_worms[k], NULL))
	bonus += 50;
      else if (board[semeai_worms[k]] == OTHER_COLOR(color)
	       && !find_defense(semeai_worms[k], NULL))
	bonus += 50;
    }
    decrease_depth_values();
    
    popgo();
  }

  verbose = save_verbose;

  if (net < 0)
    net = 0;

  net /= 25;
  net *= 3;
  
  return raw_value + net + bonus;
}


/* Remove all moves from the list that would fill our own eye. */
static void
remove_eye_filling_moves(struct local_owl_data *our_owl,
			 struct owl_move_data *moves)
{
  int k;
  int color = our_owl->color;

  for (k = 0; k < MAX_MOVES; k++) {
    if (moves[k].pos == NO_MOVE)
      break;
    else {
      struct eye_data *eye = &our_owl->my_eye[moves[k].pos];

      /* If esize==1 this eye must not be a real eye (at least one
       * worm is capturable, otherwise this move would not be
       * proposed).
       */
      if (eye->color == color && eye->msize == 0 && eye->neighbors <= 1
	  && eye->esize != 1
	  && our_owl->half_eye[moves[k].pos].type != HALF_EYE
	  && !has_neighbor(moves[k].pos, OTHER_COLOR(color)))
	moves[k].value = 0;
    }
  }
}

/* Is the vertex at pos adjacent to an element of the owl goal? */
static int
liberty_of_goal(int pos, struct local_owl_data *owl)
{
  int k;
  for (k = 0; k < 4; k++)
    if (IS_STONE(board[pos + delta[k]]) && owl->goal[pos + delta[k]])
      return 1;
  
  return 0;
}

/* Is the vertex at pos a second liberty of the owl goal? */
static int
second_liberty_of_goal(int pos, struct local_owl_data *owl)
{
  int k;
  for (k = 0; k < 4; k++)
    if (board[pos + delta[k]] == EMPTY && liberty_of_goal(pos + delta[k], owl))
      return 1;
  
  return 0;
}


/* 'liberty' is a liberty of 'worm' which we would like to fill.
 * However it is not safe to play there, so we look for a
 * backfilling move. For example in this situation:
 *
 *   ------+
 *   O.OaXc|
 *   OOOOOX|
 *   XXXXXb|
 *   ......|
 *
 * If 'worm' is the O string and 'liberty' is 'a', the
 * function returns 'b'. To fill at 'a', X must first
 * fill 'b' and 'c' and it is better to fill at 'b' first
 * since that will sometimes leave fewer or smaller ko threats.
 *
 * Returns NO_MOVE if no move is found.
 */

static int
find_semeai_backfilling_move(int worm, int liberty)
{
  int color = board[worm];
  int other = OTHER_COLOR(color);
  int result = NO_MOVE;

  if (safe_move(liberty, other) == WIN)
    return liberty;
  if (is_self_atari(liberty, other)) {
    int fill;
    if (approxlib(liberty, other, 1, &fill) > 0
	&& trymove(fill, other, "find_semeai_backfilling_move", worm)) {
      if (safe_move(liberty, other))
	result = fill;
      else if (board[worm] != EMPTY)
	result = find_semeai_backfilling_move(worm, liberty);
      popgo();
    }
  }
  if (ON_BOARD(result) && safe_move(result, other))
    return result;
  else
    return NO_MOVE;
}

/* Some helper function for do_owl_attack/defend. */

static int
reading_limit_reached(const char **live_reason, int this_variation_number)
{
  /* If (stackp > owl_reading_depth), interpret deep reading
   * conservatively as escape.
   */
  if (stackp > owl_reading_depth) {
    TRACE("%oVariation %d: ALIVE (maximum reading depth reached)\n",
	  this_variation_number);
    *live_reason = "max reading depth reached";
    return 1;
  }
  /* If the owl node limit has been reached, assume the dragon has
   * managed to escape.
   */
  if (local_owl_node_counter >= owl_node_limit) {
    result_certain = 0;
    TRACE("%oVariation %d: ALIVE (owl node limit reached)\n",
	  this_variation_number);
    *live_reason = "owl node limit reached";
    return 1;
  }
  return 0;
}

static void
clear_owl_move_data(struct owl_move_data moves[MAX_MOVES])
{
  int k;
  for (k = 0; k < MAX_MOVES; k++) {
    moves[k].pos = NO_MOVE;
    moves[k].value = -1;
    moves[k].name = NULL;
    moves[k].same_dragon = SAME_DRAGON_CONNECTED;
    moves[k].escape = 0;
    moves[k].lunch = NO_MOVE;
    moves[k].pattern_data = NULL;
    clear_cut_list(moves[k].cuts);
  }
}

static void
set_single_owl_move(struct owl_move_data moves[MAX_MOVES],
    		    int pos, const char *name)
{
  moves[0].pos          = pos;
  moves[0].value        = 25;
  moves[0].name         = name;
  moves[0].same_dragon  = SAME_DRAGON_MAYBE_CONNECTED;
  moves[0].escape       = 0;
  moves[0].lunch        = NO_MOVE;
  moves[0].pattern_data = NULL;
  clear_cut_list(moves[0].cuts);
  moves[1].value        = 0;
}


/* Returns true if a move can be found to attack the dragon
 * at (target), in which case (*attack_point) is the recommended move.
 * (attack_point) can be a null pointer if only the result is needed.
 *
 * The array goal marks the extent of the dragon. This must
 * be maintained during reading. Call this function only when
 * stackp==0; otherwise you can call do_owl_attack but you must
 * set up the goal and boundary arrays by hand first.
 *
 * Returns KO_A or KO_B if the position is ko:
 *
 * - Returns KO_A if the attack prevails provided attacker is willing to
 *   ignore any ko threat (the attacker makes the first ko capture).
 *
 * - Returns KO_B if attack succeeds provided attacker has a ko threat
 *   which must be answered (the defender makes the first ko capture).
 *
 * If GNU Go is compiled with `configure --enable-experimental-owl-ext'
 * then a return codes of GAIN is also possible.
 *
 * - Returns GAIN if the attack fails but another worm of the
 *   opponent's is captured in during the failed attack. The location
 *   of the killed worm is returned through the *kworm field.
 *
 * */

int
owl_attack(int target, int *attack_point, int *certain, int *kworm)
{
  int result;
  struct local_owl_data *owl;
  int reading_nodes_when_called = get_reading_node_counter();
  double start = 0.0;
  int tactical_nodes;
  int move = NO_MOVE;
  int wpos = NO_MOVE;
  int wid = MAX_GOAL_WORMS;

  result_certain = 1;
  if (worm[target].unconditional_status == DEAD) {
    if (attack_point)
      *attack_point = NO_MOVE;
    if (kworm)
      *kworm = NO_MOVE;
    if (certain)
      *certain = 1;
    return 1;
  }

  if (search_persistent_owl_cache(OWL_ATTACK, target, 0, 0, &result,
				  attack_point, kworm, certain))
    return result;

  if (debug & DEBUG_OWL_PERFORMANCE)
    start = gg_cputime();
  
  TRACE("owl_attack %1m\n", target);
  init_owl(&owl, target, NO_MOVE, NO_MOVE, 1, NULL);
  owl_make_domains(owl, NULL);
  prepare_goal_list(target, owl, owl_goal_worm, &goal_worms_computed,
		    kworm, 1);
  result = do_owl_attack(target, &move, &wid, owl, 0);
  finish_goal_list(&goal_worms_computed, &wpos, owl_goal_worm, wid);
  tactical_nodes = get_reading_node_counter() - reading_nodes_when_called;

  DEBUG(DEBUG_OWL_PERFORMANCE,
    "owl_attack %1m, result %d %1m (%d, %d nodes, %f seconds)\n",
    target, result, move, local_owl_node_counter,
    tactical_nodes, gg_cputime() - start);

  store_persistent_owl_cache(OWL_ATTACK, target, 0, 0,
			     result, move, wpos,
			     result_certain, tactical_nodes,
			     owl->goal, board[target]);
  if (attack_point)
    *attack_point = move;
  if (kworm)
    *kworm = wpos;
  if (certain)
    *certain = result_certain;

  return result;
}


/* Static function containing the main recursive code for 
 * owl_attack.
 */

static int
do_owl_attack(int str, int *move, int *wormid,
	      struct local_owl_data *owl, int escape)
{
  int color = board[str];
  int other = OTHER_COLOR(color);
  struct owl_move_data vital_moves[MAX_MOVES];
  struct owl_move_data shape_moves[MAX_MOVES];
  struct owl_move_data *moves;
  struct matched_patterns_list_data shape_patterns;
  signed char mw[BOARDMAX];
  int number_tried_moves = 0;
  int pass;
  int k;
  int savemove = 0;
  int saveworm = MAX_GOAL_WORMS;
  int savecode = 0;
  int eyemin = -1;               /* Lower bound on the number of eyes. */
  int eyemax = -1;               /* Upper bound on the number of eyes. */
  struct eyevalue probable_eyes; /* Best guess of eyevalue. */
  const char *live_reason;
  int move_cutoff;
  int xpos;
  int value1;
  int value2;
  int this_variation_number = count_variations - 1;
  
  SETUP_TRACE_INFO("owl_attack", str);

  shape_patterns.initialized = 0;

  str = find_origin(str);

  if (tt_get(&ttable, OWL_ATTACK, str, NO_MOVE, depth - stackp, NULL, 
	     &value1, &value2, &xpos) == 2) {

    TRACE_CACHED_RESULT(value1, xpos);
    if (move)
      *move = xpos;

    if (value1 == GAIN) {
      if (wormid) {
	if (goal_worms_computed)
	  *wormid = value2;
	else
	  *wormid = MAX_GOAL_WORMS;
      }
    }
    
    if (value1 == WIN)
      TRACE("%oVariation %d: DEAD (cached)\n", this_variation_number);
    else
      TRACE("%oVariation %d: ALIVE (cached)\n", this_variation_number);
    
    SGFTRACE(xpos, value1, "cached");
    
    return value1;
  }


  /* If reading goes to deep or we run out of nodes, we assume life. */
  if (reading_limit_reached(&live_reason, this_variation_number)) {
    SGFTRACE(0, 0, live_reason);
    READ_RETURN(OWL_ATTACK, str, depth - stackp, move, 0, 0);
  }

  memset(mw, 0, sizeof(mw));
  global_owl_node_counter++;
  local_owl_node_counter++;

  current_owl_data = owl;
  memset(owl->safe_move_cache, 0, sizeof(owl->safe_move_cache));

  /* First see whether there is any chance to kill. */
  if (owl_estimate_life(owl, NULL, vital_moves, &live_reason, 1,
			&probable_eyes, &eyemin, &eyemax, NULL)) {
    /*
     * We need to check here if there's a worm under atari. If yes,
     * locate it and report a (gote) GAIN.
     */
    int acode = 0;
    int mpos = NO_MOVE;
    if (experimental_owl_ext && goal_worms_computed) {
      int size = 0;
      saveworm = MAX_GOAL_WORMS;
      for (k = 0; k < MAX_GOAL_WORMS; k++) {
	if (owl_goal_worm[k] == NO_MOVE)
	  break;
	if (board[owl_goal_worm[k]] == EMPTY
	    || countlib(owl_goal_worm[k]) > 1)
	  continue;
	if (worm[owl_goal_worm[k]].size > size) {
	  saveworm = k;
	  size = worm[owl_goal_worm[k]].size;
	}
      }
      if (saveworm != MAX_GOAL_WORMS && size >= 3) {
	acode = GAIN;
	findlib(worm[owl_goal_worm[saveworm]].origin, 1, &mpos);
	/* ASSERT1( ... */
      }
    }
    SGFTRACE(0, acode, live_reason);
    TRACE("%oVariation %d: ALIVE (%s)\n", this_variation_number, live_reason);
    if (acode == 0) {
      READ_RETURN(OWL_ATTACK, str, depth - stackp, move, 0, 0);
    }
    else {
      if (wormid)
	*wormid = saveworm;
      READ_RETURN2(OWL_ATTACK, str, depth - stackp,
		   move, mpos, acode, saveworm);
    }
  }

  /* We try moves in five passes.
   *                                stackp==0   stackp>0
   * 0. Vital moves in the interval  [70..]      [45..]
   * 1. Shape moves
   * 2. Vital moves in the interval  [..69]      [..44]
   * 3. Tactical attack moves (except certain kos)
   * 4. Moves found by the defender
   * 5. Tactical ko attack moves which were not tried in pass 3
   */
  for (pass = 0; pass < 6; pass++) {
    moves = NULL;
    move_cutoff = 1;
    
    current_owl_data = owl;
    /* Get the shape moves if we are in the right pass. */
    switch (pass) {
    case 1:
      if (stackp > owl_branch_depth && number_tried_moves > 0)
	continue;
      
      owl_shapes(&shape_patterns, shape_moves, other, owl, &owl_attackpat_db);
      moves = shape_moves;
      break;

    case 0:
    case 2:
      if (stackp > owl_branch_depth && number_tried_moves > 0)
	continue;
      
      moves = vital_moves;
      if (pass == 0 || stackp > owl_distrust_depth) {
	if (stackp == 0)
	  move_cutoff = 70;
	else
	  move_cutoff = 45;
      }
      if (eyemax < 2 && stackp > 2)
	move_cutoff = 99; /* Effectively disable vital moves. */
      break;

    case 3:
    case 5:
      {
	/* Look for a tactical attack. This is primarily intended for
	 * the case where the whole dragon is a single string, therefore
	 * we only look at the string at the "origin".
	 *
	 * We must be wary with attacks giving ko. Unless the dragon
	 * otherwise looks alive, this may turn a dead dragon into one
	 * which can live by ko. Such moves will be tried anyway in
	 * pass 5. Notice though that we can only reach there if an owl
	 * defense was found in pass 4.
	 */
	int apos;
	int result;
	SGFTree *save_sgf_dumptree = sgf_dumptree;
	int save_count_variations = count_variations;

	sgf_dumptree = NULL;
	count_variations = 0;
	result = attack(str, &apos);
	if (result == WIN
	    || (result != 0 && (min_eyes(&probable_eyes) >= 2
				|| pass == 5))) {
	  set_single_owl_move(shape_moves, apos, "tactical attack");
	  moves = shape_moves;
	}
	sgf_dumptree = save_sgf_dumptree;
	count_variations = save_count_variations;
      }
      break;

    /* If we found no move in the first four passes we ask the defender
     * for a move suggestion.
     */
    case 4:
      if (number_tried_moves == 0) {
	int dpos;
	int dcode = do_owl_defend(str, &dpos, NULL, owl, escape);
	/* No defense, we won. */
	if (dcode == 0) {
	  TRACE("%oVariation %d: DEAD (no defense)\n",
		this_variation_number);
	  SGFTRACE(0, WIN, "no defense");
	  close_pattern_list(other, &shape_patterns);
	  READ_RETURN(OWL_ATTACK, str, depth - stackp, move, 0, WIN);
	}
	else if (dpos != NO_MOVE) {
	  /* The dragon could be defended by one more move. Try to
	   * attack with this move.
	   *
	   * If the move is suicide for us, try to find a backfilling
	   * move to play instead. Do this also if the move is a
	   * send-two-return-one sacrifice.
	   */
	  const char *name = "defense move";
	  SGFTree *save_sgf_dumptree = sgf_dumptree;
	  int save_count_variations = count_variations;

	  sgf_dumptree = NULL;
	  count_variations = 0;

	  if (is_suicide(dpos, other) || send_two_return_one(dpos, other)) {
	    int dpos2;
	    for (k = 0; k < 4; k++) {
	      if (board[dpos + delta[k]] == other
		  && find_defense(dpos + delta[k], &dpos2)) {
		dpos = dpos2;
		name = "defense move (backfill)";
		break;
	      }
	    }
	  }

	  sgf_dumptree = save_sgf_dumptree;
	  count_variations = save_count_variations;
	
	  if (dpos != NO_MOVE) {
	    set_single_owl_move(shape_moves, dpos, name);
	    moves = shape_moves;
	  }
	}
      }
      break;
    } /* switch (pass) */
      

    /* FIXME: This block probably should reappear somewhere in this
     * function.
     */
#if 0
    /* First test whether the dragon has escaped. */
    if (owl_escape_route(owl) >= 5) {
      /* FIXME: We probably should make distinction in the returned
       * result whether the dragon lives by making two eyes or by
       * escaping.
       */
      TRACE("%oVariation %d: ALIVE (escaped)\n", this_variation_number);
      SGFTRACE(0, 0, "escaped");
      close_pattern_list(other, &shape_patterns);
      READ_RETURN0(OWL_ATTACK, str, depth - stackp);
    }
#endif

    if (!moves)
      continue;
    
    /* For the up to MAX_MOVES best moves with value equal to
     * move_cutoff or higher, try to attack the dragon and see if it
     * can then be defended.
     */
    for (k = 0; k < MAX_MOVES; k++) {
      int mpos;
      int ko_move = -1;
      int origin = NO_MOVE;
      int captured;
      int wid = MAX_GOAL_WORMS;
      int dcode;

      /* Consider only the highest scoring move if we're deeper than
       * owl_branch_depth.
       *
       * FIXME: To behave as intended, k should be replaced by
       *        number_tried_moves.
       */
      if (stackp > owl_branch_depth && k > 0)
	break;

      current_owl_data = owl;

      /* Shape moves are selected on demand. */
      if (pass == 1) {
        if (!get_next_move_from_list(&shape_patterns, other,
	                             shape_moves, move_cutoff, owl))
          break;
      }
      else
	if (moves[k].value < move_cutoff)
	  break;

      mpos = moves[k].pos;
      ASSERT_ON_BOARD1(mpos);
    
      /* Have we already tested this move? */
      if (mw[mpos])
	continue;

      captured = (color == WHITE ? white_captured : black_captured);

      /* Try to make the move. */
      if (!komaster_trymove(mpos, other, moves[k].name, str,
			    &ko_move, savecode == 0))
	continue;

      captured = (color == WHITE ? white_captured : black_captured) - captured;

      TRACE("Trying %C %1m. Escape = %d. Current stack: ",
	    other, mpos, escape);
      if (verbose)
	dump_stack();

      /* We have now made a move. Analyze the new position. */
      push_owl(&owl);
      mw[mpos] = 1;
      number_tried_moves++;
      owl_update_boundary_marks(mpos, owl);
      
      /* If the origin of the dragon has been captured, we look
       * for another string which was part of the original dragon,
       * marked when stackp==0, which has not been captured. If no
       * such string is found, owl_attack declares victory.
       */
      if (IS_STONE(board[str]))
	origin = str;
      else
	origin = select_new_goal_origin(NO_MOVE, owl);

      /* Test whether the move cut the goal dragon apart. */
      if (moves[k].cuts[0] != NO_MOVE && origin != NO_MOVE) {
	owl_test_cuts(owl->goal, owl->color, moves[k].cuts);
	if (!owl->goal[origin])
	  origin = select_new_goal_origin(origin, owl);
      }
      mark_goal_in_sgf(owl->goal);

      if (origin == NO_MOVE)
	dcode = 0;
      else
	dcode = do_owl_defend(origin, NULL, &wid, owl, escape);

      if (!ko_move) {
	if (dcode == 0) {
	  pop_owl(&owl);
	  popgo();
  	  if (sgf_dumptree) {
	    const char *wintxt;
	    char winstr[192];
	    if (origin == NO_MOVE)
	      wintxt = "all original stones captured";
	    else
	      wintxt = "attack effective";
	    sprintf(winstr, "%s)\n  (%d variations", wintxt,
	  		    count_variations - this_variation_number);
	    SGFTRACE(mpos, WIN, winstr);
	  }
          close_pattern_list(other, &shape_patterns);
	  READ_RETURN(OWL_ATTACK, str, depth - stackp, move, mpos, WIN);
	}
	else if (experimental_owl_ext && dcode == LOSS) {
	  if (saveworm == MAX_GOAL_WORMS
	      || worm[owl_goal_worm[wid]].size
		 > worm[owl_goal_worm[saveworm]].size)
	    saveworm = wid;
	}
	/* The conditions here are set so that this code doesn't get
	 * triggered when the capture is immediate (the tactical
	 * reading code should take care of these).
	 */
	else if (experimental_owl_ext && goal_worms_computed
#if 0
		 && stackp > 1
#endif
		 && captured >= 3) {
	  int w = MAX_GOAL_WORMS;
	  int size = 0;
	  int l;
	  /* locate the biggest captured worm */
	  for (l = 0; l < MAX_GOAL_WORMS; l++) {
	    if (owl_goal_worm[l] == NO_MOVE)
	      break;
	    if (board[owl_goal_worm[l]] == EMPTY)
	      if (size == 0 || worm[owl_goal_worm[l]].size > size) {
		w = l;
		size = worm[owl_goal_worm[l]].size;
	      }
	  }
	  if (w != MAX_GOAL_WORMS) {
	    if (GAIN > savecode) {
  	      /* if new result better, just update */
	      dcode = LOSS;
	      saveworm = w;
	    }
	    else if (GAIN == savecode) {
	      /* bigger ? */
	      int wpos = owl_goal_worm[saveworm];
	      if (size > worm[wpos].size)
  		saveworm = w;
	    }
	  }
	}
	UPDATE_SAVED_KO_RESULT(savecode, savemove, dcode, mpos);
      }
      else { /* ko_move */
	if (dcode != WIN) {
	  if (mpos == 0) {
	    SGFTRACE(mpos, KO_B, "all original stones captured with ko");
	  }
	  else {
	    SGFTRACE(mpos, KO_B, "attack effective - ko");
	  }
	  /* We already know the savecode was previously 0. */
	  savemove = mpos;
	  savecode = KO_B;

	  /* It's possible that the defender has no defense even if we
           * give up the ko. In order to force a test of this,
           * assuming this was our only move, we decrease the number
           * of tried moves counter, disregarding this move.
	   */
	  number_tried_moves--;
	}
      }
    
      pop_owl(&owl);
      popgo();
    }
  }

  close_pattern_list(other, &shape_patterns);
  
  if (savecode) {
    if (savecode == GAIN) {
      SGFTRACE(savemove, savecode, "attack effective (gain) - E");
      if (wormid)
	*wormid = saveworm;
      READ_RETURN2(OWL_ATTACK, str, depth - stackp,
		   move, savemove, savecode, saveworm);
    }
    else {
      SGFTRACE(savemove, savecode, "attack effective (ko) - E");
      READ_RETURN(OWL_ATTACK, str, depth - stackp, move, savemove, savecode);
    }
  }

  if (sgf_dumptree) {
    char winstr[128];
    sprintf(winstr, "attack failed)\n  (%d variations",
	  	    count_variations - this_variation_number);
    SGFTRACE(0, 0, winstr);
  }
  
  READ_RETURN0(OWL_ATTACK, str, depth - stackp);
}


/* Returns true if the dragon at (target) can be captured given
 * two moves in a row. The first two moves to capture the
 * dragon are given as (*attack1) and (*attack2).
 */

int
owl_threaten_attack(int target, int *attack1, int *attack2)
{
  struct owl_move_data moves[MAX_MOVES];
  int k;
  int other = OTHER_COLOR(board[target]);
  struct local_owl_data *owl;
  int result = 0;
  int reading_nodes_when_called = get_reading_node_counter();
  signed char saved_boundary[BOARDMAX];
  double start = 0.0;
  int tactical_nodes;
  int move = 0;
  int move2 = 0;
  struct matched_patterns_list_data shape_patterns;

  shape_patterns.initialized = 0;
  result_certain = 1;
  if (search_persistent_owl_cache(OWL_THREATEN_ATTACK, target, 0, 0,
				  &result, attack1, attack2, NULL))
    return result;

  if (debug & DEBUG_OWL_PERFORMANCE)
    start = gg_cputime();
  
  gg_assert(stackp == 0);
  TRACE("owl_threaten_attack %1m\n", target);
  init_owl(&owl, target, NO_MOVE, NO_MOVE, 1, NULL);
  memcpy(saved_boundary, owl->boundary, sizeof(saved_boundary));
  owl_make_domains(owl, NULL);
  owl_shapes(&shape_patterns, moves, other, owl, &owl_attackpat_db);
  for (k = 0; k < MAX_MOVES; k++) {
    current_owl_data = owl;
    if (!get_next_move_from_list(&shape_patterns, other, moves, 1, owl))
      break;
    else {
      int mpos = moves[k].pos;

      if (mpos != NO_MOVE && moves[k].value > 0)
	if (trymove(mpos, other, moves[k].name, target)) {
	  int pos;
	  int origin = NO_MOVE;
	  owl->lunches_are_current = 0;
	  owl_update_boundary_marks(mpos, owl);
	  
	  /* If the origin of the dragon has been captured, we look
	   * for another string which was part of the original dragon,
	   * marked when stackp==0, which has not been captured. If no
	   * such string is found, owl_attack declares victory.
	   */
	  
	  if (board[target] == EMPTY) {
	    for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
	      if (IS_STONE(board[pos]) && owl->goal[pos] == 1) {
		origin = find_origin(pos);
		break;
	      }
	    }
	    
	    if (origin == NO_MOVE
		|| do_owl_attack(origin, NULL, NULL, owl, 0)) {
	      /* probably this can't happen */
	      popgo();
	      gg_assert(stackp == 0);
	      result = 1;
	      break;
	    }
	  }
	  else if (do_owl_attack(target, &move2, NULL, owl, 0) == WIN) {
	    move = moves[k].pos;
	    popgo();
	    gg_assert(stackp == 0);
	    result = 1;
	    break;
	  }
	  popgo();
	  memcpy(owl->boundary, saved_boundary, sizeof(saved_boundary));
	}
    }
  }
  tactical_nodes = get_reading_node_counter() - reading_nodes_when_called;
  gg_assert(stackp == 0);

  DEBUG(DEBUG_OWL_PERFORMANCE,
    "owl_threaten_attack %1m %1m %1m, result %d (%d, %d nodes, %f seconds)\n",
    target, move, move2, result, local_owl_node_counter,
    tactical_nodes, gg_cputime() - start);

  store_persistent_owl_cache(OWL_THREATEN_ATTACK, target, 0, 0,
			     result, move, move2, 0,
			     tactical_nodes, owl->goal, board[target]);

  if (attack1)
    *attack1 = move;
  if (attack2)
    *attack2 = move2;

  close_pattern_list(other, &shape_patterns);
  return result;
}


/* Returns true if a move can be found to defend the dragon
 * at (target), in which case (*defense_point) is the recommended move.
 * (defense_point) can be a null pointer if the result is not needed.
 *
 * The array goal marks the extent of the dragon. This must
 * be maintained during reading. Call this function only when
 * stackp==0; otherwise you can call do_owl_attack but you must
 * set up the goal and boundary arrays by hand first.
 *
 * Returns KO_A or KO_B if the position is ko:
 *
 * - Returns KO_A if the defendse succeeds provided the defender is willing to
 *   ignore any ko threat (the defender makes the first ko capture).
 * - Returns KO_B if the defense succeeds provided the defender has a ko threat
 *   which must be answered (the attacker makes the first ko capture).
 *
 * If GNU Go is compiled with `configure --enable-experimental-owl-ext'
 * then a return codes of GAIN is also possible.
 *
 * - Returns LOSS if the defense succeeds but another worm of the
 *   defender's is captured in during the defense. The location
 *   of the killed worm is returned through the *kworm field.
 *
 * The array goal marks the extent of the dragon. This must
 * be maintained during reading.  
 */

int
owl_defend(int target, int *defense_point, int *certain, int *kworm)
{
  int result;
  static struct local_owl_data *owl;
  int reading_nodes_when_called = get_reading_node_counter();
  double start = 0.0;
  int tactical_nodes;
  int move = NO_MOVE;
  int wpos = NO_MOVE;
  int wid = MAX_GOAL_WORMS;

  result_certain = 1;
  if (worm[target].unconditional_status == DEAD)
    return 0;

  if (search_persistent_owl_cache(OWL_DEFEND, target, 0, 0, &result, 
				  defense_point, kworm, certain))
    return result;

  if (debug & DEBUG_OWL_PERFORMANCE)
    start = gg_cputime();

  TRACE("owl_defend %1m\n", target);
  init_owl(&owl, target, NO_MOVE, NO_MOVE, 1, NULL);
  owl_make_domains(owl, NULL);
  prepare_goal_list(target, owl, owl_goal_worm, &goal_worms_computed,
		    kworm, 1);
  result = do_owl_defend(target, &move, &wid, owl, 0);
  finish_goal_list(&goal_worms_computed, &wpos, owl_goal_worm, wid);
  tactical_nodes = get_reading_node_counter() - reading_nodes_when_called;

  DEBUG(DEBUG_OWL_PERFORMANCE,
    "owl_defend %1m, result %d %1m (%d, %d nodes, %f seconds)\n",
	    target, result, move, local_owl_node_counter,
	    tactical_nodes, gg_cputime() - start);

  store_persistent_owl_cache(OWL_DEFEND, target, 0, 0, result, move, wpos,
			     result_certain, tactical_nodes, owl->goal,
			     board[target]);

  if (defense_point)
    *defense_point = move;
  if (kworm)
    *kworm = wpos;
  if (certain)
    *certain = result_certain;
  
  return result;
}


/* Static function containing the main recursive code for owl_defend.
 */

static int
do_owl_defend(int str, int *move, int *wormid, struct local_owl_data *owl,
	      int escape)
{
  int color = board[str];
  struct owl_move_data shape_moves[MAX_MOVES];
  struct owl_move_data vital_moves[MAX_MOVES];
  struct owl_move_data *moves;
  struct matched_patterns_list_data shape_patterns;
  signed char mw[BOARDMAX];
  int number_tried_moves = 0;
  int pass;
  int k;
  int savemove = 0;
  int saveworm = MAX_GOAL_WORMS;
  int savecode = 0;
  int eyemin = -1;               /* Lower bound on the number of eyes. */
  int eyemax = -1;               /* Upper bound on the number of eyes. */
  struct eyevalue probable_eyes; /* Best guess of eyevalue. */
  int escape_route;
  const char *live_reason;
  int move_cutoff;
  int xpos;
  int value1;
  int value2;
  int this_variation_number = count_variations - 1;

  SETUP_TRACE_INFO("owl_defend", str);

  shape_patterns.initialized = 0;
  
  str = find_origin(str);

  if (tt_get(&ttable, OWL_DEFEND, str, NO_MOVE, depth - stackp, NULL, 
	     &value1, &value2, &xpos) == 2) {
    
    TRACE_CACHED_RESULT(value1, xpos);
    if (move)
      *move = xpos;

    if (value1 == LOSS) {
      if (wormid) {
	if (goal_worms_computed)
	  *wormid = value2;
	else
	  *wormid = MAX_GOAL_WORMS;
      }
    }

    if (value1 == WIN || value1 == LOSS)
      TRACE("%oVariation %d: ALIVE (cached)\n", this_variation_number);
    else
      TRACE("%oVariation %d: DEAD (cached)\n", this_variation_number);

    SGFTRACE(xpos, value1, "cached");

    return value1;
  }

  /* In order to get a defense move even if we seem to already have
   * escaped and to reduce the impact of overestimated escape
   * possibilities, we don't declare escape victory on the first move.
   *
   * FIXME: Should introduce a new owl depth value rather than having
   *        this hardwired value.
   */
  escape_route = owl_escape_route(owl);
  if (stackp > 2 && escape_route >= 5) {
    /* FIXME: We probably should make distinction in the returned
     * result whether the dragon lives by making two eyes or by
     * escaping.
     */
    TRACE("%oVariation %d: ALIVE (escaped)\n", this_variation_number);
    SGFTRACE(0, WIN, "escaped");
    READ_RETURN(OWL_DEFEND, str, depth - stackp, move, 0, WIN);
  }

  /* If reading goes to deep or we run out of nodes, we assume life. */
  if (reading_limit_reached(&live_reason, this_variation_number)) {
    SGFTRACE(0, WIN, live_reason);
    READ_RETURN(OWL_DEFEND, str, depth - stackp, move, 0, WIN);
  }

  memset(mw, 0, sizeof(mw));
  local_owl_node_counter++;
  global_owl_node_counter++;

  current_owl_data = owl;
  memset(owl->safe_move_cache, 0, sizeof(owl->safe_move_cache));

  /* First see whether we might already be alive. */
  if (escape < MAX_ESCAPE) {
    if (owl_estimate_life(owl, NULL, vital_moves, &live_reason, 0,
			  &probable_eyes, &eyemin, &eyemax, NULL)) {
      SGFTRACE(0, WIN, live_reason);
      TRACE("%oVariation %d: ALIVE (%s)\n",
	    this_variation_number, live_reason);
      READ_RETURN(OWL_DEFEND, str, depth - stackp, move, 0, WIN);
    }
  }
  else {
    /* In this case we don't recompute eyes. However, to avoid accessing
     * partially-random data left on stack, we copy eye data from the
     * previous depth level. It should be reasonably close to the actual
     * state of eyes.
     */
    memcpy(owl->my_eye, owl->restore_from->my_eye, sizeof(owl->my_eye));
    memcpy(owl->half_eye, owl->restore_from->half_eye, sizeof(owl->half_eye));

    vital_moves[0].pos = 0;
    vital_moves[0].value = -1;
    set_eyevalue(&probable_eyes, 0, 0, 0, 0);
  }

  /* We try moves in four passes.
   *                                stackp==0   stackp>0
   * 0. Vital moves in the interval  [70..]      [45..]
   * 1. Shape moves
   * 2. Vital moves in the interval  [..69]      [..44]
   * 3. Tactical defense moves
   */
  for (pass = 0; pass < 4; pass++) {
    moves = NULL;
    move_cutoff = 1;
    
    current_owl_data = owl;
    switch (pass) {
    /* Get the shape moves if we are in the right pass. */
    case 1:
      
      if (stackp > owl_branch_depth && number_tried_moves > 0)
	continue;
      
      owl_shapes(&shape_patterns, shape_moves, color, owl, &owl_defendpat_db);
      moves = shape_moves;
      break;

    case 0:
    case 2:
      if (stackp > owl_branch_depth && number_tried_moves > 0)
	continue;
      
      moves = vital_moves;
      if (pass == 0 || stackp > owl_distrust_depth) {
	if (stackp == 0)
	  move_cutoff = 70;
	else if (eyemin + min_eyes(&probable_eyes) > 3)
	  move_cutoff = 25;
	else if (eyemin + min_eyes(&probable_eyes) >= 3)
	  move_cutoff = 35;
	else
	  move_cutoff = 45;
      }
      if (eyemax < 2 && stackp > 2)
	move_cutoff = 99; /* Effectively disable vital moves. */
      break;

    case 3:
      {
	int goalcount = 0;

	/* If the goal is small, try a tactical defense. */

	for (k = BOARDMIN; k < BOARDMAX; k++)
	  if (ON_BOARD(k))
	    goalcount += owl->goal[k];

	if (goalcount < 5) {

	  /* Look for a tactical defense. This is primarily intended for
	   * the case where the whole dragon is a single string, therefore
	   * we only look at the string at the "origin".
	   *
	   * We only accept clearly effective tactical defenses here,
	   * using a liberty heuristic. The reason for this is problems
	   * with ineffective self ataris which do defend tactically but
	   * have no strategical effect other than wasting owl nodes or
	   * confusing the eye analysis.
	   */
	  int dpos;
	  SGFTree *save_sgf_dumptree = sgf_dumptree;
	  int save_count_variations = count_variations;

	  sgf_dumptree = NULL;
	  count_variations = 0;
	  if (attack_and_defend(str, NULL, NULL, NULL, &dpos)
	      && (approxlib(dpos, color, 2, NULL) > 1
		  || does_capture_something(dpos, color))) {
	    TRACE("Found tactical defense for %1m at %1m.\n", str, dpos);
	    set_single_owl_move(shape_moves, dpos, "tactical_defense");
	    moves = shape_moves;
	  }
	  sgf_dumptree = save_sgf_dumptree;
	  count_variations = save_count_variations;
	}
	if (!moves)
	  continue;
      }
    } /* switch (pass) */

    /* For the up to MAX_MOVES best moves with value equal to
     * move_cutoff or higher, try to defend the dragon and see if it
     * can then be attacked.
     */
    for (k = 0; k < MAX_MOVES; k++) {
      int mpos;
      int ko_move = -1;
      int new_escape;
      int wid = MAX_GOAL_WORMS;
      
      /* Consider only the highest scoring move if we're deeper than
       * owl_branch_depth.
       *
       * FIXME: To behave as intended, k should be replaced by
       *        number_tried_moves.
       */
      if (stackp > owl_branch_depth && k > 0)
	break;
      
      current_owl_data = owl;
      
      if (pass == 1) {
        if (!get_next_move_from_list(&shape_patterns, color, shape_moves,
	                             move_cutoff, owl))
	  break;
      }
      else
	if (moves[k].value < move_cutoff)
	  break;
      
      mpos = moves[k].pos;
      modify_eyefilling_move(&mpos, color);
      ASSERT_ON_BOARD1(mpos);
      
      /* Have we already tested this move? */
      if (mw[mpos])
	continue;
      
      /* Try to make the move. */
      if (!komaster_trymove(mpos, color, moves[k].name, str,
			    &ko_move, savecode == 0))
	continue;

      new_escape = escape;
      if (moves[k].escape)
	new_escape++;

      TRACE("Trying %C %1m. Escape = %d. Current stack: ",
	    color, mpos, escape);
      if (verbose)
	dump_stack();

      /* We have now made a move. Analyze the new position. */
      push_owl(&owl);
      mw[mpos] = 1;
      number_tried_moves++;

      /* Add the stone just played to the goal dragon, unless the
       * pattern explicitly asked for not doing this.
       */
      owl_update_goal(mpos, moves[k].same_dragon, moves[k].lunch, owl, 0,
		      moves[k].pattern_data);
      mark_goal_in_sgf(owl->goal);

      if (!ko_move) {
	int acode = do_owl_attack(str, NULL, &wid, owl, new_escape);
	if (!acode) {
	  pop_owl(&owl);
	  popgo();
	  if (sgf_dumptree) {
	    char winstr[192];
	    sprintf(winstr, "defense effective)\n  (%d variations",   
	  		    count_variations - this_variation_number);
	    SGFTRACE(mpos, WIN, winstr);
	  }
	  close_pattern_list(color, &shape_patterns);
	  READ_RETURN(OWL_DEFEND, str, depth - stackp, move, mpos, WIN);
	}
	if (acode == GAIN)
	  saveworm = wid;
	UPDATE_SAVED_KO_RESULT(savecode, savemove, acode, mpos);
      }
      else {
	if (do_owl_attack(str, NULL, NULL, owl, new_escape) != WIN) {
	  savemove = mpos;
	  savecode = KO_B;
	}
      }
      
      /* Undo the tested move. */
      pop_owl(&owl);
      popgo();
    }
  }

  close_pattern_list(color, &shape_patterns);
  
  if (savecode) {
    if (savecode == LOSS) {
      SGFTRACE(savemove, savecode, "defense effective (loss) - B");
      if (wormid)
	*wormid = saveworm;
      READ_RETURN2(OWL_DEFEND, str, depth - stackp,
		   move, savemove, savecode, saveworm);
    }
    else {
      SGFTRACE(savemove, savecode, "defense effective (ko) - B");
      READ_RETURN(OWL_DEFEND, str, depth - stackp, move, savemove, savecode);
    }
  }

  if (number_tried_moves == 0 && min_eyes(&probable_eyes) >= 2) {
    SGFTRACE(0, WIN, "genus probably >= 2");
    READ_RETURN(OWL_DEFEND, str, depth - stackp, move, 0, WIN);
  }
  

  if (sgf_dumptree) {
    char winstr[196];
    int print_genus = eyemin == 1 ? 1 : 0;
    sprintf(winstr, "defense failed - genus %d)\n  (%d variations",
	  	    print_genus, count_variations - this_variation_number);
    SGFTRACE(0, 0, winstr);
  }

  READ_RETURN0(OWL_DEFEND, str, depth - stackp);
}


/* Returns true if the dragon at (target) can be defended given
 * two moves in a row. The first two moves to defend the
 * dragon are given as (*defend1) and (*defend2).
 */

int
owl_threaten_defense(int target, int *defend1, int *defend2)
{
  struct owl_move_data moves[MAX_MOVES];
  int k;
  int color = board[target];
  int result = 0;
  struct local_owl_data *owl;
  int reading_nodes_when_called = get_reading_node_counter();
  signed char saved_goal[BOARDMAX];
  double start = 0.0;
  int tactical_nodes;
  int move = 0;
  int move2 = 0;
  struct matched_patterns_list_data shape_patterns;

  shape_patterns.initialized = 0;

  result_certain = 1;
  if (worm[target].unconditional_status == DEAD)
    return 0;

  if (search_persistent_owl_cache(OWL_THREATEN_DEFENSE, target, 0, 0,
				  &result, defend1, defend2, NULL))
    return result;

  if (debug & DEBUG_OWL_PERFORMANCE)
    start = gg_cputime();

  TRACE("owl_threaten_defense %1m\n", target);
  init_owl(&owl, target, NO_MOVE, NO_MOVE, 1, NULL);
  memcpy(saved_goal, owl->goal, sizeof(saved_goal));
  owl_make_domains(owl, NULL);
  owl_shapes(&shape_patterns, moves, color, owl, &owl_defendpat_db);
  for (k = 0; k < MAX_MOVES; k++) {
    current_owl_data = owl;
    if (!get_next_move_from_list(&shape_patterns, color, moves, 1, owl))
      break;
    else {
      if (moves[k].pos != NO_MOVE && moves[k].value > 0)
	if (trymove(moves[k].pos, color, moves[k].name, target)) {
	  owl->lunches_are_current = 0;
	  owl_update_goal(moves[k].pos, moves[k].same_dragon,
	      		  moves[k].lunch, owl, 0, moves[k].pattern_data);
	  if (do_owl_defend(target, &move2, NULL, owl, 0) == WIN) {
	    move = moves[k].pos;
	    popgo();
	    /* Don't return the second move if occupied before trymove */
	    if (move2 != NO_MOVE && IS_STONE(board[move2]))
	      move2 = NO_MOVE;
	    result = WIN;
	    break;
	  }
	  else
	    popgo();
	  memcpy(owl->goal, saved_goal, sizeof(saved_goal));
	}
    }
  }
  tactical_nodes = get_reading_node_counter() - reading_nodes_when_called;
  gg_assert(stackp == 0);

  DEBUG(DEBUG_OWL_PERFORMANCE, 
    "owl_threaten_defense %1m %1m %1m, result %d (%d, %d nodes, %f seconds)\n",
	    target, move, move2, result, local_owl_node_counter,
	    tactical_nodes, gg_cputime() - start);

  store_persistent_owl_cache(OWL_THREATEN_DEFENSE, target, 0, 0,
			     result, move, move2, 0,
			     tactical_nodes, owl->goal, board[target]);

  if (defend1)
    *defend1 = move;
  if (defend2)
    *defend2 = move2;

  close_pattern_list(color, &shape_patterns);
  return result;
}



/*
 * This function calls owl_determine_life() to get an eye estimate,
 * and matchpat() for vital attack moves, and decides according to
 * various policies (depth-dependent) whether the dragon should thus
 * be considered alive.
 */
static int
owl_estimate_life(struct local_owl_data *owl,
		  struct local_owl_data *second_owl,
    		  struct owl_move_data vital_moves[MAX_MOVES],
		  const char **live_reason, int does_attack,
		  struct eyevalue *probable_eyes, int *eyemin, int *eyemax,
		  int eyefilling_points[BOARDMAX])
{
  SGFTree *save_sgf_dumptree = sgf_dumptree;
  int save_count_variations = count_variations;
  struct owl_move_data dummy_moves[MAX_MOVES];
  int other = OTHER_COLOR(owl->color);

  sgf_dumptree = NULL;
  count_variations = 0;

  owl_determine_life(owl, second_owl, does_attack, vital_moves,
		     probable_eyes, eyemin, eyemax, eyefilling_points);

  matches_found = 0;
  memset(found_matches, 0, sizeof(found_matches));

  if (get_level() >= 8) {
    memset(owl->safe_move_cache, 0, sizeof(owl->safe_move_cache));
    if (!does_attack) {
      clear_owl_move_data(dummy_moves);
      matchpat(owl_shapes_callback, other,
	       &owl_vital_apat_db, dummy_moves, owl->goal);
    }
    else if (max_eyes(probable_eyes) >= 2)
      matchpat(owl_shapes_callback, other,
	       &owl_vital_apat_db, vital_moves, owl->goal);
  }

  if ((debug & DEBUG_EYES) && (debug & DEBUG_OWL))
    gprintf("owl: eyemin=%d matches_found=%d\n", *eyemin, matches_found);
  if (*eyemin >= matches_found)
    *eyemin -= matches_found;
  else
    *eyemin = 0;

  sgf_dumptree = save_sgf_dumptree;
  count_variations = save_count_variations;

  if (*eyemin >= 2
      || (*eyemin == 1 && min_eyes(probable_eyes) >= 4)
      || (stackp > owl_distrust_depth
	  && min_eyes(probable_eyes) >= 2
	  && !matches_found)) {
    if (*eyemin >= 2)
      *live_reason = "2 or more secure eyes";
    else if (*eyemin == 1 && min_eyes(probable_eyes) >= 4)
      *live_reason = "1 secure eye, likely >= 4";
    else if (stackp > owl_distrust_depth
	     && min_eyes(probable_eyes) >= 2
	     && !matches_found)
      *live_reason = "getting deep, looks lively";
    else
      gg_assert(0);
    return 1;
  }

  if (!does_attack
      && (*eyemin + matches_found >= 2
	  || (*eyemin + matches_found == 1 && min_eyes(probable_eyes) >= 4)
      || (stackp > owl_distrust_depth
	  && min_eyes(probable_eyes) >= 2))) {
    /* We are not yet alive only due to owl vital attack patterns matching.
     * Let's try to defend against it.
     */
    owl_add_move(vital_moves, dummy_moves[0].defense_pos,
		 dummy_moves[0].value, dummy_moves[0].name,
		 SAME_DRAGON_CONNECTED, NO_MOVE, 0, NO_MOVE, MAX_MOVES, NULL);
  }

  return 0;
}


/* 
 * This function is invoked from do_owl_attack() and do_owl_defend()
 * for each node to determine whether the the dragon has sufficient
 * eye potential to live. It also generates vital moves to attack or
 * defend the eyes. There are two distinct sources for eyes. The first
 * is the eyespaces found by make_domains() and evaluated by
 * compute_eyes_pessimistic(). The second is the lunches found by
 * owl_find_lunches() and evaluated by sniff_lunch().
 *
 * The best guess of the eye potential is stored as an eyevalue in
 * *probable_eyes. This is not entirely reliable though since the
 * graph matching techniques in optics.c fail to understand subtleties
 * like atari inside the eyespace, cutting points in the wall, and
 * shortage of outside liberties. (The patterns in owl_vital_apats.db
 * are used to compensate for this. See do_owl_attack() and
 * do_owl_defend() for how these are used.) Also the estimates from
 * sniff_lunch() are fairly unreliable.
 *
 * A lower and upper bound on the number of eyes are returned in
 * *eyemin and *eyemax. The value of *eyemin must be offset by the
 * matches of owl_vital_apats.db. If that number is 2 or larger, we
 * should be certain of life.
 *
 * Vital moves to attack or defend eyes are returned in the moves[]
 * array. Also moves to reduce the uncertainty of the eye estimates
 * are added to this array, but with smaller move values. The
 * parameter does_attack determines whether to generate vital attack
 * moves or vital defense moves.
 *
 * The dragon is specified by the information in the owl struct. The
 * color of the dragon is passed in the color parameter.
 *
 * For use in the semeai code, a second dragon can be provided. Set
 * this to NULL when only one dragon is involved.
 *
 * For use from the semeai code it is also possible to find eyefilling
 * points, which adds stones to nakade shapes. Set eyefilling_points
 * to NULL if this information is not of interest.
 */

static void
owl_determine_life(struct local_owl_data *owl,
		   struct local_owl_data *second_owl,
		   int does_attack,
		   struct owl_move_data *moves,
		   struct eyevalue *probable_eyes, int *eyemin, int *eyemax,
		   int eyefilling_points[BOARDMAX])
{
  int color = owl->color;
  struct eye_data *eye = owl->my_eye;
  int mw[BOARDMAX];  /* mark relevant eye origins */
  int mz[BOARDMAX];  /* mark potentially irrelevant eye origins */
  int vital_values[BOARDMAX];
  int dummy_eyemin = 0;
  int dummy_eyemax = 0;
  struct eyevalue eyevalue;
  struct eyevalue eyevalue_list[BOARDMAX/2];
  int eyes_attack_points[BOARDMAX/2];
  int pessimistic_min;
  int attack_point;
  int defense_point;
  int pos;
  int k;
  int lunch;
  int num_eyes = 0;
  int num_lunches = 0;
  int save_debug = debug;
  memset(vital_values, 0, sizeof(vital_values));

  if (!eyemin)
    eyemin = &dummy_eyemin;
  if (!eyemax)
    eyemax = &dummy_eyemax;

  *eyemin = 0;
  *eyemax = 0;
  
  /* Turn off eye debugging if we're not also debugging owl. */
  if (!(debug & DEBUG_OWL))
    debug &= ~DEBUG_EYES;
  
  clear_owl_move_data(moves);
  
  if (!owl->lunches_are_current)
    owl_find_lunches(owl);
  
  if (0) {
    for (k = 0; k < MAX_LUNCHES; k++)
      if (owl->lunch[k] != NO_MOVE)
	gprintf("owl lunch %1m, attack %1m, defend %1m\n",
		owl->lunch[k],
		owl->lunch_attack_point[k],
		owl->lunch_defense_point[k]);
  }

  owl_make_domains(owl, second_owl);

  owl_find_relevant_eyespaces(owl, mw, mz);

  /* Reset halfeye data. Set topological eye value to something big. */
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (ON_BOARD(pos)) {
      owl->half_eye[pos].type = 0;
      owl->half_eye[pos].value = 10.0;
    }
  }
  
  /* Find topological half eyes and false eyes. */
  find_half_and_false_eyes(color, eye, owl->half_eye, mw);

  /* The eyespaces may have been split or changed in other ways by the
   * topological analysis, so we need to regenerate them and once more
   * determine which ones are relevant.
   */
  partition_eyespaces(owl->my_eye, owl->color);
  owl_find_relevant_eyespaces(owl, mw, mz);
  
  set_eyevalue(probable_eyes, 0, 0, 0, 0);

  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (ON_BOARD(pos) && mw[pos] > 1) {
      int value = 0;
      const char *reason = "";
      compute_eyes_pessimistic(pos, &eyevalue, &pessimistic_min,
			       &attack_point, &defense_point,
			       eye, owl->half_eye, eyefilling_points);

      /* If the eyespace is more in contact with own stones not in the goal,
       * than with ones in the goal, there is a risk that we can be cut off
       * from a major part of the eyespace. Thus we can't trust the opinion
       * of compute_eyes().
       *
       * (Obviously this is a quite fuzzy heuristic. With more accurate
       * connection analysis in the owl code we could do this more robustly.)
       */
      if (mw[pos] < mz[pos]
	  || (mw[pos] < 3 * mz[pos] && mz[pos] > 5))
	pessimistic_min = 0;

      /* It appears that this policy is needed no longer. */
#if 0
      /* If this eyespace includes an owl inessential string, we must assume
       * that the pessimistic min is 0.
       */
      if (pessimistic_min > 0) {
	for (pos2 = BOARDMIN; pos2 < BOARDMAX; pos2++) {
	  if (ON_BOARD(pos2)
	      && eye[pos2].origin == pos
	      && owl->inessential[pos2]) {
	    pessimistic_min = 0;
	    break;
	  }
	}
      }
#endif

      eyes_attack_points[num_eyes] = NO_MOVE;
      eyevalue_list[num_eyes] = eyevalue;
      *eyemin += pessimistic_min;

      /* Fill in the value field for use by the owl_eyespace() function. */
      eye[pos].value = eyevalue;

      /* This shortcut has been disabled for two reasons:
       * 1. Due to the vital attack moves being able to later reduce
       * the *eyemin, we can't say that a certain *eyemin is
       * sufficient.
       * 2. This part of the code is in no way time critical.
       */
#if 0
      /* Found two certain eyes---look no further. */
      if (*eyemin >= 2) {
	debug = save_debug;
	return 2;
      }
#endif

      if (eye_move_urgency(&eyevalue)) {
	value = 50;
	if (max_eyes(&eyevalue) - min_eyes(&eyevalue) == 2)
	  value = 70;
	else if (max_eyes(&eyevalue) - pessimistic_min == 2)
	  value = 60;
	/* Prefer attack over defense to avoid settling with mutual
	 * life when both dragons have 1.5 eyes.
	 */
	value += 5 * does_attack;
	reason = "vital move";
      }
      else if (max_eyes(&eyevalue) != pessimistic_min) {
	if (max_eyes(&eyevalue) - pessimistic_min == 2)
	  value = 40;
	else
	  value = 30;
	reason = "marginal eye space";
      }

      if (value > 0) {
	if (does_attack && attack_point != NO_MOVE) {
	  if (vital_values[attack_point] > 0) {
	    value += vital_values[attack_point];
	    if (value > 98)
	      value = 98; /* Higher values may get special interpretation. */
	  }

	  TRACE("%s at %1m, score %d (eye at %1m, value %s, pessimistic_min %d)\n",
		reason, attack_point, value,
		pos, eyevalue_to_string(&eyevalue), pessimistic_min);

	  if (eye[attack_point].marginal
	      && modify_stupid_eye_vital_point(owl, &attack_point, 1))
	    TRACE("vital point looked stupid, moved it to %1m\n",
		  attack_point);

	  if (attack_point != NO_MOVE) {
	    owl_add_move(moves, attack_point, value, reason,
			 SAME_DRAGON_MAYBE_CONNECTED, NO_MOVE,
			 0, NO_MOVE, MAX_MOVES, NULL);
	    vital_values[attack_point] = value;
	    eyes_attack_points[num_eyes] = attack_point;
	  }
	}

	/* The reason for the last set of tests is that we don't
	 * want to play a self atari in e.g. this position
	 *
	 * |XXX.
	 * |OOX.
	 * |.OX.
	 * |XOXX
	 * |XOOX
	 * |O*OX
	 * +----
	 *
	 * but it's okay in this position
	 * 
	 * |XXXXX
	 * |....X
	 * |OOOOX
	 * |.XXOX
	 * |.*XOX
	 * +-----
	 *
	 * In both cases * is the vital point according to the graph
	 * matching. The significant difference is that in the first
	 * case the vital point is adjacent to stones in the goal.
	 */
	else if (!does_attack
		 && defense_point != NO_MOVE
		 && board[defense_point] == EMPTY
		 && (!liberty_of_goal(defense_point, owl)
		     || !is_self_atari(defense_point, color)
		     || is_ko(defense_point, color, NULL)
		     || safe_move(defense_point, color) != 0)) {
	  if (vital_values[defense_point] > 0) {
	    value += vital_values[defense_point];
	    if (value > 98)
	      value = 98; /* Higher values may get special interpretation. */
	  }

	  TRACE("%s at %1m, score %d (eye at %1m, value %s, pessimistic_min %d)\n",
		reason, defense_point, value, pos,
		eyevalue_to_string(&eyevalue), pessimistic_min);

	  if ((eye[defense_point].marginal
	       || eye[defense_point].origin != pos)
	      && modify_stupid_eye_vital_point(owl, &defense_point, 0))
	    TRACE("vital point looked stupid, moved it to %1m\n",
		  defense_point);

	  if (defense_point != NO_MOVE) {
	    owl_add_move(moves, defense_point, value, reason,
			 SAME_DRAGON_MAYBE_CONNECTED, NO_MOVE,
			 0, NO_MOVE, MAX_MOVES, NULL);
	    vital_values[defense_point] = value;
	  }
	}
      }
      num_eyes++;
    }
  }

  /* Sniff each lunch for nutritional value. The assumption is that
   * capturing the lunch is gote, therefore the number of half eyes
   * equals the MINIMUM number of eyes yielded by the resulting eye
   * space.
   */
  {
    for (lunch = 0; (lunch < MAX_LUNCHES); lunch++)
      if (owl->lunch[lunch] != NO_MOVE
	  && owl->lunch_defense_point[lunch] != NO_MOVE) {
	int value = 0;
	int lunch_min;
	int lunch_probable;
	int lunch_max;
	struct eyevalue e;
	sniff_lunch(owl->lunch[lunch], 
		    &lunch_min, &lunch_probable, &lunch_max, owl);

	set_eyevalue(&e, 0, 0, lunch_probable, lunch_probable);
	*eyemax += lunch_max;

	if (lunch_probable == 0) {
	  if (countstones(owl->lunch[lunch]) == 1)
	    continue;
	  value = 20;
	}
	else if (lunch_probable == 1 && lunch_max == 1)
	  value = 60 + countstones(owl->lunch[lunch]);
	else if (lunch_probable == 1 && lunch_max == 2)
	  value = 70 + countstones(owl->lunch[lunch]);
	else
	  value = 75 + countstones(owl->lunch[lunch]);

	if (owl->lunch_attack_code[lunch] != WIN)
	  value -= 10;

	if (does_attack) {
	  defense_point = improve_lunch_defense(owl->lunch[lunch],
						owl->lunch_defense_point[lunch]);

	  if (vital_values[defense_point]) {
	    /* The point here is that the move which saves the lunch also
	     * attacks an eye. So this attack move reduces the global eye
	     * potential. The eyes arithmetic for probable_eyes has then
	     * to be adapted accordingly.
	     */
	    int ne;
	    for (ne = 0; ne < num_eyes - num_lunches; ne++)
	      if (eyes_attack_points[ne] == defense_point)
		break;
	    gg_assert(ne < num_eyes - num_lunches);
	    /* merge eye values */
	    add_eyevalues(&eyevalue_list[ne], &e, &eyevalue_list[ne]);
	    /* and adjust */
	    eyevalue_list[ne].a = 0;
	    eyevalue_list[ne].b = 0;
	  } 
	  else {
	    num_lunches++;
	    eyevalue_list[num_eyes++] = e;
	  }

	  TRACE("save lunch at %1m with %1m, score %d, probable eye %d, max eye %d\n",
		owl->lunch[lunch], defense_point, value,
		lunch_probable, lunch_max);
	  owl_add_move(moves, defense_point, value,
	      	       "save lunch", SAME_DRAGON_MAYBE_CONNECTED,
		       NO_MOVE, 0, NO_MOVE, MAX_MOVES, NULL);
	}
	else {
	  attack_point = improve_lunch_attack(owl->lunch[lunch],
					      owl->lunch_attack_point[lunch]);
	  TRACE("eat lunch at %1m with %1m, score %d, probable eye %d, max eye %d\n",
		owl->lunch[lunch], attack_point, value,
		lunch_probable, lunch_max);
	  /* We only remember the lunch for owl_update_goal() if the lunch
	   * cannot be defended with ko after the move.
	   * If we capture the lunch by an illegal ko capture, we become
	   * ko master with this move, and hence the above is true.
	   */
	  if (owl->lunch_attack_code[lunch] ==  WIN
	      || is_illegal_ko_capture(attack_point, owl->color))
	    owl_add_move(moves, attack_point, value, "eat lunch",
			 SAME_DRAGON_MAYBE_CONNECTED, owl->lunch[lunch],
			 0, NO_MOVE, MAX_MOVES, NULL);
	  else
	    owl_add_move(moves, attack_point, value, "eat lunch",
			 SAME_DRAGON_MAYBE_CONNECTED, NO_MOVE, 0, NO_MOVE,
			 MAX_MOVES, NULL);
	  num_lunches++;
	  eyevalue_list[num_eyes++] = e;
	}
      }
  }

  /* now, totalize the eye potential */
  {
    int ne;
    for (ne = 0; ne < num_eyes - num_lunches; ne++)
      add_eyevalues(probable_eyes, &eyevalue_list[ne], probable_eyes);

    *eyemax += max_eyes(probable_eyes);
    /* If we have at least two different eyespaces and can create one eye
     * in sente, we assume there's a chance to create another one. This is
     * needed because optics code don't know about eyespaces influencing
     * each other and combination moves (i.e. double threats to create an
     * eye).
     */
    if (num_eyes - num_lunches > 1 && max_eye_threat(probable_eyes) > 1)
      *eyemax += 1;

    for (; ne < num_eyes; ne++)
      add_eyevalues(probable_eyes, &eyevalue_list[ne], probable_eyes);
  }

  debug = save_debug;
}


/* The eyespaces we want to evaluate are the ones which
 * are adjacent to the dragon (whose stones comprise the
 * support of goal) which are not GRAY bordered. These
 * are the eyespaces of the dragon. Now we find their
 * origins.
 *
 * It is required that there are at least two distinct connections,
 * adjacent or diagonal, between non-marginal eyespace vertices and
 * stones of the goal dragon. Otherwise there is a risk that we
 * include irrelevant eye spaces.
 */

static void
owl_find_relevant_eyespaces(struct local_owl_data *owl,
			    int mw[BOARDMAX], int mz[BOARDMAX])
{
  int pos;
  int eye_color;
  int k;
  struct eye_data *eye = owl->my_eye;
  
  if (owl->color == WHITE)
    eye_color = WHITE;
  else
    eye_color = BLACK;

  memset(mw, 0, BOARDMAX * sizeof(mw[0]));
  memset(mz, 0, BOARDMAX * sizeof(mz[0]));
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (board[pos] == owl->color) {
      for (k = 0; k < 8; k++) {
	int pos2 = pos + delta[k];
	if (ON_BOARD(pos2)
	    && eye[pos2].color == eye_color
	    && !eye[pos2].marginal) {
	  if (owl->goal[pos])
	    mw[eye[pos2].origin]++;
	  else
	    mz[eye[pos2].origin]++;
	}
      }
    }
  }
}

/* Case 1.
 *
 * The optics code occasionally comes up with stupid vital moves, like
 * a in this position:
 *
 * ----+
 * O...|
 * OX..|
 * OX..|
 * O.X.|
 * .O.a|
 * ....|
 *
 * This function moves such moves to the second line.
 *
 * Case 2.
 *
 * In this position the optics code can suggest the empty 1-2 point as
 * vital move for the eyespace on the right edge. That is okay for attack
 * but obviously not for defense.
 *
 * ----+
 * XO.O|
 * XOOX|
 * XXO.|
 * .XOO|
 * .XXX|
 *
 * Case 3.
 *
 * Playing into a snapback is usually not an effective way to destroy
 * an eye.
 *
 * XOOO|
 * XOXX|
 * XXO.|
 * .XXO|
 * ....|
 *
 * This function changes the attack point to NO_MOVE (i.e. removes it).
 */
static int
modify_stupid_eye_vital_point(struct local_owl_data *owl, int *vital_point,
			      int is_attack_point)
{
  int up;
  int right;
  int k;
  int libs[2];

  /* Case 1. */
  for (k = 0; k < 4; k++) {
    up = delta[k];
    if (ON_BOARD(*vital_point - up))
      continue;

    if (board[*vital_point + up] != EMPTY)
      continue;

    right = delta[(k+1) % 4];

    if (board[*vital_point + right] != EMPTY
	|| board[*vital_point - right] != EMPTY)
      continue;

    if (board[*vital_point + 2 * up] != EMPTY
	|| board[*vital_point + up + right] != EMPTY
	|| board[*vital_point + up - right] != EMPTY) {
      *vital_point += up;
      return 1;
    }
  }

  /* Case 2. */
  if (!is_attack_point) {
    if (approxlib(*vital_point, OTHER_COLOR(owl->color), 1, NULL) == 0) {
      for (k = 4; k < 8; k++) {
	int pos = *vital_point + delta[k];
	if (board[pos] == OTHER_COLOR(owl->color)
	    && countlib(pos) == 1) {
	  findlib(pos, 1, vital_point);
	  return 1;
	}
      }
    }
  }

  /* Case 3. */
  if (is_attack_point
      && does_capture_something(*vital_point, OTHER_COLOR(owl->color))
      && accuratelib(*vital_point, OTHER_COLOR(owl->color), 2, libs) == 1
      && !attack(libs[0], NULL)) {
    *vital_point = NO_MOVE;
    return 1;
  }

  return 0;
}


/* The purpose of this function is to avoid moves which needlessly
 * fill in an eye. A typical example, from ld_owl:188, is
 *
 * -----+
 * .O.OX|
 * XOOXX|
 * XXOOX|
 * .XXO.|
 * ..XOO|
 * ..XXX|
 *
 * where various patterns manage to propose the eye-filling move on
 * the top edge instead of capturing the opponent stones and get two
 * solid eyes. This function modifies the move accordingly.
 */
static int
modify_eyefilling_move(int *move, int color)
{
  int k;
  int r;
  int other = OTHER_COLOR(color);
  /* Only do this for a small eye. */
  for (k = 0; k < 4; k++)
    if (ON_BOARD(*move + delta[k]) && board[*move + delta[k]] != color)
      return 0;

  for (r = 4; r < 8; r++)
    if (board[*move + delta[r]] == other
	&& countlib(*move + delta[r]) == 1) {
      for (k = 0; k < 4; k++)
	if (board[*move + delta[k]] == color
	    && countlib(*move + delta[k]) == 1
	    && !adjacent_strings(*move + delta[r], *move + delta[k]))
	  break;

      if (k == 4) {
	int new_move;
	findlib(*move + delta[r], 1, &new_move);
	TRACE("Changing eyefilling move at %1m to capture at %1m.\n",
	      *move, new_move);
	*move = new_move;
	return 1;
      }
    }
  
  return 0;    
}


/* 
 * Generates up to max_moves moves, attempting to attack or defend the goal
 * dragon. The found moves are put in moves, an array of owl_move_data
 * structs, starting in the position 'initial'.  The entries in the array are
 * sorted by value with moves[initial] having highest priority. When no more
 * moves are available this is indicated by value and coordinates in the array
 * being -1.
 *
 * This function automatically initializes the owl_safe_move cache the
 * pattern list. WATCH OUT: This has to be matched with a call to
 * close_pattern_list(pattern_list)!!!
 *
 * Returns 1 if at least one move is found, or 0 if no move is found.
 */

static void
owl_shapes(struct matched_patterns_list_data *pattern_list,
           struct owl_move_data moves[MAX_MOVES],
	   int color, struct local_owl_data *owl, struct pattern_db *type)
{
  SGFTree *save_sgf_dumptree = sgf_dumptree;
  int save_count_variations = count_variations;
  sgf_dumptree = NULL;
  count_variations = 0;

  current_owl_data = owl;
  
  clear_owl_move_data(moves);

  /* We must reset the owl safe_move_cache before starting the
   * pattern matching. The cache is used by owl_shapes_callback().
   */
  memset(owl->safe_move_cache, 0, sizeof(owl->safe_move_cache));
  init_pattern_list(pattern_list);
  matchpat(collect_owl_shapes_callbacks, color, type, pattern_list, owl->goal);

  sgf_dumptree = save_sgf_dumptree;
  count_variations = save_count_variations;
}


/* This function contains all the expensive checks for a matched pattern. */
static int
check_pattern_hard(int move, int color, struct pattern *pattern, int ll)
{
  int constraint_checked = 0;
  int safe_move_checked = 0;

  /* The very first check is whether we can disregard the pattern due
   * due to an owl safe_move_cache lookup.
   */
  if (!(pattern->class & CLASS_s))
    if (current_owl_data->safe_move_cache[move]) {
      if (current_owl_data->safe_move_cache[move] == 1)
        return 0;
      else
        safe_move_checked = 1;
    }

  /* If the constraint is cheap to check, we do this first. */
  if ((pattern->autohelper_flag & HAVE_CONSTRAINT)
      && pattern->constraint_cost < 0.45) {
    if (!pattern->autohelper(ll, move, color, 0))
      return 0;
    constraint_checked = 1;
  }

  /* For sacrifice patterns, the survival of the stone to be played is
   * not checked. Otherwise we discard moves which can be captured. 
   * Illegal ko captures are accepted for ko analysis.
   */
  if (!(pattern->class & CLASS_s) && !safe_move_checked) {
    if (!owl_safe_move(move, color)) {
      if (0)
	TRACE("  move at %1m wasn't safe, discarded\n", move);
      return 0;
    }
    if (!is_legal(move, color)) {
      if (0)
	TRACE("  move at %1m wasn't legal, discarded\n", move);
      return 0;
    }
  }
  
  /* For class n patterns, the pattern is contingent on an opponent
   * move at * not being captured.
   *
   * We can't use owl_safe_move() here because we would try the wrong color.
   */
  if (pattern->class & CLASS_n) {
    if (safe_move(move, OTHER_COLOR(color)) == 0) {
      if (0)
	TRACE("  opponent can't play safely at %1m, move discarded\n", move);
      return 0;
    }
  }

  /* If the pattern has a constraint, call the autohelper to see
   * if the pattern must be rejected.
   */
  if ((pattern->autohelper_flag & HAVE_CONSTRAINT) && !constraint_checked)
    if (!pattern->autohelper(ll, move, color, 0))
      return 0;
  return 1;
}


/* This initializes a pattern list, allocating memory for 200 patterns.
 * If more patterns need to be stored, collect_owl_shapes_callbacks will
 * dynamically reallocate additional memory.
 * The space for list->pattern_list is allocated here.
 *
 * This function is automatically called from owl_shapes. Every call here
 * has to be matched by a call to close_pattern_list below.
 */
static void
init_pattern_list(struct matched_patterns_list_data *list)
{
  gg_assert(!list->initialized);

  list->counter = 0;
  list->used = 0;

  list->pattern_list = malloc(200 * sizeof(list->pattern_list[0]));
  list->list_size = 200;
  gg_assert(list->pattern_list != NULL);
  list->pattern_heap = NULL;

  if (0)
    gprintf("List at %x has new array at %x\n", list, list->pattern_list);

  list->initialized = 1;
}


/* This function has to get called before the memory of *list is freed
 * in the calling function.
 */
static void
close_pattern_list(int color, struct matched_patterns_list_data *list)
{
  if (list->initialized) {
    if (0)
      gprintf("%d patterns matched, %d patterns checked\n", list->counter,
	      list->used);
    if (0)
      gprintf("Pattern list at %x freed for list at %x\n",
	      list->pattern_list, list);
    if (allpats && verbose) {
      int i;
      int found_one = 0;
      SGFTree *save_sgf_dumptree = sgf_dumptree;
      int save_count_variations = count_variations;
      sgf_dumptree = NULL;
      count_variations = 0;

      if (!current_owl_data->lunches_are_current)
	owl_find_lunches(current_owl_data);

      if (!list->pattern_heap)
	pattern_list_build_heap(list);

      for (i = 0; i < list->heap_num_patterns; i++)
	if (check_pattern_hard(list->pattern_heap[i]->move, color,
	     		       list->pattern_heap[i]->pattern,
			       list->pattern_heap[i]->ll)) {
	  if (!found_one) {
	    TRACE("Remaining valid (but unused) patterns at stack: ");
	    dump_stack();
	    found_one = 1;
	  }
      	  TRACE("Pattern %s found at %1m with value %d\n",
	        list->pattern_heap[i]->pattern->name,
	        list->pattern_heap[i]->move,
	        (int) list->pattern_heap[i]->pattern->value);
	}
      
      sgf_dumptree = save_sgf_dumptree;
      count_variations = save_count_variations;
    }

    free(list->pattern_list);
    free(list->pattern_heap);
  }
  list->counter = -1;
}


/* Can be called from gdb for debugging:
 * (gdb) set dump_pattern_list(&shape_patterns)
 */
void
dump_pattern_list(struct matched_patterns_list_data *list)
{
  int i;
  struct matched_pattern_data *matched_pattern;
  if (!list->initialized)
    return;
  gprintf("%oList size %d. %d Patterns in list, %d have been used.",
	  list->list_size, list->counter, list->used);
  for (i = 0; i < list->counter; i++) {
    matched_pattern = &list->pattern_list[i];
    gprintf("%o\n  Pattern %s (orient. %d) at %1m, value %f.",
	    matched_pattern->pattern->name, matched_pattern->ll,
	    matched_pattern->move, matched_pattern->pattern->value);
    if (matched_pattern->next_pattern_index != -1)
      gprintf("%o * ");
  }
  gprintf("%o\n");

  gprintf("%oCurrent heap ordering: \n");
  for (i = 0; i < list->heap_num_patterns; i++) {
    matched_pattern = list->pattern_heap[i];
    gprintf("%o %s (%1m), %f; ", matched_pattern->pattern->name,
	    matched_pattern->move, matched_pattern->pattern->value);
  }
  gprintf("\n");
}


/* This function stores a found pattern in the list for later evaluation.
 * The only processing done is computing the position of the move, and
 * forgetting the color.
 */
static void
collect_owl_shapes_callbacks(int anchor, int color, struct pattern *pattern,
                             int ll, void *data)
{
  struct matched_patterns_list_data *matched_patterns = data;
  struct matched_pattern_data *next_pattern;

  UNUSED(color); /* The calling function has to remember that. */

  if (matched_patterns->counter >= matched_patterns->list_size) {
    matched_patterns->list_size += 100;
    matched_patterns->pattern_list
        = realloc(matched_patterns->pattern_list,
	          matched_patterns->list_size
	          * sizeof(matched_patterns->pattern_list[0]));
  }

  next_pattern = &matched_patterns->pattern_list[matched_patterns->counter];
  next_pattern->move	= AFFINE_TRANSFORM(pattern->move_offset, ll, anchor);
  next_pattern->value	= pattern->value;
  next_pattern->ll	= ll;
  next_pattern->anchor	= anchor;
  next_pattern->pattern	= pattern;
  next_pattern->next_pattern_index = -1;

  matched_patterns->counter++;
}


#define MAX_STORED_REASONS	4

static int
valuate_combinable_pattern_chain(struct matched_patterns_list_data *list,
				 int pos)
{
  /* FIXME: This is just a first attempt at pattern combination.
   *	    Improve it.  The first idea is to differentiate between
   *	    move reason types.  For instance, when there is a secure
   *	    eye already, a threat to create another is more severe.
   *
   *	    This will certainly involve splitting the function into
   *	    attack and defense versions.
   */

  int pattern_index = list->first_pattern_index[pos];
  int num_capture_threats = 0;
  int capture_threats[MAX_STORED_REASONS];
  int num_eye_threats = 0;
  int eye_threats[MAX_STORED_REASONS];
  int num_reverse_sente = 0;
  int reverse_sente_against[MAX_STORED_REASONS];
  int num_move_reasons;
  float full_value = 0.0;

  ASSERT1(pattern_index != -1, pos);

  do {
    struct matched_pattern_data *pattern_data = (list->pattern_list
						 + pattern_index);
    struct pattern_attribute *attribute;

    /* Skip patterns that haven't passed constraint validation. */
    if (pattern_data->pattern) {
      for (attribute = pattern_data->pattern->attributes;
	   attribute->type != LAST_ATTRIBUTE;
	   attribute++) {
	int k;
	int target = AFFINE_TRANSFORM(attribute->offset, pattern_data->ll,
				      pattern_data->move);

	switch (attribute->type) {
	case THREATENS_TO_CAPTURE:
	  if (num_capture_threats < MAX_STORED_REASONS) {
	    ASSERT1(IS_STONE(board[target]), target);
	    target = find_origin(target);

	    for (k = 0; k < num_capture_threats; k++) {
	      if (capture_threats[k] == target)
		break;
	    }

	    if (k == num_capture_threats) {
	      capture_threats[num_capture_threats++] = target;
	      full_value += pattern_data->pattern->value;
	    }
	  }

	  break;

	case THREATENS_EYE:
	  if (num_eye_threats < MAX_STORED_REASONS) {
	    target = current_owl_data->my_eye[target].origin;

	    for (k = 0; k < num_eye_threats; k++) {
	      if (eye_threats[k] == target)
		break;
	    }

	    if (k == num_eye_threats) {
	      eye_threats[num_eye_threats++] = target;
	      full_value += pattern_data->pattern->value;
	    }
	  }

	  break;

	case REVERSE_SENTE:
	  if (num_reverse_sente < MAX_STORED_REASONS) {
	    ASSERT1(board[target] == EMPTY, target);

	    for (k = 0; k < num_reverse_sente; k++) {
	      if (reverse_sente_against[k] == target)
		break;
	    }

	    if (k == num_reverse_sente) {
	      reverse_sente_against[num_reverse_sente++] = target;
	      full_value += pattern_data->pattern->value;
	    }
	  }

	  break;

	default:
	  gg_assert(0);
	}
      }
    }

    pattern_index = pattern_data->next_pattern_index;
  } while (pattern_index >= 0);


  num_move_reasons = num_capture_threats + num_eye_threats + num_reverse_sente;
  if (num_move_reasons <= 1) {
    /* Not much to combine, eh? */
    return 0;
  }

  if (num_move_reasons == 2)
    return gg_min(gg_normalize_float2int(full_value, 1.0), 75);
  if (num_move_reasons == 3)
    return gg_min(gg_normalize_float2int(full_value * 0.85, 1.0), 90);
  return gg_min(gg_normalize_float2int(full_value * 0.75, 1.0), 99);
}


#if USE_BDIST

/* Compute the squared of the distance of a point on the board to the
 * center of the board.
 */
static int
bdist(int move)
{
  /* i = 0:              idist = - (board_size - 1)
   * i = board_size -1 : idist =    board_size - 1
   */
  int idist = 2*I(move) - board_size + 1;
  int jdist = 2*J(move) - board_size + 1;
  return idist*idist + jdist*jdist;
}


/* NOTICE : In order to stabilize the regression test results,
 * arbitrary parameters like pattern memory address and move position
 * have been included in the sorting algorithm.
 */

#define BETTER_PATTERN(a, b)				\
  ((a)->value > (b)->value				\
   || ((a)->value == (b)->value				\
       && ((a)->pattern < (b)->pattern			\
	   || ((a)->pattern == (b)->pattern		\
	       && ((a)->bdist < (b)->bdist		\
		   || ((a)->bdist == (b)->bdist		\
		       && (a)->move < (b)->move))))))

#else	/* not USE_BDIST */

#define BETTER_PATTERN(a, b)				\
  ((a)->value > (b)->value				\
   || ((a)->value == (b)->value				\
       && ((a)->pattern < (b)->pattern			\
	   || ((a)->pattern == (b)->pattern		\
	       && (a)->move < (b)->move))))

#endif	/* not USE_BDIST */


static void
pattern_list_prepare(struct matched_patterns_list_data *list)
{
  int k;
  int pos;

  list->heap_num_patterns = 0;

  /* This is more than needed in case of (combinable) pattern chains,
   * but it is easier to allocate more than to count real number of
   * heap elements first.
   */
  if (list->counter > 0) { /* avoid malloc(0) */
    list->pattern_heap = malloc(list->counter * sizeof(*(list->pattern_heap)));
    gg_assert(list->pattern_heap != NULL);
  }
  else {
    /* free() has defined behaviour for NULL pointer */
    list->pattern_heap = NULL;
  }

  for (pos = BOARDMIN; pos < BOARDMAX; pos++)
    list->first_pattern_index[pos] = -1;

  for (k = 0; k < list->counter; k++) {
    int move = list->pattern_list[k].move;

#if USE_BDIST
    list->pattern_list[k].bdist = bdist(move);
#endif

    /* Allocate heap elements for normal patterns.  Link combinable
     * patterns in chains.
     */
    if (!(list->pattern_list[k].pattern->class & CLASS_c))
      list->pattern_heap[list->heap_num_patterns++] = &list->pattern_list[k];
    else {
      list->pattern_list[k].next_pattern_index = list->first_pattern_index[move];
      list->first_pattern_index[move] = k;
    }
  }

  /* Allocate one heap element for each chain of combinable patterns
   * and calculate initial chain values (as if all patterns passed
   * constraint validation).
   */
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (list->first_pattern_index[pos] != -1) {
      struct matched_pattern_data *pattern_data
	= &list->pattern_list[list->first_pattern_index[pos]];

      pattern_data->value = valuate_combinable_pattern_chain(list, pos);
      list->pattern_heap[list->heap_num_patterns++] = pattern_data;
    }
  }

  if (list->heap_num_patterns > 0)
    pattern_list_build_heap(list);
}


/* Fast heap building.  Takes O(n) only. */
static void
pattern_list_build_heap(struct matched_patterns_list_data *list)
{
  int k;
  int limit = list->heap_num_patterns / 2;

  for (k = limit; --k >= 0;) {
    int parent;
    int child;
    struct matched_pattern_data *pattern_data = list->pattern_heap[k];

    for (parent = k; parent < limit; parent = child) {
      child = 2 * parent + 1;
      if (child + 1 < list->heap_num_patterns
	  && BETTER_PATTERN(list->pattern_heap[child + 1],
			    list->pattern_heap[child]))
	child++;

      if (BETTER_PATTERN(pattern_data, list->pattern_heap[child]))
	break;

      list->pattern_heap[parent] = list->pattern_heap[child];
    }

    list->pattern_heap[parent] = pattern_data;
  }
}


/* Pops patterns list's heap once. */
static void
pattern_list_pop_heap_once(struct matched_patterns_list_data *list)
{
  int parent;
  int child;

  list->heap_num_patterns--;
  for (parent = 0; 2 * parent + 1 < list->heap_num_patterns; parent = child) {
    child = 2 * parent + 1;
    if (BETTER_PATTERN(list->pattern_heap[child + 1],
		       list->pattern_heap[child]))
      child++;

    if (BETTER_PATTERN(list->pattern_heap[list->heap_num_patterns],
		       list->pattern_heap[child]))
      break;

    list->pattern_heap[parent] = list->pattern_heap[child];
  }

  list->pattern_heap[parent] = list->pattern_heap[list->heap_num_patterns];
}


/* Sink top element of heap because it got devalued.  This happens
 * when a combinable pattern doesn't pass check_pattern_hard() -- it
 * is no longer counted and its whole chain's value is reduced.
 */
static void
pattern_list_sink_heap_top_element(struct matched_patterns_list_data *list)
{
  int parent;
  int child;
  struct matched_pattern_data *heap_top_element = list->pattern_heap[0];

  for (parent = 0; 2 * parent + 1 < list->heap_num_patterns; parent = child) {
    child = 2 * parent + 1;
    if (child + 1 < list->heap_num_patterns
	&& BETTER_PATTERN(list->pattern_heap[child + 1],
			  list->pattern_heap[child]))
      child++;

    if (BETTER_PATTERN(heap_top_element,
		       list->pattern_heap[child]))
      break;

    list->pattern_heap[parent] = list->pattern_heap[child];
  }

  list->pattern_heap[parent] = heap_top_element;
}


/* Adds all goal strings in the pattern area to the cuts[] list, if there
 * is more than one.
 */
static void
generate_cut_list(struct pattern *pattern, int ll, int anchor,
    		  int cuts[MAX_CUTS], struct local_owl_data *owl)
{
  int k;
  int num = 0;
  signed char mark[BOARDMAX];

  memset(mark, 0, BOARDMAX);
  for (k = 0; k < pattern->patlen; k++) {
    int pos = AFFINE_TRANSFORM(pattern->patn[k].offset, ll, anchor);
    if (!IS_STONE(board[pos]))
      continue;
    pos = find_origin(pos);
    if (!mark[pos] && board[pos] == owl->color && owl->goal[pos]) {
      cuts[num++] = pos;
      mark[pos] = 1;
      if (num == MAX_CUTS)
	return;
    }
  }
  if (num == 1)
    cuts[0] = NO_MOVE;
  else if ((debug & DEBUG_SPLIT_OWL) && num > 1)
    gprintf("Move provokes %d cuts, among them %1m and %1m.\n", num,
	    cuts[0], cuts[1]);
}

/* This function searches in the previously stored list of matched
 * patterns for the highest valued unused patterns that have a valid
 * constraint.  It returns the moves at the next empty positions in
 * the array moves[].  Empty positions in the moves array are marked
 * by having value <= 0.  There must be enough empty positions in the
 * list.
 *
 * If the highest valued pattern found has a value less than cutoff,
 * no move is returned.  Returns 1 if a move is found, 0 otherwise.
 *
 * This function also dispatches constraint validation of combinable
 * pattern chains.  Whenever a pattern from a chain fails constraints,
 * the chain is reevaluated and most likely drops in value enough to
 * let other patterns (or chains) climb to the top of pattern heap.
 *
 * This function loops until enough moves are found or the end of the
 * list is reached.
 */

static int
get_next_move_from_list(struct matched_patterns_list_data *list, int color,
			struct owl_move_data *moves, int cutoff,
			struct local_owl_data *owl)
{
  int move_found = 0;
  SGFTree *save_sgf_dumptree = sgf_dumptree;
  int save_count_variations = count_variations;

  sgf_dumptree = NULL;
  count_variations = 0;

  /* Prepare pattern list if needed. */
  if (!list->pattern_heap)
    pattern_list_prepare(list);

  while (list->heap_num_patterns > 0) {
    int k;
    struct matched_pattern_data *pattern_data;
    struct pattern *pattern;
    int move;
    int value;
    int ll;
    int anchor;
    int next_pattern_index;

    /* Peek top element of heap associated with pattern list. */
    if (list->pattern_heap[0]->value < cutoff)
      break;

    pattern_data = list->pattern_heap[0];
    pattern = list->pattern_heap[0]->pattern;
    move    = list->pattern_heap[0]->move;
    value   = list->pattern_heap[0]->value;
    ll      = list->pattern_heap[0]->ll;
    anchor  = list->pattern_heap[0]->anchor;
    next_pattern_index = list->pattern_heap[0]->next_pattern_index;

    list->used++;

    ASSERT_ON_BOARD1(move);
    for (k = 0; k < MAX_MOVES; k++) {
      if (moves[k].pos == move || moves[k].value <= 0)
	break;
    }

    if (moves[k].pos == move) {
      /* No point in testing this pattern/chain.  Throw it out. */
      pattern_list_pop_heap_once(list);
      continue;
    }

    /* There has to be an empty space. */
    gg_assert(k < MAX_MOVES);

    /* If a pattern chain was devalued because its last pattern didn't
     * pass constraint validation, `pattern' is set NULL (i.e. nothing
     * more to test).  Note that devalued chains might still be
     * useful, i.e. if 2 of 3 patterns passed check_pattern_hard().
     */
    if (pattern == NULL
	|| check_pattern_hard(move, color, pattern, ll)) {
      if (next_pattern_index == -1) {
	/* Normal pattern or last one in a chain. */
	pattern_list_pop_heap_once(list);
      }
      else {
	/* We just validated a non-last pattern in a chain.  Since the
	 * chain remains at the same value, we keep the heap structure
	 * untouched.  However, we need to set heap's top to point to
	 * next pattern of the chain.
	 */
	list->pattern_heap[0] = list->pattern_list + next_pattern_index;
	list->pattern_heap[0]->value = value;
	continue;
      }

      moves[k].pos = move;
      moves[k].value = value;
      clear_cut_list(moves[k].cuts);
      move_found = 1;

      if (pattern && !(pattern->class & CLASS_c)) {
	moves[k].name = pattern->name;
	TRACE("Pattern %s found at %1m with value %d\n",
	      pattern->name, move, moves[k].value);

	if (pattern->class & CLASS_C) {
	  /* Cut possible. (Only used in attack patterns). Try to find
	   * goal strings in the pattern area and store them in the cut list
	   * if there is more than one.
	   */
	  DEBUG(DEBUG_SPLIT_OWL,
	      	"Generating cut list for move at %1m.\n", move);
	  generate_cut_list(pattern, ll, anchor, moves[k].cuts, owl);
	}

	if (pattern->class & CLASS_B)
	  moves[k].same_dragon = SAME_DRAGON_NOT_CONNECTED;
	else if (pattern->class & CLASS_a) {
	  moves[k].same_dragon = SAME_DRAGON_ALL_CONNECTED;
	  moves[k].pattern_data = pattern_data;
	}
	else if (!(pattern->class & CLASS_b))
	  moves[k].same_dragon = SAME_DRAGON_CONNECTED;
	else {
	  int i;
	  enum same_dragon_value same_dragon = SAME_DRAGON_MAYBE_CONNECTED;

	  /* If we do not yet know whether the move belongs to the
	   * same dragon, we see whether another pattern can clarify.
	   */
	  for (i = 0; i < list->heap_num_patterns; i++) {
	    pattern_data = list->pattern_heap[i];

	    if (pattern_data->pattern
		&& pattern_data->move == move
		&& ((pattern_data->pattern->class & CLASS_B)
		    || !(pattern_data->pattern->class & CLASS_b))) {
	      if (check_pattern_hard(move, color, pattern_data->pattern,
				     pattern_data->ll)) {
		TRACE("Additionally pattern %s found at %1m\n",
		      pattern_data->pattern->name, move);
		if (pattern_data->pattern->class & CLASS_B)
		  same_dragon = SAME_DRAGON_NOT_CONNECTED;
		else if (pattern_data->pattern->class & CLASS_a) {
		  same_dragon = SAME_DRAGON_ALL_CONNECTED;
		  moves[k].pattern_data = pattern_data;
		}
		else
		  same_dragon = SAME_DRAGON_CONNECTED;

		break;
	      }
	    }
	  }

	  moves[k].same_dragon = same_dragon;
	}
      }
      else {
	moves[k].name = "Pattern combination";
	if (verbose) {
	  /* FIXME: write names of all patterns in chain. */
	}

	/* FIXME: Add handling of CLASS_b.
	 *
	 * FIXME: It is silently assumed that all patterns in the
	 *	  chain have the same class.  When the last pattern in
	 *	  chain didn't match, this will not work at all.
	 */
	if (pattern && pattern->class & CLASS_B)
	  moves[k].same_dragon = SAME_DRAGON_NOT_CONNECTED;
	else if (pattern && pattern->class & CLASS_a) {
	  moves[k].same_dragon = SAME_DRAGON_ALL_CONNECTED;
	  moves[k].pattern_data = list->pattern_heap[0];
	}
	else
	  moves[k].same_dragon = SAME_DRAGON_CONNECTED;
      }

      if (pattern && pattern->class & CLASS_E)
	moves[k].escape = 1;
      else
	moves[k].escape = 0;

      break;
    }
    else {			/* !check_pattern_hard(...) */
      if (!(pattern->class & CLASS_c)) {
	/* Just forget about it. */
	pattern_list_pop_heap_once(list);
      }
      else {
	/* Set this pattern to not matched and advance to next one in
	 * the chain, if any.
	 */
	list->pattern_heap[0]->pattern = NULL;
	if (next_pattern_index != -1)
	  list->pattern_heap[0] = list->pattern_list + next_pattern_index;

	/* Reevaluate chain and adjust heap structure accordingly. */
	list->pattern_heap[0]->value = valuate_combinable_pattern_chain(list,
									move);
	pattern_list_sink_heap_top_element(list);
      }
    }
  }

  sgf_dumptree = save_sgf_dumptree;
  count_variations = save_count_variations;

  return move_found;
}


/* This function takes an array of already found moves (passed as
 * 'data') and looks for moves to replace these. Only moves near
 * the goal dragon are considered.
 */
static void
owl_shapes_callback(int anchor, int color, struct pattern *pattern,
		    int ll, void *data)
{
  int tval;  /* trial move and its value */
  int move;
  struct owl_move_data *moves = data; /* considered moves passed as data */
  enum same_dragon_value same_dragon = SAME_DRAGON_MAYBE_CONNECTED;
  int escape = 0;
  int defense_pos;

  /* Pick up the location of the move */
  move = AFFINE_TRANSFORM(pattern->move_offset, ll, anchor);

  /* Before we do any expensive reading, check whether this move
   * already is known with a higher value or if there are too many
   * other moves with higher value.
   */
  if (!allpats) {
    int k;
    for (k = 0; k < MAX_MOVES; k++) {
      if (moves[k].value == -1)
	break;
      if (moves[k].pos == move) {
	if (moves[k].value >= pattern->value)
	  return;
	else
	  break;
      }
    }
    if (k == MAX_MOVES && moves[MAX_MOVES - 1].value >= pattern->value)
      return;
  }
  
  if (!check_pattern_hard(move, color, pattern, ll))
    return;

  /* and work out the value of this move */
  if (pattern->helper) {
    /* ask helper function to consider the move */
    gg_assert(0);
    DEBUG(DEBUG_HELPER, "  asking helper to consider '%s'+%d at %1m\n",
	  pattern->name, ll, move);
    tval = pattern->helper(pattern, ll, move, color);
    
    if (tval > 0) {
      DEBUG(DEBUG_HELPER, "helper likes pattern '%s' value %d at %1m\n",
	    pattern->name, tval, move);
    }
    else {
      DEBUG(DEBUG_HELPER, "  helper does not like pattern '%s' at %1m\n",
	    pattern->name, move);
      return;  /* pattern matcher does not like it */
    }
  }
  else { /* no helper */
    tval = (int) pattern->value;
  }

  /* having made it here, we have made it through all the extra checks */

  TRACE("Pattern %s found at %1m with value %d\n", pattern->name, move, tval);

  if (pattern->class & CLASS_B)
    same_dragon = SAME_DRAGON_NOT_CONNECTED;
  else if (pattern->class & CLASS_b)
    same_dragon = SAME_DRAGON_MAYBE_CONNECTED;
  else if (pattern->class & CLASS_a) {
    same_dragon = SAME_DRAGON_ALL_CONNECTED;
    /* FIXME: Currently this code is only used with vital attack
     * moves, so there is no use for the "a" classification. If it
     * would be needed in the future it's necessary to set up a struct
     * matched_pattern_data here to be passed to owl_add_move(). This
     * is not all that simple with respect to memory management
     * however. Notice that a local variable in this function would go
     * out of scope too early.
     */
    gg_assert(0);
  }
  else
    same_dragon = SAME_DRAGON_CONNECTED;

  if (pattern->class & CLASS_E)
    escape = 1;
  else 
    escape = 0;

  /* Finally, check for position of defense move. */
  {
    int k;
    defense_pos = move;
    for (k = 0; k < pattern->patlen; k++)
      if (pattern->patn[k].att == ATT_not)
	defense_pos = AFFINE_TRANSFORM(pattern->patn[k].offset, ll, anchor);
  }
  
  owl_add_move(moves, move, tval, pattern->name, same_dragon, NO_MOVE,
      	       escape, defense_pos, MAX_MOVES, NULL);
}


/* Add a move to the list of candidate moves */

static void
owl_add_move(struct owl_move_data *moves, int move, int value,
	     const char *reason, enum same_dragon_value same_dragon, int lunch,
	     int escape, int defense_pos, int max_moves,
	     struct matched_pattern_data *pattern_data)
{
  int k;

  if (!found_matches[move]) {
    found_matches[move] = 1;
    matches_found++;
  }
  
  /* Add the new move to the list of already found moves, if the value
   * is sufficently large. We keep the list sorted.
   *
   * First we must see if this move already is in the list.
   */
  for (k = 0; k < max_moves; k++) {
    if (moves[k].value == -1)
      break;
    if (moves[k].pos == move) {
      if (same_dragon > moves[k].same_dragon) {
	moves[k].same_dragon = same_dragon;
	moves[k].pattern_data = pattern_data;
      }
      if (!moves[k].escape)
	escape = 0;
      break;
    }
  }

  /* Did we already have this move in the list with a higher value? */
  if (k < max_moves && moves[k].value >= value)
    return;

  /* Insert the move at the right place in the list and adjust other
   * entries as needed.
   */
  for (; k >= 0; k--) {
    if (k == 0 || value <= moves[k-1].value) {
      /* Can't get higher. Insert the move below this point and quit
       * looping.
       */
      if (k < max_moves) {
	moves[k].pos = move;
	moves[k].value = value;
	moves[k].name = reason;
	/* If B or b class pattern, this move shouldn't be added to the
         * dragon under consideration.
	 */
	moves[k].same_dragon = same_dragon;
	moves[k].pattern_data = pattern_data;
	moves[k].lunch = lunch;
	moves[k].escape = escape;
	moves[k].defense_pos = defense_pos;
      }
      break;
    }
    /* Shuffle the passed move one step downwards. */
    if (k < max_moves)
      moves[k] = moves[k-1]; /* struct copy */
  }

  /* Assert that the list contains unique moves. */
  if (0) {
    int l;
    for (k = 0; k < max_moves; k++)
      for (l = k+1; l < max_moves; l++)
	gg_assert(moves[k].pos == 0
		  || moves[k].pos != moves[l].pos);
  }
}  


/* Marks the dragons at apos and bpos. If only one dragon
 * needs marking, bpos should be passed as NO_MOVE. 
 */

static void
owl_mark_dragon(int apos, int bpos, struct local_owl_data *owl,
		int new_dragons[BOARDMAX])
{
  int pos;
  int color = board[apos];
  
  ASSERT1(bpos == NO_MOVE || board[bpos] == color, bpos);

  if (new_dragons == NULL) {
    for (pos = BOARDMIN; pos < BOARDMAX; pos++)
      if (ON_BOARD(pos)) {
	if (is_same_dragon(pos, apos) || is_same_dragon(pos, bpos))
	  owl->goal[pos] = 1;
	else
	  owl->goal[pos] = 0;
      }
  }
  else {
    for (pos = BOARDMIN; pos < BOARDMAX; pos++)
      if (ON_BOARD(pos)) {
	if (IS_STONE(board[pos])
	    && (new_dragons[pos] == new_dragons[apos]
		|| new_dragons[pos] == new_dragons[bpos]))
	  owl->goal[pos] = 1;
	else
	  owl->goal[pos] = 0;
      }
  }

  memcpy(owl->cumulative_goal, owl->goal, sizeof(owl->goal));
  owl->color = color;
  owl_mark_boundary(owl);
}


/* Marks the worms at apos and bpos. If only one worm
 * needs marking, bpos should be passed as NO_MOVE. 
 */

static void
owl_mark_worm(int apos, int bpos, struct local_owl_data *owl)
{
  int pos;
  int color = board[apos];
  
  ASSERT1(bpos == NO_MOVE || board[bpos] == color, bpos);

  for (pos = BOARDMIN; pos < BOARDMAX; pos++)
    if (ON_BOARD(pos)) {
      if (is_same_worm(pos, apos) || is_same_worm(pos, bpos))
	owl->goal[pos] = 1;
      else
	owl->goal[pos] = 0;
    }

  owl->color = color;
}


/* Mark the boundary strings of the dragon. A boundary string is marked 2
 * if it adjoins a friendly live dragon, 1 otherwise.
 */

static void
owl_mark_boundary(struct local_owl_data *owl)
{
  int k;
  int pos;
  int color = owl->color;
  int other = OTHER_COLOR(color);
  
  memset(owl->boundary, 0, sizeof(owl->boundary));
  memset(owl->neighbors, 0, sizeof(owl->neighbors));

  /* Find all friendly neighbors of the dragon in goal. */
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (board[pos] == color && owl->goal[pos]) {
      for (k = 0; k < 4; k++) {
	if (board[pos + delta[k]] == EMPTY
	    && board[pos + 2 * delta[k]] == color
	    && !owl->neighbors[pos + 2 * delta[k]])
	  mark_string(pos + 2 * delta[k], owl->neighbors, 1);
      }

      for (; k < 8; k++) {
	int pos2 = pos + delta[k];

	if (board[pos2] == color
	    && !owl->neighbors[pos2]
	    && (board[SOUTH(gg_min(pos, pos2))] == EMPTY
		|| board[NORTH(gg_max(pos, pos2))] == EMPTY))
	  mark_string(pos2, owl->neighbors, 1);
      }
    }
  }

  /* First find all boundary strings (including those adjacent not to
   * the goal dragon, but one of its neighbors).
   */
  for (pos = BOARDMIN; pos < BOARDMAX; pos++)
    if (board[pos] == other && !owl->boundary[pos]) {
      for (k = 0; k < 8; k++)
	if (ON_BOARD(pos + delta[k])
	    && (owl->goal[pos + delta[k]] || owl->neighbors[pos + delta[k]])) {
	  mark_string(pos, owl->boundary, 1);
	  break;
	}
    }

  /* Upgrade the mark of a boundary string if it adjoins a safe
   * friendly dragon.
   */
  for (pos = BOARDMIN; pos < BOARDMAX; pos++)
    if (owl->boundary[pos] == 1) {
      for (k = 0; k < 8; k++) {
	int pos2 = pos + delta[k];
	if (board[pos2] == color
	    && !owl->goal[pos2]
	    && !owl->neighbors[pos2]
	    && ((dragon[pos2].crude_status != DEAD && countstones(pos2) > 2)
		|| dragon[pos2].crude_status == ALIVE)) {
	  mark_string(pos, owl->boundary, 2);
	  break;
	}
      }
    }
  
  /* During the owl reading, stones farther away may become parts of
   * the boundary. We mark those strings neighboring some other
   * friendly dragon with boundary value 2 right away, since we have
   * no mechanism for detecting this later.
   */
  for (pos = BOARDMIN; pos < BOARDMAX; pos++)
    if (board[pos] == other && owl->boundary[pos] == 0) {
      /* If a lunch has been amalgamated into a larger dragon, we
       * have to back out now.
       *
       * Notice that we assume that no stone of the attacking color
       * has been placed on the board with trymove() when this
       * function is called. Thus we can (mostly) trust the worm data for
       * stones of this color.
       */
      if (worm[pos].attack_codes[0] != 0
	  && worm[pos].size != dragon[pos].size)
	continue;
      
      /* This can happen if called when stackp > 0 */
      if (dragon[pos].id == -1)
	continue;
      
      for (k = 0; k < DRAGON2(pos).neighbors; k++) {
	int d = DRAGON2(pos).adjacent[k];
	int apos = dragon2[d].origin;
	
	if (board[apos] == color && !owl->goal[apos]) {
	  owl->boundary[pos] = 2;
	  break;
	}
      }
    }
}

/* Add the stone just played to the goal dragon if same_dragon is
 * SAME_DRAGON_CONNECTED. We also add all stones belonging to the same
 * generalized string to the goal. If same_dragon is
 * SAME_DRAGON_MAYBE_CONNECTED, we only add the stones if at least one
 * stone of the generalized string already was part of the goal. If
 * same_dragon is SAME_DRAGON_NOT_CONNECTED, we don't add any stones
 * at all.
 *
 * The SAME_DRAGON_ALL_CONNECTED case is like SAME_DRAGON_CONNECTED
 * but additionally all other own stones in the pattern suggesting the
 * move are also added to the goal.
 */
static void
owl_update_goal(int pos, enum same_dragon_value same_dragon, int lunch,
    		struct local_owl_data *owl, int semeai_call,
		struct matched_pattern_data *pattern_data)
{
  int stones[MAX_BOARD * MAX_BOARD];
  int num_stones;
  int k;
  int do_add = 1;
  SGFTree *save_sgf_dumptree = sgf_dumptree;
  int save_count_variations = count_variations;
  

  /* Turn off sgf output during find_superstring(). */
  sgf_dumptree = NULL;
  count_variations = 0;
  
  if (same_dragon == SAME_DRAGON_NOT_CONNECTED)
    num_stones = findstones(pos, MAX_BOARD * MAX_BOARD, stones);
  else if (semeai_call)
    find_superstring_conservative(pos, &num_stones, stones);
  else
    find_superstring(pos, &num_stones, stones);

  /* Turn sgf output back on. */
  sgf_dumptree = save_sgf_dumptree;
  count_variations = save_count_variations;
  
  /* If same_dragon field is 1, only add if the played stone
   * clearly is in contact with the goal dragon.
   */
  if (same_dragon <= SAME_DRAGON_MAYBE_CONNECTED) {
    do_add = 0;
    for (k = 0; k < num_stones; k++)
      if (owl->goal[stones[k]] != 0) {
	do_add = 1;
	break;
      }
  }
  
  if (do_add)
    for (k = 0; k < num_stones; k++) {
      if (owl->goal[stones[k]] == 0) {
	if (0)
	  TRACE("Added %1m to goal.\n", stones[k]);
	owl->goal[stones[k]] = 2;
	owl->cumulative_goal[stones[k]] = 1;
      }
    }

  /* If this move captures a lunch, we add all it's direct neighbours to the
   * goal.
   */
  if (!semeai_call && lunch != NO_MOVE && board[lunch] != EMPTY) {
    int adj, adjs[MAXCHAIN];
    int k;
    adj = chainlinks(lunch, adjs);
    for (k = 0; k < adj; k++)
      if (!owl->goal[adjs[k]]) {
	mark_string(adjs[k], owl->goal, 2);
	mark_string(adjs[k], owl->cumulative_goal, 2);
      }
  }

  /* Now we handle the SAME_DRAGON_ALL_CONNECTED case. The move has
   * already been added to the goal above, so it remains to find all
   * other friendly stones in the pattern and add them too. We do that
   * by a recursive call to this function in SAME_DRAGON_CONNECTED mode.
   * This is maybe not the most elegant technique, however.
   */
  if (same_dragon == SAME_DRAGON_ALL_CONNECTED) {
    gg_assert(pattern_data != NULL);
    for (k = 0; k < pattern_data->pattern->patlen; k++) {
      int pos2;
      
      /* all the following stuff (currently) applies only at occupied cells */
      if (pattern_data->pattern->patn[k].att != ATT_O)
	continue;
      
      /* transform pattern real coordinate */
      pos2 = AFFINE_TRANSFORM(pattern_data->pattern->patn[k].offset,
			      pattern_data->ll, pattern_data->anchor);

      if (!owl->goal[pos2])
	owl_update_goal(pos2, SAME_DRAGON_CONNECTED, NO_MOVE, owl, semeai_call,
			pattern_data);
    }
  }

  if (1 && verbose)
    goaldump(owl->goal);
}


/* Computes the connected components of a the graph that is given by
 * having graph[i][j] = 1 if i and j are connected, and that has size
 * graph_size.
 *
 * This function is generic, but without having the fixed MAX_CUTS
 * array size it is ugly to write in ANSI C89 (no variably sized arrays),
 * so we leave it here for now.
 */
static int
connected_components(signed char graph[MAX_CUTS][MAX_CUTS], int graph_size,
		     signed char component[MAX_CUTS])
{
  int num_components = 0;
  int k, j;

  if (graph_size <= 0)
    return 0;

  memset(component, -1, MAX_CUTS);
  for (;;) {
    int found_one;
    /* Find unidentified string. */
    for (k = 0; k < graph_size; k++)
      if (component[k] == -1)
	break;
    if (k == graph_size)
      break; /* All are identified. */
    component[k] = num_components; /* Start new component. */
    do { /* Spread new component. */
      found_one = 0;
      for (j = k+1; j < graph_size; j++)
	if (graph[k][j] && component[j] == -1) {
	  component[j] = num_components;
	  found_one = 1;
	}
    } while (found_one);
    num_components++;
  }
  gg_assert(num_components > 0);
  return num_components;
}

/* This functions gets called after a move has been made that threatens
 * to cut the owl goal dragon. It cuts the goal if necessary, and sets it
 * to the biggest remaining component.
 */
static void
owl_test_cuts(signed char goal[BOARDMAX], int color, int cuts[MAX_CUTS])
{
  int k, j;
  signed char connected[MAX_CUTS][MAX_CUTS];
  /* int connect_move[MAX_CUTS][MAX_CUTS]; */
  int num_cuts;
  int found_cut = 0;
  SGFTree *save_sgf_dumptree = sgf_dumptree;
  int save_count_variations = count_variations;
    
  sgf_dumptree = NULL;
  count_variations = 0;

  memset(connected, 1, MAX_CUTS*MAX_CUTS);
  if (debug & DEBUG_SPLIT_OWL) {
    gprintf("Called for this goal: ");
    goaldump(goal);
    gprintf("At this position:\n");
    showboard(0);
  }

  /* Delete captured strings from list. */
  for (k = 0; k < MAX_CUTS; k++) {
    if (cuts[k] == NO_MOVE)
      break;
    if (board[cuts[k]] == EMPTY) {
      for (j = k + 1; j < MAX_CUTS; j++) {
	if (cuts[j] == NO_MOVE)
	  break;
	cuts[j-1] = cuts[j];
      }
      cuts[k] = NO_MOVE;
      k--;
    }
  }
  num_cuts = k;

  /* Test for each pair of strings in cuts[] whether it can now be
   * disconnected.
   */
  for (k = 0; k < num_cuts; k++) {
    ASSERT1(board[cuts[k]] == color, cuts[k]);
    for (j = k + 1; j < num_cuts; j++)
      if (fast_disconnect(cuts[k], cuts[j], NULL) == WIN) {
	found_cut = 1;
	connected[k][j] = 0;
	connected[j][k] = 0;
      }
  }

  if (found_cut) {
    signed char component[MAX_CUTS];
    signed char component2[BOARDMAX];
    int component_size[MAX_CUTS];
    int num_components;
    int biggest_component = -1;
    struct connection_data *conn_data;
    int c_id;
    int pos;

    /* Start by computing the connected components among the strings
     * listed in cuts[].
     */
    num_components = connected_components(connected, num_cuts, component);
    if (num_components <= 1) {
      sgf_dumptree = save_sgf_dumptree;
      count_variations = save_count_variations;
      return;
    }

    /* Now break up the goal by associating each goal stone to one of
     * the connected components.
     *
     * First we compute the connection distances from each of the
     * partial goals we have found.
     */
    memset(component2, -1, BOARDMAX);
    memset(component_size, 0, sizeof(int) * num_components);
    conn_data = malloc(sizeof(struct connection_data) * num_components);
    for (c_id = 0; c_id < num_components; c_id++) {
      signed char this_goal[BOARDMAX];
      memset(this_goal, 0, BOARDMAX);

      for (k = 0; k < num_cuts; k++)
	if (component[k] == c_id) {
	  mark_string(cuts[k], this_goal, 1);
	  mark_string(cuts[k], component2, (signed char) c_id);
	}
      init_connection_data(color, this_goal, NO_MOVE, FP(3.01),
	  		   conn_data + c_id, 1);
      spread_connection_distances(color, conn_data + c_id);
    }

    /* Now put each goal string to the component to which it has the
     * smallest distance.
     */
    for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
      int closest_dist = HUGE_CONNECTION_DISTANCE;
      int closest_component = -1;
      if (board[pos] != color || !goal[pos])
	continue;
      if (pos != find_origin(pos))
	continue;
      for (c_id = 0; c_id < num_components; c_id++) {
	if (conn_data[c_id].distances[pos] < closest_dist) {
	  closest_dist = conn_data[c_id].distances[pos];
	  closest_component = c_id;
	}
      }
      /* FIXME: What to do if no close component found? */
      if (closest_component != -1) {
	mark_string(pos, component2, (signed char) closest_component);
	component_size[closest_component] += countstones(pos);
      }
    }

    /* Now find the biggest_component. */
    {
      int biggest_size = 0;
      for (c_id = 0; c_id < num_components; c_id++)
	if (component_size[c_id] > biggest_size) {
	  biggest_size = component_size[c_id];
	  biggest_component = c_id;
	}
      gg_assert(biggest_component != -1);
    }

    /* Now delete everything except the biggest component from the goal. */
    for (pos = BOARDMIN; pos < BOARDMAX; pos++)
      if (component2[pos] != biggest_component)
	goal[pos] = 0;
    if (debug & DEBUG_SPLIT_OWL) {
      gprintf("Split dragon. Biggest component is %d (of %d).\n",
	      biggest_component, num_components);
      showboard(0);
      componentdump(component2);
    }
    free(conn_data);
  }
  sgf_dumptree = save_sgf_dumptree;
  count_variations = save_count_variations;
}

/* We update the boundary marks. The boundary mark must be
 * constant on each string. It is nonzero if the string
 * adjoins the goal dragon, or if the string includes a
 * stone played in the course of analysis. If the string
 * adjoins a live friendly dragon, the boundary mark is 2.
 */
static void
owl_update_boundary_marks(int pos, struct local_owl_data *owl)
{
  signed char boundary_mark = 0;
  int k;

  for (k = 0; k < 4; k++) {
    int pos2 = pos + delta[k];

    if (ON_BOARD(pos2) && owl->boundary[pos2] > boundary_mark)
      boundary_mark = owl->boundary[pos2];

    if (board[pos2] == owl->color
	&& dragon[pos2].color == owl->color
	&& dragon[pos2].status == ALIVE
	&& !owl->goal[pos2]
	&& !owl->neighbors[pos2])
      boundary_mark = 2;
  }

  mark_string(pos, owl->boundary, boundary_mark);
}

/* Lists the goal array. For use in GDB:
 * (gdb) set goaldump(goal).
 */

void
goaldump(const signed char goal[BOARDMAX])
{
  int pos;
  for (pos = BOARDMIN; pos < BOARDMAX; pos++)
    if (ON_BOARD(pos) && goal[pos])
      gprintf("%o%1m (%d)  ", pos, (int) goal[pos]);
  gprintf("\n");
}

void
componentdump(const signed char component[BOARDMAX])
{
  int pos;
  for (pos = BOARDMIN; pos < BOARDMAX; pos++)
    if (ON_BOARD(pos) && component[pos] != -1)
      gprintf("%o%1m (%d)  ", pos, (int) component[pos]);
  gprintf("\n");
}

/*
 * Owl attack moves are ineffective when the dragon can still live in a
 * semeai. This function tests whether an owl attack move has this problem.
 * If not, an owl attack move reason is added, otherwise we treat the
 * move as a strategic attack.
 */
static void
test_owl_attack_move(int pos, int dr, int kworm, int acode)
{
  int color = OTHER_COLOR(board[dr]);
  if (DRAGON2(dr).semeais == 0
      || DRAGON2(dr).semeai_defense_point == NO_MOVE
      || (DRAGON2(dr).semeais == 1 && semeai_move_reason_known(pos, dr))
      || acode == GAIN) {
    add_owl_attack_move(pos, dr, kworm, acode);
    DEBUG(DEBUG_OWL, "owl: %1m attacks %1m (%s) at move %d\n",
	  pos, dr, result_to_string(DRAGON2(dr).owl_attack_code),
	  movenum+1);
  }
  else {
    int dr2 = DRAGON2(dr).semeai_defense_target;
    int semeai_result, certain;
    int save_verbose = verbose;
    if (verbose > 0)
      verbose--;
    owl_analyze_semeai_after_move(pos, color, dr, dr2, &semeai_result,
				  NULL, NULL, 1, &certain, 0);
    verbose = save_verbose;
    if (certain >= DRAGON2(dr).semeai_defense_certain
	&& (semeai_result >= REVERSE_RESULT(acode))) {
      /* Demote the move reasons. */
      DEBUG(DEBUG_OWL, "owl: %1m ineffective owl attack on %1m (can live in semeai with %1m)\n", pos, dr, dr2);
      add_strategical_attack_move(pos, dr);
    }
    else {
      add_owl_attack_move(pos, dr, kworm, acode);
      DEBUG(DEBUG_OWL, "owl: %1m attacks %1m (%s) at move %d\n",
	    pos, dr, result_to_string(DRAGON2(dr).owl_attack_code),
	    movenum+1);
    }
  }
}

/* Add owl move reasons. This function should be called once during
 * genmove. It has to be called after semeai_move_reasons().
 */

void
owl_reasons(int color)
{
  int pos;

  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (!IS_STONE(board[pos])
        || dragon[pos].origin != pos)
      continue;
    
    if (dragon[pos].status == CRITICAL
	&& DRAGON2(pos).owl_attack_point != NO_MOVE) {
      if (board[pos] == color) {
	if (DRAGON2(pos).owl_defense_point != NO_MOVE) {
	  if (DRAGON2(pos).owl_defense_code == LOSS) {
	    add_loss_move(DRAGON2(pos).owl_defense_point, pos,
			  DRAGON2(pos).owl_defense_kworm);
	    DEBUG(DEBUG_OWL, "owl: %1m defends %1m with loss at move %d\n",
		  DRAGON2(pos).owl_defense_point, pos, movenum+1);
	  }
	  else {
	    add_owl_defense_move(DRAGON2(pos).owl_defense_point, pos,
				 DRAGON2(pos).owl_defense_code);
	    DEBUG(DEBUG_OWL, "owl: %1m defends %1m at move %d\n",
		  DRAGON2(pos).owl_defense_point, pos, movenum+1);
	  }
	}
      }
      else { /* opponent's dragon */
	/* We don't want to add this move reason if the attacker
	 * dies because the victim only formed a nakade shape.
	 *
	 * FIXME: This code overlaps heavily with some code in
	 *	  examine_move_safety() in move_reasons.c. The caching
	 *	  scheme should minimize the performance hit, but of course
	 *	  it's unfortunate to have the code duplication.
	 */
	int move = DRAGON2(pos).owl_attack_point;
	
	/* No worries if we catch something big. */
	if (dragon[pos].effective_size < 8) {
	  /* Look through the neighbors of the victim for dragons of
	   * our color. If we find at least one being thought alive
	   * everything is ok. Otherwise we keep track of the
	   * largest one for further examination.
	   */
	  int largest = 0;
	  int k;
	  int bpos = NO_MOVE;
	  int kworm = NO_MOVE;
	  int safe = 0;
	  for (k = 0; k < DRAGON2(pos).neighbors; k++) {
	    int d = DRAGON2(pos).adjacent[k];
	    if (DRAGON(d).color == color) {
	      if (DRAGON(d).status == ALIVE) {
		safe = 1;
		break;
	      }
	      if (DRAGON(d).size > largest) {
		bpos = dragon2[d].origin;
		largest = DRAGON(d).size;
	      }
	    }
	  }
	  
	  /* It may occasionally happen that no neighbor of our
	   * color was found. Assume safe in that case.
	   */
	  if (bpos == NO_MOVE)
	    safe = 1;
	  
	  /* If not yet thought safe, ask the owl code whether the
	   * owl attack defends the (largest) attacker.
	   */
	  if (!safe && owl_does_defend(move, bpos, &kworm) != WIN) {
	    DEBUG(DEBUG_OWL,
		  "owl: %1m attacks %1m at move %d, but the attacker dies.\n",
		  move, pos, movenum+1);
	    DRAGON2(pos).safety = INESSENTIAL;
	    continue;
	  }
	}

	/* If we've reached this far, it only remains to check the move
	 * against semeai complications. */
	test_owl_attack_move(move, pos, DRAGON2(pos).owl_attack_kworm,
	    		    DRAGON2(pos).owl_attack_code);
      }
    }
    else if (DRAGON2(pos).owl_status == DEAD
	     && DRAGON2(pos).owl_threat_status == CAN_THREATEN_DEFENSE) {
      if (board[pos] == color 
	  && DRAGON2(pos).owl_defense_point != NO_MOVE) {
	add_owl_defense_threat_move(DRAGON2(pos).owl_defense_point, pos, WIN);
	DEBUG(DEBUG_OWL, "owl: %1m threatens to defend %1m at move %d\n", 
	      DRAGON2(pos).owl_defense_point, pos, movenum+1);
      }
      if (board[pos] == color
	    && DRAGON2(pos).owl_second_defense_point != NO_MOVE
	  && is_legal(DRAGON2(pos).owl_second_defense_point, color)) {
	add_owl_defense_threat_move(DRAGON2(pos).owl_second_defense_point,
				    pos, WIN);
	DEBUG(DEBUG_OWL, "owl: %1m threatens to defend %1m at move %d\n", 
	      DRAGON2(pos).owl_second_defense_point, pos, movenum+1);
      }

      /* If the opponent can threaten to live, an attacking
       * move gets a small value to make sure it's really dead.
       */
      if (board[pos] == OTHER_COLOR(color)
	  && DRAGON2(pos).owl_threat_status == CAN_THREATEN_DEFENSE
	  && DRAGON2(pos).owl_attack_point != NO_MOVE) {
	add_owl_prevent_threat_move(DRAGON2(pos).owl_attack_point, pos);
	DEBUG(DEBUG_OWL, "owl: %1m prevents a threat against %1m at move %d\n",
	      DRAGON2(pos).owl_attack_point, pos, movenum+1);
      }
    }
    else if (DRAGON2(pos).owl_status == ALIVE) {
      if (board[pos] == OTHER_COLOR(color)
	  && DRAGON2(pos).owl_threat_status == CAN_THREATEN_ATTACK) {
	if (DRAGON2(pos).owl_attack_point != NO_MOVE) {
	  add_owl_attack_threat_move(DRAGON2(pos).owl_attack_point, pos, WIN);
	  DEBUG(DEBUG_OWL, "owl: %1m threatens %1m at move %d\n",
		DRAGON2(pos).owl_attack_point, pos, movenum+1);
	}
	if (DRAGON2(pos).owl_second_attack_point != NO_MOVE
	    && is_legal(DRAGON2(pos).owl_second_attack_point, color)) {
	  add_owl_attack_threat_move(DRAGON2(pos).owl_second_attack_point, pos,
				     WIN);
	  DEBUG(DEBUG_OWL, "owl: %1m threatens %1m at move %d\n",
		DRAGON2(pos).owl_second_attack_point, pos, movenum+1);
	}
      }
      else if (board[pos] == OTHER_COLOR(color)
	       && DRAGON2(pos).owl_attack_point != NO_MOVE
	       && DRAGON2(pos).owl_attack_code == GAIN) {
	add_owl_attack_move(DRAGON2(pos).owl_attack_point, pos,
		            DRAGON2(pos).owl_attack_kworm, GAIN);
	DEBUG(DEBUG_OWL, "owl: %1m attacks %1m with gain at move %d\n", 
	      DRAGON2(pos).owl_attack_point, pos, movenum+1);
      }
      else if (board[pos] == color
	       && DRAGON2(pos).owl_defense_point != NO_MOVE
	       && DRAGON2(pos).owl_defense_code == LOSS) {
	add_loss_move(DRAGON2(pos).owl_defense_point, pos,
		      DRAGON2(pos).owl_defense_kworm);
	DEBUG(DEBUG_OWL, "owl: %1m defends %1m with loss at move %d\n",
	      DRAGON2(pos).owl_defense_point, pos, movenum+1);
      }
      else if (board[pos] == color
	       && DRAGON2(pos).owl_attack_point != NO_MOVE
	       && DRAGON2(pos).owl_attack_code == GAIN
	       && DRAGON2(pos).owl_defense_code == WIN
	       && DRAGON2(pos).owl_defense_point != NO_MOVE) {
	add_owl_defense_move(DRAGON2(pos).owl_defense_point, pos,
			     DRAGON2(pos).owl_defense_code);
	DEBUG(DEBUG_OWL, "owl: %1m defends %1m against possible loss at move %d\n",
	      DRAGON2(pos).owl_defense_point, pos, movenum+1);

      }
      /* The owl code found the friendly dragon alive, but was uncertain,
       * and an extra point of defense was found, so this might
       * be a good place to play.
       */
      else if (board[pos] == color
	       && !DRAGON2(pos).owl_attack_certain
	       && DRAGON2(pos).owl_defense_certain
	       && ON_BOARD(DRAGON2(pos).owl_defense_point)) {
	add_owl_uncertain_defense_move(DRAGON2(pos).owl_defense_point, pos);
	DEBUG(DEBUG_OWL, 
	      "owl: %1m defends the uncertain dragon at %1m at move %d\n",
	      DRAGON2(pos).owl_defense_point, pos, movenum+1);
      }
    }

    /* The owl code found the dragon dead, but was uncertain,
     * and an extra point of attack was found, so this might
     * be a good place to play.
     */
    else if (DRAGON2(pos).owl_status == DEAD
	     && board[pos] == OTHER_COLOR(color)
	     && !DRAGON2(pos).owl_attack_certain
	     && ON_BOARD(DRAGON2(pos).owl_attack_point)) {
      add_owl_uncertain_defense_move(DRAGON2(pos).owl_attack_point, pos);
      DEBUG(DEBUG_OWL,
	    "owl: %1m might defend the uncertain dragon at %1m at move %d\n",
	    DRAGON2(pos).owl_attack_point, pos, movenum+1);
    }
  }
}

/* Use the owl code to determine whether the move at (move) makes
 * the dragon at (target) owl safe. This is used to test whether
 * tactical defenses are strategically viable and whether a vital eye
 * point does kill an owl critical dragon. 
 *
 * Should be called only when stackp==0.
 */

int
owl_does_defend(int move, int target, int *kworm)
{
  int color = board[target];
  int result = 0;
  struct local_owl_data *owl;
  int reading_nodes_when_called = get_reading_node_counter();
  int tactical_nodes;
  int origin;
  int acode;
  int wpos = NO_MOVE;
  int wid = MAX_GOAL_WORMS;
  double start = 0.0;

  if (debug & DEBUG_OWL_PERFORMANCE)
    start = gg_cputime();

  if (worm[target].unconditional_status == DEAD)
    return 0;

  origin = dragon[target].origin;
  TRACE("owl_does_defend %1m %1m(%1m)\n", move, target, origin);

  if (search_persistent_owl_cache(OWL_DOES_DEFEND, move, target, 0,
				  &result, kworm, NULL, NULL))
    return result;

  if (trymove(move, color, "owl_does_defend", target)) {
    /* Check if a compatible owl_attack() is cached. */
    if (search_persistent_owl_cache(OWL_ATTACK, origin, 0, 0,
				    &result, NULL, kworm, NULL)) {
      popgo();
      return REVERSE_RESULT(result);
    }
    
    /*
     * FIXME: (move) will be added to the goal dragon although we
     * do not know whether it is really connected.
     */
    init_owl(&owl, target, NO_MOVE, move, 1, NULL);
    prepare_goal_list(target, owl, owl_goal_worm, &goal_worms_computed,
		      kworm, 0);
    acode = do_owl_attack(target, NULL, &wid, owl, 0);
    finish_goal_list(&goal_worms_computed, &wpos, owl_goal_worm, wid);
    result = REVERSE_RESULT(acode);
    popgo();
  }
  else
    return 0;  /* Don't cache anything in this case. */

  tactical_nodes = get_reading_node_counter() - reading_nodes_when_called;

  DEBUG(DEBUG_OWL_PERFORMANCE,
	"owl_does_defend %1m %1m(%1m), result %d (%d, %d nodes, %f seconds)\n",
	move, target, origin, result, local_owl_node_counter,
	tactical_nodes, gg_cputime() - start);

  store_persistent_owl_cache(OWL_DOES_DEFEND, move, target, 0,
			     result, wpos, 0, 0,
			     tactical_nodes, owl->goal, board[target]);

  if (kworm)
    *kworm = wpos;
  return result;
}


/* Use the owl code to determine whether the dragon at (target) is owl
 * safe after an own move at (move). This is used to detect
 * blunders. In case the dragon is not safe, it also tries to find a
 * defense point making (target) safe in a later move.
 *
 * Should be called only when stackp==0.
 */

int
owl_confirm_safety(int move, int target, int *defense_point, int *kworm)
{
  int color = board[target];
  int result = 0;
  struct local_owl_data *owl;
  int reading_nodes_when_called = get_reading_node_counter();
  int tactical_nodes;
  int origin;
  int defense = 0;
  double start = 0.0;
  int acode;
  int wpos = NO_MOVE;
  int wid = MAX_GOAL_WORMS;

  if (debug & DEBUG_OWL_PERFORMANCE)
    start = gg_cputime();

  if (worm[target].unconditional_status == DEAD)
    return 0;

  origin = dragon[target].origin;
  TRACE("owl_confirm_safety %1m %1m(%1m)\n", move, target, origin);

  if (search_persistent_owl_cache(OWL_CONFIRM_SAFETY, move, target, 0,
				  &result, defense_point, kworm, NULL))
    return result;

  if (trymove(move, color, "owl_confirm_safety", target)) {
    /* Check if a compatible owl_attack() is cached. */
    if (search_persistent_owl_cache(OWL_ATTACK, origin, 0, 0,
				    &result, defense_point, kworm, NULL)) {
      popgo();
      if (result == 0)
	return WIN;
      else if (result == GAIN)
	return LOSS;
      else
	return 0;
    }
    
    init_owl(&owl, target, NO_MOVE, move, 1, NULL);
    prepare_goal_list(target, owl, owl_goal_worm, &goal_worms_computed,
		      kworm, 0);
    acode = do_owl_attack(target, &defense, &wid, owl, 0);
    finish_goal_list(&goal_worms_computed, &wpos, owl_goal_worm, wid);
    if (acode == 0)
      result = WIN;
    else if (acode == GAIN)
      result = LOSS;
    popgo();
  }
  else
    return 0;  /* Don't cache anything in this case. */

  tactical_nodes = get_reading_node_counter() - reading_nodes_when_called;

  DEBUG(DEBUG_OWL_PERFORMANCE,
	"owl_confirm_safety %1m %1m(%1m), result %d %1m (%d, %d nodes, %f seconds)\n",
	move, target, origin, result, defense,
	local_owl_node_counter, tactical_nodes,
	gg_cputime() - start);

  store_persistent_owl_cache(OWL_CONFIRM_SAFETY, move, target, 0,
			     result, defense, wpos, 0,
			     tactical_nodes, owl->goal, board[target]);

  if (defense_point)
    *defense_point = defense;
  if (kworm)
    *kworm = wpos;

  return result;
}


/* Use the owl code to determine whether the attack move at (move) of
 * the dragon (target) is effective, i.e. whether it kills the stones.
 *
 * Should be called only when stackp==0.
 */

int
owl_does_attack(int move, int target, int *kworm)
{
  int color = board[target];
  int other = OTHER_COLOR(color);
  int result = 0;
  struct local_owl_data *owl;
  int reading_nodes_when_called = get_reading_node_counter();
  int tactical_nodes;
  int origin;
  int dcode;
  int wpos = NO_MOVE;
  int wid = MAX_GOAL_WORMS;
  double start = 0.0;

  if (debug & DEBUG_OWL_PERFORMANCE)
    start = gg_cputime();

  if (worm[target].unconditional_status == ALIVE)
    return 0;

  origin = dragon[target].origin;
  TRACE("owl_does_attack %1m %1m(%1m)\n", move, target, origin);

  if (search_persistent_owl_cache(OWL_DOES_ATTACK, move, target, 0,
				  &result, kworm, NULL, NULL))
    return result;

  /* FIXME: We want to do this after the trymove(), but currently
   * owl_mark_dragon() may crash if the trymove() happens to remove
   * some stones of the goal dragon from the board.
   */
#if 1
  init_owl(&owl, target, NO_MOVE, NO_MOVE, 1, NULL);
#endif

  if (trymove(move, other, "owl_does_attack", target)) {
    /* Check if a compatible owl_defend() is cached. */
    if (search_persistent_owl_cache(OWL_DEFEND, origin, 0, 0,
				    &result, NULL, kworm, NULL)) {
      popgo();
      return REVERSE_RESULT(result);
    }

#if 0
    local_owl_node_counter = 0;
    owl->lunches_are_current = 0;
    owl_mark_dragon(target, NO_MOVE, owl);
#endif
    owl_update_boundary_marks(move, owl);
#if 0
    compute_owl_escape_values(owl);
#endif
    /* FIXME: Should also check if part of the dragon was captured,
     *        like do_owl_attack() does.
     */
    if (board[target] == EMPTY)
      dcode = 0;
    else {
      prepare_goal_list(target, owl, owl_goal_worm, &goal_worms_computed,
	                 kworm, 0);
      dcode = do_owl_defend(target, NULL, &wid, owl, 0);
      finish_goal_list(&goal_worms_computed, &wpos, owl_goal_worm, wid);
    }
    result = REVERSE_RESULT(dcode);
    owl->lunches_are_current = 0;
    popgo();
  }
  else
    return 0;  /* Don't cache anything in this case. */

  tactical_nodes = get_reading_node_counter() - reading_nodes_when_called;

  DEBUG(DEBUG_OWL_PERFORMANCE,
	"owl_does_attack %1m %1m(%1m), result %d (%d, %d nodes, %f seconds)\n",
	move, target, origin, result, local_owl_node_counter,
	tactical_nodes, gg_cputime() - start);

  store_persistent_owl_cache(OWL_DOES_ATTACK, move, target, 0,
			     result, wpos, 0, 0,
			     tactical_nodes, owl->goal, board[target]);

  if (kworm)
    *kworm = wpos;
  return result;
}


/* Use the owl code to determine whether connecting the two dragons
 * (target1) and (target2) by playing at (move) results in a living
 * dragon. Should be called only when stackp==0.
 */

int
owl_connection_defends(int move, int target1, int target2)
{
  int color = board[target1];
  int result = 0;
  int reading_nodes_when_called = get_reading_node_counter();
  int tactical_nodes;
  double start = 0.0;
  struct local_owl_data *owl;

  if (debug & DEBUG_OWL_PERFORMANCE)
    start = gg_cputime();

  ASSERT1(board[target2] == color, target2);
  TRACE("owl_connection_defends %1m %1m %1m\n", move, target1, target2);

  if (worm[target1].unconditional_status == DEAD)
    return 0;
  if (worm[target2].unconditional_status == DEAD)
    return 0;

  if (search_persistent_owl_cache(OWL_CONNECTION_DEFENDS, move, target1,
				  target2, &result, NULL, NULL, NULL))
    return result;

  init_owl(&owl, target1, target2, NO_MOVE, 1, NULL);

  if (trymove(move, color, "owl_connection_defends", target1)) {
    owl_update_goal(move, SAME_DRAGON_MAYBE_CONNECTED, NO_MOVE, owl, 0, NULL);
    if (!do_owl_attack(move, NULL, NULL, owl, 0))
      result = WIN;
    owl->lunches_are_current = 0;
    popgo();
  }
  tactical_nodes = get_reading_node_counter() - reading_nodes_when_called;
  
  DEBUG(DEBUG_OWL_PERFORMANCE,
	"owl_conn_defends %1m %1m %1m, result %d (%d, %d nodes, %f seconds)\n",
	move, target1, target2, result, local_owl_node_counter,
	tactical_nodes, gg_cputime() - start);

  store_persistent_owl_cache(OWL_CONNECTION_DEFENDS, move, target1, target2,
			     result, 0, 0, 0, tactical_nodes,
			     owl->goal, color);

  return result;
}


/* This function attempts to make a list of dead strings
 * which may be relevant to the life of the goal dragon.
 * Such strings are called owl lunches. They are ignored
 * (treated as invisible) during the running of make_domains.
 *
 * In certain cases we also need to identify tactically safe strings
 * which should be included in the eyespace because they can't live
 * independently without capturing the surrounding stones. We call
 * such stones INESSENTIAL. The characteristics of those are described
 * in the comments to the test_lunch_essentiality() function below.
 */

static void
owl_find_lunches(struct local_owl_data *owl)
{
  int k;
  int pos;
  int lunches = 0;
  int prevlunch;
  int lunch;
  int acode;
  int apos;
  int dcode;
  int dpos;
  int color = owl->color;
  int other = OTHER_COLOR(color);
  signed char already_checked[BOARDMAX];

  SGFTree *save_sgf_dumptree = sgf_dumptree;
  int save_count_variations = count_variations;

  sgf_dumptree = NULL;
  count_variations = 0;
  for (prevlunch = 0; prevlunch < MAX_LUNCHES; prevlunch++)
    owl->lunch[prevlunch] = NO_MOVE;
  memset(owl->inessential, 0, sizeof(owl->inessential));

  memset(already_checked, 0, sizeof(already_checked));
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (board[pos] == color && owl->goal[pos]) {
      /* Loop over the eight neighbors. */
      for (k = 0; k < 8; k++) {
	int pos2 = pos + delta[k];
	int valid_lunch;
	int essential;

	/* If the immediate neighbor is empty, we look two steps away. */
	if (k < 4 && board[pos2] == EMPTY)
	  pos2 += delta[k];

	if (board[pos2] != other)
	  continue;

	lunch = find_origin(pos2);
	if (already_checked[lunch] || owl->inessential[lunch])
	  continue;
	already_checked[lunch] = 1;

	attack_and_defend(lunch, &acode, &apos, &dcode, &dpos);

	valid_lunch = 0;
	if (acode != 0
	    && (!liberty_of_goal(apos, owl) || safe_move(apos, color)))
	  valid_lunch = 1;

	essential = 0;
	if (valid_lunch)
	  essential = test_unstable_lunch_essentiality(lunch, dcode,
						       apos, dpos, color);

	if (!essential) {
	  int num_stones;
	  int stones[MAX_BOARD * MAX_BOARD];
	  essential = test_lunch_essentiality(lunch, owl->goal, color,
					      &num_stones, stones);
	  if (!essential && valid_lunch) {
	    /* If the lunch is part of a larger superstring it cannot
	     * be trusted as inessential.
	     */
	    if (countstones(lunch) != num_stones)
	      essential = 1;
	  }

	  if (!essential) {
	    int r;
	    TRACE("Inessential string found at %1m.\n", lunch);
	    for (r = 0; r < num_stones; r++)
	      owl->inessential[stones[r]] = 1;
	  }
	}

	if (valid_lunch && essential) {
	  owl->lunch[lunches] = lunch;
	  owl->lunch_attack_code[lunches]  = acode;
	  owl->lunch_attack_point[lunches] = apos;
	  owl->lunch_defend_code[lunches]  = dcode;
	  ASSERT1(board[apos] == EMPTY, lunch);
	  if (dcode != 0) {
	    owl->lunch_defense_point[lunches] = dpos;
	    ASSERT1(board[dpos] == EMPTY, lunch);
	  }
	  else
	    owl->lunch_defense_point[lunches] = NO_MOVE;
	  lunches++;
	  if (lunches == MAX_LUNCHES) {
	    sgf_dumptree = save_sgf_dumptree;
	    count_variations = save_count_variations;
	    owl->lunches_are_current = 1;
	    return;
	  }
	}
      }
    }
  }

  owl->lunches_are_current = 1;
  sgf_dumptree = save_sgf_dumptree;
  count_variations = save_count_variations;
}


/*
 * This function determines whether tactically safe strings should be
 * included in the eyespaces, e.g. in this position:
 *
 * -------
 * OXXOOXO
 * OX.O.XO
 * OXX.XXO
 * OOXXXOO
 * .OOOOO.
 *
 * The three O stones cannot be captured, but they can't live
 * independently without capturing the surrounding stones. We call
 * such stones INESSENTIAL and identify them by the condition that for
 * each liberty of the corresponding superstring, the following must
 * hold:
 *
 * 1. At least one neighbor of the liberty is the goal dragon.
 * 2. No neighbor of the liberty is the same color as the tested string,
 *    unless part of the same superstring.
 * 3. No neighbor of the liberty of the same color as the goal dragon
 *    does not belong to the goal dragon.
 * 4. No neighbor of the liberty belonging to the goal dragon can be
 *    tactically captured.
 *
 * There is a weakness with this characterization though, which can be
 * seen in this position:
 *
 * --------
 * OX..OOX.
 * OX.X.XOO
 * OX.XX.O.
 * O.XXOOO.
 * .OOOO...
 *
 * The two O stones intruding in X's eyespace cannot be tactically
 * captured and their liberties satisfy the requirements above. Still
 * it doesn't make any sense to count those stones as
 * inessential. Therefore we add another requirement on the stones
 * themself:
 *
 * 5. No neighbor of the stones does not belong to the goal or can be
 *    tactically captured.
 *
 * A second weakness can be noticed in this position:
 *
 * |OOOO.
 * |XXXO.
 * |O.XOO
 * |OXXXO
 * |.O.XO
 * +-----
 *
 * The white stones in the corner should qualify as inessential but
 * the corner liberty doesn't satisfy requirement 1. Therefore we add
 * an alternative requirement:
 *
 * 1b. The liberty is a topologically false eye with respect to the
 *     goal dragon.
 *
 * This is not quite good enough though, as shown in this position:
 *
 * ----------
 * OX.X.OO...
 * OXX.OOX.O.
 * O.XXXXX.O.
 * OOOOOOOOO.
 *
 * The four O stones are regarded as inessential after inclusion of
 * rule 1b, which is clearly inappropriate. To solve this problem we
 * modify the rule:
 *
 * 1b'. The liberty is a topologically false eye with respect to the
 *      goal dragon and is adjacent to no empty vertex.
 */

static int
test_lunch_essentiality(int lunch, signed char *goal, int color,
			int *num_stones, int *stones)
{
  /* Test for inessentiality. */
  int adj;
  int adjs[MAXCHAIN];
  int liberties;
  int libs[MAXLIBS];
  int r;
  int superstring[BOARDMAX];

  /* First check the neighbors of the string. */
  adj = chainlinks(lunch, adjs);
  for (r = 0; r < adj; r++)
    if (!goal[adjs[r]] || attack(adjs[r], NULL) != 0)
      return 1;

  find_superstring_stones_and_liberties(lunch, num_stones, stones,
					&liberties, libs, 0);

  memset(superstring, 0, sizeof(superstring));
  for (r = 0; r < *num_stones; r++)
    superstring[stones[r]] = 1;

  for (r = 0; r < liberties; r++) {
    int bpos = libs[r];
    int goal_found = 0;
    int s;

    for (s = 0; s < 4; s++) {
      int cpos = bpos + delta[s];

      if (!ON_BOARD(cpos))
	continue;

      if (board[cpos] == color) {
	if (attack(cpos, NULL) != 0)
	  return 1;
	else if (goal[cpos])
	  goal_found = 1;
	else
	  return 1;
      }
      else if (board[cpos] == OTHER_COLOR(color) && !superstring[cpos])
	return 1;
    }

    if (!goal_found) {
      /* Requirement 1 not satisfied. Test requirement 1b.
       * N.B. This is a simplified topological eye test.
       * The simplification may be good, bad, or neutral.
       */
      int off_board = 0;
      int diagonal_goal = 0;
      for (s = 4; s < 8; s++) {
	if (!ON_BOARD(bpos + delta[s]))
	  off_board++;
	else if (goal[bpos + delta[s]])
	  diagonal_goal++;
      }
      if (diagonal_goal + (off_board >= 2) < 2)
	return 1;
      else {
	/* Check that the liberty is adjacent to no empty
	 * vertex, as required by 1b'.
	 */
	for (s = 0; s < 4; s++)
	  if (board[bpos + delta[s]] == EMPTY)
	    return 1;
      }
    }
  }

  return 0;
}


/* Lunch essentiality is mainly relevant for tactically safe stones.
 * Tactically dead stones are included in eyespaces anyway and
 * tactically critical stones need to be captured in order for the
 * eyespace to exist at all. The latter is not entirely true,
 * however.
 *
 * In this kind of position, based on STS-RV_1:65,
 *
 * .OOOOOOO
 * .OXXXXOO
 * .OX..XXO
 * OOX*O.XO
 * OXXXXXXO
 * OX..OOOX
 * XOOO.OOX
 * XXO.X.OX
 * .XOOXOOX
 * .XOOOOOX
 * .XXXXXXX
 *
 * it is of vital importance not to play inside your own eye. Thus it
 * is critical that the stones inside the eyes are either tactically
 * dead or considered as inessential lunches so that the eyes are
 * really found as eyes.
 *
 * At low depths, the white nakade stone is tactically dead since it
 * can't obtain more than three liberties and black has sufficient
 * outer liberties to be able to capture. At sufficiently high depths
 * it is considered tactically safe because three liberties are
 * enough. However, at exactly the right depth it is tactically
 * unstable because four liberties are required so that it can be
 * attacked but after trying * as defense stackp is one higher and
 * the three liberties it retains suffice it can no longer be
 * attacked. It is still true that it cannot live independently
 * without killing the surrounding stones so it does match the
 * definition of essentiality and test_lunch_essentiality() will
 * correctly detect this, if called.
 *
 * The problem with calling test_lunch_essentiality() for any
 * tactically unstable lunch is that it will give too many false
 * positives, typically for lunches which can defend themselves by
 * somehow breaking out of the eyespace.
 *
 * What we would need is a way to distinguish between tactical
 * defenses which are successful solely by obtaining liberties inside
 * the eyespace and defenses which manage to connect or break out to
 * the outside. Unfortunately the tactical reading functions don't
 * provide any support for that so the function below implements
 * heuristics guessing when a tactically unstable lunch cannot be
 * considered inessential. In addition to this screening it must also
 * pass the test_lunch_essentiality() function before it can count as
 * inessential.
 */

static int
test_unstable_lunch_essentiality(int lunch, int dcode, int apos,
				 int dpos, int color)
{
  int other = OTHER_COLOR(color);
  int k;

  if (dcode != WIN || dpos == PASS_MOVE)
    return 1;

  if (!liberty_of_string(dpos, lunch)
      && !liberty_of_string(apos, lunch))
    return 1;

  if (!liberty_of_string(dpos, lunch))
    return 0;

  if (approxlib(dpos, other, 5, NULL) < countlib(lunch))
    return 1;

  for (k = 0; k < 8; k++) {
    int pos2 = dpos + delta[k];
    if (board[pos2] == other
	&& !same_string(pos2, lunch))
      return 1;
  }

  for (k = 0; k < 4; k++) {
    int pos2 = dpos + delta[k];
    if (board[pos2] == EMPTY
	&& !liberty_of_string(pos2, lunch)) {
      int n;
      for (n = 0; n < 4; n++) {
	int pos3 = pos2 + delta[n];
	if (board[pos3] == EMPTY
	    && !liberty_of_string(pos3, lunch)) {
	  return 1;
	}
      }
    }
  }

  return 0;
}

/* Try to improve the move to attack a lunch. Essentially we try to avoid
 * unsafe moves when there are less risky ways to attack.
 *
 * This function also improves lunch attack point in a special case when
 * we capture a one- or two-stone lunch on the first line. If we eat it
 * with a first line move, there is a huge risk we'll end up with a false
 * eye. Therefore, we move the attack to the second line when it works.
 *
 *   .*OO	.*OOO	    .*OOOO
 *   .,XO	.,X.O	    .,XX.O
 *   ----	-----	    ------
 *
 * In all these position the attack point is moved from ',' to '*'.
 */
static int
improve_lunch_attack(int lunch, int attack_point)
{
  int color = OTHER_COLOR(board[lunch]);
  int defense_point;
  int k;

  if (safe_move(attack_point, color)) {
    if (is_edge_vertex(lunch)
	&& is_edge_vertex(attack_point)
	&& neighbor_of_string(attack_point, lunch)) {
      int stones = countstones(lunch);
      int libs[2];

      if (stones == 1
	  || (stones == 2
	      && findlib(lunch, 2, libs) == 2
	      && is_edge_vertex(libs[0])
	      && is_edge_vertex(libs[1]))) {
	for (k = 0; k < 4; k++) {
	  int apos = attack_point + delta[k];
	  if (!ON_BOARD(attack_point - delta[k]) && board[apos] == EMPTY) {
	    if (does_attack(apos, lunch) && safe_move(apos, color)
		&& !defend_against(attack_point, color, apos)) {
	      return apos;
	    }
	    break;
	  }
	}
      }
    }

    return attack_point;
  }

  for (k = 0; k < 4; k++) {
    int pos = attack_point + delta[k];
    if (board[pos] == color
	&& attack(pos, NULL)
	&& find_defense(pos, &defense_point)
	&& defense_point != NO_MOVE
	&& does_attack(defense_point, lunch)) {
      TRACE("Moved attack of lunch %1m from %1m to %1m.\n",
	    lunch, attack_point, defense_point);
      return defense_point;
    }
  }
  
  return attack_point;
}

/* Try to improve the move to defend a lunch.
 *
 * An example where this is useful is the position below, where the
 * defense of A is moved from b to c. This is a possible variation in
 * ld_owl:182.
 *
 * ...X..|      ...X..|
 * ...X..|	...Xc.|
 * ..XXO.|	..XXOb|
 * XXXOOX|	XXXOOA|
 * XOOOX.|	XOOOX.|
 * .XOX.X|	.XOX.X|
 * ------+	------+
 */
static int
improve_lunch_defense(int lunch, int defense_point)
{
  int color = board[lunch];
  int k;
  
  for (k = 0; k < 4; k++) {
    int pos = defense_point + delta[k];
    if (board[pos] == OTHER_COLOR(color)
	&& countlib(pos) == 2) {
      int libs[2];
      int pos2;
      
      findlib(pos, 2, libs);
      if (libs[0] == defense_point)
	pos2 = libs[1];
      else
	pos2 = libs[0];

      if (accuratelib(pos2, color, MAXLIBS, NULL)
	  > accuratelib(defense_point, color, MAXLIBS, NULL)
	  && does_defend(pos2, lunch)) {
	TRACE("Moved defense of lunch %1m from %1m to %1m.\n",
	      lunch, defense_point, pos2);
	return pos2;
      }
    }
  }
  
  return defense_point;
}


/* Wrapper for make domains. The second set of owl data is optional.
 * Use a null pointer if it is not needed. Otherwise, make_domains
 * is run separately for the two owl data, but information about
 * tactically dead lunches is used from *both* sources through
 * the owl_lively() calls.
 */

static void
owl_make_domains(struct local_owl_data *owla, struct local_owl_data *owlb)
{
  /* We need to set this so that owl_lively() can be used. */
  struct eye_data *black_eye = NULL;
  struct eye_data *white_eye = NULL;
  
  current_owl_data = owla;
  other_owl_data = owlb;

  if (!owla->lunches_are_current)
    owl_find_lunches(owla);
  if (owla->color == BLACK)
    black_eye = owla->my_eye;
  else
    white_eye = owla->my_eye;
  
  if (owlb) {
    gg_assert(owla->color == OTHER_COLOR(owlb->color));
    if (!owlb->lunches_are_current)
      owl_find_lunches(owlb);
  }
  make_domains(black_eye, white_eye, 1);
}

/* True unless (pos) is EMPTY or occupied by a lunch for the goal dragon.  
 * Used during make_domains (see optics.c: lively macro). A ``lively''
 * worm is one that might be alive, hence cannot be ignored in 
 * determining eye spaces.
 */

int 
owl_lively(int pos)
{
  int origin;
  int lunch;
  ASSERT_ON_BOARD1(pos);

  if (board[pos] == EMPTY)
    return 0;
  origin = find_origin(pos);

  /* When reading a semeai there is a second set of owl data to consider.
   * Strings of the second owl are considered lively no matter what,
   * since declaring such a string dead prematurely can prevent the
   * semeai code from finishing its job.
   *
   * On the other hand a friendly string which is a lunch of the
   * other dragon and can't be saved is not lively.
   */
  if (other_owl_data) {

    if (include_semeai_worms_in_eyespace && other_owl_data->goal[pos])
      return 0;
    
    if (other_owl_data->goal[pos] && !semeai_trust_tactical_attack(pos))
      return 1;
    /* FIXME: Shouldn't we check other_owl_data->inessential[origin] here? */
    for (lunch = 0; lunch < MAX_LUNCHES; lunch++)
      if (other_owl_data->lunch[lunch] == origin
	  && other_owl_data->lunch_defense_point[lunch] == NO_MOVE)
	return 0;
  }

  /* Inessential stones are not lively. */
  if (current_owl_data->inessential[origin])
    return 0;

  /* Lunches that can't be saved are dead, so don't report them as lively. */
  for (lunch = 0; lunch < MAX_LUNCHES; lunch++)
    if (current_owl_data->lunch[lunch] == origin
	&& current_owl_data->lunch_defense_point[lunch] == NO_MOVE)
      return 0;

  return 1;
}


/* Caching version of safe_move for the callback. This function has
 * its own cache, separate from the global safe move cache. Note that
 * since the cache is reset by owl_shapes before starting pattern
 * matching, and since (unlike safe_move) this function is always
 * called from the same place in owl_shapes_callback, the color will
 * be the same each time it is called. So there is no need to have
 * separate caches for B and W.
 */

static int
owl_safe_move(int move, int color)
{
  int acode, safe = 0;

  if (trymove(move, color, "owl_safe_move", 0)) {
    acode = attack(move, NULL);
    if (acode != WIN)
      safe = 1;
    else
      safe = 0;
    popgo();
  }
  current_owl_data->safe_move_cache[move] = safe+1;
  return safe;
}
  

/* This function, called when stackp==0, returns true if capturing
 * the string at (str) results in a live group.
 */

#define MAX_SUBSTANTIAL_LIBS 10

int
owl_substantial(int str)
{
  int k;
  int libs[MAX_SUBSTANTIAL_LIBS + 1];
  int liberties = findlib(str, MAX_SUBSTANTIAL_LIBS+1, libs);
  int reading_nodes_when_called = get_reading_node_counter();
  int tactical_nodes;
  int result;
  double start = 0.0;
  struct local_owl_data *owl;
  int num_moves = 0;

  if (debug & DEBUG_OWL_PERFORMANCE)
    start = gg_cputime();

  /* FIXME: We want to use the full init_owl here too (cf. similar
   * remark below).
   */
  reduced_init_owl(&owl, 1);

  owl->color = OTHER_COLOR(board[str]);
  local_owl_node_counter = 0;

  /* Big strings are always substantial since the biggest nakade is
   * six stones. (There are probably rare exceptions to this
   * rule, but they are unlikely to come up in a game.)
   */
  if (countstones(str) > 6)
    return 1;
  
  if (liberties > MAX_SUBSTANTIAL_LIBS)
    return 0;

  memset(owl->goal, 0, sizeof(owl->goal));
  /* Mark the neighbors of the string. If one is found which is alive, return
   * true. */
  {
    int adjs[MAXCHAIN];
    int adj;

    adj = chainlinks(str, adjs);
    for (k = 0; k < adj; k++) {
      if (dragon[adjs[k]].status == ALIVE)
	return 1;
      mark_dragon(adjs[k], owl->goal, 1);
    }
  }

  /* We must check the cache while stackp == 0, but we wait until the
   * trivial tests have been done.
   */
  if (search_persistent_owl_cache(OWL_SUBSTANTIAL, str, 0, 0,
				  &result, NULL, NULL, NULL))
    return result;

  /* fill all the liberties */
  for (k = 0; k < liberties; k++) {
    if (trymove(libs[k], owl->color, NULL, 0)) {
      if (get_level() >= 8)
	increase_depth_values();
      owl->goal[libs[k]] = 1;
      num_moves++;
    }
    else {
      /* if we can't fill, try swapping with the next liberty */
      if (k < liberties-1
	  && trymove(libs[k+1], owl->color, NULL, 0)) {
	if (get_level() >= 8)
	  increase_depth_values();
	owl->goal[libs[k+1]] = 1;
	libs[k+1] = libs[k];
	num_moves++;
      }
      else {
	/* Can't fill the liberties. Give up! */
	while (num_moves-- > 0) {
	  if (get_level() >= 8)
	    decrease_depth_values();
	  popgo();
	}
	return 0;
      }
    }
  }
  /* FIXME: We would want to use init_owl() here too, but it doesn't
   * fit very well with the construction of the goal array above.
   */
  memcpy(owl->cumulative_goal, owl->goal, BOARDMAX);
  compute_owl_escape_values(owl);
  owl_mark_boundary(owl);
  owl->lunches_are_current = 0;

  if (do_owl_attack(libs[0], NULL, NULL, owl, 0))
    result = 0;
  else
    result = 1;
  while (num_moves-- > 0) {
    if (get_level() >= 8)
      decrease_depth_values();
    popgo();
  }

  tactical_nodes = get_reading_node_counter() - reading_nodes_when_called;
  DEBUG(DEBUG_OWL_PERFORMANCE,
	"owl_substantial %1m, result %d (%d, %d nodes, %f seconds)\n",
	str, result, local_owl_node_counter,
	tactical_nodes, gg_cputime() - start);

  store_persistent_owl_cache(OWL_SUBSTANTIAL, str, 0, 0, result, 0, 0, 0,
			     tactical_nodes, owl->goal, owl->color);

  return result;
}



/* Returns true if and only if (i, j) is a 1-2 vertex, i.e. next to a
 * corner.
 */
static int
one_two_point(int pos)
{
  int i = I(pos);
  int j = J(pos);
  
  if ((i == 0 || i == board_size-1 || j == 0 || j == board_size-1)
      && (i == 1 || i == board_size-2 || j == 1 || j == board_size-2))
    return 1;

  return 0;
}



/* Reports the number of eyes gotten by capturing a boundary string.
 * This implementation tends to give an optimistic view of the
 * chances, so if it tells that the lunch is worthless, it truly
 * should be. The converse is not true.
 */

static void
sniff_lunch(int lunch, int *min, int *probable, int *max,
	    struct local_owl_data *owl)
{
  int other = OTHER_COLOR(board[lunch]);
  int libs[MAXLIBS];
  int liberties;
  int r;

  ASSERT1(IS_STONE(board[lunch]), lunch);

  if (owl->boundary[lunch] == 2) {
    *min = 2;
    *probable = 2;
    *max = 2;
    return;
  }

  /* Do we believe this capture would help escaping? */
  liberties = findlib(lunch, MAXLIBS, libs);
  for (r = 0; r < liberties; r++) {
    if (owl->escape_values[libs[r]] > 0
	&& !is_self_atari(libs[r], other)) {
      int k;
      for (k = 0; k < 8; k++)
	if (ON_BOARD(libs[r] + delta[k]) && owl->goal[libs[r] + delta[k]])
	  break;
      if (k == 8) {
	*min = 2;
	*probable = 2;
	*max = 2;
	return;
      }
    }
  }

  estimate_lunch_eye_value(lunch, min, probable, max, 1);

  if (*min < 2) {
    int bonus = estimate_lunch_half_eye_bonus(lunch, owl->half_eye);
    *min += bonus/2;
    *probable += bonus;
    *max += (bonus + 1)/2;
  }

  if (*probable < 2)
    eat_lunch_escape_bonus(lunch, min, probable, max, owl);
}

/* Capturing a lunch can give eyes by turning a false eye into a proper one,
 * etc. This function returns the likely increase in half eyes
 * by capturing the string at (lunch).
 */
static int
estimate_lunch_half_eye_bonus(int lunch,
    			      struct half_eye_data half_eye[BOARDMAX])
{
  int stones[10];
  int k;
  int size = findstones(lunch, 10, stones);
  int half_eyes = 0;

  ASSERT1(size < 10, lunch);

  for (k = 0; k < size; k++) {
    int stone = stones[k];
    int d;
    for (d = 4; d < 8; d++) {
      int pos = stone + delta[d];
      if (ON_BOARD(pos)
	  && (is_halfeye(half_eye, pos) || is_false_eye(half_eye, pos)))
	half_eyes++;
    }
  }
  return half_eyes;
}


void
estimate_lunch_eye_value(int lunch, int *min, int *probable, int *max,
			 int appreciate_one_two_lunches)
{
  int other = OTHER_COLOR(board[lunch]);
  int size = countstones(lunch);

  if (size > 6) {
    *min = 2;
    *probable = 2;
    *max = 2;
  }
  else if (size > 4) {
    *min = 1;
    *probable = 2;
    *max = 2;
  }
  else if (size > 2) {
    *min = 0;
    *probable = 1;
    *max = 2;
  }
  else if (size == 2) {
    int stones[2];
    findstones(lunch, 2, stones);
    /* A lunch on a 1-2 point tends always to be worth contesting. */
    if ((obvious_false_eye(stones[0], other)
	|| obvious_false_eye(stones[1], other))
	&& (!appreciate_one_two_lunches
	    || !(one_two_point(stones[0]) || one_two_point(stones[1])))) {
      *min = 0;
      *probable = 0;
      *max = 0;
    }
    else {
      *min = 0;
      *probable = 1;
      *max = 1;
    }
  }
  else if (size == 1) {
    if (!obvious_false_eye(lunch, other)) {
      *min = 0;
      *probable = 1;
      *max = 1;
    }
    else {
      *min = 0;
      *probable = 0;
      *max = 0;
    }
  }
}

/* Gives a bonus for a lunch capture which joins a (or some) friendly
 * string(s) to the goal dragon and improves the escape potential at
 * the same time. This is indicated in some situations where the owl
 * code would stop the analysis because of various cutoffs. See
 * do_owl_defend()
 * 
 * The following implementation tries to get a precise idea of the
 * escape potential improvement by calling dragon_escape() twice.
 */
static void
eat_lunch_escape_bonus(int lunch, int *min, int *probable, int *max,
		       struct local_owl_data *owl)
{
  int adjacent[MAXCHAIN];
  int neighbors;
  int adjoins = 0;
  int n;
  /* Be very careful before touching this value.
   * See owl_estimate_life() for details.
   */
  UNUSED(min);
  
  /* Don't mess up with kos */
  if (is_ko_point(lunch))
    return;
  
  neighbors = chainlinks(lunch, adjacent);
  for (n = 0; n < neighbors; n++)
    adjoins |= !owl->goal[adjacent[n]];
  
  if (adjoins) {
    int before, after;
    before = dragon_escape(owl->goal, owl->color, owl->escape_values);
    /* if the escape route is already large enough to be considered
     * a WIN by the owl code, then no need for more */
    if (before < 5) {
      signed char new_goal[BOARDMAX];
      memcpy(new_goal, owl->goal, sizeof(new_goal));
      for (n = 0; n < neighbors; n++)
	if (!owl->goal[adjacent[n]])
	  mark_string(adjacent[n], new_goal, 2);
      after = dragon_escape(new_goal, owl->color, owl->escape_values);
	
      /* Following is completely ad hoc. Another set of tests might
       * very well get better results. */
      if (after - before >= 3) {
	if (after >= 8 || (before == 0 && after >= 5)) {
	  *probable = 2;
	  *max = 2;
	}
	else if (*max < 2)
	  (*max)++;
      }
    }
  }
}

 
/* Find a new origin when it has been captured or cut out of the
 * goal. Used in do_owl_attack()
 */
static int
select_new_goal_origin(int origin, struct local_owl_data *owl)
{
  int pos;
  for (pos = BOARDMIN; pos < BOARDMAX; pos++)
    if (board[pos] == owl->color && owl->goal[pos] == 1)
      return find_origin(pos);

  return origin;
}


/* Retrieve topological eye values stored in the half_eye[] array of
 * the current owl data.
 *
 * FIXME: Sooner or later we'll want this to return a non-rounded
 * value. When we change this, we have to review all patterns using
 * the autohelper owl_topological_eye().
 */
int
owl_topological_eye(int pos, int color)
{
  float value;
  UNUSED(color);
  value = current_owl_data->half_eye[pos].value;
  if (value > 2.0 && value < 4.0)
    return 3;
  else if (value <= 2.0)
    return (int) (value + 0.99); /* Round up. */
  else
    return (int) value;          /* Round down. */
}

/* This function returns true if it is judged that the capture of the
 * string at (pos) is sufficient to create one eye.
 *
 * Update: Now it instead returns the max number of eyes.
 */

int
vital_chain(int pos)
{
  int min;
  int probable;
  int max;
  sniff_lunch(pos, &min, &probable, &max, current_owl_data);

  return max;
}


static void
compute_owl_escape_values(struct local_owl_data *owl)
{
  int pos;
  int m, n;
  signed char safe_stones[BOARDMAX];
  SGFTree *save_sgf_dumptree = sgf_dumptree;
  int save_count_variations = count_variations;
  signed char mx[BOARDMAX];
  memset(mx, 0, sizeof(mx));
    
  sgf_dumptree = NULL;
  count_variations = 0;
  get_lively_stones(OTHER_COLOR(owl->color), safe_stones);
  sgf_dumptree = save_sgf_dumptree;
  count_variations = save_count_variations;

  compute_escape_influence(owl->color, safe_stones, NULL, NULL,
			   owl->escape_values);

  DEBUG(DEBUG_ESCAPE, "Owl escape values:\n");
  for (m = 0; m < board_size; m++) {
    for (n = 0; n < board_size; n++) {
      pos = POS(m, n);
      if (dragon[pos].color == owl->color && !owl->goal[pos]) {
	if (dragon[pos].crude_status == ALIVE)
	  owl->escape_values[pos] = 6;
	else if (dragon[pos].crude_status == UNKNOWN) {
	  if (DRAGON2(pos).moyo_size > 5)
	    owl->escape_values[pos] = 4;
	  else if (DRAGON2(pos).escape_route > 5) {
	    if (mx[dragon[pos].origin])
	      owl->escape_values[pos] = owl->escape_values[dragon[pos].origin];
	    else {
	      int pos2;
	      signed char escape_values[BOARDMAX];
	      signed char dragon_stones[BOARDMAX];

	      compute_escape_influence(owl->color, safe_stones, owl->goal,
				       NULL, escape_values);

	      /* mark_dragon() can't be used here in case a string of
	       * the dragon was captured by the initial move in
	       * owl_does_attack(). Actually it isn't really proper to
	       * use is_same_dragon() at stackp>0 either but it's more
	       * robust at least.
	       */
	      for (pos2 = BOARDMIN; pos2 < BOARDMAX; pos2++) {
		if (ON_BOARD(pos2))
		  dragon_stones[pos2] = is_same_dragon(pos2, pos);
	      }
	      
	      if (dragon_escape(dragon_stones, owl->color, escape_values) > 5)
		owl->escape_values[dragon[pos].origin] = 4;

	      mx[dragon[pos].origin] = 1;
	    }
	  }
	}
      }
      DEBUG(DEBUG_ESCAPE, "%o%d", owl->escape_values[pos]);
    }
    DEBUG(DEBUG_ESCAPE, "%o\n");
  }
}


/* Used by autohelpers. */
int
owl_escape_value(int pos)
{
  /* FIXME: Should have a more robust mechanism to avoid 
   * escaping inwards. Returning a negative value is just a kludge.
   */
  int k;
  ASSERT_ON_BOARD1(pos);
  if (current_owl_data->goal[pos])
    return -10;

  if (board[pos] == EMPTY)
    for (k = 0; k < 8; k++)
      if (ON_BOARD(pos + delta[k]) && current_owl_data->goal[pos + delta[k]])
	return -10;
  
  return current_owl_data->escape_values[pos];
}


/* Used by autohelpers. */
int
owl_goal_dragon(int pos)
{
  return current_owl_data->goal[pos] != 0;
}

/* Used by autohelpers.
 * Returns 1 if (pos) is an eyespace for the color of the dragon currently
 * under owl investigation.
 */
int
owl_eyespace(int pos)
{
  int origin;
  ASSERT_ON_BOARD1(pos);
  
  origin = current_owl_data->my_eye[pos].origin;
  return (ON_BOARD(origin)
	  && (current_owl_data->my_eye[origin].color
	      == current_owl_data->color)
	  && max_eyes(&current_owl_data->my_eye[origin].value) > 0);
}


/* Used by autohelpers.
 * Returns 1 if (pos) is an eyespace for the color of the dragon currently
 * under owl investigation, which is possibly worth (at least) 2 eyes.
 */
int
owl_big_eyespace(int pos)
{
  int origin;
  ASSERT_ON_BOARD1(pos);

  origin = current_owl_data->my_eye[pos].origin;
  return (ON_BOARD(origin) 
	  && (current_owl_data->my_eye[origin].color
	      == current_owl_data->color)
	  && max_eyes(&current_owl_data->my_eye[origin].value) >= 2);
}


/* Used by autohelpers.
 * Returns 1 if (pos) is an eyespace for the color of the dragon currently
 * under owl investigation.
 */
int
owl_mineye(int pos)
{
  int origin;
  ASSERT_ON_BOARD1(pos);
  
  origin = current_owl_data->my_eye[pos].origin;
  if (!ON_BOARD(origin)
      || (current_owl_data->my_eye[origin].color
	  != current_owl_data->color))
    return 0;
      
  return min_eyes(&current_owl_data->my_eye[origin].value);
}


/* Used by autohelpers.
 * Returns 1 if (pos) is an eyespace for the color of the dragon currently
 * under owl investigation.
 */
int
owl_maxeye(int pos)
{
  int origin;
  ASSERT_ON_BOARD1(pos);
  
  origin = current_owl_data->my_eye[pos].origin;
  if (!ON_BOARD(origin)
      || (current_owl_data->my_eye[origin].color
	  != current_owl_data->color))
    return 0;
      
  return max_eyes(&current_owl_data->my_eye[origin].value);
}


/* Used by autohelpers.
 * Returns 1 if (pos) is a non-marginal eyespace for the color of the
 * dragon currently under owl investigation.
 */
int
owl_proper_eye(int pos)
{
  ASSERT_ON_BOARD1(pos);

  return ((current_owl_data->my_eye[pos].color
	   == current_owl_data->color)
	  && !current_owl_data->my_eye[pos].marginal);
}
  

/* Used by autohelpers.
 * Returns the effective size of the eyespace at pos.
 */
int
owl_eye_size(int pos)
{
  int origin;
  ASSERT_ON_BOARD1(pos);

  origin = current_owl_data->my_eye[pos].origin;
  return current_owl_data->my_eye[origin].esize
	 - current_owl_data->my_eye[origin].msize;
}
  

/* Used by autohelpers.
 * Returns whether str is a lunch.
 */
int
owl_lunch(int str)
{
  int k;
  int origin;
  ASSERT_ON_BOARD1(str);
  ASSERT1(current_owl_data->lunches_are_current, str);
  origin = find_origin(str);

  for (k = 0; k < MAX_LUNCHES; k++) {
    if (current_owl_data->lunch[k] == NO_MOVE)
      break;
    if (current_owl_data->lunch[k] == origin)
      return 1;
  }

  return 0;
}
  

/* Used by autohelpers.

 * Returns 1 if (pos) is considered to be a strong dragon. This is
 * intended to be used to decide whether connecting to some external
 * stones is an easy way to live. The current implementation is fairly
 * conservative, requiring that (pos) was part of a dragon with two
 * eyes according to the static analysis. This requirement may be
 * relaxed considerably in the future.
 *
 * (pos) must not be part of the goal dragon.
 */
int
owl_strong_dragon(int pos)
{
  ASSERT_ON_BOARD1(pos);
  ASSERT1(IS_STONE(board[pos]), pos);
  
  return (!current_owl_data->goal[pos]
	  && dragon[pos].color == board[pos]
	  && dragon[pos].crude_status == ALIVE);
}
  

static int
owl_escape_route(struct local_owl_data *owl)
{
  signed char modified_escape[BOARDMAX];
  int pos;
  memcpy(modified_escape, owl->escape_values, sizeof(modified_escape));
  for (pos = BOARDMIN; pos < BOARDMAX; pos++)
    if (ON_BOARD(pos) && owl->cumulative_goal[pos])
      modified_escape[pos] = 0;
  return dragon_escape(owl->goal, owl->color, modified_escape);
}


/****************************
 * Initialization of owl data
 ****************************/

/* This is a temporary solution. We want to be able to use the full
 * init_owl() also in owl_substantial.
 */
static void
reduced_init_owl(struct local_owl_data **owl, int at_bottom_of_stack)
{
  if (at_bottom_of_stack)
    owl_stack_pointer = 0;
  else
    owl_stack_pointer++;

  check_owl_stack_size();
  *owl = owl_stack[owl_stack_pointer];
  VALGRIND_MAKE_WRITABLE(*owl, sizeof(struct local_owl_data));
}


/* Initialize owl data. Set at_bottom_of_stack to 1 the first time you
 * call init_owl() and to 0 any following time (only relevant if you
 * need more than one set of owl data).
 */
static void
init_owl(struct local_owl_data **owl, int target1, int target2, int move,
         int at_bottom_of_stack, int new_dragons[BOARDMAX])
{
  reduced_init_owl(owl, at_bottom_of_stack);

  local_owl_node_counter = 0;
  (*owl)->lunches_are_current = 0;
  owl_mark_dragon(target1, target2, *owl, new_dragons);
  if (move != NO_MOVE)
    owl_update_goal(move, SAME_DRAGON_MAYBE_CONNECTED, NO_MOVE, *owl, 0, NULL);
  compute_owl_escape_values(*owl);
}


/***********************
 * Storage of owl data
 ***********************/

/* Check the size of the owl stack and extend it if too small. */
static void
check_owl_stack_size(void)
{
  while (owl_stack_size <= owl_stack_pointer) {
    owl_stack[owl_stack_size] = malloc(sizeof(*owl_stack[0]));
    gg_assert(owl_stack[owl_stack_size] != NULL);
    owl_stack_size++;
  }
}

/* Push owl data one step upwards in the stack. Gets called from
 * push_owl.
 */
static void
do_push_owl(struct local_owl_data **owl)
{
  struct local_owl_data *new_owl = owl_stack[owl_stack_pointer];

  /* Mark all the data in *new_owl as uninitialized. */
  VALGRIND_MAKE_WRITABLE(new_owl, sizeof(struct local_owl_data));
  /* Copy the owl data. */
  memcpy(new_owl->goal, (*owl)->goal, sizeof(new_owl->goal));
  memcpy(new_owl->cumulative_goal, (*owl)->cumulative_goal,
         sizeof(new_owl->cumulative_goal));
  memcpy(new_owl->boundary, (*owl)->boundary, sizeof(new_owl->boundary));
  memcpy(new_owl->neighbors, (*owl)->neighbors, sizeof(new_owl->neighbors));
  memcpy(new_owl->escape_values, (*owl)->escape_values,
	 sizeof(new_owl->escape_values));
  new_owl->color = (*owl)->color;

  new_owl->lunches_are_current = 0;

  /* Needed for stack organization. Since there may be one or two sets
   * of owl data active at we don't know whether to restore from the
   * previos stack entry or two steps back.
   */
  new_owl->restore_from = *owl;

  /* Finally move the *owl pointer. */
  *owl = new_owl;
}


/* Push owl data one step upwards in the stack. The stack is extended
 * with dynamically allocated memory if it is too small.
 *
 * This function no longer may move existing owl data around, so
 * existing pointers do not risk becoming invalid.
 */
static void
push_owl(struct local_owl_data **owl)
{
  owl_stack_pointer++;
  check_owl_stack_size();
  do_push_owl(owl);
}


/* Retrieve owl data from the stack. */
static void
pop_owl(struct local_owl_data **owl)
{
  *owl = (*owl)->restore_from;
  owl_stack_pointer--;
}


/*
 * List worms in order to track captures during owl reading
 * (GAIN/LOSS codes)
 */
static int
list_goal_worms(struct local_owl_data *owl, int goal_worm[MAX_GOAL_WORMS])
{
  int pos, k;
  int w = 0;

  for (k = 0; k < MAX_GOAL_WORMS; k++)
    goal_worm[k] = NO_MOVE;

  for (pos = BOARDMIN; pos < BOARDMAX && w < MAX_GOAL_WORMS; pos++) {
    if (ON_BOARD(pos)
	&& board[pos]
	&& owl->goal[pos] == 1) {
      int origin = find_origin(pos);
      for (k = 0; k < w; k++)
	if (goal_worm[k] == origin) 
	  break;
      if (k == w)
	goal_worm[w++] = pos;
    }
  }

  /* experimental: let's try to fill up the array with other neighboring
   * opponent worms
   */
  if (1 && (w > 0) && (w < MAX_GOAL_WORMS)) {
    pos = goal_worm[0];
    for (k = 0; k < DRAGON2(pos).neighbors && w < MAX_GOAL_WORMS; k++) {
      int ii;
      int d = DRAGON2(pos).adjacent[k];
      if (DRAGON(d).color != owl->color)
	continue;

      for (ii = BOARDMIN; ii < BOARDMAX && w < MAX_GOAL_WORMS; ii++)
	if (ON_BOARD(ii) && board[ii] && worm[ii].origin == ii
	    && worm[ii].size >= 3 && dragon[ii].id == d)
	  goal_worm[w++] = ii;
    }
  }

  return w;
}

static void
prepare_goal_list(int str, struct local_owl_data *owl,
		  int list[MAX_GOAL_WORMS], int *flag,
		  int *kworm, int do_list)
{
  gg_assert(flag != NULL);

  if (kworm) {
    if (do_list)
      list_goal_worms(owl, list);
    /* N.B. We cannot use sizeof(list) below because a formal array
     * parameter implicitly is converted to a pointer and sizeof(list)
     * thus equals sizeof(int *), which is not what we want.
     */
    memcpy(dragon_goal_worms[dragon[str].id], list,
	   sizeof(dragon_goal_worms[dragon[str].id]));
    *flag = 1;
  }
  else
    *flag = 0;
}

static void
finish_goal_list(int *flag, int *wpos, int list[MAX_GOAL_WORMS], int index)
{
  gg_assert(flag != NULL);
  gg_assert(wpos != NULL);

  *flag = 0;
  if (index == MAX_GOAL_WORMS)
    *wpos = NO_MOVE;
  else
    *wpos = list[index];
}


/* Returns the number of worms in the goal dragon, and a pointer to each */

#if 0
static int
catalog_goal(struct local_owl_data *owl, int goal_worm[MAX_GOAL_WORMS])
{
  int pos;
  int worms = 0;
  int k;

  for (k = 0; k < MAX_WORMS; k++)
    goal_worm[k] = NO_MOVE;

  for (pos = BOARDMIN; pos < BOARDMAX && worms < MAX_WORMS; pos++)
    if (ON_BOARD(pos)
	&& board[pos]
	&& (owl->goal)[pos]) {
      int origin = find_origin(pos);
      if (pos == origin) {
	if (0) {
	  DEBUG(DEBUG_SEMEAI, "goal worm: %1m\n", pos);
	}
	goal_worm[worms++] = pos;
      }
    }
  return worms;
}
#endif

/***********************/

/* Clear statistics. */
void
reset_owl_node_counter()
{
  global_owl_node_counter = 0;
}


/* Retrieve statistics. */
int
get_owl_node_counter()
{
  return global_owl_node_counter;
}


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
