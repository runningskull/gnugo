/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * This is GNU Go, a Go program. Contact gnugo@gnu.org, or see       *
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
#include "patterns.h"
#include "cache.h"
#include "sgftree.h"
#include "gg_utils.h"

#define MAX_MOVES 3           /* maximum number of branches at each node */
#define MAX_SEMEAI_MOVES 6    /* semeai branch factor */
#define MAX_SEMEAI_DEPTH 100  /* Don't read below this depth */
#define MAX_LUNCHES 10
#define MAX_GOAL_WORMS 15  /* maximum number of worms in a dragon to be cataloged */
#define MAX_ESCAPE 3  /* After this many escape moves, owl_determine_life is */
                      /*    not called                                       */

struct local_owl_data {
  char goal[BOARDMAX];
  char boundary[BOARDMAX];

  /* FIXME: escape_values[] are never recomputed. Consider moving this array
   *	    from stack to a static or dynamic variable so it is not copied
   *	    around in do_push_owl(). Be aware of semeai code though.
   */
  char escape_values[BOARDMAX];
  int color;

  struct eye_data my_eye[BOARDMAX];
  /* array of half-eye data for use during owl reading */
  struct half_eye_data half_eye[BOARDMAX];
  
  int lunch[MAX_LUNCHES];
  int lunch_attack_code[MAX_LUNCHES];
  int lunch_attack_point[MAX_LUNCHES];
  int lunch_defend_code[MAX_LUNCHES];
  int lunch_defense_point[MAX_LUNCHES];
  char inessential[BOARDMAX];
  
  int lunches_are_current; /* If true, owl lunch data is current */  

  char safe_move_cache[BOARDMAX];

  /* This is used to organize the owl stack. */
  int restore_from;
  int number_in_stack;
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


struct owl_move_data {
  int pos;          /* move coordinate */
  int value;        /* value */
  const char *name; /* name of the pattern suggesting the move */
  int same_dragon;  /* whether the move extends the dragon or not */
  int escape;       /* true if an escape pattern is matched */
  int defense_pos;  /* defense coordinate for vital owl attack patterns. */
};

struct matched_pattern_data {
  int move;
  int ll;
  struct pattern *pattern;
};
  
struct matched_patterns_list_data {
  int initialized;
  int counter; 		/* Number of patterns in the list. */
  int used;		/* How many patterns have already been used?*/
  int ordered_up_to;	/* How far the list has been ordered. */
  int list_size;	
  struct matched_pattern_data *pattern_list;
};

void dump_pattern_list(struct matched_patterns_list_data *list);


static int do_owl_attack(int str, int *move, int *wormid,
			 struct local_owl_data *owl,
			 int komaster, int kom_pos, int escape);
static int do_owl_defend(int str, int *move, int *wormid,
			 struct local_owl_data *owl,
			 int komaster, int kom_pos, int escape);
static void owl_shapes(struct matched_patterns_list_data *list,
                       struct owl_move_data moves[MAX_MOVES], int color,
		       struct local_owl_data *owl, struct pattern_db *type);
static void collect_owl_shapes_callbacks(int anchor, int color,
	  			         struct pattern *pattern_db,
				         int ll, void *data);
static int get_next_move_from_list(struct matched_patterns_list_data *list,
                                   int color, struct owl_move_data *moves,
				   int cutoff);
static void init_pattern_list(struct matched_patterns_list_data *list);
static void close_pattern_list(int color,
			       struct matched_patterns_list_data *list);
static void owl_shapes_callback(int anchor, int color,
				struct pattern *pattern_db,
				int ll, void *data);
static void owl_add_move(struct owl_move_data *moves, int move, int value,
			 const char *reason, int same_dragon, int escape,
			 int defense_pos, int max_moves);
static void owl_determine_life(struct local_owl_data *owl,
			       struct local_owl_data *second_owl,
			       int komaster, int does_attack,
			       struct owl_move_data *moves,
			       struct eyevalue *probable_eyes,
			       int *eyemin, int *eyemax);
static void owl_find_relevant_eyespaces(struct local_owl_data *owl,
					char mw[BOARDMAX], char mz[BOARDMAX]);
static int owl_estimate_life(struct local_owl_data *owl,
			     struct local_owl_data *second_owl,
    		  	     struct owl_move_data vital_moves[MAX_MOVES],
		  	     const char **live_reason,
			     int komaster, int does_attack,
		  	     struct eyevalue *probable_eyes,
			     int *eyemin, int *eyemax);
static int modify_stupid_eye_vital_point(struct local_owl_data *owl,
					 int *vital_point,
					 int is_attack_point);
static void owl_mark_dragon(int apos, int bpos,
			    struct local_owl_data *owl);
static void owl_mark_worm(int apos, int bpos,
			  struct local_owl_data *owl);
static void owl_mark_boundary(struct local_owl_data *owl);
static void owl_update_goal(int pos, int same_dragon,
			    struct local_owl_data *owl, int semeai_call);
static void owl_update_boundary_marks(int pos, struct local_owl_data *owl);
static void owl_find_lunches(struct local_owl_data *owl);
static int improve_lunch_attack(int lunch, int attack_point);
static int improve_lunch_defense(int lunch, int defense_point);
static void owl_make_domains(struct local_owl_data *owla,
			     struct local_owl_data *owlb);
static int owl_safe_move(int move, int color);
static void sniff_lunch(int lunch, int *min, int *probable, int *max,
			struct local_owl_data *owl);
static void eat_lunch_escape_bonus(int lunch, int *min, int *probable,
				   int *max, struct local_owl_data *owl);
static void compute_owl_escape_values(struct local_owl_data *owl);
static int owl_escape_route(struct local_owl_data *owl);
static void do_owl_analyze_semeai(int apos, int bpos, 
				  struct local_owl_data *owla,
				  struct local_owl_data *owlb,
				  int komaster, int kom_pos,
				  int *resulta, int *resultb,
				  int *move, int pass, int owl_phase);
static int semeai_trymove_and_recurse(int apos, int bpos,
				      struct local_owl_data **owla,
				      struct local_owl_data **owlb,
				      int komaster, int kom_pos, int owl_phase,
				      int move, int color, int ko_allowed,
				      int move_value, const char *move_name,
				      int same_dragon, int *semeai_move,
				      int *this_resulta, int *this_resultb);
static void semeai_add_sgf_comment(int value, int owl_phase);
static int semeai_trust_tactical_attack(int str);
static void semeai_review_owl_moves(struct owl_move_data owl_moves[MAX_MOVES],
				    struct local_owl_data *owla,
				    struct local_owl_data *owlb, int color,
				    int *safe_outside_liberty_found,
				    int *safe_common_liberty_found,
				    char mw[BOARDMAX],
				    struct owl_move_data semeai_moves[MAX_SEMEAI_MOVES],
				    int guess_same_dragon, int value_bonus,
				    int *critical_semeai_worms);
static int semeai_move_value(int move, struct local_owl_data *owla,
			     struct local_owl_data *owlb, int raw_value,
			     int *critical_semeai_worms);
static int find_semeai_backfilling_move(int worm, int liberty);
static int liberty_of_goal(int pos, struct local_owl_data *owl);
static int second_liberty_of_goal(int pos, struct local_owl_data *owl);
static int matches_found;
static char found_matches[BOARDMAX];

static void reduced_init_owl(struct local_owl_data **owl,
    			     int at_bottom_of_stack);
static void init_owl(struct local_owl_data **owl, int target1, int target2,
		     int move, int use_stack);

static struct local_owl_data *owl_stack = NULL;
static int owl_stack_size = 0;
static int owl_stack_pointer = 0;
static void push_owl(struct local_owl_data **owla,
    		     struct local_owl_data **owlb);
static void do_push_owl(struct local_owl_data **owl);
static void pop_owl(struct local_owl_data **owl);

#if 0
static int catalog_goal(struct local_owl_data *owl,
    			int goal_worm[MAX_GOAL_WORMS]);
#endif

static int list_goal_worms(struct local_owl_data *owl,
    			   int goal_worm[MAX_GOAL_WORMS]);

/* FIXME: taken from move_reasons.h */
#define MAX_DRAGONS       2*MAX_BOARD*MAX_BOARD/3

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

/* Called when (apos) and (bpos) point to adjacent dragons
 * of the opposite color, both with matcher_status DEAD or
 * CRITICAL, analyzes the semeai, assuming that the player
 * of the (apos) dragon moves first.
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
				semeai_move, owl, semeai_result_certain);
}

/* Same as the function above with the addition that an arbitrary move
 * may be made before the analysis is performed.
 */
void
owl_analyze_semeai_after_move(int move, int color, int apos, int bpos,
			      int *resulta, int *resultb, int *semeai_move, 
			      int owl, int *semeai_result_certain)
{
  char ms[BOARDMAX];
  int w1, w2;
  int str;
  SGFTree *save_sgf_dumptree = sgf_dumptree;
  int save_verbose = verbose;
  int dummy_resulta;
  int dummy_resultb;

  struct local_owl_data *owla;
  struct local_owl_data *owlb;
  
  if (!resulta)
    resulta = &dummy_resulta;
  if (!resultb)
    resultb = &dummy_resultb;

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
      /* FIXME: Consider also strings neighboring the outside as
       * critical semeai worms.
       */
      if (countstones(str) > 6 && s_worms < MAX_SEMEAI_WORMS) {
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
    TRACE("owl_analyze_semeai: %1m vs. %1m\n", apos, bpos);
  else
    TRACE("owl_analyze_semeai_after_move %C %1m: %1m vs. %1m\n",
	  color, move, apos, bpos);
  
  if (owl) {
    init_owl(&owla, apos, NO_MOVE, NO_MOVE, 1);
    init_owl(&owlb, bpos, NO_MOVE, NO_MOVE, 0);
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

  if (move == PASS_MOVE)
    do_owl_analyze_semeai(apos, bpos, owla, owlb, EMPTY, NO_MOVE,
			  resulta, resultb, semeai_move, 0, owl);
  else {
    semeai_trymove_and_recurse(bpos, apos, &owlb, &owla, EMPTY, NO_MOVE, owl,
			       move, color, 1, 0, "mandatory move", 1,
			       semeai_move, resultb, resulta);
    *resulta = REVERSE_RESULT(*resulta);
    *resultb = REVERSE_RESULT(*resultb);
  }

  if (semeai_result_certain)
    *semeai_result_certain = result_certain;
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
		      int komaster, int kom_pos,
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
  struct owl_move_data common_liberty;
  struct owl_move_data backfill_outside_liberty;
  struct owl_move_data backfill_common_liberty;
  int safe_outside_liberty_found = 0;
  int safe_common_liberty_found = 0;
  char mw[BOARDMAX];  
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
  Read_result *read_result = NULL;
  int this_variation_number = count_variations - 1;
  int you_look_alive = 0;
  int I_look_alive = 0;
  int dummy_move;
  int tested_moves;
  int critical_semeai_worms[MAX_SEMEAI_WORMS];
  
  SETUP_TRACE_INFO2("do_owl_analyze_semeai", apos, bpos);

  if (!move)
    move = &dummy_move;
  
  ASSERT1(board[apos] == owla->color, apos);
  ASSERT1(board[bpos] == owlb->color, bpos);

  if (stackp <= semeai_branch_depth && (hashflags & HASH_SEMEAI)
      && !pass && owl_phase) {
    if (get_read_result2(SEMEAI, EMPTY, 0, &apos, &bpos, &read_result)) {
      TRACE_CACHED_RESULT2(*read_result);

      if (rr_get_result1(*read_result) != 0)
	*move = rr_get_move(*read_result);
      
      *resulta = rr_get_result1(*read_result);
      *resultb = rr_get_result2(*read_result);

      TRACE("%oVariation %d: %1m %1m %s %s %1m (cached) ",
            this_variation_number, apos, bpos,
	    result_to_string(*resulta),
	    result_to_string(*resultb),
	    *move);
      SGFTRACE_SEMEAI(rr_get_move(*read_result), *resulta,
	              *resultb, "cached");
      return;
    }
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
  common_liberty.pos = NO_MOVE;
  backfill_outside_liberty.pos = NO_MOVE;
  backfill_common_liberty.pos = NO_MOVE;
  for (k = 0; k < MAX_SEMEAI_MOVES; k++) {
    moves[k].pos = 0;
    moves[k].value = -1;
    moves[k].name = NULL;
    moves[k].same_dragon = 2;
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
    int sworm;

    for (sworm = 0; sworm <= s_worms; sworm++) {
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
	  READ_RETURN_SEMEAI(read_result, move, upos, WIN, WIN);
	}
	else if (acode != 0
		 && find_defense(semeai_worms[sworm], NULL)) {
	  critical_semeai_worms[sworm] = 1;
	  owl_add_move(moves, upos, 95, "attack semeai worm", 1, 0, NO_MOVE,
		       MAX_SEMEAI_MOVES);
	  TRACE("Added %1m %d (-1)\n", upos, 95);
	}
      }
    }
    /* Look for a tactical rescue. If a semeai worm of owla is tactically
     * threatened, try to save it.
     */

    for (sworm = 0; sworm <= s_worms; sworm++)
      if (board[semeai_worms[sworm]] == color
	  && attack(semeai_worms[sworm], NULL)
	  && find_defense(semeai_worms[sworm], &upos)) {
	critical_semeai_worms[sworm] = 1;
	owl_add_move(moves, upos, 85, "defend semeai worm", 1, 0, NO_MOVE,
		     MAX_SEMEAI_MOVES);
	TRACE("Added %1m %d (0)\n", upos, 85);
      }
  }

  /* We generate the candidate moves. During the early stages of
   * the semeai, there may be moves to expand or shrink the
   * eyespaces of the two dragons. During the later stages, the
   * picture is simplified and reading the semeai is a matter 
   * of filling liberties until one of the dragons may be removed,
   * or a seki results. The first stage we call the owl phase.
   */
  if (owl_phase) {
    /* First the vital moves. These include moves to attack or
     * defend the eyespace (e.g. nakade, or hane to reduce the
     * number of eyes) or moves to capture a lunch. 
     */
    struct eyevalue probable_eyes_a;
    struct eyevalue probable_eyes_b;
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
#if 0
    for (k = 0; k < MAX_LUNCHES; k++) {
      if (owlb->lunch[k] != NO_MOVE 
	  && owla->goal[owlb->lunch[k]]) {
	owlb->lunch[k] = NO_MOVE;
      }
    }
#endif


    if (owl_estimate_life(owla, owlb, vital_defensive_moves,
			  &live_reasona, komaster, 0, &probable_eyes_a,
			  &eyemin_a, &eyemax_a))
      I_look_alive = 1;
    else if (stackp > 2 && owl_escape_route(owla) >= 5) {
      live_reasona = "escaped";
      I_look_alive = 1;
    }

    if (owl_estimate_life(owlb, owla, vital_offensive_moves,
			  &live_reasonb, komaster, 1, &probable_eyes_b,
			  &eyemin_b, &eyemax_b))
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
      READ_RETURN_SEMEAI(read_result, move, PASS_MOVE, WIN, 0);
    }
    
    /* Next the shape moves. */
    if (!I_look_alive) {
      owl_shapes(&shape_defensive_patterns, shape_defensive_moves, color,
		 owla, &owl_defendpat_db);
      for (k = 0; k < MAX_MOVES-1; k++)
	if (!get_next_move_from_list(&shape_defensive_patterns, color,
				     shape_defensive_moves, 1))
	  break;
    }
    owl_shapes(&shape_offensive_patterns, shape_offensive_moves, color, owlb, 
	       &owl_attackpat_db);
    for (k = 0; k < MAX_MOVES-1; k++)
      if (!get_next_move_from_list(&shape_offensive_patterns, color,
	                           shape_offensive_moves, 1))
	break;
    
    /* Now we review the moves already considered, while collecting
     * them into a single list. 
     */

    if (!I_look_alive) {
      semeai_review_owl_moves(vital_defensive_moves, owla, owlb, color,
			      &safe_outside_liberty_found,
			      &safe_common_liberty_found,
			      mw, moves, 0, 30,
			      critical_semeai_worms);
      
      semeai_review_owl_moves(shape_defensive_moves, owla, owlb, color,
			      &safe_outside_liberty_found,
			      &safe_common_liberty_found,
			      mw, moves, 0, 0,
			      critical_semeai_worms);
    }

    if (!you_look_alive) {
      semeai_review_owl_moves(vital_offensive_moves, owla, owlb, color,
			      &safe_outside_liberty_found,
			      &safe_common_liberty_found,
			      mw, moves, 1, 30,
			      critical_semeai_worms);
      
      semeai_review_owl_moves(shape_offensive_moves, owla, owlb, color,
			      &safe_outside_liberty_found,
			      &safe_common_liberty_found,
			      mw, moves, 1, 0,
			      critical_semeai_worms);
    }

    if (level < 8) {
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
   */
  if (!you_look_alive
      && !safe_outside_liberty_found && moves[0].value < 100) {
    int pos;
    for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
      if (!ON_BOARD(pos))
	continue;
      
      if (board[pos] == EMPTY && !mw[pos]) {
	if (liberty_of_goal(pos, owlb)) {
	  if (!liberty_of_goal(pos, owla)) {
	    /* outside liberty */
	    if (safe_move(pos, color)) {
	      safe_outside_liberty_found = 1;
	      outside_liberty.pos = pos;
	      break;
	    }
	    else if (backfill_outside_liberty.pos == NO_MOVE)
	      backfill_outside_liberty.pos = find_semeai_backfilling_move(bpos,
									  pos);
	  }
	  else {
	    /* common liberty */
	    if (safe_move(pos, color)) {
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
   * to fill a mutual liberty or play a corresponding backfilling
   * move.
   */
  if (!you_look_alive) {
    if (safe_outside_liberty_found
	&& outside_liberty.pos != NO_MOVE) {
      move_value = semeai_move_value(outside_liberty.pos,
				     owla, owlb, 50,
				     critical_semeai_worms);
      owl_add_move(moves, outside_liberty.pos, move_value,
		   "safe outside liberty", 0, 0, NO_MOVE, MAX_SEMEAI_MOVES);
      TRACE("Added %1m %d (5)\n", outside_liberty.pos, move_value);
    }
    else if (backfill_outside_liberty.pos != NO_MOVE) {
      move_value = semeai_move_value(backfill_outside_liberty.pos,
				     owla, owlb, 50,
				     critical_semeai_worms);
      owl_add_move(moves, backfill_outside_liberty.pos, move_value,
		   "backfilling move", 0, 0, NO_MOVE, MAX_SEMEAI_MOVES);
      TRACE("Added %1m %d (6)\n", backfill_outside_liberty.pos, move_value);
    }
    else if (safe_common_liberty_found
	     && common_liberty.pos != NO_MOVE) {
      move_value = semeai_move_value(common_liberty.pos,
				     owla, owlb, 10,
				     critical_semeai_worms);
      owl_add_move(moves, common_liberty.pos, move_value,
		   "safe common liberty", 1, 0, NO_MOVE, MAX_SEMEAI_MOVES);
      TRACE("Added %1m %d (7)\n", common_liberty.pos, move_value);
    }
    else if (backfill_common_liberty.pos != NO_MOVE) {
      move_value = semeai_move_value(backfill_common_liberty.pos,
				     owla, owlb, 10,
				     critical_semeai_worms);
      owl_add_move(moves, backfill_common_liberty.pos, move_value,
		   "backfilling move", 0, 0, NO_MOVE, MAX_SEMEAI_MOVES);
      TRACE("Added %1m %d (6)\n", backfill_common_liberty.pos, move_value);
    }
  }

  if (moves[0].pos == NO_MOVE) {
    TRACE("No move found\n");
  }
  
  /* Now we are ready to try moves. Turn on the sgf output ... */
  sgf_dumptree = save_sgf_dumptree;
  count_variations = save_count_variations;
  tested_moves = 0;
  for (k = 0; k < MAX_SEMEAI_MOVES; k++) {
    int mpos = moves[k].pos;
    if (mpos == NO_MOVE)
      break;

    /* Do not try too many moves. */
    if (tested_moves > 2
	|| (stackp > semeai_branch_depth2 && tested_moves > 1)
	|| (stackp > semeai_branch_depth && tested_moves > 0)) {
      /* If allpats, try and pop to get the move in the sgf record. */
      if (!allpats)
	break;
      else if (trymove(mpos, color, moves[k].name, apos, komaster, kom_pos)) {
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
    if (semeai_trymove_and_recurse(apos, bpos, &owla, &owlb, komaster,
				   kom_pos, owl_phase, mpos, color,
				   best_resulta == 0 || best_resultb == 0,
				   moves[k].value, moves[k].name,
				   moves[k].same_dragon, NULL,
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
	READ_RETURN_SEMEAI(read_result, move, mpos, WIN, WIN);
      }
      /* We consider our own safety most important and attacking the
       * opponent as a secondary aim. This means that we prefer seki
       * over a ko for life and death.
       *
       * FIXME: If our dragon is considerably smaller than the
       * opponent dragon, we should probably prefer ko over seki.
       */
      else if (this_resulta > best_resulta
	       || (this_resulta == best_resulta
		   && this_resultb > best_resultb)) {
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
  if (tested_moves == 0 && you_look_alive) {
    *resulta = 0;
    *resultb = 0;
    *move = PASS_MOVE;
    SGFTRACE_SEMEAI(PASS_MOVE, 0, 0, "You live, I die");
    READ_RETURN_SEMEAI(read_result, move, PASS_MOVE, 0, 0);
  }

  /* If we can't find a move and opponent passed, it's seki */
  if (tested_moves == 0 && pass == 1) {
    *resulta = WIN;
    *resultb = 0;
    *move = PASS_MOVE;
    TRACE("Seki\n");
    SGFTRACE_SEMEAI(PASS_MOVE, WIN, 0, "Seki");
    READ_RETURN_SEMEAI(read_result, move, PASS_MOVE, WIN, 0);
  }
  
  /* If no move was found, then pass. */
  if (tested_moves == 0) {
    do_owl_analyze_semeai(bpos, apos, owlb, owla, komaster, kom_pos,
			  resultb, resulta, NULL, 1, owl_phase);
    *resulta = REVERSE_RESULT(*resulta);
    *resultb = REVERSE_RESULT(*resultb);
    TRACE("No move found\n");
    SGFTRACE_SEMEAI(PASS_MOVE, *resulta, *resultb, "No move found");
    *move = PASS_MOVE;
    READ_RETURN_SEMEAI(read_result, move, PASS_MOVE, *resulta, *resultb);
  }

  *resulta = best_resulta;
  *resultb = best_resultb;
  if (best_resulta == 0)
    best_move = PASS_MOVE;
  *move = best_move;
  SGFTRACE_SEMEAI(best_move, best_resulta, best_resultb, best_move_name);
  READ_RETURN_SEMEAI(read_result, move, best_move, best_resulta, best_resultb);
}

/* Play a move, update goal and boundaries appropriately, and call
 * do_owl_analyze_semeai() recursively to determine the result of this
 * move.
 */
static int
semeai_trymove_and_recurse(int apos, int bpos, struct local_owl_data **owla,
			   struct local_owl_data **owlb,
			   int komaster, int kom_pos, int owl_phase,
			   int move, int color, int ko_allowed,
			   int move_value, const char *move_name,
			   int same_dragon, int *semeai_move,
			   int *this_resulta, int *this_resultb)
{
  int new_komaster = EMPTY;
  int new_kom_pos = NO_MOVE;
  int ko_move = 0;
  
  gg_assert(this_resulta != NULL && this_resultb != NULL);
  *this_resulta = 0;
  *this_resultb = 0;
  if (!komaster_trymove(move, color, move_name, apos, komaster, kom_pos,
			&new_komaster, &new_kom_pos, &ko_move, ko_allowed))
    return 0;
  
  semeai_add_sgf_comment(move_value, owl_phase);
  TRACE("Trying %C %1m. Current stack: ", color, move);
  if (verbose) {
    dump_stack();
    goaldump((*owla)->goal);
    gprintf("\n");
    goaldump((*owlb)->goal);
    gprintf("\n");
  }
  TRACE("%s, value %d, same_dragon %d\n", move_name, move_value, same_dragon);
    
  push_owl(owla, owlb);

  if ((*owla)->color == color) {
    owl_update_goal(move, same_dragon, *owla, 1);
    owl_update_boundary_marks(move, *owlb);
  }
  else {
    owl_update_goal(move, same_dragon, *owlb, 1);
    owl_update_boundary_marks(move, *owla);
  }
    
  /* Do a recursive call to read the semeai after the move we just
   * tried. If dragon b was captured by the move, call
   * do_owl_attack() to see whether it sufficed for us to live.
   */
  if (board[bpos] == EMPTY) {
    /* FIXME: Are all owl_data fields and relevant static
     * variables properly set up for a call to do_owl_attack()?
     */
    *this_resulta = REVERSE_RESULT(do_owl_attack(apos, NULL, NULL, *owla,
						new_komaster, new_kom_pos,
						0));
    *this_resultb = *this_resulta;
  }
  else {
    do_owl_analyze_semeai(bpos, apos, *owlb, *owla, new_komaster, new_kom_pos,
			  this_resultb, this_resulta, semeai_move, 0,
			  owl_phase);
    *this_resulta = REVERSE_RESULT(*this_resulta);
    *this_resultb = REVERSE_RESULT(*this_resultb);
  }
    
  pop_owl(owlb);
  pop_owl(owla);
    
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
			char mw[BOARDMAX],
			struct owl_move_data semeai_moves[MAX_SEMEAI_MOVES],
			int guess_same_dragon, int value_bonus,
			int *critical_semeai_worms)
{
  int move;
  int move_value;
  int same_dragon;
  int k;
  
  for (k = 0; k < MAX_MOVES-1; k++) {
    move = owl_moves[k].pos;
    if (move == NO_MOVE)
      break;

    /* Does the move fill a liberty in the semeai? */
    if (liberty_of_goal(move, owlb)
	&& safe_move(move, color)) {
      if (!liberty_of_goal(move, owla))
	*safe_outside_liberty_found = 1;
      else
	*safe_common_liberty_found = 1;
    }

    /* For some types of owl moves we don't have same_dragon
     * information recorded and need to guess.
     */
    if (guess_same_dragon) {
      if (liberty_of_goal(move, owla)
	  || second_liberty_of_goal(move, owla))
	same_dragon = 1;
      else
	same_dragon = 0;
    }
    else
      same_dragon = owl_moves[k].same_dragon;

    mw[move] = 1;
    move_value = (semeai_move_value(move, owla, owlb, owl_moves[k].value,
				    critical_semeai_worms)
		  + value_bonus);
    owl_add_move(semeai_moves, move, move_value, owl_moves[k].name, 
		 same_dragon, owl_moves[k].escape,
		 NO_MOVE, MAX_SEMEAI_MOVES);
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
	if (owla->goal[pos])
	  net -= 75*countlib(pos);
	if (owlb->goal[pos])
	  net += 100*countlib(pos);	  
      }
    }
    if (!trymove(move, color, NULL, 0, 0, NO_MOVE)) {
      verbose = save_verbose;
      return 0;
    }
    for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
      if (IS_STONE(board[pos])
	  && pos == find_origin(pos)) {
	if (owla->goal[pos]
	    || (pos == move && liberty_of_goal(move, owla)))
	  net += 75*countlib(pos);
	if (owlb->goal[pos])
	  net -= 100*countlib(pos);
      }
    }

    increase_depth_values();
    for (k = 0; k <= s_worms; k++) {
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

  if (safe_move(liberty, other))
    return liberty;
  if (is_self_atari(liberty, other)) {
    int fill;
    if (approxlib(liberty, other, 1, &fill) > 0
	&& trymove(fill, other, "find_semeai_backfilling_move", worm, 
		   EMPTY, NO_MOVE)) {
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
    moves[k].same_dragon = 2;
    moves[k].escape = 0;
  }
}

static void
set_single_owl_move(struct owl_move_data moves[MAX_MOVES],
    		    int pos, const char *name)
{
  moves[0].pos         = pos;
  moves[0].value       = 25;
  moves[0].name        = name;
  moves[0].same_dragon = 1;
  moves[0].escape      = 0;
  moves[1].value       = 0;
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
  double start = 0;
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
  init_owl(&owl, target, NO_MOVE, NO_MOVE, 1);
  owl_make_domains(owl, NULL);
  prepare_goal_list(target, owl, owl_goal_worm, &goal_worms_computed,
		    kworm, 1);
  result = do_owl_attack(target, &move, &wid, owl, EMPTY, 0, 0);
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
	      struct local_owl_data *owl,
	      int komaster, int kom_pos, int escape)
{
  int color = board[str];
  int other = OTHER_COLOR(color);
  struct owl_move_data vital_moves[MAX_MOVES];
  struct owl_move_data shape_moves[MAX_MOVES];
  struct owl_move_data *moves;
  struct matched_patterns_list_data shape_patterns;
  char mw[BOARDMAX];
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
  Read_result *read_result = NULL;
  int this_variation_number = count_variations - 1;
  
  SETUP_TRACE_INFO("owl_attack", str);

  shape_patterns.initialized = 0;

  if ((stackp <= owl_branch_depth) && (hashflags & HASH_OWL_ATTACK)) {
    if (get_read_result(OWL_ATTACK, komaster, kom_pos, &str, &read_result)) {
      TRACE_CACHED_RESULT(*read_result);
      if (rr_get_result(*read_result) != 0) {
	if (move)
	  *move = rr_get_move(*read_result);
      }
      if (rr_get_result(*read_result) == GAIN) {
	if (wormid) {
	  if (goal_worms_computed)
	    *wormid = rr_get_result2(*read_result);
	  else
	    *wormid = MAX_GOAL_WORMS;
	}
      }

      if (rr_get_result(*read_result) == WIN)
	TRACE("%oVariation %d: DEAD (cached)\n", this_variation_number);
      else
	TRACE("%oVariation %d: ALIVE (cached)\n", this_variation_number);

      SGFTRACE(rr_get_move(*read_result), rr_get_result(*read_result),
	       "cached");
      return rr_get_result(*read_result);
    }
  }


  /* If reading goes to deep or we run out of nodes, we assume life. */
  if (reading_limit_reached(&live_reason, this_variation_number))  {
    SGFTRACE(0, 0, live_reason);
    READ_RETURN(read_result, move, 0, 0);
  }

  memset(mw, 0, sizeof(mw));
  global_owl_node_counter++;
  local_owl_node_counter++;

  current_owl_data = owl;
  memset(owl->safe_move_cache, 0, sizeof(owl->safe_move_cache));

  /* First see whether there is any chance to kill. */
  if (owl_estimate_life(owl, NULL, vital_moves, &live_reason, komaster, 1,
			&probable_eyes, &eyemin, &eyemax)) {
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
    if (acode == 0)
      READ_RETURN(read_result, move, 0, 0);
    else {
      if (wormid)
	*wormid = saveworm;
      READ_RETURN2(read_result, move, mpos, acode, saveworm);
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
	if (result == WIN ||
	    (result != 0 && (min_eyes(&probable_eyes) >= 2
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
	int dcode = do_owl_defend(str, &dpos, NULL, owl, komaster,
				  kom_pos, escape);
	/* No defense, we won. */
	if (dcode == 0) {
	  TRACE("%oVariation %d: DEAD (no defense)\n",
		this_variation_number);
	  SGFTRACE(0, WIN, "no defense");
	  close_pattern_list(other, &shape_patterns);
	  READ_RETURN(read_result, move, 0, WIN);
	}
	else if (dpos != NO_MOVE) {
	  /* The dragon could be defended by one more move. Try to
	   * attack with this move.
	   *
	   * If the move is suicide for us, try to find a backfilling
	   * move to play instead.
	   */
	  const char *name = "defense move";

	  if (is_suicide(dpos, other)) {
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
      READ_RETURN0(read_result);
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
      int new_komaster;
      int new_kom_pos;
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
	                             shape_moves, move_cutoff))
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
			    komaster, kom_pos, &new_komaster, &new_kom_pos,
			    &ko_move, savecode == 0))
	continue;

      captured = (color == WHITE ? white_captured : black_captured) - captured;

      TRACE("Trying %C %1m. Escape = %d. Current stack: ",
	    other, mpos, escape);
      if (verbose)
	dump_stack();

      /* We have now made a move. Analyze the new position. */
      push_owl(&owl, NULL);
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
      else {
	int pos;
	origin = NO_MOVE;
	for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
	  if (board[pos] == color && owl->goal[pos] == 1) {
	      origin = find_origin(pos);
	      break;
	  }
	}
      }

      if (origin == NO_MOVE)
	dcode = 0;
      else
	dcode = do_owl_defend(origin, NULL, &wid, owl, 
			      new_komaster, new_kom_pos, escape);

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
	  READ_RETURN(read_result, move, mpos, WIN);
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
		 && stackp>1
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
      READ_RETURN2(read_result, move, savemove, savecode, saveworm);
    }
    else {
      SGFTRACE(savemove, savecode, "attack effective (ko) - E");
      READ_RETURN(read_result, move, savemove, savecode);
    }
  }

  if (sgf_dumptree) {
    char winstr[128];
    sprintf(winstr, "attack failed)\n  (%d variations",
	  	    count_variations - this_variation_number);
    SGFTRACE(0, 0, winstr);
  }
  READ_RETURN0(read_result);
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
  char saved_boundary[BOARDMAX];
  double start = 0;
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
  init_owl(&owl, target, NO_MOVE, NO_MOVE, 1);
  memcpy(saved_boundary, owl->boundary, sizeof(saved_boundary));
  owl_make_domains(owl, NULL);
  owl_shapes(&shape_patterns, moves, other, owl, &owl_attackpat_db);
  for (k = 0; k < MAX_MOVES; k++) {
    current_owl_data = owl;
    if (!get_next_move_from_list(&shape_patterns, other, moves, 1))
      break;
    else {
      int mpos = moves[k].pos;

      if (mpos != NO_MOVE && moves[k].value > 0)
	if (trymove(mpos, other, moves[k].name, target, EMPTY, 0)) {
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
		|| do_owl_attack(origin, NULL, NULL, owl, EMPTY, 0, 0)) {
	      /* probably this can't happen */
	      popgo();
	      gg_assert(stackp == 0);
	      result = 1;
	      break;
	    }
	  }
	  else if (do_owl_attack(target, &move2, NULL,
				  owl, EMPTY, 0, 0) == WIN) {
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
  double start = 0;
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
  init_owl(&owl, target, NO_MOVE, NO_MOVE, 1);
  owl_make_domains(owl, NULL);
  prepare_goal_list(target, owl, owl_goal_worm, &goal_worms_computed,
		    kworm, 1);
  result = do_owl_defend(target, &move, &wid, owl, EMPTY, 0, 0);
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
do_owl_defend(int str, int *move, int *wormid,
	      struct local_owl_data *owl,
	      int komaster, int kom_pos, int escape)
{
  int color = board[str];
  struct owl_move_data shape_moves[MAX_MOVES];
  struct owl_move_data vital_moves[MAX_MOVES];
  struct owl_move_data *moves;
  struct matched_patterns_list_data shape_patterns;
  char mw[BOARDMAX];
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
  Read_result *read_result = NULL;
  int this_variation_number = count_variations - 1;

  SETUP_TRACE_INFO("owl_defend", str);

  shape_patterns.initialized = 0;
  
  if ((stackp <= owl_branch_depth) && (hashflags & HASH_OWL_DEFEND)) {
    if (get_read_result(OWL_DEFEND, komaster, kom_pos, &str, &read_result)) {
      TRACE_CACHED_RESULT(*read_result);
      if (rr_get_result(*read_result) != 0) {
	if (move)
	  *move = rr_get_move(*read_result);
      }
      if (rr_get_result(*read_result) == LOSS) {
	if (wormid) {
	  if (goal_worms_computed)
	    *wormid = rr_get_result2(*read_result);
	  else
	    *wormid = MAX_GOAL_WORMS;
	}
      }

      if (rr_get_result(*read_result) == WIN
	  || rr_get_result(*read_result) == LOSS)
	TRACE("%oVariation %d: ALIVE (cached)\n", this_variation_number);
      else
	TRACE("%oVariation %d: DEAD (cached)\n", this_variation_number);

      SGFTRACE(rr_get_move(*read_result), rr_get_result(*read_result),
	       "cached");
      return rr_get_result(*read_result);
    }
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
    READ_RETURN(read_result, move, 0, WIN);
  }

  /* If reading goes to deep or we run out of nodes, we assume life. */
  if (reading_limit_reached(&live_reason, this_variation_number))  {
    SGFTRACE(0, WIN, live_reason);
    READ_RETURN(read_result, move, 0, WIN);
  }

  memset(mw, 0, sizeof(mw));
  local_owl_node_counter++;
  global_owl_node_counter++;

  current_owl_data = owl;
  memset(owl->safe_move_cache, 0, sizeof(owl->safe_move_cache));

  /* First see whether we might already be alife. */
  if (escape < MAX_ESCAPE) {
    if (owl_estimate_life(owl, NULL, vital_moves, &live_reason, komaster, 0,
	  		  &probable_eyes, &eyemin, &eyemax)) {
      SGFTRACE(0, WIN, live_reason);
      TRACE("%oVariation %d: ALIVE (%s)\n",
	    this_variation_number, live_reason);
      READ_RETURN(read_result, move, 0, WIN);
    }
  }
  else {
    /* In this case we don't recompute eyes. However, to avoid accessing
     * partially-random data left on stack, we copy eye data from the
     * previous depth level. It must be reasonably close to the actual
     * state of eyes.
     */
    memcpy(owl->my_eye, owl_stack[owl->restore_from].my_eye,
	   sizeof(owl->my_eye));
    memcpy(owl->half_eye, owl_stack[owl->restore_from].half_eye,
	   sizeof(owl->half_eye));

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
#if 1
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
#endif
    } /* switch (pass) */

    /* For the up to MAX_MOVES best moves with value equal to
     * move_cutoff or higher, try to defend the dragon and see if it
     * can then be attacked.
     */
    for (k = 0; k < MAX_MOVES; k++) {
      int mpos;
      int new_komaster, new_kom_pos;
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
	                             move_cutoff))
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
      
      /* Try to make the move. */
      if (!komaster_trymove(mpos, color, moves[k].name, str,
			    komaster, kom_pos, &new_komaster, &new_kom_pos,
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
      push_owl(&owl, NULL);
      mw[mpos] = 1;
      number_tried_moves++;

      /* Add the stone just played to the goal dragon, unless the
       * pattern explicitly asked for not doing this.
       */
      owl_update_goal(mpos, moves[k].same_dragon, owl, 0);

      if (!ko_move) {
	int acode = do_owl_attack(str, NULL, &wid, owl, new_komaster,
			          new_kom_pos, new_escape);
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
	  READ_RETURN(read_result, move, mpos, WIN);
	}
	if (acode == GAIN)
	  saveworm = wid;
	UPDATE_SAVED_KO_RESULT(savecode, savemove, acode, mpos);
      }
      else {
	if (do_owl_attack(str, NULL, NULL, owl, 
			  new_komaster, new_kom_pos, new_escape) != WIN) {
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
      READ_RETURN2(read_result, move, savemove, savecode, saveworm);
    }
    else {
      SGFTRACE(savemove, savecode, "defense effective (ko) - B");
      READ_RETURN(read_result, move, savemove, savecode);
    }
  }

  if (number_tried_moves == 0 && min_eyes(&probable_eyes) >= 2) {
    SGFTRACE(0, WIN, "genus probably >= 2");
    READ_RETURN(read_result, move, 0, WIN);
  }
  

  if (sgf_dumptree) {
    char winstr[196];
    int print_genus = eyemin == 1 ? 1 : 0;
    sprintf(winstr, "defense failed - genus %d)\n  (%d variations",
	  	    print_genus, count_variations - this_variation_number);
    SGFTRACE(0, 0, winstr);
  }

  READ_RETURN0(read_result);
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
  char saved_goal[BOARDMAX];
  double start = 0;
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
  init_owl(&owl, target, NO_MOVE, NO_MOVE, 1);
  memcpy(saved_goal, owl->goal, sizeof(saved_goal));
  owl_make_domains(owl, NULL);
  owl_shapes(&shape_patterns, moves, color, owl, &owl_defendpat_db);
  for (k = 0; k < MAX_MOVES; k++) {
    current_owl_data = owl;
    if (!get_next_move_from_list(&shape_patterns, color, moves, 1))
      break;
    else {
      if (moves[k].pos != NO_MOVE && moves[k].value > 0)
	if (trymove(moves[k].pos, color, moves[k].name, target, EMPTY, 0)) {
	  owl->lunches_are_current = 0;
	  owl_update_goal(moves[k].pos, moves[k].same_dragon, owl, 0);
	  if (do_owl_defend(target, &move2, NULL, owl, EMPTY, 0, 0) == WIN) {
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
 * various policies (depth-dependant) whether the dragon should thus
 * be considered alive.
 */
static int
owl_estimate_life(struct local_owl_data *owl,
		  struct local_owl_data *second_owl,
    		  struct owl_move_data vital_moves[MAX_MOVES],
		  const char **live_reason, int komaster, int does_attack,
		  struct eyevalue *probable_eyes, int *eyemin, int *eyemax)
{
  SGFTree *save_sgf_dumptree = sgf_dumptree;
  int save_count_variations = count_variations;
  struct owl_move_data dummy_moves[MAX_MOVES];
  int other = OTHER_COLOR(owl->color);

  sgf_dumptree = NULL;
  count_variations = 0;

  owl_determine_life(owl, second_owl, komaster, does_attack, vital_moves,
		     probable_eyes, eyemin, eyemax);

  matches_found = 0;
  memset(found_matches, 0, sizeof(found_matches));

  if (level >= 8) {
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
		 dummy_moves[0].value, dummy_moves[0].name, 2, 0, NO_MOVE,
		 MAX_MOVES);
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
 * The parameter komaster is currently unused. It is included to
 * prepare better handling of ko once the optics code becomes more ko
 * aware.
 */

static void
owl_determine_life(struct local_owl_data *owl,
		   struct local_owl_data *second_owl,
		   int komaster, int does_attack,
		   struct owl_move_data *moves,
		   struct eyevalue *probable_eyes, int *eyemin, int *eyemax)
{
  int color = owl->color;
  struct eye_data *eye = owl->my_eye;
  char mw[BOARDMAX];  /* mark relevant eye origins */
  char mz[BOARDMAX];  /* mark potentially irrelevant eye origins */
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
  UNUSED(komaster);

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
			       eye, owl->half_eye);

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

	  owl_add_move(moves, attack_point, value, reason, 1, 0, NO_MOVE,
		       MAX_MOVES);
	  vital_values[attack_point] = value;
	  eyes_attack_points[num_eyes] = attack_point;
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

	  owl_add_move(moves, defense_point, value, reason, 1, 0, NO_MOVE,
		       MAX_MOVES);
	  vital_values[defense_point] = value;
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
	      	       "save lunch", 1, 0, NO_MOVE, MAX_MOVES);
	}
	else {
	  attack_point = improve_lunch_attack(owl->lunch[lunch],
					      owl->lunch_attack_point[lunch]);
	  TRACE("eat lunch at %1m with %1m, score %d, probable eye %d, max eye %d\n",
		owl->lunch[lunch], attack_point, value,
		lunch_probable, lunch_max);
	  owl_add_move(moves, attack_point, value, "eat lunch",
	      	       1, 0, NO_MOVE, MAX_MOVES);
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
 * support of goal) which are not GRAY_BORDERED. These
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
			    char mw[BOARDMAX], char mz[BOARDMAX])
{
  int pos;
  int eye_color;
  int k;
  struct eye_data *eye = owl->my_eye;
  
  if (owl->color == WHITE)
    eye_color = WHITE_BORDER;
  else
    eye_color = BLACK_BORDER;

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
 */
static int
modify_stupid_eye_vital_point(struct local_owl_data *owl, int *vital_point,
			      int is_attack_point)
{
  int up;
  int right;
  int k;

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
 * Returns 1 if at least one move is found, or 0 if no move is found.  */

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
  list->counter = 0;
  list->used = 0;
  list->ordered_up_to = 0;
  gg_assert(!list->initialized);
  list->pattern_list = malloc(200*sizeof(list->pattern_list[0]));
  if (0)
    gprintf("List at %x has new array at %x\n", list, list->pattern_list);
  gg_assert(list->pattern_list != NULL);
  list->list_size = 200;
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
      
      for (i = list->used ; i < list->counter; i++)
    	if (check_pattern_hard(list->pattern_list[i].move, color,
	     		       list->pattern_list[i].pattern,
			       list->pattern_list[i].ll)) {
	  if (!found_one) {
	    TRACE("Remaining valid (but unused) patterns at stack: ");
	    dump_stack();
	    found_one = 1;
	  }
      	  TRACE("Pattern %s found at %1m with value %d\n",
	        list->pattern_list[i].pattern->name,
	        list->pattern_list[i].move,
	        (int) list->pattern_list[i].pattern->value);
	}
      
      sgf_dumptree = save_sgf_dumptree;
      count_variations = save_count_variations;
    }
    free(list->pattern_list);
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
  gprintf("%oList size %d. %d Patterns in list, "
          "%d have been used, ordered up to %d.\n",
          list->list_size, list->counter, list->used, list->ordered_up_to);
  for (i = 0; i < list->counter; i++) {
    matched_pattern = &list->pattern_list[i];
    gprintf("%o  Pattern %s (orient. %d) at %1m, value %f.\n",
	    matched_pattern->pattern->name, matched_pattern->ll,
	    matched_pattern->move, matched_pattern->pattern->value);
  }
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
  next_pattern->move = AFFINE_TRANSFORM(pattern->move_offset, ll, anchor);
  next_pattern->pattern = pattern;
  next_pattern->ll = ll;
  matched_patterns->counter++;
}

#define USE_BDIST 1
#if USE_BDIST

/* compute the squared of the distance of a point on the board
 * to the center of the board
 */

static int
bdist(int move)
{
  /* i = 0:              idist = - (board_size - 1)
     i = board_size -1 : idist =    board_size - 1
     */
  int idist = 2*I(move) - board_size + 1;
  int jdist = 2*J(move) - board_size + 1;
  return idist*idist + jdist*jdist;
}
#endif

/* This function searches in the previously stored list of matched patterns
 * for the highest valued unused patterns that have a valid constraint.
 * It returns the moves at the next empty positions in the array (moves[]).
 * (Empty positions in the moves array are marked by having value <=0. There
 * must be enough empty positions in the list.)
 * If the highest valued pattern found has a value less than cutoff,
 * no move is returned.
 * Returns 1 if a move is found, 0 otherwise.
 *
 * One bubble sort-like iteration is used to find the next highest valued
 * pattern; then it is checked whether this move has not been tried before,
 * and if the pattern constraint is valid. This is repeated until enough
 * moves are found or the end of the list is reached.
 */

static int
get_next_move_from_list(struct matched_patterns_list_data *list, int color,
                        struct owl_move_data *moves, int cutoff)
{
  int top, bottom;
  int k;
  int i;
  int move;
  struct matched_pattern_data matched_pattern;
  int move_found = 0;
  SGFTree *save_sgf_dumptree = sgf_dumptree;
  int save_count_variations = count_variations;
    
  sgf_dumptree = NULL;
  count_variations = 0;

  /* The patterns above list->used have already been either discarded or
   * used by the calling function.
   */
  for (top = list->used; top < list->counter; top++) {
    /*
     * NOTICE : In order to stabilize the regression test results,
     * arbitrary parameters like pattern memory address and move position
     * have been included in the sorting algorithm.
     */
    float top_val = list->pattern_list[top].pattern->value;
    struct pattern *top_pattern = list->pattern_list[top].pattern;
    int top_move = list->pattern_list[top].move;
#if USE_BDIST
    int top_dist = bdist(list->pattern_list[top].move);
#endif

    /* Maybe we already know the top entry (if previous call was ended
     * by a value cutoff.
     */
    if (top >= list->ordered_up_to) {
      /* One bubble sort iteration. */
      for (bottom = list->counter-1; bottom > top; bottom--) {
	float bot_val = list->pattern_list[bottom].pattern->value;
	struct pattern *bot_pattern = NULL;
	int bot_move = NO_MOVE;
#if USE_BDIST
	int bot_dist = 0;
#endif
	if (bot_val >= top_val) {
	  bot_pattern = list->pattern_list[bottom].pattern;
	  bot_move = list->pattern_list[bottom].move;
#if USE_BDIST
	  bot_dist = bdist(list->pattern_list[bottom].move);
#endif
	}
#if USE_BDIST
        if (bot_val > top_val
           || (bot_val == top_val
               && bot_pattern < top_pattern)
           || (bot_val == top_val
               && bot_pattern == top_pattern
               && bot_dist < top_dist)
           || (bot_val == top_val
               && bot_pattern == top_pattern
               && bot_dist == top_dist
	       && bot_move < top_move)) {
#else
        if (bot_val > top_val
           || (bot_val == top_val
               && bot_pattern < top_pattern)
           || (bot_val == top_val
               && bot_pattern == top_pattern
               && bot_move < top_move)) {
#endif

	  matched_pattern = list->pattern_list[bottom];
	  list->pattern_list[bottom] = list->pattern_list[top];
	  list->pattern_list[top] = matched_pattern;

	  top_val = bot_val;
          top_pattern = bot_pattern;
          top_move = bot_move;
#if USE_BDIST
          top_dist = bot_dist;
#endif
	}
      }
      list->ordered_up_to++;
    }
    matched_pattern = list->pattern_list[top];
    if (top_val < (float) cutoff) {
      list->ordered_up_to = top + 1;
      list->used = top;
      sgf_dumptree = save_sgf_dumptree;
      count_variations = save_count_variations;
      return 0;
    }
    move = matched_pattern.move;
    ASSERT_ON_BOARD1(move);
    for (k = 0; k < MAX_MOVES; k++) {
      if (moves[k].pos == move || moves[k].value <= 0)
	break;
    }
    if (moves[k].pos == move)
      continue;
    gg_assert(k < MAX_MOVES); /* There has to be an empty space. */
    if (check_pattern_hard(move, color, matched_pattern.pattern,
			   matched_pattern.ll)) {
      moves[k].pos = move;
      moves[k].value = (int) top_val;
      moves[k].name = matched_pattern.pattern->name;
      move_found = 1;
      TRACE("Pattern %s found at %1m with value %d\n",
	    matched_pattern.pattern->name, move, moves[k].value);

      if (matched_pattern.pattern->class & CLASS_B)
        moves[k].same_dragon = 0;
      else if (matched_pattern.pattern->class & CLASS_b) {
	int same_dragon = 1;
	/* If we do not yet know whether the move belongs to the same dragon,
	 * we see whether another pattern can clarify.
	 */
	for (i = top + 1; i < list->counter; i++)
	  if ((list->pattern_list[i].move == move) 
	      && ((list->pattern_list[i].pattern->class & CLASS_B)
		  || !(list->pattern_list[i].pattern->class & CLASS_b))
	      && check_pattern_hard(move, color, list->pattern_list[i].pattern,
				    list->pattern_list[i].ll)) {
	    TRACE("Additionally pattern %s found at %1m\n",
		  list->pattern_list[i].pattern->name, move);
	    if (list->pattern_list[i].pattern->class & CLASS_B)
	      same_dragon = 0;
	    else
	      same_dragon = 2;
	    break;
	  }
	moves[k].same_dragon = same_dragon;
      }
      else
	moves[k].same_dragon = 2;
      if (matched_pattern.pattern->class & CLASS_E)
	moves[k].escape = 1;
      else
	moves[k].escape = 0;
      break;
    } /* if check_pattern_hard */
  }

  sgf_dumptree = save_sgf_dumptree;
  count_variations = save_count_variations;
  list->ordered_up_to = top+1;
  list->used = top+1;
  return (move_found);
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
  int same_dragon = 1;
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
    same_dragon = 0;
  else if (pattern->class & CLASS_b)
    same_dragon = 1;
  else
    same_dragon = 2;

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
  
  owl_add_move(moves, move, tval, pattern->name, same_dragon, escape,
	       defense_pos, MAX_MOVES);
}


/* Add a move to the list of candidate moves */

static void
owl_add_move(struct owl_move_data *moves, int move, int value,
	     const char *reason, int same_dragon, int escape, int defense_pos,
	     int max_moves)
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
      if (same_dragon > moves[k].same_dragon)
	moves[k].same_dragon = same_dragon;
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


/* Marks the dragons at (apos) and (bpos). If only one dragon
 * needs marking, (bpos) should be passed as (0). 
 */

static void
owl_mark_dragon(int apos, int bpos, struct local_owl_data *owl)
{
  int pos;
  int color = board[apos];
  
  ASSERT1(bpos == NO_MOVE || board[bpos] == color, bpos);

  for (pos = BOARDMIN; pos < BOARDMAX; pos++)
    if (ON_BOARD(pos)) {
      if (is_same_dragon(pos, apos) || is_same_dragon(pos, bpos))
	owl->goal[pos] = 1;
      else
	owl->goal[pos] = 0;
    }

  owl->color = color;
  owl_mark_boundary(owl);
}


/* Marks the worms at (apos) and (bpos). If only one worm
 * needs marking, (bpos) should be passed as (0). 
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
  int pos;
  int other = OTHER_COLOR(owl->color);
  int k;
  
  memset(owl->boundary, 0, sizeof(owl->boundary));
  /* first find all boundary strings. */
  for (pos = BOARDMIN; pos < BOARDMAX; pos++)
    if (board[pos] == other && !owl->boundary[pos]) {
      for (k = 0; k < 8; k++)
	if (ON_BOARD(pos + delta[k]) && owl->goal[pos + delta[k]]) {
	  mark_string(pos, owl->boundary, 1);
	  break;
	}
    }
  
  /* Upgrade the mark of a boundary string if it adjoins a safe
   * friendly dragon.
   */
  for (pos = BOARDMIN; pos < BOARDMAX; pos++)
    if (board[pos] == other && owl->boundary[pos] == 1) {
      for (k = 0; k < 4; k++) {
	int pos2 = pos + delta[k];
	if (board[pos2] == owl->color
	    && !owl->goal[pos2]
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
	
	if (board[apos] == owl->color && !owl->goal[apos]) {
	  owl->boundary[pos] = 2;
	  break;
	}
      }
    }
}

/* Add the stone just played to the goal dragon if same_dragon is
 * 2. We also add all stones belonging to the same generalized string
 * to the goal. If same_dragon is 1, we only add the stones if at
 * least one stone of the generalized string already was part of the
 * goal. If same_dragon is 0, we don't add any stones at all.
 */
static void
owl_update_goal(int pos, int same_dragon, struct local_owl_data *owl,
    		int semeai_call)
{
  int stones[MAX_BOARD * MAX_BOARD];
  int num_stones;
  int k;
  int do_add = 1;
  SGFTree *save_sgf_dumptree = sgf_dumptree;
  int save_count_variations = count_variations;
  
  if (same_dragon == 0)
    return;

  /* Turn off sgf output during find_superstring(). */
  sgf_dumptree = NULL;
  count_variations = 0;
  
  if (semeai_call)
    find_superstring_conservative(pos, &num_stones, stones);
  else
    find_superstring(pos, &num_stones, stones);

  /* Turn sgf output back on. */
  sgf_dumptree = save_sgf_dumptree;
  count_variations = save_count_variations;
  
  /* If same_dragon field is 1, only add if the played stone
   * clearly is in contact with the goal dragon.
   */
  if (same_dragon == 1) {
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
      }
    }

  if (0)
    goaldump(owl->goal);
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
  char boundary_mark = 0;
  int k;

  for (k = 0; k < 4; k++) {
    int pos2 = pos + delta[k];
    if (ON_BOARD(pos2) && owl->boundary[pos2] > boundary_mark)
      boundary_mark = owl->boundary[pos2];
    if (board[pos2] == owl->color
	&& dragon[pos2].color == owl->color
	&& dragon[pos2].status == ALIVE
	&& !owl->goal[pos2])
      boundary_mark = 2;
  }
  owl->boundary[pos] = boundary_mark;

  for (k = 0; k < 4; k++) {
    int pos2 = pos + delta[k];
    if (board[pos2] == board[pos]
	&& owl->boundary[pos2] < boundary_mark)
      mark_string(pos2, owl->boundary, boundary_mark);
  }
}

/* Lists the goal array. For use in GDB:
 * (gdb) set goaldump(goal).
 */

void
goaldump(const char goal[BOARDMAX])
{
  int pos;
  for (pos = BOARDMIN; pos < BOARDMAX; pos++)
    if (ON_BOARD(pos) && goal[pos])
      gprintf("%o%1m (%d)  ", pos, (int) goal[pos]);
  gprintf("\n");
}


/* Add owl reasons. This function should be called once during
 * genmove.
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
	
	/* If we've reached this far, the attack is okay. */
	if (DRAGON2(pos).owl_attack_code == GAIN) {
	  add_gain_move(move, pos, DRAGON2(pos).owl_attack_kworm );
	  DEBUG(DEBUG_OWL, "owl: %1m attacks %1m with gain at move %d\n",
		move, pos, movenum+1);
	}
	else {
	  add_owl_attack_move(move, pos, DRAGON2(pos).owl_attack_code);
	  DEBUG(DEBUG_OWL, "owl: %1m attacks %1m at move %d\n", move, pos,
		movenum+1);
	}
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
	add_gain_move(DRAGON2(pos).owl_attack_point, pos,
		      DRAGON2(pos).owl_attack_kworm);
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
  double start = 0;

  if (debug & DEBUG_OWL_PERFORMANCE)
    start = gg_cputime();

  if (worm[target].unconditional_status == DEAD)
    return 0;

  origin = dragon[target].origin;
  TRACE("owl_does_defend %1m %1m(%1m)\n", move, target, origin);

  if (search_persistent_owl_cache(OWL_DOES_DEFEND, move, target, 0,
				  &result, kworm, NULL, NULL))
    return result;

  if (trymove(move, color, "owl_does_defend", target, EMPTY, 0)) {
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
    init_owl(&owl, target, NO_MOVE, move, 1);
    prepare_goal_list(target, owl, owl_goal_worm, &goal_worms_computed,
		      kworm, 0);
    acode = do_owl_attack(target, NULL, &wid, owl, EMPTY, 0, 0);
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

  if (trymove(move, color, "owl_confirm_safety", target, EMPTY, NO_MOVE)) {
    /* Check if a compatible owl_attack() is cached. */
    if (search_persistent_owl_cache(OWL_ATTACK, origin, 0, 0,
				    &result, defense_point, kworm, NULL)) {
      popgo();
      if (result==0)
	return WIN;
      else if (result==GAIN)
	return LOSS;
      else
	return 0;
    }
    
    init_owl(&owl, target, NO_MOVE, move, 1);
    prepare_goal_list(target, owl, owl_goal_worm, &goal_worms_computed,
		      kworm, 0);
    acode = do_owl_attack(target, &defense, &wid, owl, EMPTY, NO_MOVE, 0);
    finish_goal_list(&goal_worms_computed, &wpos, owl_goal_worm, wid);
    if (acode==0)
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
    *kworm= wpos;

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
    init_owl(&owl, target, NO_MOVE, NO_MOVE, 1);
#endif

    if (trymove(move, other, "owl_does_attack", target, EMPTY, 0)) {
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
      dcode = do_owl_defend(target, NULL, &wid, owl, EMPTY, 0, 0);
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

  init_owl(&owl, target1, target2, NO_MOVE, 1);

  if (trymove(move, color, "owl_connection_defends", target1, EMPTY, 0)) {
    owl_update_goal(move, 1, owl, 0);
    if (!do_owl_attack(move, NULL, NULL, owl, EMPTY, 0, 0))
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
 * which should be included in the eyespace, e.g. in this position:
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
  char already_checked[BOARDMAX];

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

	/* If the immediate neighbor is empty, we look two steps away. */
	if (k < 4 && board[pos2] == EMPTY)
	  pos2 += delta[k];

	if (board[pos2] != other)
	  continue;

	lunch = find_origin(pos2);
	if (already_checked[lunch])
	  continue;
	already_checked[lunch] = 1;

	attack_and_defend(lunch, &acode, &apos, &dcode, &dpos);
	if (acode != 0) {
	  owl->lunch[lunches] = lunch;
	  owl->lunch_attack_code[lunches]  = acode;
	  owl->lunch_attack_point[lunches] = apos;
	  owl->lunch_defend_code[lunches]  = dcode;
	  if (dcode != 0)
	    owl->lunch_defense_point[lunches] = dpos;
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
	else if (!owl->inessential[lunch]) {
	  /* Test for inessentiality. */
	  int adj;
	  int adjs[MAXCHAIN];
	  int num_stones;
	  int stones[MAX_BOARD * MAX_BOARD];
	  int liberties;
	  int libs[MAXLIBS];
	  int r;
	  int essential = 0;
	  int superstring[BOARDMAX];

	  /* First check the neighbors of the string. */
	  adj = chainlinks(lunch, adjs);
	  for (r = 0; r < adj; r++) {
	    if (!owl->goal[adjs[r]] || attack(adjs[r], NULL) != 0) {
	      essential = 1;
	      break;
	    }
	  }

	  if (essential)
	    continue;

	  find_superstring_stones_and_liberties(lunch, &num_stones, stones,
						&liberties, libs, 0);

	  memset(superstring, 0, sizeof(superstring));
	  for (r = 0; r < num_stones; r++)
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
		if (attack(cpos, NULL) != 0) {
		  essential = 1;
		  break;
		}
		else if (owl->goal[cpos])
		  goal_found = 1;
		else {
		  essential = 1;
		  break;
		}
	      }
	      else if (board[cpos] == other
		       && !superstring[cpos]) {
		essential = 1;
		break;
	      }
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
		else if (owl->goal[bpos + delta[s]])
		  diagonal_goal++;
	      }
	      if (diagonal_goal + (off_board >= 2) < 2)
		essential = 1;
	      else {
		/* Check that the liberty is adjacent to no empty
		 * vertex, as required by 1b'.
		 */
		for (s = 0; s < 4; s++) {
		  if (board[bpos + delta[s]] == EMPTY) {
		    essential = 1;
		    break;
		  }
		}
	      }
	    }

	    if (essential)
	      break;
	  }

	  if (!essential) {
	    TRACE("Inessential string found at %1m.\n", lunch);
	    for (r = 0; r < num_stones; r++)
	      owl->inessential[stones[r]] = 1;
	  }
	}
      }
    }
  }

  owl->lunches_are_current = 1;
  sgf_dumptree = save_sgf_dumptree;
  count_variations = save_count_variations;
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
	    if (does_attack(apos, lunch) && safe_move(apos, color))
	      return apos;
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
    if (owlb->color == BLACK)
      black_eye = owlb->my_eye;
    else
      white_eye = owlb->my_eye;
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

  if (trymove(move, color, "owl_safe_move", 0, EMPTY, 0)) {
    acode = attack(move, NULL);
    if (acode != WIN)
      safe = 1;
    else
      safe = 0;
    current_owl_data->lunches_are_current = 0;
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

  if (debug & DEBUG_OWL_PERFORMANCE)
    start = gg_cputime();

  /* FIXME: We want to use the full init_owl here too (cf. similar
   * remark below).
   */
  reduced_init_owl(&owl, 1);

  owl->color = OTHER_COLOR(board[str]);
  local_owl_node_counter = 0;
  gg_assert(stackp == 0);

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
    if (trymove(libs[k], owl->color, NULL, 0, EMPTY, 0)) {
      if (level >= 8)
	increase_depth_values();
      owl->goal[libs[k]] = 1;
    }
    else {
      /* if we can't fill, try swapping with the next liberty */
      if (k < liberties-1
	  && trymove(libs[k+1], owl->color, NULL, 0, EMPTY, 0)) {
	if (level >= 8)
	  increase_depth_values();
	owl->goal[libs[k+1]] = 1;
	libs[k+1] = libs[k];
      }
      else {
	/* Can't fill the liberties. Give up! */
	while (stackp > 0) {
	  if (level >= 8)
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
  compute_owl_escape_values(owl);
  owl_mark_boundary(owl);
  owl->lunches_are_current = 0;

  if (do_owl_attack(libs[0], NULL, NULL, owl, EMPTY, 0, 0))
    result = 0;
  else
    result = 1;
  while (stackp > 0) {
    if (level >= 8)
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

  if (*probable < 2)
    eat_lunch_escape_bonus(lunch, min, probable, max, owl);
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
      *max = 2;
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
      char new_goal[BOARDMAX];
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
  char safe_stones[BOARDMAX];
  
  get_lively_stones(OTHER_COLOR(owl->color), safe_stones);
  compute_escape_influence(owl->color, safe_stones, NULL, owl->escape_values);
  DEBUG(DEBUG_ESCAPE, "Owl escape values:\n");

  for (m = 0; m < board_size; m++) {
    for (n = 0; n < board_size; n++) {
      pos = POS(m, n);
      if (dragon[pos].color == owl->color) {
	if (dragon[pos].crude_status == ALIVE)
	  owl->escape_values[pos] = 6;
	else if (dragon[pos].crude_status == UNKNOWN
		 && (DRAGON2(pos).escape_route > 5
		     || DRAGON2(pos).moyo_size > 5))
	  owl->escape_values[pos] = 4;
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
	      == BORDER_COLOR(current_owl_data->color))
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
	      == BORDER_COLOR(current_owl_data->color))
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
	  != BORDER_COLOR(current_owl_data->color)))
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
	  != BORDER_COLOR(current_owl_data->color)))
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
	   == BORDER_COLOR(current_owl_data->color))
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
  return dragon_escape(owl->goal, owl->color, owl->escape_values);
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
  if (owl_stack_size == 0) {
    if (experimental_semeai)
      owl_stack_size = gg_max(owl_reading_depth + 2,
	  		      2 * semeai_branch_depth + 4);
    else
      owl_stack_size = owl_reading_depth + 2;
    owl_stack = malloc(owl_stack_size * sizeof(*owl_stack));
    gg_assert(owl_stack != NULL);
  }

  if (at_bottom_of_stack) {
    owl_stack_pointer = 0;
    *owl = &owl_stack[0];
  }
  else {
    owl_stack_pointer++;
    *owl = &owl_stack[owl_stack_pointer];
  }
  owl_stack[owl_stack_pointer].number_in_stack = owl_stack_pointer;
}


/*
 * If use_stack is set, the stack is initialized, and the return value
 * of *owl is a pointer to the bottom of the stack.
 *
 * at_bottom_of_stack = 1 means **owl will be initialized at the bottom
 * 	of the stack
 * (otherwise, it will be set at the lowest available spot in the stack)
 */
static void
init_owl(struct local_owl_data **owl, int target1, int target2, int move,
         int at_bottom_of_stack)
{
  reduced_init_owl(owl, at_bottom_of_stack);

  local_owl_node_counter = 0;
  (*owl)->lunches_are_current = 0;
  owl_mark_dragon(target1, target2, *owl);
  if (move != NO_MOVE)
    owl_update_goal(move, 1, *owl, 0);
  compute_owl_escape_values(*owl);
}


/***********************
 * Storage of owl data
 ***********************/

/* Push owl data one step upwards in the stack. Gets called from
 * push_owl.
 */
static void
do_push_owl(struct local_owl_data **owl)
{
  struct local_owl_data *new_owl = &owl_stack[++owl_stack_pointer];

  gg_assert(&owl_stack[(*owl)->number_in_stack] == *owl);

  /* Copy the owl data. */
  memcpy(new_owl->goal, (*owl)->goal, sizeof(new_owl->goal));
  memcpy(new_owl->boundary, (*owl)->boundary, sizeof(new_owl->boundary));
  memcpy(new_owl->escape_values, (*owl)->escape_values,
	 sizeof(new_owl->escape_values));
  new_owl->color = (*owl)->color;

  new_owl->lunches_are_current = 0;

  /* Needed for stack organization: */
  new_owl->number_in_stack = owl_stack_pointer;
  new_owl->restore_from = (*owl)->number_in_stack;

  /* Finally move the *owl pointer. */
  *owl = new_owl;
}


/* Push owl data one step upwards in the stack. The stack is dynamically
 * reallocated if it is too small. Second argument is used from the
 * semeai code; use NULL otherwise.
 *
 * If you use push_owl with two arguments, later call
 * pop_owl(&owlb); pop_owl(&owla);
 * in this order!
 *
 * Note that the owl stack might get moved in this function. This means
 * that all pointers to the owl stack will get invalid. Only the pointers
 * owla (and owlb) at the current recursion depth get corrected immediately.
 * All other pointers will get corrected when pop_owl() is called. 
 */
static void
push_owl(struct local_owl_data **owla, struct local_owl_data **owlb)
{
  /* Do we need to enlarge the stack? */
  if (owl_stack_pointer == owl_stack_size - 1
      || (owl_stack_pointer == owl_stack_size - 2 && owlb)) {
    int num_a = (*owla)->number_in_stack;
    int num_b = 0;
    struct local_owl_data *old_stack_loc = owl_stack;
    gg_assert(*owla == &(owl_stack[num_a]));
    if (owlb) {
      num_b = (*owlb)->number_in_stack;
      gg_assert(*owlb == &(owl_stack[num_b]));
    }  
    if (0) {
      gprintf("Have to enlarge owl stack! (size %d, owl_stack %d, stackp %d)\n",
	      owl_stack_size, owl_stack_pointer, stackp);
      dump_stack();
    }
    /* Better reallocate too much, than to have reallocate more often: */
    owl_stack_size += 2;
    owl_stack = realloc(owl_stack, owl_stack_size * sizeof(*owl_stack));
    gg_assert(owl_stack != NULL);
    if (0 && (owl_stack != old_stack_loc))
      gprintf("Stack has moved! New stack size %d.\n", owl_stack_size);
    *owla = &(owl_stack[num_a]);
    if (owlb)
      *owlb = &(owl_stack[num_b]);
  }

  do_push_owl(owla);
  if (owlb)
    do_push_owl(owlb);
}


/* Retrieve owl data from the stack. */
static void
pop_owl(struct local_owl_data **owl)
{
  *owl = &(owl_stack[owl_stack[owl_stack_pointer].restore_from]);
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
  if (1 && (w > 0) && (w < MAX_GOAL_WORMS) ) {
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
