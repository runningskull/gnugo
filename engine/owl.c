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



#define MAX_MOVES 3           /* maximum number of branches at each node */
#define MAX_SEMEAI_MOVES 2    /* semeai branch factor--must be <= MAX_MOVES */
#define MAX_LUNCHES 10

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "liberty.h"
#include "patterns.h"
#include "cache.h"
#include "sgftree.h"

struct local_owl_data {
  char goal[BOARDMAX];
  char boundary[BOARDMAX];
  int escape_values[BOARDMAX];
  int color;

  struct eye_data black_eye[BOARDMAX];
  struct eye_data white_eye[BOARDMAX];
  /* array of half-eye data for use during owl reading */
  struct half_eye_data half_eye[BOARDMAX];
  
  int lunch[MAX_LUNCHES];
  int lunch_attack_code[MAX_LUNCHES];
  int lunch_attack_point[MAX_LUNCHES];
  int lunch_defend_code[MAX_LUNCHES];
  int lunch_defense_point[MAX_LUNCHES];
  int inessential[BOARDMAX];
  
  int lunches_are_current; /* If true, owl lunch data is current */  

  /* Node limitation. */
  int local_owl_node_counter;
};


static int owl_safe_move_cache[BOARDMAX];
static int result_certain;

/* Statistics. */
static int global_owl_node_counter = 0;

static struct local_owl_data *current_owl_data;
static struct local_owl_data *other_owl_data;

struct owl_move_data {
  int pos;          /* move coordinate */
  int value;        /* value */
  const char *name; /* name of the pattern suggesting the move */
  int same_dragon;  /* whether the move extends the dragon or not */
};


/* Persistent owl result cache to reuse owl results between moves. */
struct owl_cache {
  char board[BOARDMAX];
  int movenum;
  int tactical_nodes;
  int routine;
  int apos;  /* first input coordinate */
  int bpos;  /* second input coordinate */
  int cpos;  /* third input coordinate */
  int result;
  int result_certain;
  int move;  /* first result coordinate */
  int move2;  /* second result coordinate */
};

#define MAX_OWL_CACHE_SIZE 80
static struct owl_cache persistent_owl_cache[MAX_OWL_CACHE_SIZE];
static int persistent_owl_cache_size = 0;

#define OWL_THREATEN_ATTACK    0
#define OWL_THREATEN_DEFENSE   1
#define OWL_DOES_DEFEND        2
#define OWL_DOES_ATTACK        3
#define OWL_CONNECTION_DEFENDS 4
#define OWL_SUBSTANTIAL        5
#define OWL_CONFIRM_SAFETY     6
/* The following two are defined in cache.h */
/* #define OWL_ATTACK    8 */
/* #define OWL_DEFEND    9 */

static int verify_stored_board(char board[BOARDMAX]);
static int search_persistent_owl_cache(int routine, int apos,
				       int bpos, int cpos,
				       int *result, int *move,
				       int *move2, int *certain);
static void store_persistent_owl_cache(int routine, int apos,
				       int bpos, int cpos,
				       int result, int move,
				       int move2, int certain,
				       int tactical_nodes,
				       char goal[BOARDMAX],
				       int goal_color);
static void print_persistent_owl_cache_entry(int k);
static void mark_dragon_hotspot_values(float values[MAX_BOARD][MAX_BOARD],
				       int pos, float contribution);


static int do_owl_attack(int str, int *move, 
			 struct local_owl_data *owl,
			 int komaster, int kom_pos);
static int do_owl_defend(int str, int *move,
			 struct local_owl_data *owl,
			 int komaster, int kom_pos);
static int owl_shapes(struct owl_move_data moves[MAX_MOVES], int color,
		      struct local_owl_data *owl,
		      struct pattern_db *type);
static void owl_shapes_callback(int m, int n, int color,
				struct pattern *pattern_db,
				int ll, void *data);
static void owl_add_move(struct owl_move_data *moves, int move, int value,
			 const char *reason, int same_dragon);
static int owl_determine_life(struct local_owl_data *owl,
			      struct eye_data eye[BOARDMAX],
			      int color, int komaster, int does_attack,
			      struct owl_move_data *moves, int *probable_min,
			      int *probable_max);
static void owl_mark_dragon(int ai, int aj, int bi, int bj,
			    struct local_owl_data *owl);
static void owl_mark_boundary(struct local_owl_data *owl);
static void owl_update_goal(int pos, int same_dragon,
			    struct local_owl_data *owl);
static void owl_update_boundary_marks(int pos, struct local_owl_data *owl);
static void owl_find_lunches(struct local_owl_data *owl);
static void owl_make_domains(struct local_owl_data *owla,
			     struct local_owl_data *owlb);
static int owl_safe_move(int move, int color);
static void sniff_lunch(int lunch, int *min, int *probable, int *max,
			struct local_owl_data *owl);
static void compute_owl_escape_values(struct local_owl_data *owl);
static int owl_escape_route(struct local_owl_data *owl);
static void do_owl_analyze_semeai(int apos, int bpos, 
		      struct local_owl_data *owla,
		      struct local_owl_data *owlb, int komaster,
		      int *resulta, int *resultb,
		      int *move, int pass);
static int liberty_of_goal(int pos, struct local_owl_data *owl);
static int matches_found;
static char found_matches[BOARDMAX];

static struct local_owl_data *owl_stack = NULL;
static int owl_stack_size = 0;
static int owl_stack_pointer = 0;
static void push_owl(struct local_owl_data *owl);
static void pop_owl(struct local_owl_data *owl);

/* Called when (ai,aj) and (bi,bj) point to adjacent dragons
 * of the opposite color, both with matcher_status DEAD or
 * CRITICAL, analyzes the semeai, assuming that the player
 * of the (ai,aj) dragon moves first.
 */

static int owl_phase;

void
owl_analyze_semeai(int ai, int aj, int bi, int bj)
{
  static struct local_owl_data owla;
  static struct local_owl_data owlb;
  int resulta, resultb;
  int color = BOARD(ai, aj);
  int move;
  
  gg_assert(BOARD(ai, aj) == OTHER_COLOR(BOARD(bi, bj)));
  owl_phase = 1;
  TRACE("owl_analyze_semeai: %m vs. %m\n", ai, aj, bi, bj);
  owla.lunches_are_current = 0;
  owlb.lunches_are_current = 0;
  owl_mark_dragon(ai, aj, -1, -1, &owla);
  owl_mark_dragon(bi, bj, -1, -1, &owlb);
  compute_owl_escape_values(&owla);
  compute_owl_escape_values(&owlb);
  owl_make_domains(&owla, &owlb);
  do_owl_analyze_semeai(POS(ai, aj), POS(bi, bj), &owla, &owlb, EMPTY,
			&resulta, &resultb, &move, 0);
  gprintf("If %s moves first (at %1m), %m is %s, %m is %s\n",
	  color == BLACK ? "black" : "white",
	  move,
	  ai, aj, status_to_string(resulta),
  	  bi, bj, status_to_string(resultb));
}

/* It is assumed that the (ai,aj) player moves first, and
 * determines the best result for both players. The
 * parameter "pass" is 1 if the opponent's last move is
 * pass. In this case, if no move is found but the genus
 * is less than 1, then the position is declared seki.
 *
 * If a move is needed to get this result, then (*ti,*tj) is
 * the location, otherwise this field returns (-1,-1) (tenuki).
 */

static void
do_owl_analyze_semeai(int apos, int bpos, 
		      struct local_owl_data *owla,
		      struct local_owl_data *owlb, int komaster,
		      int *resulta, int *resultb,
		      int *move, int pass)
{
  int color = board[apos];
  int other = OTHER_COLOR(color);
  struct owl_move_data vital_defensive_moves[MAX_MOVES];
  struct owl_move_data vital_offensive_moves[MAX_MOVES];
  struct owl_move_data shape_defensive_moves[MAX_MOVES];
  struct owl_move_data shape_offensive_moves[MAX_MOVES];
  struct owl_move_data moves[2*MAX_SEMEAI_MOVES+2];
  struct owl_move_data outside_liberty;
  struct owl_move_data common_liberty;
  struct owl_move_data backfilling_move;
  char saved_goal[BOARDMAX];
  int safe_outside_liberty_found = 0;
  int unsafe_outside_liberty_found = 0;
  int safe_common_liberty_found = 0;
  int unsafe_common_liberty_found = 0;
  int backfilling_move_found = 0;
  int best_resulta = UNKNOWN;
  int best_resultb = UNKNOWN;
  int best_move = 0;
  int this_resulta = UNKNOWN;
  int this_resultb = UNKNOWN;
  char mw[BOARDMAX];  
  int k;
  int m, n;
  int same_dragon;
  int save_owl_phase = owl_phase;
  SGFTree *save_sgf_dumptree = sgf_dumptree;
  int save_count_variations = count_variations;

  if (count_variations > 100)
    return;
  outside_liberty.pos = 0;
  common_liberty.pos = 0;
  backfilling_move.pos = 0;
  /* turn off the sgf file and variation counting */
  sgf_dumptree = NULL;
  count_variations = 0;
  for (k = 0; k < 2*MAX_SEMEAI_MOVES+2; k++) {
    moves[k].pos = 0;
    moves[k].value = -1;
    moves[k].name = NULL;
    moves[k].same_dragon = 1;
  }
  gg_assert(other == board[bpos]);
  memset(mw, 0, sizeof(mw));
  /* 
   * We generate the candidate moves. During the early stages of
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
    int probable_mina, probable_maxa, probable_minb, probable_maxb;

    /* We do not wish for any string of the 'b' dragon to be 
     * counted as a lunch of the 'a' dragon since owl_determine_life 
     * can give a wrong result in the case of a semeai. So we eliminate 
     * such lunches.
     */
    
    owl_find_lunches(owla);
    owl_find_lunches(owlb);
    for (k = 0; k < MAX_LUNCHES; k++) {
      if (owla->lunch[k] != NO_MOVE 
	  && (owlb->goal)[owla->lunch[k]]) {
	owla->lunch[k] = NO_MOVE;
      }
    }
    if (color == BLACK)
      owl_determine_life(owla, owla->black_eye,
			 BLACK, komaster, 1, 
			 vital_defensive_moves,
			 &probable_mina, &probable_maxa);
    else
      owl_determine_life(owla, owla->white_eye,
			 WHITE, komaster, 1, 
			 vital_defensive_moves,
			 &probable_mina, &probable_maxa);
    
    if (other == BLACK)
      owl_determine_life(owlb, owlb->black_eye,
			 BLACK, komaster, 0, 
			 vital_offensive_moves,
			 &probable_minb, &probable_maxb);
    else
      owl_determine_life(owlb, owlb->white_eye,
			 WHITE, komaster, 1, 
			 vital_offensive_moves,
			 &probable_minb, &probable_maxb);
    
    /* Certain cases can be handled immediately. */
    /* I live, you die, no move needed. */
    if ((probable_mina >= 2) && (probable_maxb < 2)) {
      *resulta = ALIVE;
      *resultb = DEAD;
      if (move)
	*move = 0;
      sgf_dumptree = save_sgf_dumptree;
      count_variations =   save_count_variations;
      return;
    }
    /* I am alive */
    if ((probable_mina >= 2) ||
	(stackp > 2 && owl_escape_route(owla) >= 5)) {
      if (probable_maxb < 2) {
	/* you are already dead */
	*resulta = ALIVE;
	*resultb = DEAD;
	if (move) *move = 0;
	sgf_dumptree = save_sgf_dumptree;
	count_variations = save_count_variations;
	return;
      }
      else if (probable_minb < 2) {
	/* I can kill */
	gg_assert(vital_offensive_moves[0].pos != 0);
	*resulta = ALIVE;
	*resultb = DEAD;
	if (move)
	  *move = vital_offensive_moves[0].pos;
	sgf_dumptree = save_sgf_dumptree;
	count_variations = save_count_variations;
	return;
      }
      else {
	/* both live */
	 *resulta = ALIVE;
	 *resultb = ALIVE;
	 if (move) *move = 0;
	 sgf_dumptree = save_sgf_dumptree;
	 count_variations = save_count_variations;
	 return;
      }
    }
    if ((probable_minb >= 2) || owl_escape_route(owlb) >= 5) {
      /* you are alive */
      if (probable_maxa < 2) {
	/* I am dead */
	*resulta = DEAD;
	*resultb = ALIVE;
	if (move) *move = 0;
	sgf_dumptree = save_sgf_dumptree;
	count_variations = save_count_variations;
	return;
      }
      else if (probable_mina <2) {
	/* I can live */
	gg_assert(vital_defensive_moves[0].pos != 0);
	*resulta = ALIVE;
	*resultb = ALIVE;
	if (move) *move = vital_defensive_moves[0].pos;
	sgf_dumptree = save_sgf_dumptree;
	count_variations = save_count_variations;
	return;
      }
      else {
	/* I die */
	*resulta = DEAD;
	*resultb = ALIVE;
	if (move) *move = 0;
	sgf_dumptree = save_sgf_dumptree;
	count_variations = save_count_variations;
	return;
      }
    }
    
    /* Next the shape moves. FIXME: We generate more moves than we use if
     * MAX_SEMEAI_MOVE < MAX_MOVES.  
     */
    
    owl_shapes(shape_defensive_moves, color, owla, 
	       &owl_defendpat_db);
    owl_shapes(shape_offensive_moves, color, owlb, 
	       &owl_attackpat_db);

    /* Now we review the moves already considered, while collecting
     * them into a single list. If no owl moves are found, we end the owl
     * phase. If no owl move of value > 30 is found, we want to be sure that we
     * have included a move that fills a liberty. If no such move is found, we
     * will have to add it later.
     */
  
    for (k = 0; 
	 k < MAX_SEMEAI_MOVES && vital_defensive_moves[k].pos != 0; k++) {
      if (liberty_of_goal(vital_defensive_moves[k].pos, owlb)) {
	if (!liberty_of_goal(vital_defensive_moves[k].pos, owla)) {
	  if (safe_move(vital_defensive_moves[k].pos, color))
	    safe_outside_liberty_found = 1;
	  else
	    unsafe_outside_liberty_found = 1;
	}
	else {
	  if (safe_move(vital_defensive_moves[k].pos, color))
	    safe_common_liberty_found = 1;
	  else
	    unsafe_common_liberty_found = 1;
	}
      }
      mw[vital_defensive_moves[k].pos] = 1;
      owl_add_move(moves, vital_defensive_moves[k].pos,
		   vital_defensive_moves[k].value,
		   "vital defensive move", 
		   vital_defensive_moves[k].same_dragon);
    }
    for (k = 0; 
	 k < MAX_SEMEAI_MOVES && vital_offensive_moves[k].pos != 0; k++) {
      if (liberty_of_goal(vital_offensive_moves[k].pos, owlb)) {
	if (!liberty_of_goal(vital_offensive_moves[k].pos, owla)) {
	  if (safe_move(vital_offensive_moves[k].pos, color))
	    safe_outside_liberty_found = 1;
	  else
	    unsafe_outside_liberty_found = 1;
	}
	else {
	  if (safe_move(vital_offensive_moves[k].pos, color))
	    safe_common_liberty_found = 1;
	  else
	    unsafe_common_liberty_found = 1;
	}
      }
      mw[vital_offensive_moves[k].pos] = 1;
      if (liberty_of_goal(vital_offensive_moves[k].pos, owla))
	same_dragon = 1;
      else
	same_dragon = 0;
      owl_add_move(moves, vital_offensive_moves[k].pos,
		   vital_offensive_moves[k].value,
		   vital_offensive_moves[k].name, same_dragon);
    }
    for (k = 0; 
	 k < MAX_SEMEAI_MOVES && shape_defensive_moves[k].pos != 0; k++) {
      if (liberty_of_goal(shape_defensive_moves[k].pos, owlb)) {
	if (!liberty_of_goal(shape_defensive_moves[k].pos, owla)) {
	  if (safe_move(shape_defensive_moves[k].pos, color))
	    safe_outside_liberty_found = 1;
	  else
	    unsafe_outside_liberty_found = 1;
	}
	else {
	  if (safe_move(shape_defensive_moves[k].pos, color))
	    safe_common_liberty_found = 1;
	  else
	    unsafe_common_liberty_found = 1;
	}
      }
      mw[shape_offensive_moves[k].pos] = 1;
      owl_add_move(moves, shape_defensive_moves[k].pos,
		   shape_defensive_moves[k].value,
		   shape_defensive_moves[k].name,
		   shape_defensive_moves[k].same_dragon);
    }
    for (k= 0; 
	 k < MAX_SEMEAI_MOVES && shape_offensive_moves[k].pos != 0; k++) {
      if (liberty_of_goal(shape_offensive_moves[k].pos, owlb)) {
	if (!liberty_of_goal(shape_offensive_moves[k].pos, owla)) {
	  if (safe_move(shape_offensive_moves[k].pos, color))
	    safe_outside_liberty_found = 1;
	  else
	    unsafe_outside_liberty_found = 1;
	}
	else {
	  if (safe_move(shape_offensive_moves[k].pos, color))
	    safe_common_liberty_found = 1;
	  else
	    unsafe_common_liberty_found = 1;
	}
      }
      mw[shape_offensive_moves[k].pos] = 1;
      if (liberty_of_goal(shape_offensive_moves[k].pos, owla))
	same_dragon = 1;
      else
	same_dragon = 0;
      owl_add_move(moves, shape_offensive_moves[k].pos,
		   shape_offensive_moves[k].value,
		   shape_offensive_moves[k].name,
		   same_dragon);
    }
    /* If no owl moves were found, turn off the owl phase */
    if (moves[0].pos == 0)
      owl_phase = 0;
  }
  /* now we look for a move to fill a liberty. There may
   * already be such a move on the list.
   */
  if (!safe_outside_liberty_found) {
    for (m = 0; !safe_outside_liberty_found && m < board_size; m++)
      for (n = 0; !safe_outside_liberty_found && n < board_size; n++) {
	int pos = POS(m, n);
	if (board[pos] == EMPTY && !mw[pos]) {
	  if (liberty_of_goal(pos, owlb)) {
	    if (!liberty_of_goal(pos, owla)) {
	      /* outside liberty */
	      if (safe_move(pos, color)) {
		safe_outside_liberty_found = 1;
		outside_liberty.pos = pos;
	      }
	      else if (!safe_outside_liberty_found) {
		int libs[2];
		
		unsafe_outside_liberty_found = 1;
		outside_liberty.pos = pos;
		
		if (accurate_approxlib(pos, color, 2, libs) == 1) {
		  backfilling_move_found = 1;
		  backfilling_move.pos = libs[0];
		}
	      }
	    }
	    else {
	      /* common liberty */
	      if (safe_move(pos, color)) {
		safe_common_liberty_found = 1;
		common_liberty.pos = pos;
	      }
	      else
		unsafe_common_liberty_found = 1;
	    }
	  }
	}
      }
  }
  /* Look for a tactical attack. We seek a worm of owlb which adjoins 
   * a worm of owla and which can be attacked. If such exists, we 
   * declare victory. We limit the search to strings with two or 
   * fewer liberties to avoid having to increase the depth parameters.
   */
  {
    int ma[BOARDMAX];
    int origin;
    int upos;

    memset(ma, 0, sizeof(ma));
    for (m = 0; m < board_size; m++)
      for (n = 0; n < board_size; n++)
	if ((owlb->goal)[POS(m, n)]) {
	  origin = find_origin(POS(m, n));
	  if (!ma[origin] &&
	      ((m > 0 && (owla->goal)[POS(m-1, n)])
	       || (m < board_size-1 && (owla->goal)[POS(m+1, n)])
	       || (n > 0 && (owla->goal)[POS(m, n-1)])
	       || (n <board_size-1 && (owla->goal)[POS(m, n+1)]))) {
	    if (countlib(origin) < 3 && attack(origin, &upos)) {
	      *resulta = ALIVE;
	      *resultb = DEAD;
	      if (move) *move = upos;
	      sgf_dumptree = save_sgf_dumptree;
	      count_variations = save_count_variations;
	      return;
	    }
	    /* we mark the strings we've tried and failed to prevent 
	     * duplicate reading.
	     */
	    else
	      ma[origin] = 1;
	  }
	}
  }
  
  if (safe_outside_liberty_found
      && outside_liberty.pos != 0)
    owl_add_move(moves, outside_liberty.pos, 50, "safe outside liberty", 0);
  else {
    if (unsafe_outside_liberty_found
	&& outside_liberty.pos != 0) {
      owl_add_move(moves, outside_liberty.pos, 30,
		   "unsafe outside liberty", 0);
      if (backfilling_move_found
	  && backfilling_move.pos != 0)    
	owl_add_move(moves, backfilling_move.pos, 30, "backfilling move", 0);
    }
    else if (safe_common_liberty_found
	     && common_liberty.pos != 0)
      owl_add_move(moves, common_liberty.pos, 20, "safe common liberty", 1);
  }
  /* Now we are ready to try moves. Turn on the sgf output ... */
  sgf_dumptree = save_sgf_dumptree;
  count_variations = save_count_variations;
  memcpy(saved_goal, owla->goal, sizeof(saved_goal));
  for (k = 0; k < 2*MAX_SEMEAI_MOVES+2; k++) {
    int mpos = moves[k].pos;
    int mi = I(mpos);
    int mj = J(mpos);

    if (mpos != 0 
	&& trymove(mpos, color, moves[k].name, apos, EMPTY, 0)) {
      dump_stack();
      if (moves[k].same_dragon)
	mark_string(POS(mi, mj), owla->goal, 1);
      owla->lunches_are_current = 0;
      owl_update_boundary_marks(mpos, owla);
      if (liberty_of_goal(mpos, owla))
	(owla->goal)[POS(mi, mj)] = 1;
      do_owl_analyze_semeai(bpos, apos, owlb, owla, komaster,
			    &this_resultb, &this_resulta, NULL, 0);
      if ((this_resultb == DEAD) && (this_resulta == ALIVE)) {
	memcpy(owla->goal, saved_goal, sizeof(saved_goal));
	popgo();
	owl_phase = save_owl_phase;
	*resulta = ALIVE;
	*resultb = DEAD;
	if (move) *move = mpos;
	return;
      }
      if (this_resulta == ALIVE_IN_SEKI
	  && this_resultb == ALIVE_IN_SEKI
	  && best_resulta != ALIVE) {
	best_resulta = ALIVE_IN_SEKI;
	best_resultb = ALIVE_IN_SEKI;
	best_move = mpos;
      }
      if (this_resulta == DEAD
	  && this_resultb == ALIVE
	  && best_resulta == UNKNOWN) {
	best_resulta = DEAD;
	best_resultb = ALIVE;
	best_move = mpos;
      }
      memcpy(owla->goal, saved_goal, sizeof(saved_goal));
      popgo();
      owl_phase = save_owl_phase;
    }
  }
  /* If the opponent passed and you can't find a safe move, it's seki */
  if (moves[k].value < 40 && pass == 1) {
    *resulta = ALIVE_IN_SEKI;
    *resultb = ALIVE_IN_SEKI;
    if (move) *move = 0;
    return;
  }
  /* If we can't do better than seki, try passing */
  if (best_resulta != ALIVE
      && best_resulta != UNKNOWN) {
    do_owl_analyze_semeai(bpos, apos, owlb, owla, komaster,
			  &this_resultb, &this_resulta, NULL, 0);
    if (this_resulta == ALIVE) {
      *resulta = ALIVE;
      *resultb = DEAD;
      if (move) *move = 0;
      return;
    }
    if (this_resulta == DEAD) {
      if ((best_resulta == DEAD) 
	  || (best_resulta == UNKNOWN))
	{
	  *resulta = DEAD;
	  *resultb = ALIVE;
	  if (move) *move = 0;
	  return;
	}
      else {
	*resulta = best_resulta;
	*resultb = best_resultb;
	if (move) *move = best_move;
      return;
      }
    }
    if (this_resulta == ALIVE_IN_SEKI
	&& best_resulta != ALIVE) {
      *resulta = ALIVE_IN_SEKI;
      *resultb = ALIVE_IN_SEKI;
      if (move) *move = 0;
      return;
    }
  }
  else {
    *resulta = best_resulta;
    *resultb = best_resultb;
    if (move) *move = best_move;
    return;
  }
}

				   
/* If (i,j) points to an empty intersection, returns true if
 * this spot is adjacent to an element of the owl goal.  */

static int
liberty_of_goal(int pos, struct local_owl_data *owl)
{
  int i = I(pos);
  int j = J(pos);
  if ((i > 0 && (owl->goal)[POS(i-1, j)])
      || (i < board_size-1 && (owl->goal)[POS(i+1, j)])
      || (j > 0 && (owl->goal)[POS(i, j-1)])
      || (j < board_size-1 && (owl->goal)[POS(i, j+1)]))
    return 1;
  return 0;
}


/* Returns true if a move can be found to attack the dragon
 * at (m,n), in which case (*ui,*uj) is the recommended move.
 * (*ui,*uj) can be null pointers if only the result is needed.
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
 * */

int
owl_attack(int m, int n, int *ui, int *uj, int *certain)
{
  int result;
  static struct local_owl_data owl;
  int reading_nodes_when_called = get_reading_node_counter();
  clock_t start = 0, end;
  double elapsed;
  int tactical_nodes;
  int str = POS(m, n);
  int move = 0;

  result_certain = 1;
  if (worm[POS(m, n)].unconditional_status == DEAD) {
    if (ui) *ui = -1;
    if (uj) *uj = -1;
    return 1;
  }

  if (search_persistent_owl_cache(OWL_ATTACK, str, 0, 0, &result,
				  &move, NULL, certain)) {
    if (ui) *ui = I(move);
    if (uj) *uj = J(move);
    return result;
  }

  if (debug & DEBUG_OWL_PERFORMANCE)
    start = clock();
  owl.local_owl_node_counter = 0;
  TRACE("owl_attack %m\n", m, n);
  owl.lunches_are_current = 0;
  owl_mark_dragon(m, n, -1, -1, &owl);
  compute_owl_escape_values(&owl);
  owl_make_domains(&owl, NULL);
  result = do_owl_attack(str, &move, &owl, EMPTY, 0);
  tactical_nodes = get_reading_node_counter() - reading_nodes_when_called;

  if (debug & DEBUG_OWL_PERFORMANCE) {
    end = clock();
    elapsed = ((double) (end - start)) / CLOCKS_PER_SEC;
    gprintf("owl_attack %m, result %d %1m (%d, %d nodes, %f seconds)\n",
	    m, n, result, move, owl.local_owl_node_counter,
	    tactical_nodes, elapsed);
  }

  store_persistent_owl_cache(OWL_ATTACK, str, 0, 0,
			     result, move, 0,
			     result_certain, tactical_nodes,
			     owl.goal, board[str]);

  if (ui) *ui = I(move);
  if (uj) *uj = J(move);
  if (certain) *certain = result_certain;

  return result;
}


/* Static function containing the main recursive code for 
 * owl_attack.
 */

static int
do_owl_attack(int str, int *move, struct local_owl_data *owl,
	      int komaster, int kom_pos)
{
  int color = board[str];
  int other = OTHER_COLOR(color);
  struct owl_move_data vital_moves[MAX_MOVES];
  struct owl_move_data shape_moves[MAX_MOVES];
  struct owl_move_data *moves;
  char mw[BOARDMAX];
  int number_tried_moves = 0;
  int pass;
  int k;
  int savemove = 0;
  int savecode = 0;
  int true_genus = -1;
  int probable_min = -1;
  int probable_max = -1;
  int move_cutoff;
  int dcode;
  int found_read_result;
  Read_result *read_result;
  int this_variation_number = count_variations - 1;
  int m = I(str);
  int n = J(str);
  
  SETUP_TRACE_INFO("owl_attack", str);

  if ((stackp <= owl_branch_depth) && (hashflags & HASH_OWL_ATTACK)) {
    found_read_result = get_read_result(OWL_ATTACK, komaster, kom_pos,
					&str, &read_result);
    if (found_read_result) {
      TRACE_CACHED_RESULT(*read_result);
      if (rr_get_result(*read_result) != 0) {
	if (move)
	  *move = rr_get_move(*read_result);
      }

      if (rr_get_result(*read_result) == WIN)
	TRACE("%oVariation %d: DEAD (cached)\n", this_variation_number);
      else
	TRACE("%oVariation %d: ALIVE (cached)\n", this_variation_number);

      SGFTRACE(rr_get_move(*read_result), rr_get_result(*read_result),
	       "cached");
      return rr_get_result(*read_result);
    }

    /* This data should always be recorded. */
    if (read_result) {
      rr_set_compressed_data(*read_result, OWL_ATTACK, komaster, kom_pos,
			     str, stackp);
    }
  }
  else
    read_result = NULL;

  /* If we're deeper than owl_reading_depth, assume the dragon has
   * managed to escape.
   */
  if (stackp > owl_reading_depth) {
    TRACE("%oVariation %d: ALIVE (maximum reading depth reached)\n",
	  this_variation_number);
    SGFTRACE(0, 0, "max reading depth reached");
    READ_RETURN0(read_result);
  }
  
  /* If the owl node limit has been reached, assume the dragon has
   * managed to escape.
   */
  if (owl->local_owl_node_counter >= owl_node_limit) {
    result_certain = 0;
    TRACE("%oVariation %d: ALIVE (owl node limit reached)\n",
	  this_variation_number);
    SGFTRACE(0, 0, "owl node limit reached");
    READ_RETURN0(read_result);
  }

  memset(mw, 0, sizeof(mw));
  global_owl_node_counter++;
  owl->local_owl_node_counter++;

  /* Always start with picking up the vital moves so we can see if
   * there is any chance to kill.
   */
  {
    SGFTree *save_sgf_dumptree = sgf_dumptree;
    int save_count_variations = count_variations;
    
    sgf_dumptree = NULL;
    count_variations = 0;
    if (color == BLACK)
      true_genus = owl_determine_life(owl, owl->black_eye,
				      BLACK, komaster, 1, vital_moves,
				      &probable_min, &probable_max);
    else 
      true_genus = owl_determine_life(owl, owl->white_eye,
				      WHITE, komaster, 1, vital_moves,
				      &probable_min, &probable_max);
    
    current_owl_data = owl;
    memset(owl_safe_move_cache, 0, sizeof(owl_safe_move_cache));

    matches_found = 0;
    memset(found_matches, 0, sizeof(found_matches));
    if (level >= 9)
      global_matchpat(owl_shapes_callback, other, 
		      &owl_vital_apat_db, vital_moves, owl->goal);
    
    sgf_dumptree = save_sgf_dumptree;
    count_variations = save_count_variations;
    
    true_genus -= matches_found;

    if (true_genus >= 2
	|| (true_genus == 1 && probable_min >= 4)
	|| (stackp > owl_distrust_depth
	    && probable_min >= 2
	    && !matches_found)) {
      TRACE("%oVariation %d: ALIVE (2 or more secure eyes)\n",
	    this_variation_number, true_genus);
      SGFTRACE(0, 0, "2 or more secure eyes");
      READ_RETURN(read_result, move, 0, 0);
    }
  }

  /* We try moves in five passes.
   *                                stackp==0   stackp>0
   * 0. Vital moves in the interval  [70..]      [45..]
   * 1. Shape moves
   * 2. Vital moves in the interval  [..69]      [..44]
   * 3. Tactical attack moves
   * 4. Moves found by the defender
   */
  for (pass = 0; pass < 5; pass++) {
    moves = NULL;
    move_cutoff = 1;
    
    /* Get the shape moves if we are in the right pass. */
    if (pass == 1) {

      if (stackp > owl_branch_depth && number_tried_moves > 0)
	continue;
      
      owl_shapes(shape_moves, other, owl, &owl_attackpat_db);
      /* A move of value 100 is considered a win */
      if (shape_moves[0].value >= 100) {
	/* to make sure this move is recorded in the sgf file */
	if (trymove(shape_moves[0].pos, other,
		    shape_moves[0].name, str, komaster, kom_pos))
	  popgo();
	TRACE("%oVariation %d: DEAD (Winning owl_attackpat)\n",
	      this_variation_number);
	SGFTRACE(shape_moves[0].pos, WIN, "winning attack pattern");
	READ_RETURN(read_result, move, shape_moves[0].pos, WIN);
      }

      /* A move of value 99 is considered a forced move. No other move need
       * be considered. If there are two of these on the board, we lose.
       */
      if (shape_moves[0].value == 99) {
	if (shape_moves[1].value == 99) {
	  TRACE("%oVariation %d: ALIVE (multiple forced moves)\n",
		this_variation_number);
	  SGFTRACE(0, 0, "multiple forced moves");
	  READ_RETURN0(read_result);
	}
	move_cutoff = 99;
      }

      moves = shape_moves;
    }
    else if (pass == 0 || pass == 2) {
      
      if (stackp > owl_branch_depth && number_tried_moves > 0)
	continue;
      
      moves = vital_moves;
      if (pass == 0 || stackp > owl_distrust_depth) {
	if (stackp == 0)
	  move_cutoff = 70;
	else
	  move_cutoff = 45;
      }
      if (probable_max < 2 && stackp > 2)
	move_cutoff = 99; /* Effectively disable vital moves. */
    }
    else if (pass == 3) {
      /* Look for a tactical attack. This is primarily intended for
       * the case where the whole dragon is a single string, therefore
       * we only look at the string at the "origin".
       *
       * We must be wary with attacks giving ko. Unless the dragon
       * otherwise looks alive, this may turn a dead dragon into one
       * which can live by ko.
       */
      int apos;
      int result;
      SGFTree *save_sgf_dumptree = sgf_dumptree;
      int save_count_variations = count_variations;
      
      sgf_dumptree = NULL;
      count_variations = 0;
      result = attack(POS(m, n), &apos);
      if (result == WIN || (result != 0 && probable_min >= 2)) {
	shape_moves[0].pos         = apos;
	shape_moves[0].value       = 25;
	shape_moves[0].name        = "tactical attack";
	shape_moves[0].same_dragon = 2;
	shape_moves[1].value       = 0;
	moves = shape_moves;
      }
      sgf_dumptree = save_sgf_dumptree;
      count_variations = save_count_variations;
    }

    /* If we found no move in the first four passes we ask the defender
     * for a move suggestion.
     */
    if (pass == 4 && number_tried_moves == 0) {
      int dpos;
      dcode = do_owl_defend(str, &dpos, owl, komaster, kom_pos);
      /* No defense, we won. */
      if (dcode == 0) {
	TRACE("%oVariation %d: DEAD (no defense)\n",
	      this_variation_number);
	SGFTRACE(0, WIN, "no defense");
	READ_RETURN(read_result, move, 0, WIN);
      }
      else if (dpos != 0) {
	/* The dragon could be defended by another move. Try to attack
         * with this move.
	 */
	shape_moves[0].pos         = dpos;
	shape_moves[0].value       = 25;
	shape_moves[0].name        = "defense move";
	shape_moves[0].same_dragon = 2;
	shape_moves[1].value       = 0;
	moves = shape_moves;
      }
    }
      
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
      int origin = 0;

      if (moves[k].value < move_cutoff)
	break;

      mpos = moves[k].pos;

      ASSERT_ON_BOARD1(mpos);
            
      /* Consider only the highest scoring move if we're deeper than
       * owl_branch_depth.
       */
      if (stackp > owl_branch_depth && k > 0)
	break;
    
      gg_assert(mpos != 0);
    
      /* Have we already tested this move? */
      if (mw[mpos])
	continue;

      /* Try to make the move. */
      if (!komaster_trymove(mpos, other, moves[k].name, str,
			    komaster, kom_pos, &new_komaster, &new_kom_pos,
			    &ko_move, savecode == 0))
	continue;
      TRACE("Trying %C %1m\n", other, mpos);

      /* We have now made a move. Analyze the new position. */
      push_owl(owl);
      mw[mpos] = 1;
      number_tried_moves++;
      owl->lunches_are_current = 0;
      owl_update_boundary_marks(mpos, owl);
      
      /* If the origin of the dragon has been captured, we look
       * for another string which was part of the original dragon,
       * marked when stackp==0, which has not been captured. If no
       * such string is found, owl_attack declares victory.
       */
      if (board[str] != EMPTY)
	origin = str;
      else {
	int found_string = 0;
	int oi, oj;
	for (oi = 0; oi < board_size && !found_string; oi++)
	  for (oj = 0; oj < board_size && !found_string; oj++) {
	    if (BOARD(oi, oj) == color && owl->goal[POS(oi, oj)] == 1) {
	      origin = find_origin(POS(oi, oj));
	      found_string = 1;
	    }
	  }
	
	if (!found_string)
	  origin = 0;
      }

      if (origin == 0)
	dcode = 0;
      else
	dcode = do_owl_defend(origin, NULL, owl, new_komaster, new_kom_pos);

      if (!ko_move) {
	if (dcode == 0) {
	  pop_owl(owl);
	  popgo();
	  if (origin == 0) {
	    SGFTRACE(mpos, WIN, "all original stones captured");
	  }
	  else {
	    SGFTRACE(mpos, WIN, "attack effective");
	  }
	  READ_RETURN(read_result, move, mpos, WIN);
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
    
      pop_owl(owl);
      popgo();
    }
  }
  
  if (savecode) {
    SGFTRACE(savemove, savecode, "attack effective (ko) - E");
    READ_RETURN(read_result, move, savemove, savecode);
  }

  SGFTRACE(0, 0, NULL);
  READ_RETURN0(read_result);
}


/* Returns true if the dragon at (m, n) can be captured given
 * two moves in a row. The first two moves to capture the
 * dragon are given as (*ui, *uj) and (*vi, *vj).
 */

int
owl_threaten_attack(int m, int n, int *ui, int *uj, int *vi, int *vj)
{
  struct owl_move_data moves[MAX_MOVES];
  int k;
  int str = POS(m, n);
  int other = OTHER_COLOR(board[str]);
  static struct local_owl_data owl;
  int result = 0;
  int reading_nodes_when_called = get_reading_node_counter();
  char saved_boundary[BOARDMAX];
  clock_t start = 0, end;
  double elapsed;
  int tactical_nodes;
  int move = 0;
  int move2 = 0;

  result_certain = 1;
  if (search_persistent_owl_cache(OWL_THREATEN_ATTACK, str, 0, 0,
				  &result, &move, &move2, NULL)) {
    if (ui) *ui = I(move);
    if (uj) *uj = J(move);
    if (vi) *vi = I(move2);
    if (vj) *vj = J(move2);
    return result;
  }

  if (debug & DEBUG_OWL_PERFORMANCE)
    start = clock();
  owl.local_owl_node_counter = 0;
  gg_assert(stackp == 0);
  TRACE("owl_threaten_attack %m\n", m, n);
  owl.lunches_are_current = 0;
  owl_mark_dragon(m, n, -1, -1, &owl);
  memcpy(saved_boundary, owl.boundary, sizeof(saved_boundary));
  compute_owl_escape_values(&owl);
  owl_make_domains(&owl, NULL);
  if (owl_shapes(moves, other, &owl, &owl_attackpat_db)) {
    for (k = 0; k < MAX_MOVES; k++) {
      int mpos = moves[k].pos;

      if (mpos != 0 && moves[k].value > 0)
	if (trymove(mpos, other, moves[k].name, str, EMPTY, 0)) {
	  int oi, oj;
	  int origin = 0;
	  owl.lunches_are_current = 0;
	  owl_update_boundary_marks(mpos, &owl);
	  
	  /* If the origin of the dragon has been captured, we look
	   * for another string which was part of the original dragon,
	   * marked when stackp==0, which has not been captured. If no
	   * such string is found, owl_attack declares victory.
	   */
	  
	  if (board[str] == EMPTY) {
	    int found_string = 0;
	    for (oi = 0; oi < board_size && !found_string; oi++)
	      for (oj = 0; oj < board_size && !found_string; oj++) {
		if (BOARD(oi, oj) != EMPTY 
		    && owl.goal[POS(oi, oj)] == 1) {
		  origin = find_origin(POS(oi, oj));
		  found_string = 1;
		}
	      }
	    if (!found_string 
		|| do_owl_attack(origin, NULL, &owl, EMPTY, 0)) {
	      /* probably this can't happen */
	      popgo();
	      gg_assert(stackp == 0);
	      result = 1;
	      break;
	    }
	  }
	  else if (do_owl_attack(str, &move2, &owl, EMPTY, 0) == WIN) {
	    move = moves[k].pos;
	    popgo();
	    gg_assert(stackp == 0);
	    result = 1;
	    break;
	  }
	  popgo();
	  memcpy(owl.boundary, saved_boundary, sizeof(saved_boundary));
	}
    }
  }
  tactical_nodes = get_reading_node_counter() - reading_nodes_when_called;
  gg_assert(stackp == 0);

  if (debug & DEBUG_OWL_PERFORMANCE) {
    end = clock();
    elapsed = ((double) (end - start)) / CLOCKS_PER_SEC;
    gprintf("owl_threaten_attack %m %1m %1m, result %d (%d, %d nodes, %f seconds)\n",
	    m, n, move, move2, result,
	    owl.local_owl_node_counter,
	    tactical_nodes, elapsed);
  }

  store_persistent_owl_cache(OWL_THREATEN_ATTACK, str, 0, 0,
			     result, move, move2, 0,
			     tactical_nodes, owl.goal, board[str]);
  if (ui) *ui = I(move);
  if (uj) *uj = J(move);
  if (vi) *vi = I(move2);
  if (vj) *vj = J(move2);
  return result;
}


/* Returns true if a move can be found to defend the dragon
 * at (m,n), in which case (*ui,*uj) is the recommended move.
 * (*ui, *uj) can be null pointers if the result is not needed.
 *
 * The array goal marks the extent of the dragon. This must
 * be maintained during reading. Call this function only when
 * stackp==0; otherwise you can call do_owl_attack but you must
 * set up the goal and boundary arrays by hand first.
 *
 * Returns 2 or 3 if the position is ko:
 *
 * - Returns 2 if the attack prevails provided attacker is willing to
 *   ignore any ko threat (the attacker makes the first ko capture).
 * - Returns 3 if attack succeeds provided attacker has a ko threat
 *   which must be answered (the defender makes the first ko capture).
 *
 * The array goal marks the extent of the dragon. This must
 * be maintained during reading.  */

int
owl_defend(int m, int n, int *ui, int *uj, int *certain)
{
  int result;
  static struct local_owl_data owl;
  int reading_nodes_when_called = get_reading_node_counter();
  clock_t start = 0, end;
  double elapsed;
  int tactical_nodes;
  int move = 0;
  int str = POS(m, n);

  result_certain = 1;
  if (worm[POS(m, n)].unconditional_status == DEAD)
    return 0;

  if (search_persistent_owl_cache(OWL_DEFEND, str, 0, 0,
				  &result, &move, NULL, certain)) {
    if (ui) *ui = I(move);
    if (uj) *uj = J(move);
    return result;
  }

  if (debug & DEBUG_OWL_PERFORMANCE)
    start = clock();
  owl.local_owl_node_counter = 0;
  TRACE("owl_defend %m\n", m, n);
  owl.lunches_are_current = 0;
  owl_mark_dragon(m, n, -1, -1, &owl);
  compute_owl_escape_values(&owl);
  owl_make_domains(&owl, NULL);
  result = do_owl_defend(str, &move, &owl, EMPTY, 0);
  tactical_nodes = get_reading_node_counter() - reading_nodes_when_called;
  if (debug & DEBUG_OWL_PERFORMANCE) {
    end = clock();
    elapsed = ((double) (end - start)) / CLOCKS_PER_SEC;
    gprintf("owl_defend %m, result %d %1m (%d, %d nodes, %f seconds)\n",
	    m, n, result, move, owl.local_owl_node_counter,
	    tactical_nodes, elapsed);
  }

  store_persistent_owl_cache(OWL_DEFEND, str, 0, 0, result, move, 0,
			     result_certain, tactical_nodes, owl.goal,
			     board[str]);
  if (ui) *ui = I(move);
  if (uj) *uj = J(move);
  if (certain) *certain = result_certain;
  return result;
}


/* Static function containing the main recursive code for owl_defend.
 */

static int
do_owl_defend(int str, int *move, struct local_owl_data *owl,
	      int komaster, int kom_pos)
{
  int color = board[str];
  int other = OTHER_COLOR(color);
  struct owl_move_data shape_moves[MAX_MOVES];
  struct owl_move_data vital_moves[MAX_MOVES];
  struct owl_move_data *moves;
  char mw[BOARDMAX];
  int number_tried_moves = 0;
  int pass;
  int k;
  int savemove = 0;
  int savecode = 0;
  int true_genus = -1;
  int probable_min = -1;
  int probable_max = -1;
  int move_cutoff;
  int acode;
  int found_read_result;
  Read_result *read_result;
  int this_variation_number = count_variations - 1;
  int m = I(str);
  int n = J(str);
  
  SETUP_TRACE_INFO("owl_defend", str);

  if ((stackp <= owl_branch_depth) && (hashflags & HASH_OWL_DEFEND)) {
    found_read_result = get_read_result(OWL_DEFEND, komaster, kom_pos,
					&str, &read_result);
    if (found_read_result) {
      TRACE_CACHED_RESULT(*read_result);
      if (rr_get_result(*read_result) != 0) {
	if (move)
	  *move = rr_get_move(*read_result);
      }

      if (rr_get_result(*read_result) == WIN)
	TRACE("%oVariation %d: ALIVE (cached)\n", this_variation_number);
      else
	TRACE("%oVariation %d: DEAD (cached)\n", this_variation_number);

      SGFTRACE(rr_get_move(*read_result), rr_get_result(*read_result),
	       "cached");
      return rr_get_result(*read_result);
    }
    /* This data should always be recorded. */
    if (read_result) {
      rr_set_compressed_data(*read_result, OWL_DEFEND, 
			     komaster, kom_pos, str, stackp);
    }
  }
  else
    read_result = NULL;

  /* In order to get a defense move even if we seem to already have
   * escaped and to reduce the impact of overestimated escape
   * possibilities, we don't declare escape victory on the first move.
   *
   * FIXME: Should introduce a new owl depth value rather than having
   *        this hardwired value.
   */
  if (stackp > 2 && owl_escape_route(owl) >= 5) {
    /* FIXME: We probably should make distinction in the returned
     * result whether the dragon lives by making two eyes or by
     * escaping.
     */
    TRACE("%oVariation %d: ALIVE (escaped)\n", this_variation_number);
    SGFTRACE(0, WIN, "escaped");
    READ_RETURN(read_result, move, 0, WIN);
  }

  /* If (stackp > owl_reading_depth), interpret deep reading 
   * conservatively as escape.
   */
  if (stackp > owl_reading_depth) {
    TRACE("%oVariation %d: ALIVE (maximum reading depth reached)\n",
	  this_variation_number);
    SGFTRACE(0, WIN, "max reading depth reached");
    READ_RETURN(read_result, move, 0, WIN);
  }
  
  /* If the owl node limit has been reached, assume the dragon has
   * managed to escape.
   */
  if (owl->local_owl_node_counter >= owl_node_limit) {
    result_certain = 0;
    TRACE("%oVariation %d: ALIVE (owl node limit reached)\n",
	  this_variation_number);
    SGFTRACE(0, WIN, "owl node limit reached");
    READ_RETURN(read_result, move, 0, WIN);
  }

  memset(mw, 0, sizeof(mw));
  owl->local_owl_node_counter++;
  global_owl_node_counter++;

  /* Always start with picking up the vital moves so we can see if
   * we already are safe.
   */
  {
    SGFTree *save_sgf_dumptree = sgf_dumptree;
    int save_count_variations = count_variations;
    
    sgf_dumptree = NULL;
    count_variations = 0;
    if (color == BLACK)
      true_genus = owl_determine_life(owl, owl->black_eye,
				      BLACK, komaster, 0, vital_moves,
				      &probable_min, &probable_max);
    else 
      true_genus = owl_determine_life(owl, owl->white_eye,
				      WHITE, komaster, 0, vital_moves,
				      &probable_min, &probable_max);
    
    current_owl_data = owl;
    memset(owl_safe_move_cache, 0, sizeof(owl_safe_move_cache));

    /* We don't care about the moves, just whether matches are found.
     * The content of shape_moves[] will be discarded when we call
     * owl_shapes().
     */
    for (k = 0; k < MAX_MOVES; k++) {
      shape_moves[k].pos = 0;
      shape_moves[k].value = -1;
      shape_moves[k].name = NULL;
      shape_moves[k].same_dragon = 1;
    }

    matches_found = 0;
    memset(found_matches, 0, sizeof(found_matches));
    if (level >= 9) 
      global_matchpat(owl_shapes_callback, other, 
		      &owl_vital_apat_db, shape_moves, owl->goal);

    true_genus -= matches_found;

    sgf_dumptree = save_sgf_dumptree;
    count_variations = save_count_variations;
    
    if (true_genus >= 2 ||
	(true_genus == 1 && probable_min >= 4)
	|| (stackp > owl_distrust_depth
	    && probable_min >= 2
	    && !matches_found)) {
      TRACE("%oVariation %d: ALIVE (2 or more secure eyes)\n",
	    this_variation_number, true_genus);
	SGFTRACE(0, WIN, "2 or more secure eyes");
	READ_RETURN(read_result, move, 0, WIN);
    }
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
    
    /* Get the shape moves if we are in the right pass. */
    if (pass == 1) {
      
      if (stackp > owl_branch_depth && number_tried_moves > 0)
	continue;
      
      owl_shapes(shape_moves, color, owl, &owl_defendpat_db);
      /* A move of value 100 is considered a win */
      if (shape_moves[0].value >= 100) {
	/* to make sure this move is recorded in the sgf file */
	if (trymove(shape_moves[0].pos, color, shape_moves[0].name, str,
		    komaster, kom_pos))
	  popgo();
	TRACE("%oVariation %d: ALIVE (Winning owl_defendpat)\n", 
	      this_variation_number);
	SGFTRACE(shape_moves[0].pos, WIN, "winning defense pattern");
	READ_RETURN(read_result, move, shape_moves[0].pos, WIN);
      }
      moves = shape_moves;
    }
    else if (pass == 0 || pass == 2) {

      if (stackp > owl_branch_depth && number_tried_moves > 0)
	continue;
      
      moves = vital_moves;
      if (pass == 0 || stackp > owl_distrust_depth) {
	if (stackp == 0)
	  move_cutoff = 70;
	else if (true_genus + probable_min >= 3)
	  move_cutoff = 25;
	else
	  move_cutoff = 45;
      }
      if (probable_max < 2 && stackp > 2)
	move_cutoff = 99; /* Effectively disable vital moves. */
    }
    else {
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
      if (attack_and_defend(POS(m, n), NULL, NULL, NULL, &dpos)
	  && (approxlib(dpos, color, 2, NULL) > 1
	      || does_capture_something(dpos, color))) {
	shape_moves[0].pos         = dpos;
	shape_moves[0].value       = 25;
	shape_moves[0].name        = "tactical defense";
	shape_moves[0].same_dragon = 2;
	shape_moves[1].value       = 0;
	moves = shape_moves;
      }
      sgf_dumptree = save_sgf_dumptree;
      count_variations = save_count_variations;
    }

    if (!moves)
      continue;
    
    /* For the up to MAX_MOVES best moves with value equal to
     * move_cutoff or higher, try to defend the dragon and see if it
     * can then be attacked.
     */
    for (k = 0; k < MAX_MOVES; k++) {
      int mpos;
      int new_komaster, new_kom_pos;
      int ko_move = -1;
      
      if (moves[k].value < move_cutoff)
	break;

      mpos = moves[k].pos;

      ASSERT_ON_BOARD1(mpos);
      /* Consider only the highest scoring move if we're deeper than
       * owl_branch_depth.
       */
      if (stackp > owl_branch_depth && k > 0)
	break;
      
      gg_assert(mpos != 0);

      /* Have we already tested this move? */
      if (mw[mpos])
	continue;

      /* Try to make the move. */
      if (!komaster_trymove(mpos, color, moves[k].name, str,
			    komaster, kom_pos, &new_komaster, &new_kom_pos,
			    &ko_move, savecode == 0))
	continue;

      TRACE("Trying %C %1m\n", color, mpos);

      /* We have now made a move. Analyze the new position. */
      push_owl(owl);
      mw[mpos] = 1;
      number_tried_moves++;
      owl->lunches_are_current = 0;

      /* Add the stone just played to the goal dragon, unless the
       * pattern explicitly asked for not doing this.
       */
      owl_update_goal(mpos, moves[k].same_dragon, owl);

      if (!ko_move) {
	acode = do_owl_attack(str, NULL, owl, new_komaster, new_kom_pos);
	if (!acode) {
	  pop_owl(owl);
	  popgo();
	  SGFTRACE(mpos, WIN, "defense effective - A");
	  READ_RETURN(read_result, move, mpos, WIN);
	}
	UPDATE_SAVED_KO_RESULT(savecode, savemove, acode, mpos);
      }
      else {
	if (do_owl_attack(str, NULL, owl, new_komaster, new_kom_pos) != WIN) {
	  savemove = mpos;
	  savecode = KO_B;
	}
      }
      
      /* Undo the tested move. */
      pop_owl(owl);
      popgo();
    }
  }
  
  if (savecode) {
    SGFTRACE(savemove, savecode, "defense effective (ko) - B");
    READ_RETURN(read_result, move, savemove, savecode);
  }

  if (number_tried_moves == 0 && probable_min >= 2) {
    SGFTRACE(0, WIN, "genus probably >= 2");
    READ_RETURN(read_result, move, 0, WIN);
  }
  
  if (true_genus == 1) {
    SGFTRACE(0, 0, "genus 1");
  }
  else {
    SGFTRACE(0, 0, "genus 0");
  }
  
  READ_RETURN0(read_result);
}


/* Returns true if the dragon at (m, n) can be defended given
 * two moves in a row. The first two moves to defend the
 * dragon are given as (*ui, *uj) and (*vi, *vj).
 */

int
owl_threaten_defense(int m, int n, int *ui, int *uj, int *vi, int *vj)
{
  struct owl_move_data moves[MAX_MOVES];
  int k;
  int str = POS(m, n);
  int color = board[str];
  int result = 0;
  static struct local_owl_data owl;
  int reading_nodes_when_called = get_reading_node_counter();
  char saved_goal[BOARDMAX];
  clock_t start = 0, end;
  double elapsed;
  int tactical_nodes;
  int move = 0;
  int move2 = 0;

  result_certain = 1;
  if (worm[POS(m, n)].unconditional_status == DEAD)
    return 0;

  if (search_persistent_owl_cache(OWL_THREATEN_DEFENSE, str, 0, 0,
				  &result, &move, &move2, NULL)) {
    if (ui) *ui = I(move);
    if (uj) *uj = J(move);
    if (vi) *vi = I(move2);
    if (vj) *vj = J(move2);
    return result;
  }

  if (debug & DEBUG_OWL_PERFORMANCE)
    start = clock();
  owl.local_owl_node_counter = 0;
  TRACE("owl_threaten_defense %m\n", m, n);
  owl.lunches_are_current = 0;
  owl_mark_dragon(m, n, -1, -1, &owl);
  memcpy(saved_goal, owl.goal, sizeof(saved_goal));
  compute_owl_escape_values(&owl);
  owl_make_domains(&owl, NULL);
  if (owl_shapes(moves, color, &owl, &owl_defendpat_db)) {
    for (k = 0; k < MAX_MOVES; k++) {
      if (moves[k].pos != 0 && moves[k].value > 0)
	if (trymove(moves[k].pos, color, moves[k].name, str, EMPTY, 0)) {
	  owl.lunches_are_current = 0;
	  owl_update_goal(moves[k].pos, moves[k].same_dragon, &owl);
	  if (do_owl_defend(str, &move2, &owl, EMPTY, 0) == WIN) {
	    move = moves[k].pos;
	    popgo();
	    /* Don't return the second move if occupied before trymove */
	    if (move2 != 0 && board[move2] != EMPTY)
	      move2 = 0;
	    result = WIN;
	    break;
	  }
	  else
	    popgo();
	  memcpy(owl.goal, saved_goal, sizeof(saved_goal));
	}
    }
  }
  tactical_nodes = get_reading_node_counter() - reading_nodes_when_called;
  gg_assert(stackp == 0);

  if (debug & DEBUG_OWL_PERFORMANCE) {
    end = clock();
    elapsed = ((double) (end - start)) / CLOCKS_PER_SEC;
    gprintf("owl_threaten_defense %m %1m %1m, result %d (%d, %d nodes, %f seconds)\n",
	    m, n, move, move2, result, owl.local_owl_node_counter,
	    tactical_nodes, elapsed);
  }

  store_persistent_owl_cache(OWL_THREATEN_DEFENSE, str, 0, 0,
			     result, move, move2, 0,
			     tactical_nodes, owl.goal, board[str]);
  if (ui) *ui = I(move);
  if (uj) *uj = J(move);
  if (vi) *vi = I(move2);
  if (vj) *vj = J(move2);
  return result;
}


/* 
 * This function is invoked when a terminal node is reached. It runs
 * make_domains() and returns twice the number of eyes, counted very
 * pessimistically.
 * 
 * The support of goal is a dragon for which no defensive move is
 * found. If the string is BLACK then eye is owl_black_eye, and if
 * the string is WHITE then eye is owl_white_eye.
 */

static int
owl_determine_life(struct local_owl_data *owl,
		   struct eye_data eye[BOARDMAX],
		   int color, int komaster, int does_attack,
		   struct owl_move_data *moves, int *probable_min,
		   int *probable_max)
{
  char mw[BOARDMAX];  /* mark relevant eye origins */
  signed char mx[BOARDMAX]; /* mark potential half or false eyes */
  int vital_values[BOARDMAX];
  int true_genus = 0;
  int max, min, pessimistic_min;
  int attack_point;
  int defense_point;
  int m, n;
  int k;
  int lunch;
  int eye_color;
  int topological_intersections;
  memset(mw, 0, sizeof(mw));
  memset(mx, 0, sizeof(mx));
  memset(vital_values, 0, sizeof(vital_values));
  UNUSED(komaster);

  for (k = 0; k < MAX_MOVES; k++) {
    moves[k].pos = 0;
    moves[k].value = -1;
    moves[k].name = NULL;
    moves[k].same_dragon = 1;
  }
  
  if (!owl->lunches_are_current)
    owl_find_lunches(owl);
  
  if (0) {
    int k;
    for (k = 0; k < MAX_LUNCHES; k++)
      if (owl->lunch[k] != NO_MOVE)
	gprintf("owl lunch %1m, attack %1m, defend %1m\n",
		owl->lunch[k],
		owl->lunch_attack_point[k],
		owl->lunch_defense_point[k]);
  }

  owl_make_domains(owl, NULL);

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

  if (color == WHITE)
    eye_color = WHITE_BORDER;
  else
    eye_color = BLACK_BORDER;

  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++)
      if (BOARD(m, n) && owl->goal[POS(m, n)]) {
	for (k = 0; k < 8; k++) {
	  int dm = deltai[k];
	  int dn = deltaj[k];
	  if (ON_BOARD2(m+dm, n+dn)
	      && eye[POS(m+dm, n+dn)].color == eye_color
	      && eye[POS(m+dm, n+dn)].origin != NO_MOVE
	      && !eye[POS(m+dm, n+dn)].marginal)
	    mw[eye[POS(m+dm, n+dn)].origin]++;
	}
      }

  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++)
      owl->half_eye[POS(m, n)].type = 0;

  /* Find topological half eyes and false eyes by analyzing the
   * diagonal intersections, as described in the Texinfo
   * documentation (Eyes/Eye Topology).
   */

  /* First mark the potential halfeyes or false eyes. */
  topological_intersections = 0;
  for (m = 0; m<board_size; m++)
    for (n = 0; n<board_size; n++) {
      if (eye[POS(m, n)].color == eye_color
	  && eye[POS(m, n)].origin != NO_MOVE
	  && mw[eye[POS(m, n)].origin] > 1
	  && (!eye[POS(m, n)].marginal || life)
	  && eye[POS(m, n)].neighbors <= 1) {
	mx[POS(m, n)] = 1;
	topological_intersections++;
      }
    }

  /* Then examine them. */
  while (topological_intersections > 0) {
    for (m = 0; m<board_size; m++)
      for (n = 0; n<board_size; n++) {
	int sum;
	int ai = -1;
	int aj = -1;
	int di = -1;
	int dj = -1;

	if (mx[POS(m, n)] <= 0)
	  continue;

	mx[POS(m, n)] = -1;
	topological_intersections--;
	
	sum = topological_eye(m, n, color, &ai, &aj, &di, &dj, 
			      owl->black_eye, owl->white_eye,
			      owl->half_eye);

	if (sum >= 4) {
	  int previously_marginal = eye[POS(m, n)].marginal;
	  owl->half_eye[POS(m, n)].type = FALSE_EYE;
	  if (eye[POS(m, n)].esize == 1
	      || is_legal2(m, n, OTHER_COLOR(color))
	      || BOARD(m, n) == OTHER_COLOR(color)) {
	    add_half_eye(m, n, eye, owl->half_eye);
	    
	    /* Marginal status may have changed. This can change the
             * topological eye evaluation for diagonal neighbors, so
             * we mark these for another pass if they have already
             * been examined.
	     */
	    if (!previously_marginal) {
	      int k;
	      for (k=4; k<8; k++) {
		int i = m + deltai[k];
		int j = n + deltaj[k];
		if (ON_BOARD2(i, j) && mx[POS(i, j)] == -1) {
		  mx[POS(i, j)] = 1;
		  topological_intersections++;
		}
	      }
	    }
	  }
	}
	else if (sum == 3) {
	  owl->half_eye[POS(m, n)].type = HALF_EYE;
	  ASSERT1(owl->half_eye[POS(m,n)].num_attacks > 0, POS(m, n));
	  ASSERT_ON_BOARD1(owl->half_eye[POS(m, n)].attack_point[0]);
	  ASSERT1(owl->half_eye[POS(m, n)].num_defends > 0, POS(m, n));
	  ASSERT_ON_BOARD1(owl->half_eye[POS(m, n)].defense_point[0]);
	}
      }
  }

  *probable_min = 0;
  *probable_max = 0;
  /* This test must be conditioned on (m,n) being its own origin,
   * because some origins get moved during the topological eye
   * code.
   *
   * FIXME: I don't think eye origins are moved around any more. 
   */

  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      int pos = POS(m, n);
      if (mw[pos] > 1
	  && (eye[pos].origin == pos))
      {
	int value = 0;
	const char *reason = "";
	int i, j;
	compute_eyes_pessimistic(pos, &max, &min, &pessimistic_min,
				 &attack_point, &defense_point,
				 eye, owl->half_eye);
	/* If this eyespace includes an owl inessential string, we
         * must assume that the pessimistic min is 0.
	 */
	for (i = 0; i < board_size; i++)
	  for (j = 0; j < board_size; j++)
	    if (mw[POS(i, j)] > 1
		&& eye[POS(i, j)].origin == pos
		&& owl->inessential[POS(i, j)])
	      pessimistic_min = 0;
	
	true_genus += pessimistic_min;
	*probable_min += min;
	*probable_max += max;

	/* Fill in the maxeye field for use by the owl_eyespace() function. */
	eye[pos].maxeye = max;
	
	/* This shortcut has been disabled for two reasons:
	 * 1. Due to the vital attack moves being able to later reduce
	 * the true genus, we can't say that a certain true_genus is
	 * sufficient.
	 * 2. This part of the code is in now way time critical. If
	 * the life code is enabled, this may change.
	 */
#if 0
	/* Found two certain eyes---look no further. */
	if (true_genus >= 2)
	  return 2;
#endif
	
	if (max != min) {
	  value = 50;
	  if (max - min == 2)
	    value = 70;
	  else if (max - pessimistic_min == 2)
	    value = 60;
	  reason = "vital move";
	}
	else if (max != pessimistic_min) {
	  if (max - pessimistic_min == 2)
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
	    
	    TRACE("%s at %1m, score %d (eye at %1m, max %d, min %d, pessimistic_min %d)\n",
		  reason, attack_point, value, pos, max, min,
		  pessimistic_min);
	    owl_add_move(moves, attack_point, value, reason, 1);
	    vital_values[attack_point] = value;
	  }
	  else if (!does_attack && defense_point != NO_MOVE
		   && board[defense_point] == EMPTY
		   && !is_self_atari(defense_point, color)) {
	    if (vital_values[defense_point] > 0) {
	      value += vital_values[defense_point];
	      if (value > 98)
		value = 98; /* Higher values may get special interpretation. */
	    }
	    
	    TRACE("%s at %1m, score %d (eye at %1m, max %d, min %d, pessimistic_min %d)\n",
		  reason, defense_point, value, pos, max, min,
		  pessimistic_min);
	    owl_add_move(moves, defense_point, value, reason, 1);
	    vital_values[defense_point] = value;
	  }
	}
      }
    }
  /* sniff each lunch for nutritional value. The
     assumption is that capturing the lunch is gote,
     therefore the number of half eyes equals the
     MINIMUM number of eyes yielded by the resulting
     eye space.
   */
  {
    for (lunch = 0; (lunch < MAX_LUNCHES); lunch++)
      if ((owl->lunch[lunch] != NO_MOVE) &&
	  (owl->lunch_defense_point[lunch] != NO_MOVE)) {
	int value = 0;
	int lunch_min;
	int lunch_probable;
	int lunch_max;
	sniff_lunch(owl->lunch[lunch],
		    &lunch_min, &lunch_probable, &lunch_max, owl);

	*probable_max += lunch_max;
	
	if (lunch_probable == 0)
	  value = 20;
	else if (lunch_probable == 1 && lunch_max == 1) {
	  value = 60 + countstones(owl->lunch[lunch]);
	}
	else if (lunch_probable == 1 && lunch_max == 2)
	  value = 70 + countstones(owl->lunch[lunch]);
	else
	  value = 75 + countstones(owl->lunch[lunch]);
	
	if (owl->lunch_attack_code[lunch] != WIN)
	  value -= 10;

	if (does_attack) {
	  TRACE("save lunch at %1m with %1m, score %d\n",
		owl->lunch[lunch],
		owl->lunch_defense_point[lunch], value);
	  owl_add_move(moves, owl->lunch_defense_point[lunch],
		       value, "save lunch", 1);
	}
	else {
	  TRACE("eat lunch at %1m with %1m, score %d\n",
		owl->lunch[lunch],
		owl->lunch_attack_point[lunch], value);
	  owl_add_move(moves, owl->lunch_attack_point[lunch],
		       value, "eat lunch", 1);
	}
      }
  }
  return true_genus;
}

	
/* 
 * Generates up to max_moves moves, attempting to attack or defend the
 * dragon at (goali, goalj). The found moves are put in moves, an
 * array of owl_move_data structs, starting in the position 'initial'.
 * The entries in the array are sorted by value with moves[initial] 
 * having highest priority. When no more moves are
 * available this is indicated by value and coordinates in the
 * array being -1.
 *
 * Returns 1 if at least one move is found, or 0 if no move is found.  */

static int 
owl_shapes(struct owl_move_data moves[MAX_MOVES], int color,
	   struct local_owl_data *owl, struct pattern_db *type)
{
  int k;
  SGFTree *save_sgf_dumptree = sgf_dumptree;
  int save_count_variations = count_variations;
  sgf_dumptree = NULL;
  count_variations = 0;

  current_owl_data = owl;
  
  for (k = 0; k < MAX_MOVES; k++) {
    moves[k].pos = 0;
    moves[k].value = -1;
    moves[k].name = NULL;
    moves[k].same_dragon = 1;
  }

  /* We must reset the owl_safe_move_cache before starting the
   * pattern matching. The cache is used by owl_shapes_callback().
   */
  memset(owl_safe_move_cache, 0, sizeof(owl_safe_move_cache));
  global_matchpat(owl_shapes_callback, color, type, moves, owl->goal);

  sgf_dumptree = save_sgf_dumptree;
  count_variations = save_count_variations;

  if (moves[0].value > 0)
    return 1;
  else
    return 0;
}  



/* This function takes an array of already found moves (passed as
 * 'data') and looks for moves to replace these. Only moves near
 * (goali, goalj) are considered.
 */

static void
owl_shapes_callback(int m, int n, int color, struct pattern *pattern,
		    int ll, void *data)
{
  int ti, tj, tval;  /* trial move and its value */
  int move;
  struct owl_move_data *moves = data; /* considered moves passed as data */

  /* Pick up the location of the move */
  TRANSFORM(pattern->movei, pattern->movej, &ti, &tj, ll);
  ti += m;
  tj += n;
  move = POS(ti, tj);

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
  
  /* For sacrifice patterns, the survival of the stone to be played is
   * not checked. Otherwise we discard moves which can be captured. 
   * Illegal ko captures are accepted for ko analysis.
   */
  if (! (pattern->class & CLASS_s)) {
    if (!owl_safe_move(move, color)) {
      if (0)
	TRACE("  move at %1m wasn't safe, discarded\n", move);
      return;
    }
    if (!is_legal(move, color)) {
      if (0)
	TRACE("  move at %1m wasn't legal, discarded\n", move);
      return;
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
      return;
    }
  }

  /* If the pattern has a constraint, call the autohelper to see
   * if the pattern must be rejected.
   */
  if (pattern->autohelper_flag & HAVE_CONSTRAINT) {
    if (!pattern->autohelper(pattern, ll, ti, tj, color, 0))
      return;
  }

  /* and work out the value of this move */
  if (pattern->helper) {
    /* ask helper function to consider the move */
    DEBUG(DEBUG_HELPER, "  asking helper to consider '%s'+%d at %1m\n",
	  pattern->name, ll, move);
    tval = pattern->helper(pattern, ll, ti, tj, color);
    
    if (tval > 0) {
      DEBUG(DEBUG_HELPER, "helper likes pattern '%s' value %d at %1m\n",
	    pattern->name, tval, move);
    }
    else {
      DEBUG(DEBUG_HELPER,"  helper does not like pattern '%s' at %1m\n",
	    pattern->name, move);
      return;  /* pattern matcher does not like it */
    }
  }
  else { /* no helper */
    tval = (int) pattern->value;
  }

  /* having made it here, we have made it through all the extra checks */

  TRACE("Pattern %s found at %1m with value %d\n", pattern->name, move, tval);

  owl_add_move(moves, move, tval, pattern->name,
	       (pattern->class & CLASS_B) == 0);
}


/* Add a move to the list of candidate moves */

static void
owl_add_move(struct owl_move_data *moves, int move, int value,
	     const char *reason, int same_dragon)
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
  for (k = 0; k < MAX_MOVES; k++) {
    if (moves[k].value == -1)
      break;
    if (moves[k].pos == move) {
      if (same_dragon)
	moves[k].same_dragon = 1;
      break;
    }
  }

  /* Did we already have this move in the list with a higher value? */
  if (k < MAX_MOVES && moves[k].value >= value)
    return;

  /* Insert the move at the right place in the list and adjust other
   * entries as needed.
   */
  for (; k >= 0; k--) {
    if (k == 0 || value <= moves[k-1].value) {
      /* Can't get higher. Insert the move below this point and quit
       * looping.
       */
      if (k < MAX_MOVES) {
	moves[k].pos = move;
	moves[k].value = value;
	moves[k].name = reason;
	/* If B class pattern, this move shouldn't be added to the
         * dragon under consideration.
	 */
	moves[k].same_dragon = same_dragon;
      }
      break;
    }
    /* Shuffle the passed move one step downwards. */
    if (k < MAX_MOVES)
      moves[k] = moves[k-1]; /* struct copy */
  }

  /* Assert that the list contains unique moves. */
  if (1) {
    int l;
    for (k = 0; k < MAX_MOVES; k++)
      for (l = k+1; l < MAX_MOVES; l++)
	gg_assert(moves[k].pos == 0
		  || moves[k].pos != moves[l].pos);
  }
}  

/* Marks the dragons at (ai, aj) and (bi, bj). If only one dragon
 * needs marking, (bi, bj) should be passed as (0). 
 */

static void
owl_mark_dragon(int ai, int aj, int bi, int bj, struct local_owl_data *owl)
{
  int i, j;
  int color = BOARD(ai, aj);
  
  gg_assert(bi == -1 || BOARD(bi, bj) == color);
  
  for (i = 0; i < board_size; i++) {
    for (j = 0; j < board_size; j++) {
      if (same_dragon(POS(i, j), POS(ai, aj)) || same_dragon(POS(i, j), POS(bi, bj)))
	owl->goal[POS(i, j)] = 1;
      else
	owl->goal[POS(i, j)] = 0;
    }
  }
  (owl->color) = color;
  owl_mark_boundary(owl);
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
	    && dragon[pos2].status != DEAD) {
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
      int k;
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

/* Add the stone just played to the goal dragon, unless same_dragon is
 * 0. We also add all stones belonging to the same generalized string
 * to the goal. If same_dragon is 2, we only add the stones if at
 * least one stone of the generalized string already was part of the
 * goal.
 */
static void
owl_update_goal(int pos, int same_dragon, struct local_owl_data *owl)
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
  
  find_superstring(pos, &num_stones, stones);

  /* Turn sgf output back on. */
  sgf_dumptree = save_sgf_dumptree;
  count_variations = save_count_variations;
  
  /* If same_dragon field is 2, only add if the played stone
   * clearly is in contact with the goal dragon.
   */
  if (same_dragon == 2) {
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
goaldump(char goal[BOARDMAX])
{
  int pos;
  for (pos = BOARDMIN; pos < BOARDMAX; pos++)
    if (goal[pos])
      gprintf("%o%1m (%d)  ", pos, (int) goal[pos]);
  gprintf("\n");
}


/* Add owl reasons. This function should be called once during
 * genmove.
 */

void
owl_reasons(int color)
{
  int m, n;
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      int pos = POS(m, n);
      if (dragon[pos].origin == pos
	  && dragon[pos].matcher_status == CRITICAL
	  && dragon[pos].owl_attack_point != NO_MOVE) {
	if (board[pos] == color) {
	  if (dragon[pos].owl_defense_point != NO_MOVE) {
	    add_owl_defense_move(dragon[pos].owl_defense_point, pos);
	    DEBUG(DEBUG_OWL, "owl: %1m defends %m at move %d\n",
		  dragon[pos].owl_defense_point, m, n, movenum+1);
	  }
	}
	else { /* opponent's dragon */
	  /* We don't want to add this move reason if the attacker
	   * dies because the victim only formed a nakade shape.
	   *
	   * FIXME: This code overlaps heavily with some code in
	   * examine_move_safety() in move_reasons.c. The caching
	   * scheme should minimize the performance hit, but of course
	   * it's unfortunate to have the code duplication.
	   */
	  int ti = I(dragon[pos].owl_attack_point);
	  int tj = J(dragon[pos].owl_attack_point);

	  /* No worries if we catch something big. */
	  if (dragon[pos].effective_size < 8) {
	    /* Look through the neighbors of the victim for dragons of
             * our color. If we find at least one being thought alive
             * everything is ok. Otherwise we keep track of the
             * largest one for further examination.
	     */
	    int largest = 0;
	    int k;
	    int bi = -1, bj = -1;
	    int safe = 0;
	    for (k = 0; k < DRAGON2(pos).neighbors; k++) {
	      int d = DRAGON2(pos).adjacent[k];
	      if (DRAGON(d).color == color) {
		if (DRAGON(d).matcher_status == ALIVE) {
		  safe = 1;
		  break;
		}
		if (DRAGON(d).size > largest) {
		  bi = I(dragon2[d].origin);
		  bj = J(dragon2[d].origin);
		  largest = DRAGON(d).size;
		}
	      }
	    }

	    /* It may occasionally happen that no neighbor of our
	     * color was found. Assume safe in that case.
	     */
	    if (bi == -1)
	      safe = 1;

	    /* If not yet thought safe, ask the owl code whether the
	     * owl attack defends the (largest) attacker.
	     */
	    if (!safe && !owl_does_defend(ti, tj, bi, bj)) {
	      DEBUG(DEBUG_OWL,
		    "owl: %m attacks %1m at move %d, but the attacker dies.\n",
		    ti, tj, pos, movenum+1);
	      DRAGON2(pos).safety = INESSENTIAL;
	      continue;
	    }
	  }
	  
	  /* If we've reached this far, the attack is okay. */
	  add_owl_attack_move(POS(ti, tj), pos);
	  DEBUG(DEBUG_OWL, "owl: %m attacks %1m at move %d\n", ti, tj, pos,
		movenum+1);
	}
      }
      else if (dragon[pos].origin == pos
	       && dragon[pos].owl_status == DEAD
	       && dragon[pos].owl_threat_status == CAN_THREATEN_DEFENSE) {
	if (board[pos] == color 
	    && dragon[pos].owl_defense_point != NO_MOVE)
	  add_owl_defense_threat_move(dragon[pos].owl_defense_point, pos) ;
	if (board[pos] == color
	    && dragon[pos].owl_second_defense_point != NO_MOVE
	    && is_legal(dragon[pos].owl_second_defense_point, color))
	  add_owl_defense_threat_move(dragon[pos].owl_second_defense_point,
				      pos);
	/* If the opponent can threaten to live, an attacking
	 * move gets a small value to make sure it's really dead.
	 */
	if (board[pos] == OTHER_COLOR(color)
	    && dragon[pos].owl_threat_status == CAN_THREATEN_DEFENSE
	    && dragon[pos].owl_attack_point != NO_MOVE)
	  add_owl_prevent_threat_move(dragon[pos].owl_attack_point, pos);
      }
      else if (dragon[pos].origin == pos
	       && dragon[pos].owl_status == ALIVE
	       && board[pos] == OTHER_COLOR(color)
	       && dragon[pos].owl_threat_status == CAN_THREATEN_ATTACK) {
	if (dragon[pos].owl_attack_point != NO_MOVE)
	  add_owl_attack_threat_move(dragon[pos].owl_attack_point, pos);
	if (dragon[pos].owl_second_attack_point != NO_MOVE
	    && is_legal(dragon[pos].owl_second_attack_point, color))
	  add_owl_attack_threat_move(dragon[pos].owl_second_attack_point, pos);
      }
      /* The owl code found the friendly dragon alive, but was uncertain,
       * and an extra point of defense was found, so this might
       * be a good place to play.
       */
      else if (dragon[pos].origin == pos
	       && dragon[pos].owl_status == ALIVE
	       && board[pos] == color
	       && !dragon[pos].owl_attack_certain
	       && dragon[pos].owl_defend_certain
	       && ON_BOARD(dragon[pos].owl_defense_point))
	add_owl_uncertain_defense_move(dragon[pos].owl_defense_point, pos);
      /* The owl code found the dragon dead, but was uncertain,
       * and an extra point of attack was found, so this might
       * be a good place to play.
       */
      else if (dragon[pos].origin == pos
	       && dragon[pos].owl_status == DEAD
	       && board[pos] == OTHER_COLOR(color)
	       && !dragon[pos].owl_attack_certain
	       && ON_BOARD(dragon[pos].owl_attack_point))
	add_owl_uncertain_defense_move(dragon[pos].owl_attack_point, pos);
    }
}

/* Use the owl code to determine whether the move at (ti, tj) makes
 * the dragon at (m, n) owl safe. This is used to test whether
 * tactical defenses are strategically viable and whether a vital eye
 * point does kill an owl critical dragon. 
 *
 * Should be called only when stackp==0.
 */

int
owl_does_defend(int ti, int tj, int m, int n)
{
  int move = POS(ti, tj);
  int str = POS(m, n);
  int color = board[str];
  int result = 0;
  static struct local_owl_data owl;
  int reading_nodes_when_called = get_reading_node_counter();
  int tactical_nodes;
  int opos;
  owl.local_owl_node_counter = 0;

  if (worm[POS(m, n)].unconditional_status == DEAD)
    return 0;

  opos = dragon[POS(m, n)].origin;
  TRACE("owl_does_defend %m %m(%1m)\n", ti, tj, m, n, opos);

  if (search_persistent_owl_cache(OWL_DOES_DEFEND, move, str, 0,
				  &result, NULL, NULL, NULL))
    return result;

  if (trymove(move, color, "owl_does_defend", str, EMPTY, 0)) {
    /* Check if a compatible owl_attack() is cached. */
    if (search_persistent_owl_cache(OWL_ATTACK, opos, 0, 0,
				    &result, NULL, NULL, NULL)) {
      popgo();
      if (!result)
	return WIN;
      else
	return 0;
    }
    
    owl.lunches_are_current = 0;
    owl_mark_dragon(m, n, -1, -1, &owl);
    owl_update_goal(move, 2, &owl);
    compute_owl_escape_values(&owl);
    if (!do_owl_attack(str, NULL, &owl, EMPTY, 0))
      result = WIN;
    owl.lunches_are_current = 0;
    popgo();
  }
  else
    return 0;  /* Don't cache anything in this case. */

  tactical_nodes = get_reading_node_counter() - reading_nodes_when_called;

  DEBUG(DEBUG_OWL_PERFORMANCE,
	"owl_does_defend %1m %1m(%1m), result %d (%d, %d nodes)\n",
	move, str, opos, result,
	owl.local_owl_node_counter, tactical_nodes);

  store_persistent_owl_cache(OWL_DOES_DEFEND, move, str, 0,
			     result, 0, 0, 0,
			     tactical_nodes, owl.goal, board[str]);

  return result;
}


/* Use the owl code to determine whether the dragon at (m, n) is owl
 * safe after an own move at (ti, tj). This is used to detect
 * blunders. In case the dragon is not safe, it also tries to find a
 * defense point making (ti, tj) safe in a later move.
 *
 * Should be called only when stackp==0.
 */

int
owl_confirm_safety(int ti, int tj, int m, int n, int *di, int *dj)
{
  int move = POS(ti, tj);
  int str = POS(m, n);
  int color = board[str];
  int result = 0;
  static struct local_owl_data owl;
  int reading_nodes_when_called = get_reading_node_counter();
  int tactical_nodes;
  int opos;
  int defense = 0;
  owl.local_owl_node_counter = 0;

  if (worm[POS(m, n)].unconditional_status == DEAD)
    return 0;

  opos = dragon[POS(m, n)].origin;
  TRACE("owl_confirm_safety %m %m(%1m)\n", ti, tj, m, n, opos);

  if (search_persistent_owl_cache(OWL_CONFIRM_SAFETY, move, str, 0,
				  &result, &defense, NULL, NULL)) {
    if (di) *di = I(defense);
    if (dj) *dj = J(defense);
    return result;
  }

  if (trymove(move, color, "owl_confirm_safety", str, EMPTY, 0)) {
    /* Check if a compatible owl_attack() is cached. */
    if (search_persistent_owl_cache(OWL_ATTACK, opos, 0, 0,
				    &result, &defense, NULL, NULL)) {
      popgo();
      if (di) *di = I(defense);
      if (dj) *dj = J(defense);
      if (!result)
	return WIN;
      else
	return 0;
    }
    
    owl.lunches_are_current = 0;
    owl_mark_dragon(m, n, -1, -1, &owl);
    owl_update_goal(move, 2, &owl);
    compute_owl_escape_values(&owl);
    if (!do_owl_attack(str, &defense, &owl, EMPTY, 0))
      result = WIN;
    owl.lunches_are_current = 0;
    popgo();
  }
  else
    return 0;  /* Don't cache anything in this case. */

  tactical_nodes = get_reading_node_counter() - reading_nodes_when_called;

  DEBUG(DEBUG_OWL_PERFORMANCE,
	"owl_confirm_safety %1m %1m(%1m), result %d %1m (%d, %d nodes)\n",
	move, str, opos, result, defense,
	owl.local_owl_node_counter, tactical_nodes);

  store_persistent_owl_cache(OWL_DOES_DEFEND, move, str, 0,
			     result, defense, 0, 0,
			     tactical_nodes, owl.goal, BOARD(m, n));

  if (di) *di = I(defense);
  if (dj) *dj = J(defense);

  return result;
}


/* Use the owl code to determine whether the move at (ti, tj) of the
 * string (m, n) is effective, i.e. whether it kills the stones.
 *
 * Should be called only when stackp==0.
 */

int
owl_does_attack(int ti, int tj, int m, int n)
{
  int move = POS(ti, tj);
  int str = POS(m, n);
  int color = board[str];
  int other = OTHER_COLOR(color);
  int result = 0;
  static struct local_owl_data owl;
  int reading_nodes_when_called = get_reading_node_counter();
  owl.local_owl_node_counter = 0;
  owl.lunches_are_current = 0;
  owl_mark_dragon(m, n, -1, -1, &owl);
  compute_owl_escape_values(&owl);
  TRACE("owl_does_attack %m %m\n", ti, tj, m, n);
  
  if (trymove(move, other, "owl_does_attack", str, EMPTY, 0)) {
    owl_update_boundary_marks(move, &owl);
    /* FIXME: Should also check if part of the dragon was captured,
     *        like do_owl_attack() does.
     */
    if (board[str] == EMPTY
	|| !do_owl_defend(str, NULL, &owl, EMPTY, 0))
      result = WIN;
    owl.lunches_are_current = 0;
    popgo();
  }
  
  DEBUG(DEBUG_OWL_PERFORMANCE,
	"owl_does_attack %m %m, result %d (%d, %d nodes)\n",
	ti, tj, m, n, result, owl.local_owl_node_counter,
	get_reading_node_counter() - reading_nodes_when_called);
  return result;
}


/* Use the owl code to determine whether connecting the two dragons
 * (ai, aj) and (bi, bj) by playing at (ti, tj) results in a living
 * dragon. Should be called only when stackp==0.
 */

int
owl_connection_defends(int ti, int tj, int ai, int aj, int bi, int bj)
{
  int move = POS(ti, tj);
  int apos = POS(ai, aj);
  int bpos = POS(bi, bj);
  int color = board[apos];
  int result = 0;
  int reading_nodes_when_called = get_reading_node_counter();
  int tactical_nodes;
  static struct local_owl_data owl;

  gg_assert(board[bpos] == color);
  TRACE("owl_connection_defends %m %m %m\n", ti, tj, ai, aj, bi, bj);

  if (worm[POS(ai, aj)].unconditional_status == DEAD)
    return 0;

  if (search_persistent_owl_cache(OWL_CONNECTION_DEFENDS, move, apos,
				  bpos, &result, NULL, NULL, NULL))
    return result;

  owl.local_owl_node_counter = 0;
  owl.lunches_are_current = 0;
  owl_mark_dragon(ai, aj, bi, bj, &owl);
  compute_owl_escape_values(&owl);

  if (trymove(move, color, "owl_connection_defends", apos, EMPTY, 0)) {
    owl_update_goal(move, 2, &owl);
    if (!do_owl_attack(move, NULL, &owl, EMPTY, 0))
      result = WIN;
    owl.lunches_are_current = 0;
    popgo();
  }
  tactical_nodes = get_reading_node_counter() - reading_nodes_when_called;
  
  DEBUG(DEBUG_OWL_PERFORMANCE,
	"owl_conn_defends %m %m %m, result %d (%d, %d nodes)\n",
	ti, tj, ai, aj, bi, bj, result, owl.local_owl_node_counter,
	tactical_nodes);

  store_persistent_owl_cache(OWL_CONNECTION_DEFENDS, move, apos, bpos,
			     result, 0, 0, 0, tactical_nodes, owl.goal, color);

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
 * 2. No neighbor of the liberty is the same color as the tested string.
 * 3. No neighbor of the liberty of the same color as the goal dragon
 *    does not belong to the goal dragon.
 * 4. No neighbor of the liberty belonging to the goal dragon can be
 *     tactically captured.
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
 * tactically captured.
 */

static void
owl_find_lunches(struct local_owl_data *owl)
{
  int m, n;
  int k;
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
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++)
      if (BOARD(m, n) == color && owl->goal[POS(m, n)]) {
	/* Loop over the eight neighbors. */
	for (k = 0; k < 8; k++) {
	  int dm = deltai[k];
	  int dn = deltaj[k];
	  
	  /* If the immediate neighbor is empty, we look two steps away. */
	  if (k < 4 && BOARD(m+dm, n+dn) == EMPTY) {
	    dm = 2*dm;
	    dn = 2*dn;
	  }
	  
	  if (BOARD(m+dm, n+dn) != other)
	    continue;
	    
	  lunch = find_origin(POS(m+dm, n+dn));
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
	    int stones[MAX_BOARD*MAX_BOARD];
	    int liberties;
	    int libs[MAXLIBS];
	    int r;
	    int essential = 0;
	    int superstring[BOARDMAX];

	    /* First check the neighbors of the string. */
	    adj = chainlinks(lunch, adjs);
	    for (r = 0; r < adj; r++) {
	      if (!owl->goal[adjs[r]]
		  || attack(adjs[r], NULL) != 0) 
	      {
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
	      if (!goal_found)
		essential = 1;
	      
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
  
  owl->lunches_are_current = 1;
  sgf_dumptree = save_sgf_dumptree;
  count_variations = save_count_variations;
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
  current_owl_data = owla;
  other_owl_data = owlb;
  
  if (!owla->lunches_are_current)
    owl_find_lunches(owla);
  make_domains(owla->black_eye, owla->white_eye, 1);
  
  if (owlb) {
    if (!owlb->lunches_are_current)
      owl_find_lunches(owlb);
    make_domains(owlb->black_eye, owlb->white_eye, 1);
  }
}

/* True unless (i, j) is EMPTY or occupied by a lunch for the goal dragon.  
 * Used during make_domains (see optics.c: lively macro). A ``lively''
 * worm is one that might be alive, hence cannot be ignored in 
 * determining eye spaces.
 */

int 
owl_lively(int i, int j)
{
  int origin;
  int lunch;
  ASSERT_ON_BOARD2(i, j);

  if (BOARD(i, j) == EMPTY)
    return 0;
  origin = find_origin(POS(i, j));

  /* Lunches that can't be saved are dead, so don't report them as lively */
  for (lunch = 0; lunch < MAX_LUNCHES; lunch++)
    if (current_owl_data->lunch[lunch] == origin
	&& current_owl_data->lunch_defense_point[lunch] == NO_MOVE)
      return 0;

  /* Inessential stones are not lively. */
  if (current_owl_data->inessential[origin])
    return 0;
  
  /* When reading a semeai there is a second set of owl data to consider */
  if (other_owl_data)
    for (lunch = 0; lunch < MAX_LUNCHES; lunch++)
      if (other_owl_data->lunch[lunch] == origin
	  && other_owl_data->lunch_defense_point[lunch] == NO_MOVE)
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

  if (owl_safe_move_cache[move])
    return owl_safe_move_cache[move]-1;

  if (trymove(move, color, "owl_safe_move", 0, EMPTY, 0)) {
    acode = attack(move, NULL);
    if (acode != WIN)
      safe = 1;
    else
      safe = 0;
    current_owl_data->lunches_are_current = 0;
    popgo();
  }
  owl_safe_move_cache[move] = safe+1;
  return safe;
}
  

/* This function, called when stackp==0, returns true if capturing
 * the string at (i,j) results in a live group.
 */

#define MAX_SUBSTANTIAL_LIBS 10

int
owl_substantial(int i, int j)
{
  int str = POS(i, j);
  int k;
  int m, n;
  int libs[MAX_SUBSTANTIAL_LIBS + 1];
  int liberties = findlib(str, MAX_SUBSTANTIAL_LIBS+1, libs);
  int reading_nodes_when_called = get_reading_node_counter();
  int tactical_nodes;
  static struct local_owl_data owl;
  int result;

  owl.color = OTHER_COLOR(board[str]);
  owl.local_owl_node_counter = 0;
  gg_assert(stackp == 0);

  /* Big strings are always substantial since the biggest nakade is
   * six stones. (There are probably rare exceptions to this
   * rule, but they are unlikely to come up in a game.)
   */
  if (countstones(str) > 6)
    return 1;
  
  if (liberties > MAX_SUBSTANTIAL_LIBS)
    return 0;
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++)
      owl.goal[POS(m, n)] = 0;
  /* Mark the neighbors of the string. If one is found which is alive, return
   * true. */
  {
    int adjs[MAXCHAIN];
    int adj;

    adj = chainlinks(str, adjs);
    for (k = 0; k < adj; k++) {
      if (dragon[adjs[k]].matcher_status == ALIVE)
	return 1;
      for (m = 0; m < board_size; m++)
	for (n = 0; n < board_size; n++)
	  if (same_dragon(POS(m, n), adjs[k]))
	    owl.goal[POS(m, n)] = 1;
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
    if (trymove(libs[k], owl.color, NULL, 0, EMPTY, 0)) {
      if (level >= 10)
	increase_depth_values();
      owl.goal[libs[k]] = 1;
    }
    else {
      /* if we can't fill, try swapping with the next liberty */
      if (k < liberties-1
	  && trymove(libs[k+1], owl.color, NULL, 0, EMPTY, 0)) {
	owl.goal[libs[k]] = 1;
	libs[k+1] = libs[k];
      }
      else {
	/* Can't fill the liberties. Give up! */
	while (stackp > 0) {
	  if (level >= 10)
	    decrease_depth_values();
	  popgo();
	}
	return 0;
      }
    }
  }
  compute_owl_escape_values(&owl);
  owl_mark_boundary(&owl);
  owl.lunches_are_current = 0;

  if (do_owl_attack(libs[0], NULL, &owl, EMPTY, 0))
    result = 0;
  else
    result = 1;
  while (stackp > 0) {
    if (level >= 10)
      decrease_depth_values();
    popgo();
  }

  tactical_nodes = get_reading_node_counter() - reading_nodes_when_called;
  DEBUG(DEBUG_OWL_PERFORMANCE,
	"owl_substantial %m, result %d (%d, %d nodes)\n",
	i, j, result, owl.local_owl_node_counter,
	tactical_nodes);

  store_persistent_owl_cache(OWL_SUBSTANTIAL, str, 0, 0, result, 0, 0, 0,
			     tactical_nodes, owl.goal, owl.color);

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
sniff_lunch(int pos, int *min, int *probable, int *max,
	    struct local_owl_data *owl)
{
  int other = OTHER_COLOR(board[pos]);
  int size;

  ASSERT1(board[pos] != EMPTY, pos);

  if (owl->boundary[pos] == 2) {
    *min = 2;
    *probable = 2;
    *max = 2;
    return;
  }

  size = countstones(pos);
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
    findstones(pos, 2, stones);
    /* A lunch on a 1-2 point tends always to be worth contesting. */
    if ((obvious_false_eye(stones[0], other)
	|| obvious_false_eye(stones[1], other))
	&& !(one_two_point(stones[0]) || one_two_point(stones[1]))) {
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
    if (!obvious_false_eye(pos, other)) {
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


/* Conservative relative of topological_eye. Essentially the same
 * algorithm is used, but only tactically safe opponent strings on
 * diagonals are considered. This may underestimate the false/half eye
 * status, but it should never be overestimated.
 */
int
obvious_false_eye(int pos, int color)
{
  int i = I(pos);
  int j = J(pos);
  int k;
  int diagonal_sum = 0;
  for (k = 4; k < 8; k++) {
    int di = deltai[k];
    int dj = deltaj[k];
    
    if (!ON_BOARD2(i+di, j) && !ON_BOARD2(i, j+dj))
      diagonal_sum--;
    
    if (!ON_BOARD2(i+di, j+dj))
      diagonal_sum++;
    else if (BOARD(i+di, j+dj) == OTHER_COLOR(color)
	     && !attack(POS(i+di, j+dj), NULL))
      diagonal_sum += 2;
  }
  
  return diagonal_sum >= 4;
}


/* Trampoline to topological_eye(). */
int
owl_topological_eye(int m, int n, int color)
{
  return topological_eye(m, n, color, NULL, NULL, NULL, NULL,
			 current_owl_data->black_eye,
			 current_owl_data->white_eye,
			 current_owl_data->half_eye);
}

/* This function returns true if it is judged that the capture of the
 * string at (m,n) is sufficient to create one eye.
 */

int
vital_chain(int pos)
{
  int min;
  int probable;
  int max;
  sniff_lunch(pos, &min, &probable, &max, current_owl_data);

  if (max > 0)
    return 1;
  
  return 0;
}


static void
compute_owl_escape_values(struct local_owl_data *owl)
{
  int pos;
  
  compute_escape_influence(owl->goal, owl->color, owl->escape_values, 0);
  DEBUG(DEBUG_ESCAPE, "Owl escape values:\n");

  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (!ON_BOARD(pos))
      continue;
    if (board[pos] == owl->color) {
      if (dragon[pos].status == ALIVE)
	owl->escape_values[pos] = 6;
      else if (dragon[pos].status == UNKNOWN
	       && (DRAGON2(pos).escape_route > 5 || DRAGON2(pos).moyo > 5))
	owl->escape_values[pos] = 4;
    }
    DEBUG(DEBUG_ESCAPE, "%o%d", owl->escape_values[pos]);
  }
  
  DEBUG(DEBUG_ESCAPE, "%o\n");
}


/* Used by autohelpers. */
int
owl_escape_value(int m, int n)
{
  /* FIXME: Should have a more robust mechanism to avoid 
   * escaping inwards. Returning a negative value is just a kludge.
   */
  int k;
  if (current_owl_data->goal[POS(m, n)])
    return -10;

  if (BOARD(m, n) == EMPTY) {
    for (k = 0; k < 8; k++) {
      int i = m + deltai[k];
      int j = n + deltaj[k];
      if (ON_BOARD2(i, j) && current_owl_data->goal[POS(i, j)])
	return -10;
    }
  }
  
  return current_owl_data->escape_values[POS(m, n)];
}


/* Used by autohelpers. */
int
owl_goal_dragon(int pos)
{
  return current_owl_data->goal[pos] != 0;
}

/* Used by autohelpers.
 * Returns 1 if (ai, aj) is an eyespace for the color having a stone
 * at (bi, bj), but only if it's worth at least a half eye.
 */
int
owl_eyespace(int ai, int aj, int bi, int bj)
{
  int opos;
  ASSERT2(BOARD(bi, bj) != EMPTY, bi, bj);
  
  if (BOARD(bi, bj) == WHITE) {
    opos = current_owl_data->white_eye[POS(ai, aj)].origin;
    return (!is_pass(opos)
        && current_owl_data->white_eye[opos].color == WHITE_BORDER
	&& current_owl_data->white_eye[opos].maxeye > 0);
  }
  else {
    opos = current_owl_data->black_eye[POS(ai, aj)].origin;
    return (!is_pass(opos)
	&& current_owl_data->black_eye[opos].color == BLACK_BORDER
	&& current_owl_data->black_eye[opos].maxeye > 0);
  }
}
  

/* Used by autohelpers.
 * Returns 1 if (ai, aj) is an eyespace for the color having a stone
 * at (bi, bj), which is possibly worth 2 eyes.
 */
int
owl_big_eyespace(int ai, int aj, int bi, int bj)
{
  int opos;
  ASSERT2(BOARD(bi, bj) != EMPTY, bi, bj);
  
  ASSERT_ON_BOARD2(ai, aj);

  if (BOARD(bi, bj) == WHITE) {
    opos = current_owl_data->white_eye[POS(ai, aj)].origin;
    return (ON_BOARD(opos) 
	&& current_owl_data->white_eye[opos].color == WHITE_BORDER
	&& current_owl_data->white_eye[opos].maxeye == 2);
  }
  else {
    opos = current_owl_data->black_eye[POS(ai, aj)].origin;
    return (ON_BOARD(opos)
	&& current_owl_data->black_eye[opos].color == BLACK_BORDER
	&& current_owl_data->black_eye[opos].maxeye == 2);
  }
}
  

static int
owl_escape_route(struct local_owl_data *owl)
{
  return dragon_escape(owl->goal, owl->color, owl->escape_values);
}


/***********************
 * Storage of owl data
 ***********************/

/* Push owl data onto a stack. The stack is dynamically reallocated if
 * it is too small.
 */
static void
push_owl(struct local_owl_data *owl)
{
  int new_size = owl_stack_size;

  /* Do we need to enlarge the stack. */
  if (owl_stack_size == 0)
    new_size = owl_reading_depth;
  else if (owl_stack_pointer == owl_stack_size)
    new_size++;

  /* If so, reallocate space. */
  if (new_size > owl_stack_size) {
    owl_stack = realloc(owl_stack, new_size * sizeof(*owl_stack));
    gg_assert(owl_stack != NULL);
    owl_stack_size = new_size;
  }

  /* Store the owl data. */
  owl_stack[owl_stack_pointer] = *owl;
  owl_stack_pointer++;
}

/* Retrieve owl data from the stack. The local_owl_node_counter field
 * is not reset.
 */
static void
pop_owl(struct local_owl_data *owl)
{
  int nodes = owl->local_owl_node_counter;
  owl_stack_pointer--;
  *owl = owl_stack[owl_stack_pointer];
  owl->local_owl_node_counter = nodes;
}

/***********************
 * Persistent owl cache
 ***********************/

#define HIGH_LIBERTY_BIT 4

/* FIXME: Unify with the same function in reading.c. */
static void
draw_active_area(char board[BOARDMAX])
{
  int i, j, ii;
  int c = ' ';

  start_draw_board();
  
  for (i = 0; i < board_size; i++) {
    ii = board_size - i;
    fprintf(stderr, "\n%2d", ii);
    
    for (j = 0; j < board_size; j++) {
      if (board[POS(i, j)] == EMPTY)
	c = '.';
      else if (board[POS(i, j)] == WHITE)
	c = 'o';
      else if (board[POS(i, j)] == (WHITE | HIGH_LIBERTY_BIT))
	c = 'O';
      else if (board[POS(i, j)] == BLACK)
	c = 'x';
      else if (board[POS(i, j)] == (BLACK | HIGH_LIBERTY_BIT))
	c = 'X';
      if (board[POS(i, j)] == GRAY)
	c = '?';
      
      fprintf(stderr, " %c", c);
    }
    
    fprintf(stderr, " %d", ii);
  }

  end_draw_board();
}

/* Returns 1 if the stored board is compatible with the current board,
 * 0 otherwise.
 */
static int
verify_stored_board(char p[BOARDMAX])
{
  int m, n;
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++)
      if (p[POS(m, n)] == GRAY)
	continue;
      else if ((p[POS(m, n)] & 3) != BOARD(m, n))
	return 0;
      else if (!(p[POS(m, n)] & HIGH_LIBERTY_BIT))
	continue;
      else if (countlib2(m, n) <= 4)
	return 0;
  
  return 1;
}

/* Remove persistent cache entries which are no longer compatible with
 * the board. For efficient use of the cache, it's recommended to call
 * this function once per move, before starting the owl reading. It's
 * not required for correct operation though. 
 */
void purge_persistent_owl_cache()
{
  int k;
  static int last_purge_position_number = -1;
  gg_assert(stackp == 0);

  /* Never do this more than once per move. */
  if (last_purge_position_number == position_number)
    return;
  else
    last_purge_position_number = position_number;

  for (k = 0; k < persistent_owl_cache_size; k++) {
    if (!verify_stored_board(persistent_owl_cache[k].board)) {
      /* Move the last entry in the cache here and back up the loop
       * counter to redo the test at this position in the cache.
       */
      if (k < persistent_owl_cache_size - 1)
	persistent_owl_cache[k] 
	  = persistent_owl_cache[persistent_owl_cache_size - 1];
      k--;
      persistent_owl_cache_size--;
    }
  }
}

static int
search_persistent_owl_cache(int routine, int apos, int bpos, int cpos,
			    int *result, int *move, int *move2, int *certain)
{
  int k;
  gg_assert(stackp == 0 || stackp == 1);

  for (k = 0; k < persistent_owl_cache_size; k++) {
    if (persistent_owl_cache[k].routine == routine
	&& persistent_owl_cache[k].apos == apos
	&& persistent_owl_cache[k].bpos == bpos
	&& persistent_owl_cache[k].cpos == cpos
	&& verify_stored_board(persistent_owl_cache[k].board)) {
      *result = persistent_owl_cache[k].result;
      if (move) *move = persistent_owl_cache[k].move;
      if (move2) *move2 = persistent_owl_cache[k].move2;
      if (certain) *certain = persistent_owl_cache[k].result_certain;
      return 1;
    }
  }
  return 0;
}

static void
store_persistent_owl_cache(int routine, int apos, int bpos, int cpos,
			   int result, int move, int move2, int certain,
			   int tactical_nodes,
			   char goal[BOARDMAX], int goal_color)
{
  char active[BOARDMAX];
  int m, n;
  int k;
  int r;
  int other = OTHER_COLOR(goal_color);
  gg_assert(stackp == 0);

  /* If cache is full, first try to purge it. */
  if (persistent_owl_cache_size == MAX_OWL_CACHE_SIZE)
    purge_persistent_owl_cache();

  /* FIXME: Kick out oldest or least expensive entry instead of giving up. */
  if (persistent_owl_cache_size == MAX_OWL_CACHE_SIZE) {
    DEBUG(DEBUG_OWL_PERFORMANCE, "Persistent owl cache full.\n");
    return;
  }

  persistent_owl_cache[persistent_owl_cache_size].routine = routine;
  persistent_owl_cache[persistent_owl_cache_size].apos	  = apos;
  persistent_owl_cache[persistent_owl_cache_size].bpos	  = bpos;
  persistent_owl_cache[persistent_owl_cache_size].cpos	  = cpos;
  persistent_owl_cache[persistent_owl_cache_size].result  = result;
  persistent_owl_cache[persistent_owl_cache_size].result_certain = certain;
  persistent_owl_cache[persistent_owl_cache_size].move	  = move;
  persistent_owl_cache[persistent_owl_cache_size].move2	  = move2;
  persistent_owl_cache[persistent_owl_cache_size].tactical_nodes =
    tactical_nodes;
  persistent_owl_cache[persistent_owl_cache_size].movenum = movenum;
  
  /* Remains to set the board. We let the active area be
   * the goal +
   * distance four expansion through empty intersections and own stones +
   * adjacent opponent strings +
   * liberties of adjacent opponent strings with less than five liberties +
   * liberties of low liberty neighbors of adjacent opponent strings
   * with less than five liberties.
   */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++)
      active[POS(m, n)] = (goal[POS(m, n)] != 0);

  /* Also add critical moves to the active area. */
  if (ON_BOARD1(move))
    active[move] = 1;

  if (ON_BOARD1(move2))
    active[move2] = 1;

  for (k = 1; k < 5; k++) {
    for (m = 0; m < board_size; m++)
      for (n = 0; n < board_size; n++) {
	int pos = POS(m, n);
	if (board[pos] == other || active[pos] != 0) 
	  continue;
	if ((ON_BOARD(SOUTH(pos)) && active[SOUTH(pos)])
	    || (ON_BOARD(WEST(pos)) && active[WEST(pos)])
	    || (ON_BOARD(NORTH(pos)) && active[NORTH(pos)])
	    || (ON_BOARD(EAST(pos)) && active[EAST(pos)])) {
	  if (board[pos] == EMPTY)
	    active[pos] = k + 1;
	  else
	    mark_string(pos, active, (char) (k + 1));
	}
      }
  }
  
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      int pos = POS(m, n);
      if (board[pos] != other || active[pos] != 0) 
	continue;
      for (r = 0; r < 4; r++) {
	int pos2 = pos + delta[r];
	if (ON_BOARD(pos2) && board[pos2] != other && active[pos2] != 0) {
	  active[pos] = 1;
	  break;
	}
      }
    }
  
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      int pos = POS(m, n);
      if (board[pos] == other && active[pos] != 0 && countlib(pos) < 5) {
	int libs[4];
	int liberties = findlib(pos, 4, libs);
	int adjs[MAXCHAIN];
	int adj;
	for (r = 0; r < liberties; r++)
	  active[libs[r]] = 1;
	
	/* Also add liberties of neighbor strings if these are three
	 * or less.
	 */
	adj = chainlinks(pos, adjs);
	for (r = 0; r < adj; r++) {
	  if (countlib(adjs[r]) <= 3) {
	    int s;
	    liberties = findlib(adjs[r], 3, libs);
	    for (s = 0; s < liberties; s++)
	      active[libs[s]] = 1;
	  }
	}
      }
    }

  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      int pos = POS(m, n);
      int value = board[pos];
      if (!active[pos])
	value = GRAY;
      else if (board[pos] != EMPTY && countlib2(m, n) > 4)
	value |= HIGH_LIBERTY_BIT;
	
      persistent_owl_cache[persistent_owl_cache_size].board[pos] = value;
    }

  if (0) {
    gprintf("%o Stored result in cache (entry %d):\n",
	    persistent_owl_cache_size);
    print_persistent_owl_cache_entry(persistent_owl_cache_size);
  }
  
  persistent_owl_cache_size++;
}


/* For debugging purposes. */
static void
print_persistent_owl_cache_entry(int k)
{
  struct owl_cache *entry = &(persistent_owl_cache[k]);
  gprintf("%omovenum         = %d\n",  entry->movenum);
  gprintf("%otactical_nodes  = %d\n",  entry->tactical_nodes);
  gprintf("%oroutine         = %d\n",  entry->routine);
  gprintf("%o(apos)  	     = %1m\n", entry->apos);
  gprintf("%o(bpos)  	     = %1m\n", entry->bpos);
  gprintf("%o(cpos)  	     = %1m\n", entry->cpos);
  gprintf("%oresult          = %d\n",  entry->result);
  gprintf("%o(move)          = %1m\n", entry->move);
  gprintf("%o(move2)         = %1m\n", entry->move2);
  
  draw_active_area(entry->board);
}


/* Helper for the owl_hotspots() function below. */
static void
mark_dragon_hotspot_values(float values[MAX_BOARD][MAX_BOARD],
			   int pos, float contribution)
{
  int i, j, k;
  ASSERT1(board[pos] != EMPTY, pos);
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++) {
      if (BOARD(i, j) != EMPTY)
	continue;
      for (k = 0; k < 8; k++) {
	int di = deltai[k];
	int dj = deltaj[k];
	if (ON_BOARD2(i+di, j+dj)
	    && BOARD(i+di, j+dj) != EMPTY
	    && same_dragon(POS(i+di, j+dj), pos)
	    && (countlib2(i+di, j+dj) <= 4
		|| i == 0 || i == board_size-1
		|| j==0 || j == board_size-1)) {
	  if (k < 4) {
	    values[i][j] += contribution;
	    break;
	  }
	  else {
	    if (BOARD(i+di, j) == EMPTY || countlib2(i+di, j) <= 2
		|| BOARD(i, j+dj) == EMPTY || countlib2(i, j+dj) <= 2)
	      values[i][j] += 0.5 * contribution;
	    break;
	  }
	}
      }
    }
}
  

/* Based on the entries in the owl cache and their tactical_nodes
 * field, compute where the relatively most expensive owl reading is
 * going on.
 */
void
owl_hotspots(float values[MAX_BOARD][MAX_BOARD])
{
  int m, n, k, r;
  int libs[MAXLIBS];
  int liberties;
  int sum_tactical_nodes = 0;

  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++)
      values[m][n] = 0.0;
  
  /* Compute the total number of tactical nodes for the cached entries. */
  for (k = 0; k < persistent_owl_cache_size; k++)
    sum_tactical_nodes += persistent_owl_cache[k].tactical_nodes;

  if (sum_tactical_nodes <= 100)
    return;

  /* Loop over all entries and increase the value of vertices adjacent
   * to dragons involving expensive owl reading.
   */
  for (k = 0; k < persistent_owl_cache_size; k++) {
    struct owl_cache *entry = &(persistent_owl_cache[k]);
    float contribution = entry->tactical_nodes / (float) sum_tactical_nodes;
    if (0) {
      gprintf("Owl hotspots: %d %1m %f\n", entry->routine, entry->apos,
	      contribution);
    }
    switch (entry->routine) {
    case OWL_ATTACK:
    case OWL_THREATEN_ATTACK:
    case OWL_DEFEND:
    case OWL_THREATEN_DEFENSE:
      mark_dragon_hotspot_values(values, entry->apos, contribution);
      break;
    case OWL_DOES_DEFEND:
    case OWL_DOES_ATTACK:
      mark_dragon_hotspot_values(values, entry->bpos, contribution);
      break;
    case OWL_CONNECTION_DEFENDS:
      mark_dragon_hotspot_values(values, entry->bpos, contribution);
      mark_dragon_hotspot_values(values, entry->cpos, contribution);
      break;
    case OWL_SUBSTANTIAL:
      /* Only consider the liberties of (ai, aj). */
      liberties = findlib(entry->apos, MAXLIBS, libs);
      for (r = 0; r < liberties; r++)
	values[I(libs[r])][J(libs[r])] += contribution;
      break;
    default:
      gg_assert(0); /* Shouldn't happen. */
      break;
    }
  }
}


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
