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

#include "liberty.h"
#include "influence.h"
#include "patterns.h"
#include "gg_utils.h"

static void add_influence_source(int pos, int color, float strength,
                                 float attenuation,
                                 struct influence_data *q);
static void segment_influence(struct influence_data *q);
void print_influence(const struct influence_data *q,
		     const char *info_string);
static void print_numeric_influence(const struct influence_data *q,
				    const float values[BOARDMAX],
				    const char *format, int width,
				    int draw_stones, int mark_epsilon);
static void print_influence_areas(const struct influence_data *q);
 
static void value_territory(struct influence_data *q);
static void enter_intrusion_source(int source_pos, int strength_pos,
                                   float strength, float attenuation,
                                   struct influence_data *q);
static void add_marked_intrusions(struct influence_data *q, int color);
 

/* Influence computed for the initial position, i.e. before making
 * some move.
 */
struct influence_data initial_black_influence;
struct influence_data initial_white_influence;

/* Influence computed after some move has been made. */
struct influence_data move_influence;
struct influence_data followup_influence;

/* Influence used for estimation of escape potential. */
static struct influence_data escape_influence;

/* Pointer to influence data used during pattern matching. */
static struct influence_data *current_influence = NULL;

/* Cache of delta_territory_values. */
static float delta_territory_cache[BOARDMAX];
static float followup_territory_cache[BOARDMAX];
static int territory_cache_position_number = -1;
static int territory_cache_color = -1;

/* If set, print influence map when computing this move. Purely for
 * debugging.
 */
static int debug_influence = NO_MOVE;

/* We use fixed point arithmetics in accumulate_influence(). Everything
 * is stored as (int) value * GG_ONE.
 */
#define GG_ONE (1 << 12)
#define FLOAT2FIXED(f) ((int) ((f) * GG_ONE) + 0.5)
#define FIXED2FLOAT(i) (((float) (i)) / GG_ONE)
#define FIXED_MULT(i, j) ((i) * (j) / GG_ONE)

/* This is the core of the influence function. Given the coordinates
 * and color of an influence source, it radiates the influence
 * outwards until it hits a barrier or the strength of the influence
 * falls under a certain threshold.
 *
 * The radiation is performed by a breadth first propagation,
 * implemented by means of an internal queue.
 *
 * Since this function has turned out be one of the bottlenecks, loop
 * unrolling makes a noticeable performance difference. It does,
 * however, make the code much harder to read and maintain. Therefore
 * we include both the original and the unrolled versions.
 */

#define EXPLICIT_LOOP_UNROLLING 1

#if EXPLICIT_LOOP_UNROLLING
/* In addition to the parameters, this macro expects
 *  m,n = original source of influence
 *  ii = point influence is being spread from
 *  delta_i = I(ii) - m
 *  delta_j = J(ii) - n
 *  current_strength combines strength and damping factor
 *  b is 1/(square of distance from m,n to i,j) ; or halved
 *    for diagonals
 * 
 *  arg is POS(i + arg_di, j + arg_dj)
 *  arg_d is 1 for diagonal movement
 *
 */


#define code1(arg_di, arg_dj, arg, arg_d) do { \
      if (!safe[arg] \
	  && ((arg_di)*(delta_i) + (arg_dj)*(delta_j) > 0 \
	      || queue_start == 1)) { \
	int contribution; \
	int permeability = permeability_array[ii]; \
	if (arg_d) { \
	  permeability = FIXED_MULT(permeability, \
		                    gg_max(permeability_array \
				               [ii + DELTA(arg_di, 0)], \
			                   permeability_array \
					       [ii + DELTA(0, arg_dj)])); \
	  if (permeability == 0) \
	    continue; \
	} \
	contribution = FIXED_MULT(current_strength, permeability); \
	if (queue_start != 1) { \
	  int a = (arg_di)*(delta_i) + (arg_dj)*(delta_j); \
	  contribution *= (a*a); /* contribution *= cos(phi) */ \
	  contribution = FIXED_MULT(b, contribution); \
	} \
	if (contribution <= FLOAT2FIXED(INFLUENCE_CUTOFF)) \
	  continue; \
	if (working[arg] == 0) { \
	  queue[queue_end] = (arg); \
	  queue_end++; \
	} \
	working[arg] += contribution; \
      } } while (0) 
#endif

/* Propagates the influence from (pos) with given strength and attenuation
 * across the board.
 */

static void
accumulate_influence(int pos, int strength, int inv_attenuation,
    		     int inv_diagonal_damping,
                     const int permeability_array[BOARDMAX],
		     const char safe[BOARDMAX],
		     int influence[BOARDMAX])
{
  int ii;
  int m = I(pos);
  int n = J(pos);
  int k;
  static int queue[BOARDMAX];
  int d;
  int b;

  /* Clear the queue. Entry 0 is implicitly (m, n). */
  int queue_start = 0;
  int queue_end = 1;

  static int working[BOARDMAX];
  static int working_area_initialized = 0;

  if (!working_area_initialized) {
    for (ii = 0; ii < BOARDMAX; ii++)
      working[ii] = 0;
    working_area_initialized = 1;
  }

  /* We put the original source into slot 0.  */
  queue[0] = pos;
  working[pos] = strength;

  /* Spread influence until the stack is empty. */
  while (queue_start < queue_end) {
    int current_strength;
    int delta_i, delta_j;

    ii = queue[queue_start];
    delta_i = I(ii) - m;
    delta_j = J(ii) - n;
    queue_start++;
    if (permeability_array[ii] == 0)
      continue;
    if (0)
      gprintf("Picked %1m from queue. w=%f start=%d end=%d\n",
	      ii, working[ii], queue_start, queue_end);
    if (queue_start == 1)
      b = GG_ONE;
    else
      b = GG_ONE / ((delta_i)*(delta_i) + (delta_j)*(delta_j));

    current_strength = FIXED_MULT(working[ii], inv_attenuation);

#if !EXPLICIT_LOOP_UNROLLING
    /* Try to spread influence in each of the eight directions. */    
    for (d = 0; d < 8; d++) {
      int di = deltai[d];
      int dj = deltaj[d];
      int d_ii = delta[d];

      /* Verify that (ii + d_ii) is
       * 1. Inside the board.
       * 2. Not occupied.
       * 3. Directed outwards. For the origin all directions are outwards.
       */
      if (ON_BOARD(ii + d_ii)
	  && q->p[ii + d_ii] == EMPTY
	  && (di*(delta_i) + dj*(delta_j) > 0
	      || queue_start == 1)) {

	float contribution;
	float permeability = permeability_array[ii];
	float dfactor;
	float inv_damping;

	/* Now compute the damping of the influence.
	 * First we have the permeability at the point we are
	 * spreading from. For diagonal movement we also take the
	 * permeability of the vertices we are "passing by" into
	 * account.
	 */
	if (d > 3) { /* diagonal movement */
	  permeability *= gg_max(permeability_array[ii + DELTA(di, 0)],
				 permeability_array[ii + DELTA(0, dj)]);
	  inv_damping = inv_diagonal_damping;
	  dfactor = 0.5;
	}
	else {
	  inv_damping = 1.0;
	  dfactor = 1.0;
	}

	if (permeability == 0.0)
	  continue;

	contribution = permeability * current_strength * inv_damping;

	/* Finally direction dependent damping. */
	if (ii != pos) {
	  int a = di*(delta_i) + dj*(delta_j);
	  gg_assert(a > 0);
	  contribution *= (a*a) * b * dfactor;
	}

	/* Stop spreading influence if the contribution becomes too low. */
	if (contribution <= INFLUENCE_CUTOFF)
	  continue;
	
	/* If no influence here before, add the point to the queue for
	 * further spreading.
	 */
	if (0)
	  gprintf("  Spreading %s influence from %1m to %1m, d=%d\n",
		  color_to_string(color), ii, ii + d_ii, d);
	if (working[ii + d_ii] == 0.0) {
	  queue[queue_end] = ii + d_ii;
	  queue_end++;
	}
	working[ii + d_ii] += contribution;
      }
    }
#else
    for (d = 0; d < 4; d++)
      if (ON_BOARD(ii + delta[d]))
	code1(deltai[d], deltaj[d], ii + delta[d], 0);

    /* Update factors for diagonal movement. */
    b /= 2;
    current_strength = FIXED_MULT(current_strength, inv_diagonal_damping);

    for (; d < 8; d++)
      if (ON_BOARD(ii + delta[d]))
	code1(deltai[d], deltaj[d], ii + delta[d], 1);
#endif
  }
  
  /* Add the values in the working area to the accumulated influence
   * and simultaneously reset the working area. We know that all
   * influenced points were stored in the queue, so we just traverse
   * it.
   */
  for (k = 0; k < queue_end; k++) {
    ii = queue[k];
    if (working[ii] > (FLOAT2FIXED(INFLUENCE_CUTOFF))
	|| influence[ii] == 0)
      influence[ii] += working[ii];
    working[ii] = 0;
  }
}

/* Initialize the influence_data structure.  */

static void
init_influence(struct influence_data *q, int color,
	       const char safe_stones[BOARDMAX], 
	       const float strength[BOARDMAX])
{
  int ii;
  float attenuation;

  if (q != &escape_influence) {
    q->color_to_move = color;
    if (q->is_territorial_influence)
      attenuation = TERR_DEFAULT_ATTENUATION;
    else
      attenuation = DEFAULT_ATTENUATION;
  }
  else {
    q->color_to_move = EMPTY;
    if (q->is_territorial_influence)
      attenuation = 2 * TERR_DEFAULT_ATTENUATION;
    else
      attenuation = 2 * DEFAULT_ATTENUATION;
  }
  
  q->intrusion_counter = 0;

  /* Remember this for later. */
  memcpy(q->safe, safe_stones, BOARDMAX * sizeof(*safe_stones));
  q->captured = black_captured - white_captured;
  
  for (ii = BOARDMIN; ii < BOARDMAX; ii++)
    if (ON_BOARD(ii)) {
      /* Initialize. */
      q->white_influence[ii] = 0.0;
      q->black_influence[ii] = 0.0;
      q->white_attenuation[ii] = attenuation;
      q->black_attenuation[ii] = attenuation;
      q->white_permeability[ii] = 1.0;
      q->black_permeability[ii] = 1.0;
      q->white_strength[ii] = 0.0;
      q->black_strength[ii] = 0.0;
      q->non_territory[ii] = EMPTY;

      if (IS_STONE(board[ii])) {
	if (!safe_stones[ii]) {
	  if (board[ii] == WHITE)
	    q->white_permeability[ii] = 0.0;
	  else
	    q->black_permeability[ii] = 0.0;
	}
	else {
	  if (board[ii] == WHITE) {
	    if (strength)
	      q->white_strength[ii] = strength[ii];
	    else
	      q->white_strength[ii] = DEFAULT_STRENGTH;
	    q->black_permeability[ii] = 0.0;
	  }
	  else {
	    if (strength)
	      q->black_strength[ii] = strength[ii];
	    else
	      q->black_strength[ii] = DEFAULT_STRENGTH;
	    q->white_permeability[ii] = 0.0;
	  }
	}
      }
      else
	/* Ideally, safe_stones[] should always be zero for empty
	 * intersections. This is currently, however, sometimes not true
	 * when an inessential worm gets captured. So we revise this
	 * in our private copy here.
	 */
	q->safe[ii] = 0;
    }
}

/* Adds an influence source at position pos with prescribed strength
 * and attenuation. color can be BLACK, WHITE or both. If there
 * already exists an influence source of the respective color at pos
 * that is stronger than the new one, we do nothing.
 */
static void
add_influence_source(int pos, int color, float strength, float attenuation,
                     struct influence_data *q)
{
  if ((color & WHITE) && (q->white_strength[pos] < strength)) {
    q->white_strength[pos] = strength;
    q->white_attenuation[pos] = attenuation;
  }
  
  if ((color & BLACK) && (q->black_strength[pos] < strength)) {
    q->black_strength[pos] = strength;
    q->black_attenuation[pos] = attenuation;
  }
}

/* Experimental influence: Adds an intrusion as an entry in the list
 * q->intrusions.
 */
static void
enter_intrusion_source(int source_pos, int strength_pos,
                       float strength, float attenuation,
                       struct influence_data *q)
{
  if (q->intrusion_counter >= MAX_INTRUSIONS) {
    TRACE_INFLUENCE("intrusion list exhausted\n");
    return;
  }
  q->intrusions[q->intrusion_counter].source_pos = source_pos;
  q->intrusions[q->intrusion_counter].strength_pos = strength_pos;
  q->intrusions[q->intrusion_counter].strength = strength;
  q->intrusions[q->intrusion_counter].attenuation = attenuation;
  q->intrusion_counter++;
}

/* Comparison of intrusions datas, to sort them. */
static int
compare_intrusions(const void *p1, const void *p2)
{
  const struct intrusion_data *intr1 = p1;
  const struct intrusion_data *intr2 = p2;
  if (intr1->source_pos - intr2->source_pos != 0) {
    return (intr1->source_pos - intr2->source_pos);
  }
  else if (intr1->strength_pos - intr2->strength_pos != 0) {
    return (intr1->strength_pos - intr2->strength_pos);
  }
  else if (intr1->strength > intr2->strength) {
    return 1;
  }
  else
    return -1;
}

/* It may happen that we have a low intensity influence source at a
 * blocked intersection (due to an intrusion). This function resets the
 * permeabilities.
 */
static void
reset_unblocked_blocks(struct influence_data *q)
{
  int pos;
  for (pos = BOARDMIN; pos < BOARDMAX; pos++)
    if (ON_BOARD(pos)) {
      if (!q->safe[pos] && q->white_strength[pos] > 0.0
	  && q->white_permeability[pos] != 1.0) {
	TRACE_INFLUENCE("  black block removed from %1m\n", pos);
	q->white_permeability[pos] = 1.0;
      }
      if (!q->safe[pos] && q->black_strength[pos] > 0.0
	  && q->black_permeability[pos] != 1.0) {
	TRACE_INFLUENCE("  white block removed from %1m\n", pos);
	q->black_permeability[pos] = 1.0;
      }
    }
}


/* This function goes through the list of intrusion sources, and adds
 * the intrusion as influence sources for color. The strength is
 * corrected so that each stone's intrusions sources can have total
 * strength of at most 60%/100% of the strength of the stone.
 * (100% is if q == &followup_influence, 60% otherwise).
 */
static void
add_marked_intrusions(struct influence_data *q, int color)
{
  int i;
  int j = 0;
  int source_pos;
  float strength_sum;
  float correction;
  float source_strength;
  float allowed_strength;

  gg_sort(q->intrusions, q->intrusion_counter, sizeof(q->intrusions[0]),
          compare_intrusions);

  /* Go through all intrusion sources. */
  for (i = 0; i < q->intrusion_counter; i = j) {
    strength_sum = 0.0;
    source_pos = q->intrusions[i].source_pos;
    /* "Anonymous" intrusios go in uncorrected. */
    if (source_pos == NO_MOVE) {
      add_influence_source(q->intrusions[i].strength_pos, color,
                           q->intrusions[j].strength,
                           q->intrusions[j].attenuation, q);
      TRACE_INFLUENCE("Adding %s intrusion at %1m, value %f\n",
	    (color == BLACK) ? "black" : "white",
	    q->intrusions[j].strength_pos, q->intrusions[j].strength);
      j = i+1;
      continue;
    }
    if (color == BLACK) {
      source_strength = q->black_strength[source_pos];
    }
    else {
      source_strength = q->white_strength[source_pos];
    }

    /* First loop: Determine correction factor. */
    for (j = i; (j < q->intrusion_counter)
                 && (q->intrusions[j].source_pos == source_pos); j++) {
      /* Of identical strength positions, only take strongest value. */
      if (j == i
          || q->intrusions[j].strength_pos != q->intrusions[j-1].strength_pos)
        strength_sum += q->intrusions[j].strength;
    }
    if (q == &followup_influence)
      allowed_strength = source_strength;
    else
      allowed_strength = 0.6 * source_strength;
    if (strength_sum > allowed_strength)
      correction = (allowed_strength / strength_sum);
    else
      correction = 1.0;

    /* Second loop: Add influence sources. */
    for (j = i; (j < q->intrusion_counter)
                 && (q->intrusions[j].source_pos == source_pos); j++) {
      /* Of identical strenght positions, only take strongest value. */
      if (j == i || q->intrusions[j].strength_pos
                    != q->intrusions[j-1].strength_pos) {
        add_influence_source(q->intrusions[j].strength_pos, color,
                             correction * q->intrusions[j].strength,
                             q->intrusions[j].attenuation, q);
        TRACE_INFLUENCE(
              "Adding %s intrusion for %1m at %1m, value %f (correction %f)\n",
              (color == BLACK) ? "black" : "white", source_pos,
              q->intrusions[j].strength_pos,
              correction * q->intrusions[j].strength, correction);
      }
    }
  }
}

/* Callback for the matched patterns in influence.db and barriers.db.
 * The pattern classes used here are:
 * A - Barrier pattern, where O plays first and X tries to block influence.
 * D - Barrier pattern, where O plays first and O tries to block influence.
 * B - Intrusion patterns, adding a low intensity influence source.
 * E - Enhance patterns, FIXME: document this one!
 * t - Non-territory patterns, marking vertices as not territory.
 * I - Invasion patterns, adding a low intensity influence source.
 * e - Escape bonus. Used together with I to increase the value substantially
 *     if escape influence is being computed.
 *
 * Classes A, D, and B are matched with color as O, and it is assumed
 * that O is in turn to move. Classes E and I are matched with either
 * color as O.
 */
static void
influence_callback(int anchor, int color, struct pattern *pattern, int ll,
		   void *data)
{
  int pos = AFFINE_TRANSFORM(pattern->move_offset, ll, anchor);
  int k;
  struct influence_data *q = data;
  
  /* Patterns marked T are only used in "territorial influence",
   * patterns marked W get ignored other(w)ise.
   */
  if (((pattern->class & CLASS_T) && !(q->is_territorial_influence))
      || ((pattern->class & CLASS_W) && q->is_territorial_influence))
    return;

  /* We also ignore enhancement patterns in territorial influence. */
  if ((pattern->class & CLASS_E)
      && q->is_territorial_influence)
    return;

  /* Don't use invasion (I) patterns when scoring. */
  if (doing_scoring && (pattern->class & CLASS_I))
    return;
  
  /* Loop through pattern elements to see if an A or D pattern
   * can possibly have any effect. If not we can skip evaluating
   * constraint and/or helper. */
  if (pattern->class & (CLASS_A | CLASS_D)) {
    int something_to_do = 0;
    for (k = 0; k < pattern->patlen; ++k) { /* match each point */
      int blocking_color;
      int ii;
      if (pattern->patn[k].att != ATT_comma
	  && (!q->is_territorial_influence || pattern->patn[k].att != ATT_not))
	break;  /* All commas are guaranteed to come first. */

      /* transform pattern real coordinate */
      ii = AFFINE_TRANSFORM(pattern->patn[k].offset, ll, anchor);

      /* Territorial connection, making a barrier for opponent influence. */
      if (pattern->class & CLASS_D)
	blocking_color = color;
      else
	blocking_color = OTHER_COLOR(color);
      if ((blocking_color == WHITE
	   && q->black_permeability[ii] != 0.0)
	  || (blocking_color == BLACK
	      && q->white_permeability[ii] != 0.0)) {
	something_to_do = 1;
	break;
      }
    }
    if (!something_to_do)
      return;
  }

  /* FIXME: Integrate the following loop into the previous one. */
  
  /* Require that all O stones in the pattern are tactically safe for
   * territorial connection patterns of type D and X stones for type
   * A. Both colors must be tactically safe for patterns of class B.
   * For patterns of class B, t, D and E, O stones must have non-zero
   * influence strength. Similarly for patterns of class A and t, X
   * stones must have non-zero influence strength.
   *
   * Patterns also having class s are an exception from this rule.
   */
  if ((pattern->class & (CLASS_D | CLASS_A | CLASS_B | CLASS_E | CLASS_t))
      && !(pattern->class & CLASS_s)) {
    for (k = 0; k < pattern->patlen; ++k) { /* match each point */
      if ((pattern->patn[k].att == ATT_O
	   && (pattern->class & (CLASS_D | CLASS_B | CLASS_E | CLASS_t)))
	  || (pattern->patn[k].att == ATT_X
	      && (pattern->class & (CLASS_A | CLASS_B | CLASS_t)))) {
	/* transform pattern real coordinate */
	int ii = AFFINE_TRANSFORM(pattern->patn[k].offset, ll, anchor);
	if (pattern->class & CLASS_E) {
	  if ((color == WHITE && q->white_strength[ii] == 0.0)
	      || (color == BLACK && q->black_strength[ii] == 0.0))
	    return; /* Match failed. */
	}
	/* FIXME: This test is probably not necessary any more. */
	else if (!(pattern->class & (CLASS_D | CLASS_B | CLASS_t))) {
	  if ((stackp == 0 && worm[ii].attack_codes[0] != 0)
	      || attack(ii, NULL) != 0)
	    return; /* Match failed */
	}
	/* One test left for class B and t. */
	if ((pattern->class & (CLASS_B | CLASS_t))
	    && pattern->patn[k].att == ATT_O) {
	  if ((color == WHITE && q->white_strength[ii] == 0.0)
	      || (color == BLACK && q->black_strength[ii] == 0.0))
	    return; /* Match failed. */
	}
	
	if ((pattern->class & (CLASS_A | CLASS_t))
	    && pattern->patn[k].att == ATT_X) {
	  if ((color == BLACK && q->white_strength[ii] == 0.0)
	      || (color == WHITE && q->black_strength[ii] == 0.0))
	    return; /* Match failed. */
	}
	
	if (pattern->class & CLASS_D) {
	  gg_assert(pattern->patn[k].att == ATT_O);
	  if ((color == WHITE && q->white_strength[ii] == 0.0)
	      || (color == BLACK && q->black_strength[ii] == 0.0))
	    return; /* Match failed. */
	}
      }
    }
  }


  /* If the pattern has a constraint, call the autohelper to see
   * if the pattern must be rejected.
   */
  if (pattern->autohelper_flag & HAVE_CONSTRAINT) {
    if (!pattern->autohelper(ll, pos, color, 0))
      return;
  }

  /* If the pattern has a helper, call it to see if the pattern must
   * be rejected.
   */
  if (pattern->helper) {
    if (!pattern->helper(pattern, ll, pos, color)) {
      TRACE_INFLUENCE(
	    "Influence pattern %s+%d rejected by helper at %1m\n",
	    pattern->name, ll, pos);
      return;
    }
  }

  TRACE_INFLUENCE("influence pattern '%s'+%d matched at %1m\n",
	pattern->name, ll, anchor);

  /* For t patterns, everything happens in the action. */
  if ((pattern->class & CLASS_t)
      && (pattern->autohelper_flag & HAVE_ACTION)) {
    pattern->autohelper(ll, pos, color, INFLUENCE_CALLBACK);
  }
  
  
  /* For I patterns, add a low intensity, both colored, influence
   * source at *.
   */
  if (pattern->class & CLASS_I) {
    int this_color = EMPTY;
    if (q->color_to_move == EMPTY || (pattern->class & CLASS_s))
      this_color = BLACK | WHITE;
    else if (q->color_to_move != color)
      this_color = q->color_to_move;

    /* Increase strength if we're computing escape influence. */
    if (q == &escape_influence && (pattern->class & CLASS_e))
      add_influence_source(pos, this_color,
			   20 * pattern->value, 1.5, q);
    else
      add_influence_source(pos, this_color, pattern->value, 1.5, q);

    TRACE_INFLUENCE(
	  "  low intensity influence source at %1m, strength %f, color %C\n",
	  pos, pattern->value, this_color);
    return;
  }
  
  /* For E patterns, add a new influence source of the same color and
   * pattern defined strength at *.
   */
  if (pattern->class & CLASS_E) {
    add_influence_source(pos, color,
			 pattern->value, DEFAULT_ATTENUATION, q);
    TRACE_INFLUENCE(
	  "  extra %C source at %1m, strength %f\n", color,
	  pos, pattern->value);
    return;
  }
  
  /* Loop through pattern elements and perform necessary actions
   * for A, D, B, and t patterns.
   */
  for (k = 0; k < pattern->patlen; ++k) { /* match each point */
    if (((pattern->class & (CLASS_D | CLASS_A))
	 && (pattern->patn[k].att == ATT_comma
	     || (pattern->patn[k].att == ATT_not
	         && q->is_territorial_influence)))
	|| ((pattern->class & CLASS_B)
	    && pattern->patn[k].att == ATT_not)) {
      /* transform pattern real coordinate */
      int ii = AFFINE_TRANSFORM(pattern->patn[k].offset, ll, anchor);

      /* Territorial connection, making a barrier for opponent influence. */
      if (pattern->class & (CLASS_A | CLASS_D)) {
	int blocking_color;
	if (pattern->class & CLASS_D)
	  blocking_color = color;
	else
	  blocking_color = OTHER_COLOR(color);
	TRACE_INFLUENCE("  barrier for %s influence at %1m\n",
	      color_to_string(OTHER_COLOR(blocking_color)), ii);
	if (pattern->patn[k].att == ATT_comma) {
	  if (blocking_color == WHITE)
	    q->black_permeability[ii] = 0.0;
	  else
	    q->white_permeability[ii] = 0.0;
	}
	/* Weak barrier at !-marked points. */
	else {
	  if (blocking_color == WHITE)
	    q->black_permeability[ii] *= 0.7;
	  else
	    q->white_permeability[ii] *= 0.7;
	  
	}
      }
      
      /* Low intensity influence source for the color in turn to move. */
      if (pattern->class & CLASS_B) {
        if (q->is_territorial_influence)
          enter_intrusion_source(anchor, ii, pattern->value,
	  		         TERR_DEFAULT_ATTENUATION, q);
        else
          add_influence_source(ii, color,
			       pattern->value, DEFAULT_ATTENUATION, q);
	TRACE_INFLUENCE("  intrusion at %1m\n", ii);
      }
    }
  }
}

/* Callback for matched barriers patterns in followup influence.
 * This adds an intrusion source for all B patterns in barriers.db for
 * the color that has made a move if all the following conditions are
 * fulfilled:
 * - the anchor ("Q") is adjacent (directly or diagonally) to a "saved stone"
 *  (this is ensured by matchpat before calling back here)
 * - at least one of the O stones in the pattern is a saved stone.
 * - the usual pattern constraint ("; oplay_attack_either(...)") is fulfilled
 * - the pattern action (typically ">return (!xplay_attack(...))") returns
 *   true if  called with parameter action = FOLLOWUP_INFLUENCE_CALLBACK.
 * "Saved stones" are: the move played + tactically rescued stones + stones
 *                     in a critcal dragon brought to life by this move
 */
static void
followup_influence_callback(int anchor, int color, struct pattern *pattern,
                            int ll, void *data)
{
  int k;
  int t;
  struct influence_data *q = data;
  UNUSED(color);
 
  /* We use only B  patterns in followup influence. */
  if (!(pattern->class & CLASS_B))
    return;

  t = AFFINE_TRANSFORM(pattern->move_offset, ll, anchor);
  /* If the pattern has a constraint, call the autohelper to see
   * if the pattern must be rejected.
   */
  if (pattern->autohelper_flag & HAVE_CONSTRAINT) {
    if (!pattern->autohelper(ll, t, color, 0))
      return;
  }

  /* If the pattern has a helper, call it to see if the pattern must
   * be rejected.
   */
  if (pattern->helper) {
    if (!pattern->helper(pattern, ll, t, color)) {
      TRACE_INFLUENCE(
            "Influence pattern %s+%d rejected by helper at %1m\n",
            pattern->name, ll, t);
      return;
    }
  }
 
 /* Actions in B patterns are used as followup specific constraints. */
 if ((pattern->autohelper_flag & HAVE_ACTION)
     && !pattern->autohelper(ll, t, color,
                             FOLLOWUP_INFLUENCE_CALLBACK))
    return;

  TRACE_INFLUENCE("influence pattern '%s'+%d matched at %1m\n",
	pattern->name, ll, anchor);

  for (k = 0; k < pattern->patlen; ++k)  /* match each point */
    if (pattern->patn[k].att == ATT_not) {
      /* transform pattern real coordinate */
      int ii = AFFINE_TRANSFORM(pattern->patn[k].offset, ll, anchor);

      /* Low intensity influence source for the color in turn to move. */
      enter_intrusion_source(anchor, ii, pattern->value,
			     TERR_DEFAULT_ATTENUATION, q);
      TRACE_INFLUENCE("  followup for %1m: intrusion at %1m\n",
            anchor, ii);
    }
}

/* Called from actions for t patterns. Marks (pos) as not being
 * territory for (color).
 */
void
influence_mark_non_territory(int pos, int color)
{
  TRACE_INFLUENCE("  non-territory for %C at %1m\n", color, pos);
  current_influence->non_territory[pos] |= color;
}

/* Match the patterns in influence.db and barriers.db in order to add:
 * - influence barriers,
 * - extra influence sources at possible invasion and intrusion points, and
 * - extra influence induced by strong positions.
 * Reduce permeability around each living stone.
 * Reset permeability to 1.0 at intrusion points.
 */
static void
find_influence_patterns(struct influence_data *q, int color)
{
  int ii;

  current_influence = q;
  matchpat(influence_callback, ANCHOR_COLOR, &influencepat_db, q, NULL);
  if (color != EMPTY)
    matchpat(influence_callback, color, &barrierspat_db, q, NULL);

  if (q->is_territorial_influence)
    add_marked_intrusions(q, color);

  /* When color == EMPTY, we introduce a weaker kind of barriers
   * manually instead of searching for patterns.
   */
  for (ii = BOARDMIN; ii < BOARDMAX; ii++)
    if (ON_BOARD(ii)
	&& q->safe[ii]) {
      int k;
      for (k = 0; k < 8; k++) {
	int d = delta[k];
	if (ON_BOARD(ii + d) && !q->safe[ii + d]) {
	  /* Reduce less diagonally. */
	  float reduction = (k < 4) ? 0.25 : 0.65;
	  if (board[ii] == BLACK)
	    q->white_permeability[ii + d] *= reduction;
	  else
	    q->black_permeability[ii + d] *= reduction;
	}
      }
    }

  reset_unblocked_blocks(q);
}


/* Do the real work of influence computation. This is called from
 * compute_influence and compute_escape_influence.
 */
static void
do_compute_influence(int color, const char safe_stones[BOARDMAX],
    		     const float strength[BOARDMAX], struct influence_data *q,
		     int move, const char *trace_message)
{
  int ii;
  int int_white_permeabilities[BOARDMAX];
  int int_black_permeabilities[BOARDMAX];
  int int_white_influence[BOARDMAX];
  int int_black_influence[BOARDMAX];
  int inv_diagonal_damping;

  init_influence(q, color, safe_stones, strength);

  modify_depth_values(stackp - 1);
  if (q != &escape_influence)
    find_influence_patterns(q, color);
  else
    find_influence_patterns(q, EMPTY);
  modify_depth_values(1 - stackp);

  if (q->is_territorial_influence)
    inv_diagonal_damping = GG_ONE / TERR_DIAGONAL_DAMPING;
  else
    inv_diagonal_damping = GG_ONE / DIAGONAL_DAMPING;
  memset(int_white_influence, 0, sizeof(int_white_influence));
  memset(int_black_influence, 0, sizeof(int_black_influence));
  for (ii = BOARDMIN; ii < BOARDMAX; ii++) {
    int_white_permeabilities[ii] = FLOAT2FIXED(q->white_permeability[ii]);
    int_black_permeabilities[ii] = FLOAT2FIXED(q->black_permeability[ii]);
  }

  
  for (ii = BOARDMIN; ii < BOARDMAX; ii++)
    if (ON_BOARD(ii)) {
      if (q->white_strength[ii] > 0.0)
	accumulate_influence(ii, FLOAT2FIXED(q->white_strength[ii]),
	                     GG_ONE / q->white_attenuation[ii],
			     inv_diagonal_damping,
			     int_white_permeabilities, q->safe,
			     int_white_influence);
      if (q->black_strength[ii] > 0.0)
	accumulate_influence(ii, FLOAT2FIXED(q->black_strength[ii]),
	                     GG_ONE / q->black_attenuation[ii],
			     inv_diagonal_damping,
			     int_black_permeabilities, q->safe,
			     int_black_influence);
    }

  for (ii = BOARDMIN; ii < BOARDMAX; ii++) {
    q->white_influence[ii] = FIXED2FLOAT(int_white_influence[ii]);
    q->black_influence[ii] = FIXED2FLOAT(int_black_influence[ii]);
  }

  value_territory(q);
  segment_influence(q);
  
  if ((move == NO_MOVE
       && (printmoyo & PRINTMOYO_INITIAL_INFLUENCE))
      || (debug_influence && move == debug_influence))
    print_influence(q, trace_message);
}


/* Compute the influence values for both colors.
 * 
 * The caller must
 * - set up the board[] state
 * - mark safe stones with INFLUENCE_SAFE_STONE, dead stones with 0
 * - mark stones newly saved by a move with INFLUENCE_SAVED_STONE
 *   (this is relevant if the influence_data *q is reused to compute
 *   a followup value for this move).
 *
 * Results will be stored in q.
 *
 * (move) has no effects except toggling debugging. Set it to -1
 * for no debug output at all (otherwise it will be controlled by
 * the -m command line option).
 *
 * It is assumed that color is in turn to move. (This affects the
 * barrier patterns (class A, D) and intrusions (class B)). Color
 */

void
compute_influence(int color, const char safe_stones[BOARDMAX],
    	          const float strength[BOARDMAX], struct influence_data *q,
		  int move, const char *trace_message)
{
  q->is_territorial_influence = 1;
  q->color_to_move = color;
  do_compute_influence(color, safe_stones, strength, q, move, trace_message);
}

/* Return the color of the territory at (pos). If it's territory for
 * neither color, EMPTY is returned.
 */
int
whose_territory(const struct influence_data *q, int pos)
{
  float bi = q->black_influence[pos];
  float wi = q->white_influence[pos];
  float terr = q->territory_value[pos];

  ASSERT_ON_BOARD1(pos);
  
  if (bi > 0.0 && wi == 0.0 && terr < -0.95)
    return BLACK;

  if (wi > 0.0 && bi == 0.0 && terr > 0.95)
    return WHITE;

  return EMPTY;
}

/* Return the color who has a moyo at (pos). If neither color has a
 * moyo there, EMPTY is returned. The definition of moyo in terms of the
 * influences is totally ad hoc.
 */
int
whose_moyo(const struct influence_data *q, int pos)
{
  float bi = q->black_influence[pos];
  float wi = q->white_influence[pos];

  int territory_color = whose_territory(q, pos);
  if (territory_color != EMPTY)
    return territory_color;
  
  if (bi > 7.0 * wi && bi > 5.0 && wi < 10.0)
    return BLACK;

  if (wi > 7.0 * bi && wi > 5.0 && bi < 10.0)
    return WHITE;
  
  return EMPTY;
}

/* Return the color who has a moyo at (pos). If neither color has a
 * moyo there, EMPTY is returned.
 * The definition of moyo in terms of the influences is totally ad
 * hoc.
 *
 * This differs from whose_moyo() in that it never counts moyo for
 * tactically unstable stones, which is useful when the surrounding
 * moyo size is estimated. It also doesn't count moyo where there is
 * an eye space inhibition.
 */
static int
whose_moyo_restricted(const struct influence_data *q, int pos)
{
  float bi = q->black_influence[pos];
  float wi = q->white_influence[pos];
  int color;

  int territory_color = whose_territory(q, pos);

  if (worm[pos].attack_codes[0] != 0
      && worm[pos].defense_codes[0] != 0)
    return EMPTY;
  
  /* default */
  if (territory_color != EMPTY)
    color = territory_color;
  else if (bi > 10.0 * wi && bi > 10.0 && wi < 10.0)
    color = BLACK;
  else if (wi > 10.0 * bi && wi > 10.0 && bi < 10.0)
    color = WHITE;
  else
    color = EMPTY;
  
  if (color == WHITE && (white_eye[pos].type & INHIBIT_CONNECTION))
    return EMPTY;
  
  if (color == BLACK && (black_eye[pos].type & INHIBIT_CONNECTION))
    return EMPTY;
  
  return color;
}


/* Return the color who has dominating influence ("area") at (pos).
 * If neither color dominates the influence there, EMPTY is returned.
 * The definition of area in terms of the influences is totally ad
 * hoc.
 */
int
whose_area(const struct influence_data *q, int pos)
{
  float bi = q->black_influence[pos];
  float wi = q->white_influence[pos];

  int moyo_color = whose_moyo(q, pos);
  if (moyo_color != EMPTY)
    return moyo_color;
  
  if (bi > 3.0 * wi && bi > 1.0 && wi < 40.0)
    return BLACK;

  if (wi > 3.0 * bi && wi > 1.0 && bi < 40.0)
    return WHITE;
  
  return EMPTY;
}

/* This curve determines how much influence is needed at least to claim
 * an intersection as territory, in dependence of the "center value".
 * (In the center, more effort is needed to get territory!)
 * The center value is at the moment defined as follows:
 * If d1, d2 are the distance to vertical and horizontal border, resp.,
 * with d1<d2, then
 * central = 3 * d1 + min(d2, 4)
 * So this is mainly a function of the distance to the border; the
 * distance to the second-nearest border gives a small correction of at
 * most 4. This distinguishes edge and corner positions.
 *
 * The values for intersections close to a corner or to the edge have
 * to be consistent such that standard corner enclosure etc. are
 * sufficient to claim territory. The center values are more arbitrary
 * suspect to tuning.
 */
static struct interpolation_data min_infl_for_territory =
  { 6,  0.0, 24.0, { 6.0, 15.0, 26.0, 36.0, 45.0, 50.0, 55.0 }};

/* Determines the territory correction factor in dependence of the ratio
 * ( influence of stronger color / min_infl_for_territory(intersection))
 */
static struct interpolation_data territory_correction = 
  { 5, (float) 0.0, 1.0, {0.0, 0.25, 0.45, 0.65, 0.85, 1.0}};

static void
value_territory(struct influence_data *q)
{
  int ii;
  int dist_i, dist_j;
  float central;
  float first_guess[BOARDMAX];
  float ratio;
  int k;

  /* First loop: guess territory directly from influence. */
  for (ii = BOARDMIN; ii < BOARDMAX; ii++)
    if (ON_BOARD(ii)) {
      first_guess[ii] = 0.0;

      if (!q->safe[ii]) {
        float diff = 0.0;
        if (q->white_influence[ii] + q->black_influence[ii] > 0)
          diff = (q->white_influence[ii] - q->black_influence[ii])
                /(q->white_influence[ii] + q->black_influence[ii]);
        first_guess[ii] = diff * diff * diff;

	/* If both side have small influence, we have to reduce this value.
	 * What we consider "small influence" depends on how central this
	 * intersection lies.
	 */
        dist_i = gg_min(I(ii), board_size - I(ii) - 1);
        dist_j = gg_min(J(ii), board_size - J(ii) - 1);
	if (dist_i > dist_j)
	  dist_i = gg_min(4, dist_i);
	else
	  dist_j = gg_min(4, dist_j);
	central = (float) 2 * gg_min(dist_i, dist_j) + dist_i + dist_j;
        ratio = gg_max(q->black_influence[ii], q->white_influence[ii])
                / gg_interpolate(&min_infl_for_territory, central);
        first_guess[ii] *= gg_interpolate(&territory_correction, ratio);

	/* Dead stone, upgrade to territory. Notice that this is not
         * the point for a prisoner, which is added later. Instead
         * this is to make sure that the vertex is not regarded as
         * moyo or area. Also notice that the non-territory
         * degradation below may over-rule this decision.
	 */
	if (board[ii] == BLACK)
	  first_guess[ii] = 1.0;
	else if (board[ii] == WHITE)
	  first_guess[ii] = -1.0;
      }
      q->territory_value[ii] = first_guess[ii];
    }

  /* Second loop: Correct according to neighbour vertices. Each territory
   * value is degraded to the minimum value of its neighbors (unless this
   * neighbor has reduced permeability for the opponent's influence).
   */
  for (ii = BOARDMIN; ii < BOARDMAX; ii++)
    if (ON_BOARD(ii)) {
      /* Do not overrule dead stone territory above.
       * FIXME: This does not do what it claims to do. Correcting it
       * seems to break some tests, though.
       */
      if (!q->safe[ii]) {
	/* Loop over all neighbors. */
        for (k = 0; k < 4; k++) {
          if (!ON_BOARD(ii + delta[k]))
            continue;
          if (q->territory_value[ii] > 0.0) {
            /* White territory. */
            if (!q->safe[ii + delta[k]]) {
	      float neighbor_val =
		q->black_permeability[ii + delta[k]]
		  * first_guess[ii + delta[k]]
		+ (1.0 - q->black_permeability[ii + delta[k]])
		  * first_guess[ii];
              q->territory_value[ii]
                = gg_max(0, gg_min(q->territory_value[ii], neighbor_val));
	    }
          }
          else {
            /* Black territory. */
            if (!q->safe[ii + delta[k]]) {
	      float neighbor_val =
		q->white_permeability[ii + delta[k]]
		  * first_guess[ii + delta[k]]
		+ (1 - q->white_permeability[ii + delta[k]])
		  * first_guess[ii];
              q->territory_value[ii]
                = gg_min(0, gg_max(q->territory_value[ii], neighbor_val));
	    }
          }
        }
      }
    }

  /* Third loop: Nonterritory patterns, points for prisoners. */
  for (ii = BOARDMIN; ii < BOARDMAX; ii++)
    if (ON_BOARD(ii)) {
      if (!q->safe[ii]) {
	/* If marked as non-territory for the color currently owning
         * it, reset the territory value.
	 */
	if (q->territory_value[ii] > 0.0
	    && (q->non_territory[ii] & WHITE))
	  q->territory_value[ii] = 0.0;

	if (q->territory_value[ii] < 0.0
	    && (q->non_territory[ii] & BLACK))
	  q->territory_value[ii] = 0.0;
	
	/* Dead stone, add one to the territory value. */
	if (board[ii] == BLACK)
	  q->territory_value[ii] += 1.0;
	else if (board[ii] == WHITE)
	  q->territory_value[ii] -= 1.0;
      }
    }
}


/* Segment the influence map into connected regions of territory,
 * moyo, or area. What to segment on is determined by the the function
 * pointer region_owner. The segmentation is performed for both
 * colors. The connected regions may include stones of the own color,
 * but only empty intersections (and dead opponent stones) count
 * toward the region size.
 */
static void
segment_region(struct influence_data *q, owner_function_ptr region_owner,
	       int type, int segmentation[BOARDMAX])
{
  int ii;
  static char marked[BOARDMAX];

  /* Reset the markings. */
  memset(marked, 0, sizeof(marked));

  for (ii = BOARDMIN; ii < BOARDMAX; ii++)
    if (ON_BOARD(ii)) {
      if ((!marked[ii]) && region_owner(q, ii) != EMPTY) {
	/* Found an unlabelled intersection. Use flood filling to find
         * the rest of the region.
	 */
	int size = 0;
	float terr_val = 0.0;
	int queue_start = 0;
	int queue_end = 1;
	int color = region_owner(q, ii);
	q->number_of_regions++;
	marked[ii] = 1;
	q->queue[0] = ii;
	while (queue_start < queue_end) {
	  int tt = q->queue[queue_start];
	  int k;
	  queue_start++;
	  if (!q->safe[tt] || board[tt] != color) {
	    size++;
	    if (q->is_territorial_influence)
	      terr_val += gg_abs(q->territory_value[tt]);
	  }
	  segmentation[tt] = q->number_of_regions;
	  for (k = 0; k < 4; k++) {
	    int d = delta[k];
	    if (ON_BOARD(tt + d)
		&& !marked[tt + d]
		&& region_owner(q, tt + d) == color) {
	      q->queue[queue_end] = tt + d;
	      queue_end++;
	      marked[tt + d] = 1;
	    }
	  }
	}
	if (color == WHITE)
	  q->region_type[q->number_of_regions] = WHITE_REGION | type;
	else
	  q->region_type[q->number_of_regions] = BLACK_REGION | type;
	q->region_size[q->number_of_regions] = size;
	q->region_territorial_value[q->number_of_regions] = terr_val;
	if (0)
	  gprintf("Region %d of type %d (color %s) at %1m. Size %d\n",
		  q->number_of_regions, q->region_type[q->number_of_regions],
		  color_to_string(color), ii, size);
      }
    }
}

/* Segment the influence map into connected regions of territory,
 * moyo, and area for both colors. The region numbers are stored in
 * the territory_segmentation, moyo_segmentation, and
 * area_segmentation arrays of the influence_data struct respectively.
 * All types of regions use the same numbering, with zero as a dummy
 * for nonregions. The region_type and region_size arrays hold
 * information about each region.
 */
static void
segment_influence(struct influence_data *q)
{
  int ii;
  q->number_of_regions = 0;
  q->region_type[0] = 0;
  q->region_size[0] = 0;
  for (ii = BOARDMIN; ii < BOARDMAX; ii++)
    if (ON_BOARD(ii)) {
      q->territory_segmentation[ii] = 0;
      q->moyo_segmentation[ii] = 0;
      q->area_segmentation[ii] = 0;
    }
  segment_region(q, whose_territory, IS_TERRITORY, q->territory_segmentation);
  segment_region(q, whose_moyo_restricted, IS_MOYO, q->moyo_segmentation);
  segment_region(q, whose_area,      IS_AREA,      q->area_segmentation);
}


/* Export the moyo segmentation. */
void
influence_get_moyo_segmentation(const struct influence_data *q,
    			        struct moyo_data *moyos)
{
  int ii;
  int min_moyo_id;
  int max_moyo_id;
  int i;

  min_moyo_id = MAX_REGIONS;
  max_moyo_id = 0;

  /* Find out range of region ids used by moyos. */
  for (ii = BOARDMIN; ii < BOARDMAX; ii++)
    if (ON_BOARD(ii)) {
      if (q->moyo_segmentation[ii] != 0) {
        min_moyo_id = gg_min(min_moyo_id, q->moyo_segmentation[ii]);
        max_moyo_id = gg_max(max_moyo_id, q->moyo_segmentation[ii]);
      }
    }
  moyos->number = max_moyo_id - min_moyo_id + 1;

  /* Export segmentation. */
  for (ii = BOARDMIN; ii < BOARDMAX; ii++)
    if (ON_BOARD(ii)) {
      if (q->moyo_segmentation[ii] != 0) {
        moyos->segmentation[ii]
	  = q->moyo_segmentation[ii] - min_moyo_id + 1;
      }
      else
        moyos->segmentation[ii] = 0;
    }
  
  /* Export size and owner info. */
  for (i = min_moyo_id; i <= max_moyo_id; i++) {
    moyos->size[i - min_moyo_id + 1] = q->region_size[i];
    moyos->territorial_value[i - min_moyo_id + 1]
        = q->region_territorial_value[i];
    if (q->region_type[i] & BLACK_REGION)
      moyos->owner[i - min_moyo_id + 1] = BLACK;
    else
      moyos->owner[i - min_moyo_id + 1] = WHITE;
  }
}

/* Another function to export a certain amount of moyo data. */
void
influence_get_moyo_data(const struct influence_data *q,
    		        int moyo_color[BOARDMAX],
			float territory_value[BOARDMAX])
{
  int ii;
  for (ii = BOARDMIN; ii < BOARDMAX; ii++)
    if (ON_BOARD(ii)) {
      moyo_color[ii] = whose_moyo_restricted(q, ii);
      territory_value[ii] = gg_abs(q->territory_value[ii]);
    }
}


/* Export the territory valuation at an intersection from initial_influence;
 * it is given from (color)'s point of view.
 */
float
influence_territory(const struct influence_data *q, int pos, int color)
{
  if (color == WHITE)
    return q->territory_value[pos];
  else
    return -q->territory_value[pos];
}


/* Redo the segmentation of the initial influence. */
void
resegment_initial_influence()
{
  segment_influence(&initial_black_influence);
  segment_influence(&initial_white_influence);
}

/* Compute a followup influence. It is assumed that the stones that
 * deserve a followup have been marked INFLUENCE_SAVED_STONE in
 * base->safe.
 */
void
compute_followup_influence(const struct influence_data *base,
    			   struct influence_data *q,
			   int move, const char *trace_message) 
{
  int ii;
  char goal[BOARDMAX];
  /* This is the color that will get a followup value. */
  int color = OTHER_COLOR(base->color_to_move);

  memcpy(q, base, sizeof(*q));
 
  /* We mark the saved stones and their neighbors in the goal array.
   */
  for (ii = BOARDMIN; ii < BOARDMAX; ii++)
    if (ON_BOARD(ii)) {
      if (q->safe[ii] == INFLUENCE_SAVED_STONE)
        goal[ii] = 1;
      else
	goal[ii] = 0;
    }

  q->intrusion_counter = 0;

  current_influence = q;
  /* Match B patterns for saved stones. */
  matchpat_goal_anchor(followup_influence_callback, color, &barrierspat_db, 
           	       q, goal, 1);
 
  /* Now add the intrusions. */
  add_marked_intrusions(q, color);

  reset_unblocked_blocks(q);
  
  /* Spread influence for new influence sources. */
  {
    int inv_diagonal_damping = GG_ONE / TERR_DIAGONAL_DAMPING;
    int int_permeabilities[BOARDMAX];
    int int_influence[BOARDMAX];

    /* Initialize integer copies of arrays. */
    memset(int_influence, 0, BOARDMAX * sizeof(int));
    for (ii = BOARDMIN; ii < BOARDMAX; ii++)
      if (ON_BOARD(ii)) {
	if (color == WHITE)
	  int_permeabilities[ii] = FLOAT2FIXED(q->white_permeability[ii]);
	else
	  int_permeabilities[ii] = FLOAT2FIXED(q->black_permeability[ii]);
      }

    /* Spread new influence. */
    for (ii = BOARDMIN; ii < BOARDMAX; ii++)
      if (ON_BOARD(ii)) {
	if (color == BLACK
	    && q->black_strength[ii] > base->black_strength[ii])
	  accumulate_influence(ii, FLOAT2FIXED(q->black_strength[ii]),
			       GG_ONE / q->black_attenuation[ii],
			       inv_diagonal_damping,
			       int_permeabilities, q->safe,
			       int_influence);
	else if (color == WHITE
		 && q->white_strength[ii] > base->white_strength[ii])
	  accumulate_influence(ii, FLOAT2FIXED(q->white_strength[ii]),
			       GG_ONE / q->white_attenuation[ii],
			       inv_diagonal_damping,
			       int_permeabilities, q->safe,
			       int_influence);
      }

    /* Add new influence. */
    for (ii = BOARDMIN; ii < BOARDMAX; ii++)
      if (ON_BOARD(ii)) {
	if (color == WHITE)
	  q->white_influence[ii] += FIXED2FLOAT(int_influence[ii]);
	else
	  q->black_influence[ii] += FIXED2FLOAT(int_influence[ii]);
      }
  }

  value_territory(q);

  if (debug_influence && debug_influence == move)
    print_influence(q, trace_message);
}


/* Compute influence based escape values and return them in the
 * escape_value array.  
 */

void
compute_escape_influence(int color, const char safe_stones[BOARDMAX],
    			 const float strength[BOARDMAX],
    			 char escape_value[BOARDMAX])
{
  int k;
  int ii;

  /* IMPORTANT: The caching relies on the fact that safe_stones[] and
   * strength[] will currently always be identical for identical board[]
   * states. Better check for these, too.
   */
  static int cached_board[BOARDMAX];
  static char escape_values[BOARDMAX][2];
  static int active_caches[2] = {0, 0};

  /* Encode the values of color and dragons_known into an integer
   * between 0 and 3.
   */
  int cache_number = (color == WHITE);

  int board_was_cached = 1;

  /* Notice that we compare the out of board markers as well, in case
   * the board size should have changed between calls.
   */
  for (ii = BOARDMIN; ii < BOARDMAX; ii++) {
    if (cached_board[ii] != board[ii]) {
      cached_board[ii] = board[ii];
      board_was_cached = 0;
    }
  }

  if (!board_was_cached)
    for (k = 0; k < 2; k++)
      active_caches[k] = 0;
  
  if (active_caches[cache_number]) {
    for (ii = BOARDMIN; ii < BOARDMAX; ii++)
      if (ON_BOARD(ii))
	escape_value[ii] = escape_values[ii][cache_number];
    
    return;
  }

  /* Use enhance pattern and higher attenuation for escape influence. */
  escape_influence.is_territorial_influence = 0;
  escape_influence.color_to_move = EMPTY;

  do_compute_influence(OTHER_COLOR(color), safe_stones, strength,
      		       &escape_influence, -1, NULL);
  
  for (ii = BOARDMIN; ii < BOARDMAX; ii++)
    if (ON_BOARD(ii)) {
      if (whose_moyo(&escape_influence, ii) == color)
       escape_value[ii] = 4;
      else if (whose_area(&escape_influence, ii) == color)
       escape_value[ii] = 2;
      else if (whose_area(&escape_influence, ii) == EMPTY)
       escape_value[ii] = 1;
      else
       escape_value[ii] = 0;
    }

  if (0 && (debug & DEBUG_ESCAPE) && verbose > 0) {
    print_numeric_influence(&escape_influence,
			    escape_influence.white_influence,
			    "%3.0f", 3, 1, 1);
    print_numeric_influence(&escape_influence,
			    escape_influence.black_influence,
			    "%3.0f", 3, 1, 1);
  }    

  /* Save the computed values in the cache. */
  for (ii = BOARDMIN; ii < BOARDMAX; ii++)
    if (ON_BOARD(ii))
      escape_values[ii][cache_number] = escape_value[ii];
  active_caches[cache_number] = 1;
}

/* We cache territory computations. This avoids unnecessary re-computations
 * when review_move_reasons is run a second time for the endgame patterns.
 */
int 
retrieve_delta_territory_cache(int pos, int color, float *move_value,
    			       float *followup_value)
{
  ASSERT_ON_BOARD1(pos);
  ASSERT1(IS_STONE(color), pos);

  if (territory_cache_position_number == position_number
      && territory_cache_color == color
      && delta_territory_cache[pos] != NOT_COMPUTED) {
    *move_value = delta_territory_cache[pos];
    *followup_value = followup_territory_cache[pos];
    if (0) 
      gprintf("%1m: retrieved territory value from cache: %f, %f\n", pos,
	      *move_value, *followup_value);
    return 1;
  }
  return 0;
}

void 
store_delta_territory_cache(int pos, int color,
			    float move_value, float followup_value)
{
  ASSERT_ON_BOARD1(pos);
  ASSERT1(IS_STONE(color), pos);

  if (territory_cache_position_number != position_number
      || territory_cache_color != color) {
    int ii;
    for (ii = BOARDMIN; ii < BOARDMAX; ii++)
      delta_territory_cache[ii] = NOT_COMPUTED;
    territory_cache_position_number = position_number;
    territory_cache_color = color;
    if (0)
      gprintf("Cleared delta territory cache.\n");
  }
  delta_territory_cache[pos] = move_value;
  followup_territory_cache[pos] = followup_value;
  if (0)
    gprintf("%1m: Stored delta territory cache: %f, %f\n", pos, move_value,
	    followup_value);
}

/* Compute the difference in territory between two influence data,
 * from the point of view of (color).
 * (move) is only passed for debugging output.
 */
float
influence_delta_territory(const struct influence_data *base,
    			  const struct influence_data *q, int color,
			  int move)
{
  int ii;
  float total_delta = 0.0;
  ASSERT_ON_BOARD1(move);
  ASSERT1(IS_STONE(color), move);

  for (ii = BOARDMIN; ii < BOARDMAX; ii++)
    if (ON_BOARD(ii)) {
      float new_value = q->territory_value[ii];
      float old_value = base->territory_value[ii];
      float this_delta = new_value - old_value;
      /* Negate values if we are black. */
      if (color == BLACK) {
	new_value = -new_value;
	old_value = -old_value;
	this_delta = -this_delta;
      }
      
      if (move != -1
	  && (this_delta > 0.02
              || -this_delta > 0.02))
	TRACE_TERRITORY(
	      "  %1m:   - %1m territory change %f (%f -> %f)\n",
	      move, ii, this_delta, old_value, new_value);
      total_delta += this_delta;
    }

  {
    /* Finally, captured stones: */
    float this_delta = q->captured - base->captured;
    if (color == BLACK)
      this_delta = -this_delta;
    if (move != -1
	&& this_delta != 0.0)
      TRACE_TERRITORY("  %1m:   - captured stones %f\n",
	    move, this_delta);
    total_delta += this_delta;
  }
  return total_delta;
}


/* Print the influence map when we have computed influence for the
 * move at (i, j).
 */
void
debug_influence_move(int i, int j)
{
  debug_influence = POS(i, j);
}


/* Print influence for debugging purposes, according to
 * printmoyo bitmap (controlled by -m command line option).
 */
void
print_influence(const struct influence_data *q, const char *info_string)
{
  if (printmoyo & PRINTMOYO_ATTENUATION) {
    /* Print the attenuation values. */
    fprintf(stderr, "white attenuation (%s):\n", info_string);
    print_numeric_influence(q, q->white_attenuation, "%3.2f", 3, 0, 0);
    fprintf(stderr, "black attenuation (%s):\n", info_string);
    print_numeric_influence(q, q->black_attenuation, "%3.2f", 3, 0, 0);
  }

  if (printmoyo & PRINTMOYO_PERMEABILITY) {
    /* Print the white permeability values. */
    fprintf(stderr, "white permeability:\n");
    print_numeric_influence(q, q->white_permeability, "%3.1f", 3, 0, 0);
    
    /* Print the black permeability values. */
    fprintf(stderr, "black permeability:\n");
    print_numeric_influence(q, q->black_permeability, "%3.1f", 3, 0, 0);
  }

  if (printmoyo & PRINTMOYO_STRENGTH) {
    /* Print the strength values. */
    fprintf(stderr, "white strength:\n");
    if (q->is_territorial_influence)
      print_numeric_influence(q, q->white_strength, "%5.1f", 5, 0, 0);
    else
      print_numeric_influence(q, q->white_strength, "%3.0f", 3, 0, 1);
    fprintf(stderr, "black strength:\n");
    if (q->is_territorial_influence)
      print_numeric_influence(q, q->black_strength, "%5.1f", 5, 0, 0);
    else
      print_numeric_influence(q, q->black_strength, "%3.0f", 3, 0, 1);
  }

  if (printmoyo & PRINTMOYO_NUMERIC_INFLUENCE) {
    /* Print the white influence values. */
    fprintf(stderr, "white influence (%s):\n", info_string);
    if (q->is_territorial_influence)
      print_numeric_influence(q, q->white_influence, "%5.1f", 5, 1, 0);
    else
      print_numeric_influence(q, q->white_influence, "%3.0f", 3, 1, 1);
    /* Print the black influence values. */
    fprintf(stderr, "black influence (%s):\n", info_string);
    if (q->is_territorial_influence)
      print_numeric_influence(q, q->black_influence, "%5.1f", 5, 1, 0);
    else
      print_numeric_influence(q, q->black_influence, "%3.0f", 3, 1, 1);
  }

  if (printmoyo & PRINTMOYO_PRINT_INFLUENCE) {
    fprintf(stderr, "influence regions (%s):\n", info_string);
    print_influence_areas(q);
  }
  if (printmoyo & PRINTMOYO_VALUE_TERRITORY) {
    fprintf(stderr, "territory (%s)", info_string);
    print_numeric_influence(q, q->territory_value, "%5.2f", 5, 1, 0);
  }
}



/*
 * Print numeric influence values.
 */
static void
print_numeric_influence(const struct influence_data *q,
			const float values[BOARDMAX],
			const char *format, int width,
			int draw_stones, int mark_epsilon)
{
  int i, j;
  char ch;
  char format_stone[20];

  memset(format_stone, ' ', 20);
  format_stone[(width + 1) / 2] = '%';
  format_stone[(width + 3) / 2] = 'c';
  format_stone[width + 2] = 0;

  fprintf(stderr, "   ");
  for (i = 0, ch = 'A'; i < board_size; i++, ch++) {
    if (ch == 'I')
      ch++;
    fprintf(stderr, format_stone, ch);
  }
  fprintf(stderr, "\n");

  for (i = 0; i < board_size; i++) {
    int ii = board_size - i;
    fprintf(stderr, "%2d ", ii);
    for (j = 0; j < board_size; j++) {
      int ii = POS(i, j);
      if (draw_stones && q->safe[ii]) {
        if (board[ii] == WHITE)
	  fprintf(stderr, format_stone, 'O');
	else 
	  fprintf(stderr, format_stone, 'X');
      }
      else {
	if (mark_epsilon && values[ii] > 0.0 && values[ii] < 1.0)
	  fprintf(stderr, "eps");
	else
	  fprintf(stderr, format, values[ii]);
	fprintf(stderr, " ");
      }
    }
    fprintf(stderr, "%2d\n", ii);
  }

  fprintf(stderr, "   ");
  for (i = 0, ch = 'A'; i < board_size; i++, ch++) {
    if (ch == 'I')
      ch++;
    fprintf(stderr, format_stone, ch);
  }
  fprintf(stderr, "\n");
}

/* Draw colored board illustrating territory, moyo, and area. */
static void
print_influence_areas(const struct influence_data *q)
{
  int ii;
  start_draw_board();
  for (ii = BOARDMIN; ii < BOARDMAX; ii++)
    if (ON_BOARD(ii)) {
      int c = EMPTY;
      int color = GG_COLOR_BLACK;
      if (q->safe[ii]) {
	color = GG_COLOR_BLACK;
        if (board[ii] == WHITE)
	  c = 'O';
        else
	  c = 'X';
      }
      else if (whose_territory(q, ii) == WHITE) {
	c = 'o';
	color = GG_COLOR_CYAN;
      }
      else if (whose_territory(q, ii) == BLACK) {
	c = 'x';
	color = GG_COLOR_CYAN;
      }
      else if (whose_moyo(q, ii) == WHITE) {
	c = 'o';
	color = GG_COLOR_YELLOW;
      }
      else if (whose_moyo(q, ii) == BLACK) {
	c = 'x';
	color = GG_COLOR_YELLOW;
      }
      else if (whose_area(q, ii) == WHITE) {
	c = 'o';
	color = GG_COLOR_RED;
      }
      else if (whose_area(q, ii) == BLACK) {
	c = 'x';
	color = GG_COLOR_RED;
      }
      draw_color_char(I(ii), J(ii), c, color);
    }
  end_draw_board();
}


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
