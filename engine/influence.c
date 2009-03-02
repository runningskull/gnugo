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

#include "liberty.h"
#include "influence.h"
#include "patterns.h"
#include "gg_utils.h"

static void add_influence_source(int pos, int color, float strength,
                                 float attenuation,
                                 struct influence_data *q);
static void print_influence(const struct influence_data *q,
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
static void add_marked_intrusions(struct influence_data *q);
 

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


/* Thresholds values used in the whose_moyo() functions */
static struct moyo_determination_data moyo_data;
static struct moyo_determination_data moyo_restricted_data;
 
/* Thresholds value used in the whose_territory() function */
static float territory_determination_value; 
 


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





/* If set, print influence map when computing this move. Purely for
 * debugging.
 */
static int debug_influence = NO_MOVE;

/* Assigns an id to all influence computations for reference in the
 * delta territory cache.
 */
static int influence_id = 0;

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
 *  arg is i + arg_di ; arg_j is j + arg_dj
 *  arg_d is 1 for diagonal movement
 *
 */


#define code1(arg_di, arg_dj, arg, arg_d) do { \
      if (!q->safe[arg] \
	  && ((arg_di)*(delta_i) + (arg_dj)*(delta_j) > 0 \
	      || queue_start == 1)) { \
	float contribution; \
	float permeability = permeability_array[ii]; \
	if (arg_d) { \
	  permeability *= gg_max(permeability_array[ii + DELTA(arg_di, 0)], \
			         permeability_array[ii + DELTA(0, arg_dj)]); \
	  if (permeability == 0.0) \
	    continue; \
	} \
	contribution = current_strength * permeability; \
	if (queue_start != 1) { \
	  int a = (arg_di)*(delta_i) + (arg_dj)*(delta_j); \
	  contribution *= (a*a) * b; /* contribution *= cos(phi) */ \
	} \
	if (contribution <= INFLUENCE_CUTOFF) \
	  continue; \
	if (working[arg] == 0.0) { \
	  q->queue[queue_end] = (arg); \
	  queue_end++; \
	} \
	working[arg] += contribution; \
      } } while (0) 
#endif


static void
accumulate_influence(struct influence_data *q, int pos, int color)
{
  int ii;
  int m = I(pos);
  int n = J(pos);
  int k;
#if !EXPLICIT_LOOP_UNROLLING
  int d;
#endif
  float b;
  float inv_attenuation;
  float inv_diagonal_damping;
  float *permeability_array;

  /* Clear the queue. Entry 0 is implicitly (m, n). */
  int queue_start = 0;
  int queue_end = 1;

  static float working[BOARDMAX];
  static int working_area_initialized = 0;

  if (!working_area_initialized) {
    for (ii = 0; ii < BOARDMAX; ii++)
      working[ii] = 0.0;
    working_area_initialized = 1;
  }

  if (0)
    gprintf("Accumulating influence for %s at %m\n",
	    color_to_string(color), m, n);

  /* Attenuation only depends on the influence origin. */
  if (color == WHITE)
    inv_attenuation = 1.0 / q->white_attenuation[pos];
  else
    inv_attenuation = 1.0 / q->black_attenuation[pos];

  if (q->is_territorial_influence)
    inv_diagonal_damping = 1.0 / TERR_DIAGONAL_DAMPING;
  else
    inv_diagonal_damping = 1.0 / DIAGONAL_DAMPING;

  if (color == WHITE)
    permeability_array = q->white_permeability;
  else
    permeability_array = q->black_permeability;

  /* We put the original source into slot 0.  */
  q->queue[0] = pos;
    
  if (color == WHITE)
    working[pos] = q->white_strength[pos];
  else
    working[pos] = q->black_strength[pos];


  /* Spread influence until the stack is empty. */
  while (queue_start < queue_end) {
    float current_strength;
    int delta_i, delta_j;

    ii = q->queue[queue_start];
    delta_i = I(ii) - m;
    delta_j = J(ii) - n;
    queue_start++;
    if (permeability_array[ii] == 0.0)
      continue;
    if (0)
      gprintf("Picked %1m from queue. w=%f start=%d end=%d\n",
	      ii, working[ii], queue_start, queue_end);
    if (queue_start == 1)
      b = 1.0;
    else
      b = 1.0 / ((delta_i)*(delta_i) + (delta_j)*(delta_j));

    current_strength = working[ii] * inv_attenuation;

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
      	  && (!q->safe[ii + d_ii])
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
	  q->queue[queue_end] = ii + d_ii;
	  queue_end++;
	}
	working[ii + d_ii] += contribution;
      }
    }
#else
    if (ON_BOARD(ii + delta[0]))
      code1(deltai[0], deltaj[0], ii + delta[0], 0);
    if (ON_BOARD(ii + delta[1]))
      code1(deltai[1], deltaj[1], ii + delta[1], 0);
    if (ON_BOARD(ii + delta[2]))
      code1(deltai[2], deltaj[2], ii + delta[2], 0);
    if (ON_BOARD(ii + delta[3]))
      code1(deltai[3], deltaj[3], ii + delta[3], 0);

    /* Update factors for diagonal movement. */
    b *= 0.5;
    current_strength *= inv_diagonal_damping;

    if (ON_BOARD(ii + delta[4]))
      code1(deltai[4], deltaj[4], ii + delta[4], 1);
    if (ON_BOARD(ii + delta[5]))
      code1(deltai[5], deltaj[5], ii + delta[5], 1);
    if (ON_BOARD(ii + delta[6]))
      code1(deltai[6], deltaj[6], ii + delta[6], 1);
    if (ON_BOARD(ii + delta[7]))
      code1(deltai[7], deltaj[7], ii + delta[7], 1);
#endif
  }
  
  /* Add the values in the working area to the accumulated influence
   * and simultaneously reset the working area. We know that all
   * influenced points were stored in the queue, so we just traverse
   * it.
   */
  for (k = 0; k < queue_end; k++) {
    ii = q->queue[k];

    if (color == WHITE) {
      if (working[ii] > 1.01 * INFLUENCE_CUTOFF
	  || q->white_influence[ii] == 0.0)
	q->white_influence[ii] += working[ii];
    }
    else {
      if (working[ii] > 1.01 * INFLUENCE_CUTOFF
	  || q->black_influence[ii] == 0.0)
	q->black_influence[ii] += working[ii];
    }
    
    working[ii] = 0.0;
  }
}



/* Initialize the influence_data structure.  */

static void
init_influence(struct influence_data *q,
	       const signed char safe_stones[BOARDMAX], 
	       const float strength[BOARDMAX])
{
  int ii;
  float attenuation;
  
  /* Initialisation of some global positional values, based on 
   * game stage. 
   */
  if (cosmic_gnugo) {
    float t;
    if ((board_size != 19) || (movenum <= 2) || ((movenum / 2) % 2))
      cosmic_importance = 0.0;
    else {
      cosmic_importance = 1.0 - (movenum / 150.0)*(movenum / 150.0); 
      cosmic_importance = gg_max(0.0, cosmic_importance);
    }

    t = cosmic_importance;
    
    moyo_data.influence_balance     = t * 15.0  +  (1.0-t) * 5.0;  
    moyo_data.my_influence_minimum  = t * 5.0   +  (1.0-t) * 5.0;
    moyo_data.opp_influence_maximum = t * 30.0  +  (1.0-t) * 30.0;
    
    /* we use the same values for moyo and moyo_restricted */
    moyo_restricted_data = moyo_data;

    territory_determination_value   = t * 0.95 +  (1.0-t) * 0.95; 
      
    min_infl_for_territory.values[0] = t * 6.0   +  (1.0-t) * 10.0;
    min_infl_for_territory.values[1] = t * 10.0  +  (1.0-t) * 15.0;
    min_infl_for_territory.values[2] = t * 20.0  +  (1.0-t) * 15.0;
    min_infl_for_territory.values[3] = t * 20.0  +  (1.0-t) * 20.0;
    min_infl_for_territory.values[4] = t * 20.0  +  (1.0-t) * 20.0;
    min_infl_for_territory.values[5] = t * 15.0  +  (1.0-t) * 15.0;
    min_infl_for_territory.values[6] = t * 10.0  +  (1.0-t) * 15.0;   
  }
  else {  
    /* non-cosmic values */
    cosmic_importance = 0.0;
  
    moyo_data.influence_balance     = 7.0;
    moyo_data.my_influence_minimum  = 5.0;
    moyo_data.opp_influence_maximum = 10.0;
    
    moyo_restricted_data.influence_balance     = 10.0;
    moyo_restricted_data.my_influence_minimum  = 10.0;
    moyo_restricted_data.opp_influence_maximum = 10.0;
    
    territory_determination_value = 0.95;
    
    min_infl_for_territory.values[0] = 6.0;
    min_infl_for_territory.values[1] = 15.0;
    min_infl_for_territory.values[2] = 26.0;
    min_infl_for_territory.values[3] = 36.0;
    min_infl_for_territory.values[4] = 45.0;
    min_infl_for_territory.values[5] = 50.0;
    min_infl_for_territory.values[6] = 55.0; 
  }
  
  if (q->is_territorial_influence)
    attenuation = TERR_DEFAULT_ATTENUATION;
  else
    attenuation = 2 * DEFAULT_ATTENUATION;
  
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

/* Adds an intrusion as an entry in the list q->intrusions.  */
static void
enter_intrusion_source(int source_pos, int strength_pos,
                       float strength, float attenuation,
                       struct influence_data *q)
{
  if (q->intrusion_counter >= MAX_INTRUSIONS) {
    DEBUG(DEBUG_INFLUENCE, "intrusion list exhausted\n");
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
  if (intr1->source_pos - intr2->source_pos != 0)
    return (intr1->source_pos - intr2->source_pos);
  else if (intr1->strength_pos - intr2->strength_pos != 0)
    return (intr1->strength_pos - intr2->strength_pos);
  else if (intr1->strength > intr2->strength)
    return 1;
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
	DEBUG(DEBUG_INFLUENCE, "  black block removed from %1m\n", pos);
	q->white_permeability[pos] = 1.0;
      }
      if (!q->safe[pos] && q->black_strength[pos] > 0.0
	  && q->black_permeability[pos] != 1.0) {
	DEBUG(DEBUG_INFLUENCE, "  white block removed from %1m\n", pos);
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
add_marked_intrusions(struct influence_data *q)
{
  int i;
  int j = 0;
  int source_pos;
  float strength_sum;
  float correction;
  float source_strength;
  float allowed_strength;
  int color = q->color_to_move;

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
      DEBUG(DEBUG_INFLUENCE, "Adding %s intrusion at %1m, value %f\n",
	    (color == BLACK) ? "black" : "white",
	    q->intrusions[j].strength_pos, q->intrusions[j].strength);
      j = i+1;
      continue;
    }
    if (color == BLACK)
      source_strength = q->black_strength[source_pos];
    else
      source_strength = q->white_strength[source_pos];

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
        DEBUG(DEBUG_INFLUENCE,
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
  
  /* We also ignore enhancement patterns in territorial influence. */
  if ((pattern->class & CLASS_E) && q->is_territorial_influence)
    return;

  /* Don't use invasion (I) patterns when scoring. */
  if (doing_scoring && (pattern->class & CLASS_I))
    return;
  
  /* Loop through pattern elements to see if an A or D pattern
   * can possibly have any effect. If not we can skip evaluating
   * constraint and/or helper.
   */
  if (pattern->class & (CLASS_A | CLASS_D)) {
    int something_to_do = 0;
    gg_assert(q->is_territorial_influence);
    for (k = 0; k < pattern->patlen; ++k) { /* match each point */
      int blocking_color;
      int ii;
      /* The order of elements is: All commas, all "!", then other. */
      if (pattern->patn[k].att != ATT_comma
	  && pattern->patn[k].att != ATT_not)
	break;  

      ii = AFFINE_TRANSFORM(pattern->patn[k].offset, ll, anchor);

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

  /* Require that all O stones in the pattern have non-zero influence
   * strength for patterns of type D, E, B, t, and all X stones have
   * non-zero strength for patterns of type A and t.
   *
   * Patterns also having class s are an exception from this rule.
   */
  if ((pattern->class & (CLASS_D | CLASS_A | CLASS_B | CLASS_E | CLASS_t))
      && !(pattern->class & CLASS_s)) {
    for (k = 0; k < pattern->patlen; ++k) { /* match each point */
      int ii = AFFINE_TRANSFORM(pattern->patn[k].offset, ll, anchor);
      if (pattern->patn[k].att == ATT_O) {
	if ((pattern->class & (CLASS_B | CLASS_t | CLASS_E | CLASS_D))
	    && ((color == WHITE && q->white_strength[ii] == 0.0)
	        || (color == BLACK && q->black_strength[ii] == 0.0)))
	  return;
      }
      else if (pattern->patn[k].att == ATT_X) {
	if ((pattern->class & (CLASS_A | CLASS_t))
	    && ((color == BLACK && q->white_strength[ii] == 0.0)
	        || (color == WHITE && q->black_strength[ii] == 0.0)))
	  return; /* Match failed. */
      }
    }
  }

  /* If the pattern has a constraint, call the autohelper to see
   * if the pattern must be rejected.
   */
  if ((pattern->autohelper_flag & HAVE_CONSTRAINT)
      && !pattern->autohelper(ll, pos, color, 0))
    return;

  DEBUG(DEBUG_INFLUENCE, "influence pattern '%s'+%d matched at %1m\n",
	pattern->name, ll, anchor);

  /* For t patterns, everything happens in the action. */
  if ((pattern->class & CLASS_t)
      && (pattern->autohelper_flag & HAVE_ACTION)) {
    pattern->autohelper(ll, pos, color, INFLUENCE_CALLBACK);
    return;
  }
  
  /* For I patterns, add a low intensity, both colored, influence
   * source at *.
   */
  if (pattern->class & CLASS_I) {
    int this_color = EMPTY;
    float strength;
    float attenuation;

    if (q->color_to_move == EMPTY || (pattern->class & CLASS_s))
      this_color = BLACK | WHITE;
    else if (q->color_to_move != color)
      this_color = q->color_to_move;
      
    if (cosmic_gnugo) {
      float t = 0.15 + (1.0 - cosmic_importance);
      t = gg_min(1.0, t);
      t = gg_max(0.0, t);
      strength = t * pattern->value;  
      attenuation = 1.6;
    }
    else {
      strength = pattern->value;  
      attenuation = 1.5;
    }

    /* Increase strength if we're computing escape influence. */
    if (!q->is_territorial_influence && (pattern->class & CLASS_e))
      add_influence_source(pos, this_color, 20 * strength, attenuation, q);
    else
      add_influence_source(pos, this_color, strength, attenuation, q);

    DEBUG(DEBUG_INFLUENCE,
	  "  low intensity influence source at %1m, strength %f, color %C\n",
	  pos, strength, this_color);
    return;
  }
    
  /* For E patterns, add a new influence source of the same color and
   * pattern defined strength at *.
   */
  if (pattern->class & CLASS_E) {
    add_influence_source(pos, color, pattern->value, DEFAULT_ATTENUATION, q);
    DEBUG(DEBUG_INFLUENCE,
	  "  extra %C source at %1m, strength %f\n", color,
	  pos, pattern->value);
    return;
  }

  /* For B patterns add intrusions sources at "!" points. */
  if (pattern->class & CLASS_B) {
    float strength;
    if (cosmic_gnugo) {
      float t = 0.15 + (1.0 - cosmic_importance);
      t = gg_min(1.0, t);
      t = gg_max(0.0, t);
      strength = t * pattern->value;
    }
    else 
      strength = pattern->value;
    
    for (k = 0; k < pattern->patlen; ++k)  /* match each point */
      if (pattern->patn[k].att == ATT_not) {
	/* transform pattern real coordinate */
	int ii = AFFINE_TRANSFORM(pattern->patn[k].offset, ll, anchor);

	/* Low intensity influence source for the color in turn to move. */  
	if (q->is_territorial_influence)
	  enter_intrusion_source(anchor, ii, strength, 
				 TERR_DEFAULT_ATTENUATION, q);
	else
	  add_influence_source(ii, color, strength, DEFAULT_ATTENUATION, q); 
	DEBUG(DEBUG_INFLUENCE, "  intrusion at %1m\n", ii);
      }
    return;
  }
  

  gg_assert(pattern->class & (CLASS_D | CLASS_A));
  /* For A, D patterns, add blocks for all "," or "!" points.  */
  for (k = 0; k < pattern->patlen; ++k) { /* match each point */
    if (pattern->patn[k].att == ATT_comma
	|| pattern->patn[k].att == ATT_not) {
      /* transform pattern real coordinate */
      int ii = AFFINE_TRANSFORM(pattern->patn[k].offset, ll, anchor);
      int blocking_color;
      if (pattern->class & CLASS_D)
	blocking_color = color;
      else
	blocking_color = OTHER_COLOR(color);
      DEBUG(DEBUG_INFLUENCE, "  barrier for %s influence at %1m\n",
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
  if (pattern->autohelper_flag & HAVE_CONSTRAINT
      && !pattern->autohelper(ll, t, color, 0))
    return;

  /* Actions in B patterns are used as followup specific constraints. */
  if ((pattern->autohelper_flag & HAVE_ACTION)
      && !pattern->autohelper(ll, t, color, FOLLOWUP_INFLUENCE_CALLBACK))
    return;

  DEBUG(DEBUG_INFLUENCE, "influence pattern '%s'+%d matched at %1m\n",
	pattern->name, ll, anchor);

  for (k = 0; k < pattern->patlen; ++k)  /* match each point */
    if (pattern->patn[k].att == ATT_not) {
      /* transform pattern real coordinate */
      int ii = AFFINE_TRANSFORM(pattern->patn[k].offset, ll, anchor);

      /* Low intensity influence source for the color in turn to move. */
      enter_intrusion_source(anchor, ii, pattern->value,
			     TERR_DEFAULT_ATTENUATION, q);
      DEBUG(DEBUG_INFLUENCE, "  followup for %1m: intrusion at %1m\n",
            anchor, ii);
    }
}

/* Called from actions for t patterns. Marks (pos) as not being
 * territory for (color).
 */
void
influence_mark_non_territory(int pos, int color)
{
  DEBUG(DEBUG_INFLUENCE, "  non-territory for %C at %1m\n", color, pos);
  current_influence->non_territory[pos] |= color;
}

/* Erases all territory for color at (pos), and all directly neighboring
 * fields.
 */
void
influence_erase_territory(struct influence_data *q, int pos, int color)
{
  int k;
  ASSERT1((color == WHITE && q->territory_value[pos] >= 0.0)
          || (color == BLACK && q->territory_value[pos] <= 0.0), pos);

  current_influence = q;

  q->territory_value[pos] = 0.0;
  influence_mark_non_territory(pos, color);
  for (k = 0; k < 4; k++) {
    if (ON_BOARD(pos + delta[k])) {
      q->territory_value[pos + delta[k]] = 0.0;
      influence_mark_non_territory(pos + delta[k], color);
    }
  }
}

/* Match the patterns in influence.db and barriers.db in order to add:
 * - influence barriers,
 * - extra influence sources at possible invasion and intrusion points, and
 * - extra influence induced by strong positions.
 * Reduce permeability around each living stone.
 * Reset permeability to 1.0 at intrusion points.
 */
static void
find_influence_patterns(struct influence_data *q)
{
  int ii;

  current_influence = q;
  matchpat(influence_callback, ANCHOR_COLOR, &influencepat_db, q, NULL);
  if (q->color_to_move != EMPTY)
    matchpat(influence_callback, q->color_to_move, &barrierspat_db, q, NULL);

  if (q->is_territorial_influence)
    add_marked_intrusions(q);

  /* Additionally, we introduce a weaker kind of barriers around living
   * stones.
   */
  for (ii = BOARDMIN; ii < BOARDMAX; ii++)
    if (ON_BOARD(ii) && !q->safe[ii]) {
      int k;
      float black_reduction = 1.0;
      float white_reduction = 1.0;
      for (k = 0; k < 8; k++) {
	int d = delta[k];
	if (IS_STONE(board[ii + d]) && q->safe[ii + d]) {
	  /* Reduce less diagonally. */
	  float reduction = (k < 4) ? 0.25 : 0.65;
	  if (board[ii + d] == BLACK)
	    white_reduction *= reduction;
	  else
	    black_reduction *= reduction;
	}
	else if (IS_STONE(board[ii + d]) && !q->safe[ii + d]) {
	  if (board[ii + d] == BLACK)
	    white_reduction = -100.0;
	  else
	    black_reduction = -100.0;
	}
      }
      if (black_reduction > 0.0)
	q->black_permeability[ii] *= black_reduction;
      if (white_reduction > 0.0)
	q->white_permeability[ii] *= white_reduction;
    }

  reset_unblocked_blocks(q);
}

/* This function checks whether we have two or more adjacent blocks for
 * influence of color next to pos. If yes, it returns the position of the
 * least valuable blocks; otherwise, it returns NO_MOVE.
 */
static int
check_double_block(int color, int pos, const struct influence_data *q)
{
  int k;
  int block_neighbors = 0;
  const float *permeability = ((color == BLACK) ? q->black_permeability :
						  q->white_permeability);

  /* Count neighboring blocks. */
  for (k = 0; k < 4; k++)
    if (board[pos + delta[k]] == EMPTY && permeability[pos + delta[k]] == 0.0)
      block_neighbors++;

  if (block_neighbors >= 2) {
    /* Search for least valuable block. */
    float smallest_value = 4.0 * MAX_BOARD * MAX_BOARD;
    int smallest_block = NO_MOVE;
    /* We count opponent's territory as positive. */
    float sign = ((color == WHITE) ? -1.0 : 1.0);
    for (k = 0; k < 4; k++) {
      int neighbor = pos + delta[k];
      if (board[neighbor] == EMPTY && permeability[neighbor] == 0.0) {
	/* Value is sum of opponents territory at this and all 4 neighboring
	 * intersections.
	 */
	float this_value = sign * q->territory_value[neighbor];
	int j;
	for (j = 0; j < 4; j++)
	  if (ON_BOARD(neighbor + delta[j]))
	    this_value += sign * q->territory_value[neighbor + delta[j]];
	/* We use an artifical tie breaker to avoid possible platform
	 * dependency.
	 */
	if (this_value + 0.0005 < smallest_value) {
	  smallest_block = neighbor;
	  smallest_value = this_value;
	}
      }
    }
    ASSERT1(ON_BOARD1(smallest_block), pos);
    return smallest_block;
  }
  return NO_MOVE;
}

#define MAX_DOUBLE_BLOCKS 20 


/* This function checks for the situation where an influence source for
 * the color to move is direclty neighbored by 2 or more influence blocks.
 * It then removes the least valuable of these blocks, and re-runs the
 * influence accumulation for this position.
 *
 * See endgame:840 for an example where this is essential.
 */
static void
remove_double_blocks(struct influence_data *q,
    		     const signed char inhibited_sources[BOARDMAX])
{
  int ii;
  float *strength = ((q->color_to_move == WHITE) ? q->white_strength :
      						   q->black_strength);
  int double_blocks[MAX_DOUBLE_BLOCKS];
  int num_blocks = 0;
  for (ii = BOARDMIN; ii < BOARDMAX; ii++)
    if (board[ii] == EMPTY
	&& !(inhibited_sources && inhibited_sources[ii])
	&& strength[ii] > 0.0) {
      double_blocks[num_blocks] = check_double_block(q->color_to_move, ii, q);
      if (double_blocks[num_blocks] != NO_MOVE) {
	num_blocks++;
	if (num_blocks == MAX_DOUBLE_BLOCKS)
	  break;
      }
    }
  {
    int k;
    float *permeability = ((q->color_to_move == BLACK)
			   ? q->black_permeability : q->white_permeability);
    for (k = 0; k < num_blocks; k++) {
      DEBUG(DEBUG_INFLUENCE, "Removing block for %s at %1m.\n",
	    color_to_string(q->color_to_move), double_blocks[k]);
      permeability[double_blocks[k]] = 1.0;
      accumulate_influence(q, double_blocks[k], q->color_to_move);
    }
  }
}


/* Do the real work of influence computation. This is called from
 * compute_influence and compute_escape_influence.
 *
 * q->is_territorial_influence and q->color_to_move must be set by the caller.
 */
static void
do_compute_influence(const signed char safe_stones[BOARDMAX],
		     const signed char inhibited_sources[BOARDMAX],
    		     const float strength[BOARDMAX], struct influence_data *q,
		     int move, const char *trace_message)
{
  int ii;
  init_influence(q, safe_stones, strength);

  modify_depth_values(stackp - 1);
  find_influence_patterns(q);
  modify_depth_values(1 - stackp);
  
  for (ii = BOARDMIN; ii < BOARDMAX; ii++)
    if (ON_BOARD(ii) && !(inhibited_sources && inhibited_sources[ii])) {
      if (q->white_strength[ii] > 0.0)
	accumulate_influence(q, ii, WHITE);
      if (q->black_strength[ii] > 0.0)
	accumulate_influence(q, ii, BLACK);
    }

  value_territory(q);
  remove_double_blocks(q, inhibited_sources);

  value_territory(q);
  
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
compute_influence(int color, const signed char safe_stones[BOARDMAX],
    	          const float strength[BOARDMAX], struct influence_data *q,
		  int move, const char *trace_message)
{
  int save_debug = debug;
  VALGRIND_MAKE_WRITABLE(q, sizeof(*q));

  q->is_territorial_influence = 1;
  q->color_to_move = color;

  /* Turn off DEBUG_INFLUENCE for influence computations we are not
   * interested in.
   */
  if ((move == NO_MOVE
       && !(printmoyo & PRINTMOYO_INITIAL_INFLUENCE))
      || (move != NO_MOVE && move != debug_influence))
    debug = debug &~ DEBUG_INFLUENCE;

  influence_id++;
  q->id = influence_id;

  do_compute_influence(safe_stones, NULL, strength,
		       q, move, trace_message);

  debug = save_debug;
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

  if (bi > 0.0 && wi == 0.0 && terr < -territory_determination_value)
     return BLACK;
  if (wi > 0.0 && bi == 0.0 && terr > territory_determination_value)
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
    
  if (bi > moyo_data.influence_balance * wi
      && bi > moyo_data.my_influence_minimum
      && wi < moyo_data.opp_influence_maximum)
    return BLACK;
  if (wi > moyo_data.influence_balance * bi
      && wi > moyo_data.my_influence_minimum
      && bi < moyo_data.opp_influence_maximum)
    return WHITE;
  
  return EMPTY;
}

/* Return the color who has a moyo at (pos). If neither color has a
 * moyo there, EMPTY is returned.
 * The definition of moyo in terms of the influences is totally ad
 * hoc.
 *
 * It has a slightly different definition of moyo than whose_moyo.
 */
int
whose_moyo_restricted(const struct influence_data *q, int pos)
{
  float bi = q->black_influence[pos];
  float wi = q->white_influence[pos];

  int territory_color = whose_territory(q, pos);

  /* default */
  if (territory_color != EMPTY)
    return territory_color;
  else if (bi > moyo_restricted_data.influence_balance * wi
           && bi > moyo_restricted_data.my_influence_minimum
           && wi < moyo_restricted_data.opp_influence_maximum)
    return BLACK;
  else if (wi > moyo_restricted_data.influence_balance * bi
           && wi > moyo_restricted_data.my_influence_minimum
           && bi < moyo_restricted_data.opp_influence_maximum)
    return WHITE; 
  else
    return EMPTY;
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

 
static void
value_territory(struct influence_data *q)
{
  int ii;
  int dist_i, dist_j;
  float central;
  float first_guess[BOARDMAX];
  float ratio;
  int k;

  memset(first_guess, 0, BOARDMAX*sizeof(float));
  memset(q->territory_value, 0, BOARDMAX*sizeof(float));
  /* First loop: guess territory directly from influence. */
  for (ii = BOARDMIN; ii < BOARDMAX; ii++)
    if (ON_BOARD(ii)
	&& !q->safe[ii]) {
      float diff = 0.0;
      if (q->white_influence[ii] + q->black_influence[ii] > 0)
	diff = (q->white_influence[ii] - q->black_influence[ii])
	       / (q->white_influence[ii] + q->black_influence[ii]);
      first_guess[ii] = diff * diff * diff;

      /* If both side have small influence, we have to reduce this value.
       * What we consider "small influence" depends on how central this
       * intersection lies.
       *
       * The values of central on an 11x11 board become:
       *
       *  4  5  6  7  7  7  7  7  6  5  4
       *  5  8  9 10 10 10 10 10  9  8  5
       *  6  9 12 13 13 13 13 13 12  9  6
       *  7 10 13 16 16 16 16 16 13 10  7
       *  7 10 13 16 17 17 17 16 13 10  7
       *  7 10 13 16 17 18 17 16 13 10  7
       *  7 10 13 16 17 17 17 16 13 10  7
       *  7 10 13 16 16 16 16 16 13 10  7
       *  6  9 12 13 13 13 13 13 12  9  6
       *  5  8  9 10 10 10 10 10  9  8  5
       *  4  5  6  7  7  7  7  7  6  5  4
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

      /* Do not make this adjustment when scoring unless both
       * players have non-zero influence.
       */
      if (doing_scoring && (q->black_influence[ii] == 0.0
			    || q->white_influence[ii] == 0.0))
	ratio = 1.0;
      
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
      q->territory_value[ii] = first_guess[ii];
    }

  /* Second loop: Correct according to neighbour vertices. Each territory
   * value is degraded to the minimum value of its neighbors (unless this
   * neighbor has reduced permeability for the opponent's influence).
   */
  for (ii = BOARDMIN; ii < BOARDMAX; ii++)
    if (ON_BOARD(ii)
	/* Do not overrule dead stone territory above.
	 * FIXME: This does not do what it claims to do. Correcting it
	 * seems to break some tests, though.
	 */
	&& !q->safe[ii]) {
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

  /* Third loop: Nonterritory patterns, points for prisoners. */
  for (ii = BOARDMIN; ii < BOARDMAX; ii++)
    if (ON_BOARD(ii)
	&& !q->safe[ii]) {
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


/* Segment the influence map into connected regions of territory,
 * moyo, or area. What to segment on is determined by the the function
 * pointer region_owner. The segmentation is performed for both
 * colors. The connected regions may include stones of the own color,
 * but only empty intersections (and dead opponent stones) count
 * toward the region size.
 */
static void
segment_region(struct influence_data *q, owner_function_ptr region_owner,
	       struct moyo_data *regions)
{
  int ii;
  static signed char marked[BOARDMAX];
  regions->number = 0;

  /* Reset the markings. */
  for (ii = BOARDMIN; ii < BOARDMAX; ii++) {
    marked[ii] = 0;
    regions->segmentation[ii] = 0;
  }

  for (ii = BOARDMIN; ii < BOARDMAX; ii++)
    if (ON_BOARD(ii)
	&& !marked[ii]
	&& region_owner(q, ii) != EMPTY) {
      /* Found an unlabelled intersection. Use flood filling to find
       * the rest of the region.
       */
      int size = 0;
      float terr_val = 0.0;
      int queue_start = 0;
      int queue_end = 1;
      int color = region_owner(q, ii);
      regions->number++;
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
	regions->segmentation[tt] = regions->number;
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
      regions->size[regions->number] = size;
      regions->territorial_value[regions->number] = terr_val;
      regions->owner[regions->number] = color;
    }
}



/* Export a territory segmentation. */
void
influence_get_territory_segmentation(struct influence_data *q,
    			             struct moyo_data *moyos)
{
  segment_region(q, whose_territory, moyos);
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

int
influence_considered_lively(const struct influence_data *q, int pos)
{
  int color = board[pos];
  ASSERT1(IS_STONE(color), pos);
  return (q->safe[pos]
          && ((color == WHITE && q->white_strength[pos] > 0)
	      || (color == BLACK && q->black_strength[pos] > 0)));
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
  signed char goal[BOARDMAX];
  /* This is the color that will get a followup value. */
  int color = OTHER_COLOR(base->color_to_move);
  int save_debug = debug;

  memcpy(q, base, sizeof(*q));
  ASSERT1(IS_STONE(q->color_to_move), move);
  q->color_to_move = color;
 
  /* We mark the saved stones and their neighbors in the goal array.
   */
  for (ii = BOARDMIN; ii < BOARDMAX; ii++)
    if (ON_BOARD(ii)) {
      if (q->safe[ii] == INFLUENCE_SAVED_STONE)
        goal[ii] = 1;
      else
	goal[ii] = 0;
    }


  /* Turn off DEBUG_INFLUENCE for influence computations we are not
   * interested in.
   */
  if ((move == NO_MOVE
       && !(printmoyo & PRINTMOYO_INITIAL_INFLUENCE))
      || (move != debug_influence))
    debug = debug &~ DEBUG_INFLUENCE;

  q->intrusion_counter = 0;
  current_influence = q;
  /* Match B patterns for saved stones. */
  matchpat_goal_anchor(followup_influence_callback, color, &barrierspat_db, 
           	       q, goal, 1);

  debug = save_debug;
 
  /* Now add the intrusions. */
  add_marked_intrusions(q);

  reset_unblocked_blocks(q);
  
  /* Spread influence for new influence sources. */
  for (ii = BOARDMIN; ii < BOARDMAX; ii++)
    if (ON_BOARD(ii))
      if ((color == BLACK
            && q->black_strength[ii] > base->black_strength[ii])
          || (color == WHITE
              && q->white_strength[ii] > base->white_strength[ii]))
        accumulate_influence(q, ii, color);

  value_territory(q);

  if (debug_influence && debug_influence == move)
    print_influence(q, trace_message);
}


/* Compute influence based escape values and return them in the
 * escape_value array.  
 */

void
compute_escape_influence(int color, const signed char safe_stones[BOARDMAX],
			 const signed char goal[BOARDMAX],
    			 const float strength[BOARDMAX],
    			 signed char escape_value[BOARDMAX])
{
  int k;
  int ii;
  int save_debug = debug;

  /* IMPORTANT: The caching relies on the fact that safe_stones[] and
   * strength[] will currently always be identical for identical board[]
   * states. Better check for these, too.
   */
  static int cached_board[BOARDMAX];
  static signed char escape_values[BOARDMAX][2];
  static int active_caches[2] = {0, 0};

  int cache_number = (color == WHITE);

  VALGRIND_MAKE_WRITABLE(&escape_influence, sizeof(escape_influence));

  if (!goal) {
    /* Encode the values of color and dragons_known into an integer
     * between 0 and 3.
     */
    int board_was_cached = 1;

    /* Notice that we compare the out of board markers as well, in
     * case the board size should have changed between calls.
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
  }

  /* Use enhance pattern and higher attenuation for escape influence. */
  escape_influence.is_territorial_influence = 0;
  escape_influence.color_to_move = EMPTY;

  /* Turn off DEBUG_INFLUENCE unless we are specifically interested in
   * escape computations.
   */
  if (!(debug & DEBUG_ESCAPE))
    debug &= ~DEBUG_INFLUENCE;

  do_compute_influence(safe_stones, goal, strength,
      		       &escape_influence, -1, NULL);

  debug = save_debug;
  
  for (ii = BOARDMIN; ii < BOARDMAX; ii++)
    if (ON_BOARD(ii)) {
      if (whose_moyo(&escape_influence, ii) == color)
	escape_value[ii] = 4;
      else if (whose_area(&escape_influence, ii) == color)
	escape_value[ii] = 2;
      else if (whose_area(&escape_influence, ii) == EMPTY) {
	if (goal) {
	  escape_value[ii] = 0;

	  if (!goal[ii]) {
	    int goal_proximity = 0;

	    for (k = 0; k < 8; k++) {
	      if (ON_BOARD(ii + delta[k])) {
		goal_proximity += 2 * goal[ii + delta[k]];
		if (k < 4 && ON_BOARD(ii + 2 * delta[k]))
		  goal_proximity += goal[ii + delta[k]];
	      }
	      else
		goal_proximity += 1;
	    }

	    if (goal_proximity < 6)
	      escape_value[ii] = 1;
	  }
	}
	else
	  escape_value[ii] = 1;
      }
      else
	escape_value[ii] = 0;
    }

  if (0 && (debug & DEBUG_ESCAPE) && verbose > 0)
    print_influence(&escape_influence, "escape influence");

  if (!goal) {
    /* Save the computed values in the cache. */
    for (ii = BOARDMIN; ii < BOARDMAX; ii++)
      if (ON_BOARD(ii))
	escape_values[ii][cache_number] = escape_value[ii];
    active_caches[cache_number] = 1;
  }
}


/* Cache of delta_territory_values. */
static float delta_territory_cache[BOARDMAX];
static float followup_territory_cache[BOARDMAX];
static Hash_data delta_territory_cache_hash[BOARDMAX];
static int territory_cache_position_number = -1;
static int territory_cache_influence_id = -1;
static int territory_cache_color = -1;

/* We cache territory computations. This avoids unnecessary re-computations
 * when review_move_reasons is run a second time for the endgame patterns.
 *
 * (*base) points to the initial_influence data that would be used
 * to make the territory computation against.
 */
int 
retrieve_delta_territory_cache(int pos, int color, float *move_value,
    			       float *followup_value,
			       const struct influence_data *base,
			       Hash_data safety_hash)
{
  ASSERT_ON_BOARD1(pos);
  ASSERT1(IS_STONE(color), pos);

  /* We check whether the color, the board position, or the base influence
   * data has changed since the cache entry got entered.
   */
  if (territory_cache_position_number == position_number
      && territory_cache_color == color
      && territory_cache_influence_id == base->id
      && delta_territory_cache[pos] != NOT_COMPUTED) {
    int i;
    for (i = 0; i < NUM_HASHVALUES; i++)
      if (delta_territory_cache_hash[pos].hashval[i]
	  != safety_hash.hashval[i])
	return 0;
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
			    float move_value, float followup_value,
			    const struct influence_data *base,
			    Hash_data safety_hash)
{
  int i;

  ASSERT_ON_BOARD1(pos);
  ASSERT1(IS_STONE(color), pos);

  if (territory_cache_position_number != position_number
      || territory_cache_color != color
      || territory_cache_influence_id != base->id) {
    int ii;
    for (ii = BOARDMIN; ii < BOARDMAX; ii++)
      delta_territory_cache[ii] = NOT_COMPUTED;
    territory_cache_position_number = position_number;
    territory_cache_influence_id = base->id;
    territory_cache_color = color;
    if (0)
      gprintf("Cleared delta territory cache.\n");
  }
  delta_territory_cache[pos] = move_value;
  followup_territory_cache[pos] = followup_value;
  for (i = 0; i < NUM_HASHVALUES; i++)
    delta_territory_cache_hash[pos].hashval[i] = safety_hash.hashval[i];
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
  float this_delta;
  ASSERT_ON_BOARD1(move);
  ASSERT1(IS_STONE(color), move);

  for (ii = BOARDMIN; ii < BOARDMAX; ii++)
    if (ON_BOARD(ii)) {
      float new_value = q->territory_value[ii];
      float old_value = base->territory_value[ii];
      this_delta = new_value - old_value;
      /* Negate values if we are black. */
      if (color == BLACK) {
	new_value = -new_value;
	old_value = -old_value;
	this_delta = -this_delta;
      }
      
      if (move != -1
	  && (this_delta > 0.02 || -this_delta > 0.02))
	DEBUG(DEBUG_TERRITORY,
	      "  %1m:   - %1m territory change %f (%f -> %f)\n",
	      move, ii, this_delta, old_value, new_value);
      total_delta += this_delta;
    }

  /* Finally, captured stones: */
  this_delta = q->captured - base->captured;
  if (color == BLACK)
    this_delta = -this_delta;
  if (move != -1
      && this_delta != 0.0)
    DEBUG(DEBUG_TERRITORY, "  %1m:   - captured stones %f\n",
	  move, this_delta);
  total_delta += this_delta;

  return total_delta;
}


/* Estimate the score. A positive value means white is ahead. The
 * score is estimated influence data *q, which must have been
 * computed in advance.
 */
float
influence_score(const struct influence_data *q, int use_chinese_rules)
{
  float score = 0.0;
  int ii;

  for (ii = BOARDMIN; ii < BOARDMAX; ii++)
    if (ON_BOARD(ii))
      score += q->territory_value[ii];
  
  if (use_chinese_rules)
    score += stones_on_board(WHITE) - stones_on_board(BLACK) + komi + handicap;
  else
    score += black_captured - white_captured + komi;

  return score;
}


/* Uses initial_influence to estimate the game advancement (fuseki,
 * chuban, yose) returned as a value between 0.0 (start) and 1.0 (game
 * over)
 */
float
game_status(int color)
{
  struct influence_data *iq = INITIAL_INFLUENCE(color);
  struct influence_data *oq = OPPOSITE_INFLUENCE(color);
  int count = 0;
  int ii;

  for (ii = BOARDMIN; ii < BOARDMAX; ii++)
    if (ON_BOARD(ii)) {
      if (iq->safe[ii])
	count += WEIGHT_TERRITORY;
      else if (whose_territory(iq, ii) != EMPTY
	       && whose_territory(oq, ii) != EMPTY)
	count += WEIGHT_TERRITORY;
      else if (whose_moyo(oq, ii) != EMPTY)
	count += WEIGHT_MOYO;
      else if (whose_area(oq, ii) != EMPTY)
	count += WEIGHT_AREA;
    }

  return (float) count / (WEIGHT_TERRITORY * board_size * board_size);
}


/* Print the influence map when we have computed influence for the
 * move at (i, j).
 */
void
debug_influence_move(int move)
{
  debug_influence = move;
}


/* One more way to export influence data. This should only be used
 * for debugging.
 */
void
get_influence(const struct influence_data *q,
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
	      int non_territory[BOARDMAX])
{
  int ii;
  
  for (ii = BOARDMIN; ii < BOARDMAX; ii++) {
    white_influence[ii] = q->white_influence[ii];
    black_influence[ii] = q->black_influence[ii];
    white_strength[ii] = q->white_strength[ii];
    black_strength[ii] = q->black_strength[ii];
    white_attenuation[ii] = q->white_attenuation[ii];
    black_attenuation[ii] = q->black_attenuation[ii];
    white_permeability[ii] = q->white_permeability[ii];
    black_permeability[ii] = q->black_permeability[ii];
    territory_value[ii] = q->territory_value[ii];
    non_territory[ii] = q->non_territory[ii];

    if (board[ii] == EMPTY) {
      if (whose_territory(q, ii) == WHITE)
	influence_regions[ii] = 3;
      else if (whose_territory(q, ii) == BLACK)
	influence_regions[ii] = -3;
      else if (whose_moyo(q, ii) == WHITE)
	influence_regions[ii] = 2;
      else if (whose_moyo(q, ii) == BLACK)
	influence_regions[ii] = -2;
      else if (whose_area(q, ii) == WHITE)
	influence_regions[ii] = 1;
      else if (whose_area(q, ii) == BLACK)
	influence_regions[ii] = -1;
      else
	influence_regions[ii] = 0;
    }
    else if (board[ii] == WHITE)
      influence_regions[ii] = 4;
    else if (board[ii] == BLACK)
      influence_regions[ii] = -4;
  }
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
    print_numeric_influence(q, q->white_influence, "%5.1f", 5, 1, 0);
    /* Print the black influence values. */
    fprintf(stderr, "black influence (%s):\n", info_string);
    print_numeric_influence(q, q->black_influence, "%5.1f", 5, 1, 0);
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

