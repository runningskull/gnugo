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
static void print_influence(struct influence_data *q,
			    const char *info_string);
static void print_numeric_influence(struct influence_data *q,
				    float values[BOARDMAX],
				    const char *format, int draw_stones,
				    int mark_epsilon);
static void print_influence_areas(struct influence_data *q);
 
static void value_territory(struct influence_data *q, int pos, int color);
static void enter_intrusion_source(int source_pos, int strength_pos,
                                   float strength, float attenuation,
                                   struct influence_data *q);
static void add_marked_intrusions(struct influence_data *q, int color);
static void print_influence_territory(struct influence_data *q,
                                      const char *info_string);
 

/* Influence computed for the initial position, i.e. before making
 * some move.
 */
static struct influence_data initial_influence;
static struct influence_data initial_opposite_influence;

/* Influence computed after some move has been made. */
static struct influence_data move_influence;
static struct influence_data followup_influence;

/* Coordinates for the move influence was last computed for. */
static int influence_move = NO_MOVE;
static int influence_color = EMPTY;

/* Influence used for estimation of escape potential. */
static struct influence_data escape_influence;

/* Pointer to influence data used during pattern matching. */
static struct influence_data *current_influence = NULL;

/* Cache of delta_territory_values. */
static float delta_territory_cache[BOARDMAX];
static float followup_territory_cache[BOARDMAX];

/* If set, print influence map when computing this move. Purely for
 * debugging.
 */
static int debug_influence = NO_MOVE;

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
      if (q->p[arg] == EMPTY \
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
  float (*permeability_array);

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

/* Initialize the influence_data structure. If the dragons have been
 * computed, we weight the strength of the influence with the dragon
 * status.
 *
 * The saved_stones parameter tells which critical stones have been
 * defended (value INFLUENCE_SAVED_STONE) or attacked (value
 * INFLUENCE_CAPTURED_STONE) by the current move. If no move has been
 * done, it should only contain zeros (INFLUENCE_UNCHANGED_STONE).
 *
 * The name saved_stones is historic and should probably be changed
 * since it also includes captured stones.
 *
 */

static float strength_map[10] = {
  0.0,   /* DEAD            */
  0.9,   /* ALIVE           */
  0.5,   /* CRITICAL        */
  0.0,   /* INESSENTIAL     */
  0.0,   /* TACTICALLY DEAD */
  0.7,   /* WEAK            */
  0.8,   /* WEAKLY_ALIVE    */
  0.6,   /* ALIVE IN SEKI   */
  0.95,  /* STRONGLY ALIVE  */
  1.0    /* INVINCIBLE      */
};

static void
init_influence(struct influence_data *q, int color,
	       char saved_stones[BOARDMAX])
{
  int ii;
  float attenuation;

  gg_assert(saved_stones != NULL);
  
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
  
  for (ii = 0; ii < BOARDMAX; ii++)
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
      q->p[ii] = board[ii];
      q->non_territory[ii] = EMPTY;

      /* FIXME: Simplify the code below! */
      
      /* Dead stones (or critical ones for the color which will not
       * make the next move) count as empty space. However, influence
       * cannot flow through these for the owner of the stones.
       */
      if (IS_STONE(board[ii])) {
	if (saved_stones[ii] == INFLUENCE_CAPTURED_STONE
	    || (saved_stones[ii] == INFLUENCE_UNCHANGED_STONE
		&& ((worm[ii].attack_codes[0] != 0
		    && (OTHER_COLOR(q->p[ii]) == color
			|| worm[ii].defense_codes[0] == 0))
		    || (q->dragons_known
			&& dragon[ii].id != -1
			&& (DRAGON2(ii).safety == DEAD
			    || DRAGON2(ii).safety == TACTICALLY_DEAD
			    || (DRAGON2(ii).safety == CRITICAL
				&& board[ii] == OTHER_COLOR(color))))))) {
	  if (q->p[ii] == WHITE)
	    q->white_permeability[ii] = 0.0;
	  else
	    q->black_permeability[ii] = 0.0;
	  q->p[ii] = EMPTY;
	}
	else if (saved_stones[ii] == INFLUENCE_SAVED_STONE
		 || !q->dragons_known
		 || dragon[ii].id == -1
		 || (DRAGON2(ii).safety != DEAD
		     && DRAGON2(ii).safety != TACTICALLY_DEAD
		     && DRAGON2(ii).safety != CRITICAL)
		 || (DRAGON2(ii).safety == CRITICAL
		     && board[ii] == color)) {
	  if (q->p[ii] == WHITE)
	    q->black_permeability[ii] = 0.0;
	  else
	    q->white_permeability[ii] = 0.0;
	}

	/* We need to make an exception to the rules above for
         * INESSENTIAL stones. Instead of making the conditions above
         * still more complex we correct it here.
	 *
	 * If q->p[ii] is allowed to be 0, the territory evaluation
	 * will think it's a prisoner for the opponent, and various
	 * territory corrections and interpolations will mess up.
	 */
	if (IS_STONE(board[ii])
	    && q->dragons_known
	    && dragon[ii].id != -1
	    && (DRAGON2(ii).safety == INESSENTIAL
	        || (worm[ii].inessential
		    && ((DRAGON2(ii).safety != DEAD
			 && DRAGON2(ii).safety != TACTICALLY_DEAD
		         && DRAGON2(ii).safety != CRITICAL)
		        || (DRAGON2(ii).safety == CRITICAL
		            && board[ii] == color)))) 
	    && q->p[ii] == EMPTY)
	  q->p[ii] = board[ii];
      }
      
      /* When evaluating influence after a move, the newly placed
       * stone will have the invalid dragon id -1.
       */
      if (IS_STONE(board[ii])) {
	if (!q->dragons_known || saved_stones[ii] == INFLUENCE_SAVED_STONE) {
	  if (q->p[ii] == WHITE)
	    q->white_strength[ii] = DEFAULT_STRENGTH;
	  else if (q->p[ii] == BLACK)
	    q->black_strength[ii] = DEFAULT_STRENGTH;
	}
	else if (saved_stones[ii] != INFLUENCE_CAPTURED_STONE) {
	  ASSERT1(dragon[ii].id != -1, ii);
	  if (q->p[ii] == WHITE) {
	    if (color == BLACK && DRAGON2(ii).safety == CRITICAL)
	      q->white_strength[ii] = 0.0;
	    else
	      q->white_strength[ii] = (DEFAULT_STRENGTH
					 * strength_map[DRAGON2(ii).safety]);
	  }
	  else if (q->p[ii] == BLACK) {
	    if (color == WHITE && DRAGON2(ii).safety == CRITICAL)
	      q->black_strength[ii] = 0.0;
	    else
	      q->black_strength[ii] = (DEFAULT_STRENGTH
					 * strength_map[DRAGON2(ii).safety]);
	  }
	}
      }
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
      if (q->p[pos] == EMPTY && q->white_strength[pos] > 0.0
	  && q->white_permeability[pos] != 1.0) {
	DEBUG(DEBUG_INFLUENCE, "  black block removed from %1m\n", pos);
	q->white_permeability[pos] = 1.0;
      }
      if (q->p[pos] == EMPTY && q->black_strength[pos] > 0.0
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
      DEBUG(DEBUG_INFLUENCE, "Adding %s intrusion at %1m, value %f\n",
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
influence_callback(int m, int n, int color, struct pattern *pattern, int ll,
		   void *data)
{
  int pos = AFFINE_TRANSFORM(pattern->movei, pattern->movej, ll, m, n);
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
      ii = AFFINE_TRANSFORM(pattern->patn[k].x, pattern->patn[k].y, ll, m, n);

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
	int ii = AFFINE_TRANSFORM(pattern->patn[k].x, pattern->patn[k].y,
				  ll, m , n);
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
    if (!pattern->autohelper(pattern, ll, pos, color, 0))
      return;
  }

  /* If the pattern has a helper, call it to see if the pattern must
   * be rejected.
   */
  if (pattern->helper) {
    if (!pattern->helper(pattern, ll, pos, color)) {
      DEBUG(DEBUG_INFLUENCE,
	    "Influence pattern %s+%d rejected by helper at %1m\n",
	    pattern->name, ll, pos);
      return;
    }
  }

  DEBUG(DEBUG_INFLUENCE, "influence pattern '%s'+%d matched at %1m\n",
	pattern->name, ll, POS(m, n));

  /* For t patterns, everything happens in the action. */
  if ((pattern->class & CLASS_t)
      && (pattern->autohelper_flag & HAVE_ACTION)) {
    pattern->autohelper(pattern, ll, pos, color, INFLUENCE_CALLBACK);
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

    DEBUG(DEBUG_INFLUENCE,
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
    DEBUG(DEBUG_INFLUENCE,
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
      int ii = AFFINE_TRANSFORM(pattern->patn[k].x, pattern->patn[k].y,
      	   			ll, m, n);

      /* Territorial connection, making a barrier for opponent influence. */
      if (pattern->class & (CLASS_A | CLASS_D)) {
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
      
      /* Low intensity influence source for the color in turn to move. */
      if (pattern->class & CLASS_B) {
        if (q->is_territorial_influence)
          enter_intrusion_source(POS(m, n), ii, pattern->value,
	  		         TERR_DEFAULT_ATTENUATION, q);
        else
          add_influence_source(ii, color,
			       pattern->value, DEFAULT_ATTENUATION, q);
	DEBUG(DEBUG_INFLUENCE, "  intrusion at %1m\n", ii);
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
followup_influence_callback(int m, int n, int color, struct pattern *pattern,
                            int ll, void *data)
{
  int k;
  int t;
  struct influence_data *q = data;
  UNUSED(color);
 
  /* We use only B  patterns in followup influence. */
  if (!(pattern->class & CLASS_B))
    return;

  t = AFFINE_TRANSFORM(pattern->movei, pattern->movej, ll, m, n);
  /* If the pattern has a constraint, call the autohelper to see
   * if the pattern must be rejected.
   */
  if (pattern->autohelper_flag & HAVE_CONSTRAINT) {
    if (!pattern->autohelper(pattern, ll, t, color, 0))
      return;
  }

  /* If the pattern has a helper, call it to see if the pattern must
   * be rejected.
   */
  if (pattern->helper) {
    if (!pattern->helper(pattern, ll, t, color)) {
      DEBUG(DEBUG_INFLUENCE,
            "Influence pattern %s+%d rejected by helper at %1m\n",
            pattern->name, ll, t);
      return;
    }
  }
 
 /* Actions in B patterns are used as followup specific constraints. */
 if ((pattern->autohelper_flag & HAVE_ACTION)
     && !pattern->autohelper(pattern, ll, t, color,
                             FOLLOWUP_INFLUENCE_CALLBACK))
    return;

  DEBUG(DEBUG_INFLUENCE, "influence pattern '%s'+%d matched at %1m\n",
	pattern->name, ll, POS(m, n));

  for (k = 0; k < pattern->patlen; ++k)  /* match each point */
    if (pattern->patn[k].att == ATT_not) {
      /* transform pattern real coordinate */
      int ii = AFFINE_TRANSFORM(pattern->patn[k].x, pattern->patn[k].y,
				ll, m, n);

      /* Low intensity influence source for the color in turn to move. */
      enter_intrusion_source(POS(m, n), ii, pattern->value,
			     TERR_DEFAULT_ATTENUATION, q);
      DEBUG(DEBUG_INFLUENCE, "  followup for %1m: intrusion at %1m\n",
            POS(m, n), ii);
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
	&& IS_STONE(q->p[ii])) {
      int k;
      for (k = 0; k < 8; k++) {
	int d = delta[k];
	if (ON_BOARD(ii + d) && q->p[ii + d] == EMPTY) {
	  /* Reduce less diagonally. */
	  float reduction = (k < 4) ? 0.25 : 0.65;
	  if (q->p[ii] == BLACK)
	    q->white_permeability[ii + d] *= reduction;
	  else
	    q->black_permeability[ii + d] *= reduction;
	}
      }
    }

  reset_unblocked_blocks(q);
}

/* Compute the influence values for both colors, after having made a
 * move for OTHER_COLOR(color) at (pos). If these move is NO_MOVE
 * no move is made. In any case it's assumed that color is in turn to
 * move. (This affects the barrier patterns (class A, D) and intrusions
 * (class B)).
 */
static void
compute_influence(struct influence_data *q, int color, int pos,
		  char no_influence[BOARDMAX], char saved_stones[BOARDMAX])
{
  int ii;
  char dummy_saved_stones[BOARDMAX];
  if (saved_stones != NULL)
    init_influence(q, color, saved_stones);
  else {
    memset(dummy_saved_stones, 0, sizeof(dummy_saved_stones));
    init_influence(q, color, dummy_saved_stones);
  }

#if 0
  /* This is used when computing escape influence to remove the
   * influence from the escaping dragon.
   */
  if (no_influence) {
    for (i = 0; i < board_size; i++)
      for (j = 0; j < board_size; j++)
	if (no_influence[i][j]) {
	  q->p[i][j] = EMPTY;
	  q->white_strength[i][j] = 0.0;
	  q->black_strength[i][j] = 0.0;
	  q->white_permeability[i][j] = 1.0;
	  q->black_permeability[i][j] = 1.0;
	}
  }
#else
  UNUSED(no_influence);
#endif
  
  if (q != &escape_influence)
    find_influence_patterns(q, color);
  else
    find_influence_patterns(q, EMPTY);
  
  for (ii = BOARDMIN; ii < BOARDMAX; ii++)
    if (ON_BOARD(ii)) {
      if (q->white_strength[ii] > 0.0)
	accumulate_influence(q, ii, WHITE);
      if (q->black_strength[ii] > 0.0)
	accumulate_influence(q, ii, BLACK);
    }

  value_territory(q, pos, color);
  segment_influence(q);
  
  if (((q == &initial_influence || q == &initial_opposite_influence)
       && (printmoyo & PRINTMOYO_INITIAL_INFLUENCE))
      || (debug_influence && pos == debug_influence)) {
    if (q == &initial_opposite_influence)
      print_influence(q, (q->dragons_known ? "dragons_known, opposite, color"
			  : "dragons_unknown, opposite, color"));
    else
      print_influence(q, q->dragons_known ? "dragons_known"
	  		 : "dragons_unknown");
  }
}

/* Return the color of the territory at (pos). If it's territory for
 * neither color, EMPTY is returned.
 * The definition of territory in terms of the influences is totally
 * ad hoc.
 */
static int
whose_territory(struct influence_data *q, int pos)
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

/* Return the color who has a moyo at (m, n). If neither color has a
 * moyo there, EMPTY is returned. The definition of moyo in terms of the
 * influences is totally ad hoc.
 */
static int
whose_moyo(struct influence_data *q, int pos)
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

/* Return the color who has a moyo at (m, n). If neither color has a
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
whose_moyo_restricted(struct influence_data *q, int pos)
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


/* Return the color who has dominating influence ("area") at (m, n).
 * If neither color dominates the influence there, EMPTY is returned.
 * The definition of area in terms of the influences is totally ad
 * hoc.
 */
static int
whose_area(struct influence_data *q, int pos)
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
struct interpolation_data min_infl_for_territory =
  { 6,  0.0, 24.0, { 6.0, 15.0, 26.0, 36.0, 45.0, 50.0, 55.0 }};

/* Determines the territory correction factor in dependence of the ratio
 * ( influence of stronger color / min_infl_for_territory(intersection))
 */
struct interpolation_data territory_correction = 
  { 5, (float) 0.0, 1.0, {0.0, 0.25, 0.45, 0.65, 0.85, 1.0}};

static void
value_territory(struct influence_data *q, int pos, int color)
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

      if (q->p[ii] == EMPTY) {
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
      if (q->p[ii] == EMPTY) {
	/* Loop over all neighbors. */
        for (k = 0; k < 4; k++) {
          if (!ON_BOARD(ii + delta[k]))
            continue;
          if (q->territory_value[ii] > 0.0) {
            /* White territory. */
            if (q->p[ii + delta[k]] != WHITE) {
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
            if (q->p[ii + delta[k]] != BLACK) {
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
      if (q->p[ii] == EMPTY) {
	/* If marked as non-territory for the color currently owning
         * it, reset the territory value.
	 */
	if (q->territory_value[ii] > 0.0
	    && (q->non_territory[ii] & WHITE))
	  q->territory_value[ii] = 0.0;

	if (q->territory_value[ii] < 0.0
	    && (q->non_territory[ii] & BLACK))
	  q->territory_value[ii] = 0.0;
	
	/* Dead stone, add one to the territory value.
	 *
	 * We also want to include stones which were captured by the
	 * last move when computing move influence. Therefore we look
	 * at worm[ii].color instead of just board[ii].
	 */
	if (worm[ii].color == BLACK)
	  q->territory_value[ii] += 1.0;
	else if (worm[ii].color == WHITE)
	  q->territory_value[ii] -= 1.0;
      }
    }

  /* Final correction. Never count the last played move as territory
   * for the color playing it. Ideally this should never happen, but
   * currently we need this workaround.
   */
  if (ON_BOARD(pos)) {
    if (color == BLACK && q->territory_value[pos] < 0.0)
      q->territory_value[pos] = 0.0;
    else if (color == WHITE && q->territory_value[pos] > 0.0)
      q->territory_value[pos] = 0.0;
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
	  if (q->p[tt] != color) {
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

/* Return the size of the moyo around (pos).
 */
int
influence_get_moyo_size(int pos, int color)
{
  int result1 = 0;
  int result2 = 0;
  struct influence_data *q;

  q = &initial_influence;

  /* Does the color match. */
  if ((q->region_type[q->moyo_segmentation[pos]] == WHITE_MOYO)
      ^ (color == BLACK))
    result1 = q->region_size[q->moyo_segmentation[pos]];

  q = &initial_opposite_influence;

  /* Does the color match. */
  if ((q->region_type[q->moyo_segmentation[pos]] == WHITE_MOYO)
      ^ (color == BLACK))
    result2 = q->region_size[q->moyo_segmentation[pos]];

  return gg_min(result1, result2);
}

/* Export the moyo segmentation. If (opposite) is true, then
 * initial_opposite_influence is used, otherwise initial_influence.
 */
void
influence_get_moyo_segmentation(int opposite, struct moyo_data *moyos)
{
  int ii;
  int min_moyo_id;
  int max_moyo_id;
  int i;
  struct influence_data *q;

  min_moyo_id = MAX_REGIONS;
  max_moyo_id = 0;

  if (opposite)
    q = &initial_opposite_influence;
  else
    q = &initial_influence;

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
influence_get_moyo_data(int opposite, int moyo_color[BOARDMAX],
			float territory_value[BOARDMAX])
{
  int ii;
  struct influence_data *q;

  if (opposite)
    q = &initial_opposite_influence;
  else
    q = &initial_influence;

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
influence_initial_territory(int pos, int color)
{
  if (color == WHITE)
    return initial_influence.territory_value[pos];
  else
    return -initial_influence.territory_value[pos];
}

/* Compute the influence before a move has been made, which can
 * later be compared to the influence after a move. Assume that
 * the other color is in turn to move.
 */
void
compute_initial_influence(int color, int dragons_known)
{
  int ii;

  initial_influence.dragons_known = dragons_known;
  initial_influence.is_territorial_influence = dragons_known;
  initial_opposite_influence.dragons_known = dragons_known;
  initial_opposite_influence.is_territorial_influence = dragons_known;

  decrease_depth_values();

  compute_influence(&initial_influence, OTHER_COLOR(color), NO_MOVE,
		    NULL, NULL);
  if (dragons_known) {
    if ((printmoyo & PRINTMOYO_VALUE_TERRITORY)
	&& (printmoyo & PRINTMOYO_INITIAL_INFLUENCE))
      print_influence_territory(&initial_influence,
				"territory (initial influence):\n");
  }

  compute_influence(&initial_opposite_influence, color, NO_MOVE, NULL, NULL);
  /* Invalidate information in move_influence. */
  influence_move = NO_MOVE;
  influence_color = EMPTY;
  /* Clear delta_territory cache. */
  for (ii = BOARDMIN; ii < BOARDMAX; ii++)
    if (ON_BOARD(ii)) {
      delta_territory_cache[ii] = NOT_COMPUTED;
      followup_territory_cache[ii] = NOT_COMPUTED;
    }

  increase_depth_values();
}

/* Redo the segmentation of the initial influence. */
void
resegment_initial_influence()
{
  segment_influence(&initial_influence);
  segment_influence(&initial_opposite_influence);
}

/* Experimental influence: Compute a followup influence for the move at
 * (m, n). Compute the territorial followup value.
 */
static void
compute_followup_influence(int pos, int color,
                           char saved_stones[BOARDMAX])
{
  int ii;
  char goal[BOARDMAX];

  UNUSED(pos);
  memcpy(&followup_influence, &move_influence, sizeof(move_influence));
 
  /* We mark the saved stones and their neighbors in the goal array
   * and in q->w.
   */
  for (ii = BOARDMIN; ii < BOARDMAX; ii++)
    if (ON_BOARD(ii)) {
      if (saved_stones[ii])
        goal[ii] = 1;
      else
	goal[ii] = 0;
    }

  followup_influence.intrusion_counter = 0;

  current_influence = &followup_influence;
  /* Match B patterns for saved stones. */
  matchpat_goal_anchor(followup_influence_callback, color, &barrierspat_db, 
           	       &followup_influence, goal, 1);
 
  /* Now add the intrusions. */
  add_marked_intrusions(&followup_influence, color);

  reset_unblocked_blocks(&followup_influence);
  
  /* Spread influence for new influence sources. */
  for (ii = BOARDMIN; ii < BOARDMAX; ii++)
    if (ON_BOARD(ii))
      if ((color == BLACK
          && followup_influence.black_strength[ii]
             > move_influence.black_strength[ii])
          || (color == WHITE
          && followup_influence.white_strength[ii]
             > move_influence.white_strength[ii]))
        accumulate_influence(&followup_influence, ii, color);

  value_territory(&followup_influence, pos, color);
}

/* Let color play at (m, n) and compute the influence after this move,
 * assuming that the other color is in turn to move next.
 */
static void
compute_move_influence(int pos, int color,
		       char saved_stones[BOARDMAX])
{
  /* Don't recompute if we already have the current values stored. */
  if (influence_move == pos
      && influence_color == color)
    return;

  move_influence.dragons_known = 1;
  move_influence.is_territorial_influence = 1;

  if (tryko(pos, color, "compute_move_influence", EMPTY, NO_MOVE)) {
    compute_influence(&move_influence, OTHER_COLOR(color), pos,
		      NULL, saved_stones);
    compute_followup_influence(pos, color, saved_stones);
    popgo();

    if (debug_influence && debug_influence == pos) {
      if (printmoyo & PRINTMOYO_VALUE_TERRITORY)
        print_influence_territory(&move_influence,
	    			  "territory (after move):\n");
      print_influence(&followup_influence, "followup influence");
      if (printmoyo & PRINTMOYO_VALUE_TERRITORY)
	print_influence_territory(&followup_influence,
				  "territory (followup):\n");
    }
  }
  else {
    gprintf("Computing influence for illegal move %1m (move number %d)\n",
	    pos, movenum+1);
    return;
  }

  influence_move = pos;
  influence_color = color;
}


/* Assume that the stones marked by the goal array do not generate
 * influence and compute influence. Influence based escape values are
 * returned in the escape_value array.  
 */

void
compute_escape_influence(char goal[BOARDMAX], int color,
			 char escape_value[BOARDMAX],
			 int dragons_known)
{
  int k;
  int ii;

  /* IMPORTANT: This caching relies on the fact that the goal
   * parameter currently is not used.
   */
  static int cached_board[BOARDMAX];
  static char escape_values[BOARDMAX][4];
  static int active_caches[4];

  /* Encode the values of color and dragons_known into an integer
   * between 0 and 3.
   */
  int cache_number = 2 * (color == WHITE) + dragons_known;

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
    for (k = 0; k < 4; k++)
      active_caches[k] = 0;
  
  if (active_caches[cache_number]) {
    for (ii = BOARDMIN; ii < BOARDMAX; ii++)
      if (ON_BOARD(ii))
	escape_value[ii] = escape_values[ii][cache_number];
    
    return;
  }

  /* Use enhance pattern and higher attenuation for escape influence. */
  escape_influence.is_territorial_influence = 0;
  escape_influence.dragons_known = dragons_known;

  decrease_depth_values();
  compute_influence(&escape_influence, OTHER_COLOR(color), NO_MOVE,
		    goal, NULL);
  increase_depth_values();
  
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
			    "%3.0f", 3, 1);
    print_numeric_influence(&escape_influence,
			    escape_influence.black_influence,
			    "%3.0f", 3, 1);
  }    

  /* Save the computed values in the cache. */
  for (ii = BOARDMIN; ii < BOARDMAX; ii++)
    if (ON_BOARD(ii))
      escape_values[ii][cache_number] = escape_value[ii];
  active_caches[cache_number] = 1;
}


/* Return the color who has territory at pos, or EMPTY. */
int
influence_territory_color(int pos)
{
  return whose_territory(&initial_influence, pos);
}

/* Return the color who has moyo at pos, or EMPTY. */
int
influence_moyo_color(int pos)
{
  return whose_moyo(&initial_influence, pos);
}

/* Return the color who has moyo at pos, or EMPTY, using influence
 * computed with the opposite color to move.
 */
int
influence_moyo_color_opposite(int pos)
{
  return whose_moyo(&initial_opposite_influence, pos);
}

/* Return the color who has area at pos, or EMPTY. */
int
influence_area_color(int pos)
{
  return whose_area(&initial_influence, pos);
}

/* Compute the difference in territory made by a move by color at (pos).
 * This also includes the changes in moyo and area.
 * In experimental-influence mode, followup_value must not be a NULL
 * pointer, and the followup_value will be returned there.
 */
float
influence_delta_territory(int pos, int color, char saved_stones[BOARDMAX],
                          float *followup_value)
{
  int ii;
  float delta = 0.0;
  float followup_delta = 0.0;
  if (delta_territory_cache[pos] != NOT_COMPUTED) {
    gg_assert(followup_territory_cache[pos] != NOT_COMPUTED);
    gg_assert(followup_value != NULL);
    *followup_value = followup_territory_cache[pos];
    return delta_territory_cache[pos];
  }
  if (0)
    gprintf("influence_delta_territory for %1m %s = ", pos,
	    color_to_string(color));

  compute_move_influence(pos, color, saved_stones);

  for (ii = BOARDMIN; ii < BOARDMAX; ii++)
    if (ON_BOARD(ii)) {
      float new_value = move_influence.territory_value[ii];
      float old_value = initial_influence.territory_value[ii];
      float followup_value = followup_influence.territory_value[ii];
      /* Negate values if we are black. */
      if (color == BLACK) {
	new_value = -new_value;
	old_value = -old_value;
	followup_value = -followup_value;
      }
      
      if (new_value - old_value > 0.02
          || old_value - new_value > 0.02)
	DEBUG(DEBUG_TERRITORY, "  %1m:   - %1m territory change %f (%f -> %f)\n",
	      pos, ii, new_value - old_value, old_value, new_value);
      delta += new_value - old_value;
      followup_delta += followup_value - new_value;
    }
  
  if (0)
    gprintf("%f\n", delta);
  delta_territory_cache[pos] = delta;

  gg_assert(followup_value != NULL);
  followup_territory_cache[pos] = followup_delta;
  *followup_value = followup_delta;

  return delta;
}


/* Estimate the score. A positive value means white is ahead. The
 * score is estimated from the initial_influence, which must have been
 * computed in advance. The score estimate does not include captured
 * stones (i.e., the ones having been removed from the board) or komi.
 * moyo_coeff and area_coeff are the relative weights to be used for
 * the moyo and area difference respectively.
 */
#define DEBUG_INFLUENCE_SCORE 0
float
influence_estimate_score(float moyo_coeff, float area_coeff)
{
  int black_territory = 0;
  int white_territory = 0;
  int black_moyo = 0;
  int white_moyo = 0;
  int black_area = 0;
  int white_area = 0;
  float score;
  int ii;

  for (ii = BOARDMIN; ii < BOARDMAX; ii++)
    if (board[ii] == EMPTY) {
      if (whose_territory(&initial_influence, ii) == BLACK) {
	if (DEBUG_INFLUENCE_SCORE)
	  gprintf("%1m black 1t\n", ii);
	black_territory++;
      }
      else if (whose_territory(&initial_influence, ii) == WHITE) {
	if (DEBUG_INFLUENCE_SCORE)
	  gprintf("%1m white 1t\n", ii);
	white_territory++;
      }
      else if (whose_moyo(&initial_influence, ii) == BLACK) {
	if (DEBUG_INFLUENCE_SCORE)
	  gprintf("%1m black 1m\n", ii);
	black_moyo++;
      }
      else if (whose_moyo(&initial_influence, ii) == WHITE) {
	if (DEBUG_INFLUENCE_SCORE)
	  gprintf("%1m white 1m\n", ii);
	white_moyo++;
      }
      else if (whose_area(&initial_influence, ii) == BLACK) {
	if (DEBUG_INFLUENCE_SCORE)
	  gprintf("%1m black 1a\n", ii);
	black_area++;
      }
      else if (whose_area(&initial_influence, ii) == WHITE) {
	if (DEBUG_INFLUENCE_SCORE)
	  gprintf("%1m white 1a\n", ii);
	white_area++;
      }
    }
    else if (board[ii] == BLACK
	     && initial_influence.black_strength[ii] == 0) {
      if (DEBUG_INFLUENCE_SCORE)
	gprintf("%1m white 2t\n", ii);
      white_territory += 2;
    }
    else if (board[ii] == WHITE
	     && initial_influence.white_strength[ii] == 0) {
      if (DEBUG_INFLUENCE_SCORE)
	gprintf("%1m black 2t\n", ii);
      black_territory += 2;
    }

  DEBUG(DEBUG_SCORING, "black:%d %d %d, white: %d %d %d\n",
	black_territory, black_moyo,
	black_area, white_territory, white_moyo, white_area);
  
  score = (white_territory - black_territory
	   + moyo_coeff * (white_moyo - black_moyo)
	   + area_coeff * (white_area - black_area));
  
  return score;
}

/* Print the influence map when we have computed influence for the
 * move at (i, j).
 */
void
debug_influence_move(int i, int j)
{
  debug_influence = POS(i, j);
}

/* Copy and encode influence data. */
static void
retrieve_influence(struct influence_data *q,
		   float white_influence[BOARDMAX],
		   float black_influence[BOARDMAX],
		   int influence_regions[BOARDMAX])
{
  int ii;
  for (ii = BOARDMIN; ii < BOARDMAX; ii++)
    if (board[ii] == EMPTY) {
      white_influence[ii] = q->white_influence[ii];
      black_influence[ii] = q->black_influence[ii];
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
}
  
/* Compute initial influence and export it. The color parameter tells
 * who is in turn to move.
 */
void
get_initial_influence(int color, int dragons_known,
		      float white_influence[BOARDMAX],
		      float black_influence[BOARDMAX],
		      int influence_regions[BOARDMAX])
{
  compute_initial_influence(color, dragons_known);
  retrieve_influence(&initial_influence, white_influence,
		     black_influence, influence_regions);
}

/* Compute influence after a move and export it.
 */
void
get_move_influence(int move, int color,
		   char saved_stones[BOARDMAX],
		   float white_influence[BOARDMAX],
		   float black_influence[BOARDMAX],
		   int influence_regions[BOARDMAX])
{
  compute_move_influence(move, color, saved_stones);
  retrieve_influence(&move_influence, white_influence,
		     black_influence, influence_regions);
}

/* Compute initial influence and print it. Notice that it's assumed
 * that the printmoyo global tells what information to print. The
 * color parameter tells who is in turn to move.
 */
void
print_initial_influence(int color, int dragons_known)
{
  compute_initial_influence(color, dragons_known);
  print_influence(&initial_influence, (dragons_known ? "dragons_known"
				       : "dragons_unknown"));

  print_influence(&initial_opposite_influence,
		  dragons_known ? "dragons_known, opposite color"
		  : "dragons_unknown, opposite color");
}

/* Compute influence after doing a move and print it. Notice that it's
 * assumed that the printmoyo global tells what information to print.
 */
void
print_move_influence(int pos, int color,
		     char saved_stones[BOARDMAX])
{
  compute_move_influence(pos, color, saved_stones);
  print_influence(&move_influence, "after move, dragons known");

}

/* Print influence for debugging purposes. */
static void
print_influence(struct influence_data *q, const char *info_string)
{
  if (printmoyo & PRINTMOYO_ATTENUATION) {
    /* Print the attenuation values. */
    fprintf(stderr, "white attenuation (%s):\n", info_string);
    print_numeric_influence(q, q->white_attenuation, "%3.2f", 0, 0);
    fprintf(stderr, "black attenuation (%s):\n", info_string);
    print_numeric_influence(q, q->black_attenuation, "%3.2f", 0, 0);
  }

  if (printmoyo & PRINTMOYO_PERMEABILITY) {
    /* Print the white permeability values. */
    fprintf(stderr, "white permeability:\n");
    print_numeric_influence(q, q->white_permeability, "%3.1f", 0, 0);
    
    /* Print the black permeability values. */
    fprintf(stderr, "black permeability:\n");
    print_numeric_influence(q, q->black_permeability, "%3.1f", 0, 0);
  }

  if (printmoyo & PRINTMOYO_STRENGTH) {
    /* Print the strength values. */
    fprintf(stderr, "white strength:\n");
    if (q->is_territorial_influence)
      print_numeric_influence(q, q->white_strength, "%5.1f", 0, 0);
    else
      print_numeric_influence(q, q->white_strength, "%3.0f", 0, 1);
    fprintf(stderr, "black strength:\n");
    if (q->is_territorial_influence)
      print_numeric_influence(q, q->black_strength, "%5.1f", 0, 0);
    else
      print_numeric_influence(q, q->black_strength, "%3.0f", 0, 1);
  }

  if (printmoyo & PRINTMOYO_NUMERIC_INFLUENCE) {
    /* Print the white influence values. */
    fprintf(stderr, "white influence (%s):\n", info_string);
    if (q->is_territorial_influence)
      print_numeric_influence(q, q->white_influence, "%5.1f", 5, 0);
    else
      print_numeric_influence(q, q->white_influence, "%3.0f", 3, 1);
    /* Print the black influence values. */
    fprintf(stderr, "black influence (%s):\n", info_string);
    if (q->is_territorial_influence)
      print_numeric_influence(q, q->black_influence, "%5.1f", 5, 0);
    else
      print_numeric_influence(q, q->black_influence, "%3.0f", 3, 1);
  }

  if (printmoyo & PRINTMOYO_PRINT_INFLUENCE) {
    fprintf(stderr, "influence regions (%s):\n", info_string);
    print_influence_areas(q);
  }
}

/* Print the numerical territory valuation. */
static void
print_influence_territory(struct influence_data *q, const char *info_string)
{
  fprintf(stderr, info_string);
  print_numeric_influence(q, q->territory_value, "%5.2f", 5, 0);
}


char black_stone[8][10] = { "X", " X", "  X", "  X ", "  X  ", "   X  ",
                         "   X   ", "    X   "};
char white_stone[8][10] = { "O", " O", "  O", "  O ", "  O  ", "   O  ",
                         "   O   ", "    O   "};
/* Print numeric influence values.
 * If draw_stones is not zero, then it denotes the lenght (in characters)
 * of the numeric output fields.
 */ 
static void
print_numeric_influence(struct influence_data *q,
			float values[BOARDMAX],
			const char *format,
			int draw_stones,
			int mark_epsilon)
{
  int i, j;
  for (i = 0; i < board_size; i++) {
    for (j = 0; j < board_size; j++) {
      int ii = POS(i, j);
      if (draw_stones && q->p[ii] == WHITE)
	fprintf(stderr, white_stone[draw_stones]);
      else if (draw_stones && q->p[ii] == BLACK)
	fprintf(stderr, black_stone[draw_stones]);
      else {
	if (mark_epsilon && values[ii] > 0.0 && values[ii] < 1.0)
	  fprintf(stderr, "eps");
	else
	  fprintf(stderr, format, values[ii]);
	fprintf(stderr, " ");
      }
    }
    fprintf(stderr, "\n");
  }
  fprintf(stderr, "\n");
}

/* Draw colored board illustrating territory, moyo, and area. */
static void
print_influence_areas(struct influence_data *q)
{
  int ii;
  start_draw_board();
  for (ii = BOARDMIN; ii < BOARDMAX; ii++)
    if (ON_BOARD(ii)) {
      int c = EMPTY;
      int color = GG_COLOR_BLACK;
      if (q->p[ii] == WHITE) {
	c = 'O';
	color = GG_COLOR_BLACK;
      }
      else if (q->p[ii] == BLACK) {
	c = 'X';
	color = GG_COLOR_BLACK;
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
