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
				    float values[MAX_BOARD][MAX_BOARD],
				    const char *format, int draw_stones,
				    int mark_epsilon);
static void print_influence_areas(struct influence_data *q);
 
static void value_territory(struct influence_data *q, int m, int n, int color);
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
static int influence_movei = -1;
static int influence_movej = -1;
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
static int debug_influence_i = -1;
static int debug_influence_j = -1;

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

#define code1(arg_di, arg_dj, arg_i, arg_j, arg_d) \
      if (q->p[arg_i][arg_j] == EMPTY \
	  && ((arg_di)*(i-m) + (arg_dj)*(j-n) > 0 \
	      || queue_start == 1)) { \
	permeability = permeability_array[i][j]; \
	if (arg_d) \
	  permeability *= gg_max(permeability_array[arg_i][j], \
			         permeability_array[i][arg_j]); \
	if (permeability == 0.0) \
	  continue; \
	damping = (arg_d) ? diagonal_attenuation : attenuation; \
	if (i == m && j == n) \
	  cos2phi = 1.0; \
	else { \
	  float a = (arg_di)*(i-m) + (arg_dj)*(j-n); \
	  float c = (arg_di)*(arg_di) + (arg_dj)*(arg_dj); \
	  gg_assert(a > 0.0); \
	  cos2phi = (a*a) / (b*c); \
	} \
	contribution = cos2phi * permeability * current_strength / damping; \
	if (contribution <= INFLUENCE_CUTOFF) { \
          if (permeability < 1.0 || q->w[arg_i][arg_j] != 0.0) \
	    continue; \
          else \
            contribution = 1.01 * INFLUENCE_CUTOFF; \
	} \
	if (q->w[arg_i][arg_j] == 0.0) { \
	  q->queuei[queue_end] = (arg_i); \
	  q->queuej[queue_end] = (arg_j); \
	  queue_end++; \
	} \
	q->w[arg_i][arg_j] += contribution; \
      }
#endif

static void
accumulate_influence(struct influence_data *q, int m, int n, int color)
{
  int i, j;
  int k;
#if !EXPLICIT_LOOP_UNROLLING
  int d;
#endif
  float damping;
  float b;
  float current_strength;
  float cos2phi;
  float permeability;
  float contribution;
  float attenuation;
  float diagonal_attenuation;
  float (*permeability_array)[MAX_BOARD];
  
  /* Clear the queue. */
  int queue_start = 0;
  int queue_end = 0;

  if (0)
    gprintf("Accumulating influence for %s at %m\n",
	    color_to_string(color), m, n);

  /* Attenuation only depends on the influence origin. */
  if (color == WHITE)
    attenuation = q->white_attenuation[m][n];
  else
    attenuation = q->black_attenuation[m][n];
  if (q->is_territorial_influence)
    diagonal_attenuation = attenuation * TERR_DIAGONAL_DAMPING;
  else
    diagonal_attenuation = attenuation * DIAGONAL_DAMPING;

  if (color == WHITE)
    permeability_array = q->white_permeability;
  else
    permeability_array = q->black_permeability;
    
  /* Put the influence origin on the stack. */
  if (color == WHITE)
    q->w[m][n] = q->white_strength[m][n];
  else
    q->w[m][n] = q->black_strength[m][n];
  q->queuei[queue_end] = m;
  q->queuej[queue_end] = n;
  queue_end++;

  /* Spread influence until the stack is empty. */
  while (queue_start < queue_end) {

    /* Pick first element in queue. */
    i = q->queuei[queue_start];
    j = q->queuej[queue_start];
    b = (i-m)*(i-m) + (j-n)*(j-n);
    if (0)
      gprintf("Picked %m from queue. w=%f start=%d end=%d\n",
	      i, j, q->w[i][j], queue_start, queue_end);
    current_strength = q->w[i][j];
    queue_start++;

#if !EXPLICIT_LOOP_UNROLLING
    /* Try to spread influence in each of the eight directions. */    
    for (d = 0; d < 8; d++) {
      int di = deltai[d];
      int dj = deltaj[d];

      /* Verify that (i+di, j+dj) is
       * 1. Inside the board.
       * 2. Not occupied.
       * 3. Directed outwards. For the origin all directions are outwards.
       */
      if (ON_BOARD2(i+di, j+dj)
	  && q->p[i+di][j+dj] == EMPTY
	  && (di*(i-m) + dj*(j-n) > 0
	      || queue_start == 1)) {

	/* Now compute the damping of the influence.
	 * First we have the permeability at the point we are
	 * spreading from. For diagonal movement we also take the
	 * permeability of the vertices we are "passing by" into
	 * account.
	 */
	permeability = permeability_array[i][j];
	if (d > 3) /* diagonal movement */
	  permeability *= gg_max(permeability_array[i+di][j],
				 permeability_array[i][j+dj]);

	if (permeability == 0.0)
	  continue;
	
	/* Then we have the distance dependent damping. */
	damping = 1.0;
	switch (d) {
	case 0: /* south */
	case 1: /* west */
	case 2: /* north */
	case 3: /* east */
	  damping = attenuation;
	  break;
	case 4: /* southwest */
	case 5: /* northwest */
	case 6: /* northeast */
	case 7: /* southeast */
	  damping = diagonal_attenuation;
	  break;
	}
	
	/* Finally direction dependent damping. */
	if (i == m && j == n)
	  cos2phi = 1.0;
	else {
	  float a = di*(i-m) + dj*(j-n);
	  float c = di*di + dj*dj;
	  gg_assert(a > 0.0);
	  cos2phi = (a*a) / (b*c);
	}

	contribution = cos2phi * permeability * current_strength / damping;

	/* Stop spreading influence if the contribution becomes too low. */
	if (contribution <= INFLUENCE_CUTOFF) {
          if (permeability < 1.0 || q->w[i+di][j+dj] != 0.0)
	    continue;
          else
            contribution = 1.01 * INFLUENCE_CUTOFF;
	}
	
	/* If no influence here before, add the point to the queue for
	 * further spreading.
	 */
	if (0)
	  gprintf("  Spreading %s influence from %m to %m, d=%d\n",
		  color_to_string(color), i, j, i+di, j+dj, d);
	if (q->w[i+di][j+dj] == 0.0) {
	  q->queuei[queue_end] = i + di;
	  q->queuej[queue_end] = j + dj;
	  queue_end++;
	}
	q->w[i+di][j+dj] += contribution;
      }
    }
#else
    if (i > 0)
      code1(-1, 0, i-1, j, 0);
    if (i < board_size-1)
      code1(1, 0, i+1, j, 0);
    if (j > 0)
      code1(0, -1, i, j-1, 0);
    if (j < board_size-1)
      code1(0, 1, i, j+1, 0);
    if (i > 0 && j > 0)
      code1(-1, -1, i-1, j-1, 1);
    if (i < board_size-1 && j > 0)
      code1(1, -1, i+1, j-1, 1);
    if (i < board_size-1 && j < board_size-1)
      code1(1, 1, i+1, j+1, 1);
    if (i > 0 && j < board_size-1)
      code1(-1, 1, i-1, j+1, 1);
 
#endif
  }
  
  /* Add the values in the working area to the accumulated influence
   * and simultaneously reset the working area. We know that all
   * influenced points were stored in the queue, so we just traverse
   * it.
   */
  for (k = 0; k < queue_end; k++) {
    i = q->queuei[k];
    j = q->queuej[k];

    if (color == WHITE) {
      if (q->w[i][j] > 1.01 * INFLUENCE_CUTOFF
	  || q->white_influence[i][j] == 0.0)
	q->white_influence[i][j] += q->w[i][j];
    }
    else {
      if (q->w[i][j] > 1.01 * INFLUENCE_CUTOFF
	  || q->black_influence[i][j] == 0.0)
	q->black_influence[i][j] += q->w[i][j];
    }
    
    q->w[i][j] = 0.0;
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
  int i, j;
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
  
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++) {
      int pos = POS(i, j);
      /* Initialize. */
      q->white_influence[i][j] = 0.0;
      q->black_influence[i][j] = 0.0;
      q->w[i][j] = 0.0;
      q->white_attenuation[i][j] = attenuation;
      q->black_attenuation[i][j] = attenuation;
      q->white_permeability[i][j] = 1.0;
      q->black_permeability[i][j] = 1.0;
      q->white_strength[i][j] = 0.0;
      q->black_strength[i][j] = 0.0;
      q->p[i][j] = board[pos];
      q->non_territory[i][j] = EMPTY;

      /* FIXME: Simplify the code below! */
      
      /* Dead stones (or critical ones for the color which will not
       * make the next move) count as empty space. However, influence
       * cannot flow through these for the owner of the stones.
       */
      if (IS_STONE(board[pos])) {
	if (saved_stones[pos] == INFLUENCE_CAPTURED_STONE
	    || (saved_stones[pos] == INFLUENCE_UNCHANGED_STONE
		&& ((worm[pos].attack_codes[0] != 0
		    && (OTHER_COLOR(q->p[i][j]) == color
			|| worm[pos].defend_codes[0] == 0))
		    || (q->dragons_known
			&& dragon[pos].id != -1
			&& (DRAGON2(pos).safety == DEAD
			    || DRAGON2(pos).safety == TACTICALLY_DEAD
			    || (DRAGON2(pos).safety == CRITICAL
				&& board[pos] == OTHER_COLOR(color))))))) {
	  if (q->p[i][j] == WHITE)
	    q->white_permeability[i][j] = 0.0;
	  else
	    q->black_permeability[i][j] = 0.0;
	  q->p[i][j] = EMPTY;
	}
	else if (saved_stones[pos] == INFLUENCE_SAVED_STONE
		 || !q->dragons_known
		 || dragon[pos].id == -1
		 || (DRAGON2(pos).safety != DEAD
		     && DRAGON2(pos).safety != TACTICALLY_DEAD
		     && DRAGON2(pos).safety != CRITICAL)
		 || (DRAGON2(pos).safety == CRITICAL
		     && board[pos] == color)) {
	  if (q->p[i][j] == WHITE)
	    q->black_permeability[i][j] = 0.0;
	  else
	    q->white_permeability[i][j] = 0.0;
	}

	/* We need to make an exception to the rules above for
         * INESSENTIAL stones. Instead of making the conditions above
         * still more complex we correct it here.
	 *
	 * If q->p[i][j] is allowed to be 0, the territory evaluation
	 * will think it's a prisoner for the opponent, and various
	 * territory corrections and interpolations will mess up.
	 */
	if (IS_STONE(board[pos])
	    && q->dragons_known
	    && dragon[pos].id != -1
	    && (DRAGON2(pos).safety == INESSENTIAL
	        || (worm[pos].inessential
		    && ((DRAGON2(pos).safety != DEAD
			 && DRAGON2(pos).safety != TACTICALLY_DEAD
		         && DRAGON2(pos).safety != CRITICAL)
		        || (DRAGON2(pos).safety == CRITICAL
		            && board[pos] == color)))) 
	    && q->p[i][j] == EMPTY)
	  q->p[i][j] = board[pos];
      }
      
      /* When evaluating influence after a move, the newly placed
       * stone will have the invalid dragon id -1.
       */
      if (IS_STONE(board[pos])) {
	if (!q->dragons_known || saved_stones[pos] == INFLUENCE_SAVED_STONE) {
	  if (q->p[i][j] == WHITE)
	    q->white_strength[i][j] = DEFAULT_STRENGTH;
	  else if (q->p[i][j] == BLACK)
	    q->black_strength[i][j] = DEFAULT_STRENGTH;
	}
	else if (saved_stones[pos] != INFLUENCE_CAPTURED_STONE) {
	  ASSERT1(dragon[pos].id != -1, pos);
	  if (q->p[i][j] == WHITE) {
	    if (color == BLACK && DRAGON2(pos).safety == CRITICAL)
	      q->white_strength[i][j] = 0.0;
	    else
	      q->white_strength[i][j] = (DEFAULT_STRENGTH
					 * strength_map[DRAGON2(pos).safety]);
	  }
	  else if (q->p[i][j] == BLACK) {
	    if (color == WHITE && DRAGON2(pos).safety == CRITICAL)
	      q->black_strength[i][j] = 0.0;
	    else
	      q->black_strength[i][j] = (DEFAULT_STRENGTH
					 * strength_map[DRAGON2(pos).safety]);
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
  int m = I(pos);
  int n = J(pos);
  
  if ((color & WHITE) && (q->white_strength[m][n] < strength)) {
    q->white_strength[m][n] = strength;
    q->white_attenuation[m][n] = attenuation;
  }
  
  if ((color & BLACK) && (q->black_strength[m][n] < strength)) {
    q->black_strength[m][n] = strength;
    q->black_attenuation[m][n] = attenuation;
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

/* Experimental influence: Comparison of intrusions datas, to sort them. */
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

/* Experimental influence: This function goes through the list of 
 * intrusion sources, and adds the intrusion as influence sources for color. 
 * The strength is corrected so that each stone's intrusions sources
 * can have total strength of at most 60%/100% of the strength of the stone.
 * (100% is if q=&followup_influence, 60% otherwise).
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
      source_strength = q->black_strength[I(source_pos)][J(source_pos)];
    }
    else {
      source_strength = q->white_strength[I(source_pos)][J(source_pos)];
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
  int pos;
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
      int x, y;
      int blocking_color;
      if (pattern->patn[k].att != ATT_comma
	  && (!q->is_territorial_influence || pattern->patn[k].att != ATT_not))
	break;  /* All commas are guaranteed to come first. */

      /* transform pattern real coordinate */
      TRANSFORM(pattern->patn[k].x, pattern->patn[k].y, &x, &y, ll);
      x += m;
      y += n;

      /* Territorial connection, making a barrier for opponent influence. */
      if (pattern->class & CLASS_D)
	blocking_color = color;
      else
	blocking_color = OTHER_COLOR(color);
      if ((blocking_color == WHITE
	   && q->black_permeability[x][y] != 0.0)
	  || (blocking_color == BLACK
	      && q->white_permeability[x][y] != 0.0)) {
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
	int x, y;
	TRANSFORM(pattern->patn[k].x, pattern->patn[k].y, &x, &y, ll);
	x += m;
	y += n;
	if (pattern->class & CLASS_E) {
	  if ((color == WHITE && q->white_strength[x][y] == 0.0)
	      || (color == BLACK && q->black_strength[x][y] == 0.0))
	    return; /* Match failed. */
	}
	/* FIXME: This test is probably not necessary any more. */
	else if (!(pattern->class & (CLASS_D | CLASS_B | CLASS_t))) {
	  if ((stackp == 0 && worm[POS(x, y)].attack_codes[0] != 0)
	      || attack(POS(x, y), NULL) != 0)
	    return; /* Match failed */
	}
	/* One test left for class B and t. */
	if ((pattern->class & (CLASS_B | CLASS_t))
	    && pattern->patn[k].att == ATT_O) {
	  if ((color == WHITE && q->white_strength[x][y] == 0.0)
	      || (color == BLACK && q->black_strength[x][y] == 0.0))
	    return; /* Match failed. */
	}
	
	if ((pattern->class & (CLASS_A | CLASS_t))
	    && pattern->patn[k].att == ATT_X) {
	  if ((color == BLACK && q->white_strength[x][y] == 0.0)
	      || (color == WHITE && q->black_strength[x][y] == 0.0))
	    return; /* Match failed. */
	}
	
	if (pattern->class & CLASS_D) {
	  gg_assert(pattern->patn[k].att == ATT_O);
	  if ((color == WHITE && q->white_strength[x][y] == 0.0)
	      || (color == BLACK && q->black_strength[x][y] == 0.0))
	    return; /* Match failed. */
	}
      }
    }
  }

  pos = AFFINE_TRANSFORM(pattern->movei, pattern->movej, ll, m, n);

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
      int x, y;
      TRANSFORM(pattern->patn[k].x, pattern->patn[k].y, &x, &y, ll);
      x += m;
      y += n;

      /* Territorial connection, making a barrier for opponent influence. */
      if (pattern->class & (CLASS_A | CLASS_D)) {
	int blocking_color;
	if (pattern->class & CLASS_D)
	  blocking_color = color;
	else
	  blocking_color = OTHER_COLOR(color);
	DEBUG(DEBUG_INFLUENCE, "  barrier for %s influence at %m\n",
	      color_to_string(OTHER_COLOR(blocking_color)), x, y);
	if (pattern->patn[k].att == ATT_comma) {
	  if (blocking_color == WHITE)
	    q->black_permeability[x][y] = 0.0;
	  else
	    q->white_permeability[x][y] = 0.0;
	}
	/* Weak barrier at !-marked points. */
	else {
	  if (blocking_color == WHITE)
	    q->black_permeability[x][y] *= 0.7;
	  else
	    q->white_permeability[x][y] *= 0.7;
	  
	}
      }
      
      /* Low intensity influence source for the color in turn to move. */
      if (pattern->class & CLASS_B) {
        if (q->is_territorial_influence)
          enter_intrusion_source(POS(m, n), POS(x, y), pattern->value,
	  		         TERR_DEFAULT_ATTENUATION, q);
        else
          add_influence_source(POS(x, y), color,
			       pattern->value, DEFAULT_ATTENUATION, q);
	DEBUG(DEBUG_INFLUENCE, "  intrusion at %m\n", x, y);
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
  int saved_stone_involved = 0;
  struct influence_data *q = data;
  UNUSED(color);
 
  /* We use only B  patterns in followup influence. */
  if (!(pattern->class & CLASS_B))
    return;

  /* We check first whether a saved stone is involved. */
  for (k = 0; k < pattern->patlen; ++k) /* match each point */
    if (pattern->patn[k].att == ATT_O) {
      /* transform pattern real coordinate */
      int x, y;
      TRANSFORM(pattern->patn[k].x, pattern->patn[k].y, &x, &y, ll);
      x += m;
      y += n;
      if (q->w[x][y] == MARKED)
        saved_stone_involved = 1;
    }
  
  if (!saved_stone_involved)
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
      int pos;
      pos = AFFINE_TRANSFORM(pattern->patn[k].x, pattern->patn[k].y,
			     ll, m, n);

      /* Low intensity influence source for the color in turn to move. */
      enter_intrusion_source(POS(m, n), pos, pattern->value,
			     TERR_DEFAULT_ATTENUATION, q);
      DEBUG(DEBUG_INFLUENCE, "  followup for %1m: intrusion at %1m\n",
            POS(m, n), pos);
    }
}

/* Called from actions for t patterns. Marks (pos) as not being
 * territory for (color).
 */
void
influence_mark_non_territory(int pos, int color)
{
  DEBUG(DEBUG_INFLUENCE, "  non-territory for %C at %1m\n", color, pos);
  current_influence->non_territory[I(pos)][J(pos)] |= color;
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
  int m, n;

  current_influence = q;
  matchpat(influence_callback, ANCHOR_COLOR, &influencepat_db, q, NULL);
  if (color != EMPTY)
    matchpat(influence_callback, color, &barrierspat_db, q, NULL);

  if (q->is_territorial_influence)
    add_marked_intrusions(q, color);

  /* When color == EMPTY, we introduce a weaker kind of barriers
   * manually instead of searching for patterns.
   */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++)
      if (IS_STONE(q->p[m][n])) {
	int k;
	for (k = 0; k < 8; k++) {
	  int dm = deltai[k];
	  int dn = deltaj[k];
	  if (ON_BOARD2(m+dm, n+dn) && q->p[m+dm][n+dn] == EMPTY) {
	    /* Reduce less diagonally. */
	    float reduction = (k < 4) ? 0.25 : 0.65;
	    if (q->p[m][n] == BLACK)
	      q->white_permeability[m+dm][n+dn] *= reduction;
	    else
	      q->black_permeability[m+dm][n+dn] *= reduction;
	  }
	}
      }
  
  /* It may happen that we have a low intensity influence source at a
   * blocked intersection (due to an intrusion). Reset the
   * permeability at this point.
   */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      if (q->p[m][n] == EMPTY && q->white_strength[m][n] > 0.0
	  && q->white_permeability[m][n] != 1.0) {
	DEBUG(DEBUG_INFLUENCE, "  black block removed from %m\n", m, n);
	q->white_permeability[m][n] = 1.0;
      }
      if (q->p[m][n] == EMPTY && q->black_strength[m][n] > 0.0
	  && q->black_permeability[m][n] != 1.0) {
	DEBUG(DEBUG_INFLUENCE, "  white block removed from %m\n", m, n);
	q->black_permeability[m][n] = 1.0;
      }
    }
}

/* Compute the influence values for both colors, after having made a
 * move for OTHER_COLOR(color) at (m, n). If these coordinates are -1
 * no move is made. In any case it's assumed that color is in turn to
 * move. (This affects the barrier patterns (class A, D) and intrusions
 * (class B).
 */
static void
compute_influence(struct influence_data *q, int color, int m, int n,
		  char no_influence[BOARDMAX], char saved_stones[BOARDMAX])
{
  int i, j;
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
  
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++) {
      if (q->white_strength[i][j] > 0.0)
	accumulate_influence(q, i, j, WHITE);
      if (q->black_strength[i][j] > 0.0)
	accumulate_influence(q, i, j, BLACK);
    }

  if (q->is_territorial_influence)
    value_territory(q, m, n, color);
  segment_influence(q);
  
  if (((q == &initial_influence || q == &initial_opposite_influence)
       && (printmoyo & PRINTMOYO_INITIAL_INFLUENCE))
      || (m == debug_influence_i
	  && n == debug_influence_j && m >= 0)) {
    if (q == &initial_opposite_influence)
      print_influence(q, (q->dragons_known ? "dragons_known, opposite, color"
			  : "dragons_unknown, opposite, color"));
    else
      print_influence(q, q->dragons_known ? "dragons_known"
	  		 : "dragons_unknown");
  }
}

/* Return the color of the territory at (m, n). If it's territory for
 * neither color, EMPTY is returned.
 * The definition of territory in terms of the influences is totally
 * ad hoc.
 */
static int
whose_territory(struct influence_data *q, int m, int n)
{
  float bi = q->black_influence[m][n];
  float wi = q->white_influence[m][n];

  ASSERT_ON_BOARD2(m, n);
  
  if (bi > 0.0 && wi == 0.0)
    return BLACK;

  if (wi > 0.0 && bi == 0.0)
    return WHITE;

  return EMPTY;
}

/* Return the color who has a moyo at (m, n). If neither color has a
 * moyo there, EMPTY is returned. The definition of moyo in terms of the
 * influences is totally ad hoc.
 */
static int
whose_moyo(struct influence_data *q, int m, int n)
{
  float bi = q->black_influence[m][n];
  float wi = q->white_influence[m][n];

  int territory_color = whose_territory(q, m, n);
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
whose_moyo_restricted(struct influence_data *q, int m, int n)
{
  float bi = q->black_influence[m][n];
  float wi = q->white_influence[m][n];
  int color;

  int territory_color = whose_territory(q, m, n);

  if (worm[POS(m, n)].attack_codes[0] != 0
      && worm[POS(m, n)].defend_codes[0] != 0)
    return EMPTY;
  
  /* default */
  if (territory_color != EMPTY)
    color = territory_color;
  else if ((bi > 10.0 * wi && bi > 10.0 && wi < 10.0) || bi > 25.0 * wi)
    color = BLACK;
  else if ((wi > 10.0 * bi && wi > 10.0 && bi < 10.0) || wi > 25.0 * bi)
    color = WHITE;
  else
    color = EMPTY;
  
  if (color == WHITE && (white_eye[POS(m, n)].type & INHIBIT_CONNECTION))
    return EMPTY;
  
  if (color == BLACK && (black_eye[POS(m, n)].type & INHIBIT_CONNECTION))
    return EMPTY;
  
  return color;
}


/* Return the color who has dominating influence ("area") at (m, n).
 * If neither color dominates the influence there, EMPTY is returned.
 * The definition of area in terms of the influences is totally ad
 * hoc.
 */
static int
whose_area(struct influence_data *q, int m, int n)
{
  float bi = q->black_influence[m][n];
  float wi = q->white_influence[m][n];

  int moyo_color = whose_moyo(q, m, n);
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
value_territory(struct influence_data *q, int m, int n, int color)
{
  int i, j;
  int dist_i, dist_j;
  float central;
  float first_guess[MAX_BOARD][MAX_BOARD];
  float ratio;
  float neighbor_val;
  int k;

  /* First loop: guess territory directly from influence. */
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++) {
      first_guess[i][j] = 0.0;

      if (q->p[i][j] == EMPTY) {
        float diff = 0.0;
        if (q->white_influence[i][j] + q->black_influence[i][j] > 0)
          diff = (q->white_influence[i][j] - q->black_influence[i][j])
                /(q->white_influence[i][j] + q->black_influence[i][j]);
        first_guess[i][j] = diff * diff * diff;

        dist_i = gg_min(i, board_size - i -1);
        dist_j = gg_min(j, board_size - j -1);
	if (dist_i > dist_j)
	  dist_i = gg_min(4, dist_i);
	else
	  dist_j = gg_min(4, dist_j);
	central = (float) 2 * gg_min(dist_i, dist_j) + dist_i + dist_j;
        ratio = gg_max(q->black_influence[i][j], q->white_influence[i][j])
                / gg_interpolate(&min_infl_for_territory, central);
        first_guess[i][j] *= gg_interpolate(&territory_correction, ratio);

	/* Dead stone, upgrade to territory. Notice that this is not
         * the point for a prisoner, which is added later. Instead
         * this is to make sure that the vertex is not regarded as
         * moyo or area. Also notice that the non-territory
         * degradation below may over-rule this decision.
	 */
	if (BOARD(i, j) == BLACK)
	  first_guess[i][j] = 1.0;
	else if (BOARD(i, j) == WHITE)
	  first_guess[i][j] = -1.0;
      }
      q->territory_value[i][j] = first_guess[i][j];
    }

  /* Second loop: Correct according to neighbour vertices. Each territory
   * value is degraded to the minimum value of its neighbors (unless this
   * neighbor has reduced permeability for the opponent's influence).
   */
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++) {
      /* Do not overrule dead stone territory above.
       * FIXME: This does not do what it claims to do. Correcting it
       * seems to break some tests, though.
       */
      if (q->p[i][j] == EMPTY) {
	/* Loop over all neighbors. */
        for (k = 0; k < 4; k++) {
          if (!ON_BOARD2(i+deltai[k],j+deltaj[k]))
            continue;
          if (q->territory_value[i][j] > 0.0) {
            /* White territory. */
            if (q->p[i+deltai[k]][j+deltaj[k]] != WHITE) {
	      neighbor_val =
		q->black_permeability[i+deltai[k]][j+deltaj[k]]
		  * first_guess[i+deltai[k]][j+deltaj[k]]
		+ (1 - q->black_permeability[i+deltai[k]][j+deltaj[k]])
		  * first_guess[i][j];
              q->territory_value[i][j]
                = gg_max(0, gg_min(q->territory_value[i][j], neighbor_val));
	    }
          }
          else {
            /* Black territory. */
            if (q->p[i+deltai[k]][j+deltaj[k]] != BLACK) {
	      neighbor_val =
		q->white_permeability[i+deltai[k]][j+deltaj[k]]
		  * first_guess[i+deltai[k]][j+deltaj[k]]
		+ (1 - q->white_permeability[i+deltai[k]][j+deltaj[k]])
		  * first_guess[i][j];
              q->territory_value[i][j]
                = gg_min(0, gg_max(q->territory_value[i][j], neighbor_val));
	    }
          }
        }
      }
    }

  /* Third loop: Nonterritory patterns, points for prisoners. */
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++) {
      if (q->p[i][j] == EMPTY) {
	/* If marked as non-territory for the color currently owning
         * it, reset the territory value.
	 */
	if (q->territory_value[i][j] > 0.0
	    && (q->non_territory[i][j] & WHITE))
	  q->territory_value[i][j] = 0.0;

	if (q->territory_value[i][j] < 0.0
	    && (q->non_territory[i][j] & BLACK))
	  q->territory_value[i][j] = 0.0;
	
	/* Dead stone, add one to the territory value. */
	if (BOARD(i, j) == BLACK)
	  q->territory_value[i][j] += 1.0;
	else if (BOARD(i, j) == WHITE)
	  q->territory_value[i][j] -= 1.0;
      }
    }

  /* Final correction. Never count the last played move as territory
   * for the color playing it. Ideally this should never happen, but
   * currently we need this workaround.
   */
  if (ON_BOARD2(m, n)) {
    if (color == BLACK && q->territory_value[m][n] < 0.0)
      q->territory_value[m][n] = 0.0;
    else if (color == WHITE && q->territory_value[m][n] > 0.0)
      q->territory_value[m][n] = 0.0;
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
	       int type, int (*segmentation)[MAX_BOARD])
{
  int m, n;
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      if (q->w[m][n] == UNMARKED && region_owner(q, m, n) != EMPTY) {
	/* Found an unlabelled intersection. Use flood filling to find
         * the rest of the region.
	 */
	int size = 0;
	float terr_val = 0.0;
	int queue_start = 0;
	int queue_end = 1;
	int color = region_owner(q, m, n);
	q->number_of_regions++;
	q->w[m][n] = MARKED;
	q->queuei[0] = m;
	q->queuej[0] = n;
	while (queue_start < queue_end) {
	  int i = q->queuei[queue_start];
	  int j = q->queuej[queue_start];
	  int k;
	  queue_start++;
	  if (q->p[i][j] != color) {
	    size++;
	    if (q->is_territorial_influence)
	      terr_val += gg_abs(q->territory_value[i][j]);
	  }
	  segmentation[i][j] = q->number_of_regions;
	  for (k = 0; k < 4; k++) {
	    int di = deltai[k];
	    int dj = deltaj[k];
	    if (ON_BOARD2(i+di, j+dj)
		&& q->w[i+di][j+dj] == UNMARKED
		&& region_owner(q, i+di, j+dj) == color) {
	      q->queuei[queue_end] = i+di;
	      q->queuej[queue_end] = j+dj;
	      queue_end++;
	      q->w[i+di][j+dj] = MARKED;
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
	  gprintf("Region %d of type %d (color %s) at %m. Size %d\n",
		  q->number_of_regions, q->region_type[q->number_of_regions],
		  color_to_string(color), m, n, size);
      }
    }
  /* Reset the working area w. */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++)
      q->w[m][n] = UNMARKED;
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
  int i, j;
  q->number_of_regions = 0;
  q->region_type[0] = 0;
  q->region_size[0] = 0;
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++) {
      q->territory_segmentation[i][j] = 0;
      q->moyo_segmentation[i][j] = 0;
      q->area_segmentation[i][j] = 0;
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
  if ((q->region_type[q->moyo_segmentation[I(pos)][J(pos)]] == WHITE_MOYO)
      ^ (color == BLACK))
    result1 = q->region_size[q->moyo_segmentation[I(pos)][J(pos)]];

  q = &initial_opposite_influence;

  /* Does the color match. */
  if ((q->region_type[q->moyo_segmentation[I(pos)][J(pos)]] == WHITE_MOYO)
      ^ (color == BLACK))
    result2 = q->region_size[q->moyo_segmentation[I(pos)][J(pos)]];

  return gg_min(result1, result2);
}

/* Export the moyo segmentation. If (opposite) is true, then
 * initial_opposite_influence is used, otherwise initial_influence.
 */
void
influence_get_moyo_segmentation(int opposite, struct moyo_data *moyos)
{
  int m, n;
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
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      if (q->moyo_segmentation[m][n] != 0) {
        min_moyo_id = gg_min(min_moyo_id, q->moyo_segmentation[m][n]);
        max_moyo_id = gg_max(max_moyo_id, q->moyo_segmentation[m][n]);
      }
    }
  moyos->number = max_moyo_id - min_moyo_id + 1;

  /* Export segmentation. */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      if (q->moyo_segmentation[m][n] != 0) {
        moyos->segmentation[POS(m, n)]
	  = q->moyo_segmentation[m][n] - min_moyo_id + 1;
      }
      else
        moyos->segmentation[POS(m, n)] = 0;
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

/* Export the territory valuation at an intersection from initial_influence;
 * it is given from (color)'s point of view.
 */
float
influence_initial_territory(int pos, int color)
{
  if (color == WHITE)
    return initial_influence.territory_value[I(pos)][J(pos)];
  else
    return -initial_influence.territory_value[I(pos)][J(pos)];
}

/* Compute the influence before a move has been made, which can
 * later be compared to the influence after a move. Assume that
 * the other color is in turn to move.
 */
void
compute_initial_influence(int color, int dragons_known)
{
  int i, j;

  initial_influence.dragons_known = dragons_known;
  initial_influence.is_territorial_influence = dragons_known;
  initial_opposite_influence.dragons_known = dragons_known;
  initial_opposite_influence.is_territorial_influence = dragons_known;

  decrease_depth_values();

  compute_influence(&initial_influence, OTHER_COLOR(color), -1, -1,
		    NULL, NULL);
  if (dragons_known) {
    if ((printmoyo & PRINTMOYO_VALUE_TERRITORY)
	&& (printmoyo & PRINTMOYO_INITIAL_INFLUENCE))
      print_influence_territory(&initial_influence,
				"territory (initial influence):\n");
  }

  compute_influence(&initial_opposite_influence, color, -1, -1, NULL, NULL);
  /* Invalidate information in move_influence. */
  influence_movei = -1;
  influence_movej = -1;
  influence_color = EMPTY;
  /* Clear delta_territory cache. */
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++) {
      delta_territory_cache[POS(i, j)] = NOT_COMPUTED;
      followup_territory_cache[POS(i, j)] = NOT_COMPUTED;
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
compute_followup_influence(int m, int n, int color,
                           char saved_stones[BOARDMAX])
{
  int i, j;
  int ii;
  int k;
  char goal[BOARDMAX];

  UNUSED(m);
  UNUSED(n);  
  memcpy(&followup_influence, &move_influence, sizeof(move_influence));
 
  /* We mark the saved stones and their neighbors in the goal array
   * and in q->w.
   */
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++) {
      goal[POS(i,j)] = 0;
      followup_influence.w[i][j] = UNMARKED;
    }
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++)
      if (saved_stones[POS(i,j)]) {
	ii = POS(i, j);
        goal[ii] = 1;
	followup_influence.w[i][j] = MARKED;
	for (k = 0; k < 8; k++) 
	  if (board[ii] == board[ii+delta[k]])
	    goal[ii+delta[k]] = 1;
      }

  followup_influence.intrusion_counter = 0;

  current_influence = &followup_influence;
  /* Match B patterns for saved stones. */
  matchpat_goal_anchor(followup_influence_callback, color, &barrierspat_db, 
           	       &followup_influence, goal, 1);

  /* Reset the working area w. */
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++)
      followup_influence.w[i][j] = UNMARKED;
 
  /* Now add the intrusions. */
  add_marked_intrusions(&followup_influence, color);

  /* It may happen that we have a low intensity influence source at a
   * blocked intersection (due to an intrusion). Reset the
   * permeability at this point.
   */
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++) {
      if (followup_influence.p[i][j] == EMPTY
          && followup_influence.white_strength[i][j] > 0.0
	  && followup_influence.white_permeability[i][j] != 1.0) {
	DEBUG(DEBUG_INFLUENCE, "  black block removed from %m\n", i, j);
	followup_influence.white_permeability[i][j] = 1.0;
      }
      if (followup_influence.p[i][j] == EMPTY
          && followup_influence.black_strength[i][j] > 0.0
	  && followup_influence.black_permeability[i][j] != 1.0) {
	DEBUG(DEBUG_INFLUENCE, "  white block removed from %m\n", i, j);
	followup_influence.black_permeability[i][j] = 1.0;
      }
    }
  
  /* Spread influence for new influence sources. */
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++)
      if ((color == BLACK
          && followup_influence.black_strength[i][j]
             > move_influence.black_strength[i][j])
          || (color == WHITE
          && followup_influence.white_strength[i][j]
             > move_influence.white_strength[i][j]))
        accumulate_influence(&followup_influence, i, j, color);

  value_territory(&followup_influence, m, n, color);
}

/* Let color play at (m, n) and compute the influence after this move,
 * assuming that the other color is in turn to move next.
 */
static void
compute_move_influence(int m, int n, int color,
		       char saved_stones[BOARDMAX])
{
  /* Don't recompute if we already have the current values stored. */
  if (influence_movei == m
      && influence_movej == n
      && influence_color == color)
    return;

  move_influence.dragons_known = 1;
  move_influence.is_territorial_influence = 1;

  if (tryko(POS(m, n), color, "compute_move_influence", EMPTY, NO_MOVE)) {
    compute_influence(&move_influence, OTHER_COLOR(color), m, n,
		      NULL, saved_stones);
    compute_followup_influence(m, n, color, saved_stones);
    popgo();

    if (m == debug_influence_i
	&& n == debug_influence_j && m >= 0) {
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
    gprintf("Computing influence for illegal move %m (move number %d)\n",
	    m, n, movenum+1);
    return;
  }

  influence_movei = m;
  influence_movej = n;
  influence_color = color;
}


/* Assume that the stones marked by the goal array do not generate
 * influence and compute influence. Influence based escape values are
 * returned in the escape_value array.  
 */

void
compute_escape_influence(char goal[BOARDMAX], int color,
			 int escape_value[BOARDMAX],
			 int dragons_known)
{
  int i, j;
  int k;
  int ii;

  /* IMPORTANT: This caching relies on the fact that the goal
   * parameter currently is not used.
   */
  static int cached_board[BOARDMAX];
  static int escape_values[BOARDMAX][4];
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
  compute_influence(&escape_influence, OTHER_COLOR(color), -1, -1,
		    goal, NULL);
  increase_depth_values();
  
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++) {
      ii = POS(i, j);

      if (whose_moyo(&escape_influence, i, j) == color)
       escape_value[ii] = 4;
      else if (whose_area(&escape_influence, i, j) == color)
       escape_value[ii] = 2;
      else if (whose_area(&escape_influence, i, j) == EMPTY)
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
  return whose_territory(&initial_influence, I(pos), J(pos));
}

/* Return the color who has moyo at pos, or EMPTY. */
int
influence_moyo_color(int pos)
{
  return whose_moyo(&initial_influence, I(pos), J(pos));
}

/* Return the color who has moyo at pos, or EMPTY, using influence
 * computed with the opposite color to move.
 */
int
influence_moyo_color_opposite(int pos)
{
  return whose_moyo(&initial_opposite_influence, I(pos), J(pos));
}

/* Return the color who has area at pos, or EMPTY. */
int
influence_area_color(int pos)
{
  return whose_area(&initial_influence, I(pos), J(pos));
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
  int i, j;
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

  compute_move_influence(I(pos), J(pos), color, saved_stones);

  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++) {
      float new_value = move_influence.territory_value[i][j];
      float old_value = initial_influence.territory_value[i][j];
      float followup_value = followup_influence.territory_value[i][j];
      /* Negate values if we are black. */
      if (color == BLACK) {
	new_value = -new_value;
	old_value = -old_value;
	followup_value = -followup_value;
      }
      
      if (new_value - old_value > 0.02
          || old_value - new_value > 0.02)
	DEBUG(DEBUG_TERRITORY, "  %1m:   - %m territory change %f (%f -> %f)\n",
	      pos, i, j, new_value - old_value, old_value, new_value);
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
  int i, j;

  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++)
      if (BOARD(i, j) == EMPTY) {
	if (whose_territory(&initial_influence, i, j) == BLACK) {
	  if (DEBUG_INFLUENCE_SCORE)
	    gprintf("%m black 1t\n", i, j);
	  black_territory++;
	}
	else if (whose_territory(&initial_influence, i, j) == WHITE) {
	  if (DEBUG_INFLUENCE_SCORE)
	    gprintf("%m white 1t\n", i, j);
	  white_territory++;
	}
	else if (whose_moyo(&initial_influence, i, j) == BLACK) {
	  if (DEBUG_INFLUENCE_SCORE)
	    gprintf("%m black 1m\n", i, j);
	  black_moyo++;
	}
	else if (whose_moyo(&initial_influence, i, j) == WHITE) {
	  if (DEBUG_INFLUENCE_SCORE)
	    gprintf("%m white 1m\n", i, j);
	  white_moyo++;
	}
	else if (whose_area(&initial_influence, i, j) == BLACK) {
	  if (DEBUG_INFLUENCE_SCORE)
	    gprintf("%m black 1a\n", i, j);
	  black_area++;
	}
	else if (whose_area(&initial_influence, i, j) == WHITE) {
	  if (DEBUG_INFLUENCE_SCORE)
	    gprintf("%m white 1a\n", i, j);
	  white_area++;
	}
      }
      else if (BOARD(i, j) == BLACK
	       && initial_influence.black_strength[i][j] == 0) {
	if (DEBUG_INFLUENCE_SCORE)
	  gprintf("%m white 2t\n", i, j);
	white_territory += 2;
      }
      else if (BOARD(i, j) == WHITE
	       && initial_influence.white_strength[i][j] == 0) {
	if (DEBUG_INFLUENCE_SCORE)
	  gprintf("%m black 2t\n", i, j);
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
  debug_influence_i = i;
  debug_influence_j = j;
}

/* Copy and encode influence data. */
static void
retrieve_influence(struct influence_data *q,
		   float white_influence[BOARDMAX],
		   float black_influence[BOARDMAX],
		   int influence_regions[BOARDMAX])
{
  int m, n;
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      white_influence[POS(m, n)] = q->white_influence[m][n];
      black_influence[POS(m, n)] = q->black_influence[m][n];
      if (whose_territory(q, m, n) == WHITE)
	influence_regions[POS(m, n)] = 3;
      else if (whose_territory(q, m, n) == BLACK)
	influence_regions[POS(m, n)] = -3;
      else if (whose_moyo(q, m, n) == WHITE)
	influence_regions[POS(m, n)] = 2;
      else if (whose_moyo(q, m, n) == BLACK)
	influence_regions[POS(m, n)] = -2;
      else if (whose_area(q, m, n) == WHITE)
	influence_regions[POS(m, n)] = 1;
      else if (whose_area(q, m, n) == BLACK)
	influence_regions[POS(m, n)] = -1;
      else
	influence_regions[POS(m, n)] = 0;
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
  compute_move_influence(I(move), J(move), color, saved_stones);
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
  compute_move_influence(I(pos), J(pos), color, saved_stones);
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
			float values[MAX_BOARD][MAX_BOARD],
			const char *format,
			int draw_stones,
			int mark_epsilon)
{
  int i, j;
  for (i = 0; i < board_size; i++) {
    for (j = 0; j < board_size; j++) {
      if (draw_stones && q->p[i][j] == WHITE)
	fprintf(stderr, white_stone[draw_stones]);
      else if (draw_stones && q->p[i][j] == BLACK)
	fprintf(stderr, black_stone[draw_stones]);
      else {
	if (mark_epsilon && values[i][j] > 0.0 && values[i][j] < 1.0)
	  fprintf(stderr, "eps");
	else
	  fprintf(stderr, format, values[i][j]);
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
  int i, j;
  start_draw_board();
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++) {
      int c = EMPTY;
      int color = GG_COLOR_BLACK;
      if (q->p[i][j] == WHITE) {
	c = 'O';
	color = GG_COLOR_BLACK;
      }
      else if (q->p[i][j] == BLACK) {
	c = 'X';
	color = GG_COLOR_BLACK;
      }
      else if (whose_territory(q, i, j) == WHITE) {
	c = 'o';
	color = GG_COLOR_CYAN;
      }
      else if (whose_territory(q, i, j) == BLACK) {
	c = 'x';
	color = GG_COLOR_CYAN;
      }
      else if (whose_moyo(q, i, j) == WHITE) {
	c = 'o';
	color = GG_COLOR_YELLOW;
      }
      else if (whose_moyo(q, i, j) == BLACK) {
	c = 'x';
	color = GG_COLOR_YELLOW;
      }
      else if (whose_area(q, i, j) == WHITE) {
	c = 'o';
	color = GG_COLOR_RED;
      }
      else if (whose_area(q, i, j) == BLACK) {
	c = 'x';
	color = GG_COLOR_RED;
      }
      draw_color_char(i, j, c, color);
    }
  end_draw_board();
}


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
