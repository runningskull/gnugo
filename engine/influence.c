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




#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "liberty.h"
#include "influence.h"
#include "patterns.h"

static void value_territory(struct influence_data *q);
static void add_influence_source(int pos, int color, float strength,
                                 float attenuation,
                                 struct influence_data *q);
static void segment_influence(struct influence_data *q);
static void print_influence(struct influence_data *q, int dragons_known);
static void print_numeric_influence(struct influence_data *q,
				    float values[MAX_BOARD][MAX_BOARD],
				    const char *format, int draw_stones,
				    int mark_epsilon);
static void print_influence_areas(struct influence_data *q);

/* Influence computed for the initial position, i.e. before making
 * some move.
 */
static struct influence_data initial_influence;
static struct influence_data initial_opposite_influence;

/* Influence computed after some move has been made. */
static struct influence_data move_influence;

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
	if (i==m && j==n) \
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
      code1( 1, 0, i+1, j, 0);
    if (j > 0)
      code1( 0,-1, i, j-1, 0);
    if (j < board_size-1)
      code1( 0, 1, i, j+1, 0);
    if (i > 0 && j > 0)
      code1(-1,-1, i-1, j-1, 1);
    if (i < board_size-1 && j > 0)
      code1( 1,-1, i+1, j-1, 1);
    if (i < board_size-1 && j < board_size-1)
      code1( 1, 1, i+1, j+1, 1);
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
init_influence(struct influence_data *q, int color, int dragons_known,
	       char saved_stones[BOARDMAX])
{
  int i, j;
  float attenuation;

  gg_assert(saved_stones != NULL);
  
  if (q != &escape_influence) {
    q->color_to_move = color;
    attenuation = DEFAULT_ATTENUATION;
  }
  else {
    q->color_to_move = EMPTY;
    attenuation = 2 * DEFAULT_ATTENUATION;
  }
  
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
		    || (dragons_known
			&& dragon[pos].id != -1
			&& (DRAGON2(pos).safety == DEAD
			    || (DRAGON2(pos).safety == CRITICAL
				&& board[pos] == OTHER_COLOR(color))))))) {
	  if (q->p[i][j] == WHITE)
	    q->white_permeability[i][j] = 0.0;
	  else
	    q->black_permeability[i][j] = 0.0;
	  q->p[i][j] = EMPTY;
	}
	else if (saved_stones[pos] == INFLUENCE_SAVED_STONE
		 || !dragons_known
		 || dragon[pos].id == -1
		 || (DRAGON2(pos).safety != DEAD
		     && DRAGON2(pos).safety != CRITICAL)
		 || (DRAGON2(pos).safety == CRITICAL
		     && board[pos] == color)) {
	  if (q->p[i][j] == WHITE)
	    q->black_permeability[i][j] = 0.0;
	  else
	    q->white_permeability[i][j] = 0.0;
	}
      }
      
      /* When evaluating influence after a move, the newly placed
       * stone will have the invalid dragon id -1.
       */
      if (IS_STONE(board[pos])) {
	if (!dragons_known || saved_stones[pos] == INFLUENCE_SAVED_STONE) {
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
void
add_influence_source(int pos, int color, float strength, float attenuation,
                     struct influence_data *q)
{
  if ((color & WHITE) && (q->white_strength[I(pos)][J(pos)] < strength)) {
    q->white_strength[I(pos)][J(pos)] = strength;
    q->white_attenuation[I(pos)][J(pos)] = attenuation;
  }
  if ((color & BLACK) && (q->black_strength[I(pos)][J(pos)] < strength)) {
    q->black_strength[I(pos)][J(pos)] = strength;
    q->black_attenuation[I(pos)][J(pos)] = attenuation;
  }
}

/* Callback for the matched patterns in influence.db and barriers.db.
 * The pattern classes used here are:
 * A - Barrier pattern, where O plays first and X tries to block influence.
 * D - Barrier pattern, where O plays first and O tries to block influence.
 * B - Intrusion patterns, adding a low intensity influence source.
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
  int ti, tj;
  int k;
  struct influence_data *q = data;
  
  /* Loop through pattern elements to see if an A or D pattern
   * can possibly have any effect. If not we can skip evaluating
   * constraint and/or helper. */
  if (pattern->class & (CLASS_A | CLASS_D)) {
    int something_to_do = 0;
    for (k = 0; k < pattern->patlen; ++k) { /* match each point */
      int x, y;
      int blocking_color;
      if (pattern->patn[k].att != ATT_comma)
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

  TRANSFORM(pattern->movei, pattern->movej, &ti, &tj, ll);
  ti += m;
  tj += n;

  /* If the pattern has a constraint, call the autohelper to see
   * if the pattern must be rejected.
   */
  if (pattern->autohelper_flag & HAVE_CONSTRAINT) {
    if (!pattern->autohelper(pattern, ll, POS(ti, tj), color, 0))
      return;
  }

  /* If the pattern has a helper, call it to see if the pattern must
   * be rejected.
   */
  if (pattern->helper) {
    if (!pattern->helper(pattern, ll, POS(ti, tj), color)) {
      DEBUG(DEBUG_INFLUENCE,
	    "Influence pattern %s+%d rejected by helper at %1m\n",
	    pattern->name, ll, POS(ti, tj));
      return;
    }
  }

  DEBUG(DEBUG_INFLUENCE, "influence pattern '%s'+%d matched at %1m\n",
	pattern->name, ll, POS(m, n));

  /* For t patterns, everything happens in the action. */
  if ((pattern->class & CLASS_t)
      && (pattern->autohelper_flag & HAVE_ACTION)) {
    pattern->autohelper(pattern, ll, POS(ti, tj), color, 1);
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
      add_influence_source(POS(ti, tj), this_color,
          20 * pattern->value, 1.5, q);
    else
      add_influence_source(POS(ti, tj), this_color,
          pattern->value, 1.5, q);

    DEBUG(DEBUG_INFLUENCE,
	  "  low intensity influence source at %m, strength %f, color %C\n",
	  ti, tj, pattern->value, this_color);
    return;
  }
  
  /* For E patterns, add a new influence source of the same color and
   * pattern defined strength at *.
   */
  if (pattern->class & CLASS_E) {
    add_influence_source(POS(ti, tj), color,
        pattern->value, DEFAULT_ATTENUATION, q);
    DEBUG(DEBUG_INFLUENCE,
	  "  extra %C source at %m, strength %f\n", color,
	  ti, tj, pattern->value);
    return;
  }
  
  /* Loop through pattern elements and perform necessary actions
   * for A, D, B, and t patterns.
   */
  for (k = 0; k < pattern->patlen; ++k) { /* match each point */
    if ((   (pattern->class & (CLASS_D | CLASS_A))
	    && pattern->patn[k].att == ATT_comma)
	|| ((pattern->class & CLASS_B)
	    && pattern->patn[k].att == ATT_not)) {
      /* transform pattern real coordinate */
      int x, y;
      TRANSFORM(pattern->patn[k].x,pattern->patn[k].y,&x,&y,ll);
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
	if (blocking_color == WHITE)
	  q->black_permeability[x][y] = 0.0;
	else
	  q->white_permeability[x][y] = 0.0;
      }
      
      /* Low intensity influence source for the color in turn to move. */
      if (pattern->class & CLASS_B) {
        add_influence_source(POS(x, y), color,
            pattern->value, DEFAULT_ATTENUATION, q);
	DEBUG(DEBUG_INFLUENCE, "  intrusion at %m\n", x, y);
      }
    }
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

/* Match the patterns in influence.db and barriers.db in order to add
 * influence barriers, add extra influence sources at possible
 * invasion and intrusion points, and add extra influence induced by
 * strong positions.
 */
static void
find_influence_patterns(struct influence_data *q, int color)
{
  int m, n;

  current_influence = q;
  matchpat(influence_callback, ANCHOR_COLOR, &influencepat_db, q, NULL);
  if (color != EMPTY)
    matchpat(influence_callback, color, &barrierspat_db, q, NULL);

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
	    float reduction = (k < 4) ? 0.25 : 0.5;
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
 * move.
 */
static void
compute_influence(struct influence_data *q, int color, int m, int n,
		  int dragons_known, char no_influence[BOARDMAX],
		  char saved_stones[BOARDMAX])
{
  int i, j;
  char dummy_saved_stones[BOARDMAX];
  if (saved_stones != NULL)
    init_influence(q, color, dragons_known, saved_stones);
  else {
    memset(dummy_saved_stones, 0, sizeof(dummy_saved_stones));
    init_influence(q, color, dragons_known, dummy_saved_stones);
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
  segment_influence(q);
  /* FIXME: The "board_size - 19" stuff below is an ugly workaround for a bug
   *        in main.c
   */
  if ((q == &initial_influence
       && (printmoyo & PRINTMOYO_INITIAL_INFLUENCE))
      || (m == (board_size - 19) + debug_influence_i
	  && n == debug_influence_j && m >= 0))
    print_influence(q, dragons_known);
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

/* Give territorial value to each vertex.
 *
 * A vertex with a lively stone has territorial value of 0.
 * For other vertices the value is
 * 1 if the vertex is classified as territory.
 * 0.35 if it is classified as moyo
 * 0.13 if it is classified as area
 * 0.0 if it is classified as neutral
 * One point is added for a vertex with a dead stone.
 * Furthermore black points have a negative sign.
 *
 * The results are written to the territory_value[][] array of the
 * influence data.
 */
static void
value_territory(struct influence_data *q)
{
  int i, j;
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++) {
      q->territory_value[i][j] = 0.0;

      if (q->p[i][j] == EMPTY
	  || (q->black_strength[i][j] == 0 && q->white_strength[i][j] == 0)) {
	int owner = whose_territory(q, i, j);
	if (owner == BLACK)
	  q->territory_value[i][j] = -1.0;
	else if (owner == WHITE)
	  q->territory_value[i][j] = 1.0;
	else {
	  owner = whose_moyo(q, i, j);
	  if (owner == BLACK)
	    q->territory_value[i][j] = -0.35;
	  else if (owner == WHITE)
	    q->territory_value[i][j] = 0.35;
	  else {
           owner = whose_area(q, i, j);
	    if (owner == BLACK)
	      q->territory_value[i][j] = -0.13;
	    else if (owner == WHITE)
	      q->territory_value[i][j] = 0.13;
	  }
	}

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
	  if (q->p[i][j] != color)
	    size++;
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

/* Return the size of the moyo around (m, n).
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
influence_get_moyo_segmentation(int opposite, 
                                struct moyo_data *moyos)
{
  int m, n;
  int pos;
  int min_moyo_id;
  int max_moyo_id;
  int i;
  struct influence_data *q;

  min_moyo_id = MAX_REGIONS;
  max_moyo_id = 0;

  if (opposite) {
    q = &initial_opposite_influence;
  }
  else {
    q = &initial_influence;
  };
  /* Find out range of region ids used by moyos. */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      pos = POS(m, n);
      if (q->moyo_segmentation[I(pos)][J(pos)] != 0) {
        min_moyo_id = gg_min(min_moyo_id,
                    q->moyo_segmentation[I(pos)][J(pos)]);
        max_moyo_id = gg_max(max_moyo_id,
                    q->moyo_segmentation[I(pos)][J(pos)]);
      }
    }
  moyos->number = max_moyo_id - min_moyo_id + 1;

  /* Export segmentation. */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      pos = POS(m, n);
      if (q->moyo_segmentation[I(pos)][J(pos)] != 0) {
        moyos->segmentation[pos]
             = q->moyo_segmentation[I(pos)][J(pos)] - min_moyo_id + 1;
      }
      else {
        moyos->segmentation[pos] = 0;
      }
    }
  /* Export size and owner info. */
  for (i = min_moyo_id; i <= max_moyo_id; i++) {
    moyos->size[i - min_moyo_id + 1] = q->region_size[i];
    if (q->region_type[i] & BLACK_REGION) {
      moyos->owner[i - min_moyo_id  +1] = BLACK;
    }
    else {
      moyos->owner[i - min_moyo_id  +1] = WHITE;
    }
  }
}

/* Compute the influence before a move has been made, which can
 * later be compared to the influence after a move. Assume that
 * the other color is in turn to move.
 */
void
compute_initial_influence(int color, int dragons_known)
{
  int i, j;
  compute_influence(&initial_influence, OTHER_COLOR(color), -1, -1,
		    dragons_known, NULL, NULL);
  value_territory(&initial_influence);
  compute_influence(&initial_opposite_influence, color, -1, -1,
		    dragons_known, NULL, NULL);
  /* Invalidate information in move_influence. */
  influence_movei = -1;
  influence_movej = -1;
  influence_color = EMPTY;
  /* Clear delta_territory cache. */
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++)
      delta_territory_cache[POS(i, j)] = NOT_COMPUTED;
}

/* Redo the segmentation of the initial influence. */
void
resegment_initial_influence()
{
  segment_influence(&initial_influence);
  segment_influence(&initial_opposite_influence);
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

  if (tryko(POS(m, n), color, "compute_move_influence", EMPTY, NO_MOVE)) {
    increase_depth_values();
    compute_influence(&move_influence, OTHER_COLOR(color), m, n, 1,
		      NULL, saved_stones);
    decrease_depth_values();
    popgo();
    value_territory(&move_influence);
  }
  else {
    gprintf("Computing influence for illegal move %m (move number %d)\n",
	    m, n, movenum);
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
  int ii;

  compute_influence(&escape_influence, OTHER_COLOR(color), -1, -1,
		    dragons_known, goal, NULL);
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
			    "%3.0f", 1, 1);
    print_numeric_influence(&escape_influence,
			    escape_influence.black_influence,
			    "%3.0f", 1, 1);
  }    
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

/* Return the color who has area at pos, or EMPTY. */
int
influence_area_color(int pos)
{
  return whose_area(&initial_influence, I(pos), J(pos));
}

/* Compute the difference in territory made by a move by color at (pos).
 * This also includes the changes in moyo and area.
 */
float
influence_delta_territory(int pos, int color, char saved_stones[BOARDMAX])
{
  int i, j;
  float delta = 0.0;
  if (delta_territory_cache[pos] != NOT_COMPUTED)
    return delta_territory_cache[pos];
  if (0)
    gprintf("influence_delta_territory for %1m %s = ", pos,
	    color_to_string(color));
  compute_move_influence(I(pos), J(pos), color, saved_stones);

  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++) {
      float new_value = move_influence.territory_value[i][j];
      float old_value = initial_influence.territory_value[i][j];
      /* Negate values if we are black. */
      if (color == BLACK) {
	new_value = -new_value;
	old_value = -old_value;
      }
      
      if (new_value != old_value) {
	DEBUG(DEBUG_TERRITORY, "  - %m territory change %f (%f -> %f)\n",
		i, j, new_value - old_value, old_value, new_value);
	delta += new_value - old_value;
      }
    }
  
  if (0)
    gprintf("%f\n", delta);
  delta_territory_cache[pos] = delta;
  
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
  print_influence(&initial_influence, dragons_known);
}

/* Compute influence after doing a move and print it. Notice that it's
 * assumed that the printmoyo global tells what information to print.
 */
void
print_move_influence(int pos, int color,
		     char saved_stones[BOARDMAX])
{
  compute_move_influence(I(pos), J(pos), color, saved_stones);
  print_influence(&move_influence, 1);
}

/* Print influence for debugging purposes. */
static void
print_influence(struct influence_data *q, int dragons_known)
{
  if (printmoyo & PRINTMOYO_ATTENUATION) {
    /* Print the attenuation values. */
    fprintf(stderr, "white attenuation (%s):\n",
	    dragons_known ? "dragons known" : "dragons unknown");
    print_numeric_influence(q, q->white_attenuation, "%3.2f", 0, 0);
    fprintf(stderr, "black attenuation (%s):\n",
	    dragons_known ? "dragons known" : "dragons unknown");
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
    print_numeric_influence(q, q->white_strength, "%3.0f", 0, 1);
    fprintf(stderr, "black strength:\n");
    print_numeric_influence(q, q->black_strength, "%3.0f", 0, 1);
  }

  if (printmoyo & PRINTMOYO_NUMERIC_INFLUENCE) {
    /* Print the white influence values. */
    fprintf(stderr, "white influence (%s):\n",
	    dragons_known ? "dragons known" : "dragons unknown");
    print_numeric_influence(q, q->white_influence, "%3.0f", 1, 1);
    
    /* Print the black influence values. */
    fprintf(stderr, "black influence (%s):\n",
	    dragons_known ? "dragons known" : "dragons unknown");
    print_numeric_influence(q, q->black_influence, "%3.0f", 1, 1);
  }

  if (printmoyo & PRINTMOYO_PRINT_INFLUENCE) {
    fprintf(stderr, "influence regions (%s):\n",
	    dragons_known ? "dragons known" : "dragons unknown");
    print_influence_areas(q);
  }
}

/* Print numeric influence values. */
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
	fprintf(stderr, "  O ");
      else if (draw_stones && q->p[i][j] == BLACK)
	fprintf(stderr, "  X ");
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
