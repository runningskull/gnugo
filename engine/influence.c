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

/* Influence computed after some move has been made. */
static struct influence_data move_influence;

/* Coordinates for the move influence was last computed for. */
static int influence_movei = -1;
static int influence_movej = -1;
static int influence_color = EMPTY;

/* Influence used for estimation of escape potential. */
static struct influence_data escape_influence;

/* Cache of delta_territory_values. */
static int delta_territory_cache[MAX_BOARD][MAX_BOARD];

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
	if (i==m && j==n)
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
    if (i>0)
      code1(-1, 0, i-1, j, 0);
    if (i<board_size-1)
      code1( 1, 0, i+1, j, 0);
    if (j>0)
      code1( 0,-1, i, j-1, 0);
    if (j<board_size-1)
      code1( 0, 1, i, j+1, 0);
    if (i>0 && j>0)
      code1(-1,-1, i-1, j-1, 1);
    if (i<board_size-1 && j>0)
      code1( 1,-1, i+1, j-1, 1);
    if (i<board_size-1 && j<board_size-1)
      code1( 1, 1, i+1, j+1, 1);
    if (i>0 && j<board_size-1)
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
      if (q->w[i][j] > 1.01 * INFLUENCE_CUTOFF || q->white_influence[i][j] == 0.0)
	q->white_influence[i][j] += q->w[i][j];
    }
    else {
      if (q->w[i][j] > 1.01 * INFLUENCE_CUTOFF || q->black_influence[i][j] == 0.0)
	q->black_influence[i][j] += q->w[i][j];
    }
    
    q->w[i][j] = 0.0;
  }
}

/* Initialize the influence_data structure. If the dragons have been
 * computed, we weight the strength of the influence with the dragon
 * status.
 *
 * The saved_stones parameter tells which strings have been tactically
 * defended by the current move. If no move has been done, it should
 * be passed as NULL.
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
	       char saved_stones[MAX_BOARD][MAX_BOARD])
{
  int i, j;
  float attenuation;

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
      q->white_influence[i][j] = 0.0;
      q->black_influence[i][j] = 0.0;
      q->w[i][j] = 0.0;
      q->white_attenuation[i][j] = attenuation;
      q->black_attenuation[i][j] = attenuation;
      q->white_permeability[i][j] = 1.0;
      q->black_permeability[i][j] = 1.0;
      q->white_strength[i][j] = 0.0;
      q->black_strength[i][j] = 0.0;
      q->p[i][j] = BOARD(i, j);
      
      if (BOARD(i, j) != EMPTY) {
	if (worm[POS(i, j)].attack_codes[0] == WIN
	    && (OTHER_COLOR(q->p[i][j]) == color
		|| worm[POS(i, j)].defend_codes[0] == 0)) {
	  if (q->p[i][j] == WHITE)
	    q->white_permeability[i][j] = 0.0;
	  else
	    q->black_permeability[i][j] = 0.0;
	  q->p[i][j] = EMPTY;
	}
	else if (!dragons_known
		 || dragon[POS(i, j)].id == -1
		 || DRAGON2(i, j).safety != DEAD) {
	  if (q->p[i][j] == WHITE)
	    q->black_permeability[i][j] = 0.0;
	  else
	    q->white_permeability[i][j] = 0.0;
	}
	
	/* Stop influence radiation through saved stones. */
	if (saved_stones && saved_stones[i][j] && BOARD(i, j) != color) {
	  if (BOARD(i, j) == WHITE)
	    q->black_permeability[i][j] = 0.0;
	  else
	    q->white_permeability[i][j] = 0.0;
	}
      }
      
      /* When evaluating influence after a move, the newly placed
       * stone will have the invalid dragon id -1.
       */
      if (BOARD(i, j) != EMPTY) {
	if (!dragons_known || dragon[POS(i, j)].id == -1) {
	  if (q->p[i][j] == WHITE)
	    q->white_strength[i][j] = DEFAULT_STRENGTH;
	  else if (q->p[i][j] == BLACK)
	    q->black_strength[i][j] = DEFAULT_STRENGTH;
	}
	else {
	  if (q->p[i][j] == WHITE)
	    q->white_strength[i][j] = (DEFAULT_STRENGTH
				       * strength_map[DRAGON2(i, j).safety]);
	  else if (q->p[i][j] == BLACK)
	    q->black_strength[i][j] = (DEFAULT_STRENGTH
				       * strength_map[DRAGON2(i, j).safety]);
	}

	/* Stop influence radiation from captured stones.
	 * (Yes, saved_stones now also includes captured stones.)
	 */
	if (saved_stones && saved_stones[i][j] && BOARD(i, j) == color) {
	  if (BOARD(i, j) == WHITE)
	    q->white_strength[i][j] = 0.0;
	  else
	    q->black_strength[i][j] = 0.0;
	}
	
      }
    }
}

/* Callback for the matched patterns in influence.db and barriers.db.
 * The pattern classes used here are:
 * A - Barrier pattern, where O plays first and X tries to block influence.
 * D - Barrier pattern, where O plays first and O tries to block influence.
 * B - Intrusion patterns, adding a low intensity influence source.
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
   * For patterns of class B, D and E, O stones must have non-zero
   * influence strength. Similarly for patterns of class A, X stones
   * must have non-zero influence strength.
   *
   * Patterns also having class s are an exception from this rule.
   */
  if ((pattern->class & (CLASS_D | CLASS_A | CLASS_B | CLASS_E))
      && !(pattern->class & CLASS_s)) {
    for (k = 0; k < pattern->patlen; ++k) { /* match each point */
      if ((pattern->patn[k].att == ATT_O
	   && (pattern->class & (CLASS_D | CLASS_B | CLASS_E)))
	  || (pattern->patn[k].att == ATT_X
	      && (pattern->class & (CLASS_A | CLASS_B)))) {
	/* transform pattern real coordinate */
	int x, y;
	TRANSFORM(pattern->patn[k].x,pattern->patn[k].y,&x,&y,ll);
	x += m;
	y += n;
	if (pattern->class & CLASS_E) {
	  if ((color == WHITE && q->white_strength[x][y] == 0.0)
	      || (color == BLACK && q->black_strength[x][y] == 0.0))
	    return; /* Match failed. */
	}
	else {
	  if ((stackp == 0 && worm[POS(x, y)].attack_codes[0] != 0)
	      || attack(POS(x, y), NULL) != 0)
	    return; /* Match failed */
	}
	/* One test left for class B. */
	if ((pattern->class & CLASS_B)
	    && pattern->patn[k].att == ATT_O) {
	  if ((color == WHITE && q->white_strength[x][y] == 0.0)
	      || (color == BLACK && q->black_strength[x][y] == 0.0))
	    return; /* Match failed. */
	}
	
	if (pattern->class & CLASS_A) {
	  gg_assert(pattern->patn[k].att == ATT_X);
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
    if (!pattern->autohelper(pattern, ll, ti, tj, color, 0))
      return;
  }

  /* If the pattern has a helper, call it to see if the pattern must
   * be rejected.
   */
  if (pattern->helper) {
    if (!pattern->helper(pattern, ll, ti, tj, color)) {
      DEBUG(DEBUG_INFLUENCE,
	    "Influence pattern %s+%d rejected by helper at %m\n",
	    pattern->name, ll, ti, tj);
      return;
    }
  }

  DEBUG(DEBUG_INFLUENCE, "influence pattern '%s'+%d matched at %m\n",
	pattern->name, ll, m, n);

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
    if (q == &escape_influence && (pattern->class & CLASS_e)) {
      if (this_color & WHITE)
	q->white_strength[ti][tj] = 20 * pattern->value;
      if (this_color & BLACK)
	q->black_strength[ti][tj] = 20 * pattern->value;
    }
    else {
      if (this_color & WHITE)
	q->white_strength[ti][tj] = pattern->value;
      if (this_color & BLACK)
	q->black_strength[ti][tj] = pattern->value;
    }

    if (this_color & WHITE)
      q->white_attenuation[ti][tj] = 1.5;
    if (this_color & BLACK)
      q->black_attenuation[ti][tj] = 1.5;
    
    DEBUG(DEBUG_INFLUENCE,
	  "  low intensity influence source at %m, strength %f, color %C\n",
	  ti, tj, pattern->value, this_color);
    return;
  }
  
  /* For E patterns, add a new influence source of the same color and
   * pattern defined strength at *.
   */
  if (pattern->class & CLASS_E) {
    if (color == WHITE) {
      q->white_strength[ti][tj] = pattern->value;
      q->white_attenuation[ti][tj] = DEFAULT_ATTENUATION;
    }
    else {
      q->black_strength[ti][tj] = pattern->value;
      q->black_attenuation[ti][tj] = DEFAULT_ATTENUATION;
    }
    DEBUG(DEBUG_INFLUENCE,
	  "  extra %C source at %m, strength %f\n", color,
	  ti, tj, pattern->value);
    return;
  }
  
  /* Loop through pattern elements and perform necessary actions
   * for A, D, and B patterns. */
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
	DEBUG(DEBUG_INFLUENCE, "  intrusion at %m\n", x, y);
	if (color == WHITE)
	  q->white_strength[x][y] = pattern->value;
	else
	  q->black_strength[x][y] = pattern->value;
      }
    }
  }
}

/* Match the patterns in influence.db and barriers.db in order to add
 * influence barriers, add extra influence sources at possible
 * invasion and intrusion points, and add extra influence induced by
 * strong positions.
 */
static void
find_influence_patterns(struct influence_data *q, int color)
{
  int m,n;

  global_matchpat(influence_callback, ANCHOR_COLOR, &influencepat_db, q, NULL);
  if (color != EMPTY)
    global_matchpat(influence_callback, color, &barrierspat_db, q, NULL);

  /* When color == EMPTY, we introduce a weaker kind of barriers
   * manually instead of searching for patterns.
   */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++)
      if (BOARD(m, n) != EMPTY) {
	int k;
	for (k = 0; k < 8; k++) {
	  int dm = deltai[k];
	  int dn = deltaj[k];
	  if (ON_BOARD2(m+dm, n+dn) && q->p[m+dm][n+dn] == EMPTY) {
	    /* Reduce less diagonally. */
	    float reduction = (k < 4) ? 0.25 : 0.5;
	    if (BOARD(m, n) == BLACK)
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
		  int dragons_known, char no_influence[MAX_BOARD][MAX_BOARD],
		  char saved_stones[MAX_BOARD][MAX_BOARD])
{
  int i, j;
  init_influence(q, color, dragons_known, saved_stones);
  if (m != -1) {
    if (color == WHITE)
      q->black_strength[m][n] = DEFAULT_STRENGTH;
    else
      q->white_strength[m][n] = DEFAULT_STRENGTH;
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

/* Return the size of the moyo around (m, n). If initial is 1, the
 * initial influence is used, otherwise the last computed move
 * influence.
 */
int
influence_get_moyo_size(int m, int n, int color, int initial)
{
  struct influence_data *q;
  if (initial)
    q = &initial_influence;
  else
    q = &move_influence;

  /* Does the color match. */
  if ((q->region_type[q->moyo_segmentation[m][n]] == WHITE_MOYO)
      ^ (color == BLACK))
    return q->region_size[q->moyo_segmentation[m][n]];

  return 0;
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
  /* Invalidate information in move_influence. */
  influence_movei = -1;
  influence_movej = -1;
  influence_color = EMPTY;
  /* Clear delta_territory cache. */
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++)
      delta_territory_cache[i][j] = NOT_COMPUTED;
}

/* Redo the segmentation of the initial influence. */
void
resegment_initial_influence()
{
  segment_influence(&initial_influence);
}

/* Let color play at (m, n) and compute the influence after this move,
 * assuming that the other color is in turn to move next.
 */
static void
compute_move_influence(int m, int n, int color,
		       char saved_stones[MAX_BOARD][MAX_BOARD])
{
  /* Don't recompute if we already have the current values stored. */
  if (influence_movei == m
      && influence_movej == n
      && influence_color == color)
    return;

  if (tryko2(m, n, color, "compute_move_influence", EMPTY, -1, -1)) {
    increase_depth_values();
    compute_influence(&move_influence, OTHER_COLOR(color), m, n, 1,
		      NULL, saved_stones);
    decrease_depth_values();
    popgo();
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
compute_escape_influence(char goal[MAX_BOARD][MAX_BOARD], int color,
			 int escape_value[MAX_BOARD][MAX_BOARD],
			 int dragons_known)
{
  int i, j;
  compute_influence(&escape_influence, OTHER_COLOR(color), -1, -1,
		    dragons_known, goal, NULL);
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++) {
      if (whose_moyo(&escape_influence, i, j) == color)
	escape_value[i][j] = 4;
      else if (whose_area(&escape_influence, i, j) == color)
	escape_value[i][j] = 2;
      else if (whose_area(&escape_influence, i, j) == EMPTY)
	escape_value[i][j] = 1;
      else
	escape_value[i][j] = 0;
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


/* Count the amount of territory for color. */
static int
sum_territory(struct influence_data *q, int color)
{
  int i, j;
  int territory = 0;
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++)
      if ((q->p[i][j] == EMPTY ||
	   (q->black_strength[i][j] == 0 && q->white_strength == 0))
	  && whose_territory(q, i, j) == color)
	territory++;
  return territory;
}

/* Count the amount of moyo for color. */
static int
sum_moyo(struct influence_data *q, int color)
{
  int i, j;
  int moyo = 0;
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++)
      if ((q->p[i][j] == EMPTY ||
	   (q->black_strength[i][j] == 0 && q->white_strength == 0))
	  && whose_moyo(q, i, j) == color)
	moyo++;
  return moyo;
}

/* Count the amount of moyo, that is not also territory, for color. */
static int
sum_strict_moyo(struct influence_data *q, int color)
{
  int i, j;
  int moyo = 0;
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++)
      if ((q->p[i][j] == EMPTY ||
	   (q->black_strength[i][j] == 0 && q->white_strength == 0))
	  && whose_moyo(q, i, j) == color
	  && whose_territory(q, i, j) != color)
	moyo++;
  return moyo;
}

/* Count the amount of area for color. */
static int
sum_area(struct influence_data *q, int color)
{
  int i, j;
  int area = 0;
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++)
      if ((q->p[i][j] == EMPTY
	   || (q->black_strength[i][j] == 0 && q->white_strength == 0))
	  && whose_area(q, i, j) == color)
	area++;
  return area;
}

/* Count the amount of area, that is not also moyo, for color. */
static int
sum_strict_area(struct influence_data *q, int color)
{
  int i, j;
  int area = 0;
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++)
      if ((q->p[i][j] == EMPTY ||
	   (q->black_strength[i][j] == 0 && q->white_strength == 0))
	  && whose_area(q, i, j) == color
	  && whose_moyo(q, i, j) != color)
	area++;
  return area;
}

/* Return the color who has territory at (m, n), or EMPTY. */
int
influence_territory_color(int m, int n)
{
  return whose_territory(&initial_influence, m, n);
}

/* Return the color who has moyo at (m, n), or EMPTY. */
int
influence_moyo_color(int m, int n)
{
  return whose_moyo(&initial_influence, m, n);
}

/* Return the color who has area at (m, n), or EMPTY. */
int
influence_area_color(int m, int n)
{
  return whose_area(&initial_influence, m, n);
}

/* Compute the difference in territory made by a move by color at (m, n). */
int
influence_delta_territory(int m, int n, int color,
			  char saved_stones[MAX_BOARD][MAX_BOARD])
{
  int delta;
  if (delta_territory_cache[m][n] != NOT_COMPUTED)
    return delta_territory_cache[m][n];
  if (0)
    gprintf("influence_delta_territory for %m %s = ", m, n,
	    color_to_string(color));
  compute_move_influence(m, n, color, saved_stones);
  delta = (sum_territory(&move_influence, color) -
	   sum_territory(&initial_influence, color) -
	   sum_territory(&move_influence, OTHER_COLOR(color)) +
	   sum_territory(&initial_influence, OTHER_COLOR(color)));
  if (0)
    gprintf("%d\n", delta);
  delta_territory_cache[m][n] = delta;
  return delta;
}

/* Compute the difference in moyo made by a move by color at (m, n). */
int
influence_delta_moyo(int m, int n, int color,
		     char saved_stones[MAX_BOARD][MAX_BOARD])
{
  int delta;
  if (0)
    gprintf("influence_delta_moyo for %m %s = ", m, n,
	    color_to_string(color));
  compute_move_influence(m, n, color, saved_stones);
  delta = (sum_moyo(&move_influence, color) -
	   sum_moyo(&initial_influence, color) -
	   sum_moyo(&move_influence, OTHER_COLOR(color)) +
	   sum_moyo(&initial_influence, OTHER_COLOR(color)));
  if (0)
    gprintf("%d\n", delta);
  return delta;
}

/* Compute the difference in strict moyo made by a move by color at (m, n). */
int
influence_delta_strict_moyo(int m, int n, int color,
			    char saved_stones[MAX_BOARD][MAX_BOARD])
{
  int delta;
  if (0)
    gprintf("influence_delta_strict_moyo for %m %s = ", m, n,
	    color_to_string(color));
  compute_move_influence(m, n, color, saved_stones);
  /* friendly influence is valued greater than unfriendly 
   * to compensate for an observed failure to take fuseki big points */
  delta = (1.5*sum_strict_moyo(&move_influence, color) -
	   1.5*sum_strict_moyo(&initial_influence, color) -
	   sum_strict_moyo(&move_influence, OTHER_COLOR(color)) +
	   sum_strict_moyo(&initial_influence, OTHER_COLOR(color)));
  if (0)
    gprintf("%d\n", delta);
  return delta;
}

/* Compute the difference in area made by a move by color at (m, n). */
int
influence_delta_area(int m, int n, int color,
		     char saved_stones[MAX_BOARD][MAX_BOARD])
{
  int delta;
  if (0)
    gprintf("influence_delta_area for %m %s = ", m, n,
	    color_to_string(color));
  compute_move_influence(m, n, color, saved_stones);
  /* friendly influence is valued greater than unfriendly 
   * to compensate for an observed failure to take fuseki big points */
  delta = (1.5*sum_area(&move_influence, color) -
	   1.5*sum_area(&initial_influence, color) -
	   sum_area(&move_influence, OTHER_COLOR(color)) +
	   sum_area(&initial_influence, OTHER_COLOR(color)));
  if (0)
    gprintf("%d\n", delta);
  return delta;
}

/* Compute the difference in strict area made by a move by color at (m, n). */
int
influence_delta_strict_area(int m, int n, int color,
			    char saved_stones[MAX_BOARD][MAX_BOARD])
{
  int delta;
  if (0)
    gprintf("influence_delta_strict_area for %m %s = ", m, n,
	    color_to_string(color));
  compute_move_influence(m, n, color, saved_stones);
  delta = (sum_strict_area(&move_influence, color) -
	   sum_strict_area(&initial_influence, color) -
	   sum_strict_area(&move_influence, OTHER_COLOR(color)) +
	   sum_strict_area(&initial_influence, OTHER_COLOR(color)));
  if (0)
    gprintf("%d\n", delta);
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
		   float white_influence[MAX_BOARD][MAX_BOARD],
		   float black_influence[MAX_BOARD][MAX_BOARD],
		   int influence_regions[MAX_BOARD][MAX_BOARD])
{
  int m, n;
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      white_influence[m][n] = q->white_influence[m][n];
      black_influence[m][n] = q->black_influence[m][n];
      if (whose_territory(q, m, n) == WHITE)
	influence_regions[m][n] = 3;
      else if (whose_territory(q, m, n) == BLACK)
	influence_regions[m][n] = -3;
      else if (whose_moyo(q, m, n) == WHITE)
	influence_regions[m][n] = 2;
      else if (whose_moyo(q, m, n) == BLACK)
	influence_regions[m][n] = -2;
      else if (whose_area(q, m, n) == WHITE)
	influence_regions[m][n] = 1;
      else if (whose_area(q, m, n) == BLACK)
	influence_regions[m][n] = -1;
      else
	influence_regions[m][n] = 0;
    }
}
  
/* Compute initial influence and export it. The color parameter tells
 * who is in turn to move.
 */
void
get_initial_influence(int color, int dragons_known,
		      float white_influence[MAX_BOARD][MAX_BOARD],
		      float black_influence[MAX_BOARD][MAX_BOARD],
		      int influence_regions[MAX_BOARD][MAX_BOARD])
{
  compute_initial_influence(color, dragons_known);
  retrieve_influence(&initial_influence, white_influence,
		     black_influence, influence_regions);
}

/* Compute influence after a move and export it.
 */
void
get_move_influence(int i, int j, int color,
		   char saved_stones[MAX_BOARD][MAX_BOARD],
		   float white_influence[MAX_BOARD][MAX_BOARD],
		   float black_influence[MAX_BOARD][MAX_BOARD],
		   int influence_regions[MAX_BOARD][MAX_BOARD])
{
  compute_move_influence(i, j, color, saved_stones);
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
print_move_influence(int m, int n, int color,
		     char saved_stones[MAX_BOARD][MAX_BOARD])
{
  compute_move_influence(m, n, color, saved_stones);
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
