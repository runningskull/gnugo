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
#include "patterns.h"

static void compute_effective_worm_sizes(void);
static void compute_unconditional_status(void);
static void find_worm_attacks_and_defenses(void);
static void find_worm_threats(void);
static int  genus(int i, int j);
static void markcomponent(int i, int j, int m, int n,
			  int mg[MAX_BOARD][MAX_BOARD]);
static void cavity_recurse(int i, int j, 
			   int mx[MAX_BOARD][MAX_BOARD], 
			   int *border_color, int *edge, int *size,
			   int *vertexi, int *vertexj, int ai, int aj);
static void ping_cave(int i, int j, int *result1,  int *result2,
		      int *result3, int *result4);
static int touching(int i, int j, int color);
static void ping_recurse(int i, int j, int *counter, 
			 int mx[MAX_BOARD][MAX_BOARD], 
			 int mr[MAX_BOARD][MAX_BOARD], int color);
static void find_attack_patterns(void);
static void attack_callback(int m, int n, int color,
			    struct pattern *pattern, int ll, void *data);
static void find_defense_patterns(void);
static void defense_callback(int m, int n, int color,
			     struct pattern *pattern, int ll, void *data);


/* A STRING is a maximal connected set of stones of the same color, 
 * black or white. A WORM is the same thing as a string, except that
 * its color can be empty. An empty worm is called a CAVITY.
 *
 * Worms are eventually amalgamated into dragons. An empty dragon
 * is called a CAVE.
 */



/* make_worms() finds all worms and assembles some data about them.
 *
 * Each worm is marked with an origin, having coordinates (origini, originj).
 * This is an arbitrarily chosen element of the worm, in practice the
 * algorithm puts the origin at the first element when they are given
 * the lexicographical order, though its location is irrelevant for
 * applications. To see if two stones lie in the same worm, compare
 * their origins.
 *
 * We will use the field dragon[m][n].genus to keep track of
 * black- or white-bordered cavities (essentially eyes) which are found.  
 * so this field must be zero'd now.
 */

void
make_worms(void)
{
  int m,n; /* iterate over board */

  /* Build the basic worm data:  color, origin, size, liberties. */
  build_worms();

  /* No point continuing if the board is completely empty. */
  if (stones_on_board(BLACK | WHITE) == 0)
    return;

  /* Compute effective sizes of all worms. */
  compute_effective_worm_sizes();

  /* Look for unconditionally alive and dead worms, and unconditional
   * territory.
   */
  compute_unconditional_status();
  
  find_worm_attacks_and_defenses();
  
  /* Find kos. Both the empty vertex and the stone involved in the ko
   * are marked.
   */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      int color;  /* Color of the capturing stone. */
      int ko_pos;

      if (BOARD(m, n) != EMPTY
	  || worm[m][n].size != 1
	  || worm[m][n].color == GRAY_BORDER)
	continue;

      if (worm[m][n].color == WHITE_BORDER)
	color = BLACK;
      else
	color = WHITE;

      if (is_ko(POS(m, n), color, &ko_pos)) {
	worm[m][n].ko = 1;
	worm[I(ko_pos)][J(ko_pos)].ko = 1;
      }
    }
  gg_assert(stackp == 0);

  /* Count liberties of different orders and initialize cutstone fields. */
  
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++)
      if (BOARD(m, n) && is_worm_origin(m, n, m, n)) {
	int lib1, lib2, lib3, lib4;

	ping_cave(m, n, &lib1, &lib2, &lib3, &lib4);
	gg_assert(worm[m][n].liberties == lib1);
	worm[m][n].liberties2 = lib2;
	worm[m][n].liberties3 = lib3;
	worm[m][n].liberties4 = lib4;
	worm[m][n].cutstone = 0;
	worm[m][n].cutstone2 = 0;
	propagate_worm(m, n);
      }
  
  gg_assert(stackp == 0);

/*
 * There are two concepts of cutting stones in the worm array.
 *
 * worm.cutstone:
 *
 *     A CUTTING STONE is one adjacent to two enemy strings,
 *     which do not have a liberty in common. The most common
 *     type of cutting string is in this situation.
 *  
 *     XO
 *     OX
 *     
 *     A POTENTIAL CUTTING STONE is adjacent to two enemy
 *     strings which do share a liberty. For example, X in:
 *     
 *     XO
 *     O.
 *     
 *     For cutting strings we set worm[m][n].cutstone=2. For potential
 *     cutting strings we set worm[m][n].cutstone=1. For other strings,
 *     worm[m][n].cutstone=0.
 *
 * worm.cutstone2:
 *
 *     Cutting points are identified by the patterns in the
 *     connections database. Proper cuts are handled by the fact
 *     that attacking and defending moves also count as moves
 *     cutting or connecting the surrounding dragons. 
 *
 * The cutstone field will now be set. The cutstone2 field is set
 * later, during find_cuts(), called from make_domains().
 *
 * We maintain both fields because the historically older cutstone
 * field is needed to deal with the fact that e.g. in the position
 *
 *
 *    OXX.O
 *    .OOXO
 *    OXX.O
 *
 * the X stones are amalgamated into one dragon because neither cut
 * works as long as the two O stones are in atari. Therefore we add
 * one to the cutstone field for each potential cutting point,
 * indicating that these O stones are indeed worth rescuing.
 *
 * For the time being we use both concepts in parallel. It's
 * possible we also need the old concept for correct handling of lunches.
 */

  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      int ti = -1, tj = -1;
      int ui = -1, uj = -1;
      int i, j, k, di, dj;

      /* Only work on each worm once. This is easiest done if we only 
       * work with the origin of each worm.
       */
      if (BOARD(m, n) == EMPTY || !is_worm_origin(m, n, m, n))
	continue;

      /* Try to find two adjacent worms (ui,uj) and (ti,tj) 
       * of opposite colour from (m, n).
       */
      for (i = 0; i < board_size; i++)
	for (j = 0; j < board_size; j++) {

	  /* Work only with the opposite color from (m, n). */
	  if (BOARD(i, j) != OTHER_COLOR(BOARD(m, n))) 
	    continue;
	      
	  for (k = 0; k < 4; k++) {
	    di = i + deltai[k];
	    dj = j + deltaj[k];
	    if (ON_BOARD2(di,dj) && is_worm_origin(di, dj, m, n)) {

	      ASSERT2(BOARD(di, dj) == BOARD(m, n), m, n);

	    /* If we have not already found a worm which meets the criteria,
	     * store it into (ti, tj), otherwise store it into (ui, uj).
	     */
	      if (ti == -1) {
	        ti = I(worm[i][j].origin);
	        tj = J(worm[i][j].origin);
	      }
	      else if (!is_worm_origin(i, j, ti, tj)) {
	        ui = I(worm[i][j].origin);
	        uj = J(worm[i][j].origin);
	      }
	    }
	  } /* loop over k */
	} /* loop over i,j */

      /* 
       *  We now verify the definition of cutting stones. We have
       *  verified that the string at (m,n) is adjacent to two enemy
       *  strings at (ti,tj) and (ui,uj). We need to know if these
       *  strings share a liberty.
       */

      /* Only do this if we really found anything. */
      if (ui != -1) {
	int vi, vj;  /* look for a common liberty vi,vj */
	TRACE("Worm at %m has t %m and u %m\n", m, n, ti, tj, ui, uj);
        worm[m][n].cutstone = 2;
	for (vi = 0; vi < board_size; vi++)
	  for (vj = 0; vj < board_size; vj++) {

	    if (BOARD(vi, vj) != EMPTY) 
	      continue;
	      
	    if (((vi > 0               && is_worm_origin(vi-1, vj, ti, tj))
		 || (vi < board_size-1 && is_worm_origin(vi+1, vj, ti, tj))
		 || (vj > 0            && is_worm_origin(vi, vj-1, ti, tj))
		 || (vj < board_size-1 && is_worm_origin(vi, vj+1, ti, tj)))
		&&
		((vi > 0               && is_worm_origin(vi-1, vj, ui, uj))
		 || (vi < board_size-1 && is_worm_origin(vi+1, vj, ui, uj))
		 || (vj > 0            && is_worm_origin(vi, vj-1, ui, uj))
		 || (vj < board_size-1 && is_worm_origin(vi, vj+1, ui, uj))))
	      worm[m][n].cutstone = 1;
	  }
	  DEBUG(DEBUG_WORMS, "Worm at %m has t %m and u %m, cutstone %d\n",
		m, n, ti, tj, ui, uj, worm[m][n].cutstone);
      } /* ui != -1, ie we found two adjacent strings */

    } /* loop over m,n */

  gg_assert(stackp == 0);
  
  /* Set the genus of all worms. */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      if (BOARD(m, n) && is_worm_origin(m, n, m, n)) {
 	worm[m][n].genus = genus(m, n);
	propagate_worm(m, n);
      }
    }
  gg_assert(stackp == 0);

  /* We try first to resolve small semeais. */
  small_semeai();
  gg_assert(stackp == 0);

  /* Now we try to improve the values of worm.attack and worm.defend. If we
   * find that capturing the string at (m,n) also defends the string at (i,j),
   * or attacks it, then we move the point of attack and defense.
   * We don't move the attacking point of strings that can't be defended.
   */
  {
    int mi[MAX_BOARD][MAX_BOARD]; /* mark changed information */
    int mxcolor[MAX_BOARD][MAX_BOARD]; /* mark tried moves for color */
    int mxother[MAX_BOARD][MAX_BOARD]; /* mark tried moves for other */
    int i, j;

    memset(mi, 0, sizeof(mi));
    memset(mxcolor, 0, sizeof(mi));
    memset(mxother, 0, sizeof(mi));
    
    for (m = 0; m < board_size; m++)
      for (n = 0; n < board_size; n++) {

	int color = BOARD(m, n);
	int other = OTHER_COLOR(color);

	int ai = I(worm[m][n].attack_point);
	int aj = J(worm[m][n].attack_point);
	int di = I(worm[m][n].defense_point);
	int dj = J(worm[m][n].defense_point);
	  
	/* For each worm, only work with the origin. */
	if (BOARD(m, n) == EMPTY || !is_worm_origin(m, n, m, n))
	  continue;

	/* If the opponent has an attack on the worm (m, n), and we
	 * have not tried this move before, carry it out and see
	 * what it leads to.
	 */
	if (ai != -1 && mxother[ai][aj] == 0) {

	  mxother[ai][aj] = 1;
	  /* First, carry out the attacking move. */
	  if (trymove2(ai, aj, other, "make_worms", -1, -1,
		      EMPTY, -1, -1)) {

	    /* We must read to the same depth that was used in the
	     * initial determination of worm.attack and worm.defend
	     * to avoid horizon effect. Since stackp has been
	     * incremented we must also increment depth and
	     * backfill_depth. */
	      
	    /* Now we try to find a group which is saved or attacked as well
	       by this move. */
	    TRACE("trying %m\n", ai, aj);
	    increase_depth_values();

	    for (i = 0; i < board_size; i++)
	      for (j = 0; j < board_size; j++) {

		/* If a worm has its origin (i, j), and it's not (m, n)...*/
		if (BOARD(i, j)
		    && is_worm_origin(i, j, i, j)
		    && (i != m || j != n)) {

		  /* Either the worm is of the same color as (m, n),
		   * then we try to attack it.  If there was a previous 
		   * attack and defense of it, and there is no defence
		   * for the attack now...
		   */
		  if (worm[i][j].color == color
		      && worm[i][j].attack_code != 0
		      && worm[i][j].defend_code != 0
		      && !find_defense(i, j, NULL, NULL)) {

		    int attack_works = 1;
		    /* Sometimes find_defense() fails to find a
		     * defense which has been found by other means.
		     * Try if the old defense move still works.
		     */
		    if (worm[i][j].defense_point != 0
			&& trymove(worm[i][j].defense_point,
				   color, "make_worms", 0, EMPTY, 0)) {
		      if (!attack(i, j, NULL, NULL))
			attack_works = 0;
		      popgo();
		    }
		      
		    /* ...then move the attack point of that worm to
		     * the attack point of (m, n).
		     */
		    if (attack_works) {
		      TRACE("moving point of attack of %m to %m\n",
			    i, j, ai, aj);
		      worm[i][j].attack_point = POS(ai, aj);
		      add_attack_move(ai, aj, i, j);
		      mi[i][j] = 1;
		    }
		  }
		  /* Or the worm is of the opposite color as (m, n).
		   * If there previously was an attack on it, but there
		   * is none now, then move the defence point of (i, j)
		   * to the defence point of (m, n).
		   */
		  else if (worm[i][j].color == other
			   && worm[i][j].attack_code != 0
			   && !attack(i, j, NULL, NULL)) {
		    if (worm[i][j].defend_code != 0)
		      TRACE("moving point of defense of %m to %m\n",
			    i, j, di, dj);
		    else
		      TRACE("setting point of defense of %m to %m\n",
			    i, j, di, dj);
		    worm[i][j].defend_code   = WIN;
		    worm[i][j].defense_point = POS(ai, aj);
		    add_defense_move(ai, aj, i, j);
		    mi[i][j] = 1;
		  }
		}
	      }
	    popgo();
	    decrease_depth_values();
	  }
	}

	/* If there is a defense point for the worm (m, n), and we
	 * have not tried this move before, move there and see what
	 * it leads to.
	 */
	if (di != -1 && mxcolor[di][dj] == 0) {

	  mxcolor[di][dj] = 1;
	  /* First carry out the defending move. */
	  if (trymove2(di, dj, color, "make_worms", -1, -1, EMPTY, -1, -1)) {
	      
	    TRACE("trying %m\n", di, dj);
	    increase_depth_values();

	    for (i = 0; i < board_size; i++)
	      for (j = 0; j < board_size; j++) {

		/* If a worm has its origin (i, j), and it's not (m, n)...*/
		if (BOARD(i, j)
		    && is_worm_origin(i, j, i, j)
		    && (i != m || j != n)) {

		  /* Either the worm is of the opposite color as (m, n),
		   * then we try to attack it.  If there was a previous 
		   * attack and defense of it, and there is no defence
		   * for the attack now...
		   */
		  if (worm[i][j].color == other
		      && worm[i][j].attack_code != 0 
		      && worm[i][j].defend_code != 0
		      && !find_defense(i, j, NULL, NULL)) {

		    int attack_works = 1;
		    /* Sometimes find_defense() fails to find a
		       defense which has been found by other means.
		       Try if the old defense move still works. */
		    if (trymove(worm[i][j].defense_point,
				other, "make_worms", 0, EMPTY, 0)) {
		      if (!attack(i, j, NULL, NULL))
			attack_works = 0;
		      popgo();
		    }
		      
		    /* ...then move the attack point of that worm to
		     * the defense point of (m, n).
		     */
		    if (attack_works) {
		      TRACE("moving point of attack of %m to %m\n",
			    i, j, di, dj);
		      worm[i][j].attack_point = POS(di, dj);
		      add_attack_move(di, dj, i, j);
		      mi[i][j] = 1;
		    }
		  }
		  /* Or the worm is of the same color as (m, n).
		   * If there previously was an attack on it, but there
		   * is none now, then move the defence point of (i, j)
		   * to the defence point of (m, n).
		   */
		  else if (worm[i][j].color == color
			   && worm[i][j].attack_code != 0
			   && !attack(i, j, NULL, NULL)) {
		    if (worm[i][j].defend_code != 0)
		      TRACE("moving point of defense of %m to %m\n",
			    i, j, di, dj);
		    else
		      TRACE("setting point of defense of %m to %m\n",
			    i, j, di, dj);
		    worm[i][j].defend_code   = WIN;
		    worm[i][j].defense_point = POS(di, dj);
		    add_defense_move(di, dj, i, j);
		    mi[i][j] = 1;
		  }
		}
	      }
	    popgo();
	    decrease_depth_values();
	  }
	}
      }

    /* Propagate the newly generated info to all other stones of each worm. */
    for (i = 0; i < board_size; i++)
      for (j = 0; j < board_size; j++)
	if (mi[i][j])
	  propagate_worm(i, j);
  }

  gg_assert(stackp == 0);
  
  /* Sometimes it happens that the tactical reading finds adjacent
   * strings which both can be attacked but not defended. (The reason
   * seems to be that the attacker tries harder to attack a string,
   * than the defender tries to capture it's neighbors.) When this
   * happens, the eyes code produces overlapping eye spaces and still
   * worse all the nondefendable stones actually get amalgamated with
   * their allies on the outside.
   *
   * To solve this we scan through the strings which can't be defended
   * and check whether they have a neighbor that can be attacked. In
   * this case we set the defense point of the former string to the
   * attacking point of the latter.
   *
   * Please notice that find_defense() will still read this out
   * incorrectly, which may lead to some confusion later.
   */

  /* First look for vertical neighbors. */
  for (m = 0; m < board_size-1; m++)
    for (n = 0; n < board_size; n++)
      if (!is_same_worm(m, n, m+1, n)
	  && BOARD(m, n) != EMPTY
	  && BOARD(m+1, n) != EMPTY) {
        if (worm[m][n].attack_code != 0 && worm[m+1][n].attack_code != 0) {
	  if (worm[m][n].defend_code == 0
	      && does_defend(I(worm[m+1][n].attack_point),
			     J(worm[m+1][n].attack_point), m, n)) {
	    /* FIXME: need to check ko relationship here */
	    change_defense(POS(m, n), worm[m+1][n].attack_point, WIN);
	  }
	  if (worm[m+1][n].defend_code == 0
              && does_defend(I(worm[m][n].attack_point),
			     J(worm[m][n].attack_point), m+1, n)) {
	    /* FIXME: need to check ko relationship here */	    
	    change_defense(POS(m+1, n), worm[m][n].attack_point, WIN);
	  }
        }
      }
  
  /* Then look for horizontal neighbors. */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size-1; n++)
      if (!is_same_worm(m, n, m, n+1)
	  && BOARD(m, n) != EMPTY
	  && BOARD(m, n+1) != EMPTY) {
        if (worm[m][n].attack_code != 0 && worm[m][n+1].attack_code != 0) {
	  if (worm[m][n].defend_code == 0
              && does_defend(I(worm[m][n+1].attack_point),
			     J(worm[m][n+1].attack_point), m, n)) {
	    /* FIXME: need to check ko relationship here */	    
	    change_defense(POS(m, n), worm[m][n+1].attack_point, WIN);
	  }
	  if (worm[m][n+1].defend_code == 0
              && does_defend(I(worm[m][n].attack_point),
			     J(worm[m][n].attack_point), m, n+1)) {
	    /* FIXME: need to check ko relationship here */	    
	    change_defense(POS(m, n+1), worm[m][n].attack_point, WIN);
	  }
	}
      }
  
  gg_assert(stackp == 0);
  
  /* Find adjacent worms that can be easily captured, aka lunches. */

  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      int i, j;

      if (BOARD(m, n) == EMPTY || !is_worm_origin(m, n, m, n))
	continue;

      if (find_lunch(m, n, &i, &j, NULL, NULL)
	  && (worm[i][j].attack_code == WIN
	      || worm[i][j].attack_code == KO_A)) {
	TRACE("lunch found for %m at %m\n", m, n, i, j);
	worm[m][n].lunch = POS(i, j);
      }
      else
	worm[m][n].lunch = NO_MOVE;

      propagate_worm(m, n);
    }
  
  if (!disable_threat_computation)
    find_worm_threats();
}


/* 
 * Clear all worms and initialize the basic data fields:
 *   color, origin, size, liberties
 * This is a substep of make_worms().
 */

void
build_worms()
{
  int vertexi[MAX_BOARD * MAX_BOARD];
  int vertexj[MAX_BOARD * MAX_BOARD];
  int m, n;
  int k;
  int size;

  /* Set all worm data fields to 0. */
  memset(worm, 0 , sizeof(worm));

  /* Initialize the worm data for each worm. */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++)
      worm[m][n].origin = -1;

  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      if (worm[m][n].origin != -1)
	continue;
      worm[m][n].color = BOARD(m, n);
      worm[m][n].origin = POS(m, n);
      worm[m][n].ko = 0;
      worm[m][n].inessential = 0;
      worm[m][n].invincible = 0;
      worm[m][n].unconditional_status = UNKNOWN;
      worm[m][n].effective_size = 0.0;
      if (BOARD(m, n) != EMPTY) {
	worm[m][n].liberties = countlib2(m, n);
	worm[m][n].size = countstones2(m, n);
	propagate_worm(m, n);
      }
      else { /* cavity */
	worm[m][n].color = examine_cavity(m, n, NULL, &size,
					  vertexi, vertexj);
	worm[m][n].size = size;
	/* Propagate to the rest of the cavity. */
	for (k = 0; k<size; k++)
	  if (vertexi[k] != m || vertexj[k] != n)
	    worm[vertexi[k]][vertexj[k]] = worm[m][n];
      }
    }
}


/* Compute effective size of each worm. 
 *
 * Effective size is the number of stones in a worm plus half the
 * number of empty intersections that are at least as close to this
 * worm as to any other worm. This is used to estimate the direct
 * territorial value of capturing a worm. Intersections that are
 * shared are counted with equal fractional values for each worm.
 *
 * We never count intersections further away than distance 3.
 */

static void
compute_effective_worm_sizes()
{
  int m, n;

  /* Distance to closest worm, -1 means unassigned, 0 that there is
   * a stone at the location, 1 a liberty of a stone, and so on.
   */
  int distance[MAX_BOARD][MAX_BOARD];
  /* Pointer to the origin of the closest worms. A very large number of
   * worms may potentially be equally close, but no more than
   * 2*(board_size-1).
   */
  static int wormi[MAX_BOARD][MAX_BOARD][2*(MAX_BOARD-1)];
  static int wormj[MAX_BOARD][MAX_BOARD][2*(MAX_BOARD-1)];
  int nworms[MAX_BOARD][MAX_BOARD];   /* number of equally close worms */
  int found_one;
  int dist; /* current distance */
  int k, l;
    
  /* Initialize arrays. */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {

      for (k = 0; k < 2*(board_size-1); k++) {
	wormi[m][n][k] = -1;
	wormj[m][n][k] = -1;
      }
      nworms[m][n] = 0;
	
      if (BOARD(m, n) == EMPTY)
	distance[m][n] = -1;
      else {
	distance[m][n] = 0;
	wormi[m][n][0] = I(worm[m][n].origin);
	wormj[m][n][0] = J(worm[m][n].origin);
	nworms[m][n]++;
      }
    }

  dist = 0;
  found_one = 1;
  while (found_one && dist <= 3) {
    found_one = 0;
    dist++;
    for (m = 0; m < board_size; m++)
      for (n = 0; n < board_size; n++) {
	if (distance[m][n] != -1)
	  continue; /* already claimed */

	if (m > 0 && distance[m-1][n] == dist-1) {
	  found_one = 1;
	  distance[m][n] = dist;
	  for (k = 0; k < nworms[m-1][n]; k++) {
	    int already_counted = 0;
	    for (l = 0; l < nworms[m][n]; l++)
	      if (wormi[m][n][l] == wormi[m-1][n][k]
		  && wormj[m][n][l] == wormj[m-1][n][k]) {
		already_counted = 1;
		break;
	      }
	    if (!already_counted) {
	      gg_assert (nworms[m][n] < 2*(board_size-1));
	      wormi[m][n][nworms[m][n]] = wormi[m-1][n][k];
	      wormj[m][n][nworms[m][n]] = wormj[m-1][n][k];
	      nworms[m][n]++;
	    }
	  }
	}

	if (m < board_size-1 && distance[m+1][n] == dist-1) {
	  found_one = 1;
	  distance[m][n] = dist;
	  for (k = 0; k < nworms[m+1][n]; k++) {
	    int already_counted = 0;
	    for (l = 0; l < nworms[m][n]; l++)
	      if (wormi[m][n][l] == wormi[m+1][n][k]
		  && wormj[m][n][l] == wormj[m+1][n][k]) {
		already_counted = 1;
		break;
	      }
	    if (!already_counted) {
	      gg_assert (nworms[m][n] < 2*(board_size-1));
	      wormi[m][n][nworms[m][n]] = wormi[m+1][n][k];
	      wormj[m][n][nworms[m][n]] = wormj[m+1][n][k];
	      nworms[m][n]++;
	    }
	  }
	}

	if (n > 0 && distance[m][n-1] == dist-1) {
	  found_one  =  1;
	  distance[m][n] = dist;
	  for (k = 0; k < nworms[m][n-1]; k++) {
	    int already_counted = 0;
	    for (l = 0; l < nworms[m][n]; l++)
	      if (wormi[m][n][l] == wormi[m][n-1][k]
		  && wormj[m][n][l] == wormj[m][n-1][k]) {
		already_counted = 1;
		break;
	      }
	    if (!already_counted) {
	      gg_assert (nworms[m][n] < 2*(board_size-1));
	      wormi[m][n][nworms[m][n]] = wormi[m][n-1][k];
	      wormj[m][n][nworms[m][n]] = wormj[m][n-1][k];
	      nworms[m][n]++;
	    }
	  }
	}

	if (n < board_size-1 && distance[m][n+1] == dist-1) {
	  found_one = 1;
	  distance[m][n] = dist;
	  for (k = 0; k < nworms[m][n+1]; k++) {
	    int already_counted = 0;
	    for (l = 0; l < nworms[m][n]; l++)
	      if (wormi[m][n][l] == wormi[m][n+1][k]
		  && wormj[m][n][l] == wormj[m][n+1][k]) {
		already_counted = 1;
		break;
	      }
	    if (!already_counted) {
	      gg_assert (nworms[m][n] < 2*(board_size-1));
	      wormi[m][n][nworms[m][n]] = wormi[m][n+1][k];
	      wormj[m][n][nworms[m][n]] = wormj[m][n+1][k];
	      nworms[m][n]++;
	    }
	  }
	}
      }
  }

  /* Distribute (fractional) contributions to the worms. */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++)
      for (k = 0; k < nworms[m][n]; k++) {
	int ai = wormi[m][n][k];
	int aj = wormj[m][n][k];
	if (BOARD(m, n) == EMPTY)
	  worm[ai][aj].effective_size += 0.5/nworms[m][n];
	else
	  worm[ai][aj].effective_size += 1.0;
      }
	
  /* Propagate the effective size values all over the worms. */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++)
      if (BOARD(m, n) != EMPTY && is_worm_origin(m, n, m, n))
	propagate_worm(m, n);
}

/* Identify worms which are unconditionally uncapturable in the
 * strongest sense, i.e. even if the opponent is allowed an arbitrary
 * number of consecutive moves. Also identify worms which are
 * similarly unconditionally dead and empty points which are
 * unconditional territory for either player.
 */
static void
compute_unconditional_status()
{
  int unconditional_territory[MAX_BOARD][MAX_BOARD];
  int m, n;
  int color;
  int other;
  
  for (color = WHITE; color <= BLACK; color++) {
    other = OTHER_COLOR(color);
    unconditional_life(unconditional_territory, color);
    for (m = 0; m < board_size; m++)
      for (n = 0; n < board_size; n++) {
	if (!unconditional_territory[m][n])
	  continue;
	
	if (BOARD(m, n) == color) {
	  worm[m][n].unconditional_status = ALIVE;
	  if (unconditional_territory[m][n] == 1)
	    worm[m][n].invincible = 1;
	}
	else if (BOARD(m, n) == EMPTY) {
	  if (color == WHITE)
	    worm[m][n].unconditional_status = WHITE_BORDER;
	  else
	    worm[m][n].unconditional_status = BLACK_BORDER;
	}
	else
	  worm[m][n].unconditional_status = DEAD;
      }
  }
  gg_assert(stackp == 0);
}

/*
 * Analyze tactical safety of each worm. 
 */

static void
find_worm_attacks_and_defenses()
{
  int m, n;

   /* 1. Start with finding attack points. */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      int acode;
      int ti, tj;

      if (BOARD(m, n) == EMPTY || !is_worm_origin(m, n, m, n))
	continue;

      TRACE ("considering attack of %m\n", m, n);
      /* Initialize all relevant fields at once. */
      worm[m][n].attack_code   = 0;
      worm[m][n].attack_point  = 0;
      worm[m][n].defend_code   = 0;
      worm[m][n].defense_point = 0;
      acode = attack(m, n, &ti, &tj);
      if (acode) {
	TRACE("worm at %m can be attacked at %m\n", m, n, ti, tj);
	worm[m][n].attack_code = acode;
	worm[m][n].attack_point = POS(ti, tj);
	add_attack_move(ti, tj, m, n);
      }
      propagate_worm(m, n);
    }
  gg_assert(stackp == 0);
  
  /* 2. Use pattern matching to find a few more attacks. */
  find_attack_patterns();
  gg_assert(stackp == 0);
  
  /* 3. Now find defense moves. */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      int dcode;

      if (BOARD(m, n) == EMPTY || !is_worm_origin(m, n, m, n))
	continue;

      if (worm[m][n].attack_code != 0) {
	int ti, tj;
	TRACE ("considering defense of %m\n", m, n);
	dcode = find_defense(m, n, &ti, &tj);
	if (dcode) {
	  TRACE ("worm at %m can be defended at %m\n", m, n, ti, tj);
	  worm[m][n].defend_code   = dcode;
	  worm[m][n].defense_point = POS(ti, tj);
	  if (ti != -1)
	    add_defense_move(ti, tj, m, n);
	}
	else {
	  /* If the point of attack is not adjacent to the worm, 
	   * it is possible that this is an overlooked point of
	   * defense, so we try and see if it defends.
	   */
	  int ai = I(worm[m][n].attack_point);
	  int aj = J(worm[m][n].attack_point);
	  if (!liberty_of_string2(ai, aj, m, n))
	    if (trymove2(ai, aj, worm[m][n].color, "make_worms", -1, -1,
			EMPTY, -1, -1)) {
	      int acode = attack(m, n, NULL, NULL);
	      if (acode != WIN) {
		int change_defense = 0;
		/* FIXME: Include defense code when move 
		 *        is registered. */
		add_defense_move(ai, aj, m, n);

		if (acode == 0) {
		  worm[m][n].defend_code = WIN;
		  change_defense = 1;
		}
		else if (acode == KO_B && worm[m][n].defend_code != WIN) {
		  worm[m][n].defend_code = KO_A;
		  change_defense = 1;
		}
		else if (acode == KO_A && worm[m][n].defend_code == 0) {
		  worm[m][n].defend_code = KO_B;
		  change_defense = 1;
		}
		
		if (change_defense) {
		  worm[m][n].defense_point = POS(ai, aj);
		  TRACE ("worm at %m can be defended at %m with code %d\n",
			 m, n, ai, aj, worm[m][n].defend_code);
		}
	      }	 
	      popgo();
	    }
	}
      }
      propagate_worm(m, n);
    }
  gg_assert(stackp == 0);

  /* 4. Use pattern matching to find a few more defense moves. */
  find_defense_patterns();
  gg_assert(stackp == 0);
  
  /*
   * In the new move generation scheme, we need to find all moves that
   * attacks and or defends some string.
   */

  /*
   * 5. Find additional attacks and defenses by testing all immediate
   *    liberties. Further attacks and defenses are found by pattern
   *    matching and by trying whether each attack or defense point
   *    attacks or defends other strings.
   */
  {
    static int libs[MAXLIBS];
    int k;
    int color;
    int other;
    int liberties;

    for (m = 0; m < board_size; m++)
      for (n = 0; n < board_size; n++) {
	color = BOARD(m, n);
	if (color == EMPTY || !is_worm_origin(m, n, m, n))
	  continue;

	other = OTHER_COLOR(color);
	
	if (worm[m][n].attack_code == 0)
	  continue;
	
	/* There is at least one attack on this group. Try the
	 * liberties.
	 */
	liberties = findlib(POS(m, n), MAXLIBS, libs);
	
	for (k = 0; k < liberties; k++) {
	  int ai = I(libs[k]);
	  int aj = J(libs[k]);
	  /* Try to attack on the liberty. */
	  if (trymove2(ai, aj, other, "make_worms", -1, -1,
		       EMPTY, -1, -1)) {
	    if (!BOARD(m, n) || attack(m, n, NULL, NULL)) {
	      int dcode;
	      if (!BOARD(m, n))
		dcode = 0;
	      else
		dcode = find_defense(m, n, NULL, NULL);

	      if (dcode == 0
		  || (dcode == KO_B && (worm[m][n].attack_code == 0
					|| worm[m][n].attack_code == KO_B))
		  || (dcode == KO_A && worm[m][n].attack_code == 0))
		add_attack_move(ai, aj, m, n);
	    }
	    popgo();
	  }
	  /* Try to defend at the liberty. */
	  if (worm[m][n].defend_code != 0)
	    if (trymove2(ai, aj, color, "make_worms", -1, -1,
			EMPTY, -1, -1)) {
	      int acode = attack(m, n, NULL, NULL);
	      if (acode == 0
		  || (acode == KO_B && (worm[m][n].defend_code == 0
					|| worm[m][n].defend_code == KO_B))
		  || (acode == KO_A && worm[m][n].defend_code == 0))
		add_defense_move(ai, aj, m, n);
	      popgo();
	    }
	}
      }
  }
  gg_assert(stackp == 0);
}


/*
 * Find moves threatening to attack or save all worms.
 */

static void
find_worm_threats()
{
  int m, n;

  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      int liberties;
      static int libs[MAXLIBS];

      int k;
      int l;
      int color;
      int other;

      /* Only work with origins. */
      color = BOARD(m, n);
      if (color == EMPTY || !is_worm_origin(m, n, m, n))
	continue;

      other = OTHER_COLOR(color);

      /* 1. Start with finding attack threats. */
      /* Only try those worms that have no attack. */
      if (worm[m][n].attack_code == 0) {

	liberties = findlib(POS(m, n), MAXLIBS, libs);

	/* This test would seem to be unnecessary since we only attack
	 * strings with attack_code == 0, but it turns out that single
	 * stones with one liberty that can be captured, but come to
	 * live again in a snap-back get attack_code == 0. 
	 *
	 * The test against 6 liberties is just an optimization.
	 */
	if (liberties > 1 && liberties < 6) {
	  for (k = 0; k < liberties; k++) {
	    int ai = I(libs[k]);
	    int aj = J(libs[k]);

	    /* Try to threaten on the liberty. */
	    if (trymove2(ai, aj, other, "threaten attack", m, n,
			EMPTY, -1, -1)) {
	      /* FIXME: Support ko also. */
	      if (attack(m, n, NULL, NULL) == WIN)
		add_attack_threat_move(ai, aj, m, n);
	      popgo();
	    }

	    /* Try to threaten on second order liberties. */
	    for (l = 0; l < 4; l++) {
	      int bi = I(libs[k]) + deltai[l];
	      int bj = J(libs[k]) + deltaj[l];

	      if (!ON_BOARD2(bi, bj)
		  || BOARD(bi, bj) != EMPTY
		  || liberty_of_string2(bi, bj, m, n))
		continue;

	      if (trymove2(bi, bj, other, "threaten attack", m, n,
			  EMPTY, -1, -1)) {
		/* FIXME: Support ko also. */
		if (attack(m, n, NULL, NULL) == WIN)
		  add_attack_threat_move(bi, bj, m, n);
		popgo();
	      }
	    }
	  }
	}

	/* FIXME: Try other moves also (patterns?). */
      }

      /* 2. Continue with finding defense threats. */
      /* Only try those worms that have an attack. */
      /* FIXME: Support ko also */
      if (worm[m][n].attack_code == WIN
	  && worm[m][n].defend_code == 0) {

	liberties = findlib(POS(m, n), MAXLIBS, libs);

	for (k = 0; k < liberties; k++) {
	  int ai = I(libs[k]);
	  int aj = J(libs[k]);

	  /* Try to threaten on the liberty. */
	  if (trymove2(ai, aj, color, "threaten defense", -1, -1,
		      EMPTY, -1, -1)) {
	    /* FIXME: Support ko also. */
	    if (attack(m, n, NULL, NULL) == WIN
		&& find_defense(m, n, NULL, NULL) == WIN)
	      add_defense_threat_move(ai, aj, m, n);
	    popgo();
	  }

	  /* Try to threaten on second order liberties. */
	  for (l = 0; l < 4; l++) {
	    int bi = I(libs[k]) + deltai[l];
	    int bj = J(libs[k]) + deltaj[l];

	    if (!ON_BOARD2(bi, bj)
		|| BOARD(bi, bj) != EMPTY
		|| liberty_of_string2(bi, bj, m, n))
	      continue;

	    if (trymove2(bi, bj, other, "threaten defense", m, n,
			EMPTY, -1, -1)) {
	      /* FIXME: Support ko also. */
	      if (attack(m, n, NULL, NULL) == WIN
		  && find_defense(m, n, NULL, NULL) == WIN)
		add_defense_threat_move(bi, bj, m, n);
	      popgo();
	    }
	  }
	}

	/* FIXME: Try other moves also (patterns?). */
      }
    }
}


/*
 * Test whether two worms are the same. Used by autohelpers.
 * Before this function can be called, build_worms must have been run.
 */

int
is_same_worm(int ai, int aj, int bi, int bj)
{
  return (worm[ai][aj].origin == worm[bi][bj].origin);
}


/*
 * Test whether the origin of the worm at (wi, wj) is (i, j).
 */

int
is_worm_origin(int wi, int wj, int i, int j)
{
  return (worm[wi][wj].origin == POS(i, j));
}


/* 
 * propagate_worm() takes the worm data at one stone and copies it to 
 * the remaining members of the worm.
 *
 * Even though we don't need to copy all the fields, it's probably
 * better to do a structure copy which should compile to a block copy.
 */

void 
propagate_worm(int m, int n)
{
  int k;
  int num_stones;
  int stones[MAX_BOARD * MAX_BOARD];
  gg_assert(stackp == 0);
  ASSERT2(BOARD(m, n) != EMPTY, m, n);

  num_stones = findstones(POS(m, n), MAX_BOARD * MAX_BOARD, stones);
  for (k = 0; k < num_stones; k++)
    if (stones[k] != POS(m, n))
      worm[I(stones[k])][J(stones[k])] = worm[m][n];
}


/* ping_cave(i, j, *lib1, ...) is applied when (i, j) points to a string.
 * It computes the vector (*lib1, *lib2, *lib3, *lib4), 
 * where *lib1 is the number of liberties of the string, 
 * *lib2 is the number of second order liberties (empty vertices
 * at distance two) and so forth.
 *
 * The definition of liberties of order >1 is adapted to the problem
 * of detecting the shape of the surrounding cavity. In particular
 * we want to be able to see if a group is loosely surrounded.
 *
 * A liberty of order n is an empty space which may be connected
 * to the string by placing n stones of the same color on the board, 
 * but no fewer. The path of connection may pass through an intervening group
 * of the same color. The stones placed at distance >1 may not touch a
 * group of the opposite color. At the edge, also diagonal neighbors
 * count as touching. The path may also not pass through a liberty at distance
 * 1 if that liberty is flanked by two stones of the opposing color. This
 * reflects the fact that the O stone is blocked from expansion to the
 * left by the two X stones in the following situation:
 * 
 *          X.
 *          .O
 *          X.
 *
 * On the edge, one stone is sufficient to block expansion:
 *
 *          X.
 *          .O
 *          --
 */

static void 
ping_cave(int i, int j, int *lib1, int *lib2, int *lib3, int *lib4)
{
  int m, n;
  int k;
  int libs[MAXLIBS];
  int mrc[MAX_BOARD][MAX_BOARD];
  int mse[MAX_BOARD][MAX_BOARD];
  int color = BOARD(i, j);
  int other = OTHER_COLOR(color);

  memset(mse, 0, sizeof(mse));

  /* Find and mark the first order liberties. */
  *lib1 = findlib(POS(i, j), MAXLIBS, libs);
  for (k = 0; k < *lib1; k++)
    mse[I(libs[k])][J(libs[k])] = 1;

  /* Reset mse at liberties which are flanked by two stones of the
   * opposite color, or one stone and the edge.
   */

  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++)
      if (mse[m][n]
	  && (((m == 0 || BOARD(m-1, n) == other)
	       && (m == board_size-1 || BOARD(m+1, n) == other))
	      ||((n == 0 || BOARD(m, n-1) == other)
	       && (n == board_size-1 || BOARD(m, n+1) == other))))
	mse[m][n] = 0;

  *lib2 = 0;
  memset(mrc, 0, sizeof(mrc));
  ping_recurse(i, j, lib2, mse, mrc, color);

  *lib3 = 0;
  memset(mrc, 0, sizeof(mrc));
  ping_recurse(i, j, lib3, mse, mrc, color);

  *lib4 = 0;
  memset(mrc, 0, sizeof(mrc));
  ping_recurse(i, j, lib4, mse, mrc, color);
}


/* recursive function called by ping_cave */

static void 
ping_recurse(int i, int j, int *counter, 
	     int mx[MAX_BOARD][MAX_BOARD], 
	     int mr[MAX_BOARD][MAX_BOARD], 
	     int color)
{
  int k;
  mr[i][j] = 1;

  for (k = 0; k < 4; k++) {
    int ai = i + deltai[k];
    int aj = j + deltaj[k];
    if (BOARD(ai, aj) == EMPTY
	&& mx[ai][aj] == 0
	&& mr[ai][aj] == 0
	&& !touching(ai, aj, OTHER_COLOR(color))) {
      (*counter)++;
      mr[ai][aj] = 1;
      mx[ai][aj] = 1;
    }
  }
  
  if (!worm[i][j].ko) {
    for (k = 0; k < 4; k++) {
      int ai = i + deltai[k];
      int aj = j + deltaj[k];
      if (ON_BOARD2(ai, aj)
	  && mr[ai][aj] == 0
	  && (mx[ai][aj] == 1
	      || BOARD(ai, aj) == color))
	ping_recurse(ai, aj, counter, mx, mr, color);
    }
  }
}


/* touching(i, j, color) returns true if the vertex at (i, j) is
 * touching any stone of (color).
 */

static int
touching(int i, int j, int color)
{
  return (BOARD(i-1, j) == color
	  || BOARD(i+1, j) == color
	  || BOARD(i, j-1) == color
	  || BOARD(i, j+1) == color);
}


/* The GENUS of a string is the number of connected components of
 * its complement, minus one. It is an approximation to the number of
 * eyes of the string. If (i, j) points to the origin of a string,
 * genus(i, j) returns its genus.
 */

static int 
genus(int i, int j)
{
  int m, n;
  int mg[MAX_BOARD][MAX_BOARD];
  int gen = -1;

  memset(mg, 0, sizeof(mg));
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      if (!mg[m][n] && (BOARD(m, n) == EMPTY
			|| !is_worm_origin(m, n, i, j))) {
	markcomponent(i, j, m, n, mg);
	gen++;
      }
    }

  return gen;
}


/* This recursive function marks the component at (m, n) of 
 * the complement of the string with origin (i, j)
 */

static void 
markcomponent(int i, int j, int m, int n, int mg[MAX_BOARD][MAX_BOARD])
{
  int k;
  mg[m][n] = 1;
  for (k = 0; k < 4; k++) {
    int dm = deltai[k];
    int dn = deltaj[k];
    if (ON_BOARD2(m+dm, n+dn)
	&& mg[m+dm][n+dn] == 0
	&& (BOARD(m+dm, n+dn) == EMPTY
	    || !is_worm_origin(m+dm, n+dn, i, j)))
      markcomponent(i, j, m+dm, n+dn, mg);
  }
}


/* examine_cavity(m, n, *edge, *size), if (m, n) is EMPTY, examines the
 * cavity at (m, n), determines its size and returns its bordercolor,
 * which can be BLACK_BORDER, WHITE_BORDER or GRAY_BORDER. The edge
 * parameter is set to the number of edge vertices in the cavity.
 * (vertexi[], vertexj[]) hold the vertices of the cavity. vertexi[]
 * and vertexj[] should be dimensioned to be able to hold the whole
 * board.
 *
 * If (m, n) is nonempty, it returns the same result, imagining
 * that the string at (m, n) is removed. The edge parameter is
 * set to the number of vertices where the cavity meets the
 * edge in a point outside the removed string.  
 */

int
examine_cavity(int m, int n, int *edge, int *size, int *vertexi, int *vertexj)
{
  int border_color = EMPTY;
  int ml[MAX_BOARD][MAX_BOARD];
  int sz;
  int oi, oj;
  
  ASSERT_ON_BOARD2(m, n);

  ASSERT2(((vertexi == NULL) ^ (vertexj == NULL)) == 0, m, n);
  
  memset(ml, 0, sizeof(ml));

  sz = 0;
  if (edge)
    *edge = 0;

  if (BOARD(m, n) != EMPTY) {
    int origin = find_origin(POS(m, n));
    oi = I(origin);
    oj = J(origin);
  }
  else {
    oi = -1;
    oj = -1;
  }
  
  cavity_recurse(m, n, ml, &border_color, edge, &sz, vertexi, vertexj, oi, oj);

  if (size)
    *size = sz;
  
  if (border_color == (BLACK | WHITE))
    return GRAY_BORDER;  
  if (border_color == BLACK)
    return BLACK_BORDER;
  if (border_color == WHITE)
    return WHITE_BORDER;

  /* We should have returned now, unless the board is completely empty.
   * Verify that this is the case and then return GRAY_BORDER.
   */
  gg_assert(border_color == EMPTY && stones_on_board(BLACK | WHITE) == 0);
  
  return GRAY_BORDER;
}


/* helper function for examine_cavity.
 * border_color contains information so far : transitions allowed are
 *   EMPTY       -> BLACK/WHITE
 *   BLACK/WHITE -> BLACK | WHITE
 *
 * mx[i][j] is 1 if (i, j) has already been visited.
 *
 * if (ai, aj) points to the origin of a string, it will be ignored.
 *
 * On (fully-unwound) exit
 *   *border_color should be BLACK, WHITE or BLACK | WHITE
 *   *edge is the count of edge pieces
 *   *size is the count of vertices
 *   (vertexi[], vertexj[]) holds a list of the vertices
 *
 * *border_color should be EMPTY if and only if the board
 * is completely empty or only contains the ignored string.
 */

static void 
cavity_recurse(int i, int j, int mx[MAX_BOARD][MAX_BOARD], 
	       int *border_color, int *edge, int *size,
	       int *vertexi, int *vertexj,
	       int ai, int aj)
{
  int k;
  ASSERT2(mx[i][j] == 0, i, j);

  mx[i][j] = 1;

  if (vertexi) {
    vertexi[*size] = i;
    vertexj[*size] = j;
  }
  (*size)++;
  
  if ((edge)
      && ((i == 0) || (i == board_size-1) || (j == 0) || (j == board_size-1))
      && (BOARD(i, j) == EMPTY)) 
    (*edge)++;

  /* Loop over the four neighbors. */
  for (k = 0; k < 4; k++) {
    int di = deltai[k];
    int dj = deltaj[k];
    if (ON_BOARD2(i+di, j+dj) && !mx[i+di][j+dj]) {
      int neighbor_empty = 0;
      
      if (BOARD(i+di, j+dj) == EMPTY)
	neighbor_empty = 1;
      else {
	/* Count the neighbor as empty if it is part of the (ai, aj) string. */
	if (POS(ai, aj) == find_origin(POS(i+di, j+dj)))
	  neighbor_empty = 1;
	else
	  neighbor_empty = 0;
      }
      
      if (!neighbor_empty)
	*border_color |= BOARD(i+di, j+dj);
      else
	cavity_recurse(i+di, j+dj, mx, border_color, edge, size,
		       vertexi, vertexj, ai, aj);
    }
  }
}


/* Find attacking moves by pattern matching, for both colors. */
static void
find_attack_patterns(void)
{
  global_matchpat(attack_callback, ANCHOR_OTHER, &attpat_db, NULL, NULL);
}

/* Try to attack every X string in the pattern, whether there is an attack
 * before or not. Only exclude already known attacking moves.
 */
static void
attack_callback(int m, int n, int color, struct pattern *pattern, int ll,
		void *data)
{
  int ti, tj;
  int k;
  UNUSED(data);

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
      TRACE("Attack pattern %s+%d rejected by helper at %m\n", pattern->name,
	    ll, ti, tj);
      return;
    }
  }

  /* Loop through pattern elements in search of X strings to attack. */
  for (k = 0; k < pattern->patlen; ++k) { /* match each point */
    if (pattern->patn[k].att == ATT_X) {
      /* transform pattern real coordinate */
      int x, y;
      int ai, aj;
      TRANSFORM(pattern->patn[k].x,pattern->patn[k].y,&x,&y,ll);
      x += m;
      y += n;

      ai = I(worm[x][y].origin);
      aj = J(worm[x][y].origin);

      /* A string with 5 liberties or more is considered tactically alive. */
      if (countlib2(ai, aj) > 4)
	continue;

      if (attack_move_known(ti, tj, ai, aj))
	continue;

      /* No defenses are known at this time, so defend_code is always 0. */
#if 0
      /* If the string can be attacked but not defended, ignore it. */
      if (worm[x][y].attack_code == WIN && worm[x][y].defend_code == 0)
	continue;
#endif
      
      /* FIXME: Don't attack the same string more than once.
       * Play (ti, tj) and see if there is a defense.
       */
      if (trymove2(ti, tj, color, "attack_callback", ai, aj,
		  EMPTY, -1, -1)) {
	int dcode;
	if (!BOARD(ai, aj))
	  dcode = 0;
	else if (!attack(ai, aj, NULL, NULL))
	  dcode = WIN;
	else
	  dcode = find_defense(ai, aj, NULL, NULL);

	popgo();

	if (dcode == 0) {
	  TRACE("Attack pattern %s+%d found attack on %m at %m\n",
		pattern->name, ll, ai, aj, ti, tj);
	  worm[ai][aj].attack_code = WIN;
	  worm[ai][aj].attack_point = POS(ti, tj);
	}
	else if (dcode == KO_A) {
	  TRACE("Attack pattern %s+%d found attack on %m at %m with ko (acode=%d)\n",
		pattern->name, ll, ai, aj, ti, tj, KO_A);
	  if (worm[ai][aj].attack_code == KO_B 
	      || worm[ai][aj].attack_code == 0) {
	    worm[ai][aj].attack_code = KO_B;
	    worm[ai][aj].attack_point = POS(ti, tj);
	  }
	}
	else if (dcode == KO_B) {
	  TRACE("Attack pattern %s+%d found attack on %m at %m with ko (acode=%d)\n",
		pattern->name, ll, ai, aj, ti, tj, KO_B);
	  if (worm[ai][aj].attack_code != WIN) {
	    worm[ai][aj].attack_code = KO_A;
	    worm[ai][aj].attack_point = POS(ti, tj);
	  }
	}

	if (dcode != WIN) {
	  propagate_worm(ai, aj);
	  add_attack_move(ti, tj, ai, aj);
	}
      }
    }
  }
}

static void
find_defense_patterns(void)
{
  global_matchpat(defense_callback, ANCHOR_COLOR, &defpat_db, NULL, NULL);
}

static void
defense_callback(int m, int n, int color, struct pattern *pattern, int ll,
		 void *data)
{
  int ti, tj;
  int k;
  UNUSED(data);

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
      TRACE("Defense pattern %s+%d rejected by helper at %m\n", pattern->name,
	    ll, ti, tj);
      return;
    }
  }

  /* Loop through pattern elements in search for O strings to defend. */
  for (k = 0; k < pattern->patlen; ++k) { /* match each point */
    if (pattern->patn[k].att == ATT_O) {
      /* transform pattern real coordinate */
      int x, y;
      int ai, aj;
      TRANSFORM(pattern->patn[k].x,pattern->patn[k].y,&x,&y,ll);
      x += m;
      y += n;

      ai = I(worm[x][y].origin);
      aj = J(worm[x][y].origin);

      if (worm[ai][aj].attack_code == 0
	  || defense_move_known(ti, tj, ai, aj))
	continue;
      
      /* FIXME: Don't try to defend the same string more than once.
       * FIXME: For all attacks on this string, we should test whether
       *        the proposed move happens to refute the attack.
       * Play (ti, tj) and see if there is an attack. */
      if (trymove2(ti, tj, color, "defense_callback", ai, aj,
		  EMPTY, -1, -1)) {
	int acode = attack(ai, aj, NULL, NULL);

	popgo();
	
	if (acode == 0) {
	  TRACE("Defense pattern %s+%d found defense of %m at %m\n",
		pattern->name, ll, ai, aj, ti, tj);
	  worm[ai][aj].defend_code   = WIN;
	  worm[ai][aj].defense_point = POS(ti, tj);
	}
	else if (acode == KO_A) {
	  TRACE("Defense pattern %s+%d found defense of %m at %m with ko (acode=%d)\n",
		pattern->name, ll, ai, aj, ti, tj, 3);
	  if (worm[ai][aj].defend_code != WIN) {
	    worm[ai][aj].defend_code   = KO_B;
	    worm[ai][aj].defense_point = POS(ti, tj);
	  }
	}
	else if (acode == KO_B) {
	  TRACE("Defense pattern %s+%d found defense of %m at %m with ko (acode=%d)\n",
		pattern->name, ll, ai, aj, ti, tj, 3);
	  if (worm[ai][aj].defend_code != WIN) {
	    worm[ai][aj].defend_code   = KO_A;
	    worm[ai][aj].defense_point = POS(ti, tj);
	  }
	}

	if (acode != WIN) {
	  propagate_worm(ai, aj);
	  add_defense_move(ti, tj, ai, aj);
	}
      }
    }
  }
}

/* ================================================================ */
/*                      Debugger functions                          */
/* ================================================================ */

/* For use in gdb, print details of the worm at (m,n). 
 * Add this to your .gdbinit file:
 *
 * define worm
 * set ascii_report_worm("$arg0")
 * end
 *
 * Now 'worm S8' will report the details of the S8 worm.
 *
 */

void
ascii_report_worm(char *string)
{
  int m, n;
  string_to_location(board_size, string, &m, &n);
  report_worm(m, n);
}


void
report_worm(int m, int n)
{
  if (BOARD(m, n) == EMPTY) {
    gprintf("There is no worm at %m\n", m, n);
    return;
  }

  gprintf("*** worm at %m:\n", m, n);
  gprintf("color: %s; origin: %1m; size: %d; effective size: %f\n",
	  (worm[m][n].color == WHITE) ? "White" : "Black",
	  worm[m][n].origin, worm[m][n].size, worm[m][n].effective_size);

  gprintf("liberties: %d order 2 liberties:%d order 3:%d order 4:%d\n",
	  worm[m][n].liberties, 
	  worm[m][n].liberties2, 
	  worm[m][n].liberties3, 
	  worm[m][n].liberties4);

  if (worm[m][n].attack_point != NO_MOVE)
    gprintf("attack point %1m, ", worm[m][n].attack_point);
  else
    gprintf("no attack point, ");

  if (worm[m][n].attack_code == 1)
    gprintf("attack code WIN\n");
  else if (worm[m][n].attack_code == KO_A)
    gprintf("attack code KO_A\n");
  else if (worm[m][n].attack_code == KO_B)
    gprintf("attack code KO_B\n");

  if (worm[m][n].defense_point != NO_MOVE)
    gprintf("defense point %1m, ", worm[m][n].defense_point);
  else
    gprintf("no defense point, ");

  if (worm[m][n].defend_code == 1)
    gprintf("defend code WIN\n");
  else if (worm[m][n].defend_code == KO_A)
    gprintf("defend code KO_A\n");
  else if (worm[m][n].defend_code == KO_B)
    gprintf("defend code KO_B\n");

  if (worm[m][n].lunch != NO_MOVE)
    gprintf("lunch at %1m\n", worm[m][n].lunch);

  gprintf("cutstone: %d, cutstone2: %d\n",
	  worm[m][n].cutstone, worm[m][n].cutstone2);

  gprintf("genus: %d, ", worm[m][n].genus);

  if (worm[m][n].inessential)
    gprintf("inessential: YES, ");
  else
    gprintf("inessential: NO, ");

  if (worm[m][n].invincible)
    gprintf("invincible: YES, \n");
  else
    gprintf("invincible: NO, \n");

  if (worm[m][n].unconditional_status == ALIVE)
    gprintf("unconditional status ALIVE\n");
  else if (worm[m][n].unconditional_status == DEAD)
    gprintf("unconditional status DEAD\n");
  else if (worm[m][n].unconditional_status == WHITE_BORDER)
    gprintf("unconditional status WHITE_BORDER\n");
  else if (worm[m][n].unconditional_status == BLACK_BORDER)
    gprintf("unconditional status BLACK_BORDER\n");
  else if (worm[m][n].unconditional_status == UNKNOWN)
    gprintf("unconditional status UNKNOWN\n");
}


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
