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
static void find_worm_attacks_and_defenses(void);
static void find_worm_threats(void);
static int  genus(int i, int j);
static void markcomponent(int i, int j, int m, int n,
			  int mg[MAX_BOARD][MAX_BOARD]);
static void cavity_recurse(int i, int j, 
			   int mx[MAX_BOARD][MAX_BOARD], 
			   int *border_color, int *edge, int *size,
			   int *vertexi, int *vertexj, int ai, int aj);
static void ping_cave(int i, int j, 
		      int *result1,  int *result2, int *result3, int *result4);
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

  /* Identify strongly unconditionally uncapturable worms. This means
   * that they can't be captured even if the opponent is allowed an
   * arbitrary number of consecutive moves.
   */
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
	  
	  if (p[m][n] == color) {
	    worm[m][n].unconditional_status = ALIVE;
	    if (unconditional_territory[m][n] == 1)
	      worm[m][n].invincible = 1;
	  }
	  else if (p[m][n] == EMPTY) {
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
  
  find_worm_attacks_and_defenses();
  
  /* Find kos. Check carefully that the purported ko move doesn't actually
   * capture more than one stone.
   *
   * We can't just call is_ko() here because this wouldn't tell us
   * which stone got captured in the ko.
   */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      int captures;
      int capi, capj;

      if (p[m][n] != EMPTY
	  || (worm[m][n].size != 1)
	  || (worm[m][n].color == GRAY_BORDER)) 
	continue;

      captures = 0;
      capi = -1;
      capj = -1;

      if (m > 0
	  && worm[m-1][n].size == 1
	  && worm[m-1][n].liberties == 1) {
	captures++;
	capi = m-1;
	capj = n;
      }
      if (m < board_size-1
	  && worm[m+1][n].size == 1
	  && worm[m+1][n].liberties == 1) {
	captures++;
	capi = m+1;
	capj = n;
      }
      if (n > 0
	  && worm[m][n-1].size == 1
	  && worm[m][n-1].liberties == 1) {
	captures++;
	capi = m;
	capj = n-1;
      }
      if (n < board_size-1
	  && worm[m][n+1].size == 1
	  && worm[m][n+1].liberties == 1) {
	captures++;
	capi = m;
	capj = n+1;
      }
      if (captures == 1) {
	worm[m][n].ko = 1;
	worm[capi][capj].ko = 1;
      }
    }
  gg_assert(stackp == 0);

  /* Count liberties of different orders and initialize cutstone fields. */
  
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++)
      if ((p[m][n])
	  && (worm[m][n].origini == m)
	  && (worm[m][n].originj == n)) {
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

      int ui=-1, uj=-1;
      int ti=-1, tj=-1;
      int i,j,k,di,dj;

      /* Only work on each worm once. This is easiest done if we only 
       * work with the origin of each worm. */
      if (p[m][n] == EMPTY
	  || (worm[m][n].origini != m) || (worm[m][n].originj != n))
	continue;

      /* Try to find two adjacent worms (ui,uj) and (ti,tj) 
       * of opposite colour from (m, n). */
      for (i = 0; i < board_size; i++)
	for (j = 0; j < board_size; j++) {

	  /* Work only with the opposite color from (m, n). */
	  if (p[i][j] != OTHER_COLOR(p[m][n])) 
	    continue;
	      
	  for (k = 0; k < 4; k++) {
	    di = i + deltai[k];
	    dj = j + deltaj[k];
	    if (ON_BOARD(di,dj)
	      && (worm[di][dj].origini == m)
	      && (worm[di][dj].originj == n)) {

	      ASSERT(p[di][dj] == p[m][n], m, n);

	    /* If we have not already found a worm which meets the criteria,
	     * store it into (ti, tj), otherwise store it into (ui, uj).
	     */
	      if (ti == -1) {
	        ti = worm[i][j].origini;
	        tj = worm[i][j].originj;
	      }
	      else if (ti != worm[i][j].origini
		       || tj != worm[i][j].originj) {
	        ui = worm[i][j].origini;
	        uj = worm[i][j].originj;
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

	    if (p[vi][vj] != EMPTY) 
	      continue;
	      
	    if (((vi > 0 
		  && worm[vi-1][vj].origini == ti
		  && worm[vi-1][vj].originj == tj)
		 || (vi < board_size-1
		     && worm[vi+1][vj].origini == ti
		     && worm[vi+1][vj].originj == tj)
		 || (vj > 0
		     && worm[vi][vj-1].origini == ti
		     && worm[vi][vj-1].originj == tj)
		 || (vj < board_size-1
		     && worm[vi][vj+1].origini == ti
		     && worm[vi][vj+1].originj == tj))
		&&
		((vi > 0
		  && worm[vi-1][vj].origini == ui
		  && worm[vi-1][vj].originj == uj)
		 || (vi < board_size-1
		     && worm[vi+1][vj].origini == ui
		     && worm[vi+1][vj].originj == uj)
		 || (vj > 0
		     && worm[vi][vj-1].origini == ui
		     && worm[vi][vj-1].originj == uj)
		 || (vj < board_size-1
		     && worm[vi][vj+1].origini == ui
		     && worm[vi][vj+1].originj == uj)))
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
      if (p[m][n] && (worm[m][n].origini == m) && (worm[m][n].originj == n)) {
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

	int color = p[m][n];
	int other = OTHER_COLOR(color);

	int ai = worm[m][n].attacki;
	int aj = worm[m][n].attackj;
	int di = worm[m][n].defendi;
	int dj = worm[m][n].defendj;
	  
	/* For each worm, only work with the origin. */
	if (p[m][n] == EMPTY
	    || (worm[m][n].origini != m)
	    || (worm[m][n].originj != n))
	  continue;

	/* If the opponent has an attack on the worm (m, n), and we
	 * have not tried this move before, carry it out and see
	 * what it leads to.
	 */
	if ((ai != -1) && (mxother[ai][aj] == 0)) {

	  mxother[ai][aj] = 1;
	  /* First, carry out the attacking move. */
	  if (trymove(ai, aj, other, "make_worms", -1, -1,
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
		if (p[i][j]
		    && worm[i][j].origini == i
		    && worm[i][j].originj == j
		    && (i != m || j != n)) {

		  /* Either the worm is of the same color as (m, n),
		     then we try to attack it.  If there was a previous 
		     attack and defense of it, and there is no defence
		     for the attack now... */
		  if ((worm[i][j].color == color) 
		      && (worm[i][j].attack_code != 0)
		      && (worm[i][j].defend_code != 0)
		      && !find_defense(i, j, NULL, NULL)) {

		    int attack_works = 1;
		    /* Sometimes find_defense() fails to find a
		       defense which has been found by other means.
		       Try if the old defense move still works. */
		    if (worm[i][j].defendi != -1
			&& trymove(worm[i][j].defendi, worm[i][j].defendj,
				   color, "make_worms", -1, -1,
				   EMPTY, -1, -1)) {
		      if (!attack(i, j, NULL, NULL))
			attack_works = 0;
		      popgo();
		    }
		      
		    /* ...then move the attack point of that worm to
		       the attack point of (m, n). */
		    if (attack_works) {
		      TRACE("moving point of attack of %m to %m\n",
			    i, j, ai, aj);
		      worm[i][j].attacki = ai;
		      worm[i][j].attackj = aj;
		      add_attack_move(ai, aj, i, j);
		      mi[i][j] = 1;
		    }
		  }
		  /* Or the worm is of the opposite color as (m, n).
		     If there previously was an attack on it, but there
		     is none now, then move the defence point of (i, j)
		     to the defence point of (m, n). */
		  else if ((worm[i][j].color == other) 
			   && (worm[i][j].attack_code != 0) 
			   && !attack(i, j, NULL, NULL)) {
		    if (worm[i][j].defend_code != 0)
		      TRACE("moving point of defense of %m to %m\n",
			    i, j, di, dj);
		    else
		      TRACE("setting point of defense of %m to %m\n",
			    i, j, di, dj);
		    worm[i][j].defendi = ai;
		    worm[i][j].defendj = aj;
		    worm[i][j].defend_code = WIN;
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
	if ((di != -1) && (mxcolor[di][dj] == 0)) {

	  mxcolor[di][dj] = 1;
	  /* First carry out the defending move. */
	  if (trymove(di, dj, color, "make_worms", -1, -1, EMPTY, -1, -1)) {
	      
	    TRACE("trying %m\n", di, dj);
	    increase_depth_values();

	    for (i = 0; i < board_size; i++)
	      for (j = 0; j < board_size; j++) {

		/* If a worm has its origin (i, j), and it's not (m, n)...*/
		if (p[i][j]
		    && worm[i][j].origini == i
		    && worm[i][j].originj == j
		    && (i != m || j != n)) {

		  /* Either the worm is of the opposite color as (m, n),
		     then we try to attack it.  If there was a previous 
		     attack and defense of it, and there is no defence
		     for the attack now... */
		  if ((worm[i][j].color == other) 
		      && (worm[i][j].attack_code != 0) 
		      && (worm[i][j].defend_code != 0)
		      && !find_defense(i, j, NULL, NULL)) {

		    int attack_works = 1;
		    /* Sometimes find_defense() fails to find a
		       defense which has been found by other means.
		       Try if the old defense move still works. */
		    if (trymove(worm[i][j].defendi, worm[i][j].defendj,
				other, "make_worms", -1, -1,
				EMPTY, -1, -1)) {
		      if (!attack(i, j, NULL, NULL))
			attack_works = 0;
		      popgo();
		    }
		      
		    /* ...then move the attack point of that worm to
		       the defense point of (m, n). */
		    if (attack_works) {
		      TRACE("moving point of attack of %m to %m\n",
			    i, j, di, dj);
		      worm[i][j].attacki = di;
		      worm[i][j].attackj = dj;
		      add_attack_move(di, dj, i, j);
		      mi[i][j] = 1;
		    }
		  }
		  /* Or the worm is of the same color as (m, n).
		     If there previously was an attack on it, but there
		     is none now, then move the defence point of (i, j)
		     to the defence point of (m, n). */
		  else if ((worm[i][j].color == color)
			   && (worm[i][j].attack_code != 0) 
			   && !attack(i, j, NULL, NULL)) {
		    if (worm[i][j].defend_code != 0)
		      TRACE("moving point of defense of %m to %m\n",
			    i, j, di, dj);
		    else
		      TRACE("setting point of defense of %m to %m\n",
			    i, j, di, dj);
		    worm[i][j].defendi = di;
		    worm[i][j].defendj = dj;
		    worm[i][j].defend_code = WIN;
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
      if ((worm[m][n].origini != worm[m+1][n].origini
	   || worm[m][n].originj != worm[m+1][n].originj)
	  && p[m][n]!=EMPTY && p[m+1][n]!=EMPTY) {
        if (worm[m][n].attack_code != 0 && worm[m+1][n].attack_code != 0) {
	  if ((worm[m][n].defend_code == 0)
	      && (does_defend(worm[m+1][n].attacki,
			      worm[m+1][n].attackj, m, n))) {
	    /* FIXME: need to check ko relationship here */
	    change_defense(m, n, 
			   worm[m+1][n].attacki, worm[m+1][n].attackj, WIN);
	  }
	  if ((worm[m+1][n].defend_code == 0)
              && (does_defend(worm[m][n].attacki,
			      worm[m][n].attackj, m+1, n))) {
	    /* FIXME: need to check ko relationship here */	    
	    change_defense(m+1, n, 
			   worm[m][n].attacki, worm[m][n].attackj, WIN);
	  }
        }
      }
  
  /* Then look for horizontal neighbors. */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size-1; n++)
      if ((worm[m][n].origini != worm[m][n+1].origini ||
	   worm[m][n].originj != worm[m][n+1].originj) &&
	  p[m][n]!=EMPTY && p[m][n+1]!=EMPTY) {
        if (worm[m][n].attack_code != 0 && worm[m][n+1].attack_code != 0) {
	  if ((worm[m][n].defend_code == 0)
              && (does_defend(worm[m][n+1].attacki,
			      worm[m][n+1].attackj, m, n))) {
	    /* FIXME: need to check ko relationship here */	    
	    change_defense(m, n, 
			   worm[m][n+1].attacki, worm[m][n+1].attackj, WIN);
	  }
	  if ((worm[m][n+1].defend_code == 0)
              && (does_defend(worm[m][n].attacki,
			      worm[m][n].attackj, m, n+1))) {
	    /* FIXME: need to check ko relationship here */	    
	    change_defense(m, n+1, 
			   worm[m][n].attacki, worm[m][n].attackj, WIN);
	  }
	}
      }
  
  gg_assert(stackp == 0);
  
  /* Find adjacent worms that can be easily captured, aka lunches. */

  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      int i,j;

      if (p[m][n] == EMPTY
	  || worm[m][n].origini != m
	  || worm[m][n].originj != n)
	continue;

      if (find_lunch(m, n, &i, &j, NULL, NULL)
	  && (worm[i][j].attack_code == WIN
	      || worm[i][j].attack_code == KO_A)) {
	TRACE("lunch found for %m at %m\n", m, n, i, j);
	worm[m][n].lunchi = i;
	worm[m][n].lunchj = j;
      }
      else {
	worm[m][n].lunchi = -1;
	worm[m][n].lunchj = -1;
      }
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
    for (n = 0; n < board_size; n++) {
      worm[m][n].color = p[m][n];
      worm[m][n].origini = m;
      worm[m][n].originj = n;
      worm[m][n].ko = 0;
      worm[m][n].inessential = 0;
      worm[m][n].invincible = 0;
      worm[m][n].unconditional_status = UNKNOWN;
      worm[m][n].effective_size = 0.0;
      if (p[m][n] != EMPTY) {
	worm[m][n].liberties = countlib(m, n);
	worm[m][n].size = countstones(m, n);
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
	
      if (p[m][n] == EMPTY)
	distance[m][n] = -1;
      else {
	distance[m][n] = 0;
	wormi[m][n][0] = worm[m][n].origini;
	wormj[m][n][0] = worm[m][n].originj;
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
	if (p[m][n] == EMPTY)
	  worm[ai][aj].effective_size += 0.5/nworms[m][n];
	else
	  worm[ai][aj].effective_size += 1.0;
      }
	
  /* Propagate the effective size values all over the worms. */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++)
      if (p[m][n] != EMPTY
	  && worm[m][n].origini == m && worm[m][n].originj == n)
	propagate_worm(m, n);
}


/*
 * Analyze tactical safety of each worm. 
 */

static void
find_worm_attacks_and_defenses(void)
{
  int m, n;

   /* 1. Start with finding attack points. */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      int acode;
      int ti, tj;

      if (p[m][n] == EMPTY
	  || worm[m][n].origini != m
	  || worm[m][n].originj != n)
	continue;

      TRACE ("considering attack of %m\n", m, n);
      /* Initialize all relevant fields at once. */
      worm[m][n].attacki = -1;
      worm[m][n].attackj = -1;
      worm[m][n].attack_code = 0;
      worm[m][n].defendi = -1;
      worm[m][n].defendj = -1;
      worm[m][n].defend_code = 0;
      acode = attack(m, n, &ti, &tj);
      if (acode) {
	TRACE("worm at %m can be attacked at %m\n", m, n, ti, tj);
	worm[m][n].attacki = ti;
	worm[m][n].attackj = tj;
	worm[m][n].attack_code = acode;
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

      if (p[m][n] == EMPTY
	  || (worm[m][n].origini != m)
	  || (worm[m][n].originj != n))
	continue;

      if (worm[m][n].attack_code != 0) {
	int ti, tj;
	TRACE ("considering defense of %m\n", m, n);
	dcode = find_defense(m, n, &ti, &tj);
	if (dcode) {
	  TRACE ("worm at %m can be defended at %m\n", m, n, ti, tj);
	  worm[m][n].defendi = ti;
	  worm[m][n].defendj = tj;
	  worm[m][n].defend_code = dcode;
	  if (ti != -1)
	    add_defense_move(ti, tj, m, n);
	}
	else {
	  /* If the point of attack is not adjacent to the worm, 
	   * it is possible that this is an overlooked point of
	   * defense, so we try and see if it defends.
	   */
	  int ai = worm[m][n].attacki;
	  int aj = worm[m][n].attackj;
	  if (!liberty_of_string(ai, aj, m, n))
	    if (trymove(ai, aj, worm[m][n].color, "make_worms", -1, -1,
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
		  worm[m][n].defendi = ai;
		  worm[m][n].defendj = aj;
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
    static int libi[MAXLIBS];
    static int libj[MAXLIBS];
    int k;
    int color;
    int other;
    int libs;

    for (m = 0; m < board_size; m++)
      for (n = 0; n < board_size; n++) {
	color = p[m][n];
	if (color == EMPTY
	    || worm[m][n].origini!=m
	    || worm[m][n].originj!=n)
	  continue;

	other = OTHER_COLOR(color);
	
	if (worm[m][n].attack_code == 0)
	  continue;
	
	/* There is at least one attack on this group. Try the
	 * liberties.
	 */
	libs = findlib(m, n, MAXLIBS, libi, libj);
	
	for (k = 0; k < libs; k++) {
	  int ai = libi[k];
	  int aj = libj[k];
	  /* Try to attack on the liberty. */
	  if (trymove(ai, aj, other, "make_worms", -1, -1,
		      EMPTY, -1, -1)) {
	    if (!p[m][n] || attack(m, n, NULL, NULL)) {
	      int dcode;
	      if (!p[m][n])
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
	    if (trymove(ai, aj, color, "make_worms", -1, -1,
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
      int libs;
      static int libi[MAXLIBS];
      static int libj[MAXLIBS];

      int k;
      int l;
      int color;
      int other;

      /* Only work with origins. */
      color = p[m][n];
      if (color == EMPTY
	  || (worm[m][n].origini != m)
	  || (worm[m][n].originj != n))
	continue;

      other = OTHER_COLOR(color);

      /* 1. Start with finding attack threats. */
      /* Only try those worms that have no attack. */
      if (worm[m][n].attack_code == 0) {

	libs = findlib(m, n, MAXLIBS, libi, libj);

	/* This test would seem to be unnecessary since we only attack
	 * strings with attack_code == 0, but it turns out that single
	 * stones with one liberty that can be captured, but come to
	 * live again in a snap-back get attack_code == 0. 
	 *
	 * The test against 6 liberties is just an optimization.
	 */
	if (libs > 1 && libs < 6) {
	  for (k = 0; k < libs; k++) {
	    int ai = libi[k];
	    int aj = libj[k];

	    /* Try to threaten on the liberty. */
	    if (trymove(ai, aj, other, "threaten attack", m, n,
			EMPTY, -1, -1)) {
	      /* FIXME: Support ko also. */
	      if (attack(m, n, NULL, NULL) == WIN)
		add_attack_threat_move(ai, aj, m, n);
	      popgo();
	    }

	    /* Try to threaten on second order liberties. */
	    for (l = 0; l < 4; l++) {
	      int bi = libi[k] + deltai[l];
	      int bj = libj[k] + deltaj[l];

	      if (!ON_BOARD(bi, bj)
		  || p[bi][bj] != EMPTY
		  || liberty_of_string(bi, bj, m, n))
		continue;

	      if (trymove(bi, bj, other, "threaten attack", m, n,
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

	libs = findlib(m, n, MAXLIBS, libi, libj);

	for (k = 0; k < libs; k++) {
	  int ai = libi[k];
	  int aj = libj[k];

	  /* Try to threaten on the liberty. */
	  if (trymove(ai, aj, color, "threaten defense", -1, -1,
		      EMPTY, -1, -1)) {
	    /* FIXME: Support ko also. */
	    if (attack(m, n, NULL, NULL) == WIN
		&& find_defense(m, n, NULL, NULL) == WIN)
	      add_defense_threat_move(ai, aj, m, n);
	    popgo();
	  }

	  /* Try to threaten on second order liberties. */
	  for (l = 0; l < 4; l++) {
	    int bi = libi[k] + deltai[l];
	    int bj = libj[k] + deltaj[l];

	    if (!ON_BOARD(bi, bj)
		|| p[bi][bj] != EMPTY
		|| liberty_of_string(bi, bj, m, n))
	      continue;

	    if (trymove(bi, bj, other, "threaten defense", m, n,
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
  return (worm[ai][aj].origini == worm[bi][bj].origini
	  && worm[ai][aj].originj == worm[bi][bj].originj);
}


/*
 * Test whether the origin of the worm at (wi, wj) is (i, j).
 */

int
is_worm_origin(int wi, int wj, int i, int j)
{
  return (worm[wi][wj].origini == i
	  && worm[wi][wj].originj == j);
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
  int stones;
  int stonei[MAX_BOARD * MAX_BOARD];
  int stonej[MAX_BOARD * MAX_BOARD];
  gg_assert(stackp == 0);
  ASSERT(p[m][n] != EMPTY, m, n);

  stones = findstones(m, n, MAX_BOARD * MAX_BOARD, stonei, stonej);
  for (k = 0; k < stones; k++)
    if (stonei[k] != m || stonej[k] != n)
      worm[stonei[k]][stonej[k]] = worm[m][n];
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
  int libi[MAXLIBS];
  int libj[MAXLIBS];
  int mrc[MAX_BOARD][MAX_BOARD];
  int mse[MAX_BOARD][MAX_BOARD];
  int color = p[i][j];
  int other = OTHER_COLOR(color);

  memset(mse, 0, sizeof(mse));

  /* Find and mark the first order liberties. */
  *lib1 = findlib(i, j, MAXLIBS, libi, libj);
  for (k = 0; k < *lib1; k++)
    mse[libi[k]][libj[k]] = 1;

  /* Reset mse at liberties which are flanked by two stones of the
   * opposite color, or one stone and the edge.
   */

  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++)
      if (mse[m][n]
	  && (((m == 0 || p[m-1][n] == other)
	       && (m == board_size-1 || p[m+1][n] == other))
	      ||((n == 0 || p[m][n-1] == other)
	       && (n == board_size-1 || p[m][n+1] == other))))
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
  mr[i][j] = 1;
  if (i > 0
      && mx[i-1][j] == 0
      && p[i-1][j] == EMPTY
      && mr[i-1][j] == 0
      && !touching(i-1, j, OTHER_COLOR(color)))
  {
    (*counter)++;
    mr[i-1][j] = 1;
    mx[i-1][j] = 1;
  }
  
  if (i < board_size-1
      && mx[i+1][j] == 0
      && p[i+1][j] == EMPTY
      && mr[i+1][j] == 0
      && !touching(i+1, j, OTHER_COLOR(color)))
  {
    (*counter)++;
    mr[i+1][j] = 1;
    mx[i+1][j] = 1;
  }
  
  if (j > 0
      && mx[i][j-1] == 0
      && p[i][j-1] == EMPTY
      && mr[i][j-1] == 0
      && !touching(i, j-1, OTHER_COLOR(color)))
  {
    (*counter)++;
    mr[i][j-1] = 1;
    mx[i][j-1] = 1;
  }
  
  if (j < board_size-1
      && mx[i][j+1] == 0
      && p[i][j+1] == EMPTY
      && mr[i][j+1] == 0
      && !touching(i, j+1, OTHER_COLOR(color)))
  {
    (*counter)++;
    mr[i][j+1] = 1;
    mx[i][j+1] = 1;
  }

  if (!worm[i][j].ko) {
    if (i > 0
	&& mr[i-1][j] == 0
	&& (mx[i-1][j] == 1
	    || p[i-1][j] == color))
      ping_recurse(i-1, j, counter, mx, mr, color);

    if (i < board_size-1
	&& mr[i+1][j] == 0
	&& (mx[i+1][j] == 1
	    || p[i+1][j] == color))
      ping_recurse(i+1, j, counter, mx, mr, color);

    if (j > 0
	&& mr[i][j-1] == 0
	&& (mx[i][j-1] == 1
	    || p[i][j-1] == color))
      ping_recurse(i, j-1, counter, mx, mr, color);

    if (j < board_size-1
	&& mr[i][j+1] == 0
	&& (mx[i][j+1] == 1
	    || p[i][j+1] == color))
      ping_recurse(i, j+1, counter, mx, mr, color);
  }
}


/* touching(i, j, color) returns true if the vertex at (i, j) is
 * touching any stone of (color).
 */

static int
touching(int i, int j, int color)
{
  if (i > 0 && p[i-1][j] == color)
    return 1;

  if (i < board_size-1 && p[i+1][j] == color)
    return 1;

  if (j > 0 && p[i][j-1] == color)
    return 1;

  if (j < board_size-1 && p[i][j+1] == color)
    return 1;

  return 0;
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
      if (!mg[m][n] && (p[m][n] == EMPTY
			|| worm[m][n].origini != i
			|| worm[m][n].originj != j))
      {
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
  mg[m][n] = 1;
  if ((m > 0) && (mg[m-1][n] == 0))
    if (p[m-1][n] == EMPTY
	|| worm[m-1][n].origini != i
	|| worm[m-1][n].originj != j)
      markcomponent(i, j, m-1, n, mg);

  if ((m < board_size-1) && (mg[m+1][n] == 0))
    if (p[m+1][n] == EMPTY 
	|| worm[m+1][n].origini != i
	|| worm[m+1][n].originj != j)
      markcomponent(i, j, m+1, n, mg);

  if (n > 0 && mg[m][n-1] == 0)
    if (p[m][n-1] == EMPTY
	|| worm[m][n-1].origini != i
	|| worm[m][n-1].originj != j)
      markcomponent(i, j, m, n-1, mg);

  if (n < board_size-1 && mg[m][n+1] == 0)
    if (p[m][n+1] == EMPTY
	|| worm[m][n+1].origini != i
	|| worm[m][n+1].originj != j)
      markcomponent(i, j, m, n+1, mg);
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
  
  ASSERT_ON_BOARD(m, n);

  ASSERT(((vertexi == NULL) ^ (vertexj == NULL)) == 0, m, n);
  
  memset(ml, 0, sizeof(ml));

  sz = 0;
  if (edge)
    *edge = 0;

  if (p[m][n] != EMPTY)
    find_origin(m, n, &oi, &oj);
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
  ASSERT(mx[i][j] == 0, i, j);

  mx[i][j] = 1;

  if (vertexi) {
    vertexi[*size] = i;
    vertexj[*size] = j;
  }
  (*size)++;
  
  if ((edge)
      && ((i == 0) || (i == board_size-1) || (j == 0) || (j == board_size-1))
      && (p[i][j] == EMPTY)) 
    (*edge)++;

  /* Loop over the four neighbors. */
  for (k = 0; k < 4; k++) {
    int di = deltai[k];
    int dj = deltaj[k];
    if (ON_BOARD(i+di, j+dj) && !mx[i+di][j+dj]) {
      int neighbor_empty = 0;
      
      if (p[i+di][j+dj] == EMPTY)
	neighbor_empty = 1;
      else {
	/* Count the neighbor as empty if it is part of the (ai, aj) string. */
	int oi, oj;
	find_origin(i+di, j+dj, &oi, &oj);
	if (oi == ai && oj == aj)
	  neighbor_empty = 1;
	else
	  neighbor_empty = 0;
      }
      
      if (!neighbor_empty)
	*border_color |= p[i+di][j+dj];
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
  int m, n;

  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++)
      if (p[m][n])
	matchpat(m, n, attack_callback, OTHER_COLOR(p[m][n]), &attpat_db, NULL, NULL);
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

      ai = worm[x][y].origini;
      aj = worm[x][y].originj;

      /* A string with 5 liberties or more is considered tactically alive. */
      if (countlib(ai, aj) > 4)
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
      if (trymove(ti, tj, color, "attack_callback", ai, aj,
		  EMPTY, -1, -1)) {
	int dcode;
	if (!p[ai][aj])
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
	  worm[ai][aj].attacki = ti;
	  worm[ai][aj].attackj = tj;
	}
	else if (dcode == KO_A) {
	  TRACE("Attack pattern %s+%d found attack on %m at %m with ko (acode=%d)\n",
		pattern->name, ll, ai, aj, ti, tj, KO_A);
	  if (worm[ai][aj].attack_code == KO_B 
	      || worm[ai][aj].attack_code == 0) {
	    worm[ai][aj].attack_code = KO_B;
	    worm[ai][aj].attacki = ti;
	    worm[ai][aj].attackj = tj;
	  }
	}
	else if (dcode == KO_B) {
	  TRACE("Attack pattern %s+%d found attack on %m at %m with ko (acode=%d)\n",
		pattern->name, ll, ai, aj, ti, tj, KO_B);
	  if (worm[ai][aj].attack_code != WIN) {
	    worm[ai][aj].attack_code = KO_A;
	    worm[ai][aj].attacki = ti;
	    worm[ai][aj].attackj = tj;
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
  int m, n;

  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++)
      if (p[m][n])
	matchpat(m, n, defense_callback, p[m][n], &defpat_db, NULL, NULL);
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

      ai = worm[x][y].origini;
      aj = worm[x][y].originj;

      if (worm[ai][aj].attack_code == 0
	  || defense_move_known(ti, tj, ai, aj))
	continue;
      
      /* FIXME: Don't try to defend the same string more than once.
       * FIXME: For all attacks on this string, we should test whether
       *        the proposed move happens to refute the attack.
       * Play (ti, tj) and see if there is an attack. */
      if (trymove(ti, tj, color, "defense_callback", ai, aj,
		  EMPTY, -1, -1)) {
	int acode = attack(ai, aj, NULL, NULL);

	popgo();
	
	if (acode == 0) {
	  TRACE("Defense pattern %s+%d found defense of %m at %m\n",
		pattern->name, ll, ai, aj, ti, tj);
	  worm[ai][aj].defend_code = WIN;
	  worm[ai][aj].defendi = ti;
	  worm[ai][aj].defendj = tj;
	}
	else if (acode == KO_A) {
	  TRACE("Defense pattern %s+%d found defense of %m at %m with ko (acode=%d)\n",
		pattern->name, ll, ai, aj, ti, tj, 3);
	  if (worm[ai][aj].defend_code != WIN) {
	    worm[ai][aj].defend_code = KO_B;
	    worm[ai][aj].defendi = ti;
	    worm[ai][aj].defendj = tj;
	  }
	}
	else if (acode == KO_B) {
	  TRACE("Defense pattern %s+%d found defense of %m at %m with ko (acode=%d)\n",
		pattern->name, ll, ai, aj, ti, tj, 3);
	  if (worm[ai][aj].defend_code != WIN) {
	    worm[ai][aj].defend_code = KO_A;
	    worm[ai][aj].defendi = ti;
	    worm[ai][aj].defendj = tj;
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
  if (p[m][n] == EMPTY) {
    gprintf("There is no worm at %m\n", m, n);
    return;
  }

  gprintf("*** worm at %m:\n", m, n);
  gprintf("color: %s; origin: %m; size: %d; effective size: %f\n",
	  (worm[m][n].color == WHITE) ? "White" : "Black",
	  worm[m][n].origini, worm[m][n].originj,
	  worm[m][n].size, worm[m][n].effective_size);

  gprintf("liberties: %d order 2 liberties:%d order 3:%d order 4:%d\n",
	  worm[m][n].liberties, 
	  worm[m][n].liberties2, 
	  worm[m][n].liberties3, 
	  worm[m][n].liberties4);

  if (worm[m][n].attacki != -1)
    gprintf("attack point %m, ",
	    worm[m][n].attacki, worm[m][n].attackj);
  else
    gprintf("no attack point, ");

  if (worm[m][n].attack_code == 1)
    gprintf("attack code WIN\n");
  else if (worm[m][n].attack_code == KO_A)
    gprintf("attack code KO_A\n");
  else if (worm[m][n].attack_code == KO_B)
    gprintf("attack code KO_B\n");

  if (worm[m][n].defendi != -1)
    gprintf("defense point %m, ",
	    worm[m][n].defendi, worm[m][n].defendj);
  else
    gprintf("no defense point, ");

  if (worm[m][n].defend_code == 1)
    gprintf("defend code WIN\n");
  else if (worm[m][n].defend_code == KO_A)
    gprintf("defend code KO_A\n");
  else if (worm[m][n].defend_code == KO_B)
    gprintf("defend code KO_B\n");

  if (worm[m][n].lunchi != -1)
    gprintf("lunch at %m\n", worm[m][n].lunchi, worm[m][n].lunchj);

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
