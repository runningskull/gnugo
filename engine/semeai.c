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

#include "liberty.h"

#define INFINITY 1000

static void analyze_semeai(int m, int n, int i, int j);
static void add_appropriate_semeai_moves(int ti, int tj, 
					 int ai, int aj, int bi, int bj, 
					 int my_status, int your_status,
					 int margin_of_safety);

/* semeai() searches for pairs of dragons of opposite color which
 * have safety DEAD. If such a pair is found, analyze_semeai is
 * called to determine which dragon will prevail in a semeai, and
 * whether a move now will make a difference in the outcome. The
 * dragon statuses are revised, and if a move now will make a
 * difference in the outcome, an owl reason is generated.
 */

void
semeai(int color)
{
  int d1, d2;
  int k;
  int ai = -1, aj = -1;
  int bi = -1, bj = -1;
  int other = OTHER_COLOR(color);

  TRACE("Semeai Player is THINKING for %s!\n", 
	color_to_string(color));

  for (d1 = 0; d1 < number_of_dragons; d1++) {
    if (DRAGON(d1).color != color
	|| (DRAGON(d1).matcher_status != DEAD
	    && DRAGON(d1).matcher_status != CRITICAL))
      continue;

    for (k = 0; k < dragon2[d1].neighbors; k++) {
      d2 = dragon2[d1].adjacent[k];
      if (DRAGON(d2).color != other
	  || (DRAGON(d2).matcher_status != DEAD
	      && DRAGON(d2).matcher_status != CRITICAL))
	continue;

      /* Dragons d1 (our) and d2 (opponent) are adjacent and both DEAD
       * or CRITICAL.
       */
      ai = I(DRAGON(d1).origin);
      aj = J(DRAGON(d1).origin);
      bi = I(DRAGON(d2).origin);
      bj = J(DRAGON(d2).origin);

      /* Ignore inessential worms or dragons */
      if (worm[POS(ai, aj)].inessential 
	  || DRAGON2(POS(ai, aj)).safety == INESSENTIAL
	  || worm[POS(bi, bj)].inessential 
	  || DRAGON2(POS(bi, bj)).safety == INESSENTIAL)
	continue;

      analyze_semeai(ai, aj, bi, bj);      
    }
  }
}

/* liberty_of_dragon(i, j, m, n) returns true if the vertex at (i, j) is a
 * liberty of the dragon with origin at (m, n).
 */

static int 
liberty_of_dragon(int i, int j, int m, int n)
{
  if (i == -1)
    return 0;

  if (BOARD(i, j) != EMPTY)
    return 0;

  if (i > 0
      && dragon[POS(i-1, j)].origin == POS(m, n))
    return 1;

  if (i < board_size - 1
      && dragon[POS(i+1, j)].origin == POS(m, n))
    return 1;

  if (j > 0
      && dragon[POS(i, j-1)].origin == POS(m, n))
    return 1;

  if (j < board_size - 1
      && dragon[POS(i, j+1)].origin == POS(m, n))
    return 1;

  return 0;
}



/* analyzes a pair of adjacent dragons which are 
 * DEAD or CRITICAL.
 */
static void
analyze_semeai(int ai, int aj, int bi, int bj)
{
  /* We start liberty counts at 1 since we will be subtracting
   * the number of worms. */
  int mylibs = 1, yourlibs = 1, commonlibs = 0; 
  int yourlibi = -1, yourlibj = -1;
  int commonlibi = -1, commonlibj = -1;
  int color = BOARD(ai, aj);
  int i, j;
  int m, n;
  int my_status = UNKNOWN;
  int your_status = UNKNOWN;
  int di, dj;
  int margin_of_safety = 0;
  int owl_code_sufficient = 0;
  
  DEBUG(DEBUG_SEMEAI, "semeai_analyzer: %m (me) vs %m (them)\n",
	ai, aj, bi, bj);

  /* If both dragons are owl-critical, or my dragon is owl-critical
   * and your dragon is owl-dead, and the attack point for my dragon
   * owl_does_defend your dragon, add another owl defend move reason
   * and possibly change the owl status of your dragon to critical.
   *
   * Correction: We can't add an owl defense move reason here because
   * this would be a defense of an opponent dragon.
   */
  if (dragon[POS(ai, aj)].owl_status == CRITICAL
      && (dragon[POS(bi, bj)].owl_status == CRITICAL
	  || dragon[POS(bi, bj)].owl_status == DEAD)) {
    if (dragon[POS(bi, bj)].owl_defense_point == dragon[POS(ai, aj)].owl_attack_point)
      return;
    if (dragon[POS(ai, aj)].owl_attack_point != NO_MOVE
	&& owl_does_defend(I(dragon[POS(ai, aj)].owl_attack_point),
			   J(dragon[POS(ai, aj)].owl_attack_point), bi, bj)) {
#if 0
      add_owl_defense_move(dragon[POS(ai, aj)].owl_attacki,
			   dragon[POS(ai, aj)].owl_attackj,
			   bi, bj);
      DEBUG(DEBUG_SEMEAI, "added owl defense of %m at %m\n",
	    bi, bj, dragon[POS(ai, aj)].owl_defendi, dragon[POS(ai, aj)].owl_defendj);
#endif
      if (dragon[POS(bi, bj)].owl_status == DEAD) {
	for (m = 0; m < board_size; m++)
	  for (n = 0; n < board_size; n++)
	    if (BOARD(m, n) == BOARD(bi, bj) && same_dragon(POS(m, n), POS(bi, bj))) {
	      dragon[POS(m, n)].owl_status = CRITICAL;
	      dragon[POS(m, n)].matcher_status = CRITICAL;
	    }
	DEBUG(DEBUG_SEMEAI,
	      "changed owl_status and matcher_status of %m to CRITICAL\n",
	      bi, bj);
      }
      owl_code_sufficient = 1;
    }
  }

  /* If both dragons are owl-critical, and the defense point for my
   * dragon owl_does_attack your dragon, add another owl attack move
   * reason.
   */
  if ((dragon[POS(ai, aj)].owl_status == CRITICAL)
      && (dragon[POS(bi, bj)].owl_status == CRITICAL)) {
    if (dragon[POS(bi, bj)].owl_attack_point == dragon[POS(ai, aj)].owl_defense_point)
      return;
    if (dragon[POS(ai, aj)].owl_defense_point != NO_MOVE
	&& owl_does_attack(I(dragon[POS(ai, aj)].owl_defense_point),
			   J(dragon[POS(ai, aj)].owl_defense_point), bi, bj)) {
      add_owl_attack_move(dragon[POS(ai, aj)].owl_defense_point,
			  POS(bi, bj));
      DEBUG(DEBUG_SEMEAI, "added owl attack of %m at %1m\n",
	    bi, bj, dragon[POS(ai, aj)].owl_defense_point);
      owl_code_sufficient = 1;
    }
  }

  /* If both dragons are owl-critical, or your dragon is owl-critical
   * and my dragon is owl-dead, and the attack point for your dragon
   * owl_does_defend my dragon, add another owl defense move reason
   * and possibly change the owl status of my dragon to critical.
   */
  if ((dragon[POS(ai, aj)].owl_status == CRITICAL
       || dragon[POS(ai, aj)].owl_status == DEAD)
      && dragon[POS(bi, bj)].owl_status == CRITICAL) {
    if (dragon[POS(bi, bj)].owl_attack_point == dragon[POS(ai, aj)].owl_defense_point)
      return;
    if (dragon[POS(bi, bj)].owl_attack_point != NO_MOVE
	&& owl_does_defend(I(dragon[POS(bi, bj)].owl_attack_point),
			   J(dragon[POS(bi, bj)].owl_attack_point), ai, aj)) {
      add_owl_defense_move(dragon[POS(bi, bj)].owl_attack_point,
			   POS(ai, aj));
      DEBUG(DEBUG_SEMEAI, "added owl defense of %m at %1m\n",
	    ai, aj, dragon[POS(bi, bj)].owl_attack_point);
      if (dragon[POS(ai, aj)].owl_status == DEAD) {
	for (m = 0; m < board_size; m++)
	  for (n = 0; n < board_size; n++)
	    if (BOARD(m, n) == BOARD(ai, aj) && same_dragon(POS(m, n), POS(ai, aj))) {
	      dragon[POS(m, n)].owl_status = CRITICAL;
	      dragon[POS(m, n)].matcher_status = CRITICAL;
	    }
	DEBUG(DEBUG_SEMEAI,
	      "changed owl_status and matcher_status of %m to CRITICAL\n",
	      ai, aj);
      }
      owl_code_sufficient = 1;
    }
  }

  /* If both dragons are owl-critical, and the defense point for your
   * dragon owl_does_attack my dragon, add another owl attack move
   * reason.
   *
   * Correction: We can't add an owl attack move reason here because
   * this would be an attack on our own dragon.
   */
  if ((dragon[POS(ai, aj)].owl_status == CRITICAL)
      && (dragon[POS(bi, bj)].owl_status == CRITICAL)) {
    if (dragon[POS(bi, bj)].owl_defense_point == dragon[POS(ai, aj)].owl_attack_point)
      return;
    if (dragon[POS(bi, bj)].owl_defense_point != NO_MOVE
	&& owl_does_attack(I(dragon[POS(bi, bj)].owl_defense_point),
			   J(dragon[POS(bi, bj)].owl_defense_point), ai, aj)) {
#if 0
      add_owl_attack_move(dragon[POS(bi, bj)].owl_defendi,
			  dragon[POS(bi, bj)].owl_defendj,
			  ai, aj);
      DEBUG(DEBUG_SEMEAI, "added owl attack of %m at %m\n",
	    ai, aj, dragon[POS(bi, bj)].owl_attacki, dragon[POS(bi, bj)].owl_attackj);
#endif
      owl_code_sufficient = 1;
    }
  }

  /* If the owl code was able to resolve the semeai, exit. */
  if (owl_code_sufficient) {
    DEBUG(DEBUG_SEMEAI, "...owl code sufficient to resolve semeai, exiting\n");
    return;
  }


  /* The semeai module is prone to errors since semeai cannot
   * really be handled by static analysis. It is really only needed
   * when the dragons have many liberties since tight situations
   * can be handled by the tactical reading code. Thus we exclude
   * dragon pairs where either has a tactically DEAD or CRITICAL
   * string which is adjacent to the other dragon which is owl
   * substantial.
   */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      if (worm[POS(m, n)].origin == POS(m, n)
	  && worm[POS(m, n)].attack_codes[0] == WIN)
	if (dragon[POS(m, n)].origin == POS(ai, aj)
	    || dragon[POS(m, n)].origin == POS(bi, bj)) {
	  int adj;
	  int adjs[MAXCHAIN];
	  int r;
	  
	  adj = chainlinks(POS(m, n), adjs);
	  for (r = 0; r < adj; r++) {
	    int ci = I(adjs[r]);
	    int cj = J(adjs[r]);
	    if (dragon[POS(ci, cj)].origin == POS(ai, aj)
		|| dragon[POS(ci, cj)].origin == POS(bi, bj))
	      if (owl_substantial(m, n)) {
		DEBUG(DEBUG_SEMEAI, "...tactical situation detected, exiting\n");
		return;
	      }
	  }
	}
    }
  
  
  /* Mark the dragons as involved in semeai */
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++)
      if (same_dragon(POS(i, j), POS(ai, aj)) || same_dragon(POS(i, j), POS(bi, bj)))
	DRAGON2(POS(i, j)).semeai = 1;

  /* First we try to determine the number of liberties of each
   * dragon, and the number of common liberties. We subtract
   * 1 minus the number of worms of the dragon from the liberty
   * count, since if a dragon has several worms, a move may
   * have to be invested in connecting them. At the same time
   * we try to find a liberty of the opponent's dragon, and a
   * common liberty, for future reference.
   */
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++) {
      if (BOARD(i, j)
	  && worm[POS(i, j)].origin == POS(i, j)) {
	if (same_dragon(POS(i, j), POS(ai, aj)))
	  mylibs--;
	if (same_dragon(POS(i, j), POS(bi, bj)))
	  yourlibs--;
      }
      else if (BOARD(i, j) == EMPTY) {
	if (liberty_of_dragon(i, j, bi, bj)) {
	  yourlibs ++;
	  if (liberty_of_dragon(i, j, ai, aj)) {
	    commonlibs++;
	    mylibs++;
	    commonlibi = i;
	    commonlibj = j;
	  }
	  else {
	    yourlibi = i;
	    yourlibj = j;
	  }
	}
	else if (liberty_of_dragon(i, j, ai, aj))
	  mylibs++;
      }
    }
  /* We add 1 to the
   * number of liberties of an owl critical dragon if the point
   * of attack is not a liberty of the dragon, since a move
   * may have to be invested in attacking it.
   */

  if (dragon[POS(ai, aj)].owl_status == CRITICAL
      && !liberty_of_string(dragon[POS(ai, aj)].owl_attack_point, POS(ai, aj)))
    mylibs++;
  
  if (dragon[POS(bi, bj)].owl_status == CRITICAL
      && !liberty_of_string(dragon[POS(bi, bj)].owl_attack_point, POS(bi, bj)))
    yourlibs++;
  
  /* Now we compute the statuses which result from a
   * naive comparison of the number of liberties. There
   * is some uncertainty in these calculations, so we
   * must exercise caution in applying the results.
   *
   * RULES FOR PLAYING SEMEAI. Let M be the number of liberties
   * of my group, excluding common liberties; let Y be the
   * number of liberties of your group, excluding common
   * liberties; and let C be the number of common liberties.
   * 
   *             If both groups have zero eyes:
   * 
   * (1)  If C=0 and M=Y, whoever moves first wins. CRITICAL.
   * (2)  If C=0 and M>Y, I win.
   * (3)  If C=0 and M<Y, you win.
   * (4)  If C>0 and M >= Y+C then your group is dead and mine is alive.
   * (5)  If C>0 and M = Y+C-1 then the situation is CRITICAL. 
   * (5a) If M=0, then Y=0 and C=1. Whoever moves first kills.
   * (5b) If M>0, then I can kill or you can make seki.
   * (6)  If M < Y+C-1 and Y < M+C-1 then the situation is seki.
   * (7)  If C>0 and Y=M+C-1 the situation is CRITICAL. 
   * (7a) If Y=0, then M=0 and C=1 as in (5). 
   * (7b) If Y>0, you can kill or I can make seki.
   * (8)  If C>0 and Y > M+C then your group is alive and mine is dead.
   *
   *              If both groups have one eye:
   *
   * In this case M > 0 and Y > 0.
   * 
   * (1) If M>C+Y then I win.
   * (2) If Y>C+M then you win.
   * (3) If C=0 and M=Y then whoever moves first kills. CRITICAL.
   * (4) If C>0 and M=C+Y then I can kill, you can make seki. CRITICAL.
   * (5) If C>0 and M<C+Y, Y<C+M, then the situation is seki. 
   * (6) If C>0 and Y=C+M, then you can kill, I can make seki. CRITICAL.
   *
   *            If I have an eye and you dont:
   * 
   * In this case, M > 0. This situation (me ari me nashi) can
   * never be seki. The common liberties must be filled by you,
   * making it difficult to win.
   * 
   * (1) If M+C>Y then I win.
   * (2) If M+C=Y then whoever moves first wins. CRITICAL.
   * (3) If M+C<Y then you win.
   *
   *            If you have an eye and I don't
   * 
   * In this case, Y > 0. 
   * 
   * (1) If Y+C>M you win.
   * (2) If Y+C=M whoever moves first wins. CRITICAL.
   * (3) If Y+C<M I win.  */

  if (DRAGON2(POS(ai, aj)).genus == 0
      && DRAGON2(POS(bi, bj)).genus == 0) {
    if (commonlibs == 0) {
      if (mylibs > yourlibs) {
	my_status = ALIVE;
	your_status = DEAD;
	margin_of_safety = mylibs - yourlibs;
      }
      else if (mylibs < yourlibs) {
	my_status = DEAD;
	your_status = ALIVE;
	margin_of_safety = yourlibs - mylibs;
      }
      else {
	my_status = CRITICAL;
	your_status = CRITICAL;
	margin_of_safety = 0;
      }
    }
    else if (mylibs == yourlibs + commonlibs - 1) {
      if (mylibs == 0) {
	my_status = CRITICAL;
	your_status = CRITICAL;
	margin_of_safety = 0;
      }
      else {
	/* I can kill, you can make seki */
	my_status = ALIVE;
	your_status = CRITICAL;
	margin_of_safety = 0;
      }
    }
    else if ((mylibs < yourlibs + commonlibs - 1)
	     && (yourlibs < mylibs+commonlibs - 1)) {
      /* Seki */
      my_status = ALIVE;
      your_status = ALIVE;
      margin_of_safety = INFINITY; 
    }
    else if ((commonlibs > 0) 
	     && (yourlibs == mylibs + commonlibs - 1)) {
      if (yourlibs == 0) {
	my_status = CRITICAL;
	your_status = CRITICAL;
	margin_of_safety = 0;
      }
      else {
	/* you can kill, I can make seki */
	my_status = CRITICAL;
	your_status = ALIVE;
	margin_of_safety = 0;
      }
    }
    else if ((commonlibs > 0) 
	     && yourlibs > mylibs + commonlibs) {
      my_status = DEAD;
      your_status = ALIVE;
      margin_of_safety = yourlibs - mylibs - commonlibs;
    }
  }
  if (DRAGON2(POS(ai, aj)).genus > 0
      && DRAGON2(POS(bi, bj)).genus > 0) {
    if (mylibs > yourlibs + commonlibs) {
      my_status = ALIVE;
      your_status = DEAD;
      margin_of_safety = mylibs - yourlibs - commonlibs;
    }
    else if (yourlibs > mylibs + commonlibs) {
      my_status = DEAD;
      your_status = ALIVE;
      margin_of_safety = yourlibs - mylibs - commonlibs;
    }
    else if ((commonlibs == 0) 
	     && (mylibs == yourlibs)) {
      my_status = CRITICAL;
      your_status = CRITICAL;
      margin_of_safety = 0;
    }
    else if ((commonlibs > 0) 
	     && (mylibs == commonlibs + yourlibs)) {
      my_status = ALIVE;
      your_status = CRITICAL;
      margin_of_safety = 0;
    }
    else if ((commonlibs > 0) 
	     && (mylibs < commonlibs + yourlibs)
	     && (yourlibs < commonlibs + mylibs)) {
      /* seki */
      my_status = ALIVE;
      your_status = ALIVE;
      margin_of_safety = INFINITY;
    }
    else if ((commonlibs > 0)
	     && yourlibs == commonlibs + mylibs) {
      my_status = CRITICAL;
      your_status = ALIVE;
      margin_of_safety = 0;
    }
  }
  if (DRAGON2(POS(ai, aj)).genus > 0
      && DRAGON2(POS(bi, bj)).genus == 0) {
    if (mylibs > commonlibs + yourlibs) {
      my_status = ALIVE;
      your_status = DEAD;
      margin_of_safety = mylibs - commonlibs - yourlibs;
    }
    else if (mylibs + commonlibs == yourlibs) {
      my_status = CRITICAL;
      your_status = CRITICAL;
    }
    else if (mylibs + commonlibs < yourlibs) {
      my_status = DEAD;
      your_status = ALIVE;
      margin_of_safety = mylibs + commonlibs - yourlibs;
    }
  }
  if (DRAGON2(POS(ai, aj)).genus == 0
      && DRAGON2(POS(bi, bj)).genus > 0) {
    if (yourlibs + commonlibs > mylibs) {
      my_status = DEAD;
      your_status = ALIVE;
      margin_of_safety = yourlibs + commonlibs - mylibs;
    }
    else if (yourlibs + commonlibs == mylibs) {
      my_status = CRITICAL;
      your_status = CRITICAL;
      margin_of_safety = 0;
    }
    else if (yourlibs + commonlibs > mylibs) {
      my_status = DEAD;
      your_status = ALIVE;
      margin_of_safety = yourlibs - mylibs - commonlibs;
    }
  }
  
  /* Update matcher statuses */

  /* We do not want to change the matcher status of the friendly
   * dragon if the owl status is critical. If my_status==DEAD by
   * the preceeding heuristics but the owl code finds a way to
   * live, then we should by all means take it. On the other hand
   * if my_status==ALIVE we are alive by semeai, but as a matter
   * of "safety first" if the owl code finds a way to live we may
   * want to take it. So the matcher status is not changed.
   */
  
  if (dragon[POS(ai, aj)].owl_status != CRITICAL) {
    if (my_status == ALIVE) {
      DEBUG(DEBUG_SEMEAI, 
	    "Changing matcher_status of %m to ALIVE.\n", ai, aj);
      DRAGON2(POS(ai, aj)).safety = ALIVE_IN_SEKI;
      for (di = 0; di < board_size; di++)
	for (dj = 0; dj < board_size; dj++)
	  if (same_dragon(POS(ai, aj), POS(di, dj))) {
	    dragon[POS(di, dj)].matcher_status = ALIVE;
	  }
    }
    else if (my_status == CRITICAL) {
      DEBUG(DEBUG_SEMEAI, 
	    "Changing matcher_status of %m to CRITICAL.\n", ai, aj);
      DRAGON2(POS(ai, aj)).safety = CRITICAL;
      for (di = 0; di < board_size; di++)
	for (dj = 0; dj < board_size; dj++)
	  if (same_dragon(POS(ai, aj), POS(di, dj)))
	    dragon[POS(di, dj)].matcher_status = CRITICAL;
    }
    else if (my_status == DEAD) {
      DEBUG(DEBUG_SEMEAI, 
	    "Changing matcher_status of %m to DEAD.\n", ai, aj);
      DRAGON2(POS(ai, aj)).safety = DEAD;
      for (di = 0; di < board_size; di++)
	for (dj = 0; dj < board_size; dj++)
	  if (same_dragon(POS(ai, aj), POS(di, dj)))
	    dragon[POS(di, dj)].matcher_status = DEAD;
    }
  }

  if (your_status == ALIVE) {
    DEBUG(DEBUG_SEMEAI, 
	  "Changing matcher_status of %m to ALIVE.\n", bi, bj);
    DRAGON2(POS(bi, bj)).safety = ALIVE_IN_SEKI;
    for (di = 0; di < board_size; di++)
      for (dj = 0; dj < board_size; dj++)
	if (same_dragon(POS(bi, bj), POS(di, dj)))
	  dragon[POS(di, dj)].matcher_status = ALIVE;
  }

  else if (your_status == CRITICAL) {
    DEBUG(DEBUG_SEMEAI, 
	  "Changing matcher_status of %m to CRITICAL.\n", bi, bj);
    DRAGON2(POS(bi, bj)).safety = CRITICAL;
    for (di = 0; di < board_size; di++)
      for (dj = 0; dj < board_size; dj++)
	if (same_dragon(POS(bi, bj), POS(di, dj))) {
	  dragon[POS(di, dj)].matcher_status = CRITICAL;
	}
  }
  else if (your_status == DEAD) {
    DEBUG(DEBUG_SEMEAI, 
	  "Changing matcher_status of %m to DEAD.\n", bi, bj);
    DRAGON2(POS(bi, bj)).safety = DEAD;
    for (di = 0; di < board_size; di++)
      for (dj = 0; dj < board_size; dj++)
	if (same_dragon(POS(bi, bj), POS(di, dj)))
	  dragon[POS(di, dj)].matcher_status = DEAD;
  }
  
  /* Find the recommended semeai moves. In order of priority,
   * (1) We defend our dragon;
   * (2) We attack your dragon;
   * (3) If common liberties > 1, make an eye;
   * (4) If common liberties > 1, kill an eye;
   * (5) Fill a liberty of yours;
   * (6) Fill a common liberty.  */

  if ((my_status == CRITICAL) || (your_status == CRITICAL)) {
    int found_one = 0;
    if (dragon[POS(ai, aj)].owl_status == CRITICAL
	&& dragon[POS(ai, aj)].owl_defense_point != NO_MOVE)
      add_appropriate_semeai_moves(I(dragon[POS(ai, aj)].owl_defense_point),
				   J(dragon[POS(ai, aj)].owl_defense_point),
				   ai, aj, bi, bj, my_status, your_status,
				   margin_of_safety);
    else if (dragon[POS(bi, bj)].owl_status == CRITICAL
	     && dragon[POS(bi, bj)].owl_attack_point != NO_MOVE)
      add_appropriate_semeai_moves(I(dragon[POS(bi, bj)].owl_attack_point),
				   J(dragon[POS(bi, bj)].owl_attack_point),
				   ai, aj, bi, bj, my_status, your_status,
				   margin_of_safety);
    else if (commonlibs > 1) {
      if (DRAGON2(POS(ai, aj)).heyes > 0)
	add_appropriate_semeai_moves(I(DRAGON2(POS(ai, aj)).heye),
				     J(DRAGON2(POS(ai, aj)).heye),
				     ai, aj, bi, bj, my_status, your_status,
				     margin_of_safety);
      if (DRAGON2(POS(bi, bj)).heyes > 0)
	add_appropriate_semeai_moves(I(DRAGON2(POS(bi, bj)).heye),
				     J(DRAGON2(POS(bi, bj)).heye),
				     ai, aj, bi, bj, my_status, your_status,
				     margin_of_safety);
    }
    else {
      for (i = 0; i < board_size-1; i++)
	for (j = 0; j < board_size-1; j++)
	  if (liberty_of_dragon(i, j, bi, bj) 
	      && !liberty_of_dragon(i, j, ai, aj)
	      && safe_move2(i, j, color)) {
	    /* add move reasons for EVERY outside liberty where we can
             * play safely. A move to win a semeai might not be a
             * safe move if it is inside the opponent's eyespace. 
             * However we hope that the reading code can analyze the
             * semeai in cases where every safe liberty has been filled.
	     */
	    add_appropriate_semeai_moves(i, j, ai, aj, bi, bj, 
					 my_status, your_status,
					 margin_of_safety);
	    found_one = 1;
	  }
      if (!found_one) {
	/* No outside liberties --- look for common liberties.
	 * Filling a common liberty is usually bad but if our 
	 * heuristics are accurate, we should only reach this point 
	 * if we definitely have enough liberties to win. As a
	 * sanity check, we require filling a common liberty to
	 * be a safe move.
	 */
	for (i = 0; i < board_size-1; i++)
	  for (j = 0; j < board_size-1; j++)
	    if (liberty_of_dragon(i, j, bi, bj)
		&& safe_move2(i, j, color))
	      add_appropriate_semeai_moves(i, j, ai, aj, bi, bj, 
					   my_status, your_status,
					   margin_of_safety);
      }
    }
  }
}

/* Add those move reasons which are appropriate. */

static void
add_appropriate_semeai_moves(int ti, int tj, int ai, int aj, int bi, int bj, 
			     int my_status, int your_status, 
			     int margin_of_safety)
{
  if (my_status == CRITICAL)
    add_semeai_move(POS(ti, tj), POS(ai, aj));
  else if (margin_of_safety==1)
    add_semeai_threat(POS(ti, tj), POS(ai, aj));
  if (your_status == CRITICAL)
      add_semeai_move(POS(ti, tj), POS(bi, bj));
  else if (margin_of_safety==1)
    add_semeai_threat(POS(ti, tj), POS(bi, bj));
}


/* revise_semeai(color) changes the status of any DEAD dragon of
 * OPPOSITE_COLOR(color) which occurs in a semeai to UNKNOWN.
 * It returns true if such a dragon is found.
 */

int
revise_semeai(int color)
{
  int m, n;
  int found_one = 0;
  int other = OTHER_COLOR(color);

  gg_assert(dragon2 != NULL);

  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      if (DRAGON2(POS(m, n)).semeai
	  && dragon[POS(m, n)].matcher_status == DEAD
	  && dragon[POS(m, n)].color == other)
      {
	found_one = 1;
	dragon[POS(m, n)].matcher_status = UNKNOWN;
	if (dragon[POS(m, n)].origin == POS(m, n))
	  TRACE("revise_semeai: changed status of dragon %m from DEAD to UNKNOWN\n",
		m, n);
      }
    }

  return found_one;
}


/* small_semeai() addresses a deficiency in the reading code:
 * for reasons of speed, savestone3 and savestone4 do not
 * sufficiently check whether there may be an adjoining string
 * which can be attacked. So they may overlook a defensive
 * move which consists of attacking an adjoining string.
 *
 * small_semeai(), called by make_worms() searches for a 
 * string A with 3 or 4 liberties such that worm[A], attack_code != 0.
 * If there is a string B next to A (of the opposite color)
 * such that worm[B].attack_code != 0, the following action is
 * taken: if worm[A].liberties == worm[B].liberties, then
 * worm[A].defend is set to worm[B].defend and vice versa;
 * and if worm[A].liberties > worm[B].liberties, then worm[A].defendi
 * is set to -1.
 */

void
small_semeai()
{
  int i, j;
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++)
      if (BOARD(i, j)
	  && (worm[POS(i, j)].liberties == 3 || worm[POS(i, j)].liberties == 4)
	  && worm[POS(i, j)].attack_codes[0] != 0) {
	int other = OTHER_COLOR(BOARD(i, j));
	if (i > 0 && BOARD(i-1, j) == other)
	  small_semeai_analyzer(i, j, i-1, j);
	if (i < board_size-1 && BOARD(i+1, j) == other)
	  small_semeai_analyzer(i, j, i+1, j);
	if (j > 0 && BOARD(i, j-1) == other)
	  small_semeai_analyzer(i, j, i, j-1);
	if (j < board_size-1 && BOARD(i, j+1) == other)
	  small_semeai_analyzer(i, j, i, j+1);
      }
}

/* Helper function for small_semeai. Tries to resolve the
 * semeai between (i,j) and (m,n), possibly revising points
 * of attack and defense.
 */

void
small_semeai_analyzer(int i, int j, int m, int n)
{
  int apos;
  int color = BOARD(i, j);
  int other = BOARD(m, n);

  if (worm[POS(m, n)].attack_codes[0] == 0 || worm[POS(m, n)].liberties < 3)
    return;
  if (worm[POS(i, j)].attack_codes[0] == 0 || worm[POS(i, j)].liberties < 3)
    return;


  /* FIXME: There are many more possibilities to consider */
  if (trymove(worm[POS(i, j)].attack_points[0], other,
	      "small_semeai_analyzer", POS(i, j), EMPTY, 0)) {
    int acode = attack(POS(m, n), &apos);
    if (acode == 0) {
      popgo();
      change_defense(POS(m, n), worm[POS(i, j)].attack_points[0], 1);
    }
    else if (trymove(apos, color, "small_semeai_analyzer", POS(i, j),
		     EMPTY, NO_MOVE)) {
      if (attack(POS(i, j), NULL) == 0) {
	popgo();
	popgo();
	change_attack(POS(i, j), 0, 0);
      }
      else {
	popgo();
	popgo();
      }
    }
    else
      popgo();
  }
  gg_assert(stackp == 0);
  
  if (trymove(worm[POS(m, n)].attack_points[0], color, 
	      "small_semeai_analyzer", POS(m, n), EMPTY, 0)) {
    int acode = attack(POS(i, j), &apos);
    if (acode == 0) {
      popgo();
      change_defense(POS(i, j), worm[POS(m, n)].attack_points[0], 1);
    }
    else if (trymove(apos, other, "small_semeai_analyzer", POS(m, n),
		     EMPTY, NO_MOVE)) {
      if (attack(POS(m, n), NULL) == 0) {
	popgo();
	popgo();
	change_attack(POS(m, n), 0, 0);
      }
      else {
	popgo();
	popgo();
      }
    }
    else
      popgo();
  }
  gg_assert(stackp == 0);
}


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
