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
#include <ctype.h>

#include "liberty.h"
#include "patterns.h"


static void initialize_supplementary_dragon_data(void);
static void find_neighbor_dragons(void);
static void add_adjacent_dragons(int a, int b);
static void add_adjacent_dragon(int a, int b);
static int dragon_invincible(int m, int n);
static int compute_dragon_status(int i, int j);
static void dragon_eye(int m, int n, struct eye_data[BOARDMAX]);
static int compute_escape(int m, int n, int dragon_status_known);

static int dragon2_initialized;
static int lively_white_dragons;
static int lively_black_dragons;

#define occupied(m, n) (m != -1 && BOARD(m, n) != EMPTY)


/* This basic function finds all dragons and collects some basic information
 * about them in the dragon array.
 *
 * color is the player in turn to move. This does in no way affect the
 * information collected about the dragons, but it does affect what
 * information is passed on to the move generation code. If
 * color == EMPTY no information at all is passed on to the move generation.
 */

void 
make_dragons(int color, int stop_before_owl)
{
  int m, n;
  int i, j;
  int d;

  start_timer(2);
  dragon2_initialized = 0;
  
  /* We start with the dragon data copied from the worm data, then
   * modify it as the worms are amalgamated into larger dragons.
   */

  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      dragon[POS(m, n)].id                 = -1;
      dragon[POS(m, n)].size               = worm[POS(m, n)].size;
      dragon[POS(m, n)].effective_size     = worm[POS(m, n)].effective_size;
      dragon[POS(m, n)].color              = worm[POS(m, n)].color;
      dragon[POS(m, n)].origin             = worm[POS(m, n)].origin;
      dragon[POS(m, n)].owl_attack_point   = NO_MOVE;
      dragon[POS(m, n)].owl_attack_certain =  1;
      dragon[POS(m, n)].owl_defense_point  = NO_MOVE;
      dragon[POS(m, n)].owl_defend_certain =  1;
      dragon[POS(m, n)].owl_status         = UNCHECKED;
      dragon[POS(m, n)].status             = UNKNOWN;
      dragon[POS(m, n)].matcher_status     = UNKNOWN;
      dragon[POS(m, n)].owl_threat_status  = UNCHECKED;
      dragon[POS(m, n)].owl_second_attack_point  = NO_MOVE;
      dragon[POS(m, n)].owl_second_defense_point = NO_MOVE;
      half_eye[POS(m, n)].type        =  0;
      
      if (worm[POS(m, n)].origin == POS(m, n))
	DEBUG(DEBUG_DRAGONS, 
	      "Initialising dragon from worm at %m, size %d\n", 
	      m, n, worm[POS(m, n)].size);
    }

  time_report(2, "  time to initialize dragons", -1, -1);
  make_domains(black_eye, white_eye, 0);
  time_report(2, "  time to make domains", -1, -1);

  /* Find explicit connections patterns in database and amalgamate
   * involved dragons.
   */
  find_connections();
  time_report(2, "  time to find connections", -1, -1);
  
  /* Amalgamate dragons sharing an eyespace (not ko). At the same time
   * we decide to which dragon an eyespace belongs. Ko eyespaces
   * (typically false eyes but sometimes halfeyes) get assigned to an
   * arbitrary neighbor that is not the ko stone.
   */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {

      if (black_eye[POS(m, n)].color == BLACK_BORDER
	  && black_eye[POS(m, n)].origin == POS(m, n)) {
	if (!is_ko_point2(m, n)
	   || black_eye[POS(m, n)].esize > 1) /* Only exclude living kos. */
	  dragon_eye(m, n, black_eye);
	else {
	  if (m > 0 && !is_ko_point2(m-1, n))
	    black_eye[POS(m, n)].dragon = dragon[POS(m-1, n)].origin;
	  else if (m < board_size-1 && !is_ko_point2(m+1, n))
	    black_eye[POS(m, n)].dragon = dragon[POS(m+1, n)].origin;
	  else if (n > 0 && !is_ko_point2(m, n-1))
	    black_eye[POS(m, n)].dragon = dragon[POS(m, n-1)].origin;
	  else if (n < board_size-1 && !is_ko_point2(m, n+1))
	    black_eye[POS(m, n)].dragon = dragon[POS(m, n+1)].origin;
	}
      }
	  
      if (white_eye[POS(m, n)].color == WHITE_BORDER
	  && white_eye[POS(m, n)].origin == POS(m, n)) {
	if (!is_ko_point2(m, n)
	    || white_eye[POS(m, n)].esize > 1) /* Only exclude living kos. */
	  dragon_eye(m, n, white_eye);
	else {
	  if (m > 0 && !is_ko_point2(m-1, n))
	    white_eye[POS(m, n)].dragon = dragon[POS(m-1, n)].origin;
	  else if (m < board_size-1 && !is_ko_point2(m+1, n))
	    white_eye[POS(m, n)].dragon = dragon[POS(m+1, n)].origin;
	  else if (n > 0 && !is_ko_point2(m, n-1))
	    white_eye[POS(m, n)].dragon = dragon[POS(m, n-1)].origin;
	  else if (n < board_size-1 && !is_ko_point2(m, n+1))
	    white_eye[POS(m, n)].dragon = dragon[POS(m, n+1)].origin;
	}
      }
    }
  time_report(2, "  time to amalgamate dragons", -1, -1);

  /* At this time, all dragons have been finalized and we can
   * initialize the dragon2[] array. After that we can no longer allow
   * amalgamation of dragons.
   */
  initialize_supplementary_dragon_data();
  time_report(2, "  time to initialize dragon2", -1, -1);
  
  /* Find adjacent worms which can be easily captured: */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      if (worm[POS(m, n)].origin != POS(m, n)
	  || BOARD(m, n) == EMPTY
	  || worm[POS(m, n)].lunch == NO_MOVE)
	continue;

      i = I(worm[POS(m, n)].lunch);
      j = J(worm[POS(m, n)].lunch);

      /* In contrast to worm lunches, a dragon lunch must also be
       * able to defend itself. 
       */
      if (worm[POS(i, j)].defend_codes[0] == 0)
	continue;

      /* Tell the move generation code about the lunch. */
      if (color != EMPTY)
	add_lunch(POS(m, n), POS(i, j));
	
      /* If several lunches are found, we pick the juiciest.
       * First maximize cutstone, then minimize liberties.
       */
      {
	int origini = I(dragon[POS(m, n)].origin);
	int originj = J(dragon[POS(m, n)].origin);
	int lunch = DRAGON2(origini, originj).lunch;

	if (lunch == NO_MOVE
	    || worm[POS(i, j)].cutstone > worm[lunch].cutstone
	    || (worm[POS(i, j)].cutstone == worm[lunch].cutstone
		&& (worm[POS(i, j)].liberties < worm[lunch].liberties))) {
	  DRAGON2(origini, originj).lunch = worm[POS(i, j)].origin;
	  TRACE("at %m setting %m.lunch to %1m (cutstone=%d)\n",
		m, n, origini, originj,
		worm[POS(i, j)].origin, worm[POS(i, j)].cutstone);
	}
      }
    }

  time_report(2, "  time to find lunches", -1, -1);

  /* In case origins of dragons got moved, put the dragons of eyes aright. */
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++) {
      if (black_eye[POS(i, j)].dragon != NO_MOVE) {
	int dr=dragon[black_eye[POS(i, j)].dragon].origin;
	black_eye[POS(i, j)].dragon = dr;
      }

      if (white_eye[POS(i, j)].dragon != NO_MOVE) {
	int dr=dragon[white_eye[POS(i, j)].dragon].origin;
	  white_eye[POS(i, j)].dragon = dr;
      }
    }
  time_report(2, "  time to fix origins", -1, -1);

  /* Find topological half eyes and false eyes by analyzing the
   * diagonal intersections, as described in the Texinfo
   * documentation (Eyes/Eye Topology).
   */

  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      int sum;
      int ai = -1;
      int aj = -1;
      int di = -1;
      int dj = -1;

      if (black_eye[POS(m, n)].color == BLACK_BORDER
	  && (!black_eye[POS(m, n)].marginal || life)
	  && (black_eye[POS(m, n)].neighbors <= 1)
	  && (black_eye[POS(m, n)].dragon != NO_MOVE)) {
	sum = topological_eye(m, n, BLACK, &ai, &aj, &di, &dj,
			      black_eye, white_eye, half_eye);
	if (sum >= 4) {
	  half_eye[POS(m, n)].type = FALSE_EYE;
	  if ((black_eye[POS(m, n)].esize == 1)
	      || is_legal2(m, n, WHITE)
	      || BOARD(m, n) == WHITE)
	    add_half_eye(m, n, black_eye, half_eye);
	}
	else if (sum == 3)
	  half_eye[POS(m, n)].type = HALF_EYE;
      }
      
      if (white_eye[POS(m, n)].color == WHITE_BORDER
	  && (!white_eye[POS(m, n)].marginal || life)
	  && (white_eye[POS(m, n)].neighbors <= 1)
	  && (white_eye[POS(m, n)].dragon != NO_MOVE)) {
	sum = topological_eye(m, n, WHITE, &ai, &aj, &di, &dj,
			      black_eye, white_eye, half_eye);
	if (sum >= 4) {
	  half_eye[POS(m, n)].type = FALSE_EYE;
	  if ((white_eye[POS(m, n)].esize == 1)
	      || is_legal2(m, n, BLACK)
	      || BOARD(m, n) == BLACK)
	    add_half_eye(m, n, white_eye, half_eye);
	}
	else if (sum == 3)
	  half_eye[POS(m, n)].type = HALF_EYE;
      }
    }

  /* Pattern based modification of the eye shapes computed by
   * make_domains and halfeye analysis.
   */
  modify_eye_spaces();
  
  /* Compute the number of eyes, half eyes, etc. in an eye space. */
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++) {
      if ((black_eye[POS(i, j)].color == BLACK_BORDER) 
	  && (black_eye[POS(i, j)].origin == POS(i, j))) 
      {
	int max, min, attacki, attackj, defendi, defendj;

	compute_eyes(i, j, &max, &min, &attacki, &attackj,
		     &defendi, &defendj, black_eye, half_eye, 1, color);
	DEBUG(DEBUG_EYES, "Black eyespace at %m: min=%d, max=%d\n",
	      i, j, min, max);
	black_eye[POS(i, j)].maxeye = max;
	black_eye[POS(i, j)].mineye = min;
	black_eye[POS(i, j)].attack_point  = POS(attacki, attackj);
	black_eye[POS(i, j)].defense_point = POS(defendi, defendj);
	propagate_eye(POS(i, j), black_eye);
      }

      if ((white_eye[POS(i, j)].color == WHITE_BORDER) 
	  && (white_eye[POS(i, j)].origin == POS(i, j))) 
      {
	int max, min, attacki, attackj, defendi, defendj;

	compute_eyes(i, j, &max, &min, &attacki, &attackj,
		     &defendi, &defendj, white_eye, half_eye, 1, color);
	DEBUG(DEBUG_EYES, "White eyespace at %m: min=%d, max=%d\n",
	      i, j, min, max);
	white_eye[POS(i, j)].maxeye = max;
	white_eye[POS(i, j)].mineye = min;
	white_eye[POS(i, j)].attack_point = POS(attacki, attackj);
	white_eye[POS(i, j)].defense_point = POS(defendi, defendj);
	propagate_eye(POS(i, j), white_eye);
      }
    }
  time_report(2, "  time to find eyes", -1, -1);

  /* Now we compute the genus. */
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++) {
      if (black_eye[POS(i, j)].color == BLACK_BORDER
	  && black_eye[POS(i, j)].dragon != NO_MOVE
	  && black_eye[POS(i, j)].origin == POS(i, j))
      {
	m = I(black_eye[POS(i, j)].dragon);
	n = J(black_eye[POS(i, j)].dragon);
	gg_assert (BOARD(m, n) == BLACK);
	TRACE("eye at %m found for dragon at %m--augmenting genus\n",
	      i, j, m, n);
	DRAGON2(m, n).genus += (black_eye[POS(i, j)].mineye);
	DRAGON2(m, n).heyes += (black_eye[POS(i, j)].maxeye
				- black_eye[POS(i, j)].mineye);
	if (black_eye[POS(i, j)].maxeye - black_eye[POS(i, j)].mineye > 0)
	  DRAGON2(m, n).heye = black_eye[POS(i, j)].attack_point;
      }
      if ((white_eye[POS(i, j)].color == WHITE_BORDER) 
	  && (white_eye[POS(i, j)].dragon != NO_MOVE)
	  && (white_eye[POS(i, j)].origin == POS(i, j))) 
      {
	m = I(white_eye[POS(i, j)].dragon);
	n = J(white_eye[POS(i, j)].dragon);
	gg_assert (BOARD(m, n) == WHITE);
	TRACE("eye at %m found for dragon at %m--augmenting genus\n",
	      i, j, m, n);
	DRAGON2(m, n).genus += (white_eye[POS(i, j)].mineye);
	DRAGON2(m, n).heyes += (white_eye[POS(i, j)].maxeye
				- white_eye[POS(i, j)].mineye);
	if (white_eye[POS(i, j)].maxeye - white_eye[POS(i, j)].mineye > 0) {
	  DRAGON2(m, n).heye = white_eye[POS(i, j)].attack_point;
	}
      }
    }

  time_report(2, "  time to compute genus", -1, -1);

  /* Compute the escape route measure. */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) 
      if (dragon[POS(m, n)].origin == POS(m, n)
	  && BOARD(m, n) != EMPTY) {
	DRAGON2(m, n).escape_route = compute_escape(m, n, 0);
      }

  time_report(2, "  time to compute escape", -1, -1);

  /* Update the segmentation of the initial influence before we
   * compute the surrounding moyo sizes. The reason for this is that
   * now the eyespace inhibition found by find_cuts() can be taken
   * into account.
   */
  resegment_initial_influence();
  time_report(2, "  resegment_initial_influence", -1, -1);

  /* Compute the surrounding moyo sizes. */
  for (d = 0; d < number_of_dragons; d++) {
    dragon2[d].moyo = influence_get_moyo_size(I(dragon2[d].origin),
					      J(dragon2[d].origin),
					      DRAGON(d).color, 1);
  }
  time_report(2, "  influence_get_moyo_size", -1, -1);

  /* Determine status: ALIVE, DEAD, CRITICAL or UNKNOWN */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      if (dragon[POS(m, n)].origin == POS(m, n) && BOARD(m, n)) {
	dragon[POS(m, n)].status = compute_dragon_status(m, n);
	sgffile_dragon_status(m, n, dragon[POS(m, n)].status);
      }
    }
  time_report(2, "  compute_dragon_status", -1, -1);

  /* We must update the dragon status at every intersection before we
   * call the owl code. This updates all fields.
   */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      struct dragon_data *d = &(dragon[POS(m, n)]);
      dragon[POS(m, n)] = dragon[d->origin];
    }
  
  find_neighbor_dragons();
  time_report(2, "  find_neighbor_dragons", -1, -1);

  if (stop_before_owl)
    return;
  
  /* Determine owl status of each dragon. */

  purge_persistent_owl_cache();

  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      int attacki = -1;
      int attackj = -1;
      int second_attacki = -1;
      int second_attackj = -1;
      int defendi = -1;
      int defendj = -1;
      int second_defendi = -1;
      int second_defendj = -1;
      
      if (BOARD(m, n) == EMPTY
	  || dragon[POS(m, n)].origin != POS(m, n))
	continue;

      /* Some dragons can be ignored. But
       * Be extra careful with big dragons. */
      if (DRAGON2(m, n).escape_route > 25
	  || DRAGON2(m, n).moyo > 20
	  || (DRAGON2(m, n).moyo > 10
	      && DRAGON2(m, n).moyo > dragon[POS(m, n)].size)) {
	dragon[POS(m, n)].owl_status = UNCHECKED;
	dragon[POS(m, n)].owl_threat_status = UNCHECKED;
	dragon[POS(m, n)].owl_attack_point  = NO_MOVE;
	dragon[POS(m, n)].owl_defense_point = NO_MOVE;
	dragon[POS(m, n)].owl_second_attack_point  = NO_MOVE;
	dragon[POS(m, n)].owl_second_defense_point = NO_MOVE;
      }
      else {
	start_timer(3);
	if (owl_attack(m, n, &attacki, &attackj, 
		       &dragon[POS(m, n)].owl_attack_certain)) {
	  dragon[POS(m, n)].owl_attack_point = POS(attacki, attackj);
	  if (attacki != -1
	      && owl_defend(m, n, &defendi, &defendj, 
			    &dragon[POS(m, n)].owl_defend_certain)) {
	    if (defendi != -1) {
	      dragon[POS(m, n)].owl_status = CRITICAL;
	      dragon[POS(m, n)].owl_defense_point = POS(defendi, defendj);
	    }
	    else {
	      /* Due to irregularities in the owl code, it may
	       * occasionally happen that a dragon is found to be
	       * attackable but also alive as it stands. In this case
	       * we still choose to say that the owl_status is
	       * CRITICAL, although we don't have any defense move to
	       * propose. Having the status right is important e.g.
	       * for connection moves to be properly valued.
	       */
	      dragon[POS(m, n)].owl_status = CRITICAL;
	      DEBUG(DEBUG_OWL_PERFORMANCE,
		    "Inconsistent owl attack and defense results for %m.\n",
		    m, n);
	    }
	  }
	  else {
	    dragon[POS(m, n)].owl_status = DEAD; 
	    dragon[POS(m, n)].owl_defense_point = NO_MOVE;
	    if (level >= 8
		&& !disable_threat_computation) {
	      if (owl_threaten_defense(m, n, &defendi, &defendj,
				       &second_defendi, &second_defendj)) {
		dragon[POS(m, n)].owl_threat_status = CAN_THREATEN_DEFENSE;
		dragon[POS(m, n)].owl_defense_point = POS(defendi, defendj);
		dragon[POS(m, n)].owl_second_defense_point = POS(second_defendi, 
							    second_defendj);
	      }
	      else
		dragon[POS(m, n)].owl_threat_status = DEAD;;
	    }
	  }
	}
	else {
	  if (!dragon[POS(m, n)].owl_attack_certain
	      && owl_defend(m, n, &defendi, &defendj, 
			    &dragon[POS(m, n)].owl_defend_certain)) {
	    /* If the result of owl_attack was not certain, we may
	     * still want the result of owl_defend */
	    dragon[POS(m, n)].owl_defense_point = POS(defendi, defendj);
	  }
	  dragon[POS(m, n)].owl_status = ALIVE;
	  dragon[POS(m, n)].owl_attack_point = NO_MOVE;
	  if (level >= 8
	      && !disable_threat_computation) {
	    if (owl_threaten_attack(m, n, &attacki, &attackj,
				    &second_attacki, &second_attackj)) {
	      dragon[POS(m, n)].owl_threat_status = CAN_THREATEN_ATTACK;
	      dragon[POS(m, n)].owl_attack_point = POS(attacki, attackj);
	      dragon[POS(m, n)].owl_second_attack_point = POS(second_attacki,
							 second_attackj);
	    }
	    else
	      dragon[POS(m, n)].owl_threat_status = ALIVE;
	  }
	}
	time_report(3, "    owl reading for dragon at ", m, n);
      }
    }

  time_report(2, "  owl reading", -1, -1);

  /* The dragon data is now correct at the origin of each dragon but
   * we need to copy it to every vertex.  
   */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      struct dragon_data *d = &(dragon[POS(m, n)]);
      dragon[POS(m, n)] = dragon[d->origin];
    }

  /* Compute the status to be used by the matcher. We most trust the
   * owl status, if it is available.
   */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++)
      if (BOARD(m, n) != EMPTY) {
	if (dragon[POS(m, n)].owl_status != UNCHECKED)
	  dragon[POS(m, n)].matcher_status = dragon[POS(m, n)].owl_status;
	else if (dragon[POS(m, n)].status == DEAD 
		 || dragon[POS(m, n)].status == CRITICAL) {
	  /* If a dragon has sufficient escape potential or
	   * surrounding moyo to stop the owl code from being run, the
	   * matcher_status should be no worse than UNKNOWN,
	   * regardless what the static life and death analysis
	   * guesses.
	   */
	  dragon[POS(m, n)].matcher_status = UNKNOWN;
	}
	else
	  dragon[POS(m, n)].matcher_status = dragon[POS(m, n)].status;
      }

  time_report(2, "  compute matcher status", -1, -1);

  /* Compute the safety value. */
  for (d = 0; d < number_of_dragons; d++) {
    int true_genus;
    m = I(dragon2[d].origin);
    n = J(dragon2[d].origin);
    true_genus = 2 * dragon2[d].genus + dragon2[d].heyes;
    /* FIXME: Probably need a better definition of INESSENTIAL dragons.
     *        There are cases where a string is owl insubstantial
     *        yet allowing it to be captured greatly weakens our
     *        position.
     */
    if (dragon[POS(m, n)].size == worm[POS(m, n)].size
	&& !owl_substantial(m, n))
      dragon2[d].safety = INESSENTIAL;
    else if (dragon[POS(m, n)].size == worm[POS(m, n)].size
	     && worm[POS(m, n)].attack_codes[0] != 0
	     && worm[POS(m, n)].defend_codes[0] == 0)
      dragon2[d].safety = TACTICALLY_DEAD;
    else if (0) /* Seki is detected by the call to semeai() below. */
      dragon2[d].safety = ALIVE_IN_SEKI;
    else if (dragon[POS(m, n)].owl_status == DEAD)
      dragon2[d].safety = DEAD;
    else if (dragon[POS(m, n)].owl_status == CRITICAL)
      dragon2[d].safety = CRITICAL;
    else if (dragon[POS(m, n)].owl_status == UNCHECKED
	     && true_genus < 4
	     && dragon2[d].moyo <= 10)
      dragon2[d].safety = WEAK;
    else if (dragon_invincible(m, n))
      dragon2[d].safety = INVINCIBLE;
    else if (true_genus >= 6 || dragon2[d].moyo > 20)
      dragon2[d].safety = STRONGLY_ALIVE;
    else if ((2 * true_genus + dragon2[d].moyo < 8
	      && dragon2[d].escape_route < 10)
	     || (dragon[POS(m, n)].owl_threat_status == CAN_THREATEN_ATTACK)) {
      if (DRAGON(d).owl_attack_certain)
	  dragon2[d].safety = WEAKLY_ALIVE;
      else
	  dragon2[d].safety = WEAK;
    }
    else
      dragon2[d].safety = ALIVE;
  }

  time_report(2, "  compute dragon safety", -1, -1);

  /* Resolve semeais. This may revise the safety and status fields. */
  semeai(color);
  time_report(2, "  semeai module", -1, -1);

  /* The matcher_status is now correct at the origin of each dragon
   * but we need to copy it to every vertex.
   */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      dragon[POS(m, n)].matcher_status =
	dragon[dragon[POS(m, n)].origin].matcher_status;
    }

  /* Revise essentiality of critical worms. Specifically, a critical
   * worm which is adjacent to no enemy dragon with matcher_status
   * better than DEAD, is considered INESSENTIAL.
   */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      int pos = POS(m, n);
      if (is_worm_origin(pos, pos)
	  && worm[pos].attack_codes[0] != 0
	  && worm[pos].defend_codes[0] != 0
	  && !worm[pos].inessential) {
	int adjs[MAXCHAIN];
	int neighbors;
	int r;
	int essential = 0;
	
	neighbors = chainlinks(pos, adjs);
	for (r = 0; r < neighbors; r++)
	  if (dragon[adjs[r]].matcher_status != DEAD) {
	    essential = 1;
	    break;
	  }

	if (!essential) {
	  DEBUG(DEBUG_WORMS, "Worm %1m revised to be inessential.\n", pos);
	  worm[pos].inessential = 1;
	  propagate_worm(pos);
	}
      }
    }
  
  time_report(2, "  revise inessentiality", -1, -1);

  /* Count the non-dead dragons. */
  lively_white_dragons = 0;
  lively_black_dragons = 0;
  for (d = 0; d < number_of_dragons; d++)
    if (DRAGON(d).status != DEAD) {
      if (DRAGON(d).color == WHITE)
	lively_white_dragons++;
      else
	lively_black_dragons++;
    }
}


/* Initialize the dragon2[] array. */
static void
initialize_supplementary_dragon_data()
{
  int m, n;
  int i, j;
  int d;
  
  /* Give each dragon (caves excluded) an id number for indexing into
   * the dragon2 array. After this the DRAGON2 macro can be used.
   */
  number_of_dragons = 0;
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      if (BOARD(m, n) == EMPTY)
	continue;
      i = I(dragon[POS(m, n)].origin);
      j = J(dragon[POS(m, n)].origin);
      if (dragon[POS(i, j)].id == -1)
	dragon[POS(i, j)].id = number_of_dragons++;
      dragon[POS(m, n)].id = dragon[POS(i, j)].id;
    }

  /* Now number_of_dragons contains the number of dragons and we can
   * allocate a dragon2 array of the appropriate size. First throw
   * away the old array.
   *
   * FIXME: As a future optimization we should only allocate a new
   *       array if the old one is too small.
   */
  if (dragon2 != NULL)
    free(dragon2);

  dragon2 = malloc(number_of_dragons * sizeof(*dragon2));
  gg_assert(dragon2 != NULL);

  /* Find the origins of the dragons to establish the mapping back to
   * the board. After this the DRAGON macro can be used.
   */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      if (BOARD(m, n) != EMPTY
	  && dragon[POS(m, n)].origin == POS(m, n)) {
	DRAGON2(m, n).origin = POS(m, n);
      }
    }

  /* Initialize the rest of the dragon2 data. */
  for (d = 0; d < number_of_dragons; d++) {
    dragon2[d].neighbors    = 0;
    dragon2[d].moyo         = -1;
    dragon2[d].safety       = -1;
    dragon2[d].escape_route = 0;
    dragon2[d].genus        = 0;
    dragon2[d].heyes        = 0;
    dragon2[d].heye         = NO_MOVE;
    dragon2[d].lunch        = NO_MOVE;
    dragon2[d].semeai       = 0;
    dragon2[d].semeai_margin_of_safety = -1;
  }

  dragon2_initialized = 1;
}


/* Examine which dragons are adjacent to each other. This is
 * complicated by the fact that adjacency may involve a certain
 * amount of empty space.
 *
 * The approach we use is to extend the dragons into their
 * surrounding influence areas until they collide. We also accept
 * one step extensions into neutral regions. After having done this
 * we can look for immediate adjacencies.
 */
static void
find_neighbor_dragons()
{
  int m, n;
  int i, j;
  int d;
  int dragons[MAX_BOARD][MAX_BOARD];
  int distances[MAX_BOARD][MAX_BOARD];
  int dist;
  int k;
  int color;

  gg_assert(dragon2_initialized);
  
  /* Initialize the arrays. */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      if (BOARD(m, n) != EMPTY) {
	dragons[m][n] = dragon[POS(m, n)].id;
	distances[m][n] = 0;
      }
      else {
	dragons[m][n] = -1;
	distances[m][n] = -1;
      }
    }

  /* Expand from dist-1 to dist. Break out of the loop at the end if
     * we couldn't expand anything. Never expand more than five steps.
     */
  for (dist = 1; dist <= 5; dist++) {
    int found_one = 0;
      
    for (m = 0; m < board_size; m++)
      for (n = 0; n < board_size; n++) {
	if (distances[m][n] != dist-1 || dragons[m][n] < 0)
	  continue;
	color = DRAGON(dragons[m][n]).color;
	for (k = 0; k < 4; k++) {
	  i = m + deltai[k];
	  j = n + deltaj[k];
	  if (!ON_BOARD2(i, j))
	    continue;
	  /* Consider expansion from (m, n) to adjacent intersection
	   * (i, j).
	   */
	  if (distances[i][j] >= 0 && distances[i][j] < dist)
	    continue; /* (i, j) already occupied. */
	  if (influence_area_color(m, n) == color
	      && influence_area_color(i, j) != OTHER_COLOR(color)) {
	    /* Expansion ok. Now see if someone else has tried to
	     * expand here. In that case we indicate a collision by
	     * setting the dragon number to -2.
	     */
	    if (distances[i][j] == dist) {
	      if (dragons[i][j] != dragons[m][n])
		dragons[i][j] = -2;
	    }
	    else {
	      dragons[i][j] = dragons[m][n];
	      distances[i][j] = dist;
	      found_one = 1;
	    }
	  }
	}
      }
    if (!found_one)
      break;
  }

  if (0) {
    for (m = 0; m < board_size; m++) {
      for (n = 0; n < board_size; n++) {
	fprintf(stderr, "%3d", dragons[m][n]);
      }
      fprintf(stderr, "\n");
    }
    fprintf(stderr, "\n");
      
    for (m = 0; m < board_size; m++) {
      for (n = 0; n < board_size; n++) {
	fprintf(stderr, "%3d", distances[m][n]);
      }
      fprintf(stderr, "\n");
    }
    fprintf(stderr, "\n");
  }

  /* Now go through dragons to find neighbors. It suffices to look
     * south and east for neighbors. In the case of a collision zone
     * where dragons==-2 we set all the neighbors of this intersection
     * as adjacent to each other.
     */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      if (dragons[m][n] == -2) {
	int neighbors = 0;
	int adjacent[4];
	for (k = 0; k < 4; k++) {
	  i = m + deltai[k];
	  j = n + deltaj[k];
	  if (ON_BOARD2(i, j) && dragons[i][j] >= 0)
	    adjacent[neighbors++] = dragons[i][j];
	}
	for (i = 0; i < neighbors; i++)
	  for (j = i+1; j < neighbors; j++)
	    add_adjacent_dragons(adjacent[i], adjacent[j]);
      }
      else if (dragons[m][n] >= 0) {
	if (m < board_size-1) {
	  if (dragons[m+1][n] >= 0
	      && dragons[m+1][n] != dragons[m][n])
	    add_adjacent_dragons(dragons[m][n], dragons[m+1][n]);
	}
	if (n < board_size-1) {
	  if (dragons[m][n+1] >= 0
	      && dragons[m][n+1] != dragons[m][n])
	    add_adjacent_dragons(dragons[m][n], dragons[m][n+1]);
	}
      }
    }

  if (0) {
    for (d = 0; d < number_of_dragons; d++) {
      gprintf("dragon %d at %1m:", d, dragon2[d].origin);
      for (i = 0; i < dragon2[d].neighbors; i++)
	gprintf(" %1m(%d)", dragon2[dragon2[d].adjacent[i]].origin,
		dragon2[d].adjacent[i]);
      gprintf("\n");
    }
  }
}

/* Add the dragons with id a and b as adjacent to each other. */
static void
add_adjacent_dragons(int a, int b)
{
  gg_assert(a >= 0 && a < number_of_dragons && b >= 0 && b < number_of_dragons);
  if (a == b)
    return;
  add_adjacent_dragon(a, b);
  add_adjacent_dragon(b, a);
}

/* Add the dragon with id b as adjacent to a. */
static void
add_adjacent_dragon(int a, int b)
{
  int i;
  gg_assert(a >= 0 && a < number_of_dragons && b >= 0 && b < number_of_dragons);
  /* If the array of adjacent dragons already is full, ignore
   * additional neighbors.
   */
  if (dragon2[a].neighbors == MAX_NEIGHBOR_DRAGONS)
    return;
  
  for (i = 0; i < dragon2[a].neighbors; i++)
    if (dragon2[a].adjacent[i] == b)
      return;

  dragon2[a].adjacent[dragon2[a].neighbors++] = b;
}

/* A dragon is considered invincible if it satisfies either of the two
 * following conditions:
 * a) At least two distinct eyespaces without topological halfeyes or
 * marginal vertices.
 * b) At least one string which is unconditionally alive according to the
 * unconditional_life() function in utils.c.
 */

static int
dragon_invincible(int m, int n)
{

  struct eye_data *eye;

  int i, j;

  int strong_eyes = 0;

  gg_assert(BOARD(m, n) != EMPTY);

  /* First look for invincible strings in the dragon. */
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++)
      if (same_dragon(i, j, m, n) && worm[POS(i, j)].invincible)
        return 1;

  /* Examine the eye spaces.
   * FIXME: The check for half eyes or false eyes may be too weak.
   */
  if (BOARD(m, n) == BLACK)
    eye = black_eye;
  else
    eye = white_eye;

  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++)
      if (eye[POS(i, j)].origin == POS(i, j)
          && same_dragon(I(eye[POS(i, j)].dragon), J(eye[POS(i, j)].dragon), m, n)) {
        if (eye[POS(i, j)].msize == 0 && eye[POS(i, j)].mineye > 0)
          strong_eyes++;
      }

  if (strong_eyes >= 2)
    return 1;

  return 0;
}



/* print status info on all dragons. (Can be invoked from gdb) 
 */
void 
show_dragons(void)
{
  static const char *snames[] = 
    {"dead", "alive", "critical", "unknown", "unchecked"};

  static const char *safety_names[] =
  {"dead", "alive", "critical", "inessential", "tactically dead", "weak",
   "weakly_alive", "alive_in_seki", "strongly_alive", "invincible"};
  
  int m, n;
  int k;

  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      struct worm_data *w = &(worm[POS(m, n)]);

      if (w->origin == POS(m, n)) {
	if (BOARD(m, n)) {
	  gprintf("%m : (dragon %1m) %s string of size %d (%f), genus %d: (%d,%d,%d,%d)",
		  m, n, dragon[POS(m, n)].origin,
		  color_to_string(BOARD(m, n)),
		  w->size,
		  w->effective_size,
		  w->genus,
		  w->liberties,
		  w->liberties2,
		  w->liberties3,
		  w->liberties4);
	  if (w->cutstone == 1)
	    gprintf ("%o - is a potential cutting stone\n");
	  else if (w->cutstone == 2)
	    gprintf("%o - is a cutting stone\n");
	  else
	    gprintf("%o\n");

	  if (w->cutstone2 > 0)
	    gprintf("- cutstone2 = %d\n", w->cutstone2);

	  /* FIXME: List all attack and defense points. Also list all
           * threats.
	   */
	  for (k = 0; k < MAX_TACTICAL_POINTS; k++) {
	    if (w->attack_codes[k] == 0)
	      break;
	    gprintf("- attackable at %1m, attack code = %d\n",
		    w->attack_points[k], w->attack_codes[k]);
	  }
	  
	  for (k = 0; k < MAX_TACTICAL_POINTS; k++) {
	    if (w->defend_codes[k] == 0)
	      break;
	    if (w->defend_codes[k] != 0)
	      gprintf("- defendable at %1m, defend code = %d\n",
		      w->defense_points[k], w->defend_codes[k]);
	  }

	  for (k = 0; k < MAX_TACTICAL_POINTS; k++) {
	    if (w->attack_threat_codes[k] == 0)
	      break;
	    gprintf("- attack threat at %1m, attack threat code = %d\n",
		    w->attack_threat_points[k], w->attack_threat_codes[k]);
	  }
	  
	  for (k = 0; k < MAX_TACTICAL_POINTS; k++) {
	    if (w->defense_threat_codes[k] == 0)
	      break;
	    if (w->defense_threat_codes[k] != 0)
	      gprintf("- defense threat at %1m, defense threat code = %d\n",
		      w->defense_threat_points[k], w->defense_threat_codes[k]);
	  }

	  if (w->lunch != NO_MOVE)
	    gprintf("... adjacent worm %1m is lunch\n", w->lunch);
	  
	  if (w->inessential)
	    gprintf("- is inessential\n");
	  
	  if (w->invincible)
	    gprintf("- is invincible\n");
	  
	  if (is_ko_point2(m, n))
	    gprintf("- is a ko stone\n");
	}
      }
    }

  gprintf("%o\n");
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      struct dragon_data *d = &(dragon[POS(m, n)]);
      struct dragon_data2 *d2 = &(dragon2[d->id]);
      int k;

      if (d->origin == POS(m, n)) {
	if (BOARD(m, n)) {
	  gprintf("%m : %s dragon size %d (%f), genus %d, half eyes %d, escape factor %d, status %s, matcher status %s, moyo size %d safety %s",
		  m, n,
		  BOARD(m, n)==BLACK ? "B" : "W",
		  d->size,
		  d->effective_size,
		  d2->genus,
		  d2->heyes,
		  d2->escape_route,
		  snames[d->status],
		  snames[d->matcher_status],
		  d2->moyo,
		  safety_names[d2->safety]);
	  gprintf(", owl status %s\n", snames[d->owl_status]);
	  if (d->owl_status == CRITICAL) {
	    gprintf("... owl attackable at %1m\n",
		    d->owl_attack_point);
	    gprintf("... owl defendable at %1m\n",
		    d->owl_defense_point);
	  }
	  gprintf("... neighbors");
	  for (k = 0; k < d2->neighbors; k++) {
	    int d = d2->adjacent[k];
	    gprintf(" %1m", dragon2[d].origin);
	  }
	  gprintf("\n");
	  if (d2->lunch != NO_MOVE)
	    gprintf("... adjacent worm %1m is lunch\n", d2->lunch);
	}
      }
    }
}


  
/* 
 * dragon_eye(m, n, *di, *dj) is invoked with (m, n) the origin of an
 * eyespace. It unites all the worms adjacent to non-marginal points of
 * the eyespace into a single dragon with origin (*di, *dj). In addition
 * to marginal eye space points, amalgamation is inhibited for points
 * with the INHIBIT_CONNECTION type set.
 *
 * This is based on the older function dragon_ring.
 */

static void
dragon_eye(int m, int n, struct eye_data eye[BOARDMAX])
{
  int i, j;
  int dragoni = -1, dragonj = -1;
  int color;
  int k;

  /* don't amalgamate across ikken tobi */
  if (eye[POS(m, n)].esize == 3 && eye[POS(m, n)].msize > 1)
    return;

  DEBUG(DEBUG_DRAGONS, "amalgamate dragons around %m\n", m, n);
  if (eye[POS(m, n)].color == BLACK_BORDER)
    color = BLACK;
  else {
    gg_assert(eye[POS(m, n)].color == WHITE_BORDER);
    color = WHITE;
  }

  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++) {
      if (eye[POS(i, j)].origin == POS(m, n)
	  && !eye[POS(i, j)].marginal
	  && !(eye[POS(i, j)].type & INHIBIT_CONNECTION)) {
	for (k = 0; k < 4; k++) {
	  int di = deltai[k];
	  int dj = deltaj[k];
	  if (BOARD(i+di, j+dj) == color) {
	    if (dragoni == -1) {
	      dragoni = I(dragon[POS(i+di, j+dj)].origin);
	      dragonj = J(dragon[POS(i+di, j+dj)].origin);
	    }
	    else if (dragon[POS(i+di, j+dj)].origin != POS(dragoni, dragonj)) {
	      join_dragons(i+di, j+dj, dragoni, dragonj);
	      dragoni = I(dragon[POS(i+di, j+dj)].origin);
	      dragonj = J(dragon[POS(i+di, j+dj)].origin);
	    }
	  }
	}
      }
    }
  
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++) {
      if ((eye[POS(i, j)].color == BLACK_BORDER 
	   || eye[POS(i, j)].color == WHITE_BORDER) 
	  && eye[POS(i, j)].origin == POS(m, n))
      {
	eye[POS(i, j)].dragon = POS(dragoni, dragonj);
      }
    }
}


/* 
 * join_dragons amalgamates the dragon at (ai, aj) to the
 * dragon at (bi, bj).
 */

void 
join_dragons(int ai, int aj, int bi, int bj)
{
  int t, u;

  int i = I(dragon[POS(ai, aj)].origin);
  int j = J(dragon[POS(ai, aj)].origin);
  int m = I(dragon[POS(bi, bj)].origin);
  int n = J(dragon[POS(bi, bj)].origin);
  int oi; /* new origini */
  int oj; /* new originj */
  
  gg_assert(BOARD(i, j) == BOARD(m, n));
  gg_assert(dragon2_initialized == 0);
  gg_assert(BOARD(i, j) != EMPTY);

  if (BOARD(i, j) == EMPTY) {
    /* Joining caves. */
    oi = m;
    oj = n;
  }
  else {
    /* We want to have the origin pointing to the largest string of
     * the dragon.  If this is not unique, we take the "upper
     * leftmost" one.
     */
    if (worm[POS(i, j)].size > worm[POS(m, n)].size
	|| (worm[POS(i, j)].size == worm[POS(m, n)].size
	    && (i < m || (i == m && j < n)))) {
      oi = i;
      oj = j;
      DEBUG(DEBUG_DRAGONS, "joining dragon at %m to dragon at %m\n",
	    m, n, i, j);
    }
    else {
      oi = m;
      oj = n;
      DEBUG(DEBUG_DRAGONS, "joining dragon at %m to dragon at %m\n",
	    i, j, m, n);
    }
  }
  
  dragon[POS(oi, oj)].size  = dragon[POS(m, n)].size + dragon[POS(i, j)].size;
  dragon[POS(oi, oj)].effective_size  = (dragon[POS(m, n)].effective_size
				    + dragon[POS(i, j)].effective_size);

  for (t = 0; t < board_size; t++)
    for (u = 0; u < board_size; u++) {
      if (dragon[POS(t, u)].origin == POS(i, j)
	  || dragon[POS(t, u)].origin == POS(m, n)) {
	dragon[POS(t, u)].origin = POS(oi, oj);
      }
    }
}



/*
 * compute_dragon_status(i, j) tries to determine whether the dragon
 * at (i, j) is ALIVE, DEAD, or UNKNOWN. The algorithm is not perfect
 * and can give incorrect answers.
 *
 * The dragon is judged alive if its genus is >1. It is judged dead if
 * the genus is <2, it has no escape route, and no adjoining string can
 * be easily captured. Otherwise it is judged UNKNOWN.  */

static int 
compute_dragon_status(int i, int j)
{
  int true_genus = 2*DRAGON2(i, j).genus + DRAGON2(i, j).heyes;
  int lunch = DRAGON2(i, j).lunch;

  gg_assert(dragon2_initialized);
  
  /* If it has two sure eyes, everything is just dandy. */
  if (true_genus > 3)
    return ALIVE;

  /* If the dragon consists of one worm, there is an attack, but 
   * no defense and there is less than one eye and one half eye,
   * the situation is hopeless.
   */
  if (dragon[POS(i, j)].size == worm[POS(i, j)].size
      && worm[POS(i, j)].attack_codes[0] != 0 
      && worm[POS(i, j)].defend_codes[0] == 0
      && true_genus < 3)
    return DEAD;
  
  if (lunch != NO_MOVE
      && true_genus < 3
      && worm[lunch].defend_codes[0] != 0
      && DRAGON2(i, j).escape_route < 5)
    if (true_genus == 2 || worm[lunch].size > 2)
      return CRITICAL;

  if (lunch != NO_MOVE
      && true_genus >= 3)
    return ALIVE;

  if (lunch == NO_MOVE || worm[lunch].cutstone < 2) 
  {
    if (true_genus < 3
	&& DRAGON2(i, j).escape_route == 0
	&& DRAGON2(i, j).moyo < 5)
      return DEAD;

    if (true_genus == 3
	&& DRAGON2(i, j).escape_route < 5)
      return CRITICAL;
  }

  return UNKNOWN;
}


/* The dragon escape measure. This is defined as follows.
 *   
 * Let a PATH be a sequence of adjacent intersections that do nowhere
 * touch or include an opponent stone or touch the border. It may
 * include friendly stones and those are allowed to touch opponent
 * stones or the border). Let a DISTANCE N INTERSECTION be an
 * intersection connected to a dragon by a path of length N, but by no
 * shorter path. The connection of the path to the dragon may either
 * be by direct adjacency or diagonally if both adjoining
 * intersections are empty.
 *
 * It is assumed that each intersection has an escape value, which
 * would typically depend on influence and (preliminary) dragon
 * status. We define the escape potential as the sum of the escape
 * values over the distance four intersections of the dragon.
 * 
 * Example of distance N intersections, 1 <= N <= 4:
 * 
 * . . . . . . . . .    . . . . . . . . .
 * . . . . . X . . O    . . . . . X . . O
 * . . X . . . . . O    . . X . 2 . 4 . O
 * X . . . . . . . .    X . . 1 1 2 3 4 .
 * X O . O . . . . O    X O 1 O 1 2 3 4 O
 * X O . O . . . . .    X O 1 O 1 . 4 . .
 * X O . . . X . O O    X O 1 . . X . . O
 * . . . X . . . . .    . 1 . X . . . . .
 * X . . . . X . . .    X . . . . X . . .
 * . . . . . . . . .    . . . . . . . . .
 *
 * Additionally, a path may not pass a connection inhibited
 * intersection.
 */

#define ENQUEUE(i, j) (queuei[queue_end] = (i),\
		       queuej[queue_end++] = (j),\
		       mx[i][j] = 1)

/* Compute the escape potential described above. The dragon is marked
 * in the goal array.
 */
int
dragon_escape(char goal[BOARDMAX], int color,
	      int escape_value[BOARDMAX])
{
  int i, j;
  int k;
  static int mx[MAX_BOARD][MAX_BOARD];
  static int mx_initialized = 0;
  int queuei[MAX_BOARD * MAX_BOARD];
  int queuej[MAX_BOARD * MAX_BOARD];
  int queue_start = 0;
  int queue_end = 0;
  int other = OTHER_COLOR(color);
  int distance;
  int escape_potential = 0;

  gg_assert(color != EMPTY);
  
  if (!mx_initialized) {
    memset(mx, 0, sizeof(mx));
    mx_initialized = 1;
  }

  /* Enter the stones of the dragon in the queue. */
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++)
      if (goal[POS(i, j)])
	ENQUEUE(i, j);
  
  /* Find points at increasing distances from the dragon. At distance
   * four, sum the escape values at those points to get the escape
   * potential.
   */
  for (distance = 0; distance <= 4; distance++) {
    int save_queue_end = queue_end;
    while (queue_start < save_queue_end) {
      i = queuei[queue_start];
      j = queuej[queue_start];
      queue_start++;

      /* Do not pass connection inhibited intersections. */
      if ((color == WHITE
	   && ((white_eye[POS(i, j)].type & INHIBIT_CONNECTION)
	       || white_eye[POS(i, j)].cut == 1))
	  || (color == BLACK
	      && ((black_eye[POS(i, j)].type & INHIBIT_CONNECTION)
		  || black_eye[POS(i, j)].cut == 1)))
	continue;
      
      if (distance == 4)
	escape_potential += escape_value[POS(i, j)];
      else {
	if (i > 0
	    && !mx[i-1][j]
	    && (BOARD(i-1, j) == color
		|| (BOARD(i-1, j) == EMPTY
		    && i > 1 && BOARD(i-2, j) != other
		    && j > 0 && BOARD(i-1, j-1) != other
		    && j < board_size-1 && BOARD(i-1, j+1) != other)))
	  ENQUEUE(i-1, j);

      	if (i < board_size-1
	    && !mx[i+1][j]
	    && (BOARD(i+1, j) == color
		|| (BOARD(i+1, j) == EMPTY
		    && i < board_size-2 && BOARD(i+2, j) != other
		    && j > 0 && BOARD(i+1, j-1) != other
		    && j < board_size-1 && BOARD(i+1, j+1) != other)))
	  ENQUEUE(i+1, j);

	if (j > 0
	    && !mx[i][j-1]
	    && (BOARD(i, j-1) == color
		|| (BOARD(i, j-1) == EMPTY
		    && j > 1 && BOARD(i, j-2) != other
		    && i > 0 && BOARD(i-1, j-1) != other
		    && i < board_size-1 && BOARD(i+1, j-1) != other)))
	  ENQUEUE(i, j-1);

	if (j < board_size-1
	    && !mx[i][j+1]
	    && (BOARD(i, j+1) == color
		|| (BOARD(i, j+1) == EMPTY
		    && j < board_size-2 && BOARD(i, j+2) != other
		    && i > 0 && BOARD(i-1, j+1) != other
		    && i < board_size-1 && BOARD(i+1, j+1) != other)))
	  ENQUEUE(i, j+1);

	/* For distance one intersections, allow kosumi to move out. I.e.
	 *
	 * ??..
	 * X.*.
	 * ?O.?
	 * ??X?
	 *
	 */
	if (distance == 0) {
	  if (i > 1 && j > 1
	      && BOARD(i, j-1) == EMPTY && BOARD(i-1, j) == EMPTY
	      && (BOARD(i-1, j-1) == color
		  || (BOARD(i-1, j-1) == EMPTY
		      && BOARD(i-2, j-1) != other
		      && BOARD(i-1, j-2) != other)))
	    ENQUEUE(i-1, j-1);

	  if (i > 1 && j < board_size-2
	      && BOARD(i, j+1) == EMPTY && BOARD(i-1, j) == EMPTY
	      && (BOARD(i-1, j+1) == color
		  || (BOARD(i-1, j+1) == EMPTY
		      && BOARD(i-2, j+1) != other
		      && BOARD(i-1, j+2) != other)))
	    ENQUEUE(i-1, j+1);

	  if (i < board_size-2 && j < board_size-2
	      && BOARD(i, j+1) == EMPTY && BOARD(i+1, j) == EMPTY
	      && (BOARD(i+1, j+1) == color
		  || (BOARD(i+1, j+1) == EMPTY
		      && BOARD(i+2, j+1) != other
		      && BOARD(i+1, j+2) != other)))
	    ENQUEUE(i+1, j+1);

	  if (i < board_size-2 && j > 1
	      && BOARD(i, j-1) == EMPTY && BOARD(i+1, j) == EMPTY
	      && (BOARD(i+1, j-1) == color
		  || (BOARD(i+1, j-1) == EMPTY
		      && BOARD(i+2, j-1) != other
		      && BOARD(i+1, j-2) != other)))
	    ENQUEUE(i+1, j-1);
	}
      }
    }
  }

  /* Reset used mx cells. */
  for (k = 0; k < queue_end; k++)
    mx[queuei[k]][queuej[k]] = 0;

  return escape_potential;
}

/* Wrapper to call the function above and compute the escape potential
 * for the dragon at (m, n).
 */
static int
compute_escape(int m, int n, int dragon_status_known)
{
  int i, j;
  char goal[BOARDMAX];
  int escape_value[BOARDMAX];

  ASSERT2(BOARD(m, n) != EMPTY, m, n);
  
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++) {
      goal[POS(i, j)] = same_dragon(i, j, m, n);
    }

  compute_escape_influence(goal, BOARD(m, n), escape_value,
			   dragon_status_known);

  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++) {
      if (dragon_status_known) {
	if (dragon[POS(i, j)].status == ALIVE)
	  escape_value[POS(i, j)] = 6;
	else if (dragon[POS(i, j)].status == UNKNOWN
		 && (DRAGON2(i, j).escape_route > 5 || DRAGON2(i, j).moyo > 5))
	  escape_value[POS(i, j)] = 4;
      }
      else {
	if (BOARD(i, j) == BOARD(m, n)
	    && !goal[POS(i, j)]
	    && worm[POS(i, j)].attack_codes[0] == 0)
	  escape_value[POS(i, j)] = 2;
      }
    }

  return dragon_escape(goal, BOARD(m, n), escape_value);
}


/* 
 * Test whether two dragons are the same. Used by autohelpers and elsewhere.
 */

int
same_dragon(int ai, int aj, int bi, int bj)
{
  if (ai == -1 || aj == -1 || bi == -1 || bj == -1)
    return (ai == bi && aj == bj);
  
  ASSERT_ON_BOARD2(ai, aj);
  ASSERT_ON_BOARD2(bi, bj);

  return (dragon[POS(ai, aj)].origin == dragon[POS(bi, bj)].origin);
}


/* ================================================================ */
/*                       A few status functions                     */
/* ================================================================ */

/*
 * These functions are only here because then we don't need to expose
 * the dragon structure to the external program.
 */

int
dragon_status(int i, int j)
{
  return dragon[POS(i, j)].status;
}


int
matcher_status(int i, int j)
{
  return dragon[POS(i, j)].matcher_status;
}


int
lively_dragon_exists(int color)
{
  if (color == WHITE)
    return lively_white_dragons > 0;
  else
    return lively_black_dragons > 0;
}


/* ================================================================ */
/*                      Debugger functions                          */
/* ================================================================ */

/* For use in gdb, print details of the dragon at (m,n). 
 * Add this to your .gdbinit file:
 *
 * define dragon
 * set ascii_report_dragon("$arg0")
 * end
 *
 * Now 'dragon S8' will report the details of the S8 dragon.
 *
 */

void
ascii_report_dragon(char *string)
{
  int m, n;
  string_to_location(board_size, string, &m, &n);
  report_dragon(m, n);
}


void
report_dragon(int m, int n)
{
  int i, j, k;
  struct dragon_data *d = &(dragon[POS(m, n)]);
  struct dragon_data2 *d2 = &(dragon2[d->id]);
  
  if (BOARD(m, n) == EMPTY) {
    gprintf("There is no dragon at %m\n", m, n);
    return;
  }

  ASSERT2(d->id >= 0, m, n);

  gprintf("*** dragon at %m:\n", m, n);
  gprintf("color: %s; origin: %1m; size: %d; effective size: %f\n",
	  (d->color == WHITE) ? "WHITE" : "BLACK",
	  d->origin, d->size, d->effective_size);

  gprintf("strings:");
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++)
      if (worm[POS(i, j)].origin == POS(i, j)
	  && same_dragon(i, j, m, n))
	gprintf(" %m", i, j);

  gprintf("\nhalf eyes: %d, ", d2->heyes);
  if (d2->heye != NO_MOVE)
    gprintf("half eye: %1m, ", d2->heye);
  else
    gprintf("half eye: NONE,");

  gprintf(" genus %d, escape_route %d,", d2->genus, d2->escape_route);
  if (d2->lunch != NO_MOVE)
    gprintf(" lunch at %1m\n", d2->lunch);
  else
    gprintf(" no lunch\n");

  gprintf("dragon status %s, owl status %s, matcher status %s, owl threat status %s\n",
	  status_to_string(d->status),
	  status_to_string(d->owl_status),
	  status_to_string(d->matcher_status),
	  status_to_string(d->owl_threat_status));

  if (d->owl_attack_point != NO_MOVE)
    gprintf("owl attack point %1m, ", d->owl_attack_point);
  else
    gprintf("no owl attack point, ");

  if (d->owl_second_attack_point != NO_MOVE)
    gprintf("second owl attack point %1m\n", d->owl_second_attack_point);
  else
    gprintf("no second owl attack point\n");

  if (d->owl_defense_point != NO_MOVE)
    gprintf("owl defense point %1m, ", d->owl_defense_point);
  else
    gprintf("no owl defense point, ");

  if (d->owl_second_defense_point != NO_MOVE)
    gprintf("second owl defense point %1m\n", d->owl_second_defense_point);
  else
    gprintf("no second owl defense point\n");

  if (d2->semeai)
    gprintf("This dragon is involved in a semeai. Margin of safety %d\n",
	    d2->semeai_margin_of_safety);
  else
    gprintf("This dragon is not involved in a semeai.\n");

  gprintf("neighbor dragons: ");
  for (k = 0; k < d2->neighbors; k++)
    gprintf("%1m ", dragon2[d2->adjacent[k]].origin);

  gprintf("\nmoyo: %d; safety: %d\n", d2->moyo, d2->safety);
}


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
