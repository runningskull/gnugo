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
static int dragon_invincible(int pos);
static int compute_dragon_status(int pos);
static void dragon_eye(int pos, struct eye_data[BOARDMAX]);
static int compute_escape(int pos, int dragon_status_known);

static int dragon2_initialized;
static int lively_white_dragons;
static int lively_black_dragons;


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
  int ii;
  int i, j;
  int dr;
  int d;
  int k;

  start_timer(2);
  dragon2_initialized = 0;
  
  /* We start with the dragon data copied from the worm data, then
   * modify it as the worms are amalgamated into larger dragons.
   */

  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      ii = POS(m, n);

      dragon[ii].id                 = -1;
      dragon[ii].size               = worm[ii].size;
      dragon[ii].effective_size     = worm[ii].effective_size;
      dragon[ii].color              = worm[ii].color;
      dragon[ii].origin             = worm[ii].origin;
#if 0
      for (i = 0; i < MAX_TACTICAL_POINTS; ++i) {
       dragon[ii].owl_attack_points[i]   = NO_MOVE;
       dragon[ii].owl_attack_codes[i]    = 0;
      }
#else
      dragon[ii].owl_attack_point   = NO_MOVE;
      dragon[ii].owl_attack_code    = 0;
#endif
      dragon[ii].owl_attack_certain = 1;
      dragon[ii].owl_defense_point  = NO_MOVE;
      dragon[ii].owl_defense_code   = 0;
      dragon[ii].owl_defend_certain = 1;
      dragon[ii].owl_status         = UNCHECKED;
      dragon[ii].status             = UNKNOWN;
      dragon[ii].matcher_status     = UNKNOWN;
      dragon[ii].owl_threat_status  = UNCHECKED;
      dragon[ii].owl_second_attack_point  = NO_MOVE;
      dragon[ii].owl_second_defense_point = NO_MOVE;
      half_eye[ii].type             =  0;
      half_eye[ii].value            =  10.0; /* Something big. */
      
      if (IS_STONE(board[ii]) && worm[ii].origin == ii)
	DEBUG(DEBUG_DRAGONS, 
	      "Initialising dragon from worm at %1m, size %d\n", 
	      ii, worm[ii].size);
    }
  time_report(2, "  time to initialize dragons", NO_MOVE, 1.0);

  make_domains(black_eye, white_eye, 0);
  time_report(2, "  time to make domains", NO_MOVE, 1.0);

  /* Find explicit connections patterns in database and amalgamate
   * involved dragons.
   */
  find_connections();
  time_report(2, "  time to find connections", NO_MOVE, 1.0);
  
  /* Amalgamate dragons sharing an eyespace (not ko). At the same time
   * we decide to which dragon an eyespace belongs. Ko eyespaces
   * (typically false eyes but sometimes halfeyes) get assigned to an
   * arbitrary neighbor that is not the ko stone.
   */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      ii = POS(m, n);

      if (black_eye[ii].color == BLACK_BORDER
	  && black_eye[ii].origin == ii) {
	if (!is_ko_point(ii)
	    || black_eye[ii].esize > 1) /* Only exclude living kos. */
	  dragon_eye(ii, black_eye);
	else {
	  for (k = 0; k < 4; k++)
	    if (ON_BOARD(ii + delta[k]) && !is_ko_point(ii + delta[k])) {
	      black_eye[ii].dragon = dragon[ii + delta[k]].origin;
	      break;
	    }
	}
      }
	  
      if (white_eye[ii].color == WHITE_BORDER
	  && white_eye[ii].origin == ii) {
	if (!is_ko_point(ii)
	    || white_eye[ii].esize > 1) /* Only exclude living kos. */
	  dragon_eye(ii, white_eye);
	else {
	  for (k = 0; k < 4; k++)
	    if (ON_BOARD(ii + delta[k]) && !is_ko_point(ii + delta[k])) {
	      white_eye[ii].dragon = dragon[ii + delta[k]].origin;
	      break;
	    }
	}
      }
    }
  time_report(2, "  time to amalgamate dragons", NO_MOVE, 1.0);

  /* At this time, all dragons have been finalized and we can
   * initialize the dragon2[] array. After that we can no longer allow
   * amalgamation of dragons.
   */
  initialize_supplementary_dragon_data();
  time_report(2, "  time to initialize dragon2", NO_MOVE, 1.0);
  
  /* Find adjacent worms which can be easily captured: */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      int food;

      ii = POS(m, n);

      if (worm[ii].origin != ii
	  || board[ii] == EMPTY
	  || worm[ii].lunch == NO_MOVE)
	continue;

      food = worm[ii].lunch;

      /* In contrast to worm lunches, a dragon lunch must also be
       * able to defend itself. 
       */
      if (worm[food].defend_codes[0] == 0)
	continue;

      /* Tell the move generation code about the lunch. */
      if (IS_STONE(color))
	add_lunch(ii, food);
	
      /* If several lunches are found, we pick the juiciest.
       * First maximize cutstone, then minimize liberties.
       */
      {
	int origin = dragon[ii].origin;
	int lunch = DRAGON2(origin).lunch;

	if (lunch == NO_MOVE
	    || worm[food].cutstone > worm[lunch].cutstone
	    || (worm[food].cutstone == worm[lunch].cutstone
		&& (worm[food].liberties < worm[lunch].liberties))) {
	  DRAGON2(origin).lunch = worm[food].origin;
	  TRACE("at %1m setting %1m.lunch to %1m (cutstone=%d)\n",
		ii, origin,
		worm[food].origin, worm[food].cutstone);
	}
      }
    }
  time_report(2, "  time to find lunches", NO_MOVE, 1.0);

  /* In case origins of dragons got moved, put the dragons of eyes aright. */
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++) {
      ii = POS(i, j);

      if (black_eye[ii].dragon != NO_MOVE) {
	int dr = dragon[black_eye[ii].dragon].origin;
	black_eye[ii].dragon = dr;
      }

      if (white_eye[ii].dragon != NO_MOVE) {
	int dr = dragon[white_eye[ii].dragon].origin;
	  white_eye[ii].dragon = dr;
      }
    }
  time_report(2, "  time to fix origins", NO_MOVE, 1.0);

  /* Find topological half eyes and false eyes by analyzing the
   * diagonal intersections, as described in the Texinfo
   * documentation (Eyes/Eye Topology).
   *
   * FIXME: Consolidate this piece of code with the very similar one
   * in owl_determine_life().
   */

  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      float sum;
      ii = POS(m, n);

      if (black_eye[ii].color == BLACK_BORDER
	  && (!black_eye[ii].marginal || life)
	  && black_eye[ii].neighbors <= 1
	  && black_eye[ii].dragon != NO_MOVE) {
	sum = topological_eye(ii, BLACK, black_eye, white_eye, half_eye);
	if (sum >= 4.0) {
	  half_eye[ii].type = FALSE_EYE;
	  if (black_eye[ii].esize == 1
	      || is_legal(ii, WHITE)
	      || board[ii] == WHITE)
	    add_false_eye(ii, black_eye, half_eye);
	}
	else if (sum > 2.0)
	  half_eye[ii].type = HALF_EYE;
      }
      
      if (white_eye[ii].color == WHITE_BORDER
	  && (!white_eye[ii].marginal || life)
	  && white_eye[ii].neighbors <= 1
	  && white_eye[ii].dragon != NO_MOVE) {
	sum = topological_eye(ii, WHITE, black_eye, white_eye, half_eye);
	if (sum >= 4.0) {
	  half_eye[ii].type = FALSE_EYE;
	  if (white_eye[ii].esize == 1
	      || is_legal(ii, BLACK)
	      || board[ii] == BLACK)
	    add_false_eye(ii, white_eye, half_eye);
	}
	else if (sum > 2.0)
	  half_eye[ii].type = HALF_EYE;
      }
    }

  /* Pattern based modification of the eye shapes computed by
   * make_domains and halfeye analysis.
   */
  modify_eye_spaces();
  
  /* Compute the number of eyes, half eyes, etc. in an eye space. */
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++) {
      ii = POS(i, j);

      if (black_eye[ii].color == BLACK_BORDER
	  && black_eye[ii].origin == ii)
      {
	int max, min, attack_point, defense_point;

	compute_eyes(ii, &max, &min, &attack_point, &defense_point, 
		     black_eye, half_eye, 1, color);
	DEBUG(DEBUG_EYES, "Black eyespace at %1m: min=%d, max=%d\n",
	      ii, min, max);
	black_eye[ii].maxeye = max;
	black_eye[ii].mineye = min;
	black_eye[ii].attack_point = attack_point;
	black_eye[ii].defense_point = defense_point;
	propagate_eye(ii, black_eye);
      }

      if (white_eye[ii].color == WHITE_BORDER
	  && white_eye[ii].origin == ii)
      {
	int max, min, attack_point, defense_point;

	compute_eyes(ii, &max, &min, &attack_point, &defense_point,
		     white_eye, half_eye, 1, color);
	DEBUG(DEBUG_EYES, "White eyespace at %1m: min=%d, max=%d\n",
	      ii, min, max);
	white_eye[ii].maxeye = max;
	white_eye[ii].mineye = min;
	white_eye[ii].attack_point = attack_point;
	white_eye[ii].defense_point = defense_point;
	propagate_eye(ii, white_eye);
      }
    }
  time_report(2, "  time to find eyes", NO_MOVE, 1.0);

  /* Now we compute the genus. */
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++) {
      int ii = POS(i, j);

      if (black_eye[ii].color == BLACK_BORDER
	  && black_eye[ii].dragon != NO_MOVE
	  && black_eye[ii].origin == ii)
      {
	dr = black_eye[ii].dragon;

	gg_assert(board[dr] == BLACK);
	TRACE("eye at %1m found for dragon at %1m--augmenting genus\n",
	      ii, dr);
	DRAGON2(dr).genus += (black_eye[ii].mineye);
	DRAGON2(dr).heyes += (black_eye[ii].maxeye - black_eye[ii].mineye);
	if (black_eye[ii].maxeye - black_eye[ii].mineye > 0)
	  DRAGON2(dr).heye = black_eye[ii].attack_point;
      }
      if ((white_eye[ii].color == WHITE_BORDER) 
	  && (white_eye[ii].dragon != NO_MOVE)
	  && (white_eye[ii].origin == ii)) 
      {
	dr = white_eye[ii].dragon;

	gg_assert (board[dr] == WHITE);
	TRACE("eye at %1m found for dragon at %1m--augmenting genus\n", 
	      ii, dr);
	DRAGON2(dr).genus += (white_eye[ii].mineye);
	DRAGON2(dr).heyes += (white_eye[ii].maxeye - white_eye[ii].mineye);
	if (white_eye[ii].maxeye - white_eye[ii].mineye > 0) {
	  DRAGON2(dr).heye = white_eye[ii].attack_point;
	}
      }
    }
  time_report(2, "  time to compute genus", NO_MOVE, 1.0);

  /* Compute the escape route measure. */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      ii = POS(m, n);

      if (dragon[ii].origin == ii
	  && IS_STONE(board[ii])) {
	DRAGON2(ii).escape_route = compute_escape(ii, 0);
      }
    }
  time_report(2, "  time to compute escape", NO_MOVE, 1.0);

  /* Update the segmentation of the initial influence before we
   * compute the surrounding moyo sizes. The reason for this is that
   * now the eyespace inhibition found by find_cuts() can be taken
   * into account.
   */
  resegment_initial_influence();
  time_report(2, "  resegment_initial_influence", NO_MOVE, 1.0);

  /* Compute the surrounding moyo sizes. */
  for (d = 0; d < number_of_dragons; d++) {
    dragon2[d].moyo = influence_get_moyo_size(dragon2[d].origin,
					      DRAGON(d).color);
  }
  time_report(2, "  influence_get_moyo_size", NO_MOVE, 1.0);

  /* Determine status: ALIVE, DEAD, CRITICAL or UNKNOWN */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      ii = POS(m, n);

      if (dragon[ii].origin == ii && board[ii]) {
	dragon[ii].status = compute_dragon_status(ii);
	sgffile_dragon_status(I(ii), J(ii), dragon[ii].status);
      }
    }
  time_report(2, "  compute_dragon_status", NO_MOVE, 1.0);

  /* We must update the dragon status at every intersection before we
   * call the owl code. This updates all fields.
   */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      int ii = POS(m, n);
      struct dragon_data *d = &(dragon[ii]);

      dragon[ii] = dragon[d->origin];
    }

  find_neighbor_dragons();
  time_report(2, "  find_neighbor_dragons", NO_MOVE, 1.0);

  if (stop_before_owl)
    return;
  
  /* Determine life and death status of each dragon using the owl code
   * if necessary.
   */
  purge_persistent_owl_cache();

  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      int attack_point = NO_MOVE;
      int second_attack_point = NO_MOVE;
      int defense_point = NO_MOVE;
      int second_defense_point = NO_MOVE;
      
      ii = POS(m, n);

      if (board[ii] == EMPTY
	  || dragon[ii].origin != ii)
	continue;

      /* Some dragons can be ignored. But
       * Be extra careful with big dragons. */
      if (DRAGON2(ii).escape_route > 25
	  || DRAGON2(ii).moyo > 20
	  || (DRAGON2(ii).moyo > 10
	      && DRAGON2(ii).moyo > dragon[ii].size)) {
	dragon[ii].owl_status = UNCHECKED;
	dragon[ii].owl_threat_status = UNCHECKED;
	dragon[ii].owl_attack_point  = NO_MOVE;
	dragon[ii].owl_defense_point = NO_MOVE;
	dragon[ii].owl_second_attack_point  = NO_MOVE;
	dragon[ii].owl_second_defense_point = NO_MOVE;
      }
      else {
	int acode = 0;
	int dcode = 0;
	start_timer(3);
	acode = owl_attack(ii, &attack_point, &dragon[ii].owl_attack_certain);
	if (acode != 0) {
	  dragon[ii].owl_attack_point = attack_point;
	  dragon[ii].owl_attack_code = acode;
	  if (attack_point != NO_MOVE) {
	    dcode = owl_defend(ii, &defense_point,
			       &dragon[ii].owl_defend_certain);
	    if (dcode != 0) {
	      if (defense_point != NO_MOVE) {
		dragon[ii].owl_status = CRITICAL;
		dragon[ii].owl_defense_point = defense_point;
		dragon[ii].owl_defense_code = dcode;
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
		dragon[ii].owl_status = CRITICAL;
		DEBUG(DEBUG_OWL_PERFORMANCE,
		      "Inconsistent owl attack and defense results for %1m.\n", 
		      ii);
	      }
	    }
	  }
	  if (dcode == 0) {
	    dragon[ii].owl_status = DEAD; 
	    dragon[ii].owl_defense_point = NO_MOVE;
	    dragon[ii].owl_defense_code = 0;
	    if (level >= 8
		&& !disable_threat_computation) {
	      if (owl_threaten_defense(ii, &defense_point,
				       &second_defense_point)) {
		dragon[ii].owl_threat_status = CAN_THREATEN_DEFENSE;
		dragon[ii].owl_defense_point = defense_point;
		dragon[ii].owl_second_defense_point = second_defense_point;
	      }
	      else
		dragon[ii].owl_threat_status = DEAD;;
	    }
	  }
	}
	else {
	  if (!dragon[ii].owl_attack_certain) {
	    dcode = owl_defend(ii, &defense_point, 
			       &dragon[ii].owl_defend_certain);
	    if (dcode != 0) {
	      /* If the result of owl_attack was not certain, we may
	       * still want the result of owl_defend */
	      dragon[ii].owl_defense_point = defense_point;
	      dragon[ii].owl_defense_code = dcode;
	    }
	  }
	  dragon[ii].owl_status = ALIVE;
	  dragon[ii].owl_attack_point = NO_MOVE;
	  dragon[ii].owl_attack_code = 0;
	  if (level >= 8
	      && !disable_threat_computation) {
	    if (owl_threaten_attack(ii, &attack_point, &second_attack_point)) {
	      dragon[ii].owl_threat_status = CAN_THREATEN_ATTACK;
	      dragon[ii].owl_attack_point = attack_point;
	      dragon[ii].owl_second_attack_point = second_attack_point;
	    }
	    else
	      dragon[ii].owl_threat_status = ALIVE;
	  }
	}
	time_report(3, "    owl reading for dragon at ", ii, 1.0);
      }
    }
  time_report(2, "  owl reading", NO_MOVE, 1.0);

  /* The dragon data is now correct at the origin of each dragon but
   * we need to copy it to every vertex.  
   */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      int ii = POS(m, n);
      struct dragon_data *d = &(dragon[ii]);
      dragon[ii] = dragon[d->origin];
    }

  /* Compute the status to be used by the matcher. We most trust the
   * owl status, if it is available.
   */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      ii = POS(m, n);

      if (IS_STONE(board[ii])) {
	if (dragon[ii].owl_status != UNCHECKED)
	  dragon[ii].matcher_status = dragon[ii].owl_status;
	else if (dragon[ii].status == DEAD 
		 || dragon[ii].status == CRITICAL) {
	  /* If a dragon has sufficient escape potential or
	   * surrounding moyo to stop the owl code from being run, the
	   * matcher_status should be no worse than UNKNOWN,
	   * regardless what the static life and death analysis
	   * guesses.
	   */
	  dragon[ii].matcher_status = UNKNOWN;
	}
	else
	  dragon[ii].matcher_status = dragon[ii].status;
      }
    }
  time_report(2, "  compute matcher status", NO_MOVE, 1.0);

  /* Compute the safety value. */
  for (d = 0; d < number_of_dragons; d++) {
    int true_genus;
    int origin = dragon2[d].origin;

    true_genus = 2 * dragon2[d].genus + dragon2[d].heyes;
    /* FIXME: Probably need a better definition of INESSENTIAL dragons.
     *        There are cases where a string is owl insubstantial
     *        yet allowing it to be captured greatly weakens our
     *        position.
     */
    if (dragon[origin].size == worm[origin].size
	&& !owl_substantial(origin))
      dragon2[d].safety = INESSENTIAL;
    else if (dragon[origin].size == worm[origin].size
	     && worm[origin].attack_codes[0] != 0
	     && worm[origin].defend_codes[0] == 0)
      dragon2[d].safety = TACTICALLY_DEAD;
    else if (0) /* Seki is detected by the call to semeai() below. */
      dragon2[d].safety = ALIVE_IN_SEKI;
    else if (dragon[origin].owl_status == DEAD)
      dragon2[d].safety = DEAD;
    else if (dragon[origin].owl_status == CRITICAL)
      dragon2[d].safety = CRITICAL;
    else if (dragon[origin].owl_status == UNCHECKED
	     && true_genus < 4
	     && dragon2[d].moyo <= 10)
      dragon2[d].safety = WEAK;
    else if (dragon_invincible(origin))
      dragon2[d].safety = INVINCIBLE;
    else if (true_genus >= 6 || dragon2[d].moyo > 20)
      dragon2[d].safety = STRONGLY_ALIVE;
    else if ((2 * true_genus + dragon2[d].moyo
	      + 2 * (dragon2[d].lunch != NO_MOVE) < 8
	      && dragon2[d].escape_route < 10)
	     || (dragon[origin].owl_threat_status == CAN_THREATEN_ATTACK)) {
      if (DRAGON(d).owl_attack_certain)
	  dragon2[d].safety = WEAKLY_ALIVE;
      else
	  dragon2[d].safety = WEAK;
    }
    else
      dragon2[d].safety = ALIVE;
  }
  time_report(2, "  compute dragon safety", NO_MOVE, 1.0);

  /* Resolve semeais. This may revise the safety and status fields. */
  semeai(color);
  time_report(2, "  semeai module", NO_MOVE, 1.0);

  /* The matcher_status is now correct at the origin of each dragon
   * but we need to copy it to every vertex.
   */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      ii = POS(m, n);

      dragon[ii].matcher_status = dragon[dragon[ii].origin].matcher_status;
    }

  /* Revise essentiality of critical worms. Specifically, a critical
   * worm which is adjacent to no enemy dragon with matcher_status
   * better than DEAD, is considered INESSENTIAL.
   */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      ii = POS(m, n);
      if (is_worm_origin(ii, ii)
	  && worm[ii].attack_codes[0] != 0
	  && worm[ii].defend_codes[0] != 0
	  && !worm[ii].inessential) {
	int adjs[MAXCHAIN];
	int neighbors;
	int r;
	int essential = 0;
	
	neighbors = chainlinks(ii, adjs);
	for (r = 0; r < neighbors; r++)
	  if (dragon[adjs[r]].matcher_status != DEAD) {
	    essential = 1;
	    break;
	  }

	if (!essential) {
	  DEBUG(DEBUG_WORMS, "Worm %1m revised to be inessential.\n", ii);
	  worm[ii].inessential = 1;
	  propagate_worm(ii);
	}
      }
    }
  time_report(2, "  revise inessentiality", NO_MOVE, 1.0);

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
  int d;
  
  /* Give each dragon (caves excluded) an id number for indexing into
   * the dragon2 array. After this the DRAGON2 macro can be used.
   */
  number_of_dragons = 0;
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      int ii = POS(m, n);
      int origin = dragon[ii].origin;

      if (board[ii] == EMPTY)
	continue;

      if (dragon[origin].id == -1)
	dragon[origin].id = number_of_dragons++;
      dragon[ii].id = dragon[origin].id;
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
      int ii = POS(m, n);

      if (IS_STONE(board[ii])
	  && dragon[ii].origin == ii) {
	DRAGON2(ii).origin = ii;
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
  int ii;
  int i, j;
  int jj;
  int d;
  int dragons[BOARDMAX];
  int distances[BOARDMAX];
  int dist;
  int k;
  int color;

  gg_assert(dragon2_initialized);
  
  /* Initialize the arrays. */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      ii = POS(m, n);

      if (IS_STONE(board[ii])) {
	dragons[ii] = dragon[ii].id;
	distances[ii] = 0;
      }
      else {
	dragons[ii] = -1;
	distances[ii] = -1;
      }
    }

  /* Expand from dist-1 to dist. Break out of the loop at the end if
     * we couldn't expand anything. Never expand more than five steps.
     */
  for (dist = 1; dist <= 5; dist++) {
    int found_one = 0;
      
    for (m = 0; m < board_size; m++)
      for (n = 0; n < board_size; n++) {
	ii = POS(m, n);

	if (distances[ii] != dist-1 || dragons[ii] < 0)
	  continue;

	color = DRAGON(dragons[ii]).color;
	for (k = 0; k < 4; k++) {
	  jj = ii + delta[k];

	  if (!ON_BOARD1(jj))
	    continue;

	  /* Consider expansion from (ii) to adjacent intersection
	   * (jj).
	   */
	  if (distances[jj] >= 0 && distances[jj] < dist)
	    continue; /* (jj) already occupied. */

	  if (influence_area_color(ii) == color
	      && influence_area_color(jj) != OTHER_COLOR(color)) {
	    /* Expansion ok. Now see if someone else has tried to
	     * expand here. In that case we indicate a collision by
	     * setting the dragon number to -2.
	     */
	    if (distances[jj] == dist) {
	      if (dragons[jj] != dragons[ii])
		dragons[jj] = -2;
	    }
	    else {
	      dragons[jj] = dragons[ii];
	      distances[jj] = dist;
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
	fprintf(stderr, "%3d", dragons[POS(m, n)]);
      }
      fprintf(stderr, "\n");
    }
    fprintf(stderr, "\n");
      
    for (m = 0; m < board_size; m++) {
      for (n = 0; n < board_size; n++) {
	fprintf(stderr, "%3d", distances[POS(m, n)]);
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
      ii = POS(m, n);

      if (dragons[ii] == -2) {
	int neighbors = 0;
	int adjacent[4];

	for (k = 0; k < 4; k++) {
	  jj = ii + delta[k];

	  if (ON_BOARD1(jj) && dragons[jj] >= 0)
	    adjacent[neighbors++] = dragons[jj];
	}
	for (i = 0; i < neighbors; i++)
	  for (j = i+1; j < neighbors; j++)
	    add_adjacent_dragons(adjacent[i], adjacent[j]);
      }
      else if (dragons[ii] >= 0) {
	if (ON_BOARD(NORTH(ii))) {
	  if (dragons[NORTH(ii)] >= 0
	      && dragons[NORTH(ii)] != dragons[ii])
	    add_adjacent_dragons(dragons[ii], dragons[NORTH(ii)]);
	}
	if (ON_BOARD(EAST(ii))) {
	  if (dragons[EAST(ii)] >= 0
	      && dragons[EAST(ii)] != dragons[ii])
	    add_adjacent_dragons(dragons[ii], dragons[EAST(ii)]);
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
dragon_invincible(int pos)
{

  struct eye_data *eye;

  int i, j;
  int ii;

  int strong_eyes = 0;

  gg_assert(IS_STONE(board[pos]));

  /* First look for invincible strings in the dragon. */
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++) {
      ii = POS(i, j);

      if (is_same_dragon(ii, pos) && worm[ii].invincible)
        return 1;
    }

  /* Examine the eye spaces.
   * FIXME: The check for half eyes or false eyes may be too weak.
   */
  if (board[pos] == BLACK)
    eye = black_eye;
  else
    eye = white_eye;

  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++) {
      ii = POS(i, j);

      if (eye[ii].origin == ii
          && is_same_dragon(eye[ii].dragon, pos)) {
        if (eye[ii].msize == 0 && eye[ii].mineye > 0)
          strong_eyes++;
      }
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
  int ii;
  int k;

  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      struct worm_data *w = &(worm[POS(m, n)]);

      ii = POS(m, n);

      if (w->origin == ii) {
	if (board[ii]) {
	  gprintf("%1m : (dragon %1m) %s string of size %d (%f), genus %d: (%d,%d,%d,%d)",
		  ii, dragon[ii].origin,
		  color_to_string(board[ii]),
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
	  
	  if (is_ko_point(POS(m, n)))
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

      ii = POS(m, n);

      if (d->origin == ii) {
	if (board[ii]) {
	  gprintf("%1m : %s dragon size %d (%f), genus %d, half eyes %d, escape factor %d, status %s, matcher status %s, moyo size %d safety %s",
		  ii,
		  board[ii]==BLACK ? "B" : "W",
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
	    gprintf("... owl attackable at %1m, code %d\n",
		    d->owl_attack_point, d->owl_attack_code);
	    gprintf("... owl defendable at %1m, code %d\n",
		    d->owl_defense_point, d->owl_defense_code);
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
 * dragon_eye(pos, eye_data) is invoked with (pos) the origin of an
 * eyespace. It unites all the worms adjacent to non-marginal points
 * of the eyespace into a single dragon.. In addition to marginal eye
 * space points, amalgamation is inhibited for points with the
 * INHIBIT_CONNECTION type set.
 *
 * This is based on the older function dragon_ring.
 */

static void
dragon_eye(int pos, struct eye_data eye[BOARDMAX])
{
  int i, j;
  int ii;
  int dr = NO_MOVE;
  int color;
  int k;

  /* don't amalgamate across ikken tobi */
  if (eye[pos].esize == 3 && eye[pos].msize > 1)
    return;

  DEBUG(DEBUG_DRAGONS, "amalgamate dragons around %1m\n", pos);
  if (eye[pos].color == BLACK_BORDER)
    color = BLACK;
  else {
    gg_assert(eye[pos].color == WHITE_BORDER);
    color = WHITE;
  }

  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++) {
      ii = POS(i, j);

      if (eye[ii].origin == pos
	  && !eye[ii].marginal
	  && !(eye[ii].type & INHIBIT_CONNECTION)) {
	for (k = 0; k < 4; k++) {
	  int d = delta[k];

	  if (board[ii+d] == color) {
	    if (dr == NO_MOVE)
	      dr = dragon[ii+d].origin;
	    else if (dragon[ii+d].origin != dr) {
	      join_dragons(ii+d, dr);
	      dr = dragon[ii+d].origin;
	    }
	  }
	}
      }
    }
  
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++) {
      ii = POS(i, j);

      if ((eye[ii].color == BLACK_BORDER 
	   || eye[ii].color == WHITE_BORDER) 
	  && eye[ii].origin == pos)
      {
	eye[ii].dragon = dr;
      }
    }
}


/* 
 * join_dragons amalgamates the dragon at (d1) to the
 * dragon at (d2).
 */

void 
join_dragons(int d1, int d2)
{
  int ii;
  int origin; /* new origin */

  /* Normalize dragon coordinates. */
  d1 = dragon[d1].origin;
  d2 = dragon[d2].origin;
  
  gg_assert(board[d1] == board[d2]);
  gg_assert(dragon2_initialized == 0);
  gg_assert(IS_STONE(board[d1]));

  /* We want to have the origin pointing to the largest string of
   * the dragon.  If this is not unique, we take the "upper
   * leftmost" one.
   */
  if (worm[d1].size > worm[d2].size
      || (worm[d1].size == worm[d2].size
	  && d1 < d2)) {
    origin = d1;
    DEBUG(DEBUG_DRAGONS, "joining dragon at %1m to dragon at %1m\n", d2, d1);
  }
  else {
    origin = d2;
    DEBUG(DEBUG_DRAGONS, "joining dragon at %1m to dragon at %1m\n", d1, d2);
  }
  
  dragon[origin].size  = dragon[d2].size + dragon[d1].size;
  dragon[origin].effective_size  = (dragon[d2].effective_size
				    + dragon[d1].effective_size);

  for (ii = BOARDMIN; ii < BOARDMAX; ii++) {
    if (ON_BOARD(ii)
	&& (dragon[ii].origin == d1 || dragon[ii].origin == d2))
      dragon[ii].origin = origin;
  }
}



/*
 * compute_dragon_status(pos) tries to determine whether the dragon
 * at (pos) is ALIVE, DEAD, or UNKNOWN. The algorithm is not perfect
 * and can give incorrect answers.
 *
 * The dragon is judged alive if its genus is >1. It is judged dead if
 * the genus is <2, it has no escape route, and no adjoining string can
 * be easily captured. Otherwise it is judged UNKNOWN.  */

static int 
compute_dragon_status(int pos)
{
  int true_genus = 2*DRAGON2(pos).genus + DRAGON2(pos).heyes;
  int lunch = DRAGON2(pos).lunch;

  gg_assert(dragon2_initialized);
  
  /* If it has two sure eyes, everything is just dandy. */
  if (true_genus > 3)
    return ALIVE;

  /* If the dragon consists of one worm, there is an attack, but 
   * no defense and there is less than one eye and one half eye,
   * the situation is hopeless.
   */
  if (dragon[pos].size == worm[pos].size
      && worm[pos].attack_codes[0] != 0 
      && worm[pos].defend_codes[0] == 0
      && true_genus < 3)
    return DEAD;
  
  if (lunch != NO_MOVE
      && true_genus < 3
      && worm[lunch].defend_codes[0] != 0
      && DRAGON2(pos).escape_route < 5)
    if (true_genus == 2 || worm[lunch].size > 2)
      return CRITICAL;

  if (lunch != NO_MOVE
      && true_genus >= 3)
    return ALIVE;

  if (lunch == NO_MOVE || worm[lunch].cutstone < 2) {
    if (true_genus < 3
	&& DRAGON2(pos).escape_route == 0
	&& DRAGON2(pos).moyo < 5)
      return DEAD;

    if (true_genus == 3
	&& DRAGON2(pos).escape_route < 5)
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
 * be by direct adjacency or, in the first step, diagonally if both
 * adjoining intersections are empty.
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

#define ENQUEUE(pos) (queue[queue_end++] = (pos),\
		      mx[pos] = 1)

/* Compute the escape potential described above. The dragon is marked
 * in the goal array.
 */
int
dragon_escape(char goal[BOARDMAX], int color,
	      int escape_value[BOARDMAX])
{
  int i, j;
  int ii;
  int k;
  static int mx[BOARDMAX];
  static int mx_initialized = 0;
  int queue[MAX_BOARD * MAX_BOARD];
  int queue_start = 0;
  int queue_end = 0;
  int other = OTHER_COLOR(color);
  int distance;
  int escape_potential = 0;

  gg_assert(IS_STONE(color));
  
  if (!mx_initialized) {
    memset(mx, 0, sizeof(mx));
    mx_initialized = 1;
  }

  /* Enter the stones of the dragon in the queue. */
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++) {
      ii = POS(i, j);

      if (goal[ii])
	ENQUEUE(ii);
    }
  
  /* Find points at increasing distances from the dragon. At distance
   * four, sum the escape values at those points to get the escape
   * potential.
   */
  for (distance = 0; distance <= 4; distance++) {
    int save_queue_end = queue_end;
    while (queue_start < save_queue_end) {
      ii = queue[queue_start];
      queue_start++;

      /* Do not pass connection inhibited intersections. */
      if ((color == WHITE
	   && ((white_eye[ii].type & INHIBIT_CONNECTION)
	       || white_eye[ii].cut == 1))
	  || (color == BLACK
	      && ((black_eye[ii].type & INHIBIT_CONNECTION)
		  || black_eye[ii].cut == 1)))
	continue;
      if (distance == 4)
	escape_potential += escape_value[ii];
      else {
	if (ON_BOARD(SOUTH(ii))
	    && !mx[SOUTH(ii)]
	    && (board[SOUTH(ii)] == color
		|| (board[SOUTH(ii)] == EMPTY
		    && ON_BOARD(SE(ii)) && board[SE(ii)] != other
		    && ON_BOARD(SS(ii)) && board[SS(ii)] != other
		    && ON_BOARD(SW(ii)) && board[SW(ii)] != other)))
	  ENQUEUE(SOUTH(ii));
	
	if (ON_BOARD(WEST(ii))
	    && !mx[WEST(ii)]
	    && (board[WEST(ii)] == color
		|| (board[WEST(ii)] == EMPTY
		    && ON_BOARD(SW(ii)) && board[SW(ii)] != other
		    && ON_BOARD(WW(ii)) && board[WW(ii)] != other
		    && ON_BOARD(NW(ii)) && board[NW(ii)] != other)))
	  ENQUEUE(WEST(ii));
	
	if (ON_BOARD(NORTH(ii))
	    && !mx[NORTH(ii)]
	    && (board[NORTH(ii)] == color
		|| (board[NORTH(ii)] == EMPTY
		    && ON_BOARD(NW(ii)) && board[NW(ii)] != other
		    && ON_BOARD(NN(ii)) && board[NN(ii)] != other
		    && ON_BOARD(NE(ii)) && board[NE(ii)] != other)))
	  ENQUEUE(NORTH(ii));
	
	if (ON_BOARD(EAST(ii))
	    && !mx[EAST(ii)]
	    && (board[EAST(ii)] == color
		|| (board[EAST(ii)] == EMPTY
		    && ON_BOARD(NE(ii)) && board[NE(ii)] != other
		    && ON_BOARD(EE(ii)) && board[EE(ii)] != other
		    && ON_BOARD(SE(ii)) && board[SE(ii)] != other)))
	  ENQUEUE(EAST(ii));
	
	/* For distance one intersections, allow kosumi to move out. I.e.
	 *
	 * ??..
	 * X.*.
	 * ?O.?
	 * ??X?
	 *
	 */
	if (distance == 0) {
	  if (board[SOUTH(ii)] == EMPTY
	      && board[WEST(ii)] == EMPTY
	      && (board[SW(ii)] == color
		  || (board[SW(ii)] == color
		      && ON_BOARD(SOUTH(SW(ii)))
		      && board[SOUTH(SW(ii))] != other
		      && ON_BOARD(WEST(SW(ii)))
		      && board[WEST(SW(ii))] != other)))
	    ENQUEUE(SW(ii));
		      
	  if (board[WEST(ii)] == EMPTY
	      && board[NORTH(ii)] == EMPTY
	      && (board[NW(ii)] == color
		  || (board[NW(ii)] == color
		      && ON_BOARD(WEST(NW(ii)))
		      && board[WEST(NW(ii))] != other
		      && ON_BOARD(NORTH(NW(ii)))
		      && board[NORTH(NW(ii))] != other)))
	    ENQUEUE(NW(ii));
		      
	  if (board[NORTH(ii)] == EMPTY
	      && board[EAST(ii)] == EMPTY
	      && (board[NE(ii)] == color
		  || (board[NE(ii)] == color
		      && ON_BOARD(NORTH(NE(ii)))
		      && board[NORTH(NE(ii))] != other
		      && ON_BOARD(EAST(NE(ii)))
		      && board[EAST(NE(ii))] != other)))
	    ENQUEUE(NE(ii));
		      
	  if (board[EAST(ii)] == EMPTY
	      && board[SOUTH(ii)] == EMPTY
	      && (board[SE(ii)] == color
		  || (board[SE(ii)] == color
		      && ON_BOARD(EAST(SE(ii)))
		      && board[EAST(SE(ii))] != other
		      && ON_BOARD(SOUTH(SE(ii)))
		      && board[SOUTH(SE(ii))] != other)))
	    ENQUEUE(SE(ii));
	}
      }
    }
  }

  /* Reset used mx cells. */
  for (k = 0; k < queue_end; k++)
    mx[queue[k]] = 0;

  return escape_potential;
}

/* Wrapper to call the function above and compute the escape potential
 * for the dragon at (pos).
 */
static int
compute_escape(int pos, int dragon_status_known)
{
  int i, j;
  int ii;
  char goal[BOARDMAX];
  int escape_value[BOARDMAX];

  ASSERT1(IS_STONE(board[pos]), pos);
  
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++) {
      ii = POS(i, j);

      goal[ii] = is_same_dragon(ii, pos);
    }

  /* Compute escape_value array.  Points are awarded for moyo (4),
   * area (2) or EMPTY (1).  Values may change without notice.
   */
  compute_escape_influence(goal, board[pos], escape_value,
			   dragon_status_known);

  /* If we can reach a live group, award 6 points. */
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++) {
      ii = POS(i, j);

      if (dragon_status_known) {
	if (dragon[ii].status == ALIVE)
	  escape_value[ii] = 6;
	else if (dragon[ii].status == UNKNOWN
		 && (DRAGON2(ii).escape_route > 5 || DRAGON2(ii).moyo > 5))
	  escape_value[ii] = 4;
      }
      else {
	if (board[ii] == board[pos]
	    && !goal[ii]
	    && worm[ii].attack_codes[0] == 0)
	  escape_value[ii] = 2;
      }
    }

  return dragon_escape(goal, board[pos], escape_value);
}


/* 
 * Test whether two dragons are the same. Used by autohelpers and elsewhere.
 */

int
is_same_dragon(int d1, int d2)
{
  if (d1 == NO_MOVE || d2 == NO_MOVE)
    return (d1 == d2);
  
  ASSERT_ON_BOARD1(d1);
  ASSERT_ON_BOARD1(d2);

  return (dragon[d1].origin == dragon[d2].origin);
}

/* Test whether two dragons are neighbors. */
int
are_neighbor_dragons(int d1, int d2)
{
  int k;
  d1 = dragon[d1].origin;
  d2 = dragon[d2].origin;
  
  for (k = 0; k < DRAGON2(d1).neighbors; k++)
    if (dragon2[DRAGON2(d1).adjacent[k]].origin == d2)
      return 1;

  /* Just to be make sure that this function is always symmetric, we
   * do it the other way round too.
   */
  for (k = 0; k < DRAGON2(d2).neighbors; k++)
    if (dragon2[DRAGON2(d2).adjacent[k]].origin == d1)
      return 1;

  return 0;
}

/* ================================================================ */
/*                       A few status functions                     */
/* ================================================================ */

/*
 * These functions are only here because then we don't need to expose
 * the dragon structure to the external program.
 */

int
dragon_status(int pos)
{
  return dragon[pos].status;
}


int
matcher_status(int pos)
{
  return dragon[pos].matcher_status;
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
  int pos = POS(m, n);
  int i, j;
  int ii;
  int k;
  struct dragon_data *d = &(dragon[pos]);
  struct dragon_data2 *d2 = &(dragon2[d->id]);
  
  if (board[pos] == EMPTY) {
    gprintf("There is no dragon at %1m\n", pos);
    return;
  }

  if (d->id < 0) {
    gprintf("Dragon data not available at %1m\n", pos);
    return;
  }

  gprintf("*** dragon at %1m:\n", pos);
  gprintf("color: %s; origin: %1m; size: %d; effective size: %f\n",
	  (d->color == WHITE) ? "WHITE" : "BLACK",
	  d->origin, d->size, d->effective_size);

  gprintf("strings:");
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++) {
      ii = POS(i, j);

      if (worm[ii].origin == ii
	  && is_same_dragon(ii, pos))
	gprintf(" %1m", ii);
    }

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
    gprintf("owl attack point %1m, code %d, ",
	    d->owl_attack_point, d->owl_attack_code);
  else
    gprintf("no owl attack point, ");

  if (d->owl_second_attack_point != NO_MOVE)
    gprintf("second owl attack point %1m\n", d->owl_second_attack_point);
  else
    gprintf("no second owl attack point\n");

  if (d->owl_defense_point != NO_MOVE)
    gprintf("owl defense point %1m, code %d, ",
	    d->owl_defense_point, d->owl_defense_code);
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
