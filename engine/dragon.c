/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * This is GNU GO, a Go program. Contact gnugo@gnu.org, or see       *
 * http://www.gnu.org/software/gnugo/ for more information.          *
 *                                                                   *
 * Copyright 1999, 2000, 2001, 2002 and 2003                         *
 * by the Free Software Foundation.                                  *
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

/* A "dragon" is a union of strings of the same color which will be
 * treated as a unit. The dragons are generated anew at each
 * move. If two strings are in the dragon, it is GNU Go's working
 * hypothesis that they will live or die together and are
 * effectively connected.
 *
 *                    _____/|        (! !)
 *                   / ____/|        /@ @)
 *                  / /   __        //  +--oo
 *                 | /   |   >>    /<  _-v--}
 *                 | |   UUU\\\     / / \\
 *                 | |   __ _\\\    \ \  U
 *                 | |  /  V  \\-->  \ \ 
 *                 | <_/           \_/  }
 *                 |      __     ____  /
 *                  \    /  \___/   / /\
 *                  <  \<          < <\ \
 *                   ( )))         ( ))))) 
 */

#include "gnugo.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "liberty.h"
#include "gg_utils.h"

static void initialize_supplementary_dragon_data(void);
static void find_neighbor_dragons(void);
static void add_adjacent_dragons(int a, int b);
static void add_adjacent_dragon(int a, int b);
static int dragon_invincible(int pos);
static int dragon_looks_inessential(int origin);
static void analyze_false_eye_territory(void);
static int connected_to_eye(int pos, int str, int color, int eye_color,
			    struct eye_data *eye);
static void connected_to_eye_recurse(int pos, int str, int color,
				     int eye_color, struct eye_data *eye,
				     char *mx, char *me, int *halfeyes);
static int compute_crude_status(int pos);
static void dragon_eye(int pos, struct eye_data[BOARDMAX]);
static int compute_escape(int pos, int dragon_status_known);
static void compute_surrounding_moyo_sizes(const struct influence_data *q);

static int dragon2_initialized;
static int lively_white_dragons;
static int lively_black_dragons;

/* This is a private array to obtain a list of worms belonging to each
 * dragon. Public access is via first_worm_in_dragon() and
 * next_worm_in_dragon().
 */
static int next_worm_list[BOARDMAX];

/* Alternative for DRAGON2 macro with asserts. */
struct dragon_data2 *
dragon2_func(int pos)
{
  ASSERT1(ON_BOARD1(pos)
          && dragon[pos].id >= 0 
          && dragon[pos].id < number_of_dragons, pos);
  return &dragon2[dragon[pos].id];
}

/* This basic function finds all dragons and collects some basic information
 * about them in the dragon array.
 *
 * color is the player in turn to move. This does in no way affect the
 * information collected about the dragons, but it does affect what
 * information is passed on to the move generation code. If
 * color == EMPTY no information at all is passed on to the move generation.
 */

void 
make_dragons(int color, int stop_before_owl, int save_verbose)
{
  int str;
  int dr;
  int d;
  int last_move;

  start_timer(2);
  dragon2_initialized = 0;
  initialize_dragon_data();

  make_domains(black_eye, white_eye, 0);
  time_report(2, "  time to make domains", NO_MOVE, 1.0);

  /* Find explicit connections patterns in database and amalgamate
   * involved dragons.
   */
  find_connections();
  time_report(2, "  time to find connections", NO_MOVE, 1.0);

  if (!experimental_connections) {
    /* Amalgamate dragons sharing an eyespace (not ko). */

    for (str = BOARDMIN; str < BOARDMAX; str++)
      if (ON_BOARD(str)) {

	if (black_eye[str].color == BLACK_BORDER
	    && black_eye[str].origin == str) {
	  if (!is_ko_point(str)
	      || black_eye[str].esize > 1) /* Only exclude living kos. */
	    dragon_eye(str, black_eye);
	}
	
	if (white_eye[str].color == WHITE_BORDER
	    && white_eye[str].origin == str) {
	  if (!is_ko_point(str)
	      || white_eye[str].esize > 1) /* Only exclude living kos. */
	    dragon_eye(str, white_eye);
	}
      }
    time_report(2, "  time to amalgamate dragons", NO_MOVE, 1.0);
  }

  /* At this time, all dragons have been finalized and we can
   * initialize the dragon2[] array. After that we can no longer allow
   * amalgamation of dragons.
   */
  initialize_supplementary_dragon_data();
  
  /* Find adjacent worms which can be easily captured: */
  
  for (str = BOARDMIN; str < BOARDMAX; str++)
    if (ON_BOARD(str)) {
      int food;

      if (worm[str].origin != str
	  || board[str] == EMPTY
	  || worm[str].lunch == NO_MOVE)
	continue;

      food = worm[str].lunch;

      /* In contrast to worm lunches, a dragon lunch must also be
       * able to defend itself. 
       */
      if (worm[food].defense_codes[0] == 0)
	continue;

      /* Tell the move generation code about the lunch. */
      if (IS_STONE(color))
	add_lunch(str, food);
	
      /* If several lunches are found, we pick the juiciest.
       * First maximize cutstone, then minimize liberties.
       */
      {
	int origin = dragon[str].origin;
	int lunch = DRAGON2(origin).lunch;

	if (lunch == NO_MOVE
	    || worm[food].cutstone > worm[lunch].cutstone
	    || (worm[food].cutstone == worm[lunch].cutstone
		&& (worm[food].liberties < worm[lunch].liberties))) {
	  DRAGON2(origin).lunch = worm[food].origin;
	  TRACE("at %1m setting %1m.lunch to %1m (cutstone=%d)\n",
		str, origin,
		worm[food].origin, worm[food].cutstone);
	}
      }
    }

  /* Find topological half eyes and false eyes. */
  find_half_and_false_eyes(BLACK, black_eye, half_eye, NULL);
  find_half_and_false_eyes(WHITE, white_eye, half_eye, NULL);

  /* Pattern based modification of the eye shapes computed by
   * make_domains and halfeye analysis.
   */
  modify_eye_spaces();
  
  /* Compute the number of eyes, half eyes, etc. in an eye space. */
  for (str = BOARDMIN; str < BOARDMAX; str++) {
    if (!ON_BOARD(str))
      continue;

    if (black_eye[str].color == BLACK_BORDER
	&& black_eye[str].origin == str) {
      struct eyevalue value;
      int attack_point, defense_point;
      
      compute_eyes(str, &value, &attack_point, &defense_point, 
		   black_eye, half_eye, 1, color);
      TRACE_EYES("Black eyespace at %1m: %s\n", str,
	    eyevalue_to_string(&value));
      black_eye[str].value = value;
      black_eye[str].attack_point = attack_point;
      black_eye[str].defense_point = defense_point;
      propagate_eye(str, black_eye);
    }
    
    if (white_eye[str].color == WHITE_BORDER
	&& white_eye[str].origin == str) {
      struct eyevalue value;
      int attack_point, defense_point;
      
      compute_eyes(str, &value, &attack_point, &defense_point,
		   white_eye, half_eye, 1, color);
      TRACE_EYES("White eyespace at %1m: %s\n", str,
	    eyevalue_to_string(&value));
      white_eye[str].value = value;
      white_eye[str].attack_point = attack_point;
      white_eye[str].defense_point = defense_point;
      propagate_eye(str, white_eye);
    }
  }

  /* Try to determine whether topologically false and half eye points
   * contribute to territory even if the eye doesn't solidify.
   */
  analyze_false_eye_territory();

  /* Now we compute the genus. */
  for (str = BOARDMIN; str < BOARDMAX; str++) {
    if (!ON_BOARD(str))
      continue;
    
    if (black_eye[str].color == BLACK_BORDER
	&& black_eye[str].origin == str
	&& find_eye_dragons(black_eye[str].origin, black_eye,
			    BLACK, &dr, 1) == 1) {
      
      gg_assert(board[dr] == BLACK);
      TRACE("eye at %1m found for dragon at %1m--augmenting genus\n",
	    str, dr);
      if (eye_move_urgency(&black_eye[str].value)
	  > eye_move_urgency(&DRAGON2(dr).genus))
	DRAGON2(dr).heye = black_eye[str].defense_point;
      add_eyevalues(&DRAGON2(dr).genus, &black_eye[str].value,
		    &DRAGON2(dr).genus);
    }
    
    if (white_eye[str].color == WHITE_BORDER
	&& white_eye[str].origin == str
	&& find_eye_dragons(white_eye[str].origin, white_eye,
			    WHITE, &dr, 1) == 1) {
      
      gg_assert(board[dr] == WHITE);
      TRACE("eye at %1m found for dragon at %1m--augmenting genus\n",
	    str, dr);
      if (eye_move_urgency(&white_eye[str].value)
	  > eye_move_urgency(&DRAGON2(dr).genus))
	DRAGON2(dr).heye = white_eye[str].defense_point;
      add_eyevalues(&DRAGON2(dr).genus, &white_eye[str].value,
		    &DRAGON2(dr).genus);
    }
  }

  /* Compute the escape route measure. */
  for (str = BOARDMIN; str < BOARDMAX; str++)
    if (IS_STONE(board[str]) && dragon[str].origin == str)
      DRAGON2(str).escape_route = compute_escape(str, 0);

  /* Update the segmentation of the initial influence before we
   * compute the surrounding moyo sizes. The reason for this is that
   * now the eyespace inhibition found by find_cuts() can be taken
   * into account.
   */
  resegment_initial_influence();

  /* Set dragon weaknesses according to initial_influence. */
  compute_refined_dragon_weaknesses();
  for (d = 0; d < number_of_dragons; d++)
    dragon2[d].weakness_pre_owl = dragon2[d].weakness;

  /* Determine status: ALIVE, DEAD, CRITICAL or UNKNOWN */
  for (str = BOARDMIN; str < BOARDMAX; str++)
    if (ON_BOARD(str))
      if (dragon[str].origin == str && board[str]) {
	dragon[str].crude_status = compute_crude_status(str);
      }
  
  /* We must update the dragon status at every intersection before we
   * call the owl code. This updates all fields.
   */
  for (str = BOARDMIN; str < BOARDMAX; str++)
    if (ON_BOARD(str)) {
      struct dragon_data *dd = &(dragon[str]);
      
      dragon[str] = dragon[dd->origin];
    }
  
  find_neighbor_dragons();
  time_report(2, "  pre-owl dragon data", NO_MOVE, 1.0);
  
  if (stop_before_owl)
    return;
  
  /* Determine life and death status of each dragon using the owl code
   * if necessary.
   */
  purge_persistent_owl_cache();
  for (str = BOARDMIN; str < BOARDMAX; str++)
    if (ON_BOARD(str)) {
      int attack_point = NO_MOVE;
      int defense_point = NO_MOVE;
      struct eyevalue no_eyes;
      set_eyevalue(&no_eyes, 0, 0, 0, 0);
      
      if (board[str] == EMPTY
	  || dragon[str].origin != str)
	continue;
      
      /* Some dragons can be ignored but be extra careful with big dragons. */
      if (crude_dragon_weakness(ALIVE, &no_eyes, 0,
	    			DRAGON2(str).moyo_territorial_value,
				DRAGON2(str).escape_route - 10)
	  < 0.00001 + gg_max(0.12, 0.32 - 0.01*dragon[str].effective_size)) {
	dragon[str].owl_status = UNCHECKED;
	dragon[str].owl_attack_point  = NO_MOVE;
	dragon[str].owl_defense_point = NO_MOVE;
      }
      else {
	int acode = 0;
	int dcode = 0;
	int kworm = NO_MOVE;
	start_timer(3);
	acode = owl_attack(str, &attack_point, 
			   &dragon[str].owl_attack_certain, &kworm);
	if (acode != 0) {
	  dragon[str].owl_attack_point = attack_point;
	  dragon[str].owl_attack_code = acode;
	  dragon[str].owl_attack_kworm = kworm;
	  if (attack_point != NO_MOVE) {
	    kworm = NO_MOVE;
	    dcode = owl_defend(str, &defense_point,
			       &dragon[str].owl_defense_certain, &kworm);
	    if (dcode != 0) {
	      if (defense_point != NO_MOVE) {
		dragon[str].owl_status = acode==GAIN ? ALIVE : CRITICAL;
		dragon[str].owl_defense_point = defense_point;
		dragon[str].owl_defense_code = dcode;
		dragon[str].owl_defense_kworm = kworm;
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
		dragon[str].owl_status = acode==GAIN ? ALIVE : CRITICAL;
		TRACE_OWL_PERFORMANCE(
		      "Inconsistent owl attack and defense results for %1m.\n", 
		      str);
		/* Let's see whether the attacking move might be the right
		 * defense:
		 */
		dcode = owl_does_defend(dragon[str].owl_attack_point,
					str, NULL);
		if (dcode != 0) {
		  dragon[str].owl_defense_point
		    = dragon[str].owl_attack_point;
		  dragon[str].owl_defense_code = dcode;
		}
	      }
	    }
	  }
	  if (dcode == 0) {
	    dragon[str].owl_status = DEAD; 
	    dragon[str].owl_defense_point = NO_MOVE;
	    dragon[str].owl_defense_code = 0;
	  }
	}
	else {
	  if (!dragon[str].owl_attack_certain) {
	    kworm = NO_MOVE;
	    dcode = owl_defend(str, &defense_point, 
			       &dragon[str].owl_defense_certain, &kworm);
	    if (dcode != 0) {
	      /* If the result of owl_attack was not certain, we may
	       * still want the result of owl_defend */
	      dragon[str].owl_defense_point = defense_point;
	      dragon[str].owl_defense_code = dcode;
	      dragon[str].owl_defense_kworm = kworm;
	    }
	  }
	  dragon[str].owl_status = ALIVE;
	  dragon[str].owl_attack_point = NO_MOVE;
	  dragon[str].owl_attack_code = 0;
	  
	  time_report(3, "    owl reading for dragon at ", str, 1.0);
	}
      }
    }
  time_report(2, "  owl reading", NO_MOVE, 1.0);
  
  /* Compute the status to be used by the matcher. We most trust the
   * owl status, if it is available.
   */
  for (str = BOARDMIN; str < BOARDMAX; str++)
    if (ON_BOARD(str)) {
      if (IS_STONE(board[str])) {
	if (dragon[str].owl_status != UNCHECKED)
	  dragon[str].status = dragon[str].owl_status;
	else if (dragon[str].crude_status == DEAD 
		 || dragon[str].crude_status == CRITICAL) {
	  /* If a dragon has sufficient escape potential or
	   * surrounding moyo to stop the owl code from being run, the
	   * status should be no worse than UNKNOWN,
	   * regardless what the static life and death analysis
	   * guesses.
	   */
	  dragon[str].status = UNKNOWN;
	}
	else {
	  /* And if the static life and death analysis said UNKNOWN,
           * we are most likely ALIVE.
	   */
	  dragon[str].status = ALIVE;
	}
      }
    }
  time_report(2, "  compute status", NO_MOVE, 1.0);

  /* The dragon data is now correct at the origin of each dragon but
   * we need to copy it to every vertex.  
   */
  for (str = BOARDMIN; str < BOARDMAX; str++)
    if (ON_BOARD(str)) {
      struct dragon_data *dd = &(dragon[str]);
      dragon[str] = dragon[dd->origin];
    }

  /* If the opponent's last move is a dead dragon, this is
   * called a *thrashing dragon*. We must be careful because
   * the opponent may be trying to trick us, so even though
   * GNU Go thinks the stone is dead, we should consider
   * attacking it, particularly if we are ahead.
   */

  last_move = get_last_move();
  if (last_move != NO_MOVE
      && dragon[last_move].status == DEAD) {
    thrashing_dragon = dragon[last_move].origin;
    if (save_verbose)
      gprintf("thrashing dragon found at %1m\n", thrashing_dragon);
  }
  else
    thrashing_dragon = 0;

  /* Owl threats. */
  for (str = BOARDMIN; str < BOARDMAX; str++)
    if (ON_BOARD(str) 
	&& board[str] != EMPTY 
	&& dragon[str].origin == str) {
      struct eyevalue no_eyes;
      set_eyevalue(&no_eyes, 0, 0, 0, 0);
      if (crude_dragon_weakness(ALIVE, &no_eyes, 0,
	    			DRAGON2(str).moyo_territorial_value,
				DRAGON2(str).escape_route - 10)
	  < 0.00001 + gg_max(0.12, 0.32 - 0.01*dragon[str].effective_size)) {
	dragon[str].owl_threat_status = UNCHECKED;
	dragon[str].owl_second_attack_point  = NO_MOVE;
	dragon[str].owl_second_defense_point = NO_MOVE;
      }
      else {
	int acode = dragon[str].owl_attack_code;
	int dcode = dragon[str].owl_defense_code;
	int defense_point, second_defense_point;
	
	if (level >= 8
	    && !disable_threat_computation
	    && (owl_threats 
		|| (thrashing_dragon
		    && is_same_dragon(str, thrashing_dragon)))) {
	  if (acode && !dcode && dragon[str].owl_attack_point != NO_MOVE) {
	    if (owl_threaten_defense(str, &defense_point,
				     &second_defense_point)) {
	      dragon[str].owl_threat_status = CAN_THREATEN_DEFENSE;
	      dragon[str].owl_defense_point = defense_point;
	      dragon[str].owl_second_defense_point = second_defense_point;
	    }
	    else
	      dragon[str].owl_threat_status = DEAD;
	  }
	  else if (!acode) {
	    int attack_point, second_attack_point;
	    if (owl_threaten_attack(str, 
				    &attack_point, &second_attack_point)) {
	      dragon[str].owl_threat_status = CAN_THREATEN_ATTACK;
	      dragon[str].owl_attack_point = attack_point;
	      dragon[str].owl_second_attack_point = second_attack_point;
	    }
	    else
	      dragon[str].owl_threat_status = ALIVE;
	  }
	}
	time_report(3, "    owl threats for dragon at ", str, 1.0);
      }
    }
  
  /* Once again, the dragon data is now correct at the origin of each dragon
   * but we need to copy it to every vertex.  
   */
  for (str = BOARDMIN; str < BOARDMAX; str++)
    if (ON_BOARD(str)) {
      struct dragon_data *dd = &(dragon[str]);
      dragon[str] = dragon[dd->origin];
    }

  time_report(2, "  owl threats ", NO_MOVE, 1.0);
  
  for (d = 0; d < number_of_dragons; d++) {
    dragon2[d].surround_status 
      = compute_surroundings(dragon2[d].origin, NO_MOVE, 0,
			     &(dragon2[d].surround_size));
    if (debug & DEBUG_DRAGONS) {
      if (dragon2[d].surround_status == 1)
	gprintf ("surrounded dragon found at %1m\n", dragon2[d].origin);
      if (dragon2[d].surround_status == 2)
	gprintf ("weakly surrounded dragon found at %1m\n", dragon2[d].origin);
    }
  }


  /* Compute the safety value. */
  for (d = 0; d < number_of_dragons; d++) {
    int true_genus;
    int origin = dragon2[d].origin;
    struct eyevalue *genus = &dragon2[d].genus;

    /* FIXME: We lose information when constructing true_genus. This
     * code can be improved.
     */
    true_genus = max_eyes(genus) + min_eyes(genus);
    if (dragon_looks_inessential(origin))
      dragon2[d].safety = INESSENTIAL;
    else if (dragon[origin].size == worm[origin].size
	     && worm[origin].attack_codes[0] != 0
	     && worm[origin].defense_codes[0] == 0)
      dragon2[d].safety = TACTICALLY_DEAD;
    else if (0) /* Seki is detected by the call to semeai() below. */
      dragon2[d].safety = ALIVE_IN_SEKI;
    else if (dragon[origin].owl_status == DEAD)
      dragon2[d].safety = DEAD;
    else if (dragon[origin].owl_status == CRITICAL)
      dragon2[d].safety = CRITICAL;
    else if (dragon_invincible(origin))
      dragon2[d].safety = INVINCIBLE;
    else if (true_genus >= 6 || dragon2[d].moyo_size > 20)
      dragon2[d].safety = STRONGLY_ALIVE;
    else
      dragon2[d].safety = ALIVE;
  }
  time_report(2, "  post owl dragon data", NO_MOVE, 1.0);

  /* Resolve semeais. This may revise the safety and status fields. */
  if (experimental_semeai && level >= 8)
    new_semeai(color);
  else 
    semeai(color);

  time_report(2, "  semeai module", NO_MOVE, 1.0);

  /* The status is now correct at the origin of each dragon
   * but we need to copy it to every vertex.
   */
  for (str = BOARDMIN; str < BOARDMAX; str++)
    if (ON_BOARD(str))
      dragon[str].status = dragon[dragon[str].origin].status;

  /* Revise essentiality of critical worms. Specifically, a critical
   * worm which is adjacent to no enemy dragon with status
   * better than DEAD, is considered INESSENTIAL.
   *
   * A typical case of this is
   *
   * |.XXXX
   * |.OO.X
   * |X.O.X
   * |.OO.X
   * +-----
   *
   * However, to be able to deal with the position
   *
   * |.XXXX
   * |.OOOO
   * |..O.O
   * |X.OOO
   * |..O.O
   * +-----
   *
   * we need to extend "adjacent" to "adjacent or shares a liberty",
   * which is why we use extended_chainlinks() rather than
   * chainlinks().
   *
   * Finally, if the position above is slightly modified to
   *
   * |.XXXXX
   * |.OOOOO
   * |...O.O
   * |X..OOO
   * |...O.O
   * +------
   *
   * we have a position where the critical X stone doesn't share a
   * liberty with any string at all. Thus the revised rule is:
   *
   * A critical worm which is adjacent to or share a liberty with at
   * least one dead opponent dragon and no opponent dragon which is
   * not dead, is considered inessential.
   */

  for (str = BOARDMIN; str < BOARDMAX; str++)
    if (ON_BOARD(str)) {
      if (is_worm_origin(str, str)
	  && worm[str].attack_codes[0] != 0
	  && worm[str].defense_codes[0] != 0
	  && !worm[str].inessential) {
	int adjs[MAXCHAIN];
	int neighbors;
	int r;
	int essential = 0;
	
	neighbors = extended_chainlinks(str, adjs, 0);
	for (r = 0; r < neighbors; r++)
	  if (dragon[adjs[r]].status != DEAD) {
	    essential = 1;
	    break;
	  }

	if (!essential && neighbors > 0) {
	  TRACE_WORMS("Worm %1m revised to be inessential.\n", str);
	  worm[str].inessential = 1;
	  propagate_worm(str);
	}
      }
    }
  time_report(2, "  revise worm inessentiality", NO_MOVE, 1.0);

  /* Revise essentiality of critical dragons. Specifically, a critical
   * dragon consisting entirely of inessential worms is considered
   * INESSENTIAL.
   */
  for (str = BOARDMIN; str < BOARDMAX; str++) {
    if (ON_BOARD(str)
	&& board[str] != EMPTY
	&& dragon[str].origin == str
	&& DRAGON2(str).safety == CRITICAL) {
      int w;
      for (w = first_worm_in_dragon(str); w != NO_MOVE;
	   w = next_worm_in_dragon(w)) {
	if (!worm[w].inessential)
	  break;
      }

      if (w == NO_MOVE) {
	TRACE_DRAGONS("Dragon %1m revised to be inessential.\n", str);
	DRAGON2(str).safety = INESSENTIAL;
      }
    }
  }
  time_report(2, "  revise dragon inessentiality", NO_MOVE, 1.0);

  /* Count the non-dead dragons. */
  lively_white_dragons = 0;
  lively_black_dragons = 0;
  for (d = 0; d < number_of_dragons; d++)
    if (DRAGON(d).crude_status != DEAD) {
      if (DRAGON(d).color == WHITE)
	lively_white_dragons++;
      else
	lively_black_dragons++;
    }
}


/* Initialize the dragon[] array. */

void
initialize_dragon_data(void)
{
  int str;
  for (str = BOARDMIN; str < BOARDMAX; str++)
    if (ON_BOARD(str)) {

      dragon[str].id                 = -1;
      dragon[str].size               = worm[str].size;
      dragon[str].effective_size     = worm[str].effective_size;
      dragon[str].color              = worm[str].color;
      dragon[str].origin             = worm[str].origin;
      dragon[str].owl_attack_point   = NO_MOVE;
      dragon[str].owl_attack_code    = 0;
      dragon[str].owl_attack_certain = 1;
      dragon[str].owl_defense_point  = NO_MOVE;
      dragon[str].owl_defense_code   = 0;
      dragon[str].owl_defense_certain = 1;
      dragon[str].owl_status         = UNCHECKED;
      dragon[str].crude_status       = UNKNOWN;
      dragon[str].status     = UNKNOWN;
      dragon[str].owl_threat_status  = UNCHECKED;
      dragon[str].owl_second_attack_point  = NO_MOVE;
      dragon[str].owl_second_defense_point = NO_MOVE;
      half_eye[str].type             =  0;
      half_eye[str].value            =  10.0; /* Something big. */
      
      if (IS_STONE(board[str]) && worm[str].origin == str)
	TRACE_DRAGONS(
	      "Initializing dragon from worm at %1m, size %d\n", 
	      str, worm[str].size);
    }
  memset(next_worm_list, 0, sizeof(next_worm_list));
}


/* Initialize the dragon2[] array. */
static void
initialize_supplementary_dragon_data(void)
{
  int str;
  int d;
  int origin;
  
  /* Give each dragon (caves excluded) an id number for indexing into
   * the dragon2 array. After this the DRAGON2 macro can be used.
   */
  number_of_dragons = 0;
  for (str = BOARDMIN; str < BOARDMAX; str++) {
    if (!ON_BOARD(str))
      continue;
    origin = dragon[str].origin;
    
    if (board[str] == EMPTY)
      continue;
    
    if (dragon[origin].id == -1)
      dragon[origin].id = number_of_dragons++;
    dragon[str].id = dragon[origin].id;
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
  for (str = BOARDMIN; str < BOARDMAX; str++) {
    if (!ON_BOARD(str))
      continue;
    if (IS_STONE(board[str])
	&& dragon[str].origin == str) {
      DRAGON2(str).origin = str;
    }
  }
  
  /* Initialize the rest of the dragon2 data. */
  for (d = 0; d < number_of_dragons; d++) {
    dragon2[d].neighbors               = 0;
    dragon2[d].hostile_neighbors       = 0;
    dragon2[d].moyo_size	       = -1;
    dragon2[d].moyo_territorial_value  = 0.0;
    dragon2[d].safety                  = -1;
    dragon2[d].escape_route            = 0;
    dragon2[d].heye                    = NO_MOVE;
    dragon2[d].lunch                   = NO_MOVE;
    dragon2[d].semeai                  = 0;
    dragon2[d].semeai_margin_of_safety = -1;
    dragon2[d].surround_status         = 0;
    set_eyevalue(&dragon2[d].genus, 0, 0, 0, 0);
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
  int pos;
  int pos2;
  int i, j;
  int d;
  int dragons[BOARDMAX];
  int distances[BOARDMAX];
  int dist;
  int k;
  int color;

  gg_assert(dragon2_initialized);
  
  /* Initialize the arrays. */
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (IS_STONE(board[pos])) {
      dragons[pos] = dragon[pos].id;
      distances[pos] = 0;
    }
    else if (ON_BOARD(pos)) {
      dragons[pos] = -1;
      distances[pos] = -1;
    }
  }

  /* Expand from dist-1 to dist. Break out of the loop at the end if
     * we couldn't expand anything. Never expand more than five steps.
     */
  for (dist = 1; dist <= 5; dist++) {
    int found_one = 0;
      
    for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
      if (!ON_BOARD(pos))
	continue;
    
      if (distances[pos] != dist-1 || dragons[pos] < 0)
	continue;
      
      color = DRAGON(dragons[pos]).color;
      for (k = 0; k < 4; k++) {
	pos2 = pos + delta[k];
	
	if (!ON_BOARD1(pos2))
	  continue;
	
	/* Consider expansion from (pos) to adjacent intersection
	 * (pos2).
	 */
	if (distances[pos2] >= 0 && distances[pos2] < dist)
	  continue; /* (pos2) already occupied. */

	/* We can always expand the first step, regardless of influence. */
	if (dist == 1
	    || (whose_area(INITIAL_INFLUENCE(color), pos) == color
		&& whose_area(INITIAL_INFLUENCE(color), pos2)
		   != OTHER_COLOR(color))) {
	  /* Expansion ok. Now see if someone else has tried to
	   * expand here. In that case we indicate a collision by
	   * setting the dragon number to -2.
	   */
	  if (distances[pos2] == dist) {
	    if (dragons[pos2] != dragons[pos])
	      dragons[pos2] = -2;
	  }
	  else {
	    dragons[pos2] = dragons[pos];
	    distances[pos2] = dist;
	    found_one = 1;
	  }
	}
      }
      if (!found_one)
	break;
    }
  }
  
  if (0) {
    for (m = 0; m < board_size; m++) {
      for (n = 0; n < board_size; n++)
	fprintf(stderr, "%3d", dragons[POS(m, n)]);
      fprintf(stderr, "\n");
    }
    fprintf(stderr, "\n");
      
    for (m = 0; m < board_size; m++) {
      for (n = 0; n < board_size; n++)
	fprintf(stderr, "%3d", distances[POS(m, n)]);
      fprintf(stderr, "\n");
    }
    fprintf(stderr, "\n");
  }

  /* Now go through dragons to find neighbors. It suffices to look
   * south and east for neighbors. In the case of a collision zone
   * where dragons==-2 we set all the neighbors of this intersection
   * as adjacent to each other.
   */
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (!ON_BOARD(pos))
      continue;
    if (dragons[pos] == -2) {
      int neighbors = 0;
      int adjacent[4];

      for (k = 0; k < 4; k++) {
	pos2 = pos + delta[k];

	if (ON_BOARD1(pos2) && dragons[pos2] >= 0)
	  adjacent[neighbors++] = dragons[pos2];
      }
      for (i = 0; i < neighbors; i++)
	for (j = i+1; j < neighbors; j++)
	  add_adjacent_dragons(adjacent[i], adjacent[j]);
    }
    else if (dragons[pos] >= 0) {
      if (ON_BOARD(NORTH(pos))) {
	if (dragons[NORTH(pos)] >= 0
	    && dragons[NORTH(pos)] != dragons[pos])
	  add_adjacent_dragons(dragons[pos], dragons[NORTH(pos)]);
      }
      if (ON_BOARD(EAST(pos))) {
	if (dragons[EAST(pos)] >= 0
	    && dragons[EAST(pos)] != dragons[pos])
	  add_adjacent_dragons(dragons[pos], dragons[EAST(pos)]);
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
  gg_assert(a >= 0 
	    && a < number_of_dragons && b >= 0 && b < number_of_dragons);
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
  gg_assert(a >= 0 
	    && a < number_of_dragons && b >= 0 && b < number_of_dragons);
  /* If the array of adjacent dragons already is full, ignore
   * additional neighbors.
   */
  if (dragon2[a].neighbors == MAX_NEIGHBOR_DRAGONS)
    return;
  
  for (i = 0; i < dragon2[a].neighbors; i++)
    if (dragon2[a].adjacent[i] == b)
      return;

  dragon2[a].adjacent[dragon2[a].neighbors++] = b;
  if (DRAGON(a).color == OTHER_COLOR(DRAGON(b).color))
    dragon2[a].hostile_neighbors++;
}

/* A dragon is considered invincible if it satisfies either of the two
 * following conditions:
 * a) At least two distinct eyespaces without topological halfeyes or
 * marginal vertices.
 * b) At least one string which is unconditionally alive according to the
 * unconditional_life() function in utils.c.
 */

static int
dragon_invincible(int dr)
{
  struct eye_data *eye;
  int eye_color;
  int k;
  int pos;
  int strong_eyes = 0;
  int mx[BOARDMAX];
  
  gg_assert(IS_STONE(board[dr]));

  /* First look for invincible strings in the dragon. */
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (ON_BOARD(pos) && is_same_dragon(pos, dr) && worm[pos].invincible)
      return 1;
  }

  /* Examine the eye spaces.
   * FIXME: The check for half eyes or false eyes may be too weak.
   */
  if (board[dr] == BLACK) {
    eye = black_eye;
    eye_color = BLACK_BORDER;
  }
  else {
    eye = white_eye;
    eye_color = WHITE_BORDER;
  }

  memset(mx, 0, sizeof(mx));

  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (board[pos] == board[dr] && is_same_dragon(pos, dr)) {
      for (k = 0; k < 4; k++) {
	int pos2 = pos + delta[k];
	if (ON_BOARD(pos2)
	    && eye[pos2].color == eye_color
	    && eye[pos2].origin != NO_MOVE
	    && !eye[pos2].marginal)
	  mx[eye[pos2].origin] = 1;
      }
    }
  }

  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (mx[pos]
	&& eye[pos].msize == 0
	&& min_eyes(&eye[pos].value) > 0)
      strong_eyes++;
  }

  if (strong_eyes >= 2)
    return 1;

  return 0;
}


/* A dragon looks inessential if it satisfies all of
 * 1. Is a single string.
 * 2. Is not owl substantial.
 *
 * FIXME: Probably need a better definition of INESSENTIAL dragons.
 *        There are cases where a string is owl insubstantial
 *        yet allowing it to be captured greatly weakens our
 *        position.
 */
static int
dragon_looks_inessential(int origin)
{
  if (dragon[origin].size != worm[origin].size)
    return 0;

  if (owl_substantial(origin))
    return 0;

  return 1;
}


/* Report which stones are alive if it's (color)'s turn to move. I.e.
 * critical stones belonging to (color) are considered alive.
 * A stone is dead resp. critical if the tactical reading code _or_ the
 * owl code thinks so.
 */
static void
get_alive_stones(int color, char safe_stones[BOARDMAX])
{
  int d;
  int ii;
  get_lively_stones(color, safe_stones);
  for (d = 0; d < number_of_dragons; d++) {
    if (dragon2[d].safety == DEAD
	|| (dragon2[d].safety == CRITICAL
	    && board[dragon2[d].origin] == OTHER_COLOR(color))) {
      for (ii = first_worm_in_dragon(dragon2[d].origin); ii != NO_MOVE;
	   ii = next_worm_in_dragon(ii))
	mark_string(ii, safe_stones, 0);
    }
  }
}

static void
set_dragon_strengths(const char safe_stones[BOARDMAX],
    		     float strength[BOARDMAX])
{
  int ii;
  for (ii = BOARDMIN; ii < BOARDMAX; ii++)
    if (ON_BOARD(ii)) {
      if (safe_stones[ii])  {
	ASSERT1(IS_STONE(board[ii]), ii);
	strength[ii] = DEFAULT_STRENGTH
	  	       * (1.0 - 0.3 * DRAGON2(ii).weakness_pre_owl);
      }
      else
	strength[ii] = 0.0;
    }
}

/* Marks all inessential stones with INFLUENCE_SAVE_STONE, leaves
 * everything else unchanged.
 */
static void
mark_inessential_stones(int color, char safe_stones[BOARDMAX])
{
  int ii;
  for (ii = BOARDMIN; ii < BOARDMAX; ii++)
    if (IS_STONE(board[ii])
	&& (DRAGON2(ii).safety == INESSENTIAL
	    || (worm[ii].inessential
	        /* FIXME: Why is the check below needed?
		 * Why does it use .safety, not .status? /ab
		 */
		&& ((DRAGON2(ii).safety != DEAD
		     && DRAGON2(ii).safety != TACTICALLY_DEAD
		     && DRAGON2(ii).safety != CRITICAL)
		    || (DRAGON2(ii).safety == CRITICAL
			&& board[ii] == color)))) )
      safe_stones[ii] = INFLUENCE_SAFE_STONE;
}

void
set_strength_data(int color, char safe_stones[BOARDMAX],
    		  float strength[BOARDMAX])
{
  gg_assert(IS_STONE(color) || color == EMPTY);

  get_alive_stones(color, safe_stones);
  set_dragon_strengths(safe_stones, strength);
  mark_inessential_stones(color, safe_stones);
}


void
compute_dragon_influence()
{
  char safe_stones[BOARDMAX];
  float strength[BOARDMAX];

  set_strength_data(BLACK, safe_stones, strength);
  compute_influence(BLACK, safe_stones, strength, &initial_black_influence,
                    NO_MOVE, "initial black influence, dragons known");

  set_strength_data(WHITE, safe_stones, strength);
  compute_influence(WHITE, safe_stones, strength, &initial_white_influence,
                    NO_MOVE, "initial white influence, dragons known");
}


/* Try to determine whether topologically false and half eye points
 * contribute to territory even if the eye doesn't solidify. The purpose
 * is to be able to distinguish between, e.g., these positions:
 *
 * |.OOOOO       |.OOOOO
 * |.O.XXO       |.O.OXO
 * |OOX.XO       |OOX.XO
 * |O*XXXO  and  |O*XXXO
 * |OX.XOO       |OX.XOO
 * |X.XOO.       |X.XOO.
 * |.XXO..       |.XXO..
 * +------       +------
 * 
 * In the left one the move at * is a pure dame point while in the
 * right one it is worth one point of territory for either player.
 *
 * In general the question is whether a topologically false eye vertex
 * counts as territory or not and the answer depends on whether each
 * string adjoining the eye is externally connected to at least one
 * proper eye.
 *
 * This function loops over the topologically false and half eye
 * vertices and calls connected_to_eye() for each adjoining string to
 * determine whether they all have external connection to an eye. The
 * result is stored in the array false_eye_territory[] array.
 */
static void
analyze_false_eye_territory(void)
{
  int pos;
  int color;
  int eye_color;
  struct eye_data *eye;
  int k;

  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (!ON_BOARD(pos))
      continue;
    
    false_eye_territory[pos] = 0;

    /* The analysis only applies to false and half eyes. */
    if (half_eye[pos].type == 0)
      continue;

    /* Determine the color of the eye. */
    if (white_eye[pos].color == WHITE_BORDER) {
      color = WHITE;
      eye_color = WHITE_BORDER;
      eye = white_eye;
    }
    else if (black_eye[pos].color == BLACK_BORDER) {
      color = BLACK;
      eye_color = BLACK_BORDER;
      eye = black_eye;
    }
    else
      continue;

    /* Make sure we have a "closed" position. Positions like
     *
     * |XXXXXX.
     * |OOOOOXX
     * |.O.O*..
     * +-------
     *
     * disqualify without further analysis. (* is a false eye vertex)
     */
    for (k = 0; k < 4; k++)
      if (ON_BOARD(pos + delta[k])
	  && board[pos + delta[k]] != color
	  && eye[pos + delta[k]].color != eye_color)
	break;
     
    if (k < 4)
      continue;

    /* Check that all adjoining strings have external connection to an
     * eye.
     */
    for (k = 0; k < 4; k++)
      if (ON_BOARD(pos + delta[k])
	  && board[pos + delta[k]] == color
	  && !connected_to_eye(pos, pos + delta[k], color, eye_color, eye))
	break;

    if (k == 4) {
      false_eye_territory[pos] = 1;
      if (0)
	gprintf("False eye territory at %1m\n", pos);
    }
  }
}

/* 
 * This function (implicitly) finds the connected set of strings of a
 * dragon, starting from (str) which is next to the analyzed halfeye
 * at (pos). Strings are for this purpose considered connected if and
 * only if they have a common liberty, which is not allowed to be the
 * half eye itself or one of its diagonal neighbors. For these strings
 * it is examined whether their liberties are parts of eyespaces worth
 * at least two halfeyes (again not counting the eyespace at (pos)).
 *
 * The real work is done by the recursive function
 * connected_to_eye_recurse() below.
 */
static int
connected_to_eye(int pos, int str, int color, int eye_color,
		 struct eye_data *eye)
{
  char mx[BOARDMAX];
  char me[BOARDMAX];
  int k;
  int halfeyes;

  /* mx marks strings and liberties which have already been investigated.
   * me marks the origins of eyespaces which have already been counted.
   * Start by marking (pos) and the surrounding vertices in mx.
   */
  memset(mx, 0, sizeof(mx));
  memset(me, 0, sizeof(me));
  mx[pos] = 1;
  for (k = 0; k < 8; k++)
    if (ON_BOARD(pos + delta[k]))
      mx[pos + delta[k]] = 1;

  halfeyes = 0;
  connected_to_eye_recurse(pos, str, color, eye_color, eye, mx, me, &halfeyes);

  if (halfeyes >= 2)
    return 1;
  
  return 0;
}

/* Recursive helper for connected_to_eye(). Stop searching when we
 * have found at least two halfeyes.
 */
static void
connected_to_eye_recurse(int pos, int str, int color, int eye_color,
			 struct eye_data *eye, char *mx, char *me,
			 int *halfeyes)
{
  int liberties;
  int libs[MAXLIBS];
  int r;
  int k;

  mark_string(str, mx, 1);
  liberties = findlib(str, MAXLIBS, libs);

  /* Search the liberties of (str) for eyespaces. */
  for (r = 0; r < liberties; r++) {
    if (eye[libs[r]].color == eye_color
	&& libs[r] != pos
	&& !me[eye[libs[r]].origin]) {
      me[eye[libs[r]].origin] = 1;
      *halfeyes += (min_eyes(&eye[libs[r]].value)
		    + max_eyes(&eye[libs[r]].value));
    }
  }

  if (*halfeyes >= 2)
    return;

  /* Search for new strings in the same dragon with a liberty in
   * common with (str), and recurse.
   */
  for (r = 0; r < liberties; r++) {
    if (mx[libs[r]])
      continue;
    mx[libs[r]] = 1;
    for (k = 0; k < 4; k++) {
      if (ON_BOARD(libs[r] + delta[k])
	  && board[libs[r] + delta[k]] == color
	  && is_same_dragon(str, libs[r] + delta[k])
	  && !mx[libs[r] + delta[k]])
	connected_to_eye_recurse(pos, libs[r] + delta[k], color, eye_color,
				 eye, mx, me, halfeyes);
      if (*halfeyes >= 2)
	return;
    }
  }
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
  
  int pos;
  int k;

  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    struct worm_data *w = &(worm[pos]);
    if (!IS_STONE(board[pos]))
      continue;

    if (w->origin == pos) {
      gprintf("%1m : (dragon %1m) %s string of size %d (%f), genus %d: (%d,%d,%d,%d)",
	      pos, dragon[pos].origin,
	      color_to_string(board[pos]),
	      w->size,
	      w->effective_size,
	      w->genus,
	      w->liberties,
	      w->liberties2,
	      w->liberties3,
	      w->liberties4);
      if (w->cutstone == 1)
	gprintf("%o - is a potential cutting stone\n");
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
	if (w->defense_codes[k] == 0)
	  break;
	if (w->defense_codes[k] != 0)
	  gprintf("- defendable at %1m, defend code = %d\n",
		  w->defense_points[k], w->defense_codes[k]);
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
      
      if (is_ko_point(pos))
	gprintf("- is a ko stone\n");
    }
  }
    
  gprintf("%o\n");
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    struct dragon_data *dd = &(dragon[pos]);
    struct dragon_data2 *d2;
    
    if (!IS_STONE(board[pos]))
      continue;
    
    d2 = &(dragon2[dd->id]);
    
    if (dd->origin == pos) {
      gprintf("%1m : %s dragon size %d (%f), genus %s, escape factor %d, crude status %s, status %s, moyo size %d, moyo territory value %f, safety %s, weakness pre owl %f, weakness %f",
	      pos,
	      board[pos] == BLACK ? "B" : "W",
	      dd->size,
	      dd->effective_size,
	      eyevalue_to_string(&d2->genus),
	      d2->escape_route,
	      snames[dd->crude_status],
	      snames[dd->status],
	      d2->moyo_size,
	      d2->moyo_territorial_value,
	      safety_names[d2->safety],
	      d2->weakness_pre_owl,
	      d2->weakness);
      gprintf(", owl status %s\n", snames[dd->owl_status]);
      if (dd->owl_status == CRITICAL) {
	gprintf("... owl attackable at %1m, code %d\n",
		dd->owl_attack_point, dd->owl_attack_code);
	gprintf("... owl defendable at %1m, code %d\n",
		dd->owl_defense_point, dd->owl_defense_code);
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

  TRACE_DRAGONS("amalgamate dragons around %1m\n", pos);
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

  /* If d1 and d2 are the same dragon, we do nothing. */
  if (d1 == d2)
    return;
  
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
    TRACE_DRAGONS("joining dragon at %1m to dragon at %1m\n", d2, d1);
  }
  else {
    origin = d2;
    TRACE_DRAGONS("joining dragon at %1m to dragon at %1m\n", d1, d2);
  }
  
  dragon[origin].size  = dragon[d2].size + dragon[d1].size;
  dragon[origin].effective_size  = (dragon[d2].effective_size
				    + dragon[d1].effective_size);

  /* Join the second next_worm_in_dragon chain at the end of the first one. */
  {
    int last_worm_origin_dragon = origin;
    while (next_worm_list[last_worm_origin_dragon] != NO_MOVE)
      last_worm_origin_dragon = next_worm_list[last_worm_origin_dragon];
    if (origin == d1)
      next_worm_list[last_worm_origin_dragon] = d2;
    else
      next_worm_list[last_worm_origin_dragon] = d1;
  }

  for (ii = BOARDMIN; ii < BOARDMAX; ii++) {
    if (ON_BOARD(ii)
	&& (dragon[ii].origin == d1 || dragon[ii].origin == d2))
      dragon[ii].origin = origin;
  }
}



/*
 * compute_crude_status(pos) tries to determine whether the dragon
 * at (pos) is ALIVE, DEAD, or UNKNOWN. The algorithm is not perfect
 * and can give incorrect answers.
 *
 * The dragon is judged alive if its genus is >1. It is judged dead if
 * the genus is <2, it has no escape route, and no adjoining string can
 * be easily captured. Otherwise it is judged UNKNOWN.  */

static int 
compute_crude_status(int pos)
{
  /* FIXME: We lose information when constructing true_genus. This
   * code can be improved.
   */
  struct eyevalue *genus = &DRAGON2(pos).genus;
  int true_genus = max_eyes(genus) + min_eyes(genus);
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
      && worm[pos].defense_codes[0] == 0
      && true_genus < 3)
    return DEAD;
  
  if (lunch != NO_MOVE
      && true_genus < 3
      && worm[lunch].defense_codes[0] != 0
      && DRAGON2(pos).escape_route < 5)
    if (true_genus == 2 || worm[lunch].size > 2)
      return CRITICAL;

  if (lunch != NO_MOVE
      && true_genus >= 3)
    return ALIVE;

  if (lunch == NO_MOVE || worm[lunch].cutstone < 2) {
    if (true_genus < 3
	&& DRAGON2(pos).escape_route == 0
	&& DRAGON2(pos).moyo_size < 5)
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
	      char escape_value[BOARDMAX])
{
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
  for (ii = BOARDMIN; ii < BOARDMAX; ii++)
    if (ON_BOARD(ii) && goal[ii])
      ENQUEUE(ii);
  
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
	      && !mx[SW(ii)]
	      && (board[SW(ii)] == color
		  || (board[SW(ii)] == EMPTY
		      && ON_BOARD(SOUTH(SW(ii)))
		      && board[SOUTH(SW(ii))] != other
		      && ON_BOARD(WEST(SW(ii)))
		      && board[WEST(SW(ii))] != other)))
	    ENQUEUE(SW(ii));
		      
	  if (board[WEST(ii)] == EMPTY
	      && board[NORTH(ii)] == EMPTY
	      && !mx[NW(ii)]
	      && (board[NW(ii)] == color
		  || (board[NW(ii)] == EMPTY
		      && ON_BOARD(WEST(NW(ii)))
		      && board[WEST(NW(ii))] != other
		      && ON_BOARD(NORTH(NW(ii)))
		      && board[NORTH(NW(ii))] != other)))
	    ENQUEUE(NW(ii));
		      
	  if (board[NORTH(ii)] == EMPTY
	      && board[EAST(ii)] == EMPTY
	      && !mx[NE(ii)]
	      && (board[NE(ii)] == color
		  || (board[NE(ii)] == EMPTY
		      && ON_BOARD(NORTH(NE(ii)))
		      && board[NORTH(NE(ii))] != other
		      && ON_BOARD(EAST(NE(ii)))
		      && board[EAST(NE(ii))] != other)))
	    ENQUEUE(NE(ii));
		      
	  if (board[EAST(ii)] == EMPTY
	      && board[SOUTH(ii)] == EMPTY
	      && !mx[SE(ii)]
	      && (board[SE(ii)] == color
		  || (board[SE(ii)] == EMPTY
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
  for (k = 0; k < queue_end; k++) {
    /* The assertion fails if the same element should have been queued
     * twice, which might happen if ENQUEUE() is called without
     * checking mx[].
     */
    ASSERT1(mx[queue[k]] == 1, queue[k]);
    mx[queue[k]] = 0;
  }

  return escape_potential;
}

/* Wrapper to call the function above and compute the escape potential
 * for the dragon at (pos).
 */
static int
compute_escape(int pos, int dragon_status_known)
{
  int ii;
  char goal[BOARDMAX];
  char escape_value[BOARDMAX];
  char safe_stones[BOARDMAX];

  ASSERT1(IS_STONE(board[pos]), pos);

  for (ii = BOARDMIN; ii < BOARDMAX; ii++)
    if (ON_BOARD(ii))
      goal[ii] = is_same_dragon(ii, pos);

  /* Compute escape_value array.  Points are awarded for moyo (4),
   * area (2) or EMPTY (1).  Values may change without notice.
   */
  get_lively_stones(OTHER_COLOR(board[pos]), safe_stones);
  compute_escape_influence(board[pos], safe_stones, 0, escape_value);

  /* If we can reach a live group, award 6 points. */
  for (ii = BOARDMIN; ii < BOARDMAX; ii++) {
    if (!ON_BOARD(ii))
      continue;

    if (dragon_status_known) {
      if (dragon[ii].crude_status == ALIVE)
	escape_value[ii] = 6;
      else if (dragon[ii].crude_status == UNKNOWN
	       && (DRAGON2(ii).escape_route > 5
		   || DRAGON2(ii).moyo_size  > 5))
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
 * Sum up the surrounding moyo sizes for each dragon. For this
 * we retrieve the moyo data stored in influence_data (*q) (which must
 * have been computed previously) from the influence module.
 * We set dragon2[].moyo_size and .moyo_value if it is smaller than the 
 * current entry.
 *
 * Currently this is implemented differently depending on whether
 * experimental connections are used or not. The reason why this is
 * needed is that most of the B patterns in conn.db are disabled for
 * experimental connections, which may cause the moyo segmentation to
 * pass through cutting points between dragons, making the surrounding
 * moyo size mostly useless. Instead we only use the part of the
 * surrounding moyo which is closest to some worm of the dragon.
 */
static void
compute_surrounding_moyo_sizes(const struct influence_data *q)
{
  int pos;
  int d;

  if (!experimental_connections) {
    int mx[MAX_MOYOS + 1];
    struct moyo_data moyos;

    influence_get_moyo_segmentation(q, &moyos);

    memset(mx, 0, sizeof(mx));
    for (d = 0; d < number_of_dragons; d++) {
      int this_moyo_size = 0;
      float this_moyo_value = 0.0;
      for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
	int moyo_number = moyos.segmentation[pos];
	if (board[pos] != DRAGON(d).color
	    || moyo_number == 0
	    || dragon[pos].id != d
	    || moyos.owner[moyo_number] != board[pos])
	  continue;
	
	if (mx[moyo_number] != d + 1) {
	  mx[moyo_number] = d + 1;
	  this_moyo_size += moyos.size[moyo_number];
	  this_moyo_value += moyos.territorial_value[moyo_number];
	}
      }
      
      if (this_moyo_size < dragon2[d].moyo_size) {
	dragon2[d].moyo_size = this_moyo_size;
	dragon2[d].moyo_territorial_value = this_moyo_value;
      }
    }
  }
  else {
    int k;
    int moyo_color[BOARDMAX];
    float territory_value[BOARDMAX];
    float moyo_sizes[BOARDMAX];
    float moyo_values[BOARDMAX];
    
    influence_get_moyo_data(q, moyo_color, territory_value);

    for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
      moyo_sizes[pos] = 0.0;
      moyo_values[pos] = 0.0;
    }
    
    for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
      if (!ON_BOARD(pos))
	continue;
      
      if (moyo_color[pos] == board[pos])
	continue;
      
      if (moyo_color[pos] == WHITE) {
	for (k = 0; k < number_close_white_worms[pos]; k++) {
	  int w = close_white_worms[pos][k];
	  int dr = dragon[w].origin;
	  int n = gg_min(number_close_white_worms[pos], 5);
	  
	  moyo_sizes[dr] += 1.0 / n;
	  moyo_values[dr] += gg_min(territory_value[pos], 1.0) / n;
	}
      }
      
      if (moyo_color[pos] == BLACK) {
	for (k = 0; k < number_close_black_worms[pos]; k++) {
	  int w = close_black_worms[pos][k];
	  int dr = dragon[w].origin;
	  int n = gg_min(number_close_black_worms[pos], 5);
	  
	  moyo_sizes[dr] += 1.0 / n;
	  moyo_values[dr] += gg_min(territory_value[pos], 1.0) / n;
	}
      }
    }

    for (d = 0; d < number_of_dragons; d++) {
      /* Limiting the number of close worms to at most 5 above (more
       * is in practice probably very rare) causes the moyo sizes to
       * become multiples of 1/60. In order to avoid problems with
       * varying (between platforms) floating point rounding
       * characteristics, we add 0.01 before converting to int.
       */
      int this_moyo_size = (int) (0.01 + moyo_sizes[dragon2[d].origin]);
      float this_moyo_value = moyo_values[dragon2[d].origin];
      
      if (this_moyo_size < dragon2[d].moyo_size) {
	dragon2[d].moyo_size = this_moyo_size;
	dragon2[d].moyo_territorial_value = this_moyo_value;
      }
    }
  }
}


static struct interpolation_data moyo_value2weakness =
  { 5, 0.0, 15.0, {1.0, 0.65, 0.3, 0.15, 0.05, 0.0}};
static struct interpolation_data escape_route2weakness =
  { 5, 0.0, 25.0, {1.0, 0.6, 0.3, 0.1, 0.05, 0.0}};
static struct interpolation_data genus2weakness =
  { 6, 0.0, 3.0, {1.0, 0.95, 0.8, 0.5, 0.2, 0.1, 0.0}};

float
crude_dragon_weakness(int safety, struct eyevalue *genus, int has_lunch,
    		      float moyo_value, float escape_route)
{
  /* FIXME: We lose information when constructing true_genus. This
   * code can be improved.
   */
  float true_genus = 0.5 * (max_eyes(genus) + min_eyes(genus)
      			    + (has_lunch != 0));
  float weakness_value[3];
  float weakness;
  int i, j;

  if (safety == INVINCIBLE || safety == INESSENTIAL)
    return 0.0;
  if (safety == TACTICALLY_DEAD || safety == DEAD || safety == CRITICAL)
    return 1.0;

  weakness_value[0] = gg_interpolate(&moyo_value2weakness, moyo_value);
  weakness_value[1] = gg_interpolate(&escape_route2weakness, escape_route);
  weakness_value[2] = gg_interpolate(&genus2weakness, true_genus);

  TRACE_DRAGONS(
	"  moyo value %f -> %f, escape %f -> %f, eyes %f -> %f,",
	moyo_value, weakness_value[0],
	escape_route, weakness_value[1],
	true_genus, weakness_value[2]);

  for (i = 0; i < 3; i++)
    for (j = i + 1; j < 3; j++)
      if (weakness_value[j] < weakness_value[i]) {
	float tmp = weakness_value[i];
	weakness_value[i] = weakness_value[j];
	weakness_value[j] = tmp;
      }

  /* The overall weakness is mostly, but not completely determined by the
   * best value found so far:
   */
  weakness = gg_min(0.7 * weakness_value[0] + 0.3 * weakness_value[1],
                    1.3 * weakness_value[0]);

  gg_assert(weakness >= 0.0 && weakness <= 1.0);

  return weakness;
}

/* This function tries to guess a coefficient measuring the weakness of
 * a dragon. This coefficient * the effective size of the dragon can be
 * used to award a strategic penalty for weak dragons.
 */
static float
compute_dragon_weakness_value(int d)
{
  int origin = dragon2[d].origin;
  float weakness;

  /* Possible ingredients for the computation:
   * 	'+' means currently used, '-' means not (yet?) used
   * - pre-owl moyo_size
   * + post-owl moyo_size and its territory value
   * + escape factor
   * + number of eyes
   *   - minus number of vital attack moves?
   * + from owl:
   *   + attack certain?
   *   - number of owl nodes
   *   - maybe reading shadow?
   *   + threat to attack?
   * - possible connections to neighbour dragons
   */

  TRACE_DRAGONS("Computing weakness of dragon at %1m:\n", origin);

  weakness = crude_dragon_weakness(dragon2[d].safety, &dragon2[d].genus,
				   dragon2[d].lunch != NO_MOVE,
      				   dragon2[d].moyo_territorial_value, 
				   (float) dragon2[d].escape_route);

  /* Now corrections due to (uncertain) owl results resp. owl threats. */
  if (!dragon[origin].owl_attack_certain)
    weakness += gg_min(0.25 * (1.0 - weakness), 0.25 * weakness);
  if (!dragon[origin].owl_defense_certain)
    weakness += gg_min(0.25 * (1.0 - weakness), 0.25 * weakness);
  if (dragon[origin].owl_threat_status == CAN_THREATEN_ATTACK)
    weakness += 0.15 * (1.0 - weakness);

  if (weakness < 0.0)
    weakness = 0.0;
  if (weakness > 1.0)
    weakness = 1.0;

  TRACE_DRAGONS(" result: %f.\n", weakness);
  return weakness;
}


/* This function has to be called _after_ the owl analysis and the
 * subsequent re-run of the influence code.
 */
void
compute_refined_dragon_weaknesses()
{
  int d;

  /* Compute the surrounding moyo sizes. */
  for (d = 0; d < number_of_dragons; d++)
    dragon2[d].moyo_size = 2 * BOARDMAX;
  
  /* Set moyo sizes according to initial_influence. */
  compute_surrounding_moyo_sizes(&initial_black_influence);
  compute_surrounding_moyo_sizes(&initial_white_influence);

  for (d = 0; d < number_of_dragons; d++)
    dragon2[d].weakness = compute_dragon_weakness_value(d);
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


/* The following two functions allow to traverse all worms in a dragon:
 * for (ii = first_worm_in_dragon(pos); ii != NO_MOVE;
 *      ii = next_worm_in_dragon(ii);)
 *   ...
 * At the moment first_worm(pos) will always be the origin of the dragon,
 * but you should not rely on that.
 */
int
first_worm_in_dragon(int w)
{
  return dragon[w].origin;
}

int
next_worm_in_dragon(int w)
{
  gg_assert(worm[w].origin == w);
  return next_worm_list[w];
}


/* ================================================================ */
/*                       A few status functions                     */
/* ================================================================ */

/*
 * These functions are only here because then we don't need to expose
 * the dragon structure to the external program.
 */

int
crude_status(int pos)
{
  return dragon[pos].crude_status;
}


int
dragon_status(int pos)
{
  return dragon[pos].status;
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

/* For use in gdb, print details of the dragon at (m, n). 
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
  report_dragon(stderr, POS(m, n));
}


void
report_dragon(FILE *outfile, int pos)
{
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

  gfprintf(outfile, "color                   %s\n", color_to_string(d->color));
  gfprintf(outfile, "origin                  %1m\n", d->origin);
  gfprintf(outfile, "size                    %d\n", d->size);
  gfprintf(outfile, "effective_size          %f\n", d->effective_size);
  gfprintf(outfile, "genus                   %s\n",
	   eyevalue_to_string(&d2->genus));
  gfprintf(outfile, "heye                    %1m\n", d2->heye);
  gfprintf(outfile, "escape_route            %d\n", d2->escape_route);
  gfprintf(outfile, "lunch                   %1m\n", d2->lunch);
  gfprintf(outfile, "crude status            %s\n",
	   status_to_string(d->crude_status));
  gfprintf(outfile, "owl_status              %s\n",
	   status_to_string(d->owl_status));
  gfprintf(outfile, "status                  %s\n",
	   status_to_string(d->status));
  gfprintf(outfile, "owl_threat_status       %s\n",
	   status_to_string(d->owl_threat_status));
  gfprintf(outfile, "owl_attack              %1m\n", d->owl_attack_point);
  gfprintf(outfile, "owl_attack_certain      %s\n",
	   (d->owl_attack_certain) ? "YES" : "NO");
  gfprintf(outfile, "owl_2nd_attack          %1m\n",
	   d->owl_second_attack_point);
  gfprintf(outfile, "owl_defend              %1m\n", d->owl_defense_point);
  gfprintf(outfile, "owl_defense_certain     %s\n",
	   (d->owl_defense_certain) ? "YES" : "NO");
  gfprintf(outfile, "owl_2nd_defend          %1m\n",
           d->owl_second_defense_point);
  gfprintf(outfile, "semeai                  %d\n", d2->semeai);
  gfprintf(outfile, "semeai_margin_of_safety %d\n",
	   d2->semeai_margin_of_safety);
  gfprintf(outfile, "neighbors               ");
  for (k = 0; k < d2->neighbors; k++)
    gfprintf(outfile, "%1m ", DRAGON(d2->adjacent[k]).origin);
  gfprintf(outfile, "\nhostile neighbors       %d\n", d2->hostile_neighbors);
  gfprintf(outfile, "moyo size               %d\n", d2->moyo_size);
  gfprintf(outfile, "moyo territorial value  %f\n",
	   d2->moyo_territorial_value);
  gfprintf(outfile, "safety                  %s\n",
	   safety_to_string(d2->safety));
  gfprintf(outfile, "weakness estimate       %f\n", d2->weakness);
  gfprintf(outfile, "strings                 ");
  for (ii = BOARDMIN; ii < BOARDMAX; ii++)
    if (ON_BOARD(ii)
	&& worm[ii].origin == ii
	&& is_same_dragon(ii, pos))
	gfprintf(outfile, "%1m ", ii);
  gfprintf(outfile, "\n");
}


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
