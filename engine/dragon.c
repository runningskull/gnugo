/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * This is GNU Go, a Go program. Contact gnugo@gnu.org, or see       *
 * http://www.gnu.org/software/gnugo/ for more information.          *
 *                                                                   *
 * Copyright 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007,   *
 * 2008, 2009 and 2010 by the Free Software Foundation.              *
 *                                                                   *
 * This program is free software; you can redistribute it and/or     *
 * modify it under the terms of the GNU General Public License as    *
 * published by the Free Software Foundation - version 3 or          *
 * (at your option) any later version.                               *
 *                                                                   *
 * This program is distributed in the hope that it will be useful,   *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of    *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     *
 * GNU General Public License in file COPYING for more details.      *
 *                                                                   *
 * You should have received a copy of the GNU General Public         *
 * License along with this program; if not, write to the Free        *
 * Software Foundation, Inc., 51 Franklin Street, Fifth Floor,       *
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
static void find_lunches(void);
static void eye_computations(void);
static void revise_inessentiality(void);
static void find_neighbor_dragons(void);
static void add_adjacent_dragons(int a, int b);
static void add_adjacent_dragon(int a, int b);
static int dragon_invincible(int pos);
static int dragon_looks_inessential(int origin);
static void identify_thrashing_dragons(void);
static void analyze_false_eye_territory(void);
static int connected_to_eye(int pos, int str, int color, int eye_color,
			    struct eye_data *eye);
static void connected_to_eye_recurse(int pos, int str, int color,
				     int eye_color, struct eye_data *eye,
				     signed char *mx, signed char *me,
				     int *halfeyes);
static enum dragon_status compute_crude_status(int pos);
static int compute_escape(int pos, int dragon_status_known);
static void compute_surrounding_moyo_sizes(const struct influence_data *q);
static void clear_cut_list(void);

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
make_dragons(int stop_before_owl)
{
  int str;
  int d;

  dragon2_initialized = 0;
  initialize_dragon_data();

  /* Find explicit connections patterns in database and amalgamate
   * involved dragons.
   */
  memset(cutting_points, 0, sizeof(cutting_points));
  find_cuts();
  find_connections();

  /* At this time, all dragons have been finalized and we can
   * initialize the dragon2[] array. After that we can no longer allow
   * amalgamation of dragons.
   */
  initialize_supplementary_dragon_data();
  
  make_domains(black_eye, white_eye, 0);

  /* Find adjacent worms which can be easily captured: */
  find_lunches();

  /* Find topological half eyes and false eyes. */
  find_half_and_false_eyes(BLACK, black_eye, half_eye, NULL);
  find_half_and_false_eyes(WHITE, white_eye, half_eye, NULL);

  /* Compute the number of eyes, half eyes, determine attack/defense points
   * etc. for all eye spaces. */
  eye_computations();
  /* Try to determine whether topologically false and half eye points
   * contribute to territory even if the eye doesn't solidify.
   */
  analyze_false_eye_territory();

  /* Now we compute the genus. */
  for (d = 0; d < number_of_dragons; d++)
    compute_dragon_genus(dragon2[d].origin, &dragon2[d].genus, NO_MOVE);

  /* Compute the escape route measure. */
  for (str = BOARDMIN; str < BOARDMAX; str++)
    if (IS_STONE(board[str]) && dragon[str].origin == str)
      DRAGON2(str).escape_route = compute_escape(str, 0);

  /* Set dragon weaknesses according to initial_influence. */
  compute_refined_dragon_weaknesses();
  for (d = 0; d < number_of_dragons; d++)
    dragon2[d].weakness_pre_owl = dragon2[d].weakness;

  /* Determine status: ALIVE, DEAD, CRITICAL or UNKNOWN */
  for (str = BOARDMIN; str < BOARDMAX; str++)
    if (ON_BOARD(str))
      if (dragon[str].origin == str && board[str])
	dragon[str].crude_status = compute_crude_status(str);
  
  /* We must update the dragon status at every intersection before we
   * call the owl code. This updates all fields.
   */
  for (str = BOARDMIN; str < BOARDMAX; str++)
    if (ON_BOARD(str) && board[str] != EMPTY)
      dragon[str] = dragon[dragon[str].origin];
  
  find_neighbor_dragons();

  for (d = 0; d < number_of_dragons; d++) {
    dragon2[d].surround_status 
      = compute_surroundings(dragon2[d].origin, NO_MOVE, 0,
			     &(dragon2[d].surround_size));
    if (dragon2[d].surround_status == SURROUNDED) {
      dragon2[d].escape_route = 0;
      if (debug & DEBUG_DRAGONS)
	gprintf("surrounded dragon found at %1m\n", dragon2[d].origin);
    }
    else if (dragon2[d].surround_status == WEAKLY_SURROUNDED) {
      dragon2[d].escape_route /= 2;
      if (debug & DEBUG_DRAGONS)
	gprintf("weakly surrounded dragon found at %1m\n", dragon2[d].origin);
    }
  }

  if (stop_before_owl)
    return;
  
  /* Determine life and death status of each dragon using the owl code
   * if necessary.
   */
  start_timer(2);
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
	DRAGON2(str).owl_status = UNCHECKED;
	DRAGON2(str).owl_attack_point  = NO_MOVE;
	DRAGON2(str).owl_defense_point = NO_MOVE;
      }
      else {
	int acode = 0;
	int dcode = 0;
	int kworm = NO_MOVE;
	int owl_nodes_before = get_owl_node_counter();
	start_timer(3);
	acode = owl_attack(str, &attack_point, 
			   &DRAGON2(str).owl_attack_certain, &kworm);
	DRAGON2(str).owl_attack_node_count
	  = get_owl_node_counter() - owl_nodes_before;
	if (acode != 0) {
	  DRAGON2(str).owl_attack_point = attack_point;
	  DRAGON2(str).owl_attack_code = acode;
	  DRAGON2(str).owl_attack_kworm = kworm;
	  if (attack_point != NO_MOVE) {
	    kworm = NO_MOVE;
	    dcode = owl_defend(str, &defense_point,
			       &DRAGON2(str).owl_defense_certain, &kworm);
	    if (dcode != 0) {
	      if (defense_point != NO_MOVE) {
		DRAGON2(str).owl_status = (acode == GAIN ? ALIVE : CRITICAL);
		DRAGON2(str).owl_defense_point = defense_point;
		DRAGON2(str).owl_defense_code = dcode;
		DRAGON2(str).owl_defense_kworm = kworm;
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
		DRAGON2(str).owl_status = (acode == GAIN ? ALIVE : CRITICAL);
		DEBUG(DEBUG_OWL_PERFORMANCE,
		      "Inconsistent owl attack and defense results for %1m.\n", 
		      str);
		/* Let's see whether the attacking move might be the right
		 * defense:
		 */
		dcode = owl_does_defend(DRAGON2(str).owl_attack_point,
					str, NULL);
		if (dcode != 0) {
		  DRAGON2(str).owl_defense_point
		    = DRAGON2(str).owl_attack_point;
		  DRAGON2(str).owl_defense_code = dcode;
		}
	      }
	    }
	  }
	  if (dcode == 0) {
	    DRAGON2(str).owl_status = DEAD; 
	    DRAGON2(str).owl_defense_point = NO_MOVE;
	    DRAGON2(str).owl_defense_code = 0;
	  }
	}
	else {
	  if (!DRAGON2(str).owl_attack_certain) {
	    kworm = NO_MOVE;
	    dcode = owl_defend(str, &defense_point, 
			       &DRAGON2(str).owl_defense_certain, &kworm);
	    if (dcode != 0) {
	      /* If the result of owl_attack was not certain, we may
	       * still want the result of owl_defend */
	      DRAGON2(str).owl_defense_point = defense_point;
	      DRAGON2(str).owl_defense_code = dcode;
	      DRAGON2(str).owl_defense_kworm = kworm;
	    }
	  }
	  DRAGON2(str).owl_status = ALIVE;
	  DRAGON2(str).owl_attack_point = NO_MOVE;
	  DRAGON2(str).owl_attack_code = 0;
	  
	}
      }
    }
  time_report(2, "  owl reading", NO_MOVE, 1.0);
  
  /* Compute the status to be used by the matcher. We most trust the
   * owl status, if it is available. If it's not we assume that we are
   * already confident that the dragon is alive, regardless of
   * crude_status.
   */
  for (str = BOARDMIN; str < BOARDMAX; str++)
    if (IS_STONE(board[str])) {
      if (DRAGON2(str).owl_status != UNCHECKED)
	dragon[str].status = DRAGON2(str).owl_status;
      else
	dragon[str].status = ALIVE;
    }

  /* The dragon data is now correct at the origin of each dragon but
   * we need to copy it to every vertex.  
   */
  for (str = BOARDMIN; str < BOARDMAX; str++)
    if (ON_BOARD(str) && board[str] != EMPTY)
      dragon[str] = dragon[dragon[str].origin];

  identify_thrashing_dragons();
  
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
	DRAGON2(str).owl_threat_status = UNCHECKED;
	DRAGON2(str).owl_second_attack_point  = NO_MOVE;
	DRAGON2(str).owl_second_defense_point = NO_MOVE;
      }
      else {
	int acode = DRAGON2(str).owl_attack_code;
	int dcode = DRAGON2(str).owl_defense_code;
	int defense_point, second_defense_point;

	if (get_level() >= 8
	    && !disable_threat_computation
	    && (owl_threats 
		|| thrashing_stone[str])) {
	  if (acode && !dcode && DRAGON2(str).owl_attack_point != NO_MOVE) {
	    if (owl_threaten_defense(str, &defense_point,
				     &second_defense_point)) {
	      DRAGON2(str).owl_threat_status = CAN_THREATEN_DEFENSE;
	      DRAGON2(str).owl_defense_point = defense_point;
	      DRAGON2(str).owl_second_defense_point = second_defense_point;
	    }
	    else
	      DRAGON2(str).owl_threat_status = DEAD;
	  }
	  else if (!acode) {
	    int attack_point, second_attack_point;
	    if (owl_threaten_attack(str, 
				    &attack_point, &second_attack_point)) {
	      DRAGON2(str).owl_threat_status = CAN_THREATEN_ATTACK;
	      DRAGON2(str).owl_attack_point = attack_point;
	      DRAGON2(str).owl_second_attack_point = second_attack_point;
	    }
	    else
	      DRAGON2(str).owl_threat_status = ALIVE;
	  }
	}
      }
    }
  
  /* Once again, the dragon data is now correct at the origin of each dragon
   * but we need to copy it to every vertex.  
   */
  for (str = BOARDMIN; str < BOARDMAX; str++)
    if (ON_BOARD(str) && board[str] != EMPTY)
      dragon[str] = dragon[dragon[str].origin];

  time_report(2, "  owl threats ", NO_MOVE, 1.0);
  

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
    else if (dragon_invincible(origin)) {
      dragon2[d].safety = INVINCIBLE;
      /* Sometimes the owl analysis may have misevaluated invincible
       * dragons, typically if they live by topologically false eyes.
       * Therefore we also set the status here.
       */
      DRAGON(d).status = ALIVE;
    }
    else if (dragon2[d].owl_status == DEAD)
      dragon2[d].safety = DEAD;
    else if (dragon2[d].owl_status == CRITICAL)
      dragon2[d].safety = CRITICAL;
    else if (true_genus >= 6 || dragon2[d].moyo_size > 20)
      dragon2[d].safety = STRONGLY_ALIVE;
    else
      dragon2[d].safety = ALIVE;
  }

  /* The status is now correct at the origin of each dragon
   * but we need to copy it to every vertex.
   */
  for (str = BOARDMIN; str < BOARDMAX; str++)
    if (ON_BOARD(str))
      dragon[str].status = dragon[dragon[str].origin].status;

  /* Revise inessentiality of critical worms and dragons. */
  revise_inessentiality();

  semeai();
  time_report(2, "  semeai module", NO_MOVE, 1.0);
  
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


/* Find capturable worms adjacent to each dragon. */
static void
find_lunches()
{
  int str;
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
}


/* Compute the value of each eye space. Store its attack and defense point.
 * A more comlete list of attack and defense points is stored in the lists
 * black_vital_points and white_vital_points.
 */
static void
eye_computations()
{ 
  int str;

  for (str = BOARDMIN; str < BOARDMAX; str++) {
    if (!ON_BOARD(str))
      continue;

    if (black_eye[str].color == BLACK
	&& black_eye[str].origin == str) {
      struct eyevalue value;
      int attack_point, defense_point;
      
      compute_eyes(str, &value, &attack_point, &defense_point, 
		   black_eye, half_eye, 1);
      DEBUG(DEBUG_EYES, "Black eyespace at %1m: %s\n", str,
	    eyevalue_to_string(&value));
      black_eye[str].value = value;
      propagate_eye(str, black_eye);
    }
    
    if (white_eye[str].color == WHITE
	&& white_eye[str].origin == str) {
      struct eyevalue value;
      int attack_point, defense_point;
      
      compute_eyes(str, &value, &attack_point, &defense_point,
		   white_eye, half_eye, 1);
      DEBUG(DEBUG_EYES, "White eyespace at %1m: %s\n", str,
	    eyevalue_to_string(&value));
      white_eye[str].value = value;
      propagate_eye(str, white_eye);
    }
  }
}


/* This function revises the inessentiality of critical worms and dragons
 * according to the criteria explained in the comments below.
 */
static void
revise_inessentiality()
{
  int str;
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
	  DEBUG(DEBUG_WORMS, "Worm %1m revised to be inessential.\n", str);
	  worm[str].inessential = 1;
	  propagate_worm(str);
	}
      }
    }

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
	DEBUG(DEBUG_DRAGONS, "Dragon %1m revised to be inessential.\n", str);
	DRAGON2(str).safety = INESSENTIAL;
      }
    }
  }
}

/* Initialize the dragon[] array. */

void
initialize_dragon_data(void)
{
  int str;
  /* VALGRIND_MAKE_WRITABLE(dragon, BOARDMAX * sizeof(struct dragon_data)); */
  for (str = BOARDMIN; str < BOARDMAX; str++)
    if (ON_BOARD(str)) {

      dragon[str].id                 = -1;
      dragon[str].size               = worm[str].size;
      dragon[str].effective_size     = worm[str].effective_size;
      dragon[str].color              = worm[str].color;
      dragon[str].origin             = worm[str].origin;
      dragon[str].crude_status       = UNKNOWN;
      dragon[str].status             = UNKNOWN;
      half_eye[str].type             =  0;
      half_eye[str].value            =  10.0; /* Something big. */
      
      if (IS_STONE(board[str]) && worm[str].origin == str)
	DEBUG(DEBUG_DRAGONS, 
	      "Initializing dragon from worm at %1m, size %d\n", 
	      str, worm[str].size);
    }
  memset(next_worm_list, 0, sizeof(next_worm_list));

  /* We need to reset this to avoid trouble on an empty board when
   * moves have previously been generated for a non-empty board.
   *
   * Comment: The cause of this is that make_dragons() is not called
   * for an empty board, only initialize_dragon_data(), so we never
   * reach initialize_supplementary_dragon_data().
   */
  number_of_dragons = 0;

  clear_cut_list();

  memset(black_vital_points, 0, BOARDMAX * sizeof(struct vital_eye_points));
  memset(white_vital_points, 0, BOARDMAX * sizeof(struct vital_eye_points));
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
    dragon2[d].neighbors                = 0;
    dragon2[d].hostile_neighbors        = 0;

    dragon2[d].moyo_size	        = -1;
    dragon2[d].moyo_territorial_value   = 0.0;
    dragon2[d].safety                   = -1;
    dragon2[d].escape_route             = 0;
    dragon2[d].heye                     = NO_MOVE;
    dragon2[d].lunch                    = NO_MOVE;
    dragon2[d].surround_status          = 0;
    set_eyevalue(&dragon2[d].genus, 0, 0, 0, 0);

    dragon2[d].semeais                  = 0;
    dragon2[d].semeai_defense_code	= 0;
    dragon2[d].semeai_defense_point	= NO_MOVE;
    dragon2[d].semeai_attack_code	= 0;
    dragon2[d].semeai_attack_point	= NO_MOVE;
    dragon2[d].owl_attack_point         = NO_MOVE;
    dragon2[d].owl_attack_code          = 0;
    dragon2[d].owl_attack_certain       = 1;
    dragon2[d].owl_defense_point        = NO_MOVE;
    dragon2[d].owl_defense_code         = 0;
    dragon2[d].owl_defense_certain      = 1;
    dragon2[d].owl_status               = UNCHECKED;
    dragon2[d].owl_threat_status        = UNCHECKED;
    dragon2[d].owl_second_attack_point  = NO_MOVE;
    dragon2[d].owl_second_defense_point = NO_MOVE;
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
    }
    if (!found_one)
      break;
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
  gg_assert(a >= 0 && a < number_of_dragons
	    && b >= 0 && b < number_of_dragons);
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
  gg_assert(a >= 0 && a < number_of_dragons
	    && b >= 0 && b < number_of_dragons);
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
 * a) At least two distinct eyespaces without topological halfeyes,
 *    marginal vertices, or tactically critical or alive opponent strings.
 *    Furthermore there may not be an owl attack of the dragon.
 * b) At least one string which is unconditionally alive according to the
 *    unconditional_life() function in utils.c.
 *
 * For the requirement on opponent strings in a), see e.g.
 * seki:404,408,409,413,504,604,908.
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
  
  ASSERT1(IS_STONE(board[dr]), dr);

  /* First look for invincible strings in the dragon. */
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (ON_BOARD(pos) && is_same_dragon(pos, dr) && worm[pos].invincible)
      return 1;
  }

  /* Can the dragon be owl attacked? */
  if (DRAGON2(dr).owl_status != UNCHECKED && DRAGON2(dr).owl_status != ALIVE)
    return 0;
  
  /* Examine the eye spaces. */
  if (board[dr] == BLACK) {
    eye = black_eye;
    eye_color = BLACK;
  }
  else {
    eye = white_eye;
    eye_color = WHITE;
  }

  memset(mx, 0, sizeof(mx));

  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (board[pos] == board[dr] && is_same_dragon(pos, dr)) {
      for (k = 0; k < 4; k++) {
	int pos2 = pos + delta[k];
	if (ON_BOARD(pos2)
	    && eye[pos2].color == eye_color
	    && eye[pos2].origin != NO_MOVE) {
	  if (eye[pos2].marginal
	      || is_halfeye(half_eye, pos2))
	    mx[eye[pos2].origin] = 2; /* bad eye */
	  else if (mx[eye[pos2].origin] == 0)
	    mx[eye[pos2].origin] = 1; /* good eye */
	  
	  if (board[pos2] == OTHER_COLOR(board[dr])
	      && (!attack(pos2, NULL) || find_defense(pos2, NULL)))
	    mx[eye[pos2].origin] = 2; /* bad eye */
	}
      }
    }
  }

  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    /* Necessary to check eye margins here since the loop above only
     * considers margins which are directly adjacent to some stone of
     * the dragon.
     */
    if (mx[pos] == 1
	&& eye[pos].msize == 0)
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
#if 0
  int d;
  int k;
#endif
  
  if (dragon[origin].size != worm[origin].size)
    return 0;

  if (worm[origin].size >= 3 && worm[origin].liberties >= 4)
    return 0;

  if (owl_substantial(origin))
    return 0;

#if 0
  /* This is a proposed modification which solves 13x13:72 but
   * breaks buzco:5. It adds the two requirements:
   *
   * 3. Has no opponent neighbor with status better than DEAD.
   * 4. Has no opponent neighbor with escape value bigger than 0.
   *
   * This probably needs to be revised before it's enabled.
   */
  for (k = 0; k < DRAGON2(origin).neighbors; k++) {
    d = DRAGON2(origin).adjacent[k];
    if (DRAGON(d).color != board[origin]
	&& (DRAGON(d).status != DEAD
	    || dragon2[d].escape_route > 0))
      return 0;
  }
#endif
  
  return 1;
}


/* Report which stones are alive if it's (color)'s turn to move. I.e.
 * critical stones belonging to (color) are considered alive.
 * A stone is dead resp. critical if the tactical reading code _or_ the
 * owl code thinks so.
 */
static void
get_alive_stones(int color, signed char safe_stones[BOARDMAX])
{
  int d;
  get_lively_stones(color, safe_stones);
  for (d = 0; d < number_of_dragons; d++) {
    if (dragon2[d].safety == DEAD
	|| (dragon2[d].safety == CRITICAL
	    && board[dragon2[d].origin] == OTHER_COLOR(color))) {
      mark_dragon(dragon2[d].origin, safe_stones, 0);
    }
  }
}


/* If the opponent's last move is a dead dragon, this is called a
 * *thrashing dragon*. We must be careful because the opponent may be
 * trying to trick us, so even though GNU Go thinks the stone is dead,
 * we should consider attacking it, particularly if we are ahead.
 *
 * This function determines whether the last played move is part of a
 * dead dragon. It also looks for dead friendly neighbors of the
 * thrashing dragon, which are also considered as thrashing. The
 * stones of the primary thrashing dragon are marked by 1 in the
 * thrashing_stone[] array and its neighbors are marked by 2.
 * Neighbors of neighbors are marked 3, and so on, up to at most
 * distance 5.
 */
static void
identify_thrashing_dragons()
{
  int k;
  int dist;
  int last_move;
  int color;

  thrashing_dragon = 0;
  memset(thrashing_stone, 0, sizeof(thrashing_stone));

  last_move = get_last_move();
  if (last_move == NO_MOVE
      || dragon[last_move].status != DEAD)
    return;

  thrashing_dragon = dragon[last_move].origin;
  DEBUG(DEBUG_DRAGONS, "thrashing dragon found at %1m\n", thrashing_dragon);
  mark_dragon(thrashing_dragon, thrashing_stone, 1);
  color = board[thrashing_dragon];
  
  for (dist = 1; dist < 5; dist++) {
    int pos;
    for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
      if (board[pos] != color
	  || dragon[pos].origin != pos
	  || thrashing_stone[pos] != dist)
	continue;
      
      for (k = 0; k < DRAGON2(pos).neighbors; k++) {
	int d = DRAGON2(pos).adjacent[k];
	if (DRAGON(d).color == color
	    && DRAGON(d).status == DEAD
	    && thrashing_stone[dragon2[d].origin] == 0) {
	  DEBUG(DEBUG_DRAGONS,
		"neighbor at distance %d of thrashing dragon found at %1m\n",
		dist + 1, DRAGON(d).origin);
	  mark_dragon(DRAGON(d).origin, thrashing_stone,
		      (signed char)(dist + 1));
	}
      }
    }
  }
}


static void
set_dragon_strengths(const signed char safe_stones[BOARDMAX],
    		     float strength[BOARDMAX])
{
  int ii;
  for (ii = BOARDMIN; ii < BOARDMAX; ii++)
    if (ON_BOARD(ii)) {
      if (safe_stones[ii]) {
	ASSERT1(IS_STONE(board[ii]), ii);
	strength[ii] = DEFAULT_STRENGTH
	  	       * (1.0 - 0.3 * DRAGON2(ii).weakness_pre_owl);
      }
      else
	strength[ii] = 0.0;
    }
}

/* Marks all inessential stones with INFLUENCE_SAFE_STONE, leaves
 * everything else unchanged.
 */
void
mark_inessential_stones(int color, signed char safe_stones[BOARDMAX])
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
			&& board[ii] == color)))))
      safe_stones[ii] = INFLUENCE_SAFE_STONE;
}

void
set_strength_data(int color, signed char safe_stones[BOARDMAX],
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
  signed char safe_stones[BOARDMAX];
  float strength[BOARDMAX];

  set_strength_data(BLACK, safe_stones, strength);
  compute_influence(BLACK, safe_stones, strength, &initial_black_influence,
                    NO_MOVE, "initial black influence, dragons known");
  break_territories(BLACK, &initial_black_influence, 1, NO_MOVE);

  set_strength_data(WHITE, safe_stones, strength);
  compute_influence(WHITE, safe_stones, strength, &initial_white_influence,
                    NO_MOVE, "initial white influence, dragons known");
  break_territories(WHITE, &initial_white_influence, 1, NO_MOVE);
}


/* Compute dragon's genus, possibly excluding one given eye.  To
 * compute full genus, just set `eye_to_exclude' to NO_MOVE.
 */
void
compute_dragon_genus(int d, struct eyevalue *genus, int eye_to_exclude)
{
  int pos;
  int dr;

  ASSERT1(IS_STONE(board[d]), d);
  gg_assert(eye_to_exclude == NO_MOVE || ON_BOARD1(eye_to_exclude));

  set_eyevalue(genus, 0, 0, 0, 0);

  if (board[d] == BLACK) {
    for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
      if (!ON_BOARD(pos))
	continue;

      if (black_eye[pos].color == BLACK
	  && black_eye[pos].origin == pos
	  && (eye_to_exclude == NO_MOVE
	      || black_eye[eye_to_exclude].origin != pos)
	  && find_eye_dragons(pos, black_eye, BLACK, &dr, 1) == 1
	  && is_same_dragon(dr, d)) {
	TRACE("eye at %1m (%s) found for dragon at %1m--augmenting genus\n",
	      pos, eyevalue_to_string(&black_eye[pos].value), dr);

	if (eye_to_exclude == NO_MOVE
	    && (eye_move_urgency(&black_eye[pos].value)
		> eye_move_urgency(genus)))
	  DRAGON2(d).heye = black_vital_points[pos].defense_points[0];

	add_eyevalues(genus, &black_eye[pos].value, genus);
      }
    }
  }
  else {
    for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
      if (!ON_BOARD(pos))
	continue;

      if (white_eye[pos].color == WHITE
	  && white_eye[pos].origin == pos
	  && (eye_to_exclude == NO_MOVE
	      || white_eye[eye_to_exclude].origin != pos)
	  && find_eye_dragons(pos, white_eye, WHITE, &dr, 1) == 1
	  && is_same_dragon(dr, d)) {
	TRACE("eye at %1m (%s) found for dragon at %1m--augmenting genus\n",
	      pos, eyevalue_to_string(&white_eye[pos].value), dr);

	if (eye_to_exclude == NO_MOVE
	    && (eye_move_urgency(&white_eye[pos].value)
		> eye_move_urgency(genus)))
	  DRAGON2(d).heye = white_vital_points[pos].defense_points[0];

	add_eyevalues(genus, &white_eye[pos].value, genus);
      }
    }
  }
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
 * result is stored in the false_eye_territory[] array.
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
    if (white_eye[pos].color == WHITE) {
      color = WHITE;
      eye_color = WHITE;
      eye = white_eye;
    }
    else if (black_eye[pos].color == BLACK) {
      color = BLACK;
      eye_color = BLACK;
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

  /* FIXME: This initialization doesn't really belong here but must be
   *        done somewhere within examine_position().
   *        The array is eventually filled by the endgame() function.
   */
  for (pos = BOARDMIN; pos < BOARDMAX; pos++)
    if (ON_BOARD(pos))
      forced_backfilling_moves[pos] = 0;
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
  signed char mx[BOARDMAX];
  signed char me[BOARDMAX];
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
			 struct eye_data *eye, signed char *mx,
			 signed char *me, int *halfeyes)
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
      
      for (k = 0; k < MAX_TACTICAL_POINTS; k++) {
	if (w->attack_codes[k] == 0)
	  break;
	gprintf("- attackable at %1m, attack code = %d\n",
		w->attack_points[k], w->attack_codes[k]);
      }
      
      for (k = 0; k < MAX_TACTICAL_POINTS; k++) {
	if (w->defense_codes[k] == 0)
	  break;
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
	      status_to_string(dd->crude_status),
	      status_to_string(dd->status),
	      d2->moyo_size,
	      d2->moyo_territorial_value,
	      status_to_string(d2->safety),
	      d2->weakness_pre_owl,
	      d2->weakness);
      gprintf(", owl status %s\n", status_to_string(d2->owl_status));
      if (d2->owl_status == CRITICAL) {
	gprintf("... owl attackable at %1m, code %d\n",
		d2->owl_attack_point, d2->owl_attack_code);
	gprintf("... owl defendable at %1m, code %d\n",
		d2->owl_defense_point, d2->owl_defense_code);
      }
      if (dd->status == CRITICAL && d2->semeais) {
	if (d2->semeai_defense_point)
	  gprintf("... semeai defense move at %1m, result code %s\n",
		  d2->semeai_defense_point,
		  result_to_string(d2->semeai_defense_code));
	if (d2->semeai_attack_point)
	  gprintf("... semeai attack move at %1m, result code %s\n",
		  d2->semeai_attack_point,
		  result_to_string(d2->semeai_attack_code));
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


static int new_dragon_origins[BOARDMAX];

/* Compute new dragons, e.g. after having made a move. This will not
 * affect any global state.
 */
void
compute_new_dragons(int dragon_origins[BOARDMAX])
{
  int pos;
  int saved_cutting_points[BOARDMAX];

  /* This is currently necessary in order not to mess up the
   * worm[].cutstone2 field. See cutstone2_helper in
   * patterns/helpers.c. On the other hand it shouldn't be very
   * interesting to recompute dragons in the original position.
   */
  gg_assert(stackp > 0);
  
  memcpy(saved_cutting_points, cutting_points, sizeof(cutting_points));
  memset(cutting_points, 0, sizeof(cutting_points));
  for (pos = BOARDMIN; pos < BOARDMAX; pos++)
    if (ON_BOARD(pos)) {
      if (board[pos] == EMPTY)
	new_dragon_origins[pos] = NO_MOVE;
      else
	new_dragon_origins[pos] = find_origin(pos);
    }
  
  find_cuts();
  find_connections();

  memcpy(cutting_points, saved_cutting_points, sizeof(cutting_points));
  memcpy(dragon_origins, new_dragon_origins, sizeof(new_dragon_origins));
}


/* This gets called if we are trying to compute dragons outside of
 * make_dragons(), typically after a move has been made.
 */
static void
join_new_dragons(int d1, int d2)
{
  int pos;
  /* Normalize dragon coordinates. */
  d1 = new_dragon_origins[d1];
  d2 = new_dragon_origins[d2];

  /* If d1 and d2 are the same dragon, we do nothing. */
  if (d1 == d2)
    return;

  ASSERT1(board[d1] == board[d2], d1);
  ASSERT1(IS_STONE(board[d1]), d1);

  /* Don't bother to do anything fancy with dragon origins. */
  for (pos = BOARDMIN; pos < BOARDMAX; pos++)
    if (ON_BOARD(pos) && new_dragon_origins[pos] == d2)
      new_dragon_origins[pos] = d1;
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

  /* If not called from make_dragons(), we don't work on the main
   * dragon[] array.
   */
  if (stackp > 0) {
    join_new_dragons(d1, d2);
    return;
  }
  
  /* Normalize dragon coordinates. */
  d1 = dragon[d1].origin;
  d2 = dragon[d2].origin;

  /* If d1 and d2 are the same dragon, we do nothing. */
  if (d1 == d2)
    return;
  
  ASSERT1(board[d1] == board[d2], d1);
  gg_assert(dragon2_initialized == 0);
  ASSERT1(IS_STONE(board[d1]), d1);

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

static enum dragon_status
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

  if (DRAGON2(pos).moyo_territorial_value > 9.99)
    return ALIVE;
  
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
dragon_escape(signed char goal[BOARDMAX], int color,
	      signed char escape_value[BOARDMAX])
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
      if (cut_possible(ii, OTHER_COLOR(color)))
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
  signed char goal[BOARDMAX];
  signed char escape_value[BOARDMAX];
  signed char safe_stones[BOARDMAX];

  ASSERT1(IS_STONE(board[pos]), pos);

  for (ii = BOARDMIN; ii < BOARDMAX; ii++)
    if (ON_BOARD(ii))
      goal[ii] = is_same_dragon(ii, pos);

  /* Compute escape_value array.  Points are awarded for moyo (4),
   * area (2) or EMPTY (1).  Values may change without notice.
   */
  get_lively_stones(OTHER_COLOR(board[pos]), safe_stones);
  compute_escape_influence(board[pos], safe_stones, NULL, 0, escape_value);

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
  int k;
  int moyo_color;
  float moyo_sizes[BOARDMAX];
  float moyo_values[BOARDMAX];
    

  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    moyo_sizes[pos] = 0.0;
    moyo_values[pos] = 0.0;
  }
  
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (!ON_BOARD(pos))
      continue;
    moyo_color = whose_moyo_restricted(q, pos);
    
    if (moyo_color == board[pos])
      continue;
    
    if (moyo_color == WHITE) {
      for (k = 0; k < number_close_white_worms[pos]; k++) {
	int w = close_white_worms[pos][k];
	int dr = dragon[w].origin;
	
	moyo_sizes[dr] += 1.0 / number_close_white_worms[pos];
	moyo_values[dr] += (gg_min(influence_territory(q, pos, WHITE), 1.0)
			    / number_close_white_worms[pos]);
      }
    }
    
    if (moyo_color == BLACK) {
      for (k = 0; k < number_close_black_worms[pos]; k++) {
	int w = close_black_worms[pos][k];
	int dr = dragon[w].origin;
	
	moyo_sizes[dr] += 1.0 / number_close_black_worms[pos];
	moyo_values[dr] += (gg_min(influence_territory(q, pos, BLACK), 1.0)
			    / number_close_black_worms[pos]);
      }
    }
  }
  
  for (d = 0; d < number_of_dragons; d++) {
    int this_moyo_size = (int) moyo_sizes[dragon2[d].origin];
    float this_moyo_value = moyo_values[dragon2[d].origin];
    
    if (this_moyo_size < dragon2[d].moyo_size) {
      dragon2[d].moyo_size = this_moyo_size;
      dragon2[d].moyo_territorial_value = this_moyo_value;
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

  DEBUG(DEBUG_DRAGONS,
	"  moyo value %f -> %f, escape %f -> %f, eyes %f -> %f\n",
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

  DEBUG(DEBUG_DRAGONS, "Computing weakness of dragon at %1m:\n", origin);

  weakness = crude_dragon_weakness(dragon2[d].safety, &dragon2[d].genus,
				   dragon2[d].lunch != NO_MOVE,
      				   dragon2[d].moyo_territorial_value, 
				   (float) dragon2[d].escape_route);

  /* Now corrections due to (uncertain) owl results resp. owl threats. */
  if (!dragon2[d].owl_attack_certain)
    weakness += gg_min(0.25 * (1.0 - weakness), 0.25 * weakness);
  if (!dragon2[d].owl_defense_certain)
    weakness += gg_min(0.25 * (1.0 - weakness), 0.25 * weakness);
  if (dragon2[d].owl_threat_status == CAN_THREATEN_ATTACK)
    weakness += 0.15 * (1.0 - weakness);

  if (weakness < 0.0)
    weakness = 0.0;
  if (weakness > 1.0)
    weakness = 1.0;

  DEBUG(DEBUG_DRAGONS, " result: %f.\n", weakness);
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

/* The strategic size is the effective size, plus a bonus for all weak
 * neighbouring dragons of the opponent.
 */
void
compute_strategic_sizes()
{
  float *bonus = calloc(number_of_dragons, sizeof(float));
  int d;
  int k;

  for (d = 0; d < number_of_dragons; d++) {
    /* Compute bonus for all neighbors of dragon (d). The total bonus for
     * all neighbors is effective_size(d) * weakness(d), and it is given
     * to a neighbor d2 proportionally to the value of
     * effective_size(d2) * weakness(d2).
     */
    float sum = 0.0;
    if (dragon2[d].safety == INESSENTIAL)
      continue;
    for (k = 0; k < dragon2[d].neighbors; k++) {
      int d2 = dragon2[d].adjacent[k];
      if (board[dragon2[d2].origin] == OTHER_COLOR(board[dragon2[d].origin])
	  && dragon2[d2].safety != INESSENTIAL)
	sum += DRAGON(d2).effective_size * dragon2[d2].weakness;
    }
    if (sum == 0.0)
      continue;
    for (k = 0; k < dragon2[d].neighbors; k++) {
      int d2 = dragon2[d].adjacent[k];
      if (board[dragon2[d2].origin] == OTHER_COLOR(board[dragon2[d].origin])
	  && dragon2[d2].safety != INESSENTIAL) {
	bonus[d2] += ((DRAGON(d2).effective_size * dragon2[d2].weakness) / sum)
		     * DRAGON(d).effective_size * dragon2[d].weakness;
	if (0)
	  gprintf("Dragon %1m receives %f effective size bonus from %1m.\n",
		  dragon2[d2].origin, 
		  ((DRAGON(d2).effective_size * dragon2[d2].weakness) / sum)
		  * DRAGON(d).effective_size * dragon2[d].weakness,
		  dragon2[d].origin);
      }
    }
  }

  for (d = 0; d < number_of_dragons; d++) {
    if (0)
      gprintf("Dragon %1m gets effective size bonus of %f.\n",
	      dragon2[d].origin, bonus[d]);
    /* We cap strategic size at 3 * effective_size. (This is ad hoc.) */
    dragon2[d].strategic_size = gg_min(bonus[d] + DRAGON(d).effective_size,
				       3 * DRAGON(d).effective_size);
  }

  free(bonus);
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


/* Mark the stones of a dragon. */
void
mark_dragon(int pos, signed char mx[BOARDMAX], signed char mark)
{
  int w;
  for (w = first_worm_in_dragon(dragon[pos].origin); w != NO_MOVE;
       w = next_worm_in_dragon(w))
    mark_string(w, mx, mark);
}


/* The following two functions allow to traverse all worms in a dragon:
 * for (ii = first_worm_in_dragon(pos); ii != NO_MOVE;
 *      ii = next_worm_in_dragon(ii);)
 *   ...
 * At the moment first_worm_in_dragon(pos) will always be the origin
 * of the dragon, but you should not rely on that.
 */
int
first_worm_in_dragon(int d)
{
  return dragon[d].origin;
}

int
next_worm_in_dragon(int w)
{
  ASSERT1(worm[w].origin == w, w);
  return next_worm_list[w];
}


/* ================================================================ */
/*                       A few status functions                     */
/* ================================================================ */

/*
 * These functions are only here because then we don't need to expose
 * the dragon structure to the external program.
 */

enum dragon_status
crude_status(int pos)
{
  return dragon[pos].crude_status;
}


enum dragon_status
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


/* Is this dragon weak? */

int 
dragon_weak(int pos)
{
  ASSERT_ON_BOARD1(pos);
  /* FIXME: This should not happen, but avoids a crash.  What is
   *   the proper fix for calling this at stackp != 0 ?
   */
  if (dragon[pos].id < 0 || dragon[pos].id >= number_of_dragons)
     return 1;
  return (DRAGON2(pos).weakness > 0.40001);
}


/* Returns the size of the biggest critical dragon on the board. */

int  
size_of_biggest_critical_dragon(void)
{ 
  int str;
  int max_size = 0;
  
  for (str = BOARDMIN; str < BOARDMAX; str++)
    if (ON_BOARD(str)) {
      
      if (board[str] == EMPTY
	  || dragon[str].origin != str)
	continue;
        
      /* Get the best available status for the dragon */
      if (dragon[str].status == CRITICAL) {
        if (dragon[str].size >= max_size)
          max_size = dragon[str].size;
      }
    }
  return max_size;
}


/************************************************************************
 *         A list of all cuts found during connection matching          *
 ************************************************************************/

#define MAX_CUTS 	3 * MAX_BOARD * MAX_BOARD

struct cut_data {
  int apos;
  int bpos;
  int move;
};

static int num_cuts = 0;
static struct cut_data cut_list[MAX_CUTS];

static void
clear_cut_list()
{
  num_cuts = 0;
}

/* Store in the list that (move) disconnects the two strings at
 * apos and bpos.
 */
void
add_cut(int apos, int bpos, int move)
{
  gg_assert(board[apos] == board[bpos]);
  if (num_cuts == MAX_CUTS)
    return;
  if (apos > bpos) {
    int tmp = apos;
    apos = bpos;
    bpos = tmp;
  }
  if (move == NO_MOVE)
    return;
  cut_list[num_cuts].apos = apos;
  cut_list[num_cuts].bpos = bpos;
  cut_list[num_cuts].move = move;
  num_cuts++;
  if (0)
  gprintf("Added %d-th cut at %1m between %1m and %1m.\n", num_cuts,
          move, apos, bpos);
}

/* For every move in the cut list disconnecting two of opponent's strings,
 * test whether the two strings can be connected at all. If so, add a
 * CUT_MOVE reason.
 */
void
cut_reasons(int color)
{
  int k;
  for (k = 0; k < num_cuts; k++)
    if (board[cut_list[k].apos] == OTHER_COLOR(color)
	&& !is_same_dragon(cut_list[k].apos, cut_list[k].bpos)
	&& string_connect(cut_list[k].apos, cut_list[k].bpos, NULL) == WIN)
      add_cut_move(cut_list[k].move, cut_list[k].apos, cut_list[k].bpos);
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
  int pos = string_to_location(board_size, string);

  if (!ON_BOARD(pos))
    fprintf(stderr, "unknown position %s\n", string);
  else
    report_dragon(stderr, pos);
}


void
report_dragon(FILE *outfile, int pos)
{
  int w;
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
  gfprintf(outfile, "strategic_size          %f\n", d2->strategic_size);
  gfprintf(outfile, "genus                   %s\n",
	   eyevalue_to_string(&d2->genus));
  gfprintf(outfile, "heye                    %1m\n", d2->heye);
  gfprintf(outfile, "escape_route            %d\n", d2->escape_route);
  gfprintf(outfile, "lunch                   %1m\n", d2->lunch);
  gfprintf(outfile, "crude_status            %s\n",
	   status_to_string(d->crude_status));
  gfprintf(outfile, "owl_status              %s\n",
	   status_to_string(d2->owl_status));
  gfprintf(outfile, "status                  %s\n",
	   status_to_string(d->status));
  gfprintf(outfile, "safety                  %s\n",
	   status_to_string(d2->safety));
  gfprintf(outfile, "weakness                %f\n", d2->weakness);
  gfprintf(outfile, "weakness_pre_owl        %f\n", d2->weakness_pre_owl);
  gfprintf(outfile, "surround_status         %d\n", d2->surround_status);
  gfprintf(outfile, "surround_size           %d\n", d2->surround_size);
  gfprintf(outfile, "moyo_size               %d\n", d2->moyo_size);
  gfprintf(outfile, "moyo_territorial_value  %f\n",
	   d2->moyo_territorial_value);
  gfprintf(outfile, "neighbors               ");
  for (k = 0; k < d2->neighbors; k++)
    gfprintf(outfile, "%1m ", DRAGON(d2->adjacent[k]).origin);
  gfprintf(outfile, "\nhostile_neighbors       %d\n", d2->hostile_neighbors);
  gfprintf(outfile, "owl_attack_code         %d\n", d2->owl_attack_code);
  gfprintf(outfile, "owl_attack_point        %1m\n", d2->owl_attack_point);
  gfprintf(outfile, "owl_attack_certain      %s\n",
	   (d2->owl_attack_certain) ? "YES" : "NO");
  gfprintf(outfile, "owl_2nd_attack_point    %1m\n",
	   d2->owl_second_attack_point);
  gfprintf(outfile, "owl_threat_status       %s\n",
	   status_to_string(d2->owl_threat_status));
  gfprintf(outfile, "owl_defense_code        %d\n", d2->owl_defense_code);
  gfprintf(outfile, "owl_defense_point       %1m\n", d2->owl_defense_point);
  gfprintf(outfile, "owl_defense_certain     %s\n",
	   (d2->owl_defense_certain) ? "YES" : "NO");
  gfprintf(outfile, "owl_2nd_defense_point   %1m\n",
           d2->owl_second_defense_point);
  gfprintf(outfile, "owl_attack_kworm        %1m\n", d2->owl_attack_kworm);
  gfprintf(outfile, "owl_defense_kworm       %1m\n", d2->owl_defense_kworm);
  gfprintf(outfile, "semeais                 %d\n", d2->semeais);
  gfprintf(outfile, "semeai_defense_code     %d\n", d2->semeai_defense_code);
  gfprintf(outfile, "semeai_defense_point    %1m\n", d2->semeai_defense_point);
  gfprintf(outfile, "semeai_defense_certain  %d\n",
	   d2->semeai_defense_certain);
  gfprintf(outfile, "semeai_defense_target   %1m\n",
      	   d2->semeai_defense_target);
  gfprintf(outfile, "semeai_attack_code      %d\n", d2->semeai_attack_code);
  gfprintf(outfile, "semeai_attack_point     %1m\n", d2->semeai_attack_point);
  gfprintf(outfile, "semeai_attack_certain   %d\n", d2->semeai_attack_certain);
  gfprintf(outfile, "semeai_attack_target    %1m\n", d2->semeai_attack_target);
  gfprintf(outfile, "strings                 ");
  for (w = first_worm_in_dragon(pos); w != NO_MOVE; w = next_worm_in_dragon(w))
    gfprintf(outfile, "%1m ", w);
  gfprintf(outfile, "\n");
}


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
