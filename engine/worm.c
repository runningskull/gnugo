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

#include "gnugo.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "liberty.h"
#include "patterns.h"

static void compute_effective_worm_sizes(void);
static void do_compute_effective_worm_sizes(int color,
					    int (*cw)[MAX_CLOSE_WORMS],
					    int *ncw, int max_distance);
static void compute_unconditional_status(void);
static void find_worm_attacks_and_defenses(void);
static void find_worm_threats(void);
static int find_lunch(int str, int *lunch);
static void change_tactical_point(int str, int move, int code,
				  int points[MAX_TACTICAL_POINTS],
				  int codes[MAX_TACTICAL_POINTS]);
static void propagate_worm2(int str);
static int genus(int str);
static void markcomponent(int str, int pos, int mg[BOARDMAX]);
static int examine_cavity(int pos, int *edge);
static void cavity_recurse(int pos, int mx[BOARDMAX], 
			   int *border_color, int *edge, int str);
static void ping_cave(int str, int *result1,  int *result2,
		      int *result3, int *result4);
static void ping_recurse(int pos, int *counter, 
			 int mx[BOARDMAX], 
			 int mr[BOARDMAX], int color);
static int touching(int pos, int color);
static void find_attack_patterns(void);
static void attack_callback(int anchor, int color,
			    struct pattern *pattern, int ll, void *data);
static void find_defense_patterns(void);
static void defense_callback(int anchor, int color,
			     struct pattern *pattern, int ll, void *data);
static void build_worms(void);

/* A STRING is a maximal connected set of stones of the same color, 
 * black or white. A WORM is the same thing as a string, except that
 * its color can be empty. An empty worm is called a CAVITY.
 *
 * Worms are eventually amalgamated into dragons. An empty dragon
 * is called a CAVE.
 */



/* make_worms() finds all worms and assembles some data about them.
 *
 * Each worm is marked with an origin.  This is an arbitrarily chosen
 * element of the worm, in practice the algorithm puts the origin at
 * the first element when they are given the lexicographical order,
 * though its location is irrelevant for applications. To see if two
 * stones lie in the same worm, compare their origins.
 *
 * We will use the field dragon[ii].genus to keep track of
 * black- or white-bordered cavities (essentially eyes) which are found.  
 * so this field must be zero'd now.
 */

void
make_worms(void)
{
  int pos;

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
  
  gg_assert(stackp == 0);

  /* Count liberties of different orders and initialize cutstone fields. */
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (IS_STONE(board[pos]) && is_worm_origin(pos, pos)) {
      int lib1, lib2, lib3, lib4;
      
      ping_cave(pos, &lib1, &lib2, &lib3, &lib4);
      gg_assert(worm[pos].liberties == lib1);
      worm[pos].liberties2 = lib2;
      worm[pos].liberties3 = lib3;
      worm[pos].liberties4 = lib4;
      worm[pos].cutstone = 0;
      worm[pos].cutstone2 = 0;
      propagate_worm(pos);
    }
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

  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    int w1 = NO_MOVE;
    int w2 = NO_MOVE;
    int k;
    int pos2;
    
    /* Only work on each worm once. This is easiest done if we only 
     * work with the origin of each worm.
     */
    if (!IS_STONE(board[pos]) || !is_worm_origin(pos, pos))
      continue;
    
    /* Try to find two adjacent worms (w1) and (w2) 
     * of opposite colour from (pos).
     */
    for (pos2 = BOARDMIN; pos2 < BOARDMAX; pos2++) {
      /* Work only with the opposite color from (pos). */
      if (board[pos2] != OTHER_COLOR(board[pos])) 
	continue;
      
      for (k = 0; k < 4; k++) {
	if (!ON_BOARD(pos2 + delta[k])
	    || worm[pos2 + delta[k]].origin != pos)
	  continue;
	
	ASSERT1(board[pos2 + delta[k]] == board[pos], pos);
	
	/* If we have not already found a worm which meets the criteria,
	 * store it into (w1), otherwise store it into (w2).
	 */
	if (w1 == NO_MOVE)
	  w1 = worm[pos2].origin;
	else if (!is_same_worm(pos2, w1))
	  w2 = worm[pos2].origin;
      }
    }
    
    /* 
     *  We now verify the definition of cutting stones. We have
     *  verified that the string at (pos) is adjacent to two enemy
     *  strings at (w1) and (w2). We need to know if these
     *  strings share a liberty.
     */
    
    /* Only do this if we really found something. */
    if (w2 != NO_MOVE) {
      worm[pos].cutstone = 2;
      if (count_common_libs(w1, w2) > 0)
	worm[pos].cutstone = 1;
      
      TRACE_WORMS("Worm at %1m has w1 %1m and w2 %1m, cutstone %d\n",
	    pos, w1, w2, worm[pos].cutstone);
    }
  }
  
  gg_assert(stackp == 0);
  
  /* Set the genus of all worms. */
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (IS_STONE(board[pos]) && is_worm_origin(pos, pos)) {
      worm[pos].genus = genus(pos);
      propagate_worm(pos);
    }
  }
  gg_assert(stackp == 0);

  /* We try first to resolve small semeais. */
  gg_assert(stackp == 0);

  /* Now we try to improve the values of worm.attack and worm.defend.
   * If we find that capturing the string at str also defends the
   * string at str2, or attacks it, then we add points of attack and
   * defense. We don't add attacking point for strings that can't be
   * defended.
   */
  {
    int color;
    int str;
    int moves_to_try[BOARDMAX];
    memset(moves_to_try, 0, sizeof(moves_to_try));

    /* Find which colors to try at what points. */
    for (str = BOARDMIN; str < BOARDMAX; str++) {
      if (IS_STONE(board[str]) && is_worm_origin(str, str)) {
	color = board[str];
	moves_to_try[worm[str].defense_points[0]] |= color;
	moves_to_try[worm[str].attack_points[0]] |= OTHER_COLOR(color);
      }
    }

    /* Loop over the board and over the colors and try the moves found
     * in the previous loop.
     */
    for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
      if (!ON_BOARD(pos))
	continue;

      for (color = WHITE; color <= BLACK; color++) {
	if (!(moves_to_try[pos] & color))
	  continue;
	
	/* Try to play color at pos and see what it leads to. */
	if (!trymove(pos, color, "make_worms", NO_MOVE, EMPTY, NO_MOVE))
	  continue;
	  
	/* We must read to the same depth that was used in the
	 * initial determination of worm.attack and worm.defend
	 * to avoid horizon effects. Since stackp has been
	 * incremented we must also increment depth values.
	 */
	
	TRACE_WORMS("trying %1m\n", pos);
	increase_depth_values();
	
	/* Now we try to find a group which is saved or attacked as well
	 * by this move.
	 */
	for (str = BOARDMIN; str < BOARDMAX; str++) {
	  if (!IS_STONE(board[str])
	      || !is_worm_origin(str, str))
	    continue;
	  
	  /* If the worm is of the opposite color to the move,
	   * then we try to defend it. If there was a previous 
	   * attack and defense of it, and there is no defense
	   * for the attack now...
	   */
	  if (worm[str].color == OTHER_COLOR(color)
	      && worm[str].attack_codes[0] != 0
	      && worm[str].defense_codes[0] != 0) {
	    int dcode = find_defense(str, NULL);
	    if (dcode < worm[str].defense_codes[0]) {
	      int attack_works = 1;
	      /* Sometimes find_defense() fails to find a
	       * defense which has been found by other means.
	       * Try if the old defense move still works.
	       */
	      if (worm[str].defense_codes[0] != 0
		  && trymove(worm[str].defense_points[0],
			     OTHER_COLOR(color), "make_worms", 0, EMPTY, 0)) {
		int this_dcode = REVERSE_RESULT(attack(str, NULL));
		if (this_dcode > dcode) {
		  dcode = this_dcode;
		  if (dcode >= worm[str].defense_codes[0])
		    attack_works = 0;
		}
		popgo();
	      }
	      
	      /* ...then add an attack point of that worm at pos. */
	      if (attack_works) {
		TRACE_WORMS(
		      "adding point of attack of %1m at %1m with code %d\n",
		      str, pos, REVERSE_RESULT(dcode));
		change_attack(str, pos, REVERSE_RESULT(dcode));
	      }
	    }
	  }
	  
	  /* If the worm is of the same color as the move we try to
	   * attack it. If there previously was an attack on it, but
	   * there is none now, then add a defense point of str at
	   * pos.
	   */
	  else if (worm[str].color == color
		   && worm[str].attack_codes[0] != 0) {
	    int acode = attack(str, NULL);
	    if (acode < worm[str].attack_codes[0]) {
	      int defense_works = 1;
	      /* Sometimes attack() fails to find an
	       * attack which has been found by other means.
	       * Try if the old attack move still works.
	       */
	      if (worm[str].attack_codes[0] != 0
		  && trymove(worm[str].attack_points[0],
			     OTHER_COLOR(color), "make_worms", 0, EMPTY, 0)) {
		int this_acode;
		if (board[str] == EMPTY)
		  this_acode = WIN;
		else
		  this_acode = REVERSE_RESULT(find_defense(str, NULL));
		if (this_acode > acode) {
		  acode = this_acode;
		  if (acode >= worm[str].attack_codes[0])
		    defense_works = 0;
		}
		popgo();
	      }
	      
	      /* ...then add an attack point of that worm at pos. */
	      if (defense_works) {
		TRACE_WORMS(
		      "adding point of defense of %1m at %1m with code %d\n",
		      str, pos, REVERSE_RESULT(acode));
		change_defense(str, pos, REVERSE_RESULT(acode));
	      }
	    }
	  }
	}
	decrease_depth_values();
	popgo();
      }
    }
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
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (IS_STONE(board[pos])
	&& IS_STONE(board[SOUTH(pos)])
	&& !is_same_worm(pos, SOUTH(pos))) {
      if (worm[pos].attack_codes[0] != 0
	  && worm[SOUTH(pos)].attack_codes[0] != 0) {
	if (worm[pos].defense_codes[0] == 0
	    && does_defend(worm[SOUTH(pos)].attack_points[0], pos)) {
	  /* FIXME: need to check ko relationship here */
	  change_defense(pos, worm[SOUTH(pos)].attack_points[0], WIN);
	}
	if (worm[SOUTH(pos)].defense_codes[0] == 0
	    && does_defend(worm[pos].attack_points[0], SOUTH(pos))) {
	  /* FIXME: need to check ko relationship here */	    
	  change_defense(SOUTH(pos), worm[pos].attack_points[0], WIN);
	}
      }
    }
  }
  
  /* Then look for horizontal neighbors. */
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (IS_STONE(board[pos])
	&& IS_STONE(board[EAST(pos)])
	&& !is_same_worm(pos, EAST(pos))) {
      if (worm[pos].attack_codes[0] != 0
	  && worm[EAST(pos)].attack_codes[0] != 0) {
	if (worm[pos].defense_codes[0] == 0
	    && does_defend(worm[EAST(pos)].attack_points[0], pos)) {
	  /* FIXME: need to check ko relationship here */	    
	  change_defense(pos, worm[EAST(pos)].attack_points[0], WIN);
	}
	if (worm[EAST(pos)].defense_codes[0] == 0
	    && does_defend(worm[pos].attack_points[0], EAST(pos))) {
	  /* FIXME: need to check ko relationship here */	    
	  change_defense(EAST(pos), worm[pos].attack_points[0], WIN);
	}
      }
    }
  }
  
  gg_assert(stackp == 0);
  
  /* Find adjacent worms that can be easily captured, aka lunches. */

  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    int lunch;
    
    if (!IS_STONE(board[pos]) || !is_worm_origin(pos, pos))
      continue;
    
    if (find_lunch(pos, &lunch)
	&& (worm[lunch].attack_codes[0] == WIN
	    || worm[lunch].attack_codes[0] == KO_A)) {
      TRACE_WORMS("lunch found for %1m at %1m\n", pos, lunch);
      worm[pos].lunch = lunch;
    }
    else
      worm[pos].lunch = NO_MOVE;
    
    propagate_worm(pos);
  }
  
  if (!disable_threat_computation)
    find_worm_threats();

  /* Identify INESSENTIAL strings.
   *
   * These are defined as surrounded strings which have no life
   * potential unless part of their surrounding chain can be captured.
   * We give a conservative definition of inessential:
   *  - the genus must be zero 
   *  - there can no second order liberties
   *  - there can be no more than two edge liberties
   *  - if it is removed from the board, the remaining cavity has
   *    border color the opposite color of the string 
   *  - it contains at most two edge vertices.
   *
   * If we get serious about identifying seki, we might want to add:
   *
   *  - if it has fewer than 4 liberties it is tactically dead.
   *
   * The last condition is helpful in excluding strings which are
   * alive in seki.
   *
   * An inessential string can be thought of as residing inside the
   * opponent's eye space.
   */

  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (IS_STONE(board[pos])
	&& worm[pos].origin == pos
	&& worm[pos].genus == 0
	&& worm[pos].liberties2 == 0
	&& !worm[pos].cutstone
	&& worm[pos].lunch == NO_MOVE) {
      int edge;
      int border_color = examine_cavity(pos, &edge);
      if (border_color != GRAY_BORDER && edge < 3) {
	TRACE_WORMS("Worm %1m identified as inessential.\n", pos);
	worm[pos].inessential = 1;
	propagate_worm(pos);
      }
    }
  }
}


/* 
 * Clear all worms and initialize the basic data fields:
 *   color, origin, size, liberties
 * This is a substep of make_worms().
 */

static void
build_worms()
{
  int pos;

  /* Set all worm data fields to 0. */
  memset(worm, 0 , sizeof(worm));

  /* Initialize the worm data for each worm. */
  for (pos = BOARDMIN; pos < BOARDMAX; pos++)
    if (ON_BOARD(pos))
      worm[pos].origin = NO_MOVE;
  
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (!ON_BOARD(pos) || worm[pos].origin != NO_MOVE)
      continue;
    worm[pos].color = board[pos];
    worm[pos].origin = pos;
    worm[pos].inessential = 0;
    worm[pos].invincible = 0;
    worm[pos].unconditional_status = UNKNOWN;
    worm[pos].effective_size = 0.0;
    if (IS_STONE(board[pos])) {
      worm[pos].liberties = countlib(pos);
      worm[pos].size = countstones(pos);
      propagate_worm(pos);
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
 *
 * This function is also used to compute arrays with information about
 * the distances to worms of both or either color. In the latter case
 * we count intersections up to a distance of 5.
 */

static void
compute_effective_worm_sizes()
{
  do_compute_effective_worm_sizes(BLACK | WHITE, close_worms,
				  number_close_worms, 3);
  do_compute_effective_worm_sizes(BLACK, close_black_worms,
				  number_close_black_worms, 5);
  do_compute_effective_worm_sizes(WHITE, close_white_worms,
				  number_close_white_worms, 5);
}

static void
do_compute_effective_worm_sizes(int color, int (*cw)[MAX_CLOSE_WORMS],
				int *ncw, int max_distance)
{
  int pos;

  /* Distance to closest worm, -1 means unassigned, 0 that there is
   * a stone at the location, 1 a liberty of a stone, and so on.
   */
  int distance[BOARDMAX];
  /* Pointer to the origin of the closest worms. A very large number of
   * worms may potentially be equally close, but no more than
   * 2*(board_size-1).
   */
  static int worms[BOARDMAX][2*(MAX_BOARD-1)];
  int nworms[BOARDMAX];   /* number of equally close worms */
  int found_one;
  int dist; /* current distance */
  int k, l;
  int r;
    
  /* Initialize arrays. */
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (!ON_BOARD(pos))
      continue;

    for (k = 0; k < 2*(board_size-1); k++)
      worms[pos][k] = NO_MOVE;
    
    nworms[pos] = 0;
    
    if (board[pos] & color) {
      distance[pos] = 0;
      worms[pos][0] = worm[pos].origin;
      nworms[pos]++;
    }
    else
      distance[pos] = -1;
  }
  
  dist = 0;
  found_one = 1;
  while (found_one && dist <= max_distance) {
    found_one = 0;
    dist++;
    for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
      if (!ON_BOARD(pos) || distance[pos] != -1)
	continue; /* already claimed */

      for (r = 0; r < 4; r++) {
	int pos2 = pos + delta[r];
	
	if (ON_BOARD(pos2) && distance[pos2] == dist - 1) {
	  found_one = 1;
	  distance[pos] = dist;
	  for (k = 0; k < nworms[pos2]; k++) {
	    int already_counted = 0;
	    for (l = 0; l < nworms[pos]; l++)
	      if (worms[pos][l] == worms[pos2][k]) {
		already_counted = 1;
		break;
	      }
	    if (!already_counted) {
	      gg_assert(nworms[pos] < 2*(board_size-1));
	      worms[pos][nworms[pos]] = worms[pos2][k];
	      nworms[pos]++;
	    }
	  }
	}
      }
    }
  }

  /* Compute the effective sizes but only when all worms are considered. */
  if (color == (BLACK | WHITE)) {
    /* Distribute (fractional) contributions to the worms. */
    for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
      if (!ON_BOARD(pos))
	continue;
      
      for (k = 0; k < nworms[pos]; k++) {
	int w = worms[pos][k];
	if (board[pos] == EMPTY)
	  worm[w].effective_size += 0.5/nworms[pos];
	else
	  worm[w].effective_size += 1.0;
      }
    }
    
    /* Propagate the effective size values all over the worms. */
    for (pos = BOARDMIN; pos < BOARDMAX; pos++)
      if (IS_STONE(board[pos]) && is_worm_origin(pos, pos))
	propagate_worm(pos);
  }

  /* Fill in the appropriate close_*_worms (cw) and
   * number_close_*_worms (ncw) arrays.
   */
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (!ON_BOARD(pos))
      continue;

    if (nworms[pos] > MAX_CLOSE_WORMS)
      ncw[pos] = 0;
    else
      ncw[pos] = nworms[pos];

    for (k = 0; k < ncw[pos]; k++)
      cw[pos][k] = worms[pos][k];
  }
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
  int unconditional_territory[BOARDMAX];
  int pos;
  int color;
  int other;
  
  for (color = WHITE; color <= BLACK; color++) {
    other = OTHER_COLOR(color);
    unconditional_life(unconditional_territory, color);

    for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
      if (!ON_BOARD(pos) || !unconditional_territory[pos])
	continue;
	
      if (board[pos] == color) {
	worm[pos].unconditional_status = ALIVE;
	if (unconditional_territory[pos] == 1)
	  worm[pos].invincible = 1;
      }
      else if (board[pos] == EMPTY) {
	if (color == WHITE)
	  worm[pos].unconditional_status = WHITE_BORDER;
	else
	  worm[pos].unconditional_status = BLACK_BORDER;
      }
      else
	worm[pos].unconditional_status = DEAD;
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
  int str;
  int k;
  int acode, dcode;
  int attack_point;
  int defense_point;
  static int libs[MAXLIBS];
  int liberties;
  int color;
  int other;

   /* 1. Start with finding attack points. */
  for (str = BOARDMIN; str < BOARDMAX; str++) {
    if (!IS_STONE(board[str]) || !is_worm_origin(str, str))
      continue;

    TRACE("considering attack of %1m\n", str);
    /* Initialize all relevant fields at once. */
    for (k = 0; k < MAX_TACTICAL_POINTS; k++) {
      worm[str].attack_codes[k]   = 0;
      worm[str].attack_points[k]  = 0;
      worm[str].defense_codes[k]  = 0;
      worm[str].defense_points[k] = 0;
    }
    propagate_worm(str);
    
    acode = attack(str, &attack_point);
    if (acode != 0) {
      TRACE_WORMS("worm at %1m can be attacked at %1m\n",
	    str, attack_point);
      change_attack(str, attack_point, acode);
    }
  }
  gg_assert(stackp == 0);
  
  /* 2. Use pattern matching to find a few more attacks. */
  find_attack_patterns();
  gg_assert(stackp == 0);
  
  /* 3. Now find defense moves. */
  for (str = BOARDMIN; str < BOARDMAX; str++) {
    if (!IS_STONE(board[str]) || !is_worm_origin(str, str))
      continue;

    if (worm[str].attack_codes[0] != 0) {

      TRACE("considering defense of %1m\n", str);
      dcode = find_defense(str, &defense_point);
      if (dcode != 0) {
	TRACE("worm at %1m can be defended at %1m\n", str, defense_point);
	if (defense_point != NO_MOVE)
	  change_defense(str, defense_point, dcode);
      }
      else {
	/* If the point of attack is not adjacent to the worm, 
	 * it is possible that this is an overlooked point of
	 * defense, so we try and see if it defends.
	 */
	attack_point = worm[str].attack_points[0];
	if (!liberty_of_string(attack_point, str))
	  if (trymove(attack_point, worm[str].color, "make_worms", NO_MOVE,
		      EMPTY, NO_MOVE)) {
	    int acode = attack(str, NULL);
	    if (acode != WIN) {
	      change_defense(str, attack_point, REVERSE_RESULT(acode));
	      TRACE("worm at %1m can be defended at %1m with code %d\n",
		    str, attack_point, REVERSE_RESULT(acode));
	    }	 
	    popgo();
	  }
      }
    }
  }
  gg_assert(stackp == 0);

  /* 4. Use pattern matching to find a few more defense moves. */
  find_defense_patterns();
  gg_assert(stackp == 0);
  
  /*
   * 5. Find additional attacks and defenses by testing all immediate
   *    liberties. Further attacks and defenses are found by pattern
   *    matching and by trying whether each attack or defense point
   *    attacks or defends other strings.
   */
  for (str = BOARDMIN; str < BOARDMAX; str++) {
    color = board[str];
    if (!IS_STONE(color) || !is_worm_origin(str, str))
      continue;
    
    other = OTHER_COLOR(color);
    
    if (worm[str].attack_codes[0] == 0)
      continue;
    
    /* There is at least one attack on this group. Try the
     * liberties.
     */
    liberties = findlib(str, MAXLIBS, libs);
    
    for (k = 0; k < liberties; k++) {
      int pos = libs[k];
      if (!attack_move_known(pos, str)) {
	/* Try to attack on the liberty. */
	if (trymove(pos, other, "make_worms", str, EMPTY, NO_MOVE)) {
	  if (board[str] == EMPTY || attack(str, NULL)) {
	    if (board[str] == EMPTY)
	      dcode = 0;
	    else
	      dcode = find_defense(str, NULL);
	    
	    if (dcode != WIN)
	      change_attack(str, pos, REVERSE_RESULT(dcode));
	  }
	  popgo();
	}
      }
      /* Try to defend at the liberty. */
      if (!defense_move_known(pos, str)) {
	if (worm[str].defense_codes[0] != 0)
	  if (trymove(pos, color, "make_worms", NO_MOVE, EMPTY, NO_MOVE)) {
	    acode = attack(str, NULL);
	    if (acode != WIN)
	      change_defense(str, pos, REVERSE_RESULT(acode));
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
  int str;
  static int libs[MAXLIBS];
  int liberties;
  
  int k;
  int l;
  int color;
  int other;
  
  for (str = BOARDMIN; str < BOARDMAX; str++) {
    color = board[str];
    if (!IS_STONE(color) || !is_worm_origin(str, str))
      continue;

    other = OTHER_COLOR(color);

    /* 1. Start with finding attack threats. */
    /* Only try those worms that have no attack. */
    if (worm[str].attack_codes[0] == 0) {
      attack_threats(str, MAX_TACTICAL_POINTS,
		     worm[str].attack_threat_points,
		     worm[str].attack_threat_codes);
      /* FIXME: Try other moves also (patterns?). */
    }
    
    /* 2. Continue with finding defense threats. */
    /* Only try those worms that have an attack. */
    if (worm[str].attack_codes[0] != 0
	&& worm[str].defense_codes[0] == 0) {
      
      liberties = findlib(str, MAXLIBS, libs);
      
      for (k = 0; k < liberties; k++) {
	int aa = libs[k];
	
	/* Try to threaten on the liberty. */
	if (trymove(aa, color, "threaten defense", NO_MOVE, EMPTY, NO_MOVE)) {
	  if (attack(str, NULL) == WIN) {
	    int dcode = find_defense(str, NULL);
	    if (dcode != 0)
	      change_defense_threat(str, aa, dcode);
	  }
	  popgo();
	}
	
	/* Try to threaten on second order liberties. */
	for (l = 0; l < 4; l++) {
	  int bb = libs[k] + delta[l];
	  
	  if (!ON_BOARD(bb)
	      || IS_STONE(board[bb])
	      || liberty_of_string(bb, str))
	    continue;
	  
	  if (trymove(bb, color, "threaten defense", str, EMPTY, NO_MOVE)) {
	    if (attack(str, NULL) == WIN) {
	      int dcode = find_defense(str, NULL);
	      if (dcode != 0)
		change_defense_threat(str, bb, dcode);
	    }
	    popgo();
	  }
	}
      }
      
      /* It might be interesting to look for defense threats by
       * attacking weak neighbors, similar to threatening attack by
       * defending a weak neighbor. However, in this case it seems
       * probable that if there is such an attack, it's a real
       * defense, not only a threat. 
       */
      
      /* FIXME: Try other moves also (patterns?). */
    }
  }
}


/* find_lunch(str, &worm) looks for a worm adjoining the
 * string at (str) which can be easily captured. Whether or not it can
 * be defended doesn't matter.
 *
 * Returns the location of the string in (*lunch).
 */
	
static int
find_lunch(int str, int *lunch)
{
  int pos;
  int k;

  ASSERT1(IS_STONE(board[str]), str);
  ASSERT1(stackp == 0, str);

  *lunch = NO_MOVE;
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (board[pos] != OTHER_COLOR(board[str]))
      continue;
    for (k = 0; k < 8; k++) {
      int apos = pos + delta[k];
      if (ON_BOARD(apos) && is_same_worm(apos, str)) {
	if (worm[pos].attack_codes[0] != 0 && !is_ko_point(pos)) {
	  /*
	   * If several adjacent lunches are found, we pick the 
	   * juiciest. First maximize cutstone, then minimize liberties. 
	   * We can only do this if the worm data is available, 
	   * i.e. if stackp==0.
	   */
	  if (*lunch == NO_MOVE
	      || worm[pos].cutstone > worm[*lunch].cutstone 
	      || (worm[pos].cutstone == worm[*lunch].cutstone 
		  && worm[pos].liberties < worm[*lunch].liberties)) {
	    *lunch = worm[pos].origin;
	  }
	}
	break;
      }
    }
  }
  
  if (*lunch != NO_MOVE)
    return 1;

  return 0;
}


/*
 * Test whether two worms are the same. Used by autohelpers.
 * Before this function can be called, build_worms must have been run.
 */

int
is_same_worm(int w1, int w2)
{
  return worm[w1].origin == worm[w2].origin;
}


/*
 * Test whether the origin of the worm at (w) is (pos).
 */

int
is_worm_origin(int w, int pos)
{
  return worm[w].origin == pos;
}


/* 
 * change_defense(str, move, dcode) is used to add and remove defense
 * points. It can also be used to change the defense code. The meaning
 * of the call is that the string (str) can be defended by (move) with
 * defense code (dcode). If (dcode) is zero, the move is removed from
 * the list of defense moves if it was previously listed.
 */

void
change_defense(int str, int move, int dcode)
{
  str = worm[str].origin;
  change_tactical_point(str, move, dcode,
			worm[str].defense_points, worm[str].defense_codes);
}


/* 
 * change_attack(str, move, acode) is used to add and remove attack
 * points. It can also be used to change the attack code. The meaning
 * of the call is that the string (str) can be attacked by (move) with
 * attack code (acode). If (acode) is zero, the move is removed from
 * the list of attack moves if it was previously listed.
 */

void
change_attack(int str, int move, int acode)
{
  str = worm[str].origin;
  TRACE_WORMS("change_attack: %1m %1m %d\n", str, move, acode);
  change_tactical_point(str, move, acode,
			worm[str].attack_points, worm[str].attack_codes);
}


/* 
 * change_defense_threat(str, move, dcode) is used to add and remove
 * defense threat points. It can also be used to change the defense
 * threat code. The meaning of the call is that the string (str) can
 * threaten to be defended by (move) with defense threat code (dcode).
 * If (dcode) is zero, the move is removed from the list of defense
 * threat moves if it was previously listed.
 */

void
change_defense_threat(int str, int move, int dcode)
{
  str = worm[str].origin;
  change_tactical_point(str, move, dcode,
			worm[str].defense_threat_points,
			worm[str].defense_threat_codes);
}


/* 
 * change_attack_threat(str, move, acode) is used to add and remove
 * attack threat points. It can also be used to change the attack
 * threat code. The meaning of the call is that the string (str) can
 * threaten to be attacked by (move) with attack threat code (acode).
 * If (acode) is zero, the move is removed from the list of attack
 * threat moves if it was previously listed.
 */

void
change_attack_threat(int str, int move, int acode)
{
  str = worm[str].origin;
  change_tactical_point(str, move, acode,
			worm[str].attack_threat_points,
			worm[str].attack_threat_codes);
}


/* Check whether (move) is listed as an attack point for (str) and
 * return the attack code. If (move) is not listed, return 0.
 */
int
attack_move_known(int move, int str)
{
  return movelist_move_known(move, MAX_TACTICAL_POINTS,
			     worm[str].attack_points,
			     worm[str].attack_codes);
}

/* Check whether (move) is listed as a defense point for (str) and
 * return the defense code. If (move) is not listed, return 0.
 */
int
defense_move_known(int move, int str)
{
  return movelist_move_known(move, MAX_TACTICAL_POINTS,
			     worm[str].defense_points,
			     worm[str].defense_codes);
}

/* Check whether (move) is listed as an attack threat point for (str)
 * and return the attack threat code. If (move) is not listed, return
 * 0.
 */
int
attack_threat_move_known(int move, int str)
{
  return movelist_move_known(move, MAX_TACTICAL_POINTS,
			     worm[str].attack_threat_points,
			     worm[str].attack_threat_codes);
}

/* Check whether (move) is listed as a defense threat point for (str)
 * and return the defense threat code. If (move) is not listed, return
 * 0.
 */
int
defense_threat_move_known(int move, int str)
{
  return movelist_move_known(move, MAX_TACTICAL_POINTS,
			     worm[str].defense_threat_points,
			     worm[str].defense_threat_codes);
}


/*
 * This function does the real work for change_attack(),
 * change_defense(), change_attack_threat(), and
 * change_defense_threat().
 */

static void
change_tactical_point(int str, int move, int code,
		      int points[MAX_TACTICAL_POINTS],
		      int codes[MAX_TACTICAL_POINTS])
{
  gg_assert(ON_BOARD(str));
  gg_assert(str == worm[str].origin);
  
  movelist_change_point(move, code, MAX_TACTICAL_POINTS, points, codes);
  propagate_worm2(str);
}


/* 
 * propagate_worm() takes the worm data at one stone and copies it to 
 * the remaining members of the worm.
 *
 * Even though we don't need to copy all the fields, it's probably
 * better to do a structure copy which should compile to a block copy.
 */

void 
propagate_worm(int pos)
{
  int k;
  int num_stones;
  int stones[MAX_BOARD * MAX_BOARD];
  gg_assert(stackp == 0);
  ASSERT1(IS_STONE(board[pos]), pos);

  num_stones = findstones(pos, MAX_BOARD * MAX_BOARD, stones);
  for (k = 0; k < num_stones; k++)
    if (stones[k] != pos)
      worm[stones[k]] = worm[pos];
}


/* 
 * propagate_worm2() is a relative to propagate_worm() which can be
 * used when stack>0 but not for the initial construction of the
 * worms.
 */

static void 
propagate_worm2(int str)
{
  int pos;
  ASSERT_ON_BOARD1(str);
  ASSERT1(IS_STONE(worm[str].color), str);

  for (pos = BOARDMIN; pos < BOARDMAX; pos++)
    if (board[pos] == board[str] && is_same_worm(pos, str))
      worm[pos] = worm[str];
}


/* Report all known attack, defense, attack threat, and defense threat
 * moves. But limit this to the moves which can be made by (color).
 */
void
worm_reasons(int color)
{
  int pos;
  int k;
  
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (!ON_BOARD(pos) || board[pos] == EMPTY)
      continue;

    if (!is_worm_origin(pos, pos))
      continue;

    if (board[pos] == OTHER_COLOR(color)) {
      for (k = 0; k < MAX_TACTICAL_POINTS; k++) {
	if (worm[pos].attack_codes[k] != 0)
	  add_attack_move(worm[pos].attack_points[k], pos,
			  worm[pos].attack_codes[k]);
	if (worm[pos].attack_threat_codes[k] != 0)
	  add_attack_threat_move(worm[pos].attack_threat_points[k], pos,
				 worm[pos].attack_threat_codes[k]);
      }
    }
      
    if (board[pos] == color) {
      for (k = 0; k < MAX_TACTICAL_POINTS; k++) {
	if (worm[pos].defense_codes[k] != 0)
	  add_defense_move(worm[pos].defense_points[k], pos,
			   worm[pos].defense_codes[k]);

	if (worm[pos].defense_threat_codes[k] != 0)
	  add_defense_threat_move(worm[pos].defense_threat_points[k], pos,
				  worm[pos].defense_threat_codes[k]);
      }
    }
  }
}


/* ping_cave(str, *lib1, ...) is applied when (str) points to a string.
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
ping_cave(int str, int *lib1, int *lib2, int *lib3, int *lib4)
{
  int pos;
  int k;
  int libs[MAXLIBS];
  int mrc[BOARDMAX];
  int mse[BOARDMAX];
  int color = board[str];
  int other = OTHER_COLOR(color);

  memset(mse, 0, sizeof(mse));

  /* Find and mark the first order liberties. */
  *lib1 = findlib(str, MAXLIBS, libs);
  for (k = 0; k < *lib1; k++)
    mse[libs[k]] = 1;

  /* Reset mse at liberties which are flanked by two stones of the
   * opposite color, or one stone and the edge.
   */

  for (pos = BOARDMIN; pos < BOARDMAX; pos++)
    if (ON_BOARD(pos)
	&& mse[pos]
	&& (((      !ON_BOARD(SOUTH(pos)) || board[SOUTH(pos)] == other)
	     && (   !ON_BOARD(NORTH(pos)) || board[NORTH(pos)] == other))
	    || ((   !ON_BOARD(WEST(pos))  || board[WEST(pos)]  == other)
		&& (!ON_BOARD(EAST(pos))  || board[EAST(pos)]  == other))))
      mse[pos] = 0;
  
  *lib2 = 0;
  memset(mrc, 0, sizeof(mrc));
  ping_recurse(str, lib2, mse, mrc, color);

  *lib3 = 0;
  memset(mrc, 0, sizeof(mrc));
  ping_recurse(str, lib3, mse, mrc, color);

  *lib4 = 0;
  memset(mrc, 0, sizeof(mrc));
  ping_recurse(str, lib4, mse, mrc, color);
}


/* recursive function called by ping_cave */

static void 
ping_recurse(int pos, int *counter,
	     int mx[BOARDMAX], int mr[BOARDMAX],
	     int color)
{
  int k;
  mr[pos] = 1;

  for (k = 0; k < 4; k++) {
    int apos = pos + delta[k];
    if (board[apos] == EMPTY
	&& mx[apos] == 0
	&& mr[apos] == 0
	&& !touching(apos, OTHER_COLOR(color))) {
      (*counter)++;
      mr[apos] = 1;
      mx[apos] = 1;
    }
  }
  
  if (!is_ko_point(pos)) {
    for (k = 0; k < 4; k++) {
      int apos = pos + delta[k];
      if (ON_BOARD(apos)
	  && mr[apos] == 0
	  && (mx[apos] == 1
	      || board[apos] == color))
	ping_recurse(apos, counter, mx, mr, color);
    }
  }
}


/* touching(pos, color) returns true if the vertex at (pos) is
 * touching any stone of (color).
 */

static int
touching(int pos, int color)
{
  return (board[SOUTH(pos)] == color
	  || board[WEST(pos)] == color
	  || board[NORTH(pos)] == color
	  || board[EAST(pos)] == color);
}


/* The GENUS of a string is the number of connected components of
 * its complement, minus one. It is an approximation to the number of
 * eyes of the string.
 */

static int 
genus(int str)
{
  int pos;
  int mg[BOARDMAX];
  int gen = -1;

  memset(mg, 0, sizeof(mg));
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (ON_BOARD(pos)
	&& !mg[pos]
	&& (board[pos] == EMPTY || !is_same_worm(pos, str))) {
      markcomponent(str, pos, mg);
      gen++;
    }
  }

  return gen;
}


/* This recursive function marks the component at (pos) of 
 * the complement of the string with origin (str)
 */

static void 
markcomponent(int str, int pos, int mg[BOARDMAX])
{
  int k;
  mg[pos] = 1;
  for (k = 0; k < 4; k++) {
    int apos = pos + delta[k];
    if (ON_BOARD(apos)
	&& mg[apos] == 0
	&& (board[apos] == EMPTY || !is_same_worm(apos, str)))
      markcomponent(str, apos, mg);
  }
}


/* examine_cavity(pos, *edge), if (pos) is EMPTY, examines the
 * cavity at (m, n) and returns its bordercolor,
 * which can be BLACK_BORDER, WHITE_BORDER or GRAY_BORDER. The edge
 * parameter is set to the number of edge vertices in the cavity.
 *
 * If (pos) is nonempty, it returns the same result, imagining
 * that the string at (pos) is removed. The edge parameter is
 * set to the number of vertices where the cavity meets the
 * edge in a point outside the removed string.  
 */

static int
examine_cavity(int pos, int *edge)
{
  int border_color = EMPTY;
  int ml[BOARDMAX];
  int origin = NO_MOVE;
  
  ASSERT_ON_BOARD1(pos);
  gg_assert(edge != NULL);
  
  memset(ml, 0, sizeof(ml));

  *edge = 0;

  if (IS_STONE(board[pos]))
    origin = find_origin(pos);
  
  cavity_recurse(pos, ml, &border_color, edge, origin);

  if (border_color == (BLACK | WHITE))
    return GRAY_BORDER;  
  if (border_color == BLACK)
    return BLACK_BORDER;
  if (border_color == WHITE)
    return WHITE_BORDER;

  /* We should have returned now, unless the board is completely empty.
   * Verify that this is the case and then return GRAY_BORDER.
   *
   * Notice that the board appears completely empty if there's only a
   * single string and pos points to it.
   */
  gg_assert(border_color == EMPTY
	    && ((pos == NO_MOVE
		 && stones_on_board(BLACK | WHITE) == 0)
		|| (pos != NO_MOVE
		    && stones_on_board(BLACK | WHITE) == countstones(pos))));
  
  return GRAY_BORDER;
}


/* helper function for examine_cavity.
 * border_color contains information so far : transitions allowed are
 *   EMPTY       -> BLACK/WHITE
 *   BLACK/WHITE -> BLACK | WHITE
 *
 * mx[pos] is 1 if (pos) has already been visited.
 *
 * if (str) points to the origin of a string, it will be ignored.
 *
 * On (fully-unwound) exit
 *   *border_color should be BLACK, WHITE or BLACK | WHITE
 *   *edge is the count of edge pieces
 *
 * *border_color should be EMPTY if and only if the board
 * is completely empty or only contains the ignored string.
 */

static void 
cavity_recurse(int pos, int mx[BOARDMAX], 
	       int *border_color, int *edge, int str)
{
  int k;
  ASSERT1(mx[pos] == 0, pos);

  mx[pos] = 1;

  if (is_edge_vertex(pos) && board[pos] == EMPTY) 
    (*edge)++;

  /* Loop over the four neighbors. */
  for (k = 0; k < 4; k++) {
    int apos = pos + delta[k];
    if (ON_BOARD(apos) && !mx[apos]) {
      int neighbor_empty = 0;
      
      if (board[apos] == EMPTY)
	neighbor_empty = 1;
      else {
	/* Count the neighbor as empty if it is part of the (ai, aj) string. */
	if (str == find_origin(apos))
	  neighbor_empty = 1;
	else
	  neighbor_empty = 0;
      }
      
      if (!neighbor_empty)
	*border_color |= board[apos];
      else
	cavity_recurse(apos, mx, border_color, edge, str);
    }
  }
}


/* Find attacking moves by pattern matching, for both colors. */
static void
find_attack_patterns(void)
{
  matchpat(attack_callback, ANCHOR_OTHER, &attpat_db, NULL, NULL);
}

/* Try to attack every X string in the pattern, whether there is an attack
 * before or not. Only exclude already known attacking moves.
 */
static void
attack_callback(int anchor, int color, struct pattern *pattern, int ll,
		void *data)
{
  int move;
  int k;
  UNUSED(data);

  move = AFFINE_TRANSFORM(pattern->move_offset, ll, anchor);

  /* If the pattern has a constraint, call the autohelper to see
   * if the pattern must be rejected.
   */
  if (pattern->autohelper_flag & HAVE_CONSTRAINT) {
    if (!pattern->autohelper(ll, move, color, 0))
      return;
  }

  /* If the pattern has a helper, call it to see if the pattern must
   * be rejected.
   */
  if (pattern->helper) {
    if (!pattern->helper(pattern, ll, move, color)) {
      TRACE_WORMS(
	    "Attack pattern %s+%d rejected by helper at %1m\n",
	    pattern->name, ll, move);
      return;
    }
  }

  /* Loop through pattern elements in search of X strings to attack. */
  for (k = 0; k < pattern->patlen; ++k) { /* match each point */
    if (pattern->patn[k].att == ATT_X) {
      /* transform pattern real coordinate */
      int pos = AFFINE_TRANSFORM(pattern->patn[k].offset, ll, anchor);

      int str = worm[pos].origin;

      /* A string with 5 liberties or more is considered tactically alive. */
      if (countlib(str) > 4)
	continue;

      if (attack_move_known(move, str))
	continue;

      /* No defenses are known at this time, so defend_code is always 0. */
      
      /* FIXME: Don't attack the same string more than once.
       * Play (ti, tj) and see if there is a defense.
       */
      if (trymove(move, color, "attack_callback", str, EMPTY, NO_MOVE)) {
	int dcode;
	if (!board[str])
	  dcode = 0;
	else if (!attack(str, NULL))
	  dcode = WIN;
	else
	  dcode = find_defense(str, NULL);

	popgo();

	/* Do not pick up suboptimal attacks at this time. Since we
         * don't know whether the string can be defended it's quite
         * possible that it only has a ko defense and then we would
         * risk to find an irrelevant move to attack with ko.
	 */
	if (dcode != WIN && REVERSE_RESULT(dcode) >= worm[str].attack_codes[0]) {
	  change_attack(str, move, REVERSE_RESULT(dcode));
	  TRACE_WORMS(
		"Attack pattern %s+%d found attack on %1m at %1m with code %d\n",
		pattern->name, ll, str, move, REVERSE_RESULT(dcode));
	}
      }
    }
  }
}

static void
find_defense_patterns(void)
{
  matchpat(defense_callback, ANCHOR_COLOR, &defpat_db, NULL, NULL);
}

static void
defense_callback(int anchor, int color, struct pattern *pattern, int ll,
		 void *data)
{
  int move;
  int k;
  UNUSED(data);

  move = AFFINE_TRANSFORM(pattern->move_offset, ll, anchor);
  
  /* If the pattern has a constraint, call the autohelper to see
   * if the pattern must be rejected.
   */
  if (pattern->autohelper_flag & HAVE_CONSTRAINT) {
    if (!pattern->autohelper(ll, move, color, 0))
      return;
  }

  /* If the pattern has a helper, call it to see if the pattern must
   * be rejected.
   */
  if (pattern->helper) {
    if (!pattern->helper(pattern, ll, move, color)) {
      TRACE_WORMS(
	    "Defense pattern %s+%d rejected by helper at %1m\n",
	    pattern->name, ll, move);
      return;
    }
  }

  /* Loop through pattern elements in search for O strings to defend. */
  for (k = 0; k < pattern->patlen; ++k) { /* match each point */
    if (pattern->patn[k].att == ATT_O) {
      /* transform pattern real coordinate */
      int pos = AFFINE_TRANSFORM(pattern->patn[k].offset, ll, anchor);
      int str = worm[pos].origin;

      if (worm[str].attack_codes[0] == 0
	  || defense_move_known(move, str))
	continue;
      
      /* FIXME: Don't try to defend the same string more than once.
       * FIXME: For all attacks on this string, we should test whether
       *        the proposed move happens to refute the attack.
       * Play (move) and see if there is an attack.
       */
      if (trymove(move, color, "defense_callback", str, EMPTY, NO_MOVE)) {
	int acode = attack(str, NULL);

	popgo();
	
	if (acode < worm[str].attack_codes[0]) {
	  change_defense(str, move, REVERSE_RESULT(acode));
	  TRACE_WORMS(
		"Defense pattern %s+%d found defense of %1m at %1m with code %d\n",
		pattern->name, ll, str, move, REVERSE_RESULT(acode));
	}
      }
    }
  }
}

void
get_lively_stones(int color, char safe_stones[BOARDMAX])
{
  int ii;
  memset(safe_stones, 0, BOARDMAX * sizeof(*safe_stones));
  for (ii = BOARDMIN; ii < BOARDMAX; ii++)
    ASSERT1(safe_stones[ii] == 0, ii);
  for (ii = BOARDMIN; ii < BOARDMAX; ii++)
    if (IS_STONE(board[ii])
	&& worm[ii].origin == ii) {
      if (worm[ii].attack_codes[0] == 0
	  || (board[ii] == color
	      && worm[ii].defense_codes[0] != 0))
	mark_string(ii, safe_stones, 1);
    }
}


void
compute_worm_influence()
{
  char safe_stones[BOARDMAX];

  get_lively_stones(BLACK, safe_stones);
  compute_influence(BLACK, safe_stones, NULL, &initial_black_influence,
      		    NO_MOVE, "initial black influence");
  get_lively_stones(WHITE, safe_stones);
  compute_influence(WHITE, safe_stones, NULL, &initial_white_influence,
      		    NO_MOVE, "initial white influence");
}

/* ================================================================ */
/*                      Debugger functions                          */
/* ================================================================ */

/* For use in gdb, print details of the worm at (m, n). 
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
  int pos = POS(m, n);
  int i;

  if (board[pos] == EMPTY) {
    gprintf("There is no worm at %1m\n", pos);
    return;
  }

  gprintf("*** worm at %1m:\n", pos);
  gprintf("color: %s; origin: %1m; size: %d; effective size: %f\n",
	  (worm[pos].color == WHITE) ? "White" : "Black",
	  worm[pos].origin, worm[pos].size, worm[pos].effective_size);

  gprintf("liberties: %d order 2 liberties:%d order 3:%d order 4:%d\n",
	  worm[pos].liberties, 
	  worm[pos].liberties2, 
	  worm[pos].liberties3, 
	  worm[pos].liberties4);

  /* List all attack points. */
  if (worm[pos].attack_points[0] == NO_MOVE)
    gprintf("no attack point, ");
  else {
    gprintf("attack point(s):");
    i = 0;
    while (worm[pos].attack_points[i] != NO_MOVE) {
      if (i > 0)
	gprintf(",");
      gprintf(" %1m: %s", worm[pos].attack_points[i],
	      result_to_string(worm[pos].attack_codes[i]));
      i++;
    }
    gprintf("\n;");
  }

  /* List all defense points. */
  if (worm[pos].defense_points[0] == NO_MOVE)
    gprintf("no defense point, ");
  else {
    gprintf("defense point(s):");
    i = 0;
    while (worm[pos].defense_points[i] != NO_MOVE) {
      if (i > 0)
	gprintf(",");
      gprintf(" %1m: %s", worm[pos].defense_points[i],
	      result_to_string(worm[pos].defense_codes[i]));
      i++;
    }
    gprintf("\n;");
  }

  /* List all attack threat points. */
  if (worm[pos].attack_threat_points[0] == NO_MOVE)
    gprintf("no attack threat point, ");
  else {
    gprintf("attack threat point(s):");
    i = 0;
    while (worm[pos].attack_threat_points[i] != NO_MOVE) {
      if (i > 0)
	gprintf(",");
      gprintf(" %1m: %s", worm[pos].attack_threat_points[i],
	      result_to_string(worm[pos].attack_threat_codes[i]));
      i++;
    }
    gprintf("\n;");
  }

  /* List all defense threat points. */
  if (worm[pos].defense_threat_points[0] == NO_MOVE)
    gprintf("no defense threat point, ");
  else {
    gprintf("defense threat point(s):");
    i = 0;
    while (worm[pos].defense_threat_points[i] != NO_MOVE) {
      if (i > 0)
	gprintf(",");
      gprintf(" %1m: %s", worm[pos].defense_threat_points[i],
	      result_to_string(worm[pos].defense_threat_codes[i]));
      i++;
    }
    gprintf("\n;");
  }

  /* Report lunch if any. */
  if (worm[pos].lunch != NO_MOVE)
    gprintf("lunch at %1m\n", worm[pos].lunch);

  gprintf("cutstone: %d, cutstone2: %d\n",
	  worm[pos].cutstone, worm[pos].cutstone2);

  gprintf("genus: %d, ", worm[pos].genus);

  if (worm[pos].inessential)
    gprintf("inessential: YES, ");
  else
    gprintf("inessential: NO, ");

  if (worm[pos].invincible)
    gprintf("invincible: YES, \n");
  else
    gprintf("invincible: NO, \n");

  if (worm[pos].unconditional_status == ALIVE)
    gprintf("unconditional status ALIVE\n");
  else if (worm[pos].unconditional_status == DEAD)
    gprintf("unconditional status DEAD\n");
  else if (worm[pos].unconditional_status == WHITE_BORDER)
    gprintf("unconditional status WHITE_BORDER\n");
  else if (worm[pos].unconditional_status == BLACK_BORDER)
    gprintf("unconditional status BLACK_BORDER\n");
  else if (worm[pos].unconditional_status == UNKNOWN)
    gprintf("unconditional status UNKNOWN\n");
}


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
