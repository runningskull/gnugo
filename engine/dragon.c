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
static void dragon_ring(int m, int n, int *i, int *j);
static int compute_dragon_status(int i, int j);
static void dragon_eye(int m, int n, struct eye_data[MAX_BOARD][MAX_BOARD]);
static int compute_escape(int m, int n, int dragon_status_known);

static int dragon2_initialized;
static int lively_white_dragons;
static int lively_black_dragons;

#define occupied(m, n) (m != -1 && p[m][n] != EMPTY)


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
  double t1 = 0., t2 = 0.;

  if (showtime)
    t1 = gg_gettimeofday();
  dragon2_initialized = 0;
  
  /* We start with the dragon data copied from the worm data, then
   * modify it as the worms are amalgamated into larger dragons.
   */

  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      dragon[m][n].id                 = -1;
      dragon[m][n].size               = worm[m][n].size;
      dragon[m][n].effective_size     = worm[m][n].effective_size;
      dragon[m][n].color              = worm[m][n].color;
      dragon[m][n].origini            = worm[m][n].origini;
      dragon[m][n].originj            = worm[m][n].originj;
      dragon[m][n].lunchi             = -1;
      dragon[m][n].lunchj             = -1;
      dragon[m][n].owl_attacki        = -1;
      dragon[m][n].owl_attackj        = -1;
      dragon[m][n].owl_attack_certain =  1;
      dragon[m][n].owl_defendi        = -1;
      dragon[m][n].owl_defendj        = -1;
      dragon[m][n].owl_defend_certain =  1;
      dragon[m][n].owl_status         = UNCHECKED;
      dragon[m][n].status             = UNKNOWN;
      dragon[m][n].matcher_status     = UNKNOWN;
      dragon[m][n].owl_threat_status  = UNCHECKED;
      dragon[m][n].owl_second_attacki = -1;
      dragon[m][n].owl_second_attackj = -1;
      dragon[m][n].owl_second_defendi = -1;
      dragon[m][n].owl_second_defendj = -1;
      dragon[m][n].escape_route       =  0;
      dragon[m][n].heyes              =  0;
      dragon[m][n].heyei              = -1;
      dragon[m][n].heyej              = -1;
      dragon[m][n].genus              =  0;
      dragon[m][n].semeai             =  0;
      half_eye[m][n].type             =  0;
      
      if (worm[m][n].origini == m && worm[m][n].originj == n)
	DEBUG(DEBUG_DRAGONS, 
	      "Initialising dragon from worm at %m, size %d\n", 
	      m, n, worm[m][n].size);
    }

  /* Amalgamate cavities. 
   *
   * Begin by finding the INESSENTIAL strings. These are defined as
   * surrounded strings which have no life potential unless part of
   * their surrounding chain can be captured. We give a conservative
   * definition of inessential: 
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
   * opponent's eye space. It then makes sense to amalgamate the
   * surrounding cavities into a single cave (empty dragon) with
   * bordercolor the opposite of the inessential worm.
   *
   * For example, in the following situation:
   *
   *   OOOOO
   *   O.X.O
   *   OOOOO
   *
   * we find two graybordered cavities of size one. The X string is
   * inessential, so these two cavities are amalgamated into a single cave.  */

  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      if (p[m][n]
	  && worm[m][n].origini == m
	  && worm[m][n].originj == n
	  && worm[m][n].genus == 0
	  && worm[m][n].liberties2 == 0
	  && worm[m][n].lunchi == -1)
      {
	int edge;
	int borigini = -1, boriginj = -1;

	int border_color = examine_cavity(m, n, &edge, NULL, NULL, NULL);
	if (border_color != GRAY_BORDER && edge < 3) {
	  dragon_ring(m, n, &borigini, &boriginj);
	  worm[m][n].inessential = 1;
	  propagate_worm(m, n);
	  dragon[borigini][boriginj].color = border_color;
	}
      }
    }

  if (showtime) {
    t2 = gg_gettimeofday();
    if (t2-t1 > 1.)
      fprintf(stderr, "  time to make dragons: %.2f sec\n", t2-t1);
    t1 = t2;
  }
  make_domains(black_eye, white_eye, 0);
  if (showtime) {
    t2 = gg_gettimeofday();
    if (t2-t1 > 1.)
      fprintf(stderr, "  time to make domains: %.2f sec\n", t2-t1);
    t1 = t2;
  }

  /* Find explicit connections patterns in database and amalgamate
   * involved dragons.
   */
  find_connections();
  if (showtime) {
    t2 = gg_gettimeofday();
    if (t2-t1 > 1.)
      fprintf(stderr, "  time to find connections: %.2f sec\n", t2-t1);
    t1 = t2;
  }
  
  /* Amalgamate dragons sharing an eyespace (not ko). At the same time
   * we decide to which dragon an eyespace belongs. Ko eyespaces
   * (typically false eyes but sometimes halfeyes) get assigned to an
   * arbitrary neighbor that is not the ko stone.
   */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {

      if ((black_eye[m][n].color == BLACK_BORDER) 
	  && (black_eye[m][n].origini == m)
	  && (black_eye[m][n].originj == n)) {
	if (!worm[m][n].ko
	   || (black_eye[m][n].esize > 1)) /* Only exclude living kos. */
	  dragon_eye(m, n, black_eye);
	else {
	  if (m > 0 && !worm[m-1][n].ko) {
	    black_eye[m][n].dragoni = dragon[m-1][n].origini;
	    black_eye[m][n].dragonj = dragon[m-1][n].originj;
	  } 
	  else if (m < board_size-1 && !worm[m+1][n].ko) {
	    black_eye[m][n].dragoni = dragon[m+1][n].origini;
	    black_eye[m][n].dragonj = dragon[m+1][n].originj;
	  } 
	  else if (n > 0 && !worm[m][n-1].ko) {
	    black_eye[m][n].dragoni = dragon[m][n-1].origini;
	    black_eye[m][n].dragonj = dragon[m][n-1].originj;
	  } 
	  else if (n < board_size-1 && !worm[m][n+1].ko) {
	    black_eye[m][n].dragoni = dragon[m][n+1].origini;
	    black_eye[m][n].dragonj = dragon[m][n+1].originj;
	  } 
	}
      }
	  
      if ((white_eye[m][n].color == WHITE_BORDER)
	  && (white_eye[m][n].origini == m)
	  && (white_eye[m][n].originj == n)) {
	if (!worm[m][n].ko
	    || (white_eye[m][n].esize > 1)) /* Only exclude living kos. */
	  dragon_eye(m, n, white_eye);
	else {
	  if (m > 0 && !worm[m-1][n].ko) {
	    white_eye[m][n].dragoni = dragon[m-1][n].origini;
	    white_eye[m][n].dragonj = dragon[m-1][n].originj;
	  } 
	  else if (m < board_size-1 && !worm[m+1][n].ko) {
	    white_eye[m][n].dragoni = dragon[m+1][n].origini;
	    white_eye[m][n].dragonj = dragon[m+1][n].originj;
	  } 
	  else if (n > 0 && !worm[m][n-1].ko) {
	    white_eye[m][n].dragoni = dragon[m][n-1].origini;
	    white_eye[m][n].dragonj = dragon[m][n-1].originj;
	  } 
	  else if (n < board_size-1 && !worm[m][n+1].ko) {
	    white_eye[m][n].dragoni = dragon[m][n+1].origini;
	    white_eye[m][n].dragonj = dragon[m][n+1].originj;
	  }
	}
      }
    }

  if (showtime) {
    t2 = gg_gettimeofday();
    if (t2-t1 > 1.)
      fprintf(stderr, "  time to amalgamate dragons: %.2f sec\n", t2-t1);
    t1 = t2;
  }

  /* At this time, all dragons have been finalized and we can
   * initialize the dragon2[] array. After that we can no longer allow
   * amalgamation of dragons.
   */
  initialize_supplementary_dragon_data();

  if (showtime) {
    t2 = gg_gettimeofday();
    if (t2-t1 > 1.)
      fprintf(stderr, "  time to initialize dragon2: %.2f sec\n", t2-t1);
    t1 = t2;
  }
  
  /* Find adjacent worms which can be easily captured: */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      if (worm[m][n].origini != m
	  || worm[m][n].originj != n
	  || p[m][n] == EMPTY
	  || worm[m][n].lunchi == -1)
	continue;

      i = worm[m][n].lunchi;
      j = worm[m][n].lunchj;

      /* In contrast to worm lunches, a dragon lunch must also be
       * able to defend itself. 
       */
      if (worm[i][j].defend_code == 0)
	continue;

      /* Tell the move generation code about the lunch. */
      if (color != EMPTY)
	add_lunch(m, n, i, j);
	
      /* If several lunches are found, we pick the juiciest.
       * First maximize cutstone, then minimize liberties.
       */
      {
	int origini = dragon[m][n].origini;
	int originj = dragon[m][n].originj;

	if ((dragon[origini][originj].lunchi == -1)
	    || (worm[i][j].cutstone
		> worm[dragon[origini][originj].lunchi]
		[dragon[origini][originj].lunchj].cutstone)
	    || ((worm[i][j].cutstone 
		 == worm[dragon[origini][originj].lunchi]
		 [dragon[origini][originj].lunchj].cutstone) 
		&& (worm[i][j].liberties
		    < worm[dragon[origini][originj].lunchi]
		    [dragon[origini][originj].lunchj].liberties))) {
	  dragon[origini][originj].lunchi = worm[i][j].origini;
	  dragon[origini][originj].lunchj = worm[i][j].originj;
	  TRACE("at %m setting %m.lunch to %m (cutstone=%d)\n",
		m, n, origini, originj,
		worm[i][j].origini, worm[i][j].originj, worm[i][j].cutstone);
	}
      }
    }

  /* Propagate lunch to rest of the dragon. */
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++) {
      dragon[i][j].lunchi
	= dragon[dragon[i][j].origini][dragon[i][j].originj].lunchi;
      dragon[i][j].lunchj
	= dragon[dragon[i][j].origini][dragon[i][j].originj].lunchj;
    }
  
  if (showtime) {
    t2 = gg_gettimeofday();
    if (t2-t1 > 1.)
      fprintf(stderr, "  time to find lunches: %.2f sec\n", t2-t1);
    t1 = t2;
  }

  /* In case origins of dragons got moved, put the dragons of eyes aright. */
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++) {
      if (black_eye[i][j].dragoni != -1) {
	  int di=dragon[black_eye[i][j].dragoni]
                       [black_eye[i][j].dragonj].origini;
	  int dj=dragon[black_eye[i][j].dragoni]
	               [black_eye[i][j].dragonj].originj;
	  black_eye[i][j].dragoni = di;
	  black_eye[i][j].dragonj = dj;
      }

      if (white_eye[i][j].dragoni != -1) {
	  int di=dragon[white_eye[i][j].dragoni]
	               [white_eye[i][j].dragonj].origini;
	  int dj=dragon[white_eye[i][j].dragoni] 
	               [white_eye[i][j].dragonj].originj;
	  white_eye[i][j].dragoni = di;
	  white_eye[i][j].dragonj = dj;
      }
    }

  if (showtime) {
    t2 = gg_gettimeofday();
    if (t2-t1 > 1.)
      fprintf(stderr, "  time to fix origins: %.2f sec\n", t2-t1);
    t1 = t2;
  }

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

      if (black_eye[m][n].color == BLACK_BORDER
	  && (!black_eye[m][n].marginal || life)
	  && (black_eye[m][n].neighbors <= 1)
	  && (black_eye[m][n].dragoni != -1)) {
	sum = topological_eye(m, n, BLACK, &ai, &aj, &di, &dj,
			      black_eye, white_eye, half_eye);
	if (sum >= 4) {
	  half_eye[m][n].type = FALSE_EYE;
	  if ((black_eye[m][n].esize == 1)
	      || is_legal(m, n, WHITE)
	      || p[m][n] == WHITE)
	    add_half_eye(m, n, black_eye, half_eye);
	}
	else if (sum == 3)
	  half_eye[m][n].type = HALF_EYE;
      }
      
      if (white_eye[m][n].color == WHITE_BORDER
	  && (!white_eye[m][n].marginal || life)
	  && (white_eye[m][n].neighbors <= 1)
	  && (white_eye[m][n].dragoni != -1)) {
	sum = topological_eye(m, n, WHITE, &ai, &aj, &di, &dj,
			      black_eye, white_eye, half_eye);
	if (sum >= 4) {
	  half_eye[m][n].type = FALSE_EYE;
	  if ((white_eye[m][n].esize == 1)
	      || is_legal(m, n, BLACK)
	      || p[m][n] == BLACK)
	    add_half_eye(m, n, white_eye, half_eye);
	}
	else if (sum == 3)
	  half_eye[m][n].type = HALF_EYE;
      }
    }

  /* Pattern based modification of the eye shapes computed by
   * make_domains and halfeye analysis.
   */
  modify_eye_spaces();
  
  /* Compute the number of eyes, half eyes, etc. in an eye space. */
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++) {
      if ((black_eye[i][j].color == BLACK_BORDER) 
	  && (black_eye[i][j].origini == i)
	  && (black_eye[i][j].originj == j)) 
      {
	int max, min, attacki, attackj, defendi, defendj;

	compute_eyes(i, j, &max, &min, &attacki, &attackj,
		     &defendi, &defendj, black_eye, half_eye, 1, color);
	DEBUG(DEBUG_EYES, "Black eyespace at %m: min=%d, max=%d\n",
	      i, j, min, max);
	black_eye[i][j].maxeye = max;
	black_eye[i][j].mineye = min;
	black_eye[i][j].attacki = attacki;
	black_eye[i][j].attackj = attackj;	  
	black_eye[i][j].defendi = defendi;
	black_eye[i][j].defendj = defendj;	  
	propagate_eye(i, j, black_eye);
      }

      if ((white_eye[i][j].color == WHITE_BORDER) 
	  && (white_eye[i][j].origini == i)
	  && (white_eye[i][j].originj == j)) 
      {
	int max, min, attacki, attackj, defendi, defendj;

	compute_eyes(i, j, &max, &min, &attacki, &attackj,
		     &defendi, &defendj, white_eye, half_eye, 1, color);
	DEBUG(DEBUG_EYES, "White eyespace at %m: min=%d, max=%d\n",
	      i, j, min, max);
	white_eye[i][j].maxeye = max;
	white_eye[i][j].mineye = min;
	white_eye[i][j].attacki = attacki;
	white_eye[i][j].attackj = attackj;	  
	white_eye[i][j].defendi = defendi;
	white_eye[i][j].defendj = defendj;	  
	propagate_eye(i, j, white_eye);
      }
    }

  if (showtime) {
    t2 = gg_gettimeofday();
    if (t2-t1 > 1.)
      fprintf(stderr, "  time to find eyes: %.2f sec\n", t2-t1);
    t1 = t2;
  }

  /* Now we compute the genus. */
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++) {
      if ((black_eye[i][j].color == BLACK_BORDER) 
	  && (black_eye[i][j].dragoni != -1) 
	  && (black_eye[i][j].origini == i)
	  && (black_eye[i][j].originj == j)) 
      {
	m = black_eye[i][j].dragoni;
	n = black_eye[i][j].dragonj;
	gg_assert (p[m][n] == BLACK);
	TRACE("eye at %m found for dragon at %m--augmenting genus\n",
	      i, j, m, n);
	dragon[m][n].genus += (black_eye[i][j].mineye);
	dragon[m][n].heyes += (black_eye[i][j].maxeye-black_eye[i][j].mineye);
	if (black_eye[i][j].maxeye - black_eye[i][j].mineye > 0) {
	  dragon[m][n].heyei = black_eye[i][j].attacki;
	  dragon[m][n].heyej = black_eye[i][j].attackj;
	}
      }
      if ((white_eye[i][j].color == WHITE_BORDER) 
	  && (white_eye[i][j].dragoni != -1)
	  && (white_eye[i][j].origini == i)
	  && (white_eye[i][j].originj == j)) 
      {
	m = white_eye[i][j].dragoni;
	n = white_eye[i][j].dragonj;
	gg_assert (p[m][n] == WHITE);
	TRACE("eye at %m found for dragon at %m--augmenting genus\n",
	      i, j, m, n);
	dragon[m][n].genus += (white_eye[i][j].mineye);
	dragon[m][n].heyes += (white_eye[i][j].maxeye-white_eye[i][j].mineye);
	if (white_eye[i][j].maxeye - white_eye[i][j].mineye > 0) {
	  dragon[m][n].heyei = white_eye[i][j].attacki;
	  dragon[m][n].heyej = white_eye[i][j].attackj;
	}
      }
    }

  /* Propagate genus to rest of the dragon. */
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++)
      dragon[i][j].genus
	= dragon[dragon[i][j].origini][dragon[i][j].originj].genus;

  if (showtime) {
    t2 = gg_gettimeofday();
    if (t2-t1 > 1.)
      fprintf(stderr, "  time to compute genus: %.2f sec\n", t2-t1);
    t1 = t2;
  }

  /* Compute the escape route measure. */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) 
      if (dragon[m][n].origini == m
	  && dragon[m][n].originj == n 
	  && p[m][n] != EMPTY) {
	dragon[m][n].escape_route = compute_escape(m, n, 0);
      }

  if (showtime) {
    t2 = gg_gettimeofday();
    if (t2-t1 > 1.)
      fprintf(stderr, "  time to compute escape: %.2f sec\n", t2-t1);
    t1 = t2;
  }

  /* Update the segmentation of the initial influence before we
   * compute the surrounding moyo sizes. The reason for this is that
   * now the eyespace inhibition found by find_cuts() can be taken
   * into account.
   */
  resegment_initial_influence();
  
  if (showtime) {
    t2 = gg_gettimeofday();
    if (t2-t1 > 1.)
      fprintf(stderr, "  resegment_initial_influence: %.2f sec\n", t2-t1);
    t1 = t2;
  }

  /* Compute the surrounding moyo sizes. */
  for (d = 0; d < number_of_dragons; d++) {
    dragon2[d].moyo = influence_get_moyo_size(dragon2[d].origini,
					      dragon2[d].originj,
					      DRAGON(d).color, 1);
  }
  
  if (showtime) {
    t2 = gg_gettimeofday();
    if (t2-t1 > 1.)
      fprintf(stderr, "  influence_get_moyo_size: %.2f sec\n", t2-t1);
    t1 = t2;
  }

  /* Determine status: ALIVE, DEAD, CRITICAL or UNKNOWN */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      if ((dragon[m][n].origini == m)
	  && (dragon[m][n].originj == n) && p[m][n])
	{
	  dragon[m][n].status = compute_dragon_status(m, n);
	  sgffile_dragon_status(m, n, dragon[m][n].status);
	}
    }

  if (showtime) {
    t2 = gg_gettimeofday();
    if (t2-t1 > 1.)
      fprintf(stderr, "  compute_dragon_status: %.2f sec\n", t2-t1);
    t1 = t2;
  }

  /* We must update the dragon status at every intersection before we
   * call the owl code. This updates all fields.
   */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      struct dragon_data *d = &(dragon[m][n]);
      dragon[m][n] = dragon[d->origini][d->originj];
    }
  
  find_neighbor_dragons();
  if (showtime) {
    t2 = gg_gettimeofday();
    if (t2-t1 > 1.)
      fprintf(stderr, "  find_neighbor_dragons: %.2f sec\n", t2-t1);
    t1 = t2;
  }

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
      double t3 = 0., t4 = 0.;
      
      if (p[m][n] == EMPTY
	  || (dragon[m][n].origini != m)
	  || (dragon[m][n].originj != n))
	continue;

      /* Some dragons can be ignored. But
       * Be extra careful with big dragons. */
      if (dragon[m][n].escape_route > 25
	  || DRAGON2(m, n).moyo > 20
	  || (DRAGON2(m, n).moyo > 10
	      && DRAGON2(m, n).moyo > dragon[m][n].size)) {
	dragon[m][n].owl_status = UNCHECKED;
	dragon[m][n].owl_threat_status = UNCHECKED;
	dragon[m][n].owl_attacki = -1;
	dragon[m][n].owl_attackj = -1;
	dragon[m][n].owl_defendi = -1;
	dragon[m][n].owl_defendj = -1;
	dragon[m][n].owl_second_attacki = -1;
	dragon[m][n].owl_second_attackj = -1;
	dragon[m][n].owl_second_defendi = -1;
	dragon[m][n].owl_second_defendj = -1;
      }
      else {
	if (showtime)
	  t3 = gg_gettimeofday();
	if (owl_attack(m, n, &attacki, &attackj, 
		       &dragon[m][n].owl_attack_certain)) {
	  dragon[m][n].owl_attacki = attacki;
	  dragon[m][n].owl_attackj = attackj;
	  if (attacki != -1
	      && owl_defend(m, n, &defendi, &defendj, 
			    &dragon[m][n].owl_defend_certain)) {
	    if (defendi != -1) {
	      dragon[m][n].owl_defendi = defendi;
	      dragon[m][n].owl_defendj = defendj;
	      dragon[m][n].owl_status = CRITICAL;
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
	      dragon[m][n].owl_status = CRITICAL;
	      DEBUG(DEBUG_OWL_PERFORMANCE,
		    "Inconsistent owl attack and defense results for %m.\n",
		    m, n);
	    }
	  }
	  else {
	    dragon[m][n].owl_status = DEAD; 
	    dragon[m][n].owl_defendi = -1;
	    dragon[m][n].owl_defendj = -1;
	    if (level >= 8
		&& !disable_threat_computation) {
	      if (owl_threaten_defense(m, n, &defendi, &defendj,
				       &second_defendi, &second_defendj)) {
		dragon[m][n].owl_threat_status = CAN_THREATEN_DEFENSE;
		dragon[m][n].owl_defendi = defendi;
		dragon[m][n].owl_defendj = defendj;
		dragon[m][n].owl_second_defendi = second_defendi;
		dragon[m][n].owl_second_defendj = second_defendj;
	      }
	      else
		dragon[m][n].owl_threat_status = DEAD;;
	    }
	  }
	}
	else {
	  if (!dragon[m][n].owl_attack_certain
	      && owl_defend(m, n, &defendi, &defendj, 
			    &dragon[m][n].owl_defend_certain)) {
	    /* If the result of owl_attack was not certain, we may
	     * still want the result of owl_defend */
	    dragon[m][n].owl_defendi = defendi;
	    dragon[m][n].owl_defendj = defendj;
	  }
	  dragon[m][n].owl_status = ALIVE;
	  dragon[m][n].owl_attacki = -1;
	  dragon[m][n].owl_attackj = -1;
	  if (level >= 8
	      && !disable_threat_computation) {
	    if (owl_threaten_attack(m, n, &attacki, &attackj,
				    &second_attacki, &second_attackj)) {
	      dragon[m][n].owl_threat_status = CAN_THREATEN_ATTACK;
	      dragon[m][n].owl_attacki = attacki;
	      dragon[m][n].owl_attackj = attackj;
	      dragon[m][n].owl_second_attacki = second_attacki;
	      dragon[m][n].owl_second_attackj = second_attackj;
	    }
	    else
	      dragon[m][n].owl_threat_status = ALIVE;
	  }
	}
	if (showtime) {
	  t4 = gg_gettimeofday();
	  if (t4-t3 > 0.5) {
	    gprintf("    owl reading time for dragon at %m: ", m, n);
	    fprintf(stderr, "%.2f sec\n", t4-t3);
	  }
	}
      }
    }

  if (showtime) {
    t2 = gg_gettimeofday();
    if (t2-t1 > 1.)
      fprintf(stderr, "  owl reading: %.2f sec\n", t2-t1);
    t1 = t2;
  }

  /* The dragon data is now correct at the origin of each dragon but
   * we need to copy it to every vertex.  
   */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      struct dragon_data *d = &(dragon[m][n]);
      dragon[m][n] = dragon[d->origini][d->originj];
    }

  /* Compute the status to be used by the matcher. We most trust the
   * owl status, if it is available.
   */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++)
      if (p[m][n] != EMPTY) {
	if (dragon[m][n].owl_status != UNCHECKED)
	  dragon[m][n].matcher_status = dragon[m][n].owl_status;
	else if (dragon[m][n].status == DEAD 
		 || dragon[m][n].status == CRITICAL) {
	  /* If a dragon has sufficient escape potential or
	   * surrounding moyo to stop the owl code from being run, the
	   * matcher_status should be no worse than UNKNOWN,
	   * regardless what the static life and death analysis
	   * guesses.
	   */
	  dragon[m][n].matcher_status = UNKNOWN;
	}
	else
	  dragon[m][n].matcher_status = dragon[m][n].status;
      }

  if (showtime) {
    t2 = gg_gettimeofday();
    if (t2-t1 > 1.)
      fprintf(stderr, "  compute matcher status: %.2f sec\n", t2-t1);
    t1 = t2;
  }

  /* Compute the safety value. */
  for (d = 0; d < number_of_dragons; d++) {
    int true_genus;
    m = dragon2[d].origini;
    n = dragon2[d].originj;
    true_genus = 2 * dragon[m][n].genus + dragon[m][n].heyes;
    /* FIXME: Probably need a better definition of INESSENTIAL dragons.
     *        There are cases where a string is owl insubstantial
     *        yet allowing it to be captured greatly weakens our
     *        position.
     */
    if (dragon[m][n].size == worm[m][n].size
	&& !owl_substantial(m, n))
      dragon2[d].safety = INESSENTIAL;
    else if (dragon[m][n].size == worm[m][n].size
	     && worm[m][n].attack_code != 0
	     && worm[m][n].defend_code == 0)
      dragon2[d].safety = TACTICALLY_DEAD;
    else if (0) /* Seki is detected by the call to semeai() below. */
      dragon2[d].safety = ALIVE_IN_SEKI;
    else if (dragon[m][n].owl_status == DEAD)
      dragon2[d].safety = DEAD;
    else if (dragon[m][n].owl_status == CRITICAL)
      dragon2[d].safety = CRITICAL;
    else if (dragon[m][n].owl_status == UNCHECKED
	     && true_genus < 4
	     && dragon2[d].moyo <= 10)
      dragon2[d].safety = WEAK;
    else if (dragon_invincible(m, n))
      dragon2[d].safety = INVINCIBLE;
    else if (true_genus >= 6 || dragon2[d].moyo > 20)
      dragon2[d].safety = STRONGLY_ALIVE;
    else if ((2 * true_genus + dragon2[d].moyo < 8
	      && DRAGON(d).escape_route < 10)
	     || (dragon[m][n].owl_threat_status == CAN_THREATEN_ATTACK)) {
      if (DRAGON(d).owl_attack_certain)
	  dragon2[d].safety = WEAKLY_ALIVE;
      else
	  dragon2[d].safety = WEAK;
    }
    else
      dragon2[d].safety = ALIVE;
  }

  if (showtime) {
    t2 = gg_gettimeofday();
    if (t2-t1 > 1.)
      fprintf(stderr, "  compute dragon safety: %.2f sec\n", t2-t1);
    t1 = t2;
  }

  /* Resolve semeais. This may revise the safety and status fields. */
  semeai(color);
  
  if (showtime) {
    t2 = gg_gettimeofday();
    if (t2-t1 > 1.)
      fprintf(stderr, "  semeai module: %.2f sec\n", t2-t1);
    t1 = t2;
  }

  /* The matcher_status is now correct at the origin of each dragon
   * but we need to copy it to every vertex.
   */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      dragon[m][n].matcher_status =
	dragon[dragon[m][n].origini][dragon[m][n].originj].matcher_status;
    }

  /* Revise essentiality of critical worms. Specifically, a critical
   * worm which is adjacent to no enemy dragon with matcher_status
   * better than DEAD, is considered INESSENTIAL.
   */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++)
      if (is_worm_origin(m, n, m, n)
	  && worm[m][n].attack_code != 0
	  && worm[m][n].defend_code != 0
	  && !worm[m][n].inessential) {
	int adji[MAXCHAIN];
	int adjj[MAXCHAIN];
	int neighbors;
	int r;
	int essential = 0;
	
	neighbors = chainlinks(m, n, adji, adjj);
	for (r = 0; r < neighbors; r++)
	  if (dragon[adji[r]][adjj[r]].matcher_status != DEAD) {
	    essential = 1;
	    break;
	  }

	if (!essential) {
	  worm[m][n].inessential = 1;
	  propagate_worm(m, n);
	}
      }
  
  if (showtime) {
    t2 = gg_gettimeofday();
    if (t2-t1 > 1.)
      fprintf(stderr, "  revise inessentiality: %.2f sec\n", t2-t1);
    t1 = t2;
  }

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
      if (p[m][n] == EMPTY)
	continue;
      i = dragon[m][n].origini;
      j = dragon[m][n].originj;
      if (dragon[i][j].id == -1)
	dragon[i][j].id = number_of_dragons++;
      dragon[m][n].id = dragon[i][j].id;
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
      if (p[m][n] != EMPTY
	  && dragon[m][n].origini == m
	  && dragon[m][n].originj == n) {
	DRAGON2(m, n).origini = m;
	DRAGON2(m, n).originj = n;
      }
    }

  /* Initialize the rest of the dragon2 data. */
  for (d = 0; d < number_of_dragons; d++) {
    dragon2[d].neighbors = 0;
    dragon2[d].moyo = -1;
    dragon2[d].safety = -1;
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
      if (p[m][n] != EMPTY) {
	dragons[m][n] = dragon[m][n].id;
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
	  if (!ON_BOARD(i, j))
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
	  if (ON_BOARD(i, j) && dragons[i][j] >= 0)
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
      gprintf("dragon %d at %m:", d, dragon2[d].origini, dragon2[d].originj);
      for (i = 0; i < dragon2[d].neighbors; i++)
	gprintf(" %m(%d)", dragon2[dragon2[d].adjacent[i]].origini,
		dragon2[dragon2[d].adjacent[i]].originj,
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

  typedef struct eye_data row_of_eye_data[MAX_BOARD];
  row_of_eye_data *eye;

  int i, j;

  int strong_eyes = 0;

  gg_assert(p[m][n] != EMPTY);

  /* First look for invincible strings in the dragon. */
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++)
      if (same_dragon(i, j, m, n) && worm[i][j].invincible)
        return 1;

  /* Examine the eye spaces.
   * FIXME: The check for half eyes or false eyes may be too weak.
   */
  if (p[m][n] == BLACK)
    eye = black_eye;
  else
    eye = white_eye;

  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++)
      if (eye[i][j].origini == i && eye[i][j].originj == j
          && same_dragon(eye[i][j].dragoni, eye[i][j].dragonj, m, n)) {
        if (eye[i][j].msize == 0 && eye[i][j].mineye > 0)
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
  static const char *cnames[] = 
    {"(empty)", "white dragon", "black dragon",
     "gray-bordered cave", "black-bordered cave", "white-bordered cave"};
  static const char *snames[] = 
    {"dead", "alive", "critical", "unknown", "unchecked"};

  static const char *safety_names[] =
  {"dead", "alive", "critical", "inessential", "tactically dead", "weak",
   "weakly_alive", "alive_in_seki", "strongly_alive", "invincible"};
  
  int m, n;

  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      struct worm_data *w = &(worm[m][n]);

      if (((w->origini)==m)&&((w->originj)==n)) {
	if (p[m][n]) {
	  gprintf("%m : (dragon %m) %s string of size %d (%f), genus %d: (%d,%d,%d,%d)",
		  m, n, dragon[m][n].origini, dragon[m][n].originj,
		  color_to_string(p[m][n]),
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
	  
	  if (w->attack_code != 0)
	    gprintf("- attackable at %m, attack code = %d\n",
		    w->attacki, w->attackj, w->attack_code);
	  if (w->defend_code != 0)
	    gprintf("- defendable at %m, defend code = %d\n",
		    w->defendi, w->defendj, w->defend_code);

	  if (w->lunchi != -1)
	    gprintf("... adjacent worm %m is lunch\n", w->lunchi, w->lunchj);
	  
	  if (w->inessential)
	    gprintf("- is inessential\n");
	  
	  if (w->invincible)
	    gprintf("- is invincible\n");
	  
	  if (w->ko == 1)
	    gprintf("- is a ko stone\n");
	}
	else 
	  gprintf("%m : cavity of size %d\n",m,n,w->size);
      }
    }

  gprintf("%o\n");
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      struct dragon_data *d = &(dragon[m][n]);
      struct dragon_data2 *d2 = &(dragon2[d->id]);
      int k;

      if (((d->origini)==m) && ((d->originj)==n)) {
	if (p[m][n]) {
	  gprintf("%m : %s dragon size %d (%f), genus %d, half eyes %d, escape factor %d, status %s, matcher status %s, moyo size %d safety %s",
		  m,n,
		  p[m][n]==BLACK ? "B" : "W",
		  d->size,
		  d->effective_size,
		  d->genus,
		  d->heyes,
		  d->escape_route,
		  snames[d->status],
		  snames[d->matcher_status],
		  d2->moyo,
		  safety_names[d2->safety]);
	  gprintf(", owl status %s\n", snames[d->owl_status]);
	  if (d->owl_status == CRITICAL) {
	    gprintf("... owl attackable at %m\n",
		    d->owl_attacki, d->owl_attackj);
	    gprintf("... owl defendable at %m\n",
		    d->owl_defendi, d->owl_defendj);
	  }
	  gprintf("... neighbors");
	  for (k = 0; k < d2->neighbors; k++) {
	    int d = d2->adjacent[k];
	    gprintf(" %m", dragon2[d].origini, dragon2[d].originj);
	  }
	  gprintf("\n");
	  if (d->lunchi != -1)
	    gprintf("... adjacent worm %m is lunch\n", d->lunchi, d->lunchj);
	}
	else {
	  gprintf("%m : cave of size %d and border color %s\n",
		  m,n,
		  d->size,
		  cnames[d->color]);
	  if (d->color == BLACK_BORDER || d->color == WHITE_BORDER) {
	    if (!worm[m][n].ko)
	      gprintf("... surrounded by dragon at %m\n",
		      d->borderi, d->borderj);
	    else
	      gprintf("... is a ko\n");
	  }
	}
      }
    }
}


  
/* The function dragon_ring(m, n, *di, *dj) amalgamates every cave
 * adjacent to the worm at (m, n). The amalgamated caves then form a
 * bigger cave ringing the worm at (m, n).
 *
 * The name is historic. Today it is only used for amalgamating caves.
 */

static void
dragon_ring(int m, int n, int *di, int *dj)
{
  int i, j;
  int dragoni = -1, dragonj = -1;
  int k;

  ASSERT(p[m][n] != EMPTY, m, n);
  DEBUG(DEBUG_DRAGONS, "amalgamate dragons around %m\n", m, n);
  
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++) {
      if (worm[i][j].origini == m && worm[i][j].originj == n) {
	ASSERT(p[i][j] != EMPTY, i, j);
	for (k = 0; k < 4; k++) {
	  int di = deltai[k];
	  int dj = deltaj[k];
	  if (ON_BOARD(i+di, j+dj) && (p[i+di][j+dj] == EMPTY)) {
	    if (dragoni == -1) {
	      dragoni = dragon[i+di][j+dj].origini;
	      dragonj = dragon[i+di][j+dj].originj;
	      ASSERT(p[dragoni][dragonj] == EMPTY, dragoni, dragonj);
	    }
	    else if (dragoni != dragon[i+di][j+dj].origini
		     || dragonj != dragon[i+di][j+dj].originj) {
	      join_dragons(i+di, j+dj, dragoni, dragonj);
	      dragoni = dragon[i+di][j+dj].origini;
	      dragonj = dragon[i+di][j+dj].originj;
	      ASSERT(p[dragoni][dragonj] == EMPTY, dragoni, dragonj);
	    }
	  }
	}
      }
    }

  *di = dragoni;   /* (m, n) is an inessential string. */
  *dj = dragonj;
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
dragon_eye(int m, int n, struct eye_data eye[MAX_BOARD][MAX_BOARD])
{
  int i, j;
  int dragoni = -1, dragonj = -1;
  int color;
  int k;

  /* don't amalgamate across ikken tobi */
  if (eye[m][n].esize == 3 && eye[m][n].msize > 1)
    return;

  DEBUG(DEBUG_DRAGONS, "amalgamate dragons around %m\n", m, n);
  if (eye[m][n].color == BLACK_BORDER)
    color = BLACK;
  else {
    gg_assert(eye[m][n].color == WHITE_BORDER);
    color = WHITE;
  }

  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++) {
      if (eye[i][j].origini == m
	  && eye[i][j].originj == n
	  && !eye[i][j].marginal
	  && !(eye[i][j].type & INHIBIT_CONNECTION)) {
	for (k = 0; k < 4; k++) {
	  int di = deltai[k];
	  int dj = deltaj[k];
	  if (ON_BOARD(i+di, j+dj) && p[i+di][j+dj] == color) {
	    if (dragoni == -1) {
	      dragoni = dragon[i+di][j+dj].origini;
	      dragonj = dragon[i+di][j+dj].originj;
	    }
	    else if ((dragoni != dragon[i+di][j+dj].origini) 
		     || (dragonj != dragon[i+di][j+dj].originj)) {
	      join_dragons(i+di, j+dj, dragoni, dragonj);
	      dragoni = dragon[i+di][j+dj].origini;
	      dragonj = dragon[i+di][j+dj].originj;
	    }
	  }
	}
      }
    }
  
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++) {
      if ((eye[i][j].color == BLACK_BORDER 
	   || eye[i][j].color == WHITE_BORDER) 
	  && eye[i][j].origini == m
	  && eye[i][j].originj == n)
      {
	eye[i][j].dragoni = dragoni;
	eye[i][j].dragonj = dragonj;
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

  int i = dragon[ai][aj].origini;
  int j = dragon[ai][aj].originj;
  int m = dragon[bi][bj].origini;
  int n = dragon[bi][bj].originj;
  int oi; /* new origini */
  int oj; /* new originj */
  
  gg_assert(p[i][j] == p[m][n]);
  gg_assert(dragon2_initialized == 0);

  if (p[i][j] == EMPTY) {
    /* Joining caves. */
    oi = m;
    oj = n;
  }
  else {
    /* We want to have the origin pointing to the largest string of the dragon.
     * If this is not unique, we take the "upper leftmost" one.
     */
    if (worm[i][j].size > worm[m][n].size
	|| (worm[i][j].size == worm[m][n].size
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
  
  dragon[oi][oj].size  = dragon[m][n].size + dragon[i][j].size;
  dragon[oi][oj].effective_size  = (dragon[m][n].effective_size
				  + dragon[i][j].effective_size);
  dragon[oi][oj].genus = dragon[m][n].genus + dragon[i][j].genus;

  for (t = 0; t < board_size; t++)
    for (u = 0; u < board_size; u++){
      if ((dragon[t][u].origini == i && dragon[t][u].originj == j)
	  || (dragon[t][u].origini == m && dragon[t][u].originj == n)) {
	dragon[t][u].origini = oi;
	dragon[t][u].originj = oj;
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
  int true_genus = 2*dragon[i][j].genus + dragon[i][j].heyes;

  gg_assert(dragon2_initialized);
  
  /* If it has two sure eyes, everything is just dandy. */
  if (true_genus > 3)
    return ALIVE;

  /* If the dragon consists of one worm, there is an attack, but 
   * no defense and there is less than one eye and one half eye,
   * the situation is hopeless.
   */
  if (dragon[i][j].size == worm[i][j].size
      && worm[i][j].attack_code != 0 
      && worm[i][j].defend_code == 0
      && true_genus < 3)
    return DEAD;
  
  if (dragon[i][j].lunchi != -1
      && true_genus < 3
      && worm[dragon[i][j].lunchi][dragon[i][j].lunchj].defend_code != 0
      && dragon[i][j].escape_route < 5)
    if (true_genus == 2
	|| worm[dragon[i][j].lunchi][dragon[i][j].lunchj].size > 2)
      return CRITICAL;

  if (dragon[i][j].lunchi != -1
      && true_genus >= 3)
    return ALIVE;

  if (dragon[i][j].lunchi == -1
      || worm[dragon[i][j].lunchi][dragon[i][j].lunchj].cutstone < 2) 
  {
    if (true_genus < 3
	&& dragon[i][j].escape_route == 0
	&& DRAGON2(i, j).moyo < 5)
      return DEAD;

    if (true_genus == 3
	&& dragon[i][j].escape_route < 5)
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
dragon_escape(char goal[MAX_BOARD][MAX_BOARD], int color,
	      int escape_value[MAX_BOARD][MAX_BOARD])
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
      if (goal[i][j])
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
	   && ((white_eye[i][j].type & INHIBIT_CONNECTION)
	       || white_eye[i][j].cut == 1))
	  || (color == BLACK
	      && ((black_eye[i][j].type & INHIBIT_CONNECTION)
		  || black_eye[i][j].cut == 1)))
	continue;
      
      if (distance == 4)
	escape_potential += escape_value[i][j];
      else {
	if (i > 0
	    && !mx[i-1][j]
	    && (p[i-1][j] == color
		|| (p[i-1][j] == EMPTY
		    && i > 1 && p[i-2][j] != other
		    && j > 0 && p[i-1][j-1] != other
		    && j < board_size-1 && p[i-1][j+1] != other)))
	  ENQUEUE(i-1, j);

      	if (i < board_size-1
	    && !mx[i+1][j]
	    && (p[i+1][j] == color
		|| (p[i+1][j] == EMPTY
		    && i < board_size-2 && p[i+2][j] != other
		    && j > 0 && p[i+1][j-1] != other
		    && j < board_size-1 && p[i+1][j+1] != other)))
	  ENQUEUE(i+1, j);

	if (j > 0
	    && !mx[i][j-1]
	    && (p[i][j-1] == color
		|| (p[i][j-1] == EMPTY
		    && j > 1 && p[i][j-2] != other
		    && i > 0 && p[i-1][j-1] != other
		    && i < board_size-1 && p[i+1][j-1] != other)))
	  ENQUEUE(i, j-1);

	if (j < board_size-1
	    && !mx[i][j+1]
	    && (p[i][j+1] == color
		|| (p[i][j+1] == EMPTY
		    && j < board_size-2 && p[i][j+2] != other
		    && i > 0 && p[i-1][j+1] != other
		    && i < board_size-1 && p[i+1][j+1] != other)))
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
	      && p[i][j-1] == EMPTY && p[i-1][j] == EMPTY
	      && (p[i-1][j-1] == color
		  || (p[i-1][j-1] == EMPTY
		      && p[i-2][j-1] != other && p[i-1][j-2] != other)))
	    ENQUEUE(i-1, j-1);

	  if (i > 1 && j < board_size-2
	      && p[i][j+1] == EMPTY && p[i-1][j] == EMPTY
	      && (p[i-1][j+1] == color
		  || (p[i-1][j+1] == EMPTY
		      && p[i-2][j+1] != other && p[i-1][j+2] != other)))
	    ENQUEUE(i-1, j+1);

	  if (i < board_size-2 && j < board_size-2
	      && p[i][j+1] == EMPTY && p[i+1][j] == EMPTY
	      && (p[i+1][j+1] == color
		  || (p[i+1][j+1] == EMPTY
		      && p[i+2][j+1] != other && p[i+1][j+2] != other)))
	    ENQUEUE(i+1, j+1);

	  if (i < board_size-2 && j > 1
	      && p[i][j-1] == EMPTY && p[i+1][j] == EMPTY
	      && (p[i+1][j-1] == color
		  || (p[i+1][j-1] == EMPTY
		      && p[i+2][j-1] != other && p[i+1][j-2] != other)))
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
  char goal[MAX_BOARD][MAX_BOARD];
  int escape_value[MAX_BOARD][MAX_BOARD];

  ASSERT(p[m][n] != EMPTY, m, n);
  
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++) {
      goal[i][j] = same_dragon(i, j, m, n);
    }

  compute_escape_influence(goal, p[m][n], escape_value, dragon_status_known);

  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++) {
      if (dragon_status_known) {
	if (dragon[i][j].status == ALIVE)
	  escape_value[i][j] = 6;
	else if (dragon[i][j].status == UNKNOWN
		 && (dragon[i][j].escape_route > 5 || DRAGON2(i, j).moyo > 5))
	  escape_value[i][j] = 4;
      }
      else {
	if (p[i][j] == p[m][n]
	    && !goal[i][j]
	    && worm[i][j].attack_code == 0)
	  escape_value[i][j] = 2;
      }
    }

  return dragon_escape(goal, p[m][n], escape_value);
}


/* 
 * Test whether two dragons are the same. Used by autohelpers and elsewhere.
 */

int
same_dragon(int ai, int aj, int bi, int bj)
{
  if (ai == -1 || aj == -1 || bi == -1 || bj == -1)
    return (ai == bi && aj == bj);
  
  ASSERT_ON_BOARD(ai, aj);
  ASSERT_ON_BOARD(bi, bj);

  return (dragon[ai][aj].origini == dragon[bi][bj].origini
	  && dragon[ai][aj].originj == dragon[bi][bj].originj);
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
  return dragon[i][j].status;
}


int
matcher_status(int i, int j)
{
  return dragon[i][j].matcher_status;
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

  if (p[m][n] == EMPTY) {
    gprintf("There is no dragon at %m\n", m, n);
    return;
  }

  gprintf("*** dragon at %m:\n", m, n);
  gprintf("color: %s; origin: %m; size: %d; effective size: %f\n",
	  (dragon[m][n].color == WHITE) ? "WHITE" : "BLACK",
	  dragon[m][n].origini, dragon[m][n].originj,
	  dragon[m][n].size, dragon[m][n].effective_size);

  gprintf("strings:");
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++)
      if (worm[i][j].origini == i
	  && worm[i][j].originj == j
	  && same_dragon(i, j, m, n))
	gprintf(" %m", i, j);

  gprintf("\nhalf eyes: %d, ", dragon[m][n].heyes);
  if (dragon[m][n].heyei != -1)
    gprintf("half eye: %m, ", dragon[m][n].heyei, dragon[m][n].heyej);
  else
    gprintf("half eye: NONE,");

  gprintf(" genus %d, escape_route %d,", dragon[m][n].genus,
	  dragon[m][n].escape_route);
  if (dragon[m][n].lunchi != -1)
    gprintf(" lunch at %m\n", dragon[m][n].lunchi, dragon[m][n].lunchj);
  else
    gprintf(" no lunch\n");

  gprintf("dragon status %s, owl status %s, matcher status %s, owl threat status %s\n",
	  status_to_string(dragon[m][n].status),
	  status_to_string(dragon[m][n].owl_status),
	  status_to_string(dragon[m][n].matcher_status),
	  status_to_string(dragon[m][n].owl_threat_status));

  if (dragon[m][n].owl_attacki != -1)
    gprintf("owl attack point %m, ",
	    dragon[m][n].owl_attacki, dragon[m][n].owl_attackj);
  else
    gprintf("no owl attack point, ");

  if (dragon[m][n].owl_second_attacki != -1)
    gprintf("second owl attack point %m\n",
	    dragon[m][n].owl_second_attacki, dragon[m][n].owl_second_attackj);
  else
    gprintf("no second owl attack point\n");

  if (dragon[m][n].owl_defendi != -1)
    gprintf("owl defense point %m, ",
	    dragon[m][n].owl_defendi, dragon[m][n].owl_defendj);
  else
    gprintf("no owl defense point, ");

  if (dragon[m][n].owl_second_defendi != -1)
    gprintf("second owl defense point %m\n",
	    dragon[m][n].owl_second_defendi, dragon[m][n].owl_second_defendj);
  else
    gprintf("no second owl defense point\n");

  if (dragon[m][n].semeai)
    gprintf("This dragon is involved in a semeai. Margin of safety %d\n",
	    dragon[m][n].semeai_margin_of_safety);
  else
    gprintf("This dragon is not involved in a semeai.\n");

  if (dragon[m][n].id != -1) {
    gprintf("neighbor dragons: ");
    for (k = 0; k < DRAGON2(m, n).neighbors; k++)
      gprintf("%m ",
	      dragon2[DRAGON2(m, n).adjacent[k]].origini,
	      dragon2[DRAGON2(m, n).adjacent[k]].originj);
  gprintf("\nmoyo: %d; safety: %d\n",
	  DRAGON2(m, n).moyo, DRAGON2(m, n).safety);
  }
}


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */





