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
#include <assert.h>
#include "liberty.h"
#include "patterns.h"

/* Try to match all (permutations of) connection patterns at (m,n).
 * For each match, if it is a B pattern, set cutting point in worm
 * data structure and make eye space marginal for the connection
 * inhibiting entries of the pattern. If it is a C pattern, amalgamate
 * the dragons in the pattern.
 */

static void
cut_connect_callback(int m, int n, int color, struct pattern *pattern,
		     int ll, void *data)
{
  int stari, starj;
  int k;
  int first_dragoni=-1, first_dragonj=-1;
  int second_dragoni=-1, second_dragonj=-1;

  int other=OTHER_COLOR(color);
  UNUSED(data);
  
  TRANSFORM(pattern->movei, pattern->movej, &stari, &starj, ll);
  stari += m;
  starj += n;
  if ((pattern->class & CLASS_B) && !safe_move(stari, starj, other))
    return;

  /* If C pattern, test if there are more than one dragon in this
   * pattern so that there is something to connect, before doing any
   * expensive reading.
   */
  if (pattern->class & CLASS_C) {
    for (k = 0; k < pattern->patlen; ++k) { /* match each point */
      int x, y; /* absolute (board) co-ords of (transformed) pattern element */

      /* transform pattern real coordinate */
      TRANSFORM(pattern->patn[k].x, pattern->patn[k].y, &x, &y, ll);
      x += m;
      y += n;
      
      /* Look for distinct dragons. */
      if (pattern->patn[k].att == ATT_O) {
	if (first_dragoni == -1) {
	  first_dragoni = dragon[x][y].origini;
	  first_dragonj = dragon[x][y].originj;
	}
	else if ((second_dragoni == -1)
		 && ((dragon[x][y].origini != first_dragoni)
		     || (dragon[x][y].originj != first_dragonj))) {
	  second_dragoni = dragon[x][y].origini;
	  second_dragonj = dragon[x][y].originj;
	  /* A second dragon found, no need to continue looping. */
	  break;
	}
      }
    }
    if (second_dragoni == -1)
      return; /* Nothing to amalgamate. */
  }
    
  /* If the pattern has a constraint, call the autohelper to see
   * if the pattern must be rejected.
   */
  if (pattern->autohelper_flag & HAVE_CONSTRAINT) {
    if (!pattern->autohelper(pattern, ll, stari, starj, color, 0))
      return;
  }

  /* If the pattern has a helper, call it to see if the pattern must
   * be rejected.
   */
  if (pattern->helper) {
    if (!pattern->helper(pattern, ll, stari, starj, color))
      return;
  }

  if ((pattern->class & (CLASS_B | CLASS_E | CLASS_e))
      && !(pattern->class & CLASS_s)) {
    /* Require that the X stones in the pattern are tactically safe. */
    for (k = 0; k < pattern->patlen; ++k) { /* match each point */
      if (pattern->patn[k].att == ATT_X) {
	/* transform pattern real coordinate */
	int x, y;
	TRANSFORM(pattern->patn[k].x,pattern->patn[k].y,&x,&y,ll);
	x += m;
	y += n;

	if (worm[x][y].attack_code == WIN
	  && (pattern->movei == -1
	      || !does_defend(stari, starj, x, y)))
	  return; /* Match failed */
      }
    }
  }

  /* Get here => Pattern matches. */
  if (pattern->class & CLASS_B) {
    TRACE("Cutting pattern %s+%d found at %m\n",
	  pattern->name, ll, m, n);
    TRACE("cutting point %m\n", stari, starj);
  }
  else if (pattern->class & CLASS_C)
    TRACE("Connecting pattern %s+%d found at %m\n",
	  pattern->name, ll, m, n);
  else if (pattern->class & CLASS_E)
    TRACE("Eye space modifying pattern %s+%d found at %m\n",
	  pattern->name, ll, m, n);
  else if (pattern->class & CLASS_e)
    TRACE("Eye space modifying pattern %s+%d found at %m\n",
	  pattern->name, ll, m, n);

  /* does the pattern have an action? */
  if (pattern->autohelper_flag & HAVE_ACTION) {
    pattern->autohelper(pattern, ll, stari, starj, color, 1);
  }

  /* If it is a B pattern, set cutting point in eye data and make eye
   * space marginal.
   */
  
  if (pattern->class & CLASS_B) {
    if (color == WHITE)
      white_eye[stari][starj].cut = 1;
    else
      black_eye[stari][starj].cut = 1;
    if (color == WHITE && white_eye[stari][starj].color == WHITE_BORDER)
      white_eye[stari][starj].marginal = 1;
    else if (color == BLACK && black_eye[stari][starj].color == BLACK_BORDER)
      black_eye[stari][starj].marginal = 1;
  }
  else if (!(pattern->class & CLASS_C))
    return; /* Nothing more to do, up to the helper or autohelper
	       to amalgamate dragons or modify eye space. */

  /* If it is a C pattern, find the dragons to connect.
   * If it is a B pattern, find eye space points to inhibit connection
   * through.
   */
  first_dragoni = -1;
  first_dragonj = -1;
  second_dragoni = -1;
  second_dragonj = -1;
  for (k = 0; k < pattern->patlen; ++k) { /* match each point */
    int x, y; /* absolute (board) co-ords of (transformed) pattern element */

    /* transform pattern real coordinate */
    TRANSFORM(pattern->patn[k].x,pattern->patn[k].y,&x,&y,ll);
    x+=m;
    y+=n;

    /* Look for dragons to amalgamate. Never amalgamate stones which
     * can be attacked.
     */
    if ((pattern->class & CLASS_C) && (p[x][y] == color)
	&& (worm[x][y].attack_code == 0)) {
      if (first_dragoni == -1) {
	first_dragoni = dragon[x][y].origini;
	first_dragonj = dragon[x][y].originj;
      }
      else if ((second_dragoni == -1)
	       && ((dragon[x][y].origini != first_dragoni)
		   || (dragon[x][y].originj != first_dragonj))) {
	second_dragoni = dragon[x][y].origini;
	second_dragonj = dragon[x][y].originj;
	/* A second dragon found, we amalgamate them at once. */
	TRACE("Pattern %s joins dragons %m, %m\n",
	      pattern->name, first_dragoni, first_dragonj,
	      second_dragoni, second_dragonj);
	join_dragons(second_dragoni, second_dragonj,
		     first_dragoni, first_dragonj);
	/* Now look for another second dragon. */
	second_dragoni = -1;
	second_dragonj = -1;
	first_dragoni = dragon[x][y].origini;
	first_dragonj = dragon[x][y].originj;
      }
    }

    /* Inhibit connections */
    if (pattern->class & CLASS_B) {
      if (pattern->patn[k].att != ATT_not)
	break; /* The inhibition points are guaranteed to come first. */
      if (color == WHITE && white_eye[x][y].color == WHITE_BORDER)
	white_eye[x][y].type |= INHIBIT_CONNECTION;
      else if (color == BLACK && black_eye[x][y].color == BLACK_BORDER)
	black_eye[x][y].type |= INHIBIT_CONNECTION;
    }
  } /* loop over elements */
}


/* Only consider B patterns. */
static void
cut_callback(int m, int n, int color, struct pattern *pattern, int ll,
	     void *data)
{
  if (pattern->class & CLASS_B)
    cut_connect_callback(m, n, color, pattern, ll, data);
}
  

/* Consider C patterns and those without classification. */
static void
conn_callback(int m, int n, int color, struct pattern *pattern, int ll,
	      void *data)
{
  if (!(pattern->class & (CLASS_B | CLASS_e)))
    cut_connect_callback(m, n, color, pattern, ll, data);
}
  
/* Only consider e patterns. */
static void
modify_eye_callback(int m, int n, int color, struct pattern *pattern,
		     int ll, void *data)
{
  if (pattern->class & CLASS_e)
    cut_connect_callback(m, n, color, pattern, ll, data);
}
  
/* Find cutting points which should inhibit amalgamations and sever
 * the adjacent eye space.
 */
void
find_cuts(void)
{
  global_matchpat(cut_callback, ANCHOR_COLOR, &conn_db, NULL, NULL);
}

/* Find explicit connection patterns and amalgamate the involved dragons. */
void
find_connections(void)
{
  global_matchpat(conn_callback, ANCHOR_COLOR, &conn_db, NULL, NULL);
}

void
modify_eye_spaces(void)
{
  global_matchpat(modify_eye_callback, ANCHOR_COLOR, &conn_db, NULL, NULL);
}


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
