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
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include <math.h>

#include "liberty.h"
#include "sgftree.h"

#include "gg_utils.h"


/*
 * Change the status of all the stones in the dragon at (x,y).
 */

void
change_matcher_status(int m, int n, int status)
{
  int i, j;
  int origin = dragon[m][n].origin;

  for (i = 0; i < board_size; i++) 
    for (j = 0; j < board_size; j++) {
      if (dragon[i][j].origin == origin)
	dragon[i][j].matcher_status = status;
    }
}


/* 
 * Change_defense(str, tpos, dcode) moves the point of defense of the
 * worm at (str) to (tpos), and sets worm[a].defend_code to dcode.
 *
 * This function is less important with the 3.0 move generation
 * scheme, but not entirely ineffective.
 */

void
change_defense(int str, int tpos, int dcode)
{
  int origin = worm[I(str)][J(str)].origin;
  int dpos   = worm[I(str)][J(str)].defense_point;  /* Old defense point. */
  
  gg_assert (stackp == 0);

  if (tpos == 0)
    TRACE("Removed defense of %1m (was %1m).\n", origin, dpos);
  else if (dpos == 0)
    TRACE("Setting defense of %1m to %1m.\n", origin, tpos);
  else
    TRACE("Moved defense of %1m from %1m to %1m.\n", origin, dpos, tpos);
  
  gg_assert(tpos == 0 || board[tpos] == EMPTY);
  gg_assert(board[str] != EMPTY);

  if (worm[I(str)][J(str)].attack_code != 0
      && worm[I(str)][J(str)].defend_code != 0
      && tpos == 0)
  {
    int m, n;

    for (m = 0; m < board_size; m++)
      for (n = 0; n < board_size; n++)
	if (dragon[m][n].origin == dragon[I(str)][J(str)].origin)
	  dragon[m][n].status = DEAD;
  }
  worm[I(origin)][J(origin)].defend_code = dcode;
  worm[I(origin)][J(origin)].defense_point = tpos;
  propagate_worm(I(origin), J(origin));

  if (tpos != 0)
    add_defense_move(I(tpos), J(tpos), I(origin), J(origin));
  else if (dpos != 0)
    remove_defense_move(I(str), J(str), I(origin), J(origin));
}


/*
 * change_attack(str, tpos, acode) moves the point of attack of the
 * worm at (str) to (tpos), and sets worm[str].attack_code to acode.
 */

void
change_attack(int str, int tpos, int acode)
{
  int origin = worm[I(str)][J(str)].origin;
  int apos   = worm[I(str)][J(str)].attack_point;  /* Old attack point. */

  gg_assert(stackp == 0);

  if (tpos == 0) 
    TRACE("Removed attack of %1m (was %1m).\n", origin, apos);
  else if (apos == 0)
    TRACE("Setting attack of %1m to %1m.\n", origin, tpos);
  else
    TRACE("Moved attack of %1m from %1m to %1m.\n", origin, apos, tpos);

  gg_assert (tpos == 0 || board[tpos] == EMPTY);
  gg_assert (board[str] != EMPTY);

  worm[I(origin)][J(origin)].attack_code  = acode;
  worm[I(origin)][J(origin)].attack_point = tpos;
  propagate_worm(I(origin), J(origin));

  if (tpos != 0)
    add_attack_move(I(tpos), J(tpos), I(origin), J(origin));
  else if (apos != 0)
    remove_attack_move(I(apos), J(apos), I(origin), J(origin));
}


/*
 * Check whether a move at (ti,tj) stops the enemy from playing at (ai,aj).
 */

int
defend_against(int ti, int tj, int color, int ai, int aj)
{
  if (trymove2(ti, tj, color, "defend_against", -1, -1, EMPTY, -1, -1)) {
    if (safe_move2(ai, aj, OTHER_COLOR(color)) == 0) {
      popgo();
      return 1;
    }
    popgo();
  }
  return 0;
}


/* 
 * Returns true if color can cut at (i,j), or if connection through (i,j)
 * is inhibited. This information is collected by find_cuts(), using the B
 * patterns in the connections database.
 */

int
cut_possible(int i, int j, int color)
{
  if (color == WHITE)
    return (black_eye[i][j].cut || (black_eye[i][j].type & INHIBIT_CONNECTION));
  else
    return (white_eye[i][j].cut || (white_eye[i][j].type & INHIBIT_CONNECTION));
}


/*
 * does_attack(ti, tj, ai, aj) returns true if the move at (ti, tj)
 * attacks (ai, aj). This means that it captures the string, and that
 * (ai, aj) is not already dead.
 */

int
does_attack(int ti, int tj, int ai, int aj)
{
  int color = BOARD(ai, aj);
  int other = OTHER_COLOR(color);
  int result = 0;
  int acode = 0;
  int dcode = 0;
  int si = -1;
  int sj = -1;
  
  if (stackp == 0) {
    if (worm[ai][aj].attack_code != 0 && worm[ai][aj].defend_code == 0)
      return 0;
    si = I(worm[ai][aj].defense_point);
    sj = J(worm[ai][aj].defense_point);
  }
  else {
    attack_and_defend(ai, aj, &acode, NULL, NULL, &dcode, &si, &sj);
    if (acode != 0 && dcode == 0)
      return 0;
  }
  
  if (trymove2(ti, tj, other, "does_attack-A", ai, aj,
	      EMPTY, -1, -1)) {
    if (!BOARD(ai, aj) || !find_defense(ai, aj, NULL, NULL)) {
      result = WIN;
      increase_depth_values();
      if (si != -1 && trymove2(si, sj, color, "does_attack-B", ai, aj,
			      EMPTY, -1, -1)) {
	if (BOARD(ai, aj) && !attack(ai, aj, NULL, NULL))
	  result = 0;
	popgo();
      }
      decrease_depth_values();
    }
    popgo();
  }

  return result;
}


/*
 * does_defend(ti, tj, ai, aj) returns true if the move at (ti, tj)
 * defends (ai, aj). This means that it defends the string, and that
 * (ai, aj) can be captured if no defense is made.
 */

int
does_defend(int ti, int tj, int ai, int aj)
{
  int color = BOARD(ai, aj);
  int other = OTHER_COLOR(color);
  int result = 0;
  int si = -1;
  int sj = -1;

  if (stackp == 0) {
    if (worm[ai][aj].attack_code == 0)
      return 0;
    else {
      si = I(worm[ai][aj].attack_point);
      sj = J(worm[ai][aj].attack_point);
    }
  }
  else if (!attack(ai, aj, &si, &sj))
    return 0;

  gg_assert(si != -1 && sj != -1);
  
  if (trymove2(ti, tj, color, "does_defend-A", ai, aj,
	      EMPTY, -1, -1)) {
    if (!attack(ai, aj, NULL, NULL)) {
      result = 1;
      increase_depth_values();
      if (trymove2(si, sj, other, "does_defend-B", ai, aj,
		  EMPTY, -1, -1)) {
	if (!BOARD(ai, aj) || !find_defense(ai, aj, NULL, NULL))
	  result = 0;
	popgo();
      }
      decrease_depth_values();
    }
    popgo();
  }

  return result;
}


/* 
 * Example: somewhere(WHITE, 2, ai, aj, bi, bj, ci, cj).
 * 
 * Returns true if one of the vertices listed
 * satisfies p[i][j]==color. Here last_move is the
 * number of moves minus one.
 */

int
somewhere(int color, int last_move, ...)
{
  va_list ap;
  int k;
  
  va_start(ap, last_move);
  for (k = 0; k <= last_move; k++) {
    int ai, aj;

    ai = va_arg(ap, int);
    aj = va_arg(ap, int);

    if (BOARD(ai, aj) == color && 
	(stackp > 0 || dragon[ai][aj].matcher_status != DEAD))
      return 1;
  }

  return 0;
}


/* The function play_break_through_n() plays a sequence of moves,
 * alternating between the players and starting with color. After
 * having played through the sequence, the three last coordinate pairs
 * gives a position to be analyzed by break_through(), to see whether
 * either color has managed to enclose some stones and/or connected
 * his own stones. If any of the three last positions is empty, it's
 * assumed that the enclosure has failed, as well as the attempt to
 * connect.
 *
 * If one or more of the moves to play turns out to be illegal for
 * some reason, the rest of the sequence is played anyway, and
 * break_through() is called as if nothing special happened.
 *
 * Like break_through(), this function returns 1 if the attempt to
 * break through was succesful and 2 if it only managed to cut
 * through.
 */
   
int
play_break_through_n(int color, int num_moves, ...)
{
  va_list ap;
  int mcolor = color;
  int success = 0;
  int i;
  int played_moves = 0;
  int xi, xj;
  int yi, yj;
  int zi, zj;
  
  va_start(ap, num_moves);

  /* Do all the moves with alternating colors. */
  for (i = 0; i < num_moves; i++) {
    int ai, aj;

    ai = va_arg(ap, int);
    aj = va_arg(ap, int);

    if (ai != -1 && (trymove2(ai, aj, mcolor, "play_attack_defend_n", -1, -1,
			     EMPTY, -1, -1)
		     || tryko2(ai, aj, mcolor, "play_attack_defend_n",
			      EMPTY, -1, -1)))
      played_moves++;
    mcolor = OTHER_COLOR(mcolor);
  }

  /* Now do the real work. */
  xi = va_arg(ap, int);
  xj = va_arg(ap, int);
  yi = va_arg(ap, int);
  yj = va_arg(ap, int);
  zi = va_arg(ap, int);
  zj = va_arg(ap, int);
    
  /* Temporarily increase the depth values with the number of explicitly
   * placed stones.
   */
#if 0
  modify_depth_values(played_moves);
#endif
  
  if (!BOARD(xi, xj) || !BOARD(yi, yj) || !BOARD(zi, zj))
    success = 1;
  else
    success = break_through(xi, xj, yi, yj, zi, zj);

#if 0
  modify_depth_values(-played_moves);
#endif
  
  /* Pop all the moves we could successfully play. */
  for (i = 0; i < played_moves; i++)
    popgo();

  va_end(ap);
  return success;
}


/* The function play_attack_defend_n() plays a sequence of moves,
 * alternating between the players and starting with color. After
 * having played through the sequence, the last coordinate pair gives
 * a target to attack or defend, depending on the value of do_attack.
 * If there is no stone present to attack or defend, it is assumed
 * that it has already been captured. If one or more of the moves to
 * play turns out to be illegal for some reason, the rest of the
 * sequence is played anyway, and attack/defense is tested as if
 * nothing special happened.
 *
 * A typical use for these functions is to set up a ladder in an
 * autohelper and see whether it works or not.
 */
   
int
play_attack_defend_n(int color, int do_attack, int num_moves, ...)
{
  va_list ap;
  int mcolor = color;
  int success = 0;
  int i;
  int played_moves = 0;
  int zi, zj;
  
  va_start(ap, num_moves);

  /* Do all the moves with alternating colors. */
  for (i = 0; i < num_moves; i++) {
    int ai, aj;

    ai = va_arg(ap, int);
    aj = va_arg(ap, int);

    if (ai != -1 && (trymove2(ai, aj, mcolor, "play_attack_defend_n", -1, -1,
			     EMPTY, -1, -1)
		     || tryko2(ai, aj, mcolor, "play_attack_defend_n",
			      EMPTY, -1, -1)))
      played_moves++;
    mcolor = OTHER_COLOR(mcolor);
  }

  /* Now do the real work. */
  zi = va_arg(ap, int);
  zj = va_arg(ap, int);

  /* Temporarily increase the depth values with the number of explicitly
   * placed stones.
   *
   * This improves the reading of pattern constraints but
   * unfortunately tends to be too expensive. For the time being it is
   * disabled.
   */
#if 0
  modify_depth_values(played_moves);
#endif
  
  if (do_attack) {
    if (!BOARD(zi, zj))
      success = 1;
    else
      success = attack(zi, zj, NULL, NULL);
  }
  else {
    if (!BOARD(zi, zj))
      success = 0;
    else {
      int dcode = find_defense(zi, zj, NULL, NULL);
      if (dcode == 0 && !attack(zi, zj, NULL, NULL))
	success = 1;
      else
	success = dcode;
    }
  }

#if 0
  modify_depth_values(-played_moves);
#endif
  
  /* Pop all the moves we could successfully play. */
  for (i = 0; i < played_moves; i++)
    popgo();

  va_end(ap);
  return success;
}


/* The function play_attack_defend2_n() plays a sequence of moves,
 * alternating between the players and starting with color. After
 * having played through the sequence, the two last coordinate pairs
 * give two targets to simultaneously attack or defend, depending on
 * the value of do_attack. If there is no stone present to attack or
 * defend, it is assumed that it has already been captured. If one or
 * more of the moves to play turns out to be illegal for some reason,
 * the rest of the sequence is played anyway, and attack/defense is
 * tested as if nothing special happened.
 *
 * A typical use for these functions is to set up a crosscut in an
 * autohelper and see whether at least one cutting stone can be
 * captured.
 */
   
int
play_attack_defend2_n(int color, int do_attack, int num_moves, ...)
{
  va_list ap;
  int mcolor = color;
  int success = 0;
  int i;
  int played_moves = 0;
  int yi, yj;
  int zi, zj;
  
  va_start(ap, num_moves);

  /* Do all the moves with alternating colors. */
  for (i = 0; i < num_moves; i++) {
    int ai, aj;

    ai = va_arg(ap, int);
    aj = va_arg(ap, int);

    if (ai != -1 && (trymove2(ai, aj, mcolor, "play_attack_defend_n", -1, -1,
			     EMPTY, -1, -1)
		     || tryko2(ai, aj, mcolor, "play_attack_defend_n",
			      EMPTY, -1, -1)))
      played_moves++;
    mcolor = OTHER_COLOR(mcolor);
  }

  /* Now do the real work. */
  yi = va_arg(ap, int);
  yj = va_arg(ap, int);
  zi = va_arg(ap, int);
  zj = va_arg(ap, int);

  /* Temporarily increase the depth values with the number of explicitly
   * placed stones.
   */
#if 0
  modify_depth_values(played_moves);
#endif
  
  if (do_attack) {
    if (!BOARD(yi, yj) || !BOARD(zi, zj) || attack_either(yi, yj, zi, zj))
      success = 1;
  }
  else {
    if (BOARD(yi, yj) && BOARD(zi, zj) && defend_both(yi, yj, zi, zj))
      success = 1;
  }

#if 0
  modify_depth_values(-played_moves);
#endif
  
  /* Pop all the moves we could successfully play. */
  for (i = 0; i < played_moves; i++)
    popgo();

  va_end(ap);
  return success;
}


/* find_lunch(m, n, *wi, *wj, *ai, *aj) looks for a worm adjoining the
 * string at (m,n) which can be easily captured. Whether or not it can
 * be defended doesn't matter.
 *
 * Returns the location of the string in (*wi, *wj), and the location
 * of the attacking move in (*ai, *aj).
 *
 * FIXME: Move this function to worm.c.
 */
	
int
find_lunch(int m, int n, int *wi, int *wj, int *ai, int *aj)
{
  int i, j, vi, vj;

  ASSERT2(BOARD(m, n) != 0, m, n);
  ASSERT2(stackp == 0, m, n);

  vi = -1;
  vj = -1;
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++)
      if (BOARD(i, j) == OTHER_COLOR(BOARD(m, n)))
	if ((   i > 0            && is_same_worm(i-1, j, m, n))
	    || (i < board_size-1 && is_same_worm(i+1, j, m, n))
	    || (j > 0            && is_same_worm(i, j-1, m, n))
	    || (j < board_size-1 && is_same_worm(i, j+1, m, n))
	    || (i > 0 && j > 0
		&& is_same_worm(i-1, j-1, m, n))
	    || (i > 0 && j < board_size-1
		&& is_same_worm(i-1, j+1, m, n))
	    || (i < board_size-1 && j > 0
		&& is_same_worm(i+1, j-1, m, n))
	    || (i < board_size-1 && j < board_size-1
		&& is_same_worm(i+1, j+1, m, n)))
	  if (worm[i][j].attack_code != 0 && !worm[i][j].ko) {
	    /*
	     * If several adjacent lunches are found, we pick the 
	     * juiciest. First maximize cutstone, then minimize liberties. 
	     * We can only do this if the worm data is available, 
	     * i.e. if stackp==0.
	     */
	    if (vi == -1
		|| worm[i][j].cutstone > worm[vi][vj].cutstone 
		|| (worm[i][j].cutstone == worm[vi][vj].cutstone 
		    && worm[i][j].liberties < worm[vi][vj].liberties)) {
	      vi = I(worm[i][j].origin);
	      vj = J(worm[i][j].origin);
	      if (ai) *ai = I(worm[i][j].attack_point);
	      if (aj) *aj = J(worm[i][j].attack_point);
	    }
	  }

  if (vi != -1) {
    if (wi) *wi = vi;
    if (wj) *wj = vj;
    return 1;
  }
  else
    return 0;
}


/* 
 * It is assumed in reading a ladder if stackp >= depth that
 * as soon as a bounding stone is in atari, the string is safe.
 * It is used similarly at other places in reading.c to implement simplifying
 * assumptions when stackp is large. DEPTH is the default value of depth.
 *
 * Unfortunately any such scheme invites the ``horizon effect.'' Increasing
 * DEPTH will make the program stronger and slower.
 *
 */

/* Tactical reading using C functions */
#define DEPTH                16
#define BRANCH_DEPTH         13
#define BACKFILL_DEPTH       12
#define BACKFILL2_DEPTH       5
#define SUPERSTRING_DEPTH     7
#define FOURLIB_DEPTH         7
#define KO_DEPTH              8

#define AA_DEPTH              6

/* Pattern based reading */
#define OWL_DISTRUST_DEPTH    6
#define OWL_BRANCH_DEPTH      8
#define OWL_READING_DEPTH    20
#define OWL_NODE_LIMIT     1000

/* Set the various reading depth parameters. If mandated_depth_value
 * is not -1 that value is used; otherwise the depth values are
 * set as a function of level. The parameter mandated_depth_value
 * can be set at the command line to force a particular value of
 * depth; normally it is -1.
 */

void
set_depth_values(int level)
{
  static int node_limits[10] = {500, 450, 400, 325, 275,
				200, 150, 100, 75, 50};
  if (level >= 10) {
    depth               = gg_max(6, DEPTH - 10 + level);
    ko_depth            = gg_max(1, KO_DEPTH - 10 + level);
    backfill_depth      = gg_max(2, BACKFILL_DEPTH - 10 + level);
    backfill2_depth     = gg_max(1, BACKFILL2_DEPTH - 10 + level);
    superstring_depth   = gg_max(1, SUPERSTRING_DEPTH - 10 + level);
    branch_depth        = gg_max(3, BRANCH_DEPTH - 10 + level);
    fourlib_depth       = gg_max(1, FOURLIB_DEPTH - 10 + level);
    aa_depth            = gg_max(0, AA_DEPTH - 10 + level);
    owl_distrust_depth  = gg_max(1, OWL_DISTRUST_DEPTH - 5 + level/2);
    owl_branch_depth    = gg_max(2, OWL_BRANCH_DEPTH - 5 + level/2);
    owl_reading_depth   = gg_max(5, OWL_READING_DEPTH - 5 + level/2);
    if (level == 10)
      owl_node_limit    = OWL_NODE_LIMIT;
    else
      owl_node_limit    = OWL_NODE_LIMIT * pow(1.5, -10 + level);
    urgent              = 0;
  }
  else if (level > 7) {
    depth               = gg_max(6, DEPTH - 9 + level);
    ko_depth            = gg_max(1, KO_DEPTH - 9 + level);
    backfill_depth      = gg_max(2, BACKFILL_DEPTH - 9 + level);
    backfill2_depth     = gg_max(1, BACKFILL2_DEPTH - 9 + level);
    superstring_depth   = 0 ;
    branch_depth        = gg_max(3, BRANCH_DEPTH - 9 + level);
    if (level < 9)
      fourlib_depth     = gg_max(1, FOURLIB_DEPTH - 8 + level);
    else
      fourlib_depth     = gg_max(1, FOURLIB_DEPTH - 9 + level);
    aa_depth            = gg_max(0, AA_DEPTH - 10 + level);
    owl_distrust_depth  = gg_max(1, OWL_DISTRUST_DEPTH - 5 
			      + (level+1)/2);
    owl_branch_depth    = gg_max(2, OWL_BRANCH_DEPTH - 5 + (level+1)/2);
    owl_reading_depth   = gg_max(5, OWL_READING_DEPTH - 5 + (level+1)/2);
    owl_node_limit      = (OWL_NODE_LIMIT * node_limits[9 - level] /
			   node_limits[0]);
    owl_node_limit      = gg_max(20, owl_node_limit);
    urgent              = 0;
  }
  else if (level == 7) {
    depth               = gg_max(6, DEPTH - 1);
    ko_depth            = gg_max(1, KO_DEPTH - 1);
    backfill_depth      = gg_max(2, BACKFILL_DEPTH - 1);
    backfill2_depth     = gg_max(1, BACKFILL2_DEPTH - 1);
    superstring_depth   = 0 ;
    branch_depth        = gg_max(3, BRANCH_DEPTH - 1);
    if (level < 9)
      fourlib_depth     = gg_max(1, FOURLIB_DEPTH);
    else
      fourlib_depth     = gg_max(1, FOURLIB_DEPTH - 1);
    aa_depth            = gg_max(0, AA_DEPTH - 2);
    owl_distrust_depth  = gg_max(1, OWL_DISTRUST_DEPTH - 1);
    owl_branch_depth    = gg_max(2, OWL_BRANCH_DEPTH - 5 + (level+1)/2);
    owl_reading_depth   = gg_max(5, OWL_READING_DEPTH - 5 + (level+1)/2);
    owl_node_limit      = (OWL_NODE_LIMIT * node_limits[9 - level] /
			   node_limits[0]);
    owl_node_limit      = gg_max(20, owl_node_limit);
    urgent              = 0;
  }
  else if (level < 7) {
    depth               = gg_max(6, DEPTH - 8 + level);
    ko_depth            = gg_max(1, KO_DEPTH - 9 + level);
    backfill_depth      = gg_max(2, BACKFILL_DEPTH - 8 + level);
    backfill2_depth     = gg_max(1, BACKFILL2_DEPTH - 8 + level);
    superstring_depth   = 0 ;
    branch_depth        = gg_max(3, BRANCH_DEPTH - 8 + level);
    if (level < 9)
      fourlib_depth     = gg_max(1, FOURLIB_DEPTH - 7 + level);
    else
      fourlib_depth     = gg_max(1, FOURLIB_DEPTH - 8 + level);
    aa_depth            = gg_max(0, AA_DEPTH - 9 + level);
    owl_distrust_depth  = gg_max(1, OWL_DISTRUST_DEPTH - 5
			      + (level+1)/2);
    owl_branch_depth    = gg_max(2, OWL_BRANCH_DEPTH - 4 + level/2);
    owl_reading_depth   = gg_max(5, OWL_READING_DEPTH - 4 + level/2);
    owl_node_limit      = (OWL_NODE_LIMIT * node_limits[8 - level] /
			   node_limits[0]);
    owl_node_limit      = gg_max(20, owl_node_limit);
    urgent              = 0;
  }


  if (mandated_depth != -1)
    depth = mandated_depth;
  if (mandated_backfill_depth != -1)
    backfill_depth = mandated_backfill_depth;
  if (mandated_backfill2_depth != -1)
    backfill2_depth = mandated_backfill2_depth;
  if (mandated_superstring_depth != -1)
    superstring_depth = mandated_superstring_depth;
  if (mandated_fourlib_depth != -1)
    fourlib_depth = mandated_fourlib_depth;
  if (mandated_ko_depth != -1)
    ko_depth = mandated_ko_depth;
  if (mandated_branch_depth != -1)
    branch_depth = mandated_branch_depth;
  if (mandated_aa_depth != -1)
    aa_depth = mandated_aa_depth;
  if (mandated_owl_distrust_depth != -1)
    owl_distrust_depth = mandated_owl_distrust_depth;
  if (mandated_owl_branch_depth != -1)
    owl_branch_depth = mandated_owl_branch_depth;
  if (mandated_owl_reading_depth != -1)
    owl_reading_depth = mandated_owl_reading_depth;
  if (mandated_owl_node_limit != -1)
    owl_node_limit = mandated_owl_node_limit;
}


/*
 * Modify the various tactical reading depth parameters. This is
 * typically used to avoid horizon effects. By temporarily increasing
 * the depth values when trying some move, one can avoid that an
 * irrelevant move seems effective just because the reading hits a
 * depth limit earlier than it did when reading only on relevant
 * moves.
 */

void
modify_depth_values(int n)
{
  depth              += n;
  backfill_depth     += n;
  backfill2_depth    += n;
  superstring_depth  += n;
  branch_depth       += n;
  fourlib_depth      += n;
  ko_depth           += n;
}

void
increase_depth_values(void)
{
  modify_depth_values(1);
}

void
decrease_depth_values(void)
{
  modify_depth_values(-1);
}

/* These functions allow more drastic temporary modifications of the
 * depth values. Typical use is to turn certain depth values way down
 * for reading where speed is more important than accuracy, e.g. for
 * the influence function.
 */

static int save_depth;
static int save_backfill_depth;
static int save_backfill2_depth;
static int save_superstring_depth;
static int save_branch_depth;
static int save_fourlib_depth;
static int save_ko_depth;

/* Currently this function is never called. */

void
set_temporary_depth_values(int d, int b, int f, int k, int br, int b2, int ss)
{
  save_depth             = depth;
  save_backfill_depth    = backfill_depth;
  save_backfill2_depth   = backfill2_depth;
  save_superstring_depth = superstring_depth;
  save_branch_depth      = branch_depth;
  save_fourlib_depth     = fourlib_depth;
  save_ko_depth          = ko_depth;

  depth             = d;
  backfill_depth    = b;
  backfill2_depth   = b2;
  superstring_depth = ss;
  branch_depth      = br;
  fourlib_depth     = f;
  ko_depth          = k;
}

void
restore_depth_values()
{
  depth             = save_depth;
  backfill_depth    = save_backfill_depth;
  backfill2_depth   = save_backfill2_depth;
  superstring_depth = save_superstring_depth;
  branch_depth      = save_branch_depth;
  fourlib_depth     = save_fourlib_depth;
  ko_depth          = save_ko_depth;
}



/* Play a stone at (m, n) and count the number of liberties for the
 * resulting string. This requires (m, n) to be empty.
 *
 * This function differs from approxlib() by the fact that it removes
 * captured stones before counting the liberties.
 */

int
accurate_approxlib(int pos, int color, int maxlib, int *libs)
{
  int liberties = 0;

  ASSERT1(board[pos] == EMPTY, pos);
  ASSERT1(color != EMPTY, pos);

  /* Use tryko() since we don't care whether the move would violate
   * the ko rule.
   */
  if (tryko(pos, color, "accurate approxlib", EMPTY, 0)) {
    if (libs != NULL)
      liberties = findlib(pos, maxlib, libs);
    else
      liberties = countlib(pos);
    popgo();
  }
  
  return liberties;
}


/* This function will detect some blunders. If the move reduces the
 * number of liberties of an adjacent friendly string, there is a
 * danger that the move could backfire, so the function checks that no
 * friendly worm which was formerly not attackable becomes attackable,
 * and it checks that no opposing worm which was not defendable
 * becomes defendable. Only worms with worm.size>size are checked.
 *
 * For use when called from fill_liberty, this function may optionally
 * return a point of defense, which, if taken, will presumably make
 * the move at (i, j) safe on a subsequent turn.
 */

int
confirm_safety(int i, int j, int color, int size, int *di, int *dj)
{
  int pos = POS(i, j);
  int libs[5];
  int liberties = accurate_approxlib(pos, color, 5, libs);
  int other = OTHER_COLOR(color);
  int issafe = 1;
  int m, n;
  int ai, aj;
  int trouble = 0;
  int k;
  int save_verbose = verbose;

  if (di) *di = -1;
  if (dj) *dj = -1;

  TRACE("Checking safety of a %s move at %m\n", 
	color_to_string(color), i, j);

  if (verbose > 0)
    verbose--;
  
  if (!atari_atari_confirm_safety(color, i, j, &ai, &aj, size)) {
    ASSERT_ON_BOARD2(ai, aj);
    if (di) *di = ai;
    if (dj) *dj = aj;
    TRACE("Combination attack appears at %m.\n", ai, aj);
    verbose = save_verbose;
    return 0;
  }

  if (liberties > 4) {
    verbose = save_verbose;
    return 1;
  }

  for (k = 0; k < 4; k++) {
    int bi = i + deltai[k];
    int bj = j + deltaj[k];
    if (ON_BOARD2(bi, bj)
	&& BOARD(bi, bj) == color
	&& liberties <= worm[bi][bj].liberties) {
      trouble = 1;
      if (dragon[bi][bj].matcher_status == ALIVE
	  && DRAGON2(bi, bj).safety != INVINCIBLE
	  && DRAGON2(bi, bj).safety != STRONGLY_ALIVE
	  && dragon[bi][bj].size >= size
	  && !owl_confirm_safety(i, j, bi, bj, di, dj)) {
	verbose = save_verbose;
	return 0;
      }
    }
  }

  if (!trouble) {
    verbose = save_verbose;
    return 1;
  }

  /* Need to increase the depth values during this reading to avoid
   * horizon effects.
   */
  increase_depth_values();
  
  if (trymove2(i, j, color, NULL, -1, -1, EMPTY, -1, -1)) {
    for (m = 0; issafe && m < board_size; m++)
      for (n = 0; issafe && n < board_size; n++)
	if (issafe
	    && BOARD(m, n)
	    && worm[m][n].origin == POS(m, n)
	    && (m != i || n != j)) {
	  if (BOARD(m, n) == color
	      && worm[m][n].attack_code == 0
	      && worm[m][n].size >= size
	      && attack(m, n, NULL, NULL)) {
	    if (di)
	      find_defense(m, n, di, dj);
	    issafe = 0;
	    TRACE("After %m Worm at %m becomes attackable.\n", i, j, m, n);
	  }
	  else if (BOARD(m, n) == other
		   && worm[m][n].attack_code != 0
		   && worm[m][n].defend_code == 0
		   && worm[m][n].size >= size
		   && find_defense(m, n, NULL, NULL)) {
	    /* Also ask the owl code whether the string can live
	     * strategically. To do this we need to temporarily undo
	     * the trymove2().
	     */
	    popgo();
	    decrease_depth_values();
	    if (owl_does_attack(i, j, m, n) != WIN)
	      issafe = 0;
	    trymove2(i, j, color, NULL, -1, -1, EMPTY, -1, -1);
	    increase_depth_values();
	    
	    if (!issafe) {
	      if (di)
		attack(m, n, di, dj);
	      
	      TRACE("After %m worm at %m becomes defendable.\n",
		    i, j, m, n);
	    }
	  }
	}
    
    if (liberties == 2) {
      if (double_atari(I(libs[0]), J(libs[0]), other)) {
	if (di && safe_move(libs[0], color) == WIN) {
	  *di = I(libs[0]);
	  *dj = J(libs[0]);
	}
	issafe = 0;
	TRACE("Double threat appears at %1m.\n", libs[0]);
      }
      else if (double_atari(I(libs[1]), J(libs[1]), other)) {
	if (di && safe_move(libs[1], color) == WIN) {
	  *di = I(libs[1]);
	  *dj = J(libs[1]);
	}
	issafe = 0;
	TRACE("Double threat appears at %1m.\n", libs[1]);
      }
    }
    popgo();
  }
  
  /* Reset the depth values. */
  decrease_depth_values();
  verbose = save_verbose;
  return issafe;
}


/* Returns true if a move by (color) fits the following shape:
 * 
 *
 *    X*        (O=color)
 *    OX
 * 
 * capturing one of the two X strings. The name is a slight
 * misnomer since this includes attacks which are not necessarily
 * double ataris, though the common double atari is the most
 * important special case.
 */

int
double_atari(int m, int n, int color)
{
  int other = OTHER_COLOR(color);
  int k;

  if (!ON_BOARD2(m, n))
    return 0;

  /* Loop over the diagonal directions. */
  for (k = 4; k < 8; k++) {
    int dm = deltai[k];
    int dn = deltaj[k];
    
    /* because (m,n) and (m+dm,n+dn) are opposite
     * corners of a square, ON_BOARD2(m,n) && ON_BOARD2(m+dm,n+dn)
     * implies ON_BOARD2(m+dm,n) and ON_BOARD2(n,n+dn)
     */
    if (ON_BOARD2(m+dm, n+dn) && ON_BOARD2(m, n)
        && BOARD(m+dm, n+dn) == color
	&& BOARD(m, n+dn) == other
	&& BOARD(m+dm, n) == other
	&& trymove2(m, n, color, "double_atari", -1, -1, EMPTY, -1, -1)) {
      if (countlib2(m, n) > 1
	  && (BOARD(m, n+dn) == EMPTY || BOARD(m+dm, n) == EMPTY 
	      || !defend_both(m, n+dn, m+dm, n))) {
	popgo();
	return 1;
      }
      popgo();
    }
  }
  
  return 0;
}
    

/* Find those worms of the given color that can never be captured,
 * even if the opponent is allowed an arbitrary number of consecutive
 * moves. The coordinates of the origins of these worms are written to
 * the wormi, wormj arrays and the number of non-capturable worms is
 * returned.
 *
 * The algorithm is to cycle through the worms until none remains or
 * no more can be captured. A worm is removed when it is found to be
 * capturable, by letting the opponent try to play on all its
 * liberties. If the attack fails, the moves are undone. When no more
 * worm can be removed in this way, the remaining ones are
 * unconditionally alive.
 *
 * After this, unconditionally dead opponent worms and unconditional
 * territory are identified. To find these, we continue from the
 * position obtained at the end of the previous operation (only
 * unconditionally alive strings remain for color) with the following
 * steps:
 *
 * 1. Play opponent stones on all liberties of the unconditionally
 *    alive strings except where illegal. (That the move order may
 *    determine exactly which liberties can be played legally is not
 *    important. Just pick an arbitrary order).
 * 2. Recursively extend opponent strings in atari, except where this
 *    would be suicide.
 * 3. Play an opponent stone anywhere it can get two empty
 *    neighbors. (I.e. split big eyes into small ones).
 * 4. Play an opponent stone anywhere it can get one empty
 *    neighbor. (I.e. reduce two space eyes to one space eyes.)
 *
 * Remaining opponent strings in atari and remaining liberties of the
 * unconditionally alive strings constitute the unconditional
 * territory.
 *
 * Opponent strings from the initial position placed on
 * unconditional territory are unconditionally dead.
 *
 * On return, unconditional_territory[][] is 1 where color has
 * unconditionally alive stones, 2 where it has unconditional
 * territory, and 0 otherwise.
 */

void
unconditional_life(int unconditional_territory[MAX_BOARD][MAX_BOARD],
		   int color)
{
  int something_captured = 1; /* To get into the first turn of the loop. */
  int found_one;
  int moves_played = 0;
  int save_moves;
  int m, n;
  int k;
  int libs[MAXLIBS];
  int liberties;
  int other = OTHER_COLOR(color);
  
  while (something_captured) {
    /* Nothing captured so far in this turn of the loop. */
    something_captured = 0;

    /* Visit all friendly strings on the board. */
    for (m = 0; m < board_size; m++)
      for (n = 0; n < board_size; n++) {
	int pos = POS(m, n);
	if (board[pos] != color || !is_worm_origin(m, n, m, n))
	  continue;
	
	/* Try to capture the worm at (m, n). */
	liberties = findlib(pos, MAXLIBS, libs);
	save_moves = moves_played;
	for (k = 0; k < liberties; k++) {
	  if (trymove(libs[k], other, "unconditional_life", pos, EMPTY, 0))
	    moves_played++;
	}

	/* Successful if already captured or a single liberty remains.
	 * Otherwise we must rewind and take back the last batch of moves.
	 */
	if (board[pos] == EMPTY)
	  something_captured = 1;
	else if (findlib(pos, 2, libs) == 1) {
	  /* Need to use tryko as a defense against the extreme case
           * when the only opponent liberty that is not suicide is an
           * illegal ko capture, like in this 5x5 position:
	   * +-----+
	   * |.XO.O|
	   * |XXOO.|
	   * |X.XOO|
	   * |XXOO.|
	   * |.XO.O|
	   * +-----+
	   */
	  int success = tryko(libs[0], other, "unconditional_life", EMPTY, 0);
	  gg_assert(success);
	  moves_played++;
	  something_captured++;
	}
	else
	  while (moves_played > save_moves) {
	    popgo();
	    moves_played--;
	  }
      }
  }

  /* The strings still remaining are uncapturable. Now see which
   * opponent strings can survive.
   *
   * 1. Play opponent stones on all liberties of the unconditionally
   *    alive strings except where illegal.
   */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      int pos = POS(m, n);
      if (board[pos] != color || !is_worm_origin(m, n, m, n))
	continue;
      
      /* Play as many liberties as we can. */
      liberties = findlib(pos, MAXLIBS, libs);
      for (k = 0; k < liberties; k++) {
	if (trymove(libs[k], other, "unconditional_life", pos, EMPTY, 0))
	  moves_played++;
      }
    }

  /* 2. Recursively extend opponent strings in atari, except where this
   *    would be suicide.
   */
  found_one = 1;
  while (found_one) {
    /* Nothing found so far in this turn of the loop. */
    found_one = 0;

    for (m = 0; m < board_size; m++)
      for (n = 0; n < board_size; n++) {
	int pos = POS(m, n);
	if (board[pos] != other || countlib(pos) > 1)
	  continue;
	
	/* Try to extend the string at (m, n). */
	findlib(pos, 1, libs);
	if (trymove(libs[0], other, "unconditional_life", pos, EMPTY, 0)) {
	  moves_played++;
	  found_one = 1;
	}
      }
  }

  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      int pos = POS(m, n);
      int apos;
      int bpos;
      int aopen, bopen;
      int alib, blib;
      if (board[pos] != other || countlib(pos) != 2)
	continue;
      findlib(pos, 2, libs);
      apos = libs[0];
      bpos = libs[1];
      if (abs(I(apos) - I(bpos)) + abs(J(apos) - J(bpos)) != 1)
	continue;

      /* Only two liberties and these are adjacent. Play one. We want
       * to maximize the number of open liberties. In this particular
       * situation we can count this with approxlib for the opposite
       * color. If the number of open liberties is the same, we
       * maximize the total number of obtained liberties.
       * Two relevant positions:
       *
       * |XXX. 
       * |OOXX    |XXXXXXX
       * |O.OX    |OOXOOOX
       * |..OX    |..OO.OX
       * +----    +-------
       */
      aopen = approxlib(apos, color, 4, NULL);
      bopen = approxlib(bpos, color, 4, NULL);
      alib  = approxlib(apos, other, 4, NULL);
      blib  = approxlib(bpos, other, 4, NULL);

      if (aopen > bopen || (aopen == bopen && alib >= blib)) {
	trymove(apos, other, "unconditional_life", pos, EMPTY, 0);
	moves_played++;
      }
      else {
	trymove(bpos, other, "unconditional_life", pos, EMPTY, 0);
	moves_played++;
      }
    }
      
  /* Identify unconditionally alive stones and unconditional territory. */
  memset(unconditional_territory, 0, sizeof(int) * MAX_BOARD * MAX_BOARD);
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      int pos = POS(m, n);
      if (board[pos] == color) {
	unconditional_territory[m][n] = 1;
	if (is_worm_origin(m, n, m, n)) {
	  liberties = findlib(pos, MAXLIBS, libs);
	  for (k = 0; k < liberties; k++)
	    unconditional_territory[I(libs[k])][J(libs[k])] = 2;
	}
      }
      else if (board[pos] == other && countlib(pos) == 1) {
	unconditional_territory[m][n] = 2;
	findlib(pos, 1, libs);
	unconditional_territory[I(libs[0])][J(libs[0])] = 2;
      }
    }

  /* Take back all moves. */
  while (moves_played > 0) {
    popgo();
    moves_played--;
  }
}


/* Score the game and determine the winner */

void
who_wins(int color, FILE *outfile)
{
  float result;

#if 0
  float white_score;
  float black_score;
  int winner;
#endif

  if (color != BLACK && color != WHITE)
    color = BLACK;

#if 0
  /* Use the aftermath code to compute the final score. (Slower but
   * more reliable.) 
   */
  result = aftermath_compute_score(color, komi);
  if (result > 0.0)
    winner = WHITE;
  else {
    winner = BLACK;
    result = -result;
  }
#endif

  result = estimate_score(NULL, NULL);
  if (result == 0.0)
    fprintf(outfile, "Result: jigo   ");
  else
    fprintf(outfile, "Result: %c+%.1f   ",
	    (result > 0.0) ? 'W' : 'B', gg_abs(result));
}



/* Find the stones of an extended string, where the extensions are
 * through the following kinds of connections:
 *
 * 1. Solid connections (just like ordinary string).
 *
 *    OO
 *
 * 2. Diagonal connection or one space jump through an intersection
 *    where an opponent move would be suicide or self-atari.
 *
 *    ...
 *    O.O
 *    XOX
 *    X.X
 *
 * 3. Bamboo joint.
 *
 *    OO
 *    ..
 *    OO
 *
 * 4. Diagonal connection where both adjacent intersections are empty.
 *
 *    .O
 *    O.
 *
 * 5. Connection through adjacent or diagonal tactically captured stones.
 *    Connections of this type are omitted when the superstring code is
 *    called from reading.c, but included when the superstring code is
 *    called from owl.c
 */

static void
do_find_superstring(int str, int *num_stones, int *stones,
		    int *num_lib, int *libs, int maxlibs,
		    int *num_adj, int *adjs, int liberty_cap,
		    int proper, int type);

static void
superstring_add_string(int str,
		       int *num_my_stones, int *my_stones,
		       int *num_stones, int *stones,
		       int *num_libs, int *libs, int maxlibs,
		       int *num_adj, int *adjs, int liberty_cap,
		       char mx[BOARDMAX],
		       char ml[BOARDMAX],
		       char ma[BOARDMAX],
		       int do_add);

void
find_superstring(int str, int *num_stones, int *stones)
{
  do_find_superstring(str, num_stones, stones,
		      NULL, NULL, 0,
		      NULL, NULL, 0,
		      0, 1);
}


/* This function computes the superstring at (str) as described above,
 * but omitting connections of type 5. Then it constructs a list of
 * liberties of the superstring which are not already liberties of
 * (str).
 *
 * If liberty_cap is nonzero, only liberties of substrings of the
 * superstring which have fewer than liberty_cap liberties are
 * generated.
 */

void
find_superstring_liberties(int str,
			   int *num_libs, int *libs, int liberty_cap)
{
  do_find_superstring(str, NULL, NULL,
		      num_libs, libs, MAX_LIBERTIES,
		      NULL, NULL, liberty_cap,
		      0, 0);
}

/* This function is the same as find_superstring_liberties, but it
 * omits those liberties of the string (str), presumably since
 * those have already been treated elsewhere.
 *
 * If liberty_cap is nonzero, only liberties of substrings of the
 * superstring which have at most liberty_cap liberties are
 * generated.
 */

void
find_proper_superstring_liberties(int str, 
				  int *num_libs, int *libs, 
				  int liberty_cap)
{
  do_find_superstring(str, NULL, NULL,
		      num_libs, libs, MAX_LIBERTIES,
		      NULL, NULL, liberty_cap,
		      1, 0);
}

/* This function computes the superstring at (str) as described above,
 * but omitting connections of type 5. Then it constructs a list of
 * liberties of the superstring which are not already liberties of
 * (str).
 *
 * If liberty_cap is nonzero, only liberties of substrings of the
 * superstring which have fewer than liberty_cap liberties are
 * generated.
 */

void
find_superstring_stones_and_liberties(int str,
				      int *num_stones, int *stones,
				      int *num_libs, int *libs,
				      int liberty_cap)
{
  do_find_superstring(str, num_stones, stones,
		      num_libs, libs, MAX_LIBERTIES,
		      NULL, NULL, liberty_cap,
		      0, 0);
}

/* analogous to chainlinks, this function finds boundary chains of the
 * superstring at (m, n), including those which are boundary chains of
 * (m, n) itself. If liberty_cap != 0, only those boundary chains with
 * <= liberty_cap liberties are reported.
 */

void
superstring_chainlinks(int str, 
		       int *num_adj, int adjs[MAXCHAIN],
		       int liberty_cap)
{
  do_find_superstring(str, NULL, NULL,
		      NULL, NULL, 0,
		      num_adj, adjs, liberty_cap,
		      0, 2);
}


/* analogous to chainlinks, this function finds boundary chains of the
 * superstring at (m, n), omitting those which are boundary chains of
 * (m, n) itself. If liberty_cap != 0, only those boundary chains with
 * <= liberty_cap liberties are reported.
 */

void
proper_superstring_chainlinks(int str,
			      int *num_adj, int adjs[MAXCHAIN],
			      int liberty_cap)
{
  do_find_superstring(str, NULL, NULL,
		      NULL, NULL, 0,
		      num_adj, adjs, liberty_cap,
		      1, 2);
}

/* Do the real work finding the superstring and recording stones,
 * liberties, and/or adjacent strings.
 */
static void
do_find_superstring(int str, int *num_stones, int *stones,
		    int *num_libs, int *libs, int maxlibs,
		    int *num_adj, int *adjs, int liberty_cap,
		    int proper, int type)
{
  int num_my_stones;
  int my_stones[MAX_BOARD * MAX_BOARD];
  
  char mx[BOARDMAX]; /* stones */
  char ml[BOARDMAX]; /* liberties */
  char ma[BOARDMAX]; /* adjacent strings */

  int k, l, r;
  int color = board[str];
  int other = OTHER_COLOR(color);

  memset(mx, 0, sizeof(mx));
  memset(ml, 0, sizeof(ml));
  memset(ma, 0, sizeof(ma));

  if (num_stones)
    *num_stones = 0;
  if (num_libs)
    *num_libs = 0;
  if (num_adj)
    *num_adj = 0;

  /* Include the string itself in the superstring. Only record stones,
   * liberties, and/or adjacent strings if proper==0.
   */
  num_my_stones = 0;
  superstring_add_string(str, &num_my_stones, my_stones,
			 num_stones, stones,
			 num_libs, libs, maxlibs,
			 num_adj, adjs, liberty_cap,
			 mx, ml, ma,
			 !proper);

  /* Loop over all found stones, looking for more strings to include
   * in the superstring. The loop is automatically extended over later
   * found stones as well.
   */
  for (r = 0; r < num_my_stones; r++) {
    int pos = my_stones[r];

    for (k = 0; k < 4; k++) {
      /* List of relative coordinates. (pos) is marked by *.
       *
       *  ef.
       *  gb.
       *  *ac
       *  .d.
       *
       */
      int right = delta[k];
      int up = delta[(k+1)%4];
      
      int apos = pos + right;
      int bpos = pos + right + up;
      int cpos = pos + 2*right;
      int dpos = pos + right - up;
      int epos = pos + 2*up;
      int fpos = pos + right + 2*up;
      int gpos = pos + up;
      int unsafe_move;
      
      if (!ON_BOARD(apos))
	continue;
      
      /* Case 1. Nothing to do since stones are added string by string. */
            
      /* Case 2. */
      if (board[apos] == EMPTY) {
	if (type == 2)
	  unsafe_move = (approxlib(apos, other, 2, NULL) < 2);
	else
	  unsafe_move = is_self_atari(apos, other);
	
	if (unsafe_move && type == 1 && is_ko(apos, other, NULL))
	  unsafe_move = 0;
	
	if (unsafe_move) {
	  if (board[bpos] == color && !mx[bpos])
	    superstring_add_string(bpos, &num_my_stones, my_stones,
				   num_stones, stones,
				   num_libs, libs, maxlibs,
				   num_adj, adjs, liberty_cap,
				   mx, ml, ma, 1);
	  if (board[cpos] == color && !mx[cpos])
	    superstring_add_string(cpos, &num_my_stones, my_stones,
				   num_stones, stones,
				   num_libs, libs, maxlibs,
				   num_adj, adjs, liberty_cap,
				   mx, ml, ma, 1);
	  if (board[dpos] == color && !mx[dpos])
	    superstring_add_string(dpos, &num_my_stones, my_stones,
				   num_stones, stones,
				   num_libs, libs, maxlibs,
				   num_adj, adjs, liberty_cap,
				   mx, ml, ma, 1);
	}
      }
      
      /* Case 3. */
      if (board[fpos] == color && !mx[epos]
	  && board[apos] == color && board[epos] == color
	  && board[bpos] == EMPTY && board[gpos] == EMPTY)
	superstring_add_string(epos, &num_my_stones, my_stones,
			       num_stones, stones,
			       num_libs, libs, maxlibs,
			       num_adj, adjs, liberty_cap,
			       mx, ml, ma, 1);
      /* Don't bother with f, it is part of the string just added. */
      
      /* Case 4. */
      if (board[bpos] == color && !mx[bpos]
	  && board[apos] == EMPTY && board[gpos] == EMPTY)
	superstring_add_string(bpos, &num_my_stones, my_stones,
			       num_stones, stones,
			       num_libs, libs, maxlibs,
			       num_adj, adjs, liberty_cap,
			       mx, ml, ma, 1);
      
      /* Case 5. */
      if (type == 1)
	for (l = 0; l < 2; l++) {
	  int upos;
	  
	  if (l == 0) {
	    /* adjacent lunch */
	    upos = apos;
	  }
	  else {
	    /* diagonal lunch */
	    upos = bpos;
	  }
	  
	  if (board[upos] != other)
	    continue;
	  
	  upos = find_origin(upos);
	  
	  /* Only do the reading once. */
	  if (mx[upos] == 1)
	    continue;
	  
	  mx[upos] = 1;
	  
	  if (attack(I(upos), J(upos), NULL, NULL)
	      && !find_defense(I(upos), J(upos), NULL, NULL)) {
	    int lunch_stones[MAX_BOARD*MAX_BOARD];
	    int num_lunch_stones = findstones(upos, MAX_BOARD*MAX_BOARD,
					      lunch_stones);
	    int r, s;
	    for (r = 0; r < num_lunch_stones; r++)
	      for (s = 0; s < 8; s++) {
		int vpos = lunch_stones[r] + delta[s];
		if (board[vpos] == color && !mx[vpos])
		  superstring_add_string(vpos,
					 &num_my_stones, my_stones,
					 num_stones, stones,
					 num_libs, libs, maxlibs,
					 num_adj, adjs, liberty_cap,
					 mx, ml, ma, 1);
	      }
	  }
	}
      if (num_libs && maxlibs > 0 && *num_libs >= maxlibs)
	return;
    }
  }
}

/* Add a new string to a superstring. Record stones, liberties, and
 * adjacent strings as asked for.
 */
static void
superstring_add_string(int str,
		       int *num_my_stones, int *my_stones,
		       int *num_stones, int *stones,
		       int *num_libs, int *libs, int maxlibs,
		       int *num_adj, int *adjs, int liberty_cap,
		       char mx[BOARDMAX],
		       char ml[BOARDMAX],
		       char ma[BOARDMAX],
		       int do_add)
{
  int num_my_libs;
  int my_libs[MAXLIBS];
  int num_my_adj;
  int my_adjs[MAXCHAIN];
  int new_stones;
  int k;
  
  ASSERT1(mx[str] == 0, str);

  /* Pick up the stones of the new string. */
  new_stones = findstones(str, board_size * board_size,
			  &(my_stones[*num_my_stones]));
  
  mark_string(str, mx, 1);
  if (stones) {
    gg_assert(num_stones);
    for (k = 0; k < new_stones; k++) {
      if (do_add) {
	stones[*num_stones] = my_stones[*num_my_stones + k];
	(*num_stones)++;
      }
    }
  }
  (*num_my_stones) += new_stones;

  /* Pick up the liberties of the new string. */
  if (libs) {
    gg_assert(num_libs);
    /* Get the liberties of the string. */
    num_my_libs = findlib(str, MAXLIBS, my_libs);

    /* Remove this string from the superstring if it has too many
     * liberties.
     */
    if (liberty_cap > 0 && num_my_libs > liberty_cap)
      (*num_my_stones) -= new_stones;

    for (k = 0; k < num_my_libs; k++) {
      if (ml[my_libs[k]])
	continue;
      ml[my_libs[k]] = 1;
      if (do_add && (liberty_cap == 0 || num_my_libs <= liberty_cap)) {
	libs[*num_libs] = my_libs[k];
	(*num_libs)++;
	if (*num_libs == maxlibs)
	  break;
      }
    }
  }

  /* Pick up adjacent strings to the new string. */
  if (adjs) {
    gg_assert(num_adj);
    num_my_adj = chainlinks(str, my_adjs);
    for (k = 0; k < num_my_adj; k++) {
      if (liberty_cap > 0 && countlib(my_adjs[k]) > liberty_cap)
	continue;
      if (ma[my_adjs[k]])
	continue;
      ma[my_adjs[k]] = 1;
      if (do_add) {
	adjs[*num_adj] = my_adjs[k];
	(*num_adj)++;
      }
    }
  }
}

/* Internal timers for assessing time spent on various tasks. */
#define NUMBER_OF_TIMERS 4
double timers[NUMBER_OF_TIMERS];

/* Start a timer. */
void
start_timer(int n)
{
  gg_assert(n >= 0 && n < NUMBER_OF_TIMERS);
  if (!showtime)
    return;

  timers[n] = gg_gettimeofday();
}

/* Report time spent and restart the timer. */
double
time_report(int n, const char *occupation, int i, int j)
{
  double t;
  double dt;
  gg_assert(n >= 0 && n < NUMBER_OF_TIMERS);
  if (!showtime)
    return 0.0;

  t = gg_gettimeofday();
  dt = t - timers[n];
  if (dt > 1.0) {
    gprintf("%s", occupation);
    if (!is_pass(POS(i, j)))
      gprintf("%m", i, j);
    fprintf(stderr, ": %.2f sec\n", dt);
  }
  timers[n] = t;
  return dt;
}



/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
