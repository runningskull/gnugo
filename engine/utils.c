/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * This is GNU GO, a Go program. Contact gnugo@gnu.org, or see       *
 * http://www.gnu.org/software/gnugo/ for more information.          *
 *                                                                   *
 * Copyright 1999, 2000, 2001, 2002 by the Free Software Foundation. *
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
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include <math.h>

#include "liberty.h"
#include "sgftree.h"
#include "random.h"
#include "gg_utils.h"


/*
 * Change the status of all the stones in the dragon at (dr).
 */

void
change_matcher_status(int dr, int status)
{
  int pos;
  int origin = dragon[dr].origin;

  for (pos = BOARDMIN; pos < BOARDMAX; pos++)
    if (ON_BOARD(pos)) {
      if (dragon[pos].origin == origin)
	dragon[pos].matcher_status = status;
    }
}


/*
 * Check whether a move at (move) stops the enemy from playing at (apos).
 */

int
defend_against(int move, int color, int apos)
{
  if (trymove(move, color, "defend_against", NO_MOVE, EMPTY, NO_MOVE)) {
    if (safe_move(apos, OTHER_COLOR(color)) == 0) {
      popgo();
      return 1;
    }
    popgo();
  }
  return 0;
}


/* 
 * Returns true if color can cut at (pos), or if connection through (pos)
 * is inhibited. This information is collected by find_cuts(), using the B
 * patterns in the connections database.
 */

int
cut_possible(int pos, int color)
{
  if (color == WHITE)
    return (black_eye[pos].cut
	    || (black_eye[pos].type & INHIBIT_CONNECTION));
  else
    return (white_eye[pos].cut
	    || (white_eye[pos].type & INHIBIT_CONNECTION));
}


/*
 * does_attack(move, str) returns true if the move at (move)
 * attacks (str). This means that it captures the string, and that
 * (str) is not already dead.
 */

int
does_attack(int move, int str)
{
  int color = board[str];
  int other = OTHER_COLOR(color);
  int result = 0;
  int acode = 0;
  int dcode = 0;
  int spos = NO_MOVE;
  
  if (stackp == 0) {
    if (worm[str].attack_codes[0] != 0 
	&& worm[str].defend_codes[0] == 0)
      return 0;
    spos = worm[str].defense_points[0];
  }
  else {
    attack_and_defend(str, &acode, NULL, &dcode, &spos);
    if (acode != 0 && dcode == 0)
      return 0;
  }
  
  if (trymove(move, other, "does_attack-A", str, EMPTY, NO_MOVE)) {
    if (!board[str] || !find_defense(str, NULL)) {
      result = WIN;
      increase_depth_values();
      if (spos != NO_MOVE && trymove(spos, color, "does_attack-B", str,
				     EMPTY, NO_MOVE)) {
	if (board[str] && !attack(str, NULL))
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
 * does_defend(move, str) returns true if the move at (move)
 * defends (str). This means that it defends the string, and that
 * (str) can be captured if no defense is made.
 */

int
does_defend(int move, int str)
{
  int color = board[str];
  int other = OTHER_COLOR(color);
  int result = 0;
  int spos = NO_MOVE;

  if (stackp == 0) {
    if (worm[str].attack_codes[0] == 0)
      return 0;
    else
      spos = worm[str].attack_points[0];
  }
  else if (!attack(str, &spos))
    return 0;

  gg_assert(spos != NO_MOVE);
  
  if (trymove(move, color, "does_defend-A", str, EMPTY, NO_MOVE)) {
    if (!attack(str, NULL)) {
      result = 1;
      increase_depth_values();
      if (trymove(spos, other, "does_defend-B", str, EMPTY, NO_MOVE)) {
	if (!board[str] || !find_defense(str, NULL))
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
 * Example: somewhere(WHITE, 2, apos, bpos, cpos).
 * 
 * Returns true if one of the vertices listed
 * satisfies board[pos]==color. Here last_move is the
 * number of moves minus one.
 */

int
somewhere(int color, int last_move, ...)
{
  va_list ap;
  int pos;
  int k;
  
  va_start(ap, last_move);
  for (k = 0; k <= last_move; k++) {
    pos = va_arg(ap, int);

    if (board[pos] == color
	&& (stackp > 0 || dragon[pos].matcher_status != DEAD))
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
  int apos;
  int xpos;
  int ypos;
  int zpos;
  
  va_start(ap, num_moves);

  /* Do all the moves with alternating colors. */
  for (i = 0; i < num_moves; i++) {
    apos = va_arg(ap, int);

    if (apos != NO_MOVE
	&& (trymove(apos, mcolor, "play_break_through_n", NO_MOVE,
		    EMPTY, NO_MOVE)
	    || tryko(apos, mcolor, "play_break_through_n", EMPTY, NO_MOVE)))
      played_moves++;
    mcolor = OTHER_COLOR(mcolor);
  }

  /* Now do the real work. */
  xpos = va_arg(ap, int);
  ypos = va_arg(ap, int);
  zpos = va_arg(ap, int);
    
  /* Temporarily increase the depth values with the number of explicitly
   * placed stones.
   */
#if 0
  modify_depth_values(played_moves);
#endif
  
  if (board[xpos] == EMPTY
      || board[ypos] == EMPTY
      || board[zpos] == EMPTY)
    success = 1;
  else
    success = break_through(xpos, ypos, zpos);

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
  int apos;
  int zpos;
  
  va_start(ap, num_moves);

  /* Do all the moves with alternating colors. */
  for (i = 0; i < num_moves; i++) {
    apos = va_arg(ap, int);

    if (apos != NO_MOVE
	&& (trymove(apos, mcolor, "play_attack_defend_n", NO_MOVE,
		    EMPTY, NO_MOVE)
	    || tryko(apos, mcolor, "play_attack_defend_n", EMPTY, NO_MOVE)))
      played_moves++;
    mcolor = OTHER_COLOR(mcolor);
  }

  /* Now do the real work. */
  zpos = va_arg(ap, int);

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
    if (board[zpos] == EMPTY)
      success = WIN;
    else
      success = attack(zpos, NULL);
  }
  else {
    if (board[zpos] == EMPTY)
      success = 0;
    else {
      int dcode = find_defense(zpos, NULL);
      if (dcode == 0 && !attack(zpos, NULL))
	success = WIN;
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
  int apos;
  int ypos;
  int zpos;
  
  va_start(ap, num_moves);

  /* Do all the moves with alternating colors. */
  for (i = 0; i < num_moves; i++) {
    apos = va_arg(ap, int);

    if (apos != NO_MOVE
	&& (trymove(apos, mcolor, "play_attack_defend_n", NO_MOVE,
		    EMPTY, NO_MOVE)
	    || tryko(apos, mcolor, "play_attack_defend_n", EMPTY, NO_MOVE)))
      played_moves++;
    mcolor = OTHER_COLOR(mcolor);
  }

  /* Now do the real work. */
  ypos = va_arg(ap, int);
  zpos = va_arg(ap, int);

  /* Temporarily increase the depth values with the number of explicitly
   * placed stones.
   */
#if 0
  modify_depth_values(played_moves);
#endif
  

  /* FIXED: tm - returns ko results correctly (3.1.22) */
  if (do_attack) {
    if (board[ypos] == EMPTY || board[zpos] == EMPTY)
      success = WIN;
    else
      success = attack_either(ypos, zpos);
  }
  else {
    if (board[ypos] == EMPTY || board[zpos] == EMPTY)
      success = 0;
    else
      success = defend_both(ypos, zpos);
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


/* FIXME: documentation needs expanding - identical in concept 
 * to play_attack_defend2_n
 */

int 
play_connect_n(int color, int do_connect, int num_moves, ...)
{
  va_list ap;
  int mcolor = color;
  int success = 0;
  int i;
  int played_moves = 0;
  int apos;
  int ypos;
  int zpos;

  /* FIXME: very pessimistic approach if connections module not
   * included. */
  if (!experimental_connections) {
    return do_connect;
  }

  va_start(ap, num_moves);

  /* Do all the moves with alternating colors. */
  for (i = 0; i < num_moves; i++) {
    apos = va_arg(ap, int);

    if (apos != NO_MOVE
	&& (trymove(apos, mcolor, "play_attack_defend_n", NO_MOVE,
		    EMPTY, NO_MOVE)
	    || tryko(apos, mcolor, "play_attack_defend_n", EMPTY, NO_MOVE)))
      played_moves++;
    mcolor = OTHER_COLOR(mcolor);
  }

  /* Now do the real work. */
  ypos = va_arg(ap, int);
  zpos = va_arg(ap, int);

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
  
  if (do_connect) {
    if (board[ypos] == EMPTY || board[zpos] == EMPTY)
      success = 0;
    else
      success = string_connect(ypos, zpos, NULL);
  }
  else {
    if (board[ypos] == EMPTY || board[zpos] == EMPTY)
      success = WIN;
    else
      success = disconnect(ypos, zpos, NULL);
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

#if 0
#undef FOURLIB_DEPTH
#define FOURLIB_DEPTH         9
#endif


#define AA_DEPTH              6
#define AA_THREAT_DEPTH       3

/* Pattern based reading */
#define OWL_DISTRUST_DEPTH    6
#define OWL_BRANCH_DEPTH      8
#define OWL_READING_DEPTH    20

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
    aa_threat_depth     = gg_max(0, AA_THREAT_DEPTH - 10 + level);
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
    aa_threat_depth     = gg_max(0, AA_THREAT_DEPTH - 10 + level);
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
    aa_threat_depth     = gg_max(0, AA_THREAT_DEPTH - 2);
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
    aa_threat_depth     = gg_max(0, AA_THREAT_DEPTH - 9 + level);
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
  if (mandated_aa_threat_depth != -1)
    aa_threat_depth = mandated_aa_threat_depth;
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

/* Play a stone at (pos) and count the number of liberties for the
 * resulting string. This requires (pos) to be empty.
 *
 * This function differs from approxlib() by the fact that it removes
 * captured stones before counting the liberties.
 */

int
accurate_approxlib(int pos, int color, int maxlib, int *libs)
{
  int fast_liberties = -1;
  int liberties = 0;
  SGFTree *save_sgf_dumptree = sgf_dumptree;
  int save_count_variations = count_variations;

  ASSERT1(board[pos] == EMPTY, pos);
  ASSERT1(IS_STONE(color), pos);

  if (!libs) {
    fast_liberties = fastlib(pos, color, 0);
    if (fast_liberties >= 0) {
      return fast_liberties;
    } 
  }

  sgf_dumptree = 0;
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

  if (fast_liberties >= 0 && liberties > 0) {
    ASSERT1(fast_liberties == liberties, pos);
  }

  sgf_dumptree = save_sgf_dumptree;
  count_variations = save_count_variations;

  return liberties;
}


/* This function will detect some blunders. If the move reduces the
 * number of liberties of an adjacent friendly string, there is a
 * danger that the move could backfire, so the function checks that no
 * friendly worm which was formerly not attackable becomes attackable,
 * and it checks that no opposing worm which was not defendable
 * becomes defendable. Only worms with worm.size>size are checked.
 *
 * The arrays saved_dragons[] and saved_worms[] should be one for
 * stones belonging to dragons or worms respectively, which are
 * supposedly saved by (move). These may be NULL if no stones are
 * supposed to gaving been saved.
 *
 * For use when called from fill_liberty, this function may optionally
 * return a point of defense, which, if taken, will presumably make
 * the move at (move) safe on a subsequent turn.
 *
 * FIXME: Most TRACE calls below are ineffective because we have
 * decreased the verbose value to avoid traces in the owl code.
 */

int
confirm_safety(int move, int color, int size, int *defense_point,
	       int saved_dragons[BOARDMAX], int saved_worms[BOARDMAX])
{
  int libs[5];
  int liberties = accurate_approxlib(move, color, 5, libs);
  int other = OTHER_COLOR(color);
  int issafe = 1;
  int pos;
  int apos;
  int trouble = 0;
  int k;
  int save_verbose = verbose;

  if (defense_point)
    *defense_point = NO_MOVE;

  TRACE("Checking safety of a %s move at %1m\n", color_to_string(color), move);

  if (verbose > 0)
    verbose--;
  
  if (!atari_atari_confirm_safety(color, move, &apos, size,
				  saved_dragons, saved_worms)) {
    ASSERT_ON_BOARD1(apos);
    if (defense_point)
      *defense_point = apos;
    verbose = save_verbose;
    TRACE("Combination attack appears at %1m.\n", apos);
    return 0;
  }

  if (liberties > 4) {
    verbose = save_verbose;
    return 1;
  }

  for (k = 0; k < 4; k++) {
    int bpos = move + delta[k];
    if (board[bpos] == color
	&& liberties <= worm[bpos].liberties) {
      trouble = 1;
      if ((dragon[bpos].matcher_status == ALIVE
	   || (dragon[bpos].matcher_status == CRITICAL
	       && saved_dragons != NULL
	       && saved_dragons[bpos]))
	  && DRAGON2(bpos).safety != INVINCIBLE
	  && DRAGON2(bpos).safety != STRONGLY_ALIVE
	  && dragon[bpos].size >= size
	  && !owl_confirm_safety(move, bpos, defense_point)) {
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
  
  if (trymove(move, color, NULL, NO_MOVE, EMPTY, NO_MOVE)) {
    for (pos = BOARDMIN; issafe && pos < BOARDMAX; pos++)
      if (issafe
	  && IS_STONE(board[pos])
	  && worm[pos].origin == pos
	  && pos != move) {
	if (board[pos] == color
	    && worm[pos].attack_codes[0] == 0
	    && worm[pos].size >= size
	    && attack(pos, NULL)) {
	  if (defense_point)
	    find_defense(pos, defense_point);
	  issafe = 0;
	  TRACE("After %1m Worm at %1m becomes attackable.\n", move, pos);
	}
	else if (board[pos] == other
		 && worm[pos].attack_codes[0] != 0
		 && worm[pos].defend_codes[0] == 0
		 && worm[pos].size >= size
		 && find_defense(pos, NULL)) {
	  /* Also ask the owl code whether the string can live
	   * strategically. To do this we need to temporarily undo
	   * the trymove().
	   */
	  popgo();
	  decrease_depth_values();
	  if (owl_does_attack(move, pos) != WIN)
	    issafe = 0;
	  trymove(move, color, NULL, NO_MOVE, EMPTY, NO_MOVE);
	  increase_depth_values();
	  
	  if (!issafe) {
	    if (defense_point) {
	      int dpos;
	      if (attack(pos, &dpos))
		*defense_point = dpos;
	      else
		TRACE("No attack found (unexpectedly) on %1m after move at %1m.\n",
		      pos, move);
	    }
	    
	    TRACE("After %1m worm at %1m becomes defendable.\n",
		  move, pos);
	  }
	}
      }
    
    if (liberties == 2) {
      if (double_atari(libs[0], other)) {
	if (defense_point && safe_move(libs[0], color) == WIN)
	  *defense_point = libs[0];
	issafe = 0;
	TRACE("Double threat appears at %1m.\n", libs[0]);
      }
      else if (double_atari(libs[1], other)) {
	if (defense_point && safe_move(libs[1], color) == WIN)
	  *defense_point = libs[1];
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
double_atari(int move, int color)
{
  int other = OTHER_COLOR(color);
  int k;
  int m = I(move);
  int n = J(move);

  if (!ON_BOARD(move))
    return 0;

  /* Loop over the diagonal directions. */
  for (k = 4; k < 8; k++) {
    int dm = deltai[k];
    int dn = deltaj[k];
    
    /* because (m, n) and (m+dm, n+dn) are opposite
     * corners of a square, ON_BOARD2(m, n) && ON_BOARD2(m+dm, n+dn)
     * implies ON_BOARD2(m+dm, n) and ON_BOARD2(n, n+dn)
     */
    if (BOARD(m+dm, n+dn) == color
	&& BOARD(m, n+dn) == other
	&& BOARD(m+dm, n) == other
	&& trymove(move, color, "double_atari", NO_MOVE, EMPTY, NO_MOVE)) {
      if (countlib(move) > 1
	  && (BOARD(m, n+dn) == EMPTY || BOARD(m+dm, n) == EMPTY 
	      || !defend_both(POS(m, n+dn), POS(m+dm, n)))) {
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
 * the worm arrays and the number of non-capturable worms is
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
unconditional_life(int unconditional_territory[BOARDMAX], int color)
{
  int something_captured = 1; /* To get into the first turn of the loop. */
  int found_one;
  int moves_played = 0;
  int save_moves;
  int pos;
  int k;
  int libs[MAXLIBS];
  int liberties;
  int other = OTHER_COLOR(color);
  
  while (something_captured) {
    /* Nothing captured so far in this turn of the loop. */
    something_captured = 0;

    /* Visit all friendly strings on the board. */
    for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
      if (board[pos] != color || !is_worm_origin(pos, pos))
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
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (board[pos] != color || !is_worm_origin(pos, pos))
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

    for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
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

  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
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
  memset(unconditional_territory, 0, sizeof(int) * BOARDMAX);
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (board[pos] == color) {
      unconditional_territory[pos] = 1;
      if (is_worm_origin(pos, pos)) {
	liberties = findlib(pos, MAXLIBS, libs);
	for (k = 0; k < liberties; k++)
	  unconditional_territory[libs[k]] = 2;
      }
    }
    else if (board[pos] == other && countlib(pos) == 1) {
      unconditional_territory[pos] = 2;
      findlib(pos, 1, libs);
      unconditional_territory[libs[0]] = 2;
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
 * superstring at (str), including those which are boundary chains of
 * (str) itself. If liberty_cap != 0, only those boundary chains with
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
 * superstring at (str), omitting those which are boundary chains of
 * (str) itself. If liberty_cap != 0, only those boundary chains with
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
      /* Notice that the order of these tests is significant. We must
       * check bpos before fpos and epos to avoid accessing memory
       * outside the board array. (Notice that fpos is two steps away
       * from pos, which we know is on the board.)
       */
      if (board[apos] == color && board[bpos] == EMPTY
	  && board[fpos] == color && board[epos] == color && !mx[epos]
	  && board[gpos] == EMPTY)
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
	  
	  if (attack(upos, NULL)
	      && !find_defense(upos, NULL)) {
	    int lunch_stones[MAX_BOARD*MAX_BOARD];
	    int num_lunch_stones = findstones(upos, MAX_BOARD*MAX_BOARD,
					      lunch_stones);
	    int m, n;
	    for (m = 0; m < num_lunch_stones; m++)
	      for (n = 0; n < 8; n++) {
		int vpos = lunch_stones[m] + delta[n];
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

  timers[n] = gg_cputime();
}

/* Report time spent and restart the timer. Make no report if elapsed
 * time is less than mintime.
 */
double
time_report(int n, const char *occupation, int move, double mintime)
{
  double t;
  double dt;
  gg_assert(n >= 0 && n < NUMBER_OF_TIMERS);

  if (!showtime)
    return 0.0;

  t = gg_cputime();
  dt = t - timers[n];
  if (dt > mintime) {
    gprintf("%s", occupation);
    if (move != NO_MOVE)
      gprintf("%1m", move);
    fprintf(stderr, ": %.2f sec\n", dt);
  }
  timers[n] = t;
  return dt;
}

/* Update the random seed with the current value in the random sequence. */
void
update_random_seed(void)
{
  random_seed = gg_rand();
  /* Since random seed 0 has a special interpretation when given as
   * command line argument with the -r option, we make sure to avoid
   * it.
   */
  if (random_seed == 0)
    random_seed = 1;
}


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
