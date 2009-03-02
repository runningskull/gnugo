/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * This is GNU Go, a Go program. Contact gnugo@gnu.org, or see       *
 * http://www.gnu.org/software/gnugo/ for more information.          *
 *                                                                   *
 * Copyright 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007,   *
 * 2008 and 2009 by the Free Software Foundation.                    *
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
#include "patterns.h"

/*
 * Change the status of all the stones in the dragon at (dr).
 */

void
change_dragon_status(int dr, enum dragon_status status)
{
  int pos;
  int origin = dragon[dr].origin;

  for (pos = BOARDMIN; pos < BOARDMAX; pos++)
    if (ON_BOARD(pos)) {
      if (dragon[pos].origin == origin)
	dragon[pos].status = status;
    }
}


/*
 * Check whether a move at (move) stops the enemy from playing at (apos).
 */

int
defend_against(int move, int color, int apos)
{
  if (trymove(move, color, "defend_against", NO_MOVE)) {
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
  return (cutting_points[pos] & OTHER_COLOR(color)) != 0;
}


/*
 * does_attack(move, str) returns the result code for an attack on the
 * string 'str' by the move 'move'. However, if the move does not
 * improve the attack result compared to tenuki, 0 is returned. In
 * particular if the string is already captured, does_attack() always
 * returns 0.
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
  
  attack_and_defend(str, &acode, NULL, &dcode, &spos);
  if (acode != 0 && dcode == 0)
    return 0;
  
  if (trymove(move, other, "does_attack-A", str)) {
    if (!board[str])
      result = WIN;
    else
      result = REVERSE_RESULT(find_defense(str, NULL));
    if (result != 0) {
      increase_depth_values();
      if (spos != NO_MOVE && trymove(spos, color, "does_attack-B", str)) {
	if (board[str]) {
	  int new_result = attack(str, NULL);
	  if (new_result < result)
	    result = new_result;
	}
	popgo();
      }
      decrease_depth_values();
    }
    popgo();
  }

  if (result < acode)
    result = 0;
  
  return result;
}


/*
 * does_defend(move, str) returns true if the move at (move)
 * defends (str). This means that it defends the string, and that
 * (str) can be captured if no defense is made.
 *
 * FIXME: Make does_defend() ko aware like does_attack().
 */

int
does_defend(int move, int str)
{
  int color = board[str];
  int other = OTHER_COLOR(color);
  int result = 0;
  int spos = NO_MOVE;

  if (!attack(str, &spos))
    return 0;

  gg_assert(spos != NO_MOVE);
  
  if (trymove(move, color, "does_defend-A", str)) {
    if (!attack(str, NULL)) {
      result = 1;
      increase_depth_values();
      if (trymove(spos, other, "does_defend-B", str)) {
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
 * satisfies board[pos]==color. Here num_moves is the
 * number of moves. If check_alive is true, the dragon is not allowed
 * to be dead. This check is only valid if stackp==0.
 */

int
somewhere(int color, int check_alive, int num_moves, ...)
{
  va_list ap;
  int pos;
  int k;

  gg_assert(stackp == 0 || !check_alive);
  
  va_start(ap, num_moves);
  for (k = 0; k < num_moves; k++) {
    pos = va_arg(ap, int);

    if (board[pos] == color
	&& (!check_alive || dragon[pos].status != DEAD)) {
      va_end(ap);
      return 1;
    }
  }

  va_end(ap);
  return 0;
}

/* Search along the edge for the first visible stone. Start at apos
 * and move in the direction of bpos. Return 1 if the first visible
 * stone is of the given color. It is required that apos and bpos are
 * at the same distance from the edge.
 *
 * FIXME: The detection of the first visible stone is quite crude and
 * probably needs to be improved.
 */
int
visible_along_edge(int color, int apos, int bpos)
{
  int ai = I(apos);
  int aj = J(apos);
  int bi = I(bpos);
  int bj = J(bpos);
  int pos;
  int forward;
  int up;
  ASSERT1((ai == bi) ^ (aj == bj), apos);

  if (ai == bi) {
    if (aj > bj)
      forward = WEST(0);
    else
      forward = EAST(0);

    if (ai < board_size/2) {
      pos = POS(0, bj);
      up = SOUTH(0);
    }
    else {
      pos = POS(board_size - 1, bj);
      up = NORTH(0);
    }
  }
  else {
    if (ai > bi)
      forward = NORTH(0);
    else
      forward = SOUTH(0);

    if (aj < board_size/2) {
      pos = POS(bi, 0);
      up = EAST(0);
    }
    else {
      pos = POS(bi, board_size - 1);
      up = WEST(0);
    }
  }
  
  for (; ON_BOARD(pos); pos += forward) {
    int k;
    for (k = 4; k >= 0; k--) {
      ASSERT_ON_BOARD1(pos + k * up);
      if (board[pos + k * up] == color)
	return 1;
      else if (board[pos + k * up] == OTHER_COLOR(color))
	return 0;
    }
  }

  return 0;
}

/* Is the board symmetric (or rather antisymmetric) with respect to
 * mirroring in tengen after a specific move has been played? If the
 * move is PASS_MOVE, check the current board.
 *
 * If strict is set we require that each stone is matched by a stone
 * of the opposite color at the mirrored vertex. Otherwise we only
 * require that each stone is matched by a stone of either color.
 */
int
test_symmetry_after_move(int move, int color, int strict)
{
  int pos;
  int result = 1;

  if (move != PASS_MOVE) {
    if (board[move] != EMPTY)
      return 0;
    if (!trymove(move, color, "find_mirror_move", NO_MOVE))
      return 0;
  }
  
  for (pos = BOARDMIN; pos < MIRROR_MOVE(pos); pos++) {
    int sum;
    if (!ON_BOARD(pos))
      continue;
    
    sum = board[pos] + board[MIRROR_MOVE(pos)];
    if (sum != EMPTY + EMPTY && sum != BLACK + WHITE) {
      if (strict || sum == EMPTY + WHITE || sum == EMPTY + BLACK) {
	result = 0;
	break;
      }
    }
  }

  if (move != PASS_MOVE)
    popgo();
  
  return result;
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
	&& (trymove(apos, mcolor, "play_break_through_n", NO_MOVE)
	    || tryko(apos, mcolor, "play_break_through_n")))
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
	&& (trymove(apos, mcolor, "play_attack_defend_n", NO_MOVE)
	    || tryko(apos, mcolor, "play_attack_defend_n")))
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
	&& (trymove(apos, mcolor, "play_attack_defend_n", NO_MOVE)
	    || tryko(apos, mcolor, "play_attack_defend_n")))
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


/* The function play_connect_n() plays a sequence of moves,
 * alternating between the players and starting with color. After
 * having played through the sequence, the two last coordinates
 * give two targets that should be connected or disconnected, depending on
 * the value of do_connect. If there is no stone present to connect or
 * disconnect, it is assumed that the connection has failed. If one or
 * more of the moves to play turns out to be illegal for some reason,
 * the rest of the sequence is played anyway, and connection/disconnection
 * is tested as if nothing special happened.
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

  va_start(ap, num_moves);

  /* Do all the moves with alternating colors. */
  for (i = 0; i < num_moves; i++) {
    apos = va_arg(ap, int);

    if (apos != NO_MOVE
	&& (trymove(apos, mcolor, "play_connect_n", NO_MOVE)
	    || tryko(apos, mcolor, "play_connect_n")))
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
  
  /* See if ypos and zpos can be connected (or disconnected). */
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


/* The function play_lib_n() plays a sequence of moves, alternating
 * between the players and starting with color. After having played
 * through the sequence, the last coordinate gives a target for liberty
 * counting. The number of liberties is returned.
 *
 * If only one move is to be played and that stone is the target,
 * accuratelib (or approxlib if appropriate) is more efficient.
 */

int 
play_lib_n(int color, int num_moves, ...)
{
  va_list ap;
  int mcolor = color;
  int libs = 0;
  int i;
  int played_moves = 0;
  int apos;
  int ypos;

  va_start(ap, num_moves);

  /* Do all the moves with alternating colors. */
  for (i = 0; i < num_moves; i++) {
    apos = va_arg(ap, int);

    if (apos != NO_MOVE
	&& (trymove(apos, mcolor, "play_connect_n", NO_MOVE)
	    || tryko(apos, mcolor, "play_connect_n")))
      played_moves++;
    mcolor = OTHER_COLOR(mcolor);
  }

  /* Now do the real work. */
  ypos = va_arg(ap, int);
  if (board[ypos] == EMPTY)
    libs = 0;
  else
    libs = countlib(ypos);
  
  /* Pop all the moves we could successfully play. */
  for (i = 0; i < played_moves; i++)
    popgo();

  va_end(ap);
  return libs;
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
#define BREAK_CHAIN_DEPTH     7
#define SUPERSTRING_DEPTH     7
#define FOURLIB_DEPTH         7
#define KO_DEPTH              8

#if 0
#undef FOURLIB_DEPTH
#define FOURLIB_DEPTH         9
#endif


#define AA_DEPTH              6

/* Pattern based reading */
#define OWL_DISTRUST_DEPTH    6
#define OWL_BRANCH_DEPTH      8
#define OWL_READING_DEPTH    20
#define SEMEAI_BRANCH_DEPTH  12
#define SEMEAI_BRANCH_DEPTH2  6

/* Connecton reading */
#define CONNECT_NODE_LIMIT 2000
#define CONNECT_DEPTH        64
#define CONNECT_DEPTH2       20

#define BREAKIN_NODE_LIMIT  400
#define BREAKIN_DEPTH	     14

/* Set the various reading depth parameters. If mandated_depth_value
 * is not -1 that value is used; otherwise the depth values are
 * set as a function of level. The parameter mandated_depth_value
 * can be set at the command line to force a particular value of
 * depth; normally it is -1.
 */

void
set_depth_values(int level, int report_levels)
{
  static int node_limits[] = {500, 500, 450, 400, 400, 325, 275,
			      200, 150, 100, 75, 50};
  int depth_level;

  /*
   * Other policies depending on level:
   * owl.c:         >=  9: use vital attack pattern database
   *                >=  8: increase depth values in owl_substantial
   *                >=  8: don't turn off owl_phase in semeai reading
   * reading.c:     >=  8: Use superstrings and do more backfilling.
   * value_moves.c: >=  6: try to find more owl attacks/defenses
   * breakin.c:     >= 10: try to find break-ins. (*)
   * worm.c:        >= 10: detect unconditionally meaningless moves
   *
   * The break-in code (*) is particularly expensive. 
   *
   * Speedups between levels 9 and 10 and between levels 7 and 8
   * are obtained by turning off services, and between these
   * levels no changes are made in the depths. The parameter
   * depth_level is the correction compared to the default settings at level
   * 10 for most reading depths.
   */
  if (level >= 10)
    depth_level = level - 10;
  else if (level == 9)
    depth_level = 0;
  else if (level == 8)
    depth_level = -1;
  else 
    depth_level = level - 8;

  depth                = gg_max(6, DEPTH 	    + depth_level);
  branch_depth         = gg_max(3, BRANCH_DEPTH	    + depth_level);
  backfill_depth       = gg_max(2, BACKFILL_DEPTH    + depth_level);
  backfill2_depth      = gg_max(1, BACKFILL2_DEPTH   + depth_level);
  break_chain_depth    = gg_max(2, BREAK_CHAIN_DEPTH + depth_level);
  if (level >= 8)
    owl_distrust_depth = gg_max(1, (2 * OWL_DISTRUST_DEPTH + depth_level) / 2);
  else
    owl_distrust_depth = gg_max(1, (2 * OWL_DISTRUST_DEPTH - 1
				    + depth_level) / 2);
  owl_branch_depth     = gg_max(2, (2 * OWL_BRANCH_DEPTH   + depth_level) / 2);
  owl_reading_depth    = gg_max(5, (2 * OWL_READING_DEPTH  + depth_level) / 2);

  /* Atari-atari depth levels are unchanged only between levels 7/8, 9/10: */
  if (level >= 10)
    aa_depth = gg_max(0, AA_DEPTH + (level - 10));
  else if (level == 9)
    aa_depth = gg_max(0, AA_DEPTH);
  else if (level >= 7)
    aa_depth = gg_max(0, AA_DEPTH - 1);
  else
    aa_depth = gg_max(0, AA_DEPTH - (8 - level));

  /* Exceptions:
   * fourlib_depth: This is constant from levels 7 to 10.
   * superstring_depth: set to 0 below level 8.
   */
  if (level >= 10)
    ko_depth            = gg_max(1, KO_DEPTH + (level - 10));
  else if (level == 9)
    ko_depth            = gg_max(1, KO_DEPTH);
  else if (level >= 7)
    ko_depth            = gg_max(1, KO_DEPTH - 1);
  else
    ko_depth            = gg_max(1, KO_DEPTH + (level - 8));

  if (level >= 10)
    fourlib_depth       = gg_max(1, FOURLIB_DEPTH + (level - 10));
  else if (level >= 7)
    fourlib_depth       = gg_max(1, FOURLIB_DEPTH);
  else
    fourlib_depth       = gg_max(1, FOURLIB_DEPTH + (level - 7));

  if (level >= 8)
    superstring_depth = gg_max(1, SUPERSTRING_DEPTH);
  else
    superstring_depth = 0;

  if (level >= 10)
    owl_node_limit      = OWL_NODE_LIMIT * pow(1.5, depth_level);
  else {
    owl_node_limit      = (OWL_NODE_LIMIT * node_limits[10 - level] /
			   node_limits[0]);
    owl_node_limit      = gg_max(20, owl_node_limit);
  }

  semeai_branch_depth  = gg_max(2, (2*SEMEAI_BRANCH_DEPTH  + depth_level) / 2);
  semeai_branch_depth2 = gg_max(2, (2*SEMEAI_BRANCH_DEPTH2 + depth_level) / 2);
  semeai_node_limit    = SEMEAI_NODE_LIMIT * pow(1.5, depth_level);

  connect_depth         = gg_max(2, CONNECT_DEPTH  + 2 * depth_level);
  connect_depth2        = gg_max(2, CONNECT_DEPTH2 + 2 * depth_level);
  connection_node_limit = CONNECT_NODE_LIMIT * pow(1.45, depth_level);
  breakin_depth 	= gg_max(2, BREAKIN_DEPTH + 2 * depth_level);
  breakin_node_limit 	= BREAKIN_NODE_LIMIT * pow(1.5, depth_level);

  if (mandated_depth != -1)
    depth = mandated_depth;
  if (mandated_backfill_depth != -1)
    backfill_depth = mandated_backfill_depth;
  if (mandated_backfill2_depth != -1)
    backfill2_depth = mandated_backfill2_depth;
  if (mandated_break_chain_depth != -1)
    break_chain_depth = mandated_break_chain_depth;
  if (mandated_superstring_depth != -1)
    superstring_depth = mandated_superstring_depth;
  if (mandated_branch_depth != -1)
    branch_depth = mandated_branch_depth;
  if (mandated_fourlib_depth != -1)
    fourlib_depth = mandated_fourlib_depth;
  if (mandated_ko_depth != -1)
    ko_depth = mandated_ko_depth;
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
  if (mandated_semeai_node_limit != -1)
    semeai_node_limit = mandated_semeai_node_limit;

  depth_offset = 0;
  
  if (report_levels) {
    fprintf(stderr, "at level %d:\n\n\
depth: %d\n\
branch_depth: %d\n\
backfill_depth: %d\n\
backfill2_depth: %d\n\
break_chain_depth: %d\n\
owl_distrust_depth: %d\n\
owl_branch_depth: %d\n\
owl_reading_depth: %d\n\
aa_depth: %d\n\
ko_depth: %d\n\
fourlib_depth: %d\n\
superstring_depth: %d\n\
owl_node_limit: %d\n\
semeai_branch_depth: %d\n\
semeai_branch_depth2: %d\n\
semeai_node_limit: %d\n\
connect_depth: %d\n\
connect_depth2: %d\n\
connection_node_limit: %d\n\
breakin_depth: %d\n\
breakin_node_limit: %d\n\n",
	    level, depth, branch_depth, backfill_depth, backfill2_depth,
	    break_chain_depth, owl_distrust_depth, owl_branch_depth,
	    owl_reading_depth, aa_depth, ko_depth, fourlib_depth,
	    superstring_depth, owl_node_limit, semeai_branch_depth, 
	    semeai_branch_depth2, semeai_node_limit, connect_depth, 
            connect_depth2, connection_node_limit, breakin_depth, 
	    breakin_node_limit);
  }
}


static int depth_modification = 0;

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
  break_chain_depth  += n;
  superstring_depth  += n;
  branch_depth       += n;
  fourlib_depth      += n;
  ko_depth           += n;
  breakin_depth	     += n;
  depth_offset       += n;
  depth_modification += n;
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

int
get_depth_modification(void)
{
  return depth_modification;
}


/*******************
 * Detect blunders *
 *******************/

static int detect_owl_blunder(int move, int color, int *defense_point,
			      signed char safe_stones[BOARDMAX], int liberties,
			      float *return_value, int save_verbose);

static void detect_tactical_blunder(int move, int color, int *defense_point,
				    signed char safe_stones[BOARDMAX],
				    int liberties, int *libs,
				    float *return_value, int save_verbose);

/* Check that the move at color doesn't involve any kind of blunder,
 * regardless of size.
 */
int
confirm_safety(int move, int color, int *defense_point,
	       signed char safe_stones[BOARDMAX])
{
  return (blunder_size(move, color, defense_point, safe_stones) == 0.0);
}

/* This function will detect some blunders. If the move reduces the
 * number of liberties of an adjacent friendly string, there is a
 * danger that the move could backfire, so the function checks that no
 * friendly worm which was formerly not attackable becomes attackable,
 * and it checks that no opposing worm which was not defendable
 * becomes defendable.
 *
 * It returns the estimated size of the blunder, or 0.0 if nothing
 * bad has happened.
 *
 * The array safe_stones[] contains the stones that are supposedly
 * safe after (move). It may be NULL.
 *
 * For use when called from fill_liberty, this function may optionally
 * return a point of defense, which, if taken, will presumably make
 * the move at (move) safe on a subsequent turn.
 */

float
blunder_size(int move, int color, int *defense_point,
	     signed char safe_stones[BOARDMAX])
{
  int libs[5];
  int liberties = accuratelib(move, color, 5, libs);
  int trouble = 0;
  int save_verbose = verbose;
  float return_value = 0.0;
  int atari;
  signed char defense_moves[BOARDMAX];
  
  if (defense_point)
    *defense_point = NO_MOVE;

  TRACE("Checking safety of a %s move at %1m\n", color_to_string(color), move);

  if (verbose > 0)
    verbose--;

  /* We start by checking whether we have accidentally killed an own
   * dragon.
   */
  trouble = detect_owl_blunder(move, color, defense_point,
			       safe_stones, liberties,
			       &return_value, save_verbose);
  

  /* Next we see whether the move has caused tactical complications.
   * The trouble variable is set if a string next to the move with few
   * liberties has not gained liberties by the move.
   */
  if (trouble)
    detect_tactical_blunder(move, color, defense_point, safe_stones,
			    liberties, libs, &return_value, save_verbose);

  /* FIXME: We would also need a detect_semeai_blunder() to check
   * against moves which make the outcome of a semeai worse, e.g. by
   * letting the opponent live in seki.
   */

  
  /* Finally we call the atari-atari code to see whether the move has
   * set up some combination attack that didn't exist before. We do
   * this last to avoid duplicate blunder reports.
   */
  atari = atari_atari_blunder_size(color, move, defense_moves, safe_stones);
  if (atari) {
    if (defense_point) {
      /* FIXME: Choose defense point more systematically. */
      int pos;
      *defense_point = NO_MOVE;
      for (pos = BOARDMIN; pos < BOARDMAX; pos++)
	if (ON_BOARD(pos) && defense_moves[pos]) {
	  *defense_point = pos;
	  break;
	}
    }
    verbose = save_verbose;
    TRACE("Combination attack appears.\n");
    return_value += (float) atari;
  }

  verbose = save_verbose;
  return return_value;
}

/* Check whether we have accidentally killed an own dragon adjacent to
 * move. If this happens, we mark its stones as no longer safe, and
 * remember the dragon's size.
 */

static int
detect_owl_blunder(int move, int color, int *defense_point,
		   signed char safe_stones[BOARDMAX], int liberties,
		   float *return_value, int save_verbose)
{
  int k;
  int ii;
  int trouble = 0;
  int dragon_analyzed[4] = {0, 0, 0, 0};
  int current_verbose = verbose;
  
  for (k = 0; k < 4; k++) {
    int bpos = move + delta[k];
    int j;
    /* We get worried if there is a liberty problem (and in this case
     * there might also be tactical trouble), or if we play inside
     * our eye space and the dragon is only just alive.
     */
    if (board[bpos] != color)
      continue;
    if (liberties <= worm[bpos].liberties
	&& liberties <= 4)
      trouble = 1;
    else
      if (min_eyes(&(DRAGON2(bpos).genus)) > 2
	  || !is_proper_eye_space(move))
	continue;

    /* Don't test the same dragon twice. */
    for (j = 0; j < k; j++)
      if (dragon_analyzed[j] == dragon[bpos].origin)
	break;
    if (j < k)
      continue;
    dragon_analyzed[k] = dragon[bpos].origin;

    /* Don't reanalyze if (move) is an owl defense for (bpos). */
    if (safe_stones && safe_stones[bpos] == OWL_SAVED_STONE)
      continue;

    if ((dragon[bpos].status == ALIVE
	 || (safe_stones 
	     && safe_stones[bpos]))
	&& DRAGON2(bpos).safety != INVINCIBLE
	&& DRAGON2(bpos).safety != STRONGLY_ALIVE) {
      int kworm = NO_MOVE;
      int acode = owl_confirm_safety(move, bpos, defense_point, &kworm);

      /* If owl couldn't confirm safety, maybe semeai can. */
      if (acode != WIN) {
	int r;
	for (r = 0; r < DRAGON2(bpos).neighbors; r++) {
	  int neighbor = dragon2[DRAGON2(bpos).adjacent[r]].origin;
	  int resultb;
	  if (board[neighbor] == color)
	    continue;
	  owl_analyze_semeai_after_move(move, color, neighbor, bpos,
					NULL, &resultb, NULL, 1, NULL, 0);
	  if (resultb == 0)
	    acode = WIN;
	}
      }
      
      if (acode == 0) {
	verbose = save_verbose;
	TRACE("Dragon at %1m becomes attackable.\n", bpos);
	verbose = current_verbose;
	*return_value += 2.0 * dragon[bpos].effective_size;
	if (safe_stones)
	  mark_dragon(bpos, safe_stones, 0);
      }
      else if (acode == LOSS) {
	verbose = save_verbose;
	TRACE("Dragon at %1m becomes attackable.\n", bpos);
	verbose = current_verbose;
	if (kworm == move) {
	  int l;
	  /* the worm origin was messed by our own move */
	  for (l = 0; l < 4; l++) {
	    int kworm = move + delta[l];
	    if (board[kworm] == color) {
	      *return_value += 2.0 * worm[kworm].effective_size;
	      if (safe_stones)
		for (ii = BOARDMIN; ii < BOARDMAX; ii++)
		  if (ON_BOARD(ii) && worm[ii].origin == worm[kworm].origin)
		    safe_stones[ii] = 0;
	    }
	  }
	}
	else {
	  *return_value += 2.0 * worm[kworm].effective_size;
	  if (safe_stones)
	    for (ii = BOARDMIN; ii < BOARDMAX; ii++)
	      if (ON_BOARD(ii) && worm[ii].origin == worm[kworm].origin)
		safe_stones[ii] = 0;
	}
      }
    }
  }

  return trouble;
}

/* Check whether a move causes any unexpected and unwelcome changes in
 * the tactical status of worms all over the board.
 */
static void
detect_tactical_blunder(int move, int color, int *defense_point,
			signed char safe_stones[BOARDMAX],
			int liberties, int *libs,
			float *return_value, int save_verbose)
{
  int other = OTHER_COLOR(color);
  int pos;
  int ii;
  int current_verbose = verbose;

  if (!trymove(move, color, NULL, NO_MOVE))
    return;
  
  /* Need to increase the depth values during this reading to avoid
   * horizon effects.
   */
  increase_depth_values();
  
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (!IS_STONE(board[pos])
	|| worm[pos].origin != pos
	|| pos == move)
      continue;
    
    /* First, we look for a new tactical attack.
     * FIXME: Verify that the tactically attacked stone matters. See
     *        e.g. the D6 move in filllib:51 which invites a harmless
     *        tactical attack of A4.
     */
    if (board[pos] == color
	&& ((safe_stones && safe_stones[pos])
	    || (!safe_stones && worm[pos].attack_codes[0] == 0))
	&& attack(pos, NULL)) {
      /* A safe worm of ours has become attackable. */
      if (defense_point) {
	find_defense(pos, defense_point);
	/* Check that this move is legal and effective also on the
	 * original board, otherwise find a tactical defense there
	 * instead.
	 */
	popgo();
	
	if (!is_legal(*defense_point, color)
	    || play_attack_defend_n(color, 1, 1, *defense_point, pos))
	  find_defense(pos, defense_point);
	
	/* Redo the move, we know that it won't fail. */
	trymove(move, color, NULL, NO_MOVE);
      }
      verbose = save_verbose;
      TRACE("After %1m Worm at %1m becomes attackable.\n", move, pos);
      verbose = current_verbose;
      *return_value += worm[pos].effective_size;
      if (safe_stones) /* Can't use mark_string. */
	for (ii = BOARDMIN; ii < BOARDMAX; ii++)
	  if (worm[ii].origin == worm[pos].origin)
	    safe_stones[ii] = 0;
    }
    else if (board[pos] == other
	     && worm[pos].origin == pos
	     && worm[pos].attack_codes[0] != 0
	     && worm[pos].defense_codes[0] == 0
	     && find_defense(pos, NULL)) {
      /* A dead opponent's worm has become defendable.
       * Also ask the owl code whether the string can live
       * strategically. To do this we need to temporarily undo
       * the trymove().
       */
      int owl_attacks;
      int defense_effective = 0;
      
      popgo();
      decrease_depth_values();
      owl_attacks = owl_does_attack(move, pos, NULL);
      if (owl_attacks != WIN) {
	*return_value += 2 * worm[pos].effective_size;
	defense_effective = 1;
	verbose = save_verbose;
	TRACE("After %1m worm at %1m becomes defendable - A.\n", move, pos);
	verbose = current_verbose;
      }
      else if (dragon[pos].status != ALIVE) {
	/* Before redoing the trymove we also check whether the worm now
	 * has a semeai defense. See blunder:26 for an example.
	 *
	 * If the worm already was alive in seki, it is generally okay
	 * that it also becomes tactically safe when the outer
	 * liberties are filled, see e.g. blunder:32,34. Thus the
	 * check above.
	 */
	int k;
	int adj[MAXCHAIN];
	int num_adj;
	num_adj = extended_chainlinks(pos, adj, 0);
	for (k = 0; k < num_adj; k++) {
	  int neighbor = adj[k];
	  int resulta;
	  owl_analyze_semeai_after_move(move, color, pos, neighbor,
					&resulta, NULL, NULL, 1, NULL, 1);
	  if (resulta != 0) {
	    *return_value += 2 * worm[pos].effective_size;
	    defense_effective = 1;
	    verbose = save_verbose;
	    TRACE("After %1m worm at %1m becomes defendable - B.\n",
		  move, pos);
	    verbose = current_verbose;
	    break;
	  }
	}
      }
      
      trymove(move, color, NULL, NO_MOVE);
      increase_depth_values();
      
      if (defense_effective && defense_point) {
	int dpos;
	if (attack(pos, &dpos)) {
	  *defense_point = dpos;
	  /* Check that this move is legal and effective also on the
           * original board, otherwise find a tactical attack there
           * instead.
	   */
	  popgo();
	  
	  if (!is_legal(dpos, color)
	      || play_attack_defend_n(color, 0, 1, dpos, pos))
	    attack(pos, defense_point);

	  /* Redo the move, we know that it won't fail. */
	  trymove(move, color, NULL, NO_MOVE);
	}
	else {
	  verbose = save_verbose;
	  TRACE("No attack found (unexpectedly) on %1m after move at %1m.\n",
		pos, move);
	  verbose = current_verbose;
	}
      }
    }
  }

  /* Look for double atari style complications of the move.
   *
   * FIXME: Since we have an atari_atari check in blunder_size(), do
   * we still need to do this step?
   */
  if (liberties == 2) {
    float d_a_blunder_size;
    if (double_atari(libs[0], other, &d_a_blunder_size, safe_stones)) {
      if (defense_point && safe_move(libs[0], color) == WIN)
	*defense_point = libs[0];
      *return_value += d_a_blunder_size;
      verbose = save_verbose;
      TRACE("Double threat appears at %1m.\n", libs[0]);
      verbose = current_verbose;
    }
    else if (double_atari(libs[1], other, &d_a_blunder_size, safe_stones)) {
      if (defense_point && safe_move(libs[1], color) == WIN)
	*defense_point = libs[1];
      *return_value += d_a_blunder_size;
      verbose = save_verbose;
      TRACE("Double threat appears at %1m.\n", libs[1]);
      verbose = current_verbose;
    }
  }
  
  /* Reset the depth values. */
  decrease_depth_values();

  popgo();
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
 * 
 * If safe_stones != NULL, then only attacks on stones marked as safe are
 * tried.
 *
 * The value of the double atari attack is returned in *value (unless
 * value is NULL), and the attacked stones are marked unsafe.
 */

int
double_atari(int move, int color, float *value,
	     signed char safe_stones[BOARDMAX])
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
     *
     * Only try to attack supposedly safe stones.
     */
    if (BOARD(m+dm, n+dn) == color
	&& BOARD(m, n+dn) == other
	&& BOARD(m+dm, n) == other
	&& (!safe_stones
	    || (safe_stones[POS(m, n+dn)] && safe_stones[POS(m+dm, n)]))
	&& trymove(move, color, "double_atari", NO_MOVE)) {
      if (countlib(move) > 1
	  && (BOARD(m, n+dn) == EMPTY || BOARD(m+dm, n) == EMPTY 
	      || !defend_both(POS(m, n+dn), POS(m+dm, n)))) {
	popgo();
	if (value) {
	  if (worm[POS(m, n+dn)].effective_size
	      > worm[POS(m+dm, n)].effective_size) {
	    *value = 2.0 * worm[POS(m, n+dn)].effective_size;
	    if (safe_stones)
	      mark_string(POS(m, n+dn), safe_stones, 0);
	  }
	  else {
	    *value = 2.0 * worm[POS(m+dm, n)].effective_size;
	    if (safe_stones)
	      mark_string(POS(m+dm, n), safe_stones, 0);
	  }
	}
	return 1;
      }
      popgo();
    }
  }
  
  return 0;
}
    

/* Returns true if a move by (color) plays into a snapback. */
int
playing_into_snapback(int move, int color)
{
  int libs[2];
  int k;
  
  if (approxlib(move, color, 1, NULL) != 0
      || accuratelib(move, color, 2, libs) != 1)
    return 0;

  for (k = 0; k < 4; k++)
    if (board[move + delta[k]] == color
	&& adjacent_strings(libs[0], move + delta[k]))
      return 1;

  return 0;
}


/* Score the game and determine the winner */

void
who_wins(int color, FILE *outfile)
{
  float result;

  silent_examine_position(EXAMINE_DRAGONS);

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
  result = aftermath_compute_score(color, NULL);
  if (result > 0.0)
    winner = WHITE;
  else {
    winner = BLACK;
    result = -result;
  }
#endif

  result = (white_score + black_score)/2.0;
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
		       signed char mx[BOARDMAX],
		       signed char ml[BOARDMAX],
		       signed char ma[BOARDMAX],
		       int do_add);

void
find_superstring(int str, int *num_stones, int *stones)
{
  do_find_superstring(str, num_stones, stones,
		      NULL, NULL, 0,
		      NULL, NULL, 0,
		      0, 1);
}

/* This is the same as find_superstring, except that connections of
 * type 5 are omitted. This is used in semeai analysis.
 */
void
find_superstring_conservative(int str, int *num_stones, int *stones)
{
  do_find_superstring(str, num_stones, stones,
		      NULL, NULL, 0,
		      NULL, NULL, 0,
		      0, 0);
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
  
  signed char mx[BOARDMAX]; /* stones */
  signed char ml[BOARDMAX]; /* liberties */
  signed char ma[BOARDMAX]; /* adjacent strings */

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
		       signed char mx[BOARDMAX],
		       signed char ml[BOARDMAX],
		       signed char ma[BOARDMAX],
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
static double timers[NUMBER_OF_TIMERS];

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

void
clearstats()
{
  stats.nodes                    = 0;
  stats.read_result_entered      = 0;
  stats.read_result_hits         = 0;
  stats.trusted_read_result_hits = 0;
}
  
void
showstats()
{
  gprintf("Nodes:                    %d\n", stats.nodes);
  gprintf("Read results entered:     %d\n", stats.read_result_entered);
  gprintf("Read result hits:         %d\n", stats.read_result_hits);
  gprintf("Trusted read result hits: %d\n", stats.trusted_read_result_hits);
}


/* Set up a compiled in pattern database for use by the Monte Carlo
 * code. If name is NULL, the first pattern database is used.
 *
 * The reason why this function and the next are placed here rather
 * than in montecarlo.c is to keep that file free from dependency on
 * patterns.h.
 */
int
choose_mc_patterns(char *name)
{
  int k;
  for (k = 0; mc_pattern_databases[k].name; k++) {
    if (!name || strcmp(name, mc_pattern_databases[k].name) == 0) {
      mc_init_patterns(mc_pattern_databases[k].values);
      return 1;
    }
  }

  return 0;
}

/* List compiled in Monte Carlo pattern databases. */
void
list_mc_patterns(void)
{
  int k;
  printf("Available builtin Monte Carlo local patterns:\n\n");
  for (k = 0; mc_pattern_databases[k].name; k++) {
    if (k == 0)
      printf("* %s (default)\n", mc_pattern_databases[k].name);
    else
      printf("* %s\n", mc_pattern_databases[k].name);
  }
  printf("\nUse \"--mc-patterns name\" to choose one of these.\n");
  printf("Use \"--mc-load-patterns filename\" to directly load a pattern database.\n");
}

/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
