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


#define TRYMOVE(pos, color) trymove(pos, color, "helper", NO_MOVE, EMPTY, NO_MOVE)
#define OFFSET(x, y) offset(x, y, move, transformation)
#define ARGS struct pattern *pattern, int transformation, int move, int color


/* This file contains helper functions which assist the pattern matcher.
 * They are invoked with (move) = the position on the board marked with '*'.
 * They are invoked with color = WHITE or BLACK: any pieces on the
 * board marked with 'O' in the pattern will always contain color,
 * and 'X's contain OTHER_COLOR(color)
 *
 * The helper must return 0 if the pattern is rejected and 1 otherwise.
 */




/*
 *
 *  XO     cb
 *  O*     at
 *
 * Check whether a cut is feasible and effective.
 *
 */

int 
basic_cut_helper(ARGS)
{
  int apos, bpos, cpos;
  int acolor, ccolor;
  UNUSED(pattern);
  
  apos = OFFSET(0, -1);  /* O to west */
  bpos = OFFSET(-1, 0);  /* O to north */
  cpos = OFFSET(-1, -1); /* X to northwest */

  acolor = board[apos];
  ccolor = OTHER_COLOR(acolor);

  ASSERT1(board[apos] != EMPTY, apos);
  ASSERT1(board[bpos] == acolor, bpos);
  ASSERT1(board[cpos] == ccolor, cpos);

  /* If c is a ko stone, assume that we would lose the ko. */
  if (worm[cpos].attack_codes[0] != 0
      && (ccolor == color
	  || is_ko_point(cpos)))
    return 0;
  if (is_ko_point(move))
    return 0;

  if (TRYMOVE(move, ccolor)) {
    if ((attack(move, NULL) == WIN)
	|| (attack(cpos, NULL) == WIN)) {
      popgo();
      return 0;
    }
    popgo();
  }
  else
    return 0;

  if (safe_move(move, acolor) == 0)
    return 0;

  /* Cut ok. */
  return 1;
}


/* Jump out into nothingness. To avoid jumping into our own territory,
 * we use the "area" measure. Also we never ever jump into our own
 * established eyespace.
 */

int 
jump_out_helper(ARGS)
{
  int own_eyespace;

  UNUSED(transformation); UNUSED(pattern);

  if (color == WHITE)
    own_eyespace = (white_eye[move].color == WHITE_BORDER);
  else
    own_eyespace = (black_eye[move].color == BLACK_BORDER);
  
  if (influence_area_color(move) != color && !own_eyespace)
    return 1;
  else
    return 0;
}


/* Make a long jump into nothingness. Since these jumps are not
 * securely connected we don't use them to jump into the opponent's
 * zone of control.
 */

int 
jump_out_far_helper(ARGS)
{
  if (influence_area_color(move) != OTHER_COLOR(color))
    return jump_out_helper(pattern, transformation, move, color);
  else
    return 0;
}


/* Active until the opponent has played his first stone.
 */

int
high_handicap_helper(ARGS)
{
  UNUSED(transformation); UNUSED(pattern); UNUSED(move);
  
  return stones_on_board(OTHER_COLOR(color)) == 0;
}


/* Active when the opponent is thought to be everywhere dead. This
 * typically happens early in high handicap games on small boards.
 * This helper is used by patterns intended to reinforce possible
 * weaknesses in the position.
 */

int
reinforce_helper(ARGS)
{
  UNUSED(transformation); UNUSED(pattern);
  
  return (!lively_dragon_exists(OTHER_COLOR(color)) 
	  && safe_move(move, color));
}


/*
 *  
 *  XXO      XXc            decrease eye space in sente (unless it kills)
 *  .*X      eta
 *  ---      ---
 *
 * or
 *
 *  XXO      XXc            decrease eye space in sente (unless it kills)
 *  .*X      eta
 *  XXO      XXd
 *
 * or
 *
 *  |XXO     |XXc           decrease eye space in sente (unless it kills)
 *  |.*X     |eta
 *  |XXO     |XXd
 *
 * or
 *
 *  |XXO     |XXc           decrease eye space in sente (unless it kills)
 *  |.*X     |eta
 *  +---     +---
 *
 */

int
throw_in_atari_helper(ARGS)
{
  int apos, bpos, cpos, dpos;
  int success = 0;
  int other = OTHER_COLOR(color);
  int libs[2];
  UNUSED(pattern);
  
  apos = OFFSET(0, 1);
  cpos = OFFSET(-1, 1);
  dpos = OFFSET(1, 1);

  /* Find second liberty of the stone a. */
  findlib(apos, 2, libs);
  if (libs[0] != move)
    bpos = libs[0];
  else
    bpos = libs[1];
  
  if (TRYMOVE(move, color)) {
    if (!attack(cpos, NULL) &&
	!(ON_BOARD(dpos) && attack(dpos, NULL))) {
      if (TRYMOVE(bpos, other)) {
	if (attack(apos, NULL))
	  success = 1;
	popgo();
      }
      else {
	success = 1; /* X move at (bpos) would have been suicide */
      }
    }
    popgo();
  }

  /* The followup is to capture the "a" string. Estimate the value to
     twice the size. */
  add_followup_value(move, 2 * worm[apos].effective_size);

  return success;
}


/*
 *
 * Prevent misreporting of a as lunch for b.
 * To be used in autohelper action line. E.g.
 *
 *  XO|          ba|
 *  O*|          O*|
 *  oo|          oo|
 *  ?o|          ?o|
 *  
 *  >not_lunch(a,b);
 */

int
not_lunch_helper(int apos, int bpos)
{
  if (worm[apos].size > 2)
    return 0;

  /* Tell the move generation code about the change in status. */
  remove_lunch(bpos, apos);
  
  if (DRAGON2(bpos).lunch == apos)
    DRAGON2(bpos).lunch = NO_MOVE;

  return 0;
}
  

/* This is intended for use in autohelpers. */

/* Check whether the string at (ai, aj) can attack any surrounding
 * string. If so, return false as the move to create a seki (probably)
 * wouldn't work.
 */

int 
seki_helper(int str)
{
  int r;
  int adj;
  int adjs[MAXCHAIN];
  
  adj = chainlinks(str, adjs);
  for (r = 0; r < adj; r++)
    if (worm[adjs[r]].attack_codes[0] != 0)
      return 0;

  return 1;
}


/*
 *
 *  XO     aO
 *  O*     O*
 *
 * Used in a connection pattern to ensure that X is a cutstone.
 */

int 
ugly_cutstone_helper(ARGS)
{
  int apos;
  UNUSED(pattern);
  UNUSED(color);
  
  apos = OFFSET(-1, -1);
  
  worm[apos].cutstone++;
  return 0;
}


/*
 *  XO     aO
 *  O*     O*
 *
 * Increase the cutstone2 field if * is a potential cutting point,
 * i.e.  if it does work as a cutting point once 'a' has been
 * defended. This helper is expected to always return 0.
 */

int 
cutstone2_helper(ARGS)
{
  int apos;
  int bpos;
  int cpos;
  int dpos;
  UNUSED(pattern);
  UNUSED(color);
  
  apos = OFFSET(-1, -1);
  bpos = OFFSET(-1,  0);
  cpos = OFFSET( 0, -1);

  if (worm[apos].defend_codes[0] == 0)
    return 0;
  
  dpos = worm[apos].defense_points[0];

  if (TRYMOVE(dpos, board[apos])) {
    if (!board[bpos] || attack(bpos, NULL)
	|| !board[cpos] || attack(cpos, NULL)
	|| safe_move(move, board[apos]) != 0) {
      popgo();
      worm[worm[apos].origin].cutstone2++;
      propagate_worm(worm[apos].origin);
      return 0;
    }
    popgo();
  }

  return 0;
}

/*
 *  ?x??   ?x??
 *  ?X..   ?cb.
 *  O*..   O*a.
 *  ----   ----
 *
 */

int 
edge_double_sente_helper(ARGS)
{
  int apos;
  int bpos;
  int cpos;
  int dpos;
  int other = OTHER_COLOR(color);
  int success = 0;
  UNUSED(pattern);
  
  apos = OFFSET( 0, 1);
  bpos = OFFSET(-1, 1);
  cpos = OFFSET(-1, 0);

  if (TRYMOVE(move, color)) {
    if (TRYMOVE(apos, other)) {
      ASSERT1(countlib(move) == 1, move);
      findlib(move, 1, &dpos);
      if (TRYMOVE(dpos, color)) {
	if (TRYMOVE(bpos, color)) {
	  if (board[cpos] == EMPTY || !defend_both(apos, cpos))
	    success = 1;
	  popgo();
	}
	popgo();
      }
      popgo();
    }
    popgo();
  }

  return success;
}
	

/*
 * This is intended for use in autohelpers.
 *
 * Give a conservative estimate of the value of saving the string (ai, aj)
 * by capturing one opponent stone.
 */

void
threaten_to_save_helper(int move, int str)
{
  add_followup_value(move, 2.0 + 2.0 * worm[str].effective_size);
}


/*
 * This is intended for use in autohelpers.
 *
 * Estimate the value of capturing the string (ai, aj) and add this as
 * a followup value. We don't do this for too stupid looking threats,
 * however, e.g. in a position like
 *
 * OOOO..
 * XXX.*.
 * XOOOX.
 * XXXXO.
 *
 * where X can get out of atari with profit by capturing three O stones.
 *
 * Another case where we don't award the followup value is when the
 * opponent can defend with a threat against our move, e.g. in this
 * position:
 *
 * .OOOXX.
 * .OXXO.X
 * ..*.X..
 * ..XX...
 * 
 */

void
threaten_to_capture_helper(int move, int str)
{
  int adj, adjs[MAXCHAIN];
  int defense_move;
  int k;
  
  adj = chainlinks2(str, adjs, 1);
  for (k = 0; k < adj; k++)
    if (worm[adjs[k]].defend_codes[0] != 0
	&& !does_defend(move, adjs[k]))
      return;

  if (!TRYMOVE(move, OTHER_COLOR(board[str])))
    return;
  if (find_defense(str, &defense_move) != 0
      && defense_move != NO_MOVE
      && TRYMOVE(defense_move, board[str])) {
    if (board[move] == EMPTY || attack(move, NULL) != 0) {
      popgo();
      popgo();
      return;
    }
    popgo();
  }
  popgo();
  
  add_followup_value(move, 2.0 * worm[str].effective_size);
  TRACE("...followup value %f\n", 2.0 * worm[str].effective_size);
}


/*
 * This is intended for use in autohelpers.
 *
 * Estimate the value of defending a string which can be put into
 * atari and add this as a reverse followup value.
 */

void
defend_against_atari_helper(int move, int str)
{
  int adj, adjs[MAXCHAIN];
  int libs[2];
  int k;

  ASSERT1(countlib(str) == 2, str);

  /* No value if the string can capture out of atari. */
  adj = chainlinks2(str, adjs, 1);
  for (k = 0; k < adj; k++)
    if (worm[adjs[k]].defend_codes[0] != 0
	&& !does_defend(move, adjs[k]))
      return;

  /* No value if opponent has no safe atari. */
  findlib(str, 2, libs);
  if (!safe_move(libs[0], OTHER_COLOR(board[str]))
      && !safe_move(libs[1], OTHER_COLOR(board[str])))
    return;
  
  TRACE("...reverse followup value %f\n", 2.0 * worm[str].effective_size);
  add_reverse_followup_value(move, 2.0 * worm[str].effective_size);
}


/*
 * This is intended for use in conn.db autohelpers.
 *
 * Amalgamate either a with b or c with b, depending on which of the
 * two dragons a and c is largest.
 *
 * If either of these pairs already have been amalgamated somehow,
 * do nothing.
 */

void
amalgamate_most_valuable_helper(int apos, int bpos, int cpos)
{
  if (!is_same_dragon(apos, bpos) && !is_same_dragon(bpos, cpos)) {
    if (dragon[apos].effective_size >= dragon[cpos].effective_size)
      join_dragons(apos, bpos);
    else
      join_dragons(bpos, cpos);
  }
}


/*
 * This is intended for use in autohelpers.
 *
 * Returns 1 if (ai, aj) is adjacent to a stone which can be captured by ko.
 */

int
finish_ko_helper(int apos)
{
  int adj, adjs[MAXCHAIN];
  int k;

  adj = chainlinks2(apos, adjs, 1);
  for (k = 0; k < adj; k++) {
    int bpos = adjs[k];
    int xpos;
    if (countstones(bpos) == 1) {
      findlib(bpos, 1, &xpos);
      if (is_ko(xpos, board[apos], NULL))
	return 1;
    }
  }

  return 0;
}


/*
 * This is intended for use in autohelpers.
 *
 * Returns 1 if (ai, aj) is next to a ko point.
 */

int
squeeze_ko_helper(int apos)
{
  int libs[2];
  int liberties;
  int k;

  liberties = findlib(apos, 2, libs);
  ASSERT1(liberties == 2, apos);

  for (k = 0; k < liberties; k++) {
    int bpos = libs[k];
    if (is_ko(bpos, OTHER_COLOR(board[apos]), NULL))
      return 1;
  }

  return 0;
}

/*
 * This is intended for use in autohelpers.
 *
 * If after playing a and b, the string at c can be attacked, this
 * function adds a small fixed move value for a move which defends
 * c.
 */

int
backfill_helper(int apos, int bpos, int cpos)
{
  int color = board[cpos];
  int other = OTHER_COLOR(color);
  int dpos  = NO_MOVE;

  if (TRYMOVE(apos, color)) {
    if (TRYMOVE(bpos, other)) {
      if (attack(cpos, NULL) && find_defense(cpos, &dpos)) {
	set_minimum_move_value(dpos, 0.1);
	TRACE("%o...setting min move value of %1m to 0.1\n", dpos);
      }
      popgo();
    }
    popgo();
  }

  return 0;
}


/* Returns true if (apos) kills or threatens to kill (bpos). */

int
owl_threatens_attack(int apos, int bpos)
{
  if (dragon[bpos].owl_status == CRITICAL
      && dragon[bpos].owl_attack_point == apos)
    return 1;
  
  if (dragon[bpos].owl_threat_status == CAN_THREATEN_ATTACK)
    if (dragon[bpos].owl_attack_point == apos
	|| dragon[bpos].owl_second_attack_point == apos)
      return 1;
  
  return 0;
}


/* Returns true if O needs to connect at c in the position below after
 * O at b and X at d, because X can cut at c. In general d is the
 * second liberty of A, which must have exactly two liberties.
 *
 * |.X   |dX
 * |XO	 |AO
 * |XO	 |Ae
 * |..	 |bc
 */
   
int
connect_and_cut_helper(int Apos, int bpos, int cpos)
{
  int dpos;
  int epos = NO_MOVE;
  int other = board[Apos];
  int color = OTHER_COLOR(other);
  int libs[2];
  int liberties = findlib(Apos, 2, libs);
  int result = 0;
  int k;

  gg_assert(IS_STONE(color));
  gg_assert(liberties == 2);

  if (libs[0] == bpos)
    dpos = libs[1];
  else
    dpos = libs[0];

  for (k = 0; k < 4; k++)
    if (board[cpos + delta[k]] == color
	&& neighbor_of_string(cpos + delta[k], Apos)) {
      epos = cpos + delta[k];
      break;
    }

  gg_assert(epos != NO_MOVE);
  
  if (TRYMOVE(bpos, color)) {
    if (TRYMOVE(dpos, other)) {
      if (TRYMOVE(cpos, other)) {
	if (board[bpos] == EMPTY
	    || board[epos] == EMPTY
	    || !defend_both(bpos, epos))
	  result = 1;
	popgo();
      }
      popgo();
    }
    popgo();
  }
  
  return result;
}


/*
 * LOCAL Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
