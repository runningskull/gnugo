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


#define TRYMOVE(x, y, z) trymove2(x, y, z, "helper", -1, -1, EMPTY, -1, -1)
#define OFFSET(x, y, z, w) offset(x, y, ti, tj, &z, &w, transformation)
#define ARGS struct pattern *pattern, int transformation, int ti, int tj, int color


/* This file contains helper functions which assist the pattern matcher.
 * They are invoked with (ti,tj) = the position on the board marked with '*'.
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
basic_cut_helper (ARGS)
{
  int ai, aj, bi, bj, ci, cj;
  int acolor, ccolor;
  UNUSED(pattern);
  
  OFFSET(0, -1, ai, aj);  /* O to west */
  OFFSET(-1, 0, bi, bj);  /* O to north */
  OFFSET(-1, -1, ci, cj); /* X to northwest */

  acolor = BOARD(ai, aj);
  ccolor = OTHER_COLOR(acolor);

  ASSERT2(BOARD(ai, aj) != EMPTY, ai, aj);
  ASSERT2(BOARD(bi, bj) == acolor, bi, bj);
  ASSERT2(BOARD(ci, cj) == ccolor, ci, cj);

  /* If c is a ko stone, assume that we would lose the ko. */
  if (worm[ci][cj].attack_code != 0
      && (ccolor == color
	  || !worm[ci][cj].ko))
    return 0;
  if (worm[ti][tj].ko)
    return 0;

  if (TRYMOVE(ti, tj, ccolor)) {
    if ((attack(ti, tj, NULL, NULL) == WIN)
	|| (attack(ci, cj, NULL, NULL) == WIN)) {
      popgo();
      return 0;
    }
    popgo();
  }
  else
    return 0;

  if (safe_move2(ti, tj, acolor) == 0)
    return 0;

  /* Cut ok. */
  return 1;
}


/* Jump out into nothingness. To avoid jumping into our own territory,
 * we use the "area" measure. Also we never ever jump into our own
 * established eyespace.
 */

int 
jump_out_helper (ARGS)
{
  int own_eyespace;

  UNUSED(transformation); UNUSED(pattern);

  if (color==WHITE)
    own_eyespace = (white_eye[ti][tj].color == WHITE_BORDER);
  else
    own_eyespace = (black_eye[ti][tj].color == BLACK_BORDER);
  
  if (influence_area_color(ti, tj) != color && !own_eyespace)
    return 1;
  else
    return 0;
}


/* Make a long jump into nothingness. Since these jumps are not
 * securely connected we don't use them to jump into the opponent's
 * zone of control.
 */

int 
jump_out_far_helper (ARGS)
{
  if (influence_area_color(ti, tj) != OTHER_COLOR(color))
    return jump_out_helper(pattern, transformation, ti, tj, color);
  else
    return 0;
}


/* Active until the opponent has played his first stone.
 */

int
high_handicap_helper (ARGS)
{
  UNUSED(transformation); UNUSED(pattern); UNUSED(ti); UNUSED(tj);
  
  return stones_on_board(OTHER_COLOR(color)) == 0;
}


/* Active when the opponent is thought to be everywhere dead. This
 * typically happens early in high handicap games on small boards.
 * This helper is used by patterns intended to reinforce possible
 * weaknesses in the position.
 */

int
reinforce_helper (ARGS)
{
  UNUSED(transformation); UNUSED(pattern);
  
  return (!lively_dragon_exists(OTHER_COLOR(color)) 
	  && safe_move2(ti, tj, color));
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
  int ai, aj, bi, bj, ci, cj, di, dj, ei, ej;
  int success = 0;
  int other = OTHER_COLOR(color);
  int libs[2];
  UNUSED(pattern);
  
  OFFSET(0, 1, ai, aj);
  OFFSET(-1, 1, ci, cj);
  OFFSET(1, 1, di, dj);
  OFFSET(0, -1, ei, ej);

  /* Find second liberty of the stone a. */
  findlib(POS(ai, aj), 2, libs);
  if (libs[0] != POS(ti, tj)) {
    bi = I(libs[0]);
    bj = J(libs[0]);
  }
  else {
    bi = I(libs[1]);
    bj = J(libs[1]);
  }
  
  if (TRYMOVE(ti, tj, color)) {
    if (!attack(ci, cj, NULL, NULL) &&
	!(ON_BOARD2(di, dj) && attack(di, dj, NULL, NULL))) {
      if (TRYMOVE(bi, bj, other)) {
	if (attack(ai, aj, NULL, NULL))
	  success = 1;
	popgo();
      }
      else {
	success = 1; /* X move at (bi, bj) would have been suicide */
      }
    }
    popgo();
  }

  /* The followup is to capture the "a" string. Estimate the value to
     twice the size. */
  add_followup_value(ti, tj, 2 * worm[ai][aj].effective_size);

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
not_lunch_helper(int ai, int aj, int bi, int bj)
{
  int m, n;

  if (worm[ai][aj].size > 2)
    return 0;

  /* Tell the move generation code about the change in status. */
  remove_lunch(bi, bj, ai, aj);
  
  if (dragon[bi][bj].lunch == POS(ai, aj))
    for (m = 0; m < board_size; m++)
      for (n = 0; n < board_size; n++)
	if (dragon[m][n].origin == dragon[bi][bj].origin)
	  dragon[m][n].lunch = NO_MOVE;

  return 0;
}
  

/*
 *  ?OO                 ?OO
 *  O.O                 O.b
 *  .*.                 at.
 *  ---                 ---
 *
 * overturn spurious attack found by find_cap2
 *
 */

int
indirect_helper (ARGS)
{
  int ai, aj, bi, bj;

  OFFSET(0, -1, ai, aj);
  OFFSET(-1, 1, bi, bj);
  UNUSED(pattern);
  UNUSED(color);

  if ((worm[bi][bj].attack_point == POS(ti, tj))
      && (worm[bi][bj].defend_code == 0) && (does_defend(ai, aj, bi, bj)))
    change_attack(POS(bi, bj), 0, 0);

  return 0;
}


  
/* This is intended for use in autohelpers. */

/* Check whether the string at (ai, aj) can attack any surrounding
 * string. If so, return false as the move to create a seki (probably)
 * wouldn't work.
 */

int 
seki_helper(int ai, int aj)
{
  int r;
  int adj;
  int adjs[MAXCHAIN];
  
  adj = chainlinks(POS(ai, aj), adjs);
  for (r = 0; r < adj; r++)
    if (worm[I(adjs[r])][J(adjs[r])].attack_code != 0)
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
ugly_cutstone_helper (ARGS)
{
  int ai, aj;
  UNUSED(pattern);
  UNUSED(color);
  
  OFFSET(-1, -1, ai, aj);

  worm[ai][aj].cutstone++;
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
cutstone2_helper (ARGS)
{
  int ai, aj;
  int bi, bj;
  int ci, cj;
  int di, dj;
  UNUSED(pattern);
  UNUSED(color);
  
  OFFSET(-1, -1, ai, aj);
  OFFSET(-1,  0, bi, bj);
  OFFSET( 0, -1, ci, cj);

  if (worm[ai][aj].defend_code == 0)
    return 0;
  
  di = I(worm[ai][aj].defense_point);
  dj = J(worm[ai][aj].defense_point);

  if (TRYMOVE(di, dj, BOARD(ai, aj))) {
    if (!BOARD(bi, bj) || attack(bi, bj, NULL, NULL)
	|| !BOARD(ci, cj) || attack(ci, cj, NULL, NULL)
	|| safe_move2(ti, tj, BOARD(ai, aj)) != 0) {
      popgo();
      worm[I(worm[ai][aj].origin)][J(worm[ai][aj].origin)].cutstone2++;
      propagate_worm(I(worm[ai][aj].origin), J(worm[ai][aj].origin));
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
  int ai, aj;
  int bi, bj;
  int ci, cj;
  int dpos;
  int other = OTHER_COLOR(color);
  int success = 0;
  UNUSED(pattern);
  
  OFFSET( 0, 1, ai, aj);
  OFFSET(-1, 1, bi, bj);
  OFFSET(-1, 0, ci, cj);

  if (TRYMOVE(ti, tj, color)) {
    if (TRYMOVE(ai, aj, other)) {
      ASSERT2(countlib2(ti, tj) == 1, ti, tj);
      findlib(POS(ti, tj), 1, &dpos);
      if (TRYMOVE(I(dpos), J(dpos), color)) {
	if (TRYMOVE(bi, bj, color)) {
	  if (BOARD(ci, cj) == EMPTY || !defend_both(ai, aj, ci, cj))
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
threaten_to_save_helper(int ti, int tj, int ai, int aj)
{
  add_followup_value(ti, tj, 2 + 2 * worm[ai][aj].effective_size);
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
 */

void
threaten_to_capture_helper(int ti, int tj, int ai, int aj)
{
  int adj, adjs[MAXCHAIN];
  int k;
  
  adj = chainlinks2(POS(ai, aj), adjs, 1);
  for (k = 0; k < adj; k++)
    if (worm[I(adjs[k])][J(adjs[k])].defend_code != 0
	&& !does_defend(ti, tj, I(adjs[k]), J(adjs[k])))
      return;
    
  add_followup_value(ti, tj, 2 * worm[ai][aj].effective_size);
}


/*
 * This is intended for use in autohelpers.
 *
 * Estimate the value of defending a string which can be put into
 * atari and add this as a reverse followup value.
 */

void
defend_against_atari_helper(int ti, int tj, int ai, int aj)
{
  int adj, adjs[MAXCHAIN];
  int libs[2];
  int k;

  ASSERT2(countlib2(ai, aj) == 2, ai, aj);

  /* No value if the string can capture out of atari. */
  adj = chainlinks2(POS(ai, aj), adjs, 1);
  for (k = 0; k < adj; k++)
    if (worm[I(adjs[k])][J(adjs[k])].defend_code != 0
	&& !does_defend(ti, tj, I(adjs[k]), J(adjs[k])))
      return;

  /* No value if opponent has no safe atari. */
  findlib(POS(ai, aj), 2, libs);
  if (is_self_atari(libs[0], OTHER_COLOR(BOARD(ai, aj)))
      && is_self_atari(libs[1], OTHER_COLOR(BOARD(ai, aj))))
    return;
  
  add_reverse_followup_value(ti, tj, 2 * worm[ai][aj].effective_size);
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
amalgamate_most_valuable_helper(int ai, int aj, int bi, int bj, int ci, int cj)
{
  if (!same_dragon(ai, aj, bi, bj) && !same_dragon(bi, bj, ci, cj)) {
    if (dragon[ai][aj].effective_size >= dragon[ci][cj].effective_size)
      join_dragons(ai, aj, bi, bj);
    else
      join_dragons(bi, bj, ci, cj);
  }
}


/*
 * This is intended for use in autohelpers.
 *
 * Returns 1 if (ai, aj) is adjacent to a stone which can be captured by ko.
 */

int
finish_ko_helper(int ai, int aj)
{
  int adj, adjs[MAXCHAIN];
  int k;

  adj = chainlinks2(POS(ai, aj), adjs, 1);
  for (k = 0; k < adj; k++) {
    int bpos = adjs[k];
    int xpos;
    if (countstones(bpos) == 1) {
      findlib(bpos, 1, &xpos);
      if (is_ko(xpos, BOARD(ai, aj), NULL))
	return 1;
    }
  }

  return 0;
}


/*
 * This is intended for use in autohelpers.
 *
 * Returns 1 if (ai, aj) is next to a ko point
 */

int
squeeze_ko_helper(int ai, int aj)
{
  int libs[2];
  int liberties;
  int k;

  liberties = findlib(POS(ai, aj), 2, libs);
  ASSERT2(liberties==2, ai, aj);

  for (k = 0; k < liberties; k++) {
    int bpos = libs[k];
    if (is_ko(bpos, OTHER_COLOR(BOARD(ai, aj)), NULL))
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
backfill_helper(int ai, int aj, int bi, int bj, int ci, int cj)
{
  int color = BOARD(ci, cj);
  int other = OTHER_COLOR(color);
  int di = -1;
  int dj = -1;

  if (TRYMOVE(ai, aj, color)) {
    if (TRYMOVE(bi, bj, other)) {
      if (attack(ci, cj, NULL, NULL) && find_defense(ci, cj, &di, &dj)) {
	set_minimum_move_value(di, dj, 0.1);
	TRACE("%o...setting min move value of %m to 0.1\n", di, dj);
      }
      popgo();
    }
    popgo();
  }

  return 0;
}


/* Returns true if (ai, aj) kills or threatens to kill (bi, bj). */

int
owl_threatens_attack(int ai, int aj, int bi, int bj)
{
  if (dragon[bi][bj].owl_status == CRITICAL
      && dragon[bi][bj].owl_attack_point == POS(ai, aj))
    return 1;
  if (dragon[bi][bj].owl_threat_status == CAN_THREATEN_ATTACK)
    if ((dragon[bi][bj].owl_attack_point == POS(ai, aj))
	|| (dragon[bi][bj].owl_second_attack_point == POS(ai, aj)))
      return 1;
  return 0;
}


/*
 * LOCAL Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
