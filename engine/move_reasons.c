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
#include <math.h>

#include "liberty.h"
#include "random.h"
#include "move_reasons.h"

/*
 * Private data structures used to collect information about moves.
 */

#define MAX_MOVE_REASONS 1000
#define MAX_WORMS 2*MAX_BOARD*MAX_BOARD/3
#define MAX_DRAGONS MAX_WORMS
#define MAX_CONNECTIONS 4*MAX_WORMS
#define MAX_WORM_PAIRS MAX_WORMS
#define MAX_EYES MAX_BOARD*MAX_BOARD/2
#define MAX_LUNCHES MAX_WORMS

static float compute_shape_factor(int m, int n);
static float connection_value(int ai, int aj, int bi, int bj, 
			      int ti, int tj, float margin);

static struct move_data move[MAX_BOARD][MAX_BOARD];
static struct move_reason move_reasons[MAX_MOVE_REASONS];
static int next_reason;

/* Worms */
static int worms[MAX_WORMS];
static int next_worm;

/* Dragons */
static int dragoni[MAX_DRAGONS];
static int dragonj[MAX_DRAGONS];
static int next_dragon;

/* Connections */
static int conn_dragon1[MAX_CONNECTIONS];
static int conn_dragon2[MAX_CONNECTIONS];
static int next_connection;

/* Unordered worm pairs */
static int worm_pair1[MAX_WORM_PAIRS];
static int worm_pair2[MAX_WORM_PAIRS];
static int next_worm_pair;

/* Eye shapes */
static int eyei[MAX_EYES];
static int eyej[MAX_EYES];
static int eyecolor[MAX_EYES];
static int next_eye;

/* Lunches */
static int lunch_dragon[MAX_LUNCHES]; /* eater */
static int lunch_worm[MAX_LUNCHES];   /* food */
static int next_lunch;

/* Point redistribution */
static int replacement_mapi[MAX_BOARD][MAX_BOARD];
static int replacement_mapj[MAX_BOARD][MAX_BOARD];

/* Initialize move reason data structures. */
void
clear_move_reasons(void)
{
  int i;
  int j;
  int k;
  next_reason = 0;
  next_worm = 0;
  next_dragon = 0;
  next_connection = 0;
  next_worm_pair = 0;
  next_eye = 0;
  next_lunch = 0;
  
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++) {
      move[i][j].value                  = 0.0;
      move[i][j].final_value            = 0.0;
      move[i][j].additional_ko_value    = 0.0;
      move[i][j].territorial_value      = 0.0;
      move[i][j].strategical_value      = 0.0;
      move[i][j].influence_value        = 0.0;
      move[i][j].maxpos_shape           = 0.0;
      move[i][j].numpos_shape           = 0;
      move[i][j].maxneg_shape           = 0.0;
      move[i][j].numneg_shape           = 0;
      move[i][j].followup_value         = 0.0;
      move[i][j].reverse_followup_value = 0.0;
      move[i][j].secondary_value        = 0.0;
      move[i][j].min_value              = 0.0;
      move[i][j].max_value              = HUGE_MOVE_VALUE;
      move[i][j].min_territory          = 0.0;
      move[i][j].max_territory          = HUGE_MOVE_VALUE;
      for (k = 0; k < MAX_REASONS; k++)     
	move[i][j].reason[k]            = -1;
      move[i][j].move_safety            = 0;
      move[i][j].worthwhile_threat      = 0;
      /* The reason we assign a random number to each move immediately
       * is to avoid dependence on which moves are evaluated when it
       * comes to choosing between multiple moves of the same value.
       * In this way we can get consistent results for use in the
       * regression tests.
       */
      move[i][j].random_number          = gg_drand();

      /* Do not send away the points (yet). */
      replacement_mapi[i][j] = -1;
      replacement_mapj[i][j] = -1;
    }
}

/*
 * Find the index of a worm in the list of worms. If necessary,
 * add a new entry. (ai, aj) must point to the origin of the worm.
 */
static int
find_worm(int str)
{
  int k;

  ASSERT_ON_BOARD1(str);
  for (k = 0; k < next_worm; k++)
    if (worms[k] == str)
      return k;
  
  /* Add a new entry. */
  gg_assert(next_worm < MAX_WORMS);
  worms[next_worm] = str;
  next_worm++;
  return next_worm - 1;
}

/*
 * Find the index of a dragon in the list of dragons. If necessary,
 * add a new entry. (ai, aj) must point to the origin of the dragon.
 */
static int
find_dragon(int ai, int aj)
{
  int k;
  ASSERT_ON_BOARD2(ai, aj);
  for (k = 0; k < next_dragon; k++)
    if ((dragoni[k] == ai) && (dragonj[k] == aj))
      return k;
  
  /* Add a new entry. */
  gg_assert(next_dragon < MAX_DRAGONS);
  dragoni[next_dragon] = ai;
  dragonj[next_dragon] = aj;
  next_dragon++;
  return next_dragon - 1;
}

/*
 * Find the index of a connection in the list of connections.
 * If necessary, add a new entry.
 */
static int
find_connection(int dragon1, int dragon2)
{
  int k;
  
  if (dragon1 > dragon2) {
    /* Swap to canonical order. */
    int tmp = dragon1;
    dragon1 = dragon2;
    dragon2 = tmp;
  }
  
  for (k = 0; k < next_connection; k++)
    if ((conn_dragon1[k] == dragon1) && (conn_dragon2[k] == dragon2))
      return k;
  
  /* Add a new entry. */
  gg_assert(next_connection < MAX_CONNECTIONS);
  conn_dragon1[next_connection] = dragon1;
  conn_dragon2[next_connection] = dragon2;
  next_connection++;
  return next_connection - 1;
}


/*
 * Find the index of an unordered pair of worms in the list of worm pairs.
 * If necessary, add a new entry.
 */
static int
find_worm_pair(int worm1, int worm2)
{
  int k;
  
  /* Make sure the worms are ordered canonically. */
  if (worm1 > worm2) {
    int tmp = worm1;
    worm1 = worm2;
    worm2 = tmp;
  }
  
  for (k = 0; k < next_worm_pair; k++)
    if ((worm_pair1[k] == worm1) && (worm_pair2[k] == worm2))
      return k;
  
  /* Add a new entry. */
  gg_assert(next_worm_pair < MAX_WORM_PAIRS);
  worm_pair1[next_worm_pair] = worm1;
  worm_pair2[next_worm_pair] = worm2;
  next_worm_pair++;
  return next_worm_pair - 1;
}

/*
 * Find the index of an eye space in the list of eye spaces.
 * If necessary, add a new entry.
 */
static int
find_eye(int i, int j, int color)
{
  int k;
  ASSERT_ON_BOARD2(i, j);
  
  for (k = 0; k < next_eye; k++)
    if ((eyei[k] == i) && (eyej[k] == j) && eyecolor[k] == color)
      return k;
  
  /* Add a new entry. */
  gg_assert(next_eye < MAX_EYES);
  eyei[next_eye] = i;
  eyej[next_eye] = j;
  eyecolor[next_eye] = color;
  next_eye++;
  return next_eye - 1;
}

/*
 * See if a lunch is already in the list of lunches, otherwise add a new
 * entry. A lunch is in this context a pair of eater (a dragon) and food
 * (a worm).
 */
void
add_lunch(int ai, int aj, int bi, int bj)
{
  int k;
  int dragon1 = find_dragon(dragon[ai][aj].origini, dragon[ai][aj].originj);
  int worm1   = find_worm(worm[bi][bj].origin);
  ASSERT_ON_BOARD2(ai, aj);
  ASSERT_ON_BOARD2(bi, bj);
  
  for (k = 0; k < next_lunch; k++)
    if ((lunch_dragon[k] == dragon1) && (lunch_worm[k] == worm1))
      return;
  
  /* Add a new entry. */
  gg_assert(next_lunch < MAX_LUNCHES);
  lunch_dragon[next_lunch] = dragon1;
  lunch_worm[next_lunch] = worm1;
  next_lunch++;
  return;
}

/*
 * Remove a lunch from the list of lunches.  A lunch is in this context a pair
 * of eater (a dragon) and food (a worm).  
 */
void
remove_lunch(int ai, int aj, int bi, int bj)
{
  int k;
  int dragon1 = find_dragon(dragon[ai][aj].origini, dragon[ai][aj].originj);
  int worm1   = find_worm(worm[bi][bj].origin);
  ASSERT_ON_BOARD2(ai, aj);
  ASSERT_ON_BOARD2(bi, bj);
  
  for (k = 0; k < next_lunch; k++)
    if ((lunch_dragon[k] == dragon1) && (lunch_worm[k] == worm1))
      break;
  
  if (k == next_lunch)
    return; /* Not found */
  
  /* Remove entry k. */
  lunch_dragon[k] = lunch_dragon[next_lunch - 1];
  lunch_worm[k] = lunch_worm[next_lunch - 1];
  next_lunch--;
}


/* ---------------------------------------------------------------- */


/*
 * Find a reason in the list of reasons. If necessary, add a new entry.
 */
static int
find_reason(int type, int what)
{
  int k;
  for (k = 0; k < next_reason; k++)
    if ((move_reasons[k].type == type) && (move_reasons[k].what == what))
      return k;
  
  /* Add a new entry. */
  gg_assert(next_reason < MAX_MOVE_REASONS);
  move_reasons[next_reason].type = type;
  move_reasons[next_reason].what = what;
  next_reason++;
  return next_reason - 1;
}

/*
 * Add a move reason for (ti, tj) if it's not already there or the
 * table is full.
 */ 
static void
add_move_reason(int ti, int tj, int type, int what)
{
  int k;

  ASSERT_ON_BOARD2(ti, tj);
  if (stackp == 0) {
    ASSERT2(BOARD(ti, tj) == EMPTY, ti, tj);
  }
  for (k = 0; k < MAX_REASONS; k++) {
    int r = move[ti][tj].reason[k];
    if (r < 0)
      break;
    if (move_reasons[r].type == type
	&& move_reasons[r].what == what)
      return;  /* Reason already listed. */
  }
  /* Reason not found, add it if there is place left. */
  if (k<MAX_REASONS)
    move[ti][tj].reason[k] = find_reason(type, what);
}

/*
 * Remove a move reason for (ti, tj). Ignore silently if the reason
 * wasn't there.
 */ 
static void
remove_move_reason(int ti, int tj, int type, int what)
{
  int k;
  int n = -1; /* Position of the move reason to be deleted. */
  ASSERT_ON_BOARD2(ti, tj);
  for (k = 0; k < MAX_REASONS; k++) {
    int r = move[ti][tj].reason[k];
    if (r < 0)
      break;
    if (move_reasons[r].type == type
	&& move_reasons[r].what == what)
      n = k;
  }
  
  if (n == -1)
    return; /* Move reason wasn't there. */
  
  /* Now move the last move reason to position n, thereby removing the
   * one we were looking for.
   */
  k--;
  move[ti][tj].reason[n] = move[ti][tj].reason[k];
  move[ti][tj].reason[k] = -1;
}

/*
 * Check whether a move reason already is recorded for a move.
 * A negative value for 'what' means only match 'type'.
 */
static int
move_reason_known(int ti, int tj, int type, int what)
{
  int k;
  int r;
  ASSERT_ON_BOARD2(ti, tj);
  for (k = 0; k < MAX_REASONS; k++) {
    r = move[ti][tj].reason[k];
    if (r < 0)
      break;
    if (move_reasons[r].type == type
	&& (what < 0
	    || move_reasons[r].what == what))
      return 1;
  }
  return 0;
}

/*
 * Add to the reasons for the move at (ti, tj) that it attacks the worm
 * at (ai, aj).
 */
void
add_attack_move(int ti, int tj, int ai, int aj)
{
  int worm_number = find_worm(worm[ai][aj].origin);

  ASSERT_ON_BOARD2(ai, aj);
  add_move_reason(ti, tj, ATTACK_MOVE, worm_number);
}

/* Query whether an attack move is already known. */
int
attack_move_known(int ti, int tj, int ai, int aj)
{
  int worm_number = find_worm(worm[ai][aj].origin);

  ASSERT_ON_BOARD2(ai, aj);
  return move_reason_known(ti, tj, ATTACK_MOVE, worm_number);
}

/*
 * Remove from the reasons for the move at (ti, tj) that it attacks
 * the worm at (ai, aj). We do this by adding a NON_ATTACK move
 * reason and wait until later to actually remove it. Otherwise it may
 * be added again.
 *
 * We must also check that there does exist an attack move reason for
 * this worm. Otherwise we may end up in an infinite loop when trying
 * to actually remove it.
 */
void
remove_attack_move(int ti, int tj, int ai, int aj)
{
  int worm_number = find_worm(worm[ai][aj].origin);

  ASSERT_ON_BOARD2(ai, aj);
  if (move_reason_known(ti, tj, ATTACK_MOVE, worm_number))
    add_move_reason(ti, tj, NON_ATTACK_MOVE, worm_number);
}

/*
 * Add to the reasons for the move at (ti, tj) that it defends the worm
 * at (ai, aj).
 */
void
add_defense_move(int ti, int tj, int ai, int aj)
{
  int worm_number = find_worm(worm[ai][aj].origin);

  ASSERT_ON_BOARD2(ai, aj);
  add_move_reason(ti, tj, DEFEND_MOVE, worm_number);
}

/* Query whether a defense move is already known. */
int
defense_move_known(int ti, int tj, int ai, int aj)
{
  int worm_number = find_worm(worm[ai][aj].origin);

  ASSERT_ON_BOARD2(ai, aj);
  return move_reason_known(ti, tj, DEFEND_MOVE, worm_number);
}

/*
 * Remove from the reasons for the move at (ti, tj) that it defends
 * the worm at (ai, aj). We do this by adding a NON_DEFEND move
 * reason and wait until later to actually remove it. Otherwise it may
 * be added again.
 *
 * We must also check that there does exist a defense move reason for
 * this worm. Otherwise we may end up in an infinite loop when trying
 * to actually remove it.
 */
void
remove_defense_move(int ti, int tj, int ai, int aj)
{
  int worm_number = find_worm(worm[ai][aj].origin);

  ASSERT_ON_BOARD2(ai, aj);
  if (move_reason_known(ti, tj, DEFEND_MOVE, worm_number))
    add_move_reason(ti, tj, NON_DEFEND_MOVE, worm_number);
}


/*
 * Add to the reasons for the move at (ti, tj) that it threatens to
 * attack the worm at (ai, aj). 
 */
void
add_attack_threat_move(int ti, int tj, int ai, int aj)
{
  int worm_number = find_worm(worm[ai][aj].origin);

  ASSERT_ON_BOARD2(ai, aj);
  add_move_reason(ti, tj, ATTACK_THREAT_MOVE, worm_number);
}

/* Query whether a threat to attack move is already known. */
int
attack_threat_move_known(int ti, int tj, int ai, int aj)
{
  int worm_number = find_worm(worm[ai][aj].origin);

  ASSERT_ON_BOARD2(ai, aj);
  return move_reason_known(ti, tj, ATTACK_THREAT_MOVE, worm_number);
}

/*
 * Add to the reasons for the move at (ti, tj) that it defends the worm
 * at (ai, aj).
 */
void
add_defense_threat_move(int ti, int tj, int ai, int aj)
{
  int worm_number = find_worm(worm[ai][aj].origin);

  ASSERT_ON_BOARD2(ai, aj);
  add_move_reason(ti, tj, DEFEND_THREAT_MOVE, worm_number);
}

/* Query whether a threat to defense move is already known. */
int
defense_threat_move_known(int ti, int tj, int ai, int aj)
{
  int worm_number = find_worm(worm[ai][aj].origin);

  ASSERT_ON_BOARD2(ai, aj);
  return move_reason_known(ti, tj, DEFEND_THREAT_MOVE, worm_number);
}


/*
 * Add to the reasons for the move at (ti, tj) that it connects the
 * dragons at (ai, aj) and (bi, bj). Require that the dragons are
 * distinct.
 */
void
add_connection_move(int ti, int tj, int ai, int aj, int bi, int bj)
{
  int dragon1 = find_dragon(dragon[ai][aj].origini, dragon[ai][aj].originj);
  int dragon2 = find_dragon(dragon[bi][bj].origini, dragon[bi][bj].originj);
  int connection;
  ASSERT_ON_BOARD2(ai, aj);
  ASSERT_ON_BOARD2(bi, bj);
  gg_assert (dragon[ai][aj].color == dragon[bi][bj].color);
  if (dragon1 == dragon2)
    return;
  connection = find_connection(dragon1, dragon2);
  add_move_reason(ti, tj, CONNECT_MOVE, connection);
}

/*
 * Add to the reasons for the move at (ti, tj) that it cuts the
 * dragons at (ai, aj) and (bi, bj). Require that the dragons are
 * distinct.
 */
void
add_cut_move(int ti, int tj, int ai, int aj, int bi, int bj)
{
  int dragon1 = find_dragon(dragon[ai][aj].origini, dragon[ai][aj].originj);
  int dragon2 = find_dragon(dragon[bi][bj].origini, dragon[bi][bj].originj);
  int connection;
  ASSERT_ON_BOARD2(ai, aj);
  ASSERT_ON_BOARD2(bi, bj);
  gg_assert (dragon[ai][aj].color == dragon[bi][bj].color);
  if (dragon1 == dragon2)
    return;
  connection = find_connection(dragon1, dragon2);
  
  /*
   * Ignore the cut or connection if either (ai, aj) or (bi, bj)
   * points to a tactically captured worm.
   */
  if ((worm[ai][aj].attack_code != 0 && worm[ai][aj].defend_code == 0)
      || (worm[bi][bj].attack_code != 0 && worm[bi][bj].defend_code == 0))
    return;
  
  add_move_reason(ti, tj, CUT_MOVE, connection);
}

/*
 * Add to the reasons for the move at (ti, tj) that it is an anti-suji.
 * This means that it's a locally inferior move or for some other reason
 * must *not* be played.
 */
void
add_antisuji_move(int ti, int tj)
{
  add_move_reason(ti, tj, ANTISUJI_MOVE, 0);
}

/*
 * Add to the reasons for the move at (ti, tj) that it wins the
 * dragon (friendly or not) at (ai, aj) in semeai. Since it is
 * possible that in some semeai one player can kill but the
 * other can only make seki, it is possible that one dragon
 * is already alive in seki. Therefore separate move reasons
 * must be added for the two dragons.
 */
void
add_semeai_move(int ti, int tj, int ai, int aj)
{
  int the_dragon = find_dragon(dragon[ai][aj].origini, dragon[ai][aj].originj);
  ASSERT_ON_BOARD2(ai, aj);
  add_move_reason(ti, tj, SEMEAI_MOVE, the_dragon);
}

/*
 * Add to the reasons for the move at (ti, tj) that given two
 * moves in a row a move here can win the dragon (friendly or
 * not) at (ai, aj) in semeai. Such a move can be used as a 
 * ko threat, and it is also given some value due to uncertainty
 * in the counting of liberties.
 */
void
add_semeai_threat(int ti, int tj, int ai, int aj)
{
  int the_dragon = find_dragon(dragon[ai][aj].origini, dragon[ai][aj].originj);
  ASSERT_ON_BOARD2(ai, aj);
  add_move_reason(ti, tj, SEMEAI_THREAT, the_dragon);
}

/*
 * Add to the reasons for the move at (ti, tj) that it's the vital
 * point for the eye space at (ai, aj) of color.
 */
void
add_vital_eye_move(int ti, int tj, int ai, int aj, int color)
{
  int eye;
  ASSERT_ON_BOARD2(ai, aj);
  if (color == WHITE)
    eye = find_eye(white_eye[ai][aj].origini, white_eye[ai][aj].originj,
		   color);
  else
    eye = find_eye(black_eye[ai][aj].origini, black_eye[ai][aj].originj,
		   color);
  add_move_reason(ti, tj, VITAL_EYE_MOVE, eye);
}

/*
 * Add to the reasons for the move at (ti, tj) that it attacks
 * either (ai, aj) or (bi, bj) (e.g. a double atari). This move
 * reason is only used for double attacks on opponent stones.
 *
 * Before accepting the move reason, check that the worms are
 * distinct and that neither is undefendable.
 */
void
add_attack_either_move(int ti, int tj, int ai, int aj, int bi, int bj)
{
  int worm1 = find_worm(worm[ai][aj].origin);
  int worm2 = find_worm(worm[bi][bj].origin);
  int worm_pair;

  ASSERT_ON_BOARD2(ai, aj);
  ASSERT_ON_BOARD2(bi, bj);
  if (worm1 == worm2)
    return;
  if (worm[ai][aj].attack_code != 0 && worm[ai][aj].defend_code == 0)
    return;
  if (worm[bi][bj].attack_code != 0 && worm[bi][bj].defend_code == 0)
    return;
  worm_pair = find_worm_pair(worm1, worm2);
  add_move_reason(ti, tj, ATTACK_EITHER_MOVE, worm_pair);
}

/*
 * Add to the reasons for the move at (ti, tj) that it defends
 * both (ai, aj) and (bi, bj) (e.g. from a double atari). This move
 * reason is only used for defense of own stones.
 */
void
add_defend_both_move(int ti, int tj, int ai, int aj, int bi, int bj)
{
  int worm1 = find_worm(worm[ai][aj].origin);
  int worm2 = find_worm(worm[bi][bj].origin);
  int worm_pair = find_worm_pair(worm1, worm2);

  ASSERT_ON_BOARD2(ai, aj);
  ASSERT_ON_BOARD2(bi, bj);
  add_move_reason(ti, tj, DEFEND_BOTH_MOVE, worm_pair);
}

/*
 * Add to the reasons for the move at (ti, tj) that it secures
 * territory by blocking.
 */
void
add_block_territory_move(int ti, int tj)
{
  add_move_reason(ti, tj, BLOCK_TERRITORY_MOVE, 0);
}

/*
 * Add to the reasons for the move at (ti, tj) that it expands
 * territory.
 */
void
add_expand_territory_move(int ti, int tj)
{
  add_move_reason(ti, tj, EXPAND_TERRITORY_MOVE, 0);
}

/*
 * Add to the reasons for the move at (ti, tj) that it expands
 * moyo.
 */
void
add_expand_moyo_move(int ti, int tj)
{
  add_move_reason(ti, tj, EXPAND_MOYO_MOVE, 0);
}

/*
 * This function is called when a shape value for the move at (ti, tj)
 * is found. 
 * 
 * We keep track of the largest positive shape value found, and the
 * total number of positive contributions, as well as the largest
 * negative shape value found, and the total number of negative
 * shape contributions.
 */
void
add_shape_value(int ti, int tj, float value)
{
  ASSERT_ON_BOARD2(ti, tj);
  if (value > 0.0) {
    if (value > move[ti][tj].maxpos_shape)
      move[ti][tj].maxpos_shape = value;
    move[ti][tj].numpos_shape += 1;
  }
  else if (value < 0.0) {
    value = -value;
    if (value > move[ti][tj].maxneg_shape)
      move[ti][tj].maxneg_shape = value;
    move[ti][tj].numneg_shape += 1;
  }
}

/*
 * Flag that this move is worthwhile to play as a pure threat move.
 */
void
add_worthwhile_threat_move(int ti, int tj)
{
  move[ti][tj].worthwhile_threat = 1;
}

/* 
 * This function computes the shape factor, which multiplies
 * the score of a move. We take the largest positive contribution
 * to shape and add 1 for each additional positive contribution found.
 * Then we take the largest negative contribution to shape, and
 * add 1 for each additional negative contribution. The resulting
 * number is raised to the power 1.05.
 *
 * The rationale behind this complicated scheme is that every
 * shape point is very significant. If two shape contributions
 * with values (say) 5 and 3 are found, the second contribution
 * should be devalued to 1. Otherwise the engine is too difficult to
 * tune since finding multiple contributions to shape can cause
 * significant overvaluing of a move.
 */

static float
compute_shape_factor(int m, int n)
{
  float exponent = move[m][n].maxpos_shape - move[m][n].maxneg_shape;
  ASSERT_ON_BOARD2(m, n);
  if (move[m][n].numpos_shape > 1)
    exponent += move[m][n].numpos_shape - 1;
  if (move[m][n].numneg_shape > 1)
    exponent -= move[m][n].numneg_shape - 1;
  return pow(1.05, exponent);
}


/*
 * Add to the reasons for the move at (ti, tj) that it attacks
 * the dragon (ai, aj) on a strategical level.
 */
void
add_strategical_attack_move(int ti, int tj, int ai, int aj)
{
  int dragon1 = find_dragon(dragon[ai][aj].origini, dragon[ai][aj].originj);
  ASSERT_ON_BOARD2(ai, aj);
  add_move_reason(ti, tj, STRATEGIC_ATTACK_MOVE, dragon1);
}

/*
 * Add to the reasons for the move at (ti, tj) that it defends
 * the dragon (ai, aj) on a strategical level.
 */
void
add_strategical_defense_move(int ti, int tj, int ai, int aj)
{
  int dragon1 = find_dragon(dragon[ai][aj].origini, dragon[ai][aj].originj);
  ASSERT_ON_BOARD2(ai, aj);
  add_move_reason(ti, tj, STRATEGIC_DEFEND_MOVE, dragon1);
}

/*
 * Add to the reasons for the move at (ti, tj) that the owl
 * code reports an attack on the dragon (ai, aj).
 */
void
add_owl_attack_move(int ti, int tj, int ai, int aj)
{
  int dragon1 = find_dragon(dragon[ai][aj].origini, dragon[ai][aj].originj);
  ASSERT_ON_BOARD2(ai, aj);
  add_move_reason(ti, tj, OWL_ATTACK_MOVE, dragon1);
}

/*
 * Add to the reasons for the move at (ti, tj) that the owl
 * code reports a defense of the dragon (ai, aj).
 */
void
add_owl_defense_move(int ti, int tj, int ai, int aj)
{
  int dragon1 = find_dragon(dragon[ai][aj].origini, dragon[ai][aj].originj);
  ASSERT_ON_BOARD2(ai, aj);
  add_move_reason(ti, tj, OWL_DEFEND_MOVE, dragon1);
}

/*
 * Add to the reasons for the move at (ti, tj) that the owl
 * code reports a move threatening to attack the dragon enemy (ai, aj).
 * That is, if the attacker is given two moves in a row, (ti, tj)
 * can be the first move.
 */
void
add_owl_attack_threat_move(int ti, int tj, int ai, int aj)
{
  int dragon1 = find_dragon(dragon[ai][aj].origini, dragon[ai][aj].originj);
  ASSERT_ON_BOARD2(ai, aj);
  add_move_reason(ti, tj, OWL_ATTACK_THREAT, dragon1);
  add_worthwhile_threat_move(ti, tj);
}

/* The owl code found the friendly dragon alive, or the unfriendly dragon
 * dead, and an extra point of attack or defense was found, so this might be a
 * good place to play.  
 */

void
add_owl_uncertain_defense_move(int ti, int tj, int ai, int aj)
{
  int dragon1 = find_dragon(dragon[ai][aj].origini, dragon[ai][aj].originj);
  ASSERT_ON_BOARD2(ai, aj);
  add_move_reason(ti, tj, UNCERTAIN_OWL_DEFENSE, dragon1);
}

/* The owl code found the opponent dragon alive, or the friendly
 * dragon dead, but was uncertain, and this move reason propose
 * an attack or defense which is expected to fail but might succeed.
 */

void
add_owl_uncertain_attack_move(int ti, int tj, int ai, int aj)
{
  int dragon1 = find_dragon(dragon[ai][aj].origini, dragon[ai][aj].originj);
  ASSERT_ON_BOARD2(ai, aj);
  add_move_reason(ti, tj, UNCERTAIN_OWL_ATTACK, dragon1);
}

/*
 * Add to the reasons for the move at (ti, tj) that the owl
 * code reports a move threatening to rescue the dragon (ai, aj).
 * That is, if the defender is given two moves in a row, (ti, tj)
 * can be the first move.
 */
void
add_owl_defense_threat_move(int ti, int tj, int ai, int aj)
{
  int dragon1 = find_dragon(dragon[ai][aj].origini, dragon[ai][aj].originj);
  ASSERT_ON_BOARD2(ai, aj);
  add_move_reason(ti, tj, OWL_DEFENSE_THREAT, dragon1);
  add_worthwhile_threat_move(ti, tj);
}

/* Add to the reasons for the move at (ti, tj) that it captures
 * at least one of a set of worms which individually are tactically
 * safe (such as a double atari). Only one such move reason is
 * permitted per move.
 */
void
add_my_atari_atari_move(int ti, int tj, int size)
{
  add_move_reason(ti, tj, MY_ATARI_ATARI_MOVE, size);
}

/* Add to the reasons for the move at (ti, tj) that an opponent move there
 * would capture at least one of a set of worms which individually are
 * tactically safe (such as a double atari), and that a move there
 * by the defender is safe---presumably it defends the threat.
 * Only one such move reason is permitted per move.  */
void
add_your_atari_atari_move(int ti, int tj, int size)
{
  add_move_reason(ti, tj, YOUR_ATARI_ATARI_MOVE, size);
}


/*
 * Add to the reasons for the move at (ti, tj) that the owl
 * code reports a move threatening to defend the dragon enemy (ai, aj),
 * and that (ti, tj) is a move which attacks the dragon. 
 * That is, if the defender is given two moves in a row, (ti, tj)
 * can be the first move. Hopefully playing at (ti, tj) makes it harder 
 * for the dragon to live.
 */
void
add_owl_prevent_threat_move(int ti, int tj, int ai, int aj)
{
  int dragon1 = find_dragon(dragon[ai][aj].origini, dragon[ai][aj].originj);
  ASSERT_ON_BOARD2(ai, aj);
  add_move_reason(ti, tj, OWL_PREVENT_THREAT, dragon1);
}

/*
 * Add value of followup moves. 
 */
void
add_followup_value(int ti, int tj, float value)
{
  ASSERT_ON_BOARD2(ti, tj);
  if (value > move[ti][tj].followup_value)
    move[ti][tj].followup_value = value;
}

/*
 * Add value of inverse followup moves. 
 */
void
add_reverse_followup_value(int ti, int tj, float value)
{
  ASSERT_ON_BOARD2(ti, tj);
  if (value > move[ti][tj].reverse_followup_value)
    move[ti][tj].reverse_followup_value = value;
}

/*
 * Set a minimum allowed value for the move.
 */
void
set_minimum_move_value(int ti, int tj, float value)
{
  ASSERT_ON_BOARD2(ti, tj);
  if (value > move[ti][tj].min_value)
    move[ti][tj].min_value = value;
}

/*
 * Set a maximum allowed value for the move.
 */
void
set_maximum_move_value(int ti, int tj, float value)
{
  ASSERT_ON_BOARD2(ti, tj);
  if (value < move[ti][tj].max_value)
    move[ti][tj].max_value = value;
}

/*
 * Set a minimum allowed territorial value for the move.
 */
void
set_minimum_territorial_value(int ti, int tj, float value)
{
  ASSERT_ON_BOARD2(ti, tj);
  if (value > move[ti][tj].min_territory)
    move[ti][tj].min_territory = value;
}

/*
 * Set a maximum allowed territorial value for the move.
 */
void
set_maximum_territorial_value(int ti, int tj, float value)
{
  ASSERT_ON_BOARD2(ti, tj);
  if (value < move[ti][tj].max_territory)
    move[ti][tj].max_territory = value;
}

/* 
 * Add a point redistribution rule, sending the points from (ai, aj)
 * to (bi, bj). 
 */
void
add_replacement_move(int ai, int aj, int bi, int bj)
{
  int ci, cj;
  int m, n;
  ASSERT_ON_BOARD2(ai, aj);
  ASSERT_ON_BOARD2(bi, bj);
  ci = replacement_mapi[bi][bj];
  cj = replacement_mapj[bi][bj];

  /* First check for an incompatible redistribution rule. */
  if (replacement_mapi[ai][aj] != -1) {
    int di = replacement_mapi[ai][aj];
    int dj = replacement_mapj[ai][aj];
    /* Crash if the old rule isn't compatible with the new one. */
    ASSERT2((bi == di && bj == dj)
	   || (bi == replacement_mapi[di][dj]
	       && bj == replacement_mapj[di][dj]), ai, aj);
    /* There already is a compatible redistribution in effect so we
     * have nothing more to do.
     */
    return;
  }

  TRACE("Move at %m is replaced by %m.\n", ai, aj, bi, bj);    

  /* Verify that we don't introduce a cyclic redistribution. */
  if (ci == ai && cj == aj) {
    gprintf("Cyclic point redistribution detected.\n");
    ASSERT2(0, ai, aj);
  }

  /* Update the replacement map. Make sure that all replacements
   * always are directed immediately to the final destination.
   */
  if (ci != -1) {
    replacement_mapi[ai][aj] = ci;
    replacement_mapj[ai][aj] = cj;
  }
  else {
    replacement_mapi[ai][aj] = bi;
    replacement_mapj[ai][aj] = bj;
  }
  
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      if (replacement_mapi[m][n] == ai
	  && replacement_mapj[m][n] == aj) {
	replacement_mapi[m][n] = replacement_mapi[ai][aj];
	replacement_mapj[m][n] = replacement_mapj[ai][aj];
      }
    }
}


/* ---------------------------------------------------------------- */


/* Test all moves which defend, attack, connect or cut to see if they
 * also attack or defend some other worm.
 *
 * FIXME: We would like to see whether an arbitrary move works to cut
 *        or connect something else too.
 */

static void
find_more_attack_and_defense_moves(int color)
{
  int unstable_worms[MAX_WORMS];
  int N = 0;  /* number of unstable worms */
  int m, n;
  int k;
  int other = OTHER_COLOR(color);
  
  TRACE("\nLooking for additional attack and defense moves. Trying moves ...\n");
  
  /* Identify the unstable worms and store them in a list. */
  for (m = 0; m < board_size; m++)
    for (n = 0; n  <board_size; n++)
      if (BOARD(m, n)
	  && worm[m][n].origin == POS(m, n)
	  && worm[m][n].attack_code != 0
	  && worm[m][n].defend_code != 0) {
	unstable_worms[N] = find_worm(POS(m, n));
	N++;
      }
  
  /* To avoid horizon effects, we temporarily increase the depth values. */
  increase_depth_values();
  
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      for (k = 0; k < MAX_REASONS; k++) {
	int r = move[m][n].reason[k];
	int what;
	if (r < 0)
	  break;
	what = move_reasons[r].what;
	if ((   move_reasons[r].type == ATTACK_MOVE
		&& board[worms[what]] == other)
	    || (move_reasons[r].type == DEFEND_MOVE
		&& board[worms[what]] == color)
	    || move_reasons[r].type == CONNECT_MOVE
	    || move_reasons[r].type == CUT_MOVE
	    || move_reasons[r].type == ATTACK_EITHER_MOVE
	    || move_reasons[r].type == DEFEND_BOTH_MOVE)
	  break;
      }
      
      if (k<MAX_REASONS && move[m][n].reason[k] != -1) {
	/* Try the move at (m,n) and see what happens. */
	int cursor_at_start_of_line = 0;
	TRACE("%m ",m ,n);
	if (trymove2(m, n, color, "find_more_attack_and_defense_moves",
		     -1, -1, EMPTY, -1, -1)) {
	  for (k = 0; k < N; k++) {
	    int ai = I(worms[unstable_worms[k]]);
	    int aj = J(worms[unstable_worms[k]]);
	    /* string of our color, see if there still is an attack,
	     * unless we already know the move works as defense move.
	     */
	    if (BOARD(ai, aj) == color
		&& !move_reason_known(m, n, DEFEND_MOVE, unstable_worms[k]))
	      if (!attack(ai, aj, NULL, NULL)) {
		if (!cursor_at_start_of_line)
		  TRACE("\n");
		TRACE("%ofound extra point of defense of %m at %m\n",
		      ai, aj, m, n);
		cursor_at_start_of_line = 1;
		add_defense_move(m, n, ai, aj);
	      }
	    
	    /* string of opponent color, see if there still is a defense,
	     * unless we already know the move works as attack move.
	     */
	    if (BOARD(ai, aj) == other
		&& !move_reason_known(m, n, ATTACK_MOVE, unstable_worms[k]))
	      if (!find_defense(ai, aj, NULL, NULL)) {
		/* Maybe find_defense() doesn't find the defense. Try to
		 * defend with the stored defense move.
		 */
		int attack_works = 1;
		if (trymove(worm[ai][aj].defense_point, other, 
			     "find_more_attack_and_defense_moves", 0,
			     EMPTY, 0)) {
		  if (!attack(ai, aj, NULL, NULL))
		    attack_works = 0;
		  popgo();
		}
		
		if (attack_works) {
		  if (!cursor_at_start_of_line)
		    TRACE("\n");
		  TRACE("%ofound extra point of attack of %m at %m\n",
			ai, aj, m, n);
		  cursor_at_start_of_line = 1;
		  add_attack_move(m, n, ai, aj);
		}
	      }
	  }
	  popgo();
	}
      }
    }
  
  TRACE("\n");
  decrease_depth_values();
}

/*
 * Remove attacks on own stones and defense of opponent stones, i.e.
 * moves which are only relevant for the opponent.
 *
 * Comment: I once thought it would be useful to take these into
 * account (cf. the proverb "opponent's key points are your key
 * points") but now it seems they only lead to trouble. It's easiest
 * just to remove them altogether.
 *
 * FIXME: There is a certain amount of superfluous code that tests
 * against this case which now can be removed.
 */
static void
remove_opponent_attack_and_defense_moves(int color)
{
  int m, n, k;
  int ai, aj;
  int found_one;
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      do {
	/* If we do remove something, we must restart the loop to be
         * sure that we look through all move_reasons.
	 */
	found_one = 0;
	for (k = 0; k < MAX_REASONS; k++) {
	  int r = move[m][n].reason[k];
	  if (r < 0)
	    break;
	  ai = I(worms[move_reasons[r].what]);
	  aj = J(worms[move_reasons[r].what]);
	  if (move_reasons[r].type == ATTACK_MOVE
	      && BOARD(ai, aj) == color) {
	    remove_move_reason(m, n, ATTACK_MOVE, move_reasons[r].what);
	    found_one = 1;
	    break;
	  }
	  else if (move_reasons[r].type == DEFEND_MOVE
		   && BOARD(ai, aj) == OTHER_COLOR(color)) {
	    remove_move_reason(m, n, DEFEND_MOVE, move_reasons[r].what);
	    found_one = 1;
	    break;
	  }
	}
      } while (found_one);
    }
}

/*
 * Remove attacks and defenses that have earlier been marked as
 * NON_ATTACK or NON_DEFEND respectively, because they actually don't
 * work.
 */
static void
do_remove_false_attack_and_defense_moves(void)
{
  int m, n, k;
  int found_one;
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      do {
	/* If we do remove something, we must restart the loop to be
         * sure that we look through all move_reasons.
	 */
	found_one = 0;
	for (k = 0; k < MAX_REASONS; k++) {
	  int r = move[m][n].reason[k];
	  if (r < 0)
	    break;
	  if (move_reasons[r].type == NON_ATTACK_MOVE) {
	    int ai = I(worms[move_reasons[r].what]);
	    int aj = J(worms[move_reasons[r].what]);
	    remove_move_reason(m, n, ATTACK_MOVE, move_reasons[r].what);
	    /* Remove ourselves too. */
	    remove_move_reason(m, n, NON_ATTACK_MOVE, move_reasons[r].what);
	    /* If (m, n) was the attack point of the worm (ai, aj), we
	     * call change_attack to remove it.
	     *
	     * FIXME: We should look through the move reasons to see
	     * whether we could come up with an alternate attack point.
	     */
	    change_attack(POS(ai, aj), 0, 0);
	    /* If there was no attack, there is nothing to defend against. */
	    if (worm[ai][aj].defend_code != 0) {
	      remove_move_reason(m, n, DEFEND_MOVE, move_reasons[r].what);
	      change_defense(POS(ai, aj), 0, 0);
	    }
	    /* FIXME: We should also remove all defense reasons for
             * this worm.
	     */
	    found_one = 1;
	    break;
	  }
	  else if (move_reasons[r].type == NON_DEFEND_MOVE) {
	    int ai = I(worms[move_reasons[r].what]);
	    int aj = J(worms[move_reasons[r].what]);
	    remove_move_reason(m, n, DEFEND_MOVE, move_reasons[r].what);
	    /* Remove ourselves too. */
	    remove_move_reason(m, n, NON_DEFEND_MOVE, move_reasons[r].what);
	    /* If (m, n) was the defense point of the worm (ai, aj), we
	     * call change_defense to remove it.
	     *
	     * FIXME: We should look through the move reasons to see
	     * whether we could come up with an alternate defense point.
	     */
	    change_defense(POS(ai, aj), 0, 0);
	    found_one = 1;
	    break;
	  }
	}
      } while (found_one);
    }
}


/* Test all moves which strategically attack or defend some owl
 * critical dragon and see whether the move is an effective owl attack
 * or owl defense for this dragon. We also test whether tactical
 * attacks on a part of a critical dragon do owl attack the entire
 * dragon.
 */
static void
find_more_owl_attack_and_defense_moves(int color)
{
  int m, n;
  int k;
  int di, dj;
  
  TRACE("\nTrying to upgrade strategical attack and defense moves.\n");
  
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      for (k = 0; k < MAX_REASONS; k++) {
	int r = move[m][n].reason[k];
	int what;
	if (r < 0)
	  break;
	what = move_reasons[r].what;
	if (move_reasons[r].type == STRATEGIC_ATTACK_MOVE
	    || move_reasons[r].type == STRATEGIC_DEFEND_MOVE) {
	  di = dragoni[what];
	  dj = dragonj[what];
	}
	else if (move_reasons[r].type == ATTACK_MOVE
		 || move_reasons[r].type == DEFEND_MOVE) {
	  di = I(worms[what]);
	  dj = J(worms[what]);
	}
	else if (move_reasons[r].type == VITAL_EYE_MOVE) {
	  int ei = eyei[move_reasons[r].what];
	  int ej = eyej[move_reasons[r].what];
	  int ecolor = eyecolor[move_reasons[r].what];
      
	  if (ecolor == WHITE) {
	    di = white_eye[ei][ej].dragoni;
	    dj = white_eye[ei][ej].dragonj;
	  }
	  else {
	    di = black_eye[ei][ej].dragoni;
	    dj = black_eye[ei][ej].dragonj;
	  }
      
	  if (di == -1) /* Maybe we should assert this not to happen. */
	    continue;

	  /* Don't care about inessential dragons. */
	  if (DRAGON2(di, dj).safety == INESSENTIAL)
	    continue;
	}      
	else
	  continue;

	if (dragon[di][dj].owl_status != CRITICAL)
	  continue;

	if ((move_reasons[r].type == STRATEGIC_ATTACK_MOVE 
	     || move_reasons[r].type == ATTACK_MOVE
	     || (move_reasons[r].type == VITAL_EYE_MOVE
		 && BOARD(di, dj) == OTHER_COLOR(color)))
	    && !move_reason_known(m, n, OWL_ATTACK_MOVE, what)
	    && owl_does_attack(m, n, di, dj)) {
	  add_owl_attack_move(m, n, di, dj);
	  TRACE("Move at %m owl attacks %m.\n", m, n, di, dj);
	}
	
	if ((move_reasons[r].type == STRATEGIC_DEFEND_MOVE
	     || move_reasons[r].type == DEFEND_MOVE
	     || (move_reasons[r].type == VITAL_EYE_MOVE
		 && BOARD(di, dj) == color))
	    && !move_reason_known(m, n, OWL_DEFEND_MOVE, what)
	    && owl_does_defend(m, n, di, dj)) {
	  add_owl_defense_move(m, n, di, dj);
	  TRACE("Move at %m owl defends %m.\n", m, n, di, dj);
	}
      }
    }
}


/*
 * It's often bad to run away with a worm that is in a strategically
 * weak position. This function gives heuristics for determining
 * whether a move at (ti, tj) to defend the worm (ai, aj) is
 * strategically sound.
 */
static int
strategically_sound_defense(int ai, int aj, int ti, int tj)
{
  UNUSED(ai); UNUSED(aj);
  return move[ti][tj].move_safety;
}



/*
 * Any move that captures or defends a worm also connects or cuts
 * the surrounding dragons. Find these secondary move reasons.
 *
 * FIXME: There is a certain amount of optimizations that could be
 *        done here.
 *
 * FIXME: Even when we defend a worm, it's possible that the
 *        opponent still can secure a connection, e.g. underneath a string
 *        with few liberties. Thus a defense move isn't necessarily a cut
 *        move.
 *
 * FIXME: Connections are transitive. If a move connects A with B and B
 *        with C, we should infer that it connects A with C as well,
 *        except when there is a shortage of liberties and C is a tail
 *        that will be lost in a correct attack.
 */

static void
induce_secondary_move_reasons(int color)
{
  int m;
  int n;
  int k;
  int i;
  int ai;
  int aj;
  int di = -1;
  int dj = -1;
  int biggest;
  
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++)
      for (k = 0; k < MAX_REASONS; k++) {
	
	int r = move[m][n].reason[k];
	if (r < 0)
	  break;
	
	if (move_reasons[r].type != ATTACK_MOVE
	    && move_reasons[r].type != DEFEND_MOVE)
	  continue;
	
	ai = I(worms[move_reasons[r].what]);
	aj = J(worms[move_reasons[r].what]);
	
	if ((   (move_reasons[r].type == ATTACK_MOVE)
		&& (BOARD(ai, aj) == color))
	    || ((move_reasons[r].type == DEFEND_MOVE)
		&& (BOARD(ai, aj) != color)))
	  continue; /* Only a move for the opponent. */
	
	if (worm[ai][aj].defend_code == 0)
	  continue; /* No defense. */

	/* Don't care about inessential dragons. */
	if (DRAGON2(ai, aj).safety == INESSENTIAL)
	  continue;
	
	/*
	 * If this is a defense move and the defense is futile for
         * strategical reasons, we shouldn't induce a cutting move
         * reason.
	 */
	if (move_reasons[r].type == DEFEND_MOVE
	    && !strategically_sound_defense(ai, aj, m, n))
	  continue;
	
	/*
	 * Find the biggest of the surrounding dragons and say that
	 * all other dragons are connected or cut with respect to that
	 * one. We might want to use some other property than size, or
	 * still better induce cuts/connections for all combinations.
	 */
	biggest = 0;
	
	/* A tactically unstable worm should never be amalgamated into
         * a larger dragon. Occasionally this does still happen and in
         * that case we need a workaround. Eventually this workaround
         * should become unnecessary.
	 */
	if (dragon[ai][aj].size == worm[ai][aj].size) {
	  for (i = 0; i < DRAGON2(ai, aj).neighbors; i++) {
	    int d = DRAGON2(ai, aj).adjacent[i];
	    if (DRAGON(d).color == dragon[ai][aj].color)
	      continue;
	    
	    if (DRAGON(d).size > biggest) {
	      di = DRAGON(d).origini;
	      dj = DRAGON(d).originj;
	      biggest = DRAGON(d).size;
	    }
	  }
	  
	  if (biggest == 0)
	    continue;
	  
	  for (i = 0; i < DRAGON2(ai, aj).neighbors; i++) {
	    int d = DRAGON2(ai, aj).adjacent[i];
	    int ei = DRAGON(d).origini;
	    int ej = DRAGON(d).originj;
	    if (DRAGON(d).color == dragon[ai][aj].color)
	      continue;
	    
	    if (di != ei || dj != ej) {
	      if (move_reasons[r].type == ATTACK_MOVE) {
		/* Exclude the case when (ai, aj) is dead and both
                 * (di, dj) and (ei, ej) are strongly alive or
                 * better. Then the move would only be losing
                 * points.
		 */
		if (dragon[ai][aj].matcher_status != DEAD
		    || (DRAGON2(di, dj).safety != STRONGLY_ALIVE
			&& DRAGON2(di, dj).safety != INVINCIBLE)
		    || (DRAGON2(ei, ej).safety != STRONGLY_ALIVE
			&& DRAGON2(ei, ej).safety != INVINCIBLE))
		  add_connection_move(m, n, di, dj, ei, ej);
	      }
	      else
		add_cut_move(m, n, di, dj, ei, ej);
	    }
	  }
	}
	else {
	  /* Workaround. If the unstable worm has been amalgamated
           * with stable worms, it would be incorrect to add
           * cut/connect move reasons for all neighbors of this
           * dragon. Instead we fall back to using chainlinks() to
           * find the neighbors of the worm. The shortcoming of this
           * is that it only counts neighbors in direct contact with
           * the worm, which is not always sufficient.
	   */
	  int adj, adjs[MAXCHAIN];
	  adj = chainlinks(POS(ai, aj), adjs);
	  for (i = 0; i < adj; i++) {
	    int adji = I(adjs[i]);
	    int adjj = J(adjs[i]);
	    if (dragon[adji][adjj].color == dragon[ai][aj].color)
	      continue;
	    if (dragon[adji][adjj].size > biggest) {
	      di = dragon[adji][adjj].origini;
	      dj = dragon[adji][adjj].originj;
	      biggest = dragon[adji][adjj].size;
	    }
	  }
	  
	  if (biggest == 0)
	    continue;
	  
	  for (i = 0; i < adj; i++) {
	    int adji = I(adjs[i]);
	    int adjj = J(adjs[i]);
	    int ei = dragon[adji][adjj].origini;
	    int ej = dragon[adji][adjj].originj;
	    if (dragon[adji][adjj].color == dragon[ai][aj].color)
	      continue;
	    
	    if (di != ei || dj != ej) {
	      if (move_reasons[r].type == ATTACK_MOVE) {
		/* Exclude the case when (ai, aj) is dead and both
                 * (di, dj) and (ei, ej) are strongly alive or
                 * better. Then the move would only be losing
                 * points.
		 */
		if (dragon[ai][aj].matcher_status != DEAD
		    || (DRAGON2(di, dj).safety != STRONGLY_ALIVE
			&& DRAGON2(di, dj).safety != INVINCIBLE)
		    || (DRAGON2(ei, ej).safety != STRONGLY_ALIVE
			&& DRAGON2(ei, ej).safety != INVINCIBLE))
		  add_connection_move(m, n, di, dj, ei, ej);
	      }
	      else
		add_cut_move(m, n, di, dj, ei, ej);
	    }
	  }
	}
      }
}


/* Examine the strategical and tactical safety of the moves. This is
 * used to decide whether or not the stone should generate influence
 * when the move is evaluated. The idea is to avoid overestimating the
 * value of strategically unsafe defense moves and connections of dead
 * dragons. This sets the move.move_safety field.
 */
static void
examine_move_safety(int color)
{
  int i, j;
  int k;
  
  start_timer(3);
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++) {
      int safety = 0;
      int tactical_safety = 0;
      
      for (k = 0; k < MAX_REASONS; k++) {
	int r = move[i][j].reason[k];
	int type;
	int what;
	if (r == -1)
	  break;
	type = move_reasons[r].type;
	what = move_reasons[r].what;
	switch (type) {
	case CUT_MOVE:
	  if (doing_scoring)
	    break;
	  /* otherwise fall through */
	case SEMEAI_MOVE:
	case ATTACK_EITHER_MOVE:
	case DEFEND_BOTH_MOVE:    /* Maybe need better check for this case. */
	case OWL_DEFEND_MOVE:
	  tactical_safety = 1;
	  safety = 1;
	  break;
	case BLOCK_TERRITORY_MOVE:
	case EXPAND_TERRITORY_MOVE:
	case EXPAND_MOYO_MOVE:
	  safety = 1;
	  break;
	case ATTACK_MOVE:
	case OWL_ATTACK_MOVE:
	  {
	    int ai = -1, aj = -1;
	    int bi = -1, bj = -1;
	    int size;
	    int our_color_neighbors;
	    int k;
	    
	    if (type == ATTACK_MOVE) {
	      ai = I(worms[what]);
	      aj = J(worms[what]);
	      size = worm[ai][aj].effective_size;
	    }
	    else {
	      ai = dragoni[what];
	      aj = dragonj[what];
	      size = dragon[ai][aj].effective_size;
	    }
	    
	    /* No worries if we catch something big. */
	    if (size >= 8) {
	      tactical_safety = 1;
	      safety = 1;
	      break;
	    }
	    
	    /* If the victim has multiple neighbor dragons of our
             * color, we leave it to the connection move reason to
             * determine safety.
	     */
	    our_color_neighbors = 0;
	    if (type == ATTACK_MOVE) {
	      /* We could use the same code as for OWL_ATTACK_MOVE
               * below if we were certain that the capturable string
               * had not been amalgamated with a living dragon.
	       */
	      int adj, adjs[MAXCHAIN];
	      adj = chainlinks(POS(ai, aj), adjs);
	      for (k = 0; k < adj; k++) {
		int adji = I(adjs[k]);
		int adjj = J(adjs[k]);
		if (BOARD(adji, adjj) == color) {
		  /* Check whether this string is part of the same
                   * dragon as an earlier string. We only want to
                   * count distinct neighbor dragons.
		   */
		  int l;
		  for (l = 0; l < k; l++)
		    if (dragon[I(adjs[l])][J(adjs[l])].id
			== dragon[adji][adjj].id)
		      break;
		  if (l == k) {
		    /* New dragon. */
		    our_color_neighbors++;
		    bi = adji;
		    bj = adjj;
		  }
		}
	      }
	    }
	    else {
	      for (k = 0; k < DRAGON2(ai, aj).neighbors; k++)
		if (DRAGON(DRAGON2(ai, aj).adjacent[k]).color == color) {
		  our_color_neighbors++;
		  bi = dragon2[DRAGON2(ai, aj).adjacent[k]].origini;
		  bj = dragon2[DRAGON2(ai, aj).adjacent[k]].originj;
		}
	    }
	    
	    if (our_color_neighbors > 1)
	      break;
	    
	    /* It may happen in certain positions that no neighbor of
             * our color is found. The working hypothesis is that
	     * the move is safe then. One example is a position like
	     *
	     * ----+
	     * OX.X|
	     * OOX.|
	     *  OOX|
	     *   OO|
	     *
	     * where the top right stone only has friendly neighbors
	     * but can be attacked.
	     *
	     * As a further improvement, we also look for a friendly
	     * dragon adjacent to the considered move.
	     */

	    for (k = 0; k < 4; k++) {
	      int di = deltai[k];
	      int dj = deltaj[k];
	      if (BOARD(i+di, j+dj) == color) {
		bi = i + di;
		bj = j + dj;
		break;
	      }
	    }
	    
	    if (bi == -1) {
	      tactical_safety = 1;
	      safety = 1;
	      break;
	    }
	    
	    /* If the attacker is thought to be alive, we trust that
             * sentiment.
	     */
	    if (dragon[bi][bj].matcher_status == ALIVE) {
	      tactical_safety = 1;
	      safety = 1;
	      break;
	    }
	    
	    /* It remains the possibility that what we have captured
             * is just a nakade shape. Ask the owl code whether this
             * move saves our attacking dragon.
	     *
	     * FIXME: Might need to involve semeai code too here.
	     */
	    if (owl_does_defend(i, j, bi, bj)) {
	      tactical_safety = 1;
	      safety = 1;
	    }
	    break;
	  }
	case DEFEND_MOVE:
	  {
	    int ai = I(worms[what]);
	    int aj = J(worms[what]);

	    if (dragon[ai][aj].matcher_status == ALIVE)
	      /* It would be better if this never happened, but it does
	       * sometimes. The owl reading can be very slow then.
	       */
	      safety = 1;
	    
	    else if (owl_does_defend(i, j, ai, aj))
	      safety = 1;
	    break;
	  }
	  
	case ATTACK_THREAT_MOVE:
	case DEFEND_THREAT_MOVE:
	  break;

	case CONNECT_MOVE:
	  {
	    int dragon1 = conn_dragon1[move_reasons[r].what];
	    int dragon2 = conn_dragon2[move_reasons[r].what];
	    int ai = dragoni[dragon1];
	    int aj = dragonj[dragon1];
	    int bi = dragoni[dragon2];
	    int bj = dragonj[dragon2];
	    if (dragon[ai][aj].owl_status == ALIVE
		|| dragon[bi][bj].owl_status == ALIVE) {
	      tactical_safety = 1;
	      safety = 1;
	    }
	    else if ((dragon[ai][aj].owl_status == UNCHECKED
		      && dragon[ai][aj].status == ALIVE)
		     || (dragon[bi][bj].owl_status == UNCHECKED
			 && dragon[bi][bj].status == ALIVE)) {
	      tactical_safety = 1;
	      safety = 1;
	    }
	    else if (owl_connection_defends(i, j, ai, aj, bi, bj)) {
	      tactical_safety = 1;
	      safety = 1;
	    }
	    break;
	  }
	}
	if (safety == 1 && (tactical_safety == 1 || safe_move2(i, j, color)))
	  break;
      }
      
      if (safety == 1 && (tactical_safety || safe_move2(i, j, color)))
	move[i][j].move_safety = 1;
      else
	move[i][j].move_safety = 0;

      time_report(3, "    examine_move_safety: ", i, j);
    }
}

static void
list_move_reasons(int color)
{
  int m;
  int n;
  int k;
  int ai = -1;
  int aj = -1;
  int bi = -1;
  int bj = -1;
  int dragon1 = -1;
  int dragon2 = -1;
  int worm1 = -1;
  int worm2 = -1;
  int ecolor = 0;
  
  gprintf("\nMove reasons:\n");
  
  for (n = 0; n < board_size; n++)
    for (m = board_size-1; m >= 0; m--) {
      for (k = 0; k < MAX_REASONS; k++) {
	
	int r = move[m][n].reason[k];
	if (r < 0)
	  break;
	
	switch(move_reasons[r].type) {
	case ATTACK_MOVE:
	case DEFEND_MOVE:
	  ai = I(worms[move_reasons[r].what]);
	  aj = J(worms[move_reasons[r].what]);
	  
	  if (move_reasons[r].type == ATTACK_MOVE
	      && BOARD(ai, aj) != color)
	    gprintf("Move at %m attacks %m%s\n", m, n, ai, aj,
		    (worm[ai][aj].defend_code == 0) ? " (defenseless)" : "");
	  else if ((move_reasons[r].type == DEFEND_MOVE
		    && BOARD(ai, aj) == color))
	    gprintf("Move at %m defends %m\n", m, n, ai, aj);
	  break;
	  
	case ATTACK_THREAT_MOVE:
	case DEFEND_THREAT_MOVE:
	  ai = I(worms[move_reasons[r].what]);
	  aj = J(worms[move_reasons[r].what]);
	  
	  if (move_reasons[r].type == ATTACK_THREAT_MOVE
	      && BOARD(ai, aj) != color)
	    gprintf("Move at %m threatens to attack %m\n", m, n, ai, aj);
	  else if ((move_reasons[r].type == DEFEND_THREAT_MOVE
		    && BOARD(ai, aj) == color))
	    gprintf("Move at %m threatens to defend %m\n", m, n, ai, aj);
	  break;

	case UNCERTAIN_OWL_DEFENSE:
	  ai = I(worms[move_reasons[r].what]);
	  aj = J(worms[move_reasons[r].what]);
	  if (BOARD(ai, aj) == color)
	    gprintf("%m found alive but not certainly, %m defends it again\n",
		    ai, aj, m, n);
	  else
	    gprintf("%m found dead but not certainly, %m attacks it again\n",
		    ai, aj, m, n);
	  break;	  

	case CONNECT_MOVE:
	case CUT_MOVE:
	  dragon1 = conn_dragon1[move_reasons[r].what];
	  dragon2 = conn_dragon2[move_reasons[r].what];
	  ai = dragoni[dragon1];
	  aj = dragonj[dragon1];
	  bi = dragoni[dragon2];
	  bj = dragonj[dragon2];
	  if (move_reasons[r].type == CONNECT_MOVE)
	    gprintf("Move at %m connects %m and %m\n", m, n, ai, aj, bi, bj);
	  else
	    gprintf("Move at %m cuts %m and %m\n", m, n, ai, aj, bi, bj);
	  break;
	  
	case ANTISUJI_MOVE:
	  gprintf("Move at %m is an antisuji\n", m, n);
	  break;
	  
	case SEMEAI_MOVE:
	  ai = dragoni[move_reasons[r].what];
	  aj = dragonj[move_reasons[r].what];
	  gprintf("Move at %m wins semeai for %m\n", m, n,
		  ai, aj);
	  break;
	  
	case SEMEAI_THREAT:
	  ai = dragoni[move_reasons[r].what];
	  aj = dragonj[move_reasons[r].what];
	  gprintf("Move at %m threatens to win semeai for %m\n", m, n,
		  ai, aj);
	  break;
	  
	case VITAL_EYE_MOVE:
	  ai = eyei[move_reasons[r].what];
	  aj = eyej[move_reasons[r].what];
	  ecolor = eyecolor[move_reasons[r].what];
	  if (ecolor == WHITE)
	    gprintf("Move at %m vital eye point for dragon %m (eye %m)\n",
		    m, n, white_eye[ai][aj].dragoni,
		    white_eye[ai][aj].dragonj, ai, aj);
	  else
	    gprintf("Move at %m vital eye point for dragon %m (eye %m)\n",
		    m, n, black_eye[ai][aj].dragoni,
		    black_eye[ai][aj].dragonj, ai, aj);
	  break;
	  
	case NON_ATTACK_MOVE:
	case NON_DEFEND_MOVE:
	  ai = I(worms[move_reasons[r].what]);
	  aj = J(worms[move_reasons[r].what]);
	  
	  if (move_reasons[r].type == NON_ATTACK_MOVE
	      && BOARD(ai, aj) != color)
	    gprintf("Move at %m does in fact not attack %m\n", m, n, ai, aj);
	  else if ((move_reasons[r].type == NON_DEFEND_MOVE
		    && BOARD(ai, aj) == color))
	    gprintf("Move at %m does in fact not defend %m\n", m, n, ai, aj);
	  break;
	  
	case ATTACK_EITHER_MOVE:
	case DEFEND_BOTH_MOVE:
	  worm1 = worm_pair1[move_reasons[r].what];
	  worm2 = worm_pair2[move_reasons[r].what];
	  ai = I(worms[worm1]);
	  aj = J(worms[worm1]);
	  bi = I(worms[worm2]);
	  bj = J(worms[worm2]);
	  
	  if (move_reasons[r].type == ATTACK_EITHER_MOVE)
	    gprintf("Move at %m attacks either %m or %m\n",
		    m, n, ai, aj, bi, bj);
	  else
	    gprintf("Move at %m defends both %m and %m\n",
		    m, n, ai, aj, bi, bj);
	  break;
	  	
	case OWL_ATTACK_MOVE:
	  ai = dragoni[move_reasons[r].what];
	  aj = dragonj[move_reasons[r].what];
	  gprintf("Move at %m owl-attacks %m\n", m, n, ai, aj);
	  break;
	  
	case OWL_DEFEND_MOVE:
	  ai = dragoni[move_reasons[r].what];
	  aj = dragonj[move_reasons[r].what];
	  gprintf("Move at %m owl-defends %m\n", m, n, ai, aj);
	  break;
	  
	case OWL_ATTACK_THREAT:
	  ai = dragoni[move_reasons[r].what];
	  aj = dragonj[move_reasons[r].what];
	  gprintf("Move at %m owl-threatens to attack %m\n", m, n, ai, aj);
	  break;
	  
	case OWL_DEFENSE_THREAT:
	  ai = dragoni[move_reasons[r].what];
	  aj = dragonj[move_reasons[r].what];
	  gprintf("Move at %m owl-threatens to defend %m\n", m, n, ai, aj);
	  break;
	  
	case OWL_PREVENT_THREAT:
	  ai = dragoni[move_reasons[r].what];
	  aj = dragonj[move_reasons[r].what];
	  gprintf("Move at %m owl-prevents a threat to attack or defend %m\n", 
		  m, n, ai, aj);
	  break;

	case BLOCK_TERRITORY_MOVE:
	  gprintf("Move at %m blocks territory\n", m, n);
	  break;
	  
	case EXPAND_TERRITORY_MOVE:
	  gprintf("Move at %m expands territory\n", m, n);
	  break;
	  
	case EXPAND_MOYO_MOVE:
	  gprintf("Move at %m expands moyo\n", m, n);
	  break;
	  
	case STRATEGIC_ATTACK_MOVE:
	case STRATEGIC_DEFEND_MOVE:
	  ai = dragoni[move_reasons[r].what];
	  aj = dragonj[move_reasons[r].what];
	  
	  if (move_reasons[r].type == STRATEGIC_ATTACK_MOVE)
	    gprintf("Move at %m strategically attacks %m\n", m, n, ai, aj);
	  else
	    gprintf("Move at %m strategically defends %m\n", m, n, ai, aj);
	  break;
	  
	case MY_ATARI_ATARI_MOVE:
	  gprintf("Move at %m captures something\n", m, n);

	case YOUR_ATARI_ATARI_MOVE:
	  gprintf("Move at %m defends threat to capture something\n", m, n);
	}
      }
      if (k > 0 && move[m][n].move_safety == 0)
	gprintf("Move at %m strategically or tactically unsafe\n", m, n);
    }
}


/* Find the stones which are tactically defended by the move by color
 * at (i, j). This is used to stop opponent influence from passing
 * through strings that we have saved. However, the saved strings do
 * not generate influence by themselves, because that would upset the
 * current evaluation of saved strings.
 *
 * Notice: Contrary to the name, saved_stones[][] now also records
 * captured stones.
 *
 * FIXME: Probably should include DEFEND_BOTH defenses too.
 */
void
find_stones_saved_by_move(int m, int n, int color,
			  char saved_stones[MAX_BOARD][MAX_BOARD])
{
  int k;
  
  memset(saved_stones, 0, MAX_BOARD * MAX_BOARD);
  for (k = 0; k < MAX_REASONS; k++) {
    int r = move[m][n].reason[k];
    if (r < 0)
      break;

    if (move_reasons[r].type == DEFEND_MOVE) {
      int ai = I(worms[move_reasons[r].what]);
      int aj = J(worms[move_reasons[r].what]);
      
      /* Defense of enemy stones. */
      if (BOARD(ai, aj) != color)
	continue;

      mark_string2(ai, aj, saved_stones, 1);
    }
    else if (move_reasons[r].type == ATTACK_MOVE) {
      int ai = I(worms[move_reasons[r].what]);
      int aj = J(worms[move_reasons[r].what]);
      
      /* Attack on our stones. */
      if (BOARD(ai, aj) == color)
	continue;

      mark_string2(ai, aj, saved_stones, 1);
    }
  }
}

/*
 * An attempt to estimate the safety of a dragon.
 *
 * FIXME: Important to test more exactly how effective a strategical
 *        attack or defense of a weak dragon is. This can be done by
 *        measuring escape factor and moyo size after the move and
 *        compare with the old values. Also necessary to test whether
 *        an attack or defense of a critical dragon is effective.
 *        Notice that this wouldn't exactly go into this function but
 *        rather where it's called.
 */

static float safety_values[10] = {
/* DEAD        */  0.0,
/* ALIVE       */  0.9,
/* CRITICAL    */  0.1,
/* INESSENTIAL */  1.0,   /* Yes, 1.0. We simply don't worry about it. */
/* TACT. DEAD  */  0.0,
/* WEAK        */  0.4,
/* WEAK ALIVE  */  0.6,
/* SEKI        */  0.8,
/* STR. ALIVE  */  1.0,
/* INVINCIBLE  */  1.0};
		  
static float
dragon_safety(int ai, int aj, int ignore_dead_dragons)
{
  int dragon_safety = DRAGON2(ai, aj).safety;

  /* Kludge: If a dragon is dead, we return 1.0 in order not
   * to try to run away.
   */
  if (ignore_dead_dragons
      && (dragon_safety == DEAD
	  || dragon_safety == INESSENTIAL
	  || dragon_safety == TACTICALLY_DEAD))
    return 1.0;

  /* More detailed guesses for WEAK and WEAKLY_ALIVE dragons. */
  if (dragon_safety == WEAK || dragon_safety == WEAKLY_ALIVE) {
    int escape = dragon[ai][aj].escape_route;
    int moyo = DRAGON2(ai, aj).moyo;
    /* If escape <= 5 and moyo <= 10, the dragon won't be WEAK, since
     * the owl code has been run.
     */
    if (escape < 10 && moyo < 5)
      return 0.1;
    else if (escape < 15 && moyo < 5)
      return 0.2;
    else if (escape < 10 && moyo < 10)
      return 0.3;
    else if (escape < 5 && moyo < 15)
      return 0.4;
    else if (escape < 15 && moyo < 15)
      return 0.7;
    else
      return 0.9;
  }
  
  return safety_values[dragon_safety];
}

/*
 * Strategical value of connecting (or cutting) the dragon at (ai, aj)
 * to the dragon at (bi, bj). Notice that this function is assymetric.
 * This is because connection_value(a, b) is intended to measure the
 * strategical value on the a dragon from a connection to the b dragon.
 * 
 * Consider the following position:
 * +---------+
 * |XXO.O.OXX|
 * |.XOOOOOX.|
 * |XXXX.XXXX|
 * |.XOOXOOX.|
 * |XXO.X.O.X|
 * |OOOXXXOOO|
 * |..OOOOO..|
 * |.........|
 * +---------+
 * 
 * X has three dragons, one invincible to the left (A), one critical to
 * the right (B), and one dead in the center (C). The move at the cutting
 * point has three move reasons:
 * connect A and B
 * connect A and C
 * connect B and C
 * 
 * The strategical value on A of either connection is of course zero,
 * since it's very unconditionally alive. The strategical value on B is
 * high when it's connected to A but small (at least should be) from the
 * connection to C. Similarly for dragon C. In effect the total
 * strategical value of this move is computed as:
 * 
 * max(connection_value(A, B), connection_value(A, C))
 * + max(connection_value(B, A), connection_value(B, C))
 * + max(connection_value(C, A), connection_value(C, B))
 *
 * The parameter 'margin' is the margin by which we are ahead.
 * If this exceeds 20 points we use the cautious impact values,
 * which value connections more.  This is because we can afford
 * to waste a move making sure of safety. If the margin is between
 * 0 and 20 points we interpret linearly between the two sets of
 * impact values.
 */

/* Values higher than 1.0 to give connections a bonus over other vital
 * moves.
 */
static float impact_values[10][10] = {
/*        (bi, bj) DEAD ALIV CRIT INES TACT WEAK WE_A SEKI STRO INVI */
/* DEAD        */ {0.0, 0.9, 0.0, 0.0, 0.0, 0.8, 0.85,0.8, 0.95,1.0 },
/* ALIVE       */ {0.0, 0.08,0.05,0.0, 0.0, 0.05,0.07,0.05,0.09,0.1 },
/* CRITICAL    */ {0.0, 1.04,0.85,0.0, 0.0, 0.75,0.9, 0.85,1.08,1.1 },
/* INESSENTIAL */ {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 },
/* TACT. DEAD  */ {0.0, 0.9, 0.0, 0.0, 0.0, 0.8, 0.85,0.8, 0.95,1.0 },
/* WEAK        */ {0.1, 0.6, 0.25,0.0, 0.0, 0.2, 0.25,0.25,0.65,0.65},
/* WEAK ALIVE  */ {0.0, 0.4, 0.3, 0.0, 0.0, 0.15,0.2, 0.2 ,0.45,0.45},
/* SEKI        */ {0.0, 0.2, 0.15,0.0, 0.0, 0.1, 0.15,0.2, 0.25,0.3 },
/* STR. ALIVE  */ {0.0, 0.02,0.01,0.0, 0.0, 0.01,0.01,0.01,0.03,0.03},
/* INVINCIBLE  */ {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 }};
/* (ai, aj)    */
		  
static float cautious_impact_values[10][10] = {
/*        (bi, bj) DEAD ALIV CRIT INES TACT WEAK WE_A SEKI STRO INVI */
/* DEAD        */ {0.3, 0.9, 0.0, 0.0, 0.0, 0.8, 0.85,0.8, 0.95,1.0 },
/* ALIVE       */ {0.0, 0.2, 0.05,0.2, 0.0, 0.1,0.15,0.10, 0.2 ,0.2 },
/* CRITICAL    */ {0.0, 1.04,0.85,0.0, 0.0, 0.75,0.9, 0.85,1.08,1.1 },
/* INESSENTIAL */ {0.1, 0.6, 0.0, 0.4, 0.0, 0.3, 0.5, 0.5, 0.6, 0.6 },
/* TACT. DEAD  */ {0.2, 0.9, 0.0, 0.0, 0.0, 0.8, 0.85,0.8, 0.95,1.0 },
/* WEAK        */ {0.1, 0.6, 0.25,0.0, 0.0, 0.2, 0.25,0.25,0.65,0.65},
/* WEAK ALIVE  */ {0.0, 0.4, 0.3, 0.0, 0.0, 0.2,0.2, 0.2 ,0.45,0.45},
/* SEKI        */ {0.0, 0.2, 0.15,0.0, 0.0, 0.1, 0.15,0.2, 0.25,0.3 },
/* STR. ALIVE  */ {0.0, 0.02,0.01,0.0, 0.0, 0.01,0.01,0.01,0.03,0.03},
/* INVINCIBLE  */ {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 }};
/* (ai, aj)    */
		  
static float
connection_value(int ai, int aj, int bi, int bj, int ti, int tj, float margin)
{
  int safety1 = DRAGON2(ai, aj).safety;
  int safety2 = DRAGON2(bi, bj).safety;
  int true_genus1 = 2 * dragon[ai][aj].genus + dragon[ai][aj].heyes;
  int true_genus2 = 2 * dragon[bi][bj].genus + dragon[bi][bj].heyes;
  float impact;

  /* If the connected dragon gets sufficient eyespace to live on its
   * own, although neither of the unconnected ones did, we simulate
   * this by upgrading the safety of the second dragon to ALIVE.
   */
  if (true_genus1 < 4 && true_genus2 < 4) {
    if (true_genus1 + true_genus2 >= 4
	||  (true_genus1 + true_genus2 >= 3
	     && ((dragon[ai][aj].heyei == ti
		  && dragon[ai][aj].heyej == tj)
		 || (dragon[bi][bj].heyei == ti
		     && dragon[bi][bj].heyej == tj))))
      safety2 = ALIVE;
  }

  /* If the b dragon is critical but has genus 0 and no moyo, we
   * assume it doesn't help dragon a to connect to b.
   */
  if (safety2 == CRITICAL && true_genus2 == 0 && DRAGON2(bi, bj).moyo == 0)
    return 0.0;
  

  /* When scoring, we want to be restrictive with reinforcement moves
   * inside own territory. Thus if both dragons are strongly alive or
   * invincible, or if one is and the other is alive, no bonus is
   * awarded.
   *
   * Notice that this requires that the territorial value is computed
   * before the strategical value.
   */
  if (doing_scoring && move[ti][tj].territorial_value < 0.0) {
    if (safety1 == ALIVE
	&& (safety2 == STRONGLY_ALIVE || safety2 == INVINCIBLE))
      return 0.0;
    
    if ((safety1 == STRONGLY_ALIVE || safety1 == INVINCIBLE)
	&& (safety2 == ALIVE || safety2 == STRONGLY_ALIVE
	    || safety2 == INVINCIBLE))
      return 0.0;
  }

  if (doing_scoring || margin < 0.0)
    impact = impact_values[safety1][safety2];
  else if (margin > 20.0)
    impact = cautious_impact_values[safety1][safety2];
  else impact = 0.05*margin*cautious_impact_values[safety1][safety2]
	 + (1-0.05*margin)*impact_values[safety1][safety2];
  
  return impact * 2.0 * dragon[ai][aj].effective_size;
}

/*
 * Estimate the direct territorial value of a move at (m,n).
 */
static void
estimate_territorial_value(int m, int n, int color,
			   char saved_stones[MAX_BOARD][MAX_BOARD],
			   float score)
{
  int k;
  int ai = -1;
  int aj = -1;
#if 0
  int bi = -1;
  int bj = -1;
  int ecolor = 0;
#endif
  
  float this_value = 0.0;
  float tot_value = 0.0;
  float secondary_value = 0.0;

  int does_block = 0;
  int does_expand = 0;
  
  for (k = 0; k < MAX_REASONS; k++) {
    int r = move[m][n].reason[k];
    if (r < 0)
      break;

    this_value = 0.0;
    switch (move_reasons[r].type) {
    case ATTACK_MOVE:
      /* FIXME: Need to take ko attacks properly into account. */
      ai = I(worms[move_reasons[r].what]);
      aj = J(worms[move_reasons[r].what]);
      
      /* Attack on our stones. */
      if (BOARD(ai, aj) == color)
	break;
      
      /* Defenseless stone. */
      if (worm[ai][aj].defend_code == 0) {
	DEBUG(DEBUG_MOVE_REASONS,
	      "  %m: %f (secondary) - attack on %m (defenseless)\n",
	      m, n, worm[ai][aj].size, ai, aj);
	secondary_value += worm[ai][aj].size;
	break;
      }

      /* Strategically unsafe move. */
      if (!move[m][n].move_safety) {
	DEBUG(DEBUG_MOVE_REASONS,
	      "  %m: 0.0 - attack on %m (unsafe move)\n",
	      m, n, ai, aj);
	break;
      }

      this_value = 2 * worm[ai][aj].effective_size;

      /* If the stones are dead, there is only a secondary value in
       * capturing them tactically as well.
       */
      if (dragon[ai][aj].matcher_status == DEAD) {
	DEBUG(DEBUG_MOVE_REASONS,
	      "  %m: %f (secondary) - attack on %m (dead)\n",
	      m, n, 0.2 * this_value, ai, aj);
	secondary_value += 0.2 * this_value;
	break;
      }
      
      /* If the move also owl attacks the same stones, count points
       * for that move reason instead.
       */
      if (move_reason_known(m, n, OWL_ATTACK_MOVE, find_dragon(ai, aj))) {
	DEBUG(DEBUG_MOVE_REASONS,
	      "  %m: 0.0 - attack on %m (owl attacked as well)\n",
	      m, n, ai, aj);
	break;
      }
      
      TRACE("  %m: %f - attack on worm %m\n",
	    m, n, this_value, ai, aj);
      
      tot_value += this_value;
      break;
      
    case DEFEND_MOVE:
      /* FIXME: Need to take ko defense properly into account. */
      ai = I(worms[move_reasons[r].what]);
      aj = J(worms[move_reasons[r].what]);
      
      /* Defense of enemy stones. */
      if (BOARD(ai, aj) != color)
	break;
      
      /* 
       * Estimate value 
       */
      if (!strategically_sound_defense(ai, aj, m, n)) {
	DEBUG(DEBUG_MOVE_REASONS,
	      "  %m: 0.0 - defense of %m (strategically unsound defense)\n",
	      m, n, ai, aj);
	break;
      }	

      this_value = 2 * worm[ai][aj].effective_size;

      /* If the stones are dead, we use the convention that
       * defending them has a strategical value rather than
       * territorial. Admittedly this make more sense for attacks on
       * dead stones.
       */
      if (dragon[ai][aj].matcher_status == DEAD) {
	DEBUG(DEBUG_MOVE_REASONS,
	      "  %m: %f (secondary) - defense of %m (dead)\n",
	      m, n, 0.2 * this_value, ai, aj);
	secondary_value += 0.2 * this_value;
	break;
      }

      /* If the stones are inessential, there is no value in saving them. */
      if (worm[ai][aj].inessential) {
	DEBUG(DEBUG_MOVE_REASONS,
	      "  %m: 0.0 - defense of %m (inessential)\n",
	      m, n, ai, aj);
	break;
      }
      
      /* If the move also owl defends the same stones, count points
       * for that move reason instead.
       */
      if (move_reason_known(m, n, OWL_DEFEND_MOVE, find_dragon(ai, aj))) {
	DEBUG(DEBUG_MOVE_REASONS,
	      "  %m: 0.0 - defense of %m (owl defended as well)\n",
	      m, n, ai, aj);
	break;
      }
      
      TRACE("  %m: %f - defense of worm %m\n",
	    m, n, this_value, ai, aj);
    
      tot_value += this_value;
      break;

    case ATTACK_THREAT_MOVE:
      ai = I(worms[move_reasons[r].what]);
      aj = J(worms[move_reasons[r].what]);

      /* Threat on our stones. */
      if (BOARD(ai, aj) == color)
	break;
      
      if (dragon[ai][aj].matcher_status == DEAD) {
	DEBUG(DEBUG_MOVE_REASONS,
	      "  %m: 0.0 - threatens to capture %m (dead)\n",
	      m, n, ai, aj);
	break;
      }

      /* If the move also owl attacks the same stones, there is 
       * no use to threaten tactically.
       */
      if (move_reason_known(m, n, OWL_ATTACK_MOVE, find_dragon(ai, aj))) {
	DEBUG(DEBUG_MOVE_REASONS,
	      "  %m: 0.0 - threaten to capture %m (owl attacked as well)\n",
	      m, n, ai, aj);
	break;
      }
      
      /* The followup value of a move threatening to attack (ai,aj)
       * is twice its effective size, with adjustments. If the
       * worm has an adjacent (friendly) dead dragon we add its
       * value. On the other hand if it has an adjacent critical
       * worm, and if (m,n) does not defend that worm, we subtract
       * the value of the worm, since (ai,aj) may be defended by
       * attacking that worm. We make at most one adjustment
       * of each type.
       */	 

      if (trymove2(m, n, color, "estimate_territorial_value",
		   -1, -1, EMPTY, -1, -1)) {
	int adjs[MAXCHAIN];
	int adjusted_value = 2 * worm[ai][aj].effective_size;
	int adjustment_up = 0;
	int adjustment_down = 0;
	int s;
	int adj = chainlinks(POS(ai, aj), adjs);

	for (s = 0; s < adj; s++) {
	  int adji = I(adjs[s]);
	  int adjj = J(adjs[s]);
	  if (same_string2(m, n, adji, adjj))
	    continue;
	  if (dragon[adji][adjj].color == color
	      && dragon[adji][adjj].matcher_status == DEAD
	      && 2*dragon[adji][adjj].effective_size > adjustment_up)
	    adjustment_up = 2*dragon[adji][adjj].effective_size;
	  if (dragon[adji][adjj].color == color
	      && attack(adji, adjj, NULL, NULL)
	      && 2*worm[adji][adjj].effective_size > adjustment_down)
	    adjustment_down = 2*worm[adji][adjj].effective_size;
	}
	adjusted_value += adjustment_up;
	adjusted_value -= adjustment_down;
	if (adjusted_value > 0.) {
	  add_followup_value(m, n, 2 * worm[ai][aj].effective_size);
	  TRACE("  %m: %f (followup) - threatens to capture %m\n",
		m, n, 2 * worm[ai][aj].effective_size, ai, aj);
	}
	popgo();
      }
      break;

    case DEFEND_THREAT_MOVE:
      ai = I(worms[move_reasons[r].what]);
      aj = J(worms[move_reasons[r].what]);

      /* Threat on our stones. */
      if (BOARD(ai, aj) == color)
	break;
      
      if (dragon[ai][aj].matcher_status == DEAD) {
	DEBUG(DEBUG_MOVE_REASONS,
	      "  %m: 0.0 - threatens to defend %m (dead)\n",
	      m, n, ai, aj);
	break;
      }

      /* If the stones are inessential, there is no value in saving them. */
      if (worm[ai][aj].inessential) {
	DEBUG(DEBUG_MOVE_REASONS,
	      "  %m: 0.0 - threaten to defend %m (inessential)\n",
	      m, n, ai, aj);
	break;
      }
      
      /* If the move also owl defends the same stones, there is no
       * use in saving them
       */
      if (move_reason_known(m, n, OWL_DEFEND_MOVE, find_dragon(ai, aj))) {
	DEBUG(DEBUG_MOVE_REASONS,
	      "  %m: 0.0 - threaten to defend of %m (owl defended as well)\n",
	      m, n, ai, aj);
	break;
      }
      
      add_followup_value(m, n, 2 * worm[ai][aj].effective_size);

      TRACE("  %m: %f (followup) - threatens to defend %m\n",
	    m, n, 2 * worm[ai][aj].effective_size, ai, aj);

      break;

    case UNCERTAIN_OWL_DEFENSE:
      
      ai = dragoni[move_reasons[r].what];
      aj = dragonj[move_reasons[r].what];
      
      if (DRAGON2(ai, aj).safety == INESSENTIAL) {
	DEBUG(DEBUG_MOVE_REASONS,
	      "  %m: 0 - owl attack/defend for inessential dragon %m\n",
	      m, n, ai, aj);
	break;
      }

      /* If the dragon is a single ko stone we don't award any
       * territorial value for it. This isn't exactly right but a
       * reasonable workaround to avoid or at least limit overvaluation.
       */
      if (dragon[ai][aj].size == 1 && worm[ai][aj].ko == 1) {
	DEBUG(DEBUG_MOVE_REASONS,
	      "  %m: 0 - owl attack/defend for ko stone (workaround) %m\n",
	      m, n, ai, aj);
	break;
      }

      /* If there is an adjacent dragon which is critical we should
       * skip this type of move reason, since attacking or defending
       * the critical dragon is more urgent.
       */

      {
	int d;
	int found_one = 0;

	for (d = 0; d < DRAGON2(ai, aj).neighbors; d++)
	  if (DRAGON(DRAGON2(ai, aj).adjacent[d]).matcher_status == CRITICAL)
	    found_one = 1;
	if (found_one)
	  break;
      }

      /* If we are behind, we should skip this type of move reason. 
       * If we are ahead, we should value it more.  */

      if ((color == BLACK && score > 0.)
	  || (color == WHITE && score < 0.))
	this_value = 0.;
      else 
	this_value = gg_min(2*dragon[ai][aj].effective_size, gg_abs(score/2));

      TRACE("  %m: %f - owl uncertain attack/defend for %m\n",
	      m, n, this_value, ai, aj);
      tot_value += this_value;
      does_block = 1;
      break;
      
    case CONNECT_MOVE:
    case CUT_MOVE:
    case STRATEGIC_ATTACK_MOVE:
    case STRATEGIC_DEFEND_MOVE:
    case BLOCK_TERRITORY_MOVE:
      does_block = 1;
      break;
      
    case EXPAND_MOYO_MOVE:
    case EXPAND_TERRITORY_MOVE:
      does_expand = 1;
      break;
      
    case SEMEAI_MOVE:
      /* If this move also owl attacks or defends the same dragon, we
       * count the points for that move reason instead.
       */
      if (move_reason_known(m, n, OWL_ATTACK_MOVE, move_reasons[r].what)
	  || move_reason_known(m, n, OWL_DEFEND_MOVE, move_reasons[r].what))
	break;

      ai = dragoni[move_reasons[r].what];
      aj = dragonj[move_reasons[r].what];
      
      /* If the dragon consists of a single worm, and this move
       * tactically attacks or defends that worm, count the points for
       * that move reason instead.
       */
      if (worm[ai][aj].size == dragon[ai][aj].size
	  && (move_reason_known(m, n, ATTACK_MOVE, find_worm(POS(ai, aj)))
	      || move_reason_known(m, n, DEFEND_MOVE, find_worm(POS(ai, aj)))))
	break;
      
      this_value = 2 * dragon[ai][aj].effective_size;
      TRACE("  %m: %f - semeai involving %m\n",
	    m, n, this_value, ai, aj);
      tot_value += this_value;
      break;
      
    case SEMEAI_THREAT:
      /* If this move also owl attacks or defends the same dragon, we
       * count the points for that move reason instead.
       */
      if (move_reason_known(m, n, OWL_ATTACK_MOVE, move_reasons[r].what)
	  || move_reason_known(m, n, OWL_DEFEND_MOVE, move_reasons[r].what))
	break;
      
      ai = dragoni[move_reasons[r].what];
      aj = dragonj[move_reasons[r].what];

      /* If the dragon consists of a single worm, and this move
       * tactically attacks or defends that worm, count the points for
       * that move reason instead.
       */
      if (worm[ai][aj].size == dragon[ai][aj].size
	  && (move_reason_known(m, n, ATTACK_MOVE, find_worm(POS(ai, aj)))
	      || move_reason_known(m, n, DEFEND_MOVE, find_worm(POS(ai, aj)))))
	break;
      
      /* threaten to win the semeai as a ko threat */
      add_followup_value(m, n, 2 * dragon[ai][aj].effective_size);
      TRACE("  %m: %f (followup) - threatens to win semeai for %m\n",
	    m, n, 2 * dragon[ai][aj].effective_size, ai, aj);

      break;
      
    case VITAL_EYE_MOVE:
      /* These are upgraded to owl attacks or defenses in
       * find_more_owl_attack_and_defense_moves() and should no longer
       * be counted here.
       */
#if 0
      ai = eyei[move_reasons[r].what];
      aj = eyej[move_reasons[r].what];
      ecolor = eyecolor[move_reasons[r].what];
      
      if (ecolor == WHITE) {
	bi = white_eye[ai][aj].dragoni;
	bj = white_eye[ai][aj].dragonj;
      }
      else {
	bi = black_eye[ai][aj].dragoni;
	bj = black_eye[ai][aj].dragonj;
      }
      
      if (bi == -1) /* Maybe we should assert this not to happen. */
	break; 

      /* Don't care about inessential dragons. */
      if (DRAGON2(bi, bj).safety == INESSENTIAL)
	break;
      
      /* If there is an owl attack/defend move reason for this location,
       * we don't care about it, since otherwise we would count the
       * points twice.
       */
      if (move_reason_known(m, n, OWL_DEFEND_MOVE, find_dragon(bi, bj))
	  || move_reason_known(m, n, OWL_ATTACK_MOVE, find_dragon(bi, bj))) {
	DEBUG(DEBUG_MOVE_REASONS,
	      "  %m: 0.0 - vital for %m: owl attack/defense as well\n",
	      m, n, bi, bj);
	break;
      }

      /* FIXME: These are ad hoc and need improvement. */
      /* It is not sufficient to test for status==CRITICAL, need
       * to count the eyes as well.
       */
      {
	int save_verbose = verbose;
	if (verbose > 0)
	  verbose --;

	if (2 * dragon[bi][bj].genus + dragon[bi][bj].heyes == 3
	    && (dragon[bi][bj].owl_status == CRITICAL
		&& ((BOARD(bi, bj) == color
		     && owl_does_defend(m, n, bi, bj))
		    || (BOARD(bi, bj) == OTHER_COLOR(color)
			&& owl_does_attack(m, n, bi, bj))))) {
	  verbose = save_verbose;
	  this_value = 2 * dragon[bi][bj].effective_size;
	  TRACE("  %m: %f - vital for %m\n",
		m, n, this_value, bi, bj);
	  tot_value += this_value;
	}
	else {
	  verbose = save_verbose;
	  if (2*dragon[bi][bj].genus + dragon[bi][bj].heyes < 6)
	    secondary_value += dragon[bi][bj].effective_size / 4;
	  else
	    secondary_value += dragon[bi][bj].effective_size / 20;
	}
      }
#endif
      break;
	
    case OWL_ATTACK_MOVE:
    case OWL_DEFEND_MOVE:
      ai = dragoni[move_reasons[r].what];
      aj = dragonj[move_reasons[r].what];
      
      if (DRAGON2(ai, aj).safety == INESSENTIAL) {
	DEBUG(DEBUG_MOVE_REASONS,
	      "  %m: 0 - owl attack/defend for inessential dragon %m\n",
	      m, n, ai, aj);
	break;
      }

      /* If the dragon is a single ko stone we don't award any
       * territorial value for it. This isn't exactly right but a
       * reasonable workaround to avoid or at least limit overvaluation.
       */
      if (dragon[ai][aj].size == 1 && worm[ai][aj].ko == 1) {
	DEBUG(DEBUG_MOVE_REASONS,
	      "  %m: 0 - owl attack/defend for ko stone (workaround) %m\n",
	      m, n, ai, aj);
	break;
      }

      this_value = 2 * dragon[ai][aj].effective_size;
      TRACE("  %m: %f - owl attack/defend for %m\n",
	      m, n, this_value, ai, aj);
      tot_value += this_value;
      does_block = 1;
      break;

    case OWL_ATTACK_THREAT:
      ai = dragoni[move_reasons[r].what];
      aj = dragonj[move_reasons[r].what];
      add_followup_value(m, n, 2 * dragon[ai][aj].effective_size);
      TRACE("  %m: %f (followup) - threatens to owl attack %m\n",
	    m, n, 2 * dragon[ai][aj].effective_size, ai, aj);
      break;
      
    case OWL_DEFENSE_THREAT:
      ai = dragoni[move_reasons[r].what];
      aj = dragonj[move_reasons[r].what];
      add_followup_value(m, n, 2 * dragon[ai][aj].effective_size);
      TRACE("  %m: %f (followup) - threatens to owl defend %m\n",
	    m, n, 2 * dragon[ai][aj].effective_size, ai, aj);
      break;
      
    case OWL_PREVENT_THREAT:
      /* A move attacking a dragon whose defense can be threatened.
       */
      ai = dragoni[move_reasons[r].what];
      aj = dragonj[move_reasons[r].what];

      /* If the opponent just added a stone to a dead dragon, then
       * attack it. If we are ahead, add a safety move here, at most
       * half the margin of victory.
       *
       * This does not apply if we are doing scoring.
       */

      if (!doing_scoring
	  && same_dragon(I(last_moves[0]), J(last_moves[0]), ai, aj)) {
	this_value = 1.5 * dragon[ai][aj].effective_size;
	TRACE("  %m: %f - attack last move played, although it seems dead\n",
	      m, n, this_value);
	tot_value += this_value;
      }
      else if (!doing_scoring && ((color == BLACK && score < -0.)
				  || (color == WHITE && score > 0.))) {
	this_value = gg_min(1.5 * dragon[ai][aj].effective_size,
			    gg_abs(score/2));
	TRACE("  %m: %f - attack last move played, although it seems dead\n",
	      m, n, this_value);
	tot_value += this_value;
      }
      else {
	/* FIXME: Why are we computing a this_value here when it's
         * never used?
	 */
	if ((color == BLACK && score > 0.)
	    || (color == WHITE && score < 0.))
	  this_value = 0.;
	else 
	  this_value = gg_min(2*dragon[ai][aj].effective_size, gg_abs(score/2));
	
	add_reverse_followup_value(m, n, 2 * dragon[ai][aj].effective_size);
	if (BOARD(ai, aj) == color)
	  TRACE("  %m: %f (reverse followup) - prevent threat to attack %m\n",
		m, n, 2 * dragon[ai][aj].effective_size, ai, aj);
	else 
	  TRACE("  %m: %f (reverse followup) - prevent threat to defend %m\n",
		m, n, 2 * dragon[ai][aj].effective_size, ai, aj);
      }
      break;
      
    case MY_ATARI_ATARI_MOVE:
      this_value = 2 * move_reasons[r].what+3.;
      if (influence_territory_color(m, n) == OTHER_COLOR(color))
	does_expand = 1;
      tot_value += this_value;
      TRACE("  %m: %f - combination attack kills one of several worms\n",
	    m, n, this_value);
      break;
      
    case YOUR_ATARI_ATARI_MOVE:
      this_value = 2 * move_reasons[r].what+3.;
      if (influence_territory_color(m, n) == color)
	this_value += 7.;
      tot_value += this_value;
      TRACE("  %m: %f - defends against combination attack on several worms\n",
	    m, n, this_value);
      break;
    }
  }

  /* Currently no difference in the valuation between blocking and
   * expanding moves.
   */
  this_value = 0.0;
  if ((does_block || does_expand) && move[m][n].move_safety == 1) {
    this_value = influence_delta_territory(m, n, color, saved_stones);
    if (this_value != 0.0)
      TRACE("  %m: %f - block or expand territory\n", m, n, this_value);
    else
      DEBUG(DEBUG_MOVE_REASONS, "  %m: 0.0 - block or expand territory\n", m, n);
  }

  /* Test if min_territory or max_territory values constrain the
   * delta_territory value.
   */
  if (this_value < move[m][n].min_territory && move[m][n].min_territory > 0) {
    this_value = move[m][n].min_territory;
    TRACE("  %m:   %f - revised to meet minimum territory value\n",
	  m, n, this_value);
  }
  if (this_value > move[m][n].max_territory) {
    this_value = move[m][n].max_territory;
    TRACE("  %m:   %f - revised to meet maximum territory value\n",
	  m, n, this_value);
  }
    
  tot_value += this_value;
  /* subtract one point for a sacrifice (playing in opponent's territory) */
  if (tot_value > 1.0 && !safe_move2(m, n, color))
    tot_value -= 1;

  move[m][n].territorial_value = tot_value;
  move[m][n].secondary_value += secondary_value;
}


/*
 * Estimate the influence value of a move at (m,n).
 *
 * FIXME: This is much too simplified.
 */
static void
estimate_influence_value(int m, int n, int color,
			 char saved_stones[MAX_BOARD][MAX_BOARD])
{
  int k;
  int does_expand = 0;

  /* If the influence_value is non-zero, we have already computed it
   * once and don't have to redo it. This may happen when move values
   * are reevaluated after looking for endgame moves or after
   * revise_semeai().
   */
  if (move[m][n].influence_value == 0.0) {
    for (k = 0; k < MAX_REASONS; k++) {
      int r = move[m][n].reason[k];
      if (r < 0)
	break;
      
      switch (move_reasons[r].type) {
      case CONNECT_MOVE:
      case CUT_MOVE:
      case BLOCK_TERRITORY_MOVE:
      case EXPAND_TERRITORY_MOVE:
      case STRATEGIC_ATTACK_MOVE:
      case STRATEGIC_DEFEND_MOVE:
      case EXPAND_MOYO_MOVE:
      case OWL_ATTACK_MOVE:
      case OWL_DEFEND_MOVE:
      case SEMEAI_MOVE:
	does_expand = 1;
	break;
      }
    }
    
    if (!does_expand || move[m][n].move_safety == 0)
      return;
    
    move[m][n].influence_value =
      0.35 * influence_delta_strict_moyo(m, n, color, saved_stones);
    move[m][n].influence_value +=
      0.13 * influence_delta_strict_area(m, n, color, saved_stones);
  }
  
  if (move[m][n].influence_value != 0.0)
    TRACE("  %m: %f - influence\n",
	  m, n, move[m][n].influence_value);
}

/*
 * Estimate the strategical value of a move at (m,n).
 */
static void
estimate_strategical_value(int m, int n, int color, float score)
{
  int k;
  int l;
  int ai = -1;
  int aj = -1;
  int bi = -1;
  int bj = -1;
  int d1 = -1;
  int d2 = -1;
  int worm1 = -1;
  int worm2 = -1;
  int ecolor = 0;
  
  float this_value = 0.0;
  float tot_value = 0.0;

  /* Strategical value of connecting or cutting dragons. */
  static float dragon_value[MAX_DRAGONS];

  for (k = 0; k < next_dragon; k++)
    dragon_value[k] = 0.0;
  
  for (k = 0; k < MAX_REASONS; k++) {
    int r = move[m][n].reason[k];
    if (r < 0)
      break;
    
    this_value = 0.0;
    switch (move_reasons[r].type) {
      case ATTACK_MOVE:
      case DEFEND_MOVE:
	worm1 = move_reasons[r].what;
	ai = I(worms[worm1]);
	aj = J(worms[worm1]);
      
	/* Attack on our stones. */
	if (move_reasons[r].type == ATTACK_MOVE
	    && BOARD(ai, aj) == color)
	  break;
	
	/* Defense of enemy stones. */
	if (move_reasons[r].type == DEFEND_MOVE
	    && BOARD(ai, aj) != color)
	  break;
	
	/* Defenseless stone */
	if (worm[ai][aj].defend_code == 0)
	  break;

	/* Require the defense to be strategically viable. */
	if (move_reasons[r].type == DEFEND_MOVE
	    && !strategically_sound_defense(ai, aj, m, n))
	  break;

	/* Do the same for attack moves. */
	if (move_reasons[r].type == ATTACK_MOVE && !move[m][n].move_safety)
	  break;
	
	/* This is totally ad hoc, just guessing the value of
         * potential cutting points.
	 */
	if (worm[ai][aj].cutstone2 > 1) {
	  this_value = 10.0 * (worm[ai][aj].cutstone2 - 1);
	  TRACE("  %m: %f - %m cutstone\n", m, n, 
		this_value, ai, aj);
	}
	
	tot_value += this_value;
	
	/* If the string is a lunch for a weak dragon, the attack or
         * defense has a strategical value. This can be valued along
	 * the same lines as strategic_attack/strategic_defend.
	 *
	 * No points are awarded if the lunch is an inessential dragon
	 * or worm.
	 */
	if (DRAGON2(ai, aj).safety == INESSENTIAL || worm[ai][aj].inessential)
	  break;

	/* Can't use k in this loop too. */
	for (l = 0; l < next_lunch; l++)
	  if (lunch_worm[l] == worm1) {
	    d1 = lunch_dragon[l];
	    bi = dragoni[d1];
	    bj = dragonj[d1];

	    /* FIXME: This value cannot be computed
	       without some measurement of how the actual move affects
	       the dragon. The dragon safety alone is not enough. The 
	       question is whether the dragon is threatened by the move 
	       or not. */
	    this_value = (dragon[bi][bj].effective_size
			  * (1.0 - dragon_safety(bi, bj, 0)));

	    /* If this dragon consists of only one worm and that worm
	     * can be tactically captured or defended by this move, we
	     * have already counted the points as territorial value,
	     * unless it's assumed to be dead.
	     */
	    if (dragon[bi][bj].matcher_status != DEAD
		&& dragon[bi][bj].size == worm[bi][bj].size
		&& (move_reason_known(m, n, ATTACK_MOVE, find_worm(POS(bi, bj)))
		    || move_reason_known(m, n, DEFEND_MOVE,
					 find_worm(POS(bi, bj)))))
	      this_value = 0.0;

	    if (this_value > dragon_value[d1])
	      dragon_value[d1] = this_value;
	  }

	break;
	
      case ATTACK_THREAT_MOVE:
      case DEFEND_THREAT_MOVE:
        break;

      case ATTACK_EITHER_MOVE:
      case DEFEND_BOTH_MOVE:
	/* This is complete nonsense, but still better than nothing.
	 * FIXME: Do this in a reasonable way.
	 */
	worm1 = worm_pair1[move_reasons[r].what];
	worm2 = worm_pair2[move_reasons[r].what];
	ai = I(worms[worm1]);
	aj = J(worms[worm1]);
	bi = I(worms[worm2]);
	bj = J(worms[worm2]);

	/* If both worms are dead, this move reason has no value. */
	if (dragon[ai][aj].matcher_status == DEAD 
	    && dragon[bi][bj].matcher_status == DEAD)
	  break;

	/* If we already have unconditional attack or defense of
         * either worm, this move reason has no additional value.
	 */
	if (move_reasons[r].type == ATTACK_EITHER_MOVE
	    && (move_reason_known(m, n, ATTACK_MOVE, worm1)
		|| move_reason_known(m, n, ATTACK_MOVE, worm2)))
	  break;
	if (move_reasons[r].type == DEFEND_BOTH_MOVE
	    && (move_reason_known(m, n, DEFEND_MOVE, worm1)
		|| move_reason_known(m, n, DEFEND_MOVE, worm2)))
	  break;

	/* Also if there is a combination attack, we assume it covers
         * the same thing.
	 */
	if (move_reasons[r].type == ATTACK_EITHER_MOVE
	    && move_reason_known(m, n, MY_ATARI_ATARI_MOVE, -1))
	  break;
	if (move_reasons[r].type == DEFEND_BOTH_MOVE
	    && move_reason_known(m, n, YOUR_ATARI_ATARI_MOVE, -1))
	  break;

	this_value = 2 * gg_min(worm[ai][aj].effective_size,
				worm[bi][bj].effective_size);
	if (move_reasons[r].type == ATTACK_EITHER_MOVE)
	  TRACE("  %m: %f - attacks either %m or %m\n",
		m, n, this_value, ai, aj, bi, bj);
	else
	  TRACE("  %m: %f - defends both %m and %m\n",
		m, n, this_value, ai, aj, bi, bj);

	tot_value += this_value;
	break;
	
      case CONNECT_MOVE:
	if (!move[m][n].move_safety)
	  break;
	/* Otherwise fall through. */
      case CUT_MOVE:
	if (doing_scoring && !move[m][n].move_safety)
	  break;

	d1 = conn_dragon1[move_reasons[r].what];
	d2 = conn_dragon2[move_reasons[r].what];
	ai = dragoni[d1];
	aj = dragonj[d1];
	bi = dragoni[d2];
	bj = dragonj[d2];

	/* If we are ahead by more than 20, value connections more strongly */
	if ((color == WHITE && score > 20.0)
	    || (color == BLACK && score < -20.0))
	  this_value = connection_value(ai, aj, bi, bj, m, n, gg_abs(score));
	else
	  this_value = connection_value(ai, aj, bi, bj, m, n, 0);
	if (this_value > dragon_value[d1])
	  dragon_value[d1] = this_value;
	
	if ((color == WHITE && score > 20.0)
	    || (color == BLACK && score < -20.0))
	  this_value = connection_value(bi, bj, ai, aj, m, n, gg_abs(score));
	else
	  this_value = connection_value(bi, bj, ai, aj, m, n, 0);
	if (this_value > dragon_value[d2])
	  dragon_value[d2] = this_value;
	
	break;
	
      case SEMEAI_MOVE:
	/*
	 * The strategical value of winning a semeai is
	 * own dragons (usually) becomes fully secure, while adjoining
	 * enemy dragons do not.
	 *
	 * FIXME: Valuation not implemented at all yet. 
	 */

	break;
	
      case VITAL_EYE_MOVE:
	/*
	 * The value of the threatened group itself has already been
	 * accounted for in territorial_value. Now we need to determine
	 * the effect this has on surrounding groups.
	 *
	 * FIXME: Valuation not implemented.
	 */
	ai = eyei[move_reasons[r].what];
	aj = eyej[move_reasons[r].what];
	ecolor = eyecolor[move_reasons[r].what];

	if (ecolor == WHITE) {
	  bi = white_eye[ai][aj].dragoni;
	  bj = white_eye[ai][aj].dragonj;
	}
	else {
	  bi = black_eye[ai][aj].dragoni;
	  bj = black_eye[ai][aj].dragonj;
	}

	if (bi == -1) /* Maybe we should assert this not to happen. */
	  break; 

	/* If there is an owl attack/defend move reason for this location,
	 * we don't care about it, since otherwise we would count the
	 * points twice.
	 */
	if (move_reason_known(m, n, OWL_DEFEND_MOVE, find_dragon(bi, bj))
	    || move_reason_known(m, n, OWL_ATTACK_MOVE, find_dragon(bi, bj))) {
	  DEBUG(DEBUG_MOVE_REASONS,
		"  %m: 0.0 - vital for %m: owl attack/defense as well\n",
		m, n, bi, bj);
	  break;
	}

#if 0
	if (dragon[bi][bj].status == CRITICAL) {
	  this_value = ???
	  TRACE("  %m: %f - vital for %m\n",
		m, n, this_value, bi, bj);
	  tot_value += this_value;
	}
#endif
	break;

      case STRATEGIC_ATTACK_MOVE:
      case STRATEGIC_DEFEND_MOVE:	
	/* The right way to do this is to estimate the safety of the
	 * dragon before and after the move. Unfortunately we are
	 * missing good ways to do this currently.
	 *
	 * Temporary solution is to only look at an ad hoc measure of
	 * the dragon safety and ignoring the effectiveness of the
	 * move.
	 *
	 * FIXME: Improve the implementation.
	 */
	d1 = move_reasons[r].what;
	ai = dragoni[d1];
	aj = dragonj[d1];

	/* FIXME: This value cannot be computed
	   without some measurement of how the actual move affects
	   the dragon. The dragon safety alone is not enough. The 
	   question is whether the dragon is threatened by the move 
	   or not. */	
	this_value = (dragon[ai][aj].effective_size
		      * (1.0 - dragon_safety(ai, aj, 1)));

	/* To prefer good connections and cuts, we lower this value
	 * somewhat.
	 */
	this_value *= 0.75;

	if (this_value > dragon_value[d1])
	  dragon_value[d1] = this_value;

	break;
    }
  }
  
  for (k = 0; k < next_dragon; k++) {
    if (dragon_value[k] == 0.0)
      continue;

    ai = dragoni[k];
    aj = dragonj[k];
    
    /* If this dragon consists of only one worm and that worm can
     * be tactically captured or defended by this move, we have
     * already counted the points as territorial value, unless
     * it's assumed to be dead.
     */
    if (dragon[ai][aj].matcher_status != DEAD
	&& dragon[ai][aj].size == worm[ai][aj].size
	&& (move_reason_known(m, n, ATTACK_MOVE, find_worm(POS(ai, aj)))
	    || move_reason_known(m, n, DEFEND_MOVE, find_worm(POS(ai, aj)))))
      continue;
    
    /* If the dragon has been owl captured, owl defended, or involved
     * in a semeai, we have likewise already counted the points as
     * territorial value.
     */
    if (move_reason_known(m, n, OWL_ATTACK_MOVE, k)
	|| move_reason_known(m, n, OWL_DEFEND_MOVE, k)
	|| move_reason_known(m, n, SEMEAI_MOVE, k)) {
      /* But if the strategical value was larger than the territorial
       * value (e.g. because connecting to strong dragon) we award the
       * excess value as a bonus.
       */
      float excess_value = (dragon_value[k] - 
			    2 * dragon[dragoni[k]][dragonj[k]].effective_size);
      if (excess_value > 0.0) {
	TRACE("  %m: %f - strategic bonus for %m\n",
	      m, n, excess_value, dragoni[k], dragonj[k]);
	tot_value += excess_value;
      }
      
      continue;
    }
    
    TRACE("  %m: %f - strategic effect on %m\n",
	  m, n, dragon_value[k], dragoni[k], dragonj[k]);
    tot_value += dragon_value[k];
  }

  move[m][n].strategical_value = tot_value;
}


/* Look through the move reasons to see whether (m, n) is an antisuji move. */
static int
is_antisuji_move(int m, int n)
{
  int k;
  for (k = 0; k < MAX_REASONS; k++) {
    int r = move[m][n].reason[k];
    if (r < 0)
      break;
    if (move_reasons[r].type == ANTISUJI_MOVE)
      return 1; /* This move must not be played. End of story. */
  }
  return 0;
}

/* Count how many distinct strings are (solidly) connected by the move
 * at (m, n). Add a bonus for strings with few liberties. Also add
 * bonus for opponent strings put in atari or removed.
 */
static int
move_connects_strings(int m, int n, int color)
{
  int si[4];
  int sj[4];
  int strings = 0;
  int own_strings = 0;
  int k, l;
  int oi, oj;
  int fewlibs = 0;

  for (k = 0; k < 4; k++) {
    int i = m + deltai[k];
    int j = n + deltaj[k];
    int origin;

    if (!ON_BOARD2(i, j) || BOARD(i, j) == EMPTY)
      continue;

    origin = find_origin(POS(i, j));
    oi = I(origin);
    oj = J(origin);

    for (l = 0; l < strings; l++)
      if (si[l] == oi && sj[l] == oj)
	break;

    if (l == strings) {
      si[strings] = oi;
      sj[strings] = oj;
      strings++;
    }
  }

  for (k = 0; k < strings; k++) {
    if (BOARD(si[k], sj[k]) == color) {
      int newlibs = approxlib(POS(m, n), color, MAXLIBS, NULL);
      own_strings++;
      if (newlibs >= countlib2(si[k], sj[k])) {
	if (countlib2(si[k], sj[k]) <= 4)
	  fewlibs++;
	if (countlib2(si[k], sj[k]) <= 2)
	  fewlibs++;
      }
    }
    else {
      if (countlib2(si[k], sj[k]) <= 2)
	fewlibs++;
      if (countlib2(si[k], sj[k]) <= 1)
	fewlibs++;
    }
  }

  /* Do some thresholding. */
  if (fewlibs > 4)
    fewlibs = 4;
  if (fewlibs == 0 && own_strings == 1)
    own_strings = 0;

  return own_strings + fewlibs;
}

/* Compare two move reasons, used for sorting before presentation. */
static int
compare_move_reasons(const void *p1, const void *p2)
{
  const int mr1 = *(const int *) p1;
  const int mr2 = *(const int *) p2;

  if (move_reasons[mr1].type != move_reasons[mr2].type)
    return move_reasons[mr2].type - move_reasons[mr1].type;
  else
    return move_reasons[mr2].what - move_reasons[mr1].what;
}


/*
 * Combine the reasons for a move at (m, n) into an old style value.
 * These heuristics are now somewhat less ad hoc but probably still
 * need a lot of improvement.
 */
static float
value_move_reasons(int m, int n, int color, float pure_threat_value,
		   float score)
{
  float tot_value;
  float shape_factor;
  char saved_stones[MAX_BOARD][MAX_BOARD];

  gg_assert (stackp == 0);
  /* Is it an antisuji? */
  if (is_antisuji_move(m, n))
    return 0.0; /* This move must not be played. End of story. */

  /* If this move has no reason at all, we can skip some steps. */
  if ((!urgent || allpats)
      && (move[m][n].reason[0] >= 0
	  || move[m][n].min_territory > 0.0)) {
    int num_reasons;

    /* Sort the move reasons. This makes it easier to visually compare
     * the reasons for different moves in the trace outputs.
     */
    num_reasons = 0;
    while (move[m][n].reason[num_reasons] >= 0)
      num_reasons++;
    qsort(&(move[m][n].reason[0]), num_reasons, sizeof(move[m][n].reason[0]),
	  compare_move_reasons);

    /* Estimate the value of various aspects of the move. The order
     * is significant. Territorial value must be computed before
     * strategical value. See connection_value().
     */
    find_stones_saved_by_move(m, n, color, saved_stones);
    estimate_territorial_value(m, n, color, saved_stones, score);
    estimate_strategical_value(m, n, color, score);
    estimate_influence_value(m, n, color, saved_stones);
  }

  tot_value = (move[m][n].territorial_value
	       + move[m][n].strategical_value
	       + move[m][n].influence_value);

  shape_factor = compute_shape_factor(m, n);

  if (tot_value > 0.0) {
    int c;
    
    /* In the endgame, there are a few situations where the value can
     * be 0 points + followup.  But we want to take the intersections first
     * were we actually get some points.  0.5 points is a 1 point ko which
     * is the smallest value that is actually worth something.
     */
    if (tot_value >= 0.5) {
      float old_tot_value = tot_value;
      tot_value += gg_min(tot_value, 0.5 * move[m][n].followup_value);
      if (move[m][n].followup_value != 0.0)
	TRACE("  %m: %f - followup\n",
	      m, n, move[m][n].followup_value);
      
      tot_value += gg_min(0.75* tot_value, 
			  0.4 * move[m][n].reverse_followup_value);
      if (move[m][n].reverse_followup_value != 0.0)
	TRACE("  %m: %f - reverse followup\n",
	      m, n, move[m][n].reverse_followup_value);

      /* If a ko fight is going on, we should use the full followup
       * and reverse followup values in the total value. We save the
       * additional contribution for later access.
       */
      move[m][n].additional_ko_value =
	move[m][n].followup_value + move[m][n].reverse_followup_value -
	(tot_value - old_tot_value);

      /* Not sure whether this could happen, but check for safety. */
      if (move[m][n].additional_ko_value < 0.0)
	move[m][n].additional_ko_value = 0.0;
    }
    else {
      move[m][n].additional_ko_value =
	shape_factor *
	(move[m][n].followup_value + move[m][n].reverse_followup_value);
    }

    tot_value += 0.05 * move[m][n].secondary_value;
    if (move[m][n].secondary_value != 0.0)
      TRACE("  %m: %f - secondary\n",
	    m, n, 0.05 * move[m][n].secondary_value);

    if (move[m][n].numpos_shape + move[m][n].numneg_shape > 0) {
      /* shape_factor has already been computed. */
      float old_value = tot_value;
      tot_value *= shape_factor;
      if (verbose) {
	/* Should all have been TRACE, except we want field sizes. */
	gprintf("  %m: %f - shape ", m, n, tot_value - old_value);
	fprintf(stderr,
		"(shape values +%4.2f(%d) -%4.2f(%d), shape factor %5.3f)\n",
		move[m][n].maxpos_shape, move[m][n].numpos_shape,
		move[m][n].maxneg_shape, move[m][n].numneg_shape,
		shape_factor);
      }
    }

    /* Add a special shape bonus for moves which connect strings. */
    c = move_connects_strings(m, n, color);
    if (c > 0) {
      float shape_factor2 = pow(1.02, (float) c) - 1;
      float base_value = gg_max(gg_min(tot_value, 5.0), 1.0);
      if (verbose) {
	/* Should all have been TRACE, except we want field sizes. */
	gprintf("  %m: %f - connects strings ", m, n,
		base_value * shape_factor2);
	fprintf(stderr, "(connect value %d, shape factor %5.3f)\n", c,
		shape_factor2);
      }
      tot_value += base_value * shape_factor2;
    }
  }
  else {
    move[m][n].additional_ko_value =
      shape_factor *
      (move[m][n].followup_value +
       gg_min(move[m][n].followup_value,
	      move[m][n].reverse_followup_value));
  }

  /* If the move is valued 0 or small, but has followup values and is
   * flagged as a worthwhile threat, add up to pure_threat_value to
   * the move.
   *
   * FIXME: We shouldn't have to call confirm_safety() here. It's
   * potentially too expensive.
   */
  if (pure_threat_value > 0.0 
      && move[m][n].worthwhile_threat
      && tot_value <= pure_threat_value
      && BOARD(m, n) == EMPTY
      && move[m][n].additional_ko_value > 0.0
      && is_legal2(m, n, color)
      && confirm_safety(m, n, color, 0, NULL, NULL)) {
    float new_tot_value = gg_min(pure_threat_value,
				 tot_value
				 + 0.25 * move[m][n].additional_ko_value);

    /* Make sure that moves with independent value are preferred over
     * those without.
     */
    new_tot_value *= (1.0 - 0.1 * (pure_threat_value - tot_value)
		      / pure_threat_value);
    
    if (new_tot_value > tot_value) {
      TRACE("  %m: %f - carry out threat or defend against threat\n",
	    m, n, new_tot_value - tot_value);
      tot_value = new_tot_value;
    }
  }
  
  /* Test if min_value or max_value values constrain the total value. */
  if (tot_value > move[m][n].max_value) {
    tot_value = move[m][n].max_value;
    TRACE("  %m:   %f - maximum accepted value\n",
	  m, n, tot_value);
  }
  
  if (tot_value < move[m][n].min_value && move[m][n].min_value > 0) {
    tot_value = move[m][n].min_value;
    TRACE("  %m:   %f - minimum accepted value\n",
	  m, n, tot_value);
  }

  if (tot_value > 0
      || move[m][n].territorial_value > 0
      || move[m][n].strategical_value > 0) {
    TRACE("Move generation values %m to %f\n", 
	  m, n, tot_value);
    move_considered(m, n, tot_value);
  }

  return tot_value;
}

/*
 * Loop over all possible moves and value the move reasons for each.
 */
static void
value_moves(int color, float pure_threat_value, float score)
{
  int m;
  int n;

  TRACE("\nMove valuation:\n");
  
  /* visit the moves in the standard lexicographical order */
  for (n = 0; n < board_size; n++)
    for (m = board_size-1; m >= 0; m--) {
      move[m][n].value = value_move_reasons(m, n, color, pure_threat_value,
					    score);
      if (move[m][n].value == 0.0)
	continue;
      /* Maybe this test should be performed elsewhere. This is just
       * to get some extra safety. We don't filter out illegal ko
       * captures here though, because if that is the best move, we
       * should reevaluate ko threats.
       */
      if (!(is_illegal_ko_capture2(m, n, color) || is_legal2(m, n, color))) {
	move[m][n].value = 0.0;
	TRACE("Move at %m wasn't legal.\n", m, n);
      }
      else {
	/* Add a random number between 0 and 0.01 to use in comparisons. */
	move[m][n].value += 0.01 * move[m][n].random_number;
      }
    }
}

static void
print_top_moves(void)
{
  int k;
  int m, n;
  float tval;
  
  for (k = 0; k < 10; k++)
    best_move_values[k] = 0.0;
  
  /* Search through all board positions for the 10 highest valued
   * moves and print them.
   */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      if (move[m][n].final_value <= 0.0)
	continue;
      
      tval = move[m][n].final_value;

      for (k = 9; k >= 0; k--)
	if (tval > best_move_values[k]) {
	  if (k < 9) {
	    best_move_values[k+1] = best_move_values[k];
	    best_movei[k+1] = best_movei[k];
	    best_movej[k+1] = best_movej[k];
	  }
	  best_move_values[k] = tval;
	  best_movei[k] = m;
	  best_movej[k] = n;
	}
    }

  TRACE("\nTop moves:\n");
  for (k = 0; k < 10 && best_move_values[k] > 0.0; k++) {
    TRACE("%d. %M %f\n", k+1, best_movei[k], best_movej[k],
	  best_move_values[k]);
  }
}

static void
reevaluate_ko_threats(void)
{
  int m, n;
  TRACE("Reevaluating ko threats.\n");
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) 
      if (move[m][n].additional_ko_value > 0.0) {
	TRACE("%m: %f + %f = %f\n", m, n, move[m][n].value,
	      move[m][n].additional_ko_value,
	      move[m][n].value + move[m][n].additional_ko_value);
	move[m][n].value += move[m][n].additional_ko_value;
      }
}

static void
redistribute_points(void)
{
  int m, n;
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) 
      move[m][n].final_value = move[m][n].value;
  
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      int i = replacement_mapi[m][n];
      int j = replacement_mapj[m][n];
      if (i == -1)
	continue;
      TRACE("Redistributing points from %m to %m.\n", m, n, i, j);
      if (move[i][j].final_value < move[m][n].final_value) {
	TRACE("%m is now valued %f.\n", i, j, move[m][n].final_value);
	move[i][j].final_value = move[m][n].final_value;
      }
      TRACE("%m is now valued 0.\n", m, n);
      move[m][n].final_value = 0.0;
    }
}

/*
 * Review the move reasons to find which (if any) move we want to play.
 *
 * The parameter pure_threat_value is the value assigned to a move
 * which only threatens to capture or kill something. The reason for
 * playing these is that the move may be effective because we have
 * misevaluated the dangers or because the opponent misplays.
 */
int
review_move_reasons(int *i, int *j, float *val, int color,
		    float pure_threat_value, float score)
{
  int m, n;
  float tval;
  float bestval = 0.0;
  int best_i = -1, best_j = -1;
  int ko_values_have_been_added = 0;
  int allowed_blunder_size = 0;

  int good_move_found = 0;
  int save_verbose;
  
  start_timer(2);
  if (!urgent || allpats) {
    find_more_attack_and_defense_moves(color);
    time_report(2, "  find_more_attack_and_defense_moves", -1, -1);
  }

  remove_opponent_attack_and_defense_moves(color);
  time_report(2, "  remove_opponent_attack_and_defense_moves", -1, -1);

  do_remove_false_attack_and_defense_moves();
  time_report(2, "  do_remove_false_attack_and_defense_moves", -1, -1);

  save_verbose = verbose;
  if (verbose > 0)
    verbose--;
  if (level > 5) {
    find_more_owl_attack_and_defense_moves(color);
    time_report(2, "  find_more_owl_attack_and_defense_moves", -1, -1);
  }
  verbose = save_verbose;

  induce_secondary_move_reasons(color);
  time_report(2, "  induce_secondary_move_reasons", -1, -1);
  
  if (verbose > 0)
    verbose--;
  examine_move_safety(color);
  time_report(2, "  examine_move_safety", -1, -1);
  verbose = save_verbose;

  if (printworms || verbose)
    list_move_reasons(color);

  /* Evaluate all moves with move reasons. */
  value_moves(color, pure_threat_value, score);
  time_report(2, "  value_moves", -1, -1);

  /* Perform point redistribution */
  redistribute_points();

  /* Search through all board positions for the 10 highest valued
   * moves and print them.
   */
  print_top_moves();
  
  while (!good_move_found) {
    bestval = 0.0;
    best_i = -1;
    best_j = -1;

    /* Search through all board positions for the highest valued move. */
    for (m = 0; m < board_size; m++)
      for (n = 0; n < board_size; n++) {
	if (move[m][n].final_value == 0.0)
	  continue;
	
	tval = move[m][n].value;
	
	if (tval > bestval) {
	  if (is_legal2(m, n, color) || is_illegal_ko_capture2(m, n, color)) {
	    bestval = tval;
	    best_i = m;
	    best_j = n;
	  }
	  else {
	    TRACE("Move at %m would be suicide.\n", m, n);
	    move[m][n].value = 0.0;
	    move[m][n].final_value = 0.0;
	  }
	}
      }
    
    /* Compute the size of strings we can allow to lose due to blunder
     * effects. If ko threat values have been added, only the base
     * value of the move must be taken into account here.
     */
    if (!ko_values_have_been_added || !ON_BOARD2(best_i, best_j))
      allowed_blunder_size = (int) (bestval / 2);
    else {
      int base_value;
      ASSERT_ON_BOARD2(best_i, best_j);
      base_value = bestval - move[best_i][best_j].additional_ko_value;
      allowed_blunder_size = (int) (base_value / 2);
    }
    
    /* If the best move is an illegal ko capture, reevaluate ko
     * threats and search again.
     */
    if (bestval > 0.0 && is_illegal_ko_capture2(best_i, best_j, color)) {
      TRACE("Move at %m would be an illegal ko capture.\n", best_i, best_j);
      reevaluate_ko_threats();
      redistribute_points();
      time_report(2, "  reevaluate_ko_threats", -1, -1);
      ko_values_have_been_added = 1;
      move[best_i][best_j].value = 0.0;
      move[best_i][best_j].final_value = 0.0;
      print_top_moves();
      good_move_found = 0;
    }
    /* Call confirm_safety() to check that we're not about to make a
     * blunder. Otherwise reject this move and scan through all move
     * values once more.
     */
    else if (bestval > 0.0
	     && !confirm_safety(best_i, best_j, 
				color, allowed_blunder_size, NULL, NULL)) {
      TRACE("Move at %m would be a blunder.\n", best_i, best_j);
      move[best_i][best_j].value = 0.0;
      move[best_i][best_j].final_value = 0.0;
      good_move_found = 0;
    }
    else
      good_move_found = 1;
  }
  
  if (bestval > 0.0 
      && best_i != -1) {
    *val = bestval;
    *i = best_i;
    *j = best_j;
    return 1;
  }
  else
    return 0;
}


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
