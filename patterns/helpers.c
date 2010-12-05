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

#include <stdio.h>
#include "liberty.h"
#include "patterns.h"


#define TRYMOVE(pos, color) trymove(pos, color, "helper", NO_MOVE)
#define OFFSET_BY(x, y) AFFINE_TRANSFORM(OFFSET(x, y), trans, move)
#define ARGS struct pattern *pattern, int trans, int move, int color


/* This file contains helper functions which assist the pattern matcher.
 * They are invoked with (move) = the position on the board marked with '*'.
 * They are invoked with color = WHITE or BLACK: any pieces on the
 * board marked with 'O' in the pattern will always contain color,
 * and 'X's contain OTHER_COLOR(color)
 *
 * The helper must return 0 if the pattern is rejected and 1 otherwise.
 */




/* Jump out into nothingness. To avoid jumping into our own territory,
 * we use the "area" measure. Also we never ever jump into our own
 * established eyespace.
 */

int 
jump_out_helper(ARGS)
{
  int own_eyespace;

  UNUSED(trans); UNUSED(pattern);

  own_eyespace = (white_eye[move].color == color);
  
  if (whose_area(OPPOSITE_INFLUENCE(color), move) != color && !own_eyespace)
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
  if (whose_area(OPPOSITE_INFLUENCE(color), move) != OTHER_COLOR(color))
    return jump_out_helper(pattern, trans, move, color);
  else
    return 0;
}


/* Active until the opponent has played his first stone.
 */

int
high_handicap_helper(ARGS)
{
  UNUSED(trans); UNUSED(pattern); UNUSED(move);
  
  return !doing_scoring && stones_on_board(OTHER_COLOR(color)) == 0;
}


/* Active when the opponent is thought to be everywhere dead. This
 * typically happens early in high handicap games on small boards.
 * This helper is used by patterns intended to reinforce possible
 * weaknesses in the position.
 */

int
reinforce_helper(ARGS)
{
  UNUSED(trans); UNUSED(pattern);
  
  return (!doing_scoring
	  && !lively_dragon_exists(OTHER_COLOR(color)) 
	  && safe_move(move, color));
}


/*
 *  
 *  XXO      XXc            decrease eye space in sente (unless it kills)
 *  .*X      e*a
 *  ---      ---
 *
 * or
 *
 *  XXO      XXc            decrease eye space in sente (unless it kills)
 *  .*X      e*a
 *  XXO      XXd
 *
 * or
 *
 *  |XXO     |XXc           decrease eye space in sente (unless it kills)
 *  |.*X     |e*a
 *  |XXO     |XXd
 *
 * or
 *
 *  |XXO     |XXc           decrease eye space in sente (unless it kills)
 *  |.*X     |e*a
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
  
  apos = OFFSET_BY(0, 1);
  cpos = OFFSET_BY(-1, 1);
  dpos = OFFSET_BY(1, 1);

  /* Find second liberty of the stone a. */
  findlib(apos, 2, libs);
  if (libs[0] != move)
    bpos = libs[0];
  else
    bpos = libs[1];
  
  if (TRYMOVE(move, color)) {
    if (!attack(cpos, NULL) && !(ON_BOARD(dpos) && attack(dpos, NULL))) {
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
   * twice the size.
   */
  add_followup_value(move, 2 * worm[apos].effective_size);
  TRACE("...followup value %f\n", 2 * worm[apos].effective_size);

  return success;
}


/* This is intended for use in autohelpers. */

/* Check whether the string at (str) can attack any surrounding
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

  if (stackp > 0)
    return 0;
  
  apos = OFFSET_BY(-1, -1);
  bpos = OFFSET_BY(-1,  0);
  cpos = OFFSET_BY( 0, -1);

  if (worm[apos].defense_codes[0] == 0)
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
 *  ?X..   ?Xb.
 *  O*..   c*a.
 *
 * Is the push at * sente? c must have exactly two liberties. This is
 * called edge_double_sente_helper because it mainly called for edge
 * hanes, but it can be used anywhere on the board.
 */

int 
edge_double_sente_helper(int move, int apos, int bpos, int cpos)
{
  int color = board[cpos];
  int success = 0;
  ASSERT1((color == BLACK || color == WHITE), move);
  
  if (TRYMOVE(move, color)) {
    ASSERT1(countlib(move) == 2, move);
    success = connect_and_cut_helper(move, apos, bpos);
    popgo();
  }

  return success;
}

/*
 * This is intended for use in autohelpers.
 *
 * Give a conservative estimate of the value of saving the string (str)
 * by capturing one opponent stone.
 */

void
threaten_to_save_helper(int move, int str)
{
  add_followup_value(move, 2.0 + 2.0 * worm[str].effective_size);
  TRACE("...followup value %f\n", 2.0 + 2.0 * worm[str].effective_size);
}


/* For use in autohelpers.
 *
 * Adds a reverse followup value if the opponent's move here would threaten
 * to capture (str).
 */
void
prevent_attack_threat_helper(int move, int str)
{
  add_reverse_followup_value(move, 2.0 * worm[str].effective_size);
  TRACE("...reverse followup value %f\n", 2.0 * worm[str].effective_size);
}


/* This function is obsolete.  Code in `value_moves.c' is more general. */
#if 0

/*
 * This is intended for use in autohelpers.
 *
 * Estimate the value of capturing the string (str) and add this as
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
    if (worm[adjs[k]].defense_codes[0] != 0
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

  /* In addition to the move found by find_defense(), also try all
   * chain breaking moves in the same way.
   */
  adj = chainlinks2(str, adjs, 1);
  for (k = 0; k < adj; k++) {
    int lib;
    findlib(adjs[k], 1, &lib);
    if (TRYMOVE(lib, board[str])) {
      if (!attack(str, NULL)
	  && (board[move] == EMPTY || attack(move, NULL) != 0)) {
	popgo();
	popgo();
	return;
      }
      popgo();
    }
  }

  popgo();
  
  add_followup_value(move, 2.0 * worm[str].effective_size);
  TRACE("...followup value %f\n", 2.0 * worm[str].effective_size);
}

#endif


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
    if (worm[adjs[k]].defense_codes[0] != 0
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
 * Returns 1 if (pos) is adjacent to a stone which can be captured by ko.
 */

int
finish_ko_helper(int pos)
{
  int adj, adjs[MAXCHAIN];
  int lib;
  int k;

  adj = chainlinks2(pos, adjs, 1);
  for (k = 0; k < adj; k++) {
    if (countstones(adjs[k]) == 1) {
      findlib(adjs[k], 1, &lib);
      if (is_ko(lib, board[pos], NULL))
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
squeeze_ko_helper(int pos)
{
  int libs[2];
  int liberties;
  int k;

  liberties = findlib(pos, 2, libs);
  ASSERT1(liberties == 2, pos);

  for (k = 0; k < liberties; k++) {
    int aa = libs[k];
    if (is_ko(aa, OTHER_COLOR(board[pos]), NULL))
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
  if (DRAGON2(bpos).owl_status == CRITICAL
      && DRAGON2(bpos).owl_attack_point == apos)
    return 1;
  
  if (DRAGON2(bpos).owl_threat_status == CAN_THREATEN_ATTACK)
    if (DRAGON2(bpos).owl_attack_point == apos
	|| DRAGON2(bpos).owl_second_attack_point == apos)
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
 * This is similar to connect_and_cut_helper(), except it starts with
 * a move at A and that d is found as a general defense point for A. A
 * is no longer restricted to two liberties.
 *
 * |.X   |dX
 * |XO	 |XO
 * |.O	 |Ae
 * |..	 |bc
 */
   
int
connect_and_cut_helper2(int Apos, int bpos, int cpos, int color)
{
  int dpos;
  int epos = NO_MOVE;
  int other = OTHER_COLOR(color);
  int result = 0;
  int k;

  gg_assert(IS_STONE(color));


  if (TRYMOVE(Apos, color)) {
    for (k = 0; k < 4; k++)
      if (board[cpos + delta[k]] == other
	  && neighbor_of_string(cpos + delta[k], Apos)) {
	epos = cpos + delta[k];
	break;
      }

    gg_assert(epos != NO_MOVE);
    
    if (TRYMOVE(bpos, other)) {
      if (!find_defense(Apos, &dpos) || dpos == NO_MOVE) {
	popgo();
	popgo();
	return 0;
      }
      
      if (TRYMOVE(dpos, color)) {
	if (TRYMOVE(cpos, color)) {
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
    popgo();
  }
  
  return result;
}



void
test_attack_either_move(int move, int color, int worma, int wormb)
{
  ASSERT_ON_BOARD1(move);
  ASSERT1(board[move] == EMPTY, move);
  ASSERT1(board[worma] == OTHER_COLOR(color)
          && board[wormb] == OTHER_COLOR(color), move);

  if (!defend_both(worma, wormb)) {
    if (0)
      gprintf("%1m: Reject attack_either_move for %1m, %1m (can't defend both)\n",
	      move, worma, wormb);
    return;
  }
  if (trymove(move, color, "test_attack_either_move", worma)) {
    if (board[worma] == OTHER_COLOR(color)
	&& board[wormb] == OTHER_COLOR(color)) {
      if (!find_defense(worma, NULL) || !find_defense(wormb, NULL)) {
	if (0)
	  gprintf("%1m: Rej. attack_either_move for %1m & %1m (regular attack)\n",
		  move, worma, wormb);
      }
      else if (!defend_both(worma, wormb))
        add_either_move(move, ATTACK_STRING, worma, ATTACK_STRING, wormb);
      else {
	if (0)
	  gprintf("%1m: Rej. attack_either_move for %1m & %1m (doesn't work)\n",
		  move, worma, wormb);
      }
    }
    else
      if (0)
	gprintf("%1m: Rej. attack_either_move for %1m & %1m (captured directly)\n",
		move, worma, wormb);
    popgo();
  }
}

/* True if str is adjacent to a stone in atari, which is tactically
 * attackable (to exclude pointless captures of snapback stones).
 */
int
adjacent_to_stone_in_atari(int str)
{
  int adj;
  int adjs[MAXCHAIN];
  int k;

  adj = chainlinks2(str, adjs, 1);
  for (k = 0; k < adj; k++)
    if (attack(adjs[k], NULL))
      return 1;
  
  return 0;
}


/* True if str is adjacent to a stone in atari, which is tactically
 * defendable.
 */
int
adjacent_to_defendable_stone_in_atari(int str)
{
  int adj;
  int adjs[MAXCHAIN];
  int k;

  adj = chainlinks2(str, adjs, 1);
  for (k = 0; k < adj; k++)
    if (attack_and_defend(adjs[k], NULL, NULL, NULL, NULL))
      return 1;
  
  return 0;
}

void
backfill_replace(int move, int str)
{
  int defense_move = NO_MOVE;

  if (TRYMOVE(move, OTHER_COLOR(board[str]))) {
    if (attack_and_defend(str, NULL, NULL, NULL, &defense_move)) {
      /* Must undo the trymove before adding the replacement move. */
      popgo();
      add_replacement_move(move, defense_move, board[str]);
    }
    else
      popgo();
  }
}


/* True if
 * 1. White to move.
 * 2. All white stones look dead.
 * 3. Less than 40% of the board is filled or less than 20% of the
 *    board is filled with white stones.
 *
 * This is intended for patterns forcing white to thrash around early
 * in high handicap games, instead of passing because it looks like no
 * stones can live.
 */
int 
thrash_around_helper(ARGS)
{
  UNUSED(pattern);
  UNUSED(trans);
  UNUSED(move);

  /* Do not generate these moves when doing scoring or if fuseki move
   * generation is disabled (typically used when solving life and
   * death problems embedded on a big board).
   */
  if (doing_scoring
      || disable_fuseki
      || (stones_on_board(BLACK | WHITE) > board_size * board_size * 2 / 5
	  && stones_on_board(WHITE) > board_size * board_size / 5)
      || color == BLACK
      || lively_dragon_exists(WHITE))
    return 0;

  return 1;
}


/* Returns true if
 *
 * 1. The board size is odd.
 * 2. A white move is being generated.
 * 3. The komi is less than or equal to zero.
 * 4. str is placed at tengen.
 * 5. At least 10 moves have been played.
 * 6. The board is currently mirror symmetric.
 *
 * This is intended for patterns to break mirror go when black starts at
 * tengen and then mirrors white. We only care about breaking the mirroring
 * if komi is non-positive, otherwise the mirroring is to our advantage.
 */
int
break_mirror_helper(int str, int color)
{
  if (board_size % 2 == 1
      && color == WHITE
      && komi <= 0.0
      && I(str) == (board_size - 1) / 2
      && J(str) == (board_size - 1) / 2
      && stones_on_board(BLACK | WHITE) > 10
      && test_symmetry_after_move(PASS_MOVE, EMPTY, 1))
    return 1;

  return 0;
}


/* This helper is intended to detect semeai kind of positions where
 * the tactical reading can't be trusted enough to allow amalgamation
 * over presumably tactically dead strings.
 *
 * It has turned out to be best not to trust tactical reading of three
 * and four liberty strings at all. Not trusting two liberty strings
 * leads to an underamalgamation and unnecessarily many dragons on the
 * board. Therefore we try to detect two liberty strings with an
 * enclosed nakade, which after capturing leads to an unreliable
 * reading at three or four liberties.
 *
 * More specifically we check whether the string has a neighbor with
 * the following properties:
 * 1. At least three stones in size.
 * 2. All its liberties are common liberties with the string.
 * 3. It has no second order liberties.
 * 4. Its liberties are adjacent to no other strings than itself and
 *    the primary string.
 *
 * If we find such a neighbor 1 is returned, otherwise 0.
 */
int distrust_tactics_helper(int str)
{
  int color = board[str];
  int adj;
  int adjs[MAXCHAIN];
  int k;
  int r;
  int s;
  int lib = countlib(str);

  ASSERT1(IS_STONE(board[str]), str);
  
  if (lib > 2)
    return 1;
  else if (lib == 1)
    return 0;
  
  adj = chainlinks3(str, adjs, lib);
  for (r = 0; r < adj; r++) {
    int nakade = 1;
    int adjlib;
    int adjlibs[3];
    if (countstones(adjs[r]) < 3)
      continue;
    adjlib = findlib(adjs[r], 3, adjlibs);
    for (s = 0; s < adjlib; s++) {
      int str_found = 0;
      for (k = 0; k < 4; k++) {
	int pos = adjlibs[s] + delta[k];
	if (board[pos] == EMPTY
	    && !liberty_of_string(pos, adjs[r]))
	  nakade = 0;
	else if (board[pos] == color) {
	  if (same_string(pos, str))
	    str_found = 1;
	  else
	    nakade = 0;
	}
	else if (board[pos] == OTHER_COLOR(color)
		 && !same_string(pos, adjs[r]))
	  nakade = 0;
      }
      if (!str_found)
	nakade = 0;
    }
    if (nakade)
      return 1;
  }
  
  return 0;
}


/*
 * This helper returns 1 if str is part of a 4 stone string having a
 * bent four in the corner shape.
 *
 * |x???
 * |Oxx?
 * |OOOx
 * +----
 *
 */

int
bent_four_helper(int str)
{
  int stones[4];
  int good_corner_found = 0;
  int color = board[str];
  int k;

  if (!IS_STONE(color))
    return 0;

  if (findstones(str, 4, stones) != 4)
    return 0;

  /* All stones must be on the edge. Also detect the presence of a
   * corner stone.
   */
  for (k = 0; k < 4; k++) {
    if (!is_edge_vertex(stones[k]))
      return 0;
    if (is_corner_vertex(stones[k])) {
      int corner = stones[k];
      if ((board[EAST(corner)] == color)
	  + (board[SOUTH(corner)] == color)
	  + (board[WEST(corner)] == color)
	  + (board[NORTH(corner)] == color) == 2)
	good_corner_found = 1;
    }
  }

  return good_corner_found;
}


/*
 * LOCAL Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
