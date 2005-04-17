/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * This is GNU Go, a Go program. Contact gnugo@gnu.org, or see       *
 * http://www.gnu.org/software/gnugo/ for more information.          *
 *                                                                   *
 * Copyright 1999, 2000, 2001, 2002, 2003 and 2004                   *
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

#include <stdio.h>
#include "liberty.h"
#include "patterns.h"
#include "old-board.h"


#define TRYMOVE(goban, pos, color) trymove(goban, pos, color, "helper", NO_MOVE)
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
  
  return !doing_scoring && stones_on_board(goban, OTHER_COLOR(color)) == 0;
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
	  && safe_move(goban, move, color));
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
  findlib(goban, apos, 2, libs);
  if (libs[0] != move)
    bpos = libs[0];
  else
    bpos = libs[1];
  
  if (TRYMOVE(goban, move, color)) {
    if (!attack(goban, cpos, NULL)
	&& !(ON_BOARD(goban, dpos) && attack(goban, dpos, NULL))) {
      if (TRYMOVE(goban, bpos, other)) {
	if (attack(goban, apos, NULL))
	  success = 1;
	popgo(goban);
      }
      else {
	success = 1; /* X move at (bpos) would have been suicide */
      }
    }
    popgo(goban);
  }

  /* The followup is to capture the "a" string. Estimate the value to
   * twice the size.
   */
  add_followup_value(move, 2 * worm[apos].effective_size);
  TRACE(goban, "...followup value %f\n", 2 * worm[apos].effective_size);

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
  
  adj = chainlinks(goban, str, adjs);
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

  if (TRYMOVE(goban, dpos, board[apos])) {
    if (!board[bpos] || attack(goban, bpos, NULL)
	|| !board[cpos] || attack(goban, cpos, NULL)
	|| safe_move(goban, move, board[apos]) != 0) {
      popgo(goban);
      worm[worm[apos].origin].cutstone2++;
      propagate_worm(worm[apos].origin);
      return 0;
    }
    popgo(goban);
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
  ASSERT1(goban, (color == BLACK || color == WHITE), move);
  
  if (TRYMOVE(goban, move, color)) {
    ASSERT1(goban, countlib(goban, move) == 2, move);
    success = connect_and_cut_helper(move, apos, bpos);
    popgo(goban);
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
  TRACE(goban, "...followup value %f\n", 2.0 + 2.0 * worm[str].effective_size);
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
  TRACE(goban, "...reverse followup value %f\n", 2.0 * worm[str].effective_size);
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
  
  adj = chainlinks2(goban, str, adjs, 1);
  for (k = 0; k < adj; k++)
    if (worm[adjs[k]].defense_codes[0] != 0
	&& !does_defend(goban, move, adjs[k]))
      return;

  if (!TRYMOVE(goban, move, OTHER_COLOR(board[str])))
    return;
  if (find_defense(goban, str, &defense_move) != 0
      && defense_move != NO_MOVE
      && TRYMOVE(goban, defense_move, board[str])) {
    if (board[move] == EMPTY || attack(goban, move, NULL) != 0) {
      popgo(goban);
      popgo(goban);
      return;
    }
    popgo(goban);
  }

  /* In addition to the move found by find_defense(), also try all
   * chain breaking moves in the same way.
   */
  adj = chainlinks2(goban, str, adjs, 1);
  for (k = 0; k < adj; k++) {
    int lib;
    findlib(goban, adjs[k], 1, &lib);
    if (TRYMOVE(goban, lib, board[str])) {
      if (!attack(goban, str, NULL)
	  && (board[move] == EMPTY || attack(goban, move, NULL) != 0)) {
	popgo(goban);
	popgo(goban);
	return;
      }
      popgo(goban);
    }
  }

  popgo(goban);
  
  add_followup_value(move, 2.0 * worm[str].effective_size);
  TRACE(goban, "...followup value %f\n", 2.0 * worm[str].effective_size);
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

  ASSERT1(goban, countlib(goban, str) == 2, str);

  /* No value if the string can capture out of atari. */
  adj = chainlinks2(goban, str, adjs, 1);
  for (k = 0; k < adj; k++)
    if (worm[adjs[k]].defense_codes[0] != 0
	&& !does_defend(goban, move, adjs[k]))
      return;

  /* No value if opponent has no safe atari. */
  findlib(goban, str, 2, libs);
  if (!safe_move(goban, libs[0], OTHER_COLOR(board[str]))
      && !safe_move(goban, libs[1], OTHER_COLOR(board[str])))
    return;
  
  TRACE(goban, "...reverse followup value %f\n", 2.0 * worm[str].effective_size);
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
  int k;

  adj = chainlinks2(goban, pos, adjs, 1);
  for (k = 0; k < adj; k++) {
    int aa = adjs[k];
    int xx;

    if (countstones(goban, aa) == 1) {
      findlib(goban, aa, 1, &xx);
      if (is_ko(goban, xx, board[pos], NULL))
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

  liberties = findlib(goban, pos, 2, libs);
  ASSERT1(goban, liberties == 2, pos);

  for (k = 0; k < liberties; k++) {
    int aa = libs[k];
    if (is_ko(goban, aa, OTHER_COLOR(board[pos]), NULL))
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

  if (TRYMOVE(goban, apos, color)) {
    if (TRYMOVE(goban, bpos, other)) {
      if (attack(goban, cpos, NULL) && find_defense(goban, cpos, &dpos)) {
	set_minimum_move_value(dpos, 0.1);
	TRACE(goban, "%o...setting min move value of %1m to 0.1\n", dpos);
      }
      popgo(goban);
    }
    popgo(goban);
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
  int liberties = findlib(goban, Apos, 2, libs);
  int result = 0;
  int k;

  gg_assert(goban, IS_STONE(color));
  gg_assert(goban, liberties == 2);

  if (libs[0] == bpos)
    dpos = libs[1];
  else
    dpos = libs[0];

  for (k = 0; k < 4; k++)
    if (board[cpos + delta[k]] == color
	&& neighbor_of_string(goban, cpos + delta[k], Apos)) {
      epos = cpos + delta[k];
      break;
    }

  gg_assert(goban, epos != NO_MOVE);
  
  if (TRYMOVE(goban, bpos, color)) {
    if (TRYMOVE(goban, dpos, other)) {
      if (TRYMOVE(goban, cpos, other)) {
	if (board[bpos] == EMPTY
	    || board[epos] == EMPTY
	    || !defend_both(goban, bpos, epos))
	  result = 1;
	popgo(goban);
      }
      popgo(goban);
    }
    popgo(goban);
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

  gg_assert(goban, IS_STONE(color));


  if (TRYMOVE(goban, Apos, color)) {
    for (k = 0; k < 4; k++)
      if (board[cpos + delta[k]] == other
	  && neighbor_of_string(goban, cpos + delta[k], Apos)) {
	epos = cpos + delta[k];
	break;
      }

    gg_assert(goban, epos != NO_MOVE);
    
    if (TRYMOVE(goban, bpos, other)) {
      if (!find_defense(goban, Apos, &dpos) || dpos == NO_MOVE) {
	popgo(goban);
	popgo(goban);
	return 0;
      }
      
      if (TRYMOVE(goban, dpos, color)) {
	if (TRYMOVE(goban, cpos, color)) {
	  if (board[bpos] == EMPTY
	      || board[epos] == EMPTY
	      || !defend_both(goban, bpos, epos))
	    result = 1;
	  popgo(goban);
	}
	popgo(goban);
      }
      popgo(goban);
    }
    popgo(goban);
  }
  
  return result;
}



void
test_attack_either_move(int move, int color, int worma, int wormb)
{
  ASSERT_ON_BOARD1(goban, move);
  ASSERT1(goban, board[move] == EMPTY, move);
  ASSERT1(goban, board[worma] == OTHER_COLOR(color)
          && board[wormb] == OTHER_COLOR(color), move);

  if (!defend_both(goban, worma, wormb)) {
    if (0)
      gprintf(goban, "%1m: Reject attack_either_move for %1m, %1m (can't defend both)\n",
	      move, worma, wormb);
    return;
  }
  if (trymove(goban, move, color, "test_attack_either_move", worma)) {
    if (board[worma] == OTHER_COLOR(color)
	&& board[wormb] == OTHER_COLOR(color)) {
      if (!find_defense(goban, worma, NULL)
	  || !find_defense(goban, wormb, NULL)) {
	if (0)
	  gprintf(goban, "%1m: Rej. attack_either_move for %1m & %1m (regular attack)\n",
		  move, worma, wormb);
      }
      else if (!defend_both(goban, worma, wormb))
        add_either_move(move, ATTACK_STRING, worma, ATTACK_STRING, wormb);
      else {
	if (0)
	  gprintf(goban, "%1m: Rej. attack_either_move for %1m & %1m (doesn't work)\n",
		  move, worma, wormb);
      }
    }
    else
      if (0)
	gprintf(goban, "%1m: Rej. attack_either_move for %1m & %1m (captured directly)\n",
		move, worma, wormb);
    popgo(goban);
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

  adj = chainlinks2(goban, str, adjs, 1);
  for (k = 0; k < adj; k++)
    if (attack(goban, adjs[k], NULL))
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

  adj = chainlinks2(goban, str, adjs, 1);
  for (k = 0; k < adj; k++)
    if (attack_and_defend(goban, adjs[k], NULL, NULL, NULL, NULL))
      return 1;
  
  return 0;
}

void
backfill_replace(int move, int str)
{
  int defense_move = NO_MOVE;

  if (TRYMOVE(goban, move, OTHER_COLOR(board[str]))) {
    if (attack_and_defend(goban, str, NULL, NULL, NULL, &defense_move)) {
      /* Must undo the trymove before adding the replacement move. */
      popgo(goban);
      add_replacement_move(move, defense_move);
    }
    else
      popgo(goban);
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
      || (stones_on_board(goban, BLACK | WHITE) > board_size * board_size * 2 / 5
	  && stones_on_board(goban, WHITE) > board_size * board_size / 5)
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
      && stones_on_board(goban, BLACK | WHITE) > 10
      && test_symmetry_after_move(goban, PASS_MOVE, EMPTY, 1))
    return 1;

  return 0;
}


/*
 * LOCAL Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
