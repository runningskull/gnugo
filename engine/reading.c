/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * This is GNU GO, a Go program. Contact gnugo@gnu.org, or see       *
 * http://www.gnu.org/software/gnugo/ for more information.          *
 *                                                                   *
 * Copyright 1999, 2000, 2001, 2002 and 2003                         *
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

#include "gnugo.h"

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>

#include "liberty.h"
#include "cache.h"
#include "patterns.h"
#include "gg_utils.h"

/* Size of array where candidate moves are stored. */
#define MAX_MOVES 50

#define ADD_CANDIDATE_MOVE(move, this_score, moves)\
  do {\
    int u;\
    for (u = 0; u < (moves).num; u++)\
      if ((moves).pos[u] == (move)) {\
	(moves).score[u] += this_score;\
	break;\
      }\
    if ((u == (moves).num) && ((moves).num < MAX_MOVES)) {\
      (moves).pos[(moves).num] = move;\
      (moves).score[(moves).num] = this_score;\
      ((moves).num)++;\
    }\
  } while (0)

#define REMOVE_CANDIDATE_MOVE(move, moves)\
  do {\
    int u, v;\
    for (u = 0; u < (moves).num; u++) {\
      if ((moves).pos[u] == (move)) {\
        for (v = u; v < (moves).num-1; v++) {\
	  (moves).pos[v] = (moves).pos[v+1];\
	  (moves).score[v] = (moves).score[v+1];\
	}\
        ((moves).num)--;\
	break;\
      }\
    }\
  } while (0)


/* This macro checks whether the reported result is a loss, so we have won
 * and can exit, or else if it is the best result so far.
 * Note that SGFTRACE must have been setup.
 */
#define CHECK_RESULT(savecode, savemove, code, move_pos, move_ptr, \
    	             trace_message) \
  do {\
    if (code == 0) {\
      if (move_ptr) \
	*(move_ptr) = (move_pos); \
      SGFTRACE(move_pos, WIN, trace_message); \
      return WIN; \
    } \
    else if (REVERSE_RESULT(code) > savecode) {\
      savemove = move_pos; \
      savecode = REVERSE_RESULT(code); \
    } \
  } while (0)

/* Reverse of CHECK_RESULT, for results passed from a helper function. */
#define CHECK_RESULT_UNREVERSED(savecode, savemove, code, move_pos, move_ptr, \
    	             		trace_message) \
	CHECK_RESULT(savecode, savemove, REVERSE_RESULT(code), move_pos, \
	    	     move_ptr, trace_message)


#define RETURN_RESULT(savecode, savemove, move_ptr, trace_message) \
  do {\
    if (savecode) {\
      if (move_ptr)\
	*(move_ptr) = (savemove); \
      SGFTRACE(savemove, savecode, trace_message); \
    }\
    else \
      SGFTRACE(0, 0, NULL); \
    return savecode; \
  } while (0)


struct reading_moves
{
  int pos[MAX_MOVES];
  int score[MAX_MOVES];
  int num;
};

/*
 * The functions in reading.c are used to read whether groups 
 * can be captured or not. See the Texinfo documentation 
 * (Reading) for more information.
 *
 * NULL POINTERS: Many functions in this file can use pointers
 * to return the locations of recommended plays. These can be
 * set NULL in which case these values are not returned.
 */

static int do_find_defense(int str, int *move, int komaster, int kom_pos);
static int defend1(int str, int *move, int komaster, int kom_pos);
static int defend2(int str, int *move, int komaster, int kom_pos);
static int defend3(int str, int *move, int komaster, int kom_pos);
static int defend4(int str, int *move, int komaster, int kom_pos);
static void special_rescue_moves(int str, int lib,
    				 struct reading_moves *moves);
static void special_rescue2_moves(int str, int libs[2],
    				  struct reading_moves *moves);
static void special_rescue3_moves(int str, int libs[3],
				  struct reading_moves *moves);
static void hane_rescue_moves(int str, int libs[4],
			      struct reading_moves *moves);
static void special_rescue5_moves(int str, int libs[3],
    				  struct reading_moves *moves);
static void special_rescue6_moves(int str, int libs[3],
    				  struct reading_moves *moves);
static void set_up_snapback_moves(int str, int lib,
    				  struct reading_moves *moves);
static void edge_clamp_moves(int str, struct reading_moves *moves);
static int do_attack(int str, int *move, int komaster, int kom_pos);
static int attack1(int str, int *move, int komaster, int kom_pos);
static int attack2(int str, int *move, int komaster, int kom_pos);
static int attack3(int str, int *move, int komaster, int kom_pos);
static int attack4(int str, int *move, int komaster, int kom_pos);
static int find_cap2(int str, int alib, int blib, int *move,
		     int komaster, int kom_pos);
static int find_cap(int str, int *move, int komaster, int kom_pos);
static int special_attack2(int str, int libs[2], int *move,
			   int komaster, int kom_pos);
static int special_attack3(int str, int libs[2], int *move,
			   int komaster, int kom_pos);
static int special_attack4(int str, int libs[2], int *move,
			   int komaster, int kom_pos);
static int draw_back(int str, int *move, int komaster, int kom_pos);
static int edge_closing_backfill(int str, int apos, int *move);
static void edge_block_moves(int str, int apos,
			     struct reading_moves *moves);
static void propose_edge_moves(int str, int *libs, int liberties,
			       struct reading_moves *moves, int color);
static void break_chain_moves(int str, struct reading_moves *moves);
static void break_chain2_efficient_moves(int str, 
					 struct reading_moves *moves);
static void do_find_break_chain2_efficient_moves(int str, int adj,
						 struct reading_moves *moves);
static void break_chain2_moves(int str, struct reading_moves *moves,
			       int require_safe);
static void break_chain2_defense_moves(int str, struct reading_moves *moves);
static void break_chain3_moves(int str, struct reading_moves *moves);
static void superstring_breakchain_moves(int str, int liberty_cap,
					 struct reading_moves *moves);
static void double_atari_chain2_moves(int str,
    				      struct reading_moves *moves);
static void order_moves(int str, struct reading_moves *moves,
			int color, const char *funcname, int first_move);
static int simple_ladder_attack(int str, int *move, int komaster, int kom_pos);
static int simple_ladder_defend(int str, int *move, int komaster, int kom_pos);
static int in_list(int move, int num_moves, int *moves);


/* Statistics. */
static int reading_node_counter = 0;
static int nodes_when_called = 0;


/* ================================================================ */  
/*                          Goal functions                          */
/* ================================================================ */


/*
 * These functions define goals for the reading process.  They use 
 * the rest of the reading machinery to evaluate whether the goal
 * is fulfillable.
 *
 * The simplest goals are defined by attack() and find_defense(),
 * namely to see if it is possible to capture or defend a single
 * string.  More complex goals are defined by e.g. attack_either()
 * or defend_both().
 *
 * The functions in this section and the next are the only ones which are
 * callable from outside this file.  
 */


/* attack(str, *move) determines if the string at (str) can be 
 * captured, and if so, (*move) returns the attacking move, unless
 * (move) is a null pointer. Use a null pointer if you are interested
 * in the result of the attack but not the attacking move itself.
 *
 * Return WIN if the attack succeeds unconditionally, 0 if it doesn't.
 * Returns KO_A or KO_B if the result depends on ko: 
 *   - Returns KO_A if the attack succeeds provided attacker is willing to
 *     ignore any ko threat (the attacker makes the first ko capture).
 *   - Returns KO_B if attack succeeds provided attacker has a ko threat
 *     which must be answered (the defender makes the first ko capture).
 */

int
attack(int str, int *move)
{
  int result;
  int nodes;
  int origin;
  int the_move = NO_MOVE;

  nodes_when_called = reading_node_counter;
  /* Don't even spend time looking in the cache if there are more than
   * four liberties.
   */
  if (countlib(str) > 4)
    return 0;

  origin = find_origin(str);
  if (search_persistent_reading_cache(ATTACK, origin, &result, &the_move)) {
    if (move)
      *move = the_move;
    return result;
  }

  memset(shadow, 0, sizeof(shadow));
  result = do_attack(str, &the_move, EMPTY, 0);
  nodes = reading_node_counter - nodes_when_called;

  if (debug & DEBUG_READING_PERFORMANCE) {
    if (reading_node_counter - nodes_when_called
	>= MIN_READING_NODES_TO_REPORT) {
      if (result != 0)
	gprintf("%oattack %1m(%1m) = %d %1M, %d nodes ", str, origin, result,
		the_move, nodes);
      else
	gprintf("%oattack %1m(%1m) = %d, %d nodes ", str, origin, result,
		nodes);
      dump_stack();
    }
  }

  store_persistent_reading_cache(ATTACK, origin, result, the_move, nodes);
  
  if (move)
    *move = the_move;
  return result;
}


/* find_defense(str, *move) attempts to find a move that will save
 * the string at (str). It returns WIN if such a move is found, with
 * (*move) the location of the saving move, unless (move) is a
 * null pointer. It is not checked that tenuki defends, so this may 
 * give an erroneous answer if !attack(str).
 * 
 * Returns KO_A or KO_B if the result depends on ko. Returns KO_A if the
 * string can be defended provided the defender is willing to ignore
 * any ko threat. Returns KO_B if the defender wins by having a ko threat
 * which must be answered.
 */

int 
find_defense(int str, int *move)
{
  int result;
  int nodes;
  int origin;
  int the_move = NO_MOVE;

  nodes_when_called = reading_node_counter;
  /* Don't even spend time looking in the cache if there are more than
   * four liberties.
   */
  if (countlib(str) > 4) {
    if (move)
      *move = NO_MOVE;
    return WIN;
  }

  origin = find_origin(str);
  if (search_persistent_reading_cache(FIND_DEFENSE, origin, 
				      &result, &the_move)) {
    if (move)
      *move = the_move;
    return result;
  }

  memset(shadow, 0, sizeof(shadow));
  result = do_find_defense(str, &the_move, EMPTY, 0);
  nodes = reading_node_counter - nodes_when_called;

  if (debug & DEBUG_READING_PERFORMANCE) {
    if (reading_node_counter - nodes_when_called
	>= MIN_READING_NODES_TO_REPORT) {
      if (result != 0)
	gprintf("%odefend %1m(%1m) = %d %1M, %d nodes ", str, origin, result,
		the_move, nodes);
      else
	gprintf("%odefend %1m(%1m) = %d, %d nodes ", str, origin, result,
		nodes);
      dump_stack();
    }
  }

  store_persistent_reading_cache(FIND_DEFENSE, origin, result, 
				 the_move, nodes);
  
  if (move)
    *move = the_move;
  return result;
}


/* attack_and_defend(str, &acode, &attack_point,
 *                        &dcode, &defense_point)
 * is a frontend to the attack() and find_defense() functions, which
 * guarantees a consistent result. If a string cannot be attacked, 0
 * is returned and acode is 0. If a string can be attacked and
 * defended, WIN is returned, acode and dcode are both non-zero, and
 * (attack_point), (defense_point) both point to vertices on the board. 
 * If a string can be attacked but not defended, 0 is again returned, 
 * acode is non-zero, dcode is 0, and (attack_point) points to a vertex 
 * on the board.
 *
 * This function in particular guarantees that if there is an attack,
 * it will never return (defense_point) = NO_MOVE, which means the string is
 * safe without defense. Separate calls to attack() and find_defense()
 * may occasionally give this result, due to irregularities introduced
 * by the persistent reading cache.
 */
int
attack_and_defend(int str,
		  int *attack_code, int *attack_point,
		  int *defend_code, int *defense_point)
{
  int acode = 0;
  int apos = NO_MOVE;
  int dcode = 0;
  int dpos = NO_MOVE;

  acode = attack(str, &apos);
  if (acode != 0) {
    dcode = find_defense(str, &dpos);
    
    /* If find_defense() says the string is safe as is, contradicting
     * the opinion of attack(), we remove the corresponding entries
     * from the persistent cache and try again.
     */
    if (dcode == WIN && dpos == NO_MOVE) {
      delete_persistent_reading_cache_entry(ATTACK, str);
      delete_persistent_reading_cache_entry(FIND_DEFENSE, str);
      acode = attack(str, &apos);
      if (acode != 0)
        dcode = find_defense(str, &dpos);
    }
  }

  ASSERT1(!(acode != 0 && dcode == WIN && dpos == NO_MOVE), str);

  if (attack_code)
    *attack_code = acode;
  if (attack_point)
    *attack_point = apos;
  if (defend_code)
    *defend_code = dcode;
  if (defense_point)
    *defense_point = dpos;

  return acode != 0 && dcode != 0;
}


/*
 * attack_either(astr, bstr) returns true if there is a move which
 * guarantees that at least one of the strings (astr) and (bstr)
 * can be captured. A typical application for this is in connection
 * patterns, where after a cut it suffices to capture one of the cutting
 * stones.
 *
 * FIXME: The current implementation only looks for uncoordinated
 *        attacks. This is insufficient to find double ataris or 
 *        moves such as 'a' in positions like
 *
 *        XOOOOOOOX
 *        XOXXOXXOX
 *        XX..a..XX
 *        ---------
 *
 *        where neither of the threatened X stones can be captured right
 *        out. Still either can be captured by a move down to a.
 */

int
attack_either(int astr, int bstr)
{
  int asuccess = 0;
  int bsuccess = 0;
  int color = board[astr];
  ASSERT1(IS_STONE(color) , astr);
  ASSERT1(color == board[bstr], bstr);

  /* Start by attacking the string with the fewest liberties. On
   * average this seems to be slightly more efficient.
   */
  if (countlib(astr) > countlib(bstr)) {
    int t = astr;
    astr = bstr;
    bstr = t;
  }

  asuccess = attack(astr, NULL);
  if (asuccess == WIN)
    return asuccess;

  bsuccess = attack(bstr, NULL);
  if (asuccess || bsuccess) {
    return (asuccess > bsuccess) ? asuccess : bsuccess;
  }

  /* Try (a little) harder */
  {
    int libs[2];
    int alibs = findlib(astr, 2, libs);
    int defended0 = WIN;
    int defended1 = WIN;
    int other = OTHER_COLOR(color);
    /* Let's just try the case where the group with the fewest liberties
     * has only 2, and try each atari in turn.
     */
    if (alibs == 2) {
      if (trymove(libs[0], other, "attack_either-A", astr, EMPTY, NO_MOVE)) {
	defended0 = defend_both(astr, bstr);
	popgo();
      }
      if (defended0 
	  && trymove(libs[1], other, "attack_either-B", astr,
		     EMPTY, NO_MOVE)) {
	defended1 = defend_both(astr, bstr);
	popgo();
      }
    }
    return REVERSE_RESULT(gg_min(defended0, defended1));
  }

}


/*
 * defend_both(astr, bstr) returns true if both the strings (astr)
 * and (bstr) can be defended simultaneously or if there is no attack.
 * A typical application for this is in connection patterns, where
 * after a cut it's necessary to defend both cutting stones.
 *
 * FIXME: The current implementation only makes halfhearted
 * attempts to find coordinated defense moves. A proper implementation
 * would require some serious reading.
 */

int
defend_both(int astr, int bstr)
{
  int a_threatened = 0;
  int b_threatened = 0;
  int a_savepos;
  int b_savepos;
  int acode = 0;
  int dcode = 0;
  
  int color = board[astr];
  ASSERT1(IS_STONE(color) , astr);
  ASSERT1(color == board[bstr], bstr);

  attack_and_defend(astr, &acode, NULL, &dcode, &a_savepos);
  if (acode != 0) {
    a_threatened = 1;
    if (dcode == 0)
      return 0; /* (astr) already lost */
  }
  
  attack_and_defend(bstr, &acode, NULL, &dcode, &b_savepos);
  if (acode != 0) {
    b_threatened = 1;
    if (dcode == 0)
      return 0; /* (bstr) already lost */
  }

  /* Neither string can be attacked or only one of them, in which case
   * we have time to save it.
   */
  if (!a_threatened || !b_threatened)
    return WIN;
  
  /* If both strings are threatened we assume that one will become lost,
   * unless find_defense() happened to return the same defense point for
   * both (which e.g. may happen if they are in fact the same string).
   * This is still a bit too pessimistic, as there may be one move which
   * saves both strings. To do this right we should try each move which
   * defends either string and see if it also defends the other string.
   */

  if (a_savepos == b_savepos)
    return WIN; /* Both strings can be attacked but also defended 
                 * by one move. */

  /* We also try each of the returned defense points and see whether
   * the other string can still be attacked. This still gives a
   * somewhat pessimistic estimation.
   */

  if (trymove(a_savepos, color, "defend_both-A", astr, EMPTY, NO_MOVE)) {
    if (board[bstr] && !attack(bstr, NULL)) {
      popgo();
      return WIN;
    }
    popgo();
  }
  
  if (trymove(b_savepos, color, "defend_both-B", bstr, EMPTY, NO_MOVE)) {
    if (board[astr] && !attack(astr, NULL)) {
      popgo();
      return WIN;
    }
    popgo();
  }

  /* The next improvement is to try to attack a common adjacent string. */
  {
    int adjs1[MAXCHAIN];
    int neighbors1;
    int adjs2[MAXCHAIN];
    int neighbors2;
    int r;
    int s;
    int epos;
    int fpos;
    
    neighbors1 = chainlinks(astr, adjs1);
    neighbors2 = chainlinks(bstr, adjs2);
    
    for (r = 0; r < neighbors1; r++) {
      epos = adjs1[r];
      if (countlib(epos) <= 4
	  && (epos != a_savepos)
	  && (epos != b_savepos)) {
	/* Is (epos) also adjacent to (bstr)? */
	for (s = 0; s < neighbors2; s++) {
	  if (adjs2[s] == adjs1[r])
	    break;
	}
	if (s == neighbors2)
	  continue;   /* No, it wasn't. */

	if (attack(epos, &fpos)) {
	  if (trymove(fpos, color, "defend_both-C", astr, EMPTY, NO_MOVE)) {
	    if (board[astr] && board[bstr]
		&& !attack(astr, NULL) 
		&& !attack(bstr, NULL)) {
	      popgo();
	      return WIN;
	    }
	    popgo();
	  }
	}
      }
    }  
  }
  
  /* Both strings can be attacked but we have only time to defend one. */
  return 0;
}


/*
 * break_through(apos, bpos, cpos) returns WIN if a position can
 * succesfully be broken through and CUT if it can be cut. The position
 * is assumed to have the shape (the colors may be reversed)
 *
 * .O.       dbe
 * OXO       aFc
 *
 * It is X to move and try to capture at least one of a, b, and c. If
 * this succeeds, X is said to have broken through the position.
 * Otherwise X may try to cut through the position, which means
 * keeping F safe and getting a tactically safe string at either d or
 * e.
 *
 * Important notice: a, b, and c must be given in the correct order.
 *
 * FIXME: The reading involved here can most likely be improved.
 *
 * FIXME: We need to take ko results properly into account.
 */

static int
break_through_helper(int apos, int bpos, int cpos,
		     int dpos, int epos, int Fpos,
		     int color, int other);

int
break_through(int apos, int bpos, int cpos)
{
  int color = board[apos];
  int other = OTHER_COLOR(color);

  int dpos;
  int epos;
  int Fpos;
  int gpos;
  
  int success = 0;
  int success2 = 0;
  
  /* Basic sanity checking. */
  ASSERT1(IS_STONE(color) , apos);
  ASSERT1(color == board[bpos], bpos);
  ASSERT1(color == board[cpos], cpos);

  /* Construct the rest of the points in the pattern. */
  Fpos = (apos + cpos) / 2;      /* F midpoint between a and c. */
  dpos = apos + bpos - Fpos;     /* Use diagonal relation a+b = d+F. */
  epos = bpos + cpos - Fpos;     /* Use diagonal relation b+c = e+F. */

  /* More sanity checking. */
  ASSERT1(board[dpos] == EMPTY , dpos);
  ASSERT1(board[epos] == EMPTY , epos);

  /* F might already have been captured. (play_break_through_n() can't
   * detect this.
   */
  if (board[Fpos] == EMPTY)
    return 0;
  
  ASSERT1(board[Fpos] == other, Fpos);

  /* First X tries to play at d. */
  success = break_through_helper(apos, bpos, cpos, dpos, epos, Fpos,
				 color, other);
  if (success == WIN)
    return WIN;
  
  success2 = break_through_helper(cpos, bpos, apos, epos, dpos, Fpos,
				  color, other);

  if (success2 == WIN)
    return WIN;

  if (success2 == CUT)
    success = CUT;

  /* If we haven't been lucky yet, we might need to start by
   * defending F.
   *
   * FIXME: The function would probably be considerably faster if we
   * start by checking whether F needs defense. Beware of ko potential
   * though.
   */
  success2 = 0;
  if (attack_and_defend(Fpos, NULL, NULL, NULL, &gpos)) {
    if (trymove(gpos, other, "break_through-A", Fpos, EMPTY, NO_MOVE)) {
      /* Now we let O defend his position by playing either d or e.
       * FIXME: There may be other plausible moves too.
       */
      if (trymove(dpos, color, "break_through-B", Fpos, EMPTY, NO_MOVE)) {
	/* O connects at d, so X cuts at e. */
	if (safe_move(epos, other)) {
	  success2 = CUT;
	  if (!board[cpos] || attack(cpos, NULL))
	    success2 = WIN;
	}
	popgo();
      }

      if (success2 > 0 && trymove(epos, color, "break_through-C", Fpos,
				  EMPTY, NO_MOVE)) {
	/* O connects at e, so X cuts at d. */
	if (safe_move(dpos, other)) {
	  /* success2 is already WIN or CUT. */
	  if (board[apos] && !attack(apos, NULL))
	    success2 = CUT;
	}
	else
	  success2 = 0;
	popgo();
      }
      popgo();
    }
  }
    
  if (success2 > 0)
    return success2;

  return success;
}

/* Helper function for break_through(). Since we can symmetrically
 * start by cutting at d or e, we use the same code for both attacks,
 * simply switching positions between the two calls.
 */
static int
break_through_helper(int apos, int bpos, int cpos,
		     int dpos, int epos, int Fpos,
		     int color, int other)
{
  int success = 0;
  int gpos;

  if (trymove(dpos, other, "break_through_helper-A", Fpos, EMPTY, NO_MOVE)) {
    /* If F can be attacked we can't start in this way. */
    if (!attack(Fpos, NULL)) {
      /* If d is safe too, we have at least managed to break through. */
      if (!attack(dpos, &gpos))
	success = CUT;

      /* Too bad, d could be attacked. We let O play the attack and
       * then try to make a second cut at e. But first we must test if
       * O at e is sufficient to capture d.
       */
      else {
	if (trymove(epos, color, "break_through_helper-E", Fpos,
		    EMPTY, NO_MOVE)) {
	  if (!board[dpos] || !find_defense(dpos, NULL)) {
	    popgo();
	    popgo();
	    return 0;
	  }
	  popgo();
	}
	
	if (gpos == epos) {
	  popgo();
	  return 0;
	}

	if (trymove(gpos, color, "break_through_helper-F", Fpos,
		    EMPTY, NO_MOVE)) {
	  if (trymove(epos, other, "break_through_helper-G", Fpos,
		      EMPTY, NO_MOVE)) {
	    if (!attack(epos, NULL)) {
	      success = CUT;
     	      /* Make sure b and c are safe.  If not, back up & let O try 
	       * to defend in a different way. */
	      if (board[bpos] 
		  && board[cpos] 
		  && defend_both(bpos, cpos)) {
		/* Can't do better than CUT. */
		popgo();  
		popgo();
		popgo();
		return CUT;
	      }
	    }
	    else {
	      /* Lost everything. (Note we ignore ko at the moment.) */
	      popgo();
	      popgo();
	      popgo();
	      return 0;
	    }
	    popgo();
	  }
	  popgo();
	}
      }
      
      /* By now, we're sure a cut works, so now we can try 
       * to capture something. */
	if (!board[apos] || !board[bpos] || !defend_both(apos, bpos))
	  success = WIN;
	else {
	  /* Both a and b could be defended, or didn't need to be.
	   * Let's see if a move at e is sufficient for O.
	   */
	  int attack_on_b = 0;
	  int attack_on_a = 0;

	  if (trymove(epos, color, "break_through_helper-B", Fpos, 
		      EMPTY, NO_MOVE)) {
	    if (attack(bpos, NULL))
	      attack_on_b = 1;
	    else if (attack(apos, NULL))
	      attack_on_a = 1;
	    popgo();
	  }

	  /* Let O find a defense and play it. */
	  if (attack_on_a || attack_on_b) {
	    int hpos = NO_MOVE;

	    if (((attack_on_a && find_defense(apos, &hpos))
		|| (attack_on_b && find_defense(bpos, &hpos)))
		&& hpos != NO_MOVE
		&& trymove(hpos, color, "break_through_helper-C", Fpos,
			   EMPTY, NO_MOVE)) {
	      /* Now we make a second cut at e, trying to capture
	       * either b or c.
	       */
	      if (trymove(epos, other, "break_through_helper-D", Fpos,
			  EMPTY, NO_MOVE)) {
		if (!board[bpos]
		    || !board[cpos] 
		    || !defend_both(bpos, cpos))
		  success = WIN;
		popgo();
	      }
	      popgo();
	    }
	    else
	      success = WIN; /* This should have been covered by
			      * defend_both(), so probably unnecessary. */
	}
      }
    }
    popgo();
  }

  return success;
}


/* ---------------------------------------------------------------- */
/*                              Threats                             */
/* ---------------------------------------------------------------- */


/* Return up to max_threats threats to capture the string at str.
 * If the string is directly attackable the number of threats
 * is reported to be 0.
 *
 * NOTE:  You can call attack_threats with moves[] and codes[] 
 *        already partly filled in. So if you want to get the
 *        threats from scratch, you have to set them to 0
 *        yourself.
 *
 * FIXME: Shall we report upgrades, like we can capture in ko but
 *        have a threat to capture unconditionally?
 */

int
attack_threats(int str, int max_points, int moves[], int codes[])
{
  int other;
  int num_threats;
  int liberties;
  int libs[MAXLIBS];
  int num_adj;
  int adjs[MAXCHAIN];
  int k;
  int l;
  int r;

  ASSERT1(IS_STONE(board[str]), str);
  other = OTHER_COLOR(board[str]);

  /* Only handle strings with no way to capture immediately.
   * For now, we treat ko the same as unconditionally. */
  if (attack(str, NULL) != 0)
    return 0;

  /* This test would seem to be unnecessary since we only threaten
   * strings with attack_code == 0, but it turns out that single
   * stones with one liberty that can be captured, but come to
   * live again in a snap-back get attack_code == 0.
   *
   * The test against 6 liberties is just an optimization.
   */
  liberties = findlib(str, MAXLIBS, libs);
  if (liberties > 1 && liberties < 6) {
    for (k = 0; k < liberties; k++) {
      int aa = libs[k];

      /* Try to threaten on the liberty. */
      if (trymove(aa, other, "attack_threats-A", str, EMPTY, NO_MOVE)) {
       int acode = attack(str, NULL);
       if (acode != 0)
	 movelist_change_point(aa, acode, max_points, moves, codes);
       popgo();
      }

      /* Try to threaten on second order liberties. */
      for (l = 0; l < 4; l++) {
       int bb = libs[k] + delta[l];

       if (!ON_BOARD(bb)
           || IS_STONE(board[bb])
           || liberty_of_string(bb, str))
         continue;

       if (trymove(bb, other, "attack_threats-B", str, EMPTY, NO_MOVE)) {
         int acode = attack(str, NULL);
         if (acode != 0)
	   movelist_change_point(bb, acode, max_points, moves, codes);
         popgo();
       }
      }
    }
  }

  /* Threaten to attack by saving weak neighbors. */
  num_adj = chainlinks(str, adjs);
  for (k = 0; k < num_adj; k++) {
    int bb;
    int dd;  /* Defense point of weak neighbor. */
    int acode;
    int dcode;

    attack_and_defend(adjs[k], &acode, NULL, &dcode, &dd);
    if (acode == 0 || dcode == 0)
      continue;

    /* The strange code using r == -1 below is only avoid duplication
     * of the code starting with "if (trymove..)" below.
     * If r == -1 and stackp == 0 then use the defense point what we got from
     * attack_and_defend above. Otherwise step through all defense points.
     */
    for (r = -1; r < max_points; r++) {
      if (stackp == 0) {
	if (r == -1)
	  continue;
	if (worm[adjs[k]].defense_codes[r] == 0)
	  break;
	bb = worm[adjs[k]].defense_points[r];
      }
      else {
	if (r == -1)
	  bb = dd;
	else
	  break;
      }

      /* Test the move and see if it is a threat. */
      if (trymove(bb, other, "attack_threats-C", str, EMPTY, NO_MOVE)) {
	if (board[str] == EMPTY)
	  acode = WIN;
	else
	  acode = attack(str, NULL);
	if (acode != 0)
	  movelist_change_point(bb, acode, max_points, moves, codes);
	popgo();
      }
    }
  }

  /* Return actual number of threats found regardless of attack code. */
  if (codes[max_points - 1] > 0)
    return max_points;
  for (num_threats = 0; num_threats < max_points; num_threats++)
    if (codes[num_threats] == 0)
      break;
  return num_threats;
}


/* ================================================================ */  
/*                       Defensive functions                        */
/* ================================================================ */


/* Like find_defense, but takes the komaster argument. If the
 * opponent is komaster, reading functions will not try
 * to take ko.
 */

static int
do_find_defense(int str, int *move, int komaster, int kom_pos)
{
  int xpos;
  int dcode = 0;
  int liberties;
  int found_read_result;
  Read_result *read_result = NULL;
  
  SETUP_TRACE_INFO("find_defense", str);
  
  /* We first check if the number of liberties is larger than four. In
   * that case we don't cache the result and to avoid needlessly
   * storing the position in the hash table, we must do this test
   * before we look for cached results.
   */
  liberties = countlib(str);
  
  if (liberties > 4
      || (liberties == 4 && stackp > fourlib_depth)
      || (liberties == 3 && stackp > depth)) {
    /* No need to cache the result in these cases. */
    SGFTRACE(0, WIN, "too many liberties or stackp > depth");
    if (move)
      *move = 0;
    return WIN;
  }

  if ((stackp <= depth) && (hashflags & HASH_FIND_DEFENSE)) {
    found_read_result = get_read_result(FIND_DEFENSE, komaster, kom_pos, 
					&str, &read_result);
    if (found_read_result) {
      TRACE_CACHED_RESULT(*read_result);
      if (rr_get_result(*read_result) != 0)
	if (move)
	  *move = rr_get_move(*read_result);

      SGFTRACE(rr_get_move(*read_result),
	       rr_get_result(*read_result), "cached");
      return rr_get_result(*read_result);
    }
  }

  if (liberties == 1)
    dcode = defend1(str, &xpos, komaster, kom_pos);
  else if (liberties == 2)
    dcode = defend2(str, &xpos, komaster, kom_pos);
  else if (liberties == 3)
    dcode = defend3(str, &xpos, komaster, kom_pos);
  else if (liberties == 4)
    dcode = defend4(str, &xpos, komaster, kom_pos);

  if (dcode) {
    READ_RETURN(read_result, move, xpos, dcode);
  }
    
  READ_RETURN0(read_result);
}


/* If str points to a string with exactly one liberty, defend1 
 * determines whether it can be saved by extending or capturing
 * a boundary chain having one liberty. The function returns WIN if the string
 * can be saved, otherwise 0. It returns KO_A or KO_B if it can be saved,
 * conditioned on ko. Returns KO_A if it can be saved provided (color) is
 * willing to ignore any ko threat. Returns KO_B if it can be saved if (color)
 * has a ko threat which must be answered.
 *
 * The pair defend1-attack2 call each other recursively to
 * read situations such as ladders. They read all ladders to the end.
 * If the reading ply (stackp) is deeper than the deep-reading cutoff
 * parameter depth, whose default value DEPTH is defined in gnugo.h, then a
 * string is assumed alive if it can get 3 liberties. When
 * fourlib_depth < stackp < depth, a string is considered alive if it can get
 * four liberties. When stackp < fourlib_depth, it is considered alive
 * if it can get 5 liberties.
 * */

static int
defend1(int str, int *move, int komaster, int kom_pos)
{
  int color = board[str];
  int other = OTHER_COLOR(color);
  int xpos;
  int lib;
  struct reading_moves moves;
  int savemove = 0;
  int savecode = 0;
  int liberties;
  int k;

  SETUP_TRACE_INFO("defend1", str);
  reading_node_counter++;
  
  ASSERT1(IS_STONE(board[str]), str);
  ASSERT1(countlib(str) == 1, str);

  /* lib will be the liberty of the string. */
  liberties = findlib(str, 1, &lib);
  ASSERT1(liberties == 1, str);

  /* Collect moves to try in the first batch.
   * 1. First order liberty.
   * 2. Chain breaking moves.
   * 3. Moves to set up a snapback.
   */
  moves.pos[0] = lib;
  moves.score[0] = 0;
  moves.num = 1;
  
  break_chain_moves(str, &moves);
  set_up_snapback_moves(str, lib, &moves);

  order_moves(str, &moves, color, read_function_name, 0);

  for (k = 0; k < moves.num; k++) {
    int new_komaster;
    int new_kom_pos;
    int ko_move;

    xpos = moves.pos[k];
    if (komaster_trymove(xpos, color, "defend1-A", str, komaster, kom_pos,
			 &new_komaster, &new_kom_pos,
			 &ko_move, stackp <= ko_depth && savecode == 0)) {
      if (!ko_move) {
	int acode = do_attack(str, NULL, new_komaster, new_kom_pos);
	popgo();
	CHECK_RESULT(savecode, savemove, acode, xpos, move,
		     "defense effective - A");
      }
      else {
	if (do_attack(str, NULL, new_komaster, new_kom_pos) != WIN) {
	  savemove = xpos;
	  savecode = KO_B;
	}
	popgo();
      }
    }
  }

  /* If the string is a single stone and a capture would give a ko,
   * try to defend it with ko by backfilling.
   *
   * FIXME: What is an example of this? Is it correct that the
   *           return value is WIN and not KO_A or KO_B?
   */
  if (stackp <= backfill_depth
      && countstones(str) == 1
      && is_ko(lib, other, NULL)) {
    int libs2[6];
    liberties = approxlib(lib, color, 6, libs2);
    if (liberties <= 5) {
      for (k = 0; k < liberties; k++) {
	int apos = libs2[k];
	if ((liberties == 1 || !is_self_atari(apos, other))
	    && trymove(apos, color, "defend1-C", str, komaster, kom_pos)) {
	  int acode = do_attack(str, NULL, komaster, kom_pos);
	  popgo();
	  CHECK_RESULT(savecode, savemove, acode, apos, move, "backfilling");
	}
      }
    }
  }
  
  RETURN_RESULT(savecode, savemove, move, "saved move");
}



/* If str points to a group with two liberties, defend2 determines
 * whether the group can be saved by extending, or by capturing part of
 * its surrounding chain. A group is considered safe if either part of
 * the surrounding chain may be captured, or if it can get 3
 * liberties. It is presumed that the opponent could kill if tenuki.
 * If both extensions work, it prefers the one which maximizes 
 * liberties.
 *
 * *move returns the move to save the stones.
 */

static int 
defend2(int str, int *move, int komaster, int kom_pos)
{
  int color, other;
  int xpos;
  int liberties;
  int libs[2];
  int liberties2;
  int libs2[6];
  struct reading_moves moves;
  int saved_num_moves;
  int savemove = 0;
  int savecode = 0;
  int k;
  int r;

  SETUP_TRACE_INFO("defend2", str);
  reading_node_counter++;

  color = board[str];
  other = OTHER_COLOR(color);

  gg_assert(IS_STONE(board[str]));
  gg_assert(countlib(str) == 2);

  liberties = findlib(str, 2, libs);
  ASSERT1(liberties == 2, str);

  /* Collect moves to try in the first batch.
   * 1. First order liberties.
   * 2. Chain breaking moves.
   * 3. Second order liberties moving up from first line to second.
   * 4. Edge clamps.
   */
  for (k = 0; k < liberties; k++) {
    moves.pos[k] = libs[k];
    moves.score[k] = 0;
  }
  moves.num = liberties;
  
  break_chain_moves(str, &moves);
  break_chain2_efficient_moves(str, &moves);
  propose_edge_moves(str, libs, liberties, &moves, color);
  edge_clamp_moves(str, &moves);

  if (stackp <= depth) {
    for (k = 0; k < liberties; k++)
      special_rescue_moves(str, libs[k], &moves);
  }

  if (stackp <= backfill_depth)
    special_rescue2_moves(str, libs, &moves);

  order_moves(str, &moves, color, read_function_name, 0);

  for (k = 0; k < moves.num; k++) {
    int new_komaster;
    int new_kom_pos;
    int ko_move;

    xpos = moves.pos[k];
    
    if (komaster_trymove(xpos, color, "defend2-A", str,
			 komaster, kom_pos, &new_komaster, &new_kom_pos,
			 &ko_move, stackp <= ko_depth && savecode == 0)) {
      if (!ko_move) {
	int acode = do_attack(str, NULL, new_komaster, new_kom_pos);
	popgo();
	CHECK_RESULT(savecode, savemove, acode, xpos, move,
	             "defense effective - A");
      }
      else {
	if (do_attack(str, NULL, new_komaster, new_kom_pos) != WIN) {
	  savemove = xpos;
	  savecode = KO_B;
	}
	popgo();
      }
    }
  }

  saved_num_moves = moves.num;

  /* Look for backfilling moves. */
  for (k = 0; k < liberties; k++) {
    if (is_self_atari(libs[k], other)) {
      liberties2 = approxlib(libs[k], color, 6, libs2);
      /* Note: liberties2 must be smaller than 5, otherwise libs[k] had been
       * a direct defense.
       */
      for (r = 0; r < liberties2; r++) {
	xpos = libs2[r];
	/* If the newly placed stone would be in atari, but not a single
	 * stone, we don't even try.
	 */
	if (!is_self_atari(xpos, color)
	    || has_neighbor(xpos, color))
	  ADD_CANDIDATE_MOVE(xpos, 0, moves);

      }
    }
    liberties2 = approxlib(libs[k], other, 3, libs2);
    if (liberties2 <= 2) {
      for (r = 0; r < liberties2; r++) {
	xpos = libs2[r];
	if (!is_self_atari(xpos, color))
	  ADD_CANDIDATE_MOVE(xpos, 0, moves);
      }
    }
  }

  /* Only order and test the new set of moves. */
  order_moves(str, &moves, other, read_function_name, saved_num_moves);
    
  for (k = saved_num_moves; k < moves.num; k++) {
    int new_komaster;
    int new_kom_pos;
    int ko_move;
    xpos = moves.pos[k];
    
    if (komaster_trymove(xpos, color, "defend2-B", str,
			 komaster, kom_pos, &new_komaster, &new_kom_pos,
			 &ko_move, stackp <= ko_depth && savecode == 0)) {
      if (!ko_move) {
	int acode = do_attack(str, NULL, new_komaster, new_kom_pos);
	popgo();
	CHECK_RESULT(savecode, savemove, acode, xpos, move,
		     "defense effective - B");
      }
      else {
	if (do_attack(str, NULL, new_komaster, new_kom_pos) != WIN) {
	  savemove = xpos;
	  savecode = KO_B;
	}
	popgo();
      }
    }
  }

  saved_num_moves = moves.num;

  if (stackp <= superstring_depth) {
    superstring_breakchain_moves(str, 4, &moves);
  }

  /* If nothing else works, we try playing a liberty of the
   * super_string.
   */
  if (stackp <= superstring_depth) {
    int ss_liberties;
    int ss_libs[MAX_LIBERTIES + 4];

    find_superstring_liberties(str, &ss_liberties, ss_libs, 3);
    for (k = 0; k < ss_liberties; k++) {
      int apos = ss_libs[k];
      
      if (liberty_of_string(apos, str))
	continue;
      /* If the newly placed would be in atari, we won't even try. */
      if (approxlib(apos, color, 2, NULL) > 1)
	ADD_CANDIDATE_MOVE(apos, 0, moves);
    }
    
    /* Now we are truly desperate. Try playing second order liberties of
     * the superstring.
     */
    for (k = 0; k < ss_liberties; k++) {
      int apos = ss_libs[k];

      if (liberty_of_string(apos, str))
	continue;

      special_rescue_moves(str, apos, &moves);
    }
  }

  break_chain2_defense_moves(str, &moves);

  if (stackp <= backfill_depth) {
    special_rescue5_moves(str, libs, &moves);
  }

  if (stackp <= backfill2_depth) {
    break_chain3_moves(str, &moves);
  }

  /* Only order and test the new set of moves. */
  order_moves(str, &moves, other, read_function_name, saved_num_moves);
    
  for (k = saved_num_moves; k < moves.num; k++) {
    int new_komaster;
    int new_kom_pos;
    int ko_move;
    xpos = moves.pos[k];
    
    if (komaster_trymove(xpos, color, "defend2-E", str,
			 komaster, kom_pos, &new_komaster, &new_kom_pos,
			 &ko_move, stackp <= ko_depth && savecode == 0)) {
      if (!ko_move) {
	int acode = do_attack(str, NULL, new_komaster, new_kom_pos);
	popgo();
	CHECK_RESULT(savecode, savemove, acode, xpos, move,
		     "defense effective - A");
      }
      else {
	if (do_attack(str, NULL, new_komaster, new_kom_pos) != WIN) {
	  savemove = xpos;
	  savecode = KO_B;
	}
	popgo();
      }
    }
  }

  if (savecode != 0)
    RETURN_RESULT(savecode, savemove, move, "saved move");

  RETURN_RESULT(savecode, savemove, move, NULL);
}


/* defend3(str, *move) attempts to find a move rescuing the 
 * string at (str) with 3 liberties.  If such a move can be found,
 * it returns true and the saving move in *move.
 */

static int 
defend3(int str, int *move, int komaster, int kom_pos)
{
  int color, other;
  int xpos;
  int liberties;
  int libs[3];
  struct reading_moves moves;
  int saved_num_moves;
  int savemove = 0;
  int savecode = 0;
  int k;

  SETUP_TRACE_INFO("defend3", str);
  reading_node_counter++;

  color = board[str];
  other = OTHER_COLOR(color);

  gg_assert(IS_STONE(board[str]));
  gg_assert(countlib(str) == 3);

  liberties = findlib(str, 3, libs);
  ASSERT1(liberties == 3, str);

  /* Collect moves to try in the first batch.
   * 1. First order liberties.
   * 2. Chain breaking moves.
   * 3. Second order liberties moving up from first line to second.
   * 4. Edge clamps.
   */
  for (k = 0; k < liberties; k++) {
    moves.pos[k] = libs[k];
    moves.score[k] = 0;
  }
  moves.num = liberties;
  
  break_chain_moves(str, &moves);
  break_chain2_efficient_moves(str, &moves);
  propose_edge_moves(str, libs, liberties, &moves, color);
  edge_clamp_moves(str, &moves);

  if (stackp <= backfill2_depth)
    hane_rescue_moves(str, libs, &moves);

  order_moves(str, &moves, color, read_function_name, 0);

  for (k = 0; k < moves.num; k++) {
    int new_komaster;
    int new_kom_pos;
    int ko_move;

    if (stackp >= branch_depth && k > 0)
      break;
    
    xpos = moves.pos[k];
    
    if (komaster_trymove(xpos, color, "defend3-A", str, komaster, kom_pos,
			 &new_komaster, &new_kom_pos,
			 &ko_move, stackp <= ko_depth && savecode == 0)) {
      if (!ko_move) {
	int acode = do_attack(str, NULL, new_komaster, new_kom_pos);
	popgo();
	CHECK_RESULT(savecode, savemove, acode, xpos, move,
	  	     "defense effective - A");
      }
      else {
	if (do_attack(str, NULL, new_komaster, new_kom_pos) != WIN) {
	  savemove = xpos;
	  savecode = KO_B;
	}
	popgo();
      }
    }
  }

  /* If nothing else works, try to defend with second order liberties. */

  saved_num_moves = moves.num;

  if (stackp <= backfill_depth)
    special_rescue3_moves(str, libs, &moves);

  if (stackp <= depth) {
    for (k = 0; k < liberties; k++)
      special_rescue_moves(str, libs[k], &moves);
  }

  if (level >= 10 && stackp <= backfill2_depth)
    superstring_breakchain_moves(str, 4, &moves);

  if (stackp <= backfill2_depth)
    break_chain2_defense_moves(str, &moves);

  if (stackp <= backfill_depth) {
    special_rescue5_moves(str, libs, &moves);
    special_rescue6_moves(str, libs, &moves);
  }

  /* Only order and test the new set of moves. */
  order_moves(str, &moves, other, read_function_name, saved_num_moves);
  
  for (k = saved_num_moves; k < moves.num; k++) {
    int new_komaster;
    int new_kom_pos;
    int ko_move;
    xpos = moves.pos[k];
    
    if (komaster_trymove(xpos, color, "defend3-E", str,
			 komaster, kom_pos, &new_komaster, &new_kom_pos,
			 &ko_move, stackp <= ko_depth && savecode == 0)) {
      if (!ko_move) {
	int acode = do_attack(str, NULL, new_komaster, new_kom_pos);
	popgo();
	CHECK_RESULT(savecode, savemove, acode, xpos, move,
		     "defense effective - A");
      }
      else {
	if (do_attack(str, NULL, new_komaster, new_kom_pos) != WIN) {
	  savemove = xpos;
	  savecode = KO_B;
	}
	popgo();
      }
    }
  }

  saved_num_moves = moves.num;

  /* If nothing else works, we try playing a liberty of the
   * super_string.
   */
  if (level >= 10 && stackp <= backfill2_depth) {
    int ss_liberties;
    int ss_libs[MAX_LIBERTIES + 4];

    find_superstring_liberties(str, &ss_liberties, ss_libs, 3);
    for (k = 0; k < ss_liberties; k++) {
      int apos = ss_libs[k];
	
      if (liberty_of_string(apos, str))
	continue;
      /* If the newly placed would be in atari, we won't even try. */
      if (approxlib(apos, color, 2, NULL) > 1)
	ADD_CANDIDATE_MOVE(apos, 0, moves);
    }

    /* Now we are truly desperate. Try playing second order liberties of
     * the superstring.
     */
    for (k = 0; k < ss_liberties; k++) {
      int apos = ss_libs[k];
	
      if (liberty_of_string(apos, str))
	continue;
      
      special_rescue_moves(str, apos, &moves);
    }
  }

  if (stackp <= backfill2_depth)
    break_chain3_moves(str, &moves);

  /* Only order and test the new set of moves. */
  order_moves(str, &moves, other, read_function_name, saved_num_moves);
  
  for (k = saved_num_moves; k < moves.num; k++) {
    int new_komaster;
    int new_kom_pos;
    int ko_move;
    xpos = moves.pos[k];
    
    if (komaster_trymove(xpos, color, "defend3-E", str,
			 komaster, kom_pos, &new_komaster, &new_kom_pos,
			 &ko_move, stackp <= ko_depth && savecode == 0)) {
      if (!ko_move) {
	int acode = do_attack(str, NULL, new_komaster, new_kom_pos);
	popgo();
	CHECK_RESULT(savecode, savemove, acode, xpos, move,
		     "defense effective - A");
      }
      else {
	if (do_attack(str, NULL, new_komaster, new_kom_pos) != WIN) {
	  savemove = xpos;
	  savecode = KO_B;
	}
	popgo();
      }
    }
  }

  if (savecode != 0)
    RETURN_RESULT(savecode, savemove, move, "saved move");

  RETURN_RESULT(0, 0, move, NULL);
}


/* defend4(str, *move) attempts to find a move rescuing the 
 * string at (str) with 4 liberties.  If such a move can be found,
 * it returns true, and if the pointer move is not NULL, 
 * then it returns the saving move in *move.
 */

static int 
defend4(int str, int *move, int komaster, int kom_pos)
{
  int color, other;
  int xpos;
  int liberties;
  int libs[4];
  struct reading_moves moves;
  int savemove = 0;
  int savecode = 0;
  int k;
  
  SETUP_TRACE_INFO("defend4", str);
  reading_node_counter++;

  color = board[str];
  other = OTHER_COLOR(color);

  gg_assert(IS_STONE(board[str]));
  gg_assert(countlib(str) == 4);

  liberties = findlib(str, 4, libs);
  ASSERT1(liberties == 4, str);

  /* Collect moves to try in the first batch.
   * 1. First order liberties.
   * 2. Chain breaking moves.
   */
  for (k = 0; k < liberties; k++) {
    moves.pos[k] = libs[k];
    moves.score[k] = 0;
  }
  moves.num = liberties;
  
  break_chain_moves(str, &moves);
  break_chain2_efficient_moves(str, &moves);

  if (stackp <= backfill_depth) {
    break_chain2_defense_moves(str, &moves);
  }

  order_moves(str, &moves, color, read_function_name, 0);

  for (k = 0; k < moves.num; k++) {
    int new_komaster;
    int new_kom_pos;
    int ko_move;

    if (stackp >= branch_depth && k > 0)
      break;
    
    xpos = moves.pos[k];
    
    if (komaster_trymove(xpos, color, "defend4-A", str, komaster, kom_pos,
			 &new_komaster, &new_kom_pos,
			 &ko_move, stackp <= ko_depth && savecode == 0)) {
      if (!ko_move) {
	int acode = do_attack(str, NULL, new_komaster, new_kom_pos);
	popgo();
	CHECK_RESULT(savecode, savemove, acode, xpos, move,
	  	     "defense effective - A");
      }
      else {
	if (do_attack(str, NULL, new_komaster, new_kom_pos) != WIN) {
	  savemove = xpos;
	  savecode = KO_B;
	}
	popgo();
      }
    }
  }


  if (savecode != 0)
    RETURN_RESULT(savecode, savemove, move, "saved move");

  RETURN_RESULT(0, 0, move, NULL);
}


/*
 * special_rescue_moves(str, lib, *move) is called with (str) a
 * string having a liberty at (lib).
 *
 * This adds moves on a second order liberty to the list of candidate
 * moves in the struct *moves; e.g. in shapes like:
 *
 *   .        O        O       X.XXO
 *  O.*  or  ..*  or  O.*  or  XOOXO
 *   O        O        O       ...*.
 *                             -----
 *
 * This will occasionally save a string where no other move will. To
 * reduce the branching caused by these moves, we require that the
 * opponent can be trivially captured when trying to intercept on the
 * corresponding first order liberty.
 */

static void
special_rescue_moves(int str, int lib, struct reading_moves *moves)
{
  int color = board[str];
  int other = OTHER_COLOR(color);
  int k;

  /* Loop over the four neighbours of the liberty, (lib + d). */
  for (k = 0; k < 4; k++) {
    int d = delta[k];
    if (board[lib + d] == EMPTY) {

      /* Use approxlib() to test for trivial capture. */
      if (approxlib(lib, other, 3, NULL) > 2)
	continue;

      /* Don't play into a self atari. */
      if (is_self_atari(lib + d, color))
	continue;
      
      ADD_CANDIDATE_MOVE(lib + d, 0, *moves);
    }
  }
}


/* In a situation like this:
 *       
 *   OOXXXX     the following code can find the
 *   .OXOOX     defensive move at 'c'.
 *   .cO.OX
 *   .X.OOX
 *   ------
 *
 *   OOXXXX     It also can find more general moves like 'c' here.
 *   .OXOOX     
 *   cXO.OX
 *   ...OOX
 *   ------
 */
static void
special_rescue2_moves(int str, int libs[2], struct reading_moves *moves)
{
  int color = board[str];
  int other = OTHER_COLOR(color);
  int newlibs[4];
  int liberties;
  int newstr;
  int k, r, s;
  
  for (r = 0; r < 2; r++) {
    /* Let alib be one of the liberties and require it to be suicide
     * for the opponent.
     */
    int alib = libs[r];
    if (!is_suicide(alib, other))
      continue;

    for (k = 0; k < 4; k++) {
      if (board[alib + delta[k]] == color
	  && !same_string(alib + delta[k], str)) {
	newstr = alib + delta[k];
	liberties = findlib(newstr, 4, newlibs);
	
	for (s = 0; s < liberties && s < 4; s++) {
	  if (!is_self_atari(newlibs[s], color))
	    ADD_CANDIDATE_MOVE(newlibs[s], 0, *moves);
	}
	break_chain_moves(newstr, moves);
	break_chain2_efficient_moves(newstr, moves);
	edge_clamp_moves(newstr, moves);
      }
    }
  }
}


/* In a situation like this:
 *
 *   ...X.XXO
 *   .XXXOOXO
 *   XXOO.OXO     the following code can find the
 *   .O..X.*.     defensive move at '*'.
 *   --------
 *
 *   OXO   cde
 *   .*.   afg
 *   ---   b--
 */
static void
special_rescue3_moves(int str, int libs[3], struct reading_moves *moves)
{
  int color = board[str];
  int other = OTHER_COLOR(color);
  int apos, bpos, cpos, dpos, epos, fpos, gpos;
  int k, l, r;

  ASSERT1(countlib(str) == 3, str);
  
  for (r = 0; r < 3; r++) {
    /* Let (apos) be one of the three liberties. */
    apos = libs[r];
    /* Try to find the configuration above. */
    for (k = 0; k < 4; k++) {
      bpos = apos + delta[k];
      if (ON_BOARD(bpos))
	continue;

      cpos = apos - delta[k];
      if (board[cpos] != color)
	continue;
      
      if (!same_string(cpos, str))
	continue;

      for (l = 0; l < 2; l++) {
	int normal = delta[(k+1)%4];
	if (l == 1)
	  normal = -normal;
	
	dpos = cpos + normal;
	if (board[dpos] != other)
	  continue;

	epos = dpos + normal;
	if (board[epos] != color)
	  continue;

	fpos = apos + normal;
	if (board[fpos] != EMPTY)
	  continue;

	gpos = fpos + normal;
	if (board[gpos] != EMPTY)	
	  continue;

	/* Configuration found. Now require an X move at 'a' not
	 * getting too many liberties.
	 */

	if (approxlib(apos, other, 4, NULL) > 3)
	  continue;
	
	/* Try to play at (fpos). */
	ADD_CANDIDATE_MOVE(fpos, 0, *moves);
      }
    }
  }
}


/* In a situation like this:
 *
 *   .XXXXX
 *   XX.*OO
 *   X.OX..     the following code can find the
 *   ......     defensive move at '*'.
 *   ------
 *
 *   .*   ac
 *   OX   bd
 *
 * The only requirement is that d has at most as many liberties as b,
 * and as the newly placed stone at c.
 */
static void
hane_rescue_moves(int str, int libs[4], struct reading_moves *moves)
{
  int color = board[str];
  int other = OTHER_COLOR(color);
  int apos, bpos, cpos, dpos;
  int num_libs = countlib(str);
  int k, l, r;

  ASSERT1(num_libs <= 4, str);
  
  for (r = 0; r < num_libs; r++) {
    /* Let (apos) be one of the three liberties. */
    apos = libs[r];
    /* Try to find the configuration above. */
    for (k = 0; k < 4; k++) {
      bpos = apos + delta[k];
      if (board[bpos] != color)
	continue;

      if (!same_string(bpos, str))
	continue;

      for (l = 0; l < 2; l++) {
	int normal = delta[(k+1)%4];
	if (l == 1)
	  normal = -normal;

	cpos = apos + normal;
	if (board[cpos] != EMPTY)
	  continue;

	dpos = bpos + normal;
	if (board[dpos] != other)
	  continue;

	/* Configuration found. Now check liberty constraint. */
	{
	  int dlibs = countlib(dpos);
	  if (dlibs > num_libs
	      || dlibs > accuratelib(cpos, color, dlibs, NULL))
	    continue;
	}
	
	if (0 && !in_list(cpos, moves->num, moves->pos)) {
	  gprintf("hane_rescue_move added for %1m at %1m\n", str, cpos);
	  dump_stack();
	  showboard(0);
	}
	ADD_CANDIDATE_MOVE(cpos, 0, *moves);
      }
    }
  }
}


/* In situations like these
 *
 *   |XXXX    |.X...    |.X...
 *   |OOOX    |.XOO.    |XXOO.
 *   |..OX    |OOXO.    |OOXO.
 *   |O.OX    |O.X*O    |O.XOO
 *   |.X*.    |O.X.O    |O.X*O
 *   +----    +-----    +-----
 *
 * the smaller of the O strings can be defended by *. The property
 * they have in common is that the defended string has (at least) two
 * liberties in common with an X string and it's effective to play on
 * an exterior liberty of this string. Similarly it may be worth
 * defending a weak neighbor of the X string.
 *
 * This function may be called for strings with 2 or 3 liberties and
 * returns moves which are potentially useful in these positions.
 */
static void
special_rescue5_moves(int str, int libs[3],
                      struct reading_moves *moves)
{
  int color = board[str];
  int other = OTHER_COLOR(color);
  int apos, bpos;
  int k, r, s;
  int liberties = countlib(str);
  int libs2[4];
  int liberties2;

  ASSERT1(liberties == 2 || liberties == 3, str);
  
  for (r = 0; r < liberties; r++) {
    apos = libs[r];
    
    for (k = 0; k < 4; k++) {
      bpos = apos + delta[k];
      if (board[bpos] != other)
	continue;

      /* Don't bother if it has too many liberties. */
      if (countlib(bpos) > liberties + 1)
	continue;

      if (count_common_libs(str, bpos) < 2)
	continue;

      liberties2 = findlib(bpos, 4, libs2);
      for (s = 0; s < liberties2; s++)
	if (!liberty_of_string(libs2[s], str)
	    && !is_self_atari(libs2[s], color))
	  ADD_CANDIDATE_MOVE(libs2[s], 0, *moves);

      /* Reinforce the second order chain. */
      if (liberties2 <= liberties) {
	int adj;
	int adjs[MAXCHAIN];
	int t;
	adj = chainlinks2(bpos, adjs, 1);
	for (t = 0; t < adj; t++) {
	  int cpos;
	  break_chain_moves(adjs[t], moves);
	  
	  findlib(adjs[t], 1, &cpos);
	  if (!is_self_atari(cpos, color))
	    ADD_CANDIDATE_MOVE(cpos, 0, *moves);
	}
  
	/* Defend against double atari in the surrounding chain early. */
	double_atari_chain2_moves(bpos, moves);
      }
    }
  }
}


/* In situations like this
 *
 *   |.bOX
 *   |.Xa.
 *   |.OXX
 *   |.O..
 *   |.XX.
 *
 * the lower O string can often be defended at a or b.
 *
 * This function may be called for strings with 3 or 4 liberties and
 * returns the * moves in the configuration below:
 *
 * |..O   |.*O
 * |.X.   |.c*
 * |.O?   |ab?
 *
 */
static void
special_rescue6_moves(int str, int libs[3], struct reading_moves *moves)
{
  int color = board[str];
  int other = OTHER_COLOR(color);
  int apos, bpos, cpos;
  int right, up;
  int k, l, r;
  int liberties = countlib(str);

  ASSERT1(liberties == 3 || liberties == 4, str);
  
  for (r = 0; r < liberties; r++) {
    apos = libs[r];
    
    for (k = 0; k < 4; k++) {
      right = delta[k];
      
      if (ON_BOARD(apos - right))
	continue;
      
      bpos = apos + right;
      if (board[bpos] != color || !same_string(str, bpos))
	continue;

      for (l = 0; l < 2; l++) {
	up = delta[(k+1) % 4];
	if (l == 1)
	  up = -up;

	cpos = bpos + up;
	if (board[cpos] != other)
	  continue;

	if (board[apos + up] != EMPTY)
	  continue;

	if (board[cpos + right] != EMPTY)
	  continue;

	if (board[apos + up + up] != EMPTY)
	  continue;
	
	if (board[cpos + up] != EMPTY)
	  continue;
	
	if (board[cpos + up + right] != color)
	  continue;
	
	
	if (!is_self_atari(cpos + right, color))
	  ADD_CANDIDATE_MOVE(cpos + right, 0, *moves);
	if (!is_self_atari(cpos + up, color))
	  ADD_CANDIDATE_MOVE(cpos + up, 0, *moves);
      }
    }
  }
}


/* In positions like
 *
 * |.XXOO.
 * |XXOX..
 * |OOOX*.
 * |......
 * +------
 *
 * the O stones to the left are best defended by the move at *.
 *
 * This function tries to find an adjacent string (apos) with exactly
 * three liberties. One of the liberties (bpos) must be on the edge
 * (but not in the corner). Diagonal to this liberty must be one stone
 * of the attacked string (cpos) and another liberty (dpos) of the
 * adjacent string. The third liberty (epos) must be adjacent to
 * (dpos). Furthermore must an O stone at (dpos) get at least three
 * liberties and and X stone at (epos) must get at most three
 * liberties.
 *
 * |.XXOO.
 * |XXOXe.
 * |OOcad.
 * |...b..
 * +------
 *
 * The defense move at (dpos) is proposed if the above conditions
 * are satisfied.
 */

static void
edge_clamp_moves(int str, struct reading_moves *moves)
{
  int color = board[str];
  int other = OTHER_COLOR(color);
  int apos;
  int bpos;
  int cpos;
  int dpos;
  int epos;
  int adj, adjs[MAXCHAIN];
  int libs[3];
  int k, l, r;
  
  /* Pick up neighbors with three liberties. */
  adj = chainlinks2(str, adjs, 3);

  for (r = 0; r < adj; r++) {
    apos = adjs[r];
    /* Find a liberty at the edge. */
    bpos = NO_MOVE;
    findlib(apos, 3, libs);
    for (k = 0; k < 3; k++) {
      if (is_edge_vertex(libs[k])) {
	bpos = libs[k];
	break;
      }
    }
    if (bpos == NO_MOVE)
      continue;

    /* Edge liberty found. Establish up and right directions. */
    for (k = 0; k < 4; k++) {
      int up = delta[k];
      if (ON_BOARD(bpos - up))
	continue;
      if (board[bpos + up] != other)
	continue;
       
      for (l = 0; l < 2; l++) {
	int right = delta[(k+1)%4];
	if (l == 1)
	  right = -right;

	cpos = bpos + up - right;
	dpos = bpos + up + right;

	if (board[cpos] != color || !same_string(cpos, str))
	  continue;

	if (board[dpos] != EMPTY || !liberty_of_string(dpos, apos))
	  continue;

	epos = dpos + up;

	if (board[epos] != EMPTY || !liberty_of_string(epos, apos))
	  continue;

	if (approxlib(dpos, color, 3, NULL) < 3)
	  continue;
	
	if (approxlib(epos, other, 4, NULL) > 3)
	  continue;

	/* (dpos) looks like a good move. Add it to the list with a
         * substantial initial score.
	 */
	ADD_CANDIDATE_MOVE(dpos, 10, *moves);
      }
    }
  }
}


/* 
 * This function handles some special cases on the edge.
 *
 * 1. If (str) points to a string and 'a' an edge liberty of it,
 *    there is no point of trying to defend the string by crawling 
 *    along the edge if there is no hope of ever getting more liberties.
 *    This is of course if the blocking enemy group has enough liberties
 *    of its own.
 *
 *      XX       XX
 *      O.       Oa
 *      --       --
 *
 *    This function searches the edge towards the corner and sees if there
 *    is a friendly stone on one of the two first lines. If not, the move
 *    is removed from the  list of moves.
 *
 * 2. If (str) points to a string and 'a' an edge liberty of it,
 *    the drawing back/climbing up move 'b' is often correct attack or
 *    defense. Another good move to try is 'c' (but usually not for
 *    defense of a 2 liberty string).
 * 
 *      X.?        Xbc
 *      O..        Oa.
 *      ---        ---
 *
 *    This function adds the points configured like 'b' and 'c' relative to
 *    (str) to the list of moves.
 *
 * color is the color to move.
 */

static void
propose_edge_moves(int str, int *libs, int liberties,
                   struct reading_moves *moves, int to_move)
{
  int color = board[str];
  int other = OTHER_COLOR(color);
  int right;
  int up;
  int apos;
  int k, l;
  int r;

  for (r = 0; r < liberties; r++) {
    apos = libs[r];
    for (k = 0; k < 4; k++) {
      up = delta[k];
      if (ON_BOARD(apos - up))
	continue;
      
      for (l = 0; l < 2; l++) {
	right = delta[(k+1)%4];
	if (l == 1)
	  right = -right;
       
	if (board[apos + up] == other	   /* other on top of liberty */
	    && countlib(apos + up) > 4     /* blocking group must be secure */
	    && color == to_move) {         /* only applicable as defense */

	  /* Case 1: other above the liberty (crawl along the edge). */
	  int xpos = apos;
	
	  while (ON_BOARD(xpos)) {
	    if (board[xpos] == color
		|| board[xpos + up] == color)
	      break;

	    xpos += right;
	  }

	  /* If no friendly stone found, then it is pointless and we
	   * can just as well remove the move.
	   */
	  if (!ON_BOARD(xpos)) {
	    REMOVE_CANDIDATE_MOVE(apos, *moves);
	  }
	}
	else if (board[apos + up] == EMPTY  /* empty above the liberty */
		 && board[apos - right + up] == other
		 && board[apos + right] == EMPTY) { /* empty to the right */

	  /* Case 2: Try to escape or contain. */
	  
	  /* Add b 
	   * If adjacent X stone in atari, boost the initial score of this
	   * move.
	   */
	  if (countlib(apos + up - right) == 1)
	    ADD_CANDIDATE_MOVE(apos + up, 10, *moves);
	  else {
	    ADD_CANDIDATE_MOVE(apos + up, 0, *moves);
	    
	    /* Add c if empty */
	    if (board[apos + right + up] == EMPTY
		&& (liberties != 2 || color != to_move))
	      ADD_CANDIDATE_MOVE(apos + right + up, 0, *moves);
	  }
	}
      }
    }
  }
}


/* ================================================================ */  
/*                       Attacking functions                        */
/* ================================================================ */

/* FIXME: These two globals should be wrapped up in accessors, at least. */
char *rgoal;
int goallib;

/* Like attack, but takes the komaster argument. If the
 * opponent is komaster, reading functions will not try
 * to take ko.
 */
static int 
do_attack(int str, int *move, int komaster, int kom_pos)
{
  int color = board[str];
  int xpos;
  int libs;
  int result = 0;
  int found_read_result;
  Read_result *read_result = NULL;

  SETUP_TRACE_INFO("attack", str);

  ASSERT1(color != 0, str);

  if (color == 0)      /* if assertions are turned off, silently fails */
    return 0;

  libs = countlib(str);

  if (libs > 4
      || (libs == 4 && stackp > fourlib_depth)) {
    /* No need to cache the result in these cases. */
    if (sgf_dumptree) {
      char buf[100];
      sprintf(buf, "got 4 liberties (stackp:%d>%d)", 
              stackp, fourlib_depth);
      SGFTRACE(0, 0, buf);
    }
    return 0;
  }

  if ((stackp <= depth) && (hashflags & HASH_ATTACK)) {
    found_read_result = get_read_result(ATTACK, komaster, kom_pos, 
					&str, &read_result);
    if (found_read_result) {
      TRACE_CACHED_RESULT(*read_result);
      if (rr_get_result(*read_result) != 0)
	if (move)
	  *move = rr_get_move(*read_result);

      SGFTRACE(rr_get_move(*read_result),
	       rr_get_result(*read_result), "cached");
      return rr_get_result(*read_result);
    }
  }

  /* Treat the attack differently depending on how many liberties the 
     string at (str) has. */
  if (libs == 1)
    result = attack1(str, &xpos, komaster, kom_pos);
  else if (libs == 2) {
    if (stackp > depth + 10)
      result = simple_ladder_attack(str, &xpos, komaster, kom_pos);
    else
      result = attack2(str, &xpos, komaster, kom_pos);
  }
  else if (libs == 3)
    result = attack3(str, &xpos, komaster, kom_pos);
  else if (libs == 4)
    result = attack4(str, &xpos, komaster, kom_pos);


  ASSERT1(result >= 0 && result <= WIN, str);
  
  if (result)
    READ_RETURN(read_result, move, xpos, result);

  READ_RETURN0(read_result);
}


/* If (str) points to a group with exactly one liberty, attack1
 * determines whether it can be captured by playing at this liberty.
 * If successful, (*move) is the killing move. move may be NULL if
 * caller is only interested in whether it can be captured.
 *
 * The attack may fail for two different reasons. The first one is
 * that the attack may be an illegal ko capture, in this case KO_B is
 * returned (need to play a ko threat before the attack can be
 * fulfilled).
 *
 * The second cause for failure is that the attack is caught in a
 * snapback. We must require that it is a proper snapback, though. By
 * proper snapback we mean a position like
 *
 *  XXXXO
 *  XO.XO
 *  XOXOO
 *  -----
 *
 * where capture by O and recapture by X leaves the X stone intact
 * with at least two liberties:
 *
 *  XXXXO
 *  X..XO
 *  X.XOO
 *  -----
 *
 * There are a number of different kinds of improper snapbacks, which
 * have in common that the attacked string ends up captured. We don't
 * consider these as failures to attack. Three examples are given below.
 *
 *   XXOOOOO     (X can recapture but loses most of the string.)
 *   X.XXXXO
 *   -------
 *
 *   XXXOOOOOOOO (Like the previous example, except O loses one more stone)
 *   XO*XXXXXXXO
 *   -----------
 *
 *   XXXOO       (After three captures, the lone X stone is gone.)
 *   XO.XO
 *   -----
 *
 * This function is fast and never branches. There's little point in
 * caching the result.
 */

static int
attack1(int str, int *move, int komaster, int kom_pos)
{
  int color = board[str];
  int other = OTHER_COLOR(color);
  int xpos;
  int result = -1;
  
  SETUP_TRACE_INFO("attack1", str);
  reading_node_counter++;
  
  /* Pick up the position of the liberty. */
  findlib(str, 1, &xpos);

  /* If the attacked string consists of more than one stone, the
   * attack never fails. (This assumes simple ko rule. With superko
   * rule it could still be a ko violation.)
   */
  if (countstones(str) > 1)
    result = WIN;
  
  /* Try to play on the liberty. This fails if and only if it is an
   * illegal ko capture.
   */
  else if (trymove(xpos, other, "attack1-A", str, komaster, kom_pos)) {
    /* Is the attacker in atari? If not the attack was successful. */
    if (countlib(xpos) > 1)
      result = WIN;

    /* If the attacking string is also a single stone, a possible
     * recapture would be a ko violation, so the defender has to make
     * a ko threat first.
     */
    else if (countstones(xpos) == 1) {
      /* If the defender is allowed to take the ko the result is KO_A. */
      if (komaster != other)
	result = KO_A;
      else 
	/* But if the attacker is komaster, the attack was successful. */
	result = WIN;
    }
      
    /* Otherwise, do recapture. Notice that the liberty must be
     * at (str) since we have already established that this string
     * was a single stone.
     */
    else if (trymove(str, color, "attack1-B", str, komaster, kom_pos)) {
      /* If this was a proper snapback, (str) will now have more
       * than one liberty.
       */
      if (countlib(str) > 1) {
	/* Proper snapback, attack fails. */
	result = 0;
      }
      else
	result = WIN;
      popgo();
    }
    popgo();
  }
  else {/* Illegal ko capture. */
    if (komaster != color) 
      result = KO_B;
    else
      result = 0;
  }

  /* If not yet successful, try backfilling.
   * FIXME: Maybe only meaningful to do this in positions involving ko.
   */
  if (result != WIN) {
    int liberties;
    int libs[6];
    int k;
    liberties = approxlib(xpos, color, 6, libs);
    if (liberties <= 5)
      for (k = 0; k < liberties; k++) {
	int apos = libs[k];
	if (!is_self_atari(apos, other)
	    && trymove(apos, other, "attack1-C", str, komaster, kom_pos)) {
	  int dcode = do_find_defense(str, NULL, komaster, kom_pos);
	  if (dcode != WIN && do_attack(str, NULL, komaster, kom_pos)) {
	    if (dcode == 0) {
	      popgo();
	      SGFTRACE(apos, WIN, "backfilling");
	      *move = apos;
	      return WIN;
	    }
	    UPDATE_SAVED_KO_RESULT(result, xpos, dcode, apos);
	  }
	  popgo();
	}
      }
  }
  
  if (result > 0) {
    *move = xpos;
    SGFTRACE(xpos, result, NULL);
  }
  else {
    SGFTRACE(0, 0, NULL);
  }
  
  return result;
}


/* If str points to a group with exactly two liberties
 * attack2 determines whether it can be captured in ladder or net.
 * If yes, *move is the killing move. move may be null if caller 
 * is only interested in whether it can be captured.
 *  
 * Returns KO_A or KO_B if it can be killed conditioned on ko. Returns
 * KO_A if it can be killed provided (other) is willing to ignore any
 * ko threat. Returns KO_B if (other) wins provided he has a ko threat
 * which must be answered. Can give a return code KO_B yet *move=0 if
 * the winning move is an illegal ko capture. In this case, making a
 * ko threat and having it answered should transform the position to
 * one where the return code is KO_A.
 *
 * See the comment before defend1 about ladders and reading depth.
 */

static int 
attack2(int str, int *move, int komaster, int kom_pos)
{
  int color = board[str];
  int other = OTHER_COLOR(color);
  int hpos;
  int xpos;
  int liberties, r;
  int libs[2];
  int libs2[2];
  int adj, adjs[MAXCHAIN];
  int savemove = 0;
  int savecode = 0;
  int acode;
  int dcode;
  int k;
  int atari_possible = 0;
  struct reading_moves moves;
  int adjacent_liberties = 0;

  SETUP_TRACE_INFO("attack2", str);
  reading_node_counter++;
  moves.num = 0;

  str = find_origin(str);
  ASSERT1(IS_STONE(board[str]), str);
  ASSERT1(countlib(str) == 2, str);

  /* The attack may fail if a boundary string is in atari and cannot 
   * be defended.  First we must try defending such a string. 
   *
   * We start by trying to defend the boundary string by looking for an
   * adjacent string which is in atari. 
   */
  adj = chainlinks2(str, adjs, 1);
  for (r = 0; r < adj; r++) {
    /* if stackp > depth and any boundary chain is in atari, assume safe.
     * However, if the captured chain is only of size 1, there can still
     * be a working ladder, so continue if that is the case.
     */
    if (stackp > depth && countstones(adjs[r]) > 1) {
      RETURN_RESULT(0, 0, move, "boundary in atari");
    }

    /* Pick up moves breaking the second order chain. */
    if (stackp <= depth)
      break_chain_moves(adjs[r], &moves);
    
    findlib(adjs[r], 1, &hpos);
    ADD_CANDIDATE_MOVE(hpos, 0, moves);
  }

  /* Get the two liberties of (str). */
  liberties = findlib(str, 2, libs);
  ASSERT1(liberties == 2, str);

  if (libs[0] == SOUTH(libs[1])
      || libs[0] == WEST(libs[1])
      || libs[0] == NORTH(libs[1])
      || libs[0] == EAST(libs[1]))
    adjacent_liberties = 1;
  
  for (k = 0; k < 2; k++) {
    int apos = libs[k];
    if (!is_self_atari(apos, other))
      atari_possible = 1;
    /* we only want to consider the move at (apos) if:
     * stackp <= backfill_depth
     * -or-  stackp <= depth and it is an isolated stone
     * -or-  it is not in immediate atari
     */
    if (stackp <= backfill_depth
	|| ((stackp <= depth || adjacent_liberties) 
	    && board[SOUTH(apos)] != other
	    && board[WEST(apos)] != other
	    && board[NORTH(apos)] != other
	    && board[EAST(apos)] != other)
	|| !is_self_atari(apos, other))
      ADD_CANDIDATE_MOVE(apos, 0, moves);

    /* Try backfilling if atari is impossible. */
    if (stackp <= backfill_depth && approxlib(apos, other, 2, libs2) == 1) {
      ADD_CANDIDATE_MOVE(libs2[0], 0, moves);
      /* If there is a neighbor in atari, we also try back-capturing. */
      for (r = 0; r < 4; r++) {
	int bpos = libs2[0] + delta[r];
	if (board[bpos] == other && chainlinks2(bpos, adjs, 1) > 0) {
	  /* FIXME: If there is more than one neighbor in atari, we
           * currently just take one randomly. This is maybe not good
           * enough. We might also want to check against snapback.
	   */
	  findlib(adjs[0], 1, &xpos);
	  ADD_CANDIDATE_MOVE(xpos, 0, moves);
	}
      }
    }
  }

  /* If we can't make a direct atari, look for edge blocking moves. */
  if (!atari_possible)
    for (k = 0; k < 2; k++) 
      edge_block_moves(str, libs[k], &moves);
    

  /* If one of the surrounding chains have only two liberties, which
   * coincide with the liberties of the attacked string, we try to
   * backcapture.
   */
  
  adj = chainlinks2(str, adjs, 2);
  for (r = 0; r < adj; r++) {
    int apos = adjs[r];
    if (liberty_of_string(libs[0], apos)
	&& liberty_of_string(libs[1], apos))
      break_chain_moves(apos, &moves);
  }
  
  propose_edge_moves(str, libs, liberties, &moves, other);
  order_moves(str, &moves, other, read_function_name, 0);

  for (k = 0; k < moves.num; k++) {
    int new_komaster;
    int new_kom_pos;
    int ko_move;

    int apos = moves.pos[k];
    if (komaster_trymove(apos, other, "attack2-A", str,
			 komaster, kom_pos, &new_komaster, &new_kom_pos,
			 &ko_move, stackp <= ko_depth && savecode == 0)) {
      if (!ko_move) {
	dcode = do_find_defense(str, NULL, new_komaster, new_kom_pos);
	if (dcode != WIN
	    && do_attack(str, NULL, new_komaster, new_kom_pos)) {
	  popgo();
	  CHECK_RESULT(savecode, savemove, dcode, apos, move,
	    	       "attack effective");
	}
	else
	  popgo();
      }
      else {
	if (do_find_defense(str, NULL, new_komaster, new_kom_pos) != WIN
	    && do_attack(str, NULL, new_komaster, new_kom_pos) != 0) {
	  savemove = apos;
	  savecode = KO_B;
	}
        popgo();
      }
    }
  }
  
  /* The simple ataris didn't work. Try something more fancy. */
  acode = find_cap2(str, libs[0], libs[1], &xpos, komaster, kom_pos);
  CHECK_RESULT_UNREVERSED(savecode, savemove, acode, xpos, move, "find cap2");

  if (stackp <= backfill_depth) {
    acode = special_attack2(str, libs, &xpos, komaster, kom_pos);
    CHECK_RESULT_UNREVERSED(savecode, savemove, acode, xpos, move,
      			    "special attack2");
  }

  if (stackp <= backfill_depth) {
    acode = special_attack3(str, libs, &xpos, komaster, kom_pos);
    CHECK_RESULT_UNREVERSED(savecode, savemove, acode, xpos, move,
			    "special attack3");
  }

  if (stackp <= backfill_depth) {
    acode = special_attack4(str, libs, &xpos, komaster, kom_pos);
    CHECK_RESULT_UNREVERSED(savecode, savemove, acode, xpos, move,
      			    "special attack4");
  }

  /* If it is not possible to make a direct atari, we try filling
   * a liberty of the superstring.
   */
  if (level >= 10
      && stackp <= backfill_depth
      && (stackp <= superstring_depth || !atari_possible)) {
    int ss_liberties;
    int ss_libs[MAX_LIBERTIES + 4];
    int liberty_cap = 2;

    if (stackp <= backfill2_depth)
      liberty_cap = 3;
    
    find_superstring_liberties(str, &ss_liberties, ss_libs, liberty_cap);
    if (ss_liberties <= 5) {
      for (k = 0; k < ss_liberties; k++) {
	int apos = ss_libs[k];
	
	if (liberty_of_string(apos, str))
	  continue;
	if (trymove(apos, other, "attack2-C", str, komaster, kom_pos)) {
	  if (countlib(apos) == 1) {
	    /* can't atari, try backfilling. */
	    findlib(apos, 1, &xpos);
	    if (approxlib(xpos, other, 2, NULL) > 1) {
	      popgo();
	      if (trymove(xpos, other, "attack2-D", str, komaster, kom_pos)) {
		dcode = do_find_defense(str, NULL, komaster, kom_pos);
		if (dcode != WIN && do_attack(str, NULL, komaster, kom_pos)) {
		  popgo();
		  CHECK_RESULT(savecode, savemove, dcode, xpos, move,
		      	       "attack effective");
		}
		else
		  popgo();
	      }
	    }
	    else
	      popgo();
	  }
	  else {
	    dcode = do_find_defense(str, NULL, komaster, kom_pos);
	    if (dcode != WIN 
		&& do_attack(str, NULL, komaster, kom_pos)) {
	      popgo();
	      CHECK_RESULT(savecode, savemove, dcode, apos, move,
			   "attack effective");
	    }
	    else
	      popgo();
	  }
	}
      }
    }
  }

  if (savecode == 0) {
    RETURN_RESULT(0, 0, move, NULL);
  }


  RETURN_RESULT(savecode, savemove, move, "saved move");
}



/* attack3(str, *move) is used when (str) points to a group with
 * three liberties. It returns true if it finds a way to kill the group.
 *
 * Return code is KO_A if the group can be killed if the attacker is 
 * willing to ignore any ko threat.
 *
 * Return code is KO_B if the group can be killed if the attacker is 
 * able to find a ko threat which must be answered.
 *
 * If non-NULL (*move) will be set to the move which makes the
 * attack succeed.
 */

static int 
attack3(int str, int *move, int komaster, int kom_pos)
{
  int color = board[str];
  int other = OTHER_COLOR(color);
  int adj, adjs[MAXCHAIN];
  int xpos;
  int liberties;
  int libs[3];
  int r;
  int dcode = 0;
  int k;
  struct reading_moves moves;
  int savemove = 0;
  int savecode = 0;
  
  SETUP_TRACE_INFO("attack3", str);
  reading_node_counter++;
  moves.num = 0;
  
  gg_assert(IS_STONE(board[str]));
  
  if (stackp > depth)
    RETURN_RESULT(0, 0, move, "stackp > depth");
  
  adj = chainlinks2(str, adjs, 1);
  for (r = 0; r < adj; r++) {
    int hpos;
    break_chain_moves(adjs[r], &moves);
    
    findlib(adjs[r], 1, &hpos);
    ADD_CANDIDATE_MOVE(hpos, 0, moves);
  }
  
  /* Defend against double atari in the surrounding chain early. */
  double_atari_chain2_moves(str, &moves);
  
  /* Get the three liberties of (str). */
  liberties = findlib(str, 3, libs);
  ASSERT1(liberties == 3, str);
  
  for (k = 0; k < 3; k++) {
    int apos = libs[k];
    /* we only want to consider the move at (apos) if:
     * stackp <= backfill_depth
     * -or-  stackp <= depth and it is an isolated stone
     * -or-  it is not in immediate atari
     */
    if (stackp <= backfill_depth
	|| (stackp <= depth
	    && board[SOUTH(apos)] != other
	    && board[WEST(apos)] != other
	    && board[NORTH(apos)] != other
	    && board[EAST(apos)] != other)
	|| !is_self_atari(apos, other))
      ADD_CANDIDATE_MOVE(apos, 0, moves);

    if (edge_closing_backfill(str, apos, &xpos))
      ADD_CANDIDATE_MOVE(xpos, 0, moves);

    /* Look for edge blocking moves. */
    edge_block_moves(str, apos, &moves);
  }
  
  /* Pick up some edge moves. */
  propose_edge_moves(str, libs, liberties, &moves, other);
  order_moves(str, &moves, other, read_function_name, 0);

  /* Try the moves collected so far. */
  for (k = 0; k < moves.num; k++) {
    int new_komaster;
    int new_kom_pos;
    int ko_move;
    
    if (stackp >= branch_depth && k > 0)
      break;
    xpos = moves.pos[k];
    if (komaster_trymove(xpos, other, "attack3-A", str,
			 komaster, kom_pos, &new_komaster, &new_kom_pos,
			 &ko_move, stackp <= ko_depth && savecode == 0)) {
      if (!ko_move) {
	dcode = do_find_defense(str, NULL, new_komaster, new_kom_pos);
	if (dcode != WIN && do_attack(str, NULL, new_komaster, new_kom_pos)) {
	  popgo();
	  CHECK_RESULT(savecode, savemove, dcode, xpos, move,
	    	       "attack effective");
	}
	else
	  popgo();
      }
      else {
	if (do_find_defense(str, NULL, new_komaster, new_kom_pos) != WIN
	    && do_attack(str, NULL, new_komaster, new_kom_pos) != 0) {
	  savemove = xpos;
	  savecode = KO_B;
	}
	popgo();
      }
    }
  }
    
  /* The simple ataris didn't work. Try something more fancy. */
  if (stackp <= backfill_depth) {
    int acode = find_cap(str, &xpos, komaster, kom_pos);
    CHECK_RESULT_UNREVERSED(savecode, savemove, acode, xpos, move,
      			    "find cap3");
  }

  if (stackp <= fourlib_depth) {
    int acode = draw_back(str, &xpos, komaster, kom_pos);
    CHECK_RESULT_UNREVERSED(savecode, savemove, acode, xpos, move,
      			    "draw back");
  }

  /* Try to defend chain links with two liberties. */
  if (stackp <= backfill2_depth) {
    int saved_num_moves = moves.num;
    adj = chainlinks2(str, adjs, 2);
    for (r = 0; r < adj; r++) {
      int libs2[2];
      findlib(adjs[r], 2, libs2);
      if (approxlib(libs2[0], other, 4, NULL) > 3
	  && approxlib(libs2[1], other, 4, NULL) > 3)
	continue;
      break_chain_moves(adjs[r], &moves);
      break_chain2_moves(adjs[r], &moves, 1);
      for (k = 0; k < 2; k++)
	ADD_CANDIDATE_MOVE(libs2[k], 0, moves);
    }
    /* Only order and test the new set of moves. */
    order_moves(str, &moves, other, read_function_name, saved_num_moves);
    for (k = saved_num_moves; k < moves.num; k++) {
      int new_komaster;
      int new_kom_pos;
      int ko_move;

      if (stackp >= branch_depth && k > 0)
	break;
      xpos = moves.pos[k];
      
      if (komaster_trymove(xpos, other, "attack3-C", str,
			   komaster, kom_pos, &new_komaster, &new_kom_pos,
			   &ko_move, stackp <= ko_depth && savecode == 0)) {
	if (!ko_move) {
	  dcode = do_find_defense(str, NULL, new_komaster, new_kom_pos);
	  if (dcode != WIN
	      && do_attack(str, NULL, new_komaster, new_kom_pos)) {
	    popgo();
	    CHECK_RESULT(savecode, savemove, dcode, xpos, move,
			 "attack effective");
	  }
	  else
	    popgo();
	}
	else {
	  if (do_find_defense(str, NULL, new_komaster, new_kom_pos) != WIN
	      && do_attack(str, NULL, new_komaster, new_kom_pos) != 0) {
	    savemove = xpos;
	    savecode = KO_B;
	  }
	  popgo();
	}
      }
    }
  }
    
  /* If nothing else works, we try filling a liberty of the
   * super_string.
   */
  if (level >= 10 && stackp <= backfill2_depth) {
    int ss_liberties;
    int ss_libs[MAX_LIBERTIES + 4];

    find_superstring_liberties(str, &ss_liberties, ss_libs, 3);
    if (ss_liberties <= 5) {
      for (k = 0; k < ss_liberties; k++) {
	int apos = ss_libs[k];
	
	if (liberty_of_string(apos, str))
	  continue;
	if (trymove(apos, other, "attack3-E", str, komaster, kom_pos)) {
	  if (countlib(apos) == 1) {
	    /* can't atari, try backfilling */
	    findlib(apos, 1, &xpos);
	    if (approxlib(xpos, other, 2, NULL) > 1) {
	      popgo();
	      if (trymove(xpos, other, "attack3-F", str, komaster, kom_pos)) {
		dcode = do_find_defense(str, NULL, komaster, kom_pos);
		if (dcode != WIN && do_attack(str, NULL, komaster, kom_pos)) {
		  popgo();
		  CHECK_RESULT(savecode, savemove, dcode, xpos, move,
		    	       "attack effective");
		}
		else
		  popgo();
	      }
	    }
	    else
	      popgo();
	  }
	  else {
	    dcode = do_find_defense(str, NULL, komaster, kom_pos);
	    if (dcode != WIN && do_attack(str, NULL, komaster, kom_pos)) {
	      popgo();
	      CHECK_RESULT(savecode, savemove, dcode, apos, move,
			   "attack effective");
	    }
	    else
	      popgo();
	  }
	}
      }
    }
  }

  RETURN_RESULT(savecode, savemove, move, "saved move");
}


/* attack4 tries to capture a string with 4 liberties. This function
 * is not cached.
 */

static int 
attack4(int str, int *move, int komaster, int kom_pos)
{
  int color = board[str];
  int other = OTHER_COLOR(color);
  int xpos;
  int r;
  int k;
  int liberties;
  int libs[4];
  int adj, adjs[MAXCHAIN];
  int dcode = 0;
  struct reading_moves moves;
  int savemove = 0;
  int savecode = 0;

  SETUP_TRACE_INFO("attack4", str);
  
  gg_assert(IS_STONE(board[str]));
  reading_node_counter++;
  moves.num = 0;
  
  if (stackp > depth) {
    SGFTRACE(0, 0, "stackp > depth");
    return 0;
  }

  adj = chainlinks2(str, adjs, 1);
  for (r = 0; r < adj; r++) {
    int hpos;
    break_chain_moves(adjs[r], &moves);
    
    findlib(adjs[r], 1, &hpos);
    ADD_CANDIDATE_MOVE(hpos, 0, moves);
  }


  /* Defend against double atari in the surrounding chain early. */
  double_atari_chain2_moves(str, &moves);

  /* Give a score bonus to the chain preserving moves. */
  for (k = 0; k < moves.num; k++)
    moves.score[k] += 5;
  
  /* Get the four liberties of (str). */
  liberties = findlib(str, 4, libs);
  ASSERT1(liberties == 4, str);

  for (k = 0; k < 4; k++) {
    int apos = libs[k];
    /* we only want to consider the move at (apos) if:
     * stackp <= backfill_depth
     * -or-  stackp <= depth and it is an isolated stone
     * -or-  it is not in immediate atari
     */
    if (stackp <= backfill_depth
	|| (stackp <= depth
	    && board[SOUTH(apos)] != other
	    && board[WEST(apos)] != other
	    && board[NORTH(apos)] != other
	    && board[EAST(apos)] != other)
	|| !is_self_atari(apos, other))
      ADD_CANDIDATE_MOVE(apos, 0, moves);

    if (edge_closing_backfill(str, apos, &xpos))
      ADD_CANDIDATE_MOVE(xpos, 10, moves);

    /* Look for edge blocking moves. */
    edge_block_moves(str, apos, &moves);
  }

  /* Pick up some edge moves. */
  propose_edge_moves(str, libs, liberties, &moves, other);
  order_moves(str, &moves, other, read_function_name, 0);

  /* Try the moves collected so far. */
  for (k = 0; k < moves.num; k++) {
    int new_komaster;
    int new_kom_pos;
    int ko_move;

    if (stackp >= branch_depth && k > 0)
      break;
    xpos = moves.pos[k];
    /* Conditional ko capture is disabled because it seems to expensive. */
    if (komaster_trymove(xpos, other, "attack4-A", str,
			 komaster, kom_pos, &new_komaster, &new_kom_pos,
			 &ko_move, 0 && stackp <= ko_depth && savecode == 0)) {
      if (!ko_move) {
	dcode = do_find_defense(str, NULL, new_komaster, new_kom_pos);
	if (dcode != WIN && do_attack(str, NULL, new_komaster, new_kom_pos)) {
	  popgo();
	  CHECK_RESULT(savecode, savemove, dcode, xpos, move,
	    	       "attack effective");
	}
	else
	  popgo();
      }
      else {
	if (do_find_defense(str, NULL, new_komaster, new_kom_pos) != WIN
	    && do_attack(str, NULL, new_komaster, new_kom_pos) != 0) {
	  savemove = xpos;
	  savecode = KO_B;
	}
	popgo();
      }
    }
  }

  if (stackp <= backfill_depth) {
    int acode = find_cap(str, &xpos, komaster, kom_pos);
    CHECK_RESULT_UNREVERSED(savecode, savemove, acode, xpos, move,
      			    "find cap");
  }

  RETURN_RESULT(savecode, savemove, move, "saved move");
}


/* find_cap2(str, alib, blib, &i, &j, komaster)
 * looks for a configuration of the following type:
 *
 *  X.
 *  .*
 *
 * where X is an element of the string in question. It tries the
 * move at * and returns true this move captures the string, leaving
 * (i, j) pointing to *.
 */

static int
find_cap2(int str, int alib, int blib, int *move, int komaster, int kom_pos)
{
  int ai, aj;
  int bi, bj;

  /* Check if the two liberties are located like the figure above. */
  if (alib != SW(blib)
      && alib != NW(blib)
      && alib != NE(blib)
      && alib != SE(blib))
    return 0;

  ai = I(alib);
  aj = J(alib);
  bi = I(blib);
  bj = J(blib);
  /* Which of the two corner points should we use? One of them is
   * always occupied by the string at (str), the other one is either
   * free or occupied by something else.
   */
  if (BOARD(bi, aj) == EMPTY)
    *move = POS(bi, aj);
  else if (BOARD(ai, bj) == EMPTY)
    *move = POS(ai, bj);
  else
    return 0;

  /* Ok, we found the spot. Now see if the move works. */
  if (trymove(*move, OTHER_COLOR(board[str]), "find_cap2", str,
	      komaster, kom_pos)) {
    int dcode = do_find_defense(str, NULL, komaster, kom_pos);
    popgo();
    switch (dcode) {
    case 0:
      return WIN;
      break;
    case WIN:
      return 0;
      break;
    case KO_B:
      return KO_B;
      break;
    case KO_A:
      return KO_A;
      break;
    }
  }

  return 0;
}


/* If (str) points to a string with 2 - 4 liberties, find_cap(str, &move)
 * looks for a configuration of the following type:
 *
 *  Xa
 *  b*
 *
 * where X are elements of the string in question and a and b are
 * two of its liberties. It tries the move at * and returns true
 * this move captures the string, leaving (*move) pointing to *.
 *
 * For larger strings, this can find moves like
 *
 * XXXXX
 * XX.XX
 * X.*.X
 * XX.XX
 * XXXXX
 *
 * even though they are not capping moves.
 */

static int
find_cap(int str, int *move, int komaster, int kom_pos)
{
  int alib, blib;
  int numlibs;
  int libs[4];
  int xpos = 0;
  int i,j;
  int savemove = 0;
  int savecode = 0;
  int acode;

  numlibs = findlib(str, 4, libs);
  if (numlibs > 4 || numlibs < 2)
    return 0;

  for (i = 0; i < numlibs - 1; i++)
    for (j = i + 1; j < numlibs; j++) {
      alib = libs[i];
      blib = libs[j];

      acode = find_cap2(str, alib, blib, &xpos, komaster, kom_pos);
      if (acode == WIN) {
        *move = xpos;
        return WIN;
      }
      UPDATE_SAVED_KO_RESULT_UNREVERSED(savecode, savemove, acode, xpos);
    }

  if (savecode != 0)
    *move = savemove;

  return savecode;
}



/* In a situation like this:
 *       
 * -----        the code that
 * cO.OX        follows can find
 * XXOOX        the attacking move
 * XO.OX        at c.
 * XOOOX
 * XXXXX
 *
 * The name of the function corresponds to special_rescue2, which is
 * fairly similar to this situation.
 */

static int
special_attack2(int str, int libs[2], int *move, int komaster, int kom_pos)
{
  int color = board[str];
  int other = OTHER_COLOR(color);
  int newlibs[3];
  int xpos;
  int savemove = 0;
  int savecode = 0;
  int k;

  for (k = 0; k < 2; k++) {
    if (is_suicide(libs[k], other) 
	&& (approxlib(libs[k], color, 3, newlibs) == 2)) {
      if (newlibs[0] != libs[1-k])
	xpos = newlibs[0];
      else
	xpos = newlibs[1];

      if (!is_self_atari(xpos, other)
	  && trymove(xpos, other, "special_attack2", str, komaster, kom_pos)) {
	int dcode = do_find_defense(str, NULL, komaster, kom_pos);
	if (dcode != WIN && do_attack(str, NULL, komaster, kom_pos)) {
	  if (dcode == 0) {
	    popgo();
	    *move = xpos;
	    return WIN;
	  }
	  UPDATE_SAVED_KO_RESULT(savecode, savemove, dcode, xpos);
	}
	popgo();
      }
    }
  }

  if (savecode != 0)
    *move = savemove;

  return savecode;
}


/* In situations like these:
 *
 * ..XXX..   ...XX
 * .XX.XX.   .cO.X
 * XXOOOXX   ....X
 * XO.O.OX   XOOXX
 * XO.c.OX   XXXX.
 * -------
 *
 * the code that follows can find the attacking move at c.
 */

static int
special_attack3(int str, int libs[2], int *move, int komaster, int kom_pos)
{
  int color = board[str];
  int other = OTHER_COLOR(color);
  int acode;
  int dcode;
  int savemove = 0;
  int savecode = 0;
  int newlibs[2];
  int xpos;
  int ypos;
  int apos;
  int bpos;
  int k;

  gg_assert(countlib(str) == 2);

  for (k = 0; k < 2; k++) {
    apos = libs[k];
    bpos = libs[1-k];
    
    if (apos == SOUTH(bpos) || apos == NORTH(bpos)) {
      if (board[WEST(apos)] == EMPTY)
	xpos = WEST(apos);
      else if (board[EAST(apos)] == EMPTY)
	xpos = EAST(apos);
      else
	continue;
    }
    else if (apos == WEST(bpos) || apos == EAST(bpos)) {
      if (board[SOUTH(apos)] == EMPTY)
	xpos = SOUTH(apos);
      else if (board[NORTH(apos)] == EMPTY)
	xpos = NORTH(apos);
      else
	continue;
    }
    else
      return 0; /* Incorrect configuration, give up. */

    if (is_self_atari(xpos, other)
	|| !trymove(xpos, other, "special_attack3-A", str, komaster, kom_pos))
      continue;

    if (countlib(xpos) == 2) {
      findlib(xpos, 2, newlibs);
      if (newlibs[0] == apos)
	ypos = newlibs[1];
      else
	ypos = newlibs[0];

      if (!is_self_atari(ypos, color)
	  && trymove(ypos, color, "special_attack3-B", str,
		     komaster, kom_pos)) {
	acode = do_attack(str, NULL, komaster, kom_pos);
	if (acode == 0) {
	  popgo();
	  popgo();
	  continue;
	}
	UPDATE_SAVED_KO_RESULT_UNREVERSED(savecode, savemove, acode, xpos);
	popgo();
      }
    }

    dcode = do_find_defense(str, NULL, komaster, kom_pos);
    if (dcode != WIN && do_attack(str, NULL, komaster, kom_pos)) {
      if (dcode == 0) {
	popgo();
	*move = xpos;
	return WIN;
      }
      UPDATE_SAVED_KO_RESULT(savecode, savemove, dcode, xpos);
    }
    popgo();
  }

  if (savecode != 0)
    *move = savemove;

  return savecode;
}


/* In situations like these:
 *
 * ...O.O...   ...O.O...
 * XXXXOOXXX   XXXXOOXXX
 * XOOOXXO*.   Xsssbbcd.
 * .X.O.....   .X.sa.e..
 * ---------   ---------
 *
 * the code that follows can find the attacking move at *.
 */

static int
special_attack4(int str, int libs[2], int *move, int komaster, int kom_pos)
{
  int color = board[str];
  int other = OTHER_COLOR(color);
  int dcode;
  int savemove = 0;
  int savecode = 0;
  int adj, adjs[MAXCHAIN];
  int adj2, adjs2[MAXCHAIN];
  int libs2[2];
  int apos;
  int bpos = 0;
  int cpos;
  int dpos;
  int epos;
  int dlibs;
  int elibs;
  int k, s, t;

  gg_assert(countlib(str) == 2);

  /* To avoid making this too general, we require that both
   * liberties are self ataris for X.
   */
  if (!is_self_atari(libs[0], other) 
      || !is_self_atari(libs[1], other))
    return 0;

  /* Pick up chain links with 2 liberties. */
  adj = chainlinks2(str, adjs, 2);
  
  for (k = 0; k < 2; k++) {
    apos = libs[k];

    /* Check that (apos) also is a liberty of one of the two liberty
     * chain links.
     */
    for (s = 0; s < adj; s++)
      if (liberty_of_string(apos, adjs[s])) {
	bpos = adjs[s];
	break;
      }

    /* Nothing found. */
    if (s == adj)
      continue;

    /* Now require that (bpos) has a chain link, different from (str),
     * also with two liberties.
     */
    adj2 = chainlinks2(bpos, adjs2, 2);

    for (s = 0; s < adj2; s++) {
      cpos = adjs2[s];
      if (same_string(cpos, str))
	continue;
      
      /* Pick up the liberties of (cpos). */
      findlib(cpos, 2, libs2);

      /* Try playing at a liberty. Before doing this, verify that
       * (cpos) cannot get more than two liberties by answering on the
       * other liberty and that we are not putting ourselves in atari.
       * We also shouldn't allow ourselves to get fewer liberties than
       * the defender.
       */
      for (t = 0; t < 2; t++) {
	dpos = libs2[t];
	epos = libs2[1-t];

	if (is_self_atari(dpos, other))
	  continue;

	elibs = approxlib(epos, color, 4, NULL);
	if (elibs > 3)
	  continue;

	dlibs = approxlib(dpos, other, 3, NULL);
	if (elibs > dlibs)
	  continue;

	if (trymove(dpos, other, "special_attack4", str, komaster, kom_pos)) {
	  dcode = do_find_defense(str, NULL, komaster, kom_pos);

	  if (dcode != WIN && do_attack(str, NULL, komaster, kom_pos)) {
	    if (dcode == 0) {
	      popgo();
	      *move = dpos;
	      return WIN;
	    }
	    UPDATE_SAVED_KO_RESULT(savecode, savemove, dcode, dpos);
	  }
	  popgo();
	}
      }
    }
  }

  if (savecode != 0)
    *move = savemove;

  return savecode;
}


/* 
 * If (str) points to a string, draw_back(str, &move, komaster)
 * looks for a move in the following configuration which attacks
 * the string:
 *
 *      X*            X=attacker, O=defender
 *      O.
 *
 * In the initial implementation we consider cases 
 * where X has exactly 2 liberties. 
 *
 */

static int
draw_back(int str, int *move, int komaster, int kom_pos)
{
  int r, k;
  int adj, adjs[MAXCHAIN];
  int libs[2];
  int savemove = 0;
  int savecode = 0;

  adj = chainlinks2(str, adjs, 2);
  for (r = 0; r < adj; r++) {
    findlib(adjs[r], 2, libs);
    for (k = 0; k < 2; k++) {
      if (!liberty_of_string(libs[k], str)
	     && ((ON_BOARD1(SOUTH(libs[k]))
		     && liberty_of_string(SOUTH(libs[k]), str))
	      || (ON_BOARD1(WEST(libs[k]))
	      	     && liberty_of_string(WEST(libs[k]), str))
	      || (ON_BOARD1(NORTH(libs[k]))
	             && liberty_of_string(NORTH(libs[k]), str))
	      || (ON_BOARD1(EAST(libs[k]))
	             && liberty_of_string(EAST(libs[k]), str)))) {
	if (trymove(libs[k], OTHER_COLOR(board[str]), "draw_back", str,
		    komaster, kom_pos)) {
	  int dcode = do_find_defense(str, NULL, komaster, kom_pos);
	  if (dcode != WIN && do_attack(str, NULL, komaster, kom_pos)) {
	    if (dcode == 0) {
	      popgo();
	      *move = libs[k];
	      return WIN;
	    }
	    UPDATE_SAVED_KO_RESULT(savecode, savemove, dcode, libs[k]);
	  }
	  popgo();
	}
      }
    }
  }

  if (savecode != 0)
    *move = savemove;

  return savecode;
}

/* In the following position the reading is much simplifed if we start
 * with the edge closing backfilling move at *.
 *
 * |OO...
 * |.OOO.
 * |.X.O.
 * |XXXO.
 * |.X.*.
 * +-----
 *
 * This function identifies the situation
 *
 * ?XOb
 * Xatc
 * ----
 *
 * where a is a liberty of the attacked string, t is the proposed move,
 * and b and c do not contain more O stones than X stones.
 */

static int
edge_closing_backfill(int str, int apos, int *move)
{
  int color = board[str];
  int other = OTHER_COLOR(color);
  int k;
  int bpos;
  int cpos;
  int number_x, number_o;

  for (k = 0; k < 4; k++) {
    int up = delta[k];
    int right = delta[(k+1)%4];
    if (ON_BOARD(apos - up))
      continue;
    if (board[apos + up] != color)
      return 0;
    if (board[apos + right] == EMPTY
	&& (!ON_BOARD(apos - right) 
	    || board[apos - right] == color))
      ; /* Everything ok so far. */
    else if (board[apos - right] == EMPTY
	     && (!ON_BOARD(apos + right) 
		 || board[apos + right] == color)) {
      /* Negate right direction. */
      right = -right;
    }
    else
      return 0;
    
    if (board[apos + up + right] != other)
      return 0;

    bpos = apos + up + 2 * right;
    if (!ON_BOARD(bpos))
      return 0;

    cpos = apos + 2 * right;

    number_x = 0;
    number_o = 0;
    if (board[bpos] == color)
      number_x++;
    else if (board[bpos] == other)
      number_o++;

    if (board[cpos] == color)
      number_x++;
    else if (board[cpos] == other)
      number_o++;

    if (number_o > number_x)
      return 0;

    *move = apos + right;
    return WIN;
  }

  return 0;
}


/* In positions like
 *
 * OOX..
 * XXO*.
 * x.X..
 * -----
 *
 * where the X stones to the left are being attacked, it is usually
 * important to start by considering the move at *. Thus we propose
 * the move at * with a high initial score.
 *
 * This function identifies the situation
 *
 * XO.?   bdf?
 * .X.o   aceg
 * ----   ----
 *
 * where a is a liberty of the attacked string, b is a stone of the
 * attacked string, and f is the considered moves.
 */

static void
edge_block_moves(int str, int apos, struct reading_moves *moves)
{
  int color = board[str];
  int other = OTHER_COLOR(color);
  int k, l;
  int cpos;
  int dpos;
  int epos;
  int fpos;
  int gpos;
  int hpos;

  /* Search for the right orientation. */
  for (k = 0; k < 4; k++) {
    int up = delta[k];
    if (ON_BOARD(apos - up))
      continue;
    if (board[apos + up] != color || !same_string(apos + up, str))
      return;
    
    for (l = 0; l < 2; l++) {
      int right = delta[(k+1)%4];
      if (l == 1)
	right = -right;

      cpos = apos + right;
      dpos = apos + right + up;

      if (board[cpos] != color || board[dpos] != other || countlib(dpos) > 1)
	continue;

      epos = cpos + right;
      fpos = dpos + right;
      gpos = epos + right;
      hpos = apos - right;
      
      if (!ON_BOARD(epos))
	continue;
      
      if (board[epos] == EMPTY && board[fpos] == EMPTY 
	  && (board[gpos] != color))
	ADD_CANDIDATE_MOVE(fpos, 30, *moves);
    }
  }
}

/* ================================================================ */
/*            Defending by attacking surrounding strings            */
/* ================================================================ */

/* Add the chainbreaking moves relative to the string (str) to the
 * (moves) struct.
 */
static void
break_chain_moves(int str, struct reading_moves *moves)
{
  int r;
  int xpos;
  int adj, adjs[MAXCHAIN];
  
  /* Find links in atari. */
  adj = chainlinks2(str, adjs, 1);
  
  for (r = 0; r < adj; r++) {
    findlib(adjs[r], 1, &xpos);
    ADD_CANDIDATE_MOVE(xpos, 1, *moves);
  }
}


/*
 * Find moves which immediately capture chain links with 2
 * liberties, in the sense that the links cannot escape atari.
 *
 * The used heuristics are slightly sloppy, so useless moves may
 * appear occasionally. This should, however, only lead to slightly
 * worse performance but not to incorrect results.
 */
static void
break_chain2_efficient_moves(int str, struct reading_moves *moves)
{
  int r;
  int adj, adjs[MAXCHAIN];
  
  /* Find links with 2 liberties. */
  adj = chainlinks2(str, adjs, 2);
  
  for (r = 0; r < adj; r++)
    do_find_break_chain2_efficient_moves(str, adjs[r], moves);
}


/* Helper function for break_chain2_efficient_moves(). */
static void
do_find_break_chain2_efficient_moves(int str, int adj,
				     struct reading_moves *moves)
{
  int color = board[str];
  int other = OTHER_COLOR(color);
  int k;
  int adj2, adjs2[MAXCHAIN];
  int libs[2];
  int pos1;
  int pos2;
  ASSERT1(countlib(adj) == 2, adj);
  
  adj2 = chainlinks2(adj, adjs2, 1);
  if (adj2 == 1 && countlib(str) > 2) {
    int apos;
    findlib(adjs2[0], 1, &apos);
    if (!is_self_atari(apos, color))
      ADD_CANDIDATE_MOVE(apos, 0, *moves);
    return;
  }
  
  if (adj2 > 1)
    return;
    
  findlib(adj, 2, libs);
  for (k = 0; k < 2; k++)
    if (approxlib(libs[k], other, 3, NULL) <= 2
	&& !is_self_atari(libs[1 - k], color))
      ADD_CANDIDATE_MOVE(libs[1 - k], 0, *moves);
  
  /* A common special case is this kind of edge position
   * 
   * ..XXX.
   * X.XOO.
   * XOOX*.
   * ......
   * ------
   *
   * where a move at * is most effective for saving the two stones
   * to the left.
   *
   * The code below tries to identify this case. We use the crude
   * heuristic that the two liberties of the X stone we want to
   * capture should be placed diagonally and that one liberty should
   * be on the edge. Then we propose to play the other liberty.
   * Notice that both moves may be proposed when attacking a stone
   * on 2-2.
   *
   * Update: This was too crude. Also require that the X stone is on
   * the second line and that the proposed move is not a self-atari.
   */
  if (libs[0] != SW(libs[1])
      && libs[0] != NW(libs[1])
      && libs[0] != NE(libs[1])
      && libs[0] != SE(libs[1]))
    return;

  /* Since we know that the two liberties are diagonal, the following
   * construction gives the two vertices "between" the liberties.
   */
  pos1 = NORTH(gg_max(libs[0], libs[1]));
  pos2 = SOUTH(gg_min(libs[0], libs[1]));
  if ((board[pos1] != other
       || !is_edge_vertex(pos2)
       || !same_string(pos1, adj))
      && (board[pos2] != other
	  || !is_edge_vertex(pos1)
	  || !same_string(pos2, adj)))
    return;

  if (is_edge_vertex(libs[0]) && !is_self_atari(libs[1], color))
    ADD_CANDIDATE_MOVE(libs[1], 1, *moves);

  if (is_edge_vertex(libs[1]) && !is_self_atari(libs[0], color))
    ADD_CANDIDATE_MOVE(libs[0], 1, *moves);
}

/*
 * (str) points to a group with two liberties. break_chain2_moves
 * tries to defend this worm by attacking * a neighbouring worm with
 * two liberties.
 * This is done by playing on either of its liberties
 * (if (require_safe) is true these are only used if they are not
 * self-ataris), taking a neighbour out of atari or by backfilling if
 * both liberties are self-ataris.
 */

static void
break_chain2_moves(int str, struct reading_moves *moves, int require_safe)
{
  int color = board[str];
  int other = OTHER_COLOR(color);
  int r;
  int k;
  int apos;
  int adj;
  int adjs[MAXCHAIN];
  int dummy_adjs[MAXCHAIN];
  int libs[2];
  int unsafe[2];

  adj = chainlinks2(str, adjs, 2);
  
  for (r = 0; r < adj; r++) {
    apos = adjs[r];

    /* If stackp > backfill_depth, don't bother playing liberties of
     * 2-liberty strings if those also have at least one neighbor in
     * atari. This is intended to solve reading:171 and generally reduce
     * the number of nodes.
     *
     * FIXME: We may need to still accept these moves if they happen to
     * take the ataried string out of atari.
     */
    if (stackp > backfill_depth
	&& chainlinks2(apos, dummy_adjs, 1) > 0)
      continue;
      
    findlib(apos, 2, libs);
    for (k = 0; k < 2; k++) {
      unsafe[k] = is_self_atari(libs[k], color);
      if (!unsafe[k]
	  || is_ko(libs[k], color, NULL)
	  || (!require_safe
	      && approxlib(libs[k], other, 5, NULL) < 5))
	ADD_CANDIDATE_MOVE(libs[k], 0, *moves);
    }

    if (stackp <= backfill2_depth || have_common_lib(str, apos, NULL)) {
      break_chain_moves(apos, moves);
      if (unsafe[0] && unsafe[1]) {
	int libs2[3];
	for (k = 0; k < 2; k++) {
	  if (approxlib(libs[k], other, 3, libs2) == 2) {
	    int s;
	    for (s = 0; s < 2; s++)
	      if (!is_self_atari(libs2[s], color))
		ADD_CANDIDATE_MOVE(libs2[s], 0, *moves);
	  }
	}
      }
    }
  }
}

/*
 * (str) points to a group to be defended. 
 * break_chain2_defense_moves is a wrapper around break_chain2_moves.
 * It devalues all entries by 2.
 *
 * Rationale: Otherwise, these moves get overvalued by order_moves. In
 * particular, if there is both a direct and a break_chain2 defense,
 * then the latter one might be just an irrelevant intermediate forcing
 * move. Hence, we should rather return the direct defense.
 */

static void
break_chain2_defense_moves(int str, struct reading_moves *moves)
{
  int saved_num_moves = moves->num;
  int k;

  break_chain2_moves(str, moves, !(stackp <= backfill_depth));
  for (k = saved_num_moves; k < moves->num; k++)
    moves->score[k] = -2;
}


/*
 * (str) points to a group.
 * If there is a string in the surrounding chain having
 * exactly three liberties whose attack leads to the rescue of
 * (str), break_chain3_moves(str, *moves) adds attack moves against
 * the surrounding string as candidate moves.
 */

static void
break_chain3_moves(int str, struct reading_moves *moves)
{
  int color = board[str];
  int other = OTHER_COLOR(color);
  int r;
  int k;
  int u = 0, v;
  int apos;
  int adj;
  int adjs[MAXCHAIN];
  int libs[3];
  int possible_moves[MAX_MOVES];
  int mw[BOARDMAX];

  memset(mw, 0, sizeof(mw));
  
  adj = chainlinks2(str, adjs, 3);
  for (r = 0; r < adj; r++) {
    int lib1 = 0, lib2 = 0, lib3 = 0;
    apos = adjs[r];

    /* We make a list in the (adjs) array of the liberties
     * of boundary strings having exactly three liberties. We mark
     * each liberty in the mw array so that we do not list any
     * more than once.
     */
    findlib(apos, 3, libs);

    /* If the 3 liberty chain easily can run away through one of the
     * liberties, we don't play on any of the other liberties.
     */
    lib1 = approxlib(libs[0], other, 4, NULL);
    lib2 = approxlib(libs[1], other, 4, NULL);
    if (lib1 >= 4 && lib2 >= 4)
      continue;
    lib3 = approxlib(libs[2], other, 4, NULL);

    if ((lib1 >= 4 || lib2 >= 4) && lib3 >= 4)
      continue;

    if (lib1 >= 4 && !mw[libs[0]]) {
      mw[libs[0]] = 1;
      possible_moves[u++] = libs[0];
      continue;
    }
    
    if (lib2 >= 4 && !mw[libs[1]]) {
      mw[libs[1]] = 1;
      possible_moves[u++] = libs[1];
      continue;
    }
    
    if (lib3 >= 4 && !mw[libs[2]]) {
      mw[libs[2]] = 1;
      possible_moves[u++] = libs[2];
      continue;
    }

    /* No easy escape, try all liberties. */
    for (k = 0; k < 3; k++) {
      if (!mw[libs[k]]) {
	mw[libs[k]] = 1;
	possible_moves[u++] = libs[k];
      }
    }
  }
  

  for (v = 0; v < u; v++) {
    /* We do not wish to consider the move if it can be 
     * immediately recaptured, unless stackp < backfill2_depth.
     * Beyond backfill2_depth, the necessary capturing move might not
     * get generated in follow-up for the attacker.
     * (This currently only makes a difference at stackp == backfill2_depth.)
     */
    int xpos = possible_moves[v];
    if (stackp < backfill2_depth
	|| approxlib(xpos, color, 2, NULL) > 1)
      /* We use a negative initial score here as we prefer to find
       * direct defense moves.
       */
      ADD_CANDIDATE_MOVE(xpos, -2, *moves);
  }
}

/* This function looks for moves attacking those components
 * of the surrounding chain of the superstring (see find_superstring
 * for the definition) which have fewer than liberty_cap liberties,
 * and which are not adjacent to the string itself, since those
 * are tested by break_chain_moves.
 */
static void
superstring_breakchain_moves(int str, int liberty_cap,
    			     struct reading_moves *moves)
{
  int adj;
  int adjs[MAXCHAIN];
  int k;
  int apos;

  proper_superstring_chainlinks(str, &adj, adjs, liberty_cap);
  for (k = 0; k < adj; k++) {
    int liberties = countlib(adjs[k]);
    if (liberties == 1) {
      findlib(adjs[k], 1, &apos);
      ADD_CANDIDATE_MOVE(apos, 0, *moves);
    }
    else if (liberties == 2)
      do_find_break_chain2_efficient_moves(str, adjs[k], moves);
  }
}

/*
 * If str points to a group, double_atari_chain2_moves() adds all moves
 * which make a double atari on some strings in the surrounding chain
 * to the (movei[], movej[]) arrays.
 */

static void
double_atari_chain2_moves(int str, struct reading_moves *moves)
{
  int r, k;
  int apos;
  int adj;
  int adjs[MAXCHAIN];
  int libs[2];
  int mw[BOARDMAX];

  memset(mw, 0, sizeof(mw));

  adj = chainlinks2(str, adjs, 2);
  for (r = 0; r < adj; r++) {
    apos = adjs[r];
    findlib(apos, 2, libs);
    for (k = 0; k < 2; k++) {
      mw[libs[k]]++;
      if (mw[libs[k]] == 2) {
	/* Found a double atari, but don't play there unless the move
         * is safe for the defender.
	 */
	if (!is_self_atari(libs[k], board[str]))
	  ADD_CANDIDATE_MOVE(libs[k], 1, *moves);
      }
    }
  }
}


/*
 * Returns true if the move is in a given list of moves.
 */

static int
in_list(int move, int num_moves, int *moves)
{
  int k;

  for (k = 0; k < num_moves; k++)
    if (moves[k] == move)
      return 1;
  return 0;
}


/* ================================================================ */
/*                          Move ordering                           */
/* ================================================================ */

/* Parameters used in the ordering of moves to try in the tactical
 * reading.
 */

/*                                              0   1   2   3   4  >4  */
static int defend_lib_score[6]              = {-5, -4,  0,  3,  5, 50};
static int defend_not_adjacent_lib_score[5] = { 0,  0,  2,  3,  5};
static int defend_capture_score[6]          = { 0,  6,  9, 13, 18, 24};
static int defend_atari_score[6]            = { 0,  2,  4,  6,  7, 8};
static int defend_save_score[6]             = { 0,  3,  6,  8, 10, 12};
static int defend_open_score[5]             = { 0,  1,  2,  3,  4};
static int attack_own_lib_score[5]          = {10, -4,  2,  3,  4};
static int attack_string_lib_score[6]       = {-5,  2,  3,  7, 10, 19};
static int attack_capture_score[6]          = {-4,  4, 10, 15, 20, 25};
static int attack_save_score[6]             = { 0, 10, 13, 18, 21, 24};
static int attack_open_score[5]             = { 0,  0,  2,  4,  4};
static int defend_not_edge_score            = 5;
static int attack_not_edge_score            = 1;
static int attack_ko_score                  = -15;
static int cannot_defend_penalty            = -20;
static int safe_atari_score                 = 8;


/* The string at (str) is under attack. The moves.num moves in
 * (moves) for color have been deemed interesting in
 * the attack or defense of the group. Most of these moves will be
 * immediate liberties of the group.
 *
 * This function orders the moves in the order where the move most
 * likely to succeed to attack or defend the string will be first and
 * so on.
 *
 * Currently, this is defined as:
 * 1) Moves which let the defending string get more liberties are more
 *    interesting.
 * 2) Moves adjacent to the most open liberties are more 
 *    interesting than those with fewer open liberties.
 * 3) Moves on the edge are less interesting.
 *
 * Moves below first_move are ignored and assumed to be sorted already.
 */

static void
order_moves(int str, struct reading_moves *moves, int color,
	    const char *funcname, int first_move)
{
  int string_color = board[str];
  int string_libs = countlib(str);
  int r;
  int i, j;
  int maxscore;
  int max_at;

  /* don't bother sorting if only one move, or none at all */
  if (moves->num - first_move < 2)
    return;

  /* Assign a score to each move. */
  for (r = first_move; r < moves->num; r++) {
    int move = moves->pos[r];

    /* Look at the neighbors of this move and count the things we
     * find. Friendly and opponent stones are related to color, i.e.
     * the player to move, not to the color of the string.
     */
    int number_edges       = 0; /* outside board */
    int number_same_string = 0; /* the string being attacked */
    int number_own         = 0; /* friendly stone */
    int number_opponent    = 0; /* opponent stone */
    int captured_stones    = 0; /* number of stones captured by this move */
    int threatened_stones  = 0; /* number of stones threatened by this move */
    int saved_stones       = 0; /* number of stones in atari saved */
    int number_open        = 0; /* empty intersection */

    /* We let the incremental_board code do the heavy work. */
    incremental_order_moves(move, color, str, &number_edges,
			    &number_same_string, &number_own,
			    &number_opponent, &captured_stones,
			    &threatened_stones, &saved_stones, &number_open);

    if (0)
      gprintf("%o %1m values: %d %d %d %d %d %d %d %d\n", move, number_edges,
	      number_same_string, number_own, number_opponent, captured_stones,
	      threatened_stones, saved_stones, number_open);
    
    /* Different score strategies depending on whether the move is
     * attacking or defending the string.
     */
    if (color == string_color) {
      /* Defense move.
       *
       * 1) Add twice the number of liberties the group receives by
       *    extending to the intersection of the move, if more than one.
       *    Only applicable if the move is adjacent to the group.
       */
      
      int libs = approxlib(move, color, 10, NULL);
      if (number_same_string > 0) {
	if (libs > 5 || (libs == 4 && stackp > fourlib_depth))
	  moves->score[r] += defend_lib_score[5] + (libs - 4);
	else
	  moves->score[r] += defend_lib_score[libs];
      }
      else {
	/* Add points for the number of liberties the played stone
         * obtains when not adjacent to the attacked string.
	 */
	if (libs > 4)
	  moves->score[r] += defend_not_adjacent_lib_score[4];
	else
	  moves->score[r] += defend_not_adjacent_lib_score[libs];
      }
      
      /* 2) Add the number of open liberties near the move to its score. */
      gg_assert(number_open <= 4);
      moves->score[r] += defend_open_score[number_open];
      
      /* 3) Add a bonus if the move is not on the edge. 
       */
      if (number_edges == 0 || captured_stones > 0)
	moves->score[r] += defend_not_edge_score;
      
      /* 4) Add thrice the number of captured stones. */
      if (captured_stones <= 5)
	moves->score[r] += defend_capture_score[captured_stones];
      else
	moves->score[r] += defend_capture_score[5] + captured_stones;

      /* 5) Add points for stones put into atari, unless this is a
       *    self atari.
       */
      if (libs + captured_stones > 1) {
	if (threatened_stones <= 5)
	  moves->score[r] += defend_atari_score[threatened_stones];
	else
	  moves->score[r] += defend_atari_score[5] + threatened_stones;
      }

      /* 6) Add a bonus for saved stones. */
      if (saved_stones <= 5)
	moves->score[r] += defend_save_score[saved_stones];
      else
	moves->score[r] += defend_save_score[5];
    }
    else {
      /* Attack move.
       *
       * 1) Add the number of liberties the attacker gets when playing
       *    there, but never more than four.
       */
      int libs = approxlib(move, color, 4, NULL);
      if (libs > 4)
	libs = 4;
      moves->score[r] += attack_own_lib_score[libs];

      if (libs == 0 && captured_stones == 1)
	moves->score[r] += attack_ko_score;
      
      /* 2) If the move is not a self atari and adjacent to the
       *    string, add the number of liberties the opponent would
       *    gain by playing there. If the string has two liberties,
       *    self-ataris are also ok since they may be snapbacks, but
       *    only if a single stone is sacrificed.
       */
      if ((libs + captured_stones > 1 || (string_libs <= 2 && number_own == 0))
	  && number_same_string > 0) {
	int safe_atari;
	int liberties = approxlib(move, string_color, 5, NULL);
	if (liberties > 5 || (liberties == 4 && stackp > fourlib_depth))
	  liberties = 5;
	moves->score[r] += attack_string_lib_score[liberties];

	safe_atari = (string_libs <= 2 && libs + captured_stones > 1);
	/* The defender can't play here without getting into atari, so
         * we probably souldn't either.
	 */
	if (liberties == 1 && saved_stones == 0 && !safe_atari)
	  moves->score[r] += cannot_defend_penalty;

	/* Bonus if we put the attacked string into atari without
         * ourselves getting into atari.
	 */
	if (safe_atari)
	  moves->score[r] += safe_atari_score;
      }
      
      /* 3) Add the number of open liberties near the move to its score. */
      gg_assert(number_open <= 4);
      moves->score[r] += attack_open_score[number_open];
      
      /* 4) Add a bonus if the move is not on the edge. */
      if (number_edges == 0)
	moves->score[r] += attack_not_edge_score;
      
      /* 5) Add twice the number of captured stones. */
      if (captured_stones <= 5)
	moves->score[r] += attack_capture_score[captured_stones];
      else
	moves->score[r] += attack_capture_score[5];

      /* 6) Add a bonus for saved stones. */
      if (saved_stones <= 5)
	moves->score[r] += attack_save_score[saved_stones];
      else
	moves->score[r] += attack_save_score[5];
    }
  }
  
  /* Now sort the moves.  We use selection sort since this array will
   * probably never be more than 10 moves long.  In this case, the
   * overhead imposed by quicksort will probably overshadow the gains
   * given by the O(n*log(n)) behaviour over the O(n^2) behaviour of
   * selection sort.
   */
  for (i = first_move; i < moves->num-1; i++) {

    /* Find the move with the biggest score. */
    maxscore = moves->score[i];
    max_at = 0; /* This is slightly faster than max_at = i. */
    for (j = i + 1; j < moves->num; j++) {
      if (moves->score[j] > maxscore) {
	maxscore = moves->score[j];
	max_at = j;
      }
    }

    /* Now exchange the move at i with the move at max_at.
     * Don't forget to exchange the scores as well.
     */
    if (max_at != 0) {

      int temp = moves->pos[max_at];

      moves->pos[max_at] = moves->pos[i];
      moves->score[max_at] = moves->score[i];

      moves->pos[i] = temp;
      moves->score[i] = maxscore;
    }
  }

  if (0) {
    gprintf("%oVariation %d:\n", count_variations);
    for (i = first_move; i < moves->num; i++)
      gprintf("%o  %1M %d\n", moves->pos[i], moves->score[i]);
  }

  if (sgf_dumptree) {
    char buf[500];
    char *pos;
    int chars;
    sprintf(buf, "Move order for %s: %n", funcname, &chars);
    pos = buf + chars;
    for (i = first_move; i < moves->num; i++) {
      sprintf(pos, "%c%d (%d) %n",
	      J(moves->pos[i]) + 'A' + (J(moves->pos[i]) >= 8),
	      board_size - I(moves->pos[i]), moves->score[i], &chars);
      pos += chars;
    }
    sgftreeAddComment(sgf_dumptree, buf);
  }
}


/* Set new values for the move ordering parameters. */
void
tune_move_ordering(int params[MOVE_ORDERING_PARAMETERS])
{
  int k;
  for (k = 0; k < 6; k++) {
    defend_lib_score[k]                = params[k];
    if (k < 5)
      defend_not_adjacent_lib_score[k] = params[k + 6];
    defend_capture_score[k]            = params[k + 11];
    defend_atari_score[k]              = params[k + 17];
    defend_save_score[k]               = params[k + 23];
    if (k < 5) {
      defend_open_score[k]             = params[k + 29];
      attack_own_lib_score[k]          = params[k + 34];
    }
    attack_string_lib_score[k]         = params[k + 39];
    attack_capture_score[k]            = params[k + 45];
    attack_save_score[k]               = params[k + 51];
    if (k < 5)
      attack_open_score[k]             = params[k + 57];
  }
  defend_not_edge_score                = params[62];
  attack_not_edge_score                = params[63];
  attack_ko_score                      = params[64];
  cannot_defend_penalty                = params[65];
  safe_atari_score                     = params[66];

  if (verbose) {
    gprintf("static int defend_lib_score[6]              = {%d, %d, %d, %d, %d, %d};\n",
	    defend_lib_score[0], defend_lib_score[1],
	    defend_lib_score[2], defend_lib_score[3],
	    defend_lib_score[4], defend_lib_score[5]);
    gprintf("static int defend_not_adjacent_lib_score[5] = {%d, %d, %d, %d, %d};\n",
	    defend_not_adjacent_lib_score[0], defend_not_adjacent_lib_score[1],
	    defend_not_adjacent_lib_score[2], defend_not_adjacent_lib_score[3],
	    defend_not_adjacent_lib_score[4]);
    gprintf("static int defend_capture_score[6]          = {%d, %d, %d, %d, %d, %d};\n",
	    defend_capture_score[0], defend_capture_score[1],
	    defend_capture_score[2], defend_capture_score[3],
	    defend_capture_score[4], defend_capture_score[5]);
    gprintf("static int defend_atari_score[6]            = {%d, %d, %d, %d, %d, %d};\n",
	    defend_atari_score[0], defend_atari_score[1],
	    defend_atari_score[2], defend_atari_score[3],
	    defend_atari_score[4], defend_atari_score[5]);
    gprintf("static int defend_save_score[6]             = {%d, %d, %d, %d, %d, %d};\n",
	    defend_save_score[0], defend_save_score[1],
	    defend_save_score[2], defend_save_score[3],
	    defend_save_score[4], defend_save_score[5]);
    gprintf("static int defend_open_score[5]             = {%d, %d, %d, %d, %d};\n",
	    defend_open_score[0], defend_open_score[1],
	    defend_open_score[2], defend_open_score[3],
	    defend_open_score[4]);
    gprintf("static int attack_own_lib_score[5]          = {%d, %d, %d, %d, %d};\n",
	    attack_own_lib_score[0], attack_own_lib_score[1],
	    attack_own_lib_score[2], attack_own_lib_score[3],
	    attack_own_lib_score[4]);
    gprintf("static int attack_string_lib_score[6]       = {%d, %d, %d, %d, %d, %d};\n",
	    attack_string_lib_score[0], attack_string_lib_score[1],
	    attack_string_lib_score[2], attack_string_lib_score[3],
	    attack_string_lib_score[4], attack_string_lib_score[5]);
    gprintf("static int attack_capture_score[6]          = {%d, %d, %d, %d, %d, %d};\n",
	    attack_capture_score[0], attack_capture_score[1],
	    attack_capture_score[2], attack_capture_score[3],
	    attack_capture_score[4], attack_capture_score[5]);
    gprintf("static int attack_save_score[6]             = {%d, %d, %d, %d, %d, %d};\n",
	    attack_save_score[0], attack_save_score[1],
	    attack_save_score[2], attack_save_score[3],
	    attack_save_score[4], attack_save_score[5]);
    gprintf("static int attack_open_score[5]             = {%d, %d, %d, %d, %d};\n",
	    attack_open_score[0], attack_open_score[1],
	    attack_open_score[2], attack_open_score[3],
	    attack_open_score[4]);
    gprintf("static int defend_not_edge_score            = %d;\n", defend_not_edge_score);
    gprintf("static int attack_not_edge_score            = %d;\n", attack_not_edge_score);
    gprintf("static int attack_ko_score                  = %d;\n", attack_ko_score);
    gprintf("static int cannot_defend_penalty            = %d;\n", cannot_defend_penalty);
    gprintf("static int safe_atari_score                 = %d;\n", safe_atari_score);
  }
}

/*
 * set_up_snapback_moves() is called with (str) a string having a
 * single liberty at (lib).
 *
 * This adds moves which may defend a string in atari by capturing a
 * neighbor in a snapback. One example is this position:
 *
 * OOOOO
 * OXXXO
 * OX.OX
 * OXOXX
 * OX*..
 * -----
 *
 * This code also finds the move * to defend the lone O stone with ko
 * in this position:
 *
 * |XXXXX
 * |XOOOX
 * |OX.OO
 * |.*...
 * +-----
 *
 */

static void
set_up_snapback_moves(int str, int lib, struct reading_moves *moves)
{
  int color = board[str];
  int other = OTHER_COLOR(color);
  int libs2[2];

  ASSERT1(countlib(str) == 1, str);

  /* This can only work if our string is a single stone and the
   * opponent is short of liberties.
   */
  if (stackp <= backfill_depth
      && countstones(str) == 1
      && approxlib(lib, other, 2, libs2) == 1
      && !is_self_atari(libs2[0], color))
    ADD_CANDIDATE_MOVE(libs2[0], 0, *moves);
}




/* ================================================================ */
/*                         Reading utilities                        */
/* ================================================================ */


static int safe_move_cache[BOARDMAX][2];
static int safe_move_cache_when[BOARDMAX][2];
static void clear_safe_move_cache(void);

static void
clear_safe_move_cache(void)
{
  int k;

  for (k = BOARDMIN; k < BOARDMAX; k++) {
    safe_move_cache_when[k][0] = -1;
    safe_move_cache_when[k][1] = -1;
  }
}

/* safe_move(move, color) checks whether a move at (move) is illegal
 * or can immediately be captured. If stackp==0 the result is cached.
 * If the move only can be captured by a ko, it's considered safe.
 * This may or may not be a good convention.
 *
 * For performance reasons, the result of this function is cached.
 */

int
safe_move(int move, int color)
{
  int safe = 0;
  static int initialized = 0;
  
  if (!initialized) {
    clear_safe_move_cache();
    initialized = 1;
  }

  /* If we have this position cached, use the previous value. */
  if (stackp == 0
      && safe_move_cache_when[move][color==BLACK] == position_number)
    return safe_move_cache[move][color==BLACK];

  /* Otherwise calculate the value... */
  if (trymove(move, color, "safe_move-A", 0, EMPTY, 0)) {
    safe = REVERSE_RESULT(attack(move, NULL));
    popgo();
  }
  else if (is_ko(move, color, NULL)
	   && tryko(move, color, "safe_move-B", EMPTY, 0)) {
    if (do_attack(move, NULL, color, move) != WIN)
      safe = KO_B;
    else
      safe = 0;
    popgo();
  }

  /* ...and store it in the cache.
   * FIXME: Only store result in cache when we're working at
   * full depth.
   *
   * Comment: This is currently not a problem since no reduced depth
   * reading is performed.
   */
  if (stackp == 0) {
    if (0)
      gprintf("Safe move at %1m for %s cached when depth=%d, position number=%d\n",
	      move, color_to_string(color), depth, position_number);
    safe_move_cache_when[move][color==BLACK] = position_number;
    safe_move_cache[move][color==BLACK] = safe;
  }

  return safe;
}


/* ===================== Statistics  ============================= */


/* Clear statistics. */
void
reset_reading_node_counter()
{
  reading_node_counter = 0;
}


/* Retrieve statistics. */
int
get_reading_node_counter()
{
  return reading_node_counter;
}

/* ============ Reading shadow =============== */

/* Draw the reading shadow, for debugging purposes */

void
draw_reading_shadow()
{
  int i, j, ii;
  int c = ' ';

  start_draw_board();
  
  for (i = 0; i < board_size; i++) {
    ii = board_size - i;
    fprintf(stderr, "\n%2d", ii);
    
    for (j = 0; j < board_size; j++) {
      if (!shadow[POS(i, j)] && BOARD(i, j) == EMPTY)
	c = '.';
      else if (!shadow[POS(i, j)] && BOARD(i, j) == WHITE)
	c = 'O';
      else if (!shadow[POS(i, j)] && BOARD(i, j) == BLACK)
	c = 'X';
      if (shadow[POS(i, j)] && BOARD(i, j) == EMPTY)
	c = ',';
      else if (shadow[POS(i, j)] && BOARD(i, j) == WHITE)
	c = 'o';
      else if (shadow[POS(i, j)] && BOARD(i, j) == BLACK)
	c = 'x';
      
      fprintf(stderr, " %c", c);
    }
    
    fprintf(stderr, " %d", ii);
  }

  end_draw_board();
}


/* ================================================================ */
/*              Code for special purposes.                          */
/* ================================================================ */

/* simple_ladder(str, &move) tries to capture a string (str)
 * with exactly two liberties under simplified assumptions, which are
 * adequate in a ladder. The rules are as follows:
 *
 * 1. The attacker is allowed to play at each of the two liberties,
 *    but no other move. If the move was legal, the string now has
 *    exactly one liberty.
 * 2. The defender must move out of atari. This can only be done by
 *    either extending at the liberty or capturing a neighboring
 *    string which was in atari. All such moves may be tested.
 * 3. Depending on the resulting number of liberties of the string
 *    after the defender's move, we value each node as follows:
 *
 *    3 or more liberties:           the attack has failed
 *    2 liberties:                   recurse
 *    1 liberty:                     the attack has succeeded
 *
 *    illegal move for the defender: successful attack
 *    illegal move for the attacker: failed attack
 *
 * Return codes are as usual 0 for failure, WIN for success, KO_A for
 * a ko where the defender must make the first ko threat and KO_B for
 * a ko where the attacked has to make the first threat. If the attack
 * was successful, (*move) contains the attacking move, unless it is a
 * null pointer.
 *
 * The differences compared to the attack2()/defend1() combination for
 * reading ladders is that this one is a strict ladder reader which
 * never allows the defender to have more than one liberty when it's
 * in turn to move. This has a number of consequences.
 *
 * 1. This function will miss tactical captures involving other
 *    techniques than the ladder.
 *
 * 2. This function is faster because it gives up faster when the
 *    ladder doesn't work. In particular it can't branch out in a huge
 *    tree of exotic variations.
 *
 * 3. This function always reads ladders to the very end. There are no
 *    depth limits or other assumptions to stop reading prematurely.
 *
 * 4. If this function returns WIN, it is guaranteed that the defender
 *    has no way whatsoever to escape, all possibilities are tried.
 *    The converse is definitely not true.
 */

int
simple_ladder(int str, int *move)
{
  return simple_ladder_attack(str, move, EMPTY, NO_MOVE);
}

static int
simple_ladder_attack(int str, int *move, int komaster, int kom_pos)
{
  int color = board[str];
  int other = OTHER_COLOR(color);
  int apos;
  int libs[2];
  int savemove = 0;
  int savecode = 0;
  int dcode;
  int k;
  struct reading_moves moves;

  SETUP_TRACE_INFO("simple_ladder_attack", str);
  reading_node_counter++;
  moves.num = 0;

  str = find_origin(str);
  ASSERT1(IS_STONE(board[str]), str);
  ASSERT1(countlib(str) == 2, str);

  /* Give up if we attacked depending on ko for too long. */
  if (stackp > depth + 20 && komaster == OTHER_COLOR(board[str])) {
    SGFTRACE(0, 0, NULL);
    if (move)
      *move = PASS_MOVE;
    return 0;
  }

  /* Get the two liberties of (str). */
  findlib(str, 2, libs);

  for (k = 0; k < 2; k++)
    ADD_CANDIDATE_MOVE(libs[k], 0, moves);

  order_moves(str, &moves, other, read_function_name, 0);

  for (k = 0; k < moves.num; k++) {
    int new_komaster;
    int new_kom_pos;
    int ko_move;

    apos = moves.pos[k];
    if (komaster_trymove(apos, other, "simple_ladder_attack", str,
			 komaster, kom_pos, &new_komaster, &new_kom_pos,
			 &ko_move, savecode == 0)) {
      if (!ko_move) {
	dcode = simple_ladder_defend(str, NULL, new_komaster, new_kom_pos);
	if (dcode != WIN) {
	  if (dcode == 0) {
	    popgo();
	    SGFTRACE(apos, WIN, "attack effective");
	    if (move)
	      *move = apos;
	    return WIN;
	  }
	  UPDATE_SAVED_KO_RESULT(savecode, savemove, dcode, apos);
	}
      }
      else {
	if (simple_ladder_defend(str, NULL, new_komaster, new_kom_pos) != WIN) {
	  savemove = apos;
	  savecode = KO_B;
	}
      }
      popgo();
    }
  }
  
  RETURN_RESULT(savecode, savemove, move, "saved move");
}


static int
simple_ladder_defend(int str, int *move, int komaster, int kom_pos)
{
  int color = board[str];
  int xpos;
  int lib;
  struct reading_moves moves;
  int savemove = 0;
  int savecode = 0;
  int k;

  SETUP_TRACE_INFO("simple_ladder_defend", str);
  reading_node_counter++;
  moves.num = 0;
  
  gg_assert(IS_STONE(board[str]));
  ASSERT1(countlib(str) == 1, str);

  /* lib will be the liberty of the string. */
  findlib(str, 1, &lib);

  moves.pos[0] = lib;
  moves.score[0] = 0;
  moves.num = 1;
  
  break_chain_moves(str, &moves);
  order_moves(str, &moves, color, read_function_name, 0);

  for (k = 0; k < moves.num; k++) {
    int new_komaster;
    int new_kom_pos;
    int ko_move;

    xpos = moves.pos[k];
    if (komaster_trymove(xpos, color, "simple_ladder_defend", str,
			 komaster, kom_pos,
			 &new_komaster, &new_kom_pos,
			 &ko_move, savecode == 0)) {
      int acode;
      int new_libs = countlib(str);
      if (new_libs > 2)
	acode = 0;
      else if (new_libs < 2)
	acode = WIN;
      else
	acode = simple_ladder_attack(str, NULL, new_komaster, new_kom_pos);
      popgo();
      
      if (!ko_move)
	CHECK_RESULT(savecode, savemove, acode, xpos, move,
	  	     "defense effective");
      else {
	if (acode != WIN) {
	  savemove = xpos;
	  savecode = KO_B;
	}
      }
    }
  }

  RETURN_RESULT(savecode, savemove, move, "saved move");
}


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
