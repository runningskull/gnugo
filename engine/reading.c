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
#include <stdarg.h>
#include <string.h>

#include "liberty.h"
#include "cache.h"

/* Size of array where candidate moves are stored. */
#define MAX_MOVES 50

#define ADD_CANDIDATE_MOVE(move, score, moves, scores, num_moves)\
  do {\
    int l;\
    for (l = 0; l < num_moves; l++)\
      if (moves[l] == (move)) {\
        if (scores[l] < score)\
          scores[l] = score;\
	break;\
      }\
    if ((l == num_moves) && (num_moves < MAX_MOVES)) {\
      moves[num_moves] = move;\
      scores[num_moves] = score;\
      (num_moves)++;\
    }\
  } while(0) \

#define REMOVE_CANDIDATE_MOVE(move, moves, scores, num_moves)\
  do {\
    int k, l;\
    for (k = 0; k < num_moves; k++) {\
      if (moves[k] == (move)) {\
        for (l = k; l < num_moves-1; l++) {\
	  moves[l] = moves[l+1];\
	  scores[l] = scores[l+1];\
	}\
        (num_moves)--;\
	break;\
      }\
    }\
  } while(0) \


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
static int restricted_defend1(int str, int *move, int komaster, int kom_pos,
			      int num_forbidden_moves, int *forbidden_moves);
static int defend2(int str, int *move, int komaster, int kom_pos);
static int defend3(int str, int *move, int komaster, int kom_pos);
static int defend4(int str, int *move, int komaster, int kom_pos);
static int special_rescue(int str, int lib, int *move, 
			  int komaster, int kom_pos);
static int special_rescue2(int str, int libs[2], int *move, 
			   int komaster, int kom_pos);
static int special_rescue3(int str, int libs[3], int *move, 
			   int komaster, int kom_pos);
static int special_rescue4(int str, int libs[3], int *move, 
			   int komaster, int kom_pos);
static void edge_clamp(int str, int moves[MAX_MOVES],
		       int scores[MAX_MOVES], int *num_moves);
static int do_attack(int str, int *move, int komaster, int kom_pos);
static int attack1(int str, int *move, int komaster, int kom_pos);
static int attack2(int str, int *move, int komaster, int kom_pos);
static int restricted_attack2(int str, int *move, int komaster, int kom_pos,
			      int num_forbidden_moves, int *forbidden_moves);
static int attack3(int str, int *move, int komaster, int kom_pos);
static int attack4(int str, int *move, int komaster, int kom_pos);
static int find_cap2(int str, int alib, int blib, int *move,
		     int komaster, int kom_pos);
static int find_cap3(int str, int *move, int komaster, int kom_pos);
static int special_attack2(int str, int libs[2], int *move,
			   int komaster, int kom_pos);
static int special_attack3(int str, int libs[2], int *move,
			   int komaster, int kom_pos);
static int special_attack4(int str, int libs[2], int *move,
			   int komaster, int kom_pos);
static int draw_back(int str, int *move, int komaster, int kom_pos);
static int edge_closing_backfill(int str, int apos, int *move);
static void edge_block(int str, int apos, int moves[MAX_MOVES],
		       int scores[MAX_MOVES], int *num_moves);
static void propose_edge_moves(int str, int *libs, int liberties,
			       int moves[MAX_MOVES], int scores[MAX_MOVES],
			       int *num_moves, int color);
static void break_chain_moves(int str, int moves[MAX_MOVES],
			      int scores[MAX_MOVES], int *num_moves);
static void break_chain2_efficient_moves(int str, int moves[MAX_MOVES],
					 int scores[MAX_MOVES],
					 int *num_moves);
static void break_chain2_moves(int str, int moves[MAX_MOVES],
			       int scores[MAX_MOVES], int *num_moves,
			       int require_safe);
static int break_chain2(int str, int *move, int komaster, int kom_pos);
static int break_chain3(int str, int *move, int komaster, int kom_pos);
static int superstring_breakchain(int str, int *move,
				  int komaster, int kom_pos,
				  int liberty_cap);
static void double_atari_chain2(int str, int moves[MAX_MOVES],
				int scores[MAX_MOVES], int *num_moves);
static void order_moves(int str, int num_moves, int *moves,
			int *scores, int color, const char *funcname);
static int naive_ladder(int str, int *move);
static int naive_ladder_defense(int str, int apos, int bpos,
				int color, int other);
static int naive_ladder_break_through(int str, int apos, int color, int other);
static int in_list(int move, int num_moves, int *moves);


/* ================================================================ */


/* Persistent reading cache to reuse read results between moves and
 * within the same move when one or more far away moves have been
 * played.
 */

#define MAX_CACHE_DEPTH 5

struct reading_cache {
  char board[BOARDMAX];
  int movenum;
  int nodes;
  int score;
  int remaining_depth;
  int routine; /* ATTACK or FIND_DEFENSE */
  int str;  /* contested string (origin) */
  int result;
  int move;    /* attack/defense point */
  int stack[MAX_CACHE_DEPTH];
  int move_color[MAX_CACHE_DEPTH];
};

#define MAX_READING_CACHE_SIZE 100
static struct reading_cache persistent_reading_cache[MAX_READING_CACHE_SIZE];
static int persistent_reading_cache_size = 0;

static void draw_active_area(char p[BOARDMAX]);
static int verify_stored_board(char p[BOARDMAX]);
static int search_persistent_reading_cache(int routine, int str,
					   int *result, int *move);
static void store_persistent_reading_cache(int routine, int str,
					   int result, int move, int nodes);
static void print_persistent_reading_cache_entry(int k);
static void mark_string_hotspot_values(float values[MAX_BOARD][MAX_BOARD],
				       int m, int n, float contribution);

/* Statistics. */
static int reading_node_counter = 0;

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

#define MIN_NODES_TO_REPORT 1000

int
attack(int str, int *move)
{
  int nodes_when_called = reading_node_counter;
  int result;
  int nodes;
  int origin;
  int the_move;

  /* Don't even spend time looking in the cache if there are more than
   * four liberties.
   */
  if (countlib(str) > 4)
    return 0;

  origin = find_origin(str);
  if (search_persistent_reading_cache(ATTACK, origin, &result, &the_move)) {
    if (move) *move = the_move;
    return result;
  }

  memset(shadow, 0, sizeof(shadow));
  result = do_attack(str, &the_move, EMPTY, 0);
  nodes = reading_node_counter - nodes_when_called;

  if (debug & DEBUG_READING_PERFORMANCE) {
    if (reading_node_counter - nodes_when_called >= MIN_NODES_TO_REPORT) {
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
  
  if (move) *move = the_move;
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
  int nodes_when_called = reading_node_counter;
  int result;
  int nodes;
  int origin;
  int the_move;

  /* Don't even spend time looking in the cache if there are more than
   * four liberties.
   */
  if (countlib(str) > 4) {
    if (move) *move = NO_MOVE;
    return WIN;
  }

  origin = find_origin(str);
  if (search_persistent_reading_cache(FIND_DEFENSE, origin, 
				      &result, &the_move)) {
    if (move) *move = the_move;
    return result;
  }

  memset(shadow, 0, sizeof(shadow));
  result = do_find_defense(str, &the_move, EMPTY, 0);
  nodes = reading_node_counter - nodes_when_called;

  if (debug & DEBUG_READING_PERFORMANCE) {
    if (reading_node_counter - nodes_when_called >= MIN_NODES_TO_REPORT) {
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
  
  if (move) *move = the_move;
  return result;
}


/* attack_and_defend(str, &acode, &attack_point,
 *                        &dcode, &defense_point)
 * is a frontend to the attack() and find_defense() functions, which
 * guarantees a consistent result. If a string cannot be attacked, 0
 * is returned and acode is 0. If a string can be attacked and
 * defended, WIN is returned, acode and dcode are both non-zero, and
 * (attack_point), (defense_point) both point to vertices on the board. 
 * If a stringcan be attacked but not defended, 0 is again returned, 
 * acode is non-zero, dcode is 0, and (ai, aj) point to a vertex 
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
    
    /* If find_defense() says the string is safe as is, we believe
     * this in favor of attack()'s opinion. Actually this is probably
     * incorrect, but we can't easily find a defense point to return.
     */
    if (dcode == WIN && dpos == NO_MOVE) {
      acode = 0;
      apos = NO_MOVE;
    }
  }

  if (attack_code)   *attack_code   = acode;
  if (attack_point)  *attack_point  = apos;
  if (defend_code)   *defend_code   = dcode;
  if (defense_point) *defense_point = dpos;

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
  int color = board[astr];
  ASSERT1(color != EMPTY , astr);
  ASSERT1(color == board[bstr], bstr);

  /* Start by attacking the string with the fewest liberties. On
   * average this seems to be slightly more efficient.
   */
  if (countlib(astr) <= countlib(bstr))
    return attack(astr, NULL) || attack(bstr, NULL);
  else
    return attack(bstr, NULL) || attack(astr, NULL);
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
  int cpos, dpos;
  int acode = 0;
  int dcode = 0;
  
  int color = board[astr];
  ASSERT1(color != EMPTY , astr);
  ASSERT1(color == board[bstr], bstr);

  attack_and_defend(astr, &acode, NULL, &dcode, &cpos);
  if (acode != 0) {
    a_threatened = 1;
    if (dcode == 0)
      return 0; /* (astr) already lost */
  }
  
  attack_and_defend(bstr, &acode, NULL, &dcode, &dpos);
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

  if (cpos == dpos)
    return WIN; /* Both strings can be attacked but also defended 
                 * by one move. */

  /* We also try each of the returned defense points and see whether
   * the other string can still be attacked. This still gives a
   * somewhat pessimistic estimation.
   */

  if (trymove(cpos, color, "defend_both-A", astr, EMPTY, NO_MOVE)) {
    if (board[bstr] && !attack(bstr, NULL)) {
      popgo();
      return WIN;
    }
    popgo();
  }
  
  if (trymove(dpos, color, "defend_both-B", bstr, EMPTY, NO_MOVE)) {
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
	  && (epos != cpos)
	  && (epos != dpos)) {
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
  ASSERT1(color != EMPTY , apos);
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
      if (!attack(dpos, &gpos)) {
	success = CUT;
	/* So now we can try to capture something. */
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
	      if (!board[bpos] 
		  || !board[cpos] 
		  || !defend_both(bpos, cpos))
		success = WIN;
	    }
	    popgo();
	  }
	  popgo();
	}
      }
    }
    popgo();
  }

  return success;
}


/* ================================================================ */
/*                     Global reading functions                     */
/* ================================================================ */

/* atari_atari(color, *i, *j) looks for a series of ataris on
 * strings of the other color culminating in the capture of
 * a string which is thought to be invulnerable by the reading
 * code. Such a move can be missed since it may be that each
 * string involved individually can be rescued, but nevertheless
 * one of them can be caught. The simplest example is a double
 * atari. The return value is the size of the smallest opponent
 * worm. 
 *
 * One danger with this scheme is that the first atari
 * tried might be irrelevant to the actual combination.
 * To detect this possibility, once we've found a combination,
 * we mark that first move as forbidden, then try again. If
 * no combination of the same size or larger turns up, then
 * the first move was indeed essential.
 *
 * For the purpose of the move generation, returns the
 * size of the smallest of the worms under attack.
 */

static int aa_status[BOARDMAX]; /* ALIVE, DEAD or CRITICAL */
static int forbidden[BOARDMAX];
static int get_aa_status(int pos);
static int do_atari_atari(int color, int *attack_point,
			  int *defense_point, int cpos,
			  int save_verbose, int minsize);

/* Set to 1 if you want verbose traces from this function. */

int
atari_atari(int color, int *move, int save_verbose)
{
  int m, n;
  int fpos;
  int aa_val;
  int other = OTHER_COLOR(color);

  /* Collect worm statuses of opponent's worms. We need to
   * know this because we only want to report unexpected
   * results. For example, we do not want to report success
   * if we find we can kill a worm which is already dead.
   * The worm status of empty points is set to UNKNOWN to signal
   * that stones added along the way need special attention.
   */
  if (aa_depth < 2)
    return 0;
  memset(forbidden, 0, sizeof(forbidden));
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      if (BOARD(m, n) == other) {
	if (dragon[m][n].matcher_status == DEAD)
	  aa_status[POS(m, n)] = DEAD;
	else if (worm[m][n].attack_code != 0) {
	  if (worm[m][n].defend_code != 0)
	    aa_status[POS(m, n)] = CRITICAL;
	  else
	    aa_status[POS(m, n)] = DEAD;
	}
	else
	  aa_status[POS(m, n)] = ALIVE;
      }
      else
	aa_status[POS(m, n)] = UNKNOWN;
    }

  /* reclassify a worm with 2 liberties as INSUBSTANTIAL if capturing
   * it does not result in a live group.
   */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) 
      if (BOARD(m, n) == other
	  && worm[m][n].origin == POS(m, n)
	  && worm[m][n].liberties == 2
	  && aa_status[POS(m, n)] == ALIVE
	  && !owl_substantial(m, n)) {
	int ti, tj;
	for (ti = 0; ti < board_size; ti++)
	  for (tj = 0; tj < board_size; tj++)
	    if (is_worm_origin(ti, tj, m, n))
	      aa_status[POS(ti, tj)] = INSUBSTANTIAL;
      }

  if (debug & DEBUG_ATARI_ATARI) {
    gprintf("atari_atari for %C\n", color);
    gprintf("aa_status: (ALIVE worms not listed)\n");
    for (m = 0; m < board_size; m++)
      for (n = 0; n < board_size; n++) {
	if (BOARD(m, n) == other && is_worm_origin(m, n, m, n)) {
	  const char *status = "UNKNOWN (shouldn't happen)";
	  if (aa_status[POS(m, n)] == DEAD)
	    status = "DEAD";
	  else if (aa_status[POS(m, n)] == CRITICAL)
	    status = "CRITICAL";
	  else if (aa_status[POS(m, n)] == INSUBSTANTIAL)
	    status = "INSUBSTANTIAL";

	  if (aa_status[POS(m, n)] != ALIVE)
	    gprintf("%M: %s\n", m, n, status);
	}
      }
  }

  aa_val = do_atari_atari(color, &fpos, NULL, NO_MOVE,
			  save_verbose, 0);

  if (aa_val == 0)
    return 0;

  /* We try excluding the first atari found and see if the
   * combination still works. Repeat until failure.
   */
  while (1) {
    int new_aa_val;
    forbidden[fpos] = 1;
    new_aa_val = do_atari_atari(color, &fpos, NULL, NO_MOVE,
				save_verbose, aa_val);

    /* The last do_atari_atari call fails. When do_atari_atari fails,
     * it does not change the value of (fpos), so these correspond
     * to a move that works and is necessary.
     */
    if (new_aa_val == 0) {
      if (move) *move = fpos;
      return aa_val;
    }
    aa_val = new_aa_val;
  }

  /* We'll never get here, but the compiler may be more happy if it
   * looks like we're returning something.
   */
  return 0;
}


/* Helper function for retrieving the aa_status for a string. We can't
 * reliably do this simply by looking up aa_status[m][n] since this is
 * only valid at vertices which were non-empty at the start of the
 * reading. For later added stones, we need to find their aa_status by
 * locating a part of the string which was a worm at the beginning of
 * the reading.
 */
static int
get_aa_status(int pos)
{
  int stones[MAX_BOARD * MAX_BOARD];
  int num_stones;
  int k;

  if (aa_status[pos] != UNKNOWN)
    return aa_status[pos];

  num_stones = findstones(pos, MAX_BOARD * MAX_BOARD, stones);
  for (k = 0; k < num_stones; k++)
    if (aa_status[stones[k]] != UNKNOWN)
      return aa_status[stones[k]];

  return UNKNOWN;
}

/* Helper function for atari_atari. Here worms is the number of
 * opponent worms involved in the combination, and (ci, cj) is
 * the location of the last friendly move played. Moves marked
 * with the forbidden array are not tried. If no move is found,
 * the values of *i and *j are not changed.
 *
 * If not NULL, *attacki, *attackj are left pointing to the location
 * of the attacking move, and *defendi, *defendj point to to
 * a move defending the combination. In rare cases a defensive
 * move might not be found. If a non-static function calling
 * do_atari_atari gets a return value of 1 but -1,-1 as the
 * defense point, this should be treated as equivalent to
 * a return value of 0.
 */

static int
do_atari_atari(int color, 
	       int *attack_point, int *defense_point, int cpos,
	       int save_verbose, int minsize)
{
  int m, n;
  int k;
  
  int other = OTHER_COLOR(color);

  if (debug & DEBUG_ATARI_ATARI) {
    gprintf("%odo_atari_atari: ");
    dump_stack();
    gprintf("%oforbidden moves: ");
    for (m = 0; m < board_size; m++)
      for (n = 0; n < board_size; n++) {
	if (forbidden[POS(m, n)])
	  gprintf("%o%m ", m, n);
      }
    gprintf("\n");
  }

  /* First look for strings adjacent to the last friendly move played
   * which can be unexpectedly attacked.
   */
  if (cpos != NO_MOVE)
    for (m = 0; m < board_size; m++)
      for (n = 0; n < board_size; n++) {
	int apos;

	if (BOARD(m, n) != other)
	  continue;

	if (POS(m, n) != find_origin(POS(m, n)))
	  continue;

	if (minsize > 0 && countstones2(m, n) < minsize)
	  continue;

	if (get_aa_status(POS(m, n)) != ALIVE
	    || !neighbor_of_string(cpos, POS(m, n)))
	  continue;
	
	if (debug & DEBUG_ATARI_ATARI)
	  gprintf("Considering attack of %m. depth = %d.\n", m, n, depth);
	if (attack(POS(m, n), &apos)) {
	  if (save_verbose || (debug & DEBUG_ATARI_ATARI)) {
	    gprintf("%oThe worm %m can be attacked at %1m after ", m, n,
		    apos);
	    dump_stack();
	  }	  
	  if (attack_point) *attack_point = apos;
	  
	  /* We look for a move defending the combination.
	   * Normally this is found by find_defense but failing
	   * that, if the attacking move is a safe move for color, 
	   * it probably defends.
	   */
	  if (defense_point) {
	    if (!find_defense(POS(m, n), defense_point)) {
	      if (safe_move(apos, other)) {
		*defense_point = apos;
	      }
	      /* No defense is found */
	      else {
		*defense_point = NO_MOVE;
	      }
	    }
	  }

	  DEBUG(DEBUG_ATARI_ATARI, "%oreturn value:%d (%m)\n",
		countstones2(m, n), m, n);
	  return countstones2(m, n);
	}
      }

  if (stackp > aa_depth)
    return 0;

  /* Next look for a string, not insubstantial, having exactly two
   * liberties and no boundary stones in atari. Atari, fill, then try
   * again. If that doesn't work, try the other atari.
   */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      int libs[2];
      int status;

      if (BOARD(m, n) != other) 
	continue;

      if (POS(m, n) != find_origin(POS(m, n)))
	continue;
      
      if (minsize > 0 && countstones2(m, n) < minsize)
	continue;

      status = get_aa_status(POS(m, n));
      if (status != ALIVE)
	continue;

      if (findlib(POS(m, n), 2, libs) != 2)
	continue;

      for (k = 0; k < 2; k++) {
	int apos = libs[k];
	int bpos;

	if (!forbidden[apos]
	    && accurate_approxlib(apos, color, 2, NULL) > 1) {
	  if (trymove(apos, color, "do_atari_atari-A", POS(m, n),
		       EMPTY, NO_MOVE)) {
	    /* try to defend the stone (m,n) which is in atari */
	    int aa_val = 0;

	    /* Because we know (m, n) is in atari there is a trivial
	     * attack and we can be sure find_defense() will give a
	     * useful defense point if it returns non-zero. Usually we
	     * would need to call attack_and_defend() to be certain of
	     * this.
	     *
	     * On the other hand, if there is no defense we have
	     * already been successful.
	     */
	    if (find_defense(POS(m, n), &bpos)
		&& trymove(bpos, other, "do_atari_atari-B", POS(m, n),
			    EMPTY, NO_MOVE)) {
	      /* These moves may have been irrelevant for later
               * reading, so in order to avoid horizon problems, we
               * need to temporarily increase the depth values.
	       */
	      increase_depth_values();
	      increase_depth_values();
	      aa_val = do_atari_atari(color, NULL, defense_point,
				      apos, save_verbose, minsize);
	      decrease_depth_values();
	      decrease_depth_values();
	      popgo();
	    }
	    else {
	      /* No way to save the ataried stone. We have been successful. */
	      popgo();
	      if (save_verbose || (debug & DEBUG_ATARI_ATARI)) {
		gprintf("%oThe worm %m can be attacked at %1m after ", m, n,
			apos);
		dump_stack();
	      }	  
	      if (attack_point) *attack_point = apos;
	      if (defense_point && !find_defense(POS(m, n), defense_point))
		*defense_point = NO_MOVE;
	      
	      DEBUG(DEBUG_ATARI_ATARI, "%oreturn value:%d (%m)\n",
		    countstones2(m, n), m, n);
	      return countstones2(m, n);
	    }

	    if (aa_val) {
	      /* The atari at (ai,aj) seems to work but we still
	       * must check there is not a better defense.
	       */
	      int cpos;
	      int res = restricted_defend1(POS(m, n), &cpos, EMPTY, 0, 
					   1, &bpos);
	      if (res) {
		if (trymove(cpos, other, "do_atari_atari-C", 
			     POS(m, n), EMPTY, NO_MOVE)) {
		  increase_depth_values();
		  increase_depth_values();
		  if (!do_atari_atari(color, NULL, defense_point,
				      apos, save_verbose, minsize)) 
		    aa_val = 0;
		  decrease_depth_values();
		  decrease_depth_values();
		  popgo();
		}
	      }
	      if (aa_val) {
		if (attack_point) *attack_point = apos;
		popgo();
		DEBUG(DEBUG_ATARI_ATARI, 
		      "%oreturn value:%d (min %d, %d (%m))\n",
		      gg_min(aa_val, countstones2(m, n)), aa_val,
		      countstones2(m, n), m, n);
		/* If no defense point is known and (ai,aj) is a safe
		 * move for other, it probably defends the combination.
		 */
		if (defense_point 
		    && *defense_point == NO_MOVE
		    && safe_move(apos, other)) {
		  *defense_point = apos;
		}
		return gg_min(aa_val, countstones2(m, n));
	      }
	    }
	    popgo();
	  }
	}
      }
    }
  return 0;
}

/* Ask the atari_atari code whether there appears any combination
 * attack which would capture at least minsize stones after playing at
 * (tpos). If this happens, (*move) points to a move which prevents
 * this blunder.
 *
 * FIXME: Most of the code below is common with atari_atari() and
 *        should be broken out of both functions.
 */
int
atari_atari_confirm_safety(int color, int tpos, int *move, int minsize)
{
  int m, n;
  int fpos;
  int defense_point, after_defense_point;
  int aa_val, after_aa_val;
  int other = OTHER_COLOR(color);

  /* If aa_depth is too small, we can't see any combination attacks,
   * so in this respect the move is safe enough.
   */
  if (aa_depth < 2)
    return 1;

  /* Collect worm statuses of opponent's worms. We need to
   * know this because we only want to report unexpected
   * results. For example, we do not want to report success
   * if we find we can kill a worm which is already dead.
   * The worm status of empty points is set to UNKNOWN to signal
   * that stones added along the way need special attention.
   */
  memset(forbidden, 0, sizeof(forbidden));
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      if (BOARD(m, n) == color) {
	if (dragon[m][n].matcher_status == DEAD)
	  aa_status[POS(m, n)] = DEAD;
	else if (worm[m][n].attack_code != 0) {
	  if (worm[m][n].defend_code != 0)
	    aa_status[POS(m, n)] = CRITICAL;
	  else
	    aa_status[POS(m, n)] = DEAD;
	}
	else
	  aa_status[POS(m, n)] = ALIVE;
      }
      else
	aa_status[POS(m, n)] = UNKNOWN;
    }
  
  /* reclassify a worm with 2 liberties as INSUBSTANTIAL if capturing
   * it does not result in a live group.
   */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) 
      if (BOARD(m, n) == color
	  && worm[m][n].origin == POS(m, n)
	  && worm[m][n].liberties == 2
	  && aa_status[POS(m, n)] == ALIVE
	  && !owl_substantial(m, n)) {
	int ui, uj;
	for (ui = 0; ui < board_size; ui++)
	  for (uj = 0; uj < board_size; uj++)
	    if (is_worm_origin(ui, uj, m, n))
	      aa_status[POS(ui, uj)] = INSUBSTANTIAL;
      }

  if (debug & DEBUG_ATARI_ATARI) {
    gprintf("atari_atari for %C\n", other);
    gprintf("aa_status: (ALIVE worms not listed)\n");
    for (m = 0; m < board_size; m++)
      for (n = 0; n < board_size; n++) {
	if (BOARD(m, n) == color && is_worm_origin(m, n, m, n)) {
	  const char *status = "UNKNOWN (shouldn't happen)";
	  if (aa_status[POS(m, n)] == DEAD)
	    status = "DEAD";
	  else if (aa_status[POS(m, n)] == CRITICAL)
	    status = "CRITICAL";
	  else if (aa_status[POS(m, n)] == INSUBSTANTIAL)
	    status = "INSUBSTANTIAL";
	  
	  if (aa_status[POS(m, n)] != ALIVE)
	    gprintf("%M: %s\n", m, n, status);
	}
      }
  }
  
  /* Accept illegal ko capture here. */
  if (!tryko(tpos, color, NULL, EMPTY, NO_MOVE))
    /* Really shouldn't happen. */
    abortgo(__FILE__, __LINE__, "trymove", I(tpos), J(tpos)); 
  increase_depth_values();

  aa_val = do_atari_atari(other, &fpos, &defense_point,
			  NO_MOVE, 0, minsize);
  after_aa_val = aa_val;

  if (aa_val == 0 || defense_point == NO_MOVE) {

  /* No sufficiently large combination attack, so the move is safe from
   * this danger.
   *   
   * On rare occasions do_atari_atari might find a combination
   * but no defense. In this case we assume that the combination
   * is illusory.
   */

    popgo();
    decrease_depth_values();
    return 1;
  }

  while (aa_val >= after_aa_val) {
    /* Try dropping moves from the combination and see if it still
     * works. What we really want is to get the proper defense move
     * into (*move).
     */
    after_defense_point = defense_point;
    forbidden[fpos] = 1;
    aa_val = do_atari_atari(other, &fpos, &defense_point, 
			    NO_MOVE, 0, aa_val);
  }

  popgo();
  decrease_depth_values();
  /* We know that a combination exists, but we don't know if
   * the original move at (ai, aj) was really relevant. So we
   * try omitting it and see if a combination is still found.
   */
  if (do_atari_atari(other, NULL, NULL, NO_MOVE, 0, minsize) >= after_aa_val)
    return 1;
  else {
    if (move) *move = after_defense_point;
    return 0;
  }
}


/* Ask the atari_atari code if after color plays at (apos)
 * and other plays at (bpos) there appears any combination
 * attack. Returns the size of the combination.
 *
 * FIXME: Most of the code below is common with atari_atari() and
 *        should be broken out of both functions.
 */

int
atari_atari_try_combination(int color, int apos, int bpos)
{
  int other = OTHER_COLOR(color);
  int aa_val = 0;
  int m, n;
  int save_verbose = verbose;

  if (aa_depth < 2)
    return 0;
  if (verbose > 0)
    verbose--;
  memset(forbidden, 0, sizeof(forbidden));
      
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      if (BOARD(m, n) == other) {
	if (dragon[m][n].matcher_status == DEAD)
	  aa_status[POS(m, n)] = DEAD;
	else if (worm[m][n].attack_code != 0) {
	  if (worm[m][n].defend_code != 0)
	    aa_status[POS(m, n)] = CRITICAL;
	  else
	    aa_status[POS(m, n)] = DEAD;

	}
	else
	  aa_status[POS(m, n)] = ALIVE;
      }
      else
	aa_status[POS(m, n)] = UNKNOWN;
    }

  /* reclassify a worm with 2 liberties as INSUBSTANTIAL if capturing
   * it does not result in a live group.
   */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) 
      if (BOARD(m, n) == other
	  && worm[m][n].origin == POS(m, n)
	  && worm[m][n].liberties == 2
	  && aa_status[POS(m, n)] == ALIVE
	  && !owl_substantial(m, n)) {
	int ti, tj;
	for (ti = 0; ti < board_size; ti++)
	  for (tj = 0; tj < board_size; tj++)
	    if (is_worm_origin(ti, tj, m, n))
	      aa_status[POS(ti, tj)] = INSUBSTANTIAL;
      }

  if (trymove(apos, color, NULL, NO_MOVE, EMPTY, NO_MOVE)) {
    if (trymove(bpos, other, NULL, NO_MOVE, EMPTY, NO_MOVE)) {
      aa_val = do_atari_atari(color, NULL, NULL, apos, 0, 0);
      popgo();
    }
    popgo();
  }
  verbose = save_verbose;
  return aa_val;
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
  Read_result *read_result;
  
  SETUP_TRACE_INFO("find_defense", str);
  
  RTRACE("Can we rescue %1m?\n", str);

  /* We first check if the number of liberties is larger than four. In
   * that case we don't cache the result and to avoid needlessly
   * storing the position in the hash table, we must do this test
   * before we look for cached results.
   */
  liberties = countlib(str);
  
  if (liberties > 4
      || (liberties == 4 && stackp > depth)
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

    /* This data should always be recorded. */
    if (read_result) {
      rr_set_compressed_data(*read_result, FIND_DEFENSE, 
			     komaster, kom_pos, str, stackp);
    }
  }
  else
    read_result = NULL;

  if (liberties == 1)
    dcode = defend1(str, &xpos, komaster, kom_pos);
  else if (liberties == 2)
    dcode = defend2(str, &xpos, komaster, kom_pos);
  else if (liberties == 3)
    dcode = defend3(str, &xpos, komaster, kom_pos);
  else if (liberties == 4)
    dcode = defend4(str, &xpos, komaster, kom_pos);

  if (dcode) {
    RTRACE("saving move for %1m found at %1m!\n", str, xpos);
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
  int moves[MAX_MOVES];
  int scores[MAX_MOVES];
  int num_moves = 0;
  int savemove = 0;
  int savecode = 0;
  int liberties;
  int k;
  int found_read_result;
  Read_result *read_result;

  SETUP_TRACE_INFO("defend1", str);
  reading_node_counter++;
  
  gg_assert(board[str] != EMPTY);
  ASSERT1(countlib(str) == 1, str);
  RTRACE("try to escape atari on %1m.\n", str);

  if ((stackp <= depth) && (hashflags & HASH_DEFEND1)) {
  
    found_read_result = get_read_result(DEFEND1, komaster, kom_pos,
					&str, &read_result);
    if (found_read_result) {
      TRACE_CACHED_RESULT(*read_result);
      if (rr_get_result(*read_result) != 0)
	*move = rr_get_move(*read_result);

      SGFTRACE(rr_get_move(*read_result),
	       rr_get_result(*read_result), "cached");
      return rr_get_result(*read_result);
    }

    /* This data should always be recorded. */
    if (read_result) {
      rr_set_compressed_data(*read_result, DEFEND1, komaster, kom_pos,
			     str, stackp);
    }
  }
  else
    read_result = NULL;

  /* lib will be the liberty of the string. */
  liberties = findlib(str, 1, &lib);
  ASSERT1(liberties == 1, str);

  /* Collect moves to try in the first batch.
   * 1. First order liberty.
   * 2. Chain breaking moves.
   */
  moves[0] = lib;
  scores[0] = 0;
  num_moves = 1;
  
  break_chain_moves(str, moves, scores, &num_moves);

  order_moves(str, num_moves, moves, scores, color, read_function_name);

  for (k = 0; k < num_moves; k++) {
    int new_komaster;
    int new_kom_pos;
    int ko_move;

    xpos = moves[k];
    if (komaster_trymove(xpos, color, "defend1-A", str, komaster, kom_pos,
			 &new_komaster, &new_kom_pos,
			 &ko_move, stackp <= ko_depth && savecode == 0)) {
      if (!ko_move) {
	int acode = do_attack(str, NULL, new_komaster, new_kom_pos);
	popgo();
	if (acode == 0) {
	  SGFTRACE(xpos, WIN, "defense effective - A");
	  READ_RETURN(read_result, move, xpos, WIN);
	}
	/* if the move works with ko we save it, then look for something
	 * better.
	 */
	UPDATE_SAVED_KO_RESULT(savecode, savemove, acode, xpos);
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
	    && trymove(apos, color, "attack1-C", str, komaster, kom_pos)) {
	  int acode = do_attack(str, NULL, komaster, kom_pos);
	  if (acode == 0) {
	    popgo();
	    SGFTRACE(apos, WIN, "backfilling");
	    READ_RETURN(read_result, move, apos, WIN);
	  }
	  UPDATE_SAVED_KO_RESULT(savecode, savemove, acode, apos);
	  popgo();
	}
      }
    }
  }
  
  if (savecode != 0) {
    SGFTRACE(savemove, savecode, "saved move");
    READ_RETURN(read_result, move, savemove, savecode);
  }
  
  SGFTRACE(0, 0, NULL);
  READ_RETURN0(read_result);
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
  int moves[MAX_MOVES];
  int scores[MAX_MOVES];
  int num_moves = 0;
  int savemove = 0;
  int savecode = 0;
  int bc;
  int k;
  int r;
  int s;
  int found_read_result;
  Read_result *read_result;

  SETUP_TRACE_INFO("defend2", str);
  reading_node_counter++;
  
  RTRACE("trying to rescue %1m\n", str);
  color = board[str];
  other = OTHER_COLOR(color);

  gg_assert(board[str] != EMPTY);
  gg_assert(countlib(str) == 2);

  if ((stackp <= depth) && (hashflags & HASH_DEFEND2)) {
  
    found_read_result = get_read_result(DEFEND2, komaster, kom_pos,
					&str, &read_result);
    if (found_read_result) {
      TRACE_CACHED_RESULT(*read_result);
      if (rr_get_result(*read_result) != 0)
	*move = rr_get_move(*read_result);

      SGFTRACE(rr_get_move(*read_result),
	       rr_get_result(*read_result), "cached");
      return rr_get_result(*read_result);
    }

    /* This data should always be recorded. */
    if (read_result) {
      rr_set_compressed_data(*read_result, DEFEND2, komaster, kom_pos,
			     str, stackp);
    }
  }
  else
    read_result = NULL;

  liberties = findlib(str, 2, libs);
  ASSERT1(liberties == 2, str);

  /* Collect moves to try in the first batch.
   * 1. First order liberties.
   * 2. Chain breaking moves.
   * 3. Second order liberties moving up from first line to second.
   * 4. Edge clamps.
   */
  for (k = 0; k < liberties; k++) {
    moves[k] = libs[k];
    scores[k] = 0;
  }
  num_moves = liberties;
  
  break_chain_moves(str, moves, scores, &num_moves);
  break_chain2_efficient_moves(str, moves, scores, &num_moves);
  propose_edge_moves(str, libs, liberties, moves, scores, &num_moves, color);
  edge_clamp(str, moves, scores, &num_moves);

  order_moves(str, num_moves, moves, scores, color, read_function_name);

  for (k = 0; k < num_moves; k++) {
    int new_komaster;
    int new_kom_pos;
    int ko_move;

    xpos = moves[k];
    
    if (komaster_trymove(xpos, color, "defend2-A", str,
			 komaster, kom_pos, &new_komaster, &new_kom_pos,
			 &ko_move, stackp <= ko_depth && savecode == 0)) {
      if (!ko_move) {
	int acode = do_attack(str, NULL, new_komaster, new_kom_pos);
	popgo();
	if (acode == 0) {
	  SGFTRACE(xpos, WIN, "defense effective - A");
	  READ_RETURN(read_result, move, xpos, WIN);
	}
	/* if the move works with ko we save it, then look for something
	 * better.
	 */
	UPDATE_SAVED_KO_RESULT(savecode, savemove, acode, xpos);
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

  /* Look for backfilling moves. */
  for (k = 0; k < liberties; k++) {
    if (is_self_atari(libs[k], other)) {
      liberties2 = approxlib(libs[k], color, 6, libs2);
      for (r = 0; r < liberties2; r++) {
	xpos = libs2[r];
	/* Don't reconsider previously tested moves. */
	for (s = 0; s < num_moves; s++)
	  if (xpos == moves[s])
	    break;
	if (s < num_moves)
	  continue;

	if (trymove(xpos, color, "defend2-C", str, komaster, kom_pos)) {
	  int acode;
	  /* If the newly placed stone is in atari, we give up without
           * fight.
	   */
	  if (countlib(xpos) == 1 && countstones(xpos) > 1)
	    acode = WIN;
	  else
	    acode = do_attack(str, NULL, komaster, kom_pos);

	  popgo();
	  if (acode == 0) {
	    SGFTRACE(xpos, WIN, "backfill effective");
	    READ_RETURN(read_result, move, xpos, WIN);
	  }
	  UPDATE_SAVED_KO_RESULT(savecode, savemove, acode, xpos);
	}
      }
    }
    liberties2 = approxlib(libs[k], other, 3, libs2);
    if (liberties2 <= 2) {
      for (r = 0; r < liberties2; r++) {
	xpos = libs2[r];
	/* Don't reconsider previously tested moves. */
	for (s = 0; s < num_moves; s++)
	  if (xpos == moves[s])
	    break;
	if (s < num_moves)
	  continue;
	
	if (!is_self_atari(xpos, color)
	    && trymove(xpos, color, "defend2-D", str, komaster, kom_pos)) {
	  int acode = do_attack(str, NULL, komaster, kom_pos);
	  popgo();
	  if (acode == 0) {
	    SGFTRACE(xpos, WIN, "backfill effective");
	    READ_RETURN(read_result, move, xpos, WIN);
	  }
	  UPDATE_SAVED_KO_RESULT(savecode, savemove, acode, xpos);
	}
      }
    }
  }

  if (stackp <= depth) {
    for (k = 0; k < liberties; k++) {
      int dcode = special_rescue(str, libs[k], &xpos, komaster, kom_pos);
      if (dcode == WIN) {
	SGFTRACE(xpos, WIN, "special rescue");
	READ_RETURN(read_result, move, xpos, WIN);
      }
      UPDATE_SAVED_KO_RESULT_UNREVERSED(savecode, savemove, dcode, xpos);
    }
  }
  
  if (stackp <= backfill_depth) {
    int dcode = special_rescue2(str, libs, &xpos, komaster, kom_pos);
    if (dcode == WIN) {
      SGFTRACE(xpos, WIN, "special rescue2");
      READ_RETURN(read_result, move, xpos, WIN);
    }
    UPDATE_SAVED_KO_RESULT_UNREVERSED(savecode, savemove, dcode, xpos);
  }
  
  if (level >= 10 && stackp <= superstring_depth) {
    int dcode = superstring_breakchain(str, &xpos, komaster, kom_pos, 4);
    if (dcode == WIN) {
      SGFTRACE(xpos, WIN, "superstring_breakchain");
      READ_RETURN(read_result, move, xpos, WIN);
    }
    UPDATE_SAVED_KO_RESULT_UNREVERSED(savecode, savemove, dcode, xpos);
  }

  /* If nothing else works, we try playing a liberty of the
   * super_string.
   */
  if (level >= 10 && stackp <= superstring_depth) {
    int liberties;
    int libs[MAX_LIBERTIES + 4];

    find_superstring_liberties(str, &liberties, libs, 3);
    for (k = 0; k < liberties; k++) {
      int apos = libs[k];
	
      if (liberty_of_string(apos, str))
	continue;
      if (trymove(apos, color, "defend2-E", str, komaster, kom_pos)) {
	int acode;
	/* If the newly placed stone is in atari, we give up without fight. */
	if (countlib(apos) == 1)
	  acode = WIN;
	else
	  acode = do_attack(str, NULL, komaster, kom_pos);
	popgo();
	if (acode == 0) {
	  SGFTRACE(apos, WIN, "superstring liberty");
	  READ_RETURN(read_result, move, apos, WIN);
	}
	UPDATE_SAVED_KO_RESULT(savecode, savemove, acode, apos);
      }
    }
    
    /* Now we are truly desperate. Try playing second order liberties of
     * the superstring.
     */
    for (k = 0; k < liberties; k++) {
      int apos = libs[k];
      int dcode;
	
      if (liberty_of_string(apos, str))
	continue;
      
      dcode = special_rescue(str, apos, &xpos, komaster, kom_pos);
      if (dcode == WIN) {
	SGFTRACE(xpos, WIN, "special rescue");
	READ_RETURN(read_result, move, xpos, WIN);
      }
      UPDATE_SAVED_KO_RESULT_UNREVERSED(savecode, savemove, dcode, xpos);
    }
  }

  /* We place the more speculative moves trying to break chain links
   * with 2 or 3 liberties last, because these moves often are not
   * really relevant.
   */
  
  bc = break_chain2(str, &xpos, komaster, kom_pos);
  if (bc == WIN) {
    SGFTRACE(xpos, bc, "break chain2");
    READ_RETURN(read_result, move, xpos, bc);
  }
  UPDATE_SAVED_KO_RESULT_UNREVERSED(savecode, savemove, bc, xpos);
  
  if (stackp <= backfill2_depth) {
    bc = break_chain3(str, &xpos, komaster, kom_pos);
    if (bc == WIN) {
      SGFTRACE(xpos, bc, "break chain3");
      READ_RETURN(read_result, move, xpos, bc);
    }
    UPDATE_SAVED_KO_RESULT_UNREVERSED(savecode, savemove, bc, xpos);
  }
  
  if (savecode != 0) {
    SGFTRACE(savemove, savecode, "saved move");
    READ_RETURN(read_result, move, savemove, savecode);
  }

  RTRACE("failed to find rescuing move.\n");
  SGFTRACE(0, 0, NULL);
  READ_RETURN0(read_result);
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
#if 0
  int liberties2;
  int libs2[6];
#endif
  int moves[MAX_MOVES];
  int scores[MAX_MOVES];
  int num_moves = 0;
  int savemove = 0;
  int savecode = 0;
  int bc;
  int k;
#if 0
  int r;
  int s;
#endif
  int found_read_result;
  Read_result *read_result;

  SETUP_TRACE_INFO("defend3", str);
  reading_node_counter++;

  RTRACE("trying to rescue %1m\n", str);
  color = board[str];
  other = OTHER_COLOR(color);

  gg_assert(board[str] != EMPTY);
  gg_assert(countlib(str) == 3);

  if ((stackp <= depth) && (hashflags & HASH_DEFEND3)) {
    found_read_result = get_read_result(DEFEND3, komaster, kom_pos,
					&str, &read_result);
    if (found_read_result) {
      TRACE_CACHED_RESULT(*read_result);
      if (rr_get_result(*read_result) != 0)
	*move = rr_get_move(*read_result);

      SGFTRACE(rr_get_move(*read_result),
	       rr_get_result(*read_result), "cached");
      return rr_get_result(*read_result);
    }

    /* This data should always be recorded. */
    if (read_result) {
      rr_set_compressed_data(*read_result, DEFEND3, komaster, kom_pos, 
			     str, stackp);
    }
  }
  else
    read_result = NULL;

  liberties = findlib(str, 3, libs);
  ASSERT1(liberties == 3, str);

  /* Collect moves to try in the first batch.
   * 1. First order liberties.
   * 2. Chain breaking moves.
   * 3. Second order liberties moving up from first line to second.
   * 4. Edge clamps.
   */
  for (k = 0; k < liberties; k++) {
    moves[k] = libs[k];
    scores[k] = 0;
  }
  num_moves = liberties;
  
  break_chain_moves(str, moves, scores, &num_moves);
  break_chain2_efficient_moves(str, moves, scores, &num_moves);
  propose_edge_moves(str, libs, liberties, moves, scores, &num_moves, color);
  edge_clamp(str, moves, scores, &num_moves);

  order_moves(str, num_moves, moves, scores, color, read_function_name);

  for (k = 0; k < num_moves; k++) {
    int new_komaster;
    int new_kom_pos;
    int ko_move;

    if (stackp >= branch_depth && k > 0)
      break;
    
    xpos = moves[k];
    
    if (komaster_trymove(xpos, color, "defend3-A", str, komaster, kom_pos,
			 &new_komaster, &new_kom_pos,
			 &ko_move, stackp <= ko_depth && savecode == 0)) {
      if (!ko_move) {
	int acode = do_attack(str, NULL, new_komaster, new_kom_pos);
	popgo();
	if (acode == 0) {
	  SGFTRACE(xpos, WIN, "defense effective - A");
	  READ_RETURN(read_result, move, xpos, WIN);
	}
	/* if the move works with ko we save it, then look for something
	 * better.
	 */
	UPDATE_SAVED_KO_RESULT(savecode, savemove, acode, xpos);
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

#if 0
  if (stackp <= backfill_depth) {
    bc = break_chain2(str, &xpos, komaster);
    if (bc == WIN) {
      SGFTRACE(xpos, bc, "break chain2");
      READ_RETURN(read_result, move, xpos, bc);
    }
    UPDATE_SAVED_KO_RESULT_UNREVERSED(savecode, savemove, bc, xpos);
  }
  
  if (stackp <= backfill2_depth) {
    bc = break_chain3(str, &xpos, komaster);
    if (bc == WIN) {
      SGFTRACE(xpos, bc, "break chain3");
      READ_RETURN(read_result, move, xpos, bc);
    }
    UPDATE_SAVED_KO_RESULT_UNREVERSED(savecode, savemove, bc, xpos);
  }
#endif
  
  /* This looks a little too expensive. */
#if 0
  /* Look for backfilling moves. */
  if (stackp <= backfill_depth) {
    for (k = 0; k < liberties; k++) {
      if (is_self_atari(libs[k], other)) {
	liberties2 = approxlib(libs[k], color, 6, libs2);
	for (r = 0; r < liberties2; r++) {
	  xpos = libs2[r];
	  /* Don't reconsider previously tested moves. */
	  for (s = 0; s < num_moves; s++)
	    if (xpos == moves[s])
	      break;
	  if (s < num_moves)
	    continue;
	  
	  if (trymove(xpos, color, "defend3-D", str, komaster, kom_pos)) {
	    int acode;
	    /* If the newly placed stone is in atari, we give up
             * without fight.
	     */
	    if (countlib(xpos) == 1)
	      acode = WIN;
	    else
	      acode = do_attack(str, NULL, komaster, kom_pos);

	    popgo();
	    if (acode == 0) {
	      SGFTRACE(xpos, WIN, "backfill effective");
	      READ_RETURN(read_result, move, xpos, WIN);
	    }
	    UPDATE_SAVED_KO_RESULT(savecode, savemove, acode, xpos);
	  }
	}
      }
      else {
	liberties2 = approxlib(libs[k], other, 3, libs2);
	if (liberties2 <= 3) {
	  for (r = 0; r < liberties2; r++) {
	    xpos = libs2[r];
	    /* Don't reconsider previously tested moves. */
	    for (s = 0; s < num_moves; s++)
	      if (xpos == moves[s])
		break;
	    if (s < num_moves)
	      continue;
	    
	    if (!is_self_atari(xpos, color)
		&& trymove(xpos, color, "defend2-D", str, komaster, kom_pos)) {
	      int acode = do_attack(str, NULL, komaster, kom_pos);
	      popgo();
	      if (acode == 0) {
		SGFTRACE(xpos, WIN, "backfill effective");
		READ_RETURN(read_result, move, xpos, WIN);
	      }
	      UPDATE_SAVED_KO_RESULT(savecode, savemove, acode, xpos);
	    }
	  }
	}
      }
    }
  }
#endif
  
  /* If nothing else works, try to defend with second order liberties. */
  if (stackp <= depth) {
    for (k = 0; k < liberties; k++) {
      int dcode = special_rescue(str, libs[k], &xpos, komaster, kom_pos);
      if (dcode == WIN) {
	SGFTRACE(xpos, WIN, "special rescue");
	READ_RETURN(read_result, move, xpos, WIN);
      }
      UPDATE_SAVED_KO_RESULT_UNREVERSED(savecode, savemove, dcode, xpos);
    }
  }

  if (stackp <= backfill_depth) {
    int dcode = special_rescue3(str, libs, &xpos, komaster, kom_pos);
    if (dcode == WIN) {
      SGFTRACE(xpos, WIN, "special rescue2");
      READ_RETURN(read_result, move, xpos, WIN);
    }
    UPDATE_SAVED_KO_RESULT_UNREVERSED(savecode, savemove, dcode, xpos);
  }
    
  if (stackp <= backfill_depth) {
    int dcode = special_rescue4(str, libs, &xpos, komaster, kom_pos);
    if (dcode == WIN) {
      SGFTRACE(xpos, WIN, "special rescue4");
      READ_RETURN(read_result, move, xpos, WIN);
    }
    UPDATE_SAVED_KO_RESULT_UNREVERSED(savecode, savemove, dcode, xpos);
  }
    
  if (level >= 10 && stackp <= backfill2_depth) {
    int dcode = superstring_breakchain(str, &xpos, komaster, kom_pos, 4);
    if (dcode == WIN) {
      SGFTRACE(xpos, WIN, "superstring_breakchain");
      READ_RETURN(read_result, move, xpos, WIN);
    }
    UPDATE_SAVED_KO_RESULT_UNREVERSED(savecode, savemove, dcode, xpos);
  }

  /* If nothing else works, we try playing a liberty of the
   * super_string.
   */
  if (level >= 10 && stackp <= backfill2_depth) {
    int liberties;
    int libs[MAX_LIBERTIES + 4];

    find_superstring_liberties(str, &liberties, libs, 3);
    for (k = 0; k < liberties; k++) {
      int apos = libs[k];
	
      if (liberty_of_string(apos, str))
	continue;
      if (trymove(apos, color, "defend3-C", str, komaster, kom_pos)) {
	int acode;
	/* If the newly placed stone is in atari, we give up without fight. */
	if (countlib(apos) == 1)
	  acode = WIN;
	else
	  acode = do_attack(str, NULL, komaster, kom_pos);

	popgo();
	if (acode == 0) {
	  SGFTRACE(apos, WIN, "superstring liberty");
	  READ_RETURN(read_result, move, apos, WIN);
	}
	UPDATE_SAVED_KO_RESULT(savecode, savemove, acode, apos);
      }
    }

    /* Now we are truly desperate. Try playing second order liberties of
     * the superstring.
     */
    for (k = 0; k < liberties; k++) {
      int apos = libs[k];
      int dcode;
	
      if (liberty_of_string(apos, str))
	continue;
      
      dcode = special_rescue(str, apos, &xpos, komaster, kom_pos);
      if (dcode == WIN) {
	SGFTRACE(xpos, WIN, "special rescue");
	READ_RETURN(read_result, move, xpos, WIN);
      }
      UPDATE_SAVED_KO_RESULT_UNREVERSED(savecode, savemove, dcode, xpos);
    }
  }

  /* We place the more speculative moves trying to break chain links
   * with 2 or 3 liberties last, because these moves often are not
   * really relevant.
   */
#if 1
  if (stackp <= backfill2_depth) {
    bc = break_chain2(str, &xpos, komaster, kom_pos);
    if (bc == WIN) {
      SGFTRACE(xpos, bc, "break chain2");
      READ_RETURN(read_result, move, xpos, bc);
    }
    UPDATE_SAVED_KO_RESULT_UNREVERSED(savecode, savemove, bc, xpos);
  }
  
  if (stackp <= backfill2_depth) {
    bc = break_chain3(str, &xpos, komaster, kom_pos);
    if (bc == WIN) {
      SGFTRACE(xpos, bc, "break chain3");
      READ_RETURN(read_result, move, xpos, bc);
    }
    UPDATE_SAVED_KO_RESULT_UNREVERSED(savecode, savemove, bc, xpos);
  }
#endif
  
  if (savecode != 0) {
    SGFTRACE(savemove, savecode, "saved move");
    READ_RETURN(read_result, move, savemove, savecode);
  }

  RTRACE("failed to find rescuing move.\n");
  SGFTRACE(0, 0, NULL);
  READ_RETURN0(read_result);
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
  int moves[MAX_MOVES];
  int scores[MAX_MOVES];
  int num_moves = 0;
  int savemove = 0;
  int savecode = 0;
  int k;
  int found_read_result;
  Read_result *read_result;
  
  SETUP_TRACE_INFO("defend4", str);
  reading_node_counter++;

  RTRACE("trying to rescue %1m\n", str);
  color = board[str];
  other = OTHER_COLOR(color);

  gg_assert(board[str] != EMPTY);
  gg_assert(countlib(str) == 4);

  if ((stackp <= depth) && (hashflags & HASH_DEFEND4)) {
    found_read_result = get_read_result(DEFEND4, komaster, kom_pos,
					&str, &read_result);
    if (found_read_result) {
      TRACE_CACHED_RESULT(*read_result);
      if (rr_get_result(*read_result) != 0)
	*move = rr_get_move(*read_result);

      SGFTRACE(rr_get_move(*read_result),
	       rr_get_result(*read_result), "cached");
      return rr_get_result(*read_result);
    }

    /* This data should always be recorded. */
    if (read_result) {
      rr_set_compressed_data(*read_result, DEFEND4, komaster, kom_pos, 
			     str, stackp);
    }
  }
  else
    read_result = NULL;

  liberties = findlib(str, 4, libs);
  ASSERT1(liberties == 4, str);

  /* Collect moves to try in the first batch.
   * 1. First order liberties.
   * 2. Chain breaking moves.
   */
  for (k = 0; k < liberties; k++) {
    moves[k] = libs[k];
    scores[k] = 0;
  }
  num_moves = liberties;
  
  break_chain_moves(str, moves, scores, &num_moves);
  break_chain2_efficient_moves(str, moves, scores, &num_moves);

  order_moves(str, num_moves, moves, scores, color, read_function_name);

  for (k = 0; k < num_moves; k++) {
    int new_komaster;
    int new_kom_pos;
    int ko_move;

    if (stackp >= branch_depth && k > 0)
      break;
    
    xpos = moves[k];
    
    if (komaster_trymove(xpos, color, "defend4-A", str, komaster, kom_pos,
			 &new_komaster, &new_kom_pos,
			 &ko_move, stackp <= ko_depth && savecode == 0)) {
      if (!ko_move) {
	int acode = do_attack(str, NULL, new_komaster, new_kom_pos);
	popgo();
	if (acode == 0) {
	  SGFTRACE(xpos, WIN, "defense effective - A");
	  READ_RETURN(read_result, move, xpos, WIN);
	}
	/* if the move works with ko we save it, then look for something
	 * better.
	 */
	UPDATE_SAVED_KO_RESULT(savecode, savemove, acode, xpos);
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

  if (stackp <= backfill_depth) {
    int bc = break_chain2(str, &xpos, komaster, kom_pos);
    if (bc == WIN) {
      SGFTRACE(xpos, WIN, "break chain2");
      READ_RETURN(read_result, move, xpos, WIN);
    }
    UPDATE_SAVED_KO_RESULT_UNREVERSED(savecode, savemove, bc, xpos);
  }

  if (savecode != 0) {
    SGFTRACE(savemove, savecode, "saved move");
    READ_RETURN(read_result, move, savemove, savecode);
  }

  RTRACE("failed to find rescuing move.\n");
  SGFTRACE(0, 0, NULL);
  READ_RETURN0(read_result);
}


/*
 * special_rescue(str, lib, *move) is called with (str) a
 * string having a liberty at (lib). The saving move is returned
 * in (*move).
 *
 * This checks whether a move on a second order liberty is a rescuing
 * move, e.g. in shapes like:
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

static int
special_rescue(int str, int lib, int *move, int komaster, int kom_pos)
{
  int color = board[str];
  int other = OTHER_COLOR(color);
  int k;
  int savemove = 0;
  int savecode = 0;

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
      
      if (trymove(lib + d, color, "special_rescue", str, komaster, kom_pos)) {
	int acode = do_attack(str, NULL, komaster, kom_pos);
	if (acode == 0) {
	  popgo();
	  *move = lib + d;
	  return WIN;
	}
	UPDATE_SAVED_KO_RESULT(savecode, savemove, acode, lib + d);
	popgo();
      }
    }
  }

  if (savecode != 0)
    *move = savemove;

  return savecode;
}


/* In a situation like this:
 *       
 *   OOXXXX     the following code can find the
 *   .OXOOX     defensive move at 'c'.
 *   .cO.OX
 *   .X.OOX
 *   ------
 */
static int
special_rescue2(int str, int libs[2], int *move, int komaster, int kom_pos)
{
  int color = board[str];
  int other = OTHER_COLOR(color);
  int newlibs[3];
  int xpos;
  int savemove = 0;
  int savecode = 0;
  int k;

  for (k = 0; k < 2; k++) {
    /* Let (ai, aj) and (bi, bj) be the two liberties. Reverse the
     * order during the second pass through the loop.
     */
    int alib = libs[k];
    int blib = libs[1-k];
    if (is_suicide(alib, other) 
	&& (approxlib(alib, color, 3, newlibs) == 2)) {
      if (newlibs[0] != blib)
	xpos = newlibs[0];
      else
	xpos = newlibs[1];

      if (!is_self_atari(xpos, color)
	  && trymove(xpos, color, "special_rescue2", str, komaster, kom_pos)) {
	int acode = do_attack(str, NULL, komaster, kom_pos);
	if (acode != WIN) {
	  if (acode == 0) {
	    popgo();
	    *move = xpos;
	    return WIN;
	  }
	  UPDATE_SAVED_KO_RESULT(savecode, savemove, acode, xpos);
	}
	popgo();
      }
    }
  }

  if (savecode != 0)
    *move = savemove;

  return savecode;
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
static int
special_rescue3(int str, int libs[3], int *move, int komaster, int kom_pos)
{
  int color = board[str];
  int other = OTHER_COLOR(color);
  int apos, bpos, cpos, dpos, epos, fpos, gpos;
  int savemove = 0;
  int savecode = 0;
  int k, l, r;

  ASSERT1(countlib(str) == 3, str);
  
  for (r = 0; r < 3; r++) {
    /* Let (ai, aj) be one of the three liberties. */
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
	if (trymove(fpos, color, "special_rescue3", str, komaster, kom_pos)) {
	  int acode = do_attack(str, NULL, komaster, kom_pos);
	  if (acode != WIN) {
	    if (acode == 0) {
	      popgo();
	      *move = fpos;
	      return WIN;
	    }
	    UPDATE_SAVED_KO_RESULT(savecode, savemove, acode, fpos);
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


/* In a situation like this:
 *
 *   .XXXXX
 *   XX.*OO
 *   X.OX..     the following code can find the
 *   ......     defensive move at '*'.
 *   ------
 *
 *   .*O   acd
 *   OX.   be.
 */
static int
special_rescue4(int str, int libs[3], int *move, int komaster, int kom_pos)
{
  int color = board[str];
  int other = OTHER_COLOR(color);
  int apos, bpos, cpos, dpos, epos;
  int savemove = 0;
  int savecode = 0;
  int k, l, r;

  ASSERT1(countlib(str) == 3, str);
  
  for (r = 0; r < 3; r++) {
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

	dpos = cpos + normal;
	if (board[dpos] != color)
	  continue;

	epos = bpos + normal;
	if (board[epos] != other)
	  continue;

	/* Configuration found. Now require that (dpos) has at least 3
         * liberties and (epos) at most 3 liberties.
	 */

	if (countlib(dpos) < 3)
	  continue;
	
	if (countlib(epos) > 3)
	  continue;
	
	/* Try to play at (cpos). */
	if (trymove(cpos, color, "special_rescue4", str, komaster, kom_pos)) {
	  int acode = do_attack(str, NULL, komaster, kom_pos);
	  if (acode != WIN) {
	    if (acode == 0) {
	      popgo();
	      *move = cpos;
	      return WIN;
	    }
	    UPDATE_SAVED_KO_RESULT(savecode, savemove, acode, cpos);
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
edge_clamp(int str, int moves[MAX_MOVES], int scores[MAX_MOVES],
	   int *num_moves)
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
    bpos = 0;
    findlib(apos, 3, libs);
    for (k = 0; k < 3; k++) {
      int i = I(libs[k]);
      int j = J(libs[k]);
      if (i == 0 || i == board_size-1 || j == 0 || j == board_size-1) {
	bpos = libs[k];
	break;
      }
    }
    if (bpos == 0)
      continue;

    /* Edge liberty found. Establish up and right directions. */
    for (k = 0; k < 4; k++) {
      int up = delta[k];
      if (ON_BOARD(bpos - up))
	continue;
      if (board[bpos + up] != other)
	continue;
       
      for (l = 0; l < 2; l++) {
	int right = delta[(k+4)%4];
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
	ADD_CANDIDATE_MOVE(dpos, 10, moves, scores, *num_moves);
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
propose_edge_moves(int str, int *libs, int liberties, int moves[MAX_MOVES], 
		   int scores[MAX_MOVES], int *num_moves, int to_move)
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
	  int  xpos = apos;
	
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
	    REMOVE_CANDIDATE_MOVE(apos, moves, scores, *num_moves);
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
	    ADD_CANDIDATE_MOVE(apos + up, 10, moves, scores, *num_moves);
	  else {
	    ADD_CANDIDATE_MOVE(apos + up, 0, moves, scores, *num_moves);
	    
	    /* Add c if empty */
	    if (board[apos + right + up] == EMPTY
		&& (liberties != 2 || color != to_move))
	      ADD_CANDIDATE_MOVE(apos + right + up, 0,
				 moves, scores, *num_moves);
	  }
	}
      }
    }
  }
}


/* ================================================================ */  
/*                       Attacking functions                        */
/* ================================================================ */


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
  Read_result *read_result;

  SETUP_TRACE_INFO("attack", str);

  ASSERT1(color != 0, str);

  if (color == 0)      /* if assertions are turned off, silently fails */
    return 0;

  libs = countlib(str);

  if (libs > 4
      || (libs == 4 && stackp > fourlib_depth)) {
    /* No need to cache the result in these cases. */
    SGFTRACE(0, 0, "too many liberties");
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

    /* This data should always be recorded. */
    if (read_result) {
      rr_set_compressed_data(*read_result, ATTACK, komaster, kom_pos, 
			     str, stackp);
    }
  }
  else
    read_result = NULL;

  /* Treat the attack differently depending on how many liberties the 
     string at (str) has. */
  if (libs == 1)
    result = attack1(str, &xpos, komaster, kom_pos);
  else if (libs == 2)
    result = attack2(str, &xpos, komaster, kom_pos);
  else if (libs == 3)
    result = attack3(str, &xpos, komaster, kom_pos);
  else if (libs == 4)
    result = attack4(str, &xpos, komaster, kom_pos);

  ASSERT1(result >= 0 && result <= 3, str);
  
  if (result)
    READ_RETURN(read_result, move, xpos, result);

  READ_RETURN0(read_result);
}


/* If (str) points to a group with exactly one liberty, attack1
 * determines whether it can be captured by playing at this liberty.
 * If yes, (*move) is the killing move. move may be NULL if caller is
 * only interested in whether it can be captured.
 *
 * The attack may fail for two different reasons. The first one is
 * that the attack may be an illegal ko capture, in this case 3 is
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
  int apos;
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
  int moves[MAX_MOVES];
  int scores[MAX_MOVES];
  int num_moves = 0;
  int adjacent_liberties = 0;
  int found_read_result;
  Read_result *read_result;

  SETUP_TRACE_INFO("attack2", str);
  reading_node_counter++;

  str = find_origin(str);
  ASSERT1(board[str] != EMPTY, str);
  ASSERT1(countlib(str) == 2, str);

  RTRACE("checking attack on %1m with 2 liberties\n", str);

  if ((stackp <= depth) && (hashflags & HASH_ATTACK2)) {
  
    found_read_result = get_read_result(ATTACK2, komaster, kom_pos,
					&str, &read_result);
    if (found_read_result) {
      TRACE_CACHED_RESULT(*read_result);
      if (rr_get_result(*read_result) != 0)
	*move = rr_get_move(*read_result);

      SGFTRACE(rr_get_move(*read_result),
	       rr_get_result(*read_result), "cached");
      return rr_get_result(*read_result);
    }

    /* This data should always be recorded. */
    if (read_result) {
      rr_set_compressed_data(*read_result, ATTACK2, komaster, kom_pos, 
			     str, stackp);
    }
  }
  else
    read_result = NULL;

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
      SGFTRACE(0, 0, "boundary in atari");
      READ_RETURN0(read_result);
    }

    /* Pick up moves breaking the second order chain. */
    if (stackp <= depth)
      break_chain_moves(adjs[r], moves, scores, &num_moves);
    
    findlib(adjs[r], 1, &hpos);
    ADD_CANDIDATE_MOVE(hpos, 0, moves, scores, num_moves);
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
    apos = libs[k];
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
      ADD_CANDIDATE_MOVE(apos, 0, moves, scores, num_moves);

    /* Try backfilling if atari is impossible. */
    if (stackp <= backfill_depth && approxlib(apos, other, 2, libs2) == 1) {
      ADD_CANDIDATE_MOVE(libs2[0], 0, moves, scores, num_moves);
      /* If there is a neighbor in atari, we also try back-capturing. */
      for (r = 0; r < 4; r++) {
	int bpos = libs2[0] + delta[r];
	if (board[bpos] == other && chainlinks2(bpos, adjs, 1) > 0) {
	  /* FIXME: If there is more than one neighbor in atari, we
           * currently just take one randomly. This is maybe not good
           * enough. We might also want to check against snapback.
	   */
	  findlib(adjs[0], 1, &xpos);
	  ADD_CANDIDATE_MOVE(xpos, 0, moves, scores, num_moves);
	}
      }
    }
  }

  /* If we can't make a direct atari, look for edge blocking moves. */
  if (!atari_possible)
    for (k = 0; k < 2; k++) 
      edge_block(str, libs[k], moves, scores, &num_moves);
    

  /* If one of the surrounding chains have only two liberties, which
   * coincide with the liberties of the attacked string, we try to
   * backcapture.
   */
  
  adj = chainlinks2(str, adjs, 2);
  for (r = 0; r < adj; r++) {
    apos = adjs[r];
    if (liberty_of_string(libs[0], apos)
	&& liberty_of_string(libs[1], apos))
      break_chain_moves(apos, moves, scores, &num_moves);
  }
  
  propose_edge_moves(str, libs, liberties, moves, scores, &num_moves, other);
  order_moves(str, num_moves, moves, scores, other, read_function_name);

  for (k = 0; k < num_moves; k++) {
    int new_komaster;
    int new_kom_pos;
    int ko_move;

    apos = moves[k];
    if (komaster_trymove(apos, other, "attack2-A", str,
			 komaster, kom_pos, &new_komaster, &new_kom_pos,
			 &ko_move, stackp <= ko_depth && savecode == 0)) {
      if (!ko_move) {
	dcode = do_find_defense(str, NULL, new_komaster, new_kom_pos);
	if (dcode != WIN
	    && do_attack(str, NULL, new_komaster, new_kom_pos)) {
	  if (dcode == 0) {
	    popgo();
	    SGFTRACE(apos, WIN, "attack effective");
	    READ_RETURN(read_result, move, apos, WIN);
	  }
	  UPDATE_SAVED_KO_RESULT(savecode, savemove, dcode, apos);
	}
      }
      else {
	if (do_find_defense(str, NULL, new_komaster, new_kom_pos) != WIN
	    && do_attack(str, NULL, new_komaster, new_kom_pos) != 0) {
	  savemove = apos;
	  savecode = KO_B;
	}
      }
      popgo();
    }
  }
  
  /* The simple ataris didn't work. Try something more fancy. */
  acode = find_cap2(str, libs[0], libs[1], &xpos, komaster, kom_pos);
  if (acode == WIN) {
    SGFTRACE(xpos, WIN, "find cap2");
    READ_RETURN(read_result, move, xpos, WIN);
  }
  UPDATE_SAVED_KO_RESULT_UNREVERSED(savecode, savemove, acode, xpos);

  if (stackp <= backfill_depth) {
    acode = special_attack2(str, libs, &xpos, komaster, kom_pos);
    if (acode == WIN) {
      SGFTRACE(xpos, WIN, "special attack2");
      READ_RETURN(read_result, move, xpos, WIN);
    }
    UPDATE_SAVED_KO_RESULT_UNREVERSED(savecode, savemove, acode, xpos);
  }

  if (stackp <= backfill_depth) {
    acode = special_attack3(str, libs, &xpos, komaster, kom_pos);
    if (acode == WIN) {
      SGFTRACE(xpos, WIN, "special attack3");
      READ_RETURN(read_result, move, xpos, WIN);
    }
    UPDATE_SAVED_KO_RESULT_UNREVERSED(savecode, savemove, acode, xpos);
  }

  if (stackp <= backfill_depth) {
    acode = special_attack4(str, libs, &xpos, komaster, kom_pos);
    if (acode == WIN) {
      SGFTRACE(xpos, WIN, "special attack4");
      READ_RETURN(read_result, move, xpos, WIN);
    }
    UPDATE_SAVED_KO_RESULT_UNREVERSED(savecode, savemove, acode, xpos);
  }

  /* If it is not possible to make a direct atari, we try filling
   * a liberty of the superstring.
   */
  if (level >= 10
      && stackp <= backfill_depth
      && (stackp <= superstring_depth || !atari_possible)) {
    int liberties;
    int libs[MAX_LIBERTIES + 4];
    int liberty_cap = 2;

    if (stackp <= backfill2_depth)
      liberty_cap = 3;
    
    find_superstring_liberties(str, &liberties, libs, liberty_cap);
    if (liberties <= 5) {
      for (k = 0; k < liberties; k++) {
	int apos = libs[k];
	
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
		  if (dcode == 0) {
		    popgo();
		    SGFTRACE(xpos, WIN, "attack effective");
		    READ_RETURN(read_result, move, xpos, WIN);
		  }
		  UPDATE_SAVED_KO_RESULT(savecode, savemove, dcode, xpos);
		}
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
	      if (dcode == 0) {
		popgo();
		SGFTRACE(apos, WIN, "attack effective");
		READ_RETURN(read_result, move, apos, WIN);
	      }
	      UPDATE_SAVED_KO_RESULT(savecode, savemove, dcode, apos);
	    }
	    popgo();
	  }
	}
      }
    }
  }

  if (savecode == 0) {
    RTRACE("ALIVE!!\n");
    SGFTRACE(0, 0, NULL);
    READ_RETURN0(read_result);
  }

  SGFTRACE(savemove, savecode, "saved move");
  READ_RETURN(read_result, move, savemove, savecode);
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
  int moves[MAX_MOVES];
  int scores[MAX_MOVES];
  int num_moves = 0;
  int savemove = 0;
  int savecode = 0;
  int found_read_result;
  Read_result *read_result;
  
  SETUP_TRACE_INFO("attack3", str);
  reading_node_counter++;
  
  gg_assert(board[str] != EMPTY);
  
  if ((stackp <= depth) && (hashflags & HASH_ATTACK3)) {
    found_read_result = get_read_result(ATTACK3, komaster, kom_pos,
					&str, &read_result);
    if (found_read_result) {
      TRACE_CACHED_RESULT(*read_result);
      if (rr_get_result(*read_result) != 0)
	*move = rr_get_move(*read_result);
      
      SGFTRACE(rr_get_move(*read_result),
	       rr_get_result(*read_result), "cached");
      return rr_get_result(*read_result);
    }
    
    /* This data should always be recorded. */
    if (read_result) {
      rr_set_compressed_data(*read_result, ATTACK3, komaster, kom_pos, 
			     str, stackp);
    }
  }
  else
    read_result = NULL;
  
  if (stackp > depth) {
    SGFTRACE(0, 0, "stackp > depth");
    READ_RETURN0(read_result);
  }
  
  adj = chainlinks2(str, adjs, 1);
  for (r = 0; r < adj; r++) {
    int hpos;
    break_chain_moves(adjs[r], moves, scores, &num_moves);
    
    findlib(adjs[r], 1, &hpos);
    ADD_CANDIDATE_MOVE(hpos, 0, moves, scores, num_moves);
  }
  
  /* Defend against double atari in the surrounding chain early. */
  double_atari_chain2(str, moves, scores, &num_moves);
  
  /* Get the three liberties of (str). */
  liberties = findlib(str, 3, libs);
  ASSERT1(liberties == 3, str);
  
  for (k = 0; k < 3; k++) {
#if 0
    int libs2[2];
#endif
    int apos = libs[k];
    /* we only want to consider the move at (ai,aj) if:
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
      ADD_CANDIDATE_MOVE(apos, 0, moves, scores, num_moves);

    if (edge_closing_backfill(str, apos, &xpos))
      ADD_CANDIDATE_MOVE(xpos, 0, moves, scores, num_moves);

#if 0
    /* Try backfilling if atari is impossible. */
    if (stackp <= backfill_depth
	&& approxlib(apos, other, 2, libs2) == 1) {
      ADD_CANDIDATE_MOVE(libs2[0], 0, moves, scores, num_moves);
    }
#endif
    
    /* Look for edge blocking moves. */
    edge_block(str, apos, moves, scores, &num_moves);
  }
  
  /* Pick up some edge moves. */
  propose_edge_moves(str, libs, liberties, moves, scores, &num_moves, other);
  order_moves(str, num_moves, moves, scores, other, read_function_name);

  /* Try the moves collected so far. */
  for (k = 0; k < num_moves; k++) {
    int new_komaster;
    int new_kom_pos;
    int ko_move;
    
    if (stackp >= branch_depth && k > 0)
      break;
    xpos = moves[k];
    if (komaster_trymove(xpos, other, "attack3-A", str,
			 komaster, kom_pos, &new_komaster, &new_kom_pos,
			 &ko_move, stackp <= ko_depth && savecode == 0)) {
      if (!ko_move) {
	dcode = do_find_defense(str, NULL, new_komaster, new_kom_pos);
	if (dcode != WIN && do_attack(str, NULL, new_komaster, new_kom_pos)) {
	  if (dcode == 0) {
	    popgo();
	    SGFTRACE(xpos, WIN, "attack effective");
	    READ_RETURN(read_result, move, xpos, WIN);
	  }
	  UPDATE_SAVED_KO_RESULT(savecode, savemove, dcode, xpos);
	}
      }
      else {
	if (do_find_defense(str, NULL, new_komaster, new_kom_pos) != WIN
	    && do_attack(str, NULL, new_komaster, new_kom_pos) != 0) {
	  savemove = xpos;
	  savecode = KO_B;
	}
      }
      popgo();
    }
  }
    
  /* The simple ataris didn't work. Try something more fancy. */
  if (stackp <= backfill_depth) {
    int acode = find_cap3(str, &xpos, komaster, kom_pos);
    if (acode == WIN) {
      SGFTRACE(xpos, WIN, "find cap3");
      READ_RETURN(read_result, move, xpos, WIN);
    }
    UPDATE_SAVED_KO_RESULT_UNREVERSED(savecode, savemove, acode, xpos);
  }

  if (stackp <= fourlib_depth) {
    int acode = draw_back(str, &xpos, komaster, kom_pos);
    if (acode == WIN) {
      SGFTRACE(xpos, WIN, "draw back");
      READ_RETURN(read_result, move, xpos, WIN);
    }
    UPDATE_SAVED_KO_RESULT_UNREVERSED(savecode, savemove, acode, xpos);
  }

  /* Try to defend chain links with two liberties. */
  if (stackp <= backfill2_depth) {
    int saved_num_moves = num_moves;
    adj = chainlinks2(str, adjs, 2);
    for (r = 0; r < adj; r++) {
      int libs2[2];
      findlib(adjs[r], 2, libs2);
      if (approxlib(libs2[0], other, 4, NULL) > 3
	  && approxlib(libs2[1], other, 4, NULL) > 3)
	continue;
      break_chain_moves(adjs[r], moves, scores, &num_moves);
      break_chain2_moves(adjs[r], moves, scores, &num_moves, 1);
      for (k = 0; k < 2; k++)
	ADD_CANDIDATE_MOVE(libs2[k], 0, moves, scores, num_moves);
    }
    /* Only order and test the new set of moves. */
    order_moves(str, num_moves-saved_num_moves,
		&(moves[saved_num_moves]),
		&(scores[saved_num_moves]), other, read_function_name);
    for (k = saved_num_moves; k < num_moves; k++) {
      int new_komaster;
      int new_kom_pos;
      int ko_move;

      if (stackp >= branch_depth && k > 0)
	break;
      xpos = moves[k];
      
      if (komaster_trymove(xpos, other, "attack3-C", str,
			   komaster, kom_pos, &new_komaster, &new_kom_pos,
			   &ko_move, stackp <= ko_depth && savecode == 0)) {
	if (!ko_move) {
	  dcode = do_find_defense(str, NULL, new_komaster, new_kom_pos);
	  if (dcode != WIN
	      && do_attack(str, NULL, new_komaster, new_kom_pos)) {
	    if (dcode == 0) {
	      popgo();
	      SGFTRACE(xpos, WIN, "attack effective");
	      READ_RETURN(read_result, move, xpos, WIN);
	    }
	    UPDATE_SAVED_KO_RESULT(savecode, savemove, dcode, xpos);
	  }
	}
	else {
	  if (do_find_defense(str, NULL, new_komaster, new_kom_pos) != WIN
	      && do_attack(str, NULL, new_komaster, new_kom_pos) != 0) {
	    savemove = xpos;
	    savecode = KO_B;
	  }
	}
	popgo();
      }
    }
  }
    
  /* If nothing else works, we try filling a liberty of the
   * super_string.
   */
  if (level >= 10 && stackp <= backfill2_depth) {
    int liberties;
    int libs[MAX_LIBERTIES + 4];

    find_superstring_liberties(str, &liberties, libs, 3);
    if (liberties <= 5) {
      for (k = 0; k < liberties; k++) {
	int apos = libs[k];
	
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
		  if (dcode == 0) {
		    popgo();
		    SGFTRACE(xpos, WIN, "attack effective");
		    READ_RETURN(read_result, move, xpos, WIN);
		  }
		  UPDATE_SAVED_KO_RESULT(savecode, savemove, dcode, xpos);
		}
		popgo();
	      }
	    }
	    else
	      popgo();
	  }
	  else {
	    dcode = do_find_defense(str, NULL, komaster, kom_pos);
	    if (dcode != WIN && do_attack(str, NULL, komaster, kom_pos)) {
	      if (dcode == 0) {
		popgo();
		SGFTRACE(apos, WIN, "attack effective");
		READ_RETURN(read_result, move, apos, WIN);
	      }
	      UPDATE_SAVED_KO_RESULT(savecode, savemove, dcode, apos);
	    }
	    popgo();
	  }
	}
      }
    }
  }

  if (savecode != 0) {
    SGFTRACE(savemove, savecode, "saved move");
    READ_RETURN(read_result, move, savemove, savecode);
  }
  
  SGFTRACE(0, 0, NULL);
  READ_RETURN0(read_result);
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
  int moves[MAX_MOVES];
  int scores[MAX_MOVES];
  int num_moves = 0;
  Read_result *read_result = NULL;
  int savemove = 0;
  int savecode = 0;

  SETUP_TRACE_INFO("attack4", str);
  
  gg_assert(board[str] != EMPTY);
  reading_node_counter++;
  
  if (stackp > depth) {
    SGFTRACE(0, 0, "stackp > depth");
    return 0;
  }

  adj = chainlinks2(str, adjs, 1);
  for (r = 0; r < adj; r++) {
    int hpos;
    break_chain_moves(adjs[r], moves, scores, &num_moves);
    
    findlib(adjs[r], 1, &hpos);
    ADD_CANDIDATE_MOVE(hpos, 0, moves, scores, num_moves);
  }


  /* Defend against double atari in the surrounding chain early. */
  double_atari_chain2(str, moves, scores, &num_moves);

  /* Give a score bonus to the chain preserving moves. */
  for (k = 0; k < num_moves; k++)
    scores[k] += 5;
  
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
      ADD_CANDIDATE_MOVE(apos, 0, moves, scores, num_moves);

    if (edge_closing_backfill(str, apos, &xpos))
      ADD_CANDIDATE_MOVE(xpos, 10, moves, scores, num_moves);

    /* Look for edge blocking moves. */
    edge_block(str, apos, moves, scores, &num_moves);
  }

  /* Pick up some edge moves. */
  propose_edge_moves(str, libs, liberties, moves, scores, &num_moves, other);
  order_moves(str, num_moves, moves, scores, other, read_function_name);

  /* Try the moves collected so far. */
  for (k = 0; k < num_moves; k++) {
    int new_komaster;
    int new_kom_pos;
    int ko_move;

    if (stackp >= branch_depth && k > 0)
      break;
    xpos = moves[k];
    /* Conditional ko capture is disabled because it seems to expensive. */
    if (komaster_trymove(xpos, other, "attack4-A", str,
			 komaster, kom_pos, &new_komaster, &new_kom_pos,
			 &ko_move, 0 && stackp <= ko_depth && savecode == 0)) {
      if (!ko_move) {
	dcode = do_find_defense(str, NULL, new_komaster, new_kom_pos);
	if (dcode != WIN && do_attack(str, NULL, new_komaster, new_kom_pos)) {
	  if (dcode == 0) {
	    popgo();
	    SGFTRACE(xpos, WIN, "attack effective");
	    READ_RETURN(read_result, move, xpos, WIN);
	  }
	  UPDATE_SAVED_KO_RESULT(savecode, savemove, dcode, xpos);
	}
      }
      else {
	if (do_find_defense(str, NULL, new_komaster, new_kom_pos) != WIN
	    && do_attack(str, NULL, new_komaster, new_kom_pos) != 0) {
	  savemove = xpos;
	  savecode = KO_B;
	}
      }
      popgo();
    }
  }

  if (savecode != 0) {
    SGFTRACE(savemove, savecode, "saved move");
    *move = savemove;
    return savecode;
  }

  SGFTRACE(0, 0, NULL);
  return 0;
}


/* If (str) points to a string with 2 or 3 liberties,
 * find_cap2(str, ai, aj, bi, bj, &i, &j, komaster)
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
  RTRACE("trying to capture %1m with capping move at %1m\n", str, *move);
  if (trymove(*move, OTHER_COLOR(board[str]), "find_cap2", str,
	      komaster, kom_pos)) {
    int dcode = do_find_defense(str, NULL, komaster, kom_pos);
    popgo();
    switch (dcode) {
    case 0:
      RTRACE("cap2 succeeded!\n");
      return WIN;
      break;
    case WIN:
      RTRACE("cap2 failed!\n");
      return 0;
      break;
    case KO_B:
      RTRACE("cap2 succeeded with ko return code KO_B\n");
      return KO_B;
      break;
    case KO_A:
      RTRACE("cap2 succeeded with ko return code KO_A\n");
      return KO_A;
      break;
    }
  }

  return 0;
}    


/* If (str) points to a string with 3 liberties, find_cap3(str, &move)
 * looks for a configuration of the following type:
 *
 *  XXa
 *  cb*
 *
 * where X are elements of the string in question and a, b and c are
 * its liberties. It tries the move at * and returns true this move
 * captures the string, leaving (*move) pointing to *.
 */

static int
find_cap3(int str, int *move, int komaster, int kom_pos)
{
  int alib, blib;
  int libs[3];
  int xpos = 0;
  int k;
  int savemove = 0;
  int savecode = 0;
  int acode;

  if (findlib(str, 3, libs) != 3)
    return 0;

  for (k = 0; k < 3; k++) {
    /* k and k+1 mod 3 will be (0,1), (1,2) and (2,0); These are the 
     * three combinations of indices that we have to send to find_cap2.
     */
    alib = libs[k];
    blib = libs[(k+1)%3];

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
 * XO.OX        at 'c=(xi,xj)'.
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
	  && (liberty_of_string(SOUTH(libs[k]), str)
	      || liberty_of_string(WEST(libs[k]), str)
	      || liberty_of_string(NORTH(libs[k]), str)
	      || liberty_of_string(EAST(libs[k]), str))) {
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


/* The first version of this function seemed to induce too many
 * variations and has therefore been replaced by a much more limited
 * version.
 */
#if 0

/* In positions like
 *
 * OO...
 * XXO*.
 * x.X*.
 * -----
 *
 * where the X stones to the left are being attacked, it is often a
 * good idea to first consider either or both of the moves marked by *
 * in the diagram. Notice that propose_edge_moves() doesn't help with
 * this, since the rightmost X stone is not part of the attacked
 * string, only the corresponding superstring.
 *
 * This function identifies the situation
 *
 * ?XO.?   ?bdf?
 * ?.X.o   haceg
 * -----   -----
 *
 * where a is a liberty of the attacked string, b is a stone of the
 * attacked string, and e and f are the considered moves. Also
 * considered is the situation where the conditions to the right are
 * not correct but c has only two liberties anyway. If safe, the move
 * to make atari on c is proposed.
 *
 * Notice, this code is disabled, as commented above.
 */

static void
edge_block(int str, int apos, int moves[MAX_MOVES], int scores[MAX_MOVES],
	   int *num_moves)
{
  int color = board[str];
  int other = OTHER_COLOR(color);
  int cpos;
  int dpos;
  int epos;
  int fpos;
  int gpos;
  int hpos;
  int score;
  int k, l;

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

      if (board[cpos] != color || board[dpos] != other)
	continue;

      epos = cpos + right;
      fpos = dpos + right;
      gpos = epos + right;
      hpos = apos - right;
      
      if (!ON_BOARD(epos))
	continue;
      
      if (board[epos] == EMPTY && board[fpos] == EMPTY 
	  && (board[gpos] != color)) {
	/* Everything is set up, suggest moves at e and f. */
	if (!ON_BOARD(hpos) || board[hpos] == color)
	  score = 0;
	else
	  score = -5;
	if (countlib(str) == 2)
	  score -= 10;
	ADD_CANDIDATE_MOVE(epos, score, moves, scores, *num_moves);

	if (countlib(dpos) == 1)
	  score = 25;
	else
	  score = 0;
	if (countlib(str) == 2)
	  score -= 10;
	ADD_CANDIDATE_MOVE(fpos, score, moves, scores, *num_moves);
      }
      else if (countlib(cpos) == 2 && countlib(dpos) > 1) {
	int libs[2];
	int move;
	findlib(cpos, 2, libs);
	if (libs[0] == apos)
	  move = libs[1];
	else
	  move = libs[0];
	if (!is_self_atari(move, other))
	  ADD_CANDIDATE_MOVE(move, 0, moves, scores, *num_moves);
      }
    }
  }
}

#else

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
edge_block(int str, int apos, int moves[MAX_MOVES], int scores[MAX_MOVES],
	   int *num_moves)
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
	ADD_CANDIDATE_MOVE(fpos, 30, moves, scores, *num_moves);
    }
  }
}

#endif

/* ================================================================ */
/*            Defending by attacking surrounding strings            */
/* ================================================================ */

/* Add the chainbreaking moves relative to the string (str) to the
 * (moves[]) array.
 */
static void
break_chain_moves(int str, int moves[MAX_MOVES], int scores[MAX_MOVES],
		  int *num_moves)
{
  int r;
  int xpos;
  int adj, adjs[MAXCHAIN];
  
  /* Find links in atari. */
  adj = chainlinks2(str, adjs, 1);
  
  for (r = 0; r < adj; r++) {
    findlib(adjs[r], 1, &xpos);
    ADD_CANDIDATE_MOVE(xpos, 0, moves, scores, *num_moves);
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
break_chain2_efficient_moves(int str, int moves[MAX_MOVES],
			     int scores[MAX_MOVES], int *num_moves)
{
  int r;
  int adj, adjs[MAXCHAIN];
  int adj2, adjs2[MAXCHAIN];
  int libs[2];
  int ai, aj;
  int bi, bj;
  int ci, cj;
  
  /* Find links with 2 liberties. */
  adj = chainlinks2(str, adjs, 2);
  
  for (r = 0; r < adj; r++) {
    adj2 = chainlinks2(adjs[r], adjs2, 1);
    if (adj2 == 1 && countlib(str) > 2) {
      int apos;
      findlib(adjs2[0], 1, &apos);
      ADD_CANDIDATE_MOVE(apos, 0, moves, scores, *num_moves);
      continue;
    }
    
    if (adj2 > 1)
      continue;
    
    findlib(adjs[r], 2, libs);
    if (approxlib(libs[0], board[adjs[r]], 3, NULL) <= 2)
      ADD_CANDIDATE_MOVE(libs[1], 0, moves, scores, *num_moves);
    if (approxlib(libs[1], board[adjs[r]], 3, NULL) <= 2)
      ADD_CANDIDATE_MOVE(libs[0], 0, moves, scores, *num_moves);
    
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
    ai = I(libs[0]);
    aj = J(libs[0]);
    bi = I(libs[1]);
    bj = J(libs[1]);
    ci = I(adjs[r]);
    cj = J(adjs[r]);
    if (gg_abs(ai - bi) == 1
	&& gg_abs(aj - bj) == 1
	&& (ci == 1 || ci == board_size - 2
	    || cj == 1 || cj == board_size - 2)) {

      if ((ai == 0 || ai == board_size - 1
	   || aj == 0 || aj == board_size - 1)
	  && !is_self_atari(libs[1], board[str]))
	ADD_CANDIDATE_MOVE(libs[1], 0, moves, scores, *num_moves);

      if ((bi == 0 || bi == board_size - 1
	   || bj == 0 || bj == board_size - 1)
	  && !is_self_atari(libs[0], board[str]))
	ADD_CANDIDATE_MOVE(libs[0], 0, moves, scores, *num_moves);
    }
  }
}

static void
break_chain2_moves(int str, int moves[MAX_MOVES],
		   int scores[MAX_MOVES], int *num_moves, int require_safe)
{
  int color = board[str];
  int other = OTHER_COLOR(color);
  int r;
  int k;
  int apos;
  int adj;
  int adjs[MAXCHAIN];
  int libs[2];
  int unsafe[2];

  adj = chainlinks2(str, adjs, 2);
  
  for (r = 0; r < adj; r++) {
    apos = adjs[r];

    findlib(apos, 2, libs);
    for (k = 0; k < 2; k++) {
      unsafe[k] = is_self_atari(libs[k], color);
      if (!unsafe[k]
	  || (!require_safe
	      && approxlib(libs[k], other, 5, NULL) < 5))
	ADD_CANDIDATE_MOVE(libs[k], 0, moves, scores, *num_moves);
    }

    if (stackp <= backfill2_depth) {
      break_chain_moves(apos, moves, scores, num_moves);
      if (unsafe[0] && unsafe[1]) {
	int libs2[3];
	for (k = 0; k < 2; k++) {
	  if (approxlib(libs[k], other, 3, libs2) == 2) {
	    int s;
	    for (s = 0; s < 2; s++)
	      if (!is_self_atari(libs2[s], color))
		ADD_CANDIDATE_MOVE(libs2[s], 0, moves, scores, *num_moves);
	  }
	}
      }
    }
  }
}

/*
 * (str) points to a group. break_chain2(str, &move)
 * returns WIN if there is a string in the surrounding chain having
 * exactly two liberties whose attack leads to the rescue of
 * (str). Then (*move) points to the location of the attacking move.
 * 
 * Returns KO_A if the saving move depends on ignoring a ko threat;
 * 
 * Returns KO_B if the saving move requires making a ko threat and winning
 * the ko.
 */

static int 
break_chain2(int str, int *move, int komaster, int kom_pos)
{
  int color = board[str];
  int other = OTHER_COLOR(color);
  int v;
  int libs[2];
  int liberties;
  int moves[MAX_MOVES];
  int scores[MAX_MOVES];
  int num_moves = 0;
  int savemove = 0;
  int savecode = 0;

  SETUP_TRACE_INFO("break_chain2", str);
  
  RTRACE("in break_chain2 at %1m\n", str);
  
  break_chain2_moves(str, moves, scores, &num_moves, 0);
  order_moves(str, num_moves, moves, scores, color, read_function_name);

  /* We do not wish to consider the move if it can be 
   * immediately recaptured, unless stackp <= backfill_depth.
   *
   * With the incremental board code it's not much point in using
   * approxlib() as heuristic whether we want to play the move. Better
   * and more accurate to really play the move and then use findlib() to
   * see how many liberties we obtained.
   */

  for (v = 0; v < num_moves; v++) {
    int new_komaster;
    int new_kom_pos;
    int ko_move;

    if (komaster_trymove(moves[v], color, "break_chain2-A", str,
			 komaster, kom_pos, &new_komaster, &new_kom_pos,
			 &ko_move, savecode == 0 && stackp <= ko_depth)) {
      if (ko_move) {
	if (do_attack(str, NULL, new_komaster, new_kom_pos) != WIN) {
	  savecode = KO_B;
	  savemove = moves[v];
	}
	popgo();
	continue;
      }
    }
    else
      continue;

    /* If we reach here, we have made a move, which was not a
     * conditional ko capture.
     */
    liberties = findlib(moves[v], 2, libs);
    if (liberties > 1) {
      int acode = do_attack(str, NULL, new_komaster, new_kom_pos);
      if (acode == 0) {
	*move = moves[v];
	popgo();
	SGFTRACE(moves[v], 1, "attack defended-A");
	return WIN;
      }
      UPDATE_SAVED_KO_RESULT(savecode, savemove, acode, moves[v]);
    }
    else if (stackp <= backfill_depth) {
      int newer_komaster;
      int newer_kom_pos;
      int bpos = libs[0];
      int try_harder = 0;

      ASSERT1(liberties == 1, str);

      if (komaster_trymove(bpos, other, "break_chain2-C", str,
			   new_komaster, new_kom_pos,
			   &newer_komaster, &newer_kom_pos,
			   &ko_move, savecode == 0 && stackp <= ko_depth)) {
	if (!ko_move) {
	  if (countlib(bpos) <= 2)
	    try_harder = 1;
	  if (board[str] != EMPTY) {
	    int dcode = do_find_defense(str, NULL, newer_komaster,
					newer_kom_pos);
	    if (dcode == WIN && !try_harder) {
	      *move = moves[v];
	      popgo();
	      popgo();
	      SGFTRACE(moves[v], WIN, "attack defended-B");
	      return WIN;
	    }
	    /* FIXME: Possibly the ko result codes are not handled
	     * correctly in the presence of two trymove().
	     */
	    UPDATE_SAVED_KO_RESULT_UNREVERSED(savecode, savemove,
					      dcode, moves[v]);
	  }
	  popgo();
	}
	else {
	  try_harder = 1;
	  ASSERT1(board[str] != EMPTY, str);
	  if (do_find_defense(str, NULL, newer_komaster, newer_kom_pos) != 0) {
	    savecode = KO_A; /* Not KO_B since we are one move deeper 
			      * than usual. */
	    savemove = moves[v];
	  }
	  popgo();
	}
      }

      if (try_harder) {
	int acode = do_attack(str, NULL, new_komaster, new_kom_pos);
	if (acode == 0) {
	  *move = moves[v];
	  popgo();
	  SGFTRACE(moves[v], WIN, "attack defended-C");
	  return WIN;
	}
	UPDATE_SAVED_KO_RESULT(savecode, savemove, acode, moves[v]);
      }
    }
    popgo(); /* (moves[v]) */
  }

  if (savecode != 0) {
    *move = savemove;
    SGFTRACE(savemove, savecode, "saved move");
    return savecode;
  }

  SGFTRACE(0, 0, NULL);
  return 0;
}


/*
 * (si,sj) points to a group. break_chain3(str, *i, *j)
 * returns 1 if there is a string in the surrounding chain having
 * exactly three liberties whose attack leads to the rescue of
 * (str). Then (*i, *j) points to the location of the attacking move.
 * 
 * Returns KO_A if the saving move depends on ignoring a ko threat;
 * 
 * Returns KO_B if the saving move requires making a ko threat and winning
 *   the ko.
 */

static int 
break_chain3(int str, int *move, int komaster, int kom_pos)
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
  int moves[MAX_MOVES];
  int mw[BOARDMAX];
  int savemove = 0;
  int savecode = 0;
  int liberties = countlib(str);

  SETUP_TRACE_INFO("break_chain3", str);

  memset(mw, 0, sizeof(mw));
  
  RTRACE("in break_chain3 at %1m\n", str);
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
      moves[u++] = libs[0];
      continue;
    }
    
    if (lib2 >= 4 && !mw[libs[1]]) {
      mw[libs[1]] = 1;
      moves[u++] = libs[1];
      continue;
    }
    
    if (lib3 >= 4 && !mw[libs[2]]) {
      mw[libs[2]] = 1;
      moves[u++] = libs[2];
      continue;
    }

    /* No easy escape, try all liberties. */
    for (k = 0; k < 3; k++) {
      if (!mw[libs[k]]) {
	mw[libs[k]] = 1;
	moves[u++] = libs[k];
      }
    }
  }
  
  /* We do not wish to consider the move if it can be 
   * immediately recaptured, unless stackp <= backfill2_depth.
   */

  for (v = 0; v < u; v++) {
    if (!trymove(moves[v], color, "break_chain3-A", str, komaster, kom_pos))
      continue;

    if (countlib(moves[v]) == 1 && stackp > backfill2_depth) {
      popgo();
      continue;
    }
    
    /* If we just filled our own liberty we back out now */
    if (countlib(str) >= liberties) {
      int acode = do_attack(str, NULL, komaster, kom_pos);
      if (acode == 0) {
	*move = moves[v];
	popgo();
	SGFTRACE(moves[v], WIN, "attack defended");
	return WIN;
      }
      UPDATE_SAVED_KO_RESULT(savecode, savemove, acode, moves[v]);
    }
    popgo(); /* (moves[v]) */
  }

  if (savecode != 0) {
    *move = savemove;
    SGFTRACE(savemove, savecode, "saved move");
    return savecode;
  }

  SGFTRACE(0, 0, NULL);
  return 0;
}

/* This function looks for moves attacking those components
 * of the surrounding chain of the superstring (see find_superstring
 * for the definition) which have fewer than liberty_cap liberties,
 * and which are not adjacent to the string itself, since those
 * are tested by break_chain. If such a boundary chain can be
 * attacked, and if attacking the boundary results in saving
 * the (str) string, then success is reported.
 */
/* FIXME: Consider ko captures */
static int
superstring_breakchain(int str, int *move, int komaster, int kom_pos,
		       int liberty_cap)
{
  int color = board[str];
  int adj;
  int adjs[MAXCHAIN];
  int k;
  int apos;
  int savemove = 0;
  int savecode = 0;

  SETUP_TRACE_INFO("superstring_breakchain", str);

  proper_superstring_chainlinks(str, &adj, adjs, liberty_cap);
  for (k = 0; k < adj; k++) {
    int new_komaster, new_kom_pos;
    int ko_move;

    if (countlib(adjs[k]) > 1)
      continue;
    findlib(adjs[k], 1, &apos);

    if (komaster_trymove(apos, color, "superstring_break_chain", str,
			 komaster, kom_pos, &new_komaster, &new_kom_pos,
			 &ko_move, savecode == 0 && stackp <= ko_depth)) {
      if (!ko_move) {
	int acode = do_attack(str, NULL, new_komaster, new_kom_pos);
	if (acode == 0) {
	  popgo();
	  *move = apos;
	  SGFTRACE(apos, WIN, "attack defended");
	  return WIN;
	}
	else if (acode != WIN) {
	  UPDATE_SAVED_KO_RESULT(savecode, savemove, acode, apos);
	}
	popgo();
      }
      else {
	if (do_attack(str, NULL, new_komaster, new_kom_pos) != WIN) {
	  savemove = apos;
	  savecode = KO_B;
	}
	popgo();
      }
    }
  }
  
  if (savecode != 0) {
    *move = savemove;
    SGFTRACE(savemove, savecode, "saved move");
    return savecode;
  }

  SGFTRACE(0, 0, NULL);
  return 0;
}

/*
 * If str points to a group, double_atari_chain2() adds all moves
 * which make a double atari on some strings in the surrounding chain
 * to the (movei[], movej[]) arrays.
 */

static void
double_atari_chain2(int str, int moves[MAX_MOVES], int scores[MAX_MOVES],
		    int *num_moves)
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
	  ADD_CANDIDATE_MOVE(libs[k], 0, moves, scores, *num_moves);
      }
    }
  }
}


/* ================================================================ */
/*                Restricted Attack and Defense                     */
/* ================================================================ */

/* These functions try to attack and defend a string, avoiding moves
 * from a certain set. It is assumed that as soon as the string gets
 * three liberties, it is alive.
 * 
 * These functions can be used to generate backfilling moves as
 * follows: Suppose that we would like to make atari on a
 * string, but the atari is not safe until we make a backfilling
 * move. To find the backfilling move, we make a list of the
 * liberties of the string under attack, declaring these moves
 * forbidden. Neither player will play them while the restricted
 * functions are in effect. We fill the liberty, creating a
 * string which is under attack, and look for a defensive move
 * which avoids the forbidden moves. This is the backfilling
 * move.
 *
 * These are minimalist functions capable of reading a ladder
 * and not much more.
 */

/* Given a list of moves, restricted_defend1 tries to find a 
 * move that defends the string (str) with one liberty,
 * not considering moves from the list.
 */
static int
restricted_defend1(int str, int *move, int komaster, int kom_pos,
		   int num_forbidden_moves, int *forbidden_moves)
{
  int color = board[str];
  int other = OTHER_COLOR(color);
  int xpos;
  int lib;
  int moves[MAX_MOVES];
  int scores[MAX_MOVES];
  int num_moves = 0;
  int savemove = 0;
  int savecode = 0;
  int liberties;
  int k;

  SETUP_TRACE_INFO("restricted_defend1", str);
  reading_node_counter++;
  
  ASSERT1(board[str] != EMPTY, str);
  ASSERT1(countlib(str) == 1, str);

  RTRACE("try to escape atari on %1m.\n", str);

  /* (lib) will be the liberty of the string. */
  liberties = findlib(str, 1, &lib);
  ASSERT1(liberties == 1, str);

  /* Collect moves to try in the first batch.
   * 1. First order liberty.
   * 2. Chain breaking moves.
   */
  moves[0] = lib;
  scores[0] = 0;
  num_moves = 1;
  
  break_chain_moves(str, moves, scores, &num_moves);
  order_moves(str, num_moves, moves, scores, color, read_function_name);
  for (k = 0; k < num_moves; k++) {
    int ko_capture;

    xpos= moves[k];
    if (in_list(xpos, num_forbidden_moves, forbidden_moves))
      continue;
    /* To avoid loops with double ko, we do not allow any ko captures,
     * even legal ones, if the opponent is komaster.
     */
    if (is_ko(xpos, color, NULL))
      ko_capture = 1;
    else
      ko_capture = 0;

    if ((komaster != other || !ko_capture)
	&& trymove(xpos, color, "restricted_defend1-A", str,
		   komaster, kom_pos)) {
      int libs = countlib(str);
      if (libs > 2) {
	popgo();
	SGFTRACE(xpos, WIN, "defense effective");
	if (move)
	  *move = xpos;
	return WIN;
      }
      if (libs == 2) {
	int acode;

	if (!ko_capture)
	  acode = restricted_attack2(str, NULL, komaster, kom_pos,
				     num_forbidden_moves, forbidden_moves);
	else
	  acode = restricted_attack2(str, NULL, color, xpos,
				     num_forbidden_moves, forbidden_moves);
	popgo();
	if (acode == 0) {
	  SGFTRACE(xpos, WIN, "defense effective");
	  if (move)
	    *move = xpos;
	  return WIN;
	}
	/* if the move works with ko we save it, then look for something
	 * better.
	 */
	UPDATE_SAVED_KO_RESULT(savecode, savemove, acode, xpos);
      }
      else
	popgo();
    }
    else {
      int ko_pos;
      if (stackp <= ko_depth
	  && savecode == 0 
	  && (komaster == EMPTY
	      || (komaster == color
		  && kom_pos == xpos))
	  && is_ko(xpos, color, &ko_pos)
	  && tryko(xpos, color, "restricted_defend1-B", color, ko_pos)) {
	int libs = countlib(str);
	if (libs > 2) {
	  popgo();
	  UPDATE_SAVED_KO_RESULT(savecode, savemove, 2, xpos);
	}
	else if (libs == 2) {
	  int acode;
	  acode = restricted_attack2(str, NULL, color, xpos,
				     num_forbidden_moves, forbidden_moves);
	  popgo();
	  UPDATE_SAVED_KO_RESULT(savecode, savemove, acode, xpos);
	}
	else
	  popgo();
      }
    }
  }

  if (savecode != 0) {
    if (move)
      *move = savemove;
    SGFTRACE(savemove, savecode, "saved move");
    return savecode;
  }

  SGFTRACE(0, 0, NULL);
  return 0;
}


/* Given a list of moves, restricted_attack2 tries to find a 
 * move that attacks the string (str) with two liberties,
 * not considering moves from the list.
 */
static int
restricted_attack2(int str, int *move, int komaster, int kom_pos,
		   int num_forbidden_moves, int *forbidden_moves)
{
  int color = board[str];
  int other = OTHER_COLOR(color);
  int apos;
  int liberties;
  int libs[2];
  int savemove = 0;
  int savecode = 0;
  int k;

  SETUP_TRACE_INFO("restricted_attack2", str);
  reading_node_counter++;

  str = find_origin(str);
  ASSERT1(board[str] != EMPTY, str);
  ASSERT1(countlib(str) == 2, str);

  RTRACE("restricted attack on %1m with 2 liberties\n", str);

  /* The attack may fail if a boundary string is in atari and cannot 
   * be defended.  First we must try defending such a string. 
   */
  /* Get the two liberties of (str). */
  liberties = findlib(str, 2, libs);
  ASSERT1(liberties == 2, str);

  for (k = 0; k < 2; k++) {
    int ko_pos;
    int ko_capture;

    apos = libs[k];
    if (in_list(apos, num_forbidden_moves, forbidden_moves))
      continue;
    /* To avoid loops with double ko, we do not allow any ko captures,
     * even legal ones, if the opponent is komaster. 
     */
    if (is_ko(apos, other, &ko_pos))
      ko_capture = 1;
    else
      ko_capture = 0;

    if ((komaster != color || !ko_capture)
	&& trymove(apos, other, "restricted_attack2", str,
		   komaster, kom_pos)) {
      if ((!ko_capture 
	   && !restricted_defend1(str, NULL, komaster, kom_pos,
				  num_forbidden_moves, forbidden_moves))
	  || (ko_capture
	      && !restricted_defend1(str, NULL, other, ko_pos,
				     num_forbidden_moves, forbidden_moves))) {
	popgo();
	SGFTRACE(apos, WIN, "attack effective");
	if (move)
	  *move = apos;
	return WIN;
      }
      popgo();
    }
    else if (savecode == 0
	     && (komaster == EMPTY
		 || (komaster == other
		     && kom_pos == apos))
	     && tryko(apos, other, "restricted_attack2", komaster, kom_pos)) {
      if (!restricted_defend1(str, NULL, other, ko_pos,
			      num_forbidden_moves, forbidden_moves)) {
	popgo();
	savecode = KO_B;
	savemove = apos;
      }
      else
	popgo();
    }
  }

  if (savecode != 0) {
    if (move)
      *move = savemove;
    SGFTRACE(savemove, savecode, "saved move");
    return savecode;
  }

  SGFTRACE(0, 0, NULL);
  return 0;
}

/* Returns true if the move is in a given list of moves. */

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
static int defend_lib_score[6]              = {-5, -4,  0,  2,  5, 20};
static int defend_not_adjacent_lib_score[5] = { 0,  0,  2,  4,  6};
static int defend_capture_score[6]          = { 0,  6,  9, 13, 18, 24};
static int defend_atari_score[6]            = { 0,  2,  4,  6,  8, 10};
static int defend_save_score[6]             = { 0,  3,  6,  8, 10, 12};
static int defend_open_score[5]             = { 0,  1,  2,  3,  4};
static int attack_own_lib_score[5]          = {10, -4,  2,  3,  4};
static int attack_string_lib_score[6]       = {-5,  1,  3,  7, 12, 20};
static int attack_capture_score[6]          = {-4,  4, 10, 15, 20, 25};
static int attack_save_score[6]             = { 0, 11, 15, 18, 21, 24};
static int attack_open_score[5]             = { 0,  1,  2,  3,  4};
static int defend_not_edge_score            = 5;
static int attack_not_edge_score            = 1;
static int attack_ko_score                  = -15;
static int cannot_defend_penalty            = -20;
static int safe_atari_score                 = 5;


/* The string at (str) is under attack. The num_moves moves in
 * (movei[], movej[]) for color color have been deemed interesting in
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
 */

static void
order_moves(int str, int num_moves, int *moves, int *scores, int color,
	    const char *funcname)
{
  int string_color = board[str];
  int string_libs = countlib(str);
  int r;
  int i, j;
  int maxscore;
  int max_at;

  /* don't bother sorting if only one move, or none at all */
  if (num_moves < 2)
    return;

  /* Assign a score to each move. */
  for (r = 0; r < num_moves; r++) {
    int move = moves[r];

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
      
      int libs = approxlib(move, color, 5, NULL);
      if (number_same_string > 0) {
	if (libs > 5 || (libs == 4 && stackp > fourlib_depth))
	  scores[r] += defend_lib_score[5];
	else
	  scores[r] += defend_lib_score[libs];
      }
      else {
	/* Add points for the number of liberties the played stone
         * obtains when not adjacent to the attacked string.
	 */
	if (libs > 4)
	  scores[r] += defend_not_adjacent_lib_score[4];
	else
	  scores[r] += defend_not_adjacent_lib_score[libs];
      }
      
      /* 2) Add the number of open liberties near the move to its score. */
      gg_assert(number_open <= 4);
      scores[r] += defend_open_score[number_open];
      
      /* 3) Add a bonus if the move is not on the edge. 
       */
      if (number_edges == 0 || captured_stones > 0)
	scores[r] += defend_not_edge_score;
      
      /* 4) Add thrice the number of captured stones. */
      if (captured_stones <= 5)
	scores[r] += defend_capture_score[captured_stones];
      else
	scores[r] += defend_capture_score[5] + captured_stones;

      /* 5) Add points for stones put into atari, unless this is a
       *    self atari.
       */
      if (libs + captured_stones > 1) {
	if (threatened_stones <= 5)
	  scores[r] += defend_atari_score[threatened_stones];
	else
	  scores[r] += defend_atari_score[5] + threatened_stones;
      }

      /* 6) Add a bonus for saved stones. */
      if (saved_stones <= 5)
	scores[r] += defend_save_score[saved_stones];
      else
	scores[r] += defend_save_score[5];
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
      scores[r] += attack_own_lib_score[libs];

      if (libs == 0 && captured_stones == 1)
	scores[r] += attack_ko_score;
      
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
	scores[r] += attack_string_lib_score[liberties];

	safe_atari = (string_libs <= 2 && libs + captured_stones > 1);
	/* The defender can't play here without getting into atari, so
         * we probably souldn't either.
	 */
	if (liberties == 1 && saved_stones == 0 && !safe_atari)
	  scores[r] += cannot_defend_penalty;

	/* Bonus if we put the attacked string into atari without
         * ourselves getting into atari.
	 */
	if (safe_atari)
	  scores[r] += safe_atari_score;
      }
      
      /* 3) Add the number of open liberties near the move to its score. */
      gg_assert(number_open <= 4);
      scores[r] += attack_open_score[number_open];
      
      /* 4) Add a bonus if the move is not on the edge. */
      if (number_edges == 0)
	scores[r] += attack_not_edge_score;
      
      /* 5) Add twice the number of captured stones. */
      if (captured_stones <= 5)
	scores[r] += attack_capture_score[captured_stones];
      else
	scores[r] += attack_capture_score[5];

      /* 6) Add a bonus for saved stones. */
      if (saved_stones <= 5)
	scores[r] += attack_save_score[saved_stones];
      else
	scores[r] += attack_save_score[5];
    }
  }
  
  /* Now sort the moves.  We use selection sort since this array will
   * probably never be more than 10 moves long.  In this case, the
   * overhead imposed by quicksort will probably overshadow the gains
   * given by the O(n*log(n)) behaviour over the O(n^2) behaviour of
   * selection sort.
   */
  for (i = 0; i < num_moves-1; i++) {

    /* Find the move with the biggest score. */
    maxscore = scores[i];
    max_at = i;
    for (j = i+1; j < num_moves; j++) {
      if (scores[j] > maxscore) {
	maxscore = scores[j];
	max_at = j;
      }
    }

    /* Now exchange the move at i with the move at max_at.
     * Don't forget to exchange the scores as well.
     */
    if (max_at != i) {
      int temp = moves[i];
      int tempmax = scores[i];

      moves[i] = moves[max_at];
      scores[i] = scores[max_at];

      moves[max_at] = temp;
      scores[max_at] = tempmax;
    }
  }

  if (0) {
    gprintf("%oVariation %d:\n", count_variations);
    for (i = 0; i < num_moves; i++)
      gprintf("%o  %1M %d\n", moves[i], scores[i]);
  }

  if (sgf_dumptree) {
    char buf[500];
    char *pos;
    int chars;
    sprintf(buf, "Move order for %s: %n", funcname, &chars);
    pos = buf + chars;
    for (i = 0; i < num_moves; i++) {
      sprintf(pos, "%c%d (%d) %n", J(moves[i]) + 'A' + (J(moves[i]) >= 8),
	      board_size - I(moves[i]), scores[i], &chars);
      pos += chars;
    }
    sgftreeAddComment(sgf_dumptree, NULL, buf);
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
    int acode = attack(move, NULL);
    if (acode == 0)
      safe = WIN;
    else if (acode == WIN)
      safe = 0;
    else if (acode == KO_A)
      safe = KO_B;
    else if (acode == KO_B)
      safe = KO_A;
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

/* ============ Reading shadow and persistent cache =============== */


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

static void
draw_active_area(char p[BOARDMAX])
{
  int i, j, ii;
  int c = ' ';

  start_draw_board();
  
  for (i = 0; i < board_size; i++) {
    ii = board_size - i;
    fprintf(stderr, "\n%2d", ii);
    
    for (j = 0; j < board_size; j++) {
      if (p[POS(i, j)] == EMPTY)
	c = '.';
      else if (p[POS(i, j)] == WHITE)
	c = 'O';
      else if (p[POS(i, j)] == BLACK)
	c = 'X';
      if (p[POS(i, j)] == GRAY)
	c = '?';
      
      fprintf(stderr, " %c", c);
    }
    
    fprintf(stderr, " %d", ii);
  }

  end_draw_board();
}


/* Returns 1 if the stored board is compatible with the current board,
 * 0 otherwise.
 */
static int
verify_stored_board(char p[BOARDMAX])
{
  int k;
  for (k = BOARDMIN; k < BOARDMAX; k++)
    if (ON_BOARD(k) && p[k] != GRAY && p[k] != board[k])
      return 0;
  
  return 1;
}


/* Remove persistent cache entries which have (m, n) within their
 * active areas.
 */
void purge_persistent_reading_cache()
{
  int k;
  int r;
  static int last_purge_position_number = -1;
  gg_assert(stackp == 0);

  /* Never do this more than once per move. */
  if (last_purge_position_number == position_number)
    return;
  else
    last_purge_position_number = position_number;

  for (k = 0; k < persistent_reading_cache_size; k++) {
    int played_moves = 0;
    int entry_ok = 1;

    for (r = 0; r < MAX_CACHE_DEPTH; r++) {
      int apos = persistent_reading_cache[k].stack[r];
      int color = persistent_reading_cache[k].move_color[r];
      if (apos == 0)
	break;
      if (board[apos] == EMPTY
	  && trymove(apos, color, "purge_persistent_reading_cache", 0,
		     EMPTY, 0))
	played_moves++;
      else {
	entry_ok = 0;
	break;
      }
    }

    if (!entry_ok 
	|| !verify_stored_board(persistent_reading_cache[k].board)) {
      /* Move the last entry in the cache here and back up the loop
       * counter to redo the test at this position in the cache.
       */
      if (0)
	gprintf("Purging entry %d from cache.\n", k);
      if (k < persistent_reading_cache_size - 1)
	persistent_reading_cache[k] 
	  = persistent_reading_cache[persistent_reading_cache_size - 1];
      k--;
      persistent_reading_cache_size--;
    }

    while (played_moves > 0) {
      popgo();
      played_moves--;
    }
  }
}


/* Look for a valid read result in the persistent cache. */
static int
search_persistent_reading_cache(int routine, int str, int *result, int *move)
{
  int k;
  int r;

  for (k = 0; k < persistent_reading_cache_size; k++) {
    /* Check that everything matches. */
    struct reading_cache *entry = &(persistent_reading_cache[k]);
    int apos = 0;
    if (entry->routine != routine
	|| entry->str != str
	|| entry->remaining_depth < (depth - stackp))
      continue;
    
    for (r = 0; r < MAX_CACHE_DEPTH; r++) {
      apos = entry->stack[r];
      if (apos == 0
	  || (entry->board[apos] != GRAY
	      && board[apos] != entry->board[apos]))
	break;
    }

    if (r < MAX_CACHE_DEPTH && apos != 0)
      continue;

    if (!verify_stored_board(entry->board))
      continue;

    /* Matched alright. Increase score, fill in the answer, and return. */
    entry->score += entry->nodes;
    *result = entry->result;
    if (move)
      *move = entry->move;
    ASSERT1(entry->result == 0 || (entry->move == 0) || ON_BOARD(entry->move),
	    entry->move);

    if ((debug & DEBUG_READING_PERFORMANCE)
	&& entry->nodes >= MIN_NODES_TO_REPORT) {
      if (entry->result != 0)
	gprintf("%o%s %1m = %d %1m, cached (%d nodes) ",
		routine == ATTACK ? "attack" : "defend",
		str, entry->result, entry->move, entry->nodes);
      else 
	gprintf("%o%s %1m = %d, cached (%d nodes) ",
		routine == ATTACK ? "attack" : "defend",
		str, entry->result, entry->nodes);
      dump_stack();
    }

    if (0) {
      /* Check that the cached value is correct. */
      int result2;
      int move2;

      if (routine == ATTACK) 
	result2 = do_attack(str, &move2, EMPTY, 0);
      else
	result2 = do_find_defense(str, &move2, EMPTY, 0);
      
      if (result2 != entry->result ||
	  (result2 != 0 && (move2 != entry->move))) {
	gprintf("%oIncorrect cached result %d %1m, should be %d %1m\n",
		entry->result, entry->move, result2, move2);
	print_persistent_reading_cache_entry(k);
	dump_stack();
	showboard(0);
      }
    }

    return 1;
  }

  return 0;
}


/* Store a new read result in the persistent cache. */
static void
store_persistent_reading_cache(int routine, int str, int result, int move,
			       int nodes)
{
  char active[BOARDMAX];
  int k;
  int r;
  int score = nodes;
  struct reading_cache *entry;

  ASSERT1(result == 0 || (move == 0) || ON_BOARD(move), move);

  /* Never cache results at too great depth. */
  if (stackp > MAX_CACHE_DEPTH)
    return;

  /* If cache is still full, consider kicking out an old entry. */
  if (persistent_reading_cache_size == MAX_READING_CACHE_SIZE) {
    int worst_entry = -1;
    int worst_score = score;
    
    for (k = 1; k < persistent_reading_cache_size; k++) {
      if (persistent_reading_cache[k].score < worst_score) {
	worst_score = persistent_reading_cache[k].score;
	worst_entry = k;
      }
    }

    if (worst_entry != -1) {
      /* Move the last entry in the cache here to make space.
       */
      if (worst_entry < persistent_reading_cache_size - 1)
	persistent_reading_cache[worst_entry] 
	  = persistent_reading_cache[persistent_reading_cache_size - 1];
      persistent_reading_cache_size--;
    }
    else
      return;
  }

  entry = &(persistent_reading_cache[persistent_reading_cache_size]);
  entry->movenum = movenum;
  entry->nodes   = nodes;
  entry->score   = score;
  entry->remaining_depth = depth - stackp;
  entry->routine = routine;
  entry->str	 = str;
  entry->result  = result;
  entry->move	 = move;

  for (r = 0; r < MAX_CACHE_DEPTH; r++) {
    if (r < stackp)
      get_move_from_stack(r, &(entry->stack[r]), &(entry->move_color[r]));
    else {
      entry->stack[r] = 0;
      entry->move_color[r] = EMPTY;
    }
  }
  
  /* Remains to set the board. We let the active area be the contested
   * string and reading shadow + adjacent empty and strings +
   * neighbors of active area so far + one more expansion from empty
   * to empty.
   */
  for (k = BOARDMIN; k < BOARDMAX; k++)
    active[k] = shadow[k];

  mark_string(str, active, 1);

  /* To be safe, also add the successful move. */
  if (result != 0 && move != 0)
    active[move] = 1;

  /* Add adjacent strings and empty. */
  for (k = BOARDMIN; k < BOARDMAX; k++) {
    if (!ON_BOARD(k))
      continue;
    if (active[k] != 0) 
      continue;
    if ((ON_BOARD(SOUTH(k)) && active[SOUTH(k)] == 1)
	|| (ON_BOARD(WEST(k)) && active[WEST(k)] == 1)
	|| (ON_BOARD(NORTH(k)) && active[NORTH(k)] == 1)
	|| (ON_BOARD(EAST(k)) && active[EAST(k)] == 1)) {
      if (board[k] != EMPTY)
	mark_string(k, active, 2);
      else
	active[k] = 2;
    }
  }

  /* Remove invincible strings. No point adding their liberties and
   * neighbors.
   */
  for (k = BOARDMIN; k < BOARDMAX; k++) {
    if (!ON_BOARD(k))
      continue;
    if (board[k] != EMPTY && worm[I(k)][J(k)].invincible)
      active[k] = 0;
  }
  
  /* Expand empty to empty. */
  for (k = BOARDMIN; k < BOARDMAX; k++) {
    if (board[k] != EMPTY || active[k] != 0) 
      continue;
    if ((board[SOUTH(k)] == EMPTY && active[SOUTH(k)] == 2)
	|| (board[WEST(k)] == EMPTY && active[WEST(k)] == 2)
	|| (board[NORTH(k)] == EMPTY && active[NORTH(k)] == 2)
	|| (board[EAST(k)] == EMPTY && active[EAST(k)] == 2))
      active[k] = 3;
  }
  
  /* Add neighbors of active area so far. 
   * By using -1 as mark in this step, the test for old marks is
   * simplified.
   */
  for (k = BOARDMIN; k < BOARDMAX; k++) {
    if (!ON_BOARD(k))
      continue;
    if (active[k] != 0) 
      continue;
    if ((ON_BOARD(SOUTH(k)) && active[SOUTH(k)] > 0)
	|| (ON_BOARD(WEST(k)) && active[WEST(k)] > 0)
	|| (ON_BOARD(NORTH(k)) && active[NORTH(k)] > 0)
	|| (ON_BOARD(EAST(k)) && active[EAST(k)] > 0))
      active[k] = -1;
  }

  /* Also add the previously played stones to the active area. */
  for (r = 0; r < stackp; r++)
    active[entry->stack[r]] = 4;

  for (k = BOARDMIN; k < BOARDMAX; k++) {
    if (!ON_BOARD(k))
      continue;
    entry->board[k] = 
      active[k] != 0 ? board[k] : GRAY;
  }

  if (0) {
    gprintf("%o Stored result in cache (entry %d):\n",
	    persistent_reading_cache_size);
    print_persistent_reading_cache_entry(persistent_reading_cache_size);
    gprintf("%o Reading shadow was:\n");
    draw_reading_shadow();
  }
  
  persistent_reading_cache_size++;
}


/* For debugging purposes. */
static void
print_persistent_reading_cache_entry(int k)
{
  struct reading_cache *entry = &(persistent_reading_cache[k]);
  int r;
  gprintf("%omovenum         = %d\n",  entry->movenum);
  gprintf("%onodes           = %d\n",  entry->nodes);
  gprintf("%oscore           = %d\n",  entry->score);
  gprintf("%oremaining_depth = %d\n",  entry->remaining_depth);
  gprintf("%oroutine         = %d\n",  entry->routine);
  gprintf("%ostr             = %1m\n", entry->str);
  gprintf("%oresult          = %d\n",  entry->result);
  gprintf("%omove            = %1m\n", entry->move);
  
  for (r = 0; r < MAX_CACHE_DEPTH; r++) {
    if (entry->stack[r] == 0)
      break;
    gprintf("%ostack[%d]      = %C %1m\n", r, entry->move_color[r],
	    entry->stack[r]);
  }

  draw_active_area(entry->board);
}


/* Helper for the reading_hotspots() function below. */
static void
mark_string_hotspot_values(float values[MAX_BOARD][MAX_BOARD],
			   int m, int n, float contribution)
{
  int i, j, k;
  
  /* If p[m][n] is EMPTY, we just give the contribution to close empty
   * vertices. This is a rough simplification.
   */
  if (BOARD(m, n) == EMPTY) {
    for (i = -1; i <= 1; i++)
      for (j = -1; j <= 1; j++)
	if (ON_BOARD2(m+i, n+j) && BOARD(m+i, n+j) == EMPTY)
	  values[m+i][n+j] += contribution;
    return;
  }
  
  /* Otherwise we give contribution to liberties and diagonal
   * neighbors of the string at (m, n).
   */
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++) {
      if (BOARD(i, j) != EMPTY)
	continue;
      for (k = 0; k < 8; k++) {
	int di = deltai[k];
	int dj = deltaj[k];
	if (ON_BOARD2(i+di, j+dj)
	    && BOARD(i+di, j+dj) != EMPTY
	    && same_string2(i+di, j+dj, m, n)) {
	  if (k < 4) {
	    values[i][j] += contribution;
	    break;
	  }
	  else {
	    if (BOARD(i+di, j) == EMPTY || countlib2(i+di, j) <= 2
		|| BOARD(i, j+dj) == EMPTY || countlib2(i, j+dj) <= 2)
	      values[i][j] += 1.0 * contribution;
	    break;
	  }
	}
      }
    }
}
  

/* Based on the entries in the reading cache and their nodes field,
 * compute where the relatively most expensive tactical reading is
 * going on.
 */
void
reading_hotspots(float values[MAX_BOARD][MAX_BOARD])
{
  int m, n, k;
  int sum_nodes = 0;

  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++)
      values[m][n] = 0.0;
  
  /* Compute the total number of nodes for the cached entries. */
  for (k = 0; k < persistent_reading_cache_size; k++)
    sum_nodes += persistent_reading_cache[k].nodes;

  if (sum_nodes <= 100)
    return;

  /* Loop over all entries and increase the value of vertices adjacent
   * to dragons involving expensive tactical reading.
   */
  for (k = 0; k < persistent_reading_cache_size; k++) {
    struct reading_cache *entry = &(persistent_reading_cache[k]);
    float contribution = entry->nodes / (float) sum_nodes;
    if (0) {
      gprintf("Reading hotspots: %d %1m %f\n", entry->routine, entry->str,
	      contribution);
    }
    switch (entry->routine) {
    case ATTACK:
    case FIND_DEFENSE:
      mark_string_hotspot_values(values, I(entry->str), J(entry->str),
				 contribution);
      break;
    default:
      gg_assert(0); /* Shouldn't happen. */
      break;
    }
  }
}


/* ================================================================ */
/*              Experimental code, not currently in use.            */
/* ================================================================ */


/* naive_ladder(str, &move) tries to capture a string (str)
 * with exactly two liberties under simplified assumptions, which are
 * adequate in a ladder. The rules are as follows:
 *
 * 1. The attacker is allowed to play at each of the two liberties.
 *    If the move was legal, the string now has exactly one
 *    liberty.
 * 2. Define the last stone of the string to be the stone of the
 *    string adjacent to the last liberty. The defender is allowed to
 *    try the following moves:
 *    - extend the string by playing on the liberty
 *    - try to capture the last stone played by the attacker
 *    - try to capture any stone that was put into atari by the
 *      previous move. It is conjectured that it's sufficient to look
 *      for such stones at a distance two from the last liberty.
 *    We only consider captures that can be done immediately.
 * 3. Depending on the resulting number of liberties of the string, we
 *    value each node as follows:
 *    3 or more liberties: the attack has failed
 *    2 liberties:         recurse
 *    1 liberty:           the attack has succeeded
 *    illegal move for the defender: successful attack
 *    illegal move for the attacker: failed attack
 *
 * Return codes are as usual 0 for failure and 1 for success. If the
 * attack was successful, (*move) contains the attacking move, unless
 * it is a null pointer.
 *
 * The differences compared to the attack2()/defend1() combination for
 * reading ladders is that this one really always reads them to the
 * very end and that it is faster (because it doesn't call
 * break_chain()). In contrast to attack2() though, this function can
 * only be used for capturing in ladders.
 *
 * FIXME: The ladder capture may depend on ko. Need to add the ko
 *        return codes.
 */

static int
naive_ladder(int str, int *move)
{
  int color = board[str];
  int other = OTHER_COLOR(color);
  int apos, bpos;
  int acount = 0, bcount = 0;
  int liberties;
  int libs[2];
  
  ASSERT1(board[str] != EMPTY, str);
  ASSERT1(countlib(str) == 2, str);
  DEBUG(DEBUG_READING, "naive_ladder(%1m)\n", str);

  RTRACE("checking ladder attack on %1m with 2 liberties\n", str);

  /* Get the two liberties of (str) into (apos) and (bpos). */ 
  liberties = findlib(str, 2, libs);
  ASSERT1(liberties == 2, str);
  apos = libs[0];
  bpos = libs[1];

  /* if (bpos) looks more promising we wish to switch the two liberties.
   * We check whether (bpos) is adjacent to more open liberties than
   * (apos).
   *
   * FIXME: Use order_moves() instead.
   */

  if (board[SOUTH(apos)] == EMPTY)
    acount++;
  if (board[WEST(apos)] == EMPTY)
    acount++;
  if (board[NORTH(apos)] == EMPTY)
    acount++;
  if (board[EAST(apos)] == EMPTY)
    acount++;
  if (board[SOUTH(bpos)] == EMPTY)
    bcount++;
  if (board[WEST(bpos)] == EMPTY)
    bcount++;
  if (board[NORTH(bpos)] == EMPTY)
    bcount++;
  if (board[EAST(bpos)] == EMPTY)
    bcount++;

  if (bcount > acount) {
    apos = libs[1];
    bpos = libs[0];
  }

  RTRACE("considering atari at %1m\n", apos);
  
  if (trymove(apos, other, "naive_ladder-A", str, EMPTY, 0)) {
    if (!naive_ladder_defense(str, apos, bpos, color, other)) {
      popgo();
      if (move)
	*move = apos;
      return WIN;
    }
    popgo();
  }
	  
  if (trymove(bpos, other, "naive_ladder-B", str, EMPTY, 0)) {
    if (!naive_ladder_defense(str, bpos, apos, color, other)) {
      popgo();
      if (move)
	*move = bpos;
      return WIN;
    }
    popgo();
  }

  /* Neither move worked. */
  return 0;
}


/* Try to save the one-liberty string (str) from being caught in a
 * ladder. (apos) is the last played attacking stone and (bpos) is
 * the last remaining liberty.
 */

static int
naive_ladder_defense(int str, int apos, int bpos, int color, int other)
{
  int liberties;
  
  /* Try to capture the just played stone. */
  if (naive_ladder_break_through(str, apos, color, other))
    return WIN;

  /* Try to run away by extending on the last liberty. */
  if (trymove(bpos, color, "naive_ladder_defense", str, EMPTY, 0)) {
    liberties = countlib(str);
    if (liberties >= 3
	|| (liberties == 2
	    && !naive_ladder(str, NULL))) {
      popgo();
      return WIN;
    }
    popgo();
  }
  
  /* Try to capture a string at distance two (Manhattan metric) from
   * the last liberty.
   */
  if (naive_ladder_break_through(str, SW(bpos), color, other))
    return WIN;

  if (naive_ladder_break_through(str, NW(bpos), color, other))
    return WIN;

  if (naive_ladder_break_through(str, NE(bpos), color, other))
    return WIN;

  if (naive_ladder_break_through(str, SE(bpos), color, other))
    return WIN;

  if (naive_ladder_break_through(str, SS(bpos), color, other))
    return WIN;

  if (naive_ladder_break_through(str, WW(bpos), color, other))
    return WIN;

  if (naive_ladder_break_through(str, NN(bpos), color, other))
    return WIN;

  if (naive_ladder_break_through(str, EE(bpos), color, other))
    return WIN;

  /* Nothing worked. */
  return 0;
}


/* Try to break out of the ladder by capturing (apos). We must first
 * verify that there is an opponent stone there and that it is in
 * atari so we can capture it immediately. After the capture we count
 * liberties for (str) to see if the ladder is decided yet.
 */
static int
naive_ladder_break_through(int str, int apos, int color, int other)
{
  int liberties;
  int bpos;
  
  if (board[apos] != other)
    return 0;
  
  if (findlib(apos, 1, &bpos) != 1)
    return 0;

  if (trymove(bpos, color, "naive_ladder_break_through", str, EMPTY, 0)) {
    liberties = countlib(str);
    if (liberties >= 3
	|| (liberties == 2
	    && !naive_ladder(str, NULL))) {
      popgo();
      return WIN;
    }
    popgo();
  }
  
  return 0;
}




/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
