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

#define ADD_CANDIDATE_MOVE(ti, tj, score, movei, movej, scores, moves)\
  do {\
    int l;\
    for (l = 0; l < moves; l++)\
      if (movei[l] == (ti) && movej[l] == (tj)) {\
        if (scores[l] < score)\
          scores[l] = score;\
	break;\
      }\
    if ((l == moves) && (moves < MAX_MOVES)) {\
      movei[moves] = ti;\
      movej[moves] = tj;\
      scores[moves] = score;\
      (moves)++;\
    }\
  } while(0) \

#define REMOVE_CANDIDATE_MOVE(ti, tj, movei, movej, scores, moves)\
  do {\
    int k, l;\
    for (k = 0; k < moves; k++) {\
      if (movei[k] == (ti) && movej[k] == (tj)) {\
        for (l = k; l < moves-1; l++) {\
	  movei[l] = movei[l+1];\
	  movej[l] = movej[l+1];\
	  scores[l] = scores[l+1];\
	}\
        (moves)--;\
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

static int do_find_defense(int si, int sj, int *i, int *j, 
			   int komaster, int kom_i, int kom_j);
static int defend1(int si, int sj, int *i, int *j, 
		   int komaster, int kom_i, int kom_j);
static int restricted_defend1(int si, int sj, int *i, int *j, 
			      int komaster, int kom_i, int kom_j,
			      int forbidden_moves, 
			      int *forbidden_movei, int *forbidden_movej);
static int defend2(int si, int sj, int *i, int *j, 
		   int komaster, int kom_i, int kom_j);
static int defend3(int si, int sj, int *i, int *j, 
		   int komaster, int kom_i, int kom_j);
static int defend4(int si, int sj, int *i, int *j, 
		   int komaster, int kom_i, int kom_j);
static int special_rescue(int si, int sj, int ai, int aj, 
			  int *ti, int *tj, 
			  int komaster, int kom_i, int kom_j);
static int special_rescue2(int si, int sj, int libi[2], int libj[2], 
			   int *ti, int *tj, 
			   int komaster, int kom_i, int kom_j);
static int special_rescue3(int si, int sj, int libi[3], int libj[3],
			   int *ti, int *tj, 
			   int komaster, int kom_i, int kom_j);
static int special_rescue4(int si, int sj, int libi[3], int libj[3],
			   int *ti, int *tj, 
			   int komaster, int kom_i, int kom_j);
static void edge_clamp(int si, int sj,
		       int movei[MAX_MOVES], int movej[MAX_MOVES],
		       int scores[MAX_MOVES], int *moves);
static int do_attack(int si, int sj, int *i, int *j, 
		     int komaster, int kom_i, int kom_j);
static int attack1(int si, int sj, int *i, int *j, 
		   int komaster, int kom_i, int kom_j);
static int attack2(int si, int sj, int *i, int *j, 
		   int komaster, int kom_i, int kom_j);
static int restricted_attack2(int si, int sj, int *i, int *j, 
			      int komaster, int kom_i, int kom_j,
			      int forbidden_moves, 
			      int *forbidden_movei, int *forbidden_movej);
static int attack3(int si, int sj, int *i, int *j, 
		   int komaster, int kom_i, int kom_j);
static int attack4(int si, int sj, int *i, int *j, 
		   int komaster, int kom_i, int kom_j);
static int find_cap2(int si, int sj, int ai, int aj, int bi, int bj,
		     int *i, int *j, 
		     int komaster, int kom_i, int kom_j);
static int find_cap3(int si, int sj, int *i, int *j, 
		     int komaster, int kom_i, int kom_j);
static int special_attack2(int si, int sj, int libi[2], int libj[2], 
			   int *ti, int *tj, 
			   int komaster, int kom_i, int kom_j);
static int special_attack3(int si, int sj, int libi[2], int libj[2], 
			   int *ti, int *tj, 
			   int komaster, int kom_i, int kom_j);
static int special_attack4(int si, int sj, int libi[2], int libj[2],
			   int *ti, int *tj, 
			   int komaster, int kom_i, int kom_j);
static int draw_back(int si, int sj, int *ti, int *tj, 
		     int komaster, int kom_i, int kom_j);
static int edge_closing_backfill(int si, int sj, int ai, int aj,
				 int *ti, int *tj);
static void edge_block(int si, int sj, int ai, int aj,
		       int movei[MAX_MOVES], int movej[MAX_MOVES],
		       int scores[MAX_MOVES], int *moves);
static void propose_edge_moves(int si, int sj, int *libi, int *libj, int libs,
			       int movei[MAX_MOVES], int movej[MAX_MOVES],
			       int scores[MAX_MOVES], int *moves, int color);
static void break_chain_moves(int si, int sj,
			      int movei[MAX_MOVES], int movej[MAX_MOVES],
			      int scores[MAX_MOVES], int *moves);
static void break_chain2_efficient_moves(int si, int sj,
					 int movei[MAX_MOVES],
					 int movej[MAX_MOVES],
					 int scores[MAX_MOVES],
					 int *moves);
static void break_chain2_moves(int si, int sj,
			       int movei[MAX_MOVES], int movej[MAX_MOVES],
			       int scores[MAX_MOVES], int *moves,
			       int require_safe);
static int break_chain2(int si, int sj, int *i, int *j, 
			int komaster, int kom_i, int kom_j);
static int break_chain3(int si, int sj, int *i, int *j, 
			int komaster, int kom_i, int kom_j);
static int superstring_breakchain(int si, int sj, int *i, int *j,
				  int komaster, int kom_i, int kom_j,
				  int liberty_cap);
static void double_atari_chain2(int si, int sj,
				int movei[MAX_MOVES], int movej[MAX_MOVES],
				int scores[MAX_MOVES], int *moves);
static void order_moves(int si, int sj, int num_moves, int *movei, int *movej,
			int *scores, int color, const char *funcname);
static int naive_ladder(int si, int sj, int *i, int *j);
static int naive_ladder_defense(int si, int sj, int ai, int aj,
				int bi, int bj, int color, int other);
static int naive_ladder_break_through(int si, int sj, int ai, int aj,
				      int color, int other);
static int in_list(int m, int n, int moves, int *movei, int *movej);


/* Persistent reading cache to reuse read results between moves and
 * within the same move when one or more far away moves have been
 * played.
 */

#define MAX_CACHE_DEPTH 5

struct reading_cache {
  char board[MAX_BOARD][MAX_BOARD];
  int movenum;
  int nodes;
  int score;
  int remaining_depth;
  int routine; /* ATTACK or FIND_DEFENSE */
  int si, sj;  /* contested string (origin) */
  int result;
  int i, j;    /* attack/defense point */
  int stacki[MAX_CACHE_DEPTH];
  int stackj[MAX_CACHE_DEPTH];
  int move_color[MAX_CACHE_DEPTH];
};

#define MAX_READING_CACHE_SIZE 100
static struct reading_cache persistent_reading_cache[MAX_READING_CACHE_SIZE];
static int persistent_reading_cache_size = 0;

static void draw_active_area(char board[MAX_BOARD][MAX_BOARD]);
static int verify_stored_board(char board[MAX_BOARD][MAX_BOARD]);
static int search_persistent_reading_cache(int routine, int si, int sj,
					   int *result, int *i, int *j);
static void store_persistent_reading_cache(int routine, int si, int sj,
					   int result, int i, int j,
					   int nodes);
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


/* attack(si, sj, *i, *j) determines if the string at (m, n) can be 
 * captured, and if so, (*i, *j) returns the attacking move, unless
 * (*i, *j) are null pointers. Use null pointers if you are interested
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
attack(int si, int sj, int *i, int *j)
{
  int nodes_when_called = reading_node_counter;
  int result;
  int nodes;
  int oi, oj;
  int temp_i, temp_j;

  /* Don't even spend time looking in the cache if there are more than
   * four liberties.
   */
  if (countlib(si, sj) > 4)
    return 0;

  find_origin(si, sj, &oi, &oj);
  if (search_persistent_reading_cache(ATTACK, oi, oj, &result, i, j))
    return result;

  memset(shadow, 0, sizeof(shadow));
  result = do_attack(si, sj, &temp_i, &temp_j, EMPTY, -1, -1);
  nodes = reading_node_counter - nodes_when_called;

  if (debug & DEBUG_READING_PERFORMANCE) {
    if (reading_node_counter - nodes_when_called >= MIN_NODES_TO_REPORT) {
      if (result != 0)
	gprintf("%oattack %m(%m) = %d %M, %d nodes ", si, sj, oi, oj, result,
		temp_i, temp_j, nodes);
      else
	gprintf("%oattack %m(%m) = %d, %d nodes ", si, sj, oi, oj, result,
		nodes);
      dump_stack();
    }
  }

  store_persistent_reading_cache(ATTACK, oi, oj, result,
				 temp_i, temp_j, nodes);
  
  if (i) *i = temp_i;
  if (j) *j = temp_j;
  return result;
}


/* find_defense(m, n, *i, *j) attempts to find a move that will save
 * the string at (m,n). It returns WIN if such a move is found, with
 * (*i, *j) the location of the saving move, unless (*i, *j) are
 * null pointers. It is not checked that tenuki defends, so this may 
 * give an erroneous answer if !attack(m,n).
 * 
 * Returns KO_A or KO_B if the result depends on ko. Returns KO_A if the
 * string can be defended provided the defender is willing to ignore
 * any ko threat. Returns KO_B if the defender wins by having a ko threat
 * which must be answered.
 */

int 
find_defense(int si, int sj, int *i, int *j)
{
  int nodes_when_called = reading_node_counter;
  int result;
  int nodes;
  int oi, oj;
  int temp_i, temp_j;

  /* Don't even spend time looking in the cache if there are more than
   * four liberties.
   */
  if (countlib(si, sj) > 4) {
    if (i) *i = -1;
    if (j) *j = -1;
    return WIN;
  }

  find_origin(si, sj, &oi, &oj);
  if (search_persistent_reading_cache(FIND_DEFENSE, oi, oj, &result, i, j))
    return result;

  memset(shadow, 0, sizeof(shadow));
  result = do_find_defense(si, sj, &temp_i, &temp_j, EMPTY, -1, -1);
  nodes = reading_node_counter - nodes_when_called;

  if (debug & DEBUG_READING_PERFORMANCE) {
    if (reading_node_counter - nodes_when_called >= MIN_NODES_TO_REPORT) {
      if (result != 0)
	gprintf("%odefend %m(%m) = %d %M, %d nodes ", si, sj, oi, oj, result,
		temp_i, temp_j, nodes);
      else
	gprintf("%odefend %m(%m) = %d, %d nodes ", si, sj, oi, oj, result,
		nodes);
      dump_stack();
    }
  }

  store_persistent_reading_cache(FIND_DEFENSE, oi, oj, result,
				 temp_i, temp_j, nodes);
  
  if (i) *i = temp_i;
  if (j) *j = temp_j;
  return result;
}

/* attack_and_defend(si, sj, &acode, &ai, &aj, &dcode, &di, &dj) is a
 * frontend to the attack() and find_defense() functions, which
 * guarantees a consistent result. If a string cannot be attacked, 0
 * is returned and acode is 0. If a string can be attacked and
 * defended, WIN is returned, acode and dcode are both non-zero, and
 * (ai, aj), (di, dj) both point to vertices on the board. If a string
 * can be attacked but not defended, 0 is again returned, acode is
 * non-zero, dcode is 0, and (ai, aj) point to a vertex on the board.
 *
 * This function in particular guarantees that if there is an attack,
 * it will never return (di, dj) = (-1, -1), which means the string is
 * safe without defense. Separate calls to attack() and find_defense()
 * may occasionally give this result, due to irregularities introduced
 * by the persistent reading cache.
 */
int attack_and_defend(int si, int sj,
		      int *attack_code, int *attacki, int *attackj,
		      int *defend_code, int *defendi, int *defendj)
{
  int acode = 0;
  int ai = -1, aj = -1;
  int dcode = 0;
  int di = -1, dj = -1;

  acode = attack(si, sj, &ai, &aj);
  if (acode != 0) {
    dcode = find_defense(si, sj, &di, &dj);
    
    /* If find_defense() says the string is safe as is, we believe
     * this in favor of attack()'s opinion. Actually this is probably
     * incorrect, but we can't easily find a defense point to return.
     */
    if (dcode == WIN && di == -1) {
      acode = 0;
      ai = -1;
      aj = -1;
    }
  }

  if (attack_code) *attack_code = acode;
  if (attacki) *attacki = ai;
  if (attackj) *attackj = aj;
  if (defend_code) *defend_code = dcode;
  if (defendi) *defendi = di;
  if (defendj) *defendj = dj;

  return acode != 0 && dcode != 0;
}


/*
 * attack_either(ai, aj, bi, bj) returns true if there is a move which
 * guarantees that at least one of the strings (ai, aj) and (bi, bj)
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
attack_either(int ai, int aj, int bi, int bj)
{
  int color = p[ai][aj];
  ASSERT(color != EMPTY , ai, aj);
  ASSERT(color == p[bi][bj], bi, bj);

  /* Start by attacking the string with the fewest liberties. On
   * average this seems to be slightly more efficient.
   */
  if (countlib(ai, aj) <= countlib(bi, bj))
    return attack(ai, aj, NULL, NULL) || attack(bi, bj, NULL, NULL);
  else
    return attack(bi, bj, NULL, NULL) || attack(ai, aj, NULL, NULL);
}


/*
 * defend_both(ai, aj, bi, bj) returns true if both the strings (ai, aj)
 * and (bi, bj) can be defended simultaneously or if there is no attack.
 * A typical application for this is in connection patterns, where
 * after a cut it's necessary to defend both cutting stones.
 *
 * FIXME: The current implementation only makes halfhearted
 * attempts to find coordinated defense moves. A proper implementation
 * would require some serious reading.
 */

int
defend_both(int ai, int aj, int bi, int bj)
{
  int a_threatened = 0;
  int b_threatened = 0;
  int ci, cj, di, dj;
  int acode = 0;
  int dcode = 0;
  
  int color = p[ai][aj];
  ASSERT(color != EMPTY , ai, aj);
  ASSERT(color == p[bi][bj], bi, bj);

  attack_and_defend(ai, aj, &acode, NULL, NULL, &dcode, &ci, &cj);
  if (acode != 0) {
    a_threatened = 1;
    if (dcode == 0)
      return 0; /* (ai, aj) already lost */
  }
  
  attack_and_defend(bi, bj, &acode, NULL, NULL, &dcode, &di, &dj);
  if (acode != 0) {
    b_threatened = 1;
    if (dcode == 0)
      return 0; /* (bi, bj) already lost */
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

  if (ci == di && cj == dj)
    return WIN; /* Both strings can be attacked but also defended 
                 * by one move. */

  /* We also try each of the returned defense points and see whether
   * the other string can still be attacked. This still gives a
   * somewhat pessimistic estimation.
   */

  if (trymove(ci, cj, color, "defend_both-A", ai, aj, EMPTY, -1, -1)) {
    if (p[bi][bj] && !attack(bi, bj, NULL, NULL)) {
      popgo();
      return WIN;
    }
    popgo();
  }
  
  if (trymove(di, dj, color, "defend_both-B", bi, bj, EMPTY, -1, -1)) {
    if (p[ai][aj] && !attack(ai, aj, NULL, NULL)) {
      popgo();
      return WIN;
    }
    popgo();
  }

  /* The next improvement is to try to attack a common adjacent string. */
  {
    int adj1i[MAXCHAIN];
    int adj1j[MAXCHAIN];
    int neighbors1;
    int adj2i[MAXCHAIN];
    int adj2j[MAXCHAIN];
    int neighbors2;
    int r;
    int s;
    int ei, ej;
    int fi, fj;
    
    neighbors1 = chainlinks(ai, aj, adj1i, adj1j);
    neighbors2 = chainlinks(bi, bj, adj2i, adj2j);
    
    for (r = 0; r < neighbors1; r++) {
      ei = adj1i[r];
      ej = adj1j[r];
      if (countlib(ei, ej) <= 4
	  && (ei != ci || ej != cj)
	  && (ei != di || ej != dj)) {
	/* Is (ei, ej) also adjacent to (bi, bj)? */
	for (s = 0; s < neighbors2; s++) {
	  if (adj2i[s] == ei && adj2j[s] == ej)
	    break;
	}
	if (s == neighbors2)
	  continue;   /* No, it wasn't. */

	if (attack(ei, ej, &fi, &fj)) {
	  if (trymove(fi, fj, color, "defend_both-C", ai, aj, EMPTY, -1, -1)) {
	    if (p[ai][aj] && p[bi][bj]
		&& !attack(ai, aj, NULL, NULL) 
		&& !attack(bi, bj, NULL, NULL)) {
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
 * break_through(ai, aj, bi, bj, ci, cj) returns WIN if a position can
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
break_through_helper(int ai, int aj, int bi, int bj, int ci, int cj,
		     int di, int dj, int ei, int ej, int Fi, int Fj,
		     int color, int other);

int
break_through(int ai, int aj, int bi, int bj, int ci, int cj)
{
  int di, dj;
  int ei, ej;
  int Fi, Fj;
  int gi, gj;
  
  int color = p[ai][aj];
  int other = OTHER_COLOR(color);

  int success = 0;
  int success2 = 0;
  
  /* Basic sanity checking. */
  ASSERT(color != EMPTY , ai, aj);
  ASSERT(color == p[bi][bj], bi, bj);
  ASSERT(color == p[ci][cj], ci, cj);

  /* Construct the rest of the points in the pattern. */
  Fi = (ai + ci) / 2;    /* F midpoint between a and c. */
  Fj = (aj + cj) / 2;
  di = ai + bi - Fi;     /* Use diagonal relation a+b = d+F. */
  dj = aj + bj - Fj;
  ei = bi + ci - Fi;     /* Use diagonal relation b+c = e+F. */
  ej = bj + cj - Fj;

  /* More sanity checking. */
  ASSERT(p[di][dj] == EMPTY , di, dj);
  ASSERT(p[ei][ej] == EMPTY , ei, ej);

  /* F might already have been captured. (play_break_through_n() can't
   * detect this.
   */
  if (p[Fi][Fj] == EMPTY)
    return 0;
  
  ASSERT(p[Fi][Fj] == other , Fi, Fj);

  /* First X tries to play at d. */
  success = break_through_helper(ai, aj, bi, bj, ci, cj,
				 di, dj, ei, ej, Fi, Fj, color, other);
  if (success == WIN)
    return WIN;
  
  success2 = break_through_helper(ci, cj, bi, bj, ai, aj,
				  ei, ej, di, dj, Fi, Fj, color, other);

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
  if (attack_and_defend(Fi, Fj, NULL, NULL, NULL, NULL, &gi, &gj)) {
    if (trymove(gi, gj, other, "break_through-A", Fi, Fj, EMPTY, -1, -1)) {
      /* Now we let O defend his position by playing either d or e.
       * FIXME: There may be other plausible moves too.
       */
      if (trymove(di, dj, color, "break_through-B", Fi, Fj, EMPTY, -1, -1)) {
	/* O connects at d, so X cuts at e. */
	if (safe_move(ei, ej, other)) {
	  success2 = CUT;
	  if (!p[ci][cj] || attack(ci, cj, NULL, NULL))
	    success2 = WIN;
	}
	popgo();
      }

      if (success2 > 0 && trymove(ei, ej, color, "break_through-C", Fi, Fj,
				  EMPTY, -1, -1)) {
	/* O connects at e, so X cuts at d. */
	if (safe_move(di, dj, other)) {
	  /* success2 is already WIN or CUT. */
	  if (p[ai][aj] && !attack(ai, aj, NULL, NULL))
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
break_through_helper(int ai, int aj, int bi, int bj, int ci, int cj,
		     int di, int dj, int ei, int ej, int Fi, int Fj,
		     int color, int other)
{
  int success = 0;
  int gi, gj;

  if (trymove(di, dj, other, "break_through_helper-A", Fi, Fj, 
	      EMPTY, -1, -1)) {
    /* If F can be attacked we can't start in this way. */
    if (!attack(Fi, Fj, NULL, NULL)) {
      /* If d is safe too, we have at least managed to break through. */
      if (!attack(di, dj, &gi, &gj)) {
	success = CUT;
	/* So now we can try to capture something. */
	if (!p[ai][aj] || !p[bi][bj] || !defend_both(ai, aj, bi, bj))
	  success = WIN;
	else {
	  /* Both a and b could be defended, or didn't need to be.
	   * Let's see if a move at e is sufficient for O.
	   */
	  int attack_on_b = 0;
	  int attack_on_a = 0;

	  if (trymove(ei, ej, color, "break_through_helper-B", Fi, Fj,
		      EMPTY, -1, -1)) {
	    if (attack(bi, bj, NULL, NULL))
	      attack_on_b = 1;
	    else if (attack(ai, aj, NULL, NULL))
	      attack_on_a = 1;
	    popgo();
	  }

	  /* Let O find a defense and play it. */
	  if (attack_on_a || attack_on_b) {
	    int hi = -1;
	    int hj = -1;
	    if (((attack_on_a && find_defense(ai, aj, &hi, &hj))
		|| (attack_on_b && find_defense(bi, bj, &hi, &hj)))
		&& hi != -1
		&& trymove(hi, hj, color, "break_through_helper-C", Fi, Fj,
			   EMPTY, -1, -1)) {
	      /* Now we make a second cut at e, trying to capture
	       * either b or c.
	       */
	      if (trymove(ei, ej, other, "break_through_helper-D", Fi, Fj,
			  EMPTY, -1, -1)) {
		if (!p[bi][bj] || !p[ci][cj] || !defend_both(bi, bj, ci, cj))
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
	if (trymove(ei, ej, color, "break_through_helper-E", Fi, Fj,
		    EMPTY, -1, -1)) {
	  if (!p[di][dj] || !find_defense(di, dj, NULL, NULL)) {
	    popgo();
	    popgo();
	    return 0;
	  }
	  popgo();
	}
	
	if (gi == ei && gj == ej) {
	  popgo();
	  return 0;
	}

	if (trymove(gi, gj, color, "break_through_helper-F", Fi, Fj,
		    EMPTY, -1, -1)) {
	  if (trymove(ei, ej, other, "break_through_helper-G", Fi, Fj,
		      EMPTY, -1, -1)) {
	    if (!attack(ei, ej, NULL, NULL)) {
	      success = CUT;
	      if (!p[bi][bj] || !p[ci][cj] || !defend_both(bi, bj, ci, cj))
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

static int aa_status[MAX_BOARD][MAX_BOARD]; /* ALIVE, DEAD or CRITICAL */
static int forbidden[MAX_BOARD][MAX_BOARD];
static int get_aa_status(int m, int n);
static int do_atari_atari(int color, int *attacki, int *attackj,
			  int *defendi, int *defendj, int ci, int cj,
			  int save_verbose, int minsize);

/* Set to 1 if you want verbose traces from this function. */

int
atari_atari(int color, int *i, int *j, int save_verbose)
{
  int m, n;
  int fi, fj;
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
      if (p[m][n] == other) {
	if (dragon[m][n].matcher_status == DEAD)
	  aa_status[m][n] = DEAD;
	else if (worm[m][n].attack_code != 0) {
	  if (worm[m][n].defend_code != 0)
	    aa_status[m][n] = CRITICAL;
	  else
	    aa_status[m][n] = DEAD;

	}
	else
	  aa_status[m][n] = ALIVE;
      }
      else
	aa_status[m][n] = UNKNOWN;
    }

  /* reclassify a worm with 2 liberties as INSUBSTANTIAL if capturing
   * it does not result in a live group.
   */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) 
      if (p[m][n] == other
	  && worm[m][n].origini == m
	  && worm[m][n].originj == n
	  && worm[m][n].liberties == 2
	  && aa_status[m][n] != DEAD
	  && !owl_substantial(m, n)) {
	int ti, tj;
	for (ti = 0; ti < board_size; ti++)
	  for (tj = 0; tj < board_size; tj++)
	    if (is_worm_origin(ti, tj, m, n))
	      aa_status[ti][tj] = INSUBSTANTIAL;
      }

  if (debug & DEBUG_ATARI_ATARI) {
    gprintf("atari_atari for %C\n", color);
    gprintf("aa_status: (ALIVE worms not listed)\n");
    for (m = 0; m < board_size; m++)
      for (n = 0; n < board_size; n++) {
	if (p[m][n] == other && is_worm_origin(m, n, m, n)) {
	  const char *status = "UNKNOWN (shouldn't happen)";
	  if (aa_status[m][n] == DEAD)
	    status = "DEAD";
	  else if (aa_status[m][n] == CRITICAL)
	    status = "CRITICAL";
	  else if (aa_status[m][n] == INSUBSTANTIAL)
	    status = "INSUBSTANTIAL";

	  if (aa_status[m][n] != ALIVE)
	    gprintf("%M: %s\n", m, n, status);
	}
      }
  }

  aa_val = do_atari_atari(color, &fi, &fj, NULL, NULL, -1, -1,
			  save_verbose, 0);

  if (aa_val == 0)
    return 0;

  /* We try excluding the first atari found and see if the
   * combination still works. Repeat until failure.
   */
  while (1) {
    int new_aa_val;
    forbidden[fi][fj] = 1;
    new_aa_val = do_atari_atari(color, &fi, &fj, NULL, NULL, -1, -1,
				save_verbose, aa_val);

    /* The last do_atari_atari call fails. When do_atari_atari fails,
     * it does not change the value of (fi, fj), so these correspond
     * to a move that works and is necessary.
     */
    if (new_aa_val == 0) {
      if (i) *i = fi;
      if (j) *j = fj;
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
get_aa_status(int m, int n)
{
  int stonei[MAX_BOARD * MAX_BOARD];
  int stonej[MAX_BOARD * MAX_BOARD];
  int stones;
  int k;

  if (aa_status[m][n] != UNKNOWN)
    return aa_status[m][n];

  stones = findstones(m, n, MAX_BOARD * MAX_BOARD, stonei, stonej);
  for (k = 0; k < stones; k++)
    if (aa_status[stonei[k]][stonej[k]] != UNKNOWN)
      return aa_status[stonei[k]][stonej[k]];

  return UNKNOWN;
}

/* Helper function for atari_atari. Here worms is the number of
 * opponent worms involved in the combination, and (ci, cj) is
 * the location of the last friendly move played. Moves marked
 * with the forbidden array are not tried. If no move is found,
 * the values of *i and *j are not changed.
 */

static int
do_atari_atari(int color, int *attacki, int *attackj,
	       int *defendi, int *defendj, int ci, int cj,
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
	if (forbidden[m][n])
	  gprintf("%o%m ", m, n);
      }
    gprintf("\n");
  }

  /* First look for strings adjacent to the last friendly move played
   * which can be unexpectedly attacked.
   */
  if (ci != -1)
    for (m = 0; m < board_size; m++)
      for (n = 0; n < board_size; n++) {
	int om, on;
	int ai, aj;

	if (p[m][n] != other)
	  continue;

	find_origin(m, n, &om, &on);
	if (m != om || n != on)
	  continue;

	if (minsize > 0 && countstones(m, n) < minsize)
	  continue;

	if (get_aa_status(m, n) != ALIVE
	    || !neighbor_of_string(ci, cj, m, n))
	  continue;
	
	if (debug & DEBUG_ATARI_ATARI)
	  gprintf("Considering attack of %m. depth = %d.\n", m, n, depth);
	if (attack(m, n, &ai, &aj)) {
	  if (save_verbose || (debug & DEBUG_ATARI_ATARI)) {
	    gprintf("%oThe worm %m can be attacked at %m after ", m, n,
		    ai, aj);
	    dump_stack();
	  }	  
	  if (attacki) *attacki = ai;
	  if (attackj) *attackj = aj;
	  if (defendi)
	    find_defense(m, n, defendi, defendj);

	  DEBUG(DEBUG_ATARI_ATARI, "%oreturn value:%d (%m)\n",
		countstones(m, n), m, n);
	  return countstones(m, n);
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
      int om, on;
      int libi[2], libj[2];
      int status;

      if (p[m][n] != other) 
	continue;

      find_origin(m, n, &om, &on);
      if (m != om || n != on)
	continue;
      
      if (minsize > 0 && countstones(m, n) < minsize)
	continue;

      status = get_aa_status(m, n);
      if (status == INSUBSTANTIAL || status == DEAD || status == UNKNOWN)
	continue;

      if (findlib(m, n, 2, libi, libj) != 2)
	continue;

      for (k = 0; k < 2; k++) {
	int ai = libi[k];
	int aj = libj[k];
	int bi, bj;
	if (!forbidden[ai][aj]
	    && accurate_approxlib(ai, aj, color, 2, NULL, NULL) > 1) {
	  if (trymove(ai, aj, color, "do_atari_atari-A", m, n,
		      EMPTY, -1, -1)) {
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
	    if (find_defense(m, n, &bi, &bj)
		&& trymove(bi, bj, other, "do_atari_atari-B", m, n,
			   EMPTY, -1, -1)) {
	      /* These moves may have been irrelevant for later
               * reading, so in order to avoid horizon problems, we
               * need to temporarily increase the depth values.
	       */
	      increase_depth_values();
	      increase_depth_values();
	      aa_val = do_atari_atari(color, NULL, NULL, defendi, defendj,
				      ai, aj, save_verbose, minsize);
	      decrease_depth_values();
	      decrease_depth_values();
	      popgo();
	    }
	    else {
	      /* No way to save the ataried stone. We have been successful. */
	      popgo();
	      if (save_verbose || (debug & DEBUG_ATARI_ATARI)) {
		gprintf("%oThe worm %m can be attacked at %m after ", m, n,
			ai, aj);
		dump_stack();
	      }	  
	      if (attacki) *attacki = ai;
	      if (attackj) *attackj = aj;
	      if (defendi)
		find_defense(m, n, defendi, defendj);
	      
	      DEBUG(DEBUG_ATARI_ATARI, "%oreturn value:%d (%m)\n",
		    countstones(m, n), m, n);
	      return countstones(m, n);
	    }

	    if (aa_val) {
	      /* The atari at (ai,aj) seems to work but we still
	       * must check there is not a better defense.
	       */
	      if (restricted_defend1(m, n, &ci, &cj, EMPTY, -1, -1,
				     1, &bi, &bj)) {
		if (trymove(ci, cj, other, "do_atari_atari-C", 
			    m, n, EMPTY, -1, -1)) {
		  increase_depth_values();
		  increase_depth_values();
		  if (!do_atari_atari(color, NULL, NULL, defendi, defendj,
				      ai, aj, save_verbose, minsize)) 
		    aa_val = 0;
		  decrease_depth_values();
		  decrease_depth_values();
		  popgo();
		}
	      }
	      if (aa_val) {
		if (attacki) *attacki = ai;
		if (attackj) *attackj = aj;
		popgo();
		DEBUG(DEBUG_ATARI_ATARI, "%oreturn value:%d (min %d, %d (%m))\n",
		      gg_min(aa_val, countstones(m, n)), aa_val,
		      countstones(m, n), m, n);
		return gg_min(aa_val, countstones(m, n));
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
 * (ti, tj). If this happens, (*i, *j) points to a move which prevents
 * this blunder.
 *
 * FIXME: Most of the code below is common with atari_atari() and
 *        should be broken out of both functions.
 */
int
atari_atari_confirm_safety(int color, int ti, int tj, int *i, int *j,
			   int minsize)
{
  int m, n;
  int fi, fj;
  int aa_val;
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
      if (p[m][n] == color) {
	if (dragon[m][n].matcher_status == DEAD)
	  aa_status[m][n] = DEAD;
	else if (worm[m][n].attack_code != 0) {
	  if (worm[m][n].defend_code != 0)
	    aa_status[m][n] = CRITICAL;
	  else
	    aa_status[m][n] = DEAD;
	}
	else
	  aa_status[m][n] = ALIVE;
      }
      else
	aa_status[m][n] = UNKNOWN;
    }
  
  /* reclassify a worm with 2 liberties as INSUBSTANTIAL if capturing
   * it does not result in a live group.
   */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) 
      if (p[m][n] == color
	  && worm[m][n].origini == m
	  && worm[m][n].originj == n
	  && worm[m][n].liberties == 2
	  && aa_status[m][n] != DEAD
	  && !owl_substantial(m, n)) {
	int ti, tj;
	for (ti = 0; ti < board_size; ti++)
	  for (tj = 0; tj < board_size; tj++)
	    if (is_worm_origin(ti, tj, m, n))
	      aa_status[ti][tj] = INSUBSTANTIAL;
      }

  if (debug & DEBUG_ATARI_ATARI) {
    gprintf("atari_atari for %C\n", other);
    gprintf("aa_status: (ALIVE worms not listed)\n");
    for (m = 0; m < board_size; m++)
      for (n = 0; n < board_size; n++) {
	if (p[m][n] == color && is_worm_origin(m, n, m, n)) {
	  const char *status = "UNKNOWN (shouldn't happen)";
	  if (aa_status[m][n] == DEAD)
	    status = "DEAD";
	  else if (aa_status[m][n] == CRITICAL)
	    status = "CRITICAL";
	  else if (aa_status[m][n] == INSUBSTANTIAL)
	    status = "INSUBSTANTIAL";
	  
	  if (aa_status[m][n] != ALIVE)
	    gprintf("%M: %s\n", m, n, status);
	}
      }
  }
  
  /* Accept illegal ko capture here. */
  if (!tryko(ti, tj, color, NULL, EMPTY, -1, -1))
    /* Really shouldn't happen. */
    abortgo(__FILE__, __LINE__, "trymove", ti, tj); 

  aa_val = do_atari_atari(other, &fi, &fj, i, j, -1, -1, 0, minsize);
  
  /* No sufficiently large combination attack, so the move is safe from
   * this danger.
   */
  if (aa_val == 0) {
    popgo();
    return 1;
  }

  while (aa_val > 0) {
    /* Try dropping moves from the combination and see if it still
     * works. What we really want is to get the proper defense move
     * into (*i, *j).
     */
    forbidden[fi][fj] = 1;
    aa_val = do_atari_atari(other, &fi, &fj, i, j, -1, -1, 0, aa_val);
  }

  popgo();
  return 0;
}


/* Ask the atari_atari code if after color plays at (ai,aj)
 * and other plays at (bi,bj) there appears any combination
 * attack. Returns the size of the combination.
 *
 * FIXME: Most of the code below is common with atari_atari() and
 *        should be broken out of both functions.
 */

int
atari_atari_try_combination(int color, int ai, int aj, int bi, int bj)
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
      if (p[m][n] == other) {
	if (dragon[m][n].matcher_status == DEAD)
	  aa_status[m][n] = DEAD;
	else if (worm[m][n].attack_code != 0) {
	  if (worm[m][n].defend_code != 0)
	    aa_status[m][n] = CRITICAL;
	  else
	    aa_status[m][n] = DEAD;

	}
	else
	  aa_status[m][n] = ALIVE;
      }
      else
	aa_status[m][n] = UNKNOWN;
    }

  /* reclassify a worm with 2 liberties as INSUBSTANTIAL if capturing
   * it does not result in a live group.
   */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) 
      if (p[m][n] == other
	  && worm[m][n].origini == m
	  && worm[m][n].originj == n
	  && worm[m][n].liberties == 2
	  && aa_status[m][n] != DEAD
	  && !owl_substantial(m, n)) {
	int ti, tj;
	for (ti = 0; ti < board_size; ti++)
	  for (tj = 0; tj < board_size; tj++)
	    if (is_worm_origin(ti, tj, m, n))
	      aa_status[ti][tj] = INSUBSTANTIAL;
      }

  if (trymove(ai, aj, color, NULL, -1, -1, EMPTY, -1, -1)) {
    if (trymove(bi, bj, other, NULL, -1, -1, EMPTY, -1, -1)) {
      aa_val = do_atari_atari(color, NULL, NULL, NULL, NULL,
			      ai, aj, 0, 0);
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
do_find_defense(int si, int sj, int *i, int *j, 
		int komaster, int kom_i, int kom_j)
{
  int xi, xj;
  int dcode = 0;
  int liberties;
  int found_read_result;
  Read_result *read_result;
  
  SETUP_TRACE_INFO("find_defense", si, sj);
  
  RTRACE("Can we rescue %m?\n", si, sj);

  /* We first check if the number of liberties is larger than four. In
   * that case we don't cache the result and to avoid needlessly
   * storing the position in the hash table, we must do this test
   * before we look for cached results.
   */
  liberties = countlib(si, sj);
  
  if (liberties > 4
      || (liberties == 4 && stackp > depth)
      || (liberties == 3 && stackp > depth)) {
    /* No need to cache the result in these cases. */
    SGFTRACE(-1, -1, WIN, "too many liberties or stackp > depth");
    if (i) *i = -1;
    if (j) *j = -1;
    return WIN;
  }

  if ((stackp <= depth) && (hashflags & HASH_FIND_DEFENSE)) {
    found_read_result = get_read_result(FIND_DEFENSE, komaster, kom_i, kom_j, 
					&si, &sj, &read_result);
    if (found_read_result) {
      TRACE_CACHED_RESULT(*read_result);
      if (rr_get_result(*read_result) != 0) {
	if (i) *i = rr_get_result_i(*read_result);
	if (j) *j = rr_get_result_j(*read_result);
      }

      SGFTRACE(rr_get_result_i(*read_result), rr_get_result_j(*read_result),
	       rr_get_result(*read_result), "cached");
      return rr_get_result(*read_result);
    }

    /* This data should always be recorded. */
    if (read_result) {
      rr_set_compressed_data(*read_result, FIND_DEFENSE, 
			     komaster, kom_i, kom_j, si, sj, stackp);
    }
  } else
    read_result = NULL;

  if (liberties == 1)
    dcode = defend1(si, sj, &xi, &xj, komaster, kom_i, kom_j);
  else if (liberties == 2)
    dcode = defend2(si, sj, &xi, &xj, komaster, kom_i, kom_j);
  else if (liberties == 3)
    dcode = defend3(si, sj, &xi, &xj, komaster, kom_i, kom_j);
  else if (liberties == 4)
    dcode = defend4(si, sj, &xi, &xj, komaster, kom_i, kom_j);

  if (dcode) {
    RTRACE("saving move for %m found at %m!\n", si, sj, xi, xj);
    READ_RETURN(read_result, i, j, xi, xj, dcode);
  }
    
  READ_RETURN0(read_result);
}


/* If si, sj points to a string with exactly one liberty, defend1 
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
defend1(int si, int sj, int *i, int *j, int komaster, int kom_i, int kom_j)
{
  int color, other;
  int xi, xj;
  int libi, libj;
  int movei[MAX_MOVES], movej[MAX_MOVES];
  int scores[MAX_MOVES];
  int moves = 0;
  int savei = -1, savej = -1;
  int savecode = 0;
  int liberties;
  int k;
  int found_read_result;
  Read_result *read_result;

  SETUP_TRACE_INFO("defend1", si, sj);
  reading_node_counter++;
  
  gg_assert(p[si][sj] != EMPTY);
  ASSERT(countlib(si, sj) == 1, si, sj);
  RTRACE("try to escape atari on %m.\n",  si, sj);

  color = p[si][sj];
  other = OTHER_COLOR(color);

  if ((stackp <= depth) && (hashflags & HASH_DEFEND1)) {
  
    found_read_result = get_read_result(DEFEND1, komaster, kom_i, kom_j,
					&si, &sj, &read_result);
    if (found_read_result) {
      TRACE_CACHED_RESULT(*read_result);
      if (rr_get_result(*read_result) != 0) {
	if (i) *i = rr_get_result_i(*read_result);
	if (j) *j = rr_get_result_j(*read_result);
      }

      SGFTRACE(rr_get_result_i(*read_result), rr_get_result_j(*read_result),
	       rr_get_result(*read_result), "cached");
      return rr_get_result(*read_result);
    }

    /* This data should always be recorded. */
    if (read_result) {
      rr_set_compressed_data(*read_result, DEFEND1, komaster, kom_i, kom_j,
			     si, sj, stackp);
    }
  } else
    read_result = NULL;

  /* (libi, libj) will be the liberty of the string. */
  liberties = findlib(si, sj, 1, &libi, &libj);
  ASSERT(liberties == 1, si, sj);

  /* Collect moves to try in the first batch.
   * 1. First order liberty.
   * 2. Chain breaking moves.
   */
  movei[0] = libi;
  movej[0] = libj;
  scores[0] = 0;
  moves = 1;
  
  break_chain_moves(si, sj, movei, movej, scores, &moves);

  order_moves(si, sj, moves, movei, movej, scores, color, read_function_name);

  for (k = 0; k < moves; k++) {
    int new_komaster, new_kom_i, new_kom_j;
    int ko_move;

    xi = movei[k];
    xj = movej[k];
    if (komaster_trymove(xi, xj, color, "defend1-A", si, sj,
			 komaster, kom_i, kom_j,
			 &new_komaster, &new_kom_i, &new_kom_j,
			 &ko_move, stackp <= ko_depth && savecode == 0)) {
      if (!ko_move) {
	int acode = do_attack(si, sj, NULL, NULL,
			      new_komaster, new_kom_i, new_kom_j);
	popgo();
	if (acode == 0) {
	  SGFTRACE(xi, xj, WIN, "defense effective - A");
	  READ_RETURN(read_result, i, j, xi, xj, WIN);
	}
	/* if the move works with ko we save it, then look for something
	 * better.
	 */
	UPDATE_SAVED_KO_RESULT(savecode, savei, savej, acode, xi, xj);
      }
      else {
	if (do_attack(si, sj, NULL, NULL,
		      new_komaster, new_kom_i, new_kom_j) != WIN) {
	  savei = xi;
	  savej = xj;
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
      && countstones(si, sj) == 1
      && is_ko(libi, libj, other, NULL, NULL)) {
    int lib2i[6];
    int lib2j[6];
    liberties = approxlib(libi, libj, color, 6, lib2i, lib2j);
    if (liberties <= 5) {
      for (k = 0; k < liberties; k++) {
	int ai = lib2i[k];
	int aj = lib2j[k];
	if ((liberties == 1 || !is_self_atari(ai, aj, other))
	    && trymove(ai, aj, color, "attack1-C", si, sj,
		       komaster, kom_i, kom_j)) {
	  int acode = do_attack(si, sj, NULL, NULL, komaster, kom_i, kom_j);
	  if (acode == 0) {
	    popgo();
	    SGFTRACE(ai, aj, WIN, "backfilling");
	    READ_RETURN(read_result, i, j, ai, aj, WIN);
	  }
	  UPDATE_SAVED_KO_RESULT(savecode, savei, savej, acode, ai, aj);
	  popgo();
	}
      }
    }
  }
  
  if (savecode != 0) {
    SGFTRACE(savei, savej, savecode, "saved move");
    READ_RETURN(read_result, i, j, savei, savej, savecode);
  }
  
  SGFTRACE(-1, -1, 0, NULL);
  READ_RETURN0(read_result);
}



/* If si, sj points to a group with two liberties, defend2 determines
 * whether the group can be saved by extending, or by capturing part of
 * its surrounding chain. A group is considered safe if either part of
 * the surrounding chain may be captured, or if it can get 3
 * liberties. It is presumed that the opponent could kill if tenuki.
 * If both extensions work, it prefers the one which maximizes 
 * liberties.
 *
 * i and j return the move to save the stones. Can be NULL if caller
 * is not interested in the details.
*/

static int 
defend2(int si, int sj, int *i, int *j, int komaster, int kom_i, int kom_j)
{
  int color, other;
  int xi, xj;
  int liberties;
  int libi[2], libj[2];
  int liberties2;
  int lib2i[6], lib2j[6];
  int movei[MAX_MOVES], movej[MAX_MOVES];
  int scores[MAX_MOVES];
  int moves = 0;
  int savei = -1, savej = -1;
  int savecode = 0;
  int bc;
  int k;
  int r;
  int s;
  int found_read_result;
  Read_result *read_result;

  SETUP_TRACE_INFO("defend2", si, sj);
  reading_node_counter++;
  
  RTRACE("trying to rescue %m\n",  si, sj);
  color = p[si][sj];
  other = OTHER_COLOR(color);

  gg_assert(p[si][sj] != EMPTY);
  gg_assert(countlib(si, sj) == 2);

  if ((stackp <= depth) && (hashflags & HASH_DEFEND2)) {
  
    found_read_result = get_read_result(DEFEND2, komaster, kom_i, kom_j,
					&si, &sj, &read_result);
    if (found_read_result) {
      TRACE_CACHED_RESULT(*read_result);
      if (rr_get_result(*read_result) != 0) {
	if (i) *i = rr_get_result_i(*read_result);
	if (j) *j = rr_get_result_j(*read_result);
      }

      SGFTRACE(rr_get_result_i(*read_result), rr_get_result_j(*read_result),
	       rr_get_result(*read_result), "cached");
      return rr_get_result(*read_result);
    }

    /* This data should always be recorded. */
    if (read_result) {
      rr_set_compressed_data(*read_result, DEFEND2, komaster, kom_i, kom_j,
			     si, sj, stackp);
    }
  } else
    read_result = NULL;

  liberties = findlib(si, sj, 2, libi, libj);
  ASSERT(liberties == 2, si, sj);

  /* Collect moves to try in the first batch.
   * 1. First order liberties.
   * 2. Chain breaking moves.
   * 3. Second order liberties moving up from first line to second.
   * 4. Edge clamps.
   */
  for (k = 0; k < liberties; k++) {
    movei[k] = libi[k];
    movej[k] = libj[k];
    scores[k] = 0;
  }
  moves = liberties;
  
  break_chain_moves(si, sj, movei, movej, scores, &moves);
  break_chain2_efficient_moves(si, sj, movei, movej, scores, &moves);
  propose_edge_moves(si, sj, libi, libj, liberties, movei, movej, scores,
		     &moves, color);
  edge_clamp(si, sj, movei, movej, scores, &moves);

  order_moves(si, sj, moves, movei, movej, scores, color, read_function_name);

  for (k = 0; k < moves; k++) {
    int new_komaster, new_kom_i, new_kom_j;
    int ko_move;

    xi = movei[k];
    xj = movej[k];
    
    if (komaster_trymove(xi, xj, color, "defend2-A", si, sj,
			 komaster, kom_i, kom_j,
			 &new_komaster, &new_kom_i, &new_kom_j,
			 &ko_move, stackp <= ko_depth && savecode == 0)) {
      if (!ko_move) {
	int acode = do_attack(si, sj, NULL, NULL,
			      new_komaster, new_kom_i, new_kom_j);
	popgo();
	if (acode == 0) {
	  SGFTRACE(xi, xj, WIN, "defense effective - A");
	  READ_RETURN(read_result, i, j, xi, xj, WIN);
	}
	/* if the move works with ko we save it, then look for something
	 * better.
	 */
	UPDATE_SAVED_KO_RESULT(savecode, savei, savej, acode, xi, xj);
      }
      else {
	if (do_attack(si, sj, NULL, NULL,
		      new_komaster, new_kom_i, new_kom_j) != WIN) {
	  savei = xi;
	  savej = xj;
	  savecode = KO_B;
	}
	popgo();
      }
    }
  }

  /* Look for backfilling moves. */
  for (k = 0; k < liberties; k++) {
    if (is_self_atari(libi[k], libj[k], other)) {
      liberties2 = approxlib(libi[k], libj[k], color, 6, lib2i, lib2j);
      for (r = 0; r < liberties2; r++) {
	xi = lib2i[r];
	xj = lib2j[r];
	/* Don't reconsider previously tested moves. */
	for (s = 0; s < moves; s++)
	  if (xi == movei[s] && xj == movej[s])
	    break;
	if (s < moves)
	  continue;

	if (trymove(xi, xj, color, "defend2-C", si, sj,
		    komaster, kom_i, kom_j)) {
	  int acode;
	  /* If the newly placed stone is in atari, we give up without
           * fight.
	   */
	  if (countlib(xi, xj) == 1 && countstones(xi, xj) > 1)
	    acode = WIN;
	  else
	    acode = do_attack(si, sj, NULL, NULL, komaster, kom_i, kom_j);

	  popgo();
	  if (acode == 0) {
	    SGFTRACE(xi, xj, WIN, "backfill effective");
	    READ_RETURN(read_result, i, j, xi, xj, WIN);
	  }
	  UPDATE_SAVED_KO_RESULT(savecode, savei, savej, acode, xi, xj);
	}
      }
    }
    else if (approxlib(libi[k], libj[k], other, 3, lib2i, lib2j) == 2) {
      for (r = 0; r < 2; r++) {
	xi = lib2i[r];
	xj = lib2j[r];
	/* Don't reconsider previously tested moves. */
	for (s = 0; s < moves; s++)
	  if (xi == movei[s] && xj == movej[s])
	    break;
	if (s < moves)
	  continue;

	if (!is_self_atari(xi, xj, color)
	    && trymove(xi, xj, color, "defend2-D", si, sj,
		       komaster, kom_i, kom_j)) {
	  int acode = do_attack(si, sj, NULL, NULL, komaster, kom_i, kom_j);
	  popgo();
	  if (acode == 0) {
	    SGFTRACE(xi, xj, WIN, "backfill effective");
	    READ_RETURN(read_result, i, j, xi, xj, WIN);
	  }
	  UPDATE_SAVED_KO_RESULT(savecode, savei, savej, acode, xi, xj);
	}
      }      
    }
  }

  if (stackp <= depth) {
    for (k = 0; k < liberties; ++k) {
      int dcode = special_rescue(si, sj, libi[k], libj[k], &xi, &xj, 
				 komaster, kom_i, kom_j);
      if (dcode == WIN) {
	SGFTRACE(xi, xj, WIN, "special rescue");
	READ_RETURN(read_result, i, j, xi, xj, WIN);
      }
      UPDATE_SAVED_KO_RESULT_UNREVERSED(savecode, savei, savej, dcode, xi, xj);
    }
  }
  
  if (stackp <= backfill_depth) {
    int dcode = special_rescue2(si, sj, libi, libj, &xi, &xj, 
				komaster, kom_i, kom_j);
    if (dcode == WIN) {
      SGFTRACE(xi, xj, WIN, "special rescue2");
      READ_RETURN(read_result, i, j, xi, xj, WIN);
    }
    UPDATE_SAVED_KO_RESULT_UNREVERSED(savecode, savei, savej, dcode, xi, xj);
  }
  
  if (level >= 10 && stackp <= superstring_depth) {
    int dcode = superstring_breakchain(si, sj, &xi, &xj,
				       komaster, kom_i, kom_j, 4);
    if (dcode == WIN) {
      SGFTRACE(xi, xj, WIN, "superstring_breakchain");
      READ_RETURN(read_result, i, j, xi, xj, WIN);
    }
    UPDATE_SAVED_KO_RESULT_UNREVERSED(savecode, savei, savej, dcode, xi, xj);
  }

  /* If nothing else works, we try playing a liberty of the
   * super_string.
   */
  if (level >= 10 && stackp <= superstring_depth) {
    int libs;
    int libi[MAX_LIBERTIES + 4], libj[MAX_LIBERTIES + 4];

    find_superstring_liberties(si, sj, &libs, libi, libj, 3);
    for (k = 0; k < libs; k++) {
      int ai = libi[k];
      int aj = libj[k];
	
      if (liberty_of_string(ai, aj, si, sj))
	continue;
      if (trymove(ai, aj, color, "defend2-E", si, sj,
		    komaster, kom_i, kom_j)) {
	int acode;
	/* If the newly placed stone is in atari, we give up without fight. */
	if (countlib(ai, aj) == 1)
	  acode = WIN;
	else
	  acode = do_attack(si, sj, NULL, NULL, komaster, kom_i, kom_j);
	popgo();
	if (acode == 0) {
	  SGFTRACE(ai, aj, WIN, "superstring liberty");
	  READ_RETURN(read_result, i, j, ai, aj, WIN);
	}
	UPDATE_SAVED_KO_RESULT(savecode, savei, savej, acode, ai, aj);
      }
    }
    
    /* Now we are truly desperate. Try playing second order liberties of
     * the superstring.
     */
    for (k = 0; k < libs; k++) {
      int ai = libi[k];
      int aj = libj[k];
      int dcode;
	
      if (liberty_of_string(ai, aj, si, sj))
	continue;
      
      dcode = special_rescue(si, sj, ai, aj, &xi, &xj, 
			     komaster, kom_i, kom_j);
      if (dcode == WIN) {
	SGFTRACE(xi, xj, WIN, "special rescue");
	READ_RETURN(read_result, i, j, xi, xj, WIN);
      }
      UPDATE_SAVED_KO_RESULT_UNREVERSED(savecode, savei, savej, dcode,
					xi, xj);
    }
  }

  /* We place the more speculative moves trying to break chain links
   * with 2 or 3 liberties last, because these moves often are not
   * really relevant.
   */
  
  bc = break_chain2(si, sj, &xi, &xj, komaster, kom_i, kom_j);
  if (bc == WIN) {
    SGFTRACE(xi, xj, bc, "break chain2");
    READ_RETURN(read_result, i, j, xi, xj, bc);
  }
  UPDATE_SAVED_KO_RESULT_UNREVERSED(savecode, savei, savej, bc, xi, xj);
  
  if (stackp <= backfill2_depth) {
    bc = break_chain3(si, sj, &xi, &xj, komaster, kom_i, kom_j);
    if (bc == WIN) {
      SGFTRACE(xi, xj, bc, "break chain3");
      READ_RETURN(read_result, i, j, xi, xj, bc);
    }
    UPDATE_SAVED_KO_RESULT_UNREVERSED(savecode, savei, savej, bc, xi, xj);
  }
  
  if (savecode != 0) {
    SGFTRACE(savei, savej, savecode, "saved move");
    READ_RETURN(read_result, i, j, savei, savej, savecode);
  }

  RTRACE("failed to find rescuing move.\n");
  SGFTRACE(-1, -1, 0, NULL);
  READ_RETURN0(read_result);
}


/* defend3(si, sj, *i, *j) attempts to find a move rescuing the 
 * string at (si, sj) with 3 liberties.  If such a move can be found,
 * it returns true, and if the pointers i, j are not NULL, 
 * then it returns the saving move in (*i, *j).
 */

static int 
defend3(int si, int sj, int *i, int *j, int komaster, int kom_i, int kom_j)
{
  int color, other;
  int xi, xj;
  int liberties;
  int libi[3], libj[3];
#if 0
  int liberties2;
  int lib2i[6], lib2j[6];
#endif
  int movei[MAX_MOVES], movej[MAX_MOVES];
  int scores[MAX_MOVES];
  int moves = 0;
  int savei = -1, savej = -1;
  int savecode = 0;
  int bc;
  int k;
#if 0
  int r;
  int s;
#endif
  int found_read_result;
  Read_result *read_result;

  SETUP_TRACE_INFO("defend3", si, sj);
  reading_node_counter++;

  RTRACE("trying to rescue %m\n",  si, sj);
  color = p[si][sj];
  other = OTHER_COLOR(color);

  gg_assert(p[si][sj] != EMPTY);
  gg_assert(countlib(si, sj) == 3);

  if ((stackp <= depth) && (hashflags & HASH_DEFEND3)) {
    found_read_result = get_read_result(DEFEND3, komaster, kom_i, kom_j,
					&si, &sj, &read_result);
    if (found_read_result) {
      TRACE_CACHED_RESULT(*read_result);
      if (rr_get_result(*read_result) != 0) {
	if (i) *i = rr_get_result_i(*read_result);
	if (j) *j = rr_get_result_j(*read_result);
      }

      SGFTRACE(rr_get_result_i(*read_result), rr_get_result_j(*read_result),
	       rr_get_result(*read_result), "cached");
      return rr_get_result(*read_result);
    }

    /* This data should always be recorded. */
    if (read_result) {
      rr_set_compressed_data(*read_result, DEFEND3, komaster, kom_i, kom_j, 
			     si, sj, stackp);
    }
  } else
    read_result = NULL;

  liberties = findlib(si, sj, 3, libi, libj);
  ASSERT(liberties == 3, si, sj);

  /* Collect moves to try in the first batch.
   * 1. First order liberties.
   * 2. Chain breaking moves.
   * 3. Second order liberties moving up from first line to second.
   * 4. Edge clamps.
   */
  for (k = 0; k < liberties; k++) {
    movei[k] = libi[k];
    movej[k] = libj[k];
    scores[k] = 0;
  }
  moves = liberties;
  
  break_chain_moves(si, sj, movei, movej, scores, &moves);
  break_chain2_efficient_moves(si, sj, movei, movej, scores, &moves);
  propose_edge_moves(si, sj, libi, libj, liberties, movei, movej, scores, 
		     &moves, color);
  edge_clamp(si, sj, movei, movej, scores, &moves);

  order_moves(si, sj, moves, movei, movej, scores, color, read_function_name);

  for (k = 0; k < moves; k++) {
    int new_komaster, new_kom_i, new_kom_j;
    int ko_move;

    if (stackp >= branch_depth && k > 0)
      break;
    
    xi = movei[k];
    xj = movej[k];
    
    if (komaster_trymove(xi, xj, color, "defend3-A", si, sj,
			 komaster, kom_i, kom_j,
			 &new_komaster, &new_kom_i, &new_kom_j,
			 &ko_move, stackp <= ko_depth && savecode == 0)) {
      if (!ko_move) {
	int acode = do_attack(si, sj, NULL, NULL,
			      new_komaster, new_kom_i, new_kom_j);
	popgo();
	if (acode == 0) {
	  SGFTRACE(xi, xj, WIN, "defense effective - A");
	  READ_RETURN(read_result, i, j, xi, xj, WIN);
	}
	/* if the move works with ko we save it, then look for something
	 * better.
	 */
	UPDATE_SAVED_KO_RESULT(savecode, savei, savej, acode, xi, xj);
      }
      else {
	if (do_attack(si, sj, NULL, NULL,
		      new_komaster, new_kom_i, new_kom_j) != WIN) {
	  savei = xi;
	  savej = xj;
	  savecode = KO_B;
	}
	popgo();
      }
    }
  }

#if 0
  if (stackp <= backfill_depth) {
    bc = break_chain2(si, sj, &xi, &xj, komaster);
    if (bc == WIN) {
      SGFTRACE(xi, xj, bc, "break chain2");
      READ_RETURN(read_result, i, j, xi, xj, bc);
    }
    UPDATE_SAVED_KO_RESULT_UNREVERSED(savecode, savei, savej, bc, xi, xj);
  }
  
  if (stackp <= backfill2_depth) {
    bc = break_chain3(si, sj, &xi, &xj, komaster);
    if (bc == WIN) {
      SGFTRACE(xi, xj, bc, "break chain3");
      READ_RETURN(read_result, i, j, xi, xj, bc);
    }
    UPDATE_SAVED_KO_RESULT_UNREVERSED(savecode, savei, savej, bc, xi, xj);
  }
#endif
  
  /* This looks a little too expensive. */
#if 0
  /* Look for backfilling moves. */
  if (stackp <= backfill_depth) {
    for (k = 0; k < liberties; k++) {
      if (is_self_atari(libi[k], libj[k], other)) {
	liberties2 = approxlib(libi[k], libj[k], color, 6, lib2i, lib2j);
	for (r = 0; r < liberties2; r++) {
	  xi = lib2i[r];
	  xj = lib2j[r];
	  /* Don't reconsider previously tested moves. */
	  for (s = 0; s < moves; s++)
	    if (xi == movei[s] && xj == movej[s])
	      break;
	  if (s < moves)
	    continue;
	  
	  if (trymove(xi, xj, color, "defend3-D", si, sj,
		      komaster, kom_i, kom_j)) {
	    int acode;
	    /* If the newly placed stone is in atari, we give up
             * without fight.
	     */
	    if (countlib(xi, xj) == 1)
	      acode = WIN;
	    else
	      acode = do_attack(si, sj, NULL, NULL, komaster, kom_i, kom_j);

	    popgo();
	    if (acode == 0) {
	      SGFTRACE(xi, xj, WIN, "backfill effective");
	      READ_RETURN(read_result, i, j, xi, xj, WIN);
	    }
	    UPDATE_SAVED_KO_RESULT(savecode, savei, savej, acode, xi, xj);
	  }
	}
      }
      else {
	liberties2 = approxlib(libi[k], libj[k], other, 3, lib2i, lib2j);
	if (liberties2 <= 3) {
	  for (r = 0; r < liberties2; r++) {
	    xi = lib2i[r];
	    xj = lib2j[r];
	    /* Don't reconsider previously tested moves. */
	    for (s = 0; s < moves; s++)
	      if (xi == movei[s] && xj == movej[s])
		break;
	    if (s < moves)
	      continue;
	    
	    if (!is_self_atari(xi, xj, color)
		&& trymove(xi, xj, color, "defend2-D", si, sj,
			   komaster, kom_i, kom_j)) {
	      int acode = do_attack(si, sj, NULL, NULL, 
				    komaster, kom_i, kom_j);
	      popgo();
	      if (acode == 0) {
		SGFTRACE(xi, xj, WIN, "backfill effective");
		READ_RETURN(read_result, i, j, xi, xj, WIN);
	      }
	      UPDATE_SAVED_KO_RESULT(savecode, savei, savej, acode, xi, xj);
	    }
	  }
	}
      }
    }
  }
#endif
  
  /* If nothing else works, try to defend with second order liberties. */
  if (stackp <= depth) {
    for (k = 0; k < liberties; ++k) {
      int dcode = special_rescue(si, sj, libi[k], libj[k], &xi, &xj, 
				 komaster, kom_i, kom_j);
      if (dcode == WIN) {
	SGFTRACE(xi, xj, WIN, "special rescue");
	READ_RETURN(read_result, i, j, xi, xj, WIN);
      }
      UPDATE_SAVED_KO_RESULT_UNREVERSED(savecode, savei, savej, dcode, xi, xj);
    }
  }

  if (stackp <= backfill_depth) {
    int dcode = special_rescue3(si, sj, libi, libj, &xi, &xj, 
				komaster, kom_i, kom_j);
    if (dcode == WIN) {
      SGFTRACE(xi, xj, WIN, "special rescue2");
      READ_RETURN(read_result, i, j, xi, xj, WIN);
    }
    UPDATE_SAVED_KO_RESULT_UNREVERSED(savecode, savei, savej, dcode, xi, xj);
  }
    
  if (stackp <= backfill_depth) {
    int dcode = special_rescue4(si, sj, libi, libj, &xi, &xj, 
				komaster, kom_i, kom_j);
    if (dcode == WIN) {
      SGFTRACE(xi, xj, WIN, "special rescue4");
      READ_RETURN(read_result, i, j, xi, xj, WIN);
    }
    UPDATE_SAVED_KO_RESULT_UNREVERSED(savecode, savei, savej, dcode, xi, xj);
  }
    
  if (level >= 10 && stackp <= backfill2_depth) {
    int dcode = superstring_breakchain(si, sj, &xi, &xj,
				       komaster, kom_i, kom_j, 4);
    if (dcode == WIN) {
      SGFTRACE(xi, xj, WIN, "superstring_breakchain");
      READ_RETURN(read_result, i, j, xi, xj, WIN);
    }
    UPDATE_SAVED_KO_RESULT_UNREVERSED(savecode, savei, savej, dcode, xi, xj);
  }

  /* If nothing else works, we try playing a liberty of the
   * super_string.
   */
  if (level >= 10 && stackp <= backfill2_depth) {
    int libs;
    int libi[MAX_LIBERTIES + 4], libj[MAX_LIBERTIES + 4];

    find_superstring_liberties(si, sj, &libs, libi, libj, 3);
    for (k = 0; k < libs; k++) {
      int ai = libi[k];
      int aj = libj[k];
	
      if (liberty_of_string(ai, aj, si, sj))
	continue;
      if (trymove(ai, aj, color, "defend3-C", si, sj, 
		  komaster, kom_i, kom_j)) {
	int acode;
	/* If the newly placed stone is in atari, we give up without fight. */
	if (countlib(ai, aj) == 1)
	  acode = WIN;
	else
	  acode = do_attack(si, sj, NULL, NULL, komaster, kom_i, kom_j);

	popgo();
	if (acode == 0) {
	  SGFTRACE(ai, aj, WIN, "superstring liberty");
	  READ_RETURN(read_result, i, j, ai, aj, WIN);
	}
	UPDATE_SAVED_KO_RESULT(savecode, savei, savej, acode, ai, aj);
      }
    }

    /* Now we are truly desperate. Try playing second order liberties of
     * the superstring.
     */
    for (k = 0; k < libs; k++) {
      int ai = libi[k];
      int aj = libj[k];
      int dcode;
	
      if (liberty_of_string(ai, aj, si, sj))
	continue;
      
      dcode = special_rescue(si, sj, ai, aj, &xi, &xj, komaster, kom_i, kom_j);
      if (dcode == WIN) {
	SGFTRACE(xi, xj, WIN, "special rescue");
	READ_RETURN(read_result, i, j, xi, xj, WIN);
      }
      UPDATE_SAVED_KO_RESULT_UNREVERSED(savecode, savei, savej, dcode,
					xi, xj);
    }
  }

  /* We place the more speculative moves trying to break chain links
   * with 2 or 3 liberties last, because these moves often are not
   * really relevant.
   */
#if 1
  if (stackp <= backfill2_depth) {
    bc = break_chain2(si, sj, &xi, &xj, komaster, kom_i, kom_j);
    if (bc == WIN) {
      SGFTRACE(xi, xj, bc, "break chain2");
      READ_RETURN(read_result, i, j, xi, xj, bc);
    }
    UPDATE_SAVED_KO_RESULT_UNREVERSED(savecode, savei, savej, bc, xi, xj);
  }
  
  if (stackp <= backfill2_depth) {
    bc = break_chain3(si, sj, &xi, &xj, komaster, kom_i, kom_j);
    if (bc == WIN) {
      SGFTRACE(xi, xj, bc, "break chain3");
      READ_RETURN(read_result, i, j, xi, xj, bc);
    }
    UPDATE_SAVED_KO_RESULT_UNREVERSED(savecode, savei, savej, bc, xi, xj);
  }
#endif
  
  if (savecode != 0) {
    SGFTRACE(savei, savej, savecode, "saved move");
    READ_RETURN(read_result, i, j, savei, savej, savecode);
  }

  RTRACE("failed to find rescuing move.\n");
  SGFTRACE(-1, -1, 0, NULL);
  READ_RETURN0(read_result);
}


/* defend4(si, sj, *i, *j) attempts to find a move rescuing the 
 * string at (si, sj) with 4 liberties.  If such a move can be found,
 * it returns true, and if the pointers i, j are not NULL, 
 * then it returns the saving move in (*i, *j).
 */

static int 
defend4(int si, int sj, int *i, int *j, int komaster, int kom_i, int kom_j)
{
  int color, other;
  int xi, xj;
  int liberties;
  int libi[4], libj[4];
  int movei[MAX_MOVES], movej[MAX_MOVES];
  int scores[MAX_MOVES];
  int moves = 0;
  int savei = -1, savej = -1;
  int savecode = 0;
  int k;
  int found_read_result;
  Read_result *read_result;
  
  SETUP_TRACE_INFO("defend4", si, sj);
  reading_node_counter++;

  RTRACE("trying to rescue %m\n",  si, sj);
  color = p[si][sj];
  other = OTHER_COLOR(color);

  gg_assert(p[si][sj] != EMPTY);
  gg_assert(countlib(si, sj) == 4);

  if ((stackp <= depth) && (hashflags & HASH_DEFEND4)) {
    found_read_result = get_read_result(DEFEND4, komaster, kom_i, kom_j,
					&si, &sj, &read_result);
    if (found_read_result) {
      TRACE_CACHED_RESULT(*read_result);
      if (rr_get_result(*read_result) != 0) {
	if (i) *i = rr_get_result_i(*read_result);
	if (j) *j = rr_get_result_j(*read_result);
      }

      SGFTRACE(rr_get_result_i(*read_result), rr_get_result_j(*read_result),
	       rr_get_result(*read_result), "cached");
      return rr_get_result(*read_result);
    }

    /* This data should always be recorded. */
    if (read_result) {
      rr_set_compressed_data(*read_result, DEFEND4, komaster, kom_i, kom_j, 
			     si, sj, stackp);
    }
  } else
    read_result = NULL;

  liberties = findlib(si, sj, 4, libi, libj);
  ASSERT(liberties == 4, si, sj);

  /* Collect moves to try in the first batch.
   * 1. First order liberties.
   * 2. Chain breaking moves.
   */
  for (k = 0; k < liberties; k++) {
    movei[k] = libi[k];
    movej[k] = libj[k];
    scores[k] = 0;
  }
  moves = liberties;
  
  break_chain_moves(si, sj, movei, movej, scores, &moves);
  break_chain2_efficient_moves(si, sj, movei, movej, scores, &moves);

  order_moves(si, sj, moves, movei, movej, scores, color, read_function_name);

  for (k = 0; k < moves; k++) {
    int new_komaster, new_kom_i, new_kom_j;
    int ko_move;

    if (stackp >= branch_depth && k > 0)
      break;
    
    xi = movei[k];
    xj = movej[k];
    
    if (komaster_trymove(xi, xj, color, "defend4-A", si, sj,
			 komaster, kom_i, kom_j,
			 &new_komaster, &new_kom_i, &new_kom_j,
			 &ko_move, stackp <= ko_depth && savecode == 0)) {
      if (!ko_move) {
	int acode = do_attack(si, sj, NULL, NULL,
			      new_komaster, new_kom_i, new_kom_j);
	popgo();
	if (acode == 0) {
	  SGFTRACE(xi, xj, WIN, "defense effective - A");
	  READ_RETURN(read_result, i, j, xi, xj, WIN);
	}
	/* if the move works with ko we save it, then look for something
	 * better.
	 */
	UPDATE_SAVED_KO_RESULT(savecode, savei, savej, acode, xi, xj);
      }
      else {
	if (do_attack(si, sj, NULL, NULL,
		      new_komaster, new_kom_i, new_kom_j) != WIN) {
	  savei = xi;
	  savej = xj;
	  savecode = KO_B;
	}
	popgo();
      }
    }
  }

  if (stackp <= backfill_depth) {
    int bc = break_chain2(si, sj, &xi, &xj, komaster, kom_i, kom_j);
    if (bc == WIN) {
      SGFTRACE(xi, xj, WIN, "break chain2");
      READ_RETURN(read_result, i, j, xi, xj, WIN);
    }
    UPDATE_SAVED_KO_RESULT_UNREVERSED(savecode, savei, savej, bc, xi, xj);
  }

  if (savecode != 0) {
    SGFTRACE(savei, savej, savecode, "saved move");
    READ_RETURN(read_result, i, j, savei, savej, savecode);
  }

  RTRACE("failed to find rescuing move.\n");
  SGFTRACE(-1, -1, 0, NULL);
  READ_RETURN0(read_result);
}


/*
 * special_rescue(si, sj, ai, aj, *ti, *tj) is called with (si, sj) a
 * string having a liberty at (ai, aj). The saving move is returned
 * in (*ti, *tj).
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
special_rescue(int si, int sj, int ai, int aj, int *ti, int *tj, 
	       int komaster, int kom_i, int kom_j)
{
  int other = OTHER_COLOR(p[si][sj]);
  int k;
  int savei = -1, savej = -1;
  int savecode = 0;

  /* Loop over the four neighbours of the liberty, (ai+di, aj+dj). */
  for (k = 0; k < 4; k++) {
    int di = deltai[k];
    int dj = deltaj[k];
    if (ON_BOARD(ai+di, aj+dj)
	&& p[ai+di][aj+dj] == EMPTY) {
      /* Use approxlib() to test for trivial capture. */
      if (approxlib(ai, aj, other, 3, NULL, NULL) > 2)
	continue;

      /* Don't play into a self atari. */
      if (is_self_atari(ai+di, aj+dj, p[si][sj]))
	continue;

      if (trymove(ai+di, aj+dj, p[si][sj], "special_rescue", si, sj,
		  komaster, kom_i, kom_j)) {
	int acode = do_attack(si, sj, NULL, NULL, komaster, kom_i, kom_j);
	if (acode == 0) {
	  popgo();
	  if (ti) *ti = ai+di;
	  if (tj) *tj = aj+dj;
	  return WIN;
	}
	UPDATE_SAVED_KO_RESULT(savecode, savei, savej, acode, ai+di, aj+dj);
	popgo();
      }
    }
  }

  if (savecode != 0) {
    if (ti) *ti = savei;
    if (tj) *tj = savej;
    return savecode;
  }
  
  return 0;
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
special_rescue2(int si, int sj, int libi[2], int libj[2], int *ti, int *tj, 
		int komaster, int kom_i, int kom_j)
{
  int color = p[si][sj];
  int other = OTHER_COLOR(color);
  int newlibi[3];
  int newlibj[3];
  int xi, xj;
  int savei = -1, savej = -1;
  int savecode = 0;
  int k;

  for (k = 0; k < 2; ++k) {
    /* Let (ai, aj) and (bi, bj) be the two liberties. Reverse the
     * order during the second pass through the loop.
     */
    int ai = libi[k];
    int aj = libj[k];
    int bi = libi[1-k];
    int bj = libj[1-k];
    if (is_suicide(ai, aj, other) 
	&& (approxlib(ai, aj, color, 3, newlibi, newlibj) == 2)) {
      if ((newlibi[0] != bi) || (newlibj[0] != bj)) {
	xi = newlibi[0];
	xj = newlibj[0];
      } else {
	xi = newlibi[1];
	xj = newlibj[1];
      }

      if (!is_self_atari(xi, xj, color)
	  && trymove(xi, xj, color, "special_rescue2", si, sj,
		     komaster, kom_i, kom_j)) {
	int acode = do_attack(si, sj, NULL, NULL, komaster, kom_i, kom_j);
	if (acode != WIN) {
	  if (acode == 0) {
	    popgo();
	    if (ti) *ti = xi;
	    if (tj) *tj = xj;
	    return WIN;
	  }
	  UPDATE_SAVED_KO_RESULT(savecode, savei, savej, acode, xi, xj);
	}
	popgo();
      }
    }
  }

  if (savecode) {
    if (ti) *ti = savei;
    if (tj) *tj = savej;
    return savecode;
  }

  return 0;
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
special_rescue3(int si, int sj, int libi[3], int libj[3], int *ti, int *tj, 
		int komaster, int kom_i, int kom_j)
{
  int color = p[si][sj];
  int other = OTHER_COLOR(color);
  int ai, aj, bi, bj, ci, cj, di, dj, ei, ej, fi, fj, gi, gj, oi, oj;
  int savei = -1, savej = -1;
  int savecode = 0;
  int k, l, r;

  ASSERT(countlib(si, sj) == 3, si, sj);
  
  for (r = 0; r < 3; r++) {
    /* Let (ai, aj) be one of the three liberties. */
    ai = libi[r];
    aj = libj[r];
    /* Try to find the configuration above. */
    for (k = 0; k < 4; k++) {
      bi = ai + deltai[k];
      bj = aj + deltaj[k];
      if (ON_BOARD(bi, bj))
	continue;

      ci = ai - deltai[k];
      cj = aj - deltaj[k];
      if (p[ci][cj] != color)
	continue;
      
      find_origin(ci, cj, &oi, &oj);
      if (oi != si || oj != sj)
	continue;

      for (l = 0; l < 2; l++) {
	int normali = deltaj[k];
	int normalj = -deltai[k];
	if (l == 1) {
	  normali = -normali;
	  normalj = -normalj;
	}
	
	di = ci + normali;
	dj = cj + normalj;
	if (!ON_BOARD(di, dj) || p[di][dj] != other)
	  continue;

	ei = di + normali;
	ej = dj + normalj;
	if (!ON_BOARD(ei, ej) || p[ei][ej] != color)
	  continue;

	fi = ai + normali;
	fj = aj + normalj;
	if (!ON_BOARD(fi, fj) || p[fi][fj] != EMPTY)
	  continue;

	gi = fi + normali;
	gj = fj + normalj;
	if (!ON_BOARD(gi, gj) || p[gi][gj] != EMPTY)	
	  continue;

	/* Configuration found. Now require an X move at 'a' not
	 * getting too many liberties
	 */

	if (approxlib(ai, aj, other, 4, NULL, NULL) > 3)
	  continue;
	
	/* Try to play at (fi, fj). */
	if (trymove(fi, fj, color, "special_rescue3", si, sj,
		    komaster, kom_i, kom_j)) {
	  int acode = do_attack(si, sj, NULL, NULL, komaster, kom_i, kom_j);
	  if (acode != WIN) {
	    if (acode == 0) {
	      popgo();
	      if (ti) *ti = fi;
	      if (tj) *tj = fj;
	      return WIN;
	    }
	    UPDATE_SAVED_KO_RESULT(savecode, savei, savej, acode, fi, fj);
	  }
	  popgo();
	}
      }
    }
  }

  if (savecode) {
    if (ti) *ti = savei;
    if (tj) *tj = savej;
    return savecode;
  }
  
  return 0;
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
special_rescue4(int si, int sj, int libi[3], int libj[3], int *ti, int *tj, 
		int komaster, int kom_i, int kom_j)
{
  int color = p[si][sj];
  int other = OTHER_COLOR(color);
  int ai, aj, bi, bj, ci, cj, di, dj, ei, ej, oi, oj;
  int savei = -1, savej = -1;
  int savecode = 0;
  int k, l, r;

  ASSERT(countlib(si, sj) == 3, si, sj);
  
  for (r = 0; r < 3; r++) {
    /* Let (ai, aj) be one of the three liberties. */
    ai = libi[r];
    aj = libj[r];
    /* Try to find the configuration above. */
    for (k = 0; k < 4; k++) {
      bi = ai + deltai[k];
      bj = aj + deltaj[k];
      if (!ON_BOARD(bi, bj) || p[bi][bj] != color)
	continue;

      find_origin(bi, bj, &oi, &oj);
      if (oi != si || oj != sj)
	continue;

      for (l = 0; l < 2; l++) {
	int normali = deltaj[k];
	int normalj = -deltai[k];
	if (l == 1) {
	  normali = -normali;
	  normalj = -normalj;
	}
	
	ci = ai + normali;
	cj = aj + normalj;
	if (!ON_BOARD(ci, cj) || p[ci][cj] != EMPTY)
	  continue;

	di = ci + normali;
	dj = cj + normalj;
	if (!ON_BOARD(di, dj) || p[di][dj] != color)
	  continue;

	ei = bi + normali;
	ej = bj + normalj;
	if (!ON_BOARD(ei, ej) || p[ei][ej] != other)
	  continue;

	/* Configuration found. Now require that (di, dj) has at least 3
         * liberties and (ei, ej) at most 3 liberties.
	 */

	if (countlib(di, dj) < 3)
	  continue;
	
	if (countlib(ei, ej) > 3)
	  continue;
	
	/* Try to play at (ci, cj). */
	if (trymove(ci, cj, color, "special_rescue4", si, sj,
		    komaster, kom_i, kom_j)) {
	  int acode = do_attack(si, sj, NULL, NULL, komaster, kom_i, kom_j);
	  if (acode != WIN) {
	    if (acode == 0) {
	      popgo();
	      if (ti) *ti = ci;
	      if (tj) *tj = cj;
	      return WIN;
	    }
	    UPDATE_SAVED_KO_RESULT(savecode, savei, savej, acode, ci, cj);
	  }
	  popgo();
	}
      }
    }
  }

  if (savecode) {
    if (ti) *ti = savei;
    if (tj) *tj = savej;
    return savecode;
  }
  
  return 0;
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
 * This function tries to find an adjacent string (ai, aj) with
 * exactly three liberties. One of the liberties (bi, bj) must be on
 * the edge (but not in the corner). Diagonal to this liberty must be
 * one stone of the attacked string (ci, cj) and another liberty (di,
 * dj) of the adjacent string. The third liberty (ei, ej) must be
 * adjacent to (di, dj). Furthermore must an O stone at (di, dj) get
 * at least three liberties and and X stone at (ei, ej) must get at
 * most three liberties.
 *
 * |.XXOO.
 * |XXOXe.
 * |OOcad.
 * |...b..
 * +------
 *
 * The defense move at (di, dj) is proposed if the above conditions
 * are satisfied.
 */

static void
edge_clamp(int si, int sj, int movei[MAX_MOVES], int movej[MAX_MOVES],
	   int scores[MAX_MOVES], int *moves)
{
  int k, l, r;
  int color = p[si][sj];
  int other = OTHER_COLOR(color);
  int ai, aj;
  int bi, bj;
  int ci, cj;
  int di, dj;
  int ei, ej;
  int adj, adji[MAXCHAIN], adjj[MAXCHAIN];
  int libi[3], libj[3];
  
  /* Pick up neighbors with three liberties. */
  adj = chainlinks2(si, sj, adji, adjj, 3);

  for (r = 0; r < adj; r++) {
    ai = adji[r];
    aj = adjj[r];
    /* Find a liberty at the edge. */
    bi = -1;
    bj = -1;
    findlib(ai, aj, 3, libi, libj);
    for (k = 0; k < 3; k++) {
      if (libi[k] == 0 || libi[k] == board_size-1
	  || libj[k] == 0 || libj[k] == board_size-1) {
	bi = libi[k];
	bj = libj[k];
	break;
      }
    }
    if (bj == -1)
      continue;

    /* Edge liberty found. Establish up and right directions. */
    for (k = 0; k < 4; k++) {
      int upi = deltai[k];
      int upj = deltaj[k];
      if (ON_BOARD(bi-upi, bj-upj))
	continue;
      if (p[bi+upi][bj+upj] != other)
	continue;
       
      for (l = 0; l < 2; l++) {
	int righti = upj;
	int rightj = -upi;
	if (l == 1) {
	  righti = -righti;
	  rightj = -rightj;
	}

	ci = bi + upi - righti;
	cj = bj + upj - rightj;
	di = bi + upi + righti;
	dj = bj + upj + rightj;

	if (!ON_BOARD(ci, cj) 
	    || p[ci][cj] != color 
	    || !same_string(ci, cj, si, sj))
	  continue;

	if (!ON_BOARD(di, dj) 
	    || p[di][dj] != EMPTY 
	    || !liberty_of_string(di, dj, ai, aj))
	  continue;

	ei = di + upi;
	ej = dj + upj;

	if (p[ei][ej] != EMPTY || !liberty_of_string(ei, ej, ai, aj))
	  continue;

	if (approxlib(di, dj, color, 3, NULL, NULL) < 3)
	  continue;
	
	if (approxlib(ei, ej, other, 4, NULL, NULL) > 3)
	  continue;

	/* (di, dj) looks like a good move. Add it to the list with a
         * substantial initial score.
	 */
	ADD_CANDIDATE_MOVE(di, dj, 10, movei, movej, scores, *moves);
      }
    }
  }
}


/* 
 * This function handles some special cases on the edge.
 *
 * 1. If (si, sj) points to a string and 'a' an edge liberty of it,
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
 *    is removed from the  list of moves (movei[], movej[]).
 *
 * 2. If (si, sj) points to a string and 'a' an edge liberty of it,
 *    the drawing back/climbing up move 'b' is often correct attack or
 *    defense. Another good move to try is 'c' (but usually not for
 *    defense of a 2 liberty string).
 * 
 *      X.?        Xbc
 *      O..        Oa.
 *      ---        ---
 *
 *    This function adds the points configured like 'b' and 'c' relative to
 *    (si, sj) to the list of moves (movei[], movej[]).
 *
 * color is the color to move.
 */

static void
propose_edge_moves(int si, int sj, int *libi, int *libj, int libs,
		   int movei[MAX_MOVES], int movej[MAX_MOVES], 
		   int scores[MAX_MOVES], int *moves, int to_move)
{
  int color = p[si][sj];
  int other = OTHER_COLOR(color);
  int rightdir = 0;
  int righti, rightj;
  int updir = 0;
  int upi, upj;
  int found_it;
  int ai, aj;
  int k;

  for (k = 0; k < libs; k++) {
    ai = libi[k];
    aj = libj[k];
    
    /* Find out some directions.  We call the directions `rightdir and
     * `updir', as in the figure below, but in reality, they can have
     * any one of the standard eight directons through rotation.
     * rightdir and updir are indices into (deltai[],deltaj[]).
     *
     *     X.?        Xbc
     *     O..        Oa.
     *     ---        ---
     *
     * deltai, deltaj store directions in the order SOUTH, WEST, NORTH, EAST.
     */
    found_it = 0;
    if (ai == 0) {
      /* Top edge */
      if (aj > 0 && p[ai][aj-1] == color) {
	/* Group to the left */
	found_it = 1;
	updir = 0;
	rightdir = 3;
      }
      else if (aj < board_size - 1 && p[ai][aj+1] == color) {
	/* Group to the left */
	found_it = 1;
	updir = 0;
	rightdir = 1;
      }
    }
    else if (ai == board_size - 1) {
      /* Bottom edge */
      if (aj > 0 && p[ai][aj-1] == color) {
	/* Group to the left */
	found_it = 1;
	updir = 2;
	rightdir = 3;
      }
      else if (aj < board_size - 1 && p[ai][aj+1] == color) {
	/* Group to the left */
	found_it = 1;
	updir = 2;
	rightdir = 1;
      }
    }

    if (aj == 0) {
      /* Left edge */
      if (ai > 0 && p[ai-1][aj] == color) {
	/* Group to the top */
	found_it = 1;
	updir = 3;
	rightdir = 0;
      }
      else if (ai < board_size - 1 && p[ai+1][aj] == color) {
	/* Group to the bottom */
	found_it = 1;
	updir = 3;
	rightdir = 2;
      }
    }
    else if (aj == board_size - 1) {
      /* Right edge */
      if (ai > 0 && p[ai-1][aj] == color) {
	/* Group to the top */
	found_it = 1;
	updir = 1;
	rightdir = 0;
      }
      else if (ai < board_size - 1 && p[ai+1][aj] == color) {
	/* Group to the bottom */
	found_it = 1;
	updir = 1;
	rightdir = 2;
      }
    }

    if (!found_it)
      continue;

    upi = deltai[updir];
    upj = deltaj[updir];
    righti = deltai[rightdir];
    rightj = deltaj[rightdir];

    if (p[ai+upi][aj+upj] == other	   /* other on top of liberty */
	&& countlib(ai+upi, aj+upj) > 4    /* blocking group must be secure */
	&& p[si][sj] == to_move)           /* only applicable as defense */
    {
      /* Case 1: other above the liberty (crawl along the edge). */
      int  xi, xj;
	
      xi = ai;
      xj = aj;
      while (ON_BOARD(xi, xj)) {
	if (p[xi][xj] == color
	    || p[xi+upi][xj+upj] == color)
	  break;

	xi += righti;
	xj += rightj;
      }

      /* If no friendly stone found, then it is pointless and we
       * can just as well remove the move.
       */
      if (!ON_BOARD(xi, xj)) {
	REMOVE_CANDIDATE_MOVE(ai, aj, movei, movej, scores, *moves);
      }
    }
    else if (p[ai+upi][aj+upj] == EMPTY           /* empty above the liberty */
	     && p[ai-righti+upi][aj-rightj+upj] == other
	     && ON_BOARD(ai+righti, aj+rightj)    /* c is on board */
	     && p[ai+righti][aj+rightj] == EMPTY) /* empty to the right */
    {
      /* Case 2: Try to escape or contain. */

      /* Add b 
       * If adjacent X stone in atari, boost the initial score of this
       * move.
       */
      if (countlib(ai+upi-righti, aj+upj-rightj) == 1)
	ADD_CANDIDATE_MOVE(ai+upi, aj+upj, 10, movei, movej, scores, *moves);
      else {
	ADD_CANDIDATE_MOVE(ai+upi, aj+upj, 0, movei, movej, scores, *moves);

	/* Add c if empty */
	if (p[ai+righti+upi][aj+rightj+upj] == EMPTY
	    && (libs != 2 || p[si][sj] != to_move))
	  ADD_CANDIDATE_MOVE(ai+righti+upi, aj+rightj+upj, 0,
			     movei, movej, scores, *moves);
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
do_attack(int si, int sj, int *i, int *j, int komaster, int kom_i, int kom_j)
{
  int color = p[si][sj];
  int xi, xj;
  int libs;
  int result = 0;
  int found_read_result;
  Read_result *read_result;

  SETUP_TRACE_INFO("attack", si, sj);

  ASSERT(color != 0, si, sj);

  if (color == 0)      /* if assertions are turned off, silently fails */
    return 0;

  libs = countlib(si, sj);

  if (libs > 4
      || (libs == 4 && stackp > fourlib_depth)) {
    /* No need to cache the result in these cases. */
    SGFTRACE(0, 0, 0, "too many liberties");
    return 0;
  }

  if ((stackp <= depth) && (hashflags & HASH_ATTACK)) {
    found_read_result = get_read_result(ATTACK, komaster, kom_i, kom_j, 
					&si, &sj, &read_result);
    if (found_read_result) {
      TRACE_CACHED_RESULT(*read_result);
      if (rr_get_result(*read_result) != 0) {
	if (i) *i = rr_get_result_i(*read_result);
	if (j) *j = rr_get_result_j(*read_result);
      }

      SGFTRACE(rr_get_result_i(*read_result), rr_get_result_j(*read_result),
	       rr_get_result(*read_result), "cached");
      return rr_get_result(*read_result);
    }

    /* This data should always be recorded. */
    if (read_result) {
      rr_set_compressed_data(*read_result, ATTACK, komaster, kom_i, kom_j, 
			     si, sj, stackp);
    }
  } else
    read_result = NULL;

  /* Treat the attack differently depending on how many liberties the 
     string at (si, sj) has. */
  if (libs == 1)
    result = attack1(si, sj, &xi, &xj, komaster, kom_i, kom_j);
  else if (libs == 2)
    result = attack2(si, sj, &xi, &xj, komaster, kom_i, kom_j);
  else if (libs == 3)
    result = attack3(si, sj, &xi, &xj, komaster, kom_i, kom_j);
  else if (libs == 4)
    result = attack4(si, sj, &xi, &xj, komaster, kom_i, kom_j);

  ASSERT(result >= 0 && result <= 3, si, sj);
  
  if (result)
    READ_RETURN(read_result, i, j, xi, xj, result);

  READ_RETURN0(read_result);
}


/* If (si, sj) points to a group with exactly one liberty, attack1
 * determines whether it can be captured by playing at this liberty.
 * If yes, (*i, *j) is the killing move. i & j may be NULL if caller is
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
attack1(int si, int sj, int *i, int *j, int komaster, int kom_i, int kom_j)
{
  int xi, xj;
  int color = p[si][sj];
  int other = OTHER_COLOR(color);
  int result = -1;
  
  SETUP_TRACE_INFO("attack1", si, sj);
  reading_node_counter++;
  
  /* Pick up the position of the liberty. */
  findlib(si, sj, 1, &xi, &xj);

  /* If the attacked string consists of more than one stone, the
   * attack never fails. (This assumes simple ko rule. With superko
   * rule it could still be a ko violation.)
   */
  if (countstones(si, sj) > 1)
    result = WIN;
  
  /* Try to play on the liberty. This fails if and only if it is an
   * illegal ko capture.
   */
  else if (trymove(xi, xj, other, "attack1-A", si, sj,
		   komaster, kom_i, kom_j)) {
    /* Is the attacker in atari? If not the attack was successful. */
    if (countlib(xi, xj) > 1)
      result = WIN;

    /* If the attacking string is also a single stone, a possible
     * recapture would be a ko violation, so the defender has to make
     * a ko threat first.
     */
    else if (countstones(xi, xj) == 1) {
      /* If the defender is allowed to take the ko the result is KO_A. */
      if (komaster != other)
	result = KO_A;
      else 
	/* But if the attacker is komaster, the attack was successful. */
	result = WIN;
    }
      
    /* Otherwise, do recapture. Notice that the liberty must be
     * at (si, sj) since we have already established that this string
     * was a single stone.
     */
    else if (trymove(si, sj, color, "attack1-B", si, sj,
		     komaster, kom_i, kom_j)) {
      /* If this was a proper snapback, (si, sj) will now have more
       * than one liberty.
       */
      if (countlib(si, sj) > 1) {
	/* Proper snapback, attack fails. */
	RTRACE("SNAPBACK at %m!\n", i, j);
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
    int libi[6];
    int libj[6];
    int k;
    liberties = approxlib(xi, xj, color, 6, libi, libj);
    if (liberties <= 5)
      for (k = 0; k < liberties; k++) {
	int ai = libi[k];
	int aj = libj[k];
	if (!is_self_atari(ai, aj, other)
	    && trymove(ai, aj, other, "attack1-C", si, sj,
		       komaster, kom_i, kom_j)) {
	  int dcode = do_find_defense(si, sj, NULL, NULL, 
				      komaster, kom_i, kom_j);
	  if (dcode != WIN && do_attack(si, sj, NULL, NULL, 
				      komaster, kom_i, kom_j)) {
	    if (dcode == 0) {
	      popgo();
	      SGFTRACE(ai, aj, WIN, "backfilling");
	      if (i) *i = ai;
	      if (j) *j = aj;
	      return WIN;
	    }
	    UPDATE_SAVED_KO_RESULT(result, xi, xj, dcode, ai, aj);
	  }
	  popgo();
	}
      }
  }
  
  if (result > 0) {
    if (i) *i = xi;
    if (j) *j = xj;
    SGFTRACE(xi, xj, result, NULL);
  }
  else {
    SGFTRACE(-1, -1, 0, NULL);
  }
  
  return result;
}


/* If si, sj points to a group with exactly two liberties
 * attack2 determines whether it can be captured in ladder or net.
 * If yes, *i, *j is the killing move. i & j may be null if caller 
 * is only interested in whether it can be captured.
 *  
 * Returns KO_A or KO_B if it can be killed conditioned on ko. Returns
 * KO_A if it can be killed provided (other) is willing to ignore
 * any ko threat. Returns KO_B if (other) wins provided he has a
 * ko threat which must be answered. Can give a return code KO_B 
 * yet (*i,*j)=(-1,-1) if the winning move is an illegal
 * ko capture. In this case, making a ko threat and having it
 * answered should transform the position to one where the return
 * code is KO_A. 
 *
 * See the comment before defend1 about ladders and reading depth.  */

static int 
attack2(int si, int sj, int *i, int *j, int komaster, int kom_i, int kom_j)
{
  int color, other;
  int ai, aj;
  int hi, hj;
  int xi, xj;
  int liberties, r;
  int libi[2], libj[2];
  int lib2i[2], lib2j[2];
  int adj, adji[MAXCHAIN], adjj[MAXCHAIN];
  int savei = -1, savej = -1;
  int savecode = 0;
  int acode;
  int dcode;
  int k;
  int atari_possible = 0;
  int movei[MAX_MOVES], movej[MAX_MOVES];
  int scores[MAX_MOVES];
  int moves = 0;
  int adjacent_liberties = 0;
  int found_read_result;
  Read_result *read_result;

  SETUP_TRACE_INFO("attack2", si, sj);
  reading_node_counter++;

  find_origin(si, sj, &si, &sj);
  gg_assert(p[si][sj] != EMPTY);
  gg_assert(countlib(si, sj) == 2);

  RTRACE("checking attack on %m with 2 liberties\n", si, sj);

  color = p[si][sj];
  other = OTHER_COLOR(color);

  if ((stackp <= depth) && (hashflags & HASH_ATTACK2)) {
  
    found_read_result = get_read_result(ATTACK2, komaster, kom_i, kom_j,
					&si, &sj, &read_result);
    if (found_read_result) {
      TRACE_CACHED_RESULT(*read_result);
      if (rr_get_result(*read_result) != 0) {
	if (i) *i = rr_get_result_i(*read_result);
	if (j) *j = rr_get_result_j(*read_result);
      }

      SGFTRACE(rr_get_result_i(*read_result), rr_get_result_j(*read_result),
	       rr_get_result(*read_result), "cached");
      return rr_get_result(*read_result);
    }

    /* This data should always be recorded. */
    if (read_result) {
      rr_set_compressed_data(*read_result, ATTACK2, komaster, kom_i, kom_j, 
			     si, sj, stackp);
    }
  } else
    read_result = NULL;

  /* The attack may fail if a boundary string is in atari and cannot 
   * be defended.  First we must try defending such a string. 
   *
   * We start by trying to defend the boundary string by looking for an
   * adjacent string which is in atari. 
   */
  adj = chainlinks2(si, sj, adji, adjj, 1);
  for (r = 0; r < adj; r++) {
    /* if stackp > depth and any boundary chain is in atari, assume safe.
     * However, if the captured chain is only of size 1, there can still
     * be a working ladder, so continue if that is the case.
     */
    if (stackp > depth && countstones(adji[r], adjj[r]) > 1) {
      SGFTRACE(-1, -1, 0, "boundary in atari");
      READ_RETURN0(read_result);
    }

    /* Pick up moves breaking the second order chain. */
    if (stackp <= depth)
      break_chain_moves(adji[r], adjj[r], movei, movej, scores, &moves);
    
    findlib(adji[r], adjj[r], 1, &hi, &hj);
    ADD_CANDIDATE_MOVE(hi, hj, 0, movei, movej, scores, moves);
  }

  /* Get the two liberties of (si, sj). */
  liberties = findlib(si, sj, 2, libi, libj);
  ASSERT(liberties == 2, si, sj);

  {
    int di = libi[1] - libi[0];
    int dj = libj[1] - libj[0];
    if (di < 0)
      di = -di;
    if (dj < 0)
      dj = -dj;
    if (di + dj == 1)
      adjacent_liberties = 1;
  }
  
  for (k = 0; k < 2; k++) {
    ai = libi[k];
    aj = libj[k];
    if (!is_self_atari(ai, aj, other))
      atari_possible = 1;
    /* we only want to consider the move at (ai,aj) if:
     * stackp <= backfill_depth
     * -or-  stackp <= depth and it is an isolated stone
     * -or-  it is not in immediate atari
     */
    if ((stackp <= backfill_depth) 
	|| ((stackp <= depth || adjacent_liberties) 
	    && ((ai == 0) || (p[ai-1][aj] != other))
	    && ((ai == board_size-1) || (p[ai+1][aj] != other))
	    && ((aj == 0) || (p[ai][aj-1] != other))
	    && ((aj == board_size-1) || (p[ai][aj+1] != other)))
	|| !is_self_atari(ai, aj, other))
      ADD_CANDIDATE_MOVE(ai, aj, 0, movei, movej, scores, moves);

    /* Try backfilling if atari is impossible. */
    if (stackp <= backfill_depth
	&& approxlib(ai, aj, other, 2, lib2i, lib2j) == 1) {
      int ui = lib2i[0];
      int uj = lib2j[0];
      ADD_CANDIDATE_MOVE(ui, uj, 0, movei, movej, scores, moves);
    }
  }

  /* If we can't make a direct atari, look for edge blocking moves. */
  if (!atari_possible)
    for (k = 0; k < 2; k++) 
      edge_block(si, sj, libi[k], libj[k], movei, movej, scores, &moves);
    

  /* If one of the surrounding chains have only two liberties, which
   * coincide with the liberties of the attacked string, we try to
   * backcapture.
   */
  
  adj = chainlinks2(si, sj, adji, adjj, 2);
  for (r = 0; r<adj; r++) {
    ai = adji[r];
    aj = adjj[r];
    if (liberty_of_string(libi[0], libj[0], ai, aj)
	&& liberty_of_string(libi[1], libj[1], ai, aj))
      break_chain_moves(ai, aj, movei, movej, scores, &moves);
  }
  
  propose_edge_moves(si, sj, libi, libj, liberties, movei, movej, scores,
		     &moves, other);
  order_moves(si, sj, moves, movei, movej, scores, other, read_function_name);

  for (k = 0; k < moves; k++) {
    int new_komaster, new_kom_i, new_kom_j;
    int ko_move;

    ai = movei[k];
    aj = movej[k];
    if (komaster_trymove(ai, aj, other, "attack2-A", si, sj,
			 komaster, kom_i, kom_j,
			 &new_komaster, &new_kom_i, &new_kom_j,
			 &ko_move, stackp <= ko_depth && savecode == 0)) {
      if (!ko_move) {
	dcode = do_find_defense(si, sj, NULL, NULL,
				new_komaster, new_kom_i, new_kom_j);
	if (dcode != WIN
	    && do_attack(si, sj, NULL, NULL,
			 new_komaster, new_kom_i, new_kom_j)) {
	  if (dcode == 0) {
	    popgo();
	    SGFTRACE(ai, aj, WIN, "attack effective");
	    READ_RETURN(read_result, i, j, ai, aj, WIN);
	  }
	  UPDATE_SAVED_KO_RESULT(savecode, savei, savej, dcode, ai, aj);
	}
      }
      else {
	if (do_find_defense(si, sj, NULL, NULL,
			    new_komaster, new_kom_i, new_kom_j) != WIN
	    && do_attack(si, sj, NULL, NULL,
			 new_komaster, new_kom_i, new_kom_j) != 0) {
	  savei = ai;
	  savej = aj;
	  savecode = KO_B;
	}
      }
      popgo();
    }
  }
  
  /* The simple ataris didn't work. Try something more fancy. */
  acode = find_cap2(si, sj, libi[0], libj[0], libi[1], libj[1], &xi, &xj,
		    komaster, kom_i, kom_j);
  if (acode == WIN) {
    SGFTRACE(xi, xj, WIN, "find cap2");
    READ_RETURN(read_result, i, j, xi, xj, WIN);
  }
  UPDATE_SAVED_KO_RESULT_UNREVERSED(savecode, savei, savej, acode, xi, xj);

  if (stackp <= backfill_depth) {
    acode = special_attack2(si, sj, libi, libj, &xi, &xj, 
			    komaster, kom_i, kom_j);
    if (acode == WIN) {
      SGFTRACE(xi, xj, WIN, "special attack2");
      READ_RETURN(read_result, i, j, xi, xj, WIN);
    }
    UPDATE_SAVED_KO_RESULT_UNREVERSED(savecode, savei, savej, acode, xi, xj);
  }

  if (stackp <= backfill_depth) {
    acode = special_attack3(si, sj, libi, libj, &xi, &xj, 
			    komaster, kom_i, kom_j);
    if (acode == WIN) {
      SGFTRACE(xi, xj, WIN, "special attack3");
      READ_RETURN(read_result, i, j, xi, xj, WIN);
    }
    UPDATE_SAVED_KO_RESULT_UNREVERSED(savecode, savei, savej, acode, xi, xj);
  }

  if (stackp <= backfill_depth) {
    acode = special_attack4(si, sj, libi, libj, &xi, &xj, 
			    komaster, kom_i, kom_j);
    if (acode == WIN) {
      SGFTRACE(xi, xj, WIN, "special attack4");
      READ_RETURN(read_result, i, j, xi, xj, WIN);
    }
    UPDATE_SAVED_KO_RESULT_UNREVERSED(savecode, savei, savej, acode, xi, xj);
  }

  /* If it is not possible to make a direct atari, we try filling
   * a liberty of the superstring.
   */
  if (level >= 10
      && stackp <= backfill_depth
      && (stackp <= superstring_depth || !atari_possible)) {
    int libs;
    int libi[MAX_LIBERTIES + 4], libj[MAX_LIBERTIES + 4];
    int liberty_cap = 2;

    if (stackp <= backfill2_depth)
      liberty_cap = 3;
    
    find_superstring_liberties(si, sj, &libs, libi, libj, liberty_cap);
    if (libs <= 5) {
      for (k = 0; k < libs; k++) {
	int ai = libi[k];
	int aj = libj[k];
	
	if (liberty_of_string(ai, aj, si, sj))
	  continue;
	if (trymove(ai, aj, other, "attack2-C", si, sj, 
		    komaster, kom_i, kom_j)) {
	  if (countlib(ai, aj) == 1) {
	    /* can't atari, try backfilling. */
#if 0
	    if (restricted_defend1(ai, aj, &xi, &xj, komaster, kom_i, kom_j,
				   libs, libi, libj)) {
#else
	    findlib(ai, aj, 1, &xi, &xj);
	    if (approxlib(xi, xj, other, 2, NULL, NULL) > 1) {
#endif
	      popgo();
	      if (trymove(xi, xj, other, "attack2-D", si, sj,
			  komaster, kom_i, kom_j)) {
		dcode = do_find_defense(si, sj, NULL, NULL, 
					komaster, kom_i, kom_j);
		if (dcode != WIN && do_attack(si, sj, NULL, NULL, 
					    komaster, kom_i, kom_j)) {
		  if (dcode == 0) {
		    popgo();
		    SGFTRACE(xi, xj, WIN, "attack effective");
		    READ_RETURN(read_result, i, j, xi, xj, WIN);
		  }
		  UPDATE_SAVED_KO_RESULT(savecode, savei, savej, dcode,
					 xi, xj);
		}
		popgo();
	      }
	    }
	    else
	      popgo();
	  }
	  else {
	    dcode = do_find_defense(si, sj, NULL, NULL, 
				    komaster, kom_i, kom_j);
	    if (dcode != WIN 
		&& do_attack(si, sj, NULL, NULL, komaster, kom_i, kom_j)) {
	      if (dcode == 0) {
		popgo();
		SGFTRACE(ai, aj, WIN, "attack effective");
		READ_RETURN(read_result, i, j, ai, aj, WIN);
	      }
	      UPDATE_SAVED_KO_RESULT(savecode, savei, savej, dcode, ai, aj);
	    }
	    popgo();
	  }
	}
      }
    }
  }

  if (savecode == 0) {
    RTRACE("ALIVE!!\n");
    SGFTRACE(-1, -1, 0, NULL);
    READ_RETURN0(read_result);
  }

  SGFTRACE(savei, savej, savecode, "saved move");
  READ_RETURN(read_result, i, j, savei, savej, savecode);
}



/* attack3(si, sj, *i, *j) is used when (si, sj) points to a group with
 * three liberties. It returns true if it finds a way to kill the group.
 *
 * Return code is KO_A if the group can be killed if the attacker is 
 * willing to ignore any ko threat.
 *
 * Return code is KO_B if the group can be killed if the attacker is 
 * able to find a ko threat which must be answered.
 *
 * If non-NULL (*i, *j) will be set to the move which makes the
 * attack succeed.
 */

static int 
attack3(int si, int sj, int *i, int *j, int komaster, int kom_i, int kom_j)
{
  int color = p[si][sj];
  int other = OTHER_COLOR(color);
  int adj, adji[MAXCHAIN], adjj[MAXCHAIN];
  int xi, xj;
  int liberties;
  int libi[3], libj[3];
  int r;
  int dcode = 0;
  int k;
  int movei[MAX_MOVES], movej[MAX_MOVES];
  int scores[MAX_MOVES];
  int moves = 0;
  int savei = -1, savej = -1;
  int savecode = 0;
  int found_read_result;
  Read_result *read_result;
  
  SETUP_TRACE_INFO("attack3", si, sj);
  reading_node_counter++;
  
  gg_assert(p[si][sj] != EMPTY);
  
  if ((stackp <= depth) && (hashflags & HASH_ATTACK3)) {
    found_read_result = get_read_result(ATTACK3, komaster, kom_i, kom_j,
					&si, &sj, &read_result);
    if (found_read_result) {
      TRACE_CACHED_RESULT(*read_result);
      if (rr_get_result(*read_result) != 0) {
	if (i) *i = rr_get_result_i(*read_result);
	if (j) *j = rr_get_result_j(*read_result);
      }
      
      SGFTRACE(rr_get_result_i(*read_result), rr_get_result_j(*read_result),
	       rr_get_result(*read_result), "cached");
      return rr_get_result(*read_result);
    }
    
    /* This data should always be recorded. */
    if (read_result) {
      rr_set_compressed_data(*read_result, ATTACK3, komaster, kom_i, kom_j, 
			     si, sj, stackp);
    }
  } else
    read_result = NULL;
  
  if (stackp > depth) {
    SGFTRACE(-1, -1, 0, "stackp > depth");
    READ_RETURN0(read_result);
  }
  
  adj = chainlinks2(si, sj, adji, adjj, 1);
  for (r = 0; r < adj; r++) {
    int hi, hj;
    break_chain_moves(adji[r], adjj[r], movei, movej, scores, &moves);
    
    findlib(adji[r], adjj[r], 1, &hi, &hj);
    ADD_CANDIDATE_MOVE(hi, hj, 0, movei, movej, scores, moves);
  }
  
  /* Defend against double atari in the surrounding chain early. */
  double_atari_chain2(si, sj, movei, movej, scores, &moves);
  
  /* Get the three liberties of (si, sj). */
  liberties = findlib(si, sj, 3, libi, libj);
  ASSERT(liberties == 3, si, sj);
  
  for (k = 0; k < 3; k++) {
#if 0
    int lib2i[2];
    int lib2j[2];
#endif
    int ai = libi[k];
    int aj = libj[k];
    /* we only want to consider the move at (ai,aj) if:
     * stackp <= backfill_depth
     * -or-  stackp <= depth and it is an isolated stone
     * -or-  it is not in immediate atari
     */
    if ((stackp <= backfill_depth) 
	|| ((stackp <= depth) 
	    && ((ai == 0) || (p[ai-1][aj] != other))
	    && ((ai == board_size-1) || (p[ai+1][aj] != other))
	    && ((aj == 0) || (p[ai][aj-1] != other))
	    && ((aj == board_size-1) || (p[ai][aj+1] != other)))
	|| !is_self_atari(ai, aj, other))
      ADD_CANDIDATE_MOVE(ai, aj, 0, movei, movej, scores, moves);

    if (edge_closing_backfill(si, sj, ai, aj, &xi, &xj))
      ADD_CANDIDATE_MOVE(xi, xj, 0, movei, movej, scores, moves);

#if 0
    /* Try backfilling if atari is impossible. */
    if (stackp <= backfill_depth
	&& approxlib(ai, aj, other, 2, lib2i, lib2j) == 1) {
      int ui = lib2i[0];
      int uj = lib2j[0];
      ADD_CANDIDATE_MOVE(ui, uj, 0, movei, movej, scores, moves);
    }
#endif
    
    /* Look for edge blocking moves. */
    edge_block(si, sj, ai, aj, movei, movej, scores, &moves);
  }
  
  /* Pick up some edge moves. */
  propose_edge_moves(si, sj, libi, libj, liberties, movei, movej, scores,
		     &moves, other);
  order_moves(si, sj, moves, movei, movej, scores, other, read_function_name);

  /* Try the moves collected so far. */
  for (k = 0; k < moves; k++) {
    int new_komaster, new_kom_i, new_kom_j;
    int ko_move;
    
    if (stackp >= branch_depth && k > 0)
      break;
    xi = movei[k];
    xj = movej[k];
    if (komaster_trymove(xi, xj, other, "attack3-A", si, sj,
			 komaster, kom_i, kom_j,
			 &new_komaster, &new_kom_i, &new_kom_j,
			 &ko_move, stackp <= ko_depth && savecode == 0)) {
      if (!ko_move) {
	dcode = do_find_defense(si, sj, NULL, NULL,
				new_komaster, new_kom_i, new_kom_j);
	if (dcode != WIN
	    && do_attack(si, sj, NULL, NULL,
			 new_komaster, new_kom_i, new_kom_j)) {
	  if (dcode == 0) {
	    popgo();
	    SGFTRACE(xi, xj, WIN, "attack effective");
	    READ_RETURN(read_result, i, j, xi, xj, WIN);
	  }
	  UPDATE_SAVED_KO_RESULT(savecode, savei, savej, dcode, xi, xj);
	}
      }
      else {
	if (do_find_defense(si, sj, NULL, NULL,
			    new_komaster, new_kom_i, new_kom_j) != WIN
	    && do_attack(si, sj, NULL, NULL,
			 new_komaster, new_kom_i, new_kom_j) != 0) {
	  savei = xi;
	  savej = xj;
	  savecode = KO_B;
	}
      }
      popgo();
    }
  }
    
  /* The simple ataris didn't work. Try something more fancy. */
  if (stackp <= backfill_depth) {
    int acode = find_cap3(si, sj, &xi, &xj, komaster, kom_i, kom_j);
    if (acode == WIN) {
      SGFTRACE(xi, xj, WIN, "find cap3");
      READ_RETURN(read_result, i, j, xi, xj, WIN);
    }
    UPDATE_SAVED_KO_RESULT_UNREVERSED(savecode, savei, savej, acode, xi, xj);
  }

  if (stackp <= fourlib_depth) {
    int acode = draw_back(si, sj, &xi, &xj, komaster, kom_i, kom_j);
    if (acode == WIN) {
      SGFTRACE(xi, xj, WIN, "draw back");
      READ_RETURN(read_result, i, j, xi, xj, WIN);
    }
    UPDATE_SAVED_KO_RESULT_UNREVERSED(savecode, savei, savej, acode, xi, xj);
  }

  /* Try to defend chain links with two liberties. */
  if (stackp <= backfill2_depth) {
    int saved_moves = moves;
    adj = chainlinks2(si, sj, adji, adjj, 2);
    for (r = 0; r < adj; r++) {
      int lib2i[2], lib2j[2];
      findlib(adji[r], adjj[r], 2, lib2i, lib2j);
      if (approxlib(lib2i[0], lib2j[0], other, 4, NULL, NULL) > 3
	  && approxlib(lib2i[1], lib2j[1], other, 4, NULL, NULL) > 3)
	continue;
      break_chain_moves(adji[r], adjj[r], movei, movej, scores, &moves);
      break_chain2_moves(adji[r], adjj[r], movei, movej, scores, &moves, 1);
      for (k = 0; k < 2; k++)
	ADD_CANDIDATE_MOVE(lib2i[k], lib2j[k], 0,
			   movei, movej, scores, moves);
    }
    /* Only order and test the new set of moves. */
    order_moves(si, sj, moves-saved_moves,
		&(movei[saved_moves]), &(movej[saved_moves]),
		&(scores[saved_moves]), other, read_function_name);
    for (k = saved_moves; k < moves; k++) {
      int new_komaster, new_kom_i, new_kom_j;
      int ko_move;

      if (stackp >= branch_depth && k > 0)
	break;
      xi = movei[k];
      xj = movej[k];
      
      if (komaster_trymove(xi, xj, other, "attack3-C", si, sj,
			   komaster, kom_i, kom_j,
			   &new_komaster, &new_kom_i, &new_kom_j,
			   &ko_move, stackp <= ko_depth && savecode == 0)) {
	if (!ko_move) {
	  dcode = do_find_defense(si, sj, NULL, NULL,
				  new_komaster, new_kom_i, new_kom_j);
	  if (dcode != WIN
	      && do_attack(si, sj, NULL, NULL,
			   new_komaster, new_kom_i, new_kom_j)) {
	    if (dcode == 0) {
	      popgo();
	      SGFTRACE(xi, xj, WIN, "attack effective");
	      READ_RETURN(read_result, i, j, xi, xj, WIN);
	    }
	    UPDATE_SAVED_KO_RESULT(savecode, savei, savej, dcode, xi, xj);
	  }
	}
      else {
	if (do_find_defense(si, sj, NULL, NULL,
			    new_komaster, new_kom_i, new_kom_j) != WIN
	    && do_attack(si, sj, NULL, NULL,
			 new_komaster, new_kom_i, new_kom_j) != 0) {
	    savei = xi;
	    savej = xj;
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
    int libs;
    int libi[MAX_LIBERTIES + 4], libj[MAX_LIBERTIES + 4];

    find_superstring_liberties(si, sj, &libs, libi, libj, 3);
    if (libs <= 5) {
      for (k = 0; k < libs; k++) {
	int ai = libi[k];
	int aj = libj[k];
	
	if (liberty_of_string(ai, aj, si, sj))
	  continue;
	if (trymove(ai, aj, other, "attack3-E", si, sj,
		    komaster, kom_i, kom_j)) {
	  if (countlib(ai, aj) == 1) {
	    /* can't atari, try backfilling */
#if 0
	    if (restricted_defend1(ai, aj, &xi, &xj, komaster, kom_i, kom_j,
				   libs, libi, libj)) {
#else
	    findlib(ai, aj, 1, &xi, &xj);
	    if (approxlib(xi, xj, other, 2, NULL, NULL) > 1) {
#endif
	      popgo();
	      if (trymove(xi, xj, other, "attack3-F", si, sj,
			  komaster, kom_i, kom_j)) {
		dcode = do_find_defense(si, sj, NULL, NULL, 
					komaster, kom_i, kom_j);
		if (dcode != WIN && do_attack(si, sj, NULL, NULL, 
					    komaster, kom_i, kom_j)) {
		  if (dcode == 0) {
		    popgo();
		    SGFTRACE(xi, xj, WIN, "attack effective");
		    READ_RETURN(read_result, i, j, xi, xj, WIN);
		  }
		  UPDATE_SAVED_KO_RESULT(savecode, savei, savej, dcode,
					 xi, xj);
		}
		popgo();
	      }
	    }
	    else
	      popgo();
	  }
	  else {
	    dcode = do_find_defense(si, sj, NULL, NULL, 
				    komaster, kom_i, kom_j);
	    if (dcode != WIN && do_attack(si, sj, NULL, NULL, 
					komaster, kom_i, kom_j)) {
	      if (dcode == 0) {
		popgo();
		SGFTRACE(ai, aj, WIN, "attack effective");
		READ_RETURN(read_result, i, j, ai, aj, WIN);
	      }
	      UPDATE_SAVED_KO_RESULT(savecode, savei, savej, dcode, ai, aj);
	    }
	    popgo();
	  }
	}
      }
    }
  }

  if (savecode != 0) {
    SGFTRACE(savei, savej, savecode, "saved move");
    READ_RETURN(read_result, i, j, savei, savej, savecode);
  }
  
  SGFTRACE(-1, -1, 0, NULL);
  READ_RETURN0(read_result);
}


/* attack4 tries to capture a string with 4 liberties. This function
 * is not cached.
 */

static int 
attack4(int si, int sj, int *i, int *j, int komaster, int kom_i, int kom_j)
{
  int color = p[si][sj];
  int other = OTHER_COLOR(color);
  int xi, xj;
  int r;
  int k;
  int liberties;
  int libi[4], libj[4];
  int adj, adji[MAXCHAIN], adjj[MAXCHAIN];
  int dcode = 0;
  int movei[MAX_MOVES], movej[MAX_MOVES];
  int scores[MAX_MOVES];
  int moves = 0;
  Read_result *read_result = NULL;
  int savecode = 0;
  int savei = -1, savej = -1;

  SETUP_TRACE_INFO("attack4", si, sj);
  
  gg_assert(p[si][sj] != EMPTY);
  reading_node_counter++;
  
  if (stackp > depth) {
    SGFTRACE(-1, -1, 0, "stackp > depth");
    return 0;
  }

  adj = chainlinks2(si, sj, adji, adjj, 1);
  for (r = 0; r < adj; r++) {
    int hi, hj;
    break_chain_moves(adji[r], adjj[r], movei, movej, scores, &moves);
    
    findlib(adji[r], adjj[r], 1, &hi, &hj);
    ADD_CANDIDATE_MOVE(hi, hj, 0, movei, movej, scores, moves);
  }


  /* Defend against double atari in the surrounding chain early. */
  double_atari_chain2(si, sj, movei, movej, scores, &moves);

  /* Give a score bonus to the chain preserving moves. */
  for (k = 0; k < moves; k++)
    scores[k] += 5;
  
  /* Get the four liberties of (si, sj). */
  liberties = findlib(si, sj, 4, libi, libj);
  ASSERT(liberties == 4, si, sj);

  for (k = 0; k < 4; k++) {
    int ai = libi[k];
    int aj = libj[k];
    /* we only want to consider the move at (ai,aj) if:
     * stackp <= backfill_depth
     * -or-  stackp <= depth and it is an isolated stone
     * -or-  it is not in immediate atari
     */
    if ((stackp <= backfill_depth) 
	|| (stackp <= depth
	    && (ai == 0 || p[ai-1][aj] != other)
	    && (ai == board_size-1 || p[ai+1][aj] != other)
	    && (aj == 0 || p[ai][aj-1] != other)
	    && (aj == board_size-1 || p[ai][aj+1] != other))
	|| !is_self_atari(ai, aj, other))
      ADD_CANDIDATE_MOVE(ai, aj, 0, movei, movej, scores, moves);

    if (edge_closing_backfill(si, sj, ai, aj, &xi, &xj))
      ADD_CANDIDATE_MOVE(xi, xj, 10, movei, movej, scores, moves);

    /* Look for edge blocking moves. */
    edge_block(si, sj, ai, aj, movei, movej, scores, &moves);
  }

  /* Pick up some edge moves. */
  propose_edge_moves(si, sj, libi, libj, liberties, movei, movej, scores,
		     &moves, other);
  order_moves(si, sj, moves, movei, movej, scores, other, read_function_name);

  /* Try the moves collected so far. */
  for (k = 0; k < moves; k++) {
    int new_komaster, new_kom_i, new_kom_j;
    int ko_move;

    if (stackp >= branch_depth && k > 0)
      break;
    xi = movei[k];
    xj = movej[k];
    /* Conditional ko capture is disabled because it seems to expensive. */
    if (komaster_trymove(xi, xj, other, "attack4-A", si, sj,
			 komaster, kom_i, kom_j,
			 &new_komaster, &new_kom_i, &new_kom_j,
			 &ko_move, 0 && stackp <= ko_depth && savecode == 0)) {
      if (!ko_move) {
	dcode = do_find_defense(si, sj, NULL, NULL,
				new_komaster, new_kom_i, new_kom_j);
	if (dcode != WIN
	    && do_attack(si, sj, NULL, NULL,
			 new_komaster, new_kom_i, new_kom_j)) {
	  if (dcode == 0) {
	    popgo();
	    SGFTRACE(xi, xj, WIN, "attack effective");
	    READ_RETURN(read_result, i, j, xi, xj, WIN);
	  }
	  UPDATE_SAVED_KO_RESULT(savecode, savei, savej, dcode, xi, xj);
	}
      }
      else {
	if (do_find_defense(si, sj, NULL, NULL,
			    new_komaster, new_kom_i, new_kom_j) != WIN
	    && do_attack(si, sj, NULL, NULL,
			 new_komaster, new_kom_i, new_kom_j) != 0) {
	  savei = xi;
	  savej = xj;
	  savecode = KO_B;
	}
      }
      popgo();
    }
  }

  if (savecode != 0) {
    SGFTRACE(savei, savej, savecode, "saved move");
    if (i) *i = savei;
    if (j) *j = savej;
    return savecode;
  }

  SGFTRACE(-1, -1, 0, NULL);
  return 0;
}


/* If (si, sj) points to a string with 2 or 3 liberties,
 * find_cap2(si, sj, ai, aj, bi, bj, &i, &j, komaster)
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
find_cap2(int si, int sj, int ai, int aj, int bi, int bj, int *i, int *j,
	  int komaster, int kom_i, int kom_j)
{
  int ti = -1, tj = -1;

  /* Check if the two liberties are located like the figure above. */
  if ((ai == bi+1 || ai == bi-1)
      && (aj == bj+1 || aj == bj-1))
  {
    /* Which of the two corner points should we use? One of them is 
     * always occupied by the string at (m, n), the other one is either
     * free or occupied by something else.
     */
    if (p[bi][aj] == EMPTY) {
      ti = bi;
      tj = aj;
    }
    if (p[ai][bj] == EMPTY) {
      ti = ai;
      tj = bj;
    }

    /* If we didn't find a free intersection, we couldn't make the move. */
    if (ti == -1)
      return 0;

    /* Ok, we found the spot. Now see if the move works. */
    RTRACE("trying to capture %m with capping move at %m\n", si, sj, ti, tj);
    if (trymove(ti, tj, OTHER_COLOR(p[si][sj]),"find_cap2", si, sj,
		komaster, kom_i, kom_j)) {
      int dcode = do_find_defense(si, sj, NULL, NULL, komaster, kom_i, kom_j);
      popgo();
      if (dcode != WIN) {
	if (i) *i = ti;
	if (j) *j = tj;
      }
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
  }

  return 0;
}    


/* If (si,sj) points to a string with 3 liberties, find_cap3(si,sj,&i,&j)
 * looks for a configuration of the following type:
 *
 *  XXa
 *  cb*
 *
 * where X are elements of the string in question and a, b and c are
 * its liberties. It tries the move at * and returns true this move
 * captures the string, leaving (i,j) pointing to *.
 */

static int
find_cap3(int si, int sj, int *i, int *j, int komaster, int kom_i, int kom_j)
{
  int ai, aj, bi, bj;
  int libi[3], libj[3];
  int xi = -1, xj = -1;
  int k;
  int savecode = 0;
  int savei = -1;
  int savej = -1;
  int acode;

  if (findlib(si, sj, 3, libi, libj) != 3)
    return 0;

  for (k = 0; k < 3; ++k) {
    /* k and k+1 mod 3 will be (0,1), (1,2) and (2,0); These are the 
     * three combinations of indices that we have to send to find_cap2.
     */
    ai = libi[k];
    aj = libj[k];
    bi = libi[(k+1)%3];
    bj = libj[(k+1)%3];

    acode = find_cap2(si, sj, ai, aj, bi, bj, &xi, &xj, 
		      komaster, kom_i, kom_j);
    if (acode == WIN) {
      if (i) *i = xi;
      if (j) *j = xj;
      return WIN;
    }
    UPDATE_SAVED_KO_RESULT_UNREVERSED(savecode, savei, savej, acode, xi, xj);
  }

  if (savecode != 0) {
    if (i) *i = savei;
    if (j) *j = savej;
  }
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
special_attack2(int si, int sj, int libi[2], int libj[2], 
		int *ti, int *tj, int komaster, int kom_i, int kom_j)
{
  int color = p[si][sj];
  int other = OTHER_COLOR(color);
  int savecode = 0;
  int newlibi[3], newlibj[3];
  int xi, xj;
  int savei = -1, savej = -1;
  int k;

  for (k = 0; k < 2; ++k) {
    if (is_suicide(libi[k], libj[k], other) 
	&& (approxlib(libi[k], libj[k], color, 3, newlibi, newlibj) == 2)) {
      if (newlibi[0] != libi[1-k] || newlibj[0] != libj[1-k]) {
	xi = newlibi[0];
	xj = newlibj[0];
      } else {
	xi = newlibi[1];
	xj = newlibj[1];
      }

      if (!is_self_atari(xi, xj, other)
	  && trymove(xi, xj, other, "special_attack2", si, sj,
		     komaster, kom_i, kom_j)) {
	int dcode = do_find_defense(si, sj, NULL, NULL, 
				    komaster, kom_i, kom_j);
	if (dcode != WIN 
	    && do_attack(si, sj, NULL, NULL, komaster, kom_i, kom_j)) {
	  if (dcode == 0) {
	    popgo();
	    if (ti) *ti = xi;
	    if (tj) *tj = xj;
	    return WIN;
	  }
	  UPDATE_SAVED_KO_RESULT(savecode, savei, savej, dcode, xi, xj);
	}
	popgo();
      }
    }
  }

  if (savecode) {
    if (ti) *ti = savei;
    if (tj) *tj = savej;
    return savecode;
  }

  return 0;
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
 * the code that follows can find the attacking move at 'c=(xi,xj)'.
 */

static int
special_attack3(int si, int sj, int libi[2], int libj[2], 
		int *ti, int *tj, int komaster, int kom_i, int kom_j)
{
  int color = p[si][sj];
  int other = OTHER_COLOR(color);
  int acode;
  int dcode;
  int savecode = 0;
  int savei = -1, savej = -1;
  int newlibi[2], newlibj[2];
  int xi, xj;
  int yi, yj;
  int ai, aj;
  int bi, bj;
  int k;

  gg_assert(countlib(si, sj) == 2);

  for (k = 0; k < 2; ++k) {
    ai = libi[k];
    aj = libj[k];
    bi = libi[1-k];
    bj = libj[1-k];

    if (ai == bi && (aj == bj+1 || aj == bj-1)) {
      xj = aj;
      if (ai < board_size-1 && p[ai+1][aj] == EMPTY)
	xi = ai+1;
      else if (ai > 0 && p[ai-1][aj] == EMPTY)
	xi = ai-1;
      else
	continue;
    }
    else if (aj == bj && (ai == bi+1 || ai == bi-1)) {
      xi = ai;
      if (aj < board_size-1 && p[ai][aj+1] == EMPTY)
	xj = aj+1;
      else if (aj > 0 && p[ai][aj-1] == EMPTY)
	xj = aj-1;
      else
	continue;
    }
    else
      return 0; /* Incorrect configuration, give up. */
      
    if (is_self_atari(xi, xj, other)
	|| !trymove(xi, xj, other, "special_attack3-A", si, sj,
		    komaster, kom_i, kom_j))
      continue;
    
    if (countlib(xi, xj) == 2) {
      findlib(xi, xj, 2, newlibi, newlibj);
      if (newlibi[0] == ai && newlibj[0] == aj) {
	yi = newlibi[1];
	yj = newlibj[1];
      }
      else {
	yi = newlibi[0];
	yj = newlibj[0];
      }
      if (!is_self_atari(yi, yj, color)
	  && trymove(yi, yj, color, "special_attack3-B", si, sj,
		     komaster, kom_i, kom_j)) {
	acode = do_attack(si, sj, NULL, NULL, komaster, kom_i, kom_j);
	if (acode == 0) {
	  popgo();
	  popgo();
	  continue;
	}
	UPDATE_SAVED_KO_RESULT_UNREVERSED(savecode, savei, savej,
					  acode, xi, xj);
	popgo();
      }
    }
    
    dcode = do_find_defense(si, sj, NULL, NULL, komaster, kom_i, kom_j);
    if (dcode != WIN && do_attack(si, sj, NULL, NULL, komaster, kom_i, kom_j)) {
      if (dcode == 0) {
	popgo();
	if (ti) *ti = xi;
	if (tj) *tj = xj;
	return WIN;
      }
      UPDATE_SAVED_KO_RESULT(savecode, savei, savej, dcode, xi, xj);
    }
    popgo();
  }

  if (savecode) {
    if (ti) *ti = savei;
    if (tj) *tj = savej;
    return savecode;
  }

  return 0;
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
special_attack4(int si, int sj, int libi[2], int libj[2],
		int *ti, int *tj, int komaster, int kom_i, int kom_j)
{
  int color = p[si][sj];
  int other = OTHER_COLOR(color);
  int dcode;
  int savecode = 0;
  int savei = -1, savej = -1;
  int adj, adji[MAXCHAIN], adjj[MAXCHAIN];
  int adj2, adj2i[MAXCHAIN], adj2j[MAXCHAIN];
  int lib2i[2], lib2j[2];
  int ai, aj;
  int bi = -1, bj = -1;
  int ci, cj;
  int di, dj;
  int ei, ej;
  int k, s, t;

  gg_assert(countlib(si, sj) == 2);

  /* To avoid making this too general, we require that both
   * liberties are self ataris for X.
   */
  if (!is_self_atari(libi[0], libj[0], other) 
      || !is_self_atari(libi[1], libj[1], other))
    return 0;

  /* Pick up chain links with 2 liberties. */
  adj = chainlinks2(si, sj, adji, adjj, 2);
  
  for (k = 0; k < 2; ++k) {
    ai = libi[k];
    aj = libj[k];

    /* Check that (ai, aj) also is a liberty of one of the two liberty
     * chain links.
     */
    for (s = 0; s < adj; s++)
      if (liberty_of_string(ai, aj, adji[s], adjj[s])) {
	bi = adji[s];
	bj = adjj[s];
	break;
      }

    /* Nothing found. */
    if (s == adj)
      continue;

    /* Now require that (bi, bj) has a chain link, different from (si,
     * sj), also with two liberties.
     */
    adj2 = chainlinks2(bi, bj, adj2i, adj2j, 2);

    for (s = 0; s < adj2; s++) {
      find_origin(adj2i[s], adj2j[s], &ci, &cj);
      if (ci == si && cj == sj)
	continue;
      
      /* Pick up the liberties of (ci, cj). */
      findlib(ci, cj, 2, lib2i, lib2j);

      /* Try playing at a liberty. Before doing this, verify that (ci,
       * cj) cannot get more than two liberties by answering on the
       * other liberty and that we are not putting ourselves in atari.
       */
      for (t = 0; t < 2; t++) {
	di = lib2i[t];
	dj = lib2j[t];
	ei = lib2i[1-t];
	ej = lib2j[1-t];
	if (is_self_atari(di, dj, other))
	  break;
	if (approxlib(ei, ej, color, 4, NULL, NULL) > 3)
	  break;

	if (trymove(di, dj, other, "special_attack4", si, sj,
		    komaster, kom_i, kom_j)) {
	  dcode = do_find_defense(si, sj, NULL, NULL, komaster, kom_i, kom_j);
	  if (dcode != WIN && do_attack(si, sj, NULL, NULL, 
				      komaster, kom_i, kom_j)) {
	    if (dcode == 0) {
	      popgo();
	      if (ti) *ti = di;
	      if (tj) *tj = dj;
	      return WIN;
	    }
	    UPDATE_SAVED_KO_RESULT(savecode, savei, savej, dcode, di, dj);
	  }
	  popgo();
	}
      }
    }
  }

  if (savecode) {
    if (ti) *ti = savei;
    if (tj) *tj = savej;
    return savecode;
  }

  return 0;
}


/* 
 * If (si, sj) points to a string, draw_back(si, sj, &ti, &tj, komaster)
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
draw_back(int si, int sj, int *ti, int *tj, int komaster, int kom_i, int kom_j)
{
  int r, k;
  int adj, adji[MAXCHAIN], adjj[MAXCHAIN];
  int xi[2], xj[2];
  int savei = -1, savej = -1;
  int savecode = 0;

  adj = chainlinks2(si, sj, adji, adjj, 2);
  for (r = 0; r < adj; r++) {
    findlib(adji[r], adjj[r], 2, xi, xj);
    for (k = 0; k < 2; k++) {
      if (!liberty_of_string(xi[k], xj[k], si, sj) &&
	  ((xi[k] > 0 && liberty_of_string(xi[k]-1, xj[k], si, sj))
	   || (xi[k] < board_size-1 
	       && liberty_of_string(xi[k]+1, xj[k], si, sj))
	   || (xj[k] > 0 && liberty_of_string(xi[k], xj[k]-1, si, sj))
	   || (xj[k] < board_size-1 
	       && liberty_of_string(xi[k], xj[k]+1, si, sj)))) {
	if (trymove(xi[k], xj[k], OTHER_COLOR(p[si][sj]),
		    "draw_back", si, sj, komaster, kom_i, kom_j)) {
	  int dcode = do_find_defense(si, sj, NULL, NULL, 
				      komaster, kom_i, kom_j);
	  if (dcode != WIN && do_attack(si, sj, NULL, NULL, 
				      komaster, kom_i, kom_j)) {
	    if (dcode == 0) {
	      popgo();
	      if (ti) *ti = xi[k];
	      if (tj) *tj = xj[k];
	      return WIN;
	    }
	    UPDATE_SAVED_KO_RESULT(savecode, savei, savej,
				   dcode, xi[k], xj[k]);
	  }
	  popgo();
	}
      }
    }
  }

  if (savecode) {
    if (ti) *ti = savei;
    if (tj) *tj = savej;
    return savecode;
  }

  return 0;
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
edge_closing_backfill(int si, int sj, int ai, int aj, int *ti, int *tj)
{
  int k;
  int color = p[si][sj];
  int other = OTHER_COLOR(color);
  int bi, bj;
  int ci, cj;
  int number_x, number_o;

  /* Liberty must be on an edge. */
  if (ai != 0 && ai != board_size - 1 && aj != 0 && aj != board_size - 1)
    return 0;

  for (k = 0; k < 4; k++) {
    int upi = deltai[k];
    int upj = deltaj[k];
    int righti = upj;
    int rightj = -upi;
    if (ON_BOARD(ai-upi, aj-upj))
      continue;
    if (p[ai+upi][aj+upj] != color)
      return 0;
    if (ON_BOARD(ai+righti, aj+rightj)
	&& p[ai+righti][aj+rightj] == EMPTY
	&& (!ON_BOARD(ai-righti, aj-rightj) 
	    || p[ai-righti][aj-rightj] == color))
      ; /* Everything ok so far. */
    else if (ON_BOARD(ai-righti, aj-rightj)
	     && p[ai-righti][aj-rightj] == EMPTY
	     && (!ON_BOARD(ai+righti, aj+rightj) 
		 || p[ai+righti][aj+rightj] == color)) {
      /* Negate right direction. */
      righti = -righti;
      rightj = -rightj;
    }
    else
      return 0;
    
    if (p[ai+upi+righti][aj+upj+rightj] != other)
      return 0;

    bi = ai + upi + 2 * righti;
    bj = aj + upj + 2 * rightj;
    if (!ON_BOARD(bi, bj))
      return 0;

    ci = ai + 2 * righti;
    cj = aj + 2 * rightj;

    number_x = 0;
    number_o = 0;
    if (p[bi][bj] == color)
      number_x++;
    else if (p[bi][bj] == other)
      number_o++;

    if (p[ci][cj] == color)
      number_x++;
    else if (p[ci][cj] == other)
      number_o++;

    if (number_o > number_x)
      return 0;

    *ti = ai + righti;
    *tj = aj + rightj;
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
edge_block(int si, int sj, int ai, int aj,
	   int movei[MAX_MOVES], int movej[MAX_MOVES],
	   int scores[MAX_MOVES], int *moves)
{
  int k, l;
  int color = p[si][sj];
  int other = OTHER_COLOR(color);
  int ci, cj;
  int di, dj;
  int ei, ej;
  int fi, fj;
  int gi, gj;
  int hi, hj;
  int score;

  /* Liberty must be on an edge. */
  if (ai != 0 && ai != board_size - 1 && aj != 0 && aj != board_size - 1)
    return;

  /* Search for the right orientation. */
  for (k = 0; k < 4; k++) {
    int upi = deltai[k];
    int upj = deltaj[k];
    if (ON_BOARD(ai-upi, aj-upj))
      continue;
    if (p[ai+upi][aj+upj] != color || !same_string(ai+upi, aj+upj, si, sj))
      return;
    
    for (l = 0; l < 2; l++) {
      int righti = upj;
      int rightj = -upi;
      if (l == 1) {
	righti = -righti;
	rightj = -rightj;
      }

      ci = ai + righti;
      cj = aj + rightj;
      di = ai + righti + upi;
      dj = aj + rightj + upj;

      if (!ON_BOARD(ci, cj) || p[ci][cj] != color || p[di][dj] != other)
	continue;

      ei = ci + righti;
      ej = cj + rightj;
      fi = di + righti;
      fj = dj + rightj;
      gi = ei + righti;
      gj = ej + rightj;
      hi = ai - righti;
      hj = aj - rightj;
      
      if (!ON_BOARD(ei, ej))
	continue;
      
      if (p[ei][ej] == EMPTY && p[fi][fj] == EMPTY 
	  && (!ON_BOARD(gi, gj) || p[gi][gj] != color)) {
	/* Everything is set up, suggest moves at e and f. */
	if (!ON_BOARD(hi, hj) || p[hi][hj] == color)
	  score = 0;
	else
	  score = -5;
	if (countlib(si, sj) == 2)
	  score -= 10;
	ADD_CANDIDATE_MOVE(ei, ej, score, movei, movej, scores, *moves);

	if (countlib(di, dj) == 1)
	  score = 25;
	else
	  score = 0;
	if (countlib(si, sj) == 2)
	  score -= 10;
	ADD_CANDIDATE_MOVE(fi, fj, score, movei, movej, scores, *moves);
      }
      else if (countlib(ci, cj) == 2 && countlib(di, dj) > 1) {
	int libi[2], libj[2];
	int ti, tj;
	findlib(ci, cj, 2, libi, libj);
	if (libi[0] == ai && libj[0] == aj) {
	  ti = libi[1];
	  tj = libj[1];
	}
	else {
	  ti = libi[0];
	  tj = libj[0];
	}
	if (!is_self_atari(ti, tj, other))
	  ADD_CANDIDATE_MOVE(ti, tj, 0, movei, movej, scores, *moves);
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
 * attacked string, and f is the considered moves.  */

static void
edge_block(int si, int sj, int ai, int aj,
	   int movei[MAX_MOVES], int movej[MAX_MOVES],
	   int scores[MAX_MOVES], int *moves)
{
  int k, l;
  int color = p[si][sj];
  int other = OTHER_COLOR(color);
  int ci, cj;
  int di, dj;
  int ei, ej;
  int fi, fj;
  int gi, gj;
  int hi, hj;

  /* Liberty must be on an edge. */
  if (ai != 0 && ai != board_size - 1 && aj != 0 && aj != board_size - 1)
    return;

  /* Search for the right orientation. */
  for (k = 0; k < 4; k++) {
    int upi = deltai[k];
    int upj = deltaj[k];
    if (ON_BOARD(ai-upi, aj-upj))
      continue;
    if (p[ai+upi][aj+upj] != color || !same_string(ai+upi, aj+upj, si, sj))
      return;
    
    for (l = 0; l < 2; l++) {
      int righti = upj;
      int rightj = -upi;
      if (l == 1) {
	righti = -righti;
	rightj = -rightj;
      }

      ci = ai + righti;
      cj = aj + rightj;
      di = ai + righti + upi;
      dj = aj + rightj + upj;

      if (!ON_BOARD(ci, cj) || p[ci][cj] != color 
	  || p[di][dj] != other || countlib(di, dj) > 1)
	continue;

      ei = ci + righti;
      ej = cj + rightj;
      fi = di + righti;
      fj = dj + rightj;
      gi = ei + righti;
      gj = ej + rightj;
      hi = ai - righti;
      hj = aj - rightj;
      
      if (!ON_BOARD(ei, ej))
	continue;
      
      if (p[ei][ej] == EMPTY && p[fi][fj] == EMPTY 
	  && (!ON_BOARD(gi, gj) || p[gi][gj] != color))
	ADD_CANDIDATE_MOVE(fi, fj, 30, movei, movej, scores, *moves);
    }
  }
}

#endif

/* ================================================================ */
/*            Defending by attacking surrounding strings            */
/* ================================================================ */

/* Add the chainbreaking moves relative to the string (si, sj) to the
 * (movei[], movej[]) arrays.
 */
static void
break_chain_moves(int si, int sj, int movei[MAX_MOVES], int movej[MAX_MOVES],
		  int scores[MAX_MOVES], int *moves)
{
  int r;
  int xi, xj;
  int adj, adji[MAXCHAIN], adjj[MAXCHAIN];
  
  /* Find links in atari. */
  adj = chainlinks2(si, sj, adji, adjj, 1);
  
  for (r = 0; r < adj; r++) {
    findlib(adji[r], adjj[r], 1, &xi, &xj);
    ADD_CANDIDATE_MOVE(xi, xj, 0, movei, movej, scores, *moves);
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
break_chain2_efficient_moves(int si, int sj,
			     int movei[MAX_MOVES], int movej[MAX_MOVES],
			     int scores[MAX_MOVES], int *moves)
{
  int r;
  int adj, adji[MAXCHAIN], adjj[MAXCHAIN];
  int adj2, adj2i[MAXCHAIN], adj2j[MAXCHAIN];
  int libi[2], libj[2];
  
  /* Find links with 2 liberties. */
  adj = chainlinks2(si, sj, adji, adjj, 2);
  
  for (r = 0; r < adj; r++) {
    adj2 = chainlinks2(adji[r], adjj[r], adj2i, adj2j, 1);
    if (adj2 == 1 && countlib(si, sj) > 2) {
      int ai, aj;
      findlib(adj2i[0], adj2j[0], 1, &ai, &aj);
      ADD_CANDIDATE_MOVE(ai, aj, 0, movei, movej, scores, *moves);
      continue;
    }
    
    if (adj2 > 1)
      continue;
    
    findlib(adji[r], adjj[r], 2, libi, libj);
    if (approxlib(libi[0], libj[0], p[adji[r]][adjj[r]], 3, NULL, NULL) <= 2)
      ADD_CANDIDATE_MOVE(libi[1], libj[1], 0, movei, movej, scores, *moves);
    if (approxlib(libi[1], libj[1], p[adji[r]][adjj[r]], 3, NULL, NULL) <= 2)
      ADD_CANDIDATE_MOVE(libi[0], libj[0], 0, movei, movej, scores, *moves);
    
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
    if (gg_abs(libi[0] - libi[1]) == 1
	&& gg_abs(libj[0] - libj[1]) == 1
	&& (adji[r] == 1 || adji[r] == board_size - 2
	    || adjj[r] == 1 || adjj[r] == board_size - 2)) {

      if ((libi[0] == 0 || libi[0] == board_size - 1
	   || libj[0] == 0 || libj[0] == board_size - 1)
	  && !is_self_atari(libi[1], libj[1], p[si][sj]))
	ADD_CANDIDATE_MOVE(libi[1], libj[1], 0, movei, movej, scores, *moves);

      if ((libi[1] == 0 || libi[1] == board_size - 1
	   || libj[1] == 0 || libj[1] == board_size - 1)
	  && !is_self_atari(libi[0], libj[0], p[si][sj]))
	ADD_CANDIDATE_MOVE(libi[0], libj[0], 0, movei, movej, scores, *moves);
    }
  }
}

static void
break_chain2_moves(int si, int sj, int movei[MAX_MOVES], int movej[MAX_MOVES],
		   int scores[MAX_MOVES], int *moves, int require_safe)
{
  int r;
  int k;
  int ai, aj;
  int adj;
  int adji[MAXCHAIN], adjj[MAXCHAIN];
  int libi[2], libj[2];
  int color = p[si][sj];
  int other = OTHER_COLOR(color);

  adj = chainlinks2(si, sj, adji, adjj, 2);
  
  for (r = 0; r < adj; r++) {
    ai = adji[r];
    aj = adjj[r];

    findlib(ai, aj, 2, libi, libj);
    for (k = 0; k < 2; ++k) {
      int unsafe = is_self_atari(libi[k], libj[k], color);
      if (!unsafe
	  || (!require_safe
	      && approxlib(libi[k], libj[k], other, 5, NULL, NULL) < 5))
	ADD_CANDIDATE_MOVE(libi[k], libj[k], 0, movei, movej, scores, *moves);
    }

    if (stackp <= backfill2_depth)
      break_chain_moves(ai, aj, movei, movej, scores, moves);
  }
}

/*
 * (si,sj) points to a group. break_chain2(si, sj, *i, *j)
 * returns WIN if there is a string in the surrounding chain having
 * exactly two liberties whose attack leads to the rescue of
 * (si, sj). Then (*i, *j) points to the location of the attacking move.
 * 
 * Returns KO_A if the saving move depends on ignoring a ko threat;
 * 
 * Returns KO_B if the saving move requires making a ko threat and winning
 * the ko.
 */

static int 
break_chain2(int si, int sj, int *i, int *j, 
	     int komaster, int kom_i, int kom_j)
{
  int color, other;
  int v;
  int savei = -1, savej = -1;
  int libi[2], libj[2];
  int liberties;
  int ti[MAXCHAIN], tj[MAXCHAIN];
  int scores[MAXCHAIN];
  int moves = 0;
  int mw[MAX_BOARD][MAX_BOARD];
  int savecode = 0;

  SETUP_TRACE_INFO("break_chain2", si, sj);
  
  memset(mw, 0, sizeof(mw));

  RTRACE("in break_chain2 at %m\n", si, sj);
  color = p[si][sj];
  other = OTHER_COLOR(color);
  
  break_chain2_moves(si, sj, ti, tj, scores, &moves, 0);
  order_moves(si, sj, moves, ti, tj, scores, color, read_function_name);

  /* We do not wish to consider the move if it can be 
   * immediately recaptured, unless stackp <= backfill_depth.
   *
   * With the incremental board code it's not much point in using
   * approxlib() as heuristic whether we want to play the move. Better
   * and more accurate to really play the move and then use findlib() to
   * see how many liberties we obtained.
   */

  for (v = 0; v < moves; v++) {
    int new_komaster, new_kom_i, new_kom_j;
    int ko_move;

    if (komaster_trymove(ti[v], tj[v], color, "break_chain2-A", si, sj,
			 komaster, kom_i, kom_j,
			 &new_komaster, &new_kom_i, &new_kom_j,
			 &ko_move, savecode == 0 && stackp <= ko_depth)) {
      if (ko_move) {
	if (do_attack(si, sj, NULL, NULL,
		      new_komaster, new_kom_i, new_kom_j) != WIN) {
	  savecode = KO_B;
	  savei = ti[v];
	  savej = tj[v];
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
    liberties = findlib(ti[v], tj[v], 2, libi, libj);
    if (liberties > 1) {
      int acode = do_attack(si, sj, i, j,
			    new_komaster, new_kom_i, new_kom_j);
      if (acode == 0) {
	if (i) *i = ti[v];
	if (j) *j = tj[v];
	popgo();
	SGFTRACE(ti[v], tj[v], 1, "attack defended-A");
	return WIN;
      }
      UPDATE_SAVED_KO_RESULT(savecode, savei, savej, acode, ti[v], tj[v]);
    }
    else if (stackp <= backfill_depth) {
      int newer_komaster, newer_kom_i, newer_kom_j;
      int bi = libi[0];
      int bj = libj[0];
      int try_harder = 0;

      gg_assert(liberties == 1);

      if (komaster_trymove(bi, bj, other, "break_chain2-C", si, sj,
			   new_komaster, new_kom_i, new_kom_j,
			   &newer_komaster, &newer_kom_i, &newer_kom_j,
			   &ko_move, savecode == 0 && stackp <= ko_depth)) {
	if (!ko_move) {
	  if (countlib(bi, bj) <= 2)
	    try_harder = 1;
	  if (p[si][sj] != EMPTY) {
	    int dcode = do_find_defense(si, sj, NULL, NULL, newer_komaster,
					newer_kom_i, newer_kom_j);
	    if (dcode == WIN && !try_harder) {
	      if (i) *i = ti[v];
	      if (j) *j = tj[v];
	      popgo();
	      popgo();
	      SGFTRACE(ti[v], tj[v], WIN, "attack defended-B");
	      return WIN;
	    }
	    /* FIXME: Possibly the ko result codes are not handled
	     * correctly in the presence of two trymove().
	     */
	    UPDATE_SAVED_KO_RESULT_UNREVERSED(savecode, savei, savej,
					      dcode, ti[v], tj[v]);
	  }
	  popgo();
	}
	else {
	  try_harder = 1;
	  gg_assert(p[si][sj] != EMPTY);
	  if (do_find_defense(si, sj, NULL, NULL, newer_komaster,
			      newer_kom_i, newer_kom_j) != 0) {
	    savecode = KO_A; /* Not KO_B since we are one move deeper 
			      * than usual. */
	    savei = ti[v];
	    savej = tj[v];
	  }
	  popgo();
	}
      }

      if (try_harder) {
	int acode = do_attack(si, sj, i, j,
			      new_komaster, new_kom_i, new_kom_j);
	if (acode == 0) {
	  if (i) *i = ti[v];
	  if (j) *j = tj[v];
	  popgo();
	  SGFTRACE(ti[v], tj[v], WIN, "attack defended-C");
	  return WIN;
	}
	UPDATE_SAVED_KO_RESULT(savecode, savei, savej, acode, ti[v], tj[v]);
      }
    }
    popgo(); /* (ti[v], tj[v]) */
  }

  if (savecode != 0) {
    if (i) *i = savei;
    if (j) *j = savej;
    SGFTRACE(savei, savej, savecode, "saved move");
    return savecode;
  }

  SGFTRACE(-1, -1, 0, NULL);
  return 0;
}


/*
 * (si,sj) points to a group. break_chain3(si, sj, *i, *j)
 * returns 1 if there is a string in the surrounding chain having
 * exactly three liberties whose attack leads to the rescue of
 * (si, sj). Then (*i, *j) points to the location of the attacking move.
 * 
 * Returns KO_A if the saving move depends on ignoring a ko threat;
 * 
 * Returns KO_B if the saving move requires making a ko threat and winning
 *   the ko.
 */

static int 
break_chain3(int si, int sj, int *i, int *j, 
	     int komaster, int kom_i, int kom_j)
{
  int color, other;
  int r;
  int k;
  int u = 0, v;
  int ai, aj;
  int savei = -1, savej = -1;
  int adj;
  int adji[MAXCHAIN], adjj[MAXCHAIN];
  int libi[3], libj[3];
  int ti[MAXCHAIN], tj[MAXCHAIN];
  int mw[MAX_BOARD][MAX_BOARD];
  int savecode = 0;
  int liberties = countlib(si, sj);

  SETUP_TRACE_INFO("break_chain3", si, sj);

  memset(mw, 0, sizeof(mw));
  
  RTRACE("in break_chain3 at %m\n", si, sj);
  color = p[si][sj];
  other = OTHER_COLOR(color);
  adj = chainlinks2(si, sj, adji, adjj, 3);
  for (r = 0; r < adj; r++) {
    int lib1 = 0, lib2 = 0, lib3 = 0;
    ai = adji[r];
    aj = adjj[r];

    /* We make a list in the (adji, adjj) array of the liberties
     * of boundary strings having exactly three liberties. We mark
     * each liberty in the mw array so that we do not list any
     * more than once.
     */
    findlib(ai, aj, 3, libi, libj);

    /* If the 3 liberty chain easily can run away through one of the
     * liberties, we don't play on any of the other liberties.
     */
    lib1 = approxlib(libi[0], libj[0], other, 4, NULL, NULL);
    lib2 = approxlib(libi[1], libj[1], other, 4, NULL, NULL);
    if (lib1 >= 4 && lib2 >= 4)
      continue;
    lib3 = approxlib(libi[2], libj[2], other, 4, NULL, NULL);

    if ((lib1 >= 4 || lib2 >= 4) && lib3 >= 3)
      continue;

    if (lib1 >= 4 && !mw[libi[0]][libj[0]]) {
      mw[libi[0]][libj[0]] = 1;
      ti[u] = libi[0];
      tj[u] = libj[0];
      u++;
      continue;
    }
    
    if (lib2 >= 4 && !mw[libi[1]][libj[1]]) {
      mw[libi[1]][libj[1]] = 1;
      ti[u] = libi[1];
      tj[u] = libj[1];
      u++;
      continue;
    }
    
    if (lib3 >= 4 && !mw[libi[2]][libj[2]]) {
      mw[libi[2]][libj[2]] = 1;
      ti[u] = libi[2];
      tj[u] = libj[2];
      u++;
      continue;
    }

    /* No easy escape, try all liberties. */
    for (k = 0; k < 3; ++k) {
      if (!mw[libi[k]][libj[k]]) {
	mw[libi[k]][libj[k]] = 1;
	ti[u] = libi[k];
	tj[u] = libj[k];
	u++;
      }
    }
  }
  
  /* We do not wish to consider the move if it can be 
   * immediately recaptured, unless stackp <= backfill2_depth.
   */

  for (v = 0; v < u; v++) {
    if (!trymove(ti[v], tj[v], color, "break_chain3-A", si, sj,
		 komaster, kom_i, kom_j))
      continue;

    if (countlib(ti[v], tj[v]) == 1 && stackp > backfill2_depth) {
      popgo();
      continue;
    }
    
    /* If we just filled our own liberty we back out now */
    if (countlib(si, sj) >= liberties) {
      int acode = do_attack(si, sj, i, j, komaster, kom_i, kom_j);
      if (acode == 0) {
	if (i) *i = ti[v];
	if (j) *j = tj[v];
	popgo();
	SGFTRACE(ti[v], tj[v], WIN, "attack defended");
	return WIN;
      }
      UPDATE_SAVED_KO_RESULT(savecode, savei, savej, acode, ti[v], tj[v]);
    }
    popgo(); /* (ti[v], tj[v]) */
  }

  if (savecode != 0) {
    if (i) *i = savei;
    if (j) *j = savej;
    SGFTRACE(savei, savej, savecode, "saved move");
    return savecode;
  }

  SGFTRACE(-1, -1, 0, NULL);
  return 0;
}

/* This function looks for moves attacking those components
 * of the surrounding chain of the superstring (see find_superstring
 * for the definition) which have fewer than liberty_cap liberties,
 * and which are not adjacent to the string itself, since those
 * are tested by break_chain. If such a boundary chain can be
 * attacked, and if attacking the boundary results in saving
 * the (si,sj) string, then success is reported.
 */
/* FIXME: Consider ko captures */
static int
superstring_breakchain(int si, int sj, int *i, int *j, int komaster,
		       int kom_i, int kom_j, int liberty_cap)
{
  int adj;
  int adji[MAXCHAIN], adjj[MAXCHAIN];
  int k, ai, aj;
  int color = p[si][sj];
  int savei = -1;
  int savej = -1;
  int savecode = 0;

  SETUP_TRACE_INFO("superstring_breakchain", si, sj);

  proper_superstring_chainlinks(si, sj, &adj, adji, adjj, liberty_cap);
  for (k = 0; k < adj; k++) {
    int new_komaster, new_kom_i, new_kom_j;
    int ko_move;

    if (countlib(adji[k], adjj[k]) > 1)
      continue;
    findlib(adji[k], adjj[k], 1, &ai, &aj);

    if (komaster_trymove(ai, aj, color, "superstring_break_chain", si, sj,
			 komaster, kom_i, kom_j,
			 &new_komaster, &new_kom_i, &new_kom_j,
			 &ko_move, savecode == 0 && stackp <= ko_depth)) {
      if (!ko_move) {
	int acode = do_attack(si, sj, NULL, NULL,
			      new_komaster, new_kom_i, new_kom_j);
	if (acode == 0) {
	  popgo();
	  if (i) *i = ai;
	  if (j) *j = aj;
	  SGFTRACE(ai, aj, WIN, "attack defended");
	  return WIN;
	}
	else if (acode != WIN) {
	  UPDATE_SAVED_KO_RESULT(savecode, savei, savej, acode, 
				 ai, aj);
	}
	popgo();
      }
      else {
	if (do_attack(si, sj, NULL, NULL,
		      new_komaster, new_kom_i, new_kom_j) != WIN) {
	  savei = ai;
	  savej = aj;
	  savecode = KO_B;
	}
	popgo();
      }
    }
  }
  
  if (savecode) {
    if (i) *i = savei;
    if (j) *j = savej;
    SGFTRACE(savei, savej, savecode, "saved move");
    return savecode;
  }

  SGFTRACE(-1, -1, 0, NULL);
  return 0;
}

/*
 * If si, sj points to a group, double_atari_chain2() adds all moves
 * which make a double atari on some strings in the surrounding chain
 * to the (movei[], movej[]) arrays.
 */

static void
double_atari_chain2(int si, int sj,
		    int movei[MAX_MOVES], int movej[MAX_MOVES],
		    int scores[MAX_MOVES], int *moves)
{
  int r, k;
  int ai, aj;
  int adj;
  int adji[MAXCHAIN], adjj[MAXCHAIN];
  int libi[2], libj[2];
  int mw[MAX_BOARD][MAX_BOARD];

  memset(mw, 0, sizeof(mw));

  adj = chainlinks2(si, sj, adji, adjj, 2);
  for (r = 0; r < adj; r++) {
    ai = adji[r];
    aj = adjj[r];
    findlib(ai, aj, 2, libi, libj);
    for (k = 0; k < 2; ++k) {
      mw[libi[k]][libj[k]]++;
      if (mw[libi[k]][libj[k]] == 2) {
	/* Found a double atari, but don't play there unless the move
         * is safe for the defender.
	 */
	if (!is_self_atari(libi[k], libj[k], p[si][sj]))
	  ADD_CANDIDATE_MOVE(libi[k], libj[k], 0,
			     movei, movej, scores, *moves);
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
 * move that defends the string (si, sj) with one liberty,
 * not considering moves from the list.
 */
static int
restricted_defend1(int si, int sj, int *i, int *j, 
		   int komaster, int kom_i, int kom_j,
		   int forbidden_moves, 
		   int *forbidden_movei, int *forbidden_movej)
{
  int color, other;
  int xi, xj;
  int libi, libj;
  int movei[MAX_MOVES], movej[MAX_MOVES];
  int scores[MAX_MOVES];
  int moves = 0;
  int savei = -1, savej = -1;
  int savecode = 0;
  int liberties;
  int k;

  SETUP_TRACE_INFO("restricted_defend1", si, sj);
  reading_node_counter++;
  
  gg_assert(p[si][sj] != EMPTY);
  ASSERT(countlib(si, sj) == 1, si, sj);

  RTRACE("try to escape atari on %m.\n",  si, sj);

  color = p[si][sj];
  other = OTHER_COLOR(color);

  /* (libi, libj) will be the liberty of the string. */
  liberties = findlib(si, sj, 1, &libi, &libj);
  ASSERT(liberties == 1, si, sj);

  /* Collect moves to try in the first batch.
   * 1. First order liberty.
   * 2. Chain breaking moves.
   */
  movei[0] = libi;
  movej[0] = libj;
  scores[0] = 0;
  moves = 1;
  
  break_chain_moves(si, sj, movei, movej, scores, &moves);
  order_moves(si, sj, moves, movei, movej, scores, color, read_function_name);
  for (k = 0; k < moves; k++) {
    int ko_capture;
    int ko_i, ko_j;

    xi = movei[k];
    xj = movej[k];
    if (in_list(xi, xj, forbidden_moves, forbidden_movei, forbidden_movej))
      continue;
    /* To avoid loops with double ko, we do not allow any ko captures,
     * even legal ones, if the opponent is komaster.
     */
    if (is_ko(xi, xj, color, &ko_i, &ko_j))
      ko_capture = 1;
    else
      ko_capture = 0;

    if ((komaster != other || !ko_capture)
	&& trymove(xi, xj, color, "restricted_defend1-A", si, sj,
		   komaster, kom_i, kom_j)) {
      int libs = countlib(si, sj);
      if (libs > 2) {
	popgo();
	SGFTRACE(xi, xj, WIN, "defense effective");
	if (i) *i = xi;
	if (j) *j = xj;
	return WIN;
      }
      if (libs == 2) {
	int acode;

	if (!ko_capture)
	  acode = restricted_attack2(si, sj, NULL, NULL, 
				     komaster, kom_i, kom_j,
				     forbidden_moves,
				     forbidden_movei, forbidden_movej);
	else
	  acode = restricted_attack2(si, sj, NULL, NULL, color, xi, xj,
				     forbidden_moves,
				     forbidden_movei, forbidden_movej);
	popgo();
	if (acode == 0) {
	  SGFTRACE(xi, xj, WIN, "defense effective");
	  if (i) *i = xi;
	  if (j) *j = xj;
	  return WIN;
	}
	/* if the move works with ko we save it, then look for something
	 * better.
	 */
	UPDATE_SAVED_KO_RESULT(savecode, savei, savej, acode, xi, xj);
      }
      else
	popgo();
    }
    else {
      int ko_i, ko_j;
      if (stackp <= ko_depth
	  && savecode == 0 
	  && (komaster == EMPTY
	      || (komaster == color
		  && kom_i == xi
		  && kom_j == xj))
	  && is_ko(xi, xj, color, &ko_i, &ko_j)
	  && tryko(xi, xj, color, "restricted_defend1-B",
		   color, ko_i, ko_j)) {
	int libs = countlib(si, sj);
	if (libs > 2) {
	  popgo();
	  UPDATE_SAVED_KO_RESULT(savecode, savei, savej, 2, xi, xj);
	}
	else if (libs == 2) {
	  int acode;
	  acode = restricted_attack2(si, sj, NULL, NULL, color, xi, xj,
				     forbidden_moves,
				     forbidden_movei, forbidden_movej);
	  popgo();
	  UPDATE_SAVED_KO_RESULT(savecode, savei, savej, acode, xi, xj);
	}
	else
	  popgo();
      }
    }
  }
  if (savecode != 0) {
    SGFTRACE(savei, savej, savecode, "saved move");
    if (i) *i = savei;
    if (j) *j = savej;
    return savecode;
  }
  SGFTRACE(-1, -1, 0, NULL);
  return 0;
}


/* Given a list of moves, restricted_attack2 tries to find a 
 * move that attacks the string (si, sj) with two liberties,
 * not considering moves from the list.
 */
static int
restricted_attack2(int si, int sj, int *i, int *j, 
		   int komaster, int kom_i, int kom_j,
		   int forbidden_moves, 
		   int *forbidden_movei, int *forbidden_movej)
{
  int color, other;
  int ai, aj;
  int liberties;
  int libi[2], libj[2];
  int savei = -1, savej = -1;
  int savecode = 0;
  int k;

  SETUP_TRACE_INFO("restricted_attack2", si, sj);
  reading_node_counter++;

  find_origin(si, sj, &si, &sj);
  gg_assert(p[si][sj] != EMPTY);
  gg_assert(countlib(si, sj) == 2);

  RTRACE("restricted attack on %m with 2 liberties\n", si, sj);

  color = p[si][sj];
  other = OTHER_COLOR(color);

  /* The attack may fail if a boundary string is in atari and cannot 
   * be defended.  First we must try defending such a string. 
   */
  /* Get the two liberties of (si, sj). */
  liberties = findlib(si, sj, 2, libi, libj);
  ASSERT(liberties == 2, si, sj);

  for (k = 0; k < 2; k++) {
    int ko_i, ko_j;
    int ko_capture;

    ai = libi[k];
    aj = libj[k];
    if (in_list(ai, aj, forbidden_moves, forbidden_movei, forbidden_movej))
      continue;
    /* To avoid loops with double ko, we do not allow any ko captures,
     * even legal ones, if the opponent is komaster. 
     */
    if (is_ko(ai, aj, other, &ko_i, &ko_j))
      ko_capture = 1;
    else
      ko_capture = 0;

    if ((komaster != color || !ko_capture)
	&& trymove(ai, aj, other, "restricted_attack2", si, sj,
		   komaster, kom_i, kom_j)) {
      if ((!ko_capture 
	   && !restricted_defend1(si, sj, NULL, NULL, komaster, kom_i, kom_j,
				  forbidden_moves,
				  forbidden_movei, forbidden_movej))
	  || (ko_capture
	      && !restricted_defend1(si, sj, NULL, NULL, other, ko_i, ko_j,
				     forbidden_moves,
				     forbidden_movei, forbidden_movej))) {
	popgo();
	SGFTRACE(ai, aj, WIN, "attack effective");
	if (i) *i = ai;
	if (j) *j = aj;
	return WIN;
      }
      popgo();
    }
    else if (savecode == 0
	     && (komaster == EMPTY
		 || (komaster == other
		     && kom_i == ai
		     && kom_j == aj))
	     && tryko(ai, aj, other, "restricted_attack2",
		      komaster, kom_i, kom_j)) {
      if (!restricted_defend1(si, sj, NULL, NULL, other, ko_i, ko_j,
			      forbidden_moves,
			      forbidden_movei, forbidden_movej)) {
	popgo();
	savecode = KO_B;
	savei = ai;
	savej = aj;
      }
      else
	popgo();
    }
  }
  if (savecode) {
    SGFTRACE(savei, savej, savecode, "saved move");
    if (i) *i = savei;
    if (j) *j = savej;
    return savecode;
  }
  SGFTRACE(-1, -1, 0, NULL);
  return 0;
}

/* Returns true if the move at (m, n) is in a given list of moves */

static int
in_list(int m, int n, int moves, int *movei, int *movej)
{
  int k;

  for (k = 0; k < moves; k++)
    if (movei[k] == m && movej[k] == n)
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


/* The string at (si, sj) is under attack. The num_moves moves in
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
order_moves(int si, int sj, int num_moves, int *movei, int *movej,
	    int *scores, int color, const char *funcname)
{
  int move;
  int i, j;
  int maxscore;
  int max_at;
  int string_color = p[si][sj];
  int string_libs = countlib(si, sj);

  /* don't bother sorting if only one move, or none at all */
  if (num_moves < 2)
    return;

  /* Assign a score to each move. */
  for (move = 0; move < num_moves; ++move) {
    int mi = movei[move];
    int mj = movej[move];

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
    incremental_order_moves(mi, mj, color, si, sj, &number_edges,
			    &number_same_string, &number_own,
			    &number_opponent, &captured_stones,
			    &threatened_stones, &saved_stones, &number_open);

    if (0)
      gprintf("%o %m values: %d %d %d %d %d %d %d %d\n", mi, mj, number_edges,
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
      
      int libs = approxlib(mi, mj, color, 5, NULL, NULL);
      if (number_same_string > 0) {
	if (libs > 5 || (libs == 4 && stackp > fourlib_depth))
	  scores[move] += defend_lib_score[5];
	else
	  scores[move] += defend_lib_score[libs];
      }
      else {
	/* Add points for the number of liberties the played stone
         * obtains when not adjacent to the attacked string.
	 */
	if (libs > 4)
	  scores[move] += defend_not_adjacent_lib_score[4];
	else
	  scores[move] += defend_not_adjacent_lib_score[libs];
      }
      
      /* 2) Add the number of open liberties near the move to its score. */
      gg_assert(number_open <= 4);
      scores[move] += defend_open_score[number_open];
      
      /* 3) Add a bonus if the move is not on the edge. 
       */
      if (number_edges == 0 || captured_stones > 0)
	scores[move] += defend_not_edge_score;
      
      /* 4) Add thrice the number of captured stones. */
      if (captured_stones <= 5)
	scores[move] += defend_capture_score[captured_stones];
      else
	scores[move] += defend_capture_score[5] + captured_stones;

      /* 5) Add points for stones put into atari, unless this is a
       *    self atari.
       */
      if (libs + captured_stones > 1) {
	if (threatened_stones <= 5)
	  scores[move] += defend_atari_score[threatened_stones];
	else
	  scores[move] += defend_atari_score[5] + threatened_stones;
      }

      /* 6) Add a bonus for saved stones. */
      if (saved_stones <= 5)
	scores[move] += defend_save_score[saved_stones];
      else
	scores[move] += defend_save_score[5];
    }
    else {
      /* Attack move.
       *
       * 1) Add the number of liberties the attacker gets when playing
       *    there, but never more than four.
       */
      int libs = approxlib(mi, mj, color, 4, NULL, NULL);
      if (libs > 4)
	libs = 4;
      scores[move] += attack_own_lib_score[libs];

      if (libs == 0 && captured_stones == 1)
	scores[move] += attack_ko_score;
      
      /* 2) If the move is not a self atari and adjacent to the
       *    string, add the number of liberties the opponent would
       *    gain by playing there. If the string has two liberties,
       *    self-ataris are also ok since they may be snapbacks, but
       *    only if a single stone is sacrificed.
       */
      if ((libs + captured_stones > 1 || (string_libs <= 2 && number_own == 0))
	  && number_same_string > 0) {
	int safe_atari;
	int liberties = approxlib(mi, mj, string_color, 5, NULL, NULL);
	if (liberties > 5 || (liberties == 4 && stackp > fourlib_depth))
	  liberties = 5;
	scores[move] += attack_string_lib_score[liberties];

	safe_atari = (string_libs <= 2 && libs + captured_stones > 1);
	/* The defender can't play here without getting into atari, so
         * we probably souldn't either.
	 */
	if (liberties == 1 && saved_stones == 0 && !safe_atari)
	  scores[move] += cannot_defend_penalty;

	/* Bonus if we put the attacked string into atari without
         * ourselves getting into atari.
	 */
	if (safe_atari)
	  scores[move] += safe_atari_score;
      }
      
      /* 3) Add the number of open liberties near the move to its score. */
      gg_assert(number_open <= 4);
      scores[move] += attack_open_score[number_open];
      
      /* 4) Add a bonus if the move is not on the edge. */
      if (number_edges == 0)
	scores[move] += attack_not_edge_score;
      
      /* 5) Add twice the number of captured stones. */
      if (captured_stones <= 5)
	scores[move] += attack_capture_score[captured_stones];
      else
	scores[move] += attack_capture_score[5];

      /* 6) Add a bonus for saved stones. */
      if (saved_stones <= 5)
	scores[move] += attack_save_score[saved_stones];
      else
	scores[move] += attack_save_score[5];
    }
  }
  
  /* Now sort the moves.  We use selection sort since this array will
   * probably never be more than 10 moves long.  In this case, the
   * overhead imposed by quicksort will probably overshadow the gains
   * given by the O(n*log(n)) behaviour over the O(n^2) behaviour of
   * selection sort.
   */
  for (i = 0; i < num_moves-1; ++i) {

    /* Find the move with the biggest score. */
    maxscore = scores[i];
    max_at = i;
    for (j = i+1; j < num_moves; ++j) {
      if (scores[j] > maxscore) {
	maxscore = scores[j];
	max_at = j;
      }
    }

    /* Now exchange the move at i with the move at max_at.
     * Don't forget to exchange the scores as well.
     */
    if (max_at != i) {
      int tempi = movei[i];
      int tempj = movej[i];
      int tempmax = scores[i];

      movei[i] = movei[max_at];
      movej[i] = movej[max_at];
      scores[i] = scores[max_at];

      movei[max_at] = tempi;
      movej[max_at] = tempj;
      scores[max_at] = tempmax;
    }
  }

  if (0) {
    gprintf("%oVariation %d:\n", count_variations);
    for (i = 0; i < num_moves; i++)
      gprintf("%o  %M %d\n", movei[i], movej[i], scores[i]);
  }

  if (sgf_dumptree) {
    char buf[500];
    char *pos;
    int chars;
    sprintf(buf, "Move order for %s: %n", funcname, &chars);
    pos = buf + chars;
    for (i = 0; i < num_moves; i++) {
      sprintf(pos, "%c%d (%d) %n", movej[i] + 'A' + (movej[i] >= 8),
	      board_size - movei[i], scores[i], &chars);
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


static int safe_move_cache[MAX_BOARD][MAX_BOARD][2];
static int safe_move_cache_when[MAX_BOARD][MAX_BOARD][2];
static void clear_safe_move_cache(void);

static void
clear_safe_move_cache(void)
{
  int i,j;

  for (i = 0; i < MAX_BOARD;i++)
    for (j = 0; j < MAX_BOARD;j++) {
      safe_move_cache_when[i][j][0] = -1;
      safe_move_cache_when[i][j][1] = -1;
    }
}

/* safe_move(i, j, color) checks whether a move at (i, j) is illegal
 * or can immediately be captured. If stackp==0 the result is cached.
 * If the move only can be captured by a ko, it's considered safe.
 * This may or may not be a good convention.
 *
 * For performance reasons, the result of this function is cached.
 */

int 
safe_move(int i, int j, int color)
{
  int safe = 0;
  static int initialized = 0;
  int ko_i, ko_j;
  
  if (!initialized) {
    clear_safe_move_cache();
    initialized = 1;
  }

  /* If we have this position cached, use the previous value. */
  if (stackp == 0
      && safe_move_cache_when[i][j][color==BLACK] == position_number)
    return safe_move_cache[i][j][color==BLACK];

  /* Otherwise calculate the value... */
  if (trymove(i, j, color, "safe_move-A", -1, -1, EMPTY, -1, -1)) {
    int acode = attack(i, j, NULL, NULL);
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
#if 1
  else if (is_ko(i, j, color, &ko_i, &ko_j)
	   && tryko(i, j, color, "safe_move-B", EMPTY, -1, -1)) {
    if (do_attack(i, j, NULL, NULL, color, i, j) != WIN)
      safe = KO_B;
    else
      safe = 0;
    popgo();
  }
#endif  
  /* ...and store it in the cache.
   * FIXME: Only store result in cache when we're working at
   * full depth.
   *
   * Comment: This is currently not a problem since no reduced depth
   * reading is performed.
   */
  if (stackp == 0) {
    if (0)
      gprintf("Safe move at %m for %s cached when depth=%d, position number=%d\n",
	      i, j, color_to_string(color), depth, position_number);
    safe_move_cache_when[i][j][color==BLACK] = position_number;
    safe_move_cache[i][j][color==BLACK] = safe;
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
      if (!shadow[i][j] && p[i][j] == EMPTY)
	c = '.';
      else if (!shadow[i][j] && p[i][j] == WHITE)
	c = 'O';
      else if (!shadow[i][j] && p[i][j] == BLACK)
	c = 'X';
      if (shadow[i][j] && p[i][j] == EMPTY)
	c = ',';
      else if (shadow[i][j] && p[i][j] == WHITE)
	c = 'o';
      else if (shadow[i][j] && p[i][j] == BLACK)
	c = 'x';
      
      fprintf(stderr, " %c", c);
    }
    
    fprintf(stderr, " %d", ii);
  }

  end_draw_board();
}

static void
draw_active_area(char board[MAX_BOARD][MAX_BOARD])
{
  int i, j, ii;
  int c = ' ';

  start_draw_board();
  
  for (i = 0; i < board_size; i++) {
    ii = board_size - i;
    fprintf(stderr, "\n%2d", ii);
    
    for (j = 0; j < board_size; j++) {
      if (board[i][j] == EMPTY)
	c = '.';
      else if (board[i][j] == WHITE)
	c = 'O';
      else if (board[i][j] == BLACK)
	c = 'X';
      if (board[i][j] == GRAY)
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
verify_stored_board(char board[MAX_BOARD][MAX_BOARD])
{
  int m, n;
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++)
      if (board[m][n] != GRAY && board[m][n] != p[m][n])
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
      int ai = persistent_reading_cache[k].stacki[r];
      int aj = persistent_reading_cache[k].stackj[r];
      int color = persistent_reading_cache[k].move_color[r];
      if (ai == -1)
	break;
      if (ON_BOARD(ai, aj)
	  && p[ai][aj] == EMPTY
	  && trymove(ai, aj, color, "purge_persistent_reading_cache", -1, -1,
		     EMPTY, -1, -1))
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
search_persistent_reading_cache(int routine, int si, int sj,
				int *result, int *i, int *j)
{
  int k;
  int r;

  for (k = 0; k < persistent_reading_cache_size; k++) {
    /* Check that everything matches. */
    struct reading_cache *entry = &(persistent_reading_cache[k]);
    int ai = -1;
    int aj = -1;
    if (entry->routine != routine
	|| entry->si != si
	|| entry->sj != sj
	|| entry->remaining_depth < (depth - stackp))
      continue;
    
    for (r = 0; r < MAX_CACHE_DEPTH; r++) {
      ai = entry->stacki[r];
      aj = entry->stackj[r];
      if (ai == -1 
	  || (entry->board[ai][aj] != GRAY
	      && p[ai][aj] != entry->board[ai][aj]))
	break;
    }

    if (r < MAX_CACHE_DEPTH && ai != -1)
      continue;

    if (!verify_stored_board(entry->board))
      continue;

    /* Matched alright. Increase score, fill in the answer, and return. */
    entry->score += entry->nodes;
    *result = entry->result;
    if (i) *i = entry->i;
    if (j) *j = entry->j;
    ASSERT(entry->result == 0
	   || (entry->i == -1 && entry->j == -1)
	   || ON_BOARD(entry->i, entry->j),
	   entry->i, entry->j);

    if ((debug & DEBUG_READING_PERFORMANCE)
	&& entry->nodes >= MIN_NODES_TO_REPORT) {
      if (entry->result != 0)
	gprintf("%o%s %m = %d %m, cached (%d nodes) ",
		routine == ATTACK ? "attack" : "defend",
		si, sj,	entry->result, entry->i, entry->j, entry->nodes);
      else 
	gprintf("%o%s %m = %d, cached (%d nodes) ",
		routine == ATTACK ? "attack" : "defend",
		si, sj, entry->result, entry->nodes);
      dump_stack();
    }

    if (0) {
      /* Check that the cached value is correct. */
      int result2;
      int i2;
      int j2;

      if (routine == ATTACK) 
	result2 = do_attack(si, sj, &i2, &j2, EMPTY, -1, -1);
      else
	result2 = do_find_defense(si, sj, &i2, &j2, EMPTY, -1, -1);
      
      if (result2 != entry->result ||
	  (result2 != 0 && (i2 != entry->i || j2 != entry->j))) {
	gprintf("%oIncorrect cached result %d %m, should be %d %m\n",
		entry->result, entry->i, entry->j, result2, i2, j2);
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
store_persistent_reading_cache(int routine, int si, int sj,
			       int result, int i, int j,
			       int nodes)
{
  char active[MAX_BOARD][MAX_BOARD];
  int m, n;
  int k;
  int r;
  int score = nodes;
  struct reading_cache *entry;

  ASSERT(result == 0 || (i == -1 && j == -1) || ON_BOARD(i, j), i, j);

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
  entry->si	  = si;
  entry->sj	  = sj;
  entry->result  = result;
  entry->i	  = i;
  entry->j	  = j;

  for (r = 0; r < MAX_CACHE_DEPTH; r++) {
    if (r < stackp)
      get_move_from_stack(r, &(entry->stacki[r]), &(entry->stackj[r]),
			  &(entry->move_color[r]));
    else {
      entry->stacki[r] = -1;
      entry->stackj[r] = -1;
      entry->move_color[r] = EMPTY;
    }
  }
  
  /* Remains to set the board. We let the active area be the contested
   * string and reading shadow + adjacent empty and strings +
   * neighbors of active area so far + one more expansion from empty
   * to empty.
   */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++)
      active[m][n] = shadow[m][n];

  mark_string(si, sj, active, 1);

  /* To be safe, also add the successful move. */
  if (result != 0 && i != -1)
    active[i][j] = 1;

  /* Add adjacent strings and empty. */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      if (active[m][n] != 0) 
	continue;
      if ((m > 0 && active[m-1][n] == 1)
	  || (m < board_size-1 && active[m+1][n] == 1)
	  || (n > 0 && active[m][n-1] == 1)
	  || (n < board_size-1 && active[m][n+1] == 1)) {
	if (p[m][n] != EMPTY)
	  mark_string(m, n, active, 2);
	else
	  active[m][n] = 2;
      }
    }

  /* Remove invincible strings. No point adding their liberties and
   * neighbors.
   */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++)
      if (p[m][n] != EMPTY && worm[m][n].invincible)
	active[m][n] = 0;
  
  /* Expand empty to empty. */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      if (p[m][n] != EMPTY || active[m][n] != 0) 
	continue;
      if ((m > 0 && active[m-1][n] == 2 && p[m-1][n] == EMPTY)
	  || (m < board_size-1 && active[m+1][n] == 2 && p[m+1][n] == EMPTY)
	  || (n > 0 && active[m][n-1] == 2 && p[m][n-1] == EMPTY)
	  || (n < board_size-1 && active[m][n+1] == 2 && p[m][n+1] == EMPTY)) {
	active[m][n] = 3;
      }
    }
  
  /* Add neighbors of active area so far. 
   * By using -1 as mark in this step, the test for old marks is
   * simplified.
   */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      if (active[m][n] != 0)
	continue;
      if ((m > 0 && active[m-1][n] > 0)
	  || (m < board_size-1 && active[m+1][n] > 0)
	  || (n > 0 && active[m][n-1] > 0)
	  || (n < board_size-1 && active[m][n+1] > 0))
	active[m][n] = -1;
    }

  /* Also add the previously played stones to the active area. */
  for (r = 0; r < stackp; r++)
    active[entry->stacki[r]][entry->stackj[r]] = 4;

  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++)
      entry->board[m][n] = 
	active[m][n] != 0 ? p[m][n] : GRAY;

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
  gprintf("%omovenum         = %d\n", entry->movenum);
  gprintf("%onodes           = %d\n", entry->nodes);
  gprintf("%oscore           = %d\n", entry->score);
  gprintf("%oremaining_depth = %d\n", entry->remaining_depth);
  gprintf("%oroutine         = %d\n", entry->routine);
  gprintf("%o(si, sj)        = %m\n", entry->si, entry->sj);
  gprintf("%oresult          = %d\n", entry->result);
  gprintf("%o(i, j)          = %m\n", entry->i, entry->j);
  
  for (r = 0; r < MAX_CACHE_DEPTH; r++) {
    if (entry->stacki[r] == -1)
      break;
    gprintf("%ostack[%d]      = %C %m\n", r, entry->move_color[r],
	    entry->stacki[r], entry->stackj[r]);
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
  if (p[m][n] == EMPTY) {
    for (i = -1; i <= 1; i++)
      for (j = -1; j <= 1; j++)
	if (ON_BOARD(m+i, n+j) && p[m+i][n+j] == EMPTY)
	  values[m+i][n+j] += contribution;
    return;
  }
  
  /* Otherwise we give contribution to liberties and diagonal
   * neighbors of the string at (m, n).
   */
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++) {
      if (p[i][j] != EMPTY)
	continue;
      for (k = 0; k < 8; k++) {
	int di = deltai[k];
	int dj = deltaj[k];
	if (ON_BOARD(i+di, j+dj)
	    && p[i+di][j+dj] != EMPTY
	    && same_string(i+di, j+dj, m, n)) {
	  if (k < 4) {
	    values[i][j] += contribution;
	    break;
	  }
	  else {
	    if (p[i+di][j] == EMPTY || countlib(i+di, j) <= 2
		|| p[i][j+dj] == EMPTY || countlib(i, j+dj) <= 2)
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
      gprintf("Reading hotspots: %d %m %f\n", entry->routine, entry->si,
	      entry->sj, contribution);
    }
    switch (entry->routine) {
    case ATTACK:
    case FIND_DEFENSE:
      mark_string_hotspot_values(values, entry->si, entry->sj, contribution);
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


/* naive_ladder(si, sj, &i, &j) tries to capture a string (si, sj)
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
 * attack was successful, (*i, *j) contains the attacking move, unless
 * i and j are null pointers.
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
naive_ladder(int si, int sj, int *i, int *j)
{
  int color, other;
  int ai, aj, bi, bj;
  int acount = 0, bcount = 0;
  int liberties;
  int libi[2], libj[2];
  
  gg_assert(p[si][sj] != EMPTY);
  gg_assert(countlib(si, sj) == 2);
  DEBUG(DEBUG_READING, "naive_ladder(%m)\n", si, sj);

  RTRACE("checking ladder attack on %m with 2 liberties\n", si, sj);

  color = p[si][sj];
  other = OTHER_COLOR(color);

  /* Get the two liberties of (si, sj) into (ai, aj) and (bi, bj). */ 
  liberties = findlib(si, sj, 2, libi, libj);
  ASSERT(liberties == 2, si, sj);
  ai = libi[0];
  aj = libj[0];
  bi = libi[1];
  bj = libj[1];

  /* if (bi, bj) looks more promising we wish to switch the two liberties.
   * We check whether (bi, bj) is adjacent to more open liberties than
   * (ai, aj).
   *
   * FIXME: Use order_moves() instead.
   */

  if (ai > 0 && p[ai-1][aj] == EMPTY)
    acount++;
  if (ai < board_size-1 && p[ai+1][aj] == EMPTY)
    acount++;
  if (aj > 0 && p[ai][aj-1] == EMPTY)
    acount++;
  if (aj < board_size-1 && p[ai][aj+1] == EMPTY)
    acount++;
  if (bi > 0 && p[bi-1][bj] == EMPTY)
    bcount++;
  if (bi < board_size-1 && p[bi+1][bj] == EMPTY)
    bcount++;
  if (bj > 0 && p[bi][bj-1] == EMPTY)
    bcount++;
  if (bj < board_size-1 && p[bi][bj+1] == EMPTY)
    bcount++;

  if (bcount > acount) {
    ai = libi[1];
    aj = libj[1];
    bi = libi[0];
    bj = libj[0];
  }

  RTRACE("considering atari at %m\n", ai, aj);
  
  if (trymove(ai, aj, other, "naive_ladder-A", si, sj, EMPTY, -1, -1)) {
    if (!naive_ladder_defense(si, sj, ai, aj, bi, bj, color, other)) {
      popgo();
      if (i) *i = ai;
      if (j) *j = aj;
      return WIN;
    }
    popgo();
  }
	  
  if (trymove(bi, bj, other, "naive_ladder-B", si, sj,
	      EMPTY, -1, -1)) {
    if (!naive_ladder_defense(si, sj, bi, bj, ai, aj, color, other)) {
      popgo();
      if (i) *i = bi;
      if (j) *j = bj;
      return WIN;
    }
    popgo();
  }

  /* Neither move worked. */
  return 0;
}


/* Try to save the one-liberty string (si, sj) from being caught in a
 * ladder. (ai, aj) is the last played attacking stone and (bi, bj) is
 * the last remaining liberty.
 */

static int
naive_ladder_defense(int si, int sj, int ai, int aj, int bi, int bj,
		     int color, int other) {
  int liberties;
  
  /* Try to capture the just played stone. */
  if (naive_ladder_break_through(si, sj, ai, aj, color, other))
    return WIN;

  /* Try to run away by extending on the last liberty. */
  if (trymove(bi, bj, color, "naive_ladder_defense", si, sj,
	      EMPTY, -1, -1)) {
    liberties = countlib(si, sj);
    if (liberties >= 3
	|| (liberties == 2
	    && !naive_ladder(si, sj, NULL, NULL))) {
      popgo();
      return WIN;
    }
    popgo();
  }
  
  /* Try to capture a string at distance two (Manhattan metric) from
   * the last liberty.
   */
  if (naive_ladder_break_through(si, sj, bi-2, bj, color, other))
    return WIN;

  if (naive_ladder_break_through(si, sj, bi-1, bj-1, color, other))
    return WIN;
  
  if (naive_ladder_break_through(si, sj, bi, bj-2, color, other))
    return WIN;
  
  if (naive_ladder_break_through(si, sj, bi+1, bj-1, color, other))
    return WIN;
  
  if (naive_ladder_break_through(si, sj, bi+2, bj, color, other))
    return WIN;
  
  if (naive_ladder_break_through(si, sj, bi+1, bj+1, color, other))
    return WIN;
  
  if (naive_ladder_break_through(si, sj, bi, bj+2, color, other))
    return WIN;
  
  if (naive_ladder_break_through(si, sj, bi-1, bj+1, color, other))
    return WIN;
  
  /* Nothing worked. */
  return 0;
}


/* Try to break out of the ladder by capturing (ai, aj). We must first
 * verify that there is an opponent stone there and that it is in
 * atari so we can capture it immediately. After the capture we count
 * liberties for (si, sj) to see if the ladder is decided yet.
 */
static int
naive_ladder_break_through(int si, int sj, int ai, int aj,
			   int color, int other)
{
  int liberties;
  int bi, bj;
  
  if (ai < 0
      || ai >= board_size
      || aj < 0
      || aj >= board_size)
    return 0;

  if (p[ai][aj] != other)
    return 0;
  
  if (findlib(ai, aj, 1, &bi, &bj) != 1)
    return 0;

  if (trymove(bi, bj, color, "naive_ladder_break_through", si, sj,
	      EMPTY, -1, -1)) {
    liberties = countlib(si, sj);
    if (liberties >= 3
	|| (liberties == 2
	    && !naive_ladder(si, sj, NULL, NULL))) {
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
