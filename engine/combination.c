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


/* This file contains functions that deals with threats and, 
 * especially, combinations of threats.
 */

#include <string.h>

#include "liberty.h"
#include "gnugo.h"
#include "gg_utils.h"


static void find_double_threats(int color);
static int atari_atari_find_attack_moves(int color, int minsize,
					 int moves[MAX_BOARD * MAX_BOARD],
					 int targets[MAX_BOARD * MAX_BOARD]);
static int atari_atari_find_defense_moves(int str,
					  int moves[MAX_BOARD * MAX_BOARD]);
static int is_atari(int pos, int color);


/* Generate move reasons for combination attacks and defenses against
 * them.
 *
 * This is one of the move generators called from genmove().
 */

void
combinations(int color)
{
  int save_verbose;
  int aa;
  int other = OTHER_COLOR(color);
  int aa_val;

  /* Find intersections with multiple threats. */
  find_double_threats(color);

  save_verbose = verbose;
  if (verbose > 0)
    verbose--;

  if (save_verbose)
    gprintf("\nlooking for combination attacks ...\n");
  aa_val = atari_atari(color, &aa, save_verbose);
  if (aa_val)
    add_my_atari_atari_move(aa, aa_val);
  aa_val = atari_atari(other, &aa, save_verbose);
  if (aa_val && safe_move(aa, color))
    add_your_atari_atari_move(aa, aa_val);
  verbose = save_verbose;
}


#define MAX_THREATENED_STRINGS  10  /* Should be enough for one intersection */

static void
find_double_threats(int color)
{
  int i, j;
  int ii;
  int k;
  int l;

  for (i = 0; i < board_size; ++i)
    for (j = 0; j < board_size; ++j) {
      int num_a_threatened_groups;
      int a_threatened_groups[MAX_THREATENED_STRINGS];
#if 0
      int num_d_threatened_groups;
      int d_threatened_groups[MAX_THREATENED_STRINGS];
#endif

      ii = POS(i, j);
      
      /* Generate ATTACK_EITHER_MOVE move reasons for each pair of the 
       * threatened strings.  We must also remove the threats, because
       * otherwise we would get followup points for them also.     
       *
       * FIXME: 
       *   - This is perhaps not the best way to do it, but realistically
       *     it will be seldom that more than two strings are threatened
       *     at the same point.  Still, we should find a better way.
       *   - ATTACK_EITHER_MOVE should be generalized to more than two strings.
       */
      num_a_threatened_groups = get_attack_threats(ii, MAX_THREATENED_STRINGS,
						   a_threatened_groups);
      if (num_a_threatened_groups > 1) {
	if (trymove(ii, color, "find_double_threats-A", ii, EMPTY, NO_MOVE)) {
	  for (k = 0; k < num_a_threatened_groups - 1; ++k)
	    for (l = k + 1; l < num_a_threatened_groups; ++l) {
	      /* Note: If we used attack_either() here instead of trymove()
	       *       and !defend_both(), we would not make use of the fact
	       *       that we already know of a common threat point for
	       *       the two strings.
	       * Besides, attack_either is currently (3.1.11) not very good.
	       *
	       * The call to attack() is intended to detect the case
	       * where the move at ii is a snapback capture.
	       */
	      if (board[a_threatened_groups[k]] == EMPTY
		  || board[a_threatened_groups[l]] == EMPTY) {
		if (!attack(ii, NULL)) {
		  add_attack_either_move(ii, a_threatened_groups[k], 
					 a_threatened_groups[l]);
		  remove_attack_threat_move(ii, a_threatened_groups[k]);
		  remove_attack_threat_move(ii, a_threatened_groups[l]);
		}
	      }
	      else if (!defend_both(a_threatened_groups[k],
				    a_threatened_groups[l])) {
		add_attack_either_move(ii, a_threatened_groups[k], 
				       a_threatened_groups[l]);
		remove_attack_threat_move(ii, a_threatened_groups[k]);
		remove_attack_threat_move(ii, a_threatened_groups[l]);
	      }
	    }
	  popgo();
	}
      }
    }


  /* FIXME:
   *   TODO:
   *     - defense threats
   *     - combinations of owl threats and other threats
   *     - combinations of threats to cut and connect
   *     - combinations of breakins into enemy territory
   */
}


/* ================================================================ */
/*                       Combination attacks                        */
/* ================================================================ */


/* atari_atari(color, *move) looks for a series of ataris on
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
static void compute_aa_status(int color);
static int get_aa_status(int pos);
static int do_atari_atari(int color, int *attack_point,
                         int *defense_point, int last_friendly,
			  int save_verbose, int minsize);
static int atari_atari_succeeded(int color, int *attack_point,
                                int *defense_point, int last_friendly,
                                int save_verbose, int minsize);

/* Set to 1 if you want verbose traces from this function. */

int
atari_atari(int color, int *move, int save_verbose)
{
  int fpos;
  int aa_val;

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

  compute_aa_status(color);
  
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


/* Ask the atari_atari code whether there appears any combination
 * attack which would capture at least minsize stones after playing at
 * (tpos). If this happens, (*move) points to a move which prevents
 * this blunder.
 */
int
atari_atari_confirm_safety(int color, int tpos, int *move, int minsize)
{
  int fpos;
  int defense_point = NO_MOVE, after_defense_point = NO_MOVE;
  int aa_val, after_aa_val;
  int other = OTHER_COLOR(color);

  /* If aa_depth is too small, we can't see any combination attacks,
   * so in this respect the move is safe enough.
   */
  if (aa_depth < 2)
    return 1;

  memset(forbidden, 0, sizeof(forbidden));

  compute_aa_status(other);

  /* Accept illegal ko capture here. */
  if (!tryko(tpos, color, NULL, EMPTY, NO_MOVE))
    /* Really shouldn't happen. */
    abortgo(__FILE__, __LINE__, "trymove", I(tpos), J(tpos));
  increase_depth_values();

  aa_val = do_atari_atari(other, &fpos, &defense_point, NO_MOVE, 0, minsize);
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
    aa_val = do_atari_atari(other, &fpos, &defense_point, NO_MOVE, 0, aa_val);
  }

  popgo();
  decrease_depth_values();
  /* We know that a combination exists, but we don't know if
   * the original move at (aa) was really relevant. So we
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
 */

int
atari_atari_try_combination(int color, int apos, int bpos)
{
  int other = OTHER_COLOR(color);
  int aa_val = 0;
  int save_verbose = verbose;

  if (aa_depth < 2)
    return 0;
  if (verbose > 0)
    verbose--;
  memset(forbidden, 0, sizeof(forbidden));

  compute_aa_status(color);

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


/* ---------------------------------------------------------------- */
/*                Helper functions for atari_atari.                 */
/* ---------------------------------------------------------------- */


/* Helper function for computing the aa_status for a string.
 */

static void
compute_aa_status(int color)
{
  int other = OTHER_COLOR(color);
  int pos;
  /* Collect worm statuses of opponent's worms. We need to
   * know this because we only want to report unexpected
   * results. For example, we do not want to report success
   * if we find we can kill a worm which is already dead.
   * The worm status of empty points is set to UNKNOWN to signal
   * that stones added along the way need special attention.
   */
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (board[pos] == other) {
      if (dragon[pos].matcher_status == DEAD)
	aa_status[pos] = DEAD;
      else if (worm[pos].attack_codes[0] != 0) {
	if (worm[pos].defend_codes[0] != 0)
	  aa_status[pos] = CRITICAL;
	else
	  aa_status[pos] = DEAD;
      }
      else
	aa_status[pos] = ALIVE;
    }
    else if(ON_BOARD(pos))
      aa_status[pos] = UNKNOWN;
  }
  
  /* reclassify a worm with 2 liberties as INSUBSTANTIAL if capturing
   * it does not result in a live group.
   */
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (board[pos] == other
	&& worm[pos].origin == pos
	&& worm[pos].liberties == 2
	&& aa_status[pos] == ALIVE
	&& !owl_substantial(pos)) {
      int pos2;
      for (pos2 = BOARDMIN; pos2 < BOARDMAX; pos2++)
	if (ON_BOARD(pos2) && is_worm_origin(pos2, pos))
	  aa_status[pos2] = INSUBSTANTIAL;
    }
  }
    
  if (debug & DEBUG_ATARI_ATARI) {
    gprintf("compute_aa_status() for %C\n", color);
    gprintf("aa_status: (ALIVE worms not listed)\n");
    for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
      if (board[pos] == other && is_worm_origin(pos, pos)) {
	const char *status = "UNKNOWN (shouldn't happen)";
	if (aa_status[pos] == DEAD)
	  status = "DEAD";
	else if (aa_status[pos] == CRITICAL)
	  status = "CRITICAL";
	else if (aa_status[pos] == INSUBSTANTIAL)
	  status = "INSUBSTANTIAL";
	
	if (aa_status[pos] != ALIVE)
	  gprintf("%1M: %s\n", pos, status);
      }
    }
  }
}


/* Helper function for retrieving the aa_status for a string. We can't
 * reliably do this simply by looking up aa_status[pos] since this is
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
 * opponent worms involved in the combination, and (last_friendly) is
 * the location of the last friendly move played. Moves marked
 * with the forbidden array are not tried. If no move is found,
 * the values of *attack_point and *defense_point are not changed.
 *
 * If not NULL, *attack_point is left pointing to the location of the
 * attacking move, and *defense_point points to a move defending the
 * combination. In rare cases a defensive move might not be found. If
 * a non-static function calling do_atari_atari gets a return value of
 * 1 but NO_MOVE as the defense point, this should be treated as
 * equivalent to a return value of 0.
 */

#define MAX_THREAT_MOVES  MAX_TACTICAL_POINTS

static int
do_atari_atari(int color, int *attack_point, int *defense_point,
	       int last_friendly, int save_verbose, int minsize)
{
  int other = OTHER_COLOR(color);
  int k;
  int num_attack_moves;
  int attack_moves[MAX_BOARD * MAX_BOARD];
  int targets[MAX_BOARD * MAX_BOARD];
  int num_defense_moves;
  int defense_moves[MAX_BOARD * MAX_BOARD];
  int pos;
  
  if (debug & DEBUG_ATARI_ATARI) {
    gprintf("%odo_atari_atari: ");
    dump_stack();
    gprintf("%oforbidden moves: ");
    for (pos = BOARDMIN; pos < BOARDMAX; pos++)
      if (ON_BOARD(pos) && forbidden[pos])
	gprintf("%o%1m ", pos);
    gprintf("\n");
  }

  /* First look for strings adjacent to the last friendly move played
   * (or to another stone in the same string) which can be
   * unexpectedly attacked.  If so, the combination attack
   * has succeeded.
   */
  if (last_friendly != NO_MOVE) {
    int retval = atari_atari_succeeded(color, attack_point, defense_point,
				       last_friendly, save_verbose, minsize);
    if (retval != 0)
      return retval;
  }

  if (stackp > aa_depth)
    return 0;

  /* Find attack moves. These are typically ataris but may also be
   * more general.
   */
  num_attack_moves = atari_atari_find_attack_moves(color, minsize,
						   attack_moves, targets);

  /* Try the attacking moves and let the opponent defend. Then call
   * ourselves recursively.
   */
  for (k = 0; k < num_attack_moves; k++) {
    int aa_val;
    int str = targets[k];
    int apos = attack_moves[k];
    int bpos;
    int r;
    
    if (!trymove(apos, color, "do_atari_atari-A", str, EMPTY, NO_MOVE))
      continue;
	
    /* Try to defend the stone (str) which is threatened. */
    aa_val = countstones(str);

    /* Pick up defense moves. */
    num_defense_moves = atari_atari_find_defense_moves(str, defense_moves);
    
    for (r = 0; r < num_defense_moves; r++) {
      bpos = defense_moves[r];

      if (trymove(bpos, other, "do_atari_atari-B", str, EMPTY, NO_MOVE)) {
	int new_aa_val;
	/* These moves may have been irrelevant for later
	 * reading, so in order to avoid horizon problems, we
	 * need to temporarily increase the depth values.
	 */
	modify_depth_values(2);
	new_aa_val = do_atari_atari(color, NULL, defense_point,
				    apos, save_verbose, minsize);
	modify_depth_values(-2);
	if (new_aa_val < aa_val)
	  aa_val = new_aa_val;
	popgo();
      }

      /* Defense successful, no need to try any further. */
      if (aa_val == 0)
	break;
    }

    /* Undo the attacking move. */
    popgo();

    if (aa_val == 0)
      continue;

    /* atari_atari successful */
    if (save_verbose || (debug & DEBUG_ATARI_ATARI)) {
      gprintf("%oThe worm %1m can be attacked at %1m after ", str, apos);
      dump_stack();
    }

    if (attack_point)
      *attack_point = apos;

    if (defense_point) {
      if (!find_defense(str, defense_point))
	*defense_point = NO_MOVE;

      /* If no defense point is known and (apos) is a safe
       * move for other, it probably defends the combination.
       */
      if ((*defense_point == NO_MOVE || !safe_move(*defense_point, other))
	  && safe_move(apos, other))
	*defense_point = apos;
    }
    
    DEBUG(DEBUG_ATARI_ATARI, "%oreturn value:%d (%1m)\n", aa_val, str);
    return aa_val;
  }
    
  /* No atari_atari attack. */
  return 0;
}


static int
atari_atari_succeeded(int color, int *attack_point, int *defense_point,
		      int last_friendly, int save_verbose, int minsize)
{
  int m, n;
  int other = OTHER_COLOR(color);

  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      int ii = POS(m, n);
      int aa;

      if (board[ii] != other)
       continue;

      if (ii != find_origin(ii))
       continue;

      if (minsize > 0 && countstones(ii) < minsize)
       continue;

      if (get_aa_status(ii) != ALIVE)
       continue;

      if (board[last_friendly] != EMPTY
	  && !adjacent_strings(last_friendly, ii))
       continue;

      if (board[last_friendly] == EMPTY
	  && !liberty_of_string(last_friendly, ii))
       continue;

      if (debug & DEBUG_ATARI_ATARI)
	gprintf("Considering attack of %1m. depth = %d.\n", ii, depth);

      if (attack(ii, &aa)) {
	if (save_verbose || (debug & DEBUG_ATARI_ATARI)) {
	  gprintf("%oThe worm %1m can be attacked at %1m after ", ii, aa);
	  dump_stack();
	}
	if (attack_point) *attack_point = aa;

	/* We look for a move defending the combination.
	 * Normally this is found by find_defense but failing
	 * that, if the attacking move is a safe move for color,
	 * it probably defends.
	 */
	if (defense_point) {
	  if (!find_defense(ii, defense_point)) {
	    if (safe_move(aa, other)) {
	      *defense_point = aa;
	    }
	    /* No defense is found */
	    else {
	      *defense_point = NO_MOVE;
	    }
	  }
	}
	
	DEBUG(DEBUG_ATARI_ATARI, "%oreturn value:%d (%1m)\n",
	      countstones(ii), ii);
	return countstones(ii);
      }
    }
  
  return 0;
}

static int
atari_atari_find_attack_moves(int color, int minsize,
			      int moves[MAX_BOARD * MAX_BOARD],
			      int targets[MAX_BOARD * MAX_BOARD])
{
  int other = OTHER_COLOR(color);
  int pos;
  int k;
  int num_moves = 0;
  int num_threat_moves;
  int threat_moves[MAX_THREAT_MOVES];
  int threat_codes[MAX_THREAT_MOVES];
  int mx[BOARDMAX];

  /* We set mx to 0 for every move added to the moves[] array. */
  memset(mx, 0, sizeof(mx));
  
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (board[pos] != other) 
      continue;
    
    if (pos != find_origin(pos))
      continue;
      
    if (minsize > 0 && countstones(pos) < minsize)
      continue;

    if (get_aa_status(pos) != ALIVE)
      continue;

    /* Pick up general threat moves or simple ataris. */
    if (stackp < aa_threat_depth) {
      memset(threat_moves, 0, sizeof(threat_moves));
      memset(threat_codes, 0, sizeof(threat_codes));
      num_threat_moves = attack_threats(pos, MAX_THREAT_MOVES,
					threat_moves, threat_codes);
      if ((debug & DEBUG_ATARI_ATARI)
	  && num_moves > 0) {
	int i;
	gprintf("Threats on %1m: ", pos);
	for (i = 0; i < num_moves; i++)
	  gprintf("%1m ", moves[i]);
	gprintf("\n");
      }
    }
    else {
      num_threat_moves = findlib(pos, 2, threat_moves);
      if (num_threat_moves != 2)
	continue;
    }

    /* Add the threats on (pos) to the moves[] array, unless forbidden
     * or assumed to be ineffective.
     */
    for (k = 0; k < num_threat_moves; k++) {
      int move = threat_moves[k];
	
      if (mx[move] != 0 || forbidden[move])
	continue;

      if ((accurate_approxlib(move, color, 2, NULL) < 2
	   || !is_atari(move, color))
	  && !safe_move(move, color))
	continue;

      moves[num_moves] = move;
      targets[num_moves] = pos;
      num_moves++;
      mx[move] = 1;
    }
  }

  return num_moves;
}


static int
atari_atari_find_defense_moves(int str, int moves[MAX_BOARD * MAX_BOARD])
{
  int num_moves = 0;
  int move;
  int move2;

  /* Because we know (str) is threatened there is an
   * attack and we can be sure find_defense() will give a
   * useful defense point if it returns non-zero. Usually we
   * would need to call attack_and_defend() to be certain of
   * this.
   */
  if (!find_defense(str, &move))
    return 0;
  moves[num_moves++] = move;

  /* Look for additional defense moves. */
  if (countlib(str) == 1
      && restricted_defend1(str, &move2, EMPTY, 0, 1, &move))
    moves[num_moves++] = move2;

  return num_moves;
}

/* Returns true if a move by (color) at (pos) is atari on something.
 * FIXME: Move this to an appropriate location.
 */

static int
is_atari(int pos, int color)
{
  int other = OTHER_COLOR(color);

  if (!is_legal(pos, color))
    return 0;
  if (board[NORTH(pos)] == other 
      && countlib(NORTH(pos)) == 2)
    return 1;
  if (board[EAST(pos)] == other 
      && countlib(EAST(pos)) == 2)
    return 1;
  if (board[SOUTH(pos)] == other 
      && countlib(SOUTH(pos)) == 2)
    return 1;
  if (board[WEST(pos)] == other 
      && countlib(WEST(pos)) == 2)
    return 1;
  return 0;
}




/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */


