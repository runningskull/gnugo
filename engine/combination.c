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
#include "patterns.h"


static void find_double_threats(int color);
static int atari_atari_find_attack_moves(int color, int minsize,
					 int moves[MAX_BOARD * MAX_BOARD],
					 int targets[MAX_BOARD * MAX_BOARD]);
static int atari_atari_attack_patterns(int color, int minsize,
				       int moves[MAX_BOARD * MAX_BOARD],
				       int targets[MAX_BOARD * MAX_BOARD]);
static void atari_atari_attack_callback(int m, int n, int color,
					struct pattern *pattern,
					int ll, void *data);
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
  int attack_point;
  int defense_point;
  int other = OTHER_COLOR(color);
  int aa_val;

  /* Find intersections with multiple threats. */
  find_double_threats(color);

  save_verbose = verbose;
  if (verbose > 0)
    verbose--;

  if (save_verbose)
    gprintf("\nlooking for combination attacks ...\n");
  
  aa_val = atari_atari(color, &attack_point, NULL, save_verbose);
  if (aa_val) {
    if (save_verbose)
      gprintf("Combination attack for %C with size %d found at %1m\n",
	      color, aa_val, attack_point);
    add_my_atari_atari_move(attack_point, aa_val);
  }
  
  aa_val = atari_atari(other, &attack_point, &defense_point, save_verbose);
  if (aa_val && safe_move(defense_point, color)) {
    if (save_verbose)
      gprintf("Combination attack for %C with size %d found at %1m, defense at %1m\n",
	      other, aa_val, attack_point, defense_point);
    add_your_atari_atari_move(defense_point, aa_val);
  }
  
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

#define USE_ATARI_ATARI_PATTERNS 0

static int aa_status[BOARDMAX]; /* ALIVE, DEAD or CRITICAL */
static int forbidden[BOARDMAX];
static int vital_string[BOARDMAX]; /* May not sacrifice these stones. */
static void compute_aa_status(int color, int saved_dragons[BOARDMAX],
			      int saved_worms[BOARDMAX]);
static int get_aa_status(int pos);
static int is_vital_string(int str);
static int do_atari_atari(int color, int *attack_point,
                         int *defense_point, int last_friendly,
			  int save_verbose, int minsize);
static int atari_atari_succeeded(int color, int *attack_point,
                                int *defense_point, int last_friendly,
                                int save_verbose, int minsize);

/* Set to 1 if you want verbose traces from this function. */

int
atari_atari(int color, int *attack_move, int *defense_move, int save_verbose)
{
  int apos;
  int dpos;
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

  compute_aa_status(color, NULL, NULL);
  
  aa_val = do_atari_atari(color, &apos, &dpos, NO_MOVE,
			  save_verbose, 0);

  if (aa_val == 0)
    return 0;

  /* We try excluding the first atari found and see if the
   * combination still works. Repeat until failure.
   */
  while (1) {
    int new_aa_val;
    forbidden[apos] = 1;
    new_aa_val = do_atari_atari(color, &apos, &dpos, NO_MOVE,
				save_verbose, aa_val);

    /* The last do_atari_atari call fails. When do_atari_atari fails,
     * it does not change the value of (apos), so these correspond
     * to a move that works and is necessary.
     */
    if (new_aa_val == 0) {
      if (attack_move)
	*attack_move = apos;
      if (defense_move)
	*defense_move = dpos;
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
 * (move). If this happens, (*defense) points to a move which prevents
 * this blunder.
 *
 * The arrays saved_dragons[] and saved_worms[] should be one for
 * stones belonging to dragons or worms respectively, which are
 * supposedly saved by (move). These may be NULL if no stones are
 * supposed to gaving been saved.
 */
int
atari_atari_confirm_safety(int color, int move, int *defense, int minsize,
			   int saved_dragons[BOARDMAX],
			   int saved_worms[BOARDMAX])
{
  int apos;
  int defense_point = NO_MOVE, after_defense_point = NO_MOVE;
  int aa_val, after_aa_val;
  int other = OTHER_COLOR(color);

  /* If aa_depth is too small, we can't see any combination attacks,
   * so in this respect the move is safe enough.
   */
  if (aa_depth < 2)
    return 1;

  memset(forbidden, 0, sizeof(forbidden));

  compute_aa_status(other, saved_dragons, saved_worms);

  /* Accept illegal ko capture here. */
  if (!tryko(move, color, NULL, EMPTY, NO_MOVE))
    /* Really shouldn't happen. */
    abortgo(__FILE__, __LINE__, "trymove", I(move), J(move));
  increase_depth_values();

  aa_val = do_atari_atari(other, &apos, &defense_point, NO_MOVE, 0, minsize);
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
     * into (*defense).
     */
    after_defense_point = defense_point;
    forbidden[apos] = 1;
    aa_val = do_atari_atari(other, &apos, &defense_point, NO_MOVE, 0, aa_val);
  }

  popgo();
  decrease_depth_values();
  /* We know that a combination exists, but we don't know if
   * the original move at (aa) was really relevant. So we
   * try omitting it and see if a combination is still found.
   */
  compute_aa_status(other, NULL, NULL);
  if (do_atari_atari(other, NULL, NULL, NO_MOVE, 0, minsize) >= after_aa_val)
    return 1;
  else {
    if (defense)
      *defense = after_defense_point;
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

  if (USE_ATARI_ATARI_PATTERNS)
    return 0;
  
  if (aa_depth < 2)
    return 0;
  if (verbose > 0)
    verbose--;
  memset(forbidden, 0, sizeof(forbidden));

  compute_aa_status(color, NULL, NULL);

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


/* Helper function for computing the aa_status for a string. It also
 * sets up the vital_string[] array.
 */

static void
compute_aa_status(int color, int saved_dragons[BOARDMAX],
		  int saved_worms[BOARDMAX])
{
  int other = OTHER_COLOR(color);
  int pos;
  SGFTree *save_sgf_dumptree = sgf_dumptree;
  int save_count_variations = count_variations;
  int save_verbose = verbose;
  sgf_dumptree = NULL;
  count_variations = 0;
  if (verbose)
    verbose--;
  
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
      else if (dragon[pos].matcher_status == CRITICAL
	       && (!saved_dragons || !saved_dragons[pos]))
	aa_status[pos] = CRITICAL;
      else if (worm[pos].attack_codes[0] != 0) {
	if (worm[pos].defend_codes[0] != 0) {
	  if (saved_worms && saved_worms[pos])
	    aa_status[pos] = ALIVE;
	  else
	    aa_status[pos] = CRITICAL;
	}
	else
	  aa_status[pos] = DEAD;
      }
      else
	aa_status[pos] = ALIVE;
    }
    else if (ON_BOARD(pos))
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

  /* Set up the vital strings array. This contains stones we can't be
   * allowed to sacrifice. This is used by
   * atari_atari_confirm_safety() to make sure that stones which are
   * supposedly saved by the move can't be captured in a combination
   * attack.
   */

  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (ON_BOARD(pos)
	&& ((saved_dragons && saved_dragons[pos])
	    || (saved_worms && saved_worms[pos])))
      vital_string[pos] = 1;
    else
      vital_string[pos] = 0;
  }
  
  sgf_dumptree = save_sgf_dumptree;
  count_variations = save_count_variations;
  verbose = save_verbose;
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


/* Helper function to examine whether a stone is part of a vital
 * string. Since the vital_string[] array was generated at stackp == 0
 * new stones may have appeared on the board.
 */
static int
is_vital_string(int str)
{
  int stones[MAX_BOARD * MAX_BOARD];
  int n = findstones(str, MAX_BOARD * MAX_BOARD, stones);
  int k;
  for (k = 0; k < n; k++)
    if (vital_string[stones[k]])
      return 1;

  return 0;
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
  SGFTree *save_sgf_dumptree;
  int save_count_variations;
  
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
    int retval;
    save_sgf_dumptree = sgf_dumptree;
    save_count_variations = count_variations;
    sgf_dumptree = NULL;
    count_variations = 0;
    retval = atari_atari_succeeded(color, attack_point, defense_point,
				   last_friendly, save_verbose, minsize);
    sgf_dumptree = save_sgf_dumptree;
    count_variations = save_count_variations;
    if (retval != 0)
      return retval;
  }

  if (stackp > aa_depth)
    return 0;

  /* Find attack moves. These are typically ataris but may also be
   * more general.
   */
  save_sgf_dumptree = sgf_dumptree;
  save_count_variations = count_variations;
  sgf_dumptree = NULL;
  count_variations = 0;
  num_attack_moves = atari_atari_find_attack_moves(color, minsize,
						   attack_moves, targets);
  sgf_dumptree = save_sgf_dumptree;
  count_variations = save_count_variations;

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
    save_sgf_dumptree = sgf_dumptree;
    save_count_variations = count_variations;
    sgf_dumptree = NULL;
    count_variations = 0;
    num_defense_moves = atari_atari_find_defense_moves(str, defense_moves);
    sgf_dumptree = save_sgf_dumptree;
    count_variations = save_count_variations;
    
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
    if (num_defense_moves == 0
	&& (save_verbose || (debug & DEBUG_ATARI_ATARI))) {
      gprintf("%oThe worm %1m can be attacked at %1m after ", str, apos);
      dump_stack();
    }

    if (attack_point)
      *attack_point = apos;

    if (defense_point) {
      save_sgf_dumptree = sgf_dumptree;
      save_count_variations = count_variations;
      sgf_dumptree = NULL;
      count_variations = 0;

      if (!find_defense(str, defense_point))
	*defense_point = NO_MOVE;

      /* If no defense point is known and (apos) is a safe
       * move for other, it probably defends the combination.
       */
      if ((*defense_point == NO_MOVE || !safe_move(*defense_point, other))
	  && safe_move(apos, other))
	*defense_point = apos;

      sgf_dumptree = save_sgf_dumptree;
      count_variations = save_count_variations;
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

      if (minsize > 0 && countstones(ii) < minsize && !is_vital_string(ii))
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

      if (attack(ii, &aa) && !forbidden[aa]) {
	if (save_verbose || (debug & DEBUG_ATARI_ATARI)) {
	  gprintf("%oThe worm %1m can be attacked at %1m after ", ii, aa);
	  dump_stack();
	}
	if (attack_point)
	  *attack_point = aa;

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

  if (USE_ATARI_ATARI_PATTERNS)
    return atari_atari_attack_patterns(color, minsize, moves, targets);
  
  /* We set mx to 0 for every move added to the moves[] array. */
  memset(mx, 0, sizeof(mx));
  
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (board[pos] != other) 
      continue;
    
    if (pos != find_origin(pos))
      continue;
      
    if (minsize > 0 && countstones(pos) < minsize && !is_vital_string(pos))
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

      if ((is_self_atari(move, color)
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

/* FIXME: Move these to a struct and pass to callback through the
 * *data parameter.
 */
static int current_minsize;
static int *current_moves;
static int *current_targets;
static int current_num_moves;
static int mx[BOARDMAX];

static int atari_atari_attack_patterns(int color, int minsize,
				       int moves[MAX_BOARD * MAX_BOARD],
				       int targets[MAX_BOARD * MAX_BOARD])
{
  current_minsize = minsize;
  current_moves = moves;
  current_targets = targets;
  current_num_moves = 0;
  memset(mx, 0, sizeof(mx));
  
  matchpat(atari_atari_attack_callback, color, &aa_attackpat_db, NULL, NULL);
  
  return current_num_moves;
}

/* Try to attack every X string in the pattern, whether there is an attack
 * before or not. Only exclude already known attacking moves.
 */
static void
atari_atari_attack_callback(int m, int n, int color,
			    struct pattern *pattern, int ll, void *data)
{
  int ti, tj;
  int move;
  int k;
  int str1 = NO_MOVE;
  UNUSED(data);

  TRANSFORM(pattern->movei, pattern->movej, &ti, &tj, ll);
  ti += m;
  tj += n;
  move = POS(ti, tj);

  if (mx[move] || forbidden[move])
    return;
  
  /* If the pattern has a constraint, call the autohelper to see
   * if the pattern must be rejected.
   */
  if (pattern->autohelper_flag & HAVE_CONSTRAINT)
    if (!pattern->autohelper(pattern, ll, move, color, 0))
      return;

  /* If the pattern has a helper, call it to see if the pattern must
   * be rejected.
   */
  if (pattern->helper)
    if (!pattern->helper(pattern, ll, move, color))
      return;

  /* Loop through pattern elements in search of X strings to
   * threaten to attack.
   */
  for (k = 0; k < pattern->patlen; ++k) { /* match each point */
    if (pattern->patn[k].att == ATT_X) {
      /* transform pattern real coordinate */
      int x, y;
      int str;
      TRANSFORM(pattern->patn[k].x, pattern->patn[k].y, &x, &y, ll);
      x += m;
      y += n;

      str = worm[POS(x, y)].origin;

      if (str == str1)
	continue;

      if (str1 == NO_MOVE)
	str1 = str;

      if (current_minsize > 0
	  && countstones(str) < current_minsize
	  && !is_vital_string(str))
	continue;

      if (get_aa_status(str) != ALIVE)
	continue;

      /* Usually we don't want to play self atari. However, if we
       * capture in snapback it's okay.
       */
      if (!(pattern->class & CLASS_s) && is_self_atari(move, color)) {
	if (countlib(str) > 2)
	  continue;

	if (!safe_move(move, color))
	  continue;
      }
      
      /*
       * Play (move) and see if there is an attack.
       */
      if (trymove(move, color, "attack_callback", str, EMPTY, NO_MOVE)) {
	int acode;
	if (!board[str])
	  acode = WIN;
	else
	  acode = attack(str, NULL);

	popgo();

	if (acode != 0) {
	  current_moves[current_num_moves] = move;
	  current_targets[current_num_moves] = str;
	  current_num_moves++;
	  mx[move] = 1;
	  DEBUG(DEBUG_ATARI_ATARI,
		"aa_attack pattern %s+%d found threat on %1m at %1m with code %d\n",
		pattern->name, ll, str, move, acode);
	}
      }
    }
  }
}


static int
atari_atari_find_defense_moves(int str, int moves[MAX_BOARD * MAX_BOARD])
{
  int num_moves = 0;
  int move;
  int k;
  int liberties;
  int libs[4];
  int neighbors;
  int adjs[MAXCHAIN];
  int mx[BOARDMAX];

  memset(mx, 0, sizeof(mx));

  /* Because we know (str) is threatened there is an
   * attack and we can be sure find_defense() will give a
   * useful defense point if it returns non-zero. Usually we
   * would need to call attack_and_defend() to be certain of
   * this.
   */
  if (!find_defense(str, &move))
    return 0;
  moves[num_moves++] = move;
  mx[move] = 1;
  
  /* Consider all moves to attack a neighbor or to play on a liberty. */
  liberties = findlib(str, 4, libs);
  for (k = 0; k < liberties; k++) {
    if (!mx[libs[k]]
	&& trymove(libs[k], board[str], "aa_defend-A", str, EMPTY, NO_MOVE)) {
      if (attack(str, NULL) == 0) {
	moves[num_moves++] = libs[k];
	mx[libs[k]] = 1;
      }
      popgo();
    }
  }

  neighbors = chainlinks(str, adjs);
  for (k = 0; k < neighbors; k++) {
    int attack_point;
    if (attack(adjs[k], &attack_point) == WIN
	&& !mx[attack_point]) {
      moves[num_moves++] = attack_point;
      mx[attack_point] = 1;
    }
  }

#if 0
  {
    int move2;
    /* Look for additional defense moves. */
    if (countlib(str) == 1
	&& restricted_defend1(str, &move2, EMPTY, 0, 1, &move))
      moves[num_moves++] = move2;
  }
#endif

  if (0) {
    gprintf("defense moves for %1m:", str);
    for (k = 0; k < num_moves; k++)
      gprintf("%o %1m", moves[k]);
    gprintf("\n");
  }
  
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
  
  if (board[SOUTH(pos)] == other 
      && countlib(SOUTH(pos)) == 2)
    return 1;
  
  if (board[WEST(pos)] == other 
      && countlib(WEST(pos)) == 2)
    return 1;
  
  if (board[NORTH(pos)] == other 
      && countlib(NORTH(pos)) == 2)
    return 1;
  
  if (board[EAST(pos)] == other 
      && countlib(EAST(pos)) == 2)
    return 1;
  
  return 0;
}




/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
