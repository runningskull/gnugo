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


/* This file contains functions that deals with threats and, 
 * especially, combinations of threats.
 */

#include "gnugo.h"

#include <string.h>

#include "liberty.h"
#include "gg_utils.h"
#include "patterns.h"


static void find_double_threats(int color);

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
  char defense_points[BOARDMAX];
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
  if (aa_val > 0) {
    if (save_verbose)
      gprintf("Combination attack for %C with size %d found at %1m\n",
	      color, aa_val, attack_point);
    add_my_atari_atari_move(attack_point, aa_val);
  }
  
  aa_val = atari_atari(other, &attack_point, defense_points, save_verbose);
  if (aa_val > 0) {
    int pos;
    if (save_verbose)
      gprintf("Combination attack for %C with size %d found at %1m\n",
	      other, aa_val, attack_point);
    
    for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
      if (ON_BOARD(pos) && defense_points[pos]) {
	add_your_atari_atari_move(pos, aa_val);
	if (save_verbose)
	  gprintf("- defense at %1m\n", pos);
      }
    }
  }
  verbose = save_verbose;
}


#define MAX_THREATENED_STRINGS  10  /* Should be enough for one intersection */

static void
find_double_threats(int color)
{
  int ii;
  int k;
  int l;

  for (ii = BOARDMIN; ii < BOARDMAX; ii++) {
    int num_a_threatened_groups;
    int a_threatened_groups[MAX_THREATENED_STRINGS];

    if (!ON_BOARD(ii))
      continue;

    /* Generate an EITHER_MOVE move reasons for each pair of the 
     * threatened strings.  We must also remove the threats, because
     * otherwise we would get followup points for them as well.
     *
     * FIXME: 
     *   - This is perhaps not the best way to do it, but realistically
     *     it will be seldom that more than two strings are threatened
     *     at the same point.  Still, we should find a better way.
     *   - EITHER_MOVE should be generalized to more than two strings.
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
		add_either_move(ii, ATTACK_STRING, a_threatened_groups[k], 
				ATTACK_STRING, a_threatened_groups[l]);
		remove_attack_threat_move(ii, a_threatened_groups[k]);
		remove_attack_threat_move(ii, a_threatened_groups[l]);
	      }
	    }
	    else if (!defend_both(a_threatened_groups[k],
				  a_threatened_groups[l])) {
	      add_either_move(ii, ATTACK_STRING, a_threatened_groups[k], 
			      ATTACK_STRING, a_threatened_groups[l]);
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

/* Local struct to keep track of atari_atari attack moves and what
 * they threat.
 */
#define AA_MAX_TARGETS_PER_MOVE 4

#define MAX_AA_DIST 5

struct aa_move {
  int move;
  int target[AA_MAX_TARGETS_PER_MOVE];
};

#define AA_MAX_MOVES MAX_BOARD * MAX_BOARD  
static int aa_status[BOARDMAX]; /* ALIVE, DEAD or CRITICAL */
static int forbidden[BOARDMAX];
static int aa_values[BOARDMAX];
static void compute_aa_status(int color, const char safe_stones[BOARDMAX]);
static void compute_aa_values(int color);
static int get_aa_status(int pos);
static int do_atari_atari(int color, int *attack_point, int *defense_point,
			  char all_potential_defenses[BOARDMAX],
			  int last_friendly, int save_verbose, int minsize,
			  char goal[BOARDMAX]);
static int atari_atari_succeeded(int color, int *attack_point,
                                int *defense_point, int last_friendly,
                                int save_verbose, int minsize);
static void atari_atari_find_attack_moves(int color, int minsize,
					  struct aa_move attacks[AA_MAX_MOVES],
					  char goal[BOARDMAX]);
static void atari_atari_attack_patterns(int color, int minsize,
					struct aa_move attacks[AA_MAX_MOVES],
					char goal[BOARDMAX]);
static void atari_atari_attack_callback(int anchor, int color,
					struct pattern *pattern,
					int ll, void *data);
static int atari_atari_find_defense_moves(int targets[AA_MAX_TARGETS_PER_MOVE],
					  int moves[AA_MAX_MOVES]);
static int get_aa_value(int str);
static int update_aa_goal(char goal[BOARDMAX], char new_goal[BOARDMAX],
			  int apos, int color);
static void aa_init_moves(struct aa_move attacks[AA_MAX_MOVES]);
static void aa_add_move(struct aa_move attacks[AA_MAX_MOVES],
			int move, int target);
static int aa_move_known(struct aa_move attacks[AA_MAX_MOVES],
			 int move, int target);
static void aa_sort_moves(struct aa_move attacks[AA_MAX_MOVES]);

/* Set to 1 if you want verbose traces from this function. */

int
atari_atari(int color, int *attack_move, char defense_moves[BOARDMAX],
	    int save_verbose)
{
  int other = OTHER_COLOR(color);
  int apos;
  int dpos;
  int aa_val;
  char saved_defense_moves[BOARDMAX];

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

  compute_aa_status(color, NULL);
  compute_aa_values(color);
  
  if (defense_moves)
    memset(defense_moves, 0, BOARDMAX);
  aa_val = do_atari_atari(color, &apos, &dpos, defense_moves, NO_MOVE,
			  save_verbose, 0, NULL);

  if (aa_val == 0)
    return 0;

  /* We try excluding the first atari found and see if the
   * combination still works. Repeat until failure.
   */
  while (1) {
    int new_aa_val;
    
    if (attack_move)
      *attack_move = apos;
    
    forbidden[apos] = 1;
    if (defense_moves) {
      memcpy(saved_defense_moves, defense_moves, BOARDMAX);
      memset(defense_moves, 0, BOARDMAX);
    }
    new_aa_val = do_atari_atari(color, &apos, &dpos, defense_moves, NO_MOVE,
				save_verbose, aa_val, NULL);

    /* The last do_atari_atari call fails. When do_atari_atari fails,
     * it does not change the value of (apos), so these correspond
     * to a move that works and is necessary.
     */
    if (new_aa_val == 0)
      break;
    else
      aa_val = new_aa_val;
  }

  if (defense_moves) {
    int pos;
    memcpy(defense_moves, saved_defense_moves, BOARDMAX);
    /* defense_moves[] contains potential defense moves. Now we
     * examine which of them really work.
     */
    forbidden[apos] = 0;
    for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
      if (!ON_BOARD(pos) || !defense_moves[pos])
	continue;

      if (!trymove(pos, other, "atari_atari", NO_MOVE, EMPTY, NO_MOVE)) {
	defense_moves[pos] = 0;
	if (save_verbose)
	  gprintf("%1m deleted defense point, illegal\n", pos);
	continue;
      }

      if (attack(pos, NULL)) {
	defense_moves[pos] = 0;
	popgo();
	if (save_verbose)
	  gprintf("%1m deleted defense point, unsafe\n", pos);
	continue;
      }
      
      if (do_atari_atari(color, &apos, &dpos, NULL, NO_MOVE,
			 save_verbose, aa_val, NULL) > 0) {
	if (save_verbose)
	  gprintf("%1m deleted defense point, didn't work\n", pos);
	defense_moves[pos] = 0;
      }
      
      popgo();
    }
  }
  return aa_val;
}


/* Wrapper around atari_atari_blunder_size. Check whether a
 * combination attack of size at least minsize appears after move
 * at (move) has been made.
 * The arrays saved_dragons[] and saved_worms[] should be one for
 * stones belonging to dragons or worms respectively, which are
 * supposedly saved by (move).
 */
int
atari_atari_confirm_safety(int color, int move, int *defense, int minsize,
			   const char saved_dragons[BOARDMAX],
			   const char saved_worms[BOARDMAX])
{
  char safe_stones[BOARDMAX];

  mark_safe_stones(color, move, saved_dragons, saved_worms, safe_stones);

  return (atari_atari_blunder_size(color, move, defense, safe_stones)
	  >= minsize);
}
 

/* This function checks whether any new combination attack appears after
 * move at (move) has been made, and returns its size (in points).
 * safe_stones marks which of our stones are supposedly safe after this move.
 */
int
atari_atari_blunder_size(int color, int move, int *defense,
			 const char safe_stones[BOARDMAX])
{
  int apos;
  int defense_point = NO_MOVE, after_defense_point = NO_MOVE;
  int aa_val, after_aa_val;
  int other = OTHER_COLOR(color);

  /* If aa_depth is too small, we can't see any combination attacks,
   * so in this respect the move is safe enough.
   */
  if (aa_depth < 2)
    return 0;

  memset(forbidden, 0, sizeof(forbidden));

  /* FIXME: Maybe these should be moved after the tryko() below? */
  compute_aa_status(other, safe_stones);
  compute_aa_values(other);

  /* Accept illegal ko capture here. */
  if (!tryko(move, color, NULL, EMPTY, NO_MOVE))
    /* Really shouldn't happen. */
    abortgo(__FILE__, __LINE__, "trymove", I(move), J(move));
  increase_depth_values();

  aa_val = do_atari_atari(other, &apos, &defense_point, NULL,
			  NO_MOVE, 0, 0, NULL);
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
    return 0;
  }

  while (aa_val >= after_aa_val && defense_point != NO_MOVE) {
    /* Try dropping moves from the combination and see if it still
     * works. What we really want is to get the proper defense move
     * into (*defense).
     */
    after_defense_point = defense_point;
    forbidden[apos] = 1;
    aa_val = do_atari_atari(other, &apos, &defense_point, NULL,
			    NO_MOVE, 0, aa_val, NULL);
  }

  popgo();
  decrease_depth_values();
  /* We know that a combination exists, but we don't know if
   * the original move at (aa) was really relevant. So we
   * try omitting it and see if a combination is still found.
   */
  compute_aa_status(other, NULL);
  compute_aa_values(other);
  aa_val = do_atari_atari(other, NULL, NULL, NULL, NO_MOVE, 0, 0, NULL);
  if (after_aa_val - aa_val > 0) {
    if (defense)
      *defense = after_defense_point;
    return after_aa_val - aa_val;
  }
  else
    return 0;
}


/* ---------------------------------------------------------------- */
/*                Helper functions for atari_atari.                 */
/* ---------------------------------------------------------------- */


/* Helper function for computing the aa_status for all opponent's strings.
 * If safe_stones is given, we just copy the information from there.
 */

static void
compute_aa_status(int color, const char safe_stones[BOARDMAX])
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
      if (safe_stones) {
	if (safe_stones[pos])
	  aa_status[pos] = ALIVE;
	else
	  aa_status[pos] = DEAD;
      }
      else {
	if (dragon[pos].status == DEAD)
	  aa_status[pos] = DEAD;
	else if (dragon[pos].status == CRITICAL)
	  aa_status[pos] = CRITICAL;
	else if (worm[pos].attack_codes[0] != 0) {
	  if (worm[pos].defense_codes[0] != 0)
	    aa_status[pos] = CRITICAL;
	  else
	    aa_status[pos] = DEAD;
	}
	else
	  aa_status[pos] = ALIVE;
      }
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
 *
 * The goal array limits where we are allowed to consider threats.
 * Only strings for which goal is set to 1 may be threatened. If goal
 * is NULL, anything may be attacked. Thus goal is typically NULL when
 * do_atari_atari() is called from an external function. After the
 * first threat has been made, the goal array is set to one in a
 * neighborhood of the move and after subsequent threats it is
 * expanded with neighborhoods of those moves. The details of this can
 * be found in the function update_aa_goal().
 */

static int
do_atari_atari(int color, int *attack_point, int *defense_point,
	       char all_potential_defenses[BOARDMAX], int last_friendly,
	       int save_verbose, int minsize, char goal[BOARDMAX])
{
  int other = OTHER_COLOR(color);
  int k;
  struct aa_move attacks[AA_MAX_MOVES];
  int num_defense_moves;
  int defense_moves[AA_MAX_MOVES];
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
    if (retval != 0) {
      if (sgf_dumptree)
	/* FIXME: Better message. */
	sgftreeAddComment(sgf_dumptree, "attack found");
      return retval;
    }
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
  atari_atari_find_attack_moves(color, minsize, attacks, goal);
  sgf_dumptree = save_sgf_dumptree;
  count_variations = save_count_variations;

  /* Try the attacking moves and let the opponent defend. Then call
   * ourselves recursively.
   */
  for (k = 0; attacks[k].move != NO_MOVE; k++) {
    int aa_val;
    int str = attacks[k].target[0];
    int apos = attacks[k].move;
    int bpos;
    int r;
    
    if (!trymove(apos, color, "do_atari_atari-A", str, EMPTY, NO_MOVE))
      continue;
    
    if (all_potential_defenses) {
      all_potential_defenses[apos] = 1;
      if (countlib(apos) <= 2) {
	int libs[2];
	int num_libs = findlib(apos, 2, libs);
	all_potential_defenses[libs[0]] = 1;
	if (num_libs == 2)
	  all_potential_defenses[libs[1]] = 1;
      }
    }

    if (!IS_STONE(board[str])) {
      /* Error situation. This could be caused by a wrong matcher status. */
      if (save_verbose || (debug & DEBUG_ATARI_ATARI))
	gprintf("%oError condition found by atari_atari\n");
      popgo();
      return 0;
    }

    /* Try to defend the stone (str) which is threatened. */
    aa_val = get_aa_value(str);

    /* Pick up defense moves. */
    save_sgf_dumptree = sgf_dumptree;
    save_count_variations = count_variations;
    sgf_dumptree = NULL;
    count_variations = 0;
    num_defense_moves = atari_atari_find_defense_moves(attacks[k].target,
						       defense_moves);
    sgf_dumptree = save_sgf_dumptree;
    count_variations = save_count_variations;
    
    for (r = 0; r < num_defense_moves; r++) {
      bpos = defense_moves[r];

      if (all_potential_defenses)
	all_potential_defenses[bpos] = 1;

      if (trymove(bpos, other, "do_atari_atari-B", str, EMPTY, NO_MOVE)) {
	int new_aa_val;
	char new_goal[BOARDMAX];
	/* These moves may have been irrelevant for later
	 * reading, so in order to avoid horizon problems, we
	 * need to temporarily increase the depth values.
	 */
	modify_depth_values(2);
	update_aa_goal(goal, new_goal, apos, color);
	new_aa_val = do_atari_atari(color, NULL, defense_point,
				    all_potential_defenses,
				    apos, save_verbose, minsize, new_goal);
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
    if (num_defense_moves == 0) {
      if (save_verbose || (debug & DEBUG_ATARI_ATARI)) {
	gprintf("%oThe worm %1m can be attacked at %1m after ", str, apos);
	dump_stack();
      }
      if (sgf_dumptree)
	/* FIXME: Better message. */
	sgftreeAddComment(sgf_dumptree, "attack found");
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
    
    TRACE_ATARI_ATARI("%oreturn value:%d (%1m)\n", aa_val, str);
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

      if (minsize > 0
	  && get_aa_value(ii) < minsize)
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
	    if (safe_move(aa, other))
	      *defense_point = aa;
	    else
	      *defense_point = NO_MOVE;
	  }
	}
	
	TRACE_ATARI_ATARI("%oreturn value:%d (%1m)\n",
	      get_aa_value(ii), ii);
	return get_aa_value(ii);
      }
    }
  
  return 0;
}

#define MAX_THREAT_MOVES  MAX_TACTICAL_POINTS

static void
atari_atari_find_attack_moves(int color, int minsize,
			      struct aa_move attacks[AA_MAX_MOVES],
			      char goal[BOARDMAX])
{
  int k;
  int r;

  aa_init_moves(attacks);

  atari_atari_attack_patterns(color, minsize, attacks, goal);

  /* Sort the attack moves. */
  aa_sort_moves(attacks);
  
  if (debug & DEBUG_ATARI_ATARI) {
    gprintf("Attack moves:");
    for (k = 0; k < AA_MAX_MOVES && attacks[k].move != NO_MOVE; k++) {
      gprintf("%o %1m(", attacks[k].move);
      for (r = 0; r < AA_MAX_TARGETS_PER_MOVE; r++) {
	if (attacks[k].target[r] == NO_MOVE)
	  break;
	gprintf("%o%s%1m", r == 0 ? "" : ",", attacks[k].target[r]);
      }
      gprintf("%o)");
    }
    gprintf("%o\n");
  }
}

/* FIXME: Move these to a struct and pass to callback through the
 * *data parameter.
 */
static int current_minsize;
static struct aa_move *current_attacks;
static int conditional_attack_point[BOARDMAX];

static void
atari_atari_attack_patterns(int color, int minsize,
			    struct aa_move attacks[AA_MAX_MOVES],
			    char goal[BOARDMAX])
{
  char revised_goal[BOARDMAX];
  current_minsize = minsize;
  current_attacks = attacks;
  memset(conditional_attack_point, 0, sizeof(conditional_attack_point));

  /* If goal is NULL and there are forbidden moves we need to compute
   * a new goal around the forbidden moves.
   */
  if (goal == NULL && update_aa_goal(goal, revised_goal, NO_MOVE, color))
    goal = revised_goal;

  matchpat(atari_atari_attack_callback, color, &aa_attackpat_db, NULL, goal);
}

/* Try to attack every X string in the pattern, whether there is an attack
 * before or not. Only exclude already known attacking moves.
 */
static void
atari_atari_attack_callback(int anchor, int color,
			    struct pattern *pattern, int ll, void *data)
{
  int move;
  int k;
  UNUSED(data);

  move = AFFINE_TRANSFORM(pattern->move_offset, ll, anchor);

  if (forbidden[move])
    return;
  
  /* If the pattern has a constraint, call the autohelper to see
   * if the pattern must be rejected.
   */
  if (pattern->autohelper_flag & HAVE_CONSTRAINT)
    if (!pattern->autohelper(ll, move, color, 0))
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
      int str = find_origin(AFFINE_TRANSFORM(pattern->patn[k].offset,
					     ll, anchor));

      if (current_minsize > 0
	  && get_aa_value(str) < current_minsize)
	continue;

      if (aa_move_known(current_attacks, move, str))
	continue;

      if (get_aa_status(str) != ALIVE)
	continue;

      /* Usually we don't want to play self atari. However, if we
       * capture in snapback it's okay. For s class patterns we don't
       * have this requirement.
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
	int attack_point = NO_MOVE;

	if (!board[str])
	  acode = WIN;
	else
	  acode = attack(str, &attack_point);

	popgo();

	if (acode != 0) {
	  if ((pattern->class & CLASS_c)
	      && !aa_move_known(current_attacks, move, NO_MOVE)) {
	    /* Conditional pattern. */
	    TRACE_ATARI_ATARI(
		  "aa_attack pattern %s+%d (conditional) found threat on %1m at %1m with code %d\n",
		  pattern->name, ll, str, move, acode);
	    if (conditional_attack_point[move] == NO_MOVE)
	      conditional_attack_point[move] = str;
	    else if (conditional_attack_point[move] != str) {
	      aa_add_move(current_attacks, move,
			  conditional_attack_point[move]);
	      aa_add_move(current_attacks, move, str);
	    }
	  }
	  else {
	    aa_add_move(current_attacks, move, str);
	    TRACE_ATARI_ATARI(
		  "aa_attack pattern %s+%d found threat on %1m at %1m with code %d\n",
		  pattern->name, ll, str, move, acode);
	  }
	}
      }
    }
  }
}


static int
atari_atari_find_defense_moves(int targets[AA_MAX_TARGETS_PER_MOVE],
			       int moves[AA_MAX_MOVES])
{
  int num_moves = 0;
  int move;
  int k;
  int liberties;
  int libs[4];
  int neighbors;
  int adjs[MAXCHAIN];
  int mx[BOARDMAX];
  int r, s;

  memset(mx, 0, sizeof(mx));

  for (r = 0; r < AA_MAX_TARGETS_PER_MOVE && targets[r] != NO_MOVE; r++) {
    int str = targets[r];

    /* If the attack move happened to remove (str), there's no defense. */
    if (board[str] == EMPTY)
      continue;
    
    /* Because we know (str) is threatened there is an
     * attack and we can be sure find_defense() will give a
     * useful defense point if it returns non-zero. Usually we
     * would need to call attack_and_defend() to be certain of
     * this.
     */
    if (!find_defense(str, &move))
      continue;
    moves[num_moves++] = move;
    if (num_moves == AA_MAX_MOVES)
      return num_moves;
    mx[move] = 1;
    
    /* Consider all moves to attack a neighbor or to play on a liberty. */
    liberties = findlib(str, 4, libs);
    for (k = 0; k < liberties; k++) {
      if (!mx[libs[k]]
	  && trymove(libs[k], board[str], "aa_defend-A", str,
		     EMPTY, NO_MOVE)) {
	if (attack(str, NULL) == 0) {
	  moves[num_moves++] = libs[k];
	  mx[libs[k]] = 1;
	}
	popgo();
	if (num_moves == AA_MAX_MOVES)
	  return num_moves;
      }
    }
    
    neighbors = chainlinks(str, adjs);
    for (k = 0; k < neighbors; k++) {
      int attack_point;
      if (attack(adjs[k], &attack_point) == WIN
	  && !mx[attack_point]) {
	moves[num_moves++] = attack_point;
	if (num_moves == AA_MAX_MOVES)
	  return num_moves;
	mx[attack_point] = 1;
      }

      /* If the neighbor has at most three liberties, try all of them
       * for defense, except self-ataris.
       */
      liberties = findlib(adjs[k], 3, libs);
      if (liberties <= 3) {
	for (s = 0; s < liberties; s++) {
	  if (!mx[libs[s]]
	      && !is_self_atari(libs[s], board[str])
	      && trymove(libs[s], board[str], "aa_defend-B", str,
			 EMPTY, NO_MOVE)) {
	    if (attack(str, NULL) == 0) {
	      moves[num_moves++] = libs[s];
	      mx[libs[s]] = 1;
	    }
	    popgo();
	    if (num_moves == AA_MAX_MOVES)
	      return num_moves;
	  }
	}
      }
    }
    
    if (debug & DEBUG_ATARI_ATARI) {
      gprintf("Defense moves for %1m:", str);
      for (k = 0; k < num_moves; k++)
	gprintf("%o %1m", moves[k]);
      gprintf("%o\n");
    }
  }

  return num_moves;
}


/* Try to guess the value of the strings. We do this by adding twice
 * the number of stones to the number of liberties and second order
 * liberties within the moyo around the string. This is of course
 * quite crude since it doesn't take into account any strategic
 * effects, e.g. a string being cutting stones. 
 */
static void
compute_aa_values(int color)
{
  int other = OTHER_COLOR(color);
  int pos;
  int value;
  int liberties;
  int libs[MAXLIBS];
  int mx[BOARDMAX];
  int r, k;

  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (board[pos] != other
	|| pos != find_origin(pos)
	|| aa_status[pos] != ALIVE) {
      aa_values[pos] = 0;
      continue;
    }
      
    memset(mx, 0, sizeof(mx));
    liberties = findlib(pos, MAXLIBS, libs);
    value = 2 * countstones(pos);

    for (r = 0; r < liberties; r++) {
      if (!mx[libs[r]]
	  && (whose_moyo(&initial_black_influence, libs[r]) == other
	      || whose_moyo(&initial_white_influence, libs[r]) == other)) {
	mx[libs[r]] = 1;
	value++;
      }
      for (k = 0; k < 4; k++) {
	int librd = libs[r] + delta[k];
	if (!ON_BOARD1(librd) || mx[librd])
	  continue;
	mx[librd] = 1;
	if (board[librd] == EMPTY
	    && (whose_moyo(&initial_black_influence, librd) == other
		|| (whose_moyo(&initial_white_influence, librd) == other)))
	  value++;
      }
    }

    aa_values[pos] = value;
    if (1)
      TRACE_ATARI_ATARI("aa_value for %1m = %d\n", pos, value);
  }
}

/* The aa_value for a string is the sum of the aa_values for all
 * included strings in the original position. This will systematically
 * overvalue strings which consist of multiple original strings, but
 * this is okay since the defender very rarely should defend a string
 * first and then sacrifice it later.
 */
static int
get_aa_value(int str)
{
  int stones[MAX_BOARD * MAX_BOARD];
  int k;
  int num_stones = findstones(str, MAX_BOARD * MAX_BOARD, stones);
  int value = 0;
  
  for (k = 0; k < num_stones; k++)
    value += aa_values[stones[k]];

  return value;
}


/* update_aa_goal(goal, new_goal, apos, color) extends the goal array
 * with vertices in a neighborhood of apos. The algorithm is that
 * starting at apos, a distance measure is computed to nearby
 * vertices. The distance increases with one for each step through
 * empty vertices and by a liberty depending number when passing
 * through strings of the attacked color. Strings with 3 or fewer
 * liberties are free to pass through while strings with more
 * liberties cost (libs - 3) to pass through. Stones with a distance
 * of 5 or less are included in the goal.
 *
 * Additionally neighborhoods of the moves in the forbidden array are
 * included in the goal, to make it possible to limit the goal to a
 * specific area from the beginning. This is needed when trying to
 * decide which moves are relevant to the combination.
 */

#define ENQUEUE(pos, dist) \
    do { \
      if ((dist) <= MAX_AA_DIST) { \
        if (dists[pos] == 0) { \
          queue[queue_end++] = (pos); \
          dists[pos] = (dist); \
        } \
        else if (dists[pos] < (dist)) \
          dists[pos] = (dist); \
      } \
    } while (0);

static int
update_aa_goal(char goal[BOARDMAX], char new_goal[BOARDMAX], int apos,
	       int color)
{
  int other = OTHER_COLOR(color);
  int dists[BOARDMAX];
  int queue[MAX_BOARD * MAX_BOARD];
  int queue_end = 0;
  int k, r, s;
  int pos;
  
  if (goal == NULL)
    memset(new_goal, 0, BOARDMAX);
  else
    memcpy(new_goal, goal, BOARDMAX);

  memset(dists, 0, sizeof(dists));

  if (apos != NO_MOVE) {
    dists[apos] = 1;
    queue[queue_end++] = apos;
  }

  if (queue_end == 0)
    return 0;
  
  for (r = 0; r < queue_end; r++) {
    int smallest_dist = MAX_BOARD * MAX_BOARD;
    int best_index = -1;

    gg_assert(queue_end < MAX_BOARD * MAX_BOARD);
    
    for (k = r; k < queue_end; k++) {
      if (dists[queue[k]] < smallest_dist) {
	smallest_dist = dists[queue[k]];
	best_index = k;
      }
    }
    
    if (best_index != r) {
      int tmp = queue[r];
      queue[r] = queue[best_index];
      queue[best_index] = tmp;
    }
    
    pos = queue[r];
    if (board[pos] == other)
      new_goal[pos] = 1;

    /* FIXME: We shouldn't let dead opponent stones stop the
     * propagation of distance.
     *
     * As a partial fix we include pos == apos in a test below.
     */
    for (k = 0; k < 4; k++) {
      int pos2 = pos + delta[k];
      if (!ON_BOARD(pos2))
       continue;
      if ((board[pos] == other || pos == apos) && board[pos2] == EMPTY) {
        ENQUEUE(pos2, dists[pos] + 1);
      }
      else if (board[pos] != other && board[pos2] == other) {
	int stones[MAX_BOARD * MAX_BOARD];
	int size = findstones(pos2, MAX_BOARD * MAX_BOARD, stones);
	int libs = countlib(pos2);
	int deltadist = libs - 3;
	if (deltadist < 0)
	  deltadist = 0;
	for (s = 0; s < size; s++)
	  ENQUEUE(stones[s], dists[pos] + deltadist);
      }
    }
  }
  return 1;
}

/* Initialize an array with atari_atari attacks. The convention is that
 * the array ends when a NO_MOVE is encountered in the move field.
 */
static void
aa_init_moves(struct aa_move attacks[AA_MAX_MOVES])
{
  attacks[0].move = NO_MOVE;
}


/* Add an atari_atari attack move to a struct aa_move array. If the
 * move already is included in the array, we check whether the target
 * also is known for that move and add it if not.
 */
static void
aa_add_move(struct aa_move attacks[AA_MAX_MOVES], int move, int target)
{
  int k;
  int r;

  for (k = 0; k < AA_MAX_MOVES; k++)
    if (attacks[k].move == move || attacks[k].move == NO_MOVE)
      break;

  /* If the array is full, give up. */
  if (k == AA_MAX_MOVES)
    return;

  target = find_origin(target);

  /* New move. */
  if (attacks[k].move == NO_MOVE) {
    attacks[k].move = move;
    attacks[k].target[0] = target;
    if (AA_MAX_TARGETS_PER_MOVE > 0)
      attacks[k].target[1] = NO_MOVE;

    if (k < AA_MAX_MOVES - 1)
      attacks[k+1].move = NO_MOVE;

    return;
  }

  /* Known move, maybe new target. */
  for (r = 0; r < AA_MAX_TARGETS_PER_MOVE; r++)
    if (attacks[k].target[r] == target || attacks[k].target[r] == NO_MOVE)
      break;

  /* No place for more targets. */
  if (r == AA_MAX_TARGETS_PER_MOVE)
    return;

  /* Target known. */
  if (attacks[k].target[r] == target)
    return;

  /* Add target. */
  attacks[k].target[r] = target;
  if (r < AA_MAX_TARGETS_PER_MOVE - 1)
    attacks[k].target[r + 1] = NO_MOVE;
}

/* Check whether an atari_atari attack move is included in an struct
 * aa_move array. If target is not NO_MOVE, we also require that the
 * target is known for the move.
 */
static int
aa_move_known(struct aa_move attacks[AA_MAX_MOVES], int move, int target)
{
  int k;
  int r;

  for (k = 0; k < AA_MAX_MOVES; k++)
    if (attacks[k].move == move || attacks[k].move == NO_MOVE)
      break;

  /* If the array is full, give up and claim the move to be known. */
  if (k == AA_MAX_MOVES)
    return 1;

  /* Unknown move. */
  if (attacks[k].move == NO_MOVE)
    return 0;

  /* Move known, but how about the target?
   * If no target specified, just return 1.
   */
  if (target == NO_MOVE)
    return 1;

  target = find_origin(target);
  for (r = 0; r < AA_MAX_TARGETS_PER_MOVE; r++)
    if (attacks[k].target[r] == target || attacks[k].target[r] == NO_MOVE)
      break;

  /* No place for more targets. Give up and claim the target to be known. */
  if (r == AA_MAX_TARGETS_PER_MOVE)
    return 1;

  /* Target known. */
  if (attacks[k].target[r] == target)
    return 1;

  /* Unknown target. */
  return 0;
}


/* Auxiliary function for aa_sort_moves(). */
static int
target_comp_func(const void *a, const void *b)
{
  int asize = get_aa_value(*((const int *) a));
  int bsize = get_aa_value(*((const int *) b));
  return asize - bsize;
}


/* Auxiliary function for aa_sort_moves(). */
static int
move_comp_func(const void *a, const void *b)
{
  const struct aa_move *aa = a;
  const struct aa_move *bb = b;
  int asize = get_aa_value(aa->target[0]);
  int bsize = get_aa_value(bb->target[0]);
  return asize - bsize;
}

/* Sort the attack moves. For each move the targets are sorted in
 * decreasing size. Then the moves are sorted with increasing size
 * of their first target.
 */
static void
aa_sort_moves(struct aa_move attacks[AA_MAX_MOVES])
{
  int k;
  int r;
  int number_of_attacks;
  int number_of_targets;

  for (k = 0; k < AA_MAX_MOVES && attacks[k].move != NO_MOVE; k++) {
    for (r = 0; r < AA_MAX_TARGETS_PER_MOVE; r++)
      if (attacks[k].target[r] == NO_MOVE)
	break;
    number_of_targets = r;
    gg_sort(attacks[k].target, number_of_targets,
	    sizeof(attacks[k].target[0]), target_comp_func);
  }
  number_of_attacks = k;
  gg_sort(attacks, number_of_attacks, sizeof(attacks[0]), move_comp_func);
}

/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
