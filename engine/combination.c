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

/* FIXME: Move much of atari_atari here. */

#include "liberty.h"
#include "gnugo.h"
#include "gg_utils.h"


static void find_double_threats(int color);


/* Generate move reasons for combination attacks and defenses against
 * them.
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
      int num_d_threatened_groups;
      int d_threatened_groups[MAX_THREATENED_STRINGS];

      ii = POS(i, j);
      
      /* Generate ATTACK_EITHER_MOVE move reasons for each pair of the 
       * threatened strings.  
       *
       * FIXME: 
       *   - This is perhaps not the best way to do it, but realistically
       *     it will be seldom that more than two strings are threatened
       *     at the same point.  Still, we should find a better way.
       *   - attack_either_move should be generalized to more than two strings.
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
	       */
	      if (!defend_both(a_threatened_groups[k], a_threatened_groups[l]))
		add_attack_either_move(ii, a_threatened_groups[k], 
				       a_threatened_groups[l]);
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

/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */


