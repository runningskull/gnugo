/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * This is GNU GO, a Go program. Contact gnugo@gnu.org, or see       *
 * http://www.gnu.org/software/gnugo/ for more information.          *
 *                                                                   *
 * Copyright 1999, 2000, 2001, 2002 by the Free Software Foundation. *
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

#include "liberty.h"
#include "patterns.h"

/* Maximum number of dragons considered by a, B, C, and d class patterns. */
#define MAX_DRAGONS_PER_PATTERN 5

/* Values of joseki patterns. */
#define U_VALUE 40
#define J_VALUE 27
#define j_VALUE 20
#define t_VALUE 15

/* Values of joseki patterns if using experimental influence. */
#define EXP_J_VALUE 35
#define EXP_j_VALUE 22
#define EXP_t_VALUE 17

/* Global variables set to joseki values acc. to experimental_influence. */
float J_value = J_VALUE;
float j_value = j_VALUE;
float t_value = t_VALUE;


/* 
 * This callback is invoked for each matched pattern.
 */

static void
shapes_callback(int m, int n, int color, struct pattern *pattern, int ll,
		void *data)
{
  int k, l;
  int move;
  
  /* Dragons of our color. */
  int my_dragons[MAX_DRAGONS_PER_PATTERN];
  int my_ndragons = 0;

  /* Dragons of other color. */
  int your_dragons[MAX_DRAGONS_PER_PATTERN];
  int your_ndragons = 0;

  /* Make a local copy of the classification that we may modify. */
  unsigned int class = pattern->class;
  
  /* Y patterns get rejected if F flag does not match with
   * experimental_influence mode.
   */
  if (class & CLASS_Y)
    if (!((experimental_influence && !(class & CLASS_F))
	  ||(!experimental_influence && (class & CLASS_F))))
      return;

  /* Don't accept fuseki marked patterns while scoring. */
  if (doing_scoring && (class & CLASS_F))
    return;
  
  /* Don't need auxiliary data in this callback. */
  UNUSED(data);
  
  /* Pick up the location of the move */
  move = AFFINE_TRANSFORM(pattern->movei, pattern->movej, ll, m, n);
  
  /* For some classes of patterns we need to find all dragons present
   * in the pattern.
   */
  if ((class & (CLASS_B | CLASS_C | CLASS_c | CLASS_a | CLASS_d | CLASS_O |
		CLASS_J | CLASS_j | CLASS_U | CLASS_T | CLASS_t)) != 0)
  {
    int other = OTHER_COLOR(color);

    /* Match each point. */
    for (k = 0; k < pattern->patlen; ++k) { 
      int pos; /* absolute (board) co-ord of (transformed) pattern element */
      int origin; /* dragon origin */
      
      /* all the following stuff (currently) applies only at occupied cells */
      if (pattern->patn[k].att == ATT_dot)
	continue;

      /* transform pattern real coordinate */
      pos = AFFINE_TRANSFORM(pattern->patn[k].x, pattern->patn[k].y, ll, m, n);

      /* Already, matchpat rejects O patterns containing a friendly stone with
       * DEAD or CRITICAL matcher_status. If the stone is tactically
       * CRITICAL it still could have matcher_status ALIVE since it might
       * be amalgamated into a live dragon. In this case we want to reject the
       * pattern if (move) does not rescue it. This is most easily tested
       * here within shapes_callback(), since the value of (move) is not
       * known by matchpat().
       */
      if ((class & CLASS_O)
	  && board[pos] == color
	  && worm[pos].attack_points[0] != 0
	  && !does_defend(move, pos))
	return;

      origin = dragon[pos].origin;
      if (board[pos] == color && my_ndragons < MAX_DRAGONS_PER_PATTERN) {
	for (l = 0; l < my_ndragons; l++) {
	  if (my_dragons[l] == origin)
	    break;
	}
	if (l == my_ndragons) {
	  /* We found another dragon of our color. Check that it (or
           * rather the underlying worm) cannot be tactically
           * captured before adding it to the list of my_dragons.  
	   */
	  if (worm[pos].attack_codes[0] == 0
	      || does_defend(move, pos)) {
	    /* Ok, add the dragon to the list. */
	    my_dragons[l] = origin;
	    my_ndragons++;
	  }
	}
      }

      if (board[pos] == other && your_ndragons < MAX_DRAGONS_PER_PATTERN) {
	for (l = 0; l < your_ndragons; l++) {
	  if (your_dragons[l] == origin)
	    break;
	}
	if (l == your_ndragons) {
	  /* We found another opponent dragon, add it to the list. */
	  your_dragons[l] = origin;
	  your_ndragons++;
	}
      }      
    } /* loop over elements */
  } /* if we need to loop over the elements */

  /* Nothing to connect. Remove C class bit. */
  if (my_ndragons < 2)
    class &= ~CLASS_C;

  /* Nothing to cut. Remove B class bit. */
  if (your_ndragons < 2)
    class &= ~CLASS_B;
  
  /*
   * If this pattern can't produce any effect (e.g. if it was a B or C
   * pattern with only one dragon of the appropriate color), don't
   * do any expensive checking but return immediately.
   * If it only has some move_values, these will be ignored.
   */
  if (!pattern->helper
      && !allpats
      && !(pattern->autohelper_flag & HAVE_ACTION)
      && !(class & (CLASS_MOVE_REASONS | CLASS_MOVE_VALUES)))
    return;
  
  /* For sacrifice patterns, the survival of the stone to be played is
   * not checked (but it still needs to be legal). Otherwise we
   * discard moves which can be captured.
   */
  if (!(class & CLASS_s)) {
    /* Don't allow ko unsafety. */
    if (safe_move(move, color) != WIN) {
      if (0)
	TRACE("  move at %1m wasn't safe, discarded\n", move);
      return;
    }
  }
  else {
    /* Allow illegal ko captures at this stage. */
    if (!is_ko(move, color, NULL) && !is_legal(move, color)) {
      if (0)
	TRACE("  move at %1m wasn't legal, discarded\n", move);
      return;
    }
  }
  
  /* For class n patterns, the pattern is contingent on an opponent
   * move at * not being captured.
   */
  if (class & CLASS_n) {
    /* Allow ko unsafety. */
    if (safe_move(move, OTHER_COLOR(color)) == 0) {
      if (0)
	TRACE("  opponent can't play safely at %1m, move discarded\n", move);
      return;
    }
  }
  
  /* If the pattern has a constraint, call the autohelper to see
   * if the pattern must be rejected.
   */
  if (pattern->autohelper_flag & HAVE_CONSTRAINT) {
    if (!pattern->autohelper(pattern, ll, move, color, 0))
      return;
  }

  /* Ask helper for acceptance of pattern. */
  if (pattern->helper) {
    /* ask helper function to consider the move */
    int accepted;
    DEBUG(DEBUG_HELPER, "  asking helper to consider '%s'+%d at %1m\n", 
	  pattern->name, ll, move);
    accepted = pattern->helper(pattern, ll, move, color);
    
    if (accepted) {
      DEBUG(DEBUG_HELPER, "helper likes pattern '%s' at %1m\n",
	    pattern->name, move);
    }
    else {
      DEBUG(DEBUG_HELPER, "  helper does not like pattern '%s' at %1m\n", 
	    pattern->name, move);
      return;  /* pattern matcher does not like it */
    }
  }
      
  /* If using -a, want to see all matches even if not -v */
  if (allpats || verbose) {
    TRACE("pattern '%s'+%d matched at %1m\n", pattern->name, ll, move);
  }
  
  /* does the pattern have an action? */
  if (pattern->autohelper_flag & HAVE_ACTION) {
    pattern->autohelper(pattern, ll, move, color, 1);
  }

  /* Pattern class B, cut all combinations of opponent dragons. */
  if (class & CLASS_B) {
    for (k = 0; k < your_ndragons; k++)
      for (l = k+1; l < your_ndragons; l++) {
	add_cut_move(move, your_dragons[k], your_dragons[l]);
	TRACE("...cuts dragons %1m, %1m\n", your_dragons[k], your_dragons[l]);
      }
  }

  /* Pattern class C, connect all combinations of our dragons. */
  if (class & CLASS_C) {
    for (k = 0; k < my_ndragons; k++)
      for (l = k+1; l < my_ndragons; l++) {
	add_connection_move(move, my_dragons[k], my_dragons[l]);
	TRACE("...connects dragons %1m, %1m\n", my_dragons[k], my_dragons[l]);
      }
  }

  /* Pattern class c, add strategical defense move reason for all our
   * dragons and a small shape bonus.
   *
   * This is a preliminary effect of "weak connection" and may need to
   * be revised.
   */
  if (class & CLASS_c) {
    for (k = 0; k < my_ndragons; k++) {
      add_strategical_defense_move(move, my_dragons[k]);
      TRACE("...strategical defense (weak connection) of %1m\n",
	    my_dragons[k]);
    }
    add_shape_value(move, 1);
    TRACE("...shape value 1\n");
  }

  /* Pattern class b is obsolete in the pattern databases handled here. */
  gg_assert(!(class & CLASS_b));

  /* Pattern class e, expand to make territory. */
  if (class & CLASS_e) {
    add_expand_territory_move(move);
    TRACE("...expands territory\n");
  }

  /* Pattern class E, expand to make moyo. */
  if (class & CLASS_E) {
    add_expand_moyo_move(move);
    TRACE("...expands moyo\n");
  }

  /* Pattern class a, strategical level attack on all opponent dragons. */
  if (class & CLASS_a) {
    for (k = 0; k < your_ndragons; k++) {
      add_strategical_attack_move(move, your_dragons[k]);
      TRACE("...strategical attack on %1m\n", your_dragons[k]);
    }
  }
  
  /* Pattern class d, strategical level defense of all own dragons. */
  if (class & CLASS_d) {
    for (k = 0; k < my_ndragons; k++) {
      add_strategical_defense_move(move, my_dragons[k]);
      TRACE("...strategical defense of %1m\n", my_dragons[k]);
    }
  }

  /* Pattern class J, joseki standard move. Add expand territory and
   * moyo, and require the value at least J_value.
   */
  if (class & CLASS_J) {
    TRACE("...joseki standard move\n");
    add_expand_territory_move(move);
    TRACE("...expands territory\n");
    add_expand_moyo_move(move);
    TRACE("...expands moyo\n");
    set_minimum_move_value(move, J_value);
    TRACE("... minimum move value %f\n", J_value);
  }

  /* Pattern class j, less urgent joseki move. Add expand territory and
   * moyo, set a minimum value of j_value. If it is a fuseki pattern, set also
   * the maximum value to j_value.
   */
  if (class & CLASS_j) {
    float min_value = j_value;
    TRACE("...less urgent joseki move\n");
    add_expand_territory_move(move);
    TRACE("...expands territory\n");
    add_expand_moyo_move(move);
    TRACE("...expands moyo\n");

    /* Board size modification. */
    min_value *= board_size / 19.0;
    
    if (class & VALUE_SHAPE) {
      min_value *= (1 + 0.01 * pattern->shape);
      class &= ~VALUE_SHAPE;
    };

    if ((board_size >= 17) && (class & CLASS_F)) {
      min_value *= 1.005; /* Otherwise, j patterns not of CLASS_F would */
                          /* get preferred in value_move_reasons */
      set_maximum_move_value(move, min_value);
      TRACE("...move value %f (shape %f)\n", min_value, pattern->shape);
    }
    else 
      TRACE("...minimum move value %f (shape %f)\n",
            min_value, pattern->shape);
    set_minimum_move_value(move, min_value);
  }

  /* Pattern class t, minor joseki move (tenuki OK).
   * Set the (min-)value at t_value
   */
  if (class & CLASS_t) {
    float min_value = t_value;
    TRACE("...minor joseki move\n");
    
    /* Board size modification. */
    min_value *= board_size / 19.0;
    
    if (class & VALUE_SHAPE) {
      min_value *= (1 + 0.01 * pattern->shape);
      class &= ~VALUE_SHAPE;
    };
    
    if ((board_size >= 17) && (class & CLASS_F)) {
      min_value *= 1.005; /* Otherwise, j patterns not of CLASS_F would */
                          /* get preferred in value_move_reasons */
      set_maximum_move_value(move, min_value);
      TRACE("...move value %f (shape %f)\n", min_value, pattern->shape);
    }
    else 
      TRACE("...minimum move value %f (shape %f)\n",
            min_value, pattern->shape);
    set_minimum_move_value(move, min_value);
  }

  /* Pattern class U, very urgent move joseki. Add strategical defense
   * and attack, plus a shape bonus of 15 and a minimum value of 40.
   */
  if (class & CLASS_U) {
    TRACE("...joseki urgent move\n");
    for (k = 0; k < my_ndragons; k++) {
      add_strategical_defense_move(move, my_dragons[k]);
      TRACE("...strategical defense of %1m\n", my_dragons[k]);
    }
    urgent = 1;
    for (k = 0; k < your_ndragons; k++) {
      add_strategical_attack_move(move, your_dragons[k]);
      TRACE("...strategical attack on %1m\n", your_dragons[k]);
    }
    add_shape_value(move, 15);
    TRACE("...shape value 15\n");
    set_minimum_move_value(move, U_VALUE);
    TRACE("...(min) move value %f\n", U_VALUE);
  }

  /* Pattern class T, joseki trick move. For the moment we never play
   * these.
   */
  if (class & CLASS_T) {
    TRACE("...joseki trick move\n");
    add_antisuji_move(move);
    TRACE("...antisuji\n");
  }

  /* Pattern class W, worthwhile threat move. */
  if (class & CLASS_W) {
    TRACE("...worthwhile threat move\n");
    add_worthwhile_threat_move(move);
  }

  /* Minimum move value specified. */
  if (class & VALUE_MINVAL) {
    set_minimum_move_value(move, pattern->value);
    TRACE("...(min) move value %f\n", pattern->value);
  }

  /* Maximum move value specified. */
  if (class & VALUE_MAXVAL) {
    set_maximum_move_value(move, pattern->maxvalue);
    TRACE("...max move value %f\n", pattern->maxvalue);
  }

  /* Minimum territorial value specified. */
  if (class & VALUE_MINTERRITORY) {
    set_minimum_territorial_value(move, pattern->minterritory);
    TRACE("...(min) territorial value %f\n", pattern->minterritory);
  }

  /* Maximum territorial value specified. */
  if (class & VALUE_MAXTERRITORY) {
    set_maximum_territorial_value(move, pattern->maxterritory);
    TRACE("...max territorial value %f\n", pattern->maxterritory);
  }

  /* Shape value specified. */
  if (class & VALUE_SHAPE) {
    add_shape_value(move, pattern->shape);
    TRACE("...shape value %f\n", pattern->shape);
  }

  /* Followup value specified. */
  if (class & VALUE_FOLLOWUP) {
    add_followup_value(move, pattern->followup);
    TRACE("...followup value %f\n", pattern->followup);
  }

  /* Reverse followup value specified. */
  if (class & VALUE_REV_FOLLOWUP) {
    add_reverse_followup_value(move, pattern->reverse_followup);
    TRACE("...reverse followup value %f\n", pattern->reverse_followup);
  }
}


/*
 * Match all patterns in patterns.db and patterns2.db on all positions.  
 *
 * This function is one of the basic generators of move reasons, called
 * by genmove().
 */
void
shapes(int color)
{
  TRACE("\nPattern matcher is looking for move reasons for %s!\n",
	color_to_string(color));

  /* Modify joseki values if using experimental influence. */
  if (experimental_influence) {
    J_value = EXP_J_VALUE;
    j_value = EXP_j_VALUE;
    t_value = EXP_t_VALUE;
  }

  matchpat(shapes_callback, color, &pat_db, NULL, NULL);

  if (josekidb)
    matchpat(shapes_callback, color, &joseki_db, NULL, NULL);

  if (!disable_fuseki && !doing_scoring)
    matchpat(shapes_callback, color, &fusekipat_db, NULL, NULL);
}


/*
 * Match all patterns in endgame.db on all positions.
 */
void
endgame_shapes(int color)
{
  TRACE("\nEndgame pattern matcher is looking for move reasons for %s!\n",
	color_to_string(color));

  matchpat(shapes_callback, color, &endpat_db, NULL, NULL);
}


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
