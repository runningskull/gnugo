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


/* values for move_reason.type */
#define THREAT_BIT    1

#define ATTACK_MOVE              2
#define ATTACK_MOVE_GOOD_KO      4
#define ATTACK_MOVE_BAD_KO       6
#define ATTACK_THREAT           (ATTACK_MOVE | THREAT_BIT)
#define DEFEND_MOVE              8
#define DEFEND_MOVE_GOOD_KO     10
#define DEFEND_MOVE_BAD_KO      12
#define DEFEND_THREAT           (DEFEND_MOVE | THREAT_BIT)

#define CONNECT_MOVE            14
#define CUT_MOVE                16

#define SEMEAI_MOVE             18
#define SEMEAI_THREAT           (SEMEAI_MOVE | THREAT_BIT)

#define BLOCK_TERRITORY_MOVE    20
#define EXPAND_TERRITORY_MOVE   22
#define EXPAND_MOYO_MOVE        24

#define OWL_ATTACK_MOVE         26
#define OWL_ATTACK_MOVE_GOOD_KO 28
#define OWL_ATTACK_MOVE_BAD_KO  30
#define OWL_ATTACK_THREAT       (OWL_ATTACK_MOVE | THREAT_BIT)
#define OWL_DEFEND_MOVE         32
#define OWL_DEFEND_MOVE_GOOD_KO 34
#define OWL_DEFEND_MOVE_BAD_KO  36
#define OWL_DEFEND_THREAT       (OWL_DEFEND_MOVE | THREAT_BIT)
#define OWL_PREVENT_THREAT      38
#define UNCERTAIN_OWL_ATTACK    40
#define UNCERTAIN_OWL_DEFENSE   42
#define STRATEGIC_ATTACK_MOVE   44
#define STRATEGIC_DEFEND_MOVE   46

#define MY_ATARI_ATARI_MOVE     50
#define YOUR_ATARI_ATARI_MOVE   52
#define VITAL_EYE_MOVE          54

#define ATTACK_EITHER_MOVE      60
#define DEFEND_BOTH_MOVE        62

#define ANTISUJI_MOVE           70


#define EITHER_MOVE             100

/* Bitmap values for move_reason.status */
#define ACTIVE 0
#define TERRITORY_REDUNDANT 1
#define STRATEGICALLY_REDUNDANT 2
#define REDUNDANT 3
#define SECONDARY 4

#define MAX_REASONS 40

#define HUGE_MOVE_VALUE 10.0*MAX_BOARD*MAX_BOARD

struct move_reason {
  int type;   /* e.g. attack, defend, or connect */
  int what;   /* pointer into list of strings, list of pair of dragons,
		 or similar */
  int status; /* This is a bitmap to mark redundant or secondary
                 move reasons. */
};

struct move_data {
  float value;    /* total comparison value, computed at the very end */
  float final_value; /* value after point redistribution. */
  float additional_ko_value; /* Additional threat value if ko fight going on.*/

  float territorial_value; /* Value in terms of actual profit. */
  float strategical_value; /* Value with respect to strength, weakness, and
			      safety of all groups on the board. */

  float maxpos_shape;      /* Maximal positive contribution to shape */
  float maxneg_shape;      /* Maximal negative contribution to shape */
  int numpos_shape;        /* Number of positive contributions to shape */
  int numneg_shape;        /* Number of negative contributions to shape */

  float followup_value;    /* Value of followup move (our sente). */
  float reverse_followup_value;	/* Value of opponents followup move
				   (reverse sente). */
  float secondary_value;   /* Secondary move value. */

  float min_value;         /* Minimum allowed value for the move. */
  float max_value;         /* Maximum allowed value for the move. */
  float min_territory;     /* Minimum territorial value. */
  float max_territory;     /* Maximum territorial value. */

  int reason[MAX_REASONS]; /* List of reasons for a move. */
  int move_safety;         /* Whether the move seems safe. */
  int worthwhile_threat;   /* Play this move as a pure threat. */
  float random_number;     /* Random number connected to this move. */
};



/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */

