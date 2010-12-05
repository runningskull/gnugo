/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * This is GNU Go, a Go program. Contact gnugo@gnu.org, or see       *
 * http://www.gnu.org/software/gnugo/ for more information.          *
 *                                                                   *
 * Copyright 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007,   *
 * 2008, 2009 and 2010 by the Free Software Foundation.              *
 *                                                                   *
 * This program is free software; you can redistribute it and/or     *
 * modify it under the terms of the GNU General Public License as    *
 * published by the Free Software Foundation - version 3 or          *
 * (at your option) any later version.                               *
 *                                                                   *
 * This program is distributed in the hope that it will be useful,   *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of    *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     *
 * GNU General Public License in file COPYING for more details.      *
 *                                                                   *
 * You should have received a copy of the GNU General Public         *
 * License along with this program; if not, write to the Free        *
 * Software Foundation, Inc., 51 Franklin Street, Fifth Floor,       *
 * Boston, MA 02111, USA.                                            *
\* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "liberty.h"
#include "winsocket.h"


/* The cosmic style uses more influence than the defaults attenuation 
 * coefficients !
 * The "TERR_.."-values are used in the influence computations used
 * for territory evaluation. (initial_influence with dragons_known,
 * move_influence)
 */
#define DEFAULT_ATTENUATION \
       	(cosmic_importance * 2.7  + (1.0 - cosmic_importance) * 3.0)
#define TERR_DEFAULT_ATTENUATION \
	(cosmic_importance * 2.15 + (1.0 - cosmic_importance) * 2.4)

/* Extra damping coefficient for spreading influence diagonally. */
#define DIAGONAL_DAMPING \
	(cosmic_importance * 2.5 + (1.0 - cosmic_importance) * 2.0)
#define TERR_DIAGONAL_DAMPING \
	(cosmic_importance * 2.5 + (1.0 - cosmic_importance) * 1.7)






/* Smallest amount of influence that we care about distributing. */
#define INFLUENCE_CUTOFF 0.02

/* Value in delta_territory_cache indicating that the value has not
 * been computed. Arbitrary but unattainable.
 */
#define NOT_COMPUTED (-2.0 * MAX_BOARD * MAX_BOARD)

/* Maximum number of regions allowed between territory, moyo, and area.
 * FIXME: This number is vastly exaggerated. Should be possible to
 * come up with a much better upper bound.
 */ 
#define MAX_REGIONS (3*MAX_BOARD*MAX_BOARD + 1)

#define MAX_INTRUSIONS (2 * MAX_BOARD * MAX_BOARD)

struct intrusion_data
{
  int source_pos; 	/* Stone from which intrusion originates.*/
  int strength_pos;     /* Position of the intrusion influence soure. */
  float strength;
  float attenuation;
};

struct influence_data
{
  signed char safe[BOARDMAX];

  float white_influence[BOARDMAX]; 	/* Accumulated influence. */
  float black_influence[BOARDMAX]; 	/* Accumulated influence. */
  float white_strength[BOARDMAX];  	/* Strength of influence source. */
  float black_strength[BOARDMAX];  	/* Strength of influence source. */
  float white_attenuation[BOARDMAX]; 
  float black_attenuation[BOARDMAX];
  float white_permeability[BOARDMAX];
  float black_permeability[BOARDMAX];

  int is_territorial_influence; /* 0 only if computing escape_influence.*/

  float territory_value[BOARDMAX];
  int non_territory[BOARDMAX];
  int captured;

  int color_to_move; /* Which color is in turn to move. */
  
  int queue[MAX_BOARD * MAX_BOARD];     /* Points receiving influence. */

  int intrusion_counter;
  struct intrusion_data intrusions[MAX_INTRUSIONS];

  int id;
};

/* Typedef for pointer to either of the functions whose_territory(),
 * whose_loose_territory(), whose_moyo(), and whose_area().
 */
typedef int (*owner_function_ptr)(const struct influence_data *q, int pos);

/* Used for tuning game advancement algorythm */
#define WEIGHT_TERRITORY 10
#define WEIGHT_MOYO       3
#define WEIGHT_AREA       1



/* cosmic_importance is a number between 0.0 and 1.0 ;
 * when cosmic_importance is 0.0, the default influence
 * values are used; when cosmic_importance is 1.0, GNU Go
 * will try to play an influence-oriented fuseki by 
 * over-estimatingthe potential territory values of moyos.
 * In the current implementation, cosmic_importance decreases 
 * slowly for 19*19 games from 1.0 at move 4 to 0.0 at move 120.
 */
float cosmic_importance;


/* Used in the whose_moyo() function */
struct moyo_determination_data
{
  float influence_balance;
  float my_influence_minimum;
  float opp_influence_maximum;
};




/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */

