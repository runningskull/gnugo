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

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "liberty.h"

/* Default attenuation coefficient.
 * The "TERR_.."-values are used in the influence computations used
 * for territory evaluation. (initial_influence with dragons_known,
 * move_influence)
 */
#define DEFAULT_ATTENUATION 3.0
#define TERR_DEFAULT_ATTENUATION 2.4

/* Extra damping coefficient for spreading influence diagonally. */
#define DIAGONAL_DAMPING 2.0
#define TERR_DIAGONAL_DAMPING 1.7


/* Smallest amount of influence that we care about distributing. */
#define INFLUENCE_CUTOFF 0.02

/* Value in delta_territory_cache indicating that the value has not
 * been computed. Arbitrary but unattainable.
 */
#define NOT_COMPUTED (-2.0 * MAX_BOARD * MAX_BOARD)

/* Territory, moyo, and area are segmented into connected components
 * and given a number from the same series. These values are used in
 * region_type[].
 */
#define WHITE_REGION    0
#define BLACK_REGION    1
#define IS_TERRITORY    2
#define IS_MOYO         4
#define IS_AREA         8
#define WHITE_TERRI     (WHITE_REGION | IS_TERRITORY)
#define BLACK_TERRI     (BLACK_REGION | IS_TERRITORY)
#define WHITE_MOYO      (WHITE_REGION | IS_MOYO)
#define BLACK_MOYO      (BLACK_REGION | IS_MOYO)
#define WHITE_AREA      (WHITE_REGION | IS_AREA)
#define BLACK_AREA      (BLACK_REGION | IS_AREA)

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
  char safe[BOARDMAX];

  float white_influence[BOARDMAX]; 	/* Accumulated influence. */
  float black_influence[BOARDMAX]; 	/* Accumulated influence. */
  float white_strength[BOARDMAX];  	/* Strength of influence source. */
  float black_strength[BOARDMAX];  	/* Strength of influence source. */
  float white_attenuation[BOARDMAX]; 
  float black_attenuation[BOARDMAX];
  float white_permeability[BOARDMAX];
  float black_permeability[BOARDMAX];

  int territory_segmentation[BOARDMAX];
  int moyo_segmentation[BOARDMAX];
  int area_segmentation[BOARDMAX];
  int region_type[MAX_REGIONS];
  int region_size[MAX_REGIONS];
  float region_territorial_value[MAX_REGIONS];
  int number_of_regions;

  int is_territorial_influence; /* 0 only if computing escape_influence.*/

  float territory_value[BOARDMAX];
  int non_territory[BOARDMAX];
  int captured;

  int color_to_move; /* Which color is in turn to move. */
  
  int queue[MAX_BOARD * MAX_BOARD];     /* Points receiving influence. */

  int intrusion_counter;
  struct intrusion_data intrusions[MAX_INTRUSIONS];
};

/* Typedef for pointer to either of the functions whose_territory(),
 * whose_moyo(), and whose_area().
 */
typedef int (*owner_function_ptr)(const struct influence_data *q, int pos);

/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
