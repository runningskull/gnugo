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
#include <assert.h>
#include <string.h>

#include "liberty.h"

/* default attenuation coefficient. */
#define DEFAULT_ATTENUATION 3.0
#define EXP_DEFAULT_ATTENUATION 2.4

/* Extra damping coefficient for spreading influence diagonally. */
#define DIAGONAL_DAMPING 2.0
#define EXP_DIAGONAL_DAMPING 1.7

/* Default strength of the influence from a stone. May be lowered if
 * it is unsafe.
 */
#define DEFAULT_STRENGTH 100.0

/* Smallest amount of influence that we care about distributing. */
/*#define INFLUENCE_CUTOFF 0.02*/
#define INFLUENCE_CUTOFF 0.02

/* Value in delta_territory_cache indicating that the value has not
 * been computed. Arbitrary but unattainable.
 */
#define NOT_COMPUTED (-2.0 * MAX_BOARD * MAX_BOARD)

/* Values for the float working area w when used only for marking. */
#define UNMARKED 0.0
#define MARKED 1.0

/* Territory, moyo, and area are segmented into connected components
 * and given a number from the same series. These values are used in
 * region_type[][].
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

#define MAX_INTRUSIONS (MAX_BOARD * MAX_BOARD)

struct intrusion_data
{
  int source_pos; 	/* Stone from which intrusion originates.*/
  int strength_pos;     /* Position of the intrusion influence soure. */
  float strength;
  float attenuation;
};

struct influence_data
{
  float white_influence[MAX_BOARD][MAX_BOARD]; /* Accumulated influence. */
  float black_influence[MAX_BOARD][MAX_BOARD]; /* Accumulated influence. */
  int p[MAX_BOARD][MAX_BOARD];           /* Working copy of board array. */
  float white_strength[MAX_BOARD][MAX_BOARD];  /* Strength of influence source. */
  float black_strength[MAX_BOARD][MAX_BOARD];  /* Strength of influence source. */
  float white_attenuation[MAX_BOARD][MAX_BOARD]; /* Attenuation. */
  float black_attenuation[MAX_BOARD][MAX_BOARD]; /* Attenuation. */
  float white_permeability[MAX_BOARD][MAX_BOARD];
  float black_permeability[MAX_BOARD][MAX_BOARD];

  int territory_segmentation[MAX_BOARD][MAX_BOARD];
  int moyo_segmentation[MAX_BOARD][MAX_BOARD];
  int area_segmentation[MAX_BOARD][MAX_BOARD];
  int region_type[MAX_REGIONS];
  int region_size[MAX_REGIONS];
  int number_of_regions;

  float territory_value[MAX_BOARD][MAX_BOARD];
  int non_territory[MAX_BOARD][MAX_BOARD];

  int color_to_move; /* Which color is in turn to move. */
  
  float w[MAX_BOARD][MAX_BOARD];         /* Working area. */
  int queuei[MAX_BOARD * MAX_BOARD];     /* Points receiving influence. */
  int queuej[MAX_BOARD * MAX_BOARD];

  int intrusion_counter;
  struct intrusion_data intrusions[MAX_INTRUSIONS];
};

/* Typedef for pointer to either of the functions whose_territory(),
 * whose_moyo(), and whose_area().
 */
typedef int (*owner_function_ptr)(struct influence_data *q, int m, int n);

/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
