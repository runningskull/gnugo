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

#ifndef _GG_UTILS_H_
#define _GG_UTILS_H_

#include <stdarg.h>
#include <stddef.h>

#include <config.h>

void gg_init_color(void);
void write_color_char(int c, int x);
void write_color_string(int c, const char *str);

void gg_vsnprintf(char *dest, unsigned long len, const char *fmt,
		  va_list args);
void gg_snprintf(char *dest, unsigned long len, const char *fmt, ...);

double gg_gettimeofday(void);
double gg_cputime(void);

float gg_normalize_float(float x, float a);
int gg_normalize_float2int(float x, float a);
void gg_sort(void *base, size_t nel, size_t width,
	     int (*compar)(const void *, const void *));

#define MAX_INTERPOLATION_STEPS 20
struct interpolation_data
{
  int sections;
  float range_lowerbound;
  float range_upperbound;
  float values[MAX_INTERPOLATION_STEPS + 1];
};

float gg_interpolate(struct interpolation_data *f, float x);
float soft_cap(float a, float b);

const char *gg_version(void);

/* prototypes for basic reorientation functions */

void rotate(int i, int j, int *ri, int *rj, int bs, int rot);
void inv_rotate(int i, int j, int *ri, int *rj, int bs, int rot);

#endif /* _GG_UTILS_H_ */


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
