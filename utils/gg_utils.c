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
#include <stdarg.h>
#include <assert.h>

#include "gg_utils.h"

/* Avoid compiler warnings with unused parameters */
#define UNUSED(x)  (void)x

void
gg_init_color()
{
  /* Left dummy function for SPEC. /ab. */
}



void 
write_color_char_no_space(int c, int x)
{
  fprintf(stderr, "%c", x);
}

void
write_color_string(int c, const char *str)
{
  while (*str)
    write_color_char_no_space(c, *str++);
}

void
write_color_char(int c, int x)
{
  fprintf(stderr, " ");
  write_color_char_no_space(c, x);
}

/*
 * A wrapper around vsnprintf.
 */

void
gg_vsnprintf(char *dest, unsigned long len, const char *fmt, va_list args)
{
  /* Removed calls to vsnprintf() for SPEC as it is not guaranteed to exist
   * in ANSI C. /ab
   */
  UNUSED(len);
  vsprintf(dest, fmt, args);
}

void
gg_snprintf(char *dest, unsigned long len, const char *fmt, ...)
{
  va_list args;
  va_start(args, fmt);
  gg_vsnprintf(dest, len, fmt, args);
  va_end(args);
}

double
gg_gettimeofday(void)
{
  return 0.0;
}

const char *
gg_version(void)
{
  return VERSION;
}

/* return cputime used in secs */

double
gg_cputime(void)
{
  return 0.0;
}

/* Before we sort floating point values (or just compare them) we
 * may need to normalize them. This may sound cryptic but is
 * required to avoid an obscure platform dependency.
 *
 * The underlying problem is that most fractional decimal numbers
 * can't be represented exactly in a floating point number with base
 * two. The error may be small but it is there. When such numbers
 * are added or subtracted, the errors accumulate and even if the
 * result (counting exactly) should be a number which can be
 * represented exactly, this cannot be assumed to be the case.
 *
 * To give an example of this, the computation 0.3 + 0.05 - 0.35 may
 * sum to 0, a small negative value, or a small positive value.
 * Moreover, which case we encounter depends on the number of
 * mantissa bits in the floating point type used and the exact
 * details of the floating point arithmetic on the platform.
 *
 * In the context of sorting, assume that two values both should be
 * 0.35, but one has been computed as 0.3 + 0.05 and the other
 * directly assigned 0.35. Then it depends on the platform whether
 * they compare as equal or one of them is larger than the other.
 *
 * This code normalizes the values to avoid this problem. It is
 * assumed that all values encountered are integer multiples of a.
 */
float
gg_normalize_float(float x, float a)
{
  return a * ((int) (0.5 + x / a));
}

int
gg_normalize_float2int(float x, float a)
{
  return ((int) (0.5 + x / a));
}

/* A sorting algorithm, call-compatible with the libc qsort() function.
 *
 * The reason to prefer this to standard qsort() is that quicksort is
 * an unstable sorting algorithm, i.e. the relative ordering of
 * elements with the same comparison value may change. Exactly how the
 * ordering changes depends on implementation specific details like
 * the strategy for choosing the pivot element. Thus a list with
 * "equal" values may be sorted differently between platforms, which
 * potentially can lead to significant differences in the move
 * generation.
 *
 * This is an implementation of the combsort algorithm.
 *
 * Testing shows that it is faster than the GNU libc qsort() function
 * on small data sets and within a factor of two slower for large
 * random data sets. Its performance does not degenerate for common
 * special cases (i.e. sorted or reversed data) but it seems to be
 * susceptible to O(N^2) behavior for repetitive data with specific
 * cycle lengths.
 *
 * Like qsort() this algorithm is unstable, but since the same
 * implementation (this one) is used on all platforms, the reordering
 * of equal elements will be consistent.
 */
void
gg_sort(void *base, size_t nel, size_t width,
	int (*cmp)(const void *, const void *))
{
  int gap = nel;
  int swap_made;
  char *end = (char *) base + width * (nel - 1);
  do {
    char *a, *b;
    swap_made = 0;
    gap = (10 * gap + 3) / 13;
    for (a = base, b = a + gap * width; b <= end; a += width, b += width) {
      if (cmp((void *) a, (void *) b) > 0) {
	char *c = a;
	char *d = b;
	size_t size = width;
	while (size-- > 0) {
	  char tmp = *c;
	  *c++ = *d;
	  *d++ = tmp;
	}
	swap_made = 1;
      }
    }
  } while (gap > 1 || swap_made);
}


/* Linearly interpolate f(x) from the data given in interpolation_data. */
float
gg_interpolate(struct interpolation_data *f, float x)
{
  int i;
  float ratio;
  float diff;
  if (x < f->range_lowerbound)
    return f->values[0];
  else if (x > f->range_upperbound)
    return f->values[f->sections];
  else {
    ratio = ((float) f->sections) * (x - f->range_lowerbound)
              /(f->range_upperbound - f->range_lowerbound);
    i = (int) ratio;
    diff = ratio - ((float) i);
    if (0)
      fprintf(stderr, "Floating point Ratio: %f, integer: %d, diff %f",
	      ratio, i, diff);
    return ((1 - diff) * f->values[i] + diff * f->values[i+1]);
  }
}


/* This is the simplest function that returns appr. a when a is small,
 * and approximately b when a is large.
 */
float
soft_cap(float a, float b)
{
  return ((a * b) / ( a + b));
}


/* Reorientation of point (i, j) into (*ri, *rj) */
void
rotate(int i, int j, int *ri, int *rj, int bs, int rot)
{
  int bs1;
  assert(bs > 0);
  assert(ri != NULL && rj != NULL);
  assert(rot >= 0 && rot < 8);
  /* PASS case */
  if (i == -1 && j == -1) {
    *ri = i;
    *rj = j;
    return;
  }

  assert(i >= 0 && i < bs);
  assert(j >= 0 && j < bs);

  bs1 = bs - 1;
  if (rot == 0) {
    /* identity map */
    *ri = i;
    *rj = j;
  }
  else if (rot == 1) {
    /* rotation over 90 degrees */
    *ri = bs1 - j;
    *rj = i;
  }
  else if (rot == 2) {
    /* rotation over 180 degrees */
    *ri = bs1 - i;
    *rj = bs1 - j;
  }
  else if (rot == 3) {
    /* rotation over 270 degrees */
    *ri = j;
    *rj = bs1 - i;
  }
  else if (rot == 4) {
    /* flip along diagonal */
    *ri = j;
    *rj = i;
  }
  else if (rot == 5) {
    /* flip */
    *ri = bs1 - i;
    *rj = j;
  }
  else if (rot == 6) {
    /* flip along diagonal */
    *ri = bs1 - j;
    *rj = bs1 - i;
  }
  else if (rot == 7) {
    /* flip */
    *ri = i;
    *rj = bs1 - j;
  }
}

/* inverse reorientation of reorientation rot */
void
inv_rotate(int i, int j, int *ri, int *rj, int bs, int rot)
{
  /* every reorientation is it's own inverse except rotations
     over 90 and 270 degrees */
  if (rot == 1)
    rotate(i, j, ri, rj, bs, 3);
  else if (rot == 3)
    rotate(i, j, ri, rj, bs, 1);
  else
    rotate(i, j, ri, rj, bs, rot);
}



/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
