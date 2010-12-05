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
#include <stdarg.h>
#include <assert.h>

#include "gg_utils.h"
#include "random.h"

#ifdef HAVE_GLIB_H
#include <glib.h>
#endif

/* Avoid compiler warnings with unused parameters */
#define UNUSED(x)  (void)x

/* Define TERMINFO or ANSI_COLOR to enable coloring of pieces.
 * This is normally done in config.h.
 */

/* enabling color */

/* linux console :
 *  0=black
 *  1=red
 *  2=green
 *  3=yellow/brown
 *  4=blue
 *  5=magenta
 *  6=cyan
 *  7=white
 */

#ifdef TERMINFO

#ifdef _AIX
#define _TPARM_COMPAT
#endif

#if HAVE_CURSES_H
#include <curses.h>
#elif HAVE_NCURSES_CURSES_H
#include <ncurses/curses.h>
#else
#endif

#if HAVE_TERM_H
#include <term.h>
#elif HAVE_NCURSES_TERM_H
#include <ncurses/term.h>
#else
#endif


/* terminfo attributes */
static char *setaf;		/* terminfo string to set color */
static char *op;		/* terminfo string to reset colors */

static int init = 0;

#endif /* TERMINFO */

/* for gg_cputime */

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_TIMES_H
#include <sys/times.h>
#elif defined(WIN32)
#include <windows.h>
#endif

void
gg_init_color()
{
#ifdef TERMINFO

/* compiler is set to make string literals  const char *
 * But system header files dont prototype things correctly.
 * These are equivalent to a non-const string literals
 */

  static char setaf_literal[] = "setaf";
  static char op_literal[] = "op";
  static char empty_literal[] = "";

  if (init)
    return;
  
  init = 1;

  setupterm(NULL, 2, NULL);
  setaf = tigetstr(setaf_literal);
  if (!setaf)
    setaf = empty_literal;
  op = tigetstr(op_literal);
  if (!op)
    op = empty_literal;
 
#endif /* TERMINFO */
}



#ifdef WIN32
#ifdef VC
#include <crtdbg.h>

verifyW32(BOOL b)
{
  if (!b) {
    _ASSERTE(0 && "Win32 Error");
    fprintf(stderr, "Win32 Err: %ld\n", GetLastError());
  }
}

#else
/* mingw32 lacks crtdbg.h and _ASSERTE */
verifyW32(BOOL b)
{
  if (!b) {
    fprintf(stderr, "Win32 Err: %ld\n", GetLastError());
  }
}

#endif

#endif

void 
write_color_char_no_space(int c, int x)
{
#ifdef TERMINFO

  fprintf(stderr, "%s%c", tparm(setaf, c, 0, 0, 0, 0, 0, 0, 0, 0), x);
  fputs(tparm(op, 0, 0, 0, 0, 0, 0, 0, 0, 0), stderr);

#elif defined(ANSI_COLOR)

  fprintf(stderr, "\033[%dm%c\033[0m", 30+c, x);

#elif defined(WIN32)
  
  static HANDLE hStdErr = 0;
  DWORD iCharsWritten;
  BOOL succeed32;
  CONSOLE_SCREEN_BUFFER_INFO bufInfo;
  if (!hStdErr) {
    hStdErr = GetStdHandle(STD_ERROR_HANDLE);
    if (hStdErr == INVALID_HANDLE_VALUE) {
      fprintf(stderr, "Unable to open stderr.\n");
    }
  }

  /* Red & Blue are switched from what MS-Windows wants:
   *   FOREGROUND_BLUE      0x0001 // text color contains blue.
   *   FOREGROUND_GREEN     0x0002 // text color contains green.
   *   FOREGROUND_RED       0x0004 // text color contains red
   * This magic switches the bits back:
   */
  c = (c & 1) * 4 + (c & 2) + (c & 4) / 4;
  c += FOREGROUND_INTENSITY;
  succeed32 = GetConsoleScreenBufferInfo(hStdErr, &bufInfo);
  if (!succeed32) {  /* Probably redirecting output, just give plain text. */
    fprintf(stderr, "%c", x);
    return;
  }
  verifyW32(SetConsoleTextAttribute(hStdErr, (WORD) c));
  verifyW32(WriteConsole(hStdErr, &x, 1, &iCharsWritten, 0));
  verifyW32(SetConsoleTextAttribute(hStdErr, bufInfo.wAttributes));

#else

  fprintf(stderr, "%c", x);

#endif
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
    
#ifdef HAVE_VSNPRINTF
  vsnprintf(dest, len, fmt, args);
#elif HAVE_G_VSNPRINTF
  g_vsnprintf(dest, len, fmt, args);
#elif HAVE__VSNPRINTF
  _vsnprintf(dest, len, fmt, args);
#else
  UNUSED(len);
  vsprintf(dest, fmt, args);
#endif

}

void
gg_snprintf(char *dest, unsigned long len, const char *fmt, ...)
{
  va_list args;
  va_start(args, fmt);
  gg_vsnprintf(dest, len, fmt, args);
  va_end(args);
}

/* Get the time of day, calling gettimeofday from sys/time.h
 * if available, otherwise substituting a workaround for portability.
 */

double
gg_gettimeofday(void)
{
  struct timeval tv;
#ifdef HAVE_GETTIMEOFDAY
  gettimeofday(&tv, NULL);
#else
  tv.tv_sec  = time(NULL);
  tv.tv_usec = 0;
#endif
  return tv.tv_sec + 1.e-6 * tv.tv_usec;
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
#if HAVE_SYS_TIMES_H && HAVE_TIMES && HAVE_UNISTD_H
    struct tms t;
    times(&t);
    return (t.tms_utime + t.tms_stime + t.tms_cutime + t.tms_cstime)
            / ((double) sysconf(_SC_CLK_TCK));
#elif defined(WIN32)
    FILETIME creationTime, exitTime, kernelTime, userTime;
    ULARGE_INTEGER uKernelTime, uUserTime, uElapsedTime;
    GetProcessTimes(GetCurrentProcess(), &creationTime, &exitTime,
                    &kernelTime, &userTime);
    uKernelTime.LowPart = kernelTime.dwLowDateTime;
    uKernelTime.HighPart = kernelTime.dwHighDateTime;
    uUserTime.LowPart = userTime.dwLowDateTime;
    uUserTime.HighPart = userTime.dwHighDateTime;
    uElapsedTime.QuadPart = uKernelTime.QuadPart + uUserTime.QuadPart;
    /*_ASSERTE(0 && "Debug Times");*/
    /* convert from multiples of 100nanosecs to seconds: */
    return (signed __int64)(uElapsedTime.QuadPart) * 1.e-7;
#else
    static int warned = 0;
    if (!warned) {
      fprintf(stderr, "CPU timing unavailable - returning wall time.");
      warned = 1;
    }
    /* return wall clock seconds */
    return gg_gettimeofday();
#endif
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
              / (f->range_upperbound - f->range_lowerbound);
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
  return ((a * b) / (a + b));
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


/* Intermediate layer to random.c. gg_srand() should only be called via the
 * functions below.
 */
 
/* Private variable remembering the random seed. */
static unsigned int random_seed;

unsigned int
get_random_seed()
{
  return random_seed;
}

void
set_random_seed(unsigned int seed)
{
  random_seed = seed;
  gg_srand(seed);
}

/* Update the random seed. This should be called at the start of each
 * new game.
 * We reset the random seed before obtaining a new one, to make the
 * next random seed depend deterministically on the old one.
 */
void
update_random_seed(void)
{
  gg_srand(random_seed);
  random_seed = gg_rand();
  /* Since random seed 0 has a special interpretation when given as
   * command line argument with the -r option, we make sure to avoid
   * it.
   */
  if (random_seed == 0)
    random_seed = 1;
  gg_srand(random_seed);
}


/* Restart the pseudo-random sequence with the initialization given
 * by the random seed. Should be called at each move.
 */
void
reuse_random_seed()
{
  gg_srand(random_seed);
}



/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
