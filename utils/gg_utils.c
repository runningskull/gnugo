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
#include <stdarg.h>
#include <assert.h>

#include "gg_utils.h"

#ifdef HAVE_GLIB_H
#include <glib.h>
#endif

#define UNUSED(x)  x=x

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
static int   max_color;		/* terminfo max colour */

static int init = 0;

#endif /* TERMINFO */

/* for gg_cputime */

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
  static char colors_literal[] = "colors";
  static char empty_literal[] = "";

  if (init)
    return;
  
  init = 1;

  setupterm(NULL, 2, NULL);
  setaf = tigetstr(setaf_literal);
  if (!setaf)
    setaf = empty_literal;
  max_color = tigetnum(colors_literal) - 1;
  if (max_color < 1)
    max_color = 1;
  else if (max_color > 30)
    max_color = 30;
  
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
  fputs(tparm(setaf, max_color, 0, 0, 0, 0, 0, 0, 0, 0), stderr);

#elif defined(ANSI_COLOR)

  fprintf(stderr, "\033[%dm%c\033[0m", 30+c, x);

#elif defined(WIN32)
  
  static HANDLE hStdErr=0;
  DWORD iCharsWritten;
  BOOL succeed32;
  CONSOLE_SCREEN_BUFFER_INFO bufInfo;
  if (!hStdErr) {
    hStdErr = GetStdHandle(STD_ERROR_HANDLE);
    if (hStdErr == INVALID_HANDLE_VALUE) {
      fprintf(stderr, "Unable to open stderr.\n");
    }
  }

  /*Red & Blue are switched from what MS-Windows wants:
   *   FOREGROUND_BLUE      0x0001 // text color contains blue.
   *   FOREGROUND_GREEN     0x0002 // text color contains green.
   *   FOREGROUND_RED       0x0004 // text color contains red
   *This magic switches the bits back: */
  c = (c & 1) * 4 + (c & 2) + (c & 4) / 4;
  c += FOREGROUND_INTENSITY;
  succeed32 = GetConsoleScreenBufferInfo(hStdErr, &bufInfo);
  if (!succeed32) {  //Probably redirecting output, just give plain text.
    fprintf(stderr, "%c", x);
    return;
  }
  verifyW32(SetConsoleTextAttribute(hStdErr, (WORD)c) );
  verifyW32(WriteConsole(hStdErr, &x, 1, &iCharsWritten, 0));
  verifyW32(SetConsoleTextAttribute(hStdErr, bufInfo.wAttributes));

#else

  fprintf(stderr, "%c", x);

#endif
}

void
write_color_string(int c, const char *str)
{
  while (*str) {
    write_color_char_no_space(c, *str++);
  }
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
#else
  UNUSED(len);
  vsprintf(dest, fmt, args);
#endif

}

void
gg_snprintf(char *dest, unsigned long len, const char *fmt, ...) {
  va_list args;
  va_start(args, fmt);
  gg_vsnprintf(dest, len, fmt, args);
  va_end(args);
}

/* Get the time of day, calling gettimeofday from sys/time.h
 * if available, otherwise substituting a workaround for portability.
 */

int
gg_gettimeofday2(struct timeval *tv, void *p)
{
#ifdef HAVE_GETTIMEOFDAY
  return gettimeofday(tv, NULL);
#else
  if (tv != NULL) {
    tv->tv_sec  = time(NULL);
    tv->tv_usec = 0;
  }
  return 1;
#endif
}

double
gg_gettimeofday(void)
{
  struct timeval tv;
  gg_gettimeofday2(&tv, NULL);
  return tv.tv_sec + 1.e-6*tv.tv_usec;
}

const char *
gg_version(void) {
  return VERSION;
}

/* return cputime used in secs */

double gg_cputime(void)
{
#if HAVE_SYS_TIMES_H && HAVE_TIMES
    struct tms t;
    return times(&t) / ((double) CLOCKS_PER_SEC);
#elif defined(WIN32)
    FILETIME creationTime, exitTime, kernelTime, userTime;
    ULARGE_INTEGER uKernelTime,uUserTime,uElapsedTime;
    unsigned long ulElapsedTime;
    GetProcessTimes(GetCurrentProcess(), &creationTime, &exitTime,
                    &kernelTime, &userTime);
    uKernelTime.LowPart = kernelTime.dwLowDateTime;
    uKernelTime.HighPart = kernelTime.dwHighDateTime;
    uUserTime.LowPart = userTime.dwLowDateTime;
    uUserTime.HighPart = userTime.dwHighDateTime;
    uElapsedTime.QuadPart = uKernelTime.QuadPart + uUserTime.QuadPart;
    /*_ASSERTE(0 && "Debug Times");*/
    /* convert from multiples of 100nanosecs to seconds: */
    return uElapsedTime.QuadPart * 1.e-7;
#else
    /* return wall clock seconds */
    return gg_gettimeofday();
#endif
}

/* Reorientation of point (i,j) into (*ri, *rj) */
void rotate(int i, int j, int *ri, int *rj, int bs, int rot) {
  int bs1;
  assert (bs > 0);
  assert (ri != NULL && rj != NULL);
  assert (rot >= 0 && rot < 8);
  /* PASS case */
  if (i == -1 && j == -1) {
    *ri = i;
    *rj = j;
    return;
  }
  assert (i >= 0 && i < bs);
  assert (j >= 0 && j < bs);

  bs1 = bs - 1;
  if (rot == 0) {
    /* identity map */
    *ri = i;
    *rj = j;
  } else if (rot == 1) {
    /* rotation over 90 degrees */
    *ri = bs1 - j;
    *rj = i;
  } else if (rot == 2) {
    /* rotation over 180 degrees */
    *ri = bs1 - i;
    *rj = bs1 - j;
  } else if (rot == 3) {
    /* rotation over 270 degrees */
    *ri = j;
    *rj = bs1 - i;
  } else if (rot == 4) {
    /* flip along diagonal */
    *ri = j;
    *rj = i;
  } else if (rot == 5) {
    /* flip */
    *ri = bs1 - i;
    *rj = j;
  } else if (rot == 6) {
    /* flip along diagonal */
    *ri = bs1 - j;
    *rj = bs1 - i;
  } else if (rot == 7) {
    /* flip */
    *ri = i;
    *rj = bs1 - j;
  }
}

/* inverse reorientation of reorientation rot */
void inv_rotate(int i, int j, int *ri, int *rj, int bs, int rot) {
  /* every reorientation is it's own inverse except rotations
     over 90 and 270 degrees */
  if (rot == 1) {
    rotate(i, j, ri, rj, bs, 3);
  } else if (rot == 3) {
    rotate(i, j, ri, rj, bs, 1);
  } else {
    rotate(i, j, ri, rj, bs, rot);
  }
}



/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
