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
#include <curses.h>
#include <term.h>

/* terminfo attributes */
static char *setaf;		/* terminfo string to set color */
static int   max_color;		/* terminfo max colour */

static int init = 0;

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

void
write_color_char(int c, int x)
{
#ifdef TERMINFO

  fprintf(stderr, " %s%c", tparm(setaf, c, 0, 0, 0, 0, 0, 0, 0, 0), x);
  fputs(tparm(setaf, max_color, 0, 0, 0, 0, 0, 0, 0, 0), stderr);

#elif defined(ANSI_COLOR)

  fprintf(stderr, " \033[%dm%c\033[0m", 30+c, x);

#else

  fprintf(stderr, " %c", x);

#endif
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


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
