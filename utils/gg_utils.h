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

#ifndef _GG_UTILS_H_
#define _GG_UTILS_H_

#include <stdarg.h>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#if TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# if HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  include <time.h>
# endif
#endif

#ifdef __MINGW32__
#include <windows.h>
#include <winsock.h>
#include <io.h>
#endif

#ifdef HAVE_VISUAL_C
#include <winsock.h>
#include <io.h>
#endif

void gg_init_color(void);
void write_color_char(int c, int x);
void write_color_string(int c, const char *str);

void gg_vsnprintf(char *dest, unsigned long len, const char *fmt,
		  va_list args);
void gg_snprintf(char *dest, unsigned long len, const char *fmt, ...);

int gg_gettimeofday2(struct timeval *tv, void *p);
double gg_gettimeofday(void);

const char *gg_version(void);

/* prototypes for basic reorientation functions */

void  rotate(int i, int j, int *ri, int *rj, int bs, int rot);
void  inv_rotate(int i, int j, int *ri, int *rj, int bs, int rot);

#endif /* _GG_UTILS_H_ */


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
