/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * This is GNU Go, a Go program. Contact gnugo@gnu.org, or see       *
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

#ifndef _CLOCK_H_
#define _CLOCK_H_

/* ============================================================= *\
 *                        Time handling                          *
 *                          for GNU Go                           *
 *                         __       __                           *
 *                        <  >     <  >                          *
 *                      +--++-------++--+                        *
 *                      |  .'11 12 1'.  |                        *
 *                      |  :10 \    2:  |                        *
 *                      |  :9   @-> 3:  |                        *
 *                      |  :8       4;  |                        *
 *                      |  '..7 6 5..'  |                        *
 *                      |_______________|                        *
 *                                                               *
\* ============================================================= */

#include <stdio.h>
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include "gnugo.h"

#ifdef HAVE_VISUAL_C
#include <winsock.h>
#include <io.h>
#include <time.h>
#else
#include <sys/time.h>
#endif

#ifdef __MINGW32__
#include <windows.h>
#include <winsock.h>
#include <io.h>
#endif

/* interface */
 
/* initialization and activation */
void clock_init(int time, int byo_time, int byo_stones);
void clock_enable(void);
 
void clock_enable_autolevel(void);

/* main access */
void clock_push_button(int color);
void clock_unpush_button(int color);

/* getting informations about clock */
int clock_is_byoyomi(int color);
int clock_is_time_over(int color);
double clock_get_timer(int color);
double clock_get_btimer(int color);
double clock_get_time_left(int color);
double clock_get_btime_left(int color, int *stones);

/* adaptative system */
void clock_adapt_level(int *p_level, int color);

/* output */
void clock_print(int color);
void clock_report_autolevel(FILE *f, int color);


#endif  /* _CLOCK_H_ */


