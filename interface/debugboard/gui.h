/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * This is GNU Go, a Go program. Contact gnugo@gnu.org, or see       *
 * http://www.gnu.org/software/gnugo/ for more information.          *
 *                                                                   *
 * Copyright 1999, 2000, 2001, 2002 by the Free Software Foundation. *
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

/*
 * File: display.h
 */

#include <stdlib.h>

#include "liberty.h"
#include "display.h"


/* ================================================================ */


void prepare_worms_tab(void);
void display_worm(int w);
void display_worm_tactical_data(int m, int n);

void prepare_dragons_tab(void);
void display_dragon(int pos);

void prepare_eyes_tab(void);
void display_eye(int color, struct eye_data eyedata[BOARDMAX], int pos);

void prepare_help_tab(void);



/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
