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

/*-------------------------------------------------------------------------
 * interface.h
 * 	This file contains all headers for interfaces
 *-------------------------------------------------------------------------*/

#ifndef _PLAY_INTERFACE_H
#define _PLAY_INTERFACE_H

#include "gnugo.h"
#include "sgftree.h"

void play_ascii(SGFTree *tree, Gameinfo *gameinfo, 
		char *filename, char *until);
void play_ascii_emacs(SGFTree *tree, Gameinfo *gameinfo, 
		      char *filename, char *until);
void play_gtp(FILE *gtp_input, int gtp_initial_orientation);
void play_gmp(Gameinfo *gameinfo);
void play_solo(Gameinfo *gameinfo, int benchmark);
void play_replay(Gameinfo *gameinfo, int color_to_test);

void load_and_analyze_sgf_file(Gameinfo *gameinfo);
void load_and_score_sgf_file(SGFTree *tree, Gameinfo *gameinfo,
			     const char *scoringmode);


#endif


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
