/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * This is GNU Go, a Go program. Contact gnugo@gnu.org, or see       *
 * http://www.gnu.org/software/gnugo/ for more information.          *
 *                                                                   *
 * Copyright 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007,   *
 * 2008 and 2009 by the Free Software Foundation.                    *
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


/* This file contains the global functions of the board library libboard.a. */

#include "board.h"
#include "hash.h"

/* The board state itself. */
int          board_size = DEFAULT_BOARD_SIZE; /* board size */
Intersection board[BOARDSIZE];
int          board_ko_pos;
int          white_captured;    /* number of black and white stones captured */
int          black_captured;

Intersection initial_board[BOARDSIZE];
int          initial_board_ko_pos;
int          initial_white_captured;
int          initial_black_captured;
int          move_history_color[MAX_MOVE_HISTORY];
int          move_history_pos[MAX_MOVE_HISTORY];
Hash_data    move_history_hash[MAX_MOVE_HISTORY];
int          move_history_pointer;

float komi = 0.0;
int handicap = 0;
int movenum;
enum suicide_rules suicide_rule = FORBIDDEN;
enum ko_rules ko_rule = SIMPLE;


signed char shadow[BOARDMAX];

/* Hashing of positions. */
Hash_data board_hash;

int stackp;             /* stack pointer */
int position_number;    /* position number */

/* Some statistics gathered partly in board.c and hash.c */
struct stats_data stats;

/* Variation tracking in SGF trees: */
int count_variations  = 0;
SGFTree *sgf_dumptree = NULL;
