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

/*-------------------------------------------------------------
  showbord.c -- Show current go board and playing information
-------------------------------------------------------------*/

/* 
 * NOTE : this is no longer intended as the main user interface
 * as it was in GNU Go 1.2. It is now a debugging aid, showing
 * the internal state of dragons, and things. But with
 * color enabled, it should be easy enough to see the state
 * of play at a glance.
 *
 * Note : the dragons must have been calculated before this is called
 */

#include "gnugo.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "liberty.h"
#include "gg_utils.h"


/*
 * Stuff to enumerate the dragons 
 */

/* Element at origin of each worm stores allocated worm number. */
static unsigned char dragon_num[BOARDMAX];

static int next_white;		/* next worm number to allocate */
static int next_black;

/* linux console :
 *  0=black
 *  1=red             [critical]
 *  2=green           [alive]
 *  3=yellow/brown    [unknown]
 *  4=blue
 *  5=magenta
 *  6=cyan            [dead]
 *  7=white           [unchecked]
 */

/* Both black and white are common background colors and should be
 * avoided.
 */
static const int colors[3][5] = {
  {0,0,0,0,0}, /*not used */
  {6,2,1,3,5}, /* WHITE : dead, alive, critical, unknown, unchecked */
  {6,2,1,3,5}  /* BLACK : dead, alive, critical, unknown, unchecked */
};

static const int domain_colors[4] = {5, 1, 2, 3}; /* gray, black, white, both */


/* True if the coordinate is a hoshi point.
 */
static int
is_hoshi_point(int m, int n)
{
  int hoshi;
  int middle;

  /* No hoshi points on these boards. */
  if (board_size == 2 || board_size == 4)
    return 0;

  /* In the middle of a 3x3 board. */
  if (board_size == 3) {
    if (m == 1 && n == 1)
      return 1;

    return 0;
  }

  if (board_size == 5) {
    if (m == 1 && (n == 1 || n == 3))
      return 1;
    if (m == 2 && n == 2)
      return 1;
    if (m == 3 && (n == 1 || n == 3))
      return 1;

    return 0;
  }

  /* 3-3 points are hoshi on sizes 7--11, 4-4 on larger. */
  if (board_size <= 11)
    hoshi = 2;
  else
    hoshi = 3;

  /* Coordinate for midpoint. */
  middle = board_size/2;
    
  /* Normalize the coordinates by mirroring to the lower numbers. */
  if (m >= middle)
    m = board_size - 1 - m;
  if (n >= middle)
    n = board_size - 1 - n;
  
  /* Is this a corner hoshi? */
  if (m == hoshi && n == hoshi)
    return 1;

  /* If even sized board, only hoshi points in the corner. */
  if (board_size%2 == 0)
    return 0;

  /* Less then 12 in board size only middle point. */
  if (board_size < 12) {
    if (m == middle && n == middle)
      return 1;

    return 0;
  }

  /* Is this a midpoint hoshi? */
  if ((m == hoshi || m == middle)
      && (n == hoshi || n == middle))
    return 1;

  /* No more chances. */
  return 0;
}

/* Print a line with coordinate letters above the board. */
static void
draw_letter_coordinates(FILE *outfile)
{
  int i;
  int ch;
  
  fprintf(outfile, "  ");
  for (i = 0, ch = 'A'; i < board_size; i++, ch++) {
    if (ch == 'I')
      ch++;
    fprintf(outfile, " %c", ch);
  }
}


/* The following four functions define an API for drawing boards. The
 * typical use would be along the following lines:
 *
 * start_draw_board();
 * for (m = 0; m < board_size; m++)
 *   for (n = 0; n < board_size; n++) {
 *     int color = ...;
 *     int c = ...;
 *     draw_color_char(m, n, c, color);
 *   }
 * end_draw_board();
 *
 * Coordinate system, hoshi points, and linefeeds are written
 * automatically by the board drawing functions. The coordinates m, n
 * must be ordered as in the full loops above.
 *
 */

/* Init color and print a line with coordinate letters above the board. */
void
start_draw_board()
{
  gg_init_color();
  draw_letter_coordinates(stderr);
}

/* Draw a colored character. If c has the value EMPTY, either a "." or
 * a "+" is drawn, depending on whether it is a hoshi stone. If this
 * is the first or last intersection on a line, the coordinate number
 * is also drawn.
 */
void
draw_color_char(int m, int n, int c, int color)
{
  /* Is this the first column? */
  if (n == 0)
    fprintf(stderr, "\n%2d", board_size - m);

  /* Do we see a hoshi point? */
  if (c == EMPTY) {
    if (is_hoshi_point(m, n))
      c = '+';
    else
      c = '.';
  }

  /* Use fprintf to draw black characters. This way they'll turn out
   * white on terminals with black background.
   */
  if (color == GG_COLOR_BLACK)
    fprintf(stderr, " %c", c);
  else
    write_color_char(color, c);
  
  /* Is this the last column? */
  if (n == board_size - 1)
    fprintf(stderr, " %-2d", board_size - m);
}

/* Draw a black character as specified above. */
void
draw_char(int m, int n, int c)
{
  draw_color_char(m, n, c, GG_COLOR_BLACK);
}

/* Print a line with coordinate letters under the board. */
void
end_draw_board()
{
  fprintf(stderr, "\n");
  draw_letter_coordinates(stderr);
  fprintf(stderr, "\n");
}


/* 
 * Write one stone. Use 'empty' if the board is empty ('-' or '+')
 * We use capital letters A,B,... for black, lower case a,b,... for white.
 * This allows us to indicate up to 26 dragons uniquely, and more with
 * low risk of ambiguity.
 */

/* The variable xo=1 if running gnugo -T, 2 if running gnugo -E, or
 * 3 if displaying owl_status.
 */

static void 
showchar(int i, int j, int empty, int xo)
{
  struct dragon_data *d;  /* dragon data at (i, j) */
  int x;
  ASSERT_ON_BOARD2(i, j);
  x = BOARD(i, j);
  d = &(dragon[POS(i, j)]);

  if (x == EMPTY) {
    if (xo != 2)
      fprintf(stderr, " %c", empty);
    else {
      int empty_color;
      char empty_char;
      
      if (black_eye[POS(i, j)].color == BLACK_BORDER) {
	if (white_eye[POS(i, j)].color == WHITE_BORDER)
	  empty_color = domain_colors[3];
	else
	  empty_color = domain_colors[1];

	if (black_eye[POS(i, j)].marginal)
	  empty_char = '!';
	else
	  empty_char = 'x';
      }
      else if (white_eye[POS(i, j)].color == WHITE_BORDER) {
	empty_color = domain_colors[2];
	if (white_eye[POS(i, j)].marginal)
	  empty_char = '!';
	else
	  empty_char = 'o';
      }
      else {
	empty_color = domain_colors[0];
	empty_char = '.';
      }

      write_color_char(empty_color, empty_char);
    }
  }
  else {
    int w;

    if (xo == 0 || ! ON_BOARD1(d->origin)) {
      fprintf(stderr, " %c", BOARD(i, j) == BLACK ? 'X' : 'O');
      return;
    }

    /* Figure out ascii character for this dragon. This is the
     * dragon number allocated to the origin of this worm. */

    w = dragon_num[d->origin];
    if (!w) {
      /* Not yet allocated - allocate next one. */
      /* Count upwards for black, downwards for white to reduce confusion. */
      if (BOARD(i, j) == BLACK)
	w = dragon_num[d->origin] = next_black++;
      else
	w = dragon_num[d->origin] = next_white--; 
    }

    w = w%26 + (BOARD(i, j) == BLACK ? 'A' : 'a');
    
    /* Now draw it. */
    if (xo == 1)
      write_color_char(colors[BOARD(i, j)][d->crude_status], w);
    else if (xo == 2) {
      if (BOARD(i, j) == BLACK)
	write_color_char(domain_colors[1], 'X');
      else
	write_color_char(domain_colors[2], 'O');
    }
    else if (xo == 3)
      write_color_char(colors[BOARD(i, j)][d->owl_status], w);
    else if (xo == 4)
      write_color_char(colors[BOARD(i, j)][d->status], w);
  }
}




/*
 * Show go board.
 *
 * xo=0:      black and white XO board for ascii game
 * xo=1:      colored dragon display
 * xo=2:      colored eye display
 * xo=3:      colored owl display
 * xo=4:      colored matcher status display
 *
 */

void
showboard(int xo)
{
  int i, j, ii;
  gg_init_color();

  /* Set all dragon numbers to 0. */
  memset(dragon_num, 0, sizeof(dragon_num));
  
  next_white = (259 - 26);
  next_black = 26;
  
  start_draw_board();
  
  for (i = 0; i < board_size; i++) {
    ii = board_size - i;
    fprintf(stderr, "\n%2d", ii);
    
    for (j = 0; j < board_size; j++)
      showchar(i, j, is_hoshi_point(i, j) ? '+' : '.', xo);
    
    fprintf(stderr, " %d", ii);
    
    if (xo == 0 && ((board_size < 10 && i == board_size-2)
		    || (board_size >= 10 && i == 8)))
      fprintf(stderr, "     WHITE has captured %d stones", black_captured);
    
    if (xo == 0 && ((board_size < 10 && i == board_size-1)
		    || (board_size >= 10 && i == 9)))
      fprintf(stderr, "     BLACK has captured %d stones", white_captured);
    
    if (xo == 3) {
      if (i == board_size-5)
	write_color_string(GG_COLOR_GREEN, "    green=alive");
      if (i == board_size-4)
	write_color_string(GG_COLOR_CYAN, "    cyan=dead");
      if (i == board_size-3)
	write_color_string(GG_COLOR_RED, "    red=critical");
      if (i == board_size-2)
	write_color_string(GG_COLOR_YELLOW, "    yellow=unknown");
      if (i == board_size-1)
	write_color_string(GG_COLOR_MAGENTA, "    magenta=unchecked");
    }
  }

  end_draw_board();
}


/* Bare bones version of showboard(0). No fancy options, no hint of
 * color, and you can choose where to write it.
 */
void
simple_showboard(FILE *outfile)
{
  int i, j;

  draw_letter_coordinates(outfile);
  
  for (i = 0; i < board_size; i++) {
    fprintf(outfile, "\n%2d", board_size - i);
    
    for (j = 0; j < board_size; j++) {
      if (BOARD(i, j) == EMPTY)
	fprintf(outfile, " %c", is_hoshi_point(i, j) ? '+' : '.');
      else
	fprintf(outfile, " %c", BOARD(i, j) == BLACK ? 'X' : 'O');
    }

    fprintf(outfile, " %d", board_size - i);
    
    if ((board_size < 10 && i == board_size-2)
	|| (board_size >= 10 && i == 8))
      fprintf(outfile, "     WHITE has captured %d stones", black_captured);
    
    if ((board_size < 10 && i == board_size-1)
	|| (board_size >= 10 && i == 9))
      fprintf(outfile, "     BLACK has captured %d stones", white_captured);
  }
  
  fprintf(outfile, "\n");
  draw_letter_coordinates(outfile);
}

/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
