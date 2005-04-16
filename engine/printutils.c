/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * This is GNU Go, a Go program. Contact gnugo@gnu.org, or see       *
 * http://www.gnu.org/software/gnugo/ for more information.          *
 *                                                                   *
 * Copyright 1999, 2000, 2001, 2002, 2003 and 2004                   *
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

#include "board.h"
#include "board-private.h"
#include "hash.h"
#include "gg_utils.h"
#include "sgftree.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include <ctype.h>

/*
 * This function underpins all the TRACE and DEBUG stuff.
 * It accepts %c, %d, %f, %s, and %x as usual. But it
 * also accepts %m, which takes TWO integers and writes a move.
 * Other accepted formats are
 * %H: Print a hashvalue.
 * %C: Print a color as a string.
 * Nasty bodge: %o at the start means outdent, i.e. cancel indent.
 */

void
vgprintf(const Goban *goban, FILE *outputfile, const char *fmt, va_list ap)
{
  if (fmt[0] == '%' && fmt[1] == 'o')
    fmt += 2;  /* Cancel indentation. */
  else if (goban->stackp > 0) {
    fprintf(outputfile, "%.*s", goban->stackp * 2,
	    "                                ");
  }

  for (; *fmt; ++fmt) {
    if (*fmt == '%') {
      switch (*++fmt) {
      case 'c':
      {
	/* rules of promotion => passed as int, not char */
	int c = va_arg(ap, int);
	putc(c, outputfile);
	break;
      }
      case 'd':
      {
	int d = va_arg(ap, int);
	fprintf(outputfile, "%d", d);
	break;
      }
      case 'x':
      {
	unsigned int d = va_arg(ap, unsigned int);
	fprintf(outputfile, "%x", d);
	break;
      }
      case 'f':
      {
	double f = va_arg(ap, double); /* passed as double, not float */
	fprintf(outputfile, "%.2f", f);
	break;
      }
      case 's':
      {
	char *s = va_arg(ap, char *);
	fputs(s, outputfile);
	break;
      }
      case '2':
	fmt++;
	if (*fmt != 'm' && *fmt != 'M') {
	  fprintf(outputfile, "\n\nUnknown format string '2%c'\n", *fmt);
	  break;
	}
	/* else fall through - 2 modifier on %m is default. */
      case 'm':
      case 'M':
      {
	char movename[4];
	int m = va_arg(ap, int);
	int n = va_arg(ap, int);
	if (m == -1 && n == -1)
	  fputs("PASS", outputfile);
	else if (!ON_BOARD2(goban, m, n))
	  fprintf(outputfile, "[%d,%d]", m, n);
	else {
	  /* Generate the move name. */
	  if (n < 8)
	    movename[0] = n + 65;
	  else
	    movename[0] = n + 66;
	  if (*fmt == 'm')
	    sprintf(movename+1, "%d", goban->board_size - m);
	  else
	    sprintf(movename+1, "%-2d", goban->board_size - m);
	  fputs(movename, outputfile);
	}
	break;
      }
      case '1':
      fmt++;
      if (*fmt != 'm' && *fmt != 'M') {
	fprintf(outputfile, "\n\nUnknown format string '1%c'\n", *fmt);
	break;
      }
      else {
	char movename[4];
	int pos = va_arg(ap, int);
	int m = I(pos);
	int n = J(pos);
	if (pos == NO_MOVE)
	  fputs("PASS", outputfile);
	else if (!ON_BOARD1(goban, pos))
	  fprintf(outputfile, "[%d]", pos);
	else {
	  /* Generate the move name. */
	  if (n < 8)
	    movename[0] = n + 65;
	  else
	    movename[0] = n + 66;
	  if (*fmt == 'm')
	    sprintf(movename + 1, "%d", goban->board_size - m);
	  else
	    sprintf(movename + 1, "%-2d", goban->board_size - m);
	  fputs(movename, outputfile);
	}
	break;
      }
      case 'H':
      {
	unsigned long h = va_arg(ap, unsigned long);
	fprintf(outputfile, "%lx", h);
	break;
      }
      case 'C':
      {
	int color = va_arg(ap, int);
	fputs(color_to_string(color), outputfile);
	break;
      }
      default:
	fprintf(outputfile, "\n\nUnknown format character '%c'\n", *fmt);
	break;
      }
    }
    else
      putc(*fmt, outputfile);
  }
}


/*
 * required wrapper around vgprintf, writes to outfile.
 */

void
gfprintf(const Goban *goban, FILE *outfile, const char *fmt, ...)
{
  va_list ap;
  va_start(ap, fmt);
  vgprintf(goban, outfile, fmt, ap);
  va_end(ap);
}


/*
 * required wrapper around vgprintf, writes to stderr.
 * Always returns 1 to allow use in short-circuit logical expressions.
 */

int
gprintf(const Goban *goban, const char *fmt, ...)
{
  va_list ap;
  va_start(ap, fmt);
  vgprintf(goban, stderr, fmt, ap);
  va_end(ap);
  return 1;
}


/*
 * required wrapper around vgprintf, in contrast to gprintf this one
 * writes to stdout.
 */

void
mprintf(const Goban *goban, const char *fmt, ...)
{
  va_list ap;
  va_start(ap, fmt);
  vgprintf(goban, stdout, fmt, ap);
  va_end(ap);
}

/* This writes the move history information in sgf format to stderr.
 * This is only intended as a stand-alone debug tool for use in
 * abortgo(). Anywhere else you should use the normal sgf library.
 */
static void
dump_board_sgf(const Goban *goban)
{
  int pos;
  int initial_colors_found = EMPTY;
  int color;
  int k;

  for (pos = BOARDMIN; pos < BOARDMAX; pos++)
    if (ON_BOARD(goban, pos))
      initial_colors_found |= goban->private->initial_board[pos];

  fprintf(stderr, "(;GM[1]FF[4]SZ[%d]KM[%.1f]GN[GNU Go %s stepped on a bug]\n",
	  goban->board_size, goban->komi, gg_version());

  for (color = WHITE; color <= BLACK; color++) {
    if (initial_colors_found & color) {
      fprintf(stderr, "A%s", color == WHITE ? "W" : "B");
      for (k = 0, pos = BOARDMIN; pos < BOARDMAX; pos++) {
	if (ON_BOARD(goban, pos)
	    && goban->private->initial_board[pos] == color) {
	  fprintf(stderr, "[%c%c]", 'a' + J(pos), 'a' + I(pos));
	  k++;
	  if (k % 16 == 0)
	    fprintf(stderr, "\n");
	}
      }
      if (k % 16 != 0)
	fprintf(stderr, "\n");
    }
  }

  if (goban->private->move_history_pointer > 0) {
    for (k = 0; k < goban->private->move_history_pointer; k++) {
      fprintf(stderr, ";%s",
	      goban->private->move_history_color[k] == WHITE ? "W" : "B");
      if (goban->private->move_history_pos[k] == PASS_MOVE)
	fprintf(stderr, "[]");
      else {
	fprintf(stderr, "[%c%c]",
		'a' + J(goban->private->move_history_pos[k]),
		'a' + I(goban->private->move_history_pos[k]));
      }

      if (k % 12 == 11)
	fprintf(stderr, "\n");
    }
    if (k % 12 != 0)
      fprintf(stderr, "\n");
  }
  fprintf(stderr, ")\n");
}

/*
 * A wrapper around abort() which shows the state variables at the time
 * of the problem. (pos) is typically a related move, or NO_MOVE.
 */

void
abortgo(const Goban *goban, const char *file, int line,
	const char *msg, int pos)
{
  if (goban) {
    gprintf(goban, "%o\n\n***assertion failure:\n%s:%d - %s near %1m***\n\n",
	    file, line, msg, pos);

    dump_stack(goban);

    /* Print the board at the top of the stack. */
    simple_showboard(goban, stderr);
    fprintf(stderr, "\n");

    dump_board_sgf(goban);
  }
  else {
    fprintf(stderr, "\n\n***assertion failure:\n%s:%d - %s***\n\n",
	    file, line, msg);
  }

  fprintf(stderr, "gnugo %s (seed %d): You stepped on a bug.\n",
	  gg_version(), get_random_seed());

  if (goban && goban->board_size >= 9 && goban->board_size <= 19) {
    fprintf(stderr, "\
Please mail this message, including the debug output above,	\
to gnugo@gnu.org\n");
  }

  fprintf(stderr, "\n");

  fflush(stderr);
  fflush(stdout);

  abort();  /* cause core dump */
}


static const char *color_names[] = {
  COLOR_NAMES
};

/* Convert a color value to a string. */
const char *
color_to_string(int color)
{
  gg_assert(NULL, color < NUM_KOMASTER_STATES);
  return color_names[color];
}

/* Convert a location to a string. */
const char *
location_to_string(int board_size, int pos)
{
  static int initialized = 0;
  static char buffers[MAX_BOARD][MAX_BOARD][5];

  int i;
  int j;

  if (!initialized) {
    for (i = 0; i < MAX_BOARD; i++) {
      for (j = 0; j < MAX_BOARD; j++)
	location_to_buffer(MAX_BOARD, POS(i, j), buffers[i][j]);
    }

    initialized = 1;
  }

  i = I(pos);
  j = J(pos);

  ASSERT1(NULL, MIN_BOARD <= board_size && board_size <= MAX_BOARD, pos);
  ASSERT1(NULL, 0 <= i && i < board_size && 0 <= j && j < board_size, pos);

  return buffers[i + (MAX_BOARD - board_size)][j];
}

/* Convert a location to a string, writing to a buffer. */

void
location_to_buffer(int board_size, int pos, char *buf)
{
  char *bufp = buf;
  int i = I(pos);
  int j = J(pos);

  if (pos == NO_MOVE) {
    strcpy(buf, "Pass");
    return;
  }

  *bufp = 'A'+j;
  if (*bufp >= 'I')
    (*bufp)++;
  bufp++;

  i = board_size - i;
  if (i > 9)
    *bufp++ = '0' + i/10;
  *bufp++ = '0' + i%10;

  *bufp = 0;
}


/*
 * Get the (m, n) coordinates in the standard GNU Go coordinate system
 * from the string STR.  This means that m is the nth row from the top
 * and n is the column. Both coordinates are between 0 and boardsize-1,
 * inclusive.
 *
 * Return 1 if ok, otherwise return 0;
 */

int
string_to_location(int boardsize, const char *str, int *m, int *n)
{
  if (*str == '\0')
    return 0;

  if (!isalpha((int) *str))
    return 0;
  *n = tolower((int) *str) - 'a';
  if (tolower((int) *str) >= 'i')
    --*n;
  if (*n < 0 || *n > boardsize - 1)
    return 0;

  if (!isdigit((int) *(str+1)))
    return 0;
  *m = boardsize - atoi(str + 1);
  if (*m < 0 || *m > boardsize - 1)
    return 0;

  return 1;
}


/* Some simple functions to draw an ASCII board. */

/* True if the coordinate is a hoshi point. */
int
is_hoshi_point(int board_size, int m, int n)
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
  middle = board_size / 2;

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

  /* Less than 12 in board size only middle point. */
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
void
draw_letter_coordinates(int board_size, FILE *outfile)
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


/* Bare bones version of showboard(0). No fancy options, no hint of
 * color, and you can choose where to write it.
 */
void
simple_showboard(const Goban *goban, FILE *outfile)
{
  int board_size = goban->board_size;
  int i, j;

  draw_letter_coordinates(board_size, outfile);

  for (i = 0; i < board_size; i++) {
    fprintf(outfile, "\n%2d", board_size - i);

    for (j = 0; j < board_size; j++) {
      if (goban->board[POS(i, j)] == EMPTY)
	fprintf(outfile, " %c", is_hoshi_point(board_size, i, j) ? '+' : '.');
      else
	fprintf(outfile, " %c", goban->board[POS(i, j)] == BLACK ? 'X' : 'O');
    }

    fprintf(outfile, " %d", board_size - i);

    if ((board_size < 10 && i == board_size - 2)
	|| (board_size >= 10 && i == 8)) {
      fprintf(outfile, "     WHITE (O) has captured %d stones",
	      goban->black_captured);
    }

    if ((board_size < 10 && i == board_size - 1)
	|| (board_size >= 10 && i == 9)) {
      fprintf(outfile, "     BLACK (X) has captured %d stones",
	      goban->white_captured);
    }
  }

  fprintf(outfile, "\n");
  draw_letter_coordinates(board_size, outfile);
}


/* Adds square marks for each goal intersecion in the
 * `goban->sgf_dumptree'.  This function cannot be in sgf/ as it has
 * to understand the 1-D board.
 */
void
mark_goal_in_sgf(const Goban *goban, char goal[BOARDMAX])
{
  int pos;
  SGFNode *node;

  if (!goban->sgf_dumptree)
    return;

  node = sgftreeNodeCheck(goban->sgf_dumptree);
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (ON_BOARD(goban, pos) && goal[pos])
      sgfSquare(node, I(pos), J(pos));
  }
}



/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
