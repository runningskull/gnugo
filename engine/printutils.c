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

#include "gnugo.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include <ctype.h>

#include "liberty.h"
#include "gnugo.h"
#include "gg_utils.h"
#include "cache.h"

/*
 * This function underpins all the TRACE and DEBUG stuff.
 * It accepts %c, %d, %f, %s, and %x as usual. But it
 * also accepts %m, which takes TWO integers and writes a move.
 * Other accepted formats are
 * %H: Print a hashvalue.
 * %C: Print a color as a string.
 * %M: Like %m but three characters wide for all moves (pads with spaces).
 * Nasty bodge: %o at the start means outdent, i.e. cancel indent.
 */

static void 
vgprintf(FILE *outputfile, const char *fmt, va_list ap)
{
  if (fmt[0] == '%' && fmt[1] == 'o')
    fmt += 2;  /* cancel indent */
  else if (stackp > 0)
    fprintf(outputfile, "%.*s", stackp*2, "                                ");

  for (; *fmt ; ++fmt) {
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
	char *s = va_arg(ap, char*);
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
	else if (!ON_BOARD2(m, n))
	  fprintf(outputfile, "[%d,%d]", m, n);
	else {
	  /* Generate the move name. */
	  if (n < 8)
	    movename[0] = n + 65;
	  else
	    movename[0] = n + 66;
	  if (*fmt == 'm')
	    sprintf(movename+1, "%d", board_size - m);
	  else
	    sprintf(movename+1, "%-2d", board_size - m);
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
	else if (!ON_BOARD1(pos))
	  fprintf(outputfile, "[%d]", pos);
	else {
	  /* Generate the move name. */
	  if (n < 8)
	    movename[0] = n + 65;
	  else
	    movename[0] = n + 66;
	  if (*fmt == 'm')
	    sprintf(movename + 1, "%d", board_size - m);
	  else
	    sprintf(movename + 1, "%-2d", board_size - m);
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
gfprintf(FILE *outfile, const char *fmt, ...)
{
  va_list ap;
  va_start(ap, fmt);
  vgprintf(outfile, fmt, ap);
  va_end(ap);
}


/*
 * required wrapper around vgprintf, writes to stderr.
 * Always returns 1 to allow use in short-circuit logical expressions.
 */

int 
gprintf(const char *fmt, ...)
{
  va_list ap;
  va_start(ap, fmt);
  vgprintf(stderr, fmt, ap);
  va_end(ap);
  return 1;
}


/*
 * required wrapper around vgprintf, in contrast to gprintf this one
 * writes to stdout.
 */

void
mprintf(const char *fmt, ...)
{
  va_list ap;
  va_start(ap, fmt);
  vgprintf(stdout, fmt, ap);
  va_end(ap);
}

/*
 * A wrapper around abort() which shows the state variables at the time
 * of the problem.  (x, y) are typically a related move, or -1, -1.
 */

void 
abortgo(const char *file, int line, const char *msg, int x, int y)
{
  verbose = 4;
  TRACE("%o\n\n***assertion failure:\n%s:%d - %s near %m***\n\n",
	file, line, msg, x, y);
  dump_stack();

  /* Dump the stack as board images. */
  showboard(0);
  while (stackp > 0) {
    popgo();
    showboard(0);
  }

  fprintf(stderr, "\ngnugo %s (seed %d): You stepped on a bug.\n",
          gg_version(), random_seed);
  if (board_size >= 9 && board_size <= 19) {
    fprintf(stderr, "\
Please save this game as an sgf file and mail it to gnugo@gnu.org\n\
If you can, please also include the debug output above this message.\n");
  }
  fprintf(stderr, "\n");

  fflush(stderr);
  fflush(stdout);

  abort();  /* cause core dump */
}


/* Convert a color value to a string. */
const char *
color_to_string(int color)
{
  if (color == EMPTY)
    return "empty";
  else if (color == WHITE)
    return "white";
  else if (color == BLACK)
    return "black";
  else if (color == GRAY)
    return "gray";
  else if (color == WHITE_BORDER)
    return "white border";
  else if (color == BLACK_BORDER)
    return "black border";
  else
    return "purple?";
}

/* Convert a location to a string. */
const char *
location_to_string(int pos)
{
  static int init = 0;
  static char buf[BOARDSIZE][5];
  if (!init) {
    int pos;
    for (pos = 0; pos < BOARDSIZE; pos++)
      location_to_buffer(pos, buf[pos]);
    init = 1;
  }
  ASSERT1(pos >= 0 && pos < BOARDSIZE, pos);
  return buf[pos];
}

/* Convert a location to a string, writing to a buffer. */

void
location_to_buffer(int pos, char *buf)
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

/* Convert a status value to a string. */
const char *
status_to_string(int status)
{
  if (status == DEAD)
    return "DEAD";
  else if (status == ALIVE)
    return "ALIVE";
  else if (status == CRITICAL)
    return "CRITICAL";
  else if (status == UNKNOWN)
    return "UNKNOWN";
  else if (status == UNCHECKED)
    return "UNCHECKED";
  else if (status == CAN_THREATEN_ATTACK)
    return "CAN_THREATEN_ATTACK";
  else if (status == CAN_THREATEN_DEFENSE)
    return "CAN_THREATEN_DEFENSE";
  else
    return "ERROR";
}


/* Convert a safety value to a string. */
const char *
safety_to_string(int status)
{
  if (status == DEAD)
    return "DEAD";
  else if (status == ALIVE)
    return "ALIVE";
  else if (status == CRITICAL)
    return "CRITICAL";
  else if (status == INESSENTIAL)
    return "INESSENTIAL";
  else if (status == TACTICALLY_DEAD)
    return "TACTICALLY_DEAD";
  else if (status == ALIVE_IN_SEKI)
    return "ALIVE_IN_SEKI";
  else if (status == STRONGLY_ALIVE)
    return "STRONGLY_ALIVE";
  else if (status == INVINCIBLE)
    return "INVINCIBLE";
  else if (status == INSUBSTANTIAL)
    return "INSUBSTANTIAL";
  else if (status == CAN_THREATEN_ATTACK)
    return "CAN_THREATEN_ATTACK";
  else if (status == CAN_THREATEN_DEFENSE)
    return "CAN_THREATEN_DEFENSE";
  else
    return "ERROR";
}

/* Convert a routine to a string. */
const char *
routine_to_string(int routine)
{
  if (routine == FIND_DEFENSE)
    return "FIND_DEFENSE";
  else if (routine == ATTACK)
    return "ATTACK";
  else if (routine == OWL_ATTACK)
    return "OWL_ATTACK";
  else if (routine == OWL_DEFEND)
    return "OWL_DEFEND";
  else if (routine == SEMEAI)
    return "SEMEAI";
  else if (routine == CONNECT)
    return "CONNECT";
  else if (routine == DISCONNECT)
    return "DISCONNECT";
  else
    return "ERROR";
}

/* Convert a read result to a string */
const char *
result_to_string(int result)
{
  switch (result) {
  case 0:             return "0";
  case KO_B:          return "KO_B";
  case LOSS:          return "LOSS";
  case GAIN:          return "GAIN";
  case KO_A:          return "KO_A";
  case WIN:           return "WIN";

  /* ALIVE_IN_SEKI is not defined as a return code, but is used here anyhow. */
  case ALIVE_IN_SEKI: return "SEKI";
  default:            return "ERROR";
  }
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
string_to_location(int boardsize, char *str, int *m, int *n)
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


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
