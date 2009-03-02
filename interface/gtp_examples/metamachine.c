/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * This is GNU Go, a Go program. Contact gnugo@gnu.org, or see       *
 * http://www.gnu.org/software/gnugo/ for more information.          *
 *                                                                   *
 * Copyright 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007,   *
 * 2008 and 2009 by the Free Software Foundation.                    *
 *                                                                   *
 * This program is free software; you can redistribute it and/or     *
 * modify it under the terms of the GNU General Public License as    *
 * published by the Free Software Foundation - version 3             *
 * or (at your option) any later version.                            *
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

/*
 * This program sits between a GTP client and a GTP engine,
 * passing commands back and forth and modifying them in 
 * some cases.
 *
 * To the client it appears to be a GTP engine. 
 *                
 *             stdin             pipe a
 *  GTP client ----> metamachine -----> GTP engine
 *             <----             <-----
 *            stdout             pipe b
 *
 * Most commands are passed verbatim to the engine. The
 * exception is gg_genmove, which is intercepted then
 * processed differently. The top two moves are both
 * tried, the position evaluated by estimate_score,
 * and the move yielding the higher score is selected.
 *
 * Usage: no arguments gives normal GTP behavior.
 * 'metamachine --debug' sends diagnostics to stderr.  */

#include <stdio.h>
#include <unistd.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>

void error(const char *msg);
void gprintf(FILE *outputfile, const char *fmt, ...);
void trace(const char *fmt, ...);
void tell_gnugo(char *gnugo_line, const char *msg);

int boardsize = 19;
char delimiters[] = " \t\r\n";
char gnugo_line[128], client_line[128];
FILE *to_gnugo_stream, *from_gnugo_stream;

int debug = 0;

#define EMPTY        0
#define WHITE        1
#define BLACK        2

#define GTP_BUFSIZE  1000

void ask_gnugo(char *line, int verbose, const char *msg);

int
main(int argc, char *const *argv)
{
  int pfd_a[2];
  int pfd_b[2];
  int id;
  int k;
  char command[GTP_BUFSIZE];

  for (k = 1; k < argc; k++)
    if (argc > 1 && strstr(argv[k], "debug"))
      debug = 1;
  if (pipe(pfd_a) == -1)
    error("can't open pipe a");
  if (pipe(pfd_b) == -1)
    error("can't open pipe b");
  switch (fork()) {
  case -1:
    error("fork failed (try chopsticks)");
  case 0:
    /* Attach pipe a to stdin */
    if (dup2(pfd_a[0], 0) == -1) 
      error("dup pfd_a[0] failed");
    /* attach pipe b to stdout" */
    if (dup2(pfd_b[1], 1) == -1)
      error("dup pfd_b[1] failed");
    execlp("gnugo", "gnugo", "--mode", "gtp", "--quiet", NULL);
    error("execlp failed");
  }
  /* Attach pipe a to to_gnugo_stream  */
  to_gnugo_stream = fdopen(pfd_a[1], "w");
  /* Attach pipe b to from_gnugo_stream */
  from_gnugo_stream = fdopen(pfd_b[0], "r");
  
  while (1) {
    char *p;
    int n;

    if (!fgets(client_line, GTP_BUFSIZE, stdin)
	|| (strstr(client_line, "quit") == client_line)) {
      tell_gnugo("quit\n", "a");
	return 1;
    }

    /* remove comments */
    if ((p = strchr(client_line, '#')) != NULL)
      *p = 0;
    
    p = client_line;

    /* Look for an identification number. */
    if (sscanf(p, "%d%n", &id, &n) == 1)
      p += n;
    else
      id = -1; /* No identification number. */
    trace("id = %d\n", id);

    /* Look for command name. */
    if (sscanf(p, " %s %n", command, &n) < 1)
      continue; /* Whitespace only on this line, ignore. */
    p += n;
    trace("command: %s\n", command);

    if (!strncmp(command, "boardsize", 9)) {
      char *token;
      tell_gnugo(client_line, "b");
      ask_gnugo(gnugo_line, 1, "1");

      token = strtok(client_line, delimiters);
      token = strtok(NULL, delimiters);
      boardsize = atoi(token);
    }
    else if (!strncmp(command, "gg_genmove", 10)) {
      int move_i[10], move_j[10];
      float move_value[10], position_value[10];
      int moves_considered;
      int k;
      char *token;
      int line_length = 0;
      int color;

      if (strstr(client_line, "black"))
	color = BLACK;
      else if (strstr(client_line, "white"))
	color = WHITE;
      else {
	color = EMPTY;
	printf("?\n\n");
      }

      if (color == BLACK)
	tell_gnugo("top_moves_black\n", "c");
      else 
	tell_gnugo("top_moves_white\n", "d");

      ask_gnugo(gnugo_line, 0, "2");
      token = strtok(gnugo_line, delimiters);
      for (k = 0; k < 10; k++) {
	move_i[k] = -1;
	move_j[k] = -1;
	move_value[k] = 0.0;
      }
      for (k = 0; k < 10; k++) {
	token = strtok(NULL, delimiters);
	if (!token)
	  break;
	string_to_location(boardsize, token, move_i+k, move_j+k);
	token = strtok(NULL, delimiters);
	if (!token)
	  break;
	sscanf(token, "%f", move_value+k);
	trace("move %d: %m valued %f\n", k,
		  move_i[k], move_j[k], move_value[k]);
      }
      moves_considered = k;
      if (debug)
	fprintf(stderr, "moves considered: %d\n",
		moves_considered);
      for (k = 0; k < 2 && k < moves_considered; k++) {
	float upper, lower;
	int n;

	trace("%s %m\n", color == BLACK ? "black" : "white",
		  move_i[k], move_j[k]);
	gprintf(to_gnugo_stream, "%s %m\n", color == BLACK ? "black" : "white",
		move_i[k], move_j[k]);
	fflush(to_gnugo_stream);
	ask_gnugo(gnugo_line, 0, "3");
	tell_gnugo("estimate_score\n", "e");
	ask_gnugo(gnugo_line, 0, "4");
	strtok(gnugo_line, "()\n");
	token = strtok(NULL, "()\n");
	trace("%s\n", token);
	sscanf(token, "upper bound: %f, lower: %f%n",
	       &upper, &lower, &n);
	if (n < 2)
	  error("can't read territory");
	trace("upper %f, lower %f\n", upper, lower);
	tell_gnugo("undo\n", "f");
	ask_gnugo(gnugo_line, 0, "5");
	fflush(stdout);
	if (color == BLACK)
	  position_value[k] = - upper;
	else
	  position_value[k] = lower;
	trace("position value %f\n", position_value[k]);
      }
      if (moves_considered == 0) {
	if (id == -1)
	  gprintf(stdout, "= PASS\n\n");
	else
	  gprintf(stdout, "=%d PASS\n\n", id);
	fflush(stdout);
      }
      else if (moves_considered == 1
	       || position_value[0] > position_value[1]) {
	gprintf(to_gnugo_stream, "%s %m\n", color == BLACK ? "black" : "white",
		move_i[0], move_j[0]);
	ask_gnugo(gnugo_line, 0, "6");
	if (id == -1)
	  gprintf(stdout, "= %m\n\n", move_i[0], move_j[0]);
	else
	  gprintf(stdout, "=%d %m\n\n", id, move_i[0], move_j[0]);
	fflush(stdout);
      }
      else {
	gprintf(to_gnugo_stream, "%s %m\n", color == BLACK ? "black" : "white",
		move_i[1], move_j[1]);
	ask_gnugo(gnugo_line, 0, "7");
	if (id == -1)
	  gprintf(stdout, "= %m\n\n", move_i[1], move_j[1]);
	else
	  gprintf(stdout, "=%d %m\n\n", id, move_i[1], move_j[1]);
	fflush(stdout);
      }
    }
    else {
      tell_gnugo(client_line, "g");
      ask_gnugo(gnugo_line, 1, "8");
    }
    /* loadsgf commands could change the boardsize, so we get
     * it from the engine, after the command is run. */
    if (!strncmp(command, "loadsgf", 7)) {
      tell_gnugo("query_boardsize\n", "i");
      ask_gnugo(gnugo_line, 0, "10");
      if (!sscanf(gnugo_line, "= %d", &boardsize))
	error("can't get boardsize");
      trace("setting boardsize %d\n", boardsize);
      fflush(stderr);
    }
  }
}

 
/* bark and barf */

void
error(const char *msg)      
{
  fprintf(stderr, "metamachine: %s\n", msg);
  tell_gnugo("quit\n", msg);
  abort();
}

/* Send a GTP command to the engine. */

void
tell_gnugo(char *gnugo_line, const char *msg)
{
  gprintf(to_gnugo_stream, gnugo_line);
  fflush(to_gnugo_stream);
  if (debug) {
    fprintf(stderr, "%s: %s", msg, gnugo_line);
    fflush(stderr);
  }
}

/* Obtains the engine's response to a GTP command. If verbose is true,
 * the reply is echoed to stdout.
 */

void
ask_gnugo(char *gnugo_line, int verbose, const char *msg)
{
  int line_length = 0;
  char line[GTP_BUFSIZE];

  while (line_length != 1) {
    if (!fgets(line, 128, from_gnugo_stream))
      error("can't get response");
    line_length = strlen(line);
    if (line_length > 1 
	&& (line[0] == '=' || line[0] == '?'))
      strncpy(gnugo_line, line, 128);
    if (verbose)
      printf(line);
    if (debug)
      fprintf(stderr, "%s: %s\n", msg, gnugo_line);
  }
  if (verbose)
    fflush(stdout);
  fflush(stderr);
}


/* Adapted from GNU Go. Formatted output with %m format. */

void 
gprintf(FILE *outputfile, const char *fmt, ...)
{
  va_list ap;
  va_start(ap, fmt);
  vgprintf(outputfile, fmt, ap);
  fflush(outputfile);
  va_end(ap);
}

/* print diagnostic */

void
trace(const char *fmt, ...)
{
  va_list ap;
  if (debug) {
    va_start(ap, fmt);
    vgprintf(stderr, fmt, ap);
    fflush(stderr);
    va_end(ap);
  }
}

int
vgprintf(FILE *outputfile, const char *fmt, va_list ap)
{
  for ( ; *fmt; ++fmt) {
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
      case 'm':
      case 'M':
	{
	  char movename[4];
	  int m = va_arg(ap, int);
	  int n = va_arg(ap, int);
	  if (m == -1 && n == -1)
	    fputs("PASS", outputfile);
	  else if (m < 0 || n < 0 || m >= 19 || n >= 19)
	    fprintf(outputfile, "[%d,%d]", m, n);
	  else {
	    /* Generate the move name. */
	    if (n < 8)
	      movename[0] = n + 65;
	    else
	      movename[0] = n + 66;
	    if (*fmt == 'm')
	      sprintf(movename+1, "%d", boardsize-m);
	    else
	      sprintf(movename+1, "%-2d", boardsize-m);
	    fputs(movename, outputfile);
	  }
	  break;
	}
      default:
	{
	  fprintf(outputfile, "\n\nUnknown format character: '%c'\n", *fmt);
	  abort();
	}
      }
    }
    else
      putc(*fmt, outputfile);
  }
  fflush(outputfile);
}

/* Extracts coordinates from a location in algebraic notation */

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

