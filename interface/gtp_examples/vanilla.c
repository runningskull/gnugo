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

/* This program uses two pipes to communicate with the
 * GNU Go client. To an external client it appears to
 * be a gtp engine. It accomplishes this by intercepting
 * gtp commands and passing them on to GNU Go.
 *
 * This program in and of itself is not useful but it
 * can be the basis of useful programs. Example: hack
 * in gmp.c and get a gtp / gmp mediator.
 * 
 * Pipe a: client --> gnugo
 * Pipe b: gnugo --> client
 */

#include <stdio.h>
#include <unistd.h>
#include <string.h>

void error(const char *msg);
#define TELL_GNUGO(x) if (verbose) fprintf(stderr, "%s", x); \
                      if (fprintf(to_gnugo_stream, "%s", x) < 0) \
                         error ("can't write command in to_gnugo_stream"); \
                      fflush(to_gnugo_stream);
#define ASK_GNUGO(x)  if (!fgets(x, 128, from_gnugo_stream)) \
                         error("can't get response");
int
main()
{
  int pfd_a[2];
  int pfd_b[2];
  char gnugo_line[128], client_line[128];
  int length = 0;
  int verbose = 1;
  FILE *to_gnugo_stream, *from_gnugo_stream;

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
  /* We use stderr to communicate with the client since stdout is needed. */
  /* Attach pipe a to to_gnugo_stream  */
  to_gnugo_stream = fdopen(pfd_a[1], "w");
  /* Attach pipe b to from_gnugo_stream */
  from_gnugo_stream = fdopen(pfd_b[0], "r");

  while (1) {
    int length = 0;
    if (!fgets(client_line, 128, stdin) 
	|| !strncmp(client_line, "quit", 4)) {
      TELL_GNUGO("quit\n");
      return 1;
    }
    if (!strncmp(client_line, "genmove_black", 13)) {
      char *token;
      const char delimiters[] = " \t\r\n";
      float value1, value2;

      TELL_GNUGO("top_moves_black\n");
      ASK_GNUGO(gnugo_line);
      token = strtok(gnugo_line, delimiters);
      token = strtok(NULL, delimiters);
      TELL_GNUGO("black ");
      TELL_GNUGO(token);
      TELL_GNUGO("\n");
      ASK_GNUGO(gnugo_line);
      while (length != 1) {
	ASK_GNUGO(gnugo_line);
	length = strlen(gnugo_line);
	printf(gnugo_line);
	fflush(stdout);
      }
    }
    else {
      TELL_GNUGO(client_line);
      while (length != 1) {
	ASK_GNUGO(gnugo_line);
	length = strlen(gnugo_line);
	printf(gnugo_line);
	fflush(stdout);
      }
    }
  }
}

void
error(const char *msg)      
{
  fprintf(stderr, "vanilla: %s\n", msg);
  abort();
}



/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
