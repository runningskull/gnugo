/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * This is GNU GO, a Go program. Contact gnugo@gnu.org, or see       *
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

#include "gnugo.h"
#include "liberty.h"
#include "patterns.h"

#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>

FILE *to_gnugo_stream, *from_gnugo_stream;
char gnugo_line[128];
int gnugo_line_length;

int pfd_a[2];
int pfd_b[2];

#define TELL_GNUGO(x, args...) if (verbose) fprintf(stderr, x, ##args);  \
                         if (fprintf(to_gnugo_stream, x, ##args) < 0) \
                           error ("can't write command in to_gnugo_stream"); \
                         fflush(to_gnugo_stream);
#define ASK_GNUGO(x)   gnugo_line_length = 0; \
                       while (gnugo_line_length != 1) { \
                         if (!fgets(gnugo_line, 128, from_gnugo_stream)) \
                           error("can't get response"); \
                         gnugo_line_length = strlen(gnugo_line); \
                         if (verbose) \
                           fprintf(stderr, gnugo_line); \
                         }

#define MAX_ORACLE_MOVES 10

struct oracle_move_data {
  int pos;            /* move coordinate */
  int color;          /* color to play */
  int value;          /* value */
  int ab_value;       /* alpha-beta value */
  const char *reason; /* why this move */
};

static void oracle_callback(int m, int n, int color, struct pattern *pattern,
			    int ll, void *data);
static void set_mask(void);
static void oracle_add_move(struct oracle_move_data *moves, 
			    int this_move, int this_value, 
			    const char *this_reason);
void do_consult_oracle(int color);
void error(const char *msg);
static int oracle_trymove(int pos, int color, const char *message, int str,
			  int komaster, int kom_pos);
static void oracle_popgo(void);


/* Produce a list of moves within a restricted part of the board
 * using patterns in oracle.db
 */

int
move_mask[BOARDMAX];

/* mark upper left corner */

static void
set_mask(void)
{
  int m, n;

  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      if (m < 10 && n < 10)
	move_mask[POS(m, n)] = 1;
      else
	move_mask[POS(m, n)] = 0;  
    }
}

void
consult_oracle(int color)
{
  set_mask();
  do_consult_oracle(color);
  TELL_GNUGO("quit\n");
}

void
do_consult_oracle(int color)
{
  struct oracle_move_data oracle_moves[MAX_ORACLE_MOVES];
  int k;

  for (k = 0; k < MAX_ORACLE_MOVES; k++)
    oracle_moves[k].value = -1;
  
  matchpat(oracle_callback, color, &oracle_db, oracle_moves, NULL);
  for (k = 0; k < MAX_ORACLE_MOVES; k++)
    if (oracle_moves[k].value > -1) {
      oracle_trymove(oracle_moves[k].pos, color, oracle_moves[k].reason,
		     0, 0, NO_MOVE);
      do_consult_oracle(OTHER_COLOR(color));
      oracle_popgo();
    }
}

static void
oracle_callback(int m, int n, int color, struct pattern *pattern,
		int ll, void *data)
{
  int this_move;
  struct oracle_move_data *moves = data;
  UNUSED(color);

  this_move = AFFINE_TRANSFORM(pattern->movei, pattern->movej, ll, m, n);
  if (move_mask[this_move])
    oracle_add_move(moves, this_move, pattern->value, pattern->name);
  else
    gprintf("outside the area\n");
}

/* Add a move to a list */

static void
oracle_add_move(struct oracle_move_data moves[MAX_ORACLE_MOVES],
		int this_move, int this_value, const char *this_reason)
{
  int k,l;
  
  for (k = 0; k < MAX_ORACLE_MOVES; k++)
    if (moves[k].value == -1
	|| this_value >= moves[k].value)
      break;
  for (l = MAX_ORACLE_MOVES-1; l > k; l--) {
    moves[l].pos = moves[l-1].pos;
    moves[l].value = moves[l-1].value;
    moves[l].reason = moves[l-1].reason;
  }
  moves[k].pos = this_move;
  moves[k].value = this_value;
  moves[k].reason = this_reason;
}

/* Forks and attaches pipes to a new GNU Go process in gtp mode.
 * Loads the sgf file
 */

void
summon_oracle(char *infilename)
{
  if (pipe(pfd_a) == -1)
    error("can't open pipe a");
  if (pipe(pfd_b) == -1)
    error("can't open pipe b");
  switch(fork()) {
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
  TELL_GNUGO("loadsgf %s\n", infilename);
  fflush(to_gnugo_stream);
  gnugo_line_length = 0;
}

void
error(const char *msg)      
{
  fprintf(stderr, "oracle: %s\n", msg);
  abort();
}

static int 
oracle_trymove(int pos, int color, const char *message, int str,
	       int komaster, int kom_pos)
{
  if (!trymove(pos, color, message, str, komaster, kom_pos))
    return 0;
  if (verbose)
    gfprintf(stderr, "%o%s %1m\n", 
	     color == BLACK ? "black" : "white", pos);
  gfprintf(to_gnugo_stream, "%o%s %1m\n", 
	   color == BLACK ? "black" : "white", pos);
  fflush(to_gnugo_stream);
  ASK_GNUGO(gnugo_line);
  return 1;
}

static void
oracle_popgo(void)
{
  popgo();
  TELL_GNUGO("undo\n");
  ASK_GNUGO(gnugo_line);
}





/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
