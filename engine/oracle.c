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


/* The functions in this file implement a mechanism whereby
 * GNU Go can fork a second gnugo process, called the oracle.
 * The two processes communicate by means of the GTP.
 * The functions oracle_trymove() and oracle_popgo() call
 * trymove and popgo in the primary gnugo processes but
 * actually play and undo the move in the oracle. This
 * the oracle can be queried for information which is
 * normally only available at the top level.
 */

#include "config.h"

#if ORACLE

#include "gnugo.h"
#include "liberty.h"
#include "patterns.h"

#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>

#define USE_POSIX 1

FILE *to_gnugo_stream, *from_gnugo_stream;
char gnugo_line[128];
int gnugo_line_length;

int pfd_a[2];
int pfd_b[2];

#define TELL_ORACLE(x, args...) do { \
              if (debug & DEBUG_ORACLE_STREAM) fprintf(stderr, x, ##args);  \
                if (fprintf(to_gnugo_stream, x, ##args) < 0) \
                   error("can't write command in to_gnugo_stream"); \
                fflush(to_gnugo_stream); \
              } while (0)

#define ASK_ORACLE    do { \
                        gnugo_line_length = 0;   \
                        while (gnugo_line_length != 1) { \
                          if (!fgets(gnugo_line, 128, from_gnugo_stream)) \
                             error("can't get response"); \
                          gnugo_line_length = strlen(gnugo_line); \
                          if (debug & DEBUG_ORACLE_STREAM) \
                            fprintf(stderr, gnugo_line); \
                          } \
                        } while (0)

#define MAX_ORACLE_MOVES 10

struct oracle_move_data {
  int pos;            /* move coordinate */
  int color;          /* color to play */
  int value;          /* value */
  int ab_value;       /* alpha-beta value */
  const char *reason; /* why this move */
};

static void oracle_callback(int anchor, int color, struct pattern *pattern,
			    int ll, void *data);
static void oracle_add_move(struct oracle_move_data *moves, 
			    int this_move, int this_value, 
			    const char *this_reason);
void do_consult_oracle(int color);
void error(const char *msg);
static int oracle_trymove(int pos, int color, const char *message, int str,
			  int komaster, int kom_pos);
static void oracle_popgo(void);
static void tell_oracle(const char *fmt, ...);
static void ask_oracle(void);
static int search_width(void);


/*** * *** * *** * *** * *** * *** * *** * *** * *** * *** * *** * ***\
 *                    Primary Oracle Functions                       *
\*** * *** * *** * *** * *** * *** * *** * *** * *** * *** * *** * ***/

/* Forks and attaches pipes to a new GNU Go process in gtp mode.
 * Loads the sgf file
 */

void
summon_oracle(void)
{
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
  oracle_exists = 1;
  /* Attach pipe a to to_gnugo_stream  */
  to_gnugo_stream = (FILE *) fdopen(pfd_a[1], "w");
  /* Attach pipe b to from_gnugo_stream */
  from_gnugo_stream = (FILE *) fdopen(pfd_b[0], "r");
}

/* load an sgf file */

void
oracle_loadsgf(char *infilename, char *untilstring)
{
  if (untilstring)
    TELL_ORACLE("loadsgf %s %s\n", infilename, untilstring);
  else
    TELL_ORACLE("loadsgf %s\n", infilename);
  ASK_ORACLE;
  fflush(to_gnugo_stream);
  gnugo_line_length = 0;
}

/* Tell the oracle to go away. */

void
dismiss_oracle(void)
{
  if (oracle_exists)
    TELL_ORACLE("quit\n");
  oracle_exists = 0;
}

/* complain and die! */

void
error(const char *msg)      
{
  fprintf(stderr, "oracle: %s\n", msg);
  abort();
}

/* Call trymove in the primary process, and have the oracle actually
 * play the move.
 */
static int 
oracle_trymove(int pos, int color, const char *message, int str,
	       int komaster, int kom_pos)
{
  if (!trymove(pos, color, message, str, komaster, kom_pos))
    return 0;
  if (debug & DEBUG_ORACLE_STREAM)
    gfprintf(stderr, "%o%s %1m\n", 
	     color == BLACK ? "black" : "white", pos);
  gfprintf(to_gnugo_stream, "%o%s %1m\n", 
	   color == BLACK ? "black" : "white", pos);
  fflush(to_gnugo_stream);
  ASK_ORACLE;
  return 1;
}

/* Undo the move. 
 */
static void
oracle_popgo(void)
{
  popgo();
  TELL_ORACLE("undo\n");
  ASK_ORACLE;
}

/* Play the move.
 */

int
oracle_play_move(int pos, int color)
{
  play_move(pos, color);

  if (debug & DEBUG_ORACLE_STREAM)
    gfprintf(stderr, "%o%s %1m\n", 
	     color == BLACK ? "black" : "white", pos);
  gfprintf(to_gnugo_stream, "%o%s %1m\n", 
	   color == BLACK ? "black" : "white", pos);
  fflush(to_gnugo_stream);
  ASK_ORACLE;
  return 1;
}

/* FIXME: Debugging needed. This variadic function doesn't work right if we
 * try to pass a const *char argument, like the infilename in
 * oracle_loadsgf. So for the time being we stick with the variadic macro
 * TELL_ORACLE.
 */
static void
tell_oracle(const char *fmt, ...)
{
  va_list ap;
  va_start(ap, fmt);
  if (debug & DEBUG_ORACLE_STREAM) fprintf(stderr, fmt, ap);
  if (fprintf(to_gnugo_stream, fmt, ap) < 0)
    error("can't write command in to_gnugo_stream");
  fflush(to_gnugo_stream);
  va_end(ap);
}

/* FIXME: Debugging needed. This variadic function seems a little more
 * reliable than the corresponding variadic macro ASK_ORACLE.  
 */

static void
ask_oracle(void)
{
  int line_length = 0;
  char line[128];

  while (line_length != 1) {
    if (!fgets(line, 128, from_gnugo_stream))
      error("can't get response");
    line_length = strlen(line);
    if (line_length > 1 
	&& (line[0] == '=' || line[0] == '?'))
      strncpy(gnugo_line, line, 128);
    if (debug & DEBUG_ORACLE_STREAM) {
      fprintf(stderr, line);
      fflush(stderr);
    }
  }
}


/* clear the oracle's board and set the boardsize */

void 
oracle_clear_board(int boardsize)
{
  TELL_ORACLE("boardsize %d\n", boardsize);
  ASK_ORACLE;
}

/*** * *** * *** * *** * *** * *** * *** * *** * *** * *** * *** * ***\
 *              Demonstration: a pattern matcher                     *
\*** * *** * *** * *** * *** * *** * *** * *** * *** * *** * *** * ***/

/* Call the pattern matcher */

void
consult_oracle(int color)
{
  do_consult_oracle(color);
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
oracle_callback(int anchor, int color, struct pattern *pattern,
		int ll, void *data)
{
  int this_move;
  struct oracle_move_data *moves = data;
  UNUSED(color);

  this_move = AFFINE_TRANSFORM(pattern->move_offset, ll, anchor);
  if (within_search_area(this_move))
    oracle_add_move(moves, this_move, pattern->value, pattern->name);
  else
    gprintf("outside the area\n");
}

/* Add a move to a list */

static void
oracle_add_move(struct oracle_move_data moves[MAX_ORACLE_MOVES],
		int this_move, int this_value, const char *this_reason)
{
  int k, l;
  
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

/*** * *** * *** * *** * *** * *** * *** * *** * *** * *** * *** * ***\
 *              Demonstration: metamachine                           *
\*** * *** * *** * *** * *** * *** * *** * *** * *** * *** * *** * ***/

#define FIRST_LEVEL_MOVES 3
#define SECOND_LEVEL_MOVES 2

static float
do_metamachine_genmove(int *i, int *j, int color, int width);

int
metamachine_genmove(int *i, int *j, int color)
{
  float value;
  int pos;

  if (limit_search) {
    TELL_ORACLE("limit_search 1\n");
    ASK_ORACLE;
    for (pos = BOARDMIN; pos < BOARDMAX; pos++)
      if (within_search_area(pos)) {
	if (debug & DEBUG_ORACLE_STREAM)
	  gfprintf(stderr, "%oset_search_limit %1m\n", pos);
	gfprintf(to_gnugo_stream, "%oset_search_limit %1m\n", pos);
	fflush(to_gnugo_stream);
	ASK_ORACLE;
      }	
  }
  count_variations = 1;
  value = do_metamachine_genmove(i, j, color, search_width());
  sgffile_enddump(outfilename);
  count_variations = 0;
  return value;
}

static float
do_metamachine_genmove(int *i, int *j, int color, int width)
{
  int k, moves_considered;
  float move_value[10];
  float best_score = 0.;
  int best_move = -1;
  char *token;
  int move_i[10], move_j[10];
  float score[10];
  char delimiters[] = " \t\r\n";
  char buf[100];

  if (color == BLACK) 
    TELL_ORACLE("top_moves_black\n");
  else
    TELL_ORACLE("top_moves_white\n");
  ask_oracle();
  token = strtok(gnugo_line, delimiters);
  for (k = 0; k < 10; k++) {
    move_i[k] = -1;
    move_j[k] = -1;
    move_value[k] = 0.0;
  }
  moves_considered = width;
  if (verbose)
    dump_stack();
  for (k = 0; k < moves_considered; k++) {
    token = strtok(NULL, delimiters);
    if (!token)
      break;
    string_to_location(board_size, token, move_i+k, move_j+k);
    token = strtok(NULL, delimiters);
    if (!token)
      break;
    sscanf(token, "%f", move_value+k);
    TRACE("move %d: %m valued %f\n", k,
	  move_i[k], move_j[k], move_value[k]);
  }
  /* if we left the loop early, k is the number of valid moves */
  moves_considered = k;
  if (moves_considered == 0) {
    *i = -1;
    *j = -1;
    return 0;
  }
  if (moves_considered == 1) {
    *i = move_i[0];
    *j = move_j[0];
    return 1;
  }
  for (k = 0; k < moves_considered; k++) {
    if (oracle_trymove(POS(move_i[k], move_j[k]), color, "", 0, 0, NO_MOVE)) {
      int new_width = search_width();

      if (new_width == 0) {
	TELL_ORACLE("experimental_score %s\n", 
		    color == BLACK ? "black" : "white");
	ask_oracle();
	sscanf(gnugo_line, "= %f", score+k);
      }
      else {
	score[k] =
	  do_metamachine_genmove(i, j, OTHER_COLOR(color), new_width);
      }
      if (verbose)
	dump_stack();
      TRACE("score: %f\n", color == WHITE ? score[k] : -score[k]);
      sprintf(buf, "value %.2f", color == WHITE ? score[k] : -score[k]);
      if (sgf_dumptree)
	sgftreeAddComment(sgf_dumptree, buf);
      oracle_popgo();
    }
    if (best_move == -1
	|| (color == WHITE && score[k] > best_score) 
	|| (color == BLACK && score[k] < best_score)) {
      best_move = k;
      best_score = score[k];
    }
  }
  TRACE("best: %f at %2m\n", best_score, move_i[best_move], move_j[best_move]);
  *i = move_i[best_move];
  *j = move_j[best_move];
  return score[best_move];
}

/* decide how wide to search */

static int
search_width(void)
{
  if (stackp == 0)
    return 3;
  else if (stackp == 1)
    return 2;
  else
    return 0;
}


#endif



/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
