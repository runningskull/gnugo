/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * This is GNU GO, a Go program. Contact gnugo@gnu.org, or see   *
 * http://www.gnu.org/software/gnugo/ for more information.      *
 *                                                               *
 * Copyright 1999, 2000, 2001 by the Free Software Foundation.   *
 *                                                               *
 * This program is free software; you can redistribute it and/or *
 * modify it under the terms of the GNU General Public License   *
 * as published by the Free Software Foundation - version 2.     *
 *                                                               *
 * This program is distributed in the hope that it will be       *
 * useful, but WITHOUT ANY WARRANTY; without even the implied    *
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR       *
 * PURPOSE.  See the GNU General Public License in file COPYING  *
 * for more details.                                             *
 *                                                               *
 * You should have received a copy of the GNU General Public     *
 * License along with this program; if not, write to the Free    *
 * Software Foundation, Inc., 59 Temple Place - Suite 330,       *
 * Boston, MA 02111, USA.                                        *
\* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */


/*
 * sgffile.c
 *
 * This file contains functions that create an SGF file on the fly.
 * There can be only one file open simultaneously.
 *
 * See sgf/sgftree.c for more general handling of SGF trees and file I/O.
 *
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <signal.h>
#include <string.h>

#include "liberty.h"
#include "sgftree.h"

/* The SGF file while a game is played. */
static FILE *sgfout = NULL;

static int  sgffile_flush_file(void);


/* ================================================================ */


/*
 * Handling of the SGF file itself (open, close, etc).
 */


/*
 * Open the sgf file for output.  The filename "-" means stdout.
 */

int 
sgffile_open_file(const char *sgf_filename)
{
  /* If the file already was open, close it and assume we want to
   * start writing from scratch.
   */
  if (sgfout)
    sgffile_close_file();
  
  if (strcmp(sgf_filename, "-") == 0)
    sgfout = stdout;
  else
    sgfout = fopen(sgf_filename, "w");
  
  if (!sgfout)
    return 0;
  else
    return 1;
}


/*
 * Flush buffered output to the sgf file.
 */

static int 
sgffile_flush_file()
{
  if (!sgfout) 
    return 0;
  
  fflush(sgfout);
  return 1;
}


/*
 * Close the sgf file for output.
 */

int 
sgffile_close_file()
{
  if (!sgfout)
    return 0;
  
  fprintf(sgfout, ")\n");
  /* Don't close sgfout if it happens to be stdout. */
  if (sgfout != stdout)
    fclose(sgfout);
  sgfout = NULL;
  
  return 1;
}


/* ---------------------------------------------------------------- */


/*
 * Basic output functions.
 */


/*
 * Write a line to the sgf file.
 */

int 
sgffile_write_line(const char * line, ...)
{
  va_list ap;

  if (!sgfout)
    return 0;

  va_start(ap, line);
  vfprintf(sgfout, line, ap);
  va_end(ap);

  return sgffile_flush_file();
}


/*
 * Write a comment to the SGF file.
 */

void
sgffile_write_comment(const char *comment)
{
  if (!sgfout)
    return ;
  
  fprintf(sgfout, "C[%s]", comment);
}


/*
 * Add a stone to the SGF file.
 */

void
sgffile_put_stone(int i, int j, int color)
{
  if (sgfout)
    fprintf(sgfout, "A%c[%c%c]", color==WHITE ? 'W' : 'B',
	    'a' + j, 'a' + i);
}


/* 
 * Write header information to the sgf file.
 */

int 
sgffile_write_gameinfo(Gameinfo *ginfo, const char *gametype)
{
  char outbuf[200];

  if (!sgfout) 
    return 0;

  fprintf(sgfout, "(;GM[1]FF[4]");
  fprintf(sgfout, "RU[%s]", "Japanese");
  fprintf(sgfout, "SZ[%d]", ginfo->position.boardsize);
  fprintf(sgfout, "\n");
  
  sprintf(outbuf, "GNU Go %s (level %d) %s", VERSION, level, gametype);
  fprintf(sgfout, "PW[%s]PB[%s]", 
	  (ginfo->computer_player == WHITE 
	   || ginfo->computer_player == GRAY) ? outbuf : "Unknown",
	  (ginfo->computer_player == BLACK 
	   || ginfo->computer_player == GRAY) ? outbuf : "Unknown");
  fprintf(sgfout, "HA[%d]", ginfo->handicap);
  fprintf(sgfout, "KM[%.1f]", ginfo->position.komi);
  fprintf(sgfout, "GN[GNU Go %s %s ", VERSION, gametype);
  fprintf(sgfout, "Random Seed %d", ginfo->seed);
  fprintf(sgfout, "] ");
  fprintf(sgfout, "\n");
  
  return sgffile_flush_file();
}


/* ---------------------------------------------------------------- */


/*
 * The functions below here accesses internal gnugo data structures.
 */


/*
 * A move has been made; Write out the move and the potential moves
 * that were also considered.
 */

void 
sgffile_move_made(int i, int j, int color, int value)
{
  int m, n;
  int done_label = 0;
  
  if (!sgfout)
    return;

  for (m = 0; m < board_size; ++m) {
    for (n = 0; n < board_size; ++n) {
      if (potential_moves[m][n] > 0.0) {
	if (!done_label) {
	  fprintf(sgfout, "\nLB");
	  done_label = 1;
	}
	if (potential_moves[m][n] < 1.0)
	  fprintf(sgfout, "[%c%c:<1]", 'a'+n, 'a'+m);
	else
	  fprintf(sgfout, "[%c%c:%d]", 'a'+n, 'a'+m,
		  (int) potential_moves[m][n]);
      }
    }
  }

  if (value)
    fprintf(sgfout, "\nC[Value of move: %d]", value);

  /* If it is a pass move */
  if (is_pass(POS(i, j))) {
    if (board_size > 19)
      fprintf(sgfout, "\n;%c[]\n", color == WHITE ? 'W' : 'B');
    else
      fprintf(sgfout, "\n;%c[tt]\n", color == WHITE ? 'W' : 'B');
  }
  else
    fprintf(sgfout, "\n;%c[%c%c]\n", color == WHITE ? 'W' : 'B',
	    'a' + j, 'a' + i);

  fflush(sgfout);  /* in case cgoban terminates us without notice */
}  


/*
 * Mark dead and critical dragons in the sgf file.
 */

void 
sgffile_dragon_status(int i, int j, int status)
{
  if (sgfout) {
    switch (status) {
      case DEAD:
	fprintf(sgfout, "LB[%c%c:X]\n", 'a'+j, 'a'+i);
	break;
      case CRITICAL:
	fprintf(sgfout, "LB[%c%c:!]\n", 'a'+j, 'a'+i);
	break;
    }
  }
}


/* ---------------------------------------------------------------- */

/*
 * sgffile_printboard writes the current board position to the output file.
 * The parameter next, tells whose turn it is to move.
 */

void
sgffile_printboard(int next) 
{
  int i,j;
  int start = 0;

  if (!sgfout)
    return;

  /* Write the white stones to the file. */
  for (i = 0; i < board_size; i++) {
    for (j = 0; j < board_size; j++) {
      if (BOARD(i, j) == WHITE) {
	if (!start) {
	  fprintf(sgfout, "AW");
	  start = 1;
	}
	fprintf(sgfout, "[%c%c]", j+'a', i+'a');
      }
    }
  }
  fprintf(sgfout, "\n");

  /* Write the black stones to the file. */
  start = 0;
  for (i = 0; i < board_size; i++) {
    for (j = 0; j < board_size; j++) {
      if (BOARD(i, j) == BLACK) {
	if (!start) {
	  fprintf(sgfout, "AB");
	  start = 1;
	}
	fprintf(sgfout, "[%c%c]", j+'a', i+'a');
      }
    }
  }
  fprintf(sgfout, "\n");

  /* If no game is going on, then return. */
  if (next != WHITE && next != BLACK) 
    return;

  /* Write whose turn it is to move. */
  if (next == WHITE) 
    fprintf(sgfout, "PL[W]\n"); 
  else if (next == BLACK)
    fprintf(sgfout, "PL[B]\n"); 

  /* Mark the intersections where it is illegal to move. */
  start = 0;
  for (i = 0; i < board_size; i++) {
    for (j = 0; j < board_size; j++) {
      if (BOARD(i, j) == EMPTY && !is_legal(POS(i, j), next)) {
	if (!start) {
	  fprintf(sgfout, "IL");
	  start = 1;
	}
	fprintf(sgfout, "[%c%c]", j+'a', i+'a');
      }
    }
  }
  fprintf(sgfout, "\n");
}


/* ================================================================ 
 * Dumping of information about a position into an sgftree.
 * Used by sgffile_decideposition, etc.
 * ================================================================ */


static void
sgftree_printboard(SGFTree *tree);

/*
 * begin_sgfdump begins outputting all moves considered by
 * trymove and tryko to an sgf file.
 */

void
begin_sgftreedump(SGFTree *tree)
{
  SGFNode *node;
  gg_assert(sgf_dumptree == NULL);
  sgf_dumptree = tree;
  sgftree_clear(sgf_dumptree);
  node = sgftreeCreateHeaderNode(sgf_dumptree, board_size, 0.0);
  sgftreeSetLastNode(sgf_dumptree, node);
  sgftree_printboard(sgf_dumptree);
}


/*
 * end_sgfdump ends the dump and closes the sgf file.
 */

void 
end_sgftreedump(const char *filename)
{
  writesgf(sgf_dumptree->root, filename);
  sgf_dumptree = NULL;
}


/*
 * sgftree_printboard adds the current board position to the tree.
 */

static void
sgftree_printboard(SGFTree *tree)
{
  int i, j;
  SGFNode *node;
  
  gg_assert(tree);
  node = sgfAddChild(tree->lastnode);
  
  /* Write the white stones to the file. */
  for (i = 0; i < board_size; i++) {
    for (j = 0; j < board_size; j++) {
      if (BOARD(i, j) == WHITE)
	sgfAddStone(node, WHITE, i, j);
    }
  }

  /* Write the black stones to the file. */
  for (i = 0; i < board_size; i++) {
    for (j = 0; j < board_size; j++) {
      if (BOARD(i, j) == BLACK)
	sgfAddStone(node, BLACK, i, j);
    }
  }

  sgftreeSetLastNode(tree, node);
}


void
sgffile_recordboard(SGFNode *node)
{
  int i, j;

  for (i = 0; i < board_size; i++) {
    for (j = 0; j < board_size; j++) {
      if (BOARD(i, j) == BLACK) {
	sgffile_put_stone(i, j, BLACK);
	if (node)
	  sgfAddStone(node, BLACK, i, j);
      }
    }
  }
}


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
