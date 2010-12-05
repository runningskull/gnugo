/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * This is GNU Go, a Go program. Contact gnugo@gnu.org, or see       *
 * http://www.gnu.org/software/gnugo/ for more information.          *
 *                                                                   *
 * Copyright 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007,   *
 * 2008, 2009 and 2010 by the Free Software Foundation.              *
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

/* Convert joseki from sgf format to patterns.db format. */

#include "board.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define USAGE "\
Usage : joseki prefix filename\n\
"

/* Joseki move types. */
#define STANDARD  0
#define URGENT    1
#define MINOR     2
#define TRICK     3
#define ANTISUJI  4
#define TENUKI_OK 5


/* We don't want to play moves on edges of board which might have been
 * cropped, since there might appear an accidential capture.
 */
#define SAFE_ON_BOARD(i, j) ((i) >= 0 && (j) >= 0\
			     && (i) < MAX_BOARD - 1 && (j) < MAX_BOARD - 1)

static int boardsize;


/* Identify the type of joseki move.
 * FIXME: We might want the relax the requirement that this info comes
 *        as the very first character.
 */
static int
identify_move_type(char *text)
{
  if (!text)
    return STANDARD;
  
  switch ((int) *text) {
  case 'u':
  case 'U':
    return URGENT;
    break;
  case 'J':
  case 'S':
    return STANDARD;
    break;
  case 'j':
  case 's':
    return MINOR;
    break;
  case 'T':
    return TRICK;
    break;
  case 't':
    return TENUKI_OK;
    break;
  case '0':
  case 'a':
  case 'A':
    return ANTISUJI;
    break;
  }

  return STANDARD;
}

/* Copy the lines starting with a certain character to stdout. */
static void
write_selected_lines(char *text, char start_char)
{
  char *p;
  if (!text)
    return;
  while (1) {
    p = strchr(text, '\n');
    if (p)
      *p = 0;
    if (*text == start_char)
      printf("%s\n", text);
    if (p) {
      *p = '\n';
      text = p+1;
    }
    else
      break;
  }
}

/* Is there any line starting with a certain character? */
static int
selected_line_exists(char *text, char start_char)
{
  char *p;
  if (!text)
    return 0;
  while (1) {
    if (*text == start_char)
      return 1;
    p = strchr(text, '\n');
    if (p)
      text = p+1;
    else
      break;
  }
  return 0;
}

/* Write the main diagram or the constraint diagram. In the former
 * case, pass a NULL pointer for labels.
 */
static void
write_diagram(int movei, int movej, int color, int marki, int markj,
	      char labels[MAX_BOARD][MAX_BOARD])
{
  int i, j;
  
  for (i = -1; i <= marki; i++) {
    for (j = markj; j >= 0; j--) {
      if (i == -1)
	printf("-");
      else if (labels && labels[i][j])
	printf("%c", labels[i][j]);
      else if (i == movei && j == movej)
	printf("*");
      else if (BOARD(i, j) == color)
	printf("O");
      else if (BOARD(i, j) == OTHER_COLOR(color))
	printf("X");               
      else
	printf(".");
    }
    if (i == -1)
      printf("+\n");
    else
      printf("|\n");
  }
}

/* Write the colon line of the pattern. */
static void
write_colon_line(int move_type, char symmetry, char *text)
{
  char *p;

  /* Locate a possible colon line in the sgf file comment. */
  if (!text)
    p = NULL;
  else if (*text == ':')
    p = text + 1;
  else {
    p = strstr(text, "\n:");
    if (p)
      p += 2;
  }

  printf(":%c,sF", symmetry);
  switch (move_type) {
  case URGENT:
    printf("U");
    break;
  case STANDARD:
    printf("J");
    break;
  case MINOR:
    printf("j");
    break;
  case TRICK:
    printf("T");
    break;
  case TENUKI_OK:
    printf("t");
    break;
  case ANTISUJI:
    printf("N");
    break;
  }

  if (p) {
    /* A little trick to guess whether the supplied colon line in the
     * sgf file begins with a classification.
     */
    if (strchr(p, '(')
	&& (!strchr(p, ',') || strchr(p, ',') > strchr(p, '(')))
      printf(",");
    while (*p != 0 && *p != '\n')
      fputc(*(p++), stdout);
  }
  printf("\n");
}


/* Check if the board and labels are symmetric. */
static int
board_is_symmetric(int n, char labels[MAX_BOARD][MAX_BOARD])
{
  int i;
  int j;

  for (i = 0; i <= n; i++) {
    for (j = 0; j < i; j++) {
      if (BOARD(i, j) != BOARD(j, i)
	  || (labels && labels[i][j] != labels[j][i]))
	return 0;
    }
  }

  return 1;
}

/* Write a pattern to stdout. */
static void
make_pattern(int movei, int movej, int color,
	     int marki, int markj, int multiple_marks,
	     char labels[MAX_BOARD][MAX_BOARD], char *text,
	     const char *prefix)
{
  static int pattern_number = 0;
  int move_type;
  char symmetry = '8';

  pattern_number++;
  move_type = identify_move_type(text);

  printf("Pattern %s%d\n", prefix, pattern_number);

  /* Write comments. */
  write_selected_lines(text, '#');
  printf("\n");

  /* Write the main diagram. */
  write_diagram(movei, movej, color, marki, markj, NULL);
  printf("\n");

  /* Write the colon line. */
  if (movei == movej && marki == markj && board_is_symmetric(marki, labels))
    symmetry = '/';
  write_colon_line(move_type, symmetry, text);
  printf("\n");

  /* Write the constraint diagram if there are any labels, a
   * constraint line, or an action line.
   */
  if (labels
      || selected_line_exists(text, ';')
      || selected_line_exists(text, '>')) {
    write_diagram(movei, movej, color, marki, markj, labels);

    printf("\n");

    /* Write constraint and action lines. */
    write_selected_lines(text, ';');
    write_selected_lines(text, '>');
    printf("\n");
  }

  printf("\n");

  /* Basic sanity checking. We do this at the end to simplify debugging. */
  if (multiple_marks)
    fprintf(stderr, "Warning: Multiple square marks in pattern %s%d\n",
	    prefix, pattern_number);

  if (is_suicide(POS(movei, movej), color)) {
    fprintf(stderr, "Error: Illegal move in pattern %s%d\n",
	    prefix, pattern_number);
    exit(EXIT_FAILURE);
  }  
}


/* Analyze the node properties in order to make a pattern. Then make
 * recursive calls for child node and siblings.
 */
static void
analyze_node(SGFNode *node, const char *prefix)
{
  SGFProperty *prop;
  int i, j;
  char labels[MAX_BOARD][MAX_BOARD];
  int label_found = 0;
  int movei = -1;
  int movej = -1;
  int color = EMPTY;
  int marki = -1;
  int markj = -1;
  int multiple_marks = 0;
  char *comment = NULL;

  /* Clear the labels array. */
  memset(labels, 0, MAX_BOARD * MAX_BOARD);
  
  /* Check the node properties for a move, a square mark, labels, and
   * a comment.
   */
  for (prop = node->props; prop; prop = prop->next) {
    switch (prop->name) {
    case SGFSQ: /* Square */
    case SGFMA: /* Mark */
      if (marki != -1)
	multiple_marks = 1;
      else {
	get_moveXY(prop, &marki, &markj, boardsize);
	markj = boardsize - 1 - markj;
      }
      break;
      
    case SGFW: /* White move */
      color = WHITE;
      get_moveXY(prop, &movei, &movej, boardsize);
      movej = boardsize - 1 - movej;
      break;
      
    case SGFB: /* Black move */
      color = BLACK;
      get_moveXY(prop, &movei, &movej, boardsize);
      movej = boardsize - 1 - movej;
      break;

    case SGFLB: /* Label, with value like "mh:A" */
      get_moveXY(prop, &i, &j, boardsize);
      j = boardsize - 1 - j;
      gg_assert(prop->value[2] == ':');
      if (ON_BOARD2(i, j)) {
	labels[i][j] = prop->value[3];
	label_found = 1;
      }
      break;

    case SGFC: /* Comment */
      comment = prop->value;
      break;
    }
  }

  /* If we have a move and a square mark, produce a pattern. */
  if (SAFE_ON_BOARD(movei, movej) && ON_BOARD2(marki, markj))
    make_pattern(movei, movej, color, marki, markj, multiple_marks,
		 (label_found ? labels : NULL), comment, prefix);

  /* Traverse child, if any. */
  if (node->child) {
    if (SAFE_ON_BOARD(movei, movej))
      tryko(POS(movei, movej), color, NULL);
    analyze_node(node->child, prefix);
    if (SAFE_ON_BOARD(movei, movej))
      popgo();
  }

  /* Traverse sibling, if any. */
  if (node->next)
    analyze_node(node->next, prefix);
}


int
main(int argc, char *argv[])
{
  const char *filename;
  const char *prefix;
  SGFNode *sgf;

  /* Check number of arguments. */
  if (argc != 3) {
    fprintf(stderr, USAGE);
    exit(EXIT_FAILURE);
  }

  prefix = argv[1];
  filename = argv[2];

  /* Read the sgf file into a tree in memory. */
  sgf = readsgffile(filename);
  if (!sgf) {
    fprintf(stderr, "%s: Couldn't open sgf file %s.\n", argv[0], filename);
    exit(EXIT_FAILURE);
  }

#define PREAMBLE "\
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #\n\
# This is GNU Go, a Go program. Contact gnugo@gnu.org, or see       #\n\
# http://www.gnu.org/software/gnugo/ for more information.          #\n\
#                                                                   #\n\
# Copyright 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007,   #\n\
# 2008 and 2009 by the Free Software Foundation.                    #\n\
#                                                                   #\n\
# This program is free software; you can redistribute it and/or     #\n\
# modify it under the terms of the GNU General Public License as    #\n\
# published by the Free Software Foundation - version 3 or          #\n\
# (at your option) any later version.                               #\n\
#                                                                   #\n\
# This program is distributed in the hope that it will be useful,   #\n\
# but WITHOUT ANY WARRANTY; without even the implied warranty of    #\n\
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     #\n\
# GNU General Public License in file COPYING for more details.      #\n\
#                                                                   #\n\
# You should have received a copy of the GNU General Public         #\n\
# License along with this program; if not, write to the Free        #\n\
# Software Foundation, Inc., 51 Franklin Street, Fifth Floor,       #\n\
# Boston, MA 02111, USA.                                            #\n\
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #\n\
# This file is automatically generated by joseki. Do not edit       #\n\
# it directly. Instead, edit the corresponding sgf file.            #\n\
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #\n\
\n\n\
"

  printf(PREAMBLE);
  printf("attribute_map general\n\n");

  /* Call the engine to setup and clear the board. */
  board_size = MAX_BOARD;
  clear_board();
  
  /* Determine board size of the file. */
  if (!sgfGetIntProperty(sgf, "SZ", &boardsize)) {
    fprintf(stderr, "joseki: error: can't determine file board size\n");
    return 1;
  }
  
  /* Walk through the tree and make patterns. */
  analyze_node(sgf, prefix);
    
  return 0;
}

      
/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
