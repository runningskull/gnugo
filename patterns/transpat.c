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

#include "patlib.h"
#include <stdio.h>
#include <fnmatch.h>

/* 
 * The purpose of this program is to transform the patterns
 * in a database into a canonical form.
 *
 * Usage is:
 * 
 * transpat <command> <patnames> [patfile]
 * 
 * where
 * 
 *   command is one of "flipleft" or "normalize"
 *   patnames is a standard glob pattern like "EE*"
 *   patfile is an optional file name.  If no file name is given,
 *     transpat reads from stdin.
 * 
 * If "flipleft" is given as a command, the appropriate patterns are
 * flipped left-right, and if "normalize" is given the appropriate
 * patterns are made lower-edge or lower-left-edge patterns if there are
 * any edges at all.
 * 
 * The transformations array have misleading comments.  This is what the
 * transformations really do:
 *
 * 0: identity transformation
 * 1: rotation 270 degrees
 * 2: rotation 180 degrees
 * 3: rotation  90 degrees
 * 4: rotation  90 degrees and flip left-right
 * 5: flip up-down
 * 6: rotation  90 degrees and flip up-down
 * 7: flip left-right
 *
 * The reason for the mismatch is that the comments apply to an ordinary
 * coordinate system, but the coordinate system here have the axises
 * switched. 
 */


static void flippat(Textpattern *tp);
static int  match_name(Textpattern *tp, char *patnames);


/*
 * Transform a pattern so that:
 *  - If there is 1 edge, make it the lower edge
 *  - If there is a corner, make it the lower left corner.
 *  - if there are 3 edges, make the middle edge the lower one.
 *  Otherwise don't do anything.

 */

static void
flippat(Textpattern *tp)
{
  int  transno;

  switch (tp->edge_constraints) {
  case 0:     return;		  /* Nothing to do here */

  case NORTH_EDGE: transno = 5; break; /* Flip up-down */
  case SOUTH_EDGE: return;		  /* Nothing to do here */
  case EAST_EDGE:  transno = 1; break; /* Rotate 270 degrees */
  case WEST_EDGE:  transno = 3; break; /* Rotate 90  degrees */

  case NORTH_EDGE | SOUTH_EDGE: return;
  case NORTH_EDGE | WEST_EDGE:  transno = 3; break;
  case NORTH_EDGE | EAST_EDGE:  transno = 2; break;
  case WEST_EDGE  | EAST_EDGE:  return;
  case WEST_EDGE  | SOUTH_EDGE: return;
  case EAST_EDGE  | SOUTH_EDGE: transno = 1; break;

  case NORTH_EDGE | WEST_EDGE | EAST_EDGE:  transno = 2; break;
  case NORTH_EDGE | WEST_EDGE | SOUTH_EDGE: transno = 3; break;
  case NORTH_EDGE | EAST_EDGE | SOUTH_EDGE: transno = 1; break;
  case WEST_EDGE  | EAST_EDGE | SOUTH_EDGE: return;

  default: return;
  }

  textpattern_transform(tp, transno);
}


/*
 * Return 1 if the textpattern has a name that matches the pattern.
 * Otherwise return 0.
 */

static int
match_name(Textpattern *tp, char *patnames)
{
  return fnmatch(patnames, tp->name, FNM_NOESCAPE) == 0;
}


#define FLIPLEFT   1
#define NORMALIZE  2


int
main(int argc, char *argv[])
{
  FILE *infile;
  Textpattern tp;
  int val;
  char *patnames;
  int  command = 0;

  if (argc < 2 || argc > 4) {
    fprintf(stderr, 
	    "usage: %s command patnames [patfilename]\n\
where\n\
    command     is one of 'flipleft' or 'normalize'\n\
    patnames    is a standard glob pattern (like EE*)\n\
    patfilename is an optional filename\n", argv[0]);
    exit(1);
  }

  /* Get the command */
  if (strcmp(argv[1], "flipleft") == 0)
    command = FLIPLEFT;
  else if (strcmp(argv[1], "normalize") == 0)
    command = NORMALIZE;
  
  /* Get the patterns we should work on. */
  patnames = argv[2];

  /* Try to open infile. */
  if (argc == 4){
    if ((infile = fopen(argv[3], "r")) == 0) {
      perror(argv[3]);
      exit(1);
    }
  }
  else 
    infile = stdin;

  parse_init(argv[1], infile);

  /* Read and write initial whitespace and comments. */
  if (parse_whitespace(0))
    fprintf(stdout, get_whitespace());

  /* Parse all the patterns, transform them appropriately and write them
     out to file. */
  do {
    val = textpattern_parse(infile, NORMAL_PATTERNS, 1, 0, 1, &tp);
    if (val) {
      if (match_name(&tp, patnames)) {
	switch (command) {
	case FLIPLEFT:   textpattern_transform(&tp, 7);  break;
	case NORMALIZE:  flippat(&tp);                   break;
	}
      }
      textpattern_print(stdout, &tp);
    }
    else {
      printf("error\n");
      exit(1);
    }
  } while (val != 0 && !feof(infile));
 
  exit(0);
}
