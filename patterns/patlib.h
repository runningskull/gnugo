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

/* This file describes the patterns as they appear in the
 * various <name>.db files.
 */


#include <stdio.h>

#include "patterns.h"


#define NORMAL_PATTERNS     0
#define CONNECTION_PATTERNS 1
#define ESCAPE_PATTERNS     2



typedef struct lblval {
  int  label;			/* 0 - (MAXLABEL-1) */
  int  x;
  int  y;
} Lblval;


typedef struct textpattern_t {
  char    *name;		/* The name of the pattern */
  char    *comment;		/* The comment just below the name */
  int      height;
  int      width;
  
  int      num_elements;	/* Number of elements in the pattern */
  char    *elements;		/* The elements */
  int      edge_constraints;	/* NORTH_EDGE | WEST_EDGE */
                                /* | SOUTH_EDGE | EAST_EDGE */
  char    *explanation;		/* Optional explanation */
  char    *comment2;

  char    *entry_line;		/* The line describing the pattern */
  char    *comment3;

  int      num_constraint_elements; /* Number of labels */
  char    *constraint_elements;		/* The labels */
  char    *comment4;

  int      constraint_lines_len;
  char    *constraint_lines;
  char    *comment5;

  int      action_lines_len;
  char    *action_lines;
  char    *comment6;
} Textpattern;

extern void textpattern_clear(Textpattern *tp);
extern void textpattern_print(FILE *outfile, Textpattern *tp);
extern void parse_init(char *filename, FILE *file);
extern int  parse_whitespace(int to_end_of_line_only);
extern char *get_whitespace(void);
extern int  textpattern_parse(FILE *infile,
			      int parm_pattern_type, int parm_anchor_both, 
			      int parm_fullboard, int parm_verbose,
			      Textpattern *tp);
extern void textpattern_transform(Textpattern *tp, int transform);


extern const int transformations[8][2][2];



/*
 * Local Variables:
 * tab-width: 8

 * c-basic-offset: 2
 * End:
 */
