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

/* patlib.c
 *
 * A collection of routines for handling patterns in text form.
 */

/* See also patterns.h, and the *.db files. */


#include <ctype.h>
#include <string.h>
#include <assert.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>

#include "liberty.h"
#include "gg_utils.h"
#include "patterns.h"
#include "patlib.h"


/* ================================================================ */
/*                    Miscellaneous declarations                    */
/* ================================================================ */


/* valid characters that can appear in a pattern
 * position in string is att value to store
 */
const char VALID_PATTERN_CHARS[]     = ".XOxo,a!*?Q";
const char VALID_EDGE_CHARS[]        = "+-|";
const char VALID_CONSTRAINT_LABELS[] = "abcdefghijklmnpqrstuvwyzABCDEFGHIJKLMNQRSTUVWYZ";


/* The offsets into the list are the ATT_* defined in patterns.h.
 * The following defns are for internal use only, and are not
 * written out to the compiled pattern database.
 */

#define ATT_star  8
#define ATT_wild  9
#define ATT_Q    10



/* ================================================================ */
/*                     The textpattern structure                    */
/* ================================================================ */


void
textpattern_clear(Textpattern *tp)
{
  tp->name                    = NULL;
  tp->comment                 = NULL;
  tp->height                  = 0;
  tp->width                   = 0;
  tp->num_elements            = 0;
  tp->elements                = NULL;
  tp->edge_constraints        = 0;
  tp->explanation             = NULL;
  tp->comment2                = NULL;

  tp->entry_line              = NULL;
  tp->comment3                = NULL;
  tp->num_constraint_elements = 0;
  tp->constraint_elements     = NULL;
  tp->comment4                = NULL;
  tp->constraint_lines_len    = 0;
  tp->constraint_lines        = NULL;
  tp->comment5                = NULL;
  tp->action_lines_len        = 0;
  tp->action_lines            = NULL;
  tp->comment6                = NULL;
}


static void
textpattern_ns_edge_print(FILE *outfile, Textpattern *tp)
{
  int  i;

  if (tp->edge_constraints & WEST_EDGE)
    fputc('+', outfile);
  for (i = 0; i < tp->width; ++i)
    fputc('-', outfile);
  if (tp->edge_constraints & EAST_EDGE)
    fputc('+', outfile);
  fputc('\n', outfile);
}


void
textpattern_print(FILE *outfile, Textpattern *tp)
{
  int i, j;

  /* Print the pattern line */
  fprintf(outfile, "Pattern %s\n", tp->name ? tp->name : "<NONAME>");

  /* Print comment or similar. */
  if (tp->comment)
    fprintf(outfile, tp->comment);

  /* Print the pattern diagram */
  if (tp->edge_constraints & NORTH_EDGE)
    textpattern_ns_edge_print(outfile, tp);

  for (i = 0; i < tp->height; ++i) {
    if (tp->edge_constraints & WEST_EDGE)
      fputc('|', outfile);
    for (j = 0; j < tp->width; ++j)
      fputc(tp->elements[i*tp->width+j], outfile);
    if (tp->edge_constraints & EAST_EDGE)
      fputc('|', outfile);

    if (i == 0 && tp->explanation)
      fprintf(outfile, "%s", tp->explanation);

    fputc('\n', outfile);
  }
  if (tp->edge_constraints & SOUTH_EDGE) 
    textpattern_ns_edge_print(outfile, tp);
  if (tp->comment2)
    fprintf(outfile, tp->comment2);

  fprintf(outfile, "%s", tp->entry_line);
  if (tp->comment3)
    fprintf(outfile, tp->comment3);

  /* Print the constraint diagram */
  if (tp->num_constraint_elements > 0) {
    if (tp->edge_constraints & NORTH_EDGE)
      textpattern_ns_edge_print(outfile, tp);

    for (i = 0; i < tp->height; ++i) {
      if (tp->edge_constraints & WEST_EDGE)
	fputc('|', outfile);
      for (j = 0; j < tp->width; ++j)
	fputc(tp->constraint_elements[i*tp->width+j], outfile);
      if (tp->edge_constraints & EAST_EDGE)
	fputc('|', outfile);
      fputc('\n', outfile);
    }
    if (tp->edge_constraints & SOUTH_EDGE) 
      textpattern_ns_edge_print(outfile, tp);

    if (tp->comment4)
      fprintf(outfile, tp->comment4);
  }

  if (tp->constraint_lines_len > 0)
    fprintf(outfile, "%s", tp->constraint_lines);
  if (tp->comment5)
    fprintf(outfile, tp->comment5);

  if (tp->action_lines_len > 0)
    fprintf(outfile, "%s", tp->action_lines);
  if (tp->comment6)
    fprintf(outfile, tp->comment6);
}


/* ================================================================ */
/*                      Parsing of textpatterns                     */
/* ================================================================ */


#define MAXTOKEN  20
#define MAXBUFFER 50000


char *infilename;
int   current_line = 1;
FILE *infile;
int   next_char;
int   inttoken;
char  strbuffer[MAXBUFFER];
char  whitespacebuffer[MAXBUFFER];
int   strbuffer_index;


static void parse_error(const char *format, ...);
static void parse_warning(const char *format, ...);
static int get_next_char(void);

static void parse_away_whitespace(void);
static int parse_identifier(void);
static int parse_to_end_of_line(int cont);

static int parse_pattern_line(Textpattern *tp);
static int parse_pattern_diagram(Textpattern *tp);
static int parse_entry_line(Textpattern *tp);
static int parse_constraint_diagram(Textpattern *tp);
static int parse_constraint_lines(Textpattern *tp);
static int parse_action_lines(Textpattern *tp);


void
parse_init(char *filename, FILE *file)
{
  infilename = strdup(filename);
  infile     = file;

  get_next_char();
}


static void
parse_error(const char *format, ...)
{
  va_list ap;
  va_start(ap, format);

  fprintf(stderr, "%s:%d: error: ", infilename, current_line);
  vfprintf(stderr, format, ap);
  fputc('\n', stderr);

  va_end(ap);
}


static void
parse_warning(const char *format, ...)
{
  va_list ap;
  va_start(ap, format);

  fprintf(stderr, "%s:%d: warning: ", infilename, current_line);
  vfprintf(stderr, format, ap);
  fputc('\n', stderr);

  va_end(ap);
}


static int
get_next_char()
{
  next_char = getc(infile);
  if (next_char == '\n')
    current_line++;

  return next_char;
}


int
parse_whitespace(int to_end_of_line_only)
{
  char *p = whitespacebuffer;
  int  is_comment;

  if (!isspace(next_char) 
      && next_char != '#')
    return 0;

  is_comment = 0;
  while ((isspace(next_char) 
	  || is_comment
	  || next_char == '#')
	 && next_char != EOF)
  {
    /* Switch on of off comments */
    if (is_comment) {
      if (next_char == '\n')
	is_comment = 0;
    }
    else
      if (next_char == '#')
	is_comment = 1;

    if (to_end_of_line_only && next_char == '\n') {
      get_next_char();
      break;
    }

    if (p-whitespacebuffer < MAXBUFFER-1)
      *p++ = next_char;
    else
      parse_warning("Whitespace too big (truncated)");

    get_next_char();
  }
  *p++ = '\0';

  if (p == whitespacebuffer)
    return 0;

  return 1;
}


char *
get_whitespace(void)
{
  return whitespacebuffer;
}


static void
parse_away_whitespace(void)
{
  while (isspace(next_char) && next_char != '\n')
    get_next_char();
}


#if 0

static int
parse_int(void)
{
  int  val = 0;

  if (!isdigit(next_char))
    return 0;

  val = next_char - '0';
  while (isdigit(get_next_char()))
    val = val * 10 + next_char;
  inttoken = val;

  return 1;
}

#endif

static int
parse_identifier()
{
  if (!isalpha(next_char) && next_char != '_')
    return 0;

  strbuffer_index = 0;
  do {
    strbuffer[strbuffer_index++] = next_char;
    get_next_char();
  } while (strbuffer_index < MAXTOKEN
	   && (isalpha(next_char)
	       || isdigit(next_char)
	       || next_char == '_'));
  strbuffer[strbuffer_index] = '\0';

  if (strbuffer_index == MAXTOKEN) {
    parse_error("Too long identifier: %s", strbuffer);
    return 0;
  }

  return 1;
}


static int
parse_to_end_of_line(int cont)
{
  if (!cont)
    strbuffer_index = 0;

  do {
    strbuffer[strbuffer_index++] = next_char;
    get_next_char();
  } while (strbuffer_index < MAXBUFFER-1
	   && next_char != '\n');
  strbuffer[strbuffer_index++] = '\n';
  strbuffer[strbuffer_index] = '\0';

  if (strbuffer_index == MAXBUFFER) {
    parse_error("Too long line");
    return 0;
  }
  else
    get_next_char();

  return 1;
}


/* ---------------------------------------------------------------- */


static int
parse_pattern_line(Textpattern *tp)
{
  if (!parse_identifier()) 
    return 0;

  if (strcmp(strbuffer, "Pattern")) {
    parse_error("Pattern expected");
    return 0;
  }

  parse_away_whitespace();
  if (!parse_identifier()) 
    return 0;
  tp->name = strdup(strbuffer);
  parse_whitespace(1);

  return 1;
}


static int
parse_north_south_constraint(Textpattern *tp)
{
  int  width;

  width = 0;
  while (next_char == '-') {
    width++;
    get_next_char();
  }

  if (width == 0) {
    parse_error("Error in north/south constraint");
    return 0;
  }

  if (tp->width >0) {
    /* This has to be the south border */
    if (width != tp->width
	|| (next_char == '+' && (tp->edge_constraints & EAST_EDGE) == 0)) {
      parse_error("Error in south constraint");
      return 0;
    }
  }
  else {
    /* North constraint -> set width. */
    tp->width = width;
    if (next_char == '+')
      tp->edge_constraints |= EAST_EDGE;
  }

  /* Finish the line */
  if (next_char == '+')
    get_next_char();
  parse_whitespace(1);

  return 1;
}


static int
parse_pattern_diagram(Textpattern *tp)
{
  int  width;

  /* See if there is a north border constraint. */
  if (next_char == '+' || next_char == '-') {
    tp->edge_constraints |= NORTH_EDGE;
    if (next_char == '+') {
      tp->edge_constraints |= WEST_EDGE;
      get_next_char();
    }
    if (parse_north_south_constraint(tp) == 0)
      return 0;
  }

  /* Read each line */
  strbuffer_index = 0;
  while (next_char == '|'
	 || strchr(VALID_PATTERN_CHARS, next_char)) {

    /* Handle WEST_EDGE constraint. */
    if (next_char == '|') {
      tp->edge_constraints |= WEST_EDGE;
      get_next_char();
    }
    else {
      if (tp->edge_constraints & WEST_EDGE) {
	parse_error("Missing west constraint");
	return 0;
      }
    }

    /* Handle the pattern itself. */
    width = 0;
    while (strchr(VALID_PATTERN_CHARS, next_char)) {
      strbuffer[strbuffer_index++] = next_char;
      width++;
      get_next_char();
    }

    /* In case this happens to be the first row */
    if (tp->height == 0) {
      tp->width = width;
      if (next_char == '|')
	tp->edge_constraints |= EAST_EDGE;
    }

    if (tp->width != width) {
      parse_error("Error in pattern diagram");
      return 0;
    }
    tp->height++;

    /* Handle EAST_EDGE constraint. */
    if (next_char == '|') {
      if (tp->edge_constraints & EAST_EDGE)
	get_next_char();
      else {
	parse_error("Unexpected east constraint");
	return 0;
      }
    }
    else {
      if (tp->edge_constraints & EAST_EDGE) {
	parse_error("Missing east constraint");
	return 0;
      }
    }

    /* This strange sequence is because we don't want to destroy strbuffer. */
    {
      /* FIXME: Only one row of explanation possible */
      /* FIXME: Check boundaries */
      char  buf[1024];
      char *p = buf;
      int  only_spaces = 1;

      while (next_char != EOF && next_char != '\n') {
	*p++ = next_char;
	if (!isspace(next_char))
	  only_spaces = 0;
	get_next_char();
      }

      if (!only_spaces && tp->explanation == NULL) {
	*p = '\0';
	tp->explanation = strdup(buf);
      }

      get_next_char();
    }
  }

  /* Store the pattern into the textpattern struct. */
  tp->num_elements = strbuffer_index;
  strbuffer[strbuffer_index] = '\0';
  tp->elements = strdup(strbuffer);

  /* Check a possible south constraint. */
  if (tp->edge_constraints & WEST_EDGE) {
    if (next_char == '+') {
      tp->edge_constraints |= SOUTH_EDGE;
      get_next_char();
    }
  }
  else {
    if (next_char == '+') {
      parse_error("Didn't expect '+' here");
      return 0;
    }
    else if (next_char == '-') 
      tp->edge_constraints |= SOUTH_EDGE;
  }

  if (tp->edge_constraints & SOUTH_EDGE) {
    if (parse_north_south_constraint(tp) == 0)
      return 0;
  }
      

  if (next_char == '+' || next_char == '-') {
    tp->edge_constraints |= SOUTH_EDGE;
    if (next_char == '+') {
      tp->edge_constraints |= WEST_EDGE;
      get_next_char();
    }
    if (parse_north_south_constraint(tp) == 0)
      return 0;
  }

  return 1;
}


static int
parse_entry_line(Textpattern *tp)
{
  if (next_char != ':')
    return 0;

  /* FIXME: No syntax control is done on the entry line. */
  parse_to_end_of_line(0);
  tp->entry_line = strdup(strbuffer);

  return 1;
}

static int
parse_constraint_diagram(Textpattern *tp)
{
  int  width;

  /* Constraint diagram is optional. */
  if (!strchr(VALID_PATTERN_CHARS, next_char)
      && !strchr(VALID_EDGE_CHARS, next_char)
      && !strchr(VALID_CONSTRAINT_LABELS, next_char))
    return 1;


  /* See if there is a north border constraint. */
  if (next_char == '+') {
    if ((tp->edge_constraints & NORTH_EDGE) == 0
	|| (tp->edge_constraints & WEST_EDGE) == 0) {
      parse_error("Edge error in constraint diagram");
      return 0;

      get_next_char();
    }
  }
   
  if (next_char == '-') {
    if ((tp->edge_constraints & WEST_EDGE) == 1) {
      parse_error("Edge error in constraint diagram");
      return 0;
    }

    if (parse_north_south_constraint(tp) == 0)
      return 0;
  }

  /* Read each line */
  strbuffer_index = 0;
  while (next_char == '|'
	 || strchr(VALID_PATTERN_CHARS, next_char)
	 || strchr(VALID_CONSTRAINT_LABELS, next_char)) {

    /* Handle WEST_EDGE constraint. */
    if (next_char == '|') {
      if ((tp->edge_constraints & WEST_EDGE) == 0) {
	parse_error("Edge error in constraint diagram");
	return 0;
      }
      get_next_char();
    }
    else {
      if (tp->edge_constraints & WEST_EDGE) {
	parse_error("Missing west constraint");
	return 0;
      }
    }

    /* Handle the pattern itself. */
    width = 0;
    while (strchr(VALID_PATTERN_CHARS, next_char)
	   || strchr(VALID_CONSTRAINT_LABELS, next_char)) {
      strbuffer[strbuffer_index++] = next_char;
      width++;
      get_next_char();
    }

    if (tp->width != width) {
      parse_error("Error in constraint diagram");
      return 0;
    }

    /* parse_whitespace() destroys strbuffer. */
    parse_away_whitespace();
    if (next_char != '\n') {
      while (next_char != EOF && next_char != '\n')
	get_next_char();
    }
    get_next_char();
  }

  /* Store the constraint diagram into the textpattern struct. */
  tp->num_constraint_elements = strbuffer_index;
  strbuffer[strbuffer_index] = '\0';
  tp->constraint_elements = strdup(strbuffer);

  /* Check a possible south constraint. */
  if (tp->edge_constraints & WEST_EDGE) {
    if (next_char == '+') {
      tp->edge_constraints |= SOUTH_EDGE;
      get_next_char();
    }
  }
  else {
    if (next_char == '+') {
      parse_error("Didn't expect '+' here");
      return 0;
    }
    else if (next_char == '-') 
      tp->edge_constraints |= SOUTH_EDGE;
  }

  if (tp->edge_constraints & SOUTH_EDGE) {
    if (parse_north_south_constraint(tp) == 0)
      return 0;
  }
  else {
    if (next_char == '+' || next_char == '-') {
      if (next_char == '+')
	get_next_char();
      if (parse_north_south_constraint(tp) == 0)
	return 0;
    }
  }

  return 1;
}


static int
parse_constraint_lines(Textpattern *tp)
{
  if (next_char != ';') {
    if (tp->num_constraint_elements != 0)
      parse_warning("constraint diagram but no constraint");

    /* Constraint lines are optional */
    return 1;
  }

  /* FIXME: No syntax control is done on the constraint lines. */
  parse_to_end_of_line(0);
  while (next_char == ';')
    parse_to_end_of_line(1);
  tp->constraint_lines_len = strbuffer_index;
  tp->constraint_lines = strdup(strbuffer);

  return 1;
}


static int
parse_action_lines(Textpattern *tp)
{
  if (next_char != '>')
    /* Action lines are optional */
    return 1;

  /* FIXME: No syntax control is done on the action lines. */
  parse_to_end_of_line(0);
  while (next_char == '>')
    parse_to_end_of_line(1);
  tp->action_lines_len = strbuffer_index;
  tp->action_lines = strdup(strbuffer);

  return 1;
}


/* ---------------------------------------------------------------- */
/* Old variables and defines. */


#define MAXLINE 500		/* Max line length */


static int pattern_type = NORMAL_PATTERNS;
static int fullboard    = 0;
static int anchor_both  = 0;
static int verbose      = 0;



int
textpattern_parse(FILE *parm_infile,
		  int parm_pattern_type, int parm_anchor_both, 
		  int parm_fullboard, int parm_verbose,
		  Textpattern *tp)
{
  infile       = parm_infile;
  pattern_type = parm_pattern_type;
  anchor_both  = parm_anchor_both;
  fullboard    = parm_fullboard;
  verbose      = parm_verbose;

  /* Initialize pattern number and buffer for automatically generated
   * helper code.
   */

  assert(tp != NULL);
  textpattern_clear(tp);
  
  /* Get to the pattern itself */
  parse_whitespace(0);

  /* The line:    Pattern   XYZ   */
  if (!parse_pattern_line(tp))
    return 0;
  if (parse_whitespace(0))
    tp->comment = strdup(whitespacebuffer);

  /* The pattern diagram. */
  if (!parse_pattern_diagram(tp))
    return 0;
  if (parse_whitespace(0))
    tp->comment2 = strdup(whitespacebuffer);

  /* The entry line. */
  if (!parse_entry_line(tp))
    return 0;
  if (parse_whitespace(0))
    tp->comment3 = strdup(whitespacebuffer);


  if (!parse_constraint_diagram(tp))
    return 0;
  if (parse_whitespace(0))
    tp->comment4 = strdup(whitespacebuffer);

  if (!parse_constraint_lines(tp))
    return 0;
  if (parse_whitespace(0))
    tp->comment5 = strdup(whitespacebuffer);

  if (!parse_action_lines(tp))
    return 0;
  if (parse_whitespace(0))
    tp->comment6 = strdup(whitespacebuffer);

  return 1;
}


void
textpattern_transform(Textpattern *tp, int transform)
{
  int size, size2;
  char *array1;
  char *array2;
  char *p1, *p2;
  int mini, minj;
  int maxi, maxj;
  int  r;
  int  newedge;
  int  i=0, j=0;
  int  i1, j1;

  assert(0 <= transform && transform < 8);

  /* Let size be twice the (maximum of width and height) plus 1. */
  size = tp->width;
  if (tp->height > size)
    size = tp->height;
  size++;
  size2 = size*2;

  array1 = (char *) malloc(size2 * size2);
  array2 = (char *) malloc(size2 * size2);

  assert(array1 != NULL && array2 != NULL);

  newedge = 0;
  for (r = 0; r < 4; ++r) {
    switch (tp->edge_constraints & (1<<r)) {
    case 0:     i =  0; j =  0; break;
    case NORTH_EDGE: i = -1; j =  0; break;
    case SOUTH_EDGE: i =  1; j =  0; break;
    case EAST_EDGE:  i =  0; j =  1; break;
    case WEST_EDGE:  i =  0; j = -1; break;
    }
    
    if (i != 0 || j != 0) {
      TRANSFORM2(i, j, &i1, &j1, transform);
      if (i1 == 0) {
	if (j1 < 0)
	  newedge |= WEST_EDGE;
	else
	  newedge |= EAST_EDGE;
      } 
      else {
	if (i1 < 0)
	  newedge |= NORTH_EDGE;
	else
	  newedge |= SOUTH_EDGE;
      }
    }
  }
  tp->edge_constraints = newedge;

  mini = 9999;
  maxi = -9999;
  minj = 9999;
  maxj = -9999;
  for (r = 0; r < tp->num_elements; ++r) {
    i = r / tp->width;
    j = r % tp->width;
    TRANSFORM2(i, j, &i1, &j1, transform);

    array1[(i1+size)*size2 + j1+size] = tp->elements[r];
    if (tp->num_constraint_elements > 0)
      array2[(i1+size)*size2 + j1+size] = tp->constraint_elements[r];
    if (i1 < mini) mini = i1;
    if (i1 > maxi) maxi = i1;
    if (j1 < minj) minj = j1;
    if (j1 > maxj) maxj = j1;
  }

  tp->height = maxi - mini + 1;
  tp->width  = maxj - minj + 1;

  p1 = tp->elements;
  p2 = tp->constraint_elements;
  for (i1 = mini; i1 <= maxi; ++i1)
    for (j1 = minj; j1 <= maxj; ++j1) {
      *p1++ = array1[(i1 + size) * size2 + size + j1];
      if (tp->num_constraint_elements > 0)
	*p2++ = array2[(i1 + size) * size2 + size + j1];
    }

#if 0
  /* Debug */
  for (i1 = mini; i1 <= maxi; ++i1) {
    for (j1 = minj; j1 <= maxj; ++j1) 
      putchar(array[(i1 + size) * size * 2 + size + j1]);
    putchar('\n');
  }
#endif

  free(array1);
  free(array2);
}



/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
