/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * This is GNU Go, a Go program. Contact gnugo@gnu.org, or see       *
 * http://www.gnu.org/software/gnugo/ for more information.          *
 *                                                                   *
 * Copyright 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007,   *
 * 2008 and 2009 by the Free Software Foundation.                    *
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

/*
 * A little program to convert the SGF Spec into something usable in a
 * parser- basically, the two letters moved into a 'short'
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_LINE 128

static short str2short(char *str);

/*
 * combine two characters into a short.
 */

static short
str2short(char *str)
{
  return (str[0] | str[1] << 8);
}

int 
main(void)
{
  char instring[MAX_LINE];
  char sgf_tag[MAX_LINE];
  char comment[MAX_LINE];
  int i;

  while (fgets(instring, MAX_LINE, stdin)) {
    i = strlen(instring) - 1;
      
    /* Remove trailing newline and spaces */
    while (i >= 0 
	   && (instring[i] == ' ' || instring[i] == 10))
      instring[i--] = 0;
      
    /* Blank lines stay blank lines */
    if (strcmp(instring, "") == 0) {
      fprintf(stdout, "\n");
      continue;
    }
      
    sscanf(instring, "%4s %75s", sgf_tag, comment);
    strncpy(sgf_tag, instring, 4);
    if (strlen(instring) > 4) 
      strncpy(comment, instring+4, MAX_LINE-4);
    else
      strcpy(comment, "");

    /* outdated and modified notations. */
    /* just shift everything over */
    if (sgf_tag[0] == '*' || sgf_tag[0] == '!') {
      sgf_tag[0] = sgf_tag[1];
      sgf_tag[1] = sgf_tag[2];
      sgf_tag[2] = sgf_tag[3];
    }
      
    /* If its not a real tag, just take it as a comment. */
    if (sgf_tag[0] < 'A' || sgf_tag[0] > 'Z') {
      fprintf(stdout, "/* %s */\n", instring);
      continue;
    }
    else {
      /* otherwise, write the tag and value to the file */
      if (strlen(comment))
	fprintf(stdout, "     /* %s */\n", comment);
      fprintf(stdout, "#define SGF%s %5d\n", sgf_tag, str2short(sgf_tag));
    }
  }
  
  /* Needs 0 exit status or else make fails. */
  return EXIT_SUCCESS;
}
