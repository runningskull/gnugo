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

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#define BUFSIZE 160
#define MAX_STONES 40
#define MAX_BOARDSIZE 19

#define USAGE "\
Usage : uncompress_fuseki boardsize filename\n\
"

/* Write a board point in sgf format. Also do a sanity check. */
static void
write_stone(int i, int j)
{
  assert(i > 0 && i <= MAX_BOARDSIZE);
  assert(j > 0 && j <= MAX_BOARDSIZE);
  printf("%c%c", j + 'a' - 1, i + 'a' - 1);
}

int
main(int argc, char *argv[])
{
  const char *filename;
  FILE *input_FILE;
  char line[BUFSIZE];
  char name[BUFSIZE];
  int value;
  int k;
  int row = 0;
  int movei = -1;
  int movej = -1;
  int xi[MAX_STONES];
  int xj[MAX_STONES];
  int oi[MAX_STONES];
  int oj[MAX_STONES];
  int num_x = 0;
  int num_o = 0;
  
  /* Check number of arguments. */
  if (argc != 2) {
    fprintf(stderr, USAGE);
    return EXIT_FAILURE;
  }

  filename = argv[1];

  input_FILE = fopen(filename, "r");
  if (!input_FILE) {
    fprintf(stderr, "compress_fuseki: Cannot open file %s\n", filename);
    return EXIT_FAILURE;
  }
  
  while (fgets(line, BUFSIZE, input_FILE)) {
    if (sscanf(line, "Pattern %s", name) == 1) {
      /* A new name has been picked up.
       * Reset the row counter and the lists of stones.
       */
      row = 0;
      num_x = 0;
      num_o = 0;
    }
    else if (line[0] == ':') {
      /* The colon line ends the pattern. First get the move value. */
      if (sscanf(line, ":8,-,value(%d)", &value) != 1) {
	fprintf(stderr, "compress_fuseki: Misformed colon line \"%s\"\n",
		line);
	return EXIT_FAILURE;
      }

      /* Write the compressed description of this pattern.
       * Pad the stone list with passes (tt) if unbalanced colors.
       */
      printf("%s %d ", name, value);
      write_stone(movei, movej);
      while (num_x > 0 || num_o > 0) {
	if (num_x > 0) {
	  num_x--;
	  write_stone(xi[num_x], xj[num_x]);
	}
	else if (num_o > 0)
	  printf("tt");
	if (num_o > 0) {
	  num_o--;
	  write_stone(oi[num_o], oj[num_o]);
	}
	else if (num_x > 0)
	  printf("tt");
      }
      printf("\n");
    }
    else if (line[0] == '|') {
      /* Found a diagram line. */
      row++;
      for (k = 1; line[k] && line[k] != '|'; k++) {
	if (line[k] == '*') {
	  movei = row;
	  movej = k;
	}
	else if (line[k] == 'X') {
	  xi[num_x] = row;
	  xj[num_x] = k;
	  num_x++;
	  assert(num_x < MAX_STONES);
	}
	else if (line[k] == 'O') {
	  oi[num_o] = row;
	  oj[num_o] = k;
	  num_o++;
	  assert(num_o < MAX_STONES);
	}
      }
    }
  }

  return EXIT_SUCCESS;
}

/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
