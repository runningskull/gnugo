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

/* Extract fuseki patterns from the initial moves of a collection
 * of games.
 *
 * This program finds the most common positions from the initial moves
 * of a collection of games, and generates patterns in patterns.db
 * format for the most common moves in these positions.
 *
 * Positions are identified by Zobrist hash values, completely
 * ignoring the risk for hash collisions. In order to take all
 * symmetries into account, we compute 8 hash values, one for each
 * transformation of the board. Rather than playing on 8 boards in
 * parallel, we construct 8 transformed copies of the Zobrist hash
 * tables and compute one hash value for each of these. To get a
 * transformation invariant hash, we finally sort the 8 hash values.
 *
 * Example:
 * extract_fuseki sgflist 9 8 400
 *
 * generates (up to) 400 patterns, considering the 8 first moves of
 * the 9x9 games listed in the file sgflist, and writes the patterns
 * to stdout. sgflist is a file containing sgf filenames, one per line.
 *
 * The generated patterns may look like, e.g.
 * Pattern Fuseki33
 * # 3/18
 * 
 * |.........
 * |.........
 * |...*.X...
 * |.........
 * |....O....
 * |.........
 * |.........
 * |.........
 * |.........
 * +---------
 * 
 * :8,-,value(3)
 * 
 * The comment line gives the information that this position has been
 * found 18 times among the analyzed games, and 3 out of these 18 times,
 * the move * has been played. The same number 3 is entered as pattern
 * value on the colon line for use by the fuseki module.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <limits.h>
#include "../sgf/sgftree.h"
#include "liberty.h"
#include "random.h"

#define USAGE "\n\
Usage: extract_fuseki files boardsize moves patterns\n\
\n\
files:     The name of a file listing sgf files to examine,\n\
           one filename per line.\n\
boardsize: Only consider games with this size.\n\
moves:     Number of moves considered in each game.\n\
patterns:  Number of patterns to generate.\n\
\n\
"

/* Maximum length of sgf filename. */
#define BUFSIZE 1000

/* Number of moves to consider in each game, given as argument.*/
int moves_per_game;
/* Maximum number of patterns to generate, given as argument.*/
int patterns_to_extract;

/* Number of games to analyze. */
int number_of_games;
/* Dynamically allocated list of sgf file names. */
char **sgf_names;

/* Zobrist hash tables, rotated and reflected into all 8 transformations. */
unsigned int O_hash[8][MAX_BOARD][MAX_BOARD];
unsigned int X_hash[8][MAX_BOARD][MAX_BOARD];
unsigned int move_hash[8][MAX_BOARD][MAX_BOARD];

/* A board is hashed 8 times, once for each transformation, and these
 * numbers are sorted into a transformation invariant hash.
 */
struct invariant_hash {
  unsigned int values[8];
};

/* This is defined in engine/matchpat.c */
extern const int transformations[8][2][2];

#define TRANSFORM(i,j,ti,tj,trans) \
do { \
  *ti = transformations[trans][0][0] * (i) + transformations[trans][0][1] * (j); \
  *tj = transformations[trans][1][0] * (i) + transformations[trans][1][1] * (j); \
} while(0)


/* A situation is the combination of a board position and the move to
 * be made. We use the invariant hashes excluding and including the move
 * as identification. If are interested in positions, we only use the first
 * hash value.
 *
 * Yes, we ignore the possibility of a hash collision.
 */
struct situation {
  struct invariant_hash pre;
  struct invariant_hash post;
};

/* Dynamically allocated table of situations encountered in the analysis. */
struct situation *situation_table;
int number_of_situations;

/* Data type for frequencies of e.g. situations or positions. 'index'
 * is the index into situation_table.
 */
struct frequency {
  int index;
  int n;
};

/* Position frequency table. */ 
struct frequency *frequency_table;
int number_of_distinct_positions;

/* The most common situations are called winners. These are the ones
 * we generate patterns for.
 */
struct winner {
  int index;
  int position_frequency;
  int move_frequency;
  char pattern[MAX_BOARD][MAX_BOARD];
};

/* Dynamically allocated table of winners. */
struct winner *winning_moves;
int number_of_winning_moves;

/* Read the sgf file names. These are assumed to be stored one per
 * line in the file with the name given by 'name'. The sgf file names
 * are copied into dynamically allocated memory by strdup() and
 * pointers to the names are stored into the 'filenames[]' array. It
 * is assumed that 'filenames' has been allocated sufficiently large
 * before this this function is called. If 'filenames' is NULL, the
 * sgf file names are only counted. The number of sgf file names is
 * returned.
 */
static int
read_sgf_filenames(const char *name, char *filenames[])
{
  int n;
  char buf[BUFSIZE];
  FILE *namefile = fopen(name, "r");
  if (!namefile) {
    fprintf(stderr, "Fatal error, couldn't open %s.\n", name);
    exit(EXIT_FAILURE);
  }

  n = 0;
  while (fgets(buf, BUFSIZE, namefile) != NULL) {
    if (filenames != NULL) {
      buf[strlen(buf) - 1] = 0; /* Delete newline character. */
      filenames[n] = strdup(buf);
      if (filenames[n] == NULL) {
	fprintf(stderr, "Fatal error, strdup() failed.\n");
	exit(EXIT_FAILURE);
      }
    }
    n++;
  }

  return n;
}

/* Fill one of the zobrist hash tables with random numbers. */
static void
init_zobrist_table(unsigned int hash[8][MAX_BOARD][MAX_BOARD])
{
  unsigned int k;
  int m, n;
  int i, j;
  int mid = board_size/2;

  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      hash[0][m][n] = 0;
      for (k = 0; 32*k < CHAR_BIT*sizeof(hash[0][0][0]); k++)
	hash[0][m][n] |= gg_urand() << k*32;
    }

  /* Fill in all transformations of the hash table. */
  for (k = 1; k < 8; k++)
    for (m = 0; m < board_size; m++)
      for (n = 0; n < board_size; n++) {
	TRANSFORM(m-mid, n-mid, &i, &j, k);
	hash[k][m][n] = hash[0][i+mid][j+mid];
      }

  /* Debug output. */
  if (0) {
    for (k = 0; k < 8; k++) {
      for (m = 0; m < board_size; m++) {
	for (n = 0; n < board_size; n++)
	  fprintf(stderr, "%8x ", hash[k][m][n]);
	fprintf(stderr, "\n");
      }
      fprintf(stderr, "\n");
      fprintf(stderr, "\n");
    }
  }
}

/* Initialize all Zobrist hash tables with random numbers. */
static void
init_zobrist_numbers(void)
{
  gg_srand(1);
  init_zobrist_table(O_hash);
  init_zobrist_table(X_hash);
  init_zobrist_table(move_hash);
}

/* Initialize the situation_table array. */
static void
init_situations(void)
{
  situation_table = calloc(moves_per_game * number_of_games,
			  sizeof(*situation_table));
  if (!situation_table) {
    fprintf(stderr, "Fatal error, failed to allocate situations table.\n");
    exit(EXIT_FAILURE);
  }
  number_of_situations = 0;
}

/* Compare two hash values. Used for sorting the hash values in the
 * invariant hash.
 */
static int
compare_numbers(const void *a, const void *b)
{
  unsigned int aa = *((const unsigned int *)a);
  unsigned int bb = *((const unsigned int *)b);
  if (aa > bb)
    return 1;
  if (aa < bb)
    return -1;
  return 0;
}

/* Compute hash values for all transformations of the position
 * currently in the p[][] array. The hash values are not sorted by
 * this function.
 */
static void
common_hash_board(struct invariant_hash *hash, int color_to_play)
{
  int m, n;
  int k;
  
  for (k = 0; k < 8; k++)
    hash->values[k] = 0;
  
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      for (k = 0; k < 8; k++) {
	if (p[m][n] == color_to_play)
	  hash->values[k] ^= O_hash[k][m][n];
	else if (p[m][n] != EMPTY)
	  hash->values[k] ^= X_hash[k][m][n];
      }
    }
}

/* Compute invariant hash for the current position. */
static void
hash_board(struct invariant_hash *hash, int color_to_play)
{
  common_hash_board(hash, color_to_play);
  /* Sort the 8 hash values. */
  qsort(hash->values, 8, sizeof(hash->values[0]), compare_numbers);
}

/* Compute invariant hash for the current situation, i.e. position
 * plus a move to be made.
 */
static void
hash_board_and_move(struct invariant_hash *hash, int color_to_play,
		    int m, int n)
{
  int k;
  
  common_hash_board(hash, color_to_play);

  for (k = 0; k < 8; k++)
    hash->values[k] ^= move_hash[k][m][n];

  /* Notice that we of course must wait with sorting until we have
   * added the move to the hash values.
   */
  qsort(hash->values, 8, sizeof(hash->values[0]), compare_numbers);
}


/* Adapted from play_sgf_tree() in engine/sgfutils.c. Returns the
 * next move from the game record in (*m, *n) and color in *color. If
 * handicap stones are encountered, these are put on the board
 * immediately. Return value is 1 if another move was found in the
 * game record, 0 otherwise.
 */
static int
get_move_from_sgf(SGFNode *node, int *m, int *n, int *color)
{
  SGFProperty *prop;
  int i, j;
      
  for (prop=node->props; prop; prop=prop->next) {
    switch(prop->name) {
    case SGFAB:
      get_moveXY(prop, &i, &j, board_size);
      /* Put handicap stones on the board at once. */
      add_stone(i, j, BLACK);
      break;
      
    case SGFAW:
      fprintf(stderr, "Warning: white stone added.\n");
      return 0;
      break;
      
    case SGFPL:
      fprintf(stderr, "Warning: PL property encountered.\n");
      return 0;
      break;
      
    case SGFW:
    case SGFB:
      *color = (prop->name == SGFW) ? WHITE : BLACK;
      
      if (!get_moveXY(prop, m, n, board_size)) {
	fprintf(stderr, "Warning: failed to get move coordinates.\n");
	return 0;
      }
      return 1;
      break;
    }
  }

  return 0;
}

/* Add a situation to the situation_table array. */
static void
add_situation(struct invariant_hash *pre, struct invariant_hash *post)
{
  situation_table[number_of_situations].pre = *pre;
  situation_table[number_of_situations].post = *post;
  number_of_situations++;
}

/* Compare two situations. Used for sorting the situation_table array
 * and when building frequency tables for the different moves at the
 * same position.
 */
static int
compare_situations(const void *a, const void *b)
{
  const struct situation *aa = a;
  const struct situation *bb = b;
  int k;

  for (k = 0; k < 8; k++) {
    if (aa->pre.values[k] > bb->pre.values[k])
      return 1;
    if (aa->pre.values[k] < bb->pre.values[k])
      return -1;
  }
  
  for (k = 0; k < 8; k++) {
    if (aa->post.values[k] > bb->post.values[k])
      return 1;
    if (aa->post.values[k] < bb->post.values[k])
      return -1;
  }

  return 0;
}

/* Compare two positions. Used when building frequency table for the
 * different positions encountered.
 */
static int
compare_positions(const void *a, const void *b)
{
  const struct situation *aa = a;
  const struct situation *bb = b;
  int k;

  for (k = 0; k < 8; k++) {
    if (aa->pre.values[k] > bb->pre.values[k])
      return 1;
    if (aa->pre.values[k] < bb->pre.values[k])
      return -1;
  }
  
  return 0;
}

/* Compare two frequency table entries. The returned values are
 * "backwards" because we always want to sort frequencies in falling
 * order.
 */
static int
compare_frequencies(const void *a, const void *b)
{
  const struct frequency *aa = a;
  const struct frequency *bb = b;

  if (aa->n < bb->n)
      return 1;
  
  if (aa->n > bb->n)
      return -1;
  
  return 0;
}

/* If this situation is listed among the winners, fill in the pattern
 * entry of the winner struct.
 */
static void
store_pattern_if_winner(struct invariant_hash *pre,
			struct invariant_hash *post,
			int color, int m, int n)
{
  int k;
  struct situation s;
  s.pre = *pre;
  s.post = *post;
  for (k = 0; k < number_of_winning_moves; k++) {
    if (compare_situations(&situation_table[winning_moves[k].index],
			   &s) == 0) {
      /* This is a winner. Record the pattern. */
      int i,j;
      for (i = 0; i < board_size; i++)
	for (j = 0; j < board_size; j++) {
	  if (p[i][j] == EMPTY)
	    winning_moves[k].pattern[i][j] = '.';
	  else if (p[i][j] == color)
	    winning_moves[k].pattern[i][j] = 'O';
	  else
	    winning_moves[k].pattern[i][j] = 'X';
	}
      winning_moves[k].pattern[m][n] = '*';
    }
  }
}

/* Play through the initial moves of a game. If 'collect_statistics'
 * is one, store all encounterd situations in the situation_table
 * array. Otherwise, see if there are any winners among the situations
 * and store the corresponding pattern so that it can subsequently be
 * printed. Return 0 if there was some problem with the game record,
 * e.g. fewer moves than expected.
 */
static int
examine_game(SGFNode *sgf, int collect_statistics)
{
  int k;
  int m, n;
  SGFNode *node = sgf;
  struct invariant_hash prehash;
  struct invariant_hash posthash;
  int color;

  /* Call the engine to clear the board. */
  clear_board();

  /* Loop through the first moves_per_game moves of each game. */
  for (k = 0; k < moves_per_game && node != NULL; node = node->child) {
    if (!get_move_from_sgf(node, &m, &n, &color))
      continue;
    gg_assert(m >= 0 && m < board_size && n >= 0 && n <= board_size);
    hash_board(&prehash, color);
    hash_board_and_move(&posthash, color, m, n);
    if (collect_statistics)
      add_situation(&prehash, &posthash);
    else
      store_pattern_if_winner(&prehash, &posthash, color, m, n);
    play_move(m, n, color);

    /* Debug output. */
    if (0) {
      int l;
      for (l = 0; l < 8; l++)
	fprintf(stderr, "%8x ", prehash.values[l]);
      fprintf(stderr, " ");
      for (l = 0; l < 8; l++)
	fprintf(stderr, "%8x ", posthash.values[l]);
      fprintf(stderr, "\n");
      showboard(0);
    }

    k++;
  }

  if (!node)
    return 0;
  
  return 1;
}

/* Play through the initial moves of all games and collect hash values
 * for the encountered situations.
 */
static void
collect_situations(void)
{
  int k;
  init_situations();
  for (k = 0; k < number_of_games; k++) {
    int size;
    SGFNode *sgf;

    /* Debug output. */
    if (0)
      fprintf(stderr, "Reading %s\n", sgf_names[k]);

    sgf = readsgffile(sgf_names[k]);
    if (!sgf) {
      fprintf(stderr, "Warning: Couldn't open sgf file %s.\n", sgf_names[k]);
      continue;
    }
    
    if (!sgfGetIntProperty(sgf, "SZ", &size) || size != board_size)
      continue; /* Board of wrong size, ignore the game. */

    if (!examine_game(sgf, 1))
      fprintf(stderr, "Warning: Problem with sgf file %s.\n", sgf_names[k]);
  }
}

/* Find the most common positions and moves, for which we want to
 * generate patterns.
 */
static void
analyze_statistics(void)
{
  int k;
  /* Sort all the collected situations. */
  qsort(situation_table, number_of_situations, sizeof(*situation_table),
	compare_situations);

  /* Debug output. */
  if (0) {
    int i, k;
    for (i = 0; i < number_of_situations; i++) {
      fprintf(stderr, "%4d ", i);
      for (k = 0; k < 8; k++)
	fprintf(stderr, "%8x ", situation_table[i].pre.values[k]);
      fprintf(stderr, "  ");
      for (k = 0; k < 8; k++)
	fprintf(stderr, "%8x ", situation_table[i].post.values[k]);
      fprintf(stderr, "\n");
    }
  }

  /* Set up frequency table. */
  frequency_table = calloc(number_of_situations, sizeof(*frequency_table));
  if (!frequency_table) {
    fprintf(stderr, "Fatal error, failed to allocate frequency table.\n");
    exit(EXIT_FAILURE);
  }
  number_of_distinct_positions = 0;

  /* Make frequency analysis of the positions before the moves. */
  for (k = 0; k < number_of_situations; k++) {
    if (k == 0 || compare_positions(&situation_table[k],
				    &situation_table[k-1]) != 0) {
      frequency_table[number_of_distinct_positions].index = k;
      frequency_table[number_of_distinct_positions].n = 0;
      number_of_distinct_positions++;
    }
    frequency_table[number_of_distinct_positions-1].n++;
  }

  /* Sort the frequency table, in falling order. */
  qsort(frequency_table, number_of_distinct_positions,
	sizeof(*frequency_table), compare_frequencies);

  /* Debug output. */
  if (0) {
    int l;
    for (l = 0; l < number_of_distinct_positions; l++) {
      fprintf(stderr, "%4d %5d\n", frequency_table[l].n,
	      frequency_table[l].index);
    }
  }

  /* Set up winners array. */
  winning_moves = calloc(patterns_to_extract, sizeof(*winning_moves));
  if (!winning_moves) {
    fprintf(stderr, "Fatal error, failed to allocate winning moves table.\n");
    exit(EXIT_FAILURE);
  }
  number_of_winning_moves = 0;
  
  /* Starting with the most common position, find the most common
   * moves for each position, until the number of patterns to be
   * generated is reached. Don't include moves with a frequency
   * smaller than a tenth of the most common move.
   */
  for (k = 0; k < number_of_distinct_positions; k++) {
    int index = frequency_table[k].index;
    int i;

    /* Build a new frequency table for the different moves in this position. */
    struct frequency move_frequencies[MAX_BOARD * MAX_BOARD];
    int number_of_moves = 0;
    for (i=index; ;i++) {
      if (i == number_of_situations
	  || (i > index
	      && compare_positions(&situation_table[i],
				   &situation_table[i-1]) != 0))
	break;
      if (i==index || compare_situations(&situation_table[i],
					 &situation_table[i-1]) != 0) {
	move_frequencies[number_of_moves].index = i;
	move_frequencies[number_of_moves].n = 0;
	number_of_moves++;
      }
      move_frequencies[number_of_moves-1].n++;
    }
    
    /* Sort the moves, in falling order. */
    qsort(move_frequencies, number_of_moves,
	  sizeof(*move_frequencies), compare_frequencies);

    /* Debug output. */
    if (0) {
      for (i = 0; i < number_of_moves; i++) {
	fprintf(stderr, "%4d %3d %4d\n", index, move_frequencies[i].n,
		move_frequencies[i].index);
      }
    }

    /* Add the moves to the list of winners. */
    for (i = 0; i < number_of_moves; i++) {
      if (10 * move_frequencies[i].n < move_frequencies[0].n
	  && move_frequencies[i].n < 10)
	break;
      winning_moves[number_of_winning_moves].index = move_frequencies[i].index;
      winning_moves[number_of_winning_moves].position_frequency =
	frequency_table[k].n;
      winning_moves[number_of_winning_moves].move_frequency =
	move_frequencies[i].n;
      number_of_winning_moves++;
      
      if (number_of_winning_moves == patterns_to_extract)
	break;
    }
    
    if (number_of_winning_moves == patterns_to_extract)
      break;
  }

  /* Debug output. */
  if (0) {
    int i;
    for (i = 0; i < number_of_winning_moves; i++) {
      fprintf(stderr, "%4d %3d %3d\n",
	      winning_moves[i].index,
	      winning_moves[i].position_frequency,
	      winning_moves[i].move_frequency);
    }
  }
}

/* Scan through the games a second time to pick up the patterns
 * corresponding to the winning moves.
 */
static void
generate_patterns(void)
{
  int k;
  for (k = 0; k < number_of_games; k++) {
    int size;
    SGFNode *sgf = readsgffile(sgf_names[k]);
    if (!sgf) {
      fprintf(stderr, "Warning: Couldn't open sgf file %s.\n", sgf_names[k]);
      continue;
    }
    if (!sgfGetIntProperty(sgf, "SZ", &size) || size != board_size)
      continue; /* Board of wrong size, ignore the game. */
    (void) examine_game(sgf, 0);
  }
}

/* Print the winning patterns in patterns.db format on stdout. */
static void
print_patterns(void)
{
  int k;
  int m, n;
  for (k = 0; k < number_of_winning_moves; k++) {
    printf("Pattern Fuseki%d\n", k+1);
    printf("# %d/%d\n\n", winning_moves[k].move_frequency,
	   winning_moves[k].position_frequency);
    for (m = 0; m < board_size; m++) {
      printf("|");
      for (n = 0; n < board_size; n++) {
	printf("%c", winning_moves[k].pattern[m][n]);
      }
      printf("\n");
    }
    printf("+");
    for (n = 0; n < board_size; n++) {
      printf("-");
    }
    printf("\n\n:8,-,value(%d)\n\n\n", winning_moves[k].move_frequency);
  }
}

int
main(int argc, char *argv[])
{
  /* Check number of arguments. */
  if (argc < 5) {
    fprintf(stderr, USAGE);
    exit(EXIT_FAILURE);
  }

  /* Check arguments. */
  board_size = atoi(argv[2]);
  if (board_size % 2 == 0) {
    fprintf(stderr, "Fatal error, only odd boardsizes supported: %d.\n",
	    board_size);
    exit(EXIT_FAILURE);
  }
  if (board_size < 9 || board_size > 19)
    fprintf(stderr, "Warning: strange boardsize: %d.\n", board_size);
  
  moves_per_game = atoi(argv[3]);
  if (moves_per_game < 1 || moves_per_game > 20)
    fprintf(stderr, "Warning: strange number of moves per game: %d.\n",
	    moves_per_game);

  patterns_to_extract = atoi(argv[4]);
  if (patterns_to_extract < 1)
    fprintf(stderr, "Warning: strange number of patterns to extract: %d.\n",
	    patterns_to_extract);

  /* Count the number of sgf files. */
  number_of_games = read_sgf_filenames(argv[1], NULL);

  /* Allocate space for the list of sgf file names. */
  sgf_names = calloc(number_of_games, sizeof(*sgf_names));
  if (sgf_names == NULL) {
    fprintf(stderr, "Fatal error, failed to allocate memory.\n");
    exit(EXIT_FAILURE);
  }
  
  /* Read the list of sgf files and store in memory. */
  (void) read_sgf_filenames(argv[1], sgf_names);

  /* Build tables of random numbers for Zobrist hashing. */
  init_zobrist_numbers();
  
  /* Play through the initial moves of all games and collect hash values
   * for the encountered situations.
   */
  collect_situations();

  /* Find the most common positions and moves, for which we want to
   * generate patterns.
   */
  analyze_statistics();

  /* Generate patterns from the chosen positions and moves.
   */
  generate_patterns();

  /* Write the patterns to stdout in patterns.db format.
   */
  print_patterns();

  return 0;
}

/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
