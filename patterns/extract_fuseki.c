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

/*
 * Notes on the statistics:
 * 
 * The statistics code assumes that every input file is valid. Use
 * the output file option to sort out which input files are valid, and
 * check output for problems. Rerun after fixing/removing invalid files.
 *
 * Outcome is defined by RE in sgf. Files without a parsable RE, or which
 * do not have a winner, are invalid and need to be excluded.
 * 
 * Pearson chi squared at 5% is used to test significance of
 * differences among moves at a given position. Moves excluded by
 * popularity rules are grouped together and considered as one.  A
 * positive result means that among all possible moves in a position,
 * there's a difference somewhere. The next question is where. One
 * clue comes from dchisq, which is the contribution to the overall
 * chi squared for each move, with larger meaning higher impact on
 * significance of overall result. Another comes from post hoc tests.
 * Each pair of moves from a position with a statistically significant
 * impact of move choice is compared, again with Pearson chi squared
 * at 5%, and the positive tests printed. No correction is done for
 * multiple tests. Pairs with less than 6 total moves are not tested,
 * so it's possible for there to be a significant overall result
 * without any positive post hocs. In this case, the overall result is
 * doubtful as well. 
 *
 * If the interest is solely in statistics, using min_pos_freq to
 * avoid positions without enough data to hope for significance makes
 * sense: 6 at a minimum. 
 *
 * Note that the popularity exclusion rules can result in patterns being
 * left in the db which have no parent in the db.
 * 
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <limits.h>
#include <math.h>
#include "liberty.h"
#include "gg_utils.h"
#include "random.h"
#include "../sgf/sgftree.h"

#define USAGE "\n\
Usage: extract_fuseki files boardsize moves patterns handicap strength half_board min_pos_freq min_move_percent min_move_freq [output file]\n\
files:     The name of a file listing sgf files to examine,\n\
           one filename per line.\n\
boardsize: Only consider games with this size.\n\
moves:     Number of moves considered in each game.\n\
handicap:  0 - no handicap, 1 - any game, 2-9 - two to nine handicap stones\n\
           10 any handicap game\n\
strength:  The lowest strength of the players (1k-30k)\n\
half_board: 0 - full board patterns, 1 - half board patterns\n\
min_pos_freq: how many times a position must occur before patterns\n\
           from it are generated\n\
min_move_percent: minimum popularity relative to most popular move \n\
           (counted by unique players) required of a move \n\
           in a given position before it gets a pattern\n\
min_move_freq: minimum number of unique players who must play a move\n\
            before it gets a pattern\n\
output file: Optional (if this exists, extract_fuseki will sort the games instead)\n\
"

/* Maximum length of sgf filename. */
#define BUFSIZE 1000

/* Number of moves to consider in each game, given as argument.*/
int moves_per_game;

/* Flag checking the setting for generating half board patterns */
int half_board_patterns = 0;

/* Maximum number of patterns to generate */
#define MAX_PATTERNS_TO_EXTRACT 100000


/* Handicap value, given as argument.*/
int handicap_value;

/* Lowest strength, given as argument.*/
int player_strength;


/* Min # of times a position must be seen before moves from it become
 * patterns.
 * Might want this larger to ensure reasonable statistics, 6 or more, say
 * or smaller to hit every move down to unique games, 2 say;
 * or even keep churning out moves with 1.
 *
 * Given as argument.
*/

int min_position_freq;


/* popularity arguments */
double min_move_percent;
int min_move_freq;


/* Number of games to analyze. */
int number_of_games;

/* Dynamically allocated array marking the games that could not be
 * used for some reason.
 */
int *unused_games;

/* WARN 1 warns about unused games. */
/* WARN 2 also notes assumptions about metainfo. */
#define WARN 1


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


/* A situation is the combination of a board position and the move to
 * be made. We use the invariant hashes excluding and including the move
 * as identification. If are interested in positions, we only use the first
 * hash value.
 *
 * We ignore the possibility of a hash collision.
 *
 * outcome is the color which won the game
 * player is the (hashed) name of the player who made the move
 */
struct situation {
  struct invariant_hash pre;
  struct invariant_hash post;
  int outcome;
  unsigned int player;
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
  int n_win;
  int n_player;
};

/* Position frequency table. */ 
struct frequency *frequency_table;
int number_of_distinct_positions;

/* The most common situations are called winners. These are the ones
 * we generate patterns for.
 * 
 * 'index' is normally an index into situation_table, or -1 for
 * special aggregate entry (with no pattern) to collect stats for
 * unpopular moves
 *
 * pre is hash[0], and must be stored here for aggregate
 */
struct winner {
  int index;
  unsigned int pre;
  int position_frequency;
  int move_frequency;
  int n_player;
  int position_success;
  int move_success;
  char pattern[MAX_BOARD][MAX_BOARD];
};

/* Dynamically allocated table of winners. */
struct winner *winning_moves;
int number_of_winning_moves;

/* critical values of chisquare distribution with n degrees of freedom */
/* p < 0.05
 */
double chisquarecrit05[] = {
  3.8415, 5.9915, 7.8147, 9.4877, 11.0705, 12.5916, 14.0671, 15.5073,
  16.9190, 18.3070, 19.6751, 21.0261, 22.3620, 23.6848, 24.9958, 26.2962,
  27.5871, 28.8693, 30.1435, 31.4104, 32.67057, 33.92444, 35.17246,
  36.41503, 37.65248, 38.88514, 40.11327, 41.33714, 42.55697, 43.77297,
  44.98534, 46.19426, 47.39988, 48.60237, 49.80185, 50.99846, 52.19232,
  53.38354, 54.57223, 55.75848, 56.94239, 58.12404, 59.30351, 60.48089,
  61.65623, 62.82962, 64.00111, 65.17077, 66.33865, 67.50481};

/* p < 0.10, should be same size as 05 */
double chisquarecrit10[] = {
  2.7055, 4.6052, 6.2514, 7.7794, 9.2364, 10.6446, 12.0170, 13.3616,
  14.6837, 15.9872, 17.2750, 18.5493, 19.8119, 21.0641, 22.3071, 23.5418,
  24.7690, 25.9894, 27.2036, 28.4120, 29.61509, 30.81328, 32.00690,
  33.19624, 34.38159, 35.56317, 36.74122, 37.91592, 39.08747, 40.25602,
  41.42174, 42.58475, 43.74518, 44.90316, 46.05879, 47.21217, 48.36341,
  49.51258, 50.65977, 51.80506, 52.94851, 54.09020, 55.23019, 56.36854,
  57.50530, 58.64054, 59.77429, 60.90661, 62.03754, 63.16712};

double chisquarecrit01[] = {
  6.63489660102121, 9.21034037197618, 11.3448667301444, 13.2767041359876,
  15.086272469389, 16.8118938297709, 18.4753069065824, 20.0902350296632,
  21.6659943334619, 23.2092511589544, 24.7249703113183, 26.2169673055359,
  27.6882496104570, 29.1412377406728, 30.5779141668925, 31.9999269088152,
  33.4086636050046, 34.8053057347051, 36.1908691292701, 37.5662347866250,
  38.9321726835161, 40.2893604375938, 41.6383981188585, 42.9798201393516,
  44.3141048962192, 45.6416826662832, 46.9629421247514, 48.2782357703155,
  49.5878844728988, 50.8921813115171, 52.1913948331919, 53.4857718362354,
  54.7755397601104, 56.0609087477891, 57.3420734338592, 58.619214501687,
  59.8925000450869, 61.1620867636897, 62.4281210161849, 63.6907397515645,
  64.9500713352112, 66.2062362839932, 67.4593479223258, 68.7095129693454,
  69.9568320658382, 71.2014002483115, 72.4433073765482, 73.6826385201058,
  74.9194743084782, 76.1538912490127};

double chisquarecrit001[] = {
  10.8275661706627, 13.8155105579643, 16.2662361962381, 18.4668269529032,
  20.5150056524329, 22.4577444848253, 24.3218863478569, 26.1244815583761,
  27.8771648712566, 29.5882984450744, 31.26413362024, 32.9094904073602,
  34.5281789748709, 36.1232736803981, 37.6972982183538, 39.2523547907685,
  40.7902167069025, 42.31239633168, 43.8201959645175, 45.3147466181259,
  46.7970380415613, 48.2679422908352, 49.7282324664315, 51.1785977773774,
  52.6196557761728, 54.0519623885766, 55.4760202057452, 56.8922853933536,
  58.3011734897949, 59.7030643044299, 61.0983060810581, 62.4872190570885,
  63.870098522345, 65.2472174609424, 66.618828843701, 67.9851676260242,
  69.3464524962412, 70.702887411505, 72.0546629519878, 73.401957518991,
  74.7449383984238, 76.0837627077, 77.418578241314, 78.749524228043,
  80.076732010819, 81.40032565871, 82.720422519124, 84.0371337172235,
  85.350564608593, 86.6608151904032};

/*
 * Append the files that are sorted to a specific file
 */

static void
write_sgf_filenames(const char *name, char *filenames[])
{
  int n;
  FILE *namefile = fopen(name, "a");
  if (!namefile) {
    fprintf(stderr, "Fatal error, couldn't open %s.\n", name);
    exit(EXIT_FAILURE);
  }
  
  for (n = 0; n < number_of_games; n++) {
    if (unused_games[n] == 0)
      fprintf(namefile, "%s\n", filenames[n]);
  }
}


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
      if (buf[strlen(buf) - 2] == '\r') {
	buf[strlen(buf) - 2] = '\0'; 
	/* Delete carriage return character, if any. */
      }
      else {
	buf[strlen(buf) - 1] = '\0'; 
	/* Delete newline character. */
      }
      
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
	TRANSFORM2(m-mid, n-mid, &i, &j, k);
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
  unsigned int aa = *((const unsigned int *) a);
  unsigned int bb = *((const unsigned int *) b);
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
	if (BOARD(m, n) == color_to_play)
	  hash->values[k] ^= O_hash[k][m][n];
	else if (BOARD(m, n) != EMPTY)
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
  gg_sort(hash->values, 8, sizeof(hash->values[0]), compare_numbers);
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
  gg_sort(hash->values, 8, sizeof(hash->values[0]), compare_numbers);
}


/* the so called X31 hash from gtk, for hashing strings */
static unsigned int
hash_string(const char *v)
{
  unsigned int h = 0;
  while (*v != '\0') {
    h = (h << 5) - h + *v;
    v++;
  }
  return h;
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
  
  for (prop = node->props; prop; prop = prop->next) {
    if (!prop || !prop->name || !node) {
      /* something wrong with the SGF file properties */
      if (1)
	fprintf(stderr, "Something wrong with the SGF file properties.\n");
      return 0;
    }
    switch (prop->name) {
    case SGFAB:
      get_moveXY(prop, &i, &j, board_size);
      /* Put handicap stones on the board at once. */
      add_stone(POS(i, j), BLACK);
      break;
      
    case SGFAW:
      if (0)
	fprintf(stderr, "Warning: white stone added.\n");
      return 0;
      break;
      
    case SGFPL:
      if (0)
	fprintf(stderr, "Warning: PL property encountered.\n");
      return 0;
      break;
      
    case SGFW:
    case SGFB:
      *color = (prop->name == SGFW) ? WHITE : BLACK;
      
      if (!get_moveXY(prop, m, n, board_size)) {
	if (0)
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
add_situation(struct invariant_hash *pre, struct invariant_hash *post,
	      int outcome, unsigned int player)
{
  situation_table[number_of_situations].pre = *pre;
  situation_table[number_of_situations].post = *post;
  situation_table[number_of_situations].outcome = outcome;
  situation_table[number_of_situations].player = player;
  number_of_situations++;
}

/* Compare two situations. Used (indirectly, see compare_situations2)
 * for sorting the situation_table array
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

static int
compare_situations2(const void *a, const void *b)
{
  const struct situation *aa = a;
  const struct situation *bb = b;
  int r = compare_situations(a, b);
  if (r != 0) 
    return r;
  if (aa->player > bb->player)
    return 1;
  if (aa->player < bb->player)
    return -1;

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
 * 
 * The first version counts every game equally, the second version
 * counts a game only once per unique player.
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

static int
compare_frequencies2(const void *a, const void *b)
{
  const struct frequency *aa = a;
  const struct frequency *bb = b;
  
  if (aa->n_player < bb->n_player)
    return 1;
  
  if (aa->n_player > bb->n_player)
    return -1;
  
  return 0;
}

/*
 * find_region answers in what region the move is.
 * There are 9 regions, corners, sides and center.
 */

static int
find_region(int m, int n)
{
  if (m < 7) {
    if (n < 7)
      return 0;
    else if (n > 11)
      return 1;
    else if (n > 6 && m < 5)
      return 6;
  }
  else if (m > 11) {
    if (n < 7)
      return 2;
    else if (n > 11)
      return 3;
    else if (n > 6 && m > 13)
      return 7;
  }
  else if (m > 6) {
    if (n < 5)
      return 4;
    else if (n > 13)
      return 5;
  }
  /* otherwise in center */
  return 8;
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
  int region = 8;
  int i, j;
  int move_number = 1;
  s.pre = *pre;
  s.post = *post;

  for (k = 0; k < number_of_winning_moves; k++) {
    if (winning_moves[k].index != -1
	&& compare_situations(&situation_table[winning_moves[k].index],
			      &s) == 0) {
      /* This is a winner. Record the pattern. */
      for (i = 0; i < board_size; i++)
	for (j = 0; j < board_size; j++) {
	  if (BOARD(i, j) == EMPTY)
	    winning_moves[k].pattern[i][j] = '.';
	  else if (BOARD(i, j) == color) {
	    winning_moves[k].pattern[i][j] = 'O';
	    move_number++;
	  }
	  else if ((color == WHITE && BOARD(i, j) == BLACK)
		   || (color == BLACK && BOARD(i, j) == WHITE)) {
	    winning_moves[k].pattern[i][j] = 'X';
	    move_number++;
	  }
	  else { /* something is wrong */
	    fprintf(stderr, "Error in store_pattern_if_winner: %d\n", k);
	    winning_moves[k].pattern[i][j] = '.';
	  }
	}
      winning_moves[k].pattern[m][n] = '*';
      /* Add ? in areas far away from the move. */
      if (half_board_patterns == 1 && move_number > 3 && board_size == 19)
        region = find_region(m, n);
      if (region != 8) {
        for (i = 0; i < board_size; i++) {
          for (j = 0; j < board_size; j++) {
            if (region == 0) {
              if (i + j > 23)
     	        winning_moves[k].pattern[i][j] = '?';
     	    }
            else if (region == 1) {
              if (i - j > 5)
 	        winning_moves[k].pattern[i][j] = '?';
 	    }
            else if (region == 2) {
              if (i + board_size - j < 14)
 	        winning_moves[k].pattern[i][j] = '?';
 	    }
 	    else if (region == 3) {
              if (i + j < 13)
 	        winning_moves[k].pattern[i][j] = '?';
 	    }
            else if (region == 4) {
              if (j > 10)
 	        winning_moves[k].pattern[i][j] = '?';
 	    }
            else if (region == 5) {
              if (j < 8)
 	        winning_moves[k].pattern[i][j] = '?';
 	    }
            else if (region == 6) {
              if (i > 10)
 	        winning_moves[k].pattern[i][j] = '?';
 	    }
            else if (region == 7) {
              if (i < 8)
 	        winning_moves[k].pattern[i][j] = '?';
 	    }
          }
        }
      }
    }
  }
}

/* Play through the initial moves of a game. If 'collect_statistics'
 * is set, store all encountered situations in the situation_table
 * array. 'collect_statistics' will be set to the color which won the
 * game.  Otherwise, see if there are any winners among the situations
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
  char *PW, *PB;
  unsigned int white_player, black_player;

  if (!sgfGetCharProperty(sgf, "PW", &PW))
    white_player = hash_string("");
  else
    white_player = hash_string(PW);

  if (!sgfGetCharProperty(sgf, "PB", &PB))
    black_player = hash_string("");
  else
    black_player = hash_string(PB);
  
  /* Call the engine to clear the board. */
  clear_board();
  
  /* Loop through the first moves_per_game moves of each game. */
  for (k = 0; k < moves_per_game && node != NULL; node = node->child) {
    if (!get_move_from_sgf(node, &m, &n, &color)) {
      if (k > 0) {
	/* something is wrong with the file */
	if (0)
	  fprintf(stderr, "move number:%d\n", k);
	return 0;
      }
      continue;
    }
    gg_assert(m >= 0 && m < board_size && n >= 0 && n <= board_size);
    hash_board(&prehash, color);
    hash_board_and_move(&posthash, color, m, n);
    if (collect_statistics != EMPTY)
      add_situation(&prehash, &posthash, collect_statistics == color, 
		    color == WHITE ? white_player : black_player);
    else
      store_pattern_if_winner(&prehash, &posthash, color, m, n);
    play_move(POS(m, n), color);
    
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
  if (!node) {
    if (0)
      fprintf(stderr, "Node error\n");
    return 0;
  }
  
  return 1;
}

/* Tests if the player has enough strength in the game to be interesting 
 * for the library
 */
  
static int
enough_strength(char *strength)
{
  int length = 0;
  int i = 0;
  int kyu = 30;
  if (player_strength >= 30)
    return 1;
  
  length = strlen(strength);
  /* check if dan or pro player */
  for (i = 0; i < length; i++)
    if (strength[i] == 'd' || strength[i] == 'D' 
	|| strength[i] == 'p' || strength[i] == 'P')
      return 1;
  
  /* get the kyu strength as an integer */
  for (i = 0; i < length; i++) {
    if (strength[i] == 'k')
      strength[i] = '\0';
    kyu = atoi(strength);
    if (kyu == 0) {
      if (player_strength >= 30)
	return 1;
      else
	return 0;
    }
  }
  
  if (kyu <= player_strength)
    return 1;

  /* not enough strength */
  return 0;
}

    
/* 
 * used by both sort_games and collect_situations to
 * check validity of a game record
 * 0 means failure for any reason
 * 1 means probably okay, without going through moves
 */
static int
check_game(SGFNode *sgf, char *sgfname)
{
  int handicap, size;
  char *WR, *BR; /* white rank */
  char thirty_kyu[] = "30k";
  char *RE;
  
  size = 19;
  if (!sgfGetIntProperty(sgf, "SZ", &size)) {
    if (WARN > 1)
      fprintf(stderr, "Warning: no SZ in sgf file %s , assuming size %d\n",
	      sgfname, size);
  }
  if (size != board_size) {
    if (WARN)
      fprintf(stderr, "Warning: wrong size of board %d in sgf file %s .\n",
	      size, sgfname);
    return 0;
  }
    
  /* No handicap games */
  if (handicap_value == 0) {
    if (sgfGetIntProperty(sgf, "HA", &handicap) && handicap > 1) {
      if (WARN)
	fprintf(stderr,
		"No handicap games allowed, sgf file %s has handicap %d\n",
		sgfname, handicap);
      return 0;
    }
  }
    
  /* Only handicap games */
  if (handicap_value > 1) {
    if (!sgfGetIntProperty(sgf, "HA", &handicap)) {
      if (WARN)
	fprintf(stderr, "Sgf file %s is not a handicap game\n", sgfname);
      return 0;
    }
      
    /* only specific handicap games */
    if (handicap_value != 10 && handicap != handicap_value) {
      if (WARN)
	fprintf(stderr,
		"Sgf file %s has wrong number of handicap stones %d\n",
		sgfname, handicap);
      return 0;
    }

    /* any reasonable handicap games */
    if (handicap_value == 10 && (handicap < 2 || handicap > 9)) {
      if (WARN)
	fprintf(stderr,
		"Sgf file %s has wrong/weird number of handicap stones %d\n",
		sgfname, handicap);
      return 0;
    }
  }

  /* Examine strength of players in the game and compare it 
   * with minimum player strength.
   */
  
  BR = thirty_kyu;
  if (!sgfGetCharProperty(sgf, "BR", &BR)) {
    if (WARN > 1)
      fprintf(stderr, "No black strength in sgf file %s assuming %s\n",
	      sgfname, BR);
  }
  if (!enough_strength(BR)) {
    if (WARN)
      fprintf(stderr, "Wrong black strength %s in sgf file %s\n", BR, sgfname);
    return 0;
  }
      
  WR = thirty_kyu;
  if (!sgfGetCharProperty(sgf, "WR", &WR)) {
    if (WARN > 1)
      fprintf(stderr, "No white strength in sgf file %s assuming %s\n",
	      sgfname, WR);
  }
  if (!enough_strength(WR)) {
    if (WARN)
      fprintf(stderr, "Wrong white strength %s in sgf file %s\n", WR, sgfname);
    return 0;
  }

  /* Must have result. */
  if (!sgfGetCharProperty(sgf, "RE", &RE)) {
    if (WARN)
      fprintf(stderr, "No result in game %s\n", sgfname);
    return 0;
  }
  
  if (strncmp(RE, "B+", 2) != 0 && strncmp(RE, "W+", 2) != 0) {
    if (WARN)
      fprintf(stderr, "Couldn't parse winner in result %s from game %s\n",
	      RE, sgfname);
    return 0;
  }
  
  /* Looks okay. */
  return 1;
}

/*
 * Sort out the games that can be used.
 */

static void
sort_games(void)
{
  int k;

  for (k = 0; k < number_of_games; k++) {
    SGFNode *sgf;
    
    /* Progress output. */
    if (k % 500 == 0)
      fprintf(stderr, "Sorting number %d, %s\n", k, sgf_names[k]);
    
    sgf = readsgffilefuseki(sgf_names[k], 0);
    
    
    if (!sgf) {
      if (WARN)
	fprintf(stderr, "Warning: Couldn't open sgf file %s number %d.\n",
		sgf_names[k], k);
      unused_games[k] = 1; /* the game could not be used */
      continue;
    }

    if (!check_game(sgf, sgf_names[k]))
      unused_games[k] = 1;
    
    /* Free memory of SGF file */
    sgfFreeNode(sgf);
  }
}


/* Play through the initial moves of all games and collect hash values
 * for the encountered situations.
 */
static void
collect_situations(void)
{
  int k;
  int winner; /* who won the game in question */
  
  init_situations();
  for (k = 0; k < number_of_games; k++) {
    SGFNode *sgf;
    char *RE;
    
    /* Progress output. */
    if (k % 500 == 0)
      fprintf(stderr, "Reading number %d, %s\n", k, sgf_names[k]);
    
    sgf = readsgffilefuseki(sgf_names[k], moves_per_game);
    
    if (!sgf) {
      if (WARN)
	fprintf(stderr, "Warning: Couldn't open sgf file %s.\n", sgf_names[k]);
      unused_games[k] = 1; /* the game could not be used */
      continue;
    }
    
    if (!check_game(sgf, sgf_names[k])) {
      unused_games[k] = 1; 
      sgfFreeNode(sgf);
      continue;
    }
    
    if (!sgfGetCharProperty(sgf, "RE", &RE)) {
      gg_assert(0);
    }

    if (strncmp(RE, "B+", 2) == 0)
      winner = BLACK;
    else if (strncmp(RE, "W+", 2) == 0)
      winner = WHITE;
    else {
      gg_assert(0);
    }
    
    if (!examine_game(sgf, winner)) {
      if (WARN)
	fprintf(stderr, "Warning: Problem with sgf file %s\n", sgf_names[k]);
      unused_games[k] = 1; /* the game could not be used */
    }
    
    /* Free memory of SGF file */
    sgfFreeNode(sgf);
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
  gg_sort(situation_table, number_of_situations, sizeof(*situation_table),
	  compare_situations2);
  
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
      frequency_table[number_of_distinct_positions].n_win = 0;
      frequency_table[number_of_distinct_positions].n_player = 0;
      number_of_distinct_positions++;
    }
    frequency_table[number_of_distinct_positions-1].n++;
    frequency_table[number_of_distinct_positions-1].n_win += situation_table[k].outcome;
    if (frequency_table[number_of_distinct_positions-1].n == 1 
	|| situation_table[k].player != situation_table[k-1].player) 
      frequency_table[number_of_distinct_positions-1].n_player++; 
  }
  
  /* Sort the frequency table, in falling order. */
  gg_sort(frequency_table, number_of_distinct_positions,
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
  winning_moves = calloc(MAX_PATTERNS_TO_EXTRACT, sizeof(*winning_moves));
  if (!winning_moves) {
    fprintf(stderr, "Fatal error, failed to allocate winning moves table.\n");
    exit(EXIT_FAILURE);
  }
  number_of_winning_moves = 0;
  
  /* Starting with the most common position, find the most common
   * moves for each position, until the number of patterns to be
   * generated is reached.
   */
  for (k = 0; k < number_of_distinct_positions; k++) {
    int index = frequency_table[k].index;
    int i;
    
    /* Build a new frequency table for the different moves in this position. */
    struct frequency move_frequencies[MAX_BOARD * MAX_BOARD];
    int number_of_moves = 0;

    /* A position must occur a minimum before we analyze and record
     * the moves from it.
     */
    if (frequency_table[k].n < min_position_freq)
      break;

    for (i = index; ;i++) {
      if (i == number_of_situations
	  || (i > index
	      && compare_positions(&situation_table[i],
				   &situation_table[i-1]) != 0))
	break;
      
      if (i == index || compare_situations(&situation_table[i],
					   &situation_table[i-1]) != 0) {
	move_frequencies[number_of_moves].index = i;
	move_frequencies[number_of_moves].n = 0;
	move_frequencies[number_of_moves].n_win = 0;
	move_frequencies[number_of_moves].n_player = 0;
	number_of_moves++;
      }
      move_frequencies[number_of_moves-1].n++;
      move_frequencies[number_of_moves-1].n_win += situation_table[i].outcome;
      
      if (move_frequencies[number_of_moves-1].n == 1 
	  || situation_table[i].player != situation_table[i-1].player) 
	move_frequencies[number_of_moves-1].n_player++;
    }
    
    /* Sort the moves, in falling order. */
    gg_sort(move_frequencies, number_of_moves,
	    sizeof(*move_frequencies), compare_frequencies2);
    
    /* Debug output. */
    if (0) {
      for (i = 0; i < number_of_moves; i++) {
	fprintf(stderr, "%4d %3d %4d\n", index, move_frequencies[i].n,
		move_frequencies[i].index);
      }
    }
    
    /* Add the moves to the list of winners. */
    for (i = 0; i < number_of_moves; i++) {
      /* This is where the cut-off of moves is decided 
       * based on popularity from command line arguments.
       */
	
      if (0.01 * min_move_percent*move_frequencies[0].n_player
	  > move_frequencies[i].n_player 
	  || move_frequencies[i].n_player < min_move_freq) {
	winning_moves[number_of_winning_moves].index = -1;
	winning_moves[number_of_winning_moves].pre =
	  situation_table[frequency_table[k].index].pre.values[0];
	winning_moves[number_of_winning_moves].position_frequency =
	  frequency_table[k].n;
	winning_moves[number_of_winning_moves].n_player = 0;	
	winning_moves[number_of_winning_moves].move_frequency = 0;
	winning_moves[number_of_winning_moves].position_success =
	  frequency_table[k].n_win;
	winning_moves[number_of_winning_moves].move_success = 0;
	
	while (i < number_of_moves) {
	  gg_assert(0.01 * min_move_percent*move_frequencies[0].n_player 
		     > move_frequencies[i].n_player
		     || move_frequencies[i].n_player < min_move_freq);
	  gg_assert(situation_table[move_frequencies[i].index].pre.values[0]
		    == winning_moves[number_of_winning_moves].pre);
	  winning_moves[number_of_winning_moves].n_player +=
	    move_frequencies[i].n_player;	
	  winning_moves[number_of_winning_moves].move_frequency += 	
	    move_frequencies[i].n;
	  winning_moves[number_of_winning_moves].move_success +=
	    move_frequencies[i].n_win;
	  i++;
	}
	number_of_winning_moves++;
	continue;
      }
      
      winning_moves[number_of_winning_moves].index = move_frequencies[i].index;
      winning_moves[number_of_winning_moves].pre =
	situation_table[frequency_table[k].index].pre.values[0];
      winning_moves[number_of_winning_moves].position_frequency =
	frequency_table[k].n;
      winning_moves[number_of_winning_moves].move_frequency =
	move_frequencies[i].n;
      winning_moves[number_of_winning_moves].n_player =
	move_frequencies[i].n_player;	

      winning_moves[number_of_winning_moves].position_success =
	frequency_table[k].n_win;
      winning_moves[number_of_winning_moves].move_success =
	move_frequencies[i].n_win;
      number_of_winning_moves++;
      
      if (number_of_winning_moves == MAX_PATTERNS_TO_EXTRACT)
	break;
    }
    
    if (number_of_winning_moves == MAX_PATTERNS_TO_EXTRACT)
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
  SGFNode *sgf;
  for (k = 0; k < number_of_games; k++) {
    
    /* Progress output. */
    if (k % 500 == 0)
      fprintf(stderr, "Generating number %d, %s\n", k, sgf_names[k]);
    
    /* Check if this game is a valid game. */
    if (unused_games[k]) {
      if (0)
	fprintf(stderr, "Not used\n");
      continue;
    }
    
    sgf = readsgffilefuseki(sgf_names[k], moves_per_game);
    if (!sgf) {
      fprintf(stderr, "Warning: Couldn't open sgf file %s.\n", sgf_names[k]);
      continue;
    }
    
    examine_game(sgf, 0);
    
    /* Free memory of SGF file. */
    sgfFreeNode(sgf);
  }
}

/* Print the winning patterns in patterns.db format on stdout. */
static void
print_patterns(void)
{
  int k, l;
  int m, n;
  double chisq = 0.0;
  int df = 0;
  unsigned int pre = situation_table[winning_moves[0].index].pre.values[0];
  int first_in_set = 0;
  gg_assert(winning_moves[0].index != -1);
  l = 1;
  for (k = 0; k < number_of_winning_moves; k++) {
    /* Do not print erroneous patterns. */
    if (winning_moves[k].pattern[0][0] != '\0'
	|| winning_moves[k].index == -1) {
      double grand_sum = winning_moves[k].position_frequency;
      double grand_wins = winning_moves[k].position_success;
#if 0
      double grand_losses = grand_sum - grand_wins;
#endif
      double row_sum = winning_moves[k].move_frequency;
      double wins =  winning_moves[k].move_success;
      double losses = row_sum - wins;
      double expect_wins = row_sum*grand_wins/grand_sum;
      double expect_losses = row_sum - expect_wins;
      /* We're _not_ using a Yates corrected chisquare.
       * Two reasons: 1. It's never correct for > 2x2 table
       *              2. Our marginals are sampled, not fixed, so
       *	         Yates and usual Fisher exact are wrong distribution.
       *  Straight chi squared is best.
       */
      double dchisq = 0.0;
      /* This was Yates line. It's wrong. */
#if 0
      if (expect_wins > 0.0)
	dchisq += pow(gg_abs(wins - expect_wins) - 0.5, 2) / expect_wins;
#endif
      
      if (expect_wins > 0.0)
	dchisq += pow(wins - expect_wins, 2) / expect_wins;
      if (expect_losses > 0.0)
	dchisq  += pow(losses - expect_losses, 2) / expect_losses;
      
      gg_assert(winning_moves[k].index == -1
		|| (situation_table[winning_moves[k].index].pre.values[0]
		    == winning_moves[k].pre));
      
      /* Did we just finish a set? If so, print totals and reset. */
      if (winning_moves[k].pre != pre) { 
	/* p-value is 1 - incomplete gamma function(d.o.f/2, chisq/2)
	 * variable df is number of moves, actual d.o.f is df-1
	 * k is referring to the pattern _after_ the set we just completed.
	 */
	printf("\n### Summary of pattern pre 0x%08x\n### N Chi_squared df: %d %g %d ",
	       pre, winning_moves[k-1].position_frequency, chisq, df - 1);
	/* and array is indexed at zero for d.o.f = 1... */
	if (df-1 < 1)
	  printf("NS\n\n");
	else if (df - 1 < (int) (sizeof(chisquarecrit05) / sizeof(double))
		 && chisq > chisquarecrit05[df-2]) { 
	  /* The overall result is significant at 5%. In this case, at
	   * least some authorities will allow us to examine several
	   * individual contrasts w/o futher correction. We compare
	   * every pair of moves, which is perhaps too many, but the
	   * purpose is to give the human expert (who would in any
	   * case be required to examine the output) some sense of
	   * where the differences are. Something like a Bonferroni
	   * correction could result in a significant test overall,
	   * but no significant contrasts, which is obviously
	   * nonsense. The significance of the overall result must
	   * come from somewhere.
	   */
	  int m, n;
	  if (chisq > chisquarecrit001[df-2]) 
	    printf("!!! p < 0.001\n");
	  else if (chisq > chisquarecrit01[df-2]) 
	    printf("!!! p < 0.01\n");
	  else
	    printf("!!! p < 0.05\n");
	  for (m = first_in_set; m < k; m++) {
	    for (n = m + 1; n < k; n++) {
	      double grand_sum = (winning_moves[m].move_frequency
				  + winning_moves[n].move_frequency);
	      double grand_wins = (winning_moves[m].move_success
				   + winning_moves[n].move_success);
#if 0
	      double grand_losses = grand_sum - grand_wins;
#endif
	      double row_sum_m = winning_moves[m].move_frequency;
	      double row_sum_n = winning_moves[n].move_frequency;

	      double wins_m =  winning_moves[m].move_success;
	      double losses_m = row_sum_m - wins_m;
	      double wins_n =  winning_moves[n].move_success;
	      double losses_n = row_sum_n - wins_n;

	      double expect_wins_m = row_sum_m * grand_wins/grand_sum;
	      double expect_losses_m = row_sum_m - expect_wins_m;
	      double expect_wins_n = row_sum_n * grand_wins/grand_sum;
	      double expect_losses_n = row_sum_n - expect_wins_n;
	      double dchisq_m = 0.0;
	      double dchisq_n = 0.0;
	      if (expect_wins_m > 0.0)
		dchisq_m += pow(wins_m - expect_wins_m, 2) / expect_wins_m;
	      if (expect_losses_m > 0.0)
		dchisq_m  += pow(losses_m - expect_losses_m, 2) / expect_losses_m;
	      if (expect_wins_n > 0.0)
		dchisq_n += pow(wins_n - expect_wins_n, 2) / expect_wins_n;
	      if (expect_losses_n > 0.0)
		dchisq_n  += pow(losses_n - expect_losses_n, 2) / expect_losses_n;
	      /* We demand at least N=6. Nonsense with smaller N. */
	      if (dchisq_m + dchisq_n > chisquarecrit05[0] && grand_sum > 5) {
		printf("#### 0x%08x %c 0x%08x (p < 0.05) chisq = %g N = %g\n", 
		       situation_table[winning_moves[m].index].post.values[0],
		       (1.0 * wins_m / row_sum_m
			> 1.0 * wins_n / row_sum_n) ? '>' : '<',
		       situation_table[winning_moves[n].index].post.values[0],
		       dchisq_m + dchisq_n, grand_sum);
	      }
	    }
	  }
	  printf("\n\n");
	}
	else if (df-1 < (int) (sizeof(chisquarecrit10) / sizeof(double))
		 && chisq > chisquarecrit10[df - 2])
	  printf("??? p < 0.10\n\n");
	else if (!(df - 1 < (int) (sizeof(chisquarecrit05) / sizeof(double))))
	  printf("df out of range...\n\n");
	else
	  printf("NS\n\n");
	
	pre = winning_moves[k].pre;
#if 0
	pre = situation_table[winning_moves[k].index].pre.values[0];
#endif
	first_in_set = k;
	chisq = 0.0;
	df = 0;
      }
      /* increment totals */
      chisq += dchisq;
      df++;
      
      if (winning_moves[k].index == -1) {
	printf("# Unpopular moves\n");
	printf("# pre: 0x%08x\n", winning_moves[k].pre);
	printf("# post: could be various\n");
	printf("# frequency: %d/%d\n", 
	       winning_moves[k].move_frequency,
	       winning_moves[k].position_frequency);
	printf("# unique players: %d\n", winning_moves[k].n_player);
	printf("# wins: %d/%d\n\n", 
	       winning_moves[k].move_success,
	       winning_moves[k].position_success);
	printf("# success: %.1f%% vs %.1f%% for this position overall, dchisq = %g\n\n", 
	       100.0 * winning_moves[k].move_success / winning_moves[k].move_frequency,
	       100.0 * winning_moves[k].position_success / winning_moves[k].position_frequency,
	       dchisq);
      }
      else {
	printf("Pattern F-H%d-%d\n", handicap_value, l);
	printf("# pre : 0x%08x\n",
	       situation_table[winning_moves[k].index].pre.values[0]);
	printf("# post: 0x%08x\n",
	       situation_table[winning_moves[k].index].post.values[0]);
	printf("# frequency: %d/%d\n", winning_moves[k].move_frequency,
	       winning_moves[k].position_frequency);
	printf("# unique players: %d\n", winning_moves[k].n_player);
	printf("# wins: %d/%d\n\n", winning_moves[k].move_success,
	       winning_moves[k].position_success);
	printf("# success: %.1f%% vs %.1f%% for this position overall, dchisq = %g\n\n", 
	       100.0 * winning_moves[k].move_success / winning_moves[k].move_frequency,
	       100.0 * winning_moves[k].position_success / winning_moves[k].position_frequency,
	       dchisq);
	
	printf("+");
	for (n = 0; n < board_size; n++)
	  printf("-");

	printf("+\n");
	for (m = 0; m < board_size; m++) {
	  printf("|");
	  for (n = 0; n < board_size; n++) {
	    if (winning_moves[k].pattern[m][n] == '\0') {
	      fprintf(stderr, "Something wrong in print pattern\n");
	      printf(".");
	    }
	    else
	      printf("%c", winning_moves[k].pattern[m][n]);
	  }
	  printf("|\n");
	}
	
	printf("+");
	for (n = 0; n < board_size; n++)
	  printf("-");
	printf("+");
	
	printf("\n\n:8,-,value(%d)\n\n\n", winning_moves[k].n_player);
	l++;
      }
    }
    else {
      fprintf(stderr,
	      "Skipping pattern pre 0x%08x post 0x%08x, stats may be wrong...\n", 
	      situation_table[winning_moves[k].index].pre.values[0], 
	      situation_table[winning_moves[k].index].post.values[0]);
    }
  }
}

int
main(int argc, char *argv[])
{
  int number_of_unused_games = 0;
  int i = 0;
  
  /* Check number of arguments. */
  if (argc < 10) {
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
  
  handicap_value = atoi(argv[4]);
  if (handicap_value < 0 || handicap_value > 10)
    fprintf(stderr, "Warning: unusual handicap value: %d.\n",
	    handicap_value);
  
  player_strength = atoi(argv[5]);
  if (player_strength < 0 || player_strength > 30)
    fprintf(stderr, "Warning: wrong lowest strength: %d.\n",
	    player_strength);
  
  half_board_patterns = atoi(argv[6]);
  if (half_board_patterns != 0 && half_board_patterns != 1) {
    fprintf(stderr,
	    "Warning: incorrect half_board_flag (0 or 1). Setting the value to 0.\n");
    half_board_patterns = 0;
  }

  min_position_freq = atoi(argv[7]);
  if (min_position_freq < 1) {
    fprintf(stderr, "Warning: setting min_position_freq to 1\n");
    min_position_freq = 1;
  }

  min_move_percent = atof(argv[8]);
  if (min_move_percent < 0. || min_move_percent > 100.) {
    fprintf(stderr, "Warning: strange min_move_percent %g, setting to 1%%\n", 
	    min_move_percent);
    min_move_percent = 1.0;
  }

  min_move_freq = atoi(argv[9]);
  if (min_move_freq < 0)
    fprintf(stderr, "Warning: strange min_move_freq %d\n", min_move_freq);

  /* Count the number of sgf files. */
  number_of_games = read_sgf_filenames(argv[1], NULL);
  
  /* Allocate space for the list of unused files. */
  unused_games = calloc(number_of_games, sizeof(*unused_games));
  if (unused_games == NULL) {
    fprintf(stderr, "Fatal error, failed to allocate memory.\n");
    exit(EXIT_FAILURE);
  }
  
  /* Allocate space for the list of sgf file names. */
  sgf_names = calloc(number_of_games, sizeof(*sgf_names));
  if (sgf_names == NULL) {
    fprintf(stderr, "Fatal error, failed to allocate memory.\n");
    exit(EXIT_FAILURE);
  }
  
  /* Read the list of sgf files and store in memory. */
  read_sgf_filenames(argv[1], sgf_names);
  
  /* Save memory by sorting out the games that can be used first */
  if (argv[10] != NULL) {
    fprintf(stderr, "Starting game sort\n");
    sort_games();
    fprintf(stderr, "Starting game writes\n");
    write_sgf_filenames(argv[10], sgf_names);
  }
  else {
    /* Build tables of random numbers for Zobrist hashing. */
    init_zobrist_numbers();
    
    /* Play through the initial moves of all games and collect hash values
     * for the encountered situations.
     */
    collect_situations();
    fprintf(stderr, "collect OK.\n");
    
    /* Find the most common positions and moves, for which we want to
     * generate patterns.
     */
    analyze_statistics();
    fprintf(stderr, "analyze OK.\n");
    
    /* Generate patterns from the chosen positions and moves.
     */
    generate_patterns();
    fprintf(stderr, "generate OK.\n");

    printf("attribute_map value_only\n\n\n");
    printf("# ");
    for (i = 0; i < argc; i++)
      printf("%s ", argv[i]);
    printf("\n\n\n");

    /* Write the patterns to stdout in patterns.db format.
     */
    print_patterns();
    
    /* Tell the user everything worked out fine */
    fprintf(stderr, "The pattern database was produced with no errors.\n");

    for (i = 0; i < number_of_games; i++)
      if (unused_games[i])
	number_of_unused_games++;

    fprintf(stderr, "Out of %d games, %d were not used.\n", 
	    number_of_games, number_of_unused_games);
  }
  
  return 0;
}

/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
