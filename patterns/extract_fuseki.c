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
#include "gg_utils.h"
#include "random.h"

#define USAGE "\n\
Usage: extract_fuseki files boardsize moves patterns handicap strength half_board [output file]\n\
files:     The name of a file listing sgf files to examine,\n\
           one filename per line.\n\
boardsize: Only consider games with this size.\n\
moves:     Number of moves considered in each game.\n\
patterns:  Number of patterns to generate.\n\
handicap:  0 - no handicap, 1 - any game, 2-9 - two to nine handicap stones\n\
           10 any handicap game\n\
strength:  The lowest strength of the players (1k-30k)\n\
half_board: 0 - full board patterns, 1 - half board patterns\n\
output file: Optional (if this exists, extract_fuseki will sort the games instead)\n\
"

/* Maximum length of sgf filename. */
#define BUFSIZE 1000

/* Number of moves to consider in each game, given as argument.*/
int moves_per_game;

/* Flag checking the setting for generating half board patterns */
int half_board_patterns = 0;

/* Maximum number of patterns to generate, given as argument.*/
int patterns_to_extract;

/* Handicap value, given as argument.*/
int handicap_value;

/* Lowest strength, given as argument.*/
int player_strength;

/* Number of games to analyze. */
int number_of_games;

/* The number of games that could not be used for some reason. */
int *unused_games;

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

/*
 * Write the files that are sorted to a specific file
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
	/* Delete newline character. PC only */
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
      /* fprintf(stderr, "Warning: white stone added.\n"); */
      return 0;
      break;
      
    case SGFPL:
      /* fprintf(stderr, "Warning: PL property encountered.\n"); */
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

/*
 * find_region answers in what region the move is
 * there are 9 regions, corners, sides and center
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
    if (compare_situations(&situation_table[winning_moves[k].index],
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
	    fprintf(stderr, "Error in store_pattern_if_winner: %d\n",k);
	    winning_moves[k].pattern[i][j] = '.';
	  }
	}
      winning_moves[k].pattern[m][n] = '*';
      /* add ? in areas far away from the move */
      if (half_board_patterns == 1 && move_number > 3 && board_size == 19)
        region = find_region(m,n);
      if (region != 8) {
        for (i = 0; i < board_size; i++) {
          for (j = 0; j < board_size; j++) {
            if (region == 0) {
              if (i + j > 23)
     	        winning_moves[k].pattern[i][j] = '?';
     	    }
            else if (region == 1) {
              if ((i - j) > 5)
 	        winning_moves[k].pattern[i][j] = '?';
 	    }
            else if (region == 2) {
              if ((i + board_size - j) < 14)
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
    if (!get_move_from_sgf(node, &m, &n, &color)) {
      if (k > 0) {
	/* something is wrong with the file */
	if (0)
	  fprintf(stderr, "move number:%d\n",k);
	return 0;
      }
      continue;
    }
    gg_assert(m >= 0 && m < board_size && n >= 0 && n <= board_size);
    hash_board(&prehash, color);
    hash_board_and_move(&posthash, color, m, n);
    if (collect_statistics)
      add_situation(&prehash, &posthash);
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
  /* check if dan player */
  for (i = 0; i < length; i++)
    if (strength[i] == 'd')
      return 1;
  
  /* get the kyu strength as an integer */
  for (i = 0; i < length; i++) {
    if (strength[i] == 'k')
      strength[i] = '\0';
    kyu = atoi(strength);
  }
  
  if (kyu <= player_strength)
    return 1;

  /* not enough strength */
  return 0;
}

/*
 * Sort out the games that can be used
 */

static void
sort_games(void)
{
  int k;
  int handicap;
  char *WR; /* white rank */
  char *BR; /* black rank */
  
  for (k = 0; k < number_of_games; k++) {
    int size;
    SGFNode *sgf;
    
    /* Progress output. */
    if (k % 500 == 0)
      fprintf(stderr, "Sorting number %d, %s\n", k, sgf_names[k]);
    
    sgf = readsgffilefuseki(sgf_names[k], 0);
    
    if (!sgf) {
      if (0)
	fprintf(stderr, "Warning: Couldn't open sgf file %s.\n", sgf_names[k]);
      unused_games[k] = 1; /* the game could not be used */
      continue;
    }
    
    else if (!sgfGetIntProperty(sgf, "SZ", &size) || size != board_size) {
      if (0)
	fprintf(stderr, "Warning: wrong size of board %d.\n", size);
      unused_games[k] = 1; /* the game could not be used */
      continue; /* Board of wrong size, ignore the game. */
    }
    
    /* No handicap games */
    else if (handicap_value == 0) {
      if (sgfGetIntProperty(sgf, "HA", &handicap) && handicap > 1) {
	if (0)
	  fprintf(stderr, "No handicap games allowed %d\n", handicap);
	unused_games[k] = 1; /* the game could not be used */
	continue;
      }
    }
    
    /* Only handicap games */
    else if (handicap_value > 1) {
      if (!sgfGetIntProperty(sgf, "HA", &handicap)) {
	if (0)
	  fprintf(stderr, "Not a handicap game %d\n", handicap);
	unused_games[k] = 1; /* the game could not be used */
	continue;
      }
      
      /* only specific handicap games */
      else if (handicap_value != 10 && handicap != handicap_value) {
	if (0)
	  fprintf(stderr, "Wrong number of handicap stones %d\n", handicap);
	unused_games[k] = 1; /* the game could not be used */
	continue;
      }
    }
    
    if (unused_games[k] == 0) {
      /* examine strength of players in the game and compare it 
       * with minimum player strength 
       */
      if (sgfGetCharProperty(sgf, "BR", &BR) && !enough_strength(BR)) {
	if (0)
	  fprintf(stderr, "Wrong strength: %s.\n", BR);
	unused_games[k] = 1; /* the game could not be used */
	continue;
      }
      
      /* examine strength of players in the game and compare it with 
       * minimum player strength */
      else if (sgfGetCharProperty(sgf, "WR", &WR) && !enough_strength(WR)) {
	if (0)
	  fprintf(stderr, "Wrong strength: %s.\n", WR);
	unused_games[k] = 1; /* the game could not be used */
	continue;
      }
    }
    
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
  int handicap;
  char *WR; /* white rank */
  char *BR; /* black rank */
  
  init_situations();
  for (k = 0; k < number_of_games; k++) {
    int size;
    SGFNode *sgf;
    
    /* Progress output. */
    if (k % 500 == 0)
      fprintf(stderr, "Reading number %d, %s\n", k, sgf_names[k]);
    
    sgf = readsgffilefuseki(sgf_names[k], moves_per_game);
    
    if (!sgf) {
      if (0)
	fprintf(stderr, "Warning: Couldn't open sgf file %s.\n", sgf_names[k]);
      unused_games[k] = 1; /* the game could not be used */
      continue;
    }
    
    else if (!sgfGetIntProperty(sgf, "SZ", &size) || size != board_size) {
      if (0)
	fprintf(stderr, "Warning: wrong size of board %d.\n", size);
      unused_games[k] = 1; /* the game could not be used */
      continue; /* Board of wrong size, ignore the game. */
    }
    
    /* No handicap games */
    else if (handicap_value == 0) {
      if (sgfGetIntProperty(sgf, "HA", &handicap) && handicap > 1) {
	if (0)
	  fprintf(stderr, "No handicap games allowed %d\n", handicap);
	unused_games[k] = 1; /* the game could not be used */
	continue;
      }
    }
    
    /* Only handicap games */
    else if (handicap_value > 1) {
      if (!sgfGetIntProperty(sgf, "HA", &handicap)) {
	if (0)
	  fprintf(stderr, "Not a handicap game %d\n", handicap);
	unused_games[k] = 1; /* the game could not be used */
	continue;
      }
      
      /* only specific handicap games */
      else if (handicap_value != 10 && handicap != handicap_value) {
	if (0)
	  fprintf(stderr, "Wrong number of handicap stones %d\n", handicap);
	unused_games[k] = 1; /* the game could not be used */
	continue;
      }
    }
    
    /* examine strength of players in the game and compare it with 
     * minimum player strength */
    if (sgfGetCharProperty(sgf, "BR", &BR) && !enough_strength(BR)) {
      if (0)
	fprintf(stderr, "Wrong strength: %s.\n", BR);
      unused_games[k] = 1; /* the game could not be used */
      continue;
    }
    /* examine strength of players in the game and compare it with 
     * minimum player strength */
    else if (sgfGetCharProperty(sgf, "WR", &WR) && !enough_strength(WR)) {
      if (0)
	fprintf(stderr, "Wrong strength: %s.\n", WR);
      unused_games[k] = 1; /* the game could not be used */
      continue;
    }
    
    if (!examine_game(sgf, 1)) {
      if (0)
	fprintf(stderr, "Warning: Problem with sgf file %s.\n", sgf_names[k]);
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
	number_of_moves++;
      }
      move_frequencies[number_of_moves-1].n++;
    }
    
    /* Sort the moves, in falling order. */
    gg_sort(move_frequencies, number_of_moves,
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
      /* This is where the cut-off of moves is decided */
      if (10 * move_frequencies[i].n < move_frequencies[0].n
	  && move_frequencies[i].n < 10)
	break;
      /* Take away any move that hasn't been made by at least 2 people */
      if (move_frequencies[i].n < 2)
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
  SGFNode *sgf;
  for (k = 0; k < number_of_games; k++) {
    
    /* Progress output. */
    if (k % 500 == 0)
      fprintf(stderr, "Generating number %d, %s\n", k, sgf_names[k]);
    
    /* Check if this game is a valid game */
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
    
    (void) examine_game(sgf, 0);
    
    /* Free memory of SGF file */
    sgfFreeNode(sgf);
  }
}

/* Print the winning patterns in patterns.db format on stdout. */
static void
print_patterns(void)
{
  int k, l;
  int m, n;
  l = 1;
  for (k = 0; k < number_of_winning_moves; k++) {
    
    /* do not print errornous patterns */
    if (winning_moves[k].pattern[0][0] != '\0') {
      printf("Pattern Fuseki%d\n", l);
      printf("# %d/%d\n\n", 
	     winning_moves[k].move_frequency,
	     winning_moves[k].position_frequency);
      printf("+");
      for (n = 0; n < board_size; n++) {
	printf("-");
      }
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
      for (n = 0; n < board_size; n++) {
	printf("-");
      }
      printf("+");
      printf("\n\n:8,-,value(%d)\n\n\n", winning_moves[k].move_frequency);
      l++;
    }
  }
}

int
main(int argc, char *argv[])
{
  int number_of_unused_games = 0;
  int i = 0;
  
  /* Check number of arguments. */
  if (argc < 6) {
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
  
  handicap_value = atoi(argv[5]);
  if (handicap_value < 0 || handicap_value > 2)
    fprintf(stderr, "Warning: wrong handicap value: %d.\n",
	    handicap_value);
  
  player_strength = atoi(argv[6]);
  if (player_strength < 0 || player_strength > 30)
    fprintf(stderr, "Warning: wrong lowest strength: %d.\n",
	    player_strength);
  
  half_board_patterns = atoi(argv[7]);
  if (half_board_patterns != 0 && half_board_patterns != 1) {
    fprintf(stderr, "Warning: incorrect half_board_flag (0 or 1). Setting the value to 0.\n");
    half_board_patterns = 0;
  }
  /* Count the number of sgf files. */
  number_of_games = read_sgf_filenames(argv[1], NULL);
  
  /* Allocate space for the list of unused files. */
  unused_games = calloc(number_of_games, sizeof(int));
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
  (void) read_sgf_filenames(argv[1], sgf_names);
  
  /* Save memory by sorting out the games that can be used first */
  if (argv[8] != NULL) {
    sort_games();
    write_sgf_filenames(argv[8], sgf_names);
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
