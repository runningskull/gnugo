/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * This is GNU Go, a Go program. Contact gnugo@gnu.org, or see       *
 * http://www.gnu.org/software/gnugo/ for more information.          *
 *                                                                   *
 * Copyright 1999, 2000, 2001, 2002, 2003 and 2004                   *
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

/*
 * The code in this file implements persistent caching.
 *
 * The idea is that reading results are stored together with an
 * "active area", i.e. the part of the board having an effect on the
 * reading result. Thus if only moves outside of the active area has
 * been played since the result was stored, it can be reused.
 *
 * The active areas are not known exactly but are estimated
 * heuristically. The effects are that too large an active area
 * reduces the efficiency of the caching scheme while too small an
 * active area may cause an incorrect read result to be retrieved from
 * the cache.
 *
 * Persistent caching has so far been implemented for tactical reading,
 * owl reading, connection reading and break-in reading (with semeai
 * reading planned for the future).
 *
 * The hotspot functions are intended to locate where the most
 * expensive reading of either type is going on. This information can
 * be estimated from the contents of the persistent caches since the
 * most expensive readings are stored there with full information of
 * spent reading nodes, involved strings or dragons, and active areas.
 */

#include "gnugo.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "liberty.h"
#include "cache.h"


/* ================================================================ */
/*                     Data structures                              */
/* ================================================================ */

/* Used in active area. */
#define HIGH_LIBERTY_BIT  4
#define HIGH_LIBERTY_BIT2 8


#define MAX_READING_CACHE_DEPTH 5
#define MAX_READING_CACHE_SIZE 100

#define MAX_OWL_CACHE_DEPTH 0
#define MAX_OWL_CACHE_SIZE 150

#define MAX_CONNECTION_CACHE_DEPTH 5
#define MAX_CONNECTION_CACHE_SIZE 100

#define MAX_BREAKIN_CACHE_DEPTH 1
#define MAX_BREAKIN_CACHE_SIZE 150

#define MAX_CACHE_DEPTH 	5


/* We use the same data structure for all of the caches. Some of the entries
 * below are unused for some of the caches.
 */
struct persistent_cache_entry {
  int boardsize;
  int movenum;
  char board[BOARDMAX];
  int stack[MAX_CACHE_DEPTH];
  int move_color[MAX_CACHE_DEPTH];
  enum routine_id routine;
  int apos; /* first input coordinate */
  int bpos; /* second input coordinate */
  int cpos; /* third input coordinate */
  Hash_data goal_hash; /* hash of the goal in break-in reading */
  int result;
  int result_certain;
  int remaining_depth;
  int node_limit; 
  int move; /* first result coordinate */
  int move2; /* second result coordinate */
  int cost; /* Usually no. of tactical nodes spent on this reading result. */
  int score; /* Heuristic guess of the worth of the cache entry. */
};

/* Callback function that implements the computation of the active area.
 * This function has to be provided by each cache.
 */
typedef void (*compute_active_area_fn)(struct persistent_cache_entry *entry,
				       const char goal[BOARDMAX],
				       int goal_color);

struct persistent_cache {
  const int max_size; /* Size of above array. */
  const int max_stackp; /* Don't store positions with stackp > max_stackp. */
  const float age_factor; /* Reduce value of old entries with this factor. */
  const char *name; /* For debugging purposes. */
  const compute_active_area_fn compute_active_area;
  struct persistent_cache_entry* table; /* Array of actual results. */
  int current_size; /* Current number of entries. */
  int last_purge_position_number;
};

static void compute_active_owl_area(struct persistent_cache_entry *entry,
				    const char goal[BOARDMAX],
				    int goal_color);
static void compute_active_reading_area(struct persistent_cache_entry *entry,
					const char reading_shadow[BOARDMAX],
					int dummy);
static void compute_active_connection_area(struct persistent_cache_entry *entry,
					   const char
					   	connection_shadow[BOARDMAX],
					   int goal_color);
static void compute_active_breakin_area(struct persistent_cache_entry *entry,
				        const char breakin_shadow[BOARDMAX],
				        int dummy);

struct persistent_cache reading_cache =
  { MAX_READING_CACHE_SIZE, MAX_READING_CACHE_DEPTH, 1.0,
    "reading cache", compute_active_reading_area,
    NULL, 0, -1 };

struct persistent_cache connection_cache =
  { MAX_CONNECTION_CACHE_SIZE, MAX_CONNECTION_CACHE_DEPTH, 1.0,
    "connection cache", compute_active_connection_area,
    NULL, 0, -1 };

struct persistent_cache breakin_cache =
  { MAX_BREAKIN_CACHE_SIZE, MAX_BREAKIN_CACHE_DEPTH, 0.75,
    "breakin cache", compute_active_breakin_area,
    NULL, 0, -1 };

struct persistent_cache owl_cache =
  { MAX_OWL_CACHE_SIZE, MAX_OWL_CACHE_DEPTH, 1.0,
    "owl cache", compute_active_owl_area,
    NULL, 0, -1 };


/* ================================================================ */
/* Common helper functions.   		                            */


static void
draw_active_area(char board[BOARDMAX], int apos)
{
  int i, j, ii;
  int c = ' ';
  int cw = (apos == NO_MOVE) ? 'O' : 'o';
  int cb = (apos == NO_MOVE) ? 'X' : 'x';

  start_draw_board();
  
  for (i = 0; i < board_size; i++) {
    ii = board_size - i;
    fprintf(stderr, "\n%2d", ii);
    
    for (j = 0; j < board_size; j++) {
      int pos = POS(i, j);
      if (board[pos] == EMPTY)
	c = '.';
      else if (board[pos] == WHITE)
	c = cw;
      else if ((board[pos] & 3) == WHITE)
	c = 'O';
      else if (board[pos] == BLACK)
	c = cb;
      else if ((board[pos] & 3) == BLACK)
	c = 'X';
      if (board[pos] == GRAY)
	c = '?';
      
      if (pos == apos)
	fprintf(stderr, "[%c", c);
      else if (j > 0 && POS(i, j-1) == apos)
	fprintf(stderr, "]%c", c);
      else
	fprintf(stderr, " %c", c);
    }
    
    fprintf(stderr, " %d", ii);
  }

  end_draw_board();
}


/* Returns 1 if the stored board is compatible with the current board,
 * 0 otherwise.
 */
static int
verify_stored_board(char p[BOARDMAX])
{
  int pos;
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (!ON_BOARD(pos))
      continue;
    else if (p[pos] == GRAY)
      continue;
    else if ((p[pos] & 3) != board[pos])
      return 0;
    else if (!(p[pos] & (HIGH_LIBERTY_BIT | HIGH_LIBERTY_BIT2)))
      continue;
    else if (((p[pos] & HIGH_LIBERTY_BIT) && countlib(pos) <= 4)
             || (p[pos] & HIGH_LIBERTY_BIT2 && countlib(pos) <= 3))
      return 0;
  }
  
  return 1;
}


/* Prints out all relevant information for a cache entry, and prints
 * a board showing the active area.
 */
static void
print_persistent_cache_entry(struct persistent_cache_entry *entry)
{
  int r;

  gprintf("%omovenum         = %d\n",  entry->movenum);
  gprintf("%oscore	     = %d\n",  entry->score);
  gprintf("%ocost	     = %d\n",  entry->cost);
  gprintf("%oroutine         = %s\n",  routine_id_to_string(entry->routine));
  gprintf("%oapos            = %1m\n", entry->apos);
  if (entry->bpos != NO_MOVE)
    gprintf("%obpos          = %1m\n", entry->bpos);
  if (entry->cpos != NO_MOVE)
    gprintf("%ocpos            = %1m\n", entry->cpos);
  gprintf("%oresult          = %s\n",  result_to_string(entry->result));
  if (entry->result_certain != -1)
    gprintf("%oresult_certain  = %d\n",  entry->result_certain);
  if (entry->node_limit != -1)
    gprintf("%onode_limit      = %d\n",  entry->node_limit);
  if (entry->move != NO_MOVE)
    gprintf("%omove            = %1m\n", entry->move);
  if (entry->move2 != NO_MOVE)
    gprintf("%omove2           = %1m\n", entry->move2);
  
  for (r = 0; r < MAX_CACHE_DEPTH; r++) {
    if (entry->stack[r] == 0)
      break;
    gprintf("%ostack[%d]      = %C %1m\n", r, entry->move_color[r],
	    entry->stack[r]);
  }

  draw_active_area(entry->board, entry->apos);
}

/* To keep GCC happy and have the function included in the
 * gnugo executable. Can be used from gdb.
 */
void print_persistent_cache(struct persistent_cache *cache);

void
print_persistent_cache(struct persistent_cache *cache)
{
  int k;
  gprintf("Entire content of %s:\n", cache->name);
  for (k = 0; k < cache->current_size; k++)
    print_persistent_cache_entry(cache->table + k);
}


/* ================================================================ */
/* Core functions.						    */
/* ================================================================ */

/* The static functions below implement the core infrastructure of the
 * persistent caches. Each cache only has to provide a function
 * computing the active area, and wrappers around the search_.. and store_..
 * function below.
 */

/* Remove persistent cache entries which are no longer compatible with
 * the board. For efficient use of the cache, it's recommended to call
 * this function once per move, before starting the owl reading. It's
 * not required for correct operation though. 
 */
static void
purge_persistent_cache(struct persistent_cache *cache)
{
  int k;
  int r;
  gg_assert(stackp == 0);

  /* Never do this more than once per move. */
  if (cache->last_purge_position_number == position_number)
    return;
  else
    cache->last_purge_position_number = position_number;

  for (k = 0; k < cache->current_size; k++) {
    int played_moves = 0;
    int entry_ok = 1;
    struct persistent_cache_entry *entry = &(cache->table[k]);

    if (entry->boardsize != board_size)
      entry_ok = 0;
    else {
      for (r = 0; r < MAX_CACHE_DEPTH; r++) {
	int apos = entry->stack[r];
	int color = entry->move_color[r];
	if (apos == 0)
	  break;
	if (board[apos] == EMPTY
	    && trymove(apos, color, "purge_persistent_cache", 0))
	  played_moves++;
	else {
	  entry_ok = 0;
	  break;
	}
      }
    }

    if (!entry_ok 
	|| !verify_stored_board(entry->board)) {
      /* Move the last entry in the cache here and back up the loop
       * counter to redo the test at this position in the cache.
       */
      if (0)
	gprintf("Purging entry %d from cache.\n", k);
      if (k < cache->current_size - 1)
	*entry = cache->table[cache->current_size - 1];
      k--;
      cache->current_size--;
    }
    else {
      /* Reduce score here to penalize entries getting old. */
      entry->score *= cache->age_factor;
    }

    while (played_moves > 0) {
      popgo();
      played_moves--;
    }
  }
}


/* Find a cache entry matching the data given in the parameters.
 * Important: We assume that unused parameters are normalized to NO_MOVE
 * when storing or retrieving, so that we can ignore them here.
 */ 
static struct persistent_cache_entry *
find_persistent_cache_entry(struct persistent_cache *cache,
			    enum routine_id routine, int apos, int bpos,
			    int cpos, Hash_data *goal_hash, int node_limit)
{
  int k;
  for (k = 0; k < cache->current_size; k++) {
    struct persistent_cache_entry *entry = cache->table + k;
    if (entry->routine == routine
	&& entry->apos == apos
	&& entry->bpos == bpos
	&& entry->cpos == cpos
        && depth - stackp <= entry->remaining_depth
        && (entry->node_limit >= node_limit || entry->result_certain)
        && (goal_hash == NULL
	    || hashdata_is_equal(entry->goal_hash, *goal_hash))
        && verify_stored_board(entry->board))
      return entry;
  }
  return NULL;
}

/* Search through a persistent cache. Returns 0 if no matching entry was
 * found; returns 1 and sets the relevant return values otherwise. See
 * comment above find_persistent_cache_entry() about unused parameters.
 */
static int 
search_persistent_cache(struct persistent_cache *cache,
			enum routine_id routine, int apos, int bpos,
			int cpos, Hash_data *goal_hash, int node_limit, 
			int *result, int *move, int *move2, int *certain)
{
  /* Try to find entry. */
  struct persistent_cache_entry *entry;
  entry = find_persistent_cache_entry(cache, routine, apos, bpos, cpos,
				      goal_hash, node_limit);
  if (entry == NULL)
    return 0;

  /* Set return values. */
  *result = entry->result;
  if (move)
    *move = entry->move;
  if (move2)
    *move2 = entry->move2;
  if (certain)
    *certain = entry->result_certain;

  /* Increase score for entry. */
  entry->score += entry->cost;

  if (debug & DEBUG_PERSISTENT_CACHE) {
    gprintf("%oRetrieved position from %s:\n", cache->name);
    print_persistent_cache_entry(entry);
  }
  if (cache->name == "reading cache"
      && (debug & DEBUG_READING_PERFORMANCE)
      && entry->cost >= MIN_READING_NODES_TO_REPORT) {
    if (entry->result != 0)
      gprintf("%o%s %1m = %d %1m, cached (%d nodes) ",
              routine == ATTACK ? "attack" : "defend",
              apos, entry->result, entry->move, entry->cost);
    else
      gprintf("%o%s %1m = %d, cached (%d nodes) ",
              routine == ATTACK ? "attack" : "defend",
              apos, entry->result, entry->cost);
    dump_stack();
  }
  return 1;
}

/* Generic function that tries to store a cache entry. If the cache
 * is full, we delete the lowest scoring entry.
 *
 * Unused parameters have to be normalized to NO_MOVE by the calling
 * function.
 */
static void
store_persistent_cache(struct persistent_cache *cache,
		       enum routine_id routine,
		       int apos, int bpos, int cpos, Hash_data *goal_hash,
		       int result, int move, int move2,
		       int certain, int node_limit,
		       int cost, const char goal[BOARDMAX], int goal_color)
{
  int r;
  struct persistent_cache_entry *entry;
  if (stackp > cache->max_stackp)
    return;

  /* If cache is still full, consider kicking out an old entry. */
  if (cache->current_size == cache->max_size) {
    int worst_entry = -1;
    int worst_score = cost;
    int k;

    for (k = 0; k < cache->current_size; k++) {
      if (cache->table[k].score < worst_score) {
	worst_score = cache->table[k].score;
	worst_entry = k;
      }
    }

    if (worst_entry != -1) {
      /* Move the last entry in the cache here to make space.
       */
      if (worst_entry < cache->current_size - 1)
	cache->table[worst_entry] = cache->table[cache->current_size - 1];
      cache->current_size--;
    }
    else
      return;
  }

  entry = &(cache->table[cache->current_size]);
  entry->boardsize  	 = board_size;
  entry->routine    	 = routine;
  entry->apos	     	 = apos;
  entry->bpos	     	 = bpos;
  entry->cpos	     	 = cpos;
  if (goal_hash)
    entry->goal_hash	 = *goal_hash;
  entry->result     	 = result;
  entry->result_certain  = certain;
  entry->node_limit      = node_limit;
  entry->remaining_depth = depth - stackp;
  entry->move	         = move;
  entry->move2	         = move2;
  entry->score 		 = cost;
  entry->cost 		 = cost;
  entry->movenum 	 = movenum;

  for (r = 0; r < MAX_CACHE_DEPTH; r++) {
    if (r < stackp)
      get_move_from_stack(r, &(entry->stack[r]), &(entry->move_color[r]));
    else {
      entry->stack[r] = 0;
      entry->move_color[r] = EMPTY;
    }
  }
  
  /* Remains to set the board. */
  cache->compute_active_area(&(cache->table[cache->current_size]),
      			     goal, goal_color);
  cache->current_size++;

  if (debug & DEBUG_PERSISTENT_CACHE) {
    gprintf("%oEntered position in %s:\n", cache->name);
    print_persistent_cache_entry(entry);
    gprintf("%oCurrent size: %d\n", cache->current_size);
  }
}


/* ================================================================ */
/* Interface functions relevant to all caches.			    */
/* ================================================================ */

/* Allocate the actual cache table. */
static void
init_cache(struct persistent_cache *cache)
{
  cache->table = malloc(cache->max_size*sizeof(struct persistent_cache_entry));
  gg_assert(cache->table);
}

/* Initializes all persistent caches.
 * Needs to be called only once at startup.
 */
void
persistent_cache_init()
{
  init_cache(&reading_cache);
  init_cache(&breakin_cache);
  init_cache(&connection_cache);
  init_cache(&owl_cache);
}


/* Discards all persistent cache entries. */
void
clear_persistent_caches()
{
  reading_cache.current_size = 0;
  connection_cache.current_size = 0;
  breakin_cache.current_size = 0;
  owl_cache.current_size = 0;
}

/* Discards all persistent cache entries that are no longer useful. 
 * Should be called once per move for optimal performance (but is not 
 * necessary for proper operation).
 */
void
purge_persistent_caches()
{
  purge_persistent_cache(&reading_cache);
  purge_persistent_cache(&connection_cache);
  purge_persistent_cache(&breakin_cache);
  purge_persistent_cache(&owl_cache);
}

/* ================================================================ */
/*                  Tactical reading functions                      */
/* ================================================================ */


/* Look for a valid read result in the persistent cache.
 * Return 1 if found, 0 otherwise.
 */
int
search_persistent_reading_cache(enum routine_id routine, int str,
    				int *result, int *move)
{
  return search_persistent_cache(&reading_cache,
				 routine, str, NO_MOVE, NO_MOVE, NULL,
				 -1, result, move, NULL, NULL);
}


/* Store a new read result in the persistent cache. */
void
store_persistent_reading_cache(enum routine_id routine, int str,
    			       int result, int move, int nodes)
{
  store_persistent_cache(&reading_cache, routine,
      			 str, NO_MOVE, NO_MOVE, NULL,
			 result, move, NO_MOVE, -1, -1,
			 nodes, shadow, EMPTY);
}

static void
compute_active_reading_area(struct persistent_cache_entry *entry,
			    const char goal[BOARDMAX], int dummy)
{
  signed char active[BOARDMAX];
  int pos, r;
  UNUSED(dummy);

  /* Remains to set the board. We let the active area be the contested
   * string and reading shadow + adjacent empty and strings +
   * neighbors of active area so far + one more expansion from empty
   * to empty.
   */
  for (pos = BOARDMIN; pos < BOARDMAX; pos++)
    active[pos] = goal[pos];

  signed_mark_string(entry->apos, active, 1);

  /* To be safe, also add the successful move. */
  if (entry->result != 0 && entry->move != 0)
    active[entry->move] = 1;

  /* Add adjacent strings and empty. */
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (!ON_BOARD(pos))
      continue;
    if (active[pos] != 0) 
      continue;
    if ((ON_BOARD(SOUTH(pos)) && active[SOUTH(pos)] == 1)
	|| (ON_BOARD(WEST(pos)) && active[WEST(pos)] == 1)
	|| (ON_BOARD(NORTH(pos)) && active[NORTH(pos)] == 1)
	|| (ON_BOARD(EAST(pos)) && active[EAST(pos)] == 1)) {
      if (IS_STONE(board[pos]))
	signed_mark_string(pos, active, 2);
      else
	active[pos] = 2;
    }
  }

  /* Remove invincible strings. No point adding their liberties and
   * neighbors.
   */
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (!ON_BOARD(pos))
      continue;
    if (IS_STONE(board[pos]) && worm[pos].invincible)
      active[pos] = 0;
  }
  
  /* Expand empty to empty. */
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (IS_STONE(board[pos]) || active[pos] != 0) 
      continue;
    if ((board[SOUTH(pos)] == EMPTY && active[SOUTH(pos)] == 2)
	|| (board[WEST(pos)] == EMPTY && active[WEST(pos)] == 2)
	|| (board[NORTH(pos)] == EMPTY && active[NORTH(pos)] == 2)
	|| (board[EAST(pos)] == EMPTY && active[EAST(pos)] == 2))
      active[pos] = 3;
  }
  
  /* Add neighbors of active area so far. */
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (!ON_BOARD(pos))
      continue;
    if (active[pos] != 0) 
      continue;
    if ((ON_BOARD(SOUTH(pos)) && active[SOUTH(pos)] > 0
	 && active[SOUTH(pos)] < 4)
	|| (ON_BOARD(WEST(pos)) && active[WEST(pos)] > 0
	    && active[WEST(pos)] < 4)
	|| (ON_BOARD(NORTH(pos)) && active[NORTH(pos)] > 0
	    && active[NORTH(pos)] < 4)
	|| (ON_BOARD(EAST(pos)) && active[EAST(pos)] > 0
	    && active[EAST(pos)] < 4))
      active[pos] = 4;
  }

  /* Also add the previously played stones to the active area. */
  for (r = 0; r < stackp; r++)
    active[entry->stack[r]] = 5;

  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (!ON_BOARD(pos))
      continue;
    entry->board[pos] = 
      active[pos] != 0 ? board[pos] : GRAY;
  }
}


/* Helper for the reading_hotspots() function below. */
static void
mark_string_hotspot_values(float values[BOARDMAX],
			   int m, int n, float contribution)
{
  int i, j, k;
  
  /* If p[m][n] is EMPTY, we just give the contribution to close empty
   * vertices. This is a rough simplification.
   */
  if (BOARD(m, n) == EMPTY) {
    for (i = -1; i <= 1; i++)
      for (j = -1; j <= 1; j++)
	if (BOARD(m+i, n+j) == EMPTY)
	  values[POS(m+i, n+j)] += contribution;
    return;
  }
  
  /* Otherwise we give contribution to liberties and diagonal
   * neighbors of the string at (m, n).
   */
  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++) {
      if (BOARD(i, j) != EMPTY)
	continue;
      for (k = 0; k < 8; k++) {
	int di = deltai[k];
	int dj = deltaj[k];
	if (IS_STONE(BOARD(i+di, j+dj))
	    && same_string(POS(i+di, j+dj), POS(m, n))) {
	  if (k < 4) {
	    values[POS(i, j)] += contribution;
	    break;
	  }
	  else {
	    if (BOARD(i+di, j) == EMPTY || countlib(POS(i+di, j)) <= 2
		|| BOARD(i, j+dj) == EMPTY || countlib(POS(i, j+dj)) <= 2)
	      values[POS(i, j)] += contribution;
	    break;
	  }
	}
      }
    }
}
  

/* Based on the entries in the reading cache and their nodes field,
 * compute where the relatively most expensive tactical reading is
 * going on.
 */
void
reading_hotspots(float values[BOARDMAX])
{
  int pos;
  int k;
  int sum_nodes = 0;

  for (pos = BOARDMIN; pos < BOARDMAX; pos++)
    values[pos] = 0.0;
  
  /* Compute the total number of nodes for the cached entries. */
  for (k = 0; k < reading_cache.current_size; k++)
    sum_nodes += reading_cache.table[k].cost;

  if (sum_nodes <= 100)
    return;

  /* Loop over all entries and increase the value of vertices adjacent
   * to dragons involving expensive tactical reading.
   */
  for (k = 0; k < reading_cache.current_size; k++) {
    struct persistent_cache_entry *entry = &(reading_cache.table[k]);
    float contribution = entry->cost / (float) sum_nodes;
    if (0) {
      gprintf("Reading hotspots: %d %1m %f\n", entry->routine, entry->apos,
	      contribution);
    }
    switch (entry->routine) {
    case ATTACK:
    case FIND_DEFENSE:
      mark_string_hotspot_values(values, I(entry->apos), J(entry->apos),
				 contribution);
      break;
    default:
      gg_assert(0); /* Shouldn't happen. */
      break;
    }
  }
}


/* ================================================================ */
/*                  Connection reading functions                    */
/* ================================================================ */

/* Look for a valid read result in the persistent connection cache.
 * Return 1 if found, 0 otherwise.
 */
int
search_persistent_connection_cache(enum routine_id routine, int str1,
    				   int str2, int *result, int *move)
{
  return search_persistent_cache(&connection_cache, routine,
      				 str1, str2, NO_MOVE, NULL,
				 connection_node_limit,
				 result, move, NULL, NULL);
}

/* Store a new connection result in the persistent cache. */
void
store_persistent_connection_cache(enum routine_id routine,
    				  int str1, int str2,
				  int result, int move, int tactical_nodes,
				  char connection_shadow[BOARDMAX])
{
  store_persistent_cache(&connection_cache, routine, str1, str2, NO_MOVE, NULL,
      			 result, move, NO_MOVE, -1, connection_node_limit,
			 tactical_nodes, connection_shadow, EMPTY);
}

/* Computes the active area for the current board position and the
 * connection read result that has just been stored in *entry.
 */
static void
compute_active_connection_area(struct persistent_cache_entry *entry,
			       const char connection_shadow[BOARDMAX],
			       int dummy)
{
  int pos;
  int k, r;
  signed char active[BOARDMAX];
  int other = OTHER_COLOR(board[entry->apos]);
  UNUSED(dummy);

  /* Remains to set the board. We let the active area be
   * the two strings to connect +
   * the connection shadow +
   * distance two expansion through empty intersections and own stones +
   * adjacent opponent strings +
   * liberties and neighbors of adjacent opponent strings with less than
   * five liberties +
   * liberties and neighbors of low liberty neighbors of adjacent opponent
   * strings with less than five liberties.
   */
  for (pos = BOARDMIN; pos < BOARDMAX; pos++)
    active[pos] = connection_shadow[pos];

  signed_mark_string(entry->apos, active, 1);
  signed_mark_string(entry->bpos, active, 1);

  /* To be safe, also add the successful move. */
  if (entry->result != 0 && entry->move != 0)
    active[entry->move] = 1;

  /* Distance two expansion through empty intersections and own stones. */
  for (k = 1; k < 3; k++) {
    for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
      if (!ON_BOARD(pos) || board[pos] == other || active[pos] != 0) 
	continue;
      if ((ON_BOARD(SOUTH(pos)) && active[SOUTH(pos)] == k)
	  || (ON_BOARD(WEST(pos)) && active[WEST(pos)] == k)
	  || (ON_BOARD(NORTH(pos)) && active[NORTH(pos)] == k)
	  || (ON_BOARD(EAST(pos)) && active[EAST(pos)] == k)) {
	if (board[pos] == EMPTY)
	  active[pos] = k + 1;
	else
	  signed_mark_string(pos, active, (signed char) (k + 1));
      }
    }
  }
  
  /* Adjacent opponent strings. */
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (board[pos] != other || active[pos] != 0) 
      continue;
    for (r = 0; r < 4; r++) {
      int pos2 = pos + delta[r];
      if (ON_BOARD(pos2) && board[pos2] != other && active[pos2] != 0) {
	signed_mark_string(pos, active, 1);
	break;
      }
    }
  }
  
  /* Liberties of adjacent opponent strings with less than five liberties +
   * liberties of low liberty neighbors of adjacent opponent strings
   * with less than five liberties.
   */
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (board[pos] == other && active[pos] > 0 && countlib(pos) < 5) {
      int libs[4];
      int liberties = findlib(pos, 4, libs);
      int adjs[MAXCHAIN];
      int adj;
      for (r = 0; r < liberties; r++)
	active[libs[r]] = 1;
      
      /* Also add liberties of neighbor strings if these are three
       * or less.
       */
      adj = chainlinks(pos, adjs);
      for (r = 0; r < adj; r++) {
	signed_mark_string(adjs[r], active, -1);
	if (countlib(adjs[r]) <= 3) {
	  int s;
	  int adjs2[MAXCHAIN];
	  int adj2;
	  liberties = findlib(adjs[r], 3, libs);
	  for (s = 0; s < liberties; s++)
	    active[libs[s]] = 1;
	  adj2 = chainlinks(pos, adjs2);
	  for (s = 0; s < adj2; s++)
	    signed_mark_string(adjs2[s], active, -1);
	}
      }
    }
  }
  
  /* Also add the previously played stones to the active area. */
  for (r = 0; r < stackp; r++)
    active[entry->stack[r]] = 1;

  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    int value = board[pos];
    if (!ON_BOARD(pos))
      continue;
    if (!active[pos])
      value = GRAY;
    else if (IS_STONE(board[pos]) && countlib(pos) > 4 && active[pos] > 0)
      value |= HIGH_LIBERTY_BIT;
    
    entry->board[pos] = value;
  }

}


/* ================================================================ */
/*                   Break-in reading functions                     */
/* ================================================================ */

/* Look for a valid read result in the persistent breakin cache.
 * Return 1 if found, 0 otherwise.
 */
int
search_persistent_breakin_cache(enum routine_id routine,
    				int str, Hash_data *goal_hash,
				int node_limit, int *result, int *move)
{
  return search_persistent_cache(&breakin_cache, routine,
      				 str, NO_MOVE, NO_MOVE, goal_hash, 
				 node_limit, result, move, NULL, NULL);
}
      		          
/* Store a new breakin result in the persistent cache. */
void
store_persistent_breakin_cache(enum routine_id routine,
    			       int str, Hash_data *goal_hash,
			       int result, int move, int tactical_nodes,
			       int breakin_node_limit,
			       char breakin_shadow[BOARDMAX])
{
  store_persistent_cache(&breakin_cache, routine,
      			 str, NO_MOVE, NO_MOVE, goal_hash,
      			 result, move, NO_MOVE, -1, breakin_node_limit,
			 tactical_nodes, breakin_shadow, EMPTY);
}
  

/* Computes the active area for the current board position and the
 * read result that has just been stored in *entry.
 */
static void
compute_active_breakin_area(struct persistent_cache_entry *entry,
			    const char breakin_shadow[BOARDMAX], int dummy)
{
  int pos;
  int k, r;
  signed char active[BOARDMAX];
  int other = OTHER_COLOR(board[entry->apos]);
  UNUSED(dummy);

  /* We let the active area be
   * the string to connect +
   * the breakin shadow (which contains the goal) +
   * distance two expansion through empty intersections and own stones +
   * adjacent opponent strings +
   * liberties and neighbors of adjacent opponent strings with less than
   * five liberties +
   * liberties and neighbors of low liberty neighbors of adjacent opponent
   * strings with less than five liberties.
   */
  for (pos = BOARDMIN; pos < BOARDMAX; pos++)
    active[pos] = breakin_shadow[pos];

  signed_mark_string(entry->apos, active, 1);

  /* To be safe, also add the successful move. */
  if (entry->result != 0 && entry->move != 0)
    active[entry->move] = 1;

  /* Distance two expansion through empty intersections and own stones. */
  for (k = 1; k < 3; k++) {
    for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
      if (!ON_BOARD(pos) || board[pos] == other || active[pos] != 0) 
	continue;
      if ((ON_BOARD(SOUTH(pos)) && active[SOUTH(pos)] == k)
	  || (ON_BOARD(WEST(pos)) && active[WEST(pos)] == k)
	  || (ON_BOARD(NORTH(pos)) && active[NORTH(pos)] == k)
	  || (ON_BOARD(EAST(pos)) && active[EAST(pos)] == k)) {
	if (board[pos] == EMPTY)
	  active[pos] = k + 1;
	else
	  signed_mark_string(pos, active, (signed char) (k + 1));
      }
    }
  }
  
  /* Adjacent opponent strings. */
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (board[pos] != other || active[pos] != 0) 
      continue;
    for (r = 0; r < 4; r++) {
      int pos2 = pos + delta[r];
      if (ON_BOARD(pos2)
	  && board[pos2] != other
	  && active[pos2] && active[pos2] <= 2) {
	signed_mark_string(pos, active, 1);
	break;
      }
    }
  }
  
  /* Liberties of adjacent opponent strings with less than four liberties +
   * liberties of low liberty neighbors of adjacent opponent strings
   * with less than five liberties.
   */
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (board[pos] == other && active[pos] > 0 && countlib(pos) < 4) {
      int libs[4];
      int liberties = findlib(pos, 3, libs);
      int adjs[MAXCHAIN];
      int adj;
      for (r = 0; r < liberties; r++)
	active[libs[r]] = 1;
      
      /* Also add liberties of neighbor strings if these are three
       * or less.
       */
      adj = chainlinks(pos, adjs);
      for (r = 0; r < adj; r++) {
	signed_mark_string(adjs[r], active, -1);
	if (countlib(adjs[r]) <= 3) {
	  int s;
	  int adjs2[MAXCHAIN];
	  int adj2;
	  liberties = findlib(adjs[r], 3, libs);
	  for (s = 0; s < liberties; s++)
	    active[libs[s]] = 1;
	  adj2 = chainlinks(pos, adjs2);
	  for (s = 0; s < adj2; s++)
	    signed_mark_string(adjs2[s], active, -1);
	}
      }
    }
  }
  
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    char value = board[pos];
    if (!ON_BOARD(pos))
      continue;
    if (!active[pos])
      value = GRAY;
    else if (IS_STONE(board[pos]) && countlib(pos) > 3 && active[pos] > 0)
      value |= HIGH_LIBERTY_BIT2;
    
    entry->board[pos] = value;
  }
}


/* ================================================================ */
/*                    Owl reading functions                         */
/* ================================================================ */
int
search_persistent_owl_cache(enum routine_id routine,
    			    int apos, int bpos, int cpos,
			    int *result, int *move, int *move2, int *certain)
{
  return search_persistent_cache(&owl_cache,
      				 routine, apos, bpos, cpos, NULL,
				 owl_node_limit,
				 result, move, move2, certain);
}
  

void
store_persistent_owl_cache(enum routine_id routine,
    			   int apos, int bpos, int cpos,
			   int result, int move, int move2, int certain,
			   int tactical_nodes,
			   char goal[BOARDMAX], int goal_color)
{
  store_persistent_cache(&owl_cache, routine, apos, bpos, cpos, NULL,
      			 result, move, move2, certain, owl_node_limit,
			 tactical_nodes, goal, goal_color);
}


static void compute_active_owl_area(struct persistent_cache_entry *entry,
				    const char goal[BOARDMAX],
				    int goal_color)
{
  int k, r;
  int pos;
  int other = OTHER_COLOR(goal_color);
  signed char active[BOARDMAX];

  /* We let the active area be the goal +
   * distance four expansion through empty intersections and own stones +
   * adjacent opponent strings +
   * liberties and neighbors of adjacent opponent strings with less than
   * five liberties +
   * liberties and neighbors of low liberty neighbors of adjacent opponent
   * strings with less than five liberties.
   */
  for (pos = BOARDMIN; pos < BOARDMAX; pos++)
    if (ON_BOARD(pos))
      active[pos] = (goal[pos] != 0);

  /* Also add critical moves to the active area. */
  if (ON_BOARD1(entry->move))
    active[entry->move] = 1;

  if (ON_BOARD1(entry->move2))
    active[entry->move2] = 1;

  /* Distance four expansion through empty intersections and own stones. */
  for (k = 1; k < 5; k++) {
    for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
      if (!ON_BOARD(pos) || board[pos] == other || active[pos] > 0) 
	continue;
      if ((ON_BOARD(SOUTH(pos)) && active[SOUTH(pos)] == k)
	  || (ON_BOARD(WEST(pos)) && active[WEST(pos)] == k)
	  || (ON_BOARD(NORTH(pos)) && active[NORTH(pos)] == k)
	  || (ON_BOARD(EAST(pos)) && active[EAST(pos)] == k)) {
	if (board[pos] == EMPTY)
	  active[pos] = k + 1;
	else
	  signed_mark_string(pos, active, (signed char) (k + 1));
      }
    }
  }
  
  /* Adjacent opponent strings. */
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (board[pos] != other || active[pos] != 0) 
      continue;
    for (r = 0; r < 4; r++) {
      int pos2 = pos + delta[r];
      if (ON_BOARD(pos2) && board[pos2] != other && active[pos2] != 0) {
	signed_mark_string(pos, active, 1);
	break;
      }
    }
  }
  
  /* Liberties of adjacent opponent strings with less than five liberties +
   * liberties of low liberty neighbors of adjacent opponent strings
   * with less than five liberties.
   */
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (board[pos] == other && active[pos] > 0 && countlib(pos) < 5) {
      int libs[4];
      int liberties = findlib(pos, 4, libs);
      int adjs[MAXCHAIN];
      int adj;
      for (r = 0; r < liberties; r++)
	active[libs[r]] = 1;
      
      /* Also add liberties of neighbor strings if these are three
       * or less.
       */
      adj = chainlinks(pos, adjs);
      for (r = 0; r < adj; r++) {
	signed_mark_string(adjs[r], active, -1);
	if (countlib(adjs[r]) <= 3) {
	  int s;
	  int adjs2[MAXCHAIN];
	  int adj2;
	  liberties = findlib(adjs[r], 3, libs);
	  for (s = 0; s < liberties; s++)
	    active[libs[s]] = 1;
	  adj2 = chainlinks(pos, adjs2);
	  for (s = 0; s < adj2; s++)
	    signed_mark_string(adjs2[s], active, -1);
	}
      }
    }
  }
  
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    int value = board[pos];
    if (!ON_BOARD(pos))
      continue;
    if (!active[pos])
      value = GRAY;
    else if (IS_STONE(board[pos]) && countlib(pos) > 4 && active[pos] > 0)
      value |= HIGH_LIBERTY_BIT;
    
    entry->board[pos] = value;
  }
}


/* Helper for the owl_hotspots() function below. */
static void
mark_dragon_hotspot_values(float values[BOARDMAX], int dr,
			   float contribution, char active_board[BOARDMAX])
{
  int pos;
  int k;
  ASSERT1(IS_STONE(board[dr]), dr);
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (board[pos] != EMPTY)
      continue;
    for (k = 0; k < 8; k++) {
      int pos2 = pos + delta[k];
      if (IS_STONE(board[pos2])
	  && (is_same_dragon(pos2, dr)
	      || (are_neighbor_dragons(pos2, dr)
		  && board[pos2] == board[dr]))
	  && (countlib(pos2) <= 4
	      || is_edge_vertex(pos))) {
	if (k < 4) {
	  if (is_same_dragon(pos2, dr))
	    values[pos] += contribution;
	  else
	    values[pos] += 0.5 * contribution;
	  break;
	}
	else {
	  /* If pos2 = SOUTHWEST(pos), this construction makes
	   *    pos3 = SOUTH(pos) and
	   *    pos4 = WEST(pos)
	   * and corresponding for all other diagonal movements.
	   */
	  int pos3 = pos + delta[k % 4];
	  int pos4 = pos + delta[(k+1) % 4];
	  if (board[pos3] == EMPTY || countlib(pos3) <= 2
	      || board[pos4] == EMPTY || countlib(pos4) <= 2)
	    values[pos] += 0.5 * contribution;
	  break;
	}
      }
    }
    /* If not close to the dragon, but within the active area, give
     * negative hotspot contribution.
     */
    if (k == 8 && active_board[pos] == EMPTY) {
      values[pos] -= 0.5 * contribution;
    }
  }
}
  

/* Based on the entries in the owl cache and their tactical_nodes
 * field, compute where the relatively most expensive owl reading is
 * going on.
 */
void
owl_hotspots(float values[BOARDMAX])
{
  int pos;
  int k, r;
  int libs[MAXLIBS];
  int liberties;
  int sum_tactical_nodes = 0;

  /* Don't bother checking out of board. Set values[] to zero there too. */
  for (pos = BOARDMIN; pos < BOARDMAX; pos++)
    values[pos] = 0.0;
  
  /* Compute the total number of tactical nodes for the cached entries. */
  for (k = 0; k < owl_cache.current_size; k++)
    sum_tactical_nodes += owl_cache.table[k].score;

  if (sum_tactical_nodes <= 100)
    return;

  /* Loop over all entries and increase the value of vertices adjacent
   * to dragons involving expensive owl reading.
   */
  for (k = 0; k < owl_cache.current_size; k++) {
    struct persistent_cache_entry *entry = &(owl_cache.table[k]);
    float contribution = entry->score / (float) sum_tactical_nodes;
    if (debug & DEBUG_PERSISTENT_CACHE) {
      gprintf("Owl hotspots: %d %1m %f\n", entry->routine, entry->apos,
	      contribution);
    }
    switch (entry->routine) {
    case OWL_ATTACK:
    case OWL_THREATEN_ATTACK:
    case OWL_DEFEND:
    case OWL_THREATEN_DEFENSE:
      mark_dragon_hotspot_values(values, entry->apos,
				 contribution, entry->board);
      break;
    case OWL_DOES_DEFEND:
    case OWL_DOES_ATTACK:
    case OWL_CONFIRM_SAFETY:
      mark_dragon_hotspot_values(values, entry->bpos,
				 contribution, entry->board);
      break;
    case OWL_CONNECTION_DEFENDS:
      mark_dragon_hotspot_values(values, entry->bpos,
				 contribution, entry->board);
      mark_dragon_hotspot_values(values, entry->cpos,
				 contribution, entry->board);
      break;
    case OWL_SUBSTANTIAL:
      /* Only consider the liberties of (apos). */
      liberties = findlib(entry->apos, MAXLIBS, libs);
      for (r = 0; r < liberties; r++)
	values[libs[r]] += contribution;
      break;
    default:
      gg_assert(0); /* Shouldn't happen. */
      break;
    }
  }
}

/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
