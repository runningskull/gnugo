/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * This is GNU GO, a Go program. Contact gnugo@gnu.org, or see       *
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
 * Persistent caching has so far been implemented for tactical reading
 * and owl reading (with connection reading and semeai reading planned
 * for the future). Since different data is stored for the different
 * types of reading, similar but different data structures and
 * functions are implemented for each cache.
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
#include "liberty.h"
#include "cache.h"


/* ================================================================ */
/*                     Data structures                              */
/* ================================================================ */

/* Used in active area. */
#define HIGH_LIBERTY_BIT 4

/* Tactical reading cache. */

#define MAX_READING_CACHE_DEPTH 5
#define MAX_READING_CACHE_SIZE 100

struct reading_cache {
  int boardsize;
  char board[BOARDMAX];
  int movenum;
  int nodes;
  int score;
  int remaining_depth;
  int routine;			/* ATTACK or FIND_DEFENSE */
  int str;			/* contested string (origin) */
  int result;
  int move;			/* attack/defense point */
  int stack[MAX_READING_CACHE_DEPTH];
  int move_color[MAX_READING_CACHE_DEPTH];
};

static struct reading_cache persistent_reading_cache[MAX_READING_CACHE_SIZE];
static int persistent_reading_cache_size = 0;


/* Owl reading cache. */

#define MAX_OWL_CACHE_SIZE 150

/* Persistent owl result cache to reuse owl results between moves. */
struct owl_cache {
  int boardsize;
  char board[BOARDMAX];
  int movenum;
  int tactical_nodes;
  int routine;
  int apos;  /* first input coordinate */
  int bpos;  /* second input coordinate */
  int cpos;  /* third input coordinate */
  int result;
  int result_certain;
  int move;  /* first result coordinate */
  int move2;  /* second result coordinate */
};

static struct owl_cache persistent_owl_cache[MAX_OWL_CACHE_SIZE];
static int persistent_owl_cache_size = 0;

/* ================================================================ */
/*                     Static function declarations                 */
/* ================================================================ */

/* Common functions. */
static void draw_active_area(char board[BOARDMAX], int apos);
static int verify_stored_board(char board[BOARDMAX]);

/* Tactical reading functions. */
static int find_persistent_reading_cache_entry(int routine, int str);
static void print_persistent_reading_cache_entry(int k);
static void mark_string_hotspot_values(float values[BOARDMAX],
				       int m, int n, float contribution);

/* Owl functions. */
static void print_persistent_owl_cache_entry(int k);
static void mark_dragon_hotspot_values(float values[BOARDMAX], int pos,
				       float contribution,
				       char active_board[BOARDMAX]);


/* ================================================================ */
/*                      Common functions                            */
/* ================================================================ */


static void
draw_active_area(char board[BOARDMAX], int apos)
{
  int i, j, ii;
  int c = ' ';
  int cw = apos==NO_MOVE ? 'O' : 'o';
  int cb = apos==NO_MOVE ? 'X' : 'x';

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
      else if (board[pos] == (WHITE | HIGH_LIBERTY_BIT))
	c = 'O';
      else if (board[pos] == BLACK)
	c = cb;
      else if (board[pos] == (BLACK | HIGH_LIBERTY_BIT))
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
    else if (!(p[pos] & HIGH_LIBERTY_BIT))
      continue;
    else if (countlib(pos) <= 4)
      return 0;
  }
  
  return 1;
}


/* ================================================================ */
/*                  Tactical reading functions                      */
/* ================================================================ */

/* Remove persistent cache entries which are no longer current. */
void
purge_persistent_reading_cache()
{
  int k;
  int r;
  static int last_purge_position_number = -1;
  gg_assert(stackp == 0);

  /* Never do this more than once per move. */
  if (last_purge_position_number == position_number)
    return;
  else
    last_purge_position_number = position_number;

  for (k = 0; k < persistent_reading_cache_size; k++) {
    int played_moves = 0;
    int entry_ok = 1;

    if (persistent_reading_cache[k].boardsize != board_size)
      entry_ok = 0;
    else {
      for (r = 0; r < MAX_READING_CACHE_DEPTH; r++) {
	int apos = persistent_reading_cache[k].stack[r];
	int color = persistent_reading_cache[k].move_color[r];
	if (apos == 0)
	  break;
	if (board[apos] == EMPTY
	    && trymove(apos, color, "purge_persistent_reading_cache", 0,
		       EMPTY, 0))
	  played_moves++;
	else {
	  entry_ok = 0;
	  break;
	}
      }
    }

    if (!entry_ok 
	|| !verify_stored_board(persistent_reading_cache[k].board)) {
      /* Move the last entry in the cache here and back up the loop
       * counter to redo the test at this position in the cache.
       */
      if (0)
	gprintf("Purging entry %d from cache.\n", k);
      if (k < persistent_reading_cache_size - 1)
	persistent_reading_cache[k] 
	  = persistent_reading_cache[persistent_reading_cache_size - 1];
      k--;
      persistent_reading_cache_size--;
    }

    while (played_moves > 0) {
      popgo();
      played_moves--;
    }
  }
}

void
clear_persistent_reading_cache()
{
  persistent_reading_cache_size = 0;
}


/* Locate a matching entry in the persistent reading cache. Return the
 * entry number or -1 if none found.
 */
static int
find_persistent_reading_cache_entry(int routine, int str)
{
  int k;
  int r;
  ASSERT1(str == find_origin(str), str);

  for (k = 0; k < persistent_reading_cache_size; k++) {
    /* Check that everything matches. */
    struct reading_cache *entry = &(persistent_reading_cache[k]);
    int apos = 0;
    if (entry->routine != routine
	|| entry->str != str
	|| entry->remaining_depth < (depth - stackp)
	|| entry->boardsize != board_size)
      continue;
    
    for (r = 0; r < MAX_READING_CACHE_DEPTH; r++) {
      apos = entry->stack[r];
      if (apos == 0
	  || (entry->board[apos] != GRAY
	      && board[apos] != entry->board[apos]))
	break;
    }

    if (r < MAX_READING_CACHE_DEPTH && apos != 0)
      continue;

    if (!verify_stored_board(entry->board))
      continue;

    return k;
  }
  
  return -1;
}

/* Look for a valid read result in the persistent cache.
 * Return 1 if found, 0 otherwise.
 */
int
search_persistent_reading_cache(int routine, int str, int *result, int *move)
{
  int k;
  struct reading_cache *entry;

  k = find_persistent_reading_cache_entry(routine, str);
  if (k == -1)
    return 0;

  /* Match found. Increase score and fill in the answer. */
  entry = &(persistent_reading_cache[k]);
  entry->score += entry->nodes;
  if (result)
    *result = entry->result;
  if (move)
    *move = entry->move;
  ASSERT1(entry->result == 0
	  || entry->move == NO_MOVE
	  || ON_BOARD(entry->move),
	  entry->move);
  
  if ((debug & DEBUG_READING_PERFORMANCE)
      && entry->nodes >= MIN_READING_NODES_TO_REPORT) {
    if (entry->result != 0)
      gprintf("%o%s %1m = %d %1m, cached (%d nodes) ",
	      routine == ATTACK ? "attack" : "defend",
	      str, entry->result, entry->move, entry->nodes);
    else 
      gprintf("%o%s %1m = %d, cached (%d nodes) ",
	      routine == ATTACK ? "attack" : "defend",
	      str, entry->result, entry->nodes);
    dump_stack();
  }
  
  return 1;
}


/* Store a new read result in the persistent cache. */
void
store_persistent_reading_cache(int routine, int str, int result, int move,
			       int nodes)
{
  char active[BOARDMAX];
  int k;
  int r;
  int score = nodes;
  struct reading_cache *entry;

  ASSERT1(result == 0 || (move == 0) || ON_BOARD(move), move);

  /* Never cache results at too great depth. */
  if (stackp > MAX_READING_CACHE_DEPTH)
    return;

  /* If cache is still full, consider kicking out an old entry. */
  if (persistent_reading_cache_size == MAX_READING_CACHE_SIZE) {
    int worst_entry = -1;
    int worst_score = score;
    
    for (k = 1; k < persistent_reading_cache_size; k++) {
      if (persistent_reading_cache[k].score < worst_score) {
	worst_score = persistent_reading_cache[k].score;
	worst_entry = k;
      }
    }

    if (worst_entry != -1) {
      /* Move the last entry in the cache here to make space.
       */
      if (worst_entry < persistent_reading_cache_size - 1)
	persistent_reading_cache[worst_entry] 
	  = persistent_reading_cache[persistent_reading_cache_size - 1];
      persistent_reading_cache_size--;
    }
    else
      return;
  }

  entry = &(persistent_reading_cache[persistent_reading_cache_size]);
  entry->boardsize       = board_size;
  entry->movenum         = movenum;
  entry->nodes           = nodes;
  entry->score           = score;
  entry->remaining_depth = depth - stackp;
  entry->routine         = routine;
  entry->str	         = str;
  entry->result          = result;
  entry->move	         = move;

  for (r = 0; r < MAX_READING_CACHE_DEPTH; r++) {
    if (r < stackp)
      get_move_from_stack(r, &(entry->stack[r]), &(entry->move_color[r]));
    else {
      entry->stack[r] = 0;
      entry->move_color[r] = EMPTY;
    }
  }
  
  /* Remains to set the board. We let the active area be the contested
   * string and reading shadow + adjacent empty and strings +
   * neighbors of active area so far + one more expansion from empty
   * to empty.
   */
  for (k = BOARDMIN; k < BOARDMAX; k++)
    active[k] = shadow[k];

  mark_string(str, active, 1);

  /* To be safe, also add the successful move. */
  if (result != 0 && move != 0)
    active[move] = 1;

  /* Add adjacent strings and empty. */
  for (k = BOARDMIN; k < BOARDMAX; k++) {
    if (!ON_BOARD(k))
      continue;
    if (active[k] != 0) 
      continue;
    if ((ON_BOARD(SOUTH(k)) && active[SOUTH(k)] == 1)
	|| (ON_BOARD(WEST(k)) && active[WEST(k)] == 1)
	|| (ON_BOARD(NORTH(k)) && active[NORTH(k)] == 1)
	|| (ON_BOARD(EAST(k)) && active[EAST(k)] == 1)) {
      if (IS_STONE(board[k]))
	mark_string(k, active, 2);
      else
	active[k] = 2;
    }
  }

  /* Remove invincible strings. No point adding their liberties and
   * neighbors.
   */
  for (k = BOARDMIN; k < BOARDMAX; k++) {
    if (!ON_BOARD(k))
      continue;
    if (IS_STONE(board[k]) && worm[k].invincible)
      active[k] = 0;
  }
  
  /* Expand empty to empty. */
  for (k = BOARDMIN; k < BOARDMAX; k++) {
    if (IS_STONE(board[k]) || active[k] != 0) 
      continue;
    if ((board[SOUTH(k)] == EMPTY && active[SOUTH(k)] == 2)
	|| (board[WEST(k)] == EMPTY && active[WEST(k)] == 2)
	|| (board[NORTH(k)] == EMPTY && active[NORTH(k)] == 2)
	|| (board[EAST(k)] == EMPTY && active[EAST(k)] == 2))
      active[k] = 3;
  }
  
  /* Add neighbors of active area so far. */
  for (k = BOARDMIN; k < BOARDMAX; k++) {
    if (!ON_BOARD(k))
      continue;
    if (active[k] != 0) 
      continue;
    if ((ON_BOARD(SOUTH(k)) && active[SOUTH(k)] > 0 && active[SOUTH(k)] < 4)
	|| (ON_BOARD(WEST(k)) && active[WEST(k)] > 0 && active[WEST(k)] < 4)
	|| (ON_BOARD(NORTH(k)) && active[NORTH(k)] > 0 && active[NORTH(k)] < 4)
	|| (ON_BOARD(EAST(k)) && active[EAST(k)] > 0 && active[EAST(k)] < 4))
      active[k] = 4;
  }

  /* Also add the previously played stones to the active area. */
  for (r = 0; r < stackp; r++)
    active[entry->stack[r]] = 5;

  for (k = BOARDMIN; k < BOARDMAX; k++) {
    if (!ON_BOARD(k))
      continue;
    entry->board[k] = 
      active[k] != 0 ? board[k] : GRAY;
  }

  if (0) {
    gprintf("%o Stored result in cache (entry %d):\n",
	    persistent_reading_cache_size);
    print_persistent_reading_cache_entry(persistent_reading_cache_size);
    gprintf("%o Reading shadow was:\n");
    draw_reading_shadow();
  }
  
  persistent_reading_cache_size++;
}

/* Delete an entry from the cache, if it's there. */
void
delete_persistent_reading_cache_entry(int routine, int str)
{
  int k = find_persistent_reading_cache_entry(routine, find_origin(str));
  while (k != -1) {
    persistent_reading_cache[k]
      = persistent_reading_cache[--persistent_reading_cache_size];
    k = find_persistent_reading_cache_entry(routine, find_origin(str));
  }
}


/* For debugging purposes. */
static void
print_persistent_reading_cache_entry(int k)
{
  struct reading_cache *entry = &(persistent_reading_cache[k]);
  int r;
  gprintf("%oboardsize       = %d\n",  entry->boardsize);
  gprintf("%omovenum         = %d\n",  entry->movenum);
  gprintf("%onodes           = %d\n",  entry->nodes);
  gprintf("%oscore           = %d\n",  entry->score);
  gprintf("%oremaining_depth = %d\n",  entry->remaining_depth);
  gprintf("%oroutine         = %d\n",  entry->routine);
  gprintf("%ostr             = %1m\n", entry->str);
  gprintf("%oresult          = %d\n",  entry->result);
  gprintf("%omove            = %1m\n", entry->move);
  
  for (r = 0; r < MAX_READING_CACHE_DEPTH; r++) {
    if (entry->stack[r] == 0)
      break;
    gprintf("%ostack[%d]      = %C %1m\n", r, entry->move_color[r],
	    entry->stack[r]);
  }

  draw_active_area(entry->board, NO_MOVE);
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
  int m, n, k;
  int sum_nodes = 0;

  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++)
      values[POS(m, n)] = 0.0;
  
  /* Compute the total number of nodes for the cached entries. */
  for (k = 0; k < persistent_reading_cache_size; k++)
    sum_nodes += persistent_reading_cache[k].nodes;

  if (sum_nodes <= 100)
    return;

  /* Loop over all entries and increase the value of vertices adjacent
   * to dragons involving expensive tactical reading.
   */
  for (k = 0; k < persistent_reading_cache_size; k++) {
    struct reading_cache *entry = &(persistent_reading_cache[k]);
    float contribution = entry->nodes / (float) sum_nodes;
    if (0) {
      gprintf("Reading hotspots: %d %1m %f\n", entry->routine, entry->str,
	      contribution);
    }
    switch (entry->routine) {
    case ATTACK:
    case FIND_DEFENSE:
      mark_string_hotspot_values(values, I(entry->str), J(entry->str),
				 contribution);
      break;
    default:
      gg_assert(0); /* Shouldn't happen. */
      break;
    }
  }
}


/* ================================================================ */
/*                    Owl reading functions                         */
/* ================================================================ */

/* Remove persistent cache entries which are no longer compatible with
 * the board. For efficient use of the cache, it's recommended to call
 * this function once per move, before starting the owl reading. It's
 * not required for correct operation though. 
 */
void
purge_persistent_owl_cache()
{
  int k;
  static int last_purge_position_number = -1;
  gg_assert(stackp == 0);

  /* Never do this more than once per move. */
  if (last_purge_position_number == position_number)
    return;
  else
    last_purge_position_number = position_number;

  for (k = 0; k < persistent_owl_cache_size; k++) {
    if (persistent_owl_cache[k].boardsize != board_size
	|| !verify_stored_board(persistent_owl_cache[k].board)) {
      /* Move the last entry in the cache here and back up the loop
       * counter to redo the test at this position in the cache.
       */
      if (k < persistent_owl_cache_size - 1)
	persistent_owl_cache[k] 
	  = persistent_owl_cache[persistent_owl_cache_size - 1];
      k--;
      persistent_owl_cache_size--;
    }
  }
}

void
clear_persistent_owl_cache()
{
  persistent_owl_cache_size = 0;
}

int
search_persistent_owl_cache(int routine, int apos, int bpos, int cpos,
			    int *result, int *move, int *move2, int *certain)
{
  int k;
  gg_assert(stackp == 0 || stackp == 1);

  for (k = 0; k < persistent_owl_cache_size; k++) {
    if (persistent_owl_cache[k].routine == routine
	&& persistent_owl_cache[k].apos == apos
	&& persistent_owl_cache[k].bpos == bpos
	&& persistent_owl_cache[k].cpos == cpos
	&& verify_stored_board(persistent_owl_cache[k].board)) {
      *result = persistent_owl_cache[k].result;
      if (move)
	*move = persistent_owl_cache[k].move;
      if (move2)
	*move2 = persistent_owl_cache[k].move2;
      if (certain)
	*certain = persistent_owl_cache[k].result_certain;

      TRACE_OWL_PERSISTENT_CACHE(
	    "persistent owl cache hit: routine %s at %1m result %d\n",
	    routine_to_string(routine), apos, bpos, cpos, 
	    result_to_string(persistent_owl_cache[k].result));
      return 1;
    }
  }
  return 0;
}

void
store_persistent_owl_cache(int routine, int apos, int bpos, int cpos,
			   int result, int move, int move2, int certain,
			   int tactical_nodes,
			   char goal[BOARDMAX], int goal_color)
{
  char active[BOARDMAX];
  int pos;
  int k;
  int r;
  int other = OTHER_COLOR(goal_color);
  gg_assert(stackp == 0);

  /* If cache is full, first try to purge it. */
  if (persistent_owl_cache_size == MAX_OWL_CACHE_SIZE)
    purge_persistent_owl_cache();

  /* FIXME: Kick out oldest or least expensive entry instead of giving up. */
  if (persistent_owl_cache_size == MAX_OWL_CACHE_SIZE) {
    TRACE_OWL_PERFORMANCE("Persistent owl cache full.\n");
    return;
  }

  persistent_owl_cache[persistent_owl_cache_size].boardsize  	 = board_size;
  persistent_owl_cache[persistent_owl_cache_size].routine    	 = routine;
  persistent_owl_cache[persistent_owl_cache_size].apos	     	 = apos;
  persistent_owl_cache[persistent_owl_cache_size].bpos	     	 = bpos;
  persistent_owl_cache[persistent_owl_cache_size].cpos	     	 = cpos;
  persistent_owl_cache[persistent_owl_cache_size].result     	 = result;
  persistent_owl_cache[persistent_owl_cache_size].result_certain = certain;
  persistent_owl_cache[persistent_owl_cache_size].move	         = move;
  persistent_owl_cache[persistent_owl_cache_size].move2	         = move2;
  persistent_owl_cache[persistent_owl_cache_size].tactical_nodes =
    tactical_nodes;
  persistent_owl_cache[persistent_owl_cache_size].movenum = movenum;
  
  /* Remains to set the board. We let the active area be
   * the goal +
   * distance four expansion through empty intersections and own stones +
   * adjacent opponent strings +
   * liberties of adjacent opponent strings with less than five liberties +
   * liberties of low liberty neighbors of adjacent opponent strings
   * with less than five liberties.
   */
  for (pos = BOARDMIN; pos < BOARDMAX; pos++)
    if (ON_BOARD(pos))
      active[pos] = (goal[pos] != 0);

  /* Also add critical moves to the active area. */
  if (ON_BOARD1(move))
    active[move] = 1;

  if (ON_BOARD1(move2))
    active[move2] = 1;

  /* Distance four expansion through empty intersections and own stones. */
  for (k = 1; k < 5; k++) {
    for (pos = BOARDMIN; pos < BOARDMAX; pos++){
      if (!ON_BOARD(pos) || board[pos] == other || active[pos] != 0) 
	continue;
      if ((ON_BOARD(SOUTH(pos)) && active[SOUTH(pos)] == k)
	  || (ON_BOARD(WEST(pos)) && active[WEST(pos)] == k)
	  || (ON_BOARD(NORTH(pos)) && active[NORTH(pos)] == k)
	  || (ON_BOARD(EAST(pos)) && active[EAST(pos)] == k)) {
	if (board[pos] == EMPTY)
	  active[pos] = k + 1;
	else
	  mark_string(pos, active, (char) (k + 1));
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
	mark_string(pos, active, (char) 1);
	break;
      }
    }
  }
  
  /* Liberties of adjacent opponent strings with less than five liberties +
   * liberties of low liberty neighbors of adjacent opponent strings
   * with less than five liberties.
   */
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (board[pos] == other && active[pos] != 0 && countlib(pos) < 5) {
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
	if (countlib(adjs[r]) <= 3) {
	  int s;
	  liberties = findlib(adjs[r], 3, libs);
	  for (s = 0; s < liberties; s++)
	    active[libs[s]] = 1;
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
    else if (IS_STONE(board[pos]) && countlib(pos) > 4)
      value |= HIGH_LIBERTY_BIT;
    
    persistent_owl_cache[persistent_owl_cache_size].board[pos] = value;
  }

  if (debug & DEBUG_OWL_PERSISTENT_CACHE) {
    gprintf("%o Stored result in cache (entry %d):\n",
	    persistent_owl_cache_size);
    print_persistent_owl_cache_entry(persistent_owl_cache_size);
  }
  
  persistent_owl_cache_size++;
}


/* For debugging purposes. */
static void
print_persistent_owl_cache_entry(int k)
{
  struct owl_cache *entry = &(persistent_owl_cache[k]);
  gprintf("%omovenum         = %d\n",  entry->movenum);
  gprintf("%otactical_nodes  = %d\n",  entry->tactical_nodes);
  gprintf("%oroutine         = %s\n",  routine_to_string(entry->routine));
  gprintf("%o(apos)          = %1m\n", entry->apos);
  gprintf("%o(bpos)          = %1m\n", entry->bpos);
  gprintf("%o(cpos)          = %1m\n", entry->cpos);
  gprintf("%oresult          = %d\n",  entry->result);
  gprintf("%o(move)          = %1m\n", entry->move);
  gprintf("%o(move2)         = %1m\n", entry->move2);
  
  draw_active_area(entry->board, entry->apos);
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
  for (k = 0; k < persistent_owl_cache_size; k++)
    sum_tactical_nodes += persistent_owl_cache[k].tactical_nodes;

  if (sum_tactical_nodes <= 100)
    return;

  /* Loop over all entries and increase the value of vertices adjacent
   * to dragons involving expensive owl reading.
   */
  for (k = 0; k < persistent_owl_cache_size; k++) {
    struct owl_cache *entry = &(persistent_owl_cache[k]);
    float contribution = entry->tactical_nodes / (float) sum_tactical_nodes;
    if (debug & DEBUG_OWL_PERSISTENT_CACHE) {
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
