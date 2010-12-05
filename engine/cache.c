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


#include "random.h"
#include "gnugo.h"

#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <string.h>

#include "liberty.h"
#include "cache.h"
#include "sgftree.h"


/* ================================================================ */
/*                    The transposition table                       */
/* ---------------------------------------------------------------- */

static void tt_init(Transposition_table *table, int memsize);
static void tt_clear(Transposition_table *table);

/* The transposition table itself. */
Transposition_table ttable;


/* Arrays with random numbers for Zobrist hashing of input data (other
 * than the board position). If you add an array here, do not forget
 * to also initialize it in keyhash_init() below.
 */
static Hash_data target1_hash[BOARDMAX];
static Hash_data target2_hash[BOARDMAX];
static Hash_data routine_hash[NUM_CACHE_ROUTINES];

static void
keyhash_init(void)
{
  static int is_initialized = 0;
  
  if (!is_initialized) {
    
    INIT_ZOBRIST_ARRAY(target1_hash);
    INIT_ZOBRIST_ARRAY(target2_hash);
    INIT_ZOBRIST_ARRAY(routine_hash);
    
    is_initialized = 1;
  }
}

static void
calculate_hashval_for_tt(Hash_data *hashdata, int routine, int target1,
			 int target2, Hash_data *extra_hash)
{ 
  *hashdata = board_hash;                /* from globals.c */
  hashdata_xor(*hashdata, routine_hash[routine]);
  hashdata_xor(*hashdata, target1_hash[target1]);
  if (target2 != NO_MOVE)
    hashdata_xor(*hashdata, target2_hash[target2]);
  if (extra_hash)
    hashdata_xor(*hashdata, *extra_hash);
}



/* Initialize the transposition table. Non-positive memsize means use
 * the default size of DEFAULT_NUMBER_OF_CACHE_ENTRIES entries.
 */

static void
tt_init(Transposition_table *table, int memsize)
{
  int num_entries;
 
  /* Make sure the hash system is initialized. */
  hash_init();
  keyhash_init();

  if (memsize > 0)
    num_entries = memsize / sizeof(table->entries[0]);
  else
    num_entries = DEFAULT_NUMBER_OF_CACHE_ENTRIES;

  table->num_entries = num_entries;
  table->entries     = malloc(num_entries * sizeof(table->entries[0]));

  if (table->entries == NULL) {
    perror("Couldn't allocate memory for transposition table. \n");
    exit(1);
  }

  table->is_clean = 0;
  tt_clear(table);
}


/* Clear the transposition table. */

static void
tt_clear(Transposition_table *table)
{
  if (!table->is_clean) {
    memset(table->entries, 0, table->num_entries * sizeof(table->entries[0]));
    table->is_clean = 1;
  }
}
 
 
/* Free the transposition table. */

void
tt_free(Transposition_table *table)
{
  free(table->entries);
}


/* Get result and move. Return value:
 *   0 if not found
 *   1 if found, but depth too small to be trusted.  In this case the move
 *     can be used for move ordering.
 *   2 if found and depth is enough so that the result can be trusted.
 */
 
int
tt_get(Transposition_table *table, 
       enum routine_id routine, 
       int target1, int target2, int remaining_depth,
       Hash_data *extra_hash,
       int *value1, int *value2, int *move)
{
  Hash_data hashval;
  Hashentry *entry;
  Hashnode *node;
 
  /* Sanity check. */
  if (remaining_depth < 0 || remaining_depth > HN_MAX_REMAINING_DEPTH)
    return 0;

  /* Get the combined hash value. */
  calculate_hashval_for_tt(&hashval, routine, target1, target2, extra_hash);

  /* Get the correct entry and node. */
  entry = &table->entries[hashdata_remainder(hashval, table->num_entries)];
  if (hashdata_is_equal(hashval, entry->deepest.key))
    node = &entry->deepest;
  else if (hashdata_is_equal(hashval, entry->newest.key))
    node = &entry->newest;
  else
    return 0;

  stats.read_result_hits++;

  /* Return data.  Only set the result if remaining depth in the table
   * is big enough to be trusted.  The move can always be used for move
   * ordering if nothing else.
   */
  if (move)
    *move = hn_get_move(node->data);
  if (remaining_depth <= (int) hn_get_remaining_depth(node->data)) {
    if (value1)
      *value1 = hn_get_value1(node->data);
    if (value2)
      *value2 = hn_get_value2(node->data);
    stats.trusted_read_result_hits++;
    return 2;
  }

  return 1;
}


/* Update a transposition table entry.
 */

void
tt_update(Transposition_table *table,
	  enum routine_id routine, int target1, int target2,
	  int remaining_depth, Hash_data *extra_hash, 
	  int value1, int value2, int move)
{
  Hash_data hashval;
  Hashentry *entry;
  Hashnode *deepest;
  Hashnode *newest;
  unsigned int data;
  /* Get routine costs definitions from liberty.h. */
  static const int routine_costs[] = { ROUTINE_COSTS };
  gg_assert(routine_costs[NUM_CACHE_ROUTINES] == -1);

  /* Sanity check. */
  if (remaining_depth < 0 || remaining_depth > HN_MAX_REMAINING_DEPTH)
    return;

  /* Get the combined hash value. */
  calculate_hashval_for_tt(&hashval, routine, target1, target2, extra_hash);

  data = hn_create_data(remaining_depth, value1, value2, move,
      		        routine_costs[routine]);

  /* Get the entry and nodes. */ 
  entry = &table->entries[hashdata_remainder(hashval, table->num_entries)];
  deepest = &entry->deepest;
  newest  = &entry->newest;
 
  /* See if we found an already existing node. */
  if (hashdata_is_equal(hashval, deepest->key)
      && remaining_depth >= (int) hn_get_remaining_depth(deepest->data)) {

    /* Found deepest */
    deepest->data = data;

  }
  else if (hashdata_is_equal(hashval, newest->key)
           && remaining_depth >= (int) hn_get_remaining_depth(newest->data)) {

    /* Found newest */
    newest->data = data;

    /* If newest has become deeper than deepest, then switch them. */
    if (hn_get_remaining_depth(newest->data)
	> hn_get_remaining_depth(deepest->data)) {
      Hashnode temp;

      temp = *deepest;
      *deepest = *newest;
      *newest = temp;
    }

  }
  else if (hn_get_total_cost(data) > hn_get_total_cost(deepest->data)) {
    if (hn_get_total_cost(newest->data) < hn_get_total_cost(deepest->data))
      *newest = *deepest;
    deepest->key  = hashval;
    deepest->data = data;
  } 
  else {
    /* Replace newest. */
    newest->key  = hashval;
    newest->data = data;
  }

  stats.read_result_entered++;
  table->is_clean = 0;
}


static const char *routine_names[] = {
  ROUTINE_NAMES
};

/* Convert a routine as used in the cache table to a string. */
const char *
routine_id_to_string(enum routine_id routine)
{
  return routine_names[(int) routine];
}


/* Initialize the cache for read results, using at most the given
 * number of bytes of memory. If the memory isn't sufficient to
 * allocate a single node or if the allocation fails, the caching is
 * disabled.
 */
void
reading_cache_init(int bytes)
{
  tt_init(&ttable, bytes);
}


/* Clear the cache for read results. */
void
reading_cache_clear()
{
  tt_clear(&ttable);
}

float
reading_cache_default_size()
{
  return DEFAULT_NUMBER_OF_CACHE_ENTRIES * sizeof(Hashentry) / 1024.0 / 1024.0;
}


/* Write reading trace data to an SGF file. Normally called through the
 * macro SGFTRACE in cache.h.
 */

void
sgf_trace(const char *func, int str, int move, int result,
	  const char *message)
{
  char buf[100];

  sprintf(buf, "%s %c%d: ", func, J(str) + 'A' + (J(str) >= 8),
	  board_size - I(str));
  
  if (result == 0)
    sprintf(buf + strlen(buf), "0");
  else if (ON_BOARD(move))
    sprintf(buf + strlen(buf), "%s %c%d", result_to_string(result), 
	    J(move) + 'A' + (J(move) >= 8),
	    board_size - I(move));
  else if (is_pass(move))
    sprintf(buf + strlen(buf), "%s PASS", result_to_string(result));
  else
    sprintf(buf + strlen(buf), "%s [%d]", result_to_string(result), move);

  if (message)
    sprintf(buf + strlen(buf), " (%s)", message);
  
  sgftreeAddComment(sgf_dumptree, buf);
}

/* Write two group reading (connection) trace data to an SGF file.
 * Normally called through the macro SGFTRACE2 in cache.h.
 */

void
sgf_trace2(const char *func, int str1, int str2, int move, 
           const char *result, const char *message)
{
  char buf[100];

  sprintf(buf, "%s %c%d %c%d: ", func,
	  J(str1) + 'A' + (J(str1) >= 8), board_size - I(str1),
	  J(str2) + 'A' + (J(str2) >= 8), board_size - I(str2));
  
  if (ON_BOARD(move))
    sprintf(buf + strlen(buf), "%s %c%d", result,
	    J(move) + 'A' + (J(move) >= 8),
	    board_size - I(move));
  else if (is_pass(move))
    sprintf(buf + strlen(buf), "%s PASS", result);
  else
    sprintf(buf + strlen(buf), "%s [%d]", result, move);

  if (message)
    sprintf(buf + strlen(buf), " (%s)", message);
  
  sgftreeAddComment(sgf_dumptree, buf);
}

/* Write semeai reading trace data to an SGF file. Normally called
 * through the macro SGFTRACE_SEMEAI in cache.h.
 */

void
sgf_trace_semeai(const char *func, int str1, int str2, int move, 
		 int result1, int result2, const char *message)
{
  char buf[100];

  sprintf(buf, "%s %c%d %c%d: ", func,
	  J(str1) + 'A' + (J(str1) >= 8), board_size - I(str1),
	  J(str2) + 'A' + (J(str2) >= 8), board_size - I(str2));
  
  if (ON_BOARD(move))
    sprintf(buf + strlen(buf), "%s %s %c%d",
	    result_to_string(result1), result_to_string(result2),
	    J(move) + 'A' + (J(move) >= 8), board_size - I(move));
  else if (is_pass(move))
    sprintf(buf + strlen(buf), "%s %s PASS",
	    result_to_string(result1), result_to_string(result2));
  else
    sprintf(buf + strlen(buf), "%s %s [%d]",
	    result_to_string(result1), result_to_string(result2),
	    move);

  if (message)
    sprintf(buf + strlen(buf), " (%s)", message);
  
  sgftreeAddComment(sgf_dumptree, buf);
}

/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
