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


#include "gnugo.h"

#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <string.h>

#include "liberty.h"
#include "hash.h"
#include "cache.h"
#include "sgftree.h"

static Hashtable *movehash;

static int hashtable_init(Hashtable *table, int tablesize, int num_nodes,
			  int num_results);
static Hashtable *hashtable_new(int tablesize, int num_nodes, int num_results);
static void hashtable_clear(Hashtable *table);

static Hashnode *hashtable_enter_position(Hashtable *table, Hash_data *hd);
static Hashnode *hashtable_search(Hashtable *table, Hash_data *hd);

static Read_result *hashnode_search(Hashnode *node,
				    unsigned int query_data1,
				    unsigned int query_data2);
static Read_result *hashnode_new_result(Hashtable *table, Hashnode *node, 
					unsigned int input_data1,
					unsigned int input_data2);

static void hashnode_unlink_closed_results(Hashnode *node, 
					   int exclusions, 
					   unsigned int remaining_depth_limit,
					   int statistics[][20]);
static void hashtable_partially_clear(Hashtable *table);
static int do_get_read_result(int routine, int komaster, int kom_pos,
			      int str1, int str2, Read_result **read_result,
		   	      Hash_data *hashmodifier);


/*
 * Dump an ASCII representation of the contents of a Read_result onto
 * the FILE outfile. 
 */
void
read_result_dump(Read_result *result, FILE *outfile)
{
  fprintf(outfile, "Komaster %u (%d, %d) Routine %u, (%d, %d), depth: %u ",
	  rr_get_komaster(*result),
	  I(rr_get_kom_pos(*result)),
	  J(rr_get_kom_pos(*result)),
	  rr_get_routine(*result),
	  I(rr_get_str(*result)),
	  J(rr_get_str(*result)),
	  rr_get_remaining_depth(*result));
  fprintf(outfile, "Result: %u %u, (%d, %d)\n",
	  rr_get_status(*result),
	  rr_get_result(*result),
	  I(rr_get_move(*result)),
	  J(rr_get_move(*result)));
}


/*
 * Dump an ASCII representation of the contents of a Hashnode onto
 * the FILE outfile. 
 */

void
hashnode_dump(Hashnode *node, FILE *outfile)
{
  Read_result *result;

  /* Data about the node itself. */
  fprintf(outfile, "Hash value: %lx\n", (unsigned long) node->key.hashval);
#if FULL_POSITION_IN_HASH
  hashposition_dump(&(node->key.hashpos), outfile);
#endif

  for (result = node->results; result != NULL; result = result->next) {
    read_result_dump(result, outfile);
  }
  /* FIXME: Dump contents of data also. */
}


/*
 * Dump an ASCII representation of the contents of a Hashtable onto
 * the FILE outfile. 
 */

void
hashtable_dump(Hashtable *table, FILE *outfile)
{
  int i;
  Hashnode *hn;

  /* Data about the table itself. */
  fprintf(outfile, "Dump of hashtable\n");
  fprintf(outfile, "Total size: %d\n", (int) (table->node_limit
					      - table->all_nodes));
  fprintf(outfile, "Size of hash table: %d\n", table->hashtablesize);
  fprintf(outfile, "Number of positions in table: %d\n",
	  (int) (table->free_node - table->all_nodes));

  /* Data about the contents. */
  for (i = 0; i < table->hashtablesize; ++i) {
    fprintf(outfile, "Bucket %5d: ", i);
    hn = table->hashtable[i];
    if (hn == NULL)
      fprintf(outfile, "empty");
    else
      while (hn) {
	hashnode_dump(hn, outfile);
	hn = hn->next;
      }
    fprintf(outfile, "\n");
  }
}


#if 0
/*
 * Dump an alternative representation of the contents of a Hashtable
 * onto the FILE outfile. This one is mainly useful if you have to
 * debug the hashtable implementation itself.
 */

static void
hashtable_dump2(Hashtable *table, FILE *outfile)
{
  int i;
  Hashnode *node;
  Read_result *result;

  for (i = 0; i < table->hashtablesize; i++) {
    fprintf(outfile, "bucket %d: ", i);
    if (table->hashtable[i] == NULL)
      fprintf(outfile, "NULL\n");
    else
      fprintf(outfile, "%d\n", table->hashtable[i] - table->all_nodes);
  }

  for (node = table->all_nodes; node < table->node_limit; node++) {
    if (node->results == NULL)
      continue;
    fprintf(outfile, "node %d: ", i);
    if (node->results == NULL)
      fprintf(outfile, "NULL ");
    else
      fprintf(outfile, "%d ", node->results - table->all_results);
    if (node->next == NULL)
      fprintf(outfile, "NULL\n");
    else
      fprintf(outfile, "%d\n", node->next - table->all_nodes);
  }

  for (result = table->all_results; result < table->result_limit; result++) {
    if (rr_get_status(*result) == 0)
      continue;
    fprintf(outfile, "result %d ", i);
    if (result->next == NULL)
      fprintf(outfile, "NULL ");
    else
      fprintf(outfile, "%d ", result->next - table->all_results);
    read_result_dump(result, outfile);
  }
}
#endif


/*
 * Initialize a hash table for a given total size and size of the
 * hash table.
 *
 * Return 0 if something went wrong.  Just now this means that there
 * wasn't enough memory available.
 */

static int
hashtable_init(Hashtable *table,
	       int tablesize, int num_nodes, int num_results)
{
  /* Make sure the hash system is initialized. */
  hash_init();

  /* Allocate memory for the pointers in the hash table proper. */
  table->hashtablesize = tablesize;
  table->hashtable = (Hashnode **) malloc(tablesize * sizeof(Hashnode *));
  if (table->hashtable == NULL) {
    free(table);
    return 0;
  }

  /* Allocate memory for the nodes. */
  table->all_nodes = (Hashnode *) malloc(num_nodes * sizeof(Hashnode));
  if (table->all_nodes == NULL) {
    free(table->hashtable);
    free(table);
    return 0;
  }
  table->node_limit = table->all_nodes + num_nodes;

  /* Allocate memory for the results. */
  table->all_results = (Read_result *) malloc(num_results 
					      * sizeof(Read_result));
  if (table->all_results == NULL) {
    free(table->hashtable);
    free(table->all_nodes);
    free(table);
    return 0;
  }
  table->result_limit = table->all_results + num_results;

  /* Force complete table clearing. */
  table->first_pass = 0;
  hashtable_clear(table);

  return 1;
}


/*
 * Allocate a new hash table and return a pointer to it. 
 *
 * Return NULL if there is insufficient memory.
 */

static Hashtable *
hashtable_new(int tablesize, int num_nodes, int num_results)
{
  Hashtable *table;

  /* Make sure the hash system is initialized. */
  hash_init();

  /* Allocate the hashtable struct. */
  table = (Hashtable *) malloc(sizeof(Hashtable));
  if (table == NULL)
    return NULL;

  /* Initialize the table. */
  if (!hashtable_init(table, tablesize, num_nodes, num_results)) {
    free(table);
    return NULL;
  }

  return table;
}


/*
 * Clear an existing hash table.  
 */

static void
hashtable_clear(Hashtable *table)
{
  int bucket;
  Hashnode *node;
  Hashnode *node_limit;
  Read_result *result;
  Read_result *result_limit;

  if (!table)
    return;

  /* If the table is alredy clean, return immediatly. */
  if (table->first_pass && table->free_node == table->all_nodes)
    return;

  /* Initialize all hash buckets to the empty list. */
  for (bucket = 0; bucket < table->hashtablesize; ++bucket)
    table->hashtable[bucket] = NULL;

  /* Mark all nodes as free. Don't clean non-allocated nodes. */
  node_limit = table->first_pass ? table->free_node : table->node_limit;
  table->free_node = table->all_nodes;
  for (node = table->all_nodes; node < node_limit; node++)
    node->results = NULL;

  /* Mark all read_results as free. Don't clean non-allocated results. */
  result_limit = table->first_pass ? table->free_result : table->result_limit;
  table->free_result = table->all_results;
  for (result = table->all_results; result < result_limit; result++)
    result->data2 = 0;

  table->first_pass = 1;
}


/* Unlink all closed results except for those which has `routine' value marked
 * in `exceptions' or large enough `remaining_depth' from the linked list of
 * results at a node. It is assumed that the node contains at least one result.
 */
static void
hashnode_unlink_closed_results(Hashnode *node, 
			       int exclusions,
			       unsigned int remaining_depth_limit,
			       int statistics[][20])
{
  Read_result *result = node->results;
  Read_result **link = &node->results;

  /* Traverse all node results. */
  do {
    unsigned int result_remaining_depth = rr_get_remaining_depth(*result);
    int result_routine = rr_get_routine(*result);

    if (debug & DEBUG_READING_PERFORMANCE) {
      int stat_stackp = depth - result_remaining_depth;

      if (stat_stackp > 19)
	stat_stackp = 19;
      if (stat_stackp < 0)
	stat_stackp = 0;

      gg_assert(result_routine >= 0 && result_routine < NUM_ROUTINES);
      statistics[result_routine][stat_stackp]++;
    }

    if (rr_is_closed(*result)
	&& result_remaining_depth <= remaining_depth_limit
	&& ((1 << result_routine) & exclusions) == 0) {
      /* Unlink the result and mark it as free. */
      *link = result->next;
      result->data2 = 0;
    }
    else
      link = &result->next;

    result = result->next;
  } while (result != NULL);
}


/*
 * Clear an existing hash table except for open nodes.
 *
 * Don't even think about compressing the node and results arrays
 * afterwards in order to simplify distribution of new nodes and
 * results. The read result pointers out in reading.c and owl.c will
 * never know that you moved them around.
 *
 * (comment) The above is only true about open results. All nodes can
 *	     be moved as long as you fix links as well. However, it's
 *	     doubtful that moving nodes will help anything.
 */

static void
hashtable_partially_clear(Hashtable *table)
{
  int k, l;
  Hashnode *node;
  const int remaining_depth_limit = depth - 3;

  int statistics[NUM_ROUTINES][20];

  if (debug & DEBUG_READING_PERFORMANCE) {
    gprintf("Hashtable cleared because it became full.\n");

    for (k = 0; k < NUM_ROUTINES; ++k)
      for (l = 0; l < 20; ++l)
	statistics[k][l] = 0;
  }

  /* Walk through all_nodes. Since we free most of the nodes, it might seem
   * faster to walk through all buckets. This approach really speeds up this
   * function, but drastically slows down hashnode_unlink_closed_results()
   * for in the first case results go almost continuously in the memory and
   * in the second they are scattered randomly - very bad for memory caching.
   */
  for (node = table->all_nodes; node < table->node_limit; node++) {
    /* If there are no results attached, this node is not in the table. */
    if (node->results == NULL)
      continue;

    /* Remove all closed results for this node except OWL_{ATTACK,DEFEND} 
     * and SEMEAI.
     */
    hashnode_unlink_closed_results(node, 
				   (1 << OWL_ATTACK | 1 << OWL_DEFEND
				    | 1 << SEMEAI), remaining_depth_limit,
				   statistics);

    if (node->results == NULL) {
      int bucket = node->key.hashval[0] % table->hashtablesize;
      Hashnode *bucket_node = table->hashtable[bucket];
      Hashnode **link = &table->hashtable[bucket];

      /* Since all node results has been freed, we can free the node itself. */
      while (bucket_node != node) {
	link = &bucket_node->next;
	bucket_node = bucket_node->next;
      }

      /* Unlink the node. It is already free, since node->results == NULL. */
      *link = node->next;
    }
  }

  if (debug & DEBUG_READING_PERFORMANCE) {
    /* FIXME: These names should be where the constants are defined.
     *	      They also look a bit outdated, check against cache.h.
     */
    const char *routines[] = {
      "find_defense", "defend1",    "defend2", "defend3",
      "defend4",      "attack",     "attack2", "attack3",
      "owl_attack",   "owl_defend", "",        "",
      "",             "",           "",        "",
    };
    int total;

    fprintf(stderr, "routine        total     0     1     2     3     4     5     6     7     8     9    10    11    12    13    14    15    16    17    18    19\n");

    for (k = 0; k < NUM_ROUTINES; ++k) {
      total = 0; 
      for (l = 0; l < 20; ++l)
	total += statistics[k][l];

      if (total == 0)
	continue;

      fprintf(stderr, "%-14s%6d", routines[k], total);
      for (l = 0; l < 20; ++l)
	fprintf(stderr, "%6d", statistics[k][l]);
      fprintf(stderr, "\n");
    }
  }

  /* FIXME: This is not entirely safe although it probably works more
   * than 99.999% of all cases.  If result no 0 or node no 0 is not
   * free after the partial clearing, this will explode into our face.
   *
   * (answer) We check if a node or result is free before allocating,
   *	      so these statements are ok.
   */
  table->free_node = table->all_nodes;
  table->free_result = table->all_results;

  table->first_pass = 0;
}


/*
 * Enter a position with a given hash value into the table.  Return 
 * a pointer to the hash node where it was stored. It is assumed that
 * there is no such position in the table yet.
 */
static Hashnode *
hashtable_enter_position(Hashtable *table, Hash_data *hd)
{
  Hashnode *node;
  int bucket;

  /* If the first node is not free, skip until we find one which is free. */
  while (table->free_node < table->node_limit
	 && table->free_node->results != NULL)
    table->free_node++;

  if (table->free_node == table->node_limit) {
    /* If the table is full, try to clean it up. */
    hashtable_partially_clear(table);

    while (table->free_node < table->node_limit
	   && table->free_node->results != NULL)
      table->free_node++;

    if (table->free_node == table->node_limit) {
      /* This shouldn't happen, at least with reasonably large tables. */
      return NULL;
    }
  }

  /* We have found a free node. Allocate it for the position. */
  node = table->free_node++;
  node->key = *hd;
  node->results = NULL;

  /* And link it into the corresponding bucket list. */
  bucket = hd->hashval[0] % table->hashtablesize;
  node->next = table->hashtable[bucket];
  table->hashtable[bucket] = node;

  stats.position_entered++;
  return node;
}


/* 
 * Given a Hashposition and a Hash value, find the hashnode which contains
 * this very position with the given hash value.  
 *
 * We could compute the hash value within this functions, but later
 * when we have incremental calculation of the hash function, this 
 * would be dumb. So we demand the hash value from outside from the 
 * very beginning.
 */

static Hashnode *
hashtable_search(Hashtable *table, Hash_data *hd)
{
  Hashnode *node;
  int bucket;
  int i;

  bucket = hd->hashval[0] % table->hashtablesize;
  for (node = table->hashtable[bucket]; node != NULL; node = node->next) {
    if (node->key.hashval[0] != hd->hashval[0])
      continue;
    for (i = 1; i < NUM_HASHVALUES; i++)
      if (node->key.hashval[i] != hd->hashval[i]) {
	stats.hash_collisions++;
	break;
      }
    if (i >= NUM_HASHVALUES)
#if FULL_POSITION_IN_HASH
      if (hashposition_compare(&hd->hashpos, &node->key.hashpos) == 0)
	break;
#else
      break;
#endif
  }

  return node;
}


/* Search the result list in a hash node for a particular result. This
 * function accepts parameters in the same form as they are stored in
 * Read_result structure. Use rr_input_data1() and rr_input_data2()
 * macros to evaluate them.
 */
static Read_result *
hashnode_search(Hashnode *node,
		unsigned int query_data1, unsigned int query_data2)
{
  Read_result *result;

  for (result = node->results; result != NULL; result = result->next) {
    if (result->data1 == query_data1
	&& (result->data2 & RR_INPUT_DATA2) == query_data2)
      break;
    }

  return result;
}


/* Enter a new Read_result into a Hashnode.
 * We already have the node, now we just want to enter the result itself.
 * The result is completed later. This function enters search information
 * only (target string(s), routine, ko information, remaining depth).
 */
static Read_result *
hashnode_new_result(Hashtable *table, Hashnode *node,
		    unsigned int input_data1, unsigned int input_data2)
{
  Read_result *result;

  /* If the first result is not free, skip until we find one which is free. */
  while (table->free_result < table->result_limit
	 && !rr_is_free(*(table->free_result)))
    table->free_result++;
  
  if (table->free_result == table->result_limit) {
    Read_result *node_results = node->results;

    /* If the table is full, try to clean it up. */
    hashtable_partially_clear(table);

    if (node_results != NULL && node->results == NULL) {
      /* If the node got freed, we need to reallocate it. */
      int bucket = node->key.hashval[0] % table->hashtablesize;
      node->next = table->hashtable[bucket];
      table->hashtable[bucket] = node;
    }

    while (table->free_result < table->result_limit
	   && !rr_is_free(*(table->free_result)))
      table->free_result++;

    if (table->free_result == table->result_limit) {
      /* This shouldn't happen, at least with reasonably large tables. */
      return NULL;
    }
  }

  /* We have found a free result entry. Allocate and initialize it. */
  result = table->free_result++;
  result->data1 = input_data1;
  result->data2 = input_data2 | RR_STATUS_OPEN;

  /* Link the result into the node's list. */
  result->next = node->results;
  node->results = result;

  stats.read_result_entered++;
  return result;
}


/* Initialize the cache for read results, using at most the given
 * number of bytes of memory. If the memory isn't sufficient to
 * allocate a single node or if the allocation fails, the caching is
 * disabled.
 */
void
reading_cache_init(int bytes)
{
  /* Initialize hash table.
   *
   * The number 1.4 below is the quotient between the number of nodes
   * and the number of read results.  It was found in a test that this 
   * number varies between 1.15 and 1.4.  Thus we use 1.4.
   */
  float nodes = ((float) bytes
		 / (1.5 * sizeof(Hashnode *)
		    + sizeof(Hashnode)
		    + 1.4 * sizeof(Read_result)));
  if (0)
    gprintf("Allocated memory for %d hash nodes. \n", (int) nodes);
  /* If we get a zero size hash table, disable hashing completely. */
  if (nodes < 1.0)
    hashflags = HASH_NOTHING;
  movehash = hashtable_new((int) (1.5 * nodes),  /* table size   */
			   (int) nodes,          /* nodes        */
			   (int) (1.4 * nodes)); /* read results */
  
  if (!movehash) {
    fprintf(stderr,
	    "Warning: failed to allocate hashtable, caching disabled.\n");
    hashflags = HASH_NOTHING;
  }
}


/* Clear the cache for read results. */
void
reading_cache_clear()
{
  hashtable_clear(movehash);
}

int
get_read_result_hash_modified(int routine, int komaster, int kom_pos,
    			      int *str, Hash_data *hashmodifier,
			      Read_result **read_result)
{
  /* Only store the result if stackp <= depth. Above that, there
   * is no branching, so we won't gain anything.
   */
  if (stackp > depth) {
    *read_result = NULL;
    return 0;
  }
  
  /* Find the origin of the string in order to make the caching of read
   * results work better.
   */
  *str = find_origin(*str);
  
  return do_get_read_result(routine, komaster, kom_pos, *str, NO_MOVE,
			    read_result, hashmodifier);
}

/*
 * Return a Read_result for the current position, routine and location.
 * For performance, the location is changed to the origin of the string.
 */
int
get_read_result(int routine, int komaster, int kom_pos, int *str,
		Read_result **read_result)
{
  /* Only store the result if stackp <= depth. Above that, there
   * is no branching, so we won't gain anything.
   */
  if (stackp > depth) {
    *read_result = NULL;
    return 0;
  }
  
  /* Find the origin of the string in order to make the caching of read
   * results work better.
   */
  *str = find_origin(*str);
  
  return do_get_read_result(routine, komaster, kom_pos, *str, NO_MOVE,
			    read_result, NULL);
}


/*
 * Variant with two calling strings.
 */
int
get_read_result2(int routine, int komaster, int kom_pos, int *str1, int *str2,
		 Read_result **read_result)
{
  /* Only store the result if stackp <= depth. Above that, there
   * is no branching, so we won't gain anything.
   */
  if (stackp > depth) {
    *read_result = NULL;
    return 0;
  }
  
  /* Find the origin of the strings in order to make the caching of read
   * results work better.
   */
  *str1 = find_origin(*str1);
  *str2 = find_origin(*str2);
  
  return do_get_read_result(routine, komaster, kom_pos, *str1, *str2,
			    read_result, NULL);
}


static int
do_get_read_result(int routine, int komaster, int kom_pos,
		   int str1, int str2, Read_result **read_result,
		   Hash_data *hashmodifier)
{
  Hashnode *node;
  unsigned int data1 = rr_input_data1(routine, komaster, kom_pos,
				      str1, depth - stackp);
  unsigned int data2 = rr_input_data2(str2);
  Hash_data modified_hash;

#if CHECK_HASHING
  Hash_data    key;

  /* Assert that hash data really corresponds to the state of the board. */
  hashdata_recalc(&key, board, board_ko_pos);
#if FULL_POSITION_IN_HASH
  gg_assert(hashdata_diff_dump(&key, &hashdata) == 0);
#else
  gg_assert(hashdata_compare(&key, &hashdata) == 0);
#endif

#endif /* CHECK_HASHING */

  if (hashmodifier)
    modified_hash = xor_hashvalues(&hashdata, hashmodifier);
  else
    modified_hash = hashdata;

  /* First try to look this position up in the table. */
  node = hashtable_search(movehash, &modified_hash);
  if (node != NULL) {
    Read_result *result;

    stats.position_hits++;
    DEBUG(DEBUG_READING_CACHE, "We found position %H in the hash table...\n",
	  (unsigned long) hashdata.hashval);

    /* The position is found. So, maybe the result is already in the table? */
    result = hashnode_search(node, data1, data2);
    if (result != NULL) {
      *read_result = result;
      return 1;
    }

    DEBUG(DEBUG_READING_CACHE,
	  "...but no previous result for routine %d and (%1m, %1m)...",
	  routine, str1, str2);
  }
  else {
    node = hashtable_enter_position(movehash, &modified_hash);
    if (node) {
      DEBUG(DEBUG_READING_CACHE, "Created position %H in the hash table...\n",
	    (unsigned long) modified_hash.hashval);
    }
    else {
      DEBUG(DEBUG_READING_CACHE, "Unable to clean up the hash table!\n");
      *read_result = NULL;
      return 0;
    }
  }

  /* Enter the result into the table. */
  *read_result = hashnode_new_result(movehash, node, data1, data2);
      
  if (*read_result != NULL)
    DEBUG(DEBUG_READING_CACHE, "...allocated a new result.\n");
  else {
    /* If this ever happens and the node contains no results (can only be true
     * if the node is newly allocated), we unlink the node from its bucket.
     */
    if (node->results == NULL) {
      int bucket = node->key.hashval[0] % movehash->hashtablesize;
      movehash->hashtable[bucket] = node->next;
    }

    DEBUG(DEBUG_READING_CACHE, "Unable to clean up the hash table!\n");
  }

  return 0;
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
