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

static Read_result *hashnode_search(Hashnode *node, int routine, int komaster,
				    int kom_pos, int str1, int str2);
static Read_result *hashnode_new_result(Hashtable *table, Hashnode *node, 
					int routine, int komaster,
					int kom_pos, int str1, int str2);

static void hashtable_unlink_closed_results(Hashnode *node, 
					    int exclusions, 
					    unsigned int stackplimit,
					    int statistics[][20]);
static void hashtable_partially_clear(Hashtable *table);
static int do_get_read_result(int routine, int komaster, int kom_pos,
			      int str1, int str2, Read_result **read_result);

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
	  rr_get_stackp(*result));
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
  fprintf(outfile, "Total size: %d\n", table->num_nodes);
  fprintf(outfile, "Size of hash table: %d\n", table->hashtablesize);
  fprintf(outfile, "Number of positions in table: %d\n", table->free_node);

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
  table->num_nodes = num_nodes;
  table->all_nodes = (Hashnode *) malloc(num_nodes * sizeof(Hashnode));
  if (table->all_nodes == NULL) {
    free(table->hashtable);
    free(table);
    return 0;
  }

  /* Allocate memory for the results. */
  table->num_results = num_results;
  table->all_results = (Read_result *) malloc(num_results 
					      * sizeof(Read_result));
  if (table->all_results == NULL) {
    free(table->hashtable);
    free(table->all_nodes);
    free(table);
    return 0;
  }

  /* Initialize the table and all nodes to the empty state . */
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
  int i;
  
  if (!table)
    return;
  
  /* Initialize all hash buckets to the empty list. */
  for (bucket = 0; bucket < table->hashtablesize; ++bucket)
    table->hashtable[bucket] = NULL;

  /* Mark all read_results as free. */
  for (i = 0; i < table->num_results; i++)
    table->all_results[i].data2 = 0;
  
  /* Mark all nodes as free. */
  for (i = 0; i < table->num_nodes; i++)
    table->all_nodes[i].results = NULL;
  
  table->free_node = 0;
  table->free_result = 0;
}


/*
 * Unlink the closed results from the linked list of results at a node.
 */

static void
hashtable_unlink_closed_results(Hashnode *node, 
				int exclusions, unsigned int stackplimit,
				int statistics[][20])
{
  Read_result *previous_result = NULL;
  Read_result *current_result = node->results;
  
  while (current_result != NULL) {
    int stackp;
    int routine;

    stackp = depth - rr_get_stackp(*current_result);
    if (stackp > 19)
      stackp = 19;
    if (stackp < 0)
      stackp = 0;

    routine = rr_get_routine(*current_result);
    gg_assert(routine >= 0 && routine < NUM_ROUTINES);
    statistics[routine][stackp]++;

    if (rr_get_status(*current_result) == 2
	&& ((1 << rr_get_routine(*current_result)) & exclusions) == 0
	&& depth - rr_get_stackp(*current_result) >= stackplimit) {
      if (previous_result == NULL)
	node->results = current_result->next;
      else
	previous_result->next = current_result->next;
      current_result->data2 = 0;
    }
    else
      previous_result = current_result;

    current_result = current_result->next;
  }
}



/*
 * Clear an existing hash table except for open nodes.
 *
 * Don't even think about compressing the node and results arrays
 * afterwards in order to simplify distribution of new nodes and
 * results. The read result pointers out in reading.c and owl.c will
 * never know that you moved them around.
 */

static void
hashtable_partially_clear(Hashtable *table)
{
  Hashnode *node;
  int bucket;
  Hashnode *previous;
  Hashnode *current;
  int k, l;
  
  int statistics[NUM_ROUTINES][20];

  TRACE_READING_PERFORMANCE(
	"Hashtable cleared because it was full.\n");

  for (k = 0; k < NUM_ROUTINES; ++k)
    for (l = 0; l < 20; ++l)
      statistics[k][l] = 0;

  /* Walk through all_nodes. Closed nodes are unlinked from the
   * linked lists and marked as free.
   */
  for (k = 0; k < table->num_nodes; k++) {
    node = &(table->all_nodes[k]);

    /* If there are no results attached, this node is not in the table. */
    if (node->results == NULL)
      continue;

    bucket = node->key.hashval[0] % table->hashtablesize;
    previous = NULL;
    current = table->hashtable[bucket];

    /* Remove all closed results for this node except OWL_{ATTACK,DEFEND}. */
    hashtable_unlink_closed_results(node, 
				    (1 << OWL_ATTACK | 1 << OWL_DEFEND
				     | 1 << SEMEAI), 3,
				    statistics);
    if (node->results != NULL)
      continue;

    /* Find the node in the linked list and unlink. */
    while (current != NULL) {
      if (current != node) {
	previous = current;
	current = current->next;
      }
      else {
	if (previous == NULL)
	  table->hashtable[bucket] = current->next;
	else
	  previous->next = current->next;
	break;
      }
    }
  }

  if (debug & DEBUG_READING_PERFORMANCE) {
    /* FIXME: These names should be where the constants are defined. */
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
   */
  table->free_result = 0;
  table->free_node = 0;
}


/*
 * Enter a position with a given hash value into the table.  Return 
 * a pointer to the hash node where it was stored.  If it is already
 * there, don't enter it again, but return a pointer to the old one.
 */

static Hashnode *
hashtable_enter_position(Hashtable *table, Hash_data *hd)
{
  Hashnode *node;
  int bucket;

  /* If the position is already in the table, return a pointer to it. */
  node = hashtable_search(table, hd);
  if (node != NULL) {
    return node;
  }

  /* If the next node is not free, skip until we find one which is free. */
  while (table->free_node < table->num_nodes
	 && table->all_nodes[table->free_node].results != NULL)
    table->free_node++;
  
  /* If the table is full, return NULL */
  if (table->free_node == table->num_nodes)
    return NULL;

  /* It wasn't there and there is still room. Allocate a new node for it... */
  node = &(table->all_nodes[table->free_node++]);
  node->key = *hd;
  node->results = NULL;

  /* ...and enter it into the table. */
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
      break;
  }
  return node;
}


/* 
 * Search the result list in a hash node for a particular result. This
 * result is from ROUTINE (e.g. readlad1) at (str1) and reading depth
 * stackp.
 *
 * All these numbers must be unsigned, and 0 <= x <= 255).
 */

static Read_result *
hashnode_search(Hashnode *node, int routine, int komaster, int kom_pos,
		int str1, int str2)
{
  Read_result *result;
  unsigned int search_for1;
  unsigned int search_for2;

  search_for1 = rr_input_data1(routine, komaster, kom_pos, str1,
			       depth - stackp);
  search_for2 = rr_input_data2(str2);

  for (result = node->results; result != NULL; result = result->next) {
    if (result->data1 == search_for1
	&& (result->data2 & RR_INPUT_DATA2) == search_for2)
      break;
    }

  return result;
}


/*
 * Enter a new Read_result into a Hashnode.
 * We already have the node, now we just want to enter the result itself.
 * We will fill in the result itself later, so we only need the routine
 * number for now.
 */

static Read_result *
hashnode_new_result(Hashtable *table, Hashnode *node, int routine, 
		    int komaster, int kom_pos, int str1, int str2)
{
  Read_result *result;

  /* If the next result is not free, skip until we find one which is free. */
  while (table->free_result < table->num_results
	 && rr_get_status(table->all_results[table->free_result]) != 0)
    table->free_result++;
  
  /* If the table is full, return NULL */
  if (table->free_result == table->num_results)
    return NULL;

  /* There is still room. Allocate a new node for it... */
  result = &(table->all_results[table->free_result++]);

  /* ...and enter it into the table. */
  result->next = node->results;
  node->results = result;

  /* Now, put the input data into it. This also sets status to open. */
  rr_set_input_data2(*result, routine, komaster, kom_pos, str1, str2,
		     depth - stackp);

  stats.read_result_entered++;
  return result;

}

/* Initialize the cache for read results, using at most the given
 * number of bytes of memory. If the memory isn't sufficient to
 * allocate a single node or if the allocation fails, the caching is
 * disabled.
 */
void
reading_cache_init()
{
  /* Initialize hash table.
   *
   * The number 1.4 below is the quotient between the number of nodes
   * and the number of read results.  It was found in a test that this 
   * number varies between 1.15 and 1.4.  Thus we use 1.4.
   */
  int nodes = READING_CACHE_ENTRIES;

  if (sizeof(Hashvalue) != SIZEOF_LONG) {
    fprintf(stderr, "Warning: SIZEOF_LONG was set incorrectly, exiting.\n");
    abort(); 
  }

  if (0)
    gprintf("Allocated memory for %d hash nodes. \n", (int) nodes);
  movehash = hashtable_new((int) (1.5 * nodes),  /* table size   */
			   (int) nodes,          /* nodes        */
			   (int) (1.4 * nodes)); /* read results */
  
  if (!movehash) {
    fprintf(stderr,
	    "Warning: failed to allocate hashtable, exiting.\n");
    abort();
  }
}

/* Clear the cache for read results. */
void
reading_cache_clear()
{
  hashtable_clear(movehash);
}

/*
 * Return a Read_result for the current position, routine and location.
 * For performance, the location is changed to the origin of the string.
 */

int
get_read_result(int routine, int komaster, int kom_pos, int *str,
		Read_result **read_result)
{
  int result;
  /* Only store the result if stackp <= depth. Above that, there
   * is no branching, so we won't gain anything.
   */
  if (stackp > depth) {
    *read_result = NULL;
    return 0;
  }
  
  /* Find the origin of the string containing (si, sj),
   * in order to make the caching of read results work better.
   */
  *str = find_origin(*str);
  
  result = do_get_read_result(routine, komaster, kom_pos, *str, NO_MOVE,
			      read_result);
  if (*read_result == NULL) {
    /* Clean up the hashtable and try once more. */
    hashtable_partially_clear(movehash);
    result = do_get_read_result(routine, komaster, kom_pos, *str, NO_MOVE,
				read_result);
  }
  return result;
}

/*
 * Variant with two calling strings.
 */

int
get_read_result2(int routine, int komaster, int kom_pos, int *str1, int *str2,
		 Read_result **read_result)
{
  int result;
  /* Only store the result if stackp <= depth. Above that, there
   * is no branching, so we won't gain anything.
   */
  if (stackp > depth) {
    *read_result = NULL;
    return 0;
  }
  
  /* Find the origin of the string containing (si, sj),
   * in order to make the caching of read results work better.
   */
  *str1 = find_origin(*str1);
  *str2 = find_origin(*str2);
  
  result = do_get_read_result(routine, komaster, kom_pos, *str1, *str2,
			      read_result);
  if (*read_result == NULL) {
    /* Clean up the hashtable and try once more. */
    hashtable_partially_clear(movehash);
    result = do_get_read_result(routine, komaster, kom_pos, *str1, *str2,
				read_result);
  }
  return result;
}


static int
do_get_read_result(int routine, int komaster, int kom_pos,
		   int str1, int str2, Read_result **read_result)
{
  Hashnode *hashnode;
  int retval;

  /* Find this position in the table.  If it wasn't found, enter it. */
  hashnode = hashtable_search(movehash, &hashdata);
  if (hashnode != NULL) {
    stats.position_hits++;
    TRACE_READING_CACHE("We found position %H in the hash table...\n",
	  (unsigned long) hashdata.hashval);
  }
  else {
    hashnode = hashtable_enter_position(movehash, &hashdata);
    if (hashnode)
      TRACE_READING_CACHE("Created position %H in the hash table...\n",
	    (unsigned long) hashdata.hashval);
  }

  retval = 0;
  if (hashnode == NULL) {
    /* No hash node, so we can't enter a result into it. */
    *read_result = NULL;

  }
  else {

    /* We found it!  Now see if we can find a previous result. */
    *read_result = hashnode_search(hashnode, routine, komaster, kom_pos,
				   str1, str2);

    if (*read_result != NULL) {
      stats.read_result_hits++;
      retval = 1;
    }
    else {
      TRACE_READING_CACHE(
	    "...but no previous result for routine %d and (%1m, %1m)...",
	    routine, str1, str2);

      *read_result = hashnode_new_result(movehash, hashnode, routine,
					 komaster, kom_pos, str1, str2);
      
      if (*read_result == NULL)
	TRACE_READING_CACHE(
	      "%o...and unfortunately there was no room for one.\n");
      else
	TRACE_READING_CACHE("%o...so we allocate a new one.\n");
    }
  }

  return retval;
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

/* Write two group reading (connection or semeai) trace data to an SGF
 * file. Normally called through the macro SGFTRACE2 in cache.h.
 */

void
sgf_trace2(const char *func, int str1, int str2, int move, int result,
	   const char *message)
{
  char buf[100];

  sprintf(buf, "%s %c%d %c%d: ", func,
	  J(str1) + 'A' + (J(str1) >= 8), board_size - I(str1),
	  J(str2) + 'A' + (J(str2) >= 8), board_size - I(str2));
  
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

/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
