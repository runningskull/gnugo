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

#include <stdio.h>
#include "hash.h"

/*
 * This file, together with engine/hash.c implements hashing of go positions
 * using a method known as Zobrist hashing.  See the Texinfo documentation
 * (Reading/Hashing) for more information.  
 */

#ifndef _CACHE_H_
#define _CACHE_H_

/*
 * This struct contains the attack / defense point and the result.
 * It is kept in a linked list, and each position has a list of 
 * these.
 *
 * When a new result node is created, 'status' is set to 1 'open'.
 * This is then set to 2 'closed' when the result is entered. The main
 * use for this is to identify open result nodes when the hashtable is
 * partially cleared. Another potential use for this field is to
 * identify repeated positions in the reading, in particular local
 * double or triple kos.
 *
 * The data1 field packs into 32 bits the following
 * fields:
 *
 * komaster:  3 bits (EMPTY, BLACK, WHITE, or GRAY)
 * kom_pos : 10 bits (allows MAX_BOARD up to 31)
 * routine :  4 bits (currently 10 different choices)
 * str1    : 10 bits
 * stackp  :  5 bits (actually remaining depth, depth - stackp)
 *
 * FIXME: Rename stackp to something like remaining_depth at some
 *        appropriate time.
 *
 * The data2 field packs into 32 bits the following
 * fields:
 *
 * status :   2 bits (0 free, 1 open, 2 closed)
 * result1:   4 bits
 * result2:   4 bits
 * move   :  10 bits
 * str2   :  10 bits
 */

typedef struct read_result_t {
  unsigned int data1;	
  unsigned int data2;

  struct read_result_t *next;
} Read_result;

/* Bit mask for the input bits in the data2 field. */
#define RR_INPUT_DATA2 0x3ff

/* Get parts of a Read_result identifying the input data. */
#define rr_get_komaster(rr)   (((rr).data1  >> 29) & 0x07)
#define rr_get_kom_pos(rr)    (((rr).data1  >> 19) & 0x3ff)
#define rr_get_routine(rr)    (((rr).data1  >> 15) & 0x0f)
#define rr_get_str1(rr)       (((rr).data1  >>  5) & 0x3ff)
#define rr_get_stackp(rr)     (((rr).data1  >>  0) & 0x1f)
#define rr_get_str2(rr)       (((rr).data2  >>  0) & 0x3ff)
#define rr_get_str(rr)        rr_get_str1(rr)

/* Set corresponding parts. */
#define rr_input_data1(routine, komaster, kom_pos, str1, stackp) \
	((( (unsigned) (((((((komaster) << 10) | (kom_pos)) << 4) \
	  | (routine)) << 10) | (str1))) << 5) | ((unsigned) stackp));
#define rr_input_data2(str2) (str2) \

/* Set input data fields and at the same time set status to open. */
#define rr_set_input_data(rr, routine, komaster, kom_pos, str, stackp) \
       do { \
         (rr).data1 = rr_input_data1(routine, komaster, kom_pos, str, stackp);\
         (rr).data2 = (((rr).data2 & ~0x300003ff) | (1 << 28));\
       } while (0)

/* Variation for two distinct strings. */
#define rr_set_input_data2(rr, routine, komaster, kom_pos, str1, str2, stackp)\
       do { \
         (rr).data1 = rr_input_data1(routine, komaster, kom_pos, \
                                     str1, stackp); \
         (rr).data2 = (((rr).data2 & ~0x3ff) | (1 << 28) \
                       | rr_input_data2(str2)); \
       } while (0)

/* Get parts of a Read_result constituting the result of a search. */
#define rr_get_status(rr)      (((rr).data2 >> 28) & 0x03)
#define rr_get_result1(rr)     (((rr).data2 >> 24) & 0x0f)
#define rr_get_result2(rr)     (((rr).data2 >> 20) & 0x0f)
#define rr_get_move(rr)        (((rr).data2 >> 10) & 0x3ff)
#define rr_get_result(rr)      rr_get_result1(rr)

/* Set corresponding parts. */
#define rr_set_result_move(rr, result, move) \
	(rr).data2 = (((rr).data2 & 0x3ff) \
          | (2 << 28) | (((result) & 0x0f) << 24) | (((move) & 0x3ff) << 10))

/* Variation with two results. */
#define rr_set_result_move2(rr, result1, result2, move) \
	(rr).data2 = (((rr).data2 & 0x3ff) | (2 << 28) \
                      | (((result1) & 0x0f) << 24) \
                      | (((result2) & 0x0f) << 20) \
                      | (((move) & 0x3ff) << 10))

/*
 * The hash table consists of hash nodes.  Each hash node consists of
 * The hash value for the position it holds, the position itself and
 * the actual information which is purpose of the table from the start.
 *
 * There is also a pointer to another hash node which is used when
 * the nodes are sorted into hash buckets (see below).
 */

typedef struct hashnode_t {
  Hash_data            key;
  Read_result         *results;	/* The results of previous readings */

  struct hashnode_t   *next;
} Hashnode;


/*
 * The hash table consists of three parts:
 * - The hash table proper: a number of hash buckets with collisions
 *   being handled by a linked list.
 * - The hash nodes.  These are allocated at creation time and are 
 *   never removed or reallocated in the current implementation.
 * - The search results.  Since many different searches can
 *   be done in the same position, there should be more of these than
 *   hash nodes.
 */

typedef struct hashtable {
  int            hashtablesize;	/* Number of hash buckets */
  Hashnode     **hashtable;	/* Pointer to array of hashnode lists */

  int            num_nodes;	/* Total number of hash nodes */
  Hashnode      *all_nodes;	/* Pointer to all allocated hash nodes. */
  int            free_node;	/* Index to next free node. */

  int            num_results;	/* Total number of results */
  Read_result   *all_results;	/* Pointer to all allocated results. */
  int            free_result;	/* Index to next free result. */
} Hashtable;


void read_result_dump(Read_result *result, FILE *outfile);
void hashtable_dump(Hashtable *table, FILE *outfile);
void hashnode_dump(Hashnode *node, FILE *outfile);

/* ================================================================ */

/* Macros used from reading.c and owl.c to store and retrieve read
 * results.
 */


#define TRACE_CACHED_RESULT(rr)
#define TRACE_CACHED_RESULT2(rr)

#define SETUP_TRACE_INFO(name, str) \
  const char *read_function_name = name; \
  int q = str;

#define SETUP_TRACE_INFO2(name, str1, str2) \
  const char *read_function_name = name; \
  int q1 = str1; \
  int q2 = str2;

/* Trace messages in decidestring/decidedragon sgf file. */
void sgf_trace(const char *func, int str, int move, int result,
	       const char *message);
/* Trace messages in decideconnection/decidesemeai sgf file. */
void sgf_trace2(const char *func, int str1, int str2, int move, int result,
		const char *message);

/* Macro to hide the call to sgf_trace(). Notice that a little black
 * magic is going on here. Before using this macro, SETUP_TRACE_INFO
 * must have been called to provide the variables read_function_name
 * and q. These must of course not be used for anything else in
 * the function.
 */
#define SGFTRACE(move, result, message) \
  if (sgf_dumptree) \
    sgf_trace(read_function_name, q, move, result, message)

/* Corresponding macro for use in connection or semeai reading, where
 * two groups are involved.
 */
#define SGFTRACE2(move, result, message) \
  if (sgf_dumptree) \
    sgf_trace2(read_function_name, q1, q2, move, result, message)


int get_read_result(int routine, int komaster, int kom_pos,
		    int *str, Read_result **read_result);
int get_read_result2(int routine, int komaster, int kom_pos,
		     int *str1, int *str2, Read_result **read_result);

/* ================================================================ */

/*
 * These macros should be used in all the places where we want to
 * return a result from a reading function and where we want to
 * store the result in the hash table at the same time.
 */

#define READ_RETURN0(read_result) \
  do { \
    if (read_result) { \
      rr_set_result_move(*(read_result), 0, 0); \
    } \
    return 0; \
  } while (0)

#define READ_RETURN(read_result, point, move, value) \
  do { \
    if ((value) != 0 && (point) != 0) *(point) = (move); \
    if (read_result) { \
      rr_set_result_move(*(read_result), (value), (move)); \
    } \
    return (value); \
  } while (0)

#define READ_RETURN_SEMEAI(read_result, point, move, value_a, value_b) \
  do { \
    if ((value_a) != 0 && (point) != 0) *(point) = (move); \
    if (read_result) { \
      rr_set_result_move2(*(read_result), (value_a), (value_b), (move)); \
    } \
    return; \
  } while (0)

#define READ_RETURN_CONN(read_result, point, move, value) \
  do { \
    if ((value) != 0 && (point) != 0) *(point) = (move); \
    if (read_result) { \
      rr_set_result_move(*(read_result), (value), (move)); \
    } \
    return (value); \
  } while (0)

#define READ_RETURN2(read_result, point, move, value_a, value_b) \
  do { \
    if ((value_a) != 0 && (point) != 0) *(point) = (move); \
    if (read_result) { \
      rr_set_result_move2(*(read_result), (value_a), (value_b), (move)); \
    } \
    return (value_a); \
  } while (0)


/* ================================================================ */
/* Routine numbers. */

#define OWL_ATTACK      0
#define OWL_DEFEND      1
#define SEMEAI          2

#define FIND_DEFENSE    3
#define ATTACK          4

#define CONNECT         5
#define DISCONNECT      6

#define MAX_ROUTINE     DISCONNECT
#define NUM_ROUTINES    (MAX_ROUTINE + 1)
  

/* Routine numbers for the persistent owl cache, in addition to
 * OWL_ATTACK and OWL_DEFEND defined above.
 */
#define OWL_THREATEN_ATTACK    2
#define OWL_THREATEN_DEFENSE   3
#define OWL_DOES_DEFEND        4
#define OWL_DOES_ATTACK        5
#define OWL_CONNECTION_DEFENDS 6
#define OWL_SUBSTANTIAL        7
#define OWL_CONFIRM_SAFETY     8

 

/* ================================================================ */
/* This has actually nothing to do with caching, but is useful in
 * the same places where the caching is.
 */
  
/* Macro to use when saving ko results while continuing to look for an
 * unconditional result. It's assumed that we have tried the move at
 * (move) and then called an attack or defense function giving the
 * result passed in the code parameter.
 *
 * In general we prefer not to have to do the first ko threat. Thus a
 * savecode KO_A is always better than a savecode KO_B. Also we always
 * prefer to keep the old move if we get the same savecode once more,
 * on the assumption that the moves have been ordered with the
 * presumably best one first.
 *
 * Notice that the savecode may be either 0 (nothing found so far), KO_B
 * or KO_A. Occasionally savecode WIN is also used, indicating an effective
 * but not preferred move, typically because it's either a sacrifice
 * or a backfilling move. If possible, we prefer making non-sacrifice
 * and direct moves. Of course savecode WIN is better than KO_A or KO_B.
 */


#define UPDATE_SAVED_KO_RESULT(savecode, save, code, move) \
  if (code != 0 && REVERSE_RESULT(code) > savecode) { \
    save = move; \
    savecode = REVERSE_RESULT(code); \
  } \

/* Same as above, except this should be used when there's no
 * intervening trymove(). Thus we shouldn't reverse the save code.
 */
#define UPDATE_SAVED_KO_RESULT_UNREVERSED(savecode, save, code, move) \
  if (code != WIN && code > savecode) { \
    save = move; \
    savecode = code; \
  }


/* This too isn't really related to caching but is convenient to have here.
 * (Needs to be available in reading.c and persistent.c.)
 *
 * Minimum number of nodes for which DEBUG_READING_PERFORMANCE reports
 * anything.
 */
#define MIN_READING_NODES_TO_REPORT 1000


#endif /* _CACHE_H_ */

  
/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
