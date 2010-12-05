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

#ifndef _CACHE_H_
#define _CACHE_H_

#include <stdio.h>
#include "hash.h"

/*
 * This file, together with engine/hash.c implements hashing of go positions
 * using a method known as Zobrist hashing.  See the Texinfo documentation
 * (Reading/Hashing) for more information.  
 */


/* Hashnode: a node stored in the transposition table.
 *
 * In addition to the position, the hash lock encodes the following data,
 * all hashed:
 *   komaster
 *   kom_pos
 *   routine
 *   str1
 *   str2
 *   extra hashvalue, optional (e.g. encoding a goal array)
 *
 * The data field packs into 32 bits the following
 * fields:
 *
 *   RESERVED       :  5 bits
 *   value1         :  4 bits
 *   value2         :  4 bits
 *   move           : 10 bits
 *   cost           :  4 bits
 *   remaining_depth:  5 bits (depth - stackp)  NOTE: HN_MAX_REMAINING_DEPTH
 *
 *   The last 9 bits together give an index for the total costs.
 */
typedef struct {
  Hash_data key;
  unsigned int data; /* Should be 32 bits, but only wastes 25% if 64 bits. */
} Hashnode;

#define HN_MAX_REMAINING_DEPTH 31


/* Hashentry: an entry, with two nodes of the hash_table
 */
typedef struct {
  Hashnode deepest;
  Hashnode newest;
} Hashentry;

/* Hn is for hash node. */
#define hn_get_value1(hn)           ((hn >> 23) & 0x0f)
#define hn_get_value2(hn)           ((hn >> 19) & 0x0f)
#define hn_get_move(hn)             ((hn >>  9) & 0x3ff)
#define hn_get_cost(hn)             ((hn >>  5) & 0x0f)
#define hn_get_remaining_depth(hn)  ((hn >>  0) & 0x1f)
#define hn_get_total_cost(hn)       ((hn >>  0) & 0x1ff)

#define hn_create_data(remaining_depth, value1, value2, move, cost) \
    ((((value1)         & 0x0f)  << 23) \
   | (((value2)         & 0x0f)  << 19) \
   | (((move)           & 0x3ff) <<  9) \
   | (((cost)           & 0x0f)  <<  5) \
   | (((remaining_depth & 0x1f)  <<  0)))


/* Transposition_table: transposition table used for caching. */
typedef struct {
  unsigned int num_entries;
  Hashentry *entries;
  int is_clean;
} Transposition_table;

extern Transposition_table ttable;

/* Number of cache entries to use by default if no cache memory usage
 * has been set explicitly.
 */
#define DEFAULT_NUMBER_OF_CACHE_ENTRIES 350000

void tt_free(Transposition_table *table);
int  tt_get(Transposition_table *table, enum routine_id routine,
	    int target1, int target2, int remaining_depth,
	    Hash_data *extra_hash,
	    int *value1, int *value2, int *move);
void tt_update(Transposition_table *table, enum routine_id routine,
	       int target, int target2, int remaining_depth,
	       Hash_data *extra_hash,
	       int value1, int value2, int move);


/* ================================================================ */


/* Macros used from reading.c, readconnect.c, and owl.c to store and
 * retrieve read results.
 */

#if TRACE_READ_RESULTS

#define TRACE_CACHED_RESULT(result, move) \
      gprintf("%o%s %1m %d %d %1m (cached) ", read_function_name, \
	      q, stackp, result, move); \
      dump_stack();

#define TRACE_CACHED_RESULT2(result1, result2, move) \
      gprintf("%o%s %1m %1m %d %d %d %1m (cached) ", read_function_name, \
	      q1, q2, stackp, result1, result2, move); \
      dump_stack();


#define SETUP_TRACE_INFO(name, str) \
  const char *read_function_name = name; \
  int q = find_origin(str);

#define SETUP_TRACE_INFO2(name, str1, str2) \
  const char *read_function_name = name; \
  int q1 = board[str1] == EMPTY ? str1 : find_origin(str1); \
  int q2 = board[str2] == EMPTY ? str2 : find_origin(str2);

#else

#define TRACE_CACHED_RESULT(result, move)
#define TRACE_CACHED_RESULT2(result1, result2, move)

#define SETUP_TRACE_INFO(name, str) \
  const char *read_function_name = name; \
  int q = str;

#define SETUP_TRACE_INFO2(name, str1, str2) \
  const char *read_function_name = name; \
  int q1 = str1; \
  int q2 = str2;

#endif

/* Trace messages in decidestring/decidedragon sgf file. */
void sgf_trace(const char *func, int str, int move, int result,
	       const char *message);
/* Trace messages in decideconnection sgf file. */
void sgf_trace2(const char *func, int str1, int str2, int move, 
	        const char *result, const char *message);
/* Trace messages in decidesemeai sgf file. */
void sgf_trace_semeai(const char *func, int str1, int str2, int move, 
		      int result1, int result2, const char *message);

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
    sgf_trace2(read_function_name, q1, q2, move, \
	       result_to_string(result), message)

#define SGFTRACE_SEMEAI(move, result1, result2, message) \
  if (sgf_dumptree) \
    sgf_trace_semeai(read_function_name, q1, q2, move, \
	             result1, result2, message)


/* ================================================================ */

/*
 * These macros should be used in all the places where we want to
 * return a result from a reading function and where we want to
 * store the result in the hash table at the same time.
 */

#if !TRACE_READ_RESULTS

#define READ_RETURN0(routine, str, remaining_depth) \
  do { \
    tt_update(&ttable, routine, str, NO_MOVE, remaining_depth, NULL,\
	      0, 0, NO_MOVE);\
    return 0; \
  } while (0)

#define READ_RETURN(routine, str, remaining_depth, point, move, value) \
  do { \
    tt_update(&ttable, routine, str, NO_MOVE, remaining_depth, NULL,\
              value, 0, move);\
    if ((value) != 0 && (point) != 0) *(point) = (move); \
    return (value); \
  } while (0)

#define READ_RETURN_SEMEAI(routine, str1, str2, remaining_depth, point, move, value1, value2) \
  do { \
    tt_update(&ttable, routine, str1, str2, remaining_depth, NULL, \
              value1, value2, move); \
    if ((value1) != 0 && (point) != 0) *(point) = (move); \
    return; \
  } while (0)

#define READ_RETURN_CONN(routine, str1, str2, remaining_depth, point, move, value) \
  do { \
    tt_update(&ttable, routine, str1, str2, remaining_depth, NULL,\
              value, 0, move);\
    if ((value) != 0 && (point) != 0) *(point) = (move); \
    return (value); \
  } while (0)

#define READ_RETURN_HASH(routine, str, remaining_depth, hash, point, move, value) \
  do { \
    tt_update(&ttable, routine, str, NO_MOVE, remaining_depth, hash,\
              value, 0, move);\
    if ((value) != 0 && (point) != 0) *(point) = (move); \
    return (value); \
  } while (0)

#define READ_RETURN2(routine, str, remaining_depth, point, move, value1, value2) \
  do { \
    tt_update(&ttable, routine, str, NO_MOVE, remaining_depth, NULL,\
              value1, value2, move);\
    if ((value1) != 0 && (point) != 0) *(point) = (move); \
    return (value1); \
  } while (0)

#else /* !TRACE_READ_RESULTS */

#define READ_RETURN0(routine, str, remaining_depth) \
  do { \
    tt_update(&ttable, routine, str, NO_MOVE, remaining_depth, NULL,\
	      0, 0, NO_MOVE);\
    gprintf("%o%s %1m %d 0 0 ", read_function_name, q, stackp); \
    dump_stack(); \
    return 0; \
  } while (0)

#define READ_RETURN(routine, str, remaining_depth, point, move, value) \
  do { \
    tt_update(&ttable, routine, str, NO_MOVE, remaining_depth, NULL,\
              value, 0, move);\
    if ((value) != 0 && (point) != 0) *(point) = (move); \
    gprintf("%o%s %1m %d %d %1m ", read_function_name, q, stackp, \
	    (value), (move)); \
    dump_stack(); \
    return (value); \
  } while (0)

#define READ_RETURN_SEMEAI(routine, str1, str2, remaining_depth, point, move, value1, value2) \
  do { \
    tt_update(&ttable, routine, str1, str2, remaining_depth, NULL, \
              value1, value2, move); \
    if ((value1) != 0 && (point) != 0) *(point) = (move); \
    gprintf("%o%s %1m %1m %d %d %d %1m ", read_function_name, q1, q2, stackp, \
	    (value1), (value2), (move)); \
    dump_stack(); \
    return; \
  } while (0)

#define READ_RETURN_CONN(routine, str1, str2, remaining_depth, point, move, value) \
  do { \
    tt_update(&ttable, routine, str1, str2, remaining_depth, NULL,\
              value, 0, move);\
    if ((value) != 0 && (point) != 0) *(point) = (move); \
    gprintf("%o%s %1m %1m %d %d %1m ", read_function_name, q1, q2, stackp, \
	    (value), (move)); \
    dump_stack(); \
    return (value); \
  } while (0)

#define READ_RETURN_HASH(routine, str, remaining_depth, hash, point, move, value) \
  do { \
    tt_update(&ttable, routine, str, NO_MOVE, remaining_depth, hash,\
              value, 0, move);\
    if ((value) != 0 && (point) != 0) *(point) = (move); \
    gprintf("%o%s %1m %d %d %1m ", read_function_name, q, stackp, \
	    (value), (move)); \
    dump_stack(); \
    return (value); \
  } while (0)

#define READ_RETURN2(routine, str, remaining_depth, point, move, value1, value2) \
  do { \
    tt_update(&ttable, routine, str, NO_MOVE, remaining_depth, NULL,\
              value1, value2, move);\
    if ((value1) != 0 && (point) != 0) *(point) = (move); \
    gprintf("%o%s %1m %d %d %1m ", read_function_name, q, stackp, \
	    (value1), (move)); \
    dump_stack(); \
    return (value1); \
  } while (0)

#endif


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
