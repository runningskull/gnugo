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



#include <stdio.h>

/*
 * This file, together with engine/hash.c implements hashing of go positions
 * using a method known as Zobrist hashing.  See the Texinfo documentation
 * (Reading/Hashing) for more information.  
 */


#ifndef _CACHE_H_
#define _CACHE_H_


/* Dump (almost) all read results. */
#define TRACE_READ_RESULTS 0


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
 * The compressed_data field packs into 32 bits the following
 * fields:
 *
 * komaster: 2 bits (EMPTY, BLACK, WHITE, or GRAY)
 * kom_i   : 5 bits
 * kom_j   : 5 bits
 * routine : 4 bits (currently 10 different choices)
 * i       : 5 bits
 * j       : 5 bits
 * stackp  : 5 bits
 */

typedef struct read_result_t {
  unsigned int compressed_data;	

  int result_ri_rj;		/* ...then this was the result. */
  /*
  unsigned char  status;        // 0 free, 1 open, 2 closed
  unsigned char  result;
  unsigned char  ri;
  unsigned char  rj;
  */

  struct read_result_t *next;
} Read_result;


/* Get parts of a Read_result identifying the routine and position. */
#define rr_get_komaster(rr)   (((rr).compressed_data >> 29) & 0x03)
#define rr_get_kom_i(rr)      ((((rr).compressed_data >> 24) & 0x1f) - 1)
#define rr_get_kom_j(rr)      ((((rr).compressed_data >> 19) & 0x1f) - 1)
#define rr_get_routine(rr)    (((rr).compressed_data >> 15) & 0x0f)
#define rr_get_pos_i(rr)      (((rr).compressed_data >> 10) & 0x1f)
#define rr_get_pos_j(rr)      (((rr).compressed_data >>  5) & 0x1f)
#define rr_get_stackp(rr)     (((rr).compressed_data >>  0) & 0x1f)

/* Set corresponding parts. */
#define rr_compress_data(rr, routine, komaster, kom_i, kom_j, i, j, stackp) \
	(((((((((((((komaster) << 5) | ((kom_i) + 1)) << 5) \
	  | ((kom_j) + 1)) << 4) | (routine)) << 5) | (i)) << 5) \
	    | (j)) << 5) | stackp);

#define rr_set_compressed_data(rr, routine, komaster, kom_i, kom_j, i, j, stackp) \
       (rr).compressed_data \
	= rr_compress_data(rr, routine, komaster, kom_i, kom_j, i, j, stackp)

/* Get parts of a Read_result constituting the result of a search. */
#define rr_get_status(rr)   (((rr).result_ri_rj >> 24) & 0xff)
#define rr_get_result(rr)   (((rr).result_ri_rj >> 16) & 0xff)
#define rr_get_result_i(rr) (((rr).result_ri_rj >>  8) & 0xff)
#define rr_get_result_j(rr) (((rr).result_ri_rj >>  0) & 0xff)

/* Set corresponding parts. */
#define rr_set_result_ri_rj(rr, result, ri, rj) \
	(rr).result_ri_rj \
	    = (2 << 24 \
              | (((((result) << 8) | ((ri) & 0xff)) << 8) | ((rj) & 0xff)))

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
  Read_result         *results;	/* And here are the results of previous */
				/*    readings */

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

#if TRACE_READ_RESULTS

#define TRACE_CACHED_RESULT(rr) \
      gprintf("%o%s %m %d %d %d %d (cached) ", read_function_name, \
	      qi, qj, stackp, \
	      rr_get_result(rr), \
	      rr_get_result_i(rr), \
	      rr_get_result_j(rr)); \
      dump_stack();

#define SETUP_TRACE_INFO(name, si, sj) \
  const char *read_function_name = name; \
  int qi, qj; \
  find_origin(si, sj, &qi, &qj);

#else

#define TRACE_CACHED_RESULT(rr)

#define SETUP_TRACE_INFO(name, si, sj) \
  const char *read_function_name = name; \
  int qi = si; \
  int qj = sj;

#endif

/* The caching of read results currently mangle negative move
 * coordinates, effectively returning the value as a 2-complement
 * 8-bit pattern in an int. To correct this we need to translate the
 * interval [128,255] to [-128,-1].
 */
#define FIXUP_CACHED_VALUE(x) ((x >= 128) ? (((int) x) - 256) : (x))

/* Trace messages in decidestring/decidedragon sgf file. */
void sgf_trace(const char *func, int si, int sj, int i, int j,
	       int result, const char *message);

/* Macro to hide the call to sgf_trace(). Notice that a little black
 * magic is going on here. Before using this macro, SETUP_TRACE_INFO
 * must have been called to provide the variables read_function_name,
 * qi, and qj. These must of course not be used for anything else in
 * the function.
 */
#define SGFTRACE(i, j, result, message) \
  if (sgf_dumptree) \
    sgf_trace(read_function_name, qi, qj, i, j, result, message)

extern Hashtable *movehash;

int get_read_result(int routine, int komaster, int kom_i, int kom_j,
		    int *si, int *sj,
		    Read_result **read_result);

/* ================================================================ */

/*
 * These macros should be used in all the places where we want to
 * return a result from a reading function and where we want to
 * store the result in the hash table at the same time.
 */
#if !TRACE_READ_RESULTS

#define READ_RETURN0(read_result) \
  do { \
    if (read_result) { \
      rr_set_result_ri_rj(*(read_result), 0, 0, 0); \
    } \
    return 0; \
  } while (0)

#define READ_RETURN(read_result, pointi, pointj, resulti, resultj, value) \
  do { \
    if ((value) != 0 && (pointi) != 0) *(pointi)=(resulti); \
    if ((value) != 0 && (pointj) != 0) *(pointj)=(resultj); \
    if (read_result) { \
      rr_set_result_ri_rj(*(read_result), (value), (resulti), (resultj)); \
    } \
    return (value); \
  } while (0)

#else

#define READ_RETURN0(read_result) \
  do { \
    if (read_result) { \
      rr_set_result_ri_rj(*(read_result), 0, 0, 0); \
    } \
    gprintf("%o%s %m %d 0 0 0 ", read_function_name, qi, qj, stackp); \
    dump_stack(); \
    return 0; \
  } while (0)

#define READ_RETURN(read_result, pointi, pointj, resulti, resultj, value) \
  do { \
    if ((value) != 0 && (pointi) != 0) *(pointi)=(resulti); \
    if ((value) != 0 && (pointj) != 0) *(pointj)=(resultj); \
    if (read_result) { \
      rr_set_result_ri_rj(*(read_result), (value), (resulti), (resultj)); \
    } \
    gprintf("%o%s %m %d %d %d %d ", read_function_name, qi, qj, stackp, \
	    (value), (resulti), (resultj)); \
    dump_stack(); \
    return (value); \
  } while (0)

#endif
  
/* ================================================================ */
/* Routine numbers. */

#define FIND_DEFENSE    0
#define DEFEND1         1
#define DEFEND2         2
#define DEFEND3         3
#define DEFEND4         4

#define ATTACK          5
#define ATTACK2         6
#define ATTACK3         7

#define OWL_ATTACK      8
#define OWL_DEFEND      9

#define MAX_ROUTINE     OWL_DEFEND
#define NUM_ROUTINES    (MAX_ROUTINE+1)
  
#endif


/* ================================================================ */
/* This has actually nothing to do with caching, but is useful in
 * the same places where the caching is.
 */
  
/* Macro to use when saving ko results while continuing to look for an
 * unconditional result. It's assumed that we have tried the move at
 * (i, j) and then called an attack or defense function giving the
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


#define UPDATE_SAVED_KO_RESULT(savecode, savei, savej, code, i, j) \
  if (code == KO_B && (savecode == 0 || savecode == KO_B)) { \
    savei = i; \
    savej = j; \
    savecode = KO_A; \
  } \
  else if (code == KO_A && savecode == 0) { \
    savei = i; \
    savej = j; \
    savecode = KO_B; \
  }

/* Same as above, except this should be used when there's no
 * intervening trymove(). Thus we shouldn't reverse the save code.
 */
#define UPDATE_SAVED_KO_RESULT_UNREVERSED(savecode, savei, savej, code, i, j) \
  if ((code == KO_B && savecode == 0) \
      || (code == KO_A && (savecode == 0 || savecode == KO_B))) { \
    savei = i; \
    savej = j; \
    savecode = code; \
  }

  
/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
