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

#ifndef _DFA_H_
#define _DFA_H_

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>

/********************************
 *         parameters           *
 ********************************/

/* #define DFA_TRACE define this to trace the program */
/* #define DFA_PARANOIAC define this to activate a lot of assert() */
#define DFA_MAX_BOARD 21
#define MAX_ORDER DFA_MAX_BOARD*DFA_MAX_BOARD*4
/* maximum pattern matched at one positions */
#define DFA_MAX_MATCHED 8*500
#define DFA_RESIZE_STEP 20000
#define DFA_INIT_SIZE 250
#ifndef EMPTY
#define EMPTY     0		/* . */
#define WHITE     1		/* O */
#define BLACK     2		/* X */
#endif
#define OUT_BOARD 3		/* # */

/********************************
 *    data types definition     *
 ********************************/

/* intersections */

typedef unsigned short Intersection_t;

/* attribute list */

typedef struct attrib
{
  int val;
  int next;
} attrib_t;


/* dfa state */

typedef struct state
{
  int att;
  int next[4];
} state_t;


/* dfa */

typedef struct dfa
{
  /* file header */
  char name[80];
  int pre_rotated;
  
  /* transition graph */
  state_t *states;
  int max_states;
  int last_state;

  /* attributes sets */
  attrib_t *indexes;
  int max_indexes;
  int last_index;
} dfa_t;


/* The run-time data structures are different from those used
 * internally to build the DFA */

/* attribute list */
typedef struct attrib_rt
{
  short val;
  short next;
} attrib_rt_t;

/* dfa state */
typedef struct state_rt
{
  short att;
  unsigned short next[4];
} state_rt_t;

typedef struct dfa_rt
{
  /* file header */
  const char name[80];
  const int pre_rotated;
  
  /* transition graph */
  const state_rt_t *states;

  /* attributes sets */
  const attrib_rt_t *indexes;
} dfa_rt_t;


/* scan order */

#if 0
typedef struct
{
  int i;
  int j;
} order_t;
#endif


/********************************
 *    functions declaration     *
 ********************************/

void dfa_init(void);	/* Every call to a dfa function must be done */
void dfa_end(void);	/* between calls of those 2 functions. */
void buildSpiralOrder(int order[MAX_ORDER][8]); /* Needed by matchpat */

/* basic dfa manipulation */
void print_c_dfa(FILE *of, const char *name, dfa_t *pdfa);
void new_dfa(dfa_t *pdfa, const char *name);
void copy_dfa(dfa_t *p_to, dfa_t *p_from);
void kill_dfa(dfa_t *pdfa);
int dfa_size(dfa_t *pdfa);	/* in kB */
void save_dfa(const char *f_name, dfa_t *pdfa);
dfa_t *load_dfa(const char *f_path, const char *f_name, dfa_t **ppdfa);
void dfa_finalize(dfa_t *pdfa);
void dfa_shuffle(dfa_t *pdfa);
int dfa_minmax_delta(dfa_t *pdfa, int next_index, int isMin);
void dump_dfa(FILE *f, dfa_t *pdfa);

struct pattern;

/* conversion between a gnugo pattern struct into a dfa string. */
void pattern_2_string(struct pattern *pat, struct patval_b *elements,
		      char *str, int ci, int cj);
void dfa_rotate_string(char *strrot, const char *str, int ll);

/* add a string with attribute att_val into a dfa */
float dfa_add_string(dfa_t *pdfa, const char *str, int pattern_index, int ll);


/* conversion macros */

#define EXPECTED_COLOR(player_c, position_c) convert[player_c][position_c]

/* incremental macro */

#define BASE DFA_MAX_BOARD * 2
#define DFA_POS(i, j)  (4 * DFA_MAX_BOARD * (i) + (j))
#define DFA_OFFSET DFA_POS(DFA_MAX_BOARD, DFA_MAX_BOARD)


/********************************
 *    global variables          *
 ********************************/

extern int dfa_verbose;		/* the verbose level */


/**************************************
 *	Experimental dfa builder      *
 **************************************/

#define DFA_ATTRIB_BLOCK_SIZE	150000
#define DFA_NODE_BLOCK_SIZE	 50000

typedef struct _dfa_attrib	 dfa_attrib;
typedef struct _dfa_attrib_block dfa_attrib_block;
typedef struct _dfa_attrib_array dfa_attrib_array;
typedef struct _dfa_node	 dfa_node;
typedef struct _dfa_node_block	 dfa_node_block;
typedef struct _dfa_graph	 dfa_graph;

struct _dfa_attrib {
  dfa_attrib	   *next;
  int		    string_index;
};

struct _dfa_attrib_block {
  dfa_attrib_block *previous;
  dfa_attrib	    attrib[DFA_ATTRIB_BLOCK_SIZE];
};

struct _dfa_attrib_array {
  dfa_attrib_block *last_block;
  int		    allocated;
};

struct _dfa_node {
  dfa_node	   *branch[4];
  dfa_attrib	   *attributes;
  dfa_attrib	   *passing_strings;
};

struct _dfa_node_block {
  dfa_node_block   *previous;
  dfa_node	    node[DFA_NODE_BLOCK_SIZE];
};

struct _dfa_graph {
  int		    num_nodes;
  dfa_node	   *root;
  dfa_node_block   *last_block;
  int		    allocated;
  dfa_attrib_array  attributes;
};


#define DFA_HASH_BLOCK_SIZE	 10000

#define DFA_HASH_TABLE_SIZE	  4096
#define DFA_HASH_VALUE_1	     1
#define DFA_HASH_VALUE_2	    79
#define DFA_HASH_VALUE_3	  2971

typedef struct _dfa_hash_entry	 dfa_hash_entry;
typedef struct _dfa_hash_block	 dfa_hash_block;

struct _dfa_hash_entry {
  dfa_hash_entry   *next;
  dfa_attrib	   *key;
  dfa_node	   *value;
};

struct _dfa_hash_block {
  dfa_hash_block   *previous;
  dfa_hash_entry    entry[DFA_HASH_BLOCK_SIZE];
};


typedef struct _dfa_pattern	 dfa_pattern;
typedef struct _dfa_patterns	 dfa_patterns;

struct _dfa_pattern {
  dfa_pattern	   *next;
  int		    num_variations;
  int		    current_variation;
  char		   *variation[8];
};

struct _dfa_patterns {
  int		    num_patterns;
  dfa_pattern	   *patterns;
  dfa_pattern	   *last_pattern;
  dfa_graph	    graph;
};


void		dfa_graph_reset(dfa_graph *graph);

void		dfa_patterns_reset(dfa_patterns *patterns);
void		dfa_patterns_clear(dfa_patterns *patterns);
void		dfa_patterns_add_pattern(dfa_patterns *patterns,
					 const char *string, int index);
void		dfa_patterns_set_last_pattern_variation(dfa_patterns *patterns,
							int variation);
void		dfa_patterns_select_shortest_variation(dfa_patterns *patterns);
void		dfa_patterns_build_graph(dfa_patterns *patterns);
int *		dfa_patterns_optimize_variations(dfa_patterns *patterns,
						 int iterations);


#endif /* _DFA_H_ */


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
