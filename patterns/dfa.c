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

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * * * * * * * * fast pattern matching with DFA  version 2.9 * * *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include "liberty.h"
#include "patterns.h"
#include "dfa.h"
#include <assert.h>
#include <stdlib.h>

#include <config.h>


/*********************
 *   Public data     *
 *********************/

/* If > 0 more detailed information is given */
int dfa_verbose = 0;


/*********************
 *  Private data     *
 *********************/

/* the private board */
int dfa_p[DFA_MAX_BOARD * 4 * DFA_MAX_BOARD * 4];

/* auxiliary dfa's for high level functions */
#define DFA_BINS 33 /* Number of temporary bins used to store intermediate DFAs */
static dfa_t aux_dfa[DFA_BINS];	/* used to store intermediate DFAs */
static dfa_t aux_temp;          /* used to store temporary DFAs */

/* To be sure that everything was well initialized */
static int dfa_was_initialized = 0;
static int aux_count = 0;


/* convert is a table to convert the colors */
const int convert[3][4] = {
  {-1, -1, -1, -1},		/* not used */
  {EMPTY, WHITE, BLACK, OUT_BOARD},	/* WHITE */
  {EMPTY, BLACK, WHITE, OUT_BOARD}	/* BLACK */
};


/* convert ATT_* values to the corresponding expected values on the board */
static const char att2val[8] = {
  '.', 'X', 'O', 'x', 'o', ',', 'a', '!'
};

#define EXPECTED_VAL(att_val) att2val[att_val]


/************************************************
 *   forward declaration of private functions   *
 ************************************************/
static void clean_dfa(dfa_t *pdfa);
static void resize_dfa(dfa_t *pdfa, int max_states, int max_indexes);
static void create_dfa(dfa_t *pdfa, const char *str, int att_val);
static void do_sync_product(int l, int r);
static void sync_product(dfa_t *pout, dfa_t *pleft, dfa_t *pright);


/********************************
 *   manipulating scan orders   *
 ********************************/

/* The spiral order is the way we scan the board,
 * we begin on the anchor and we 
 * progressively scan all its neigbouring intersections,
 * collecting all the known patterns we meet on our way:
 *
 *                  4      4      4
 * 1    1     13    13    513    513  ... and so on until we reach
 *      2     2     2      2     827      a stopping state in the
 *                                6        dfa.
 */

int spiral[MAX_ORDER][8];

/*
 * Build the spiral order for each
 * transformation: instead of changing the board
 * or changing the patterns, we only change the order,
 * for eg. the same dfa can perform the pattern matching
 *
 * that way for identity:
 *
 *     765                                            567
 *     814F      and this way for mirror symetry:    F418
 *     923E                                          E329
 *     CABD                                          DBAC
 *
 * Anther possibility is to generate one string by pattern and by
 * transformation in mkpat to avoid any runtime transformation 
 * but it may increase the size of the dfa.
 * 
 */

static const int generator[4] = {
  4 * DFA_MAX_BOARD, 1, -4 * DFA_MAX_BOARD, -1
};

void
buildSpiralOrder(int order[MAX_ORDER][8])
{
  int mark[DFA_MAX_BOARD * 4 * DFA_MAX_BOARD * 4];
  int fifo[8 * MAX_ORDER];
  int top = 0, end = 0;
  int i, j, i0, j0;
  int k, ll;
  int ii;
  int delta;

  if (dfa_verbose > 1)
    fprintf(stderr, "Building spiral order\n");

  /* First we build the basic (pseudo)spiral order */

  /* initialization */

  for (ii = 0; ii < DFA_MAX_BOARD * 4 * DFA_MAX_BOARD * 4; ii++)
    mark[ii] = 1;

  for (i = DFA_MAX_BOARD; i < DFA_MAX_BOARD * 3; i++)
    for (j = DFA_MAX_BOARD; j < DFA_MAX_BOARD * 3; j++)
      mark[DFA_POS(i, j)] = 0;

  end = 0;
  top = 1;
  fifo[end] = 2 * DFA_OFFSET;
  mark[fifo[end]] = 1;

  /* generation */
  while (end < MAX_ORDER) {
    ii = fifo[end];
    order[end][0] = ii - 2 * DFA_OFFSET;
    end++;
    
    for (k = 0; k != 4; k++) {
      delta = generator[k];
      
      if (!mark[ii + delta]) {
	fifo[top] = ii + delta;
	mark[ii + delta] = 1;
	top++;
      }
    }
  }

  /* Then we compute all the geometric transformations on this order */
  for (k = 0; k < MAX_ORDER; k++) {
    j0 = order[k][0] % (4 * DFA_MAX_BOARD);
    if (j0 >= 2 * DFA_MAX_BOARD)
      j0 -= 4 * DFA_MAX_BOARD;
    if (j0 < - 2 * DFA_MAX_BOARD)
      j0 += 4 * DFA_MAX_BOARD;
    i0 = (order[k][0] - j0) / (4 * DFA_MAX_BOARD);
    for (ll = 1; ll != 8; ll++) {
      TRANSFORM2(i0, j0, &i, &j, ll);
      order[k][ll] = DFA_POS(i, j);
    }
  }

  if (0) {
    for (ll = 0; ll < 8; ll++)
      for (i = 0; i < 13; i++)
        fprintf(stderr, "i:%d; ll:%d; %d(%c)\n", i, ll, order[i][ll], 'A'+i);
  }
}

/********************************
 * manipulating attributes list *
 ********************************/

/*
 * Test if val is member of the attributes set att
 */

static int
member_att(dfa_t *pdfa, int att, int val)
{
  while (att != 0) {
    if (pdfa->indexes[att].val == val)
      return 1;
    
    att = pdfa->indexes[att].next;
  }
  
  return 0;
}

/* 
 * return the union of two attribute sets att1 & att2
 * repectively from dfa1 and dfa2 into
 * att in dfa.
 */

static int
union_att(dfa_t *pdfa, dfa_t *pdfa1, int att1, dfa_t *pdfa2, int att2)
{
  int att;
  int att_aux;

  /* copy att1 in att */
  att = 0;
  while (att1 != 0) {
    pdfa->last_index++;
    if (pdfa->last_index >= pdfa->max_indexes)
      resize_dfa(pdfa, pdfa->max_states, pdfa->max_indexes + DFA_RESIZE_STEP);
    att_aux = pdfa->last_index;
    
    pdfa->indexes[att_aux].val = pdfa1->indexes[att1].val;
    pdfa->indexes[att_aux].next = att;
    att = att_aux;
    att1 = pdfa1->indexes[att1].next;
  }

  /* add to att the new elements of att2 */
  while (att2 != 0) {
    if (!member_att(pdfa, att, pdfa2->indexes[att2].val)) {
      pdfa->last_index++;
      if (pdfa->last_index >= pdfa->max_indexes)
	resize_dfa(pdfa, pdfa->max_states, pdfa->max_indexes + DFA_RESIZE_STEP);
      att_aux = pdfa->last_index;

      pdfa->indexes[att_aux].val = pdfa2->indexes[att2].val;
      pdfa->indexes[att_aux].next = att;
      att = att_aux;
    }
    att2 = pdfa2->indexes[att2].next;
  }

  return att;
}


/* Remove all attribute entry repetitions from a dfa.
 */
static void
compactify_att(dfa_t *pdfa)
{
  int k;
  int last = 0;
  int save_last = pdfa->last_index;
  int *map;
  int *search_first;
  int *search_next;
  int size = (save_last + 1) * sizeof(int);

  map = malloc(size);
  map[0] = 0;
  search_first = malloc(size);
  memset(search_first, 0, size);
  search_next = malloc(size);
  memset(search_next, 0, size);

  for (k = 1; k <= save_last; k++) {
    int i = search_first[pdfa->indexes[k].val];

    if (i) {
      while (pdfa->indexes[i].next != pdfa->indexes[k].next) {
	if (!search_next[i]) {
	  search_next[i] = ++last;
	  i = 0;
	  break;
	}

	i = search_next[i];
      }
    }
    else
      search_first[pdfa->indexes[k].val] = ++last;

    if (i)
      map[k] = i;
    else {
      map[k] = last;
      pdfa->indexes[last] = pdfa->indexes[k];
    }
  }

  free(search_first);
  free(search_next);
  
  if (last < save_last) {
    pdfa->last_index = last;
    for (k = 0; k <= pdfa->last_index; k++)
      pdfa->indexes[k].next = map[pdfa->indexes[k].next];

    for (k = 0; k <= pdfa->last_state; k++)
      pdfa->states[k].att = map[pdfa->states[k].att];

    if (0)
      fprintf(stderr, "compactified: %d attributes left of %d\n",
	      last, save_last);

    compactify_att(pdfa);
  }
  
  free(map);
}


/**********************
 * manipulating dfa's *
 **********************/


/*
 * return the effective size of a dfa in kB.
 */

int
dfa_size(dfa_t *pdfa)
{
  int states_size, indexes_size;

  states_size = (pdfa->last_state + 1) * sizeof(state_rt_t);
  indexes_size = (pdfa->last_index + 1) * sizeof(attrib_rt_t);

  return (states_size + indexes_size + sizeof(dfa_rt_t)) / 1024;
}


/* 
 * resize memory for a dfa 
 */

static void
resize_dfa(dfa_t *pdfa, int max_states, int max_indexes)
{
  state_t *pBuf;
  attrib_t *pBuf2;
  int i;

  if (dfa_verbose > 1)
    fprintf(stderr, "Resizing dfa %s\n", pdfa->name);

  assert(pdfa->last_state <= pdfa->max_states);
  assert(pdfa->last_index <= pdfa->max_indexes);

  pBuf = realloc(pdfa->states, max_states * sizeof(state_t));
  pBuf2 = realloc(pdfa->indexes, max_indexes * sizeof(attrib_t));
  if (pBuf == NULL || pBuf2 == NULL) {
    fprintf(stderr, "No memory left for dfa: %s", pdfa->name);
    exit(EXIT_FAILURE);
  }

  for (i = pdfa->max_states; i < max_states; i++)
    memset(pBuf + i, 0, sizeof(state_t));
  for (i = pdfa->max_indexes; i < max_indexes; i++)
    memset(pBuf2 + i, 0, sizeof(attrib_t));

  pdfa->states = pBuf;
  pdfa->max_states = max_states;
  pdfa->indexes = pBuf2;
  pdfa->max_indexes = max_indexes;
}



/* 
 * dump a dfa (debugging purpose).
 */

static const char *line =
  "----------------------------------------------------\n";

void
dump_dfa(FILE *f, dfa_t *pdfa)
{
  int i;
  int att, k;

  fprintf(f, line);
  fprintf(f, " name : %s\n", pdfa->name);
  fprintf(f, " Nb states :  %7d, max= %d\n", pdfa->last_state + 1,
	  pdfa->max_states);
  fprintf(f, " Nb Indexes : %7d, max= %d\n", pdfa->last_index,
	  pdfa->max_indexes);
  fprintf(f, " memory needed : %d Mb\n", dfa_size(pdfa) / 1024);
  fprintf(f, line);

  if (dfa_size(pdfa) > 10000) /* change this value if needed */
    return;
  fprintf(f, " state  |   .    |   O    |   X    |   #    |  att \n");
  fprintf(f, line);
  for (i = 1; i != pdfa->last_state + 1; i++) {
    int *pnext = pdfa->states[i].next;
    fprintf(f, " %6d |", i);
    fprintf(f, " %6d | %6d | %6d |", pnext[0], pnext[1], pnext[2]);
    fprintf(f, " %6d |", pnext[OUT_BOARD]);
    att = pdfa->states[i].att;
    k = 0;
    fprintf(f, " %5d:", att);
    while (att != 0 && k < 10) {
      fprintf(f, " %4d", pdfa->indexes[att].val);
      att = pdfa->indexes[att].next;
      k++;
    }
    if (att != 0)
      fprintf(f, " ...");
    fprintf(f, "\n");
  }
  fprintf(f, line);
  fflush(f);
}


/*
 * Reset a dfa
 */

static void
clean_dfa(dfa_t *pdfa)
{
  memset(pdfa->states, 0, pdfa->max_states * sizeof(state_t));
  memset(pdfa->indexes, 0, pdfa->max_indexes * sizeof(attrib_t));
  pdfa->last_state = 1;		/* initial state */
  pdfa->last_index = 0;
  pdfa->indexes[0].val = -1;
}


/* 
 * allocate memory for a new dfa 
 */

void
new_dfa(dfa_t *pdfa, const char *name)
{
  memset(pdfa, 0, sizeof(dfa_t));
  resize_dfa(pdfa, DFA_INIT_SIZE, DFA_INIT_SIZE);
  clean_dfa(pdfa);
  if (name != NULL)
    strcpy(pdfa->name, name);
  else
    strcpy(pdfa->name, "noname ");

  if (dfa_verbose > 1)
    fprintf(stderr, "dfa %s is born :)\n", pdfa->name);

}

/*
 * free memory used by a dfa
 */

void
kill_dfa(dfa_t *pdfa)
{
  free(pdfa->states);
  free(pdfa->indexes);
  if (dfa_verbose > 1)
    fprintf(stderr, "dfa %s is dead :(\n", pdfa->name);

  memset(pdfa, 0, sizeof(dfa_t));
}


/*
 * Copy a dfa and resize the destination dfa if necessary.
 */

void
copy_dfa(dfa_t *p_to, dfa_t *p_from)
{
  assert(p_to != p_from);

  if (p_to->max_states < p_from->last_state)
    resize_dfa(p_to, p_from->max_states, p_to->max_indexes);

  if (p_to->max_indexes < p_from->last_index)
    resize_dfa(p_to, p_to->max_states, p_from->max_indexes);

  clean_dfa(p_to);

  memcpy(p_to->states, p_from->states,
	 sizeof(state_t) * (p_from->last_state + 1));
  memcpy(p_to->indexes, p_from->indexes,
	 sizeof(attrib_t) * (p_from->last_index + 1));

  p_to->last_state = p_from->last_state;
  p_to->last_index = p_from->last_index;
}


/*
 * print c dfa:
 * print the dfa in c format.
 */

void
print_c_dfa(FILE *of, const char *name, dfa_t *pdfa)
{
  int i;

  if (sizeof(unsigned short) < 2) {
    fprintf(of, "#error shorts too short");
    fprintf(stderr, "Error: shorts are expected to be at least 2 bytes long.\n");
    exit(EXIT_FAILURE);
  }

  assert(dfa_minmax_delta(pdfa, -1, 1) > 0);
  if (dfa_minmax_delta(pdfa, -1, 0)  > 65535) {
    fprintf(of, "#error too many states");
    fprintf(stderr, "Error: The dfa states are too disperse. Can't fit delta into a short.\n");
    exit(EXIT_FAILURE);
  }

  if (pdfa->last_index + 1 > 65535) {
    fprintf(of, "#error too many states");
    fprintf(stderr, "Error: Too many index entries. Can't fit delta into a short.\n");
    exit(EXIT_FAILURE);
  }


  fprintf(of, "\n#include \"dfa.h\"\n");

  fprintf(of, "static const state_rt_t state_%s[%d] = {\n",
	  name, pdfa->last_state + 1);
  for (i = 0; i != pdfa->last_state + 1; i++) {
    int j;
    fprintf(of, "{%d,", pdfa->states[i].att);
    fprintf(of, "{");
    for (j = 0; j < 4; j++) {
      int n = pdfa->states[i].next[j];
      assert((n == 0) || ((n - i > 0) && (n - i < 65535)));
      fprintf(of, "%d", n ? n - i : 0);
      if (j != 3)
        fprintf(of, ",");
    }
    fprintf(of, "}},%s", ((i+1)%3 ? "\t" : "\n"));
  }
  fprintf(of, "};\n\n");


  fprintf(of, "static const attrib_rt_t idx_%s[%d] = {\n",
	  name, pdfa->last_index + 1);
  for (i = 0; i != pdfa->last_index + 1; i++)
    fprintf(of, "{%d,%d},%s", pdfa->indexes[i].val, pdfa->indexes[i].next,
                              ((i+1)%4 ? "\t" : "\n"));
  fprintf(of, "};\n\n");

  fprintf(of, "static dfa_rt_t dfa_%s = {\n", name);
  fprintf(of, " \"%s\",\n", name);
  fprintf(of, " %d,\n", pdfa->pre_rotated);
  fprintf(of, "state_%s,\n", name);
  fprintf(of, "idx_%s", name);
  fprintf(of, "};\n");

}


/*
 * Create a linear dfa from a string and an attributes value
 * and resize the dfa if needed.
 *
 * For eg.
 * create_dfa (pdfa, "Oo?.", 2001)
 * gives:
 *
 *           1              0,1            0,1,2             0
 * (1,{}) -------> (2,{}) -------> (3,{}) -------> (4,{}) ------> (5,{2001})
 *                                                  
 * An empty string force a junk pattern : The scanner will always 
 * consider this pattern as active.
 *
 * The possible input symbols are :
 * 
 * '.', ',', '*', '!' for EMPTY expected.
 * 'X'                for BLACK expected.
 * 'O'                for WHITE expected.
 * 'x'                for BLACK|EMPTY expected.
 * 'o'                for WHITE|EMPTY expected.
 * '#', '+', '-', '|' for OUT_BOARD expected.
 * '?'                for EMPTY|BLACK|WHITE expected.
 * '$'                for EMPTY|BLACK|WHITE|OUT_BOARD expected.
 */

static void
create_dfa(dfa_t *pdfa, const char *str, int att_val)
{
  int new_state;

  if (dfa_verbose > 1)
    fprintf(stderr, "linear dfa in %s with string: %s\n", pdfa->name, str);

  assert(str != NULL);
  assert(pdfa->max_states > 1);
  assert(pdfa->max_indexes > 1);

  clean_dfa(pdfa);
  new_state = 1;
  for (; *str != '\0' && strchr("$#+-|OoXx.?,!a*", *str); str++) {
    memset(pdfa->states[new_state].next, 0, 4 * sizeof(int));
    if (strchr("$?.ox,a!*", *str))
      pdfa->states[new_state].next[0] = new_state + 1;
    if (strchr("$?Oo", *str))
      pdfa->states[new_state].next[1] = new_state + 1;
    if (strchr("$?Xx", *str))
      pdfa->states[new_state].next[2] = new_state + 1;
    if (strchr("$#+-|", *str))
      pdfa->states[new_state].next[OUT_BOARD] = new_state + 1;
    new_state++;
    if (new_state >= pdfa->max_states)
      resize_dfa(pdfa, pdfa->max_states + DFA_RESIZE_STEP,
		 pdfa->max_indexes);
  }
  memset(pdfa->states[new_state].next, 0, 4 * sizeof(int));

  pdfa->last_index++;
  if (pdfa->last_index >= pdfa->max_indexes)
    resize_dfa(pdfa, pdfa->max_states,
	       pdfa->max_indexes + DFA_RESIZE_STEP);

  memset(&(pdfa->indexes[pdfa->last_index]), 0, sizeof(attrib_t));
  pdfa->states[new_state].att = pdfa->last_index;

  pdfa->indexes[pdfa->states[new_state].att].val = att_val;
  pdfa->indexes[pdfa->states[new_state].att].next = 0;
  pdfa->last_state = new_state;
}


/**************************
 * Test array with a      *
 * hash table             *
 **************************/
/* used by sync_product   *
 * to store visited states*
 **************************/

#define MAX_HASH_VALUE 4096

typedef struct entry {
  int l, r; /* key */
  int val; /* value */
  struct entry *pnext; /* NULL if end of list */
} entry_t;

typedef struct test_array {
  entry_t *hash[MAX_HASH_VALUE];
} test_array_t;


/* initialize empty lists */
static void
new_test_array(test_array_t *pta)
{
  int h;

  for (h = 0; h != MAX_HASH_VALUE ; h++)
    pta->hash[h] = NULL;
}

/* Searh for (l, r) in the linked list plist */
static int 
get_from_entry_list(entry_t *plist, int l, int r)
{
  int val = 0;
  
  while (plist != NULL) {
    if (plist->l == l && plist->r == r)
      val = plist->val;
    plist = plist->pnext;
  }
  return val;
}

/* get the value associated with (l, r) or 0 if none */
static int
get_from_test_array(test_array_t *pta, int l, int r)
{
  return get_from_entry_list(pta->hash[(l+r) % MAX_HASH_VALUE], l, r);
}


/* insert a new entry at the beginning of the linked list pplist */
static void
add_to_entry_list(entry_t **pplist, int l, int r, int val)
{
  entry_t *new_entry;

  /* make sure val > 0: val = 0 is used in get_from_entry_list */
  assert(val > 0);
  assert(!get_from_entry_list(*pplist, l, r));

  new_entry = malloc(sizeof(entry_t));
  if (new_entry == NULL) {
    fprintf(stderr, "No memory left for new entry\n");
    exit(EXIT_FAILURE);
  }
  new_entry->pnext = *pplist;
  new_entry->l = l;
  new_entry->r = r;
  new_entry->val = val;
  *pplist = new_entry;
}


/* add a value at (l, r) */
static void
add_to_test_array(test_array_t *pta, int l, int r, int val)
{
  add_to_entry_list(&(pta->hash[(l+r) % MAX_HASH_VALUE]), l, r, val);
}

/* free the elements of the linked list plist */
static void
free_entry_list(entry_t *plist)
{
  entry_t *pentry;
  
  while (plist != NULL) {
    pentry = plist;
    plist = plist->pnext;
    free(pentry);
  }
}

/* free allocated memory */
static void
free_test_array(test_array_t *pta)
{
  int h;

  for (h = 0; h != MAX_HASH_VALUE; h++) {
    free_entry_list(pta->hash[h]);
    pta->hash[h] = NULL;
  }
}


/* 
 * Synchronization product between two automata.
 *
 * L(A) is the set of patterns recognized by the automaton A.
 *
 * A syncronized product betwenn two acyclic deterministic automata
 * A1 and A2 is an acyclic deterministic classifier A1xA2 that 
 * recognize and classify the languages 
 * L(A1), L(A2), L(A1 Union A2) and L(A1 Inter A2).
 *
 * This algorithm do the product and the reduction at the same time.
 *
 * See Hopcroft & Ullman "The design and analysis of computer algorithms"
 * Ed. Addison-Wesley, Reading MA, 1974
 * For the theorical aspects.
 */

/* globals used to improve readability */
static dfa_t *gpout, *gpleft, *gpright;

/* Hash table used to test if a state has already been
   visited and then give its position in the new automaton. */
static test_array_t gtest;

static void
do_sync_product(int l, int r)
{
  int c;
  int nextl, nextr;
  int state;

  state = gpout->last_state;

  /* unify the attributes of states l and r */
  gpout->states[state].att = union_att(gpout, gpleft, gpleft->states[l].att,
				       gpright, gpright->states[r].att);

  /* scan each possible out-transition */
  for (c = 0; c != 4; c++) {
    nextl = gpleft->states[l].next[c];
    nextr = gpright->states[r].next[c];
    assert(nextl < gpleft->last_state + 1);
    assert(nextr < gpright->last_state + 1);
    
    /* transition to (0,0) mean no transition at all */
    if (nextl != 0 || nextr != 0) {
      /* if the out-state doesn't already exist */
      if (get_from_test_array(&gtest, nextl, nextr) == 0) {
	/* create it! */
	gpout->last_state++;
	if (gpout->last_state >= gpout->max_states)
	  resize_dfa(gpout, gpout->max_states + DFA_RESIZE_STEP,
		     gpout->max_indexes);
	
	add_to_test_array(&gtest, nextl, nextr, gpout->last_state);
	
	/* link it */
	gpout->states[state].next[c] = gpout->last_state;
	
	/* create also its sub-automaton */
	do_sync_product(nextl, nextr);
      }
      else {
	/* link it */
	gpout->states[state].next[c] =
	  get_from_test_array(&gtest, nextl, nextr);
      }
    }
    else {
      /* no output by c from the actual state */
      gpout->states[state].next[c] = 0;
    }
  }
}

static void
sync_product(dfa_t *pout, dfa_t *pleft, dfa_t *pright)
{
  pout->last_index = 0;

  if (dfa_verbose > 2) {
    fprintf(stderr, "Product between %s and %s\n", pleft->name, pright->name);
    fprintf(stderr, "result in %s\n", pout->name);
  }


  gpout = pout;
  gpleft = pleft;
  gpright = pright;
  new_test_array(&gtest);
  add_to_test_array(&gtest, 1, 1, 1);
  pout->last_state = 1;

  do_sync_product(1, 1);

  free_test_array(&gtest);
}

/* 
 * Init/end functions
 */

void
dfa_init(void)
{
  int ii;
  int j;

  if (dfa_verbose > 1)
    fprintf(stderr, "dfa: init\n");
  dfa_was_initialized++;
  buildSpiralOrder(spiral);
  for (j = 0; j < DFA_BINS; j++)
    new_dfa(&(aux_dfa[j]), "binAux ");
  new_dfa(&aux_temp, "tempAux ");

  /* set the private board to OUT_BOARD */
  for (ii = 0; ii < 4 * DFA_MAX_BOARD * 4 * DFA_MAX_BOARD; ii++)
    dfa_p[ii] = OUT_BOARD;
}

void
dfa_end(void)
{
  int j;

  if (dfa_verbose > 1)
    fprintf(stderr, "dfa: end\n");

  for (j = 0; j < DFA_BINS; j++)
    kill_dfa(&(aux_dfa[j]));
  kill_dfa(&aux_temp);
  dfa_was_initialized--;
}


/*
 * Returns max or min jump distance from state to next[next_index] for
 * all states.  If next_index < 0, then max/min for all for states.
 */

int 
dfa_minmax_delta(dfa_t *pdfa, int next_index, int isMin) {

  int ret, i, j;
  assert(next_index <= 3);
 
  if (isMin)
    ret = 99999;
  else
    ret = -1;

  for (i = 0; i <= pdfa->last_state; i++) {
    for (j = 0; j < 4; j++) {
      if (j == next_index || next_index < 0) { 
        int next = pdfa->states[i].next[j];
        if (!next)
          continue;
        if (isMin) {
          if (ret > next - i)
            ret = next - i;
        }
	else {
          if (ret < next - i)
            ret = next - i;
        }
      }
    }
  }

  return ret;
}


/*
 * Re-orders DFA into a canonical form, which does a half-hearted 
 * attempt to reduce the size of jumps for all states entries, and
 * guarantees the jumps are all forward-only.
 */
void
dfa_shuffle(dfa_t *pdfa)
{
  struct state *old_states;
  int *state_to;
  int *state_from;
  int *queue1;
  int *queue2;
  int *tempq;
  int next_new_state;
  int q1p;
  int q2p;
  int i, j;

  state_to = calloc(pdfa->last_state+1, sizeof(int));
  state_from = calloc(pdfa->last_state+1, sizeof(int));

  queue1 = malloc((pdfa->last_state+1) * sizeof(int));
  queue2 = malloc((pdfa->last_state+1) * sizeof(int));
  q1p = 1;
  q2p = 0;
  queue1[0] = 1;  /* i.e. start at state 1. */
  state_from[0] = state_to[0] = 0;
  state_from[1] = state_to[1] = 1;
  next_new_state = 2;

  while (q1p) {
    for (i = 0; i < q1p; i++) {
      for (j = 0; j < 4; j++) {
        int n = pdfa->states[queue1[i]].next[j];
        if (n && !state_to[n]) {
          state_to[n] = next_new_state;
          state_from[next_new_state] = n;
          next_new_state++;
          queue2[q2p++] = n;
        }
      }
    }
    tempq = queue1;
    queue1 = queue2;
    queue2 = tempq;
    q1p = q2p;
    q2p = 0;
  }

  old_states = malloc((pdfa->last_state+1) * sizeof(struct state));
  for (i = 1; i <= pdfa->last_state; i++) {
    for (j = 0; j < 4; j++) {
      old_states[i].next[j] = pdfa->states[i].next[j];
      old_states[i].att = pdfa->states[i].att;
    }
  }
  for (i = 1; i <= pdfa->last_state; i++) {
    for (j = 0; j < 4; j++) {
      assert(state_to[i] > 0);
      pdfa->states[i].next[j] = state_to[old_states[state_from[i]].next[j]];
    } 
    pdfa->states[i].att = old_states[state_from[i]].att;
  } 
}


/*
 * Merges cached dfas into the master DFA
 */
void 
dfa_finalize(dfa_t *pdfa) 
{
  int j;
  int next_bin = aux_count;
  int last_bin = aux_count + DFA_BINS - 1;
  while (next_bin + 1 != last_bin) {
    for (j = aux_count + 1; j <= last_bin; j += 2) {
      if (j+1 == next_bin)
        copy_dfa(&aux_dfa[next_bin % DFA_BINS], &aux_dfa[j % DFA_BINS]);
      else
        sync_product(&aux_dfa[next_bin % DFA_BINS], 
                     &aux_dfa[j % DFA_BINS], 
                     &aux_dfa[(j+1) % DFA_BINS]);
      next_bin++;
    }
    last_bin = next_bin - 1;
    aux_count--;
    next_bin = aux_count;
  }
  copy_dfa(pdfa, &aux_dfa[last_bin % DFA_BINS]);
  
  compactify_att(pdfa);
}


/*
 * Add a new string with attribute att_val into the dfa.
 * if the new size of the dfa respect some size conditions
 * return increase in kB or -1 if the pattern was rejected.
 * This function never rejects string of length <= 1.
 */

float
dfa_add_string(dfa_t *pdfa, const char *str, int pattern_index, int ll)
{
  dfa_t *new_dfa = &(aux_dfa[aux_count % DFA_BINS]);
  dfa_t *old_dfa = &(aux_dfa[(aux_count+1) % DFA_BINS]);
  float ratio;
  char strrot[MAX_ORDER+1];

  if (ll == 0)
    strcpy(strrot, str);
  else {
    int i, j;
    char strdollar[MAX_ORDER+1];
    memset(strdollar, '$', sizeof(char) * (MAX_ORDER + 1));
    strcpy(strdollar, str);
    strdollar[strlen(str)] = '$';
    memset(strrot, '$', sizeof(char) * (MAX_ORDER + 1));
    for (i = 0; i < MAX_ORDER/2; i++) {
      for (j = 0; j < MAX_ORDER+1; j++) {
        if (spiral[i][0] == spiral[j][ll]) {
          if (0 && (spiral[i][0] == 84 || spiral[i][0] == -84
              || spiral[i][0]== 1   || spiral[i][0] == -1
              || spiral[j][ll] == 84 || spiral[j][ll] == -84
              || spiral[j][ll] == 1  || spiral[j][ll] == -1 ) )
            fprintf(stderr, "i: %d  j: %d\n", i, j);
          assert(strrot[i] == '$');
          strrot[i] = strdollar[j];
          break;
        }
      }
      assert(j < MAX_ORDER+1);
    }
    j = MAX_ORDER;
    while (strrot[j] == '$') {
      j--;
    }
    strrot[j+1] = 0;
  }

  if (dfa_verbose > 1) {
    fprintf(stderr, "Adding to dfa %s the string: %s\n", pdfa->name, strrot);
    fprintf(stderr, "  pat_ind: %d; rotation: %d at bin: %d\n",
	    pattern_index, ll, aux_count);
  }

  assert(dfa_was_initialized > 0);
  assert(pdfa != NULL);

  if (pdfa->pre_rotated)
    pattern_index = pattern_index * 8 + ll;

  create_dfa(&aux_temp, strrot, pattern_index);

  /* then we do the synchronization product with dfa */
  sync_product(new_dfa, old_dfa, &aux_temp);
  aux_count++;

  ratio = 1;
  if (dfa_size(old_dfa) > 0)
    ratio = (float) (dfa_size(new_dfa) / dfa_size(old_dfa));

  return ratio;
}


/*
 * Build a pattern string from a pattern.
 * str must refer a buffer of size greater than MAX_ORDER.
 */
void
pattern_2_string(struct pattern *pat, struct patval_b *elements,
		 char *str, int trans, int ci, int cj)
{
  char work_space[DFA_MAX_BOARD * 4][DFA_MAX_BOARD * 4];
  int m, n;			/* anchor position */
  int edges, borders, to_test;
  int i, j, k;
  char c;

  m = DFA_MAX_BOARD * 2 + ci;
  n = DFA_MAX_BOARD * 2 + cj;	/* position of the anchor */

  assert(dfa_was_initialized);
  memset(str, 0, MAX_ORDER);
  memset(work_space, '#', sizeof(work_space));

  if (dfa_verbose > 0)
    fprintf(stderr, "converting pattern into string.\n");

  /* basic edge constraints */
  for (i = DFA_MAX_BOARD; i != DFA_MAX_BOARD * 3; i++)
    for (j = DFA_MAX_BOARD; j != DFA_MAX_BOARD * 3; j++)
      work_space[i][j] = '$';

  /* pattern mask */
  for (i = pat->mini + m; i != pat->maxi + m + 1; i++)
    for (j = pat->minj + n; j != pat->maxj + n + 1; j++)
      work_space[i][j] = '?';

  /* more advanced edge constraints */

  /* South constraint */
  if (pat->edge_constraints & SOUTH_EDGE) {
    for (i = m + pat->maxi + 1; i != DFA_MAX_BOARD * 3; i++)
      for (j = 0; j != DFA_MAX_BOARD * 3; j++)
	work_space[i][j] = '-';
  }

  /* East constraint */
  if (pat->edge_constraints & EAST_EDGE) {
    for (i = 0; i != DFA_MAX_BOARD * 3; i++)
      for (j = n + pat->maxj + 1; j != DFA_MAX_BOARD * 3; j++)
	work_space[i][j] = '|';
  }
  
  /* North constraint */
  if (pat->edge_constraints & NORTH_EDGE) {
    for (i = 0; i != m + pat->mini; i++)
      for (j = 0; j != DFA_MAX_BOARD * 4; j++)
	work_space[i][j] = '-';
  }

  /* West constraint */
  if (pat->edge_constraints & WEST_EDGE) {
    /* take care not to erase the south edge constraint */
    for (i = 0; i != m + pat->maxi + 1; i++)
      for (j = 0; j != n + pat->minj; j++)
	work_space[i][j] = '|';

    /* complete the last corner only if necessary */
    if (!(pat->edge_constraints & SOUTH_EDGE)) {
      for (i = m + pat->maxi + 1; i != DFA_MAX_BOARD * 3; i++)
	for (j = 0; j != n + pat->minj; j++)
	  work_space[i][j] = '|';
    }
  }

  /* dump */
  if (dfa_verbose > 4) {
    for (i = DFA_MAX_BOARD - 1; i != DFA_MAX_BOARD * 3 + 1; i++) {
      for (j = DFA_MAX_BOARD - 1; j != DFA_MAX_BOARD * 3 + 1; j++) {
	if (i == m && j == n)
	  fprintf(stderr, "s");	/* mark the anchor */
	else
	  fprintf(stderr, "%c", work_space[i][j]);
      }
      fprintf(stderr, "\n");
    }
    fprintf(stderr, "\n");
  }

  /* pattern representation on the work space */
  for (k = 0; k != pat->patlen; k++) {
    c = EXPECTED_VAL(elements[k].att);
    assert(work_space[m + elements[k].x - ci][n + elements[k].y - cj] == '?');
    work_space[m + elements[k].x - ci][n + elements[k].y - cj] = c;
  }

  /* dump */
  if (dfa_verbose > 3) {
    for (i = DFA_MAX_BOARD - 1; i != DFA_MAX_BOARD * 3 + 1; i++) {
      for (j = DFA_MAX_BOARD - 1; j != DFA_MAX_BOARD * 3 + 1; j++) {
	if (i == m && j == n)
	  fprintf(stderr, "s");	/* mark the anchor */
	else
	  fprintf(stderr, "%c", work_space[i][j]);
      }
      fprintf(stderr, "\n");
    }
    fprintf(stderr, "\n");
  }

  /* Now we can build the smallest pattern string possible 
   * from the anchor */

  to_test = pat->patlen;	/* How many positions left to test ? */
  edges = pat->edge_constraints;	/* how many constraint tested ? */
  borders = 0xF; 
  /* we must test at least one intersection by border for 
   * patterns like
   * 
   * ???
   * O.O
   * ???
   * 
   * To ensure edge position.
   */

  for (k = 0;
       (k != MAX_ORDER - 1) && ((borders > 0) || edges || to_test > 0);
       k++) {
    j = spiral[k][trans] % (4 * DFA_MAX_BOARD);
    if (j >= 2 * DFA_MAX_BOARD)
      j -= 4 * DFA_MAX_BOARD;
    if (j <  - 2 * DFA_MAX_BOARD)
      j += 4 * DFA_MAX_BOARD;
    i = (spiral[k][trans] - j) / (4 * DFA_MAX_BOARD);

    if (i == pat->maxi)
      borders &= ~SOUTH_EDGE;
    if (i == pat->mini)
      borders &= ~NORTH_EDGE;
    if (j == pat->maxj)
      borders &= ~EAST_EDGE;
    if (j == pat->minj)
      borders &= ~WEST_EDGE;
    
    assert(m + i < DFA_MAX_BOARD * 3 && m + i < DFA_MAX_BOARD * 3);
    str[k] = work_space[m + i][n + j];
    assert(strchr("XOxo.,a!?$#|-+", str[k]));
    
    if (strchr("XOxo.,a!", str[k]))
      to_test--;
    if (strchr("#|-+", str[k])) {
      if (i > pat->maxi)
	edges &= ~SOUTH_EDGE;
      if (i < pat->mini)
	edges &= ~NORTH_EDGE;
      if (j > pat->maxj)
	edges &= ~EAST_EDGE;
      if (j < pat->minj)
	edges &= ~WEST_EDGE;
    }
  }
  
  assert(k < MAX_ORDER);
  str[k] = '\0';		/* end of string */

  if (0 && dfa_verbose > 0)
    fprintf(stderr, "converted pattern %s into string: %s\n", pat->name, str);
}


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
