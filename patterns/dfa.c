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

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * * * * * * * * fast pattern matching with DFA  version 2.9 * * *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include "liberty.h"
#include "patterns.h"
#include "dfa-mkpat.h"
#include "random.h"

#include <assert.h>
#include <stdlib.h>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif


/*********************
 *   Public data     *
 *********************/

/* If > 0 more detailed information is given */
int dfa_verbose = 0;


/*********************
 *  Private data     *
 *********************/

/* auxiliary dfa's for high level functions */
#define DFA_BINS 33 /* Number of temporary bins used to store intermediate DFAs */
static dfa_t aux_dfa[DFA_BINS];	/* used to store intermediate DFAs */
static dfa_t aux_temp;          /* used to store temporary DFAs */

/* To be sure that everything was well initialized */
static int dfa_was_initialized = 0;
static int aux_count = 0;


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

static void dfa_prepare_rotation_data(void);


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

  pBuf = realloc(pdfa->states, max_states * sizeof(*pBuf));
  pBuf2 = realloc(pdfa->indexes, max_indexes * sizeof(*pBuf2));
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

  assert(dfa_minmax_delta(pdfa, -1, 1) > -32768);
  if (dfa_minmax_delta(pdfa, -1, 0)  > 32768) {
    fprintf(of, "#error too many states");
    fprintf(stderr, "Error: The dfa states are too disperse. Can't fit delta into a short.\n");
    exit(EXIT_FAILURE);
  }

  if (pdfa->last_index + 1 > 65535) {
    fprintf(of, "#error too many states");
    fprintf(stderr, "Error: Too many index entries. Can't fit delta into a short.\n");
    exit(EXIT_FAILURE);
  }


  fprintf(of, "\n#include \"dfa-mkpat.h\"\n");

  fprintf(of, "static const state_rt_t state_%s[%d] = {\n",
	  name, pdfa->last_state + 1);
  for (i = 0; i != pdfa->last_state + 1; i++) {
    int j;
    fprintf(of, "{{");
    for (j = 0; j < 4; j++) {
      int n = pdfa->states[i].next[j];
      assert((n == 0) || (abs(n - i) < 32768));
      fprintf(of, "%d", n ? n - i : 0);
      if (j != 3)
        fprintf(of, ",");
    }
    fprintf(of, "}, %d},%s", pdfa->states[i].att, ((i+1)%3 ? "\t" : "\n"));
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
  fprintf(of, "state_%s,\n", name);
  fprintf(of, "idx_%s", name);
  fprintf(of, "};\n");
}


/*
 * Create a linear dfa from a string and an attributes value
 * and resize the dfa if needed.
 *
 * For example:
 * create_dfa(pdfa, "Oo?.", 2001)
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

  new_entry = malloc(sizeof(*new_entry));
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
  int j;

  if (dfa_verbose > 1)
    fprintf(stderr, "dfa: init\n");
  dfa_was_initialized++;

  build_spiral_order();
  dfa_prepare_rotation_data();

  for (j = 0; j < DFA_BINS; j++)
    new_dfa(&(aux_dfa[j]), "binAux ");
  new_dfa(&aux_temp, "tempAux ");
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
dfa_minmax_delta(dfa_t *pdfa, int next_index, int isMin)
{

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

#define DFA_ALIGN 	2

/*
 * Re-orders DFA into a canonical form, which does a half-hearted 
 * attempt to reduce the size of jumps for all states entries.
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

  state_to = calloc(pdfa->last_state+1, sizeof(*state_to));
  state_from = calloc(pdfa->last_state+1, sizeof(*state_from));

  queue1 = malloc((pdfa->last_state+1) * sizeof(*queue1));
  queue2 = malloc((pdfa->last_state+1) * sizeof(*queue2));
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
	/* next_new_state = DFA_ALIGN * ((next_new_state-1) / DFA_ALIGN) + 1;*/
        while (n && !state_to[n]) {
          state_to[n] = next_new_state;
          state_from[next_new_state] = n;
          next_new_state++;
          queue2[q2p++] = n;
	  n = pdfa->states[n].next[0];
        }
      }
    }
    tempq = queue1;
    queue1 = queue2;
    queue2 = tempq;
    q1p = q2p;
    q2p = 0;
  }

  old_states = malloc((pdfa->last_state+1) * sizeof(*old_states));
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


/* Calculate the maximal number of patterns matched at one point for
 * one transformation.  Multiplying this number by 8 gives an upper
 * bound for the total number of matched patterns for all
 * transformation.
 */
int
dfa_calculate_max_matched_patterns(dfa_t *pdfa)
{
  int total_max = 0;
  int *state_max = calloc(pdfa->last_state + 1, sizeof(int));
  char *queued = calloc(pdfa->last_state + 1, sizeof(char));
  int *queue = malloc(pdfa->last_state * sizeof(int));
  int queue_start = 0;
  int queue_end = 1;

  queue[0] = 1;
  while (queue_start < queue_end) {
    int state = queue[queue_start++];
    int k;

    /* Increment maximal number of matched patterns for each pattern
     * matched at current `state'.
     */
    for (k = pdfa->states[state].att; k; k = pdfa->indexes[k].next)
      state_max[state]++;

    if (total_max < state_max[state])
      total_max = state_max[state];

    for (k = 0; k < 4; k++) {
      int next = pdfa->states[state].next[k];

      if (next != 0) {
	if (!queued[next]) {
	  queue[queue_end++] = next;
	  queued[next] = 1;
	}

	if (state_max[next] < state_max[state])
	  state_max[next] = state_max[state];
      }
    }
  }

  assert(queue_end == pdfa->last_state);

  free(state_max);
  free(queued);
  free(queue);

  return total_max;
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

  if (dfa_verbose > 1) {
    fprintf(stderr, "Adding to dfa %s the string: %s\n", pdfa->name, str);
    fprintf(stderr, "  pat_ind: %d; rotation: %d at bin: %d\n",
	    pattern_index, ll, aux_count);
  }

  assert(dfa_was_initialized > 0);
  assert(pdfa != NULL);

  create_dfa(&aux_temp, str, pattern_index);

  /* then we do the synchronization product with dfa */
  sync_product(new_dfa, old_dfa, &aux_temp);
  aux_count++;

  ratio = 1;
  if (dfa_size(old_dfa) > 0)
    ratio = (float) (dfa_size(new_dfa) / dfa_size(old_dfa));

  return ratio;
}


/* Used for quick string rotation. */
static int dfa_rotation_data[DFA_BASE * DFA_BASE];

static void
dfa_prepare_rotation_data(void)
{
  int k;

  for (k = 0; k < DFA_MAX_ORDER; k++)
    dfa_rotation_data[DFA_POS(0, 0) + spiral[k][0]] = k;
}


/* Create a transformation of `string' and store it in
 * `rotated_string'.  The latter must be of at least DFA_MAX_ORDER
 * characters in length. */
void
dfa_rotate_string(char *rotated_string, const char *string, int transformation)
{
  if (transformation > 0) {
    int k;
    int length = strlen(string);
    int new_length = 0;

    memset(rotated_string, '$', DFA_MAX_ORDER);

    for (k = 0; k < length; k++) {
      if (string[k] != '$') {
	int string_position = dfa_rotation_data[DFA_POS(0, 0)
						+ spiral[k][transformation]];
	rotated_string[string_position] = string[k];
	if (string_position + 1 > new_length)
	  new_length = string_position + 1;
      }
    }

    rotated_string[new_length] = 0;
  }
  else
    strcpy(rotated_string, string);
}


/*
 * Build a pattern string from a pattern.  `str' must refer a buffer
 * of size greater than DFA_MAX_ORDER.
 */
void
pattern_2_string(struct pattern *pat, struct patval_b *elements,
		 char *str, int ci, int cj)
{
  char work_space[DFA_MAX_BOARD * 4][DFA_MAX_BOARD * 4];
  int m, n;			/* anchor position */
  int edges, borders, to_test;
  int i, j, k;
  char c;

  m = DFA_MAX_BOARD * 2 + ci;
  n = DFA_MAX_BOARD * 2 + cj;	/* position of the anchor */

  assert(dfa_was_initialized);
  memset(str, 0, DFA_MAX_ORDER);
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
       (k != DFA_MAX_ORDER - 1) && ((borders > 0) || edges || to_test > 0);
       k++) {
    j = spiral[k][0] % DFA_BASE;
    if (j >= DFA_MAX_BOARD)
      j -= DFA_BASE;
    if (j <= -DFA_MAX_BOARD)
      j += DFA_BASE;
    i = (spiral[k][0] - j) / DFA_BASE;

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
  
  assert(k < DFA_MAX_ORDER);
  str[k] = '\0';		/* end of string */

  if (0 && dfa_verbose > 0)
    fprintf(stderr, "converted pattern %s into string: %s\n", pat->name, str);
}


/**************************************
 *	Experimental DFA builder      *
 **************************************/

/* This builder differs from the one above in that it builds the whole dfa
 * at once. That is, it must have all the patterns to build and cannot add
 * pattern by pattern. Currently, it is only used in DFA size optimization
 * (it seems to be significantly faster).
 */


/* Allocate a new dfa_attrib structure from a dynamic array. */
static dfa_attrib *
dfa_attrib_new(dfa_attrib_array *array, int string_index)
{
  dfa_attrib *attribute;

  if (array->allocated == DFA_ATTRIB_BLOCK_SIZE) {
    dfa_attrib_block *new_block = malloc(sizeof(*new_block));
    assert(new_block);

    new_block->previous = array->last_block;
    array->last_block = new_block;
    array->allocated = 0;
  }

  attribute = &(array->last_block->attrib[array->allocated++]);
  attribute->next = NULL;
  attribute->string_index = string_index;

  return attribute;
}


/* Initialize dfa_attrib_array structure. */
static void
dfa_attrib_array_reset(dfa_attrib_array *array)
{
  array->last_block = NULL;
  array->allocated = DFA_ATTRIB_BLOCK_SIZE;
}


/* Clear a dynamic array by freeing all blocks befor `cutoff_point'. */
static void
dfa_attrib_array_partially_clear(dfa_attrib_block *cutoff_point)
{
  if (cutoff_point) {
    dfa_attrib_block *block = cutoff_point->previous;

    while (block) {
      dfa_attrib_block *previous = block->previous;
      free(block);
      block = previous;
    }

    cutoff_point->previous = NULL;
  }
}


/* Clear a dynamic array completely. All blocks are freed. */
static void
dfa_attrib_array_clear(dfa_attrib_array *array)
{
  if (array->last_block) {
    dfa_attrib_array_partially_clear(array->last_block);
    free(array->last_block);
    array->last_block = NULL;
  }

  array->allocated = DFA_ATTRIB_BLOCK_SIZE;
}


/* Allocate a new dfa_node structure in a DFA graph. */
static dfa_node *
dfa_node_new(dfa_graph *graph)
{
  dfa_node *node;

  if (graph->allocated == DFA_NODE_BLOCK_SIZE) {
    dfa_node_block *new_block = malloc(sizeof(*new_block));
    assert(new_block);

    new_block->previous = graph->last_block;
    graph->last_block = new_block;
    graph->allocated = 0;
  }

  graph->num_nodes++;
  node = &(graph->last_block->node[graph->allocated++]);
  memset(node, 0, sizeof(dfa_node));

  return node;
}


/* This is a hash table used to quickly find a DFA node using a linked list
 * of its attributes as a key.
 */
static dfa_hash_entry *dfa_hash_table[DFA_HASH_TABLE_SIZE];
static dfa_hash_block *dfa_hash_last_block = NULL;
static int dfa_hash_allocated;


/* Allocate a dfa_entry structure dynamically. */
static dfa_hash_entry *
dfa_hash_entry_new(void)
{
  if (dfa_hash_allocated == DFA_HASH_BLOCK_SIZE) {
    dfa_hash_block *new_block = malloc(sizeof(*new_block));
    assert(new_block);

    new_block->previous = dfa_hash_last_block;
    dfa_hash_last_block = new_block;
    dfa_hash_allocated = 0;
  }

  return &(dfa_hash_last_block->entry[dfa_hash_allocated++]);
}


/* Clear the hash table completely. Used after having finished a graph level. */
static void
dfa_hash_clear(void)
{
  memset(dfa_hash_table, 0, DFA_HASH_TABLE_SIZE * sizeof(dfa_hash_entry *));

  if (dfa_hash_last_block) {
    dfa_hash_block *block = dfa_hash_last_block->previous;

    while (block) {
      dfa_hash_block *previous = block->previous;
      free(block);
      block = previous;
    }

    dfa_hash_last_block->previous = NULL;
    dfa_hash_allocated = 0;
  }
  else
    dfa_hash_allocated = DFA_HASH_BLOCK_SIZE;
}


/* Compute the hash value of a key (linked list of attributes). */
static int
dfa_hash_value(dfa_attrib *key)
{
  int hash_value = DFA_HASH_VALUE_1 * key->string_index;
  if (key->next) {
    hash_value += DFA_HASH_VALUE_2 * key->next->string_index;
    if (key->next->next)
      hash_value += DFA_HASH_VALUE_3 * key->next->next->string_index;
  }

  return hash_value % DFA_HASH_TABLE_SIZE;
}


/* Search for a node with a given key in the hash table. */
static dfa_node *
dfa_hash_search(dfa_attrib *key)
{
  int hash_value = dfa_hash_value(key);
  dfa_hash_entry *entry;

  for (entry = dfa_hash_table[hash_value]; entry; entry = entry->next) {
    dfa_attrib *left = key;
    dfa_attrib *right = entry->key;

    while (left && right) {
      if (left->string_index != right->string_index)
	break;

      left = left->next;
      right = right->next;
    }

    if (!left && !right)
      return entry->value;
  }

  return NULL;
}


/* Add a node to the table. The list of strings which pass through it is used
 * as a key.
 */
static void
dfa_hash_add_node(dfa_node *node)
{
  int hash_value = dfa_hash_value(node->passing_strings);
  dfa_hash_entry *entry;

  entry = dfa_hash_entry_new();
  entry->next = dfa_hash_table[hash_value];
  dfa_hash_table[hash_value] = entry;

  entry->key = node->passing_strings;
  entry->value = node;
}


/* DFA iterator. Used to walk the array of nodes in backward direction. */
static dfa_node_block *dfa_iterator_block;
static int dfa_iterator_node_num;


/* Reset the iterator. The last added node of the specified graph becomes
 * the current node.
 */
static dfa_node*
dfa_iterator_reset(dfa_graph *graph)
{
  assert(graph->last_block);

  if (graph->allocated > 0) {
    dfa_iterator_block = graph->last_block;
    dfa_iterator_node_num = graph->allocated - 1;
  }
  else {
    dfa_iterator_block = graph->last_block->previous;
    assert(dfa_iterator_block);
    dfa_iterator_node_num = DFA_NODE_BLOCK_SIZE - 1;
  }

  return &(dfa_iterator_block->node[dfa_iterator_node_num]);
}


/* Shift the current node pointer one node backwards. */
static dfa_node*
dfa_iterate(void)
{
  dfa_iterator_node_num--;
  if (dfa_iterator_node_num < 0) {
    dfa_iterator_block = dfa_iterator_block->previous;
    assert(dfa_iterator_block);
    dfa_iterator_node_num = DFA_NODE_BLOCK_SIZE - 1;
  }

  return &(dfa_iterator_block->node[dfa_iterator_node_num]);
}


/* Initialize a dfa_graph structure. */
void
dfa_graph_reset(dfa_graph *graph)
{
  graph->num_nodes = 0;
  graph->root = NULL;

  graph->last_block = NULL;
  graph->allocated = DFA_NODE_BLOCK_SIZE;
  dfa_attrib_array_reset(&(graph->attributes));
}


/* Free all resources associated with the specified DFA graph. */
static void
dfa_graph_clear(dfa_graph *graph)
{
  dfa_node_block *block = graph->last_block;
  graph->num_nodes = 0;
  graph->root = NULL;

  while (block) {
    dfa_node_block *previous = block->previous;
    free(block);
    block = previous;
  }

  graph->last_block = NULL;
  graph->allocated = DFA_NODE_BLOCK_SIZE;

  dfa_attrib_array_clear(&(graph->attributes));
}


/* dfa_graph_build_level() builds a level of a graph. Level `n' is a set of
 * nodes which correspond to n's element of a string. When matching using a
 * dfa, nodes of level `n' are only checked at (n + 1)'s iteration (root node
 * is considered to be level -1, but is matched at iteration 0).
 */
static void
dfa_graph_build_level(dfa_graph *graph, char **strings, int level,
		      dfa_node *terminal_node,
		      dfa_attrib_array *passing_strings_array)
{
  int save_num_nodes = graph->num_nodes;
  dfa_attrib_block *cutoff_point;
  dfa_node *node;
  dfa_node *this_terminal_node = dfa_iterator_reset(graph);

  cutoff_point = passing_strings_array->last_block;
  dfa_hash_clear();

  /* Walk through all nodes of the previous level (backwards, but that doesn't
   * matter - it's just because iterator works that way).
   */
  for (node = this_terminal_node; node != terminal_node; node = dfa_iterate()) {
    int k;
    int num_masks = 0;
    char mask[4];
    dfa_attrib *passing_string;
    dfa_attrib **link = &(node->attributes);
    dfa_attrib *new_passing_strings[4];
    dfa_attrib **new_link[4];

    /* Calculate all different masks for subnodes. For instance, if there are
     * three strings passing through a node of level 1, say "X$...", "Xx..."
     * and "XO...", there will be three masks: 8 (stands for '#'), 5 ('X' and
     * '.') and 2 ('O'). String "X$..." will pass further through all three
     * subnodes, "Xx..." - through subnode corresponding to mask 5 and string
     * "XO..." - through subnode corresponding to mask 2.
     */
    for (passing_string = node->passing_strings;
	 passing_string && num_masks < 4;
	 passing_string = passing_string->next) {
      int index = passing_string->string_index;
      char string_mask = strings[index][level];

      if (string_mask) {
	int limit = num_masks;

	for (k = 0; k < limit; k++) {
	  char common_branches = string_mask & mask[k];

	  if (common_branches && common_branches != mask[k]) {
	    /* Split a mask, since the string passes through a "part" of it. */
	    mask[k] ^= common_branches;
	    mask[num_masks++] = common_branches;
	  }
	  
	  string_mask ^= common_branches;
	}

	if (string_mask) {
	  /* If there is no mask corresponding to a (part) of the string's
	   * mask, add it now.
	   */
	  mask[num_masks++] = string_mask;
	}
      }
      else {
	/* If the string ends at this level, add its index to the list of
	 * matched strings of the current node. Not used at the moment,
	 * since this builder isn't used for actual DFA building.
	 */
	*link = dfa_attrib_new(&(graph->attributes), index);
	link = &((*link)->next);
      }
    }
    
    for (k = 0; k < num_masks; k++)
      new_link[k] = &(new_passing_strings[k]);

    /* Now, for each mask, create a list of all strings which will follow it
     * (pass through a node corresponding to it). It is possible to merge this
     * loop with the previous one, but it is simplier to keep them separated.
     */
    for (passing_string = node->passing_strings; passing_string;
	 passing_string = passing_string->next) {
      int index = passing_string->string_index;

      for (k = 0; k < num_masks; k++) {
	if (strings[index][level] & mask[k]) {
	  *(new_link[k]) = dfa_attrib_new(passing_strings_array, index);
	  new_link[k] = &((*(new_link[k]))->next);
	}
      }
    }

    /* Finally, create new nodes for the masks when necessary. */
    for (k = 0; k < num_masks; k++) {
      int i;

      /* Maybe we have already added such a node? */
      dfa_node *new_node = dfa_hash_search(new_passing_strings[k]);

      if (!new_node) {
	/* If not, create it, save the list of passing strings and add the
	 * new node to hash table.
	 */
	new_node = dfa_node_new(graph);
	new_node->passing_strings = new_passing_strings[k];

	dfa_hash_add_node(new_node);
      }

      /* At this moment we convert the masks to actual transitions. These are
       * also unused till we use this builder for actual DFA creation.
       */
      for (i = 0; i < 4; i++) {
	if (mask[k] & (1 << i))
	  node->branch[i] = new_node;
      }
    }
  }

  /* Free the lists of passing strings for the previous level. Useful if we
   * building an exceptionally huge DFA.
   */
  dfa_attrib_array_partially_clear(cutoff_point);

  if (graph->num_nodes != save_num_nodes) {
    /* If we have added any nodes, this level is not the last one. */
    dfa_graph_build_level(graph, strings, level + 1, this_terminal_node,
			  passing_strings_array);
  }
}


/* Convert a pattern to a string of masks. */
static char *
dfa_prepare_string(const char *string)
{
  int k;
  int l = strlen(string);
  char *dfa_string = malloc(l + 1);
  assert(dfa_string);

  for (k = 0; k < l; k++) {
    switch (string[k]) {
    case '$': dfa_string[k] = 15; break;	/* 1111 */
    case '-':
    case '|':
    case '+':
    case '#': dfa_string[k] =  8; break;	/* 1000 */
    case '.':
    case ',':
    case '!':
    case 'a': dfa_string[k] =  1; break;	/* 0001 */
    case '?': dfa_string[k] =  7; break;	/* 0111 */
    case 'O': dfa_string[k] =  2; break;	/* 0010 */
    case 'X': dfa_string[k] =  4; break;	/* 0100 */
    case 'o': dfa_string[k] =  3; break;	/* 0011 */
    case 'x': dfa_string[k] =  5; break;	/* 0101 */

    default: assert(0); 			/* Shouldn't happen. */
    }
  }

  dfa_string[l] = 0;
  return dfa_string;
}


/* Initialize a dfa_patterns structure. */
void
dfa_patterns_reset(dfa_patterns *patterns)
{
  patterns->num_patterns = 0;
  patterns->patterns = NULL;
  patterns->last_pattern = NULL;

  dfa_graph_reset(&(patterns->graph));
}


/* Clear the graph and reset all fields of a dfa_patterns structure. */
void
dfa_patterns_clear(dfa_patterns *patterns)
{
  dfa_pattern *pattern = patterns->patterns;

  while (pattern) {
    int k;
    dfa_pattern *next = pattern->next;

    for (k = 0; k < pattern->num_variations; k++)
      free(pattern->variation[k]);

    free(pattern);
    pattern = next;
  }

  patterns->num_patterns = 0;
  patterns->patterns = NULL;
  patterns->last_pattern = NULL;

  dfa_graph_clear(&(patterns->graph));
}


/* Add a pattern to a list. If `index' is equal to the index of the last
 * added pattern, add a variation to that pattern instead.
 */
void
dfa_patterns_add_pattern(dfa_patterns *patterns, const char *string, int index)
{
  dfa_pattern *pattern = NULL;

  if (index == patterns->num_patterns - 1) {
    assert(patterns->last_pattern);
    assert(patterns->last_pattern->num_variations < 8);

    pattern = patterns->last_pattern;
  }
  else {
    assert(patterns->num_patterns <= index);

    while (patterns->num_patterns <= index) {
      patterns->num_patterns++;
      pattern = malloc(sizeof(*pattern));
      pattern->num_variations = 0;

      if (patterns->last_pattern)
        patterns->last_pattern->next = pattern;
      else
        patterns->patterns = pattern;
      patterns->last_pattern = pattern;
    }

    pattern->current_variation = 0;
    pattern->next = NULL;
  }

  pattern->variation[pattern->num_variations++] = dfa_prepare_string(string);
}


/* Set the variation of the last pattern. Can be used in actual DFA building
 * or to set a hint (results of the previous optimization) for optimization.
 */
void
dfa_patterns_set_last_pattern_variation(dfa_patterns *patterns, int variation)
{
  assert(patterns->last_pattern);
  assert(patterns->last_pattern->num_variations > variation);

  patterns->last_pattern->current_variation = variation;
}


/* Make the shortest variation of the last pattern its current variation. It
 * is used as a starting point in DFA optimization process.
 */
void
dfa_patterns_select_shortest_variation(dfa_patterns *patterns)
{
  int k;
  int min_length;
  dfa_pattern *pattern = patterns->last_pattern;
  assert(pattern);

  pattern->current_variation = 0;
  min_length = strlen(pattern->variation[0]);
  for (k = 1; k < pattern->num_variations; k++) {
    int length = strlen(pattern->variation[k]);

    if (length < min_length) {
      pattern->current_variation = k;
      min_length = length;
    }
  }
}


/* Build a DFA graph for a list of patterns. */
void
dfa_patterns_build_graph(dfa_patterns *patterns)
{
  int k = 0;
  char **strings;
  dfa_attrib_array passing_strings_array;
  dfa_attrib **link;
  dfa_node *error_state;
  dfa_graph *graph = &(patterns->graph);
  dfa_pattern *pattern;
  
  strings = malloc(patterns->num_patterns * sizeof(*strings));
  assert(strings);

  dfa_graph_clear(graph);
  dfa_attrib_array_reset(&passing_strings_array);

  /* Error state node is used as a terminator for level -1 (root node). */
  error_state = dfa_node_new(graph);
  graph->root = dfa_node_new(graph);
  
  /* Add all strings as passing through root node (level -1). */
  link = &(graph->root->passing_strings);
  for (pattern = patterns->patterns; pattern; pattern = pattern->next, k++) {
    if (pattern->num_variations > 0) {
      assert(pattern->current_variation < pattern->num_variations);
      strings[k] = pattern->variation[pattern->current_variation];

      *link = dfa_attrib_new(&passing_strings_array, k);
      link = &((*link)->next);
    }
    else
      strings[k] = NULL;
  }

  dfa_graph_build_level(graph, strings, 0, error_state, &passing_strings_array);

  free(strings);
  dfa_attrib_array_clear(&passing_strings_array);
}


/* dfa_patterns_optimize_variations() tries to reduce the size of DFA by
 * altering pattern variations (in fact, transformations). The algorithm
 * is to change several patterns' variations and if this happens to give
 * size reduction, to keep the change, otherwise, revert.
 *
 * This function contains many heuristically chosen values for variation
 * changing probability etc. They have been chosen by observing algorithm
 * effectiveness and seem to be very good.
 *
 * Note that we subtract 1 from the number of nodes to be consistent with
 * the standard builder, which doesn't count error state.
 */
int *
dfa_patterns_optimize_variations(dfa_patterns *patterns, int iterations)
{
  int k = 0;
  int failed_iterations = 0;
  int min_nodes_so_far;
  int num_nodes_original;
  int *best_variations;
  double lower_limit = 2.0 / patterns->num_patterns;
  double upper_limit = 6.0 / patterns->num_patterns;
  double change_probability = 4.0 / patterns->num_patterns;
  dfa_pattern *pattern;

  best_variations = malloc(patterns->num_patterns * sizeof(*best_variations));
  assert(best_variations);
  for (pattern = patterns->patterns; pattern; pattern = pattern->next, k++)
    best_variations[k] = pattern->current_variation;

  dfa_patterns_build_graph(patterns);
  num_nodes_original = patterns->graph.num_nodes;
  min_nodes_so_far = num_nodes_original;

  fprintf(stderr, "Original number of DFA states: %d\n", min_nodes_so_far - 1);
  fprintf(stderr, "Trying to optimize in %d iterations\n", iterations);

  gg_srand(num_nodes_original + patterns->num_patterns);

  while (iterations--) {
    int changed_variations = 0;
    int k = 0;
    
    /* Randomly change some variations. */
    for (pattern = patterns->patterns; pattern; pattern = pattern->next, k++) {
      if (gg_drand() < change_probability && pattern->num_variations > 1) {
	int new_variation = gg_rand() % (pattern->num_variations - 1);
	if (new_variation >= pattern->current_variation)
	  new_variation++;
	pattern->current_variation = new_variation;
	changed_variations++;
      }
      else
	pattern->current_variation = best_variations[k];
    }

    if (changed_variations == 0) {
      iterations++;
      continue;
    }

    fprintf(stderr, ".");
    dfa_patterns_build_graph(patterns);

    if (patterns->graph.num_nodes < min_nodes_so_far) {
      /* If the new set of variations produces smaller dfa, save it. */
      int k = 0;
      for (pattern = patterns->patterns; pattern; pattern = pattern->next, k++)
	best_variations[k] = pattern->current_variation;

      fprintf(stderr, "\nOptimized: %d => %d states (%d iterations left)\n",
	      min_nodes_so_far - 1, patterns->graph.num_nodes - 1, iterations);
      min_nodes_so_far = patterns->graph.num_nodes;
      failed_iterations = 0;
    }
    else
      failed_iterations++;

    if (failed_iterations >= 30) {
      /* If haven't succeded in 30 last iterations, try to alter variation
       * change probability.
       */
      double delta = gg_drand() / patterns->num_patterns;
      if (change_probability > upper_limit
	  || (change_probability >= lower_limit && gg_rand() % 2 == 0))
	delta = -delta;

      change_probability += delta;
      failed_iterations = 0;
    }
  }

  fprintf(stderr, "\nTotal optimization result: %d => %d states\n",
	  num_nodes_original - 1, min_nodes_so_far - 1);

  dfa_graph_clear(&(patterns->graph));
  return best_variations;
}


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
