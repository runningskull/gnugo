/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
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


/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * * * * * * * * fast pattern matching with DFA  version 2.9 * * *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include "patterns.h"
#include "dfa.h"
#include <assert.h>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#define gg_assert(x) assert(x);


/*********************
 *   Public data     *
 *********************/

/* If > 0 more detailled infomation is given */
int dfa_verbose = 0;

/* conversion array (stupid hack) */
int dfa_asc2val[90] = {
  3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
  3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
  3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
  3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
  3, 3, 3, 3, 3, 3, 0, 3, 3, 3,	/* '.' == 46 */
  3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
  3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
  3, 3, 3, 3, 3, 3, 3, 3, 3, 1,	/* 'O' == 79 */
  3, 3, 3, 3, 3, 3, 3, 3, 2, 3	/* 'X' == 88 */
};

char dfa_val2asc[4] = {
    '.', 'O', 'X', '#' };


/*********************
 *  Private data     *
 *********************/

/* the private board */
int dfa_p[DFA_MAX_BOARD * 4][DFA_MAX_BOARD * 4];
int reverse_spiral[8][DFA_MAX_BOARD * 4][DFA_MAX_BOARD * 4];

/* auxiliary dfa's for high level functions */
static dfa_t aux_dfa1;		/* used to store strings */
static dfa_t aux_dfa2;		/* used to store the result of dfa's products*/

/* To be sure that everything was well initialized */
static int dfa_was_initialized = 0;


/* convert is a table to convert the colors */
const int convert[3][4] = {
  {-1, -1, -1, -1},		/* not used */
  {EMPTY, WHITE, BLACK, OUT_BOARD},	/* WHITE */
  {EMPTY, BLACK, WHITE, OUT_BOARD}	/* BLACK */

};


/* convert ATT_* values to the corresponding expected values on the board */
static const char att2val[8] =
  { '.', 'X', 'O', 'x', 'o', ',', 'a', '!' };

#define EXPECTED_VAL(att_val) att2val[att_val]


/************************************************
 *   forward declaration of private functions   *
 ************************************************/
static void clean_dfa (dfa_t * pdfa);
static void resize_dfa (dfa_t * pdfa, int maxStates, int maxIndexes);
static void create_dfa (dfa_t * pdfa, const char *str, int att_val);
static void do_sync_product (int l, int r);
static void sync_product (dfa_t * pout, dfa_t * pleft, dfa_t * pright);


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

order_t spiral[8][MAX_ORDER];

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

static const order_t generator[4] =
  { { 1, 0}, { 0,  1}, {-1, 0}, {0, -1}  };

void
buildSpiralOrder (order_t order[8][MAX_ORDER])
{
  int Mark[DFA_MAX_BOARD * 4][DFA_MAX_BOARD * 4];
  order_t fifo[8 * MAX_ORDER];
  int top = 0, end = 0;
  int i, j, k, ll;
  int di, dj;
  int delta;

  

  if (dfa_verbose > 1)
    fprintf (stderr, "Building spiral order\n");

  /* First we build the basic (pseudo)spiral order */

  /* initialization */

  for(i = 0; i != DFA_MAX_BOARD * 4; i++)
    for(j = 0; j != DFA_MAX_BOARD * 4; j++)
      {
	for(ll = 0; ll != 8; ll++)
	  reverse_spiral[ll][i][j] = MAX_ORDER;
	Mark[i][j] = 1;
      }

  for (i = DFA_MAX_BOARD; i != DFA_MAX_BOARD * 3; i++)
    for (j = DFA_MAX_BOARD; j != DFA_MAX_BOARD * 3; j++)
      Mark[i][j] = 0;

  end = 0;
  top = 1;
  fifo[end].i = DFA_MAX_BOARD * 2;
  fifo[end].j = DFA_MAX_BOARD * 2;
  Mark[fifo[end].i][fifo[end].j] = 1;

  /* generation */
  while (end < MAX_ORDER)
    {
      i = fifo[end].i;
      j = fifo[end].j;
      order[0][end].i = i - DFA_MAX_BOARD * 2;
      order[0][end].j = j - DFA_MAX_BOARD * 2;
      reverse_spiral[0][i][j] = end;
      end++;

      for (k = 0; k != 4; k++)
	{
	  di = generator[k].i;
	  dj = generator[k].j;

	  if (!Mark[i + di][j + dj])
	    {
	      fifo[top].i = i + di;
	      fifo[top].j = j + dj;
	      Mark[i + di][j + dj] = 1;
	      top++;
	    }
	}
    }

  /* Then we compute all the geometric transformations
     on this order */
  for (ll = 1; ll != 8; ll++)
    for (k = 0; k != MAX_ORDER; k++)
      {
	delta = DFA_MAX_BOARD * 2;
	TRANSFORM (order[0][k].i, order[0][k].j,
		   &(order[ll][k].i), &(order[ll][k].j), ll);
	reverse_spiral[ll][order[ll][k].i + delta][order[ll][k].j + delta] = k;
      }

}

/********************************
 * manipulating attributes list *
 ********************************/

/*
 * Test if val is member of the attributes set att
 */

static int
member_att (dfa_t * pdfa, int att, int val)
{
  int res;

  res = 0;
  while (!res && att != 0)
    {
      res = (pdfa->indexes[att].val == val);
      att = pdfa->indexes[att].next;
    }
  return res;
}

/* 
 * return the union of two attribute sets att1 & att2
 * repectively from dfa1 and dfa2 into
 * att in dfa.
 */

static int
union_att (dfa_t * pdfa, dfa_t * pdfa1, int att1, dfa_t * pdfa2,
	   int att2)
{
  int att;
  int att_aux;

  /* copy att1 in att */
  att = 0;
  while (att1 != 0)
    {
      pdfa->lastIndex++;
      if (pdfa->lastIndex >= pdfa->maxIndexes)
	resize_dfa (pdfa, pdfa->maxStates,
		    pdfa->maxIndexes + DFA_RESIZE_STEP);
      att_aux = pdfa->lastIndex;

      pdfa->indexes[att_aux].val = pdfa1->indexes[att1].val;
      pdfa->indexes[att_aux].next = att;
      att = att_aux;
      att1 = pdfa1->indexes[att1].next;
    }

  /* add to att the new elements of att2 */
  while (att2 != 0)
    {
      if (!member_att (pdfa, att, pdfa2->indexes[att2].val))
	{
	  pdfa->lastIndex++;
	  if (pdfa->lastIndex >= pdfa->maxIndexes)
	    resize_dfa (pdfa, pdfa->maxStates,
			pdfa->maxIndexes + DFA_RESIZE_STEP);
	  att_aux = pdfa->lastIndex;

	  pdfa->indexes[att_aux].val = pdfa2->indexes[att2].val;
	  pdfa->indexes[att_aux].next = att;
	  att = att_aux;
	}
      att2 = pdfa2->indexes[att2].next;
    }

  return att;
}

/**********************
 * manipulating dfa's *
 **********************/


/*
 * return the effective size of a dfa in Kb.
 */

int
dfa_size (dfa_t * pdfa)
{
  int states_size, indexes_size;

  states_size = (pdfa->lastState + 1) * sizeof (state_t);
  indexes_size = (pdfa->lastIndex + 1) * sizeof (attrib_t);

  return (states_size + indexes_size + sizeof (dfa_t)) / 1024;
}


/* 
 * resize memory for a dfa 
 */

static void
resize_dfa (dfa_t * pdfa, int maxStates, int maxIndexes)
{
  state_t *pBuf;
  attrib_t *pBuf2;
  int i;

  if (dfa_verbose > 1)
    fprintf (stderr, "Resizing dfa %s\n", pdfa->name);

  gg_assert (pdfa->lastState <= pdfa->maxStates);
  gg_assert (pdfa->lastIndex <= pdfa->maxIndexes);

  pBuf = realloc (pdfa->states, maxStates * sizeof (state_t));
  pBuf2 = realloc (pdfa->indexes, maxIndexes * sizeof (attrib_t));
  if (pBuf == NULL || pBuf2 == NULL)
    {
      fprintf (stderr, "No memory left for dfa: %s", pdfa->name);
      exit (1);
    }

  for (i = pdfa->maxStates; i < maxStates; i++)
    memset (pBuf + i, 0, sizeof (state_t));
  for (i = pdfa->maxIndexes; i < maxIndexes; i++)
    memset (pBuf2 + i, 0, sizeof (attrib_t));

  pdfa->states = pBuf;
  pdfa->maxStates = maxStates;
  pdfa->indexes = pBuf2;
  pdfa->maxIndexes = maxIndexes;

}



/* 
 * dump a dfa (debugging purpose).
 */

static const char *line =
  "----------------------------------------------------\n";

void
dump_dfa (FILE * f, dfa_t * pdfa)
{
  int i;
  int att, k;

  fprintf (f, line);
  fprintf (f, " name : %s\n", pdfa->name);
  fprintf (f, " Nb states :  %7d, max= %d\n", pdfa->lastState + 1,
	   pdfa->maxStates);
  fprintf (f, " Nb Indexes : %7d, max= %d\n", pdfa->lastIndex,
	   pdfa->maxIndexes);
  fprintf (f, " memory needed : %d Mb\n", dfa_size (pdfa) / 1024);
  fprintf (f, line);

  if (dfa_size (pdfa) > 10000) /* change this value if needed */
    return;
  fprintf (f, " state  |   .    |   O    |   X    |   #    |  att \n");
  fprintf (f, line);
  for (i = 1; i != pdfa->lastState + 1; i++)
    {
      int *pnext = pdfa->states[i].next;
      fprintf (f, " %6d |", i);
      fprintf (f, " %6d | %6d | %6d |", pnext[0], pnext[1], pnext[2]);
      fprintf (f, " %6d |", pnext[OUT_BOARD]);
      att = pdfa->states[i].att;
      k = 0;
      fprintf (f, " %5d:", att);
      while (att != 0 && k < 10)
	{
	  fprintf (f, " %4d", pdfa->indexes[att].val);
	  att = pdfa->indexes[att].next;
	  k++;
	}
      if (att != 0)
	fprintf (f, " ...");
      fprintf (f, "\n");
    }
  fprintf (f, line);
  fflush (f);
}


/*
 * Reset a dfa
 */

static void
clean_dfa (dfa_t * pdfa)
{
  memset (pdfa->states, 0, pdfa->maxStates * sizeof (state_t));
  memset (pdfa->indexes, 0, pdfa->maxIndexes * sizeof (attrib_t));
  pdfa->lastState = 1;		/* initial state */
  pdfa->lastIndex = 0;
  pdfa->indexes[0].val = -1;
}


/* 
 * allocate memory for a new dfa 
 */

void
new_dfa (dfa_t * pdfa, const char *name)
{
  memset (pdfa, 0, sizeof (dfa_t));
  resize_dfa (pdfa, DFA_INIT_SIZE, DFA_INIT_SIZE);
  clean_dfa (pdfa);
  if (name != NULL)
    strcpy (pdfa->name, name);
  else
    strcpy (pdfa->name, "noname ");

  if (dfa_verbose > 1)
    fprintf (stderr, "dfa %s is born :)\n", pdfa->name);

}

/*
 * free memory used by a dfa
 */

void
kill_dfa (dfa_t * pdfa)
{
  free (pdfa->states);
  free (pdfa->indexes);
  if (dfa_verbose > 1)
    fprintf (stderr, "dfa %s is dead :(\n", pdfa->name);

  memset (pdfa, 0, sizeof (dfa_t));
}


/*
 * Copy a dfa.
 * and rezise the destination dfa if necessary.
 */

void
copy_dfa (dfa_t * p_to, dfa_t * p_from)
{
  gg_assert (p_to != p_from);

  if (p_to->maxStates < p_from->lastState)
    resize_dfa (p_to, p_from->maxStates, p_to->maxIndexes);

  if (p_to->maxIndexes < p_from->lastIndex)
    resize_dfa (p_to, p_to->maxStates, p_from->maxIndexes);

  clean_dfa (p_to);

  memcpy (p_to->states, p_from->states,
	  sizeof (state_t) * (p_from->lastState + 1));
  memcpy (p_to->indexes, p_from->indexes,
	  sizeof (attrib_t) * (p_from->lastIndex + 1));

  p_to->lastState = p_from->lastState;
  p_to->lastIndex = p_from->lastIndex;
}






/*
 * print c dfa:
 * print the dfa in c format.
 */

void
print_c_dfa (FILE *of, const char *name, dfa_t * pdfa)
{
  int i;

  fprintf(of,"\n#include \"dfa.h\"\n");

  fprintf(of,"static state_t state_%s[%d] = {\n",name,pdfa->lastState + 1);
  for (i = 0; i != pdfa->lastState + 1; i++)
    {
      fprintf(of,"{%d,",pdfa->states[i].att);
      fprintf(of,"{%d,",pdfa->states[i].next[0]);
      fprintf(of,"%d,",pdfa->states[i].next[1]);
      fprintf(of,"%d,",pdfa->states[i].next[2]);
      fprintf(of,"%d}},\n",pdfa->states[i].next[3]);
    }
  fprintf(of,"};\n\n");


  fprintf(of,"static attrib_t idx_%s[%d] = {\n",name,pdfa->lastIndex + 1);
  for (i = 0; i != pdfa->lastIndex + 1; i++)
    fprintf(of,"{%d,%d},\n",pdfa->indexes[i].val,pdfa->indexes[i].next);
  fprintf(of,"};\n\n");

  fprintf(of,"static struct dfa dfa_%s = {\n",name);
  fprintf(of," \"%s\",\n", name);
  fprintf(of,"state_%s, %d, %d,\n",name, pdfa->lastState + 1, pdfa->lastState);
  fprintf(of,"idx_%s, %d, %d",name, pdfa->lastIndex + 1, pdfa->lastIndex);
  fprintf(of,"};\n");

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
create_dfa (dfa_t * pdfa, const char *str, int att_val)
{
  int new_state;

  if (dfa_verbose > 1)
    fprintf (stderr, "linear dfa in %s with string\n%s\n", pdfa->name,
	     str);

  gg_assert (str != NULL);
  gg_assert (pdfa->maxStates > 1);
  gg_assert (pdfa->maxIndexes > 1);

  clean_dfa (pdfa);
  new_state = 1;
  for (; *str != '\0' && strchr ("$#+-|OoXx.?,!a*", *str); str++)
    {
      memset (pdfa->states[new_state].next, 0, 4 * sizeof (int));
      if (strchr ("$?.ox,a!*", *str))
	pdfa->states[new_state].next[0] = new_state + 1;
      if (strchr ("$?Oo", *str))
	pdfa->states[new_state].next[1] = new_state + 1;
      if (strchr ("$?Xx", *str))
	pdfa->states[new_state].next[2] = new_state + 1;
      if (strchr ("$#+-|", *str))
	pdfa->states[new_state].next[OUT_BOARD] = new_state + 1;
      new_state++;
      if (new_state >= pdfa->maxStates)
	resize_dfa (pdfa, pdfa->maxStates + DFA_RESIZE_STEP,
		    pdfa->maxIndexes);
    }
  memset (pdfa->states[new_state].next, 0, 4 * sizeof (int));

  pdfa->lastIndex++;
  if (pdfa->lastIndex >= pdfa->maxIndexes)
    resize_dfa (pdfa, pdfa->maxStates,
		pdfa->maxIndexes + DFA_RESIZE_STEP);

  memset (&(pdfa->indexes[pdfa->lastIndex]), 0, sizeof (attrib_t));
  pdfa->states[new_state].att = pdfa->lastIndex;

  pdfa->indexes[pdfa->states[new_state].att].val = att_val;
  pdfa->indexes[pdfa->states[new_state].att].next = 0;
  pdfa->lastState = new_state;
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
  int l,r; /* key */
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

/* Searh for (l,r) in the linked list plist */
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

/* get the value associated with (l,r) or 0 if none */
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
  gg_assert(val > 0);
  gg_assert(!get_from_entry_list(*pplist, l, r));

  new_entry = malloc(sizeof(entry_t));
  if (new_entry == NULL) {
    fprintf (stderr, "No memory left for new entry\n");
    exit (1);
  }
  new_entry->pnext = *pplist;
  new_entry->l = l;
  new_entry->r = r;
  new_entry->val = val;
  *pplist = new_entry;
}


/* add a value at (l,r) */
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

  for (h=0; h!=MAX_HASH_VALUE; h++) {
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
do_sync_product (int l, int r)
{
  int c;
  int nextl, nextr;
  int state;

  state = gpout->lastState;

  /* unify the attributes of states l and r */
  gpout->states[state].att =
    union_att (gpout, gpleft, gpleft->states[l].att,
	       gpright, gpright->states[r].att);

  /* scan each possible out-transition */
  for (c = 0; c != 4; c++)
    {
      nextl = gpleft->states[l].next[c];
      nextr = gpright->states[r].next[c];
      gg_assert (nextl < gpleft->lastState + 1);
      gg_assert (nextr < gpright->lastState + 1);

      /* transition to (0,0) mean no transition at all */
      if (nextl != 0 || nextr != 0)
	{
	  /* if the out-state doesn't already exist */
	  if (get_from_test_array(&gtest, nextl, nextr) == 0)
	    {
	      /* create it! */
	      gpout->lastState++;
	      if (gpout->lastState >= gpout->maxStates)
		resize_dfa (gpout, gpout->maxStates + DFA_RESIZE_STEP,
			    gpout->maxIndexes);

	      add_to_test_array(&gtest, nextl, nextr, gpout->lastState);

	      /* link it */
	      gpout->states[state].next[c] = gpout->lastState;

	      /* create also its sub-automaton */
	      do_sync_product (nextl, nextr);
	    }
	  else
	    {
	      /* link it */
	      gpout->states[state].next[c] =
		get_from_test_array(&gtest, nextl, nextr);
	    }
	}
      else
	{
	  /* no output by c from the actual state */
	  gpout->states[state].next[c] = 0;
	}
    }
}

static void
sync_product (dfa_t * pout, dfa_t * pleft, dfa_t * pright)
{
  pout->lastIndex = 0;

  if (dfa_verbose > 2)
    {
      fprintf (stderr, "Product between %s and %s\n", pleft->name,
	       pright->name);
      fprintf (stderr, "result in %s\n", pout->name);
    }


  gpout = pout;
  gpleft = pleft;
  gpright = pright;
  new_test_array(&gtest);
  add_to_test_array(&gtest, 1, 1, 1);
  pout->lastState = 1;

  do_sync_product (1, 1);

  free_test_array(&gtest);
}

/* 
 * Init/end functions
 */

void
dfa_init (void)
{
  int i,j;

  if (dfa_verbose > 1)
    fprintf (stderr, "dfa: init\n");
  dfa_was_initialized++;
  buildSpiralOrder (spiral);
  new_dfa (&aux_dfa1, "stringAux ");
  new_dfa (&aux_dfa2, "copyAux ");

  /* set the private board to OUT_BOARD */
  for (i =0; i!= DFA_MAX_BOARD*4; i++)
    for (j =0; j!= DFA_MAX_BOARD*4; j++)
      dfa_p[i][j] = OUT_BOARD;
}

void
dfa_end (void)
{
  if (dfa_verbose > 1)
    fprintf (stderr, "dfa: end\n");

  kill_dfa (&aux_dfa1);
  kill_dfa (&aux_dfa2);
  dfa_was_initialized--;
}


/*
 * Add a new string with attribute att_val into the dfa.
 * if the new size of the dfa respect some size conditions
 * return increase in Kb or -1 if the pattern was rejected.
 * This function never rejects string of length <= 1.
 */

float
dfa_add_string (dfa_t * pdfa, const char *str, int pattern_index)
{
  float ratio;

  if (dfa_verbose > 1)
    fprintf (stderr, "Adding %s\n to dfa %s\n", str, pdfa->name);

  gg_assert (dfa_was_initialized > 0);
  gg_assert (pdfa != NULL);

  /* We first create a dfa in aux_dfa1 from the string. */
  create_dfa (&aux_dfa1, str, pattern_index);

  /* then we do the synchronization product with dfa */
  sync_product (&aux_dfa2, &aux_dfa1, pdfa);

  ratio = 1;
  if (dfa_size(pdfa) > 0)
    ratio = (float)dfa_size(&aux_dfa2) / (float)dfa_size(pdfa);

  /* the result is copied in dfa */
  copy_dfa (pdfa, &aux_dfa2);

  return ratio;
}


/*
 * Build a pattern string from a pattern.
 * str must refer a buffer of size greater than MAX_ORDER.
 */
void
pattern_2_string (struct pattern *pat, char *str, int trans, int ci,
		  int cj)
{
  char work_space[DFA_MAX_BOARD * 4][DFA_MAX_BOARD * 4];
  int m, n;			/* anchor position */
  int edges, borders, to_test;
  int i, j, k;
  char c;

  m = DFA_MAX_BOARD * 2 + ci;
  n = DFA_MAX_BOARD * 2 + cj;	/* position of the anchor */

  gg_assert (dfa_was_initialized);
  memset (str, 0, MAX_ORDER);
  memset (work_space, '#', sizeof (work_space));

  if (dfa_verbose > 0)
    fprintf (stderr, "converting pattern into string.\n");

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
  if (pat->edge_constraints & SOUTH_EDGE)
    {
      for (i = m + pat->maxi + 1; i != DFA_MAX_BOARD * 3; i++)
	for (j = 0; j != DFA_MAX_BOARD * 3; j++)
	  work_space[i][j] = '-';
    }

  /* East constraint */
  if (pat->edge_constraints & EAST_EDGE)
    {
      for (i = 0; i != DFA_MAX_BOARD * 3; i++)
	for (j = n + pat->maxj + 1; j != DFA_MAX_BOARD * 3; j++)
	  work_space[i][j] = '|';
    }

  /* North constraint */
  if (pat->edge_constraints & NORTH_EDGE)
    {
      for (i = 0; i != m + pat->mini; i++)
	for (j = 0; j != DFA_MAX_BOARD * 4; j++)
	  work_space[i][j] = '-';
    }

  /* West constraint */
  if (pat->edge_constraints & WEST_EDGE)
    {
      /* take care not to erase the south edge constraint */
      for (i = 0; i != m + pat->maxi + 1; i++)
	for (j = 0; j != n + pat->minj; j++)
	  work_space[i][j] = '|';

      /* complete the last corner only if necessary */
      if (!(pat->edge_constraints & SOUTH_EDGE))
	{
	  for (i = m + pat->maxi + 1; i != DFA_MAX_BOARD * 3; i++)
	    for (j = 0; j != n + pat->minj; j++)
	      work_space[i][j] = '|';
	}

    }

  /* dump */
  if (dfa_verbose > 4)
    {
      for (i = DFA_MAX_BOARD - 1; i != DFA_MAX_BOARD * 3 + 1; i++)
	{
	  for (j = DFA_MAX_BOARD - 1; j != DFA_MAX_BOARD * 3 + 1; j++)
	    {
	      if (i == m && j == n)
		fprintf (stderr, "s");	/* mark the anchor */
	      else
		fprintf (stderr, "%c", work_space[i][j]);
	    }
	  fprintf (stderr, "\n");
	}
      fprintf (stderr, "\n");
    }

  /* pattern representation on the work space */
  for (k = 0; k != pat->patlen; k++)
    {
      c = EXPECTED_VAL (pat->patn[k].att);
      gg_assert (work_space[m + pat->patn[k].x - ci]
	      [n + pat->patn[k].y - cj] == '?');
      work_space[m + pat->patn[k].x - ci][n + pat->patn[k].y - cj] = c;
    }

  /* dump */
  if (dfa_verbose > 3)
    {
      for (i = DFA_MAX_BOARD - 1; i != DFA_MAX_BOARD * 3 + 1; i++)
	{
	  for (j = DFA_MAX_BOARD - 1; j != DFA_MAX_BOARD * 3 + 1; j++)
	    {
	      if (i == m && j == n)
		fprintf (stderr, "s");	/* mark the anchor */
	      else
		fprintf (stderr, "%c", work_space[i][j]);
	    }
	  fprintf (stderr, "\n");
	}
      fprintf (stderr, "\n");
    }

  /* Now we can build the smaller pattern string as possible 
   * from the anchor */

  to_test = pat->patlen;	/* How many positions left to test ? */
  edges = pat->edge_constraints;	/* how many constraint tested ? */
  borders = 0xF; 
  /* we must test at least one intersection by border for 
     patterns like
     
     ???
     O.O
     ???
     
     To ensure edge position.
  */

  for (k = 0; (k != MAX_ORDER - 1) && 
	 ((borders > 0) || edges || to_test > 0); k++)
    {
      i = spiral[trans][k].i;
      j = spiral[trans][k].j;

      if (i == pat->maxi)
	borders &= ~SOUTH_EDGE;
      if (i == pat->mini)
	borders &= ~NORTH_EDGE;
      if (j == pat->maxj)
	borders &= ~EAST_EDGE;
      if (j == pat->minj)
	borders &= ~WEST_EDGE;

      gg_assert (m + i < DFA_MAX_BOARD * 3 && m + i < DFA_MAX_BOARD * 3);
      str[k] = work_space[m + i][n + j];
      gg_assert (strchr ("XOxo.,a!?$#|-+", str[k]));

      if (strchr ("XOxo.,a!", str[k]))
	to_test--;
      if (strchr ("#|-+", str[k]))
	{
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

  gg_assert (k < MAX_ORDER);
  str[k] = '\0';		/* end of string */
}









