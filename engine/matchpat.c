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
#include <stdlib.h>
#include <string.h>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "liberty.h"
#include "patterns.h"
#include "dfa.h"


/**************************************************************************/
/* Pattern profiling functions:                                           */
/**************************************************************************/

/* define this to see how each phase of pattern rejection is performing */
/* #define PROFILE_MATCHER */


#ifdef PROFILE_MATCHER
static int totals[6];
#endif


#if PROFILE_PATTERNS
/* Initialize pattern profiling fields in one pattern struct array. */
static void
clear_profile(struct pattern *pattern)
{

  for (; pattern->patn; ++pattern) {
    pattern->hits = 0;
    pattern->reading_nodes = 0;
#if DFA_ENABLED
    pattern->dfa_hits = 0;
#endif
  }
}
#endif

#if PROFILE_PATTERNS
/* Print profiling information for one pattern struct array. */
static void
print_profile(struct pattern *pattern, int *total_hits,
			   int *total_nodes, int *total_dfa_hits)
{
  for (; pattern->patn; ++pattern)
    if (pattern->hits > 0) {
      *total_hits += pattern->hits;
      *total_nodes += pattern->reading_nodes;
#if DFA_ENABLED 
      *total_dfa_hits += pattern->dfa_hits;
      fprintf(stderr, "%6d ", pattern->dfa_hits);
#else
      *total_dfa_hits = 0;
#endif
      fprintf(stderr, "%6d %9d %8.1f %s\n", 
	      pattern->hits,
	      pattern->reading_nodes,
	      pattern->reading_nodes / (float) pattern->hits, 
	      pattern->name);
    }
}
#endif /* PROFILE_PATTERNS */


/* Initialize pattern profiling fields in pattern struct arrays. */
void
prepare_pattern_profiling()
{
#if PROFILE_PATTERNS
  clear_profile(pat_db.patterns);
  clear_profile(attpat_db.patterns);
  clear_profile(defpat_db.patterns);
  clear_profile(endpat_db.patterns);
  clear_profile(conn_db.patterns);
  clear_profile(influencepat_db.patterns);
  clear_profile(barrierspat_db.patterns);
  clear_profile(owl_attackpat_db.patterns);
  clear_profile(owl_vital_apat_db.patterns);
  clear_profile(owl_defendpat_db.patterns);
#else
  fprintf(stderr,
	  "Warning, no support for pattern profiling in this binary.\n");
#endif
}


/* Report result of pattern profiling. Only patterns with at least one
 * match are listed.
 */
void
report_pattern_profiling()
{
#if PROFILE_PATTERNS
  int hits = 0;
  int dfa_hits = 0;
  int nodes = 0;

  print_profile(pat_db.patterns, &hits, &nodes, &dfa_hits);
  print_profile(attpat_db.patterns, &hits, &nodes, &dfa_hits);
  print_profile(defpat_db.patterns, &hits, &nodes, &dfa_hits);
  print_profile(endpat_db.patterns, &hits, &nodes, &dfa_hits);
  print_profile(conn_db.patterns, &hits, &nodes, &dfa_hits);
  print_profile(influencepat_db.patterns, &hits, &nodes, &dfa_hits);
  print_profile(barrierspat_db.patterns, &hits, &nodes, &dfa_hits);
  print_profile(owl_attackpat_db.patterns, &hits, &nodes, &dfa_hits);
  print_profile(owl_vital_apat_db.patterns, &hits, &nodes, &dfa_hits);
  print_profile(owl_defendpat_db.patterns, &hits, &nodes, &dfa_hits);
  fprintf(stderr, "------ ---------\n");
  fprintf(stderr, "%6d, %6d %9d\n", dfa_hits, hits, nodes);
#endif
}








/**************************************************************************/
/* Standard matcher:                                                      */
/**************************************************************************/


/* Forward declarations. */

static void fixup_patterns_for_board_size(struct pattern *pattern);
static void do_matchpat(int m, int n, matchpat_callback_fn_ptr callback,
			int color, struct pattern *database,
			void *callback_data, char goal[MAX_BOARD][MAX_BOARD]);
void transform(int i, int j, int *ti, int *tj, int trans);
void offset(int i, int j, int basei, int basej, int *ti, int *tj, int trans);

/* Precomputed tables to allow rapid checks on the piece at
 * the board. This table relies on the fact that color is
 * 1 or 2.
 *
 * For pattern element i,  require  (p[m][n] & andmask[i]) == valmask[i]
 *
 * .XO) For i=0,1,2,  p[m][n] & 3 is a no-op, so we check p[][] == valmask
 * x)   For i=3, we are checking that p[][] is not color, so AND color and
 *      we get 0 for either empty or OTHER_COLOR, but color if it contains
 *      color
 * o)   Works the other way round for checking it is not X.
 *
 *
 *  gcc allows the entries to be computed at run-time, but that is not ANSI.
 */
 
static const int and_mask[2][8] = {
  /*  .      X      O     x      o      ,      a      !         color */ 
  {   3,     3,     3,  WHITE, BLACK,   3,     3,     3   }, /* BLACK */
  {   3,     3,     3,  BLACK, WHITE,   3,     3,     3   }  /* WHITE */
};

static const int val_mask[2][8] = {
  { EMPTY, BLACK, WHITE,  0,     0,   EMPTY, EMPTY, EMPTY},  /* BLACK */ 
  { EMPTY, WHITE, BLACK,  0,     0,   EMPTY, EMPTY, EMPTY}   /* WHITE */
};


/* and a table for checking classes quickly
 * class_mask[status][color] contains the mask to look for in class.
 * ie. if  pat[r].class & class_mask[dragon[x][y].status][p[x][y]]
 * is not zero then we reject it
 * Most elements if class_mask[] are zero - it is a sparse
 * matrix containing
 *  CLASS_O in [DEAD][color]
 *  CLASS_O in [CRITICAL][color]
 *  CLASS_o in [ALIVE][color]
 *  CLASS_X in [DEAD][other]
 *  CLASS_x in [ALIVE][other]
 *
 * so eg. if we have a dead white dragon, and we
 * are checking a pattern for black, then
 *  class_mask[DEAD][other]  will contain CLASS_X
 * Then we reject any patterns which have CLASS_X
 * set in the class bits.
 *
 * Making it static guarantees that all fields are
 * initially set to 0, and we overwrite the ones
 * we care about each time.
 */
  
static int class_mask[MAX_DRAGON_STATUS][3];


/* In the current implementation, the edge constraints depend on
 * the board size, because we pad width or height out to the
 * board size. (This is because it is easy to find the corners
 * of the rotated pattern, but it is harder to transform the
 * bitmask of edge constraints.)
 *
 * But since version 1.103, board size is variable. Thus we
 * make a first pass through the table once we know the board
 * size.
 *
 * This should be called once for each pattern database.
 */

static void
fixup_patterns_for_board_size(struct pattern *pattern)
{

  for ( ; pattern->patn; ++pattern )
    if (pattern->edge_constraints != 0) {

      /* If the patterns have been fixed up for a different board size
       * earlier, we need to undo the modifications that were done
       * below before we do them anew. The first time this function is
       * called, this step is effectively a no-op.
       */
      
      if (pattern->edge_constraints & NORTH)
	pattern->maxi = pattern->mini + pattern->height;
	
      if (pattern->edge_constraints & SOUTH)
	pattern->mini = pattern->maxi - pattern->height;
	
      if (pattern->edge_constraints & WEST)
	pattern->maxj = pattern->minj + pattern->width;
      
      if (pattern->edge_constraints & EAST)
	pattern->minj = pattern->maxj - pattern->width;
      
      /* we extend the pattern in the direction opposite the constraint,
       * such that maxi (+ve) - mini (-ve) = board_size-1
       * Note : the pattern may be wider than the board, so
       * we need to be a bit careful !
       */
      
      if (pattern->edge_constraints & NORTH)
	if (pattern->maxi < (board_size-1) + pattern->mini)
	  pattern->maxi = (board_size-1) + pattern->mini;
      
      if (pattern->edge_constraints & SOUTH)
	if (pattern->mini > pattern->maxi - (board_size-1))
	  pattern->mini = pattern->maxi - (board_size-1);
      
      if (pattern->edge_constraints & WEST)
	if (pattern->maxj <  (board_size-1) + pattern->minj)
	  pattern->maxj = (board_size-1) + pattern->minj;
      
      if (pattern->edge_constraints & EAST)
	if (pattern->minj > pattern->maxj - (board_size-1))
	  pattern->minj = pattern->maxj - (board_size-1);
    }

}

/* Compute the transform of (i,j) under transformation number trans.
 * *ti and *tj point to the transformed coordinates.
 * ORDER MATTERS : see texinfo documentation for details
 *
 * There is a copy of this table in mkpat.c
 */

const int transformations[8][2][2] = {
  {{ 1,  0}, { 0,  1}}, /* a - identity transformation matrix */
  {{ 0,  1}, {-1,  0}}, /* g - rotate 270 counter-clockwise */
  {{-1,  0}, { 0, -1}}, /* d - rotate 180 */
  {{ 0, -1}, { 1,  0}}, /* f - rotate 90 counter-clockwise */
  {{ 0, -1}, {-1,  0}}, /* h - rotate 90 and invert */
  {{-1,  0}, { 0,  1}}, /* b - flip left */
  {{ 0,  1}, { 1,  0}}, /* e - rotate 90 and flip left */
  {{ 1,  0}, { 0, -1}}  /* c - invert */
};


/* Functional version for completeness. Prefer the TRANSFORM macro
 * in patterns.h.
 */

void 
transform(int i, int j, int *ti, int *tj, int trans)
{
  TRANSFORM(i, j, ti, tj, trans);
}


/* Compute the point offset by (i,j), relative to a base point (basei,basej), 
 * taking into account a transformation.
 */

void 
offset(int i, int j, int basei, int basej, int *ti, int *tj, int trans)
{
  int ui, uj;
  TRANSFORM(i, j, &ui, &uj, trans);
  (*ti) = basei + ui;
  (*tj) = basej + uj;
}


/*
 * Try all the patterns in the given array at (m,n). Invoke the
 * callback for any that matches. Classes X,O,x,o are checked here. It
 * is up to the callback to process the other classes, and any helper
 * or autohelper functions.
 *
 * If the support of goal[MAX_BOARD][MAX_BOARD] is a subset of the board,
 * patterns are rejected which do not involve this dragon. If goal is a 
 * null pointer, this parameter is ignored.
 */

static void
do_matchpat(int m, int n, matchpat_callback_fn_ptr callback, int color,
	    struct pattern *pattern, void *callback_data,
	    char goal[MAX_BOARD][MAX_BOARD]) 
{
  const int other = OTHER_COLOR(color);
  const int anchor_test = p[m][n] ^ color;  /* see below */
  int merged_val;

  /* If we set one of class_mask[XXX][color] and
   * class_mask[XXX][other], we have to explicitly set or reset the
   * other as well, since 'color' may change between calls. Since
   * class_mask is static, it is only initialized when the program is
   * first started.
   */
  class_mask[DEAD][color] = CLASS_O;
  class_mask[DEAD][other] = CLASS_X;
  class_mask[CRITICAL][color] = CLASS_O;
  class_mask[CRITICAL][other] = 0;       /* Need to reset this. */
  class_mask[ALIVE][color] = CLASS_o;
  class_mask[ALIVE][other] = CLASS_x;

  /* Basic sanity checks. */
  gg_assert(color != EMPTY);
  ASSERT_ON_BOARD(m, n);

  /* calculate the merged value around [m][n] for the grid opt */
  {
    int i,j;
    int shift = 30;

    merged_val = 0;
    for (i = m-1; i <= m+2; ++i)
      for (j = n-1; j <= n+2; shift -= 2, ++j) {
	uint32 this;
	if (!ON_BOARD(i, j))
	  this = 3;
	else if ((this=p[i][j]) == 0)
	  continue;
	else if (color == 2)
	  this = OTHER_COLOR(this);
	merged_val |= (this << shift);
      }
  }

  /* Try each pattern - NULL pattern marks end of list. Assume at least 1 */
  gg_assert(pattern->patn);

  do {
    /*
     * These days we always match all patterns.
     */
    {
      int end_transformation;
      int ll;   /* Iterate over transformations (rotations or reflections)  */
      int k;    /* Iterate over elements of pattern */
      int found_goal, found_nongoal;
  
      /* We can check the color of the anchor stone now.
       * Roughly half the patterns are anchored at each
       * color, and since the anchor stone is invariant under
       * rotation, we can reject all rotations of a wrongly-anchored
       * pattern in one go.
       *
       * Patterns are always drawn from O perspective in .db,
       * so p[m][n] is 'color' if the pattern is anchored
       * at O, or 'other' for X.
       * Since we require that this flag contains 3 for
       * anchored_at_X, we can check that
       *   p[m][n] == (color ^ anchored_at_X)
       * which is equivalent to
       *   (p[m][n] ^ color) == anchored_at_X)
       * and the LHS is something we precomputed.
       */

      if (anchor_test != pattern->anchored_at_X)
      {
#ifdef PROFILE_MATCHER
	/* oops - need to work out something we deferred */
        totals[0] += (pattern->trfno == 5 ? 4 : pattern->trfno);
#endif
	continue;  /* does not match the anchor */
      }

      ll = 0;  /* first transformation number */
      end_transformation = pattern->trfno;

      /* Ugly trick for dealing with 'O' symmetry. */
      if (pattern->trfno == 5) {
	ll = 2;
	end_transformation = 6;
      }
      
#ifdef PROFILE_MATCHER
      totals[0] += end_transformation - ll;
#endif

      /* try each orientation transformation. Assume at least 1 */

      do {

#if PROFILE_PATTERNS
	int nodes_before;
#endif
	
#ifdef PROFILE_MATCHER
	++totals[1];
#endif

#if GRID_OPT == 1

	/* We first perform the grid check : this checks up to 16
	 * elements in one go, and allows us to rapidly reject
	 * patterns which do not match.  While this check invokes a
	 * necessary condition, it is not a sufficient test, so more
	 * careful checks are still required, but this allows rapid
	 * rejection. merged_val should contain a combination of
	 * 16 board positions around m,n.  The colours have been fixed
	 * up so that stones which are 'O' in the pattern are
	 * bit-pattern %01.
	 */
	if ((merged_val & pattern->and_mask[ll]) != pattern->val_mask[ll])
	  continue;  /* large-scale match failed */

#endif /* GRID_OPT == 1 */

#ifdef PROFILE_MATCHER
	++totals[2];
#endif
	/* Next, we do the range check. This applies the edge
	 * constraints implicitly.
	 */
	{
	  int mi, mj, xi, xj;
	  
	  TRANSFORM(pattern->mini, pattern->minj, &mi, &mj, ll);
	  TRANSFORM(pattern->maxi, pattern->maxj, &xi, &xj, ll);

	  /* {min,max}{i,j} are the appropriate corners of the original
	   * pattern, Once we transform, {m,x}{i,j} are still corners,
	   * but we don't know *which* corners.
	   * We could sort them, but it turns out to be cheaper
	   * to just test enough cases to be safe.
	   */

	  DEBUG(DEBUG_MATCHER, "---\nconsidering pattern '%s', rotation %d at %m. Range %d,%d -> %d,%d\n",
		pattern->name, ll, m, n, mi, mj, xi, xj);

	  /* now do the range-check */
	  if (!ON_BOARD(m+mi, n+mj) || !ON_BOARD(m+xi, n+xj))
	    continue;  /* out of range */
	}

#ifdef PROFILE_MATCHER	 
	++totals[3];
#endif

	/* Now iterate over the elements of the pattern. */
	found_goal = 0;
	found_nongoal = 0;
	for (k = 0; k < pattern->patlen; ++k) { /* match each point */
	  int x, y; /* absolute coords of (transformed) pattern element */

	  int att = pattern->patn[k].att;  /* what we are looking for */


	  /* Work out the position on the board of this pattern element. */

	  /* transform pattern real coordinate... */
	  TRANSFORM(pattern->patn[k].x, pattern->patn[k].y, &x, &y, ll);
	  x += m;
	  y += n;

	  ASSERT_ON_BOARD(x, y);

	  /* ...and check that p[x][y] matches (see above). */
	  if ((p[x][y] & and_mask[color-1][att]) != val_mask[color-1][att])
	    goto match_failed;

	  if (goal != NULL) {
	    if (goal[x][y])
	      found_goal = 1;
	    else if (p[x][y] == color)
	      found_nongoal = 1;
	  }
	  
	  /* Check out the class_X, class_O, class_x, class_o
	   * attributes - see patterns.db and above.
	   */
	  if ((pattern->class
	       & class_mask[dragon[x][y].matcher_status][p[x][y]]) != 0)
	    goto match_failed; 
	  
	} /* loop over elements */


#if GRID_OPT == 2

	/* Make sure the grid optimisation wouldn't have 
           rejected this pattern */
	ASSERT((merged_val & pattern->and_mask[ll])
	       == pattern->val_mask[ll], m, n);

#endif /* we don't trust the grid optimisation */


#ifdef PROFILE_MATCHER
	++totals[4];
#endif


	/* Make it here ==> We have matched all the elements to the board. */
	if ((goal != NULL) && !found_goal)
	  goto match_failed;
	if ((goal != NULL) && ((pattern->class) & CLASS_C) && !found_nongoal)
	  goto match_failed;
	


#ifdef PROFILE_MATCHER
	++totals[5];
#endif

#if PROFILE_PATTERNS
	pattern->hits++;
	nodes_before = stats.nodes;
#endif
	
	/* A match!  - Call back to the invoker to let it know. */
	callback(m, n, color, pattern, ll, callback_data);

#if PROFILE_PATTERNS
	pattern->reading_nodes += stats.nodes - nodes_before;
#endif
	
	/* We jump to here as soon as we discover a pattern has failed. */
      match_failed:
	DEBUG(DEBUG_MATCHER, 
	      "end of pattern '%s', rotation %d at %m\n---\n", 
	      pattern->name, ll, m, n);
	 
      } while (++ll < end_transformation ); /* ll loop over symmetries */
    } /* if not rejected by maxwt */
  } while ( (++pattern)->patn );  /* loop over patterns */


#ifdef PROFILE_MATCHER
  fprintf(stderr, 
	  "total %d, anchor=%d, grid=%d, edge=%d, matched=%d, accepted=%d\n",
	  totals[0], totals[1], totals[2], totals[3], totals[4], totals[5]);
#endif

}


/**************************************************************************/
/* DFA matcher:                                                           */
/**************************************************************************/

#if DFA_ENABLED
/* If DFA_SORT, all matched patterns are sorted and checked 
 * in the same order as the standard scheme */
#define DFA_SORT 1

/* Set this to show the dfa board in action */
/* #define DFA_TRACE 1 */

/* data */
extern int board_size;
static int dfa_board_size = -1;
extern int dfa_p[DFA_MAX_BOARD * 4][DFA_MAX_BOARD * 4];
extern order_t spiral[8][MAX_ORDER];
const int convert[3][4];

/* Forward declarations. */
void        dfa_match_init(void);
static void dfa_compile_for_match (int color);
static int  read_board (int ll, int m, int n, int row);
static int  scan_for_patterns(dfa_t *pdfa, int l, int m, int n, 
			      int *pat_list);
#if DFA_SORT
static int  compare_int (const void *a, const void *b);
#endif
static void do_dfa_matchpat(dfa_t *pdfa,
		int m, int n, matchpat_callback_fn_ptr callback,
		int color, struct pattern *database,
		void *callback_data, char goal[MAX_BOARD][MAX_BOARD]);
static void check_pattern_light (int m, int n, 
		matchpat_callback_fn_ptr callback,
		int color, struct pattern *pattern, int ll,
		void *callback_data,
		char goal[MAX_BOARD][MAX_BOARD]);



/***********************************************************************/



/* prepare the dfa board */
static const char *s=" --> using dfa\n";

void
dfa_match_init(void)
{
  buildSpiralOrder (spiral);

  if (owl_vital_apat_db.pdfa != NULL && !quiet)
    fprintf(stderr,"owl_vital_apat %s",s);
  if (owl_attackpat_db.pdfa != NULL && !quiet)
    fprintf(stderr,"owl_attackpat %s",s);
  if (owl_defendpat_db.pdfa != NULL && !quiet)
    fprintf(stderr,"owl_defendpat %s",s);
  if (pat_db.pdfa != NULL && !quiet)
    fprintf(stderr,"pat %s",s);
  if (attpat_db.pdfa != NULL && !quiet)
    fprintf(stderr,"attpat %s",s);
  if (defpat_db.pdfa != NULL && !quiet)
    fprintf(stderr,"defpat %s",s);
  if (endpat_db.pdfa != NULL && !quiet)
    fprintf(stderr,"endpat %s",s);
  if (conn_db.pdfa != NULL && !quiet)
    fprintf(stderr,"conn %s",s);
  if (influencepat_db.pdfa != NULL && !quiet)
    fprintf(stderr,"influencepat %s",s);
  if (barrierspat_db.pdfa !=NULL && !quiet)
    fprintf(stderr,"barrierspat %s",s);

  /* force out_board initialization */
  dfa_board_size = -1;
}


/* 
 * copy the board on a private board with adapted colors 
 * and adapted size 
 */
static void
dfa_compile_for_match(int color)
{
  int i,j;
    
  if (dfa_board_size != board_size) {
    dfa_board_size = board_size;
    /* clean up the board */
    for(i=0; i!= DFA_MAX_BOARD*4; i++)
      for(j=0; j!= DFA_MAX_BOARD*4; j++)
	dfa_p[i][j]=OUT_BOARD;
  }

  /* copy the board */
  for (i = 0; i!= dfa_board_size; i++)
    for (j = 0; j!= dfa_board_size; j++)
      dfa_p[DFA_MAX_BOARD+i][DFA_MAX_BOARD+j] = 
	EXPECTED_COLOR(color, p[i][j]);
    
}


/*
 * A function to read the board's values folowing order
 * spiral[ll] from anchor (m,n).
 * 
 */
static int
read_board (int ll, int m, int n, int row)
{
  int i2, j2;

  i2 = DFA_MAX_BOARD + m + spiral[ll][row].i;
  j2 = DFA_MAX_BOARD + n + spiral[ll][row].j;

#ifdef DFA_TRACE
  gg_assert(row < MAX_ORDER);
  gg_assert(i2 < DFA_MAX_BOARD*4 && j2 < DFA_MAX_BOARD*4);
  gg_assert(i2 >= 0 && j2 >= 0);
  fprintf(stderr,"(%d,%d)=%c ",m,n,VAL2ASC(dfa_p[i2][j2]));
#endif

  return dfa_p[i2][j2];
}


#if 0
/* debug function */
static void
dump_dfa_board(int m, int n)
{
  int i,j;

  for (i =DFA_MAX_BOARD / 2; i < DFA_MAX_BOARD*1.5 ; i++) {
    for (j = DFA_MAX_BOARD / 2 ; j < DFA_MAX_BOARD*1.5 ; j++)
      if (i!=(m+DFA_MAX_BOARD) || j!=(n+DFA_MAX_BOARD))
	fprintf(stderr,"%1d",dfa_p[i][j]);
      else
	fprintf(stderr,"*");
    fprintf(stderr,"\n");
  }
}
#endif


/* 
 * Scan the board with a dfa to get 
 * all patterns matching at (m,n) with transformation l.
 * Store patterns indexes + transformation in pat_list.
 * Return the number of patterns found.
 */
static int
scan_for_patterns(dfa_t *pdfa, int l, int m, int n, int *pat_list)
{
  int state,att, id, row;

  state = 1; /* initial state */
  row = 0; /* initial row */
  id = 0; /* position in id_list */ 
  
  while(state != 0) /* while not on error state */
    {
      /* collect patterns indexes */
      att = pdfa->states[state].att;
      while(att != 0)
	{
	  pat_list[id] = l+8*pdfa->indexes[att].val;
	  id++;
	  att = pdfa->indexes[att].next;
	}
      
      /* go to next state */
      state = pdfa->states[state].next[read_board(l,m,n,row)];
      row++;
    }

  ASSERT(row < MAX_ORDER,m,n);
  return id;
}

#if DFA_SORT
/* used to sort patterns */
static int
compare_int (const void *a, const void *b)
{
  const int *da = (const int *) a;
  const int *db = (const int *) b;
     
  return (*da > *db) - (*da < *db);
}
#endif


/* perform pattern matching with dfa filtering */
static void 
do_dfa_matchpat(dfa_t *pdfa,
		int m, int n, matchpat_callback_fn_ptr callback,
		int color, struct pattern *database,
		void *callback_data, char goal[MAX_BOARD][MAX_BOARD])
{
  int other = OTHER_COLOR(color);
  int ll;      /* Iterate over transformations (rotations or reflections)  */
  int matched; /* index in database[] of the matched pattern */

  int reorder[DFA_MAX_MATCHED];
  int *preorder = reorder;
  int maxr = 0, k;

  class_mask[DEAD][color]     = CLASS_O;
  class_mask[DEAD][other]     = CLASS_X;
  class_mask[CRITICAL][color] = CLASS_O;
  class_mask[CRITICAL][other] = 0;       /* Need to reset this. */
  class_mask[ALIVE][color]    = CLASS_o;
  class_mask[ALIVE][other]    = CLASS_x;

  /* Basic sanity checks. */
  gg_assert(color != EMPTY);
  ASSERT_ON_BOARD(m, n);


  /* Geometrical pattern matching */

  dfa_compile_for_match(color); 
  maxr = 0;
  /* one scan by transformation */
  for(ll = 0; ll != 8; ll++)
    {
      maxr += scan_for_patterns(pdfa, ll, m, n, preorder);
      preorder = reorder + maxr;
    }

  ASSERT(maxr < DFA_MAX_MATCHED,m,n);

  /* Sorting patterns keep the same order as 
   * standard pattern matching algorithm */
#if DFA_SORT
  qsort(reorder,maxr,sizeof(int),compare_int);
#endif /* DFA_SORT */


  /* Constraints and other tests */

  for(k = 0; k != maxr ; k++)
    {
      matched = reorder[k]/8;
      ll = reorder[k]%8;

#if PROFILE_PATTERNS
      database[matched].dfa_hits++;
#endif

      check_pattern_light (m, n, callback, color, database+matched, 
			   ll, callback_data, goal);
    }
}


/*
 * Do the pattern matching for a given pattern and a given 
 * transformation ll. 
 * (does not recompute what dfa filtering has already done)
 */

static void
check_pattern_light(int m, int n, matchpat_callback_fn_ptr callback, int color,
	      struct pattern *pattern, int ll, void *callback_data,
	      char goal[MAX_BOARD][MAX_BOARD])
{
  int k;			/* Iterate over elements of pattern */
  int found_goal, found_nongoal;
  
#if PROFILE_PATTERNS
  int nodes_before;
#endif
  
#ifdef PROFILE_MATCHER
  ++totals[1];
#endif
 
  /* Now iterate over the elements of the pattern. */
  found_goal = 0;
  found_nongoal = 0;
  for (k = 0; k < pattern->patlen; ++k) {
  				/* match each point */
    int x, y;			/* absolute (board) co-ords of 
  				   (transformed) pattern element */

    /* transform pattern real coordinate... */
    TRANSFORM(pattern->patn[k].x, pattern->patn[k].y, &x, &y, ll);
    x += m;
    y += n;
    ASSERT_ON_BOARD(x, y);

    /* goal check */
    if (goal != NULL) {
      if (goal[x][y])
	found_goal = 1;
      else if (p[x][y] == color)
	found_nongoal = 1;
    }

   /* class check */
    if ((pattern->class
	 & class_mask[dragon[x][y].matcher_status][p[x][y]]) != 0)
      goto match_failed;
    
  }/* loop over elements */
  
  /* Make it here ==> We have matched all the elements to the board. */
  if ((goal != NULL) && !found_goal)
    goto match_failed;
  if ((goal != NULL) && ((pattern->class) & CLASS_C) && !found_nongoal)
    goto match_failed;

#ifdef PROFILE_MATCHER
  ++totals[4];
#endif

#if PROFILE_PATTERNS
  pattern->hits++;
  nodes_before = stats.nodes;
#endif
  
  /* A match!  - Call back to the invoker to let it know. */
  callback(m, n, color, pattern, ll, callback_data);
  
#if PROFILE_PATTERNS
  pattern->reading_nodes += stats.nodes - nodes_before;
#endif
  
  /* We jump to here as soon as we discover a pattern has failed. */
 match_failed:
  DEBUG (DEBUG_MATCHER,
	 "end of pattern '%s', rotation %d at %m\n---\n",
	 pattern->name, ll, m, n);
  
} /* check_pattern_light */


#endif /* DFA_ENABLED */







/**************************************************************************/
/* Main functions:                                                        */
/**************************************************************************/

/* Select which matcher (standard or dfa) will be used
 * and call the appropriate internal function.
 * Check also if the pattern database has been 
 * prepared for matching on a board of this size.
 */
void
matchpat(int m, int n, matchpat_callback_fn_ptr callback, int color,
	 struct pattern_db *pdb, void *callback_data,
	 char goal[MAX_BOARD][MAX_BOARD]) 
{

  if (pdb->fixed_for_size != board_size) {
    fixup_patterns_for_board_size(pdb->patterns);
    pdb->fixed_for_size = board_size;
  }

#if DFA_ENABLED > 0

  if (pdb->pdfa != NULL) {
    /* dfa matcher */
    
    do_dfa_matchpat(pdb->pdfa,m, n, callback, color, pdb->patterns, 
		    callback_data, goal);
    return;
  }
#endif
  
  /* standard matcher */
  do_matchpat(m, n, callback, color, pdb->patterns, callback_data, goal);
}


/* A dedicated matcher which can only do fullboard matching on
 * odd-sized boards, optimized for fuseki patterns.
 */
void
fullboard_matchpat(fullboard_matchpat_callback_fn_ptr callback, int color,
		   struct fullboard_pattern *pattern)
{
  int other = OTHER_COLOR(color);
  int ll;   /* Iterate over transformations (rotations or reflections)  */
  int k;    /* Iterate over elements of pattern */
  int mid = (board_size-1)/2; /* We transform around the center point. */
  int m, n;
  int number_of_stones_on_board = 0;
  
  /* Basic sanity check. */
  gg_assert(color != EMPTY);
  gg_assert(board_size%2 == 1);
  
  /* Count the number of stones on the board. */
  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++)
      if (p[m][n] != EMPTY)
	number_of_stones_on_board++;
  
  /* Try each pattern - NULL pattern marks end of list. */
  for ( ; pattern->patn; ++pattern) { 
    /* The number of stones on the board must be right. This is not
     * only an optimization because we never even look at the
     * intersections which are empty in the pattern.
     */
    if (pattern->patlen != number_of_stones_on_board)
      continue;
    
    /* try each orientation transformation */
    for (ll = 0; ll < 8; ll++) {
      /* Now iterate over the elements of the pattern. */
      for (k = 0; k < pattern->patlen; ++k) { /* match each point */
	int x, y; /* board co-ords of transformed pattern element */
	int att = pattern->patn[k].att;  /* what we are looking for */

	/* Work out the position on the board of this pattern element. */
	TRANSFORM(pattern->patn[k].x, pattern->patn[k].y, &x, &y, ll);
	x += mid;
	y += mid;

        ASSERT_ON_BOARD(x, y);

	if ((att == ATT_O && p[x][y] != color)
	    || (att == ATT_X && p[x][y] != other))
	  break;
	
      } /* loop over elements */
	
      if (k == pattern->patlen) {
	/* A match!  - Call back to the invoker to let it know. */
	int x, y;
	TRANSFORM(pattern->movei, pattern->movej, &x, &y, ll);
	x += mid;
	y += mid;
	callback(x, y, pattern, ll);
      }
    }
  }
}


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
