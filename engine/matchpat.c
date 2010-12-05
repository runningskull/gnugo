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

#include "gnugo.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "liberty.h"
#include "gg_utils.h"
#include "patterns.h"
#include "dfa.h"


/**************************************************************************/
/* Pattern profiling functions:                                           */
/**************************************************************************/


#if PROFILE_PATTERNS
/* Initialize pattern profiling fields in one pattern struct array. */
static void
clear_profile(struct pattern *pattern)
{
  for (; pattern->patn; ++pattern) {
    pattern->hits = 0;
    pattern->reading_nodes = 0;
    pattern->dfa_hits = 0;
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
      *total_dfa_hits += pattern->dfa_hits;
      fprintf(stderr, "%6d %6d %9d %8.1f %s\n",
	      pattern->dfa_hits,
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
  clear_profile(aa_attackpat_db.patterns);
  clear_profile(owl_attackpat_db.patterns);
  clear_profile(owl_vital_apat_db.patterns);
  clear_profile(owl_defendpat_db.patterns);
  clear_profile(fusekipat_db.patterns);
  clear_profile(oracle_db.patterns);
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
  print_profile(aa_attackpat_db.patterns, &hits, &nodes, &dfa_hits);
  print_profile(owl_attackpat_db.patterns, &hits, &nodes, &dfa_hits);
  print_profile(owl_vital_apat_db.patterns, &hits, &nodes, &dfa_hits);
  print_profile(owl_defendpat_db.patterns, &hits, &nodes, &dfa_hits);
  print_profile(fusekipat_db.patterns, &hits, &nodes, &dfa_hits);
  print_profile(oracle_db.patterns, &hits, &nodes, &dfa_hits);
  fprintf(stderr, "------ ---------\n");
  fprintf(stderr, "%6d, %6d %9d\n", dfa_hits, hits, nodes);
#endif
}



/**************************************************************************/
/* Standard matcher:                                                      */
/**************************************************************************/


/* Forward declarations. */

static void fixup_patterns_for_board_size(struct pattern *pattern);
static void prepare_for_match(int color);
static void do_matchpat(int anchor, matchpat_callback_fn_ptr callback,
			int color, struct pattern *database,
			void *callback_data, signed char goal[BOARDMAX]);
static void matchpat_loop(matchpat_callback_fn_ptr callback, 
			  int color, int anchor,
			  struct pattern_db *pdb, void *callback_data,
			  signed char goal[BOARDMAX], int anchor_in_goal);

/* Precomputed tables to allow rapid checks on the piece at
 * the board. This table relies on the fact that color is
 * 1 or 2.
 *
 * For pattern element i,  require  (board[pos] & andmask[i]) == valmask[i]
 *
 * .XO) For i=0,1,2, board[pos] & 3 is a no-op, so we check board[pos]
 *	== valmask
 * x)   For i=3, we are checking that board[pos] is not color, so AND
 *	color and we get 0 for either empty or OTHER_COLOR, but color
 *	if it contains color
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
 * ie. if  pat[r].class & class_mask[dragon[pos].status][board[pos]]
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
  
static unsigned int class_mask[NUM_DRAGON_STATUS][3];


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
  for (; pattern->patn; ++pattern)
    if (pattern->edge_constraints != 0) {

      /* If the patterns have been fixed up for a different board size
       * earlier, we need to undo the modifications that were done
       * below before we do them anew. The first time this function is
       * called, this step is effectively a no-op.
       */
      
      if (pattern->edge_constraints & NORTH_EDGE)
	pattern->maxi = pattern->mini + pattern->height;
	
      if (pattern->edge_constraints & SOUTH_EDGE)
	pattern->mini = pattern->maxi - pattern->height;
	
      if (pattern->edge_constraints & WEST_EDGE)
	pattern->maxj = pattern->minj + pattern->width;
      
      if (pattern->edge_constraints & EAST_EDGE)
	pattern->minj = pattern->maxj - pattern->width;
      
      /* we extend the pattern in the direction opposite the constraint,
       * such that maxi (+ve) - mini (-ve) = board_size-1
       * Note : the pattern may be wider than the board, so
       * we need to be a bit careful !
       */
      
      if (pattern->edge_constraints & NORTH_EDGE)
	if (pattern->maxi < (board_size-1) + pattern->mini)
	  pattern->maxi = (board_size-1) + pattern->mini;
      
      if (pattern->edge_constraints & SOUTH_EDGE)
	if (pattern->mini > pattern->maxi - (board_size-1))
	  pattern->mini = pattern->maxi - (board_size-1);
      
      if (pattern->edge_constraints & WEST_EDGE)
	if (pattern->maxj <  (board_size-1) + pattern->minj)
	  pattern->maxj = (board_size-1) + pattern->minj;
      
      if (pattern->edge_constraints & EAST_EDGE)
	if (pattern->minj > pattern->maxj - (board_size-1))
	  pattern->minj = pattern->maxj - (board_size-1);
    }
}


/* 
 * prepare a pattern matching for color point of view
 */
static void
prepare_for_match(int color)
{
  int other = OTHER_COLOR(color);

  /* Basic sanity checks. */
  gg_assert(color != EMPTY);

  /* If we set one of class_mask[XXX][color] and
   * class_mask[XXX][other], we have to explicitly set or reset the
   * other as well, since 'color' may change between calls.
   */
  class_mask[DEAD][color]     = CLASS_O;
  class_mask[DEAD][other]     = CLASS_X;
  class_mask[CRITICAL][color] = CLASS_O;
  class_mask[CRITICAL][other] = 0;       /* Need to reset this. */
  class_mask[ALIVE][color]    = CLASS_o;
  class_mask[ALIVE][other]    = CLASS_x;
}


/*
 * Try all the patterns in the given array at (anchor). Invoke the
 * callback for any that matches. Classes X,O,x,o are checked here. It
 * is up to the callback to process the other classes, and any helper
 * or autohelper functions.
 *
 * If the support of goal[BOARDMAX] is a subset of the board, patterns
 * are rejected which do not involve this dragon. If goal is a null
 * pointer, this parameter is ignored.
 */

static void
do_matchpat(int anchor, matchpat_callback_fn_ptr callback, int color,
	    struct pattern *pattern, void *callback_data,
	    signed char goal[BOARDMAX]) 
{
  const int anchor_test = board[anchor] ^ color;  /* see below */
  int m = I(anchor);
  int n = J(anchor);
  int merged_val;

  /* Basic sanity checks. */
  ASSERT_ON_BOARD1(anchor);

  /* calculate the merged value around [m][n] for the grid opt */
  {
    /* FIXME: Convert this to 2D (using delta[]) but be aware that you'll
     *	      also need to make corresponding changes in mkpat.c!
     */
    int i, j;
    int shift = 30;

    merged_val = 0;
    for (i = m-1; i <= m+2; ++i)
      for (j = n-1; j <= n+2; shift -= 2, ++j) {
	unsigned int this;
	if (!ON_BOARD2(i, j))
	  this = 3;
	else if ((this = BOARD(i, j)) == 0)
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
      int found_goal;
  
      /* We can check the color of the anchor stone now.
       * Roughly half the patterns are anchored at each
       * color, and since the anchor stone is invariant under
       * rotation, we can reject all rotations of a wrongly-anchored
       * pattern in one go.
       *
       * Patterns are always drawn from O perspective in .db,
       * so board[pos] is 'color' if the pattern is anchored
       * at O, or 'other' for X.
       * Since we require that this flag contains 3 for
       * anchored_at_X, we can check that
       *   board[pos] == (color ^ anchored_at_X)
       * which is equivalent to
       *   (board[pos] ^ color) == anchored_at_X)
       * and the LHS is something we precomputed.
       */

      if (anchor_test != pattern->anchored_at_X)
	continue;  /* does not match the anchor */

      ll = 0;  /* first transformation number */
      end_transformation = pattern->trfno;

      /* Ugly trick for dealing with 'O' symmetry. */
      if (pattern->trfno == 5) {
	ll = 2;
	end_transformation = 6;
      }
      
      /* try each orientation transformation. Assume at least 1 */

      do {

#if PROFILE_PATTERNS
	int nodes_before;
#endif
	
#if GRID_OPT == 1

	/* We first perform the grid check : this checks up to 16
	 * elements in one go, and allows us to rapidly reject
	 * patterns which do not match.  While this check invokes a
	 * necessary condition, it is not a sufficient test, so more
	 * careful checks are still required, but this allows rapid
	 * rejection. merged_val should contain a combination of
	 * 16 board positions around m, n.  The colours have been fixed
	 * up so that stones which are 'O' in the pattern are
	 * bit-pattern %01.
	 */
	if ((merged_val & pattern->and_mask[ll]) != pattern->val_mask[ll])
	  continue;  /* large-scale match failed */

#endif /* GRID_OPT == 1 */

	/* Next, we do the range check. This applies the edge
	 * constraints implicitly.
	 */
	{
	  int mi, mj, xi, xj;
	  
	  TRANSFORM2(pattern->mini, pattern->minj, &mi, &mj, ll);
	  TRANSFORM2(pattern->maxi, pattern->maxj, &xi, &xj, ll);

	  /* {min,max}{i,j} are the appropriate corners of the original
	   * pattern, Once we transform, {m,x}{i,j} are still corners,
	   * but we don't know *which* corners.
	   * We could sort them, but it turns out to be cheaper
	   * to just test enough cases to be safe.
	   */

	  DEBUG(DEBUG_MATCHER, 
		"---\nconsidering pattern '%s', rotation %d at %1m. Range %d,%d -> %d,%d\n",
		pattern->name, ll, anchor, mi, mj, xi, xj);

	  /* now do the range-check */
	  if (!ON_BOARD2(m + mi, n + mj) || !ON_BOARD2(m + xi, n + xj))
	    continue;  /* out of range */
	}

	/* Now iterate over the elements of the pattern. */
	found_goal = 0;
	for (k = 0; k < pattern->patlen; ++k) { /* match each point */
	  int pos; /* absolute coords of (transformed) pattern element */
	  int att = pattern->patn[k].att;  /* what we are looking for */

	  /* Work out the position on the board of this pattern element. */

	  /* transform pattern real coordinate... */
	  pos = AFFINE_TRANSFORM(pattern->patn[k].offset, ll, anchor);

	  ASSERT_ON_BOARD1(pos);

	  /* ...and check that board[pos] matches (see above). */
	  if ((board[pos] & and_mask[color-1][att]) != val_mask[color-1][att])
	    goto match_failed;

	  if (goal != NULL && board[pos] != EMPTY && goal[pos])
	    found_goal = 1;
	  
	  /* Check out the class_X, class_O, class_x, class_o
	   * attributes - see patterns.db and above.
	   */
	  if ((pattern->class
	       & class_mask[dragon[pos].status][board[pos]]) != 0)
	    goto match_failed; 
	  
	} /* loop over elements */


#if GRID_OPT == 2
	/* Make sure the grid optimisation wouldn't have 
           rejected this pattern */
	ASSERT2((merged_val & pattern->and_mask[ll])
		== pattern->val_mask[ll], m, n);
#endif /* we don't trust the grid optimisation */


	/* Make it here ==> We have matched all the elements to the board. */
	if ((goal != NULL) && !found_goal)
	  goto match_failed;

#if PROFILE_PATTERNS
	pattern->hits++;
	nodes_before = stats.nodes;
#endif
	
	/* A match!  - Call back to the invoker to let it know. */
	callback(anchor, color, pattern, ll, callback_data);

#if PROFILE_PATTERNS
	pattern->reading_nodes += stats.nodes - nodes_before;
#endif
	
	/* We jump to here as soon as we discover a pattern has failed. */
      match_failed:
	DEBUG(DEBUG_MATCHER, 
	      "end of pattern '%s', rotation %d at %1m\n---\n", 
	      pattern->name, ll, anchor);
	 
      } while (++ll < end_transformation); /* ll loop over symmetries */
    } /* if not rejected by maxwt */
  } while ((++pattern)->patn);  /* loop over patterns */
}


/*
 * Scan the board to get patterns anchored by anchor from color
 * point of view.
 * the board must be prepared by dfa_prepare_for_match(color) !
 */
static void
matchpat_loop(matchpat_callback_fn_ptr callback, int color, int anchor,
	      struct pattern_db *pdb, void *callback_data,
	      signed char goal[BOARDMAX], int anchor_in_goal) 
{
  int pos;
  
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (board[pos] == anchor && (!anchor_in_goal || goal[pos] != 0))
      do_matchpat(pos, callback, color, pdb->patterns,
		  callback_data, goal);
  }
}


/**************************************************************************/
/* DFA matcher:                                                           */
/**************************************************************************/

/* Set this to show the dfa board in action */
/* #define DFA_TRACE 1 */

/* Data. */
static int dfa_board_size = -1;
static int dfa_p[DFA_BASE * DFA_BASE];

/* This is used by the EXPECTED_COLOR macro. */
static const int convert[3][4] = {
  {-1, -1, -1, -1},		/* not used */
  {EMPTY, WHITE, BLACK, OUT_BOARD},	/* WHITE */
  {EMPTY, BLACK, WHITE, OUT_BOARD}	/* BLACK */
};
#define EXPECTED_COLOR(player_c, position_c)  		\
		(convert[player_c][position_c])

/* Forward declarations. */
static void dfa_prepare_for_match(int color);
static int scan_for_patterns(dfa_rt_t *pdfa, int l, int *dfa_pos,
			     int *pat_list);
static void do_dfa_matchpat(dfa_rt_t *pdfa,
			    int anchor, matchpat_callback_fn_ptr callback,
			    int color, struct pattern *database,
			    void *callback_data, signed char goal[BOARDMAX],
                            int anchor_in_goal);
static void check_pattern_light(int anchor, 
				matchpat_callback_fn_ptr callback,
				int color, struct pattern *pattern, int ll,
				void *callback_data,
				signed char goal[BOARDMAX],
                                int anchor_in_goal);
static void dfa_matchpat_loop(matchpat_callback_fn_ptr callback,
			      int color, int anchor,
			      struct pattern_db *pdb, void *callback_data,
			      signed char goal[BOARDMAX], int anchor_in_goal);


/***********************************************************************/



/* prepare the dfa board (gnugo initialization) */
void
dfa_match_init(void)
{
  build_spiral_order();

  if (owl_vital_apat_db.pdfa != NULL)
    DEBUG(DEBUG_MATCHER, "owl_vital_apat --> using dfa\n");
  if (owl_attackpat_db.pdfa != NULL)
    DEBUG(DEBUG_MATCHER, "owl_attackpat --> using dfa\n");
  if (owl_defendpat_db.pdfa != NULL)
    DEBUG(DEBUG_MATCHER, "owl_defendpat --> using dfa\n");
  if (pat_db.pdfa != NULL)
    DEBUG(DEBUG_MATCHER, "pat --> using dfa\n");
  if (attpat_db.pdfa != NULL)
    DEBUG(DEBUG_MATCHER, "attpat --> using dfa\n");
  if (defpat_db.pdfa != NULL)
    DEBUG(DEBUG_MATCHER, "defpat --> using dfa\n");
  if (endpat_db.pdfa != NULL)
    DEBUG(DEBUG_MATCHER, "endpat --> using dfa\n");
  if (conn_db.pdfa != NULL)
    DEBUG(DEBUG_MATCHER, "conn --> using dfa\n");
  if (influencepat_db.pdfa != NULL)
    DEBUG(DEBUG_MATCHER, "influencepat --> using dfa\n");
  if (barrierspat_db.pdfa != NULL)
    DEBUG(DEBUG_MATCHER, "barrierspat --> using dfa\n");
  if (fusekipat_db.pdfa != NULL)
    DEBUG(DEBUG_MATCHER, "barrierspat --> using dfa\n");

  /* force out_board initialization */
  dfa_board_size = -1;
}

/* 
 * copy the board on a private board with adapted colors 
 * and adapted size 
 */
static void
dfa_prepare_for_match(int color)
{
  int i, j;
  int pos;
    
  if (dfa_board_size != board_size) {
    dfa_board_size = board_size;
    /* clean up the board */
    for (pos = 0; pos < DFA_BASE * DFA_BASE; pos++)
      dfa_p[pos] = OUT_BOARD;
  }

  /* copy the board */
  for (i = 0; i < dfa_board_size; i++)
    for (j = 0; j < dfa_board_size; j++)
      dfa_p[DFA_POS(i, j)] = EXPECTED_COLOR(color, BOARD(i, j));

  prepare_for_match(color);
}

#if 0
/* Debug function. */
static void
dump_dfa_board(int m, int n)
{
  int i, j;

  for (i = 0; i < dfa_board_size; i++) {
    for (j = 0; j < dfa_board_size; j++) {
      if (i != m || j != n)
	fprintf(stderr, "%1d", dfa_p[DFA_POS(i, j)]);
      else
	fprintf(stderr, "*");
    }

    fprintf(stderr, "\n");
  }
}
#endif


/*
 * Scan the board with a DFA to get all patterns matching at
 * `dfa_pos' with transformation l.  Store patterns indexes
 * `pat_list'.  Return the number of patterns found.
 */
static int
scan_for_patterns(dfa_rt_t *pdfa, int l, int *dfa_pos, int *pat_list)
{
  int delta;
  int state = 1; /* initial state */
  int row = 0; /* initial row */
  int id = 0; /* position in id_list */

  do {
    /* collect patterns indexes */
    int att = pdfa->states[state].att;
    while (att != 0) {
      pat_list[id] = pdfa->indexes[att].val;
      id++;
      att = pdfa->indexes[att].next;
    }

    /* go to next state */
    delta = pdfa->states[state].next[dfa_pos[spiral[row][l]]];
    state += delta;
    row++;
  } while (delta != 0); /* while not on error state */

  return id;
}


/* Perform pattern matching with DFA filtering. */
static void
do_dfa_matchpat(dfa_rt_t *pdfa,
		int anchor, matchpat_callback_fn_ptr callback,
		int color, struct pattern *database,
		void *callback_data, signed char goal[BOARDMAX],
		int anchor_in_goal)
{
  int k;
  int ll;      /* Iterate over transformations (rotations or reflections)  */
  int patterns[DFA_MAX_MATCHED + 8];
  int num_matched = 0;
  int *dfa_pos = dfa_p + DFA_POS(I(anchor), J(anchor));

  /* Basic sanity checks. */
  ASSERT_ON_BOARD1(anchor);

  /* One scan by transformation */
  for (ll = 0; ll < 8; ll++) {
    num_matched += scan_for_patterns(pdfa, ll, dfa_pos,
				     patterns + num_matched);
    patterns[num_matched++] = -1;
  }

  ASSERT1(num_matched <= DFA_MAX_MATCHED + 8, anchor);

  /* Constraints and other tests. */
  for (ll = 0, k = 0; ll < 8; k++) {
    int matched;

    if (patterns[k] == -1) {
      ll++;
      continue;
    }

    matched = patterns[k];

#if PROFILE_PATTERNS
    database[matched].dfa_hits++;
#endif

    check_pattern_light(anchor, callback, color, database + matched,
			ll, callback_data, goal, anchor_in_goal);
  }
}


/*
 * Do the pattern matching for a given pattern and a given 
 * transformation ll. 
 * (does not recompute what dfa filtering has already done)
 */

static void
check_pattern_light(int anchor, matchpat_callback_fn_ptr callback, int color,
		    struct pattern *pattern, int ll, void *callback_data,
		    signed char goal[BOARDMAX], int anchor_in_goal)
{
  int k;			/* Iterate over elements of pattern */
  int found_goal = 0;
  
#if PROFILE_PATTERNS
  int nodes_before;
#endif
  
  if (0)
    gprintf("check_pattern_light @ %1m rot:%d pattern: %s\n", 
	    anchor, ll, pattern->name);

  /* Throw out duplicating orientations of symmetric patterns. */
  if (pattern->trfno == 5) {
    if (ll < 2 || ll >= 6)
      return;
  }
  else {
    if (ll >= pattern->trfno)
      return;
  }

 
  /* Now iterate over the elements of the pattern. */
  for (k = 0; k < pattern->patlen; k++) {
  				/* match each point */
    int pos;			/* absolute (board) co-ords of 
  				   (transformed) pattern element */

    /* transform pattern real coordinate... */
    pos = AFFINE_TRANSFORM(pattern->patn[k].offset, ll, anchor);
    ASSERT_ON_BOARD1(pos);

    if (!anchor_in_goal) { 
      /* goal check */
      if (goal != NULL && board[pos] != EMPTY && goal[pos])
	found_goal = 1;
    }

    /* class check */
    ASSERT1(dragon[pos].status < 4, anchor);
    if ((pattern->class & class_mask[dragon[pos].status][board[pos]]) != 0)
      goto match_failed;
    
  } /* loop over elements */
  
  /* Make it here ==> We have matched all the elements to the board. */
  if (!anchor_in_goal) { 
    if (goal != NULL && !found_goal)
      goto match_failed;
  }

#if PROFILE_PATTERNS
  pattern->hits++;
  nodes_before = stats.nodes;
#endif
  
  /* A match!  - Call back to the invoker to let it know. */
  callback(anchor, color, pattern, ll, callback_data);
  
#if PROFILE_PATTERNS
  pattern->reading_nodes += stats.nodes - nodes_before;
#endif
  
  /* We jump to here as soon as we discover a pattern has failed. */
 match_failed:
  DEBUG(DEBUG_MATCHER, "end of pattern '%s', rotation %d at %1m\n---\n",
	pattern->name, ll, anchor);
  
} /* check_pattern_light */


/*
 * Scan the board to get patterns anchored by anchor from color
 * point of view.
 * the board must be prepared by dfa_prepare_for_match(color) !
 */
static void
dfa_matchpat_loop(matchpat_callback_fn_ptr callback, int color, int anchor,
		  struct pattern_db *pdb, void *callback_data,
		  signed char goal[BOARDMAX], int anchor_in_goal) 
{
  int pos;

  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (board[pos] == anchor && (!anchor_in_goal || goal[pos] != 0))
      do_dfa_matchpat(pdb->pdfa, pos, callback, color, pdb->patterns,
		      callback_data, goal, anchor_in_goal);
  }
}



/**************************************************************************/
/* Main functions:                                                        */
/**************************************************************************/


typedef void (*loop_fn_ptr_t)(matchpat_callback_fn_ptr callback, 
			      int color, int anchor,
			      struct pattern_db *pdb, void *callback_data,
			      signed char goal[BOARDMAX], int anchor_in_goal);

typedef void (*prepare_fn_ptr_t)(int color);

/* same as the old matchpat but for all the board with
 * preparation.
 *
 * 4 possible values for color argument:
 * WHITE or BLACK: matchpat is called from this color point of view.
 * ANCHOR_COLOR  : matchpat is called from the anchor's point of view.
 * ANCHOR_OTHER  : matchpat is called from the opposite color of the 
 *                 anchor's point of view.
 */

void
matchpat(matchpat_callback_fn_ptr callback, int color,
	 struct pattern_db *pdb, void *callback_data,
	 signed char goal[BOARDMAX]) 
{
  matchpat_goal_anchor(callback, color, pdb, callback_data, goal, 
                       pdb->fixed_anchor);
}

void 
matchpat_goal_anchor(matchpat_callback_fn_ptr callback, int color,
		     struct pattern_db *pdb, void *callback_data,
		     signed char goal[BOARDMAX], int anchor_in_goal) 
{
  loop_fn_ptr_t loop = matchpat_loop;
  prepare_fn_ptr_t prepare = prepare_for_match;

  /* check board size */
  if (pdb->fixed_for_size != board_size) {
    fixup_patterns_for_board_size(pdb->patterns);
    pdb->fixed_for_size = board_size;
  }

  /* select pattern matching strategy */
  if (pdb->pdfa != NULL) { 
    loop = dfa_matchpat_loop;
    prepare = dfa_prepare_for_match;
  }

  /* select strategy */
  switch (color) {
    case ANCHOR_COLOR:
      { /* match pattern for the color of their anchor */
	prepare(WHITE);
	loop(callback, WHITE, WHITE, pdb, callback_data, goal, anchor_in_goal);
	prepare(BLACK);
	loop(callback, BLACK, BLACK, pdb, callback_data, goal, anchor_in_goal);
      }
      break;
    case ANCHOR_OTHER:
      { /* match pattern for the opposite color of their anchor */
	prepare(WHITE);
	loop(callback, WHITE, BLACK, pdb, callback_data, goal, anchor_in_goal);
	prepare(BLACK);
	loop(callback, BLACK, WHITE, pdb, callback_data, goal, anchor_in_goal);
      }
      break;
    default:
      { /* match all patterns for color */
	prepare(color);
	loop(callback, color, WHITE, pdb, callback_data, goal, anchor_in_goal);
	loop(callback, color, BLACK, pdb, callback_data, goal, anchor_in_goal);
      }
  }
}


static int
fullboard_transform(int pos, int trans)
{
  int dx = I(pos) - (board_size-1)/2;
  int dy = J(pos) - (board_size-1)/2;
  int x, y;
  gg_assert(POS((board_size-1)/2, (board_size-1)/2) + DELTA(dx, dy) == pos);
  TRANSFORM2(dx, dy, &x, &y, trans);
  return POS(x + (board_size-1)/2, y + (board_size-1)/2);
}

/* A dedicated matcher which can only do fullboard matching on
 * odd-sized boards, optimized for fuseki patterns.
 */
void
fullboard_matchpat(fullboard_matchpat_callback_fn_ptr callback, int color,
		   struct fullboard_pattern *pattern)
{
  int ll;   /* Iterate over transformations (rotations or reflections)  */
  /* We transform around the center point. */
  int number_of_stones_on_board = stones_on_board(BLACK | WHITE);
  static int color_map[gg_max(WHITE, BLACK) + 1];
  /* One hash value for each rotation/reflection: */
  Hash_data current_board_hash[8];
  
  /* Basic sanity check. */
  gg_assert(color != EMPTY);
  gg_assert(board_size % 2 == 1);

  color_map[EMPTY] = EMPTY;
  if (color == WHITE) {
    color_map[WHITE] = WHITE;
    color_map[BLACK] = BLACK;
  }
  else {
    color_map[WHITE] = BLACK;
    color_map[BLACK] = WHITE;
  }

  /* Get hash data of all rotations/reflections of current board position. */
  for (ll = 0; ll < 8; ll++) {
    Intersection p[BOARDSIZE];
    int pos;
    for (pos = 0; pos < BOARDSIZE; pos++)
      if (ON_BOARD(pos))
	p[pos] = color_map[board[fullboard_transform(pos, ll)]];
      else
	p[pos] = GRAY;

    if (ON_BOARD(board_ko_pos))
      hashdata_recalc(&current_board_hash[ll], p,
		      fullboard_transform(board_ko_pos, ll));
    else 
      hashdata_recalc(&current_board_hash[ll], p, NO_MOVE);
  }

  /* Try each pattern - NULL pattern name marks end of list. */
  for (; pattern->name; pattern++) { 
    if (pattern->number_of_stones != number_of_stones_on_board)
      continue;
    /* Try each orientation transformation. */
    for (ll = 0; ll < 8; ll++)
      if (hashdata_is_equal(current_board_hash[ll], pattern->fullboard_hash)) {
	/* A match!  - Call back to the invoker to let it know. */
	int pos = AFFINE_TRANSFORM(pattern->move_offset, ll,
			           POS((board_size-1)/2, (board_size-1)/2));
	callback(pos, pattern, ll);
      }
  }
}


/**************************************************************************/
/* Corner matcher                                                         */
/**************************************************************************/

/* These arrays specify anchor corner for each transformation. They _must_
 * be in line with transformation2[][] array in patterns/transform.c.
 */
static const int corner_x[8] = {0, 0, 1, 1, 1, 1, 0, 0};
static const int corner_y[8] = {0, 1, 1, 0, 1, 0, 0, 1};

/* The number of stones in "corner area" for each board position. For example,
 * corner area for position E3 when anchoring at A1 corner, looks like this:
 *
 *   |........		In general, NUM_STONES(pos) is the number of stones
 *   |........		which are closer to the corner (stone at pos, if any,
 * 3 |#####...		counts too) than pos. Note, that say G2 is not closer
 *   |#####...		to the corner than E3, the reverse isn't true either.
 * 1 |#####...		Their distances are "incomparable" since E < G but
 *   +--------		3 > 2.
 *    A   E
 *
 * Note that we need these values in at most MAX_BOARD x MAX_BOARD array.
 * However, it may be anchored at any corner of the board, so if the board is
 * small, we may calculate NUM_STONES() at negative coordinates.
 */
static int num_stones[2*BOARDMAX];
#define NUM_STONES(pos) num_stones[(pos) + BOARDMAX]

/* Stone locations are stored in this array. They might be needed by callback
 * function.
 */
static int pattern_stones[BOARDMAX];


/* Recursively performs corner matching. This function checks whether
 * `num_variation' variations pointed by `variation' parameter match.
 * If any of them does, the function calls itself recursively. If any
 * pattern corresponding to those variations matches, it notifies
 * callback function.
 */
static void
do_corner_matchpat(int num_variations, struct corner_variation *variation,
		   int match_color, corner_matchpat_callback_fn_ptr callback,
		   int callback_color, int trans, int anchor, int stones)
{
  for (; --num_variations >= 0; variation++) {
    int move = AFFINE_TRANSFORM(variation->move_offset, trans, anchor);
    int color_check = match_color ^ variation->xor_att;
    struct corner_pattern *pattern = variation->pattern;

    if (pattern && color_check == callback_color) {
      int second_corner
	  = AFFINE_TRANSFORM(pattern->second_corner_offset, trans, anchor);

      if (NUM_STONES(second_corner) == stones
	  && (!pattern->symmetric || trans < 4)) {
	/* We have found a matching pattern. */
	ASSERT1(board[move] == EMPTY, move);

	callback(move, callback_color, pattern, trans, pattern_stones, stones);
	continue;
      }
    }

    if (variation->num_variations
	&& NUM_STONES(move) == variation->num_stones
	&& board[move] == color_check) {
      /* A matching variation. */
      pattern_stones[stones] = move;
      do_corner_matchpat(variation->num_variations, variation->variations,
			 match_color, callback, callback_color,
			 trans, anchor, stones + 1);
    }
  }
}


/* Perform corner matching at all four corners and both possible
 * transformations at each corner. `callback' is notified if any
 * matching pattern is found.
 */
void
corner_matchpat(corner_matchpat_callback_fn_ptr callback, int color,
		struct corner_db *database)
{
  int k;

  for (k = 0; k < 8; k++) {
    int anchor = POS(corner_x[k] * (board_size - 1),
		     corner_y[k] * (board_size - 1));
    int i;
    int j;
    int dx = TRANSFORM(OFFSET(1, 0), k);
    int dy = TRANSFORM(OFFSET(0, 1), k);
    int pos;
    struct corner_variation *variation = database->top_variations;

    /* Fill in the NUM_STONES() array. We use `max_width' and `max_height'
     * fields of database structure to stop working as early as possible.
     */
    NUM_STONES(anchor) = IS_STONE(board[anchor]);

    pos = anchor;
    for (i = 1; i < database->max_height; i++) {
      pos += dx;
      if (!ON_BOARD(pos)) {
	do {
	  NUM_STONES(pos) = BOARDMAX;
	  pos += dx;
	} while (++i < database->max_height);

	break;
      }

      NUM_STONES(pos) = NUM_STONES(pos - dx) + IS_STONE(board[pos]);
    }

    pos = anchor;
    for (j = 1; j < database->max_width; j++) {
      pos += dy;
      if (!ON_BOARD(pos)) {
	do {
	  NUM_STONES(pos) = BOARDMAX;
	  pos += dy;
	} while (++j < database->max_width);

	break;
      }
      
      NUM_STONES(pos) = NUM_STONES(pos - dy) + IS_STONE(board[pos]);
    }
    
    for (i = 1; i < database->max_height; i++) {
      pos = anchor + i * dy;
      for (j = 1; j < database->max_width; j++) {
	pos += dx;
	NUM_STONES(pos) = NUM_STONES(pos - dx) + NUM_STONES(pos - dy)
			- NUM_STONES(pos - dx - dy);
	if (ON_BOARD1(pos) && IS_STONE(board[pos]))
	  NUM_STONES(pos)++;
      }
    }

    /* Try to match top variations. If any of them matches, we call
     * do_corner_matchpat() to recurse that variation's tree.
     */
    for (i = 0; i < database->num_top_variations; i++) {
      int move = AFFINE_TRANSFORM(variation->move_offset, k, anchor);

      if (NUM_STONES(move) == 1 && IS_STONE(board[move])) {
	pattern_stones[0] = move;
	do_corner_matchpat(variation->num_variations, variation->variations,
			   board[move], callback, color, k, anchor, 1);
      }

      variation++;
    }
  }
}


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
