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
#include <string.h>

#include "liberty.h"
#include "gg_utils.h"
#include "patterns.h"
#include "dfa.h"


/**************************************************************************/
/* Standard matcher:                                                      */
/**************************************************************************/


/* Forward declarations. */

static void fixup_patterns_for_board_size(struct pattern *pattern);
static void prepare_for_match(int color);
static void do_matchpat(int anchor, matchpat_callback_fn_ptr callback,
			int color, struct pattern *database,
			void *callback_data, char goal[BOARDMAX]);
static void matchpat_loop(matchpat_callback_fn_ptr callback, 
			  int color, int anchor,
			  struct pattern_db *pdb, void *callback_data,
			  char goal[BOARDMAX], int anchor_in_goal);
void transform2(int i, int j, int *ti, int *tj, int trans);

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
  
static unsigned int class_mask[MAX_DRAGON_STATUS][3];


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


/* Functional version for completeness. Prefer the TRANSFORM2 macro
 * in liberty.h.
 */

void 
transform2(int i, int j, int *ti, int *tj, int trans)
{
  TRANSFORM2(i, j, ti, tj, trans);
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
	    char goal[BOARDMAX]) 
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
      int found_goal, found_nongoal;
  
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

	  TRACE_MATCHER(
		"---\nconsidering pattern '%s', rotation %d at %1m. Range %d,%d -> %d,%d\n",
		pattern->name, ll, anchor, mi, mj, xi, xj);

	  /* now do the range-check */
	  if (!ON_BOARD2(m + mi, n + mj) || !ON_BOARD2(m + xi, n + xj))
	    continue;  /* out of range */
	}

	/* Now iterate over the elements of the pattern. */
	found_goal = 0;
	found_nongoal = 0;
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

	  if (goal != NULL && board[pos] != EMPTY) {
	    if (goal[pos])
	      found_goal = 1;
	    else if (board[pos] == color)
	      found_nongoal = 1;
	  }
	  
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
	if ((goal != NULL) && ((pattern->class) & CLASS_C) && !found_nongoal)
	  goto match_failed;
	
	/* A match!  - Call back to the invoker to let it know. */
	callback(anchor, color, pattern, ll, callback_data);

	/* We jump to here as soon as we discover a pattern has failed. */
      match_failed:
	TRACE_MATCHER(
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
	      char goal[BOARDMAX], int anchor_in_goal) 
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

/* data */
extern int board_size;
static int dfa_board_size = -1;
extern int dfa_p[DFA_MAX_BOARD * 4 * DFA_MAX_BOARD * 4];
extern int spiral[MAX_ORDER][8];

/* This is used by the EXPECTED_COLOR macro. */
extern const int convert[3][4];

/* Forward declarations. */
static void dfa_prepare_for_match(int color);
static int scan_for_patterns(dfa_rt_t *pdfa, int l, int dfa_pos,
			     int *pat_list);
static void do_dfa_matchpat(dfa_rt_t *pdfa,
			    int anchor, matchpat_callback_fn_ptr callback,
			    int color, struct pattern *database,
			    void *callback_data, char goal[BOARDMAX],
                            int anchor_in_goal);
static void check_pattern_light(int anchor, 
				matchpat_callback_fn_ptr callback,
				int color, struct pattern *pattern, int ll,
				void *callback_data,
				char goal[BOARDMAX],
                                int anchor_in_goal);
static void dfa_matchpat_loop(matchpat_callback_fn_ptr callback,
			      int color, int anchor,
			      struct pattern_db *pdb, void *callback_data,
			      char goal[BOARDMAX], int anchor_in_goal);


/***********************************************************************/



/* prepare the dfa board (gnugo initialization) */
void
dfa_match_init(void)
{
  buildSpiralOrder(spiral);

  if (owl_vital_apat_db.pdfa != NULL)
    TRACE_MATCHER("owl_vital_apat --> using dfa\n");
  if (owl_attackpat_db.pdfa != NULL)
    TRACE_MATCHER("owl_attackpat --> using dfa\n");
  if (owl_defendpat_db.pdfa != NULL)
    TRACE_MATCHER("owl_defendpat --> using dfa\n");
  if (pat_db.pdfa != NULL)
    TRACE_MATCHER("pat --> using dfa\n");
  if (attpat_db.pdfa != NULL)
    TRACE_MATCHER("attpat --> using dfa\n");
  if (defpat_db.pdfa != NULL)
    TRACE_MATCHER("defpat --> using dfa\n");
  if (endpat_db.pdfa != NULL)
    TRACE_MATCHER("endpat --> using dfa\n");
  if (conn_db.pdfa != NULL)
    TRACE_MATCHER("conn --> using dfa\n");
  if (influencepat_db.pdfa != NULL)
    TRACE_MATCHER("influencepat --> using dfa\n");
  if (barrierspat_db.pdfa != NULL)
    TRACE_MATCHER("barrierspat --> using dfa\n");
  if (fusekipat_db.pdfa != NULL)
    TRACE_MATCHER("barrierspat --> using dfa\n");

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
  int ii;
    
  if (dfa_board_size != board_size) {
    dfa_board_size = board_size;
    /* clean up the board */
    for (ii = 0; ii < 4 * DFA_MAX_BOARD * 4 * DFA_MAX_BOARD; ii++)
      dfa_p[ii] = OUT_BOARD;
  }

  /* copy the board */
  for (i = 0; i < dfa_board_size; i++)
    for (j = 0; j < dfa_board_size; j++)
      dfa_p[DFA_POS(i, j) + DFA_OFFSET] = EXPECTED_COLOR(color, BOARD(i, j));

  prepare_for_match(color);
}

/* 
 * Scan the board with a dfa to get 
 * all patterns matching at (m, n) with transformation l.
 * Store patterns indexes + transformation in pat_list.
 * Return the number of patterns found.
 */
static int
scan_for_patterns(dfa_rt_t *pdfa, int l, int dfa_pos, int *pat_list)
{
  int delta;
  int state = 1; /* initial state */
  int row = 0; /* initial row */
  int id = 0; /* position in id_list */ 
  
  do {
    /* collect patterns indexes */
    int att = pdfa->states[state].att;
    while (att != 0) {
      if (pdfa->pre_rotated)
        pat_list[id] = pdfa->indexes[att].val;
      else
        pat_list[id] = l + 8 * (int)(pdfa->indexes[att].val);
      id++;
      att = pdfa->indexes[att].next;
    }
      
    /* go to next state */
    delta = pdfa->states[state].next[dfa_p[dfa_pos + spiral[row][l]]];
    state += delta;
    row++;
  } while (delta != 0); /* while not on error state */

  ASSERT1(row < MAX_ORDER, dfa_pos);
  return id;
}


/* perform pattern matching with dfa filtering */
static void 
do_dfa_matchpat(dfa_rt_t *pdfa,
		int anchor, matchpat_callback_fn_ptr callback,
		int color, struct pattern *database,
		void *callback_data, char goal[BOARDMAX],
                int anchor_in_goal)
{
  int ll;      /* Iterate over transformations (rotations or reflections)  */
  int matched; /* index in database[] of the matched pattern */

  int reorder[DFA_MAX_MATCHED];
  int *preorder = reorder;
  int maxr = 0, k;
  int dfa_pos = DFA_POS(I(anchor), J(anchor)) + DFA_OFFSET;

  /* Basic sanity checks. */
  ASSERT_ON_BOARD1(anchor);

  /* Geometrical pattern matching */
  maxr = 0;

  /* one scan by transformation */
  for (ll = 0; ll != 8; ll++) {
    maxr += scan_for_patterns(pdfa, ll, dfa_pos, preorder);
    preorder = reorder + maxr;
    if (pdfa->pre_rotated)
      break;
  }

  ASSERT1(maxr < DFA_MAX_MATCHED, anchor);

  /* Constraints and other tests */

  for (k = 0; k != maxr ; k++) {
    matched = reorder[k] / 8;
    ll = reorder[k] % 8;

    check_pattern_light(anchor, callback, color, database+matched, 
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
		    char goal[BOARDMAX], int anchor_in_goal)
{
  int k;			/* Iterate over elements of pattern */
  int found_goal = 0;
  int found_nongoal = 0;
  
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
      if (goal != NULL && board[pos] != EMPTY) {
        if (goal[pos])
	  found_goal = 1;
        else if (board[pos] == color)
	  found_nongoal = 1;
      }
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
    if (goal != NULL && (pattern->class & CLASS_C) && !found_nongoal)
      goto match_failed;
  }

  /* A match!  - Call back to the invoker to let it know. */
  callback(anchor, color, pattern, ll, callback_data);
  
  /* We jump to here as soon as we discover a pattern has failed. */
 match_failed:
  TRACE_MATCHER("end of pattern '%s', rotation %d at %1m\n---\n",
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
		  char goal[BOARDMAX], int anchor_in_goal) 
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
			      char goal[BOARDMAX], int anchor_in_goal);

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
	 char goal[BOARDMAX]) 
{
  matchpat_goal_anchor(callback, color, pdb, callback_data, goal, 
                       pdb->fixed_anchor);
}

void 
matchpat_goal_anchor(matchpat_callback_fn_ptr callback, int color,
		     struct pattern_db *pdb, void *callback_data,
		     char goal[BOARDMAX], int anchor_in_goal) 
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
  /* We transform around the center point. */
  int mid = POS((board_size-1)/2, (board_size-1)/2);
  int number_of_stones_on_board = stones_on_board(BLACK | WHITE);
  
  /* Basic sanity check. */
  gg_assert(color != EMPTY);
  gg_assert(board_size % 2 == 1);

  /* Try each pattern - NULL pattern marks end of list. */
  for (; pattern->patn; pattern++) { 
    /* The number of stones on the board must be right. This is not
     * only an optimization because we never even look at the
     * intersections which are empty in the pattern.
     */
    if (pattern->patlen != number_of_stones_on_board)
      continue;
    
    /* try each orientation transformation */
    for (ll = 0; ll < 8; ll++) {
      /* Now iterate over the elements of the pattern. */
      for (k = 0; k < pattern->patlen; k++) { /* match each point */
	int pos; /* board co-ords of transformed pattern element */
	int att = pattern->patn[k].att;  /* what we are looking for */

	/* Work out the position on the board of this pattern element. */
	pos = AFFINE_TRANSFORM(pattern->patn[k].offset, ll, mid);

        ASSERT_ON_BOARD1(pos);

	if ((att == ATT_O && board[pos] != color)
	    || (att == ATT_X && board[pos] != other))
	  break;
	
      } /* loop over elements */
	
      if (k == pattern->patlen) {
	/* A match!  - Call back to the invoker to let it know. */
	int pos = AFFINE_TRANSFORM(pattern->move_offset, ll, mid);
	callback(pos, pattern, ll);
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
