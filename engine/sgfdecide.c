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



/* ================================================================ */
/*      Show status for a string, a dragon, etc in an SGF file.     */
/* ================================================================ */

#include <stdio.h>
#include <string.h>

#include "liberty.h"
#include "sgftree.h"


/* 
 * decidestring tries to attack and defend the string at (m, n),
 * and then writes the number of variations considered in the attack
 * and defence to the sgf file.
 */

void
decidestring(int m, int n, const char *sgf_output)
{
  int      pos;
  int      acode, dcode;
  SGFTree  tree;
  
  if (BOARD(m, n) == EMPTY) {
    fprintf(stderr, "gnugo: --decidestring called on an empty vertex\n");
    return ;
  }

  if (sgf_output)
    begin_sgftreedump(&tree);

  /* Prepare pattern matcher and reading code. */
  reset_engine();

  count_variations = 1;
  acode = attack(POS(m, n), &pos);
  if (acode) {
    if (acode == WIN)
      gprintf("%m can be attacked at %1m (%d variations)\n", 
	      m, n, pos, count_variations);
    else if (acode == KO_A)
	gprintf("%m can be attacked with ko (good) at %1m (%d variations)\n", 
	      m, n, pos, count_variations);
    else if (acode == KO_B)
	gprintf("%m can be attacked with ko (bad) at %1m (%d variations)\n", 
	      m, n, pos, count_variations);

    if (debug & DEBUG_READING_PERFORMANCE) {
      gprintf("Reading shadow: \n");
      draw_reading_shadow();
    }

    count_variations = 1;
    dcode = find_defense(POS(m, n), &pos);
    if (dcode) {
      if (dcode == WIN)
	gprintf("%m can be defended at 1m (%d variations)\n", 
		m, n, pos, count_variations);
      else if (dcode == KO_A)
	gprintf("%m can be defended with ko (good) at %1m (%d variations)\n", 
		m, n, pos, count_variations);
      else if (dcode == KO_B)
	gprintf("%m can be defended with ko (bad) at %1m (%d variations)\n", 
		m, n, pos, count_variations);
    }
    else
      gprintf("%m cannot be defended (%d variations)\n", 
	      m, n, count_variations);
    if (debug & DEBUG_READING_PERFORMANCE) {
      gprintf("Reading shadow: \n");
      draw_reading_shadow();
    }

  }
  else {
    gprintf("%m cannot be attacked (%d variations)\n", 
	    m, n, count_variations);
    if (debug & DEBUG_READING_PERFORMANCE) {
      gprintf("Reading shadow: \n");
      draw_reading_shadow();
    }
  }

  if (sgf_output) {
    end_sgftreedump(sgf_output);
    count_variations = 0;
  }
}


/* 
 * decidedragon tries to attack and defend the dragon at (m, n),
 * and then writes the number of variations considered in the attack
 * and defence to the sgf file.
 */

void
decidedragon(int m, int n, const char *sgf_output)
{
  int i, j, acode, dcode;
  int save_verbose = verbose;
  SGFTree tree;
  int result_certain;

  if (BOARD(m, n) == EMPTY) {
    fprintf(stderr, "gnugo: --decidedragon called on an empty vertex\n");
    return ;
  }

  /* Prepare pattern matcher and reading code. */
  reset_engine();

  verbose = 0;
  examine_position(BOARD(m, n), EXAMINE_DRAGONS_WITHOUT_OWL);
  gprintf("finished examine_position\n");
  verbose=save_verbose;

  /* We want to see the reading performed, not just a result picked
   * from the cache. Thus we clear the cache here. */
  reading_cache_clear();
  
  if (sgf_output)
    begin_sgftreedump(&tree);

  count_variations = 1;
  acode = owl_attack(m, n, &i, &j, &result_certain);
  if (acode) {
    if (acode == WIN) {
      if (i == -1)
	gprintf("%m is dead as it stands", m, n);
      else
	gprintf("%m can be attacked at %m (%d variations)", 
		m, n, i, j, count_variations);
    }
    else if (acode == KO_A)
      gprintf("%m can be attacked with ko (good) at %m (%d variations)", 
	      m, n, i, j, count_variations);
    else if (acode == KO_B)
      gprintf("%m can be attacked with ko (bad) at %m (%d variations)", 
	      m, n, i, j, count_variations);
  }
  else 
    gprintf("%m cannot be attacked (%d variations)", m, n, count_variations);
  if (result_certain)
    gprintf("\n");
  else
    gprintf(" result uncertain\n");

  count_variations = 1;
  dcode = owl_defend(m, n, &i, &j, &result_certain);
  if (dcode) {
    if (dcode == WIN) {
      if (i == -1)
	gprintf("%m is alive as it stands", m, n);
      else 
	gprintf("%m can be defended at %m (%d variations)", 
		m, n, i, j, count_variations);
    }
    else if (dcode == KO_A)
      gprintf("%m can be defended with ko (good) at %m (%d variations)", 
	      m, n, i, j, count_variations);
    else if (dcode == KO_B)
      gprintf("%m can be defended with ko (bad) at %m (%d variations)", 
	      m, n, i, j, count_variations);
  }
  else
    gprintf("%m cannot be defended (%d variations)",
	    m, n, count_variations);
  if (result_certain)
    gprintf("\n");
  else
    gprintf(" result uncertain\n");
  
  if (sgf_output) {
    end_sgftreedump(sgf_output);
    count_variations = 0;
  }
}


void
decidesemeai(int ai, int aj, int bi, int bj, const char *sgf_output)
{
  int save_verbose = verbose;
  SGFTree tree;

  if ((BOARD(ai, aj) == EMPTY)
      || (BOARD(bi, bj) == EMPTY)) {
    fprintf(stderr, "gnugo: --decidesemeai called on an empty vertex\n");
    return ;
  }

  /* Prepare pattern matcher and reading code. */
  reset_engine();

  verbose = 0;
  examine_position(BOARD(ai, aj), EXAMINE_DRAGONS_WITHOUT_OWL);
  gprintf("finished examine_position\n");
  verbose=save_verbose;
  count_variations = 1;

  /* We want to see the reading performed, not just a result picked
   * from the cache. Thus we clear the cache here. */
  reading_cache_clear();
  
  if (sgf_output)
    begin_sgftreedump(&tree);
  owl_analyze_semeai(ai, aj, bi, bj);
  owl_analyze_semeai(bi, bj, ai, aj);

  if (sgf_output) {
    end_sgftreedump(sgf_output);
    count_variations = 0;
  }
}


/* 
 * decideposition tries to attack and defend every dragon with
 * dragon.escape<6 and writes the variations to an sgf file.
 */

void
decideposition(int color, const char *sgf_output)
{
  int m, n;
  int i, j, acode = 0, dcode = 0;
  int save_verbose=verbose;
  static const char *snames[] = {"dead", "alive", "critical", "unknown"};
  SGFTree tree;

  verbose = 0;

  /* Prepare pattern matcher and reading code. */
  reset_engine();

  examine_position(color, EXAMINE_DRAGONS_WITHOUT_OWL);
  verbose=save_verbose;

  /* We want to see the reading performed, not just a result picked
   * from the cache. Thus we clear the cache here. */
  reading_cache_clear();

  if (sgf_output)
    begin_sgftreedump(&tree);

  count_variations = 1;

  for (m = 0; m < board_size; m++)
    for (n = 0; n < board_size; n++) {
      if ((dragon[m][n].origin != POS(m, n))
	  || (BOARD(m, n) == EMPTY)
	  || (DRAGON2(m, n).escape_route >= 6))
	continue;

      gprintf("\nanalyzing %m\n", m, n);
      gprintf("status=%s, escape=%d\n", 
	      snames[dragon[m][n].status], DRAGON2(m, n).escape_route);
      acode = owl_attack(m, n, &i, &j, NULL);
      if (acode) {
	if (acode == WIN) {
	  if (i == -1)
	    gprintf("%m is dead as it stands\n", m, n);
	  else
	    gprintf("%m can be attacked at %m (%d variations)\n", 
		    m, n, i, j, count_variations);
	}
	else if (acode == KO_A)
	  gprintf("%m can be attacked with ko (good) at %m (%d variations)\n", 
		  m, n, i, j, count_variations);
	else if (acode == KO_B)
	  gprintf("%m can be attacked with ko (bad) at %m (%d variations)\n", 
		  m, n, i, j, count_variations);
	  
	count_variations = 1;
	dcode = owl_defend(m, n, &i, &j, NULL);
	if (dcode) {
	  if (dcode == WIN) {
	    if (i == -1)
	      gprintf("%m is alive as it stands\n", m, n);
	    else 
	      gprintf("%m can be defended at %m (%d variations)\n", 
		      m, n, i, j, count_variations);
	  }
	  else if (dcode == KO_A)
	    gprintf("%m can be defended with ko (good) at %m (%d variations)\n", 
		    m, n, i, j, count_variations);
	  else if (dcode == KO_B)
	    gprintf("%m can be defended with ko (bad) at %m (%d variations)\n", 
		    m, n, i, j, count_variations);
	}
	else
	  gprintf("%m cannot be defended (%d variations)\n", 
		  m, n, count_variations);
      }
      else 
	gprintf("%m cannot be attacked (%d variations)\n", 
		m, n, count_variations);
      if (acode) {
	if (dcode)
	  gprintf("status of %m revised to CRITICAL\n", m, n);
	else
	  gprintf("status of %m revised to DEAD\n", m, n);
      }
      else
	gprintf("status of %m revised to ALIVE\n", m, n);
    }

  if (sgf_output) {
    end_sgftreedump(sgf_output);
    count_variations = 0;
  }
}


/*
 * Evaluates the eyespace at (m,n) and prints a report.
 */

void
decideeye(int m, int n, const char *sgf_output)
{
  int  color;
  int  max, min;
  int  attacki, attackj;
  int  defendi, defendj;
  int  i, j;
  int  save_verbose = verbose;
  int  save_debug = debug;
  SGFTree tree;

  reset_engine();
  verbose = 0;
  debug &= ~(DEBUG_EYES | DEBUG_LIFE);
  examine_position(BLACK, EXAMINE_DRAGONS);
  verbose = save_verbose;
  debug = save_debug;
  
  if (black_eye[m][n].color == BLACK_BORDER) 
    color = BLACK;
  else if (white_eye[m][n].color == WHITE_BORDER) 
    color = WHITE;
  else {
    gprintf("The eye at %m is not of a single color.\n", m, n);
    return;
  }

  if (printboard)
    showboard(0);

  if (life)
    reset_life_node_counter();

  /* Enable sgf output. */
  if (sgf_output)
    begin_sgftreedump(&tree);
  count_variations = 1;
  
  if (black_eye[m][n].color == BLACK_BORDER) {
    i = I(black_eye[m][n].origin);
    j = J(black_eye[m][n].origin);
    compute_eyes(i, j, &max, &min, &attacki, &attackj, &defendi, &defendj,
		 black_eye, half_eye, 0, EMPTY);
    gprintf("Black eyespace at %m: min=%d, max=%d\n", i, j, min, max);
    if (max != min) {
      gprintf("  vital points: %m (attack) %m (defense)\n", attacki, attackj,
	      defendi, defendj);
    }
  }
  if (white_eye[m][n].color == WHITE_BORDER) {
    i = I(white_eye[m][n].origin);
    j = J(white_eye[m][n].origin);
    compute_eyes(i, j, &max, &min, &attacki, &attackj, &defendi, &defendj,
		 white_eye, half_eye, 0, EMPTY);
    gprintf("White eyespace at %m: min=%d, max=%d\n", i, j, min, max);
    if (max != min) {
      gprintf("  vital points: %m (attack) %m (defense)\n", attacki, attackj,
	      defendi, defendj);
    }
  }
  
  if (life)
    printf("%d positions examined by the life module.\n",
	   get_life_node_counter());

  /* Finish sgf output. */
  if (sgf_output) {
    end_sgftreedump(sgf_output);
    count_variations = 0;
  }
}


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
