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
#include <assert.h>
#include <math.h>

#include "interface.h"

#include "liberty.h" /* to get to the stats */

#include "sgftree.h"
#include "gnugo.h"
#include "random.h"
#include "gg_utils.h"


void
play_solo(Gameinfo *gameinfo, int moves)
{
  int passes = 0; /* num. consecutive passes */
  int move_val;
  double t1, t2;
  int save_moves = moves;
  int boardsize = gameinfo->position.boardsize;

  struct stats_data totalstats;
  int total_owl_count = 0;

  /* It tends not to be very imaginative in the opening,
   * so we scatter a few stones randomly to start with.
   * We add two random numbers to reduce the probability
   * of playing stones near the edge.
   */
  
  int n = 6 + 2*gg_rand()%5;
  int i, j;

  sgffile_write_gameinfo(gameinfo, "solo");
 
  /* Generate some random moves. */
  if (boardsize > 6) {
    do {
      do {
	i = (gg_rand() % 4) + (gg_rand() % (boardsize - 4));
	j = (gg_rand() % 4) + (gg_rand() % (boardsize - 4));
      } while (!gnugo_is_legal(&gameinfo->position, i, j, 
			       gameinfo->to_move));
      
      gameinfo_play_move(gameinfo, i, j, gameinfo->to_move);
    } while (--n > 0);
  }
  
  t1 = gg_gettimeofday();
  memset(&totalstats, '\0', sizeof(totalstats));
  while (passes < 2 && --moves >= 0 && !time_to_die) {
    reset_owl_node_counter();
    move_val = gnugo_genmove(&gameinfo->position, &i, &j,
			     gameinfo->to_move,
			     gameinfo->move_number);
    gameinfo_play_move(gameinfo, i, j, gameinfo->to_move);

    if (move_val < 0) {
      ++passes;
      printf("%s(%d): Pass\n", gameinfo->to_move == BLACK ? "Black" : "White",
	     gameinfo->move_number);
    }
    else {
      passes = 0;
      gprintf("%s(%d): %m\n", gameinfo->to_move == BLACK ? "Black" : "White",
	      gameinfo->move_number, i, j);
    }

    totalstats.nodes               += stats.nodes;
    totalstats.position_entered    += stats.position_entered;
    totalstats.position_hits       += stats.position_hits;
    totalstats.read_result_entered += stats.read_result_entered;
    totalstats.hash_collisions     += stats.hash_collisions;
    total_owl_count                += get_owl_node_counter();
  }
  t2 = gg_gettimeofday();
  
  /* Two passes and it's over. (EMPTY == BOTH) */
  gnugo_who_wins(&gameinfo->position, EMPTY, stdout);

#if 0
  if (t2 == t1)
    printf("%.3f moves played\n", (double) (save_moves-moves));
  else
    printf("%.3f moves/sec\n", (save_moves-moves)/(t2-t1));
#else
  printf("%10d moves played in %0.3f seconds\n", save_moves-moves, t2-t1);
  if (save_moves != moves)
    printf("%10.3f seconds/move\n", (t2-t1)/(save_moves-moves));
  printf("%10d nodes\n", totalstats.nodes);
  printf("%10d positions entered\n", totalstats.position_entered);
  printf("%10d position hits\n", totalstats.position_hits);
  printf("%10d read results entered\n", totalstats.read_result_entered);
  printf("%10d hash collisions\n", totalstats.hash_collisions);
  printf("%10d owl nodes\n", total_owl_count);
#endif
}


/* ================================================================ */


/* FIXME: This should be in a separate source file.
 */


/*
 * Load SGF file and run genmove().
 */

void 
load_and_analyze_sgf_file(SGFNode *head, Gameinfo *gameinfo, 
			  const char *untilstr, int benchmark, int to_move)
{
  int i, j;
  int next;
  int r;
  
  /* We load the header to get correct boardsize, komi, and handicap
   * for writing.
   */
  gameinfo_load_sgfheader(gameinfo, head); 
  next = gameinfo_play_sgftree(gameinfo, head, untilstr);
  if (to_move != EMPTY)
    next = to_move;
  gameinfo->computer_player = next;
  sgffile_write_gameinfo(gameinfo, "load and analyze");

  if (benchmark) {
    for (r = 0; r < benchmark; ++r) {
      genmove(&i, &j, next);
      next = OTHER_COLOR(next);
    }
  }
  else {
    genmove(&i, &j, next);
    
    if (is_pass(POS(i, j))) {
      gprintf("%s move: PASS!\n", next == WHITE ? "white (o)" : "black (X)");
      sgffile_move_made(i, j, next, 0);
    }
    else {
      gprintf("%s move %m\n", next == WHITE ? "white (o)" : "black (X)",
	      i, j);
      gnugo_play_move(&gameinfo->position, i, j, next);
      sgffile_move_made(i, j, next, 0);
    }
  }
}


/*
 * Load SGF file and score the game
 * untilstr:
 * end  - finish the game by selfplaying from the end of the file until 
 *        two passes
 * last - estimate territorial balance at the end of the of the file
 * move - load file until move is reached and estimate territorial balance
 *
 * aftermath - like 'end' but also plays out the aftermath
 */

void 
load_and_score_sgf_file(SGFTree *tree, Gameinfo *gameinfo, 
			const char *untilstr)
{
  int i, j, move_val, pass;
  int until;
  float komi;
  float result;
  char *tempc = NULL;
  char dummy;
  char text[250];
  char winner;
  int next;
  pass = 0;

  assert(tree);
  gameinfo_load_sgfheader(gameinfo, tree->root);
  sgffile_write_gameinfo(gameinfo, "load and score");
  next = gameinfo_play_sgftree(gameinfo, tree->root, untilstr);
  komi = gameinfo->position.komi;
  doing_scoring = 1;
  
  until = atoi(untilstr);
  if (!strcmp(untilstr, "end") || !strcmp(untilstr, "aftermath")) {
    until = 9999;
    do {
      move_val = genmove_conservative(&i, &j, next);
      play_move(POS(i, j), next);
      if (move_val >= 0) {
	pass = 0;
	gprintf("%d %s move %m\n", gameinfo->move_number,
		next == WHITE ? "white (o)" : "black (X)", i, j);
      }
      else {
	++pass;
	gprintf("%d %s move : PASS!\n", gameinfo->move_number, 
		next == WHITE ? "white (o)" : "black (X)");
      }
      sgffile_move_made(i, j, next, move_val);
      sgftreeAddPlay(tree, 0, next, i, j);
      gameinfo->move_number++;
      next = OTHER_COLOR(next);
    } while ((gameinfo->move_number <= until) && (pass < 2));

    if (pass >= 2) {
      /* Calculate the score */
      if (!strcmp(untilstr, "aftermath"))
	score = aftermath_compute_score(next, komi);
      else
	score = gnugo_estimate_score(&gameinfo->position,
				     &lower_bound, &upper_bound);

      if (score < 0.0) {
	sprintf(text, "Black wins by %1.1f points\n", -score);
	winner = 'B';
      }
      else if (score > 0.0) {
	sprintf(text, "White wins by %1.1f points\n", score);
	winner = 'W';
      }
      else {
	sprintf(text, "Jigo\n");
	winner = '0';
      }
      fputs(text, stdout);
      sgfAddComment(tree->lastnode, text);
      sgffile_write_comment(text);
      if (sgfGetCharProperty(tree->root, "RE", &tempc)) {
	if (sscanf(tempc, "%1c%f", &dummy, &result) == 2) {
	  fprintf(stdout, "Result from file: %1.1f\n", result);
	  fputs("GNU Go result and result from file are ", stdout);
	  if ((result == fabs(score)) && (winner == dummy))
	    fputs("identical\n", stdout);
	  else
	    fputs("different\n", stdout);
	      
	}
	else {
	  if (tempc[2] == 'R') {
	    fprintf(stdout, "Result from file: Resign\n");
	    fputs("GNU Go result and result from file are ", stdout);
	    if (tempc[0] == winner)
	      fputs("identical\n", stdout);
	    else
	      fputs("different\n", stdout);
	  }
	}
      }
      sgfWriteResult(tree->root, score, 1);
    }
  }
  doing_scoring = 0;

  if (!strcmp(untilstr, "aftermath"))
    return;

  move_val = genmove_conservative(&i, &j, next);
  score = estimate_score(NULL, NULL);

  fprintf(stdout, "\n%s seems to win by %1.1f points\n",
	  score < 0 ? "B" : "W",
	  score < 0 ? -score : score);
}


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
