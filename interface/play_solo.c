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
#include <math.h>

#include "interface.h"

#include "liberty.h" /* to get to the stats */

#include "sgftree.h"
#include "random.h"
#include "gg_utils.h"

void
play_solo(Gameinfo *gameinfo, int moves)
{
  SGFTree sgftree;
  int passes = 0; /* num. consecutive passes */
  int move_val;
  double t1, t2;
  int save_moves = moves;
  int boardsize = gnugo_get_boardsize();

  struct stats_data totalstats;
  int total_owl_count = 0;

  /* It tends not to be very imaginative in the opening,
   * so we scatter a few stones randomly to start with.
   * We add two random numbers to reduce the probability
   * of playing stones near the edge.
   */
  
  int n = 6 + 2*gg_rand()%5;
  int i, j;

  gnugo_set_komi(5.5);

  sgftree_clear(&sgftree);
  sgftreeCreateHeaderNode(&sgftree, gnugo_get_boardsize(), gnugo_get_komi());
  sgf_write_header(sgftree.root, 1, random_seed, 5.5, level, chinese_rules);
 
  /* Generate some random moves. */
  if (boardsize > 6) {
    do {
      do {
	i = (gg_rand() % 4) + (gg_rand() % (boardsize - 4));
	j = (gg_rand() % 4) + (gg_rand() % (boardsize - 4));
      } while (!gnugo_is_legal(i, j, gameinfo->to_move));

      gnugo_play_move(i, j, gameinfo->to_move);
      sgftreeAddPlay(&sgftree, gameinfo->to_move, i, j);
      sgftreeAddComment(&sgftree, "random move");
      gameinfo->to_move = OTHER_COLOR(gameinfo->to_move);
    } while (--n > 0);
  }
  
  t1 = gg_cputime();
  memset(&totalstats, '\0', sizeof(totalstats));
  while (passes < 2 && --moves >= 0) {
    reset_owl_node_counter();
    move_val = gnugo_genmove(&i, &j, gameinfo->to_move);

    gnugo_play_move(i, j, gameinfo->to_move);
    sgffile_add_debuginfo(sgftree.lastnode, move_val);
    sgftreeAddPlay(&sgftree, gameinfo->to_move, i, j);
    sgffile_output(&sgftree);
    gameinfo->to_move = OTHER_COLOR(gameinfo->to_move);

    if (move_val < 0) {
      ++passes;
      printf("%s(%d): Pass\n", gameinfo->to_move == BLACK ? "Black" : "White",
	     movenum);
    }
    else {
      passes = 0;
      gprintf("%s(%d): %m\n", gameinfo->to_move == BLACK ? "Black" : "White",
	      movenum, i, j);
    }

    totalstats.nodes               += stats.nodes;
    totalstats.position_entered    += stats.position_entered;
    totalstats.position_hits       += stats.position_hits;
    totalstats.read_result_entered += stats.read_result_entered;
    totalstats.hash_collisions     += stats.hash_collisions;
    total_owl_count                += get_owl_node_counter();
  }
  t2 = gg_cputime();
  
  /* Two passes and it's over. (EMPTY == BOTH) */
  gnugo_who_wins(EMPTY, stdout);

  score = gnugo_estimate_score(&lower_bound, &upper_bound);
  sgfWriteResult(sgftree.root, score, 1);
  sgffile_output(&sgftree);

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


/*
 * Load SGF file and run genmove().
 */

void 
load_and_analyze_sgf_file(Gameinfo *gameinfo)
{
  SGFTree sgftree;
  int i, j;
  int next;
  int move_val;
  
  next = gameinfo->to_move;
  sgftree = gameinfo->game_record;

  move_val = gnugo_genmove(&i, &j, next);

  if (is_pass(POS(i, j)))
    gprintf("%s move: PASS!\n", next == WHITE ? "white (O)" : "black (X)");
  else
    gprintf("%s move %m\n", next == WHITE ? "white (O)" : "black (X)", i, j);

  gnugo_play_move(i, j, next);
  sgftreeAddPlay(&sgftree, next, i, j);
  sgftreeAddComment(&sgftree, "load and analyze mode");
  sgffile_add_debuginfo(sgftree.lastnode, move_val);
  sgffile_output(&sgftree);
}


/*
 * Load SGF file and score the game
 * scoringmode:
 * estimate  - estimate territorial balance
 * finish    - finish the game by selfplaying and then count the score quickly
 * aftermath - like 'finish' but also play out the aftermath in order to
 *             get an accurate score
 */

void 
load_and_score_sgf_file(SGFTree *tree, Gameinfo *gameinfo, 
			const char *scoringmode)
{
  int i, j, move_val;
  float result;
  char *tempc = NULL;
  char dummy;
  char text[250];
  char winner;
  int next;
  int pass = 0;
  SGFTree score_tree;
  
  sgftree_clear(&score_tree);
  sgftreeCreateHeaderNode(&score_tree, board_size, komi);
  sgffile_printboard(&score_tree);
  
  next = gameinfo->to_move;
  doing_scoring = 1;
  reset_engine();
  
  if (!strcmp(scoringmode, "finish") || !strcmp(scoringmode, "aftermath")) {
    do {
      move_val = genmove_conservative(&i, &j, next);
      if (move_val >= 0) {
	pass = 0;
	gprintf("%d %s move %m\n", movenum,
		next == WHITE ? "white (O)" : "black (X)", i, j);
      }
      else {
	++pass;
	gprintf("%d %s move : PASS!\n", movenum, 
		next == WHITE ? "white (O)" : "black (X)");
      }
      play_move(POS(i, j), next);
      sgffile_add_debuginfo(score_tree.lastnode, move_val);
      sgftreeAddPlay(&score_tree, next, i, j);
      sgffile_output(&score_tree);
      next = OTHER_COLOR(next);
    } while (movenum <= 10000 && pass < 2);

    if (pass >= 2) {
      /* Calculate the score */
      if (!strcmp(scoringmode, "aftermath"))
	score = aftermath_compute_score(next, komi, &score_tree);
      else
	score = gnugo_estimate_score(&lower_bound, &upper_bound);

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
      sgftreeAddComment(&score_tree, text);
      if (sgfGetCharProperty(tree->root, "RE", &tempc)) {
	if (sscanf(tempc, "%1c%f", &dummy, &result) == 2) {
	  fprintf(stdout, "Result from file: %1.1f\n", result);
	  fputs("GNU Go result and result from file are ", stdout);
	  if (result == fabs(score) && winner == dummy)
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
      sgfWriteResult(score_tree.root, score, 1);
      sgffile_output(&score_tree);
    }
  }
  doing_scoring = 0;

  if (strcmp(scoringmode, "aftermath")) {
    /* Before we call estimate_score() we must make sure that the dragon
     * status is computed. Therefore the call to examine_position().
     */
    examine_position(next, EXAMINE_ALL);
    score = estimate_score(NULL, NULL);

    fprintf(stdout, "\n%s seems to win by %1.1f points\n",
      score < 0 ? "B" : "W",
      score < 0 ? -score : score);
  }
}


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
