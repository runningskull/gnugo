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

/* ================================================================ */
/*      Show status for a string, a dragon, etc in an SGF file.     */
/* ================================================================ */

#include "gnugo.h"

#include <stdio.h>
#include <string.h>

#include "liberty.h"
#include "sgftree.h"


/* 
 * decide_string tries to attack and defend the string at (pos),
 * and then writes the number of variations considered in the attack
 * and defence to the sgf file.
 */

void
decide_string(int pos)
{
  int aa, dd;
  int acode, dcode;
  SGFTree  tree;
  
  if (board[pos] == EMPTY) {
    fprintf(stderr, "gnugo: --decide-string called on an empty vertex\n");
    return ;
  }

  if (*outfilename)
    sgffile_begindump(&tree);

  /* Prepare pattern matcher and reading code. */
  reset_engine();

  count_variations = 1;
  acode = attack(pos, &aa);
  if (acode) {
    if (acode == WIN)
      gprintf("%1m can be attacked at %1m (%d variations)\n", 
	      pos, aa, count_variations);
    else if (acode == KO_A)
	gprintf("%1m can be attacked with ko (good) at %1m (%d variations)\n", 
	      pos, aa, count_variations);
    else if (acode == KO_B)
	gprintf("%1m can be attacked with ko (bad) at %1m (%d variations)\n", 
		pos, aa, count_variations);

    if (debug & DEBUG_READING_PERFORMANCE) {
      gprintf("Reading shadow: \n");
      draw_reading_shadow();
    }

    count_variations = 1;
    dcode = find_defense(pos, &dd);
    if (dcode) {
      if (dcode == WIN)
	gprintf("%1m can be defended at %1m (%d variations)\n", 
		pos, dd, count_variations);
      else if (dcode == KO_A)
	gprintf("%1m can be defended with ko (good) at %1m (%d variations)\n", 
		pos, dd, count_variations);
      else if (dcode == KO_B)
	gprintf("%1m can be defended with ko (bad) at %1m (%d variations)\n", 
		pos, dd, count_variations);
    }
    else
      gprintf("%1m cannot be defended (%d variations)\n", 
	      pos, count_variations);
    if (debug & DEBUG_READING_PERFORMANCE) {
      gprintf("Reading shadow: \n");
      draw_reading_shadow();
    }

  }
  else {
    gprintf("%1m cannot be attacked (%d variations)\n", 
	    pos, count_variations);
    if (debug & DEBUG_READING_PERFORMANCE) {
      gprintf("Reading shadow: \n");
      draw_reading_shadow();
    }
  }

  sgffile_enddump(outfilename);
  count_variations = 0;
}


/* 
 * decide_connection tries to connect and disconnect the strings at
 * (apos) and (bpos), and then writes the number of variations
 * considered in the attack and defence to the sgf file.
 */

void
decide_connection(int apos, int bpos)
{
  int move;
  int result;
  SGFTree tree;

  ASSERT_ON_BOARD1(apos);
  ASSERT_ON_BOARD1(bpos);
  
  if (board[apos] == EMPTY || board[bpos] == EMPTY) {
    fprintf(stderr, "gnugo: --decide-connection called on an empty vertex\n");
    return;
  }

  if (board[apos] != board[bpos]) {
    fprintf(stderr, "gnugo: --decide-connection called for strings of different colors\n");
    return;
  }

  if (*outfilename)
    sgffile_begindump(&tree);

  /* Prepare pattern matcher and reading code. */
  reset_engine();

  count_variations = 1;
  result = string_connect(apos, bpos, &move);
  if (result == WIN) {
    if (move == NO_MOVE)
      gprintf("%1m and %1m are connected as it stands (%d variations)\n", 
	      apos, bpos, count_variations);
    else
	gprintf("%1m and %1m can be connected at %1m (%d variations)\n", 
		apos, bpos, move, count_variations);
  }
  else if (result == KO_A)
    gprintf("%1m and %1m can be connected with ko (good) at %1m (%d variations)\n", 
	    apos, bpos, move, count_variations);
  else if (result == KO_B)
    gprintf("%1m and %1m can be connected with ko (bad) at %1m (%d variations)\n", 
	    apos, bpos, move, count_variations);
  else
    gprintf("%1m and %1m cannot be connected (%d variations)\n", 
	    apos, bpos, count_variations);
  
  count_variations = 1;
  result = disconnect(apos, bpos, &move);
  if (result == WIN) {
    if (move == NO_MOVE)
      gprintf("%1m and %1m are disconnected as it stands (%d variations)\n", 
	      apos, bpos, count_variations);
    else
	gprintf("%1m and %1m can be disconnected at %1m (%d variations)\n", 
		apos, bpos, move, count_variations);
  }
  else if (result == KO_A)
    gprintf("%1m and %1m can be disconnected with ko (good) at %1m (%d variations)\n", 
	    apos, bpos, move, count_variations);
  else if (result == KO_B)
    gprintf("%1m and %1m can be disconnected with ko (bad) at %1m (%d variations)\n", 
	    apos, bpos, move, count_variations);
  else
    gprintf("%1m and %1m cannot be disconnected (%d variations)\n", 
	    apos, bpos, count_variations);
  
  sgffile_enddump(outfilename);
  count_variations = 0;
}


/* 
 * decide_owl (formerly called decide_dragon) tries to attack and defend 
 * the dragon at (pos), and then writes the number of variations considered 
 * in the attack and defence to the sgf file.
 */

void
decide_owl(int pos)
{
  int move = NO_MOVE;
  int acode, dcode;
  SGFTree tree;
  int result_certain;
  int kworm;

  if (board[pos] == EMPTY) {
    fprintf(stderr, "gnugo: --decide-dragon called on an empty vertex\n");
    return ;
  }

  /* Prepare pattern matcher and reading code. */
  reset_engine();

  silent_examine_position(board[pos], EXAMINE_DRAGONS_WITHOUT_OWL);
  gprintf("finished examine_position\n");

  /* We want to see the reading performed, not just a result picked
   * from the cache. Thus we clear the cache here.
   */
  reading_cache_clear();
  
  if (*outfilename)
    sgffile_begindump(&tree);

  count_variations = 1;
  acode = owl_attack(pos, &move, &result_certain, &kworm);
  if (acode) {
    if (acode == WIN) {
      if (move == NO_MOVE)
	gprintf("%1m is dead as it stands", pos);
      else
	gprintf("%1m can be attacked at %1m (%d variations)", 
		pos, move, count_variations);
    }
    else if (acode == KO_A)
      gprintf("%1m can be attacked with ko (good) at %1m (%d variations)", 
	      pos, move, count_variations);
    else if (acode == KO_B)
      gprintf("%1m can be attacked with ko (bad) at %1m (%d variations)", 
	      pos, move, count_variations);
    else if (acode == GAIN)
      gprintf("%1m can be attacked with gain (captures %1m) at %1m (%d variations)", 
	      pos, kworm, move, count_variations);
  }
  else 
    gprintf("%1m cannot be attacked (%d variations)", pos, count_variations);
  
  if (result_certain)
    gprintf("\n");
  else
    gprintf(" result uncertain\n");

  reading_cache_clear();
  count_variations = 1;
  dcode = owl_defend(pos, &move, &result_certain, &kworm);

  if (dcode) {
    if (dcode == WIN) {
      if (move == NO_MOVE)
	gprintf("%1m is alive as it stands", pos);
      else 
	gprintf("%1m can be defended at %1m (%d variations)", 
		pos, move, count_variations);
    }
    else if (dcode == KO_A)
      gprintf("%1m can be defended with ko (good) at %1m (%d variations)", 
	      pos, move, count_variations);
    else if (dcode == KO_B)
      gprintf("%1m can be defended with ko (bad) at %1m (%d variations)", 
	      pos, move, count_variations);
    else if (dcode == LOSS)
      gprintf("%1m can be defended with loss (loses %1m) at %1m (%d variations)", 
	      pos, kworm, move, count_variations);
  }
  else
    gprintf("%1m cannot be defended (%d variations)",
	    pos, count_variations);

  if (result_certain)
    gprintf("\n");
  else
    gprintf(" result uncertain\n");
  
  sgffile_enddump(outfilename);
  count_variations = 0;
}


/* 
 * decide_dragon_data prints the dragon data at (pos).
 */

void
decide_dragon_data(int pos)
{
  if (board[pos] == EMPTY) {
    fprintf(stderr, "gnugo: --decide-dragon-data called on an empty vertex\n");
    return ;
  }
  reset_engine();
  silent_examine_position(board[pos], FULL_EXAMINE_DRAGONS);

  gprintf("Dragon at %1m:\n", pos);
  report_dragon(stderr, pos);
}


/* Print the result of the semeai code on the semeai at apos/bpos,
 * optionally writing an sgf file.
 */

void
decide_semeai(int apos, int bpos)
{
  SGFTree tree;
  int resulta, resultb, move;
  int color = board[apos];

  if (color == EMPTY || board[bpos] != OTHER_COLOR(color)) {
    gprintf("gnugo: --decide-semeai called on invalid data\n");
    return;
  }

  /* Prepare pattern matcher and reading code. */
  reset_engine();

  silent_examine_position(board[apos], EXAMINE_DRAGONS_WITHOUT_OWL);
  gprintf("finished examine_position\n");
  count_variations = 1;

  /* We want to see the reading performed, not just a result picked
   * from the cache. Thus we clear the cache here. */
  reading_cache_clear();
  
  if (*outfilename)
    sgffile_begindump(&tree);

  owl_analyze_semeai(apos, bpos, &resulta, &resultb, &move, 1);
  gprintf("After %s at %1m, %1m is %s, %1m is %s (%d nodes)\n",
	  color == BLACK ? "black" : "white",
	  move,
	  apos, safety_to_string(resulta),
  	  bpos, safety_to_string(resultb),
	  count_variations);
  owl_analyze_semeai(bpos, apos, &resultb, &resulta, &move, 1);
  gprintf("After %s at %1m, %1m is %s, %1m is %s (%d nodes)\n",
	  color == BLACK ? "white" : "black",
	  move,
	  apos, safety_to_string(resulta),
  	  bpos, safety_to_string(resultb),
	  count_variations);

  sgffile_enddump(outfilename);
  count_variations = 0;
}


void
decide_tactical_semeai(int apos, int bpos)
{
  SGFTree tree;
  int resulta, resultb, move;
  int color = board[apos];

  if (color == EMPTY || board[bpos] != OTHER_COLOR(color)) {
    gprintf("gnugo: --decide-semeai called on invalid data\n");
    return;
  }

  /* Prepare pattern matcher and reading code. */
  reset_engine();

  silent_examine_position(board[apos], EXAMINE_DRAGONS_WITHOUT_OWL);
  gprintf("finished examine_position\n");
  count_variations = 1;

  /* We want to see the reading performed, not just a result picked
   * from the cache. Thus we clear the cache here. */
  reading_cache_clear();
  
  if (*outfilename)
    sgffile_begindump(&tree);

  owl_analyze_semeai(apos, bpos, &resulta, &resultb, &move, 0);
  gprintf("After %s at %1m, %1m is %s, %1m is %s (%d nodes)\n",
	  color == BLACK ? "black" : "white",
	  move,
	  apos, safety_to_string(resulta),
  	  bpos, safety_to_string(resultb),
	  count_variations);
  owl_analyze_semeai(bpos, apos, &resultb, &resulta, &move, 0);
  gprintf("After %s at %1m, %1m is %s, %1m is %s (%d nodes)\n",
	  color == BLACK ? "white" : "black",
	  move,
	  apos, safety_to_string(resulta),
  	  bpos, safety_to_string(resultb),
	  count_variations);

  sgffile_enddump(outfilename);
  count_variations = 0;
}


/* 
 * decide_position tries to attack and defend every dragon with
 * dragon.escape<6 and writes the variations to an sgf file.
 */

void
decide_position(int color)
{
  int pos;
  int move = NO_MOVE;
  int acode = 0, dcode = 0;
  int kworm;
  static const char *snames[] = {"dead", "alive", "critical", "unknown"};
  SGFTree tree;

  /* Prepare pattern matcher and reading code. */
  reset_engine();

  silent_examine_position(color, EXAMINE_DRAGONS_WITHOUT_OWL);

  /* We want to see the reading performed, not just a result picked
   * from the cache. Thus we clear the cache here. */
  reading_cache_clear();

  if (*outfilename)
    sgffile_begindump(&tree);

  count_variations = 1;

  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (!ON_BOARD(pos)
	|| dragon[pos].origin != pos
	|| board[pos] == EMPTY
	|| DRAGON2(pos).escape_route >= 6)
      continue;

    gprintf("\nanalyzing %1m\n", pos);
    gprintf("status=%s, escape=%d\n", 
	    snames[dragon[pos].crude_status], DRAGON2(pos).escape_route);
    acode = owl_attack(pos, &move, NULL, &kworm);
    if (acode) {
      if (acode == WIN) {
	if (move == NO_MOVE)
	  gprintf("%1m is dead as it stands\n", pos);
	else
	  gprintf("%1m can be attacked at %1m (%d variations)\n", 
		  pos, move, count_variations);
      }
      else if (acode == KO_A)
	gprintf("%1m can be attacked with ko (good) at %1m (%d variations)\n", 
		pos, move, count_variations);
      else if (acode == KO_B)
	gprintf("%1m can be attacked with ko (bad) at %1m (%d variations)\n", 
		pos, move, count_variations);
      else if (acode == GAIN)
	gprintf("%1m can be attacked with gain (captures %1m) at %1m (%d variations)", 
		pos, kworm, move, count_variations);
      
      count_variations = 1;
      dcode = owl_defend(pos, &move, NULL, &kworm);
      if (dcode) {
	if (dcode == WIN) {
	  if (move == NO_MOVE)
	    gprintf("%1m is alive as it stands\n", pos);
	  else 
	    gprintf("%1m can be defended at %1m (%d variations)\n", 
		    pos, move, count_variations);
	}
	else if (dcode == KO_A)
	  gprintf("%1m can be defended with ko (good) at %1m (%d variations)\n", 
		  pos, move, count_variations);
	else if (dcode == KO_B)
	  gprintf("%1m can be defended with ko (bad) at %1m (%d variations)\n",
		  pos, move, count_variations);
	else if (dcode == LOSS)
	  gprintf("%1m can be defended with loss (loses %1m) at %1m (%d variations)", 
		  pos, kworm, move, count_variations);
      }
      else
	gprintf("%1m cannot be defended (%d variations)\n", 
		pos, count_variations);
    }
    else 
      gprintf("%1m cannot be attacked (%d variations)\n", 
	      pos, count_variations);
    
    if (acode) {
      if (dcode)
	gprintf("status of %1m revised to CRITICAL\n", pos);
      else
	gprintf("status of %1m revised to DEAD\n", pos);
    }
    else
      gprintf("status of %1m revised to ALIVE\n", pos);
  }
  
  sgffile_enddump(outfilename);
  count_variations = 0;
}


/*
 * Evaluates the eyespace at (pos) and prints a report. You can get
 * more information by adding -d0x02 to the command line.
 */

void
decide_eye(int pos)
{
  int color;
  struct eyevalue value;
  int attack_point;
  int defense_point;
  int eyepos;
  SGFTree tree;

  reset_engine();
  silent_examine_position(BLACK, EXAMINE_DRAGONS_WITHOUT_OWL);
  
  if (black_eye[pos].color == BLACK_BORDER) 
    color = BLACK;
  else if (white_eye[pos].color == WHITE_BORDER) 
    color = WHITE;
  else {
    gprintf("The eye at %1m is not of a single color.\n", pos);
    return;
  }

  if (printboard)
    showboard(0);

  /* Enable sgf output. */
  if (*outfilename)
    sgffile_begindump(&tree);
  count_variations = 1;
  
  if (black_eye[pos].color == BLACK_BORDER) {
    eyepos = black_eye[pos].origin;
    compute_eyes(eyepos, &value, &attack_point, &defense_point,
		 black_eye, half_eye, 0, EMPTY);
    gprintf("Black eyespace at %1m: %s\n", eyepos, eyevalue_to_string(&value));
    if (eye_move_urgency(&value) > 0) {
      gprintf("  vital points: %1m (attack) %1m (defense)\n", attack_point,
	      defense_point);
    }
  }
  
  if (white_eye[pos].color == WHITE_BORDER) {
    eyepos = white_eye[pos].origin;
    compute_eyes(eyepos, &value, &attack_point, &defense_point,
		 white_eye, half_eye, 0, EMPTY);
    gprintf("White eyespace at %1m: %s\n", eyepos, eyevalue_to_string(&value));
    if (eye_move_urgency(&value) > 0) {
      gprintf("  vital points: %1m (attack) %1m (defense)\n", attack_point,
	      defense_point);
    }
  }
  
  /* Finish sgf output. */
  sgffile_enddump(outfilename);
  count_variations = 0;
}


/* 
 * decide_combination tries to find a combination attack for (color) by
 * calling atari_atari().
 */

void
decide_combination(int color)
{
  int attack_move;
  char defense_moves[BOARDMAX];
  SGFTree tree;
  int first = 1;
  int pos;

  /* Prepare pattern matcher and reading code. */
  reset_engine();

  silent_examine_position(color, EXAMINE_ALL);

  if (*outfilename)
    sgffile_begindump(&tree);
  count_variations = 1;

  if (atari_atari(color, &attack_move, defense_moves, verbose)) {
    gprintf("Combination attack for %C at %1m, defense at ", color,
	    attack_move);
    for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
      if (ON_BOARD(pos) && defense_moves[pos]) {
	if (first)
	  first = 0;
	else
	  gprintf(", ");
	gprintf("%1m", pos);
      }
    }
    gprintf("\n");
  }
  else
    gprintf("No Combination attack for %C\n", color);
  
  sgffile_enddump(outfilename);
  count_variations = 0;
}


void
decide_surrounded(int pos)
{
  int surround_status;

  if (board[pos] == EMPTY) {
    fprintf(stderr, "location must not be empty!\n");
    return;
  }

  /* Prepare pattern matcher and reading code. */
  reset_engine();

  silent_examine_position(board[pos], EXAMINE_ALL);
  surround_status = compute_surroundings(pos, NO_MOVE, 1, NULL);
  if (surround_status == 1)
    gprintf("the dragon at %1m is SURROUNDED!\n", pos);
  else if (surround_status == 2)
    gprintf("the dragon at %1m is WEAKLY SURROUNDED!\n", pos);
  else
    gprintf("the dragon at %1m is not surrounded.\n", pos);
}  



/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */

