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

#include "interface.h"
#include "gnugo.h"
#include "gmp.h"
#include "sgftree.h"

/* --------------------------------------------------------------*/
/* Play a game against a go-modem-protocol (GMP) client */
/* --------------------------------------------------------------*/
void play_gmp(int boardsize, Gameinfo *gameinfo)
{
  SGFTree   sgftree;

  Gmp *ge;
  GmpResult message;
  const char *error;
  
  int i, j;
  int moveval;
  int passes = 0; /* two passes and its over */
  int to_move;  /* who's turn is next ? */

  int mycolor;  /* who has which color */
  int yourcolor;
  SGFNode *curnode = NULL; /* current SGFNode */
  
  mycolor  = gameinfo->computer_player;

  sgftree_clear(&sgftree);
  sgftreeCreateHeaderNode(&sgftree, boardsize, gameinfo->position.komi);

  yourcolor = OTHER_COLOR(mycolor);

  ge = gmp_create(0, 1);
  TRACE("board size=%d\n", boardsize);

  curnode = sgftree.root;
  
  /* Leave all the -1's so the client can negotiate the game parameters. */
  if (chinese_rules)
    gmp_startGame(ge, -1, -1, 5.5, -1, -1);
  else
    gmp_startGame(ge, -1, -1, 5.5, 0, -1);	
  do {
    message = gmp_check(ge, 1, NULL, NULL, &error);
  } while (!time_to_die
	   && ((message == gmp_nothing) 
	       || (message == gmp_reset)));
  
  if (message == gmp_err)  {
    fprintf(stderr, "gnugo-gmp: Error \"%s\" occurred.\n", error);
    exit(1);
  } else if (message != gmp_newGame)  {
    fprintf(stderr, "gnugo-gmp: Expecting a newGame, got %s\n",
	    gmp_resultString(message));
    exit(1);
  }

  gameinfo->handicap = gmp_handicap(ge);
  gameinfo->position.boardsize = gmp_size(ge);
  /* 
   * The specification of the go modem protocol doesn't
   *     even discuss komi. So we may have to guess the
   *     komi. If the komi is set on the command line,
   *     keep it. Otherwise, its value will be 0.0
   *     and we use 5.5 in an even game, 0.5 otherwise.
   */
  if (gameinfo->position.komi == 0.0) {
    if (gameinfo->handicap == 0)
      gameinfo->position.komi = 5.5;
    else
      gameinfo->position.komi = 0.5;
  }

  sgfOverwritePropertyInt(sgftree.root, "SZ", gameinfo->position.boardsize);

  TRACE("size=%d, handicap=%d, komi=%f\n", 
	gameinfo->position.boardsize, gameinfo->handicap, 
	gameinfo->position.komi);

  sgffile_write_gameinfo(gameinfo, "gmp");
  gameinfo->handicap = gnugo_sethand(&(gameinfo->position), gameinfo->handicap,
				    sgftree.root);
  sgfOverwritePropertyInt(sgftree.root, "HA", gameinfo->handicap);

  if (gameinfo->handicap)
    to_move = WHITE;
  else
    to_move = BLACK;

  if (gmp_iAmWhite(ge)) {
    mycolor = WHITE;     /* computer white */
    yourcolor = BLACK;   /* human black */
  }
  else {
    mycolor = BLACK;
    yourcolor = WHITE;
  }

  /* main GMP loop */
  while (passes < 2 && !time_to_die) {

    if (to_move == yourcolor) {
      moveval = 0;
      /* Get opponent's move from gmp client. */
      message = gmp_check(ge, 1, &j, &i, &error);

      if (message == gmp_err) {
	fprintf(stderr, "GNU Go: Sorry, error from gmp client\n");
	sgffile_close_file();
	return;
      }

      if (message == gmp_undo) {
	int movenumber = gameinfo->move_number;
	curnode = sgftree.root;
	movenumber -= j-1;
	if (movenumber < 0) {
	  fprintf(stderr,
		  "GNU Go: %s UNDO: already at the beginning of game tree\n",
		  "play_gmp");
	  continue;
	}
	to_move = gnugo_play_sgftree(&gameinfo->position, curnode,
				     &movenumber, &curnode);
	gameinfo->move_number = movenumber-1;
	continue;
      }

      if (message == gmp_pass) {
	++passes;
        curnode = sgfAddPlay(curnode, to_move, -1, -1);
	gnugo_play_move(&gameinfo->position, -1, -1, yourcolor);
	sgffile_move_made(-1, -1, to_move, moveval);
	gameinfo->move_number++;
      } else {
	/* not pass */
	passes = 0;
        curnode = sgfAddPlay(curnode, to_move, i, j);
	TRACE("\nyour move: %m\n\n", i, j);
	gnugo_play_move(&gameinfo->position, i, j, yourcolor);
	sgffile_move_made(i, j, to_move, moveval);
	gameinfo->move_number++;
      }

    } else {
      /* Generate my next move. */
      moveval = gnugo_genmove(&gameinfo->position, &i, &j, mycolor,
			      gameinfo->move_number);
      gameinfo->move_number++;
      gnugo_play_move(&gameinfo->position, i, j, mycolor);
      
      if (moveval < 0) {
	/* pass */
        curnode = sgfAddPlay(curnode, to_move, -1, -1);
	gmp_sendPass(ge);
	sgffile_move_made(-1, -1, to_move, moveval);
	++passes;
      } else {
	/* not pass */
        curnode = sgfAddPlay(curnode, to_move, i, j);
	gmp_sendMove(ge, j, i);
	sgffile_move_made(i, j, to_move, moveval);
	passes = 0;
	TRACE("\nmy move: %m\n\n", i, j);
      }
    }
    
    to_move = OTHER_COLOR(to_move);
  }
  
  /* two passes: game over */
  gmp_sendPass(ge);   
  
  /* We hang around here until cgoban asks us to go, since
   * sometimes cgoban crashes if we exit first.
   */
  
  if (!quiet)
    fprintf(stderr, "Game over - waiting for client to shut us down\n");
  sgffile_close_file();
  who_wins(mycolor, stderr);
  
  
  /* play_gmp() does not return to main(), therefore the score
   * writing code is here.
   */
  score = gnugo_estimate_score(&gameinfo->position, 
			       &lower_bound, &upper_bound);

  sgfWriteResult(sgftree.root, score, 1);
  
  while (!time_to_die) {
    message = gmp_check(ge, 1, &j, &i, &error);
    if (!quiet)
      fprintf(stderr, "Message %d from gmp\n", message);
    if (message == gmp_err)
      break;
  }
  if (!quiet)
    fprintf(stderr, "gnugo going down\n");
}

/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
