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
#include <ctype.h>

#include "liberty.h"
#include "interface.h"
#include "sgftree.h"

#define DEBUG_COMMANDS "\
capture <pos>    try to capture indicated group\n\
dead             Toggle display of dead stones\n\
defend <pos>     try to defend indicated group\n\
listdragons      print dragon info \n\
showarea         display area\n\
showdragons      display dragons\n\
showmoyo         display moyo\n\
showterri        display territory\n\
"

/* some options for the ascii interface */
static int opt_showboard = 1;
static int showdead = 0;
static int emacs = 0;
static SGFTree sgftree;
static int last_move_i;      /* The position of the last move */
static int last_move_j;      /* -""-                          */

/* Unreasonable score used to detect missing information. */
#define NO_SCORE 4711
/* Keep track of the score estimated before the last computer move. */
static int current_score_estimate = NO_SCORE;

static void endgame(Gameinfo *gameinfo);
static void showcapture(char *line);
static void showdefense(char *line);
static void ascii_goto(Gameinfo *gameinfo, char *line);
static void ascii_free_handicap(Gameinfo *gameinfo, char *handicap);

/* If sgf game info is written can't reset parameters like handicap, etc. */
static int sgf_initialized;

/*
 * Create letterbar for the top and bottom of the ASCII board.
 */

static void
make_letterbar(int boardsize, char *letterbar)
{
  int i, letteroffset;
  char spaces[64];
  char letter[64];

  if (boardsize <= 25)
    strcpy(spaces, " ");
  strcpy(letterbar, "   ");
  
  for (i = 0; i < boardsize; i++) {
    letteroffset = 'A';
    if (i+letteroffset >= 'I')
      letteroffset++;
    strcat(letterbar, spaces);
    sprintf(letter, "%c", i+letteroffset);
    strcat(letterbar, letter);
  }
}


/* This array contains +'s and -'s for the empty board positions.
 * hspot_size contains the board size that the grid has been
 * initialized to.
 */

static int hspot_size;
static char hspots[MAX_BOARD][MAX_BOARD];


/*
 * Mark the handicap spots on the board.
 */

static void
set_handicap_spots(int boardsize)
{
  if (hspot_size == boardsize)
    return;
  
  hspot_size = boardsize;
  
  memset(hspots, '.', sizeof(hspots));

  if (boardsize == 5) {
    /* place the outer 4 */
    hspots[1][1] = '+';
    hspots[boardsize-2][1] = '+';
    hspots[1][boardsize-2] = '+';
    hspots[boardsize-2][boardsize-2] = '+';
    /* and the middle one */
    hspots[boardsize/2][boardsize/2] = '+';
    return;
  }

  if (!(boardsize%2)) {
    /* If the board size is even, no center handicap spots. */
    if (boardsize > 2 && boardsize < 12) {
      /* Place the outer 4 only. */
      hspots[2][2] = '+';
      hspots[boardsize-3][2] = '+';
      hspots[2][boardsize-3] = '+';
      hspots[boardsize-3][boardsize-3] = '+';
    }
    else {
      /* Place the outer 4 only. */
      hspots[3][3] = '+';
      hspots[boardsize-4][3] = '+';
      hspots[3][boardsize-4] = '+';
      hspots[boardsize-4][boardsize-4] = '+';
    }
  }
  else {
    /* Uneven board size */
    if (boardsize > 2 && boardsize < 12) {
      /* Place the outer 4... */
      hspots[2][2] = '+';
      hspots[boardsize-3][2] = '+';
      hspots[2][boardsize-3] = '+';
      hspots[boardsize-3][boardsize-3] = '+';

      /* ...and the middle one. */
      hspots[boardsize/2][boardsize/2] = '+';
    }
    else {
      /* Place the outer 4... */
      hspots[3][3] = '+';
      hspots[boardsize-4][3] = '+';
      hspots[3][boardsize-4] = '+';
      hspots[boardsize-4][boardsize-4] = '+';

      /* ...and the inner 4... */
      hspots[3][boardsize/2] = '+';
      hspots[boardsize/2][3] = '+';
      hspots[boardsize/2][boardsize-4] = '+';
      hspots[boardsize-4][boardsize/2] = '+';

      /* ...and the middle one. */
      hspots[boardsize/2][boardsize/2] = '+';
    }
  }

  return;
}


/*
 * Display the board position when playing in ASCII.
 */

static void
ascii_showboard(void)
{
  int i, j;
  char letterbar[64];
  int last_pos_was_move;
  int pos_is_move;
  int dead;
  
  make_letterbar(board_size, letterbar);
  set_handicap_spots(board_size);

  printf("\n");
  printf("    White has captured %d pieces\n", black_captured);
  printf("    Black has captured %d pieces\n", white_captured);
  if (showscore) {
    if (current_score_estimate == NO_SCORE)
      printf("    No score estimate is available yet.\n");
    else if (current_score_estimate < 0)
      printf("    Estimated score: Black is ahead by %d\n",
	     -current_score_estimate);
    else if (current_score_estimate > 0)
      printf("    Estimated score: White is ahead by %d\n",
	     current_score_estimate);
    else
      printf("    Estimated score: Even!\n");
  }
   
  printf("\n");

  fflush(stdout);
  printf("%s%s\n", (emacs ? "EMACS1\n" : ""), letterbar);
  fflush(stdout);
  
  for (i = 0; i < board_size; i++) {
    printf(" %2d", board_size - i);
    last_pos_was_move = 0;
    for (j = 0; j < board_size; j++) {
      if (last_move_i == i && last_move_j == j)
	pos_is_move = 128;
      else
	pos_is_move = 0;
      dead = (dragon_status(POS(i, j))==DEAD) && showdead;
      switch (BOARD(i, j) + pos_is_move + last_pos_was_move) {
	case EMPTY+128:
	case EMPTY:
	  printf(" %c", hspots[i][j]);
	  last_pos_was_move = 0;
	  break;
	case BLACK:
	  printf(" %c", dead ? 'x' : 'X');
	  last_pos_was_move = 0;
	  break;
	case WHITE:
	  printf(" %c", dead ? 'o' : 'O');
	  last_pos_was_move = 0;
	  break;
	case BLACK+128:
	  printf("(%c)", 'X');
	  last_pos_was_move = 256;
	  break;
	case WHITE+128:
	  printf("(%c)", 'O');
	  last_pos_was_move = 256;
	  break;
	case EMPTY+256:
	  printf("%c", hspots[i][j]);
	  last_pos_was_move = 0;
	  break;
	case BLACK+256:
	  printf("%c", dead ? 'x' : 'X');
	  last_pos_was_move = 0;
	  break;
	case WHITE+256:
	  printf("%c", dead ? 'o' : 'O');
	  last_pos_was_move = 0;
	  break;
	default: 
	  fprintf(stderr, "Illegal board value %d\n", (int) BOARD(i, j));
	  exit(EXIT_FAILURE);
	  break;
      }
    }
    
    if (last_pos_was_move == 0) {
      if (board_size > 10)
	printf(" %2d", board_size - i);
      else
	printf(" %1d", board_size - i);
    }
    else {
      if (board_size > 10)
	printf("%2d", board_size - i);
      else
	printf("%1d", board_size - i);
    }
    printf("\n");
  }
  
  fflush(stdout);
  printf("%s\n\n", letterbar);
  fflush(stdout);
  
}  /* end ascii_showboard */

/*
 * command help
 */

static void
show_commands(void)
{
  printf("\nCommands:\n");
  printf(" back                Take back your last move\n");
  printf(" boardsize           Set boardsize (on move 1 only)\n");
  printf(" comment             Write a comment to outputfile\n");
  printf(" depth <num>         Set depth for reading\n");
  printf(" display             Display game board\n");
  printf(" exit                Exit GNU Go\n");
  printf(" force <move>        Force a move for current color\n");
  printf(" forward             Go to next node in game tree\n");
  printf(" goto <movenum>      Go to movenum in game tree\n");
  printf(" level <amount>      Playing level (default = 10)\n");
  printf(" handicap <num>      Set fixed handicap (on move 1 only)\n");
  printf(" freehandicap <num>  Place free handicap (on move 1 only)\n");
  printf("                     Omit <num> to place handicap yourself\n");
  printf(" help                Display this help menu\n");
  printf(" helpdebug           Display debug help menu\n");
  printf(" info                Display program settings\n");
  printf(" komi                Set komi (on move 1 only)\n");
  printf(" last                Goto last node in game tree\n");
  printf(" pass                Pass on your move\n");
  printf(" play <num>          Play <num> moves\n");
  printf(" playblack           Play as Black (switch if White)\n");
  printf(" playwhite           Play as White (switch if Black)\n");
  printf(" quit                Exit GNU Go\n");
  printf(" resign              Resign the current game\n");
  printf(" save <file>         Save the current game\n");
  printf(" load <file>         Load a game from file\n");
  printf(" score               Toggle display of score On/Off\n");
  printf(" showboard           Toggle display of board On/Off\n");
  printf(" switch              Switch the color you are playing\n");
  printf(" undo                Take the last move back (same as back)\n");
  printf(" <move>              A move of the format <letter><number>");
  printf("\n");
}

enum commands {INVALID=-1, END, EXIT, QUIT, RESIGN, 
	       PASS, MOVE, FORCE, SWITCH,
	       PLAY, PLAYBLACK, PLAYWHITE,
	       SETHANDICAP, SETBOARDSIZE, SETKOMI,
	       SETDEPTH,
               INFO, DISPLAY, SHOWBOARD, HELP, UNDO, COMMENT, SCORE,
               CMD_DEAD, CMD_BACK, CMD_FORWARD, CMD_LAST,
               CMD_CAPTURE, CMD_DEFEND,
               CMD_HELPDEBUG, CMD_SHOWAREA, CMD_SHOWMOYO, CMD_SHOWTERRI,
               CMD_GOTO, CMD_SAVE, CMD_LOAD, CMD_SHOWDRAGONS, CMD_LISTDRAGONS,
	       SETHURRY, SETLEVEL, NEW, COUNT, FREEHANDICAP
};


/*
 * Check if we have a valid command.
 */

static int
get_command(char *command)
{
  char c;
  int d;

  /* Check to see if a move was input. */
  if (!((sscanf(command, "%c%d", &c, &d) != 2)
	|| ((c = toupper((int) c)) < 'A')
	|| ((c = toupper((int) c)) > 'Z')
	|| (c == 'I')))
    return MOVE;

  /* help shortcut */
  if (command[0] == '?')
    return HELP;

  /* Kill leading spaces. */
  while (command[0] == ' ')
    command++;

  if (!strncmp(command, "playblack", 9)) return PLAYBLACK;
  if (!strncmp(command, "playwhite", 9)) return PLAYWHITE;
  if (!strncmp(command, "showboard", 9)) return SHOWBOARD;
  if (!strncmp(command, "showdragons", 9)) return CMD_SHOWDRAGONS;
  if (!strncmp(command, "listdragons", 9)) return CMD_LISTDRAGONS;
  if (!strncmp(command, "boardsize", 9)) return SETBOARDSIZE;
  if (!strncmp(command, "freehandicap", 9)) return FREEHANDICAP;
  if (!strncmp(command, "handicap", 5)) return SETHANDICAP;
  if (!strncmp(command, "display", 7)) return DISPLAY;
  if (!strncmp(command, "helpdebug", 7)) return CMD_HELPDEBUG;
  if (!strncmp(command, "resign", 6)) return RESIGN;
  if (!strncmp(command, "showmoyo", 6)) return CMD_SHOWMOYO;
  if (!strncmp(command, "showterri", 6)) return CMD_SHOWTERRI;
  if (!strncmp(command, "showarea", 6)) return CMD_SHOWAREA;
  if (!strncmp(command, "depth", 5)) return SETDEPTH;
  if (!strncmp(command, "switch", 5)) return SWITCH;
  if (!strncmp(command, "komi", 4)) return SETKOMI;
  if (!strncmp(command, "play", 4)) return PLAY;
  if (!strncmp(command, "info", 4)) return INFO;
  if (!strncmp(command, "force", 4)) return FORCE;
  if (!strncmp(command, "hurry", 5)) return SETHURRY;
  if (!strncmp(command, "level", 5)) return SETLEVEL;
  if (!strncmp(command, "pass", 4)) return PASS;
  if (!strncmp(command, "save", 3)) return CMD_SAVE;
  if (!strncmp(command, "load", 3)) return CMD_LOAD;
  if (!strncmp(command, "end", 3)) return END;
  if (!strncmp(command, "move", 3)) return MOVE;
  if (!strncmp(command, "undo", 3)) return UNDO;
  if (!strncmp(command, "comment", 3)) return COMMENT;
  if (!strncmp(command, "score", 3)) return SCORE;
  if (!strncmp(command, "dead", 3)) return CMD_DEAD;
  if (!strncmp(command, "capture", 3)) return CMD_CAPTURE;
  if (!strncmp(command, "defend", 3)) return CMD_DEFEND;
  if (!strncmp(command, "exit", 4)) return EXIT;
  if (!strncmp(command, "quit", 4)) return QUIT;
  if (!strncmp(command, "help", 1)) return HELP;
  if (!strncmp(command, "back", 2)) return CMD_BACK;
  if (!strncmp(command, "forward", 2)) return CMD_FORWARD;
  if (!strncmp(command, "last", 2)) return CMD_LAST;
  if (!strncmp(command, "goto", 2)) return CMD_GOTO;
  if (!strncmp(command, "game", 2)) return NEW;
  if (!strncmp(command, "count", 2)) return COUNT;

  /* No valid command was found. */
  return INVALID;
}


/*
 * Write sgf root node. 
 */

static void
init_sgf(Gameinfo *ginfo)
{
  if (sgf_initialized)
    return;
  sgf_initialized = 1;

  sgf_write_header(sgftree.root, 1, random_seed, komi, level, chinese_rules);
  sgfOverwritePropertyInt(sgftree.root, "HA", ginfo->handicap);
  if (ginfo->handicap > 0)
    gnugo_recordboard(sgftree.root);
}


/*
 * Generate the computer move. 
 */

static void
computer_move(Gameinfo *gameinfo, int *passes)
{
  int i, j;
  int move_val;

  init_sgf(gameinfo);
  
  /* Generate computer move. */
  move_val = gnugo_genmove(&i, &j, gameinfo->to_move);
  if (showscore) {
    gnugo_estimate_score(&lower_bound, &upper_bound);
    current_score_estimate = (int) ((lower_bound + upper_bound) / 2.0);
  }
    
  last_move_i = i;
  last_move_j = j;
  
  mprintf("%s(%d): %m\n", color_to_string(gameinfo->to_move),
	  movenum+1, i, j);
  if (is_pass(POS(i, j)))
    (*passes)++;
  else
    *passes = 0;

  gnugo_play_move(i, j, gameinfo->to_move);
  sgffile_add_debuginfo(sgftree.lastnode, move_val);
  sgftreeAddPlay(&sgftree, gameinfo->to_move, i, j);
  sgffile_output(&sgftree);

  gameinfo->to_move = OTHER_COLOR(gameinfo->to_move);
}


/*
 * Make a move.
 */

static void
do_move(Gameinfo *gameinfo, char *command, int *passes, int force)
{
  int i, j;

  if (!string_to_location(board_size, command, &i, &j)) {
    printf("\nInvalid move: %s\n", command);
    return;
  }
  
  if (!gnugo_is_legal(i, j, gameinfo->to_move)) {
    printf("\nIllegal move: %s", command);
    return;
  }

  *passes = 0;
  TRACE("\nyour move: %m\n\n", i, j);
  init_sgf(gameinfo);
  gnugo_play_move(i, j, gameinfo->to_move);
  sgffile_add_debuginfo(sgftree.lastnode, 0);
  sgftreeAddPlay(&sgftree, gameinfo->to_move, i, j);
  sgffile_output(&sgftree);

  last_move_i = i;
  last_move_j = j;
  
  if (opt_showboard && !emacs) {
    ascii_showboard();
    printf("GNU Go is thinking...\n");
  }
  if (force) {
    gameinfo->computer_player = OTHER_COLOR(gameinfo->computer_player);
    gameinfo->to_move = OTHER_COLOR(gameinfo->to_move);
    sgftreeAddComment(&sgftree, "forced");
    return;
  }
  gameinfo->to_move = OTHER_COLOR(gameinfo->to_move);
  computer_move(gameinfo, passes);
}


/*
 * Make a pass.
 */

static void
do_pass(Gameinfo *gameinfo, int *passes, int force)
{
  (*passes)++;
  init_sgf(gameinfo);
  gnugo_play_move(-1, -1, gameinfo->to_move);
  sgffile_add_debuginfo(sgftree.lastnode, 0);
  sgftreeAddPlay(&sgftree, gameinfo->to_move, -1, -1);
  sgffile_output(&sgftree);

  gameinfo->to_move = OTHER_COLOR(gameinfo->to_move);
  if (force) {
    gameinfo->computer_player = OTHER_COLOR(gameinfo->computer_player);
    sgftreeAddComment(&sgftree, "forced");
    return;
  }
  computer_move(gameinfo, passes);
}


/*
 * Play a game against an ascii client.
 */

void
play_ascii(SGFTree *tree, Gameinfo *gameinfo, char *filename, char *until)
{
  int m, num;
  int sz;
  float fnum;
  int passes = 0;  /* two passes and its over */
  int tmp;
  char line[80];
  char *line_ptr = line;
  char *command;
  char *tmpstring;
  int state = 1;
  
  setbuf(stdout, NULL); /* else set it to completely UNBUFFERED */
  
  sgftree = *tree;

  if (filename) {
    gameinfo_load_sgfheader(gameinfo, sgftree.root);
    gameinfo->to_move = gameinfo_play_sgftree(gameinfo, &sgftree, until);
    sgf_initialized = 1;
  }
  else {
    if (sgfGetIntProperty(sgftree.root, "SZ", &sz)) 
      gnugo_clear_board(sz);
    if (gameinfo->handicap == 0)
      gameinfo->to_move = BLACK;
    else {
      gameinfo->handicap = gnugo_placehand(gameinfo->handicap);
      gameinfo->to_move = WHITE;
    }
    sgf_initialized = 0;
  }

  while (state == 1) {
    /* No score is estimated yet. */
    current_score_estimate = NO_SCORE;
    
    printf("\nBeginning ASCII mode game.\n\n");
    gameinfo_print(gameinfo);
    
    /* Does the computer play first?  If so, make a move. */
    if (gameinfo->computer_player == gameinfo->to_move)
      computer_move(gameinfo, &passes);
    
    /* main ASCII Play loop */
    while (passes < 2) {
      /* Display game board. */
      if (opt_showboard)
	ascii_showboard();
      
      /* Print the prompt */
      mprintf("%s(%d): ", color_to_string(gameinfo->to_move), movenum + 1);

      /* Read a line of input. */
      line_ptr = line;
      if (!fgets(line, 80, stdin)) {
	printf("\nThanks! for playing GNU Go.\n\n");
	return ;
      }      
      while (command = strtok(line_ptr, ";"), line_ptr = 0, command) {
	
	/* Get the command or move. */
	switch (get_command(command)) {
	case RESIGN:
	  printf("\nGNU Go wins by resignation.");
	  sgftreeWriteResult(&sgftree,
			     gameinfo->to_move == WHITE ? -1000.0 : 1000.0,
			     1);
          sgffile_output(&sgftree);
	case END:
	case EXIT:
	case QUIT:
	  printf("\nThanks! for playing GNU Go.\n\n");
	  return ;
	  break;
	case HELP:
	  show_commands();
	  break;
	case CMD_HELPDEBUG:
	  printf(DEBUG_COMMANDS);
	  break;
	case SHOWBOARD:
	  opt_showboard = !opt_showboard;
	  break;
	case INFO:
	  printf("\n");
	  gameinfo_print(gameinfo);
	  break;
	case SETBOARDSIZE:
	  if (sgf_initialized) {
	    printf("Boardsize cannot be changed after record is started!\n");
	    break;
	  }
	  command += 10;
	  if (sscanf(command, "%d", &num) != 1) {
	    printf("\nInvalid command syntax!\n");
	    break;
	  }
	  if (num < MIN_BOARD || num > MAX_BOARD) {
	    printf("\nInvalid board size: %d\n", num);
	    break;
	  }
	  sz = num;
	  /* Init board. */
	  gnugo_clear_board(sz);
	  /* In case max handicap changes on smaller board. */
	  gameinfo->handicap = gnugo_placehand(gameinfo->handicap);
	  sgfOverwritePropertyInt(sgftree.root, "SZ", sz);
	  sgfOverwritePropertyInt(sgftree.root, "HA", gameinfo->handicap);
	  break;
	case SETHANDICAP:
	  if (sgf_initialized) {
	    printf("Handicap cannot be changed after game is started!\n");
	    break;
	  }
	  command += 9;
	  if (sscanf(command, "%d", &num) != 1) {
	    printf("\nInvalid command syntax!\n");
	    break;
	  }
	  if (num < 0 || num > MAX_HANDICAP) {
	    printf("\nInvalid handicap: %d\n", num);
	    break;
	  }
	  /* Init board. */
	  gnugo_clear_board(board_size);
	  /* Place stones on board but don't record sgf 
	   * in case we change more info. */
	  gameinfo->handicap = gnugo_placehand(num);
	  printf("\nSet handicap to %d\n", gameinfo->handicap);
          gameinfo->to_move = (gameinfo->handicap ? WHITE : BLACK);
	  break;
	case FREEHANDICAP:
	  if (sgf_initialized) {
	    printf("Handicap cannot be changed after game is started!\n");
	    break;
	  }
	  while (*command && *command != ' ')
	    command++;
	  ascii_free_handicap(gameinfo, command);
	  break;
	case SETKOMI:
	  if (sgf_initialized) {
	    printf("Komi cannot be modified after game record is started!\n");
	    break;
	  }
	  command += 5;
	  if (sscanf(command, "%f", &fnum) != 1) {
	    printf("\nInvalid command syntax!\n");
	    break;
	  }
	  komi = fnum;
	  printf("\nSet Komi to %.1f\n", komi);
	  break;
	case SETDEPTH:
	  {
	    command += 6;
	    if (sscanf(command, "%d", &num) != 1) {
	      printf("\nInvalid command syntax!\n");
	      break;
	    }
	    mandated_depth = num;
	    printf("\nSet depth to %d\n", mandated_depth);
	    break;
	  }
	case SETLEVEL:
	  {
	    command += 6;
	    if (sscanf(command, "%d", &num) != 1) {
	      printf("\nInvalid command syntax!\n");
	      break;
	    }
	    level = num;
	    printf("\nSet level to %d\n", level);
	    break;
	  }
	  /* Level replaces hurry as of 2.7.204. This option is retained
	   * for compatibility with gnugoclient. 
	   */
	case SETHURRY:
	  {
	    command += 6;
	    if (sscanf(command, "%d", &num) != 1) {
	      printf("\nInvalid command syntax!\n");
	      break;
	    }
	    level = 10 - num;
	    printf("\nSet hurry to %d\n", 10 - level);
	    break;
	  }
	case DISPLAY:
	  if (!opt_showboard)
	    ascii_showboard();
	  break;
	case FORCE:
	  command += 6; /* skip the force part... */
	  switch (get_command(command)) {
	  case MOVE:
	    do_move(gameinfo, command, &passes, 1);
	    break;
	  case PASS:
	    do_pass(gameinfo, &passes, 1);
	    break;
	  default:
	    printf("Illegal forced move: %s %d\n", command,
		   get_command(command));
	    break;
	  }
	  break;
	case MOVE:
	  do_move(gameinfo, command, &passes, 0);
	  break;
	case PASS:
	  do_pass(gameinfo, &passes, 0);
	  break;
	case PLAY:
	  command += 5;
	  if (sscanf(command, "%d", &num) != 1) {
	    printf("\nInvalid command syntax!\n");
	    break;
	  }
	  if (num >= 0)
	    for (m = 0; m < num; m++) {
	      gameinfo->computer_player 
		= OTHER_COLOR(gameinfo->computer_player);
	      computer_move(gameinfo, &passes);
	      if (passes >= 2)
		break;
	    }
	  else {
	    printf("\nInvalid number of moves specified: %d\n", num);
	    break;
	  }
	  break;
	case PLAYBLACK:
	  if (gameinfo->computer_player == WHITE)
	    gameinfo->computer_player = BLACK;
	  if (gameinfo->computer_player == gameinfo->to_move)
	    computer_move(gameinfo, &passes);
	  break;
	case PLAYWHITE:
	  if (gameinfo->computer_player == BLACK)
	    gameinfo->computer_player = WHITE;
	  if (gameinfo->computer_player == gameinfo->to_move)
	    computer_move(gameinfo, &passes);
	  break;
	case SWITCH:
	  gameinfo->computer_player = OTHER_COLOR(gameinfo->computer_player);
	  computer_move(gameinfo, &passes);
	  break;
	case UNDO:
	case CMD_BACK:
	  if (gnugo_undo_move(1)) {
            sgftreeAddComment(&sgftree, "undone");
	    sgftreeBack(&sgftree);
	    gameinfo->to_move = OTHER_COLOR(gameinfo->to_move);
	  }
	  else
	    printf("\nCan't undo.\n");
	  break;
	case CMD_FORWARD:
         if (sgftreeForward(&sgftree))
           gameinfo->to_move = gnugo_play_sgfnode(sgftree.lastnode,
						  gameinfo->to_move);
	  else
	    printf("\nEnd of game tree.\n");
	  break;
	case CMD_LAST:
         while (sgftreeForward(&sgftree))
           gameinfo->to_move = gnugo_play_sgfnode(sgftree.lastnode,
						  gameinfo->to_move);
	  break;
	case COMMENT:
	  printf("\nEnter comment. Press ENTER when ready.\n");
	  fgets(line, 80, stdin);
	  sgftreeAddComment(&sgftree, line);
	  break;
	case SCORE:
	  showscore = !showscore;
	  if (!showscore)
	    current_score_estimate = NO_SCORE;
	  break;
	case CMD_DEAD:
	  showdead = !showdead;
	  break;
	case CMD_CAPTURE:
	  strtok(command, " ");
	  showcapture(strtok(NULL, " "));
	  break;
	case CMD_DEFEND:
	  strtok(command, " ");
	  showdefense(strtok(NULL, " "));
	  break;
	case CMD_SHOWMOYO:
	  tmp = printmoyo;
	  printmoyo = PRINTMOYO_MOYO;
	  examine_position(gameinfo->to_move, EXAMINE_DRAGONS);
	  print_moyo();
	  printmoyo = tmp;
	  break;
	case CMD_SHOWTERRI:
	  tmp = printmoyo;
	  printmoyo = PRINTMOYO_TERRITORY;
	  examine_position(gameinfo->to_move, EXAMINE_DRAGONS);
	  print_moyo();
	  printmoyo = tmp;
	  break;
	case CMD_SHOWAREA:
	  tmp = printmoyo;
	  printmoyo = PRINTMOYO_AREA;
	  examine_position(gameinfo->to_move, EXAMINE_DRAGONS);
	  print_moyo();
	  printmoyo = tmp;
	  break;
	case CMD_SHOWDRAGONS:
	  examine_position(gameinfo->to_move, EXAMINE_DRAGONS);
	  showboard(1);
	  break;
	case CMD_GOTO:
	  strtok(command, " ");
	  ascii_goto(gameinfo, strtok(NULL, " "));
	  break;
	case CMD_SAVE:
	  strtok(command, " ");
	  tmpstring = strtok(NULL, " ");
	  if (tmpstring) {
	    /* discard newline */
	    tmpstring[strlen(tmpstring)-1] = 0;
	    /* make sure we are saving proper handicap */
	    init_sgf(gameinfo);
	    writesgf(sgftree.root, tmpstring);
	    printf("You may resume the game");
	    printf(" with -l %s --mode ascii\n", tmpstring);
	    printf("or load %s\n", tmpstring);
	  }
	  else
	    printf("Please specify filename\n");
	  break;
	case CMD_LOAD:
	  strtok(command, " ");
	  tmpstring = strtok(NULL, " ");
	  if (tmpstring) {
	    /* discard newline */
	    tmpstring[strlen(tmpstring)-1] = 0;
	    if (!sgftree_readfile(&sgftree, tmpstring)) {
	      fprintf(stderr, "Cannot open or parse '%s'\n", tmpstring);
	      break;
	    }
            /* to avoid changing handicap etc. */
            sgf_initialized = 1;
            gameinfo_load_sgfheader(gameinfo, sgftree.root);
            gameinfo_play_sgftree(gameinfo, &sgftree, NULL);
	    sgfOverwritePropertyInt(sgftree.root, "HA", gameinfo->handicap);
	  }
	  else
	    printf("Please specify a filename\n");
	  break;

	case CMD_LISTDRAGONS:
	  examine_position(gameinfo->to_move, EXAMINE_DRAGONS);
	  show_dragons();
	  break;
	case COUNT:
	case NEW:
	case INVALID:
	default:
	  printf("\nInvalid command: %s", command);
	  break;
	}
      }
    }
    
    /* two passes : game over */
    
    if (passes >= 2)
      gnugo_who_wins(gameinfo->computer_player, stdout);
    printf("\nIf you disagree, we may count the game together.\n");
    printf("You may optionally save the game as an SGF file.\n");

    sgftreeWriteResult(&sgftree, estimate_score(NULL, NULL), 1);

    state = 0;
    while (state == 0) {
      printf("\n");
      printf("Type \"save <filename>\" to save,\n");
      printf("     \"count\" to recount,\n");
      printf("     \"quit\" to quit\n");
      printf(" or  \"game\" to play again\n");
      line_ptr = line;
      if (!fgets(line, 80, stdin))
	break;
      command = strtok(line_ptr, "");
      switch (get_command(command)) {
      case CMD_SAVE:
	strtok(command, " ");
	tmpstring = strtok(NULL, " ");
	if (tmpstring) {
	  /* discard newline */
	  tmpstring[strlen(tmpstring)-1] = 0;
          init_sgf(gameinfo);
	  writesgf(sgftree.root, tmpstring);
	}
	else
	  printf("Please specify filename\n");
	break;
	
      case NEW:
	state = 1;
	break;
	
      case COUNT:
	endgame(gameinfo);
	break;

      case QUIT:
	state = 2;
	break;
	
      default:
	state = 0;
      }
    }
    sgffile_output(&sgftree);
    passes = 0;
    
    /* Play a different game next time. */
    update_random_seed();

    /* Free the sgf tree and prepare for a new game. */
    sgfFreeNode(sgftree.root);
    sgftree_clear(&sgftree);
    sgftreeCreateHeaderNode(&sgftree, board_size, komi);
    sgf_initialized = 0;

    gameinfo_clear(gameinfo, board_size, komi);
  }
  printf("\nThanks for playing GNU Go.\n\n");
}

void
play_ascii_emacs(SGFTree *tree, Gameinfo *gameinfo,
		 char *filename, char *until)
{
  emacs = 1;
  play_ascii(tree, gameinfo, filename, until);
}


/*
 * endgame() scores the game.
 */

static void
endgame(Gameinfo *gameinfo)
{
  char line[12];
  int done = 0;
  int i, j;
  int xyzzy = 1;
  
  printf("\nGame over. Let's count!.\n");  
  showdead = 1;
  ascii_showboard();
  while (!done) {
    printf("Dead stones are marked with small letters (x,o).\n");
    printf("\nIf you don't agree, enter the location of a group\n");
    printf("to toggle its state or \"done\".\n");

    if (!fgets(line, 12, stdin))
      return; /* EOF or some error */
    
    for (i = 0; i < 12; i++)
      line[i] = (isupper ((int) line[i]) ? tolower ((int) line[i]) : line[i]);
    if (!strncmp(line, "done", 4))
      done = 1;
    else if (!strncmp(line, "quit", 4))
      return;
    else if (!strncmp(line, "xyzzy", 5)) {
      if (xyzzy) {
	printf("\nYou are in a debris room filled with stuff washed in from the\n");
	printf("surface.  A low wide passage with cobbles becomes plugged\n");
	printf("with mud and debris here, but an awkward canyon leads\n");
	printf("upward and west.  A note on the wall says:\n");
	printf("   Magic Word \"XYZZY\"\n\n");
	xyzzy = 0;
      }
      else {
	printf("You are inside a building, a well house for a large spring.\n\n");
	xyzzy = 1;
      }
    }
    else if (!strncmp(line, "help", 4)) {
      printf("\nIf there are dead stones on the board I will remove them.\n");
      printf("You must tell me where they are. Type the coordinates of a\n");
      printf("dead group, or type \"done\"\n");
    }      
    else if (!strncmp(line, "undo", 4)) {
      printf("UNDO not allowed anymore. The status of the stones now\n");
      printf("toggles after entering the location of it.\n");
      ascii_showboard();
    }
    else {
      if (!string_to_location(board_size, line, &i, &j)
	  || BOARD(i, j) == EMPTY)
	printf("\ninvalid!\n");
      else {
	int status = dragon_status(POS(i, j));
	status = (status == DEAD) ? ALIVE : DEAD;
	change_dragon_status(POS(i, j), status);
	ascii_showboard();
      }
    }
  }
  gnugo_who_wins(gameinfo->computer_player, stdout);
}


static void
showcapture(char *line)
{
  int i, j, x, y;
  if (line)
    if (!string_to_location(board_size, line, &i, &j)
	|| BOARD(i, j) == EMPTY) {
      printf("\ninvalid point!\n");
      return;
    }
  
  if (gnugo_attack(i, j, &x, &y))
    mprintf("\nSuccessfull attack of %m at %m\n", i, j, x, y);
  else
    mprintf("\n%m cannot be attacked\n", i, j);
}


static void
showdefense(char *line)
{
  int i, j, x, y;
  if (line)
    if (!string_to_location(board_size, line, &i, &j)
	|| BOARD(i, j) == EMPTY) {
      printf("\ninvalid point!\n");
      return;
    }

    if (gnugo_attack(i, j, &x, &y)) {
      if (gnugo_find_defense(i, j, &x, &y))
        mprintf("\nSuccessfull defense of %m at %m\n", i, j, x, y);
      else
        mprintf("\n%m cannot be defended\n", i, j);
    }
    else
      mprintf("\nThere is no need to defend %m\n", i, j);
}


static void
ascii_goto(Gameinfo *gameinfo, char *line)
{
  const char *movenumber = line;

  if (!line)
    return;
  
  if (!strncmp(line, "last", 4))
    movenumber = "9999";
  else if (!strncmp(line, "first", 4))
    movenumber = "1";
  
  printf("goto %s\n", movenumber);
  gameinfo_play_sgftree(gameinfo, &sgftree, movenumber);
}


static void
ascii_free_handicap(Gameinfo *gameinfo, char *handicap)
{
  int handi;
  int i;
  char line[80];
  int stones[MAX_BOARD*MAX_BOARD];
  int x, y, pos;

  if (sscanf(handicap, "%d", &handi) == 1) {
    /* Gnu Go is to place handicap */
    if (handi < 0 || handi == 1) {
      printf("\nInvalid command syntax!\n");
      return;
    }

    gnugo_clear_board(board_size);
    handi = place_free_handicap(handi);
    printf("\nPlaced %d stones of free handicap.\n", handi);
  }
  else { /* User is to place handicap */
    gnugo_clear_board(board_size);
    handi = 0;

    while (1) {
      ascii_showboard();
      printf("\nType in coordinates of next handicap stone, or one of the following commands:\n");
      printf("  undo        take back the last stone placed\n");
      printf("  clear       remove all the stones placed so far\n");
      printf("  done        finish placing handicap\n\n");
      printf("You have placed %d handicap stone(s) so far.\n\n", handi);

      if (!fgets(line, 80, stdin))
        return; /* EOF or some error */
      for (i = 0; i < 80; i++)
        line[i] = (isupper ((int) line[i]) ? tolower ((int) line[i]) : line[i]);

      if (!strncmp(line, "undo", 4)) {
        if (!handi)
	  printf("\nNothing to undo.\n");
	else {
	  remove_stone(stones[--handi]);
	  gprintf("\nRemoved the stone at %m.\n", I(stones[handi]),
		  J(stones[handi]));
	}
      }
      else if (!strncmp(line, "clear", 5)) {
        gnugo_clear_board(board_size);
        handi = 0;
      }
      else if (!strncmp(line, "done", 4)) {
	if (handi == 1) /* Don't bother with checks later */
	  printf ("\nHandicap cannot be one stone. Either add "
		  "some more, or delete the only stone.\n");
	else
	  break;
      }
      else if (string_to_location(board_size, line, &x, &y)) {
	pos = POS(x,y);
	if (board[pos] != EMPTY)
	  printf("\nThere's already a stone there.\n");
	else {
	  add_stone(pos, BLACK);
	  stones[handi++] = pos;
	}
      }
      else
	printf("\nInvalid command: %s", line);
    }
  }
  gameinfo->handicap = handi;
  gameinfo->to_move = (handi ? WHITE : BLACK);
}


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
