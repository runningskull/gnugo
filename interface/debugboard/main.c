/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * This is GNU Go, a Go program. Contact gnugo@gnu.org, or see       *
 * http://www.gnu.org/software/gnugo/ for more information.          *
 *                                                                   *
 * Copyright 1999, 2000, 2001, 2002 by the Free Software Foundation. *
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

#include <stdio.h>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef CURSES

#include <unistd.h>

#if TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# if HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  include <time.h>
# endif
#endif

#ifdef __MINGW32__
#include <windows.h>
#include <winsock.h>
#include <io.h>
#endif

#ifdef HAVE_VISUAL_C
#include <winsock.h>
#include <io.h>
#endif

#include "gnugo.h"
#include "sgftree.h"
#include "interface.h"

#include "display.h"
#include "gui.h"



Tab tabs[] = {
  {"Worms"},
  {"Dragons"},
  {" Eyes "},
  {" Help "},
  {""},
};


Gameinfo  gameinfo;
SGFTree   sgftree;

int       current_color = WHITE;

static void main_loop(void);

void choose_worms_tab(void);
void choose_dragons_tab(void);
void choose_eyes_tab(void);
void choose_help_tab(void);

void show_current_info(void);


int
main(int argc, char *argv[])
{
  char *until = NULL;
  int  next;
  char *infilename;

  /* Examine options. */
  if (argc == 1 || argc > 3 ) {
    fprintf(stderr, "Usage: %s infile [movenum]\n", argv[0]);
    exit(1);
  }

  /* Try to open the infile. */
  infilename = argv[1];
  sgftree_clear(&sgftree);
  if (!sgftree_readfile(&sgftree, infilename)) {
    fprintf(stderr, "Cannot open or parse '%s'\n", infilename);
    exit(1);
  }

  if (argc == 3) {
    until = argv[2];
  }

  /* FIXME: Want to do this somewhere more structured. */
  debug = 0;
  level = 10;
  fusekidb = 1;
  josekidb = 1;
  
  /* Check if there is enough screen space. */
  if (!display_init(MINWIDTH, MINHEIGHT, board_size))
    exit(1);

  /* Initialize the GNU Go engine. */
  init_gnugo(8, time(0));

  gameinfo_clear(&gameinfo, 19, 5.5);
  next = gameinfo_play_sgftree(&gameinfo, &sgftree, until);


  reset_engine();

  display_message("Hit 'h' for help!");

  tab_window_draw(tabs);

  display_board_position(NULL);
  gnugo_examine_position(next, EXAMINE_ALL);

  choose_worms_tab();
  main_loop();
  display_cleanup();

  exit(0);
}

static void
main_loop()
{
  int  ch;

  while (1) {
    ch = display_input();
    if (ch == -1) {
#ifdef HAVE_USLEEP
      usleep(100000);
#else
      struct timeval timeout;
      timeout.tv_sec = 0;
      timeout.tv_usec = 100000;
      select(0, NULL, NULL, NULL, &timeout);
#endif
      continue;
    }

    if (ch ==  'q' || ch ==  'Q')
      break;

    switch (ch) {
    /* ---- Tabs ---- */
    case 'w': /* Worms */
      choose_worms_tab();
      show_current_info();
      break;

    case 'd': /* Dragons */
    case 'D':
      choose_dragons_tab();
      show_current_info();
      break;

    case 'e': /* Eyes */
    case 'E':
      choose_eyes_tab();
      show_current_info();
      break;

      /* ---- White or black ---- */
    case 'W':
      current_color = WHITE;
      show_current_info();
      break;
      
    case 'B':
      current_color = BLACK;
      show_current_info();
      break;
      
      /* ---- Movement ---- */
      /* LEFT */  
      
    case 2:                  /* CTRL-B */
    case '4':                /* Or use numeric keypad 4. */
#ifdef HAVE_CURSES_ARROWS
    case KEY_LEFT:
#endif
      display_board_move(0, -1);

      show_current_info();
      break;

      /* RIGHT */  
    case 6:                  /* CTRL-F */
    case '6':                /* Or use numeric keypad 6 */
#ifdef HAVE_CURSES_ARROWS
    case KEY_RIGHT:
#endif
      display_board_move(0, 1);
      show_current_info();
      break;

     /* UP */  

    case 16:                 /* CTRL-P */
    case '8':                /* Or use numeric keypad 8 */
#ifdef HAVE_CURSES_ARROWS
    case KEY_UP:
#endif
      display_board_move(-1, 0);
      show_current_info();
      break;

     /* DOWN */  

    case 14:                 /* CTRL-N */
    case '2':                /* Or use numeric keypad 2 */
#ifdef HAVE_CURSES_ARROWS
    case KEY_DOWN:
#endif
      display_board_move(1, 0);
      show_current_info();
      break;

     /* Beginning of line */
     case 1: /* CTRL-A */
       display_board_move(0, -100);
       show_current_info();
       break;
 
     /* End of line */
     case 5: /* CTRL-E */
       display_board_move(0, 100);
       show_current_info();
       break;
  
     /* ---- Miscellaneous ---- */
    case 'l': /* Refresh screen */
    case 'L':
      display_refresh();
      break;

    case '?': /* Help */
    case 'h':
    case 'H':
      choose_help_tab();
      display_help();
      break;

    default:
      display_message("Unkown character (%d) received", ch);
    }
  }
  }



/* ---------------------------------------------------------------- */

void
choose_worms_tab()
{
  tab_window_choose(tabs, 0);
  prepare_worms_tab();
  display_worm(POS(display_cur_row(), display_cur_col()));

  display_board_cursor();
}


void
choose_dragons_tab()
{
  tab_window_choose(tabs, 1);
  prepare_dragons_tab();

  display_board_cursor();
}


void
choose_eyes_tab()
{
  tab_window_choose(tabs, 2);
  prepare_eyes_tab();

  display_board_cursor();
}


void
choose_help_tab()
{
  tab_window_choose(tabs, 3);
  prepare_help_tab();

  display_board_cursor();
}


void
show_current_info()
{
  switch (display_cur_tabchoice()) {
  case 0: 
    display_board_position(display_worm_tactical_data);
    display_worm(POS(display_cur_row(), display_cur_col()));
    break;
  case 1:
    display_board_position(NULL);
    display_dragon(POS(display_cur_row(), display_cur_col()));
    break;
  case 2: 
    display_board_position(NULL);
    if (current_color == WHITE)
      display_eye(WHITE, white_eye, POS(display_cur_row(), display_cur_col()));
    else
      display_eye(BLACK, black_eye, POS(display_cur_row(), display_cur_col()));
  default:
    break;
  }

  display_board_cursor();
}

#else /* !CURSES */

/* Avoid compiler warnings with unused parameters */
#define UNUSED(x)  (void)x

int
main(int argc, char *argv[])
{
  UNUSED(argc);
  UNUSED(argv);
  fprintf(stderr, "This program requires curses. Rebuild with curses and/or check that\nconfigure did find curses successfully.\n");
  return 1;
}

#endif /* CURSES */

/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
