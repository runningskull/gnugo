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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef CURSES

#include <stdio.h>
#include <stdarg.h>
#include <sys/types.h>
#include <string.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <signal.h>
#include <sys/file.h>

#include "display.h"		/* includes <curses.h> */
#include "gnugo.h"
#include "gg_utils.h"


/* Local variables for display.c */
WINDOW   *board_frame_window;
WINDOW   *board_window;
WINDOW   *tab_window;
WINDOW   *info_window;
WINDOW   *message_window;

static Display_data  displaydata;

/* Wrapper around vwprintw.
 *
 * Some curses implementations (one of them is pdcurses)
 * don't have vwprintw.
 */

void
gg_vwprintw(WINDOW *w, const char *format, va_list ap)
{
#if HAVE_VWPRINTW
  vwprintw(w, format, ap);
#else
  char msg[128];
  gg_vsnprintf(msg, 128, format, ap);
  wprintw(w, "%s", msg);
#endif
}

/* Wrapper around wprintw.
 *
 * Most curses implementations don't declare fmt as a
 * const char, but as a plain char. This can lead to a
 * lot of compilation warnings.
 */

void
gg_wprintw (WINDOW *w, const char *format, ... )
{
  va_list args;
  va_start(args,format);
  gg_vwprintw(w, format, args);
  va_end(args);
}

/* ================================================================ */


/*
 * Initialize the curses package, and draw the windows.
 *
 * Return 1 if ok, otherwise return 0.
 */

int
display_init(int minwidth, int minheight, int size)
{

  initscr();
  if ((COLS < minwidth) || (LINES < minheight)) {
    endwin();
    display_cleanup();
    fprintf(stderr, "Not big enough screen to run debugboard.\n");
    fprintf(stderr, "You need at least %d rows and %d columns.\n",
	    minheight, minwidth);
    return 0;
  }

  board_frame_window = newwin(BOARD_FRAME_HEIGHT, minwidth,
			      BOARD_FRAME_MINROW, BOARD_FRAME_MINCOL);

  /* The board itself. */
  board_window = subwin(board_frame_window, size + 2, size * 2 + 1, 
			BOARD_MINROW, BOARD_MINCOL);

  /* The tab window. */
  tab_window = newwin(TAB_HEIGHT, TAB_WIDTH, TAB_MINROW, TAB_MINCOL);
  leaveok(tab_window, TRUE);

  /* The info window. */
  info_window = newwin(INFO_HEIGHT, INFO_WIDTH, INFO_MINROW, INFO_MINCOL);
  leaveok(info_window, TRUE);

  /* The message window. */
  message_window = newwin(MESSAGE_HEIGHT, MESSAGE_WIDTH, 
			  MESSAGE_MINROW, MESSAGE_MINCOL);
  leaveok(message_window, TRUE);

  displaydata.boardsize = size;
  displaydata.cur_row = 0;
  displaydata.cur_col = 0;
  displaydata.cur_tab_choice = 0;
  clear_all_windows();

  /* Arrange for the input. */
  cbreak();
#if HAVE_KEYPAD
  keypad(board_window, TRUE);
#endif
  noecho();

  return 1;
}



/*
 * Check for input from stdin. Return character.
 */

int
display_input() 
{
  return wgetch(board_window);
}



/*
 * Empty the screen and cleanup the curses package.
 */

void
display_cleanup()
{
  clear();
  refresh();
  endwin();
}


/****************************************************************/
/*************** Various printing on the screen *****************/


void
clear_board_frame()
{
  static char labels[] = "ABCDEFGHJKLMNOPQRSTUV";
  int  i;

  /* Draw the board frame. */
  wclear(board_frame_window);
#if 0
  box(board_frame_window, '|', '-');
  mvwaddch(board_frame_window,                  0,            0, '+');
  mvwaddch(board_frame_window,                  0, minwidth - 1, '+');
  mvwaddch(board_frame_window, BOARD_FRAME_HEIGHT,            0, '+');
  mvwaddch(board_frame_window, BOARD_FRAME_HEIGHT, minwidth - 1, '+');
#endif

  for (i = 0; i < displaydata.boardsize; ++i) {
    mvwaddch(board_frame_window, BOARD_MINROW-1, BOARD_MINCOL + 2*i + 1,
	     labels[i]);
    mvwaddch(board_frame_window, 
	     BOARD_MINROW+displaydata.boardsize + 2,
	     BOARD_MINCOL + 2*i + 1, labels[i]);
  }
  for (i = 0; i < displaydata.boardsize; ++i) {
    wmove(board_frame_window,  BOARD_MINROW+1+i, BOARD_MINCOL-3);
    gg_wprintw(board_frame_window, "%2d", displaydata.boardsize-i);

    wmove(board_frame_window,  
	  BOARD_MINROW + 1 + i, 
	  BOARD_MINCOL + 2*displaydata.boardsize + 2);
    gg_wprintw(board_frame_window, "%2d", displaydata.boardsize-i);
  }
  wrefresh(board_frame_window);

  display_board_cursor();
}


void
clear_visual_board()
{
  int  i;
  int  j;

  wclear(board_window);
  box(board_window, '|', '-');

  for (i = 0; i < displaydata.boardsize; ++i)
    for (j = 0; j < displaydata.boardsize; ++j)
      mvwaddch(board_window, i+1, 2*j+1, '.');

  wrefresh(board_window);
}


void
clear_tab_window()
{
  wclear(tab_window);
  wrefresh(tab_window);
}


void
clear_info_window()
{
  wclear(info_window);
  /*  box(info_window, '|', '-');*/
  wrefresh(info_window);
}


void
clear_message_window()
{
  wclear(message_window);
  box(message_window, '|', '-');
  wrefresh(message_window);
}

void
clear_all_windows()
{
  clear_board_frame();
  clear_visual_board();
  clear_tab_window();
  clear_info_window();
  clear_message_window();

  display_board_cursor();
}


/* ================================================================ */
/*                           Board window                           */
/* ================================================================ */


int
display_cur_row()
{
  return displaydata.cur_row;
}

int
display_cur_col()
{
  return displaydata.cur_col;
}

int
display_cur_tabchoice()
{
  return displaydata.cur_tab_choice;
}


void
display_board_char(int i, int j, int ch)
{
  mvwaddch(board_window, i+1, 2*j+1, ch);
}


void
display_board_intersection(int i, int j, int color)
{
  int  ch;

  switch (color) {
  case EMPTY: ch = '.'; break;
  case BLACK: ch = 'X'; break;
  case WHITE: ch = 'O'; break;
  default:    ch = '?'; break;
  }

  mvwaddch(board_window, i+1, 2*j+1, ch);

  wrefresh(board_window);
}

void
display_board_position(display_board_function *func)
{
  int  ch;
  int  i;
  int  j;
  int  board[MAX_BOARD][MAX_BOARD];
  int  boardsize;

  gnugo_get_board(board);
  boardsize = gnugo_get_boardsize();
  
  for (i = 0; i < boardsize; ++i)
    for (j = 0; j < boardsize; ++j) {
      switch (board[i][j]) {
      case EMPTY: ch = '.'; break;
      case BLACK: ch = 'X'; break;
      case WHITE: ch = 'O'; break;
      default:    ch = '?'; break;
      }
      mvwaddch(board_window, i+1, 2*j+1, ch);
    }

  if (func)
    func(display_cur_row(), display_cur_col());

  display_board_cursor();
}


void
display_board_cursor()
{
  wmove(board_window, displaydata.cur_row+1, 2*displaydata.cur_col+1);
  wrefresh(board_window);
}


void
display_board_moveto(int i, int j)
{
  if (i < 0)                       i = 0;
  if (i > displaydata.boardsize-1) i = displaydata.boardsize-1;
  if (j < 0)                       j = 0;
  if (j > displaydata.boardsize-1) j = displaydata.boardsize-1;

  displaydata.cur_row = i;
  displaydata.cur_col = j;

  display_board_cursor();
}


void
display_board_move(int di, int dj)
{
  display_board_moveto(displaydata.cur_row + di, displaydata.cur_col + dj);
}


/* ================================================================ */
/*                            Tab window                            */
/* ================================================================ */


void
tab_window_draw(Tab *tabs)
{
  int  len;
  int  i;
  int  j;

  wmove(tab_window, 0, 1);
  for (i = 0; tabs[i].text[0]; ++i) {
    len = strlen(tabs[i].text);
    gg_wprintw(tab_window, " ");
    for (j = 0; j < len+2; ++j)
      gg_wprintw(tab_window, "_");
    gg_wprintw(tab_window, " ");
  }

  wmove(tab_window, 1, 1);
  for (i = 0; tabs[i].text[0]; ++i) {
    gg_wprintw(tab_window, "/ ");
    gg_wprintw(tab_window, tabs[i].text);
    gg_wprintw(tab_window, " \\");
  }

  wrefresh(tab_window);
}


void
tab_window_choose(Tab *tabs, int choice)
{
  int  pos;
  int  len;
  int  i;
  int  j;

  /* Store the choice for future reference. */
  displaydata.cur_tab_choice = choice;

  /* Find out where the open space in the box is. */
  pos = 1;
  for (i = 0; i < choice; ++i) {
    pos += strlen(tabs[i].text) + 4;
  }

  /* Redraw the box and the open space. */
  len = strlen(tabs[i].text) + 4;
  box(info_window, '|', '-');
  wmove(info_window, 0, pos);
  for (j = 0; j < len; ++j)
    gg_wprintw(info_window, " ");

  /* Clear everything inside the box. */
  for (i = 1; i < INFO_HEIGHT-1; ++i)
    for (j = 1; j < INFO_WIDTH-1; ++j)
      mvwaddch(info_window, i, j, ' ');

  wrefresh(info_window);
}


/* ================================================================ */
/*                           Info window                            */
/* ================================================================ */


/* ================================================================ */
/*                          Message window                          */
/* ================================================================ */


/*
 * Display a message at the bottom of the screen.
 */

void
display_message(const char *format, ...)
{
  va_list  ap;

  va_start(ap, format);

  wmove(message_window, 1, 2);

  gg_vwprintw(message_window, format, ap);

  wclrtoeol(message_window);
  wrefresh(message_window);

  display_board_cursor();

  va_end(ap);
}

/*
 * Display a little help about possible keyboard commands.
 */

void
display_help()
{
  display_message("?: Help, q: Quit, l: Refresh screen");
}


/* ================================================================ */


/*
 * Redisplay everything on the screen. This can be used, for instance
 * when the screen has been garbled from a system message or similar.
 */

void
display_refresh()
{
  /* Empty the screen */
  touchwin(stdscr);
  refresh();

  /* Redraw everything. */
  touchwin(board_frame_window);
  wrefresh(board_frame_window);
  touchwin(board_window);
  wrefresh(board_window);
  
  touchwin(tab_window);
  wrefresh(tab_window);
  
  touchwin(info_window);
  wrefresh(info_window);
  
  touchwin(message_window);
  wrefresh(message_window);

  display_board_cursor();
}

#endif /* CURSES */

/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
