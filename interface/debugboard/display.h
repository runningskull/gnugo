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

#ifndef __DISPLAY_H__
#define __DISPLAY_H__

/*
 * File: display.h
 */

#include <stdlib.h>

#if HAVE_CURSES_H
#include <curses.h>
#elif HAVE_NCURSES_CURSES_H
#include <ncurses/curses.h>
#else
#endif

#include <stdarg.h>

#include "gnugo.h"


/* ================================================================ */


typedef struct {
  int  boardsize;
  int  cur_row;
  int  cur_col;

  int  cur_tab_choice;
} Display_data;


typedef struct {
  const char *text;
} Tab;


/* ================================================================ */


#define BOARD_FRAME_MINROW   0
#define BOARD_FRAME_MINCOL   0
#define BOARD_FRAME_HEIGHT   (MAX_BOARD + 5)
#define BOARD_FRAME_WIDTH    MINWIDTH

#define BOARD_MINROW   1
#define BOARD_MINCOL   7

#define TAB_MINROW     BOARD_FRAME_HEIGHT
#define TAB_MINCOL     0
#define TAB_HEIGHT     2
#define TAB_WIDTH      MINWIDTH

#define INFO_MINROW     (TAB_MINROW + TAB_HEIGHT)
#define INFO_MINCOL     0
#define INFO_HEIGHT     15
#define INFO_WIDTH      MINWIDTH

#define MESSAGE_MINROW     (INFO_MINROW + INFO_HEIGHT)
#define MESSAGE_MINCOL     0
#define MESSAGE_HEIGHT     3
#define MESSAGE_WIDTH      MINWIDTH


/* Minimal width & height for the display. */
#define MINWIDTH    80
#define MINHEIGHT   (MESSAGE_MINROW + MESSAGE_HEIGHT)


extern WINDOW   *board_frame_window;
extern WINDOW   *board_window;
extern WINDOW   *tab_window;
extern WINDOW   *info_window;
extern WINDOW   *message_window;

void gg_vwprintw(WINDOW *w, const char *format, va_list ap);
void gg_wprintw(WINDOW *w, const char *format, ...);

/* ================================================================ */
/*********************** display functions ************************/


/*
 * Display_init() initializes the display.
 *
 * Display_cleanup() does any necessary cleanup, such as free()ing
 * used memory and resetting the terminal to normal.
 */

int  display_init(int minwidth, int minheight, int size);
int  display_input(void);
void display_cleanup(void);

void display_refresh(void);
void display_help(void);

void clear_all_windows(void);
void clear_board_frame(void);
void clear_visual_board(void);
void clear_tab_window(void);
void clear_info_window(void);
void clear_message_window(void);


int display_cur_row(void);
int display_cur_col(void);
int display_cur_tabchoice(void);

typedef void (display_board_function)(int m, int n);
void display_board_cursor(void);
void display_board_char(int i, int j, int ch);
void display_board_intersection(int i, int j, int color);
void display_board_position(display_board_function *func);
void display_board_moveto(int i, int j);
void display_board_move(int di, int dj);


void tab_window_draw(Tab *tabs);
void tab_window_choose(Tab *tabs, int choice);

void display_message(const char *format, ...);

#define display_error display_message



#endif /* __DISPLAY_H__ */


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
