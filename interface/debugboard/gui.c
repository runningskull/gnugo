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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef CURSES

#include "display.h"
#include "gui.h"
#include "liberty.h"


void
prepare_worms_tab()
{
  wmove(info_window, 3, 2);
  gg_wprintw(info_window, "size:           ");

  wmove(info_window, 5, 2);
  gg_wprintw(info_window, "liberties:      1   2   3   4");

  wmove(info_window, 8, 2);
  gg_wprintw(info_window, "cutstone:        ");
  wmove(info_window, 9, 2);
  gg_wprintw(info_window, "cutstone2:       ");
  wmove(info_window, 10, 2);
  gg_wprintw(info_window, "genus:           ");

  wmove(info_window, 3, 36);
  gg_wprintw(info_window, "attack:          ");
  wmove(info_window, 4, 36);
  gg_wprintw(info_window, "defense:         ");
  wmove(info_window, 5, 36);
  gg_wprintw(info_window, "lunch: ");

  wmove(info_window, 7, 36);
  gg_wprintw(info_window, "uncond. status:  ");
  wmove(info_window, 8, 36);
  gg_wprintw(info_window, "flags:           ");

  wrefresh(info_window);
}


void
display_worm(Position *pos, int i, int j)
{
  UNUSED(pos);
  wmove(info_window, 1, 2);
  gg_wprintw(info_window, "%3s: %5s worm   ",
	     location_to_string2(i, j),
	     color_to_string(worm[i][j].color));
  gg_wprintw(info_window, "(origin %s) ",
	     location_to_string(worm[i][j].origin));

  wmove(info_window, 3, 16);
  gg_wprintw(info_window, "%3d  %5.3f ",
	     worm[i][j].size, worm[i][j].effective_size);

  wmove(info_window, 6, 16);
  gg_wprintw(info_window, "%3d %3d %3d %3d ", 
	     worm[i][j].liberties, worm[i][j].liberties2,
	     worm[i][j].liberties3, worm[i][j].liberties4);

  wmove(info_window, 8, 18);
  gg_wprintw(info_window, "%d", worm[i][j].cutstone);
  wmove(info_window, 9, 18);
  gg_wprintw(info_window, "%d", worm[i][j].cutstone2);
  wmove(info_window, 10, 18);
  gg_wprintw(info_window, "%d", worm[i][j].genus);

  wmove(info_window, 3, 55);
  if (worm[i][j].attack_point == 0)
    gg_wprintw(info_window, "--- [code %d]",
	       worm[i][j].attack_code);
  else
    gg_wprintw(info_window, "%3s [code %d]", 
	    location_to_string(worm[i][j].attack_point),
	    worm[i][j].attack_code);
  
  wmove(info_window, 4, 55);
  if (worm[i][j].defense_point == 0)
    gg_wprintw(info_window, "--- [code %d]",
	       worm[i][j].defend_code);
  else
    gg_wprintw(info_window, "%3s [code %d]", 
	    location_to_string(worm[i][j].defense_point),
	    worm[i][j].defend_code);

  wmove(info_window, 5, 55);
  if (worm[i][j].lunch == NO_MOVE)
    gg_wprintw(info_window, "---");
  else
    gg_wprintw(info_window, "%3s", location_to_string(worm[i][j].lunch));

  wmove(info_window, 7, 55);
  gg_wprintw(info_window, "%s         ", 
	     status_to_string(worm[i][j].unconditional_status));
  /* FIXME: Is status_to_string() correct here? */

  wmove(info_window, 8, 49);
  gg_wprintw(info_window, "                          ");
  wmove(info_window, 8, 49);
  if (worm[i][j].ko)
    gg_wprintw(info_window, "ko ");
  if (worm[i][j].inessential)
    gg_wprintw(info_window, "inessential ");
  if (worm[i][j].invincible)
    gg_wprintw(info_window, "invincible ");

  wrefresh(info_window);
}


/* ---------------------------------------------------------------- */

/* Missing: 
 * border, age, owl_time
 *
 * Missing in dragon_data2:
 * neighbors, adjacent[], moyo
 */

void
prepare_dragons_tab()
{
  wmove(info_window, 3, 2);
  gg_wprintw(info_window, "size:           ");

  wmove(info_window, 8, 2);
  gg_wprintw(info_window, "half eyes:      ");
  wmove(info_window, 9, 2);
  gg_wprintw(info_window, "genus:          ");
  wmove(info_window, 10, 2);
  gg_wprintw(info_window, "escape_route    ");
  wmove(info_window, 11, 2);
  gg_wprintw(info_window, "lunch: ");

  wmove(info_window, 3, 36);
  gg_wprintw(info_window, "status:");
  wmove(info_window, 4, 36);
  gg_wprintw(info_window, "owl_status:");
  wmove(info_window, 5, 36);
  gg_wprintw(info_window, "owl_threat_status:");
  wmove(info_window, 6, 36);
  gg_wprintw(info_window, "matcher_status:  ");

  wmove(info_window, 8, 36);
  wmove(info_window, 9, 36);
  gg_wprintw(info_window, "safety:          ");
  wmove(info_window, 10, 36);
  gg_wprintw(info_window, "semeai:          ");
  wmove(info_window, 11, 36);
  gg_wprintw(info_window, "semeai_margin:   ");

  wrefresh(info_window);
}


void
display_dragon(int i, int j)
{
  wmove(info_window, 1, 2);
  gg_wprintw(info_window, "%3s: %5s dragon ",
	     location_to_string2(i, j),
	     color_to_string(dragon[i][j].color));
  gg_wprintw(info_window, "(origin %s)  id %d   ",
	     location_to_string(dragon[i][j].origin),
	     dragon[i][j].id);

  wmove(info_window, 3, 16);
  gg_wprintw(info_window, "%3d  %5.3f ",
	     dragon[i][j].size, dragon[i][j].effective_size);

  wmove(info_window, 8, 18);
  gg_wprintw(info_window, "%d  ", dragon[i][j].heyes);
  wmove(info_window, 8, 21);
  if (dragon[i][j].heyes > 0)
    gg_wprintw(info_window, "[%s] ", 
	       location_to_string(dragon[i][j].heye));
  else
    gg_wprintw(info_window, "[---]");

  wmove(info_window, 9, 18);
  gg_wprintw(info_window, "%d  ", dragon[i][j].genus);
  wmove(info_window, 10, 18);
  gg_wprintw(info_window, "%d  ", dragon[i][j].escape_route);
  wmove(info_window, 11, 18);
  if (dragon[i][j].lunch == NO_MOVE)
    gg_wprintw(info_window, "---");
  else
    gg_wprintw(info_window, "%3s", 
	       location_to_string(dragon[i][j].lunch));

  wmove(info_window, 3, 55);
  gg_wprintw(info_window, "%s     ", status_to_string(dragon[i][j].status));

  wmove(info_window, 4, 55);
  gg_wprintw(info_window, "%-12s", status_to_string(dragon[i][j].owl_status));
  if (dragon[i][j].owl_attacki == -1)
    gg_wprintw(info_window, "[---] ");
  else
    gg_wprintw(info_window, "[%3s] ", 
	    location_to_string2(dragon[i][j].owl_attacki,
			       dragon[i][j].owl_attackj));
  if (dragon[i][j].owl_attacki == -1)
    gg_wprintw(info_window, "[---] ");
  else
    gg_wprintw(info_window, "[%3s] ", 
	    location_to_string2(dragon[i][j].owl_defendi,
			       dragon[i][j].owl_defendj));

  wmove(info_window, 5, 55);
  switch (dragon[i][j].owl_threat_status) {
  case UNCHECKED:
    gg_wprintw(info_window, "unchecked   [---] [---]");
    break;
  case CAN_THREATEN_ATTACK:
    gg_wprintw(info_window, "att. threat [%3s] [---]",
	       location_to_string2(dragon[i][j].owl_second_attacki,
				  dragon[i][j].owl_second_attackj));
    break;
  case CAN_THREATEN_DEFENSE:
    gg_wprintw(info_window, "def. threat [---] [%3s]",
	       location_to_string2(dragon[i][j].owl_second_defendi,
				  dragon[i][j].owl_second_defendj));
    break;
  default:
    gg_wprintw(info_window, "Error: %3d  [---] [---]",
	       dragon[i][j].owl_threat_status);
    break;
  }

  wmove(info_window, 6, 55);
  gg_wprintw(info_window, "%s     ", status_to_string(dragon[i][j].matcher_status));

  wmove(info_window, 8, 55);
  wmove(info_window, 9, 55);
  gg_wprintw(info_window, "%s     ", status_to_string(DRAGON2(i, j).safety));
  wmove(info_window, 10, 55);
  gg_wprintw(info_window, "%d  ", dragon[i][j].semeai);

  wmove(info_window, 11, 55);
  gg_wprintw(info_window, "%d  ", dragon[i][j].semeai_margin_of_safety);

  wrefresh(info_window);
}


/* ---------------------------------------------------------------- */


void
prepare_eyes_tab()
{
  wmove(info_window, 1, 2);
  gg_wprintw(info_window, "Displaying data for:");
  wmove(info_window, 2, 2);
  gg_wprintw(info_window, "Eye at        origin: ");

  wmove(info_window, 4, 2);
  gg_wprintw(info_window, "color:          ");
  wmove(info_window, 5, 2);
  gg_wprintw(info_window, "total size:     ");
  wmove(info_window, 6, 2);
  gg_wprintw(info_window, "marginals:      ");
  wmove(info_window, 7, 2);
  gg_wprintw(info_window, "surr. dragon:   ");

  wmove(info_window, 9, 2);
  gg_wprintw(info_window, "----------------------------------------------------------------------------");

  wmove(info_window, 11, 2);
  gg_wprintw(info_window, "type:           ");
  wmove(info_window, 12, 2);
  gg_wprintw(info_window, "marginal:       ");
  wmove(info_window, 13, 2);
  gg_wprintw(info_window, "false margin:   ");

  wmove(info_window, 4, 36);
  gg_wprintw(info_window, "max eyes:        ");
  wmove(info_window, 5, 36);
  gg_wprintw(info_window, "min eyes:        ");
  wmove(info_window, 6, 36);
  gg_wprintw(info_window, "attack point:    ");
  wmove(info_window, 7, 36);
  gg_wprintw(info_window, "defense point:   ");

  wmove(info_window, 11, 36);
  gg_wprintw(info_window, "neighbors:       ");
  wmove(info_window, 12, 36);
  gg_wprintw(info_window, "marg. neighbors: ");
  wmove(info_window, 13, 36);
  gg_wprintw(info_window, "cut:             ");

  wrefresh(info_window);
}


void
display_eye(int color, struct eye_data eyedata[MAX_BOARD][MAX_BOARD], 
	    int i, int j)
{
  wmove(info_window, 1, 24);
  gg_wprintw(info_window, "%s", color == WHITE ? "WHITE" : "BLACK");
  wmove(info_window, 2, 9);
  gg_wprintw(info_window, "%s ", location_to_string2(i, j));

  wmove(info_window, 2, 24);
  if (eyedata[i][j].origin == NO_MOVE)
    gg_wprintw(info_window, "---");
  else
    gg_wprintw(info_window, "%s ", 
	       location_to_string(eyedata[i][j].origin));

  wmove(info_window, 4, 18);
  gg_wprintw(info_window, "%-12s", color_to_string(eyedata[i][j].color));
  wmove(info_window, 5, 18);
  gg_wprintw(info_window, "%d   ", eyedata[i][j].esize);
  wmove(info_window, 6, 18);
  gg_wprintw(info_window, "%d   ", eyedata[i][j].msize);
  wmove(info_window, 7, 18);
  if (eyedata[i][j].dragon == NO_MOVE)
    gg_wprintw(info_window, "---");
  else
    gg_wprintw(info_window, "%s ",
	       location_to_string(eyedata[i][j].dragon));

  wmove(info_window, 11, 18);
  gg_wprintw(info_window, "%d  ", eyedata[i][j].type);
  wmove(info_window, 12, 18);
  gg_wprintw(info_window, "%d  ", eyedata[i][j].marginal);
  wmove(info_window, 13, 18);
  gg_wprintw(info_window, "%d  ", eyedata[i][j].false_margin);

  wmove(info_window, 4, 55);
  gg_wprintw(info_window, "%d ", eyedata[i][j].maxeye);
  wmove(info_window, 5, 55);
  gg_wprintw(info_window, "%d ", eyedata[i][j].mineye);

  wmove(info_window, 6, 55);
  if (eyedata[i][j].attack_point == NO_MOVE)
    gg_wprintw(info_window, "---          ");
  else
    gg_wprintw(info_window, "%3s", 
	       location_to_string(eyedata[i][j].attack_point));

  wmove(info_window, 7, 55);
  if (eyedata[i][j].defense_point == NO_MOVE)
    gg_wprintw(info_window, "---          ");
  else
    gg_wprintw(info_window, "%3s", 
	       location_to_string(eyedata[i][j].defense_point));
  
  wmove(info_window, 11, 55);
  gg_wprintw(info_window, "%d ", eyedata[i][j].neighbors);
  wmove(info_window, 12, 55);
  gg_wprintw(info_window, "%d ", eyedata[i][j].marginal_neighbors);
  wmove(info_window, 13, 55);
  gg_wprintw(info_window, "%d ", eyedata[i][j].cut);

  wrefresh(info_window);
}


/* ---------------------------------------------------------------- */


void
prepare_help_tab()
{
  wmove(info_window, 2, 2);
  gg_wprintw(info_window, "Worms screen   - press 'w'.");
  wmove(info_window, 3, 2);
  gg_wprintw(info_window, "Dragons screen - press 'd'.");
  wmove(info_window, 4, 2);
  gg_wprintw(info_window, "Eyes screen    - press 'e'.");
  wmove(info_window, 5, 2);
  gg_wprintw(info_window, "Help screen    - press 'h'.");

  wmove(info_window, 7, 2);
  gg_wprintw(info_window, "Show data for white - press 'W'.");
  wmove(info_window, 8, 2);
  gg_wprintw(info_window, "Show data for black - press 'B'.");

  wmove(info_window, 2, 36);
  gg_wprintw(info_window, "Move up    - press '8' or '^p'.");
  wmove(info_window, 3, 36);
  gg_wprintw(info_window, "Move down  - press '2' or '^n'.");
  wmove(info_window, 4, 36);
  gg_wprintw(info_window, "Move left  - press '4' or '^b'.");
  wmove(info_window, 5, 36);
  gg_wprintw(info_window, "Move right - press '6' or '^f'.");

  wrefresh(info_window);
}

#endif /* CURSES */

/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
