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

#include "display.h"
#include "gui.h"
#include "liberty.h"


void
prepare_worms_tab()
{
  wmove(info_window, 3, 2);
  gg_wprintw(info_window, "size:           ");

  wmove(info_window, 5, 2);
  gg_wprintw(info_window, "cutstone:        ");
  wmove(info_window, 6, 2);
  gg_wprintw(info_window, "cutstone2:       ");
  wmove(info_window, 7, 2);
  gg_wprintw(info_window, "genus:           ");

  wmove(info_window, 3, 36);
  gg_wprintw(info_window, "liberties:      1   2   3   4");

  wmove(info_window, 6, 36);
  gg_wprintw(info_window, "uncond. status:  ");
  wmove(info_window, 7, 36);
  gg_wprintw(info_window, "flags:           ");

  wmove(info_window, 9, 2);
  gg_wprintw(info_window, "attack:          ");
  wmove(info_window, 10, 2);
  gg_wprintw(info_window, "defense:         ");
  wmove(info_window, 11, 2);
  gg_wprintw(info_window, "attack threats:  ");
  wmove(info_window, 12, 2);
  gg_wprintw(info_window, "defense threats: ");
  wmove(info_window, 13, 2);
  gg_wprintw(info_window, "lunch: ");

  wrefresh(info_window);
}


void
display_worm(int w)
{
  int  i;

  wmove(info_window, 1, 2);
  gg_wprintw(info_window, "%3s: %5s worm   ",
	     location_to_string(w),
	     color_to_string(worm[w].color));
  gg_wprintw(info_window, "(origin %s) ",
	     location_to_string(worm[w].origin));

  wmove(info_window, 3, 16);
  gg_wprintw(info_window, "%3d  %5.3f ",
	     worm[w].size, worm[w].effective_size);

  wmove(info_window, 5, 19);
  gg_wprintw(info_window, "%d", worm[w].cutstone);
  wmove(info_window, 6, 19);
  gg_wprintw(info_window, "%d", worm[w].cutstone2);
  wmove(info_window, 7, 19);
  gg_wprintw(info_window, "%d", worm[w].genus);

  wmove(info_window, 4, 50);
  gg_wprintw(info_window, "%3d %3d %3d %3d ", 
	     worm[w].liberties, worm[w].liberties2,
	     worm[w].liberties3, worm[w].liberties4);

  wmove(info_window, 6, 55);
  gg_wprintw(info_window, "%s         ", 
	     status_to_string(worm[w].unconditional_status));
  /* FIXME: Is status_to_string() correct here? */

  wmove(info_window, 7, 49);
  gg_wprintw(info_window, "                          ");
  wmove(info_window, 7, 49);
  if (worm[w].inessential)
    gg_wprintw(info_window, "inessential ");
  if (worm[w].invincible)
    gg_wprintw(info_window, "invincible ");

  for (i = 0; i < MAX_TACTICAL_POINTS; i++) {
    wmove(info_window, 9, 19 + i * 6);
    if (worm[w].attack_points[i] == 0)
      gg_wprintw(info_window, "---:0 ", worm[w].attack_codes[i]);
    else
      gg_wprintw(info_window, "%3s:%d ", 
		 location_to_string(worm[w].attack_points[i]),
		 worm[w].attack_codes[i]);
  
    wmove(info_window, 10, 19 + i * 6);
    if (worm[w].defense_points[i] == 0)
      gg_wprintw(info_window, "---:0 ", worm[w].defense_codes[i]);
    else
      gg_wprintw(info_window, "%3s:%d ", 
		 location_to_string(worm[w].defense_points[i]),
		 worm[w].defense_codes[i]);

    wmove(info_window, 11, 19 + i * 6);
    if (worm[w].attack_threat_points[i] == 0)
      gg_wprintw(info_window, "---:0", worm[w].attack_threat_codes[i]);
    else
      gg_wprintw(info_window, "%3s:%d ", 
		 location_to_string(worm[w].attack_threat_points[i]),
		 worm[w].attack_threat_codes[i]);
  
    wmove(info_window, 12, 19 + i * 6);
    if (worm[w].defense_threat_points[i] == 0)
      gg_wprintw(info_window, "---:0 ", worm[w].defense_threat_codes[i]);
    else
      gg_wprintw(info_window, "%3s:%d ", 
		 location_to_string(worm[w].defense_threat_points[i]),
		 worm[w].defense_threat_codes[i]);
  }

  wmove(info_window, 13, 19);
  if (worm[w].lunch == NO_MOVE)
    gg_wprintw(info_window, "---");
  else
    gg_wprintw(info_window, "%3s", location_to_string(worm[w].lunch));

  wrefresh(info_window);
}


void
display_worm_tactical_data(int m, int n)
{
  int i;
  int ii;

  ii = POS(m, n);
  for (i = 0; i < MAX_TACTICAL_POINTS; i++) {
    if (worm[ii].attack_codes[i] != 0)
      display_board_char(I(worm[ii].attack_points[i]),
			 J(worm[ii].attack_points[i]), 'A');
    if (worm[ii].defense_codes[i] != 0)
      display_board_char(I(worm[ii].defense_points[i]),
			 J(worm[ii].defense_points[i]), 'D');
    if (worm[ii].attack_threat_codes[i] != 0)
      display_board_char(I(worm[ii].attack_threat_points[i]),
			 J(worm[ii].attack_threat_points[i]), 'a');
    if (worm[ii].defense_threat_codes[i] != 0)
      display_board_char(I(worm[ii].defense_threat_points[i]),
			 J(worm[ii].defense_threat_points[i]), 'd');
  }
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
display_dragon(int pos)
{
  struct dragon_data *d;
  struct dragon_data2 *d2;

  /* FIXME: We should clear the old entries here rather than just
   * returning when called on an empty vertex. We can't continue now,
   * however, since we may crash on bad field values.
   */
  if (board[pos] == EMPTY)
    return;
    
  d = &dragon[pos];
  d2 = &(dragon2[d->id]);
  wmove(info_window, 1, 2);
  gg_wprintw(info_window, "%3s: %5s dragon ",
	     location_to_string(pos), color_to_string(d->color));
  gg_wprintw(info_window, "(origin %s)  id %d   ",
	     location_to_string(d->origin), d->id);

  wmove(info_window, 3, 16);
  gg_wprintw(info_window, "%3d  %5.3f ", d->size, d->effective_size);

#if 0
  wmove(info_window, 8, 18);
  if (board[pos] == EMPTY)
    gg_wprintw(info_window, "    ", d2->heyes);
  else
    gg_wprintw(info_window, "%d  ", d2->heyes);
  wmove(info_window, 8, 21);
  if (board[pos] != EMPTY && d2->heyes > 0)
    gg_wprintw(info_window, "[%s] ", location_to_string(d2->heye));
  else
    gg_wprintw(info_window, "[---]");

  wmove(info_window, 9, 18);
  gg_wprintw(info_window, "%d  ", d2->genus);
#endif
  wmove(info_window, 10, 18);
  gg_wprintw(info_window, "%d  ", 
	     board[pos] == EMPTY ? 0 : d2->escape_route);
  wmove(info_window, 11, 18);
  if (d2->lunch == NO_MOVE)
    gg_wprintw(info_window, "---");
  else
    gg_wprintw(info_window, "%3s", location_to_string(d2->lunch));

  /* status */
  wmove(info_window, 3, 55);
  gg_wprintw(info_window, "%s     ", status_to_string(d->crude_status));

  /* owl status */
  wmove(info_window, 4, 55);
  gg_wprintw(info_window, "%-12s", status_to_string(d2->owl_status));
  if (d2->owl_attack_point == NO_MOVE)
    gg_wprintw(info_window, "[---] ");
  else
    gg_wprintw(info_window, "[%3s] ",
	       location_to_string(d2->owl_attack_point));
  if (d2->owl_defense_point == NO_MOVE)
    gg_wprintw(info_window, "[---] ");
  else
    gg_wprintw(info_window, "[%3s] ",
	       location_to_string(d2->owl_defense_point));

  wmove(info_window, 5, 55);
  switch (d2->owl_threat_status) {
  case DEAD:
  case ALIVE:
    gg_wprintw(info_window, "%-12s[---] [---]", 
	       status_to_string(d2->owl_threat_status));
    break;
  case UNCHECKED:
    gg_wprintw(info_window, "unchecked   [---] [---]");
    break;
  case CAN_THREATEN_ATTACK:
    gg_wprintw(info_window, "att. threat [%3s] [---]",
	       location_to_string(d2->owl_second_attack_point));
    break;
  case CAN_THREATEN_DEFENSE:
    gg_wprintw(info_window, "def. threat [---] [%3s]",
	       location_to_string(d2->owl_second_defense_point));
    break;
  default:
    gg_wprintw(info_window, "Error: %3d  [---] [---]", d2->owl_threat_status);
    break;
  }

  wmove(info_window, 6, 55);
  gg_wprintw(info_window, "%s     ", status_to_string(d->status));

  wmove(info_window, 8, 55);
  wmove(info_window, 9, 55);
  gg_wprintw(info_window, "%s     ", safety_to_string(d2->safety));
  wmove(info_window, 10, 55);
  gg_wprintw(info_window, "%d  ", d2->semeai);

  wmove(info_window, 11, 55);
  gg_wprintw(info_window, "%d  ", d2->semeai_margin_of_safety);

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
display_eye(int color, struct eye_data eyedata[BOARDMAX], int pos)
{
  wmove(info_window, 1, 24);
  gg_wprintw(info_window, "%s", color == WHITE ? "WHITE" : "BLACK");
  wmove(info_window, 2, 9);
  gg_wprintw(info_window, "%s ", location_to_string(pos));

  wmove(info_window, 2, 24);
  if (eyedata[pos].origin == NO_MOVE)
    gg_wprintw(info_window, "---");
  else
    gg_wprintw(info_window, "%s ", 
	       location_to_string(eyedata[pos].origin));

  wmove(info_window, 4, 18);
  gg_wprintw(info_window, "%-12s", color_to_string(eyedata[pos].color));
  wmove(info_window, 5, 18);
  gg_wprintw(info_window, "%d   ", eyedata[pos].esize);
  wmove(info_window, 6, 18);
  gg_wprintw(info_window, "%d   ", eyedata[pos].msize);
#if 0
  wmove(info_window, 7, 18);
  if (eyedata[pos].dragon == NO_MOVE)
    gg_wprintw(info_window, "---");
  else
    gg_wprintw(info_window, "%s ",
	       location_to_string(eyedata[pos].dragon));
#endif

  wmove(info_window, 11, 18);
  gg_wprintw(info_window, "%d  ", eyedata[pos].type);
  wmove(info_window, 12, 18);
  gg_wprintw(info_window, "%d  ", eyedata[pos].marginal);

  wmove(info_window, 4, 55);
  gg_wprintw(info_window, "%d ", max_eyes(&eyedata[pos].value));
  wmove(info_window, 5, 55);
  gg_wprintw(info_window, "%d ", min_eyes(&eyedata[pos].value));

  wmove(info_window, 6, 55);
  if (eyedata[pos].attack_point == NO_MOVE)
    gg_wprintw(info_window, "---          ");
  else
    gg_wprintw(info_window, "%3s", 
	       location_to_string(eyedata[pos].attack_point));

  wmove(info_window, 7, 55);
  if (eyedata[pos].defense_point == NO_MOVE)
    gg_wprintw(info_window, "---          ");
  else
    gg_wprintw(info_window, "%3s", 
	       location_to_string(eyedata[pos].defense_point));
  
  wmove(info_window, 11, 55);
  gg_wprintw(info_window, "%d ", eyedata[pos].neighbors);
  wmove(info_window, 12, 55);
  gg_wprintw(info_window, "%d ", eyedata[pos].marginal_neighbors);
  wmove(info_window, 13, 55);
  gg_wprintw(info_window, "%d ", eyedata[pos].cut);

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
