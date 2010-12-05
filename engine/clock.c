/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * This is GNU Go, a Go program. Contact gnugo@gnu.org, or see       *
 * http://www.gnu.org/software/gnugo/ for more information.          *
 *                                                                   *
 * Copyright 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007,   *
 * 2008, 2009 and 2010 by the Free Software Foundation.              *
 *                                                                   *
 * This program is free software; you can redistribute it and/or     *
 * modify it under the terms of the GNU General Public License as    *
 * published by the Free Software Foundation - version 3 or          *
 * (at your option) any later version.                               *
 *                                                                   *
 * This program is distributed in the hope that it will be useful,   *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of    *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     *
 * GNU General Public License in file COPYING for more details.      *
 *                                                                   *
 * You should have received a copy of the GNU General Public         *
 * License along with this program; if not, write to the Free        *
 * Software Foundation, Inc., 51 Franklin Street, Fifth Floor,       *
 * Boston, MA 02111, USA.                                            *
\* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* ============================================================= *\
 *                        Time handling                          *
 *                          for GNU Go                           *
 *                         __       __                           *
 *                        <  >     <  >                          *
 *                      +--++-------++--+                        *
 *                      |  .'11 12 1'.  |                        *
 *                      |  :10 \    2:  |                        *
 *                      |  :9   @-> 3:  |                        *
 *                      |  :8       4;  |                        *
 *                      |  '..7 6 5..'  |                        *
 *                      |_______________|                        *
 *                                                               *
\* ============================================================= */

#include "clock.h"
#include "gg_utils.h"
#include "board.h"

/* Level data */
static int level             = DEFAULT_LEVEL; /* current level */
static int level_offset      = 0;
static int min_level         = 0;
static int max_level         = gg_max(DEFAULT_LEVEL, 10);


/*************************/
/* Datas and other stuff */
/*************************/

/* clock parameters */
static int main_time = -1;
static int byoyomi_time = -1;
static int byoyomi_stones = -1; /* <= 0 if no byo-yomi */

/* Keep track of the remaining time left.
 * If stones_left is zero, .._time_left is the remaining main time.
 * Otherwise, the remaining time for this byoyomi period.
 */
struct remaining_time_data {
  double time_left;
  double time_for_last_move;
  int stones;
  int movenum;
  int in_byoyomi;
};

struct timer_data {
  struct remaining_time_data official;
  struct remaining_time_data estimated;
  int time_out;
};

static struct timer_data black_time_data;
static struct timer_data white_time_data;


/* Echo a time value in STANDARD format */
static void
timeval_print(FILE *outfile, double tv)
{
  int min;
  double sec;

  min = (int) tv / 60;
  sec = tv - min*60;

  fprintf(outfile, "%3dmin %.2fsec ", min, sec);
}


/* Print the clock status for one side. */
void
clock_print(int color)
{
  struct timer_data *const td
    = (color == BLACK) ? &black_time_data : &white_time_data;

  fprintf(stderr, "clock: "); 
  fprintf(stderr, "%s ", color_to_string(color));

  if (td->time_out)
    fprintf(stderr, "TIME OUT! ");
  else {
    if (td->estimated.in_byoyomi) {
      fprintf(stderr, "byoyomi");
      timeval_print(stderr, td->estimated.time_left);
      fprintf(stderr, "for %d stones.", td->estimated.stones);
    }
    else
      timeval_print(stderr, td->estimated.time_left);

  }
  fprintf(stderr, "\n");
}


/******************************/
/*  Initialization functions  */
/******************************/

/*
 * Initialize the time settings for this game.
 * -1 means "do not modify this value".
 *
 *  byo_time > 0 and byo_stones == 0 means no time settings.
 */
void
clock_settings(int time, int byo_time, int byo_stones)
{
  if (time >= 0)
    main_time = time;
  if (byo_time >= 0)
    byoyomi_time = byo_time;
  if (byo_stones >= 0)
    byoyomi_stones = byo_stones;
  init_timers();
}

/* Get time settings. Returns 1 if any time settings have been made,
 * 0 otherwise.
 */
int
have_time_settings(void)
{
  /* According to the semantics of the GTP command 'time_settings', the
   * following signifies no time limits.
   */
  if (byoyomi_time > 0 && byoyomi_stones == 0)
    return 0;
  else
    return (main_time >= 0 || byoyomi_time >= 0);
}


/* Initialize all timers. */
void
init_timers()
{
  white_time_data.official.time_left = main_time;
  white_time_data.official.time_for_last_move = -1.0;
  white_time_data.official.stones = 0;
  white_time_data.official.movenum = 0;
  white_time_data.official.in_byoyomi = 0;
  white_time_data.estimated = white_time_data.official;
  white_time_data.time_out = 0;
  black_time_data = white_time_data;

  level_offset = 0;
}


/*****************************/
/*  Clock access functions.  */
/*****************************/


void
update_time_left(int color, int time_left, int stones)
{
  struct timer_data *const td
    = ((color == BLACK) ? &black_time_data : &white_time_data);
  int time_used = td->official.time_left - time_left;

  if (time_left > 0)
    td->time_out = 0;
  else 
    td->time_out = 1;

  /* Did our estimate for time usage go wrong? */
  if (time_used > 0
      && gg_abs(time_used - td->estimated.time_for_last_move) >= 1.0)
    td->estimated.time_for_last_move = time_used;
  td->estimated.stones = stones;
  td->estimated.movenum = movenum;
  /* Did our clock go wrong? */
  if (gg_abs(td->estimated.time_left - time_left) >= 1.0)
    td->estimated.time_left = time_left;
  if (stones > 0)
    td->estimated.in_byoyomi = 1;
  else
    td->estimated.in_byoyomi = 0;

  td->official.stones = stones;
  td->official.movenum = movenum;
  td->official.time_for_last_move = td->official.time_for_last_move - time_left;
  td->official.time_left = time_left;
  td->official.in_byoyomi = td->estimated.in_byoyomi;
}

/*
 * Update the estimated timer after a move has been made.
 */
void
clock_push_button(int color)
{
  static double last_time = -1.0;
  static int last_movenum = -1;
  struct timer_data *const td
    = (color == BLACK) ? &black_time_data : &white_time_data;
  double now = gg_gettimeofday();

  if (!have_time_settings())
    return;

  if (last_movenum >= 0
      && movenum == last_movenum + 1
      && movenum > td->estimated.movenum) {
    double time_used = now - last_time;
    td->estimated.time_left -= time_used;
    td->estimated.movenum = movenum;
    td->estimated.time_for_last_move = time_used;
    if (td->estimated.time_left < 0) {
      if (td->estimated.in_byoyomi || byoyomi_stones == 0) {
	DEBUG(DEBUG_TIME, "%s ran out of time.\n", color_to_string(color));
	if (debug & DEBUG_TIME)
	  clock_print(color);
	td->time_out = 1;
      }
      else {
	/* Entering byoyomi. */
	gg_assert(!(td->estimated.in_byoyomi));
	td->estimated.in_byoyomi = 1;
	td->estimated.stones = byoyomi_stones - 1;
	td->estimated.time_left += byoyomi_time;
	if (td->estimated.time_left < 0)
	  td->time_out = 1;
      }
    }
    else if (td->estimated.stones > 0) {
      gg_assert(td->estimated.in_byoyomi);
      td->estimated.stones = td->estimated.stones - 1;
      if (td->estimated.stones == 0) {
	td->estimated.time_left = byoyomi_time;
	td->estimated.stones = byoyomi_stones;
      }
    }
  }

  last_movenum = movenum;
  last_time = now;

  /* Update main timer. */
  if (debug & DEBUG_TIME)
    clock_print(color);
}


/**********************/
/*  Autolevel system  */
/**********************/


/* Analyze the two most recent time reports and determine the time
 * spent on the last moves, the (effective) number of stones left and
 * the (effective) remaining time.
 */
static int
analyze_time_data(int color, double *time_for_last_move, double *time_left,
		  int *stones_left)
{
  struct remaining_time_data *const timer
    = (color == BLACK) ? &black_time_data.estimated
	               : &white_time_data.estimated;

  /* Do we have any time limits. */
  if (!have_time_settings())
    return 0;

  /* If we don't have consistent time information yet, just return. */
  if (timer->time_for_last_move < 0.0)
    return 0;

  *time_for_last_move = timer->time_for_last_move;

  if (timer->stones == 0) {
    /* Main time running. */
    *time_left = timer->time_left + byoyomi_time;
    if (byoyomi_time > 0)
      *stones_left = byoyomi_stones;
    else {
      /* Absolute time. Here we aim to be able to play at least X more
       * moves or a total of Y moves. We choose Y as a third of the
       * number of vertices and X as 40% of Y. For 19x19 this means
       * that we aim to play at least a total of 120 moves
       * (corresponding to a 240 move game) or another 24 moves.
       *
       * FIXME: Maybe we should use the game_status of
       * influence_evaluate_position() here to guess how many moves
       * are remaining.
       */
      int nominal_moves = board_size * board_size / 3;
      *stones_left = gg_max(nominal_moves - movenum / 2,
			    2 * nominal_moves / 5);
    }
  }
  else {
    *time_left = timer->time_left;
    *stones_left = timer->stones;
  }
  return 1;
}


/* Adjust the level offset given information of current playing speed
 * and remaining time and stones.
 */
void
adjust_level_offset(int color)
{
  double time_for_last_move;
  double time_left;
  int stones_left;

  if (!analyze_time_data(color, &time_for_last_move, &time_left, &stones_left))
    return;


  /* These rules are both crude and ad hoc.
   *
   * FIXME: Use rules with at least some theoretical basis.
   */
  if (time_left < time_for_last_move * (stones_left + 3))
    level_offset--;
  if (time_left < time_for_last_move * stones_left)
    level_offset--;
  if (3 * time_left < 2 * time_for_last_move * stones_left)
    level_offset--;
  if (2 * time_left < time_for_last_move * stones_left)
    level_offset--;
  if (3 * time_left < time_for_last_move * stones_left)
    level_offset--;

  if (time_for_last_move == 0)
    time_for_last_move = 1;
  if (time_left > time_for_last_move * (stones_left + 6))
    level_offset++;
  if (time_left > 2 * time_for_last_move * (stones_left + 6))
    level_offset++;

  if (level + level_offset < min_level)
    level_offset = min_level - level;

  if (level + level_offset > max_level)
    level_offset = max_level - level;

  DEBUG(DEBUG_TIME, "New level %d (%d %C %f %f %d)\n", level + level_offset,
	movenum / 2, color, time_for_last_move, time_left, stones_left);
}


/********************************/
/* Interface to level settings. */
/********************************/

int
get_level()
{
  return level + level_offset;
}

void
set_level(int new_level)
{
  level = new_level;
  level_offset = 0;
  if (level > max_level)
    max_level = level;
  if (level < min_level)
    min_level = level;
}

void
set_max_level(int new_max)
{
  max_level = new_max;
}

void
set_min_level(int new_min)
{
  min_level = new_min;
}


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
