/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * This is GNU Go, a Go program. Contact gnugo@gnu.org, or see       *
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

#include "gnugo.h"

#include "liberty.h"
#include "gg_utils.h"
#include "clock.h"

/* parameters */
#define CLOCK_MAX_MOVES           500   /* max number of moves for a game */
#define CLOCK_STEP                0.4   /* modification step for level */
#define CLOCK_HURRY               10    /* Last chance limit: */
#define CLOCK_HURRYR              0.02  /* (10 sec or or 2% of main_time) */
#define CLOCK_HURRY_LEVEL         1     /* Last chance level: */
#define CLOCK_SAFE                300   /* Keep Ahead limit: */
#define CLOCK_SAFER               0.20  /* (5min or 20% of time) */
#define CLOCK_DELTA               10    /* Maximal time difference: */
#define CLOCK_DELTAR              0.10  /* 10sec or 10% of left time */
#define CLOCK_TIME_CONTRACT       0.75  /* 75% of time to play */
#define CLOCK_MOVE_CONTRACT(b)    (((b)*(b))/2)
#define CLOCK_CONTRACT_MIN_LEVEL  5


/*************************/
/* Datas and other stuff */
/*************************/

extern int board_size;

typedef struct {

  /* clock parameters */
  int    clock_on;
  int    ready;
  double main_time;
  double byoyomi_time; /* zero if no byo-yomi */
  int    byoyomi_stones;

  /* clock status */
  double timer[3];
  double btimer[3];
  int    byoyomi[3];
  int    dead[3];

  /* dates of each move */
  int    moveno; /* invariant: COLOR(clk.moveno) = color of last move */
  double date[CLOCK_MAX_MOVES];

  /* adapative system parameters */
  int    autolevel_on;
  double level;  /* FIXME: Why is this a double and not an int? */
  double levels[CLOCK_MAX_MOVES];
  double expected[CLOCK_MAX_MOVES];
  double error; /* time/move estimation error */
} gnugo_clock;

static gnugo_clock clk;

/* Color macro:
 *   WHITE : odd moves
 *   BLACK : even moves. 
 */
#define COLOR(m)  ((m) % 2 ? WHITE : BLACK)


static const char *pname[3] = {"     ", "White", "Black"};


/* forward declarations */

static double estimate_time_by_move(int color, int move);




/* Echo a time value in STANDARD format */

static void
timeval_print(FILE *outfile, double tv)
{
  int     min;
  double  sec;

  min = (int) tv / 60;
  sec = tv - min*60;

  fprintf(outfile, "%3dmin %.2fsec ", min, sec);
}

/******************************/
/*  Initialization functions  */
/******************************/


/*
 * Initialize the structure.
 * -1 means "do not modify this value".
 * clock_init(-1, -1, -1) only resets the clock.
 */
void
clock_init(int time, int byo_time, int byo_stones)
{
  int color;

  if (time > 0) {
    clk.main_time = time;
    clk.ready = 1;
  }

  if (byo_time >= 0)
    clk.byoyomi_time = byo_time;

  if (byo_stones >= 0)
    clk.byoyomi_stones = byo_stones;

  clk.moveno = -1;
  for (color = WHITE ; color <= BLACK ; color++) {
    clk.timer[color] = 0;
    clk.btimer[color] = 0;
    clk.byoyomi[color] = 0;
    clk.dead[color] = 0;
  }
}


/*
 * Activate the clock.
 */
void 
clock_enable(void)
{
  gg_assert(clk.ready);
  clk.clock_on = 1;
}

/*
 * Activate Autolevel.
 */
void 
clock_enable_autolevel(void)
{
  gg_assert(clk.clock_on);
  clk.autolevel_on = 1;
}


/***********************/
/*  Access functions.  */
/***********************/


/* 
 * Maintain timers and all stuff up to date
 * (used by push_button).
 */
static void
clock_byoyomi_update(int color, double dt)
{
  gg_assert(clk.moveno > 0);

  /* update byoyomi timer */
  if (clk.byoyomi[color])
    clk.btimer[color] = clk.btimer[color] + dt;
 
  /* Check if player is just begining byoyomi. */
  if (clk.timer[color] < clk.main_time && !clk.byoyomi[color]) {
    clk.byoyomi[color] = clk.moveno;
    clk.btimer[color] = dt;
  }
  
  /* Check if player is time-out. */
  clk.dead[color] |= (clock_is_byoyomi(color)
		      && clk.byoyomi_time < clk.btimer[color]);

  /* Check byoyomi period reset. */
  if (clk.byoyomi[color]
      && clk.moveno - clk.byoyomi[color] == 2 * clk.byoyomi_stones - 1) {
    clk.byoyomi[color] = clk.moveno;
    clk.btimer[color] = 0;
  }
}


/*
 * Update the clock.
 */
void
clock_push_button(int color)
{
  double  now, dt, tme;

  if (!clk.clock_on)
    return;

  now = gg_gettimeofday();
  gg_assert(clk.ready);

  /* time/move estimation */
  tme = estimate_time_by_move(color, clk.moveno);

  /* first move */
  if (clk.moveno == -1) {
    /* fprintf(stderr, "clock: first move by %s.\n", pname[color]); */
    clk.moveno++;
    clk.date[0] = now;

    if (color != BLACK) { /* do an empty move for BLACK */
      clk.date[1] = now;
      clk.moveno++;
    }
    return;
  }

  /* fprintf(stderr, "clock: %s push the button.\n", pname[color]);*/
  /* fprintf(stderr, "clock: %s's turn.\n", pname[COLOR(clk.moveno+1)]);*/

  /* Pushing twice on the button does nothing. */
  if (color != COLOR(clk.moveno+1)) {
    fprintf(stderr, "clock: double push.\n");
    return;
  }

  /* Other moves (clk. moveno > -1) */
  clk.moveno++;
  gg_assert(clk.moveno < CLOCK_MAX_MOVES);
  clk.date[clk.moveno] = now;

  /* Update main timer. */
  dt = clk.date[clk.moveno] - clk.date[clk.moveno - 1];
  clk.timer[color] += dt;

  /* Estimate prediction error for next move. */
  if (clk.moveno > 11)
    clk.error = (clk.error + 2 * gg_abs(dt-tme))/3;
  else
    clk.error = 3.0;

  clock_byoyomi_update(color, dt);
  clock_print(color);
}


/*
 * Unplay a move.
 */
void 
clock_unpush_button(int color)
{
  double dt;

  if (!clk.clock_on)
    return;
     
  gg_assert(clk.ready);
  gg_assert(color == COLOR(clk.moveno));

  if (clk.moveno < 1) {
    clock_init(-1, -1, -1);
    return;
  }

  /* Update main timer. */
  dt = clk.date[clk.moveno] - clk.date[clk.moveno - 1];
  clk.timer[color] -= dt;
  clk.moveno--;

  /* Check if back from byoyomi. */
  if (clk.timer[color] < clk.main_time) {
    clk.byoyomi[color] = 0;
    clk.btimer[color] = 0;
  }
     
  /* Update byoyomi timer. */
  if (clk.byoyomi[color])
    clk.btimer[color] -= dt;

  clock_print(color);
}


/*
 * return the (exact) main timer value.
 */
double
clock_get_timer(int color)
{
  double dt;

  gg_assert(clk.clock_on && clk.ready);

  dt = gg_gettimeofday() - clk.date[clk.moveno];

  if (COLOR(clk.moveno) != color)
    return clk.timer[color] + dt;
  else 
    return clk.timer[color];
}


/*
 * Give the time left or negative if in byoyomi.
 */
double  
clock_get_time_left(int color)
{
  return clk.main_time - clock_get_timer(color);
}


/*
 * Check if color is (officially) in byoyomi.
 */
int
clock_is_byoyomi(int color)
{
  return clock_get_time_left(color) < 0;
}


/*
 * Return the (exact) main timer value.
 */
double
clock_get_btimer(int color)
{
  double dt;
  
  /* sanity check */
  gg_assert(clk.clock_on && clk.ready);
  dt = gg_gettimeofday() - clk.date[clk.moveno];

  if (COLOR(clk.moveno) != color)
    return clk.btimer[color] + dt;
  else 
    return clk.btimer[color];
}


/*
 * Get The Byoyomi time left and the number of stones to play.
 */
double
clock_get_btime_left(int color, int *stones)
{
  if (stones != NULL)
    *stones = clk.byoyomi_stones - (clk.moveno - clk.byoyomi[color]) / 2;

  return clk.byoyomi_time - clock_get_btimer(color);
}


/*
 * Check if a player is time over.
 */
int
clock_is_time_over(int color)
{
  return clock_is_byoyomi(color) && clock_get_btime_left(color, NULL) <= 0;
}


void
clock_print(int color)
{
  double tleft;
  int stones;

  if (!clk.clock_on)
    return;

  gg_assert(clk.ready);

  fprintf(stderr, "clock: "); 
  fprintf(stderr, "%s ", pname[color]);

  if (clock_is_time_over(color))
    fprintf(stderr, "TIME OUT! ");
  else {
    if (clock_is_byoyomi(color))
      tleft = clock_get_btime_left(color, &stones);
    else
      tleft = clock_get_time_left(color);
      
    if (clock_is_byoyomi(color)) {
      fprintf(stderr, "byoyomi");
      timeval_print(stderr, tleft);
      fprintf(stderr, "for %d stones.\n", stones);
    }
    else
      timeval_print(stderr, tleft);

  }
  fprintf(stderr, "\n");
}



/**********************/
/*  Autolevel system  */
/**********************/



/* Write the time/move to outfile */
void 
clock_report_autolevel(FILE *outfile, int color)
{
  int i, first;
  double dt, est, exp;

  if (!clk.autolevel_on)
    return;

  if (outfile == NULL)
    outfile = fopen("autolevel.dat", "w");
  if (outfile == NULL)
    return;

  fprintf(outfile, "#\n#  level  prediction   expected  time/move\n");  
  fprintf(outfile, "#-----------------------------------------\n"); 

  if (color == WHITE)
    first = 8;
  else
    first = 9;

  for (i = first; i < clk.moveno; i += 2) {
    dt = clk.date[i+1] - clk.date[i];
    est = estimate_time_by_move(color, i);
    exp = clk.expected[i + 1];
    fprintf(outfile, "%5.2f  %5.2f  %5.2f  %5.2f\n",
	    clk.levels[i + 1], est, exp, dt);
  }
}


/* 
 * Give an estimation of the time/move 
 * based on the last played move.
 */

/* coeficients to estimate time/move */
static const double 
coef[5] = {
  1.0/15.0, 2.0/15.0, 3.0/15.0, 4.0/15.0, 5.0/15.0 
};

static double
estimate_time_by_move(int color, int move)
{
  double res;
  int i;

  if (move <= 10)
    return 0;

  gg_assert(COLOR(move) == OTHER_COLOR(color));

  res = 0;
  for (i = 0 ; i < 5 ; i++)
    res += coef[i] * (clk.date[move-9+i*2] - clk.date[move-10+i*2]);
  
  return res;
}


/* 
 * Try to respect a "time contract". 
 */
static void 
respect_time_contract(int color)
{
  double time_left, expected_tm, predicted_tm;
  double moves_left;

  fprintf(stderr, "\n*** time contract:\n");

  predicted_tm = estimate_time_by_move(color, clk.moveno);

  /* Compute the expected mean time/move 
   * to respect the contract.
   */
  moves_left = (CLOCK_MOVE_CONTRACT(board_size) - clk.moveno) / 2.0;
  time_left = (clock_get_time_left(color)
	       - (1.0 - CLOCK_TIME_CONTRACT) * clk.main_time);
  expected_tm = time_left / moves_left;

  clk.expected[clk.moveno + 1] = expected_tm;

  fprintf(stderr, "%4.0f moves ", moves_left);
  fprintf(stderr, "must be played in %.2fsec\n", time_left);
  fprintf(stderr, "time/move: prediction=%.2f", predicted_tm);
  fprintf(stderr, "+/-%.2fsec --> ", clk.error);
  fprintf(stderr, "expected=%.2f\n", expected_tm);

  /* Compare this result with the prediction
   * (up to prediction error estimation)
   * and update the level.
   */ 
  if (clk.level > CLOCK_CONTRACT_MIN_LEVEL)
    if ((predicted_tm - clk.error) > expected_tm)
      clk.level -= CLOCK_STEP;
  if ((predicted_tm + clk.error) < expected_tm)
    clk.level += CLOCK_STEP;
}


/* 
 * Try to keep gnugo ahead on the clock. 
 */
static void
keep_ahead(int color)
{
  double dt, st, delta_max;

  fprintf(stderr, "*** safe limit reached: trying to keep ahead\n");

  dt = clock_get_time_left(color) - clock_get_time_left(OTHER_COLOR(color));
  st = clock_get_time_left(color) + clock_get_time_left(OTHER_COLOR(color));
  delta_max = gg_max(CLOCK_DELTA, CLOCK_DELTAR * st);

  fprintf(stderr, "deltamax: %gsec, delta=%gsec => ", delta_max, dt);
      
  if (dt < -delta_max) {
    fprintf(stderr, "behind\n");
    clk.level -= CLOCK_STEP;
  }
  else {
    if (dt > delta_max) {
      fprintf(stderr, "ahead\n");
      clk.level += CLOCK_STEP;
    }
    else
      fprintf(stderr, "equal\n");
  }
}


/* 
 * Modify the level during a game to avoid losing by time.
 */
void
clock_adapt_level(int *p_level, int color)
{
  int old_level;
  double hurry_limit, safe_limit;

  /* 
   * Do not touch the level during the first 10 moves
   * to estimate time/move on a reasonable sample.
   */
  if (clk.moveno < 10 || !clk.autolevel_on) {
    clk.level = *p_level;
    clk.levels[clk.moveno + 1] = clk.level;
    return;
  }

  old_level = *p_level;
  
  /* 
   * Hurry strategy:
   * Behind this limit the only priority is finish at all costs.
   */
  hurry_limit = gg_max(CLOCK_HURRY, CLOCK_HURRYR * clk.main_time);
  if (clock_get_time_left(color) < hurry_limit) {
    fprintf(stderr, "*** hurry limit reached:\n");
    clk.level = CLOCK_HURRY_LEVEL;
    *p_level = clk.level;
    return;
  }

  /* 
   * Time contract strategy:
   * try to respect the time of a standard game. 
   */
    if (clk.moveno < CLOCK_MOVE_CONTRACT(board_size))
      respect_time_contract(color);

  /* 
   * Keep ahead strategy:
   * When the safe_limit is reached gnugo tries to keep ahead in time.
   */
  safe_limit = gg_max(CLOCK_SAFE, CLOCK_SAFER * clk.main_time);
  if (clock_get_time_left(color) < safe_limit) 
    keep_ahead(color);

  /* Update the level. */
  if (clk.level > (double) max_level)
    clk.level = (double) max_level;
  if (clk.level < (double) min_level)
    clk.level = (double) min_level;

  clk.levels[clk.moveno + 1] = clk.level;
  *p_level = clk.level;
  
  fprintf(stderr, "level %4.1f at move %d\n", clk.level, movenum);
}



/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */





