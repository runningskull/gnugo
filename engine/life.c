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


/*
 * This file is about life & death.
 * 
 * For now, we only consider eye spaces.  Code for extending and
 * shrinking eye spaces is in owl.c. 
 *
 * We will only handle small to medium eye spaces for now (<= 16
 * during testing). At these small sizes, we make the simplifying
 * assumption that if any part of the surrounding wall is captured,
 * the resulting eye space will lead to 0 eyes. This assumption can be
 * dealt with later when the basic code works.
 *
 * Strategy:
 * Try to fill the entire eye space until there are only eyes of
 * size 1 left. Then nobody can play there, and we can count the eyes. */

/* FIXME: TODO
 *  - Handle ko
 *  - Handle seki
 *  - Try to find out how many outside liberties are needed for the optimal
 *    sequence.
 * - Try to find the optimal move. For example, consider this situation:
 *
 *      D E F G H J
 *      . . . X X X 9
 *      . . O X . O 8
 *      . O . X O . 7
 *      + O . X . O 6
 *      . O . X . X 5
 *      . . O X X . 4
 *      . . O O O X 3
 *      . . O . O . 2
 *      . . . . . . 1
 *
 *   In this situation, the current implementation suggests an X move at
 *   H8 as the defense move.  This works, but H6 is more efficient since 
 *   when the external liberties are filled in, we need to make at least
 *   one extra move if O plays H6.
 * - There is no need for all levels of the search to return attacking
 *   or defending moves. This is only needed at the top level. In fact,
 *   there is no need to store it into the hash table either, so the table
 *   could be halved in size (short instead of int).  However, I don't
 *   want to do this simplification yet.  Maybe handling of seki or ko
 *   will have to use some scheme like this.
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "liberty.h"


#define DEBUG_LIMIT   10

#define MAX_EYE_SIZE  16
#define MAX_BOUNDARY_STRINGS 20

/* List of eye point coordinates. */
static int eye[MAX_EYE_SIZE+1];

/* Array with inverse mapping. The index MAX_EYE_SIZE is by convention
 * used to encode a pass move.
 */
static int eyeindex[BOARDMAX];

/* Proper eye points, i.e. not margins or external diagonals. */
static int proper_eye[BOARDMAX];

/* Eye point restrictions. */
static int eye_restrictions[BOARDMAX];
#define DEFENDER_NOT_PLAY         0x01
#define ATTACKER_PLAY_SAFE        0x02
#define DEFENDER_PLAY_IF_CAPTURE  0x04

/* List of boundary strings. */
static int boundary[MAX_BOUNDARY_STRINGS];

static int eyesize;
static int eye_color;
static int boundary_size;


static int minimize_eyes(struct eye_data eyedata[BOARDMAX],
			 struct half_eye_data heye[BOARDMAX],
			 int *min, int *ko_out, int ko_in, int ko_master,
			 int *attack_point, int cutoff_eyes, int cutoff_ko);
static int maximize_eyes(struct eye_data eyedata[BOARDMAX],
			 struct half_eye_data heye[BOARDMAX],
			 int *max, int *ko_out, int ko_in, int ko_master,
			 int *defense_point, int cutoff_eyes, int cutoff_ko);
static void life_showboard(void);

/* Used by life_showboard(). */
static int stackp_when_called = 0;

/* Statistics. */
static int life_node_counter = 0;

/* ================================================================ */
/*                   Hash table for recognize_eye.                  */
/* ================================================================ */

/* FIXME: Change to dynamic allocation. */

#define EYEHASH_NODE_TABLE_SIZE 75000
#define EYEHASH_TABLE_SIZE 24999

struct eyehash_node {
  unsigned int hash;
  int result;
  struct eyehash_node *next;
};

struct eyehash_node eyehash_node_table[EYEHASH_NODE_TABLE_SIZE];

static int next_eyehash_node;

struct eyehash_node *eyehash_table[EYEHASH_TABLE_SIZE];

/*
 * Layout of the result field:
 *
 *   Each result is one 32 bit integer. (FIXME: Don't waste 32 bits on 
 *   machines with 64 bit ints.) 
 *
 *   byte 0 (LSB):  Value byte (see below)
 *   byte 1:        Status byte (see below)
 *   byte 2:        Where the defender should play.
 *   byte 3:        Where the attacker should play.
 *
 *   The value byte:
 *   +-------------------------------+
 *   | 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 |
 *   +-------------------------------+
 *   |   ko  |  eyes |   ko  |  eyes |
 *   +-------------------------------+
 *   |     attack    |     defense   |
 *   +-------------------------------+
 *
 *   The byte contains the same info twice, once for the attacker, 
 *   and once for the defender (the eye owner).
 *
 *   eyes:
 *     0:   0 eyes
 *     1:   1 eye
 *     2:   Local seki
 *     3:   2 eyes
 *
 *   ko: The komaster has ignored this number (0-3) ko threats to
 *       achieve the stated number of eyes. More than 3 ignored ko threats
 *       are never considered.
 *
 *   The status byte:
 *   +-------------------------------+
 *   | 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 |
 *   +-------------------------------+
 *   |     Reserved  | AI| DI| AR| DR|
 *   +-------------------------------+
 *
 *   Currently four status bits are used. Being set to one has the
 *   following interpretations.
 *     DR:  Defense result is ready.
 *     AR:  Attack result is ready.
 *     DI:  Defense result is invalid because we're still searching.
 *     AI:  Attack result is invalid because we're still searching.
 *          These are used to detect loops other than basic ko.
 *
 */

#define GET_DEFENSE_EYES(x)  (((x)->result)             & 0x03)
#define GET_DEFENSE_KO(x)    (((x)->result >> 2)        & 0x03)
#define GET_ATTACK_EYES(x)   (((x)->result >> 4)        & 0x03)
#define GET_ATTACK_KO(x)     (((x)->result >> 6)        & 0x03)
#define DEFENSE_READY_BIT    0x0100
#define ATTACK_READY_BIT     0x0200
#define DEFENSE_INVALID_BIT  0x0400
#define ATTACK_INVALID_BIT   0x0800
#define GET_DEFENSE_POINT(x) (((x)->result >> 16)       & 0xff)
#define GET_ATTACK_POINT(x)  (((x)->result >> 24)       & 0xff)

#define SET_DEFENSE(x, ko, eyes, point)\
((x)->result &= ~0x00ff050f,\
 (x)->result |= (point<<16) | DEFENSE_READY_BIT | (ko<<2) | (eyes))

#define SET_ATTACK(x, ko, eyes, point)\
((x)->result &= ~0xff000af0,\
 (x)->result |= (point<<24) | ATTACK_READY_BIT | (ko<<6) | (eyes<<4))


/*
 * Clear the hash table.
 */

static void
eyehash_clear(void)
{
  memset(eyehash_table, 0, sizeof(eyehash_table));
  next_eyehash_node = 0;
}

/* FIXME: This should be possible to do incrementally. */
static unsigned int
compute_hashvalue(void)
{
  int k;
  unsigned int hash = 0;
  for (k=0; k<eyesize; k++)
    hash |= board[eye[k]] << 2*k;

  return hash;
}

/* Get a node from the eyehash table. NULL is returned if the node was
 * invalid or the table is out of space.
 */
static struct eyehash_node *
get_eyehash_node(int attack)
{
  unsigned int hash = compute_hashvalue();
  unsigned int key;
  struct eyehash_node *p;
  struct eyehash_node *last_p;
  int invalid_bit = (attack ? ATTACK_INVALID_BIT : DEFENSE_INVALID_BIT);

  /* Add the ko point to the hash value. */
  if (board_ko_pos != NO_MOVE && eyeindex[board_ko_pos] >= 0)
    hash |= 3 << (2 * eyeindex[board_ko_pos]);
  key = hash % EYEHASH_TABLE_SIZE;

  /* Search through the linked list for this entry. */
  for (last_p = NULL, p = eyehash_table[key]; p; last_p = p, p = p->next)
    if (p->hash == hash)
      break;

  /* Position found in the hash table. */
  if (p) {
    if (p->result & invalid_bit)
      return NULL; /* But the result was invalid. */
    else {
      int ready_bit = (attack ? ATTACK_READY_BIT : DEFENSE_READY_BIT);
      /* Mark the invalid bit before returning the node, unless the
       * result is ready.
       */
      if (!(p->result & ready_bit))
	p->result |= invalid_bit;
      return p;
    }
  }

  /* Check if we have node space left. */
  if (next_eyehash_node == EYEHASH_NODE_TABLE_SIZE)
    return NULL;

  p = &eyehash_node_table[next_eyehash_node++];
  p->hash = hash;
  p->result = invalid_bit;
  p->next = NULL;
  /* Link in the new node. */
  if (last_p)
    last_p->next = p;
  else
    eyehash_table[key] = p;

  return p;
}


/* ================================================================ */
/*                          Prepare the eyespace                    */
/* ================================================================ */


static void
clear_eyepoints(void)
{
  int i, j;
  int ii;

  eyesize = 0;
  for (i = 0; i < board_size; ++i)
    for (j = 0; j < board_size; ++j) {
      ii = POS(i, j);

      eyeindex[ii] = -1;
      proper_eye[ii] = 0;
      eye_restrictions[ii] = 0;
    }
}

static void
include_eyepoint(int pos, int proper, int restrictions)
{
  if (eyeindex[pos] == -1) {
    /* FIXME: This is only a temporary workaround to avoid a crash in
     * the assertion below.
     */
    if (eyesize > MAX_EYE_SIZE)
      return;
    /* FIXME:
     * Should this be < MAX_EYE_SIZE?
     * The list of points in eye[] is ended with (NO_MOVE)
     */
    ASSERT1(eyesize <= MAX_EYE_SIZE, pos);
    eyeindex[pos] = eyesize;
    eye[eyesize] = pos;
    eyesize++;
  }
  if (proper)
    proper_eye[pos] = 1;

  eye_restrictions[pos] |= restrictions;
}


/* Explanations can be found above the calls in prepare_eyespace(). */
static void
check_vulnerability(int marginal_eyepoint, int stone)
{
  if (!ON_BOARD(stone))
    return;
  
  if (board[stone] == EMPTY) {
    if (eyeindex[stone] == NO_MOVE)
      include_eyepoint(stone, 0, DEFENDER_PLAY_IF_CAPTURE);
    return;
  }
  
  /* Both eyespace and boundary are excluded. */
  if (eyeindex[stone] != -1)
    return;

  /* Play the opposite color of (m, n) on the margin and try to
   * capture (stone)
   */
  if (!trymove(marginal_eyepoint, OTHER_COLOR(board[stone]),
	       "check_vulnerability", stone, EMPTY, NO_MOVE))
    return;

  if (board[stone] && attack(stone, NULL)) {
    int liberties;
    int libs[2];
    /* Vulnerability found. First pick up its liberties. */
    liberties = findlib(stone, 2, libs);
    
    if (board[stone] == eye_color && liberties == 1) {
      /* Strategy (c). */
      include_eyepoint(stone, 0, 0);
      include_eyepoint(libs[0], 0, DEFENDER_NOT_PLAY);
    }
    else if (board[stone] == OTHER_COLOR(eye_color)) {
      if (liberties > 1
	  || countstones(stone) > 6
	  || eyesize + countstones(stone) + 1 > MAX_EYE_SIZE) {
	/* Strategy (b). */
	include_eyepoint(marginal_eyepoint, 0, ATTACKER_PLAY_SAFE);
      }
      else {
	/* Strategy (a) */
	int k;
	int stones[6];
	int size = findstones(stone, 6, stones);
	gg_assert(size <= 6);
	for (k=0; k<size; k++)
	  include_eyepoint(stones[k], 0, 0);
	include_eyepoint(libs[0], 0, DEFENDER_PLAY_IF_CAPTURE);
      }
    }
  }
  popgo();
}


static void
print_eyespace(struct eye_data eyedata[BOARDMAX],
	       struct half_eye_data heye[BOARDMAX])
{
  int m, n;
  int ii;
  int k;
  int mini, maxi;
  int minj, maxj;
  
  /* Determine the size of the eye. */
  mini = board_size;
  maxi = -1;
  minj = board_size;
  maxj = -1;
  for (k = 0; k < eyesize; k++) {
    m = I(eye[k]);
    n = J(eye[k]);
    if (m < mini) mini = m;
    if (m > maxi) maxi = m;
    if (n < minj) minj = n;
    if (n > maxj) maxj = n;
  }

  /* Prints the eye shape. A half eye is shown by h, if empty or H, if an
   * enemy is present. Note that each half eye has a marginal point which is 
   * not printed, so the representation here may have less points than the 
   * matching eye pattern in eyes.db. Printing a marginal for the half eye
   * would be nice, but difficult to implement.
   */
  for (m = mini; m <= maxi; m++) {
    gprintf(""); /* Get the indentation right. */
    for (n = minj; n <= maxj; n++) {
      ii = POS(m, n);

      if (eyeindex[ii] >= 0) {
	if (board[ii] == EMPTY) {
	  if (eyedata[ii].marginal)
	    gprintf("%o!");
	  else if (is_halfeye(heye, ii))
	    gprintf("%oh");
	  else
	    gprintf("%o.");
	}
	else if (is_halfeye(heye, ii))
	  gprintf("%oH");
	else
	  gprintf("%oX");
      }
      else
	gprintf("%o ");
    }
    gprintf("\n");
  }
}


static int
prepare_eyespace(int pos, struct eye_data eyedata[BOARDMAX],
		 struct half_eye_data heye[BOARDMAX])
{
  int i, j;
  int ii;
  int k;
  
  /* Set `eye_color' to the owner of the eye. */
  eye_color = eyedata[pos].color;
  if (eye_color == BLACK_BORDER)
    eye_color = BLACK;
  if (eye_color == WHITE_BORDER)
    eye_color = WHITE;

  clear_eyepoints();
  
  /* Collect all the points of the eye into the set. 
   * We must also include all diagonal points for half eyes.
   * FIXME: This won't work if diagonal attack and defense points differ.
   */
  for (i = 0; i < board_size; ++i)
    for (j = 0; j < board_size; ++j) {
      ii = POS(i, j);

      if (eyedata[ii].origin == pos) {
	include_eyepoint(ii, eyedata[ii].marginal == 0, 0);
	if (is_halfeye(heye, ii)) {
	  for (k=0; k<heye[ii].num_attacks; k++)
	    include_eyepoint(heye[ii].attack_point[k], 0, 0);
	  for (k=0; k<heye[ii].num_defends; k++)
	    include_eyepoint(heye[ii].defense_point[k], 0, 0);
	}
      }
    }

  /* Verify that the effective eyespace isn't too large. */
  if (eyesize > MAX_EYE_SIZE)
    return 0;

  /* Get the boundary. The boundary points are defined to have the
   * properties:
   * 1. The same color as the eyespace.
   * 2. Not itself part of the eyespace.
   * 3. Adjacent to a non-marginal point in the eyespace.
   *
   * We only store the origins of the boundary strings.
   */
  boundary_size = 0;
  for (i = 0; i < board_size; ++i)
    for (j = 0; j < board_size; ++j) {
      ii = POS(i, j);

      if (board[ii] != eye_color)
	continue;
      if (eyeindex[ii] >= 0)
	continue;
      if ((   i > 0	       && proper_eye[ii-NS])
	  || (i < board_size-1 && proper_eye[ii+NS])
	  || (j > 0	       && proper_eye[ii-1])
	  || (j < board_size-1 && proper_eye[ii+1])) {
	int origin = find_origin(ii);
	if (eyeindex[origin] != -2) {
	  eyeindex[origin] = -2;
	  boundary[boundary_size] = origin;
	  boundary_size++;
	  gg_assert(boundary_size <= MAX_BOUNDARY_STRINGS);
	}
      }
    }

  /* Go through the eyespace in search of vulnerable margins. These
   * are defined as having a neighbor outside the eyespace that can be
   * captured after the neighbor plays on the margin ('!' in the
   * examples below.
   *
   *            OOOXX
   * OOOOX   OOOOXX..   OOOXX
   * OX!X.   OX!XX...   ..!O.
   * -----   --------   -----
   *  (a)       (b)          (c)
   *
   * The solution to this problem is in (a) to add the vulnerable X
   * string and its liberty to the eyespace. We also add the
   * restriction that O may only play on this liberty if the move
   * captures at least one stone in the eyespace.
   *
   * In (b) this approach doesn't easily work so instead we leave the
   * eyespace unchanged but add the restriction that X must not play
   * on the margin if the stone can then be tactically captured. This
   * approach is also used in (a) if addition of the extra points
   * would make the eyespace too large.
   *
   * In (c) we require that the vulnerable stone has no more than two
   * liberties and then add the *stone* and its external liberty to the
   * eyespace. Only X may play on the external liberty.
   *
   * FIXME: This is not very exact. A better scheme would involve the
   * concept of moves that are sente against the outside, i.e. the
   * attacker is effectively forced to pass once when such moves are
   * played.
   */

  for (k=0; k<eyesize; k++) {
    ii = eye[k];
    if (!eyedata[ii].marginal)
      continue;
    /* Found a margin. Now look for a vulnerable stone outside the
     * eyespace.
     */
    check_vulnerability(ii, ii-NS);
    check_vulnerability(ii, ii+NS);
    check_vulnerability(ii, ii-1);
    check_vulnerability(ii, ii+1);
  }

  /* Verify that the effective eyespace still isn't too large. */
  if (eyesize > MAX_EYE_SIZE)
    return 0;

  /* Clear the hash table for this eye. */
  eyehash_clear();

  /* Add a pass move at the end of the eyespace list. */
  eye[MAX_EYE_SIZE] = NO_MOVE;

  if (debug & DEBUG_EYES) {
    gprintf("Prepared eyespace:\n");
    print_eyespace(eyedata, heye);
  }
    
  return 1;
}  

/* ================================================================ */
/*                          Eye recognition                         */
/* ================================================================ */

/*
 * Compute the maximum and minimum number of eyes reachable from the
 * eye space at (m, n). The maximum is defined for color to play
 * first, and the minimum is defined for other_color to play first.
 *
 * If max != min, (*attacki, *attackj) is set to the move which
 * reduces the eye and (*defendi, *defendj) is set to the move which
 * defends it. Most of the time the attack point and the defense point
 * will be the same.
 *
 * The function returns one on success and zero if the eyespace was
 * too large for analysis.
 */

int
recognize_eye2(int pos, int *attack_point, int *defense_point,
	       int *max, int *min, 
	       struct eye_data eyedata[BOARDMAX],
	       struct half_eye_data heye[BOARDMAX],
 	       int add_moves, int color)
{
  int result1a, result1b, result2a, result2b;
  int min1a, min1b, max2a, max2b;
  int ko1a, ko1b, ko2a, ko2b;
  int attack_point1a, attack_point1b;
  int defense_point2a, defense_point2b;
  int save_stackp;
  int m = I(pos);
  int n = J(pos);

  /* FIXME: Remove the unused parameter entirely if it's not needed. */
  UNUSED(color);
  UNUSED(add_moves);

  save_stackp = stackp;

  /* For use by debug outputs. */
  stackp_when_called = stackp;
  
  /* Set up all data structures necessary to analyze the eyespace. */
  if (!prepare_eyespace(POS(m, n), eyedata, heye))
    return 0;

  DEBUG(DEBUG_EYES, "================  MINIMIZE 1 ================\n");
  result1a = minimize_eyes(eyedata, heye, &min1a, &ko1a, 0,
			   eye_color, &attack_point1a, 0, 0);
  DEBUG(DEBUG_EYES, "================  MAXIMIZE 1 ================\n");
  result2a = maximize_eyes(eyedata, heye, &max2a, &ko2a, 0,
			   eye_color, &defense_point2a, 2, 0);

  /* The hash value currently doesn't include the komaster. As a
   * workaround we clear the cache before switching komaster.
   *
   * Q: The hash value now includes the komaster. Can we
   *    therefore skip this step? 
   *
   * A: No, the life code has its own cache, which doesn't
   *    include the komaster.
   */

  eyehash_clear();
  
  DEBUG(DEBUG_EYES, "================  MINIMIZE 2 ================\n");
  result1b = minimize_eyes(eyedata, heye, &min1b, &ko1b, 0,
			   OTHER_COLOR(eye_color), &attack_point1b, 0, 0);

  DEBUG(DEBUG_EYES, "================  MAXIMIZE 2 ================\n");
  result2b = maximize_eyes(eyedata, heye, &max2b, &ko2b, 0,
			   OTHER_COLOR(eye_color), &defense_point2b, 2, 0);

  if (!result1a)
    return 0;
  
  if (!result2b)
    return 0;

  DEBUG(DEBUG_EYES, "Min: ko_master %C, eyes=%d, ko=%d, attack: %1m\n",
	eye_color, min1a, ko1a,	eye[attack_point1a]);
  
  DEBUG(DEBUG_EYES, "Min: ko_master %C, eyes=%d, ko=%d, attack: %1m\n",
	OTHER_COLOR(eye_color), min1b, ko1b, eye[attack_point1b]);
  
  DEBUG(DEBUG_EYES, "Max: ko_master %C, eyes=%d, ko=%d, defense: %1m\n",
	eye_color, max2a, ko2a,	eye[defense_point2a]);
  
  DEBUG(DEBUG_EYES, "Max: ko_master %C, eyes=%d, ko=%d, defense: %1m\n",
	OTHER_COLOR(eye_color), max2b, ko2b, eye[defense_point2b]);
  
  *min = min1a;
  *max = max2b;

  /* Ignore the distinction between seki and two proper eyes for now. */
  if (*min == 3)
    *min = 2;
  if (*max == 3)
    *max = 2;
  
  if (*min != *max) {
    if (attack_point)
      *attack_point = eye[attack_point1a];
    if (defense_point)
      *defense_point = eye[defense_point2b];
    DEBUG(DEBUG_EYES, "  vital point (attack):  %1m\n", *attack_point);
    DEBUG(DEBUG_EYES, "  vital point (defense): %1m\n", *defense_point);
  }

  /* FIXME: Currently the rest of the life and death analysis can't
   * deal with chimeras. As a workaround we report the eyespace as one
   * and a half eye instead.
   */
  if (*max - *min == 2)
    *min = 1;
  
  gg_assert(stackp == save_stackp);

  return 1;
}


/* Return true if this is an eye of size one. Usually you would also
 * want to check that an opponent move here would be suicide.
 */
static int
is_small_eye(int pos)
{
  if (!(   (!ON_BOARD(pos-NS) || board[pos-NS] == eye_color)
	&& (!ON_BOARD(pos+NS) || board[pos+NS] == eye_color)
	&& (!ON_BOARD(pos-1)  || board[pos-1] == eye_color)
	&& (!ON_BOARD(pos+1)  || board[pos+1] == eye_color)))
    return 0;
  return 1;
}

/* Assuming that this is an eye of size one, return true if it's not
 * topologically false and not a marginal eye point.
 */
static int
is_true_eye(struct half_eye_data heye[BOARDMAX], int pos)
{
  int other = OTHER_COLOR(eye_color);
  
  /* False eyes and other marginal eye points do not yield eyes. */
  if (heye[pos].type == FALSE_EYE)
    return 0;
  
  /* If this is a halfeye, check the vital diagonals. */
  if (is_halfeye(heye, pos)) {
    int good = 0;
    int bad = 0;
    int k;
    /* This requires attack points and defense points to be identical. */
    for (k=0; k<heye[pos].num_attacks; k++) {
      int aa = heye[pos].attack_point[k];

      if (board[aa] == eye_color)
	good++;
      else if (board[aa] == other)
	bad++;
      else if (is_suicide(aa, eye_color))
	bad++;
      else
	good++;
    }
    if (bad > good)
      return 0;
  }

  /* This really looks like an eye. */
  return 1;
}


/* Returns 1 if at least one string in the eyespace is captured when
 * color plays at (pos).
 */
static int
life_does_capture_something(int pos, int color)
{
  int other = OTHER_COLOR(color);
  int k;

  for (k=0; k<4; k++) {
    int d = delta[k];

    if (board[pos+d] == other
	&& countlib(pos+d) == 1
	&& eyeindex[pos+d] >= 0)
      return 1;
  }

  return 0;
}


/* Compare two (eyes, ko) values from the point of view of
 * minimize_eyes (the attacker). Returns
 * -1    if the first value is considered better
 *  0    if the values are the same
 *  1    if the second value is considered better
 *
 * The general rule is that a smaller eye value is better than a
 * larger and if they are equal it's better to have a smaller ko
 * value. The exception is that seki without ko is considered worse
 * than two eyes with ko.
 */

static int
compare_min_eyes(int eyes1, int ko1, int eyes2, int ko2)
{
  /* Seki without ko against 2 eyes with ko. */
  if (eyes1 == 2 && eyes2 == 3 && ko1 < ko2)
    return 1;
  
  /* 2 eyes with ko against seki without ko. */
  if (eyes1 == 3 && eyes2 == 2 && ko1 > ko2)
    return -1;

  /* General case */
  if (eyes1 < eyes2)
    return -1;

  if (eyes1 > eyes2)
    return 1;
  
  /* Same number of eyes. */
  if (ko1 < ko2)
    return -1;

  if (ko1 > ko2)
    return 1;

  /* Same value. */
  return 0;
}
  

/* Compare two (eyes, ko) values from the point of view of
 * maximize_eyes (the defender). Returns
 * -1    if the first value is considered better
 *  0    if the values are the same
 *  1    if the second value is considered better
 *
 * The general rule is that a larger eye value is better than a
 * smaller and if they are equal it's better to have a smaller ko
 * value. The exception is that seki without ko is considered better
 * than two eyes with ko.
 */

static int
compare_max_eyes(int eyes1, int ko1, int eyes2, int ko2)
{
  /* Seki without ko against 2 eyes with ko. */
  if (eyes1 == 2 && eyes2 == 3 && ko1 < ko2)
    return -1;
  
  /* 2 eyes with ko against seki without ko. */
  if (eyes1 == 3 && eyes2 == 2 && ko1 > ko2)
    return 1;

  /* General case */
  if (eyes1 < eyes2)
    return 1;

  if (eyes1 > eyes2)
    return -1;
  
  /* Same number of eyes. */
  if (ko1 < ko2)
    return -1;

  if (ko1 > ko2)
    return 1;

  /* Same value. */
  return 0;
}
  


#define MINIMIZE_EYES_RETURN(eyes, ko, attack, message) do {\
  *min = (eyes);\
  *ko_out = (ko);\
  if (attack_point)\
    *attack_point = (attack);\
  SET_ATTACK(cache_entry, (ko), (eyes), (attack));\
  if (stackp - stackp_when_called < DEBUG_LIMIT)\
    DEBUG(DEBUG_LIFE,\
         "exiting minimize_eyes (%s) - result = %d, ko = %d, move %1m (%H)\n",\
	  (message), (eyes), (ko), eye[attack],\
	  hashdata.hashval);\
  return 1;\
  } while(0)

/*
 * We have a set of locations in eyepoints forming an eye shape.
 * Check min number of eyes we can get if the opponent moves first.
 * `eye_color' is the owner of the eye.
 *
 * Return values:
 *   1: Ok. Min eyes in *min.
 *   0: No result.  A cycle in the reading or hash table full.
 */

static int
minimize_eyes(struct eye_data eyedata[BOARDMAX],
	      struct half_eye_data heye[BOARDMAX],
	      int *min, int *ko_out, int ko_in, int ko_master,
	      int *attack_point, int cutoff_eyes, int cutoff_ko)
{
  int other = OTHER_COLOR(eye_color);
  int ii;
  int num_other;
  int num_moves;
  int num_eyes;
  int max1;
  int localmin;
  int localko;
  int ko1;
  int result;
  int attack_point1;
  int defense_point1;
  int localattack = MAX_EYE_SIZE;
     
  int move[MAX_EYE_SIZE+1];
  int move_score[MAX_EYE_SIZE+1];
  int k;
  int score;
     
  int save_stackp;

  struct eyehash_node *cache_entry;
  
  life_node_counter++;
  
  if (stackp - stackp_when_called < DEBUG_LIMIT)
    DEBUG(DEBUG_LIFE, "entering minimize_eyes: stackp = %d node number = %d\n",
	  stackp, life_node_counter);

  /* Check the hash table and see if we have been here before. */
  cache_entry = get_eyehash_node(1);
  if (!cache_entry) {
    /* Invalid result. We have come back to the same position again
     * within the searching. Just return and hope that we get a better
     * result on some other branch. */
    if (stackp - stackp_when_called < DEBUG_LIMIT)
      DEBUG(DEBUG_LIFE,
	    "exiting minimize_eyes directly because of a reading loop. (%H)\n",
	    hashdata.hashval);
    return 0;
  }

  if (cache_entry->result & ATTACK_READY_BIT) {
    *min = GET_ATTACK_EYES(cache_entry);
    *ko_out = GET_ATTACK_KO(cache_entry);
    attack_point1 = GET_ATTACK_POINT(cache_entry);
    
    if (attack_point)
      *attack_point = attack_point1;
    
    if (stackp - stackp_when_called < DEBUG_LIMIT)
      DEBUG(DEBUG_LIFE, "exiting minimize_eyes - got result %d eyes, %d ko (move %1m) from the cache. (%H)\n",
	    *min, *ko_out, eye[attack_point1], hashdata.hashval);
    return 1;
  }

  
  /* Check whether any part of the boundary is in atari.  If so, we assume
   * capturing it guarantees no eyes.
   *
   * FIXME: This is too simplified.
   */
  for (k=0; k<boundary_size; k++) {
    int libs[2];
    if (findlib(boundary[k], 2, libs) == 1) {
      int index = eyeindex[libs[0]];
      /* If the move is outside the eyespace, return a pass.
       * FIXME: This is of course just a workaround.
       */
      if (index < 0)
	index = MAX_EYE_SIZE;
      MINIMIZE_EYES_RETURN(0, 0, index, "boundary captured");
    }
  }
  
  
  if (verbose
      && stackp - stackp_when_called < DEBUG_LIMIT
      && (debug & DEBUG_EYES))
    life_showboard();

  /* Collect how many moves the attacking color can do and how many stones
   * he has within the eye.
   */
  num_other = 0;
  num_moves = 0;
  num_eyes = 0;
  for (k=0; k<eyesize; k++) {
    int ii = eye[k];

    if (board[ii] == eye_color)
      continue;

    if (board[ii] == other) {
      if (proper_eye[ii])
	num_other++;
      continue;
    } 

    if (!is_suicide(ii, other)) {
      score = 0;
      move[num_moves] = k;
      /* Score the move. We give (preliminarily)
       * 1 point for each empty neighbor
       * 1 point for each friendly neighbor
       * 2 points if the intersection is marginal
       *
       * Assuming this is a legal move, we will get a score larger
       * than zero. The score can obviously not become larger than
       * six, but it seems unlikely we would ever exceed four.
       *
       * Looking closer, the score can actually become zero below, but
       * only if the move is fully surrounded by opponent stones and
       * captures at least one of them. This is likely to be good, so
       * we give a high score.
       */
      if (I(ii) > 0            && board[ii-NS] != eye_color)
	score++;
      if (I(ii) < board_size-1 && board[ii+NS] != eye_color)
	score++;
      if (J(ii) > 0            && board[ii-1] != eye_color)
	score++;
      if (J(ii) < board_size-1 && board[ii+1] != eye_color)
	score++;
      if (eyedata[ii].marginal)
	score += 2;
      if (score == 0)
	score = 5;
      move_score[num_moves] = score;
      num_moves++;
    }
    else {
      if (is_small_eye(ii) && is_true_eye(heye, ii))
	num_eyes++;
    }
  }

  /* Check if we are done. This means that there are no stones of
   * the attacking color and that all the eye points are suicide
   * to move into. The only way this could happen is that they are
   * of size 1 and without possibility to capture anything.
   *
   * This test is hard to get both robust and efficient. Currently
   * disabled.
   */
#if 0
  if (num_other == 0 && num_moves == 0) {
    if (num_eyes >= 2)
      num_eyes = 3;
    MINIMIZE_EYES_RETURN(num_eyes, 0, MAX_EYE_SIZE, "end of search");
  }
#endif
  if (num_moves == 0 && num_eyes >= 2)
    MINIMIZE_EYES_RETURN(3, 0, MAX_EYE_SIZE, "end of search");

  localmin = 9999;
  localko = 9999;

  /* Check against array overflow below.*/
  gg_assert(num_moves < (int) (sizeof(move)/sizeof(move[0])));
  
  /* Add a pass move with score 2 to be tested. We refrain from
   * increasing the num_legal_moves value because it's needed again
   * later
   */
  move[num_moves] = MAX_EYE_SIZE;
  move_score[num_moves] = 2;
  
  /* Now try all legal moves and see what we get. We order the moves
   * by score.
   */
  for (score=6; score>0; score--) {
    for (k=0; k<num_moves+1; k++) {
      int is_ko = 0;
      int index = move[k];
      
      if (move_score[k] != score)
	continue;

      ii = eye[index];

      gg_assert(ii == NO_MOVE || board[ii] == EMPTY);

      /* Try the move and see if we can reduce the eyes. */
      save_stackp = stackp;
      if (stackp - stackp_when_called < DEBUG_LIMIT)
	DEBUG(DEBUG_LIFE, "minimize_eyes: trymove %s %1m score %d\n",
	      color_to_string(other), ii, score);
      if (index == MAX_EYE_SIZE
	  || trymove(ii, other, "minimize_eyes", NO_MOVE, EMPTY, NO_MOVE)
	  || (ko_master == other
	      && (ko_in < 3)
	      && (is_ko = 1)   /* Intentional assignment. */
	      && tryko(ii, other, "minimize_eyes", EMPTY, NO_MOVE))) {

	/* The attacker has made his move. Now let's answer him and
	 * see how many eyes we can get.
	 */
	  
	/* But first we must check a restriction. */
	if (index != MAX_EYE_SIZE
	    && (eye_restrictions[ii] & ATTACKER_PLAY_SAFE)
	    && attack(ii, NULL))
	  result = 0;
	else
	  result = maximize_eyes(eyedata, heye, &max1, &ko1, ko_in + is_ko,
				 ko_master, &defense_point1,
				 localmin, localko);
	
	if (result == 1) {
	  /* Is this result an improvement of what we already have?
	   * Seki without ko is worse than two eyes with ko.
	   * Otherwise we primarily try to minimize the eye value.
	   */
	  if (compare_min_eyes(localmin, localko, max1, ko1 + is_ko) == 1) {
	    localmin = max1;
	    localko = ko1 + is_ko;
	    localattack = index;
	  }
	}
	
	if (index != MAX_EYE_SIZE)
	  popgo();
      }
      else {
	/* Illegal ko capture. */
	if (stackp - stackp_when_called < DEBUG_LIMIT)
	  DEBUG(DEBUG_LIFE,
		"  illegal ko capture: %s %1m ko master %s ko level %d\n",
		color_to_string(eye_color), ii, color_to_string(ko_master),
		ko_in);
      }

      gg_assert(stackp == save_stackp);

      /* Check if the the move yielded 0 eyes.
       * If so, we can as well return here. 
       */
      if (localmin == 0 && localko == 0)
	MINIMIZE_EYES_RETURN(0, 0, index, "cutoff");

      /* Check if we have reached to cutoff value. In that case we can
       * also return here since the maximizer won't be interested in a
       * smaller value anyway. We use compare_max_eyes to evaluate the
       * value from the maximizer's point of view.
       */
#if 1
      if (compare_max_eyes(localmin, localko, cutoff_eyes, cutoff_ko) == 1)
	MINIMIZE_EYES_RETURN(localmin, localko, index, "cutoff");
#endif
    }
  }

  /* If we didn't find anywhere to play, see how many eye spaces there were. */
  if (localmin == 9999) {
    if (num_moves > 0) {
      /* We didn't get a result even though there were legal moves.
       * This has to be because of a reading loop. Propagate the
       * non-result further up.
       *
       * FIXME: Not sure this is correct anymore.
       */
      return 0;
    }

    gg_assert(0);			/* Shouldn't get here. */
  }

  if (localmin > 3)
    localmin = 3;

  MINIMIZE_EYES_RETURN(localmin, localko, localattack, "all moves tested");
}


/*
 * We have a set of locations in eyepoints forming an eye shape.
 * Check max number of eyes we can get if the opponent moves first.
 * `eye_color' is the owner of the eye.
 *
 * Return values:
 *   1: Ok. Max eyes in *max.
 *   0: No result. A cycle in the reading.
 */

static int
maximize_eyes(struct eye_data eyedata[BOARDMAX],
	      struct half_eye_data heye[BOARDMAX],
	      int *max, int *ko_out, int ko_in, int ko_master,
	      int *defense_point, int cutoff_eyes, int cutoff_ko)
{
  int other = OTHER_COLOR(eye_color);
  int ii;
  int min1;
  int localmax;
  int localko;
  int local_defense = MAX_EYE_SIZE;
  int num_eyes;
  int result;
  int defense_point1;
  int ko1;
     
  int move[MAX_EYE_SIZE];
  int move_score[MAX_EYE_SIZE];
  int num_moves = 0;
  int k;
  int score;
     
  int save_stackp;

  struct eyehash_node *cache_entry;

  life_node_counter++;
  
  if (stackp - stackp_when_called < DEBUG_LIMIT)
    DEBUG(DEBUG_LIFE, "entering maximize_eyes: stackp = %d node number = %d\n",
	  stackp, life_node_counter);

  /* Check the hash table and see if we have been here before. */
  cache_entry = get_eyehash_node(0);
  if (!cache_entry) {
    /* Invalid result. We have come back to the same position again
     * within the searching or run out of cache space. Just return and
     * hope that we get a better result on some other branch.
     *
     * This should actually never happen here since maximize_eyes() is
     * always called first.
     */
    gg_assert(1);
    if (stackp - stackp_when_called < DEBUG_LIMIT)
      DEBUG(DEBUG_LIFE,
	    "exiting maximize_eyes directly because of a reading loop. (%H)\n",
	    hashdata.hashval);
    return 0;
  }

  if (cache_entry->result & DEFENSE_READY_BIT) {
    *max = GET_DEFENSE_EYES(cache_entry);
    *ko_out = GET_DEFENSE_KO(cache_entry);
    defense_point1 = GET_DEFENSE_POINT(cache_entry);
    
    if (defense_point)
      *defense_point = defense_point1;
    
    if (stackp - stackp_when_called < DEBUG_LIMIT)
      DEBUG(DEBUG_LIFE, "exiting maximize_eyes - got result %d eyes, %d ko (move %1m) from the cache. (%H)\n",
	    *max, *ko_out, eye[defense_point1], hashdata.hashval);
    return 1;
  }

  if (verbose
      && stackp - stackp_when_called < DEBUG_LIMIT
      && (debug & DEBUG_EYES))
    life_showboard();


  localmax = -1;
  localko  = 4;
  num_eyes = 0;

  /* Collect all possible moves and see what we get. */
  for (k=0; k<eyesize; k++) {
    int ii = eye[k];
    
    if (board[ii] != EMPTY)
      continue;

    /* If the eye is of size 1, of the eye owners color, and 
     * none of the neighbouring strings is in atari, then this
     * is a proper eye. Don't play there.
     *
     * gf: Actually it may be a false eye. We can replace this
     * condition with the requirement that it's suicide for the
     * opponent to play there and that all neighbors are our stones.
     *
     * FIXME: Add diagonal test here.
     */
    if (is_suicide(ii, other) && is_small_eye(ii)) {
      if (is_true_eye(heye, ii))
	num_eyes++;
      continue;
    }

    /* Check for own suicide. */
    if (is_suicide(ii, eye_color))
      continue;
    
    /* Check certain move restrictions. */
    if (eye_restrictions[ii] & DEFENDER_NOT_PLAY)
      continue;

    if ((eye_restrictions[ii] & DEFENDER_PLAY_IF_CAPTURE)
	&& !life_does_capture_something(ii, eye_color))
      continue;

    /* Score the move and save it in a list for later testing.
     * We give (preliminarily)
     * 1 point for each empty neighbor
     * 1 point for each opponent neighbor
     * 2 points if the intersection is marginal
     *
     * Assuming this is a legal move, we will get a score larger
     * than zero. The score can obviously not become larger than
     * six, but it seems unlikely we would ever exceed four.
     */
    move[num_moves] = k;
    score = 0;
    
    if (I(ii) > 0) {
      if (board[ii-NS] == other)
	score += 2;
      else if (board[ii-NS] == EMPTY)
	score++;
    }
    if (I(ii) < board_size-1) {
      if (board[ii+NS] == other)
	score += 2;
      else if (board[ii+NS] == EMPTY)
	score++;
    }
    if (J(ii) > 0) {
      if (board[ii-1] == other)
	score += 2;
      else if (board[ii-1] == EMPTY)
	score++;
    }
    if (J(ii) < board_size-1) {
      if (board[ii+1] == other)
	score += 2;
      else if (board[ii+1] == EMPTY)
	score++;
    }
    
    if (eyedata[ii].marginal)
      score += 2;
    
    if (score == 0)
      score = 1;

    if (score > 6)
      score = 6;
    
    move_score[num_moves] = score;
    num_moves++;
  }

  /* Now try all listed moves and see what we get. We order the moves
   * by score.
   */
  for (score=6; score>0; score--) {
    for (k=0; k<num_moves; k++) {
      int is_ko = 0;
      int index = move[k];
      
      if (move_score[k] != score)
	continue;
      
      ii = eye[index];

      gg_assert(board[ii] == EMPTY);
      
      /* Try the move and see if we can keep the eyes. */
      save_stackp = stackp;
      if (stackp - stackp_when_called < DEBUG_LIMIT)
	DEBUG(DEBUG_LIFE, "maximize_eyes: trymove %s %1m score %d\n",
	      color_to_string(eye_color), ii, score);
      if (trymove(ii, eye_color, "maximize_eyes", NO_MOVE, EMPTY, NO_MOVE)
	  || (ko_master == eye_color
	      && (ko_in < 3)
	      && (is_ko = 1)   /* Intentional assignment. */
	      && tryko(ii, eye_color, "maximize_eyes", EMPTY, NO_MOVE))) {
	
	/* Ok, we made our move.  Now let the opponent do his, and see
	 * how many eyes we can get. 
	 */
	result = minimize_eyes(eyedata, heye, &min1, &ko1,
			       ko_in + is_ko, ko_master,
			       &defense_point1, localmax, localko);
	if (result == 1) {
	  /* Is this result an improvement of what we already have?
	   * Seki without ko is better than two eyes with ko.
	   * Otherwise we primarily try to minimize the eye value.
	   */
	  if (compare_max_eyes(localmax, localko, min1, ko1 + is_ko) == 1) {
	    localmax = min1;
	    localko = ko1 + is_ko;
	    local_defense = index;
	  }

	  /* If we have two certain eyes and are not collecting all
           * defense moves, we break out of the loop immediately. We
           * set score to -1 to shortcut the outer loop as well.
	   */
	  if (localmax == 3 && localko == 0) {
	    popgo();
	    score = -1;
	    break;
	  }

	  /* Check if we have reached to cutoff value. In that case we can
	   * also return here since the minimizer won't be interested in a
	   * higher value anyway. We use compare_min_eyes to evaluate the
	   * value from the minimizer's point of view.
	   */
#if 1
	  if (compare_min_eyes(localmax, localko,
			       cutoff_eyes, cutoff_ko) == 1) {
	    popgo();
	    score = -1;
	    break;
	  }
#endif
	}
	
	popgo();
      }
      else {
	/* Illegal ko capture. */
	if (stackp - stackp_when_called < DEBUG_LIMIT)
	  DEBUG(DEBUG_LIFE,
		"  illegal ko capture: %s %1m ko master %s ko level %d\n",
		color_to_string(eye_color), ii, color_to_string(ko_master),
		ko_in);
      }
      gg_assert(stackp == save_stackp);
    }
  }
  
  /* If there were nowhere to play, get number of eyes */
  if (localmax == -1) {
    if (num_eyes >= 2)
      localmax = 3;
    else
      localmax = num_eyes;
    localko = 0;
    local_defense = MAX_EYE_SIZE;
  }

  *max = localmax;
  *ko_out = localko;
  
  if (defense_point)
    *defense_point = local_defense;

  SET_DEFENSE(cache_entry, localko, localmax, local_defense);
  
  if (stackp - stackp_when_called < DEBUG_LIMIT)
    DEBUG(DEBUG_LIFE,
	  "exiting maximize_eyes (1) - result = %d, ko = %d, move %1m (%H)\n", 
	  localmax, localko, eye[local_defense], hashdata.hashval);

  return 1;
}


/*
 * Show the board in a form suitable for debugging life code.
 * This is also callable from GDB.
 */

static void
life_showboard()
{
  int i, j;

  start_draw_board();
  for (i=0; i<board_size; i++) {
    for (j=0; j<board_size; j++) {
      int c;
      int ii = POS(i, j);
      int color = board[ii];

      if (color == WHITE) {
	if (move_in_stack(ii, stackp_when_called))
	  c = 'o';
	else
	  c = 'O';
      }
      else if (color == BLACK) {
	if (move_in_stack(ii, stackp_when_called))
	  c = 'x';
	else
	  c = 'X';
      }
      else
	c = EMPTY;
      draw_char(I(ii), J(ii), c);
    }
  }
  end_draw_board();
}


/* Clear statistics. */
void
reset_life_node_counter()
{
  life_node_counter = 0;
}


/* Retrieve statistics. */
int
get_life_node_counter()
{
  return life_node_counter;
}

/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
