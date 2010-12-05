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

#ifndef _DFA_H_
#define _DFA_H_

#if MAX_BOARD > 11
#define DFA_MAX_BOARD		MAX_BOARD
#else
#define DFA_MAX_BOARD 		11
#endif
#define DFA_MAX_ORDER		((2 * DFA_MAX_BOARD - 1)	\
				 * (2 * DFA_MAX_BOARD - 1))
#define DFA_BASE		(3 * DFA_MAX_BOARD)
#define DFA_POS(i, j)		(((i) + DFA_MAX_BOARD) * DFA_BASE	\
				 + ((j) + DFA_MAX_BOARD))

#ifndef EMPTY
#define EMPTY     0		/* . */
#define WHITE     1		/* O */
#define BLACK     2		/* X */
#endif
#define OUT_BOARD 3		/* # */


/* Maximum pattern matched at one positions. */
#define DFA_MAX_MATCHED		(8 * 24)


/* DFA spiral order. */
extern int spiral[DFA_MAX_ORDER][8];

void build_spiral_order(void);


/* The run-time data structures declared here are different from those
 * used internally to build the DFA. */

/* Attribute list. */
typedef struct attrib_rt
{
  short val;
  short next;
} attrib_rt_t;

/* DFA state. */
typedef struct state_rt
{
  short next[4];
  short att;
} state_rt_t;

typedef struct dfa_rt
{
  /* File header. */
  const char name[80];

  /* Transition graph. */
  const state_rt_t *states;

  /* Attributes sets. */
  const attrib_rt_t *indexes;
} dfa_rt_t;



#endif /* _DFA_H_ */


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
