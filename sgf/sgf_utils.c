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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <ctype.h>

#include "sgftree.h"

/* 
 * Return the integer X move.
 */

int
get_moveX(SGFProperty *property, int boardsize)
{
  int i;
  if (strlen(property->value) < 2)
    return -1;

  i = toupper((int) property->value[1]) - 'A';
  if (i >= boardsize)
    return -1;

  return i;
}

/* 
 * Return the integer Y move.
 */

int
get_moveY(SGFProperty *property, int boardsize)
{
  int j;
  if (strlen(property->value) < 2)
    return -1;

  j = toupper((int) property->value[0]) - 'A';
  if (j >= boardsize)
    return -1;

  return j;
}


/* Fills (*i, *j) from the property value, in GNU Go co-ords.
 * Note that GNU Go uses different conventions from sgf for
 * co-ordinates been called. 
 *
 * Returns 1 for a move, 0 for a pass.
 */

int
get_moveXY(SGFProperty *property, int *i, int *j, int boardsize)
{
  *i = get_moveX(property, boardsize);
  *j = get_moveY(property, boardsize);
  
  if (*i == -1 && *j == -1)
    return 0;

  return 1;
}


/* 
 * Debugging function to print properties as they are traversed.
 */

int
show_sgf_properties(SGFNode *node)
{
  SGFProperty *sgf_prop;
  int propcount;

  propcount = 0;

  printf("P: ");
  if (!node->props) {
    printf("None\n");
    return propcount;
  }
  else {
    sgf_prop = node->props;
    while (sgf_prop) {
      printf("%c%c ", sgf_prop->name & 0x00FF, (sgf_prop->name & 0xFF00)>>8);
      sgf_prop = sgf_prop->next;
      propcount++;
    }

    printf("(%d) ", propcount);
    if (node->next)
      printf("n");
    if (node->child)
      printf("c");
    printf("\n");
  }

  return propcount;
}


/*
 * Recursively traverse each node showing all properties.
 */

int
show_sgf_tree(SGFNode *node)
{
  int n = 0; /* number of nodes */
  
  n++;
  show_sgf_properties(node);

  /* must search depth first- siblings are equal! */
  if (node->child)
    n += show_sgf_tree(node->child);

  if (node->next)
    n += show_sgf_tree(node->next);
  
  return n;
}


/*
 * Determine if a node has a mark property in it.
 */

int
is_markup_node(SGFNode *node)
{
  SGFProperty *sgf_prop;
  
  /* If the node has no properties, there's nothing to do.
     This should have been checked by the caller, but it can't hurt. */
  if (!node->props)
    return 0;

  sgf_prop = node->props;
  while (sgf_prop) {
    switch (sgf_prop->name) {
    case SGFCR: 
    case SGFSQ: /* Square */
    case SGFTR: /* Triangle */
    case SGFMA: /* Mark */
    case SGFBM: /* bad move */
    case SGFDO: /* doubtful move */
    case SGFIT: /* interesting move */
    case SGFTE: /* good move */
      return 1;
      break;
    default:
      break;
    }
    sgf_prop = sgf_prop->next;
  }

  /* No markup property found. */
  return 0;
}


/*
 * Determine if the node has a move in it.
 */

int
is_move_node(SGFNode *node)
{
  SGFProperty *sgf_prop;
  
  /* If the node has no properties, there's nothing to do.
     This should have been checked by the caller, but it can't hurt. */
  if (!node->props)
    return 0;

  sgf_prop = node->props;
  while (sgf_prop) {
    switch (sgf_prop->name) {
    case SGFB: 
    case SGFW: 
      return 1;
      break;
    default:
      break;
    }
    sgf_prop = sgf_prop->next;
  }

  return 0;
}


/*
 * Determine if the node has a pass move in it.
 */

int
is_pass_node(SGFNode *node, int boardsize)
{
  SGFProperty *sgf_prop;
  int i, j;
  
  /* If the node has no properties, there's nothing to do.
     This should have been checked by the caller, but it can't hurt. */
  if (!node->props)
    return 0;

  sgf_prop = node->props;
  while (sgf_prop) {
    switch (sgf_prop->name) {
    case SGFB: 
    case SGFW: 
      return !get_moveXY(sgf_prop, &i, &j, boardsize);
      break;
    default:
      break;
    }
    sgf_prop = sgf_prop->next;
  }

  return 0;
}


/*
 * Determine whose move is in the node.
 */

int
find_move(SGFNode *node)
{
  SGFProperty *sgf_prop;
  
  /* If the node has no properties, there's nothing to do.
     This should have been checked by the caller, but it can't hurt. */
  if (!node->props)
    return 0;

  sgf_prop = node->props;
  while (sgf_prop) {
    switch (sgf_prop->name) {
    case SGFB: 
      return BLACK;
      break;
    case SGFW: 
      return WHITE;
      break;
    default:
      break;
    }
    sgf_prop = sgf_prop->next;
  }

  return EMPTY;
}


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
