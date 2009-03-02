/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * This is GNU Go, a Go program. Contact gnugo@gnu.org, or see       *
 * http://www.gnu.org/software/gnugo/ for more information.          *
 *                                                                   *
 * Copyright 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007,   *
 * 2008 and 2009 by the Free Software Foundation.                    *
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

/*
 * sgffile.c
 *
 * This file used to contain functions that create an SGF file on the fly.
 *
 * Today it contains supporting code around the more general SGF library
 * found in the sgf/ directory.
 */

#include "gnugo.h"

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

#include "liberty.h"
#include "sgftree.h"
#include "gg_utils.h"

/*
 * Add debug information to a node if user requested it from command
 * line.
 */

void
sgffile_add_debuginfo(SGFNode *node, float value)
{
  int pos;
  char comment[24];

  if (!outfilename[0])
    return;
  
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (!ON_BOARD(pos))
      continue;
    
    if (IS_STONE(board[pos]) && (output_flags & OUTPUT_MARKDRAGONS)) {
      if (dragon[pos].crude_status == DEAD)
	sgfLabel(node, "X", I(pos), J(pos));
      else if (dragon[pos].crude_status == CRITICAL)
	sgfLabel(node, "!", I(pos), J(pos));
    }
	
    if (potential_moves[pos] > 0.0 && (output_flags & OUTPUT_MOVEVALUES)) {
      if (potential_moves[pos] < 1.0)
	sgfLabel(node, "<1", I(pos), J(pos));
      else
	sgfLabelInt(node, (int) potential_moves[pos], I(pos), J(pos));
    }
  }
  
  if (value > 0.0 && (output_flags & OUTPUT_MOVEVALUES)) {
    sprintf(comment, "Value of move: %.2f", value);
    sgfAddComment(node, comment);
  }
}


/*
 * Write sgf tree to output file specified with -o option.
 * This can safely be done multiple times.
 */

void
sgffile_output(SGFTree *tree)
{
  if (outfilename[0])
    writesgf(tree->root, outfilename);
}


/* ================================================================ 
 * Dumping of information about a position into an sgftree.
 * Used by sgffile_decideposition, etc.
 * ================================================================ */


/*
 * sgffile_begindump begins storing all moves considered by
 * trymove and tryko in an sgf tree in memory.
 *
 * The caller only has to provide an own SGFTree pointer if he wants
 * to do something more with the tree than writing it to file as done
 * by sgffile_enddump().
 */

void
sgffile_begindump(SGFTree *tree)
{
  static SGFTree local_tree;
  gg_assert(sgf_dumptree == NULL);

  if (tree == NULL)
    sgf_dumptree = &local_tree;
  else 
    sgf_dumptree = tree;
  
  sgftree_clear(sgf_dumptree);
  sgftreeCreateHeaderNode(sgf_dumptree, board_size, komi, handicap);
  sgffile_printboard(sgf_dumptree);
}


/*
 * sgffile_enddump ends the dump and writes the sgf tree to file.
 */

void 
sgffile_enddump(const char *filename)
{
  /* Check if we have a valid filename and a tree. */
  if (filename && *filename && sgf_dumptree) {
    if (writesgf(sgf_dumptree->root, filename)) {
      /* Only delete the tree if writesgf() succeeds. If it doesn't, one
       * will most likely wish to save into another (writable) file.
       */
      sgfFreeNode(sgf_dumptree->root);
      sgf_dumptree = NULL;
    }
  }
}


/*
 * sgffile_printsgf creates an sgf of the current board position
 * (without any move history). It also adds information about who is
 * to play and marks illegal moves with the private sgf property IL.
 */

void
sgffile_printsgf(int color_to_play, const char *filename)
{
  SGFTree sgftree;
  int m, n;
  char pos[3];
  char str[128];
  float relative_komi;

  relative_komi = komi + black_captured - white_captured;
  
  sgftree_clear(&sgftree);
  sgftreeCreateHeaderNode(&sgftree, board_size, relative_komi, handicap);
  sgf_write_header(sgftree.root, 1, get_random_seed(), relative_komi,
		   handicap, get_level(), chinese_rules);
  gg_snprintf(str, 128, "GNU Go %s load and print", gg_version());
  sgfOverwriteProperty(sgftree.root, "GN", str);
  
  sgffile_printboard(&sgftree);
  
  if (color_to_play != EMPTY) {
    sgfAddProperty(sgftree.lastnode, "PL",
		   (color_to_play == WHITE ? "W" : "B"));

    for (m = 0; m < board_size; ++m)
      for (n = 0; n < board_size; ++n)
        if (BOARD(m, n) == EMPTY && !is_legal(POS(m, n), color_to_play)) {
	  gg_snprintf(pos, 3, "%c%c", 'a' + n, 'a' + m);
	  sgfAddProperty(sgftree.lastnode, "IL", pos);
	}
  }
  
  writesgf(sgftree.root, filename);
}


/*
 * sgffile_printboard adds the current board position to the tree.
 */

void
sgffile_printboard(SGFTree *tree)
{
  int i, j;
  SGFNode *node;
  
  gg_assert(tree);
  node = tree->lastnode;
  
  /* Write the white stones to the file. */
  for (i = 0; i < board_size; i++) {
    for (j = 0; j < board_size; j++) {
      if (BOARD(i, j) == WHITE)
	sgfAddStone(node, WHITE, i, j);
    }
  }

  /* Write the black stones to the file. */
  for (i = 0; i < board_size; i++) {
    for (j = 0; j < board_size; j++) {
      if (BOARD(i, j) == BLACK)
	sgfAddStone(node, BLACK, i, j);
    }
  }

  sgftreeSetLastNode(tree, node);
}


void
sgffile_recordboard(SGFNode *node)
{
  int i, j;

  if (node)
    for (i = 0; i < board_size; i++)
      for (j = 0; j < board_size; j++)
        if (BOARD(i, j) == BLACK)
          sgfAddStone(node, BLACK, i, j);
}


int
get_sgfmove(SGFProperty *property)
{
  return POS(get_moveX(property, board_size), get_moveY(property, board_size));
}


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
