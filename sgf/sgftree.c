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

#include <assert.h>

#include "gg_utils.h"
#include "sgftree.h"

void
sgftree_clear(SGFTree *tree)
{
  tree->root = NULL;
  tree->lastnode = NULL;
}

int
sgftree_readfile(SGFTree *tree, const char *infilename)
{
  SGFNode *savetree = tree->root;

  tree->root = readsgffile(infilename);
  if (tree->root == NULL) {
    tree->root = savetree;
    return 0;
  }
  
  sgfFreeNode(savetree);
  tree->lastnode = NULL;
  return 1;
}


/* Go back one node in the tree. If lastnode is NULL, go to the last
 * node (the one in main variant which has no children).
 */

int
sgftreeBack(SGFTree *tree)
{
  if (tree->lastnode) {
    if (tree->lastnode->parent)
      tree->lastnode = tree->lastnode->parent;
    else
      return 0;
  }
  else
    while (sgftreeForward(tree));
  
  return 1;
}


/* Go forward one node in the tree. If lastnode is NULL, go to the
 * tree root.
 */

int
sgftreeForward(SGFTree *tree)
{
  if (tree->lastnode) {
    if (tree->lastnode->child)
      tree->lastnode = tree->lastnode->child;
    else
      return 0;
  }
  else
    tree->lastnode = tree->root;
  
  return 1;
}


/* ================================================================ */
/*                        High level functions                      */
/* ================================================================ */

/*
 * Returns the node to modify. Use lastnode if available, otherwise
 * follow the main variation to the current end of the game.
 */

SGFNode *
sgftreeNodeCheck(SGFTree *tree)
{
  SGFNode *node = NULL;
  assert(tree->root);

  if (tree->lastnode)
    node = tree->lastnode;
  else {
    node = tree->root;
    while (node->child)
      node = node->child;
  }

  return node;
}


/*
 * Add a stone to the current or the given node.
 * Return the node where the stone was added.
 */

void
sgftreeAddStone(SGFTree *tree, int color, int movex, int movey)
{
  SGFNode *node = sgftreeNodeCheck(tree);
  sgfAddStone(node, color, movex, movey);
}


/*
 * Add a move to the gametree.
 */

void
sgftreeAddPlay(SGFTree *tree, int color, int movex, int movey)
{
  SGFNode *node = sgftreeNodeCheck(tree);
  tree->lastnode = sgfAddPlay(node, color, movex, movey);
}


/*
 * Add a move to the gametree. New variations are added after the old
 * ones rather than before.
 */

void
sgftreeAddPlayLast(SGFTree *tree, int color, int movex, int movey)
{
  SGFNode *node = sgftreeNodeCheck(tree);
  tree->lastnode = sgfAddPlayLast(node, color, movex, movey);
}


void
sgftreeCreateHeaderNode(SGFTree *tree, int boardsize, float komi)
{
  SGFNode *root = sgfNewNode();

  sgfAddPropertyInt(root, "SZ", boardsize);
  sgfAddPropertyFloat(root, "KM", komi);
  tree->root = root;
  tree->lastnode = root;
}


/*
 * Add a comment to a gametree.
 */

void
sgftreeAddComment(SGFTree *tree, const char *comment)
{
  SGFNode *node;
  assert(tree && tree->root);

  node = sgftreeNodeCheck(tree);
  sgfAddComment(node, comment);
}


/*
 * Place text on the board at position (i, j).
 */

void
sgftreeBoardText(SGFTree *tree, int i, int j, const char *text)
{
  SGFNode *node;
  assert(tree->root);

  node = sgftreeNodeCheck(tree);
  sgfBoardText(node, i, j, text);
}


/*
 * Place a character on the board at position (i, j).
 */

void
sgftreeBoardChar(SGFTree *tree, int i, int j, char c)
{
  SGFNode *node;
  assert(tree->root);

  node = sgftreeNodeCheck(tree);
  sgfBoardChar(node, i, j, c);
}


/*
 * Place a number on the board at position (i, j).
 */

void
sgftreeBoardNumber(SGFTree *tree, int i, int j, int number)
{
  SGFNode *node = sgftreeNodeCheck(tree);
  sgfBoardNumber(node, i, j, number);
}


/*
 * Place a circle mark on the board at position (i, j).
 */

void
sgftreeTriangle(SGFTree *tree, int i, int j)
{
  SGFNode *node = sgftreeNodeCheck(tree);
  sgfTriangle(node, i, j);
}


/*
 * Place a circle mark on the board at position (i, j).
 */

void
sgftreeCircle(SGFTree *tree, int i, int j)
{
  SGFNode *node = sgftreeNodeCheck(tree);
  sgfCircle(node, i, j);
}


/*
 * Place a square mark on the board at position (i, j).
 */

void
sgftreeSquare(SGFTree *tree, int i, int j)
{
  SGFNode *node = sgftreeNodeCheck(tree);
  sgfSquare(node, i, j);
}


/*
 * Place a (square) mark on the board at position (i, j).
 */

void
sgftreeMark(SGFTree *tree, int i, int j)
{
  SGFNode *node = sgftreeNodeCheck(tree);
  sgfMark(node, i, j);
}


/*
 * Start a new variant.
 */

void
sgftreeStartVariant(SGFTree *tree)
{
  SGFNode *node = sgftreeNodeCheck(tree);
  tree->lastnode = sgfStartVariant(node);
}


/*
 * Start a new variant as first child.
 */

void
sgftreeStartVariantFirst(SGFTree *tree)
{
  SGFNode *node = sgftreeNodeCheck(tree);
  tree->lastnode = sgfStartVariantFirst(node);
}


/*
 * Write result of the game to the game tree.
 */

void
sgftreeWriteResult(SGFTree *tree, float score, int overwrite)
{
  assert(tree->root);

  sgfWriteResult(tree->root, score, overwrite);
}


void
sgftreeSetLastNode(SGFTree *tree, SGFNode *last_node)
{
  tree->lastnode = last_node;
}


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */

