/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * This is GNU GO, a Go program. Contact gnugo@gnu.org, or see       *
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
  tree->root = readsgffile(infilename);
  tree->lastnode = NULL;

  return tree->root != NULL;
}

/* ================================================================ */
/*                        High level functions                      */
/* ================================================================ */

/*
 * Returns the node to modify. When node==NULL then lastnode is used,
 * except if lastnode is NULL, then the current end of game is used.
 */

SGFNode *
sgftreeNodeCheck(SGFTree *tree, SGFNode *node)
{
   assert(tree->root);

   if (!node) {
     if (tree->lastnode)
       node = tree->lastnode;
     else {
       node = tree->root;
       while (node->child)
	 node = node->child;
     }
   }
   return node;
}


/*
 * Add a stone to the current or the given node.
 * Return the node where the stone was added.
 */

SGFNode *
sgftreeAddStone(SGFTree *tree, SGFNode *node, int color, int movex, int movey)
{
  node = sgftreeNodeCheck(tree, node);
  sgfAddStone(node, color, movex, movey);

  return node;
}


/*
 * Add a move to the gametree.
 */

SGFNode *
sgftreeAddPlay(SGFTree *tree, SGFNode *node, int color, int movex, int movey)
{
  node = sgftreeNodeCheck(tree, node);
  node = sgfAddPlay(node, color, movex, movey);
  tree->lastnode = node;

  return node;
}


/*
 * Add a move to the gametree. New variations are added after the old
 * ones rather than before.
 */

SGFNode *
sgftreeAddPlayLast(SGFTree *tree, SGFNode *node, int color,
		   int movex, int movey)
{
  node = sgftreeNodeCheck(tree, node);
  node = sgfAddPlayLast(node, color, movex, movey);
  tree->lastnode = node;

  return node;
}


SGFNode *
sgftreeCreateHeaderNode(SGFTree *tree, int boardsize, float komi)
{
  SGFNode *root = sgfNewNode();

  sgfAddPropertyInt(root, "SZ", boardsize);
  sgfAddPropertyFloat(root, "KM", komi);
  tree->root = root;

  return root;
}


/*
 * Add a comment to a gametree.
 */

SGFNode *
sgftreeAddComment(SGFTree *tree, SGFNode *node, const char *comment)
{
  assert(tree && tree->root);

  node = sgftreeNodeCheck(tree, node);
  sgfAddComment(node, comment);

  return node;
}


/*
 * Place text on the board at position (i, j).
 */

SGFNode *
sgftreeBoardText(SGFTree *tree, SGFNode *node, int i, int j, const char *text)
{
  assert(tree->root);

  node = sgftreeNodeCheck(tree, node);
  sgfBoardText(node, i, j, text);

  return node;
}


/*
 * Place a character on the board at position (i, j).
 */

SGFNode *
sgftreeBoardChar(SGFTree *tree, SGFNode *node, int i, int j, char c)
{
  assert(tree->root);

  node = sgftreeNodeCheck(tree, node);
  sgfBoardChar(node, i, j, c);

  return node;
}


/*
 * Place a number on the board at position (i, j).
 */

SGFNode * 
sgftreeBoardNumber(SGFTree *tree, SGFNode *node, int i, int j, int number)
{
  node = sgftreeNodeCheck(tree, node);
  sgfBoardNumber(node, i, j, number);

  return node;
}


/*
 * Place a circle mark on the board at position (i, j).
 */

SGFNode * 
sgftreeTriangle(SGFTree *tree, SGFNode *node, int i, int j)
{
  node = sgftreeNodeCheck(tree, node);
  sgfTriangle(node, i, j);

  return node;
}


/*
 * Place a circle mark on the board at position (i, j).
 */

SGFNode * 
sgftreeCircle(SGFTree *tree, SGFNode *node, int i, int j)
{
  node = sgftreeNodeCheck(tree, node);
  sgfCircle(node, i, j);

  return node;
}


/*
 * Place a square mark on the board at position (i, j).
 */

SGFNode * 
sgftreeSquare(SGFTree *tree, SGFNode *node, int i, int j)
{
  node = sgftreeNodeCheck(tree, node);
  sgfSquare(node, i, j);

  return node;
}


/*
 * Place a (square) mark on the board at position (i, j).
 */

SGFNode *
sgftreeMark(SGFTree *tree, SGFNode *node, int i, int j)
{
  node = sgftreeNodeCheck(tree, node);
  sgfMark(node, i, j);

  return node;
}


/*
 * Start a new variant. Returns a pointer to the new node.
 */

SGFNode *
sgftreeStartVariant(SGFTree *tree, SGFNode *node)
{
  node = sgftreeNodeCheck(tree, node);
  return sgfStartVariant(node);
}


/*
 * Start a new variant as first child. Returns a pointer to the new node.
 */

SGFNode *
sgftreeStartVariantFirst(SGFTree *tree, SGFNode *node)
{
  node = sgftreeNodeCheck(tree, node);
  return sgfStartVariantFirst(node);

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

