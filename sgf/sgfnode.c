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

/*  Parts of this code were given to us by Tommy Thorn */

#include <config.h>

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <assert.h>


#include "sgftree.h"
#include "gg_utils.h"

#define STRICT_SGF 's'
#define LAX_SGF    'l'

/* Set this to 1 if you want warnings for missing GM and FF properties. */
#define VERBOSE_WARNINGS 0

/* ================================================================ */
/*                     Some utility functions.                      */
/* ================================================================ */

/* General FIXME:  Move these somewhere? */

/*
 * Utility: a checking, initializing malloc
 */

void *
xalloc(unsigned int size)
{
  void *pt = malloc(size);

  if (!pt) {
    fprintf(stderr, "xalloc: Out of memory!\n");
    exit(2);
  }

  memset(pt, 0, (unsigned long) size);
  return pt;
}

void *
xrealloc(void *pt, unsigned int size)
{
  void *ptnew = realloc(pt, size);

  if (!ptnew) {
    fprintf(stderr, "xrealloc: Out of memory!\n");
    exit(2);
  }
  return ptnew;
}


/* ================================================================ */
/*                           SGF Nodes                              */
/* ================================================================ */


/*
 * Allocate memory for a new SGF node.
 */

SGFNode *
sgfNewNode()
{
  SGFNode *newnode;
  newnode = xalloc(sizeof(SGFNode));
  newnode->next = NULL;
  newnode->props = NULL;
  newnode->parent = NULL;
  newnode->child = NULL;
  return newnode;
}

/*
 * Recursively free an sgf node
 */

void
sgfFreeNode(SGFNode *node)
{
  if (node == NULL)
    return;
  sgfFreeNode(node->next);
  sgfFreeNode(node->child);
  sgfFreeProperty(node->props);
  free(node);
}


/*
 * Add a generic text property to an SGF node.
 */

void
sgfAddProperty(SGFNode *node, const char *name, const char *value)
{
  SGFProperty *prop = node->props;

  if (prop)
    while (prop->next)
      prop = prop->next;

  sgfMkProperty(name, value, node, prop);
}


/*
 * Add an integer property to an SGF node.
 */

void
sgfAddPropertyInt(SGFNode *node, const char *name, long val)
{
  char buffer[10];

  gg_snprintf(buffer, 10, "%ld", val);
  sgfAddProperty(node, name, buffer);
}


/*
 * Add a float property to an SGF node.
 */

void
sgfAddPropertyFloat(SGFNode *node, const char *name, float val)
{
  char buffer[10];

  gg_snprintf(buffer, 10, "%3.1f", val);
  sgfAddProperty(node, name, buffer);
}


/*
 * Read a property as int from an SGF node.
 */

int
sgfGetIntProperty(SGFNode *node, const char *name, int *value)
{
  SGFProperty *prop;
  short nam = name[0] | name[1] << 8;

  for (prop = node->props; prop; prop = prop->next)
    if (prop->name == nam) {
      *value = atoi(prop->value);
      return 1;
    }

  return 0;
}


/*
 * Read a property as float from an SGF node.
 */

int
sgfGetFloatProperty(SGFNode *node, const char *name, float *value)
{
  SGFProperty *prop;
  short nam = name[0] | name[1] << 8;

  for (prop = node->props; prop; prop = prop->next)
    if (prop->name == nam) {
      *value = (float) atof(prop->value);
      /* MS-C warns of loss of data (double to float) */
      return 1;
    }

  return 0;
}


/*
 * Read a property as text from an SGF node.
 */

int
sgfGetCharProperty(SGFNode *node, const char *name, char **value)
{
  SGFProperty *prop;
  short nam = name[0] | name[1] << 8;

  for (prop = node->props; prop; prop = prop->next)
    if (prop->name == nam) {
      *value = prop->value;
      return 1;
    }

  return 0;
}


/*
 * Is there a property of this type in the node?
 */

static int
sgfHasProperty(SGFNode *node, const char *name)
{
  SGFProperty *prop;
  short nam = name[0] | name[1] << 8;

  for (prop = node->props; prop; prop = prop->next)
    if (prop->name == nam)
      return 1;

  return 0;
}


/*
 * Overwrite a property from an SGF node with text or create a new
 * one if it does not exist.
 */

void
sgfOverwriteProperty(SGFNode *node, const char *name, const char *text)
{
  SGFProperty *prop;
  short nam = name[0] | name[1] << 8;

  for (prop = node->props; prop; prop = prop->next)
    if (prop->name == nam) {
      prop->value = xrealloc(prop->value, strlen(text)+1);
      strcpy(prop->value, text);
      return;
    }

  sgfAddProperty(node, name, text);
}


/*
 * Overwrite an int property in an SGF node with val or create a new 
 * one if it does not exist.
 */

void
sgfOverwritePropertyInt(SGFNode *node, const char *name, int val)
{
  SGFProperty *prop;
  short nam = name[0] | name[1] << 8;

  for (prop = node->props; prop; prop = prop->next)
    if (prop->name == nam) {
      prop->value = xrealloc(prop->value, 12);
      gg_snprintf(prop->value, 12, "%d", val);
      return;
   }

  sgfAddPropertyInt(node, name, val);
}


/* 
 * Overwrite a float property in the gametree with val or create
 * a new one if it does not exist.
 */

void
sgfOverwritePropertyFloat(SGFNode *node, const char *name, float val)
{
  SGFProperty *prop;
  short nam = name[0] | name[1] << 8;

  for (prop = node->props; prop; prop = prop->next)
    if (prop->name == nam) {
      prop->value = xrealloc(prop->value, 15);
      gg_snprintf(prop->value, 15, "%3.1f", val);
      return;
    }

  sgfAddPropertyFloat(node, name, val);
}


/*
 * Goto previous node.
 */

SGFNode *
sgfPrev(SGFNode *node)
{
  SGFNode *q;
  SGFNode *prev;

  if (!node->parent)
    return NULL;

  q = node->parent->child;
  prev = NULL;
  while (q && q != node) {
    prev = q;
    q = q->next;
  }

  return prev;
}


/*
 * Goto root node.
 */

SGFNode *
sgfRoot(SGFNode *node)
{
  while (node->parent)
    node = node->parent;

  return node;
}


/* ================================================================ */
/*                         SGF Properties                           */
/* ================================================================ */


/*
 * Make an SGF property.
 *
 * FIXME: To speed up, we should have MkMoveProperty.
 *      Comment: We *never* have any speed problems with these functions. /iw
 */

SGFProperty *
sgfMkProperty(const char *name, const  char *value,
	      SGFNode *node, SGFProperty *last)
{
  SGFProperty *prop;

  prop = (SGFProperty *) xalloc(sizeof(SGFProperty));
  prop->value = xalloc(strlen(value)+1);
  prop->next = NULL;
  if (strlen(name) == 1) 
    prop->name = name[0] | (short) (' ' << 8);
  else
    prop->name = name[0] | name[1] << 8;
  strcpy(prop->value, value);

  if (last == NULL)
    node->props = prop;
  else
    last->next = prop;

  return prop;
}

/*
 * Recursively free an SGF property.
 *
 */

void
sgfFreeProperty(SGFProperty *prop)
{
  if (prop == NULL)
    return;
  sgfFreeProperty(prop->next);
  free(prop->value);
  free(prop);
}


/* ================================================================ */
/*                        High level functions                      */
/* ================================================================ */


/*
 * Add a stone to the current or the given node.
 * Return the node where the stone was added.
 */

SGFNode *
sgfAddStone(SGFNode *node, int color, int movex, int movey)
{
  char move[3];

  sprintf(move, "%c%c", movey + 'a', movex + 'a');
  sgfAddProperty(node, (color == BLACK) ? "AB" : "AW", move);

  return node;
}


/*
 * Add a move to the gametree.
 */

SGFNode *
sgfAddPlay(SGFNode *node, int who, int movex, int movey)
{
  char move[3];
  SGFNode *new;
  
  /* a pass move? */
  if (movex == -1 && movey == -1)
    move[0] = 0;
  else
    sprintf(move, "%c%c", movey + 'a', movex + 'a');

  if (node->child)
    new = sgfStartVariantFirst(node->child);
  else {
    new = sgfNewNode();
    node->child = new;
    new->parent = node;
  }
  
  sgfAddProperty(new, (who == BLACK) ? "B" : "W", move);

  return new;
}


/*
 * Add a move to the gametree. New variations are added after the old
 * ones rather than before.
 */

SGFNode *
sgfAddPlayLast(SGFNode *node, int who, int movex, int movey)
{
  char move[3];
  SGFNode *new;

  /* a pass move? */
  if (movex == -1 && movey == -1)
    move[0] = 0;
  else
    sprintf(move, "%c%c", movey + 'a', movex + 'a');

  new = sgfAddChild(node);
  sgfAddProperty(new, (who == BLACK) ? "B" : "W", move);

  return new;
}


SGFNode *
sgfCreateHeaderNode(int boardsize, float komi)
{
    SGFNode *root = sgfNewNode();

    sgfAddPropertyInt(root, "SZ", boardsize);
    sgfAddPropertyFloat(root, "KM", komi);

    return root;
}


/*
 * Add a comment to an SGF node.
 */

SGFNode *
sgfAddComment(SGFNode *node, const char *comment)
{
  sgfAddProperty(node, "C ", comment);

  return node;
}


/*
 * Place text on the board at position (i, j).
 */

SGFNode *
sgfBoardText(SGFNode *node, int i, int j, const char *text)
{
  void *str = xalloc(strlen(text) + 3);

  sprintf(str, "%c%c:%s", j+'a', i+'a', text);
  sgfAddProperty(node, "LB", str);
  free(str);

  return node;
}


/*
 * Place a character on the board at position (i, j).
 */

SGFNode *
sgfBoardChar(SGFNode *node, int i, int j, char c)
{
  char text[2] = "";

  text[0] = c;
  text[1] = 0;

  return sgfBoardText(node, i, j, text);
}


/*
 * Place a number on the board at position (i, j).
 */

SGFNode *
sgfBoardNumber(SGFNode *node, int i, int j, int number)
{
  char text[10];

  gg_snprintf(text, 10, "%c%c:%i", j+'a', i+'a', number);
  sgfAddProperty(node, "LB", text);

  return node;
}


/*
 * Place a triangle mark on the board at position (i, j).
 */

SGFNode *
sgfTriangle(SGFNode *node, int i, int j)
{
  char text[3];

  gg_snprintf(text, 3, "%c%c", j+'a', i+'a');
  sgfAddProperty(node, "TR", text);

  return node;
}


/*
 * Place a label on the board at position (i, j).
 */

SGFNode *
sgfLabel(SGFNode *node, const char *label, int i, int j)
{
  /* allows 12 chars labels - more than enough */
  char text[16];

  gg_snprintf(text, 16, "%c%c:%s", j+'a', i+'a', label);
  sgfAddProperty(node, "LB", text);

  return node;
}


/*
 * Place a numeric label on the board at position (i, j).
 */

SGFNode *
sgfLabelInt(SGFNode *node, int num, int i, int j)
{
  char text[16];

  gg_snprintf(text, 16, "%c%c:%d", j+'a', i+'a', num);
  sgfAddProperty(node, "LB", text);

  return node;
}


/*
 * Place a circle mark on the board at position (i, j).
 */

SGFNode *
sgfCircle(SGFNode *node, int i, int j)
{
  char text[3];

  gg_snprintf(text, 3, "%c%c", j+'a', i+'a');
  sgfAddProperty(node, "CR", text);

  return node;
}


/*
 * Place a square mark on the board at position (i, j).
 */

SGFNode *
sgfSquare(SGFNode *node, int i, int j)
{
  return sgfMark(node, i, j);   /* cgoban 1.9.5 does not understand SQ */
}


/*
 * Place a (square) mark on the board at position (i, j).
 */

SGFNode *
sgfMark(SGFNode *node, int i, int j)
{
  char text[3];

  gg_snprintf(text, 3, "%c%c", j+'a', i+'a');
  sgfAddProperty(node, "MA", text);

  return node;
}


/*
 * Start a new variant. Returns a pointer to the new node.
 */

SGFNode *
sgfStartVariant(SGFNode *node)
{
  assert(node);

  while (node->next)
    node = node->next;
  node->next = sgfNewNode();
  node->next->parent = node;

  return node->next;
}


/*
 * Start a new variant as first child. Returns a pointer to the new node.
 */

SGFNode *
sgfStartVariantFirst(SGFNode *node)
{
  SGFNode *old_first_child = node;
  SGFNode *new_first_child = sgfNewNode();

  new_first_child->next = old_first_child;
  if (old_first_child->parent)
    new_first_child->parent = old_first_child->parent;
  old_first_child->parent = new_first_child;
  if (new_first_child->parent)
    new_first_child->parent->child = new_first_child;

  return new_first_child;
}


/*
 * If no child exists, add one. Otherwise add a sibling to the
 * existing children. Returns a pointer to the new node.
 */

SGFNode *
sgfAddChild(SGFNode *node)
{
  SGFNode *new_node = sgfNewNode();
  assert(node);

  new_node->parent = node;
  
  if (!node->child)
    node->child = new_node;
  else {
    node = node->child;
    while (node->next)
      node = node->next;
    node->next = new_node;
  }

  return new_node;
}


/*
 * Write result of the game to the game tree.
 */

void
sgfWriteResult(SGFNode *node, float score, int overwrite)
{
  char text[8];
  char winner;
  float s;
  int dummy;

  /* If not writing to the SGF file, skip everything and return now. */
  if (!node)
    return;

  /* If not overwriting and there already is a result property, return. */
  if (!overwrite)
    if (sgfGetIntProperty(node, "RE", &dummy))
      return;

  if (score > 0.0) {
    winner = 'W';
    s = score;
  }
  else if (score < 0.0) {
    winner = 'B';
    s = -score;
  }
  else {
    winner = '0';
    s = 0;
  }

  if (winner == '0')
    gg_snprintf(text, 8, "0");
  else if (score < 1000.0 && score > -1000.0)
    gg_snprintf(text, 8, "%c+%3.1f", winner, s);
  else
    gg_snprintf(text, 8, "%c+%c", winner, 'R');
  sgfOverwriteProperty(node, "RE", text);
}


void
sgf_write_header(SGFNode *root, int overwrite, int seed, float komi,
		 int level, int rules)
{
  char str[128];
  int dummy;

  gg_snprintf(str, 128, "GNU Go %s Random Seed %d level %d", 
	      VERSION, seed, level);
  if (overwrite || !sgfGetIntProperty(root, "GN", &dummy))
    sgfOverwriteProperty(root, "GN", str);
  if (overwrite || !sgfGetIntProperty(root, "AP", &dummy))
    sgfOverwriteProperty(root, "AP", "GNU Go "VERSION);
  if (overwrite || !sgfGetIntProperty(root, "RU", &dummy))
    sgfOverwriteProperty(root, "RU", rules ? "Chinese" : "Japanese");
  sgfOverwriteProperty(root, "FF", "4");
  sgfOverwritePropertyFloat(root, "KM", komi);
}


/* ================================================================ */
/*                          Read SGF tree                           */
/* ================================================================ */


#define MAX_FILE_BUFFER 200000 /* buffer for reading SGF file. */

/*
 * SGF grammar:
 *
 * Collection = GameTree { GameTree }
 * GameTree   = "(" Sequence { GameTree } ")"
 * Sequence   = Node { Node }
 * Node       = ";" { Property }
 * Property   = PropIdent PropValue { PropValue }
 * PropIdent  = UcLetter { UcLetter }
 * PropValue  = "[" CValueType "]"
 * CValueType = (ValueType | Compose)
 * ValueType  = (None | Number | Real | Double | Color | SimpleText |
 *               Text | Point  | Move | Stone)
 *
 * The above grammar has a number of simple properties which enables us
 * to write a simpler parser:
 *   1) There is never a need for backtracking
 *   2) The only recursion is on gametree.
 *   3) Tokens are only one character
 * 
 * We will use a global state to keep track of the remaining input
 * and a global char variable, `lookahead' to hold the next token.  
 * The function `nexttoken' skips whitespace and fills lookahead with 
 * the new token.
 */


static void parse_error(const char *msg, int arg);
static void nexttoken(void);
static void match(int expected);


static FILE *sgffile;


#define sgf_getch() (getc(sgffile))


static char *sgferr;
#ifdef TEST_SGFPARSER
static int sgferrarg;
#endif
static int sgferrpos;

static int lookahead;


/* ---------------------------------------------------------------- */
/*                       Parsing primitives                         */
/* ---------------------------------------------------------------- */


static void
parse_error(const char *msg, int arg)
{
  fprintf(stderr, msg, arg);
  fprintf(stderr, "\n");
  exit(EXIT_FAILURE);
}


static void
nexttoken()
{
  do
    lookahead = sgf_getch();
  while (isspace(lookahead));
}


static void
match(int expected)
{
  if (lookahead != expected)
    parse_error("expected: %c", expected);
  else
    nexttoken();
}

/* ---------------------------------------------------------------- */
/*                        The parser proper                         */
/* ---------------------------------------------------------------- */


static void
propident(char *buffer, int size)
{
  if (lookahead == EOF || !isupper(lookahead)) 
    parse_error("Expected an upper case letter.", 0);
  
  while (lookahead != EOF && isalpha(lookahead)) {
    if (isupper(lookahead) && size > 1) {
      *buffer++ = lookahead;
      size--;
    }
    nexttoken();
  }
  *buffer = '\0';
}


static void
propvalue(char *buffer, int size)
{
  char *p = buffer;

  match('[');
  while (lookahead != ']' && lookahead != EOF) {
    if (lookahead == '\\') {
      lookahead = sgf_getch();
      /* Follow the FF4 definition of backslash */
      if (lookahead == '\r') {
	lookahead = sgf_getch();
	if (lookahead == '\n') 
	  lookahead = sgf_getch();
      }
      else if (lookahead == '\n') {
	lookahead = sgf_getch();
	if (lookahead == '\r') 
	  lookahead = sgf_getch();
      }
    }
    if (size > 1) {
      *p++ = lookahead;
      size--;
    }
    lookahead = sgf_getch();
  }
  match(']');
  
  /* Remove trailing whitespace */
  --p;
  while (p > buffer && isspace((int) *p))
    --p;
  *++p = '\0';
}


static SGFProperty *
property(SGFNode *n, SGFProperty *last)
{
  char name[3];
  char buffer[4000];

  propident(name, sizeof(name));
  do {
    propvalue(buffer, sizeof(buffer));
    last = sgfMkProperty(name, buffer, n, last);
  } while (lookahead == '[');
  return last;
}


static void
node(SGFNode *n)
{
  SGFProperty *last = NULL;
  match(';');
  while (lookahead != EOF && isupper(lookahead))
    last = property(n, last);
}


static SGFNode *
sequence(SGFNode *n)
{
  node(n);
  while (lookahead == ';') {
    SGFNode *new = sgfNewNode();
    new->parent = n;
    n->child = new;
    n = new;
    node(n);
  }
  return n;
}


static void
gametree(SGFNode **p, SGFNode *parent, int mode) 
{
  if (mode == STRICT_SGF)
    match('(');
  else
    for (;;) {
      if (lookahead == EOF) {
	parse_error("Empty file?", 0);
	break;
      }
      if (lookahead == '(') {
	while (lookahead == '(')
	  nexttoken();
	if (lookahead == ';')
	  break;
      }
      nexttoken();
    }

  /* The head is parsed */
  {
    SGFNode *head = sgfNewNode();
    SGFNode *last;

    head->parent = parent;
    *p = head;

    last = sequence(head);
    p = &last->child;
    while (lookahead == '(') {
      gametree(p, last->parent, STRICT_SGF);
      p = &((*p)->next);
    }
    if (mode == STRICT_SGF)
      match(')');
  }
}


/*
 * Fuseki readers
 * Reads an SGF file for extract_fuseki in a compact way
 */

static void
gametreefuseki(SGFNode **p, SGFNode *parent, int mode, 
	       int moves_per_game, int i)
{
  if (mode == STRICT_SGF)
    match('(');
  else
    for (;;) {
      if (lookahead == EOF) {
	parse_error("Empty file?", 0);
	break;
      }
      if (lookahead == '(') {
	while (lookahead == '(')
	  nexttoken();
	if (lookahead == ';')
	  break;
      }
      nexttoken();
    }
  
  /* The head is parsed */
  {

    SGFNode *head = sgfNewNode();
    SGFNode *last;
    head->parent = parent;
    *p = head;
    
    last = sequence(head);
    p = &last->child;
    while (lookahead == '(') {
      if (last->props 
	  && (last->props->name == SGFB || last->props->name == SGFW))
	i++;
      /* break after number_of_moves moves in SGF file */
      if (i >= moves_per_game) { 
	last->child = NULL;
	last->next = NULL;
	break;
      }
      else {
	gametreefuseki(p, last->parent, mode, moves_per_game, i);
	p = &((*p)->next);
      }
    }
    if (mode == STRICT_SGF)
      match(')');
  }
}

SGFNode *
readsgffilefuseki(const char *filename, int moves_per_game)
{
  SGFNode *root;
  int tmpi = 0;

  if (strcmp(filename, "-") == 0)
    sgffile = stdin;
  else
    sgffile = fopen(filename, "r");

  if (!sgffile)
    return NULL;


  nexttoken();
  gametreefuseki(&root, NULL, LAX_SGF, moves_per_game, 0);

  fclose(sgffile);

  if (sgferr) {
    fprintf(stderr, "Parse error: %s at position %d\n", sgferr, sgferrpos);
    return NULL;
  }

  /* perform some simple checks on the file */
  if (!sgfGetIntProperty(root, "GM", &tmpi)) {
    if (VERBOSE_WARNINGS)
      fprintf(stderr, "Couldn't find the game type (GM) attribute!\n");
  }
  else if (tmpi != 1) {
    fprintf(stderr, "SGF file might be for game other than go: %d\n", tmpi);
    fprintf(stderr, "Trying to load anyway.\n");
  }

  if (!sgfGetIntProperty(root, "FF", &tmpi)) {
    if (VERBOSE_WARNINGS)
      fprintf(stderr, "Can not determine SGF spec version (FF)!\n");
  }
  else if ((tmpi < 3 || tmpi > 4) && VERBOSE_WARNINGS)
    fprintf(stderr, "Unsupported SGF spec version: %d\n", tmpi);

  return root;
}





/*
 * Wrapper around readsgf which reads from a file rather than a string.
 * Returns NULL if file will not open, or some other parsing error.
 */

SGFNode *
readsgffile(const char *filename)
{
  SGFNode *root;
  int tmpi = 0;

  if (strcmp(filename, "-") == 0)
    sgffile = stdin;
  else
    sgffile = fopen(filename, "r");

  if (!sgffile)
    return NULL;


  nexttoken();
  gametree(&root, NULL, LAX_SGF);

  fclose(sgffile);

  if (sgferr) {
    fprintf(stderr, "Parse error: %s at position %d\n", sgferr, sgferrpos);
    return NULL;
  }

  /* perform some simple checks on the file */
  if (!sgfGetIntProperty(root, "GM", &tmpi)) {
    if (VERBOSE_WARNINGS)
      fprintf(stderr, "Couldn't find the game type (GM) attribute!\n");
  }
  else if (tmpi != 1) {
    fprintf(stderr, "SGF file might be for game other than go: %d\n", tmpi);
    fprintf(stderr, "Trying to load anyway.\n");
  }

  if (!sgfGetIntProperty(root, "FF", &tmpi)) {
    if (VERBOSE_WARNINGS)
      fprintf(stderr, "Can not determine SGF spec version (FF)!\n");
  }
  else if ((tmpi < 3 || tmpi > 4) && VERBOSE_WARNINGS)
    fprintf(stderr, "Unsupported SGF spec version: %d\n", tmpi);

  return root;
}



/* ================================================================ */
/*                          Write SGF tree                          */
/* ================================================================ */


#define OPTION_STRICT_FF4 0

static int sgf_column = 0;

static void
sgf_putc(int c, FILE *file)
{
  if (c == '\n' && sgf_column == 0)
    return;

  fputc(c, file);

  if (c == '\n')
    sgf_column = 0;
  else
    sgf_column++;

  if (c == ']' && sgf_column > 60) {
    fputc('\n', file);
    sgf_column = 0;
  }
}

static void
sgf_puts(const char *s, FILE *file)
{
  for (; *s; s++) {
    if (*s == '[' || *s == ']' || *s == '\\') {
      fputc('\\', file);
      sgf_column++;
    }
    fputc((int) *s, file);
    sgf_column++;
  }
}

/* Print all properties with the given name in a node to file and mark
 * them as printed.
 *
 * If is_comment is 1, multiple properties are concatenated with a
 * newline. I.e. we write
 *
 * C[comment1
 * comment2]
 *
 * instead of
 *
 * C[comment1][comment2]
 *
 * Most other property types should be written in the latter style.
 */

static void
sgf_print_name(FILE *file, short name)
{
  sgf_putc(name & 0xff, file);
  if (name >> 8 != ' ')
    sgf_putc(name >> 8, file);
}

static void
sgf_print_property(FILE *file, SGFNode *node, short name, int is_comment)
{
  int n = 0;
  SGFProperty *prop;

  for (prop = node->props; prop; prop = prop->next) {
    if (prop->name == name) {
      prop->name |= 0x20;  /* Indicate already printed. */
      if (n == 0) {
	sgf_print_name(file, name);
	sgf_putc('[', file);
      }
      else if (is_comment)
	sgf_putc('\n', file);
      else {
	sgf_putc(']', file);
	sgf_putc('[', file);
      }
      
      sgf_puts(prop->value, file);
      n++;
    }
  }

  if (n > 0)
    sgf_putc(']', file);

  /* Add a newline after certain properties. */
  if (name == SGFAB || name == SGFAW || name == SGFAE || (is_comment && n > 1))
    sgf_putc('\n', file);
}

/*
 * Print all remaining unprinted property values at node N to file.
 */

static void
sgfPrintRemainingProperties(FILE *file, SGFNode *node)
{
  SGFProperty *prop;

  for (prop = node->props; prop; prop = prop->next)
    if (!(prop->name & 0x20))
      sgf_print_property(file, node, prop->name, 0);
}


/*
 * Print the property values of NAME at node N and mark it as printed. 
 */

static void
sgfPrintCharProperty(FILE *file, SGFNode *node, const char *name)
{
  short nam = name[0] | name[1] << 8;
  
  sgf_print_property(file, node, nam, 0);
}


/*
 * Print comments from Node node.
 *
 * NOTE: cgoban does not print "C[comment1][comment2]" and I don't know
 *       what the sgfspec says.
 */

static void
sgfPrintCommentProperty(FILE *file, SGFNode *node, const char *name)
{
  short nam = name[0] | name[1] << 8;
  
  sgf_print_property(file, node, nam, 1);
}


static void
unparse_node(FILE *file, SGFNode *node)
{
  sgf_putc(';', file);
  sgfPrintCharProperty(file, node, "B ");
  sgfPrintCharProperty(file, node, "W ");
  sgfPrintCommentProperty(file, node, "N ");
  sgfPrintCommentProperty(file, node, "C ");
  sgfPrintRemainingProperties(file, node);
}


static void
unparse_root(FILE *file, SGFNode *node)
{
  sgf_putc(';', file);
  
  if (sgfHasProperty(node, "GM"))
    sgfPrintCharProperty(file, node, "GM");
  else {
    fputs("GM[1]", file);
    sgf_column += 5;
  }
  
  sgfPrintCharProperty(file, node, "FF");
  sgf_putc('\n', file);

  sgfPrintCharProperty(file, node, "SZ");
  sgf_putc('\n', file);
  
  sgfPrintCharProperty(file, node, "GN");
  sgf_putc('\n', file);
  
  sgfPrintCharProperty(file, node, "DT");
  sgf_putc('\n', file);
  
  sgfPrintCommentProperty(file, node, "PB");
  sgfPrintCommentProperty(file, node, "BR");
  sgf_putc('\n', file);
  
  sgfPrintCommentProperty(file, node, "PW");
  sgfPrintCommentProperty(file, node, "WR");
  sgf_putc('\n', file);
  
  sgfPrintCommentProperty(file, node, "N ");
  sgfPrintCommentProperty(file, node, "C ");
  sgfPrintRemainingProperties(file, node);

  sgf_putc('\n', file);
}


/*
 * p->child is the next move.
 * p->next  is the next variation
 */

static void
unparse_game(FILE *file, SGFNode *node, int root)
{
  if (!root)
    sgf_putc('\n', file);
  sgf_putc('(', file);
  if (root)
    unparse_root(file, node);
  else
    unparse_node(file, node);

  node = node->child;
  while (node != NULL && node->next == NULL) {
    unparse_node(file, node);
    node = node->child;
  } 

  while (node != NULL) {
    unparse_game(file, node, 0);
    node = node->next;
  }
  sgf_putc(')', file);
  if (root)
    sgf_putc('\n', file);
}

/* Printed properties are marked by adding the 0x20 bit to the
 * property name (changing an upper case letter to lower case). This
 * function removes this mark so that we can print the property next
 * time too. It recurses to all properties in the linked list.
 */
static void
restore_property(SGFProperty *prop)
{
  if (prop) {
    restore_property(prop->next);
    prop->name &= ~0x20;
  }
}

/* When called with the tree root, recurses to all properties in the
 * tree and removes all print marks.
 */
static void
restore_node(SGFNode *node)
{
  if (node) {
    restore_property(node->props);
    restore_node(node->child);
    restore_node(node->next);
  }
}


/*
 * Opens filename and writes the game stored in the sgf structure.
 */

int
writesgf(SGFNode *root, const char *filename)
{
  FILE *outfile;

  if (strcmp(filename, "-") == 0) 
    outfile = stdout;
  else
    outfile = fopen(filename, "w");

  if (!outfile) {
    fprintf(stderr, "Can not open %s\n", filename);
    return 0;
  }

  sgf_column = 0;
  unparse_game(outfile, root, 1);
  fclose(outfile);
  
  /* Remove "printed" marks so that the tree can be written multiple
   * times.
   */
  restore_node(root);
  
  return 1;
}


#ifdef TEST_SGFPARSER
int
main()
{
  static char buffer[25000];
  static char output[25000];
  SGFNode *game;

  sgffile = stdin;

  nexttoken();
  gametree(&game, LAX_SGF);
  if (sgferr) {
    fprintf(stderr, "Parse error:");
    fprintf(stderr, sgferr, sgferrarg);
    fprintf(stderr, " at position %d\n", sgferrpos);
  }
  else {
    unparse_game(stdin, game, 1);
    write(1, output, outputp - output);
  }
}
#endif



/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
