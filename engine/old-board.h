/* TEMPORARY: To enable compilation of code not converted to
 *	      multi-board yet.
 */

#include "board.h"

extern Goban *	    goban;


/* The board and the other parameters deciding the current position. */
extern int          board_size;             /* board size (usually 19) */
extern Intersection board[BOARDSIZE];       /* go board */
extern int          board_ko_pos;
extern int          black_captured;   /* num. of black stones captured */
extern int          white_captured;

extern Intersection initial_board[BOARDSIZE];
extern int          initial_board_ko_pos;
extern int          initial_white_captured;
extern int          initial_black_captured;
extern int          move_history_color[MAX_MOVE_HISTORY];
extern int          move_history_pos[MAX_MOVE_HISTORY];
extern int          move_history_pointer;

extern float        komi;
extern int          movenum;      /* movenumber - used for debug output */
		    
extern char         shadow[BOARDMAX];      /* reading tree shadow */

extern int chinese_rules;
extern int allow_suicide;

extern int stackp;                /* stack pointer */
extern int count_variations;      /* count (decidestring) */
extern SGFTree *sgf_dumptree;


/* This struct holds the internal board state. */
struct board_state {
  int board_size;

  Intersection board[BOARDSIZE];
  int board_ko_pos;
  int black_captured;
  int white_captured;

  Intersection initial_board[BOARDSIZE];
  int initial_board_ko_pos;
  int initial_white_captured;
  int initial_black_captured;
  int move_history_color[MAX_MOVE_HISTORY];
  int move_history_pos[MAX_MOVE_HISTORY];
  int move_history_pointer;

  float komi;
  int move_number;
};

/* This is increased by one anytime a move is (permanently) played or
 * the board is cleared.
 */
extern int position_number;
