/*
 * src/gmp.h
 * Copyright (C) 1995-1996 William Shubert.
 *
 * Parts of this file are taken from "protocol.c", which is covered under
 *   the copyright notice below.
 * Any code that is not present in the original "proocol.c" is covered under
 *   the copyright notice above.
 */
/*******************************************************
  protocol.c 1.00
  JCGA Go Communication Protocol
  copyright(c)	   Shuji Sunaga   95.7.9
  original         Standard Go Modem Protocol 1.0
                   by David Fotland
 * Permission granted to use this code for any
 * commercial or noncommercial purposes as long as this
 * copyright notice is not removed. 
 * This code was written for Borland C++ 4.0J
*******************************************************/
/*
 * You may use this code in any way you wish as long as you retain the
 *   above copyright notices.
 */

/* Modified by Paul Pogonyshev on 10.07.2003.
 * Added support for Simplified GTP.
 */

#ifndef  _GMP_H_
#define  _GMP_H_  1


/**********************************************************************
 * Data types
 **********************************************************************/
typedef enum  {
  gmp_nothing, gmp_move, gmp_pass, gmp_reset, gmp_newGame, gmp_undo, gmp_err
} GmpResult;


#ifndef  _GMP_C_
typedef struct Gmp_struct  Gmp;
#endif  /* _GMP_C_ */


/**********************************************************************
 * Fuctions
 **********************************************************************/
extern Gmp  *gmp_create(int inFile, int outFile);

extern void  gmp_destroy(Gmp *ge);

/*
 * This starts a game up.
 * If you want, you can pass in -1 for size, handicap, and chineseRules,
 *   and it will query.  You can also pass in -1 for iAmWhite, but if you do
 *   this then it will send a RESET command.  If the other machine is black
 *   and doesn't support arbitration, then this could screw things up.
 * Komi must be specified since GMP doesn't let you exchange komi information.
 * After calling this function, you should call gmp_check until you get a
 *   "gmp_newGame" returned.  Then you know that the size, etc. have all been
 *   verified, you can call "gmp_size()" or whatever to find out values you
 *   set to -1, and you can start the game.
 */
extern void  gmp_startGame(Gmp *ge, int boardsize, int handicap,
			   float komi, int chineseRules, int iAmWhite,
			   int simplified);

/*
 * Pretty self-explanatory set of routines.  For sendMove, (0,0) is the
 *   corner.
 */
extern void  gmp_sendMove(Gmp *ge, int x, int y);
extern void  gmp_sendPass(Gmp *ge);
extern void  gmp_sendUndo(Gmp *ge, int numUndos);

/*
 * gmp_check() process all data queued up until the next command that needs
 *   to be returned to the application.  If sleep is nonzero, then it will
 *   stay here until a command arrives.  If sleep is zero, then it will
 *   return immediately if no command is ready.
 * It should be called about once per second to prevent the connection
 *   between the programs from timing out.
 * If you get a move, "out1" will be the X and "out2" will be the y.
 */
extern GmpResult  gmp_check(Gmp *ge, int sleepy,
			    int *out1, int *out2, const char  **error);


/*
 * These routines return the configuration of the game that you are playing
 *   in.  They should all be set up by the time you get a gmpResult_newGame
 *   from gmp_read().
 * If you get a -1 back from these, it means that you didn't set the value
 *   when you called gmp_startGame() and your opponent wouldn't say what
 *   they had the parameter set to.
 */
extern int  gmp_size(Gmp *ge);
extern int  gmp_handicap(Gmp *ge);
extern float  gmp_komi(Gmp *ge);
extern int  gmp_chineseRules(Gmp *ge);
extern int  gmp_iAmWhite(Gmp *ge);

/*
 * This is handy if you want to print out, as an ascii string, the result
 *   that you got bock from gmp_read().
 */
extern const char  *gmp_resultString(GmpResult result);

#endif  /* _GMP_H_ */
