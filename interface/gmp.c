/*
 * src/gmp.h
 * Copyright (C) 1995-1997 William Shubert.
 *
 * You may use this code in any way you wish as long as you retain the
 *   above copyright notice.
 *
 * This is based on David Fotland's Go Modem Protocol Code and the
 *   "protocol.Z" info file from Bruce Wilcox.  It has been pretty much
 *   completely rewritten now, though.
 */

/* Modification by Daniel Bump for GNU Go: I made all GMP messages
   contingent on gmp_debug. Otherwise this is identical to Bill Shubert's
   version distributed with GoDummy.
*/

/* Modified by Paul Pogonyshev on 10.07.2003.
 * Added support for Simplified GTP.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#define  _GMP_C_  1

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# if HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  include <time.h>
# endif
#endif

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef __MINGW32__
#include <windows.h>
#include <winsock.h>
#include <io.h>
#endif

#ifdef HAVE_WINSOCK_IO_H
#include <winsock.h>
#include <io.h>
#endif

/**********************************************************************
 * Constants
 **********************************************************************/

#define  GMP_TIMEOUTRETRIES   60
#define  GMP_RETRYSECS	       1
#define  SGMP_TIMEOUTRETRIES   9
#define  SGMP_RETRYSECS       20
#define  GMP_MAXSENDSQUEUED   16


/**********************************************************************
 * Data types
 **********************************************************************/
 
typedef enum  {
  cmd_ack, cmd_deny, cmd_reset, cmd_query, cmd_respond, cmd_move,
  cmd_undo
} Command;


typedef enum  {
  query_game, query_bufSize, query_protocol, query_stones,
  query_bTime, query_wTime, query_charSet, query_rules, query_handicap,
  query_boardSize, query_timeLimit, query_color, query_who,
  query_max
} Query;


typedef struct Gmp_struct  {
  int  inFile, outFile;
  int  boardSize, sizeVerified;
  int  handicap, handicapVerified;
  float  komi;
  int komiVerified;
  int  chineseRules, rulesVerified;
  int  iAmWhite, colorVerified;
  Query  lastQuerySent;

  int  recvSoFar, sendsQueued;
  int  sendFailures, noResponseSecs;
  int  waitingHighAck;
  time_t  lastSendTime;
  int  myLastSeq, hisLastSeq;
  unsigned char  recvData[4];
  unsigned char  sendData[4];
  struct  {
    int  cmd, val;
  } sendsPending[GMP_MAXSENDSQUEUED];

  int  earlyMovePresent;
  int  earlyMoveX, earlyMoveY;

  int simplified;
} Gmp;


/**********************************************************************
 * Globals
 **********************************************************************/

int  gmp_debug = 0;
static const char  *commandNames[] = {
  "ACK", "DENY", "RESET", "QUERY", "RESPOND", "MOVE", "UNDO"};
static const char  *queryNames[] = {
  "GAME", "BUFFER SIZE", "PROTOCOL", "STONES",
  "BLACK TIME", "WHITE TIME", "CHAR SET", "RULES", "HANDICAP",
  "BOARD SIZE", "TIME LIMIT", "COLOR", "WHO"};


/**********************************************************************
 * Forward Declarations
 **********************************************************************/

/* Get the forward declaration of externally visible functions. */
#include "gmp.h"

static unsigned char  checksum(unsigned char p[4]);
static GmpResult  gotQueryResponse(Gmp *ge, int val, const char **err);
static void  putCommand(Gmp *ge, Command cmd, int val);
static GmpResult  respond(Gmp *ge, Query query);
static void  askQuery(Gmp *ge);
static int  heartbeat(Gmp *ge);
static GmpResult  getPacket(Gmp *ge, int *out1, int *out2,
			    const char **error);
static GmpResult  parsePacket(Gmp *ge, int *out1, int *out2,
			      const char **error);
static GmpResult  processCommand(Gmp *ge, Command command, int val,
				 int *out1, int *out2, const char **error);
static void  processQ(Gmp *ge);


/**********************************************************************
 * Functions
 **********************************************************************/

#define  gmp_verified(ge)  \
  ((ge)->sizeVerified && (ge)->colorVerified && \
   (ge)->handicapVerified && (ge)->rulesVerified)


Gmp  *gmp_create(int inFile, int outFile)  {
  Gmp  *ge;

  ge = malloc(sizeof(Gmp));
  ge->inFile = inFile;
  ge->outFile = outFile;

  ge->boardSize = -1;
  ge->sizeVerified = 0;

  ge->handicap = -1;
  ge->handicapVerified = 0;

  ge->komi = 0.0;

  ge->chineseRules = -1;
  ge->rulesVerified = 0;

  ge->iAmWhite = -1;
  ge->colorVerified = 0;

  ge->lastQuerySent = 0;
  
  ge->recvSoFar = 0;
  ge->sendsQueued = 0;
  ge->sendFailures = 0;
  ge->noResponseSecs = 0;
  ge->waitingHighAck = 0;
  ge->lastSendTime = 0;
  ge->myLastSeq = 0;
  ge->hisLastSeq = 0;

  ge->earlyMovePresent = 0;

  return(ge);
}


void  gmp_destroy(Gmp *ge)  {
  free(ge);
}


GmpResult  gmp_check(Gmp *ge, int gsleep, int *out1, int *out2,
		     const char **error)  {
  fd_set  readReady;
  struct timeval  noTime;
  int  intDummy;
  const char  *charPtrDummy;
  GmpResult  result;

  if (out1 == NULL)
    out1 = &intDummy;
  if (out2 == NULL)
    out2 = &intDummy;
  if (error == NULL)
    error = &charPtrDummy;
  if (gmp_verified(ge) && ge->earlyMovePresent) {
    *out1 = ge->earlyMoveX;
    *out2 = ge->earlyMoveY;
    ge->earlyMovePresent = 0;
    if (gmp_debug) {
      fprintf(stderr, "GMP: Returning early move.\n");
    }
    return(gmp_move);
  }
  *out1 = 0;
  *out2 = 0;
  *error = NULL;
  do  {
    if (time(NULL) != ge->lastSendTime)  {
      if (!heartbeat(ge))  {
	*error = "GMP Timeout";
	return(gmp_err);
      }
    }
    FD_ZERO(&readReady);
    FD_SET(ge->inFile, &readReady);
    noTime.tv_usec = 0;
    if (gsleep)
      noTime.tv_sec = 1;
    else
      noTime.tv_sec = 0;
    select(ge->inFile + 1, &readReady, NULL, NULL, &noTime);
    if (!gsleep && !FD_ISSET(ge->inFile, &readReady))
      return(gmp_nothing);
    result = getPacket(ge, out1, out2, error);
  } while (result == gmp_nothing);
  return(result);
}


static GmpResult  getPacket(Gmp *ge, int *out1, int *out2,
			    const char **error)  {
  unsigned char  charsIn[4], c;
  int  count = 0, cNum;
  static char  errOut[200];

  count = read(ge->inFile, charsIn, 4 - ge->recvSoFar);
  if (count <= 0)  {
    sprintf(errOut, "System error.");
    *error = errOut;
    return(gmp_err);
  }

  for (cNum = 0;  cNum < count;  ++cNum)  {
    c = charsIn[cNum];
    if (!ge->recvSoFar)  {
      /* idle, looking for start of packet */
      if ((c & 0xfc) == 0)  {  /* start of packet */
	ge->recvData[0] = c;
	ge->recvSoFar = 1;
      } else {
	if (gmp_debug) {
	  fprintf(stderr, "GMP: Received invalid packet.\n");
	}
      }
    } else  {
      /* We're in the packet. */
      if ((c & 0x80) == 0)  {  /* error */
	if (gmp_debug) {
	  fprintf(stderr, "GMP: Received invalid packet.\n");
	}
	ge->recvSoFar = 0;
	if ((c & 0xfc) == 0)  {
	  ge->recvData[ge->recvSoFar++] = c;
	}
      } else  {
	/* A valid character for in a packet. */
	ge->recvData[ge->recvSoFar++] = c;
	if (ge->recvSoFar == 4)  {  /* check for extra bytes */
	  assert(cNum + 1 == count);
	  ge->recvSoFar = 0;
	  if (checksum(ge->recvData) == ge->recvData[1])
	    return(parsePacket(ge, out1, out2, error));
	}
      }
    }
  }
  return(gmp_nothing);
}


static unsigned char  checksum(unsigned char p[4])  {
  unsigned char sum;
  sum = p[0] + p[2] + p[3];       
  sum |= 0x80;  /* set sign bit */
  return(sum);
}


static GmpResult  parsePacket(Gmp *ge, int *out1, int *out2,
			      const char **error)  {
  int  seq, ack, val;
  Command  command;
  GmpResult  result;

  seq = ge->recvData[0] & 1;
  ack = (ge->recvData[0] & 2) >> 1;
  if (ge->recvData[2] & 0x08)  {  /* Not-understood command. */
    if (gmp_debug) {
      fprintf(stderr, "GMP: Unknown command byte 0x%x received.\n",
	      (unsigned int) ge->recvData[2]);
    }
    return(gmp_nothing);
  }
  command = (ge->recvData[2] >> 4) & 7;
  val = ((ge->recvData[2] & 7) << 7) | (ge->recvData[3] & 0x7f);
  if (gmp_debug) {
    if (command == cmd_query) {
      if (val >= query_max) {
	if (gmp_debug)
	  fprintf(stderr, "GMP: Read in command: %s unkown value %d\n",
		  commandNames[command], val);
      } else {
	if (gmp_debug)
	  fprintf(stderr, "GMP: Read in command: %s %s\n",
		  commandNames[command], queryNames[val]);
      }
    } else {
      if (gmp_debug)
	fprintf(stderr, "GMP: Read in command: %s\n",
		commandNames[command]);
    }
  }
  if (!ge->waitingHighAck)  {
    if ((command == cmd_ack) ||  /* An ack.  We don't need an ack now. */
	(ack != ge->myLastSeq))  {  /* He missed my last message. */
      if (gmp_debug)
	fprintf(stderr, "GMP: Unexpected ACK.\n");
      return(gmp_nothing);
    } else if (seq == ge->hisLastSeq)  {  /* Seen this one before. */
      if (gmp_debug)
	fprintf(stderr, "GMP: Received repeated message.\n");
      putCommand(ge, cmd_ack, ~0);
    } else  {
      ge->hisLastSeq = seq;
      ge->sendFailures = 0;
      ge->noResponseSecs = 0;
      return(processCommand(ge, command, val, out1, out2, error));
    }
  } else  {
    /* Waiting for OK. */
    if (command == cmd_ack)  {
      if ((ack != ge->myLastSeq) || (seq != ge->hisLastSeq))  {
	/* Sequence error. */
	fprintf(stderr, "Sequence error.\n");
	return(gmp_nothing);
      }
      ge->sendFailures = 0;
      ge->noResponseSecs = 0;
      ge->waitingHighAck = 0;
      if (!gmp_verified(ge)) {
	askQuery(ge);
      }
      processQ(ge);
    } else if ((command == cmd_reset) && (ge->iAmWhite == -1)) {
      if (gmp_debug)
	fprintf(stderr, "gmp/his last seq = %d\n", seq);
      ge->hisLastSeq = seq;
      ge->waitingHighAck = 0;
      return(processCommand(ge, command, val, out1, out2, error));
    } else if (seq == ge->hisLastSeq)  {
      /* His command is old. */
    } else if (ack == ge->myLastSeq)  {
      ge->sendFailures = 0;
      ge->noResponseSecs = 0;
      ge->waitingHighAck = 0;
      ge->hisLastSeq = seq;
      result = processCommand(ge, command, val, out1, out2, error);
      processQ(ge);
      return(result);
    } else  {
      /* Conflict with opponent. */
      if (gmp_debug)
	fprintf(stderr, "Sending conflict.\n");
      ge->myLastSeq = 1 - ge->myLastSeq;
      ge->waitingHighAck = 0;
      processQ(ge);
    }
  }
  return(gmp_nothing);
}


static GmpResult  processCommand(Gmp *ge, Command command, int val,
				 int *out1, int *out2, const char **error)  {
  int  s, x, y;

  switch(command)  {
  case cmd_deny:
    putCommand(ge, cmd_ack, ~0);
    break;
  case cmd_query:
    return(respond(ge, val));
    break;
  case cmd_reset:  /* New game. */
    if (gmp_debug)
      fprintf(stderr, "GMP: Resetted.  New game.\n");
    askQuery(ge);
    return(gmp_reset);
    break;
  case cmd_undo:  /* Take back moves. */
    putCommand(ge, cmd_ack, ~0);
    *out1 = val;
    return(gmp_undo);
    break;
  case cmd_move:
    s = val & 0x1ff;
    if (s == 0)  {
      x = -1;
      y = 0;
    } else if (s == 0x1ff)  {
      x = -2;
      y = 0;
    } else  {
      --s;
      x = (s % ge->boardSize);
      y = ge->boardSize - 1 - (s / ge->boardSize);
    }
    putCommand(ge, cmd_ack, ~0);
    if (x == -1)
      return(gmp_pass);
    else  {
      if (gmp_verified(ge)) {
	*out1 = x;
	*out2 = y;
	return(gmp_move);
      } else {
	assert(ge->earlyMovePresent == 0);
	ge->earlyMovePresent = 1;
	ge->earlyMoveX = x;
	ge->earlyMoveY = y;
	askQuery(ge);
      }
    }
    break;
  case cmd_respond:
    return(gotQueryResponse(ge, val, error));
    break;
  default:  /* Don't understand command. */
    putCommand(ge, cmd_deny, 0);
    break;
  }
  return(gmp_nothing);
}


static void  putCommand(Gmp *ge, Command cmd, int val)  {
  if (ge->waitingHighAck &&
      (cmd != cmd_ack) && (cmd != cmd_respond) && (cmd != cmd_deny))  {
    if (ge->sendsQueued < 1024)  {
      ge->sendsPending[ge->sendsQueued].cmd = cmd;
      ge->sendsPending[ge->sendsQueued].val = val;
      ++ge->sendsQueued;
    } else  {
      if (gmp_debug)
	fprintf(stderr, "GMP: Send buffer full.  Catastrophic error.");
      exit(EXIT_FAILURE);
    }
    return;
  }
  if ((cmd == cmd_ack) && (ge->sendsQueued))  {
    ge->waitingHighAck = 0;
    processQ(ge);
    return;
  }
  if (cmd != cmd_ack)
    ge->myLastSeq ^= 1;
  ge->sendData[0] = ge->myLastSeq | (ge->hisLastSeq << 1);
  ge->sendData[2] = 0x80 | (cmd << 4) | ((val >> 7) & 7);
  ge->sendData[3] = 0x80 | val;
  ge->sendData[1] = checksum(ge->sendData);
  ge->lastSendTime = time(NULL);
  if (gmp_debug) {
    if (cmd == cmd_query) 
      fprintf(stderr, "GMP: Sending command: %s %s\n",
	      commandNames[cmd], queryNames[val]);
    else
      fprintf(stderr, "GMP: Sending command: %s\n", commandNames[cmd]);
  }
  write(ge->outFile, ge->sendData, 4);
  ge->waitingHighAck = (cmd != cmd_ack);
  return;
}


static GmpResult  respond(Gmp *ge, Query query)  {
  int  response;
  int  wasVerified;

  wasVerified = gmp_verified(ge);
  if (query & 0x200)  {
    /* Do you support this extended query? */
    response = 0;  /* No. */
  } else  {
    ge->waitingHighAck = 1;
    switch(query)  {
    case query_game:
      response = 1;  /* GO */
      break;
    case query_rules:
      if (ge->chineseRules == -1) {
	response = 0;
      } else {
	ge->rulesVerified = 1;
	if (ge->chineseRules == 1)
	  response = 2;
	else
	  response = 1;
      }
      break;
    case query_handicap:
      if (ge->handicap == -1)
	response = 0;
      else {
	ge->handicapVerified = 1;
	response = ge->handicap;
	if (response == 0)
	  response = 1;
      }
      break;
    case query_boardSize:
      if (ge->boardSize == -1) {
	response = 0;
      } else {
	response = ge->boardSize;
	ge->sizeVerified = 1;
      }
      break;
    case query_color:
      if (ge->iAmWhite == -1) {
	response = 0;
      } else  {
	ge->colorVerified = 1;
	if (ge->iAmWhite)
	  response = 1;
	else
	  response = 2;
      }
      break;
    default:
      response = 0;
      break;
    }    
  }
  putCommand(ge, cmd_respond, response);
  if (!wasVerified && gmp_verified(ge)) {
    if (gmp_debug)
      fprintf(stderr, "GMP: New game ready.\n");
    return(gmp_newGame);
  } else {
    return(gmp_nothing);
  }
}


static void  askQuery(Gmp *ge)  {
  if (!ge->simplified) {
    if (!ge->rulesVerified) {
      ge->lastQuerySent = query_rules;
    } else if (!ge->sizeVerified) {
      ge->lastQuerySent = query_boardSize;
    } else if (!ge->handicapVerified) {
      ge->lastQuerySent = query_handicap;
    /*  } else if (!ge->komiVerified) {
	  ge->lastQuerySent = query_komi; query komi is not define in GMP !? */
    } else {
      assert(!ge->colorVerified);
      ge->lastQuerySent = query_color;
    }
  }
  else {
    if (!ge->colorVerified)
      ge->lastQuerySent = query_color;
    else if (!ge->handicapVerified)
      ge->lastQuerySent = query_handicap;
  }

  putCommand(ge, cmd_query, ge->lastQuerySent);
}


static GmpResult  gotQueryResponse(Gmp *ge, int val, const char **err)  {
  static const char  *ruleNames[] = {"Japanese", "Chinese"};
  static const char  *colorNames[] = {"Black", "White"};
  static char errOut[200];

  switch(ge->lastQuerySent)  {
  case query_handicap:
    if (val <= 1)
      --val;
    if (ge->handicap == -1) {
      if (val == -1) {
	sprintf(errOut, "Neither player knows what the handicap should be.");
	*err = errOut;
	return(gmp_err);
      } else {
	ge->handicap = val;
	ge->handicapVerified = 1;
      }
    } else {
      ge->handicapVerified = 1;
      if ((val != -1) && (val != ge->handicap))  {
	sprintf(errOut, "Handicaps do not agree; I want %d, he wants %d.",
		ge->handicap, val);
	*err = errOut;
	return(gmp_err);
      }
    }
    break;
  case query_boardSize:
    if (ge->boardSize == -1)  {
      if (val == 0)  {
	sprintf(errOut, "Neither player knows what the board size should be.");
	*err = errOut;
	return(gmp_err);
      } else {
	ge->boardSize = val;
	ge->sizeVerified = 1;
      }
    } else {
      ge->sizeVerified = 1;
      if ((val != 0) && (val != ge->boardSize))  {
	sprintf(errOut, "Board sizes do not agree; I want %d, he wants %d.",
		ge->boardSize, val);
	*err = errOut;
	return(gmp_err);
      }
    }
    break;
  case query_rules:
    if (ge->chineseRules == -1) {
      if (val == 0) {
	sprintf(errOut, "Neither player knows what rule set to use.");
	*err = errOut;
	return(gmp_err);
      } else {
	ge->chineseRules = val - 1;
	ge->rulesVerified = 1;
      }
    } else {
      ge->rulesVerified = 1;
      if (val != 0)  {
	if (ge->chineseRules != (val == 2))  {
	  sprintf(errOut, "Rule sets do not agree; I want %s, he wants %s.",
		  ruleNames[ge->chineseRules], ruleNames[val == 2]);
	  *err = errOut;
	  return(gmp_err);
	}
      }
    }
    break;
  case query_color:
    if (ge->iAmWhite == -1)  {
      if (val == 0)  {
	sprintf(errOut, "Neither player knows who is which color.");
	*err = errOut;
	return(gmp_err);
      } else {
	ge->iAmWhite = !(val == 1);
	ge->colorVerified = 1;
      }
    } else {
      ge->colorVerified = 1;
      if (val != 0)  {
	if (ge->iAmWhite == (val == 1))  {
	  sprintf(errOut, "Colors do not agree; we both want to be %s.",
		  colorNames[ge->iAmWhite]);
	  *err = errOut;
	  return(gmp_err);
	}
      }
    }
    break;
  default:
    break;
  }
  if (!gmp_verified(ge)) {
    askQuery(ge);
    return(gmp_nothing);
  } else {
    putCommand(ge, cmd_ack, ~0);
    if (gmp_debug)
      fprintf(stderr, "GMP: New game ready.\n");
    return(gmp_newGame);
  }
}


static int  heartbeat(Gmp *ge)  {
  Command  cmd;
  
  if (ge->waitingHighAck) {
    if (++ge->noResponseSecs
	> (ge->simplified ? SGMP_RETRYSECS : GMP_RETRYSECS))  {
      if (++ge->sendFailures
	  > (ge->simplified ? SGMP_TIMEOUTRETRIES : GMP_TIMEOUTRETRIES))  {
	return(0);
      } else  {
	if (gmp_debug) {
	  cmd = (ge->sendData[2] >> 4) & 7;
	  if (cmd == cmd_query) {
	    if (gmp_debug)
	      fprintf(stderr, "GMP: Sending command: %s %s (retry)\n",
		      commandNames[cmd],
		      queryNames[ge->sendData[3] & 0x7f]);
	  }
	  else
	    if (gmp_debug)
	      fprintf(stderr, "GMP: Sending command: %s (retry)\n",
		      commandNames[cmd]);
	}
	write(ge->outFile, ge->sendData, 4);
      }
    }
  }
  return(1);
}


void  gmp_startGame(Gmp *ge, int size, int handicap, float komi,
		    int chineseRules, int iAmWhite, int simplified)  {
  assert((size == -1) || ((size > 1) && (size <= 22)));
  assert((handicap >= -1) && (handicap <= 27));
  assert((chineseRules >= -1) && (chineseRules <= 1));
  assert((iAmWhite >= -1) && (iAmWhite <= 1));

  ge->boardSize = size;
  ge->sizeVerified = simplified;

  ge->handicap = handicap;
  ge->handicapVerified = 0;

  ge->komi = komi;

  ge->chineseRules = chineseRules;
  ge->rulesVerified = simplified;

  ge->iAmWhite = iAmWhite;
  ge->colorVerified = 0;

  ge->earlyMovePresent = 0;

  ge->simplified = simplified;

  if (iAmWhite != 1) {
    putCommand(ge, cmd_reset, 0);
  }
}


void  gmp_sendPass(Gmp *ge)  {
  int  arg;

  if (ge->iAmWhite)
    arg = 0x200;
  else
    arg = 0;
  putCommand(ge, cmd_move, arg);
}


void  gmp_sendMove(Gmp *ge, int x, int y)  {
  int  val;

  val = x + ge->boardSize * (ge->boardSize - 1 - y) + 1;
  if (ge->iAmWhite)
    val |= 0x200;
  putCommand(ge, cmd_move, val);
}


void  gmp_sendUndo(Gmp *ge, int numUndos)  {
  putCommand(ge, cmd_undo, numUndos);
}


const char  *gmp_resultString(GmpResult result)  {
  static const char  *names[] = {
    "Nothing", "Move", "Pass", "Reset", "New game", "Undo", "Error"};

  assert(result <= gmp_err);
  return(names[result]);
}


int  gmp_size(Gmp *ge)  {
  return(ge->boardSize);
}


int  gmp_handicap(Gmp *ge)  {
  return(ge->handicap);
}


float  gmp_komi(Gmp *ge)  {
  return(ge->komi);
}


int  gmp_chineseRules(Gmp *ge)  {
  return(ge->chineseRules);
}


int  gmp_iAmWhite(Gmp *ge)  {
  return(ge->iAmWhite);
}


static void  processQ(Gmp *ge)  {
  int  i;

  if (!ge->waitingHighAck && ge->sendsQueued)  {
    putCommand(ge, ge->sendsPending[0].cmd, ge->sendsPending[0].val);
    --ge->sendsQueued;
    for (i = 0;  i < ge->sendsQueued;  ++i)  {
      ge->sendsPending[i] = ge->sendsPending[i + 1];
    }
  }
}

/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
