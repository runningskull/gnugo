EXTRA_DIST = engine.dsp board.dsp

# Remove these files here... they are created locally
DISTCLEANFILES = *~

INCLUDES = \
	-I../patterns \
	-I$(top_srcdir)/patterns \
	-I$(top_srcdir)/sgf \
	-I$(top_srcdir)/utils

noinst_HEADERS = cache.h gnugo.h hash.h clock.h readconnect.h \
                 influence.h liberty.h move_reasons.h

# preconfigured settings for various configurations
noinst_LIBRARIES = libengine.a libboard.a

libengine_a_SOURCES = \
      aftermath.c \
      board.c \
      breakin.c \
      cache.c \
      clock.c \
      combination.c \
      dragon.c \
      endgame.c \
      filllib.c \
      fuseki.c \
      genmove.c \
      globals.c \
      handicap.c \
      hash.c \
      influence.c \
      interface.c \
      matchpat.c \
      move_reasons.c \
      movelist.c \
      optics.c \
      oracle.c \
      owl.c \
      persistent.c \
      printutils.c \
      readconnect.c \
      reading.c \
      score.c \
      semeai.c \
      sgfdecide.c \
      sgffile.c \
      shapes.c \
      showbord.c \
      surround.c \
      utils.c \
      value_moves.c \
      worm.c

libboard_a_SOURCES = \
      board.c \
      globals.c \
      hash.c \
      printutils.c \
      sgffile.c \
      showbord.c
