EXTRA_DIST = engine.dsp board.dsp

# Remove these files here... they are created locally
DISTCLEANFILES = *~

INCLUDES = \
	-I../patterns \
	-I$(top_srcdir)/patterns \
	-I$(top_srcdir)/sgf \
	-I$(top_srcdir)/utils

noinst_HEADERS = cache.h gnugo.h hash.h clock.h \
                 influence.h liberty.h move_reasons.h

# preconfigured settings for various configurations
noinst_LIBRARIES = libengine.a libboard.a

libengine_a_SOURCES = \
      aftermath.c \
      board.c \
      cache.c \
      clock.c \
      dragon.c \
      filllib.c \
      fuseki.c \
      genmove.c \
      globals.c \
      hash.c \
      influence.c \
      interface.c \
      life.c \
      matchpat.c \
      move_reasons.c \
      optics.c \
      owl.c \
      printutils.c \
      readconnect.c \
      reading.c \
      score.c \
      semeai.c \
      sgfdecide.c \
      sgffile.c \
      shapes.c \
      showbord.c \
      utils.c \
      worm.c

libboard_a_SOURCES = \
      board.c \
      cache.c \
      globals.c \
      hash.c \
      printutils.c \
      sgffile.c \
      showbord.c
