EXTRA_DIST = CMakeLists.txt

# Remove these files here... they are created locally
DISTCLEANFILES = *~

AM_CPPFLAGS = \
	$(GNU_GO_WARNINGS) \
	-I../patterns \
	-I$(top_srcdir)/patterns \
	-I$(top_srcdir)/sgf \
	-I$(top_srcdir)/utils

noinst_HEADERS = cache.h gnugo.h hash.h clock.h readconnect.h \
                 influence.h liberty.h move_reasons.h board.h

# preconfigured settings for various configurations
noinst_LIBRARIES = libengine.a libboard.a

libengine_a_SOURCES = \
      aftermath.c \
      board.c \
      boardlib.c \
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
      montecarlo.c \
      move_reasons.c \
      movelist.c \
      optics.c \
      oracle.c \
      owl.c \
      persistent.c \
      printutils.c \
      readconnect.c \
      reading.c \
      semeai.c \
      sgfdecide.c \
      sgffile.c \
      shapes.c \
      showbord.c \
      surround.c \
      unconditional.c \
      utils.c \
      value_moves.c \
      worm.c

libboard_a_SOURCES = \
      board.c \
      boardlib.c \
      hash.c \
      printutils.c
