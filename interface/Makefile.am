bin_PROGRAMS = gnugo

EXTRA_DIST = gtp_examples gnugo.dsp gnugo.el make-xpms-file.el GoImage xpms \
             big-xpms gnugo-xpms.el gnugo-big-xpms.el CMakeLists.txt

noinst_HEADERS = interface.h gmp.h gtp.h

# Remove these files here... they are created locally
DISTCLEANFILES = *~

AM_CPPFLAGS = \
	$(GNU_GO_WARNINGS) \
	-I$(top_srcdir)/sgf \
	-I$(top_srcdir)/engine \
	-I$(top_srcdir)/utils

LDADD = \
	../engine/libengine.a \
	../patterns/libpatterns.a \
	../sgf/libsgf.a \
	../utils/libutils.a

gnugo_SOURCES = \
	main.c \
	play_ascii.c \
	play_gmp.c \
	play_gtp.c \
	play_solo.c \
	play_test.c \
	gmp.c \
	gtp.c

gnugo-xpms.el : $(shell ls xpms/*.xpm)
	emacs -batch --no-site-file -l make-xpms-file.el -f make-xpms-file $@ $(shell ls xpms/*.xpm)

gnugo-big-xpms.el : $(shell ls xpms/*.xpm)
	emacs -batch --no-site-file -l make-xpms-file.el -f make-xpms-file $@ $(shell ls big-xpms/*.xpm)

AIXOPTS=-O -qmaxmem=16384 -qro -qroconst -qinfo

# OPTIONS=$(AIXOPTS)
