bin_PROGRAMS = gnugo

SUBDIRS = debugboard

EXTRA_DIST = gtp_examples gnugo.dsp gnugo.el make-xpms-file.el GoImage xpms

GGBUILTSOURCES = gnugo-xpms.el

noinst_HEADERS = interface.h gmp.h gtp.h

# Remove these files here... they are created locally
DISTCLEANFILES = *~

INCLUDES = \
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

AIXOPTS=-O -qmaxmem=16384 -qro -qroconst -qinfo

# OPTIONS=$(AIXOPTS)
