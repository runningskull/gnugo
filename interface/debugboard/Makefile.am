EXTRA_PROGRAMS = debugboard

EXTRA_DIST = 

noinst_HEADERS = display.h gui.h

# Remove these files here... they are created locally
DISTCLEANFILES = *~

INCLUDES = \
	-I$(top_srcdir)/sgf \
	-I$(top_srcdir)/engine \
	-I$(top_srcdir)/interface \
	-I$(top_srcdir)/utils


LDADD = \
	$(top_builddir)/engine/libengine.a \
	$(top_builddir)/patterns/libpatterns.a \
	$(top_builddir)/patterns/libdfa.a \
	$(top_builddir)/sgf/libsgf.a \
	$(top_builddir)/utils/libutils.a

debugboard_SOURCES = \
	main.c \
	gui.c \
	display.c

AIXOPTS=-O -qmaxmem=16384 -qro -qroconst -qinfo

# OPTIONS=$(AIXOPTS)
