noinst_PROGRAMS = sgfgen

sgfgen_SOURCES = sgfgen.c

EXTRA_DIST = sgf_extras.def sgf_properties.def sgfgen.dsp sgf.dsp sgfgen.dsp

noinst_HEADERS = \
	sgftree.h\
	sgf_properties.h

GGBUILTSOURCES = sgf_properties.h

# Remove these files here... they are created locally
# don't clean up sgfproperties.h
DISTCLEANFILES = *~

INCLUDES =\
	-I$(top_srcdir)/engine\
	-I$(top_srcdir)/utils\
	-I$(top_srcdir)/interface

noinst_LIBRARIES = libsgf.a

libsgf_a_SOURCES = \
	sgf_utils.c \
	sgfnode.c \
	sgftree.c

sgf_properties.h: $(srcdir)/sgf_properties.def $(srcdir)/sgf_extras.def sgfgen$(EXEEXT)
	cat $(srcdir)/sgf_properties.def $(srcdir)/sgf_extras.def |\
		./sgfgen > sgf_properties.h
