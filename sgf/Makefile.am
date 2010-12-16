noinst_PROGRAMS = sgfgen

sgfgen_SOURCES = sgfgen.c

EXTRA_DIST = sgf_extras.def sgf_properties.def CMakeLists.txt

noinst_HEADERS = \
	sgftree.h\
	sgf_properties.h

# Remove these files here... they are created locally
DISTCLEANFILES = *~

AM_CPPFLAGS = $(GNU_GO_WARNINGS) -I$(top_srcdir)/utils

noinst_LIBRARIES = libsgf.a

libsgf_a_SOURCES = \
	sgf_utils.c \
	sgfnode.c \
	sgftree.c

# To rebuild sgf_properties.h in case sgf_properties.def or sgf_extras.def 
# is changed, 'make sgf_properties'. Normally this is not needed.
sgf_properties:
	cat $(srcdir)/sgf_properties.def $(srcdir)/sgf_extras.def |\
		./sgfgen > sgf_properties.h
