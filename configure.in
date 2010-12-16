dnl Process this file with autoconf to produce a configure script.

dnl *****************************************************************
dnl IMPORTANT: Don't forget to add corresponding output for --options
dnl if you add a non-trivial configure option here.
dnl *****************************************************************

dnl this is to determine if the config script is running in the proper place
dnl just give it one file relative to where it should be
AC_INIT([gnugo], [3.9.1])
AC_CONFIG_SRCDIR([engine/dragon.c])

AM_CONFIG_HEADER(config.h)
AC_PREREQ(2.52)dnl               dnl Minimum Autoconf version required.

AH_TEMPLATE([PACKAGE],
[Define to the name of the distribution.])

AH_TEMPLATE([GNU_PACKAGE],
[The concatenation of the strings "GNU ", and PACKAGE.])

AH_TEMPLATE([VERSION],
[Define to the version of the distribution.])

dnl this defines VERSION and PACKAGE
AM_INIT_AUTOMAKE
GNU_PACKAGE="GNU $PACKAGE"
AC_DEFINE_UNQUOTED(GNU_PACKAGE, "$GNU_PACKAGE")

AM_MAINTAINER_MODE

dnl See if user has expressed a preference for use of curses and/or color
dnl These set variables $enable_color and $with_curses to "no" if disabled
dnl "yes" if enabled, or undefined if not specified

AC_ARG_WITH(readline,
 [  --with-readline     try to use GNU Readline for command reading
  --without-readline  do not use GNU Readline (default)])

AC_ARG_WITH(curses,
 [  --with-curses       try to use curses for colored debugging output (default)
  --without-curses    do not use curses for colored debugging output])

AC_ARG_ENABLE(color,
 [  --enable-color              use curses or ansi escape sequences for colored
                                   debug output
  --disable-color             do not try to generated colored debug output])

dnl and look to see if they want to disable the grid optimisation

AC_ARG_ENABLE(grid-opt,
 [  --enable-grid-opt           enable the grid optimisation within the pattern
                                matcher (default)
  --enable-grid-opt=distrust  enable the grid optimisation in non-trusting mode
  --disable-grid-opt          disable the grid optimisation])

default_cache_size=-1
default_level=10
default_semeai_node_limit=500
default_owl_node_limit=1000

AC_ARG_ENABLE(cache-size,
  [  --enable-cache-size=n       reserve n MB RAM for caching (special value -1
                               default, corresponding to 8-11 MB depending on
			       platform)])

AC_ARG_ENABLE(level,
  [  --enable-level=n            n = default level (10 standard)])

AC_ARG_ENABLE(semeai-node-limit,
  [  --enable-semeai-node-limit=n        n = semeai variations (500 standard)])

AC_ARG_ENABLE(level,
  [  --enable-owl-node-limit=n           n = owl node limit (1000 standard)])

AC_ARG_ENABLE(dfa,
 [  --disable-dfa               use old non-dfa pattern matcher],
 [ if test ${enableval} = no; then	
     dfa_c=
   else	
     dfa_c=dfa
   fi] ,
 [ dfa_c=dfa ])

AC_ARG_ENABLE(chinese-rules,
 [  --enable-chinese-rules      use Chinese (area) counting
  --disable-chinese-rules     use Japanese counting (default)])

AC_ARG_ENABLE(resignation-allowed,
 [  --enable-resignation-allowed       resign lost games (default)
  --disable-resignation-allowed      never resign])

AC_ARG_ENABLE(metamachine,
 [  --enable-metamachine               enable metamachine
  --disable-metamachine              don't enable metamachine (default)])

AC_ARG_ENABLE(experimental-break-in,
 [  --enable-experimental-break-in     use the breakin module (default)
  --disable-experimental-break-in    don't use the breakin module])

AC_ARG_ENABLE(experimental-owl-ext,
 [  --enable-experimental-owl-ext      use the experimental GAIN/LOSS codes
  --disable-experimental-owl-ext     use standard owl module (default)])

AC_ARG_ENABLE(cosmic-gnugo,
 [  --enable-cosmic-gnugo              use center-oriented influence code
  --disable-cosmic-gnugo             use standard influence code (default)])

AC_ARG_ENABLE(large-scale,
 [  --enable-large-scale               look for large scale captures
  --disable-large-scale              don't seek large scale captures (default)])

AC_ARG_ENABLE(experimental-connections,
 [  --enable-experimental-connections  use experimental connection analysis 
                                         (default)
  --disable-experimental-connections use standard connection analysis])

AC_ARG_ENABLE(alternate-connections,
 [  --enable-alternate-connections     use alternate experimental connection
                                         analysis
  --disable-alternate-connections    use primary experimental connection 
                                         analysis (default)])

AC_ARG_ENABLE(socket-support,
 [  --disable-socket-support           don't compile GTP over TCP/IP support
  --enable-socket-support            compile TCP/IP support (default)])

AC_PROG_CC

dnl for automake 1.4.x
AC_EXEEXT

dnl add -lm to library list since we use some
dnl math functions such as pow and fabs

AC_SEARCH_LIBS(pow,m)

AC_CACHE_CHECK(
	[for mingw32],
	ac_cv_mingw32,
	AC_TRY_COMPILE(,
		[return __MINGW32__],
		ac_cv_mingw32="yes",
		ac_cv_mingw32="no")
)

if test $ac_cv_mingw32 = yes;then
    LIBS="$LIBS -lwsock32"
    AC_SEARCH_LIBS(vsnprintf, mingwex)
fi

AC_PROG_CPP
AC_PROG_RANLIB

dnl required since we use SUBDIRS in Makefile.am
AC_PROG_MAKE_SET

AC_HEADER_TIME

AC_C_CONST

AC_CHECK_HEADERS(unistd.h sys/time.h sys/times.h)
AC_CHECK_HEADERS(curses.h term.h ncurses/curses.h ncurses/term.h)

if test "$ac_cv_header_curses_h" = "yes";then
   curses_header="curses.h"
elif test "$ac_cv_header_ncurses_curses_h" = "yes";then
   curses_header="ncurses/curses.h"
else
   curses_header="no"
fi

if test "$ac_cv_header_term_h" = "yes";then
   term_header="term.h"
elif test "$ac_cv_header_ncurses_term_h" = "yes";then
   term_header="ncurses/term.h"
else
   term_header="no"
fi

AC_CHECK_SIZEOF(long,,[#include <stdio.h>])

dnl vsnprintf not universally available
dnl usleep not available in Unicos and mingw32
AC_CHECK_FUNCS(vsnprintf gettimeofday usleep times)

dnl if snprintf not available try to use g_snprintf from GLib
if test $ac_cv_func_vsnprintf = no; then
    AC_CHECK_PROG(glibconfig,glib-config,yes,no)
    if test $ac_cv_prog_glibconfig = yes;then
	glib_cflags=`glib-config --cflags`
	glib_libs=`glib-config --libs`
	CPPFLAGS="$CPPFLAGS $glib_cflags"
	LIBS="$LIBS $glib_libs"
	AC_CHECK_FUNCS(g_vsnprintf)
	AC_CHECK_HEADERS(glib.h)
	if test $ac_cv_func_g_vsnprintf = no; then
    	   AC_MSG_WARN([GLib installation problem.
	   Continuing without GLib ])
        fi
    else
    	   AC_MSG_WARN([Neither vsnprintf nor GLib found. GNU Go is
	   compiled but it is safer to get GLib at
	   http://www.gtk.org/])
    fi
fi
 
dnl ------ variadic #define -----------------------

AH_TEMPLATE([HAVE_VARIADIC_DEFINE],
[Define if #define can take a variable number of arguments])

AC_CACHE_CHECK(
	[for variadic cpp define],
	gnugo_cv_cpp_variadic_define,
	AC_TRY_COMPILE(
[#include <stdio.h>
#define zz(fmt,arg...) printf(fmt,##arg)],
		[zz("Hello");zz("%s","Hello");zz("%s%s","Hello","World")],
		gnugo_cv_cpp_variadic_define="yes",
		gnugo_cv_cpp_variadic_define="no")
)
if test "$gnugo_cv_cpp_variadic_define" = "yes";then
      AC_DEFINE(HAVE_VARIADIC_DEFINE)
fi

dnl ------ transparent unions ---------------------

AH_TEMPLATE([HAVE_TRANSPARENT_UNIONS],
[Define if your compiler supports transparent unions])

AC_CACHE_CHECK(
	[whether $CC supports transparent unions],
	gnugo_cv_transparent_unions,
	AC_TRY_COMPILE([],
[[  struct {
    union {
      int x;
      float y;
    }
  } A = { { .y = 0.0 } };
  A.y = 1.0;]],
		gnugo_cv_transparent_unions="yes",
		gnugo_cv_transparent_unions="no")
)

if test "$gnugo_cv_transparent_unions" = "yes"; then
	AC_DEFINE(HAVE_TRANSPARENT_UNIONS)
fi

dnl -------- color debugging support -----------

AH_TEMPLATE([TERMINFO],
[Define to 1 if termcap/terminfo is available.])

AH_TEMPLATE([ANSI_COLOR],
[Define to use ansi escape sequences for color debugging])

tmp_color_result="none"

if test "$with_curses" != no -a "$enable_color" != no ; then
  tmp_color_result="none (curses failed)"

  dnl Do a separate test for curses and termcap
  dnl DJGPP does have pdcurses, but not termcap

  dnl make sure that both curses.h and term.h are available
  dnl FIXME: better to actually figure out here what headers
  dnl are really required
  if test "$term_header" != "no" -a "$curses_header" != "no" ; then

    dnl check for a working termcap library
    AC_SEARCH_LIBS(tparm,ncurses curses pdcurses termcap terminfo termlib)

    if test "$ac_cv_search_tparm" != "no" ; then
      AC_DEFINE(TERMINFO)
    tmp_color_result="curses"
    fi
  fi
fi

if test "$with_curses" = no -a "$enable_color" != no ; then
  dnl we asked for color, but there is no termcap
  AC_DEFINE(ANSI_COLOR)
  tmp_color_result="ANSI color"

fi

AC_MSG_CHECKING(for color support)
AC_MSG_RESULT($tmp_color_result)

dnl -------- readline support -------------------

AH_TEMPLATE([READLINE], [Enable GNU Readline support])

if test "$with_readline" = yes ; then

    dnl check for a working termcap and readline library
    AC_SEARCH_LIBS(readline,termcap readline)

    if test "$ac_cv_search_readline" != "no" ; then
	AC_DEFINE(READLINE,1)
    else
	AC_DEFINE(READLINE,0)
    fi
else
    AC_DEFINE(READLINE,0)
fi

dnl ---------- grid optimisation ------------

AH_TEMPLATE([GRID_OPT],
[Define as 1 to use the grid optimisation, or 2 to run it in self-test mode])

if test "$enable_grid_opt" = "distrust" ; then
  AC_DEFINE(GRID_OPT, 2)
else
  if test "$enable_grid_opt" = "no" ; then
    AC_DEFINE(GRID_OPT, 0)
  else
    AC_DEFINE(GRID_OPT, 1)
  fi
fi


dnl ------------ set cache size ----------

AH_TEMPLATE([DEFAULT_MEMORY],
[Default hash table size in megabytes])

if test "$enable_cache_size" ; then
   AC_DEFINE_UNQUOTED(DEFAULT_MEMORY, $enable_cache_size)
else
   AC_DEFINE_UNQUOTED(DEFAULT_MEMORY, $default_cache_size)
fi


dnl ------------ set default level  ----------

AH_TEMPLATE([DEFAULT_LEVEL],
[Default level (strength). Up to 10 supported])

if test "$enable_level" ; then
   AC_DEFINE_UNQUOTED(DEFAULT_LEVEL, $enable_level)
else
   AC_DEFINE_UNQUOTED(DEFAULT_LEVEL, $default_level)
fi

dnl ------------ set cache size ----------

AH_TEMPLATE([OWL_NODE_LIMIT],
[Owl Node Limit])

if test "$enable_owl_node_limit" ; then
   AC_DEFINE_UNQUOTED(OWL_NODE_LIMIT, $enable_owl_node_limit)
else
   AC_DEFINE_UNQUOTED(OWL_NODE_LIMIT, $default_owl_node_limit)
fi


dnl ------------ set semeai variations  ----------

AH_TEMPLATE([SEMEAI_NODE_LIMIT],
[Semeai Variations. 500 default])

if test "$enable_semeai_node_limit" ; then
   AC_DEFINE_UNQUOTED(SEMEAI_NODE_LIMIT, $enable_semeai_node_limit)
else
   AC_DEFINE_UNQUOTED(SEMEAI_NODE_LIMIT, $default_semeai_node_limit)
fi

dnl ------------ dfa -------------------

AM_CONDITIONAL(DFA_ENABLED, test "$enable_dfa" != "no")

dnl FIXME: Is there a more elegant approach for this?
dnl force owl c files rebuild
rm -f \
       patterns/owl_attackpat.c \
       patterns/owl_defendpat.c \
       patterns/owl_vital_apat.c


dnl ------------ Chinese Rules -------------------

AH_TEMPLATE([CHINESE_RULES],
[Ruleset. Default Japanese])

if test "$enable_chinese_rules" = "yes" ; then
   AC_DEFINE(CHINESE_RULES, 1)
else
   AC_DEFINE(CHINESE_RULES, 0)
fi

dnl ------------ Resignation allowed-------------------

AH_TEMPLATE([RESIGNATION_ALLOWED],
[Allow resignation. Default enabled])

if test "$disable_resignation_allowed" = "yes" ; then
   AC_DEFINE(RESIGNATION_ALLOWED, 0)
else
   AC_DEFINE(RESIGNATION_ALLOWED, 1)
fi

dnl ------------ Oracle enabled-------------------

AH_TEMPLATE([ORACLE],
[Oracle. Default not enabled.])

if test "$enable_metamachine" = "yes" ; then
   AC_DEFINE(ORACLE, 1)
else
   AC_DEFINE(ORACLE, 0)
fi

dnl ------------ Experimental Breakin enabled-------------------

AH_TEMPLATE([USE_BREAK_IN],
[Break-in module. Enabled by default.])

if test "$enable_experimental_break_in" = "no" ; then
   AC_DEFINE(USE_BREAK_IN, 0)
else
   AC_DEFINE(USE_BREAK_IN, 1)
fi

dnl ------------ Owl extensions ------------

AH_TEMPLATE([EXPERIMENTAL_OWL_EXT],
[GAIN/LOSS codes. Disabled by default.])

if test "$enable_experimental_owl_ext" = "yes" ; then
   AC_DEFINE(EXPERIMENTAL_OWL_EXT, 1)
else
   AC_DEFINE(EXPERIMENTAL_OWL_EXT, 0)
fi

dnl ------------ Cosmic Consciousness -------------------

AH_TEMPLATE([COSMIC_GNUGO],
[Center oriented influence. Disabled by default.])

if test "$enable_cosmic_gnugo" = "yes" ; then
   AC_DEFINE(COSMIC_GNUGO, 1)
else
   AC_DEFINE(COSMIC_GNUGO, 0)
fi

dnl ------------ Large Scale -------------------

AH_TEMPLATE([LARGE_SCALE],
[Large Scale Captures. Disabled by default.])

if test "$enable_large_scale" = "yes" ; then
   AC_DEFINE(LARGE_SCALE, 1)
else
   AC_DEFINE(LARGE_SCALE, 0)
fi

dnl ------------ Connections -------------------

AH_TEMPLATE([EXPERIMENTAL_CONNECTIONS],
[Connection module. Default experimental.])

if test "$enable_experimental_connections" = "no" ; then
   AC_DEFINE(EXPERIMENTAL_CONNECTIONS, 0)
else
   AC_DEFINE(EXPERIMENTAL_CONNECTIONS, 1)
fi

dnl ------------ Connections -------------------

AH_TEMPLATE([ALTERNATE_CONNECTIONS],
[Connection module. Default standard.])

if test "$enable_alternate_connections" = "no" ; then
   AC_DEFINE(ALTERNATE_CONNECTIONS, 0)
else
   AC_DEFINE(ALTERNATE_CONNECTIONS, 1)
fi

dnl ------------ Owl Threats -------------------

AH_TEMPLATE([OWL_THREATS],
[Owl Threats. 0 standard.])

if test "$enable_owl_threats" = "yes" ; then
   AC_DEFINE(OWL_THREATS, 1)
else
   AC_DEFINE(OWL_THREATS, 0)
fi

dnl ------------ additional valgrind macros ------

AH_TEMPLATE([USE_VALGRIND],
[Define special valgrind macros.])

if test "$enable_valgrind" = "yes" ; then
   AC_DEFINE(USE_VALGRIND, 1)
else 
   AC_DEFINE(USE_VALGRIND, 0)
fi

dnl  ----------- special-case use of gcc ---------

dnl Not sure if we are supposed to be accessing this variable, but...

AM_CONDITIONAL(GCC_ONLY, test "$ac_compiler_gnu" = "yes")

dnl Now lines in Makefile.am can be prefixed @GCC_ONLY@.

AC_SUBST(GCC_MAJOR_VERSION)
AC_SUBST(GCC_MINOR_VERSION)
AC_SUBST(GNU_GO_WARNINGS)

if test "$ac_compiler_gnu" = "yes"; then

  dnl M4 escaping of brackets
  GCC_MAJOR_VERSION=`echo __GNUC__ | $CC -E -xc - | tail -n 1`
  GCC_MINOR_VERSION=`echo __GNUC_MINOR__ | $CC -E -xc - | tail -n 1`

  GNU_GO_WARNINGS='-Wall -W -Wpointer-arith -Wbad-function-cast -Wcast-qual -Wcast-align -Wwrite-strings -Wstrict-prototypes -Wmissing-prototypes -Wmissing-declarations -Wundef'

  if (test $GCC_MAJOR_VERSION -eq 3 && test $GCC_MINOR_VERSION -ge 4) || test $GCC_MAJOR_VERSION -gt 3; then
    GNU_GO_WARNINGS="$GNU_GO_WARNINGS -Wdeclaration-after-statement"
  fi

else
  GCC_MAJOR_VERSION=0
  GCC_MINOR_VERSION=0
  GNU_GO_WARNINGS=''
fi

dnl FIXME: please add warnings for other compilers!


AH_TEMPLATE([ENABLE_SOCKET_SUPPORT],
[Compile support for GTP communication over TCP/IP channel.])

if test "$enable_socket_support" != "no"; then
  # Check for all required headers, macros, structures and functions
  # at once.
  AC_CACHE_CHECK(
	[whether socket support can be compiled],
	gnugo_cv_sockets_supported,
	AC_TRY_LINK([#if !defined(_WIN32) && !defined(_WIN32_WCE)
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#else	/* on Windows */
#include <winsock.h>
#endif	/* on Windows */],
[[  struct sockaddr_in A;
  struct hostent *H;
  A.sin_family = AF_INET;
  A.sin_addr.s_addr = htonl(INADDR_ANY);
  A.sin_port = htons(0);
  gethostbyname(0);
  socket(PF_INET, SOCK_STREAM, 0);
  connect(0, 0, 0);
  bind(0, 0, 0);
  listen(0, 0);
  accept(0, 0, 0);]],
 	gnugo_cv_sockets_supported="yes",
	gnugo_cv_sockets_supported="no"))

  if test "$gnugo_cv_sockets_supported" = "yes"; then
    AC_DEFINE(ENABLE_SOCKET_SUPPORT)
  else
    if test "$enable_socket_support" = "yes"; then
      AC_MSG_WARN(
[[
*** Socket support was requested but does not pass configure test. ***
*** Proceed only if you know what you are doing.                   ***]])
    AC_DEFINE(ENABLE_SOCKET_SUPPORT)
    fi
  fi
fi

#AM_GNU_GETTEXT
#AC_LINK_FILES($nls_cv_header_libgt, $nls_cv_header_intl)

dnl FIXME:
dnl autoconf 2.50 recommends AC_CONFIG_FILES and AC_OUPUT
dnl This however requires automake 1.4p2 or better

AC_OUTPUT([Makefile interface/Makefile patterns/Makefile sgf/Makefile
utils/Makefile engine/Makefile doc/Makefile regression/Makefile])
