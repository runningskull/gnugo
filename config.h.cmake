/* Ruleset. Default Japanese */
#define CHINESE_RULES 0

/* Allow resignation. Default enabled */
#define RESIGNATION_ALLOWED 1

/* Default level (strength). Up to 10 supported */
#define DEFAULT_LEVEL 10

/* Center oriented influence. Disabled by default. */
#define COSMIC_GNUGO 0

/* Owl Node Limit. 1000 default. */
#define OWL_NODE_LIMIT 1000

/* Semeai Variations. 500 default */
#define SEMEAI_NODE_LIMIT 500

/* Default hash table size in megabytes */
#define DEFAULT_MEMORY -1

/* Compile support for GTP communication over TCP/IP channel. */
#undef ENABLE_SOCKET_SUPPORT

/* GAIN/LOSS codes. Disabled by default. */
#define EXPERIMENTAL_OWL_EXT 0

/* Large Scale Captures. Disabled by default. */
#define LARGE_SCALE 0

/* Oracle. Default not enabled. */
#define ORACLE 0

/* Owl Threats. 0 standard. */
#define OWL_THREATS 0

/* Break-in module. Enabled by default. */
#define USE_BREAK_IN 1

/* Connection module. Default experimental. */
#define EXPERIMENTAL_CONNECTIONS 1

/* Connection module. Default standard. */
#define ALTERNATE_CONNECTIONS 1

/* Define as 1 to use the grid optimisation, or 2 to run it in self-test mode
   */
#define GRID_OPT 1

/* Define to use ansi escape sequences for color debugging */
#undef ANSI_COLOR

/* Define to 1 if you have the <curses.h> header file. */
#cmakedefine HAVE_CURSES_H 1

/* Define to 1 if you have the `gettimeofday' function. */
#cmakedefine HAVE_GETTIMEOFDAY 1

/* Define to 1 if you have the <glib.h> header file. */
#cmakedefine HAVE_GLIB_H 1

/* Define to 1 if you have the `g_vsnprintf' function. */
#cmakedefine HAVE_G_VSNPRINTF 1

/* Define to 1 if you have the <ncurses/curses.h> header file. */
#cmakedefine HAVE_NCURSES_CURSES_H 1

/* Define to 1 if you have the <ncurses/term.h> header file. */
#cmakedefine HAVE_NCURSES_TERM_H 1

/* Define to 1 if you have the <sys/times.h> header file. */
#cmakedefine HAVE_SYS_TIMES_H 1

/* Define to 1 if you have the <sys/time.h> header file. */
#cmakedefine HAVE_SYS_TIME_H 1

/* Define to 1 if you have the <sys/types.h> header file. */
#cmakedefine HAVE_SYS_TYPES_H 1

/* Define to 1 if you have the <term.h> header file. */
#cmakedefine HAVE_TERM_H 1

/* Define to 1 if you have the <crtdbg.h> header file. */
#cmakedefine HAVE_CRTDBG_H 1

/* Define to 1 if you have the <winsock.h> and <io.h> header files. */
#cmakedefine HAVE_WINSOCK_IO_H 1

/* Define to 1 if you have the `times' function. */
#cmakedefine HAVE_TIMES 1

/* Define if your compiler supports transparent unions */
#undef HAVE_TRANSPARENT_UNIONS

/* Define to 1 if you have the <unistd.h> header file. */
#cmakedefine HAVE_UNISTD_H 1

/* Define to 1 if you have the `usleep' function. */
#cmakedefine HAVE_USLEEP 1

/* Define if #define can take a variable number of arguments */
#undef HAVE_VARIADIC_DEFINE

/* Define to 1 if you have the `vsnprintf' function. */
#cmakedefine HAVE_VSNPRINTF 1

/* Define to 1 if you have the `_vsnprintf' function. */
#cmakedefine HAVE__VSNPRINTF 1

/* Enable GNU Readline support */
#undef READLINE

/* The size of a `long', as computed by sizeof. */
#cmakedefine SIZEOF_LONG ${SIZEOF_LONG}

/* Define to 1 if termcap/terminfo is available. */
#undef TERMINFO

/* Define to 1 if you can safely include both <sys/time.h> and <time.h>. */
#cmakedefine TIME_WITH_SYS_TIME 1

/* Define special valgrind macros. */
#undef USE_VALGRIND

/* Version number of package */
#define VERSION "3.9.1"

/* Define to empty if `const' does not conform to ANSI C. */
#undef const

${PRAGMAS}
