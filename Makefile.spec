###############################################################################
# SPECtools2004
#   Version 0.1
#   This makefile should NOT be changed other than to customize for a 
#   particular benchmark.  Users may overide variables by placing them
#   in the Makefile.spec file, or by using the "runspec" program to 
#   build the Makefile.spec file for them.
###############################################################################
#	BENCHMARK	-- standard definitions for this benchmark
# See Spec/object.pm for explanations of what's what

NUMBER    = 000
NAME      = GNU Go
EXEBASE   = gnugo
BENCHLANG = C
SOURCES   = \
	sgf/sgf_utils.c \
	sgf/sgftree.c \
	sgf/sgfnode.c \
	engine/aftermath.c \
	engine/board.c \
	engine/cache.c \
	engine/combination.c \
	engine/dragon.c \
	engine/filllib.c \
	engine/fuseki.c \
	engine/genmove.c \
	engine/hash.c \
	engine/influence.c \
	engine/interface.c \
	engine/matchpat.c \
	engine/move_reasons.c \
	engine/movelist.c \
	engine/optics.c \
	engine/owl.c \
	engine/printutils.c \
	engine/readconnect.c \
	engine/reading.c \
	engine/score.c \
	engine/semeai.c \
	engine/sgfdecide.c \
	engine/sgffile.c \
	engine/shapes.c \
	engine/showbord.c \
	engine/utils.c \
	engine/value_moves.c \
	engine/worm.c \
	engine/globals.c \
	engine/persistent.c \
	engine/handicap.c \
	engine/surround.c \
	interface/gtp.c \
	interface/main.c \
	interface/play_ascii.c \
	interface/play_gtp.c \
	interface/play_solo.c \
	interface/play_test.c \
	patterns/connections.c \
	patterns/dfa.c \
	patterns/helpers.c \
	patterns/transform.c \
	patterns/owl_attackpat.c \
	patterns/conn.c \
	patterns/patterns.c \
	patterns/apatterns.c \
	patterns/dpatterns.c \
	patterns/owl_vital_apat.c \
	patterns/eyes.c \
	patterns/influence.c \
	patterns/barriers.c \
	patterns/endgame.c \
	patterns/aa_attackpat.c \
	patterns/owl_defendpat.c \
	patterns/fusekipat.c \
	patterns/fuseki9.c \
	patterns/fuseki13.c \
	patterns/fuseki19.c \
	patterns/josekidb.c \
	patterns/handipat.c \
	utils/getopt.c \
	utils/getopt1.c \
	utils/gg_utils.c \
	utils/random.c

NEED_MATH = 1

# Include standard template for this suite
-include ../../../../Makefile.defaults
-include ../../../Makefile.defaults
-include ../../Makefile.defaults
-include ../Makefile.defaults

nomakefiles:
	@echo "*** Whoops!  I didn't find a Makefile.defaults file."
	@echo "*** Your distribution is probably corrupt."
	@echo
