INCLUDE_DIRECTORIES(${CMAKE_CURRENT_SOURCE_DIR})
INCLUDE_DIRECTORIES(${GNUGo_SOURCE_DIR}/patterns)
INCLUDE_DIRECTORIES(${GNUGo_SOURCE_DIR}/sgf)
INCLUDE_DIRECTORIES(${GNUGo_SOURCE_DIR}/utils)


########### engine library ###############

SET(engine_STAT_SRCS
    aftermath.c
    board.c
    boardlib.c
    breakin.c
    cache.c
    clock.c
    combination.c
    dragon.c
    endgame.c
    filllib.c
    fuseki.c
    genmove.c
    globals.c
    handicap.c
    hash.c
    influence.c
    interface.c
    matchpat.c
    montecarlo.c
    move_reasons.c
    movelist.c
    optics.c
    oracle.c
    owl.c
    persistent.c
    printutils.c
    readconnect.c
    reading.c
    semeai.c
    sgfdecide.c
    sgffile.c
    shapes.c
    showbord.c
    surround.c
    unconditional.c
    utils.c
    value_moves.c
    worm.c
    )

ADD_LIBRARY(engine STATIC ${engine_STAT_SRCS})


########### board library ###############

SET(board_STAT_SRCS
    board.c
    boardlib.c
    hash.c
    printutils.c
    )

ADD_LIBRARY(board STATIC ${board_STAT_SRCS})
