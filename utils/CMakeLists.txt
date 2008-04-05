SET(utils_STAT_SRCS
    getopt.c
    getopt1.c
    random.c
    gg_utils.c
    winsocket.c
    )

ADD_LIBRARY(utils STATIC ${utils_STAT_SRCS})
