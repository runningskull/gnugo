INCLUDE_DIRECTORIES(${GNUGo_SOURCE_DIR}/utils)

########### sgfgen program ###############

SET(sgfgen_SRCS
    sgfgen.c
    )

ADD_EXECUTABLE(sgfgen ${sgfgen_SRCS})

TARGET_LINK_LIBRARIES(sgfgen)


########### sgf library ###############

SET(sgf_STAT_SRCS
    sgf_utils.c
    sgfnode.c
    sgftree.c
    )

ADD_LIBRARY(sgf STATIC ${sgf_STAT_SRCS})
