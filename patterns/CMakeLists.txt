INCLUDE_DIRECTORIES(${GNUGo_SOURCE_DIR}/patterns)
INCLUDE_DIRECTORIES(${GNUGo_SOURCE_DIR}/engine)
INCLUDE_DIRECTORIES(${GNUGo_SOURCE_DIR}/sgf)
INCLUDE_DIRECTORIES(${GNUGo_SOURCE_DIR}/utils)

########### mkpat program ###############

SET(mkpat_SRCS
    mkpat.c
    transform.c
    dfa.c
    )

ADD_EXECUTABLE(mkpat ${mkpat_SRCS})

TARGET_LINK_LIBRARIES(mkpat utils)


########### joseki program ###############

SET(joseki_SRCS
    joseki.c
    )

ADD_EXECUTABLE(joseki ${joseki_SRCS})

TARGET_LINK_LIBRARIES(joseki board sgf utils)


########### mkeyes program ###############

SET(mkeyes_SRCS
    mkeyes.c
    )

ADD_EXECUTABLE(mkeyes ${mkeyes_SRCS})

TARGET_LINK_LIBRARIES(mkeyes utils)


########### mkmcpat program ###############

SET(mkmcpat_SRCS
    mkmcpat.c
    )

ADD_EXECUTABLE(mkmcpat ${mkmcpat_SRCS})

IF(UNIX)
    SET(PLATFORM_LIBRARIES m)
ENDIF(UNIX)

TARGET_LINK_LIBRARIES(mkmcpat engine sgf utils ${PLATFORM_LIBRARIES})


########### uncompress_fuseki program ###############

SET(uncompress_fuseki_SRCS
    uncompress_fuseki.c
    )

ADD_EXECUTABLE(uncompress_fuseki ${uncompress_fuseki_SRCS})

TARGET_LINK_LIBRARIES(uncompress_fuseki utils board sgf)


########### extract_fuseki program ###############

SET(extract_fuseki_SRCS
    extract_fuseki.c
    )

ADD_EXECUTABLE(extract_fuseki ${extract_fuseki_SRCS})

TARGET_LINK_LIBRARIES(extract_fuseki engine patterns
                      engine patterns sgf utils)


########### next target ###############

SET(compress_fuseki_SRCS
    compress_fuseki.c
    )

ADD_EXECUTABLE(compress_fuseki ${compress_fuseki_SRCS})

TARGET_LINK_LIBRARIES(compress_fuseki)


########### Generate files. ##############

GET_TARGET_PROPERTY(JOSEKI_EXE joseki LOCATION)
GET_TARGET_PROPERTY(MKPAT_EXE mkpat LOCATION)
GET_TARGET_PROPERTY(MKEYES_EXE mkeyes LOCATION)
GET_TARGET_PROPERTY(MKMCPAT_EXE mkmcpat LOCATION)
GET_TARGET_PROPERTY(UNCOMPRESS_FUSEKI_EXE uncompress_fuseki LOCATION)

SET(JOSEKI_INPUTS "")
SET(JOSEKI_NAMES "")
SET(GG_BUILT_SOURCES "")
MACRO(BUILD_JOSEKI NAME PREFIX)
    ADD_CUSTOM_COMMAND(
        OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${NAME}.db
        COMMAND ${JOSEKI_EXE} ${PREFIX}
	        ${CMAKE_CURRENT_SOURCE_DIR}/${NAME}.sgf
                > ${CMAKE_CURRENT_BINARY_DIR}/${NAME}.db
        DEPENDS joseki ${CMAKE_CURRENT_SOURCE_DIR}/${NAME}.sgf
    )
    SET(JOSEKI_INPUTS ${JOSEKI_INPUTS}
                      "-i" ${CMAKE_CURRENT_BINARY_DIR}/${NAME}.db)
    SET(JOSEKI_NAMES ${JOSEKI_NAMES}
                     ${CMAKE_CURRENT_BINARY_DIR}/${NAME}.db)
ENDMACRO(BUILD_JOSEKI)

BUILD_JOSEKI(gogo JG)
BUILD_JOSEKI(hoshi_keima JHK)
BUILD_JOSEKI(hoshi_other JHO)
BUILD_JOSEKI(komoku JK)
BUILD_JOSEKI(sansan JS)
BUILD_JOSEKI(mokuhazushi JM)
BUILD_JOSEKI(takamoku JT)

MACRO(RUN_MKPAT OPTIONS1 OPTIONS2 PATNAME DBNAME CNAME)
    ADD_CUSTOM_COMMAND(
        OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${CNAME}
        COMMAND ${MKPAT_EXE} ${OPTIONS1} ${OPTIONS2} ${PATNAME}
                             -i ${CMAKE_CURRENT_SOURCE_DIR}/${DBNAME}
                             -o ${CMAKE_CURRENT_BINARY_DIR}/${CNAME}
        DEPENDS mkpat ${CMAKE_CURRENT_SOURCE_DIR}/${DBNAME}
        )
    SET(GG_BUILT_SOURCES ${GG_BUILT_SOURCES} ${CMAKE_CURRENT_BINARY_DIR}/${CNAME})
ENDMACRO(RUN_MKPAT)

SET(DFAFLAGS -D -m)

MACRO(RUN_MKPAT_DFA OPTIONS PATNAME DTRNAME DBNAME CNAME)
    ADD_CUSTOM_COMMAND(
        OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${CNAME}
        COMMAND ${MKPAT_EXE} ${DFAFLAGS} ${OPTIONS}
                             -t ${CMAKE_CURRENT_SOURCE_DIR}/${DTRNAME} ${PATNAME}
                             -i ${CMAKE_CURRENT_SOURCE_DIR}/${DBNAME}
                             -o ${CMAKE_CURRENT_BINARY_DIR}/${CNAME}
        DEPENDS mkpat ${CMAKE_CURRENT_SOURCE_DIR}/${DBNAME}
                      ${CMAKE_CURRENT_SOURCE_DIR}/${DTRNAME}
        )
    SET(GG_BUILT_SOURCES ${GG_BUILT_SOURCES} ${CMAKE_CURRENT_BINARY_DIR}/${CNAME})
ENDMACRO(RUN_MKPAT_DFA)

MACRO(RUN_UNCOMPRESS_FUSEKI BOARDSIZE)
    ADD_CUSTOM_COMMAND(
        OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/fuseki${BOARDSIZE}.c
        COMMAND ${UNCOMPRESS_FUSEKI_EXE} ${BOARDSIZE}
                             ${CMAKE_CURRENT_SOURCE_DIR}/fuseki${BOARDSIZE}.dbz
                             c > ${CMAKE_CURRENT_BINARY_DIR}/fuseki${BOARDSIZE}.c
        DEPENDS uncompress_fuseki
                ${CMAKE_CURRENT_SOURCE_DIR}/fuseki${BOARDSIZE}.dbz
        )
    SET(GG_BUILT_SOURCES ${GG_BUILT_SOURCES}
                         ${CMAKE_CURRENT_BINARY_DIR}/fuseki${BOARDSIZE}.c)
ENDMACRO(RUN_UNCOMPRESS_FUSEKI)

# FIXME: It's very ugly that the RUN_MKPAT macro takes two separate
# option arguments, where one is in most cases eliminated by using "".
# The problem with just having one option argument is that specifying
# it as "-c -b" causes the space to be escaped into "-c\ -b". There is
# probably some trivial workaround for someone who actually knows cmake.
RUN_MKPAT(-X "" attpat attack.db apatterns.c)
RUN_MKPAT("" "" defpat defense.db dpatterns.c)
RUN_MKPAT(-b "" handipat handicap.db handipat.c)
RUN_MKPAT(-c "" influencepat influence.db influence.c)
RUN_MKPAT(-c -b barrierspat barriers.db barriers.c)
RUN_MKPAT(-b "" endpat endgame.db endgame.c)
RUN_MKPAT(-c "" conn conn.db conn.c)
RUN_MKPAT(-b "" fusekipat fuseki.db fusekipat.c)
RUN_MKPAT_DFA(-b aa_attackpat aa_attackpats.dtr aa_attackpats.db aa_attackpat.c)
RUN_MKPAT_DFA(-b owl_vital_apat owl_vital_apats.dtr owl_vital_apats.db owl_vital_apat.c)
RUN_MKPAT_DFA(-b owl_attackpat owl_attackpats.dtr owl_attackpats.db owl_attackpat.c)
RUN_MKPAT_DFA(-b owl_defendpat owl_defendpats.dtr owl_defendpats.db owl_defendpat.c)
RUN_UNCOMPRESS_FUSEKI(9)
RUN_UNCOMPRESS_FUSEKI(13)
RUN_UNCOMPRESS_FUSEKI(19)

ADD_CUSTOM_COMMAND(
   OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/josekidb.c
   COMMAND ${MKPAT_EXE} -C joseki ${JOSEKI_INPUTS}
                        -o ${CMAKE_CURRENT_BINARY_DIR}/josekidb.c
   DEPENDS mkpat ${JOSEKI_NAMES}
   )
SET(GG_BUILT_SOURCES ${GG_BUILT_SOURCES}
                     ${CMAKE_CURRENT_BINARY_DIR}/josekidb.c)

ADD_CUSTOM_COMMAND(
   OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/patterns.c
   COMMAND ${MKPAT_EXE} -b pat -i ${CMAKE_CURRENT_SOURCE_DIR}/patterns.db
                               -i ${CMAKE_CURRENT_SOURCE_DIR}/patterns2.db
                               -o ${CMAKE_CURRENT_BINARY_DIR}/patterns.c
   DEPENDS mkpat ${CMAKE_CURRENT_SOURCE_DIR}/patterns.db
                 ${CMAKE_CURRENT_SOURCE_DIR}/patterns2.db
   )
SET(GG_BUILT_SOURCES ${GG_BUILT_SOURCES}
                     ${CMAKE_CURRENT_BINARY_DIR}/patterns.c)

ADD_CUSTOM_COMMAND(
   OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/eyes.c
   COMMAND ${MKEYES_EXE} < ${CMAKE_CURRENT_SOURCE_DIR}/eyes.db
                         > ${CMAKE_CURRENT_BINARY_DIR}/eyes.c
   DEPENDS mkeyes ${CMAKE_CURRENT_SOURCE_DIR}/eyes.db
   )
SET(GG_BUILT_SOURCES ${GG_BUILT_SOURCES} ${CMAKE_CURRENT_BINARY_DIR}/eyes.c)

ADD_CUSTOM_COMMAND(
   OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/mcpat.c
   COMMAND ${MKMCPAT_EXE} ${CMAKE_CURRENT_SOURCE_DIR}/mc_montegnu_classic.db
                          ${CMAKE_CURRENT_SOURCE_DIR}/mc_mogo_classic.db
                          ${CMAKE_CURRENT_SOURCE_DIR}/mc_uniform.db
			  > ${CMAKE_CURRENT_BINARY_DIR}/mcpat.c
   DEPENDS mkmcpat ${CMAKE_CURRENT_SOURCE_DIR}/mkmcpat.c
                   ${CMAKE_CURRENT_SOURCE_DIR}/mc_montegnu_classic.db
                   ${CMAKE_CURRENT_SOURCE_DIR}/mc_mogo_classic.db
                   ${CMAKE_CURRENT_SOURCE_DIR}/mc_uniform.db
   )

SET(GG_BUILT_SOURCES ${GG_BUILT_SOURCES}
                     ${CMAKE_CURRENT_BINARY_DIR}/mcpat.c)



########### patterns library ###############

SET(patterns_STAT_SRCS
    connections.c
    helpers.c
    transform.c
    ${GG_BUILT_SOURCES}
    )

ADD_LIBRARY(patterns STATIC ${patterns_STAT_SRCS})
