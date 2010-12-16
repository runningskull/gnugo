noinst_PROGRAMS = mkpat joseki mkeyes uncompress_fuseki mkmcpat
EXTRA_PROGRAMS = extract_fuseki compress_fuseki

DTR = aa_attackpats.dtr owl_attackpats.dtr owl_defendpats.dtr \
       owl_vital_apats.dtr

EXTRA_DIST = $(DTR)\
	dfa.c\
	gnugo-db.el\
        hoshi_keima.sgf\
        komoku.sgf\
        gogo.sgf\
        hoshi_other.sgf\
        mokuhazushi.sgf\
        takamoku.sgf\
        sansan.sgf\
        aa_attackpats.db\
        attack.db\
        barriers.db\
        conn.db\
        defense.db\
        endgame.db\
        eyes.db\
        fuseki.db\
        gogo.db\
        handicap.db\
        hoshi_keima.db\
        hoshi_other.db\
        influence.db\
        komoku.db\
        mc_mogo_classic.db\
        mc_montegnu_classic.db\
        mc_uniform.db\
        mokuhazushi.db\
        oracle.db\
        owl_attackpats.db\
        owl_defendpats.db\
        owl_vital_apats.db\
        patterns2.db\
        patterns.db\
        sansan.db\
        takamoku.db\
	fuseki13.dbz\
	fuseki19.dbz\
	fuseki9.dbz\
	CMakeLists.txt

mkpat_SOURCES  = mkpat.c transform.c dfa.c

mkpat_LDADD = ../utils/libutils.a

if DFA_ENABLED
DFAFLAGS = -D -m
else
DFAFLAGS =
endif

joseki_SOURCES = joseki.c 
joseki_LDADD = ../engine/libboard.a ../sgf/libsgf.a ../utils/libutils.a 
joseki_AM_CPPFLAGS = $(GNU_GO_WARNINGS) -I$(top_srcdir)/sgf
mkeyes_SOURCES = mkeyes.c
mkeyes_LDADD = ../utils/libutils.a
mkmcpat_SOURCES  = mkmcpat.c ../engine/globals.c
mkmcpat_LDADD = ../engine/libengine.a ../sgf/libsgf.a ../utils/libutils.a
mkmcpat_AM_CPPFLAGS = $(GNU_GO_WARNINGS)
extract_fuseki_SOURCES  = extract_fuseki.c
# Yes, we currently need duplicate libengine.a and libpatterns.a.
extract_fuseki_LDADD = ../engine/libengine.a libpatterns.a\
		       ../engine/libengine.a libpatterns.a\
                       ../sgf/libsgf.a ../utils/libutils.a
extract_fuseki_AM_CPPFLAGS = $(GNU_GO_WARNINGS) -I$(top_srcdir)/sgf

uncompress_fuseki_SOURCES = uncompress_fuseki.c
uncompress_fuseki_LDADD = ../utils/libutils.a ../engine/libboard.a ../sgf/libsgf.a
compress_fuseki_SOURCES = compress_fuseki.c

noinst_HEADERS = patterns.h eyes.h dfa.h dfa-mkpat.h

GGBUILTSOURCES = conn.c patterns.c apatterns.c dpatterns.c eyes.c\
                 influence.c barriers.c endgame.c aa_attackpat.c\
                 owl_attackpat.c\
		 owl_vital_apat.c owl_defendpat.c fusekipat.c\
                 fuseki9.c fuseki13.c fuseki19.c josekidb.c\
		 handipat.c oraclepat.c mcpat.c

DBBUILT = gogo.db hoshi_keima.db hoshi_other.db komoku.db sansan.db \
	  mokuhazushi.db takamoku.db

DBBUILT_INPUT = -i gogo.db -i hoshi_keima.db -i hoshi_other.db -i komoku.db \
               -i sansan.db -i mokuhazushi.db -i takamoku.db

MC_DB = $(srcdir)/mc_montegnu_classic.db $(srcdir)/mc_mogo_classic.db \
        $(srcdir)/mc_uniform.db

DB_TO_TAG = aa_attackpats.db attack.db barriers.db conn.db defense.db\
	    endgame.db eyes.db fuseki.db fuseki9.dbz fuseki13.dbz fuseki19.dbz\
	    handicap.db influence.db oracle.db owl_attackpats.db\
	    owl_defendpats.db owl_vital_apats.db patterns.db patterns2.db\
	    $(DBBUILT)

# Remove these files here... they are created locally
DISTCLEANFILES = $(GGBUILTSOURCES) $(DBBUILT) *~

dist-hook:
	cd $(distdir) && rm $(GGBUILTSOURCES)

# source files in this directory get access to private prototypes
AM_CPPFLAGS = \
	$(GNU_GO_WARNINGS) \
	-I$(top_srcdir)/engine \
	-I$(top_srcdir)/utils \
	-I$(top_srcdir)/sgf

noinst_LIBRARIES = libpatterns.a

libpatterns_a_SOURCES = connections.c helpers.c transform.c $(GGBUILTSOURCES)

gogo.db : $(srcdir)/gogo.sgf joseki$(EXEEXT)
	./joseki JG $(srcdir)/gogo.sgf >gogo.db

hoshi_keima.db : $(srcdir)/hoshi_keima.sgf joseki$(EXEEXT)
	./joseki JHK $(srcdir)/hoshi_keima.sgf >hoshi_keima.db

hoshi_other.db : $(srcdir)/hoshi_other.sgf joseki$(EXEEXT)
	./joseki JHO $(srcdir)/hoshi_other.sgf >hoshi_other.db

komoku.db : $(srcdir)/komoku.sgf joseki$(EXEEXT)
	./joseki JK $(srcdir)/komoku.sgf >komoku.db

sansan.db : $(srcdir)/sansan.sgf joseki$(EXEEXT)
	./joseki JS $(srcdir)/sansan.sgf >sansan.db

mokuhazushi.db : $(srcdir)/mokuhazushi.sgf joseki$(EXEEXT)
	./joseki JM $(srcdir)/mokuhazushi.sgf >mokuhazushi.db

takamoku.db : $(srcdir)/takamoku.sgf joseki$(EXEEXT)
	./joseki JT $(srcdir)/takamoku.sgf >takamoku.db

patterns.c : $(srcdir)/patterns.db $(srcdir)/patterns2.db mkpat$(EXEEXT)
	./mkpat -b pat -i $(srcdir)/patterns.db -i$(srcdir)/patterns2.db \
               -o patterns.c

josekidb.c : $(DBBUILT) mkpat$(EXEEXT)
	./mkpat -C joseki $(DBBUILT_INPUT) -o josekidb.c

apatterns.c : $(srcdir)/attack.db mkpat$(EXEEXT)
	./mkpat -X attpat -i $(srcdir)/attack.db -o apatterns.c

dpatterns.c : $(srcdir)/defense.db mkpat$(EXEEXT)
	./mkpat defpat -i $(srcdir)/defense.db -o dpatterns.c

conn.c : $(srcdir)/conn.db mkpat$(EXEEXT)
	./mkpat -c conn -i $(srcdir)/conn.db -o conn.c

endgame.c : $(srcdir)/endgame.db mkpat$(EXEEXT)
	./mkpat -b endpat -i $(srcdir)/endgame.db -o endgame.c

eyes.c: $(srcdir)/eyes.db mkeyes$(EXEEXT)
	./mkeyes < $(srcdir)/eyes.db >eyes.c

influence.c : $(srcdir)/influence.db mkpat$(EXEEXT)
	./mkpat -c influencepat -i $(srcdir)/influence.db -o influence.c

barriers.c : $(srcdir)/barriers.db mkpat$(EXEEXT)
	./mkpat -c -b barrierspat -i $(srcdir)/barriers.db -o barriers.c

aa_attackpat.c : $(srcdir)/aa_attackpats.db $(srcdir)/aa_attackpats.dtr mkpat$(EXEEXT)
	./mkpat $(DFAFLAGS) -b -t $(srcdir)/aa_attackpats.dtr aa_attackpat \
               -i $(srcdir)/aa_attackpats.db -o aa_attackpat.c

owl_attackpat.c : $(srcdir)/owl_attackpats.db $(srcdir)/owl_attackpats.dtr mkpat$(EXEEXT)
	./mkpat $(DFAFLAGS) -b -t $(srcdir)/owl_attackpats.dtr owl_attackpat \
               -i $(srcdir)/owl_attackpats.db -o owl_attackpat.c

oraclepat.c : $(srcdir)/oracle.db mkpat$(EXEEXT)
	./mkpat -b oracle -i $(srcdir)/oracle.db -o oraclepat.c

owl_vital_apat.c : $(srcdir)/owl_vital_apats.db $(srcdir)/owl_vital_apats.dtr mkpat$(EXEEXT)
	./mkpat $(DFAFLAGS) -b -t $(srcdir)/owl_vital_apats.dtr owl_vital_apat \
               -i $(srcdir)/owl_vital_apats.db -o owl_vital_apat.c

owl_defendpat.c : $(srcdir)/owl_defendpats.db $(srcdir)/owl_defendpats.dtr mkpat$(EXEEXT)
	./mkpat $(DFAFLAGS) -b -t $(srcdir)/owl_defendpats.dtr owl_defendpat \
               -i $(srcdir)/owl_defendpats.db -o owl_defendpat.c

fusekipat.c : $(srcdir)/fuseki.db mkpat$(EXEEXT)
	./mkpat -b fusekipat -i $(srcdir)/fuseki.db -o fusekipat.c

fuseki9.c : $(srcdir)/fuseki9.dbz uncompress_fuseki$(EXEEXT)
	./uncompress_fuseki 9 $(srcdir)/fuseki9.dbz c >fuseki9.c

fuseki13.c : $(srcdir)/fuseki13.dbz uncompress_fuseki$(EXEEXT)
	./uncompress_fuseki 13 $(srcdir)/fuseki13.dbz c >fuseki13.c

fuseki19.c : $(srcdir)/fuseki19.dbz uncompress_fuseki$(EXEEXT)
	./uncompress_fuseki 19 $(srcdir)/fuseki19.dbz c >fuseki19.c

handipat.c : $(srcdir)/handicap.db mkpat$(EXEEXT)
	./mkpat -b handipat -i $(srcdir)/handicap.db -o handipat.c

mcpat.c : $(MC_DB) mkmcpat$(EXEEXT)
	./mkmcpat $(MC_DB) > mcpat.c


ETAGS_ARGS = --language none --regex '/^Pattern[ \t]+[a-zA-Z0-9]+/' $(DB_TO_TAG)\
	     --language auto --no-regex
TAGS_DEPENDENCIES = $(DB_TO_TAG)
