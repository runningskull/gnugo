noinst_PROGRAMS = mkpat joseki mkeyes
EXTRA_PROGRAMS = extract_fuseki transpat

DSP = dfa.dsp patterns.dsp joseki.dsp mkeyes.dsp mkpat.dsp fuseki.dsp
DTR = aa_attackpats.dtr owl_attackpats.dtr owl_defendpats.dtr \
       owl_vital_apats.dtr

EXTRA_DIST = $(DSP)\
        $(DTR)\
	dfa.c\
	gnugo-db.el\
	hoshi_keima.sgf\
	hoshi_other.sgf\
	komoku.sgf\
	mokuhazushi.sgf\
	sansan.sgf\
	takamoku.sgf\
	patterns.db\
	patterns2.db\
	attack.db\
	defense.db\
	endgame.db\
	aa_attackpats.db\
	owl_attackpats.db\
	owl_vital_apats.db\
	owl_defendpats.db\
	eyes.db\
	conn.db\
	influence.db\
	barriers.db\
	fuseki.db\
	fuseki9.db\
	fuseki13.db\
	fuseki19.db\
	handicap.db\
	read_attack.db\
	read_defend.db

mkpat_SOURCES  = mkpat.c transform.c dfa.c

mkpat_LDADD = ../utils/libutils.a

if DFA_ENABLED
DFAFLAGS = -D -m
else
DFAFLAGS =
endif

transpat_SOURCES = transpat.c patlib.c transform.c
transpat_LDADD = 
joseki_SOURCES = joseki.c 
joseki_LDADD = ../engine/libboard.a ../sgf/libsgf.a ../utils/libutils.a 
joseki_INCLUDES = -I$(top_srcdir)/sgf
mkeyes_SOURCES = mkeyes.c
extract_fuseki_SOURCES  = extract_fuseki.c
# Yes, we currently need duplicate libengine.a and libpatterns.a.
extract_fuseki_LDADD = ../engine/libengine.a libpatterns.a\
		       ../engine/libengine.a libpatterns.a\
                       ../sgf/libsgf.a ../utils/libutils.a
extract_fuseki_INCLUDES = -I$(top_srcdir)/sgf


noinst_HEADERS = patterns.h eyes.h patlib.h dfa.h dfa-mkpat.h

GGBUILTSOURCES = conn.c patterns.c apatterns.c dpatterns.c eyes.c\
                 influence.c barriers.c endgame.c aa_attackpat.c\
                 owl_attackpat.c\
		 owl_vital_apat.c owl_defendpat.c fusekipat.c\
                 fuseki9.c fuseki13.c fuseki19.c josekidb.c\
		 read_attack.c read_defend.c handipat.c

DBBUILT = hoshi_keima.db hoshi_other.db komoku.db sansan.db \
	  mokuhazushi.db takamoku.db

DBBUILT_INPUT = -i hoshi_keima.db -i hoshi_other.db -i komoku.db \
               -i sansan.db -i mokuhazushi.db -i takamoku.db

DB_TO_TAG = aa_attackpats.db attack.db barriers.db conn.db defense.db\
	    endgame.db eyes.db fuseki.db fuseki9.db fuseki13.db fuseki19.db\
	    handicap.db influence.db owl_attackpats.db\
	    owl_defendpats.db owl_vital_apats.db patterns.db patterns2.db\
	    read_attack.db read_defend.db $(DBBUILT)

# Remove these files here... they are created locally
DISTCLEANFILES = $(GGBUILTSOURCES) $(DBBUILT) *~

dist-hook:
	cd $(distdir) && rm $(GGBUILTSOURCES)

# source files in this directory get access to private prototypes
INCLUDES = -I$(top_srcdir)/engine -I$(top_srcdir)/utils -I$(top_srcdir)/sgf

noinst_LIBRARIES = libpatterns.a

libpatterns_a_SOURCES = connections.c helpers.c transform.c $(GGBUILTSOURCES)

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

owl_vital_apat.c : $(srcdir)/owl_vital_apats.db $(srcdir)/owl_vital_apats.dtr mkpat$(EXEEXT)
	./mkpat $(DFAFLAGS) -b -t $(srcdir)/owl_vital_apats.dtr owl_vital_apat \
               -i $(srcdir)/owl_vital_apats.db -o owl_vital_apat.c

owl_defendpat.c : $(srcdir)/owl_defendpats.db $(srcdir)/owl_defendpats.dtr mkpat$(EXEEXT)
	./mkpat $(DFAFLAGS) -b -t $(srcdir)/owl_defendpats.dtr owl_defendpat \
               -i $(srcdir)/owl_defendpats.db -o owl_defendpat.c

fusekipat.c : $(srcdir)/fuseki.db mkpat$(EXEEXT)
	./mkpat -b fusekipat -i $(srcdir)/fuseki.db -o fusekipat.c

fuseki9.c : $(srcdir)/fuseki9.db mkpat$(EXEEXT)
	./mkpat -b -f fuseki9 -i $(srcdir)/fuseki9.db -o fuseki9.c

fuseki13.c : $(srcdir)/fuseki13.db mkpat$(EXEEXT)
	./mkpat -b -f fuseki13 -i $(srcdir)/fuseki13.db -o fuseki13.c

fuseki19.c : $(srcdir)/fuseki19.db mkpat$(EXEEXT)
	./mkpat -b -f fuseki19 -i $(srcdir)/fuseki19.db -o fuseki19.c

handipat.c : $(srcdir)/handicap.db mkpat$(EXEEXT)
	./mkpat -b handipat -i $(srcdir)/handicap.db -o handipat.c

read_attack.c : $(srcdir)/read_attack.db mkpat$(EXEEXT)
	./mkpat -b read_attack -i $(srcdir)/read_attack.db -o read_attack.c

read_defend.c : $(srcdir)/read_defend.db mkpat$(EXEEXT)
	./mkpat -b read_defend -i $(srcdir)/read_defend.db -o read_defend.c


ETAGS_ARGS = --language none --regex '/^Pattern[ \t]+[a-zA-Z0-9]+/' $(DB_TO_TAG)\
	     --language auto --no-regex
TAGS_DEPENDENCIES = $(DB_TO_TAG)
