noinst_PROGRAMS = mkpat joseki mkeyes extract_fuseki
EXTRA_PROGRAMS = transpat

DSP = dfa.dsp patterns.dsp joseki.dsp mkeyes.dsp mkpat.dsp fuseki.dsp

EXTRA_DIST = $(DSP)\
	dfa.c\
	hoshi.sgf\
	komoku.sgf\
	mokuhazushi.sgf\
	sansan.sgf\
	takamoku.sgf\
	patterns.db\
	patterns2.db\
	attack.db\
	defense.db\
	endgame.db\
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
	fuseki19.db

mkpat_SOURCES  = mkpat.c transform.c

mkpat_LDADD = ./libdfa.a ../utils/libutils.a

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
                       ../sgf/libsgf.a ../utils/libutils.a\
		       libdfa.a
extract_fuseki_INCLUDES = -I$(top_srcdir)/sgf


noinst_HEADERS = patterns.h eyes.h patlib.h dfa.h

GGBUILTSOURCES = conn.c patterns.c apatterns.c dpatterns.c eyes.c\
                 influence.c barriers.c endgame.c owl_attackpat.c\
		 owl_vital_apat.c owl_defendpat.c fusekipat.c\
                 fuseki9.c fuseki13.c fuseki19.c josekidb.c

DBBUILT = hoshi.db komoku.db sansan.db mokuhazushi.db takamoku.db

# Remove these files here... they are created locally
DISTCLEANFILES = $(GGBUILTSOURCES) $(DBBUILT) *~

dist-hook:
	cd $(distdir) && rm $(GGBUILTSOURCES)

# source files in this directory get access to private prototypes
INCLUDES = -I$(top_srcdir)/engine -I$(top_srcdir)/utils -I$(top_srcdir)/sgf

noinst_LIBRARIES = libpatterns.a libdfa.a

libpatterns_a_SOURCES = connections.c helpers.c $(GGBUILTSOURCES)
libdfa_a_SOURCES = dfa.c transform.c

hoshi.db : $(srcdir)/hoshi.sgf joseki$(EXEEXT)
	./joseki JH $(srcdir)/hoshi.sgf >hoshi.db

komoku.db : $(srcdir)/komoku.sgf joseki$(EXEEXT)
	./joseki JK $(srcdir)/komoku.sgf >komoku.db

sansan.db : $(srcdir)/sansan.sgf joseki$(EXEEXT)
	./joseki JS $(srcdir)/sansan.sgf >sansan.db

mokuhazushi.db : $(srcdir)/mokuhazushi.sgf joseki$(EXEEXT)
	./joseki JM $(srcdir)/mokuhazushi.sgf >mokuhazushi.db

takamoku.db : $(srcdir)/takamoku.sgf joseki$(EXEEXT)
	./joseki JT $(srcdir)/takamoku.sgf >takamoku.db

patterns.c : $(srcdir)/patterns.db $(srcdir)/patterns2.db mkpat$(EXEEXT)
	cat  $(srcdir)/patterns.db $(srcdir)/patterns2.db \
		| ./mkpat -b pat >patterns.c

josekidb.c : $(DBBUILT) mkpat$(EXEEXT)
	cat  $(DBBUILT) | ./mkpat -b joseki >josekidb.c

apatterns.c : $(srcdir)/attack.db mkpat$(EXEEXT)
	cat $(srcdir)/attack.db | ./mkpat -X attpat >apatterns.c

dpatterns.c : $(srcdir)/defense.db mkpat$(EXEEXT)
	cat $(srcdir)/defense.db | ./mkpat defpat >dpatterns.c

conn.c : $(srcdir)/conn.db mkpat$(EXEEXT)
	./mkpat -c conn < $(srcdir)/conn.db >conn.c

endgame.c : $(srcdir)/endgame.db mkpat$(EXEEXT)
	cat $(srcdir)/endgame.db | ./mkpat -b endpat >endgame.c

eyes.c: $(srcdir)/eyes.db mkeyes$(EXEEXT)
	./mkeyes < $(srcdir)/eyes.db >eyes.c

influence.c : $(srcdir)/influence.db mkpat$(EXEEXT)
	./mkpat -c influencepat < $(srcdir)/influence.db >influence.c

barriers.c : $(srcdir)/barriers.db mkpat$(EXEEXT)
	./mkpat -c -b barrierspat < $(srcdir)/barriers.db >barriers.c

owl_attackpat.c : $(srcdir)/owl_attackpats.db mkpat$(EXEEXT)
	./mkpat $(DFAFLAGS) -b owl_attackpat < $(srcdir)/owl_attackpats.db >owl_attackpat.c

owl_vital_apat.c : $(srcdir)/owl_vital_apats.db mkpat$(EXEEXT)
	./mkpat $(DFAFLAGS) -b owl_vital_apat < $(srcdir)/owl_vital_apats.db >owl_vital_apat.c

owl_defendpat.c : $(srcdir)/owl_defendpats.db mkpat$(EXEEXT)
	./mkpat $(DFAFLAGS) -b owl_defendpat < $(srcdir)/owl_defendpats.db >owl_defendpat.c

fusekipat.c : $(srcdir)/fuseki.db mkpat$(EXEEXT)
	./mkpat -b fusekipat < $(srcdir)/fuseki.db >fusekipat.c

fuseki9.c : $(srcdir)/fuseki9.db mkpat$(EXEEXT)
	./mkpat -b -f fuseki9 < $(srcdir)/fuseki9.db >fuseki9.c

fuseki13.c : $(srcdir)/fuseki13.db mkpat$(EXEEXT)
	./mkpat -b -f fuseki13 < $(srcdir)/fuseki13.db >fuseki13.c

fuseki19.c : $(srcdir)/fuseki19.db mkpat$(EXEEXT)
	./mkpat -b -f fuseki19 < $(srcdir)/fuseki19.db >fuseki19.c
