## Process this file with automake to create Makefile.in
AUTOMAKE_OPTIONS=no-dependencies

info_TEXINFOS = gnugo.texi

gnugo_TEXINFOS = analyze.texi api.texi board.texi copying.texi dfa.texi \
                 dragon.texi eyes.texi gnugo.texi gtp.texi \
                 influence.texi introduction.texi move_generation.texi \
                 moyo.texi overview.texi owl.texi patterns.texi \
                 reading.texi regression.texi sgf.texi using.texi \
                 utils.texi install.texi incremental.texi

man_MANS = gnugo.6

EPS = cdfa.eps dfa.eps dfa2.eps oldlogo.eps newlogo.eps \
	path.eps sync-prod1.eps sync-prod2.eps

FIG = cdfa.fig dfa.fig dfa2.fig path.fig sync-prod1.fig sync-prod2.fig

PNG = cdfa.png dfa.png dfa2.png path.png sync-prod1.png sync-prod2.png

JPG = cdfa.jpg dfa.jpg dfa2.jpg oldlogo.jpg path.jpg sync-prod1.jpg \
      sync-prod2.jpg newlogo.jpg

EXTRA_DIST = automake-1.4.patch gnugo.6 gnugo.pod $(EPS) $(FIG) $(PNG) $(JPG)

CLEANFILES = *.log *.dvi *.aux *.vr *.tp *.ky *.pg *.cps *.fns *.toc \
             *.fn *.cp *~

gnugo.6: $(srcdir)/gnugo.pod
	pod2man $(srcdir)/gnugo.pod --section 6 --release @VERSION@ > gnugo.6

