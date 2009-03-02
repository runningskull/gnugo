## Process this file with automake to create Makefile.in
AUTOMAKE_OPTIONS=no-dependencies

info_TEXINFOS = gnugo.texi

gnugo_TEXINFOS = analyze.texi api.texi board.texi copying.texi dfa.texi \
                 dragon.texi eyes.texi gnugo.texi gtp.texi gtp-commands.texi \
                 influence.texi introduction.texi move_generation.texi \
                 moyo.texi overview.texi owl.texi patterns.texi \
                 reading.texi regression.texi sgf.texi using.texi \
                 utils.texi install.texi montecarlo.texi

man_MANS = gnugo.6

EPS = cdfa.eps dfa.eps dfa2.eps oldlogo.eps newlogo.eps \
	path.eps sync-prod1.eps sync-prod2.eps logo-34.eps \
	logo-32.eps logo-36.eps

FIG = cdfa.fig dfa.fig dfa2.fig path.fig sync-prod1.fig sync-prod2.fig

PNG = cdfa.png dfa.png dfa2.png path.png sync-prod1.png sync-prod2.png

JPG = cdfa.jpg dfa.jpg dfa2.jpg oldlogo.jpg path.jpg sync-prod1.jpg \
      sync-prod2.jpg newlogo.jpg logo-32.jpg logo-34.jpg logo-36.jpg

PDF = cdfa.pdf dfa2.pdf dfa.pdf logo-34.pdf path.pdf sync-prod1.pdf \
      sync-prod2.pdf logo-36.pdf

EXTRA_DIST = automake-1.4.patch gnugo.6 gnugo.pod gtp-commands.sed \
	$(EPS) $(FIG) $(PNG) $(JPG) $(PDF)

CLEANFILES = *.log *.dvi *.aux *.vr *.tp *.ky *.pg *.cps *.fns *.toc \
             *.fn *.cp *~

TEXI2PDF = texi2pdf
TEXI2HTML = texi2html

gnugo.6: $(srcdir)/gnugo.pod
	pod2man $(srcdir)/gnugo.pod --section 6 --release @VERSION@ > gnugo.6

BUILT_SOURCES = gtp-commands.texi

cmdsrc = $(srcdir)/../interface/play_gtp.c

gnugo.pdf: gnugo.texi $(gnugo_TEXINFOS)
	$(TEXI2PDF) $<

gnugo.html: gnugo.texi $(gnugo_TEXINFOS)
	$(TEXI2HTML) -split=chapter -nosec_nav -expand=tex $<

## Rebuild gtp-commands.texi from play_gtp.c:

gtp-commands:
	sed -e '1r '$(srcdir)/gtp-commands.sed				\
	    -e '/static struct gtp_command commands/,/^}/!d'		\
	    -e '/^ /!d' -e '/NULL/d'					\
	    -e 's/ *{"\(.*\)"[^a-z]*\(gtp_[a-z_]*\)}.*/s,\2,\1,g/'	\
	    $(cmdsrc) > TMP.sed
	sed -f TMP.sed $(cmdsrc) > $(srcdir)/gtp-commands.texi
	rm -f TMP.sed




