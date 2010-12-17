## Process this file with automake to produce Makefile.in

SUBDIRS = utils sgf engine patterns interface doc regression

DISTCLEANFILES = *~

EXTRA_DIST = WINDOWS OSX CMakeLists.txt config.h.cmake stamp-h.in

maintainer-check:

# Tag before making distribution.  Also, don't make a distribution if
# checks fail.  Also, make sure the NEWS file is up-to-date.
cvs-dist: maintainer-check
	pkg=`echo "@PACKAGE@" | tr a-z A-Z`; \
	ver=`echo "@VERSION@" | sed 's/\./_/g'`; \
	tag="$$pkg-$$ver"; \
	echo tag=$$tag; \
	if cvs -n log -h README| grep -e $$tag > /dev/null; then \
	  echo "VERSION not new; not releasing" 1>&2; \
	  exit 1; \
	else :; \
	fi; \
	cvs tag -c $$tag
	$(MAKE) dist

dist-hook:
	cd $(distdir) \
	&& find . '(' -name '*.rej' -o -name '*.orig' ')' -exec rm '{}' ';'

vpathcheck: dist
	-rm -rf $(distdir)
	GZIP=$(GZIP_ENV) $(TAR) zxf $(distdir).tar.gz
	mkdir $(distdir)/=build
	mkdir $(distdir)/=inst
	dc_install_base=`cd $(distdir)/=inst && pwd`; \
	cd $(distdir)/=build \
	  && ../configure --srcdir=.. --prefix=$$dc_install_base \
	  && $(MAKE) $(AM_MAKEFLAGS) \
	  && $(MAKE) $(AM_MAKEFLAGS) dvi \
	  && $(MAKE) $(AM_MAKEFLAGS) check \
	  && $(MAKE) $(AM_MAKEFLAGS) install \
	  && $(MAKE) $(AM_MAKEFLAGS) installcheck
	-rm -rf $(distdir)
	@banner="Successful VPATH build for $(distdir).tar.gz"; \
	dashes=`echo "$$banner" | sed s/./=/g`; \
	echo "$$dashes"; \
	echo "$$banner"; \
	echo "$$dashes"

acdir = m4
@MAINT@aclocal-files: configure.in
@MAINT@	echo 'acfiles = \' > $@-tmp
@MAINT@	find $(acdir) -type f -name '*.m4' -print		\
@MAINT@	  | sed 's!^!$(top_srcdir)/!'				\
@MAINT@	  | tr '\012' ' '					\
@MAINT@	  >> $@-tmp
@MAINT@	echo >> $@-tmp
@MAINT@	mv $@-tmp $@

@MAINT@include aclocal-files

@MAINT@# Override default rule to use --acdir option
@MAINT@$(srcdir)/aclocal.m4: configure.in $(acfiles)
@MAINT@	cd $(srcdir) && aclocal --acdir=$(acdir)
