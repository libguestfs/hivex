# hivex
# Copyright (C) 2009-2011 Red Hat Inc.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

ACLOCAL_AMFLAGS = -I m4

# Work around broken libtool.
export to_tool_file_cmd=func_convert_file_noop

SUBDIRS = gnulib/lib generator lib images include

if HAVE_LIBXML2
SUBDIRS += xml
endif

SUBDIRS += po

if HAVE_HIVEXSH
SUBDIRS += sh
endif

if HAVE_OCAML
SUBDIRS += ocaml
endif

if HAVE_PERL
SUBDIRS += perl regedit
endif

if HAVE_PYTHON
SUBDIRS += python
endif

if HAVE_RUBY
SUBDIRS += ruby
endif

if ALWAYS_FALSE
SUBDIRS += extra-tests
endif

EXTRA_DIST = \
	.gitignore \
	FUZZING \
	LICENSE \
	README.md \
	SECURITY \
	cfg.mk \
	html/pod.css \
	m4/.gitignore \
	run.in \
	po/*.po \
	po/Makevars.template \
	po/zanata-pull.sh \
	po/zanata.xml \
	.x-sc_prohibit_magic_number_exit

# Maintainer website update.
HTMLFILES = \
	html/hivex.3.html \
	html/hivexget.1.html \
	html/hivexml.1.html \
	html/hivexregedit.1.html \
	html/hivexsh.1.html

WEBSITEDIR = $(HOME)/d/redhat/websites/libguestfs

website: $(HTMLFILES)
	cp $(HTMLFILES) $(WEBSITEDIR)

CLEANFILES = $(HTMLFILES) pod2*.tmp *~

#----------------------------------------------------------------------
# Maintainers only!

# Check no files are missing from EXTRA_DIST rules, and that all
# generated files have been included in the tarball.  (Note you must
# have done 'make dist')
maintainer-check-extra-dist:
	@zcat $(PACKAGE_NAME)-$(VERSION).tar.gz | tar tf - | sort | \
	  sed 's,^$(PACKAGE_NAME)-$(VERSION)/,,' > tarfiles
	@git ls-files | sort > gitfiles
	@comm -13 tarfiles gitfiles > comm-out
	@echo Checking for differences between EXTRA_DIST and git ...
	@cat comm-out
	@[ ! -s comm-out ]
	@rm tarfiles gitfiles comm-out
	@echo PASS: EXTRA_DIST tests

# Commit everything in the current directory and set the commit
# message to the current version number.
maintainer-commit:
	git commit -a -m "Version $(VERSION)."

# Tag HEAD with the current version.
maintainer-tag:
	git tag -a v$(VERSION) -m "Version $(VERSION)." -f
