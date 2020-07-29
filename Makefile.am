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

SUBDIRS = gnulib/lib generator lib images gnulib/tests

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

EXTRA_DIST = hivex.pc hivex.pc.in LICENSE README run.in \
	po/zanata-pull.sh po/zanata.xml

# Generate the ChangeLog automatically from the gitlog.
dist-hook:
	$(top_srcdir)/build-aux/gitlog-to-changelog > ChangeLog
	cp ChangeLog $(distdir)/ChangeLog

# Pkgconfig.
pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = hivex.pc

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

# Tag HEAD with current version (only for maintainer).

maintainer-tag:
	git tag -a v$(VERSION) -m "Version $(VERSION)" -f

CLEANFILES = $(HTMLFILES) pod2*.tmp *~
