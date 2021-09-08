# hivex
# Copyright (C) 2009-2010 Red Hat Inc.
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

EXTRA_DIST = \
	hivexget.pod \
	hivexget \
	hivexsh.pod \
	example1 \
	example2 \
	example3 \
	example4 \
	example5 \
	example6

bin_PROGRAMS = hivexsh
bin_SCRIPTS = hivexget
noinst_SCRIPTS = example1 example2 example3 example4 example5 example6

hivexsh_SOURCES = \
  hivexsh.c \
  ../include/hivex.h \
  ../lib/byte_conversions.h

hivexsh_LDADD = ../lib/libhivex.la ../gnulib/lib/libgnu.la $(LIBREADLINE)
hivexsh_CFLAGS = \
  -I$(top_srcdir)/gnulib/lib \
  -I$(top_builddir)/gnulib/lib \
  -I$(top_srcdir)/lib \
  -I$(top_srcdir)/include \
  -DLOCALEBASEDIR=\""$(datadir)/locale"\" \
  $(WARN_CFLAGS) $(WERROR_CFLAGS)

man_MANS = hivexget.1 hivexsh.1

hivexget.1: hivexget.pod
	$(POD2MAN) \
	  --section 1 \
	  -c "Windows Registry" \
	  --name "hivexget" \
	  --release "$(PACKAGE_NAME)-$(PACKAGE_VERSION)" \
	  $< > $@-t && mv $@-t $@

hivexsh.1: hivexsh.pod
	$(POD2MAN) \
	  --section 1 \
	  -c "Windows Registry" \
	  --name "hivexsh" \
	  --release "$(PACKAGE_NAME)-$(PACKAGE_VERSION)" \
	  $< > $@-t && mv $@-t $@

noinst_DATA = \
	$(top_builddir)/html/hivexget.1.html \
	$(top_builddir)/html/hivexsh.1.html

$(top_builddir)/html/hivexget.1.html: hivexget.pod
	mkdir -p $(top_builddir)/html
	cd $(top_builddir) && pod2html \
	  --css 'pod.css' \
	  --htmldir html \
	  --outfile html/hivexget.1.html \
	  $(abs_srcdir)/hivexget.pod

$(top_builddir)/html/hivexsh.1.html: hivexsh.pod
	mkdir -p $(top_builddir)/html
	cd $(top_builddir) && pod2html \
	  --css 'pod.css' \
	  --htmldir html \
	  --outfile html/hivexsh.1.html \
	  $(abs_srcdir)/hivexsh.pod

CLEANFILES = $(man_MANS)
