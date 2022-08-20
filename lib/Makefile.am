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

SUBDIRS = tools

EXTRA_DIST = \
	hivex.pc.in \
	hivex.pod \
	hivex.syms \
	local/hivex.pc.in

AM_CPPFLAGS = \
	-I$(top_srcdir)/gnulib/lib \
	-I$(top_srcdir)/include \
	-I$(top_builddir)/gnulib/lib \
	-I$(srcdir)

lib_LTLIBRARIES = libhivex.la

libhivex_la_SOURCES = \
	../include/hivex.h \
	byte_conversions.h \
	gettext.h \
	handle.c \
	hivex-internal.h \
	mmap.h \
	node.c \
	offset-list.c \
	utf16.c \
	util.c \
	value.c \
	visit.c \
	write.c

libhivex_la_LIBADD = ../gnulib/lib/libgnu.la $(LTLIBOBJS)
libhivex_la_LDFLAGS = \
	-pthread \
	-version-info 0:0:0 \
	-no-undefined \
	$(VERSION_SCRIPT_FLAGS)$(srcdir)/hivex.syms \
	$(LTLIBICONV) \
	$(LTLIBINTL) \
	$(LTLIBTHREAD)
libhivex_la_CFLAGS = $(WARN_CFLAGS) $(WERROR_CFLAGS) -pthread

# Pkgconfig.

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = hivex.pc

# Manual page.

man_MANS = hivex.3

hivex.3: hivex.pod
	$(POD2MAN) \
	  --section 3 \
	  -c "Windows Registry" \
	  --name "hivex" \
	  --release "$(PACKAGE_NAME)-$(PACKAGE_VERSION)" \
	  $< > $@-t && mv $@-t $@

noinst_DATA = \
	$(top_builddir)/html/hivex.3.html

$(top_builddir)/html/hivex.3.html: hivex.pod
	mkdir -p $(top_builddir)/html
	pod2html \
	  --css pod.css \
	  --htmldir $(top_builddir)/html \
	  --outfile $(top_builddir)/html/hivex.3.html \
	  $<

CLEANFILES = $(man_MANS) *~

# Tests.

check_PROGRAMS = test-just-header

TESTS = test-just-header

test_just_header_SOURCES = test-just-header.c
test_just_header_CFLAGS = \
	-I$(top_srcdir)/include \
	$(WARN_CFLAGS) $(WERROR_CFLAGS)
test_just_header_LDADD = \
	$(top_builddir)/lib/libhivex.la
