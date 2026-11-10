# hivex
# Copyright (C) 2009-2026 Red Hat Inc.
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

AM_CPPFLAGS = \
	-I$(top_srcdir)/gnulib/lib \
	-I$(top_srcdir)/include \
	-I$(top_builddir)/gnulib/lib \
	-I$(srcdir)

CLEANFILES = *~

LOG_COMPILER = $(top_builddir)/run

# Tests.

check_PROGRAMS = test-just-header

TESTS = test-just-header

test_just_header_SOURCES = test-just-header.c
test_just_header_CFLAGS = \
	-I$(top_srcdir)/include \
	$(WARN_CFLAGS) $(WERROR_CFLAGS)
test_just_header_LDADD = \
	$(top_builddir)/lib/libhivex.la
