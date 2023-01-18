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

# Old RHEL 5 autoconf doesn't have builddir.
builddir ?= $(top_builddir)/images

EXTRA_DIST = \
	mkrlenvalue_test_hive.py \
	minimal \
	rlenvalue_test_hive \
	special

# 'large' is a large hive used for testing purposes.  It is generated
# by the mklarge C program, to avoid having to distribute this large
# binary blob.
noinst_PROGRAMS = mklarge
mklarge_SOURCES = mklarge.c
mklarge_CPPFLAGS = \
	-I$(top_srcdir)/gnulib/lib \
	-I$(top_srcdir)/include \
	-I$(top_builddir)/gnulib/lib \
	-I$(srcdir)
mklarge_CFLAGS = $(WARN_CFLAGS) $(WERROR_CFLAGS)
mklarge_LDADD = ../lib/libhivex.la

if IS_WINDOWS
noinst_PROGRAMS += mkzero
mkzero_SOURCES = mkzero.c
mkzero_CFLAGS = $(WARN_CFLAGS) $(WERROR_CFLAGS)
mkzero_LDADD = -lntdll
endif

noinst_DATA = large

large: mklarge$(EXEEXT)
	cmp -s $(srcdir)/minimal $(builddir)/minimal || \
	    cp $(srcdir)/minimal $(builddir)/minimal
	./mklarge$(EXEEXT) $(builddir)/minimal $(builddir)/large

CLEANFILES = $(noinst_DATA)
