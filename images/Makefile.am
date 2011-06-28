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

EXTRA_DIST = minimal

# 'large' is a large hive used for testing purposes.  It is generated
# by the mklarge C program, to avoid having to distribute this large
# binary blob.
noinst_PROGRAMS = mklarge
mklarge_SOURCES = mklarge.c
mklarge_CFLAGS = \
	-I$(srcdir)/../lib \
	$(WARN_CFLAGS) $(WERROR_CFLAGS)
mklarge_LDADD = ../lib/libhivex.la

noinst_DATA = large

large: mklarge
	./mklarge $(srcdir)/minimal large

CLEANFILES = $(noinst_DATA)
