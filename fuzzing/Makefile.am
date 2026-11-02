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

# Build a persistent-mode AFL++ fuzzer.
# This is not built by default, please read 'FUZZING'.

EXTRA_DIST = \
	FUZZING

if ENABLE_FUZZING

CLEANFILES = *~

noinst_PROGRAMS = fuzz-hivex
fuzz_hivex_SOURCES = fuzz-hivex.c
fuzz_hivex_LDADD = ../lib/libhivex.la ../gnulib/lib/libgnu.la $(LIBXML2_LIBS)
fuzz_hivex_CFLAGS = \
  -I$(top_srcdir)/gnulib/lib \
  -I$(top_builddir)/gnulib/lib \
  -I$(top_srcdir)/lib \
  -I$(top_srcdir)/include \
  $(LIBXML2_CFLAGS) \
  $(WARN_CFLAGS) $(WERROR_CFLAGS)

endif ENABLE_FUZZING
