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

ACLOCAL_AMFLAGS = -I m4

SUBDIRS = gnulib/lib hivex images gnulib/tests po

EXTRA_DIST = hivex.pc hivex.pc.in README LICENSE

# Generate the ChangeLog automatically from the gitlog.
dist-hook:
	$(top_srcdir)/build-aux/gitlog-to-changelog > ChangeLog
	cp ChangeLog $(distdir)/ChangeLog

# Update the list of translatable files in po/POTFILES.in.
all-local:
	cd $(srcdir); \
	find $(DIST_SUBDIRS) \
	    -name '*.c' -o -name '*.pl' -o -name '*.pm' -o \
	    -name 'virt-cat' -o \
	    -name 'virt-df' -o \
	    -name 'virt-edit' -o \
	    -name 'virt-inspector' -o \
	    -name 'virt-ls' -o \
	    -name 'virt-rescue' -o \
	    -name 'virt-tar' | \
	grep -v '^gnulib/' | \
	LC_ALL=C sort | \
	sed 's,^\./,,' > po/POTFILES.in

# Pkgconfig.
pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = hivex.pc
