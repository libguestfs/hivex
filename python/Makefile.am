# hivex Python bindings
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
builddir ?= $(top_builddir)/python

EXTRA_DIST = \
	run-python-tests \
	hivex.py \
	hivex-py.c \
	t/*.py

if HAVE_PYTHON

pythondir = $(PYTHON_INSTALLDIR)

python_DATA = hivex.py

python_LTLIBRARIES = libhivexmod.la

libhivexmod_la_SOURCES = hivex-py.c
libhivexmod_la_CFLAGS = -Wall $(PYTHON_CFLAGS) \
		        -I$(top_srcdir)/lib -I$(top_builddir)/lib
libhivexmod_la_LIBADD = $(top_builddir)/lib/libhivex.la
libhivexmod_la_LDFLAGS = -avoid-version -shared -module -shrext $(PYTHON_EXT_SUFFIX)

TESTS_ENVIRONMENT = \
	PYTHONPATH=$(builddir):$(builddir)/.libs

TESTS = run-python-tests

CLEANFILES = hivex.pyc

endif
