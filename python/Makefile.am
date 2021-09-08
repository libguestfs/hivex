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
	hivex/__init__.py \
	hivex/hive_types.py \
	hivex-py.c \
	t/*.py

if HAVE_PYTHON

pythondir = $(PYTHON_INSTALLDIR)
pythonmoddir = $(PYTHON_INSTALLDIR)/hivex

pythonmod_DATA = \
	hivex/hive_types.py \
	hivex/__init__.py

python_LTLIBRARIES = libhivexmod.la

libhivexmod_la_SOURCES = hivex-py.c
libhivexmod_la_CFLAGS = -Wall $(PYTHON_CFLAGS) \
			-I$(top_srcdir)/include
libhivexmod_la_LIBADD = $(top_builddir)/lib/libhivex.la
libhivexmod_la_LDFLAGS = -avoid-version -shared -module -shrext $(PYTHON_EXT_SUFFIX)

TESTS_ENVIRONMENT = ../run

TESTS = run-python-tests

# hivex.py got moved to hivex/__init.py
CLEANFILES = \
	hivex.py \
	hivex.pyc \
	hivex/hive_types.pyc \
	hivex/__init__.pyc

# For Python 3
clean-local:
	-rm -rf __pycache__/ hivex/__pycache__/

endif
