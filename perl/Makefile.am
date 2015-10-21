# hivex Perl bindings
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

# Old RHEL 5 autoconf doesn't have builddir or abs_srcdir.
builddir ?= $(top_builddir)/perl
abs_srcdir ?= $(shell unset CDPATH; cd $(srcdir) && pwd)

EXTRA_DIST = \
	Makefile.PL.in \
	run-perl-tests \
	lib/Win/Hivex.pm \
	lib/Win/Hivex/Regedit.pm \
	Hivex.xs \
	t/*.t \
	typemap

if HAVE_PERL

# Interfacing automake and ExtUtils::MakeMaker known to be
# a nightmare, news at 11.

# hivex source dependencies
.PHONY: src_deps
src_deps: $(top_builddir)/lib/libhivex.la

TESTS = run-perl-tests

$(TESTS): src_deps all

TESTS_ENVIRONMENT = \
  MAKE=$(MAKE) \
  ../run

INSTALLDIRS = site

all: Makefile-pl src_deps
	$(MAKE) -f Makefile-pl

Makefile-pl: Makefile.PL
	-[ $(srcdir) != $(builddir) ] && cp -rsu $(abs_srcdir)/. $(builddir)/.
	perl Makefile.PL INSTALLDIRS=$(INSTALLDIRS) PREFIX=$(prefix)

# No!  Otherwise it is deleted before the clean-local rule runs.
#CLEANFILES = Makefile-pl

clean-local:
	-$(MAKE) -f Makefile-pl clean
	rm -f Makefile-pl Makefile-pl.old

install-data-hook:
	$(MAKE) -f Makefile-pl DESTDIR=$(DESTDIR) install

endif
