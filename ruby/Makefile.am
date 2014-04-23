# hivex Ruby bindings
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

EXTRA_DIST = \
	Rakefile.in \
	README.rdoc \
	doc/site/index.html \
	ext/hivex/extconf.rb \
	ext/hivex/_hivex.c \
	lib/hivex.rb \
	run-ruby-tests \
	tests/tc_*.rb \
	tests/test_helper.rb

CLEANFILES = \
	lib/*~ \
	tests/*~ \
	ext/hivex/*~ \
	ext/hivex/extconf.h \
	ext/hivex/_hivex.o \
	ext/hivex/_hivex.so \
	ext/hivex/mkmf.log \
	ext/hivex/Makefile

if HAVE_RUBY

TESTS = run-ruby-tests

TESTS_ENVIRONMENT = ../run

all:
	$(RAKE) build
	$(RAKE) rdoc

# Packagers can override this by doing: make INSTALLDIRS=vendor install
# INSTALLDIRS also affects where Perl bindings are installed.
INSTALLDIRS = site

RUBY_LIBDIR := $(shell $(RUBY) -rrbconfig -e "puts RbConfig::CONFIG['$(INSTALLDIRS)libdir']")
RUBY_ARCHDIR := $(shell $(RUBY) -rrbconfig -e "puts RbConfig::CONFIG['$(INSTALLDIRS)archdir']")

install:
	$(MKDIR_P) $(DESTDIR)$(RUBY_LIBDIR)
	$(MKDIR_P) $(DESTDIR)$(RUBY_ARCHDIR)
	$(INSTALL) -p -m 0644 $(srcdir)/lib/hivex.rb $(DESTDIR)$(RUBY_LIBDIR)
	$(INSTALL) -p -m 0755 $(builddir)/ext/hivex/_hivex.so $(DESTDIR)$(RUBY_ARCHDIR)

endif
