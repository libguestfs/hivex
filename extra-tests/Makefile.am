# hivex
# Copyright (C) 2013-2021 Red Hat Inc.
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

EXTRA_DIST = README check-regedit.sh insert-keys.pl

# User should normally override this.
HIVEX_TEST_DATA ?= $(top_srcdir)/../hivex-test-data
export HIVEX_TEST_DATA

# Test hives.
test_hives_xz = $(wildcard $(HIVEX_TEST_DATA)/*.xz)
test_hives = $(test_hives_xz:$(HIVEX_TEST_DATA)/%.xz=%.hive)
check_DATA = $(test_hives)

%.hive: $(HIVEX_TEST_DATA)/%.xz
	rm -f $@ $@-t
	xz --uncompress --stdout $< > $@-t
	mv $@-t $@

TESTS_ENVIRONMENT = ../run
TESTS = check-regedit.sh insert-keys.pl

CLEANFILES = *~ *.hive
