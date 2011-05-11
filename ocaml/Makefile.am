# hivex OCaml bindings
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

EXTRA_DIST = \
	.depend META.in \
	hivex.mli hivex.ml \
	hivex_c.c \
	t/*.ml

CLEANFILES = *.cmi *.cmo *.cmx *.cma *.cmxa *.o *.a *.so
CLEANFILES += t/*.cmi t/*.cmo t/*.cmx t/*.o t/*.a t/*.so

AM_CPPFLAGS = \
  -I$(top_builddir) -I$(OCAMLLIB) -I$(top_srcdir)/ocaml \
  -I$(top_srcdir)/lib \
  $(WARN_CFLAGS) $(WERROR_CFLAGS)

if HAVE_OCAML

noinst_DATA = mlhivex.cma META

if HAVE_OCAMLOPT
noinst_DATA += mlhivex.cmxa
endif

OBJS = hivex_c.o hivex.cmo
XOBJS = $(OBJS:.cmo=.cmx)

mlhivex.cma: $(OBJS)
	$(OCAMLMKLIB) -o mlhivex $^ -L$(top_builddir)/lib/.libs -lhivex

mlhivex.cmxa: $(XOBJS)
	$(OCAMLMKLIB) -o mlhivex $^ -L$(top_builddir)/lib/.libs -lhivex

hivex_c.o: hivex_c.c
	$(CC) $(AM_CPPFLAGS) $(CFLAGS) -fPIC -Wall -c $<

TESTS_ENVIRONMENT = \
	LD_LIBRARY_PATH=$(top_builddir)/lib/.libs:$(top_builddir)/ocaml \
	$(VG)

TESTS = \
	t/hivex_005_load \
	t/hivex_010_open \
	t/hivex_020_root \
	t/hivex_100_errors \
	t/hivex_110_gc_handle \
	t/hivex_200_write \
	t/hivex_300_fold
noinst_DATA += $(TESTS)

t/hivex_005_load: t/hivex_005_load.cmo mlhivex.cma
	mkdir -p t
	$(OCAMLFIND) ocamlc -cclib -L$(top_builddir)/lib/.libs -I . -package unix -linkpkg mlhivex.cma $< -o $@

t/hivex_010_open: t/hivex_010_open.cmo mlhivex.cma
	mkdir -p t
	$(OCAMLFIND) ocamlc -cclib -L$(top_builddir)/lib/.libs -I . -package unix -linkpkg mlhivex.cma $< -o $@

t/hivex_020_root: t/hivex_020_root.cmo mlhivex.cma
	mkdir -p t
	$(OCAMLFIND) ocamlc -cclib -L$(top_builddir)/lib/.libs -I . -package unix -linkpkg mlhivex.cma $< -o $@

t/hivex_100_errors: t/hivex_100_errors.cmo mlhivex.cma
	mkdir -p t
	$(OCAMLFIND) ocamlc -cclib -L$(top_builddir)/lib/.libs -I . -package unix -linkpkg mlhivex.cma $< -o $@

t/hivex_110_gc_handle: t/hivex_110_gc_handle.cmo mlhivex.cma
	mkdir -p t
	$(OCAMLFIND) ocamlc -cclib -L$(top_builddir)/lib/.libs -I . -package unix -linkpkg mlhivex.cma $< -o $@

t/hivex_200_write: t/hivex_200_write.cmo mlhivex.cma
	mkdir -p t
	$(OCAMLFIND) ocamlc -cclib -L$(top_builddir)/lib/.libs -I . -package unix -linkpkg mlhivex.cma $< -o $@

t/hivex_300_fold: t/hivex_300_fold.cmo mlhivex.cma
	mkdir -p t
	$(OCAMLFIND) ocamlc -cclib -L$(top_builddir)/lib/.libs -I . -package unix -linkpkg mlhivex.cma $< -o $@

# Need to rebuild the tests from source if the main library has
# changed at all, otherwise we get inconsistent assumptions.
t/%.cmx: t/%.ml mlhivex.cmxa
	$(OCAMLFIND) ocamlopt -package unix -linkpkg -c $< -o $@

.mli.cmi:
	$(OCAMLFIND) ocamlc -package unix -c $< -o $@
.ml.cmo:
	$(OCAMLFIND) ocamlc -package unix -c $< -o $@
.ml.cmx:
	$(OCAMLFIND) ocamlopt -package unix -c $< -o $@

depend: .depend

.depend: $(wildcard *.mli) $(wildcard *.ml)
	rm -f $@ $@-t
	$(OCAMLFIND) ocamldep $^ | sed 's/  *$$//' | sort > $@-t
	mv $@-t $@

include .depend

SUFFIXES = .cmo .cmi .cmx .ml .mli .mll .mly

# Do the installation by hand, because we want to run ocamlfind.
install-data-hook:
	mkdir -p $(DESTDIR)$(OCAMLLIB)
	mkdir -p $(DESTDIR)$(OCAMLLIB)/stublibs
	$(OCAMLFIND) install \
	  -ldconf ignore -destdir $(DESTDIR)$(OCAMLLIB) \
	  hivex \
	  META *.so *.a *.cma *.cmx *.cmxa *.cmi *.mli

CLEANFILES += $(noinst_DATA)

endif

# Tell version 3.79 and up of GNU make to not build goals in this
# directory in parallel.  (See RHBZ#502309).
.NOTPARALLEL:
