# hivex OCaml bindings
# Copyright (C) 2009-2010, 2012 Red Hat Inc.
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
  -I$(top_srcdir)/include \
  $(WARN_CFLAGS) $(WERROR_CFLAGS)

if HAVE_OCAML

noinst_DATA = mlhivex.cma META

if HAVE_OCAMLOPT
noinst_DATA += mlhivex.cmxa
endif

OBJS = hivex_c.o hivex.cmo
XOBJS = $(OBJS:.cmo=.cmx)

mlhivex.cma: $(OBJS)
	$(OCAMLMKLIB) -o mlhivex $^ \
	    -ldopt '$(LDFLAGS)' \
	    -L$(top_builddir)/lib/.libs -lhivex

mlhivex.cmxa: $(XOBJS)
	$(OCAMLMKLIB) -o mlhivex $^ \
	    -ldopt '$(LDFLAGS)' \
	    -L$(top_builddir)/lib/.libs -lhivex

hivex_c.o: hivex_c.c
	$(CC) $(AM_CPPFLAGS) $(CFLAGS) -fPIC -Wall -c $<

TESTS_ENVIRONMENT = ../run

TESTS = \
	t/hivex_005_load \
	t/hivex_010_open \
	t/hivex_020_root \
	t/hivex_100_errors \
	t/hivex_110_gc_handle \
	t/hivex_120_rlenvalue \
	t/hivex_200_write \
	t/hivex_300_fold
noinst_DATA += $(TESTS)

# https://www.redhat.com/archives/libguestfs/2011-May/thread.html#00015
t/%: t/%.cmo mlhivex.cma
	$(LIBTOOL) --mode=execute -dlopen $(top_builddir)/lib/libhivex.la \
	  $(OCAMLFIND) ocamlc -dllpath $(abs_builddir) -package unix \
	  -linkpkg mlhivex.cma $< -o $@

.mli.cmi:
	$(OCAMLFIND) ocamlc -package unix -c $< -o $@
.ml.cmo:
	mkdir -p `dirname $@`
	$(OCAMLFIND) ocamlc -package unix -c $< -o $@
.ml.cmx:
	$(OCAMLFIND) ocamlopt -package unix -c $< -o $@

depend: .depend

.depend: $(wildcard *.mli) $(wildcard *.ml)
	rm -f $@ $@-t
	$(OCAMLFIND) ocamldep $^ | \
	  $(SED) 's/ *$$//' | \
	  $(SED) -e :a -e '/ *\\$$/N; s/ *\\\n */ /; ta' | \
	  $(SED) -e 's, :,:,g' | \
	  $(SED) -e 's,$(abs_srcdir)/,$(builddir)/,g' | \
	  sort > $@-t
	mv $@-t $@

include .depend

SUFFIXES = .cmo .cmi .cmx .ml .mli .mll .mly

# Do the installation by hand, because we want to run ocamlfind.
install_files = META *.so *.a *.cma *.cmi $(srcdir)/*.mli

if HAVE_OCAMLOPT
install_files += *.cmx *.cmxa
endif

install-data-hook:
	mkdir -p $(DESTDIR)$(OCAMLLIB)/stublibs
	$(OCAMLFIND) install \
	  -ldconf ignore -destdir $(DESTDIR)$(OCAMLLIB) \
	  $(PACKAGE_NAME) \
	  $(install_files)

CLEANFILES += $(noinst_DATA)

# This "tricks" ocamlfind into allowing us to compile other OCaml
# programs against a locally compiled copy of the hivex sources.
# ocamlfind needs to see a directory called ‘hivex’ which contains
# ‘META’.  The current directory is called ‘ocaml’, but if we make
# this symlink then we can create the required directory structure.
#
# Note if you just want to use this, make sure you use
# ‘../hivex/run make’ in your other program and everything should
# just work.
CLEANFILES += hivex

all-local: hivex

hivex:
	rm -f $@
	$(LN_S) . $@

endif HAVE_OCAML

# Tell version 3.79 and up of GNU make to not build goals in this
# directory in parallel.  (See RHBZ#502309).
.NOTPARALLEL:
