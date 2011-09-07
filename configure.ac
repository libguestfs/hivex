# hivex
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

# major/minor/release must be numbers
m4_define([hivex_major],   [1])
m4_define([hivex_minor],   [3])
m4_define([hivex_release], [1])
# extra can be any string
m4_define([hivex_extra],   [])

AC_INIT([hivex],hivex_major.hivex_minor.hivex_release[]hivex_extra)
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([foreign])

m4_ifndef([AM_SILENT_RULES], [m4_define([AM_SILENT_RULES],[])])
AM_SILENT_RULES([yes]) # make --enable-silent-rules the default.

AC_CONFIG_MACRO_DIR([m4])

dnl Split up the version string.
AC_DEFINE([PACKAGE_VERSION_MAJOR],[hivex_major],[Major version number])
AC_DEFINE([PACKAGE_VERSION_MINOR],[hivex_minor],[Minor version number])
AC_DEFINE([PACKAGE_VERSION_RELEASE],[hivex_release],[Release number])
AC_DEFINE([PACKAGE_VERSION_EXTRA],["hivex_extra"],[Extra version string])

gl_EARLY
gl_INIT

AM_PROG_LIBTOOL

dnl Check for basic C environment.
AC_PROG_CC_STDC
AC_PROG_INSTALL
AC_PROG_CPP

AC_ARG_ENABLE([gcc-warnings],
  [AS_HELP_STRING([--enable-gcc-warnings],
                  [turn on lots of GCC warnings (for developers)])],
  [case $enableval in
     yes|no) ;;
     *)      AC_MSG_ERROR([bad value $enableval for gcc-warnings option]) ;;
   esac
   gl_gcc_warnings=$enableval],
  [gl_gcc_warnings=no]
)

if test "$gl_gcc_warnings" = yes; then
  gl_WARN_ADD([-Werror], [WERROR_CFLAGS])
  AC_SUBST([WERROR_CFLAGS])

  nw=
  # This, $nw, is the list of warnings we disable.
  nw="$nw -Wdeclaration-after-statement" # too useful to forbid
  nw="$nw -Waggregate-return"       # anachronistic
  nw="$nw -Wc++-compat"             # We don't care about C++ compilers
  nw="$nw -Wundef"                  # Warns on '#if GNULIB_FOO' etc in gnulib
  nw="$nw -Wtraditional"            # Warns on #elif which we use often
  nw="$nw -Wcast-qual"              # Too many warnings for now
  nw="$nw -Wconversion"             # Too many warnings for now
  nw="$nw -Wsystem-headers"         # Don't let system headers trigger warnings
  nw="$nw -Wsign-conversion"        # Too many warnings for now
  nw="$nw -Wtraditional-conversion" # Too many warnings for now
  nw="$nw -Wunreachable-code"       # Too many warnings for now
  nw="$nw -Wpadded"                 # Our structs are not padded
  nw="$nw -Wredundant-decls"        # openat.h declares e.g., mkdirat
  nw="$nw -Wlogical-op"             # any use of fwrite provokes this
  nw="$nw -Wvla"                    # two warnings in mount.c
  # things I might fix soon:
  nw="$nw -Wmissing-format-attribute" # daemon.h's asprintf_nowarn
  nw="$nw -Winline"                 # daemon.h's asprintf_nowarn
  nw="$nw -Wshadow"                 # numerous, plus we're not unanimous
  # ?? -Wstrict-overflow
  nw="$nw -Wunsafe-loop-optimizations" # just a warning that an optimization
                                    # was not possible, safe to ignore
  nw="$nw -Wpacked"                 # Allow attribute((packed)) on structs
  nw="$nw -Wlong-long"              # Allow long long since it's required
                                    # by xstrtoll.

  gl_MANYWARN_ALL_GCC([ws])
  gl_MANYWARN_COMPLEMENT([ws], [$ws], [$nw])
  for w in $ws; do
    gl_WARN_ADD([$w])
  done
  gl_WARN_ADD([-Wno-unused-parameter]) # stubs.c
  gl_WARN_ADD([-Wno-jump-misses-init]) # stubs.c
  gl_WARN_ADD([-Wno-unused-variable]) # FIXME: only temporary, for guestfs_protocol.c, etc

  # In spite of excluding -Wlogical-op above, it is enabled, as of
  # gcc 4.5.0 20090517, and it provokes warnings in cat.c, dd.c, truncate.c
  gl_WARN_ADD([-Wno-logical-op])

  gl_WARN_ADD([-fdiagnostics-show-option])

  AC_SUBST([WARN_CFLAGS])

  AC_DEFINE([lint], [1], [Define to 1 if the compiler is checking for lint.])
  AC_DEFINE([_FORTIFY_SOURCE], [2],
    [enable compile-time and run-time bounds-checking, and some warnings])
  AC_DEFINE([GNULIB_PORTCHECK], [1], [enable some gnulib portability checks])
fi

AC_C_PROTOTYPES
test "x$U" != "x" && AC_MSG_ERROR([Compiler not ANSI compliant])

AM_PROG_CC_C_O

dnl Work out how to specify the linker script to the linker.
VERSION_SCRIPT_FLAGS=-Wl,--version-script=
`/usr/bin/ld --help 2>&1 | grep -- --version-script >/dev/null` || \
    VERSION_SCRIPT_FLAGS="-Wl,-M -Wl,"
AC_SUBST(VERSION_SCRIPT_FLAGS)

dnl Check support for 64 bit file offsets.
AC_SYS_LARGEFILE

dnl Check sizeof long.
AC_CHECK_SIZEOF([long])

dnl Headers.
AC_CHECK_HEADERS([byteswap.h endian.h libintl.h])

dnl Check for mmap
AC_REPLACE_FUNCS([mmap])

dnl Functions.
AC_CHECK_FUNCS([bindtextdomain])

dnl Check for pod2man and pod2text.
AC_CHECK_PROG([POD2MAN],[pod2man],[pod2man],[no])
test "x$POD2MAN" = "xno" &&
     AC_MSG_ERROR([pod2man must be installed])
AC_CHECK_PROG([POD2TEXT],[pod2text],[pod2text],[no])
test "x$POD2TEXT" = "xno" &&
     AC_MSG_ERROR([pod2text must be installed])

dnl Readline.
AC_ARG_WITH([readline],
    [AS_HELP_STRING([--with-readline],
        [support fancy command line editing @<:@default=check@:>@])],
    [],
    [with_readline=check])

LIBREADLINE=
AS_IF([test "x$with_readline" != xno],
    [AC_CHECK_LIB([readline], [main],
        [AC_SUBST([LIBREADLINE], ["-lreadline"])
         AC_DEFINE([HAVE_LIBREADLINE], [1],
                   [Define if you have libreadline])
        ],
        [if test "x$with_readline" != xcheck; then
         AC_MSG_FAILURE(
             [--with-readline was given, but test for readline failed])
         fi
        ])])

dnl For i18n.
AM_GNU_GETTEXT([external])
AM_GNU_GETTEXT_VERSION([0.17])

dnl libxml2.
PKG_CHECK_MODULES([LIBXML2], [libxml-2.0])
AC_SUBST([LIBXML2_CFLAGS])
AC_SUBST([LIBXML2_LIBS])

dnl Check for OCaml (optional, for OCaml bindings).
AC_PROG_OCAML
AC_PROG_FINDLIB
AM_CONDITIONAL([HAVE_OCAML],
    [test "x$OCAMLC" != "xno" && test "x$OCAMLFIND" != "xno"])
AM_CONDITIONAL([HAVE_OCAMLOPT],
    [test "x$OCAMLOPT" != "xno" && test "x$OCAMLFIND" != "xno"])

if test "x$OCAMLC" != "xno"; then
    dnl Check if we have caml/unixsupport.h header (OCaml bindings only).
    old_CFLAGS="$CFLAGS"
    CFLAGS="$CFLAGS -I$OCAMLLIB"
    AC_CHECK_HEADERS([caml/unixsupport.h],[],[],
    [
    #include <caml/config.h>
    #include <caml/mlvalues.h>
    ])
    CFLAGS="$old_CFLAGS"

    dnl Do we have function caml_raise_with_args?
    f=caml_raise_with_args
    AC_MSG_CHECKING([for function $f])
    echo "char $f (); char foo() { return $f (); }" > conftest.c
    rm -f conftest_ml.ml
    touch conftest_ml.ml
    if $OCAMLC -c conftest.c 2>/dev/null && \
       $OCAMLC -c conftest_ml.ml 2>/dev/null && \
       $OCAMLC conftest.o conftest_ml.cmo -o conftest 2>/dev/null ; then
        AC_DEFINE([HAVE_CAML_RAISE_WITH_ARGS],[1],
                  [Defined if function caml_raise_with_args exists.])
        AC_MSG_RESULT([found])
    else
        AC_MSG_RESULT([not found])
    fi
    rm -f conftest conftest.* conftest_ml.*
fi

dnl Check for Perl (optional, for Perl bindings).
dnl XXX This isn't quite right, we should check for Perl devel library.
AC_CHECK_PROG([PERL],[perl],[perl],[no])

dnl Check for Perl modules that must be present to compile and
dnl test the Perl bindings.
missing_perl_modules=no
for pm in Test::More ExtUtils::MakeMaker IO::Stringy; do
    AC_MSG_CHECKING([for $pm])
    if ! perl -M$pm -e1 >/dev/null 2>&1; then
        AC_MSG_RESULT([no])
        missing_perl_modules=yes
    else
        AC_MSG_RESULT([yes])
    fi
done
if test "x$missing_perl_modules" = "xyes"; then
    AC_MSG_WARN([some Perl modules required to compile or test the Perl bindings are missing])
fi

AM_CONDITIONAL([HAVE_PERL],
    [test "x$PERL" != "xno" && test "x$missing_perl_modules" != "xyes"])

dnl Check for Python (optional, for Python bindings).
AC_CHECK_PROG([PYTHON],[python],[python],[no])

PYTHON_PREFIX=
PYTHON_VERSION=

if test "x$PYTHON" != "xno"; then
    PYTHON_PREFIX=`$PYTHON -c "import sys; print(sys.prefix)"`
    PYTHON_VERSION=`$PYTHON -c "import sys; print(sys.version[[0:3]])"`

    AC_MSG_CHECKING([for Python include path])
    if test -z "$PYTHON_INCLUDEDIR"; then
        python_path=`$PYTHON -c "import distutils.sysconfig; \
                                 print (distutils.sysconfig.get_python_inc ());"`
        PYTHON_INCLUDEDIR=$python_path
    fi
    AC_MSG_RESULT([$PYTHON_INCLUDEDIR])

    AC_MSG_CHECKING([for Python site-packages path])
    if test -z "$PYTHON_SITE_PACKAGES"; then
        PYTHON_SITE_PACKAGES=`$PYTHON -c "import distutils.sysconfig; \
                print (distutils.sysconfig.get_python_lib(1,0));"`
    fi
    AC_MSG_RESULT([$PYTHON_SITE_PACKAGES])

    old_LIBS="$LIBS"
    LIBS="$LIBS -lpython$PYTHON_VERSION"
    AC_CHECK_FUNCS([PyCapsule_New])
    LIBS="$old_LIBS"
fi

AC_SUBST(PYTHON_PREFIX)
AC_SUBST(PYTHON_VERSION)
AC_SUBST(PYTHON_INCLUDEDIR)
AC_SUBST(PYTHON_SITE_PACKAGES)

AM_CONDITIONAL([HAVE_PYTHON],
    [test "x$PYTHON_INCLUDEDIR" != "x" && test "x$PYTHON_SITE_PACKAGES" != "x"])

dnl Check for Ruby and rake (optional, for Ruby bindings).
AC_CHECK_LIB([ruby],[ruby_init],[HAVE_LIBRUBY=1],[HAVE_LIBRUBY=0])
AC_CHECK_PROG([RAKE],[rake],[rake],[no])

AM_CONDITIONAL([HAVE_RUBY],
    [test "x$RAKE" != "xno" && test -n "$HAVE_LIBRUBY"])

dnl dnl Check for Java.
dnl AC_ARG_WITH(java_home,
dnl     [AS_HELP_STRING([--with-java-home],
dnl         [specify path to JDK directory @<:@default=check@:>@])],
dnl     [],
dnl     [with_java_home=check])

dnl if test "x$with_java_home" != "xno"; then
dnl     if test "x$with_java_home" != "xyes" && test "x$with_java_home" != "xcheck"
dnl     then
dnl         # Reject unsafe characters in $JAVA_HOME
dnl         jh_lf='
dnl '
dnl         case $JAVA_HOME in
dnl           *[\\\"\#\$\&\'\`$jh_lf\ \	]*)
dnl             AC_MSG_FAILURE([unsafe \$JAVA_HOME directory (use --with-java-home=no to disable Java support)]);;
dnl         esac
dnl         if test -d "$with_java_home"; then
dnl             JAVA_HOME="$with_java_home"
dnl         else
dnl             AC_MSG_FAILURE([$with_java_home is not a directory (use --with-java-home=no to disable Java support)])
dnl         fi
dnl     fi

dnl     if test "x$JAVA_HOME" = "x"; then
dnl         # Look for Java in some likely locations.
dnl         for d in \
dnl             /usr/lib/jvm/java \
dnl             /usr/lib/jvm/java-6-openjdk
dnl         do
dnl             if test -d $d && test -f $d/bin/java; then
dnl                 JAVA_HOME=$d
dnl                 break
dnl             fi
dnl         done
dnl     fi

dnl     if test "x$JAVA_HOME" != "x"; then
dnl         AC_MSG_CHECKING(for JDK in $JAVA_HOME)
dnl         if test ! -x "$JAVA_HOME/bin/java"; then
dnl             AC_MSG_ERROR([missing $JAVA_HOME/bin/java binary (use --with-java-home=no to disable Java support)])
dnl         else
dnl             JAVA="$JAVA_HOME/bin/java"
dnl         fi
dnl         if test ! -x "$JAVA_HOME/bin/javac"; then
dnl             AC_MSG_ERROR([missing $JAVA_HOME/bin/javac binary])
dnl         else
dnl             JAVAC="$JAVA_HOME/bin/javac"
dnl         fi
dnl         if test ! -x "$JAVA_HOME/bin/javah"; then
dnl             AC_MSG_ERROR([missing $JAVA_HOME/bin/javah binary])
dnl         else
dnl             JAVAH="$JAVA_HOME/bin/javah"
dnl         fi
dnl         if test ! -x "$JAVA_HOME/bin/javadoc"; then
dnl             AC_MSG_ERROR([missing $JAVA_HOME/bin/javadoc binary])
dnl         else
dnl             JAVADOC="$JAVA_HOME/bin/javadoc"
dnl         fi
dnl         if test ! -x "$JAVA_HOME/bin/jar"; then
dnl             AC_MSG_ERROR([missing $JAVA_HOME/bin/jar binary])
dnl         else
dnl             JAR="$JAVA_HOME/bin/jar"
dnl         fi
dnl         java_version=`$JAVA -version 2>&1 | grep "java version"`
dnl         AC_MSG_RESULT(found $java_version in $JAVA_HOME)

dnl         dnl Find jni.h.
dnl         AC_MSG_CHECKING([for jni.h])
dnl         if test -f "$JAVA_HOME/include/jni.h"; then
dnl             JNI_CFLAGS="-I$JAVA_HOME/include"
dnl         else
dnl             if test "`find $JAVA_HOME -name jni.h`" != ""; then
dnl                 head=`find $JAVA_HOME -name jni.h | tail -1`
dnl                 dir=`dirname "$head"`
dnl                 JNI_CFLAGS="-I$dir"
dnl             else
dnl                 AC_MSG_FAILURE([missing jni.h header file])
dnl             fi
dnl         fi
dnl         AC_MSG_RESULT([$JNI_CFLAGS])

dnl         dnl Find jni_md.h.
dnl         AC_MSG_CHECKING([for jni_md.h])
dnl         case "$build_os" in
dnl         *linux*) system="linux" ;;
dnl         *SunOS*) system="solaris" ;;
dnl         *cygwin*) system="win32" ;;
dnl         *) system="$build_os" ;;
dnl         esac
dnl         if test -f "$JAVA_HOME/include/$system/jni_md.h"; then
dnl             JNI_CFLAGS="$JNI_CFLAGS -I$JAVA_HOME/include/$system"
dnl         else
dnl             if test "`find $JAVA_HOME -name jni_md.h`" != ""; then
dnl                 head=`find $JAVA_HOME -name jni_md.h | tail -1`
dnl                 dir=`dirname "$head"`
dnl                 JNI_CFLAGS="$JNI_CFLAGS -I$dir"
dnl             else
dnl                 AC_MSG_FAILURE([missing jni_md.h header file])
dnl             fi
dnl         fi
dnl         AC_MSG_RESULT([$JNI_CFLAGS])

dnl         dnl Need extra version flag?
dnl         AC_MSG_CHECKING([extra javac flags])
dnl         JAVAC_FLAGS=
dnl         javac_version=`$JAVAC -version 2>&1`
dnl         case "$javac_version" in
dnl         *Eclipse*)
dnl             JAVAC_FLAGS="-source 1.5" ;;
dnl         esac
dnl         AC_MSG_RESULT([$JAVAC_FLAGS])

dnl         dnl Where to install jarfiles.
dnl         dnl XXX How to make it configurable?
dnl         JAR_INSTALL_DIR=\${prefix}/share/java
dnl         JNI_INSTALL_DIR=\${libdir}

dnl         dnl JNI version.
dnl         jni_major_version=`echo "$VERSION" | awk -F. '{print $1}'`
dnl         jni_minor_version=`echo "$VERSION" | awk -F. '{print $2}'`
dnl         jni_micro_version=`echo "$VERSION" | awk -F. '{print $3}'`
dnl         JNI_VERSION_INFO=`expr "$jni_major_version" + "$jni_minor_version"`":$jni_micro_version:$jni_minor_version"
dnl     fi
dnl fi

dnl AC_SUBST(JAVA_HOME)
dnl AC_SUBST(JAVA)
dnl AC_SUBST(JAVAC)
dnl AC_SUBST(JAVAH)
dnl AC_SUBST(JAVADOC)
dnl AC_SUBST(JAR)
dnl AC_SUBST(JNI_CFLAGS)
dnl AC_SUBST(JAVAC_FLAGS)
dnl AC_SUBST(JAR_INSTALL_DIR)
dnl AC_SUBST(JNI_INSTALL_DIR)
dnl AC_SUBST(JNI_VERSION_INFO)

dnl AM_CONDITIONAL([HAVE_JAVA],[test -n "$JAVAC"])

dnl dnl Check for Haskell (GHC).
dnl AC_CHECK_PROG([GHC],[ghc],[ghc],[no])

dnl AM_CONDITIONAL([HAVE_HASKELL],
dnl     [test "x$GHC" != "xno"])

dnl Produce output files.
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_FILES([Makefile
                 generator/Makefile
                 gnulib/lib/Makefile
                 gnulib/tests/Makefile
                 hivex.pc
                 images/Makefile
                 lib/Makefile
                 lib/tools/Makefile
                 ocaml/Makefile ocaml/META
                 perl/Makefile perl/Makefile.PL
                 python/Makefile
                 po/Makefile.in
                 regedit/Makefile
                 ruby/Makefile ruby/Rakefile
                 sh/Makefile
                 xml/Makefile])
AC_CONFIG_FILES([python/run-python-tests], [chmod +x python/run-python-tests])
AC_OUTPUT

dnl Produce summary.
echo
echo
echo "------------------------------------------------------------"
echo "Thank you for downloading $PACKAGE_STRING"
echo
echo "This is how we have configured the optional components for you today:"
echo
echo -n "OCaml bindings ...................... "
if test "x$HAVE_OCAML_TRUE" = "x"; then echo "yes"; else echo "no"; fi
echo -n "Perl bindings ....................... "
if test "x$HAVE_PERL_TRUE" = "x"; then echo "yes"; else echo "no"; fi
echo -n "Python bindings ..................... "
if test "x$HAVE_PYTHON_TRUE" = "x"; then echo "yes"; else echo "no"; fi
echo -n "Ruby bindings ....................... "
if test "x$HAVE_RUBY_TRUE" = "x"; then echo "yes"; else echo "no"; fi
dnl echo -n "Java bindings ....................... "
dnl if test "x$HAVE_JAVA_TRUE" = "x"; then echo "yes"; else echo "no"; fi
dnl echo -n "Haskell bindings .................... "
dnl if test "x$HAVE_HASKELL" = "x"; then echo "yes"; else echo "no"; fi
echo
echo "If any optional component is configured 'no' when you expected 'yes'"
echo "then you should check the preceeding messages."
echo
echo "Please report bugs back to the mailing list:"
echo "http://www.redhat.com/mailman/listinfo/libguestfs"
echo
echo "Next you should type 'make' to build the package,"
echo "then 'make check' to run the tests."
echo "------------------------------------------------------------"
