# hivex
# Copyright (C) 2009-2019 Red Hat Inc.
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
m4_define([hivex_release], [23])
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

LT_INIT

dnl Define $(SED).
m4_ifdef([AC_PROG_SED],[
    AC_PROG_SED
],[
    dnl ... else hope for the best
    AC_SUBST([SED], "sed")
])

dnl Check for basic C environment.
AC_PROG_CC
AC_PROG_INSTALL
AC_PROG_CPP

AC_C_PROTOTYPES
test "x$U" != "x" && AC_MSG_ERROR([Compiler not ANSI compliant])

AM_PROG_CC_C_O

AC_ARG_ENABLE([werror],
    [AS_HELP_STRING([--enable-werror],
                    [turn on lots of GCC warnings (for developers)])],
     [case $enableval in
      yes|no) ;;
      *)      AC_MSG_ERROR([bad value $enableval for werror option]) ;;
      esac
      gcc_warnings=$enableval],
      [gcc_warnings=no]
)
WARN_CFLAGS="-Wall"
AC_SUBST([WARN_CFLAGS])
if test "x$gcc_warnings" = "xyes"; then
    WERROR_CFLAGS="-Werror"
fi
AC_SUBST([WERROR_CFLAGS])

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

dnl Is this Windows (or mingw cross-compilation)?
AC_MSG_CHECKING([if the target is Windows])
AS_CASE([$host_os],
    [mingw*|msys*], [
        is_windows=yes
    ],
    [is_windows=no]
)
AC_MSG_RESULT([$is_windows])
AM_CONDITIONAL([IS_WINDOWS], [test "x$is_windows" = "xyes"])

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
AM_GNU_GETTEXT_VERSION([0.21])
AM_ICONV

dnl libxml2 (optional).
PKG_CHECK_MODULES([LIBXML2], [libxml-2.0], [
    AC_SUBST([LIBXML2_CFLAGS])
    AC_SUBST([LIBXML2_LIBS])
    have_libxml2=yes
], [
    have_libxml2=no
])
AM_CONDITIONAL([HAVE_LIBXML2],[test "x$have_libxml2" = "xyes"])

dnl hivexsh depends on open_memstream, which is absent on OS X.
AC_CHECK_FUNC([open_memstream])
AM_CONDITIONAL([HAVE_HIVEXSH],[test "x$ac_cv_func_open_memstream" = "xyes"])

dnl Check for OCaml (optional, for OCaml bindings).
OCAMLC=no
OCAMLFIND=no
AC_ARG_ENABLE([ocaml],
        AS_HELP_STRING([--disable-ocaml], [Disable OCaml language bindings]),
        [],
        [enable_ocaml=yes])
AS_IF([test "x$enable_ocaml" != "xno"],
        [dnl OCAMLC and OCAMLFIND have to be unset first, otherwise
         dnl AC_CHECK_TOOL (inside AC_PROG_OCAML) will not look.
         OCAMLC=
         OCAMLFIND=
         AC_PROG_OCAML
         AC_PROG_FINDLIB
        ])
AM_CONDITIONAL([HAVE_OCAML],
               [test "x$OCAMLC" != "xno" && test "x$OCAMLFIND" != "xno"])
AM_CONDITIONAL([HAVE_OCAMLOPT],
               [test "x$OCAMLOPT" != "xno" && test "x$OCAMLFIND" != "xno"])

AS_IF([test "x$OCAMLC" != "xno" && test "x$OCAMLFIND" != "xno"],[
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
    if $OCAMLC -c conftest.c >&AS_MESSAGE_LOG_FD 2>&1 && \
       $OCAMLC -c conftest_ml.ml >&AS_MESSAGE_LOG_FD 2>&1 && \
       $OCAMLC -custom conftest.o conftest_ml.cmo -o conftest >&AS_MESSAGE_LOG_FD 2>&1 ; then
        AC_DEFINE([HAVE_CAML_RAISE_WITH_ARGS],[1],
                  [Defined if function caml_raise_with_args exists.])
        AC_MSG_RESULT([found])
    else
        AC_MSG_RESULT([not found])
    fi
    rm -f conftest conftest.* conftest_ml.*

    dnl Check if OCaml has caml_alloc_initialized_string (added 2017).
    AC_MSG_CHECKING([for caml_alloc_initialized_string])
    cat >conftest.c <<'EOF'
#include <caml/alloc.h>
int main () { char *p = (void *) caml_alloc_initialized_string; return 0; }
EOF
    AS_IF([$OCAMLC conftest.c >&AS_MESSAGE_LOG_FD 2>&1],[
        AC_MSG_RESULT([yes])
        AC_DEFINE([HAVE_CAML_ALLOC_INITIALIZED_STRING],[1],
                  [caml_alloc_initialized_string found at compile time.])
    ],[
        AC_MSG_RESULT([no])
    ])
    rm -f conftest.c conftest.o
])

dnl Check for Perl (optional, for Perl bindings and Perl tools).
AC_CHECK_PROG([PERL],[perl],[perl],[no])

AC_ARG_ENABLE([perl],
        AS_HELP_STRING([--disable-perl], [Disable Perl language bindings]),
        [],
        [enable_perl=yes])
AS_IF([test "x$enable_perl" != "xno"],[
        dnl Check for Perl modules that must be present to compile and
        dnl test the Perl bindings.
        missing_perl_modules=no
        for pm in Test::More ExtUtils::MakeMaker IO::Stringy; do
            AC_MSG_CHECKING([for $pm])
            if ! $PERL -M$pm -e1 >/dev/null 2>&1; then
                AC_MSG_RESULT([no])
                missing_perl_modules=yes
            else
                AC_MSG_RESULT([yes])
            fi
        done
        if test "x$missing_perl_modules" = "xyes"; then
            AC_MSG_WARN([some Perl modules required to compile or test the Perl bindings are missing])
        fi
        ])
AM_CONDITIONAL([HAVE_PERL],
    [test "x$enable_perl" != "xno" && test "x$PERL" != "xno" && test "x$missing_perl_modules" != "xyes"])

dnl Check for Python (optional, for Python bindings).
PYTHON_PREFIX=
PYTHON_VERSION=
PYTHON_CFLAGS=
PYTHON_INSTALLDIR=

AC_ARG_ENABLE([python],
        AS_HELP_STRING([--disable-python], [Disable Python language bindings]),
        [],
        [enable_python=yes])
AS_IF([test "x$enable_python" != "xno"],
        [
        AC_CHECK_PROG([PYTHON],[python],[python],[no])

        if test "x$PYTHON" != "xno"; then
	    AC_MSG_CHECKING([Python prefix])
            PYTHON_PREFIX=`$PYTHON -c "import sys; print (sys.prefix)"`
	    AC_MSG_RESULT([$PYTHON_PREFIX])

	    AC_MSG_CHECKING([Python version])
            PYTHON_VERSION_MAJOR=`$PYTHON -c "import sys; print (sys.version_info@<:@0@:>@)"`
            PYTHON_VERSION_MINOR=`$PYTHON -c "import sys; print (sys.version_info@<:@1@:>@)"`
            PYTHON_VERSION="$PYTHON_VERSION_MAJOR.$PYTHON_VERSION_MINOR"
	    AC_MSG_RESULT([$PYTHON_VERSION])

            AC_MSG_CHECKING([for Python include path])
            if test -z "$PYTHON_CFLAGS"; then
                python_path=`$PYTHON -c "import sysconfig; \
                                         print (sysconfig.get_path('include'));"`
                python_platpath=`$PYTHON -c "import sysconfig; \
                                             print (sysconfig.get_path('platinclude'));"`
                if test "$python_path" = "$python_platpath"; then
                    PYTHON_CFLAGS=-I$python_path
                else
                    PYTHON_CFLAGS="-I$python_path -I$python_platpath"
                fi
            fi
            AC_MSG_RESULT([$PYTHON_CFLAGS])

            AC_ARG_WITH([python-installdir],
                        [AS_HELP_STRING([--with-python-installdir],
			                [directory to install python modules @<:@default=check@:>@])],
			[PYTHON_INSTALLDIR="$withval"
			AC_MSG_NOTICE([Python install dir $PYTHON_INSTALLDIR])],
			[PYTHON_INSTALLDIR=check])

	    if test "x$PYTHON_INSTALLDIR" = "xcheck"; then
	        PYTHON_INSTALLDIR=
                AC_MSG_CHECKING([for Python site-packages path])
                if test -z "$PYTHON_INSTALLDIR"; then
                    PYTHON_INSTALLDIR=`$PYTHON -c "import sysconfig; \
                                                   print (sysconfig.get_path('platlib'));"`
                fi
                AC_MSG_RESULT([$PYTHON_INSTALLDIR])
            fi

            AC_MSG_CHECKING([for Python extension suffix (PEP-3149)])
            if test -z "$PYTHON_EXT_SUFFIX"; then
                python_ext_suffix=`$PYTHON -c "import sysconfig; \
                                             print (sysconfig.get_config_var('EXT_SUFFIX') or sysconfig.get_config_var('SO'))"`
                PYTHON_EXT_SUFFIX=$python_ext_suffix
            fi
            AC_MSG_RESULT([$PYTHON_EXT_SUFFIX])
        fi

        AC_SUBST(PYTHON_PREFIX)
        AC_SUBST(PYTHON_VERSION)
        AC_SUBST(PYTHON_CFLAGS)
        AC_SUBST(PYTHON_INSTALLDIR)
        AC_SUBST(PYTHON_EXT_SUFFIX)
        ])
AM_CONDITIONAL([HAVE_PYTHON],
    [test "x$PYTHON" != "xno" && test "x$PYTHON_CFLAGS" != "x" && test "x$PYTHON_INSTALLDIR" != "x"])

dnl Check for Ruby and rake (optional, for Ruby bindings).
AC_ARG_ENABLE([ruby],
    AS_HELP_STRING([--disable-ruby], [Disable Ruby language bindings]),
    [],
    [enable_ruby=yes])
AS_IF([test "x$enable_ruby" != "xno"],[
    AC_CHECK_PROG([RUBY],[ruby],[ruby],[no])
    AC_CHECK_PROG([RAKE],[rake],[rake],[no])

    AS_IF([test -n "$RUBY" && test -n "$RAKE"],[
        dnl Find the library.  Note on Debian it's not -lruby.
        AC_MSG_CHECKING([for C library for Ruby extensions])
        ruby_cmd='puts RbConfig::CONFIG@<:@"RUBY_SO_NAME"@:>@'
        echo running: $RUBY -rrbconfig -e \'$ruby_cmd\' >&AS_MESSAGE_LOG_FD
        $RUBY -rrbconfig -e "$ruby_cmd" >conftest 2>&AS_MESSAGE_LOG_FD
        libruby="$(cat conftest)"
        rm conftest
        AS_IF([test -n "$libruby"],[
            ruby_cmd='puts RbConfig::CONFIG@<:@"libdir"@:>@'
            echo running: $RUBY -rrbconfig -e \'$ruby_cmd\' >&AS_MESSAGE_LOG_FD
            $RUBY -rrbconfig -e "$ruby_cmd" >conftest 2>&AS_MESSAGE_LOG_FD
            libruby_libdir="$(cat conftest)"
            rm conftest
            test -n "$libruby_libdir" && libruby_libdir="-L$libruby_libdir"
            AC_MSG_RESULT([-l$libruby])
            AC_CHECK_LIB([$libruby],[ruby_init],
                         [have_libruby=1],[have_libruby=],[$libruby_libdir])

            dnl Symbols that we substitute when missing.
            AS_IF([test -n "$have_libruby"],[
                old_LIBS="$LIBS"
                LIBS="$LIBS -l$libruby"
                AC_CHECK_FUNCS([rb_hash_lookup])
                LIBS="$old_LIBS"
            ])
        ],[
            AC_MSG_RESULT([not found])
        ])
    ])
])
AM_CONDITIONAL([HAVE_RUBY],
    [test -n "$RUBY" && test -n "$RAKE" && test -n "$have_libruby"])

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
                 extra-tests/Makefile
                 generator/Makefile
                 gnulib/lib/Makefile
                 images/Makefile
                 include/Makefile
                 lib/Makefile
                 lib/hivex.pc
                 lib/local/hivex.pc
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
AC_CONFIG_FILES([ruby/run-ruby-tests], [chmod +x ruby/run-ruby-tests])
AC_CONFIG_FILES([run], [chmod +x,-w run])
AC_OUTPUT

dnl Produce summary.
echo
echo
echo "------------------------------------------------------------"
echo "Thank you for downloading $PACKAGE_STRING"
echo
echo "This is how we have configured the optional components for you today:"
echo
AS_ECHO_N(["OCaml bindings ...................... "])
if test "x$HAVE_OCAML_TRUE" = "x"; then echo "yes"; else echo "no"; fi
AS_ECHO_N(["Perl bindings ....................... "])
if test "x$HAVE_PERL_TRUE" = "x"; then echo "yes"; else echo "no"; fi
AS_ECHO_N(["Python bindings ..................... "])
if test "x$HAVE_PYTHON_TRUE" = "x"; then echo "yes"; else echo "no"; fi
AS_ECHO_N(["Ruby bindings ....................... "])
if test "x$HAVE_RUBY_TRUE" = "x"; then echo "yes"; else echo "no"; fi
dnl AS_ECHO_N(["Java bindings ....................... "])
dnl if test "x$HAVE_JAVA_TRUE" = "x"; then echo "yes"; else echo "no"; fi
dnl AS_ECHO_N(["Haskell bindings .................... "])
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
