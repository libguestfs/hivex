## hivex - a library for reading and writing Windows Registry "hive" files

Written by Richard W.M. Jones, rjones@redhat.com
Copyright (C) 2009-2022 Red Hat Inc.

This is a self-contained library for reading and writing Windows
Registry "hive" binary files.

Unlike many other tools in this area, it doesn't use the textual .REG
format for output, because parsing that is as much trouble as parsing
the original binary format.  Instead it makes the file available
through a C API, or there is a separate program to export the hive as
XML.

This library was derived from several sources:

* NTREG registry reader/writer library by Petter Nordahl-Hagen
   (LGPL v2.1 licensed library and program)
* http://pogostick.net/~pnh/ntpasswd/WinReg.txt
* dumphive (a BSD-licensed Pascal program by Markus Stephany)
* http://www.sentinelchicken.com/data/TheWindowsNTRegistryFileFormat.pdf
* editreg program from Samba - this program was removed in later
  versions of Samba, so you have to go back in the source repository
  to find it (GPLv2+)
* http://amnesia.gtisc.gatech.edu/~moyix/suzibandit.ltd.uk/MSc/
* reverse engineering the format (see lib/tools/visualizer.ml)

Like NTREG, this library only attempts to read Windows NT registry
files (ie. not Windows 3.1 or Windows 95/98/ME).  See the link above
for documentation on the older formats if you wish to read them.

Unlike NTREG, this code is much more careful about handling error
cases, corrupt and malicious registry files, and endianness.

## License

The license for this library is LGPL v2.1, but not later versions.
For full details, see the file LICENSE in this directory.

## Dependencies

To just build the library, the dependencies are quite minimal.  You
only need the following:

* An ISO C compiler.
* Perl tools 'pod2man' and 'pod2text'.
* OCaml interpreter (`/usr/bin/ocaml`), only for building from git.

These dependencies are needed for the tools:

* Readline library (optional, to add command-line editing to hivexsh).
* libxml2 (optional, for hivexml).

To build the language bindings, you will need various extra packages.
See the configure output for more details.

## Building

```
autoreconf -i
./generator/generator.ml
./configure
make
make check
```

## Directories and tools

`extra-tests/`

Extra tests which need external test data.  See
hivex-test-data on http://git.annexia.org

`generator/`

Generator used to write a lot of boilerplate code for
header files, documentation, language bindings etc.
The API for hivex is specified in the generator.

`images/`

Test hive files.  See images/README.

`lib/`

The C library.

`ocaml/`
`perl/`
`python/`
`ruby/`

OCaml, Perl, Python or Ruby bindings and tests.  The bindings
are generated by 'generator/generator.ml'.

Python 2 or 3 is supported.  To select between them, set
PYTHON to point to the Python interpreter you want to use, eg:

./configure PYTHON=/usr/bin/python3

`regedit/`

Regedit-like registry merging tool.

`sh/`

Interactive shell.  This also contains the old 'hivexget'
tool (originally written in C, now replaced by a hivexsh
shell script).

`xml/`

hivexml program which converts hive files to XML.