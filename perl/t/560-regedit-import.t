# Win::Hivex::Regedit test -*- perl -*-
# Copyright (C) 2010 Red Hat Inc.
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

use strict;
use warnings;

use IO::Scalar;

use Test::More tests => 16;

use Win::Hivex;
use Win::Hivex::Regedit qw(reg_import reg_export);

my $srcdir = $ENV{srcdir} || ".";

my $h = Win::Hivex->open ("$srcdir/../images/minimal", write => 1);
ok ($h);

my ($data, $expected);

# Note that we don't clear the hive between tests, so results of
# next test depend on the previous test.

$data = '
[\A]

[\B]

[\C]
"Key1"=hex(2):48,00,65,00,6c,00,6c,00,6f,00
"Key2"=str(2):"Hello"
"Key3"=hex:48,00,65,00,6c,00,6c,00,6f,00,\
  48,00,65,00,6c,00,6c,00,6f,00
"Key4"=dword:ff123456';
$expected = '[\]

[\A]

[\B]

[\C]
"Key1"=hex(2):48,00,65,00,6c,00,6c,00,6f,00
"Key2"=hex(2):48,00,65,00,6c,00,6c,00,6f,00,00,00
"Key3"=hex(3):48,00,65,00,6c,00,6c,00,6f,00,48,00,65,00,6c,00,6c,00,6f,00
"Key4"=dword:ff123456

';

run_test ($data, $expected);

$data = '
[\A]
@="Hello"

[-\B]
';
$expected = '[\]

[\A]
@=hex(1):48,00,65,00,6c,00,6c,00,6f,00,00,00

[\C]
"Key1"=hex(2):48,00,65,00,6c,00,6c,00,6f,00
"Key2"=hex(2):48,00,65,00,6c,00,6c,00,6f,00,00,00
"Key3"=hex(3):48,00,65,00,6c,00,6c,00,6f,00,48,00,65,00,6c,00,6c,00,6f,00
"Key4"=dword:ff123456

';

run_test ($data, $expected);

$data = '
[\A]
@=-

[-\C]

[\A\B]
';
$expected = '[\]

[\A]

[\A\B]

';

run_test ($data, $expected);

# In the next test, the value of ValueContainingEscapes in the
# imported data is \\W\\, which will become \W\ in the final hive.
# However Perl has complex and inconsistent rules on quoting
# backslashes.  See:
# http://en.wikibooks.org/wiki/Perl_Programming/Strings#Single_Quoted_Strings
$data = '
[\A]
"NotExistant"=-

[\A\B]
"Key\"Containing\"Quotes"=hex(0):
"ValueContainingEscapes"="\\\\W\\\\"
';
$expected = '[\]

[\A]

[\A\B]
"Key\"Containing\"Quotes"=hex(0):
"ValueContainingEscapes"=hex(1):5c,00,57,00,5c,00,00,00

';

run_test ($data, $expected);

$data = '
[\A\B]
"Key\"Containing\"Quotes"=-
"ValueContainingEscapes"=-

[-\A]
';
$expected = '[\]

';

run_test ($data, $expected);

#----------------------------------------------------------------------

sub run_test {
    my $data = shift;
    my $expected = shift;

    my $fh = new IO::Scalar \$data;
    reg_import ($fh, $h);
    ok (1);

    $fh = new IO::Scalar;
    reg_export ($h, "\\", $fh);
    ok (1);

    my $actual = ${$fh->sref};
    warn "\n\n----- ACTUAL -----\n$actual\n----- EXPECTED -----\n$expected\n\n"
        if $actual ne $expected;

    ok ($actual eq $expected)
}
