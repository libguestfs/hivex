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

use Encode qw(from_to);
use IO::Scalar;

use Test::More tests => 8;

use Win::Hivex;
use Win::Hivex::Regedit qw(reg_export);

my $srcdir = $ENV{srcdir} || ".";

my $h = Win::Hivex->open ("$srcdir/../images/minimal", write => 1);
ok ($h);

my $root = $h->root ();
ok ($root);

$h->node_add_child ($root, "B");
ok (1);

$h->node_add_child ($root, "A");
ok (1);

my $b = $h->node_get_child ($root, "B");
ok ($b);

# Encode a string as UTF16-LE.
sub utf16le
{
    my $s = shift;
    from_to ($s, "ascii", "utf-16le");
    $s;
}

# Convert a 32 bit integer to a little endian 4 byte data field.
sub dwordle
{
    pack ("V", $_[0]);
}

my @values = (
    # Values are entered in a random order here, but they should be
    # sorted on export.
    { key => "Key2", t => 2, value => utf16le ("DEF") },
    { key => "", t => 1, value => "Default" },
    { key => "Key3", t => 4, value => dwordle (0xff876543) },
    { key => "Key1", t => 1, value => "ABC" },
    );
$h->node_set_values ($b, \@values);
ok (1);

my $fh = new IO::Scalar;
reg_export ($h, "\\", $fh, prefix => "HKEY_LOCAL_MACHINE\\SOFTWARE\\");

my $expected = '[HKEY_LOCAL_MACHINE\\SOFTWARE\\]

[HKEY_LOCAL_MACHINE\\SOFTWARE\\A]

[HKEY_LOCAL_MACHINE\\SOFTWARE\\B]
@=hex(1):44,65,66,61,75,6c,74
"Key1"=hex(1):41,42,43
"Key2"=hex(2):44,00,45,00,46,00
"Key3"=dword:ff876543

';

ok (${$fh->sref} eq $expected);

$fh = new IO::Scalar;
reg_export ($h, "\\B", $fh);

$expected = '[\\B]
@=hex(1):44,65,66,61,75,6c,74
"Key1"=hex(1):41,42,43
"Key2"=hex(2):44,00,45,00,46,00
"Key3"=dword:ff876543

';

ok (${$fh->sref} eq $expected);

# don't commit because that would overwrite the original file
# $h->commit ();
