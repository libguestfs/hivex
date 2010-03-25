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

use Test::More tests => 6;

use Win::Hivex;
use Win::Hivex::Regedit qw(reg_import reg_export);

my $srcdir = $ENV{srcdir} || ".";

my $h = Win::Hivex->open ("$srcdir/../images/minimal", write => 1);
ok ($h);

my $data;

# Note: These tests are supposed to fail.

# Need a blank line between sections.
$data = '
[A]
[B]';
run_test ($data);

# Invalid header.
$data = '
[A]B';
run_test ($data);

# Must create intermediate nodes first.
$data = '
[A\B\C\D]';
run_test ($data);

# Invalid quoting.
$data = '
[A]
"Quote"it"="Hello"';
run_test ($data);

$data = '
[A]
"Quote it\"="Hello"';
run_test ($data);

# Invalid hex -- fails, 'pack' processes it anyway.
#$data = '
#[A]
#"Key"=hex(1):xy';
#run_test ($data);

#----------------------------------------------------------------------

sub run_test {
    my $data = shift;

    eval {
        my $fh = new IO::Scalar \$data;
        reg_import ($h, $fh);
    };
    #warn "$@\n";
    ok ($@);
}
