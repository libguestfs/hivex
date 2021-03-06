# hivex Perl bindings -*- perl -*-
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
use Test::More tests => 11;

use Win::Hivex;

my $srcdir = $ENV{srcdir} || ".";

my $h = Win::Hivex->open ("$srcdir/../images/minimal", write => 1);
ok ($h);

my $root = $h->root ();
ok ($root);

$h->node_add_child ($root, "B");
ok (1);

my $b = $h->node_get_child ($root, "B");
ok ($b);

my $values = [
    { key => "Key1", t => 3, value => "ABC" },
    { key => "Key2", t => 3, value => "DEF" }
    ];
$h->node_set_values ($b, $values);
ok (1);

my $value1 = { key => "Key3", t => 3, value => "GHI" };
$h->node_set_value ($b, $value1);
ok (1);

my $value2 = { key => "Key1", t => 3, value => "JKL" };
$h->node_set_value ($b, $value2);
ok (1);

my ($val, $t, $data);
$val = $h->node_get_value ($b, "Key1");
($t, $data) = $h->value_value ($val);
ok ($t == 3);
ok ($data eq "JKL");

$val = $h->node_get_value ($b, "Key3");
($t, $data) = $h->value_value ($val);
ok ($t == 3);
ok ($data eq "GHI");

# don't commit because that would overwrite the original file
# $h->commit ();
