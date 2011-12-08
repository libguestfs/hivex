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

# Demonstrate value_data_cell_offset by looking at the value data at
# "\$$$PROTO.HIV\ModerateValueParent\33Bytes", verified to be at file
# offset 8680 (0x21e8) of the hive rlenvalue_test_hive.  The returned
# length and offset for this value cell should be 37 bytes, position
# 8712.

use strict;
use warnings;
use Test::More tests => 5;

use Win::Hivex;

my $srcdir = $ENV{srcdir} || ".";

my $h = Win::Hivex->open ("$srcdir/../images/rlenvalue_test_hive");
ok ($h);

my $root = $h->root ();
ok ($root);

my $moderate_value_node = $h->node_get_child ($root, "ModerateValueParent");

my $moderate_value_value = $h->node_get_value ($moderate_value_node, "33Bytes");

my ($off, $len) = $h->value_data_cell_offset ($moderate_value_value);
ok ($off == 37);
ok ($len == 8712);

ok (1);
