#!/usr/bin/env perl
# hivex extra tests
# Copyright (C) 2013 Red Hat Inc.
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

# This is a test of RHBZ#987463: Take the real hives and try inserting
# a new key as a subkey of every key in the hive, and verify that we
# get the expected data and don't get any errors.

use strict;
use warnings;

use Win::Hivex;

my @hives = glob "*.hive";
die "no hive files found in current directory" unless @hives > 0;

foreach my $hive (@hives) {
    print "$hive\n";

    # Note we open it for writing, but never commit, so no changes
    # are saved back to disk.
    my $h = Win::Hivex->open ($hive, write => 1);
    my $root = $h->root ();

    my $added = test_node ($h, $root);
    print "inserted $added nodes successfully\n";
}

sub test_node
{
    my $h = shift;
    my $node = shift;

    local $_;
    my $added = 0;

    # Child nodes before adding the child.
    my @children_before = $h->node_children ($node);

    # Choose a random name for the new key, so that it won't clash
    # with an existing key and so that over time we test inserting at
    # all positions within the key.
    my @chars = ("A".."Z", "a".."z", "0".."9");
    my $string = "";
    $string .= $chars[rand @chars] for 1..8;
    $h->node_add_child ($node, $string);
    $added++;

    # Child nodes after adding the child.
    my @children_after = $h->node_children ($node);

    die "expected ", 0+@children_before, " + 1 == ", 0+@children_after
        unless @children_before+1 == @children_after;

    foreach (@children_after) {
        if ($h->node_name ($_) eq $string) {
            goto found;
        }
    }
    die "did not find new node in node children";

  found:
    # Iterate into the child nodes.  Note don't iterate into the
    # newly created child node.
    $added += test_node ($h, $_) foreach @children_before;

    return $added;
}

exit 0
