#!/usr/bin/env perl
# hivex extra tests
# Copyright (C) 2014 Red Hat Inc.
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

# Fuzz the real hives.  We're only checking here that hivex doesn't
# crash on the fuzzed hives, not that it can read them.

use strict;
use warnings;

use File::Temp qw(tempfile);
use File::Copy;

use Win::Hivex;

my @hives = glob "*.hive";
my $nr_hives = 0+@hives;
die "no hive files found in current directory" unless $nr_hives > 0;

# Run for this many seconds.
my $total_time = 120;

my $start_time = time ();
while (time () - $start_time <= $total_time) {
    # Pick a real hive at random.
    my $hive = $hives[int (rand ($nr_hives))];

    # Copy it and fuzz the copy.
    my ($fh, $filename) = tempfile ();
    copy ($hive, $filename) or die "copy $hive to $filename failed: $!";

    print "Fuzzing $hive (copied to $filename) ...\n";
    my $size = -s $filename;
    open ($fh, ">", $filename) or die "reopen: $filename: $!";
    for (my $i = 0; $i < 100000; ++$i) {
        seek ($fh, int (rand ($size)), 0);
        my $c = chr (int (rand (256)));
        syswrite ($fh, $c);
    }
    close ($fh);

    print "Opening ...\n";
    eval { Win::Hivex->open ($filename) };

    unlink ($filename);
}

print "Finished after running for $total_time seconds.\n";
exit 0
