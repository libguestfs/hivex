# hivex Perl bindings -*- perl -*-

use strict;
use warnings;
use utf8; # so the strings in this file are interpreted correctly.
BEGIN {
    binmode STDOUT, ':encoding(UTF-8)';
    binmode STDERR, ':encoding(UTF-8)';
}

use Test::More;

# Old Perl hivex bindings cannot handle Unicode properly.
if ($] < 5.012) {
    plan skip_all => "Version of Perl is too old to handle Unicode";
} else {
    plan tests => 8;
}

use Win::Hivex;

my $srcdir = $ENV{srcdir} || ".";
my $h = Win::Hivex->open ("$srcdir/../images/special");
ok $h, 'hive opened correctly';
my $root = $h->root;
ok $root, 'root node found';
my ($node, $value);

my @nodes = $h->node_children( $root );

($node) = grep { $h->node_name($_) eq 'abcd_äöüß' } @nodes;
ok $node, q<'abcd_äöüß' (node) has been found>;
($value) = grep { $h->value_key($_) eq 'abcd_äöüß' } $h->node_values($node);
ok $value, q<'abcd_äöüß\abcd_äöüß' (value) has been found>;

($node) = grep { $h->node_name($_) eq "zero\0key" } @nodes;
ok $node, 'key has been found';
($value) = grep { $h->value_key($_) eq "zero\0val" } $h->node_values($node);
ok $value, 'value has been found';

($node) = grep { $h->node_name($_) eq 'weird™' } @nodes;
ok $node, q<'weird™' (node) has been found>;
($value) = grep { $h->value_key($_) eq 'symbols $£₤₧€' } $h->node_values($node);
ok $value, q<'weird™\symbols $£₤₧€' (value) has been found>;
