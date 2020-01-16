# Win::Hivex::Regedit
# Copyright (C) 2009-2011 Red Hat Inc.
# Derived from code by Petter Nordahl-Hagen under a compatible license:
#   Copyright (c) 1997-2007 Petter Nordahl-Hagen.
# Derived from code by Markus Stephany under a compatible license:
#   Copyright (c)2000-2004, Markus Stephany.
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

=pod

=head1 NAME

Win::Hivex::Regedit - Helper for reading and writing regedit format files

=head1 SYNOPSIS

 use Win::Hivex;
 use Win::Hivex::Regedit qw(reg_import reg_export);

 $h = Win::Hivex->open ('SOFTWARE', write => 1);

 open FILE, "updates.reg";
 reg_import (\*FILE, $h);
 $h->commit (undef);

 reg_export ($h, "\\Microsoft\\Windows NT\\CurrentVersion", \*OUTFILE,
    prefix => "HKEY_LOCAL_MACHINE\\SOFTWARE");

=head1 DESCRIPTION

Win::Hivex::Regedit is a helper library for reading and writing the
Windows regedit (or C<.REG>) file format.  This is the textual format
that is commonly used on Windows for distributing groups of Windows
Registry changes, and this format is read and written by the
proprietary C<reg.exe> and C<regedit.exe> programs supplied with
Windows.  It is I<not> the same as the binary "hive" format which the
hivex library itself can read and write.  Note that the regedit format
is not well-specified, and hence deviations can occur between what the
Windows program can read/write and what we can read/write.  (Please
file bugs for any deviations found).

Win::Hivex::Regedit is the low-level Perl library.  There is also a
command line tool for combining hive files and reg files
(L<hivexregedit(1)>).  If you have a Windows virtual machine that you need
to merge regedit-format changes into, use the high-level
L<virt-win-reg(1)> tool (part of libguestfs tools).

=head2 FUNCTIONS

=cut

package Win::Hivex::Regedit;

use strict;
use warnings;

use Carp qw(croak carp confess);
use Encode qw(encode decode);

require Exporter;

use vars qw(@EXPORT_OK @ISA);

@ISA = qw(Exporter);
@EXPORT_OK = qw(reg_import reg_export);

=head2 reg_import

 reg_import ($fh, ($h|$map), [encoding => "UTF-16LE"]);

This function imports the registry keys from file handle C<$fh> either
into the hive C<$h> or via a map function.

The hive handle C<$h> must have been opened for writing, ie.
using the C<write =E<gt> 1> flag to C<Win::Hivex-E<gt>open>.

In the binary hive file, the first part of the key name (eg.
C<HKEY_LOCAL_MACHINE\SOFTWARE>) is not stored.  You just have to know
(somehow) that this maps to the C<SOFTWARE> hive.  Therefore if you
are given a file containing a mixture of keys that have to be added to
different hives, you have to have a way to map these to the hive
handles.  This is outside the scope of the hivex library, but if the
second argument is a CODEREF (ie. reference to a function) then this
C<$map> function is called on each key name:

 map ($keyname)
 ==> ($h, $keyname)

As shown, the function should return a pair, hive handle, and the true
key name (with the prefix stripped off).  For example:

 sub map {
   if ($_[0] =~ /^HKEY_LOCAL_MACHINE\\SOFTWARE(.*)/i) {
     return ($software_h, $1);
   } else ...
 }

C<encoding> is the encoding used by default for strings.  If not
specified, this defaults to C<"UTF-16LE">, however we highly advise
you to specify it.  See L</ENCODING STRINGS> below.

As with the regedit program, we merge the new registry keys with
existing ones, and new node values with old ones.  You can use the
C<-> (minus) character to delete individual keys and values.  This is
explained in detail in the Wikipedia page on the Windows Registry.

Remember you need to call C<$h-E<gt>commit (undef)> on the hivex
handle before any changes are written to the hive file.  See
L<hivex(3)/WRITING TO HIVE FILES>.

=cut

sub reg_import
{
    local $_;
    my $fh = shift;
    my $hmap = shift;
    my %params = @_;

    my $encoding = $params{encoding} || "utf-16le";

    my $state = "outer";
    my $newnode;
    my @newvalues;
    my @delvalues;
    my $lineno = 0;

    while (<$fh>) {
        # Join continuation lines.  This is recipe 8.1 from the Perl
        # Cookbook.  Note we allow spaces after the final \ because
        # this is fairly common in pasted regedit files.
        $lineno++;
        chomp;
        s/\r$//;
        if (s/\\\s*$//) {
            $_ .= <$fh>;
            redo unless eof ($fh);
        }

        #print STDERR "reg_import: parsing <<<$_>>>\n";

        if ($state eq "outer") {
            # Ignore blank lines, headers, comments.
            next if /^\s*(;.*)?$/;

            # .* is needed before Windows Registry Editor Version.. in
            # order to eat a possible Unicode BOM which regedit writes
            # there.
            next if /^.*Windows Registry Editor Version.*/;
            next if /^REGEDIT/;

            # Ignore comments.
            next if /^\s*;/;

            # Expect to see [...] or [-...]
            # to merge or delete a node respectively.
            if (/^\[-(.*)\]\s*$/) {
                _delete_node ($hmap, \%params, $1);
                $state = "outer";
            } elsif (/^\[(.*)\]\s*$/) {
                $state = "inner";
                $newnode = $1;
                @newvalues = ();
                @delvalues = ();
            } else {
                croak (_unexpected ($_, $lineno));
            }
        } elsif ($state eq "inner") {
            if (/^(".*)=-\s*$/) { # delete value
                my $key = _parse_quoted_string ($_);
                croak (_parse_error ($_, $lineno)) unless defined $key;
                push @delvalues, $key;
            } elsif (/^@=-\s*$/) { # delete default key
                push @delvalues, "";
            } elsif (/^".*"=/) { # ordinary value
                my $value = _parse_key_value ($_, $encoding);
                croak (_parse_error ($_, $lineno)) unless defined $value;
                push @newvalues, $value;
            } elsif (/^@=(.*)/) { # default key
                my $value = _parse_value ("", $1, $encoding);
                croak (_parse_error ($_, $lineno)) unless defined $value;
                push @newvalues, $value;
            } elsif (/^\s*(;.*)?$/) { # blank line after values
                _merge_node ($hmap, \%params, $newnode, \@newvalues, \@delvalues);
                $state = "outer";
            } else {
                croak (_unexpected ($_, $lineno));
            }
        }
    } # while

    # Still got a node left over to merge?
    if ($state eq "inner") {
        _merge_node ($hmap, \%params, $newnode, \@newvalues, \@delvalues);
    }
}

sub _parse_key_value
{
    local $_ = shift;
    my $encoding = shift;
    my $key;
    ($key, $_) = _parse_quoted_string ($_);
    return undef unless defined $key;
    return undef unless substr ($_, 0, 1) eq "=";
    return _parse_value ($key, substr ($_, 1), $encoding);
}

# Parse a double-quoted string, returning the string.  \ is used to
# escape double-quotes and other backslash characters.
#
# If called in array context and if there is anything after the quoted
# string, it is returned as the second element of the array.
#
# Returns undef if there was a parse error.
sub _parse_quoted_string
{
    local $_ = shift;

    # No initial quote character.
    return undef if substr ($_, 0, 1) ne "\"";

    my $i;
    my $out = "";
    for ($i = 1; $i < length; ++$i) {
        my $c = substr ($_, $i, 1);
        if ($c eq "\"") {
            last
        } elsif ($c eq "\\") {
            $i++;
            $c = substr ($_, $i, 1);
            $out .= $c;
        } else {
            $out .= $c;
        }
    }

    # No final quote character.
    return undef if $i == length;

    $_ = substr ($_, $i+1);
    if (wantarray) {
        return ($out, $_);
    } else {
        return $out;
    }
}

# Parse the value, optionally prefixed by a type.

sub _parse_value
{
    local $_;
    my $key = shift;
    $_ = shift;
    my $encoding = shift;       # default encoding for strings

    my $type;
    my $data;

    if (m/^dword:([[:xdigit:]]{8})$/) { # DWORD
        $type = 4;
        $data = _dword_le (hex ($1));
    } elsif (m/^hex:(.*)$/) {   # hex digits
        $type = 3;
        $data = _data_from_hex_digits ($1);
        return undef unless defined $data;
    } elsif (m/^hex\(([[:xdigit:]]+)\):(.*)$/) {   # hex digits
        $type = hex ($1);
        $data = _data_from_hex_digits ($2);
        return undef unless defined $data;
    } elsif (m/^str:(".*")$/) { # only in Wine fake-registries, I think
        $type = 1;
        $data = _parse_quoted_string ($1);
        return undef unless defined $data;
        $data .= "\0"; # Value strings are implicitly ASCIIZ.
        $data = encode ($encoding, $data);
    } elsif (m/^str\(([[:xdigit:]]+)\):(".*")$/) {
        $type = hex ($1);
        $data = _parse_quoted_string ($2);
        return undef unless defined $data;
        $data .= "\0"; # Value strings are implicitly ASCIIZ.
        $data = encode ($encoding, $data);
    } elsif (m/^(".*")$/) {
        $type = 1;
        $data = _parse_quoted_string ($1);
        return undef unless defined $data;
        $data .= "\0"; # Value strings are implicitly ASCIIZ.
        $data = encode ($encoding, $data);
    } else {
        return undef;
    }

    my %h = ( key => $key, t => $type, value => $data );
    return \%h;
}

sub _dword_le
{
    pack ("V", $_[0]);
}

sub _data_from_hex_digits
{
    local $_ = shift;
    s/[,[:space:]]//g;
    pack ("H*", $_)
}

sub _merge_node
{
    local $_;
    my $hmap = shift;
    my $params = shift;
    my $path = shift;
    my $newvalues = shift;
    my $delvalues = shift;

    my $h;
    ($h, $path) = _map_handle ($hmap, $path);

    my $node = _node_lookup ($h, $path);
    if (!defined $node) {       # Need to create this node.
        my $name = $path;
        $name = $1 if $path =~ /([^\\]+)$/;
        my $parentpath = $path;
        $parentpath =~ s/[^\\]+$//;
        my $parent = _node_lookup ($h, $parentpath);
        if (!defined $parent) {
            confess "reg_import: cannot create $path since parent $parentpath does not exist"
        }
        $node = $h->node_add_child ($parent, $name);
    }

    # Get the current set of values at this node.
    my @values = $h->node_values ($node);

    # Delete values in @delvalues original and values that are going
    # to be replaced.
    my @delvalues = @$delvalues;
    foreach (@$newvalues) {
        push @delvalues, $_->{key};
    }
    @values = grep { ! _imember ($h->value_key ($_), @delvalues) } @values;

    # Get the actual values from the hive.
    @values = map {
        my $key = $h->value_key ($_);
        my ($type, $data) = $h->value_value ($_);
        my %h = ( key => $key, t => $type, value => $data );
        $_ = \%h;
    } @values;

    # Add the new values.
    push @values, @$newvalues;

    $h->node_set_values ($node, \@values);
}

sub _delete_node
{
    local $_;
    my $hmap = shift;
    my $params = shift;
    my $path = shift;

    my $h;
    ($h, $path) = _map_handle ($hmap, $path);

    my $node = _node_lookup ($h, $path);
    # Not an error to delete a non-existant node.
    return unless defined $node;

    # However you cannot delete the root node.
    confess "reg_import: the root node of a hive cannot be deleted"
        if $node == $h->root ();

    $h->node_delete_child ($node);
}

# Call the map function, if necessary.
sub _map_handle
{
    local $_; # called function may use this
    my $hmap = shift;
    my $path = shift;
    my $h = $hmap;

    if (ref ($hmap) eq "CODE") {
        ($h, $path) = &$hmap ($path);
    }
    return ($h, $path);
}

sub _imember
{
    local $_;
    my $item = shift;

    foreach (@_) {
        return 1 if lc ($_) eq lc ($item);
    }
    return 0;
}

sub _unexpected
{
    local $_ = shift;
    my $lineno = shift;

    "reg_import: parse error: unexpected text found at line $lineno near\n$_"
}

sub _parse_error
{
    local $_ = shift;
    my $lineno = shift;

    "reg_import: parse error: at line $lineno near\n$_"
}

=head2 reg_export

 reg_export ($h, $key, $fh,
             [prefix => $prefix],
             [unsafe_printable_strings => 1]);

This function exports the registry keys starting at the root
C<$key> and recursively downwards into the file handle C<$fh>.

C<$key> is a case-insensitive path of the node to start from, relative
to the root of the hive.  It is an error if this path does not exist.
Path elements should be separated by backslash characters.

C<$prefix> is prefixed to each key name.  The usual use for this is to
make key names appear as they would on Windows.  For example the key
C<\Foo> in the SOFTWARE Registry, with $prefix
C<HKEY_LOCAL_MACHINE\SOFTWARE>, would be written as:

 [HKEY_LOCAL_MACHINE\SOFTWARE\Foo]
 "Key 1"=...
 "Key 2"=...

If C<unsafe_printable_strings> is not given or is false, then the
output is written as pure 7 bit ASCII, with line endings which are the
default for the local host.  Strings are always encoded as hex bytes.
This is safe because it preserves the original content and encoding of
strings.  See L</ENCODING STRINGS> below.

If C<unsafe_printable_strings> is true, then strings are assumed to be
UTF-16LE and are converted to UTF-8 for output.  The final zero
codepoint in the string is removed if there is one.  This is unsafe
because it does not preserve the fidelity of the strings in the
Registry and because the content type of strings is not always
UTF-16LE.  However it is useful if you just want to display strings
for quick hacking and debugging.

You may need to convert the file's encoding using L<iconv(1)> and line
endings using L<unix2dos(1)> if sending to a Windows user.

Nodes and keys are sorted alphabetically in the output.

This function does I<not> print a header.  The real regedit program
will print a header like:

 Windows Registry Editor Version 5.00

followed by a blank line.  (Other headers are possible, see the
Wikipedia page on the Windows Registry).  If you want a header, you
need to write it out yourself.

=cut

sub reg_export
{
    my $h = shift;
    my $key = shift;
    my $fh = shift;

    my $node = _node_lookup ($h, $key);
    croak "$key: path not found in this hive" unless $node;

    reg_export_node ($h, $node, $fh, 0, @_);
}

=head2 reg_export_node

 reg_export_node ($h, $node, $fh, ...);

This is exactly the same as L</reg_export> except that instead
of specifying the path to a key as a string, you pass a hivex
library C<$node> handle.

=cut

sub reg_export_node
{
    local $_;
    my $h = shift;
    my $node = shift;
    my $fh = shift;
    my $depth = shift;
    my %params = @_;

    my $max_depth = $params{max_depth};
    if (defined $max_depth && $max_depth >= 0) {
        # Check if we've gone deep enough
        if ($depth >= $max_depth) {
            return;
        }
    }

    confess "reg_export_node: \$node parameter was undef" unless defined $node;

    # Get the canonical path of this node.
    my $path = _node_canonical_path ($h, $node);

    # Print the path.
    print $fh "[";
    my $prefix = $params{prefix};
    if (defined $prefix) {
        chop $prefix if substr ($prefix, -1, 1) eq "\\";
        print $fh $prefix;
    }
    print $fh $path;
    print $fh "]\n";

    my $unsafe_printable_strings = $params{unsafe_printable_strings};
    my $unsafe = $params{unsafe};

    my @values;
    my @safe_values;

    # Get the values.
    if ($unsafe) {
        my $have_vals = 0;
        eval {
            @values = $h->node_values ($node);
            $have_vals = 1;
        };

        if (!$have_vals) {
            carp "Failed to read node values at $path";
        }
    } else {
        @values = $h->node_values ($node);
    }

    foreach (@values) {
        use bytes;

        my $key = $h->value_key ($_);
        my ($type, $data);

        if ($unsafe) {
            my $val_ok = 0;
            eval {
                ($type, $data) = $h->value_value ($_);
                $val_ok = 1;
            };

            if (!$val_ok) {
                carp "skipping unreadable value of key: $key in $path";
                next;
            }
        } else {
            ($type, $data) = $h->value_value ($_);
        }

        push @safe_values, { key => $key, type => $type, data => $data };
    }

    @values = sort { $a->{key} cmp $b->{key} } @safe_values;

    # Print the values.
    foreach (@values) {
        my $key = $_->{key};
        my $type = $_->{type};
        my $data = $_->{data};

        if ($key eq "") {
            print $fh '@='    # default key
        } else {
            print $fh '"', _escape_quotes ($key), '"='
        }

        if ($type eq 4 && length ($data) == 4) { # only handle dword specially
            my $dword = unpack ("V", $data);
            printf $fh "dword:%08x\n", $dword
        } elsif ($unsafe_printable_strings && ($type eq 1 || $type eq 2)) {
            # Guess that the encoding is UTF-16LE.  Convert it to UTF-8
            # for printing.
            $data = decode ("utf16le", $data);
            $data =~ s/\x{0}$//; # remove final zero codepoint
            $data =~ s/"/\\"/g; # XXX more quoting needed?
            printf $fh "str(%x):\"%s\"\n", $type, $data;
        } else {
            # Encode everything else as hex, see encoding section below.
            printf $fh "hex(%x):", $type;
            my $hex = join (",", map { sprintf "%02x", ord } split (//, $data));
            print $fh "$hex\n"
        }
    }
    print $fh "\n";

    my @children;

    if ($unsafe) {
        my $have_children = 0;
        eval {
            @children = $h->node_children ($node);
            $have_children = 1;
        };

        if (!$have_children) {
            carp "Could not get children of $path";
        }
    } else {
        @children = $h->node_children ($node);
    }

    @children = sort { $h->node_name ($a) cmp $h->node_name ($b) } @children;
    reg_export_node ($h, $_, $fh, $depth + 1, @_) foreach @children;
}

# Escape " and \ when printing keys.
sub _escape_quotes
{
    local $_ = shift;
    s/\\/\\\\/g;
    s/"/\\"/g;
    $_;
}

# Look up a node in the registry starting from the path.
# Return undef if it doesn't exist.

sub _node_lookup
{
    local $_;
    my $h = shift;
    my $path = shift;

    my @path = split /\\/, $path;
    shift @path if @path > 0 && $path[0] eq "";

    my $node = $h->root ();
    foreach (@path) {
        $node = $h->node_get_child ($node, $_);
        return undef unless defined $node;
    }

    return $node;
}

# Return the canonical path of node in the hive.

sub _node_canonical_path
{
    local $_;
    my $h = shift;
    my $node = shift;

    return "\\" if $node == $h->root ();
    $_ = $h->node_name ($node);
    my $parent = $h->node_parent ($node);
    my $path = _node_canonical_path ($h, $parent);
    if ($path eq "\\") {
        return "$path$_"
    } else {
        return "$path\\$_"
    }
}

=head1 ENCODING STRINGS

The situation with encoding strings in the Registry on Windows is very
confused.  There are two main encodings that you would find in the
binary (hive) file, 7 bit ASCII and UTF-16LE.  (Other encodings are
possible, it's also possible to have arbitrary binary data incorrectly
marked with a string type).

The hive file itself doesn't contain any indication of string
encoding.  Windows probably guesses the encoding.

We think that regedit probably either guesses which encoding to use
based on the file encoding, or else has different defaults for
different versions of Windows.  Neither choice is appropriate for a
tool used in a real operating system.

When using L</reg_import>, you should specify the default encoding for
strings using the C<encoding> parameter.  If not specified, it
defaults to UTF-16LE.

The file itself that is imported should be in the local encoding for
files (usually UTF-8 on modern Linux systems).  This means if you
receive a regedit file from a Windows system, you may sometimes have
to reencode it:

 iconv -f utf-16le -t utf-8 < input.reg | dos2unix > output.reg

When writing regedit files (L</reg_export>) we bypass this madness
completely.  I<All> strings (even pure ASCII) are written as hex bytes
so there is no doubt about how they should be encoded when they are
read back in.

=cut

1;

=head1 COPYRIGHT

Copyright (C) 2010-2011 Red Hat Inc.

=head1 LICENSE

Please see the file COPYING.LIB for the full license.

=head1 SEE ALSO

L<Win::Hivex(3)>,
L<hivexregedit(1)>,
L<virt-win-reg(1)>,
L<iconv(1)>,
L<dos2unix(1)>,
L<unix2dos(1)>,
L<hivex(3)>,
L<hivexsh(1)>,
L<http://libguestfs.org>,
L<Sys::Guestfs(3)>.

=cut
