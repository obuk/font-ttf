package Font::TTF::CFF_;

=head1 NAME

Font::TTF::CFF_ - Compact Font Format

=head1 DESCRIPTION

=cut

use strict;
use vars qw(@ISA);

use Font::TTF::Table;
use Font::TTF::Utils;
use List::Util qw(sum max);
use POSIX qw/ceil/;
#use Clone 'clone';
#use File::Temp qw/tempfile/;

@ISA = qw(Font::TTF::Table);

use Carp;
#use feature 'say';
use Data::Dumper qw/Dumper/;
$Data::Dumper::Indent = 1;
$Data::Dumper::Terse = 1;
#use Data::HexDump;

use constant {
    T_SID     => 0,
    T_boolean => 1,
    T_number  => 2,
    T_offset  => 3,
    T_size_and_offset => 4,
    T_ROS     => 5,
    T_delta   => 6,
    T_array   => 7,
};


# Appendix H - CFF DICT Encoding
# One-byte CFF DICT Operators
use constant {
    D_version          =>  0,
    D_Notice           =>  1,
    D_FullName         =>  2,
    D_FamilyName       =>  3,
    D_Weight           =>  4,
    D_FontBBox         =>  5,
    D_BlueValues       =>  6,
    D_OtherBlues       =>  7,
    D_FamilyBlues      =>  8,
    D_FamilyOtherBlues =>  9,
    D_StdHW            => 10,
    D_StdVW            => 11,
    D_escape           => 12, # [1] First byte of a 2-byte operator.
    D_UniqueID         => 13,
    D_XUID             => 14,
    D_charset          => 15,
    D_Encoding         => 16,
    D_CharStrings      => 17,
    D_Private          => 18,
    D_Subrs            => 19,
    D_defaultWidthX    => 20,
    D_nominalWidthX    => 21,
    # -Reserved-       => 22 .. 27,
    D_shortint         => 28, # [2] First byte of a 3-byte sequence specifying a number.
    D_longint          => 29,
    D_BCD              => 30,
    # -Reserved-       => 31,
    # <numbers>        => 32 .. 246,
    # <numbers>        => 247 .. 254, [3] First byte of a 2-byte sequence specifying a number.
    # -Reserved-       => 255,
};

# Two-byte CFF DICT Operators
use constant {
    D_Copyright          =>  0,
    D_isFixedPitch       =>  1,
    D_ItalicAngle        =>  2,
    D_UnderlinePosition  =>  3,
    D_UnderlineThickness =>  4,
    D_PaintType          =>  5,
    D_CharstringType     =>  6,
    D_FontMatrix         =>  7,
    D_StrokeWidth        =>  8,
    D_BlueScale          =>  9,
    D_BlueShift          => 10,
    D_BlueFuzz           => 11,
    D_StemSnapH          => 12,
    D_StemSnapV          => 13,
    D_ForceBold          => 14,
    # -Reserved-         => 15 .. 16,
    D_LanguageGroup      => 17,
    D_ExpansionFactor    => 18,
    D_initialRandomSeed  => 19,
    D_SyntheticBase      => 20,
    D_PostScript         => 21,
    D_BaseFontName       => 22,
    D_BaseFontBlend      => 23,
    # -Reserved-         => 24 .. 29,
    D_ROS                => 30,
    D_CIDFontVersion     => 31,
    D_CIDFontRevision    => 32,
    D_CIDFontType        => 33,
    D_CIDCount           => 34,
    D_UIDBase            => 35,
    D_FDArray            => 36,
    D_FDSelect           => 37,
    D_FontName           => 38,
    # -Reserved-         => 39 .. 255,
};

sub verbose { $_[0]->{verbose} = $_[1] if defined $_[1]; $_[0]->{verbose} }
sub debug   { $_[0]->{debug}   = $_[1] if defined $_[1]; $_[0]->{debug}   }

sub read {
    my $self = shift;
    $self->SUPER::read or return $self;

    # Table 1 - CFF Data Layout
    $self->Header;
    $self->Name_INDEX;
    $self->TopDICT_INDEX;
    $self->String_INDEX;
    $self->GlobalSubr_INDEX;

    $self;
}


=begin comment

Table 2 -  CFF Data Types

Name	Range		Description
Card8	0..255		1-byte unsigned number
Card16	0..65535	2-byte unsigned number
Offset	varies		1, 2, 3, or 4 byte offset (specified by OffSize field)
OffSize	1..4		1-byte unsigned number specifies the size of an Offset field or fields
SID	0..64999	2-byte string identifier

=end comment

=cut


# Table 3
sub decode_number {
    my $data = shift;
    my $b0 = shift @{$data};
    if ($b0 >= 32 && $b0 <= 246) {
	return $b0 - 139;
    } elsif ($b0 >= 247 && $b0 <= 250) {
	my $b1 = shift @{$data};
	return ($b0 - 247) * 256 + $b1 + 108;
    } elsif ($b0 >= 251 && $b0 <= 254) {
	my $b1 = shift @{$data};
	return -($b0 - 251) * 256 - $b1 - 108
    } elsif ($b0 == 255) {
	my ($b1, $b2, $b3, $b4) = splice @{$data}, 0, 4;
	my $int = $b1 << 24 | $b2 << 16 | $b3 << 8 | $b4;
	$int = -((~$int & 0xffff_ffff) + 1) if $int & 0x8000_0000;
	return $int / 0x1_0000 / 0x1_0000;
    } elsif ($b0 == D_shortint) {
	my ($b1, $b2) = splice @{$data}, 0, 2;
	my $int = $b1 << 8 | $b2;
	$int = -((~$int & 0xffff) + 1) if $int & 0x8000;
	return $int;
    } elsif ($b0 == D_longint) {
	my ($b1, $b2, $b3, $b4) = splice @{$data}, 0, 4;
	my $int = $b1 << 24 | $b2 << 16 | $b3 << 8 | $b4;
	$int = -((~$int & 0xffff_ffff) + 1) if $int & 0x8000_0000;
	return $int;
#    } elsif ($b0 == D_BCD) {
#	return decode_BCD($data);
    } else {
	unshift @{$data}, $b0;
	return undef;
    }
}



sub encode_number {
    my $data = shift;
    croak "no data" unless defined $data; # xxxxxx
    if ($data =~ /E|[.]/) {
	return encode_BCD($data);
    } elsif ($data >= -107 && $data <= 107) {
	return pack "C*", $data + 139;
    } elsif ($data >= 108 && $data <= 1131) {
	my $b1 = ($data - 108) % 256;
	my $b0 = int(($data - $b1 - 108) / 256) + 247;
	return pack "C*", $b0, $b1;
    } elsif ($data >= -1131 && $data <= -108) {
	my $b1 = (-$data - 108) % 256;
	my $b0 = int((-$data - $b1 - 108) / 256) + 251;
	return pack "C*", $b0, $b1;
    } elsif ($data >= -32768 && $data <= 32767) {
	my $b1 = $data & 0xff;
	my $b0 = ($data >> 8) & 0xff;
	return pack "C*", D_shortint, $b0, $b1;
    } else {
	my $b3 = $data & 0xff;
	my $b2 = ($data >> 8) & 0xff;
	my $b1 = ($data >> 16) & 0xff;
	my $b0 = ($data >> 24) & 0xff;
	return pack "C*", D_longint, $b0, $b1, $b2, $b3;
    }
}


sub test_number_encoding {

    # Table 4 - Integer Format Examples
    my @ints = (
	0 => [0x8b],
	100 => [0xef],
	-100 => [0x27],
	1000 => [0xfa, 0x7c],
	-1000 => [0xfe, 0x7c],
	10000 => [0x1c, 0x27, 0x10],
	-10000 => [0x1c, 0xd8, 0xf0],
	100000 => [0x1d, 0x00, 0x01, 0x86, 0xa0],
	-100000 => [0x1d, 0xff, 0xfe, 0x79, 0x60],
    );
    my @floats = (
	'-2.25' => [0x1e, 0xe2, 0xa2, 0x5f],
	'0.140541E-3' => [0x1e, 0x0a, 0x14, 0x05, 0x41, 0xc3, 0xff],
    );

    while (my ($v, $e) = splice @ints, 0, 2) {
	my $h1 = join ', ', map sprintf("0x%02x", $_), @$e;
	my $e1 = pack "C*", @$e;
	my $d = decode_number($e) // decode_BCD($e);
	croak "could not decode_number [ $h1 ] => $d" unless $d == $v;
	my $r1 = join ', ', map sprintf("0x%02x", $_), @$e;
	croak "could not finish decode_number [ $h1 ] => [ $r1 ]" unless @{$e} == 0;
	my $e2 = encode_number($v);
	my $h2 = join ', ', map sprintf("0x%02x", $_), unpack "C*", $e2;
	print STDERR "encode_number($v) => [ $h2 ]\n";
	croak "could not encode_number $v => [ $h2 ]" unless $e1 eq $e2;
    }
    while (my ($v, $e) = splice @floats, 0, 2) {
	my $h1 = join ', ', map sprintf("0x%02x", $_), @$e;
	my $e1 = pack "C*", @$e;
	my $d = decode_number($e) // decode_BCD($e);
	croak "could not decode_number [ $h1 ] => $d" unless $d eq $v;
	my $r1 = join ', ', map sprintf("0x%02x", $_), @$e;
	croak "could not finish decode_number [ $h1 ] => [ $r1 ]" unless @{$e} == 0;
	my $e2 = encode_number($v);
	my $h2 = join ', ', map sprintf("0x%02x", $_), unpack "C*", $e2;
	print STDERR "encode_number($v) => [ $h2 ]\n";
	croak "could not encode_number $v => [ $h2 ]" unless $e1 eq $e2;
    }

    0;
}


# Table 5 - Nibble Definitions
sub decode_BCD {
    my $data = shift;
    my $f = '';
    if ($data->[0] == 30) {
	shift @{$data};
    } else {
	return undef;
    }
    while (1) {
	my $b = shift @{$data};
	last unless defined $b;
	for ($b >> 4, $b & 15) {
	    $f .= chr(ord('0') + $_), next if $_ >= 0 && $_ <= 9;
	    $f .= '.', next                if $_ == 0x0a;
	    $f .= 'E', next                if $_ == 0x0b;
	    $f .= 'E-', next               if $_ == 0x0c;
	    $f = '-'.$f, next              if $_ == 0x0e;
	    return $f                      if $_ == 0x0f;
	    croak sprintf "decode_BCD_operand: could not decode; nibble 0x%0x.\n", $_;
	}
    }
    die "decode_BCD_operand: missing end of number.\n";
}


sub encode_BCD {
    my $data = shift;
    local $_ = "$data";
    s/E-/\xC/;
    y/[0-9\.E\-]/[\x00-\x09\x0A\x0B\x0E]/;
    my @bcd = unpack "C*";
    push @bcd, 0xf;
    push @bcd, 0xf if @bcd & 1;
    my $bcd;
    while (my ($hi, $lo) = splice(@bcd, 0, 2)) {
	$bcd .= chr($hi << 4 | $lo);
    }
    chr(D_BCD) . $bcd;
}


=begin comment

# Table 6 - Operand Types

Type	Description
number	Integer or real number
boolean	Integer type with the values 0 (false) and 1 (true)
SID	String id (see section 10)
array	One or more numbers
delta	A number or a delta-encoded array of numbers (see below)


# Table 7 - INDEX Format

Type	Name	Description
Card16	count	Number of objects stored in INDEX
OffSize	offSize	Offset array element size
Offset	offset [count+1]	Offset array (from byte preceding object data)
Card8	data [<varies>]	Object data

=end comment

=cut

sub read_INDEX {
    my $self = shift;
    my $pos = shift;

    my ($dat);
    my ($fh) = $self->{' INFILE'};
    $fh->seek($self->{' OFFSET'} + $pos, 0) if defined $pos;
    $fh->read($dat, 3);
    my ($count, $offSize) = unpack "nC", $dat;
    return { count => $count, offSize => $offSize, offset => [] } unless $count;
    my @offset;
    $fh->read($dat, $offSize * ($count + 1));
    my %u = (1 => "C*", 2 => "n*", 4 => "N*");
    if (my $u = $u{$offSize}) {
        push @offset, unpack $u, $dat;
    } elsif ($offSize == 3) {
        my @b = unpack "C*", $dat;
        for (0 .. $count) {
            push @offset, $b[0] << 16 | $b[1] << 8 | $b[2];
            splice @b, 0, 3;
        }
    } else {
        croak "offSize $offSize is not in [ 1 .. 4 ]";
        my @b = unpack "C*", $dat;
        for (0 .. $count) {
            my $offset = 0;
            for (1 .. $offSize) {
                $offset <<= 8;
                $offset += shift @b;
            }
            push @offset, $offset;
        }
    }

    croak "INDEX: must have at least 2 offsets." unless @offset >= 2;
    croak "INDEX: The first offset must be 1." unless $offset[0] == 1;

    $fh->read($dat, $offset[-1] - $offset[0]);

    # Offset	offset [count+1]	Offset array (from byte preceding object data)
    # Card8	data [<varies>]	Object data
    my $index = {
        count => $count,
        offSize => $offSize,
        offset => \@offset,
        data => $dat,
    };

    $index;
}


sub pack_INDEX {
    my ($self, $index) = @_;

    my %u = (1 => "C*", 2 => "n*", 4 => "N*");
    if (my $u = $u{$index->{offSize}}) {
        my $h =  pack "n1C1$u",
            $index->{count},
            $index->{offSize},
            @{$index->{offset}};
        return $h . $index->{data};
    } elsif ($index->{offSize} == 3) {
        my $h = pack "n1C1(CCC)*",
            $index->{count},
            $index->{offSize},
            map { ($_ >> 16) & 0xff, ($_ >> 8) & 0xff, ($_) & 0xff }
            @{$index->{offset}};
        return $h . $index->{data};
    } else {
        croak "offSize $index->{offSize} is not in [ 1 .. 4 ]";
    }

    undef;
}


=begin comment

# Table 8 - Header Format

Type	Name	Description
Card8	major	Format major version (starting at 1)
Card8	minor	Format minor version (starting at 0)
Card8	hdrSize	Header size (bytes)
OffSize	offSize	Absolute offset (0) size

=end comment

=cut

sub Header {
    my $self = shift;
    $self->{Header} //= $self->read_Header;
}


sub read_Header {
    my $self = shift;
    my ($dat, $header);
    my ($fh) = $self->{' INFILE'};
    $fh->read($dat, 4);
    @{$header}{qw/minor major hdrSize offSize/} = unpack "CCCC", $dat;
    $header;
}


sub pack_Header {
    my ($self, $header) = @_;
    $header //= $self->Header;
    pack "C4", @{$header}{qw/minor major hdrSize offSize/};
}


sub Name_INDEX {
    my $self = shift;
    $self->{Name_Index} //= $self->read_INDEX;
}


sub TopDICT_INDEX {
    my ($self, $index) = @_;
    $self->{TopDICT_Index} = $index if defined $index;
    $self->{TopDICT_Index} //= $self->read_INDEX;
}


# Table 9 - Top DICT Operator Entries

sub TopDICT_Entries {
    my $self = shift;
    $self->{TopDICT_Entries} //= $self->get_TopDICT_Entries;
}

sub get_TopDICT_Entries {
    my (%e, %e2);

    #   [ Name Operand(s) Default notes ],
    for (
	[ version => T_SID, undef, 'FontInfo' ],
        [ Notice => T_SID, undef, 'FontInfo' ],
	[ FullName => T_SID, undef, 'FontInfo' ],
        [ FamilyName => T_SID, undef, 'FontInfo' ],
	[ Weight => T_SID, undef, 'FontInfo' ],
	[ UniqueID => T_number, undef, ],
	[ FontBBox => T_array, [ 0, 0, 0, 0 ] ],
	[ XUID => T_array,  undef, ],
	[ charset => T_offset,  0, 'charset offset (0)' ],
	[ Encoding => T_offset,  0, 'encoding offset (0)' ],
	[ CharStrings => T_offset, undef, 'CharStrings offset (0)' ],
        [ Private => T_size_and_offset, undef, 'Private DICT size and offset (0)' ],
    ) {
        my $v = eval "D_$_->[0]";
        croak "D_$_->[0] is not defined" unless defined $v;
        $e{$v} = $_;
    }

    for (
        [ Copyright => T_SID, undef, 'FontInfo' ],
        [ isFixedPitch => T_boolean, 0, 'FontInfo' ],
        [ ItalicAngle => T_number, 0, 'FontInfo' ],
        [ UnderlinePosition => T_number, -100, 'FontInfo' ],
        [ UnderlineThickness => T_number,  50, 'FontInfo' ],
        [ PaintType => T_number, 0, ],
        [ CharstringType => T_number, 2, ],
        [ FontMatrix => T_array, [ 0.001, 0, 0, 0.001, 0, 0 ] ],
        [ StrokeWidth => T_number, 0, ],
        [ SyntheticBase => T_number, undef, 'synthetic base font index', ],
        [ PostScript => T_SID, undef, 'embedded PostScript language code' ],
        [ BaseFontName => T_SID, undef, '(added as needed by Adobe-based technology)' ],
        [ BaseFontBlend => T_delta, undef, '(added as needed by Adobe-based technology)' ],
    ) {
        my $v = eval "D_$_->[0]";
        croak "D_$_->[0] is not defined" unless defined $v;
        $e2{$v} = $_;
    }

    $e{eval D_escape} = \%e2;
    \%e;
}


# Table 10
sub CIDFont_Operator_Extensions
{
    my $self = shift;
    $self->{CIDFont_Operator_Extensions} //= $self->get_CIDFont_Operator_Extensions;
}

sub get_CIDFont_Operator_Extensions
{
    my (%e, %e2);

    #   [ Name Operand(s) Default notes ],
    for (
        [ ROS => T_ROS, undef, 'Registry Ordering Supplement' ],
        [ CIDFontVersion => T_number, 0, ],
        [ CIDFontRevision => T_number, 0, ],
        [ CIDFontType => T_number, 0, ],
        [ CIDCount => T_number, 8720, ],
        [ UIDBase => T_number, undef, ],
        [ FDArray => T_offset, undef, 'Font DICT (FD) INDEX offset (0)' ],
        [ FDSelect => T_offset, undef, 'FDSelect offset (0)' ],
        [ FontName => T_SID, undef, 'FD FontName' ],
    ) {
        my $v = eval "D_$_->[0]";
        croak "D_$_->[0] is not defined" unless defined $v;
        $e2{$v} = $_;
    }

    $e{eval D_escape} = \%e2;
    \%e;
}


sub TopDICT {
    my $self = shift;
    return $self->{TopDICT} //= $self->get_TopDICT;
}


sub get_TopDICT {
    my $self = shift;
    my $index = shift // $self->TopDICT_INDEX;
    my @data = unpack "C*", $index->{data};
    $self->DICT_Data('TopDICT', $self->DICT_operators, \@data);
}


sub DICT_Data {
    my $self = shift;
    my $dict_name = shift;
    my $entries = shift;
    my $data = shift;

    my $DICT = $self->DICT_defaults($entries);
    while (1) {
	last if @{$data} == 0;
        my @last_data = @{$data};
	my (@v, $ent);
        my ($k, $k2);
	while (@$data) {
	    my $v = decode_number($data) // decode_BCD($data);
	    if (defined $v) {
		push @v, $v;
	    } else {
                $k = shift @{$data};
		$ent = $entries->{$k};
                croak Dumper({ '$k' => $k }) unless ref $ent;
		if (ref $ent eq 'HASH') {
                    $k2 = shift @{$data};
		    $ent = $ent->{$k2};
                    croak Dumper({ '$k' => $k, '$k2' => $k2 }) unless ref $ent;
		    die "$dict_name: @v, $k, $k2" unless defined $ent;
		}
		last;
	    }
	}

	my ($name, $operand, $default, $note) = @{$ent};
        print STDERR "$dict_name: $name, @v = ",
            "[ ", join(', ', map sprintf("0x%02x", $_),
                       @last_data[0..($#last_data - @{$data})]), " ]\n"
            if $self->verbose >= 2; # xxxx
	if ($operand == T_SID) {
            $DICT->{$name} = $self->toString($v[0]) // $v[0];
            $DICT->{"_$name"} = $v[0];
	} elsif ($operand == T_number ||
                 $operand == T_offset ||
                 $operand == T_boolean) {
	    $DICT->{$name} = $v[0];
	} elsif ($operand == T_delta) {
	    $DICT->{$name} = [ decode_delta(@v) ];
	} elsif ($operand == T_array ||
                 $operand == T_size_and_offset) {
	    $DICT->{$name} = \@v;
	} elsif ($operand == T_ROS) {
            $DICT->{$name} = [
                $self->toString($v[0]) // $v[0],
                $self->toString($v[1]) // $v[1],
                $v[2],
            ];
            $DICT->{"_$name"} = \@v;
	} else {
	    croak "$dict_name: unknown operand #$operand\n";
	}
    }
    $DICT;
}


sub decode_delta {
    my @list = shift;
    push @list, $list[-1] + $_ for @_;
    @list;
}


sub DICT_defaults {
    my $self = shift;
    my $entries = shift;
    my $defaults;
    for my $i (keys %{$entries}) {
	my $ent = $entries->{$i};
	if (ref $ent eq 'HASH') {
	    for my $j (keys %{$ent}) {
		my ($name, $operand, $default, $note) = @{$ent->{$j}};
		$defaults->{$name} = $default;
	    }
	} else {
	    my ($name, $operand, $default, $note) = @{$ent};
	    $defaults->{$name} = $default;
	}
    }
    $defaults;
}


sub make_INDEX {
    my $self = shift;

    my @offset = (1);
    my $data = '';
    for (@_) {
        $data .= $_;
        push @offset, $offset[-1] + length;
    }

    # my $offSize = 2;
    my $offSize = ceil(log($offset[-1]) / log(256));

    {
	count => scalar(@offset) - 1,
	offSize => $offSize,
	offset => \@offset,
	data => $data,
    };
}


sub DICT_name2value {
    my $self = shift;
    $self->{DICT_name2value} //= $self->get_DICT_name2value;
}


sub get_DICT_name2value {
    my $self = shift;

    my $escape = D_escape;
    my $op = $self->DICT_operators;

    my $name2value;
    for (keys %$op) {
        if ($_ == $escape) {
            for (keys %{$op->{$escape}}) {
                my ($name, $operand, $default, $note) = @{$op->{$escape}{$_}};
                $name2value->{$name} = [ $escape, $_ ];
            }
        } else {
            my ($name, $operand, $default, $note) = @{$op->{$_}};
            $name2value->{$name} = [ $_ ];
        }
    }

    $name2value;
}


sub DICT_operators {
    my $self = shift;
    $self->{DICT_operators} //= $self->get_DICT_operators;
}


sub get_DICT_operators {
    my $self = shift;

    my $escape = D_escape;
    my %operators = (
        %{$self->TopDICT_Entries},
        %{$self->CIDFont_Operator_Extensions},
        %{$self->PrivateDICT_Operators},
        ($escape => {
            %{$self->TopDICT_Entries->{$escape}},
            %{$self->CIDFont_Operator_Extensions->{$escape}},
            %{$self->PrivateDICT_Operators->{$escape}},
        }),
    );

    \%operators;
}


sub pack_DICT {
    my $self = shift;
    my $dict_name = shift;
    my $dict = shift;

    my $operators = $self->DICT_operators;
    my $name2value = $self->DICT_name2value;

    my $fixes;
    my $data = '';
    for (sort { $a->[0] <=> $b->[0] || ($a->[1] // '') <=> ($b->[1] // '') }
         map $name2value->{$_} // (), keys %$dict) {
        my $ent =
            ref && @$_ == 1 ? $operators->{$_->[0]} :
            ref && @$_ == 2 ? $operators->{$_->[0]}{$_->[1]} :
            undef;
        if (my $packed = $self->pack_dict_item($dict_name, $dict, $ent)) {
            $packed .= chr($_) for @$_;
            $fixes->{$ent->[0]} = [
                length $data,
                length $packed,
                #$ent->[1],
            ];
            $data .= $packed;
        }
    }
    wantarray ? ($data, $fixes) : $data;
}


sub pack_dict_item {
    my ($self, $dict_name, $dict, $ent) = @_;
    return undef unless $ent;
    my ($name, $operand, $default, $note) = @{$ent};
    my $v = $dict->{$name};
    my $_v = $dict->{"_$name"};

    my $s;
    if ($self->verbose >= 2) {
        ($s = Dumper($v)) =~ s/\s+/ /g;
        print STDERR "pack $dict_name: $name: $s\n" if $self->verbose == 3;
    }
    return undef unless defined $v;
    return undef if ref $v eq 'ARRAY' && @$v == 0;
    #return undef if defined $v && defined $default && "$v" eq "$default";

    if ($operand == T_SID) {
        my $v = $_v // $self->toSID($v);
        return undef if defined $default && $v == $default;
        print STDERR "pack $dict_name: $name: $s\n" if $self->verbose == 2;
        return encode_number($v);
    } elsif ($operand == T_number ||
             $operand == T_boolean) {
        return undef if defined $default && $v == $default;
        print STDERR "pack $dict_name: $name: $s\n" if $self->verbose == 2;
	return encode_number($v);
    } elsif ($operand == T_delta) {
        return undef if cmp_array($v, $default) == 0;
        print STDERR "pack $dict_name: $name: $s\n" if $self->verbose == 2;
        return join '', encode_delta(@$v);
    } elsif ($operand == T_array) {
        return undef if cmp_array($v, $default) == 0;
        print STDERR "pack $dict_name: $name: $s\n" if $self->verbose == 2;
	return join '', encode_array(@$v);
    } elsif ($operand == T_ROS) {
        my $v = $_v // [map +($self->toSID($_->[0]), $self->toSID($_->[1]), $_->[2]), $v ];
        return undef if cmp_array($v, $default) == 0;
        print STDERR "pack $dict_name: $name: $s\n" if $self->verbose == 2;
	return join '', encode_array(@$v);
    } elsif ($operand == T_offset) {
        return undef if defined $default && $v == $default;
        print STDERR "pack $dict_name: $name: $s\n" if $self->verbose == 2;
        return encode_offset($self->Header->{offSize}, $v);
    } elsif ($operand == T_size_and_offset) {
        return undef if cmp_array($v, $default) == 0;
        print STDERR "pack $dict_name: $name: $s\n" if $self->verbose == 2;
        my ($size, $offset) = @$v;
        return join '', encode_number($size),
            encode_offset($self->Header->{offSize}, $offset);
    } else {
	croak "pack $dict_name: unknown operand #$operand\n";
    }
    undef;
}


sub encode_offset {
    my ($size, $data) = @_;
    if ($size <= 3 && $data <= 0xFFFF) {
        my $b1 = $data & 0xff;
        my $b0 = ($data >> 8) & 0xff;
        return pack "C*", D_shortint, $b0, $b1;
    } else {
        my $b3 = $data & 0xff;
        my $b2 = ($data >> 8) & 0xff;
        my $b1 = ($data >> 16) & 0xff;
        my $b0 = ($data >> 24) & 0xff;
        return pack "C*", D_longint, $b0, $b1, $b2, $b3;
    }
}


sub String_INDEX {
    my $self = shift;
    $self->{TopDICT} = undef;
    $self->{String_Index} //= $self->read_INDEX;
}


sub toString {
    my $self = shift;
    return undef unless defined(my $sid = shift);
    my $nStandardStrings = @{$self->StandardStrings};
    return $self->StandardStrings->[$sid] if $sid < $nStandardStrings;
    $self->{String} //= $self->String_INDEX;
    my $i = $sid - $nStandardStrings;
    return undef if $i >= $self->{String}->{count};
    my $j0 = $self->{String}->{offset}->[$i];
    my $j1 = $self->{String}->{offset}->[$i + 1];
    my $start = $j0 - 1; # -1 is because array indices start at 1 in CFF.
    substr($self->{String}->{data}, $start, $j1 - $j0);
}


sub GlobalSubr_INDEX {
    my $self = shift;
    $self->{GlobalSubr_Index} //= $self->read_INDEX;
}


# Table 11, 12, 13, 14, 15, 16
sub Encoding {
    my $self = shift;
    $self->{Encoding} //= $self->read_Encoding;
}


sub read_Encoding {
    my $self = shift;

    my $pos = $self->TopDICT->{Encoding};

    # An encoding is an array of codes associated with glyphs in a font, and
    # a charset is an array of "names" for all glyphs in the font.
    # (In CFF these names are actually SIDs or CIDs, which must be unique.)

    my $encoding;

    # Table 16
    if ($pos == 0) {
	$encoding = {
	    format => undef,
	    Id => $pos,
	    code => $self->StandardEncoding,
	};
    } elsif ($pos == 1) {
	$encoding = {
	    format => undef,
	    Id => $pos,
	    code => $self->ExpertEncoding,
	};
    } elsif (($pos & ~3) == 0) {
	return undef;
    } else {
	my @code = (0); # as SID or CID
        my ($dat);
        my ($fh) = $self->{' INFILE'};
	$fh->seek($self->{' OFFSET'} + $pos, 0) if defined $pos;
        $fh->read($dat, 2);
	my ($format, $n) = unpack "CC", $dat;
	my $mask = 15;
	my @dump = ();
	if (($format & $mask) == 0) {
	    # Table 11
	    my $nCodes = $n;
            $fh->read($dat, $n);
	    push @code, unpack "C*", $dat;
	    $encoding = {
		format => $format,
		nCodes => $n,
		code => \@code,
	    };
	} elsif (($format & $mask) == 1) {
	    # Table 12
	    my $nRanges = $n;

	    # Table 13 - first, nLeft
            $fh->read($dat, 2 * $n);
	    my @Range = unpack "CC" x $n, $dat;
	    while (my ($first, $nLeft) = splice @Range, 0, 2) {
		push @code, $first;
		push @code, map $first + $_, 1 .. $nLeft if $nLeft >= 1;
	    }
	    $encoding = {
		format => $format,
		nRanges => $n, Ranges => \@Range,
		code => \@code,
	    };
	} else {
	    croak "Encoding: unknown format $format";
	}

	# A few fonts have multiply-encoded glyphs which are not
	# supported directly by any of the above formats (Table 11,
	# 12, 13). This situation is indicated by setting the
	# high-order bit in the format byte and supplementing the
	# encoding, regardless of format type, as shown in #Table 14.

	# high-order bit は、何ビット目？

	if ($format & ~$mask) {
	    # Table 14
            $fh->read($dat, 1);
	    my $nSups = unpack "C", $dat;

	    # Table 15 - code, glyph
	    croak "encoding is null" unless $encoding;
	    $encoding->{nSups} = $nSups;
            $fh->read($dat, 3 * $nSups);
	    my @Supplement = unpack "Cn" x $nSups, $dat;
	    while (my ($code, $glyph) = splice @Supplement, 0, 2) {
		$encoding->{code}->[$glyph] = $code;
	    }
	}
    }

    $encoding;
}


sub pack_Encoding {
    my $self = shift;
    my $raw;
    for ($self->Encoding) {
        if (defined $_->{Id}) {
            $raw = $_->{Id};
        } else {
            my @range = encode_seqlen($_->{code});
            if (@range * 2 >= @{$_->{code}}) {
                # format 0
                $raw .= pack "CCC*", 0, scalar @{$_->{code}}, @{$_->{code}};
            } else {
                # format 1
                $raw .= pack "CCC*", 1, scalar @range, map @$_, @range;
            }
        }
    }
    $raw;
}


sub encode_seqlen {
    my $arr = shift;
    my $i = shift // 0;
    my $m = @{$arr};
    my $j = $i;
    my @range;
    while ($i < $m) {
        $j = $arr->[$i];
        my $n = 0;
        while (defined $arr->[$i + $n + 1]) {
            last unless $j + $n + 1 == $arr->[$i + $n + 1];
            $n++;
        }
        push @range, [ $arr->[$i], $n ];
        $i += $n + 1;
        $j += $n + 1;
    }
    @range;
}


# Table 17, 18, 19, 20, 21, 22
sub Charset {
    my $self = shift;
    $self->{Charset} //= $self->get_Charset;
    $self->{Charset};
}


sub get_Charset {
    my $self = shift;

    my $charset;
    # Table 22
    my $pos = $self->TopDICT->{charset};
    if ($pos == 0) {
	$charset = {
	    format => undef,
	    Id => $pos,
	    code => $self->ISOAdobeCharset,
	};
    } elsif ($pos == 1) {
	$charset = {
	    format => undef,
	    Id => $pos,
	    code => $self->ExpertCharset,
	};
    } elsif ($pos == 2) {
	$charset = {
	    format => undef,
	    Id => $pos,
	    code => $self->ExpertSubsetCharset,
	};
    } elsif (($pos & ~3) == 0) {
	return undef;
    } else {
	return undef unless my $nGlyphs = $self->nGlyphs;
        my ($dat);
        my ($fh) = $self->{' INFILE'};
        $fh->seek($self->{' OFFSET'} + $pos, 0) if defined $pos;
        $fh->read($dat, 1);
	my ($format) = unpack "C", $dat;
	my @code = (0); # as GID
	if ($format == 0) {
	    # Table 17
            $fh->read($dat, 2 * ($nGlyphs - 1));
	    push @code, unpack "n*", $dat;
	} else {
	    # Table 18 + Table 19 ... (Format 1)
	    # Table 20 + Table 21 ... (Format 2)
	    my %u = (1 => [ "nC", 3, 'Card8' ], 2 => [ "nn", 4, 'Card16' ]);
	    if (my $u = $u{$format}) {
		my $count = $nGlyphs - 1;
		while ($count > 0) {
                    $fh->read($dat, $u->[1]);
		    my ($first, $nLeft) = unpack $u->[0], $dat;
		    push @code, $first;
		    push @code, map $first + $_, 1 .. $nLeft if $nLeft >= 1;
		    $count -= 1 + $nLeft;
		}
	    } else {
		croak "Charset: unknown format #$format";	# xxxxx
                $format = undef;
	    }
	}
	$charset = {
	    format => $format,
	    code => \@code,
	};
    }
    $charset;
}


sub pack_Charset {
    my $self = shift;
    my $raw;
    for ($self->Charset) {
        if (defined $_->{Id}) {
            $raw = $_->{Id};
        } else {
            my @range = encode_seqlen($_->{code}, 1);
            my $n = scalar @range;
            my $m = max map $_->[1], @range;
            if ($n && $m <= 255 && $n * 3 <= @{$_->{code}}) {
                # format 1
                $raw .= pack "C(nC)$n", 1, map @$_, @range;
            } elsif (@range && $m >= 255 && @range * 4 <= @{$_->{code}}) {
                # format 2
                $raw .= pack "C(nn)$n", 2, map @$_, @range;
            } else {
                my @code;
                for my $i (1 .. $#{$_->{code}}) {
                    push @code, $_->{code}->[$i] // 0;
                }
                # format 0
                $raw .= pack "Cn*", 0, @code;
            }
        }
    }
    $raw;
}


sub nGlyphs {
    my $self = shift;
    $self->{nGlyphs} //= $self->get_nGlyphs;
}


sub get_nGlyphs
{
    my $self = shift;
    my ($dat);
    my ($fh) = $self->{' INFILE'};
    my $lastpos = $fh->tell;
    my $pos = $self->TopDICT->{CharStrings};
    croak "?CharStrings" unless defined $pos;
    $fh->seek($self->{' OFFSET'} + $pos, 0);
    $fh->read($dat, 3);
    my ($count, $offSize) = unpack "nC", $dat;
    $fh->seek($lastpos, 0);
    $count;
}


sub FDSelect {
    my $self = shift;
    $self->{FDSelect} //= $self->get_FDSelect;
}


sub get_FDSelect {
    my $self = shift;

    my $pos = $self->TopDICT->{FDSelect};
    return undef unless defined $pos;

    my ($dat);
    my ($fh) = $self->{' INFILE'};

    my $FDSelect = {};
    $fh->seek($self->{' OFFSET'} + $pos, 0);
    $fh->read($dat, 1);
    my $format = unpack "C", $dat;
    $FDSelect->{format} = $format;
    if ($format == 0) {
	return undef unless my $nGlyphs = $self->nGlyphs;
        $fh->read($dat, 1 * $nGlyphs);
        my @fds = unpack "C*", $dat;
        $FDSelect->{fds} = \@fds;
    } elsif ($format == 3) {
        $fh->read($dat, 2);
        my $nRanges = unpack "n", $dat;
        $fh->read($dat, (2 + 1) * $nRanges + 2);
        my @fds;
        my @Range3 = unpack "(nC)${nRanges}n1", $dat;
        for (0 .. $nRanges - 1) {
            my $i = $_ * 2;
            my $first = $Range3[$i];
            my $fd    = $Range3[$i + 1];
            my $last  = $Range3[$i + 2] - 1;
            for ($first .. $last) {
                $fds[$_] = $fd;
            }
        }
        $FDSelect->{fds} = \@fds;
    } else {
        carp "FDSelect: unknown format #$format";	# xxxxx
        $format = undef;
    }
    $FDSelect;
}


sub pack_FDSelect {
    my $self = shift;

    my $raw;
    for (grep defined, $self->FDSelect) {
        $raw .= pack "C", $_->{format};
        return undef unless my $nGlyphs = $self->nGlyphs;
        if ($_->{format} == 0) {
            $raw .= pack "C*", @{$_->{fds}};
        } elsif ($_->{format} == 3) {
            my $nRanges = 0;
            my $ranges;
            my $i = 0;
            while ($i < $nGlyphs) {
                my $j = $i + 1;
                while ($j < $nGlyphs) {
                    last unless $_->{fds}[$i] == $_->{fds}[$j];
                    $j++;
                }
                $nRanges++;
                $ranges .= pack "nC", $i, $_->{fds}[$i];
                $i = $j;
            }
            $raw .= pack "n", $nRanges;
            $raw .= $ranges;
            $raw .= pack "n", $i;
        }
    }

    $raw;
}


sub CharStrings_INDEX {
    my $self = shift;
    $self->{CharStrings_INDEX} //= $self->read_INDEX($self->TopDICT->{CharStrings});
}


sub pack_CharStrings {
    my $self = shift;
    $self->pack_INDEX($self->CharStrings_INDEX);
}


sub FontDICT_INDEX {
    my $self = shift;
    $self->{FontDICT_INDEX} //= $self->read_INDEX($self->TopDICT->{FDArray});
}


sub FontDICT {
    my ($self) = @_;
    $self->{FontDICT} //= $self->get_FontDICT;
}


sub get_FontDICT {
    my ($self) = @_;

    my @fontdict;
    if (my $index = $self->FontDICT_INDEX) {
        for (0 .. $index->{count} - 1) {
            my $i = $index->{offset}[$_];
            my $j = $index->{offset}[$_ + 1];
            my @data = unpack "C*", substr $index->{data}, $i - 1, $j - $i;
            $fontdict[$_] = $self->DICT_Data("FontDICT.$_", $self->DICT_operators, \@data);
        }
    }
    \@fontdict;
}


# Private DICT Data
sub get_PrivateDICT_and_LocalSubr_INDEX {
    my $self = shift;
    my $dict_name = shift;
    my $dict = shift;

    my ($dat);
    my ($fh) = $self->{' INFILE'};

    my ($PrivateDICT, $LocalSubr_INDEX);
    if (my $private = $dict->{Private}) {
        my ($size, $offset) = @$private;
        if ($size && $offset) {
            $fh->seek($self->{' OFFSET'} + $offset, 0);
            $fh->read($dat, $size);
            my @data = unpack "C*", $dat;
            $PrivateDICT = $self->DICT_Data($dict_name, $self->DICT_operators, \@data);
            if (my $subrs = $PrivateDICT->{Subrs}) {
                $LocalSubr_INDEX = $self->read_INDEX($offset + $subrs);
            }
        }
    }

    ($PrivateDICT, $LocalSubr_INDEX);
}


sub test_cmp_array {
    for (
        { a => [ 0, 1, 2 ], b => [ 0, 1, 3 ] },
        { a => [ 0 ], b => [ 0, 1 ] },
        { a => [ 1, 2 ], b => [ 0, 1 ] },
    ) {
        my $a = $_->{a};
        my $b = $_->{b};
        my $c = cmp_array($a, $b);
        print STDERR Dumper({
            a => ref $a ? "[ ".join(', ', @$a)." ]" : $a,
            b => ref $b ? "[ ".join(', ', @$b)." ]" : $b,
            c => $c == 0 ? "a == b" : $c < 0 ? "a < b" : "a > b ",
        });
    }
}


sub cmp_array {
    my ($a, $b) = @_;

    return  0 if !defined $a && !defined $b;
    return +1 if  defined $a && !defined $b;
    return -1 if !defined $a &&  defined $b;

    croak Dumper({ cmp_array => { a => $a, b => $b } }) unless
        defined $a && ref $a eq 'ARRAY' &&
        defined $b && ref $b eq 'ARRAY';
    my @a = @$a;
    my @b = @$b;
    my $c;
    if (@a == 0 || @b == 0) {
        $c = @a <=> @b;
        return $c if $c;
    }
    return  0 if !defined $a[0] && !defined $b[0];
    return +1 if  defined $a[0] && !defined $b[0];
    return -1 if !defined $a[0] &&  defined $b[0];
    $c = $a[0] <=> $b[0];
    return $c if $c;
    shift @a;
    shift @b;
    cmp_array(\@a, \@b);
}


sub encode_array {
    map encode_number($_), @_;
}


sub encode_delta {
    # The length of array or delta types is determined by counting the
    # operands preceding the operator. The second and subsequent
    # numbers in a delta are encoded as the difference between
    # successive values. For example, an array a0, a1, ..., an would
    # be encoded as: a0 (a1–a0) (a2–a1) ..., (an–a(n–1)).

    my @list;
    for (0 .. @_ - 1) {
        push @list, ($_ > 0)? $_[$_] - $_[$_ - 1] : $_[$_];
    }
    encode_array(@list);
}



# Table 23
sub PrivateDICT_Operators {
    my $self = shift;
    $self->{PrivateDICT_Operators} //= $self->get_PrivateDICT_Operators;
}

sub get_PrivateDICT_Operators {
    my (%e, %e2);

    #   [ Name Operand(s) Default notes ],
    for (
        [ BlueValues => T_delta ],
        [ OtherBlues => T_delta ],
        [ FamilyBlues => T_delta ],
        [ FamilyOtherBlues => T_delta ],
        [ StdHW => T_number ],
        [ StdVW => T_number ],
        [ Subrs => T_number, undef, 'Offset (self) to local subrs' ],
        [ defaultWidthX => T_number, 0, 'see below' ],
        [ nominalWidthX => T_number, 0, 'see below' ],
    ) {
        my $v = eval "D_$_->[0]";
        croak "D_$_->[0] is not defined" unless defined $v;
        $e{$v} = $_;
    }

    for (
        [ BlueScale => T_number, 0.039625],
        [ BlueShift => T_number, 7 ],
        [ BlueFuzz => T_number,  1 ],
        [ StemSnapH => T_delta ],
        [ StemSnapV => T_delta ],
        [ ForceBold => T_boolean, 0 ],
        [ LanguageGroup => T_number, 0 ],
        [ ExpansionFactor => T_number, 0.06],
        [ initialRandomSeed => T_number, 0],
    ) {
        my $v = eval "D_$_->[0]";
        croak "D_$_->[0] is not defined" unless defined $v;
        $e2{$v} = $_;
    }

    $e{eval D_escape} = \%e2;
    \%e;
}


# Appendix A -  Standard Strings
sub StandardStrings {
    my $self = shift;
    $self->{StandardStrings} //= $self->get_StandardStrings;
}


sub get_StandardStrings {
    my @list = (
#    sid/name
qw/
       0 .notdef                 21 four                    42 I
       1 space                   22 five                    43 J
       2 exclam                  23 six                     44 K
       3 quotedbl                24 seven                   45 L
       4 numbersign              25 eight                   46 M
       5 dollar                  26 nine                    47 N
       6 percent                 27 colon                   48 O
       7 ampersand               28 semicolon               49 P
       8 quoteright              29 less                    50 Q
       9 parenleft               30 equal                   51 R
      10 parenright              31 greater                 52 S
      11 asterisk                32 question                53 T
      12 plus                    33 at                      54 U
      13 comma                   34 A                       55 V
      14 hyphen                  35 B                       56 W
      15 period                  36 C                       57 X
      16 slash                   37 D                       58 Y
      17 zero                    38 E                       59 Z
      18 one                     39 F                       60 bracketleft
      19 two                     40 G                       61 backslash
      20 three                   41 H                       62 bracketright

      63 asciicircum             92 braceleft              121 ellipsis
      64 underscore              93 bar                    122 perthousand
      65 quoteleft               94 braceright             123 questiondown
      66 a                       95 asciitilde             124 grave
      67 b                       96 exclamdown             125 acute
      68 c                       97 cent                   126 circumflex
      69 d                       98 sterling               127 tilde
      70 e                       99 fraction               128 macron
      71 f                      100 yen                    129 breve
      72 g                      101 florin                 130 dotaccent
      73 h                      102 section                131 dieresis
      74 i                      103 currency               132 ring
      75 j                      104 quotesingle            133 cedilla
      76 k                      105 quotedblleft           134 hungarumlaut
      77 l                      106 guillemotleft          135 ogonek
      78 m                      107 guilsinglleft          136 caron
      79 n                      108 guilsinglright         137 emdash
      80 o                      109 fi                     138 AE
      81 p                      110 fl                     139 ordfeminine
      82 q                      111 endash                 140 Lslash
      83 r                      112 dagger                 141 Oslash
      84 s                      113 daggerdbl              142 OE
      85 t                      114 periodcentered         143 ordmasculine
      86 u                      115 paragraph              144 ae
      87 v                      116 bullet                 145 dotlessi
      88 w                      117 quotesinglbase         146 lslash
      89 x                      118 quotedblbase           147 oslash
      90 y                      119 quotedblright          148 oe
      91 z                      120 guillemotright         149 germandbls

     150 onesuperior            179 Ecircumflex            208 ecircumflex
     151 logicalnot             180 Edieresis              209 edieresis
     152 mu                     181 Egrave                 210 egrave
     153 trademark              182 Iacute                 211 iacute
     154 Eth                    183 Icircumflex            212 icircumflex
     155 onehalf                184 Idieresis              213 idieresis
     156 plusminus              185 Igrave                 214 igrave
     157 Thorn                  186 Ntilde                 215 ntilde
     158 onequarter             187 Oacute                 216 oacute
     159 divide                 188 Ocircumflex            217 ocircumflex
     160 brokenbar              189 Odieresis              218 odieresis
     161 degree                 190 Ograve                 219 ograve
     162 thorn                  191 Otilde                 220 otilde
     163 threequarters          192 Scaron                 221 scaron
     164 twosuperior            193 Uacute                 222 uacute
     165 registered             194 Ucircumflex            223 ucircumflex
     166 minus                  195 Udieresis              224 udieresis
     167 eth                    196 Ugrave                 225 ugrave
     168 multiply               197 Yacute                 226 yacute
     169 threesuperior          198 Ydieresis              227 ydieresis
     170 copyright              199 Zcaron                 228 zcaron
     171 Aacute                 200 aacute                 229 exclamsmall
     172 Acircumflex            201 acircumflex            230 Hungarumlautsmall
     173 Adieresis              202 adieresis              231 dollaroldstyle
     174 Agrave                 203 agrave                 232 dollarsuperior
     175 Aring                  204 aring                  233 ampersandsmall
     176 Atilde                 205 atilde                 234 Acutesmall
     177 Ccedilla               206 ccedilla               235 parenleftsuperior
     178 Eacute                 207 eacute                 236 parenrightsuperior

     237 twodotenleader         266 ff                     295 Vsmall
     238 onedotenleader         267 ffi                    296 Wsmall
     239 zerooldstyle           268 ffl                    297 Xsmall
     240 oneoldstyle            269 parenleftinferior      298 Ysmall
     241 twooldstyle            270 parenrightinferior     299 Zsmall
     242 threeoldstyle          271 Circumflexsmall        300 colonmonetary
     243 fouroldstyle           272 hyphensuperior         301 onefitted
     244 fiveoldstyle           273 Gravesmall             302 rupiah
     245 sixoldstyle            274 Asmall                 303 Tildesmall
     246 sevenoldstyle          275 Bsmall                 304 exclamdownsmall
     247 eightoldstyle          276 Csmall                 305 centoldstyle
     248 nineoldstyle           277 Dsmall                 306 Lslashsmall
     249 commasuperior          278 Esmall                 307 Scaronsmall
     250 threequartersemdash    279 Fsmall                 308 Zcaronsmall
     251 periodsuperior         280 Gsmall                 309 Dieresissmall
     252 questionsmall          281 Hsmall                 310 Brevesmall
     253 asuperior              282 Ismall                 311 Caronsmall
     254 bsuperior              283 Jsmall                 312 Dotaccentsmall
     255 centsuperior           284 Ksmall                 313 Macronsmall
     256 dsuperior              285 Lsmall                 314 figuredash
     257 esuperior              286 Msmall                 315 hypheninferior
     258 isuperior              287 Nsmall                 316 Ogoneksmall
     259 lsuperior              288 Osmall                 317 Ringsmall
     260 msuperior              289 Psmall                 318 Cedillasmall
     261 nsuperior              290 Qsmall                 319 questiondownsmall
     262 osuperior              291 Rsmall                 320 oneeighth
     263 rsuperior              292 Ssmall                 321 threeeighths
     264 ssuperior              293 Tsmall                 322 fiveeighths
     265 tsuperior              294 Usmall                 323 seveneighths

     324 onethird               353 AEsmall                382 001.003
     325 twothirds              354 Ccedillasmall          383 Black
     326 zerosuperior           355 Egravesmall            384 Bold
     327 foursuperior           356 Eacutesmall            385 Book
     328 fivesuperior           357 Ecircumflexsmall       386 Light
     329 sixsuperior            358 Edieresissmall         387 Medium
     330 sevensuperior          359 Igravesmall            388 Regular
     331 eightsuperior          360 Iacutesmall            389 Roman
     332 ninesuperior           361 Icircumflexsmall       390 Semibold
     333 zeroinferior           362 Idieresissmall
     334 oneinferior            363 Ethsmall
     335 twoinferior            364 Ntildesmall
     336 threeinferior          365 Ogravesmall
     337 fourinferior           366 Oacutesmall
     338 fiveinferior           367 Ocircumflexsmall
     339 sixinferior            368 Otildesmall
     340 seveninferior          369 Odieresissmall
     341 eightinferior          370 OEsmall
     342 nineinferior           371 Oslashsmall
     343 centinferior           372 Ugravesmall
     344 dollarinferior         373 Uacutesmall
     345 periodinferior         374 Ucircumflexsmall
     346 commainferior          375 Udieresissmall
     347 Agravesmall            376 Yacutesmall
     348 Aacutesmall            377 Thornsmall
     349 Acircumflexsmall       378 Ydieresissmall
     350 Atildesmall            379 001.000
     351 Adieresissmall         380 001.001
     352 Aringsmall             381 001.002
/);

    my @sid_name;
    while (my ($sid, $name) = splice @list, 0, 2) {
        $sid_name[$sid] = $name;
    }
    \@sid_name;
}


# Appendix B - Predefined Encodings (Standard Encoding)
sub StandardEncoding {
    my $self = shift;
    $self->{StandardEncoding} //= $self->get_StandardEncoding;
}

sub get_StandardEncoding {
    my @list = (
#code/SID/name
qw/
   0   0 .notdef             21   0 .notdef             42  11 asterisk
   1   0 .notdef             22   0 .notdef             43  12 plus
   2   0 .notdef             23   0 .notdef             44  13 comma
   3   0 .notdef             24   0 .notdef             45  14 hyphen
   4   0 .notdef             25   0 .notdef             46  15 period
   5   0 .notdef             26   0 .notdef             47  16 slash
   6   0 .notdef             27   0 .notdef             48  17 zero
   7   0 .notdef             28   0 .notdef             49  18 one
   8   0 .notdef             29   0 .notdef             50  19 two
   9   0 .notdef             30   0 .notdef             51  20 three
  10   0 .notdef             31   0 .notdef             52  21 four
  11   0 .notdef             32   1 space               53  22 five
  12   0 .notdef             33   2 exclam              54  23 six
  13   0 .notdef             34   3 quotedbl            55  24 seven
  14   0 .notdef             35   4 numbersign          56  25 eight
  15   0 .notdef             36   5 dollar              57  26 nine
  16   0 .notdef             37   6 percent             58  27 colon
  17   0 .notdef             38   7 ampersand           59  28 semicolon
  18   0 .notdef             39   8 quoteright          60  29 less
  19   0 .notdef             40   9 parenleft           61  30 equal
  20   0 .notdef             41  10 parenright          62  31 greater

  63  32 question            92  61 backslash          121  90 y
  64  33 at                  93  62 bracketright       122  91 z
  65  34 A                   94  63 asciicircum        123  92 braceleft
  66  35 B                   95  64 underscore         124  93 bar
  67  36 C                   96  65 quoteleft          125  94 braceright
  68  37 D                   97  66 a                  126  95 asciitilde
  69  38 E                   98  67 b                  127   0 .notdef
  70  39 F                   99  68 c                  128   0 .notdef
  71  40 G                  100  69 d                  129   0 .notdef
  72  41 H                  101  70 e                  130   0 .notdef
  73  42 I                  102  71 f                  131   0 .notdef
  74  43 J                  103  72 g                  132   0 .notdef
  75  44 K                  104  73 h                  133   0 .notdef
  76  45 L                  105  74 i                  134   0 .notdef
  77  46 M                  106  75 j                  135   0 .notdef
  78  47 N                  107  76 k                  136   0 .notdef
  79  48 O                  108  77 l                  137   0 .notdef
  80  49 P                  109  78 m                  138   0 .notdef
  81  50 Q                  110  79 n                  139   0 .notdef
  82  51 R                  111  80 o                  140   0 .notdef
  83  52 S                  112  81 p                  141   0 .notdef
  84  53 T                  113  82 q                  142   0 .notdef
  85  54 U                  114  83 r                  143   0 .notdef
  86  55 V                  115  84 s                  144   0 .notdef
  87  56 W                  116  85 t                  145   0 .notdef
  88  57 X                  117  86 u                  146   0 .notdef
  89  58 Y                  118  87 v                  147   0 .notdef
  90  59 Z                  119  88 w                  148   0 .notdef
  91  60 bracketleft        120  89 x                  149   0 .notdef

 150   0 .notdef            179 113 daggerdbl          208 137 emdash
 151   0 .notdef            180 114 periodcentered     209   0 .notdef
 152   0 .notdef            181   0 .notdef            210   0 .notdef
 153   0 .notdef            182 115 paragraph          211   0 .notdef
 154   0 .notdef            183 116 bullet             212   0 .notdef
 155   0 .notdef            184 117 quotesinglbase     213   0 .notdef
 156   0 .notdef            185 118 quotedblbase       214   0 .notdef
 157   0 .notdef            186 119 quotedblright      215   0 .notdef
 158   0 .notdef            187 120 guillemotright     216   0 .notdef
 159   0 .notdef            188 121 ellipsis           217   0 .notdef
 160   0 .notdef            189 122 perthousand        218   0 .notdef
 161  96 exclamdown         190   0 .notdef            219   0 .notdef
 162  97 cent               191 123 questiondown       220   0 .notdef
 163  98 sterling           192   0 .notdef            221   0 .notdef
 164  99 fraction           193 124 grave              222   0 .notdef
 165 100 yen                194 125 acute              223   0 .notdef
 166 101 florin             195 126 circumflex         224   0 .notdef
 167 102 section            196 127 tilde              225 138 AE
 168 103 currency           197 128 macron             226   0 .notdef
 169 104 quotesingle        198 129 breve              227 139 ordfeminine
 170 105 quotedblleft       199 130 dotaccent          228   0 .notdef
 171 106 guillemotleft      200 131 dieresis           229   0 .notdef
 172 107 guilsinglleft      201   0 .notdef            230   0 .notdef
 173 108 guilsinglright     202 132 ring               231   0 .notdef
 174 109 fi                 203 133 cedilla            232 140 Lslash
 175 110 fl                 204   0 .notdef            233 141 Oslash
 176   0 .notdef            205 134 hungarumlaut       234 142 OE
 177 111 endash             206 135 ogonek             235 143 ordmasculine
 178 112 dagger             207 136 caron              236   0 .notdef
 237   0 .notdef            244   0 .notdef            251 149 germandbls
 238   0 .notdef            245 145 dotlessi           252   0 .notdef
 239   0 .notdef            246   0 .notdef            253   0 .notdef
 240   0 .notdef            247   0 .notdef            254   0 .notdef
 241 144 ae                 248 146 lslash             255   0 .notdef
 242   0 .notdef            249 147 oslash
 243   0 .notdef            250 148 oe
/);

    my @code;
    while (my ($code, $sid, $name) = splice @list, 0, 3) {
        $code[$code] = $sid;
    }
    \@code;
}

# Appendix B - Predefined Encodings (Expert Encoding)
sub ExpertEncoding {
    my $self = shift;
    $self->{ExpertEncoding} //= $self->get_ExpertEncoding;
}

sub get_ExpertEncoding {
    my @list = (
#code/SID/name
qw/
   0   0 .notdef             18   0 .notdef             36 231 dollaroldstyle
   1   0 .notdef             19   0 .notdef             37 232 dollarsuperior
   2   0 .notdef             20   0 .notdef             38 233 ampersandsmall
   3   0 .notdef             21   0 .notdef             39 234 Acutesmall
   4   0 .notdef             22   0 .notdef             40 235 parenleftsuperior
   5   0 .notdef             23   0 .notdef             41 236 parenrightsuperior
   6   0 .notdef             24   0 .notdef             42 237 twodotenleader
   7   0 .notdef             25   0 .notdef             43 238 onedotenleader
   8   0 .notdef             26   0 .notdef             44  13 comma
   9   0 .notdef             27   0 .notdef             45  14 hyphen
  10   0 .notdef             28   0 .notdef             46  15 period
  11   0 .notdef             29   0 .notdef             47  99 fraction
  12   0 .notdef             30   0 .notdef             48 239 zerooldstyle
  13   0 .notdef             31   0 .notdef             49 240 oneoldstyle
  14   0 .notdef             32   1 space               50 241 twooldstyle
  15   0 .notdef             33 229 exclamsmall         51 242 threeoldstyle
  16   0 .notdef             34 230 Hungarumlautsmall   52 243 fouroldstyle
  17   0 .notdef             35   0 .notdef             53 244 fiveoldstyle
  54 245 sixoldstyle         83 264 ssuperior          112 289 Psmall
  55 246 sevenoldstyle       84 265 tsuperior          113 290 Qsmall
  56 247 eightoldstyle       85   0 .notdef            114 291 Rsmall
  57 248 nineoldstyle        86 266 ff                 115 292 Ssmall
  58  27 colon               87 109 fi                 116 293 Tsmall
  59  28 semicolon           88 110 fl                 117 294 Usmall
  60 249 commasuperior       89 267 ffi                118 295 Vsmall
  61 250 threequartersemdash 90 268 ffl                119 296 Wsmall
  62 251 periodsuperior      91 269 parenleftinferior  120 297 Xsmall
  63 252 questionsmall       92   0 .notdef            121 298 Ysmall
  64   0 .notdef             93 270 parenrightinferior 122 299 Zsmall
  65 253 asuperior           94 271 Circumflexsmall    123 300 colonmonetary
  66 254 bsuperior           95 272 hyphensuperior     124 301 onefitted
  67 255 centsuperior        96 273 Gravesmall         125 302 rupiah
  68 256 dsuperior           97 274 Asmall             126 303 Tildesmall
  69 257 esuperior           98 275 Bsmall             127   0 .notdef
  70   0 .notdef             99 276 Csmall             128   0 .notdef
  71   0 .notdef            100 277 Dsmall             129   0 .notdef
  72   0 .notdef            101 278 Esmall             130   0 .notdef
  73 258 isuperior          102 279 Fsmall             131   0 .notdef
  74   0 .notdef            103 280 Gsmall             132   0 .notdef
  75   0 .notdef            104 281 Hsmall             133   0 .notdef
  76 259 lsuperior          105 282 Ismall             134   0 .notdef
  77 260 msuperior          106 283 Jsmall             135   0 .notdef
  78 261 nsuperior          107 284 Ksmall             136   0 .notdef
  79 262 osuperior          108 285 Lsmall             137   0 .notdef
  80   0 .notdef            109 286 Msmall             138   0 .notdef
  81   0 .notdef            110 287 Nsmall             139   0 .notdef
  82 263 rsuperior          111 288 Osmall             140   0 .notdef
 141   0 .notdef            170 311 Caronsmall         199   0 .notdef
 142   0 .notdef            171   0 .notdef            200 326 zerosuperior
 143   0 .notdef            172 312 Dotaccentsmall     201 150 onesuperior
 144   0 .notdef            173   0 .notdef            202 164 twosuperior
 145   0 .notdef            174   0 .notdef            203 169 threesuperior
 146   0 .notdef            175 313 Macronsmall        204 327 foursuperior
 147   0 .notdef            176   0 .notdef            205 328 fivesuperior
 148   0 .notdef            177   0 .notdef            206 329 sixsuperior
 149   0 .notdef            178 314 figuredash         207 330 sevensuperior
 150   0 .notdef            179 315 hypheninferior     208 331 eightsuperior
 151   0 .notdef            180   0 .notdef            209 332 ninesuperior
 152   0 .notdef            181   0 .notdef            210 333 zeroinferior
 153   0 .notdef            182 316 Ogoneksmall        211 334 oneinferior
 154   0 .notdef            183 317 Ringsmall          212 335 twoinferior
 155   0 .notdef            184 318 Cedillasmall       213 336 threeinferior
 156   0 .notdef            185   0 .notdef            214 337 fourinferior
 157   0 .notdef            186   0 .notdef            215 338 fiveinferior
 158   0 .notdef            187   0 .notdef            216 339 sixinferior
 159   0 .notdef            188 158 onequarter         217 340 seveninferior
 160   0 .notdef            189 155 onehalf            218 341 eightinferior
 161 304 exclamdownsmall    190 163 threequarters      219 342 nineinferior
 162 305 centoldstyle       191 319 questiondownsmall  220 343 centinferior
 163 306 Lslashsmall        192 320 oneeighth          221 344 dollarinferior
 164   0 .notdef            193 321 threeeighths       222 345 periodinferior
 165   0 .notdef            194 322 fiveeighths        223 346 commainferior
 166 307 Scaronsmall        195 323 seveneighths       224 347 Agravesmall
 167 308 Zcaronsmall        196 324 onethird           225 348 Aacutesmall
 168 309 Dieresissmall      197 325 twothirds          226 349 Acircumflexsmall
 169 310 Brevesmall         198   0 .notdef            227 350 Atildesmall

 228 351 Adieresissmall     238 361 Icircumflexsmall   248 371 Oslashsmall
 229 352 Aringsmall         239 362 Idieresissmall     249 372 Ugravesmall
 230 353 AEsmall            240 363 Ethsmall           250 373 Uacutesmall
 231 354 Ccedillasmall      241 364 Ntildesmall        251 374 Ucircumflexsmall
 232 355 Egravesmall        242 365 Ogravesmall        252 375 Udieresissmall
 233 356 Eacutesmall        243 366 Oacutesmall        253 376 Yacutesmall
 234 357 Ecircumflexsmall   244 367 Ocircumflexsmall   254 377 Thornsmall
 235 358 Edieresissmall     245 368 Otildesmall        255 378 Ydieresissmall
 236 359 Igravesmall        246 369 Odieresissmall
 237 360 Iacutesmall        247 370 OEsmal
/);

    my @code;
    while (my ($code, $sid, $name) = splice @list, 0, 3) {
        $code[$code] = $sid;
    }
    \@code;
}


# Appendix C - Predefined Charsets (ISOAdobe)
# All charsets are presented in GID order beginning with GID 1. (The
# .notdef glyph is implicitly GID 0 and is therefore not shown.)

sub ISOAdobeCharset {
    my $self = shift;
    $self->{ISOAdobeCharset} //= $self->get_ISOAdobeCharset;
}


sub get_ISOAdobeCharset {
    my @list = (
#   SID/name
qw/
       1 space                   21 four                    41 H
       2 exclam                  22 five                    42 I
       3 quotedbl                23 six                     43 J
       4 numbersign              24 seven                   44 K
       5 dollar                  25 eight                   45 L
       6 percent                 26 nine                    46 M
       7 ampersand               27 colon                   47 N
       8 quoteright              28 semicolon               48 O
       9 parenleft               29 less                    49 P
      10 parenright              30 equal                   50 Q
      11 asterisk                31 greater                 51 R
      12 plus                    32 question                52 S
      13 comma                   33 at                      53 T
      14 hyphen                  34 A                       54 U
      15 period                  35 B                       55 V
      16 slash                   36 C                       56 W
      17 zero                    37 D                       57 X
      18 one                     38 E                       58 Y
      19 two                     39 F                       59 Z
      20 three                   40 G                       60 bracketleft

      61 backslash               93 bar                    125 acute
      62 bracketright            94 braceright             126 circumflex
      63 asciicircum             95 asciitilde             127 tilde
      64 underscore              96 exclamdown             128 macron
      65 quoteleft               97 cent                   129 breve
      66 a                       98 sterling               130 dotaccent
      67 b                       99 fraction               131 dieresis
      68 c                      100 yen                    132 ring
      69 d                      101 florin                 133 cedilla
      70 e                      102 section                134 hungarumlaut
      71 f                      103 currency               135 ogonek
      72 g                      104 quotesingle            136 caron
      73 h                      105 quotedblleft           137 emdash
      74 i                      106 guillemotleft          138 AE
      75 j                      107 guilsinglleft          139 ordfeminine
      76 k                      108 guilsinglright         140 Lslash
      77 l                      109 fi                     141 Oslash
      78 m                      110 fl                     142 OE
      79 n                      111 endash                 143 ordmasculine
      80 o                      112 dagger                 144 ae
      81 p                      113 daggerdbl              145 dotlessi
      82 q                      114 periodcentered         146 lslash
      83 r                      115 paragraph              147 oslash
      84 s                      116 bullet                 148 oe
      85 t                      117 quotesinglbase         149 germandbls
      86 u                      118 quotedblbase           150 onesuperior
      87 v                      119 quotedblright          151 logicalnot
      88 w                      120 guillemotright         152 mu
      89 x                      121 ellipsis               153 trademark
      90 y                      122 perthousand            154 Eth
      91 z                      123 questiondown           155 onehalf
      92 braceleft              124 grave                  156 plusminus

     157 Thorn                  181 Egrave                 205 atilde
     158 onequarter             182 Iacute                 206 ccedilla
     159 divide                 183 Icircumflex            207 eacute
     160 brokenbar              184 Idieresis              208 ecircumflex
     161 degree                 185 Igrave                 209 edieresis
     162 thorn                  186 Ntilde                 210 egrave
     163 threequarters          187 Oacute                 211 iacute
     164 twosuperior            188 Ocircumflex            212 icircumflex
     165 registered             189 Odieresis              213 idieresis
     166 minus                  190 Ograve                 214 igrave
     167 eth                    191 Otilde                 215 ntilde
     168 multiply               192 Scaron                 216 oacute
     169 threesuperior          193 Uacute                 217 ocircumflex
     170 copyright              194 Ucircumflex            218 odieresis
     171 Aacute                 195 Udieresis              219 ograve
     172 Acircumflex            196 Ugrave                 220 otilde
     173 Adieresis              197 Yacute                 221 scaron
     174 Agrave                 198 Ydieresis              222 uacute
     175 Aring                  199 Zcaron                 223 ucircumflex
     176 Atilde                 200 aacute                 224 udieresis
     177 Ccedilla               201 acircumflex            225 ugrave
     178 Eacute                 202 adieresis              226 yacute
     179 Ecircumflex            203 agrave                 227 ydieresis
     180 Edieresis              204 aring                  228 zcaron
/);

    my @charset = (0);      # .notdef
    while (my ($sid, $name) = splice @list, 0, 2) {
        push @charset, $sid;
    }
    \@charset;
}


# Appendix C - Predefined Charsets (Expert)
sub ExpertCharset {
    my $self = shift;
    $self->{ExpertCharset} //= $self->get_ExpertCharset;
}

sub get_ExpertCharset {
    my @list = (
#    SID/name
qw/
       1 space                  233 ampersandsmall         238 onedotenleader
     229 exclamsmall            234 Acutesmall              13 comma
     230 Hungarumlautsmall      235 parenleftsuperior       14 hyphen
     231 dollaroldstyle         236 parenrightsuperior      15 period
     232 dollarsuperior         237 twodotenleader          99 fraction

     239 zerooldstyle           267 ffi                    299 Zsmall
     240 oneoldstyle            268 ffl                    300 colonmonetary
     241 twooldstyle            269 parenleftinferior      301 onefitted
     242 threeoldstyle          270 parenrightinferior     302 rupiah
     243 fouroldstyle           271 Circumflexsmall        303 Tildesmall
     244 fiveoldstyle           272 hyphensuperior         304 exclamdownsmall
     245 sixoldstyle            273 Gravesmall             305 centoldstyle
     246 sevenoldstyle          274 Asmall                 306 Lslashsmall
     247 eightoldstyle          275 Bsmall                 307 Scaronsmall
     248 nineoldstyle           276 Csmall                 308 Zcaronsmall
      27 colon                  277 Dsmall                 309 Dieresissmall
      28 semicolon              278 Esmall                 310 Brevesmall
     249 commasuperior          279 Fsmall                 311 Caronsmall
     250 threequartersemdash    280 Gsmall                 312 Dotaccentsmall
     251 periodsuperior         281 Hsmall                 313 Macronsmall
     252 questionsmall          282 Ismall                 314 figuredash
     253 asuperior              283 Jsmall                 315 hypheninferior
     254 bsuperior              284 Ksmall                 316 Ogoneksmall
     255 centsuperior           285 Lsmall                 317 Ringsmall
     256 dsuperior              286 Msmall                 318 Cedillasmall
     257 esuperior              287 Nsmall                 158 onequarter
     258 isuperior              288 Osmall                 155 onehalf
     259 lsuperior              289 Psmall                 163 threequarters
     260 msuperior              290 Qsmall                 319 questiondownsmall
     261 nsuperior              291 Rsmall                 320 oneeighth
     262 osuperior              292 Ssmall                 321 threeeighths
     263 rsuperior              293 Tsmall                 322 fiveeighths
     264 ssuperior              294 Usmall                 323 seveneighths
     265 tsuperior              295 Vsmall                 324 onethird
     266 ff                     296 Wsmall                 325 twothirds
     109 fi                     297 Xsmall                 326 zerosuperior
     110 fl                     298 Ysmall                 150 onesuperior

     164 twosuperior            343 centinferior           361 Icircumflexsmall
     169 threesuperior          344 dollarinferior         362 Idieresissmall
     327 foursuperior           345 periodinferior         363 Ethsmall
     328 fivesuperior           346 commainferior          364 Ntildesmall
     329 sixsuperior            347 Agravesmall            365 Ogravesmall
     330 sevensuperior          348 Aacutesmall            366 Oacutesmall
     331 eightsuperior          349 Acircumflexsmall       367 Ocircumflexsmall
     332 ninesuperior           350 Atildesmall            368 Otildesmall
     333 zeroinferior           351 Adieresissmall         369 Odieresissmall
     334 oneinferior            352 Aringsmall             370 OEsmall
     335 twoinferior            353 AEsmall                371 Oslashsmall
     336 threeinferior          354 Ccedillasmall          372 Ugravesmall
     337 fourinferior           355 Egravesmall            373 Uacutesmall
     338 fiveinferior           356 Eacutesmall            374 Ucircumflexsmall
     339 sixinferior            357 Ecircumflexsmall       375 Udieresissmall
     340 seveninferior          358 Edieresissmall         376 Yacutesmall
     341 eightinferior          359 Igravesmall            377 Thornsmall
     342 nineinferior           360 Iacutesmall            378 Ydieresissmall
/);

    my @charset = (0);      # .notdef
    while (my ($sid, $name) = splice @list, 0, 2) {
        push @charset, $sid;
    }
    \@charset;
}


# Appendix C - Predefined Charsets (Expert Subset)

sub ExpertSubsetCharset {
    my $self = shift;
    $self->{ExpertSubsetCharset} //= $self->get_ExpertSubsetCharset;
}


sub get_ExpertSubsetCharset {
    my @list = (
#   SID/name
qw/
       1 space                  239 zerooldstyle            28 semicolon
     231 dollaroldstyle         240 oneoldstyle            249 commasuperior
     232 dollarsuperior         241 twooldstyle            250 threequartersemdash
     235 parenleftsuperior      242 threeoldstyle          251 periodsuperior
     236 parenrightsuperior     243 fouroldstyle           253 asuperior
     237 twodotenleader         244 fiveoldstyle           254 bsuperior
     238 onedotenleader         245 sixoldstyle            255 centsuperior
      13 comma                  246 sevenoldstyle          256 dsuperior
      14 hyphen                 247 eightoldstyle          257 esuperior
      15 period                 248 nineoldstyle           258 isuperior
      99 fraction                27 colon                  259 lsuperior

     260 msuperior              314 figuredash             330 sevensuperior
     261 nsuperior              315 hypheninferior         331 eightsuperior
     262 osuperior              158 onequarter             332 ninesuperior
     263 rsuperior              155 onehalf                333 zeroinferior
     264 ssuperior              163 threequarters          334 oneinferior
     265 tsuperior              320 oneeighth              335 twoinferior
     266 ff                     321 threeeighths           336 threeinferior
     109 fi                     322 fiveeighths            337 fourinferior
     110 fl                     323 seveneighths           338 fiveinferior
     267 ffi                    324 onethird               339 sixinferior
     268 ffl                    325 twothirds              340 seveninferior
     269 parenleftinferior      326 zerosuperior           341 eightinferior
     270 parenrightinferior     150 onesuperior            342 nineinferior
     272 hyphensuperior         164 twosuperior            343 centinferior
     300 colonmonetary          169 threesuperior          344 dollarinferior
     301 onefitted              327 foursuperior           345 periodinferior
     302 rupiah                 328 fivesuperior           346 commainferior
     305 centoldstyle           329 sixsuperior
/);

    my @charset = (0);      # .notdef
    while (my ($sid, $name) = splice @list, 0, 2) {
        push @charset, $sid;
    }
    \@charset;
}
