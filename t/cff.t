#!perl

use strict;
use warnings;
use Test::More;
use Font::TTF::Font;
use Font::TTF::Ttc;
use IO::String;

my ($font) = grep -f, (
    #"../HaranoAjiFonts/HaranoAjiMincho-Regular.otf",
    "/usr/share/fonts/opentype/noto/NotoSansCJK-Regular.ttc", # apt-get install fonts-noto
);

my $f = $font =~ /\.ttc$/?
    Font::TTF::Ttc->open($font)->{directs}[0] :
    Font::TTF::Font->open($font);
ok $f, "open font: $font";
my $cff = $f->{'CFF '}->read;

sub get_xml {
    my $io = shift;
    local $/ = undef;
    $io->seek(0, 0);
    $_ = <$io>;
    s/<!-- .*? -->//g;
    s/\s{2,}//g;
    s/(>)\s+/$1/g;
    s/\s+(<)/$1/g;
    $_;
}

# notdef & space
do {
    my $d = Font::TTF::CFF_::Dumper->new(file => my $io = IO::String->new);
    $cff->notdef_glyph(0);
    my $subset = $cff->subset(1);
    $d->dump_xml($subset);
    my $xml = get_xml($io);
    my @fd = $xml =~ /(<FontDict .*?>.*?<\/FontDict>)/g;
    my @cs = $xml =~ /(<CharString .*?>.*?<\/CharString>)/g;
    is   $cs[0],  q{<CharString name=".notdef" fdSelectIndex="0">endchar</CharString>}, ".notdef";
    like $cs[1], qr{<CharString name="cid00001" fdSelectIndex="1">-?\d+ endchar</CharString>}, "cid00001";

    my ($index) = $cs[1] =~ /fdSelectIndex="(\d+)"/;
    my %x = $fd[$index] =~ /<(\w+) value="(\d+)"\/>/g;
  SKIP: {
        my $have_some_feature = defined $cff->{w};
        my $why = "w is not defined";
        my $how_many = 1;
        skip $why, $how_many unless $have_some_feature;
        my $spacewidth = defined $cff->{w} ? $x{nominalWidthX} + $cff->{w} : $x{defaultWidthX};
        ok $spacewidth >= 200 && $spacewidth <= 700, "spacewidth"
            or diag explain { spacewidth => $spacewidth, w => $cff->{w} };
    };
};

# notdef_glyph(1);
do {
    my $d = Font::TTF::CFF_::Dumper->new(file => my $io = IO::String->new);
    $cff->notdef_glyph(1);
    my $subset = $cff->subset(0);
    $d->dump_xml($subset);
    my $xml = get_xml($io);
    my @fd = $xml =~ /(<FontDict .*?>.*?<\/FontDict>)/g;
    my @cs = $xml =~ /(<CharString .*?>.*?<\/CharString>)/g;
    my ($notdef) = $cs[0] =~ /<CharString .*?>(.*?)<\/CharString>/;
    isnt $notdef, "endchar", "notdef_glyph"
};

# small padding
do {
    my $d = Font::TTF::CFF_::Dumper->new(file => my $io = IO::String->new);
    $cff->padding(1);
    $cff->maxpadding(1);
    my $subset = $cff->subset(1);
    cmp_ok $cff->packing, '>', 1, "repacking required (small padding)";
};

# auto padding with notdef and space
do {
    my $d = Font::TTF::CFF_::Dumper->new(file => my $io = IO::String->new);
    $cff->padding(0);
    $cff->maxpadding(0);
    my $subset = $cff->subset(1);
    cmp_ok $cff->packing, '<=', 1, "auto padding (notdef and space)";
};

# auto padding with 10000 glyphs
do {
    my $d = Font::TTF::CFF_::Dumper->new(file => my $io = IO::String->new);
    $cff->padding(0);
    $cff->maxpadding(0);
    my $n = 1000;
    my $subset = $cff->subset(1 .. $n);
    cmp_ok $cff->packing, '<=', 1, "auto padding ($n glyphs)";
};

done_testing;
