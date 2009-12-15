use strict; use warnings;
use Data::Dumper;
use Crow::Cell;
use Crow::Cell::Light; # causes to die, because can't see CellChar constraint

my $crossword = <<EOF;
bar
a#i
zo 
EOF

my $grid = do {
    my $y=0;  
    my @lines = map {
        my $line = $_;
        my $x = 0;
        my @chars = map {
            Crow::Cell->parse_cell( x=>$x++, y=>$y, char=>$_ );
            } split //, $line;
        $y++;
        [ @chars ];
    } split /\n/, $crossword;
    \@lines;
    };

print join "\n", (
    map {
        join '', map {
            $_->output;
            } @$_
        } @$grid
    ), '';
