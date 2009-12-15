use strict; use warnings;
use MooseX::Types;

use MooseX::Types -declare => [
  qw(Char CellChar),
];

use MooseX::Types::Moose qw/Str/;

subtype Char,
  as Str,
  where { length($_) == 1 },
  message { "Must be a single character" };

subtype CellChar,
  as Char,
  where { /^[[:alpha:]]$/ },
  message { "Must be a single alpha character" };

1;
