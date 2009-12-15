use strict; use warnings;
package Crow::Types;

use MooseX::Types::Moose qw/Str/;
use MooseX::Types -declare => [
  qw(Char CellChar),
];

subtype Char,
  as Str,
  where { length($_) == 1 },
  message { "Must be a single character" };

subtype CellChar,
  as Char,
  where { /^[[:alpha:]]$/ },
  message { "Must be a single alpha character" };

1;
