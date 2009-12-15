use MooseX::Declare;

class Crow::Cell::Light extends Crow::Cell {

    use MooseX::Types::Moose qw( Maybe );
    use Crow::Types          qw( Char CellChar );
    use feature 'switch';

    has 'char' => (
        is  => 'ro',
        isa => Maybe[CellChar],
        );

    method output {
        my $char = $self->char;
        given ($char) {
            when (undef) {
                return ' ';
            }
            default {
                return $char;
            }
        }
    }
}
1;
