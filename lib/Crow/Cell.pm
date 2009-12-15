use MooseX::Declare;

class Crow::Cell {

    use MooseX::Types::Common::Numeric qw( PositiveInt );
    use MooseX::Types::Moose           qw( Str );
    use Crow::Types                    qw( Char );
    use feature 'switch';

    use Crow::Cell::Light;
    use Crow::Cell::Dark;

    has x => (
        is  => 'ro',
        isa => PositiveInt,
    );
    has y => (
        is  => 'ro',
        isa => PositiveInt,
    );

    method parse_cell (ClassName $class: PositiveInt :$x, PositiveInt :$y, Char :$char) {

        given ($char) {
            when ('#') {
                return Crow::Cell::Dark->new( x=>$x, y=>$y );
            }
            when (' ') {
                return Crow::Cell::Light->new( x=>$x, y=>$y );
            }
            default {
                return Crow::Cell::Light->new( x=>$x, y=>$y, char => $char );
            }
        }
    }
}
1;
