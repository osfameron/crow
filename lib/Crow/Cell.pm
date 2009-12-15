use MooseX::Declare;

class Crow::Cell {

    use MooseX::Types::Common::Numeric qw( PositiveInt );
    use MooseX::Types::Moose           qw( Str Int );
    use Crow::Types                    qw( Char );
    use feature 'switch';

    has x => (
        is  => 'rw',
        isa => PositiveInt,
        required => 1,
    );
    has y => (
        is  => 'rw',
        isa => PositiveInt,
        required => 1,
    );

    method parse_cell (ClassName $class: PositiveInt :$x, PositiveInt :$y, Char :$char) {

        require Crow::Cell::Light;
        require Crow::Cell::Dark;

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
