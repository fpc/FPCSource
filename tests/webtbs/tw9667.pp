{ %target=go32v2 }

{ compiled with smallest code option, control B does not work }
{ compiled with fastest code option, both controls work fine }
{ output with smallest code (note that control B output seems randomical)
1234567890
A >5/53<
B >M/77<
}
program tbug;

uses
	crt;

type
	TCharColor = record
    	car : char;
        color : byte;
    end;
	TScreen  = array[1..50,1..80] of TCharColor;

var
	CGA     : TScreen absolute $B800:0000;
	c : char;

begin
	clrscr;
    write( '1234567890');

    { control A }
    gotoxy( 1, 2);
    write( 'A >', CGA[ 1, 5].car, '/', ord( CGA[ 1, 5].car),  '<');

    { control B }
    gotoxy( 1, 3);
    c := CGA[ 1, 5].car;
    write( 'B >', c, '/', ord( c), '<');

    if (c<>'5') then
      halt(1);

    writeln;
end.
