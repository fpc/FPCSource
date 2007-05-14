{ from GPC test suite }

{$mode objfpc}
Program WriteByte;

var
  a: array [ 0..3 ] of Byte = ( ord ( 'O' ), ord ( 'K' ), 42, 137 );

var
  S: String [ 255 ];

begin
  WriteStr ( S, a [ 0 ], a [ 1 ] );
  halt(ord(S <> '7975'));
end.
