{ from gpc testsuite, original name sam7.pas }

{$ifdef fpc}
{$mode macpas}
{$endif}

Program Sam7;

Var
  foo: array [ 'a'..'f' ] of Boolean = ( false, false, true, false, false, false );
  bar: packed array [ 42..47 ] of Boolean;
  baz: array [ '0'..'5' ] of Boolean;
  i: Integer;

begin
  pack ( foo, 'a', bar );
  unpack ( bar, baz, '0' );
  for i:= 0 to 5 do
    if bar [ 42 + i ] <> baz [ chr(ord('0')+ i) ] then
      foo [ 'c' ]:= false;
  if foo [ 'c' ] and bar [ 44 ] then
    writeln ( 'OK' )
  else
    begin
      writeln ( 'failed ', foo [ 'c' ], ' ', bar [ 44 ] );
      halt(1)
    end
end.
