{ from gpc tests, original name pack4.pas }

{$ifdef fpc}
{$bitpacking on}
{$endif fpc}

Program PackUnpack;

Var
  foo: array [ 1..7 ] of Boolean;
  bar: packed array [ 1..3 ] of Boolean;
  i: Integer;
  temp: Boolean;

begin
  for i:= 1 to 3 do
    bar [ i ]:= true;
  for i:= 1 to 7 do
    foo [ i ]:= false;
  foo [ 4 ]:= true;
  foo [ 5 ]:= true;
  pack ( foo, 3, bar );
  if bar [ 3 ] and bar [ 2 ] and not bar [ 1 ] then
    begin
      for i:= 1 to 3 do
        begin
          temp:= not bar [ i ];
          bar [ i ]:= temp;
        end { for };
      unpack ( bar, foo, 5 );
      if not foo [ 1 ] and not foo [ 2 ] and not foo [ 3 ] and foo [ 4 ]
         and foo [ 5 ] and not foo [ 6 ] and not foo [ 7 ] then
        writeln ( 'OK' )
      else
        begin
          write ( 'failed: foo =' );
          for i:= 1 to 7 do
            write ( ' ', foo [ i ] );
          writeln;
          halt(1);
        end { else };
    end { if }
  else
    begin
      write ( 'failed: bar =' );
      for i:= 1 to 3 do
        write ( ' ', bar [ i ] );
      writeln;
      halt(1);
    end { else };
end.
