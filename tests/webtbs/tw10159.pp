{$ifdef fpc}
{$mode delphi}
{$endif}

uses
  Math;

var
  J, K, L: integer;
  X, Y: extended;
  errors: integer;

begin
  errors:=0;
  for J := 0 to 9 do
    for K := 0 to 9 do
      for L := 0 to 9 do
        begin
          X := ( J / 10 + K / 100 );
          Y := X + L / 1000;
          
          if L >= 5 then
            X := X + 1 / 100;
          
          if abs( SimpleRoundTo( Y, -2 ) - X ) > 0.005 then
            begin
              writeln( '0.', J, K, L, ' ', Y, SimpleRoundTo( Y, -2 ), Y:5:2 );
              inc(errors);
            end;
          if abs( SimpleRoundTo( -Y, -2 ) - (-X) ) > 0.005 then
            begin
              writeln( '0.', J, K, L, ' ', -Y, ' ', SimpleRoundTo( -Y, -2 ), ' ', (-Y):5:2 );
              inc(errors);
            end;
          if (abs(SimpleRoundTo( -Y, -2 ))<>abs(SimpleRoundTo( Y, -2 ))) then
            halt(1);
        end;
  { don't do anything with the errors yet, because there are many in any
    case. For proper fixing, it needs to use some method like in
    John Herbster's DecimalRounding unit
  }
  writeln('errors: ',errors);
end. // Test.

