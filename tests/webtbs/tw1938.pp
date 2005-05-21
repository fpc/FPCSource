
{$inline on }

var A: array [0..1] of Integer;

function F: Integer; inline;
begin
  F := A[1];
end;

begin
  A[0] := 1234;
  A[1] := 5678;
  WriteLn(F); { writes 1234 }
  if F<>5678 then
   begin
     Writeln('ERROR!');
     Halt(1);
   end;
end.
