{$mode delphi}

function REGVAR(InputBuffer : Pointer): double;
var
  x: integer;
  temp, y: double;
begin
  x := 1;
  y := 1;

  temp := exp((x/y));

  Result:= Temp;
end;

var
   Ptr1: pointer;
begin
    Ptr1 := 0;
    REGVAR(Ptr1);
    writeln('Test Complete.');
end.

