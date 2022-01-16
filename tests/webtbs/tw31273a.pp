{$mode delphi}

program Project1;
{$APPTYPE CONSOLE}

function SumX(const Arr: array of SizeInt): Integer;
begin
  if high(arr)<>-1 then
    halt(1);
  result:=1;
end;

var
  P: Pointer;
begin
  P := nil;
 Writeln(SumX(TBoundArray(P))); // Case 1
end.
