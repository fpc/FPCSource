{ %fail }

{$mode delphi}

program Project1;
{$APPTYPE CONSOLE}

function SumX(const Arr: array of SizeInt): Integer;
begin
  Result := Arr[Low(Arr)];
end;

var
  P: Pointer;
begin
  P := nil;
 Writeln(SumX(TBoundArray(nil))); // Case 2
end.
