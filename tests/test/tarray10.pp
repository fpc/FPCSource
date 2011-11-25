{$mode objfpc}{$h+}

procedure SetArray(out SingleArray: array of Single; const Value: Single);
var
  I: Integer;
begin
  for I := Low(SingleArray) to High(SingleArray) do
    SingleArray[I] := Value;
end;

var
  ValuesBuffer: array of Single;

begin
  SetLength(ValuesBuffer, 5);
  { passing <dynamic array of unmanaged type> to <out open array> should not trigger IE 201103063 }
  SetArray(ValuesBuffer, 5.7);
end.
