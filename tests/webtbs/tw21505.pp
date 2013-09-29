{$mode objfpc}{$H+}

operator >< (A, B: Integer): Integer;
begin
  Result := A - B;
end;

begin
  Writeln(2 >< 3);
  if (2 >< 3) <> -1 then
    halt(1);
end.
