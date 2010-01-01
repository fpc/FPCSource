uses Math;

var
  Result, Remainder: SmallInt;
begin
  Result := -9 div 5;
  Remainder := -9 mod 5;
  writeln(result,' - ',remainder);
  if (result<>-1) or
     (remainder<>-4) then
    halt(1);
end.
