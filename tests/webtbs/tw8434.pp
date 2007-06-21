uses sysutils;

var
  x: double;
begin
  DecimalSeparator:='.';
  x := 0.099991;
  if (Format('%5.2f', [x]) <> ' 0.10') then
    halt(1);
  if (Format('%6.3f', [x]) <> ' 0.100') then
    halt(2);
end.
