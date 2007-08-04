program test_formatloat;
{$mode objfpc}{$H+}
uses
  SysUtils;
var
  ef : Extended;
begin
  ef := 12;
  if (FormatFloat('#.#######E-0',ef) <> '1.2E1') then
    halt(1);
end.
