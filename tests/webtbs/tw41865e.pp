{ %OPT=-O2 }

program tw41865e;
{$mode objfpc}
function Half(const c: Currency): Currency;
begin
  Result := c / 2;
end;
var
  a, r: Currency;
begin
  a := 10;
  r := Half(a);
  WriteLn('raw=', PInt64(@r)^, ', expected=50000');
  if PInt64(@r)^ <> 50000 then
    Halt(1);
  writeln('ok');
end.