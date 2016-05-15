uses
  SysUtils;
var
  s: string;
  c: currency;
begin
  c:=Currency(0.12) + Currency(0.14);
  s:=CurrToStr(c);
  s:=StringReplace(s, FormatSettings.DecimalSeparator, '.', []);
  writeln('s=', s);
  if s <> '0.26' then
    Halt(1);
  writeln('OK');
end.
