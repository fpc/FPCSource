{$mode objfpc}
{$h+}
uses
  sysutils;
var
  e : extended;
  s : string;
  l : longint;
begin
  sscanf('asdf 1'+DecimalSeparator+'2345 1234','%s %f %d',[@s,@e,@l]);
  if (e<>1.2345) or
    (l<>1234) or
    (s<>'asdf') then
    halt(1);
  // writeln(s,' ',e,' ',l);
  writeln('ok');
end.

