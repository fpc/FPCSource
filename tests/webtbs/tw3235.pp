program TestStrIComp;
  uses
    SysUtils;

var l: longint;
begin
  l := StrIComp('abcdefghijklmnopqrstuvwxyz', 'ABCDEFGHIJKLMNOPQRSTUVWXYZ');
  if (l <> 0) then
    begin
      writeln('error: expected 0, got ',l);
      halt(1);
    end;
  l := StrIComp('ABCDEFGHIJKLMNOPQRSTUVWXYZ','abcdefghijklmnopqrstuvwxyz');
  if (l <> 0) then
    begin
      writeln('error: expected 0, got ',l);
      halt(1);
    end;
end.
