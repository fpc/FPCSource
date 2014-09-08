program TestStrIComp;
  uses
    {$ifdef unix}cwstring,{$endif}
    SysUtils;

var l: longint;
begin
  l := StrIComp(pwidechar('abcdefghijklmnopqrstuvwxyz'), pwidechar('ABCDEFGHIJKLMNOPQRSTUVWXYZ'));
  if (l <> 0) then
    begin
      writeln('error: expected 0, got ',l);
      halt(1);
    end;
  l := StrIComp(pwidechar('ABCDEFGHIJKLMNOPQRSTUVWXYZ'),pwidechar('abcdefghijklmnopqrstuvwxyz'));
  if (l <> 0) then
    begin
      writeln('error: expected 0, got ',l);
      halt(1);
    end;
end.
