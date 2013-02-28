{ %skiptarget=android }
program tcpstr9;
{$mode delphiunicode}
{$apptype console}
{$ifdef unix} uses cwstring; {$endif}
begin
  // this test can be only run with the compiler built right now on the
  // same system
  if StringCodePage(AnsiString('test')) <> DefaultSystemCodePage then
  begin
    WriteLn(StringCodePage(AnsiString('test')), ' <> ', DefaultSystemCodePage);
    halt(1);
  end;
  Writeln('ok');
end.

