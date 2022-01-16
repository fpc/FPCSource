{ %skiptarget=android }
program tcpstr9;
{$mode delphiunicode}
{$apptype console}
uses 
  unicodeducet, fpwidestring;
  
begin
  // this test can be only run with the compiler built right now on the
  // same system
{$if not defined(FPC_CROSSCOMPILING) and not defined(FPC_CPUCROSSCOMPILING)}
  if StringCodePage(AnsiString('test')) <> DefaultSystemCodePage then
  begin
    WriteLn(StringCodePage(AnsiString('test')), ' <> ', DefaultSystemCodePage);
    halt(1);
  end;
{$endif}
  Writeln('ok');
end.

