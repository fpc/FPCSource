program tcpstr9;
{$apptype console}
begin
  // this test can be only run with the compiler built right now on the
  // same system
  if StringCodePage('test') <> DefaultSystemCodePage then
  begin
    WriteLn(StringCodePage('test'), ' <> ', DefaultSystemCodePage);
    halt(1);
  end;
  Writeln('ok');
end.

