program tcpstr10;
{$apptype console}
{$codepage cp866}
begin
  if StringCodePage('test') <> 866 then
  begin
    WriteLn(StringCodePage('test'), ' <> ', 866);
    halt(1);
  end;
  Writeln('ok');
end.

