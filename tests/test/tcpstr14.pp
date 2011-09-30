program tcpstr14;
{$apptype console}
{$mode delphi}{$H+}

type
  t866 = type AnsiString(866);
var
  s866: t866;
begin
  Str(123, s866);
  if StringCodePage(s866) <> 866 then
    halt(1);
end.
