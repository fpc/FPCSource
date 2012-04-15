program tcpstr15;
{$apptype console}
{$codepage cp1251}
{$mode delphi}
type
  t866 = type AnsiString(866);
var
  s866: t866;
  ss: ShortString;
begin
  s866 := 'привет ';
  ss := 'земляне';
  Insert(ss, s866, 8);
  if StringCodePage(s866) <> 866 then
    halt(1);
end.
