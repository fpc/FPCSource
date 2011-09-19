{$CODEPAGE cp866}
program tcpstrassignansistr;
type
  ts866 = type AnsiString(866);

  procedure doerror(ANumber : Integer);
  begin
    //WriteLn('error ',ANumber);
    Halt(ANumber);
  end;

var
  s, x : ts866;
  i : Integer;
begin
  s := #128#156#196;
  x := s;
  if (StringCodePage(s) <> 866) then
    doerror(1);
  if (StringCodePage(x) <> 866) then
    doerror(2);
  if (Length(x) <> Length(s)) then
    doerror(3);
  for i := 1 to Length(x) do
    begin
      if (Byte(x[i]) <> Byte(s[i])) then
        doerror(4)
    end;
end.
