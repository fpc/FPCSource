{$apptype console}
uses
  unicodeducet, fpwidestring, cp866,
  sysutils;
  
type  
  ts866 = type AnsiString(866);

  procedure doerror(ANumber : Integer);
  begin
    WriteLn('error ',ANumber);
    Halt(ANumber);
  end;

var
  s : ts866;
  i : Integer;
  ss : ShortString;
begin
  s := '123'#196#200#250;
  ss := s;
  if (Length(s) <> Length(ss)) then
    doerror(1);
  for i := 1 to Length(s) do
    begin
      if (Byte(ss[i]) <> Byte(s[i])) then
        doerror(2)
    end;

  WriteLn('Ok');
end.
