{$apptype console}
uses
{$ifdef unix}
  cwstring,
{$endif unix}
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
  sa : ansistring;
  ss : ShortString;
begin
  sa := '123'#196#200#250;
  ss := sa;
  if (Length(sa) <> Length(ss)) then
    doerror(1);
  for i := 1 to Length(sa) do
    begin
      if (Byte(ss[i]) <> Byte(sa[i])) then
        doerror(2)
    end;

  s := '123'#196#200#250;
  ss := s;
  if (Length(s) <> Length(ss)) then
    doerror(3);
  for i := 1 to Length(s) do
    begin
      if (Byte(ss[i]) <> Byte(s[i])) then
        doerror(4)
    end;

  WriteLn('Ok');
end.
