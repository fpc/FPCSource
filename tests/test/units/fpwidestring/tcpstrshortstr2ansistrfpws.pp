{$mode objfpc} {$H+}
uses
  unicodeducet, fpwidestring, cp1252, cp866,
  sysutils;
  
type  
  ts866 = type AnsiString(866);
  ts1252 = type AnsiString(1252);

  procedure doerror(ANumber : Integer);
  begin
    WriteLn('error ',ANumber);
    Halt(ANumber);
  end;

var
  s : ts866;
  x : ts1252;
  ss : shortstring;
  i : Integer;
begin
  ss := #128#156#196;

  s := ss;
  if (StringCodePage(s) <> 866) then
    doerror(1);
  if (Length(s) <> Length(ss)) then
    doerror(2);
  for i := 1 to Length(s) do
    begin
      if (Byte(s[i]) <> Byte(ss[i])) then
        doerror(3)
    end;

  x := ss;
  if (StringCodePage(x) <> 1252) then
    doerror(4);
  if (Length(x) <> Length(ss)) then
    doerror(5);
  for i := 1 to Length(x) do
    begin
      if (Byte(x[i]) <> Byte(ss[i])) then
        doerror(6)
    end;

  WriteLn('Ok');
end.
