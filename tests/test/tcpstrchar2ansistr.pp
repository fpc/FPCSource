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
  x : ts866;
  c : ansichar;
  sa : ansistring;
begin
  c := 'a';
  sa := c;
  if (StringCodePage(sa) <> DefaultSystemCodePage) then
    doerror(1);
  if (Length(sa) <> 1) then
    doerror(2);
  if (Byte(sa[1]) <> Byte(c)) then
    doerror(3);
  x := c;
  if (StringCodePage(x) <> 866) then
    doerror(4);
  if (Length(x) <> 1) then
    doerror(5);
  if (Byte(x[1]) <> Byte(c)) then
    doerror(6);

  c := #156;
  sa := c;
  if (StringCodePage(sa) <> DefaultSystemCodePage) then
    doerror(10);
  if (Length(sa) <> 1) then
    doerror(20);
  if (Byte(sa[1]) <> Byte(c)) then
    doerror(30);
  x := c;
  if (StringCodePage(x) <> 866) then
    doerror(40);
  if (Length(x) <> 1) then
    doerror(50);
  if (Byte(x[1]) <> Byte(c)) then
    doerror(60);

  WriteLn('Ok');
end.
