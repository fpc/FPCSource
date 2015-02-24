uses
{$ifdef unix}
  {$ifdef darwin}iosxwstr{$else}cwstring{$endif},
{$endif unix}
  sysutils;
  
type  
  ts850 = type AnsiString(850);
  ts1252 = type AnsiString(1252);

  procedure doerror(ANumber : Integer);
  begin
    WriteLn('error ',ANumber);
    Halt(ANumber);
  end;

var
  x, y, z : ts850;
  i : Integer;
  c : Integer;
  sa : ts850;
begin
  sa := 'abc'#$00A9#$00AE'123';
  x := Copy(sa,1,2);
  if (StringCodePage(x) <> 850) then
    doerror(1);
  if (Length(x) <> 2) then
    doerror(2);
  x := Copy(sa,2,3);
  y := 'abc'#$00A9#$00AE'123';
  z := Copy(y,2,3);
  for i := 1 to Length(x) do
    begin
      if (Byte(x[i]) <> Byte(y[i+1])) then
        doerror(3);
    end;

  WriteLn('Ok');
end.
