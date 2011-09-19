{$APPTYPE CONSOLE}
uses
{$ifdef unix}
  cwstring,
{$endif unix}
  SysUtils;

type
  ts866 = type AnsiString(866);
var
  a, b, c : ts866;
begin
  a := '';
  b := 'b2';
  c := '';
  c := a + b; 
  if (StringCodePage(c) <> 866) then
    halt(1);

  a := '';
  b := 'b2';
  c := 'azerty';
  c := a + b; 
  if (StringCodePage(c) <> 866) then
    halt(1);
    
  a := 'x';
  b := '';
  c := '';
  c := a + b; 
  if (StringCodePage(c) <> 866) then
    halt(2);

  a := 'x';
  b := '';
  c := '123';
  c := a + b; 
  if (StringCodePage(c) <> 866) then
    halt(2);
    
  WriteLn('ok');
end.
