{$APPTYPE CONSOLE}
uses
{$ifdef unix}
  cwstring,
{$endif unix}
  SysUtils;

type
  ts866 = type AnsiString(866);
  ts850 = type AnsiString(850);
  ts1251 = type AnsiString(1251);
var
  a : ts1251;
  b : ts850; 
  c, d : ts866;
begin
  a := 'al';
  b := 'b2';
  d := 'd4';
  
  //without "DestS" in the array
  c := '';
  c := a + b; 
  if (StringCodePage(c) <> 866) then
    halt(1);
  c := '';
  c := a + d; 
  if (StringCodePage(c) <> 866) then
    halt(2);
  c := '';
  c := d + b; 
  if (StringCodePage(c) <> 866) then
    halt(3);
  //with empty "DestS" in the array
  c := '';
  c := c + a ; 
  if (StringCodePage(c) <> 866) then
    halt(4);
  c := '';
  c := c + d ; 
  if (StringCodePage(c) <> 866) then
    halt(5);
  //with "DestS" in the array at the start
  c := c + a ; 
  if (StringCodePage(c) <> 866) then
    halt(6);
  //with "DestS" in the array, not at the start 
  c := a + c; 
  if (StringCodePage(c) <> 866) then
    halt(7);
  
  WriteLn('ok');
end.
