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
  a := 'al';
  b := 'b2';
  c := '';
  
  //without "DestS" in the array
  c := a + b; 
  if (StringCodePage(c) <> 866) then
    halt(1);
  //with empty "DestS" in the array
  c := '';
  c := c + a ; 
  if (StringCodePage(c) <> 866) then
    halt(2);
  //with "DestS" in the array at the start
  c := c + a ; 
  if (StringCodePage(c) <> 866) then
    halt(3);
  //with "DestS" in the array, not at the start 
  c := a + c; 
  if (StringCodePage(c) <> 866) then
    halt(4);
  
  WriteLn('ok');
end.
