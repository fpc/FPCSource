{$APPTYPE CONSOLE}
// test "fpc_AnsiStr_Concat_multi" with a same type(same encoding) 
uses
{$ifdef unix}
  cwstring,
{$endif unix}
  SysUtils;

type
  ts866 = type AnsiString(866);
var
  a, b, c, d : ts866;
begin
  a := 'al';
  b := 'b2';
  c := 'c3';
  
  //without "DestS" in the array
  d := a + b + c; 
  if (StringCodePage(d) <> 866) then
    halt(1);
  //with empty "DestS" in the array
  d := '';
  d := d + a + b + c; 
  if (StringCodePage(d) <> 866) then
    halt(2);
  //with "DestS" in the array at the start
  d := d + b + c; 
  if (StringCodePage(d) <> 866) then
    halt(3);
  //with "DestS" in the array, not at the start 
  d := a + b + d + c; 
  if (StringCodePage(d) <> 866) then
    halt(4);
  
  WriteLn('ok');
end.
