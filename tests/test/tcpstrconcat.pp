{$APPTYPE CONSOLE}
{$ifdef go32v2}
  {$define USE_INTERNAL_UNICODE}
{$endif}

{$ifdef USE_INTERNAL_UNICODE}
  {$define USE_FPWIDESTRING_UNIT}
  {$define USE_UNICODEDUCET_UNIT}
  {$define USE_CPALL_UNIT}
{$endif}
uses
{$ifdef unix}
  {$ifdef darwin}iosxwstr{$else}cwstring{$endif},
{$endif unix}
 {$ifdef USE_UNICODEDUCET_UNIT}
  unicodeducet,
 {$endif}
 {$ifdef USE_FPWIDESTRING_UNIT}
  fpwidestring,
 {$endif}
 {$ifdef USE_CPALL_UNIT}
  cpall,
 {$endif}
  sysutils;

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
