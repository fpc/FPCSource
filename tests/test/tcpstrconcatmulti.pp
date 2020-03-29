{$APPTYPE CONSOLE}
// test "fpc_AnsiStr_Concat_multi" with a same type(same encoding) 
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
  
const
{$ifdef android}
  cp = 1251;
{$else}
  cp = 866;
{$endif android}

type
  ts866 = type AnsiString(cp);
var
  a, b, c, d : ts866;
begin
  a := 'al';
  b := 'b2';
  c := 'c3';
  
  //without "DestS" in the array
  d := a + b + c; 
  if (StringCodePage(d) <> cp) then
    halt(1);
  //with empty "DestS" in the array
  d := '';
  d := d + a + b + c; 
  if (StringCodePage(d) <> cp) then
    halt(2);
  //with "DestS" in the array at the start
  d := d + b + c; 
  if (StringCodePage(d) <> cp) then
    halt(3);
  //with "DestS" in the array, not at the start 
  d := a + b + d + c; 
  if (StringCodePage(d) <> cp) then
    halt(4);
  
  WriteLn('ok');
end.
