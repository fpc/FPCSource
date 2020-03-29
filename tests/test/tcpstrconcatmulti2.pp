{$APPTYPE CONSOLE}
// test "fpc_AnsiStr_Concat_multi" with a differant types(encodings) 
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
  cp1 = 1251;
  cp2 = 1252;
  cp3 = 65001;
{$else}
  cp1 = 866;
  cp2 = 850;
  cp3 = 1251;
{$endif}

type
  ts1 = type AnsiString(cp1);
  ts2 = type AnsiString(cp2);
  ts3 = type AnsiString(cp3);
var
  a : ts3;
  b : ts2;
  c, d : ts1;
begin
  a := 'al';
  b := 'b2';
  c := 'c3';
  
  //without "DestS" in the array
  d := a + b + c; 
  if (StringCodePage(d) <> cp1) then
    halt(1);
  //with empty "DestS" in the array
  d := '';
  d := d + a + b + c; 
  if (StringCodePage(d) <> cp1) then
    halt(2);
  //with "DestS" in the array at the start
  d := d + a + b + c; 
  if (StringCodePage(d) <> cp1) then
    halt(3);
  //with "DestS" in the array, not at the start 
  d := a + b + d + c; 
  if (StringCodePage(d) <> cp1) then
    halt(4);
    
  WriteLn('ok');
end.
