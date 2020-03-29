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
  d := 'd4';
  
  //without "DestS" in the array
  c := '';
  c := a + b; 
  if (StringCodePage(c) <> cp1) then
    halt(1);
  c := '';
  c := a + d; 
  if (StringCodePage(c) <> cp1) then
    halt(2);
  c := '';
  c := d + b; 
  if (StringCodePage(c) <> cp1) then
    halt(3);
  //with empty "DestS" in the array
  c := '';
  c := c + a ; 
  if (StringCodePage(c) <> cp1) then
    halt(4);
  c := '';
  c := c + d ; 
  if (StringCodePage(c) <> cp1) then
    halt(5);
  //with "DestS" in the array at the start
  c := c + a ; 
  if (StringCodePage(c) <> cp1) then
    halt(6);
  //with "DestS" in the array, not at the start 
  c := a + c; 
  if (StringCodePage(c) <> cp1) then
    halt(7);
  
  WriteLn('ok');
end.
