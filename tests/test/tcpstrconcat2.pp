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
