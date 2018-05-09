{$ifdef go32v2}
  {$define USE_INTERNAL_UNICODE}
{$endif}

{$ifdef USE_INTERNAL_UNICODE}
  {$define USE_FPWIDESTRING_UNIT}
  {$define USE_UNICODEDUCET_UNIT}
  {$define USE_CPALL_UNIT}
{$endif}
uses
 {$ifndef USE_INTERNAL_UNICODE}
   {$ifdef unix}
   {$ifdef darwin}iosxwstr{$else}cwstring{$endif},
  {$endif unix}
 {$endif ndef USE_INTERNAL_UNICODE}
 {$ifdef USE_UNICODEDUCET_UNIT}
  unicodeducet,
 {$endif}
 {$ifdef USE_FPWIDESTRING_UNIT}
  fpwidestring,
 {$endif}
 {$ifdef USE_CPALL_UNIT}
  cpall,
 {$endif}
  strings;
  
var
  w : widestring;
  u : unicodestring;
  a : ansistring;
  
begin
  a:='A';
  w:=a;
  if w[1]<>#65 then
    halt(1);
  a:=w;
  if a[1]<>'A' then
    halt(2);
  writeln('ok');

  a:='A';
  u:=a;
  if u[1]<>#65 then
    halt(3);
  a:=u;
  if a[1]<>'A' then
    halt(4);
  writeln('ok');
end.
