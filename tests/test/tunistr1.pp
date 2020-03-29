{$ifdef go32v2}
  {$define USE_INTERNAL_UNICODE}
{$endif}

{$ifdef USE_INTERNAL_UNICODE}
  {$define USE_FPWIDESTRING_UNIT}
  {$define USE_UNICODEDUCET_UNIT}
  {$define USE_CPALL_UNIT}
{$endif}
{$ifndef USE_INTERNAL_UNICODE}
{$ifdef unix}
uses
  {$ifdef darwin}iosxwstr{$else}cwstring{$endif};
{$endif}
{$else USE_INTERNAL_UNICODE}
uses
 {$ifdef USE_UNICODEDUCET_UNIT}
  unicodeducet,
 {$endif}
 {$ifdef USE_FPWIDESTRING_UNIT}
  fpwidestring,
 {$endif}
 {$ifdef USE_CPALL_UNIT}
  cpall,
 {$endif}
 { The unit strings is not really used here,
   but simpifies the conditional construction
   for fpwidestring and unicodeducet use }
  strings;
{$endif def USE_INTERNAL_UNICODE}

var
  w : unicodestring;
  a : ansistring;

begin
  a:='A';
  w:=a;
  if w[1]<>#65 then
    halt(1);
  a:=w;
  if a[1]<>'A' then
    halt(1);
  writeln('ok');
end.
