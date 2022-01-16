program TestStrIComp;

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
  SysUtils;

var l: longint;
begin
  l := StrIComp(pwidechar('abcdefghijklmnopqrstuvwxyz'), pwidechar('ABCDEFGHIJKLMNOPQRSTUVWXYZ'));
  if (l <> 0) then
    begin
      writeln('error: expected 0, got ',l);
      halt(1);
    end;
  l := StrIComp(pwidechar('ABCDEFGHIJKLMNOPQRSTUVWXYZ'),pwidechar('abcdefghijklmnopqrstuvwxyz'));
  if (l <> 0) then
    begin
      writeln('error: expected 0, got ',l);
      halt(1);
    end;
end.
