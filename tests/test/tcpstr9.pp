{ %skiptarget=android }
{$ifdef go32v2}
  {$define USE_INTERNAL_UNICODE}
{$endif}

{$ifdef USE_INTERNAL_UNICODE}
  {$define USE_FPWIDESTRING_UNIT}
  {$define USE_UNICODEDUCET_UNIT}
  {$define USE_CPALL_UNIT}
{$endif}
program tcpstr9;
{$mode delphiunicode}
{$apptype console}
{$ifdef unix}
uses
  {$ifdef darwin}iosxwstr{$else}cwstring{$endif};
{$else}
uses
 {$ifdef USE_FPWIDESTRING_UNIT}
  fpwidestring,
 {$endif}
 {$ifdef USE_UNICODEDUCET_UNIT}
  unicodeducet,
 {$endif}
 {$ifdef USE_CPALL_UNIT}
  cpall,
 {$endif}
 { The unit strings is not really used here,
   but simpifies the conditional construction
   for fpwidestring and unicodeducet use }
  strings;
{$endif}

begin
  // this test can be only run with the compiler built right now on the
  // same system
  if StringCodePage(AnsiString('test')) <> DefaultSystemCodePage then
  begin
    WriteLn(StringCodePage(AnsiString('test')), ' <> ', DefaultSystemCodePage);
    halt(1);
  end;
  Writeln('ok');
end.

