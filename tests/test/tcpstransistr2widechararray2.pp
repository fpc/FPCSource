{ this file is stored in utf8, but we don't tell the compiler so that the string
  constant gets code page 0/CP_ACP; this test is to make sure that
  fpc_ansistr_to_widechararray() translates CP_ACP to the actual value of
  DefaultSystemCodePage before calling widestringmanager.ansi2widemoveproc
}

{$ifdef go32v2}
  {$define USE_INTERNAL_UNICODE}
{$endif}

{$ifdef USE_INTERNAL_UNICODE}
  {$define USE_FPWIDESTRING_UNIT}
  {$define USE_UNICODEDUCET_UNIT}
  {$define USE_CPALL_UNIT}
{$endif}
{$ifdef unix}
uses
  {$ifdef darwin}iosxwstr{$else}cwstring{$endif};
{$else}
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
{$endif}

{$r+}
var
  u8: ansistring;
  a: array[0..10] of unicodechar;
  u16: unicodestring;
  i: longint;
begin
  DefaultSystemCodePage:=CP_UTF8;
  u8:='èà';
  a:=u8;
  u16:=unicodestring(u8);
  for i:=0 to 1 do
    begin
      writeln('u16[',i-low(a)+low(u16),'] = $',hexstr(ord(u16[i-low(a)+low(u16)]),2));
      writeln('a[',i,'] = $',hexstr(ord(a[i]),2));
      if u16[i-low(a)+low(u16)]<>a[i] then
        halt(i+1);
    end;
  if a[2]<>#0 then
    halt(3);
end.
