program tcpstr13;

// check that copy operation converts from 866 to DefaultSystemCodePage encoding

{$ifdef go32v2}
  {$define USE_INTERNAL_UNICODE}
{$endif}

{$ifdef USE_INTERNAL_UNICODE}
  {$define USE_FPWIDESTRING_UNIT}
  {$define USE_UNICODEDUCET_UNIT}
  {$define USE_CPALL_UNIT}
{$endif}
{$mode delphi}

{$ifndef USE_INTERNAL_UNICODE}
{$ifdef unix}
uses
  {$ifdef darwin}iosxwstr{$else}cwstring{$endif};
{$endif unix}
{$else def USE_INTERNAL_UNICODE}
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


type
  ts866 = type ansistring(866);

var
  s: ts866;
  a: ansistring;
begin
  s:='abc'#$00A9#$00AE'123';
//  if s[4] <> 'c' then
//    halt(1);
  a:=copy(s,1,4);
  if stringcodepage(a)<>DefaultSystemCodePage then
    halt(2);
end.
