program concatenate_resourestrings_delphiunicode;

{$mode delphiunicode}
{$codepage cp1250}
{$ifdef USE_INTERNAL_UNICODE}
  {$define USE_FPWIDESTRING_UNIT}
  {$define USE_UNICODEDUCET_UNIT}
  {$define USE_CPALL_UNIT}
{$endif}

 {$ifndef USE_INTERNAL_UNICODE}
  {$ifdef unix}
uses
  {$ifdef darwin}iosxwstr{$else}cwstring{$endif};
  {$endif unix}
 {$else USE_INTERNAL_UNICODE}
uses
 {$ifdef USE_UNICODEDUCET_UNIT}
  unicodeducet,
 {$endif}
 {$ifdef USE_FPWIDESTRING_UNIT}
  fpwidestring,
 {$endif}
 {$ifdef USE_CPALL_UNIT}
  cpall;
 {$endif}
 {$endif def USE_FPWIDESTRING_UNIT}

resourcestring
  res2 = 'ûluùouËk˝ ' + 'konÌËek';

type
  tstr1250 = type ansistring(1250);
var
  str1250: tstr1250;
begin
  str1250 := 'ûluùouËk˝ ' + 'konÌËek';
  if res2<>str1250 then
    halt(1);
end.
