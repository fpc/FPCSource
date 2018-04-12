program concatenate_resourestrings_delphiunicode;

{$mode delphiunicode}
{$codepage cp1250}
{$ifdef go32v2}
  {$define USE_FPWIDESTRING_UNIT}
  {$define USE_UNICODEDUCET_UNIT}
uses
{$ifdef USE_FPWIDESTRING_UNIT}
  fpwidestring,
{$endif}
{$ifdef USE_UNICODEDUCET_UNIT}
  unicodeducet;
{$endif}

{$endif}

{$ifdef unix}
uses
  {$ifdef darwin}iosxwstr{$else}cwstring{$endif};
{$endif}

resourcestring
  res2 = 'luouèkı ' + 'koníèek';

type
  tstr1250 = type ansistring(1250);
var
  str1250: tstr1250;
begin
  str1250 := 'luouèkı ' + 'koníèek';
  if res2<>str1250 then
    halt(1);
end.
