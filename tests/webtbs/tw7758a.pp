{ %norun }

{$ifdef go32v2}
  {$define use_fpwidestring_unit}
  {$define use_unicodeducet_unit}
{$endif}
uses
  {$ifdef unix}
  {$ifdef darwin}iosxwstr{$else}cwstring{$endif},
  {$endif}
{$ifdef USE_FPWIDESTRING_UNIT}
  fpwidestring,
{$endif}
{$ifdef USE_UNICODEDUCET_UNIT}
  unicodeducet,
{$endif}
  sysutils;

{ just to make sure that no all wide->shortstring compile time conversions }
{ fail, but only those resulting in data loss                              }
const
  cw = widestring('abc');
  de = 'a'+shortstring(cw);
  wc = widechar('a');
  df = shortstring(wc)+'abcd';
  dg = char(wc)+'abcd';

begin
end.
