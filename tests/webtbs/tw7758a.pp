{ %norun }

uses
  {$ifdef unix}
  {$ifdef darwin}iosxwstr{$else}cwstring{$endif},
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
