{$codepage utf8}

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
 { The unit strings is not really used here,
   but simpifies the conditional construction
   for fpwidestring and unicodeducet use }
  strings;

type
  ts866 = type ansistring( {$ifdef android} 1251 {$else} 866 {$endif} );
var
  u: unicodestring;
  s: utf8string;
  rs: ts866;
  p: pointer;
  i: longint;
begin
  DefaultSystemCodePage:=CP_ASCII;
  s:='§èà£ù';
  rs:=ts866('Популярные фото');
  writestr(u,s,1,s,rs);
  if u <>'§èà£ù1§èà£ùПопулярные фото' then
    halt(1);
  getmem(p,length(s)-1);
  s:='';
  for i:=1 to (256 div 3) do
    s:=s+utf8string('㒨');
  s:=s+utf8string('㒨');
  { check that splitting the last 㒨 into two parts during writestr doesn't cause a
    conversion error }
  writestr(u,s);
  if utf8string(u)<>s then
    halt(2);
end.
