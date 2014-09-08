{$codepage utf8}

{$ifdef unix}
uses
  cwstring;
{$endif}

type
  ts866 = type ansistring(866);
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
