program tsetstring;

type
  tstr866 = type ansistring(866);

var
  str866: tstr866;
  a: ansistring;
  u: unicodestring;
  s: shortstring;
  pa: pansichar;
begin
  setstring(str866,'abcdef',5);
  if stringcodepage(str866)<>866 then
    halt(1);
  if str866<>'abcde' then
    halt(2);
  setstring(a,'abc',3);
  if (stringcodepage(a)<>0) and
     (stringcodepage(a)<>DefaultSystemCodePage) then
    halt(3);
  if a<>'abc' then
    halt(4);
  pa:='12345';
  setstring(u,pa,5);
  if u<>'12345' then
    halt(5);
  setstring(s,pa,5);
  if s<>'12345' then
    halt(7);
end.

