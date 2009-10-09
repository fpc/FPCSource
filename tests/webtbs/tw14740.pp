var
  w: widestring;
  u: unicodestring;
  pw: pwidechar;
  pu: punicodechar;
  p: pchar;
begin
  pw:='abc'#0'def';
  setstring(w,pw,7);
  if w<>'abc'#0'def' then
    halt(1);
  w:='';

  pu:='abc'#0'def';
  setstring(u,pu,7);
  if u<>'abc'#0'def' then
    halt(2);
  u:='';

  p:='abc'#0'def';
  setstring(w,p,7);
  if w<>'abc'#0'def' then
    halt(3);

  setstring(u,p,7);
  if u<>'abc'#0'def' then
    halt(4);
end.
