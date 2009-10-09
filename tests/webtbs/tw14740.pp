var
  w: widestring;
  u: unicodestring;
  pw: pwidechar;
  pu: punicodechar;
begin
  pw:='abc'#0'def';
  setstring(w,pw,7);
  if w<>'abc'#0'def' then
    halt(1);

  pu:='abc'#0'def';
  setstring(u,pu,7);
  if u<>'abc'#0'def' then
    halt(2);
end.
