uses
  variants;
var
  w : widestring;
  v : variant;
  a : ansistring;
begin
  a:='';
  w:='';
  v:='asdf';
  pos(a,v);
  pos(w,v);
  pos(v,v);
  pos(v,a);
  pos(v,w);
  pos(v,v);
end.

