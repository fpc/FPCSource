uses
  unicodeducet, fpwidestring;
  
var
  w : widestring;
  u : unicodestring;
  a : ansistring;
  
begin
  a:='A';
  w:=a;
  if w[1]<>#65 then
    halt(1);
  a:=w;
  if a[1]<>'A' then
    halt(2);
  writeln('ok');

  a:='A';
  u:=a;
  if u[1]<>#65 then
    halt(3);
  a:=u;
  if a[1]<>'A' then
    halt(4);
  writeln('ok');
end.
