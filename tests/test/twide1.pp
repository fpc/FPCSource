{$ifdef unix}
uses
  cwstring;
{$endif unix}
  
var
  w : widestring;
  a : ansistring;
  
begin
  a:='A';
  w:=a;
  if w[1]<>#65 then
    halt(1);
  a:=w;
  if a[1]<>'A' then
    halt(1);
  writeln('ok');
end.
