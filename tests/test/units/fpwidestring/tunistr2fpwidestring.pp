uses
  unicodeducet, fpwidestring;

var
  i : longint;
  w,w2 : unicodestring;
  a : ansistring;

begin
  setlength(w,1000);
  for i:=1 to 1000 do
    w[i]:=widechar(i);
  for i:=1 to 10 do
    begin
      a:=w;
      w2:=a;
    end;
  writeln('ok');
end.
