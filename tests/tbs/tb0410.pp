{ %version=1.1 }
uses
   variants;

procedure p1(f : single);
  begin
  end;

procedure p2(l : longint);
  begin
  end;

var
   v : variant;
   l : longint;

begin
   v:=1;
   p1(v);
   p2(v);
   l:=v;
end.
