{ %version=1.1 }
procedure p1(f : single);
  begin
  end;

procedure p2(l : longint);
  begin
  end;

var
   v : variant;
   i : imyinterface;
   l : longint;

begin
   p1(v);
   p2(v);
   l:=v;
end.

