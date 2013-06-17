var
  r1 : record
    dummy : byte;
    a,b : longint;
  end;

begin
  r1.a:=aligned(r1.b)*2;
  aligned(r1.a):=r1.b*2;
  r1.a:=r1.b;
  r1.a:=r1.b div 10;
end.
