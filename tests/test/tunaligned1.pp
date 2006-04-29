var
  r1 : packed record
    dummy : byte;
    a,b : longint;
  end;

begin
  r1.a:=unaligned(r1.b)*2;
  unaligned(r1.a):=r1.b*2;
  r1.a:=r1.b;
  r1.a:=r1.b div 10;
end.
