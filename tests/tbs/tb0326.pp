var
   d1,d2 :double;
   i1,i2 : int64;
   c1,c2 : dword;

begin
  c1:=10;
  c2:=100;
  i1:=1000;
  i2:=10000;
  d1:=c1/c2;
  d2:=i1/i2;
  if d1<>d2 then
    begin
      writeln('error');
      halt(1);
    end;
end.
