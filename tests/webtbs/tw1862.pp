var x : array[1..1000] of double;
    z : array[1..100] of double absolute x;

begin
  z[10]:=10.0;
  if x[10]<>10.0 then
   begin
     writeln('ERROR!');
     halt(1);
   end;
end.
