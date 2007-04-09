var a,b: array[0..99] of byte;
    i: integer;
    c: byte;
begin
for i:=0 to 99 do a[i]:=i;
b:=a;

writeln('The same:',comparedword(a[0],b[0],25));
writeln('All other results should be negative.');

for i:=0 to 99 do
  begin
  c:=b[i];
  b[i]:=200;
  writeln(i:3,' ',comparedword(a[0],b[0],25));
  b[i]:=c;
  end;
end.