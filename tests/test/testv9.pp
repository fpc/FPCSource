{$ifdef fpc}
{$mode objfpc}
{$endif fpc}
uses
  Variants;

var
  a,av : array of longint;
  a2,av2 : array of array of longint;
  v,v2 : variant;
  i,j : longint;
begin
  setlength(a,1000);
  for i:=0 to high(a) do
    a[i]:=i;
  v:=a;
  for i:=0 to high(a) do
    if v[i]<>i then
      begin
        writeln('v[',i,']=',v[i]);
        halt(1);
      end;
  writeln('simple test ok');

  setlength(a2,10,30);
  for i:=0 to high(a2) do
    for j:=0 to high(a2[i]) do
      a2[i,j]:=i*j;
  writeln(length(a2));
  writeln(length(a2[0]));
  v2:=a2;
  for i:=0 to high(a2) do
    for j:=0 to high(a2[i]) do
    if v2[i,j]<>i*j then
      begin
        writeln('v2[',i,',',j,']=',v2[i,j]);
        halt(1);
      end;
  writeln('complex test ok');

  av:=v;
  writeln('1d dyn. array:=var. array ok');


  av2:=v2;
  if high(av2)<>VarArrayHighBound(v2,1) then
   halt(1);
  if high(av2[0])<>VarArrayHighBound(v2,2) then
   halt(1);
  writeln('2d dyn. array:=var. array ok');
  writeln('ok');
end.
