{$mode delphi}
var
   a,c1,c2 : ansistring;
   aa : array[1..10] of ansistring;
   i : longint;

begin
   c1:='Hello ';
   c2:=' world';
   a:=c1+c2;
   finalize(a);
   if length(a)<>0 then
     halt(1);
   for i:=1 to 10 do
     aa[i]:=c1+c2;
   finalize(aa[1],10);
   for i:=1 to 10 do
     if length(aa[i])<>0 then
       halt(1);
end.

