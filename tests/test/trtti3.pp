{ %VERSION=1.1 }

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
     begin
       writeln('length of a: ',length(a),' instead of 0');
       halt(1);
     end;
   for i:=1 to 10 do
     aa[i]:=c1+c2;
   finalize(aa[1],10);
   for i:=1 to 10 do
     if length(aa[i])<>0 then
       begin
         writeln('error at element: ',i,' contents: ''',aa[i],'''  instead of ''''');
         halt(1);
       end;
end.
