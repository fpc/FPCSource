{ %VERSION=1.1}
var
   a1 : array of longint;
   i : longint;

begin
   setlength(a1,10);
   setlength(a1,0);
   setlength(a1,10);
   setlength(a1,100);
   setlength(a1,10);
   for i:=0 to 9 do
     a1[i]:=i*i;
   setlength(a1,1000);
   setlength(a1,10);
   for i:=0 to 9 do
     writeln(a1[i]);
end.
