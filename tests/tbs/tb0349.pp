{ %VERSION=1.1}
var
   p : pwidechar;
   c1,c2 : widechar;
   i : longint;

begin
   p:=@c1;
   i:=0;
   c2:=p[i];
   p:='';
   p:='hello';
end.
