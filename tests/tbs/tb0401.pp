{ %version=1.1 }
var
   b1,b2 : boolean;
   c : char;

begin
   b1:=false;
   b2:=true;
   c:=char(b1 and b2);
   if c<>#0 then
     halt(1);
   c:=char(b1 or b2);
   if c<>#1 then
     halt(1);
   c:=char(b1);
   if c<>#0 then
     halt(1);
   c:=char(b2);
   if c<>#1 then
     halt(1);
end.
