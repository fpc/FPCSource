var
   l1,l2 : longint;
   w1,w2 : word;
   b1,b2 : byte;
   b : boolean;

begin
   if b then
     w1:=w2;
   if b then
     w1:=w2;
   if b then
     begin
        w1:=w2;
        l1:=l2;
     end;
   if b then
     w1:=w2
   else
     w2:=w1;
   {
   if b then
     begin
        w1:=w2;
        l1:=l2;
        b1:=b2;
     end;
   }
end.
