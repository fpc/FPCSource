type
   to1 = class
      destructor destroy;override;
      procedure beforedestruction;override;
   end;

var
   i : longint;

   destructor to1.destroy;

     begin
        if i<>2000 then
          halt(1);
        i:=3000;
        inherited destroy;
     end;

   procedure to1.beforedestruction;

     begin
        if i<>1000 then
          halt(1);
        i:=2000;
     end;

var
   o1 : to1;

begin
   o1:=to1.create;
   i:=1000;
   o1.destroy;
   if i<>3000 then
     halt(1);
   writeln('ok');
end.

