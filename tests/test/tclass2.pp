{ %VERSION=1.1 }
{$mode objfpc}

type
   to1 = class
      destructor destroy;override;
      procedure beforedestruction;override;
   end;

   to2 = class(to1)
      destructor destroy;override;
      procedure beforedestruction;override;
   end;

var
   i : longint;

   destructor to1.destroy;

     begin
        writeln('to1.destroy');
        if i<>2000 then
          halt(1);
        i:=3000;
        inherited destroy;
     end;

   procedure to1.beforedestruction;

     begin
        writeln('to1.beforedestruction');
        if i<>1000 then
          halt(1);
        i:=2000;
     end;

   destructor to2.destroy;

     begin
        writeln('to2.destroy');
        if i<>4000 then
          halt(1);
        i:=2000;
        inherited destroy;
        i:=5000;
     end;

   procedure to2.beforedestruction;

     begin
        writeln('to2.beforedestruction');
        if i<>3000 then
          halt(1);
        i:=4000;
     end;

var
   o1 : to1;
   o2 : to2;
begin
   o1:=to1.create;
   o2:=to2.create;
   i:=1000;
   o1.destroy;
   o2.destroy;
   if i<>5000 then
     halt(1);
   writeln('ok');
end.
