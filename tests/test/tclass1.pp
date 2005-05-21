{ %VERSION=1.1 }
{$mode objfpc}

type
   to1 = class
      constructor create;
      procedure afterconstruction;override;
   end;

   to2 = class(to1)
      constructor create;
      procedure afterconstruction;override;
   end;

var
   i : longint;

   constructor to1.create;

     begin
        writeln('to1.create');
        inherited create;
        if i<>1000 then
          halt(1);
        i:=2000;
     end;

   constructor to2.create;

     begin
        writeln('to2.create');
        if i<>3000 then
          halt(1);
        i:=1000;
        inherited create;
        i:=4000;
     end;

   procedure to1.afterconstruction;

     begin
        writeln('to1.afterconstruction');
        if i<>2000 then
          halt(1);
        i:=3000;
     end;

   procedure to2.afterconstruction;

     begin
        writeln('to2.afterconstruction');
        if i<>4000 then
          halt(1);
        i:=5000;
     end;

var
   o1 : to1;
   o2 : to2;
begin
   i:=1000;
   o1:=to1.create;
   o2:=to2.create;
   if i<>5000 then
     halt(1);
   o1.destroy;
   writeln('ok');
end.
