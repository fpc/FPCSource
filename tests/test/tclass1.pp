{ %VERSION=1.1 }
{$mode objfpc}

type
   to1 = class
      constructor create;
      procedure afterconstruction;override;
   end;

var
   i : longint;

   constructor to1.create;

     begin
        inherited create;
        if i<>1000 then
          halt(1);
        i:=2000;
     end;

   procedure to1.afterconstruction;

     begin
        if i<>2000 then
          halt(1);
        i:=3000;
     end;

var
   o1 : to1;

begin
   i:=1000;
   o1:=to1.create;
   if i<>3000 then
     halt(1);
   o1.destroy;
   writeln('ok');
end.

