{ %wpoparas=optvmts }
{ %wpopasses=1 }

{$MODE OBJFPC}
program test;

type
   TFoo = class
    type
      TSubFoo = class
      end;
   end;

begin
   TFoo.TSubFoo.Create();
end.

