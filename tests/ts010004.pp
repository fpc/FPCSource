{$mode objfpc}

{ tests forward class types }

type
   tclass1 = class;

   tclass2 = class
      class1 : tclass1;
   end;

var
   c : tclass1;

type
   tclass1 = class(tclass2)
      i : longint;
   end;

begin
   c:=tclass1.create;
   c.i:=12;
end.
