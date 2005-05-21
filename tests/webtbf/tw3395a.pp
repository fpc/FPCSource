{ %fail }

{$mode delphi}

type
   texec1 = class
     protected
       procedure execute;
     public
       constructor create;
   end;

   procedure t(p: pointer);
     begin
     end;

   constructor texec1.Create;
     begin
       // This should give an invalid typecast, because a methodpointer are 2 pointers
       t(pointer(execute));
     end;

   procedure texec1.execute;
     begin
     end;

begin
end.
