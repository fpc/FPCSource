{ %fail }

{$MODE DELPHI}

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
       { THis is not allowed }
       t(@execute);
     end;

   procedure texec1.execute;
     begin
     end;

begin
end.
