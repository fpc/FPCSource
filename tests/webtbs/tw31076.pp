{ %FAIL }

program tw31076;

{$mode objfpc}

type
   generic TFClass<TC> = class
      generic function Res<TF>(): TF; // <<--
   end;
   
   generic function TFClass.Res<TF>: TF;
   begin
   
   end;

begin

end.
