{ %NORUN }

{$MODE OBJFPC} { -*- text -*- }
program tw19500;

type
   generic TFoo <T> = class
     type
      TBar = class
         function Baz(): T;
      end;
   end;

function TFoo.TBar.Baz(): T;
begin
   Result := nil;
end;

begin
end.
