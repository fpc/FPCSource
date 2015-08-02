{$mode objfpc}
program Project1;
type
  generic TFoo<T> = class
  const
    Size = SizeOf(T);
   public
     function X: Integer;
  end;

{ TFoo }

function TFoo.X: Integer;
begin
  //Result := 100 div SizeOf(T);
  Result := 100 div Size;
end;

begin
end.
