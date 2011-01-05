{ %fail }
program tgeneric30;

{$mode delphi}

type
  TGenericClass<T> = class
    function DoSomething(Arg: T): T;
  end;

// it must be TGenericClass<T> here
function TGenericClass.DoSomething(Arg: T): T;
begin
  Result := Arg;
end;

begin
end.
