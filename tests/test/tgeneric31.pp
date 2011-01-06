{ %fail }
program tgeneric31;

{$mode delphi}

type
  TGenericClass<T1,T2> = class
    function DoSomething(Arg: T1): T1;
  end;

// it must be TGenericClass<T1,T2>
function TGenericClass<T1>.DoSomething(Arg: T1): T1;
begin
  Result := Arg;
end;

begin
end.
