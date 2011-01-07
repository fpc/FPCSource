program tgeneric34;

{$mode delphi}

type
  TFoo<T> = record
    function DoSomething(Arg: T): T;
  end;

function TFoo<T>.DoSomething(Arg: T): T;
begin
  Result := Arg;
end;

var
  FooInt: TFoo<Integer>;
begin
  if FooInt.DoSomething(1) <> 1 then
    halt(1);
end.

