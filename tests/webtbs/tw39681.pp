{ %NORUN }

program tw39681;

{$mode objfpc}{$H+}
{$ModeSwitch implicitfunctionspecialization}

type
  generic TFunc<TResult> = function: TResult;

generic procedure Bar<T>(f: specialize TFunc<T>);
begin
end;

function Foo: Integer;
begin
end;

begin
  specialize Bar<Integer>(@Foo); // works
  Bar(@Foo); // Error
end.

