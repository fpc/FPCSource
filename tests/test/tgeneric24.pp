program tgeneric24;

{$mode objfpc}{$H+}
{$apptype console}

type
  generic TFoo<T> = record
    F: T;
  end;
var
  FooInt: specialize TFoo<Integer>;
  FooStr: specialize TFoo<String>;
begin
  FooInt.F := 1;
  WriteLn(FooInt.F);
  FooStr.F := 'test';
  WriteLn(FooStr.F);
end.

