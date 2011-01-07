program tgeneric33;

{$mode objfpc}{$H+}
type
  // regular procdef
  generic TProc1<T> = function(Value: T): T;
  // object procdef
  generic TProc2<T> = function(Value: T): T of object;

  TFoo = class
    function Test2(Value: Integer): Integer;
  end;

function Test1(Value: Integer): Integer;
begin
  Result := Value + 1;
end;

function TFoo.Test2(Value: Integer): Integer;
begin
  Result := Value - 1;
end;

var
  Foo: TFoo;
  Proc1: specialize TProc1<Integer>;
  Proc2: specialize TProc2<Integer>;
begin
  Proc1 := @Test1;
  if Proc1(1) <> 2 then
    halt(1);
  Foo := TFoo.Create;
  Proc2 := @Foo.Test2;
  if Proc2(2) <> 1 then
    halt(2);
  Foo.Free;
end.
