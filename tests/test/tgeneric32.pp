program tgeneric32;
{$MODE DELPHI}
{$APPTYPE CONSOLE}
type
  TFoo<T> = class
    constructor Create;
  end;

constructor TFoo<T>.Create;
begin
  inherited Create;
end;

var
  FooInt: TFoo<Integer>;
begin
  // check inline specialization
  FooInt := TFoo<Integer>.Create;
  FooInt.Free;
end.
