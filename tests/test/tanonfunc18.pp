program tanonfunc18;

{$mode objfpc}
{$modeswitch anonymousfunctions}
{$modeswitch functionreferences}

{ test independent state when multiple functions are created }

type
  TIntFunc = reference to function: Integer;

function Foo: TIntFunc;
var
  i: Integer;
begin
  Result := function: Integer
  begin
    Result := i;
    Inc(i);
  end;
  i := 100;
end;

var
  F1, F2: TIntFunc;
  i: Integer;
begin
  F1 := Foo();
  F2 := Foo();
  for i := 0 to 4 do
    F1();
  for i := 0 to 9 do
    if F1() <> F2() + 5 then
      halt(1);
end.

