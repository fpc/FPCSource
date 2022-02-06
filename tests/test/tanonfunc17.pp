program tanonfunc17;

{$mode objfpc}
{$modeswitch anonymousfunctions}
{$modeswitch functionreferences}

{ test maintaining state between calls }

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
  F: TIntFunc;
  i: Integer;
begin
  F := Foo();
  for i := 0 to 9 do
    if F() <> (i + 100) then
      halt(i);
end.

