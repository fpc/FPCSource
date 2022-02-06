program tanonfunc30;

{$mode objfpc}
{$modeswitch anonymousfunctions}
{$modeswitch functionreferences}

{ test calling named function nested within an an anonymous method }

type
  TIntFunc = reference to function: Integer;

function Foo: TIntFunc;
begin
  Result := function: Integer
  var x: Integer;

    procedure bar;
    begin
      Inc(x, 2);
    end;

  begin
    x := 1;
    bar;
    Result := x;
  end;
end;

begin
  if foo()() <> 3 then
    halt(1);
end.

