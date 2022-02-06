{ %FAIL }

program tanonfunc42;

{$mode objfpc}
{$modeswitch anonymousfunctions}
{$modeswitch functionreferences}

{ test that capturing parent method's Result is rejected }

type
  tproc = reference to procedure;

function Foo: Integer;
var P: TProc;
begin
  Result := 0;
  P := procedure
  begin
    Result := 1
  end;
end;

begin
  Foo;
end.

