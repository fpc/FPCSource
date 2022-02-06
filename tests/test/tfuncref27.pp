{ %FAIL }

program tfuncref27;

{$mode objfpc}{$H+}
{$modeswitch anonymousfunctions}
{$modeswitch functionreferences}

type
  TTestFunc = reference to procedure;

function DoTest: TTestFunc;

  function TestSub: TTestFunc;
  begin
  end;

begin
  Result := @TestSub;
end;

begin
end.
