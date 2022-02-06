{ %FAIL }

{ anonymous reference function types as function arguments are not allowed }
program tfuncref4;

{$mode objfpc}
{$modeswitch functionreferences}

procedure Test(aArg: reference to procedure);
begin
end;

begin
end.
