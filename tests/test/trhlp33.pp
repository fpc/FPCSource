{ %FAIL }

{ methods of the extended record can be called using "inherited", but only in
  mode ObjFPC }
program trhlp33;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TTest = record
    function Test(aRecurse: Boolean): Integer;
  end;

  TTestHelper = record helper for TTest
    function Test(aRecurse: Boolean): Integer;
  end;

function TTest.Test(aRecurse: Boolean): Integer;
begin
  Result := 1;
end;

function TTestHelper.Test(aRecurse: Boolean): Integer;
begin
  if aRecurse then
    Result := inherited Test(False)
  else
    Result := 2;
end;

begin
end.
