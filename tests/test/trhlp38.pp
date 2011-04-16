{ without "inherited" the methods of the helper are called first }
program trhlp38;

{$ifdef fpc}
  {$mode delphi}
{$endif}
{$apptype console}

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
    Result := Test(False)
  else
    Result := 2;
end;

var
  t: TTest;
  res: Integer;
begin
  res := t.Test(True);
  Writeln('t.Test: ', res);
  if res <> 2 then
    Halt(1);
  Writeln('ok');
end.
