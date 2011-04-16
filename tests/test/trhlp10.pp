{ helpers may introduce new default properties (includes default properties
  introduced by the helper's parent) }
program trhlp10;

{$ifdef fpc}
  {$mode objfpc}
  {$modeswitch advancedrecords}
{$endif}
{$apptype console}

type
  TTest = record

  end;

  TTestHelper = record helper for TTest
    function GetTest(aIndex: Integer): Integer;
    property Test[Index: Integer]: Integer read GetTest; default;
  end;

  TTestHelperSub = record helper(TTestHelper) for TTest
  end;

function TTestHelper.GetTest(aIndex: Integer): Integer;
begin
  Result := aIndex;
end;

var
  t: TTest;
  res: Integer;
begin
  res := t[3];
  Writeln('value: ', res);
  if res <> 3 then
    Halt(1);
  Writeln('ok');
end.
