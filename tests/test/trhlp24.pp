{ %FAIL }

{ inheritance is supported for record helpers only in mode ObjFPC }
program trhlp24;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TTest = record

  end;

  TTestHelper = record helper for TTest
  end;

  TTestHelperSub = record helper(TTestHelper) for TTest
  end;

begin
end.
