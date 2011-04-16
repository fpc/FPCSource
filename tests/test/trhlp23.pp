{ %NORUN }

{ inheritance is supported for record helpers only in mode ObjFPC }
program trhlp23;

{$ifdef fpc}
  {$mode objfpc}
  {$modeswitch advancedrecords}
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
