{ %FAIL }

{ record helpers may only inherit from other record helpers }
program trhlp25;

{$ifdef fpc}
  {$mode objfpc}
  {$modeswitch advancedrecords}
{$endif}

type
  TTest = record

  end;

  TTestHelper = record helper(TTest) for TTest
  end;

begin
end.
