{ %FAIL }

{ in mode ObjFPC the modeswitch advancedrecords is necessary for record
  helpers }
program trhlp5;

{$ifdef fpc}
  {$mode objfpc}
{$endif}

type
  TTest = record

  end;

  TTestHelper = record helper for TTest
  end;

begin
end.
