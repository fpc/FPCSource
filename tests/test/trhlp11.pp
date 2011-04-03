{ %FAIL }

{ inside a helper's declaration the methods/fields of the extended record can't
  be accessed }
program trhlp11;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TTest = record
    Test: Integer;
  end;

  TTestHelper = record helper for TTest
    property AccessTest: Integer read Test;
  end;

begin
end.
