{ %FAIL }

{ inside a helper's declaration the methods/fields of the extended class can't
  be accessed }
program tchlp9;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TTest = class
    Test: Integer;
    function GetTest: Integer;
  end;

  TTestHelper = class helper for TTest
    property AccessTest: Integer read Test;
  end;

begin
end.
