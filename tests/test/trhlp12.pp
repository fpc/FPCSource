{ constructors in record helpers }
program trhlp12;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TTest = record

  end;

  TTestHelper = record helper for TTest
    constructor CreateHelped(aTest: Integer);
  end;

constructor TTestHelper.CreateHelped(aTest: Integer);
begin

end;

begin
end.
