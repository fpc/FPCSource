{ %FAIL }

{ record helpers can access only public fields - here: private }
program trhlp15;

{$ifdef fpc}
  {$mode delphi}
{$endif}

uses
  urhlp14;

type
  TTestHelper = record helper for TTest
    function AccessTest: Integer;
  end;

function TTestHelper.AccessTest: Integer;
begin
  Result := Test2;
end;

begin
end.

