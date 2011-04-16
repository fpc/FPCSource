{ %FAIL }

{ record helpers can access only public fields - here: strict private }
program trhlp14;

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
  Result := Test1;
end;

begin
end.
