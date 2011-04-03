{ %NORUN }

{ record helpers can access only public fields - here: public }
program trhlp16;

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
  Result := Test3;
end;

begin
end.

