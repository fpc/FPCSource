{ %FAIL }

{ inside a helper's declaration the methods/fields of the extended class can't
  be accessed }
program tchlp59;

{$ifdef fpc}
  {$mode delphi}
{$endif}
{$apptype console}

type
  TFoo = class
    Test: Integer;
    function GetTest: Integer;
  end;

  TFooHelper = class helper for TFoo
    property AccessTest: Integer read Test;
  end;

begin
end.
