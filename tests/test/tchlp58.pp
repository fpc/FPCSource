{ %NORUN }

{ tests whether class helpers can introduce properties }
program tchlp58;

{$ifdef fpc}
  {$mode delphi}
{$endif}
{$apptype console}

type
  TFoo = class
    Test: Integer;
  end;

  TFooHelper = class helper for TFoo
    function GetAccessTest: Integer;
    property AccessTest: Integer read GetAccessTest;
  end;

function TFooHelper.GetAccessTest: Integer;
begin
  Result := Test;
end;

var
  f: TFoo;
begin
  f := TFoo.Create;
  f.AccessTest;
end.
