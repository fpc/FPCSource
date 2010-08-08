{%fail}
program tvarpropsetter2;

{$ifdef fpc}
{$mode delphi}
{$endif}

{$VARPROPSETTER ON}

type
  TSomeClass = class
  private
    FTest: Integer;
    function GetTest: Integer;
    procedure SetTest(var AValue: Integer);
  public
    property Test: Integer read GetTest write SetTest;
  end;

{ TSomeClass }

function TSomeClass.GetTest: Integer;
begin
  Result := FTest;
end;

procedure TSomeClass.SetTest(var AValue: Integer);
begin
  FTest := AValue;
  AValue := 10;
end;

var
  Cl: TSomeClass;
begin
  Cl := TSomeClass.Create;
  Cl.Test := 5; // fails because requires a variable
  Cl.Free;
end.