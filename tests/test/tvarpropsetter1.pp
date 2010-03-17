program tvarpropsetter1;

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
  D: Integer;
begin
  Cl := TSomeClass.Create;
  D := 5;
  Cl.Test := D;
  if Cl.Test <> 5 then
    halt(1);
  if D <> 10 then
    halt(2);
  Cl.Free;
end.