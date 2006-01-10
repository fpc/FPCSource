{ %fail }

{$ifdef fpc}{$mode objfpc}{$H+}{$endif}

type
  TPoint = record
    X,Y : integer;
  end;

  { TSomeUselessObject }

  TSomeUselessObject = class(TObject)
    fSomeProperty: TPoint;
  private
    function GetSomeProperty: TPoint;
    procedure SetSomeProperty(AValue: TPoint);
  public
    constructor Create;
    property SomeProperty: TPoint read GetSomeProperty write SetSomeProperty;
  end;

{ TSomeUselessObject }

procedure TSomeUselessObject.SetSomeProperty(AValue: TPoint);
begin
  fSomeProperty := AValue;
end;

function TSomeUselessObject.GetSomeProperty: TPoint;
begin
  Result := fSomeProperty;
end;

constructor TSomeUselessObject.Create;
begin
  fSomeProperty.X := 50;
  fSomeProperty.Y := 100;
end;

var SomeUselessObject: TSomeUselessObject;

begin
  SomeUselessObject := TSomeUselessObject.Create;
  WriteLn('By Default X = ', SomeUselessObject.SomeProperty.X, ' and Y = ',
SomeUselessObject.fSomeProperty.Y);
  SomeUselessObject.SomeProperty.X := 200;
  SomeUselessObject.SomeProperty.Y := 500;
  WriteLn('Now X = ', SomeUselessObject.SomeProperty.X, ' and Y = ',
SomeUselessObject.fSomeProperty.Y);
end.
