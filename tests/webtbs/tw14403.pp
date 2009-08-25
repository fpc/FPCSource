{$R+}
{$mode objfpc}

type
  TDummyShapeTree = class
    function ShapesCount(const OnlyActive, OnlyVisible, OnlyCollidable: boolean): Cardinal; virtual; abstract;
  end;

  TDummyShapeTreeGroup = class(TDummyShapeTree)
  public
    Child: TDummyShapeTree;

    function ShapesCount(const OnlyActive, OnlyVisible, OnlyCollidable: boolean): Cardinal; override;
  end;

  TDummyShape = class(TDummyShapeTree)
  public
    function ShapesCount(const OnlyActive, OnlyVisible, OnlyCollidable: boolean): Cardinal; override;
  end;

function TDummyShape.ShapesCount(
  const OnlyActive, OnlyVisible, OnlyCollidable: boolean): Cardinal;
begin
  Result := 1;
end;

function TDummyShapeTreeGroup.ShapesCount(
  const OnlyActive, OnlyVisible, OnlyCollidable: boolean): Cardinal;
var
  I: Integer;
  Something: Cardinal;
begin
  Result := 0;
  for I := 0 to 1 do
  begin
    Result := Result + Child.ShapesCount(OnlyActive, OnlyVisible, OnlyCollidable);
  end;
end;

var
  G: TDummyShapeTreeGroup;
begin
  G := TDummyShapeTreeGroup.Create;
  G.Child := TDummyShape.Create;
  Writeln(G.ShapesCount(true, true, false));
  G.Free;
end.
