{$Ifdef fpc}{$mode objfpc}{$h+}{$endif}
uses
  Classes;
type
  TGLNode = class (TCollectionItem)
  private
    FCoords : array[0..2] of Byte;
    procedure SetCoordinate(Indx: Integer; AValue: Byte);
  protected
    function StoreCoordinate(Indx: Integer) : Boolean;
  published
    property X: Byte index 0 read FCoords[0] write SetCoordinate stored StoreCoordinate;
    property Y: Byte index 1 read FCoords[1] write SetCoordinate stored StoreCoordinate;
    property Z: Byte index 2 read FCoords[2] write SetCoordinate stored StoreCoordinate;
end;

{ TGLNode }

procedure TGLNode.SetCoordinate(Indx: Integer; AValue: Byte);
begin
  FCoords[Indx]:=AValue;
end;

function TGLNode.StoreCoordinate(Indx: Integer): Boolean;
begin
  result:=(FCoords[Indx] <> 0);
end;

var
  n : TGLNode;
begin
  n:=TGLNode.Create(nil);
  n.X:=1;
  n.Y:=2;
  n.Z:=3;
  writeln(n.X,',',n.Y,',',n.Z);
end.
