{$MODE ObjFpc}

uses classes;

type

TVector3 = packed record
  X, Y, Z: Single;
end;

TClassA = class
protected
  fVector: TVector3;
public
  procedure SetVector(AVector: TVector3); virtual; abstract;
end;

{ TClassB }

TClassB = class(TClassA)
public
  procedure SetVector(AVector: TVector3); override;
end;

{ TClassB }

procedure TClassB.SetVector(AVector: TVector3);
begin
  writeln('TClassB: ',AVector.X,',',AVector.Y,',',AVector.Z);
  fVector:=AVector;
end;

var
  MyVector: TVector3;
  MyClassB: TClassB;
begin
  MyVector.X:=0;
  MyVector.Y:=0;
  MyVector.Z:=3;
  MyClassB:=TClassB.Create;
  MyClassB.SetVector(MyVector);
  if (MyClassB.fvector.x<>0) or
     (MyClassB.fvector.y<>0) or
     (MyClassB.fvector.z<>3) then
    halt(1);
end.
