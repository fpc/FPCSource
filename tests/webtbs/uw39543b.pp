unit uw39543b;

interface

{$MODE objfpc}

type
  TVector4 = record
    X, Y, Z, W: Single;
  end;

function Vector4(X, Y, Z, W: Single): TVector4;

implementation

uses
  uw39543a;

function Vector4(X, Y, Z, W: Single): TVector4;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
  Result.W := W;
end;

end.