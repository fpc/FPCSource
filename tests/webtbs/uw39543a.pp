unit uw39543a;

interface

{$MODE objfpc}

uses
  uw39543b;

function Test: TVector4;

implementation

function Test: TVector4;
begin
  Result := Vector4(1, 2, 3, 4);
end;

end.