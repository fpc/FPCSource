unit tw23653;

{$mode objfpc}{$H+}

interface


type
  generic TGClass<T> = class
    procedure P;
  end;

implementation

uses
  gvector;

procedure TGClass.P;
type
  TMyVector = specialize TVector<T>;
var
  MyVector: TMyVector;
begin
end;

end.
