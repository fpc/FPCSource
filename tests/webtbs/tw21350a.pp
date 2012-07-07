{$mode delphi}

unit tw21350a;

interface

type

  { TPointEx }

  TPointEx<T> = object
    X, Y: T;
    function Create(const AX, AY: T): TPointEx<T>;
    class procedure Swap(var A, B: TPointEx<T>); static;
    class procedure OrderByY(var A, B: TPointEx<T>); static;
  end;

  TPoint = TPointEx<integer>;
  TPointF = TPointEx<single>;

implementation

function TPointEx<T>.Create(const AX, AY: T): TPointEx<T>;
begin
  result.X:=AX;
  result.Y:=AY;
end;

class procedure TPointEx<T>.Swap(var A, B: TPointEx<T>);
var
  tmp: TPointEx<T>;
begin
  tmp:=A;
  A:=B;
  B:=tmp;
end;

class procedure TPointEx<T>.OrderByY(var A, B: TPointEx<T>);
begin
  if A.Y > B.Y then
     TPointEx<T>.Swap(A,B);
end;


end.
