{$mode objfpc}{$h+}

unit tw21350b;

interface

type

  { TPointEx }

  generic TPointEx<T> = object
    X, Y: T;
    function Create(const AX, AY: T): TPointEx;
    class procedure Swap(var A, B: TPointEx); static;
    class procedure OrderByY(var A, B: TPointEx); static;
  end;

  //TPoint = specialize TPointEx<integer>;
  TPointF = specialize TPointEx<single>;

implementation

{ TPoint<T> }

function TPointEx.Create(const AX, AY: T): TPointEx;
begin
  result.X:=AX;
  result.Y:=AY;
end;

class procedure TPointEx.Swap(var A, B: TPointEx);
var
  tmp: TPointEx;
begin
  tmp:=A;
  A:=B;
  B:=tmp;
end;

class procedure TPointEx.OrderByY(var A, B: TPointEx);
begin
  if A.Y > B.Y then
     TPointEx.Swap(A,B);
end;


end.
