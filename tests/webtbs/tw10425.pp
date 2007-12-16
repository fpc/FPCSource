{ %norun }

unit tw10425;

{$mode delphi}

interface

type
  TFloat      = double;
  TPoint2D    = record x,y:TFloat; end;
  TRectangle  = array [1..2] of TPoint2D;

  TPoint2DArray  = array of TPoint2D;
  TPolygon2D     = array of TPoint2D;
  
  function AABB(const Polygon   : TPolygon2D   ):TRectangle; overload;
  function AABB(const Curve     : TPoint2DArray):TRectangle; overload;

implementation

function AABB(const Polygon : TPolygon2D):TRectangle;
var
  i  : Integer;
begin
  Result[1].x := Polygon[0].x;
  Result[1].y := Polygon[0].y;
  Result[2].x := Polygon[0].x;
  Result[2].y := Polygon[0].y;
  for i := 1 to Length(Polygon) - 1 do
  begin
    if Polygon[i].x < Result[1].x then
      Result[1].x := Polygon[i].x
    else if Polygon[i].x > Result[2].x then
      Result[2].x := Polygon[i].x;
    if Polygon[i].y < Result[1].y then
      Result[1].y := Polygon[i].y
    else if Polygon[i].y > Result[2].y then
      Result[2].y := Polygon[i].y;
  end;
end;

function AABB(const Curve : TPoint2DArray):TRectangle;
var
  i  : Integer;
begin
  Result[1].x := Curve[0].x;
  Result[1].y := Curve[0].y;
  Result[2].x := Curve[0].x;
  Result[2].y := Curve[0].y;
  for i := 1 to Length(Curve) - 1 do
  begin
    if Curve[i].x < Result[1].x then
      Result[1].x := Curve[i].x
    else if Curve[i].x > Result[2].x then
      Result[2].x := Curve[i].x;
    if Curve[i].y < Result[1].y then
      Result[1].y := Curve[i].y
    else if Curve[i].y > Result[2].y then
      Result[2].y := Curve[i].y;
  end;
end;


end.

