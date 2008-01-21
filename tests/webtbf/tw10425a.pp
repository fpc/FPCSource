{ %norun }
{ %fail }

unit tw10425a;

{$mode delphi}

interface

type
  TFloat      = double;
  TPoint2D    = record x,y:TFloat; end;
  TRectangle  = array [1..2] of TPoint2D;

  TPoint2DArray  = array of TPoint2D;
  TPolygon2D     = array of TPoint2D;
  
  function AABB:TPoint2DArray; overload;

implementation

function AABB:TPolygon2D;
begin
end;

end.

