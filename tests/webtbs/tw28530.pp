{ %NORUN }

program tw28530;

{$mode objfpc}

type
  generic TDistanceFunction<t> = function (x,y : t) : Extended;

  generic PlanarCoordinate<t> = record
    x,y : t;
    d : specialize TDistanceFunction<t>;
  end;
  TScreenCoordinate = specialize PLanarCoordinate<word>;
  TDiscreteCoordinate = specialize PlanarCoordinate<integer>;
  TRealCoordinate = specialize PlanarCoordinate<extended>;

  TScreenDistance = specialize TDistanceFunction<word>;
  TDiscreteDistance = specialize TDistanceFunction<integer>;
  TRealDistance = specialize TDistanceFunction<Extended>;

  generic TPointSet<t> = array of specialize PlanarCoordinate<t>;

begin
end.
