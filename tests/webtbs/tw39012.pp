unit tw39012;

{$mode objfpc}
{$modeswitch advancedrecords}

interface

type
  TPoint3D = record
    x,y,z: single;
//    procedure Offset(const point3D: TPoint3D);
//    procedure Scale(AScale: single);
  end;

  {** Scalar product: multiplies components and returns the sum }
  operator * (const v1,v2: TPoint3D): single; inline;

  procedure Normalize3D(var v: TPoint3D); inline;

implementation

operator * (const v1,v2: TPoint3D): single; inline;
begin
  result := v1.x*v2.x + v1.y*v2.y + v1.z*v2.z;
end;

procedure Normalize3D(var v: TPoint3D); inline;
var len: double;
begin
  len := v*v;
//  if len = 0 then exit;
//  len := sqrt(len);
//  v.x := v.x / len;
//  v.y := v.y / len;
//  v.z := v.z / len;
end;

end.
