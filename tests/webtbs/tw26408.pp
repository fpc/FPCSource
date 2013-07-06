{$mode objfpc}
program bugavx64;

uses math;

function EllipseRadialLength(Width, Height:Integer; EccentricAngle : Extended) : Extended;
var
  a, b, R : Extended;
begin
  a := Width div 2;
  b := Height div 2;
  R := Sqr(a)*Sqr(b);
  if R <> 0 then begin // tweak
    R := Sqrt(R / ((Sqr(b)*Sqr(Cos(DegToRad(EccentricAngle/16))))) +
      (Sqr(a)*Sqr(Sin(DegToRad(EccentricAngle/16)))));
  end;
  Result := R;
end;

begin
end.
