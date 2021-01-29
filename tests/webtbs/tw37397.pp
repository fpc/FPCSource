{ %opt=-Ootailrec }

program gx;
//fpc -O3 ax.pas
// graphmath.pp(518,7) Fatal: Internal error 200108231
    
{$Mode OBJFPC} {$H+}
{$inline on}


  uses types,math;

type
  PPoint = ^TPoint;
  TFloatPoint = Record
    X, Y : Extended;
  end;
    
  TBezier = Array[0..3] of TFloatPoint;

  const res: array[0..50] of tpoint =
(
(x: 1; y: 10),
(x: 0; y: -321),
(x: -3; y: -454),
(x: -8; y: -567),
(x: -16; y: -661),
(x: -21; y: -701),
(x: -28; y: -737),
(x: -35; y: -769),
(x: -44; y: -797),
(x: -54; y: -821),
(x: -65; y: -842),
(x: -78; y: -858),
(x: -93; y: -872),
(x: -109; y: -882),
(x: -127; y: -889),
(x: -147; y: -893),
(x: -168; y: -895),
(x: -192; y: -893),
(x: -218; y: -890),
(x: -246; y: -884),
(x: -276; y: -875),
(x: -344; y: -853),
(x: -422; y: -823),
(x: -510; y: -788),
(x: -611; y: -747),
(x: -724; y: -704),
(x: -850; y: -658),
(x: -989; y: -611),
(x: -1143; y: -565),
(x: -1226; y: -542),
(x: -1313; y: -519),
(x: -1403; y: -497),
(x: -1498; y: -476),
(x: -1597; y: -456),
(x: -1700; y: -437),
(x: -1807; y: -419),
(x: -1919; y: -403),
(x: -2035; y: -388),
(x: -2156; y: -375),
(x: -2282; y: -363),
(x: -2413; y: -354),
(x: -2548; y: -346),
(x: -2688; y: -341),
(x: -2834; y: -339),
(x: -2984; y: -339),
(x: -3140; y: -341),
(x: -3301; y: -347),
(x: -3467; y: -355),
(x: -3639; y: -367),
(x: -3817; y: -382),
(x: -4000; y: -400)
);

Operator + (const Addend1, Addend2 : TFloatPoint) : TFloatPoint; inline;
Begin
  With Result do begin
    X := Addend1.X + Addend2.X;
    Y := Addend1.Y + Addend2.Y;
  end;
end;

Operator * (const Multiplicand : TFloatPoint; Multiplier : Extended) : TFloatPoint;
Begin
  With Result do begin
    X := Multiplicand.X * Multiplier;
    Y := Multiplicand.Y * Multiplier;
  end;
end;

Operator * (Multiplicand : Extended; const Multiplier : TFloatPoint) : TFloatPoint;
Begin
  Result := Multiplier*Multiplicand;
end;

Operator / (const Dividend : TFloatPoint; Divisor : Extended) : TFloatPoint;
begin
  With Result do begin
    X := Dividend.X / Divisor;
    Y := Dividend.Y / Divisor;
  end;
end;


Operator := (const Value : TFloatPoint) : TPoint; inline;
begin
  Result.X := Trunc(SimpleRoundTo(Value.X, 0));
  Result.Y := Trunc(SimpleRoundTo(Value.Y, 0));
end;

function Distance(const Pt1,Pt2 : TPoint) : Extended;
begin
  Result := Sqrt(Sqr(Pt2.X - Pt1.X) + Sqr(Pt2.Y - Pt1.Y));
end;

{------------------------------------------------------------------------------
  Method:   Distance
  Params:   PT, SP,EP
  Returns:  Extended

  Use Distance to get the distance between any point(PT) and a line defined
  by any two points(SP, EP). Intended for use in Bezier2Polyline, so params
  are TFloatPoint's, NOT TPoint's.

------------------------------------------------------------------------------}
function Distance(const Pt, SP, EP : TFloatPoint) : Extended;
var
  A, B, C : Extended;

  function Slope(PT1,Pt2 : TFloatPoint) : Extended;
  begin
    If Pt2.X <> Pt1.X then
      Result := (Pt2.Y - Pt1.Y) / (Pt2.X - Pt1.X)
    else
      Result := 1;
  end;

  function YIntercept(PT1,Pt2 : TFloatPoint) : Extended;
  begin
    Result := Pt1.Y - Slope(Pt1,Pt2)*Pt1.X;
  end;

begin
  A := -Slope(SP,EP);
  B := 1;
  C := -YIntercept(SP, EP);
  Result := ABS(A*Pt.X + B*Pt.Y + C)/Sqrt(Sqr(A) + Sqr(B));
end;


function BezierMidPoint(const Bezier : TBezier) : TFloatPoint;
begin
  Result := (Bezier[0] + 3*Bezier[1] + 3*Bezier[2] + Bezier[3]) / 8;
end;


procedure SplitBezier(const Bezier : TBezier; var Left, Right : TBezier);
var
  Tmp : TFloatPoint;
begin
  Tmp := (Bezier[1] + Bezier[2]) / 2;

  left[0]  := Bezier[0];
  Left[1]  := (Bezier[0] + Bezier[1]) / 2;
  left[2]  := (Left[1] + Tmp) / 2;
  Left[3]  := BezierMidPoint(Bezier);

  right[3] := Bezier[3];
  right[2] := (Bezier[2] + Bezier[3]) / 2;
  Right[1] := (Right[2] + Tmp) / 2;
  right[0] := BezierMidPoint(Bezier);
end;

procedure Bezier2Polyline(const Bezier : TBezier; var Points : PPoint;
  var Count : Longint);
var
  Pt : TPoint;

  procedure AddPoint(const Point : TFloatPoint);
  var
    P : TPoint;
  begin
    P := Point;
    if (Pt <> P) then
    begin
      Inc(Count);
      ReallocMem(Points, SizeOf(TPoint) * Count);
      Points[Count - 1] := P;
      Pt := P;
    end;
  end;

  function Colinear(BP : TBezier; Tolerance : Extended) : Boolean;
  var
    D : Extended;
  begin
    D := SQR(Distance(BP[1], BP[0], BP[3]));
    Result := D < Tolerance;
    D := SQR(Distance(BP[2], BP[0], BP[3]));
    If Result then
      Result := Result and (D < Tolerance);
  end;

  procedure SplitRecursive(B : TBezier);
  var
    Left,
    Right : TBezier;
  begin
    If Colinear(B, 1) then begin
      AddPoint(B[0]);
      AddPoint(B[3]);
    end
    else begin
      SplitBezier(B,left,right);
      SplitRecursive(left);
      SplitRecursive(right);
    end;
  end;

begin
  Pt := Point(-1,-1);
  If (not Assigned(Points)) or (Count <= 0) then
  begin
    Count := 0;

    if Assigned(Points) then
      ReallocMem(Points, 0);
  end;
  SplitRecursive(Bezier);
end;

  
  var
    points: ppoint;
    i, ppointcount: longint;
    bezier: TBezier;
begin
	bezier[0].X := 1.0;
	bezier[0].Y := 10.0;
	bezier[1].X := 2.0;
	bezier[1].Y := -2000.0;
	bezier[2].X := -30.0;
	bezier[2].Y := 30.0;
	bezier[3].X := -4000.0;
	bezier[3].Y := -400.0;
        Bezier2Polyline(bezier,points, ppointcount);
        for i:=0 to ppointcount-1 do
          begin
            if (points[i].x <> res[i].x) and
               (points[i].y <> res[i].y) then
              halt(i+1);
//            writeln(points[i].x,' ',points[i].y);
          end;
end.
