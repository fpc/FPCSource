unit fpvtocanvas;

{$mode objfpc}{$H+}

interface

{.$define USE_LCL_CANVAS}

uses
  Classes, SysUtils, Math,
  {$ifdef USE_LCL_CANVAS}
  Graphics, LCLIntf,
  {$else}
  fpcanvas,
  {$endif}
  fpvectorial;

procedure DrawFPVectorialToCanvas(ASource: TvVectorialDocument;
  {$ifdef USE_LCL_CANVAS}
  ADest: TCanvas;
  {$else}
  ADest: TFPCustomCanvas;
  {$endif}
  ADestX: Integer = 0; ADestY: Integer = 0; AMulX: Double = 1.0; AMulY: Double = 1.0);

implementation

function Rotate2DPoint(P,Fix :TPoint; alpha:double): TPoint;
var
  sinus, cosinus : Extended;
begin
  SinCos(alpha, sinus, cosinus);
  P.x := P.x - Fix.x;
  P.y := P.y - Fix.y;
  result.x := Round(p.x*cosinus + p.y*sinus)  +  fix.x ;
  result.y := Round(-p.x*sinus + p.y*cosinus) +  Fix.y;
end;

procedure DrawRotatedEllipse(
  {$ifdef USE_LCL_CANVAS}
  ADest: TCanvas;
  {$else}
  ADest: TFPCustomCanvas;
  {$endif}
  CurEllipse: TvEllipse;
  ADestX: Integer = 0; ADestY: Integer = 0; AMulX: Double = 1.0; AMulY: Double = 1.0);
var
  PointList: array[0..6] of TPoint;
  f: TPoint;
  dk, x1, x2, y1, y2: Integer;
begin
  {$ifdef USE_LCL_CANVAS}
  CurEllipse.CalculateBoundingRectangle();
  x1 := CurEllipse.BoundingRect.Left;
  x2 := CurEllipse.BoundingRect.Right;
  y1 := CurEllipse.BoundingRect.Top;
  y2 := CurEllipse.BoundingRect.Bottom;

  dk := Round(0.654 * Abs(y2-y1));
  f.x := Round(CurEllipse.CenterX);
  f.y := Round(CurEllipse.CenterY - 1);
  PointList[0] := Rotate2DPoint(Point(x1, f.y), f, CurEllipse.Angle) ;  // Startpoint
  PointList[1] := Rotate2DPoint(Point(x1,  f.y - dk), f, CurEllipse.Angle);
  //Controlpoint of Startpoint first part
  PointList[2] := Rotate2DPoint(Point(x2- 1,  f.y - dk), f, CurEllipse.Angle);
  //Controlpoint of secondpoint first part
  PointList[3] := Rotate2DPoint(Point(x2 -1 , f.y), f, CurEllipse.Angle);
  // Firstpoint of secondpart
  PointList[4] := Rotate2DPoint(Point(x2-1 , f.y + dk), f, CurEllipse.Angle);
  // Controllpoint of secondpart firstpoint
  PointList[5] := Rotate2DPoint(Point(x1, f.y +  dk), f, CurEllipse.Angle);
  // Conrollpoint of secondpart endpoint
  PointList[6] := PointList[0];   // Endpoint of
   // Back to the startpoint
  ADest.PolyBezier(Pointlist[0]);
  {$endif}
end;

{@@
  This function draws a FPVectorial vectorial image to a TFPCustomCanvas
  descendent, such as TCanvas from the LCL.

  Be careful that by default this routine does not execute coordinate transformations,
  and that FPVectorial works with a start point in the bottom-left corner, with
  the X growing to the right and the Y growing to the top. This will result in
  an image in TFPCustomCanvas mirrored in the Y axis in relation with the document
  as seen in a PDF viewer, for example. This can be easily changed with the
  provided parameters. To have the standard view of an image viewer one could
  use this function like this:

  DrawFPVectorialToCanvas(ASource, ADest, 0, ASource.Height, 1.0, -1.0);
}
procedure DrawFPVectorialToCanvas(ASource: TvVectorialDocument;
  {$ifdef USE_LCL_CANVAS}
  ADest: TCanvas;
  {$else}
  ADest: TFPCustomCanvas;
  {$endif}
  ADestX: Integer = 0; ADestY: Integer = 0; AMulX: Double = 1.0; AMulY: Double = 1.0);
var
  i, j, k: Integer;
  PosX, PosY: Integer; // Not modified by ADestX, etc
  CurSegment: TPathSegment;
  Cur2DSegment: T2DSegment absolute CurSegment;
  Cur2DBSegment: T2DBezierSegment absolute CurSegment;
  // For bezier
  CurX, CurY: Integer; // Not modified by ADestX, etc
  CurveLength: Integer;
  t: Double;
  // For text
  CurText: TvText;
  // For entities
  CurEntity: TvEntity;
  CurCircle: TvCircle;
  CurEllipse: TvEllipse;
  CurArc: TvCircularArc;
begin
  {$ifdef FPVECTORIALDEBUG}
  WriteLn(':>DrawFPVectorialToCanvas');
  {$endif}

  PosX := 0;
  PosY := 0;

  ADest.MoveTo(ADestX, ADestY);

  // Draws all paths
  for i := 0 to ASource.PathCount - 1 do
  begin
    //WriteLn('i = ', i);
    ASource.Paths[i].PrepareForSequentialReading;

    for j := 0 to ASource.Paths[i].Len - 1 do
    begin
      //WriteLn('j = ', j);
      CurSegment := TPathSegment(ASource.Paths[i].Next());

      case CurSegment.SegmentType of
      stMoveTo:
      begin
        ADest.MoveTo(
          Round(ADestX + AMulX * Cur2DSegment.X),
          Round(ADestY + AMulY * Cur2DSegment.Y)
          );
      end;
      st2DLine, st3DLine:
      begin
        ADest.LineTo(
          Round(ADestX + AMulX * Cur2DSegment.X),
          Round(ADestY + AMulY * Cur2DSegment.Y)
          );
      end;
      { To draw a bezier we need to divide the interval in parts and make
        lines between this parts }
      st2DBezier, st3DBezier:
      begin
        CurveLength :=
          Round(sqrt(sqr(Cur2DBSegment.X3 - PosX) + sqr(Cur2DBSegment.Y3 - PosY))) +
          Round(sqrt(sqr(Cur2DBSegment.X2 - Cur2DBSegment.X3) + sqr(Cur2DBSegment.Y2 - Cur2DBSegment.Y3))) +
          Round(sqrt(sqr(Cur2DBSegment.X - Cur2DBSegment.X3) + sqr(Cur2DBSegment.Y - Cur2DBSegment.Y3)));

        for k := 1 to CurveLength do
        begin
          t := k / CurveLength;
          CurX := Round(sqr(1 - t) * (1 - t) * PosX + 3 * t * sqr(1 - t) * Cur2DBSegment.X2 + 3 * t * t * (1 - t) * Cur2DBSegment.X3 + t * t * t * Cur2DBSegment.X);
          CurY := Round(sqr(1 - t) * (1 - t) * PosY + 3 * t * sqr(1 - t) * Cur2DBSegment.Y2 + 3 * t * t * (1 - t) * Cur2DBSegment.Y3 + t * t * t * Cur2DBSegment.Y);
          ADest.LineTo(
            Round(ADestX + AMulX * CurX),
            Round(ADestY + AMulY * CurY));
        end;
        PosX := Round(Cur2DBSegment.X);
        PosY := Round(Cur2DBSegment.Y);
      end;
      end;
    end;
  end;

  // Draws all entities
  for i := 0 to ASource.GetEntityCount - 1 do
  begin
    CurEntity := ASource.GetEntity(i);
    if CurEntity is TvCircle then
    begin
      CurCircle := CurEntity as TvCircle;
      ADest.Ellipse(
        Round(ADestX + AmulX * (CurCircle.CenterX - CurCircle.Radius)),
        Round(ADestY + AMulY * (CurCircle.CenterY - CurCircle.Radius)),
        Round(ADestX + AmulX * (CurCircle.CenterX + CurCircle.Radius)),
        Round(ADestY + AMulY * (CurCircle.CenterY + CurCircle.Radius))
        );
    end
    else if CurEntity is TvEllipse then
    begin
      CurEllipse := CurEntity as TvEllipse;
      DrawRotatedEllipse(ADest, CurEllipse);
    end
    else if CurEntity is TvCircularArc then
    begin
      CurArc := CurEntity as TvCircularArc;
      {$ifdef USE_LCL_CANVAS}
      // Arc(ALeft, ATop, ARight, ABottom, Angle16Deg, Angle16DegLength: Integer);
      ADest.Arc(
        Round(ADestX + AmulX * (CurArc.CenterX - CurArc.Radius)),
        Round(ADestY + AmulY * (CurArc.CenterY - CurArc.Radius)),
        Round(ADestX + AmulX * (CurArc.CenterX + CurArc.Radius)),
        Round(ADestY + AmulY * (CurArc.CenterY + CurArc.Radius)),
        Round(16*CurArc.StartAngle),
        Round(16*CurArc.EndAngle - CurArc.StartAngle)
        );
      {$endif}
    end;
  end;

  // Draws all text
  for i := 0 to ASource.GetTextCount - 1 do
  begin
    CurText := ASource.GetText(i);
    ADest.Font.Height := Round(AmulY * CurText.FontSize);
    ADest.Pen.Style := psSolid;
    ADest.Pen.Color := clBlack;
    ADest.TextOut(Round(CurText.X), Round(CurText.Y), CurText.Value);
  end;

  {$ifdef FPVECTORIALDEBUG}
  WriteLn(':<DrawFPVectorialToCanvas');
  {$endif}
end;

end.

