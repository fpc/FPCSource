unit fpvtocanvas;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fpcanvas,
  fpvectorial;

procedure DrawFPVectorialToCanvas(ASource: TvVectorialDocument; ADest: TFPCustomCanvas;
  ADestX: Integer = 0; ADestY: Integer = 0; AMulX: Double = 1.0; AMulY: Double = 1.0);

implementation

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
procedure DrawFPVectorialToCanvas(ASource: TvVectorialDocument; ADest: TFPCustomCanvas;
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
  // For entities
  CurEntity: TvEntity;
  CurCircle: TvCircle;
begin
  {$ifdef FPVECTORIALDEBUG}
  WriteLn(':>DrawFPVectorialToCanvas');
  {$endif}

  PosX := 0;
  PosY := 0;

  ADest.MoveTo(ADestX, ADestY);

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

  for i := 0 to ASource.GetEntityCount - 1 do
  begin
    CurEntity := ASource.GetEntity(i);
    CurCircle := CurEntity as TvCircle;
    if CurEntity is TvCircle then
    begin
      ADest.Ellipse(
        Round(ADestX + AmulX * (CurCircle.X - CurCircle.Radius)),
        Round(ADestY + AMulY * (CurCircle.Y - CurCircle.Radius)),
        Round(ADestX + AmulX * (CurCircle.X + CurCircle.Radius)),
        Round(ADestY + AMulY * (CurCircle.Y + CurCircle.Radius))
        );
    end;
  end;

  {$ifdef FPVECTORIALDEBUG}
  WriteLn(':<DrawFPVectorialToCanvas');
  {$endif}
end;

end.

