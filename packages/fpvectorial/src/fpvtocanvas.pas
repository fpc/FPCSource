unit fpvtocanvas;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fpcanvas,
  fpvectorial;

procedure DrawFPVectorialToCanvas(ASource: TvVectorialDocument; ADest: TFPCustomCanvas;
  ADestX: Integer = 0; ADestY: Integer = 0; AMulX: Integer = 1; AMulY: Integer = 1);

implementation

procedure DrawFPVectorialToCanvas(ASource: TvVectorialDocument; ADest: TFPCustomCanvas;
  ADestX: Integer = 0; ADestY: Integer = 0; AMulX: Integer = 1; AMulY: Integer = 1);
var
  i, j, k: Integer;
  PosX, PosY: Integer; // Not modified by ADestX, etc
  CurSegment: TPathSegment;
  // For bezier
  CurX, CurY: Integer; // Not modified by ADestX, etc
  CurveLength: Integer;
  t: Double;
begin
  WriteLn(':>DrawFPVectorialToCanvas');

  PosX := 0;
  PosY := 0;

  ADest.MoveTo(ADestX, ADestY);

  for i := 0 to ASource.PathCount - 1 do
  begin
    //WriteLn('i = ', i);
    for j := 0 to Length(ASource.Paths[i].Points) - 1 do
    begin
      //WriteLn('j = ', j);
      CurSegment := ASource.Paths[i].Points[j];
      case CurSegment.SegmentType of
      st2DLine, st3DLine:
      begin
        PosX := Round(CurSegment.X);
        PosY := Round(CurSegment.Y);
        ADest.LineTo(
          ADestX + AMulX * PosX,
          ADestY + AMulY * PosY
          );
      end;
      { To draw a bezier we need to divide the interval in parts and make
        lines between this parts }
      st2DBezier, st3DBezier:
      begin
        CurveLength :=
          Round(sqrt(sqr(CurSegment.X3 - PosX) + sqr(CurSegment.Y3 - PosY))) +
          Round(sqrt(sqr(CurSegment.X2 - CurSegment.X3) + sqr(CurSegment.Y2 - CurSegment.Y3))) +
          Round(sqrt(sqr(CurSegment.X - CurSegment.X3) + sqr(CurSegment.Y - CurSegment.Y3)));

        for k := 1 to CurveLength do
        begin
          t := k / CurveLength;
          CurX := Round(sqr(1 - t) * (1 - t) * PosX + 3 * t * sqr(1 - t) * CurSegment.X2 + 3 * t * t * (1 - t) * CurSegment.X3 + t * t * t * CurSegment.X);
          CurY := Round(sqr(1 - t) * (1 - t) * PosY + 3 * t * sqr(1 - t) * CurSegment.Y2 + 3 * t * t * (1 - t) * CurSegment.Y3 + t * t * t * CurSegment.Y);
          ADest.LineTo(
            ADestX + AMulX * CurX,
            ADestY + AMulY * CurY);
        end;
        PosX := Round(CurSegment.X);
        PosY := Round(CurSegment.Y);
      end;
      end;
    end;
  end;

  WriteLn(':<DrawFPVectorialToCanvas');
end;

end.

