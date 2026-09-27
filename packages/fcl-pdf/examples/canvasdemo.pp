{
    Demonstrates the canvas features of fcl-image on TPDFCanvas of fcl-pdf:
    a PDF document with one page per feature set.
    See the file COPYING.FPC, included in this distribution, for details.
}
program canvasdemo;

{$mode objfpc}{$h+}

uses
  {$IFDEF UNIX}cwstring,{$ENDIF} Classes, SysUtils, Math, Types, FPImage, FPCanvas, FPImgCanv,
  FPPixlCanv, FTFont, ExtInterpolation, fppdf, fppdfcanvas;

const
  DemoWidth = 480;
  DemoHeight = 320;
  PageWidth = 595;
  PageHeight = 842;

type
  TDrawProc = procedure(aCanvas: TFPCustomCanvas);
  TPointArray = array of TPoint;

  TFeatureSet = record
    Title: String;
    FileName: String;
    Features: array of String;
    Draw: TDrawProc;
    PDFNote: String;
  end;

  { TColorCombiner }

  TColorCombiner = class
  public
    // Returns the product of the channels of aDest and aSource.
    function Multiply(const aDest, aSource: TFPColor): TFPColor;
  end;

const
  // The size of the captions; one image pixel is one point.
  CaptionSize = 12;

var
  FeatureSets: array of TFeatureSet;
  CaptionFont: TFreeTypeFont;
  TileImage, SmallImage, LargeImage: TFPMemoryImage;
  Combiner: TColorCombiner;
  // The transform of the page area; local transforms are applied before it.
  PageTransform: TFPCanvasMatrix;
  // The raster rendering of the set drawn on the PDF canvas.
  RasterCanvas: TFPCustomCanvas;


function TColorCombiner.Multiply(const aDest, aSource: TFPColor): TFPColor;

begin
  Result.Red := (LongWord(aDest.Red) * aSource.Red) div $FFFF;
  Result.Green := (LongWord(aDest.Green) * aSource.Green) div $FFFF;
  Result.Blue := (LongWord(aDest.Blue) * aSource.Blue) div $FFFF;
  Result.Alpha := alphaOpaque;
end;


// Returns the colour of the 8-bit channels.
function RGB(aRed, aGreen, aBlue: Byte; aAlpha: Byte = 255): TFPColor;

begin
  Result := FPColor(aRed * 257, aGreen * 257, aBlue * 257, aAlpha * 257);
end;


// Returns the fully saturated colour of hue aFraction (0..1).
function Rainbow(aFraction: Double): TFPColor;

var
  lHue, lPart: Double;
  lUp, lDown: Word;

begin
  lHue := Frac(aFraction) * 6;
  lPart := Frac(lHue);
  lUp := Round(lPart * $FFFF);
  lDown := $FFFF - lUp;
  case Trunc(lHue) of
    0: Result := FPColor($FFFF, lUp, 0);
    1: Result := FPColor(lDown, $FFFF, 0);
    2: Result := FPColor(0, $FFFF, lUp);
    3: Result := FPColor(0, lDown, $FFFF);
    4: Result := FPColor(lUp, 0, $FFFF);
  else
    Result := FPColor($FFFF, 0, lDown);
  end;
end;


// Writes aText at (aX, aY), the baseline, when the canvas can write text.
procedure Caption(aCanvas: TFPCustomCanvas; aX, aY: Integer; const aText: String);

begin
  if (aCanvas is TPDFCanvas) or Assigned(CaptionFont) then
    aCanvas.TextOut(aX, aY, aText);
end;


// Sets the pen, brush and drawing properties every feature set starts from.
procedure ResetCanvas(aCanvas: TFPCustomCanvas);

begin
  with aCanvas do
    begin
    Pen.Style := psSolid;
    Pen.Width := 1;
    Pen.Mode := pmCopy;
    Pen.FPColor := colBlack;
    Pen.EndCap := pecRound;
    Pen.JoinStyle := pjsRound;
    Brush.Style := bsClear;
    Brush.FPColor := colWhite;
    Brush.Image := nil;
    RectangleMode := rmInclude;
    EllipseMode := emCentered;
    DrawingMode := dmOpaque;
    Clipping := False;
    Interpolation := nil;
    end;
  aCanvas.HashWidth := 5;
  aCanvas.HatchOrigin := hoDefault;
  aCanvas.RelativeBrushImage := False;
  aCanvas.PolygonNonZeroWindingRule := False;
end;


// Applies the local transform built by the canvas calls before the page transform.
procedure EndLocalTransform(aCanvas: TFPCustomCanvas);

begin
  aCanvas.TransformMatrix := aCanvas.TransformMatrix.Multiply(PageTransform);
end;


{ Feature sets }

procedure DrawLines(aCanvas: TFPCustomCanvas);

var
  I: Integer;
  lAngle, lRadius: Double;
  lPoints: array of TPoint;

begin
  for I := 0 to 23 do
    begin
    lAngle := I * 2 * Pi / 24;
    aCanvas.Pen.FPColor := Rainbow(I / 24);
    aCanvas.Line(90, 145, 90 + Round(80 * Cos(lAngle)), 145 - Round(80 * Sin(lAngle)));
    end;
  Caption(aCanvas, 25, 300, 'Line in 24 directions');
  aCanvas.Pen.FPColor := RGB(0, 0, 160);
  aCanvas.MoveTo(190, 225);
  for I := 1 to 8 do
    if Odd(I) then
      aCanvas.LineTo(190 + I * 14, 65)
    else
      aCanvas.LineTo(190 + I * 14, 225);
  Caption(aCanvas, 195, 300, 'MoveTo and LineTo');
  lPoints := nil;
  SetLength(lPoints, 60);
  for I := 0 to High(lPoints) do
    begin
    lAngle := I * 0.35;
    lRadius := 8 + I * 1.1;
    lPoints[I] := Point(400 + Round(lRadius * Cos(lAngle)), 145 - Round(lRadius * Sin(lAngle)));
    end;
  aCanvas.Pen.FPColor := RGB(0, 128, 0);
  aCanvas.Polyline(lPoints);
  Caption(aCanvas, 375, 300, 'Polyline');
end;


procedure DrawPenStyles(aCanvas: TFPCustomCanvas);

const
  cStyles: array[0..5] of TFPPenStyle = (psSolid, psDash, psDot, psDashDot, psDashDotDot, psPattern);
  cWidths: array[0..4] of Integer = (1, 2, 3, 5, 8);

var
  I: Integer;
  lName: String;

begin
  aCanvas.Pen.Pattern := $FFF0F0CC;
  for I := 0 to High(cStyles) do
    begin
    aCanvas.Pen.Style := cStyles[I];
    aCanvas.Line(140, 25 + I * 26, 460, 25 + I * 26);
    WriteStr(lName, cStyles[I]);
    Caption(aCanvas, 15, 29 + I * 26, lName);
    end;
  aCanvas.Pen.Style := psSolid;
  for I := 0 to High(cWidths) do
    begin
    aCanvas.Pen.Width := cWidths[I];
    aCanvas.Pen.FPColor := RGB(160, 0, 0);
    aCanvas.Line(140, 195 + I * 24, 290, 195 + I * 24);
    Caption(aCanvas, 15, 199 + I * 24, Format('Width %d', [cWidths[I]]));
    aCanvas.Pen.FPColor := RGB(0, 0, 160);
    aCanvas.Line(320 + I * 30, 300, 350 + I * 30, 195);
    end;
  aCanvas.Pen.Width := 1;
  Caption(aCanvas, 320, 185, 'Width across the line');
end;


procedure DrawCapsAndJoins(aCanvas: TFPCustomCanvas);

const
  cCaps: array[0..2] of TFPPenEndCap = (pecRound, pecSquare, pecFlat);
  cJoins: array[0..2] of TFPPenJoinStyle = (pjsRound, pjsBevel, pjsMiter);

var
  I, lY: Integer;
  lName: String;
  lPoints: array of TPoint;

begin
  for I := 0 to 2 do
    begin
    lY := 50 + I * 95;
    aCanvas.Pen.Width := 18;
    aCanvas.Pen.EndCap := cCaps[I];
    aCanvas.Pen.FPColor := RGB(70, 130, 180);
    aCanvas.Line(50, lY, 190, lY);
    aCanvas.Pen.Width := 1;
    aCanvas.Pen.FPColor := colRed;
    aCanvas.Line(50, lY, 190, lY);
    WriteStr(lName, cCaps[I]);
    Caption(aCanvas, 50, lY + 35, 'EndCap ' + lName);
    lPoints := [Point(250, lY + 22), Point(300, lY - 22), Point(350, lY + 22), Point(400, lY - 22), Point(450, lY + 22)];
    aCanvas.Pen.Width := 14;
    aCanvas.Pen.EndCap := pecFlat;
    aCanvas.Pen.JoinStyle := cJoins[I];
    aCanvas.Pen.FPColor := RGB(46, 139, 87);
    aCanvas.Polyline(lPoints);
    aCanvas.Pen.Width := 1;
    aCanvas.Pen.FPColor := colRed;
    aCanvas.Polyline(lPoints);
    WriteStr(lName, cJoins[I]);
    Caption(aCanvas, 300, lY + 45, 'JoinStyle ' + lName);
    end;
end;


procedure DrawPenModes(aCanvas: TFPCustomCanvas);

const
  cStripes: array[0..2] of TFPColor = (
    (Red: $FFFF; Green: $FFFF; Blue: $FFFF; Alpha: $FFFF),
    (Red: 0; Green: $7878; Blue: $FFFF; Alpha: $FFFF),
    (Red: $FFFF; Green: $C8C8; Blue: 0; Alpha: $FFFF));

var
  lMode: TFPPenMode;
  lX, lY, S: Integer;
  lName: String;

begin
  for lMode := Low(TFPPenMode) to High(TFPPenMode) do
    begin
    lX := (Ord(lMode) mod 4) * 120;
    lY := (Ord(lMode) div 4) * 80;
    aCanvas.Pen.Mode := pmCopy;
    aCanvas.Brush.Style := bsSolid;
    for S := 0 to 2 do
      begin
      aCanvas.Brush.FPColor := cStripes[S];
      aCanvas.FillRect(lX + 5 + S * 37, lY + 4, lX + 4 + (S + 1) * 37, lY + 58);
      end;
    aCanvas.Pen.Width := 12;
    aCanvas.Pen.FPColor := RGB(220, 40, 40);
    aCanvas.Pen.Mode := lMode;
    aCanvas.Line(lX + 14, lY + 31, lX + 102, lY + 31);
    aCanvas.Pen.Mode := pmCopy;
    aCanvas.Pen.Width := 1;
    WriteStr(lName, lMode);
    Caption(aCanvas, lX + 8, lY + 74, lName);
    end;
end;


procedure DrawRectangles(aCanvas: TFPCustomCanvas);

var
  I: Integer;

begin
  aCanvas.Pen.FPColor := RGB(0, 0, 160);
  aCanvas.Rectangle(20, 25, 140, 105);
  Caption(aCanvas, 20, 125, 'Rectangle');
  aCanvas.Brush.Style := bsSolid;
  aCanvas.Brush.FPColor := RGB(255, 165, 0);
  aCanvas.FillRect(170, 25, 290, 105);
  Caption(aCanvas, 170, 125, 'FillRect');
  aCanvas.Brush.FPColor := RGB(144, 238, 144);
  aCanvas.Pen.FPColor := RGB(0, 100, 0);
  aCanvas.Rectangle(320, 25, 460, 105);
  Caption(aCanvas, 320, 125, 'Pen and brush');
  aCanvas.Brush.Style := bsClear;
  aCanvas.Pen.Width := 9;
  aCanvas.Pen.FPColor := RGB(70, 130, 180);
  aCanvas.Rectangle(20, 160, 140, 240);
  aCanvas.Pen.Width := 1;
  aCanvas.Pen.FPColor := colRed;
  aCanvas.Rectangle(20, 160, 140, 240);
  Caption(aCanvas, 20, 260, 'Width 9, inside');
  aCanvas.Pen.FPColor := colBlack;
  for I := 0 to 5 do
    aCanvas.Rectangle(180 + I * 20, 160, 200 + I * 20, 185);
  Caption(aCanvas, 180, 205, 'rmInclude: neighbours share an edge');
  aCanvas.RectangleMode := rmExclude;
  for I := 0 to 5 do
    aCanvas.Rectangle(180 + I * 20, 225, 200 + I * 20, 250);
  Caption(aCanvas, 180, 270, 'rmExclude: edges side by side');
  aCanvas.RectangleMode := rmInclude;
end;


procedure DrawEllipses(aCanvas: TFPCustomCanvas);

begin
  aCanvas.Pen.FPColor := RGB(0, 0, 160);
  aCanvas.Ellipse(20, 25, 150, 105);
  Caption(aCanvas, 20, 125, 'Ellipse');
  aCanvas.Brush.Style := bsSolid;
  aCanvas.Brush.FPColor := RGB(255, 215, 0);
  aCanvas.Pen.FPColor := RGB(139, 69, 19);
  aCanvas.Ellipse(170, 25, 300, 105);
  Caption(aCanvas, 170, 125, 'Pen and brush');
  aCanvas.Brush.FPColor := RGB(173, 216, 230);
  aCanvas.Pen.FPColor := RGB(0, 0, 139);
  aCanvas.EllipseC(395, 65, 60, 40);
  Caption(aCanvas, 335, 125, 'EllipseC');
  aCanvas.Brush.Style := bsClear;
  aCanvas.Pen.Width := 10;
  aCanvas.Pen.FPColor := RGB(70, 130, 180);
  aCanvas.Ellipse(30, 165, 150, 265);
  aCanvas.EllipseMode := emInside;
  aCanvas.Ellipse(190, 165, 310, 265);
  aCanvas.EllipseMode := emCentered;
  aCanvas.Pen.Width := 1;
  aCanvas.Pen.FPColor := colRed;
  aCanvas.Rectangle(30, 165, 150, 265);
  aCanvas.Rectangle(190, 165, 310, 265);
  Caption(aCanvas, 20, 295, 'EllipseMode emCentered');
  Caption(aCanvas, 185, 295, 'EllipseMode emInside');
  aCanvas.Pen.FPColor := colBlack;
  aCanvas.Pen.Style := psDot;
  aCanvas.Ellipse(340, 165, 460, 265);
  Caption(aCanvas, 345, 295, 'Dotted outline');
end;


// Draws the ray from (aX, aY) through (aThroughX, aThroughY) to a length of aLength.
procedure RayLine(aCanvas: TFPCustomCanvas; aX, aY, aThroughX, aThroughY, aLength: Integer);

var
  lAngle: Double;

begin
  lAngle := ArcTan2(aThroughY - aY, aThroughX - aX);
  aCanvas.Line(aX, aY, aX + Round(aLength * Cos(lAngle)), aY + Round(aLength * Sin(lAngle)));
end;


procedure DrawArcsAndPies(aCanvas: TFPCustomCanvas);

const
  cShares: array[0..3] of Integer = (35, 25, 25, 15);

var
  I, lStart, lLength: Integer;

begin
  aCanvas.Pen.Style := psDot;
  aCanvas.Pen.FPColor := RGB(170, 170, 170);
  aCanvas.Ellipse(20, 30, 160, 170);
  aCanvas.Ellipse(180, 30, 320, 170);
  aCanvas.Pen.Style := psSolid;
  aCanvas.Pen.Width := 4;
  aCanvas.Pen.FPColor := RGB(200, 0, 0);
  aCanvas.Arc(20, 30, 160, 170, 0, 90 * 16);
  aCanvas.Pen.FPColor := RGB(0, 150, 0);
  aCanvas.Arc(20, 30, 160, 170, 120 * 16, 120 * 16);
  aCanvas.Pen.FPColor := RGB(0, 0, 200);
  aCanvas.Arc(20, 30, 160, 170, 270 * 16, 60 * 16);
  Caption(aCanvas, 20, 200, 'Arc: start and length');
  aCanvas.Pen.FPColor := RGB(128, 0, 128);
  aCanvas.Arc(180, 30, 320, 170, 305, 50, 215, 85);
  aCanvas.Pen.Width := 1;
  aCanvas.Pen.FPColor := colBlack;
  aCanvas.Pen.Style := psDash;
  RayLine(aCanvas, 250, 100, 305, 50, 90);
  RayLine(aCanvas, 250, 100, 215, 85, 90);
  aCanvas.Pen.Style := psSolid;
  aCanvas.Pen.FPColor := colRed;
  aCanvas.Rectangle(303, 48, 307, 52);
  aCanvas.Rectangle(213, 83, 217, 87);
  aCanvas.Pen.FPColor := colBlack;
  Caption(aCanvas, 180, 200, 'Arc: from ray to ray');
  aCanvas.Brush.Style := bsSolid;
  lStart := 0;
  for I := 0 to High(cShares) do
    begin
    lLength := Round(cShares[I] * 3.6 * 16);
    aCanvas.Brush.FPColor := Rainbow(I / 4 + 0.05);
    aCanvas.RadialPie(340, 30, 470, 160, lStart, lLength);
    Inc(lStart, lLength);
    end;
  Caption(aCanvas, 340, 200, 'RadialPie');
  for I := 0 to 2 do
    begin
    aCanvas.Brush.FPColor := RGB(255, 200, 120);
    aCanvas.RadialPie(50 + I * 150, 235, 130 + I * 150, 315, 30 * 16, (45 + I * 110) * 16);
    Caption(aCanvas, 50 + I * 150, 228, Format('%d degrees', [45 + I * 110]));
    end;
end;


// Returns the five points of a pentagram around (aX, aY).
function StarPoints(aX, aY, aRadius: Integer): TPointArray;

var
  I: Integer;
  lAngle: Double;

begin
  Result := nil;
  SetLength(Result, 5);
  for I := 0 to 4 do
    begin
    lAngle := Pi / 2 + (I * 2 mod 5) * 2 * Pi / 5;
    Result[I] := Point(aX + Round(aRadius * Cos(lAngle)), aY - Round(aRadius * Sin(lAngle)));
    end;
end;


procedure DrawPolygons(aCanvas: TFPCustomCanvas);

var
  lPoints: TPointArray;
  I: Integer;

begin
  aCanvas.Brush.Style := bsSolid;
  aCanvas.Brush.FPColor := RGB(255, 140, 0);
  aCanvas.Pen.FPColor := RGB(139, 0, 0);
  aCanvas.Polygon(StarPoints(85, 85, 68));
  Caption(aCanvas, 30, 175, 'Even-odd rule');
  aCanvas.PolygonNonZeroWindingRule := True;
  aCanvas.Polygon(StarPoints(240, 85, 68));
  aCanvas.PolygonNonZeroWindingRule := False;
  Caption(aCanvas, 175, 175, 'Non-zero winding rule');
  aCanvas.Brush.FPColor := RGB(135, 206, 250);
  aCanvas.Pen.FPColor := RGB(0, 0, 139);
  aCanvas.Polygon([Point(335, 20), Point(465, 40), Point(440, 145), Point(385, 105), Point(345, 150)]);
  Caption(aCanvas, 345, 175, 'Polygon');
  lPoints := [Point(20, 280), Point(60, 200), Point(110, 295), Point(150, 240),
              Point(190, 195), Point(230, 295), Point(270, 235)];
  aCanvas.Brush.Style := bsClear;
  aCanvas.Pen.Width := 3;
  aCanvas.Pen.FPColor := RGB(0, 100, 0);
  aCanvas.PolyBezier(lPoints, False, True);
  aCanvas.Pen.Width := 1;
  aCanvas.Pen.FPColor := colRed;
  for I := 0 to High(lPoints) do
    aCanvas.Rectangle(lPoints[I].X - 2, lPoints[I].Y - 2, lPoints[I].X + 2, lPoints[I].Y + 2);
  Caption(aCanvas, 60, 315, 'PolyBezier, continuous');
  lPoints := [Point(320, 295), Point(335, 200), Point(465, 205), Point(455, 295)];
  aCanvas.Brush.Style := bsSolid;
  aCanvas.Brush.FPColor := RGB(221, 160, 221);
  aCanvas.Pen.FPColor := RGB(128, 0, 128);
  aCanvas.PolyBezier(lPoints, True, False);
  Caption(aCanvas, 340, 315, 'PolyBezier, filled');
end;


// Returns a brush pattern of diamonds.
function DiamondPattern: TBrushPattern;

var
  I, lOffset: Integer;

begin
  for I := 0 to High(Result) do
    begin
    lOffset := Abs(I mod 8 - 4);
    Result[I] := ($80808080 shr lOffset) or ($01010101 shl lOffset);
    end;
end;


procedure DrawBrushStyles(aCanvas: TFPCustomCanvas);

const
  cStyles: array[0..9] of TFPBrushStyle = (bsSolid, bsHorizontal, bsVertical, bsFDiagonal, bsBDiagonal,
                                          bsCross, bsDiagCross, bsPattern, bsImage, bsClear);

var
  I, lX, lY: Integer;
  lName: String;

begin
  aCanvas.HashWidth := 6;
  aCanvas.HatchOrigin := hoDefault;
  aCanvas.Brush.Pattern := DiamondPattern;
  aCanvas.Brush.Image := TileImage;
  aCanvas.Brush.FPColor := RGB(0, 90, 160);
  for I := 0 to High(cStyles) do
    begin
    lX := (I mod 5) * 96;
    lY := (I div 5) * 160;
    aCanvas.Brush.Style := cStyles[I];
    aCanvas.Rectangle(lX + 8, lY + 12, lX + 88, lY + 120);
    WriteStr(lName, cStyles[I]);
    Caption(aCanvas, lX + 10, lY + 142, lName);
    end;
  aCanvas.Brush.Image := nil;
end;


procedure DrawHatchOrigin(aCanvas: TFPCustomCanvas);

var
  lOrigin: THatchOrigin;
  lY: Integer;
  lName: String;

begin
  aCanvas.Brush.Style := bsDiagCross;
  aCanvas.Brush.FPColor := RGB(0, 90, 160);
  for lOrigin := Low(THatchOrigin) to High(THatchOrigin) do
    begin
    lY := 15 + Ord(lOrigin) * 102;
    aCanvas.HashWidth := 12;
    aCanvas.HatchOrigin := lOrigin;
    aCanvas.Rectangle(125, lY, 218, lY + 82);
    aCanvas.Rectangle(218, lY, 311, lY + 82);
    aCanvas.Ellipse(330, lY, 465, lY + 82);
    WriteStr(lName, lOrigin);
    Caption(aCanvas, 15, lY + 46, lName);
    end;
end;


procedure DrawFloodFill(aCanvas: TFPCustomCanvas);

begin
  aCanvas.Pen.Width := 2;
  aCanvas.Ellipse(20, 20, 140, 135);
  aCanvas.Polygon([Point(170, 20), Point(290, 20), Point(290, 135), Point(240, 135),
                   Point(240, 80), Point(220, 80), Point(220, 135), Point(170, 135)]);
  aCanvas.Polygon([Point(320, 135), Point(390, 20), Point(460, 135)]);
  aCanvas.Ellipse(20, 170, 140, 285);
  aCanvas.Rectangle(170, 170, 290, 285);
  aCanvas.Ellipse(320, 170, 460, 285);
  aCanvas.Ellipse(360, 200, 420, 255);
  aCanvas.Pen.Width := 1;
  aCanvas.Brush.Style := bsSolid;
  aCanvas.Brush.FPColor := RGB(255, 200, 0);
  aCanvas.FloodFill(80, 80);
  Caption(aCanvas, 25, 155, 'Solid');
  aCanvas.Brush.Style := bsPattern;
  aCanvas.Brush.Pattern := DiamondPattern;
  aCanvas.Brush.FPColor := RGB(200, 0, 0);
  aCanvas.FloodFill(180, 30);
  Caption(aCanvas, 175, 155, 'Pattern, around the notch');
  aCanvas.Brush.Style := bsCross;
  aCanvas.HashWidth := 8;
  aCanvas.HatchOrigin := hoDefault;
  aCanvas.Brush.FPColor := RGB(0, 120, 0);
  aCanvas.FloodFill(390, 100);
  Caption(aCanvas, 360, 155, 'Hatch');
  aCanvas.Brush.Style := bsImage;
  aCanvas.Brush.Image := TileImage;
  aCanvas.FloodFill(80, 230);
  Caption(aCanvas, 25, 305, 'Image');
  aCanvas.RelativeBrushImage := True;
  aCanvas.FloodFill(172, 172);
  aCanvas.RelativeBrushImage := False;
  aCanvas.Brush.Image := nil;
  Caption(aCanvas, 175, 305, 'Image, relative');
  aCanvas.Brush.Style := bsSolid;
  aCanvas.Brush.FPColor := RGB(135, 206, 235);
  aCanvas.FloodFill(330, 228);
  Caption(aCanvas, 335, 305, 'A ring: the hole stays');
end;


// Fills a checkerboard of gray and white squares.
procedure DrawChecker(aCanvas: TFPCustomCanvas; aLeft, aTop, aRight, aBottom: Integer);

var
  lX, lY: Integer;

begin
  aCanvas.Brush.Style := bsSolid;
  lY := aTop;
  while lY < aBottom do
    begin
    lX := aLeft;
    while lX < aRight do
      begin
      if Odd((lX - aLeft) div 10 + (lY - aTop) div 10) then
        aCanvas.Brush.FPColor := RGB(200, 200, 200)
      else
        aCanvas.Brush.FPColor := colWhite;
      aCanvas.FillRect(lX, lY, Min(lX + 9, aRight), Min(lY + 9, aBottom));
      Inc(lX, 10);
      end;
    Inc(lY, 10);
    end;
end;


procedure DrawDrawingModes(aCanvas: TFPCustomCanvas);

const
  cModes: array[0..2] of TFPDrawingMode = (dmOpaque, dmAlphaBlend, dmCustom);
  cNames: array[0..2] of String = ('dmOpaque', 'dmAlphaBlend', 'dmCustom: multiply');

var
  I, lX: Integer;

begin
  aCanvas.OnCombineColors := @Combiner.Multiply;
  for I := 0 to 2 do
    begin
    lX := 15 + I * 155;
    aCanvas.DrawingMode := dmOpaque;
    DrawChecker(aCanvas, lX, 20, lX + 140, 260);
    aCanvas.DrawingMode := cModes[I];
    aCanvas.Pen.Style := psClear;
    aCanvas.Brush.Style := bsSolid;
    if cModes[I] = dmCustom then
      aCanvas.Brush.FPColor := RGB(255, 120, 120)
    else
      aCanvas.Brush.FPColor := RGB(255, 0, 0, 140);
    aCanvas.Ellipse(lX + 10, 40, lX + 100, 130);
    if cModes[I] = dmCustom then
      aCanvas.Brush.FPColor := RGB(120, 255, 120)
    else
      aCanvas.Brush.FPColor := RGB(0, 200, 0, 140);
    aCanvas.Ellipse(lX + 40, 90, lX + 130, 180);
    if cModes[I] = dmCustom then
      aCanvas.Brush.FPColor := RGB(120, 120, 255)
    else
      aCanvas.Brush.FPColor := RGB(0, 0, 255, 140);
    aCanvas.Ellipse(lX + 10, 140, lX + 100, 230);
    aCanvas.DrawingMode := dmOpaque;
    aCanvas.Pen.Style := psSolid;
    Caption(aCanvas, lX, 285, cNames[I]);
    end;
  aCanvas.OnCombineColors := nil;
end;


procedure DrawGradients(aCanvas: TFPCustomCanvas);

begin
  aCanvas.GradientFill(Rect(20, 20, 220, 140), colBlack, colWhite, gdHorizontal);
  Caption(aCanvas, 20, 160, 'gdHorizontal: black to white');
  aCanvas.GradientFill(Rect(260, 20, 460, 140), RGB(255, 0, 0), RGB(0, 0, 255), gdVertical);
  Caption(aCanvas, 260, 160, 'gdVertical: red to blue');
  aCanvas.GradientFill(Rect(20, 180, 220, 290), RGB(255, 255, 0), RGB(0, 128, 0), gdVertical);
  Caption(aCanvas, 20, 310, 'gdVertical: yellow to green');
  aCanvas.GradientFill(Rect(260, 180, 460, 290), RGB(255, 165, 0), RGB(128, 0, 128), gdHorizontal);
  Caption(aCanvas, 260, 310, 'gdHorizontal: orange to purple');
end;


procedure DrawClipping(aCanvas: TFPCustomCanvas);

var
  I: Integer;
  lAngle: Double;
  lClip: TRect;

begin
  lClip := Rect(120, 50, 360, 260);
  // ClipRect is in device coordinates
  aCanvas.ClipRect := TRect.Create(aCanvas.TransformMatrix.Transform(lClip.TopLeft),
                                   aCanvas.TransformMatrix.Transform(lClip.BottomRight));
  aCanvas.Clipping := True;
  for I := 0 to 35 do
    begin
    lAngle := I * 2 * Pi / 36;
    aCanvas.Pen.FPColor := Rainbow(I / 36);
    aCanvas.Line(240, 155, 240 + Round(230 * Cos(lAngle)), 155 - Round(230 * Sin(lAngle)));
    end;
  aCanvas.Pen.FPColor := colBlack;
  aCanvas.Brush.Style := bsSolid;
  aCanvas.Brush.FPColor := RGB(255, 200, 0);
  aCanvas.Ellipse(60, 140, 200, 300);
  aCanvas.Brush.FPColor := RGB(100, 180, 255);
  aCanvas.Polygon([Point(290, 10), Point(470, 120), Point(320, 300)]);
  aCanvas.Clipping := False;
  aCanvas.Brush.Style := bsClear;
  aCanvas.Pen.Style := psDash;
  aCanvas.Rectangle(lClip);
  aCanvas.Pen.Style := psSolid;
  Caption(aCanvas, 120, 290, 'Lines, ellipse and polygon inside ClipRect');
end;


procedure DrawImages(aCanvas: TFPCustomCanvas);

var
  lInterpolations: array[0..3] of TFPCustomInterpolation;
  lNames: array[0..3] of String;
  I: Integer;
  lSource: TFPCustomCanvas;
  lDefault: String;

begin
  aCanvas.Draw(20, 40, SmallImage);
  lSource := aCanvas;
  lDefault := 'Mitchell';
  if aCanvas is TPDFCanvas then
    begin
    lSource := RasterCanvas;
    lDefault := 'viewer';
    end;
  Caption(aCanvas, 12, 110, 'Draw');
  lInterpolations[0] := nil;
  lInterpolations[1] := TFPBoxInterpolation.Create;
  lInterpolations[2] := TBilinearInterpolation.Create;
  lInterpolations[3] := TLanczosInterpolation.Create;
  lNames[0] := lDefault;
  lNames[1] := 'Box';
  lNames[2] := 'Bilinear';
  lNames[3] := 'Lanczos';
  try
    for I := 0 to 3 do
      begin
      aCanvas.Interpolation := lInterpolations[I];
      aCanvas.StretchDraw(70 + I * 102, 20, 96, 64, SmallImage);
      Caption(aCanvas, 70 + I * 102, 110, lNames[I]);
      end;
    aCanvas.Interpolation := lInterpolations[1];
    aCanvas.StretchDraw(20, 150, 120, 80, LargeImage);
    Caption(aCanvas, 20, 250, 'Shrunk, box');
    aCanvas.Interpolation := nil;
    aCanvas.StretchDraw(170, 150, 120, 80, LargeImage);
    Caption(aCanvas, 170, 250, 'Shrunk, ' + lDefault);
  finally
    aCanvas.Interpolation := nil;
    for I := 1 to 3 do
      lInterpolations[I].Free;
  end;
  aCanvas.CopyRect(330, 150, lSource, Rect(70, 20, 165, 83));
  Caption(aCanvas, 320, 250, 'CopyRect of the first');
  Caption(aCanvas, 320, 265, 'enlargement');
  Caption(aCanvas, 20, 300, 'StretchDraw of a 12x8 and a 240x160 image');
end;


// Draws the house outline of the transformation demonstration.
procedure DrawHouse(aCanvas: TFPCustomCanvas; const aColor: TFPColor);

begin
  aCanvas.Brush.Style := bsSolid;
  aCanvas.Brush.FPColor := aColor;
  aCanvas.Pen.FPColor := colBlack;
  aCanvas.Polygon([Point(-28, 22), Point(-28, -8), Point(0, -34), Point(28, -8), Point(28, 22)]);
end;


procedure DrawTransformations(aCanvas: TFPCustomCanvas);

var
  I: Integer;

begin
  aCanvas.ResetTransform;
  aCanvas.Translate(60, 80);
  EndLocalTransform(aCanvas);
  DrawHouse(aCanvas, RGB(255, 200, 120));
  aCanvas.TransformMatrix := PageTransform;
  Caption(aCanvas, 25, 140, 'Translate');
  aCanvas.ResetTransform;
  aCanvas.Rotate(Pi / 6);
  aCanvas.Translate(170, 80);
  EndLocalTransform(aCanvas);
  DrawHouse(aCanvas, RGB(144, 238, 144));
  aCanvas.TransformMatrix := PageTransform;
  Caption(aCanvas, 130, 140, 'Rotate, Translate');
  aCanvas.ResetTransform;
  aCanvas.Scale(1.5, 1.5);
  aCanvas.Translate(285, 85);
  EndLocalTransform(aCanvas);
  DrawHouse(aCanvas, RGB(173, 216, 230));
  aCanvas.TransformMatrix := PageTransform;
  Caption(aCanvas, 250, 140, 'Scale, Translate');
  aCanvas.ResetTransform;
  aCanvas.Translate(45, 0);
  aCanvas.Rotate(Pi / 6);
  aCanvas.Translate(385, 55);
  EndLocalTransform(aCanvas);
  DrawHouse(aCanvas, RGB(255, 182, 193));
  aCanvas.TransformMatrix := PageTransform;
  aCanvas.Brush.Style := bsClear;
  aCanvas.Pen.Style := psDot;
  aCanvas.Ellipse(385 - 45, 10, 385 + 45, 100);
  aCanvas.Pen.Style := psSolid;
  Caption(aCanvas, 355, 140, 'Translate, Rotate');
  for I := 0 to 11 do
    begin
    aCanvas.ResetTransform;
    aCanvas.Scale(0.45, 0.45);
    aCanvas.Translate(0, -62);
    aCanvas.Rotate(I * Pi / 6);
    aCanvas.Translate(330, 232);
    EndLocalTransform(aCanvas);
    DrawHouse(aCanvas, Rainbow(I / 12));
    end;
  aCanvas.TransformMatrix := PageTransform;
  Caption(aCanvas, 20, 250, 'Scale, Translate, Rotate');
  Caption(aCanvas, 20, 265, 'twelve times around a centre');
end;


procedure DrawText(aCanvas: TFPCustomCanvas);

const
  cSizes: array[0..3] of Integer = (10, 16, 24, 34);

var
  I, lY, lX: Integer;
  lText: String;
  lSize: TSize;
  lFont: TFPCustomFont;

begin
  if not ((aCanvas is TPDFCanvas) or Assigned(CaptionFont)) then
    exit;
  lFont := aCanvas.Font;
  lY := 20;
  for I := 0 to High(cSizes) do
    begin
    lFont.Size := cSizes[I];
    Inc(lY, cSizes[I] + 12);
    aCanvas.TextOut(20, lY, Format('%d points: brown fox', [cSizes[I]]));
    end;
  lFont.Size := 22;
  lX := 20;
  for I := 0 to 6 do
    begin
    lFont.FPColor := Rainbow(I / 7);
    lText := Copy('rainbow', I + 1, 1);
    aCanvas.TextOut(lX, 205, lText);
    Inc(lX, aCanvas.TextWidth(lText));
    end;
  lFont.FPColor := colBlack;
  lText := 'TextExtent';
  lSize := aCanvas.TextExtent(lText);
  aCanvas.TextOut(20, 265, lText);
  aCanvas.Pen.FPColor := colRed;
  aCanvas.Rectangle(20, 265 - lSize.cy, 20 + lSize.cx, 265);
  lFont.Size := 18;
  if lFont is TFreeTypeFont then
    TFreeTypeFont(lFont).Angle := Pi / 6
  else
    lFont.Orientation := 300;
  aCanvas.TextOut(260, 295, 'Rotated 30 degrees');
  if lFont is TFreeTypeFont then
    TFreeTypeFont(lFont).Angle := 0
  else
    lFont.Orientation := 0;
  lFont.Size := CaptionSize;
end;


// Marks the point (aX, aY) with a small red square.
procedure MarkPoint(aCanvas: TFPCustomCanvas; aX, aY: Integer);

begin
  aCanvas.Pen.FPColor := RGB(220, 0, 0);
  aCanvas.Brush.Style := bsClear;
  aCanvas.Rectangle(aX - 2, aY - 2, aX + 2, aY + 2);
  aCanvas.Pen.FPColor := colBlack;
end;


procedure DrawLCLShapes(aCanvas: TFPCustomCanvas);

var
  lRect: TRect;

begin
  aCanvas.Brush.Style := bsSolid;
  aCanvas.Brush.FPColor := RGB(70, 130, 180);
  aCanvas.Frame(15, 20, 60, 110);
  aCanvas.FrameRect(70, 20, 110, 110);
  Caption(aCanvas, 15, 135, 'Frame, FrameRect');

  aCanvas.Brush.FPColor := RGB(212, 208, 200);
  aCanvas.FillRect(135, 20, 225, 110);
  lRect := Rect(135, 20, 225, 110);
  aCanvas.Frame3D(lRect, colWhite, RGB(128, 128, 128), 3);
  InflateRect(lRect, -12, -12);
  aCanvas.Frame3D(lRect, RGB(128, 128, 128), colWhite, 2);
  Caption(aCanvas, 135, 135, 'Frame3D');

  aCanvas.Pen.Width := 2;
  aCanvas.Brush.FPColor := RGB(255, 200, 0);
  aCanvas.RoundRect(255, 20, 345, 110, 36, 36);
  Caption(aCanvas, 255, 135, 'RoundRect');

  aCanvas.Brush.FPColor := RGB(144, 238, 144);
  aCanvas.Chord(370, 20, 465, 110, 30 * 16, 210 * 16);
  Caption(aCanvas, 370, 135, 'Chord');

  aCanvas.Pen.Width := 1;
  aCanvas.Brush.FPColor := RGB(255, 160, 122);
  aCanvas.Pie(15, 170, 110, 265, 110, 180, 20, 250);
  MarkPoint(aCanvas, 110, 180);
  MarkPoint(aCanvas, 20, 250);
  Caption(aCanvas, 15, 290, 'Pie by two points');

  aCanvas.Pen.Width := 3;
  aCanvas.Pen.FPColor := RGB(0, 100, 0);
  aCanvas.MoveTo(135, 265);
  aCanvas.ArcTo(150, 175, 210, 235, 210, 205, 150, 205);
  aCanvas.AngleArc(190, 240, 20, 180, 180);
  aCanvas.LineTo(225, 265);
  aCanvas.Pen.Width := 1;
  aCanvas.Pen.FPColor := colBlack;
  Caption(aCanvas, 135, 290, 'ArcTo, AngleArc');

  aCanvas.Brush.FPColor := RGB(212, 208, 200);
  aCanvas.Rectangle(255, 195, 345, 235);
  Caption(aCanvas, 272, 222, 'Button');
  aCanvas.DrawFocusRect(Rect(260, 200, 340, 230));
  aCanvas.DrawFocusRect(Rect(255, 245, 345, 265));
  aCanvas.DrawFocusRect(Rect(255, 245, 345, 265));
  Caption(aCanvas, 255, 290, 'DrawFocusRect');

  aCanvas.Pen.Width := 2;
  aCanvas.Pen.FPColor := RGB(0, 0, 200);
  aCanvas.Brush.Style := bsClear;
  aCanvas.Ellipse(370, 170, 465, 265);
  aCanvas.Pen.FPColor := colBlack;
  aCanvas.Line(390, 200, 445, 235);
  aCanvas.Line(390, 235, 445, 200);
  aCanvas.Pen.Width := 1;
  aCanvas.Brush.Style := bsSolid;
  aCanvas.Brush.FPColor := RGB(255, 235, 59);
  aCanvas.FloodFill(417, 190, RGB(0, 0, 200), ffBorder);
  Caption(aCanvas, 370, 290, 'FloodFill ffBorder');
end;


// Returns the text style of TextRect with an alignment and a layout, on one clipped line.
function BoxStyle(aAlignment: TAlignment; aLayout: TFPTextLayout): TFPTextStyle;

begin
  Result := Default(TFPTextStyle);
  Result.Alignment := aAlignment;
  Result.Layout := aLayout;
  Result.SingleLine := True;
  Result.Clipping := True;
end;


// Draws aText in the box aRect with aStyle and a grey outline around the box.
procedure TextBox(aCanvas: TFPCustomCanvas; const aRect: TRect; const aText: String; const aStyle: TFPTextStyle);

begin
  aCanvas.Pen.FPColor := RGB(170, 170, 170);
  aCanvas.Brush.Style := bsClear;
  aCanvas.Frame(aRect);
  aCanvas.Pen.FPColor := colBlack;
  aCanvas.TextRect(aRect, aRect.Left + 2, aRect.Top + 2, aText, aStyle);
end;


procedure DrawTextRect(aCanvas: TFPCustomCanvas);

const
  cAlignments: array[0..2] of TAlignment = (taLeftJustify, taCenter, taRightJustify);
  cLayouts: array[0..2] of TFPTextLayout = (ftlTop, ftlCenter, ftlBottom);

var
  lColumn, lRow, lCount: Integer;
  lStyle: TFPTextStyle;
  lBox: TRect;

begin
  if not ((aCanvas is TPDFCanvas) or Assigned(CaptionFont)) then
    exit;
  aCanvas.RectangleMode := rmExclude;
  for lRow := 0 to 2 do
    for lColumn := 0 to 2 do
      begin
      lBox := Rect(15 + lColumn * 72, 15 + lRow * 52, 83 + lColumn * 72, 63 + lRow * 52);
      TextBox(aCanvas, lBox, 'Text', BoxStyle(cAlignments[lColumn], cLayouts[lRow]));
      end;
  Caption(aCanvas, 15, 185, 'Alignment across, layout down');

  lStyle := BoxStyle(taLeftJustify, ftlTop);
  lStyle.SingleLine := False;
  lStyle.Wordbreak := True;
  TextBox(aCanvas, Rect(245, 15, 465, 80), 'Wordbreak breaks a long line of text between its words to fit the box.', lStyle);
  lStyle := BoxStyle(taLeftJustify, ftlCenter);
  lStyle.EndEllipsis := True;
  TextBox(aCanvas, Rect(245, 90, 465, 112), 'EndEllipsis shortens a line that is too long for its box', lStyle);
  lStyle := BoxStyle(taCenter, ftlCenter);
  lStyle.ShowPrefix := True;
  TextBox(aCanvas, Rect(245, 122, 350, 144), 'Show&Prefix', lStyle);
  lStyle := BoxStyle(taCenter, ftlCenter);
  lStyle.Opaque := True;
  aCanvas.Brush.FPColor := RGB(255, 235, 59);
  aCanvas.Brush.Style := bsSolid;
  aCanvas.Pen.FPColor := RGB(170, 170, 170);
  aCanvas.Frame(Rect(360, 122, 465, 144));
  aCanvas.TextRect(Rect(360, 122, 465, 144), 362, 124, 'Opaque', lStyle);
  TextBox(aCanvas, Rect(245, 154, 330, 176), 'Clipping at the edge', BoxStyle(taLeftJustify, ftlCenter));
  Caption(aCanvas, 245, 200, 'TextStyle options');

  aCanvas.Pen.FPColor := RGB(220, 0, 0);
  aCanvas.Line(15, 250, 225, 250);
  aCanvas.Pen.FPColor := colBlack;
  aCanvas.TextOut(20, 250, 'toBaseline');
  aCanvas.TextOrigin := toTop;
  aCanvas.TextOut(125, 250, 'toTop');
  aCanvas.TextOrigin := toBaseline;
  Caption(aCanvas, 15, 290, 'TextOrigin at the red line');

  lCount := aCanvas.TextFitInfo('TextFitInfo counts what fits', 150);
  aCanvas.Pen.FPColor := RGB(170, 170, 170);
  aCanvas.Frame(Rect(245, 235, 395, 260));
  aCanvas.Pen.FPColor := colBlack;
  aCanvas.TextRect(Rect(245, 235, 395, 260), 247, 237,
    Copy('TextFitInfo counts what fits', 1, lCount), BoxStyle(taLeftJustify, ftlCenter));
  Caption(aCanvas, 245, 290, Format('%d characters fit in 150 pixels', [lCount]));
  aCanvas.RectangleMode := rmInclude;
end;


// Adds a feature set to FeatureSets.
procedure AddSet(const aTitle, aFileName: String; const aFeatures: array of String; aDraw: TDrawProc;
  const aPDFNote: String = '');

var
  I: Integer;

begin
  SetLength(FeatureSets, Length(FeatureSets) + 1);
  with FeatureSets[High(FeatureSets)] do
    begin
    Title := aTitle;
    FileName := aFileName;
    SetLength(Features, Length(aFeatures));
    for I := 0 to High(aFeatures) do
      Features[I] := aFeatures[I];
    Draw := aDraw;
    PDFNote := aPDFNote;
    end;
end;


// Fills FeatureSets with the demonstrated feature sets.
procedure CreateFeatureSets;

begin
  AddSet('Lines', 'lines.png',
    ['Line between two points, both end points drawn',
     'Lines in every direction',
     'MoveTo and LineTo from the pen position',
     'Polyline through a list of points',
     'Pen colour (Pen.FPColor)'], @DrawLines);
  AddSet('Pen styles and widths', 'penstyles.png',
    ['Pen.Style: psSolid, psDash, psDot, psDashDot, psDashDotDot',
     'psPattern with a custom Pen.Pattern',
     'Pen.Width from 1 to 8 pixels',
     'The width is measured across slanted lines'], @DrawPenStyles,
     'Dashes use flat ends.');
  AddSet('Line ends and corners', 'capsjoins.png',
    ['Pen.EndCap: pecRound, pecSquare, pecFlat (the red line shows the end points)',
     'Pen.JoinStyle: pjsRound, pjsBevel, pjsMiter on thick polylines'], @DrawCapsAndJoins);
  AddSet('Pen modes', 'penmodes.png',
    ['All 16 Pen.Mode raster operations (pmBlack to pmNotXor)',
     'A red thick line over white, blue and yellow stripes',
     'Each pixel of a shape is combined once'], @DrawPenModes);
  AddSet('Rectangles', 'rectangles.png',
    ['Rectangle outline with the pen',
     'FillRect with the brush',
     'Rectangle with pen and brush',
     'A thick outline stays inside the bounds',
     'RectangleMode: rmInclude and rmExclude'], @DrawRectangles);
  AddSet('Ellipses and circles', 'ellipses.png',
    ['Ellipse outline',
     'Ellipse with pen and brush',
     'EllipseC from a centre and two radii',
     'EllipseMode: emCentered and emInside for thick outlines',
     'Dotted outline'], @DrawEllipses);
  AddSet('Arcs and pies', 'arcs.png',
    ['Arc from a start angle over a length (1/16 degree, counter-clockwise)',
     'Arc from the ray through one point to the ray through another (red squares: the points)',
     'RadialPie with pen and brush, a pie chart',
     'Pies of 45, 155 and 265 degrees'], @DrawArcsAndPies);
  AddSet('Polygons and Bezier curves', 'polygons.png',
    ['Polygon with pen and brush',
     'Even-odd and non-zero winding rules (PolygonNonZeroWindingRule)',
     'PolyBezier, continuous: 1 + 3 points per curve',
     'PolyBezier, filled',
     'Red squares mark the Bezier points'], @DrawPolygons);
  AddSet('Brush styles', 'brushes.png',
    ['Brush.Style: bsSolid, bsClear',
     'Hatches: bsHorizontal, bsVertical, bsFDiagonal, bsBDiagonal, bsCross, bsDiagCross',
     'bsPattern with a custom Brush.Pattern',
     'bsImage with a Brush.Image tile',
     'HashWidth: the distance between hatch lines'], @DrawBrushStyles);
  AddSet('Hatch alignment', 'hatchorigin.png',
    ['HatchOrigin hoDefault: rectangles from their corner, ellipses from the canvas origin',
     'HatchOrigin hoShape: every shape from its own corner',
     'HatchOrigin hoCanvas: every shape from the canvas origin, neighbours join up'], @DrawHatchOrigin);
  AddSet('Flood fill', 'floodfill.png',
    ['FloodFill with a solid brush',
     'FloodFill with a pattern and a hatch',
     'FloodFill with an image, absolute and relative (RelativeBrushImage)',
     'The fill stops at the outline, around notches and holes'], @DrawFloodFill);
  AddSet('Drawing modes and transparency', 'drawingmodes.png',
    ['DrawingMode dmOpaque: the brush colour replaces the pixel',
     'DrawingMode dmAlphaBlend: translucent colours blend with the pixel',
     'DrawingMode dmCustom with OnCombineColors (a multiply)'], @DrawDrawingModes);
  AddSet('Gradients', 'gradients.png',
    ['GradientFill with gdHorizontal',
     'GradientFill with gdVertical'], @DrawGradients);
  AddSet('Clipping', 'clipping.png',
    ['ClipRect and Clipping',
     'Lines, filled ellipses and polygons are cut at the clip rectangle',
     'Dashed outline of the clip rectangle'], @DrawClipping);
  AddSet('Images', 'images.png',
    ['Draw an image at its size',
     'StretchDraw with the default, box, bilinear and Lanczos interpolations',
     'StretchDraw to shrink an image',
     'CopyRect from a canvas'], @DrawImages,
     'The viewer scales by default; CopyRect reads the raster image.');
  AddSet('Transformations', 'transformations.png',
    ['Translate, Rotate and Scale',
     'Each call applies after the transforms set before it',
     'ResetTransform and TransformMatrix'], @DrawTransformations);
  AddSet('Text', 'text.png',
    ['TextOut with a FreeType font (TFreeTypeFont) or a font embedded in the PDF',
     'Font.Size and Font.FPColor',
     'TextWidth and TextExtent',
     'TFreeTypeFont.Angle or Font.Orientation for rotated text'], @DrawText,
     'The text is the embedded DejaVu Sans.');
  AddSet('LCL canvas methods', 'lclshapes.png',
    ['Frame outlines with the pen, FrameRect with the brush',
     'Frame3D with a raised and a sunken ring',
     'RoundRect with elliptic corners, Chord',
     'Pie between the rays through two points (red squares)',
     'ArcTo and AngleArc continue the pen path',
     'DrawFocusRect; a second call removes it (lower rect)',
     'FloodFill with ffBorder fills across the black lines'], @DrawLCLShapes);
  AddSet('TextRect and text layout', 'textrect.png',
    ['TextRect with the three alignments and the three layouts',
     'Wordbreak, EndEllipsis, ShowPrefix, Opaque and Clipping',
     'TextOrigin toBaseline and toTop',
     'TextFitInfo'], @DrawTextRect,
     'The text is the embedded DejaVu Sans.');
end;


// Creates the images the image brush and the image demonstration use.
procedure CreateImages;

var
  lX, lY: Integer;
  lDistance: Double;

begin
  TileImage := TFPMemoryImage.Create(16, 16);
  for lY := 0 to 15 do
    for lX := 0 to 15 do
      if Sqr(lX - 7.5) + Sqr(lY - 7.5) < 36 then
        TileImage.Colors[lX, lY] := RGB(255, 200, 0)
      else
        TileImage.Colors[lX, lY] := RGB(200, 0, 100);
  SmallImage := TFPMemoryImage.Create(12, 8);
  for lY := 0 to 7 do
    for lX := 0 to 11 do
      if (lX + lY) mod 5 = 0 then
        SmallImage.Colors[lX, lY] := colWhite
      else
        SmallImage.Colors[lX, lY] := Rainbow(lX / 12);
  LargeImage := TFPMemoryImage.Create(240, 160);
  for lY := 0 to 159 do
    for lX := 0 to 239 do
      begin
      lDistance := Sqrt(Sqr(lX - 120) + Sqr(lY - 80));
      if Odd(Trunc(lDistance / 3)) then
        LargeImage.Colors[lX, lY] := colBlack
      else
        LargeImage.Colors[lX, lY] := Rainbow(lDistance / 150);
      end;
end;


// Returns the image of aSet drawn on a TFPImageCanvas.
function RenderSet(const aSet: TFeatureSet): TFPMemoryImage;

var
  lCanvas: TFPImageCanvas;

begin
  Result := TFPMemoryImage.Create(DemoWidth, DemoHeight);
  lCanvas := TFPImageCanvas.Create(Result);
  try
    lCanvas.Brush.Style := bsSolid;
    lCanvas.Brush.FPColor := colWhite;
    lCanvas.FillRect(0, 0, DemoWidth - 1, DemoHeight - 1);
    if Assigned(CaptionFont) then
      begin
      CaptionFont.Size := CaptionSize;
      CaptionFont.FPColor := colBlack;
      lCanvas.Font := CaptionFont;
      end;
    ResetCanvas(lCanvas);
    PageTransform := TFPCanvasMatrix.Identity;
    aSet.Draw(lCanvas);
  finally
    lCanvas.Free;
  end;
end;


// Writes the PDF document with one page per feature set, its text in the font file aFontFile when given.
procedure WritePDF(const aFileName, aFontFile: String; const aImages: array of TFPMemoryImage);

var
  lDoc: TPDFDocument;
  lSection: TPDFSection;
  lPage: TPDFPage;
  lCanvas: TPDFCanvas;
  I, J, lY, lLeft, lWidth, lHeight: Integer;

begin
  lCanvas := nil;
  lDoc := TPDFDocument.Create(nil);
  try
    lDoc.Options := [poPageOriginAtTop, poCompressText, poCompressFonts, poCompressImages, poSubsetFont];
    lDoc.Infos.Title := 'fcl-image canvas features';
    lDoc.Infos.Producer := 'canvasdemo';
    lDoc.Infos.CreationDate := Now;
    lDoc.StartDocument;
    lSection := lDoc.Sections.AddSection;
    // one canvas unit is one point: no rounding of scaled coordinates
    lLeft := (PageWidth - DemoWidth) div 2;
    lWidth := DemoWidth;
    lHeight := DemoHeight;
    for I := 0 to High(FeatureSets) do
      with FeatureSets[I] do
        begin
        lPage := lDoc.Pages.AddPage;
        lPage.PaperType := ptA4;
        lSection.AddPage(lPage);
        if lCanvas = nil then
          begin
          lCanvas := TPDFCanvas.Create(lDoc, lPage);
          lCanvas.Shadow := True;
          if aFontFile <> '' then
            lCanvas.Font.Name := aFontFile;
          end
        else
          lCanvas.Page := lPage;
        ResetCanvas(lCanvas);
        lCanvas.ResetTransform;
        lY := 50;
        lCanvas.TextOut(lLeft, lY, Format('%d. %s', [I + 1, Title]));
        Inc(lY, 20);
        for J := 0 to High(Features) do
          begin
          lCanvas.TextOut(lLeft + 12, lY, '- ' + Features[J]);
          Inc(lY, 14);
          end;
        Inc(lY, 6);
        lCanvas.TextOut(lLeft, lY, Trim('Drawn with the PDF canvas. ' + PDFNote));
        Inc(lY, 14);
        lCanvas.Line(lLeft, lY, lLeft + lWidth, lY);
        Inc(lY, 24);
        lCanvas.Pen.FPColor := RGB(190, 190, 190);
        lCanvas.Rectangle(lLeft - 1, lY - 1, lLeft + lWidth, lY + lHeight);
        ResetCanvas(lCanvas);
        PageTransform := TFPCanvasMatrix.CreateTranslation(lLeft, lY);
        lCanvas.TransformMatrix := PageTransform;
        lCanvas.Font.Size := CaptionSize;
        RasterCanvas := TFPImageCanvas.Create(aImages[I]);
        try
          Draw(lCanvas);
        finally
          FreeAndNil(RasterCanvas);
        end;
        lCanvas.Font.Size := 10;
        lCanvas.ResetTransform;
        end;
    lDoc.SaveToFile(aFileName);
  finally
    lCanvas.Free;
    lDoc.Free;
  end;
end;


// Returns the first existing file of aName in the directories tried, or ''.
function FindFontFile(const aName: String): String;

var
  lDirs: array[0..4] of String;
  I: Integer;

begin
  lDirs[0] := ExtractFilePath(ParamStr(0));
  lDirs[1] := IncludeTrailingPathDelimiter(GetCurrentDir);
  lDirs[2] := ExtractFilePath(ParamStr(0)) + '..' + PathDelim + 'examples' + PathDelim;
  lDirs[3] := IncludeTrailingPathDelimiter(GetCurrentDir) + 'examples' + PathDelim;
  lDirs[4] := IncludeTrailingPathDelimiter(GetCurrentDir) + 'fonts' + PathDelim;
  for I := 0 to High(lDirs) do
    if FileExists(lDirs[I] + aName) then
      Exit(ExpandFileName(lDirs[I] + aName));
  Result := '';
end;


// Loads the caption font; leaves CaptionFont nil when FreeType or the font is missing.
procedure LoadCaptionFont(const aFontFile: String);

var
  lImage: TFPMemoryImage;
  lCanvas: TFPImageCanvas;

begin
  CaptionFont := nil;
  if aFontFile = '' then
    begin
    WriteLn('Font file DejaVuLGCSans.ttf not found: the images have no captions.');
    exit;
    end;
  lImage := TFPMemoryImage.Create(10, 10);
  lCanvas := TFPImageCanvas.Create(lImage);
  try
    try
      InitEngine;
      CaptionFont := TFreeTypeFont.Create;
      CaptionFont.Name := aFontFile;
      CaptionFont.Resolution := 72;
      CaptionFont.Size := CaptionSize;
      lCanvas.Font := CaptionFont;
      lCanvas.TextOut(0, 9, 'A');
    except
      on E: Exception do
        begin
        WriteLn('FreeType is not available (', E.Message, '): the images have no captions.');
        FreeAndNil(CaptionFont);
        end;
    end;
  finally
    lCanvas.Free;
    lImage.Free;
  end;
end;


var
  lFileName, lFontFile: String;
  lImages: array of TFPMemoryImage;
  I: Integer;

begin
  lFileName := 'canvasdemo.pdf';
  if ParamCount >= 1 then
    begin
    lFileName := ParamStr(1);
    if (lFileName = '-h') or (lFileName = '--help') then
      begin
      Writeln('Usage: canvasdemo [-h|--help|filename [fontfile]]');
      Writeln('Writes the PDF to filename (default canvasdemo.pdf), with its text in DejaVuSans.ttf or the font file');
      Halt(0)
      end;
    end;
  if ParamCount >= 2 then
    lFontFile := ParamStr(2)
  else
    lFontFile := FindFontFile('DejaVuSans.ttf');
  LoadCaptionFont(lFontFile);
  Combiner := TColorCombiner.Create;
  CreateImages;
  CreateFeatureSets;
  lImages := nil;
  SetLength(lImages, Length(FeatureSets));
  try
    for I := 0 to High(FeatureSets) do
      lImages[I] := RenderSet(FeatureSets[I]);
    WritePDF(lFileName, lFontFile, lImages);
    WriteLn('Wrote ', lFileName, ' with ', Length(FeatureSets), ' pages');
  finally
    for I := 0 to High(lImages) do
      lImages[I].Free;
    TileImage.Free;
    SmallImage.Free;
    LargeImage.Free;
    Combiner.Free;
    CaptionFont.Free;
  end;
end.
