{
    Tests for drawing on a TFPImageCanvas: pixels, lines, shapes, flood fill,
    pen modes, brushes, clipping, copying, gradients and transformations.
    See the file COPYING.FPC, included in this distribution, for details.
}
unit tccanvas;

{$mode objfpc}{$H+}

interface

uses sysutils, classes, types, fpcunit, testregistry, fpimage, fpimgtests,
     fpcanvas, fppixlcanv, fpimgcanv, fpbarcode, fpimgbarcode;

type
  // A white canvas of 40x30 with a black pen, a red brush and RectangleMode rmExclude.
  TCanvasTestCase = class(TTestCase)
  protected
    FImage: TFPMemoryImage;
    FCanvas: TFPImageCanvas;
    procedure SetUp; override;
    procedure TearDown; override;
    // Replaces the canvas by one of that size filled with aColor.
    procedure NewCanvas(aWidth, aHeight: Integer; const aColor: TFPColor);
    // True if the pixel differs from the background colour aBackground.
    function Painted(aX, aY: Integer; const aBackground: TFPColor): Boolean;
    // True if the pixel is no longer white.
    function Painted(aX, aY: Integer): Boolean;
    // Number of pixels that are no longer white.
    function CountPainted: Integer;
    // The smallest rectangle, right and bottom included, around the painted pixels.
    function PaintedBounds: TRect;
    // Fails unless exactly the pixels of the rectangle (right and bottom excluded) are painted.
    procedure AssertPaintedRect(const aMessage: String; aLeft, aTop, aRight, aBottom: Integer);
  end;

  TTestCanvasPixels = class(TCanvasTestCase)
  private
    function Average(const aColor1, aColor2: TFPColor): TFPColor;
  published
    procedure TestColorsReadAndWriteTheImage;
    procedure TestPixelsOutsideTheCanvasAreIgnored;
    procedure TestDrawPixelOpaque;
    procedure TestDrawPixelAlphaBlend;
    procedure TestDrawPixelCustom;
    procedure TestEraseMakesEverythingTransparent;
    procedure TestClearFillsEverythingWithTheBrush;
    procedure TestClearWithAClearBrushDoesNothing;
  end;

  TTestCanvasLines = class(TCanvasTestCase)
  private
    // Fails unless every painted pixel lies within half a pixel of the line.
    procedure AssertOnLine(const aMessage: String; aX1, aY1, aX2, aY2: Integer);
  published
    procedure TestAHorizontalLineIncludesBothEnds;
    procedure TestAHorizontalLineDrawnBackwards;
    procedure TestAVerticalLineIncludesBothEnds;
    procedure TestADiagonalLine;
    procedure TestAShallowLine;
    procedure TestASteepLine;
    procedure TestALineOfOnePoint;
    procedure TestAClearPenDrawsNothing;
    procedure TestLineToStartsAtThePenPosition;
    procedure TestPolylineJoinsThePoints;
    procedure TestAThickHorizontalLine;
    procedure TestAThickVerticalLine;
    procedure TestAThickDiagonalLineIsAsWideAcross;
    procedure TestThickLineEndCaps;
    procedure TestThickPolylineJoins;
    procedure TestAThickXorPolylineCombinesEachPixelOnce;
    procedure TestADashedLine;
    procedure TestADottedLine;
    procedure TestACustomPatternLine;
    procedure TestPatternsAgreeOnHorizontalAndSlopedLines;
    procedure TestAPatternLineStartingLeftOfTheCanvas;
    procedure TestAClippedLineStaysInTheClipRect;
    procedure TestALineOutsideTheClipRectDrawsNothing;
    procedure TestPolyBezierOfStraightControlPoints;
    procedure TestPolyBezierReadsOnlyThePointsGiven;
  end;

  TTestCanvasPenModes = class(TCanvasTestCase)
  private
    // The colour of a pixel drawn with aPen on aDest in aMode, following the raster operations of GDI.
    function Expected(aMode: TFPPenMode; const aPen, aDest: TFPColor): TFPColor;
    // Draws the shape of aShape with the current pen.
    procedure DrawShape(aShape: Integer);
    // Fails unless every pen mode gives the expected colour on every pixel of the shape.
    procedure CheckModes(const aName: String; aShape: Integer);
  published
    procedure TestModesOnASolidLine;
    procedure TestModesOnASlopedLine;
    procedure TestModesOnAPatternLine;
    procedure TestModesOnARectangle;
    procedure TestModesOnAnEllipse;
    procedure TestModesOnAPolyline;
  end;

  TTestCanvasRectangles = class(TCanvasTestCase)
  published
    procedure TestFillRectExcludesRightAndBottom;
    procedure TestFillRectSortsItsCorners;
    procedure TestAnEmptyFillRectDrawsNothing;
    procedure TestRectangleOutlineExcludesRightAndBottom;
    procedure TestRectangleWithABrushFillsTheInside;
    procedure TestARectanglePartlyLeftOfTheCanvas;
    procedure TestAThickRectangleStaysInside;
    procedure TestADashedRectangle;
    procedure TestAClearBrushFillsNothing;
    procedure TestHatchedFills;
    procedure TestBrushFillsIgnoreThePenMode;
    procedure TestHatchOriginCanvasJoinsAdjacentRectangles;
    procedure TestHatchOriginShapeStartsAtTheCorner;
    procedure TestHatchOriginShapeOnAFloodFill;
    procedure TestABrushPatternStartsWithTheMostSignificantBit;
    procedure TestABrushImageIsTiledFromTheCanvasOrigin;
    procedure TestARelativeBrushImageIsTiledFromTheRectangle;
    procedure TestTheClipRectExcludesRightAndBottom;
  end;

  TTestCanvasRectangleMode = class(TCanvasTestCase)
  private
    // Switches the canvas to rmInclude.
    procedure UseInclude;
  published
    procedure TestTheDefaultIsInclude;
    procedure TestFillRectIncludesRightAndBottom;
    procedure TestRectangleOutlineIncludesRightAndBottom;
    procedure TestAZeroWidthRectIsOneColumnWhenIncluded;
    procedure TestEllipseBoundsWhenIncluded;
    procedure TestEllipseCIsCentredWhenIncluded;
    procedure TestTheClipRectIncludesRightAndBottom;
    procedure TestTheClipRectComesBackAsSet;
    procedure TestTheClipStaysWhenTheModeChanges;
    procedure TestCopyRectIncludesRightAndBottom;
    procedure TestAGradientCoversWhatFillRectCovers;
    procedure TestScaleWhenIncluded;
    procedure TestClearFillsTheCanvasInBothModes;
    procedure TestBarcodesDrawTheSameInBothModes;
  end;

  TTestCanvasEllipses = class(TCanvasTestCase)
  published
    procedure TestAnEllipseFitsItsBounds;
    procedure TestAnEllipseIsSymmetric;
    procedure TestAFilledEllipse;
    procedure TestEllipseCIsCentred;
    procedure TestAThickEllipseStaysInside;
    procedure TestAThickEllipseIsCentredByDefault;
    procedure TestAnArcOfAQuarter;
    procedure TestAnArcByPointsMatchesTheAngles;
    procedure TestARadialPie;
    procedure TestADottedEllipse;
  end;

  TTestCanvasPolygons = class(TCanvasTestCase)
  private
    procedure FillSquareAsPolygon;
  published
    procedure TestAFilledTriangle;
    procedure TestAPolygonOutlineIsClosed;
    procedure TestEvenOddAndNonZeroWinding;
    procedure TestAPolygonSquareCoversTheSameAsFillRect;
    procedure TestPatternsAgreeOnPolygonsAndRectangles;
  end;

  TTestCanvasFlood = class(TCanvasTestCase)
  private
    procedure FloodWithTheSameColor;
    procedure FloodWithAPattern;
    // Draws the outline of a box from (5,5) to (15,12), both included.
    procedure DrawBox;
  published
    procedure TestFloodFillFillsTheInsideOnly;
    procedure TestFloodFillDoesNotLeakThroughACorner;
    procedure TestFloodFillOutsideTheCanvasDoesNothing;
    procedure TestFloodFillWithTheColorAlreadyThere;
    procedure TestFloodFillWithAPatternMatchesAFilledRectangle;
    procedure TestFloodFillWithAPatternDoesNotLeak;
    procedure TestFloodFillWithAHatchStaysInside;
    procedure TestFloodFillWithAnImage;
    procedure TestARelativeImageFloodFillLeftOfTheStart;
    procedure TestAWideHatchFloodFill;
  end;

  TTestCanvasCopy = class(TCanvasTestCase)
  private
    FSource: TFPMemoryImage;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDrawCopiesTheImage;
    procedure TestDrawPartlyOffTheCanvas;
    procedure TestDrawAlphaBlends;
    procedure TestDrawRespectsTheClipRect;
    procedure TestCopyRectExcludesRightAndBottom;
    procedure TestAVerticalGradient;
    procedure TestAHorizontalGradient;
    procedure TestAGradientStaysInItsRect;
    procedure TestAnEmptyGradientLeavesThePenAlone;
    procedure TestTranslate;
    procedure TestScale;
    procedure TestResetTransform;
  end;

  // An image canvas that logs the image hooks it is called with.
  TRecordingCanvas = class(TFPImageCanvas)
  protected
    procedure DoDraw(x, y: Integer; const aImage: TFPCustomImage); override;
    procedure DoCopyRect(x, y: Integer; aCanvas: TFPCustomCanvas; const aSourceRect: TRect); override;
    procedure DoStretchDraw(x, y, w, h: Integer; aSource: TFPCustomImage); override;
  public
    Log: String;
  end;

  TTestCanvasLCLMethods = class(TCanvasTestCase)
  private
    // Returns a copy of the image drawn so far and starts a new white canvas.
    function TakeImage: TFPMemoryImage;
    procedure DrawStar(aWinding: Boolean);
  published
    procedure TestFrameOutlinesWithoutFilling;
    procedure TestFrameRectDrawsABrushBorder;
    procedure TestFrame3DColoursItsSidesAndShrinksTheRect;
    procedure TestRoundRectRoundsTheCorners;
    procedure TestRoundRectWithoutRadiiIsARectangle;
    procedure TestChordCutsTheEllipseAlongItsChord;
    procedure TestPieTakesTheRaysThroughTwoPoints;
    procedure TestArcToDrawsTheLineAndMovesThePen;
    procedure TestAngleArcFollowsTheLCLFormula;
    procedure TestDrawFocusRectTwiceRestoresThePixels;
    procedure TestPolygonTakesTheWindingRuleForOneCall;
    procedure TestPolygonDrawsARangeOfPoints;
    procedure TestPolygonAndPolylineFromAPointer;
    procedure TestCopyRectScalesTheSource;
    procedure TestStretchDrawIntoARect;
    procedure TestFloodFillSurfaceNeedsTheSeedColour;
    procedure TestFloodFillBorderFillsUpToTheBorderColour;
    procedure TestTheTextStyleStartsAsTheLCLDefault;
  end;

  TTestCanvasHooks = class(TTestCase)
  private
    FImage, FSource: TFPMemoryImage;
    FCanvas: TRecordingCanvas;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDrawCallsDoDrawInDevicePixels;
    procedure TestCopyRectCallsDoCopyRectWithIncludedBounds;
    procedure TestStretchDrawCallsDoStretchDrawInDevicePixels;
    procedure TestThePortablePropertiesAreOnTheBaseClass;
  end;

implementation

const
  cBackground: TFPColor = (Red: $F0F0; Green: $CCCC; Blue: $AAAA; Alpha: alphaOpaque);
  cPenColor: TFPColor = (Red: $FF00; Green: $0FF0; Blue: $3C3C; Alpha: alphaOpaque);

{ TCanvasTestCase }

procedure TCanvasTestCase.SetUp;

begin
  inherited SetUp;
  NewCanvas(40, 30, colWhite);
end;


procedure TCanvasTestCase.TearDown;

begin
  FreeAndNil(FCanvas);
  FreeAndNil(FImage);
  inherited TearDown;
end;


procedure TCanvasTestCase.NewCanvas(aWidth, aHeight: Integer; const aColor: TFPColor);

begin
  FreeAndNil(FCanvas);
  FreeAndNil(FImage);
  FImage := CreateSolidImage(aWidth, aHeight, aColor);
  FCanvas := TFPImageCanvas.Create(FImage);
  FCanvas.RectangleMode := rmExclude;
  FCanvas.Pen.FPColor := colBlack;
  FCanvas.Brush.FPColor := colRed;
end;


function TCanvasTestCase.Painted(aX, aY: Integer; const aBackground: TFPColor): Boolean;

begin
  Result := not (FImage[aX, aY] = aBackground);
end;


function TCanvasTestCase.Painted(aX, aY: Integer): Boolean;

begin
  Result := Painted(aX, aY, colWhite);
end;


function TCanvasTestCase.CountPainted: Integer;

var
  lX, lY: Integer;

begin
  Result := 0;
  for lY := 0 to FImage.Height - 1 do
    for lX := 0 to FImage.Width - 1 do
      if Painted(lX, lY) then
        Inc(Result);
end;


function TCanvasTestCase.PaintedBounds: TRect;

var
  lX, lY: Integer;

begin
  Result := Rect(MaxInt, MaxInt, -1, -1);
  for lY := 0 to FImage.Height - 1 do
    for lX := 0 to FImage.Width - 1 do
      if Painted(lX, lY) then
        begin
        if lX < Result.Left then
          Result.Left := lX;
        if lX > Result.Right then
          Result.Right := lX;
        if lY < Result.Top then
          Result.Top := lY;
        if lY > Result.Bottom then
          Result.Bottom := lY;
        end;
end;


procedure TCanvasTestCase.AssertPaintedRect(const aMessage: String; aLeft, aTop, aRight, aBottom: Integer);

var
  lX, lY: Integer;
  lInside: Boolean;

begin
  for lY := 0 to FImage.Height - 1 do
    for lX := 0 to FImage.Width - 1 do
      begin
      lInside := (lX >= aLeft) and (lX < aRight) and (lY >= aTop) and (lY < aBottom);
      if lInside <> Painted(lX, lY) then
        if lInside then
          Fail(Format('%s: pixel (%d,%d) inside the rectangle is not painted', [aMessage, lX, lY]))
        else
          Fail(Format('%s: pixel (%d,%d) outside the rectangle is painted', [aMessage, lX, lY]));
      end;
end;


{ TTestCanvasPixels }

function TTestCanvasPixels.Average(const aColor1, aColor2: TFPColor): TFPColor;

begin
  Result := FPColor((aColor1.Red + aColor2.Red) div 2, (aColor1.Green + aColor2.Green) div 2,
    (aColor1.Blue + aColor2.Blue) div 2, (aColor1.Alpha + aColor2.Alpha) div 2);
end;


procedure TTestCanvasPixels.TestColorsReadAndWriteTheImage;

begin
  FCanvas.Colors[3, 4] := colBlue;
  AssertColorsEqual('Writing through the canvas writes the image', colBlue, FImage[3, 4]);
  FImage[5, 6] := colGreen;
  AssertColorsEqual('Reading through the canvas reads the image', colGreen, FCanvas.Colors[5, 6]);
  AssertEquals('The canvas has the width of the image', 40, FCanvas.Width);
  AssertEquals('The canvas has the height of the image', 30, FCanvas.Height);
end;


procedure TTestCanvasPixels.TestPixelsOutsideTheCanvasAreIgnored;

begin
  FCanvas.Colors[-1, 0] := colBlue;
  FCanvas.Colors[40, 0] := colBlue;
  FCanvas.Colors[0, 30] := colBlue;
  AssertEquals('Writing outside the canvas changes nothing', 0, CountPainted);
  AssertColorsEqual('Reading outside the canvas gives transparent', colTransparent, FCanvas.Colors[-1, -1]);
end;


procedure TTestCanvasPixels.TestDrawPixelOpaque;

begin
  FCanvas.DrawPixel(2, 2, FPColor($FFFF, 0, 0, $8000));
  AssertColorsEqual('An opaque drawing mode replaces the pixel, alpha too', FPColor($FFFF, 0, 0, $8000), FImage[2, 2]);
end;


procedure TTestCanvasPixels.TestDrawPixelAlphaBlend;

begin
  FImage[2, 2] := colBlack;
  FCanvas.DrawingMode := dmAlphaBlend;
  FCanvas.DrawPixel(2, 2, FPColor($FFFF, $FFFF, $FFFF, $8000));
  AssertColorsEqual('Alpha blending mixes half white over black into gray', FPColor($8000, $8000, $8000), FImage[2, 2], 2);
end;


procedure TTestCanvasPixels.TestDrawPixelCustom;

begin
  FImage[2, 2] := colBlack;
  FCanvas.DrawingMode := dmCustom;
  FCanvas.OnCombineColors := @Average;
  FCanvas.DrawPixel(2, 2, colWhite);
  AssertColorsEqual('A custom drawing mode uses the combine function', FPColor($7FFF, $7FFF, $7FFF), FImage[2, 2]);
end;


procedure TTestCanvasPixels.TestEraseMakesEverythingTransparent;

var
  lX, lY: Integer;

begin
  FCanvas.Erase;
  for lY := 0 to 29 do
    for lX := 0 to 39 do
      AssertColorsEqual(Format('Pixel (%d,%d) after Erase', [lX, lY]), colTransparent, FImage[lX, lY]);
end;


procedure TTestCanvasPixels.TestClearFillsEverythingWithTheBrush;

begin
  FCanvas.Clear;
  AssertPaintedRect('Clear fills the whole canvas', 0, 0, 40, 30);
  AssertColorsEqual('with the brush colour', colRed, FImage[39, 29]);
end;


procedure TTestCanvasPixels.TestClearWithAClearBrushDoesNothing;

begin
  FCanvas.Brush.Style := bsClear;
  FCanvas.Clear;
  AssertEquals('A clear brush clears nothing', 0, CountPainted);
end;


{ TTestCanvasLines }

procedure TTestCanvasLines.AssertOnLine(const aMessage: String; aX1, aY1, aX2, aY2: Integer);

var
  lX, lY: Integer;
  lDistance, lLength: Double;

begin
  lLength := Sqrt(Sqr(aX2 - aX1) + Sqr(aY2 - aY1));
  for lY := 0 to FImage.Height - 1 do
    for lX := 0 to FImage.Width - 1 do
      if Painted(lX, lY) then
        begin
        lDistance := Abs((aY2 - aY1) * lX - (aX2 - aX1) * lY + aX2 * aY1 - aY2 * aX1) / lLength;
        AssertTrue(Format('%s: pixel (%d,%d) is within half a pixel of the line', [aMessage, lX, lY]),
          lDistance <= 0.5 * Sqrt(2) + 1e-9);
        end;
end;


procedure TTestCanvasLines.TestAHorizontalLineIncludesBothEnds;

begin
  FCanvas.Line(3, 5, 9, 5);
  AssertPaintedRect('A horizontal line covers both end points', 3, 5, 10, 6);
end;


procedure TTestCanvasLines.TestAHorizontalLineDrawnBackwards;

begin
  FCanvas.Line(9, 5, 3, 5);
  AssertPaintedRect('A horizontal line drawn right to left covers the same pixels', 3, 5, 10, 6);
end;


procedure TTestCanvasLines.TestAVerticalLineIncludesBothEnds;

begin
  FCanvas.Line(4, 12, 4, 2);
  AssertPaintedRect('A vertical line covers both end points', 4, 2, 5, 13);
end;


procedure TTestCanvasLines.TestADiagonalLine;

var
  I: Integer;

begin
  FCanvas.Line(2, 3, 8, 9);
  for I := 0 to 6 do
    AssertTrue(Format('Diagonal pixel %d', [I]), Painted(2 + I, 3 + I));
  AssertEquals('A diagonal of 7 steps has 7 pixels', 7, CountPainted);
end;


procedure TTestCanvasLines.TestAShallowLine;

begin
  FCanvas.Line(1, 2, 21, 9);
  AssertTrue('The start point is painted', Painted(1, 2));
  AssertTrue('The end point is painted', Painted(21, 9));
  AssertEquals('A shallow line has one pixel per column', 21, CountPainted);
  AssertOnLine('Shallow line', 1, 2, 21, 9);
end;


procedure TTestCanvasLines.TestASteepLine;

begin
  FCanvas.Line(20, 1, 13, 25);
  AssertTrue('The start point is painted', Painted(20, 1));
  AssertTrue('The end point is painted', Painted(13, 25));
  AssertEquals('A steep line has one pixel per row', 25, CountPainted);
  AssertOnLine('Steep line', 20, 1, 13, 25);
end;


procedure TTestCanvasLines.TestALineOfOnePoint;

begin
  FCanvas.Line(7, 7, 7, 7);
  AssertPaintedRect('A line from a point to itself paints that point', 7, 7, 8, 8);
end;


procedure TTestCanvasLines.TestAClearPenDrawsNothing;

begin
  FCanvas.Pen.Style := psClear;
  FCanvas.Line(1, 1, 20, 20);
  AssertEquals('A clear pen paints nothing', 0, CountPainted);
end;


procedure TTestCanvasLines.TestLineToStartsAtThePenPosition;

begin
  FCanvas.MoveTo(2, 2);
  FCanvas.LineTo(6, 2);
  AssertPaintedRect('LineTo draws from the pen position', 2, 2, 7, 3);
  AssertEquals('The pen position moves to the end, x', 6, FCanvas.PenPos.X);
  AssertEquals('The pen position moves to the end, y', 2, FCanvas.PenPos.Y);
end;


procedure TTestCanvasLines.TestPolylineJoinsThePoints;

begin
  FCanvas.Polyline([Point(2, 2), Point(8, 2), Point(8, 6)]);
  AssertTrue('The first segment', Painted(5, 2));
  AssertTrue('The second segment', Painted(8, 4));
  AssertTrue('The last point', Painted(8, 6));
  AssertFalse('No closing segment', Painted(5, 4));
  AssertEquals('The pen position is the last point', 6, FCanvas.PenPos.Y);
end;


procedure TTestCanvasLines.TestAThickHorizontalLine;

var
  lBounds: TRect;

begin
  FCanvas.Pen.Width := 3;
  FCanvas.Line(5, 10, 15, 10);
  lBounds := PaintedBounds;
  AssertEquals('A line 3 wide covers 3 rows, top', 9, lBounds.Top);
  AssertEquals('A line 3 wide covers 3 rows, bottom', 11, lBounds.Bottom);
  AssertTrue('The rows are complete', Painted(5, 9) and Painted(15, 11));
end;


procedure TTestCanvasLines.TestAThickVerticalLine;

var
  lBounds: TRect;

begin
  FCanvas.Pen.Width := 3;
  FCanvas.Line(10, 5, 10, 15);
  lBounds := PaintedBounds;
  AssertEquals('A line 3 wide covers 3 columns, left', 9, lBounds.Left);
  AssertEquals('A line 3 wide covers 3 columns, right', 11, lBounds.Right);
end;


procedure TTestCanvasLines.TestAThickDiagonalLineIsAsWideAcross;

var
  lX, lCount: Integer;

begin
  FCanvas.Pen.Width := 5;
  FCanvas.Pen.EndCap := pecFlat;
  FCanvas.Line(5, 5, 25, 25);
  lCount := 0;
  for lX := 0 to 39 do
    if Painted(lX, 15) then
      Inc(lCount);
  AssertEquals('A 45 degree line 5 wide across covers 5 x 1.41 = 7 pixels of a row', 7, lCount);
end;


procedure TTestCanvasLines.TestThickLineEndCaps;

var
  lBounds: TRect;

begin
  FCanvas.Pen.Width := 5;
  FCanvas.Pen.EndCap := pecFlat;
  FCanvas.Line(10, 10, 20, 10);
  lBounds := PaintedBounds;
  AssertEquals('A flat cap ends at the first point', 10, lBounds.Left);
  AssertEquals('A flat cap ends at the last point', 20, lBounds.Right);
  AssertEquals('The line is 5 rows high', 8, lBounds.Top);
  NewCanvas(40, 30, colWhite);
  FCanvas.Pen.Width := 5;
  FCanvas.Pen.EndCap := pecSquare;
  FCanvas.Line(10, 10, 20, 10);
  lBounds := PaintedBounds;
  AssertEquals('A square cap extends half the width before the first point', 8, lBounds.Left);
  AssertEquals('A square cap extends half the width after the last point', 22, lBounds.Right);
  AssertTrue('A square cap fills its corner', Painted(8, 8));
  NewCanvas(40, 30, colWhite);
  FCanvas.Pen.Width := 5;
  AssertTrue('Round caps are the default', FCanvas.Pen.EndCap = pecRound);
  FCanvas.Line(10, 10, 20, 10);
  lBounds := PaintedBounds;
  AssertEquals('A round cap reaches half the width before the first point', 8, lBounds.Left);
  AssertFalse('A round cap leaves its corner empty', Painted(8, 8));
end;


procedure TTestCanvasLines.TestThickPolylineJoins;

const
  cPoints: array[0..2] of TPoint = ((X: 5; Y: 20), (X: 20; Y: 20), (X: 20; Y: 5));

begin
  FCanvas.Pen.Width := 5;
  FCanvas.Pen.JoinStyle := pjsMiter;
  FCanvas.Polyline(cPoints);
  AssertTrue('A mitre join fills the outer corner', Painted(22, 22));
  NewCanvas(40, 30, colWhite);
  FCanvas.Pen.Width := 5;
  FCanvas.Pen.JoinStyle := pjsBevel;
  FCanvas.Polyline(cPoints);
  AssertFalse('A bevel join cuts the outer corner', Painted(22, 22));
  AssertTrue('A bevel join fills the corner inside the cut', Painted(21, 21));
  AssertTrue('A bevel join closes the gap between the segments', Painted(22, 20) and Painted(20, 22));
  NewCanvas(40, 30, colWhite);
  FCanvas.Pen.Width := 5;
  AssertTrue('Round joins are the default', FCanvas.Pen.JoinStyle = pjsRound);
  FCanvas.Polyline(cPoints);
  AssertFalse('A round join leaves the outer corner', Painted(22, 22));
  AssertTrue('A round join fills the rounded corner', Painted(22, 21) and Painted(21, 22));
end;


procedure TTestCanvasLines.TestAThickXorPolylineCombinesEachPixelOnce;

const
  cPoints: array[0..3] of TPoint = ((X: 3; Y: 3), (X: 30; Y: 10), (X: 12; Y: 25), (X: 35; Y: 25));

var
  lX, lY: Integer;
  lCopy: TFPMemoryImage;
  lWant: TFPColor;

begin
  FCanvas.Pen.Width := 6;
  FCanvas.Polyline(cPoints);
  lCopy := TFPMemoryImage.Create(0, 0);
  try
    lCopy.Assign(FImage);
    NewCanvas(40, 30, colWhite);
    FCanvas.Pen.Width := 6;
    FCanvas.Pen.FPColor := RGB8(255, 0, 0);
    FCanvas.Pen.Mode := pmXor;
    FCanvas.Polyline(cPoints);
    lWant := RGB8(0, 255, 255);
    for lY := 0 to 29 do
      for lX := 0 to 39 do
        if Painted(lX, lY, colWhite) or not (lCopy[lX, lY] = colWhite) then
          AssertColorsEqual(Format('Pixel (%d,%d) of the stroke is XORed exactly once', [lX, lY]), lWant, FImage[lX, lY]);
  finally
    lCopy.Free;
  end;
end;


procedure TTestCanvasLines.TestADashedLine;

var
  lX: Integer;

begin
  FCanvas.Pen.Style := psDash;
  FCanvas.Line(0, 3, 31, 3);
  for lX := 0 to 31 do
    AssertEquals(Format('Dash pattern $EEEEEEEE at x=%d', [lX]), (lX mod 4) <> 3, Painted(lX, 3));
end;


procedure TTestCanvasLines.TestADottedLine;

var
  lX: Integer;

begin
  FCanvas.Pen.Style := psDot;
  FCanvas.Line(0, 3, 31, 3);
  for lX := 0 to 31 do
    AssertEquals(Format('Dot pattern $AAAAAAAA at x=%d', [lX]), not Odd(lX), Painted(lX, 3));
end;


procedure TTestCanvasLines.TestACustomPatternLine;

var
  lX: Integer;

begin
  FCanvas.Pen.Style := psPattern;
  FCanvas.Pen.Pattern := $F0000001;
  FCanvas.Line(0, 3, 31, 3);
  for lX := 0 to 31 do
    AssertEquals(Format('Pattern $F0000001, most significant bit first, at x=%d', [lX]),
      (lX < 4) or (lX = 31), Painted(lX, 3));
end;


procedure TTestCanvasLines.TestPatternsAgreeOnHorizontalAndSlopedLines;

var
  I: Integer;

begin
  FCanvas.Pen.Style := psPattern;
  FCanvas.Pen.Pattern := $C8000000;
  FCanvas.Line(0, 0, 7, 0);
  FCanvas.Line(0, 2, 7, 9);
  for I := 0 to 7 do
    AssertEquals(Format('Step %d of a sloped line follows the pattern like a horizontal line', [I]),
      Painted(I, 0), Painted(I, 2 + I));
end;


procedure TTestCanvasLines.TestAPatternLineStartingLeftOfTheCanvas;

var
  lX: Integer;

begin
  FCanvas.Pen.Style := psDot;
  FCanvas.Line(-5, 4, 10, 4);
  for lX := 1 to 10 do
    AssertTrue(Format('Dots alternate at x=%d', [lX]), Painted(lX, 4) <> Painted(lX - 1, 4));
end;


procedure TTestCanvasLines.TestAClippedLineStaysInTheClipRect;

var
  lBounds: TRect;

begin
  FCanvas.ClipRect := Rect(5, 5, 15, 15);
  FCanvas.Clipping := True;
  FCanvas.Line(0, 10, 39, 10);
  FCanvas.Line(0, 0, 29, 29);
  lBounds := PaintedBounds;
  AssertEquals('Clipped on the left', 5, lBounds.Left);
  AssertEquals('Clipped on the right, which is excluded', 14, lBounds.Right);
  AssertEquals('Clipped at the top', 5, lBounds.Top);
  AssertEquals('Clipped at the bottom, which is excluded', 14, lBounds.Bottom);
end;


procedure TTestCanvasLines.TestALineOutsideTheClipRectDrawsNothing;

begin
  FCanvas.ClipRect := Rect(10, 10, 20, 20);
  FCanvas.Clipping := True;
  FCanvas.Line(15, 0, 15, 5);
  FCanvas.Line(0, 15, 5, 15);
  FCanvas.Line(0, 0, 5, 3);
  AssertEquals('Lines wholly outside the clip rect paint nothing', 0, CountPainted);
end;


procedure TTestCanvasLines.TestPolyBezierOfStraightControlPoints;

begin
  FCanvas.PolyBezier([Point(2, 5), Point(8, 5), Point(14, 5), Point(20, 5)]);
  AssertPaintedRect('A Bezier curve with control points on a line is that line', 2, 5, 21, 6);
end;


procedure TTestCanvasLines.TestPolyBezierReadsOnlyThePointsGiven;

var
  lPoints: array[0..7] of TPoint;

begin
  NewCanvas(60, 60, colWhite);
  lPoints[0] := Point(2, 2);
  lPoints[1] := Point(4, 2);
  lPoints[2] := Point(6, 2);
  lPoints[3] := Point(8, 2);
  lPoints[4] := Point(10, 2);
  lPoints[5] := Point(12, 2);
  lPoints[6] := Point(14, 2);
  lPoints[7] := Point(55, 55);
  FCanvas.PolyBezier(@lPoints[0], 7, False, True);
  AssertTrue('The first curve starts at point 0', Painted(2, 2));
  AssertTrue('The second curve ends at point 6', Painted(14, 2));
  AssertEquals('Nothing is drawn beyond the 7 points given (1 + 3 per curve)', 2, PaintedBounds.Bottom);
  NewCanvas(60, 60, colWhite);
  FCanvas.PolyBezier(@lPoints[0], 7);
  AssertTrue('Without Continuous the first curve ends at point 3', Painted(8, 2));
  AssertFalse('Without Continuous 7 points hold one curve of 4 points', Painted(14, 2));
  AssertEquals('Without Continuous nothing is drawn beyond the points given', 2, PaintedBounds.Bottom);
end;


{ TTestCanvasPenModes }

function TTestCanvasPenModes.Expected(aMode: TFPPenMode; const aPen, aDest: TFPColor): TFPColor;

  function Channel(aP, aD: Word): Word;

  begin
    case aMode of
      pmBlack: Result := 0;
      pmWhite: Result := $FFFF;
      pmNop: Result := aD;
      pmNot: Result := not aD;
      pmCopy: Result := aP;
      pmNotCopy: Result := not aP;
      pmMergePenNot: Result := aP or not aD;
      pmMaskPenNot: Result := aP and not aD;
      pmMergeNotPen: Result := not aP or aD;
      pmMaskNotPen: Result := not aP and aD;
      pmMerge: Result := aP or aD;
      pmNotMerge: Result := not (aP or aD);
      pmMask: Result := aP and aD;
      pmNotMask: Result := not (aP and aD);
      pmXor: Result := aP xor aD;
      pmNotXor: Result := not (aP xor aD);
    end;
  end;

begin
  Result.Red := Channel(aPen.Red, aDest.Red);
  Result.Green := Channel(aPen.Green, aDest.Green);
  Result.Blue := Channel(aPen.Blue, aDest.Blue);
  Result.Alpha := aDest.Alpha;
end;


procedure TTestCanvasPenModes.DrawShape(aShape: Integer);

begin
  FCanvas.Brush.Style := bsClear;
  case aShape of
    0: FCanvas.Line(3, 5, 30, 5);
    1: FCanvas.Line(3, 5, 30, 20);
    2:
      begin
      FCanvas.Pen.Style := psDot;
      FCanvas.Line(3, 5, 30, 5);
      end;
    3: FCanvas.Rectangle(3, 3, 20, 15);
    4: FCanvas.Ellipse(3, 3, 25, 20);
    5: FCanvas.Polyline([Point(2, 2), Point(20, 2), Point(20, 20), Point(5, 25)]);
  end;
end;


procedure TTestCanvasPenModes.CheckModes(const aName: String; aShape: Integer);

var
  lShape: array of TPoint;
  lMode: TFPPenMode;
  lX, lY, I: Integer;
  lWrong, lModeName: String;
  lWant: TFPColor;

begin
  NewCanvas(40, 30, cBackground);
  FCanvas.Pen.FPColor := cPenColor;
  FCanvas.Pen.Mode := pmCopy;
  DrawShape(aShape);
  lShape := nil;
  for lY := 0 to 29 do
    for lX := 0 to 39 do
      if Painted(lX, lY, cBackground) then
        begin
        SetLength(lShape, Length(lShape) + 1);
        lShape[High(lShape)] := Point(lX, lY);
        end;
  AssertTrue(aName + ': the shape paints pixels in copy mode', Length(lShape) > 0);
  lWrong := '';
  for lMode := Low(TFPPenMode) to High(TFPPenMode) do
    begin
    NewCanvas(40, 30, cBackground);
    FCanvas.Pen.FPColor := cPenColor;
    FCanvas.Pen.Mode := lMode;
    DrawShape(aShape);
    lWant := Expected(lMode, cPenColor, cBackground);
    for I := 0 to High(lShape) do
      if not (FImage[lShape[I].X, lShape[I].Y] = lWant) then
        begin
        WriteStr(lModeName, lMode);
        lWrong := lWrong + Format(' %s: expected %s, got %s at (%d,%d);',
          [lModeName, ColorToStr(lWant), ColorToStr(FImage[lShape[I].X, lShape[I].Y]), lShape[I].X, lShape[I].Y]);
        Break;
        end;
    end;
  AssertEquals(aName + ': every pen mode follows its raster operation, alpha kept', '', lWrong);
end;


procedure TTestCanvasPenModes.TestModesOnASolidLine;

begin
  CheckModes('Horizontal line', 0);
end;


procedure TTestCanvasPenModes.TestModesOnASlopedLine;

begin
  CheckModes('Sloped line', 1);
end;


procedure TTestCanvasPenModes.TestModesOnAPatternLine;

begin
  CheckModes('Dotted line', 2);
end;


procedure TTestCanvasPenModes.TestModesOnARectangle;

begin
  CheckModes('Rectangle outline', 3);
end;


procedure TTestCanvasPenModes.TestModesOnAnEllipse;

begin
  CheckModes('Ellipse outline', 4);
end;


procedure TTestCanvasPenModes.TestModesOnAPolyline;

begin
  CheckModes('Polyline', 5);
end;


{ TTestCanvasRectangles }

procedure TTestCanvasRectangles.TestFillRectExcludesRightAndBottom;

begin
  FCanvas.FillRect(Rect(2, 3, 7, 9));
  AssertPaintedRect('FillRect covers left..right-1 and top..bottom-1', 2, 3, 7, 9);
  AssertColorsEqual('with the brush colour', colRed, FImage[2, 3]);
end;


procedure TTestCanvasRectangles.TestFillRectSortsItsCorners;

begin
  FCanvas.FillRect(7, 9, 2, 3);
  AssertPaintedRect('Swapped corners fill the same rectangle', 2, 3, 7, 9);
end;


procedure TTestCanvasRectangles.TestAnEmptyFillRectDrawsNothing;

begin
  FCanvas.FillRect(Rect(5, 5, 5, 10));
  FCanvas.FillRect(Rect(5, 5, 10, 5));
  AssertEquals('A rectangle of no width or height paints nothing', 0, CountPainted);
end;


procedure TTestCanvasRectangles.TestRectangleOutlineExcludesRightAndBottom;

var
  lX, lY: Integer;

begin
  FCanvas.Brush.Style := bsClear;
  FCanvas.Rectangle(2, 3, 9, 8);
  for lY := 0 to 29 do
    for lX := 0 to 39 do
      AssertEquals(Format('Outline of Rect(2,3,9,8) at (%d,%d)', [lX, lY]),
        ((lX = 2) or (lX = 8)) and (lY >= 3) and (lY <= 7) or ((lY = 3) or (lY = 7)) and (lX >= 2) and (lX <= 8),
        Painted(lX, lY));
end;


procedure TTestCanvasRectangles.TestRectangleWithABrushFillsTheInside;

begin
  FCanvas.Rectangle(2, 3, 9, 8);
  AssertColorsEqual('The outline has the pen colour', colBlack, FImage[2, 3]);
  AssertColorsEqual('The inside has the brush colour', colRed, FImage[5, 5]);
  AssertEquals('Nothing right of the rectangle', False, Painted(9, 5));
  AssertEquals('Nothing below the rectangle', False, Painted(5, 8));
end;


procedure TTestCanvasRectangles.TestARectanglePartlyLeftOfTheCanvas;

begin
  FCanvas.Brush.Style := bsClear;
  FCanvas.Rectangle(-5, 10, 20, 25);
  AssertTrue('The visible part of the top edge', Painted(10, 10));
  AssertTrue('The visible part of the bottom edge', Painted(10, 24));
  AssertTrue('The right edge', Painted(19, 17));
end;


procedure TTestCanvasRectangles.TestAThickRectangleStaysInside;

var
  lBounds: TRect;

begin
  FCanvas.Brush.Style := bsClear;
  FCanvas.Pen.Width := 3;
  FCanvas.Rectangle(5, 5, 20, 15);
  lBounds := PaintedBounds;
  AssertEquals('Left edge stays inside', 5, lBounds.Left);
  AssertEquals('Top edge stays inside', 5, lBounds.Top);
  AssertEquals('Right edge stays inside the excluded right', 19, lBounds.Right);
  AssertEquals('Bottom edge stays inside the excluded bottom', 14, lBounds.Bottom);
  AssertTrue('The outline is 3 pixels thick', Painted(7, 10) and not Painted(8, 10));
end;


procedure TTestCanvasRectangles.TestADashedRectangle;

begin
  FCanvas.Brush.Style := bsClear;
  FCanvas.Pen.Style := psDash;
  FCanvas.Rectangle(0, 0, 20, 10);
  AssertTrue('A dash is painted', Painted(1, 0));
  AssertFalse('A gap is left', Painted(3, 0));
end;


procedure TTestCanvasRectangles.TestAClearBrushFillsNothing;

begin
  FCanvas.Brush.Style := bsClear;
  FCanvas.FillRect(0, 0, 20, 20);
  AssertEquals('A clear brush fills nothing', 0, CountPainted);
end;


procedure TTestCanvasRectangles.TestHatchOriginCanvasJoinsAdjacentRectangles;

var
  lX: Integer;

begin
  FCanvas.HashWidth := 4;
  FCanvas.HatchOrigin := hoCanvas;
  FCanvas.Brush.Style := bsVertical;
  FCanvas.FillRect(1, 0, 11, 10);
  FCanvas.FillRect(11, 0, 22, 10);
  for lX := 1 to 21 do
    AssertEquals(Format('Column %d is a hatch line when it is a multiple of 4', [lX]), lX mod 4 = 0, Painted(lX, 5));
end;


procedure TTestCanvasRectangles.TestHatchOriginShapeStartsAtTheCorner;

var
  lX: Integer;

begin
  AssertTrue('HatchOrigin is hoDefault by default', FCanvas.HatchOrigin = hoDefault);
  FCanvas.HashWidth := 4;
  FCanvas.HatchOrigin := hoShape;
  FCanvas.Brush.Style := bsVertical;
  FCanvas.FillRect(3, 2, 20, 10);
  for lX := 3 to 19 do
    AssertEquals(Format('Column %d is a hatch line counted from the left of the rectangle', [lX]),
      (lX - 3) mod 4 = 0, Painted(lX, 5));
  AssertFalse('Nothing is painted above the rectangle', Painted(3, 1));
end;


procedure TTestCanvasRectangles.TestHatchOriginShapeOnAFloodFill;

var
  lY: Integer;

begin
  FCanvas.HashWidth := 5;
  FCanvas.HatchOrigin := hoShape;
  FCanvas.Brush.Style := bsHorizontal;
  FCanvas.FloodFill(7, 12);
  for lY := 0 to 29 do
    AssertEquals(Format('Row %d is a hatch line counted from the start of the flood fill', [lY]),
      (lY - 12) mod 5 = 0, Painted(7, lY));
end;


procedure TTestCanvasRectangles.TestBrushFillsIgnoreThePenMode;

var
  lStyle: TFPBrushStyle;
  lCopy: TFPMemoryImage;
  lName: String;
  lPattern: TBrushPattern;
  I: Integer;

  procedure DrawShapes;
  begin
    FCanvas.HashWidth := 4;
    FCanvas.Pen.Style := psClear;
    FCanvas.Brush.Style := lStyle;
    FCanvas.Brush.Pattern := lPattern;
    FCanvas.FillRect(2, 2, 18, 14);
    FCanvas.Polygon([Point(20, 2), Point(38, 2), Point(30, 28)]);
    FCanvas.Ellipse(2, 16, 18, 28);
  end;

begin
  for I := 0 to High(lPattern) do
    lPattern[I] := $F0F0F0F0 shr (I mod 4);
  lCopy := TFPMemoryImage.Create(0, 0);
  try
    for lStyle in [bsPattern, bsHorizontal, bsVertical, bsFDiagonal, bsBDiagonal, bsCross, bsDiagCross] do
      begin
      NewCanvas(40, 30, colWhite);
      DrawShapes;
      lCopy.Assign(FImage);
      NewCanvas(40, 30, colWhite);
      FCanvas.Pen.Mode := pmXor;
      DrawShapes;
      WriteStr(lName, lStyle);
      AssertImagesEqual('A ' + lName + ' brush paints the same with an XOR pen', lCopy, FImage);
      end;
  finally
    lCopy.Free;
  end;
end;


procedure TTestCanvasRectangles.TestHatchedFills;

var
  lStyle: TFPBrushStyle;
  lCount: Integer;
  lBounds: TRect;
  lName: String;

begin
  for lStyle in [bsHorizontal, bsVertical, bsFDiagonal, bsBDiagonal, bsCross, bsDiagCross] do
    begin
    NewCanvas(40, 30, colWhite);
    FCanvas.HashWidth := 5;
    FCanvas.Brush.Style := lStyle;
    FCanvas.FillRect(2, 2, 32, 22);
    WriteStr(lName, lStyle);
    lCount := CountPainted;
    AssertTrue(lName + ' paints some pixels', lCount > 0);
    AssertTrue(lName + ' leaves gaps', lCount < 30 * 20);
    lBounds := PaintedBounds;
    AssertTrue(lName + ' stays inside the rectangle, right and bottom excluded',
      (lBounds.Left >= 2) and (lBounds.Top >= 2) and (lBounds.Right <= 31) and (lBounds.Bottom <= 21));
    end;
end;


procedure TTestCanvasRectangles.TestABrushPatternStartsWithTheMostSignificantBit;

var
  lPattern: TBrushPattern;
  I, lX: Integer;

begin
  for I := 0 to High(lPattern) do
    lPattern[I] := $80000000;
  FCanvas.Brush.Style := bsPattern;
  FCanvas.Brush.Pattern := lPattern;
  FCanvas.FillRect(0, 0, 40, 10);
  for lX := 0 to 39 do
    AssertEquals(Format('Only columns where x mod 32 = 0 are painted, x=%d', [lX]),
      (lX mod 32) = 0, Painted(lX, 5));
end;


procedure TTestCanvasRectangles.TestABrushImageIsTiledFromTheCanvasOrigin;

var
  lTile: TFPMemoryImage;

begin
  lTile := CreateCheckerImage(4, 4, 2, colBlue, colGreen);
  try
    FCanvas.Brush.Style := bsImage;
    FCanvas.Brush.Image := lTile;
    FCanvas.FillRect(3, 3, 13, 13);
    AssertColorsEqual('Pixel (4,4) takes tile pixel (0,0)', colBlue, FImage[4, 4]);
    AssertColorsEqual('Pixel (6,4) takes tile pixel (2,0)', colGreen, FImage[6, 4]);
    AssertFalse('Nothing at the excluded right edge', Painted(13, 5));
  finally
    FCanvas.Brush.Image := nil;
    lTile.Free;
  end;
end;


procedure TTestCanvasRectangles.TestARelativeBrushImageIsTiledFromTheRectangle;

var
  lTile: TFPMemoryImage;

begin
  lTile := CreateCheckerImage(4, 4, 2, colBlue, colGreen);
  try
    FCanvas.Brush.Style := bsImage;
    FCanvas.Brush.Image := lTile;
    FCanvas.RelativeBrushImage := True;
    FCanvas.FillRect(3, 3, 13, 13);
    AssertColorsEqual('Pixel (3,3) takes tile pixel (0,0)', colBlue, FImage[3, 3]);
    AssertColorsEqual('Pixel (5,3) takes tile pixel (2,0)', colGreen, FImage[5, 3]);
  finally
    FCanvas.Brush.Image := nil;
    lTile.Free;
  end;
end;


procedure TTestCanvasRectangles.TestTheClipRectExcludesRightAndBottom;

begin
  FCanvas.ClipRect := Rect(5, 6, 10, 12);
  FCanvas.Clipping := True;
  FCanvas.FillRect(0, 0, 40, 30);
  AssertPaintedRect('Only the clip rect is painted, its right and bottom excluded', 5, 6, 10, 12);
end;


{ TTestCanvasRectangleMode }

procedure TTestCanvasRectangleMode.UseInclude;

begin
  FCanvas.RectangleMode := rmInclude;
end;


procedure TTestCanvasRectangleMode.TestTheDefaultIsInclude;

var
  lCanvas: TFPImageCanvas;

begin
  lCanvas := TFPImageCanvas.Create(FImage);
  try
    AssertTrue('A new canvas includes Right and Bottom', lCanvas.RectangleMode = rmInclude);
  finally
    lCanvas.Free;
  end;
end;


procedure TTestCanvasRectangleMode.TestFillRectIncludesRightAndBottom;

begin
  UseInclude;
  FCanvas.FillRect(Rect(2, 3, 7, 9));
  AssertPaintedRect('FillRect covers left..right and top..bottom', 2, 3, 8, 10);
end;


procedure TTestCanvasRectangleMode.TestRectangleOutlineIncludesRightAndBottom;

var
  lBounds: TRect;

begin
  UseInclude;
  FCanvas.Brush.Style := bsClear;
  FCanvas.Rectangle(2, 3, 9, 8);
  lBounds := PaintedBounds;
  AssertEquals('The right edge is at Right', 9, lBounds.Right);
  AssertEquals('The bottom edge is at Bottom', 8, lBounds.Bottom);
  AssertTrue('The right edge is drawn', Painted(9, 5));
  AssertFalse('The inside stays empty', Painted(5, 5));
end;


procedure TTestCanvasRectangleMode.TestAZeroWidthRectIsOneColumnWhenIncluded;

begin
  UseInclude;
  FCanvas.FillRect(Rect(5, 5, 5, 9));
  AssertPaintedRect('A rectangle with Left = Right is one column', 5, 5, 6, 10);
end;


procedure TTestCanvasRectangleMode.TestEllipseBoundsWhenIncluded;

var
  lBounds: TRect;

begin
  UseInclude;
  FCanvas.Brush.Style := bsClear;
  FCanvas.Ellipse(4, 3, 24, 17);
  lBounds := PaintedBounds;
  AssertEquals('The leftmost pixel is Left', 4, lBounds.Left);
  AssertEquals('The rightmost pixel is Right', 24, lBounds.Right);
  AssertEquals('The topmost pixel is Top', 3, lBounds.Top);
  AssertEquals('The lowest pixel is Bottom', 17, lBounds.Bottom);
end;


procedure TTestCanvasRectangleMode.TestEllipseCIsCentredWhenIncluded;

begin
  UseInclude;
  FCanvas.Brush.Style := bsClear;
  FCanvas.EllipseC(15, 12, 6, 4);
  AssertTrue('The circle reaches x-rx', Painted(9, 12));
  AssertTrue('The circle reaches x+rx', Painted(21, 12));
  AssertTrue('The circle reaches y-ry', Painted(15, 8));
  AssertTrue('The circle reaches y+ry', Painted(15, 16));
  AssertFalse('and goes no further right', Painted(22, 12));
end;


procedure TTestCanvasRectangleMode.TestTheClipRectIncludesRightAndBottom;

begin
  UseInclude;
  FCanvas.ClipRect := Rect(5, 6, 10, 12);
  FCanvas.Clipping := True;
  FCanvas.FillRect(0, 0, 39, 29);
  AssertPaintedRect('Only the clip rect is painted, its right and bottom included', 5, 6, 11, 13);
end;


procedure TTestCanvasRectangleMode.TestTheClipRectComesBackAsSet;

var
  lRect: TRect;

begin
  FCanvas.ClipRect := Rect(5, 6, 10, 12);
  lRect := FCanvas.ClipRect;
  AssertEquals('Excluded: Right comes back as set', 10, lRect.Right);
  AssertEquals('Excluded: Bottom comes back as set', 12, lRect.Bottom);
  UseInclude;
  FCanvas.ClipRect := Rect(5, 6, 10, 12);
  lRect := FCanvas.ClipRect;
  AssertEquals('Included: Right comes back as set', 10, lRect.Right);
  AssertEquals('Included: Bottom comes back as set', 12, lRect.Bottom);
end;


procedure TTestCanvasRectangleMode.TestTheClipStaysWhenTheModeChanges;

var
  lRect: TRect;

begin
  FCanvas.ClipRect := Rect(5, 6, 10, 12);
  UseInclude;
  lRect := FCanvas.ClipRect;
  AssertEquals('The same clip seen with Right included', 9, lRect.Right);
  AssertEquals('The same clip seen with Bottom included', 11, lRect.Bottom);
  FCanvas.Clipping := True;
  FCanvas.FillRect(0, 0, 39, 29);
  AssertPaintedRect('The pixels clipped do not change with the mode', 5, 6, 10, 12);
end;


procedure TTestCanvasRectangleMode.TestCopyRectIncludesRightAndBottom;

var
  lSource: TFPMemoryImage;
  lSourceCanvas: TFPImageCanvas;

begin
  UseInclude;
  lSource := CreateGradientImage(6, 4);
  lSourceCanvas := TFPImageCanvas.Create(lSource);
  try
    FCanvas.CopyRect(10, 10, lSourceCanvas, Rect(1, 1, 3, 3));
    AssertPaintedRect('CopyRect copies left..right and top..bottom', 10, 10, 13, 13);
  finally
    lSourceCanvas.Free;
    lSource.Free;
  end;
end;


procedure TTestCanvasRectangleMode.TestAGradientCoversWhatFillRectCovers;

begin
  UseInclude;
  FCanvas.GradientFill(Rect(3, 4, 13, 9), colBlack, colBlue, gdVertical);
  AssertPaintedRect('An included gradient covers left..right and top..bottom', 3, 4, 14, 10);
  NewCanvas(40, 30, colWhite);
  FCanvas.GradientFill(Rect(3, 4, 13, 9), colBlack, colBlue, gdHorizontal);
  AssertPaintedRect('An excluded gradient covers left..right-1 and top..bottom-1', 3, 4, 13, 9);
end;


procedure TTestCanvasRectangleMode.TestScaleWhenIncluded;

begin
  UseInclude;
  FCanvas.Scale(2, 2);
  FCanvas.FillRect(1, 1, 2, 2);
  AssertPaintedRect('Two included pixels scaled by 2 become four', 2, 2, 6, 6);
end;


procedure TTestCanvasRectangleMode.TestClearFillsTheCanvasInBothModes;

begin
  FCanvas.Clear;
  AssertPaintedRect('Clear with rmExclude fills the whole canvas', 0, 0, 40, 30);
  NewCanvas(40, 30, colWhite);
  UseInclude;
  FCanvas.Clear;
  AssertPaintedRect('Clear with rmInclude fills the whole canvas', 0, 0, 40, 30);
end;


procedure TTestCanvasRectangleMode.TestBarcodesDrawTheSameInBothModes;

var
  lIncluded: TFPMemoryImage;

  procedure DrawIt;

  var
    lDraw: TFPDrawBarCode;

  begin
    lDraw := TFPDrawBarCode.Create;
    try
      lDraw.Canvas := FCanvas;
      lDraw.Rect := Rect(2, 2, 38, 20);
      lDraw.Text := '12345670';
      lDraw.Encoding := beEAN8;
      lDraw.Draw;
    finally
      lDraw.Free;
    end;
  end;

begin
  UseInclude;
  DrawIt;
  lIncluded := TFPMemoryImage.Create(0, 0);
  try
    lIncluded.Assign(FImage);
    NewCanvas(40, 30, colWhite);
    DrawIt;
    AssertTrue('The barcode drawer leaves the mode of the canvas as it was', FCanvas.RectangleMode = rmExclude);
    AssertImagesEqual('A barcode drawn on an rmExclude canvas looks as on an rmInclude one', lIncluded, FImage);
    AssertTrue('and is drawn', CountPainted > 0);
  finally
    lIncluded.Free;
  end;
end;


{ TTestCanvasEllipses }

procedure TTestCanvasEllipses.TestAnEllipseFitsItsBounds;

var
  lBounds: TRect;

begin
  FCanvas.Brush.Style := bsClear;
  FCanvas.Ellipse(4, 3, 25, 18);
  lBounds := PaintedBounds;
  AssertEquals('The leftmost pixel is the left bound', 4, lBounds.Left);
  AssertEquals('The topmost pixel is the top bound', 3, lBounds.Top);
  AssertEquals('The rightmost pixel is right-1', 24, lBounds.Right);
  AssertEquals('The lowest pixel is bottom-1', 17, lBounds.Bottom);
end;


procedure TTestCanvasEllipses.TestAnEllipseIsSymmetric;

var
  lX, lY: Integer;

begin
  FCanvas.Brush.Style := bsClear;
  FCanvas.Ellipse(4, 3, 25, 18);
  for lY := 3 to 17 do
    for lX := 4 to 24 do
      begin
      AssertEquals(Format('Mirrored left-right at (%d,%d)', [lX, lY]), Painted(lX, lY), Painted(28 - lX, lY));
      AssertEquals(Format('Mirrored top-bottom at (%d,%d)', [lX, lY]), Painted(lX, lY), Painted(lX, 20 - lY));
      end;
end;


procedure TTestCanvasEllipses.TestAFilledEllipse;

begin
  FCanvas.Pen.Style := psClear;
  FCanvas.Ellipse(4, 3, 25, 18);
  AssertColorsEqual('The centre has the brush colour', colRed, FImage[14, 10]);
  AssertFalse('The corner of the bounds stays empty', Painted(4, 3));
  AssertFalse('Nothing at the excluded right bound', Painted(25, 10));
end;


procedure TTestCanvasEllipses.TestEllipseCIsCentred;

begin
  FCanvas.Brush.Style := bsClear;
  FCanvas.EllipseC(15, 12, 6, 4);
  AssertTrue('The circle reaches x-rx', Painted(9, 12));
  AssertTrue('The circle reaches x+rx', Painted(21, 12));
  AssertTrue('The circle reaches y-ry', Painted(15, 8));
  AssertTrue('The circle reaches y+ry', Painted(15, 16));
end;


procedure TTestCanvasEllipses.TestAThickEllipseStaysInside;

var
  lBounds: TRect;

begin
  FCanvas.Brush.Style := bsClear;
  FCanvas.Pen.Width := 3;
  FCanvas.EllipseMode := emInside;
  FCanvas.Ellipse(4, 3, 25, 18);
  lBounds := PaintedBounds;
  AssertTrue('A thick outline stays inside the bounds',
    (lBounds.Left >= 4) and (lBounds.Top >= 3) and (lBounds.Right <= 24) and (lBounds.Bottom <= 17));
  AssertTrue('and is thicker than one pixel', Painted(5, 10) or Painted(6, 10));
end;


procedure TTestCanvasEllipses.TestAThickEllipseIsCentredByDefault;

var
  lBounds: TRect;

begin
  FCanvas.Brush.Style := bsClear;
  FCanvas.Pen.Width := 3;
  FCanvas.Ellipse(4, 3, 25, 18);
  lBounds := PaintedBounds;
  AssertTrue('EllipseMode is emCentered by default', FCanvas.EllipseMode = emCentered);
  AssertEquals('A centred thick outline reaches one pixel left of the bounds', 3, lBounds.Left);
  AssertEquals('A centred thick outline reaches one pixel above the bounds', 2, lBounds.Top);
  AssertEquals('A centred thick outline reaches one pixel right of the bounds', 25, lBounds.Right);
  AssertEquals('A centred thick outline reaches one pixel below the bounds', 18, lBounds.Bottom);
end;


procedure TTestCanvasEllipses.TestAnArcOfAQuarter;

var
  lBounds: TRect;

begin
  FCanvas.Arc(4, 4, 24, 24, 0, 90 * 16);
  lBounds := PaintedBounds;
  AssertTrue('The arc starts at 3 o''clock on the right edge', Painted(23, 14) or Painted(23, 13));
  AssertTrue('The arc ends at 12 o''clock on the top edge', Painted(14, 4) or Painted(13, 4));
  AssertTrue('A quarter from 0 over 90 degrees stays in the top right', (lBounds.Left >= 13) and (lBounds.Bottom <= 14));
  AssertEquals('The arc reaches the right of the bounds, which is excluded', 23, lBounds.Right);
  AssertEquals('The arc reaches the top of the bounds', 4, lBounds.Top);
end;


procedure TTestCanvasEllipses.TestAnArcByPointsMatchesTheAngles;

var
  lAngles: TFPMemoryImage;

begin
  FCanvas.Arc(4, 4, 24, 24, 90 * 16, 180 * 16);
  lAngles := TFPMemoryImage.Create(0, 0);
  try
    lAngles.Assign(FImage);
    NewCanvas(40, 30, colWhite);
    FCanvas.Arc(4, 4, 24, 24, 14, 0, 14, 30);
    AssertImagesEqual('The arc from the ray through the start point to the ray through the end point, counter-clockwise', lAngles, FImage);
  finally
    lAngles.Free;
  end;
end;


procedure TTestCanvasEllipses.TestARadialPie;

begin
  FCanvas.RadialPie(4, 4, 24, 24, 0, 90 * 16);
  AssertColorsEqual('The brush fills the quarter between the radii', colRed, FImage[18, 9]);
  AssertFalse('The other quarters stay empty', Painted(9, 18));
  AssertFalse('The top left quarter stays empty', Painted(9, 9));
  AssertColorsEqual('The pen draws the radius to 3 o''clock', colBlack, FImage[20, 14]);
  AssertColorsEqual('The pen draws the radius to 12 o''clock', colBlack, FImage[14, 8]);
end;


procedure TTestCanvasEllipses.TestADottedEllipse;

var
  lSolid: Integer;

begin
  FCanvas.Brush.Style := bsClear;
  FCanvas.Ellipse(4, 3, 25, 18);
  lSolid := CountPainted;
  NewCanvas(40, 30, colWhite);
  FCanvas.Brush.Style := bsClear;
  FCanvas.Pen.Style := psDot;
  FCanvas.Ellipse(4, 3, 25, 18);
  AssertTrue('A dotted ellipse paints about half the pixels of a solid one',
    (CountPainted > lSolid div 3) and (CountPainted < (lSolid * 2) div 3));
end;


{ TTestCanvasPolygons }

procedure TTestCanvasPolygons.FillSquareAsPolygon;

begin
  FCanvas.Pen.Style := psClear;
  FCanvas.Polygon([Point(5, 5), Point(15, 5), Point(15, 12), Point(5, 12)]);
end;


procedure TTestCanvasPolygons.TestAFilledTriangle;

begin
  FCanvas.Pen.Style := psClear;
  FCanvas.Polygon([Point(5, 5), Point(25, 5), Point(5, 25)]);
  AssertColorsEqual('Inside the triangle', colRed, FImage[8, 8]);
  AssertFalse('Outside the long side', Painted(20, 20));
  AssertFalse('Left of the triangle', Painted(3, 10));
end;


procedure TTestCanvasPolygons.TestAPolygonOutlineIsClosed;

begin
  FCanvas.Brush.Style := bsClear;
  FCanvas.Polygon([Point(5, 5), Point(25, 5), Point(5, 25)]);
  AssertTrue('The closing edge from the last to the first point', Painted(5, 15));
  AssertTrue('The long side', Painted(15, 15));
  AssertFalse('The inside stays empty', Painted(8, 8));
end;


procedure TTestCanvasPolygons.TestEvenOddAndNonZeroWinding;

const
  cPoints: array[0..9] of TPoint = ((X: 2; Y: 2), (X: 20; Y: 2), (X: 20; Y: 20), (X: 2; Y: 20), (X: 2; Y: 2),
    (X: 8; Y: 8), (X: 14; Y: 8), (X: 14; Y: 14), (X: 8; Y: 14), (X: 8; Y: 8));

begin
  FCanvas.Pen.Style := psClear;
  FCanvas.Polygon(cPoints);
  AssertTrue('Even-odd: the outer ring is filled', Painted(4, 10));
  AssertFalse('Even-odd: the inner square, wound twice, is empty', Painted(11, 11));
  NewCanvas(40, 30, colWhite);
  FCanvas.Pen.Style := psClear;
  FCanvas.PolygonNonZeroWindingRule := True;
  FCanvas.Polygon(cPoints);
  AssertTrue('Non-zero: the outer ring is filled', Painted(4, 10));
  AssertTrue('Non-zero: the inner square, wound twice the same way, is filled', Painted(11, 11));
end;


procedure TTestCanvasPolygons.TestAPolygonSquareCoversTheSameAsFillRect;

begin
  FillSquareAsPolygon;
  AssertPaintedRect('A filled square polygon covers what FillRect(5,5,15,12) covers', 5, 5, 15, 12);
end;


procedure TTestCanvasPolygons.TestPatternsAgreeOnPolygonsAndRectangles;

var
  lPattern: TBrushPattern;
  lRect: TFPMemoryImage;
  I: Integer;

begin
  for I := 0 to High(lPattern) do
    lPattern[I] := $C0000000 shr (I mod 4);
  FCanvas.Brush.Style := bsPattern;
  FCanvas.Brush.Pattern := lPattern;
  FCanvas.FillRect(0, 0, 40, 30);
  lRect := TFPMemoryImage.Create(0, 0);
  try
    lRect.Assign(FImage);
    NewCanvas(40, 30, colWhite);
    FCanvas.Brush.Style := bsPattern;
    FCanvas.Brush.Pattern := lPattern;
    FCanvas.Pen.Style := psClear;
    FCanvas.Polygon([Point(-1, -1), Point(41, -1), Point(41, 31), Point(-1, 31)]);
    AssertImagesEqual('A pattern fills a polygon like a rectangle', lRect, FImage);
  finally
    lRect.Free;
  end;
end;


{ TTestCanvasFlood }

procedure TTestCanvasFlood.DrawBox;

begin
  FCanvas.Line(5, 5, 15, 5);
  FCanvas.Line(15, 5, 15, 12);
  FCanvas.Line(15, 12, 5, 12);
  FCanvas.Line(5, 12, 5, 5);
end;


procedure TTestCanvasFlood.FloodWithTheSameColor;

begin
  FCanvas.Brush.FPColor := colWhite;
  FCanvas.FloodFill(20, 20);
end;


procedure TTestCanvasFlood.FloodWithAPattern;

var
  lPattern: TBrushPattern;
  lImage: TFPMemoryImage;
  lCanvas: TFPImageCanvas;
  I: Integer;

begin
  for I := 0 to High(lPattern) do
    lPattern[I] := $AAAAAAAA shr (I mod 2);
  lImage := CreateSolidImage(20, 20, colWhite);
  lCanvas := TFPImageCanvas.Create(lImage);
  try
    lCanvas.Brush.Style := bsPattern;
    lCanvas.Brush.Pattern := lPattern;
    lCanvas.FloodFill(3, 3);
  finally
    lCanvas.Free;
    lImage.Free;
  end;
end;


procedure TTestCanvasFlood.TestFloodFillFillsTheInsideOnly;

var
  lX, lY: Integer;

begin
  DrawBox;
  FCanvas.FloodFill(10, 8);
  for lY := 0 to 29 do
    for lX := 0 to 39 do
      if (lX > 5) and (lX < 15) and (lY > 5) and (lY < 12) then
        AssertColorsEqual(Format('Inside at (%d,%d)', [lX, lY]), colRed, FImage[lX, lY])
      else if (lX >= 5) and (lX <= 15) and (lY >= 5) and (lY <= 12) then
        AssertColorsEqual(Format('The border at (%d,%d) is kept', [lX, lY]), colBlack, FImage[lX, lY])
      else
        AssertColorsEqual(Format('Outside at (%d,%d) is untouched', [lX, lY]), colWhite, FImage[lX, lY]);
end;


procedure TTestCanvasFlood.TestFloodFillDoesNotLeakThroughACorner;

begin
  FCanvas.Line(0, 10, 10, 0);
  FCanvas.FloodFill(0, 0);
  AssertColorsEqual('The corner region is filled', colRed, FImage[2, 2]);
  AssertColorsEqual('The fill does not cross a diagonal line', colWhite, FImage[8, 8]);
end;


procedure TTestCanvasFlood.TestFloodFillOutsideTheCanvasDoesNothing;

begin
  FCanvas.FloodFill(-3, 5);
  FCanvas.FloodFill(50, 5);
  AssertEquals('A flood fill starting outside the canvas paints nothing', 0, CountPainted);
end;


procedure TTestCanvasFlood.TestFloodFillWithTheColorAlreadyThere;

begin
  FloodWithTheSameColor;
  AssertEquals('Filling with the colour already there changes nothing', 0, CountPainted);
end;


procedure TTestCanvasFlood.TestFloodFillWithAPatternMatchesAFilledRectangle;

var
  lPattern: TBrushPattern;
  lRect: TFPMemoryImage;
  I: Integer;

begin
  for I := 0 to High(lPattern) do
    lPattern[I] := $80000000 shr (I mod 3);
  FCanvas.Brush.Style := bsPattern;
  FCanvas.Brush.Pattern := lPattern;
  FCanvas.FillRect(0, 0, 40, 30);
  lRect := TFPMemoryImage.Create(0, 0);
  try
    lRect.Assign(FImage);
    NewCanvas(40, 30, colWhite);
    FCanvas.Brush.Style := bsPattern;
    FCanvas.Brush.Pattern := lPattern;
    FCanvas.FloodFill(10, 10);
    AssertImagesEqual('A pattern flood fill paints what a pattern rectangle paints', lRect, FImage);
  finally
    lRect.Free;
  end;
end;


procedure TTestCanvasFlood.TestARelativeImageFloodFillLeftOfTheStart;

var
  lTile: TFPMemoryImage;

begin
  lTile := TFPMemoryImage.Create(4, 1);
  try
    lTile[0, 0] := colRed;
    lTile[1, 0] := colGreen;
    lTile[2, 0] := colBlue;
    lTile[3, 0] := colYellow;
    FCanvas.Brush.Style := bsImage;
    FCanvas.Brush.Image := lTile;
    FCanvas.RelativeBrushImage := True;
    FCanvas.FloodFill(10, 5);
    AssertColorsEqual('The start pixel shows the first tile pixel', colRed, FImage[10, 5]);
    AssertColorsEqual('One pixel left of the start shows the last tile pixel', colYellow, FImage[9, 5]);
    AssertColorsEqual('Two pixels left of the start show the third tile pixel', colBlue, FImage[8, 5]);
  finally
    FCanvas.Brush.Image := nil;
    lTile.Free;
  end;
end;


procedure TTestCanvasFlood.TestAWideHatchFloodFill;

begin
  FCanvas.HashWidth := 40;
  FCanvas.Brush.Style := bsFDiagonal;
  FCanvas.FloodFill(10, 10);
  AssertTrue('A hatch 40 pixels wide paints its lines', CountPainted > 0);
  AssertTrue('and leaves the rest', CountPainted < 40 * 30);
end;


procedure TTestCanvasFlood.TestFloodFillWithAPatternDoesNotLeak;

begin
  AssertNoLeak('A pattern flood fill frees its bookkeeping', @FloodWithAPattern);
end;


procedure TTestCanvasFlood.TestFloodFillWithAHatchStaysInside;

var
  lX, lY, lInside: Integer;

begin
  DrawBox;
  FCanvas.Brush.Style := bsCross;
  FCanvas.HashWidth := 3;
  FCanvas.FloodFill(10, 8);
  lInside := 0;
  for lY := 0 to 29 do
    for lX := 0 to 39 do
      if FImage[lX, lY] = colRed then
        begin
        AssertTrue(Format('A hatch pixel at (%d,%d) is inside the box', [lX, lY]),
          (lX > 5) and (lX < 15) and (lY > 5) and (lY < 12));
        Inc(lInside);
        end;
  AssertTrue('A hatched flood fill paints some pixels', lInside > 0);
  AssertTrue('and leaves gaps', lInside < 9 * 6);
end;


procedure TTestCanvasFlood.TestFloodFillWithAnImage;

var
  lTile: TFPMemoryImage;

begin
  DrawBox;
  lTile := CreateCheckerImage(2, 2, 1, colBlue, colGreen);
  try
    FCanvas.Brush.Style := bsImage;
    FCanvas.Brush.Image := lTile;
    FCanvas.FloodFill(10, 8);
    AssertColorsEqual('Pixel (6,6) takes tile pixel (0,0)', colBlue, FImage[6, 6]);
    AssertColorsEqual('Pixel (7,6) takes tile pixel (1,0)', colGreen, FImage[7, 6]);
    AssertColorsEqual('Outside is untouched', colWhite, FImage[20, 20]);
  finally
    FCanvas.Brush.Image := nil;
    lTile.Free;
  end;
end;


{ TTestCanvasCopy }

procedure TTestCanvasCopy.SetUp;

begin
  inherited SetUp;
  FSource := CreateGradientImage(6, 4);
end;


procedure TTestCanvasCopy.TearDown;

begin
  FreeAndNil(FSource);
  inherited TearDown;
end;


procedure TTestCanvasCopy.TestDrawCopiesTheImage;

var
  lX, lY: Integer;

begin
  FCanvas.Draw(2, 3, FSource);
  for lY := 0 to 3 do
    for lX := 0 to 5 do
      AssertColorsEqual(Format('Image pixel (%d,%d)', [lX, lY]), FSource[lX, lY], FImage[2 + lX, 3 + lY]);
  AssertFalse('Nothing right of the image', Painted(8, 4));
  AssertFalse('Nothing below the image', Painted(3, 7));
end;


procedure TTestCanvasCopy.TestDrawPartlyOffTheCanvas;

begin
  FCanvas.Draw(-2, -1, FSource);
  AssertColorsEqual('The visible part is copied', FSource[2, 1], FImage[0, 0]);
  FCanvas.Draw(37, 28, FSource);
  AssertColorsEqual('The visible part at the far corner is copied', FSource[2, 1], FImage[39, 29]);
end;


procedure TTestCanvasCopy.TestDrawAlphaBlends;

begin
  FImage[1, 1] := colBlack;
  FSource[0, 0] := FPColor($FFFF, $FFFF, $FFFF, $8000);
  FCanvas.DrawingMode := dmAlphaBlend;
  FCanvas.Draw(1, 1, FSource);
  AssertColorsEqual('Draw blends in alpha blending mode', FPColor($8000, $8000, $8000), FImage[1, 1], 2);
end;


procedure TTestCanvasCopy.TestDrawRespectsTheClipRect;

begin
  FCanvas.ClipRect := Rect(3, 3, 5, 5);
  FCanvas.Clipping := True;
  FCanvas.Draw(2, 2, FSource);
  AssertPaintedRect('Draw paints only inside the clip rect', 3, 3, 5, 5);
end;


procedure TTestCanvasCopy.TestCopyRectExcludesRightAndBottom;

var
  lSourceCanvas: TFPImageCanvas;

begin
  lSourceCanvas := TFPImageCanvas.Create(FSource);
  try
    FCanvas.CopyRect(10, 10, lSourceCanvas, Rect(1, 1, 3, 3));
    AssertPaintedRect('CopyRect copies left..right-1 and top..bottom-1', 10, 10, 12, 12);
    AssertColorsEqual('The first pixel copied is source (1,1)', FSource[1, 1], FImage[10, 10]);
  finally
    lSourceCanvas.Free;
  end;
end;


procedure TTestCanvasCopy.TestAVerticalGradient;

var
  lY: Integer;

begin
  FCanvas.GradientFill(Rect(0, 0, 10, 11), colBlack, colWhite, gdVertical);
  AssertColorsEqual('The first row has the start colour', colBlack, FImage[5, 0]);
  AssertTrue('The last row is within one step of the end colour', FImage[5, 10].Red >= $FFFF - $FFFF div 10);
  for lY := 1 to 10 do
    AssertTrue(Format('Row %d is lighter than the row above', [lY]), FImage[5, lY].Red > FImage[5, lY - 1].Red);
end;


procedure TTestCanvasCopy.TestAHorizontalGradient;

var
  lX: Integer;

begin
  FCanvas.GradientFill(Rect(0, 0, 11, 5), colRed, colBlue, gdHorizontal);
  AssertColorsEqual('The first column has the start colour', colRed, FImage[0, 2]);
  for lX := 1 to 10 do
    AssertTrue(Format('Column %d is bluer than the one before', [lX]), FImage[lX, 2].Blue > FImage[lX - 1, 2].Blue);
end;


procedure TTestCanvasCopy.TestAGradientStaysInItsRect;

begin
  FCanvas.GradientFill(Rect(3, 4, 13, 9), colBlack, colBlue, gdVertical);
  AssertPaintedRect('A gradient covers its rect, right and bottom excluded', 3, 4, 13, 9);
end;


procedure TTestCanvasCopy.TestAnEmptyGradientLeavesThePenAlone;

begin
  FCanvas.Pen.FPColor := colGreen;
  FCanvas.Pen.Width := 4;
  FCanvas.Pen.Style := psDot;
  FCanvas.GradientFill(Rect(3, 4, 13, 4), colBlack, colBlue, gdVertical);
  AssertColorsEqual('The pen colour is restored', colGreen, FCanvas.Pen.FPColor);
  AssertEquals('The pen width is restored', 4, FCanvas.Pen.Width);
  AssertTrue('The pen style is restored', FCanvas.Pen.Style = psDot);
end;


procedure TTestCanvasCopy.TestTranslate;

begin
  FCanvas.Translate(5, 3);
  AssertTrue('A translation is a transformation', FCanvas.HasTransform);
  FCanvas.FillRect(0, 0, 2, 2);
  AssertPaintedRect('A translated FillRect moves by (5,3)', 5, 3, 7, 5);
end;


procedure TTestCanvasCopy.TestScale;

begin
  FCanvas.Scale(2, 2);
  FCanvas.FillRect(1, 1, 3, 3);
  AssertPaintedRect('A scaled FillRect doubles', 2, 2, 6, 6);
end;


procedure TTestCanvasCopy.TestResetTransform;

begin
  FCanvas.Translate(5, 3);
  FCanvas.ResetTransform;
  AssertFalse('After a reset there is no transformation', FCanvas.HasTransform);
  FCanvas.FillRect(0, 0, 2, 2);
  AssertPaintedRect('After a reset FillRect is not moved', 0, 0, 2, 2);
end;


function TTestCanvasLCLMethods.TakeImage: TFPMemoryImage;

var
  lX, lY: Integer;

begin
  Result := TFPMemoryImage.Create(FImage.Width, FImage.Height);
  for lY := 0 to FImage.Height - 1 do
    for lX := 0 to FImage.Width - 1 do
      Result.Colors[lX, lY] := FImage.Colors[lX, lY];
  NewCanvas(FImage.Width, FImage.Height, colWhite);
end;


procedure TTestCanvasLCLMethods.DrawStar(aWinding: Boolean);

begin
  FCanvas.Pen.Style := psClear;
  FCanvas.Brush.Style := bsSolid;
  FCanvas.Polygon([Point(20, 1), Point(27, 25), Point(8, 10), Point(32, 10), Point(13, 25)], aWinding);
end;


procedure TTestCanvasLCLMethods.TestFrameOutlinesWithoutFilling;

begin
  FCanvas.Brush.Style := bsSolid;
  FCanvas.Frame(2, 2, 10, 8);
  AssertTrue('The top-left corner is drawn with the pen', FImage[2, 2] = colBlack);
  AssertTrue('the bottom-right corner too, Right and Bottom excluded', FImage[9, 7] = colBlack);
  AssertTrue('The inside is not filled', FImage[5, 5] = colWhite);
  AssertEquals('Only the outline is painted', 2 * (8 + 6) - 4, CountPainted);
end;


procedure TTestCanvasLCLMethods.TestFrameRectDrawsABrushBorder;

begin
  FCanvas.Brush.Style := bsSolid;
  FCanvas.FrameRect(2, 2, 10, 8);
  AssertTrue('The border has the brush colour', FImage[2, 2] = colRed);
  AssertTrue('on all four sides', (FImage[9, 7] = colRed) and (FImage[9, 2] = colRed) and (FImage[2, 7] = colRed));
  AssertTrue('The inside is not filled', FImage[5, 5] = colWhite);
  AssertEquals('The border is one pixel wide', 2 * (8 + 6) - 4, CountPainted);
end;


procedure TTestCanvasLCLMethods.TestFrame3DColoursItsSidesAndShrinksTheRect;

var
  lRect: TRect;

begin
  lRect := Rect(2, 2, 12, 10);
  FCanvas.Frame3D(lRect, colBlue, colGreen, 2);
  AssertTrue('The top-left corner has the top colour', FImage[2, 2] = colBlue);
  AssertTrue('the bottom-left corner too', FImage[2, 9] = colBlue);
  AssertTrue('The top-right corner has the bottom colour', FImage[11, 2] = colGreen);
  AssertTrue('the bottom-right corner too', FImage[11, 9] = colGreen);
  AssertTrue('The second ring is drawn inside the first', (FImage[3, 3] = colBlue) and (FImage[10, 8] = colGreen));
  AssertTrue('Inside the rings nothing is drawn', FImage[4, 4] = colWhite);
  AssertTrue('The rect shrinks by the frame width', lRect = Rect(4, 4, 10, 8));
  AssertTrue('The brush is left as it was', FCanvas.Brush.FPColor = colRed);
end;


procedure TTestCanvasLCLMethods.TestRoundRectRoundsTheCorners;

begin
  FCanvas.Brush.Style := bsSolid;
  FCanvas.RoundRect(2, 2, 32, 26, 12, 12);
  AssertTrue('The corners of the bounds stay empty', (FImage[2, 2] = colWhite) and (FImage[31, 25] = colWhite)
    and (FImage[31, 2] = colWhite) and (FImage[2, 25] = colWhite));
  AssertTrue('The middles of the sides are drawn with the pen', (FImage[17, 2] = colBlack) and (FImage[2, 14] = colBlack)
    and (FImage[31, 14] = colBlack) and (FImage[17, 25] = colBlack));
  AssertTrue('The inside is filled with the brush', FImage[17, 14] = colRed);
end;


procedure TTestCanvasLCLMethods.TestRoundRectWithoutRadiiIsARectangle;

var
  lRect: TFPMemoryImage;

begin
  FCanvas.Brush.Style := bsSolid;
  FCanvas.Rectangle(2, 2, 32, 26);
  lRect := TakeImage;
  try
    FCanvas.Brush.Style := bsSolid;
    FCanvas.RoundRect(2, 2, 32, 26, 0, 0);
    AssertImagesEqual('RoundRect without corner radii draws the rectangle', lRect, FImage);
  finally
    lRect.Free;
  end;
end;


procedure TTestCanvasLCLMethods.TestChordCutsTheEllipseAlongItsChord;

begin
  FCanvas.Pen.Style := psClear;
  FCanvas.Brush.Style := bsSolid;
  FCanvas.Chord(0, 0, 21, 21, 0, 180 * 16);
  AssertTrue('The half above the chord is filled', FImage[10, 4] = colRed);
  AssertTrue('The half below it stays empty', FImage[10, 16] = colWhite);
  AssertTrue('Outside the ellipse nothing is drawn', FImage[0, 0] = colWhite);
end;


procedure TTestCanvasLCLMethods.TestPieTakesTheRaysThroughTwoPoints;

var
  lRadial: TFPMemoryImage;

begin
  FCanvas.Brush.Style := bsSolid;
  FCanvas.RadialPie(0, 0, 22, 22, 0, 90 * 16);
  lRadial := TakeImage;
  try
    FCanvas.Brush.Style := bsSolid;
    FCanvas.Pie(0, 0, 22, 22, 30, 11, 11, -5);
    AssertImagesEqual('Pie from the ray through (30,11) to the ray through (11,-5) is the quarter RadialPie', lRadial, FImage);
  finally
    lRadial.Free;
  end;
end;


procedure TTestCanvasLCLMethods.TestArcToDrawsTheLineAndMovesThePen;

begin
  FCanvas.MoveTo(0, 0);
  FCanvas.ArcTo(10, 10, 30, 30, 40, 20, 20, 0);
  AssertTrue('A line is drawn from the pen position', Painted(0, 0));
  AssertEquals('The pen ends at the top of the circle', 10, FCanvas.PenPos.Y);
  AssertTrue('in the middle', Abs(FCanvas.PenPos.X - 20) <= 1);
  AssertTrue('The arc is drawn', Painted(29, 20) or Painted(29, 19));
end;


procedure TTestCanvasLCLMethods.TestAngleArcFollowsTheLCLFormula;

begin
  FCanvas.MoveTo(0, 0);
  FCanvas.AngleArc(20, 15, 10, 0, 90);
  AssertTrue('A line is drawn from the pen position', Painted(0, 0));
  AssertTrue('The pen ends at the end of the arc, a quarter turn up', FCanvas.PenPos = Point(20, 5));
  AssertTrue('The arc is drawn', Painted(20, 5) or Painted(20, 6));
end;


procedure TTestCanvasLCLMethods.TestDrawFocusRectTwiceRestoresThePixels;

begin
  FCanvas.Pen.Width := 3;
  FCanvas.DrawFocusRect(Rect(2, 2, 20, 12));
  AssertTrue('A focus rect changes pixels', CountPainted > 0);
  AssertTrue('as a dotted outline', CountPainted < 2 * (18 + 10) - 4);
  FCanvas.DrawFocusRect(Rect(2, 2, 20, 12));
  AssertEquals('Drawing it again removes it', 0, CountPainted);
  AssertEquals('The pen is left as it was', 3, FCanvas.Pen.Width);
  AssertTrue('in its mode too', FCanvas.Pen.Mode = pmCopy);
end;


procedure TTestCanvasLCLMethods.TestPolygonTakesTheWindingRuleForOneCall;

begin
  DrawStar(True);
  AssertTrue('With Winding the centre of the star is filled', FImage[20, 13] = colRed);
  AssertFalse('and the canvas keeps its own rule', FCanvas.PolygonNonZeroWindingRule);
  NewCanvas(40, 30, colWhite);
  DrawStar(False);
  AssertTrue('Without Winding the centre stays empty', FImage[20, 13] = colWhite);
end;


procedure TTestCanvasLCLMethods.TestPolygonDrawsARangeOfPoints;

begin
  FCanvas.Brush.Style := bsClear;
  FCanvas.Polygon([Point(0, 0), Point(1, 1), Point(10, 5), Point(20, 5), Point(15, 20), Point(39, 29)], False, 2, 3);
  AssertTrue('Only the points from StartIndex are used', PaintedBounds = Rect(10, 5, 20, 20));
  NewCanvas(40, 30, colWhite);
  FCanvas.Polyline([Point(0, 0), Point(5, 5), Point(15, 5), Point(39, 29)], 1, 2);
  AssertTrue('Polyline draws NumPts points from StartIndex', PaintedBounds = Rect(5, 5, 15, 5));
end;


procedure TTestCanvasLCLMethods.TestPolygonAndPolylineFromAPointer;

var
  lPoints: array[0..3] of TPoint;
  lArray: TFPMemoryImage;

begin
  lPoints[0] := Point(3, 3);
  lPoints[1] := Point(30, 5);
  lPoints[2] := Point(25, 25);
  lPoints[3] := Point(5, 20);
  FCanvas.Brush.Style := bsSolid;
  FCanvas.Polygon(lPoints);
  FCanvas.Polyline([Point(0, 29), Point(39, 0)]);
  lArray := TakeImage;
  try
    FCanvas.Brush.Style := bsSolid;
    FCanvas.Polygon(@lPoints[0], 4);
    lPoints[0] := Point(0, 29);
    lPoints[1] := Point(39, 0);
    FCanvas.Polyline(@lPoints[0], 2);
    AssertImagesEqual('The pointer forms draw as the array forms', lArray, FImage);
  finally
    lArray.Free;
  end;
end;


procedure TTestCanvasLCLMethods.TestCopyRectScalesTheSource;

var
  lSource, lPart, lStretched: TFPMemoryImage;
  lSourceCanvas: TFPImageCanvas;
  lBox: TFPBoxInterpolation;
  lX, lY: Integer;

begin
  lSource := CreateCheckerImage(6, 6, 1, colBlue, colGreen);
  lPart := TFPMemoryImage.Create(4, 3);
  for lY := 0 to 2 do
    for lX := 0 to 3 do
      lPart.Colors[lX, lY] := lSource.Colors[1 + lX, 2 + lY];
  lSourceCanvas := TFPImageCanvas.Create(lSource);
  lBox := TFPBoxInterpolation.Create;
  try
    FCanvas.Interpolation := lBox;
    FCanvas.StretchDraw(2, 3, 12, 9, lPart);
    lStretched := TakeImage;
    try
      FCanvas.Interpolation := lBox;
      FCanvas.CopyRect(Rect(2, 3, 14, 12), lSourceCanvas, Rect(1, 2, 5, 5));
      AssertImagesEqual('A scaled CopyRect draws the source pixels as StretchDraw does', lStretched, FImage);
    finally
      lStretched.Free;
    end;
    NewCanvas(40, 30, colWhite);
    FCanvas.CopyRect(Rect(20, 10, 24, 13), lSourceCanvas, Rect(1, 2, 5, 5));
    AssertTrue('At the same size the pixels are copied', (FImage[20, 10] = lSource[1, 2]) and (FImage[23, 12] = lSource[4, 4]));
    AssertTrue('and nothing beyond Dest', FImage[24, 13] = colWhite);
  finally
    FCanvas.Interpolation := nil;
    lBox.Free;
    lSourceCanvas.Free;
    lPart.Free;
    lSource.Free;
  end;
end;


procedure TTestCanvasLCLMethods.TestStretchDrawIntoARect;

var
  lSource, lSized: TFPMemoryImage;
  lBox: TFPBoxInterpolation;

begin
  lSource := CreateCheckerImage(4, 3, 1, colBlue, colGreen);
  lBox := TFPBoxInterpolation.Create;
  try
    FCanvas.Interpolation := lBox;
    FCanvas.StretchDraw(2, 3, 12, 9, lSource);
    FCanvas.Interpolation := nil;
    lSized := TakeImage;
    try
      FCanvas.Interpolation := lBox;
      FCanvas.StretchDraw(Rect(2, 3, 14, 12), lSource);
      AssertImagesEqual('StretchDraw into a rect draws as StretchDraw with its size', lSized, FImage);
    finally
      lSized.Free;
    end;
  finally
    FCanvas.Interpolation := nil;
    lBox.Free;
    lSource.Free;
  end;
end;


procedure TTestCanvasLCLMethods.TestFloodFillSurfaceNeedsTheSeedColour;

begin
  FCanvas.Brush.Style := bsClear;
  FCanvas.Rectangle(2, 2, 20, 14);
  FCanvas.Brush.Style := bsSolid;
  FCanvas.FloodFill(8, 8, colBlue, ffSurface);
  AssertTrue('ffSurface does nothing when the start pixel has another colour', FImage[8, 8] = colWhite);
  FCanvas.FloodFill(8, 8, colWhite, ffSurface);
  AssertTrue('It fills the area of FillColor around the start', FImage[8, 8] = colRed);
  AssertTrue('up to the outline', (FImage[2, 8] = colBlack) and (FImage[0, 0] = colWhite));
end;


procedure TTestCanvasLCLMethods.TestFloodFillBorderFillsUpToTheBorderColour;

begin
  FCanvas.Pen.FPColor := colBlue;
  FCanvas.Brush.Style := bsClear;
  FCanvas.Rectangle(2, 2, 20, 14);
  FCanvas.Pen.FPColor := colBlack;
  FCanvas.Line(6, 5, 12, 5);
  FCanvas.Brush.Style := bsSolid;
  FCanvas.FloodFill(8, 8, colBlue, ffBorder);
  AssertTrue('ffBorder fills from the start', FImage[8, 8] = colRed);
  AssertTrue('over pixels of other colours', FImage[8, 5] = colRed);
  AssertTrue('up to the border colour', (FImage[2, 8] = colBlue) and (FImage[0, 0] = colWhite));
  NewCanvas(40, 30, colWhite);
  FCanvas.Pen.FPColor := colBlue;
  FCanvas.Brush.Style := bsClear;
  FCanvas.Rectangle(2, 2, 20, 14);
  FCanvas.Brush.Style := bsCross;
  FCanvas.HashWidth := 4;
  FCanvas.FloodFill(8, 8, colBlue, ffBorder);
  AssertTrue('A hatch brush fills on its lines', FImage[4, 8] = colRed);
  AssertTrue('and leaves the pixels between them', FImage[5, 9] = colWhite);
  AssertTrue('inside the border only', FImage[24, 8] = colWhite);
end;


procedure TTestCanvasLCLMethods.TestTheTextStyleStartsAsTheLCLDefault;

begin
  with FCanvas.TextStyle do
    begin
    AssertTrue('Left aligned', Alignment = taLeftJustify);
    AssertTrue('at the top', Layout = ftlTop);
    AssertTrue('on a single line', SingleLine);
    AssertTrue('breaking words', Wordbreak);
    AssertTrue('clipped', Clipping);
    AssertFalse('not opaque', Opaque);
    AssertFalse('without prefixes', ShowPrefix);
    end;
end;


procedure TRecordingCanvas.DoDraw(x, y: Integer; const aImage: TFPCustomImage);

begin
  Log := Log + Format('DoDraw %d,%d;', [x, y]);
  inherited DoDraw(x, y, aImage);
end;


procedure TRecordingCanvas.DoCopyRect(x, y: Integer; aCanvas: TFPCustomCanvas; const aSourceRect: TRect);

begin
  with aSourceRect do
    Log := Log + Format('DoCopyRect %d,%d %d,%d,%d,%d;', [x, y, Left, Top, Right, Bottom]);
  inherited DoCopyRect(x, y, aCanvas, aSourceRect);
end;


procedure TRecordingCanvas.DoStretchDraw(x, y, w, h: Integer; aSource: TFPCustomImage);

begin
  Log := Log + Format('DoStretchDraw %d,%d %dx%d;', [x, y, w, h]);
  inherited DoStretchDraw(x, y, w, h, aSource);
end;


procedure TTestCanvasHooks.SetUp;

begin
  inherited SetUp;
  FImage := CreateSolidImage(40, 30, colWhite);
  FSource := CreateSolidImage(4, 3, colRed);
  FCanvas := TRecordingCanvas.Create(FImage);
end;


procedure TTestCanvasHooks.TearDown;

begin
  FreeAndNil(FCanvas);
  FreeAndNil(FSource);
  FreeAndNil(FImage);
  inherited TearDown;
end;


procedure TTestCanvasHooks.TestDrawCallsDoDrawInDevicePixels;

begin
  FCanvas.Translate(5, 7);
  FCanvas.Draw(1, 2, FSource);
  AssertEquals('Draw passes the transformed position to DoDraw', 'DoDraw 6,9;', FCanvas.Log);
  AssertTrue('and DoDraw paints there', FImage.Colors[6, 9] = colRed);
  AssertTrue('not at the untransformed position', FImage.Colors[1, 2] = colWhite);
end;


procedure TTestCanvasHooks.TestCopyRectCallsDoCopyRectWithIncludedBounds;

var
  lSource: TFPImageCanvas;

begin
  lSource := TFPImageCanvas.Create(FSource);
  try
    FCanvas.RectangleMode := rmExclude;
    FCanvas.CopyRect(3, 4, lSource, Rect(0, 0, 2, 2));
    AssertEquals('CopyRect passes DoCopyRect the source with Right and Bottom included',
      'DoCopyRect 3,4 0,0,1,1;', FCanvas.Log);
    AssertTrue('and DoCopyRect copies the last pixel', FImage.Colors[4, 5] = colRed);
    AssertTrue('but not beyond it', FImage.Colors[5, 5] = colWhite);
  finally
    lSource.Free;
  end;
end;


procedure TTestCanvasHooks.TestStretchDrawCallsDoStretchDrawInDevicePixels;

begin
  FCanvas.Translate(5, 7);
  FCanvas.StretchDraw(1, 2, 8, 6, FSource);
  AssertEquals('StretchDraw passes the transformed position to DoStretchDraw', 'DoStretchDraw 6,9 8x6;', FCanvas.Log);
  AssertTrue('and DoStretchDraw paints there', FImage.Colors[13, 14] = colRed);
  AssertTrue('not at the untransformed position', FImage.Colors[1, 2] = colWhite);
end;


procedure TTestCanvasHooks.TestThePortablePropertiesAreOnTheBaseClass;

var
  lCanvas: TFPCustomCanvas;

begin
  lCanvas := FCanvas;
  AssertEquals('HashWidth starts at 15', 15, lCanvas.HashWidth);
  AssertTrue('HatchOrigin starts at hoDefault', lCanvas.HatchOrigin = hoDefault);
  AssertFalse('RelativeBrushImage starts off', lCanvas.RelativeBrushImage);
  AssertFalse('PolygonNonZeroWindingRule starts off', lCanvas.PolygonNonZeroWindingRule);
  lCanvas.HashWidth := 7;
  lCanvas.PolygonNonZeroWindingRule := True;
  AssertEquals('The pixel canvas sees the HashWidth set through the base class', 7, TFPPixelCanvas(lCanvas).HashWidth);
  AssertTrue('and the winding rule', TFPPixelCanvas(lCanvas).PolygonNonZeroWindingRule);
end;


initialization
  RegisterTests('canvas', [TTestCanvasPixels, TTestCanvasLines, TTestCanvasPenModes,
    TTestCanvasRectangles, TTestCanvasRectangleMode, TTestCanvasEllipses, TTestCanvasPolygons, TTestCanvasFlood,
    TTestCanvasCopy, TTestCanvasHooks, TTestCanvasLCLMethods]);
end.
