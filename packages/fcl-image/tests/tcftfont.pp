{
    Tests for text drawn with FreeType fonts (ftfont) on an image canvas,
    using examples/DejaVuLGCSans.ttf; ignored when libfreetype is missing.
    See the file COPYING.FPC, included in this distribution, for details.
}
unit tcftfont;

{$mode objfpc}{$H+}

interface

uses sysutils, classes, types, fpcunit, testregistry, fpimage, fpimgtests,
     fpcanvas, fpimgcanv, ftfont;

type
  TTestFreeTypeFont = class(TTestCase)
  private
    FImage: TFPMemoryImage;
    FCanvas: TFPImageCanvas;
    FFont: TFreeTypeFont;
    // The painted pixels, right and bottom included; Right is -1 when nothing is painted.
    function InkBounds: TRect;
    // Number of distinct colours among the pixels.
    function InkColors: Integer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTextIsDrawnAtItsBaseline;
    procedure TestTextHasTheFontColor;
    procedure TestTextWidthGrowsWithTheText;
    procedure TestTextWidthGrowsWithTheSize;
    procedure TestTextExtentMatchesTheInk;
    procedure TestWithoutAntiAliasingOnlyTwoColors;
    procedure TestUnicodeTextIsDrawn;
    procedure TestRotatedTextIsTallerThanWide;
  end;

implementation

procedure TTestFreeTypeFont.SetUp;

var
  lFile: String;

begin
  inherited SetUp;
  FImage := CreateSolidImage(200, 100, colWhite);
  FCanvas := TFPImageCanvas.Create(FImage);
  lFile := ExampleFile('DejaVuLGCSans.ttf');
  if not FileExists(lFile) then
    Fail('Font not found: ' + lFile + ' (run from packages/fcl-image)');
  try
    InitEngine;
    FFont := TFreeTypeFont.Create;
    FCanvas.Font := FFont;
    FFont.Name := lFile;
    FFont.Size := 20;
    FFont.FPColor := colBlack;
    FCanvas.TextWidth('x');
  except
    on E: Exception do
      Ignore('FreeType is not available: ' + E.Message);
  end;
end;


procedure TTestFreeTypeFont.TearDown;

begin
  FreeAndNil(FCanvas);
  FreeAndNil(FFont);
  FreeAndNil(FImage);
  inherited TearDown;
end;


function TTestFreeTypeFont.InkBounds: TRect;

var
  lX, lY: Integer;

begin
  Result := Rect(MaxInt, MaxInt, -1, -1);
  for lY := 0 to FImage.Height - 1 do
    for lX := 0 to FImage.Width - 1 do
      if not (FImage[lX, lY] = colWhite) then
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


function TTestFreeTypeFont.InkColors: Integer;

begin
  Result := CountColors(FImage);
end;


procedure TTestFreeTypeFont.TestTextIsDrawnAtItsBaseline;

var
  lInk: TRect;

begin
  FCanvas.TextOut(10, 50, 'Hxg');
  lInk := InkBounds;
  AssertTrue('The text paints pixels', lInk.Right >= 0);
  AssertTrue('The ink starts near x', (lInk.Left >= 9) and (lInk.Left <= 14));
  AssertTrue('Capitals rise above the baseline', lInk.Top < 50 - 10);
  AssertTrue('Capitals are less than the size above the baseline', lInk.Top >= 50 - 20);
  AssertTrue('A descender goes below the baseline', lInk.Bottom > 50);
  AssertTrue('but not by more than half the size', lInk.Bottom <= 50 + 10);
end;


procedure TTestFreeTypeFont.TestTextHasTheFontColor;

var
  lX, lY: Integer;
  lFound: Boolean;

begin
  FFont.FPColor := colBlue;
  FFont.AntiAliased := False;
  FCanvas.TextOut(10, 50, 'HHH');
  lFound := False;
  for lY := 0 to FImage.Height - 1 do
    for lX := 0 to FImage.Width - 1 do
      if not (FImage[lX, lY] = colWhite) then
        begin
        AssertColorsEqual('Every ink pixel has the font colour', colBlue, FImage[lX, lY]);
        lFound := True;
        end;
  AssertTrue('There is ink', lFound);
end;


procedure TTestFreeTypeFont.TestTextWidthGrowsWithTheText;

var
  lA, lB, lAB: Integer;

begin
  AssertTrue('W is wider than i', FCanvas.TextWidth('W') > FCanvas.TextWidth('i'));
  lA := FCanvas.TextWidth('H');
  lB := FCanvas.TextWidth('o');
  lAB := FCanvas.TextWidth('Ho');
  AssertTrue('The width of two letters is about the sum of their widths', Abs(lAB - (lA + lB)) <= 2);
  AssertTrue('The height of text is positive', FCanvas.TextHeight('Hg') > 0);
end;


procedure TTestFreeTypeFont.TestTextWidthGrowsWithTheSize;

var
  lSmall, lLarge: Integer;

begin
  FFont.Size := 10;
  lSmall := FCanvas.TextWidth('Hello');
  FFont.Size := 20;
  lLarge := FCanvas.TextWidth('Hello');
  AssertTrue('Twice the size is about twice as wide', Abs(lLarge - 2 * lSmall) <= 3);
end;


procedure TTestFreeTypeFont.TestTextExtentMatchesTheInk;

var
  lSize: TSize;
  lInk: TRect;

begin
  FCanvas.TextOut(10, 50, 'Hello');
  lSize := FCanvas.TextExtent('Hello');
  lInk := InkBounds;
  AssertTrue('The extent is at least as wide as the ink', lSize.cx >= lInk.Right - lInk.Left + 1);
  AssertTrue('The extent is not much wider than the ink', lSize.cx <= lInk.Right - lInk.Left + 1 + 6);
  AssertTrue('The extent is at least as high as the ink', lSize.cy >= lInk.Bottom - lInk.Top + 1);
end;


procedure TTestFreeTypeFont.TestWithoutAntiAliasingOnlyTwoColors;

begin
  FFont.AntiAliased := False;
  FCanvas.TextOut(10, 50, 'Hello world');
  AssertEquals('Without anti-aliasing there are only the background and the font colour', 2, InkColors);
  FImage.Free;
  FImage := CreateSolidImage(200, 100, colWhite);
  FCanvas.Image := FImage;
  FFont.AntiAliased := True;
  FCanvas.TextOut(10, 50, 'Hello world');
  AssertTrue('With anti-aliasing there are shades in between', InkColors > 2);
end;


procedure TTestFreeTypeFont.TestUnicodeTextIsDrawn;

var
  lText: UnicodeString;

begin
  lText := UTF8Decode('Ωß€');
  FCanvas.TextOut(10, 50, lText);
  AssertTrue('Non-ASCII text paints pixels', InkBounds.Right >= 0);
  AssertTrue('Non-ASCII text has a width', FCanvas.TextWidth(lText) > 0);
end;


procedure TTestFreeTypeFont.TestRotatedTextIsTallerThanWide;

var
  lInk: TRect;

begin
  FImage.Free;
  FImage := CreateSolidImage(120, 200, colWhite);
  FCanvas.Image := FImage;
  FFont.Angle := Pi / 2;
  FCanvas.TextOut(60, 150, 'Hello');
  lInk := InkBounds;
  AssertTrue('Rotated text paints pixels', lInk.Right >= 0);
  AssertTrue('Text turned a quarter is taller than wide', (lInk.Bottom - lInk.Top) > (lInk.Right - lInk.Left));
end;


initialization
  RegisterTest('ftfont', TTestFreeTypeFont);
end.
