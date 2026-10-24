{
    Tests for TFPPalette and the standard palettes of fpimage.
    See the file COPYING.FPC, included in this distribution, for details.
}
unit tcpalette;

{$mode objfpc}{$H+}

interface

uses sysutils, classes, fpcunit, testregistry, fpimage, fpimgtests;

type
  TTestPalette = class(TTestCase)
  private
    FPalette: TFPPalette;
    procedure ReadBeyondCount;
    procedure ReadNegative;
    procedure WriteBeyondCount;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestANewPaletteIsEmpty;
    procedure TestAddReturnsTheIndex;
    procedure TestAddGrowsBeyondTheCapacity;
    procedure TestSettingTheColorAtCountAppends;
    procedure TestReadingBeyondCountRaises;
    procedure TestReadingANegativeIndexRaises;
    procedure TestWritingBeyondCountRaises;
    procedure TestIndexOfFindsAColor;
    procedure TestIndexOfAppendsAMissingColor;
    procedure TestIndexOfComparesAlpha;
    procedure TestGrowingCountFillsWithBlack;
    procedure TestShrinkingCountKeepsTheFirstEntries;
    procedure TestClearEmpties;
    procedure TestCopyKeepsOrderAndDuplicates;
    procedure TestCopyReplacesTheOldEntries;
    procedure TestMergeAddsOnlyMissingColors;
    procedure TestBuildCollectsTheColorsOfAnImage;
    procedure TestCapacityIsNeverBelowCount;
  end;

  TTestStandardPalettes = class(TTestCase)
  private
    // Fails unless every entry of the palette is different.
    procedure AssertDistinct(const aName: String; aPalette: TFPPalette);
    // Fails unless every entry of the palette is opaque.
    procedure AssertOpaque(const aName: String; aPalette: TFPPalette);
  published
    procedure TestBlackAndWhite;
    procedure TestWebSafeHas216Colors;
    procedure TestWebSafeColorsAreDistinct;
    procedure TestWebSafeUsesSixLevelsPerChannel;
    procedure TestWebSafeContainsBlackAndWhite;
    procedure TestGrayScale;
    procedure TestVGAHas16DistinctColors;
    procedure TestVGAHoldsTheSixteenHtmlColors;
    procedure TestVGAStartsBlackAndEndsWhite;
  end;

implementation

{ TTestPalette }

procedure TTestPalette.SetUp;

begin
  inherited SetUp;
  FPalette := TFPPalette.Create(4);
end;


procedure TTestPalette.TearDown;

begin
  FreeAndNil(FPalette);
  inherited TearDown;
end;


procedure TTestPalette.ReadBeyondCount;

begin
  FPalette.Add(colRed);
  FPalette.Color[1];
end;


procedure TTestPalette.ReadNegative;

begin
  FPalette.Add(colRed);
  FPalette.Color[-1];
end;


procedure TTestPalette.WriteBeyondCount;

begin
  FPalette.Color[1] := colRed;
end;


procedure TTestPalette.TestANewPaletteIsEmpty;

begin
  AssertEquals('A new palette has no entries', 0, FPalette.Count);
  AssertEquals('A new palette has the capacity asked for', 4, FPalette.Capacity);
end;


procedure TTestPalette.TestAddReturnsTheIndex;

begin
  AssertEquals('The first entry is at 0', 0, FPalette.Add(colRed));
  AssertEquals('The second entry is at 1', 1, FPalette.Add(colBlue));
  AssertEquals('Two entries are counted', 2, FPalette.Count);
  AssertColorsEqual('Entry 0', colRed, FPalette[0]);
  AssertColorsEqual('Entry 1', colBlue, FPalette[1]);
end;


procedure TTestPalette.TestAddGrowsBeyondTheCapacity;

var
  I: Integer;

begin
  for I := 0 to 999 do
    FPalette.Add(FPColor(I, I * 3, I * 7));
  AssertEquals('All entries are counted', 1000, FPalette.Count);
  for I := 0 to 999 do
    AssertColorsEqual(Format('Entry %d survives the growth', [I]), FPColor(I, I * 3, I * 7), FPalette[I]);
end;


procedure TTestPalette.TestSettingTheColorAtCountAppends;

begin
  FPalette.Color[0] := colGreen;
  AssertEquals('Writing at Count appends', 1, FPalette.Count);
  AssertColorsEqual('The appended entry', colGreen, FPalette[0]);
end;


procedure TTestPalette.TestReadingBeyondCountRaises;

begin
  AssertRaises('Reading at Count raises', FPImageException, @ReadBeyondCount);
end;


procedure TTestPalette.TestReadingANegativeIndexRaises;

begin
  AssertRaises('Reading at -1 raises', FPImageException, @ReadNegative);
end;


procedure TTestPalette.TestWritingBeyondCountRaises;

begin
  AssertRaises('Writing past Count raises', FPImageException, @WriteBeyondCount);
end;


procedure TTestPalette.TestIndexOfFindsAColor;

begin
  FPalette.Add(colRed);
  FPalette.Add(colGreen);
  FPalette.Add(colBlue);
  AssertEquals('Green is found at its index', 1, FPalette.IndexOf(colGreen));
  AssertEquals('Finding does not add', 3, FPalette.Count);
end;


procedure TTestPalette.TestIndexOfAppendsAMissingColor;

begin
  FPalette.Add(colRed);
  AssertEquals('A missing colour is appended', 1, FPalette.IndexOf(colYellow));
  AssertEquals('The palette grew by one', 2, FPalette.Count);
  AssertColorsEqual('The appended colour', colYellow, FPalette[1]);
end;


procedure TTestPalette.TestIndexOfComparesAlpha;

begin
  FPalette.Add(FPColor($FFFF, 0, 0, alphaOpaque));
  AssertEquals('A colour differing only in alpha is a new entry', 1,
    FPalette.IndexOf(FPColor($FFFF, 0, 0, $8000)));
end;


procedure TTestPalette.TestGrowingCountFillsWithBlack;

var
  I: Integer;

begin
  FPalette.Add(colRed);
  FPalette.Count := 20;
  AssertEquals('The count is set', 20, FPalette.Count);
  AssertColorsEqual('The first entry is kept', colRed, FPalette[0]);
  for I := 1 to 19 do
    AssertColorsEqual(Format('New entry %d is opaque black', [I]), colBlack, FPalette[I]);
end;


procedure TTestPalette.TestShrinkingCountKeepsTheFirstEntries;

begin
  FPalette.Add(colRed);
  FPalette.Add(colGreen);
  FPalette.Add(colBlue);
  FPalette.Count := 2;
  AssertEquals('The count is set', 2, FPalette.Count);
  AssertColorsEqual('Entry 0 is kept', colRed, FPalette[0]);
  AssertColorsEqual('Entry 1 is kept', colGreen, FPalette[1]);
end;


procedure TTestPalette.TestClearEmpties;

begin
  FPalette.Add(colRed);
  FPalette.Clear;
  AssertEquals('Clear leaves no entries', 0, FPalette.Count);
end;


procedure TTestPalette.TestCopyKeepsOrderAndDuplicates;

var
  lSource: TFPPalette;

begin
  lSource := TFPPalette.Create(0);
  try
    lSource.Add(colBlue);
    lSource.Add(colRed);
    lSource.Add(colBlue);
    FPalette.Copy(lSource);
    AssertEquals('All entries are copied, duplicates too', 3, FPalette.Count);
    AssertColorsEqual('Entry 0', colBlue, FPalette[0]);
    AssertColorsEqual('Entry 1', colRed, FPalette[1]);
    AssertColorsEqual('Entry 2', colBlue, FPalette[2]);
  finally
    lSource.Free;
  end;
end;


procedure TTestPalette.TestCopyReplacesTheOldEntries;

var
  lSource: TFPPalette;

begin
  FPalette.Add(colYellow);
  FPalette.Add(colYellow);
  lSource := TFPPalette.Create(0);
  try
    lSource.Add(colGreen);
    FPalette.Copy(lSource);
    AssertEquals('Copy replaces, it does not append', 1, FPalette.Count);
    AssertColorsEqual('Entry 0', colGreen, FPalette[0]);
  finally
    lSource.Free;
  end;
end;


procedure TTestPalette.TestMergeAddsOnlyMissingColors;

var
  lSource: TFPPalette;

begin
  FPalette.Add(colRed);
  lSource := TFPPalette.Create(0);
  try
    lSource.Add(colGreen);
    lSource.Add(colRed);
    FPalette.Merge(lSource);
    AssertEquals('Only the missing colour is added', 2, FPalette.Count);
    AssertColorsEqual('The existing entry stays first', colRed, FPalette[0]);
    AssertColorsEqual('The missing entry follows', colGreen, FPalette[1]);
  finally
    lSource.Free;
  end;
end;


procedure TTestPalette.TestBuildCollectsTheColorsOfAnImage;

var
  lImage: TFPMemoryImage;
  I: Integer;

begin
  lImage := CreateFewColorsImage(10, 10, 7);
  try
    FPalette.Add(colYellow);
    FPalette.Build(lImage);
    AssertEquals('Build holds each colour of the image once, and nothing else', 7, FPalette.Count);
    for I := 0 to 6 do
      AssertTrue(Format('Colour of pixel %d is in the palette', [I]),
        FPalette.IndexOf(lImage.Colors[I, 0]) < 7);
  finally
    lImage.Free;
  end;
end;


procedure TTestPalette.TestCapacityIsNeverBelowCount;

begin
  FPalette.Add(colRed);
  FPalette.Add(colGreen);
  FPalette.Add(colBlue);
  FPalette.Capacity := 1;
  AssertEquals('Capacity is clamped to Count', 3, FPalette.Capacity);
  AssertColorsEqual('Entries survive', colBlue, FPalette[2]);
end;


{ TTestStandardPalettes }

procedure TTestStandardPalettes.AssertDistinct(const aName: String; aPalette: TFPPalette);

var
  I, J: Integer;

begin
  for I := 0 to aPalette.Count - 1 do
    for J := I + 1 to aPalette.Count - 1 do
      if aPalette[I] = aPalette[J] then
        Fail(Format('%s: entries %d and %d are the same colour %s',
          [aName, I, J, ColorToStr(aPalette[I])]));
end;


procedure TTestStandardPalettes.AssertOpaque(const aName: String; aPalette: TFPPalette);

var
  I: Integer;

begin
  for I := 0 to aPalette.Count - 1 do
    AssertEquals(Format('%s: entry %d is opaque', [aName, I]), alphaOpaque, aPalette[I].Alpha);
end;


procedure TTestStandardPalettes.TestBlackAndWhite;

var
  lPalette: TFPPalette;

begin
  lPalette := CreateBlackAndWhitePalette;
  try
    AssertEquals('Two entries', 2, lPalette.Count);
    AssertColorsEqual('Entry 0 is white', colWhite, lPalette[0]);
    AssertColorsEqual('Entry 1 is black', colBlack, lPalette[1]);
  finally
    lPalette.Free;
  end;
end;


procedure TTestStandardPalettes.TestWebSafeHas216Colors;

var
  lPalette: TFPPalette;

begin
  lPalette := CreateWebSafePalette;
  try
    AssertEquals('The web-safe palette has 6x6x6 entries', 216, lPalette.Count);
  finally
    lPalette.Free;
  end;
end;


procedure TTestStandardPalettes.TestWebSafeColorsAreDistinct;

var
  lPalette: TFPPalette;

begin
  lPalette := CreateWebSafePalette;
  try
    AssertEquals('The web-safe palette has 216 entries', 216, lPalette.Count);
    AssertDistinct('Web-safe palette', lPalette);
    AssertOpaque('Web-safe palette', lPalette);
  finally
    lPalette.Free;
  end;
end;


procedure TTestStandardPalettes.TestWebSafeUsesSixLevelsPerChannel;

var
  lPalette: TFPPalette;
  I: Integer;

begin
  lPalette := CreateWebSafePalette;
  try
    for I := 0 to lPalette.Count - 1 do
      begin
      AssertEquals(Format('Red of entry %d is a multiple of $3333', [I]), 0, lPalette[I].Red mod $3333);
      AssertEquals(Format('Green of entry %d is a multiple of $3333', [I]), 0, lPalette[I].Green mod $3333);
      AssertEquals(Format('Blue of entry %d is a multiple of $3333', [I]), 0, lPalette[I].Blue mod $3333);
      end;
  finally
    lPalette.Free;
  end;
end;


procedure TTestStandardPalettes.TestWebSafeContainsBlackAndWhite;

var
  lPalette: TFPPalette;
  lCount: Integer;

begin
  lPalette := CreateWebSafePalette;
  try
    lCount := lPalette.Count;
    AssertTrue('White is in the web-safe palette', lPalette.IndexOf(colWhite) < lCount);
    AssertTrue('Black is in the web-safe palette', lPalette.IndexOf(colBlack) < lCount);
    AssertTrue('Pure red is in the web-safe palette', lPalette.IndexOf(colRed) < lCount);
  finally
    lPalette.Free;
  end;
end;


procedure TTestStandardPalettes.TestGrayScale;

var
  lPalette: TFPPalette;
  I: Integer;

begin
  lPalette := CreateGrayScalePalette;
  try
    AssertEquals('256 entries', 256, lPalette.Count);
    for I := 0 to 255 do
      AssertColorsEqual(Format('Entry %d is gray level %d', [I, I]), RGB8(I, I, I), lPalette[I]);
  finally
    lPalette.Free;
  end;
end;


procedure TTestStandardPalettes.TestVGAHas16DistinctColors;

var
  lPalette: TFPPalette;

begin
  lPalette := CreateVGAPalette;
  try
    AssertEquals('16 entries', 16, lPalette.Count);
    AssertDistinct('VGA palette', lPalette);
    AssertOpaque('VGA palette', lPalette);
  finally
    lPalette.Free;
  end;
end;


procedure TTestStandardPalettes.TestVGAHoldsTheSixteenHtmlColors;

const
  cNames: array[0..15] of String = ('white', 'silver', 'gray', 'black', 'red',
    'maroon', 'yellow', 'olive', 'lime', 'green', 'aqua', 'teal', 'blue',
    'navy', 'fuchsia', 'purple');

var
  lPalette: TFPPalette;
  lHtml: TFPColor;
  I, J: Integer;
  lFound: Boolean;

begin
  lPalette := CreateVGAPalette;
  try
    for I := 0 to High(cNames) do
      begin
      lHtml := HtmlToFPColor(cNames[I]);
      lFound := False;
      for J := 0 to lPalette.Count - 1 do
        if ((lPalette[J].Red shr 8) = (lHtml.Red shr 8))
          and ((lPalette[J].Green shr 8) = (lHtml.Green shr 8))
          and ((lPalette[J].Blue shr 8) = (lHtml.Blue shr 8)) then
          lFound := True;
      AssertTrue('The VGA palette has the colour ' + cNames[I], lFound);
      end;
  finally
    lPalette.Free;
  end;
end;


procedure TTestStandardPalettes.TestVGAStartsBlackAndEndsWhite;

var
  lPalette: TFPPalette;

begin
  lPalette := CreateVGAPalette;
  try
    AssertColorsEqual('Entry 0 is black', colBlack, lPalette[0]);
    AssertColorsEqual('Entry 15 is white', colWhite, lPalette[15]);
  finally
    lPalette.Free;
  end;
end;


initialization
  RegisterTests('palette', [TTestPalette, TTestStandardPalettes]);
end.
