{
    Tests for TFPCustomImage and TFPMemoryImage: size, pixel access, palette
    mode, Assign, extra information and resolution.
    See the file COPYING.FPC, included in this distribution, for details.
}
unit tcmemimage;

{$mode objfpc}{$H+}

interface

uses sysutils, classes, fpcunit, testregistry, fpimage, fpimgtests;

type
  TTestMemoryImage = class(TTestCase)
  private
    FImage: TFPMemoryImage;
    procedure ReadLeftOfImage;
    procedure ReadRightOfImage;
    procedure ReadBelowImage;
    procedure WriteAboveImage;
    procedure ReadPixelWithoutPalette;
    procedure WritePixelBeyondPalette;
    procedure WritePixelMinusOne;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreateSetsTheSize;
    procedure TestANewImageIsTransparentBlack;
    procedure TestColorsComeBackAtEveryCorner;
    procedure TestSixteenBitChannelsAreKept;
    procedure TestReadingLeftOfTheImageRaises;
    procedure TestReadingRightOfTheImageRaises;
    procedure TestReadingBelowTheImageRaises;
    procedure TestWritingAboveTheImageRaises;
    procedure TestGrowingKeepsThePixels;
    procedure TestGrowingClearsTheNewArea;
    procedure TestShrinkingKeepsThePixels;
    procedure TestResizingAPaletteImageKeepsTheIndices;
    procedure TestSizeZeroAndBack;
    procedure TestWidthAndHeightPropertiesResize;
    procedure TestNoPaletteByDefault;
    procedure TestSwitchingToAPaletteKeepsTheColors;
    procedure TestSwitchingToAPaletteBuildsIt;
    procedure TestSwitchingBackFromAPaletteKeepsTheColors;
    procedure TestPixelsNeedAPalette;
    procedure TestPixelsIndexThePalette;
    procedure TestPixelBeyondThePaletteRaises;
    procedure TestPixelMinusOneRaises;
    procedure TestAssignCopiesSizeAndColors;
    procedure TestAssignCopiesExtraAndResolution;
    procedure TestAssignCopiesAPaletteImage;
    procedure TestAssignKeepsColorsWithDuplicatePaletteEntries;
    procedure TestExtraValues;
    procedure TestExtraKeysAndValuesByIndex;
    procedure TestRemoveExtra;
    procedure TestResolutionUnitConversion;
    procedure TestResolutionWidthAndHeight;
  end;

implementation

procedure TTestMemoryImage.SetUp;

begin
  inherited SetUp;
  FImage := TFPMemoryImage.Create(4, 3);
end;


procedure TTestMemoryImage.TearDown;

begin
  FreeAndNil(FImage);
  inherited TearDown;
end;


procedure TTestMemoryImage.ReadLeftOfImage;

begin
  FImage.Colors[-1, 0];
end;


procedure TTestMemoryImage.ReadRightOfImage;

begin
  FImage.Colors[4, 0];
end;


procedure TTestMemoryImage.ReadBelowImage;

begin
  FImage.Colors[0, 3];
end;


procedure TTestMemoryImage.WriteAboveImage;

begin
  FImage.Colors[0, -1] := colRed;
end;


procedure TTestMemoryImage.ReadPixelWithoutPalette;

begin
  FImage.Pixels[0, 0] := 0;
end;


procedure TTestMemoryImage.WritePixelBeyondPalette;

begin
  FImage.Pixels[0, 0] := FImage.Palette.Count;
end;


procedure TTestMemoryImage.WritePixelMinusOne;

begin
  FImage.Pixels[0, 0] := -1;
end;


procedure TTestMemoryImage.TestCreateSetsTheSize;

begin
  AssertEquals('Width', 4, FImage.Width);
  AssertEquals('Height', 3, FImage.Height);
end;


procedure TTestMemoryImage.TestANewImageIsTransparentBlack;

var
  lX, lY: Integer;

begin
  for lY := 0 to 2 do
    for lX := 0 to 3 do
      AssertColorsEqual(Format('Pixel (%d,%d) of a new image', [lX, lY]), colTransparent, FImage[lX, lY]);
end;


procedure TTestMemoryImage.TestColorsComeBackAtEveryCorner;

begin
  FImage[0, 0] := colRed;
  FImage[3, 0] := colGreen;
  FImage[0, 2] := colBlue;
  FImage[3, 2] := colYellow;
  AssertColorsEqual('Top left', colRed, FImage[0, 0]);
  AssertColorsEqual('Top right', colGreen, FImage[3, 0]);
  AssertColorsEqual('Bottom left', colBlue, FImage[0, 2]);
  AssertColorsEqual('Bottom right', colYellow, FImage[3, 2]);
  AssertColorsEqual('Other pixels are untouched', colTransparent, FImage[1, 1]);
end;


procedure TTestMemoryImage.TestSixteenBitChannelsAreKept;

begin
  FImage[1, 1] := FPColor($1234, $5678, $9ABC, $DEF0);
  AssertColorsEqual('All 16 bits of every channel are kept', FPColor($1234, $5678, $9ABC, $DEF0), FImage[1, 1]);
end;


procedure TTestMemoryImage.TestReadingLeftOfTheImageRaises;

begin
  AssertRaises('x = -1 raises', FPImageException, @ReadLeftOfImage);
end;


procedure TTestMemoryImage.TestReadingRightOfTheImageRaises;

begin
  AssertRaises('x = Width raises', FPImageException, @ReadRightOfImage);
end;


procedure TTestMemoryImage.TestReadingBelowTheImageRaises;

begin
  AssertRaises('y = Height raises', FPImageException, @ReadBelowImage);
end;


procedure TTestMemoryImage.TestWritingAboveTheImageRaises;

begin
  AssertRaises('y = -1 raises', FPImageException, @WriteAboveImage);
end;


procedure TTestMemoryImage.TestGrowingKeepsThePixels;

var
  lSource: TFPMemoryImage;
  lX, lY: Integer;

begin
  lSource := CreateGradientImage(4, 3);
  try
    FImage.Assign(lSource);
    FImage.SetSize(7, 5);
    AssertEquals('New width', 7, FImage.Width);
    AssertEquals('New height', 5, FImage.Height);
    for lY := 0 to 2 do
      for lX := 0 to 3 do
        AssertColorsEqual(Format('Pixel (%d,%d) survives growing', [lX, lY]), lSource[lX, lY], FImage[lX, lY]);
  finally
    lSource.Free;
  end;
end;


procedure TTestMemoryImage.TestGrowingClearsTheNewArea;

var
  lSource: TFPMemoryImage;
  lX, lY: Integer;

begin
  lSource := CreateSolidImage(4, 3, colRed);
  try
    FImage.Assign(lSource);
  finally
    lSource.Free;
  end;
  FImage.SetSize(6, 5);
  for lY := 0 to 4 do
    for lX := 0 to 5 do
      if (lX >= 4) or (lY >= 3) then
        AssertColorsEqual(Format('New pixel (%d,%d) is transparent black', [lX, lY]), colTransparent, FImage[lX, lY]);
end;


procedure TTestMemoryImage.TestShrinkingKeepsThePixels;

var
  lSource: TFPMemoryImage;
  lX, lY: Integer;

begin
  lSource := CreateGradientImage(9, 7);
  try
    FImage.Assign(lSource);
    FImage.SetSize(5, 4);
    for lY := 0 to 3 do
      for lX := 0 to 4 do
        AssertColorsEqual(Format('Pixel (%d,%d) survives shrinking', [lX, lY]), lSource[lX, lY], FImage[lX, lY]);
  finally
    lSource.Free;
  end;
end;


procedure TTestMemoryImage.TestResizingAPaletteImageKeepsTheIndices;

var
  lX, lY: Integer;

begin
  FImage.UsePalette := True;
  FImage.Palette.Add(colRed);
  FImage.Palette.Add(colGreen);
  FImage.Palette.Add(colBlue);
  for lY := 0 to 2 do
    for lX := 0 to 3 do
      FImage.Pixels[lX, lY] := (lX + lY) mod 3;
  FImage.SetSize(6, 4);
  for lY := 0 to 2 do
    for lX := 0 to 3 do
      AssertEquals(Format('Index at (%d,%d) survives resizing', [lX, lY]), (lX + lY) mod 3, FImage.Pixels[lX, lY]);
end;


procedure TTestMemoryImage.TestSizeZeroAndBack;

begin
  FImage.SetSize(0, 0);
  AssertEquals('Width 0', 0, FImage.Width);
  AssertEquals('Height 0', 0, FImage.Height);
  FImage.SetSize(2, 2);
  FImage[1, 1] := colRed;
  AssertColorsEqual('An image resized from nothing works', colRed, FImage[1, 1]);
  AssertColorsEqual('and starts transparent', colTransparent, FImage[0, 0]);
end;


procedure TTestMemoryImage.TestWidthAndHeightPropertiesResize;

begin
  FImage[0, 0] := colRed;
  FImage.Width := 10;
  FImage.Height := 8;
  AssertEquals('Width set', 10, FImage.Width);
  AssertEquals('Height set', 8, FImage.Height);
  FImage[9, 7] := colBlue;
  AssertColorsEqual('Last pixel is writable', colBlue, FImage[9, 7]);
  AssertColorsEqual('First pixel survives', colRed, FImage[0, 0]);
end;


procedure TTestMemoryImage.TestNoPaletteByDefault;

begin
  AssertFalse('A memory image has no palette by default', FImage.UsePalette);
  AssertNull('No palette object', FImage.Palette);
end;


procedure TTestMemoryImage.TestSwitchingToAPaletteKeepsTheColors;

var
  lSource: TFPMemoryImage;

begin
  lSource := CreateFewColorsImage(4, 3, 5);
  try
    FImage.Assign(lSource);
    FImage.UsePalette := True;
    AssertTrue('The image has a palette', FImage.UsePalette);
    AssertImagesEqual('Colours survive switching to a palette', lSource, FImage);
  finally
    lSource.Free;
  end;
end;


procedure TTestMemoryImage.TestSwitchingToAPaletteBuildsIt;

var
  lSource: TFPMemoryImage;

begin
  lSource := CreateFewColorsImage(4, 3, 5);
  try
    FImage.Assign(lSource);
    FImage.UsePalette := True;
    AssertEquals('The palette has each colour once', 5, FImage.Palette.Count);
    AssertColorsEqual('The pixel index points at its colour', lSource[2, 1],
      FImage.Palette[FImage.Pixels[2, 1]]);
  finally
    lSource.Free;
  end;
end;


procedure TTestMemoryImage.TestSwitchingBackFromAPaletteKeepsTheColors;

var
  lSource: TFPMemoryImage;

begin
  lSource := CreateFewColorsImage(4, 3, 5);
  try
    FImage.Assign(lSource);
    FImage.UsePalette := True;
    FImage.UsePalette := False;
    AssertFalse('The palette is gone', FImage.UsePalette);
    AssertImagesEqual('Colours survive switching the palette off again', lSource, FImage);
  finally
    lSource.Free;
  end;
end;


procedure TTestMemoryImage.TestPixelsNeedAPalette;

begin
  AssertRaises('Pixels without a palette raises', FPImageException, @ReadPixelWithoutPalette);
end;


procedure TTestMemoryImage.TestPixelsIndexThePalette;

var
  lBlue: Integer;

begin
  FImage.UsePalette := True;
  AssertEquals('Switching to a palette adds the colour of the existing pixels', 1, FImage.Palette.Count);
  AssertColorsEqual('That colour is transparent black', colTransparent, FImage.Palette[0]);
  FImage.Palette.Add(colRed);
  lBlue := FImage.Palette.Add(colBlue);
  FImage.Pixels[2, 1] := lBlue;
  AssertEquals('The index comes back', lBlue, FImage.Pixels[2, 1]);
  AssertColorsEqual('The colour is the palette entry', colBlue, FImage[2, 1]);
end;


procedure TTestMemoryImage.TestPixelBeyondThePaletteRaises;

begin
  FImage.UsePalette := True;
  FImage.Palette.Add(colRed);
  AssertRaises('An index past the palette raises', FPImageException, @WritePixelBeyondPalette);
end;


procedure TTestMemoryImage.TestPixelMinusOneRaises;

begin
  FImage.UsePalette := True;
  FImage.Palette.Add(colRed);
  AssertRaises('Index -1 raises', FPImageException, @WritePixelMinusOne);
end;


procedure TTestMemoryImage.TestAssignCopiesSizeAndColors;

var
  lSource: TFPMemoryImage;

begin
  lSource := CreateAlphaImage(5, 6);
  try
    FImage.Assign(lSource);
    AssertImagesEqual('Assign copies all pixels', lSource, FImage);
  finally
    lSource.Free;
  end;
end;


procedure TTestMemoryImage.TestAssignCopiesExtraAndResolution;

var
  lSource: TFPMemoryImage;

begin
  lSource := TFPMemoryImage.Create(1, 1);
  try
    lSource.Extra['author'] := 'me';
    lSource.ResolutionUnit := ruPixelsPerInch;
    lSource.ResolutionX := 300;
    lSource.ResolutionY := 150;
    FImage.Assign(lSource);
    AssertEquals('Extra information is copied', 'me', FImage.Extra['author']);
    AssertTrue('The resolution unit is copied', FImage.ResolutionUnit = ruPixelsPerInch);
    AssertEquals('Horizontal resolution is copied', 300, FImage.ResolutionX, 0.001);
    AssertEquals('Vertical resolution is copied', 150, FImage.ResolutionY, 0.001);
  finally
    lSource.Free;
  end;
end;


procedure TTestMemoryImage.TestAssignCopiesAPaletteImage;

var
  lSource: TFPMemoryImage;
  lRed, lGreen: Integer;

begin
  lSource := TFPMemoryImage.Create(2, 1);
  try
    lSource.UsePalette := True;
    lRed := lSource.Palette.Add(colRed);
    lGreen := lSource.Palette.Add(colGreen);
    lSource.Pixels[0, 0] := lGreen;
    lSource.Pixels[1, 0] := lRed;
    FImage.Assign(lSource);
    AssertTrue('The copy uses a palette', FImage.UsePalette);
    AssertEquals('The copy has the same indices', lGreen, FImage.Pixels[0, 0]);
    AssertColorsEqual('Pixel 0 is green', colGreen, FImage[0, 0]);
    AssertColorsEqual('Pixel 1 is red', colRed, FImage[1, 0]);
  finally
    lSource.Free;
  end;
end;


procedure TTestMemoryImage.TestAssignKeepsColorsWithDuplicatePaletteEntries;

var
  lSource: TFPMemoryImage;
  lRed2, lBlue: Integer;

begin
  lSource := TFPMemoryImage.Create(2, 1);
  try
    lSource.UsePalette := True;
    lSource.Palette.Add(colRed);
    lRed2 := lSource.Palette.Add(colRed);
    lBlue := lSource.Palette.Add(colBlue);
    lSource.Pixels[0, 0] := lBlue;
    lSource.Pixels[1, 0] := lRed2;
    FImage.Assign(lSource);
    AssertColorsEqual('Pixel 0 keeps its colour', colBlue, FImage[0, 0]);
    AssertColorsEqual('Pixel 1 keeps its colour', colRed, FImage[1, 0]);
  finally
    lSource.Free;
  end;
end;


procedure TTestMemoryImage.TestExtraValues;

begin
  AssertEquals('No extra information at first', 0, FImage.ExtraCount);
  FImage.Extra['key'] := 'value';
  AssertEquals('One entry', 1, FImage.ExtraCount);
  AssertEquals('The value comes back', 'value', FImage.Extra['key']);
  AssertEquals('A missing key gives an empty string', '', FImage.Extra['missing']);
  FImage.Extra['key'] := 'other';
  AssertEquals('Setting again replaces', 'other', FImage.Extra['key']);
  AssertEquals('Still one entry', 1, FImage.ExtraCount);
end;


procedure TTestMemoryImage.TestExtraKeysAndValuesByIndex;

begin
  FImage.Extra['a'] := '1';
  FImage.Extra['b'] := '2';
  AssertEquals('Key by index', 'b', FImage.ExtraKey[1]);
  AssertEquals('Value by index', '2', FImage.ExtraValue[1]);
  FImage.ExtraValue[1] := '3';
  AssertEquals('Value set by index', '3', FImage.Extra['b']);
  FImage.ExtraKey[1] := 'c';
  AssertEquals('Key set by index keeps the value', '3', FImage.Extra['c']);
end;


procedure TTestMemoryImage.TestRemoveExtra;

begin
  FImage.Extra['a'] := '1';
  FImage.Extra['b'] := '2';
  FImage.RemoveExtra('a');
  AssertEquals('One entry left', 1, FImage.ExtraCount);
  AssertEquals('The other entry stays', '2', FImage.Extra['b']);
  FImage.RemoveExtra('missing');
  AssertEquals('Removing a missing key does nothing', 1, FImage.ExtraCount);
end;


procedure TTestMemoryImage.TestResolutionUnitConversion;

begin
  FImage.ResolutionUnit := ruPixelsPerInch;
  FImage.ResolutionX := 254;
  FImage.ResolutionY := 127;
  FImage.ResolutionUnit := ruPixelsPerCentimeter;
  AssertEquals('254 dpi is 100 per cm', 100, FImage.ResolutionX, 0.01);
  AssertEquals('127 dpi is 50 per cm', 50, FImage.ResolutionY, 0.01);
  FImage.ResolutionUnit := ruPixelsPerInch;
  AssertEquals('and back to inch', 254, FImage.ResolutionX, 0.01);
end;


procedure TTestMemoryImage.TestResolutionWidthAndHeight;

begin
  AssertEquals('Without a unit the width is in pixels', 4, FImage.ResolutionWidth, 0.001);
  FImage.ResolutionUnit := ruPixelsPerInch;
  FImage.ResolutionX := 2;
  FImage.ResolutionY := 3;
  AssertEquals('Physical width is pixels divided by resolution', 2, FImage.ResolutionWidth, 0.001);
  AssertEquals('Physical height is pixels divided by resolution', 1, FImage.ResolutionHeight, 0.001);
end;


initialization
  RegisterTest('memimage', TTestMemoryImage);
end.
