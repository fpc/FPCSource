{
    Tests for the colour quantizers of fpquantizer, the ditherers of
    fpditherer and the colour hash table of fpcolhash.
    See the file COPYING.FPC, included in this distribution, for details.
}
unit tcquantize;

{$mode objfpc}{$H+}

interface

uses sysutils, classes, fpcunit, testregistry, fpimage, fpcolhash, fpquantizer, fpditherer, fpimgtests;

type
  TTestQuantizer = class(TTestCase)
  private
    FQuantizer: TFPColorQuantizer;
    FStages: array of TFPImgProgressStage;
    FPercents: array of Byte;
    FCancel: Boolean;
    // Records a progress event and cancels if FCancel is set.
    procedure DoProgress(Sender: TObject; Stage: TFPImgProgressStage; PercentDone: Byte;
      const Msg: AnsiString; var Continue: Boolean);
    // Reads the image at index -1.
    procedure ReadImageMinusOne;
    // Reads the image at index Count.
    procedure ReadImagePastTheEnd;
    // Sets the colour number to 1.
    procedure SetOneColor;
    // Quantizes a gradient image with a new quantizer and frees everything.
    procedure QuantizeGradient;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    // A new quantizer of the class under test.
    function CreateQuantizer: TFPColorQuantizer; virtual;
    // The largest channel difference between an image colour and its palette entry when exact colours are expected.
    function Tolerance: Word; virtual;
    // Fails unless aPalette has a colour within Tolerance of aColor, alpha compared exactly.
    procedure CheckInPalette(const aMessage: String; aPalette: TFPPalette; const aColor: TFPColor);
  published
    procedure TestPaletteHasAtMostTheColorNumber;
    procedure TestFewColorsAreReproduced;
    procedure TestColorsDifferingInTheLowestBit;
    procedure TestOpaqueImageGivesOpaquePalette;
    procedure TestAlphaOfThePalette;
    procedure TestEmptyImage;
    procedure TestNoImages;
    procedure TestSeveralImages;
    procedure TestProgressStartsAndEnds;
    procedure TestCancelAtTheStartGivesNoPalette;
    procedure TestColorNumberBelowTwoRaises;
    procedure TestInvalidImageIndexRaises;
    procedure TestNoLeak;
  end;

  TTestOctreeQuantizer = class(TTestQuantizer)
  protected
    // A TFPOctreeQuantizer.
    function CreateQuantizer: TFPColorQuantizer; override;
  end;

  TTestMedianCutSlow = class(TTestQuantizer)
  protected
    // A TFPMedianCutQuantizer in mcSlow mode.
    function CreateQuantizer: TFPColorQuantizer; override;
  end;

  TTestMedianCutNormal = class(TTestQuantizer)
  protected
    // A TFPMedianCutQuantizer in mcNormal mode.
    function CreateQuantizer: TFPColorQuantizer; override;
    // The tolerance of green and blue kept in 6 bits.
    function Tolerance: Word; override;
  end;

  TTestMedianCutFast = class(TTestQuantizer)
  protected
    // A TFPMedianCutQuantizer in mcFast mode.
    function CreateQuantizer: TFPColorQuantizer; override;
    // The tolerance of red, green and blue kept in 5 bits.
    function Tolerance: Word; override;
  end;

  TTestDitherer = class(TTestCase)
  private
    FPalette: TFPPalette;
    FDitherer: TFPBaseDitherer;
    FSource: TFPMemoryImage;
    FDest: TFPMemoryImage;
    // Dithers into itself.
    procedure DitherIntoItself;
    // Dithers with an empty palette.
    procedure DitherWithEmptyPalette;
    // Dithers a gradient with a new Floyd-Steinberg ditherer and frees everything.
    procedure DitherGradientFloydSteinberg;
    // Dithers a gradient with a new base ditherer and frees everything.
    procedure DitherGradientBase;
    // Fails unless every pixel of FDest is a colour of FPalette.
    procedure CheckOnlyPaletteColors(const aMessage: String);
    // Fails unless FDest has the palette FPalette in the same order.
    procedure CheckDestPalette(const aMessage: String);
    // Dithers FSource into FDest with a new ditherer, Floyd-Steinberg if aFloydSteinberg.
    procedure DitherWith(aFloydSteinberg: Boolean);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSameSourceAndDestinationRaises;
    procedure TestEmptyPaletteRaises;
    procedure TestBaseUsesOnlyPaletteColors;
    procedure TestBasePicksTheNearestColor;
    procedure TestFloydSteinbergUsesOnlyPaletteColors;
    procedure TestFloydSteinbergKeepsAPaletteColorOnEveryRow;
    procedure TestFloydSteinbergMixesEveryRow;
    procedure TestFloydSteinbergSingleRow;
    procedure TestDuplicatePaletteEntriesKeepTheIndexOrder;
    procedure TestFloydSteinbergDuplicatePaletteEntries;
    procedure TestSortedPaletteFindsTheNearestColor;
    procedure TestDitherLeavesTheCallerPaletteUnchanged;
    procedure TestSortPaletteLeavesTheCallerPaletteUnchanged;
    procedure TestEmptySourceGivesEmptyDestination;
    procedure TestNoLeak;
  end;

  TTestColorHash = class(TTestCase)
  private
    FTable: TFPColorHashTable;
    // Inserts one colour twice into a new table and frees it.
    procedure InsertTwice;
    // Inserts allocated data into a new table and frees it.
    procedure InsertPointerData;
    // Inserts allocated data for one colour twice into a new table and frees it.
    procedure InsertPointerTwice;
    // Adds, reads the array of and frees a new table.
    procedure FillAndList;
    // Calls GetArray on FTable and frees the result.
    procedure GetArrayOfTable;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestInsertAndGet;
    procedure TestGetOfAMissingColorIsNil;
    procedure TestAddAccumulates;
    procedure TestCountCountsDistinctColors;
    procedure TestManyColorsAreKeptApart;
    procedure TestAlphaIsPartOfTheKey;
    procedure TestInsertReplacesTheValue;
    procedure TestInsertReplaceDoesNotLeak;
    procedure TestInsertPointerReplaceDoesNotLeak;
    procedure TestTableFreesPointerData;
    procedure TestClearEmptiesTheTable;
    procedure TestGetArrayListsEveryColor;
    procedure TestGetArrayWithPointerDataRaises;
    procedure TestNoLeak;
    procedure TestPackedRoundTrip;
  end;

implementation

// A copy of aPalette.
function CopyPalette(aPalette: TFPPalette): TFPPalette;

var
  I: Integer;

begin
  Result := TFPPalette.Create(0);
  for I := 0 to aPalette.Count - 1 do
    Result.Add(aPalette[I]);
end;


// A palette with the given colours in that order.
function MakePalette(const aColors: array of TFPColor): TFPPalette;

var
  I: Integer;

begin
  Result := TFPPalette.Create(0);
  for I := Low(aColors) to High(aColors) do
    Result.Add(aColors[I]);
end;


// The Manhattan distance of the 8-bit channels of two colours, alpha ignored.
function Distance(const aColor1, aColor2: TFPColor): Integer;

begin
  Result := Abs((aColor1.Red shr 8) - (aColor2.Red shr 8))
    + Abs((aColor1.Green shr 8) - (aColor2.Green shr 8))
    + Abs((aColor1.Blue shr 8) - (aColor2.Blue shr 8));
end;


// The smallest distance from aColor to a colour of aPalette.
function NearestDistance(aPalette: TFPPalette; const aColor: TFPColor): Integer;

var
  I: Integer;

begin
  Result := MaxInt;
  for I := 0 to aPalette.Count - 1 do
    if Distance(aColor, aPalette[I]) < Result then
      Result := Distance(aColor, aPalette[I]);
end;


// True if aPalette has aColor exactly.
function PaletteHas(aPalette: TFPPalette; const aColor: TFPColor): Boolean;

var
  I: Integer;

begin
  Result := False;
  for I := 0 to aPalette.Count - 1 do
    if aPalette[I] = aColor then
      Exit(True);
end;


{ TTestQuantizer }

procedure TTestQuantizer.SetUp;

begin
  inherited SetUp;
  FQuantizer := CreateQuantizer;
  FQuantizer.OnProgress := @DoProgress;
  SetLength(FStages, 0);
  SetLength(FPercents, 0);
  FCancel := False;
end;


procedure TTestQuantizer.TearDown;

begin
  FreeAndNil(FQuantizer);
  inherited TearDown;
end;


function TTestQuantizer.CreateQuantizer: TFPColorQuantizer;

begin
  Result := TFPOctreeQuantizer.Create;
end;


function TTestQuantizer.Tolerance: Word;

begin
  Result := 0;
end;


procedure TTestQuantizer.DoProgress(Sender: TObject; Stage: TFPImgProgressStage; PercentDone: Byte;
  const Msg: AnsiString; var Continue: Boolean);

begin
  SetLength(FStages, Length(FStages) + 1);
  FStages[High(FStages)] := Stage;
  SetLength(FPercents, Length(FPercents) + 1);
  FPercents[High(FPercents)] := PercentDone;
  if FCancel then
    Continue := False;
end;


procedure TTestQuantizer.ReadImageMinusOne;

begin
  FQuantizer.Images[-1];
end;


procedure TTestQuantizer.ReadImagePastTheEnd;

begin
  FQuantizer.Images[FQuantizer.Count];
end;


procedure TTestQuantizer.SetOneColor;

begin
  FQuantizer.ColorNumber := 1;
end;


procedure TTestQuantizer.QuantizeGradient;

var
  lQuantizer: TFPColorQuantizer;
  lImage: TFPMemoryImage;
  lPalette: TFPPalette;

begin
  lPalette := nil;
  lImage := CreateGradientImage(16, 16);
  lQuantizer := CreateQuantizer;
  try
    lQuantizer.ColorNumber := 16;
    lQuantizer.Add(lImage);
    lPalette := lQuantizer.Quantize;
  finally
    lPalette.Free;
    lQuantizer.Free;
    lImage.Free;
  end;
end;


procedure TTestQuantizer.CheckInPalette(const aMessage: String; aPalette: TFPPalette; const aColor: TFPColor);

var
  lColor: TFPColor;
  I: Integer;

begin
  for I := 0 to aPalette.Count - 1 do
    begin
    lColor := aPalette[I];
    if (lColor.Alpha = aColor.Alpha) and ColorsClose(lColor, aColor, Tolerance) then
      Exit;
    end;
  Fail(Format('%s: %s is not in the palette of %d colours (tolerance %d)',
    [aMessage, ColorToStr(aColor), aPalette.Count, Tolerance]));
end;


procedure TTestQuantizer.TestPaletteHasAtMostTheColorNumber;

const
  cNumbers: array[0..3] of Integer = (2, 7, 16, 255);

var
  lImage: TFPMemoryImage;
  lPalette: TFPPalette;
  N: Integer;

begin
  lImage := CreateGradientImage(64, 64);
  try
    FQuantizer.Add(lImage);
    for N := Low(cNumbers) to High(cNumbers) do
      begin
      FQuantizer.ColorNumber := cNumbers[N];
      lPalette := FQuantizer.Quantize;
      try
        AssertNotNull(Format('%d colours: a palette is made', [cNumbers[N]]), lPalette);
        AssertTrue(Format('%d colours: the palette has at most that many colours, got %d',
          [cNumbers[N], lPalette.Count]), lPalette.Count <= cNumbers[N]);
        AssertTrue(Format('%d colours: the palette is not empty', [cNumbers[N]]), lPalette.Count > 0);
      finally
        lPalette.Free;
      end;
      end;
  finally
    lImage.Free;
  end;
end;


procedure TTestQuantizer.TestFewColorsAreReproduced;

var
  lImage: TFPMemoryImage;
  lPalette: TFPPalette;
  I: Integer;

begin
  lPalette := nil;
  lImage := CreateFewColorsImage(16, 16, 12);
  try
    FQuantizer.ColorNumber := 16;
    FQuantizer.Add(lImage);
    lPalette := FQuantizer.Quantize;
    AssertNotNull('A palette is made', lPalette);
    AssertTrue('The palette has at most 16 colours', lPalette.Count <= 16);
    for I := 0 to 11 do
      CheckInPalette(Format('Image colour %d', [I]), lPalette, lImage.Colors[I, 0]);
  finally
    lPalette.Free;
    lImage.Free;
  end;
end;


procedure TTestQuantizer.TestColorsDifferingInTheLowestBit;

var
  lImage: TFPMemoryImage;
  lPalette: TFPPalette;

begin
  lPalette := nil;
  lImage := TFPMemoryImage.Create(2, 1);
  try
    lImage.Colors[0, 0] := RGB8(10, 20, 30);
    lImage.Colors[1, 0] := RGB8(11, 20, 30);
    FQuantizer.Add(lImage);
    lPalette := FQuantizer.Quantize;
    AssertNotNull('A palette is made', lPalette);
    CheckInPalette('The first of two colours one apart in red', lPalette, RGB8(10, 20, 30));
    CheckInPalette('The second of two colours one apart in red', lPalette, RGB8(11, 20, 30));
  finally
    lPalette.Free;
    lImage.Free;
  end;
end;


procedure TTestQuantizer.TestOpaqueImageGivesOpaquePalette;

var
  lImage: TFPMemoryImage;
  lPalette: TFPPalette;
  I: Integer;

begin
  lPalette := nil;
  lImage := CreateGradientImage(32, 32);
  try
    FQuantizer.ColorNumber := 32;
    FQuantizer.Add(lImage);
    lPalette := FQuantizer.Quantize;
    AssertNotNull('A palette is made', lPalette);
    for I := 0 to lPalette.Count - 1 do
      AssertEquals(Format('Palette entry %d of an opaque image is opaque', [I]), alphaOpaque, lPalette[I].Alpha);
  finally
    lPalette.Free;
    lImage.Free;
  end;
end;


procedure TTestQuantizer.TestAlphaOfThePalette;

var
  lImage: TFPMemoryImage;
  lPalette: TFPPalette;
  lColors: array[0..2] of TFPColor;
  I: Integer;

begin
  lColors[0] := RGB8(248, 0, 0, 0);
  lColors[1] := RGB8(0, 248, 0, 128);
  lColors[2] := RGB8(0, 0, 248, 255);
  lPalette := nil;
  lImage := TFPMemoryImage.Create(3, 2);
  try
    for I := 0 to 5 do
      lImage.Colors[I mod 3, I div 3] := lColors[I mod 3];
    FQuantizer.ColorNumber := 8;
    FQuantizer.Add(lImage);
    lPalette := FQuantizer.Quantize;
    AssertNotNull('A palette is made', lPalette);
    if FQuantizer.SupportsAlpha then
      for I := 0 to 2 do
        CheckInPalette(Format('A quantizer with alpha keeps colour %d with its alpha', [I]), lPalette, lColors[I])
    else
      for I := 0 to lPalette.Count - 1 do
        AssertEquals(Format('A quantizer without alpha gives an opaque palette entry %d', [I]),
          alphaOpaque, lPalette[I].Alpha);
  finally
    lPalette.Free;
    lImage.Free;
  end;
end;


procedure TTestQuantizer.TestEmptyImage;

var
  lImage: TFPMemoryImage;
  lPalette: TFPPalette;
  lEmpty: Boolean;

begin
  lImage := TFPMemoryImage.Create(0, 0);
  try
    FQuantizer.Add(lImage);
    lPalette := FQuantizer.Quantize;
    lEmpty := lPalette = nil;
    if not lEmpty then
      begin
      lEmpty := lPalette.Count = 0;
      lPalette.Free;
      end;
    AssertTrue('An empty image gives no palette or an empty one', lEmpty);
  finally
    lImage.Free;
  end;
end;


procedure TTestQuantizer.TestNoImages;

var
  lPalette: TFPPalette;
  lEmpty: Boolean;

begin
  lPalette := FQuantizer.Quantize;
  lEmpty := lPalette = nil;
  if not lEmpty then
    begin
    lEmpty := lPalette.Count = 0;
    lPalette.Free;
    end;
  AssertTrue('Without images the quantizer gives no palette or an empty one', lEmpty);
end;


procedure TTestQuantizer.TestSeveralImages;

var
  lImage1, lImage2: TFPMemoryImage;
  lPalette: TFPPalette;

begin
  lImage2 := nil;
  lPalette := nil;
  lImage1 := CreateSolidImage(4, 4, RGB8(248, 0, 0));
  try
    lImage2 := CreateCheckerImage(4, 4, 1, RGB8(0, 248, 0), RGB8(0, 0, 248));
    FQuantizer.ColorNumber := 8;
    FQuantizer.Add(lImage1);
    FQuantizer.Add(lImage2);
    AssertEquals('Two images are added', 2, FQuantizer.Count);
    lPalette := FQuantizer.Quantize;
    AssertNotNull('A palette is made', lPalette);
    CheckInPalette('The colour of the first image', lPalette, RGB8(248, 0, 0));
    CheckInPalette('The first colour of the second image', lPalette, RGB8(0, 248, 0));
    CheckInPalette('The second colour of the second image', lPalette, RGB8(0, 0, 248));
  finally
    lPalette.Free;
    lImage2.Free;
    lImage1.Free;
  end;
end;


procedure TTestQuantizer.TestProgressStartsAndEnds;

var
  lImage: TFPMemoryImage;
  lPalette: TFPPalette;

begin
  lPalette := nil;
  lImage := CreateGradientImage(64, 64);
  try
    FQuantizer.ColorNumber := 16;
    FQuantizer.Add(lImage);
    lPalette := FQuantizer.Quantize;
    AssertTrue('Progress is reported', Length(FStages) >= 2);
    AssertTrue('The first progress event is psStarting', FStages[0] = psStarting);
    AssertTrue('The last progress event is psEnding', FStages[High(FStages)] = psEnding);
    AssertEquals('The last progress event reports 100%', 100, FPercents[High(FPercents)]);
  finally
    lPalette.Free;
    lImage.Free;
  end;
end;


procedure TTestQuantizer.TestCancelAtTheStartGivesNoPalette;

var
  lImage: TFPMemoryImage;
  lPalette: TFPPalette;

begin
  lImage := CreateGradientImage(16, 16);
  try
    FQuantizer.Add(lImage);
    FCancel := True;
    lPalette := FQuantizer.Quantize;
    AssertNull('A quantization cancelled at the start gives no palette', lPalette);
  finally
    lImage.Free;
  end;
end;


procedure TTestQuantizer.TestColorNumberBelowTwoRaises;

begin
  AssertRaises('A colour number of 1 is refused', FPQuantizerException, @SetOneColor);
end;


procedure TTestQuantizer.TestInvalidImageIndexRaises;

var
  lImage: TFPMemoryImage;

begin
  lImage := CreateGradientImage(2, 2);
  try
    FQuantizer.Add(lImage);
    AssertRaises('Reading the image at index Count raises', FPQuantizerException, @ReadImagePastTheEnd);
    AssertRaises('Reading the image at index -1 raises', FPQuantizerException, @ReadImageMinusOne);
  finally
    lImage.Free;
  end;
end;


procedure TTestQuantizer.TestNoLeak;

begin
  AssertNoLeak('Quantizing frees all temporary memory', @QuantizeGradient);
end;


function TTestOctreeQuantizer.CreateQuantizer: TFPColorQuantizer;

begin
  Result := TFPOctreeQuantizer.Create;
end;


function TTestMedianCutSlow.CreateQuantizer: TFPColorQuantizer;

begin
  Result := TFPMedianCutQuantizer.Create;
  TFPMedianCutQuantizer(Result).Mode := mcSlow;
end;


function TTestMedianCutNormal.CreateQuantizer: TFPColorQuantizer;

begin
  Result := TFPMedianCutQuantizer.Create;
  TFPMedianCutQuantizer(Result).Mode := mcNormal;
end;


function TTestMedianCutNormal.Tolerance: Word;

begin
  Result := 3 * 257;
end;


function TTestMedianCutFast.CreateQuantizer: TFPColorQuantizer;

begin
  Result := TFPMedianCutQuantizer.Create;
  TFPMedianCutQuantizer(Result).Mode := mcFast;
end;


function TTestMedianCutFast.Tolerance: Word;

begin
  Result := 7 * 257;
end;


{ TTestDitherer }

procedure TTestDitherer.SetUp;

begin
  inherited SetUp;
  FPalette := MakePalette([RGB8(0, 0, 0), RGB8(255, 0, 0), RGB8(0, 255, 0), RGB8(0, 0, 255),
    RGB8(255, 255, 0), RGB8(0, 255, 255), RGB8(255, 0, 255), RGB8(255, 255, 255)]);
  FDest := TFPMemoryImage.Create(0, 0);
end;


procedure TTestDitherer.TearDown;

begin
  FreeAndNil(FDitherer);
  FreeAndNil(FSource);
  FreeAndNil(FDest);
  FreeAndNil(FPalette);
  inherited TearDown;
end;


procedure TTestDitherer.DitherIntoItself;

begin
  FDitherer.Dither(FSource, FSource);
end;


procedure TTestDitherer.DitherWithEmptyPalette;

begin
  FDitherer.Dither(FSource, FDest);
end;


procedure TTestDitherer.DitherGradientFloydSteinberg;

var
  lDitherer: TFPFloydSteinbergDitherer;
  lSource, lDest: TFPMemoryImage;

begin
  lDest := nil;
  lDitherer := nil;
  lSource := CreateGradientImage(16, 12);
  try
    lDest := TFPMemoryImage.Create(0, 0);
    lDitherer := TFPFloydSteinbergDitherer.Create(FPalette);
    lDitherer.Dither(lSource, lDest);
  finally
    lDitherer.Free;
    lDest.Free;
    lSource.Free;
  end;
end;


procedure TTestDitherer.DitherGradientBase;

var
  lDitherer: TFPBaseDitherer;
  lSource, lDest: TFPMemoryImage;

begin
  lDest := nil;
  lDitherer := nil;
  lSource := CreateGradientImage(16, 12);
  try
    lDest := TFPMemoryImage.Create(0, 0);
    lDitherer := TFPBaseDitherer.Create(FPalette);
    lDitherer.Dither(lSource, lDest);
  finally
    lDitherer.Free;
    lDest.Free;
    lSource.Free;
  end;
end;


procedure TTestDitherer.CheckOnlyPaletteColors(const aMessage: String);

var
  lX, lY: Integer;

begin
  AssertEquals(aMessage + ': width', FSource.Width, FDest.Width);
  AssertEquals(aMessage + ': height', FSource.Height, FDest.Height);
  for lY := 0 to FDest.Height - 1 do
    for lX := 0 to FDest.Width - 1 do
      if not PaletteHas(FPalette, FDest.Colors[lX, lY]) then
        Fail(Format('%s: pixel (%d,%d) is %s, not a palette colour', [aMessage, lX, lY, ColorToStr(FDest.Colors[lX, lY])]));
end;


procedure TTestDitherer.CheckDestPalette(const aMessage: String);

var
  I: Integer;

begin
  AssertTrue(aMessage + ': the destination uses a palette', FDest.UsePalette);
  AssertEquals(aMessage + ': the destination palette has every entry', FPalette.Count, FDest.Palette.Count);
  for I := 0 to FPalette.Count - 1 do
    AssertColorsEqual(Format('%s: destination palette entry %d', [aMessage, I]), FPalette[I], FDest.Palette[I]);
end;


procedure TTestDitherer.DitherWith(aFloydSteinberg: Boolean);

begin
  FreeAndNil(FDitherer);
  if aFloydSteinberg then
    FDitherer := TFPFloydSteinbergDitherer.Create(FPalette)
  else
    FDitherer := TFPBaseDitherer.Create(FPalette);
  FDitherer.Dither(FSource, FDest);
end;


procedure TTestDitherer.TestSameSourceAndDestinationRaises;

begin
  FSource := CreateGradientImage(4, 4);
  FDitherer := TFPBaseDitherer.Create(FPalette);
  AssertRaises('Dithering an image into itself is refused', FPDithererException, @DitherIntoItself);
end;


procedure TTestDitherer.TestEmptyPaletteRaises;

begin
  FSource := CreateGradientImage(4, 4);
  FPalette.Clear;
  FDitherer := TFPBaseDitherer.Create(FPalette);
  AssertRaises('Dithering with an empty palette is refused', FPDithererException, @DitherWithEmptyPalette);
end;


procedure TTestDitherer.TestBaseUsesOnlyPaletteColors;

begin
  FSource := CreateGradientImage(16, 12);
  DitherWith(False);
  CheckDestPalette('Base ditherer');
  CheckOnlyPaletteColors('Base ditherer');
end;


procedure TTestDitherer.TestBasePicksTheNearestColor;

var
  lX, lY: Integer;

begin
  FSource := CreateGradientImage(16, 12);
  DitherWith(False);
  for lY := 0 to FSource.Height - 1 do
    for lX := 0 to FSource.Width - 1 do
      AssertEquals(Format('Pixel (%d,%d) gets a nearest palette colour', [lX, lY]),
        NearestDistance(FPalette, FSource.Colors[lX, lY]), Distance(FSource.Colors[lX, lY], FDest.Colors[lX, lY]));
end;


procedure TTestDitherer.TestFloydSteinbergUsesOnlyPaletteColors;

begin
  FSource := CreateGradientImage(16, 12);
  DitherWith(True);
  CheckDestPalette('Floyd-Steinberg');
  CheckOnlyPaletteColors('Floyd-Steinberg');
end;


procedure TTestDitherer.TestFloydSteinbergKeepsAPaletteColorOnEveryRow;

var
  lColor: TFPColor;
  lX, lY: Integer;

begin
  lColor := RGB8(200, 100, 50);
  FPalette.Free;
  FPalette := MakePalette([lColor, RGB8(255, 255, 255), RGB8(0, 0, 255)]);
  FSource := CreateSolidImage(6, 5, lColor);
  DitherWith(True);
  for lY := 0 to 4 do
    for lX := 0 to 5 do
      AssertColorsEqual(Format('Pixel (%d,%d) of a flat palette colour keeps that colour', [lX, lY]),
        lColor, FDest.Colors[lX, lY]);
end;


procedure TTestDitherer.TestFloydSteinbergMixesEveryRow;

var
  lWhite, lX, lY: Integer;

begin
  FPalette.Free;
  FPalette := MakePalette([RGB8(0, 0, 0), RGB8(255, 255, 255)]);
  FSource := CreateSolidImage(32, 6, RGB8(128, 128, 128));
  DitherWith(True);
  for lY := 0 to 5 do
    begin
    lWhite := 0;
    for lX := 0 to 31 do
      if FDest.Colors[lX, lY] = colWhite then
        Inc(lWhite);
    AssertTrue(Format('Row %d of a 50%% gray has between 30%% and 70%% white pixels, got %d of 32', [lY, lWhite]),
      (lWhite >= 10) and (lWhite <= 22));
    end;
end;


procedure TTestDitherer.TestFloydSteinbergSingleRow;

var
  lColor: TFPColor;
  lX: Integer;

begin
  lColor := RGB8(200, 100, 50);
  FPalette.Free;
  FPalette := MakePalette([lColor, RGB8(255, 255, 255), RGB8(0, 0, 255)]);
  FSource := CreateSolidImage(5, 1, lColor);
  DitherWith(True);
  for lX := 0 to 4 do
    AssertColorsEqual(Format('Pixel %d of a single row keeps its palette colour', [lX]), lColor, FDest.Colors[lX, 0]);
end;


procedure TTestDitherer.TestDuplicatePaletteEntriesKeepTheIndexOrder;

var
  lX, lY: Integer;

begin
  FPalette.Free;
  FPalette := MakePalette([RGB8(255, 0, 0), RGB8(255, 0, 0), RGB8(0, 0, 255)]);
  FSource := CreateSolidImage(3, 2, RGB8(0, 0, 250));
  DitherWith(False);
  CheckDestPalette('Base ditherer with a duplicate entry');
  for lY := 0 to 1 do
    for lX := 0 to 2 do
      AssertColorsEqual(Format('Pixel (%d,%d) is blue', [lX, lY]), RGB8(0, 0, 255), FDest.Colors[lX, lY]);
end;


procedure TTestDitherer.TestFloydSteinbergDuplicatePaletteEntries;

var
  lX, lY: Integer;

begin
  FPalette.Free;
  FPalette := MakePalette([RGB8(255, 0, 0), RGB8(255, 0, 0), RGB8(0, 0, 255)]);
  FSource := CreateSolidImage(3, 2, RGB8(0, 0, 255));
  DitherWith(True);
  CheckDestPalette('Floyd-Steinberg with a duplicate entry');
  for lY := 0 to 1 do
    for lX := 0 to 2 do
      AssertColorsEqual(Format('Pixel (%d,%d) is blue', [lX, lY]), RGB8(0, 0, 255), FDest.Colors[lX, lY]);
end;


procedure TTestDitherer.TestSortedPaletteFindsTheNearestColor;

var
  lPalette: TFPPalette;

begin
  FPalette.Free;
  FPalette := MakePalette([RGB8(0, 0, 0), RGB8(0, 255, 255), RGB8(128, 0, 0)]);
  lPalette := CopyPalette(FPalette);
  try
    FSource := TFPMemoryImage.Create(2, 1);
    FSource.Colors[0, 0] := RGB8(1, 0, 0);
    FSource.Colors[1, 0] := RGB8(0, 250, 250);
    FDitherer := TFPBaseDitherer.Create(lPalette);
    FDitherer.SortPalette;
    AssertTrue('SortPalette marks the palette sorted', FDitherer.PaletteSorted);
    FDitherer.Dither(FSource, FDest);
    AssertColorsEqual('(1,0,0) gets the nearest colour black', RGB8(0, 0, 0), FDest.Colors[0, 0]);
    AssertColorsEqual('(0,250,250) gets the nearest colour cyan', RGB8(0, 255, 255), FDest.Colors[1, 0]);
  finally
    FreeAndNil(FDitherer);
    lPalette.Free;
  end;
end;


procedure TTestDitherer.TestDitherLeavesTheCallerPaletteUnchanged;

var
  lCopy: TFPPalette;
  I: Integer;

begin
  lCopy := CopyPalette(FPalette);
  try
    FSource := CreateGradientImage(8, 8);
    DitherWith(True);
    DitherWith(False);
    AssertEquals('The caller palette keeps its size', lCopy.Count, FPalette.Count);
    for I := 0 to lCopy.Count - 1 do
      AssertColorsEqual(Format('Dithering leaves entry %d of the caller palette', [I]), lCopy[I], FPalette[I]);
  finally
    lCopy.Free;
  end;
end;


procedure TTestDitherer.TestSortPaletteLeavesTheCallerPaletteUnchanged;

var
  lCopy: TFPPalette;
  I: Integer;

begin
  FPalette.Free;
  FPalette := MakePalette([RGB8(255, 255, 255), RGB8(0, 0, 0), RGB8(255, 0, 0)]);
  lCopy := CopyPalette(FPalette);
  try
    FDitherer := TFPBaseDitherer.Create(FPalette);
    FDitherer.SortPalette;
    for I := 0 to lCopy.Count - 1 do
      AssertColorsEqual(Format('SortPalette leaves entry %d of the caller palette', [I]), lCopy[I], FPalette[I]);
  finally
    lCopy.Free;
  end;
end;


procedure TTestDitherer.TestEmptySourceGivesEmptyDestination;

var
  lFloyd: Boolean;

begin
  FSource := TFPMemoryImage.Create(0, 0);
  for lFloyd := False to True do
    begin
    FDest.SetSize(4, 4);
    DitherWith(lFloyd);
    AssertEquals(Format('%s: an empty source gives a destination of width 0', [BoolToStr(lFloyd, 'Floyd-Steinberg', 'Base')]),
      0, FDest.Width);
    AssertEquals(Format('%s: an empty source gives a destination of height 0', [BoolToStr(lFloyd, 'Floyd-Steinberg', 'Base')]),
      0, FDest.Height);
    end;
end;


procedure TTestDitherer.TestNoLeak;

begin
  AssertNoLeak('The base ditherer frees all temporary memory', @DitherGradientBase);
  AssertNoLeak('The Floyd-Steinberg ditherer frees all temporary memory', @DitherGradientFloydSteinberg);
end;


{ TTestColorHash }

procedure TTestColorHash.SetUp;

begin
  inherited SetUp;
  FTable := TFPColorHashTable.Create;
end;


procedure TTestColorHash.TearDown;

begin
  FreeAndNil(FTable);
  inherited TearDown;
end;


procedure TTestColorHash.InsertTwice;

var
  lTable: TFPColorHashTable;

begin
  lTable := TFPColorHashTable.Create;
  try
    lTable.Insert(RGB8(1, 2, 3), 10);
    lTable.Insert(RGB8(1, 2, 3), 20);
  finally
    lTable.Free;
  end;
end;


procedure TTestColorHash.InsertPointerData;

var
  lTable: TFPColorHashTable;
  lData: PInteger;

begin
  lTable := TFPColorHashTable.Create;
  try
    lData := GetMem(SizeOf(Integer));
    lData^ := 5;
    lTable.Insert(RGB8(1, 2, 3), Pointer(lData));
    lTable.Insert(RGB8(4, 5, 6), Pointer(nil));
  finally
    lTable.Free;
  end;
end;


procedure TTestColorHash.FillAndList;

var
  lTable: TFPColorHashTable;
  lArray: TFPColorWeightArray;
  I: Integer;

begin
  lTable := TFPColorHashTable.Create;
  try
    for I := 0 to 99 do
      lTable.Add(RGB8(I, (I * 3) and $FF, (I * 7) and $FF, 255 - I), 1);
    lArray := lTable.GetArray;
    for I := 0 to Length(lArray) - 1 do
      FreeMem(lArray[I]);
    lTable.Clear;
    lTable.Add(colRed, 2);
  finally
    lTable.Free;
  end;
end;


procedure TTestColorHash.GetArrayOfTable;

var
  lArray: TFPColorWeightArray;
  I: Integer;

begin
  lArray := FTable.GetArray;
  for I := 0 to Length(lArray) - 1 do
    FreeMem(lArray[I]);
end;


procedure TTestColorHash.TestInsertAndGet;

var
  lData: PInteger;

begin
  FTable.Insert(RGB8(10, 20, 30), 42);
  lData := FTable.Get(RGB8(10, 20, 30));
  AssertNotNull('An inserted colour is found', lData);
  AssertEquals('Get returns the inserted integer', 42, lData^);
end;


procedure TTestColorHash.TestGetOfAMissingColorIsNil;

begin
  AssertNull('An empty table finds nothing', FTable.Get(RGB8(10, 20, 30)));
  FTable.Insert(RGB8(10, 20, 30), 1);
  AssertNull('A colour differing in red is not found', FTable.Get(RGB8(11, 20, 30)));
  AssertNull('A colour differing in the high nibble of blue is not found', FTable.Get(RGB8(10, 20, 46)));
  AssertNull('A colour differing in alpha is not found', FTable.Get(RGB8(10, 20, 30, 254)));
end;


procedure TTestColorHash.TestAddAccumulates;

begin
  FTable.Add(RGB8(1, 2, 3), 1);
  FTable.Add(RGB8(1, 2, 3), 4);
  FTable.Add(RGB8(3, 2, 1), 7);
  AssertEquals('Add sums the values of a colour', 5, PInteger(FTable.Get(RGB8(1, 2, 3)))^);
  AssertEquals('Add keeps other colours apart', 7, PInteger(FTable.Get(RGB8(3, 2, 1)))^);
end;


procedure TTestColorHash.TestCountCountsDistinctColors;

var
  I: Integer;

begin
  AssertEquals('A new table is empty', 0, FTable.Count);
  for I := 0 to 49 do
    FTable.Add(RGB8(I, 0, 0), 1);
  for I := 0 to 49 do
    FTable.Add(RGB8(I, 0, 0), 1);
  AssertEquals('Count is the number of distinct colours', 50, FTable.Count);
end;


procedure TTestColorHash.TestManyColorsAreKeptApart;

var
  lData: PInteger;
  I: Integer;

begin
  for I := 4095 downto 0 do
    FTable.Insert(RGB8((I shr 8) shl 4 or 5, ((I shr 4) and 15) shl 4 or 6, (I and 15) shl 4 or 7), I);
  AssertEquals('Every colour has its own entry', 4096, FTable.Count);
  for I := 0 to 4095 do
    begin
    lData := FTable.Get(RGB8((I shr 8) shl 4 or 5, ((I shr 4) and 15) shl 4 or 6, (I and 15) shl 4 or 7));
    AssertNotNull(Format('Colour %d is found', [I]), lData);
    AssertEquals(Format('Colour %d has its own value', [I]), I, lData^);
    end;
end;


procedure TTestColorHash.TestAlphaIsPartOfTheKey;

var
  I: Integer;

begin
  for I := 0 to 255 do
    FTable.Insert(RGB8(1, 2, 3, I), I);
  AssertEquals('Colours differing in alpha have their own entry', 256, FTable.Count);
  for I := 0 to 255 do
    AssertEquals(Format('Alpha %d has its own value', [I]), I, PInteger(FTable.Get(RGB8(1, 2, 3, I)))^);
end;


procedure TTestColorHash.TestInsertReplacesTheValue;

begin
  FTable.Insert(RGB8(1, 2, 3), 10);
  FTable.Insert(RGB8(1, 2, 3), 20);
  AssertEquals('The second insert replaces the value', 20, PInteger(FTable.Get(RGB8(1, 2, 3)))^);
  AssertEquals('The second insert adds no entry', 1, FTable.Count);
end;


procedure TTestColorHash.InsertPointerTwice;

var
  lTable: TFPColorHashTable;

begin
  lTable := TFPColorHashTable.Create;
  try
    lTable.Insert(RGB8(1, 2, 3), GetMem(16));
    lTable.Insert(RGB8(1, 2, 3), GetMem(16));
  finally
    lTable.Free;
  end;
end;


procedure TTestColorHash.TestInsertPointerReplaceDoesNotLeak;

begin
  AssertNoLeak('Replacing the data of a colour frees the replaced data, which the table owns', @InsertPointerTwice);
end;


procedure TTestColorHash.TestInsertReplaceDoesNotLeak;

begin
  AssertNoLeak('Inserting a colour twice frees the replaced value', @InsertTwice);
end;


procedure TTestColorHash.TestTableFreesPointerData;

begin
  AssertNoLeak('The table frees the data inserted as pointers', @InsertPointerData);
end;


procedure TTestColorHash.TestClearEmptiesTheTable;

begin
  FTable.Add(RGB8(1, 2, 3), 1);
  FTable.Add(RGB8(4, 5, 6), 1);
  FTable.Clear;
  AssertEquals('Clear empties the table', 0, FTable.Count);
  AssertNull('Clear removes the colours', FTable.Get(RGB8(1, 2, 3)));
  FTable.Add(RGB8(1, 2, 3), 3);
  AssertEquals('A cleared table can be filled again', 3, PInteger(FTable.Get(RGB8(1, 2, 3)))^);
end;


procedure TTestColorHash.TestGetArrayListsEveryColor;

var
  lArray: TFPColorWeightArray;
  lColor: TFPColor;
  lFound: array[0..99] of Boolean;
  I, J: Integer;

begin
  for I := 0 to 99 do
    FTable.Add(RGB8(I, (I * 3) and $FF, (I * 7) and $FF, 255 - I), I + 1);
  lArray := FTable.GetArray;
  try
    AssertEquals('The array has an element per colour', 100, Length(lArray));
    FillChar(lFound, SizeOf(lFound), 0);
    for J := 0 to Length(lArray) - 1 do
      begin
      lColor := Packed2FPColor(lArray[J]^.Col);
      I := lArray[J]^.Num - 1;
      AssertTrue(Format('Element %d has a value of the table', [J]), (I >= 0) and (I <= 99));
      AssertColorsEqual(Format('Element %d has the colour of its value', [J]), RGB8(I, (I * 3) and $FF, (I * 7) and $FF, 255 - I), lColor);
      AssertFalse(Format('Element %d is listed once', [J]), lFound[I]);
      lFound[I] := True;
      end;
  finally
    for J := 0 to Length(lArray) - 1 do
      FreeMem(lArray[J]);
  end;
end;


procedure TTestColorHash.TestGetArrayWithPointerDataRaises;

begin
  FTable.Insert(RGB8(1, 2, 3), Pointer(nil));
  AssertRaises('GetArray of a table with pointer data raises', TFPColorHashException, @GetArrayOfTable);
end;


procedure TTestColorHash.TestNoLeak;

begin
  AssertNoLeak('Adding, listing and clearing frees everything', @FillAndList);
end;


procedure TTestColorHash.TestPackedRoundTrip;

var
  lPacked: TFPPackedColor;
  lColor: TFPColor;
  I: Integer;

begin
  for I := 0 to 255 do
    begin
    lPacked.R := I;
    lPacked.G := 255 - I;
    lPacked.B := (I * 7) and $FF;
    lPacked.A := (I * 13) and $FF;
    lColor := Packed2FPColor(lPacked);
    AssertColorsEqual(Format('Value %d: Packed2FPColor spreads each byte over 16 bits', [I]),
      RGB8(I, 255 - I, (I * 7) and $FF, (I * 13) and $FF), lColor);
    lPacked := FPColor2Packed(lColor);
    AssertEquals(Format('Value %d: red survives the round trip', [I]), I, lPacked.R);
    AssertEquals(Format('Value %d: green survives the round trip', [I]), 255 - I, lPacked.G);
    AssertEquals(Format('Value %d: blue survives the round trip', [I]), (I * 7) and $FF, lPacked.B);
    AssertEquals(Format('Value %d: alpha survives the round trip', [I]), (I * 13) and $FF, lPacked.A);
    end;
  lColor := FPColor($12FF, $3400, $56AB, $7801);
  lPacked := FPColor2Packed(lColor);
  AssertEquals('FPColor2Packed keeps the high byte of red', $12, lPacked.R);
  AssertEquals('FPColor2Packed keeps the high byte of green', $34, lPacked.G);
  AssertEquals('FPColor2Packed keeps the high byte of blue', $56, lPacked.B);
  AssertEquals('FPColor2Packed keeps the high byte of alpha', $78, lPacked.A);
end;


initialization
  RegisterTests('quantize', [TTestOctreeQuantizer, TTestMedianCutSlow, TTestMedianCutNormal,
    TTestMedianCutFast, TTestDitherer, TTestColorHash]);
end.
