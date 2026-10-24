{
    Tests for the PNG reader and writer: writer options, the chunks written,
    and files made by an encoder of the test itself for what the writer omits.
    See the file COPYING.FPC, included in this distribution, for details.
}
unit tcpng;

{$mode objfpc}{$H+}

interface

uses sysutils, classes, fpcunit, testregistry, zstream, fpimage, fpimgtests,
     fpreadpng, fpwritepng;

type
  // A sample of the pixel (aX,aY), channel aChannel, in the bit depth of the file.
  TSampleFunc = function(aX, aY, aChannel: Integer): Word of object;

  // A PNG encoder independent of fpwritepng: every filter, Adam7 and every depth.
  TPNGBuilder = class
  private
    FChunks: TMemoryStream;
    function RawRow(aX0, aDX, aY, aCount: Integer): TBytes;
    procedure WriteChunk(aStream: TStream; const aType: AnsiString; const aData: array of Byte);
  public
    Width, Height: Integer;
    ColorType, BitDepth: Byte;
    Interlaced: Boolean;
    // Filter type for every row, or -1 to use 0..4 in turn.
    Filter: Integer;
    Sample: TSampleFunc;
    constructor Create;
    destructor Destroy; override;
    // Number of channels of the colour type.
    function Channels: Integer;
    // Adds a chunk that goes between IHDR and IDAT.
    procedure AddChunk(const aType: AnsiString; const aData: array of Byte);
    // The whole file; the compressed data is split into IDAT chunks of aSplit bytes when positive.
    function Build(aSplit: Integer = 0): TMemoryStream;
  end;

  TTestPNGRoundTrip = class(TTestCase)
  private
    FWriter: TFPWriterPNG;
    FReader: TFPReaderPNG;
    FImage: TFPMemoryImage;
    FRead: TFPMemoryImage;
    procedure RoundTrip;
    procedure SetImage(aImage: TFPMemoryImage);
    procedure WriteTooManyColors;
    procedure WriteEmpty;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTheDefaultKeepsSixteenBitColors;
    procedure TestTheDefaultDropsAlpha;
    procedure TestUseAlphaKeepsAlpha;
    procedure TestEightBitWithAlpha;
    procedure TestEightBitKeepsEightBitColors;
    procedure TestGrayScaleKeepsGray;
    procedure TestGrayScaleOfColorsIsTheirLuma;
    procedure TestGrayScaleWithAlpha;
    procedure TestIndexedKeepsTheColors;
    procedure TestIndexedFromAPaletteImageKeepsThePalette;
    procedure TestIndexedWithTranslucentColors;
    procedure TestIndexedWithTooManyColorsIsRejected;
    procedure TestOneTransparentColorUsesTRNS;
    procedure TestTransparentPixelsOfManyColorsKeepTheirAlpha;
    procedure TestEveryCompressionLevel;
    procedure TestSmallAndLargeSizes;
    procedure TestRowsOfMoreThan32KBytes;
    procedure TestAnEmptyImageIsRejected;
  end;

  TTestPNGChunks = class(TTestCase)
  private
    FWriter: TFPWriterPNG;
    FImage: TFPMemoryImage;
    FStream: TMemoryStream;
    procedure WriteIt;
    function BE32(aOffset: Integer): LongWord;
    // Fails unless the header of the file written has that depth and colour type.
    procedure CheckHeader(const aMessage: String; aDepth, aColorType: Byte);
    // The chunk types of the file written, in order, separated by spaces.
    function ChunkList: String;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTheSignature;
    procedure TestTheHeaderForEachOption;
    procedure TestEveryChunkHasAValidCRC;
    procedure TestTheChunkOrder;
    procedure TestTheResolutionIsWritten;
    procedure TestNoResolutionMeansNoPHYs;
    procedure TestImageSizeOfAWideImage;
    procedure TestTheWriterLeavesTheImageAlone;
    procedure TestAResolutionWithoutUnitSurvives;
    procedure TestWritingAfterOtherDataKeepsIt;
    procedure TestTwoImagesInOneStream;
    procedure TestWritingToAWriteOnlyStream;
  end;

  TTestPNGDecoding = class(TTestCase)
  private
    FBuilder: TPNGBuilder;
    FReader: TFPReaderPNG;
    FImage: TFPMemoryImage;
    FStream: TMemoryStream;
    FPalette: array of TFPColor;
    FTrnsValue: array[0..2] of Word;
    FHasTrns: Boolean;
    function GradientSample(aX, aY, aChannel: Integer): Word;
    function IndexSample(aX, aY, aChannel: Integer): Word;
    // The colour a sample of the file stands for.
    function Expected(aX, aY: Integer): TFPColor;
    // Sets up the builder.
    procedure Prepare(aWidth, aHeight: Integer; aColorType, aDepth: Byte; aInterlaced: Boolean; aFilter: Integer);
    // A palette of aCount colours, written as PLTE.
    procedure AddPalette(aCount: Integer);
    // Builds, reads and compares every pixel.
    procedure CheckDecoded(const aMessage: String; aSplit: Integer = 0);
    procedure ReadStream;
    procedure ReadWithBadCRC;
    procedure ReadCriticalUnknown;
    procedure ReadPaletteTypeWithoutPalette;
    procedure ReadIndexBeyondPalette;
    procedure ReadBadDepth;
    procedure ReadTruncated;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestEveryFilterRGB8;
    procedure TestEveryFilterRGBA16;
    procedure TestEveryFilterGray1;
    procedure TestInterlaced;
    procedure TestInterlacedTinyImages;
    procedure TestInterlacedPalette2;
    procedure TestGrayDepths;
    procedure TestPaletteDepths;
    procedure TestGrayAlpha;
    procedure TestRGB16;
    procedure TestRGBA8;
    procedure TestTransparentGray;
    procedure TestTransparentGray16;
    procedure TestTransparentRGB;
    procedure TestTransparentRGB16;
    procedure TestPaletteTransparencyShorterThanThePalette;
    procedure TestDataSplitOverSeveralChunks;
    procedure TestUnknownAncillaryChunksAreSkipped;
    procedure TestUnknownCriticalChunksRaise;
    procedure TestABadCRCRaises;
    procedure TestPaletteTypeWithoutPaletteRaises;
    procedure TestAnIndexBeyondThePaletteRaises;
    procedure TestAnInvalidDepthIsRejected;
    procedure TestATruncatedFileRaises;
    procedure TestTheGammaIsRead;
    procedure TestReadingIntoAPaletteImageKeepsThePalette;
    procedure TestHeaderProperties;
  end;

implementation

var
  CRCTable: array[0..255] of LongWord;

procedure MakeCRCTable;

var
  lN, lK: Integer;
  lC: LongWord;

begin
  for lN := 0 to 255 do
    begin
    lC := lN;
    for lK := 0 to 7 do
      if Odd(lC) then
        lC := $EDB88320 xor (lC shr 1)
      else
        lC := lC shr 1;
    CRCTable[lN] := lC;
    end;
end;


// The CRC-32 of PNG over a buffer.
function CRC32(const aBuffer; aCount: Integer; aCRC: LongWord = $FFFFFFFF): LongWord;

var
  lP: PByte;
  I: Integer;

begin
  lP := @aBuffer;
  Result := aCRC;
  for I := 0 to aCount - 1 do
    Result := CRCTable[(Result xor lP[I]) and $FF] xor (Result shr 8);
end;


procedure WriteBE32(aStream: TStream; aValue: LongWord);

var
  lBytes: array[0..3] of Byte;

begin
  lBytes[0] := aValue shr 24;
  lBytes[1] := (aValue shr 16) and $FF;
  lBytes[2] := (aValue shr 8) and $FF;
  lBytes[3] := aValue and $FF;
  aStream.WriteBuffer(lBytes, 4);
end;


// The predictor of the Paeth filter.
function Paeth(aLeft, aUp, aUpLeft: Integer): Integer;

var
  lP, lPA, lPB, lPC: Integer;

begin
  lP := aLeft + aUp - aUpLeft;
  lPA := Abs(lP - aLeft);
  lPB := Abs(lP - aUp);
  lPC := Abs(lP - aUpLeft);
  if (lPA <= lPB) and (lPA <= lPC) then
    Result := aLeft
  else if lPB <= lPC then
    Result := aUp
  else
    Result := aUpLeft;
end;


{ TPNGBuilder }

constructor TPNGBuilder.Create;

begin
  inherited Create;
  FChunks := TMemoryStream.Create;
  BitDepth := 8;
  ColorType := 2;
end;


destructor TPNGBuilder.Destroy;

begin
  FChunks.Free;
  inherited Destroy;
end;


function TPNGBuilder.Channels: Integer;

begin
  case ColorType of
    0, 3: Result := 1;
    2: Result := 3;
    4: Result := 2;
    6: Result := 4;
  else
    Result := 1;
  end;
end;


procedure TPNGBuilder.WriteChunk(aStream: TStream; const aType: AnsiString; const aData: array of Byte);

var
  lCRC: LongWord;

begin
  WriteBE32(aStream, Length(aData));
  aStream.WriteBuffer(aType[1], 4);
  if Length(aData) > 0 then
    aStream.WriteBuffer(aData[0], Length(aData));
  lCRC := CRC32(aType[1], 4);
  if Length(aData) > 0 then
    lCRC := CRC32(aData[0], Length(aData), lCRC);
  WriteBE32(aStream, lCRC xor $FFFFFFFF);
end;


procedure TPNGBuilder.AddChunk(const aType: AnsiString; const aData: array of Byte);

begin
  WriteChunk(FChunks, aType, aData);
end;


function TPNGBuilder.RawRow(aX0, aDX, aY, aCount: Integer): TBytes;

var
  lBits, lBitPos, I, lChannel, lX: Integer;
  lValue: Word;

begin
  lBits := Channels * BitDepth;
  SetLength(Result, (aCount * lBits + 7) div 8);
  if Length(Result) > 0 then
    FillChar(Result[0], Length(Result), 0);
  lBitPos := 0;
  for I := 0 to aCount - 1 do
    begin
    lX := aX0 + I * aDX;
    for lChannel := 0 to Channels - 1 do
      begin
      lValue := Sample(lX, aY, lChannel);
      case BitDepth of
        16:
          begin
          Result[lBitPos div 8] := lValue shr 8;
          Result[lBitPos div 8 + 1] := lValue and $FF;
          end;
        8:
          Result[lBitPos div 8] := lValue;
      else
        Result[lBitPos div 8] := Result[lBitPos div 8] or (lValue shl (8 - BitDepth - (lBitPos mod 8)));
      end;
      Inc(lBitPos, BitDepth);
      end;
    end;
end;


function TPNGBuilder.Build(aSplit: Integer): TMemoryStream;

const
  cPass: array[0..6, 0..3] of Integer = ((0, 0, 8, 8), (4, 0, 8, 8), (0, 4, 4, 8),
    (2, 0, 4, 4), (0, 2, 2, 4), (1, 0, 2, 2), (0, 1, 1, 2));
  cSignature: array[0..7] of Byte = ($89, $50, $4E, $47, $0D, $0A, $1A, $0A);

var
  lRaw, lZip: TMemoryStream;
  lCompressor: TCompressionStream;
  lPass, lPasses, lX0, lY0, lDX, lDY, lCount, lY, I, lBpp, lRowIndex: Integer;
  lRow, lPrev, lOut: TBytes;
  lLeft, lUp, lUpLeft: Integer;
  lType: Byte;
  lHeader: array[0..12] of Byte;
  lPos, lSize: Int64;

begin
  lBpp := (Channels * BitDepth) div 8;
  if lBpp < 1 then
    lBpp := 1;
  lRaw := TMemoryStream.Create;
  lZip := TMemoryStream.Create;
  try
    if Interlaced then
      lPasses := 7
    else
      lPasses := 1;
    lRowIndex := 0;
    for lPass := 0 to lPasses - 1 do
      begin
      if Interlaced then
        begin
        lX0 := cPass[lPass, 0];
        lY0 := cPass[lPass, 1];
        lDX := cPass[lPass, 2];
        lDY := cPass[lPass, 3];
        end
      else
        begin
        lX0 := 0;
        lY0 := 0;
        lDX := 1;
        lDY := 1;
        end;
      if lX0 >= Width then
        Continue;
      lCount := (Width - lX0 + lDX - 1) div lDX;
      lPrev := nil;
      lY := lY0;
      while lY < Height do
        begin
        lRow := RawRow(lX0, lDX, lY, lCount);
        if lPrev = nil then
          begin
          SetLength(lPrev, Length(lRow));
          FillChar(lPrev[0], Length(lPrev), 0);
          end;
        if Filter < 0 then
          lType := lRowIndex mod 5
        else
          lType := Filter;
        Inc(lRowIndex);
        SetLength(lOut, Length(lRow));
        for I := 0 to High(lRow) do
          begin
          if I >= lBpp then
            begin
            lLeft := lRow[I - lBpp];
            lUpLeft := lPrev[I - lBpp];
            end
          else
            begin
            lLeft := 0;
            lUpLeft := 0;
            end;
          lUp := lPrev[I];
          case lType of
            0: lOut[I] := lRow[I];
            1: lOut[I] := (lRow[I] - lLeft) and $FF;
            2: lOut[I] := (lRow[I] - lUp) and $FF;
            3: lOut[I] := (lRow[I] - (lLeft + lUp) div 2) and $FF;
            4: lOut[I] := (lRow[I] - Paeth(lLeft, lUp, lUpLeft)) and $FF;
          end;
          end;
        lRaw.WriteBuffer(lType, 1);
        if Length(lOut) > 0 then
          lRaw.WriteBuffer(lOut[0], Length(lOut));
        lPrev := lRow;
        Inc(lY, lDY);
        end;
      end;
    lCompressor := TCompressionStream.Create(cldefault, lZip);
    try
      lCompressor.CopyFrom(lRaw, 0);
    finally
      lCompressor.Free;
    end;
    Result := TMemoryStream.Create;
    Result.WriteBuffer(cSignature, 8);
    lHeader[0] := Width shr 24;
    lHeader[1] := (Width shr 16) and $FF;
    lHeader[2] := (Width shr 8) and $FF;
    lHeader[3] := Width and $FF;
    lHeader[4] := Height shr 24;
    lHeader[5] := (Height shr 16) and $FF;
    lHeader[6] := (Height shr 8) and $FF;
    lHeader[7] := Height and $FF;
    lHeader[8] := BitDepth;
    lHeader[9] := ColorType;
    lHeader[10] := 0;
    lHeader[11] := 0;
    lHeader[12] := Ord(Interlaced);
    WriteChunk(Result, 'IHDR', lHeader);
    Result.CopyFrom(FChunks, 0);
    if aSplit <= 0 then
      aSplit := lZip.Size;
    lPos := 0;
    while lPos < lZip.Size do
      begin
      lSize := lZip.Size - lPos;
      if lSize > aSplit then
        lSize := aSplit;
      SetLength(lOut, lSize);
      Move((PByte(lZip.Memory) + lPos)^, lOut[0], lSize);
      WriteChunk(Result, 'IDAT', lOut);
      Inc(lPos, lSize);
      end;
    WriteChunk(Result, 'IEND', []);
    Result.Position := 0;
  finally
    lZip.Free;
    lRaw.Free;
  end;
end;


{ TTestPNGRoundTrip }

procedure TTestPNGRoundTrip.SetUp;

begin
  inherited SetUp;
  FWriter := TFPWriterPNG.Create;
  FReader := TFPReaderPNG.Create;
end;


procedure TTestPNGRoundTrip.TearDown;

begin
  FreeAndNil(FRead);
  FreeAndNil(FImage);
  FreeAndNil(FReader);
  FreeAndNil(FWriter);
  inherited TearDown;
end;


procedure TTestPNGRoundTrip.RoundTrip;

begin
  FreeAndNil(FRead);
  FRead := fpimgtests.RoundTrip(FImage, FWriter, FReader);
end;


procedure TTestPNGRoundTrip.SetImage(aImage: TFPMemoryImage);

begin
  FreeAndNil(FImage);
  FImage := aImage;
end;


procedure TTestPNGRoundTrip.WriteTooManyColors;

begin
  FWriter.Indexed := True;
  RoundTrip;
end;


procedure TTestPNGRoundTrip.WriteEmpty;

begin
  RoundTrip;
end;


procedure TTestPNGRoundTrip.TestTheDefaultKeepsSixteenBitColors;

var
  lX, lY: Integer;

begin
  SetImage(TFPMemoryImage.Create(7, 5));
  for lY := 0 to 4 do
    for lX := 0 to 6 do
      FImage[lX, lY] := FPColor(lX * 9001 + 3, lY * 13007 + 1, (lX * lY * 4099) and $FFFF);
  RoundTrip;
  AssertImagesEqual('The default writer keeps all 16 bits', FImage, FRead);
end;


procedure TTestPNGRoundTrip.TestTheDefaultDropsAlpha;

begin
  SetImage(CreateAlphaImage(6, 4));
  RoundTrip;
  AssertImagesEqual('Without UseAlpha the colours are kept', FImage, FRead, 0, True);
  AssertEquals('Without UseAlpha every pixel is opaque', alphaOpaque, FRead[0, 0].Alpha);
end;


procedure TTestPNGRoundTrip.TestUseAlphaKeepsAlpha;

begin
  SetImage(CreateAlphaImage(9, 7));
  FWriter.UseAlpha := True;
  RoundTrip;
  AssertImagesEqual('UseAlpha keeps alpha', FImage, FRead);
end;


procedure TTestPNGRoundTrip.TestEightBitWithAlpha;

begin
  SetImage(CreateAlphaImage(9, 7));
  FWriter.UseAlpha := True;
  FWriter.WordSized := False;
  RoundTrip;
  AssertImagesEqual('An 8-bit PNG with alpha keeps 8-bit colours and alpha', FImage, FRead);
end;


procedure TTestPNGRoundTrip.TestEightBitKeepsEightBitColors;

begin
  SetImage(CreateGradientImage(17, 9));
  FWriter.WordSized := False;
  RoundTrip;
  AssertImagesEqual('An 8-bit PNG keeps 8-bit colours', FImage, FRead);
end;


procedure TTestPNGRoundTrip.TestGrayScaleKeepsGray;

begin
  SetImage(CreateGrayImage(16, 16));
  FWriter.GrayScale := True;
  RoundTrip;
  AssertImagesEqual('A gray PNG keeps gray pixels', FImage, FRead, 1);
end;


procedure TTestPNGRoundTrip.TestGrayScaleOfColorsIsTheirLuma;

var
  lGray: Word;

begin
  SetImage(CreateSolidImage(2, 2, colGreen));
  FWriter.GrayScale := True;
  RoundTrip;
  lGray := CalculateGray(colGreen);
  AssertColorsEqual('A colour becomes its luma', FPColor(lGray, lGray, lGray), FRead[1, 1], 1);
end;


procedure TTestPNGRoundTrip.TestGrayScaleWithAlpha;

var
  lX, lY: Integer;

begin
  SetImage(CreateGrayImage(8, 8));
  for lY := 0 to 7 do
    for lX := 0 to 7 do
      FImage[lX, lY] := FPColor(FImage[lX, lY].Red, FImage[lX, lY].Red, FImage[lX, lY].Red, (lX * 32) * 257);
  FWriter.GrayScale := True;
  FWriter.UseAlpha := True;
  FWriter.WordSized := False;
  RoundTrip;
  AssertImagesEqual('A gray PNG with alpha keeps gray and alpha', FImage, FRead, 257);
end;


procedure TTestPNGRoundTrip.TestIndexedKeepsTheColors;

begin
  SetImage(CreateFewColorsImage(20, 10, 37));
  FWriter.Indexed := True;
  RoundTrip;
  AssertEquals('The reader reports an indexed file', True, FReader.Indexed);
  AssertImagesEqual('An indexed PNG keeps the colours', FImage, FRead);
end;


procedure TTestPNGRoundTrip.TestIndexedFromAPaletteImageKeepsThePalette;

var
  lStream: TMemoryStream;
  I: Integer;

begin
  SetImage(CreateFewColorsImage(10, 10, 12));
  FImage.UsePalette := True;
  FWriter.Indexed := True;
  FWriter.WordSized := False;
  lStream := TMemoryStream.Create;
  try
    FImage.SaveToStream(lStream, FWriter);
    lStream.Position := 0;
    FRead := TFPMemoryImage.Create(0, 0);
    FRead.UsePalette := True;
    FRead.LoadFromStream(lStream, FReader);
    AssertEquals('The palette has the same size', FImage.Palette.Count, FRead.Palette.Count);
    for I := 0 to FImage.Palette.Count - 1 do
      AssertColorsEqual(Format('Palette entry %d', [I]), FImage.Palette[I], FRead.Palette[I]);
    AssertEquals('Pixel indices are kept', FImage.Pixels[3, 4], FRead.Pixels[3, 4]);
  finally
    lStream.Free;
  end;
end;


procedure TTestPNGRoundTrip.TestIndexedWithTranslucentColors;

var
  lX: Integer;

begin
  SetImage(TFPMemoryImage.Create(8, 1));
  for lX := 0 to 7 do
    FImage[lX, 0] := RGB8(lX * 30, 100, 200, lX * 36);
  FWriter.Indexed := True;
  FWriter.UseAlpha := True;
  RoundTrip;
  AssertImagesEqual('An indexed PNG keeps the alpha of its palette', FImage, FRead);
end;


procedure TTestPNGRoundTrip.TestIndexedWithTooManyColorsIsRejected;

begin
  SetImage(CreateGradientImage(20, 20));
  AssertRaises('400 colours cannot be indexed', FPImageException, @WriteTooManyColors);
end;


procedure TTestPNGRoundTrip.TestOneTransparentColorUsesTRNS;

begin
  SetImage(CreateCheckerImage(6, 6, 2, colRed, FPColor(0, $FFFF, 0, 0)));
  FWriter.UseAlpha := True;
  RoundTrip;
  AssertEquals('One fully transparent colour needs no alpha channel', 2, FReader.ColorType);
  AssertImagesEqual('The transparent colour comes back transparent', FImage, FRead);
end;


procedure TTestPNGRoundTrip.TestTransparentPixelsOfManyColorsKeepTheirAlpha;

var
  lX, lY: Integer;

begin
  SetImage(CreateGradientImage(6, 6));
  FImage[0, 0] := FPColor($1000, 0, 0, 0);
  FImage[1, 0] := FPColor(0, $2000, 0, 0);
  FWriter.UseAlpha := True;
  RoundTrip;
  for lY := 0 to 5 do
    for lX := 0 to 5 do
      AssertEquals(Format('Alpha of pixel (%d,%d)', [lX, lY]), FImage[lX, lY].Alpha, FRead[lX, lY].Alpha);
end;


procedure TTestPNGRoundTrip.TestEveryCompressionLevel;

var
  lLevel: TCompressionLevel;

begin
  SetImage(CreateGradientImage(40, 30));
  for lLevel := Low(TCompressionLevel) to High(TCompressionLevel) do
    begin
    FWriter.CompressionLevel := lLevel;
    RoundTrip;
    AssertImagesEqual(Format('Compression level %d', [Ord(lLevel)]), FImage, FRead);
    end;
end;


procedure TTestPNGRoundTrip.TestSmallAndLargeSizes;

const
  cSizes: array[0..4, 0..1] of Integer = ((1, 1), (1, 9), (9, 1), (300, 200), (3, 1000));

var
  I: Integer;

begin
  for I := 0 to High(cSizes) do
    begin
    SetImage(CreateGradientImage(cSizes[I, 0], cSizes[I, 1]));
    RoundTrip;
    AssertImagesEqual(Format('An image of %dx%d', [cSizes[I, 0], cSizes[I, 1]]), FImage, FRead);
    end;
end;


procedure TTestPNGRoundTrip.TestRowsOfMoreThan32KBytes;

begin
  SetImage(CreateGradientImage(6000, 2));
  RoundTrip;
  AssertImagesEqual('A 16-bit RGB image 6000 pixels wide (36000 bytes a row)', FImage, FRead);
end;


procedure TTestPNGRoundTrip.TestAnEmptyImageIsRejected;

begin
  SetImage(TFPMemoryImage.Create(0, 0));
  AssertRaises('An image of no size cannot be written', FPImageException, @WriteEmpty);
end;


{ TTestPNGChunks }

procedure TTestPNGChunks.SetUp;

begin
  inherited SetUp;
  FWriter := TFPWriterPNG.Create;
  FImage := CreateGradientImage(5, 4);
  FStream := TMemoryStream.Create;
end;


procedure TTestPNGChunks.TearDown;

begin
  FreeAndNil(FStream);
  FreeAndNil(FImage);
  FreeAndNil(FWriter);
  inherited TearDown;
end;


procedure TTestPNGChunks.WriteIt;

begin
  FStream.Clear;
  FImage.SaveToStream(FStream, FWriter);
end;


function TTestPNGChunks.BE32(aOffset: Integer): LongWord;

var
  lP: PByte;

begin
  lP := PByte(FStream.Memory) + aOffset;
  Result := (LongWord(lP[0]) shl 24) or (lP[1] shl 16) or (lP[2] shl 8) or lP[3];
end;


procedure TTestPNGChunks.CheckHeader(const aMessage: String; aDepth, aColorType: Byte);

begin
  WriteIt;
  AssertEquals(aMessage + ': bit depth', aDepth, PByte(FStream.Memory)[24]);
  AssertEquals(aMessage + ': colour type', aColorType, PByte(FStream.Memory)[25]);
end;


function TTestPNGChunks.ChunkList: String;

var
  lPos: Int64;
  lLength: LongWord;
  lType: String;

begin
  Result := '';
  lPos := 8;
  while lPos + 12 <= FStream.Size do
    begin
    lLength := BE32(lPos);
    SetString(lType, PChar(FStream.Memory) + lPos + 4, 4);
    if Result <> '' then
      Result := Result + ' ';
    Result := Result + lType;
    lPos := lPos + 12 + lLength;
    end;
end;


procedure TTestPNGChunks.TestTheSignature;

const
  cSignature: array[0..7] of Byte = ($89, $50, $4E, $47, $0D, $0A, $1A, $0A);

begin
  WriteIt;
  AssertTrue('The file starts with the PNG signature', CompareMem(FStream.Memory, @cSignature, 8));
  AssertEquals('IHDR is 13 bytes', 13, BE32(8));
  AssertEquals('Width in IHDR', 5, BE32(16));
  AssertEquals('Height in IHDR', 4, BE32(20));
end;


procedure TTestPNGChunks.TestTheHeaderForEachOption;

begin
  CheckHeader('Default', 16, 2);
  FWriter.WordSized := False;
  CheckHeader('8-bit', 8, 2);
  FWriter.GrayScale := True;
  CheckHeader('8-bit gray', 8, 0);
  FWriter.UseAlpha := True;
  FImage[0, 0] := RGB8(1, 2, 3, 100);
  CheckHeader('8-bit gray with alpha', 8, 4);
  FWriter.GrayScale := False;
  CheckHeader('8-bit colour with alpha', 8, 6);
  FWriter.WordSized := True;
  CheckHeader('16-bit colour with alpha', 16, 6);
  FWriter.Indexed := True;
  CheckHeader('Indexed', 8, 3);
end;


procedure TTestPNGChunks.TestEveryChunkHasAValidCRC;

var
  lPos: Int64;
  lLength, lCRC: LongWord;

begin
  FWriter.Indexed := True;
  FWriter.UseAlpha := True;
  FImage[0, 0] := RGB8(1, 2, 3, 100);
  WriteIt;
  lPos := 8;
  while lPos + 12 <= FStream.Size do
    begin
    lLength := BE32(lPos);
    lCRC := CRC32((PByte(FStream.Memory) + lPos + 4)^, lLength + 4) xor $FFFFFFFF;
    AssertEquals('CRC of the chunk at ' + IntToStr(lPos), lCRC, BE32(lPos + 8 + lLength));
    lPos := lPos + 12 + lLength;
    end;
  AssertEquals('The chunks fill the file exactly', FStream.Size, lPos);
end;


procedure TTestPNGChunks.TestTheChunkOrder;

var
  lList: String;

begin
  FWriter.Indexed := True;
  WriteIt;
  lList := ChunkList;
  AssertEquals('IHDR comes first', 1, Pos('IHDR', lList));
  AssertTrue('PLTE comes before IDAT', Pos('PLTE', lList) < Pos('IDAT', lList));
  AssertTrue('PLTE is there', Pos('PLTE', lList) > 0);
  AssertEquals('IEND comes last', Length(lList) - 3, Pos('IEND', lList));
end;


procedure TTestPNGChunks.TestTheResolutionIsWritten;

var
  lList: String;
  lPos: Int64;
  lLength: LongWord;
  lType: String;

begin
  FImage.ResolutionUnit := ruPixelsPerInch;
  FImage.ResolutionX := 300;
  FImage.ResolutionY := 150;
  WriteIt;
  lList := ChunkList;
  AssertTrue('A pHYs chunk is written', Pos('pHYs', lList) > 0);
  lPos := 8;
  repeat
    lLength := BE32(lPos);
    SetString(lType, PChar(FStream.Memory) + lPos + 4, 4);
    if lType <> 'pHYs' then
      lPos := lPos + 12 + lLength;
  until lType = 'pHYs';
  AssertTrue('300 dpi is 11811 per metre', Abs(Int64(BE32(lPos + 8)) - 11811) <= 1);
  AssertTrue('150 dpi is 5906 per metre', Abs(Int64(BE32(lPos + 12)) - 5906) <= 1);
  AssertEquals('The unit is the metre', 1, PByte(FStream.Memory)[lPos + 16]);
end;


procedure TTestPNGChunks.TestNoResolutionMeansNoPHYs;

begin
  WriteIt;
  AssertEquals('An image without resolution gets no pHYs chunk', 0, Pos('pHYs', ChunkList));
end;


procedure TTestPNGChunks.TestImageSizeOfAWideImage;

var
  lSize: TPoint;

begin
  WriteIt;
  { width 70000 = $00011170, big-endian at offset 16 }
  PByte(FStream.Memory)[16] := $00;
  PByte(FStream.Memory)[17] := $01;
  PByte(FStream.Memory)[18] := $11;
  PByte(FStream.Memory)[19] := $70;
  PByte(FStream.Memory)[23] := 2;
  FStream.Position := 0;
  lSize := TFPReaderPNG.ImageSize(FStream);
  AssertEquals('ImageSize gives a width over 65535', 70000, lSize.X);
  AssertEquals('ImageSize gives the height', 2, lSize.Y);
end;


procedure TTestPNGChunks.TestTheWriterLeavesTheImageAlone;

begin
  FImage.ResolutionUnit := ruPixelsPerInch;
  FImage.ResolutionX := 300;
  FImage.ResolutionY := 300;
  WriteIt;
  AssertTrue('Writing keeps the resolution unit of the image', FImage.ResolutionUnit = ruPixelsPerInch);
  AssertEquals('Writing keeps the resolution of the image', 300, FImage.ResolutionX, 0.001);
end;


procedure TTestPNGChunks.TestAResolutionWithoutUnitSurvives;

var
  lReader: TFPReaderPNG;
  lRead: TFPMemoryImage;

begin
  FImage.ResolutionUnit := ruNone;
  FImage.ResolutionX := 3;
  FImage.ResolutionY := 1;
  lReader := TFPReaderPNG.Create;
  try
    lRead := RoundTrip(FImage, FWriter, lReader);
    try
      AssertTrue('The unit stays none', lRead.ResolutionUnit = ruNone);
      AssertEquals('The horizontal value comes back', 3, lRead.ResolutionX, 0.001);
      AssertEquals('The vertical value comes back', 1, lRead.ResolutionY, 0.001);
    finally
      lRead.Free;
    end;
  finally
    lReader.Free;
  end;
end;


procedure TTestPNGChunks.TestWritingAfterOtherDataKeepsIt;

var
  lReader: TFPReaderPNG;
  lRead: TFPMemoryImage;

begin
  FStream.WriteBuffer(PChar('prefix')^, 6);
  WriteImage(FImage, FWriter, FStream);
  AssertTrue('The bytes before the image are kept', CompareMem(FStream.Memory, PChar('prefix'), 6));
  FStream.Position := 6;
  lReader := TFPReaderPNG.Create;
  lRead := TFPMemoryImage.Create(0, 0);
  try
    lRead.LoadFromStream(FStream, lReader);
    AssertImagesEqual('The image after the prefix reads back', FImage, lRead);
  finally
    lRead.Free;
    lReader.Free;
  end;
end;


procedure TTestPNGChunks.TestTwoImagesInOneStream;

var
  lReader: TFPReaderPNG;
  lSecond, lRead: TFPMemoryImage;

begin
  lSecond := CreateCheckerImage(3, 3, 1, colRed, colBlue);
  lReader := TFPReaderPNG.Create;
  lRead := TFPMemoryImage.Create(0, 0);
  try
    WriteImage(FImage, FWriter, FStream);
    WriteImage(lSecond, FWriter, FStream);
    FStream.Position := 0;
    lRead.LoadFromStream(FStream, lReader);
    AssertImagesEqual('The first image', FImage, lRead);
    lRead.LoadFromStream(FStream, lReader);
    AssertImagesEqual('The second image follows the first', lSecond, lRead);
  finally
    lRead.Free;
    lReader.Free;
    lSecond.Free;
  end;
end;


procedure TTestPNGChunks.TestWritingToAWriteOnlyStream;

var
  lOut: TWriteOnlyStream;

begin
  lOut := TWriteOnlyStream.Create;
  try
    FImage.SaveToStream(lOut, FWriter, False);
    WriteIt;
    AssertEquals('A write-only stream gets all bytes', FStream.Size, lOut.Data.Size);
    AssertTrue('and the same bytes', CompareMem(FStream.Memory, lOut.Data.Memory, FStream.Size));
  finally
    lOut.Free;
  end;
end;


{ TTestPNGDecoding }

procedure TTestPNGDecoding.SetUp;

begin
  inherited SetUp;
  FBuilder := TPNGBuilder.Create;
  FReader := TFPReaderPNG.Create;
  FImage := TFPMemoryImage.Create(0, 0);
  FPalette := nil;
  FHasTrns := False;
end;


procedure TTestPNGDecoding.TearDown;

begin
  FreeAndNil(FStream);
  FreeAndNil(FImage);
  FreeAndNil(FReader);
  FreeAndNil(FBuilder);
  inherited TearDown;
end;


function TTestPNGDecoding.GradientSample(aX, aY, aChannel: Integer): Word;

var
  lMax: LongWord;

begin
  lMax := (LongWord(1) shl FBuilder.BitDepth) - 1;
  Result := ((aX * 37 + aY * 101 + aChannel * 53) * 97 + aX * aY) mod (lMax + 1);
end;


function TTestPNGDecoding.IndexSample(aX, aY, aChannel: Integer): Word;

begin
  Result := (aX * 3 + aY * 5) mod Length(FPalette);
end;


function TTestPNGDecoding.Expected(aX, aY: Integer): TFPColor;

  function Scale(aValue: Word): Word;

  begin
    Result := LongWord(aValue) * $FFFF div ((LongWord(1) shl FBuilder.BitDepth) - 1);
  end;

var
  lS: array[0..3] of Word;
  I: Integer;

begin
  for I := 0 to FBuilder.Channels - 1 do
    lS[I] := FBuilder.Sample(aX, aY, I);
  case FBuilder.ColorType of
    0: Result := FPColor(Scale(lS[0]), Scale(lS[0]), Scale(lS[0]));
    2: Result := FPColor(Scale(lS[0]), Scale(lS[1]), Scale(lS[2]));
    3: Result := FPalette[lS[0]];
    4: Result := FPColor(Scale(lS[0]), Scale(lS[0]), Scale(lS[0]), Scale(lS[1]));
    6: Result := FPColor(Scale(lS[0]), Scale(lS[1]), Scale(lS[2]), Scale(lS[3]));
  end;
  if FHasTrns then
    case FBuilder.ColorType of
      0: if lS[0] = FTrnsValue[0] then
           Result.Alpha := alphaTransparent;
      2: if (lS[0] = FTrnsValue[0]) and (lS[1] = FTrnsValue[1]) and (lS[2] = FTrnsValue[2]) then
           Result.Alpha := alphaTransparent;
    end;
end;


procedure TTestPNGDecoding.Prepare(aWidth, aHeight: Integer; aColorType, aDepth: Byte; aInterlaced: Boolean; aFilter: Integer);

begin
  FreeAndNil(FBuilder);
  FBuilder := TPNGBuilder.Create;
  FBuilder.Width := aWidth;
  FBuilder.Height := aHeight;
  FBuilder.ColorType := aColorType;
  FBuilder.BitDepth := aDepth;
  FBuilder.Interlaced := aInterlaced;
  FBuilder.Filter := aFilter;
  if aColorType = 3 then
    FBuilder.Sample := @IndexSample
  else
    FBuilder.Sample := @GradientSample;
  FHasTrns := False;
end;


procedure TTestPNGDecoding.AddPalette(aCount: Integer);

var
  lData: TBytes;
  I: Integer;

begin
  SetLength(FPalette, aCount);
  SetLength(lData, aCount * 3);
  for I := 0 to aCount - 1 do
    begin
    lData[I * 3] := (I * 71) and $FF;
    lData[I * 3 + 1] := (I * 29 + 7) and $FF;
    lData[I * 3 + 2] := (255 - I * 13) and $FF;
    FPalette[I] := RGB8(lData[I * 3], lData[I * 3 + 1], lData[I * 3 + 2]);
    end;
  FBuilder.AddChunk('PLTE', lData);
end;


procedure TTestPNGDecoding.ReadStream;

begin
  FImage.LoadFromStream(FStream, FReader);
end;


procedure TTestPNGDecoding.CheckDecoded(const aMessage: String; aSplit: Integer);

var
  lX, lY: Integer;

begin
  FreeAndNil(FStream);
  FStream := FBuilder.Build(aSplit);
  ReadStream;
  AssertEquals(aMessage + ': width', FBuilder.Width, FImage.Width);
  AssertEquals(aMessage + ': height', FBuilder.Height, FImage.Height);
  for lY := 0 to FBuilder.Height - 1 do
    for lX := 0 to FBuilder.Width - 1 do
      AssertColorsEqual(Format('%s: pixel (%d,%d)', [aMessage, lX, lY]), Expected(lX, lY), FImage[lX, lY]);
end;


procedure TTestPNGDecoding.ReadWithBadCRC;

var
  lP: PByte;

begin
  FreeAndNil(FStream);
  FStream := FBuilder.Build;
  lP := PByte(FStream.Memory) + FStream.Size - 20;
  lP^ := lP^ xor $FF;
  ReadStream;
end;


procedure TTestPNGDecoding.ReadCriticalUnknown;

begin
  FBuilder.AddChunk('ZZZZ', [1, 2, 3]);
  FreeAndNil(FStream);
  FStream := FBuilder.Build;
  ReadStream;
end;


procedure TTestPNGDecoding.ReadPaletteTypeWithoutPalette;

begin
  Prepare(4, 4, 3, 8, False, 0);
  SetLength(FPalette, 4);
  FreeAndNil(FStream);
  FStream := FBuilder.Build;
  ReadStream;
end;


procedure TTestPNGDecoding.ReadIndexBeyondPalette;

begin
  Prepare(4, 4, 3, 8, False, 0);
  AddPalette(2);
  SetLength(FPalette, 9);
  FreeAndNil(FStream);
  FStream := FBuilder.Build;
  ReadStream;
end;


procedure TTestPNGDecoding.ReadBadDepth;

begin
  Prepare(4, 4, 2, 4, False, 0);
  FBuilder.BitDepth := 8;
  FreeAndNil(FStream);
  FStream := FBuilder.Build;
  PByte(FStream.Memory)[24] := 4;
  { recompute the CRC of IHDR }
  PLongWord(PByte(FStream.Memory) + 29)^ := NtoBE(CRC32((PByte(FStream.Memory) + 12)^, 17) xor $FFFFFFFF);
  ReadStream;
end;


procedure TTestPNGDecoding.ReadTruncated;

begin
  Prepare(40, 40, 2, 8, False, 0);
  FreeAndNil(FStream);
  FStream := FBuilder.Build;
  FStream.Size := FStream.Size div 2;
  FStream.Position := 0;
  ReadStream;
end;


procedure TTestPNGDecoding.TestEveryFilterRGB8;

begin
  Prepare(13, 11, 2, 8, False, -1);
  CheckDecoded('RGB 8 with filters 0..4');
end;


procedure TTestPNGDecoding.TestEveryFilterRGBA16;

begin
  Prepare(9, 11, 6, 16, False, -1);
  CheckDecoded('RGBA 16 with filters 0..4');
end;


procedure TTestPNGDecoding.TestEveryFilterGray1;

begin
  Prepare(21, 11, 0, 1, False, -1);
  CheckDecoded('Gray 1-bit with filters 0..4');
end;


procedure TTestPNGDecoding.TestInterlaced;

begin
  Prepare(13, 11, 2, 8, True, -1);
  CheckDecoded('Interlaced RGB 8');
end;


procedure TTestPNGDecoding.TestInterlacedTinyImages;

const
  cSizes: array[0..4, 0..1] of Integer = ((1, 1), (2, 1), (1, 2), (3, 3), (5, 9));

var
  I: Integer;

begin
  for I := 0 to High(cSizes) do
    begin
    Prepare(cSizes[I, 0], cSizes[I, 1], 2, 8, True, -1);
    CheckDecoded(Format('Interlaced %dx%d', [cSizes[I, 0], cSizes[I, 1]]));
    end;
end;


procedure TTestPNGDecoding.TestInterlacedPalette2;

begin
  Prepare(11, 7, 3, 2, True, -1);
  AddPalette(4);
  CheckDecoded('Interlaced 2-bit palette');
end;


procedure TTestPNGDecoding.TestGrayDepths;

const
  cDepths: array[0..4] of Byte = (1, 2, 4, 8, 16);

var
  lDepth: Byte;

begin
  for lDepth in cDepths do
    begin
    Prepare(11, 3, 0, lDepth, False, 0);
    CheckDecoded(Format('Gray %d-bit', [lDepth]));
    end;
end;


procedure TTestPNGDecoding.TestPaletteDepths;

const
  cDepths: array[0..3] of Byte = (1, 2, 4, 8);

var
  lDepth: Byte;

begin
  for lDepth in cDepths do
    begin
    Prepare(11, 3, 3, lDepth, False, 0);
    AddPalette(1 shl lDepth);
    CheckDecoded(Format('Palette %d-bit', [lDepth]));
    end;
end;


procedure TTestPNGDecoding.TestGrayAlpha;

begin
  Prepare(7, 5, 4, 8, False, -1);
  CheckDecoded('Gray and alpha 8-bit');
  Prepare(7, 5, 4, 16, False, -1);
  CheckDecoded('Gray and alpha 16-bit');
end;


procedure TTestPNGDecoding.TestRGB16;

begin
  Prepare(7, 5, 2, 16, False, -1);
  CheckDecoded('RGB 16-bit');
end;


procedure TTestPNGDecoding.TestRGBA8;

begin
  Prepare(7, 5, 6, 8, False, -1);
  CheckDecoded('RGBA 8-bit');
end;


procedure TTestPNGDecoding.TestTransparentGray;

begin
  Prepare(9, 4, 0, 8, False, 0);
  FTrnsValue[0] := GradientSample(2, 1, 0);
  FHasTrns := True;
  FBuilder.AddChunk('tRNS', [0, FTrnsValue[0]]);
  CheckDecoded('tRNS for 8-bit gray');
end;


procedure TTestPNGDecoding.TestTransparentGray16;

begin
  Prepare(9, 4, 0, 16, False, 0);
  FTrnsValue[0] := GradientSample(2, 1, 0);
  FHasTrns := True;
  FBuilder.AddChunk('tRNS', [FTrnsValue[0] shr 8, FTrnsValue[0] and $FF]);
  CheckDecoded('tRNS for 16-bit gray');
end;


procedure TTestPNGDecoding.TestTransparentRGB;

var
  I: Integer;

begin
  Prepare(9, 4, 2, 8, False, 0);
  for I := 0 to 2 do
    FTrnsValue[I] := GradientSample(3, 2, I);
  FHasTrns := True;
  FBuilder.AddChunk('tRNS', [0, FTrnsValue[0], 0, FTrnsValue[1], 0, FTrnsValue[2]]);
  CheckDecoded('tRNS for 8-bit RGB');
end;


procedure TTestPNGDecoding.TestTransparentRGB16;

var
  I: Integer;

begin
  Prepare(9, 4, 2, 16, False, 0);
  for I := 0 to 2 do
    FTrnsValue[I] := GradientSample(3, 2, I);
  FHasTrns := True;
  FBuilder.AddChunk('tRNS', [FTrnsValue[0] shr 8, FTrnsValue[0] and $FF,
    FTrnsValue[1] shr 8, FTrnsValue[1] and $FF, FTrnsValue[2] shr 8, FTrnsValue[2] and $FF]);
  CheckDecoded('tRNS for 16-bit RGB');
end;


procedure TTestPNGDecoding.TestPaletteTransparencyShorterThanThePalette;

begin
  Prepare(8, 2, 3, 8, False, 0);
  AddPalette(6);
  FBuilder.AddChunk('tRNS', [0, 128]);
  FPalette[0].Alpha := 0;
  FPalette[1].Alpha := 128 * 257;
  CheckDecoded('tRNS with fewer entries than the palette');
end;


procedure TTestPNGDecoding.TestDataSplitOverSeveralChunks;

begin
  Prepare(30, 20, 2, 8, False, -1);
  CheckDecoded('IDAT in chunks of 7 bytes', 7);
end;


procedure TTestPNGDecoding.TestUnknownAncillaryChunksAreSkipped;

begin
  Prepare(5, 5, 2, 8, False, 0);
  FBuilder.AddChunk('tEXt', [Ord('a'), 0, Ord('b')]);
  FBuilder.AddChunk('zzZz', [1, 2, 3, 4]);
  CheckDecoded('Unknown ancillary chunks');
end;


procedure TTestPNGDecoding.TestUnknownCriticalChunksRaise;

begin
  Prepare(5, 5, 2, 8, False, 0);
  AssertRaises('An unknown critical chunk raises', FPImageException, @ReadCriticalUnknown);
end;


procedure TTestPNGDecoding.TestABadCRCRaises;

begin
  Prepare(5, 5, 2, 8, False, 0);
  AssertRaises('A chunk with a bad CRC raises', FPImageException, @ReadWithBadCRC);
end;


procedure TTestPNGDecoding.TestPaletteTypeWithoutPaletteRaises;

begin
  AssertRaises('Colour type 3 without PLTE raises', FPImageException, @ReadPaletteTypeWithoutPalette);
end;


procedure TTestPNGDecoding.TestAnIndexBeyondThePaletteRaises;

begin
  AssertRaises('An index past the palette raises', FPImageException, @ReadIndexBeyondPalette);
end;


procedure TTestPNGDecoding.TestAnInvalidDepthIsRejected;

begin
  AssertRaises('RGB with 4 bits per channel is rejected', FPImageException, @ReadBadDepth);
end;


procedure TTestPNGDecoding.TestATruncatedFileRaises;

begin
  AssertRaises('A file cut in the middle raises', FPImageException, @ReadTruncated);
end;


procedure TTestPNGDecoding.TestTheGammaIsRead;

begin
  Prepare(2, 2, 2, 8, False, 0);
  FBuilder.AddChunk('gAMA', [0, 0, $B1, $8F]);
  CheckDecoded('With gAMA');
  AssertEquals('The gamma of the file', 0.45455, FReader.Gamma, 0.00001);
end;


procedure TTestPNGDecoding.TestReadingIntoAPaletteImageKeepsThePalette;

var
  I: Integer;

begin
  Prepare(6, 3, 3, 4, False, 0);
  AddPalette(10);
  FImage.UsePalette := True;
  CheckDecoded('Into a palette image');
  AssertEquals('The palette of the file', 10, FImage.Palette.Count);
  for I := 0 to 9 do
    AssertColorsEqual(Format('Palette entry %d', [I]), FPalette[I], FImage.Palette[I]);
  AssertEquals('The index of the file', IndexSample(4, 2, 0), FImage.Pixels[4, 2]);
end;


procedure TTestPNGDecoding.TestHeaderProperties;

begin
  Prepare(5, 4, 4, 16, True, 0);
  CheckDecoded('Gray and alpha, interlaced');
  AssertEquals('Bit depth', 16, FReader.BitDepth);
  AssertEquals('Colour type', 4, FReader.ColorType);
  AssertEquals('Interlace', 1, FReader.Interlace);
  AssertTrue('Gray', FReader.GrayScale);
  AssertTrue('Alpha', FReader.UseAlpha);
  AssertTrue('Word sized', FReader.WordSized);
  AssertFalse('Not indexed', FReader.Indexed);
end;


initialization
  MakeCRCTable;
  RegisterTests('png', [TTestPNGRoundTrip, TTestPNGChunks, TTestPNGDecoding]);
end.
