{
    Tests for the WebP reader and writer: lossless and lossy files written by libwebp, round trips
    through every path of the encoder, the chunks written, metadata, animations and damaged files.
    See the file COPYING.FPC, included in this distribution, for details.
}
unit tcwebp;

{$mode objfpc}{$H+}

interface

uses sysutils, classes, types, fpcunit, testregistry, fpimage, fpimgtests, fpimagelist, fpimgcmn,
     webpcomn, fpwebpvp8l, fpwebpvp8, fpreadwebp, fpwritewebp, fpreadgif, fpwritegif;

type
  TPixelFunc = function(x, y: Integer): TFPColor;

  TTestWebP = class(TTestCase)
  private
    FStream: TMemoryStream;
    FReader: TFPReaderWebP;
    FWriter: TFPWriterWebP;
    FImage: TFPMemoryImage;
    FRead: TFPMemoryImage;
    FList: TFPImageList;
    // Replaces the stream with the bytes of a hexadecimal string.
    procedure LoadHex(const aHex: String);
    procedure ReadIt;
    // Reads a fixture and checks every pixel against aFunc.
    procedure CheckFixture(const aName, aHex: String; aWidth, aHeight: Integer; aFunc: TPixelFunc);
    // Round trips aImage and checks that it comes back unchanged.
    procedure CheckRoundTrip(const aMessage: String; aImage: TFPMemoryImage);
    // Returns the chunk names of the stream, separated by spaces.
    function ChunkList: String;
    // Returns the data of the first chunk named aFourCC.
    function ChunkData(const aFourCC: String): TBytes;
    // Returns an animation frame description.
    function Info(aLeft, aTop: Integer; aDelay: Cardinal; aDisposal: TFPFrameDisposal; aBlend: TFPFrameBlend): TFPFrameInfo;
    procedure WriteOddOffset;
    procedure WriteDisposalToPrevious;
    procedure WriteTooWide;
    procedure ReadTruncated;
    procedure ReadDamaged;
    // Returns the CRC32 of the 8-bit RGBA bytes of aImage, row by row.
    function RGBACRC(aImage: TFPCustomImage): LongWord;
    // Reads every frame of a lossy fixture and checks its size and the CRC32 of each frame.
    procedure CheckLossy(const aName, aHex: String; aWidth, aHeight: Integer; const aCRCs: array of LongWord);
    // Replaces the ALPH chunk of the raw alpha fixture by alpha filtered with aFilter, and checks it is read back.
    procedure CheckAlphaFilter(aFilter: Integer);
    procedure ReadTruncatedLossy;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLibWebPPredictorAndCrossColor;
    procedure TestLibWebPAlpha;
    procedure TestLibWebPColorIndexBundled;
    procedure TestLibWebPMetaPrefixCodes;
    procedure TestLibWebPColorCache;
    procedure TestRoundTripOfAGradient;
    procedure TestRoundTripOfAlpha;
    procedure TestRoundTripOfTwoColors;
    procedure TestRoundTripOfFourColors;
    procedure TestRoundTripOfSixteenColors;
    procedure TestRoundTripOf256Colors;
    procedure TestRoundTripOfNoise;
    procedure TestRoundTripOfOnePixel;
    procedure TestRoundTripOfARowAndAColumn;
    procedure TestRoundTripOfRepeats;
    procedure TestTheFileWritten;
    procedure TestTheVP8LHeaderWritten;
    procedure TestMetadataIsWrittenAndRead;
    procedure TestAnImageWithoutMetadataHasNoVP8X;
    procedure TestAnAnimationComesBack;
    procedure TestTheAnimationChunksWritten;
    procedure TestRawFramesKeepTheirPlace;
    procedure TestAFrameAtAnOddOffsetRaises;
    procedure TestDisposalToThePreviousRaises;
    procedure TestOneFrameIsAStillImage;
    procedure TestReadingOneImageOfAnAnimation;
    procedure TestTooWideRaises;
    procedure TestContentsCheck;
    procedure TestReadingAfterOtherData;
    procedure TestImageSize;
    procedure TestTruncatedDataRaises;
    procedure TestDamagedDataRaises;
    procedure TestLossyWithTheNormalFilterAndSegments;
    procedure TestLossyOfAnOddSize;
    procedure TestLossyWithTheSimpleFilterAndPartitions;
    procedure TestLossyWithCompressedFilteredAlpha;
    procedure TestLossyWithRawAlpha;
    procedure TestALossyAnimation;
    procedure TestAlphaHorizontalFilter;
    procedure TestAlphaVerticalFilter;
    procedure TestAlphaGradientFilter;
    procedure TestTheSizeOfALossyImage;
    procedure TestATruncatedLossyImageRaises;
    procedure TestGIFToWebP;
  end;

implementation

{$i webpfixtures.inc}
{$i webplossyfixtures.inc}

function PhotoPixel(x, y: Integer): TFPColor;

begin
  Result := RGB8((x * x + 3 * y) mod 256, (x * y + 7) mod 256, (x + 2 * y * y) mod 256);
end;


function AlphaPixel(x, y: Integer): TFPColor;

begin
  Result := RGB8((x * 5) mod 256, (y * 9) mod 256, (x * y) mod 256, (x * 8 + y * 3) mod 256);
end;


function PalettePixel(x, y: Integer): TFPColor;

begin
  case (x div 3 + y div 2) mod 3 of
    0: Result := RGB8(255, 0, 0);
    1: Result := RGB8(0, 128, 255);
  else
    Result := RGB8(20, 20, 20);
  end;
end;


function Noise(x, y: Integer): LongWord;

begin
  Result := ((QWord(x) * 2654435761 xor QWord(y) * 40503 xor QWord(x) * y * 97) and $FFFFFFFF) shr 13;
end;


function RegionPixel(x, y: Integer): TFPColor;

var
  n: LongWord;

begin
  n := Noise(x, y);
  if x < 40 then
    Result := RGB8(n mod 16 * 8, 30, 30)
  else if y < 30 then
    Result := RGB8(30, 30, n mod 8 * 16)
  else
    Result := RGB8(n mod 4 * 60, n mod 4 * 60, 200);
end;


function CachePixel(x, y: Integer): TFPColor;

var
  k: Integer;

begin
  k := (x * 7 + y * 13) mod 300;
  Result := RGB8((k * 3) mod 256, (k * 5) mod 256, (k div 3) mod 256);
end;


procedure TTestWebP.SetUp;

begin
  inherited SetUp;
  FStream := TMemoryStream.Create;
  FReader := TFPReaderWebP.Create;
  FWriter := TFPWriterWebP.Create;
end;


procedure TTestWebP.TearDown;

begin
  FreeAndNil(FList);
  FreeAndNil(FRead);
  FreeAndNil(FImage);
  FreeAndNil(FWriter);
  FreeAndNil(FReader);
  FreeAndNil(FStream);
  inherited TearDown;
end;


procedure TTestWebP.LoadHex(const aHex: String);

var
  i: Integer;
  lByte: Byte;

begin
  FStream.Clear;
  for i := 0 to Length(aHex) div 2 - 1 do
    begin
    lByte := StrToInt('$' + Copy(aHex, i * 2 + 1, 2));
    FStream.WriteBuffer(lByte, 1);
    end;
  FStream.Position := 0;
end;


procedure TTestWebP.ReadIt;

begin
  FreeAndNil(FRead);
  FRead := TFPMemoryImage.Create(0, 0);
  FRead.LoadFromStream(FStream, FReader);
end;


procedure TTestWebP.CheckFixture(const aName, aHex: String; aWidth, aHeight: Integer; aFunc: TPixelFunc);

var
  x, y: Integer;

begin
  LoadHex(aHex);
  ReadIt;
  AssertEquals(aName + ': width', aWidth, FRead.Width);
  AssertEquals(aName + ': height', aHeight, FRead.Height);
  for y := 0 to aHeight - 1 do
    for x := 0 to aWidth - 1 do
      AssertColorsEqual(Format('%s: pixel (%d,%d)', [aName, x, y]), aFunc(x, y), FRead.Colors[x, y]);
end;


procedure TTestWebP.CheckRoundTrip(const aMessage: String; aImage: TFPMemoryImage);

begin
  FImage := aImage;
  FRead := RoundTrip(FImage, FWriter, FReader);
  AssertImagesEqual(aMessage, FImage, FRead);
end;


function TTestWebP.ChunkList: String;

var
  lChunks: TWebPChunks;
  i: Integer;

begin
  lChunks := WebPReadChunks(FStream, 12, FStream.Size - 12);
  Result := '';
  for i := 0 to High(lChunks) do
    begin
    if Result <> '' then
      Result := Result + ' ';
    Result := Result + lChunks[i].FourCC;
    end;
end;


function TTestWebP.ChunkData(const aFourCC: String): TBytes;

var
  lChunks: TWebPChunks;
  i: Integer;

begin
  Result := nil;
  lChunks := WebPReadChunks(FStream, 12, FStream.Size - 12);
  for i := 0 to High(lChunks) do
    if lChunks[i].FourCC = aFourCC then
      begin
      SetLength(Result, lChunks[i].Size);
      Move((PByte(FStream.Memory) + lChunks[i].Offset)^, Result[0], lChunks[i].Size);
      exit;
      end;
  Fail('The file has a chunk ' + aFourCC);
end;


function TTestWebP.Info(aLeft, aTop: Integer; aDelay: Cardinal; aDisposal: TFPFrameDisposal; aBlend: TFPFrameBlend): TFPFrameInfo;

begin
  Result := DefaultFrameInfo;
  Result.Kind := fkAnimation;
  Result.Left := aLeft;
  Result.Top := aTop;
  Result.Delay := aDelay;
  Result.Disposal := aDisposal;
  Result.Blend := aBlend;
end;


procedure TTestWebP.WriteOddOffset;

begin
  FList := TFPImageList.Create;
  FList.Add(CreateSolidImage(4, 4, colRed), Info(0, 0, 0, fdNone, fbSource));
  FList.Add(CreateSolidImage(2, 2, colRed), Info(1, 0, 0, fdNone, fbSource));
  FList.SaveToStream(FStream, FWriter);
end;


procedure TTestWebP.WriteDisposalToPrevious;

begin
  FList := TFPImageList.Create;
  FList.Add(CreateSolidImage(4, 4, colRed), Info(0, 0, 0, fdPrevious, fbSource));
  FList.Add(CreateSolidImage(4, 4, colRed), Info(0, 0, 0, fdNone, fbSource));
  FList.SaveToStream(FStream, FWriter);
end;


procedure TTestWebP.WriteTooWide;

begin
  FImage := CreateSolidImage(16385, 1, colRed);
  FImage.SaveToStream(FStream, FWriter);
end;


procedure TTestWebP.ReadTruncated;

begin
  FImage := CreateGradientImage(40, 30);
  FImage.SaveToStream(FStream, FWriter);
  FStream.Size := FStream.Size - 20;
  FStream.Position := 0;
  ReadIt;
end;


procedure TTestWebP.ReadDamaged;

var
  i: Integer;

begin
  FImage := CreateGradientImage(40, 30);
  FImage.SaveToStream(FStream, FWriter);
  for i := 30 to FStream.Size - 1 do
    PByte(FStream.Memory)[i] := PByte(FStream.Memory)[i] xor $5A;
  FStream.Position := 0;
  ReadIt;
end;


function TTestWebP.RGBACRC(aImage: TFPCustomImage): LongWord;

var
  lBytes: array of Byte;
  lColor: TFPColor;
  x, y, lPos: Integer;

begin
  lBytes := nil;
  SetLength(lBytes, aImage.Width * aImage.Height * 4);
  lPos := 0;
  for y := 0 to aImage.Height - 1 do
    for x := 0 to aImage.Width - 1 do
      begin
      lColor := aImage.Colors[x, y];
      lBytes[lPos] := lColor.Red shr 8;
      lBytes[lPos + 1] := lColor.Green shr 8;
      lBytes[lPos + 2] := lColor.Blue shr 8;
      lBytes[lPos + 3] := lColor.Alpha shr 8;
      Inc(lPos, 4);
      end;
  Result := CalculateCRC($FFFFFFFF, lBytes[0], Length(lBytes)) xor $FFFFFFFF;
end;


procedure TTestWebP.CheckLossy(const aName, aHex: String; aWidth, aHeight: Integer; const aCRCs: array of LongWord);

var
  i: Integer;

begin
  LoadHex(aHex);
  FList := TFPImageList.Create;
  FList.LoadFromStream(FStream, FReader);
  AssertEquals(aName + ': the frames', Length(aCRCs), FList.Count);
  for i := 0 to FList.Count - 1 do
    begin
    AssertEquals(aName + ': width', aWidth, FList.Images[i].Width);
    AssertEquals(aName + ': height', aHeight, FList.Images[i].Height);
    AssertEquals(Format('%s: frame %d decodes as libwebp decodes it', [aName, i]), aCRCs[i], RGBACRC(FList.Images[i]));
    end;
end;


procedure TTestWebP.CheckAlphaFilter(aFilter: Integer);

var
  lSource: TMemoryStream;
  lChunks: TWebPChunks;
  lAlpha, lFiltered: array[0..127] of Byte;
  lData: array[0..128] of Byte;
  lSize: LongWord;
  i, x, y, lPred: Integer;

begin
  for y := 0 to 7 do
    for x := 0 to 15 do
      lAlpha[y * 16 + x] := (x * 29 + y * y * 13 + (x * y) mod 7) and $FF;
  for y := 0 to 7 do
    for x := 0 to 15 do
      begin
      if (x = 0) and (y = 0) then
        lPred := 0
      else if y = 0 then
        lPred := lAlpha[x - 1]
      else if x = 0 then
        lPred := lAlpha[(y - 1) * 16]
      else if aFilter = 1 then
        lPred := lAlpha[y * 16 + x - 1]
      else if aFilter = 2 then
        lPred := lAlpha[(y - 1) * 16 + x]
      else
        begin
        lPred := Integer(lAlpha[y * 16 + x - 1]) + lAlpha[(y - 1) * 16 + x] - lAlpha[(y - 1) * 16 + x - 1];
        if lPred < 0 then
          lPred := 0
        else if lPred > 255 then
          lPred := 255;
        end;
      lFiltered[y * 16 + x] := (lAlpha[y * 16 + x] - lPred) and $FF;
      end;
  lData[0] := aFilter shl 2;
  Move(lFiltered, lData[1], 128);
  LoadHex(FixtureLossyRawalpha);
  lChunks := WebPReadChunks(FStream, 12, FStream.Size - 12);
  lSource := TMemoryStream.Create;
  try
    lSource.CopyFrom(FStream, 0);
    FStream.Clear;
    FStream.WriteBuffer(WebPRIFF, 4);
    FStream.WriteBuffer(lSize, 4);
    FStream.WriteBuffer(WebPWEBP, 4);
    for i := 0 to High(lChunks) do
      if lChunks[i].FourCC = WebPALPH then
        WebPWriteChunk(FStream, WebPALPH, lData, SizeOf(lData))
      else
        WebPWriteChunk(FStream, lChunks[i].FourCC, (PByte(lSource.Memory) + lChunks[i].Offset)^, lChunks[i].Size);
  finally
    lSource.Free;
  end;
  lSize := NtoLE(LongWord(FStream.Size - 8));
  FStream.Position := 4;
  FStream.WriteBuffer(lSize, 4);
  FStream.Position := 0;
  ReadIt;
  for y := 0 to 7 do
    for x := 0 to 15 do
      AssertEquals(Format('Filter %d: the alpha of pixel (%d,%d)', [aFilter, x, y]), lAlpha[y * 16 + x],
        FRead.Colors[x, y].Alpha shr 8);
end;


procedure TTestWebP.ReadTruncatedLossy;

begin
  LoadHex(FixtureLossyLossy);
  FStream.Size := FStream.Size - 200;
  FStream.Position := 0;
  ReadIt;
end;


procedure TTestWebP.TestLibWebPPredictorAndCrossColor;

begin
  CheckFixture('predictor, cross-colour and subtract-green transforms', FixturePhoto, 24, 16, @PhotoPixel);
end;


procedure TTestWebP.TestLibWebPAlpha;

begin
  CheckFixture('alpha and a limited code length count', FixtureAlpha, 24, 16, @AlphaPixel);
end;


procedure TTestWebP.TestLibWebPColorIndexBundled;

begin
  CheckFixture('a colour index of 3 colours, 4 pixels to a byte', FixturePal, 30, 20, @PalettePixel);
end;


procedure TTestWebP.TestLibWebPMetaPrefixCodes;

begin
  CheckFixture('meta prefix codes', FixtureMeta, 80, 60, @RegionPixel);
end;


procedure TTestWebP.TestLibWebPColorCache;

begin
  CheckFixture('a colour cache', FixtureCache, 48, 48, @CachePixel);
end;


procedure TTestWebP.TestRoundTripOfAGradient;

begin
  CheckRoundTrip('A gradient comes back', CreateGradientImage(37, 23));
end;


procedure TTestWebP.TestRoundTripOfAlpha;

begin
  CheckRoundTrip('Alpha comes back', CreateAlphaImage(19, 13));
end;


procedure TTestWebP.TestRoundTripOfTwoColors;

begin
  CheckRoundTrip('Two colours, eight pixels to a byte, come back', CreateMonoImage(21, 9));
end;


procedure TTestWebP.TestRoundTripOfFourColors;

begin
  CheckRoundTrip('Four colours, four pixels to a byte, come back', CreateFewColorsImage(23, 7, 4));
end;


procedure TTestWebP.TestRoundTripOfSixteenColors;

begin
  CheckRoundTrip('Sixteen colours, two pixels to a byte, come back', CreateFewColorsImage(25, 11, 16));
end;


procedure TTestWebP.TestRoundTripOf256Colors;

var
  lImage: TFPMemoryImage;
  x, y: Integer;

begin
  lImage := TFPMemoryImage.Create(32, 16);
  for y := 0 to 15 do
    for x := 0 to 31 do
      lImage.Colors[x, y] := RGB8((x * 16 + y) mod 256, 7, 200, 255 - (y and 1));
  CheckRoundTrip('256 colours come back', lImage);
end;


procedure TTestWebP.TestRoundTripOfNoise;

var
  lImage: TFPMemoryImage;
  x, y: Integer;
  n: LongWord;

begin
  lImage := TFPMemoryImage.Create(47, 31);
  for y := 0 to 30 do
    for x := 0 to 46 do
      begin
      n := Noise(x, y);
      lImage.Colors[x, y] := RGB8(n and $FF, (n shr 3) and $FF, (n shr 6) and $FF, (n shr 1) and $FF);
      end;
  CheckRoundTrip('Noise of many colours comes back', lImage);
end;


procedure TTestWebP.TestRoundTripOfOnePixel;

begin
  CheckRoundTrip('One pixel comes back', CreateSolidImage(1, 1, RGB8(1, 2, 3, 4)));
end;


procedure TTestWebP.TestRoundTripOfARowAndAColumn;

begin
  CheckRoundTrip('A row comes back', CreateGradientImage(300, 1));
  FreeAndNil(FImage);
  FreeAndNil(FRead);
  CheckRoundTrip('A column comes back', CreateGradientImage(1, 300));
end;


procedure TTestWebP.TestRoundTripOfRepeats;

var
  lImage: TFPMemoryImage;
  x, y: Integer;

begin
  lImage := TFPMemoryImage.Create(200, 100);
  for y := 0 to 99 do
    for x := 0 to 199 do
      lImage.Colors[x, y] := RGB8((x mod 13) * 19, (y mod 7) * 36, ((x div 13 + y) mod 3) * 100);
  CheckRoundTrip('Repeating rows and runs come back', lImage);
  AssertTrue('Back references make them small', FStream.Size < 2000);
end;


procedure TTestWebP.TestTheFileWritten;

begin
  FImage := CreateGradientImage(9, 5);
  FImage.SaveToStream(FStream, FWriter);
  AssertTrue('The file starts with RIFF', CompareMem(FStream.Memory, PChar('RIFF'), 4));
  AssertEquals('The RIFF size is the rest of the file', FStream.Size - 8, LEtoN(PLongWord(PByte(FStream.Memory) + 4)^));
  AssertTrue('The RIFF form is WEBP', CompareMem(PByte(FStream.Memory) + 8, PChar('WEBP'), 4));
  AssertEquals('A still image without metadata is one VP8L chunk', 'VP8L', ChunkList);
end;


procedure TTestWebP.TestTheVP8LHeaderWritten;

var
  lData: TBytes;
  lInfo: TVP8LInfo;

begin
  FImage := CreateAlphaImage(300, 7);
  FImage.SaveToStream(FStream, FWriter);
  lData := ChunkData('VP8L');
  AssertEquals('The VP8L signature', $2F, lData[0]);
  AssertTrue('The header is read', VP8LReadInfo(@lData[0], Length(lData), lInfo));
  AssertEquals('Width', 300, lInfo.Width);
  AssertEquals('Height', 7, lInfo.Height);
  AssertTrue('Alpha is marked as used', lInfo.AlphaUsed);
end;


procedure TTestWebP.TestMetadataIsWrittenAndRead;

var
  lData: TBytes;

begin
  FImage := CreateAlphaImage(6, 4);
  FImage.Metadata[MetaICC] := TBytes.Create(1, 2, 3);
  FImage.Metadata[MetaExif] := TBytes.Create(4, 5);
  FImage.Metadata[MetaXMP] := TBytes.Create(6);
  FImage.SaveToStream(FStream, FWriter);
  AssertEquals('The chunks of an image with metadata', 'VP8X ICCP VP8L EXIF XMP ', ChunkList);
  lData := ChunkData('VP8X');
  AssertEquals('The VP8X flags', WebPFlagICC or WebPFlagExif or WebPFlagXMP or WebPFlagAlpha, lData[0]);
  AssertEquals('The canvas width', 5, WebPRead24(@lData[4]));
  AssertEquals('The canvas height', 3, WebPRead24(@lData[7]));
  FStream.Position := 0;
  ReadIt;
  AssertEquals('The ICC profile is read', 3, Length(FRead.Metadata[MetaICC]));
  AssertEquals('The EXIF data is read', 5, FRead.Metadata[MetaExif][1]);
  AssertEquals('The XMP data is read', 6, FRead.Metadata[MetaXMP][0]);
  AssertImagesEqual('The image is read', FImage, FRead);
end;


procedure TTestWebP.TestAnImageWithoutMetadataHasNoVP8X;

begin
  FImage := CreateAlphaImage(6, 4);
  FImage.SaveToStream(FStream, FWriter);
  AssertEquals('Alpha alone needs no VP8X', 'VP8L', ChunkList);
end;


procedure TTestWebP.TestAnAnimationComesBack;

var
  lBack: TFPImageList;
  lInfo: TFPFramesInfo;

begin
  FList := TFPImageList.Create;
  FList.Add(CreateGradientImage(10, 8), Info(0, 0, 100, fdNone, fbSource));
  FList.Add(CreateSolidImage(4, 2, colBlue), Info(2, 4, 250, fdNone, fbSource));
  lInfo := DefaultFramesInfo;
  lInfo.LoopCount := 3;
  lInfo.Background := colYellow;
  FList.Info := lInfo;
  FList.SaveToStream(FStream, FWriter);
  FStream.Position := 0;
  lBack := TFPImageList.Create;
  try
    lBack.LoadFromStream(FStream);
    AssertEquals('Two frames', 2, lBack.Count);
    AssertEquals('The plays', 3, lBack.Info.LoopCount);
    AssertColorsEqual('The background', colYellow, lBack.Info.Background);
    AssertEquals('The canvas', 10, lBack.Info.Width);
    AssertImagesEqual('The first frame', FList.Images[0], lBack.Images[0]);
    AssertColorsEqual('The second frame is drawn at its place', colBlue, lBack.Images[1].Colors[3, 5]);
    AssertColorsEqual('over the first', FList.Images[0].Colors[0, 0], lBack.Images[1].Colors[0, 0]);
    AssertEquals('The delay of the second frame', 250, lBack[1].Info.Delay);
  finally
    lBack.Free;
  end;
end;


procedure TTestWebP.TestTheAnimationChunksWritten;

var
  lData: TBytes;

begin
  FList := TFPImageList.Create;
  FList.Add(CreateSolidImage(6, 6, colRed), Info(0, 0, 70000, fdBackground, fbSource));
  FList.Add(CreateSolidImage(2, 3, colBlue), Info(4, 2, 30, fdNone, fbOver));
  FList.SaveToStream(FStream, FWriter);
  AssertEquals('The chunks of an animation', 'VP8X ANIM ANMF ANMF', ChunkList);
  lData := ChunkData('VP8X');
  AssertEquals('Animation and alpha are flagged', WebPFlagAnimation or WebPFlagAlpha, lData[0]);
  lData := ChunkData('ANMF');
  AssertEquals('The first frame is not moved', 0, WebPRead24(@lData[0]));
  AssertEquals('Its width', 5, WebPRead24(@lData[6]));
  AssertEquals('Its duration', 70000, WebPRead24(@lData[12]));
  AssertEquals('Disposed of, not blended', WebPFrameDispose or WebPFrameNoBlend, lData[15]);
  AssertTrue('Its image follows', CompareMem(@lData[16], PChar('VP8L'), 4));
end;


procedure TTestWebP.TestRawFramesKeepTheirPlace;

var
  lImage: TFPCustomImage;
  lInfo: TFPFrameInfo;

begin
  FList := TFPImageList.Create;
  FList.Add(CreateSolidImage(6, 6, colRed), Info(0, 0, 0, fdNone, fbSource));
  FList.Add(CreateSolidImage(2, 3, colBlue), Info(4, 2, 0, fdBackground, fbOver));
  FList.SaveToStream(FStream, FWriter);
  FStream.Position := 0;
  FReader.Composite := False;
  FReader.BeginFrames(FStream);
  FReader.ReadNextFrame(lInfo).Free;
  lImage := FReader.ReadNextFrame(lInfo);
  try
    AssertEquals('A raw frame has its own width', 2, lImage.Width);
    AssertEquals('Left, in pixels', 4, lInfo.Left);
    AssertEquals('Top, in pixels', 2, lInfo.Top);
    AssertTrue('Its disposal', lInfo.Disposal = fdBackground);
    AssertTrue('Its blending', lInfo.Blend = fbOver);
  finally
    lImage.Free;
  end;
  FReader.EndFrames;
end;


procedure TTestWebP.TestAFrameAtAnOddOffsetRaises;

begin
  AssertRaises('A frame at an odd offset raises', EWebPError, @WriteOddOffset);
end;


procedure TTestWebP.TestDisposalToThePreviousRaises;

begin
  AssertRaises('Disposal to the previous frame raises', EWebPError, @WriteDisposalToPrevious);
end;


procedure TTestWebP.TestOneFrameIsAStillImage;

var
  lInfo: TFPFramesInfo;

begin
  FImage := CreateGradientImage(5, 5);
  lInfo := DefaultFramesInfo;
  lInfo.FrameCount := 1;
  FWriter.BeginFrames(FStream, lInfo);
  FWriter.WriteNextFrame(FImage, DefaultFrameInfo);
  FWriter.EndFrames;
  AssertEquals('One frame is written as a still image', 'VP8L', ChunkList);
end;


procedure TTestWebP.TestReadingOneImageOfAnAnimation;

begin
  FList := TFPImageList.Create;
  FList.Add(CreateSolidImage(6, 4, colRed), Info(0, 0, 0, fdNone, fbSource));
  FList.Add(CreateSolidImage(2, 2, colBlue), Info(2, 2, 0, fdNone, fbSource));
  FList.SaveToStream(FStream, FWriter);
  FStream.Position := 0;
  ReadIt;
  AssertEquals('Reading one image gives the canvas', 6, FRead.Width);
  AssertColorsEqual('with the first frame', colRed, FRead.Colors[3, 3]);
end;


procedure TTestWebP.TestTooWideRaises;

begin
  AssertRaises('An image wider than 16384 pixels raises', EWebPError, @WriteTooWide);
end;


procedure TTestWebP.TestContentsCheck;

begin
  FImage := CreateGradientImage(3, 3);
  FImage.SaveToStream(FStream, FWriter);
  FStream.Position := 0;
  AssertTrue('A WebP file is accepted', FReader.CheckContents(FStream));
  AssertEquals('The check leaves the position alone', 0, FStream.Position);
  PByte(FStream.Memory)[12] := Ord('X');
  AssertFalse('An unknown first chunk is rejected', FReader.CheckContents(FStream));
  FStream.Free;
  FStream := BytesStream([Ord('R'), Ord('I'), Ord('F'), Ord('F'), 4, 0, 0, 0, Ord('W'), Ord('A'), Ord('V'), Ord('E')]);
  AssertFalse('Another RIFF form is rejected', FReader.CheckContents(FStream));
end;


procedure TTestWebP.TestReadingAfterOtherData;

begin
  FImage := CreateAlphaImage(8, 6);
  FStream.WriteBuffer(PChar('prefix')^, 6);
  WriteImage(FImage, FWriter, FStream);
  FStream.WriteBuffer(PChar('next')^, 4);
  FStream.Position := 6;
  ReadIt;
  AssertImagesEqual('A WebP after other data reads back', FImage, FRead);
  AssertEquals('The reader stops at the end of the file', FStream.Size - 4, FStream.Position);
end;


procedure TTestWebP.TestImageSize;

var
  lSize: TPoint;

begin
  FImage := CreateGradientImage(33, 12);
  FImage.SaveToStream(FStream, FWriter);
  FStream.Position := 0;
  lSize := TFPReaderWebP.ImageSize(FStream);
  AssertEquals('The width without reading the image', 33, lSize.X);
  AssertEquals('The height without reading the image', 12, lSize.Y);
end;


procedure TTestWebP.TestTruncatedDataRaises;

begin
  AssertRaises('A truncated file raises', EWebPError, @ReadTruncated);
end;


procedure TTestWebP.TestDamagedDataRaises;

begin
  AssertRaises('Damaged data raises', EWebPError, @ReadDamaged);
end;


procedure TTestWebP.TestLossyWithTheNormalFilterAndSegments;

begin
  CheckLossy('The normal loop filter and segments', FixtureLossyLossy, 32, 24, FixtureLossyLossyCRC);
end;


procedure TTestWebP.TestLossyOfAnOddSize;

begin
  CheckLossy('An odd size, cropped from its macroblocks', FixtureLossyOdd, 13, 7, FixtureLossyOddCRC);
end;


procedure TTestWebP.TestLossyWithTheSimpleFilterAndPartitions;

begin
  CheckLossy('The simple loop filter, sharpness and four token partitions', FixtureLossySimple, 24, 16,
    FixtureLossySimpleCRC);
end;


procedure TTestWebP.TestLossyWithCompressedFilteredAlpha;

begin
  CheckLossy('Lossless alpha, filtered and preprocessed', FixtureLossyAlpha, 24, 16, FixtureLossyAlphaCRC);
end;


procedure TTestWebP.TestLossyWithRawAlpha;

begin
  CheckLossy('Raw alpha', FixtureLossyRawalpha, 16, 8, FixtureLossyRawalphaCRC);
end;


procedure TTestWebP.TestALossyAnimation;

begin
  CheckLossy('A lossy animation with alpha', FixtureLossyAnim, 24, 16, FixtureLossyAnimCRC);
  AssertEquals('The delay of the second frame', 90, FList[1].Info.Delay);
end;


procedure TTestWebP.TestAlphaHorizontalFilter;

begin
  CheckAlphaFilter(1);
end;


procedure TTestWebP.TestAlphaVerticalFilter;

begin
  CheckAlphaFilter(2);
end;


procedure TTestWebP.TestAlphaGradientFilter;

begin
  CheckAlphaFilter(3);
end;


procedure TTestWebP.TestTheSizeOfALossyImage;

var
  lSize: TPoint;

begin
  LoadHex(FixtureLossyLossy);
  AssertTrue('A lossy WebP is accepted', FReader.CheckContents(FStream));
  lSize := TFPReaderWebP.ImageSize(FStream);
  AssertEquals('The width of a lossy image', 32, lSize.X);
  AssertEquals('The height of a lossy image', 24, lSize.Y);
end;


procedure TTestWebP.TestATruncatedLossyImageRaises;

begin
  AssertRaises('A truncated lossy image raises', EWebPError, @ReadTruncatedLossy);
end;


procedure TTestWebP.TestGIFToWebP;

var
  lGIF: TFPWriterGIF;
  lBack: TFPImageList;
  i: Integer;

begin
  FList := TFPImageList.Create;
  lGIF := TFPWriterGIF.Create;
  lBack := TFPImageList.Create;
  try
    FList.Add(CreateSolidImage(5, 4, colRed), Info(0, 0, 100, fdNone, fbSource));
    FList.Add(CreateSolidImage(5, 4, colGreen), Info(0, 0, 200, fdNone, fbSource));
    FList.SaveToStream(FStream, lGIF);
    FStream.Position := 0;
    FList.LoadFromStream(FStream);
    FStream.Clear;
    FList.SaveToStream(FStream, FWriter);
    FStream.Position := 0;
    lBack.LoadFromStream(FStream);
    AssertEquals('The WebP has the frames of the GIF', 2, lBack.Count);
    for i := 0 to 1 do
      AssertImagesEqual('Frame ' + IntToStr(i), FList.Images[i], lBack.Images[i]);
    AssertEquals('The delay of the second frame', 200, lBack[1].Info.Delay);
  finally
    lBack.Free;
    lGIF.Free;
  end;
end;


initialization
  RegisterTest('webp', TTestWebP);
end.
