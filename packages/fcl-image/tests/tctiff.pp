{
    Tests for the TIFF reader and writer: round trips, pages, tiles and the
    reader on hand-built files of every byte order, compression and layout.
    See the file COPYING.FPC, included in this distribution, for details.
}
unit tctiff;

{$mode objfpc}{$H+}

interface

uses sysutils, classes, fpcunit, testregistry, fpimage, fpimgtests,
     fptiffcmn, fpreadtiff, fpwritetiff;

type
  TTiffTestEntry = record
    Tag, Typ: Word;
    Values: array of DWord;
  end;

  TTestTIFFRoundTrip = class(TTestCase)
  private
    FReader: TFPReaderTiff;
    FWriter: TFPWriterTiff;
    FImage: TFPMemoryImage;
    FRead: TFPMemoryImage;
    FStream: TMemoryStream;
    // Writes FImage into FStream and reads it back into FRead.
    procedure RoundTripIt;
    // Creates a TFPMemoryImage for every image directory the reader loads.
    procedure CreateImage(Sender: TFPReaderTiff; ImgFileDir: TTiffIFD);
    // Writes FImage with the red channel set to 12 bits.
    procedure WriteTwelveBits;
    // Writes FImage as a palette image.
    procedure WritePalette;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRGBUncompressed;
    procedure TestRGBDeflate;
    procedure TestAlpha;
    procedure TestAlphaDeflate;
    procedure TestGray8Bit;
    procedure TestGrayWhiteIsZero;
    procedure TestGray16Bit;
    procedure TestRGB16Bit;
    procedure TestFloatSamples;
    procedure TestGrayStoresTheLuma;
    procedure TestManyStrips;
    procedure TestTiles;
    procedure TestTilesDeflate;
    procedure TestMirroredOrientations;
    procedure TestRotatedOrientations;
    procedure TestTheResolutionInInches;
    procedure TestTheResolutionInCentimeters;
    procedure TestTwoPages;
    procedure TestTheBiggestImageIsRead;
    procedure TestReusingTheWriterWritesOnlyTheNewImage;
    procedure TestUnsupportedBitsAreRejected;
    procedure TestPaletteImagesAreRejected;
    procedure TestWritingLeavesTheImageExtrasAlone;
  end;

  TTestTIFFStreams = class(TTestCase)
  private
    FReader: TFPReaderTiff;
    FWriter: TFPWriterTiff;
    FImage: TFPMemoryImage;
    FRead: TFPMemoryImage;
    FStream: TMemoryStream;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestWritingAfterOtherDataKeepsIt;
    procedure TestOffsetsAreRelativeToTheTiffStart;
    procedure TestTheReaderDoesNotReadPastTheTiff;
    procedure TestWritingToAWriteOnlyStream;
    procedure TestImageSizeWithoutReading;
  end;

  TTestTIFFReader = class(TTestCase)
  private
    FReader: TFPReaderTiff;
    FRead: TFPMemoryImage;
    FStream: TMemoryStream;
    FBigEndian: Boolean;
    FTiled: Boolean;
    FEntries: array of TTiffTestEntry;
    FChunks: array of TBytes;
    // Starts a new hand-built TIFF in the given byte order.
    procedure StartTiff(aBigEndian: Boolean);
    // Adds a directory entry of type aType (3 short, 4 long, 5 rational as pairs).
    procedure AddEntry(aTag, aType: Word; const aValues: array of DWord);
    // Adds width, height, bits per sample, photometric interpretation, samples per pixel and compression unless 0.
    procedure AddBasics(aWidth, aHeight, aPhotometric: DWord; const aBits: array of DWord; aCompression: DWord = TiffCompressionNone);
    // Adds a strip, or a tile if FTiled is set.
    procedure AddChunk(const aBytes: array of Byte);
    // Adds a strip or tile from a byte array.
    procedure AddChunkBytes(const aBytes: TBytes);
    // Writes the TIFF into FStream with offsets and byte counts of the chunks.
    procedure FinishTiff;
    // Writes a 16-bit value in the byte order of the TIFF.
    procedure W16(aValue: Word);
    // Writes a 32-bit value in the byte order of the TIFF.
    procedure W32(aValue: DWord);
    // Reads FStream from its start into FRead.
    procedure ReadIt;
    // Fails unless FRead has aColor at (aX, aY) within aTolerance.
    procedure CheckColor(const aMessage: String; aX, aY: Integer; const aColor: TFPColor; aTolerance: Word = 0);
    // Replaces FStream with a writer's uncompressed TIFF of an 8x8 gradient.
    procedure WriteSample;
    // Builds a 3x2 gray image with the given orientation tag.
    procedure BuildOrientation(aOrientation: DWord);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLittleEndianRGB;
    procedure TestBigEndianRGB;
    procedure Test16BitSamplesInBothByteOrders;
    procedure TestSeveralStrips;
    procedure TestNoRowsPerStripIsOneStrip;
    procedure TestNoCompressionTagMeansNone;
    procedure TestPackBits;
    procedure TestACutPackBitsLiteralRaises;
    procedure TestLZWByHand;
    procedure TestLZWCodeWidthsAndClearCode;
    procedure TestLZWInABigEndianFile;
    procedure TestLZWWithPredictor;
    procedure TestTiles;
    procedure TestPlanarConfigurationWithOneSample;
    procedure TestPlanarConfigurationWithThreeSamples;
    procedure TestPredictor8Bit;
    procedure TestPredictor16Bit;
    procedure TestPalette8Bit;
    procedure TestPalette4Bit;
    procedure TestPalette1Bit;
    procedure TestBilevelBlackIsZero;
    procedure TestBilevelWhiteIsZero;
    procedure TestBilevelFillOrder2;
    procedure TestGray4Bit;
    procedure TestGray12Bit;
    procedure TestUnassociatedAlpha;
    procedure TestAssociatedAlpha;
    procedure TestMirroredOrientations;
    procedure TestRotatedOrientations;
    procedure TestTheResolutionIsRead;
    procedure TestSampleValueTagsDoNotLeakIntoTheNextRead;
    procedure TestTheFloatExample;
    procedure TestAMissingPhotometricInterpretationRaises;
    procedure TestAnUnsupportedCompressionRaises;
    procedure TestACutDirectoryRaises;
    procedure TestACutStripRaises;
    procedure TestADirectoryOutsideTheStreamRaises;
    procedure TestContentsCheck;
    procedure TestACutHeaderIsRejected;
  end;

implementation

const
  cPrefix: array[0..5] of Byte = (9, 8, 7, 6, 5, 4);

// An opaque gray colour of 8-bit level aLevel.
function Gray8(aLevel: Byte): TFPColor;

begin
  Result := RGB8(aLevel, aLevel, aLevel);
end;


// An image with every channel, alpha included, using full 16-bit values.
function Create16BitImage(aWidth, aHeight: Integer; aGray: Boolean): TFPMemoryImage;

var
  lX, lY: Integer;
  lColor: TFPColor;

begin
  Result := TFPMemoryImage.Create(aWidth, aHeight);
  for lY := 0 to aHeight - 1 do
    for lX := 0 to aWidth - 1 do
      begin
      lColor.Red := (lX * 4099 + lY * 1013 + 17) and $FFFF;
      if aGray then
        begin
        lColor.Green := lColor.Red;
        lColor.Blue := lColor.Red;
        end
      else
        begin
        lColor.Green := (lX * 911 + lY * 7919 + 3) and $FFFF;
        lColor.Blue := (lX * 30011 + lY * 257 + 1) and $FFFF;
        end;
      lColor.Alpha := (lX * 1237 + lY * 5003 + 29) and $FFFF;
      Result.Colors[lX, lY] := lColor;
      end;
end;


// The TIFF LZW encoding of aData: MSB-first codes, clear code first, width change as libtiff.
function EncodeLZW(const aData: array of Byte): TBytes;

var
  lDict: array of Word;
  lOut: TBytes;
  lOutCount: Integer;
  lAcc: QWord;
  lBits, lWidth, lNext, lEnt, I: Integer;
  lCode: Word;

  procedure PutByte(aByte: Byte);

  begin
    if lOutCount >= Length(lOut) then
      SetLength(lOut, Length(lOut) * 2 + 16);
    lOut[lOutCount] := aByte;
    Inc(lOutCount);
  end;

  procedure PutCode(aCode: Integer);

  begin
    lAcc := (lAcc shl lWidth) or QWord(aCode);
    Inc(lBits, lWidth);
    while lBits >= 8 do
      begin
      PutByte((lAcc shr (lBits - 8)) and $FF);
      Dec(lBits, 8);
      lAcc := lAcc and ((QWord(1) shl lBits) - 1);
      end;
  end;

  procedure ResetTable;

  begin
    FillWord(lDict[0], Length(lDict), 0);
    lNext := 258;
  end;

  procedure AddedEntry;

  begin
    Inc(lNext);
    if lNext = 4094 then
      begin
      PutCode(256);
      ResetTable;
      lWidth := 9;
      end
    else if lNext > (1 shl lWidth) - 1 then
      Inc(lWidth);
  end;

begin
  SetLength(lDict, 4096 * 256);
  SetLength(lOut, 0);
  lOutCount := 0;
  lAcc := 0;
  lBits := 0;
  lWidth := 9;
  ResetTable;
  PutCode(256);
  if Length(aData) > 0 then
    begin
    lEnt := aData[0];
    for I := 1 to High(aData) do
      begin
      lCode := lDict[lEnt * 256 + aData[I]];
      if lCode <> 0 then
        lEnt := lCode
      else
        begin
        PutCode(lEnt);
        lDict[lEnt * 256 + aData[I]] := lNext;
        AddedEntry;
        lEnt := aData[I];
        end;
      end;
    PutCode(lEnt);
    AddedEntry;
    end;
  PutCode(257);
  if lBits > 0 then
    PutByte((lAcc shl (8 - lBits)) and $FF);
  SetLength(lOut, lOutCount);
  Result := lOut;
end;


{ TTestTIFFRoundTrip }

procedure TTestTIFFRoundTrip.SetUp;

begin
  inherited SetUp;
  FReader := TFPReaderTiff.Create;
  FWriter := TFPWriterTiff.Create;
  FStream := TMemoryStream.Create;
end;


procedure TTestTIFFRoundTrip.TearDown;

begin
  FreeAndNil(FStream);
  FreeAndNil(FRead);
  FreeAndNil(FImage);
  FreeAndNil(FWriter);
  FreeAndNil(FReader);
  inherited TearDown;
end;


procedure TTestTIFFRoundTrip.RoundTripIt;

begin
  FStream.Clear;
  FImage.SaveToStream(FStream, FWriter);
  FStream.Position := 0;
  FreeAndNil(FRead);
  FRead := TFPMemoryImage.Create(0, 0);
  FRead.LoadFromStream(FStream, FReader);
end;


procedure TTestTIFFRoundTrip.TestWritingLeavesTheImageExtrasAlone;

begin
  FImage := CreateGradientImage(4, 3);
  FImage.ResolutionUnit := ruPixelsPerInch;
  FImage.ResolutionX := 72;
  FImage.ResolutionY := 72;
  RoundTripIt;
  AssertEquals('Writing adds no extras to the image', 0, FImage.ExtraCount);
end;


procedure TTestTIFFRoundTrip.CreateImage(Sender: TFPReaderTiff; ImgFileDir: TTiffIFD);

begin
  ImgFileDir.Img := TFPMemoryImage.Create(0, 0);
  ImgFileDir.FreeImg := True;
end;


procedure TTestTIFFRoundTrip.WriteTwelveBits;

begin
  FImage.Extra[TiffRedBits] := '12';
  FImage.SaveToStream(FStream, FWriter);
end;


procedure TTestTIFFRoundTrip.WritePalette;

begin
  FImage.Extra[TiffPhotoMetric] := '3';
  FImage.SaveToStream(FStream, FWriter);
end;


procedure TTestTIFFRoundTrip.TestRGBUncompressed;

begin
  FImage := CreateGradientImage(23, 17);
  RoundTripIt;
  AssertImagesEqual('An uncompressed RGB TIFF keeps every colour', FImage, FRead);
end;


procedure TTestTIFFRoundTrip.TestRGBDeflate;

begin
  FImage := CreateGradientImage(23, 17);
  FImage.Extra[TiffCompression] := IntToStr(TiffCompressionDeflateAdobe);
  RoundTripIt;
  AssertImagesEqual('A Deflate RGB TIFF keeps every colour', FImage, FRead);
  AssertEquals('The reader reports Deflate compression', IntToStr(TiffCompressionDeflateAdobe), FRead.Extra[TiffCompression]);
end;


procedure TTestTIFFRoundTrip.TestAlpha;

begin
  FImage := CreateAlphaImage(19, 11);
  RoundTripIt;
  AssertImagesEqual('A TIFF keeps alpha', FImage, FRead);
end;


procedure TTestTIFFRoundTrip.TestAlphaDeflate;

begin
  FImage := CreateAlphaImage(19, 11);
  FImage.Extra[TiffCompression] := IntToStr(TiffCompressionDeflateAdobe);
  RoundTripIt;
  AssertImagesEqual('A Deflate TIFF keeps alpha', FImage, FRead);
end;


procedure TTestTIFFRoundTrip.TestGray8Bit;

begin
  FImage := CreateGrayImage(17, 13);
  FImage.Extra[TiffPhotoMetric] := '1';
  RoundTripIt;
  AssertImagesEqual('An 8-bit gray TIFF keeps every level', FImage, FRead);
  AssertEquals('The reader reports 8 gray bits', '8', FRead.Extra[TiffGrayBits]);
end;


procedure TTestTIFFRoundTrip.TestGrayWhiteIsZero;

begin
  FImage := CreateGrayImage(17, 13);
  FImage.Extra[TiffPhotoMetric] := '0';
  RoundTripIt;
  AssertImagesEqual('A white-is-zero gray TIFF keeps every level', FImage, FRead);
  AssertEquals('The reader reports white is zero', '0', FRead.Extra[TiffPhotoMetric]);
end;


procedure TTestTIFFRoundTrip.TestGray16Bit;

begin
  FImage := Create16BitImage(13, 7, True);
  FImage.Extra[TiffPhotoMetric] := '1';
  FImage.Extra[TiffGrayBits] := '16';
  FImage.Extra[TiffAlphaBits] := '16';
  RoundTripIt;
  AssertImagesEqual('A 16-bit gray TIFF keeps every 16-bit level and alpha', FImage, FRead);
end;


procedure TTestTIFFRoundTrip.TestRGB16Bit;

begin
  FImage := Create16BitImage(13, 7, False);
  FImage.Extra[TiffRedBits] := '16';
  FImage.Extra[TiffGreenBits] := '16';
  FImage.Extra[TiffBlueBits] := '16';
  FImage.Extra[TiffAlphaBits] := '16';
  RoundTripIt;
  AssertImagesEqual('A 16-bit RGBA TIFF keeps every 16-bit value', FImage, FRead);
end;


procedure TTestTIFFRoundTrip.TestFloatSamples;

begin
  FImage := Create16BitImage(13, 7, False);
  FImage.Extra[TiffSampleFormat] := IntToStr(TiffSampleFormatIEEEFloat);
  FImage.Extra[TiffRedBits] := '32';
  FImage.Extra[TiffGreenBits] := '32';
  FImage.Extra[TiffBlueBits] := '32';
  RoundTripIt;
  AssertImagesEqual('A 32-bit float TIFF keeps every 16-bit value', FImage, FRead, 0, True);
  AssertEquals('A float TIFF without alpha is opaque', alphaOpaque, FRead[3, 3].Alpha);
end;


procedure TTestTIFFRoundTrip.TestGrayStoresTheLuma;

const
  cColors: array[0..3] of TFPColor = (
    (Red: $FFFF; Green: 0; Blue: 0; Alpha: $FFFF),
    (Red: 0; Green: $FFFF; Blue: 0; Alpha: $FFFF),
    (Red: 0; Green: 0; Blue: $FFFF; Alpha: $FFFF),
    (Red: $C0C0; Green: $4040; Blue: $8080; Alpha: $FFFF));

var
  I: Integer;
  lGray: Word;

begin
  FImage := TFPMemoryImage.Create(4, 1);
  for I := 0 to 3 do
    FImage[I, 0] := cColors[I];
  FImage.Extra[TiffPhotoMetric] := '1';
  RoundTripIt;
  for I := 0 to 3 do
    begin
    lGray := (CalculateGray(cColors[I]) shr 8) * 257;
    AssertColorsEqual(Format('Colour %d is stored as its luma', [I]),
      FPColor(lGray, lGray, lGray), FRead[I, 0], 257);
    end;
end;


procedure TTestTIFFRoundTrip.TestManyStrips;

begin
  FImage := CreateAlphaImage(64, 100);
  RoundTripIt;
  AssertImagesEqual('An image of several strips comes back', FImage, FRead);
end;


procedure TTestTIFFRoundTrip.TestTiles;

begin
  FImage := CreateAlphaImage(40, 24);
  FImage.Extra[TiffTileWidth] := '16';
  FImage.Extra[TiffTileLength] := '16';
  RoundTripIt;
  AssertImagesEqual('A tiled TIFF with partial tiles comes back', FImage, FRead);
end;


procedure TTestTIFFRoundTrip.TestTilesDeflate;

begin
  FImage := CreateAlphaImage(40, 24);
  FImage.Extra[TiffTileWidth] := '16';
  FImage.Extra[TiffTileLength] := '16';
  FImage.Extra[TiffCompression] := IntToStr(TiffCompressionDeflateAdobe);
  RoundTripIt;
  AssertImagesEqual('A Deflate tiled TIFF with partial tiles comes back', FImage, FRead);
end;


procedure TTestTIFFRoundTrip.TestMirroredOrientations;

var
  lOrientation: Integer;

begin
  FImage := CreateGradientImage(5, 3);
  for lOrientation := 2 to 4 do
    begin
    FImage.Extra[TiffOrientation] := IntToStr(lOrientation);
    RoundTripIt;
    AssertImagesEqual(Format('Orientation %d comes back', [lOrientation]), FImage, FRead);
    end;
end;


procedure TTestTIFFRoundTrip.TestRotatedOrientations;

var
  lOrientation: Integer;

begin
  FImage := CreateGradientImage(5, 3);
  for lOrientation := 5 to 8 do
    begin
    FImage.Extra[TiffOrientation] := IntToStr(lOrientation);
    RoundTripIt;
    AssertImagesEqual(Format('Orientation %d comes back', [lOrientation]), FImage, FRead);
    end;
end;


procedure TTestTIFFRoundTrip.TestTheResolutionInInches;

begin
  FImage := CreateGradientImage(4, 4);
  FImage.ResolutionUnit := ruPixelsPerInch;
  FImage.ResolutionX := 300;
  FImage.ResolutionY := 150;
  RoundTripIt;
  AssertTrue('The unit comes back as pixels per inch', FRead.ResolutionUnit = ruPixelsPerInch);
  AssertEquals('The horizontal resolution comes back', 300, FRead.ResolutionX, 0.001);
  AssertEquals('The vertical resolution comes back', 150, FRead.ResolutionY, 0.001);
end;


procedure TTestTIFFRoundTrip.TestTheResolutionInCentimeters;

begin
  FImage := CreateGradientImage(4, 4);
  FImage.ResolutionUnit := ruPixelsPerCentimeter;
  FImage.ResolutionX := 118.5;
  FImage.ResolutionY := 59.25;
  RoundTripIt;
  AssertTrue('The unit comes back as pixels per centimeter', FRead.ResolutionUnit = ruPixelsPerCentimeter);
  AssertEquals('The horizontal resolution comes back', 118.5, FRead.ResolutionX, 0.001);
  AssertEquals('The vertical resolution comes back', 59.25, FRead.ResolutionY, 0.001);
end;


procedure TTestTIFFRoundTrip.TestTwoPages;

var
  lSecond: TFPMemoryImage;

begin
  FImage := CreateGradientImage(7, 5);
  FImage.Extra[TiffPageNumber] := '0';
  FImage.Extra[TiffPageCount] := '2';
  lSecond := CreateAlphaImage(3, 9);
  try
    lSecond.Extra[TiffPageNumber] := '1';
    lSecond.Extra[TiffPageCount] := '2';
    lSecond.Extra[TiffPageName] := 'second';
    FWriter.AddImage(FImage);
    FWriter.AddImage(lSecond);
    FWriter.SaveToStream(FStream);
    FStream.Position := 0;
    FReader.OnCreateImage := @CreateImage;
    FReader.LoadFromStream(FStream);
    AssertEquals('Both pages are read', 2, FReader.ImageCount);
    AssertImagesEqual('The first page comes back', FImage, FReader.Images[0].Img);
    AssertImagesEqual('The second page comes back', lSecond, FReader.Images[1].Img);
    AssertEquals('The first page has number 0', 0, FReader.Images[0].PageNumber);
    AssertEquals('The second page has number 1', 1, FReader.Images[1].PageNumber);
    AssertEquals('The second page knows the page count', 2, FReader.Images[1].PageCount);
    AssertEquals('The second page has its name', 'second', FReader.Images[1].Img.Extra[TiffPageName]);
  finally
    lSecond.Free;
  end;
end;


procedure TTestTIFFRoundTrip.TestTheBiggestImageIsRead;

var
  lSmall: TFPMemoryImage;

begin
  FImage := CreateGradientImage(9, 8);
  lSmall := CreateGradientImage(3, 2);
  try
    FWriter.AddImage(lSmall);
    FWriter.AddImage(FImage);
    FWriter.SaveToStream(FStream);
  finally
    lSmall.Free;
  end;
  FStream.Position := 0;
  FRead := TFPMemoryImage.Create(0, 0);
  FRead.LoadFromStream(FStream, FReader);
  AssertImagesEqual('The biggest of two images is read', FImage, FRead);
end;


procedure TTestTIFFRoundTrip.TestReusingTheWriterWritesOnlyTheNewImage;

var
  lFirst: TFPMemoryImage;

begin
  lFirst := CreateGradientImage(9, 8);
  try
    lFirst.SaveToStream(FStream, FWriter);
  finally
    lFirst.Free;
  end;
  FImage := CreateAlphaImage(4, 3);
  FStream.Clear;
  FImage.SaveToStream(FStream, FWriter);
  FStream.Position := 0;
  FReader.OnCreateImage := @CreateImage;
  FReader.LoadFromStream(FStream);
  AssertEquals('A writer used twice writes one image the second time', 1, FReader.ImageCount);
  AssertImagesEqual('The second write contains the second image', FImage, FReader.Images[0].Img);
end;


procedure TTestTIFFRoundTrip.TestUnsupportedBitsAreRejected;

begin
  FImage := CreateGradientImage(4, 4);
  AssertRaises('12 bits per red sample are rejected', FPImageException, @WriteTwelveBits);
end;


procedure TTestTIFFRoundTrip.TestPaletteImagesAreRejected;

begin
  FImage := CreateGradientImage(4, 4);
  AssertRaises('A palette photometric interpretation is rejected', FPImageException, @WritePalette);
end;


{ TTestTIFFStreams }

procedure TTestTIFFStreams.SetUp;

begin
  inherited SetUp;
  FReader := TFPReaderTiff.Create;
  FWriter := TFPWriterTiff.Create;
  FStream := TMemoryStream.Create;
  FImage := CreateAlphaImage(9, 7);
end;


procedure TTestTIFFStreams.TearDown;

begin
  FreeAndNil(FStream);
  FreeAndNil(FRead);
  FreeAndNil(FImage);
  FreeAndNil(FWriter);
  FreeAndNil(FReader);
  inherited TearDown;
end;


procedure TTestTIFFStreams.TestWritingAfterOtherDataKeepsIt;

var
  I: Integer;

begin
  FStream.WriteBuffer(cPrefix, SizeOf(cPrefix));
  WriteImage(FImage, FWriter, FStream);
  for I := 0 to High(cPrefix) do
    AssertEquals(Format('Byte %d before the TIFF is kept', [I]), cPrefix[I], PByte(FStream.Memory)[I]);
  FStream.Position := SizeOf(cPrefix);
  AssertTrue('The TIFF after the prefix is recognised', FReader.CheckContents(FStream));
  FRead := TFPMemoryImage.Create(0, 0);
  FRead.LoadFromStream(FStream, FReader);
  AssertImagesEqual('The TIFF after the prefix reads back', FImage, FRead);
end;


procedure TTestTIFFStreams.TestOffsetsAreRelativeToTheTiffStart;

var
  lOffset: DWord;

begin
  FStream.WriteBuffer(cPrefix, SizeOf(cPrefix));
  WriteImage(FImage, FWriter, FStream);
  Move((PByte(FStream.Memory) + SizeOf(cPrefix) + 4)^, lOffset, 4);
  AssertEquals('The first directory offset counts from the TIFF start, not the stream start', 8, LEtoN(lOffset));
end;


procedure TTestTIFFStreams.TestTheReaderDoesNotReadPastTheTiff;

var
  lSize: Int64;

begin
  lSize := WriteImage(FImage, FWriter, FStream);
  FStream.WriteBuffer(cPrefix, SizeOf(cPrefix));
  FStream.Position := 0;
  FRead := TFPMemoryImage.Create(0, 0);
  FRead.LoadFromStream(FStream, FReader);
  AssertTrue(Format('TIFF offsets are relative to its start and define no end position: ' +
    'the reader only guarantees not to consume data after the TIFF (position %d, TIFF size %d)',
    [FStream.Position, lSize]), FStream.Position <= lSize);
end;


procedure TTestTIFFStreams.TestWritingToAWriteOnlyStream;

var
  lOut: TWriteOnlyStream;
  lWriter: TFPWriterTiff;

begin
  lOut := TWriteOnlyStream.Create;
  lWriter := TFPWriterTiff.Create;
  try
    FImage.SaveToStream(lOut, lWriter, False);
    FImage.SaveToStream(FStream, FWriter);
    AssertEquals('A write-only stream gets all bytes', FStream.Size, lOut.Data.Size);
    AssertTrue('A write-only stream gets the same bytes', CompareMem(FStream.Memory, lOut.Data.Memory, FStream.Size));
  finally
    lWriter.Free;
    lOut.Free;
  end;
end;


procedure TTestTIFFStreams.TestImageSizeWithoutReading;

var
  lSize: TPoint;

begin
  FStream.WriteBuffer(cPrefix, SizeOf(cPrefix));
  WriteImage(FImage, FWriter, FStream);
  FStream.Position := SizeOf(cPrefix);
  lSize := TFPReaderTiff.ImageSize(FStream);
  AssertEquals('ImageSize gives the width', 9, lSize.X);
  AssertEquals('ImageSize gives the height', 7, lSize.Y);
  AssertEquals('ImageSize leaves the position alone', SizeOf(cPrefix), FStream.Position);
end;


{ TTestTIFFReader }

procedure TTestTIFFReader.SetUp;

begin
  inherited SetUp;
  FReader := TFPReaderTiff.Create;
  FStream := TMemoryStream.Create;
  StartTiff(False);
end;


procedure TTestTIFFReader.TearDown;

begin
  FreeAndNil(FStream);
  FreeAndNil(FRead);
  FreeAndNil(FReader);
  inherited TearDown;
end;


procedure TTestTIFFReader.StartTiff(aBigEndian: Boolean);

begin
  FBigEndian := aBigEndian;
  FTiled := False;
  SetLength(FEntries, 0);
  SetLength(FChunks, 0);
end;


procedure TTestTIFFReader.AddEntry(aTag, aType: Word; const aValues: array of DWord);

var
  I, lCount: Integer;

begin
  lCount := Length(FEntries);
  SetLength(FEntries, lCount + 1);
  FEntries[lCount].Tag := aTag;
  FEntries[lCount].Typ := aType;
  SetLength(FEntries[lCount].Values, Length(aValues));
  for I := 0 to High(aValues) do
    FEntries[lCount].Values[I] := aValues[I];
end;


procedure TTestTIFFReader.AddBasics(aWidth, aHeight, aPhotometric: DWord; const aBits: array of DWord; aCompression: DWord);

begin
  AddEntry(256, 4, [aWidth]);
  AddEntry(257, 4, [aHeight]);
  AddEntry(258, 3, aBits);
  AddEntry(262, 3, [aPhotometric]);
  AddEntry(277, 3, [Length(aBits)]);
  if aCompression > 0 then
    AddEntry(259, 3, [aCompression]);
end;


procedure TTestTIFFReader.AddChunk(const aBytes: array of Byte);

var
  lBytes: TBytes;

begin
  SetLength(lBytes, Length(aBytes));
  if Length(aBytes) > 0 then
    Move(aBytes[0], lBytes[0], Length(aBytes));
  AddChunkBytes(lBytes);
end;


procedure TTestTIFFReader.AddChunkBytes(const aBytes: TBytes);

begin
  SetLength(FChunks, Length(FChunks) + 1);
  FChunks[High(FChunks)] := aBytes;
end;


procedure TTestTIFFReader.W16(aValue: Word);

begin
  if FBigEndian then
    aValue := NtoBE(aValue)
  else
    aValue := NtoLE(aValue);
  FStream.WriteBuffer(aValue, 2);
end;


procedure TTestTIFFReader.W32(aValue: DWord);

begin
  if FBigEndian then
    aValue := NtoBE(aValue)
  else
    aValue := NtoLE(aValue);
  FStream.WriteBuffer(aValue, 4);
end;


// The number of bytes of the values of an entry.
function EntryBytes(const aEntry: TTiffTestEntry): DWord;

begin
  if aEntry.Typ = 3 then
    Result := 2 * Length(aEntry.Values)
  else
    Result := 4 * Length(aEntry.Values);
end;


// The TIFF count of an entry: rationals take two values.
function EntryCount(const aEntry: TTiffTestEntry): DWord;

begin
  if aEntry.Typ = 5 then
    Result := Length(aEntry.Values) div 2
  else
    Result := Length(aEntry.Values);
end;


procedure TTestTIFFReader.FinishTiff;

var
  I, J: Integer;
  lPos: DWord;
  lExt, lOffsets, lCounts: array of DWord;
  lEntry: TTiffTestEntry;
  lZero: DWord;

  procedure WriteValues(const aEntry: TTiffTestEntry);

  var
    K: Integer;

  begin
    for K := 0 to High(aEntry.Values) do
      if aEntry.Typ = 3 then
        W16(aEntry.Values[K])
      else
        W32(aEntry.Values[K]);
  end;

begin
  SetLength(lOffsets, Length(FChunks));
  SetLength(lCounts, Length(FChunks));
  for I := 0 to High(FChunks) do
    begin
    lOffsets[I] := 0;
    lCounts[I] := Length(FChunks[I]);
    end;
  if FTiled then
    begin
    AddEntry(324, 4, lOffsets);
    AddEntry(325, 4, lCounts);
    end
  else
    begin
    AddEntry(273, 4, lOffsets);
    AddEntry(279, 4, lCounts);
    end;
  for I := 1 to High(FEntries) do
    begin
    lEntry := FEntries[I];
    J := I - 1;
    while (J >= 0) and (FEntries[J].Tag > lEntry.Tag) do
      begin
      FEntries[J + 1] := FEntries[J];
      Dec(J);
      end;
    FEntries[J + 1] := lEntry;
    end;
  lPos := 8 + 2 + 12 * Length(FEntries) + 4;
  SetLength(lExt, Length(FEntries));
  for I := 0 to High(FEntries) do
    if EntryBytes(FEntries[I]) > 4 then
      begin
      lExt[I] := lPos;
      Inc(lPos, EntryBytes(FEntries[I]));
      if Odd(lPos) then
        Inc(lPos);
      end
    else
      lExt[I] := 0;
  for I := 0 to High(FChunks) do
    begin
    lOffsets[I] := lPos;
    Inc(lPos, Length(FChunks[I]));
    end;
  for I := 0 to High(FEntries) do
    if (FEntries[I].Tag = 273) or (FEntries[I].Tag = 324) then
      for J := 0 to High(lOffsets) do
        FEntries[I].Values[J] := lOffsets[J];
  FStream.Clear;
  if FBigEndian then
    FStream.WriteBuffer(PChar('MM')^, 2)
  else
    FStream.WriteBuffer(PChar('II')^, 2);
  W16(42);
  W32(8);
  W16(Length(FEntries));
  lZero := 0;
  for I := 0 to High(FEntries) do
    begin
    W16(FEntries[I].Tag);
    W16(FEntries[I].Typ);
    W32(EntryCount(FEntries[I]));
    if lExt[I] <> 0 then
      W32(lExt[I])
    else
      begin
      WriteValues(FEntries[I]);
      if EntryBytes(FEntries[I]) < 4 then
        FStream.WriteBuffer(lZero, 4 - EntryBytes(FEntries[I]));
      end;
    end;
  W32(0);
  for I := 0 to High(FEntries) do
    if lExt[I] <> 0 then
      begin
      WriteValues(FEntries[I]);
      if Odd(FStream.Position) then
        FStream.WriteBuffer(lZero, 1);
      end;
  for I := 0 to High(FChunks) do
    if Length(FChunks[I]) > 0 then
      FStream.WriteBuffer(FChunks[I][0], Length(FChunks[I]));
  FStream.Position := 0;
end;


procedure TTestTIFFReader.ReadIt;

begin
  FreeAndNil(FRead);
  FRead := TFPMemoryImage.Create(0, 0);
  FStream.Position := 0;
  FRead.LoadFromStream(FStream, FReader);
end;


procedure TTestTIFFReader.CheckColor(const aMessage: String; aX, aY: Integer; const aColor: TFPColor; aTolerance: Word);

begin
  AssertColorsEqual(aMessage, aColor, FRead[aX, aY], aTolerance);
end;


procedure TTestTIFFReader.WriteSample;

var
  lImage: TFPMemoryImage;
  lWriter: TFPWriterTiff;

begin
  lImage := CreateGradientImage(8, 8);
  lWriter := TFPWriterTiff.Create;
  try
    FStream.Clear;
    lImage.SaveToStream(FStream, lWriter);
    FStream.Position := 0;
  finally
    lWriter.Free;
    lImage.Free;
  end;
end;


procedure TTestTIFFReader.BuildOrientation(aOrientation: DWord);

begin
  StartTiff(False);
  AddBasics(3, 2, 1, [8]);
  AddEntry(274, 3, [aOrientation]);
  AddChunk([10, 20, 30, 40, 50, 60]);
  FinishTiff;
end;


procedure TTestTIFFReader.TestLittleEndianRGB;

begin
  AddBasics(2, 2, 2, [8, 8, 8]);
  AddChunk([10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120]);
  FinishTiff;
  ReadIt;
  AssertEquals('The width is read', 2, FRead.Width);
  AssertEquals('The height is read', 2, FRead.Height);
  CheckColor('Pixel (0,0)', 0, 0, RGB8(10, 20, 30));
  CheckColor('Pixel (1,0)', 1, 0, RGB8(40, 50, 60));
  CheckColor('Pixel (0,1)', 0, 1, RGB8(70, 80, 90));
  CheckColor('Pixel (1,1)', 1, 1, RGB8(100, 110, 120));
end;


procedure TTestTIFFReader.TestBigEndianRGB;

begin
  StartTiff(True);
  AddBasics(2, 2, 2, [8, 8, 8]);
  AddChunk([10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120]);
  FinishTiff;
  ReadIt;
  AssertEquals('The width of a big-endian TIFF is read', 2, FRead.Width);
  AssertEquals('The height of a big-endian TIFF is read', 2, FRead.Height);
  CheckColor('Big-endian pixel (0,0)', 0, 0, RGB8(10, 20, 30));
  CheckColor('Big-endian pixel (1,0)', 1, 0, RGB8(40, 50, 60));
  CheckColor('Big-endian pixel (0,1)', 0, 1, RGB8(70, 80, 90));
  CheckColor('Big-endian pixel (1,1)', 1, 1, RGB8(100, 110, 120));
end;


procedure TTestTIFFReader.Test16BitSamplesInBothByteOrders;

begin
  StartTiff(True);
  AddBasics(1, 1, 2, [16, 16, 16]);
  AddChunk([$12, $34, $56, $78, $9A, $BC]);
  FinishTiff;
  ReadIt;
  CheckColor('Big-endian 16-bit samples', 0, 0, FPColor($1234, $5678, $9ABC));
  StartTiff(False);
  AddBasics(1, 1, 2, [16, 16, 16]);
  AddChunk([$34, $12, $78, $56, $BC, $9A]);
  FinishTiff;
  ReadIt;
  CheckColor('Little-endian 16-bit samples', 0, 0, FPColor($1234, $5678, $9ABC));
end;


procedure TTestTIFFReader.TestSeveralStrips;

var
  lX, lY: Integer;

begin
  AddBasics(3, 5, 1, [8]);
  AddEntry(278, 3, [2]);
  AddChunk([0, 1, 2, 10, 11, 12]);
  AddChunk([20, 21, 22, 30, 31, 32]);
  AddChunk([40, 41, 42]);
  FinishTiff;
  ReadIt;
  for lY := 0 to 4 do
    for lX := 0 to 2 do
      CheckColor(Format('Pixel (%d,%d) of three strips', [lX, lY]), lX, lY, Gray8(lY * 10 + lX));
end;


procedure TTestTIFFReader.TestNoRowsPerStripIsOneStrip;

var
  lX, lY: Integer;

begin
  AddBasics(3, 3, 1, [8]);
  AddChunk([0, 1, 2, 10, 11, 12, 20, 21, 22]);
  FinishTiff;
  ReadIt;
  for lY := 0 to 2 do
    for lX := 0 to 2 do
      CheckColor(Format('Pixel (%d,%d) of a single strip', [lX, lY]), lX, lY, Gray8(lY * 10 + lX));
end;


procedure TTestTIFFReader.TestNoCompressionTagMeansNone;

begin
  AddBasics(3, 1, 1, [8], 0);
  AddChunk([10, 20, 30]);
  FinishTiff;
  ReadIt;
  CheckColor('Without a compression tag pixel 0 is read uncompressed', 0, 0, Gray8(10));
  CheckColor('Without a compression tag pixel 2 is read uncompressed', 2, 0, Gray8(30));
end;


procedure TTestTIFFReader.TestPackBits;

const
  cRow0: array[0..7] of Byte = (1, 2, 3, 9, 9, 9, 9, 9);

var
  lX: Integer;

begin
  AddBasics(8, 2, 1, [8], TiffCompressionPackBits);
  { literal of 3, run of 5 nines, no-operation byte, run of 8 sevens }
  AddChunk([2, 1, 2, 3, $FC, 9, $80, $F9, 7]);
  FinishTiff;
  ReadIt;
  for lX := 0 to 7 do
    CheckColor(Format('PackBits pixel (%d,0)', [lX]), lX, 0, Gray8(cRow0[lX]));
  for lX := 0 to 7 do
    CheckColor(Format('PackBits pixel (%d,1)', [lX]), lX, 1, Gray8(7));
end;


procedure TTestTIFFReader.TestACutPackBitsLiteralRaises;

begin
  AddBasics(8, 1, 1, [8], TiffCompressionPackBits);
  { a literal of 8 bytes with only 3 present }
  AddChunk([7, 1, 2, 3]);
  FinishTiff;
  AssertRaises('A PackBits literal cut short raises', FPImageException, @ReadIt);
end;


procedure TTestTIFFReader.TestLZWByHand;

begin
  AddBasics(4, 1, 1, [8], TiffCompressionLZW);
  { 9-bit codes: clear, 7, 258 (7 7), 8, end of information }
  AddChunk([$80, $01, $E0, $40, $88, $08]);
  FinishTiff;
  ReadIt;
  CheckColor('LZW pixel 0', 0, 0, Gray8(7));
  CheckColor('LZW pixel 1', 1, 0, Gray8(7));
  CheckColor('LZW pixel 2', 2, 0, Gray8(7));
  CheckColor('LZW pixel 3', 3, 0, Gray8(8));
end;


procedure TTestTIFFReader.TestLZWCodeWidthsAndClearCode;

const
  cWidth = 128;
  cHeight = 96;

var
  lData: array of Byte;
  lSeed: DWord;
  I: Integer;

begin
  SetLength(lData, cWidth * cHeight);
  lSeed := 12345;
  for I := 0 to High(lData) do
    if I < 300 then
      lData[I] := 77
    else
      begin
      lSeed := (QWord(lSeed) * 1103515245 + 12345) and $FFFFFFFF;
      lData[I] := (lSeed shr 16) and $FF;
      end;
  AddBasics(cWidth, cHeight, 1, [8], TiffCompressionLZW);
  AddChunkBytes(EncodeLZW(lData));
  FinishTiff;
  ReadIt;
  for I := 0 to High(lData) do
    if FRead[I mod cWidth, I div cWidth].Red <> lData[I] * 257 then
      Fail(Format('LZW with 10, 11 and 12-bit codes and table resets: pixel (%d,%d) expected %d, got %d',
        [I mod cWidth, I div cWidth, lData[I], FRead[I mod cWidth, I div cWidth].Red shr 8]));
end;


procedure TTestTIFFReader.TestLZWInABigEndianFile;

const
  cData: array[0..11] of Byte = (5, 5, 5, 5, 6, 6, 5, 5, 6, 6, 6, 5);

var
  I: Integer;

begin
  StartTiff(True);
  AddBasics(12, 1, 1, [8], TiffCompressionLZW);
  AddChunkBytes(EncodeLZW(cData));
  FinishTiff;
  ReadIt;
  for I := 0 to High(cData) do
    CheckColor(Format('LZW pixel %d of a big-endian file', [I]), I, 0, Gray8(cData[I]));
end;


procedure TTestTIFFReader.TestLZWWithPredictor;

var
  lData: array[0..16 * 4 * 3 - 1] of Byte;
  lExpected: TFPMemoryImage;
  lX, lY, C: Integer;
  lLast, lValue: array[0..2] of Byte;

begin
  lExpected := CreateGradientImage(16, 4);
  try
    for lY := 0 to 3 do
      begin
      for C := 0 to 2 do
        lLast[C] := 0;
      for lX := 0 to 15 do
        begin
        lValue[0] := lExpected[lX, lY].Red shr 8;
        lValue[1] := lExpected[lX, lY].Green shr 8;
        lValue[2] := lExpected[lX, lY].Blue shr 8;
        for C := 0 to 2 do
          begin
          lData[(lY * 16 + lX) * 3 + C] := Byte(lValue[C] - lLast[C]);
          lLast[C] := lValue[C];
          end;
        end;
      end;
    AddBasics(16, 4, 2, [8, 8, 8], TiffCompressionLZW);
    AddEntry(317, 3, [2]);
    AddChunkBytes(EncodeLZW(lData));
    FinishTiff;
    ReadIt;
    AssertImagesEqual('LZW with horizontal differencing gives the colours back', lExpected, FRead);
  finally
    lExpected.Free;
  end;
end;


procedure TTestTIFFReader.TestTiles;

var
  lTile: array[0..255] of Byte;
  lTileX, lTileY, lX, lY: Integer;

begin
  AddBasics(24, 20, 1, [8]);
  AddEntry(322, 3, [16]);
  AddEntry(323, 3, [16]);
  FTiled := True;
  for lTileY := 0 to 1 do
    for lTileX := 0 to 1 do
      begin
      for lY := 0 to 15 do
        for lX := 0 to 15 do
          if (lTileX * 16 + lX < 24) and (lTileY * 16 + lY < 20) then
            lTile[lY * 16 + lX] := ((lTileX * 16 + lX) * 7 + (lTileY * 16 + lY) * 11) and $FF
          else
            lTile[lY * 16 + lX] := 255;
      AddChunk(lTile);
      end;
  FinishTiff;
  ReadIt;
  AssertEquals('A tiled image has its width', 24, FRead.Width);
  AssertEquals('A tiled image has its height', 20, FRead.Height);
  for lY := 0 to 19 do
    for lX := 0 to 23 do
      CheckColor(Format('Tiled pixel (%d,%d)', [lX, lY]), lX, lY, Gray8((lX * 7 + lY * 11) and $FF));
end;


procedure TTestTIFFReader.TestPlanarConfigurationWithOneSample;

begin
  AddBasics(3, 2, 1, [8]);
  AddEntry(284, 3, [TiffPlanarConfigurationPlanar]);
  AddChunk([10, 20, 30, 40, 50, 60]);
  FinishTiff;
  ReadIt;
  CheckColor('With one sample per pixel the planar configuration is irrelevant, pixel (0,0)', 0, 0, Gray8(10));
  CheckColor('With one sample per pixel the planar configuration is irrelevant, pixel (2,1)', 2, 1, Gray8(60));
end;


procedure TTestTIFFReader.TestPlanarConfigurationWithThreeSamples;

var
  lRead: Boolean;

begin
  AddBasics(2, 2, 2, [8, 8, 8]);
  AddEntry(284, 3, [TiffPlanarConfigurationPlanar]);
  AddChunk([10, 11, 12, 13]);
  AddChunk([20, 21, 22, 23]);
  AddChunk([30, 31, 32, 33]);
  FinishTiff;
  lRead := False;
  try
    ReadIt;
    lRead := True;
  except
    on E: Exception do
      AssertTrue('A planar image that is not supported raises FPImageException, got ' + E.ClassName,
        E is FPImageException);
  end;
  if lRead then
    begin
    CheckColor('Planar pixel (0,0)', 0, 0, RGB8(10, 20, 30));
    CheckColor('Planar pixel (1,0)', 1, 0, RGB8(11, 21, 31));
    CheckColor('Planar pixel (0,1)', 0, 1, RGB8(12, 22, 32));
    CheckColor('Planar pixel (1,1)', 1, 1, RGB8(13, 23, 33));
    end;
end;


procedure TTestTIFFReader.TestPredictor8Bit;

begin
  AddBasics(3, 2, 2, [8, 8, 8]);
  AddEntry(317, 3, [2]);
  AddChunk([10, 20, 30, 5, 5, 5, 246, 225, 5,
            200, 100, 0, 156, 100, 255, 156, 56, 1]);
  FinishTiff;
  ReadIt;
  CheckColor('Predictor pixel (0,0)', 0, 0, RGB8(10, 20, 30));
  CheckColor('Predictor pixel (1,0)', 1, 0, RGB8(15, 25, 35));
  CheckColor('Predictor pixel (2,0) wraps around', 2, 0, RGB8(5, 250, 40));
  CheckColor('Predictor pixel (0,1) starts again', 0, 1, RGB8(200, 100, 0));
  CheckColor('Predictor pixel (1,1)', 1, 1, RGB8(100, 200, 255));
  CheckColor('Predictor pixel (2,1)', 2, 1, RGB8(0, 0, 0));
end;


procedure TTestTIFFReader.TestPredictor16Bit;

begin
  AddBasics(3, 1, 1, [16]);
  AddEntry(317, 3, [2]);
  { 1000, -100 and 64100 as little-endian words }
  AddChunk([$E8, $03, $9C, $FF, $64, $FA]);
  FinishTiff;
  ReadIt;
  CheckColor('16-bit predictor pixel 0', 0, 0, FPColor(1000, 1000, 1000));
  CheckColor('16-bit predictor pixel 1', 1, 0, FPColor(900, 900, 900));
  CheckColor('16-bit predictor pixel 2', 2, 0, FPColor(65000, 65000, 65000));
end;


procedure TTestTIFFReader.TestPalette8Bit;

const
  cIndexes: array[0..3] of Byte = (0, 1, 128, 255);

var
  lMap: array of DWord;
  I: Integer;

begin
  SetLength(lMap, 768);
  for I := 0 to 255 do
    begin
    lMap[I] := I * 257;
    lMap[256 + I] := (255 - I) * 257;
    lMap[512 + I] := ((I * 3) and $FF) * 257;
    end;
  AddBasics(4, 1, 3, [8]);
  AddEntry(320, 3, lMap);
  AddChunk(cIndexes);
  FinishTiff;
  ReadIt;
  for I := 0 to 3 do
    CheckColor(Format('8-bit palette pixel %d', [I]), I, 0,
      RGB8(cIndexes[I], 255 - cIndexes[I], (cIndexes[I] * 3) and $FF));
end;


procedure TTestTIFFReader.TestPalette4Bit;

const
  cIndexes: array[0..4] of Byte = (0, 5, 10, 15, 3);

var
  lMap: array of DWord;
  I: Integer;

begin
  SetLength(lMap, 48);
  for I := 0 to 15 do
    begin
    lMap[I] := I * 17 * 257;
    lMap[16 + I] := (15 - I) * 17 * 257;
    lMap[32 + I] := 100 * 257;
    end;
  AddBasics(5, 1, 3, [4]);
  AddEntry(320, 3, lMap);
  AddChunk([$05, $AF, $30]);
  FinishTiff;
  ReadIt;
  for I := 0 to 4 do
    CheckColor(Format('4-bit palette pixel %d', [I]), I, 0,
      RGB8(cIndexes[I] * 17, (15 - cIndexes[I]) * 17, 100));
end;


procedure TTestTIFFReader.TestPalette1Bit;

const
  cBits: array[0..9] of Byte = (1, 0, 1, 1, 0, 0, 1, 1, 1, 0);

var
  I: Integer;

begin
  AddBasics(10, 1, 3, [1]);
  AddEntry(320, 3, [$FFFF, 0, 0, 0, 0, $FFFF]);
  AddChunk([$B3, $80]);
  FinishTiff;
  ReadIt;
  for I := 0 to 9 do
    if cBits[I] = 1 then
      CheckColor(Format('1-bit palette pixel %d is entry 1', [I]), I, 0, colBlue)
    else
      CheckColor(Format('1-bit palette pixel %d is entry 0', [I]), I, 0, colRed);
end;


procedure TTestTIFFReader.TestBilevelBlackIsZero;

const
  cRow0: array[0..9] of Byte = (1, 0, 1, 1, 0, 0, 1, 1, 1, 0);
  cRow1: array[0..9] of Byte = (0, 0, 0, 0, 1, 1, 1, 1, 1, 1);

var
  I: Integer;

begin
  AddBasics(10, 2, 1, [1]);
  AddChunk([$B3, $80, $0F, $C0]);
  FinishTiff;
  ReadIt;
  for I := 0 to 9 do
    begin
    if cRow0[I] = 1 then
      CheckColor(Format('Black-is-zero bit 1 at (%d,0) is white', [I]), I, 0, colWhite)
    else
      CheckColor(Format('Black-is-zero bit 0 at (%d,0) is black', [I]), I, 0, colBlack);
    if cRow1[I] = 1 then
      CheckColor(Format('Black-is-zero bit 1 at (%d,1) is white', [I]), I, 1, colWhite)
    else
      CheckColor(Format('Black-is-zero bit 0 at (%d,1) is black', [I]), I, 1, colBlack);
    end;
end;


procedure TTestTIFFReader.TestBilevelWhiteIsZero;

const
  cRow0: array[0..9] of Byte = (1, 0, 1, 1, 0, 0, 1, 1, 1, 0);

var
  I: Integer;

begin
  AddBasics(10, 1, 0, [1]);
  AddChunk([$B3, $80]);
  FinishTiff;
  ReadIt;
  for I := 0 to 9 do
    if cRow0[I] = 1 then
      CheckColor(Format('White-is-zero bit 1 at %d is black', [I]), I, 0, colBlack)
    else
      CheckColor(Format('White-is-zero bit 0 at %d is white', [I]), I, 0, colWhite);
end;


procedure TTestTIFFReader.TestBilevelFillOrder2;

const
  cRow0: array[0..9] of Byte = (1, 0, 1, 1, 0, 0, 1, 1, 1, 0);

var
  I: Integer;

begin
  AddBasics(10, 1, 1, [1]);
  AddEntry(266, 3, [2]);
  AddChunk([$CD, $01]);
  FinishTiff;
  ReadIt;
  for I := 0 to 9 do
    if cRow0[I] = 1 then
      CheckColor(Format('Fill order 2 bit 1 at %d is white', [I]), I, 0, colWhite)
    else
      CheckColor(Format('Fill order 2 bit 0 at %d is black', [I]), I, 0, colBlack);
end;


procedure TTestTIFFReader.TestGray4Bit;

begin
  AddBasics(3, 1, 1, [4]);
  AddChunk([$08, $F0]);
  FinishTiff;
  ReadIt;
  CheckColor('4-bit level 0 is black', 0, 0, colBlack);
  CheckColor('4-bit level 8', 1, 0, FPColor($8888, $8888, $8888));
  CheckColor('4-bit level 15 is white', 2, 0, colWhite);
end;


procedure TTestTIFFReader.TestGray12Bit;

begin
  AddBasics(2, 1, 1, [12]);
  AddChunk([$AB, $CD, $EF]);
  FinishTiff;
  ReadIt;
  CheckColor('12-bit level $ABC', 0, 0, FPColor($ABCA, $ABCA, $ABCA));
  CheckColor('12-bit level $DEF', 1, 0, FPColor($DEFD, $DEFD, $DEFD));
end;


procedure TTestTIFFReader.TestUnassociatedAlpha;

begin
  AddBasics(2, 1, 2, [8, 8, 8, 8]);
  AddEntry(338, 3, [2]);
  AddChunk([10, 20, 30, 40, 50, 60, 70, 255]);
  FinishTiff;
  ReadIt;
  CheckColor('Unassociated alpha is kept with the colour', 0, 0, RGB8(10, 20, 30, 40));
  CheckColor('Opaque unassociated alpha', 1, 0, RGB8(50, 60, 70, 255));
end;


procedure TTestTIFFReader.TestAssociatedAlpha;

begin
  AddBasics(2, 1, 2, [8, 8, 8, 8]);
  AddEntry(338, 3, [1]);
  AddChunk([64, 32, 0, 128, 10, 20, 30, 255]);
  FinishTiff;
  ReadIt;
  CheckColor('Premultiplied colours are divided by alpha', 0, 0, RGB8(128, 64, 0, 128), 257);
  CheckColor('Opaque premultiplied colours are kept', 1, 0, RGB8(10, 20, 30, 255));
end;


procedure TTestTIFFReader.TestMirroredOrientations;

var
  lOrientation, lC, lR, lX, lY: Integer;

begin
  for lOrientation := 1 to 4 do
    begin
    BuildOrientation(lOrientation);
    ReadIt;
    AssertEquals(Format('Orientation %d keeps the width', [lOrientation]), 3, FRead.Width);
    AssertEquals(Format('Orientation %d keeps the height', [lOrientation]), 2, FRead.Height);
    for lR := 0 to 1 do
      for lC := 0 to 2 do
        begin
        case lOrientation of
          1: begin lX := lC; lY := lR; end;
          2: begin lX := 2 - lC; lY := lR; end;
          3: begin lX := 2 - lC; lY := 1 - lR; end;
        else
          begin lX := lC; lY := 1 - lR; end;
        end;
        CheckColor(Format('Orientation %d shows stored (%d,%d) at (%d,%d)', [lOrientation, lC, lR, lX, lY]),
          lX, lY, Gray8(10 * (lR * 3 + lC + 1)));
        end;
    end;
end;


procedure TTestTIFFReader.TestRotatedOrientations;

var
  lOrientation, lC, lR, lX, lY: Integer;

begin
  for lOrientation := 5 to 8 do
    begin
    BuildOrientation(lOrientation);
    ReadIt;
    AssertEquals(Format('Orientation %d swaps the width', [lOrientation]), 2, FRead.Width);
    AssertEquals(Format('Orientation %d swaps the height', [lOrientation]), 3, FRead.Height);
    for lR := 0 to 1 do
      for lC := 0 to 2 do
        begin
        case lOrientation of
          5: begin lX := lR; lY := lC; end;
          6: begin lX := 1 - lR; lY := lC; end;
          7: begin lX := 1 - lR; lY := 2 - lC; end;
        else
          begin lX := lR; lY := 2 - lC; end;
        end;
        CheckColor(Format('Orientation %d shows stored (%d,%d) at (%d,%d)', [lOrientation, lC, lR, lX, lY]),
          lX, lY, Gray8(10 * (lR * 3 + lC + 1)));
        end;
    end;
end;


procedure TTestTIFFReader.TestTheResolutionIsRead;

begin
  AddBasics(1, 1, 1, [8]);
  AddEntry(282, 5, [1181, 10]);
  AddEntry(283, 5, [590, 10]);
  AddEntry(296, 3, [3]);
  AddChunk([0]);
  FinishTiff;
  ReadIt;
  AssertTrue('Resolution unit 3 is pixels per centimeter', FRead.ResolutionUnit = ruPixelsPerCentimeter);
  AssertEquals('The horizontal resolution is the rational', 118.1, FRead.ResolutionX, 0.001);
  AssertEquals('The vertical resolution is the rational', 59, FRead.ResolutionY, 0.001);
end;


procedure TTestTIFFReader.TestSampleValueTagsDoNotLeakIntoTheNextRead;

var
  lImage: TFPMemoryImage;
  lWriter: TFPWriterTiff;

begin
  AddBasics(1, 1, 1, [8]);
  AddEntry(280, 3, [10]);
  AddEntry(281, 3, [200]);
  AddChunk([100]);
  FinishTiff;
  ReadIt;
  lImage := CreateGradientImage(5, 4);
  lWriter := TFPWriterTiff.Create;
  try
    lImage.Extra[TiffSampleFormat] := IntToStr(TiffSampleFormatIEEEFloat);
    lImage.Extra[TiffRedBits] := '32';
    lImage.Extra[TiffGreenBits] := '32';
    lImage.Extra[TiffBlueBits] := '32';
    FStream.Clear;
    lImage.SaveToStream(FStream, lWriter);
    ReadIt;
    AssertImagesEqual('A float TIFF read after a TIFF with sample value tags keeps its colours', lImage, FRead, 0, True);
  finally
    lWriter.Free;
    lImage.Free;
  end;
end;


procedure TTestTIFFReader.TestTheFloatExample;

begin
  if not FileExists(ExampleFile('float-tiff.tif')) then
    Fail('Example not found: ' + ExampleFile('float-tiff.tif') + ' (run from packages/fcl-image)');
  FRead := TFPMemoryImage.Create(0, 0);
  FRead.LoadFromFile(ExampleFile('float-tiff.tif'), FReader);
  AssertEquals('The float example is 339 pixels wide', 339, FRead.Width);
  AssertEquals('The float example is 291 pixels high', 291, FRead.Height);
  AssertEquals('The float example reports float samples', IntToStr(TiffSampleFormatIEEEFloat), FRead.Extra[TiffSampleFormat]);
  AssertEquals('The float example has 32-bit red samples', '32', FRead.Extra[TiffRedBits]);
  AssertTrue('The float example is in pixels per inch', FRead.ResolutionUnit = ruPixelsPerInch);
  AssertEquals('The float example has 87.7 pixels per inch', 87.7, FRead.ResolutionX, 0.001);
  CheckColor('Float sample 0.0667 at the top left corner', 0, 0, FPColor(4369, 4369, 4369), 1);
  CheckColor('Float samples at the centre', 169, 145, FPColor(63270, 53472, 193), 1);
  CheckColor('Float samples at the bottom right corner', 338, 290, FPColor(2660, 3277, 3071), 1);
  CheckColor('Float samples at the lower left quarter', 84, 218, FPColor(42052, 41024, 40253), 1);
  AssertEquals('The float example without alpha is opaque', alphaOpaque, FRead[10, 10].Alpha);
end;


procedure TTestTIFFReader.TestAMissingPhotometricInterpretationRaises;

begin
  AddEntry(256, 4, [2]);
  AddEntry(257, 4, [1]);
  AddEntry(258, 3, [8]);
  AddEntry(259, 3, [TiffCompressionNone]);
  AddEntry(277, 3, [1]);
  AddChunk([1, 2]);
  FinishTiff;
  AssertRaises('A TIFF without photometric interpretation raises', FPImageException, @ReadIt);
end;


procedure TTestTIFFReader.TestAnUnsupportedCompressionRaises;

begin
  AddBasics(2, 1, 1, [8], TiffCompressionCCITTFAX4);
  AddChunk([1, 2]);
  FinishTiff;
  AssertRaises('A compression the reader does not know raises', FPImageException, @ReadIt);
end;


procedure TTestTIFFReader.TestACutDirectoryRaises;

begin
  WriteSample;
  FStream.Size := 20;
  AssertRaises('A TIFF cut in its directory raises', FPImageException, @ReadIt);
end;


procedure TTestTIFFReader.TestACutStripRaises;

begin
  WriteSample;
  FStream.Size := FStream.Size - 5;
  AssertRaises('A TIFF cut in its strip raises', FPImageException, @ReadIt);
end;


procedure TTestTIFFReader.TestADirectoryOutsideTheStreamRaises;

begin
  FreeAndNil(FStream);
  FStream := BytesStream([Ord('I'), Ord('I'), 42, 0, $FF, 0, 0, 0, 0, 0]);
  AssertRaises('A directory offset outside the stream raises', FPImageException, @ReadIt);
end;


procedure TTestTIFFReader.TestContentsCheck;

begin
  FreeAndNil(FStream);
  FStream := BytesStream([0, 0, Ord('I'), Ord('I'), 42, 0, 8, 0, 0, 0]);
  FStream.Position := 2;
  AssertTrue('A little-endian header is accepted', FReader.CheckContents(FStream));
  AssertEquals('The check leaves the position alone', 2, FStream.Position);
  FreeAndNil(FStream);
  FStream := BytesStream([Ord('M'), Ord('M'), 0, 42, 0, 0, 0, 8]);
  AssertTrue('A big-endian header is accepted', FReader.CheckContents(FStream));
  FreeAndNil(FStream);
  FStream := BytesStream([Ord('I'), Ord('X'), 42, 0, 8, 0, 0, 0]);
  AssertFalse('Another byte order mark is rejected', FReader.CheckContents(FStream));
  FreeAndNil(FStream);
  FStream := BytesStream([Ord('I'), Ord('I'), 41, 0, 8, 0, 0, 0]);
  AssertFalse('Another version is rejected', FReader.CheckContents(FStream));
  FreeAndNil(FStream);
  FStream := BytesStream([Ord('I'), Ord('I'), 42, 0, 0, 0, 0, 0]);
  AssertFalse('A first directory at offset 0 is rejected', FReader.CheckContents(FStream));
end;


procedure TTestTIFFReader.TestACutHeaderIsRejected;

begin
  FreeAndNil(FStream);
  FStream := BytesStream([Ord('I'), Ord('I'), 42, 0]);
  AssertFalse('A header without the directory offset is rejected', FReader.CheckContents(FStream));
end;


initialization
  RegisterTests('tiff', [TTestTIFFRoundTrip, TTestTIFFStreams, TTestTIFFReader]);
end.
