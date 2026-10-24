{
    Tests for the BMP reader and writer: round trips at every depth, the
    headers written, stream handling and hand-built files of other variants.
    See the file COPYING.FPC, included in this distribution, for details.
}
unit tcbmp;

{$mode objfpc}{$H+}

interface

uses sysutils, classes, fpcunit, testregistry, fpimage, fpimgtests,
     bmpcomn, fpreadbmp, fpwritebmp;

type
  TTestBMPRoundTrip = class(TTestCase)
  private
    FWriter: TFPWriterBMP;
    FReader: TFPReaderBMP;
    FImage: TFPMemoryImage;
    FRead: TFPMemoryImage;
    // Writes FImage and reads it back into FRead.
    procedure RoundTrip;
    // Replaces FImage.
    procedure SetImage(aImage: TFPMemoryImage);
    // An image in palette mode with runs and single pixels of aCount colours.
    function CreateRunsImage(aWidth, aHeight, aCount: Integer): TFPMemoryImage;
    procedure WriteRLE24;
    procedure WriteIndexedWithoutPalette;
    procedure WriteTooManyColors;
    procedure SetDepth7;
    // Round trips RLE4 images of every other width from aFirst to 13.
    procedure CheckRLE4Widths(aFirst: Integer);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test24BitKeepsTheColors;
    procedure Test24BitPadsEveryWidth;
    procedure Test32BitKeepsOpaqueColors;
    procedure Test32BitKeepsAlpha;
    procedure Test16BitKeepsFiveSixFiveColors;
    procedure Test16BitStaysClose;
    procedure Test16BitWhiteStaysWhite;
    procedure Test15BitWhiteStaysWhite;
    procedure Test8BitKeepsThePalette;
    procedure Test4BitKeepsThePalette;
    procedure Test1BitKeepsThePalette;
    procedure TestRLE8KeepsThePixels;
    procedure TestRLE4KeepsThePixels;
    procedure TestRLE8TallAndNarrow;
    procedure TestRLE4OfEvenWidth;
    procedure TestRLE4OfOddWidth;
    procedure TestRLEWithTrueColorIsRejected;
    procedure TestIndexedWithoutPaletteIsRejected;
    procedure TestPaletteTooLargeForTheDepthIsRejected;
    procedure TestAnInvalidDepthIsRejected;
  end;

  TTestBMPHeader = class(TTestCase)
  private
    FImage: TFPMemoryImage;
    FWriter: TFPWriterBMP;
    FStream: TMemoryStream;
    procedure WriteIt;
    function LongAt(aOffset: Integer): LongInt;
    function WordAt(aOffset: Integer): Word;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTheFileHeader;
    procedure TestTheInfoHeader;
    procedure TestThePaletteFollowsTheHeader;
    procedure TestTranslucentImagesGetAV5HeaderWithAlphaMask;
    procedure TestOpaqueImagesKeepTheShortHeader;
    procedure TestACompressedImageGivesItsSize;
    procedure TestTheResolutionIsWritten;
    procedure TestTheWriterLeavesTheImageAlone;
    procedure TestTheResolutionIsRead;
  end;

  TTestBMPStreams = class(TTestCase)
  private
    FImage: TFPMemoryImage;
    FWriter: TFPWriterBMP;
    FReader: TFPReaderBMP;
    FStream: TMemoryStream;
    // Writes after a prefix, then reads back from after the prefix.
    procedure CheckAfterPrefix(const aMessage: String);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestWritingAfterOtherDataKeepsIt;
    procedure TestWritingCompressedAfterOtherDataKeepsIt;
    procedure TestTwoImagesInOneStream;
    procedure TestTheReaderStopsAtTheEndOfTheImage;
    procedure TestWritingToAWriteOnlyStream;
    procedure TestImageSizeWithoutReading;
  end;

  TTestBMPReader = class(TTestCase)
  private
    FReader: TFPReaderBMP;
    FImage: TFPMemoryImage;
    FStream: TMemoryStream;
    // Builds a BMP file: header of aHeaderSize (extra header bytes after the first 40),
    // then the table (palette or masks), aGap zero bytes and the pixel bytes.
    procedure Build(aHeaderSize, aWidth, aHeight, aBitCount, aCompression, aClrUsed: Integer;
      const aHeaderExtra, aTable, aPixels: array of Byte; aGap: Integer = 0);
    procedure ReadIt;
    procedure ReadTruncated;
    procedure ReadZeroWidth;
    procedure CheckColor(const aMessage: String; aX, aY: Integer; const aColor: TFPColor);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestATopDownImage;
    procedure TestImageSizeOfATopDownImageIsPositive;
    procedure TestBitfields32;
    procedure TestBitfields16;
    procedure TestAV4HeaderWithTheMasksInside;
    procedure TestAV5Header;
    procedure TestAV5HeaderWithAnAlphaMask;
    procedure TestAnOS2Header;
    procedure TestAnOS2PaletteHasThreeBytesPerEntry;
    procedure TestThePixelsStartAtTheirOffset;
    procedure TestAPaletteShorterThanTheDepth;
    procedure TestRLE8Delta;
    procedure TestRLE8EndOfBitmap;
    procedure Test32BitWithZeroReservedByteIsOpaque;
    procedure Test32BitWithFullAlphaIsOpaque;
    procedure TestATruncatedFileRaises;
    procedure TestAZeroWidthRaises;
    procedure TestContentsCheckRejectsOtherData;
  end;

implementation

const
  cPrefix: array[0..9] of Byte = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10);

{ TTestBMPRoundTrip }

procedure TTestBMPRoundTrip.SetUp;

begin
  inherited SetUp;
  FWriter := TFPWriterBMP.Create;
  FReader := TFPReaderBMP.Create;
end;


procedure TTestBMPRoundTrip.TearDown;

begin
  FreeAndNil(FRead);
  FreeAndNil(FImage);
  FreeAndNil(FReader);
  FreeAndNil(FWriter);
  inherited TearDown;
end;


procedure TTestBMPRoundTrip.RoundTrip;

begin
  FreeAndNil(FRead);
  FRead := fpimgtests.RoundTrip(FImage, FWriter, FReader);
end;


procedure TTestBMPRoundTrip.SetImage(aImage: TFPMemoryImage);

begin
  FreeAndNil(FImage);
  FImage := aImage;
end;


function TTestBMPRoundTrip.CreateRunsImage(aWidth, aHeight, aCount: Integer): TFPMemoryImage;

var
  lX, lY, lIndex: Integer;

begin
  Result := TFPMemoryImage.Create(aWidth, aHeight);
  Result.UsePalette := True;
  Result.Palette.Clear;
  for lIndex := 0 to aCount - 1 do
    Result.Palette.Add(RGB8((lIndex * 53) and $FF, (lIndex * 91) and $FF, (lIndex * 17 + 40) and $FF));
  for lY := 0 to aHeight - 1 do
    for lX := 0 to aWidth - 1 do
      begin
      if lX < aWidth div 2 then
        lIndex := (lY div 2) mod aCount
      else if Odd(lY) then
        lIndex := (lX * 7) mod aCount
      else
        lIndex := ((lX div 3) + lY) mod aCount;
      Result.Pixels[lX, lY] := lIndex;
      end;
end;


procedure TTestBMPRoundTrip.WriteRLE24;

begin
  FWriter.BitsPerPixel := 24;
  FWriter.RLECompress := True;
  RoundTrip;
end;


procedure TTestBMPRoundTrip.WriteIndexedWithoutPalette;

begin
  FWriter.BitsPerPixel := 8;
  RoundTrip;
end;


procedure TTestBMPRoundTrip.WriteTooManyColors;

begin
  FWriter.BitsPerPixel := 4;
  RoundTrip;
end;


procedure TTestBMPRoundTrip.SetDepth7;

begin
  FWriter.BitsPerPixel := 7;
end;


procedure TTestBMPRoundTrip.Test24BitKeepsTheColors;

begin
  SetImage(CreateGradientImage(17, 11));
  FWriter.BitsPerPixel := 24;
  RoundTrip;
  AssertImagesEqual('A 24-bit BMP keeps every colour', FImage, FRead);
end;


procedure TTestBMPRoundTrip.Test24BitPadsEveryWidth;

var
  lWidth: Integer;

begin
  FWriter.BitsPerPixel := 24;
  for lWidth := 1 to 9 do
    begin
    SetImage(CreateGradientImage(lWidth, 3));
    RoundTrip;
    AssertImagesEqual(Format('A 24-bit BMP of width %d', [lWidth]), FImage, FRead);
    end;
end;


procedure TTestBMPRoundTrip.Test32BitKeepsOpaqueColors;

begin
  SetImage(CreateGradientImage(9, 4));
  FWriter.BitsPerPixel := 32;
  RoundTrip;
  AssertImagesEqual('A 32-bit BMP keeps opaque colours opaque', FImage, FRead);
end;


procedure TTestBMPRoundTrip.Test32BitKeepsAlpha;

begin
  SetImage(CreateAlphaImage(9, 4));
  FWriter.BitsPerPixel := 32;
  RoundTrip;
  AssertImagesEqual('A 32-bit BMP keeps alpha', FImage, FRead);
end;


procedure TTestBMPRoundTrip.Test16BitKeepsFiveSixFiveColors;

begin
  SetImage(CreateCheckerImage(6, 4, 1, RGB8($84, $82, $84), RGB8($08, $04, $10)));
  FImage.Colors[0, 0] := colBlack;
  FImage.Colors[1, 0] := colWhite;
  FWriter.BitsPerPixel := 16;
  RoundTrip;
  AssertImagesEqual('Colours whose 8 bits repeat their top 5-6-5 bits come back exactly', FImage, FRead);
end;


procedure TTestBMPRoundTrip.Test16BitStaysClose;

begin
  SetImage(CreateGradientImage(40, 30));
  FWriter.BitsPerPixel := 16;
  RoundTrip;
  AssertImagesEqual('A 16-bit BMP stays within 8 levels', FImage, FRead, 8 * 257);
end;


procedure TTestBMPRoundTrip.Test16BitWhiteStaysWhite;

begin
  SetImage(CreateSolidImage(3, 2, colWhite));
  FWriter.BitsPerPixel := 16;
  RoundTrip;
  AssertColorsEqual('White survives a 16-bit BMP', colWhite, FRead[1, 1]);
end;


procedure TTestBMPRoundTrip.Test15BitWhiteStaysWhite;

begin
  SetImage(CreateSolidImage(3, 2, colWhite));
  FWriter.BitsPerPixel := 15;
  RoundTrip;
  AssertColorsEqual('White survives a 15-bit BMP', colWhite, FRead[1, 1]);
end;


procedure TTestBMPRoundTrip.Test8BitKeepsThePalette;

begin
  SetImage(CreateRunsImage(21, 7, 200));
  FWriter.BitsPerPixel := 8;
  RoundTrip;
  AssertTrue('An 8-bit BMP is read into a palette image', FRead.UsePalette);
  AssertImagesEqual('An 8-bit BMP keeps every colour', FImage, FRead);
end;


procedure TTestBMPRoundTrip.Test4BitKeepsThePalette;

begin
  SetImage(CreateRunsImage(13, 5, 16));
  FWriter.BitsPerPixel := 4;
  RoundTrip;
  AssertImagesEqual('A 4-bit BMP keeps every colour', FImage, FRead);
end;


procedure TTestBMPRoundTrip.Test1BitKeepsThePalette;

begin
  SetImage(CreateRunsImage(13, 5, 2));
  FWriter.BitsPerPixel := 1;
  RoundTrip;
  AssertImagesEqual('A 1-bit BMP keeps every colour', FImage, FRead);
end;


procedure TTestBMPRoundTrip.TestRLE8KeepsThePixels;

begin
  SetImage(CreateRunsImage(300, 9, 100));
  FWriter.BitsPerPixel := 8;
  FWriter.RLECompress := True;
  RoundTrip;
  AssertImagesEqual('An RLE8 BMP keeps every pixel', FImage, FRead);
end;


procedure TTestBMPRoundTrip.TestRLE4KeepsThePixels;

begin
  SetImage(CreateRunsImage(300, 9, 16));
  FWriter.BitsPerPixel := 4;
  FWriter.RLECompress := True;
  RoundTrip;
  AssertImagesEqual('An RLE4 BMP keeps every pixel', FImage, FRead);
end;


procedure TTestBMPRoundTrip.TestRLE8TallAndNarrow;

begin
  SetImage(CreateRunsImage(1, 40, 5));
  FWriter.BitsPerPixel := 8;
  FWriter.RLECompress := True;
  RoundTrip;
  AssertImagesEqual('An RLE8 BMP one pixel wide keeps every pixel', FImage, FRead);
end;


procedure TTestBMPRoundTrip.CheckRLE4Widths(aFirst: Integer);

var
  lWidth: Integer;

begin
  FWriter.BitsPerPixel := 4;
  FWriter.RLECompress := True;
  lWidth := aFirst;
  while lWidth <= 13 do
    begin
    SetImage(CreateRunsImage(lWidth, 4, 16));
    RoundTrip;
    AssertImagesEqual(Format('An RLE4 BMP of width %d', [lWidth]), FImage, FRead);
    Inc(lWidth, 2);
    end;
end;


procedure TTestBMPRoundTrip.TestRLE4OfEvenWidth;

begin
  CheckRLE4Widths(2);
end;


procedure TTestBMPRoundTrip.TestRLE4OfOddWidth;

begin
  CheckRLE4Widths(1);
end;


procedure TTestBMPRoundTrip.TestRLEWithTrueColorIsRejected;

begin
  SetImage(CreateGradientImage(4, 4));
  AssertRaises('RLE compression of a 24-bit image is rejected', FPImageException, @WriteRLE24);
end;


procedure TTestBMPRoundTrip.TestIndexedWithoutPaletteIsRejected;

begin
  SetImage(CreateGradientImage(4, 4));
  AssertRaises('An indexed BMP of an image without palette is rejected', FPImageException, @WriteIndexedWithoutPalette);
end;


procedure TTestBMPRoundTrip.TestPaletteTooLargeForTheDepthIsRejected;

begin
  SetImage(CreateRunsImage(8, 8, 20));
  AssertRaises('20 colours do not fit 4 bits', FPImageException, @WriteTooManyColors);
end;


procedure TTestBMPRoundTrip.TestAnInvalidDepthIsRejected;

begin
  AssertRaises('7 bits per pixel is rejected', FPImageException, @SetDepth7);
end;


{ TTestBMPHeader }

procedure TTestBMPHeader.SetUp;

begin
  inherited SetUp;
  FImage := CreateGradientImage(5, 3);
  FWriter := TFPWriterBMP.Create;
  FStream := TMemoryStream.Create;
end;


procedure TTestBMPHeader.TearDown;

begin
  FreeAndNil(FStream);
  FreeAndNil(FWriter);
  FreeAndNil(FImage);
  inherited TearDown;
end;


procedure TTestBMPHeader.WriteIt;

begin
  FStream.Clear;
  FImage.SaveToStream(FStream, FWriter);
end;


function TTestBMPHeader.LongAt(aOffset: Integer): LongInt;

var
  lBytes: PByte;

begin
  lBytes := PByte(FStream.Memory) + aOffset;
  Result := LongInt(lBytes[0] or (lBytes[1] shl 8) or (lBytes[2] shl 16) or (LongWord(lBytes[3]) shl 24));
end;


function TTestBMPHeader.WordAt(aOffset: Integer): Word;

var
  lBytes: PByte;

begin
  lBytes := PByte(FStream.Memory) + aOffset;
  Result := lBytes[0] or (lBytes[1] shl 8);
end;


procedure TTestBMPHeader.TestTheFileHeader;

begin
  WriteIt;
  AssertEquals('The file starts with BM', BMmagic, WordAt(0));
  AssertEquals('The file size is the size of the stream', FStream.Size, LongAt(2));
  AssertEquals('The reserved field is zero', 0, LongAt(6));
  AssertEquals('The pixels start after both headers', 54, LongAt(10));
end;


procedure TTestBMPHeader.TestTheInfoHeader;

begin
  WriteIt;
  AssertEquals('The info header is 40 bytes', 40, LongAt(14));
  AssertEquals('Width', 5, LongAt(18));
  AssertEquals('Height, positive for bottom-up', 3, LongAt(22));
  AssertEquals('One plane', 1, WordAt(26));
  AssertEquals('24 bits per pixel by default', 24, WordAt(28));
  AssertEquals('Uncompressed', BI_RGB, LongAt(30));
  AssertEquals('Image size: rows of 15 bytes padded to 16', 16 * 3, LongAt(34));
  AssertEquals('The stream holds headers and rows', 54 + 16 * 3, FStream.Size);
end;


procedure TTestBMPHeader.TestThePaletteFollowsTheHeader;

var
  lBytes: PByte;

begin
  FImage.UsePalette := True;
  FWriter.BitsPerPixel := 8;
  WriteIt;
  AssertEquals('Colours used is the palette size', FImage.Palette.Count, LongAt(46));
  AssertEquals('The pixels start after the palette', 54 + 4 * FImage.Palette.Count, LongAt(10));
  lBytes := PByte(FStream.Memory) + 54;
  AssertEquals('Palette entry 0: blue', FImage.Palette[0].Blue shr 8, lBytes[0]);
  AssertEquals('Palette entry 0: green', FImage.Palette[0].Green shr 8, lBytes[1]);
  AssertEquals('Palette entry 0: red', FImage.Palette[0].Red shr 8, lBytes[2]);
end;


procedure TTestBMPHeader.TestTranslucentImagesGetAV5HeaderWithAlphaMask;

begin
  FImage[1, 1] := RGB8(10, 20, 30, 128);
  FWriter.BitsPerPixel := 32;
  WriteIt;
  AssertEquals('A 124-byte V5 header', 124, LongAt(14));
  AssertEquals('Bit fields', BI_BITFIELDS, LongAt(30));
  AssertEquals('Red mask', $00FF0000, LongAt(54));
  AssertEquals('Green mask', $0000FF00, LongAt(58));
  AssertEquals('Blue mask', $000000FF, LongAt(62));
  AssertEquals('Alpha mask', LongInt($FF000000), LongAt(66));
  AssertEquals('The pixels follow the header', 14 + 124, LongAt(10));
  AssertEquals('The file holds headers and pixels', 14 + 124 + 5 * 3 * 4, FStream.Size);
end;


procedure TTestBMPHeader.TestOpaqueImagesKeepTheShortHeader;

begin
  FWriter.BitsPerPixel := 32;
  WriteIt;
  AssertEquals('An opaque 32-bit image keeps the 40-byte header', 40, LongAt(14));
  AssertEquals('Uncompressed', BI_RGB, LongAt(30));
end;


procedure TTestBMPHeader.TestACompressedImageGivesItsSize;

begin
  FImage.UsePalette := True;
  FWriter.BitsPerPixel := 8;
  FWriter.RLECompress := True;
  WriteIt;
  AssertEquals('RLE8 compression', BI_RLE8, LongAt(30));
  AssertEquals('The image size is what follows the offset', FStream.Size - LongAt(10), LongAt(34));
  AssertEquals('The file size is the size of the stream', FStream.Size, LongAt(2));
end;


procedure TTestBMPHeader.TestTheResolutionIsWritten;

begin
  FImage.ResolutionUnit := ruPixelsPerInch;
  FImage.ResolutionX := 300;
  FImage.ResolutionY := 150;
  WriteIt;
  AssertTrue('300 dpi is 11811 per metre', Abs(LongAt(38) - 11811) <= 1);
  AssertTrue('150 dpi is 5906 per metre', Abs(LongAt(42) - 5906) <= 1);
end;


procedure TTestBMPHeader.TestTheWriterLeavesTheImageAlone;

begin
  FImage.ResolutionUnit := ruPixelsPerInch;
  FImage.ResolutionX := 300;
  FImage.ResolutionY := 300;
  WriteIt;
  AssertTrue('Writing keeps the resolution unit of the image', FImage.ResolutionUnit = ruPixelsPerInch);
  AssertEquals('Writing keeps the resolution of the image', 300, FImage.ResolutionX, 0.001);
end;


procedure TTestBMPHeader.TestTheResolutionIsRead;

var
  lRead: TFPMemoryImage;
  lReader: TFPReaderBMP;

begin
  FImage.ResolutionUnit := ruPixelsPerInch;
  FImage.ResolutionX := 300;
  FImage.ResolutionY := 300;
  lReader := TFPReaderBMP.Create;
  try
    lRead := RoundTrip(FImage, FWriter, lReader);
    try
      lRead.ResolutionUnit := ruPixelsPerInch;
      AssertEquals('The resolution comes back', 300, lRead.ResolutionX, 0.5);
      AssertEquals('The vertical resolution comes back', 300, lRead.ResolutionY, 0.5);
    finally
      lRead.Free;
    end;
  finally
    lReader.Free;
  end;
end;


{ TTestBMPStreams }

procedure TTestBMPStreams.SetUp;

begin
  inherited SetUp;
  FImage := CreateGradientImage(6, 5);
  FWriter := TFPWriterBMP.Create;
  FReader := TFPReaderBMP.Create;
  FStream := TMemoryStream.Create;
end;


procedure TTestBMPStreams.TearDown;

begin
  FreeAndNil(FStream);
  FreeAndNil(FReader);
  FreeAndNil(FWriter);
  FreeAndNil(FImage);
  inherited TearDown;
end;


procedure TTestBMPStreams.CheckAfterPrefix(const aMessage: String);

var
  lRead: TFPMemoryImage;
  I: Integer;

begin
  FStream.WriteBuffer(cPrefix, SizeOf(cPrefix));
  WriteImage(FImage, FWriter, FStream);
  for I := 0 to High(cPrefix) do
    AssertEquals(aMessage + ': byte ' + IntToStr(I) + ' before the image is kept', cPrefix[I], PByte(FStream.Memory)[I]);
  FStream.Position := SizeOf(cPrefix);
  lRead := TFPMemoryImage.Create(0, 0);
  try
    lRead.LoadFromStream(FStream, FReader);
    AssertImagesEqual(aMessage + ': the image after the prefix reads back', FImage, lRead);
  finally
    lRead.Free;
  end;
end;


procedure TTestBMPStreams.TestWritingAfterOtherDataKeepsIt;

begin
  CheckAfterPrefix('24-bit');
end;


procedure TTestBMPStreams.TestWritingCompressedAfterOtherDataKeepsIt;

begin
  FImage.UsePalette := True;
  FWriter.BitsPerPixel := 8;
  FWriter.RLECompress := True;
  CheckAfterPrefix('RLE8');
end;


procedure TTestBMPStreams.TestTwoImagesInOneStream;

var
  lSecond, lRead: TFPMemoryImage;

begin
  lSecond := CreateCheckerImage(3, 7, 1, colRed, colBlue);
  lRead := TFPMemoryImage.Create(0, 0);
  try
    WriteImage(FImage, FWriter, FStream);
    WriteImage(lSecond, FWriter, FStream);
    FStream.Position := 0;
    lRead.LoadFromStream(FStream, FReader);
    AssertImagesEqual('The first image reads back', FImage, lRead);
    lRead.LoadFromStream(FStream, FReader);
    AssertImagesEqual('The second image follows the first', lSecond, lRead);
  finally
    lRead.Free;
    lSecond.Free;
  end;
end;


procedure TTestBMPStreams.TestTheReaderStopsAtTheEndOfTheImage;

var
  lRead: TFPMemoryImage;
  lSize: Int64;

begin
  lSize := WriteImage(FImage, FWriter, FStream);
  FStream.WriteBuffer(cPrefix, SizeOf(cPrefix));
  FStream.Position := 0;
  lRead := TFPMemoryImage.Create(0, 0);
  try
    lRead.LoadFromStream(FStream, FReader);
    AssertEquals('The reader leaves the stream at the end of the image', lSize, FStream.Position);
  finally
    lRead.Free;
  end;
end;


procedure TTestBMPStreams.TestWritingToAWriteOnlyStream;

var
  lOut: TWriteOnlyStream;

begin
  lOut := TWriteOnlyStream.Create;
  try
    FImage.SaveToStream(lOut, FWriter, False);
    FImage.SaveToStream(FStream, FWriter);
    AssertEquals('A write-only stream gets all bytes', FStream.Size, lOut.Data.Size);
    AssertTrue('A write-only stream gets the same bytes', CompareMem(FStream.Memory, lOut.Data.Memory, FStream.Size));
  finally
    lOut.Free;
  end;
end;


procedure TTestBMPStreams.TestImageSizeWithoutReading;

var
  lSize: TPoint;

begin
  FStream.WriteBuffer(cPrefix, SizeOf(cPrefix));
  WriteImage(FImage, FWriter, FStream);
  FStream.Position := SizeOf(cPrefix);
  lSize := TFPReaderBMP.ImageSize(FStream);
  AssertEquals('ImageSize gives the width', 6, lSize.X);
  AssertEquals('ImageSize gives the height', 5, lSize.Y);
  AssertEquals('ImageSize leaves the position alone', SizeOf(cPrefix), FStream.Position);
end;


{ TTestBMPReader }

procedure TTestBMPReader.SetUp;

begin
  inherited SetUp;
  FReader := TFPReaderBMP.Create;
  FImage := TFPMemoryImage.Create(0, 0);
  FStream := TMemoryStream.Create;
end;


procedure TTestBMPReader.TearDown;

begin
  FreeAndNil(FStream);
  FreeAndNil(FImage);
  FreeAndNil(FReader);
  inherited TearDown;
end;


procedure WriteLE16(aStream: TStream; aValue: Word);

var
  lBytes: array[0..1] of Byte;

begin
  lBytes[0] := aValue and $FF;
  lBytes[1] := aValue shr 8;
  aStream.WriteBuffer(lBytes, 2);
end;


procedure WriteLE32(aStream: TStream; aValue: LongWord);

var
  lBytes: array[0..3] of Byte;

begin
  lBytes[0] := aValue and $FF;
  lBytes[1] := (aValue shr 8) and $FF;
  lBytes[2] := (aValue shr 16) and $FF;
  lBytes[3] := aValue shr 24;
  aStream.WriteBuffer(lBytes, 4);
end;


procedure TTestBMPReader.Build(aHeaderSize, aWidth, aHeight, aBitCount, aCompression, aClrUsed: Integer;
  const aHeaderExtra, aTable, aPixels: array of Byte; aGap: Integer);

var
  lOffset, I: Integer;
  lZero: Byte;

begin
  lZero := 0;
  lOffset := 14 + aHeaderSize + Length(aTable) + aGap;
  FStream.Clear;
  WriteLE16(FStream, BMmagic);
  WriteLE32(FStream, lOffset + Length(aPixels));
  WriteLE32(FStream, 0);
  WriteLE32(FStream, lOffset);
  WriteLE32(FStream, aHeaderSize);
  WriteLE32(FStream, LongWord(aWidth));
  WriteLE32(FStream, LongWord(aHeight));
  WriteLE16(FStream, 1);
  WriteLE16(FStream, aBitCount);
  WriteLE32(FStream, aCompression);
  WriteLE32(FStream, Length(aPixels));
  WriteLE32(FStream, 2835);
  WriteLE32(FStream, 2835);
  WriteLE32(FStream, aClrUsed);
  WriteLE32(FStream, 0);
  if Length(aHeaderExtra) > 0 then
    FStream.WriteBuffer(aHeaderExtra[0], Length(aHeaderExtra));
  for I := 40 + Length(aHeaderExtra) to aHeaderSize - 1 do
    FStream.WriteBuffer(lZero, 1);
  if Length(aTable) > 0 then
    FStream.WriteBuffer(aTable[0], Length(aTable));
  for I := 1 to aGap do
    FStream.WriteBuffer(lZero, 1);
  if Length(aPixels) > 0 then
    FStream.WriteBuffer(aPixels[0], Length(aPixels));
  FStream.Position := 0;
end;


procedure TTestBMPReader.ReadIt;

begin
  FImage.LoadFromStream(FStream, FReader);
end;


procedure TTestBMPReader.ReadTruncated;

begin
  FStream.Size := FStream.Size - 10;
  FStream.Position := 0;
  ReadIt;
end;


procedure TTestBMPReader.ReadZeroWidth;

begin
  Build(40, 0, 2, 24, BI_RGB, 0, [], [], [0, 0, 0, 0, 0, 0, 0, 0]);
  ReadIt;
end;


procedure TTestBMPReader.CheckColor(const aMessage: String; aX, aY: Integer; const aColor: TFPColor);

begin
  AssertColorsEqual(aMessage, aColor, FImage[aX, aY]);
end;


procedure TTestBMPReader.TestATopDownImage;

begin
  { 2x2, 24-bit, top row red/green, bottom row blue/white }
  Build(40, 2, -2, 24, BI_RGB, 0, [], [],
    [0, 0, 255, 0, 255, 0, 0, 0,
     255, 0, 0, 255, 255, 255, 0, 0]);
  ReadIt;
  AssertEquals('Height of a top-down image', 2, FImage.Height);
  CheckColor('Top left', 0, 0, colRed);
  CheckColor('Top right', 1, 0, colGreen);
  CheckColor('Bottom left', 0, 1, colBlue);
  CheckColor('Bottom right', 1, 1, colWhite);
end;


procedure TTestBMPReader.TestImageSizeOfATopDownImageIsPositive;

var
  lSize: TPoint;

begin
  Build(40, 2, -2, 24, BI_RGB, 0, [], [], [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]);
  lSize := TFPReaderBMP.ImageSize(FStream);
  AssertEquals('ImageSize of a top-down image: width', 2, lSize.X);
  AssertEquals('ImageSize of a top-down image: height is positive', 2, lSize.Y);
end;


procedure TTestBMPReader.TestBitfields32;

begin
  { masks R=$0000FF00, G=$00FF0000, B=$FF000000: bytes in the file are x,R,G,B }
  Build(40, 2, 1, 32, BI_BITFIELDS, 0, [],
    [$00, $FF, $00, $00, $00, $00, $FF, $00, $00, $00, $00, $FF],
    [0, 255, 0, 0, 0, 10, 20, 30]);
  ReadIt;
  CheckColor('Pixel 0 is red', 0, 0, colRed);
  CheckColor('Pixel 1 with its own masks', 1, 0, RGB8(10, 20, 30));
end;


procedure TTestBMPReader.TestBitfields16;

begin
  { 5-6-5 masks; pixels $F800 (red) and $07E0 (green), row padded to 4 bytes }
  Build(40, 2, 1, 16, BI_BITFIELDS, 0, [],
    [$00, $F8, $00, $00, $E0, $07, $00, $00, $1F, $00, $00, $00],
    [$00, $F8, $E0, $07]);
  ReadIt;
  AssertTrue('Pixel 0 is red', (FImage[0, 0].Red > $F000) and (FImage[0, 0].Green = 0) and (FImage[0, 0].Blue = 0));
  AssertTrue('Pixel 1 is green', (FImage[1, 0].Green > $F000) and (FImage[1, 0].Red = 0) and (FImage[1, 0].Blue = 0));
end;


procedure TTestBMPReader.TestAV4HeaderWithTheMasksInside;

begin
  { a 108-byte header holds the masks itself, at offset 40 of the header }
  Build(108, 2, 1, 16, BI_BITFIELDS, 0,
    [$00, $F8, $00, $00, $E0, $07, $00, $00, $1F, $00, $00, $00, $00, $00, $00, $00],
    [],
    [$00, $F8, $1F, $00]);
  ReadIt;
  AssertTrue('Pixel 0 is red', (FImage[0, 0].Red > $F000) and (FImage[0, 0].Green = 0) and (FImage[0, 0].Blue = 0));
  AssertTrue('Pixel 1 is blue', (FImage[1, 0].Blue > $F000) and (FImage[1, 0].Red = 0) and (FImage[1, 0].Green = 0));
end;


procedure TTestBMPReader.TestAV5Header;

begin
  Build(124, 1, 2, 24, BI_RGB, 0, [], [],
    [30, 20, 10, 0,
     60, 50, 40, 0]);
  ReadIt;
  CheckColor('Bottom row comes first', 0, 1, RGB8(10, 20, 30));
  CheckColor('Top row comes last', 0, 0, RGB8(40, 50, 60));
end;


procedure TTestBMPReader.TestAV5HeaderWithAnAlphaMask;

begin
  { masks R,G,B,A at offset 40 of the 124-byte header; pixels in B,G,R,A byte order }
  Build(124, 2, 1, 32, BI_BITFIELDS, 0,
    [$00, $00, $FF, $00, $00, $FF, $00, $00, $FF, $00, $00, $00, $00, $00, $00, $FF],
    [],
    [30, 20, 10, 255, 60, 50, 40, 0]);
  ReadIt;
  CheckColor('Alpha 255 from the alpha mask is opaque', 0, 0, RGB8(10, 20, 30));
  CheckColor('Alpha 0 from the alpha mask is transparent', 1, 0, RGB8(40, 50, 60, 0));
end;


procedure TTestBMPReader.TestAnOS2Header;

begin
  { BITMAPCOREHEADER: size 12, width and height as 16-bit words }
  FStream.Clear;
  WriteLE16(FStream, BMmagic);
  WriteLE32(FStream, 14 + 12 + 8);
  WriteLE32(FStream, 0);
  WriteLE32(FStream, 14 + 12);
  WriteLE32(FStream, 12);
  WriteLE16(FStream, 2);
  WriteLE16(FStream, 1);
  WriteLE16(FStream, 1);
  WriteLE16(FStream, 24);
  FStream.WriteBuffer(PChar(#0#0#255#255#255#255#0#0)^, 8);
  FStream.Position := 0;
  ReadIt;
  AssertEquals('Width of an OS/2 bitmap', 2, FImage.Width);
  AssertEquals('Height of an OS/2 bitmap', 1, FImage.Height);
  CheckColor('Pixel 0 is red', 0, 0, colRed);
  CheckColor('Pixel 1 is white', 1, 0, colWhite);
end;


procedure TTestBMPReader.TestAnOS2PaletteHasThreeBytesPerEntry;

var
  I: Integer;

begin
  { 1-bit OS/2 bitmap: two palette entries of 3 bytes (BGR), then one padded row }
  FStream.Clear;
  WriteLE16(FStream, BMmagic);
  WriteLE32(FStream, 14 + 12 + 6 + 4);
  WriteLE32(FStream, 0);
  WriteLE32(FStream, 14 + 12 + 6);
  WriteLE32(FStream, 12);
  WriteLE16(FStream, 3);
  WriteLE16(FStream, 1);
  WriteLE16(FStream, 1);
  WriteLE16(FStream, 1);
  FStream.WriteBuffer(PChar(#255#0#0#0#0#255)^, 6);
  FStream.WriteBuffer(PChar(#$A0#0#0#0)^, 4);
  FStream.Position := 0;
  ReadIt;
  CheckColor('Bit 1 is palette entry 1, red', 0, 0, colRed);
  CheckColor('Bit 0 is palette entry 0, blue', 1, 0, colBlue);
  CheckColor('Bit 1 again', 2, 0, colRed);
  for I := 0 to FImage.Palette.Count - 1 do
    AssertEquals(Format('Palette entry %d is opaque', [I]), alphaOpaque, FImage.Palette[I].Alpha);
end;


procedure TTestBMPReader.TestThePixelsStartAtTheirOffset;

begin
  { 8-bit, 2 palette entries, 6 bytes between the palette and the pixels }
  Build(40, 4, 1, 8, BI_RGB, 2, [],
    [0, 0, 0, 0, 0, 0, 255, 0],
    [1, 0, 1, 1], 6);
  ReadIt;
  CheckColor('Pixel 0 is palette entry 1', 0, 0, colRed);
  CheckColor('Pixel 1 is palette entry 0', 1, 0, colBlack);
  CheckColor('Pixel 3 is palette entry 1', 3, 0, colRed);
end;


procedure TTestBMPReader.TestAPaletteShorterThanTheDepth;

begin
  Build(40, 4, 1, 8, BI_RGB, 2, [],
    [0, 0, 0, 0, 255, 0, 0, 0],
    [1, 0, 1, 0]);
  ReadIt;
  AssertEquals('The palette has the colours used', 2, FImage.Palette.Count);
  CheckColor('Pixel 0 is blue', 0, 0, colBlue);
  CheckColor('Pixel 1 is black', 1, 0, colBlack);
end;


procedure TTestBMPReader.TestRLE8Delta;

begin
  { 4x3, rows stored bottom first:
    row 2: 4 x red; row 1: 1 x green, then move 2 right and 1 line on;
    row 0: 1 x red at x=3 }
  Build(40, 4, 3, 8, BI_RLE8, 3, [],
    [0, 0, 0, 0, 0, 0, 255, 0, 0, 255, 0, 0],
    [4, 1, 0, 0,
     1, 2, 0, 2, 2, 1,
     1, 1, 0, 1]);
  ReadIt;
  CheckColor('Bottom row is red', 2, 2, colRed);
  CheckColor('Middle row starts green', 0, 1, colGreen);
  CheckColor('Skipped pixels of the middle row are index 0', 3, 1, colBlack);
  CheckColor('Skipped pixels of the top row are index 0', 1, 0, colBlack);
  CheckColor('The pixel after the move lands on the top row', 3, 0, colRed);
end;


procedure TTestBMPReader.TestRLE8EndOfBitmap;

var
  lEnd: Int64;

begin
  { 2x3: row 2 red, then end of bitmap; bytes after it belong to something else }
  Build(40, 2, 3, 8, BI_RLE8, 3, [],
    [0, 0, 0, 0, 0, 0, 255, 0, 0, 255, 0, 0],
    [2, 1, 0, 0,
     0, 1,
     2, 2, 0, 1]);
  lEnd := FStream.Size - 4;
  ReadIt;
  CheckColor('Bottom row is red', 0, 2, colRed);
  CheckColor('Rows after the end of the bitmap are index 0', 0, 1, colBlack);
  CheckColor('The top row is index 0 too', 1, 0, colBlack);
  AssertEquals('Reading stops at the end-of-bitmap code', lEnd, FStream.Position);
end;


procedure TTestBMPReader.Test32BitWithZeroReservedByteIsOpaque;

begin
  Build(40, 1, 1, 32, BI_RGB, 0, [], [], [30, 20, 10, 0]);
  ReadIt;
  CheckColor('A zero fourth byte reads as opaque', 0, 0, RGB8(10, 20, 30));
end;


procedure TTestBMPReader.Test32BitWithFullAlphaIsOpaque;

begin
  Build(40, 2, 1, 32, BI_RGB, 0, [], [], [30, 20, 10, 255, 60, 50, 40, 128]);
  ReadIt;
  CheckColor('A fourth byte of 255 reads as opaque', 0, 0, RGB8(10, 20, 30));
  CheckColor('A fourth byte of 128 reads as half transparent', 1, 0, RGB8(40, 50, 60, 128));
end;


procedure TTestBMPReader.TestATruncatedFileRaises;

var
  lWriter: TFPWriterBMP;
  lSource: TFPMemoryImage;

begin
  lWriter := TFPWriterBMP.Create;
  lSource := CreateGradientImage(8, 8);
  try
    lSource.SaveToStream(FStream, lWriter);
  finally
    lSource.Free;
    lWriter.Free;
  end;
  AssertRaises('A file that ends in the middle of the pixels raises', Exception, @ReadTruncated);
end;


procedure TTestBMPReader.TestAZeroWidthRaises;

begin
  AssertRaises('A width of 0 raises', FPImageException, @ReadZeroWidth);
end;


procedure TTestBMPReader.TestContentsCheckRejectsOtherData;

var
  lText: TStringStream;

begin
  lText := TStringStream.Create('BM but not really a bitmap file, just text');
  try
    AssertFalse('Text starting with BM is not a BMP', FReader.CheckContents(lText));
  finally
    lText.Free;
  end;
end;


initialization
  RegisterTests('bmp', [TTestBMPRoundTrip, TTestBMPHeader, TTestBMPStreams, TTestBMPReader]);
end.
