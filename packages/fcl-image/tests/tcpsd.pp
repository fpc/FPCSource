{
    Tests for the Photoshop (PSD) reader: hand-built files of every colour
    mode and depth, raw and PackBits data, skipped sections and streams.
    See the file COPYING.FPC, included in this distribution, for details.
}
unit tcpsd;

{$mode objfpc}{$H+}

interface

uses sysutils, classes, fpcunit, testregistry, fpimage, fpimgtests,
     psdcomn, fpreadpsd;

type
  TTestPSDReader = class(TTestCase)
  private
    FReader: TFPReaderPSD;
    FRead: TFPMemoryImage;
    FStream: TMemoryStream;
    FPrefix: String;
    FVersion: Word;
    FColorData: TBytes;
    FResources: TBytes;
    FLayers: TBytes;
    // Writes a big-endian word to the stream.
    procedure WriteBE16(aValue: Word);
    // Writes a big-endian longword to the stream.
    procedure WriteBE32(aValue: Cardinal);
    // Writes bytes to the stream.
    procedure WriteBytes(const aBytes: array of Byte);
    // Writes a section: its 32-bit length, then its bytes.
    procedure WriteSection(const aBytes: TBytes);
    // Builds a PSD file after FPrefix, from the fields and the image data section.
    procedure Build(aChannels: Word; aWidth, aHeight: Cardinal; aDepth, aMode, aCompression: Word;
      const aData: array of Byte);
    // Builds a 3x2 RGB file with PackBits rows of different lengths.
    procedure BuildRLE;
    // Builds a 2x1 raw RGB file.
    procedure BuildSmallRGB;
    // Reads the stream from its position into FRead.
    procedure ReadIt;
    // Reads the stream twice with one new reader into one new image.
    procedure ReadTwice;
    // Reads the stream with ImageRead and no image, and checks the image created.
    procedure ReadWithoutImage;
    // Cuts the last 3 bytes of the stream and reads it from the start.
    procedure ReadTruncated;
    // Reads the stream from the start into a new image, swallowing any exception.
    procedure ReadTruncatedCatching;
    // Fails unless the pixel of FRead at (aX,aY) is aColor within aTolerance.
    procedure CheckColor(const aMessage: String; aX, aY: Integer; const aColor: TFPColor; aTolerance: Word = 0);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRGB8;
    procedure TestRGB8WithAlpha;
    procedure TestRGB8ExtraChannelsAreIgnored;
    procedure TestRGB16;
    procedure TestRGB16WithAlpha;
    procedure TestGray8;
    procedure TestGray8WhiteIsWhite;
    procedure TestGray16;
    procedure TestGray8WithAlpha;
    procedure TestDuotoneReadsAsGray;
    procedure TestIndexed;
    procedure TestBitmapWidthOfEight;
    procedure TestBitmapRowsArePaddedToBytes;
    procedure TestCMYK8;
    procedure TestCMYK16;
    procedure TestLab8;
    procedure TestRLE;
    procedure TestRLEStopsAtTheEndOfTheImage;
    procedure TestRawStopsAtTheEndOfTheImage;
    procedure TestResourcesAndLayersAreSkipped;
    procedure TestResolutionResource;
    procedure TestUnknownCompressionRaises;
    procedure TestPSBIsRejected;
    procedure TestReadingAfterAPrefix;
    procedure TestContentsCheck;
    procedure TestContentsCheckRejectsAnUnknownVersion;
    procedure TestReadingWithoutAnImage;
    procedure TestReadingDoesNotLeak;
    procedure TestAFailingReadDoesNotLeak;
    procedure TestATruncatedFileRaises;
    procedure TestATruncatedRLEFileRaises;
    procedure TestATruncatedHeaderRaises;
    procedure TestPSDAndPDDAreRegistered;
  end;

implementation

// A dynamic array with the given bytes.
function Bytes(const aBytes: array of Byte): TBytes;

var
  I: Integer;

begin
  Result := nil;
  SetLength(Result, Length(aBytes));
  for I := 0 to High(aBytes) do
    Result[I] := aBytes[I];
end;


procedure TTestPSDReader.SetUp;

begin
  inherited SetUp;
  FReader := TFPReaderPSD.Create;
  FStream := TMemoryStream.Create;
  FPrefix := '';
  FVersion := 1;
  FColorData := nil;
  FResources := nil;
  FLayers := nil;
end;


procedure TTestPSDReader.TearDown;

begin
  FreeAndNil(FStream);
  FreeAndNil(FRead);
  FreeAndNil(FReader);
  inherited TearDown;
end;


procedure TTestPSDReader.WriteBE16(aValue: Word);

begin
  WriteBytes([aValue shr 8, aValue and $FF]);
end;


procedure TTestPSDReader.WriteBE32(aValue: Cardinal);

begin
  WriteBytes([aValue shr 24, (aValue shr 16) and $FF, (aValue shr 8) and $FF, aValue and $FF]);
end;


procedure TTestPSDReader.WriteBytes(const aBytes: array of Byte);

begin
  if Length(aBytes) > 0 then
    FStream.WriteBuffer(aBytes[0], Length(aBytes));
end;


procedure TTestPSDReader.WriteSection(const aBytes: TBytes);

begin
  WriteBE32(Length(aBytes));
  if Length(aBytes) > 0 then
    FStream.WriteBuffer(aBytes[0], Length(aBytes));
end;


procedure TTestPSDReader.Build(aChannels: Word; aWidth, aHeight: Cardinal; aDepth, aMode, aCompression: Word;
  const aData: array of Byte);

begin
  FStream.Clear;
  if FPrefix <> '' then
    FStream.WriteBuffer(FPrefix[1], Length(FPrefix));
  WriteBytes([Ord('8'), Ord('B'), Ord('P'), Ord('S')]);
  WriteBE16(FVersion);
  WriteBytes([0, 0, 0, 0, 0, 0]);
  WriteBE16(aChannels);
  WriteBE32(aHeight);
  WriteBE32(aWidth);
  WriteBE16(aDepth);
  WriteBE16(aMode);
  WriteSection(FColorData);
  WriteSection(FResources);
  if FVersion = 2 then
    begin
    WriteBE32(0);
    WriteSection(FLayers);
    end
  else
    WriteSection(FLayers);
  WriteBE16(aCompression);
  WriteBytes(aData);
  FStream.Position := Length(FPrefix);
end;


procedure TTestPSDReader.BuildRLE;

begin
  Build(3, 3, 2, 8, PSD_RGB, 1,
    [0, 2, 0, 4, 0, 4, 0, 2, 0, 3, 0, 4,
     $FE, 10,
     $02, 1, 2, 3,
     $00, 20, $FF, 21,
     $FE, 22,
     $80, $FE, 30,
     $02, 31, 32, 33]);
end;


procedure TTestPSDReader.BuildSmallRGB;

begin
  Build(3, 2, 1, 8, PSD_RGB, 0, [10, 40, 20, 50, 30, 60]);
end;


procedure TTestPSDReader.ReadIt;

begin
  FreeAndNil(FRead);
  FRead := TFPMemoryImage.Create(0, 0);
  FRead.LoadFromStream(FStream, FReader);
end;


procedure TTestPSDReader.ReadTwice;

var
  lReader: TFPReaderPSD;
  lImage: TFPMemoryImage;

begin
  lReader := TFPReaderPSD.Create;
  lImage := TFPMemoryImage.Create(0, 0);
  try
    FStream.Position := 0;
    lImage.LoadFromStream(FStream, lReader);
    FStream.Position := 0;
    lImage.LoadFromStream(FStream, lReader);
  finally
    lImage.Free;
    lReader.Free;
  end;
end;


procedure TTestPSDReader.ReadWithoutImage;

var
  lImage: TFPCustomImage;

begin
  lImage := FReader.ImageRead(FStream, nil);
  try
    AssertNotNull('An image is created', lImage);
    AssertEquals('The image created has the width of the file', 2, lImage.Width);
    AssertEquals('The image created has the height of the file', 1, lImage.Height);
    AssertColorsEqual('The image created has the pixels of the file', RGB8(40, 50, 60), lImage.Colors[1, 0]);
  finally
    lImage.Free;
  end;
end;


procedure TTestPSDReader.ReadTruncated;

begin
  FStream.Size := FStream.Size - 3;
  FStream.Position := 0;
  ReadIt;
end;


procedure TTestPSDReader.ReadTruncatedCatching;

var
  lImage: TFPMemoryImage;

begin
  lImage := TFPMemoryImage.Create(0, 0);
  try
    FStream.Position := 0;
    try
      lImage.LoadFromStream(FStream, FReader);
    except
      on Exception do ;
    end;
  finally
    lImage.Free;
  end;
end;


procedure TTestPSDReader.CheckColor(const aMessage: String; aX, aY: Integer; const aColor: TFPColor; aTolerance: Word);

begin
  AssertColorsEqual(aMessage, aColor, FRead[aX, aY], aTolerance);
end;


procedure TTestPSDReader.TestRGB8;

begin
  Build(3, 3, 2, 8, PSD_RGB, 0,
    [10, 20, 30, 40, 50, 60,
     1, 2, 3, 4, 5, 6,
     101, 102, 103, 104, 105, 106]);
  ReadIt;
  AssertEquals('The width', 3, FRead.Width);
  AssertEquals('The height', 2, FRead.Height);
  CheckColor('Pixel (0,0) takes the first byte of each plane', 0, 0, RGB8(10, 1, 101));
  CheckColor('Pixel (1,0)', 1, 0, RGB8(20, 2, 102));
  CheckColor('Pixel (2,0)', 2, 0, RGB8(30, 3, 103));
  CheckColor('Pixel (0,1) starts the second row of each plane', 0, 1, RGB8(40, 4, 104));
  CheckColor('Pixel (1,1)', 1, 1, RGB8(50, 5, 105));
  CheckColor('Pixel (2,1) takes the last byte of each plane', 2, 1, RGB8(60, 6, 106));
end;


procedure TTestPSDReader.TestRGB8WithAlpha;

begin
  Build(4, 2, 1, 8, PSD_RGB, 0, [10, 40, 20, 50, 30, 60, 255, 0]);
  ReadIt;
  CheckColor('The fourth channel is the alpha of an opaque pixel', 0, 0, RGB8(10, 20, 30, 255));
  CheckColor('The fourth channel is the alpha of a transparent pixel', 1, 0, RGB8(40, 50, 60, 0));
end;


procedure TTestPSDReader.TestRGB8ExtraChannelsAreIgnored;

begin
  Build(5, 2, 1, 8, PSD_RGB, 0, [10, 40, 20, 50, 30, 60, 255, 128, 7, 7]);
  ReadIt;
  CheckColor('Pixel 0 uses the first four channels', 0, 0, RGB8(10, 20, 30, 255));
  CheckColor('Pixel 1 uses the first four channels', 1, 0, RGB8(40, 50, 60, 128));
  AssertEquals('The fifth channel is read past', FStream.Size, FStream.Position);
end;


procedure TTestPSDReader.TestRGB16;

begin
  Build(3, 2, 1, 16, PSD_RGB, 0,
    [$12, $34, $FF, $FF,
     $56, $78, $00, $00,
     $9A, $BC, $80, $01]);
  ReadIt;
  CheckColor('16-bit channels are big-endian words', 0, 0, FPColor($1234, $5678, $9ABC));
  CheckColor('The second pixel of each 16-bit plane', 1, 0, FPColor($FFFF, 0, $8001));
end;


procedure TTestPSDReader.TestRGB16WithAlpha;

begin
  Build(4, 1, 1, 16, PSD_RGB, 0, [$11, $11, $22, $22, $33, $33, $80, $00]);
  ReadIt;
  CheckColor('The fourth 16-bit channel is the alpha', 0, 0, FPColor($1111, $2222, $3333, $8000));
end;


procedure TTestPSDReader.TestGray8;

begin
  Build(1, 3, 1, 8, PSD_GRAYSCALE, 0, [0, 128, 1]);
  ReadIt;
  CheckColor('Gray 0 is black', 0, 0, colBlack);
  CheckColor('Gray 128', 1, 0, RGB8(128, 128, 128));
  CheckColor('Gray 1', 2, 0, RGB8(1, 1, 1));
end;


procedure TTestPSDReader.TestGray8WhiteIsWhite;

begin
  Build(1, 1, 1, 8, PSD_GRAYSCALE, 0, [255]);
  ReadIt;
  CheckColor('Gray 255 is white', 0, 0, colWhite);
end;


procedure TTestPSDReader.TestGray16;

begin
  Build(1, 2, 1, 16, PSD_GRAYSCALE, 0, [$12, $34, $FF, $FF]);
  ReadIt;
  CheckColor('A 16-bit gray is a big-endian word', 0, 0, FPColor($1234, $1234, $1234));
  CheckColor('16-bit gray $FFFF is white', 1, 0, colWhite);
end;


procedure TTestPSDReader.TestGray8WithAlpha;

begin
  Build(2, 2, 1, 8, PSD_GRAYSCALE, 0, [100, 200, 255, 0]);
  ReadIt;
  CheckColor('The second channel is the alpha of an opaque gray', 0, 0, RGB8(100, 100, 100, 255));
  CheckColor('The second channel is the alpha of a transparent gray', 1, 0, RGB8(200, 200, 200, 0));
end;


procedure TTestPSDReader.TestDuotoneReadsAsGray;

begin
  FColorData := Bytes([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);
  Build(1, 2, 1, 8, PSD_DUOTONE, 0, [0, 100]);
  ReadIt;
  CheckColor('Duotone 0 reads as black', 0, 0, colBlack);
  CheckColor('Duotone 100 reads as gray 100', 1, 0, RGB8(100, 100, 100));
  AssertEquals('The duotone specification is skipped', FStream.Size, FStream.Position);
end;


procedure TTestPSDReader.TestIndexed;

var
  lMap: TBytes;

begin
  lMap := nil;
  SetLength(lMap, 768);
  FillChar(lMap[0], 768, 0);
  lMap[0] := 255;
  lMap[256] := 0;
  lMap[512] := 0;
  lMap[1] := 0;
  lMap[257] := 128;
  lMap[513] := 255;
  lMap[2] := 12;
  lMap[258] := 34;
  lMap[514] := 56;
  FColorData := lMap;
  Build(1, 3, 1, 8, PSD_INDEXED, 0, [2, 0, 1]);
  ReadIt;
  CheckColor('Index 2 is the third entry of the planar map', 0, 0, RGB8(12, 34, 56));
  CheckColor('Index 0 is the first entry of the planar map', 1, 0, RGB8(255, 0, 0));
  CheckColor('Index 1 is the second entry of the planar map', 2, 0, RGB8(0, 128, 255));
end;


procedure TTestPSDReader.TestBitmapWidthOfEight;

const
  cRows: array[0..1] of Byte = ($A5, $0F);

var
  lX, lY: Integer;

begin
  Build(1, 8, 2, 1, PSD_BITMAP, 0, [$A5, $0F]);
  ReadIt;
  for lY := 0 to 1 do
    for lX := 0 to 7 do
      if (cRows[lY] and ($80 shr lX)) <> 0 then
        CheckColor(Format('A set bit at (%d,%d) is black', [lX, lY]), lX, lY, colBlack)
      else
        CheckColor(Format('A clear bit at (%d,%d) is white', [lX, lY]), lX, lY, colWhite);
end;


procedure TTestPSDReader.TestBitmapRowsArePaddedToBytes;

begin
  Build(1, 3, 2, 1, PSD_BITMAP, 0, [$A0, $40]);
  ReadIt;
  CheckColor('Row 0, pixel 0 is black', 0, 0, colBlack);
  CheckColor('Row 0, pixel 1 is white', 1, 0, colWhite);
  CheckColor('Row 0, pixel 2 is black', 2, 0, colBlack);
  CheckColor('Row 1 starts at the next byte: pixel 0 is white', 0, 1, colWhite);
  CheckColor('Row 1, pixel 1 is black', 1, 1, colBlack);
  CheckColor('Row 1, pixel 2 is white', 2, 1, colWhite);
  AssertEquals('The reader stops at the end of the padded rows', FStream.Size, FStream.Position);
end;


procedure TTestPSDReader.TestCMYK8;

begin
  Build(4, 4, 1, 8, PSD_CMYK, 0,
    [255, 0, 255, 255,
     255, 255, 0, 255,
     255, 255, 255, 255,
     255, 255, 255, 0]);
  ReadIt;
  CheckColor('No ink is white', 0, 0, colWhite);
  CheckColor('Full cyan', 1, 0, RGB8(0, 255, 255));
  CheckColor('Full magenta', 2, 0, RGB8(255, 0, 255));
  CheckColor('Full black ink is black', 3, 0, colBlack);
end;


procedure TTestPSDReader.TestCMYK16;

begin
  Build(4, 2, 1, 16, PSD_CMYK, 0,
    [$FF, $FF, $FF, $FF,
     $FF, $FF, $FF, $FF,
     $FF, $FF, $00, $00,
     $FF, $FF, $FF, $FF]);
  ReadIt;
  CheckColor('16-bit: no ink is white', 0, 0, colWhite);
  CheckColor('16-bit: full yellow', 1, 0, FPColor($FFFF, $FFFF, 0));
end;


procedure TTestPSDReader.TestLab8;

begin
  Build(3, 2, 1, 8, PSD_LAB, 0, [255, 0, 128, 128, 128, 128]);
  ReadIt;
  CheckColor('L 100 with neutral a and b is white', 0, 0, colWhite, $300);
  CheckColor('L 0 with neutral a and b is black', 1, 0, colBlack, $300);
end;


procedure TTestPSDReader.TestRLE;

begin
  BuildRLE;
  ReadIt;
  AssertTrue('The reader reports compressed data', FReader.Compressed);
  CheckColor('Pixel (0,0)', 0, 0, RGB8(10, 20, 30));
  CheckColor('Pixel (1,0): a literal then a run in green', 1, 0, RGB8(10, 21, 30));
  CheckColor('Pixel (2,0)', 2, 0, RGB8(10, 21, 30));
  CheckColor('Pixel (0,1): a literal row in red', 0, 1, RGB8(1, 22, 31));
  CheckColor('Pixel (1,1)', 1, 1, RGB8(2, 22, 32));
  CheckColor('Pixel (2,1)', 2, 1, RGB8(3, 22, 33));
end;


procedure TTestPSDReader.TestRLEStopsAtTheEndOfTheImage;

var
  lEnd: Int64;

begin
  BuildRLE;
  lEnd := FStream.Size;
  FStream.Seek(0, soEnd);
  WriteBytes([1, 2, 3, 4, 5]);
  FStream.Position := 0;
  ReadIt;
  AssertEquals('The reader stops after the last PackBits row', lEnd, FStream.Position);
end;


procedure TTestPSDReader.TestRawStopsAtTheEndOfTheImage;

var
  lEnd: Int64;

begin
  BuildSmallRGB;
  lEnd := FStream.Size;
  FStream.Seek(0, soEnd);
  WriteBytes([1, 2, 3, 4, 5]);
  FStream.Position := 0;
  ReadIt;
  AssertEquals('The reader stops after the last plane', lEnd, FStream.Position);
end;


procedure TTestPSDReader.TestResourcesAndLayersAreSkipped;

begin
  FResources := Bytes([Ord('8'), Ord('B'), Ord('I'), Ord('M'), $04, $04, 0, 0, 0, 0, 0, 3, 1, 2, 3, 0,
    Ord('8'), Ord('B'), Ord('I'), Ord('M'), $04, $0A, 0, 0, 0, 0, 0, 2, 0, 1]);
  FLayers := Bytes([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);
  BuildSmallRGB;
  ReadIt;
  CheckColor('Pixel 0 after the skipped sections', 0, 0, RGB8(10, 20, 30));
  CheckColor('Pixel 1 after the skipped sections', 1, 0, RGB8(40, 50, 60));
  AssertEquals('The reader stops at the end of the image', FStream.Size, FStream.Position);
end;


procedure TTestPSDReader.TestResolutionResource;

begin
  FResources := Bytes([Ord('8'), Ord('B'), Ord('I'), Ord('M'), $03, $ED, 0, 0, 0, 0, 0, 16,
    $00, $48, $00, $00, 0, 1, 0, 1, $00, $96, $00, $00, 0, 1, 0, 1]);
  BuildSmallRGB;
  ReadIt;
  AssertTrue('The resolution unit is pixels per inch', FRead.ResolutionUnit = ruPixelsPerInch);
  AssertEquals('The horizontal resolution', 72.0, FRead.ResolutionX, 0.001);
  AssertEquals('The vertical resolution', 150.0, FRead.ResolutionY, 0.001);
  CheckColor('The pixels after the resolution resource', 1, 0, RGB8(40, 50, 60));
end;


procedure TTestPSDReader.TestUnknownCompressionRaises;

begin
  Build(3, 2, 1, 8, PSD_RGB, 2, [10, 40, 20, 50, 30, 60]);
  AssertRaises('ZIP compression is rejected', FPImageException, @ReadIt);
end;


procedure TTestPSDReader.TestPSBIsRejected;

begin
  FVersion := 2;
  BuildSmallRGB;
  AssertRaises('A PSB file (version 2) is rejected', FPImageException, @ReadIt);
end;


procedure TTestPSDReader.TestReadingAfterAPrefix;

begin
  FPrefix := 'prefix';
  BuildSmallRGB;
  ReadIt;
  CheckColor('Pixel 0 of the image after the prefix', 0, 0, RGB8(10, 20, 30));
  CheckColor('Pixel 1 of the image after the prefix', 1, 0, RGB8(40, 50, 60));
  AssertEquals('The reader stops at the end of the image', FStream.Size, FStream.Position);
end;


procedure TTestPSDReader.TestContentsCheck;

begin
  BuildSmallRGB;
  AssertTrue('A valid header is accepted', FReader.CheckContents(FStream));
  AssertEquals('The check leaves the position alone', 0, FStream.Position);
  PByte(FStream.Memory)[0] := Ord('X');
  AssertFalse('A wrong signature is rejected', FReader.CheckContents(FStream));
  BuildSmallRGB;
  FStream.Size := 10;
  FStream.Position := 0;
  AssertFalse('A stream shorter than the header is rejected', FReader.CheckContents(FStream));
end;


procedure TTestPSDReader.TestContentsCheckRejectsAnUnknownVersion;

begin
  FVersion := 3;
  BuildSmallRGB;
  AssertFalse('Version 3 is rejected', FReader.CheckContents(FStream));
end;


procedure TTestPSDReader.TestReadingWithoutAnImage;

begin
  BuildSmallRGB;
  ReadWithoutImage;
end;


procedure TTestPSDReader.TestReadingDoesNotLeak;

begin
  BuildRLE;
  AssertNoLeak('Reading twice with one reader frees what the first read allocated', @ReadTwice);
end;


procedure TTestPSDReader.TestAFailingReadDoesNotLeak;

begin
  BuildRLE;
  FStream.Size := FStream.Size - 3;
  AssertNoLeak('A read that fails frees what it allocated', @ReadTruncatedCatching);
end;


procedure TTestPSDReader.TestATruncatedFileRaises;

begin
  Build(3, 4, 4, 8, PSD_RGB, 0, [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]);
  AssertRaises('A file that ends in the middle of the pixels raises', FPImageException, @ReadTruncated);
end;


procedure TTestPSDReader.TestATruncatedRLEFileRaises;

begin
  BuildRLE;
  AssertRaises('A file that ends in the middle of the PackBits data raises', FPImageException, @ReadTruncated);
end;


procedure TTestPSDReader.TestATruncatedHeaderRaises;

begin
  BuildSmallRGB;
  FStream.Size := 20;
  FStream.Position := 0;
  AssertRaises('A file that ends in the header raises', FPImageException, @ReadIt);
end;


procedure TTestPSDReader.TestPSDAndPDDAreRegistered;

begin
  AssertTrue('The PSD Format type has the PSD reader', ImageHandlers.ImageReader['PSD Format'] = TFPReaderPSD);
  AssertTrue('The PDD Format type has the PSD reader', ImageHandlers.ImageReader['PDD Format'] = TFPReaderPSD);
  AssertTrue('The .psd extension finds the PSD reader', TFPCustomImage.FindReaderFromExtension('psd') = TFPReaderPSD);
  AssertTrue('The .pdd extension finds the PSD reader', TFPCustomImage.FindReaderFromExtension('pdd') = TFPReaderPSD);
end;


initialization
  RegisterTest('psd', TTestPSDReader);
end.
