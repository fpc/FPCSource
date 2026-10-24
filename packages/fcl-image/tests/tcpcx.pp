{
    Tests for the PCX reader and writer: round trips with and without
    compression, the header written and hand-built files of other depths.
    See the file COPYING.FPC, included in this distribution, for details.
}
unit tcpcx;

{$mode objfpc}{$H+}

interface

uses sysutils, classes, fpcunit, testregistry, fpimage, fpimgtests,
     pcxcomn, fpreadpcx, fpwritepcx;

type
  TTestPCX = class(TTestCase)
  private
    FReader: TFPReaderPCX;
    FWriter: TFPWriterPCX;
    FImage: TFPMemoryImage;
    FRead: TFPMemoryImage;
    FStream: TMemoryStream;
    // Builds a PCX file from a header of the given layout and the data after it.
    procedure Build(aBitsPerPixel, aPlanes: Byte; aWidth, aHeight, aBytesPerLine: Word;
      aEncoding: Byte; const aData: array of Byte);
    function ReadHeader: TPCXHeader;
    procedure ReadIt;
    procedure ReadTruncatedSafely;
    procedure ReadSixteenBits;
    procedure CheckColor(const aMessage: String; aX, aY: Integer; const aColor: TFPColor);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRoundTripKeepsTheColors;
    procedure TestCompressedRoundTripKeepsTheColors;
    procedure TestRoundTripOfEveryWidth;
    procedure TestCompressionMakesASolidImageSmaller;
    procedure TestTheHeaderWritten;
    procedure TestBytesPerLineIsEven;
    procedure TestTheResolutionSurvives;
    procedure TestTheWriterLeavesTheImageAlone;
    procedure TestWritingAfterOtherDataKeepsIt;
    procedure TestMonochrome;
    procedure TestSixteenColorPlanes;
    procedure TestFourBitsInOnePlane;
    procedure TestTwoBitsInOnePlane;
    procedure TestARunOfNothingIsSkipped;
    procedure TestAnUnsupportedLayoutRaises;
    procedure TestEightBitWithPalette;
    procedure TestEightBitReaderStopsAfterThePalette;
    procedure TestRLERunsAndEscapedBytes;
    procedure TestAFailedReadDoesNotLeak;
    procedure TestContentsCheck;
  end;

implementation

procedure TTestPCX.SetUp;

begin
  inherited SetUp;
  FReader := TFPReaderPCX.Create;
  FWriter := TFPWriterPCX.Create;
  FStream := TMemoryStream.Create;
end;


procedure TTestPCX.TearDown;

begin
  FreeAndNil(FStream);
  FreeAndNil(FRead);
  FreeAndNil(FImage);
  FreeAndNil(FWriter);
  FreeAndNil(FReader);
  inherited TearDown;
end;


procedure TTestPCX.Build(aBitsPerPixel, aPlanes: Byte; aWidth, aHeight, aBytesPerLine: Word;
  aEncoding: Byte; const aData: array of Byte);

var
  lHeader: TPCXHeader;
  I: Integer;

begin
  FillChar(lHeader, SizeOf(lHeader), 0);
  lHeader.FileID := $0A;
  lHeader.Version := 5;
  lHeader.Encoding := aEncoding;
  lHeader.BitsPerPixel := aBitsPerPixel;
  lHeader.XMax := aWidth - 1;
  lHeader.YMax := aHeight - 1;
  lHeader.HRes := 72;
  lHeader.VRes := 72;
  lHeader.ColorPlanes := aPlanes;
  lHeader.BytesPerLine := aBytesPerLine;
  lHeader.PaletteType := 1;
  for I := 0 to 15 do
    begin
    lHeader.ColorMap[I].Red := I * 17;
    lHeader.ColorMap[I].Green := 255 - I * 17;
    lHeader.ColorMap[I].Blue := (I * 50) and $FF;
    end;
  FStream.Clear;
  FStream.WriteBuffer(lHeader, SizeOf(lHeader));
  if Length(aData) > 0 then
    FStream.WriteBuffer(aData[0], Length(aData));
  FStream.Position := 0;
end;


function TTestPCX.ReadHeader: TPCXHeader;

begin
  FStream.Position := 0;
  FStream.ReadBuffer(Result, SizeOf(Result));
end;


procedure TTestPCX.ReadIt;

begin
  FreeAndNil(FRead);
  FRead := TFPMemoryImage.Create(0, 0);
  FRead.LoadFromStream(FStream, FReader);
end;


procedure TTestPCX.ReadTruncatedSafely;

var
  lReader: TFPReaderPCX;
  lImage: TFPMemoryImage;

begin
  lReader := TFPReaderPCX.Create;
  lImage := TFPMemoryImage.Create(0, 0);
  try
    FStream.Position := 0;
    try
      lImage.LoadFromStream(FStream, lReader);
    except
      on EStreamError do ;
    end;
  finally
    lImage.Free;
    lReader.Free;
  end;
end;


procedure TTestPCX.CheckColor(const aMessage: String; aX, aY: Integer; const aColor: TFPColor);

begin
  AssertColorsEqual(aMessage, aColor, FRead[aX, aY]);
end;


procedure TTestPCX.TestRoundTripKeepsTheColors;

begin
  FImage := CreateGradientImage(18, 7);
  FRead := RoundTrip(FImage, FWriter, FReader);
  AssertImagesEqual('An uncompressed PCX keeps every colour', FImage, FRead);
end;


procedure TTestPCX.TestCompressedRoundTripKeepsTheColors;

begin
  FImage := CreateCheckerImage(40, 9, 5, RGB8(200, 201, 202), RGB8(10, 250, 3));
  FImage[3, 3] := RGB8(255, 192, 193);
  FWriter.Compressed := True;
  FRead := RoundTrip(FImage, FWriter, FReader);
  AssertImagesEqual('A compressed PCX keeps every colour', FImage, FRead);
end;


procedure TTestPCX.TestRoundTripOfEveryWidth;

var
  lWidth: Integer;
  lCompressed: Boolean;

begin
  for lCompressed := False to True do
    for lWidth := 1 to 5 do
      begin
      FreeAndNil(FImage);
      FreeAndNil(FRead);
      FImage := CreateGradientImage(lWidth, 3);
      FWriter.Compressed := lCompressed;
      FRead := RoundTrip(FImage, FWriter, FReader);
      AssertImagesEqual(Format('A PCX of width %d, compressed %s', [lWidth, BoolToStr(lCompressed, True)]), FImage, FRead);
      end;
end;


procedure TTestPCX.TestCompressionMakesASolidImageSmaller;

var
  lPlain: Int64;

begin
  FImage := CreateSolidImage(64, 64, colRed);
  lPlain := WriteImage(FImage, FWriter, FStream);
  FStream.Clear;
  FWriter.Compressed := True;
  AssertTrue('Compressing one colour gives a smaller file', WriteImage(FImage, FWriter, FStream) < lPlain div 4);
end;


procedure TTestPCX.TestTheHeaderWritten;

var
  lHeader: TPCXHeader;

begin
  FImage := CreateGradientImage(10, 4);
  FImage.SaveToStream(FStream, FWriter);
  lHeader := ReadHeader;
  AssertEquals('File id', $0A, lHeader.FileID);
  AssertEquals('Version 5', 5, lHeader.Version);
  AssertEquals('Not compressed by default', 0, lHeader.Encoding);
  AssertEquals('8 bits per plane', 8, lHeader.BitsPerPixel);
  AssertEquals('Three planes', 3, lHeader.ColorPlanes);
  AssertEquals('XMax', 9, lHeader.XMax);
  AssertEquals('YMax', 3, lHeader.YMax);
  AssertEquals('Header and three planes of every row', 128 + 10 * 3 * 4, FStream.Size);
end;


procedure TTestPCX.TestBytesPerLineIsEven;

begin
  FImage := CreateGradientImage(5, 2);
  FImage.SaveToStream(FStream, FWriter);
  AssertEquals('BytesPerLine is even, as the format requires', 0, ReadHeader.BytesPerLine mod 2);
end;


procedure TTestPCX.TestTheResolutionSurvives;

begin
  FImage := CreateGradientImage(3, 3);
  FImage.ResolutionUnit := ruPixelsPerInch;
  FImage.ResolutionX := 300;
  FImage.ResolutionY := 150;
  FRead := RoundTrip(FImage, FWriter, FReader);
  AssertTrue('The unit is the inch', FRead.ResolutionUnit = ruPixelsPerInch);
  AssertEquals('Horizontal resolution', 300, FRead.ResolutionX, 0.001);
  AssertEquals('Vertical resolution', 150, FRead.ResolutionY, 0.001);
end;


procedure TTestPCX.TestTheWriterLeavesTheImageAlone;

begin
  FImage := CreateGradientImage(3, 3);
  FImage.ResolutionUnit := ruPixelsPerCentimeter;
  FImage.ResolutionX := 100;
  FImage.ResolutionY := 100;
  FImage.SaveToStream(FStream, FWriter);
  AssertTrue('Writing keeps the resolution unit of the image', FImage.ResolutionUnit = ruPixelsPerCentimeter);
  AssertEquals('Writing keeps the resolution of the image', 100, FImage.ResolutionX, 0.001);
end;


procedure TTestPCX.TestWritingAfterOtherDataKeepsIt;

begin
  FImage := CreateGradientImage(4, 3);
  FStream.WriteBuffer(PChar('prefix')^, 6);
  WriteImage(FImage, FWriter, FStream);
  AssertTrue('The bytes before the image are kept', CompareMem(FStream.Memory, PChar('prefix'), 6));
  FStream.Position := 6;
  ReadIt;
  AssertImagesEqual('The image after the prefix reads back', FImage, FRead);
  AssertEquals('The reader stops at the end of the image', FStream.Size, FStream.Position);
end;


procedure TTestPCX.TestMonochrome;

begin
  { 10x1, 1 bit, one plane, 2 bytes per line: bits 1011000001 }
  Build(1, 1, 10, 1, 2, 0, [$B0, $40]);
  ReadIt;
  CheckColor('Bit 0 set is white', 0, 0, colWhite);
  CheckColor('Bit 1 clear is black', 1, 0, colBlack);
  CheckColor('Bit 9 set is white', 9, 0, colWhite);
end;


procedure TTestPCX.TestSixteenColorPlanes;

begin
  { 8x1, 1 bit, four planes: pixel 0 has index 15, pixel 1 index 1, pixel 2 index 8 }
  Build(1, 4, 8, 1, 2, 0, [$C0, 0, $80, 0, $80, 0, $A0, 0]);
  ReadIt;
  CheckColor('Index 15 is entry 15 of the header map', 0, 0, RGB8(255, 0, (15 * 50) and $FF));
  CheckColor('Index 1 is entry 1 of the header map', 1, 0, RGB8(17, 238, 50));
  CheckColor('Index 8 is entry 8 of the header map', 2, 0, RGB8(136, 119, (8 * 50) and $FF));
end;


procedure TTestPCX.ReadSixteenBits;

begin
  Build(16, 1, 1, 1, 2, 0, [0, 0]);
  ReadIt;
end;


procedure TTestPCX.TestFourBitsInOnePlane;

begin
  { 3x1, 4 bits packed: indices 15, 1, 8 }
  Build(4, 1, 3, 1, 2, 0, [$F1, $80]);
  ReadIt;
  CheckColor('Index 15', 0, 0, RGB8(255, 0, (15 * 50) and $FF));
  CheckColor('Index 1', 1, 0, RGB8(17, 238, 50));
  CheckColor('Index 8', 2, 0, RGB8(136, 119, (8 * 50) and $FF));
end;


procedure TTestPCX.TestTwoBitsInOnePlane;

begin
  { 4x1, 2 bits packed: indices 3, 0, 2, 1 }
  Build(2, 1, 4, 1, 2, 0, [$C9, $00]);
  ReadIt;
  CheckColor('Index 3', 0, 0, RGB8(51, 204, 150));
  CheckColor('Index 0', 1, 0, RGB8(0, 255, 0));
  CheckColor('Index 2', 2, 0, RGB8(34, 221, 100));
  CheckColor('Index 1', 3, 0, RGB8(17, 238, 50));
end;


procedure TTestPCX.TestARunOfNothingIsSkipped;

begin
  { 24-bit 2x1, 2 bytes a plane: a run byte $C0 holds nothing }
  Build(8, 3, 2, 1, 2, 1, [$C0, 99, 10, 20, 30, 40, 50, 60]);
  ReadIt;
  CheckColor('Pixel 0 ignores the empty run', 0, 0, RGB8(10, 30, 50));
  CheckColor('Pixel 1', 1, 0, RGB8(20, 40, 60));
end;


procedure TTestPCX.TestAnUnsupportedLayoutRaises;

begin
  AssertRaises('16 bits in one plane is rejected', FPImageException, @ReadSixteenBits);
end;


procedure TTestPCX.TestEightBitWithPalette;

var
  lData: TBytes;
  I: Integer;

begin
  SetLength(lData, 4 + 1 + 768);
  lData[0] := 0;
  lData[1] := 1;
  lData[2] := 255;
  lData[3] := 0;
  lData[4] := $0C;
  for I := 0 to 255 do
    begin
    lData[5 + I * 3] := I;
    lData[6 + I * 3] := 255 - I;
    lData[7 + I * 3] := 128;
    end;
  Build(8, 1, 3, 1, 4, 0, lData);
  ReadIt;
  CheckColor('Index 0', 0, 0, RGB8(0, 255, 128));
  CheckColor('Index 1', 1, 0, RGB8(1, 254, 128));
  CheckColor('Index 255', 2, 0, RGB8(255, 0, 128));
end;


procedure TTestPCX.TestEightBitReaderStopsAfterThePalette;

var
  lData: TBytes;

begin
  SetLength(lData, 2 + 1 + 768);
  FillChar(lData[0], Length(lData), 0);
  lData[2] := $0C;
  Build(8, 1, 2, 1, 2, 0, lData);
  FStream.Position := FStream.Size;
  FStream.WriteBuffer(PChar('next')^, 4);
  FStream.Position := 0;
  ReadIt;
  AssertEquals('The reader stops after the palette, before what follows', FStream.Size - 4, FStream.Position);
end;


procedure TTestPCX.TestRLERunsAndEscapedBytes;

begin
  { 6x1 24-bit, 6 bytes per plane: red plane is a run of 6 x 200;
    green plane: $C1 $C5 escapes the byte $C5, then 5 x 7; blue plane raw }
  Build(8, 3, 6, 1, 6, 1,
    [$C6, 200,
     $C1, $C5, $C5, 7,
     1, 2, 3, 4, 5, 6]);
  ReadIt;
  CheckColor('Pixel 0', 0, 0, RGB8(200, $C5, 1));
  CheckColor('Pixel 1', 1, 0, RGB8(200, 7, 2));
  CheckColor('Pixel 5', 5, 0, RGB8(200, 7, 6));
end;


procedure TTestPCX.TestAFailedReadDoesNotLeak;

begin
  Build(8, 3, 20, 20, 20, 0, [1, 2, 3]);
  AssertNoLeak('A read that fails on a short file frees its buffers', @ReadTruncatedSafely);
end;


procedure TTestPCX.TestContentsCheck;

var
  lHeader: TPCXHeader;

begin
  Build(8, 3, 1, 1, 2, 0, [0, 0, 0, 0, 0, 0]);
  AssertTrue('A valid header is accepted', FReader.CheckContents(FStream));
  AssertEquals('The check leaves the position alone', 0, FStream.Position);
  lHeader := ReadHeader;
  lHeader.FileID := $0B;
  FStream.Position := 0;
  FStream.WriteBuffer(lHeader, SizeOf(lHeader));
  FStream.Position := 0;
  AssertFalse('Another file id is rejected', FReader.CheckContents(FStream));
end;


initialization
  RegisterTest('pcx', TTestPCX);
end.
