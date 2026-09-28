{
    Tests for the Radiance HDR (RGBE) reader and writer: the conversion of
    pixels, flat and run-length encoded scanlines, and orientations.
    See the file COPYING.FPC, included in this distribution, for details.
}
unit tchdr;

{$mode objfpc}{$H+}

interface

uses sysutils, classes, fpcunit, testregistry, fpimage, fpimgtests,
     hdrcomn, fpreadhdr, fpwritehdr;

type
  TTestHDR = class(TTestCase)
  private
    FReader: TFPReaderHDR;
    FImage: TFPMemoryImage;
    FRead: TFPMemoryImage;
    FStream: TMemoryStream;
    // Replaces the stream with a header of the given resolution line, followed by the bytes.
    procedure Build(const aResolution: String; const aBytes: array of Byte; const aFormat: String = HDRFormatRGBE);
    procedure ReadIt;
    // Writes FImage with a writer of the given compression and returns what was written.
    function WriteIt(aCompressed: Boolean): TBytes;
    procedure ReadTruncated;
    procedure ReadXYZE;
    procedure ReadLongRun;
    procedure ReadBadResolution;
    procedure CheckColor(const aMessage: String; aX, aY: Integer; const aColor: TFPColor);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRGBEToColor;
    procedure TestZeroExponentIsBlack;
    procedure TestValuesAboveOneAreClamped;
    procedure TestColorToRGBE;
    procedure TestBlackToRGBE;
    procedure TestContentsCheck;
    procedure TestReadFlatScanlines;
    procedure TestReadOldRepeat;
    procedure TestReadRunLengthScanline;
    procedure TestReadBottomUp;
    procedure TestReadRightToLeft;
    procedure TestReadColumns;
    procedure TestReadTruncatedRaises;
    procedure TestReadXYZERaises;
    procedure TestReadRunPastTheEndRaises;
    procedure TestReadBadResolutionRaises;
    procedure TestWrittenHeader;
    procedure TestRoundTripIsClose;
    procedure TestCompressedEqualsFlat;
    procedure TestNarrowImagesAreFlat;
    procedure TestTwoImagesInOneStream;
    procedure TestHDRIsRegistered;
  end;

implementation

const
  HDRHeader = HDRMagic + #10'# test'#10;

procedure TTestHDR.SetUp;

begin
  inherited SetUp;
  FReader := TFPReaderHDR.Create;
  FStream := TMemoryStream.Create;
end;


procedure TTestHDR.TearDown;

begin
  FreeAndNil(FStream);
  FreeAndNil(FRead);
  FreeAndNil(FImage);
  FreeAndNil(FReader);
  inherited TearDown;
end;


procedure TTestHDR.Build(const aResolution: String; const aBytes: array of Byte; const aFormat: String);

var
  lText: String;

begin
  FStream.Clear;
  lText := HDRHeader + 'FORMAT=' + aFormat + #10#10 + aResolution + #10;
  FStream.WriteBuffer(lText[1], Length(lText));
  if Length(aBytes) > 0 then
    FStream.WriteBuffer(aBytes[0], Length(aBytes));
  FStream.Position := 0;
end;


procedure TTestHDR.ReadIt;

begin
  FreeAndNil(FRead);
  FRead := TFPMemoryImage.Create(0, 0);
  FRead.LoadFromStream(FStream, FReader);
end;


function TTestHDR.WriteIt(aCompressed: Boolean): TBytes;

var
  lWriter: TFPWriterHDR;

begin
  lWriter := TFPWriterHDR.Create;
  try
    lWriter.Compressed := aCompressed;
    FStream.Clear;
    FImage.SaveToStream(FStream, lWriter);
  finally
    lWriter.Free;
  end;
  SetLength(Result, FStream.Size);
  Move(FStream.Memory^, Result[0], FStream.Size);
  FStream.Position := 0;
end;


procedure TTestHDR.ReadTruncated;

begin
  Build('-Y 1 +X 2', [128, 0, 0, 129, 0, 128]);
  ReadIt;
end;


procedure TTestHDR.ReadXYZE;

begin
  Build('-Y 1 +X 1', [128, 0, 0, 129], HDRFormatXYZE);
  ReadIt;
end;


procedure TTestHDR.ReadLongRun;

begin
  Build('-Y 1 +X 8', [2, 2, 0, 8, 128 + 9, 1]);
  ReadIt;
end;


procedure TTestHDR.ReadBadResolution;

begin
  Build('-Y 1 -Y 1', [128, 0, 0, 129]);
  ReadIt;
end;


procedure TTestHDR.CheckColor(const aMessage: String; aX, aY: Integer; const aColor: TFPColor);

begin
  AssertColorsEqual(aMessage, aColor, FRead[aX, aY]);
end;


// Returns an RGBE pixel.
function Pixel(aR, aG, aB, aE: Byte): TRGBE;

begin
  Result.R := aR;
  Result.G := aG;
  Result.B := aB;
  Result.E := aE;
end;


procedure TTestHDR.TestRGBEToColor;

begin
  AssertColorsEqual('A mantissa of 128 with an exponent of 129 is 1', FPColor(65535, 32768, 0),
    RGBEToColor(Pixel(128, 64, 0, 129)));
end;


procedure TTestHDR.TestZeroExponentIsBlack;

begin
  AssertColorsEqual('An exponent of 0 is black', colBlack, RGBEToColor(Pixel(200, 200, 200, 0)));
end;


procedure TTestHDR.TestValuesAboveOneAreClamped;

begin
  AssertColorsEqual('Values above 1 become 65535', FPColor(65535, 65535, 0), RGBEToColor(Pixel(255, 128, 0, 130)));
end;


procedure TTestHDR.TestColorToRGBE;

var
  lPixel: TRGBE;

begin
  lPixel := ColorToRGBE(FPColor(65535, 32768, 0));
  AssertEquals('The largest component has a mantissa of 128', 128, lPixel.R);
  AssertEquals('Half of it has a mantissa of 64', 64, lPixel.G);
  AssertEquals('Zero has a mantissa of 0', 0, lPixel.B);
  AssertEquals('The exponent of 1 is 129', 129, lPixel.E);
end;


procedure TTestHDR.TestBlackToRGBE;

var
  lPixel: TRGBE;

begin
  lPixel := ColorToRGBE(colBlack);
  AssertEquals('Black has an exponent of 0', 0, lPixel.E);
  AssertEquals('and a mantissa of 0', 0, lPixel.R);
end;


procedure TTestHDR.TestContentsCheck;

var
  lText: String;

begin
  Build('-Y 1 +X 1', [128, 0, 0, 129]);
  AssertTrue('#?RADIANCE is accepted', FReader.CheckContents(FStream));
  AssertEquals('The check leaves the position', 0, FStream.Position);
  FStream.Clear;
  lText := '#?RGBE'#10#10'-Y 1 +X 1'#10;
  FStream.WriteBuffer(lText[1], Length(lText));
  FStream.Position := 0;
  AssertTrue('#?RGBE is accepted', FReader.CheckContents(FStream));
  FStream.Clear;
  lText := '#?RADIANCEX'#10;
  FStream.WriteBuffer(lText[1], Length(lText));
  FStream.Position := 0;
  AssertFalse('Another first line is rejected', FReader.CheckContents(FStream));
end;


procedure TTestHDR.TestReadFlatScanlines;

begin
  Build('-Y 2 +X 2', [128, 0, 0, 129, 0, 128, 0, 129, 0, 0, 128, 129, 0, 0, 0, 0]);
  ReadIt;
  AssertEquals('The width', 2, FRead.Width);
  AssertEquals('The height', 2, FRead.Height);
  CheckColor('Pixel 0 is red', 0, 0, colRed);
  CheckColor('Pixel 1 is green', 1, 0, colGreen);
  CheckColor('Pixel 2 is blue', 0, 1, colBlue);
  CheckColor('Pixel 3 is black', 1, 1, colBlack);
end;


procedure TTestHDR.TestReadOldRepeat;

begin
  { a pixel, then 1,1,1,n repeating it n times }
  Build('-Y 1 +X 4', [0, 128, 0, 129, 1, 1, 1, 2, 128, 0, 0, 129]);
  ReadIt;
  CheckColor('The first pixel', 0, 0, colGreen);
  CheckColor('A repeated pixel', 1, 0, colGreen);
  CheckColor('The second repeated pixel', 2, 0, colGreen);
  CheckColor('The pixel after the repeat', 3, 0, colRed);
end;


procedure TTestHDR.TestReadRunLengthScanline;

begin
  Build('-Y 1 +X 8', [2, 2, 0, 8,
                      128 + 8, 128,
                      8, 0, 16, 32, 48, 64, 80, 96, 128,
                      128 + 8, 0,
                      128 + 8, 129]);
  ReadIt;
  CheckColor('A run of red', 0, 0, colRed);
  CheckColor('with literal green values', 4, 0, FPColor(65535, 32768, 0));
  CheckColor('up to the end of the scanline', 7, 0, colYellow);
end;


procedure TTestHDR.TestReadBottomUp;

begin
  Build('+Y 2 +X 1', [128, 0, 0, 129, 0, 0, 128, 129]);
  ReadIt;
  CheckColor('+Y: the first scanline is the bottom row', 0, 1, colRed);
  CheckColor('and the second is the top row', 0, 0, colBlue);
end;


procedure TTestHDR.TestReadRightToLeft;

begin
  Build('-Y 1 -X 2', [128, 0, 0, 129, 0, 0, 128, 129]);
  ReadIt;
  CheckColor('-X: the first pixel is on the right', 1, 0, colRed);
  CheckColor('and the second on the left', 0, 0, colBlue);
end;


procedure TTestHDR.TestReadColumns;

begin
  Build('+X 2 -Y 1', [128, 0, 0, 129, 0, 0, 128, 129]);
  ReadIt;
  AssertEquals('+X first: the scanlines are columns, the width is the first count', 2, FRead.Width);
  AssertEquals('and the height the second', 1, FRead.Height);
  CheckColor('The first column', 0, 0, colRed);
  CheckColor('The second column', 1, 0, colBlue);
end;


procedure TTestHDR.TestReadTruncatedRaises;

begin
  AssertRaises('Missing pixel data raises', FPImageException, @ReadTruncated);
end;


procedure TTestHDR.TestReadXYZERaises;

begin
  AssertRaises('The XYZE format raises', FPImageException, @ReadXYZE);
end;


procedure TTestHDR.TestReadRunPastTheEndRaises;

begin
  AssertRaises('A run past the end of the scanline raises', FPImageException, @ReadLongRun);
end;


procedure TTestHDR.TestReadBadResolutionRaises;

begin
  AssertRaises('A resolution line with two Y axes raises', FPImageException, @ReadBadResolution);
end;


procedure TTestHDR.TestWrittenHeader;

var
  lBytes: TBytes;
  lText, lExpected: String;

begin
  FImage := CreateSolidImage(3, 2, colRed);
  lBytes := WriteIt(True);
  lExpected := '#?RADIANCE'#10'FORMAT=32-bit_rle_rgbe'#10#10'-Y 2 +X 3'#10;
  SetString(lText, PChar(@lBytes[0]), Length(lExpected));
  AssertEquals('The header, top to bottom and left to right', lExpected, lText);
end;


procedure TTestHDR.TestRoundTripIsClose;

begin
  FImage := CreateGradientImage(40, 9);
  WriteIt(True);
  ReadIt;
  AssertImagesEqual('Each component is within 1/128 of the largest one', FImage, FRead, 512);
  AssertEquals('The alpha read is opaque', AlphaOpaque, FRead[0, 0].Alpha);
end;


procedure TTestHDR.TestCompressedEqualsFlat;

var
  lCompressed, lFlat: TBytes;
  lFlatImage: TFPMemoryImage;

begin
  FImage := CreateGradientImage(64, 5);
  FImage.Colors[10, 2] := colRed;
  lFlat := WriteIt(False);
  lFlatImage := TFPMemoryImage.Create(0, 0);
  try
    lFlatImage.LoadFromStream(FStream, FReader);
    lCompressed := WriteIt(True);
    ReadIt;
    AssertImagesEqual('A run-length encoded image reads as the flat one', lFlatImage, FRead);
  finally
    lFlatImage.Free;
  end;
  AssertTrue('Encoding by component makes a gradient smaller', Length(lCompressed) < Length(lFlat));
end;


procedure TTestHDR.TestNarrowImagesAreFlat;

var
  lBytes: TBytes;

begin
  FImage := CreateSolidImage(7, 3, colRed);
  lBytes := WriteIt(True);
  AssertEquals('A scanline of 7 pixels is written flat', Length('#?RADIANCE'#10'FORMAT=32-bit_rle_rgbe'#10#10'-Y 3 +X 7'#10) + 7 * 3 * 4,
    Length(lBytes));
end;


procedure TTestHDR.TestTwoImagesInOneStream;

var
  lWriter: TFPWriterHDR;
  lSecond: TFPMemoryImage;

begin
  FImage := CreateSolidImage(9, 2, colRed);
  lSecond := CreateSolidImage(3, 4, colBlue);
  lWriter := TFPWriterHDR.Create;
  try
    WriteImage(FImage, lWriter, FStream);
    WriteImage(lSecond, lWriter, FStream);
  finally
    lWriter.Free;
    lSecond.Free;
  end;
  FStream.Position := 0;
  ReadIt;
  CheckColor('The first image', 8, 1, colRed);
  ReadIt;
  AssertEquals('The second image follows the first', 3, FRead.Width);
  CheckColor('The second image', 2, 3, colBlue);
end;


procedure TTestHDR.TestHDRIsRegistered;

begin
  AssertTrue('HDR has a reader', ImageHandlers.ImageReader['Radiance HDR'] = TFPReaderHDR);
  AssertTrue('HDR has a writer', ImageHandlers.ImageWriter['Radiance HDR'] = TFPWriterHDR);
end;


initialization
  RegisterTest('hdr', TTestHDR);
end.
