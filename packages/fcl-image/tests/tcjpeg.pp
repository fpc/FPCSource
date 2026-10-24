{
    Tests for the JPEG reader and writer: round trips judged by PSNR, options,
    scaling, EXIF orientation, streams, the CMYK example and damaged input.
    See the file COPYING.FPC, included in this distribution, for details.
}
unit tcjpeg;

{$mode objfpc}{$H+}

interface

uses sysutils, classes, fpcunit, testregistry, fpimage, fpimgtests,
     jpegcomn, fpreadjpeg, fpwritejpeg;

type
  TTestJPEG = class(TTestCase)
  private
    FReader: TFPReaderJPEG;
    FWriter: TFPWriterJPEG;
    FImage: TFPMemoryImage;
    FRead: TFPMemoryImage;
    FStream: TMemoryStream;
    // Replaces the stream with FImage written by FWriter.
    procedure WriteIt;
    // Reads the stream from its start into FRead.
    procedure ReadIt;
    // Writes FImage at aQuality, reads it back and returns the byte count.
    function WriteAndReadAt(aQuality: Integer): Int64;
    // Keeps the first aSize bytes of the stream.
    procedure CutStream(aSize: Int64);
    // Reads the stream, expected to raise.
    procedure ReadDamaged;
    // Inserts an EXIF APP1 segment with aOrientation after the start marker.
    procedure InsertExif(aOrientation: Word);
    // Reads the stream with a Scale and checks the size of the result.
    procedure CheckScale(aScale: TJPEGScale; aWidth, aHeight: Integer);
    // Reads the stream with MinWidth and MinHeight and checks the size of the result.
    procedure CheckMinSize(aMinWidth, aMinHeight, aWidth, aHeight: Integer);
    // Fails unless FRead has the colour aColor at (aX, aY) within aTolerance 8-bit levels.
    procedure CheckColor(const aMessage: String; aX, aY: Integer; const aColor: TFPColor; aTolerance: Integer);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRoundTripKeepsTheSize;
    procedure TestQuality100OnASmoothGradient;
    procedure TestQuality75OnASmoothGradient;
    procedure TestHigherQualityGivesLargerFilesAndBetterImages;
    procedure TestAlphaIsNotStored;
    procedure TestGrayScaleWriterGivesGrayPixels;
    procedure TestTheReaderReportsAGrayFile;
    procedure TestProgressiveEncodingIsReadable;
    procedure TestReadHalfSize;
    procedure TestReadQuarterSize;
    procedure TestReadEighthSize;
    procedure TestScaledSizesRoundUp;
    procedure TestScaledImageKeepsTheColor;
    procedure TestMinSizePicksTheSmallestScaleAboveIt;
    procedure TestMinSizeLargerThanTheImageKeepsTheFullSize;
    procedure TestMinSizeDoesNotStickToTheReader;
    procedure TestExifOrientationNormal;
    procedure TestExifOrientationMirrored;
    procedure TestExifOrientationRotated;
    procedure TestTheResolutionInInches;
    procedure TestTheResolutionInCentimeters;
    procedure TestDensityUnits;
    procedure TestWritingAfterOtherDataKeepsIt;
    procedure TestReadingAfterOtherData;
    procedure TestTheReaderStopsAtTheEndOfTheImage;
    procedure TestTwoImagesInOneStream;
    procedure TestWritingToAWriteOnlyStream;
    procedure TestImageSizeWithoutReading;
    procedure TestContentsCheck;
    procedure TestOtherDataIsRejected;
    procedure TestACutHeaderRaises;
    procedure TestACutScanIsHandled;
    procedure TestTheCMYKExample;
  end;

implementation

const
  cPrefix: array[0..4] of Byte = (1, 2, 3, 4, 5);

// An opaque image with every channel changing slowly and linearly.
function CreateSmoothImage(aWidth, aHeight: Integer): TFPMemoryImage;

var
  lX, lY: Integer;

begin
  Result := TFPMemoryImage.Create(aWidth, aHeight);
  for lY := 0 to aHeight - 1 do
    for lX := 0 to aWidth - 1 do
      Result.Colors[lX, lY] := RGB8(40 + (lX * 170) div (aWidth - 1),
        30 + (lY * 190) div (aHeight - 1),
        60 + ((lX + lY) * 120) div (aWidth + aHeight - 2));
end;


// The colour of cell aIndex of the image made by CreateCellImage.
function CellColor(aIndex: Integer): TFPColor;

begin
  case aIndex of
    0: Result := RGB8(220, 30, 30);
    1: Result := RGB8(30, 200, 30);
    2: Result := RGB8(30, 30, 220);
    3: Result := RGB8(230, 220, 40);
    4: Result := RGB8(40, 220, 220);
  else
    Result := RGB8(210, 40, 210);
  end;
end;


// A 48x32 image of six solid cells of 16x16 pixels.
function CreateCellImage: TFPMemoryImage;

var
  lX, lY: Integer;

begin
  Result := TFPMemoryImage.Create(48, 32);
  for lY := 0 to 31 do
    for lX := 0 to 47 do
      Result.Colors[lX, lY] := CellColor((lY div 16) * 3 + lX div 16);
end;


type
  TJPEGReaderAccess = class(TFPReaderJPEG);


procedure TTestJPEG.SetUp;

begin
  inherited SetUp;
  FReader := TFPReaderJPEG.Create;
  FWriter := TFPWriterJPEG.Create;
  FStream := TMemoryStream.Create;
end;


procedure TTestJPEG.TearDown;

begin
  FreeAndNil(FStream);
  FreeAndNil(FRead);
  FreeAndNil(FImage);
  FreeAndNil(FWriter);
  FreeAndNil(FReader);
  inherited TearDown;
end;


procedure TTestJPEG.WriteIt;

begin
  FStream.Clear;
  FImage.SaveToStream(FStream, FWriter);
end;


procedure TTestJPEG.ReadIt;

begin
  FreeAndNil(FRead);
  FRead := TFPMemoryImage.Create(0, 0);
  FStream.Position := 0;
  FRead.LoadFromStream(FStream, FReader);
end;


function TTestJPEG.WriteAndReadAt(aQuality: Integer): Int64;

begin
  FWriter.CompressionQuality := aQuality;
  WriteIt;
  Result := FStream.Size;
  ReadIt;
end;


procedure TTestJPEG.CutStream(aSize: Int64);

begin
  FStream.Size := aSize;
  FStream.Position := 0;
end;


procedure TTestJPEG.ReadDamaged;

begin
  ReadIt;
end;


procedure TTestJPEG.InsertExif(aOrientation: Word);

const
  cExif: array[0..35] of Byte = (
    $FF, $E1, 0, 34,
    Ord('E'), Ord('x'), Ord('i'), Ord('f'), 0, 0,
    Ord('I'), Ord('I'), 42, 0, 8, 0, 0, 0,
    1, 0,
    $12, $01, 3, 0, 1, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0);

var
  lExif: array[0..35] of Byte;
  lCopy: TMemoryStream;

begin
  lExif := cExif;
  lExif[28] := Lo(aOrientation);
  lExif[29] := Hi(aOrientation);
  lCopy := TMemoryStream.Create;
  try
    lCopy.WriteBuffer(FStream.Memory^, 2);
    lCopy.WriteBuffer(lExif, SizeOf(lExif));
    lCopy.WriteBuffer((PByte(FStream.Memory) + 2)^, FStream.Size - 2);
    FStream.Clear;
    FStream.CopyFrom(lCopy, 0);
  finally
    lCopy.Free;
  end;
end;


procedure TTestJPEG.CheckScale(aScale: TJPEGScale; aWidth, aHeight: Integer);

begin
  FReader.Scale := aScale;
  ReadIt;
  AssertEquals(Format('Scale %d gives the width', [Ord(aScale)]), aWidth, FRead.Width);
  AssertEquals(Format('Scale %d gives the height', [Ord(aScale)]), aHeight, FRead.Height);
end;


procedure TTestJPEG.CheckMinSize(aMinWidth, aMinHeight, aWidth, aHeight: Integer);

begin
  FReader.Scale := jsFullSize;
  FReader.MinWidth := aMinWidth;
  FReader.MinHeight := aMinHeight;
  ReadIt;
  AssertEquals(Format('A minimum of %dx%d gives the width', [aMinWidth, aMinHeight]), aWidth, FRead.Width);
  AssertEquals(Format('A minimum of %dx%d gives the height', [aMinWidth, aMinHeight]), aHeight, FRead.Height);
end;


procedure TTestJPEG.CheckColor(const aMessage: String; aX, aY: Integer; const aColor: TFPColor; aTolerance: Integer);

begin
  AssertColorsEqual(aMessage, aColor, FRead[aX, aY], aTolerance * 257);
end;


procedure TTestJPEG.TestRoundTripKeepsTheSize;

begin
  FImage := CreateGradientImage(37, 21);
  FRead := RoundTrip(FImage, FWriter, FReader);
  AssertEquals('The width comes back', 37, FRead.Width);
  AssertEquals('The height comes back', 21, FRead.Height);
end;


procedure TTestJPEG.TestQuality100OnASmoothGradient;

var
  lPSNR: Double;

begin
  FImage := CreateSmoothImage(96, 64);
  WriteAndReadAt(100);
  lPSNR := ImagePSNR(FImage, FRead);
  AssertTrue(Format('Quality 100 on a smooth gradient gives more than 40 dB, got %.2f', [lPSNR]), lPSNR > 40);
end;


procedure TTestJPEG.TestQuality75OnASmoothGradient;

var
  lPSNR: Double;

begin
  FImage := CreateSmoothImage(96, 64);
  WriteAndReadAt(75);
  lPSNR := ImagePSNR(FImage, FRead);
  AssertTrue(Format('Quality 75 on a smooth gradient gives more than 30 dB, got %.2f', [lPSNR]), lPSNR > 30);
end;


procedure TTestJPEG.TestHigherQualityGivesLargerFilesAndBetterImages;

const
  cQualities: array[0..4] of Integer = (20, 40, 60, 80, 95);

var
  I: Integer;
  lSize, lLastSize: Int64;
  lPSNR, lLastPSNR: Double;

begin
  FImage := CreateGradientImage(64, 64);
  lLastSize := 0;
  lLastPSNR := 0;
  for I := 0 to High(cQualities) do
    begin
    lSize := WriteAndReadAt(cQualities[I]);
    lPSNR := ImagePSNR(FImage, FRead);
    AssertTrue(Format('Quality %d gives a larger file than the quality before (%d > %d bytes)',
      [cQualities[I], lSize, lLastSize]), lSize > lLastSize);
    AssertTrue(Format('Quality %d gives a better image than the quality before (%.2f > %.2f dB)',
      [cQualities[I], lPSNR, lLastPSNR]), lPSNR > lLastPSNR);
    lLastSize := lSize;
    lLastPSNR := lPSNR;
    end;
end;


procedure TTestJPEG.TestAlphaIsNotStored;

var
  lX, lY: Integer;

begin
  FImage := CreateAlphaImage(16, 16);
  FRead := RoundTrip(FImage, FWriter, FReader);
  for lY := 0 to FRead.Height - 1 do
    for lX := 0 to FRead.Width - 1 do
      AssertEquals(Format('Pixel (%d,%d) read from a JPEG is opaque', [lX, lY]), alphaOpaque, FRead[lX, lY].Alpha);
end;


procedure TTestJPEG.TestGrayScaleWriterGivesGrayPixels;

var
  lX, lY: Integer;
  lColor: TFPColor;
  lGray: TFPMemoryImage;
  lPSNR: Double;

begin
  FImage := CreateSmoothImage(48, 32);
  FWriter.GrayScale := True;
  FWriter.CompressionQuality := 95;
  FRead := RoundTrip(FImage, FWriter, FReader);
  lGray := TFPMemoryImage.Create(FImage.Width, FImage.Height);
  try
    for lY := 0 to FRead.Height - 1 do
      for lX := 0 to FRead.Width - 1 do
        begin
        lColor := FRead[lX, lY];
        AssertTrue(Format('Pixel (%d,%d) is gray', [lX, lY]),
          (lColor.Red = lColor.Green) and (lColor.Green = lColor.Blue));
        lGray[lX, lY] := RGB8(CalculateGray(FImage[lX, lY]) shr 8, CalculateGray(FImage[lX, lY]) shr 8,
          CalculateGray(FImage[lX, lY]) shr 8);
        end;
    lPSNR := ImagePSNR(lGray, FRead);
    AssertTrue(Format('The gray levels are the luma of the colours, more than 35 dB, got %.2f', [lPSNR]), lPSNR > 35);
  finally
    lGray.Free;
  end;
end;


procedure TTestJPEG.TestTheReaderReportsAGrayFile;

begin
  FImage := CreateGrayImage(16, 16);
  WriteIt;
  ReadIt;
  AssertFalse('A colour JPEG is not reported as gray', FReader.GrayScale);
  FWriter.GrayScale := True;
  WriteIt;
  ReadIt;
  AssertTrue('A gray JPEG is reported as gray', FReader.GrayScale);
end;


procedure TTestJPEG.TestProgressiveEncodingIsReadable;

var
  lPSNR: Double;

begin
  FImage := CreateSmoothImage(96, 64);
  WriteAndReadAt(75);
  AssertFalse('A baseline JPEG is not reported as progressive', FReader.ProgressiveEncoding);
  FWriter.ProgressiveEncoding := True;
  WriteAndReadAt(75);
  AssertTrue('A progressive JPEG is reported as progressive', FReader.ProgressiveEncoding);
  AssertEquals('A progressive JPEG has the width', 96, FRead.Width);
  AssertEquals('A progressive JPEG has the height', 64, FRead.Height);
  lPSNR := ImagePSNR(FImage, FRead);
  AssertTrue(Format('A progressive JPEG at quality 75 gives more than 30 dB, got %.2f', [lPSNR]), lPSNR > 30);
end;


procedure TTestJPEG.TestReadHalfSize;

begin
  FImage := CreateSmoothImage(64, 48);
  WriteIt;
  CheckScale(jsHalf, 32, 24);
end;


procedure TTestJPEG.TestReadQuarterSize;

begin
  FImage := CreateSmoothImage(64, 48);
  WriteIt;
  CheckScale(jsQuarter, 16, 12);
end;


procedure TTestJPEG.TestReadEighthSize;

begin
  FImage := CreateSmoothImage(64, 48);
  WriteIt;
  CheckScale(jsEighth, 8, 6);
end;


procedure TTestJPEG.TestScaledSizesRoundUp;

begin
  FImage := CreateSmoothImage(50, 30);
  WriteIt;
  CheckScale(jsHalf, 25, 15);
  CheckScale(jsQuarter, 13, 8);
  CheckScale(jsEighth, 7, 4);
end;


procedure TTestJPEG.TestScaledImageKeepsTheColor;

var
  lX, lY: Integer;

begin
  FImage := CreateSolidImage(64, 48, RGB8(200, 100, 50));
  WriteIt;
  FReader.Scale := jsQuarter;
  ReadIt;
  for lY := 0 to FRead.Height - 1 do
    for lX := 0 to FRead.Width - 1 do
      CheckColor(Format('Pixel (%d,%d) of a quarter size solid image keeps the colour', [lX, lY]),
        lX, lY, RGB8(200, 100, 50), 6);
end;


procedure TTestJPEG.TestMinSizePicksTheSmallestScaleAboveIt;

begin
  FImage := CreateSmoothImage(64, 64);
  WriteIt;
  CheckMinSize(16, 16, 16, 16);
  CheckMinSize(20, 20, 32, 32);
  CheckMinSize(8, 8, 8, 8);
  CheckMinSize(30, 10, 32, 32);
  CheckMinSize(10, 30, 32, 32);
end;


procedure TTestJPEG.TestMinSizeLargerThanTheImageKeepsTheFullSize;

begin
  FImage := CreateSmoothImage(64, 64);
  WriteIt;
  CheckMinSize(100, 100, 64, 64);
end;


procedure TTestJPEG.TestMinSizeDoesNotStickToTheReader;

begin
  FImage := CreateSmoothImage(64, 64);
  WriteIt;
  FReader.MinWidth := 8;
  FReader.MinHeight := 8;
  ReadIt;
  FReader.MinWidth := 0;
  FReader.MinHeight := 0;
  ReadIt;
  AssertEquals('Without a minimum size the next read has the full width', 64, FRead.Width);
  AssertEquals('Without a minimum size the next read has the full height', 64, FRead.Height);
end;


procedure TTestJPEG.TestExifOrientationNormal;

var
  I: Integer;

begin
  FImage := CreateCellImage;
  FWriter.CompressionQuality := 95;
  WriteIt;
  InsertExif(1);
  ReadIt;
  AssertEquals('Orientation 1 is reported', Ord(eoNormal), Ord(TJPEGReaderAccess(FReader).Orientation));
  AssertEquals('Orientation 1 keeps the width', 48, FRead.Width);
  AssertEquals('Orientation 1 keeps the height', 32, FRead.Height);
  for I := 0 to 5 do
    CheckColor(Format('Orientation 1 keeps cell %d in place', [I]), 8 + 16 * (I mod 3), 8 + 16 * (I div 3), CellColor(I), 24);
end;


procedure TTestJPEG.TestExifOrientationMirrored;

var
  lOrientation, I, lC, lR, lX, lY: Integer;

begin
  FImage := CreateCellImage;
  FWriter.CompressionQuality := 95;
  for lOrientation := 2 to 4 do
    begin
    WriteIt;
    InsertExif(lOrientation);
    ReadIt;
    AssertEquals(Format('Orientation %d is reported', [lOrientation]), lOrientation, Ord(TJPEGReaderAccess(FReader).Orientation));
    AssertEquals(Format('Orientation %d keeps the width', [lOrientation]), 48, FRead.Width);
    AssertEquals(Format('Orientation %d keeps the height', [lOrientation]), 32, FRead.Height);
    for I := 0 to 5 do
      begin
      lC := 8 + 16 * (I mod 3);
      lR := 8 + 16 * (I div 3);
      case lOrientation of
        2: begin lX := 47 - lC; lY := lR; end;
        3: begin lX := 47 - lC; lY := 31 - lR; end;
      else
        begin lX := lC; lY := 31 - lR; end;
      end;
      CheckColor(Format('Orientation %d puts stored cell %d at (%d,%d)', [lOrientation, I, lX, lY]), lX, lY, CellColor(I), 24);
      end;
    end;
end;


procedure TTestJPEG.TestExifOrientationRotated;

var
  lOrientation, I, lC, lR, lX, lY: Integer;

begin
  FImage := CreateCellImage;
  FWriter.CompressionQuality := 95;
  for lOrientation := 5 to 8 do
    begin
    WriteIt;
    InsertExif(lOrientation);
    ReadIt;
    AssertEquals(Format('Orientation %d is reported', [lOrientation]), lOrientation, Ord(TJPEGReaderAccess(FReader).Orientation));
    AssertEquals(Format('Orientation %d swaps the width', [lOrientation]), 32, FRead.Width);
    AssertEquals(Format('Orientation %d swaps the height', [lOrientation]), 48, FRead.Height);
    for I := 0 to 5 do
      begin
      lC := 8 + 16 * (I mod 3);
      lR := 8 + 16 * (I div 3);
      case lOrientation of
        5: begin lX := lR; lY := lC; end;
        6: begin lX := 31 - lR; lY := lC; end;
        7: begin lX := 31 - lR; lY := 47 - lC; end;
      else
        begin lX := lR; lY := 47 - lC; end;
      end;
      CheckColor(Format('Orientation %d puts stored cell %d at (%d,%d)', [lOrientation, I, lX, lY]), lX, lY, CellColor(I), 24);
      end;
    end;
end;


procedure TTestJPEG.TestTheResolutionInInches;

begin
  FImage := CreateSmoothImage(16, 16);
  FImage.ResolutionUnit := ruPixelsPerInch;
  FImage.ResolutionX := 300;
  FImage.ResolutionY := 150;
  FRead := RoundTrip(FImage, FWriter, FReader);
  AssertTrue('The unit comes back as pixels per inch', FRead.ResolutionUnit = ruPixelsPerInch);
  AssertEquals('The horizontal resolution comes back', 300, FRead.ResolutionX, 0.001);
  AssertEquals('The vertical resolution comes back', 150, FRead.ResolutionY, 0.001);
end;


procedure TTestJPEG.TestTheResolutionInCentimeters;

begin
  FImage := CreateSmoothImage(16, 16);
  FImage.ResolutionUnit := ruPixelsPerCentimeter;
  FImage.ResolutionX := 118;
  FImage.ResolutionY := 59;
  FRead := RoundTrip(FImage, FWriter, FReader);
  AssertTrue('The unit comes back as pixels per centimeter', FRead.ResolutionUnit = ruPixelsPerCentimeter);
  AssertEquals('The horizontal resolution comes back', 118, FRead.ResolutionX, 0.001);
  AssertEquals('The vertical resolution comes back', 59, FRead.ResolutionY, 0.001);
end;


procedure TTestJPEG.TestDensityUnits;

begin
  AssertTrue('JFIF unit 0 is no unit', density_unitToResolutionUnit(0) = ruNone);
  AssertTrue('JFIF unit 1 is pixels per inch', density_unitToResolutionUnit(1) = ruPixelsPerInch);
  AssertTrue('JFIF unit 2 is pixels per centimeter', density_unitToResolutionUnit(2) = ruPixelsPerCentimeter);
  AssertTrue('An unknown JFIF unit is no unit', density_unitToResolutionUnit(7) = ruNone);
  AssertEquals('No unit is JFIF unit 0', 0, ResolutionUnitTodensity_unit(ruNone));
  AssertEquals('Pixels per inch is JFIF unit 1', 1, ResolutionUnitTodensity_unit(ruPixelsPerInch));
  AssertEquals('Pixels per centimeter is JFIF unit 2', 2, ResolutionUnitTodensity_unit(ruPixelsPerCentimeter));
end;


procedure TTestJPEG.TestWritingAfterOtherDataKeepsIt;

var
  lSize: Int64;
  I: Integer;
  lCopy: TMemoryStream;

begin
  FImage := CreateSmoothImage(24, 16);
  FStream.WriteBuffer(cPrefix, SizeOf(cPrefix));
  lSize := WriteImage(FImage, FWriter, FStream);
  for I := 0 to High(cPrefix) do
    AssertEquals(Format('Byte %d before the image is kept', [I]), cPrefix[I], PByte(FStream.Memory)[I]);
  AssertEquals('The image follows the prefix', SizeOf(cPrefix) + lSize, FStream.Size);
  AssertEquals('The image starts with the start of image marker', $FF, PByte(FStream.Memory)[SizeOf(cPrefix)]);
  AssertEquals('The image starts with the start of image marker, second byte', $D8, PByte(FStream.Memory)[SizeOf(cPrefix) + 1]);
  lCopy := TMemoryStream.Create;
  try
    lCopy.WriteBuffer((PByte(FStream.Memory) + SizeOf(cPrefix))^, lSize);
    FStream.Clear;
    FStream.CopyFrom(lCopy, 0);
  finally
    lCopy.Free;
  end;
  ReadIt;
  AssertEquals('The image written after the prefix reads back with its width', 24, FRead.Width);
end;


procedure TTestJPEG.TestReadingAfterOtherData;

begin
  FImage := CreateSmoothImage(24, 16);
  FStream.WriteBuffer(cPrefix, SizeOf(cPrefix));
  WriteImage(FImage, FWriter, FStream);
  FStream.Position := SizeOf(cPrefix);
  FRead := TFPMemoryImage.Create(0, 0);
  FRead.LoadFromStream(FStream, FReader);
  AssertEquals('The image after the prefix has its width', 24, FRead.Width);
  AssertEquals('The image after the prefix has its height', 16, FRead.Height);
end;


procedure TTestJPEG.TestTheReaderStopsAtTheEndOfTheImage;

var
  lSize: Int64;

begin
  FImage := CreateSmoothImage(24, 16);
  lSize := WriteImage(FImage, FWriter, FStream);
  FStream.WriteBuffer(cPrefix, SizeOf(cPrefix));
  FStream.Position := 0;
  FRead := TFPMemoryImage.Create(0, 0);
  FRead.LoadFromStream(FStream, FReader);
  AssertEquals('The reader leaves the stream after the end of image marker', lSize, FStream.Position);
end;


procedure TTestJPEG.TestTwoImagesInOneStream;

var
  lSecond: TFPMemoryImage;

begin
  FImage := CreateSmoothImage(24, 16);
  lSecond := CreateSmoothImage(8, 40);
  try
    WriteImage(FImage, FWriter, FStream);
    WriteImage(lSecond, FWriter, FStream);
  finally
    lSecond.Free;
  end;
  FStream.Position := 0;
  FRead := TFPMemoryImage.Create(0, 0);
  FRead.LoadFromStream(FStream, FReader);
  AssertEquals('The first image has its width', 24, FRead.Width);
  FRead.LoadFromStream(FStream, FReader);
  AssertEquals('The second image follows the first, width', 8, FRead.Width);
  AssertEquals('The second image follows the first, height', 40, FRead.Height);
end;


procedure TTestJPEG.TestWritingToAWriteOnlyStream;

var
  lOut: TWriteOnlyStream;

begin
  FImage := CreateSmoothImage(24, 16);
  lOut := TWriteOnlyStream.Create;
  try
    FImage.SaveToStream(lOut, FWriter, False);
    WriteIt;
    AssertEquals('A write-only stream gets all bytes', FStream.Size, lOut.Data.Size);
    AssertTrue('A write-only stream gets the same bytes', CompareMem(FStream.Memory, lOut.Data.Memory, FStream.Size));
  finally
    lOut.Free;
  end;
end;


procedure TTestJPEG.TestImageSizeWithoutReading;

var
  lSize: TPoint;

begin
  FImage := CreateSmoothImage(24, 16);
  FStream.WriteBuffer(cPrefix, SizeOf(cPrefix));
  WriteImage(FImage, FWriter, FStream);
  FStream.Position := SizeOf(cPrefix);
  lSize := TFPReaderJPEG.ImageSize(FStream);
  AssertEquals('ImageSize gives the width', 24, lSize.X);
  AssertEquals('ImageSize gives the height', 16, lSize.Y);
  AssertEquals('ImageSize leaves the position alone', SizeOf(cPrefix), FStream.Position);
end;


procedure TTestJPEG.TestContentsCheck;

begin
  FImage := CreateSmoothImage(8, 8);
  FStream.WriteBuffer(cPrefix, SizeOf(cPrefix));
  WriteImage(FImage, FWriter, FStream);
  FStream.Position := SizeOf(cPrefix);
  AssertTrue('A JPEG is accepted', FReader.CheckContents(FStream));
  AssertEquals('The check leaves the position alone', SizeOf(cPrefix), FStream.Position);
  FStream.Position := 0;
  AssertFalse('Other bytes are rejected', FReader.CheckContents(FStream));
end;


procedure TTestJPEG.TestOtherDataIsRejected;

begin
  FreeAndNil(FStream);
  FStream := BytesStream([Ord('G'), Ord('I'), Ord('F'), Ord('8'), Ord('9'), Ord('a'), 0, 0, 0, 0]);
  AssertRaises('Reading other data raises', FPImageException, @ReadDamaged);
end;


procedure TTestJPEG.TestACutHeaderRaises;

begin
  FImage := CreateSmoothImage(24, 16);
  WriteIt;
  CutStream(40);
  AssertRaises('A JPEG cut in its header raises', FPImageException, @ReadDamaged);
end;


procedure TTestJPEG.TestACutScanIsHandled;

var
  lRaised: Boolean;

begin
  FImage := CreateGradientImage(64, 64);
  WriteIt;
  CutStream(FStream.Size - (FStream.Size div 3));
  lRaised := False;
  try
    ReadIt;
  except
    on E: Exception do
      begin
      AssertTrue('A JPEG cut in its scan raises FPImageException if it raises, got ' + E.ClassName, E is FPImageException);
      lRaised := True;
      end;
  end;
  if not lRaised then
    begin
    AssertEquals('A JPEG cut in its scan that is read has the full width', 64, FRead.Width);
    AssertEquals('A JPEG cut in its scan that is read has the full height', 64, FRead.Height);
    end;
end;


procedure TTestJPEG.TestTheCMYKExample;

begin
  if not FileExists(ExampleFile('cmyk.jpg')) then
    Fail('Example not found: ' + ExampleFile('cmyk.jpg') + ' (run from packages/fcl-image)');
  FReader.Performance := jpBestQuality;
  FRead := TFPMemoryImage.Create(0, 0);
  FRead.LoadFromFile(ExampleFile('cmyk.jpg'), FReader);
  AssertEquals('The CMYK example is 538 pixels wide', 538, FRead.Width);
  AssertEquals('The CMYK example is 417 pixels high', 417, FRead.Height);
  AssertFalse('The CMYK example is not reported as gray', FReader.GrayScale);
  CheckColor('Adobe CMYK at the top left corner', 0, 0, RGB8(216, 6, 58), 6);
  CheckColor('Adobe CMYK at the centre', 269, 208, RGB8(253, 124, 158), 6);
  CheckColor('Adobe CMYK at the bottom right corner', 537, 416, RGB8(124, 14, 104), 6);
  CheckColor('Adobe CMYK at the first quarter', 134, 104, RGB8(113, 36, 152), 6);
  CheckColor('Adobe CMYK at the top right quarter', 403, 104, RGB8(247, 87, 88), 6);
  CheckColor('Adobe CMYK at the bottom left quarter', 134, 312, RGB8(230, 168, 248), 6);
  CheckColor('Adobe CMYK at the bottom right quarter', 403, 312, RGB8(4, 15, 16), 6);
  AssertEquals('The CMYK example is opaque', alphaOpaque, FRead[100, 100].Alpha);
end;


initialization
  RegisterTest('jpeg', TTestJPEG);
end.
