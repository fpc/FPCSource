{
    Tests for the Netpbm reader and writers: binary and text variants of
    PBM, PGM and PPM, 16-bit samples, the depth guessed and stream handling.
    See the file COPYING.FPC, included in this distribution, for details.
}
unit tcpnm;

{$mode objfpc}{$H+}

interface

uses sysutils, classes, fpcunit, testregistry, fpimage, fpimgtests,
     fpreadpnm, fpwritepnm;

type
  TTestPNM = class(TTestCase)
  private
    FReader: TFPReaderPNM;
    FWriter: TFPWriterPNM;
    FImage: TFPMemoryImage;
    FRead: TFPMemoryImage;
    FStream: TMemoryStream;
    // Replaces the stream with the text, followed by the bytes.
    procedure Build(const aText: String; const aBytes: array of Byte);
    procedure ReadIt;
    procedure UseWriter(aWriter: TFPWriterPNM);
    // The first two characters of what was written.
    function Magic: String;
    procedure WriteFullWidthText;
    procedure ReadP8;
    procedure ReadBadP1;
    procedure CheckColor(const aMessage: String; aX, aY: Integer; const aColor: TFPColor);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestBinaryPPMKeepsTheColors;
    procedure TestBinaryPGMKeepsGray;
    procedure TestPGMOfAColorIsItsLuma;
    procedure TestBinaryPBMKeepsBlackAndWhite;
    procedure TestPBMMakesLightColorsWhite;
    procedure TestSixteenBitPPM;
    procedure TestSixteenBitPGM;
    procedure TestFullWidthNeedsBinary;
    procedure TestTextPPMKeepsTheColors;
    procedure TestTextPGMKeepsGray;
    procedure TestTextPBMKeepsBlackAndWhite;
    procedure TestTheDepthGuessedForBlackAndWhite;
    procedure TestTheDepthGuessedForGray;
    procedure TestTheDepthGuessedForColors;
    procedure TestTheDepthGuessedForPrimaryColors;
    procedure TestWritingAfterOtherDataKeepsIt;
    procedure TestWritingToAWriteOnlyStream;
    procedure TestTwoImagesInOneStream;
    procedure TestReadP1;
    procedure TestReadP1WithoutSpaces;
    procedure TestReadP2WithComments;
    procedure TestReadP2WithASmallMaximum;
    procedure TestReadP2WithSixteenBits;
    procedure TestReadP3;
    procedure TestReadP4Padding;
    procedure TestReadP5;
    procedure TestReadP5WithAMaximumOf1000;
    procedure TestReadP6WithSixteenBits;
    procedure TestUnsupportedSubtypeIsRejected;
    procedure TestTextLinesAreShort;
    procedure TestReadP2ClampsSamplesAboveTheMaximum;
    procedure TestReadP2WithoutAFinalNewline;
    procedure TestReadP1WithABadDigit;
  end;

implementation

procedure TTestPNM.SetUp;

begin
  inherited SetUp;
  FReader := TFPReaderPNM.Create;
  FWriter := TFPWriterPNM.Create;
  FStream := TMemoryStream.Create;
end;


procedure TTestPNM.TearDown;

begin
  FreeAndNil(FStream);
  FreeAndNil(FRead);
  FreeAndNil(FImage);
  FreeAndNil(FWriter);
  FreeAndNil(FReader);
  inherited TearDown;
end;


procedure TTestPNM.Build(const aText: String; const aBytes: array of Byte);

begin
  FStream.Clear;
  if aText <> '' then
    FStream.WriteBuffer(aText[1], Length(aText));
  if Length(aBytes) > 0 then
    FStream.WriteBuffer(aBytes[0], Length(aBytes));
  FStream.Position := 0;
end;


procedure TTestPNM.ReadIt;

begin
  FreeAndNil(FRead);
  FRead := TFPMemoryImage.Create(0, 0);
  FRead.LoadFromStream(FStream, FReader);
end;


procedure TTestPNM.UseWriter(aWriter: TFPWriterPNM);

begin
  FreeAndNil(FWriter);
  FWriter := aWriter;
end;


function TTestPNM.Magic: String;

begin
  SetString(Result, PChar(FStream.Memory), 2);
end;


procedure TTestPNM.WriteFullWidthText;

begin
  FWriter.FullWidth := True;
  FWriter.BinaryFormat := False;
  FImage.SaveToStream(FStream, FWriter);
end;


procedure TTestPNM.ReadP8;

begin
  Build('P8'#10'1 1'#10'255'#10, [0]);
  ReadIt;
end;


procedure TTestPNM.ReadBadP1;

begin
  Build('P1'#10'2 1'#10'1 2'#10, []);
  ReadIt;
end;


procedure TTestPNM.CheckColor(const aMessage: String; aX, aY: Integer; const aColor: TFPColor);

begin
  AssertColorsEqual(aMessage, aColor, FRead[aX, aY]);
end;


procedure TTestPNM.TestBinaryPPMKeepsTheColors;

begin
  UseWriter(TFPWriterPPM.Create);
  FImage := CreateGradientImage(13, 7);
  FRead := RoundTrip(FImage, FWriter, FReader);
  FImage.SaveToStream(FStream, FWriter);
  AssertEquals('A PPM is written as P6', 'P6', Magic);
  AssertImagesEqual('A binary PPM keeps every colour', FImage, FRead);
end;


procedure TTestPNM.TestBinaryPGMKeepsGray;

begin
  UseWriter(TFPWriterPGM.Create);
  FImage := CreateGrayImage(9, 5);
  FRead := RoundTrip(FImage, FWriter, FReader);
  AssertImagesEqual('A binary PGM keeps gray pixels', FImage, FRead, 257);
end;


procedure TTestPNM.TestPGMOfAColorIsItsLuma;

var
  lGray: Word;

begin
  UseWriter(TFPWriterPGM.Create);
  FImage := CreateSolidImage(2, 2, colGreen);
  FRead := RoundTrip(FImage, FWriter, FReader);
  lGray := CalculateGray(colGreen);
  AssertColorsEqual('A colour becomes its luma', FPColor(lGray, lGray, lGray), FRead[1, 1], 257);
end;


procedure TTestPNM.TestBinaryPBMKeepsBlackAndWhite;

begin
  UseWriter(TFPWriterPBM.Create);
  FImage := CreateMonoImage(13, 5);
  FRead := RoundTrip(FImage, FWriter, FReader);
  FImage.SaveToStream(FStream, FWriter);
  AssertEquals('A PBM is written as P4', 'P4', Magic);
  AssertImagesEqual('A binary PBM keeps black and white', FImage, FRead);
end;


procedure TTestPNM.TestPBMMakesLightColorsWhite;

begin
  UseWriter(TFPWriterPBM.Create);
  FImage := CreateSolidImage(3, 1, colYellow);
  FImage[1, 0] := colNavy;
  FRead := RoundTrip(FImage, FWriter, FReader);
  CheckColor('Yellow, a light colour, becomes white', 0, 0, colWhite);
  CheckColor('Navy, a dark colour, becomes black', 1, 0, colBlack);
end;


procedure TTestPNM.TestSixteenBitPPM;

var
  lX, lY: Integer;

begin
  UseWriter(TFPWriterPPM.Create);
  FWriter.FullWidth := True;
  FImage := TFPMemoryImage.Create(5, 3);
  for lY := 0 to 2 do
    for lX := 0 to 4 do
      FImage[lX, lY] := FPColor(lX * 13001 + 7, lY * 21011 + 3, (lX + lY) * 5003);
  FRead := RoundTrip(FImage, FWriter, FReader);
  AssertImagesEqual('A 16-bit PPM keeps all 16 bits', FImage, FRead);
end;


procedure TTestPNM.TestSixteenBitPGM;

var
  lX: Integer;

begin
  UseWriter(TFPWriterPGM.Create);
  FWriter.FullWidth := True;
  FImage := TFPMemoryImage.Create(6, 1);
  for lX := 0 to 5 do
    FImage[lX, 0] := FPColor(lX * 12345, lX * 12345, lX * 12345);
  FRead := RoundTrip(FImage, FWriter, FReader);
  AssertImagesEqual('A 16-bit PGM keeps 16-bit gray', FImage, FRead, 1);
end;


procedure TTestPNM.TestFullWidthNeedsBinary;

begin
  FImage := CreateGradientImage(2, 2);
  AssertRaises('16 bits in the text format is rejected', FPImageException, @WriteFullWidthText);
end;


procedure TTestPNM.TestTextPPMKeepsTheColors;

begin
  UseWriter(TFPWriterPPM.Create);
  FWriter.BinaryFormat := False;
  FImage := CreateGradientImage(13, 7);
  FRead := RoundTrip(FImage, FWriter, FReader);
  FImage.SaveToStream(FStream, FWriter);
  AssertEquals('A text PPM is written as P3', 'P3', Magic);
  AssertImagesEqual('A text PPM keeps every colour', FImage, FRead);
end;


procedure TTestPNM.TestTextPGMKeepsGray;

begin
  UseWriter(TFPWriterPGM.Create);
  FWriter.BinaryFormat := False;
  FImage := CreateGrayImage(9, 5);
  FRead := RoundTrip(FImage, FWriter, FReader);
  AssertImagesEqual('A text PGM keeps gray pixels', FImage, FRead, 257);
end;


procedure TTestPNM.TestTextPBMKeepsBlackAndWhite;

begin
  UseWriter(TFPWriterPBM.Create);
  FWriter.BinaryFormat := False;
  FImage := CreateMonoImage(13, 5);
  FRead := RoundTrip(FImage, FWriter, FReader);
  AssertImagesEqual('A text PBM keeps black and white', FImage, FRead);
end;


procedure TTestPNM.TestTheDepthGuessedForBlackAndWhite;

begin
  FImage := CreateMonoImage(5, 5);
  AssertTrue('A black and white image is guessed as PBM', FWriter.GuessColorDepthOfImage(FImage) = pcdBlackWhite);
  FImage.SaveToStream(FStream, FWriter);
  AssertEquals('and written as P4', 'P4', Magic);
end;


procedure TTestPNM.TestTheDepthGuessedForGray;

begin
  FImage := CreateGrayImage(5, 5);
  AssertTrue('A gray image is guessed as PGM', FWriter.GuessColorDepthOfImage(FImage) = pcdGrayscale);
end;


procedure TTestPNM.TestTheDepthGuessedForColors;

begin
  FImage := CreateSolidImage(3, 3, RGB8(100, 150, 200));
  AssertTrue('A colour image is guessed as PPM', FWriter.GuessColorDepthOfImage(FImage) = pcdRGB);
end;


procedure TTestPNM.TestTheDepthGuessedForPrimaryColors;

begin
  FImage := CreateSolidImage(3, 3, colWhite);
  FImage[1, 1] := colRed;
  AssertTrue('An image with pure red is guessed as PPM', FWriter.GuessColorDepthOfImage(FImage) = pcdRGB);
  FImage[1, 1] := RGB8(128, 128, 128);
  AssertTrue('An image with white and mid gray is guessed as PGM', FWriter.GuessColorDepthOfImage(FImage) = pcdGrayscale);
end;


procedure TTestPNM.TestWritingAfterOtherDataKeepsIt;

begin
  UseWriter(TFPWriterPPM.Create);
  FImage := CreateGradientImage(4, 3);
  FStream.WriteBuffer(PChar('prefix')^, 6);
  WriteImage(FImage, FWriter, FStream);
  AssertTrue('The bytes before the image are kept', CompareMem(FStream.Memory, PChar('prefix'), 6));
  FStream.Position := 6;
  ReadIt;
  AssertImagesEqual('The image after the prefix reads back', FImage, FRead);
end;


procedure TTestPNM.TestWritingToAWriteOnlyStream;

var
  lOut: TWriteOnlyStream;

begin
  UseWriter(TFPWriterPPM.Create);
  FImage := CreateGradientImage(4, 3);
  lOut := TWriteOnlyStream.Create;
  try
    FImage.SaveToStream(lOut, FWriter, False);
    FImage.SaveToStream(FStream, FWriter);
    AssertEquals('A write-only stream gets all bytes', FStream.Size, lOut.Data.Size);
  finally
    lOut.Free;
  end;
end;


procedure TTestPNM.TestTwoImagesInOneStream;

var
  lSecond: TFPMemoryImage;

begin
  Build('P6'#10'2 1'#10'255'#10, [255, 0, 0, 0, 0, 255]);
  FStream.Position := FStream.Size;
  FStream.WriteBuffer(PChar('P6'#10'1 1'#10'255'#10)^, 11);
  FStream.WriteBuffer(PChar(#0#255#0)^, 3);
  FStream.Position := 0;
  ReadIt;
  CheckColor('The first image', 1, 0, colBlue);
  AssertEquals('The reader stops at the end of the first image', 17, FStream.Position);
  lSecond := TFPMemoryImage.Create(0, 0);
  try
    lSecond.LoadFromStream(FStream, FReader);
    AssertColorsEqual('The second image follows', colGreen, lSecond[0, 0]);
  finally
    lSecond.Free;
  end;
end;


procedure TTestPNM.TestReadP1;

begin
  Build('P1'#10'3 2'#10'1 0 1'#10'0 1 0'#10, []);
  ReadIt;
  CheckColor('1 is black', 0, 0, colBlack);
  CheckColor('0 is white', 1, 0, colWhite);
  CheckColor('Second row', 1, 1, colBlack);
end;


procedure TTestPNM.TestReadP1WithoutSpaces;

begin
  Build('P1'#10'3 2'#10'101'#10'010'#10, []);
  ReadIt;
  CheckColor('1 is black', 0, 0, colBlack);
  CheckColor('0 is white', 1, 0, colWhite);
  CheckColor('Second row', 1, 1, colBlack);
end;


procedure TTestPNM.TestReadP2WithComments;

begin
  Build('P2'#10'# a comment'#10'2 1 # width and height'#10'255'#10'0 255'#10, []);
  ReadIt;
  AssertEquals('Width after a comment', 2, FRead.Width);
  CheckColor('0 is black', 0, 0, colBlack);
  CheckColor('255 is white', 1, 0, colWhite);
end;


procedure TTestPNM.TestReadP2WithASmallMaximum;

begin
  Build('P2'#10'3 1'#10'15'#10'0 8 15'#10, []);
  ReadIt;
  CheckColor('0 of 15 is black', 0, 0, colBlack);
  CheckColor('8 of 15 is scaled', 1, 0, FPColor(34952, 34952, 34952));
  CheckColor('15 of 15 is white', 2, 0, colWhite);
end;


procedure TTestPNM.TestReadP2WithSixteenBits;

begin
  Build('P2'#10'2 1'#10'65535'#10'4660 65535'#10, []);
  ReadIt;
  CheckColor('A text value of 16 bits is taken as it is', 0, 0, FPColor($1234, $1234, $1234));
  CheckColor('65535 is white', 1, 0, colWhite);
end;


procedure TTestPNM.TestReadP3;

begin
  Build('P3'#10'2 1'#10'255'#10'255 0 0  10 20 30'#10, []);
  ReadIt;
  CheckColor('Red', 0, 0, colRed);
  CheckColor('A mixed colour', 1, 0, RGB8(10, 20, 30));
end;


procedure TTestPNM.TestReadP4Padding;

begin
  { 10x2: every row padded to 2 bytes }
  Build('P4'#10'10 2'#10, [$80, $40, $00, $C0]);
  ReadIt;
  CheckColor('Row 0, pixel 0 is black', 0, 0, colBlack);
  CheckColor('Row 0, pixel 9 is black', 9, 0, colBlack);
  CheckColor('Row 0, pixel 8 is white', 8, 0, colWhite);
  CheckColor('Row 1, pixel 8 is black', 8, 1, colBlack);
  CheckColor('Row 1, pixel 0 is white', 0, 1, colWhite);
end;


procedure TTestPNM.TestReadP5;

begin
  Build('P5'#10'3 1'#10'255'#10, [0, 128, 255]);
  ReadIt;
  CheckColor('0', 0, 0, colBlack);
  CheckColor('128', 1, 0, RGB8(128, 128, 128));
  CheckColor('255', 2, 0, colWhite);
end;


procedure TTestPNM.TestReadP5WithAMaximumOf1000;

begin
  { two-byte samples, most significant byte first: 500 and 1000 }
  Build('P5'#10'2 1'#10'1000'#10, [$01, $F4, $03, $E8]);
  ReadIt;
  CheckColor('500 of 1000 is mid gray', 0, 0, FPColor(32767, 32767, 32767));
  CheckColor('1000 of 1000 is white', 1, 0, colWhite);
end;


procedure TTestPNM.TestReadP6WithSixteenBits;

begin
  Build('P6'#10'1 1'#10'65535'#10, [$12, $34, $56, $78, $9A, $BC]);
  ReadIt;
  CheckColor('Samples are most significant byte first', 0, 0, FPColor($1234, $5678, $9ABC));
end;


procedure TTestPNM.TestUnsupportedSubtypeIsRejected;

begin
  Build('P8'#10'1 1'#10, []);
  AssertFalse('P8 is not accepted by the contents check', FReader.CheckContents(FStream));
  AssertRaises('Reading P8 raises', Exception, @ReadP8);
end;


procedure TTestPNM.TestTextLinesAreShort;

var
  lText: String;
  lLines: TStringList;
  lLine: String;

begin
  UseWriter(TFPWriterPPM.Create);
  FWriter.BinaryFormat := False;
  FImage := CreateGradientImage(40, 2);
  FImage.SaveToStream(FStream, FWriter);
  SetString(lText, PChar(FStream.Memory), FStream.Size);
  lLines := TStringList.Create;
  try
    lLines.Text := lText;
    AssertTrue('A long row is split over several lines', lLines.Count > 5);
    for lLine in lLines do
      AssertTrue('No text line is longer than 70 characters: ' + lLine, Length(lLine) <= 70);
  finally
    lLines.Free;
  end;
end;


procedure TTestPNM.TestReadP2ClampsSamplesAboveTheMaximum;

begin
  Build('P2'#10'2 1'#10'15'#10'0 99'#10, []);
  ReadIt;
  CheckColor('A sample above the maximum is white', 1, 0, colWhite);
end;


procedure TTestPNM.TestReadP2WithoutAFinalNewline;

begin
  Build('P2'#10'2 1'#10'255'#10'0 255', []);
  ReadIt;
  CheckColor('The last sample ends at the end of the stream', 1, 0, colWhite);
end;


procedure TTestPNM.TestReadP1WithABadDigit;

begin
  AssertRaises('A digit other than 0 or 1 in a P1 image raises', FPImageException, @ReadBadP1);
end;


initialization
  RegisterTest('pnm', TTestPNM);
end.
