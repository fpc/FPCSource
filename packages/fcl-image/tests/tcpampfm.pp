{
    Tests for the PAM (P7) and PFM (PF, Pf) variants of the Netpbm reader
    and their writers.
    See the file COPYING.FPC, included in this distribution, for details.
}
unit tcpampfm;

{$mode objfpc}{$H+}

interface

uses sysutils, classes, fpcunit, testregistry, fpimage, fpimgtests,
     fpreadpnm, fpwritepnm;

type
  TTestPAMPFM = class(TTestCase)
  private
    FReader: TFPReaderPNM;
    FImage: TFPMemoryImage;
    FRead: TFPMemoryImage;
    FStream: TMemoryStream;
    // Replaces the stream with the text, followed by the bytes.
    procedure Build(const aText: String; const aBytes: array of Byte);
    procedure ReadIt;
    // Writes FImage with aWriter, frees the writer and returns the header text up to aLines lines.
    function WriteWith(aWriter: TFPCustomImageWriter; aLines: Integer): String;
    procedure ReadBadDepth;
    procedure ReadUnknownField;
    procedure ReadBadScale;
    procedure CheckColor(const aMessage: String; aX, aY: Integer; const aColor: TFPColor);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestPAMIsAcceptedByTheContentsCheck;
    procedure TestPFMIsAcceptedByTheContentsCheck;
    procedure TestReadPAMGrayscale;
    procedure TestReadPAMRGBAlpha;
    procedure TestReadPAMSixteenBits;
    procedure TestReadPAMBlackAndWhite;
    procedure TestReadPAMWithComments;
    procedure TestReadPAMWithABadDepth;
    procedure TestReadPAMWithAnUnknownField;
    procedure TestReadPFMColorIsBottomUp;
    procedure TestReadPFMBigEndian;
    procedure TestReadPFMClamps;
    procedure TestReadPFMWithABadScale;
    procedure TestPAMKeepsTheColors;
    procedure TestPAMKeepsTheAlpha;
    procedure TestPAMWithoutAlphaWhenOpaque;
    procedure TestPAMGuessesGray;
    procedure TestPAMGuessesBlackAndWhite;
    procedure TestPAMSixteenBits;
    procedure TestPFMKeepsTheColors;
    procedure TestPFMGuessesGray;
    procedure TestPAMAndPFMAreRegistered;
  end;

implementation

procedure TTestPAMPFM.SetUp;

begin
  inherited SetUp;
  FReader := TFPReaderPNM.Create;
  FStream := TMemoryStream.Create;
end;


procedure TTestPAMPFM.TearDown;

begin
  FreeAndNil(FStream);
  FreeAndNil(FRead);
  FreeAndNil(FImage);
  FreeAndNil(FReader);
  inherited TearDown;
end;


procedure TTestPAMPFM.Build(const aText: String; const aBytes: array of Byte);

begin
  FStream.Clear;
  if aText <> '' then
    FStream.WriteBuffer(aText[1], Length(aText));
  if Length(aBytes) > 0 then
    FStream.WriteBuffer(aBytes[0], Length(aBytes));
  FStream.Position := 0;
end;


procedure TTestPAMPFM.ReadIt;

begin
  FreeAndNil(FRead);
  FRead := TFPMemoryImage.Create(0, 0);
  FRead.LoadFromStream(FStream, FReader);
end;


function TTestPAMPFM.WriteWith(aWriter: TFPCustomImageWriter; aLines: Integer): String;

var
  lText: String;
  lPos: Integer;

begin
  try
    FStream.Clear;
    FImage.SaveToStream(FStream, aWriter);
  finally
    aWriter.Free;
  end;
  SetString(lText, PChar(FStream.Memory), FStream.Size);
  lPos := 0;
  while (aLines > 0) and (lPos < Length(lText)) do
    begin
    Inc(lPos);
    if lText[lPos] = #10 then
      Dec(aLines);
    end;
  Result := Copy(lText, 1, lPos);
  FStream.Position := 0;
  ReadIt;
end;


procedure TTestPAMPFM.ReadBadDepth;

begin
  Build('P7'#10'WIDTH 1'#10'HEIGHT 1'#10'DEPTH 5'#10'MAXVAL 255'#10'ENDHDR'#10, [0, 0, 0, 0, 0]);
  ReadIt;
end;


procedure TTestPAMPFM.ReadUnknownField;

begin
  Build('P7'#10'WIDTH 1'#10'HEIGHT 1'#10'COLOUR 1'#10'DEPTH 1'#10'MAXVAL 255'#10'ENDHDR'#10, [0]);
  ReadIt;
end;


procedure TTestPAMPFM.ReadBadScale;

begin
  Build('Pf'#10'1 1'#10'x'#10, [0, 0, 0, 0]);
  ReadIt;
end;


procedure TTestPAMPFM.CheckColor(const aMessage: String; aX, aY: Integer; const aColor: TFPColor);

begin
  AssertColorsEqual(aMessage, aColor, FRead[aX, aY]);
end;


procedure TTestPAMPFM.TestPAMIsAcceptedByTheContentsCheck;

begin
  Build('P7'#10'WIDTH 1'#10, []);
  AssertTrue('P7 is accepted by the contents check', FReader.CheckContents(FStream));
end;


procedure TTestPAMPFM.TestPFMIsAcceptedByTheContentsCheck;

begin
  Build('PF'#10'1 1'#10, []);
  AssertTrue('PF is accepted by the contents check', FReader.CheckContents(FStream));
  Build('Pf'#10'1 1'#10, []);
  AssertTrue('Pf is accepted by the contents check', FReader.CheckContents(FStream));
end;


procedure TTestPAMPFM.TestReadPAMGrayscale;

begin
  Build('P7'#10'WIDTH 3'#10'HEIGHT 1'#10'DEPTH 1'#10'MAXVAL 255'#10'TUPLTYPE GRAYSCALE'#10'ENDHDR'#10, [0, 128, 255]);
  ReadIt;
  AssertEquals('The width comes from WIDTH', 3, FRead.Width);
  CheckColor('0 is black', 0, 0, colBlack);
  CheckColor('128 is mid gray', 1, 0, RGB8(128, 128, 128));
  CheckColor('255 is white', 2, 0, colWhite);
end;


procedure TTestPAMPFM.TestReadPAMRGBAlpha;

begin
  Build('P7'#10'WIDTH 2'#10'HEIGHT 1'#10'DEPTH 4'#10'MAXVAL 255'#10'TUPLTYPE RGB_ALPHA'#10'ENDHDR'#10,
        [255, 0, 0, 255, 10, 20, 30, 128]);
  ReadIt;
  CheckColor('An opaque red', 0, 0, colRed);
  CheckColor('The fourth sample is the alpha', 1, 0, RGB8(10, 20, 30, 128));
end;


procedure TTestPAMPFM.TestReadPAMSixteenBits;

begin
  Build('P7'#10'WIDTH 1'#10'HEIGHT 1'#10'DEPTH 2'#10'MAXVAL 65535'#10'TUPLTYPE GRAYSCALE_ALPHA'#10'ENDHDR'#10,
        [$12, $34, $80, $00]);
  ReadIt;
  CheckColor('Samples of 16 bits are most significant byte first', 0, 0, FPColor($1234, $1234, $1234, $8000));
end;


procedure TTestPAMPFM.TestReadPAMBlackAndWhite;

begin
  Build('P7'#10'WIDTH 2'#10'HEIGHT 1'#10'DEPTH 1'#10'MAXVAL 1'#10'TUPLTYPE BLACKANDWHITE'#10'ENDHDR'#10, [0, 1]);
  ReadIt;
  CheckColor('0 is black', 0, 0, colBlack);
  CheckColor('1 is white', 1, 0, colWhite);
end;


procedure TTestPAMPFM.TestReadPAMWithComments;

begin
  Build('P7'#10'# made by hand'#10'WIDTH 1 # one'#10'HEIGHT 1'#10'DEPTH 3'#10'MAXVAL 255'#10'ENDHDR'#10, [1, 2, 3]);
  ReadIt;
  CheckColor('Comments in the header are skipped', 0, 0, RGB8(1, 2, 3));
end;


procedure TTestPAMPFM.TestReadPAMWithABadDepth;

begin
  AssertRaises('A depth of 5 raises', FPImageException, @ReadBadDepth);
end;


procedure TTestPAMPFM.TestReadPAMWithAnUnknownField;

begin
  AssertRaises('An unknown header field raises', FPImageException, @ReadUnknownField);
end;


procedure TTestPAMPFM.TestReadPFMColorIsBottomUp;

var
  lValues: array[0..5] of Single = (1, 0, 0, 0, 0.5, 1);

begin
  Build('PF'#10'1 2'#10'-1.0'#10, []);
  FStream.Position := FStream.Size;
  FStream.WriteBuffer(lValues, SizeOf(lValues));
  FStream.Position := 0;
  ReadIt;
  CheckColor('The first row of the file is the bottom one', 0, 1, colRed);
  CheckColor('The second row of the file is the top one', 0, 0, FPColor(0, 32768, 65535));
end;


procedure TTestPAMPFM.TestReadPFMBigEndian;

begin
  { 0.5 as a big-endian single }
  Build('Pf'#10'1 1'#10'1.0'#10, [$3F, $00, $00, $00]);
  ReadIt;
  CheckColor('A positive scale means big-endian samples', 0, 0, FPColor(32768, 32768, 32768));
end;


procedure TTestPAMPFM.TestReadPFMClamps;

var
  { -2, 7.5, a NaN and positive infinity }
  lValues: array[0..3] of Cardinal = ($C0000000, $40F00000, $7FC00000, $7F800000);

begin
  Build('Pf'#10'4 1'#10'-1'#10, []);
  FStream.Position := FStream.Size;
  FStream.WriteBuffer(lValues, SizeOf(lValues));
  FStream.Position := 0;
  ReadIt;
  CheckColor('A negative value is black', 0, 0, colBlack);
  CheckColor('A value above 1 is white', 1, 0, colWhite);
  CheckColor('A NaN is black', 2, 0, colBlack);
  CheckColor('Infinity is white', 3, 0, colWhite);
end;


procedure TTestPAMPFM.TestReadPFMWithABadScale;

begin
  AssertRaises('A scale that is not a number raises', FPImageException, @ReadBadScale);
end;


procedure TTestPAMPFM.TestPAMKeepsTheColors;

var
  lHeader: String;

begin
  FImage := CreateGradientImage(13, 7);
  lHeader := WriteWith(TFPWriterPAM.Create, 7);
  AssertEquals('The header of an RGB image',
    'P7'#10'WIDTH 13'#10'HEIGHT 7'#10'DEPTH 3'#10'MAXVAL 255'#10'TUPLTYPE RGB'#10'ENDHDR'#10, lHeader);
  AssertImagesEqual('A PAM keeps every colour', FImage, FRead);
end;


procedure TTestPAMPFM.TestPAMKeepsTheAlpha;

var
  lHeader: String;

begin
  FImage := CreateAlphaImage(9, 5);
  lHeader := WriteWith(TFPWriterPAM.Create, 5);
  AssertEquals('An image that is not opaque has RGB_ALPHA tuples',
    'P7'#10'WIDTH 9'#10'HEIGHT 5'#10'DEPTH 4'#10'MAXVAL 255'#10, lHeader);
  AssertImagesEqual('A PAM keeps the alpha', FImage, FRead);
end;


procedure TTestPAMPFM.TestPAMWithoutAlphaWhenOpaque;

var
  lWriter: TFPWriterPAM;
  lHeader: String;

begin
  FImage := CreateAlphaImage(9, 5);
  lWriter := TFPWriterPAM.Create;
  lWriter.UseAlpha := False;
  lHeader := WriteWith(lWriter, 4);
  AssertEquals('UseAlpha off writes no alpha channel', 'P7'#10'WIDTH 9'#10'HEIGHT 5'#10'DEPTH 3'#10, lHeader);
  AssertEquals('and the pixels read are opaque', AlphaOpaque, FRead[0, 0].Alpha);
end;


procedure TTestPAMPFM.TestPAMGuessesGray;

var
  lHeader: String;

begin
  FImage := CreateGrayImage(9, 5);
  lHeader := WriteWith(TFPWriterPAM.Create, 6);
  AssertEquals('A gray image has GRAYSCALE tuples',
    'P7'#10'WIDTH 9'#10'HEIGHT 5'#10'DEPTH 1'#10'MAXVAL 255'#10'TUPLTYPE GRAYSCALE'#10, lHeader);
  AssertImagesEqual('A gray PAM keeps the gray', FImage, FRead);
end;


procedure TTestPAMPFM.TestPAMGuessesBlackAndWhite;

var
  lHeader: String;

begin
  FImage := CreateMonoImage(9, 5);
  lHeader := WriteWith(TFPWriterPAM.Create, 6);
  AssertEquals('A black and white image has BLACKANDWHITE tuples',
    'P7'#10'WIDTH 9'#10'HEIGHT 5'#10'DEPTH 1'#10'MAXVAL 1'#10'TUPLTYPE BLACKANDWHITE'#10, lHeader);
  AssertImagesEqual('A black and white PAM keeps the pixels', FImage, FRead);
end;


procedure TTestPAMPFM.TestPAMSixteenBits;

var
  lWriter: TFPWriterPAM;
  lHeader: String;

begin
  FImage := CreateSolidImage(2, 2, FPColor($1234, $5678, $9ABC, $DEF0));
  lWriter := TFPWriterPAM.Create;
  lWriter.FullWidth := True;
  lHeader := WriteWith(lWriter, 5);
  AssertEquals('FullWidth writes a maximum of 65535',
    'P7'#10'WIDTH 2'#10'HEIGHT 2'#10'DEPTH 4'#10'MAXVAL 65535'#10, lHeader);
  CheckColor('Samples of 16 bits are kept', 1, 1, FPColor($1234, $5678, $9ABC, $DEF0));
end;


procedure TTestPAMPFM.TestPFMKeepsTheColors;

var
  lHeader: String;

begin
  FImage := CreateSolidImage(3, 2, FPColor($1234, $5678, $9ABC));
  FImage.Colors[1, 0] := colRed;
  lHeader := WriteWith(TFPWriterPFM.Create, 3);
  AssertEquals('A colour image is written as little-endian PF', 'PF'#10'3 2'#10'-1.0'#10, lHeader);
  AssertEquals('Three floats per pixel', Length(lHeader) + 3 * 2 * 12, FStream.Size);
  AssertImagesEqual('A PFM keeps 16-bit colours', FImage, FRead);
end;


procedure TTestPAMPFM.TestPFMGuessesGray;

var
  lHeader: String;

begin
  FImage := CreateGrayImage(4, 3);
  lHeader := WriteWith(TFPWriterPFM.Create, 3);
  AssertEquals('A gray image is written as Pf', 'Pf'#10'4 3'#10'-1.0'#10, lHeader);
  AssertEquals('One float per pixel', Length(lHeader) + 4 * 3 * 4, FStream.Size);
  AssertImagesEqual('A gray PFM keeps the gray', FImage, FRead);
end;


procedure TTestPAMPFM.TestPAMAndPFMAreRegistered;

begin
  AssertTrue('PAM has a reader', ImageHandlers.ImageReader['Netpbm Portable Arbitrary Map'] = TFPReaderPNM);
  AssertTrue('PAM has a writer', ImageHandlers.ImageWriter['Netpbm Portable Arbitrary Map'] = TFPWriterPAM);
  AssertTrue('PFM has a reader', ImageHandlers.ImageReader['Portable Float Map'] = TFPReaderPNM);
  AssertTrue('PFM has a writer', ImageHandlers.ImageWriter['Portable Float Map'] = TFPWriterPFM);
end;


initialization
  RegisterTest('pampfm', TTestPAMPFM);
end.
