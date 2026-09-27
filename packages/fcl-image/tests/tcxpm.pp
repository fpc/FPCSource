{
    Tests for the XPM reader and writer: round trips at every colour size,
    codes of several characters, transparency and hand-written files.
    See the file COPYING.FPC, included in this distribution, for details.
}
unit tcxpm;

{$mode objfpc}{$H+}

interface

uses sysutils, classes, fpcunit, testregistry, fpimage, fpimgtests,
     fpreadxpm, fpwritexpm;

type
  TTestXPM = class(TTestCase)
  private
    FReader: TFPReaderXPM;
    FWriter: TFPWriterXPM;
    FImage: TFPMemoryImage;
    FRead: TFPMemoryImage;
    FStream: TMemoryStream;
    // Replaces the stream with an XPM file of these header, colour and pixel lines.
    procedure Build(const aLines: array of String);
    procedure ReadIt;
    procedure CheckColor(const aMessage: String; aX, aY: Integer; const aColor: TFPColor);
    procedure ReadTruncated;
    procedure ReadBadHexLength;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRoundTripKeepsSixteenBitColors;
    procedure TestRoundTripWithTwoHexDigits;
    procedure TestRoundTripWithOneHexDigit;
    procedure TestRoundTripOfAPaletteImage;
    procedure TestRoundTripWithTwoCharacterCodes;
    procedure TestRoundTripWithThreeCharacterCodes;
    procedure TestATransparentPixelComesBackTransparent;
    procedure TestTheOutputIsValidC;
    procedure TestReadHexOfEveryLength;
    procedure TestReadColorNames;
    procedure TestReadX11ColorNames;
    procedure TestReadOtherKeysBeforeTheColor;
    procedure TestReadTwoCharactersPerPixel;
    procedure TestReadHotSpotAndExtensions;
    procedure TestReadExtensionsWithoutHotSpot;
    procedure TestContentsCheck;
    procedure TestTruncatedDataRaises;
    procedure TestAHexColorOfTheWrongLengthRaises;
  end;

implementation

procedure TTestXPM.SetUp;

begin
  inherited SetUp;
  FReader := TFPReaderXPM.Create;
  FWriter := TFPWriterXPM.Create;
  FStream := TMemoryStream.Create;
end;


procedure TTestXPM.TearDown;

begin
  FreeAndNil(FStream);
  FreeAndNil(FRead);
  FreeAndNil(FImage);
  FreeAndNil(FWriter);
  FreeAndNil(FReader);
  inherited TearDown;
end;


procedure TTestXPM.Build(const aLines: array of String);

var
  lText: String;
  I: Integer;

begin
  lText := '/* XPM */'#10'static char *test[] = {'#10;
  for I := 0 to High(aLines) do
    begin
    lText := lText + '"' + aLines[I] + '"';
    if I < High(aLines) then
      lText := lText + ',';
    lText := lText + #10;
    end;
  lText := lText + '};'#10;
  FStream.Clear;
  FStream.WriteBuffer(lText[1], Length(lText));
  FStream.Position := 0;
end;


procedure TTestXPM.ReadIt;

begin
  FreeAndNil(FRead);
  FRead := TFPMemoryImage.Create(0, 0);
  FRead.LoadFromStream(FStream, FReader);
end;


procedure TTestXPM.CheckColor(const aMessage: String; aX, aY: Integer; const aColor: TFPColor);

begin
  AssertColorsEqual(aMessage, aColor, FRead[aX, aY]);
end;


procedure TTestXPM.ReadTruncated;

begin
  Build(['2 3 1 1',
         'a c #FF0000',
         'aa']);
  ReadIt;
end;


procedure TTestXPM.ReadBadHexLength;

begin
  Build(['1 1 1 1',
         'a c #FF00',
         'a']);
  ReadIt;
end;


procedure TTestXPM.TestRoundTripKeepsSixteenBitColors;

begin
  FImage := CreateSolidImage(3, 2, FPColor($1234, $5678, $9ABC));
  FImage[1, 1] := FPColor($FFFF, 1, $8000);
  FRead := RoundTrip(FImage, FWriter, FReader);
  AssertImagesEqual('Four hex digits keep all 16 bits', FImage, FRead);
end;


procedure TTestXPM.TestRoundTripWithTwoHexDigits;

begin
  FImage := CreateFewColorsImage(9, 5, 20);
  FWriter.ColorCharSize := 2;
  FRead := RoundTrip(FImage, FWriter, FReader);
  AssertImagesEqual('Two hex digits keep 8-bit colours', FImage, FRead);
end;


procedure TTestXPM.TestRoundTripWithOneHexDigit;

begin
  FImage := CreateFewColorsImage(9, 5, 20);
  FWriter.ColorCharSize := 1;
  FRead := RoundTrip(FImage, FWriter, FReader);
  AssertImagesEqual('One hex digit keeps the top 4 bits', FImage, FRead, $1111);
end;


procedure TTestXPM.TestRoundTripOfAPaletteImage;

begin
  FImage := CreateFewColorsImage(7, 3, 6);
  FImage.UsePalette := True;
  FRead := RoundTrip(FImage, FWriter, FReader);
  AssertTrue('An XPM is read into a palette image', FRead.UsePalette);
  AssertImagesEqual('A palette image keeps its colours', FImage, FRead);
end;


procedure TTestXPM.TestRoundTripWithTwoCharacterCodes;

begin
  FImage := CreateFewColorsImage(20, 10, 200);
  FRead := RoundTrip(FImage, FWriter, FReader);
  AssertImagesEqual('200 colours need codes of two characters', FImage, FRead);
end;


procedure TTestXPM.TestRoundTripWithThreeCharacterCodes;

begin
  FImage := CreateGradientImage(90, 70);
  FRead := RoundTrip(FImage, FWriter, FReader);
  AssertImagesEqual('6300 colours need codes of three characters', FImage, FRead);
end;


procedure TTestXPM.TestATransparentPixelComesBackTransparent;

begin
  FImage := CreateSolidImage(3, 1, colBlue);
  FImage[1, 0] := FPColor($FFFF, 0, 0, alphaTransparent);
  FRead := RoundTrip(FImage, FWriter, FReader);
  AssertEquals('A fully transparent pixel comes back transparent', alphaTransparent, FRead[1, 0].Alpha);
  CheckColor('An opaque pixel stays as it is', 0, 0, colBlue);
end;


procedure TTestXPM.TestTheOutputIsValidC;

var
  lLines: TStringList;

begin
  FImage := CreateSolidImage(2, 2, colRed);
  FImage.SaveToStream(FStream, FWriter);
  FStream.Position := 0;
  lLines := TStringList.Create;
  try
    lLines.LoadFromStream(FStream);
    AssertEquals('The first line is the XPM comment', '/* XPM */', lLines[0]);
    AssertEquals('The array is an array of char pointers', 'static char *graphic[] = {', lLines[1]);
    AssertEquals('The file ends the array', '};', lLines[lLines.Count - 1]);
  finally
    lLines.Free;
  end;
end;


procedure TTestXPM.TestReadHexOfEveryLength;

begin
  Build(['4 1 4 1',
         'a c #F80',
         'b c #FF8800',
         'c c #ABCDEF123',
         'd c #123456789ABC',
         'abcd']);
  ReadIt;
  CheckColor('Three digits repeat each digit', 0, 0, FPColor($FFFF, $8888, 0));
  CheckColor('Six digits repeat each byte', 1, 0, FPColor($FFFF, $8888, 0));
  CheckColor('Nine digits scale 12 bits to 16', 2, 0, FPColor($ABCA, $DEFD, $1231));
  CheckColor('Twelve digits are taken as they are', 3, 0, FPColor($1234, $5678, $9ABC));
end;


procedure TTestXPM.TestReadColorNames;

begin
  Build(['3 1 3 1',
         'a c None',
         'b c white',
         'c c Red',
         'abc']);
  ReadIt;
  AssertEquals('None is transparent', alphaTransparent, FRead[0, 0].Alpha);
  CheckColor('white', 1, 0, colWhite);
  CheckColor('Names ignore case', 2, 0, colRed);
end;


procedure TTestXPM.TestReadX11ColorNames;

begin
  Build(['5 1 5 1',
         'a c gray',
         'b c light gray m white',
         'c c LightSlateGray',
         'd s name c dark olive green',
         'e c lime',
         'abcde']);
  ReadIt;
  CheckColor('gray is the X11 gray', 0, 0, RGB8(190, 190, 190));
  CheckColor('A name of two words ends at the next key', 1, 0, RGB8(211, 211, 211));
  CheckColor('A name in camel case', 2, 0, RGB8(119, 136, 153));
  CheckColor('A name of three words after another key', 3, 0, RGB8(85, 107, 47));
  CheckColor('A web name missing from rgb.txt', 4, 0, colLime);
end;


procedure TTestXPM.TestReadOtherKeysBeforeTheColor;

begin
  Build(['2 1 2 1',
         'a s background m white c #0000FF',
         'b m black c #00FF00 g4 white',
         'ab']);
  ReadIt;
  CheckColor('The c key after s and m keys', 0, 0, colBlue);
  CheckColor('The c key between other keys', 1, 0, colGreen);
end;


procedure TTestXPM.TestReadTwoCharactersPerPixel;

begin
  Build(['3 2 2 2',
         'aa c #FF0000',
         'aA c #0000FF',
         'aaaAaa',
         'aAaAaA']);
  ReadIt;
  CheckColor('Codes are case sensitive: aa is red', 0, 0, colRed);
  CheckColor('aA is blue', 1, 0, colBlue);
  CheckColor('Second row', 2, 1, colBlue);
end;


procedure TTestXPM.TestReadHotSpotAndExtensions;

begin
  Build(['2 1 1 1 0 0 XPMEXT',
         'a c #FF0000',
         'aa']);
  ReadIt;
  CheckColor('A header with a hot spot and XPMEXT', 1, 0, colRed);
end;


procedure TTestXPM.TestReadExtensionsWithoutHotSpot;

begin
  Build(['2 1 1 1 XPMEXT',
         'a c #FF0000',
         'aa']);
  ReadIt;
  CheckColor('A header with XPMEXT and no hot spot', 1, 0, colRed);
end;


procedure TTestXPM.TestContentsCheck;

begin
  Build(['1 1 1 1', 'a c #000000', 'a']);
  AssertTrue('A file starting with the XPM comment is accepted', FReader.CheckContents(FStream));
  FStream.Clear;
  FStream.WriteBuffer(PChar('/* C file */')^, 12);
  FStream.Position := 0;
  AssertFalse('Another comment is rejected', FReader.CheckContents(FStream));
end;


procedure TTestXPM.TestTruncatedDataRaises;

begin
  AssertRaises('Fewer pixel lines than the height raises', FPImageException, @ReadTruncated);
end;


procedure TTestXPM.TestAHexColorOfTheWrongLengthRaises;

begin
  AssertRaises('A hex colour whose length is not a multiple of 3 raises', FPImageException, @ReadBadHexLength);
end;


initialization
  RegisterTest('xpm', TTestXPM);
end.
