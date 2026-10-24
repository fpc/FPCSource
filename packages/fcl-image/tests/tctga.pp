{
    Tests for the TARGA reader and writer: round trips, the header written,
    and hand-built files of every image type, origin and depth.
    See the file COPYING.FPC, included in this distribution, for details.
}
unit tctga;

{$mode objfpc}{$H+}

interface

uses sysutils, classes, fpcunit, testregistry, fpimage, fpimgtests,
     targacmn, fpreadtga, fpwritetga;

type
  TTestTGA = class(TTestCase)
  private
    FReader: TFPReaderTarga;
    FWriter: TFPWriterTarga;
    FImage: TFPMemoryImage;
    FRead: TFPMemoryImage;
    FStream: TMemoryStream;
    // Builds a TGA file with a colour map of aMapLength entries of aMapEntrySize bits.
    procedure Build(aImgType, aPixelSize, aFlags: Byte; aWidth, aHeight: Word;
      aMapEntrySize: Byte; aMapStart, aMapLength: Word; const aMap, aData: array of Byte);
    procedure ReadIt;
    procedure ReadTwice;
    procedure ReadWithoutImage;
    procedure ReadTruncated;
    procedure WriteTooWide;
    procedure CheckColor(const aMessage: String; aX, aY: Integer; const aColor: TFPColor);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRoundTripKeepsTheColors;
    procedure TestRoundTripOfEveryWidth;
    procedure TestTheHeaderWritten;
    procedure TestTheIdentificationSurvives;
    procedure TestTooWideIsRejected;
    procedure TestWritingAfterOtherDataKeepsIt;
    procedure TestTopLeftOrigin;
    procedure TestBottomLeftOrigin;
    procedure Test32BitAlpha;
    procedure Test16BitWhiteIsWhite;
    procedure Test16BitPrimaries;
    procedure Test16BitWithOneAlphaBit;
    procedure Test15BitPixels;
    procedure TestAnIndexBeyondTheMapIsBlack;
    procedure TestGrayWhiteIsWhite;
    procedure TestGrayLevels;
    procedure TestIndexedWith24BitMap;
    procedure TestIndexedWith16BitMap;
    procedure TestIndexedWith32BitMapAlpha;
    procedure TestIndexedMapStart;
    procedure TestRLE;
    procedure TestRLETwiceWithOneReader;
    procedure TestReadingWithoutAnImage;
    procedure TestReadingDoesNotLeak;
    procedure TestATruncatedFileRaises;
    procedure TestContentsCheck;
  end;

implementation

procedure TTestTGA.SetUp;

begin
  inherited SetUp;
  FReader := TFPReaderTarga.Create;
  FWriter := TFPWriterTarga.Create;
  FStream := TMemoryStream.Create;
end;


procedure TTestTGA.TearDown;

begin
  FreeAndNil(FStream);
  FreeAndNil(FRead);
  FreeAndNil(FImage);
  FreeAndNil(FWriter);
  FreeAndNil(FReader);
  inherited TearDown;
end;


procedure TTestTGA.Build(aImgType, aPixelSize, aFlags: Byte; aWidth, aHeight: Word;
  aMapEntrySize: Byte; aMapStart, aMapLength: Word; const aMap, aData: array of Byte);

var
  lHeader: TTargaHeader;

begin
  FillChar(lHeader, SizeOf(lHeader), 0);
  lHeader.MapType := Ord(aMapLength > 0);
  lHeader.ImgType := aImgType;
  lHeader.MapStart := FromWord(aMapStart);
  lHeader.MapLength := FromWord(aMapLength);
  lHeader.MapEntrySize := aMapEntrySize;
  lHeader.Width := FromWord(aWidth);
  lHeader.Height := FromWord(aHeight);
  lHeader.PixelSize := aPixelSize;
  lHeader.Flags := aFlags;
  FStream.Clear;
  FStream.WriteBuffer(lHeader, SizeOf(lHeader));
  if Length(aMap) > 0 then
    FStream.WriteBuffer(aMap[0], Length(aMap));
  if Length(aData) > 0 then
    FStream.WriteBuffer(aData[0], Length(aData));
  FStream.Position := 0;
end;


procedure TTestTGA.ReadIt;

begin
  FreeAndNil(FRead);
  FRead := TFPMemoryImage.Create(0, 0);
  FRead.LoadFromStream(FStream, FReader);
end;


procedure TTestTGA.ReadTwice;

var
  lReader: TFPReaderTarga;
  lImage: TFPMemoryImage;

begin
  lReader := TFPReaderTarga.Create;
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


procedure TTestTGA.ReadWithoutImage;

var
  lImage: TFPCustomImage;

begin
  lImage := FReader.ImageRead(FStream, nil);
  try
    AssertEquals('The image created has the width of the file', 2, lImage.Width);
  finally
    lImage.Free;
  end;
end;


procedure TTestTGA.ReadTruncated;

begin
  FStream.Size := FStream.Size - 5;
  FStream.Position := 0;
  ReadIt;
end;


procedure TTestTGA.WriteTooWide;

begin
  FImage := TFPMemoryImage.Create(70000, 1);
  FImage.SaveToStream(FStream, FWriter);
end;


procedure TTestTGA.CheckColor(const aMessage: String; aX, aY: Integer; const aColor: TFPColor);

begin
  AssertColorsEqual(aMessage, aColor, FRead[aX, aY]);
end;


procedure TTestTGA.TestRoundTripKeepsTheColors;

begin
  FImage := CreateGradientImage(19, 7);
  FRead := RoundTrip(FImage, FWriter, FReader);
  AssertImagesEqual('A TGA keeps every colour', FImage, FRead);
end;


procedure TTestTGA.TestRoundTripOfEveryWidth;

var
  lWidth: Integer;

begin
  for lWidth := 1 to 6 do
    begin
    FreeAndNil(FImage);
    FreeAndNil(FRead);
    FImage := CreateGradientImage(lWidth, 3);
    FRead := RoundTrip(FImage, FWriter, FReader);
    AssertImagesEqual(Format('A TGA of width %d', [lWidth]), FImage, FRead);
    end;
end;


procedure TTestTGA.TestTheHeaderWritten;

var
  lHeader: TTargaHeader;

begin
  FImage := CreateGradientImage(300, 2);
  FImage.SaveToStream(FStream, FWriter);
  FStream.Position := 0;
  FStream.ReadBuffer(lHeader, SizeOf(lHeader));
  AssertEquals('Uncompressed true colour', 2, lHeader.ImgType);
  AssertEquals('No colour map', 0, lHeader.MapType);
  AssertEquals('Width', 300, ToWord(lHeader.Width));
  AssertEquals('Height', 2, ToWord(lHeader.Height));
  AssertEquals('24 bits per pixel', 24, lHeader.PixelSize);
  AssertEquals('The origin is the top left', $20, lHeader.Flags and $20);
  AssertEquals('The file holds header and pixels', SizeOf(lHeader) + 300 * 2 * 3, FStream.Size);
end;


procedure TTestTGA.TestTheIdentificationSurvives;

begin
  FImage := CreateGradientImage(3, 3);
  FImage.Extra[KeyIdentification] := 'made by the test';
  FRead := RoundTrip(FImage, FWriter, FReader);
  AssertEquals('The identification comes back', 'made by the test', FRead.Extra[KeyIdentification]);
  AssertImagesEqual('and the pixels after it', FImage, FRead);
end;


procedure TTestTGA.TestTooWideIsRejected;

begin
  AssertRaises('A width over 65535 is rejected', FPImageException, @WriteTooWide);
end;


procedure TTestTGA.TestWritingAfterOtherDataKeepsIt;

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


procedure TTestTGA.TestTopLeftOrigin;

begin
  Build(2, 24, $20, 1, 2, 0, 0, 0, [], [0, 0, 255, 255, 0, 0]);
  ReadIt;
  CheckColor('With the top-left origin the first row is the top', 0, 0, colRed);
  CheckColor('and the second row is below it', 0, 1, colBlue);
end;


procedure TTestTGA.TestBottomLeftOrigin;

begin
  Build(2, 24, 0, 1, 2, 0, 0, 0, [], [0, 0, 255, 255, 0, 0]);
  ReadIt;
  CheckColor('With the bottom-left origin the first row is the bottom', 0, 1, colRed);
  CheckColor('and the second row is above it', 0, 0, colBlue);
end;


procedure TTestTGA.Test32BitAlpha;

begin
  Build(2, 32, $28, 3, 1, 0, 0, 0, [], [30, 20, 10, 255, 30, 20, 10, 128, 30, 20, 10, 0]);
  ReadIt;
  CheckColor('Alpha 255 is opaque', 0, 0, RGB8(10, 20, 30, 255));
  CheckColor('Alpha 128 is half transparent', 1, 0, RGB8(10, 20, 30, 128));
  CheckColor('Alpha 0 is transparent', 2, 0, RGB8(10, 20, 30, 0));
end;


procedure TTestTGA.Test16BitWhiteIsWhite;

begin
  Build(2, 16, $20, 1, 1, 0, 0, 0, [], [$FF, $7F]);
  ReadIt;
  CheckColor('5-5-5 white is white', 0, 0, colWhite);
end;


procedure TTestTGA.Test16BitPrimaries;

begin
  { 5-5-5 with the attribute bit set: red $7C00, green $03E0, blue $001F }
  Build(2, 16, $20, 3, 1, 0, 0, 0, [], [$00, $FC, $E0, $83, $1F, $80]);
  ReadIt;
  AssertTrue('Pixel 0 is pure red', (FRead[0, 0].Red >= $F800) and (FRead[0, 0].Green = 0) and (FRead[0, 0].Blue = 0));
  AssertTrue('Pixel 1 is pure green', (FRead[1, 0].Green >= $F800) and (FRead[1, 0].Red = 0) and (FRead[1, 0].Blue = 0));
  AssertTrue('Pixel 2 is pure blue', (FRead[2, 0].Blue >= $F800) and (FRead[2, 0].Red = 0) and (FRead[2, 0].Green = 0));
end;


procedure TTestTGA.Test16BitWithOneAlphaBit;

begin
  { one attribute bit: set is opaque, clear is transparent }
  Build(2, 16, $21, 2, 1, 0, 0, 0, [], [$00, $FC, $00, $7C]);
  ReadIt;
  CheckColor('The attribute bit set is opaque red', 0, 0, colRed);
  AssertEquals('The attribute bit clear is transparent', alphaTransparent, FRead[1, 0].Alpha);
end;


procedure TTestTGA.Test15BitPixels;

begin
  Build(2, 15, $20, 1, 1, 0, 0, 0, [], [$1F, $00]);
  ReadIt;
  CheckColor('A pixel size of 15 is read as 5-5-5', 0, 0, colBlue);
end;


procedure TTestTGA.TestAnIndexBeyondTheMapIsBlack;

begin
  Build(1, 8, $20, 2, 1, 24, 0, 2, [0, 0, 255, 255, 255, 255], [1, 200]);
  ReadIt;
  CheckColor('An index inside the map', 0, 0, colWhite);
  CheckColor('An index past the map is black', 1, 0, colBlack);
end;


procedure TTestTGA.TestGrayWhiteIsWhite;

begin
  Build(3, 8, $20, 2, 1, 0, 0, 0, [], [255, 0]);
  ReadIt;
  CheckColor('Gray 255 is white', 0, 0, colWhite);
  CheckColor('Gray 0 is black', 1, 0, colBlack);
end;


procedure TTestTGA.TestGrayLevels;

begin
  Build(3, 8, $20, 2, 1, 0, 0, 0, [], [128, 1]);
  ReadIt;
  CheckColor('Gray 128', 0, 0, RGB8(128, 128, 128));
  CheckColor('Gray 1', 1, 0, RGB8(1, 1, 1));
end;


procedure TTestTGA.TestIndexedWith24BitMap;

begin
  Build(1, 8, $20, 3, 1, 24, 0, 2, [0, 0, 255, 255, 255, 255], [1, 0, 1]);
  ReadIt;
  CheckColor('Index 0 is red', 1, 0, colRed);
  CheckColor('Index 1 is white', 0, 0, colWhite);
end;


procedure TTestTGA.TestIndexedWith16BitMap;

begin
  { 5-5-5 map entries: red $7C00, blue $001F }
  Build(1, 8, $20, 2, 1, 16, 0, 2, [$00, $7C, $1F, $00], [1, 0]);
  ReadIt;
  AssertTrue('Index 0 is red', (FRead[1, 0].Red >= $F800) and (FRead[1, 0].Blue = 0));
  AssertTrue('Index 1 is blue', (FRead[0, 0].Blue >= $F800) and (FRead[0, 0].Red = 0));
end;


procedure TTestTGA.TestIndexedWith32BitMapAlpha;

begin
  Build(1, 8, $28, 2, 1, 32, 0, 2, [0, 0, 255, 255, 255, 0, 0, 0], [0, 1]);
  ReadIt;
  CheckColor('A map entry with alpha 255 is opaque', 0, 0, colRed);
  CheckColor('A map entry with alpha 0 is transparent', 1, 0, RGB8(0, 0, 255, 0));
end;


procedure TTestTGA.TestIndexedMapStart;

begin
  { the map starts at index 10: index 10 is the first entry }
  Build(1, 8, $20, 2, 1, 24, 10, 2, [0, 0, 255, 0, 255, 0], [11, 10]);
  ReadIt;
  CheckColor('Index 10 is the first map entry', 1, 0, colRed);
  CheckColor('Index 11 is the second map entry', 0, 0, colGreen);
end;


procedure TTestTGA.TestRLE;

begin
  { 3x2 top-down: a run of 4 red pixels over the row end, then 2 raw pixels }
  Build(10, 24, $20, 3, 2, 0, 0, 0, [],
    [$83, 0, 0, 255,
     $01, 255, 0, 0, 0, 255, 0]);
  ReadIt;
  CheckColor('Row 0, pixel 0 is red', 0, 0, colRed);
  CheckColor('Row 0, pixel 2 is red', 2, 0, colRed);
  CheckColor('The run goes on into row 1', 0, 1, colRed);
  CheckColor('Row 1, pixel 1 is the first raw pixel', 1, 1, colBlue);
  CheckColor('Row 1, pixel 2 is the second raw pixel', 2, 1, colGreen);
end;


procedure TTestTGA.TestRLETwiceWithOneReader;

begin
  Build(10, 24, $20, 3, 1, 0, 0, 0, [], [$81, 0, 0, 255, $00, 255, 0, 0]);
  ReadIt;
  FStream.Position := 0;
  ReadIt;
  CheckColor('A second read with the same reader starts afresh', 0, 0, colRed);
  CheckColor('and ends the same', 2, 0, colBlue);
end;


procedure TTestTGA.TestReadingWithoutAnImage;

begin
  Build(2, 24, $20, 2, 1, 0, 0, 0, [], [0, 0, 0, 0, 0, 0]);
  ReadWithoutImage;
end;


procedure TTestTGA.TestReadingDoesNotLeak;

begin
  Build(1, 8, $20, 2, 1, 24, 0, 2, [0, 0, 255, 255, 255, 255], [1, 0]);
  AssertNoLeak('Reading twice with one reader frees what the first read allocated', @ReadTwice);
end;


procedure TTestTGA.TestATruncatedFileRaises;

begin
  Build(2, 24, $20, 4, 4, 0, 0, 0, [], [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]);
  AssertRaises('A file that ends in the middle of the pixels raises', Exception, @ReadTruncated);
end;


procedure TTestTGA.TestContentsCheck;

begin
  Build(2, 24, $20, 1, 1, 0, 0, 0, [], [0, 0, 0]);
  AssertTrue('A valid header is accepted', FReader.CheckContents(FStream));
  AssertEquals('The check leaves the position alone', 0, FStream.Position);
  Build(7, 24, $20, 1, 1, 0, 0, 0, [], [0, 0, 0]);
  AssertFalse('An unknown image type is rejected', FReader.CheckContents(FStream));
  Build(2, 12, $20, 1, 1, 0, 0, 0, [], [0, 0, 0]);
  AssertFalse('An unknown pixel size is rejected', FReader.CheckContents(FStream));
end;


initialization
  RegisterTest('tga', TTestTGA);
end.
