{
    Tests for the X Window Dump (XWD version 7) reader: hand-built ZPixmap
    files of every depth, both byte and bit orders, colormaps and masks.
    See the file COPYING.FPC, included in this distribution, for details.
}
unit tcxwd;

{$mode objfpc}{$H+}

interface

uses sysutils, classes, fpcunit, testregistry, fpimage, fpimgtests,
     xwdfile, fpreadxwd;

const
  XWDLSBFirst = 0;
  XWDMSBFirst = 1;
  XWDStaticGray = 0;
  XWDPseudoColor = 3;
  XWDTrueColor = 4;
  XWDXYPixmap = 1;
  XWDZPixmap = 2;

type
  TTestXWDReader = class(TTestCase)
  private
    FReader: TFPReaderXWD;
    FRead: TFPMemoryImage;
    FStream: TMemoryStream;
    FPrefix: String;
    FName: String;
    FFormat: Cardinal;
    FByteOrder: Cardinal;
    FBitOrder: Cardinal;
    FHeaderSize: Cardinal;
    FLittleEndianHeader: Boolean;
    // Writes a header value, big-endian unless FLittleEndianHeader.
    procedure WriteHeader32(aValue: Cardinal);
    // Writes a big-endian longword to the stream.
    procedure WriteBE32(aValue: Cardinal);
    // Writes a big-endian word to the stream.
    procedure WriteBE16(aValue: Word);
    // Writes bytes to the stream.
    procedure WriteBytes(const aBytes: array of Byte);
    // Builds an XWD file after FPrefix; aColors holds pixel, red, green, blue per colormap entry.
    procedure Build(aDepth, aBitsPerPixel, aWidth, aHeight, aBytesPerLine, aVisual: Cardinal;
      aRedMask, aGreenMask, aBlueMask: Cardinal; const aColors: array of Cardinal; const aData: array of Byte);
    // Builds a 3x2 pseudo colour file with rows padded to 4 bytes.
    procedure BuildPseudoColor;
    // Builds a 3x1 true colour file of 32 bits per pixel with 8-bit masks.
    procedure BuildTrueColor32(aByteOrder: Cardinal; const aData: array of Byte);
    // Reads the stream from its position into FRead.
    procedure ReadIt;
    // Reads the stream twice with one new reader into one new image.
    procedure ReadTwice;
    // Reads the stream with ImageRead and no image, and checks the image created.
    procedure ReadWithoutImage;
    // Cuts the last 3 bytes of the stream and reads it from the start.
    procedure ReadTruncated;
    // Reads the stream from the start into a new image, swallowing any exception.
    procedure ReadCatching;
    // Fails unless the pixel of FRead at (aX,aY) has the red, green and blue of aColor.
    procedure CheckRGB(const aMessage: String; aX, aY: Integer; const aColor: TFPColor);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestPseudoColor8;
    procedure TestColormapColorsAreOpaque;
    procedure TestWidePaddingIsSkipped;
    procedure TestWindowNameIsSkipped;
    procedure TestEmptyWindowName;
    procedure TestMonochromeMSBFirst;
    procedure TestMonochromeLSBFirst;
    procedure TestFourBitMSBFirst;
    procedure TestFourBitLSBFirst;
    procedure TestTrueColor16LSBFirst;
    procedure TestTrueColor15MSBFirst;
    procedure TestTrueColor24LSBFirst;
    procedure TestTrueColor24MSBFirst;
    procedure TestTrueColor32LSBFirst;
    procedure TestTrueColor32MSBFirst;
    procedure TestTrueColor32OtherMasks;
    procedure TestTrueColorIgnoresTheColormap;
    procedure TestTrueColorIsOpaque;
    procedure TestXYPixmapIsRejected;
    procedure TestReadingAfterAPrefix;
    procedure TestReaderStopsAtTheEndOfTheImage;
    procedure TestContentsCheck;
    procedure TestContentsCheckRejectsAnotherVersion;
    procedure TestContentsCheckRejectsAShortStream;
    procedure TestLittleEndianHeaderIsRejected;
    procedure TestReadingWithoutAnImage;
    procedure TestReadingDoesNotLeak;
    procedure TestAFailingReadDoesNotLeak;
    procedure TestATruncatedFileRaises;
    procedure TestATruncatedColormapRaises;
    procedure TestAShortHeaderSizeRaises;
    procedure TestXWDIsRegistered;
  end;

implementation

procedure TTestXWDReader.SetUp;

begin
  inherited SetUp;
  FReader := TFPReaderXWD.Create;
  FStream := TMemoryStream.Create;
  FPrefix := '';
  FName := 'xwdtest';
  FFormat := XWDZPixmap;
  FByteOrder := XWDMSBFirst;
  FBitOrder := XWDMSBFirst;
  FHeaderSize := 0;
  FLittleEndianHeader := False;
end;


procedure TTestXWDReader.TearDown;

begin
  FreeAndNil(FStream);
  FreeAndNil(FRead);
  FreeAndNil(FReader);
  inherited TearDown;
end;


procedure TTestXWDReader.WriteHeader32(aValue: Cardinal);

begin
  if FLittleEndianHeader then
    WriteBytes([aValue and $FF, (aValue shr 8) and $FF, (aValue shr 16) and $FF, aValue shr 24])
  else
    WriteBE32(aValue);
end;


procedure TTestXWDReader.WriteBE32(aValue: Cardinal);

begin
  WriteBytes([aValue shr 24, (aValue shr 16) and $FF, (aValue shr 8) and $FF, aValue and $FF]);
end;


procedure TTestXWDReader.WriteBE16(aValue: Word);

begin
  WriteBytes([aValue shr 8, aValue and $FF]);
end;


procedure TTestXWDReader.WriteBytes(const aBytes: array of Byte);

begin
  if Length(aBytes) > 0 then
    FStream.WriteBuffer(aBytes[0], Length(aBytes));
end;


procedure TTestXWDReader.Build(aDepth, aBitsPerPixel, aWidth, aHeight, aBytesPerLine, aVisual: Cardinal;
  aRedMask, aGreenMask, aBlueMask: Cardinal; const aColors: array of Cardinal; const aData: array of Byte);

var
  lColorCount: Cardinal;
  I: Integer;

begin
  lColorCount := Length(aColors) div 4;
  FStream.Clear;
  if FPrefix <> '' then
    FStream.WriteBuffer(FPrefix[1], Length(FPrefix));
  if FHeaderSize <> 0 then
    WriteHeader32(FHeaderSize)
  else
    WriteHeader32(sz_XWDheader + Length(FName) + 1);
  WriteHeader32(XWD_FILE_VERSION);
  WriteHeader32(FFormat);
  WriteHeader32(aDepth);
  WriteHeader32(aWidth);
  WriteHeader32(aHeight);
  WriteHeader32(0);
  WriteHeader32(FByteOrder);
  WriteHeader32(32);
  WriteHeader32(FBitOrder);
  WriteHeader32(32);
  WriteHeader32(aBitsPerPixel);
  WriteHeader32(aBytesPerLine);
  WriteHeader32(aVisual);
  WriteHeader32(aRedMask);
  WriteHeader32(aGreenMask);
  WriteHeader32(aBlueMask);
  WriteHeader32(8);
  WriteHeader32(lColorCount);
  WriteHeader32(lColorCount);
  WriteHeader32(aWidth);
  WriteHeader32(aHeight);
  WriteHeader32(0);
  WriteHeader32(0);
  WriteHeader32(0);
  if FName <> '' then
    FStream.WriteBuffer(FName[1], Length(FName));
  WriteBytes([0]);
  for I := 0 to Integer(lColorCount) - 1 do
    begin
    WriteBE32(aColors[I * 4]);
    WriteBE16(aColors[I * 4 + 1]);
    WriteBE16(aColors[I * 4 + 2]);
    WriteBE16(aColors[I * 4 + 3]);
    WriteBytes([7, 0]);
    end;
  WriteBytes(aData);
  FStream.Position := Length(FPrefix);
end;


procedure TTestXWDReader.BuildPseudoColor;

begin
  Build(8, 8, 3, 2, 4, XWDPseudoColor, 0, 0, 0,
    [5, $FFFF, 0, 0,
     9, 0, $8000, $FFFF,
     0, $1234, $5678, $9ABC],
    [5, 9, 0, $EE,
     0, 5, 9, $EE]);
end;


procedure TTestXWDReader.BuildTrueColor32(aByteOrder: Cardinal; const aData: array of Byte);

begin
  FByteOrder := aByteOrder;
  Build(24, 32, 3, 1, 12, XWDTrueColor, $FF0000, $FF00, $FF, [], aData);
end;


procedure TTestXWDReader.ReadIt;

begin
  FreeAndNil(FRead);
  FRead := TFPMemoryImage.Create(0, 0);
  FRead.LoadFromStream(FStream, FReader);
end;


procedure TTestXWDReader.ReadTwice;

var
  lReader: TFPReaderXWD;
  lImage: TFPMemoryImage;

begin
  lReader := TFPReaderXWD.Create;
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


procedure TTestXWDReader.ReadWithoutImage;

var
  lImage: TFPCustomImage;
  lColor: TFPColor;

begin
  lImage := FReader.ImageRead(FStream, nil);
  try
    AssertNotNull('An image is created', lImage);
    AssertEquals('The image created has the width of the file', 3, lImage.Width);
    AssertEquals('The image created has the height of the file', 2, lImage.Height);
    lColor := lImage.Colors[0, 0];
    lColor.Alpha := alphaOpaque;
    AssertColorsEqual('The image created has the pixels of the file', colRed, lColor);
  finally
    lImage.Free;
  end;
end;


procedure TTestXWDReader.ReadTruncated;

begin
  FStream.Size := FStream.Size - 3;
  FStream.Position := 0;
  ReadIt;
end;


procedure TTestXWDReader.ReadCatching;

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


procedure TTestXWDReader.CheckRGB(const aMessage: String; aX, aY: Integer; const aColor: TFPColor);

var
  lActual: TFPColor;

begin
  lActual := FRead[aX, aY];
  lActual.Alpha := aColor.Alpha;
  AssertColorsEqual(aMessage, aColor, lActual);
end;


procedure TTestXWDReader.TestPseudoColor8;

begin
  BuildPseudoColor;
  ReadIt;
  AssertEquals('The width', 3, FRead.Width);
  AssertEquals('The height', 2, FRead.Height);
  CheckRGB('Pixel value 5 is the colormap entry of pixel 5', 0, 0, colRed);
  CheckRGB('Pixel value 9 is the colormap entry of pixel 9', 1, 0, FPColor(0, $8000, $FFFF));
  CheckRGB('Pixel value 0 is the colormap entry of pixel 0', 2, 0, FPColor($1234, $5678, $9ABC));
  CheckRGB('Row 1 starts after the padding: pixel value 0', 0, 1, FPColor($1234, $5678, $9ABC));
  CheckRGB('Row 1, pixel value 5', 1, 1, colRed);
  CheckRGB('Row 1, pixel value 9', 2, 1, FPColor(0, $8000, $FFFF));
end;


procedure TTestXWDReader.TestColormapColorsAreOpaque;

begin
  BuildPseudoColor;
  ReadIt;
  AssertEquals('A colormap colour is opaque', alphaOpaque, FRead[0, 0].Alpha);
end;


procedure TTestXWDReader.TestWidePaddingIsSkipped;

begin
  Build(8, 8, 1, 3, 8, XWDPseudoColor, 0, 0, 0,
    [0, 0, 0, 0,
     1, $FFFF, $FFFF, $FFFF],
    [1, 0, 0, 0, 0, 0, 0, 0,
     0, 1, 1, 1, 1, 1, 1, 1,
     1, 0, 0, 0, 0, 0, 0, 0]);
  ReadIt;
  CheckRGB('Row 0 is white', 0, 0, colWhite);
  CheckRGB('Row 1 starts after 8 bytes: black', 0, 1, colBlack);
  CheckRGB('Row 2 is white', 0, 2, colWhite);
end;


procedure TTestXWDReader.TestWindowNameIsSkipped;

begin
  FName := 'A window name of some length, with spaces and digits 0123456789';
  BuildPseudoColor;
  ReadIt;
  CheckRGB('Pixel (0,0) after a long window name', 0, 0, colRed);
  CheckRGB('Pixel (2,1) after a long window name', 2, 1, FPColor(0, $8000, $FFFF));
end;


procedure TTestXWDReader.TestEmptyWindowName;

begin
  FName := '';
  BuildPseudoColor;
  ReadIt;
  CheckRGB('Pixel (0,0) after an empty window name', 0, 0, colRed);
  CheckRGB('Pixel (2,1) after an empty window name', 2, 1, FPColor(0, $8000, $FFFF));
end;


procedure TTestXWDReader.TestMonochromeMSBFirst;

begin
  FBitOrder := XWDMSBFirst;
  Build(1, 1, 3, 1, 4, XWDStaticGray, 0, 0, 0,
    [0, 0, 0, 0,
     1, $FFFF, $FFFF, $FFFF],
    [$C0, 0, 0, 0]);
  ReadIt;
  CheckRGB('MSBFirst: the high bit is pixel 0', 0, 0, colWhite);
  CheckRGB('MSBFirst: the next bit is pixel 1', 1, 0, colWhite);
  CheckRGB('MSBFirst: pixel 2 is clear', 2, 0, colBlack);
end;


procedure TTestXWDReader.TestMonochromeLSBFirst;

begin
  FBitOrder := XWDLSBFirst;
  Build(1, 1, 3, 1, 4, XWDStaticGray, 0, 0, 0,
    [0, 0, 0, 0,
     1, $FFFF, $FFFF, $FFFF],
    [$03, 0, 0, 0]);
  ReadIt;
  CheckRGB('LSBFirst: the low bit is pixel 0', 0, 0, colWhite);
  CheckRGB('LSBFirst: the next bit is pixel 1', 1, 0, colWhite);
  CheckRGB('LSBFirst: pixel 2 is clear', 2, 0, colBlack);
end;


procedure TTestXWDReader.TestFourBitMSBFirst;

begin
  FByteOrder := XWDMSBFirst;
  Build(4, 4, 3, 1, 4, XWDPseudoColor, 0, 0, 0,
    [0, 0, 0, 0,
     1, $FFFF, 0, 0,
     2, 0, $FFFF, 0],
    [$12, $00, 0, 0]);
  ReadIt;
  CheckRGB('MSBFirst: the high nibble is pixel 0', 0, 0, colRed);
  CheckRGB('MSBFirst: the low nibble is pixel 1', 1, 0, colGreen);
  CheckRGB('MSBFirst: pixel 2 is in the next byte', 2, 0, colBlack);
end;


procedure TTestXWDReader.TestFourBitLSBFirst;

begin
  FByteOrder := XWDLSBFirst;
  Build(4, 4, 3, 1, 4, XWDPseudoColor, 0, 0, 0,
    [0, 0, 0, 0,
     1, $FFFF, 0, 0,
     2, 0, $FFFF, 0],
    [$21, $00, 0, 0]);
  ReadIt;
  CheckRGB('LSBFirst: the low nibble is pixel 0', 0, 0, colRed);
  CheckRGB('LSBFirst: the high nibble is pixel 1', 1, 0, colGreen);
  CheckRGB('LSBFirst: pixel 2 is in the next byte', 2, 0, colBlack);
end;


procedure TTestXWDReader.TestTrueColor16LSBFirst;

begin
  FByteOrder := XWDLSBFirst;
  Build(16, 16, 5, 1, 12, XWDTrueColor, $F800, $07E0, $001F, [],
    [$FF, $FF, $00, $F8, $E0, $07, $1F, $00, $00, $00, $AA, $AA]);
  ReadIt;
  CheckRGB('5-6-5 $FFFF is white', 0, 0, colWhite);
  CheckRGB('5-6-5 $F800 is red', 1, 0, colRed);
  CheckRGB('5-6-5 $07E0 is green', 2, 0, colGreen);
  CheckRGB('5-6-5 $001F is blue', 3, 0, colBlue);
  CheckRGB('5-6-5 0 is black', 4, 0, colBlack);
end;


procedure TTestXWDReader.TestTrueColor15MSBFirst;

begin
  FByteOrder := XWDMSBFirst;
  Build(15, 16, 4, 1, 8, XWDTrueColor, $7C00, $03E0, $001F, [],
    [$7C, $00, $03, $E0, $00, $1F, $7F, $FF]);
  ReadIt;
  CheckRGB('5-5-5 $7C00 is red', 0, 0, colRed);
  CheckRGB('5-5-5 $03E0 is green', 1, 0, colGreen);
  CheckRGB('5-5-5 $001F is blue', 2, 0, colBlue);
  CheckRGB('5-5-5 $7FFF is white', 3, 0, colWhite);
end;


procedure TTestXWDReader.TestTrueColor24LSBFirst;

begin
  FByteOrder := XWDLSBFirst;
  Build(24, 24, 3, 2, 12, XWDTrueColor, $FF0000, $FF00, $FF, [],
    [30, 20, 10, 60, 50, 40, 90, 80, 70, $AA, $AA, $AA,
     3, 2, 1, 6, 5, 4, 9, 8, 7, $AA, $AA, $AA]);
  ReadIt;
  CheckRGB('LSBFirst 24 bits: pixel (0,0)', 0, 0, RGB8(10, 20, 30));
  CheckRGB('LSBFirst 24 bits: pixel (1,0)', 1, 0, RGB8(40, 50, 60));
  CheckRGB('LSBFirst 24 bits: pixel (2,0)', 2, 0, RGB8(70, 80, 90));
  CheckRGB('LSBFirst 24 bits: row 1 starts after the padding', 0, 1, RGB8(1, 2, 3));
  CheckRGB('LSBFirst 24 bits: pixel (2,1)', 2, 1, RGB8(7, 8, 9));
end;


procedure TTestXWDReader.TestTrueColor24MSBFirst;

begin
  FByteOrder := XWDMSBFirst;
  Build(24, 24, 3, 1, 12, XWDTrueColor, $FF0000, $FF00, $FF, [],
    [10, 20, 30, 40, 50, 60, 70, 80, 90, $AA, $AA, $AA]);
  ReadIt;
  CheckRGB('MSBFirst 24 bits: pixel 0', 0, 0, RGB8(10, 20, 30));
  CheckRGB('MSBFirst 24 bits: pixel 1', 1, 0, RGB8(40, 50, 60));
  CheckRGB('MSBFirst 24 bits: pixel 2', 2, 0, RGB8(70, 80, 90));
end;


procedure TTestXWDReader.TestTrueColor32LSBFirst;

begin
  BuildTrueColor32(XWDLSBFirst, [30, 20, 10, 0, 60, 50, 40, 0, 90, 80, 70, 0]);
  ReadIt;
  CheckRGB('LSBFirst 32 bits: pixel 0', 0, 0, RGB8(10, 20, 30));
  CheckRGB('LSBFirst 32 bits: pixel 1', 1, 0, RGB8(40, 50, 60));
  CheckRGB('LSBFirst 32 bits: pixel 2', 2, 0, RGB8(70, 80, 90));
end;


procedure TTestXWDReader.TestTrueColor32MSBFirst;

begin
  BuildTrueColor32(XWDMSBFirst, [0, 10, 20, 30, 0, 40, 50, 60, 0, 70, 80, 90]);
  ReadIt;
  CheckRGB('MSBFirst 32 bits: pixel 0', 0, 0, RGB8(10, 20, 30));
  CheckRGB('MSBFirst 32 bits: pixel 1', 1, 0, RGB8(40, 50, 60));
  CheckRGB('MSBFirst 32 bits: pixel 2', 2, 0, RGB8(70, 80, 90));
end;


procedure TTestXWDReader.TestTrueColor32OtherMasks;

begin
  FByteOrder := XWDLSBFirst;
  Build(24, 32, 2, 1, 8, XWDTrueColor, $FF, $FF00, $FF0000, [],
    [10, 20, 30, 0, 40, 50, 60, 0]);
  ReadIt;
  CheckRGB('The red mask $FF selects the low byte', 0, 0, RGB8(10, 20, 30));
  CheckRGB('The blue mask $FF0000 selects the third byte', 1, 0, RGB8(40, 50, 60));
end;


procedure TTestXWDReader.TestTrueColorIgnoresTheColormap;

begin
  FByteOrder := XWDMSBFirst;
  Build(24, 32, 2, 1, 8, XWDTrueColor, $FF0000, $FF00, $FF,
    [0, $FFFF, $FFFF, $FFFF,
     1, $FFFF, 0, $FFFF],
    [0, 10, 20, 30, 0, 0, 0, 1]);
  ReadIt;
  CheckRGB('The pixel value decomposed by the masks, after the colormap', 0, 0, RGB8(10, 20, 30));
  CheckRGB('Pixel value 1 is blue 1, not colormap entry 1', 1, 0, RGB8(0, 0, 1));
  AssertEquals('The reader stops at the end of the image', FStream.Size, FStream.Position);
end;


procedure TTestXWDReader.TestTrueColorIsOpaque;

begin
  BuildTrueColor32(XWDMSBFirst, [0, 10, 20, 30, 0, 40, 50, 60, 0, 70, 80, 90]);
  ReadIt;
  AssertEquals('A true colour pixel is opaque', alphaOpaque, FRead[0, 0].Alpha);
end;


procedure TTestXWDReader.TestXYPixmapIsRejected;

begin
  FFormat := XWDXYPixmap;
  BuildPseudoColor;
  AssertRaises('An XYPixmap of depth 8 is rejected', FPImageException, @ReadIt);
end;


procedure TTestXWDReader.TestReadingAfterAPrefix;

begin
  FPrefix := 'prefix';
  BuildPseudoColor;
  ReadIt;
  CheckRGB('Pixel (0,0) of the image after the prefix', 0, 0, colRed);
  CheckRGB('Pixel (2,1) of the image after the prefix', 2, 1, FPColor(0, $8000, $FFFF));
  AssertEquals('The reader stops at the end of the image', FStream.Size, FStream.Position);
end;


procedure TTestXWDReader.TestReaderStopsAtTheEndOfTheImage;

var
  lEnd: Int64;

begin
  BuildPseudoColor;
  lEnd := FStream.Size;
  FStream.Seek(0, soEnd);
  WriteBytes([1, 2, 3, 4, 5]);
  FStream.Position := 0;
  ReadIt;
  AssertEquals('The reader stops after the last row', lEnd, FStream.Position);
end;


procedure TTestXWDReader.TestContentsCheck;

begin
  BuildPseudoColor;
  AssertTrue('A valid header is accepted', FReader.CheckContents(FStream));
  AssertEquals('The check leaves the position alone', 0, FStream.Position);
end;


procedure TTestXWDReader.TestContentsCheckRejectsAnotherVersion;

begin
  BuildPseudoColor;
  PByte(FStream.Memory)[7] := 6;
  AssertFalse('Version 6 is rejected', FReader.CheckContents(FStream));
end;


procedure TTestXWDReader.TestContentsCheckRejectsAShortStream;

begin
  BuildPseudoColor;
  FStream.Size := 8;
  FStream.Position := 0;
  AssertFalse('A stream shorter than the header is rejected', FReader.CheckContents(FStream));
end;


procedure TTestXWDReader.TestLittleEndianHeaderIsRejected;

begin
  FLittleEndianHeader := True;
  BuildPseudoColor;
  AssertFalse('A header of least significant bytes first is rejected', FReader.CheckContents(FStream));
end;


procedure TTestXWDReader.TestReadingWithoutAnImage;

begin
  BuildPseudoColor;
  ReadWithoutImage;
end;


procedure TTestXWDReader.TestReadingDoesNotLeak;

begin
  BuildPseudoColor;
  AssertNoLeak('Reading twice with one reader frees what the first read allocated', @ReadTwice);
end;


procedure TTestXWDReader.TestAFailingReadDoesNotLeak;

begin
  FHeaderSize := 50;
  BuildPseudoColor;
  AssertNoLeak('A read that fails frees what it allocated', @ReadCatching);
end;


procedure TTestXWDReader.TestATruncatedFileRaises;

begin
  BuildPseudoColor;
  AssertRaises('A file that ends in the middle of the pixels raises', FPImageException, @ReadTruncated);
end;


procedure TTestXWDReader.TestATruncatedColormapRaises;

begin
  Build(8, 8, 1, 1, 4, XWDPseudoColor, 0, 0, 0,
    [0, 0, 0, 0,
     1, $FFFF, $FFFF, $FFFF],
    []);
  AssertRaises('A file that ends in the colormap raises', FPImageException, @ReadTruncated);
end;


procedure TTestXWDReader.TestAShortHeaderSizeRaises;

begin
  FHeaderSize := 50;
  BuildPseudoColor;
  AssertRaises('A header size below the size of the header raises', FPImageException, @ReadIt);
end;


procedure TTestXWDReader.TestXWDIsRegistered;

begin
  AssertTrue('The XWD Format type has the XWD reader', ImageHandlers.ImageReader['XWD Format'] = TFPReaderXWD);
  AssertTrue('The .xwd extension finds the XWD reader', TFPCustomImage.FindReaderFromExtension('xwd') = TFPReaderXWD);
end;


initialization
  RegisterTest('xwd', TTestXWDReader);
end.
