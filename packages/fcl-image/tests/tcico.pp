{
    Tests for the ICO and CUR readers and writers: BMP and PNG entries, masks, several sizes,
    the directory written, cursor hotspots and the detection of the formats.
    See the file COPYING.FPC, included in this distribution, for details.
}
unit tcico;

{$mode objfpc}{$H+}

interface

uses sysutils, classes, types, fpcunit, testregistry, fpimage, fpimgtests,
     icocomn, fpreadico, fpwriteico;

type
  TTestICO = class(TTestCase)
  private
    FReader: TFPReaderICO;
    FWriter: TFPWriterICO;
    FImage: TFPMemoryImage;
    FRead: TFPMemoryImage;
    FStream: TMemoryStream;
    FEntries: array of TBytes;
    FSizes: array of TPoint;
    FBitCounts: array of Word;
    // Adds raw image data as an entry of the file BuildFile makes.
    procedure AddRaw(const aData: TBytes; aWidth, aHeight: Integer; aBitCount: Word);
    // Replaces the stream with a file of type aType holding the entries added with AddRaw.
    procedure BuildFile(aType: Word);
    // Returns a 40-byte bitmap info header for an entry of aWidth x aHeight pixels.
    function DIBHeader(aWidth, aHeight: Integer; aBits: Word): TBytes;
    procedure ReadIt;
    // Returns a copy of aImage with every fully transparent pixel set to colTransparent.
    function Masked(aImage: TFPCustomImage): TFPMemoryImage;
    function ByteAt(aPos: Integer): Byte;
    function WordAt(aPos: Integer): Word;
    function LongAt(aPos: Integer): LongWord;
    procedure AddTooLarge;
    procedure SaveNothing;
    procedure ReadTruncated;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRoundTripOfABitmapEntry;
    procedure TestRoundTripOfAPNGEntry;
    procedure TestRoundTripOfA256PixelBitmap;
    procedure TestTheWriterUsesPNGAt256Pixels;
    procedure TestTheDirectoryWritten;
    procedure TestSeveralSizes;
    procedure TestTheReaderTakesTheLargestEntry;
    procedure TestTheReaderPrefersTheDeepestOfOneSize;
    procedure TestAPalettedEntryUsesItsMask;
    procedure TestA32BitEntryWithoutAlphaUsesItsMask;
    procedure TestA32BitEntryWithAlphaIgnoresItsMask;
    procedure TestReadingAfterOtherData;
    procedure TestImageSizeIsThatOfTheLargestEntry;
    procedure TestACursorKeepsItsHotSpot;
    procedure TestIconsAndCursorsAreToldApart;
    procedure TestContentsCheck;
    procedure TestAnEntryBeyondTheEndIsRejected;
    procedure TestTooLargeImagesRaise;
    procedure TestSavingNothingRaises;
    procedure TestImageWriteKeepsTheImagesAdded;
    procedure TestTheFormatsAreRegistered;
  end;

implementation

procedure TTestICO.SetUp;

begin
  inherited SetUp;
  FReader := TFPReaderICO.Create;
  FWriter := TFPWriterICO.Create;
  FStream := TMemoryStream.Create;
end;


procedure TTestICO.TearDown;

begin
  FreeAndNil(FStream);
  FreeAndNil(FRead);
  FreeAndNil(FImage);
  FreeAndNil(FWriter);
  FreeAndNil(FReader);
  FEntries := nil;
  FSizes := nil;
  FBitCounts := nil;
  inherited TearDown;
end;


procedure TTestICO.AddRaw(const aData: TBytes; aWidth, aHeight: Integer; aBitCount: Word);

var
  lIndex: Integer;

begin
  lIndex := Length(FEntries);
  SetLength(FEntries, lIndex + 1);
  SetLength(FSizes, lIndex + 1);
  SetLength(FBitCounts, lIndex + 1);
  FEntries[lIndex] := aData;
  FSizes[lIndex] := Point(aWidth, aHeight);
  FBitCounts[lIndex] := aBitCount;
end;


procedure TTestICO.BuildFile(aType: Word);

var
  lDir: TIconDir;
  lEntry: TIconDirEntry;
  lOffset: LongWord;
  i: Integer;

begin
  FStream.Clear;
  lDir.Reserved := 0;
  lDir.IconType := aType;
  lDir.Count := Length(FEntries);
  SwapIconDir(lDir);
  FStream.WriteBuffer(lDir, SizeOf(lDir));
  lOffset := SizeOf(TIconDir) + Length(FEntries) * SizeOf(TIconDirEntry);
  for i := 0 to High(FEntries) do
    begin
    lEntry.Width := FSizes[i].X and $FF;
    lEntry.Height := FSizes[i].Y and $FF;
    lEntry.ColorCount := 0;
    lEntry.Reserved := 0;
    lEntry.Planes := 1;
    lEntry.BitCount := FBitCounts[i];
    lEntry.BytesInRes := Length(FEntries[i]);
    lEntry.ImageOffset := lOffset;
    Inc(lOffset, Length(FEntries[i]));
    SwapIconDirEntry(lEntry);
    FStream.WriteBuffer(lEntry, SizeOf(lEntry));
    end;
  for i := 0 to High(FEntries) do
    FStream.WriteBuffer(FEntries[i][0], Length(FEntries[i]));
  FStream.Position := 0;
end;


function TTestICO.DIBHeader(aWidth, aHeight: Integer; aBits: Word): TBytes;

begin
  Result := nil;
  SetLength(Result, 40);
  FillChar(Result[0], 40, 0);
  PLongInt(@Result[0])^ := NtoLE(LongInt(40));
  PLongInt(@Result[4])^ := NtoLE(LongInt(aWidth));
  PLongInt(@Result[8])^ := NtoLE(LongInt(aHeight * 2));
  PWord(@Result[12])^ := NtoLE(Word(1));
  PWord(@Result[14])^ := NtoLE(aBits);
end;


procedure TTestICO.ReadIt;

begin
  FreeAndNil(FRead);
  FRead := TFPMemoryImage.Create(0, 0);
  FRead.LoadFromStream(FStream, FReader);
end;


function TTestICO.Masked(aImage: TFPCustomImage): TFPMemoryImage;

var
  x, y: Integer;

begin
  Result := TFPMemoryImage.Create(aImage.Width, aImage.Height);
  for y := 0 to aImage.Height - 1 do
    for x := 0 to aImage.Width - 1 do
      if aImage.Colors[x, y].Alpha shr 8 = 0 then
        Result.Colors[x, y] := colTransparent
      else
        Result.Colors[x, y] := aImage.Colors[x, y];
end;


function TTestICO.ByteAt(aPos: Integer): Byte;

begin
  Result := PByte(FStream.Memory)[aPos];
end;


function TTestICO.WordAt(aPos: Integer): Word;

begin
  Result := LEtoN(PWord(PByte(FStream.Memory) + aPos)^);
end;


function TTestICO.LongAt(aPos: Integer): LongWord;

begin
  Result := LEtoN(PLongWord(PByte(FStream.Memory) + aPos)^);
end;


procedure TTestICO.AddTooLarge;

var
  lImage: TFPMemoryImage;

begin
  lImage := CreateSolidImage(257, 16, colRed);
  try
    FWriter.AddImage(lImage);
  finally
    lImage.Free;
  end;
end;


procedure TTestICO.SaveNothing;

begin
  FWriter.SaveToStream(FStream);
end;


procedure TTestICO.ReadTruncated;

begin
  FStream.Position := 0;
  ReadIt;
end;


procedure TTestICO.TestRoundTripOfABitmapEntry;

var
  lExpected: TFPMemoryImage;

begin
  FImage := CreateAlphaImage(11, 9);
  FWriter.Format := iwfBMP;
  FRead := RoundTrip(FImage, FWriter, FReader);
  lExpected := Masked(FImage);
  try
    AssertImagesEqual('A bitmap entry keeps colours and alpha', lExpected, FRead);
  finally
    lExpected.Free;
  end;
end;


procedure TTestICO.TestRoundTripOfAPNGEntry;

begin
  FImage := CreateAlphaImage(13, 7);
  FWriter.Format := iwfPNG;
  FRead := RoundTrip(FImage, FWriter, FReader);
  AssertImagesEqual('A PNG entry keeps colours and alpha', FImage, FRead);
end;


procedure TTestICO.TestRoundTripOfA256PixelBitmap;

begin
  FImage := CreateGradientImage(256, 256);
  FWriter.Format := iwfBMP;
  FRead := RoundTrip(FImage, FWriter, FReader);
  AssertImagesEqual('A bitmap entry of 256 pixels, stored as 0 in the directory, comes back', FImage, FRead);
end;


procedure TTestICO.TestTheWriterUsesPNGAt256Pixels;

begin
  FImage := CreateGradientImage(256, 256);
  FWriter.AddImage(FImage);
  FreeAndNil(FImage);
  FImage := CreateGradientImage(48, 48);
  FWriter.AddImage(FImage);
  FWriter.SaveToStream(FStream);
  FStream.Position := 0;
  FReader.LoadFromStream(FStream);
  AssertTrue('An image of 256 pixels is stored as PNG', FReader.Entries[0].Format = iefPNG);
  AssertTrue('A smaller image is stored as a bitmap', FReader.Entries[1].Format = iefBMP);
end;


procedure TTestICO.TestTheDirectoryWritten;

var
  lFirstSize: LongWord;

begin
  FImage := CreateGradientImage(256, 256);
  FWriter.AddImage(FImage);
  FreeAndNil(FImage);
  FImage := CreateGradientImage(16, 8);
  FWriter.AddImage(FImage);
  FWriter.SaveToStream(FStream);
  AssertEquals('Reserved', 0, WordAt(0));
  AssertEquals('The type of an icon', IcoTypeIcon, WordAt(2));
  AssertEquals('The number of entries', 2, WordAt(4));
  AssertEquals('A width of 256 is written as 0', 0, ByteAt(6));
  AssertEquals('A height of 256 is written as 0', 0, ByteAt(7));
  AssertEquals('One plane', 1, WordAt(10));
  AssertEquals('32 bits per pixel', 32, WordAt(12));
  AssertEquals('The first image follows the directory', 6 + 2 * 16, LongAt(18));
  lFirstSize := LongAt(14);
  AssertEquals('The width of the second entry', 16, ByteAt(22));
  AssertEquals('The height of the second entry', 8, ByteAt(23));
  AssertEquals('The second image follows the first', 6 + 2 * 16 + lFirstSize, LongAt(34));
  AssertTrue('The first image is a PNG', CompareMem(PByte(FStream.Memory) + 38, @IcoPNGSignature[0], 8));
  AssertEquals('The second image starts with a bitmap info header', 40, LongAt(38 + lFirstSize));
  AssertEquals('The bitmap height counts the mask as well', 16, LongAt(38 + lFirstSize + 8));
  AssertEquals('The file ends with the second image', 38 + lFirstSize + LongAt(30), FStream.Size);
end;


procedure TTestICO.TestSeveralSizes;

var
  lSize: Integer;

begin
  for lSize in [16, 32, 48] do
    begin
    FreeAndNil(FImage);
    FImage := CreateSolidImage(lSize, lSize, RGB8(lSize, 0, 0));
    FWriter.AddImage(FImage);
    end;
  AssertEquals('Three images added', 3, FWriter.Count);
  FWriter.SaveToStream(FStream);
  FStream.Position := 0;
  FReader.LoadFromStream(FStream);
  AssertEquals('Three entries read', 3, FReader.EntryCount);
  AssertEquals('The width of the first entry', 16, FReader.Entries[0].Width);
  AssertEquals('The height of the last entry', 48, FReader.Entries[2].Height);
  AssertEquals('The depth of an entry', 32, FReader.Entries[1].BitCount);
  FRead := TFPMemoryImage.Create(0, 0);
  FReader.ReadEntry(FStream, 1, FRead);
  AssertEquals('Any entry can be read', 32, FRead.Width);
  AssertColorsEqual('with its own pixels', RGB8(32, 0, 0), FRead.Colors[5, 5]);
end;


procedure TTestICO.TestTheReaderTakesTheLargestEntry;

var
  lSize: Integer;

begin
  for lSize in [32, 64, 16] do
    begin
    FreeAndNil(FImage);
    FImage := CreateSolidImage(lSize, lSize, RGB8(lSize, 1, 2));
    FWriter.AddImage(FImage);
    end;
  FWriter.SaveToStream(FStream);
  FStream.Position := 0;
  ReadIt;
  AssertEquals('The largest entry is read', 64, FRead.Width);
  AssertColorsEqual('with its pixels', RGB8(64, 1, 2), FRead.Colors[0, 0]);
end;


procedure TTestICO.TestTheReaderPrefersTheDeepestOfOneSize;

var
  lMono, lTrue: TBytes;
  i: Integer;

begin
  lMono := Concat(DIBHeader(8, 2, 1), TBytes.Create(0, 0, 0, 0, 255, 255, 255, 0,
    $FF, 0, 0, 0, $FF, 0, 0, 0,  0, 0, 0, 0, 0, 0, 0, 0));
  lTrue := DIBHeader(8, 2, 32);
  SetLength(lTrue, 40 + 8 * 2 * 4 + 2 * 4);
  for i := 0 to 15 do
    begin
    lTrue[40 + i * 4] := 0;
    lTrue[40 + i * 4 + 1] := 0;
    lTrue[40 + i * 4 + 2] := 255;
    lTrue[40 + i * 4 + 3] := 255;
    end;
  for i := 40 + 64 to High(lTrue) do
    lTrue[i] := 0;
  AddRaw(lMono, 8, 2, 0);
  AddRaw(lTrue, 8, 2, 0);
  BuildFile(IcoTypeIcon);
  FReader.LoadFromStream(FStream);
  AssertEquals('The depth of an entry comes from its image data', 1, FReader.Entries[0].BitCount);
  AssertEquals('The deepest entry of a size is the best', 1, FReader.BestEntry);
  FStream.Position := 0;
  ReadIt;
  AssertColorsEqual('The 32-bit entry is read', colRed, FRead.Colors[3, 1]);
end;


procedure TTestICO.TestAPalettedEntryUsesItsMask;

begin
  { 8x2 pixels, 1 bit, palette black and white; rows bottom-up: colours $0F then $F0, masks $81 then 0 }
  AddRaw(Concat(DIBHeader(8, 2, 1), TBytes.Create(0, 0, 0, 0, 255, 255, 255, 0,
    $0F, 0, 0, 0, $F0, 0, 0, 0,  $81, 0, 0, 0, 0, 0, 0, 0)), 8, 2, 1);
  BuildFile(IcoTypeIcon);
  ReadIt;
  AssertEquals('The height is half that of the bitmap', 2, FRead.Height);
  AssertColorsEqual('A set bit is the second palette colour', colWhite, FRead.Colors[0, 0]);
  AssertColorsEqual('A clear bit is the first palette colour', colBlack, FRead.Colors[4, 0]);
  AssertColorsEqual('The bottom row is stored first', colBlack, FRead.Colors[1, 1]);
  AssertColorsEqual('and has its own colours', colWhite, FRead.Colors[4, 1]);
  AssertColorsEqual('A set mask bit makes the pixel transparent', colTransparent, FRead.Colors[0, 1]);
  AssertColorsEqual('at either end of the row', colTransparent, FRead.Colors[7, 1]);
end;


procedure TTestICO.TestA32BitEntryWithoutAlphaUsesItsMask;

begin
  AddRaw(Concat(DIBHeader(2, 1, 32), TBytes.Create(0, 0, 255, 0, 255, 0, 0, 0,  $40, 0, 0, 0)), 2, 1, 32);
  BuildFile(IcoTypeIcon);
  ReadIt;
  AssertColorsEqual('Without alpha a 32-bit pixel is opaque', colRed, FRead.Colors[0, 0]);
  AssertColorsEqual('and the mask makes pixels transparent', colTransparent, FRead.Colors[1, 0]);
end;


procedure TTestICO.TestA32BitEntryWithAlphaIgnoresItsMask;

begin
  AddRaw(Concat(DIBHeader(2, 1, 32), TBytes.Create(0, 0, 255, 128, 255, 0, 0, 255,  $C0, 0, 0, 0)), 2, 1, 32);
  BuildFile(IcoTypeIcon);
  ReadIt;
  AssertColorsEqual('The alpha of a 32-bit pixel is used', RGB8(255, 0, 0, 128), FRead.Colors[0, 0]);
  AssertColorsEqual('and the mask is ignored', colBlue, FRead.Colors[1, 0]);
end;


procedure TTestICO.TestReadingAfterOtherData;

begin
  FImage := CreateGradientImage(9, 5);
  FStream.WriteBuffer(PChar('prefix')^, 6);
  WriteImage(FImage, FWriter, FStream);
  FStream.Position := 6;
  ReadIt;
  AssertImagesEqual('The icon after the prefix reads back', FImage, FRead);
  AssertEquals('The reader stops at the end of the icon', FStream.Size, FStream.Position);
end;


procedure TTestICO.TestImageSizeIsThatOfTheLargestEntry;

var
  lSize: TPoint;

begin
  FImage := CreateGradientImage(24, 20);
  FWriter.AddImage(FImage);
  FreeAndNil(FImage);
  FImage := CreateGradientImage(40, 30);
  FWriter.AddImage(FImage);
  FWriter.SaveToStream(FStream);
  FStream.Position := 0;
  lSize := TFPReaderICO.ImageSize(FStream);
  AssertEquals('The width of the largest entry', 40, lSize.X);
  AssertEquals('The height of the largest entry', 30, lSize.Y);
  AssertEquals('The position is kept', 0, FStream.Position);
end;


procedure TTestICO.TestACursorKeepsItsHotSpot;

var
  lWriter: TFPWriterCUR;
  lReader: TFPReaderCUR;

begin
  FImage := CreateGradientImage(32, 32);
  FImage.Extra[IcoExtraHotSpotX] := '3';
  FImage.Extra[IcoExtraHotSpotY] := '17';
  lWriter := TFPWriterCUR.Create;
  lReader := TFPReaderCUR.Create;
  try
    FRead := RoundTrip(FImage, lWriter, lReader);
    WriteImage(FImage, lWriter, FStream);
  finally
    lReader.Free;
    lWriter.Free;
  end;
  AssertEquals('The type of a cursor', IcoTypeCursor, WordAt(2));
  AssertEquals('The hotspot x is written in the planes field', 3, WordAt(10));
  AssertEquals('The hotspot y is written in the bit count field', 17, WordAt(12));
  AssertEquals('The hotspot x is read', '3', FRead.Extra[IcoExtraHotSpotX]);
  AssertEquals('The hotspot y is read', '17', FRead.Extra[IcoExtraHotSpotY]);
  AssertImagesEqual('A cursor keeps its pixels', FImage, FRead);
end;


procedure TTestICO.TestIconsAndCursorsAreToldApart;

var
  lWriter: TFPWriterCUR;
  lReader: TFPReaderCUR;

begin
  FImage := CreateGradientImage(8, 8);
  lWriter := TFPWriterCUR.Create;
  lReader := TFPReaderCUR.Create;
  try
    WriteImage(FImage, lWriter, FStream);
    FStream.Position := 0;
    AssertFalse('The icon reader rejects a cursor', FReader.CheckContents(FStream));
    FStream.Clear;
    WriteImage(FImage, FWriter, FStream);
    FStream.Position := 0;
    AssertFalse('The cursor reader rejects an icon', lReader.CheckContents(FStream));
  finally
    lReader.Free;
    lWriter.Free;
  end;
end;


procedure TTestICO.TestContentsCheck;

begin
  FImage := CreateGradientImage(8, 8);
  WriteImage(FImage, FWriter, FStream);
  FStream.Position := 0;
  AssertTrue('An icon is accepted', FReader.CheckContents(FStream));
  FStream.Position := 4;
  FStream.WriteBuffer(PChar(#0#0)^, 2);
  FStream.Position := 0;
  AssertFalse('An icon without entries is rejected', FReader.CheckContents(FStream));
  FStream.Free;
  FStream := BytesStream([Ord('n'), Ord('o'), Ord('t'), 32, Ord('a'), 32, Ord('n'), Ord('i'), Ord('c'), Ord('o'), Ord('n')]);
  AssertFalse('Text is rejected', FReader.CheckContents(FStream));
end;


procedure TTestICO.TestAnEntryBeyondTheEndIsRejected;

begin
  FImage := CreateGradientImage(16, 16);
  FImage.SaveToStream(FStream, FWriter);
  FStream.Size := FStream.Size - 10;
  FStream.Position := 0;
  AssertFalse('A file whose entry goes beyond its end is rejected by the check', FReader.CheckContents(FStream));
  AssertRaises('Reading an entry beyond the end of the stream raises', FPImageException, @ReadTruncated);
end;


procedure TTestICO.TestTooLargeImagesRaise;

begin
  AssertRaises('An image wider than 256 pixels raises', FPImageException, @AddTooLarge);
end;


procedure TTestICO.TestSavingNothingRaises;

begin
  AssertRaises('Saving without images raises', FPImageException, @SaveNothing);
end;


procedure TTestICO.TestImageWriteKeepsTheImagesAdded;

var
  lOther: TFPMemoryImage;

begin
  FImage := CreateGradientImage(16, 16);
  FWriter.AddImage(FImage);
  lOther := CreateSolidImage(8, 8, colGreen);
  try
    lOther.SaveToStream(FStream, FWriter);
  finally
    lOther.Free;
  end;
  AssertEquals('ImageWrite writes one entry', 1, WordAt(4));
  AssertEquals('of the image written', 8, ByteAt(6));
  AssertEquals('The image added before stays', 1, FWriter.Count);
end;


procedure TTestICO.TestTheFormatsAreRegistered;

begin
  AssertEquals('The reader of .ico', 'TFPReaderICO', TFPCustomImage.FindReaderFromExtension('ico').ClassName);
  AssertEquals('The writer of .ico', 'TFPWriterICO', TFPCustomImage.FindWriterFromExtension('ico').ClassName);
  AssertEquals('The reader of .cur', 'TFPReaderCUR', TFPCustomImage.FindReaderFromExtension('cur').ClassName);
  AssertEquals('The writer of .cur', 'TFPWriterCUR', TFPCustomImage.FindWriterFromExtension('cur').ClassName);
end;


initialization
  RegisterTest('ico', TTestICO);
end.
