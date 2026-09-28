{
    Tests for the DDS reader: uncompressed pixels given by bit masks, the
    BC1, BC2 and BC3 block encodings, the DX10 header, and the surfaces as frames.
    See the file COPYING.FPC, included in this distribution, for details.
}
unit tcdds;

{$mode objfpc}{$H+}

interface

uses sysutils, classes, fpcunit, testregistry, fpimage, fpimgtests, fpimagelist, fpreaddds;

type
  TTestDDS = class(TTestCase)
  private
    FReader: TFPReaderDDS;
    FRead: TFPMemoryImage;
    FList: TFPImageList;
    FStream: TMemoryStream;
    // Replaces the stream with a DDS header of the given pixel format, followed by the bytes.
    procedure Build(aWidth, aHeight: Integer; aFlags, aFourCC, aBits, aRed, aGreen, aBlue, aAlpha: Cardinal;
      const aBytes: array of Byte);
    // Replaces the stream with a DDS file of the four-character code and the bytes.
    procedure BuildFourCC(aWidth, aHeight: Integer; const aFourCC: AnsiString; const aBytes: array of Byte);
    procedure ReadIt;
    // Sets the little-endian value at aOffset of the stream.
    procedure Patch(aOffset: Integer; aValue: Cardinal);
    // Reads all frames of the stream into FList.
    procedure ReadFrames;
    procedure ReadUnknownFourCC;
    procedure ReadTruncated;
    procedure ReadHugeBitCount;
    procedure CheckColor(const aMessage: String; aX, aY: Integer; const aColor: TFPColor);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestContentsCheck;
    procedure TestReadBGRA;
    procedure TestReadRGBWithoutAlpha;
    procedure TestRead565;
    procedure TestReadLuminance;
    procedure TestReadMaskWithAGap;
    procedure TestReadBC1FourColors;
    procedure TestReadBC1Transparent;
    procedure TestReadBC1PartialBlocks;
    procedure TestReadBC2;
    procedure TestReadBC3EightLevels;
    procedure TestReadBC3SixLevels;
    procedure TestReadPremultipliedBC2;
    procedure TestReadDX10;
    procedure TestUnknownFourCCRaises;
    procedure TestTruncatedRaises;
    procedure TestHugeBitCountRaises;
    procedure TestImageSize;
    procedure TestDDSIsRegistered;
    procedure TestMipmapsAreFrames;
    procedure TestMipLevelsStopAtOnePixel;
    procedure TestMissingSurfacesAreNotFrames;
    procedure TestCubeFacesAreFrames;
    procedure TestPartialCubeMap;
    procedure TestDX10ArrayElementsAreFrames;
    procedure TestVolumeSlicesAreFrames;
    procedure TestSingleImageIsTheFirstSurface;
    procedure TestEndFramesIsAfterTheSurfaces;
  end;

implementation

const
  // A BC1 colour block: red and blue end points, row 0 with the indices 0, 1, 2, 3, the other rows 0.
  BC1RedBlue: array[0..7] of Byte = ($00, $F8, $1F, $00, $E4, $00, $00, $00);

procedure TTestDDS.SetUp;

begin
  inherited SetUp;
  FReader := TFPReaderDDS.Create;
  FStream := TMemoryStream.Create;
end;


procedure TTestDDS.TearDown;

begin
  FreeAndNil(FStream);
  FreeAndNil(FList);
  FreeAndNil(FRead);
  FreeAndNil(FReader);
  inherited TearDown;
end;


procedure TTestDDS.Build(aWidth, aHeight: Integer; aFlags, aFourCC, aBits, aRed, aGreen, aBlue, aAlpha: Cardinal;
  const aBytes: array of Byte);

var
  lMagic: Cardinal;
  lHeader: TDDSHeader;

begin
  FStream.Clear;
  lMagic := NtoLE(Cardinal(DDSMagic));
  FStream.WriteBuffer(lMagic, 4);
  FillChar(lHeader, SizeOf(lHeader), 0);
  lHeader.Size := NtoLE(Cardinal(DDSHeaderSize));
  lHeader.Flags := NtoLE(Cardinal($1007));
  lHeader.Width := NtoLE(Cardinal(aWidth));
  lHeader.Height := NtoLE(Cardinal(aHeight));
  lHeader.PixelFormat.Size := NtoLE(Cardinal(DDSPixelFormatSize));
  lHeader.PixelFormat.Flags := NtoLE(aFlags);
  lHeader.PixelFormat.FourCC := NtoLE(aFourCC);
  lHeader.PixelFormat.RGBBitCount := NtoLE(aBits);
  lHeader.PixelFormat.RBitMask := NtoLE(aRed);
  lHeader.PixelFormat.GBitMask := NtoLE(aGreen);
  lHeader.PixelFormat.BBitMask := NtoLE(aBlue);
  lHeader.PixelFormat.ABitMask := NtoLE(aAlpha);
  lHeader.Caps := NtoLE(Cardinal($1000));
  FStream.WriteBuffer(lHeader, SizeOf(lHeader));
  if Length(aBytes) > 0 then
    FStream.WriteBuffer(aBytes[0], Length(aBytes));
  FStream.Position := 0;
end;


procedure TTestDDS.BuildFourCC(aWidth, aHeight: Integer; const aFourCC: AnsiString; const aBytes: array of Byte);

begin
  Build(aWidth, aHeight, DDPF_FOURCC, DDSFourCC(aFourCC), 0, 0, 0, 0, 0, aBytes);
end;


procedure TTestDDS.ReadIt;

begin
  FreeAndNil(FRead);
  FRead := TFPMemoryImage.Create(0, 0);
  FRead.LoadFromStream(FStream, FReader);
end;


procedure TTestDDS.Patch(aOffset: Integer; aValue: Cardinal);

begin
  aValue := NtoLE(aValue);
  Move(aValue, PByte(FStream.Memory)[aOffset], 4);
end;


procedure TTestDDS.ReadFrames;

begin
  FreeAndNil(FList);
  FList := TFPImageList.Create;
  FList.LoadFromStream(FStream, FReader);
end;


// Returns a BC1 block of one 5:6:5 colour.
function SolidBC1(aColor: Word): TBytes;

begin
  Result := TBytes.Create(Lo(aColor), Hi(aColor), Lo(aColor), Hi(aColor), 0, 0, 0, 0);
end;


procedure TTestDDS.ReadUnknownFourCC;

begin
  BuildFourCC(4, 4, 'ATI2', [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]);
  ReadIt;
end;


procedure TTestDDS.ReadTruncated;

begin
  BuildFourCC(8, 4, 'DXT1', BC1RedBlue);
  ReadIt;
end;


procedure TTestDDS.ReadHugeBitCount;

begin
  Build(1, 1, DDPF_RGB, 0, $FFFFFFFF, $FF, $FF00, $FF0000, 0, [0, 0, 0, 0]);
  ReadIt;
end;


procedure TTestDDS.CheckColor(const aMessage: String; aX, aY: Integer; const aColor: TFPColor);

begin
  AssertColorsEqual(aMessage, aColor, FRead[aX, aY]);
end;


procedure TTestDDS.TestContentsCheck;

begin
  BuildFourCC(4, 4, 'DXT1', BC1RedBlue);
  AssertTrue('A DDS file is accepted', FReader.CheckContents(FStream));
  AssertEquals('The check leaves the position', 0, FStream.Position);
  FStream.Clear;
  FStream.WriteBuffer(PChar('DDS x')^, 5);
  FStream.Position := 0;
  AssertFalse('A header of another size is rejected', FReader.CheckContents(FStream));
end;


procedure TTestDDS.TestReadBGRA;

begin
  Build(2, 1, DDPF_RGB or DDPF_ALPHAPIXELS, 0, 32, $FF0000, $FF00, $FF, $FF000000,
        [30, 20, 10, 255, 0, 0, 255, 128]);
  ReadIt;
  CheckColor('Bytes in blue, green, red, alpha order', 0, 0, RGB8(10, 20, 30));
  CheckColor('The alpha mask', 1, 0, RGB8(255, 0, 0, 128));
end;


procedure TTestDDS.TestReadRGBWithoutAlpha;

begin
  Build(1, 2, DDPF_RGB, 0, 24, $FF, $FF00, $FF0000, 0, [1, 2, 3, 4, 5, 6]);
  ReadIt;
  CheckColor('24-bit pixels', 0, 0, RGB8(1, 2, 3));
  CheckColor('The second row', 0, 1, RGB8(4, 5, 6));
end;


procedure TTestDDS.TestRead565;

begin
  Build(3, 1, DDPF_RGB, 0, 16, $F800, $07E0, $001F, 0, [$00, $F8, $E0, $07, $10, $00]);
  ReadIt;
  CheckColor('The red mask', 0, 0, colRed);
  CheckColor('The green mask', 1, 0, colGreen);
  CheckColor('A blue of 16 out of 31', 2, 0, FPColor(0, 0, 33825));
end;


procedure TTestDDS.TestReadLuminance;

begin
  Build(2, 1, DDPF_LUMINANCE, 0, 8, $FF, 0, 0, 0, [0, 200]);
  ReadIt;
  CheckColor('A luminance of 0', 0, 0, colBlack);
  CheckColor('A luminance of 200', 1, 0, RGB8(200, 200, 200));
end;


procedure TTestDDS.TestReadMaskWithAGap;

begin
  Build(2, 1, DDPF_LUMINANCE, 0, 8, $81, 0, 0, 0, [$81, $80]);
  ReadIt;
  CheckColor('All bits of a mask with a gap are the maximum', 0, 0, colWhite);
  CheckColor('The value is taken under the mask, shifted down', 1, 0, FPColor(65027, 65027, 65027));
end;


procedure TTestDDS.TestReadBC1FourColors;

begin
  BuildFourCC(4, 4, 'DXT1', BC1RedBlue);
  ReadIt;
  CheckColor('Index 0 is the first end point', 0, 0, colRed);
  CheckColor('Index 1 is the second end point', 1, 0, colBlue);
  CheckColor('Index 2 is two thirds of the first', 2, 0, RGB8(170, 0, 85));
  CheckColor('Index 3 is two thirds of the second', 3, 0, RGB8(85, 0, 170));
  CheckColor('Other rows use index 0', 3, 3, colRed);
end;


procedure TTestDDS.TestReadBC1Transparent;

begin
  { the first end point is not above the second: three colours and transparent black }
  BuildFourCC(4, 4, 'DXT1', [$1F, $00, $00, $F8, $E4, $00, $00, $00]);
  ReadIt;
  CheckColor('Index 0', 0, 0, colBlue);
  CheckColor('Index 2 is the average', 2, 0, RGB8(127, 0, 127));
  CheckColor('Index 3 is transparent black', 3, 0, colTransparent);
end;


procedure TTestDDS.TestReadBC1PartialBlocks;

begin
  BuildFourCC(5, 3, 'DXT1', [$00, $F8, $00, $F8, $00, $00, $00, $00,
                              $1F, $00, $1F, $00, $00, $00, $00, $00]);
  ReadIt;
  AssertEquals('The width is not a multiple of 4', 5, FRead.Width);
  CheckColor('The first block', 3, 2, colRed);
  CheckColor('The second block starts at 4', 4, 0, colBlue);
end;


procedure TTestDDS.TestReadBC2;

begin
  BuildFourCC(4, 4, 'DXT3', [$F0, $00, $00, $00, $00, $00, $00, $80,
                              $00, $F8, $1F, $00, $00, $00, $00, $00]);
  ReadIt;
  CheckColor('The low nibble is the first pixel', 0, 0, RGB8(255, 0, 0, 0));
  CheckColor('The high nibble is the second pixel', 1, 0, RGB8(255, 0, 0, 255));
  CheckColor('A nibble of 8 is an alpha of 136', 3, 3, RGB8(255, 0, 0, 136));
end;


procedure TTestDDS.TestReadBC3EightLevels;

begin
  { alpha end points 255 and 0; the indices of row 0 are 0, 1, 2, 7 }
  BuildFourCC(4, 4, 'DXT5', [255, 0, $88, $0E, $00, $00, $00, $00,
                              $00, $F8, $1F, $00, $00, $00, $00, $00]);
  ReadIt;
  CheckColor('Alpha index 0 is the first end point', 0, 0, RGB8(255, 0, 0, 255));
  CheckColor('Alpha index 1 is the second end point', 1, 0, RGB8(255, 0, 0, 0));
  CheckColor('Alpha index 2 is six sevenths of the first', 2, 0, RGB8(255, 0, 0, 218));
  CheckColor('Alpha index 7 is one seventh of the first', 3, 0, RGB8(255, 0, 0, 36));
end;


procedure TTestDDS.TestReadBC3SixLevels;

begin
  { alpha end points 0 and 250; the indices of row 0 are 2, 5, 6, 7 }
  BuildFourCC(4, 4, 'DXT5', [0, 250, $AA, $0F, $00, $00, $00, $00,
                              $00, $F8, $1F, $00, $00, $00, $00, $00]);
  ReadIt;
  CheckColor('Alpha index 2 is one fifth of the second', 0, 0, RGB8(255, 0, 0, 50));
  CheckColor('Alpha index 5 is four fifths of the second', 1, 0, RGB8(255, 0, 0, 200));
  CheckColor('Alpha index 6 is 0', 2, 0, RGB8(255, 0, 0, 0));
  CheckColor('Alpha index 7 is 255', 3, 0, RGB8(255, 0, 0, 255));
end;


procedure TTestDDS.TestReadPremultipliedBC2;

begin
  { a red of 66 with an alpha of 136 }
  BuildFourCC(4, 4, 'DXT2', [$88, $88, $88, $88, $88, $88, $88, $88,
                              $00, $40, $00, $40, $00, $00, $00, $00]);
  ReadIt;
  CheckColor('The colour is divided by the alpha', 0, 0, RGB8(124, 0, 0, 136));
end;


procedure TTestDDS.TestReadDX10;

var
  lDX10: TDDSHeaderDX10;

begin
  BuildFourCC(4, 4, 'DX10', []);
  FillChar(lDX10, SizeOf(lDX10), 0);
  lDX10.DXGIFormat := NtoLE(Cardinal(DXGI_FORMAT_BC1_UNORM));
  lDX10.ResourceDimension := NtoLE(Cardinal(3));
  lDX10.ArraySize := NtoLE(Cardinal(1));
  FStream.Position := FStream.Size;
  FStream.WriteBuffer(lDX10, SizeOf(lDX10));
  FStream.WriteBuffer(BC1RedBlue, SizeOf(BC1RedBlue));
  FStream.Position := 0;
  ReadIt;
  AssertTrue('The DXGI format BC1 is read as BC1', FReader.Encoding = deBC1);
  CheckColor('Index 2 of the block', 2, 0, RGB8(170, 0, 85));
end;


procedure TTestDDS.TestUnknownFourCCRaises;

begin
  AssertRaises('An unsupported four-character code raises', FPImageException, @ReadUnknownFourCC);
end;


procedure TTestDDS.TestTruncatedRaises;

begin
  AssertRaises('Missing blocks raise', Exception, @ReadTruncated);
end;


procedure TTestDDS.TestHugeBitCountRaises;

begin
  AssertRaises('A bit count above the range of Integer raises', FPImageException, @ReadHugeBitCount);
end;


procedure TTestDDS.TestImageSize;

var
  lSize: TPoint;

begin
  BuildFourCC(12, 8, 'DXT1', []);
  lSize := TFPReaderDDS.ImageSize(FStream);
  AssertEquals('The width from the header', 12, lSize.X);
  AssertEquals('The height from the header', 8, lSize.Y);
end;


procedure TTestDDS.TestDDSIsRegistered;

begin
  AssertTrue('DDS has a reader', ImageHandlers.ImageReader['DirectDraw Surface'] = TFPReaderDDS);
end;


procedure TTestDDS.TestMipmapsAreFrames;

begin
  BuildFourCC(8, 4, 'DXT1', Concat(SolidBC1($F800), SolidBC1($F800), SolidBC1($07E0), SolidBC1($001F), SolidBC1($FFFF)));
  Patch(28, 4);
  ReadFrames;
  AssertEquals('One frame for each mip level', 4, FList.Count);
  AssertEquals('The frame count of the file', 4, FList.Info.FrameCount);
  AssertEquals('Level 1 is half as wide', 4, FList.Images[1].Width);
  AssertEquals('and half as high', 2, FList.Images[1].Height);
  AssertEquals('Level 3 is one pixel', 1, FList.Images[3].Width);
  AssertColorsEqual('Level 0', colRed, FList.Images[0].Colors[7, 3]);
  AssertColorsEqual('Level 1', colGreen, FList.Images[1].Colors[3, 1]);
  AssertColorsEqual('Level 2', colBlue, FList.Images[2].Colors[1, 0]);
  AssertColorsEqual('Level 3', colWhite, FList.Images[3].Colors[0, 0]);
  AssertEquals('The name of a level', 'mip 2', FList[2].Info.Name);
  AssertTrue('Level 0 is a page', FList[0].Info.Kind = fkPage);
  AssertTrue('Smaller levels are variants', FList[1].Info.Kind = fkVariant);
end;


procedure TTestDDS.TestMipLevelsStopAtOnePixel;

begin
  BuildFourCC(4, 4, 'DXT1', Concat(SolidBC1($F800), SolidBC1($F800), SolidBC1($F800), SolidBC1($F800)));
  Patch(28, 10);
  ReadFrames;
  AssertEquals('A 4x4 image has 3 mip levels, whatever the header says', 3, FList.Count);
end;


procedure TTestDDS.TestMissingSurfacesAreNotFrames;

begin
  BuildFourCC(8, 4, 'DXT1', Concat(SolidBC1($F800), SolidBC1($F800), SolidBC1($07E0)));
  Patch(28, 4);
  ReadFrames;
  AssertEquals('Only the levels in the file are frames', 2, FList.Count);
end;


procedure TTestDDS.TestCubeFacesAreFrames;

var
  I: Integer;

begin
  Build(1, 1, DDPF_RGB, 0, 32, $FF0000, $FF00, $FF, 0,
        [0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0]);
  Patch(112, $FE00);
  ReadFrames;
  AssertEquals('Six faces', 6, FList.Count);
  for I := 0 to 5 do
    AssertColorsEqual('Face ' + IntToStr(I), RGB8(I + 1, 0, 0), FList.Images[I].Colors[0, 0]);
  AssertEquals('The name of a face', 'face -Y', FList[3].Info.Name);
  AssertEquals('The surface of a face', 3, FReader.Surfaces[3].Element);
end;


procedure TTestDDS.TestPartialCubeMap;

begin
  Build(1, 1, DDPF_RGB, 0, 32, $FF0000, $FF00, $FF, 0, [0, 0, 1, 0, 0, 0, 2, 0]);
  Patch(112, DDSCAPS2_CUBEMAP or $400 or $8000);
  ReadFrames;
  AssertEquals('Only the faces of the header', 2, FList.Count);
  AssertEquals('The second face is -Z', 'face -Z', FList[1].Info.Name);
  AssertEquals('with the element of -Z', 5, FReader.Surfaces[1].Element);
end;


procedure TTestDDS.TestDX10ArrayElementsAreFrames;

var
  lDX10: TDDSHeaderDX10;
  lPixels: array[0..7] of Byte = (10, 0, 0, 255, 20, 0, 0, 255);

begin
  BuildFourCC(1, 1, 'DX10', []);
  FillChar(lDX10, SizeOf(lDX10), 0);
  lDX10.DXGIFormat := NtoLE(Cardinal(DXGI_FORMAT_R8G8B8A8_UNORM));
  lDX10.ResourceDimension := NtoLE(Cardinal(3));
  lDX10.ArraySize := NtoLE(Cardinal(2));
  FStream.Position := FStream.Size;
  FStream.WriteBuffer(lDX10, SizeOf(lDX10));
  FStream.WriteBuffer(lPixels, SizeOf(lPixels));
  FStream.Position := 0;
  ReadFrames;
  AssertEquals('Two array elements', 2, FList.Count);
  AssertEquals('The name of an element', 'element 1', FList[1].Info.Name);
  AssertColorsEqual('The second element', RGB8(20, 0, 0), FList.Images[1].Colors[0, 0]);
end;


procedure TTestDDS.TestVolumeSlicesAreFrames;

var
  lPixels: TBytes;
  I: Integer;

begin
  SetLength(lPixels, 2 * 2 * 2 * 4 + 4);
  for I := 0 to High(lPixels) div 4 do
    lPixels[4 * I + 2] := I;
  Build(2, 2, DDPF_RGB, 0, 32, $FF0000, $FF00, $FF, 0, lPixels);
  Patch(24, 2);
  Patch(28, 2);
  Patch(112, DDSCAPS2_VOLUME);
  ReadFrames;
  AssertEquals('Two slices of level 0 and one of level 1', 3, FList.Count);
  AssertEquals('The name of a slice', 'slice 1, mip 0', FList[1].Info.Name);
  AssertColorsEqual('The second slice', RGB8(4, 0, 0), FList.Images[1].Colors[0, 0]);
  AssertColorsEqual('The slice of level 1', RGB8(8, 0, 0), FList.Images[2].Colors[0, 0]);
end;


procedure TTestDDS.TestSingleImageIsTheFirstSurface;

begin
  Build(1, 1, DDPF_RGB, 0, 32, $FF0000, $FF00, $FF, 0, [0, 0, 1, 0, 0, 0, 2, 0]);
  Patch(112, DDSCAPS2_CUBEMAP or $400 or $800);
  ReadIt;
  CheckColor('A single image is the first face', 0, 0, RGB8(1, 0, 0));
end;


procedure TTestDDS.TestEndFramesIsAfterTheSurfaces;

begin
  BuildFourCC(8, 4, 'DXT1', Concat(SolidBC1($F800), SolidBC1($F800), SolidBC1($07E0), SolidBC1($001F), SolidBC1($FFFF)));
  Patch(28, 4);
  FStream.Position := FStream.Size;
  FStream.WriteBuffer(PChar('tail')^, 4);
  FStream.Position := 0;
  ReadFrames;
  AssertEquals('The stream is left after the last surface', FStream.Size - 4, FStream.Position);
end;


initialization
  RegisterTest('dds', TTestDDS);
end.
