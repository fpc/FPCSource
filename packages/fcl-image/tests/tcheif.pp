{
    Tests for the HEIF reader and writer through libheif: HEIC and AVIF,
    lossy and lossless, alpha, metadata, several top-level images and errors.
    See the file COPYING.FPC, included in this distribution, for details.
}
unit tcheif;

{$mode objfpc}{$H+}

interface

uses sysutils, classes, math, fpcunit, testregistry, fpimage, fpimgtests, fpimagelist, fpimgexif,
     libheif, heifcomn, fpreadheif, fpwriteheif, fpwritepng;

type
  TTestHEIF = class(TTestCase)
  private
    FReader: TFPReaderHEIF;
    FImage: TFPMemoryImage;
    FRead: TFPMemoryImage;
    FList: TFPImageList;
    FStream: TMemoryStream;
    // Replaces FStream with FImage written by aWriter, and frees aWriter.
    procedure SaveWith(aWriter: TFPCustomImageWriter);
    // Writes FImage to FStream with aWriter, frees aWriter and reads FStream into FRead.
    procedure RoundTrip(aWriter: TFPWriterHEIF);
    procedure ReadIt;
    procedure LoadHex(const aHex: String);
    // Returns the four characters of the major brand of the file in FStream.
    function MajorBrand: String;
    // Returns True when the primary image of the file in FStream has an alpha channel.
    function FileHasAlpha: Boolean;
    procedure RequireEncoder(aCompression: THEIFCompression);
    procedure CheckBlocks(const aWhat: String);
    procedure ReadTruncated;
    procedure ReadPNG;
    procedure WriteEmpty;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestContentsCheck;
    procedure TestLossyRoundTripIsClose;
    procedure TestQualityChangesTheSize;
    procedure TestLosslessIsWithinOne;
    procedure TestHEICBrand;
    procedure TestAlphaIsKept;
    procedure TestOpaqueImageHasNoAlpha;
    procedure TestUseAlphaOff;
    procedure TestAVIFRoundTrip;
    procedure TestAVIFBrand;
    procedure TestSmallAVIFIsReadAtItsSizeOrRefused;
    procedure TestReadImageMagickHEIC;
    procedure TestReadImageMagickAVIF;
    procedure TestExifIsKept;
    procedure TestExifOrientationIsKeptWithoutTransformations;
    procedure TestXMPIsKept;
    procedure TestICCIsKept;
    procedure TestFramesAreTopLevelImages;
    procedure TestSingleReadIsThePrimaryImage;
    procedure TestTruncatedFileRaises;
    procedure TestOtherFormatRaises;
    procedure TestEmptyImageRaises;
    procedure TestHandlersAreRegistered;
  end;

implementation

{$i heiffixtures.inc}

const
  XMPPacket = '<x:xmpmeta xmlns:x="adobe:ns:meta/"><rdf:RDF/></x:xmpmeta>';
  // Largest difference of a lossy round trip of smooth ramps, on the 16-bit scale.
  LossyTolerance = 8 * 257;

procedure TTestHEIF.SetUp;

begin
  inherited SetUp;
  FReader := TFPReaderHEIF.Create;
  FStream := TMemoryStream.Create;
end;


procedure TTestHEIF.TearDown;

begin
  FreeAndNil(FStream);
  FreeAndNil(FList);
  FreeAndNil(FRead);
  FreeAndNil(FImage);
  FreeAndNil(FReader);
  inherited TearDown;
end;


procedure TTestHEIF.SaveWith(aWriter: TFPCustomImageWriter);

begin
  try
    FStream.Clear;
    FImage.SaveToStream(FStream, aWriter);
  finally
    aWriter.Free;
  end;
  FStream.Position := 0;
end;


procedure TTestHEIF.RoundTrip(aWriter: TFPWriterHEIF);

begin
  SaveWith(aWriter);
  ReadIt;
end;


procedure TTestHEIF.ReadIt;

begin
  FreeAndNil(FRead);
  FRead := TFPMemoryImage.Create(0, 0);
  FStream.Position := 0;
  FRead.LoadFromStream(FStream, FReader);
end;


procedure TTestHEIF.LoadHex(const aHex: String);

var
  i: Integer;
  lByte: Byte;

begin
  FStream.Clear;
  for i := 0 to Length(aHex) div 2 - 1 do
    begin
    lByte := StrToInt('$' + Copy(aHex, i * 2 + 1, 2));
    FStream.WriteBuffer(lByte, 1);
    end;
  FStream.Position := 0;
end;


function TTestHEIF.MajorBrand: String;

begin
  SetString(Result, PChar(FStream.Memory) + 8, 4);
end;


function TTestHEIF.FileHasAlpha: Boolean;

var
  lContext: Pheif_context;
  lHandle: Pheif_image_handle;
  lMask: TFPUExceptionMask;

begin
  lMask := HEIFEnterLibrary;
  lContext := heif_context_alloc();
  try
    CheckHEIF(heif_context_read_from_memory(lContext, FStream.Memory, FStream.Size, nil));
    CheckHEIF(heif_context_get_primary_image_handle(lContext, @lHandle));
    Result := heif_image_handle_has_alpha_channel(lHandle) <> 0;
    heif_image_handle_release(lHandle);
  finally
    heif_context_free(lContext);
    HEIFLeaveLibrary(lMask);
  end;
end;


procedure TTestHEIF.RequireEncoder(aCompression: THEIFCompression);

begin
  if not HEIFCanEncode(aCompression) then
    Ignore('libheif has no ' + HEIFCompressionNames[aCompression] + ' encoder');
end;


procedure TTestHEIF.CheckBlocks(const aWhat: String);

begin
  AssertEquals(aWhat + ': the width', 32, FRead.Width);
  AssertEquals(aWhat + ': the height', 16, FRead.Height);
  AssertColorsEqual(aWhat + ': the red block', colRed, FRead.Colors[8, 4], 3 * 257);
  AssertColorsEqual(aWhat + ': the green block', colGreen, FRead.Colors[24, 4], 3 * 257);
  AssertColorsEqual(aWhat + ': the blue block', colBlue, FRead.Colors[8, 12], 3 * 257);
  AssertColorsEqual(aWhat + ': the white block', colWhite, FRead.Colors[24, 12], 3 * 257);
end;


procedure TTestHEIF.ReadTruncated;

begin
  FImage := CreateGradientImage(40, 30);
  SaveWith(TFPWriterHEIF.Create);
  FStream.Size := FStream.Size div 2;
  ReadIt;
end;


procedure TTestHEIF.ReadPNG;

begin
  FImage := CreateGradientImage(8, 8);
  SaveWith(TFPWriterPNG.Create);
  ReadIt;
end;


procedure TTestHEIF.WriteEmpty;

begin
  FImage := TFPMemoryImage.Create(0, 0);
  SaveWith(TFPWriterHEIF.Create);
end;


// Returns an image of smooth ramps, which lossy compression keeps close; with aAlpha the alpha is a ramp too.
function SmoothImage(aWidth, aHeight: Integer; aAlpha: Boolean): TFPMemoryImage;

var
  lX, lY: Integer;
  lColor: TFPColor;

begin
  Result := TFPMemoryImage.Create(aWidth, aHeight);
  for lY := 0 to aHeight - 1 do
    for lX := 0 to aWidth - 1 do
      begin
      lColor := RGB8(lX * 255 div (aWidth - 1), lY * 255 div (aHeight - 1), 64 + (lX + lY) * 64 div (aWidth + aHeight));
      if aAlpha then
        lColor.Alpha := (32 + lX * 223 div (aWidth - 1)) * 257;
      Result.Colors[lX, lY] := lColor;
      end;
end;


// Returns EXIF data of one orientation tag, little-endian, starting with its TIFF header.
function ExifWithOrientation(aOrientation: Word): TBytes;

begin
  Result := TBytes.Create(Ord('I'), Ord('I'), 42, 0, 8, 0, 0, 0,
                          1, 0,
                          $12, $01, 3, 0, 1, 0, 0, 0, Lo(aOrientation), Hi(aOrientation), 0, 0,
                          0, 0, 0, 0);
end;


procedure TTestHEIF.TestContentsCheck;

begin
  RequireEncoder(hcHEVC);
  FImage := CreateGradientImage(16, 16);
  SaveWith(TFPWriterHEIF.Create);
  AssertTrue('A HEIC file is accepted', FReader.CheckContents(FStream));
  AssertEquals('The check leaves the position', 0, FStream.Position);
  SaveWith(TFPWriterPNG.Create);
  AssertFalse('A PNG file is rejected', FReader.CheckContents(FStream));
  FStream.Size := 8;
  FStream.Position := 0;
  AssertFalse('8 bytes are rejected', FReader.CheckContents(FStream));
end;


procedure TTestHEIF.TestLossyRoundTripIsClose;

begin
  RequireEncoder(hcHEVC);
  FImage := SmoothImage(64, 48, False);
  RoundTrip(TFPWriterHEIF.Create);
  AssertImagesEqual('A quality of 90 keeps smooth ramps close', FImage, FRead, LossyTolerance);
  AssertTrue('with a PSNR above 40 dB', ImagePSNR(FImage, FRead) > 40);
end;


procedure TTestHEIF.TestQualityChangesTheSize;

var
  lWriter: TFPWriterHEIF;
  lLow: Int64;

begin
  RequireEncoder(hcHEVC);
  FImage := CreateGradientImage(64, 48);
  lWriter := TFPWriterHEIF.Create;
  lWriter.Quality := 20;
  RoundTrip(lWriter);
  lLow := FStream.Size;
  lWriter := TFPWriterHEIF.Create;
  lWriter.Quality := 95;
  RoundTrip(lWriter);
  AssertTrue('A quality of 20 makes a smaller file than 95', lLow < FStream.Size);
end;


procedure TTestHEIF.TestLosslessIsWithinOne;

var
  lWriter: TFPWriterHEIF;

begin
  RequireEncoder(hcHEVC);
  FImage := CreateGradientImage(64, 48);
  lWriter := TFPWriterHEIF.Create;
  lWriter.Lossless := True;
  RoundTrip(lWriter);
  AssertImagesEqual('Lossless compression changes a component by 1 at most', FImage, FRead, 257);
end;


procedure TTestHEIF.TestHEICBrand;

begin
  RequireEncoder(hcHEVC);
  FImage := CreateGradientImage(16, 16);
  RoundTrip(TFPWriterHEIF.Create);
  AssertEquals('A file of HEVC images has the brand heic', 'heic', MajorBrand);
end;


procedure TTestHEIF.TestAlphaIsKept;

begin
  RequireEncoder(hcHEVC);
  FImage := SmoothImage(64, 48, True);
  RoundTrip(TFPWriterHEIF.Create);
  AssertTrue('An image that is not opaque has an alpha channel', FileHasAlpha);
  AssertImagesEqual('The alpha is kept', FImage, FRead, LossyTolerance);
end;


procedure TTestHEIF.TestOpaqueImageHasNoAlpha;

begin
  RequireEncoder(hcHEVC);
  FImage := CreateGradientImage(32, 24);
  RoundTrip(TFPWriterHEIF.Create);
  AssertFalse('An opaque image has no alpha channel', FileHasAlpha);
  AssertEquals('and reads as opaque', AlphaOpaque, FRead.Colors[5, 5].Alpha);
end;


procedure TTestHEIF.TestUseAlphaOff;

var
  lWriter: TFPWriterHEIF;

begin
  RequireEncoder(hcHEVC);
  FImage := CreateAlphaImage(32, 24);
  lWriter := TFPWriterHEIF.Create;
  lWriter.UseAlpha := False;
  RoundTrip(lWriter);
  AssertFalse('UseAlpha off writes no alpha channel', FileHasAlpha);
  AssertEquals('and the pixels read are opaque', AlphaOpaque, FRead.Colors[0, 0].Alpha);
end;


procedure TTestHEIF.TestAVIFRoundTrip;

begin
  RequireEncoder(hcAV1);
  FImage := SmoothImage(64, 48, True);
  RoundTrip(TFPWriterAVIF.Create);
  AssertImagesEqual('An AVIF file keeps the image close', FImage, FRead, LossyTolerance);
end;


procedure TTestHEIF.TestAVIFBrand;

begin
  RequireEncoder(hcAV1);
  FImage := CreateGradientImage(16, 16);
  RoundTrip(TFPWriterAVIF.Create);
  AssertEquals('A file of AV1 images has the brand avif', 'avif', MajorBrand);
end;


procedure TTestHEIF.TestSmallAVIFIsReadAtItsSizeOrRefused;

var
  lWritten: Boolean;

begin
  RequireEncoder(hcAV1);
  FImage := SmoothImage(8, 6, False);
  try
    SaveWith(TFPWriterAVIF.Create);
    lWritten := True;
  except
    on E: FPImageException do
      begin
      lWritten := False;
      AssertTrue('A refused image names the size it decodes as: ' + E.Message, Pos('decodes as', E.Message) > 0);
      end;
  end;
  if lWritten then
    begin
    ReadIt;
    AssertEquals('An AVIF image of 8x6 that is written reads back 8 wide', 8, FRead.Width);
    AssertEquals('and 6 high', 6, FRead.Height);
    end;
end;


procedure TTestHEIF.TestReadImageMagickHEIC;

begin
  LoadHex(FixtureHEIC);
  AssertTrue('A HEIC file of ImageMagick is accepted', FReader.CheckContents(FStream));
  ReadIt;
  CheckBlocks('HEIC of ImageMagick');
end;


procedure TTestHEIF.TestReadImageMagickAVIF;

begin
  if not HEIFCanDecode(hcAV1) then
    Ignore('libheif has no AV1 decoder');
  LoadHex(FixtureAVIF);
  ReadIt;
  CheckBlocks('AVIF of ImageMagick');
end;


procedure TTestHEIF.TestExifIsKept;

begin
  RequireEncoder(hcHEVC);
  FImage := CreateGradientImage(16, 16);
  FImage.Metadata[MetaExif] := ExifWithOrientation(6);
  RoundTrip(TFPWriterHEIF.Create);
  AssertEquals('The EXIF data is read without its offset', 26, Length(FRead.Metadata[MetaExif]));
  AssertEquals('and starts with its TIFF header', Ord('I'), FRead.Metadata[MetaExif][0]);
  AssertEquals('Its orientation is 1, as the image is decoded upright', 1, ExifOrientation(FRead.Metadata[MetaExif]));
end;


procedure TTestHEIF.TestExifOrientationIsKeptWithoutTransformations;

begin
  RequireEncoder(hcHEVC);
  FImage := CreateGradientImage(16, 16);
  FImage.Metadata[MetaExif] := ExifWithOrientation(6);
  FReader.ApplyTransformations := False;
  RoundTrip(TFPWriterHEIF.Create);
  AssertEquals('With ApplyTransformations off the orientation is kept', 6, ExifOrientation(FRead.Metadata[MetaExif]));
end;


procedure TTestHEIF.TestXMPIsKept;

begin
  RequireEncoder(hcHEVC);
  FImage := CreateGradientImage(16, 16);
  FImage.Metadata[MetaXMP] := TEncoding.UTF8.GetBytes(XMPPacket);
  RoundTrip(TFPWriterHEIF.Create);
  AssertEquals('The XMP packet is kept', XMPPacket, TEncoding.UTF8.GetString(FRead.Metadata[MetaXMP]));
end;


procedure TTestHEIF.TestICCIsKept;

var
  lProfile: TBytes;
  i: Integer;

begin
  RequireEncoder(hcHEVC);
  SetLength(lProfile, 700);
  for i := 0 to High(lProfile) do
    lProfile[i] := i mod 251;
  FImage := CreateGradientImage(16, 16);
  FImage.Metadata[MetaICC] := lProfile;
  RoundTrip(TFPWriterHEIF.Create);
  AssertEquals('The ICC profile has its length', Length(lProfile), Length(FRead.Metadata[MetaICC]));
  AssertTrue('and its bytes', CompareMem(@lProfile[0], @FRead.Metadata[MetaICC][0], Length(lProfile)));
end;


procedure TTestHEIF.TestFramesAreTopLevelImages;

var
  lWriter: TFPWriterHEIF;

begin
  RequireEncoder(hcHEVC);
  FList := TFPImageList.Create;
  FList.Add(CreateGradientImage(40, 30));
  FList.Add(CreateSolidImage(16, 8, colRed));
  FList.Add(CreateAlphaImage(24, 24));
  lWriter := TFPWriterHEIF.Create;
  try
    FList.SaveToStream(FStream, lWriter);
  finally
    lWriter.Free;
  end;
  FreeAndNil(FList);
  FStream.Position := 0;
  FList := TFPImageList.Create;
  FList.LoadFromStream(FStream, FReader);
  AssertEquals('One frame for each image', 3, FList.Count);
  AssertEquals('The frame count of the file', 3, FList.Info.FrameCount);
  AssertEquals('The size of the file is that of the primary image', 40, FList.Info.Width);
  AssertEquals('The second image', 16, FList.Images[1].Width);
  AssertColorsEqual('with its colour', colRed, FList.Images[1].Colors[8, 4], LossyTolerance);
  AssertEquals('The third image', 24, FList.Images[2].Height);
  AssertTrue('keeps its alpha', FList.Images[2].Colors[0, 0].Alpha < AlphaOpaque);
  AssertEquals('The first image is the primary one', 'primary', FList[0].Info.Name);
  AssertTrue('Frames are pages', FList[1].Info.Kind = fkPage);
end;


procedure TTestHEIF.TestSingleReadIsThePrimaryImage;

var
  lWriter: TFPWriterHEIF;

begin
  RequireEncoder(hcHEVC);
  FList := TFPImageList.Create;
  FList.Add(CreateSolidImage(20, 10, colBlue));
  FList.Add(CreateSolidImage(30, 30, colGreen));
  lWriter := TFPWriterHEIF.Create;
  try
    FList.SaveToStream(FStream, lWriter);
  finally
    lWriter.Free;
  end;
  ReadIt;
  AssertEquals('A single image is the primary one', 20, FRead.Width);
  AssertColorsEqual('with its colour', colBlue, FRead.Colors[10, 5], LossyTolerance);
end;


procedure TTestHEIF.TestTruncatedFileRaises;

begin
  RequireEncoder(hcHEVC);
  AssertRaises('Half of a file raises', FPImageException, @ReadTruncated);
end;


procedure TTestHEIF.TestOtherFormatRaises;

begin
  AssertRaises('Reading a PNG file raises', FPImageException, @ReadPNG);
end;


procedure TTestHEIF.TestEmptyImageRaises;

begin
  RequireEncoder(hcHEVC);
  AssertRaises('Writing an image of 0x0 pixels raises', FPImageException, @WriteEmpty);
end;


procedure TTestHEIF.TestHandlersAreRegistered;

begin
  AssertTrue('HEIF has a reader', ImageHandlers.ImageReader[HEIFHandlerName] = TFPReaderHEIF);
  if HEIFCanEncode(hcHEVC) then
    AssertTrue('HEIF has a writer', ImageHandlers.ImageWriter[HEIFHandlerName] = TFPWriterHEIF);
  if HEIFCanDecode(hcAV1) then
    AssertTrue('AVIF has a reader', ImageHandlers.ImageReader[AVIFHandlerName] = TFPReaderHEIF);
  if HEIFCanEncode(hcAV1) then
    AssertTrue('AVIF has a writer', ImageHandlers.ImageWriter[AVIFHandlerName] = TFPWriterAVIF);
end;


initialization
  RegisterTest('heif', TTestHEIF);
end.
