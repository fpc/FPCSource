{
    Tests for the EXIF, ICC and XMP metadata of images: the EXIF orientation helper, and the metadata
    JPEG, PNG, TIFF and WebP files keep, with the orientation the readers apply.
    See the file COPYING.FPC, included in this distribution, for details.
}
unit tcmetadata;

{$mode objfpc}{$H+}

interface

uses sysutils, classes, fpcunit, testregistry, fpimage, fpimgtests, fpimgexif, jpeglib, jmorecfg,
     fpreadjpeg, fpwritejpeg, fpreadpng, fpwritepng, fpreadtiff, fpwritetiff, fpreadwebp, fpwritewebp;

type
  { Records the APP1 markers passed to ReadExtAPPn. }
  TRecordingJPEGReader = class(TFPReaderJPEG)
  protected
    procedure ReadExtAPPn(Marker: int; var Header: array of JOCTET; HeaderLen: uint;
      var Remaining: INT32; ReadData: jpeg_ext_appn_readdata); override;
  public
    Seen: TBytes;
  end;

  TTestMetadata = class(TTestCase)
  private
    FStream: TMemoryStream;
    FImage: TFPMemoryImage;
    FRead: TFPMemoryImage;
    // Returns EXIF data with only an orientation tag of aValue, in either byte order.
    function Exif(aValue: Word; aBig: Boolean): TBytes;
    // Returns aCount bytes counting up from aStart.
    function Bytes(aCount: Integer; aStart: Byte = 0): TBytes;
    // Returns a 3x2 image of six colours, each aCell pixels square.
    function Corners(aCell: Integer = 1): TFPMemoryImage;
    // Gives FImage EXIF data, an ICC profile of aICC bytes and an XMP packet.
    procedure AddMetadata(aICC: Integer);
    // Writes FImage with aWriter and reads it back into FRead with aReader.
    procedure RoundTripWith(aWriter: TFPCustomImageWriter; aReader: TFPCustomImageReader);
    // Checks that FRead has the metadata AddMetadata gave FImage.
    procedure CheckMetadata(const aFormat: String);
    // Checks that aReader turns an image of EXIF orientation 6 upright.
    procedure CheckOrientation(const aFormat: String; aWriter: TFPCustomImageWriter; aReader: TFPCustomImageReader);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTheOrientationIsRead;
    procedure TestTheOrientationIsSet;
    procedure TestExifWithoutOrientation;
    procedure TestDamagedExifHasNoOrientation;
    procedure TestTheExifHeaderIsRemoved;
    procedure TestEveryOrientationShowsUpright;
    procedure TestApplyingTheImageOrientationResetsIt;
    procedure TestJPEGKeepsTheMetadata;
    procedure TestJPEGSplitsALargeICCProfile;
    procedure TestJPEGAppliesTheOrientation;
    procedure TestJPEGStillPassesXMPToReadExtAPPn;
    procedure TestPNGKeepsTheMetadata;
    procedure TestPNGWritesTheProfileBeforeThePalette;
    procedure TestPNGAppliesTheOrientation;
    procedure TestPNGCanKeepTheOrientation;
    procedure TestTIFFKeepsTheICCProfileAndXMP;
    procedure TestWebPAppliesTheOrientation;
    procedure TestWebPExifWithTheJPEGHeaderIsRead;
    procedure TestMetadataGoesFromFormatToFormat;
  end;

implementation

procedure TRecordingJPEGReader.ReadExtAPPn(Marker: int; var Header: array of JOCTET; HeaderLen: uint;
  var Remaining: INT32; ReadData: jpeg_ext_appn_readdata);

begin
  SetLength(Seen, HeaderLen);
  if HeaderLen > 0 then
    Move(Header[0], Seen[0], HeaderLen);
end;


procedure TTestMetadata.SetUp;

begin
  inherited SetUp;
  FStream := TMemoryStream.Create;
end;


procedure TTestMetadata.TearDown;

begin
  FreeAndNil(FRead);
  FreeAndNil(FImage);
  FreeAndNil(FStream);
  inherited TearDown;
end;


function TTestMetadata.Exif(aValue: Word; aBig: Boolean): TBytes;

begin
  if aBig then
    Result := TBytes.Create(Ord('M'), Ord('M'), 0, 42, 0, 0, 0, 8, 0, 1, $01, $12, 0, 3, 0, 0, 0, 1,
      aValue shr 8, aValue and $FF, 0, 0, 0, 0, 0, 0)
  else
    Result := TBytes.Create(Ord('I'), Ord('I'), 42, 0, 8, 0, 0, 0, 1, 0, $12, $01, 3, 0, 1, 0, 0, 0,
      aValue and $FF, aValue shr 8, 0, 0, 0, 0, 0, 0);
end;


function TTestMetadata.Bytes(aCount: Integer; aStart: Byte): TBytes;

var
  i: Integer;

begin
  Result := nil;
  SetLength(Result, aCount);
  for i := 0 to aCount - 1 do
    Result[i] := (aStart + i * 7) and $FF;
end;


function TTestMetadata.Corners(aCell: Integer): TFPMemoryImage;

const
  cColors: array[0..1, 0..2] of TFPColor = (
    ((Red: $FFFF; Green: 0; Blue: 0; Alpha: $FFFF), (Red: 0; Green: $FFFF; Blue: 0; Alpha: $FFFF),
     (Red: 0; Green: 0; Blue: $FFFF; Alpha: $FFFF)),
    ((Red: $FFFF; Green: $FFFF; Blue: 0; Alpha: $FFFF), (Red: $FFFF; Green: $FFFF; Blue: $FFFF; Alpha: $FFFF),
     (Red: 0; Green: 0; Blue: 0; Alpha: $FFFF)));

var
  x, y: Integer;

begin
  Result := TFPMemoryImage.Create(3 * aCell, 2 * aCell);
  for y := 0 to 2 * aCell - 1 do
    for x := 0 to 3 * aCell - 1 do
      Result.Colors[x, y] := cColors[y div aCell, x div aCell];
end;


procedure TTestMetadata.AddMetadata(aICC: Integer);

begin
  FImage.Metadata[MetaExif] := Exif(1, False);
  FImage.Metadata[MetaICC] := Bytes(aICC, 3);
  FImage.Metadata[MetaXMP] := Bytes(40, 9);
end;


procedure TTestMetadata.RoundTripWith(aWriter: TFPCustomImageWriter; aReader: TFPCustomImageReader);

begin
  try
    FStream.Clear;
    FImage.SaveToStream(FStream, aWriter);
    FStream.Position := 0;
    FreeAndNil(FRead);
    FRead := TFPMemoryImage.Create(0, 0);
    FRead.LoadFromStream(FStream, aReader);
  finally
    aReader.Free;
    aWriter.Free;
  end;
end;


procedure TTestMetadata.CheckMetadata(const aFormat: String);

var
  lName: String;

begin
  for lName in [MetaExif, MetaICC, MetaXMP] do
    begin
    AssertEquals(aFormat + ' keeps the size of ' + lName, Length(FImage.Metadata[lName]), Length(FRead.Metadata[lName]));
    AssertTrue(aFormat + ' keeps the bytes of ' + lName,
      CompareMem(@FImage.Metadata[lName][0], @FRead.Metadata[lName][0], Length(FImage.Metadata[lName])));
    end;
end;


procedure TTestMetadata.CheckOrientation(const aFormat: String; aWriter: TFPCustomImageWriter;
  aReader: TFPCustomImageReader);

begin
  FImage := Corners(8);
  FImage.Metadata[MetaExif] := Exif(6, True);
  RoundTripWith(aWriter, aReader);
  AssertEquals(aFormat + ': orientation 6 turns the image to 16 pixels wide', 16, FRead.Width);
  AssertEquals(aFormat + ': and 24 high', 24, FRead.Height);
  AssertColorsEqual(aFormat + ': the bottom-left cell shows at the top left', colYellow, FRead.Colors[4, 4], 6000);
  AssertColorsEqual(aFormat + ': the top-left cell shows at the top right', colRed, FRead.Colors[12, 4], 6000);
  AssertEquals(aFormat + ': the orientation of the metadata is then 1', 1, ExifOrientation(FRead.Metadata[MetaExif]));
end;


procedure TTestMetadata.TestTheOrientationIsRead;

begin
  AssertEquals('The orientation of little-endian EXIF data', 6, ExifOrientation(Exif(6, False)));
  AssertEquals('The orientation of big-endian EXIF data', 8, ExifOrientation(Exif(8, True)));
end;


procedure TTestMetadata.TestTheOrientationIsSet;

var
  lData: TBytes;

begin
  lData := Exif(6, False);
  AssertTrue('A little-endian orientation is set', ExifSetOrientation(lData, 3));
  AssertEquals('to its new value', 3, ExifOrientation(lData));
  lData := Exif(6, True);
  AssertTrue('A big-endian orientation is set', ExifSetOrientation(lData, 2));
  AssertEquals('to its new value', 2, ExifOrientation(lData));
end;


procedure TTestMetadata.TestExifWithoutOrientation;

var
  lData: TBytes;

begin
  lData := Exif(6, False);
  lData[10] := $13;
  AssertEquals('EXIF data without an orientation tag has orientation 0', 0, ExifOrientation(lData));
  AssertFalse('and its orientation cannot be set', ExifSetOrientation(lData, 1));
  AssertEquals('An orientation out of range is 0', 0, ExifOrientation(Exif(9, False)));
end;


procedure TTestMetadata.TestDamagedExifHasNoOrientation;

var
  lData: TBytes;

begin
  AssertEquals('No data', 0, ExifOrientation(nil));
  lData := Copy(Exif(6, False), 0, 14);
  AssertEquals('A truncated entry', 0, ExifOrientation(lData));
  lData := Exif(6, False);
  lData[4] := 200;
  AssertEquals('An IFD offset beyond the data', 0, ExifOrientation(lData));
  lData := Exif(6, False);
  lData[0] := Ord('X');
  AssertEquals('An unknown byte order', 0, ExifOrientation(lData));
end;


procedure TTestMetadata.TestTheExifHeaderIsRemoved;

var
  lData: TBytes;

begin
  lData := Concat(TBytes.Create(Ord('E'), Ord('x'), Ord('i'), Ord('f'), 0, 0), Exif(3, False));
  AssertEquals('The Exif header is removed', 3, ExifOrientation(ExifWithoutHeader(lData)));
  AssertEquals('Data without the header is kept', 26, Length(ExifWithoutHeader(Exif(3, False))));
end;


procedure TTestMetadata.TestEveryOrientationShowsUpright;

const
  // The pixel of the 3x2 image that shows at the top left and at the top right, as (x, y).
  cTopLeft: array[1..8, 0..1] of Integer = ((0, 0), (2, 0), (2, 1), (0, 1), (0, 0), (0, 1), (2, 1), (2, 0));
  cTopRight: array[1..8, 0..1] of Integer = ((2, 0), (0, 0), (0, 1), (2, 1), (0, 1), (0, 0), (2, 0), (2, 1));

var
  lSource: TFPMemoryImage;
  i: Integer;

begin
  lSource := Corners;
  try
    for i := 1 to 8 do
      begin
      FreeAndNil(FImage);
      FImage := Corners;
      ExifApplyOrientation(FImage, i);
      if i >= 5 then
        AssertEquals(Format('Orientation %d swaps width and height', [i]), 2, FImage.Width)
      else
        AssertEquals(Format('Orientation %d keeps the width', [i]), 3, FImage.Width);
      AssertColorsEqual(Format('Orientation %d: the top-left pixel', [i]),
        lSource.Colors[cTopLeft[i, 0], cTopLeft[i, 1]], FImage.Colors[0, 0]);
      AssertColorsEqual(Format('Orientation %d: the top-right pixel', [i]),
        lSource.Colors[cTopRight[i, 0], cTopRight[i, 1]], FImage.Colors[FImage.Width - 1, 0]);
      end;
  finally
    lSource.Free;
  end;
end;


procedure TTestMetadata.TestApplyingTheImageOrientationResetsIt;

begin
  FImage := Corners;
  FImage.Metadata[MetaExif] := Exif(3, False);
  ExifApplyImageOrientation(FImage);
  AssertColorsEqual('The image is turned', colBlack, FImage.Colors[0, 0]);
  AssertEquals('and its orientation is 1', 1, ExifOrientation(FImage.Metadata[MetaExif]));
  ExifApplyImageOrientation(FImage);
  AssertColorsEqual('Applying it again changes nothing', colBlack, FImage.Colors[0, 0]);
end;


procedure TTestMetadata.TestJPEGKeepsTheMetadata;

begin
  FImage := CreateGradientImage(16, 8);
  AddMetadata(300);
  RoundTripWith(TFPWriterJPEG.Create, TFPReaderJPEG.Create);
  CheckMetadata('JPEG');
end;


procedure TTestMetadata.TestJPEGSplitsALargeICCProfile;

var
  lText: AnsiString;
  lCount, lPos: Integer;

begin
  FImage := CreateGradientImage(8, 8);
  AddMetadata(150000);
  RoundTripWith(TFPWriterJPEG.Create, TFPReaderJPEG.Create);
  SetString(lText, PAnsiChar(FStream.Memory), FStream.Size);
  lCount := 0;
  lPos := Pos('ICC_PROFILE', lText);
  while lPos > 0 do
    begin
    Inc(lCount);
    lPos := Pos('ICC_PROFILE', lText, lPos + 1);
    end;
  AssertEquals('A profile of 150000 bytes takes three APP2 markers', 3, lCount);
  CheckMetadata('JPEG');
end;


procedure TTestMetadata.TestJPEGAppliesTheOrientation;

begin
  CheckOrientation('JPEG', TFPWriterJPEG.Create, TFPReaderJPEG.Create);
end;


procedure TTestMetadata.TestJPEGStillPassesXMPToReadExtAPPn;

var
  lReader: TRecordingJPEGReader;
  lWriter: TFPWriterJPEG;

begin
  FImage := CreateGradientImage(8, 8);
  AddMetadata(10);
  lWriter := TFPWriterJPEG.Create;
  lReader := TRecordingJPEGReader.Create;
  try
    FImage.SaveToStream(FStream, lWriter);
    FStream.Position := 0;
    FRead := TFPMemoryImage.Create(0, 0);
    FRead.LoadFromStream(FStream, lReader);
    AssertEquals('ReadExtAPPn gets the whole XMP marker', 29 + 40, Length(lReader.Seen));
    AssertEquals('starting with its namespace', Ord('h'), lReader.Seen[0]);
  finally
    lReader.Free;
    lWriter.Free;
  end;
end;


procedure TTestMetadata.TestPNGKeepsTheMetadata;

var
  lWriter: TFPWriterPNG;

begin
  FImage := CreateAlphaImage(9, 5);
  AddMetadata(5000);
  lWriter := TFPWriterPNG.Create;
  lWriter.UseAlpha := True;
  RoundTripWith(lWriter, TFPReaderPNG.Create);
  CheckMetadata('PNG');
  AssertImagesEqual('PNG keeps the pixels with the metadata', FImage, FRead);
end;


procedure TTestMetadata.TestPNGWritesTheProfileBeforeThePalette;

var
  lWriter: TFPWriterPNG;
  lText: AnsiString;

begin
  FImage := CreateFewColorsImage(8, 4, 3);
  AddMetadata(64);
  lWriter := TFPWriterPNG.Create;
  lWriter.Indexed := True;
  RoundTripWith(lWriter, TFPReaderPNG.Create);
  SetString(lText, PAnsiChar(FStream.Memory), FStream.Size);
  AssertTrue('iCCP comes before PLTE', (Pos('iCCP', lText) > 0) and (Pos('iCCP', lText) < Pos('PLTE', lText)));
  AssertTrue('eXIf comes before IDAT', (Pos('eXIf', lText) > 0) and (Pos('eXIf', lText) < Pos('IDAT', lText)));
  CheckMetadata('An indexed PNG');
end;


procedure TTestMetadata.TestPNGAppliesTheOrientation;

begin
  CheckOrientation('PNG', TFPWriterPNG.Create, TFPReaderPNG.Create);
end;


procedure TTestMetadata.TestPNGCanKeepTheOrientation;

var
  lReader: TFPReaderPNG;

begin
  FImage := Corners;
  FImage.Metadata[MetaExif] := Exif(6, False);
  lReader := TFPReaderPNG.Create;
  lReader.ApplyOrientation := False;
  RoundTripWith(TFPWriterPNG.Create, lReader);
  AssertEquals('Without ApplyOrientation the image keeps its width', 3, FRead.Width);
  AssertEquals('and its orientation', 6, ExifOrientation(FRead.Metadata[MetaExif]));
end;


procedure TTestMetadata.TestTIFFKeepsTheICCProfileAndXMP;

begin
  FImage := CreateGradientImage(6, 4);
  AddMetadata(1000);
  RoundTripWith(TFPWriterTiff.Create, TFPReaderTiff.Create);
  AssertEquals('TIFF keeps the ICC profile', 1000, Length(FRead.Metadata[MetaICC]));
  AssertTrue('byte for byte', CompareMem(@FImage.Metadata[MetaICC][0], @FRead.Metadata[MetaICC][0], 1000));
  AssertEquals('and the XMP packet', 40, Length(FRead.Metadata[MetaXMP]));
  AssertEquals('with its bytes', FImage.Metadata[MetaXMP][39], FRead.Metadata[MetaXMP][39]);
end;


procedure TTestMetadata.TestWebPAppliesTheOrientation;

begin
  CheckOrientation('WebP', TFPWriterWebP.Create, TFPReaderWebP.Create);
end;


procedure TTestMetadata.TestWebPExifWithTheJPEGHeaderIsRead;

begin
  FImage := Corners;
  FImage.Metadata[MetaExif] := Concat(TBytes.Create(Ord('E'), Ord('x'), Ord('i'), Ord('f'), 0, 0), Exif(3, False));
  RoundTripWith(TFPWriterWebP.Create, TFPReaderWebP.Create);
  AssertEquals('An EXIF chunk starting with Exif'#0#0' is read without it', 26, Length(FRead.Metadata[MetaExif]));
  AssertColorsEqual('and its orientation is applied', colBlack, FRead.Colors[0, 0]);
end;


procedure TTestMetadata.TestMetadataGoesFromFormatToFormat;

var
  lImage: TFPMemoryImage;

begin
  FImage := CreateGradientImage(12, 6);
  AddMetadata(700);
  RoundTripWith(TFPWriterJPEG.Create, TFPReaderJPEG.Create);
  lImage := FImage;
  FImage := FRead;
  FRead := nil;
  try
    RoundTripWith(TFPWriterPNG.Create, TFPReaderPNG.Create);
    FreeAndNil(FImage);
    FImage := FRead;
    FRead := nil;
    RoundTripWith(TFPWriterWebP.Create, TFPReaderWebP.Create);
    FreeAndNil(FImage);
    FImage := lImage;
    lImage := nil;
    CheckMetadata('JPEG to PNG to WebP');
  finally
    lImage.Free;
  end;
end;


initialization
  RegisterTest('metadata', TTestMetadata);
end.
