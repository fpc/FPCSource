{
    Tests for the QOI reader and writer: round trips, the header written,
    the writer checked by a decoder of the test itself, and every operation.
    See the file COPYING.FPC, included in this distribution, for details.
}
unit tcqoi;

{$mode objfpc}{$H+}

interface

uses sysutils, classes, fpcunit, testregistry, fpimage, fpimgtests,
     qoicomn, fpreadqoi, fpwriteqoi;

type
  TTestQOI = class(TTestCase)
  private
    FReader: TFPReaderQoi;
    FWriter: TFPWriterQoi;
    FImage: TFPMemoryImage;
    FRead: TFPMemoryImage;
    FStream: TMemoryStream;
    // Replaces the stream with a header, the operations and the end marker.
    procedure Build(aWidth, aHeight: LongWord; aChannels: Byte; const aOps: array of Byte);
    procedure ReadIt;
    // Decodes the stream following the QOI specification.
    function Decode(aStream: TMemoryStream): TFPMemoryImage;
    // Writes FImage and checks it with the decoder of the test.
    procedure CheckWriter(const aMessage: String);
    procedure CheckColor(const aMessage: String; aX, aY: Integer; const aColor: TFPColor);
    procedure ReadTruncated;
    procedure ReadFiveChannels;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRoundTripKeepsTheColors;
    procedure TestRoundTripKeepsAlpha;
    procedure TestRoundTripOfLongRuns;
    procedure TestRoundTripOfRepeatedColors;
    procedure TestRoundTripOfOnePixel;
    procedure TestTheHeaderWritten;
    procedure TestTheStreamEndsWithTheMarker;
    procedure TestTheWriterFollowsTheSpecification;
    procedure TestTheWriterFollowsTheSpecificationWithAlpha;
    procedure TestReadEveryOperation;
    procedure TestReadDifferencesWrapAround;
    procedure TestReadRGBKeepsAlpha;
    procedure TestReadingAfterOtherData;
    procedure TestTheReaderStopsAtTheEndMarker;
    procedure TestContentsCheck;
    procedure TestReadTwoEqualIndexOperations;
    procedure TestTheIndexStartsEmpty;
    procedure TestTruncatedDataRaises;
    procedure TestAnInvalidChannelCountRaises;
  end;

implementation

procedure TTestQOI.SetUp;

begin
  inherited SetUp;
  FReader := TFPReaderQoi.Create;
  FWriter := TFPWriterQoi.Create;
  FStream := TMemoryStream.Create;
end;


procedure TTestQOI.TearDown;

begin
  FreeAndNil(FStream);
  FreeAndNil(FRead);
  FreeAndNil(FImage);
  FreeAndNil(FWriter);
  FreeAndNil(FReader);
  inherited TearDown;
end;


procedure TTestQOI.Build(aWidth, aHeight: LongWord; aChannels: Byte; const aOps: array of Byte);

const
  cEnd: array[0..7] of Byte = (0, 0, 0, 0, 0, 0, 0, 1);

var
  lHeader: TQoiHeader;

begin
  lHeader.magic := 'qoif';
  lHeader.width := NtoBE(aWidth);
  lHeader.height := NtoBE(aHeight);
  lHeader.channels := aChannels;
  lHeader.colorspace := 0;
  FStream.Clear;
  FStream.WriteBuffer(lHeader, SizeOf(lHeader));
  if Length(aOps) > 0 then
    FStream.WriteBuffer(aOps[0], Length(aOps));
  FStream.WriteBuffer(cEnd, SizeOf(cEnd));
  FStream.Position := 0;
end;


procedure TTestQOI.ReadIt;

begin
  FreeAndNil(FRead);
  FRead := TFPMemoryImage.Create(0, 0);
  FRead.LoadFromStream(FStream, FReader);
end;


function TTestQOI.Decode(aStream: TMemoryStream): TFPMemoryImage;

var
  lP: PByte;
  lWidth, lHeight, lCount, I: LongWord;
  lIndex: array[0..63] of TQoiPixel;
  lPx: TQoiPixel;
  lB1, lB2: Byte;
  lRun, lVG: Integer;

  function Next: Byte;

  begin
    Result := lP^;
    Inc(lP);
  end;

begin
  lP := aStream.Memory;
  AssertTrue('The stream starts with qoif', CompareMem(lP, PChar('qoif'), 4));
  lWidth := BEtoN(PLongWord(lP + 4)^);
  lHeight := BEtoN(PLongWord(lP + 8)^);
  Inc(lP, 14);
  FillChar(lIndex, SizeOf(lIndex), 0);
  lPx.r := 0;
  lPx.g := 0;
  lPx.b := 0;
  lPx.a := 255;
  lRun := 0;
  lCount := lWidth * lHeight;
  Result := TFPMemoryImage.Create(lWidth, lHeight);
  for I := 0 to lCount - 1 do
    begin
    if lRun > 0 then
      Dec(lRun)
    else
      begin
      lB1 := Next;
      if lB1 = $FE then
        begin
        lPx.r := Next;
        lPx.g := Next;
        lPx.b := Next;
        end
      else if lB1 = $FF then
        begin
        lPx.r := Next;
        lPx.g := Next;
        lPx.b := Next;
        lPx.a := Next;
        end
      else
        case lB1 and $C0 of
          $00: lPx := lIndex[lB1];
          $40:
            begin
            lPx.r := Byte(Integer(lPx.r) + ((lB1 shr 4) and 3) - 2);
            lPx.g := Byte(Integer(lPx.g) + ((lB1 shr 2) and 3) - 2);
            lPx.b := Byte(Integer(lPx.b) + (lB1 and 3) - 2);
            end;
          $80:
            begin
            lB2 := Next;
            lVG := (lB1 and $3F) - 32;
            lPx.r := Byte(Integer(lPx.r) + lVG - 8 + ((lB2 shr 4) and $F));
            lPx.b := Byte(Integer(lPx.b) + lVG - 8 + (lB2 and $F));
            lPx.g := Byte(Integer(lPx.g) + lVG);
            end;
          $C0: lRun := lB1 and $3F;
        end;
      lIndex[QoiPixelIndex(lPx)] := lPx;
      end;
    Result[I mod lWidth, I div lWidth] := RGB8(lPx.r, lPx.g, lPx.b, lPx.a);
    end;
  AssertTrue('The operations end with the end marker',
    CompareMem(lP, PChar(#0#0#0#0#0#0#0#1), 8));
  AssertEquals('Nothing follows the end marker', aStream.Size, Int64(PtrUInt(lP) - PtrUInt(aStream.Memory) + 8));
end;


procedure TTestQOI.CheckWriter(const aMessage: String);

var
  lDecoded: TFPMemoryImage;

begin
  FStream.Clear;
  FImage.SaveToStream(FStream, FWriter);
  lDecoded := Decode(FStream);
  try
    AssertImagesEqual(aMessage, FImage, lDecoded, 0, not FWriter.UseAlpha);
  finally
    lDecoded.Free;
  end;
end;


procedure TTestQOI.CheckColor(const aMessage: String; aX, aY: Integer; const aColor: TFPColor);

begin
  AssertColorsEqual(aMessage, aColor, FRead[aX, aY]);
end;


procedure TTestQOI.ReadTruncated;

begin
  { one RGBA pixel and the eight marker bytes as INDEX operations: 9 of 20 pixels }
  Build(20, 1, 4, [$FF, 1, 2, 3, 255]);
  ReadIt;
end;


procedure TTestQOI.ReadFiveChannels;

begin
  Build(1, 1, 5, [$FE, 1, 2, 3]);
  ReadIt;
end;


procedure TTestQOI.TestRoundTripKeepsTheColors;

begin
  FImage := CreateGradientImage(23, 17);
  FRead := RoundTrip(FImage, FWriter, FReader);
  AssertImagesEqual('A QOI keeps every colour', FImage, FRead);
end;


procedure TTestQOI.TestRoundTripKeepsAlpha;

begin
  FImage := CreateAlphaImage(11, 9);
  FWriter.UseAlpha := True;
  FRead := RoundTrip(FImage, FWriter, FReader);
  AssertImagesEqual('A QOI with alpha keeps alpha', FImage, FRead);
end;


procedure TTestQOI.TestRoundTripOfLongRuns;

begin
  FImage := CreateSolidImage(100, 3, RGB8(1, 2, 3));
  FImage[50, 1] := colRed;
  FRead := RoundTrip(FImage, FWriter, FReader);
  AssertImagesEqual('Runs longer than 62 pixels come back', FImage, FRead);
end;


procedure TTestQOI.TestRoundTripOfRepeatedColors;

begin
  FImage := CreateFewColorsImage(30, 10, 5);
  FRead := RoundTrip(FImage, FWriter, FReader);
  AssertImagesEqual('Colours seen before come back', FImage, FRead);
end;


procedure TTestQOI.TestRoundTripOfOnePixel;

begin
  FImage := CreateSolidImage(1, 1, RGB8(9, 8, 7));
  FRead := RoundTrip(FImage, FWriter, FReader);
  AssertImagesEqual('A single pixel comes back', FImage, FRead);
end;


procedure TTestQOI.TestTheHeaderWritten;

var
  lHeader: TQoiHeader;

begin
  FImage := CreateGradientImage(300, 2);
  FImage.SaveToStream(FStream, FWriter);
  FStream.Position := 0;
  FStream.ReadBuffer(lHeader, SizeOf(lHeader));
  AssertEquals('Magic', 'qoif', lHeader.magic);
  AssertEquals('Width, big-endian', 300, BEtoN(lHeader.width));
  AssertEquals('Height, big-endian', 2, BEtoN(lHeader.height));
  AssertEquals('Three channels by default', 3, lHeader.channels);
  AssertEquals('sRGB colour space', 0, lHeader.colorspace);
  FWriter.UseAlpha := True;
  FStream.Clear;
  FImage.SaveToStream(FStream, FWriter);
  FStream.Position := 0;
  FStream.ReadBuffer(lHeader, SizeOf(lHeader));
  AssertEquals('Four channels with alpha', 4, lHeader.channels);
end;


procedure TTestQOI.TestTheStreamEndsWithTheMarker;

begin
  FImage := CreateGradientImage(5, 5);
  FImage.SaveToStream(FStream, FWriter);
  AssertTrue('The stream ends with seven zero bytes and a one',
    CompareMem(PByte(FStream.Memory) + FStream.Size - 8, PChar(#0#0#0#0#0#0#0#1), 8));
end;


procedure TTestQOI.TestTheWriterFollowsTheSpecification;

begin
  FImage := CreateGradientImage(37, 13);
  FImage[3, 3] := FImage[20, 5];
  CheckWriter('The decoder of the specification reads the writer''s gradient');
  FreeAndNil(FImage);
  FImage := CreateFewColorsImage(64, 8, 9);
  CheckWriter('The decoder of the specification reads the writer''s repeated colours');
  FreeAndNil(FImage);
  FImage := CreateSolidImage(200, 1, colWhite);
  CheckWriter('The decoder of the specification reads the writer''s long run');
end;


procedure TTestQOI.TestTheWriterFollowsTheSpecificationWithAlpha;

begin
  FImage := CreateAlphaImage(19, 7);
  FWriter.UseAlpha := True;
  CheckWriter('The decoder of the specification reads the writer''s alpha');
end;


procedure TTestQOI.TestReadEveryOperation;

begin
  { RGBA, DIFF(+1,-1,0), LUMA(g+5, r-g=-2, b-g=+3), INDEX of the first pixel, RUN of 2 }
  Build(6, 1, 4, [$FF, 10, 20, 30, 255,
                  $76,
                  $A5, $6B,
                  $09,
                  $C1]);
  ReadIt;
  CheckColor('RGBA', 0, 0, RGB8(10, 20, 30));
  CheckColor('DIFF', 1, 0, RGB8(11, 19, 30));
  CheckColor('LUMA', 2, 0, RGB8(14, 24, 38));
  CheckColor('INDEX', 3, 0, RGB8(10, 20, 30));
  CheckColor('RUN, first pixel', 4, 0, RGB8(10, 20, 30));
  CheckColor('RUN, second pixel', 5, 0, RGB8(10, 20, 30));
end;


procedure TTestQOI.TestReadDifferencesWrapAround;

begin
  { from the start pixel (0,0,0,255): DIFF(-2,-2,-2), then LUMA(g-32, r-g=-8, b-g=+7) }
  Build(2, 1, 3, [$40, $80, $0F]);
  ReadIt;
  CheckColor('Differences wrap below 0', 0, 0, RGB8(254, 254, 254));
  CheckColor('Luma differences wrap too', 1, 0, RGB8(214, 222, 229));
end;


procedure TTestQOI.TestReadRGBKeepsAlpha;

begin
  Build(2, 1, 4, [$FF, 1, 2, 3, 100, $FE, 4, 5, 6]);
  ReadIt;
  CheckColor('RGBA sets alpha', 0, 0, RGB8(1, 2, 3, 100));
  CheckColor('RGB keeps the alpha of the pixel before', 1, 0, RGB8(4, 5, 6, 100));
end;


procedure TTestQOI.TestReadingAfterOtherData;

begin
  FImage := CreateGradientImage(4, 3);
  FStream.WriteBuffer(PChar('prefix')^, 6);
  WriteImage(FImage, FWriter, FStream);
  FStream.Position := 6;
  ReadIt;
  AssertImagesEqual('The image after the prefix reads back', FImage, FRead);
end;


procedure TTestQOI.TestTheReaderStopsAtTheEndMarker;

var
  lSize: Int64;

begin
  Build(1, 1, 4, [$FE, 1, 2, 3]);
  lSize := FStream.Size;
  FStream.Position := lSize;
  FStream.WriteBuffer(PChar('next')^, 4);
  FStream.Position := 0;
  ReadIt;
  AssertEquals('The reader stops after the end marker', lSize, FStream.Position);
end;


procedure TTestQOI.TestContentsCheck;

begin
  Build(1, 1, 4, [$FE, 1, 2, 3]);
  AssertTrue('A qoif stream is accepted', FReader.CheckContents(FStream));
  FStream.Position := 0;
  FStream.WriteBuffer(PChar('qoix')^, 4);
  FStream.Position := 0;
  AssertFalse('Another magic is rejected', FReader.CheckContents(FStream));
end;


procedure TTestQOI.TestReadTwoEqualIndexOperations;

begin
  Build(4, 1, 4, [$FF, 10, 20, 30, 255,
                  $FE, 40, 50, 60,
                  $09, $09]);
  ReadIt;
  CheckColor('The first INDEX', 2, 0, RGB8(10, 20, 30));
  CheckColor('A second equal INDEX', 3, 0, RGB8(10, 20, 30));
end;


procedure TTestQOI.TestTheIndexStartsEmpty;

begin
  { 53 is the index of the start pixel (0,0,0,255) }
  Build(1, 1, 4, [$35]);
  ReadIt;
  CheckColor('An unused index entry is transparent black', 0, 0, FPColor(0, 0, 0, 0));
end;


procedure TTestQOI.TestTruncatedDataRaises;

begin
  AssertRaises('Data ending before the last pixel raises', FPImageException, @ReadTruncated);
end;


procedure TTestQOI.TestAnInvalidChannelCountRaises;

begin
  AssertRaises('A channel count other than 3 or 4 raises', FPImageException, @ReadFiveChannels);
end;


initialization
  RegisterTest('qoi', TTestQOI);
end.
