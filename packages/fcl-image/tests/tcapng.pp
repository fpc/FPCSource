{
    Tests for animated PNG: the frames written and read, the chunks written, raw and
    composited frames with their disposal and blending, and a default image outside the animation.
    See the file COPYING.FPC, included in this distribution, for details.
}
unit tcapng;

{$mode objfpc}{$H+}

interface

uses sysutils, classes, fpcunit, testregistry, fpimage, fpimgtests, fpimagelist, fpimgcmn,
     pngcomn, fpreadpng, fpwritepng, fpreadgif, fpwritegif, zstream;

type
  TTestAPNG = class(TTestCase)
  private
    FStream: TMemoryStream;
    FReader: TFPReaderPNG;
    FWriter: TFPWriterPNG;
    FImages: array of TFPMemoryImage;
    FFrames: array of TFPCustomImage;
    FInfos: array of TFPFrameInfo;
    FFramesInfo: TFPFramesInfo;
    // Adds a solid image of aColor to FImages and returns it.
    function Solid(aWidth, aHeight: Integer; const aColor: TFPColor): TFPMemoryImage;
    // Returns an animation frame description.
    function Info(aLeft, aTop: Integer; aDelay: Cardinal; aDisposal: TFPFrameDisposal; aBlend: TFPFrameBlend): TFPFrameInfo;
    // Writes the images with the frame descriptions to FStream as an animation of aFrameCount frames.
    procedure WriteAnimation(const aInfos: array of TFPFrameInfo; aFrameCount: Integer; aLoopCount: Integer = 0);
    // Reads every frame of FStream into FFrames and FInfos.
    procedure ReadAnimation(aComposite: Boolean = True);
    // Returns the types of the chunks of FStream, separated by spaces.
    function ChunkList: String;
    // Returns the data of chunk aIndex of FStream.
    function ChunkData(aIndex: Integer): TBytes;
    // Appends a chunk with its length and CRC to aStream.
    procedure AddChunk(aStream: TStream; const aType: String; const aData: TBytes);
    // Returns aValue as four big-endian bytes.
    function BE32(aValue: LongWord): TBytes;
    // Returns the zlib data of the scan lines of a row of RGBA8 pixels.
    function ZRow(const aPixels: array of Byte): TBytes;
    procedure WriteASmallFirstFrame;
    procedure WriteAFrameOutsideTheCanvas;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestFramesComeBack;
    procedure TestEightBitFramesComeBack;
    procedure TestTheChunksWritten;
    procedure TestTheAnimationControlWritten;
    procedure TestAnUnknownFrameCountIsFilledIn;
    procedure TestOneFrameIsAPlainPNG;
    procedure TestAPlainPNGIsOneFrame;
    procedure TestTheDefaultImageIsTheFirstFrame;
    procedure TestRawFramesKeepTheirPlace;
    procedure TestFramesAreCompositedByDefault;
    procedure TestDisposalToTheBackground;
    procedure TestDisposalToThePrevious;
    procedure TestBlendingOver;
    procedure TestADefaultImageOutsideTheAnimation;
    procedure TestADelayDenominatorOfZeroIsHundredths;
    procedure TestTheFirstFrameCoversTheCanvas;
    procedure TestAFrameOutsideTheCanvasRaises;
    procedure TestGIFToAPNGAndBack;
    procedure TestTheReaderIsReusable;
  end;

implementation

procedure TTestAPNG.SetUp;

begin
  inherited SetUp;
  FStream := TMemoryStream.Create;
  FReader := TFPReaderPNG.Create;
  FWriter := TFPWriterPNG.Create;
end;


procedure TTestAPNG.TearDown;

var
  i: Integer;

begin
  for i := 0 to High(FFrames) do
    FFrames[i].Free;
  FFrames := nil;
  for i := 0 to High(FImages) do
    FImages[i].Free;
  FImages := nil;
  FInfos := nil;
  FreeAndNil(FWriter);
  FreeAndNil(FReader);
  FreeAndNil(FStream);
  inherited TearDown;
end;


function TTestAPNG.Solid(aWidth, aHeight: Integer; const aColor: TFPColor): TFPMemoryImage;

begin
  Result := CreateSolidImage(aWidth, aHeight, aColor);
  SetLength(FImages, Length(FImages) + 1);
  FImages[High(FImages)] := Result;
end;


function TTestAPNG.Info(aLeft, aTop: Integer; aDelay: Cardinal; aDisposal: TFPFrameDisposal; aBlend: TFPFrameBlend): TFPFrameInfo;

begin
  Result := DefaultFrameInfo;
  Result.Kind := fkAnimation;
  Result.Left := aLeft;
  Result.Top := aTop;
  Result.Delay := aDelay;
  Result.Disposal := aDisposal;
  Result.Blend := aBlend;
end;


procedure TTestAPNG.WriteAnimation(const aInfos: array of TFPFrameInfo; aFrameCount: Integer; aLoopCount: Integer);

var
  lInfo: TFPFramesInfo;
  i: Integer;

begin
  lInfo := DefaultFramesInfo;
  lInfo.FrameCount := aFrameCount;
  lInfo.LoopCount := aLoopCount;
  FStream.Clear;
  FWriter.BeginFrames(FStream, lInfo);
  for i := 0 to High(FImages) do
    FWriter.WriteNextFrame(FImages[i], aInfos[i]);
  FWriter.EndFrames;
  FStream.Position := 0;
end;


procedure TTestAPNG.ReadAnimation(aComposite: Boolean);

var
  lImage: TFPCustomImage;
  lInfo: TFPFrameInfo;

begin
  FReader.Composite := aComposite;
  FFramesInfo := FReader.BeginFrames(FStream);
  repeat
    lImage := FReader.ReadNextFrame(lInfo);
    if lImage = nil then
      break;
    SetLength(FFrames, Length(FFrames) + 1);
    FFrames[High(FFrames)] := lImage;
    SetLength(FInfos, Length(FInfos) + 1);
    FInfos[High(FInfos)] := lInfo;
  until False;
  FReader.EndFrames;
end;


function TTestAPNG.ChunkList: String;

var
  lPos: Int64;
  lLength: LongWord;
  lType: String;

begin
  Result := '';
  lPos := 8;
  while lPos + 8 <= FStream.Size do
    begin
    lLength := BEtoN(PLongWord(PByte(FStream.Memory) + lPos)^);
    SetString(lType, PChar(FStream.Memory) + lPos + 4, 4);
    if Result <> '' then
      Result := Result + ' ';
    Result := Result + lType;
    Inc(lPos, 12 + lLength);
    end;
end;


function TTestAPNG.ChunkData(aIndex: Integer): TBytes;

var
  lPos: Int64;
  lLength: LongWord;

begin
  lPos := 8;
  lLength := BEtoN(PLongWord(PByte(FStream.Memory) + lPos)^);
  while aIndex > 0 do
    begin
    Inc(lPos, 12 + lLength);
    lLength := BEtoN(PLongWord(PByte(FStream.Memory) + lPos)^);
    Dec(aIndex);
    end;
  Result := nil;
  SetLength(Result, lLength);
  if lLength > 0 then
    Move((PByte(FStream.Memory) + lPos + 8)^, Result[0], lLength);
end;


procedure TTestAPNG.AddChunk(aStream: TStream; const aType: String; const aData: TBytes);

var
  lCRC: LongWord;
  lCode: TChunkCode;

begin
  aStream.WriteBuffer(BE32(Length(aData))[0], 4);
  Move(aType[1], lCode, 4);
  aStream.WriteBuffer(lCode, 4);
  if Length(aData) > 0 then
    aStream.WriteBuffer(aData[0], Length(aData));
  lCRC := CalculateCRC(All1Bits, lCode, 4);
  if Length(aData) > 0 then
    lCRC := CalculateCRC(lCRC, aData[0], Length(aData));
  aStream.WriteBuffer(BE32(lCRC xor All1Bits)[0], 4);
end;


function TTestAPNG.BE32(aValue: LongWord): TBytes;

begin
  Result := TBytes.Create(aValue shr 24, (aValue shr 16) and $FF, (aValue shr 8) and $FF, aValue and $FF);
end;


function TTestAPNG.ZRow(const aPixels: array of Byte): TBytes;

var
  lOut: TMemoryStream;
  lZip: TCompressionStream;
  lFilter: Byte;

begin
  lOut := TMemoryStream.Create;
  try
    lZip := TCompressionStream.Create(clDefault, lOut);
    try
      lFilter := 0;
      lZip.WriteBuffer(lFilter, 1);
      lZip.WriteBuffer(aPixels[0], Length(aPixels));
    finally
      lZip.Free;
    end;
    Result := nil;
    SetLength(Result, lOut.Size);
    Move(lOut.Memory^, Result[0], lOut.Size);
  finally
    lOut.Free;
  end;
end;


procedure TTestAPNG.WriteASmallFirstFrame;

begin
  FFramesInfo := DefaultFramesInfo;
  FFramesInfo.Width := 4;
  FFramesInfo.Height := 4;
  FFramesInfo.FrameCount := 2;
  FWriter.BeginFrames(FStream, FFramesInfo);
  FWriter.WriteNextFrame(Solid(2, 2, colRed), Info(0, 0, 0, fdNone, fbSource));
end;


procedure TTestAPNG.WriteAFrameOutsideTheCanvas;

begin
  FFramesInfo := DefaultFramesInfo;
  FFramesInfo.FrameCount := 2;
  FWriter.BeginFrames(FStream, FFramesInfo);
  FWriter.WriteNextFrame(Solid(4, 4, colRed), Info(0, 0, 0, fdNone, fbSource));
  FWriter.WriteNextFrame(Solid(2, 2, colBlue), Info(3, 0, 0, fdNone, fbSource));
end;


procedure TTestAPNG.TestFramesComeBack;

var
  i: Integer;

begin
  FImages := nil;
  SetLength(FImages, 3);
  FImages[0] := CreateGradientImage(9, 7);
  FImages[1] := CreateAlphaImage(9, 7);
  FImages[2] := CreateSolidImage(9, 7, colYellow);
  WriteAnimation([Info(0, 0, 100, fdNone, fbSource), Info(0, 0, 250, fdNone, fbSource),
    Info(0, 0, 40, fdNone, fbSource)], 3, 5);
  ReadAnimation;
  AssertEquals('Three frames are announced', 3, FFramesInfo.FrameCount);
  AssertEquals('The plays are read', 5, FFramesInfo.LoopCount);
  AssertEquals('The canvas', 9, FFramesInfo.Width);
  AssertEquals('Three frames are read', 3, Length(FFrames));
  for i := 0 to 2 do
    begin
    AssertImagesEqual('Frame ' + IntToStr(i) + ' comes back', FImages[i], FFrames[i]);
    AssertTrue('Frame ' + IntToStr(i) + ' is an animation frame', FInfos[i].Kind = fkAnimation);
    end;
  AssertEquals('The delay of the second frame', 250, FInfos[1].Delay);
end;


procedure TTestAPNG.TestEightBitFramesComeBack;

begin
  FWriter.WordSized := False;
  Solid(3, 2, RGB8(10, 20, 30, 128));
  Solid(3, 2, RGB8(200, 100, 50));
  WriteAnimation([Info(0, 0, 0, fdNone, fbSource), Info(0, 0, 0, fdNone, fbSource)], 2);
  AssertEquals('Eight bits per channel', 8, ChunkData(0)[8]);
  AssertEquals('RGBA', 6, ChunkData(0)[9]);
  ReadAnimation;
  AssertImagesEqual('The first frame comes back', FImages[0], FFrames[0]);
  AssertImagesEqual('The second frame comes back', FImages[1], FFrames[1]);
end;


procedure TTestAPNG.TestTheChunksWritten;

var
  lData: TBytes;

begin
  Solid(4, 4, colRed);
  Solid(4, 4, colGreen);
  Solid(2, 1, colBlue);
  WriteAnimation([Info(0, 0, 100, fdNone, fbSource), Info(0, 0, 100, fdBackground, fbSource),
    Info(1, 2, 70000, fdPrevious, fbOver)], 3);
  AssertEquals('The chunks of an animated PNG', 'IHDR acTL fcTL IDAT fcTL fdAT fcTL fdAT IEND', ChunkList);
  AssertEquals('The first frame control has sequence number 0', 0, BEtoN(PLongWord(@ChunkData(2)[0])^));
  AssertEquals('The first frame data has sequence number 2', 2, BEtoN(PLongWord(@ChunkData(5)[0])^));
  lData := ChunkData(6);
  AssertEquals('The last frame control has sequence number 3', 3, BEtoN(PLongWord(@lData[0])^));
  AssertEquals('Its width', 2, BEtoN(PLongWord(@lData[4])^));
  AssertEquals('Its x offset', 1, BEtoN(PLongWord(@lData[12])^));
  AssertEquals('Its y offset', 2, BEtoN(PLongWord(@lData[16])^));
  AssertEquals('A delay beyond 65535 ms is written in hundredths', 7000, BEtoN(PWord(@lData[20])^));
  AssertEquals('with a denominator of 100', 100, BEtoN(PWord(@lData[22])^));
  AssertEquals('Disposal to the previous', APNGDisposePrevious, lData[24]);
  AssertEquals('Blending over', APNGBlendOver, lData[25]);
  AssertEquals('Disposal to the background', APNGDisposeBackground, ChunkData(4)[24]);
end;


procedure TTestAPNG.TestTheAnimationControlWritten;

var
  lData: TBytes;

begin
  Solid(2, 2, colRed);
  Solid(2, 2, colGreen);
  WriteAnimation([Info(0, 0, 0, fdNone, fbSource), Info(0, 0, 0, fdNone, fbSource)], 2, 3);
  lData := ChunkData(1);
  AssertEquals('The number of frames', 2, BEtoN(PLongWord(@lData[0])^));
  AssertEquals('The number of plays', 3, BEtoN(PLongWord(@lData[4])^));
end;


procedure TTestAPNG.TestAnUnknownFrameCountIsFilledIn;

begin
  Solid(2, 2, colRed);
  Solid(2, 2, colGreen);
  Solid(2, 2, colBlue);
  WriteAnimation([Info(0, 0, 0, fdNone, fbSource), Info(0, 0, 0, fdNone, fbSource),
    Info(0, 0, 0, fdNone, fbSource)], -1);
  AssertEquals('The number of frames is written at the end', 3, BEtoN(PLongWord(@ChunkData(1)[0])^));
  AssertEquals('Filling it in leaves the chunks after it', 'IHDR acTL fcTL IDAT fcTL fdAT fcTL fdAT IEND', ChunkList);
  ReadAnimation;
  AssertEquals('Every frame is read', 3, Length(FFrames));
end;


procedure TTestAPNG.TestOneFrameIsAPlainPNG;

begin
  Solid(3, 3, colRed);
  WriteAnimation([Info(0, 0, 0, fdNone, fbSource)], 1);
  AssertEquals('One frame is written as a plain PNG', 'IHDR IDAT IEND', ChunkList);
end;


procedure TTestAPNG.TestAPlainPNGIsOneFrame;

begin
  WriteImage(Solid(5, 4, colGreen), FWriter, FStream);
  FStream.Position := 0;
  ReadAnimation;
  AssertEquals('A plain PNG has one frame', 1, Length(FFrames));
  AssertTrue('which is a page', FInfos[0].Kind = fkPage);
  AssertImagesEqual('with the image', FImages[0], FFrames[0]);
end;


procedure TTestAPNG.TestTheDefaultImageIsTheFirstFrame;

var
  lImage: TFPMemoryImage;

begin
  Solid(3, 3, colRed);
  Solid(3, 3, colBlue);
  WriteAnimation([Info(0, 0, 0, fdNone, fbSource), Info(0, 0, 0, fdNone, fbSource)], 2);
  lImage := TFPMemoryImage.Create(0, 0);
  try
    lImage.LoadFromStream(FStream, FReader);
    AssertImagesEqual('Reading one image gives the first frame', FImages[0], lImage);
  finally
    lImage.Free;
  end;
end;


procedure TTestAPNG.TestRawFramesKeepTheirPlace;

begin
  Solid(4, 4, colRed);
  Solid(2, 2, colBlue);
  WriteAnimation([Info(0, 0, 0, fdNone, fbSource), Info(1, 2, 0, fdBackground, fbOver)], 2);
  ReadAnimation(False);
  AssertEquals('A raw frame has its own width', 2, FFrames[1].Width);
  AssertEquals('Left', 1, FInfos[1].Left);
  AssertEquals('Top', 2, FInfos[1].Top);
  AssertTrue('Its disposal', FInfos[1].Disposal = fdBackground);
  AssertTrue('Its blending', FInfos[1].Blend = fbOver);
end;


procedure TTestAPNG.TestFramesAreCompositedByDefault;

begin
  Solid(4, 4, colRed);
  Solid(2, 2, colBlue);
  WriteAnimation([Info(0, 0, 0, fdNone, fbSource), Info(1, 1, 0, fdNone, fbSource)], 2);
  ReadAnimation;
  AssertEquals('A composited frame is the whole canvas', 4, FFrames[1].Width);
  AssertColorsEqual('with the frame at its place', colBlue, FFrames[1].Colors[2, 2]);
  AssertColorsEqual('over the frame before', colRed, FFrames[1].Colors[0, 0]);
  AssertEquals('It is at 0,0', 0, FInfos[1].Left);
  AssertTrue('It replaces the canvas', (FInfos[1].Blend = fbSource) and (FInfos[1].Disposal = fdNone));
end;


procedure TTestAPNG.TestDisposalToTheBackground;

begin
  Solid(4, 4, colRed);
  Solid(1, 1, colBlue);
  WriteAnimation([Info(0, 0, 0, fdBackground, fbSource), Info(0, 0, 0, fdNone, fbSource)], 2);
  ReadAnimation;
  AssertColorsEqual('The area of a frame disposed to the background is transparent', colTransparent, FFrames[1].Colors[3, 3]);
  AssertColorsEqual('The next frame is drawn', colBlue, FFrames[1].Colors[0, 0]);
end;


procedure TTestAPNG.TestDisposalToThePrevious;

begin
  Solid(3, 3, colRed);
  Solid(3, 3, colGreen);
  Solid(1, 1, colBlue);
  WriteAnimation([Info(0, 0, 0, fdNone, fbSource), Info(0, 0, 0, fdPrevious, fbSource),
    Info(0, 0, 0, fdNone, fbSource)], 3);
  ReadAnimation;
  AssertColorsEqual('A frame disposed to the previous is shown', colGreen, FFrames[1].Colors[2, 2]);
  AssertColorsEqual('and taken away before the next frame', colRed, FFrames[2].Colors[2, 2]);
end;


procedure TTestAPNG.TestBlendingOver;

begin
  Solid(2, 2, colBlue);
  Solid(2, 2, FPColor($FFFF, 0, 0, $8000));
  WriteAnimation([Info(0, 0, 0, fdNone, fbSource), Info(0, 0, 0, fdNone, fbOver)], 2);
  ReadAnimation;
  AssertColorsEqual('A translucent frame blended over mixes with the one before', FPColor($8000, 0, $7FFF, $FFFF),
    FFrames[1].Colors[0, 0], 1);
end;


procedure TTestAPNG.TestADefaultImageOutsideTheAnimation;

var
  lImage: TFPMemoryImage;
  lSignature: array[0..7] of Byte;
  lControl: TBytes;

begin
  Move(Signature, lSignature, 8);
  FStream.WriteBuffer(lSignature, 8);
  AddChunk(FStream, 'IHDR', Concat(BE32(1), BE32(1), TBytes.Create(8, 6, 0, 0, 0)));
  AddChunk(FStream, 'acTL', Concat(BE32(1), BE32(0)));
  AddChunk(FStream, 'IDAT', ZRow([255, 0, 0, 255]));
  lControl := Concat(BE32(0), BE32(1), BE32(1), BE32(0), BE32(0), TBytes.Create(0, 5, 0, 0, 0, 0));
  AddChunk(FStream, 'fcTL', lControl);
  AddChunk(FStream, 'fdAT', Concat(BE32(1), ZRow([0, 0, 255, 255])));
  AddChunk(FStream, 'IEND', nil);
  FStream.Position := 0;
  ReadAnimation;
  AssertEquals('The default image is not a frame', 1, Length(FFrames));
  AssertColorsEqual('The frame is the one of fdAT', colBlue, FFrames[0].Colors[0, 0]);
  FStream.Position := 0;
  lImage := TFPMemoryImage.Create(0, 0);
  try
    lImage.LoadFromStream(FStream, FReader);
    AssertColorsEqual('Reading one image gives the default image', colRed, lImage.Colors[0, 0]);
  finally
    lImage.Free;
  end;
end;


procedure TTestAPNG.TestADelayDenominatorOfZeroIsHundredths;

var
  lSignature: array[0..7] of Byte;

begin
  Move(Signature, lSignature, 8);
  FStream.WriteBuffer(lSignature, 8);
  AddChunk(FStream, 'IHDR', Concat(BE32(1), BE32(1), TBytes.Create(8, 6, 0, 0, 0)));
  AddChunk(FStream, 'acTL', Concat(BE32(1), BE32(0)));
  AddChunk(FStream, 'fcTL', Concat(BE32(0), BE32(1), BE32(1), BE32(0), BE32(0), TBytes.Create(0, 5, 0, 0, 0, 0)));
  AddChunk(FStream, 'IDAT', ZRow([0, 255, 0, 255]));
  AddChunk(FStream, 'IEND', nil);
  FStream.Position := 0;
  ReadAnimation;
  AssertEquals('A delay of 5 over 0 is 5 hundredths', 50, FInfos[0].Delay);
  AssertColorsEqual('The default image is the first frame', colLime, FFrames[0].Colors[0, 0]);
end;


procedure TTestAPNG.TestTheFirstFrameCoversTheCanvas;

begin
  AssertRaises('A first frame smaller than the canvas raises', PNGImageException, @WriteASmallFirstFrame);
end;


procedure TTestAPNG.TestAFrameOutsideTheCanvasRaises;

begin
  AssertRaises('A frame beyond the canvas raises', PNGImageException, @WriteAFrameOutsideTheCanvas);
end;


procedure TTestAPNG.TestGIFToAPNGAndBack;

var
  lList, lBack: TFPImageList;
  lGIF: TFPWriterGIF;
  i: Integer;

begin
  lList := TFPImageList.Create;
  lBack := TFPImageList.Create;
  lGIF := TFPWriterGIF.Create;
  try
    lList.Add(CreateSolidImage(3, 2, colRed));
    lList.Add(CreateSolidImage(3, 2, colGreen));
    for i := 0 to 1 do
      lList[i].Info := Info(0, 0, (i + 1) * 100, fdNone, fbSource);
    FStream.Clear;
    lList.SaveToStream(FStream, lGIF);
    FStream.Position := 0;
    lList.LoadFromStream(FStream);
    FStream.Clear;
    lList.SaveToStream(FStream, FWriter);
    FStream.Position := 0;
    lBack.LoadFromStream(FStream);
    AssertEquals('The APNG has the frames of the GIF', 2, lBack.Count);
    AssertImagesEqual('The second frame', lList.Images[1], lBack.Images[1]);
    AssertEquals('The delay of the second frame', 200, lBack[1].Info.Delay);
    FStream.Clear;
    lBack.SaveToStream(FStream, lGIF);
    FStream.Position := 0;
    lList.LoadFromStream(FStream);
    AssertImagesEqual('The GIF written from the APNG', lBack.Images[0], lList.Images[0]);
  finally
    lGIF.Free;
    lBack.Free;
    lList.Free;
  end;
end;


procedure TTestAPNG.TestTheReaderIsReusable;

var
  i: Integer;

begin
  Solid(2, 2, colRed);
  Solid(2, 2, colBlue);
  WriteAnimation([Info(0, 0, 0, fdNone, fbSource), Info(0, 0, 0, fdNone, fbSource)], 2);
  ReadAnimation;
  for i := 0 to High(FFrames) do
    FFrames[i].Free;
  FFrames := nil;
  FInfos := nil;
  FStream.Position := 0;
  ReadAnimation;
  AssertEquals('A second read gives every frame again', 2, Length(FFrames));
  AssertColorsEqual('with its pixels', colBlue, FFrames[1].Colors[1, 1]);
end;


initialization
  RegisterTest('apng', TTestAPNG);
end.
