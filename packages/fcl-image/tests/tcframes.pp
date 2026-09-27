{
    Tests for the frame methods of readers and writers, TFPImageList, TFPFrameCompositor
    and the metadata blocks of an image.
    See the file COPYING.FPC, included in this distribution, for details.
}
unit tcframes;

{$mode objfpc}{$H+}

interface

uses sysutils, classes, types, fpcunit, testregistry, fpimage, fpimgtests, fpimagelist,
     fpreadbmp, fpwritebmp, fpreadpng, fpwritepng, fpreadjpeg, fpwritejpeg,
     fpreadgif, fpwritegif, fpreadtga, fpwritetga, fpreadtiff, fpwritetiff, fptiffcmn,
     fpreadpcx, fpwritepcx, fpreadpnm, fpwritepnm, fpreadxpm, fpwritexpm,
     fpreadqoi, fpwriteqoi, fpreadico, fpwriteico;

type
  TTestFrames = class(TTestCase)
  private
    FStream: TMemoryStream;
    FList: TFPImageList;
    FImages: array of TFPMemoryImage;
    FFileName: String;
    // Returns a frame description of aKind with aDelay milliseconds.
    function Info(aKind: TFPFrameKind; aDelay: Cardinal = 0; const aName: String = ''): TFPFrameInfo;
    // Adds a solid image of aColor to FImages and returns it.
    function Solid(aWidth, aHeight: Integer; const aColor: TFPColor): TFPMemoryImage;
    // Writes FImages as frames of aKind with aWriter to FStream.
    procedure WriteFrames(aWriter: TFPCustomImageWriter; aKind: TFPFrameKind; aLoopCount: Integer = 0);
    // Appends aImage to FStream with a writer of aWriterClass.
    procedure WriteWith(aImage: TFPCustomImage; aWriterClass: TFPCustomImageWriterClass);
    // Writes two GIF frames that play aPlays times and returns the loop field written, -1 without one.
    function GIFLoopField(aPlays: Integer; out aRead: Integer): Integer;
    procedure WriteTwoBitmaps;
    procedure ReadWithoutBegin;
    procedure BeginOnAPNG;
    procedure ReadIndexOutOfRange;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestEveryFormatReadsAndWritesOneFrame;
    procedure TestAFormatOfOneImageRejectsASecondFrame;
    procedure TestTheFrameKindsOfTheWriters;
    procedure TestReadNextFrameCreatesTheImage;
    procedure TestTheSizeIsKnownAfterTheFirstFrame;
    procedure TestReadingNeedsBeginFrames;
    procedure TestBeginFramesChecksTheFormat;
    procedure TestGIFFramesKeepDelaysAndLoopCount;
    procedure TestTheGIFLoopFieldCountsRepeats;
    procedure TestAGIFThatPlaysOnceHasNoLoopExtension;
    procedure TestAGIFThatPlaysForEver;
    procedure TestGIFFramesAreCompositedByDefault;
    procedure TestRawGIFFramesKeepTheirPlace;
    procedure TestAGIFAfterOtherDataIsRead;
    procedure TestTIFFPagesKeepTheirNames;
    procedure TestTIFFPagesAreNumbered;
    procedure TestATIFFVariantIsAThumbnail;
    procedure TestTheTIFFReaderCreatesAnImage;
    procedure TestICOFramesAreItsSizes;
    procedure TestTheListLoadsAnAnimation;
    procedure TestTheListConvertsGIFToTIFF;
    procedure TestTheListConvertsTIFFToGIF;
    procedure TestTheListSavesAndLoadsByFileName;
    procedure TestTheListRejectsAnUnknownExtension;
    procedure TestTheListFreesOnlyWhatItOwns;
    procedure TestTheListRangeIsChecked;
    procedure TestAlphaOver;
    procedure TestCompositorKeepsWithoutDisposal;
    procedure TestCompositorClearsToTheBackground;
    procedure TestCompositorRestoresThePrevious;
    procedure TestCompositorBlendsOver;
    procedure TestCompositorReplacesWithSourceBlend;
    procedure TestCompositorClipsToTheCanvas;
    procedure TestMetadataIsStoredByName;
    procedure TestMetadataIsCopied;
    procedure TestEmptyMetadataRemovesTheBlock;
    procedure TestAssignCopiesMetadata;
  end;

implementation

procedure TTestFrames.SetUp;

begin
  inherited SetUp;
  FStream := TMemoryStream.Create;
  FList := TFPImageList.Create;
end;


procedure TTestFrames.TearDown;

var
  i: Integer;

begin
  for i := 0 to High(FImages) do
    FImages[i].Free;
  FImages := nil;
  FreeAndNil(FList);
  FreeAndNil(FStream);
  if (FFileName <> '') and FileExists(FFileName) then
    DeleteFile(FFileName);
  FFileName := '';
  inherited TearDown;
end;


function TTestFrames.Info(aKind: TFPFrameKind; aDelay: Cardinal; const aName: String): TFPFrameInfo;

begin
  Result := DefaultFrameInfo;
  Result.Kind := aKind;
  Result.Delay := aDelay;
  Result.Name := aName;
end;


function TTestFrames.Solid(aWidth, aHeight: Integer; const aColor: TFPColor): TFPMemoryImage;

begin
  Result := CreateSolidImage(aWidth, aHeight, aColor);
  SetLength(FImages, Length(FImages) + 1);
  FImages[High(FImages)] := Result;
end;


procedure TTestFrames.WriteFrames(aWriter: TFPCustomImageWriter; aKind: TFPFrameKind; aLoopCount: Integer);

var
  lInfo: TFPFramesInfo;
  i: Integer;

begin
  lInfo := DefaultFramesInfo;
  lInfo.FrameCount := Length(FImages);
  lInfo.LoopCount := aLoopCount;
  aWriter.BeginFrames(FStream, lInfo);
  for i := 0 to High(FImages) do
    aWriter.WriteNextFrame(FImages[i], Info(aKind, (i + 1) * 100, 'page ' + IntToStr(i)));
  aWriter.EndFrames;
  FStream.Position := 0;
end;


procedure TTestFrames.WriteWith(aImage: TFPCustomImage; aWriterClass: TFPCustomImageWriterClass);

var
  lWriter: TFPCustomImageWriter;

begin
  lWriter := aWriterClass.Create;
  try
    WriteImage(aImage, lWriter, FStream);
  finally
    lWriter.Free;
  end;
end;


function TTestFrames.GIFLoopField(aPlays: Integer; out aRead: Integer): Integer;

var
  lWriter: TFPWriterGIF;
  lReader: TFPReaderGif;
  lInfo: TFPFrameInfo;
  lText: String;
  lPos: Integer;

begin
  Solid(2, 2, colRed);
  Solid(2, 2, colBlue);
  lWriter := TFPWriterGIF.Create;
  lReader := TFPReaderGif.Create;
  try
    WriteFrames(lWriter, fkAnimation, aPlays);
    SetString(lText, PChar(FStream.Memory), FStream.Size);
    lPos := Pos('NETSCAPE2.0', lText);
    if lPos = 0 then
      Result := -1
    else
      Result := Ord(lText[lPos + 13]) or (Ord(lText[lPos + 14]) shl 8);
    lReader.BeginFrames(FStream);
    lReader.ReadNextFrame(lInfo).Free;
    aRead := lReader.FramesInfo.LoopCount;
    lReader.EndFrames;
  finally
    lReader.Free;
    lWriter.Free;
  end;
end;


procedure TTestFrames.WriteTwoBitmaps;

var
  lWriter: TFPWriterBMP;

begin
  lWriter := TFPWriterBMP.Create;
  try
    Solid(4, 4, colRed);
    Solid(4, 4, colBlue);
    WriteFrames(lWriter, fkPage);
  finally
    lWriter.Free;
  end;
end;


procedure TTestFrames.ReadWithoutBegin;

var
  lReader: TFPReaderBMP;
  lInfo: TFPFrameInfo;

begin
  lReader := TFPReaderBMP.Create;
  try
    lReader.ReadNextFrame(lInfo).Free;
  finally
    lReader.Free;
  end;
end;


procedure TTestFrames.BeginOnAPNG;

var
  lReader: TFPReaderBMP;
  lImage: TFPMemoryImage;

begin
  lImage := CreateGradientImage(4, 4);
  lReader := TFPReaderBMP.Create;
  try
    WriteWith(lImage, TFPWriterPNG);
    FStream.Position := 0;
    lReader.BeginFrames(FStream);
  finally
    lReader.Free;
    lImage.Free;
  end;
end;


procedure TTestFrames.ReadIndexOutOfRange;

begin
  FList.Frames[0].Free;
end;


procedure TTestFrames.TestEveryFormatReadsAndWritesOneFrame;

var
  lName: String;
  lWriter: TFPCustomImageWriter;
  lReader: TFPCustomImageReader;
  lImage, lRead: TFPCustomImage;
  lInfo: TFPFrameInfo;
  lFrames: TFPFramesInfo;
  i, lTested: Integer;

begin
  lTested := 0;
  lImage := CreateGradientImage(8, 6);
  try
    for i := 0 to ImageHandlers.Count - 1 do
      begin
      lName := ImageHandlers.TypeNames[i];
      if (ImageHandlers.ImageReader[lName] = nil) or (ImageHandlers.ImageWriter[lName] = nil) then
        continue;
      lWriter := ImageHandlers.ImageWriter[lName].Create;
      lReader := ImageHandlers.ImageReader[lName].Create;
      lRead := nil;
      try
        FStream.Clear;
        lFrames := DefaultFramesInfo;
        lFrames.FrameCount := 1;
        lWriter.BeginFrames(FStream, lFrames);
        lWriter.WriteNextFrame(lImage, DefaultFrameInfo);
        lWriter.EndFrames;
        AssertEquals(lName + ': one frame written', 1, lWriter.FramesWritten);
        FStream.Position := 0;
        lReader.BeginFrames(FStream);
        lRead := lReader.ReadNextFrame(lInfo);
        AssertNotNull(lName + ': the frame is read', lRead);
        AssertEquals(lName + ': width of the frame', 8, lRead.Width);
        AssertEquals(lName + ': height of the frame', 6, lRead.Height);
        AssertNull(lName + ': there is no second frame', lReader.ReadNextFrame(lInfo));
        AssertEquals(lName + ': one frame read', 1, lReader.FramesRead);
        lReader.EndFrames;
        Inc(lTested);
      finally
        lRead.Free;
        lReader.Free;
        lWriter.Free;
      end;
      end;
  finally
    lImage.Free;
  end;
  AssertTrue('Every format with a reader and a writer is tested', lTested >= 12);
end;


procedure TTestFrames.TestAFormatOfOneImageRejectsASecondFrame;

begin
  AssertRaises('A second frame for a format of one image raises', FPImageException, @WriteTwoBitmaps);
end;


procedure TTestFrames.TestTheFrameKindsOfTheWriters;

begin
  AssertTrue('A BMP holds one image', TFPWriterBMP.FrameKinds = []);
  AssertTrue('A GIF holds an animation', TFPWriterGIF.FrameKinds = [fkAnimation]);
  AssertTrue('A TIFF holds pages and thumbnails', TFPWriterTiff.FrameKinds = [fkPage, fkVariant]);
  AssertTrue('An icon holds sizes', TFPWriterICO.FrameKinds = [fkVariant]);
end;


procedure TTestFrames.TestReadNextFrameCreatesTheImage;

var
  lReader: TFPReaderBMP;
  lImage: TFPCustomImage;
  lInfo: TFPFrameInfo;

begin
  WriteWith(Solid(3, 2, colGreen), TFPWriterBMP);
  FStream.Position := 0;
  lReader := TFPReaderBMP.Create;
  try
    AssertEquals('A BMP has one frame', 1, lReader.BeginFrames(FStream).FrameCount);
    lImage := lReader.ReadNextFrame(lInfo);
    try
      AssertEquals('The image is of the default class', 'TFPMemoryImage', lImage.ClassName);
      AssertColorsEqual('with the pixels of the file', colGreen, lImage.Colors[1, 1]);
      AssertTrue('The frame is a page', lInfo.Kind = fkPage);
    finally
      lImage.Free;
    end;
    lReader.EndFrames;
  finally
    lReader.Free;
  end;
end;


procedure TTestFrames.TestTheSizeIsKnownAfterTheFirstFrame;

var
  lReader: TFPReaderBMP;
  lInfo: TFPFrameInfo;

begin
  WriteWith(Solid(7, 5, colGreen), TFPWriterBMP);
  FStream.Position := 0;
  lReader := TFPReaderBMP.Create;
  try
    AssertEquals('The width is not known before reading', 0, lReader.BeginFrames(FStream).Width);
    lReader.ReadNextFrame(lInfo).Free;
    AssertEquals('The width is that of the first frame', 7, lReader.FramesInfo.Width);
    AssertEquals('The height is that of the first frame', 5, lReader.FramesInfo.Height);
    lReader.EndFrames;
  finally
    lReader.Free;
  end;
end;


procedure TTestFrames.TestReadingNeedsBeginFrames;

begin
  AssertRaises('ReadNextFrame without BeginFrames raises', FPImageException, @ReadWithoutBegin);
end;


procedure TTestFrames.TestBeginFramesChecksTheFormat;

begin
  AssertRaises('BeginFrames on a stream of another format raises', FPImageException, @BeginOnAPNG);
end;


procedure TTestFrames.TestGIFFramesKeepDelaysAndLoopCount;

var
  lWriter: TFPWriterGIF;
  lReader: TFPReaderGif;
  lImage: TFPCustomImage;
  lInfo: TFPFrameInfo;
  i: Integer;

begin
  Solid(6, 4, colRed);
  Solid(6, 4, colGreen);
  Solid(6, 4, colBlue);
  lWriter := TFPWriterGIF.Create;
  lReader := TFPReaderGif.Create;
  try
    WriteFrames(lWriter, fkAnimation, 2);
    AssertEquals('The writer keeps its own loop count', 0, lWriter.LoopCount);
    AssertEquals('The canvas is the screen of the GIF', 6, lReader.BeginFrames(FStream).Width);
    for i := 0 to 2 do
      begin
      lImage := lReader.ReadNextFrame(lInfo);
      try
        AssertNotNull('Frame ' + IntToStr(i) + ' is read', lImage);
        AssertTrue('Frame ' + IntToStr(i) + ' is an animation frame', lInfo.Kind = fkAnimation);
        AssertEquals('The delay of frame ' + IntToStr(i), (i + 1) * 100, lInfo.Delay);
        AssertColorsEqual('The pixels of frame ' + IntToStr(i), FImages[i].Colors[2, 2], lImage.Colors[2, 2]);
      finally
        lImage.Free;
      end;
      end;
    AssertNull('There are three frames', lReader.ReadNextFrame(lInfo));
    AssertEquals('The loop count of the file', 2, lReader.FramesInfo.LoopCount);
    lReader.EndFrames;
  finally
    lReader.Free;
    lWriter.Free;
  end;
end;


procedure TTestFrames.TestTheGIFLoopFieldCountsRepeats;

var
  lRead: Integer;

begin
  AssertEquals('Four plays are written as three repeats', 3, GIFLoopField(4, lRead));
  AssertEquals('Three repeats are read as four plays', 4, lRead);
end;


procedure TTestFrames.TestAGIFThatPlaysOnceHasNoLoopExtension;

var
  lRead: Integer;

begin
  AssertEquals('One play is written without a loop extension', -1, GIFLoopField(1, lRead));
  AssertEquals('A GIF without a loop extension plays once', 1, lRead);
end;


procedure TTestFrames.TestAGIFThatPlaysForEver;

var
  lRead: Integer;

begin
  AssertEquals('Playing for ever is written as a loop field of 0', 0, GIFLoopField(0, lRead));
  AssertEquals('and read as 0', 0, lRead);
end;


procedure TTestFrames.TestGIFFramesAreCompositedByDefault;

var
  lReader: TFPReaderGif;

begin
  lReader := TFPReaderGif.Create;
  try
    AssertTrue('A reader composites by default', lReader.Composite);
  finally
    lReader.Free;
  end;
end;


procedure TTestFrames.TestRawGIFFramesKeepTheirPlace;

var
  lReader: TFPReaderGif;
  lImage: TFPCustomImage;
  lInfo: TFPFrameInfo;

begin
  FStream.Free;
  { a 4x4 screen and one 2x1 frame at (1,2) of colour 1 of a black and white table }
  FStream := BytesStream([Ord('G'), Ord('I'), Ord('F'), Ord('8'), Ord('9'), Ord('a'), 4, 0, 4, 0, $80, 0, 0,
    0, 0, 0, 255, 255, 255,
    $2C, 1, 0, 2, 0, 2, 0, 1, 0, 0,
    2, 2, $4C, $0A, 0,
    $3B]);
  lReader := TFPReaderGif.Create;
  try
    lReader.Composite := False;
    lReader.BeginFrames(FStream);
    lImage := lReader.ReadNextFrame(lInfo);
    try
      AssertEquals('A raw frame has its own width', 2, lImage.Width);
      AssertEquals('Left of the frame', 1, lInfo.Left);
      AssertEquals('Top of the frame', 2, lInfo.Top);
      AssertColorsEqual('The pixels of the frame', colWhite, lImage.Colors[0, 0]);
    finally
      lImage.Free;
    end;
    lReader.EndFrames;
  finally
    lReader.Free;
  end;
end;


procedure TTestFrames.TestAGIFAfterOtherDataIsRead;

var
  lImage, lRead: TFPMemoryImage;
  lReader: TFPReaderGif;

begin
  lImage := Solid(5, 3, colRed);
  FStream.WriteBuffer(PChar('prefix')^, 6);
  WriteWith(lImage, TFPWriterGIF);
  FStream.Position := 6;
  lRead := TFPMemoryImage.Create(0, 0);
  lReader := TFPReaderGif.Create;
  try
    lRead.LoadFromStream(FStream, lReader);
    AssertImagesEqual('A GIF after other data is read from where the stream stands', lImage, lRead);
  finally
    lReader.Free;
    lRead.Free;
  end;
end;


procedure TTestFrames.TestTIFFPagesKeepTheirNames;

var
  lWriter: TFPWriterTiff;
  lReader: TFPReaderTiff;
  lImage: TFPCustomImage;
  lInfo: TFPFrameInfo;
  i: Integer;

begin
  Solid(4, 3, colRed);
  Solid(5, 2, colBlue);
  lWriter := TFPWriterTiff.Create;
  lReader := TFPReaderTiff.Create;
  try
    WriteFrames(lWriter, fkPage);
    AssertEquals('The writer leaves the image without the tags it added', '', FImages[0].Extra[TiffPageName]);
    AssertEquals('Two pages are found before reading them', 2, lReader.BeginFrames(FStream).FrameCount);
    for i := 0 to 1 do
      begin
      lImage := lReader.ReadNextFrame(lInfo);
      try
        AssertTrue('Frame ' + IntToStr(i) + ' is a page', lInfo.Kind = fkPage);
        AssertEquals('The name of page ' + IntToStr(i), 'page ' + IntToStr(i), lInfo.Name);
        AssertImagesEqual('The pixels of page ' + IntToStr(i), FImages[i], lImage);
      finally
        lImage.Free;
      end;
      end;
    AssertNull('There are two pages', lReader.ReadNextFrame(lInfo));
    lReader.EndFrames;
  finally
    lReader.Free;
    lWriter.Free;
  end;
end;


procedure TTestFrames.TestTIFFPagesAreNumbered;

var
  lWriter: TFPWriterTiff;
  lReader: TFPReaderTiff;

begin
  Solid(2, 2, colRed);
  Solid(2, 2, colBlue);
  Solid(2, 2, colGreen);
  lWriter := TFPWriterTiff.Create;
  lReader := TFPReaderTiff.Create;
  try
    WriteFrames(lWriter, fkPage);
    lReader.LoadFromStream(FStream);
    AssertEquals('Three pages', 3, lReader.ImageCount);
    AssertEquals('The number of the second page', 1, lReader.Images[1].PageNumber);
    AssertEquals('The page count', 3, lReader.Images[2].PageCount);
  finally
    lReader.Free;
    lWriter.Free;
  end;
end;


procedure TTestFrames.TestATIFFVariantIsAThumbnail;

var
  lWriter: TFPWriterTiff;
  lReader: TFPReaderTiff;
  lInfo: TFPFramesInfo;
  lFrame: TFPFrameInfo;
  lImage: TFPCustomImage;

begin
  lWriter := TFPWriterTiff.Create;
  lReader := TFPReaderTiff.Create;
  try
    lInfo := DefaultFramesInfo;
    lWriter.BeginFrames(FStream, lInfo);
    lWriter.WriteNextFrame(Solid(8, 8, colRed), Info(fkPage));
    lWriter.WriteNextFrame(Solid(2, 2, colRed), Info(fkVariant));
    lWriter.EndFrames;
    FStream.Position := 0;
    lReader.BeginFrames(FStream);
    lReader.ReadNextFrame(lFrame).Free;
    AssertTrue('The first frame is a page', lFrame.Kind = fkPage);
    lImage := lReader.ReadNextFrame(lFrame);
    try
      AssertTrue('A variant comes back as a variant', lFrame.Kind = fkVariant);
      AssertEquals('with its own size', 2, lImage.Width);
      AssertEquals('It is marked as a thumbnail', '1', lImage.Extra[TiffIsThumbnail]);
    finally
      lImage.Free;
    end;
    lReader.EndFrames;
  finally
    lReader.Free;
    lWriter.Free;
  end;
end;


procedure TTestFrames.TestTheTIFFReaderCreatesAnImage;

var
  lReader: TFPReaderTiff;
  lImage: TFPCustomImage;

begin
  WriteWith(Solid(3, 3, colBlue), TFPWriterTiff);
  FStream.Position := 0;
  lReader := TFPReaderTiff.Create;
  try
    lImage := lReader.ImageRead(FStream, nil);
    try
      AssertEquals('ImageRead without an image creates one of the default class', 'TFPMemoryImage', lImage.ClassName);
    finally
      lImage.Free;
    end;
  finally
    lReader.Free;
  end;
end;


procedure TTestFrames.TestICOFramesAreItsSizes;

var
  lWriter: TFPWriterICO;
  lReader: TFPReaderICO;
  lImage: TFPCustomImage;
  lInfo: TFPFrameInfo;

begin
  Solid(16, 16, colRed);
  Solid(32, 32, colBlue);
  lWriter := TFPWriterICO.Create;
  lReader := TFPReaderICO.Create;
  try
    WriteFrames(lWriter, fkVariant);
    AssertEquals('Two entries', 2, lReader.BeginFrames(FStream).FrameCount);
    lImage := lReader.ReadNextFrame(lInfo);
    try
      AssertTrue('An entry is a variant', lInfo.Kind = fkVariant);
      AssertEquals('The first entry', 16, lImage.Width);
    finally
      lImage.Free;
    end;
    lImage := lReader.ReadNextFrame(lInfo);
    try
      AssertEquals('The second entry', 32, lImage.Width);
      AssertColorsEqual('with its pixels', colBlue, lImage.Colors[31, 31]);
    finally
      lImage.Free;
    end;
    AssertNull('There are two entries', lReader.ReadNextFrame(lInfo));
    lReader.EndFrames;
    AssertEquals('The stream is left at the end of the icon', FStream.Size, FStream.Position);
  finally
    lReader.Free;
    lWriter.Free;
  end;
end;


procedure TTestFrames.TestTheListLoadsAnAnimation;

var
  lWriter: TFPWriterGIF;

begin
  Solid(4, 4, colRed);
  Solid(4, 4, colGreen);
  lWriter := TFPWriterGIF.Create;
  try
    WriteFrames(lWriter, fkAnimation, 3);
  finally
    lWriter.Free;
  end;
  FList.LoadFromStream(FStream);
  AssertEquals('The list holds every frame', 2, FList.Count);
  AssertEquals('and the loop count', 3, FList.Info.LoopCount);
  AssertEquals('and the frame count', 2, FList.Info.FrameCount);
  AssertEquals('The delay of the second frame', 200, FList[1].Info.Delay);
  AssertColorsEqual('The pixels of the second frame', colGreen, FList.Images[1].Colors[0, 0]);
end;


procedure TTestFrames.TestTheListConvertsGIFToTIFF;

var
  lWriter: TFPWriterGIF;
  lTiff: TFPWriterTiff;
  lBack: TFPImageList;

begin
  Solid(4, 4, colRed);
  Solid(4, 4, colBlue);
  lWriter := TFPWriterGIF.Create;
  try
    WriteFrames(lWriter, fkAnimation);
  finally
    lWriter.Free;
  end;
  FList.LoadFromStream(FStream);
  lTiff := TFPWriterTiff.Create;
  lBack := TFPImageList.Create;
  try
    FStream.Clear;
    FList.SaveToStream(FStream, lTiff);
    FStream.Position := 0;
    lBack.LoadFromStream(FStream);
    AssertEquals('The TIFF has a page per frame', 2, lBack.Count);
    AssertImagesEqual('The first frame becomes the first page', FList.Images[0], lBack.Images[0]);
    AssertImagesEqual('The second frame becomes the second page', FList.Images[1], lBack.Images[1]);
  finally
    lBack.Free;
    lTiff.Free;
  end;
end;


procedure TTestFrames.TestTheListConvertsTIFFToGIF;

var
  lTiff: TFPWriterTiff;
  lGIF: TFPWriterGIF;

begin
  Solid(3, 3, colRed);
  Solid(3, 3, colGreen);
  Solid(3, 3, colBlue);
  lTiff := TFPWriterTiff.Create;
  try
    WriteFrames(lTiff, fkPage);
  finally
    lTiff.Free;
  end;
  FList.LoadFromStream(FStream);
  lGIF := TFPWriterGIF.Create;
  try
    FStream.Clear;
    FList.SaveToStream(FStream, lGIF);
  finally
    lGIF.Free;
  end;
  FStream.Position := 0;
  FList.LoadFromStream(FStream);
  AssertEquals('The GIF has a frame per page', 3, FList.Count);
  AssertImagesEqual('The last page becomes the last frame', FImages[2], FList.Images[2]);
end;


procedure TTestFrames.TestTheListSavesAndLoadsByFileName;

begin
  FFileName := IncludeTrailingPathDelimiter(GetTempDir(False)) + 'fpimgframes-' + IntToStr(GetProcessID) + '.tif';
  FList.OwnsImages := False;
  FList.Add(Solid(4, 2, colRed), Info(fkPage));
  FList.Add(Solid(2, 4, colBlue), Info(fkPage));
  AssertTrue('Saving by a known extension succeeds', FList.SaveToFile(FFileName));
  FList.Clear;
  FList.OwnsImages := True;
  AssertTrue('Loading by the extension succeeds', FList.LoadFromFile(FFileName));
  AssertEquals('Both pages are loaded', 2, FList.Count);
  AssertEquals('The second page', 4, FList.Images[1].Height);
end;


procedure TTestFrames.TestTheListRejectsAnUnknownExtension;

begin
  FList.OwnsImages := False;
  FList.Add(Solid(4, 2, colRed));
  AssertFalse('Saving by an unknown extension reports False', FList.SaveToFile('frames.unknown'));
end;


procedure TTestFrames.TestTheListFreesOnlyWhatItOwns;

var
  lOwned: TFPMemoryImage;

begin
  FList.OwnsImages := False;
  FList.Add(Solid(2, 2, colRed));
  FList.OwnsImages := True;
  lOwned := CreateSolidImage(2, 2, colBlue);
  FList.Add(lOwned);
  AssertFalse('An image added while not owning is not owned', FList[0].OwnsImage);
  AssertTrue('An image added while owning is owned', FList[1].OwnsImage);
  FList.Delete(1);
  AssertEquals('Delete removes the frame', 1, FList.Count);
  FList.Clear;
  AssertEquals('Clear removes every frame', 0, FList.Count);
  AssertEquals('The image not owned is left alone', 2, FImages[0].Width);
end;


procedure TTestFrames.TestTheListRangeIsChecked;

begin
  AssertRaises('A frame index out of range raises', FPImageException, @ReadIndexOutOfRange);
end;


procedure TTestFrames.TestAlphaOver;

var
  lResult: TFPColor;

begin
  AssertColorsEqual('An opaque colour hides what is under it', colRed, AlphaOver(colRed, colBlue));
  AssertColorsEqual('A transparent colour shows what is under it', colBlue, AlphaOver(colTransparent, colBlue));
  lResult := AlphaOver(FPColor($FFFF, 0, 0, $8000), colBlue);
  AssertColorsEqual('Half red over blue is half of each, opaque', FPColor($8000, 0, $7FFF, $FFFF), lResult, 1);
  lResult := AlphaOver(FPColor($FFFF, 0, 0, $8000), colTransparent);
  AssertColorsEqual('Half red over nothing stays half red', FPColor($FFFF, 0, 0, $8000), lResult, 1);
end;


procedure TTestFrames.TestCompositorKeepsWithoutDisposal;

var
  lCompositor: TFPFrameCompositor;
  lInfo: TFPFrameInfo;

begin
  lCompositor := TFPFrameCompositor.Create(4, 4, colTransparent);
  try
    lInfo := Info(fkAnimation);
    lCompositor.Add(Solid(4, 4, colRed), lInfo, nil);
    lInfo.Left := 2;
    lCompositor.Add(Solid(2, 2, colBlue), lInfo, nil);
    AssertColorsEqual('The first frame stays where the second does not cover it', colRed, lCompositor.Canvas.Colors[0, 0]);
    AssertColorsEqual('The second frame is drawn at its place', colBlue, lCompositor.Canvas.Colors[3, 1]);
  finally
    lCompositor.Free;
  end;
end;


procedure TTestFrames.TestCompositorClearsToTheBackground;

var
  lCompositor: TFPFrameCompositor;
  lInfo: TFPFrameInfo;
  lResult: TFPMemoryImage;

begin
  lCompositor := TFPFrameCompositor.Create(4, 4, colGreen);
  lResult := TFPMemoryImage.Create(0, 0);
  try
    lInfo := Info(fkAnimation);
    lInfo.Disposal := fdBackground;
    lInfo.Left := 1;
    lInfo.Top := 1;
    lCompositor.Add(Solid(2, 2, colRed), lInfo, nil);
    lInfo.Disposal := fdNone;
    lInfo.Left := 0;
    lInfo.Top := 0;
    lCompositor.Add(Solid(1, 1, colBlue), lInfo, lResult);
    AssertColorsEqual('The area of a frame disposed to the background is cleared', colGreen, lResult.Colors[2, 2]);
    AssertColorsEqual('The canvas starts as the background', colGreen, lResult.Colors[3, 0]);
    AssertColorsEqual('The next frame is drawn', colBlue, lResult.Colors[0, 0]);
    AssertEquals('The result is the whole canvas', 4, lResult.Width);
  finally
    lResult.Free;
    lCompositor.Free;
  end;
end;


procedure TTestFrames.TestCompositorRestoresThePrevious;

var
  lCompositor: TFPFrameCompositor;
  lInfo: TFPFrameInfo;

begin
  lCompositor := TFPFrameCompositor.Create(2, 2, colTransparent);
  try
    lInfo := Info(fkAnimation);
    lCompositor.Add(Solid(2, 2, colRed), lInfo, nil);
    lInfo.Disposal := fdPrevious;
    lCompositor.Add(Solid(2, 2, colBlue), lInfo, nil);
    AssertColorsEqual('A frame is drawn before its disposal', colBlue, lCompositor.Canvas.Colors[0, 0]);
    lInfo.Disposal := fdNone;
    lCompositor.Add(Solid(1, 1, colGreen), lInfo, nil);
    AssertColorsEqual('Disposal to the previous restores the canvas before the frame', colRed, lCompositor.Canvas.Colors[1, 1]);
    AssertColorsEqual('under the next frame', colGreen, lCompositor.Canvas.Colors[0, 0]);
  finally
    lCompositor.Free;
  end;
end;


procedure TTestFrames.TestCompositorBlendsOver;

var
  lCompositor: TFPFrameCompositor;
  lInfo: TFPFrameInfo;

begin
  lCompositor := TFPFrameCompositor.Create(1, 1, colTransparent);
  try
    lInfo := Info(fkAnimation);
    lCompositor.Add(Solid(1, 1, colBlue), lInfo, nil);
    lInfo.Blend := fbOver;
    lCompositor.Add(Solid(1, 1, FPColor($FFFF, 0, 0, $8000)), lInfo, nil);
    AssertColorsEqual('A translucent frame blended over mixes with the canvas', FPColor($8000, 0, $7FFF, $FFFF),
      lCompositor.Canvas.Colors[0, 0], 1);
  finally
    lCompositor.Free;
  end;
end;


procedure TTestFrames.TestCompositorReplacesWithSourceBlend;

var
  lCompositor: TFPFrameCompositor;
  lInfo: TFPFrameInfo;

begin
  lCompositor := TFPFrameCompositor.Create(1, 1, colTransparent);
  try
    lInfo := Info(fkAnimation);
    lCompositor.Add(Solid(1, 1, colBlue), lInfo, nil);
    lCompositor.Add(Solid(1, 1, FPColor($FFFF, 0, 0, $8000)), lInfo, nil);
    AssertColorsEqual('A frame of source blend replaces the canvas', FPColor($FFFF, 0, 0, $8000),
      lCompositor.Canvas.Colors[0, 0]);
  finally
    lCompositor.Free;
  end;
end;


procedure TTestFrames.TestCompositorClipsToTheCanvas;

var
  lCompositor: TFPFrameCompositor;
  lInfo: TFPFrameInfo;

begin
  lCompositor := TFPFrameCompositor.Create(3, 3, colTransparent);
  try
    lInfo := Info(fkAnimation);
    lInfo.Left := 2;
    lInfo.Top := -1;
    lInfo.Disposal := fdBackground;
    lCompositor.Add(Solid(3, 3, colRed), lInfo, nil);
    AssertColorsEqual('The part of a frame on the canvas is drawn', colRed, lCompositor.Canvas.Colors[2, 1]);
    AssertColorsEqual('The rest of the canvas is untouched', colTransparent, lCompositor.Canvas.Colors[1, 1]);
    lInfo.Left := 0;
    lInfo.Top := 0;
    lCompositor.Add(Solid(1, 1, colBlue), lInfo, nil);
    AssertColorsEqual('Disposal clears only what is on the canvas', colTransparent, lCompositor.Canvas.Colors[2, 1]);
  finally
    lCompositor.Free;
  end;
end;


procedure TTestFrames.TestMetadataIsStoredByName;

var
  lImage: TFPMemoryImage;

begin
  lImage := Solid(1, 1, colRed);
  AssertEquals('A new image has no metadata', 0, lImage.MetadataCount);
  lImage.Metadata[MetaExif] := TBytes.Create(1, 2, 3);
  lImage.Metadata[MetaICC] := TBytes.Create(4);
  AssertEquals('Two blocks', 2, lImage.MetadataCount);
  AssertEquals('The name of the first block', MetaExif, lImage.MetadataName[0]);
  AssertEquals('A block keeps its bytes', 3, lImage.Metadata['EXIF'][2]);
  AssertEquals('An absent block is empty', 0, Length(lImage.Metadata[MetaXMP]));
  lImage.Metadata[MetaExif] := TBytes.Create(9);
  AssertEquals('Setting a block again replaces it', 1, Length(lImage.Metadata[MetaExif]));
  lImage.RemoveMetadata(MetaICC);
  AssertEquals('RemoveMetadata removes the block', 1, lImage.MetadataCount);
  lImage.ClearMetadata;
  AssertEquals('ClearMetadata removes every block', 0, lImage.MetadataCount);
end;


procedure TTestFrames.TestMetadataIsCopied;

var
  lImage: TFPMemoryImage;
  lBytes: TBytes;

begin
  lImage := Solid(1, 1, colRed);
  lBytes := TBytes.Create(1, 2);
  lImage.Metadata[MetaXMP] := lBytes;
  lBytes[0] := 7;
  AssertEquals('Changing the bytes given leaves the block alone', 1, lImage.Metadata[MetaXMP][0]);
  lBytes := lImage.Metadata[MetaXMP];
  lBytes[1] := 7;
  AssertEquals('Changing the bytes read leaves the block alone', 2, lImage.Metadata[MetaXMP][1]);
end;


procedure TTestFrames.TestEmptyMetadataRemovesTheBlock;

var
  lImage: TFPMemoryImage;

begin
  lImage := Solid(1, 1, colRed);
  lImage.Metadata[MetaICC] := TBytes.Create(1);
  lImage.Metadata[MetaICC] := nil;
  AssertEquals('Setting a block empty removes it', 0, lImage.MetadataCount);
end;


procedure TTestFrames.TestAssignCopiesMetadata;

var
  lImage, lCopy: TFPMemoryImage;

begin
  lImage := Solid(1, 1, colRed);
  lImage.Metadata[MetaExif] := TBytes.Create(5, 6);
  lCopy := Solid(2, 2, colBlue);
  lCopy.Metadata[MetaICC] := TBytes.Create(1);
  lCopy.Assign(lImage);
  AssertEquals('Assign copies the metadata', 6, lCopy.Metadata[MetaExif][1]);
  AssertEquals('and drops what the image had', 0, Length(lCopy.Metadata[MetaICC]));
end;


initialization
  RegisterTest('frames', TTestFrames);
end.
