{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by the Free Pascal development team

    Tests for reading the frames of an animated GIF.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit tcgifread;

{$mode objfpc}{$H+}

interface

uses sysutils, classes, fpcunit, testregistry, fpimage, fpreadgif,
     fpwritegif;

type
  { The frames of an animation, written by the writer of the package and
    read back by the reader of it. }
  TTestGIFFrames = class(TTestCase)
  private
    FFrames: array of TFPCustomImage;
    FReader: TFPReaderGif;
    FStream: TMemoryStream;
    FMade: Integer;
    // Makes that many frames of that size, each of one colour.
    procedure BuildFrames(aCount, aWidth, aHeight: Integer);
    // Writes the frames held, at the delays given.
    procedure Write(const aDelays: array of Word; aLoopCount: Word);
    // Reads what was written back into the reader.
    procedure ReadBack;
    // Hands the reader an image of its own for every frame.
    procedure GiveImage(Sender: TFPReaderGif; var NewImage: TFPCustomImage);
    // The colour of the pixel, as a triple of 8-bit channels.
    function ColorAt(aFrame, aX, aY: Integer): String;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestEveryFrameIsRead;
    procedure TestEveryFrameHasItsOwnPicture;
    procedure TestTheDelaysAreRead;
    procedure TestHowOftenToPlayIsRead;
    procedure TestNothingSaidAboutPlayingIsNought;
    procedure TestTheDisposalOfAFrameIsRead;
    procedure TestAFrameIsTheSizeOfTheScreen;
    procedure TestASmallerFrameIsLaidOverTheOneBefore;
    procedure TestAHoleInAFrameShowsWhatTheDisposalLeaves;
    procedure TestTheReaderOwnsTheImagesItMakes;
    procedure TestImagesGivenToItAreNotOwnedByIt;
    procedure TestClearForgetsTheFramesRead;
    procedure TestReadingTwiceForgetsTheFirstRead;
    procedure TestWithoutCompositingAFrameIsWhatTheFileHolds;
  end;

  { Reading one image, which is what every reader of the package does. }
  TTestGIFSingleImage = class(TTestCase)
  private
    FImage: TFPMemoryImage;
    FReader: TFPReaderGif;
    FStream: TMemoryStream;
    // Puts a GIF of one frame at that place of a screen of that size in
    // FStream, written byte by byte rather than by the writer, which
    // places every frame at the corner.
    procedure BuildPlacedFrame(aScreenWidth, aScreenHeight, aLeft,
      aTop: Integer);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestOneImageStillReadsIntoTheImageGiven;
    procedure TestTheFrameOfASingleImageIsThereToBeRead;
    procedure TestAPlacedFrameIsReadOntoTheScreen;
    procedure TestAPlacedFrameWithoutCompositingKeepsItsOwnSize;
  end;

implementation

// A colour of 8-bit channels, opaque.
function RGB(aRed, aGreen, aBlue: Byte): TFPColor;

begin
  Result.Red := aRed * 257;
  Result.Green := aGreen * 257;
  Result.Blue := aBlue * 257;
  Result.Alpha := alphaOpaque;
end;


// The colour as a triple of 8-bit channels and its alpha.
function ColorText(const aColor: TFPColor): String;

begin
  Result := Format('%d,%d,%d,%d', [aColor.Red shr 8, aColor.Green shr 8,
    aColor.Blue shr 8, aColor.Alpha shr 8]);
end;


{ TTestGIFFrames }

procedure TTestGIFFrames.SetUp;

begin
  inherited SetUp;
  FReader := TFPReaderGif.Create;
  FStream := TMemoryStream.Create;
end;


procedure TTestGIFFrames.TearDown;

var
  I: Integer;

begin
  FreeAndNil(FReader);
  FreeAndNil(FStream);
  for I := 0 to High(FFrames) do
    FFrames[I].Free;
  FFrames := nil;
  inherited TearDown;
end;


procedure TTestGIFFrames.BuildFrames(aCount, aWidth, aHeight: Integer);

const
  Colors: array[0..2] of array[0..2] of Byte =
    ((255, 0, 0), (0, 255, 0), (0, 0, 255));

var
  I, X, Y: Integer;

begin
  for I := 0 to High(FFrames) do
    FFrames[I].Free;
  SetLength(FFrames, aCount);
  for I := 0 to aCount - 1 do
    begin
    FFrames[I] := TFPMemoryImage.Create(aWidth, aHeight);
    for Y := 0 to aHeight - 1 do
      for X := 0 to aWidth - 1 do
        FFrames[I].Colors[X, Y] := RGB(Colors[I mod 3][0],
          Colors[I mod 3][1], Colors[I mod 3][2]);
    end;
end;


procedure TTestGIFFrames.Write(const aDelays: array of Word;
  aLoopCount: Word);

var
  lWriter: TFPWriterGIF;

begin
  lWriter := TFPWriterGIF.Create;
  try
    lWriter.LoopCount := aLoopCount;
    FStream.Clear;
    lWriter.ImagesWrite(FStream, FFrames, aDelays);
  finally
    lWriter.Free;
  end;
end;


procedure TTestGIFFrames.ReadBack;

begin
  FStream.Position := 0;
  FReader.LoadFromStream(FStream);
end;


procedure TTestGIFFrames.GiveImage(Sender: TFPReaderGif;
  var NewImage: TFPCustomImage);

begin
  NewImage := TFPMemoryImage.Create(0, 0);
  Inc(FMade);
end;


function TTestGIFFrames.ColorAt(aFrame, aX, aY: Integer): String;

begin
  AssertTrue(Format('the read holds a frame %d', [aFrame]),
    aFrame < FReader.ImageCount);
  Result := ColorText(FReader.Images[aFrame].Img.Colors[aX, aY]);
end;


procedure TTestGIFFrames.TestEveryFrameIsRead;

begin
  BuildFrames(3, 8, 6);
  Write([10, 20, 30], 0);
  ReadBack;
  AssertEquals('the three frames written are the three read', 3,
    FReader.ImageCount);
end;


procedure TTestGIFFrames.TestEveryFrameHasItsOwnPicture;

begin
  BuildFrames(3, 8, 6);
  Write([10, 20, 30], 0);
  ReadBack;
  AssertEquals('the first frame', '255,0,0,255', ColorAt(0, 0, 0));
  AssertEquals('the second', '0,255,0,255', ColorAt(1, 4, 3));
  AssertEquals('the third', '0,0,255,255', ColorAt(2, 7, 5));
end;


procedure TTestGIFFrames.TestTheDelaysAreRead;

begin
  BuildFrames(3, 8, 6);
  Write([10, 20, 30], 0);
  ReadBack;
  AssertEquals('the delay of the first frame, in hundredths', 10,
    FReader.Images[0].Delay);
  AssertEquals('of the second', 20, FReader.Images[1].Delay);
  AssertEquals('and of the third', 30, FReader.Images[2].Delay);
end;


procedure TTestGIFFrames.TestHowOftenToPlayIsRead;

begin
  BuildFrames(2, 8, 6);
  Write([10, 10], 7);
  ReadBack;
  AssertEquals('the count of the application extension', 7,
    FReader.LoopCount);
end;


procedure TTestGIFFrames.TestNothingSaidAboutPlayingIsNought;

begin
  BuildFrames(1, 8, 6);
  Write([10], 0);
  ReadBack;
  AssertEquals('one image holds no extension that says how often to play '
    + 'it', 0, FReader.LoopCount);
end;


procedure TTestGIFFrames.TestTheDisposalOfAFrameIsRead;

begin
  BuildFrames(2, 8, 6);
  Write([10, 10], 0);
  ReadBack;
  AssertTrue('a frame of opaque pixels is left where it is',
    FReader.Images[0].Disposal = gdKeep);
end;


procedure TTestGIFFrames.TestAFrameIsTheSizeOfTheScreen;

begin
  BuildFrames(2, 9, 5);
  Write([10, 10], 0);
  ReadBack;
  AssertEquals('the screen of the file', 9, FReader.ScreenWidth);
  AssertEquals('and of the frame read', 9, FReader.Images[1].Img.Width);
  AssertEquals('as tall as the screen too', 5,
    FReader.Images[1].Img.Height);
end;


procedure TTestGIFFrames.TestASmallerFrameIsLaidOverTheOneBefore;

var
  lSmall: TFPMemoryImage;
  X, Y: Integer;

begin
  BuildFrames(1, 10, 8);
  lSmall := TFPMemoryImage.Create(4, 4);
  for Y := 0 to 3 do
    for X := 0 to 3 do
      lSmall.Colors[X, Y] := RGB(0, 0, 255);
  SetLength(FFrames, 2);
  FFrames[1] := lSmall;
  Write([10, 10], 0);
  ReadBack;
  AssertEquals('the second frame covers the corner it was written at',
    '0,0,255,255', ColorAt(1, 1, 1));
  AssertEquals('and the first frame shows through where it does not reach',
    '255,0,0,255', ColorAt(1, 9, 7));
end;


procedure TTestGIFFrames.TestAHoleInAFrameShowsWhatTheDisposalLeaves;

var
  X, Y: Integer;
  lColor: TFPColor;

begin
  BuildFrames(2, 8, 6);
  lColor := RGB(0, 0, 0);
  lColor.Alpha := alphaTransparent;
  for Y := 0 to 2 do
    for X := 0 to 3 do
      FFrames[1].Colors[X, Y] := lColor;
  Write([10, 10], 0);
  ReadBack;
  AssertTrue('a frame of the file has transparent pixels, so what is '
    + 'under a frame is taken away before the next is drawn',
    FReader.Images[0].Disposal = gdBackground);
  AssertEquals('so the hole of the second frame shows nothing at all',
    '0,0,0,0', ColorAt(1, 0, 0));
  AssertEquals('and the rest of it is the colour it was written with',
    '0,255,0,255', ColorAt(1, 7, 5));
end;


procedure TTestGIFFrames.TestTheReaderOwnsTheImagesItMakes;

begin
  BuildFrames(2, 8, 6);
  Write([10, 10], 0);
  ReadBack;
  AssertTrue('without a handler to give it one, the reader makes the image '
    + 'of a frame and frees it with the frame',
    FReader.Images[0].OwnsImage);
end;


procedure TTestGIFFrames.TestImagesGivenToItAreNotOwnedByIt;

var
  I: Integer;
  lImages: array of TFPCustomImage;

begin
  BuildFrames(2, 8, 6);
  Write([10, 10], 0);
  FMade := 0;
  FReader.OnCreateImage := @GiveImage;
  ReadBack;
  AssertEquals('the handler was asked for an image for each frame', 2,
    FMade);
  AssertFalse('and what it gave is not the readers to free',
    FReader.Images[0].OwnsImage);
  AssertEquals('the frames hold what it gave', '0,255,0,255',
    ColorAt(1, 0, 0));
  SetLength(lImages, FReader.ImageCount);
  for I := 0 to FReader.ImageCount - 1 do
    lImages[I] := FReader.Images[I].Img;
  FReader.Clear;
  for I := 0 to High(lImages) do
    begin
    AssertEquals('an image of its own stands after the reader forgot it',
      8, lImages[I].Width);
    lImages[I].Free;
    end;
end;


procedure TTestGIFFrames.TestClearForgetsTheFramesRead;

begin
  BuildFrames(2, 8, 6);
  Write([10, 10], 0);
  ReadBack;
  FReader.Clear;
  AssertEquals('nothing is left of the read', 0, FReader.ImageCount);
end;


procedure TTestGIFFrames.TestReadingTwiceForgetsTheFirstRead;

begin
  BuildFrames(3, 8, 6);
  Write([10, 10, 10], 0);
  ReadBack;
  BuildFrames(2, 8, 6);
  Write([10, 10], 0);
  ReadBack;
  AssertEquals('the frames of the second read are all that is held', 2,
    FReader.ImageCount);
end;


procedure TTestGIFFrames.TestWithoutCompositingAFrameIsWhatTheFileHolds;

var
  lSmall: TFPMemoryImage;
  X, Y: Integer;

begin
  BuildFrames(1, 10, 8);
  lSmall := TFPMemoryImage.Create(4, 4);
  for Y := 0 to 3 do
    for X := 0 to 3 do
      lSmall.Colors[X, Y] := RGB(0, 0, 255);
  SetLength(FFrames, 2);
  FFrames[1] := lSmall;
  Write([10, 10], 0);
  FReader.Composite := False;
  ReadBack;
  AssertEquals('the second frame is the picture the file holds, four wide',
    4, FReader.Images[1].Img.Width);
  AssertEquals('and four tall', 4, FReader.Images[1].Img.Height);
  AssertEquals('the whole of it is the colour it was written with',
    '0,0,255,255', ColorAt(1, 3, 3));
end;


{ TTestGIFSingleImage }

procedure TTestGIFSingleImage.SetUp;

begin
  inherited SetUp;
  FReader := TFPReaderGif.Create;
  FStream := TMemoryStream.Create;
  FImage := TFPMemoryImage.Create(0, 0);
end;


procedure TTestGIFSingleImage.TearDown;

begin
  FreeAndNil(FImage);
  FreeAndNil(FReader);
  FreeAndNil(FStream);
  inherited TearDown;
end;


procedure TTestGIFSingleImage.BuildPlacedFrame(aScreenWidth, aScreenHeight,
  aLeft, aTop: Integer);

const
  { Two pixels by two, of the four colours of the table: a clear, four
    literals and an end, packed from the low bit of each byte up. The
    third literal fills the code table to the width it starts at, so the
    last two codes are four bits wide where the first four are three. }
  Pixels: array[0..2] of Byte = ($44, $34, $05);

var
  lText: String;

begin
  FStream.Clear;
  lText := 'GIF89a';
  FStream.WriteBuffer(lText[1], 6);
  FStream.WriteByte(aScreenWidth and $FF);
  FStream.WriteByte((aScreenWidth shr 8) and $FF);
  FStream.WriteByte(aScreenHeight and $FF);
  FStream.WriteByte((aScreenHeight shr 8) and $FF);
  // A global table of four colours, and no background or aspect ratio.
  FStream.WriteByte($81);
  FStream.WriteByte(0);
  FStream.WriteByte(0);
  FStream.WriteByte(255); FStream.WriteByte(0); FStream.WriteByte(0);
  FStream.WriteByte(0); FStream.WriteByte(255); FStream.WriteByte(0);
  FStream.WriteByte(0); FStream.WriteByte(0); FStream.WriteByte(255);
  FStream.WriteByte(255); FStream.WriteByte(255); FStream.WriteByte(0);
  // The descriptor of the one frame, at the place asked for.
  FStream.WriteByte($2C);
  FStream.WriteByte(aLeft and $FF);
  FStream.WriteByte((aLeft shr 8) and $FF);
  FStream.WriteByte(aTop and $FF);
  FStream.WriteByte((aTop shr 8) and $FF);
  FStream.WriteByte(2); FStream.WriteByte(0);
  FStream.WriteByte(2); FStream.WriteByte(0);
  FStream.WriteByte(0);
  FStream.WriteByte(2);
  FStream.WriteByte(Length(Pixels));
  FStream.WriteBuffer(Pixels[0], Length(Pixels));
  FStream.WriteByte(0);
  FStream.WriteByte($3B);
  FStream.Position := 0;
end;


procedure TTestGIFSingleImage.TestOneImageStillReadsIntoTheImageGiven;

var
  lWriter: TFPWriterGIF;
  lSource: TFPMemoryImage;
  X, Y: Integer;

begin
  lSource := TFPMemoryImage.Create(5, 4);
  try
    for Y := 0 to 3 do
      for X := 0 to 4 do
        lSource.Colors[X, Y] := RGB(10, 20, 30);
    lWriter := TFPWriterGIF.Create;
    try
      lWriter.ImageWrite(FStream, lSource);
    finally
      lWriter.Free;
    end;
  finally
    lSource.Free;
  end;
  FStream.Position := 0;
  FImage.LoadFromStream(FStream, FReader);
  AssertEquals('the width of the image the caller gave', 5, FImage.Width);
  AssertEquals('its height', 4, FImage.Height);
  AssertEquals('and the colour read into it', '10,20,30,255',
    ColorText(FImage.Colors[2, 2]));
end;


procedure TTestGIFSingleImage.TestTheFrameOfASingleImageIsThereToBeRead;

begin
  BuildPlacedFrame(2, 2, 0, 0);
  FImage.LoadFromStream(FStream, FReader);
  AssertEquals('reading one image leaves its frame to be looked at', 1,
    FReader.ImageCount);
  AssertTrue('and the frame holds the image the caller gave',
    FReader.Images[0].Img = FImage);
end;


procedure TTestGIFSingleImage.TestAPlacedFrameIsReadOntoTheScreen;

begin
  BuildPlacedFrame(6, 5, 3, 2);
  FImage.LoadFromStream(FStream, FReader);
  AssertEquals('the image is as wide as the screen of the file', 6,
    FImage.Width);
  AssertEquals('and as tall', 5, FImage.Height);
  AssertEquals('the first pixel of the frame is at the place the file '
    + 'gives it', '255,0,0,255', ColorText(FImage.Colors[3, 2]));
  AssertEquals('the last of it, a pixel along and a pixel down',
    '255,255,0,255', ColorText(FImage.Colors[4, 3]));
  AssertEquals('and the screen around it is nothing at all', '0,0,0,0',
    ColorText(FImage.Colors[0, 0]));
end;


procedure TTestGIFSingleImage.TestAPlacedFrameWithoutCompositingKeepsItsOwnSize;

begin
  BuildPlacedFrame(6, 5, 3, 2);
  FReader.Composite := False;
  FImage.LoadFromStream(FStream, FReader);
  AssertEquals('the image is the two pixels the frame holds', 2,
    FImage.Width);
  AssertEquals('and two tall', 2, FImage.Height);
  AssertEquals('drawn from its own corner', '255,0,0,255',
    ColorText(FImage.Colors[0, 0]));
  AssertEquals('the place of it on the screen is the frames to tell', 3,
    FReader.Images[0].Left);
  AssertEquals('and down', 2, FReader.Images[0].Top);
end;


initialization
  RegisterTest('gifread', TTestGIFFrames);
  RegisterTest('gifread', TTestGIFSingleImage);
end.
