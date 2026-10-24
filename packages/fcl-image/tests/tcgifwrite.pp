{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by the Free Pascal development team

    Tests for the GIF writer: the bytes it writes, and reading them back.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit tcgifwrite;

{$mode objfpc}{$H+}

interface

uses sysutils, classes, fpcunit, testregistry, fpimage, fpreadgif,
     fpwritegif;

type
  { What the writer puts in the stream, read back byte by byte. }
  TTestGIFStream = class(TTestCase)
  private
    FImage: TFPMemoryImage;
    FWriter: TFPWriterGIF;
    FStream: TMemoryStream;
    // An image of that size, filled with one colour.
    procedure Build(aWidth, aHeight: Integer; const aColor: TFPColor);
    // Writes the image held, and leaves the bytes in FStream.
    procedure WriteOne;
    // Writes several copies of the image held, at the delays given.
    procedure WriteFrames(aCount: Integer; const aDelays: array of Word);
    // The byte at that position of what was written.
    function ByteAt(aIndex: Integer): Byte;
    // The count at that position, its low byte first.
    function WordAt(aIndex: Integer): Word;
    // The position of the first block of that kind, or -1.
    function FindBlock(aIntroducer, aLabel: Byte): Integer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTheSignatureIsGIF89a;
    procedure TestTheScreenTakesTheSizeOfTheImage;
    procedure TestTheScreenSaysItHasAGlobalColorTable;
    procedure TestTheTableHoldsAPowerOfTwoEntries;
    procedure TestTheColoursOfTheImageAreInTheTable;
    procedure TestTheStreamEndsWithTheTrailer;
    procedure TestOneImageHasNoLoopingExtension;
    procedure TestAnAnimationSaysHowOftenToPlay;
    procedure TestALoopCountOfItsOwn;
    procedure TestEveryFrameHasAnImageDescriptor;
    procedure TestAFrameTakesTheDelayOfTheWriter;
    procedure TestAFrameTakesADelayOfItsOwn;
    procedure TestAFrameTheDelaysRunOutBeforeTakesTheWriters;
    procedure TestATransparentPixelGivesAControlBlock;
    procedure TestWithoutTransparencyThereIsNoControlBlock;
    procedure TestTheScreenIsTheLargestOfTheFrames;
  end;

  { What comes back when the reader of the package reads it. }
  TTestGIFRoundTrip = class(TTestCase)
  private
    FImage: TFPMemoryImage;
    FRead: TFPMemoryImage;
    FWriter: TFPWriterGIF;
    FStream: TMemoryStream;
    // Writes the image held and reads it back into FRead.
    procedure RoundTrip;
    // Writes the images as an animation and reads the first frame back.
    procedure RoundTripFrames(const aImages: array of TFPCustomImage);
    // The colour at that place of what came back.
    function ColorAt(aX, aY: Integer): TFPColor;
    // Fails unless the colour read at that place is the one given.
    procedure AssertColor(const aMessage: String; aX, aY: Integer;
      const aColor: TFPColor);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestASingleColourComesBack;
    procedure TestTheSizeComesBack;
    procedure TestEveryPixelOfAPatternComesBack;
    procedure TestTwoHundredAndFiftySixColoursComeBack;
    procedure TestAGradientOfManyColoursIsQuantized;
    procedure TestATransparentPixelComesBackTransparent;
    procedure TestAnOpaqueBlackIsNotTakenForTransparent;
    procedure TestALongRunOfOneColourComesBack;
    procedure TestTheFirstFrameOfAnAnimationComesBack;
    procedure TestAPixelTallImageComesBack;
    procedure TestAnImageOfManyBlocksComesBack;
  end;

  { What it does with what it cannot write. }
  TTestGIFRefusals = class(TTestCase)
  private
    FWriter: TFPWriterGIF;
    FStream: TMemoryStream;
    procedure WriteNothing;
    procedure WriteEmptyImage;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestWritingNoImageAtAllIsRejected;
    procedure TestWritingAnImageOfNoExtentIsRejected;
    procedure TestTheWriterIsRegisteredForTheExtension;
  end;

implementation

const
  { Where the global colour table starts: the signature and the screen
    descriptor come before it. }
  TableStart = 13;

// A colour of 8-bit channels, opaque.
function RGB(aRed, aGreen, aBlue: Byte): TFPColor;

begin
  Result.Red := aRed * 257;
  Result.Green := aGreen * 257;
  Result.Blue := aBlue * 257;
  Result.Alpha := alphaOpaque;
end;


{ TTestGIFStream }

procedure TTestGIFStream.SetUp;

begin
  inherited SetUp;
  FWriter := TFPWriterGIF.Create;
  FStream := TMemoryStream.Create;
end;


procedure TTestGIFStream.TearDown;

begin
  FreeAndNil(FStream);
  FreeAndNil(FWriter);
  FreeAndNil(FImage);
  inherited TearDown;
end;


procedure TTestGIFStream.Build(aWidth, aHeight: Integer;
  const aColor: TFPColor);

var
  X, Y: Integer;

begin
  FreeAndNil(FImage);
  FImage := TFPMemoryImage.Create(aWidth, aHeight);
  for Y := 0 to aHeight - 1 do
    for X := 0 to aWidth - 1 do
      FImage.Colors[X, Y] := aColor;
end;


procedure TTestGIFStream.WriteOne;

begin
  FStream.Clear;
  FWriter.ImageWrite(FStream, FImage);
end;


procedure TTestGIFStream.WriteFrames(aCount: Integer;
  const aDelays: array of Word);

var
  lImages: array of TFPCustomImage;
  I: Integer;

begin
  SetLength(lImages, aCount);
  for I := 0 to aCount - 1 do
    lImages[I] := FImage;
  FStream.Clear;
  FWriter.ImagesWrite(FStream, lImages, aDelays);
end;


function TTestGIFStream.ByteAt(aIndex: Integer): Byte;

begin
  AssertTrue(Format('the stream is longer than %d bytes', [aIndex]),
    aIndex < FStream.Size);
  Result := PByte(FStream.Memory)[aIndex];
end;


function TTestGIFStream.WordAt(aIndex: Integer): Word;

begin
  Result := ByteAt(aIndex) or (ByteAt(aIndex + 1) shl 8);
end;


function TTestGIFStream.FindBlock(aIntroducer, aLabel: Byte): Integer;

var
  I: Integer;

begin
  Result := -1;
  for I := 0 to FStream.Size - 2 do
    if (PByte(FStream.Memory)[I] = aIntroducer)
       and (PByte(FStream.Memory)[I + 1] = aLabel) then
      Exit(I);
end;


procedure TTestGIFStream.TestTheSignatureIsGIF89a;

var
  lText: String;

begin
  Build(4, 4, RGB(255, 0, 0));
  WriteOne;
  SetLength(lText, 6);
  Move(PByte(FStream.Memory)^, lText[1], 6);
  AssertEquals('the stream opens with the signature of the version that '
    + 'has the blocks an animation needs', 'GIF89a', lText);
end;


procedure TTestGIFStream.TestTheScreenTakesTheSizeOfTheImage;

begin
  Build(37, 11, RGB(0, 255, 0));
  WriteOne;
  AssertEquals('the width of the screen', 37, WordAt(6));
  AssertEquals('the height of the screen', 11, WordAt(8));
end;


procedure TTestGIFStream.TestTheScreenSaysItHasAGlobalColorTable;

var
  lPacked: Byte;

begin
  Build(4, 4, RGB(255, 0, 0));
  WriteOne;
  lPacked := ByteAt(10);
  AssertTrue('the flag for a global colour table is set',
    (lPacked and $80) <> 0);
  AssertEquals('the background colour index', 0, ByteAt(11));
  AssertEquals('and no aspect ratio is given', 0, ByteAt(12));
end;


procedure TTestGIFStream.TestTheTableHoldsAPowerOfTwoEntries;

var
  lSize: Integer;

begin
  Build(4, 4, RGB(255, 0, 0));
  WriteOne;
  lSize := (ByteAt(10) and $07) + 1;
  AssertEquals('one colour is written in a table of the least size a GIF '
    + 'takes, which holds four', 2, lSize);
  AssertEquals('the image separator follows the four entries of it',
    $2C, ByteAt(TableStart + 4 * 3));
end;


procedure TTestGIFStream.TestTheColoursOfTheImageAreInTheTable;

begin
  Build(4, 4, RGB(18, 52, 86));
  WriteOne;
  AssertEquals('the red of the one colour', 18, ByteAt(TableStart));
  AssertEquals('its green', 52, ByteAt(TableStart + 1));
  AssertEquals('and its blue', 86, ByteAt(TableStart + 2));
end;


procedure TTestGIFStream.TestTheStreamEndsWithTheTrailer;

begin
  Build(4, 4, RGB(255, 0, 0));
  WriteOne;
  AssertEquals('the last byte says the stream is over', $3B,
    ByteAt(FStream.Size - 1));
end;


procedure TTestGIFStream.TestOneImageHasNoLoopingExtension;

begin
  Build(4, 4, RGB(255, 0, 0));
  WriteOne;
  AssertEquals('a single image is not an animation, so nothing says how '
    + 'often to play it', -1, FindBlock($21, $FF));
end;


procedure TTestGIFStream.TestAnAnimationSaysHowOftenToPlay;

var
  lAt: Integer;
  lText: String;

begin
  Build(4, 4, RGB(255, 0, 0));
  WriteFrames(3, []);
  lAt := FindBlock($21, $FF);
  AssertTrue('an animation holds an application extension', lAt >= 0);
  SetLength(lText, 11);
  Move(PByte(FStream.Memory)[lAt + 3], lText[1], 11);
  AssertEquals('the one that every reader knows the looping by',
    'NETSCAPE2.0', lText);
  AssertEquals('it plays over and over again by default', 0,
    WordAt(lAt + 16));
end;


procedure TTestGIFStream.TestALoopCountOfItsOwn;

var
  lAt: Integer;

begin
  Build(4, 4, RGB(255, 0, 0));
  FWriter.LoopCount := 5;
  WriteFrames(2, []);
  lAt := FindBlock($21, $FF);
  AssertEquals('the count the writer was given', 5, WordAt(lAt + 16));
end;


procedure TTestGIFStream.TestEveryFrameHasAnImageDescriptor;

var
  I, lCount: Integer;

begin
  Build(4, 4, RGB(255, 0, 0));
  WriteFrames(3, []);
  lCount := 0;
  // A descriptor opens with the separator, and the bytes after it are
  // the place and the size of the frame, which is the whole screen here.
  for I := 0 to FStream.Size - 10 do
    if (PByte(FStream.Memory)[I] = $2C)
       and (PByte(FStream.Memory)[I + 5] = 4)
       and (PByte(FStream.Memory)[I + 7] = 4) then
      Inc(lCount);
  AssertEquals('one descriptor for each of the three frames', 3, lCount);
end;


procedure TTestGIFStream.TestAFrameTakesTheDelayOfTheWriter;

var
  lAt: Integer;

begin
  Build(4, 4, RGB(255, 0, 0));
  FWriter.Delay := 25;
  WriteFrames(2, []);
  lAt := FindBlock($21, $F9);
  AssertTrue('a frame of an animation holds a control block', lAt >= 0);
  AssertEquals('the delay the writer was given', 25, WordAt(lAt + 4));
end;


procedure TTestGIFStream.TestAFrameTakesADelayOfItsOwn;

var
  lAt: Integer;

begin
  Build(4, 4, RGB(255, 0, 0));
  WriteFrames(2, [7, 9]);
  lAt := FindBlock($21, $F9);
  AssertEquals('the delay of the first frame', 7, WordAt(lAt + 4));
end;


procedure TTestGIFStream.TestAFrameTheDelaysRunOutBeforeTakesTheWriters;

var
  lAt, I: Integer;

begin
  Build(4, 4, RGB(255, 0, 0));
  FWriter.Delay := 30;
  WriteFrames(2, [7]);
  lAt := -1;
  for I := 0 to FStream.Size - 2 do
    if (PByte(FStream.Memory)[I] = $21)
       and (PByte(FStream.Memory)[I + 1] = $F9) then
      lAt := I;
  AssertEquals('the second frame falls back on the delay of the writer',
    30, WordAt(lAt + 4));
end;


procedure TTestGIFStream.TestATransparentPixelGivesAControlBlock;

var
  lAt: Integer;
  lColor: TFPColor;

begin
  Build(4, 4, RGB(255, 0, 0));
  lColor := RGB(0, 0, 0);
  lColor.Alpha := alphaTransparent;
  FImage.Colors[0, 0] := lColor;
  WriteOne;
  lAt := FindBlock($21, $F9);
  AssertTrue('a control block says which index is transparent', lAt >= 0);
  AssertTrue('and its flag for one is set',
    (ByteAt(lAt + 3) and $01) <> 0);
end;


procedure TTestGIFStream.TestWithoutTransparencyThereIsNoControlBlock;

begin
  Build(4, 4, RGB(255, 0, 0));
  WriteOne;
  AssertEquals('one image of opaque pixels needs nothing said about it',
    -1, FindBlock($21, $F9));
end;


procedure TTestGIFStream.TestTheScreenIsTheLargestOfTheFrames;

var
  lImages: array[0..1] of TFPCustomImage;
  lSmall: TFPMemoryImage;

begin
  Build(10, 4, RGB(255, 0, 0));
  lSmall := TFPMemoryImage.Create(3, 7);
  try
    lSmall.Colors[0, 0] := RGB(0, 0, 255);
    lImages[0] := FImage;
    lImages[1] := lSmall;
    FStream.Clear;
    FWriter.ImagesWrite(FStream, lImages);
    AssertEquals('the screen is as wide as the widest frame', 10,
      WordAt(6));
    AssertEquals('and as tall as the tallest', 7, WordAt(8));
  finally
    lSmall.Free;
  end;
end;


{ TTestGIFRoundTrip }

procedure TTestGIFRoundTrip.SetUp;

begin
  inherited SetUp;
  FWriter := TFPWriterGIF.Create;
  FStream := TMemoryStream.Create;
end;


procedure TTestGIFRoundTrip.TearDown;

begin
  FreeAndNil(FStream);
  FreeAndNil(FWriter);
  FreeAndNil(FRead);
  FreeAndNil(FImage);
  inherited TearDown;
end;


procedure TTestGIFRoundTrip.RoundTrip;

var
  lReader: TFPReaderGif;

begin
  FStream.Clear;
  FWriter.ImageWrite(FStream, FImage);
  FStream.Position := 0;
  FreeAndNil(FRead);
  FRead := TFPMemoryImage.Create(0, 0);
  lReader := TFPReaderGif.Create;
  try
    FRead.LoadFromStream(FStream, lReader);
  finally
    lReader.Free;
  end;
end;


procedure TTestGIFRoundTrip.RoundTripFrames(
  const aImages: array of TFPCustomImage);

var
  lReader: TFPReaderGif;

begin
  FStream.Clear;
  FWriter.ImagesWrite(FStream, aImages);
  FStream.Position := 0;
  FreeAndNil(FRead);
  FRead := TFPMemoryImage.Create(0, 0);
  lReader := TFPReaderGif.Create;
  try
    FRead.LoadFromStream(FStream, lReader);
  finally
    lReader.Free;
  end;
end;


function TTestGIFRoundTrip.ColorAt(aX, aY: Integer): TFPColor;

begin
  Result := FRead.Colors[aX, aY];
end;


procedure TTestGIFRoundTrip.AssertColor(const aMessage: String;
  aX, aY: Integer; const aColor: TFPColor);

var
  lRead: TFPColor;

begin
  lRead := ColorAt(aX, aY);
  AssertEquals(Format('%s: the red at %d,%d', [aMessage, aX, aY]),
    aColor.Red shr 8, lRead.Red shr 8);
  AssertEquals(Format('%s: the green at %d,%d', [aMessage, aX, aY]),
    aColor.Green shr 8, lRead.Green shr 8);
  AssertEquals(Format('%s: the blue at %d,%d', [aMessage, aX, aY]),
    aColor.Blue shr 8, lRead.Blue shr 8);
end;


procedure TTestGIFRoundTrip.TestASingleColourComesBack;

var
  X, Y: Integer;

begin
  FImage := TFPMemoryImage.Create(5, 3);
  for Y := 0 to 2 do
    for X := 0 to 4 do
      FImage.Colors[X, Y] := RGB(12, 34, 56);
  RoundTrip;
  AssertColor('one colour over the whole image', 0, 0, RGB(12, 34, 56));
  AssertColor('at the far corner as well', 4, 2, RGB(12, 34, 56));
end;


procedure TTestGIFRoundTrip.TestTheSizeComesBack;

begin
  FImage := TFPMemoryImage.Create(23, 9);
  RoundTrip;
  AssertEquals('the width', 23, FRead.Width);
  AssertEquals('the height', 9, FRead.Height);
end;


procedure TTestGIFRoundTrip.TestEveryPixelOfAPatternComesBack;

var
  X, Y: Integer;

begin
  FImage := TFPMemoryImage.Create(16, 16);
  for Y := 0 to 15 do
    for X := 0 to 15 do
      FImage.Colors[X, Y] := RGB(X * 16, Y * 16, (X xor Y) * 16);
  RoundTrip;
  for Y := 0 to 15 do
    for X := 0 to 15 do
      AssertColor('a pattern of two hundred and fifty six colours', X, Y,
        RGB(X * 16, Y * 16, (X xor Y) * 16));
end;


procedure TTestGIFRoundTrip.TestTwoHundredAndFiftySixColoursComeBack;

var
  I: Integer;

begin
  FImage := TFPMemoryImage.Create(256, 1);
  for I := 0 to 255 do
    FImage.Colors[I, 0] := RGB(I, 255 - I, I div 2);
  RoundTrip;
  for I := 0 to 255 do
    AssertColor('a table filled to the brim', I, 0,
      RGB(I, 255 - I, I div 2));
end;


procedure TTestGIFRoundTrip.TestAGradientOfManyColoursIsQuantized;

var
  X, Y, lOff: Integer;
  lRead: TFPColor;

begin
  FImage := TFPMemoryImage.Create(64, 64);
  for Y := 0 to 63 do
    for X := 0 to 63 do
      FImage.Colors[X, Y] := RGB(X * 4, Y * 4, (X + Y) * 2);
  RoundTrip;
  // Four thousand and ninety six colours do not fit a table of two
  // hundred and fifty six, so what comes back is near rather than equal.
  lOff := 0;
  for Y := 0 to 63 do
    for X := 0 to 63 do
      begin
      lRead := ColorAt(X, Y);
      if Abs(Integer(lRead.Red shr 8) - X * 4) > 24 then
        Inc(lOff);
      if Abs(Integer(lRead.Green shr 8) - Y * 4) > 24 then
        Inc(lOff);
      end;
  AssertEquals('every pixel of the gradient comes back within a shade or '
    + 'two of what it was', 0, lOff);
end;


procedure TTestGIFRoundTrip.TestATransparentPixelComesBackTransparent;

var
  X, Y: Integer;
  lColor: TFPColor;

begin
  FImage := TFPMemoryImage.Create(4, 4);
  for Y := 0 to 3 do
    for X := 0 to 3 do
      FImage.Colors[X, Y] := RGB(0, 255, 0);
  lColor := RGB(255, 255, 255);
  lColor.Alpha := alphaTransparent;
  FImage.Colors[1, 1] := lColor;
  RoundTrip;
  AssertEquals('the pixel written as transparent comes back with no '
    + 'alpha', 0, ColorAt(1, 1).Alpha);
  AssertEquals('and the one beside it is opaque', alphaOpaque,
    ColorAt(2, 1).Alpha);
  AssertColor('which keeps its colour', 2, 1, RGB(0, 255, 0));
end;


procedure TTestGIFRoundTrip.TestAnOpaqueBlackIsNotTakenForTransparent;

var
  X, Y: Integer;
  lColor: TFPColor;

begin
  FImage := TFPMemoryImage.Create(4, 4);
  for Y := 0 to 3 do
    for X := 0 to 3 do
      FImage.Colors[X, Y] := RGB(0, 0, 0);
  lColor := RGB(0, 0, 0);
  lColor.Alpha := alphaTransparent;
  FImage.Colors[0, 0] := lColor;
  RoundTrip;
  AssertEquals('the transparent pixel comes back with no alpha', 0,
    ColorAt(0, 0).Alpha);
  AssertEquals('a black pixel that was opaque stays opaque, though the '
    + 'index that stands for transparent holds black as well',
    alphaOpaque, ColorAt(3, 3).Alpha);
  AssertColor('and keeps its colour', 3, 3, RGB(0, 0, 0));
end;


procedure TTestGIFRoundTrip.TestALongRunOfOneColourComesBack;

var
  I: Integer;

begin
  // A run this long fills the code table of the compressor, which has to
  // start it over part way through the image.
  FImage := TFPMemoryImage.Create(400, 400);
  for I := 0 to 399 do
    FImage.Colors[I, 0] := RGB(0, 0, 0);
  RoundTrip;
  AssertColor('the start of the run', 0, 0, RGB(0, 0, 0));
  AssertColor('and the end of it', 399, 399, RGB(0, 0, 0));
end;


procedure TTestGIFRoundTrip.TestTheFirstFrameOfAnAnimationComesBack;

var
  lImages: array[0..2] of TFPCustomImage;
  lSecond, lThird: TFPMemoryImage;
  X, Y: Integer;

begin
  FImage := TFPMemoryImage.Create(8, 8);
  lSecond := TFPMemoryImage.Create(8, 8);
  lThird := TFPMemoryImage.Create(8, 8);
  try
    for Y := 0 to 7 do
      for X := 0 to 7 do
        begin
        FImage.Colors[X, Y] := RGB(255, 0, 0);
        lSecond.Colors[X, Y] := RGB(0, 255, 0);
        lThird.Colors[X, Y] := RGB(0, 0, 255);
        end;
    lImages[0] := FImage;
    lImages[1] := lSecond;
    lImages[2] := lThird;
    RoundTripFrames(lImages);
    AssertColor('the reader of the package reads the first frame of an '
      + 'animation', 0, 0, RGB(255, 0, 0));
    AssertColor('over the whole of it', 7, 7, RGB(255, 0, 0));
  finally
    lThird.Free;
    lSecond.Free;
  end;
end;


procedure TTestGIFRoundTrip.TestAPixelTallImageComesBack;

var
  I: Integer;

begin
  FImage := TFPMemoryImage.Create(6, 1);
  for I := 0 to 5 do
    FImage.Colors[I, 0] := RGB(I * 40, 0, 255 - I * 40);
  RoundTrip;
  for I := 0 to 5 do
    AssertColor('a row of six pixels', I, 0, RGB(I * 40, 0, 255 - I * 40));
end;


procedure TTestGIFRoundTrip.TestAnImageOfManyBlocksComesBack;

var
  X, Y, lIndex, lOff: Integer;

begin
  // A pattern of this size and this little order compresses to thousands
  // of bytes, which the writer has to cut into the blocks of at most two
  // hundred and fifty five that a GIF holds them in.
  FImage := TFPMemoryImage.Create(100, 100);
  for Y := 0 to 99 do
    for X := 0 to 99 do
      begin
      lIndex := (X * X + Y * Y * 7 + X * Y * 3) mod 256;
      FImage.Colors[X, Y] := RGB(lIndex, 255 - lIndex, (lIndex * 3) mod 256);
      end;
  RoundTrip;
  lOff := 0;
  for Y := 0 to 99 do
    for X := 0 to 99 do
      begin
      lIndex := (X * X + Y * Y * 7 + X * Y * 3) mod 256;
      if Integer(ColorAt(X, Y).Red shr 8) <> lIndex then
        Inc(lOff);
      end;
  AssertEquals('every one of ten thousand pixels comes back as it was', 0,
    lOff);
end;


{ TTestGIFRefusals }

procedure TTestGIFRefusals.SetUp;

begin
  inherited SetUp;
  FWriter := TFPWriterGIF.Create;
  FStream := TMemoryStream.Create;
end;


procedure TTestGIFRefusals.TearDown;

begin
  FreeAndNil(FStream);
  FreeAndNil(FWriter);
  inherited TearDown;
end;


procedure TTestGIFRefusals.WriteNothing;

var
  lImages: array of TFPCustomImage;

begin
  lImages := nil;
  FWriter.ImagesWrite(FStream, lImages);
end;


procedure TTestGIFRefusals.WriteEmptyImage;

var
  lImage: TFPMemoryImage;

begin
  lImage := TFPMemoryImage.Create(0, 0);
  try
    FWriter.ImageWrite(FStream, lImage);
  finally
    lImage.Free;
  end;
end;


procedure TTestGIFRefusals.TestWritingNoImageAtAllIsRejected;

begin
  AssertException('an animation of no frames is rejected',
    FPImageException, @WriteNothing);
end;


procedure TTestGIFRefusals.TestWritingAnImageOfNoExtentIsRejected;

begin
  AssertException('an image with no pixels in it is rejected',
    FPImageException, @WriteEmptyImage);
end;


procedure TTestGIFRefusals.TestTheWriterIsRegisteredForTheExtension;

var
  lClass: TFPCustomImageWriterClass;

begin
  lClass := TFPCustomImage.FindWriterFromExtension('gif');
  AssertTrue('the writer registers itself for the gif extension, as every '
    + 'writer of the package does', lClass = TFPWriterGIF);
end;


initialization
  RegisterTest('gifwrite', TTestGIFStream);
  RegisterTest('gifwrite', TTestGIFRoundTrip);
  RegisterTest('gifwrite', TTestGIFRefusals);
end.
