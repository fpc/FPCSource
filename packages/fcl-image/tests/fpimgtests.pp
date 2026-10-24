{
    Shared helpers for the fcl-image tests: generated images, comparisons,
    round trips through a writer and a reader, and test streams.
    See the file COPYING.FPC, included in this distribution, for details.
}
unit fpimgtests;

{$mode objfpc}{$H+}

interface

uses sysutils, classes, fpcunit, fpimage;

// The name of a file of the examples directory of the package.
function ExampleFile(const aName: String): String;

// A colour of 8-bit channels.
function RGB8(aRed, aGreen, aBlue: Byte; aAlpha: Byte = 255): TFPColor;

// A colour as text, for failure messages.
function ColorToStr(const aColor: TFPColor): String;

// An image filled with one colour.
function CreateSolidImage(aWidth, aHeight: Integer; const aColor: TFPColor): TFPMemoryImage;

// An opaque image with red rising left to right, green top to bottom, 8-bit exact.
function CreateGradientImage(aWidth, aHeight: Integer): TFPMemoryImage;

// The gradient image with alpha rising along the pixel index, 8-bit exact.
function CreateAlphaImage(aWidth, aHeight: Integer): TFPMemoryImage;

// An opaque gray ramp, 8-bit exact.
function CreateGrayImage(aWidth, aHeight: Integer): TFPMemoryImage;

// An image of cells of two colours.
function CreateCheckerImage(aWidth, aHeight, aCell: Integer; const aColor1, aColor2: TFPColor): TFPMemoryImage;

// An opaque image using exactly aCount distinct 8-bit colours, aCount at most 256.
function CreateFewColorsImage(aWidth, aHeight, aCount: Integer): TFPMemoryImage;

// An opaque black and white image.
function CreateMonoImage(aWidth, aHeight: Integer): TFPMemoryImage;

// Number of distinct colours in an image.
function CountColors(aImage: TFPCustomImage): Integer;

// True if both colours differ by at most aTolerance on every channel.
function ColorsClose(const aColor1, aColor2: TFPColor; aTolerance: Word): Boolean;

// Fails unless both colours differ by at most aTolerance on every channel.
procedure AssertColorsEqual(const aMessage: String; const aExpected, aActual: TFPColor; aTolerance: Word = 0);

// Fails unless both images have the same size and all pixels within aTolerance.
procedure AssertImagesEqual(const aMessage: String; aExpected, aActual: TFPCustomImage; aTolerance: Word = 0; aIgnoreAlpha: Boolean = False);

// Peak signal to noise ratio of the RGB channels in dB, 100 for identical images.
function ImagePSNR(aExpected, aActual: TFPCustomImage): Double;

// Writes aImage with aWriter into aStream at its position and returns the byte count.
function WriteImage(aImage: TFPCustomImage; aWriter: TFPCustomImageWriter; aStream: TStream): Int64;

// Writes aImage with aWriter and reads it back with aReader into a new memory image.
function RoundTrip(aImage: TFPCustomImage; aWriter: TFPCustomImageWriter; aReader: TFPCustomImageReader): TFPMemoryImage;

// A stream with the given bytes, positioned at 0.
function BytesStream(const aBytes: array of Byte): TMemoryStream;

// Fails unless aMethod raises aClass or a descendant of it.
procedure AssertRaises(const aMessage: String; aClass: ExceptClass; aMethod: TRunMethod);

// Runs aMethod twice and fails unless the second run leaves the heap as it found it.
procedure AssertNoLeak(const aMessage: String; aMethod: TRunMethod);

type
  // A write-only stream that cannot seek or change size, like a pipe or a socket.
  TWriteOnlyStream = class(TStream)
  private
    FData: TMemoryStream;
  protected
    procedure SetSize(const aNewSize: Int64); override;
  public
    constructor Create;
    destructor Destroy; override;
    // Raises: this stream cannot be read.
    function Read(var aBuffer; aCount: Longint): Longint; override;
    // Appends to the bytes written so far.
    function Write(const aBuffer; aCount: Longint): Longint; override;
    // Only reports the current position; any other seek raises.
    function Seek(const aOffset: Int64; aOrigin: TSeekOrigin): Int64; override;
    // The bytes written so far.
    property Data: TMemoryStream read FData;
  end;

implementation

{ TWriteOnlyStream }

constructor TWriteOnlyStream.Create;

begin
  inherited Create;
  FData := TMemoryStream.Create;
end;


destructor TWriteOnlyStream.Destroy;

begin
  FData.Free;
  inherited Destroy;
end;


procedure TWriteOnlyStream.SetSize(const aNewSize: Int64);

begin
  raise EStreamError.Create('A write-only stream cannot change size');
end;


function TWriteOnlyStream.Read(var aBuffer; aCount: Longint): Longint;

begin
  raise EStreamError.Create('A write-only stream cannot be read');
end;


function TWriteOnlyStream.Write(const aBuffer; aCount: Longint): Longint;

begin
  Result := FData.Write(aBuffer, aCount);
end;


function TWriteOnlyStream.Seek(const aOffset: Int64; aOrigin: TSeekOrigin): Int64;

begin
  if (aOffset = 0) and (aOrigin = soCurrent) then
    Result := FData.Position
  else
    raise EStreamError.Create('A write-only stream cannot seek');
end;


function ExampleFile(const aName: String): String;

begin
  if DirectoryExists('examples') then
    Result := 'examples' + PathDelim + aName
  else
    Result := '..' + PathDelim + 'examples' + PathDelim + aName;
end;


function RGB8(aRed, aGreen, aBlue: Byte; aAlpha: Byte): TFPColor;

begin
  Result.Red := aRed * 257;
  Result.Green := aGreen * 257;
  Result.Blue := aBlue * 257;
  Result.Alpha := aAlpha * 257;
end;


function ColorToStr(const aColor: TFPColor): String;

begin
  Result := Format('(R=$%.4x G=$%.4x B=$%.4x A=$%.4x)',
    [aColor.Red, aColor.Green, aColor.Blue, aColor.Alpha]);
end;


function CreateSolidImage(aWidth, aHeight: Integer; const aColor: TFPColor): TFPMemoryImage;

var
  lX, lY: Integer;

begin
  Result := TFPMemoryImage.Create(aWidth, aHeight);
  for lY := 0 to aHeight - 1 do
    for lX := 0 to aWidth - 1 do
      Result.Colors[lX, lY] := aColor;
end;


// A channel value spread over 0..255 along a length.
function Ramp(aPos, aLength: Integer): Byte;

begin
  if aLength <= 1 then
    Result := 0
  else
    Result := (aPos * 255) div (aLength - 1);
end;


function CreateGradientImage(aWidth, aHeight: Integer): TFPMemoryImage;

var
  lX, lY: Integer;

begin
  Result := TFPMemoryImage.Create(aWidth, aHeight);
  for lY := 0 to aHeight - 1 do
    for lX := 0 to aWidth - 1 do
      Result.Colors[lX, lY] := RGB8(Ramp(lX, aWidth), Ramp(lY, aHeight),
        (lX * 7 + lY * 13) and $FF);
end;


function CreateAlphaImage(aWidth, aHeight: Integer): TFPMemoryImage;

var
  lX, lY: Integer;
  lColor: TFPColor;

begin
  Result := CreateGradientImage(aWidth, aHeight);
  for lY := 0 to aHeight - 1 do
    for lX := 0 to aWidth - 1 do
      begin
      lColor := Result.Colors[lX, lY];
      lColor.Alpha := Ramp(lY * aWidth + lX, aWidth * aHeight) * 257;
      Result.Colors[lX, lY] := lColor;
      end;
end;


function CreateGrayImage(aWidth, aHeight: Integer): TFPMemoryImage;

var
  lX, lY: Integer;
  lGray: Byte;

begin
  Result := TFPMemoryImage.Create(aWidth, aHeight);
  for lY := 0 to aHeight - 1 do
    for lX := 0 to aWidth - 1 do
      begin
      lGray := Ramp(lY * aWidth + lX, aWidth * aHeight);
      Result.Colors[lX, lY] := RGB8(lGray, lGray, lGray);
      end;
end;


function CreateCheckerImage(aWidth, aHeight, aCell: Integer; const aColor1, aColor2: TFPColor): TFPMemoryImage;

var
  lX, lY: Integer;

begin
  Result := TFPMemoryImage.Create(aWidth, aHeight);
  for lY := 0 to aHeight - 1 do
    for lX := 0 to aWidth - 1 do
      if Odd((lX div aCell) + (lY div aCell)) then
        Result.Colors[lX, lY] := aColor2
      else
        Result.Colors[lX, lY] := aColor1;
end;


// The colour number aIndex of the set used by CreateFewColorsImage.
function FewColor(aIndex: Integer): TFPColor;

begin
  Result := RGB8((aIndex * 37) and $FF, (aIndex * 101 + 50) and $FF, (aIndex * 173 + 20) and $FF);
end;


function CreateFewColorsImage(aWidth, aHeight, aCount: Integer): TFPMemoryImage;

var
  lX, lY: Integer;

begin
  Result := TFPMemoryImage.Create(aWidth, aHeight);
  for lY := 0 to aHeight - 1 do
    for lX := 0 to aWidth - 1 do
      Result.Colors[lX, lY] := FewColor((lY * aWidth + lX) mod aCount);
end;


function CreateMonoImage(aWidth, aHeight: Integer): TFPMemoryImage;

var
  lX, lY: Integer;

begin
  Result := TFPMemoryImage.Create(aWidth, aHeight);
  for lY := 0 to aHeight - 1 do
    for lX := 0 to aWidth - 1 do
      if ((lX * 3 + lY * 5) mod 7) < 3 then
        Result.Colors[lX, lY] := colWhite
      else
        Result.Colors[lX, lY] := colBlack;
end;


function CountColors(aImage: TFPCustomImage): Integer;

var
  lList: TStringList;
  lX, lY: Integer;

begin
  lList := TStringList.Create;
  try
    lList.Sorted := True;
    lList.Duplicates := dupIgnore;
    for lY := 0 to aImage.Height - 1 do
      for lX := 0 to aImage.Width - 1 do
        lList.Add(ColorToStr(aImage.Colors[lX, lY]));
    Result := lList.Count;
  finally
    lList.Free;
  end;
end;


function ColorsClose(const aColor1, aColor2: TFPColor; aTolerance: Word): Boolean;

begin
  Result := (Abs(Integer(aColor1.Red) - aColor2.Red) <= aTolerance)
    and (Abs(Integer(aColor1.Green) - aColor2.Green) <= aTolerance)
    and (Abs(Integer(aColor1.Blue) - aColor2.Blue) <= aTolerance)
    and (Abs(Integer(aColor1.Alpha) - aColor2.Alpha) <= aTolerance);
end;


procedure AssertColorsEqual(const aMessage: String; const aExpected, aActual: TFPColor; aTolerance: Word);

begin
  if not ColorsClose(aExpected, aActual, aTolerance) then
    TAssert.Fail(Format('%s: expected %s, got %s (tolerance %d)',
      [aMessage, ColorToStr(aExpected), ColorToStr(aActual), aTolerance]));
end;


procedure AssertImagesEqual(const aMessage: String; aExpected, aActual: TFPCustomImage; aTolerance: Word; aIgnoreAlpha: Boolean);

var
  lX, lY: Integer;
  lExpected, lActual: TFPColor;

begin
  TAssert.AssertNotNull(aMessage + ': image exists', aActual);
  TAssert.AssertEquals(aMessage + ': width', aExpected.Width, aActual.Width);
  TAssert.AssertEquals(aMessage + ': height', aExpected.Height, aActual.Height);
  for lY := 0 to aExpected.Height - 1 do
    for lX := 0 to aExpected.Width - 1 do
      begin
      lExpected := aExpected.Colors[lX, lY];
      lActual := aActual.Colors[lX, lY];
      if aIgnoreAlpha then
        begin
        lExpected.Alpha := alphaOpaque;
        lActual.Alpha := alphaOpaque;
        end;
      if not ColorsClose(lExpected, lActual, aTolerance) then
        TAssert.Fail(Format('%s: pixel (%d,%d): expected %s, got %s (tolerance %d)',
          [aMessage, lX, lY, ColorToStr(lExpected), ColorToStr(lActual), aTolerance]));
      end;
end;


function ImagePSNR(aExpected, aActual: TFPCustomImage): Double;

var
  lX, lY: Integer;
  lSum, lDiff: Double;
  lExpected, lActual: TFPColor;

begin
  lSum := 0;
  for lY := 0 to aExpected.Height - 1 do
    for lX := 0 to aExpected.Width - 1 do
      begin
      lExpected := aExpected.Colors[lX, lY];
      lActual := aActual.Colors[lX, lY];
      lDiff := (Integer(lExpected.Red) - lActual.Red) / 257;
      lSum := lSum + Sqr(lDiff);
      lDiff := (Integer(lExpected.Green) - lActual.Green) / 257;
      lSum := lSum + Sqr(lDiff);
      lDiff := (Integer(lExpected.Blue) - lActual.Blue) / 257;
      lSum := lSum + Sqr(lDiff);
      end;
  lSum := lSum / (3.0 * aExpected.Width * aExpected.Height);
  if lSum = 0 then
    Result := 100
  else
    Result := 10 * Ln(Sqr(255.0) / lSum) / Ln(10);
end;


function WriteImage(aImage: TFPCustomImage; aWriter: TFPCustomImageWriter; aStream: TStream): Int64;

var
  lStart: Int64;

begin
  lStart := aStream.Position;
  aImage.SaveToStream(aStream, aWriter, False);
  Result := aStream.Position - lStart;
end;


function RoundTrip(aImage: TFPCustomImage; aWriter: TFPCustomImageWriter; aReader: TFPCustomImageReader): TFPMemoryImage;

var
  lStream: TMemoryStream;

begin
  lStream := TMemoryStream.Create;
  try
    WriteImage(aImage, aWriter, lStream);
    lStream.Position := 0;
    Result := TFPMemoryImage.Create(0, 0);
    try
      Result.LoadFromStream(lStream, aReader);
    except
      Result.Free;
      raise;
    end;
  finally
    lStream.Free;
  end;
end;


function BytesStream(const aBytes: array of Byte): TMemoryStream;

begin
  Result := TMemoryStream.Create;
  if Length(aBytes) > 0 then
    Result.WriteBuffer(aBytes[0], Length(aBytes));
  Result.Position := 0;
end;


procedure AssertRaises(const aMessage: String; aClass: ExceptClass; aMethod: TRunMethod);

var
  lRaised: String;

begin
  lRaised := '';
  try
    aMethod;
  except
    on E: Exception do
      if E is aClass then
        Exit
      else
        lRaised := E.ClassName + ': ' + E.Message;
  end;
  if lRaised = '' then
    TAssert.Fail(Format('%s: %s expected, but nothing was raised', [aMessage, aClass.ClassName]))
  else
    TAssert.Fail(Format('%s: %s expected, but got %s', [aMessage, aClass.ClassName, lRaised]));
end;


procedure AssertNoLeak(const aMessage: String; aMethod: TRunMethod);

var
  lBefore, lAfter: PtrUInt;

begin
  aMethod;
  lBefore := GetFPCHeapStatus.CurrHeapUsed;
  aMethod;
  lAfter := GetFPCHeapStatus.CurrHeapUsed;
  if lAfter <> lBefore then
    TAssert.Fail(Format('%s: %d bytes of heap not freed', [aMessage, PtrInt(lAfter - lBefore)]));
end;


end.
