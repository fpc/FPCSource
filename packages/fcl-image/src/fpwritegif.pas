{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by the Free Pascal development team

    GIF writer for fpImage: one image, or several as an animation.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$IFNDEF FPC_DOTTEDUNITS}
unit FPWriteGif;
{$ENDIF FPC_DOTTEDUNITS}

{$mode objfpc}{$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils, FpImage, FpImage.Quantizer;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils, FPImage, FPQuantizer;
{$ENDIF FPC_DOTTEDUNITS}

const
  { Colours a GIF colour table holds at most. }
  GIFMaxColors = 256;
  { Alpha below which a pixel is written as the transparent index. }
  GIFAlphaThreshold = alphaOpaque div 2;

type
  { The colours of one or more images:
    The 8-bit triples a GIF colour table is written with. }
  TGIFColorTable = class
  private
    FColors: array[0..GIFMaxColors - 1] of Longword;
    FCount: Integer;
    FBuckets: array[0..2 * GIFMaxColors - 1] of Integer;
    FCache: array[0..1023] of Longword;
    FCacheIndex: array[0..1023] of Integer;
    FTransparentIndex: Integer;
    function GetColor(AIndex: Integer): Longword;
    function SlotOf(AValue: Longword): Integer;
  public
    constructor Create;
    // Forgets every colour.
    procedure Clear;
    // The index of a colour, or -1 when the table has no entry for it.
    function IndexOf(AValue: Longword): Integer;
    // Adds a colour and gives its index, or -1 when the table already holds as many as ALimit allows.
    function Add(AValue: Longword; ALimit: Integer): Integer;
    // Adds the entry that stands for a transparent pixel and gives its index. 
    function AddTransparent: Integer;
    // The index of the colour itself, or of the nearest one the table holds. -1 while the table is empty.
    function Nearest(AValue: Longword): Integer;
    // Number of bits per pixel. Two at the least.
    function BitsPerPixel: Integer;
    // Number of colours in the table.
    property Count: Integer read FCount;
    // One of the colours, as a triple of 8-bit channels.
    property Colors[AIndex: Integer]: Longword read GetColor;
    // The index that stands for a transparent pixel, -1 when none does.
    property TransparentIndex: Integer read FTransparentIndex
      write FTransparentIndex;
  end;

  { Packs the pixels of one frame into the bit stream a GIF }
  TGIFLZWWriter = class
  private
    FStream: TStream;
    FBlock: array[0..254] of Byte;
    FBlockCount: Integer;
    FBits: Longword;
    FBitCount: Integer;
    FCodeSize: Integer;
    FCodeWidth: Integer;
    FNextCode: Integer;
    FClearCode: Integer;
    FEndCode: Integer;
    FPrefix: Integer;
    FKeys: array[0..8191] of Longword;
    FCodes: array[0..8191] of Integer;
    procedure AddByte(AValue: Byte);
    procedure FlushBlock;
    procedure ResetTable;
    function SlotOf(AKey: Longword): Integer;
    procedure WriteCode(ACode: Integer);
  public
    constructor Create(AStream: TStream; ACodeSize: Integer);
    // Takes the next pixel of the frame.
    procedure Add(APixel: Byte);
    // Writes what is left, the end code and the block terminator.
    procedure Finish;
  end;

  { Writes an fpImage as a GIF:
    ImageWrite writes the one image, like all other writers; 
    ImagesWrite writes several as the frames of one animation, 
    using a shared colour table. }
  TFPWriterGIF = class(TFPCustomImageWriter)
  private
    FDelay: Word;
    FLoopCount: Word;
    FTransparent: Boolean;
    FTable: TGIFColorTable;
    function NeedsTransparent(const AImages: array of TFPCustomImage): Boolean;
    function AddColorsOf(AImage: TFPCustomImage; ALimit: Integer;
      ASkipTransparent: Boolean): Boolean;
    procedure QuantizeColors(const AImages: array of TFPCustomImage;
      ALimit: Integer);
    procedure BuildTable(const AImages: array of TFPCustomImage);
    procedure WriteScreen(AStream: TStream; AWidth, AHeight: Integer;
      AFrames: Integer);
    procedure WriteControl(AStream: TStream; ADelay: Word);
    procedure WriteFrame(AStream: TStream; AImage: TFPCustomImage;
      ADelay: Word; AFrames: Integer);
    procedure WriteFrames(AStream: TStream;
      const AImages: array of TFPCustomImage; const ADelays: array of Word);
  protected
    procedure InternalWrite(Str: TStream; Img: TFPCustomImage); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    // Writes several images as the frames of one animation using fixed Delay property.
    procedure ImagesWrite(AStream: TStream; const AImages: array of TFPCustomImage); overload;
    // The same, with a delay of its own for each frame in hundredths of a second. 
    // If aDelays contains less values than the number of images, remaining frames use Delay instead.
    procedure ImagesWrite(AStream: TStream;  const AImages: array of TFPCustomImage; const ADelays: array of Word); overload;
    // Hundredths of a second a frame of an animation is shown for.
    property Delay: Word read FDelay write FDelay;
    // Times an animation is played, nought for over and over again.
    property LoopCount: Word read FLoopCount write FLoopCount;
    // Whether a pixel of little alpha is written as transparent. When false, the pixel is written using the exact value.
    property Transparent: Boolean read FTransparent write FTransparent;
  end;

// The triple a colour is written as, its channels reduced to 8 bits.
function GIFColorOf(const AColor: TFPColor): Longword;

implementation

const
  GIFSignature = 'GIF89a';
  { The standard application extension for an animation }
  GIFLoopIdentifier = 'NETSCAPE2.0';
  GIFExtensionIntroducer = $21;
  GIFImageSeparator = $2C;
  GIFTrailer = $3B;
  GIFGraphicControlLabel = $F9;
  GIFApplicationLabel = $FF;
  GIFMaxCode = 4095;
  GIFMaxCodeWidth = 12;

  SErrNoImagesToWrite = 'No image to write';
  SErrImageHasNoExtent = 'Image %d is %d by %d, which has no extent';
  SErrImageTooLarge = 'Image %d is %d by %d, larger than a GIF holds';

type
  { What a reader is to do with a frame before it draws the next one.
    Similar to TGifDisposal of fpreadgif }
  TGIFWriteDisposal = (wdUnspecified, wdKeep, wdBackground, wdPrevious);

function GIFColorOf(const AColor: TFPColor): Longword;

begin
  Result := (Longword(AColor.Red shr 8) shl 16)
         or (Longword(AColor.Green shr 8) shl 8)
         or Longword(AColor.Blue shr 8);
end;


// Writes a count, its low byte first, as every one in a GIF is written.
procedure WriteGIFWord(AStream: TStream; AValue: Word);

begin
  AStream.WriteByte(AValue and $FF);
  AStream.WriteByte((AValue shr 8) and $FF);
end;


{ TGIFColorTable }

constructor TGIFColorTable.Create;

begin
  inherited Create;
  Clear;
end;


procedure TGIFColorTable.Clear;

var
  I: Integer;

begin
  FCount := 0;
  FTransparentIndex := -1;
  for I := 0 to High(FBuckets) do
    FBuckets[I] := -1;
  for I := 0 to High(FCacheIndex) do
    FCacheIndex[I] := -1;
end;


function TGIFColorTable.GetColor(AIndex: Integer): Longword;

begin
  Result := 0;
  if (AIndex >= 0) and (AIndex < FCount) then
    Result := FColors[AIndex];
end;


// Spreads the channels of a colour over the low bits
function GIFHashOf(AValue: Longword): Longword;

begin
  Result := AValue xor (AValue shr 9) xor (AValue shr 17);
end;


function TGIFColorTable.SlotOf(AValue: Longword): Integer;

begin
  Result := GIFHashOf(AValue) and High(FBuckets);
  while (FBuckets[Result] >= 0) and (FColors[FBuckets[Result]] <> AValue) do
    Result := (Result + 1) and High(FBuckets);
end;


function TGIFColorTable.IndexOf(AValue: Longword): Integer;

begin
  Result := FBuckets[SlotOf(AValue)];
end;


function TGIFColorTable.Add(AValue: Longword; ALimit: Integer): Integer;

var
  lSlot: Integer;

begin
  lSlot := SlotOf(AValue);
  Result := FBuckets[lSlot];
  if Result >= 0 then
    Exit;
  if (FCount >= ALimit) or (FCount >= GIFMaxColors) then
    Exit(-1);
  FColors[FCount] := AValue;
  FBuckets[lSlot] := FCount;
  Result := FCount;
  Inc(FCount);
end;


function TGIFColorTable.AddTransparent: Integer;

begin
  Result := -1;
  if FCount >= GIFMaxColors then
    Exit;
  FColors[FCount] := 0;
  Result := FCount;
  FTransparentIndex := Result;
  Inc(FCount);
end;


function TGIFColorTable.Nearest(AValue: Longword): Integer;

var
  lSlot, lBest, lDistance, lNearest, I, D: Integer;

begin
  Result := FBuckets[SlotOf(AValue)];
  if (Result >= 0) or (FCount = 0) then
    Exit;
  lSlot := GIFHashOf(AValue) and High(FCacheIndex);
  if (FCacheIndex[lSlot] >= 0) and (FCache[lSlot] = AValue) then
    Exit(FCacheIndex[lSlot]);
  lBest := 0;
  lNearest := MaxInt;
  for I := 0 to FCount - 1 do
    begin
    if I = FTransparentIndex then
      Continue;
    D := Integer((FColors[I] shr 16) and $FF) - Integer((AValue shr 16) and $FF);
    lDistance := D * D;
    D := Integer((FColors[I] shr 8) and $FF) - Integer((AValue shr 8) and $FF);
    lDistance := lDistance + D * D;
    D := Integer(FColors[I] and $FF) - Integer(AValue and $FF);
    lDistance := lDistance + D * D;
    if lDistance < lNearest then
      begin
      lNearest := lDistance;
      lBest := I;
      if lDistance = 0 then
        Break;
      end;
    end;
  FCache[lSlot] := AValue;
  FCacheIndex[lSlot] := lBest;
  Result := lBest;
end;


function TGIFColorTable.BitsPerPixel: Integer;

begin
  Result := 2;
  while (Result < 8) and ((1 shl Result) < FCount) do
    Inc(Result);
end;


{ TGIFLZWWriter }

constructor TGIFLZWWriter.Create(AStream: TStream; ACodeSize: Integer);

begin
  inherited Create;
  FStream := AStream;
  FCodeSize := ACodeSize;
  FClearCode := 1 shl ACodeSize;
  FEndCode := FClearCode + 1;
  FBlockCount := 0;
  FBits := 0;
  FBitCount := 0;
  ResetTable;
  WriteCode(FClearCode);
end;


procedure TGIFLZWWriter.ResetTable;

var
  I: Integer;

begin
  for I := 0 to High(FKeys) do
    FKeys[I] := 0;
  FCodeWidth := FCodeSize + 1;
  FNextCode := FClearCode + 2;
  FPrefix := -1;
end;


procedure TGIFLZWWriter.AddByte(AValue: Byte);

begin
  FBlock[FBlockCount] := AValue;
  Inc(FBlockCount);
  if FBlockCount = Length(FBlock) then
    FlushBlock;
end;


procedure TGIFLZWWriter.FlushBlock;

begin
  if FBlockCount = 0 then
    Exit;
  FStream.WriteByte(FBlockCount);
  FStream.WriteBuffer(FBlock[0], FBlockCount);
  FBlockCount := 0;
end;


procedure TGIFLZWWriter.WriteCode(ACode: Integer);

begin
  FBits := FBits or (Longword(ACode) shl FBitCount);
  Inc(FBitCount, FCodeWidth);
  while FBitCount >= 8 do
    begin
    AddByte(FBits and $FF);
    FBits := FBits shr 8;
    Dec(FBitCount, 8);
    end;
end;


function TGIFLZWWriter.SlotOf(AKey: Longword): Integer;

begin
  Result := (AKey xor (AKey shr 7) xor (AKey shr 13)) and High(FKeys);
  while (FKeys[Result] <> 0) and (FKeys[Result] <> AKey + 1) do
    Result := (Result + 1) and High(FKeys);
end;


procedure TGIFLZWWriter.Add(APixel: Byte);

var
  lKey: Longword;
  lSlot: Integer;

begin
  if FPrefix < 0 then
    begin
    FPrefix := APixel;
    Exit;
    end;
  lKey := (Longword(FPrefix) shl 8) or APixel;
  lSlot := SlotOf(lKey);
  if FKeys[lSlot] = lKey + 1 then
    begin
    FPrefix := FCodes[lSlot];
    Exit;
    end;
  WriteCode(FPrefix);
  if FNextCode <= GIFMaxCode then
    begin
    // A reader takes the entry a code stands for only once it has read
    // the code after it, so it is one entry behind while it reads. 
    if (FNextCode >= (1 shl FCodeWidth))
       and (FCodeWidth < GIFMaxCodeWidth) then
      Inc(FCodeWidth);
    FKeys[lSlot] := lKey + 1;
    FCodes[lSlot] := FNextCode;
    Inc(FNextCode);
    end
  else
    begin
    WriteCode(FClearCode);
    ResetTable;
    end;
  FPrefix := APixel;
end;


procedure TGIFLZWWriter.Finish;

begin
  if FPrefix >= 0 then
    WriteCode(FPrefix);
  WriteCode(FEndCode);
  if FBitCount > 0 then
    AddByte(FBits and $FF);
  FlushBlock;
  FStream.WriteByte(0);
end;


{ TFPWriterGIF }

constructor TFPWriterGIF.Create;

begin
  inherited Create;
  FDelay := 10;
  FLoopCount := 0;
  FTransparent := True;
  FTable := TGIFColorTable.Create;
end;


destructor TFPWriterGIF.Destroy;

begin
  FTable.Free;
  inherited Destroy;
end;


function TFPWriterGIF.NeedsTransparent(
  const AImages: array of TFPCustomImage): Boolean;

var
  I, X, Y: Integer;

begin
  Result := False;
  if not FTransparent then
    Exit;
  for I := 0 to High(AImages) do
    for Y := 0 to AImages[I].Height - 1 do
      for X := 0 to AImages[I].Width - 1 do
        if AImages[I].Colors[X, Y].Alpha < GIFAlphaThreshold then
          Exit(True);
end;


function TFPWriterGIF.AddColorsOf(AImage: TFPCustomImage; ALimit: Integer;
  ASkipTransparent: Boolean): Boolean;

var
  X, Y: Integer;
  lColor: TFPColor;

begin
  for Y := 0 to AImage.Height - 1 do
    for X := 0 to AImage.Width - 1 do
      begin
      lColor := AImage.Colors[X, Y];
      if ASkipTransparent and (lColor.Alpha < GIFAlphaThreshold) then
        Continue;
      if FTable.Add(GIFColorOf(lColor), ALimit) < 0 then
        Exit(False);
      end;
  Result := True;
end;


procedure TFPWriterGIF.QuantizeColors(const AImages: array of TFPCustomImage;
  ALimit: Integer);

var
  lQuantizer: TFPColorQuantizer;
  lPalette: TFPPalette;
  I: Integer;

begin
  lQuantizer := TFPMedianCutQuantizer.Create;
  try
    lQuantizer.ColorNumber := ALimit;
    for I := 0 to High(AImages) do
      lQuantizer.Add(AImages[I]);
    lPalette := lQuantizer.Quantize;
    try
      for I := 0 to lPalette.Count - 1 do
        FTable.Add(GIFColorOf(lPalette[I]), ALimit);
    finally
      lPalette.Free;
    end;
  finally
    lQuantizer.Free;
  end;
end;


procedure TFPWriterGIF.BuildTable(const AImages: array of TFPCustomImage);

var
  lLimit, I: Integer;
  lTransparent, lExact: Boolean;

begin
  // A transparent pixel takes an entry of its own. So one less item for actual colours.
  lTransparent := NeedsTransparent(AImages);
  lLimit := GIFMaxColors;
  if lTransparent then
    Dec(lLimit);
  FTable.Clear;
  lExact := True;
  for I := 0 to High(AImages) do
    if not AddColorsOf(AImages[I], lLimit, lTransparent) then
      begin
      lExact := False;
      Break;
      end;
  if not lExact then
    begin
    // More colours than a table takes, so they are quantized down to it.
    FTable.Clear;
    QuantizeColors(AImages, lLimit);
    end;
  if lTransparent then
    FTable.AddTransparent;
end;


procedure TFPWriterGIF.WriteScreen(AStream: TStream; AWidth, AHeight: Integer;
  AFrames: Integer);

var
  lBits, lEntries, I: Integer;
  lText: String;
  lColor: Longword;

begin
  lText := GIFSignature;
  AStream.WriteBuffer(lText[1], Length(lText));
  WriteGIFWord(AStream, AWidth);
  WriteGIFWord(AStream, AHeight);
  lBits := FTable.BitsPerPixel;
  AStream.WriteByte($80 or ((lBits - 1) shl 4) or (lBits - 1));
  AStream.WriteByte(0);
  AStream.WriteByte(0);
  lEntries := 1 shl lBits;
  for I := 0 to lEntries - 1 do
    begin
    lColor := FTable.Colors[I];
    AStream.WriteByte((lColor shr 16) and $FF);
    AStream.WriteByte((lColor shr 8) and $FF);
    AStream.WriteByte(lColor and $FF);
    end;
  if AFrames < 2 then
    Exit;
  // The application extension that says how often to play an animation.
  AStream.WriteByte(GIFExtensionIntroducer);
  AStream.WriteByte(GIFApplicationLabel);
  lText := GIFLoopIdentifier;
  AStream.WriteByte(Length(lText));
  AStream.WriteBuffer(lText[1], Length(lText));
  AStream.WriteByte(3);
  AStream.WriteByte(1);
  WriteGIFWord(AStream, FLoopCount);
  AStream.WriteByte(0);
end;


procedure TFPWriterGIF.WriteControl(AStream: TStream; ADelay: Word);

var
  lDisposal: TGIFWriteDisposal;
  lPacked: Byte;

begin
  // A frame that has transparent pixels needs what is under it taken away first
  if FTable.TransparentIndex >= 0 then
    lDisposal := wdBackground
  else
    lDisposal := wdKeep;
  lPacked := Ord(lDisposal) shl 2;
  if FTable.TransparentIndex >= 0 then
    lPacked := lPacked or 1;
  AStream.WriteByte(GIFExtensionIntroducer);
  AStream.WriteByte(GIFGraphicControlLabel);
  AStream.WriteByte(4);
  AStream.WriteByte(lPacked);
  WriteGIFWord(AStream, ADelay);
  if FTable.TransparentIndex >= 0 then
    AStream.WriteByte(FTable.TransparentIndex)
  else
    AStream.WriteByte(0);
  AStream.WriteByte(0);
end;


procedure TFPWriterGIF.WriteFrame(AStream: TStream; AImage: TFPCustomImage;
  ADelay: Word; AFrames: Integer);

var
  lLZW: TGIFLZWWriter;
  lCodeSize: Integer;
  X, Y, lIndex: Integer;
  lColor: TFPColor;

begin
  if (AFrames > 1) or (FTable.TransparentIndex >= 0) then
    WriteControl(AStream, ADelay);
  AStream.WriteByte(GIFImageSeparator);
  WriteGIFWord(AStream, 0);
  WriteGIFWord(AStream, 0);
  WriteGIFWord(AStream, AImage.Width);
  WriteGIFWord(AStream, AImage.Height);
  // No table of its own.
  AStream.WriteByte(0);
  lCodeSize := FTable.BitsPerPixel;
  AStream.WriteByte(lCodeSize);
  lLZW := TGIFLZWWriter.Create(AStream, lCodeSize);
  try
    for Y := 0 to AImage.Height - 1 do
      for X := 0 to AImage.Width - 1 do
        begin
        lColor := AImage.Colors[X, Y];
        if (FTable.TransparentIndex >= 0)
           and (lColor.Alpha < GIFAlphaThreshold) then
          lIndex := FTable.TransparentIndex
        else
          lIndex := FTable.Nearest(GIFColorOf(lColor));
        lLZW.Add(Byte(lIndex));
        end;
    lLZW.Finish;
  finally
    lLZW.Free;
  end;
end;


procedure TFPWriterGIF.WriteFrames(AStream: TStream;
  const AImages: array of TFPCustomImage; const ADelays: array of Word);

var
  lWidth, lHeight, I: Integer;
  lDelay: Word;

begin
  if Length(AImages) = 0 then
    raise FPImageException.Create(SErrNoImagesToWrite);
  lWidth := 0;
  lHeight := 0;
  for I := 0 to High(AImages) do
    begin
    if (AImages[I].Width <= 0) or (AImages[I].Height <= 0) then
      raise FPImageException.CreateFmt(SErrImageHasNoExtent,
        [I, AImages[I].Width, AImages[I].Height]);
    if (AImages[I].Width > High(Word)) or (AImages[I].Height > High(Word)) then
      raise FPImageException.CreateFmt(SErrImageTooLarge,
        [I, AImages[I].Width, AImages[I].Height]);
    if AImages[I].Width > lWidth then
      lWidth := AImages[I].Width;
    if AImages[I].Height > lHeight then
      lHeight := AImages[I].Height;
    end;
  BuildTable(AImages);
  WriteScreen(AStream, lWidth, lHeight, Length(AImages));
  for I := 0 to High(AImages) do
    begin
    lDelay := FDelay;
    if I <= High(ADelays) then
      lDelay := ADelays[I];
    WriteFrame(AStream, AImages[I], lDelay, Length(AImages));
    end;
  AStream.WriteByte(GIFTrailer);
end;


procedure TFPWriterGIF.InternalWrite(Str: TStream; Img: TFPCustomImage);

var
  lImages: array[0..0] of TFPCustomImage;
  lDelays: array[0..0] of Word;

begin
  lImages[0] := Img;
  lDelays[0] := 0;
  WriteFrames(Str, lImages, lDelays);
end;


procedure TFPWriterGIF.ImagesWrite(AStream: TStream;
  const AImages: array of TFPCustomImage);

var
  lDelays: array of Word;

begin
  lDelays := nil;
  WriteFrames(AStream, AImages, lDelays);
end;


procedure TFPWriterGIF.ImagesWrite(AStream: TStream;
  const AImages: array of TFPCustomImage; const ADelays: array of Word);

begin
  WriteFrames(AStream, AImages, ADelays);
end;


initialization
  ImageHandlers.RegisterImageWriter('GIF Format', 'gif', TFPWriterGIF);
end.
