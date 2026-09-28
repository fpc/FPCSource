{
    Reader of WebP files: lossless and lossy images, animations and their EXIF, ICC and XMP metadata.
    This file is part of the Free Pascal run time library.
    See the file COPYING.FPC, included in this distribution, for details.
}
{$mode objfpc}{$h+}
{$IFNDEF FPC_DOTTEDUNITS}
unit fpreadwebp;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils, System.Types, FpImage, FpImage.ImageList, FpImage.Common.WebP,
  FpImage.WebP.VP8L, FpImage.WebP.VP8;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils, Types, FpImage, FPImageList, webpcomn, fpwebpvp8l, fpwebpvp8;
{$ENDIF FPC_DOTTEDUNITS}

type
  { Reads a WebP image, or the frames of a WebP animation. }
  TFPReaderWebP = class(TFPCustomImageReader)
  private
    FStart: Int64;
    FEnd: Int64;
    FChunks: TWebPChunks;
    FFlags: Byte;
    FAnimated: Boolean;
    FCanvasWidth, FCanvasHeight: Integer;
    FLoopCount: Integer;
    FBackground: TFPColor;
    FFrames: array of Integer;
    FNextFrame: Integer;
    FCompositor: TFPFrameCompositor;
    procedure ReadStructure(aStream: TStream);
    function ChunkBytes(aStream: TStream; const aChunk: TWebPChunk): TBytes;
    procedure DecodeImage(aStream: TStream; const aChunks: TWebPChunks; aWidth, aHeight: Integer;
      aImage: TFPCustomImage);
    procedure ReadMetadata(aStream: TStream; aImage: TFPCustomImage);
  protected
    function InternalCheck(Stream: TStream): Boolean; override;
    procedure InternalRead(Stream: TStream; Img: TFPCustomImage); override;
    class function InternalSize(Stream: TStream): TPoint; override;
    function InternalBeginFrames(Str: TStream): TFPFramesInfo; override;
    function InternalReadFrame(Str: TStream; Img: TFPCustomImage; var aInfo: TFPFrameInfo): Boolean; override;
    procedure InternalEndFrames(Str: TStream); override;
  public
    destructor Destroy; override;
  end;

implementation

destructor TFPReaderWebP.Destroy;

begin
  FCompositor.Free;
  inherited Destroy;
end;


function TFPReaderWebP.InternalCheck(Stream: TStream): Boolean;

var
  lStart: Int64;
  lHeader: packed record
    RIFF: TWebPFourCC;
    Size: LongWord;
    WEBP: TWebPFourCC;
    First: TWebPFourCC;
  end;

begin
  Result := False;
  if Stream = nil then
    exit;
  lStart := Stream.Position;
  try
    Result := (Stream.Read(lHeader, SizeOf(lHeader)) = SizeOf(lHeader)) and (lHeader.RIFF = WebPRIFF)
      and (lHeader.WEBP = WebPWEBP)
      and ((lHeader.First = WebPVP8L) or (lHeader.First = WebPVP8) or (lHeader.First = WebPVP8X));
  finally
    Stream.Position := lStart;
  end;
end;


// Reads the chunks of the file, its canvas and the chunks of its frames.
procedure TFPReaderWebP.ReadStructure(aStream: TStream);

var
  lHeader: packed record
    RIFF: TWebPFourCC;
    Size: LongWord;
    WEBP: TWebPFourCC;
  end;
  lData: TBytes;
  lInfo: TVP8LInfo;
  lSize: Int64;
  i: Integer;

begin
  FStart := aStream.Position;
  aStream.ReadBuffer(lHeader, SizeOf(lHeader));
  if (lHeader.RIFF <> WebPRIFF) or (lHeader.WEBP <> WebPWEBP) then
    raise EWebPError.Create('Not a WebP file');
  lSize := LEtoN(lHeader.Size) - 4;
  if FStart + 12 + lSize > aStream.Size then
    lSize := aStream.Size - FStart - 12;
  FEnd := FStart + 12 + lSize;
  FChunks := WebPReadChunks(aStream, FStart + 12, lSize);
  if Length(FChunks) = 0 then
    raise EWebPError.Create('WebP file without chunks');
  FFlags := 0;
  FAnimated := False;
  FLoopCount := 0;
  FBackground := colTransparent;
  FCanvasWidth := 0;
  FCanvasHeight := 0;
  FFrames := nil;
  if FChunks[0].FourCC = WebPVP8X then
    begin
    if FChunks[0].Size < 10 then
      raise EWebPError.Create('Invalid VP8X chunk');
    lData := ChunkBytes(aStream, FChunks[0]);
    FFlags := lData[0];
    FCanvasWidth := WebPRead24(@lData[4]) + 1;
    FCanvasHeight := WebPRead24(@lData[7]) + 1;
    FAnimated := (FFlags and WebPFlagAnimation) <> 0;
    if Int64(FCanvasWidth) * FCanvasHeight > WebPMaxPixels then
      raise EWebPError.Create('WebP canvas too large');
    end;
  for i := 0 to High(FChunks) do
    if FAnimated then
      begin
      if FChunks[i].FourCC = WebPANMF then
        begin
        SetLength(FFrames, Length(FFrames) + 1);
        FFrames[High(FFrames)] := i;
        end
      else if (FChunks[i].FourCC = WebPANIM) and (FChunks[i].Size >= 6) then
        begin
        lData := ChunkBytes(aStream, FChunks[i]);
        FBackground := WebPPixelToColor(LongWord(lData[0]) or (LongWord(lData[1]) shl 8)
          or (LongWord(lData[2]) shl 16) or (LongWord(lData[3]) shl 24));
        FLoopCount := lData[4] or (lData[5] shl 8);
        end;
      end
    else if ((FChunks[i].FourCC = WebPVP8L) or (FChunks[i].FourCC = WebPVP8)) and (Length(FFrames) = 0) then
      begin
      SetLength(FFrames, 1);
      FFrames[0] := i;
      end;
  if (FCanvasWidth = 0) and (Length(FFrames) > 0) then
    begin
    lData := ChunkBytes(aStream, FChunks[FFrames[0]]);
    if Length(lData) = 0 then
    else if FChunks[FFrames[0]].FourCC = WebPVP8L then
      begin
      if VP8LReadInfo(@lData[0], Length(lData), lInfo) then
        begin
        FCanvasWidth := lInfo.Width;
        FCanvasHeight := lInfo.Height;
        end;
      end
    else if not VP8ReadInfo(@lData[0], Length(lData), FCanvasWidth, FCanvasHeight) then
      begin
      FCanvasWidth := 0;
      FCanvasHeight := 0;
      end;
    end;
end;


function TFPReaderWebP.ChunkBytes(aStream: TStream; const aChunk: TWebPChunk): TBytes;

begin
  Result := nil;
  SetLength(Result, aChunk.Size);
  aStream.Position := aChunk.Offset;
  if aChunk.Size > 0 then
    aStream.ReadBuffer(Result[0], aChunk.Size);
end;


// Decodes the image of the chunks of a still image or of a frame into aImage; it is aWidth x aHeight unless 0.
procedure TFPReaderWebP.DecodeImage(aStream: TStream; const aChunks: TWebPChunks; aWidth, aHeight: Integer;
  aImage: TFPCustomImage);

var
  lData, lAlpha: TBytes;
  lPixels: TWebPPixels;
  lInfo: TVP8LInfo;
  lWidth, lHeight, i, x, y: Integer;
  lFound: Boolean;

begin
  lFound := False;
  lAlpha := nil;
  lWidth := 0;
  lHeight := 0;
  for i := 0 to High(aChunks) do
    if aChunks[i].FourCC = WebPALPH then
      lAlpha := ChunkBytes(aStream, aChunks[i])
    else if aChunks[i].FourCC = WebPVP8L then
      begin
      lData := ChunkBytes(aStream, aChunks[i]);
      if not VP8LReadInfo(@lData[0], Length(lData), lInfo) then
        raise EWebPError.Create('Invalid VP8L chunk');
      if (aWidth > 0) and ((lInfo.Width <> aWidth) or (lInfo.Height <> aHeight)) then
        raise EWebPError.Create('WebP image of another size than its frame or canvas');
      lPixels := VP8LDecode(@lData[0], Length(lData), lInfo);
      lWidth := lInfo.Width;
      lHeight := lInfo.Height;
      lFound := True;
      break;
      end
    else if aChunks[i].FourCC = WebPVP8 then
      begin
      lData := ChunkBytes(aStream, aChunks[i]);
      if not VP8ReadInfo(@lData[0], Length(lData), lWidth, lHeight) then
        raise EWebPError.Create('Invalid VP8 chunk');
      if (aWidth > 0) and ((lWidth <> aWidth) or (lHeight <> aHeight)) then
        raise EWebPError.Create('WebP image of another size than its frame or canvas');
      lPixels := VP8Decode(@lData[0], Length(lData), lWidth, lHeight);
      if Length(lAlpha) > 0 then
        WebPApplyAlpha(@lAlpha[0], Length(lAlpha), lWidth, lHeight, lPixels);
      lFound := True;
      break;
      end;
  if not lFound then
    raise EWebPError.Create('WebP frame without an image');
  aImage.UsePalette := False;
  aImage.SetSize(lWidth, lHeight);
  for y := 0 to lHeight - 1 do
    for x := 0 to lWidth - 1 do
      aImage.Colors[x, y] := WebPPixelToColor(lPixels[y * lWidth + x]);
end;


procedure TFPReaderWebP.ReadMetadata(aStream: TStream; aImage: TFPCustomImage);

var
  i: Integer;

begin
  for i := 0 to High(FChunks) do
    if FChunks[i].FourCC = WebPICCP then
      aImage.Metadata[MetaICC] := ChunkBytes(aStream, FChunks[i])
    else if FChunks[i].FourCC = WebPEXIF then
      aImage.Metadata[MetaExif] := ChunkBytes(aStream, FChunks[i])
    else if FChunks[i].FourCC = WebPXMP then
      aImage.Metadata[MetaXMP] := ChunkBytes(aStream, FChunks[i]);
end;


function TFPReaderWebP.InternalBeginFrames(Str: TStream): TFPFramesInfo;

begin
  FreeAndNil(FCompositor);
  FNextFrame := 0;
  try
    ReadStructure(Str);
  except
    on E: EReadError do
      raise EWebPError.Create('Truncated WebP file: ' + E.Message);
  end;
  Result := DefaultFramesInfo;
  Result.Width := FCanvasWidth;
  Result.Height := FCanvasHeight;
  Result.FrameCount := Length(FFrames);
  if FAnimated then
    begin
    Result.LoopCount := FLoopCount;
    Result.Background := FBackground;
    end;
end;


function TFPReaderWebP.InternalReadFrame(Str: TStream; Img: TFPCustomImage; var aInfo: TFPFrameInfo): Boolean;

var
  lChunk: TWebPChunk;
  lHeader: array[0..15] of Byte;
  lWidth, lHeight: Integer;
  lPlace: TFPFrameInfo;
  lSingle: TWebPChunks;

begin
  Result := FNextFrame < Length(FFrames);
  if not Result then
    exit;
  lChunk := FChunks[FFrames[FNextFrame]];
  try
    if FAnimated then
      begin
      if lChunk.Size < 16 then
        raise EWebPError.Create('Invalid ANMF chunk');
      Str.Position := lChunk.Offset;
      Str.ReadBuffer(lHeader, 16);
      aInfo.Kind := fkAnimation;
      aInfo.Left := 2 * WebPRead24(@lHeader[0]);
      aInfo.Top := 2 * WebPRead24(@lHeader[3]);
      lWidth := WebPRead24(@lHeader[6]) + 1;
      lHeight := WebPRead24(@lHeader[9]) + 1;
      aInfo.Delay := WebPRead24(@lHeader[12]);
      if (lHeader[15] and WebPFrameDispose) <> 0 then
        aInfo.Disposal := fdBackground;
      if (lHeader[15] and WebPFrameNoBlend) = 0 then
        aInfo.Blend := fbOver;
      if (aInfo.Left + lWidth > FCanvasWidth) or (aInfo.Top + lHeight > FCanvasHeight) then
        raise EWebPError.Create('WebP frame outside the canvas');
      DecodeImage(Str, WebPReadChunks(Str, lChunk.Offset + 16, lChunk.Size - 16), lWidth, lHeight, Img);
      if FComposite then
        begin
        if FCompositor = nil then
          FCompositor := TFPFrameCompositor.Create(FCanvasWidth, FCanvasHeight, colTransparent);
        lPlace := aInfo;
        FCompositor.Add(Img, lPlace, Img);
        aInfo.Left := 0;
        aInfo.Top := 0;
        aInfo.Disposal := fdNone;
        aInfo.Blend := fbSource;
        end;
      end
    else
      begin
      lSingle := Copy(FChunks, 0, FFrames[FNextFrame] + 1);
      if FChunks[0].FourCC = WebPVP8X then
        DecodeImage(Str, lSingle, FCanvasWidth, FCanvasHeight, Img)
      else
        DecodeImage(Str, lSingle, 0, 0, Img);
      end;
  except
    on E: EReadError do
      raise EWebPError.Create('Truncated WebP file: ' + E.Message);
  end;
  if FNextFrame = 0 then
    ReadMetadata(Str, Img);
  Inc(FNextFrame);
end;


procedure TFPReaderWebP.InternalEndFrames(Str: TStream);

begin
  FreeAndNil(FCompositor);
  Str.Position := FEnd;
end;


procedure TFPReaderWebP.InternalRead(Stream: TStream; Img: TFPCustomImage);

var
  lInfo: TFPFrameInfo;

begin
  InternalBeginFrames(Stream);
  try
    lInfo := DefaultFrameInfo;
    if not InternalReadFrame(Stream, Img, lInfo) then
      raise EWebPError.Create('WebP file without an image');
  finally
    InternalEndFrames(Stream);
  end;
end;


class function TFPReaderWebP.InternalSize(Stream: TStream): TPoint;

var
  lReader: TFPReaderWebP;

begin
  Result := Point(0, 0);
  lReader := Create;
  try
    try
      lReader.ReadStructure(Stream);
      Result := Point(lReader.FCanvasWidth, lReader.FCanvasHeight);
    except
      on Exception do
        Result := Point(0, 0);
    end;
  finally
    lReader.Free;
  end;
end;


initialization
  ImageHandlers.RegisterImageReader('WebP Format', 'webp', TFPReaderWebP);
end.
