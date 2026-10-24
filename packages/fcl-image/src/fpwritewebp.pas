{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by the Free Pascal development team

    WebP file writer. lossless. Animations, EXIF, ICC and XMP metadata.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}{$h+}
{$IFNDEF FPC_DOTTEDUNITS}
unit fpwritewebp;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils, FpImage, FpImage.Common.WebP, FpImage.WebP.VP8L;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils, FpImage, webpcomn, fpwebpvp8l;
{$ENDIF FPC_DOTTEDUNITS}

type
  { Writes lossless WebP files. }
  TFPWriterWebP = class(TFPCustomImageWriter)
  private
    FFrames: TMemoryStream;
    FAlpha: Boolean;
    FCanvasWidth, FCanvasHeight: Integer;
    FICC, FExif, FXMP: TBytes;
    function Encode(aImage: TFPCustomImage; out aAlpha: Boolean): TBytes;
    procedure TakeMetadata(aImage: TFPCustomImage);
    procedure WriteFile(aStream: TStream; aFlags: Byte; aWidth, aHeight: Integer; const aBody; aBodySize: LongWord;
      const aBodyFourCC: TWebPFourCC);
  protected
    procedure InternalWrite(Stream: TStream; Img: TFPCustomImage); override;
    procedure InternalBeginFrames(Str: TStream; const aInfo: TFPFramesInfo); override;
    procedure InternalWriteFrame(Str: TStream; Img: TFPCustomImage; const aInfo: TFPFrameInfo); override;
    procedure InternalEndFrames(Str: TStream); override;
  public
    destructor Destroy; override;
    // Returns the kinds of frames a WebP holds several of: those of an animation, placed at even offsets.
    class function FrameKinds: TFPFrameKinds; override;
  end;

implementation

destructor TFPWriterWebP.Destroy;

begin
  FFrames.Free;
  inherited Destroy;
end;


class function TFPWriterWebP.FrameKinds: TFPFrameKinds;

begin
  Result := [fkAnimation];
end;


function TFPWriterWebP.Encode(aImage: TFPCustomImage; out aAlpha: Boolean): TBytes;

var
  lPixels: TWebPPixels;
  x, y: Integer;

begin
  lPixels := nil;
  SetLength(lPixels, aImage.Width * aImage.Height);
  aAlpha := False;
  for y := 0 to aImage.Height - 1 do
    for x := 0 to aImage.Width - 1 do
      begin
      lPixels[y * aImage.Width + x] := WebPColorToPixel(aImage.Colors[x, y]);
      if lPixels[y * aImage.Width + x] shr 24 <> $FF then
        aAlpha := True;
      end;
  Result := VP8LEncode(lPixels, aImage.Width, aImage.Height);
end;


procedure TFPWriterWebP.TakeMetadata(aImage: TFPCustomImage);

begin
  FICC := aImage.Metadata[MetaICC];
  FExif := aImage.Metadata[MetaExif];
  FXMP := aImage.Metadata[MetaXMP];
end;


// Writes the RIFF header, VP8X when aFlags or metadata ask for it, ICCP, the body chunks, EXIF and XMP.
procedure TFPWriterWebP.WriteFile(aStream: TStream; aFlags: Byte; aWidth, aHeight: Integer; const aBody;
  aBodySize: LongWord; const aBodyFourCC: TWebPFourCC);

var
  lFlags: Byte;
  lSize: Int64;
  lHeader: array[0..9] of Byte;
  lRiffSize: LongWord;

  function ChunkSize(aDataSize: Int64): Int64;

  begin
    Result := 8 + aDataSize + (aDataSize and 1);
  end;

begin
  lFlags := aFlags;
  if Length(FICC) > 0 then
    lFlags := lFlags or WebPFlagICC;
  if Length(FExif) > 0 then
    lFlags := lFlags or WebPFlagExif;
  if Length(FXMP) > 0 then
    lFlags := lFlags or WebPFlagXMP;
  if (lFlags and (WebPFlagAnimation or WebPFlagICC or WebPFlagExif or WebPFlagXMP)) = 0 then
    lFlags := 0;
  if (aWidth > WebPMax24 + 1) or (aHeight > WebPMax24 + 1) then
    raise EWebPError.Create('WebP canvas too large');
  lSize := 4;
  if lFlags <> 0 then
    lSize := lSize + ChunkSize(10);
  if Length(FICC) > 0 then
    lSize := lSize + ChunkSize(Length(FICC));
  if aBodyFourCC = WebPANMF then
    lSize := lSize + aBodySize
  else
    lSize := lSize + ChunkSize(aBodySize);
  if Length(FExif) > 0 then
    lSize := lSize + ChunkSize(Length(FExif));
  if Length(FXMP) > 0 then
    lSize := lSize + ChunkSize(Length(FXMP));
  if lSize > High(LongWord) - 1 then
    raise EWebPError.Create('WebP file too large');
  aStream.WriteBuffer(WebPRIFF, 4);
  lRiffSize := NtoLE(LongWord(lSize));
  aStream.WriteBuffer(lRiffSize, 4);
  aStream.WriteBuffer(WebPWEBP, 4);
  if lFlags <> 0 then
    begin
    FillChar(lHeader, SizeOf(lHeader), 0);
    lHeader[0] := lFlags;
    WebPWrite24(@lHeader[4], aWidth - 1);
    WebPWrite24(@lHeader[7], aHeight - 1);
    WebPWriteChunk(aStream, WebPVP8X, lHeader, SizeOf(lHeader));
    end;
  if Length(FICC) > 0 then
    WebPWriteChunk(aStream, WebPICCP, FICC[0], Length(FICC));
  if aBodyFourCC = WebPANMF then
    aStream.WriteBuffer(aBody, aBodySize)
  else
    WebPWriteChunk(aStream, aBodyFourCC, aBody, aBodySize);
  if Length(FExif) > 0 then
    WebPWriteChunk(aStream, WebPEXIF, FExif[0], Length(FExif));
  if Length(FXMP) > 0 then
    WebPWriteChunk(aStream, WebPXMP, FXMP[0], Length(FXMP));
end;


procedure TFPWriterWebP.InternalWrite(Stream: TStream; Img: TFPCustomImage);

var
  lData: TBytes;
  lAlpha: Boolean;
  lFlags: Byte;

begin
  lData := Encode(Img, lAlpha);
  TakeMetadata(Img);
  lFlags := 0;
  if lAlpha then
    lFlags := WebPFlagAlpha;
  WriteFile(Stream, lFlags, Img.Width, Img.Height, lData[0], Length(lData), WebPVP8L);
end;


procedure TFPWriterWebP.InternalBeginFrames(Str: TStream; const aInfo: TFPFramesInfo);

begin
  FreeAndNil(FFrames);
  FFrames := TMemoryStream.Create;
  FAlpha := False;
  FCanvasWidth := aInfo.Width;
  FCanvasHeight := aInfo.Height;
end;


procedure TFPWriterWebP.InternalWriteFrame(Str: TStream; Img: TFPCustomImage; const aInfo: TFPFrameInfo);

var
  lData: TBytes;
  lAlpha: Boolean;
  lHeader: array[0..15] of Byte;
  lSize: LongWord;
  lDelay: Cardinal;

begin
  if FramesInfo.FrameCount = 1 then
    begin
    inherited InternalWriteFrame(Str, Img, aInfo);
    exit;
    end;
  if Odd(aInfo.Left) or Odd(aInfo.Top) or (aInfo.Left < 0) or (aInfo.Top < 0) then
    raise EWebPError.Create('A WebP frame is placed at even offsets');
  if aInfo.Disposal = fdPrevious then
    raise EWebPError.Create('WebP has no disposal to the previous frame');
  if (FramesInfo.Width > 0) and ((aInfo.Left + Img.Width > FramesInfo.Width)
     or (aInfo.Top + Img.Height > FramesInfo.Height)) then
    raise EWebPError.Create('WebP frame outside the canvas');
  if FramesWritten = 0 then
    TakeMetadata(Img);
  lData := Encode(Img, lAlpha);
  FAlpha := FAlpha or lAlpha or (aInfo.Blend = fbOver);
  if aInfo.Left + Img.Width > FCanvasWidth then
    FCanvasWidth := aInfo.Left + Img.Width;
  if aInfo.Top + Img.Height > FCanvasHeight then
    FCanvasHeight := aInfo.Top + Img.Height;
  FillChar(lHeader, SizeOf(lHeader), 0);
  WebPWrite24(@lHeader[0], aInfo.Left div 2);
  WebPWrite24(@lHeader[3], aInfo.Top div 2);
  WebPWrite24(@lHeader[6], Img.Width - 1);
  WebPWrite24(@lHeader[9], Img.Height - 1);
  lDelay := aInfo.Delay;
  if lDelay > WebPMax24 then
    lDelay := WebPMax24;
  WebPWrite24(@lHeader[12], lDelay);
  if aInfo.Disposal = fdBackground then
    lHeader[15] := lHeader[15] or WebPFrameDispose;
  if aInfo.Blend = fbSource then
    lHeader[15] := lHeader[15] or WebPFrameNoBlend;
  lSize := 16 + 8 + Length(lData) + (Length(lData) and 1);
  FFrames.WriteBuffer(WebPANMF, 4);
  lSize := NtoLE(lSize);
  FFrames.WriteBuffer(lSize, 4);
  FFrames.WriteBuffer(lHeader, 16);
  WebPWriteChunk(FFrames, WebPVP8L, lData[0], Length(lData));
end;


procedure TFPWriterWebP.InternalEndFrames(Str: TStream);

var
  lBody: TMemoryStream;
  lAnim: array[0..5] of Byte;
  lColor: LongWord;
  lLoop: Integer;
  lFlags: Byte;

begin
  if FramesInfo.FrameCount = 1 then
    exit;
  lBody := TMemoryStream.Create;
  try
    lColor := WebPColorToPixel(FramesInfo.Background);
    lAnim[0] := lColor and $FF;
    lAnim[1] := (lColor shr 8) and $FF;
    lAnim[2] := (lColor shr 16) and $FF;
    lAnim[3] := lColor shr 24;
    lLoop := FramesInfo.LoopCount;
    if lLoop < 0 then
      lLoop := 0
    else if lLoop > High(Word) then
      lLoop := High(Word);
    lAnim[4] := lLoop and $FF;
    lAnim[5] := lLoop shr 8;
    WebPWriteChunk(lBody, WebPANIM, lAnim, SizeOf(lAnim));
    FFrames.Position := 0;
    lBody.CopyFrom(FFrames, FFrames.Size);
    lFlags := WebPFlagAnimation;
    if FAlpha then
      lFlags := lFlags or WebPFlagAlpha;
    WriteFile(Str, lFlags, FCanvasWidth, FCanvasHeight, lBody.Memory^, lBody.Size, WebPANMF);
  finally
    lBody.Free;
    FreeAndNil(FFrames);
  end;
end;


initialization
  ImageHandlers.RegisterImageWriter('WebP Format', 'webp', TFPWriterWebP);
end.
