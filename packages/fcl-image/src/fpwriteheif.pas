{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by the Free Pascal development team

    HEIF (HEIC, AVIF) image writer, using libheif.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}{$h+}
{$IFNDEF FPC_DOTTEDUNITS}
unit fpwriteheif;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils, System.Types, System.Math, System.CTypes, FpImage, Api.HEIF, FpImage.Common.HEIF;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils, Types, Math, ctypes, FpImage, libheif, heifcomn;
{$ENDIF FPC_DOTTEDUNITS}

type
  { Writes HEIF files through libheif, with one top-level image for each frame; the first is the primary image. }
  TFPWriterHEIF = class(TFPCustomImageWriter)
  private
    FCompression: THEIFCompression;
    FQuality: Integer;
    FLossless: Boolean;
    FUseAlpha: Boolean;
    FContext: Pheif_context;
    FEncoder: Pheif_encoder;
    FSizes: array of TPoint;
    FCheckSizes: Boolean;
    procedure OpenContext;
    procedure CheckSizes(aData: TMemoryStream);
    procedure CloseContext;
    procedure EncodeImage(aImage: TFPCustomImage);
    procedure WriteContext(aStream: TStream);
  protected
    procedure InternalWrite(Stream: TStream; Img: TFPCustomImage); override;
    procedure InternalBeginFrames(Str: TStream; const aInfo: TFPFramesInfo); override;
    procedure InternalWriteFrame(Str: TStream; Img: TFPCustomImage; const aInfo: TFPFrameInfo); override;
    procedure InternalEndFrames(Str: TStream); override;
  public
    // Creates a writer of HEVC images with a quality of 90.
    constructor Create; override;
    // Frees the writer and the frames not written yet.
    destructor Destroy; override;
    // Returns [fkPage]: every frame is a top-level image.
    class function FrameKinds: TFPFrameKinds; override;
    // The compression of the images; the encoder for it must be present in libheif.
    property Compression: THEIFCompression read FCompression write FCompression;
    // Quality of lossy compression, from 0 to 100.
    property Quality: Integer read FQuality write FQuality;
    // Compresses the samples without loss at full chroma resolution; libheif converts RGB to YCbCr first,
    // which can change a component by 1.
    property Lossless: Boolean read FLossless write FLossless;
    // Adds an alpha channel when the image has a pixel that is not opaque.
    property UseAlpha: Boolean read FUseAlpha write FUseAlpha;
  end;

  { Writes AVIF files: HEIF files of AV1 images. }
  TFPWriterAVIF = class(TFPWriterHEIF)
  public
    // Creates a writer of AV1 images.
    constructor Create; override;
  end;

implementation

const
  HEIFProfile = 'prof';
  // Images of AV1 with a smaller width or height are decoded again after writing, to check their size.
  AV1MinSize = 16;

constructor TFPWriterHEIF.Create;

begin
  inherited Create;
  FCompression := hcHEVC;
  FQuality := 90;
  FUseAlpha := True;
end;


destructor TFPWriterHEIF.Destroy;

var
  lMask: TFPUExceptionMask;

begin
  lMask := HEIFEnterLibrary;
  try
    CloseContext;
  finally
    HEIFLeaveLibrary(lMask);
  end;
  inherited Destroy;
end;


class function TFPWriterHEIF.FrameKinds: TFPFrameKinds;

begin
  Result := [fkPage];
end;


// Returns True when aImage has a pixel that is not opaque.
function HasTranslucentPixel(aImage: TFPCustomImage): Boolean;

var
  lX, lY: Integer;

begin
  Result := True;
  for lY := 0 to aImage.Height - 1 do
    for lX := 0 to aImage.Width - 1 do
      if aImage.Colors[lX, lY].Alpha <> AlphaOpaque then
        Exit;
  Result := False;
end;


// Passes the data libheif writes to the stream given as aUserData.
function WriteToStream(aContext: Pheif_context; aData: Pointer; aSize: csize_t; aUserData: Pointer): Theif_error; cdecl;

begin
  Result.code := heif_error_Ok;
  Result.subcode := heif_suberror_Unspecified;
  Result.message := pcchar(PAnsiChar('Success'));
  try
    if aSize > 0 then
      TStream(aUserData).WriteBuffer(aData^, aSize);
  except
    Result.code := heif_error_Encoding_error;
    Result.subcode := heif_suberror_Cannot_write_output_data;
    Result.message := pcchar(PAnsiChar('Cannot write the HEIF data to the stream'));
  end;
end;


// Creates the context and the encoder of the file.
procedure TFPWriterHEIF.OpenContext;

var
  lError: Theif_error;

begin
  CloseContext;
  if not HEIFCanEncode(FCompression) then
    raise FPImageException.CreateFmt('libheif has no %s encoder', [HEIFCompressionNames[FCompression]]);
  FContext := heif_context_alloc();
  if FContext = nil then
    raise FPImageException.Create('Cannot create a libheif context');
  CheckHEIF(heif_context_get_encoder_for_format(FContext, HEIFCompressionFormats[FCompression], @FEncoder));
  if FLossless then
    begin
    CheckHEIF(heif_encoder_set_lossless(FEncoder, 1));
    lError := heif_encoder_set_parameter_string(FEncoder, pcchar(PAnsiChar('chroma')), pcchar(PAnsiChar('444')));
    if (lError.code <> heif_error_Ok) and (lError.subcode <> heif_suberror_Unsupported_parameter) then
      CheckHEIF(lError);
    end
  else
    CheckHEIF(heif_encoder_set_lossy_quality(FEncoder, FQuality));
end;


procedure TFPWriterHEIF.CloseContext;

begin
  FSizes := nil;
  FCheckSizes := False;
  if FEncoder <> nil then
    heif_encoder_release(FEncoder);
  FEncoder := nil;
  if FContext <> nil then
    heif_context_free(FContext);
  FContext := nil;
end;


// Adds aImage as the next top-level image, with its EXIF, XMP and ICC metadata.
procedure TFPWriterHEIF.EncodeImage(aImage: TFPCustomImage);

var
  lAlpha: Boolean;
  lImage: Pheif_image;
  lHandle: Pheif_image_handle;
  lOptions: Pheif_encoding_options;
  lPlane, lPixel: PByte;
  lStride, lX, lY: Integer;
  lColor: TFPColor;
  lData: TBytes;

begin
  if (aImage.Width <= 0) or (aImage.Height <= 0) then
    raise FPImageException.Create('Cannot write an empty HEIF image');
  lAlpha := FUseAlpha and HasTranslucentPixel(aImage);
  if lAlpha then
    CheckHEIF(heif_image_create(aImage.Width, aImage.Height, heif_colorspace_RGB, heif_chroma_interleaved_RGBA, @lImage))
  else
    CheckHEIF(heif_image_create(aImage.Width, aImage.Height, heif_colorspace_RGB, heif_chroma_interleaved_RGB, @lImage));
  lHandle := nil;
  lOptions := nil;
  try
    CheckHEIF(heif_image_add_plane(lImage, heif_channel_interleaved, aImage.Width, aImage.Height, 8));
    lPlane := PByte(heif_image_get_plane(lImage, heif_channel_interleaved, @lStride));
    for lY := 0 to aImage.Height - 1 do
      begin
      lPixel := lPlane + PtrInt(lY) * lStride;
      for lX := 0 to aImage.Width - 1 do
        begin
        lColor := aImage.Colors[lX, lY];
        lPixel[0] := Hi(lColor.Red);
        lPixel[1] := Hi(lColor.Green);
        lPixel[2] := Hi(lColor.Blue);
        if lAlpha then
          begin
          lPixel[3] := Hi(lColor.Alpha);
          Inc(lPixel, 4);
          end
        else
          Inc(lPixel, 3);
        end;
      end;
    lData := aImage.Metadata[MetaICC];
    if Length(lData) > 0 then
      CheckHEIF(heif_image_set_raw_color_profile(lImage, pcchar(PAnsiChar(HEIFProfile)), @lData[0], Length(lData)));
    lOptions := heif_encoding_options_alloc();
    lOptions^.save_alpha_channel := Ord(lAlpha);
    CheckHEIF(heif_context_encode_image(FContext, lImage, FEncoder, lOptions, @lHandle));
    SetLength(FSizes, Length(FSizes) + 1);
    FSizes[High(FSizes)] := Point(aImage.Width, aImage.Height);
    if (FCompression = hcAV1) and ((aImage.Width < AV1MinSize) or (aImage.Height < AV1MinSize)) then
      FCheckSizes := True;
    lData := aImage.Metadata[MetaExif];
    if Length(lData) > 0 then
      CheckHEIF(heif_context_add_exif_metadata(FContext, lHandle, @lData[0], Length(lData)));
    lData := aImage.Metadata[MetaXMP];
    if Length(lData) > 0 then
      CheckHEIF(heif_context_add_XMP_metadata(FContext, lHandle, @lData[0], Length(lData)));
  finally
    if lHandle <> nil then
      heif_image_handle_release(lHandle);
    if lOptions <> nil then
      heif_encoding_options_free(lOptions);
    heif_image_release(lImage);
  end;
end;


// Raises FPImageException when a top-level image of the file in aData does not decode to the size it was given.
procedure TFPWriterHEIF.CheckSizes(aData: TMemoryStream);

var
  lContext: Pheif_context;
  lIDs: array of Theif_item_id;
  lHandle: Pheif_image_handle;
  lImage: Pheif_image;
  lCount, lWidth, lHeight, I: Integer;

begin
  lContext := heif_context_alloc();
  try
    CheckHEIF(heif_context_read_from_memory_without_copy(lContext, aData.Memory, aData.Size, nil));
    lCount := heif_context_get_number_of_top_level_images(lContext);
    SetLength(lIDs, lCount);
    if lCount > 0 then
      lCount := heif_context_get_list_of_top_level_image_IDs(lContext, @lIDs[0], lCount);
    for I := 0 to lCount - 1 do
      begin
      if (I > High(FSizes)) or ((FSizes[I].X >= AV1MinSize) and (FSizes[I].Y >= AV1MinSize)) then
        Continue;
      CheckHEIF(heif_context_get_image_handle(lContext, lIDs[I], @lHandle));
      try
        CheckHEIF(heif_decode_image(lHandle, @lImage, heif_colorspace_RGB, heif_chroma_interleaved_RGB, nil));
        lWidth := heif_image_get_width(lImage, heif_channel_interleaved);
        lHeight := heif_image_get_height(lImage, heif_channel_interleaved);
        heif_image_release(lImage);
      finally
        heif_image_handle_release(lHandle);
      end;
      if (lWidth <> FSizes[I].X) or (lHeight <> FSizes[I].Y) then
        raise FPImageException.CreateFmt('libheif %s writes an AV1 image of %dx%d pixels that decodes as %dx%d',
          [String(PAnsiChar(heif_get_version())), FSizes[I].X, FSizes[I].Y, lWidth, lHeight]);
      end;
  finally
    heif_context_free(lContext);
  end;
end;


// Writes the file of the images added so far to aStream; when sizes must be checked, through memory first.
procedure TFPWriterHEIF.WriteContext(aStream: TStream);

var
  lWriter: Theif_writer;
  lData: TMemoryStream;

begin
  lWriter.writer_api_version := 1;
  lWriter.write := @WriteToStream;
  if not FCheckSizes then
    begin
    CheckHEIF(heif_context_write(FContext, @lWriter, aStream));
    Exit;
    end;
  lData := TMemoryStream.Create;
  try
    CheckHEIF(heif_context_write(FContext, @lWriter, lData));
    CheckSizes(lData);
    aStream.WriteBuffer(lData.Memory^, lData.Size);
  finally
    lData.Free;
  end;
end;


procedure TFPWriterHEIF.InternalWrite(Stream: TStream; Img: TFPCustomImage);

var
  lMask: TFPUExceptionMask;

begin
  lMask := HEIFEnterLibrary;
  try
    OpenContext;
    try
      EncodeImage(Img);
      WriteContext(Stream);
    finally
      CloseContext;
    end;
  finally
    HEIFLeaveLibrary(lMask);
  end;
end;


procedure TFPWriterHEIF.InternalBeginFrames(Str: TStream; const aInfo: TFPFramesInfo);

var
  lMask: TFPUExceptionMask;

begin
  lMask := HEIFEnterLibrary;
  try
    OpenContext;
  finally
    HEIFLeaveLibrary(lMask);
  end;
end;


procedure TFPWriterHEIF.InternalWriteFrame(Str: TStream; Img: TFPCustomImage; const aInfo: TFPFrameInfo);

var
  lMask: TFPUExceptionMask;

begin
  if FContext = nil then
    raise FPImageException.Create('BeginFrames was not called');
  lMask := HEIFEnterLibrary;
  try
    EncodeImage(Img);
  finally
    HEIFLeaveLibrary(lMask);
  end;
end;


procedure TFPWriterHEIF.InternalEndFrames(Str: TStream);

var
  lMask: TFPUExceptionMask;

begin
  lMask := HEIFEnterLibrary;
  try
    try
      if FContext <> nil then
        WriteContext(Str);
    finally
      CloseContext;
    end;
  finally
    HEIFLeaveLibrary(lMask);
  end;
end;


{ TFPWriterAVIF }

constructor TFPWriterAVIF.Create;

begin
  inherited Create;
  Compression := hcAV1;
end;


initialization
  if HEIFCanEncode(hcHEVC) then
    ImageHandlers.RegisterImageWriter(HEIFHandlerName, HEIFExtensions, TFPWriterHEIF);
  if HEIFCanEncode(hcAV1) then
    ImageHandlers.RegisterImageWriter(AVIFHandlerName, AVIFExtensions, TFPWriterAVIF);
end.
