{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by the Free Pascal development team

    HEIF (HEIC, AVIF) image reader, using libheif.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}{$h+}
{$IFNDEF FPC_DOTTEDUNITS}
unit fpreadheif;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils, System.Math, System.CTypes, FpImage, FpImage.Exif, Api.HEIF, FpImage.Common.HEIF;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils, Math, ctypes, FpImage, fpimgexif, libheif, heifcomn;
{$ENDIF FPC_DOTTEDUNITS}

type
  { Reads the primary image of a HEIF file through libheif; the frames are all its top-level images. }
  TFPReaderHEIF = class(TFPCustomImageReader)
  private
    FData: TBytes;
    FContext: Pheif_context;
    FImageIDs: array of Theif_item_id;
    FNextFrame: Integer;
    FApplyTransformations: Boolean;
    procedure OpenContext(aStream: TStream);
    procedure CloseContext;
    procedure ReadHandle(aHandle: Pheif_image_handle; aImage: TFPCustomImage);
    procedure ReadMetadata(aHandle: Pheif_image_handle; aImage: TFPCustomImage);
  protected
    function InternalCheck(Stream: TStream): Boolean; override;
    procedure InternalRead(Stream: TStream; Img: TFPCustomImage); override;
    function InternalBeginFrames(Str: TStream): TFPFramesInfo; override;
    function InternalReadFrame(Str: TStream; Img: TFPCustomImage; var aInfo: TFPFrameInfo): Boolean; override;
    procedure InternalEndFrames(Str: TStream); override;
  public
    // Creates a reader that applies the transformations of the file.
    constructor Create; override;
    // Frees the reader and a file still open for frames.
    destructor Destroy; override;
    // Rotates, mirrors and crops the images as the file asks, and sets the orientation of the EXIF data to 1;
    // when off, images are decoded as they are stored.
    property ApplyTransformations: Boolean read FApplyTransformations write FApplyTransformations;
  end;

implementation

const
  // Number of bytes of the file given to the file type check.
  CheckSize = 64;

constructor TFPReaderHEIF.Create;

begin
  inherited Create;
  FApplyTransformations := True;
end;


destructor TFPReaderHEIF.Destroy;

begin
  CloseContext;
  inherited Destroy;
end;


function TFPReaderHEIF.InternalCheck(Stream: TStream): Boolean;

var
  lPos: Int64;
  lHeader: array[0..CheckSize - 1] of Byte;
  lCount: Integer;

begin
  lPos := Stream.Position;
  try
    lCount := Stream.Read(lHeader, SizeOf(lHeader));
    Result := (lCount >= 12) and (heif_check_filetype(@lHeader[0], lCount) = heif_filetype_yes_supported);
  finally
    Stream.Position := lPos;
  end;
end;


// Reads the rest of aStream and opens it as a HEIF file.
procedure TFPReaderHEIF.OpenContext(aStream: TStream);

var
  lSize: Int64;

begin
  CloseContext;
  lSize := aStream.Size - aStream.Position;
  if lSize <= 0 then
    raise FPImageException.Create('Empty HEIF data');
  SetLength(FData, lSize);
  aStream.ReadBuffer(FData[0], lSize);
  FContext := heif_context_alloc();
  if FContext = nil then
    raise FPImageException.Create('Cannot create a libheif context');
  CheckHEIF(heif_context_read_from_memory_without_copy(FContext, @FData[0], lSize, nil));
end;


procedure TFPReaderHEIF.CloseContext;

var
  lMask: TFPUExceptionMask;

begin
  if FContext <> nil then
    begin
    lMask := HEIFEnterLibrary;
    try
      heif_context_free(FContext);
    finally
      HEIFLeaveLibrary(lMask);
    end;
    end;
  FContext := nil;
  FData := nil;
  FImageIDs := nil;
end;


// Decodes the image of aHandle into aImage, in 8 or 16 bits per component.
procedure TFPReaderHEIF.ReadHandle(aHandle: Pheif_image_handle; aImage: TFPCustomImage);

var
  lAlpha, lDeep, lPremultiplied: Boolean;
  lChroma: Theif_chroma;
  lOptions: Pheif_decoding_options;
  lImage: Pheif_image;
  lWidth, lHeight, lStride, lMax, lX, lY, lChannels: Integer;
  lPlane, lRow: PByte;
  lColor: TFPColor;

  // Returns the sample aIndex of the pixel at lRow, scaled to 0..65535.
  function Sample(aIndex: Integer): Word;

  begin
    if lDeep then
      Result := (Cardinal(LEtoN(PWord(lRow)[aIndex])) * 65535 + Cardinal(lMax div 2)) div Cardinal(lMax)
    else
      Result := lRow[aIndex] * 257;
  end;

  // Divides a component by the alpha of the pixel.
  function Unpremultiply(aValue: Word): Word;

  var
    lValue: Cardinal;

  begin
    if (lColor.Alpha = 0) or (lColor.Alpha = AlphaOpaque) then
      Exit(aValue);
    lValue := (Cardinal(aValue) * 65535 + lColor.Alpha div 2) div lColor.Alpha;
    if lValue > 65535 then
      lValue := 65535;
    Result := lValue;
  end;

begin
  lAlpha := heif_image_handle_has_alpha_channel(aHandle) <> 0;
  lDeep := heif_image_handle_get_luma_bits_per_pixel(aHandle) > 8;
  if lDeep then
    begin
    if lAlpha then
      lChroma := heif_chroma_interleaved_RRGGBBAA_LE
    else
      lChroma := heif_chroma_interleaved_RRGGBB_LE;
    end
  else if lAlpha then
    lChroma := heif_chroma_interleaved_RGBA
  else
    lChroma := heif_chroma_interleaved_RGB;
  lOptions := heif_decoding_options_alloc();
  try
    lOptions^.ignore_transformations := Ord(not FApplyTransformations);
    CheckHEIF(heif_decode_image(aHandle, @lImage, heif_colorspace_RGB, lChroma, lOptions));
  finally
    heif_decoding_options_free(lOptions);
  end;
  try
    lWidth := heif_image_get_width(lImage, heif_channel_interleaved);
    lHeight := heif_image_get_height(lImage, heif_channel_interleaved);
    lPlane := PByte(heif_image_get_plane_readonly(lImage, heif_channel_interleaved, @lStride));
    if (lWidth <= 0) or (lHeight <= 0) or (lPlane = nil) then
      raise FPImageException.Create('libheif decoded no pixels');
    lMax := 255;
    if lDeep then
      lMax := (1 shl heif_image_get_bits_per_pixel_range(lImage, heif_channel_interleaved)) - 1;
    lPremultiplied := lAlpha and Assigned(heif_image_is_premultiplied_alpha)
                      and (heif_image_is_premultiplied_alpha(lImage) <> 0);
    lChannels := 3 + Ord(lAlpha);
    aImage.SetSize(lWidth, lHeight);
    lColor.Alpha := AlphaOpaque;
    for lY := 0 to lHeight - 1 do
      begin
      lRow := lPlane + PtrInt(lY) * lStride;
      for lX := 0 to lWidth - 1 do
        begin
        lColor.Red := Sample(0);
        lColor.Green := Sample(1);
        lColor.Blue := Sample(2);
        if lAlpha then
          begin
          lColor.Alpha := Sample(3);
          if lPremultiplied then
            begin
            lColor.Red := Unpremultiply(lColor.Red);
            lColor.Green := Unpremultiply(lColor.Green);
            lColor.Blue := Unpremultiply(lColor.Blue);
            end;
          end;
        aImage.Colors[lX, lY] := lColor;
        Inc(lRow, lChannels * (1 + Ord(lDeep)));
        end;
      end;
  finally
    heif_image_release(lImage);
  end;
end;


// Stores the EXIF, XMP and ICC data of aHandle in the metadata of aImage.
procedure TFPReaderHEIF.ReadMetadata(aHandle: Pheif_image_handle; aImage: TFPCustomImage);

var
  lIDs: array of Theif_item_id;
  lCount, I: Integer;
  lData: TBytes;
  lSize: csize_t;
  lOffset: Cardinal;
  lType, lContent: String;

  // Returns the data of metadata block aID.
  function BlockData(aID: Theif_item_id): TBytes;

  begin
    Result := nil;
    lSize := heif_image_handle_get_metadata_size(aHandle, aID);
    if lSize = 0 then
      Exit;
    SetLength(Result, lSize);
    CheckHEIF(heif_image_handle_get_metadata(aHandle, aID, @Result[0]));
  end;

begin
  lCount := heif_image_handle_get_number_of_metadata_blocks(aHandle, nil);
  if lCount > 0 then
    begin
    SetLength(lIDs, lCount);
    lCount := heif_image_handle_get_list_of_metadata_block_IDs(aHandle, nil, @lIDs[0], lCount);
    for I := 0 to lCount - 1 do
      begin
      lType := String(PAnsiChar(heif_image_handle_get_metadata_type(aHandle, lIDs[I])));
      lContent := String(PAnsiChar(heif_image_handle_get_metadata_content_type(aHandle, lIDs[I])));
      if (lType = HEIFMetaExif) and (Length(aImage.Metadata[MetaExif]) = 0) then
        begin
        lData := BlockData(lIDs[I]);
        if Length(lData) < 4 then
          Continue;
        lOffset := BEtoN(PCardinal(@lData[0])^);
        if lOffset > Cardinal(Length(lData) - 4) then
          Continue;
        lData := ExifWithoutHeader(Copy(lData, 4 + lOffset, MaxInt));
        if FApplyTransformations then
          ExifSetOrientation(lData, 1);
        aImage.Metadata[MetaExif] := lData;
        end
      else if (lType = HEIFMetaMime) and (lContent = HEIFContentXMP) and (Length(aImage.Metadata[MetaXMP]) = 0) then
        aImage.Metadata[MetaXMP] := BlockData(lIDs[I]);
      end;
    end;
  case heif_image_handle_get_color_profile_type(aHandle) of
    heif_color_profile_type_rICC, heif_color_profile_type_prof :
      begin
      lSize := heif_image_handle_get_raw_color_profile_size(aHandle);
      if lSize > 0 then
        begin
        SetLength(lData, lSize);
        CheckHEIF(heif_image_handle_get_raw_color_profile(aHandle, @lData[0]));
        aImage.Metadata[MetaICC] := lData;
        end;
      end;
  else
    ;
  end;
end;


procedure TFPReaderHEIF.InternalRead(Stream: TStream; Img: TFPCustomImage);

var
  lHandle: Pheif_image_handle;
  lMask: TFPUExceptionMask;

begin
  lMask := HEIFEnterLibrary;
  try
    OpenContext(Stream);
    try
      CheckHEIF(heif_context_get_primary_image_handle(FContext, @lHandle));
      try
        Img.ClearMetadata;
        ReadHandle(lHandle, Img);
        ReadMetadata(lHandle, Img);
      finally
        heif_image_handle_release(lHandle);
      end;
    finally
      CloseContext;
    end;
  finally
    HEIFLeaveLibrary(lMask);
  end;
end;


function TFPReaderHEIF.InternalBeginFrames(Str: TStream): TFPFramesInfo;

var
  lCount: Integer;
  lHandle: Pheif_image_handle;
  lMask: TFPUExceptionMask;

begin
  lMask := HEIFEnterLibrary;
  try
    OpenContext(Str);
    lCount := heif_context_get_number_of_top_level_images(FContext);
    SetLength(FImageIDs, lCount);
    if lCount > 0 then
      lCount := heif_context_get_list_of_top_level_image_IDs(FContext, @FImageIDs[0], lCount);
    SetLength(FImageIDs, lCount);
    FNextFrame := 0;
    Result := DefaultFramesInfo;
    Result.FrameCount := lCount;
    CheckHEIF(heif_context_get_primary_image_handle(FContext, @lHandle));
    try
      Result.Width := heif_image_handle_get_width(lHandle);
      Result.Height := heif_image_handle_get_height(lHandle);
    finally
      heif_image_handle_release(lHandle);
    end;
  finally
    HEIFLeaveLibrary(lMask);
  end;
end;


function TFPReaderHEIF.InternalReadFrame(Str: TStream; Img: TFPCustomImage; var aInfo: TFPFrameInfo): Boolean;

var
  lHandle: Pheif_image_handle;
  lMask: TFPUExceptionMask;

begin
  Result := (FContext <> nil) and (FNextFrame < Length(FImageIDs));
  if not Result then
    Exit;
  lMask := HEIFEnterLibrary;
  try
    CheckHEIF(heif_context_get_image_handle(FContext, FImageIDs[FNextFrame], @lHandle));
    try
      Img.ClearMetadata;
      ReadHandle(lHandle, Img);
      ReadMetadata(lHandle, Img);
      if heif_image_handle_is_primary_image(lHandle) <> 0 then
        aInfo.Name := 'primary';
    finally
      heif_image_handle_release(lHandle);
    end;
  finally
    HEIFLeaveLibrary(lMask);
  end;
  aInfo.Kind := fkPage;
  Inc(FNextFrame);
end;


procedure TFPReaderHEIF.InternalEndFrames(Str: TStream);

begin
  CloseContext;
end;


initialization
  ImageHandlers.RegisterImageReader(HEIFHandlerName, HEIFExtensions, TFPReaderHEIF);
  if HEIFCanDecode(hcAV1) then
    ImageHandlers.RegisterImageReader(AVIFHandlerName, AVIFExtensions, TFPReaderHEIF);
end.
