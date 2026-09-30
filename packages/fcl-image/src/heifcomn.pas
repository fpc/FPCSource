{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by the Free Pascal development team

    HEIF reader/writer common definitions: libheif errors and codecs.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}{$h+}
{$IFNDEF FPC_DOTTEDUNITS}
unit heifcomn;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.SysUtils, System.Math, System.CTypes, FpImage, Api.HEIF;
{$ELSE FPC_DOTTEDUNITS}
uses
  SysUtils, Math, ctypes, FpImage, libheif;
{$ENDIF FPC_DOTTEDUNITS}

type
  // The compression of the images of a HEIF file: HEVC for HEIC, AV1 for AVIF.
  THEIFCompression = (hcHEVC, hcAV1);

  { An error reported by libheif. }
  EHEIFError = class(FPImageException)
  private
    FCode: Integer;
    FSubCode: Integer;
  public
    // Creates the exception from a libheif error.
    constructor CreateError(const aError: Theif_error);
    // The heif_error_* code of the error.
    property Code: Integer read FCode;
    // The heif_suberror_* code of the error.
    property SubCode: Integer read FSubCode;
  end;

const
  HEIFHandlerName = 'HEIF Image';
  AVIFHandlerName = 'AVIF Image';
  HEIFExtensions = 'heic;heif;hif';
  AVIFExtensions = 'avif';
  HEIFCompressionFormats: array[THEIFCompression] of Theif_compression_format = (heif_compression_HEVC, heif_compression_AV1);
  HEIFCompressionNames: array[THEIFCompression] of String = ('HEVC', 'AV1');
  // Metadata block type of EXIF data.
  HEIFMetaExif = 'Exif';
  // Metadata block type and content type of XMP data.
  HEIFMetaMime = 'mime';
  HEIFContentXMP = 'application/rdf+xml';

// Masks all floating point exceptions for the codecs of libheif and returns the mask in effect before.
function HEIFEnterLibrary: TFPUExceptionMask;
// Restores the floating point exception mask returned by HEIFEnterLibrary.
procedure HEIFLeaveLibrary(const aMask: TFPUExceptionMask);
// Raises EHEIFError when aError is not heif_error_Ok.
procedure CheckHEIF(const aError: Theif_error);
// Returns True when libheif has a decoder for aCompression.
function HEIFCanDecode(aCompression: THEIFCompression): Boolean;
// Returns True when libheif has an encoder for aCompression.
function HEIFCanEncode(aCompression: THEIFCompression): Boolean;

implementation

constructor EHEIFError.CreateError(const aError: Theif_error);

begin
  if Assigned(aError.message) then
    inherited Create(String(PAnsiChar(aError.message)))
  else
    inherited CreateFmt('libheif error %d.%d', [aError.code, aError.subcode]);
  FCode := aError.code;
  FSubCode := aError.subcode;
end;


function HEIFEnterLibrary: TFPUExceptionMask;

begin
  Result := SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
end;


procedure HEIFLeaveLibrary(const aMask: TFPUExceptionMask);

begin
  SetExceptionMask(aMask);
end;


procedure CheckHEIF(const aError: Theif_error);

begin
  if aError.code <> heif_error_Ok then
    raise EHEIFError.CreateError(aError);
end;


function HEIFCanDecode(aCompression: THEIFCompression): Boolean;

begin
  Result := heif_have_decoder_for_format(HEIFCompressionFormats[aCompression]) <> 0;
end;


function HEIFCanEncode(aCompression: THEIFCompression): Boolean;

begin
  Result := heif_have_encoder_for_format(HEIFCompressionFormats[aCompression]) <> 0;
end;

end.
