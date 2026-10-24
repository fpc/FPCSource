{*****************************************************************************}
{
    This file is part of the Free Pascal's "Free Components Library".
    Copyright (c) 2023 by Massimo Magnano

    JPEG reader/writer common code.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{*****************************************************************************}
{$IFNDEF FPC_DOTTEDUNITS}
unit JPEGcomn;
{$ENDIF}

{$mode ObjFPC}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, System.Jpeg.Jpeglib, System.Jpeg.Jdeferr, FpImage;
{$ELSE}
  Classes, SysUtils, JPEGLib, JDefErr, FPImage;
{$ENDIF}

type
    TFPJPEGCompressionQuality = 1..100;   // 100 = best quality, 25 = pretty awful

    PFPJPEGProgressManager = ^TFPJPEGProgressManager;
    TFPJPEGProgressManager = record
      pub : jpeg_progress_mgr;
      instance: TObject;
      last_pass: Integer;
      last_pct: Integer;
      last_time: Integer;
      last_scanline: Integer;
    end;

    TJPEGScale = (jsFullSize, jsHalf, jsQuarter, jsEighth);
    TJPEGReadPerformance = (jpBestQuality, jpBestSpeed);

    TExifOrientation = ( // all angles are clockwise
      eoUnknown, eoNormal, eoMirrorHor, eoRotate180, eoMirrorVert,
      eoMirrorHorRot270, eoRotate90, eoMirrorHorRot90, eoRotate270
    );

const
  // The starts of the APP1 markers of EXIF and XMP data and of the APP2 markers of an ICC profile.
  JPEGExifHeader: AnsiString = 'Exif'#0#0;
  JPEGXMPHeader: AnsiString = 'http://ns.adobe.com/xap/1.0/'#0;
  JPEGICCHeader: AnsiString = 'ICC_PROFILE'#0;


function density_unitToResolutionUnit(Adensity_unit: UINT8): TResolutionUnit;
function ResolutionUnitTodensity_unit(AResolutionUnit: TResolutionUnit): UINT8;
// Raises FPImageException with the code and text of the current libjpeg error.
procedure RaiseJPEGError(CurInfo: j_common_ptr);

implementation

procedure RaiseJPEGError(CurInfo: j_common_ptr);

var
  lMsg: AnsiString;

begin
  lMsg := '';
  with CurInfo^.err^ do
    begin
    if (jpeg_message_table <> nil) and (msg_code > 0) and (msg_code <= Ord(last_jpeg_message)) then
      lMsg := jpeg_message_table^[J_MESSAGE_CODE(msg_code)];
    if Pos('%s', lMsg) > 0 then
      lMsg := StringReplace(lMsg, '%s', msg_parm.s, [])
    else if Pos('%', lMsg) > 0 then
      try
        lMsg := Format(lMsg, [msg_parm.i[0], msg_parm.i[1], msg_parm.i[2], msg_parm.i[3],
                              msg_parm.i[4], msg_parm.i[5], msg_parm.i[6], msg_parm.i[7]]);
      except
        on EConvertError do ;
      end;
    raise FPImageException.CreateFmt('JPEG error %d: %s', [msg_code, lMsg]);
    end;
end;


function density_unitToResolutionUnit(Adensity_unit: UINT8): TResolutionUnit;
begin
  Case Adensity_unit of
  1: Result :=ruPixelsPerInch;
  2: Result :=ruPixelsPerCentimeter;
  else Result :=ruNone;
  end;
end;

function ResolutionUnitTodensity_unit(AResolutionUnit: TResolutionUnit): UINT8;
begin
  Case AResolutionUnit of
  ruPixelsPerInch: Result :=1;
  ruPixelsPerCentimeter: Result :=2;
  else Result :=0;
  end;
end;

end.

