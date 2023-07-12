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
unit JPEGcomn;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, JPEGLib, FPImage;

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


function density_unitToResolutionUnit(Adensity_unit: UINT8): TResolutionUnit;
function ResolutionUnitTodensity_unit(AResolutionUnit: TResolutionUnit): UINT8;

implementation

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

