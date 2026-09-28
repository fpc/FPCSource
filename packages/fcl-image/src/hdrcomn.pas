{
    Shared definitions of the Radiance HDR (RGBE) reader and writer.
    This file is part of the Free Pascal run time library.
    See the file COPYING.FPC, included in this distribution, for details.
}
{$mode objfpc}{$h+}
{$IFNDEF FPC_DOTTEDUNITS}
unit hdrcomn;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Math, FpImage;
{$ELSE FPC_DOTTEDUNITS}
uses
  Math, FpImage;
{$ENDIF FPC_DOTTEDUNITS}

const
  HDRMagic = '#?RADIANCE';
  HDRMagicRGBE = '#?RGBE';
  HDRFormatRGBE = '32-bit_rle_rgbe';
  HDRFormatXYZE = '32-bit_rle_xyze';
  // Largest number of pixels of an image.
  HDRMaxPixels = 1 shl 28;

type
  // One pixel: an 8-bit mantissa for each component and a shared exponent.
  TRGBE = packed record
    R, G, B, E: Byte;
  end;

// Converts an RGBE pixel to a colour; components are mapped from 0..1 to 0..65535, clamping values above 1.
function RGBEToColor(const aPixel: TRGBE): TFPColor;
// Converts the red, green and blue of a colour, taken as values of 0..1, to an RGBE pixel.
function ColorToRGBE(const aColor: TFPColor): TRGBE;

implementation

// Maps a linear value to 0..65535, clamping values outside 0..1.
function ValueToWord(aValue: Double): Word;

begin
  if not (aValue > 0) then
    Result := 0
  else if aValue >= 1 then
    Result := 65535
  else
    Result := Round(aValue * 65535);
end;


function RGBEToColor(const aPixel: TRGBE): TFPColor;

var
  lFactor: Double;

begin
  Result.Alpha := AlphaOpaque;
  if aPixel.E = 0 then
    begin
    Result.Red := 0;
    Result.Green := 0;
    Result.Blue := 0;
    Exit;
    end;
  lFactor := Ldexp(1, aPixel.E - (128 + 8));
  Result.Red := ValueToWord(aPixel.R * lFactor);
  Result.Green := ValueToWord(aPixel.G * lFactor);
  Result.Blue := ValueToWord(aPixel.B * lFactor);
end;


function ColorToRGBE(const aColor: TFPColor): TRGBE;

var
  lMax, lMantissa, lScale: Double;
  lExponent: Integer;

begin
  lMax := MaxValue([aColor.Red, aColor.Green, aColor.Blue]) / 65535;
  if lMax < 1e-32 then
    begin
    Result.R := 0;
    Result.G := 0;
    Result.B := 0;
    Result.E := 0;
    Exit;
    end;
  Frexp(lMax, lMantissa, lExponent);
  lScale := lMantissa * 256 / lMax / 65535;
  Result.R := Min(255, Trunc(aColor.Red * lScale));
  Result.G := Min(255, Trunc(aColor.Green * lScale));
  Result.B := Min(255, Trunc(aColor.Blue * lScale));
  Result.E := lExponent + 128;
end;

end.
