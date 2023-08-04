{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2023 by the Free Pascal development team

    FP Color Space base type definitions.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{
 2023 Massimo Magnano
     Code ported from bgrabitmap, gimp and imagemagick with some modifications and additions.
}
{$mode objfpc}{$h+}
{$modeswitch ADVANCEDRECORDS}
{$modeswitch TYPEHELPERS}

unit FPColorSpace;

interface

uses FPImage;

type
  TIlluminant = string[10];

  PXYZReferenceWhite = ^TXYZReferenceWhite;
  TXYZReferenceWhite = packed record
    X, Y, Z: single;
    ObserverAngle: integer;
    Illuminant: TIlluminant;
    L,M,S: single;
  end;

  TSpectralLocusPoint = record
    W,X,Y,Z: Single;
  end;

  TIlluminantSpectrumPoint = record
    W,Y: Single;
  end;

  { TStdRGBA }

  PStdRGBA = ^TStdRGBA;
  TStdRGBA = packed record
    red,green,blue,alpha: single;
    class function New(const ARed,AGreen,ABlue,AAlpha:single): TStdRGBA;overload;static;
    class function New(const ARed,AGreen,ABlue:single): TStdRGBA;overload;static;
  end;

  { TAdobeRGBA }

  PAdobeRGBA = ^TAdobeRGBA;
  TAdobeRGBA = packed record
    red,green,blue,alpha: byte;
    class function New(const ARed,AGreen,ABlue,AAlpha:byte): TAdobeRGBA;overload;static;
    class function New(const ARed,AGreen,ABlue:byte): TAdobeRGBA;overload;static;
  end;

  { TStdHSLA }

  PStdHSLA = ^TStdHSLA;
  TStdHSLA = packed record
    hue,saturation,lightness,alpha: single;
    class function New(const AHue,ASaturation,ALightness,AAlpha:single): TStdHSLA;overload;static;
    class function New(const AHue,ASaturation,ALightness:single): TStdHSLA;overload;static;
  end;

  { TStdHSVA }

  PStdHSVA = ^TStdHSVA;
  TStdHSVA = packed record
    hue,saturation,value,alpha: single;
    class function New(const AHue,ASaturation,AValue,AAlpha:single): TStdHSVA;overload;static;
    class function New(const AHue,ASaturation,AValue:single): TStdHSVA;overload;static;
  end;

  { TStdCMYK }

  PStdCMYK = ^TStdCMYK;
  TStdCMYK = packed record
    C,M,Y,K: single;
    class function New(const ACyan,AMagenta,AYellow,ABlack:single): TStdCMYK;static;
  end;

  { TByteMask }

  PByteMask = ^TByteMask;
  TByteMask = packed record
    gray: byte;
    class function New(const AGray:byte): TByteMask;static;
  end;

  { TLinearRGBA }

  PLinearRGBA = ^TLinearRGBA;
  TLinearRGBA = packed record
    red,green,blue,alpha: single;
    class function New(const ARed,AGreen,ABlue,AAlpha:single): TLinearRGBA;overload;static;
    class function New(const ARed,AGreen,ABlue:single): TLinearRGBA;overload;static;
  end;

  { TXYZA }

  PXYZA = ^TXYZA;
  TXYZA = packed record
    X,Y,Z,alpha: single;
    class function New(const AX,AY,AZ,AAlpha:single): TXYZA;overload;static;
    class function New(const AX,AY,AZ:single): TXYZA;overload;static;
    procedure ChromaticAdapt(const AFrom, ATo: TXYZReferenceWhite);
  end;

  { TWordXYZA }

  PWordXYZA = ^TWordXYZA;
  TWordXYZA = packed record
    X,Y,Z,alpha: word;
    class function New(const AX,AY,AZ,AAlpha:word): TWordXYZA;overload;static;
    class function New(const AX,AY,AZ:word): TWordXYZA;overload;static;
    procedure ChromaticAdapt(const AFrom, ATo: TXYZReferenceWhite);
  end;

  { TLabA }

  PLabA = ^TLabA;
  TLabA = packed record
    L,a,b,alpha: single;
    class function New(const ALightness,Aa,Ab,AAlpha:single): TLabA;overload;static;
    class function New(const ALightness,Aa,Ab:single): TLabA;overload;static;
  end;

  { TLChA }

  PLChA = ^TLChA;
  TLChA = packed record
    L,C,h,alpha: single;
    class function New(const ALightness,AChroma,AHue,AAlpha:single): TLChA;overload;static;
    class function New(const ALightness,AChroma,AHue:single): TLChA;overload;static;
  end;

  { TYCbCr }

  TYCbCrSTD = (YCbCr_601, YCbCr_709, YCbCr_2020, YCbCr_JPG);
  TYCbCrSTD_Factor= packed record
    a, b, c, d, e :single;
  end;

  PYCbCr = ^TYCbCr;
  TYCbCr = packed record
    Y,Cb,Cr: single;
    class function New(const AY, ACb, ACr:single): TYCbCr; static;
  end;

  PExpandedPixel = ^TExpandedPixel;
  { TExpandedPixel }
  {* Stores a gamma expanded RGB color. Values range from 0 to 65535 }
  TExpandedPixel = packed record
    red, green, blue, alpha: word;

    class function New(const ARed,AGreen,ABlue,AAlpha:word): TExpandedPixel;overload;static;
    class function New(const ARed,AGreen,ABlue:word): TExpandedPixel;overload;static;

    function ToFPColor(AGammaCompression: boolean = true): TFPColor;
    procedure FromFPColor(const AValue: TFPColor; AGammaExpansion: boolean = true);

    {** Returns the intensity of an gamma-expanded pixel. The intensity is the
       maximum value reached by any component }
    function GetIntensity: word; inline;
    {** Sets the intensity of a gamma-expanded pixel }
    function SetIntensity(intensity: word): TExpandedPixel;
    {** Returns the lightness of an gamma-expanded pixel. The lightness is the
       perceived brightness, 0 being black and 65535 being white }
    function GetLightness: word; inline; overload;
    {** Sets the lightness of a gamma-expanded pixel }
    function SetLightness(lightness: word): TExpandedPixel; overload;
    {** Sets the lightness of a gamma expanded pixel, provided you already know the current
       value of lightness ''curLightness''. It is a bit faster than the previous function }
    function SetLightness(lightness: word; curLightness: word): TExpandedPixel; overload;
    {** Returns the importance of the color. It is similar to saturation
        in HSL colorspace, except it is gamma corrected. A value of zero indicates
        a black/gray/white, and a value of 65535 indicates a bright color }
    function ColorImportance: word;
    {** Merge two gamma expanded pixels (so taking into account gamma correction) }
    function Merge(ec2: TExpandedPixel): TExpandedPixel; overload;
    function Merge(weight1: integer; ec2: TExpandedPixel; weight2: integer): TExpandedPixel; overload;
    {** Computes the difference (with gamma correction) between two pixels,
        taking into account all dimensions, including transparency. The
        result ranges from 0 to 65535 }
    function ExpandedDiff(ec2: TExpandedPixel): word;
    {** Returns the hue of a gamma expanded pixel }
    function GetHue: word;
  end;
  TExpandedPixelBuffer = packed array of TExpandedPixel;

const
  ExpandedPixelTransparent : TExpandedPixel =(red:0; green:0; blue:0; alpha:0);

type
  {* Pixel color defined in HSL colorspace. Values range from 0 to 65535 }

  { THSLAPixel }

  THSLAPixel = packed record
    {** Hue of the pixel. Extremum values 0 and 65535 are red }
    hue: word;
    {** Saturation of the color. 0 is gray and 65535 is the brightest color (including white) }
    saturation: word;
    {** Lightness of the color. 0 is black, 32768 is normal, and 65535 is white }
    lightness: word;
    {** Opacity of the pixel. 0 is transparent and 65535 is opaque }
    alpha: word;

    {** Creates a pixel with given HSLA values, where A stands for alpha }
    class function New(Ahue, Asaturation, Alightness, Aalpha: word): THSLAPixel; overload; static;
    {** Creates an opaque pixel with given HSL values }
    class function New(Ahue, Asaturation, Alightness: word): THSLAPixel; overload; static;

    function ToFPColor(AGammaCompression: boolean=true): TFPColor;
    procedure FromFPColor(AValue: TFPColor; AGammaExpansion: boolean=true);

    {** Converts a pixel from gamma expanded RGB to HSL color space }
    procedure FromExpanded(const ec: TExpandedPixel);
    {** Converts a pixel from HSL colorspace to gamma expanded RGB }
    function ToExpanded: TExpandedPixel;
  end;

  {* Pixel color defined in corrected HSL colorspace. G stands for corrected hue
     and B stands for actual brightness. Values range from 0 to 65535 }

  { TGSBAPixel }

  TGSBAPixel = packed record
    {** Hue of the pixel. Extremum values 0 and 65535 are red }
    hue: word;
    {** Saturation of the color. 0 is gray and 65535 is the brightest color (excluding white) }
    saturation: word;
    {** Actual perceived brightness. 0 is black, 32768 is normal, and 65535 is white }
    lightness: word;
    {** Opacity of the pixel. 0 is transparent and 65535 is opaque }
    alpha: word;

    class function New(Ahue, Asaturation, Alightness, Aalpha: word): THSLAPixel; overload; static;
    class function New(Ahue, Asaturation, Alightness: word): THSLAPixel; overload; static;

    function ToFPColor(AGammaCompression: boolean=true): TFPColor;
    procedure FromFPColor(AValue: TFPColor; AGammaExpansion: boolean=true);
    {** Converts a pixel from gamma expanded RGB to correct HSL color space }
    procedure FromExpanded(const ec: TExpandedPixel);
    {** Converts a pixel from correct HSL to gamma expanded RGB }
    function ToExpanded: TExpandedPixel;
    {** Converts a pixel from correct HSL to usual HSL }
    function ToHSLA: THSLAPixel;
    procedure FromHSLA(const c: THSLAPixel);
  end;

//*************************** Helpers ******************************************

  { TFPColorHelper }

  TFPColorHelper = record helper for TFPColor
    class function New(const ARed,AGreen,ABlue,AAlpha:word): TFPColor;overload;static;
    class function New(const ARed,AGreen,ABlue:word): TFPColor;overload;static;

    function ToExpanded(AGammaExpansion: boolean = true): TExpandedPixel;
    function ToHSLAPixel(AGammaExpansion: boolean = true): THSLAPixel;
    function ToGSBAPixel(AGammaExpansion: boolean = true): TGSBAPixel;
    function ToStdRGBA: TStdRGBA;
    function ToStdHSLA: TStdHSLA;
    function ToStdHSVA: TStdHSVA;
    function ToStdCMYK: TStdCMYK;
  end;

  { TExpandedPixelHelper }

  TExpandedPixelHelper = record helper for TExpandedPixel
    function ToHSLAPixel: THSLAPixel;
    function ToGSBAPixel: TGSBAPixel;
    function ToByteMask: TByteMask;
    function ToLinearRGBA: TLinearRGBA;
    function ToStdRGBA: TStdRGBA;
    function ToWordXYZA: TWordXYZA; overload;
    function ToWordXYZA(const AReferenceWhite: TXYZReferenceWhite): TWordXYZA; overload;
  end;

  { THSLAPixelHelper }

  THSLAPixelHelper = record helper for THSLAPixel
    function ToGSBAPixel: TGSBAPixel;
  end;

  { TStdRGBAHelper }

  TStdRGBAHelper = record helper for TStdRGBA
    function ToFPColor: TFPColor;
    function ToExpandedPixel: TExpandedPixel;
    function ToLinearRGBA: TLinearRGBA;
    function ToStdHSLA: TStdHSLA;
    function ToStdHSVA: TStdHSVA;
    function ToStdCMYK: TStdCMYK;
    {** SamplePrecision = 2^(YCbCrSamplePrecision-1) }
    function ToYCbCr(const AStd:TYCbCrSTD=YCbCr_JPG; ASamplePrecision:Single=0.5): TYCbCr; overload;
    function ToYCbCr(LumaRed:Single=0.299; LumaGreen:Single=0.587; LumaBlue:Single=0.114): TYCbCr; overload;
  end;

  { TAdobeRGBAHelper }

  TAdobeRGBAHelper = record helper for TAdobeRGBA
    function ToXYZA: TXYZA; overload;
    function ToXYZA(const AReferenceWhite: TXYZReferenceWhite): TXYZA; overload;
  end;

  { TStdHSLAHelper }

  TStdHSLAHelper = record helper for TStdHSLA
    function ToFPColor: TFPColor;
    function ToStdRGBA: TStdRGBA;
    function ToStdHSVA: TStdHSVA;
    function ToExpandedPixel: TExpandedPixel;
  end;

  { TStdHSVAHelper }

  TStdHSVAHelper = record helper for TStdHSVA
    function ToFPColor: TFPColor;
    function ToStdRGBA: TStdRGBA;
    function ToStdHSLA: TStdHSLA;
  end;

  { TStdCMYKHelper }

  TStdCMYKHelper = record helper for TStdCMYK
    function ToFPColor(const AAlpha: Word=$ffff): TFPColor;
    function ToStdRGBA(AAlpha: Single = 1): TStdRGBA;
    function ToExpandedPixel: TExpandedPixel;overload;
    function ToExpandedPixel(AAlpha: word): TExpandedPixel;overload;
  end;

  { TByteMaskHelper }

  TByteMaskHelper = record helper for TByteMask
    function ToExpandedPixel(AAlpha: byte = 255): TExpandedPixel;
  end;

  { TLinearRGBAHelper }

  TLinearRGBAHelper = record helper for TLinearRGBA
    function ToExpandedPixel: TExpandedPixel;
    function ToXYZA: TXYZA; overload;
    function ToXYZA(const AReferenceWhite: TXYZReferenceWhite): TXYZA; overload;
    function ToStdRGBA: TStdRGBA;
  end;

  { TXYZAHelper }

  TXYZAHelper = record helper for TXYZA
    function ToLinearRGBA: TLinearRGBA; overload;
    function ToLinearRGBA(const AReferenceWhite: TXYZReferenceWhite): TLinearRGBA; overload;
    function ToWordXYZA: TWordXYZA;
    function ToLabA: TLabA; overload;
    function ToLabA(const AReferenceWhite: TXYZReferenceWhite): TLabA; overload;
    function ToAdobeRGBA: TAdobeRGBA; overload;
    function ToAdobeRGBA(const AReferenceWhite: TXYZReferenceWhite): TAdobeRGBA; overload;
    procedure FromSpectrumRangeReflect(reflectance,wavelen1,wavelen2,alpha: single);
    procedure ToLMS(out L,M,S: single);
    procedure FromLMS(const L,M,S: Single);
  end;

  { TWordXYZAHelper }

  TWordXYZAHelper = record helper for TWordXYZA
    function ToXYZA: TXYZA;
    function ToExpandedPixel: TExpandedPixel; overload;
    function ToExpandedPixel(const AReferenceWhite: TXYZReferenceWhite): TExpandedPixel; overload;
  end;

  { TLabAHelper }

  TLabAHelper = record helper for TLabA
    function ToXYZA: TXYZA; overload;
    function ToXYZA(const AReferenceWhite: TXYZReferenceWhite): TXYZA; overload;
    function ToLChA: TLChA;
    function ToExpandedPixel: TExpandedPixel; overload;
    function ToExpandedPixel(const AReferenceWhite: TXYZReferenceWhite): TExpandedPixel; overload;
  end;

  { TLChAHelper }

  TLChAHelper = record helper for TLChA
    function ToLabA: TLabA;
  end;


  { TYCbCrHelper }

  TYCbCrHelper = record helper for TYCbCr
    {** SamplePrecision = 2^(YCbCrSamplePrecision-1) }
    function ToStdRGBA(const AStd:TYCbCrSTD=YCbCr_601; ASamplePrecision:Single=0.5): TStdRGBA; overload;
    function ToStdRGBA(LumaRed:Single=0.299; LumaGreen:Single=0.587; LumaBlue:Single=0.114; ASamplePrecision:Single=0.5): TStdRGBA; overload;
  end;

  {* How to handle overflow when converting from XYZ }
  TFPColorspaceOverflow =
    {** Colors outside of target colorspace are converted to transparent }
    (xroClipToTarget,
    {** Each color channel is saturated independently (hue may be lost) }
    xroSaturateEachChannel,
    {** Hue is preserved by reducing intensity or saturation }
    xroPreserveHue);

{$i fpspectraldata.inc}

var
   { Gamma conversion arrays. Should be used as readonly }
   FPGammaExpansionTab:     packed array[0..255] of word;
   FPGammaExpansionTabHalf: packed array[0..254] of word;
   FPGammaCompressionTab : packed array[0..65535] of byte;          //rounded value

   FPReferenceWhiteArray: array of TXYZReferenceWhite;
   FPReferenceWhite,
   FPReferenceWhite2D50, FPReferenceWhite2D65, FPReferenceWhite2E: PXYZReferenceWhite;

   XYZToRGBOverflowMin : TFPColorspaceOverflow = xroSaturateEachChannel;
   XYZToRGBOverflowMax : TFPColorspaceOverflow = xroSaturateEachChannel;

procedure FPGammaSet(AGamma: single = 1.7);
procedure FPGammaNone;
function FPGammaGet: single;

{** Apply gamma compression with word values }
function FPGammaCompression(AExpanded: word): word;
{** Apply gamma expansion with word values }
function FPGammaExpansion(ACompressed: word): word; overload;
function FPGammaExpansion(AValue: single): word; overload;

{** Computes the hue difference }
function FPHueDiff(h1, h2: word): word;
{** Converts a G hue (GSBA) to a H hue (HSLA) }
function FPHueGSBAToHSLA(ghue: word): word;
{** Converts a H hue (HSLA) to a G hue (GSBA) }
function FPHueHSLAToGSBA(hue: word): word;


procedure FPReferenceWhiteSet(AObserverAngle: integer; AIlluminant: TIlluminant); overload;
procedure FPReferenceWhiteSet(AReferenceWhite: TXYZReferenceWhite); overload;
function FPReferenceWhiteGet(AObserverAngle: integer; AIlluminant: TIlluminant): PXYZReferenceWhite;

function FPReferenceWhiteAdd(const AReferenceWhite: TXYZReferenceWhite):PXYZReferenceWhite; overload;
function FPReferenceWhiteAdd(AObserverAngle: integer; AIlluminant: TIlluminant; AX, AY, AZ: single):PXYZReferenceWhite; overload;

procedure FPChromaticAdaptXYZ(var X,Y,Z: single; const AFrom, ATo: TXYZReferenceWhite); inline;
procedure FPChromaticAdaptWordXYZ(var X,Y,Z: word; const AFrom, ATo: TXYZReferenceWhite); inline;

implementation


uses math;

type
  Int32or64 = {$IFDEF CPU64}Int64{$ELSE}LongInt{$ENDIF};
  UInt32or64 = {$IFDEF CPU64}UInt64{$ELSE}LongWord{$ENDIF};


{ The gamma correction is approximated here by a power function }
var
  FPGammaExpFactor   : single; //exponent

const
  redWeightShl10   = 306; // = 0.299
  greenWeightShl10 = 601; // = 0.587
  blueWeightShl10  = 117; // = 0.114

function Clamp(const V, Min, Max: single): single;
begin
    if V < Min then
      Result := Min
    else if V > Max then
      Result := Max
    else Result := V;
end;

function ClampInt(V, Min, Max: integer): integer;
begin
    if V < Min then
      Result := Min
    else if V > Max then
      Result := Max
    else Result := V;
end;

function PositiveModSingle(x, cycle: single): single;
begin
    if (x < 0) or (x >= cycle) then
      Result := x - cycle * floor(x / cycle)
    else
      result := x;
end;


{ TStdRGBA }

class function TStdRGBA.New(const ARed,AGreen,ABlue,AAlpha:single): TStdRGBA;overload;
begin
  Result.red := ARed;
  Result.green := AGreen;
  Result.blue := ABlue;
  Result.alpha := AAlpha;
end;

class function TStdRGBA.New(const ARed,AGreen,ABlue:single): TStdRGBA;overload;
begin
  Result.red := ARed;
  Result.green := AGreen;
  Result.blue := ABlue;
  Result.alpha := 1;
end;

{ TAdobeRGBA }

class function TAdobeRGBA.New(const ARed,AGreen,ABlue,AAlpha:byte): TAdobeRGBA;overload;
begin
  Result.red := ARed;
  Result.green := AGreen;
  Result.blue := ABlue;
  Result.alpha := AAlpha;
end;

class function TAdobeRGBA.New(const ARed,AGreen,ABlue:byte): TAdobeRGBA;overload;
begin
  Result.red := ARed;
  Result.green := AGreen;
  Result.blue := ABlue;
  Result.alpha := $ff;
end;

{ TStdHSLA }

class function TStdHSLA.New(const AHue,ASaturation,ALightness,AAlpha:single): TStdHSLA;overload;
begin
  Result.hue := AHue;
  Result.saturation := ASaturation;
  Result.lightness := ALightness;
  Result.alpha := AAlpha;
end;

class function TStdHSLA.New(const AHue,ASaturation,ALightness:single): TStdHSLA;overload;
begin
  Result.hue := AHue;
  Result.saturation := ASaturation;
  Result.lightness := ALightness;
  Result.alpha := 1;
end;

{ TStdHSVA }

class function TStdHSVA.New(const AHue,ASaturation,AValue,AAlpha:single): TStdHSVA;overload;
begin
  Result.hue := AHue;
  Result.saturation := ASaturation;
  Result.value := AValue;
  Result.alpha := AAlpha;
end;

class function TStdHSVA.New(const AHue,ASaturation,AValue:single): TStdHSVA;overload;
begin
  Result.hue := AHue;
  Result.saturation := ASaturation;
  Result.value := AValue;
  Result.alpha := 1;
end;

{ TStdCMYK }

class function TStdCMYK.New(const ACyan,AMagenta,AYellow,ABlack:single): TStdCMYK;
begin
  Result.C := ACyan;
  Result.M := AMagenta;
  Result.Y := AYellow;
  Result.K := ABlack;
end;

{ TByteMask }

class function TByteMask.New(const AGray:byte): TByteMask;
begin
  Result.gray := AGray;
end;

{ TLinearRGBA }

class function TLinearRGBA.New(const ARed,AGreen,ABlue,AAlpha:single): TLinearRGBA;overload;
begin
  Result.red := ARed;
  Result.green := AGreen;
  Result.blue := ABlue;
  Result.alpha := AAlpha;
end;

class function TLinearRGBA.New(const ARed,AGreen,ABlue:single): TLinearRGBA;overload;
begin
  Result.red := ARed;
  Result.green := AGreen;
  Result.blue := ABlue;
  Result.alpha := 1;
end;

{ TXYZA }

class function TXYZA.New(const AX,AY,AZ,AAlpha:single): TXYZA;overload;
begin
  Result.X := AX;
  Result.Y := AY;
  Result.Z := AZ;
  Result.alpha := AAlpha;
end;

class function TXYZA.New(const AX,AY,AZ:single): TXYZA;overload;
begin
  Result.X := AX;
  Result.Y := AY;
  Result.Z := AZ;
  Result.alpha := 1;
end;

procedure TXYZA.ChromaticAdapt(const AFrom, ATo: TXYZReferenceWhite);
begin
  FPChromaticAdaptXYZ(self.X,self.Y,self.Z, AFrom, ATo);
end;

{ TWordXYZA }

class function TWordXYZA.New(const AX,AY,AZ,AAlpha:word): TWordXYZA;overload;
begin
  Result.X := AX;
  Result.Y := AY;
  Result.Z := AZ;
  Result.alpha := AAlpha;
end;

class function TWordXYZA.New(const AX,AY,AZ:word): TWordXYZA;overload;
begin
  Result.X := AX;
  Result.Y := AY;
  Result.Z := AZ;
  Result.alpha := $ffff;
end;

procedure TWordXYZA.ChromaticAdapt(const AFrom, ATo: TXYZReferenceWhite);
begin
   FPChromaticAdaptWordXYZ(self.X,self.Y,self.Z, AFrom,ATo);
end;

{ TLabA }

class function TLabA.New(const ALightness,Aa,Ab,AAlpha:single): TLabA;overload;
begin
  Result.L := ALightness;
  Result.a := Aa;
  Result.b := Ab;
  Result.alpha := AAlpha;
end;

class function TLabA.New(const ALightness,Aa,Ab:single): TLabA;overload;
begin
  Result.L := ALightness;
  Result.a := Aa;
  Result.b := Ab;
  Result.alpha := 1;
end;

{ TLChA }

class function TLChA.New(const ALightness,AChroma,AHue,AAlpha:single): TLChA;overload;
begin
  Result.L := ALightness;
  Result.C := AChroma;
  Result.h := AHue;
  Result.alpha := AAlpha;
end;

class function TLChA.New(const ALightness,AChroma,AHue:single): TLChA;overload;
begin
  Result.L := ALightness;
  Result.C := AChroma;
  Result.h := AHue;
  Result.alpha := 1;
end;

{ TYCbCr }

class function TYCbCr.New(const AY, ACb, ACr:single): TYCbCr;
begin
  Result.Y := AY;
  Result.Cb := ACb;
  Result.Cr := ACr;
end;

function FPGammaExpansion(ACompressed: word): word;
const
  fracShift = 8;
  fracHalf = 1 shl (fracShift-1);
  fracQuarter = 1 shl (fracShift-2);
var
  intPart, fracPart, half: word;
  byteVal: byte;
begin
  if ACompressed = 0 then
    result := 0
  else if ACompressed = $ffff then
    result := $ffff
  else
  begin
    //div 257
    byteVal := ACompressed shr fracShift;
    intPart := (byteVal shl fracShift) + byteVal;
    if ACompressed < intPart then
    begin
      dec(byteVal);
      dec(intPart, 257);
    end;

    fracPart := ACompressed - intPart;
    if fracPart >= fracHalf then dec(fracPart);  //[0..256] -> [0..255]

    if fracPart >= fracHalf then
    begin
      result := FPGammaExpansionTab[byteVal+1];
      half := FPGammaExpansionTabHalf[byteVal];
      dec(result, ((result-half)*((1 shl fracShift)-fracPart)+fracQuarter) shr (fracShift-1));
    end
    else
    begin
      result := FPGammaExpansionTab[byteVal];
      if fracPart > 0 then
      begin
        half := FPGammaExpansionTabHalf[byteVal];
        inc(result, ((half-result)*fracPart+fracQuarter) shr (fracShift-1));
      end;
    end;
  end;
end;

function FPGammaExpansion(AValue: single): word;
const
  fracShift = 10;
  intRange = 255 shl fracShift;
  fracAnd = (1 shl fracShift)-1;
  fracHalf = 1 shl (fracShift-1);
  fracQuarter = 1 shl (fracShift-2);
var
  valInt, byteVal, fracPart: integer;
  half: Word;
begin
  if AValue <= 0 then exit(0)
  else if AValue >= 1 then exit(65535);

  valInt := round(AValue*intRange);
  byteVal := valInt shr fracShift;
  fracPart := valInt and fracAnd;
  if fracPart >= fracHalf then
  begin
    result := FPGammaExpansionTab[byteVal+1];
    half := FPGammaExpansionTabHalf[byteVal];
    dec(result, ((result-half)*((1 shl fracShift)-fracPart)+fracQuarter) shr (fracShift-1));
  end
  else
  begin
    result := FPGammaExpansionTab[byteVal];
    if fracPart > 0 then
    begin
      half := FPGammaExpansionTabHalf[byteVal];
      inc(result, ((half-result)*fracPart+fracQuarter) shr (fracShift-1));
    end;
  end;
end;

function FPGammaCompression(AExpanded: word): word;
var
  compByte: Byte;
  reExp, reExpDelta: Word;
begin
  if AExpanded=0 then exit(0) else
  if AExpanded=65535 then exit(65535) else
  begin
    compByte := FPGammaCompressionTab[AExpanded];
    reExp := FPGammaExpansionTab[compByte];
    result := compByte + (compByte shl 8);
    if reExp < AExpanded then
    begin
      reExpDelta := FPGammaExpansionTabHalf[compByte]-reExp;
      if reExpDelta<>0 then
        inc(result, ((AExpanded-reExp)*128+(reExpDelta shr 1)) div reExpDelta);
    end else
    begin
      reExpDelta := reExp-FPGammaExpansionTabHalf[compByte-1];
      if reExpDelta<>0 then
        dec(result, ((reExp-AExpanded)*128+(reExpDelta shr 1)) div reExpDelta);
    end;
  end;
end;

class function TExpandedPixel.New(const ARed, AGreen, ABlue, AAlpha: word): TExpandedPixel;
begin
  Result.red := ARed;
  Result.green := AGreen;
  Result.blue := ABlue;
  Result.alpha := AAlpha;
end;

class function TExpandedPixel.New(const ARed, AGreen, ABlue: word): TExpandedPixel;
begin
  Result.red := ARed;
  Result.green := AGreen;
  Result.blue := ABlue;
  Result.alpha := $ffff;
end;

function TExpandedPixel.ToFPColor(AGammaCompression: boolean): TFPColor;
begin
  if AGammaCompression then
  begin
    result.red := FPGammaCompression(self.red);
    result.green := FPGammaCompression(self.green);
    result.blue := FPGammaCompression(self.blue);
  end else
  begin
    result.red := self.red;
    result.green := self.green;
    result.blue := self.blue;
  end;
  result.alpha := self.alpha;
end;

procedure TExpandedPixel.FromFPColor(const AValue: TFPColor; AGammaExpansion: boolean);
begin
  if AGammaExpansion then
  begin
    self.red := FPGammaExpansion(AValue.red);
    self.green := FPGammaExpansion(AValue.green);
    self.blue := FPGammaExpansion(AValue.blue);
  end else
  begin
    self.red := AValue.red;
    self.green := AValue.green;
    self.blue := AValue.blue;
  end;
  self.alpha := AValue.alpha;
end;

{ The intensity is defined here as the maximum value of any color component }
function TExpandedPixel.GetIntensity: word; inline;
begin
  Result := self.red;
  if self.green > Result then
    Result := self.green;
  if self.blue > Result then
    Result := self.blue;
end;

function TExpandedPixel.SetIntensity(intensity: word): TExpandedPixel;
var
  curIntensity: word;
begin
  curIntensity := GetIntensity;
  if curIntensity = 0 then //suppose it's gray if there is no color information
  begin
    Result.red := intensity;
    Result.green := intensity;
    Result.blue := intensity;
    result.alpha := self.alpha;
  end
  else
  begin
    //linear interpolation to reached wanted intensity
    Result.red   := (self.red * intensity + (curIntensity shr 1)) div curIntensity;
    Result.green := (self.green * intensity + (curIntensity shr 1)) div curIntensity;
    Result.blue  := (self.blue * intensity + (curIntensity shr 1)) div curIntensity;
    Result.alpha := self.alpha;
  end;
end;

{ The lightness here is defined as the subjective sensation of luminosity, where
  blue is the darkest component and green the lightest }
function TExpandedPixel.GetLightness: word; inline;
begin
  Result := (self.red * redWeightShl10 + self.green * greenWeightShl10 +
    self.blue * blueWeightShl10 + 512) shr 10;
end;

function TExpandedPixel.SetLightness(lightness: word): TExpandedPixel;
var
  curLightness: word;
begin
  curLightness := GetLightness;
  if lightness = curLightness then
  begin //no change
    Result := self;
    exit;
  end;
  Result := SetLightness(lightness, curLightness);
end;

function TExpandedPixel.SetLightness(lightness: word; curLightness: word): TExpandedPixel;
var
  AddedWhiteness, maxBeforeWhite: word;
  clip: boolean;
begin
  if lightness = curLightness then
  begin //no change
    Result := self;
    exit;
  end;
  if lightness = 65535 then //set to white
  begin
    Result.red   := 65535;
    Result.green := 65535;
    Result.blue  := 65535;
    Result.alpha := self.alpha;
    exit;
  end;
  if lightness = 0 then  //set to black
  begin
    Result.red   := 0;
    Result.green := 0;
    Result.blue  := 0;
    Result.alpha := self.alpha;
    exit;
  end;
  if curLightness = 0 then  //set from black
  begin
    Result.red   := lightness;
    Result.green := lightness;
    Result.blue  := lightness;
    Result.alpha := self.alpha;
    exit;
  end;
  if lightness < curLightness then //darker is easy
  begin
    result.alpha:= self.alpha;
    result.red := (self.red * lightness + (curLightness shr 1)) div curLightness;
    result.green := (self.green * lightness + (curLightness shr 1)) div curLightness;
    result.blue := (self.blue * lightness + (curLightness shr 1)) div curLightness;
    exit;
  end;
  //lighter and grayer
  Result := self;
  AddedWhiteness := lightness - curLightness;
  maxBeforeWhite := 65535 - AddedWhiteness;
  clip   := False;
  if Result.red <= maxBeforeWhite then
    Inc(Result.red, AddedWhiteness)
  else
  begin
    Result.red := 65535;
    clip := True;
  end;
  if Result.green <= maxBeforeWhite then
    Inc(Result.green, AddedWhiteness)
  else
  begin
    Result.green := 65535;
    clip := True;
  end;
  if Result.blue <= maxBeforeWhite then
    Inc(Result.blue, AddedWhiteness)
  else
  begin
    Result.blue := 65535;
    clip := True;
  end;

  if clip then //light and whiter
  begin
    curLightness   := Result.GetLightness;
    addedWhiteness := lightness - curLightness;
    maxBeforeWhite := 65535 - curlightness;
    Result.red     := Result.red + addedWhiteness * (65535 - Result.red) div
      maxBeforeWhite;
    Result.green   := Result.green + addedWhiteness * (65535 - Result.green) div
      maxBeforeWhite;
    Result.blue    := Result.blue + addedWhiteness * (65535 - Result.blue) div
      maxBeforeWhite;
  end;
end;

function TExpandedPixel.ColorImportance: word;
var min,max: word;
begin
  min := self.red;
  max := self.red;
  if self.green > max then
    max := self.green
  else
  if self.green < min then
    min := self.green;
  if self.blue > max then
    max := self.blue
  else
  if self.blue < min then
    min  := self.blue;
  result := max - min;
end;

{ Merge two colors of same importance }
function TExpandedPixel.Merge(ec2: TExpandedPixel): TExpandedPixel;
var c12: LongWord;
begin
  if (self.alpha = 0) then
    Result := ec2
  else
  if (ec2.alpha = 0) then
    Result := self
  else
  begin
    c12 := self.alpha + ec2.alpha;
    Result.red   := (int64(self.red) * self.alpha + int64(ec2.red) * ec2.alpha + c12 shr 1) div c12;
    Result.green := (int64(self.green) * self.alpha + int64(ec2.green) * ec2.alpha + c12 shr 1) div c12;
    Result.blue  := (int64(self.blue) * self.alpha + int64(ec2.blue) * ec2.alpha + c12 shr 1) div c12;
    Result.alpha := (c12 + 1) shr 1;
  end;
end;

function TExpandedPixel.Merge(weight1: integer; ec2: TExpandedPixel; weight2: integer): TExpandedPixel;
var
    f1,f2,f12: int64;
begin
  if (weight1 = 0) then
  begin
    if (weight2 = 0) then
      result := ExpandedPixelTransparent
    else
      Result := ec2
  end
  else
  if (weight2 = 0) then
    Result := self
  else
  if (weight1+weight2 = 0) then
    Result := ExpandedPixelTransparent
  else
  begin
    f1 := int64(self.alpha)*weight1;
    f2 := int64(ec2.alpha)*weight2;
    f12 := f1+f2;
    if f12 = 0 then
      result := ExpandedPixelTransparent
    else
    begin
      Result.red   := (self.red * f1 + ec2.red * f2 + f12 shr 1) div f12;
      Result.green := (self.green * f1 + ec2.green * f2 + f12 shr 1) div f12;
      Result.blue  := (self.blue * f1 + ec2.blue * f2 + f12 shr 1) div f12;
      {$hints off}
      Result.alpha := (f12 + ((weight1+weight2) shr 1)) div (weight1+weight2);
      {$hints on}
    end;
  end;
end;

function LessStartSlope65535(value: word): word;
var factor: word;
begin
  factor := 4096 - (not value)*3 shr 7;
  result := value*factor shr 12;
end;

function TExpandedPixel.ExpandedDiff(ec2: TExpandedPixel): word;
var
  CompRedAlpha1, CompGreenAlpha1, CompBlueAlpha1, CompRedAlpha2,
  CompGreenAlpha2, CompBlueAlpha2: integer;
  DiffAlpha: word;
  ColorDiff: word;
  TempHueDiff: word;
begin
  if (self.alpha = 0) and (ec2.alpha = 0) then exit(0) else
  if (self.alpha = ec2.alpha) and (self.red = ec2.red) and
     (self.green = ec2.green) and (self.blue = ec2.blue) then exit(0);
  CompRedAlpha1 := self.red * self.alpha shr 16; //gives 0..65535
  CompGreenAlpha1 := self.green * self.alpha shr 16;
  CompBlueAlpha1 := self.blue * self.alpha shr 16;
  CompRedAlpha2 := ec2.red * ec2.alpha shr 16;
  CompGreenAlpha2 := ec2.green * ec2.alpha shr 16;
  CompBlueAlpha2 := ec2.blue * ec2.alpha shr 16;
  Result    := (Abs(CompRedAlpha2 - CompRedAlpha1)*redWeightShl10 +
    Abs(CompBlueAlpha2 - CompBlueAlpha1)*blueWeightShl10 +
    Abs(CompGreenAlpha2 - CompGreenAlpha1)*greenWeightShl10) shr 10;
  ColorDiff := min(self.ColorImportance,ec2.ColorImportance);
  if ColorDiff > 0 then
  begin
    TempHueDiff := FPHueDiff(FPHueHSLAToGSBA(self.GetHue), FPHueHSLAToGSBA(ec2.GetHue));
    if TempHueDiff < 32768 then
      TempHueDiff := LessStartSlope65535(TempHueDiff shl 1) shr 4
    else
      TempHueDiff := TempHueDiff shr 3;
    Result := ((Result shr 4)* (not ColorDiff) + TempHueDiff*ColorDiff) shr 12;
  end;
  DiffAlpha := Abs(integer(ec2.Alpha) - integer(self.Alpha));
  if DiffAlpha > Result then
    Result := DiffAlpha;
end;

function TExpandedPixel.GetHue: word;
const
  deg60  = 8192;
  deg120 = deg60 * 2;
  deg240 = deg60 * 4;
  deg360 = deg60 * 6;
var
  min, max, minMax: integer;
  r,g,b: integer;
begin
  r := self.red;
  g := self.green;
  b := self.blue;
  min := r;
  max := r;
  if g > max then
    max := g
  else
  if g < min then
    min := g;
  if b > max then
    max := b
  else
  if b < min then
    min  := b;
  minMax := max - min;

  if minMax = 0 then
    Result := 0
  else
  if max = r then
    Result := (((g - b) * deg60) div
      minMax + deg360) mod deg360
  else
  if max = g then
    Result := ((b - r) * deg60) div minMax + deg120
  else
    {max = b} Result :=
      ((r - g) * deg60) div minMax + deg240;

  Result   := (Result shl 16) div deg360; //normalize
end;

{ THSLAPixel }

class function THSLAPixel.New(Ahue, Asaturation, Alightness, Aalpha: word): THSLAPixel;
begin
  Result.hue   := Ahue;
  Result.saturation := Asaturation;
  Result.lightness  := Alightness;
  Result.alpha := Aalpha;
end;

class function THSLAPixel.New(Ahue, Asaturation, Alightness: word): THSLAPixel;
begin
  Result.hue   := Ahue;
  Result.saturation := Asaturation;
  Result.lightness  := Alightness;
  Result.alpha := $ffff;
end;

function THSLAPixel.ToFPColor(AGammaCompression: boolean): TFPColor;
begin
  result :=self.ToExpanded.ToFPColor(AGammaCompression);
end;

procedure THSLAPixel.FromFPColor(AValue: TFPColor; AGammaExpansion: boolean);
begin
  self.FromExpanded(AValue.ToExpanded(AGammaExpansion));
end;

procedure ExpandedToHSLAInline(r,g,b: Int32Or64; var dest: THSLAPixel); inline;
const
  deg60  = 10922;
  deg120 = 21845;
  deg240 = 43690;
var
  min, max, minMax: Int32or64;
  UMinMax,UTwiceLightness: UInt32or64;
begin
  if g > r then
  begin
    max := g;
    min := r;
  end else
  begin
    max := r;
    min := g;
  end;
  if b > max then
    max := b else
  if b < min then
    min := b;
  minMax := max - min;

  if minMax = 0 then
    dest.hue := 0
  else
  if max = r then
    {$PUSH}{$RANGECHECKS OFF}
    dest.hue := ((g - b) * deg60) div minMax
    {$POP}
  else
  if max = g then
    dest.hue := ((b - r) * deg60) div minMax + deg120
  else
    {max = b} dest.hue := ((r - g) * deg60) div minMax + deg240;
  UTwiceLightness := max + min;
  if min = max then
    dest.saturation := 0 else
  begin
    UMinMax:= minMax;
    if UTwiceLightness < 65536 then
      dest.saturation := (UMinMax shl 16) div (UTwiceLightness + 1)
    else
      dest.saturation := (UMinMax shl 16) div (131072 - UTwiceLightness);
  end;
  dest.lightness := UTwiceLightness shr 1;
end;

procedure THSLAPixel.FromExpanded(const ec: TExpandedPixel);
begin
  self.alpha := ec.alpha;
  ExpandedToHSLAInline(ec.red,ec.green,ec.blue,self);
end;

function THSLAPixel.ToExpanded: TExpandedPixel;
const
  deg30  = 4096;
  deg60  = 8192;
  deg120 = deg60 * 2;
  deg180 = deg60 * 3;
  deg240 = deg60 * 4;
  deg360 = deg60 * 6;

  function ComputeColor(p, q: Int32or64; h: Int32or64): Int32or64; inline;
  begin
    if h < deg180 then
    begin
      if h < deg60 then
        Result := p + ((q - p) * h + deg30) div deg60
      else
        Result := q
    end else
    begin
      if h < deg240 then
        Result := p + ((q - p) * (deg240 - h) + deg30) div deg60
      else
        Result := p;
    end;
  end;

var
  q, p, L, S, H: Int32or64;
begin
  L := self.lightness;
  S := self.saturation;
  if S = 0 then  //gray
  begin
    result.red   := L;
    result.green := L;
    result.blue  := L;
    result.alpha := self.alpha;
    exit;
  end;
  {$hints off}
  if L < 32768 then
    q := (L shr 1) * ((65535 + S) shr 1) shr 14
  else
    q := L + S - ((L shr 1) *
      (S shr 1) shr 14);
  {$hints on}
  if q > 65535 then q := 65535;
  p   := (L shl 1) - q;
  if p > 65535 then p := 65535;
  H := self.hue * deg360 shr 16;
  result.green := ComputeColor(p, q, H);
  inc(H, deg120);
  if H > deg360 then Dec(H, deg360);
  result.red   := ComputeColor(p, q, H);
  inc(H, deg120);
  if H > deg360 then Dec(H, deg360);
  result.blue  := ComputeColor(p, q, H);
  result.alpha := self.alpha;
end;

{ TGSBAPixel }

class function TGSBAPixel.New(Ahue, Asaturation, Alightness, Aalpha: word): THSLAPixel;
begin
  Result.hue   := Ahue;
  Result.saturation := Asaturation;
  Result.lightness  := Alightness;
  Result.alpha := Aalpha;
end;

class function TGSBAPixel.New(Ahue, Asaturation, Alightness: word): THSLAPixel;
begin
  Result.hue   := Ahue;
  Result.saturation := Asaturation;
  Result.lightness  := Alightness;
  Result.alpha := $ffff;
end;

function TGSBAPixel.ToFPColor(AGammaCompression: boolean): TFPColor;
begin
  result :=self.ToExpanded.ToFPColor(AGammaCompression);
end;

procedure TGSBAPixel.FromFPColor(AValue: TFPColor; AGammaExpansion: boolean);
begin
  self.FromExpanded(AValue.ToExpanded(AGammaExpansion));
end;

procedure TGSBAPixel.FromExpanded(const ec: TExpandedPixel);
var
   Alightness: UInt32Or64;
   red,green,blue: Int32or64;
   hsla: THSLAPixel;
begin
  red   := ec.red;
  green := ec.green;
  blue  := ec.blue;
  hsla.alpha := ec.alpha;

  Alightness := (red * redWeightShl10 + green * greenWeightShl10 +
    blue * blueWeightShl10 + 512) shr 10;

  ExpandedToHSLAInline(red,green,blue,hsla);
  self := TGSBAPixel(hsla);

  if self.lightness > 32768 then
    self.saturation := self.saturation* UInt32or64(not self.lightness) div 32767;
  self.lightness := Alightness;
  self.hue := FPHueHSLAToGSBA(self.hue);
end;

function TGSBAPixel.ToExpanded: TExpandedPixel;
var
   c: THSLAPixel;
begin
  c :=THSLAPixel(self);
  c.hue := FPHueGSBAToHSLA(self.hue);
  c.lightness := 32768;
  result :=c.ToExpanded;
  result.SetLightness(self.lightness);
end;

function TGSBAPixel.ToHSLA: THSLAPixel;
begin
  result.FromExpanded(self.ToExpanded);
end;

procedure TGSBAPixel.FromHSLA(const c: THSLAPixel);
begin
  self.FromExpanded(c.ToExpanded);
end;

{ TFPColorHelper }

class function TFPColorHelper.New(const ARed,AGreen,ABlue,AAlpha:word): TFPColor;overload;
begin
  Result.red := ARed;
  Result.green := AGreen;
  Result.blue := ABlue;
  Result.alpha := AAlpha;
end;

class function TFPColorHelper.New(const ARed,AGreen,ABlue:word): TFPColor;overload;
begin
  Result.red := ARed;
  Result.green := AGreen;
  Result.blue := ABlue;
  Result.alpha := $ffff;
end;

function TFPColorHelper.ToExpanded(AGammaExpansion: boolean): TExpandedPixel;
begin
  result.FromFPColor(self, AGammaExpansion);
end;

function TFPColorHelper.ToHSLAPixel(AGammaExpansion: boolean): THSLAPixel;
begin
  result.FromFPColor(self, AGammaExpansion);
end;

function TFPColorHelper.ToGSBAPixel(AGammaExpansion: boolean): TGSBAPixel;
begin
  result.FromFPColor(self, AGammaExpansion);
end;

function TFPColorHelper.ToStdRGBA: TStdRGBA;
const oneOver65535 = 1/65535;
begin
    result.red := red * oneOver65535;
    result.green := green * oneOver65535;
    result.blue := blue * oneOver65535;
    result.alpha := alpha * oneOver65535;
end;

function TFPColorHelper.ToStdHSLA: TStdHSLA;
begin
  result :=self.ToStdRGBA.ToStdHSLA;
end;

function TFPColorHelper.ToStdHSVA: TStdHSVA;
begin
  result :=self.ToStdRGBA.ToStdHSVA;
end;

function TFPColorHelper.ToStdCMYK: TStdCMYK;
begin
  result :=self.ToStdRGBA.ToStdCMYK;
end;

{ TExpandedPixelHelper }

function TExpandedPixelHelper.ToHSLAPixel: THSLAPixel;
begin
  result.alpha := self.alpha;
  ExpandedToHSLAInline(self.red,self.green,self.blue,result);
end;

function TExpandedPixelHelper.ToGSBAPixel: TGSBAPixel;
begin
  result.FromExpanded(self);
end;

function TExpandedPixelHelper.ToByteMask: TByteMask;
begin
  result.gray:= (self.red * redWeightShl10 + self.green * greenWeightShl10 +
    self.blue * blueWeightShl10 + (1 shl 9)) shr 18;
end;

function TExpandedPixelHelper.ToLinearRGBA: TLinearRGBA;
begin
  result.red := self.red / 65535;
  result.green := self.green / 65535;
  result.blue := self.blue / 65535;
  result.alpha := self.alpha / 65535;
end;

function TExpandedPixelHelper.ToStdRGBA: TStdRGBA;
begin
  result.red := FPGammaCompression(self.red);
  result.green := FPGammaCompression(self.green);
  result.blue := FPGammaCompression(self.blue);
  result.alpha := self.alpha/65535;
end;

function TExpandedPixelHelper.ToWordXYZA: TWordXYZA;
begin
  if FPReferenceWhite=nil then raise FPImageException.Create('Reference white (FPReferenceWhite) can not be null');
  result := self.ToWordXYZA(FPReferenceWhite^);
end;

function TExpandedPixelHelper.ToWordXYZA(const AReferenceWhite: TXYZReferenceWhite): TWordXYZA;
begin
  if AReferenceWhite.Illuminant = 'D50' then
  begin
    with self do
    begin
      result.X := ClampInt(round((red * 0.4360746 + green * 0.3850649 + blue * 0.1430804)*(50000/65535)),0,65535);
      result.Y := ClampInt(round((red * 0.2225045 + green * 0.7168786 + blue * 0.0606169)*(50000/65535)),0,65535);
      result.Z := ClampInt(round((red * 0.0139322 + green * 0.0971045 + blue * 0.7141733)*(50000/65535)),0,65535);
    end;
    result.ChromaticAdapt(FPReferenceWhite2D50^, AReferenceWhite);
  end else
  begin
    with self do
    begin
      result.X := ClampInt(round((red * 0.4124564 + green * 0.3575761 + blue * 0.1804375)*(50000/65535)),0,65535);
      result.Y := ClampInt(round((red * 0.2126729 + green * 0.7151522 + blue * 0.0721750)*(50000/65535)),0,65535);
      result.Z := ClampInt(round((red * 0.0193339 + green * 0.1191920 + blue * 0.9503041)*(50000/65535)),0,65535);
    end;
    result.ChromaticAdapt(FPReferenceWhite2D65^, AReferenceWhite);
  end;
  Result.alpha := self.alpha;
end;

{ THSLAPixelBasicHelper }

function THSLAPixelHelper.ToGSBAPixel: TGSBAPixel;
begin
   result.FromExpanded(self.ToExpanded);
end;

{ TStdRGBAHelper }

function TStdRGBAHelper.ToFPColor: TFPColor;
begin
  result.red := ClampInt(round(red * 65535), 0, 65535);
  result.green := ClampInt(round(green * 65535), 0, 65535);
  result.blue := ClampInt(round(blue * 65535), 0, 65535);
  result.alpha := ClampInt(round(alpha * 65535), 0, 65535);
end;

function TStdRGBAHelper.ToExpandedPixel: TExpandedPixel;
begin
  result.red := FPGammaExpansion(self.red);
  result.green := FPGammaExpansion(self.green);
  result.blue := FPGammaExpansion(self.blue);
  result.alpha:= round(self.alpha*65535);
end;

function TStdRGBAHelper.ToLinearRGBA: TLinearRGBA;
begin
  result := self.ToExpandedPixel.ToLinearRGBA;
  result.alpha := self.alpha;
end;

function TStdRGBAHelper.ToStdHSLA: TStdHSLA;
var
  d, cmax, cmin: double;
begin
  with self do
  begin
    cmax := Max(red, Max(green, blue));
    cmin := Min(red, Min(green, blue));
    result.lightness := (cmax + cmin) / 2;

    if cmax = cmin then
    begin
      result.hue := 0;
      result.saturation := 0;
    end
    else
    begin
      d := cmax - cmin;
      if result.lightness < 0.5 then
        result.saturation := d / (cmax + cmin)
      else
        result.saturation := d / (2 - cmax - cmin);

      if red = cmax then
        result.hue := (green - blue) / d
      else
      if green = cmax then
        result.hue := 2 + (blue - red) / d
      else
        result.hue := 4 + (red - green) / d;
      if result.hue < 0 then result.hue :=result.hue+6;
      result.hue := result.hue * 60;
    end;
    result.alpha := alpha;
  end;
end;

function TStdRGBAHelper.ToStdHSVA: TStdHSVA;
var
  Delta, mini: single;
begin
  with self do
  begin
    result.value := max(max(red, green), blue);
    mini := min(min(red, green), blue);
    Delta := result.value - mini;

    if result.value = 0.0 then
      result.saturation := 0
    else
      result.saturation := Delta / result.value;

    if result.saturation = 0.0 then
      result.hue := 0
    else
    begin
      if red = result.value then
        result.hue := 60.0 * (green - blue) / Delta
      else
      if green = result.value then
        result.hue := 120.0 + 60.0 * (blue - red) / Delta
      else
      {if blue = result.value then}
        result.hue := 240.0 + 60.0 * (red - green) / Delta;

      if result.hue < 0.0 then
        result.hue :=result.hue+360.0;
    end;
    result.alpha := alpha;
  end;
end;

function TStdRGBAHelper.ToStdCMYK: TStdCMYK;
begin
  with self do
  begin
    result.K := 1 - max(max(red, green), blue);
    if result.K >= 1 then
    begin
      result.C := 0;
      result.M := 0;
      result.Y := 0;
    end
    else
    begin
      result.C := 1 - red / (1 - result.K);
      result.M := 1 - green / (1 - result.K);
      result.Y := 1 - blue / (1 - result.K);
    end;
  end;
end;

function TStdRGBAHelper.ToYCbCr(const AStd: TYCbCrSTD; ASamplePrecision:Single=0.5): TYCbCr;
begin
  with self, YCbCrSTD_Factors[AStd]  do
  begin
    result.Y := a * red + b * green + c * blue;
    result.Cb := ((blue - result.Y) / d)+ASamplePrecision;
    result.Cr := ((red - result.Y) / e)+ASamplePrecision;
  end;
end;

function TStdRGBAHelper.ToYCbCr(LumaRed: Single; LumaGreen: Single; LumaBlue: Single): TYCbCr;
begin
  with self  do
  begin
    result.Y :=  ( LumaRed * red + LumaGreen * green + LumaBlue * blue );
    result.Cb := ( blue - result.Y ) / ( 2 - 2 * LumaBlue );
    result.Cr := ( red - result.Y ) / ( 2 - 2 * LumaRed );
  end;
end;

{ TAdobeRGBAHelper }

function TAdobeRGBAHelper.ToXYZA: TXYZA;
begin
  if FPReferenceWhite=nil then raise FPImageException.Create('Reference white (FPReferenceWhite) can not be null');
  result := self.ToXYZA(FPReferenceWhite^);
end;

function TAdobeRGBAHelper.ToXYZA(const AReferenceWhite: TXYZReferenceWhite): TXYZA;
var R,G,B: single;
begin
  R := FPGammaExpansionTab[self.red]/65535;
  G := FPGammaExpansionTab[self.green]/65535;
  B := FPGammaExpansionTab[self.blue]/65535;
  if AReferenceWhite.Illuminant = 'D50' then
  begin
    result.X := R*0.6097559 + G*0.2052401 + B*0.1492240;
    result.Y := R*0.3111242 + G*0.6256560 + B*0.0632197;
    result.Z := R*0.0194811 + G*0.0608902 + B*0.7448387;
    result.ChromaticAdapt(FPReferenceWhite2D50^, AReferenceWhite);
  end else
  begin
    result.X := R*0.5767309 + G*0.1855540 + B*0.1881852;
    result.Y := R*0.2973769 + G*0.6273491 + B*0.0752741;
    result.Z := R*0.0270343 + G*0.0706872 + B*0.9911085;
    result.ChromaticAdapt(FPReferenceWhite2D65^, AReferenceWhite);
  end;
  result.alpha := self.alpha/255;
end;

{ TStdHSLAHelper }

function TStdHSLAHelper.ToFPColor: TFPColor;
begin
  result :=self.ToStdRGBA.ToFPColor;
end;

function TStdHSLAHelper.ToStdRGBA: TStdRGBA;
var
  C, X, M, rp, gp, bp, sp, lp, h360: single;
begin
  lp := self.lightness;
  sp := self.saturation;
  C := (1 - abs(2 * Lp - 1)) * Sp;
  h360 := PositiveModSingle(self.hue, 360);
  X := C * (1 - abs(PositiveModSingle(h360 / 60, 2) - 1));
  m := Lp - C / 2;
  rp := 0;
  gp := 0;
  bp := 0;
  case floor(h360) of
    -1..59:
    begin
      rp := C;
      gp := X;
      bp := 0;
    end;
    60..119:
    begin
      rp := X;
      gp := C;
      bp := 0;
    end;
    120..179:
    begin
      rp := 0;
      gp := C;
      bp := X;
    end;
    180..239:
    begin
      rp := 0;
      gp := X;
      bp := C;
    end;
    240..299:
    begin
      rp := X;
      gp := 0;
      bp := C;
    end;
    300..359:
    begin
      rp := C;
      gp := 0;
      bp := X;
    end;
  end;
  result.red := rp + m;
  result.green := gp + m;
  result.blue := bp + m;
  result.alpha := self.alpha;
end;

function TStdHSLAHelper.ToStdHSVA: TStdHSVA;
var
  s, l, v: single;
begin
  Result.hue := self.hue;
  s := self.saturation;
  l := self.lightness;
  v := (2 * l + s * (1 - abs(2 * l - 1))) / 2;
  if v <> 0 then
    Result.saturation := 2 * (v - l) / v
  else
    Result.saturation := 0;
  Result.value := v;
end;

function TStdHSLAHelper.ToExpandedPixel: TExpandedPixel;
begin
  result :=self.ToStdRGBA.ToExpandedPixel;
end;

{ TStdHSVAHelper }

function TStdHSVAHelper.ToFPColor: TFPColor;
begin
  result :=self.ToStdRGBA.ToFPColor;
end;

function TStdHSVAHelper.ToStdRGBA: TStdRGBA;
var
  C, X, M, rp, gp, bp, sp, vp: single;
  h360: single;
begin
  vp := self.value;
  sp := self.saturation;
  C := Vp * sp;
  h360 := PositiveModSingle(self.hue, 360);
  X := C * (1 - abs(PositiveModSingle(h360 / 60, 2) - 1));
  m := vp - c;
  rp := 0;
  gp := 0;
  bp := 0;
  case floor(h360) of
    -1..59:
    begin
      rp := C;
      gp := X;
      bp := 0;
    end;
    60..119:
    begin
      rp := X;
      gp := C;
      bp := 0;
    end;
    120..179:
    begin
      rp := 0;
      gp := C;
      bp := X;
    end;
    180..239:
    begin
      rp := 0;
      gp := X;
      bp := C;
    end;
    240..299:
    begin
      rp := X;
      gp := 0;
      bp := C;
    end;
    300..359:
    begin
      rp := C;
      gp := 0;
      bp := X;
    end;
  end;
  result.red := rp + m;
  result.green := gp + m;
  result.blue := bp + m;
  result.alpha := self.alpha;
end;

function TStdHSVAHelper.ToStdHSLA: TStdHSLA;
var
  s, v, l: single;
begin
  Result.hue := self.hue;
  s := self.saturation;
  v := self.value;
  l := 0.5 * v * (2 - s);
  if l <> 0 then
    Result.saturation := v * s / (1 - abs(2 * l - 1))
  else
    Result.saturation := 0;
  Result.lightness := l;
end;

{ TStdCMYKHelper }

function TStdCMYKHelper.ToStdRGBA(AAlpha: Single): TStdRGBA;
begin
  with self do
  begin
    result.red := (1 - C) * (1 - K);
    result.green := (1 - M) * (1 - K);
    result.blue := (1 - Y) * (1 - K);
    result.alpha := AAlpha;
  end;
end;

function TStdCMYKHelper.ToExpandedPixel: TExpandedPixel;
begin
  result :=self.ToStdRGBA.ToExpandedPixel;
end;

function TStdCMYKHelper.ToExpandedPixel(AAlpha: word): TExpandedPixel;
begin
  result :=self.ToStdRGBA(AAlpha).ToExpandedPixel;
end;

function TStdCMYKHelper.ToFPColor(const AAlpha: Word): TFPColor;
begin
  result :=self.ToStdRGBA.ToFPColor;
  result.alpha := AAlpha
end;

{ TLabAHelper }

function TLabAHelper.ToExpandedPixel: TExpandedPixel;
begin
  result :=self.ToXYZA.ToLinearRGBA.ToExpandedPixel;
end;

function TLabAHelper.ToExpandedPixel(const AReferenceWhite: TXYZReferenceWhite): TExpandedPixel;
begin
  result :=self.ToXYZA(AReferenceWhite).ToLinearRGBA(AReferenceWhite).ToExpandedPixel;
end;

function TLabAHelper.ToXYZA: TXYZA;
begin
  if FPReferenceWhite=nil then raise FPImageException.Create('Reference white (FPReferenceWhite) can not be null');
  result :=self.ToXYZA(FPReferenceWhite^);
end;

function TLabAHelper.ToXYZA(const AReferenceWhite: TXYZReferenceWhite): TXYZA;
var
  xp, yp, zp: double;
begin
  yp := (self.L + 16) / 116;
  xp := self.a / 500 + yp;
  zp := yp - self.b / 200;
  if yp > 0.2069 then
    yp := IntPower(yp, 3)
  else
    yp := (yp - 0.138) / 7.787;
  if xp > 0.2069 then
    xp := IntPower(xp, 3)
  else
    xp := (xp - 0.138) / 7.787;
  if zp > 0.2069 then
    zp := IntPower(zp, 3)
  else
    zp := (zp - 0.138) / 7.787;
  Result.X := AReferenceWhite.X * xp;
  Result.Y := AReferenceWhite.Y * yp;
  Result.Z := AReferenceWhite.Z * zp;
  Result.alpha := self.Alpha;
end;

function TLabAHelper.ToLChA: TLChA;
var
  a, b, HRad: single;
begin
  a := self.a;
  b := self.b;
  HRad := ArcTan2(b, a);
  if HRad >= 0 then
    result.H := (HRad / PI) * 180
  else
    result.H := 360 - (ABS(HRad) / PI) * 180;
  result.L := self.L;
  result.C := SQRT(a*a + b*b);
  result.alpha := self.Alpha;
end;

{ TLChAHelper }

function TLChAHelper.ToLabA: TLabA;

Var
  rh,rhs,rhc : single;

begin
  result.L := self.L;
  rh:=DegToRad(self.h);
  sincos(rh,rhs,rhc);
  result.a := rhc * self.C;
  result.b := rhs * self.C;
  result.Alpha:= self.alpha;
end;

{ TYCbCrHelper }

function TYCbCrHelper.ToStdRGBA(const AStd: TYCbCrSTD; ASamplePrecision:Single): TStdRGBA;
begin
  with self, YCbCrSTD_Factors[AStd]  do
  begin
    //"analog" Y is in the range 0 to 1 ; Cb and Cr in the range -0.5 to +0.5
    //"digital" are normalized to 0..255; In 601 Standard values are between 16 and 235 for Y, 16 to 240 for Cb and Cr.

    result.red := Y + e * (Cr-ASamplePrecision);
    result.green := Y - (a * e / b) * (Cr-ASamplePrecision) - (c * d / b) * (Cb-ASamplePrecision);
    result.blue := Y + d * (Cb-ASamplePrecision);
  end;
end;

function TYCbCrHelper.ToStdRGBA(LumaRed: Single; LumaGreen: Single; LumaBlue: Single; ASamplePrecision:Single): TStdRGBA;
begin
  with self  do
  begin
    result.red := (Cr-ASamplePrecision) * ( 2 - 2 * LumaRed ) + Y;
    result.blue := (Cb-ASamplePrecision) * ( 2 - 2 * LumaBlue ) + Y;
    result.green :=  ( Y - LumaBlue * result.blue - LumaRed * result.red ) / LumaGreen;
  end;
end;

{ TByteMaskHelper }

function TByteMaskHelper.ToExpandedPixel(AAlpha: byte): TExpandedPixel;
begin
  result.red := self.gray+(self.gray shl 8);
  result.green := result.red;
  result.blue := result.red;
  result.alpha := AAlpha;
end;

{ TLinearRGBAHelper }

function TLinearRGBAHelper.ToExpandedPixel: TExpandedPixel;
begin
  with self do
  begin
    result.red := ClampInt(round(red * 65535), 0, 65535);
    result.green := ClampInt(round(green * 65535), 0, 65535);
    result.blue := ClampInt(round(blue * 65535), 0, 65535);
    result.alpha := ClampInt(round(alpha * 65535), 0, 65535);
  end;
end;

function TLinearRGBAHelper.ToXYZA: TXYZA;
begin
  if FPReferenceWhite=nil then raise FPImageException.Create('Reference white (FPReferenceWhite) can not be null');
  result := self.ToXYZA(FPReferenceWhite^);
end;

function TLinearRGBAHelper.ToXYZA(const AReferenceWhite: TXYZReferenceWhite): TXYZA;
begin
  if AReferenceWhite.Illuminant = 'D50' then
  begin
    with self do
    begin
      result.X := red * 0.4360746 + green * 0.3850649 + blue * 0.1430804;
      result.Y := red * 0.2225045 + green * 0.7168786 + blue * 0.0606169;
      result.Z := red * 0.0139322 + green * 0.0971045 + blue * 0.7141733;
    end;
    result.ChromaticAdapt(FPReferenceWhite2D50^, AReferenceWhite);
  end else
  begin
    with self do
    begin
      result.X := red * 0.4124564 + green * 0.3575761 + blue * 0.1804375;
      result.Y := red * 0.2126729 + green * 0.7151522 + blue * 0.0721750;
      result.Z := red * 0.0193339 + green * 0.1191920 + blue * 0.9503041;
    end;
    result.ChromaticAdapt(FPReferenceWhite2D65^, AReferenceWhite);
  end;
  Result.alpha := self.alpha;
end;

function TLinearRGBAHelper.ToStdRGBA: TStdRGBA;
begin
  result := self.ToExpandedPixel.ToStdRGBA;
  result.alpha := self.alpha;
end;

{ TXYZAHelper }

function TXYZAHelper.ToLinearRGBA: TLinearRGBA;
begin
  if FPReferenceWhite=nil then raise FPImageException.Create('Reference white (FPReferenceWhite) can not be null');
  result := self.ToLinearRGBA(FPReferenceWhite^);
end;

function TXYZAHelper.ToLinearRGBA(const AReferenceWhite: TXYZReferenceWhite): TLinearRGBA;
var
  minVal, lightVal, maxVal: single;
  ad: TXYZA;
begin
  ad := self;
  if AReferenceWhite.Illuminant = 'D50' then
  begin
    ad.ChromaticAdapt(AReferenceWhite, FPReferenceWhite2D50^);
    with ad do
    begin
      result.red := X * 3.1338561 + Y * (-1.6168667) + Z * (-0.4906146);
      result.green := X * (-0.9787684) + Y * 1.9161415 + Z * 0.0334540;
      result.blue := X * 0.0719453 + Y * (-0.2289914) + Z * 1.4052427;
    end;
  end else
  begin
    ad.ChromaticAdapt(AReferenceWhite, FPReferenceWhite2D65^);
    with ad do
    begin
      result.red := X * 3.2404542 + Y * (-1.5371385) + Z * (-0.4985314);
      result.green := X * (-0.9692660) + Y * 1.8760108 + Z * 0.0415560;
      result.blue := X * 0.0556434 + Y * (-0.2040259) + Z * 1.0572252;
    end;
  end;
  if ( (XYZToRGBOverflowMin = xroClipToTarget) and ((result.red < 0) or
       (result.green < 0) or (result.blue < 0)) ) or
     ( (XYZToRGBOverflowMax = xroClipToTarget) and ((result.red > 1) or
       (result.green > 1) or (result.blue > 1)) ) then
  begin
    result.red := 0;
    result.green := 0;
    result.blue := 0;
    result.alpha := 0;
    exit;
  end;
  case XYZToRGBOverflowMin of
    xroPreserveHue: begin
        minVal := min(min(result.red,result.green),result.blue);
        if minVal<0 then
        begin
          lightVal := result.red*0.299+result.green*0.587+result.blue*0.114;
          if lightVal <= 0 then
          begin
            result.red := 0;
            result.green := 0;
            result.blue := 0;
          end else
          begin
            result.red := (result.red-minVal)*lightVal/(lightVal-minVal);
            result.green := (result.green-minVal)*lightVal/(lightVal-minVal);
            result.blue := (result.blue-minVal)*lightVal/(lightVal-minVal);
          end;
        end;
      end;
  end;
  case XYZToRGBOverflowMax of
    xroPreserveHue:
      begin
        maxVal := max(max(result.red,result.green),result.blue);
        if maxVal > 1 then
        begin
          result.red := result.red/maxVal;
          result.green := result.green/maxVal;
          result.blue := result.blue/maxVal;
        end;
      end;
    xroSaturateEachChannel:
      begin
        if result.red > 1 then result.red := 1;
        if result.green > 1 then result.green := 1;
        if result.blue > 1 then result.blue := 1;
      end;
  end;
  if XYZToRGBOverflowMin = xroSaturateEachChannel then
  begin
    if result.red < 0 then result.red := 0;
    if result.green < 0 then result.green := 0;
    if result.blue < 0 then result.blue := 0;
  end;
  Result.alpha := self.alpha;
end;

function TXYZAHelper.ToWordXYZA: TWordXYZA;
begin
  result.X := ClampInt(round(self.X*50000),0,65535);
  result.Y := ClampInt(round(self.Y*50000),0,65535);
  result.Z := ClampInt(round(self.Z*50000),0,65535);
  result.alpha := round(self.alpha*65535);
end;

function TXYZAHelper.ToLabA: TLabA;
begin
  if FPReferenceWhite=nil then raise FPImageException.Create('Reference white (FPReferenceWhite) can not be null');
  result := self.ToLabA(FPReferenceWhite^);
end;

function TXYZAHelper.ToLabA(const AReferenceWhite: TXYZReferenceWhite): TLabA;
var
  xp, yp, zp: double;
begin
  xp := self.X / AReferenceWhite.X;
  yp := self.Y / AReferenceWhite.Y;
  zp := self.Z / AReferenceWhite.Z;
  if xp > 0.008856 then
    xp := Power(xp, 1 / 3)
  else
    xp := (7.787 * xp) + 0.138;
  if yp > 0.008856 then
    yp := Power(yp, 1 / 3)
  else
    yp := (7.787 * yp) + 0.138;
  if zp > 0.008856 then
    zp := Power(zp, 1 / 3)
  else
    zp := (7.787 * zp) + 0.138;

  result.L := Clamp((116 * yp) - 16, 0, 100);
  result.a := 500 * (xp - yp);
  result.b := 200 * (yp - zp);
  Result.Alpha := self.alpha;
end;

function TXYZAHelper.ToAdobeRGBA: TAdobeRGBA;
begin
  if FPReferenceWhite=nil then raise FPImageException.Create('Reference white (FPReferenceWhite) can not be null');
  result := self.ToAdobeRGBA(FPReferenceWhite^);
end;

function TXYZAHelper.ToAdobeRGBA(const AReferenceWhite: TXYZReferenceWhite): TAdobeRGBA;
var R,G,B: single;
  ad: TXYZA;
begin
  ad := self;
  if AReferenceWhite.Illuminant = 'D50' then
  begin
    ad.ChromaticAdapt(AReferenceWhite, FPReferenceWhite2D50^);
    with ad do
    begin
      R := Clamp(1.9624274*X - 0.6105343*Y - 0.3413404*Z,0,1);
      G := Clamp(-0.9787684*X + 1.9161415*Y + 0.0334540*Z,0,1);
      B := Clamp(0.0286869*X - 0.1406752*Y + 1.3487655*Z,0,1);
    end;
  end else
  begin
    ad.ChromaticAdapt(AReferenceWhite, FPReferenceWhite2D65^);
    with ad do
    begin
      R := Clamp(2.0413690*X - 0.5649464*Y - 0.3446944*Z,0,1);
      G := Clamp(-0.9692660*X + 1.8760108*Y + 0.0415560*Z,0,1);
      B := Clamp(0.0134474*X - 0.1183897*Y + 1.0154096*Z,0,1);
    end;
  end;
  result.red := FPGammaCompressionTab[round(R*65535)];
  result.green := FPGammaCompressionTab[round(G*65535)];
  result.blue := FPGammaCompressionTab[round(B*65535)];
  result.alpha := ClampInt(round(self.alpha*255),0,255);
end;

procedure TXYZAHelper.FromSpectrumRangeReflect(reflectance, wavelen1, wavelen2, alpha: single);
var
  isEqualEnergy: boolean;
  fromRefWhite: PXYZReferenceWhite;

  function GetIlluminantSpectrum(AIndex: integer): single;
  begin
    if isEqualEnergy then result := 1 else
      result := IlluminantSpectrumD65[AIndex].Y;
  end;

  procedure IncludeWavelength(fromWavelen, toWavelen: single);
  var i: integer;
    factor, ill: single;
  begin
    for i := 0 to high(SpectralLocus) do
      if (SpectralLocus[i].W+2.5 >= fromWavelen) and
         (SpectralLocus[i].W-2.5 < toWavelen) then
      begin
        factor := 1;
        if SpectralLocus[i].W-2.5 < fromWavelen then
          factor :=factor-((fromWavelen - (SpectralLocus[i].W-2.5))/5);
        if SpectralLocus[i].W+2.5 > toWavelen then
          factor :=factor-(((SpectralLocus[i].W+2.5) - toWavelen)/5);
        if factor > 0 then
        begin
          ill := GetIlluminantSpectrum(i);
          self.X :=self.X+(SpectralLocus[i].X*factor*ill);
          self.X :=self.Y+(SpectralLocus[i].Y*factor*ill);
          self.X :=self.Z+(SpectralLocus[i].Z*factor*ill);
        end;
      end;
  end;

var
  minWavelen, maxWavelen, ill: single;
  totalXYZ: TXYZA;
  i: Integer;
begin
  self.X := 0;
  self.Y := 0;
  self.Z := 0;
  self.alpha:= alpha;

  if FPReferenceWhite=nil then raise FPImageException.Create('Reference white (FPReferenceWhite) can not be null');

  with FPReferenceWhite^ do
    isEqualEnergy := (X = 1) and (Y = 1) and (Z = 1);
  if isEqualEnergy then fromRefWhite := FPReferenceWhite2E
  else fromRefWhite := FPReferenceWhite2D65;

  totalXYZ := TXYZA.New(0,0,0);
  for i := 0 to high(SpectralLocus) do
  begin
    ill := GetIlluminantSpectrum(i);
    totalXYZ.X :=totalXYZ.X+(SpectralLocus[i].X*ill);
    totalXYZ.Y :=totalXYZ.Y+(SpectralLocus[i].Y*ill);
    totalXYZ.Z :=totalXYZ.Z+(SpectralLocus[i].Z*ill);
  end;

  minWavelen := SpectralLocus[0].W;
  maxWavelen := SpectralLocus[high(SpectralLocus)].W;

  if wavelen1 <= minWavelen then wavelen1 := minWavelen-2.5;
  if wavelen2 >= maxWavelen then wavelen2 := maxWavelen+2.5;

  if wavelen2 > wavelen1 then
    IncludeWavelength(wavelen1, wavelen2)
  else
  begin
    IncludeWavelength(wavelen1, maxWavelen+2.5);
    IncludeWavelength(minWavelen-2.5, wavelen2);
  end;

  self.X := self.X * fromRefWhite^.X/totalXYZ.X * reflectance;
  self.Y := self.Y * fromRefWhite^.Y/totalXYZ.Y * reflectance;
  self.Z := self.Z * fromRefWhite^.Z/totalXYZ.Z * reflectance;
  self.ChromaticAdapt(fromRefWhite^, FPReferenceWhite^);
end;

procedure TXYZAHelper.ToLMS(out L, M, S: single);
begin
  with self do
  begin
    L := max(0.8951*X+0.2664*Y-0.1615*Z, 0);
    M := max(-0.7502*X+1.7135*Y+0.0367*Z, 0);
    S := max(0.0389*X-0.0685*Y+1.0296*Z, 0);
  end;
end;

procedure TXYZAHelper.FromLMS(const L, M, S: Single);
begin
  X := 0.98699*L-0.14705*M+0.16006*S;
  Y := 0.43230*L+0.51836*M+0.04933*S;
  Z := -0.00853*L+0.04004*M+0.96849*S;
  alpha :=1;
end;

{ TWordXYZAHelper }

function TWordXYZAHelper.ToXYZA: TXYZA;
const oneOver50000 = 1/50000;
begin
  result.X := self.X*oneOver50000;
  result.Y := self.Y*oneOver50000;
  result.Z := self.Z*oneOver50000;
  result.alpha:= self.alpha*(1/65535);
end;

function TWordXYZAHelper.ToExpandedPixel: TExpandedPixel;
begin
  if FPReferenceWhite=nil then raise FPImageException.Create('Reference white (FPReferenceWhite) can not be null');
  result := self.ToExpandedPixel(FPReferenceWhite^);
end;

function TWordXYZAHelper.ToExpandedPixel(const AReferenceWhite: TXYZReferenceWhite): TExpandedPixel;
var
  minVal, lightVal, maxVal,
  r,g,b, valRangeDiv2: Int32or64;
  ad: TWordXYZA;
begin
  ad := self;
  if AReferenceWhite.Illuminant = 'D50' then
  begin
    ad.ChromaticAdapt(AReferenceWhite, FPReferenceWhite2D50^);
    with ad do
    begin
      r := round((X * 3.1338561 + Y * (-1.6168667) + Z * (-0.4906146))*(65535/50000));
      g := round((X * (-0.9787684) + Y * 1.9161415 + Z * 0.0334540)*(65535/50000));
      b := round((X * 0.0719453 + Y * (-0.2289914) + Z * 1.4052427)*(65535/50000));
    end;
  end else
  begin
    ad.ChromaticAdapt(AReferenceWhite, FPReferenceWhite2D65^);
    with ad do
    begin
      r := round((X * 3.2404542 + Y * (-1.5371385) + Z * (-0.4985314))*(65535/50000));
      g := round((X * (-0.9692660) + Y * 1.8760108 + Z * 0.0415560)*(65535/50000));
      b := round((X * 0.0556434 + Y * (-0.2040259) + Z * 1.0572252)*(65535/50000));
    end;
  end;
  if ( (XYZToRGBOverflowMin = xroClipToTarget) and ((r < 0) or
       (g < 0) or (b < 0)) ) or
     ( (XYZToRGBOverflowMax = xroClipToTarget) and ((r > 65535) or
       (g > 65535) or (b > 65535)) ) then
  begin
    result.red := 0;
    result.green := 0;
    result.blue := 0;
    result.alpha := 0;
    exit;
  end;
  case XYZToRGBOverflowMin of
    xroPreserveHue: begin
        minVal := min(min(r,g),b);
        if minVal<0 then
        begin
          lightVal := r*redWeightShl10 + g*greenWeightShl10
                    + b*blueWeightShl10;
          if lightVal <= 0 then
          begin
            result.red := 0;
            result.green := 0;
            result.blue := 0;
            Result.alpha := self.alpha;
            exit;
          end else
          begin
            lightVal := (lightVal+512) shr 10;
            valRangeDiv2 := (lightVal-minVal) shr 1;
            r := (int64(r-minVal)*lightVal+valRangeDiv2) div (lightVal-minVal);
            g := (int64(g-minVal)*lightVal+valRangeDiv2) div (lightVal-minVal);
            b := (int64(b-minVal)*lightVal+valRangeDiv2) div (lightVal-minVal);
          end;
        end;
      end;
  end;
  case XYZToRGBOverflowMax of
    xroPreserveHue:
      begin
        maxVal := max(max(r,g),b);
        if maxVal > 65535 then
        begin
          r := (int64(r)*65535+(maxVal shr 1)) div maxVal;
          g := (int64(g)*65535+(maxVal shr 1)) div maxVal;
          b := (int64(b)*65535+(maxVal shr 1)) div maxVal;
        end;
      end;
    xroSaturateEachChannel:
      begin
        if r > 65535 then r := 65535;
        if g > 65535 then g := 65535;
        if b > 65535 then b := 65535;
      end;
  end;
  if XYZToRGBOverflowMin = xroSaturateEachChannel then
  begin
    if r < 0 then r := 0;
    if g < 0 then g := 0;
    if b < 0 then b := 0;
  end;
  result.red := r;
  result.green := g;
  result.blue := b;
  Result.alpha := self.alpha;
end;


procedure FPGammaNone;
var i,j: integer;
  prevExp, nextExp: Word;
begin
  FPGammaExpFactor := 1;
  prevExp := 0;
  for i := 0 to 255 do
  begin
    FPGammaExpansionTab[i] := (i shl 8) + i;
    if i = 255 then nextExp := 65535
    else
    begin
      nextExp := FPGammaExpansionTab[i]+128;
      FPGammaExpansionTabHalf[i] := nextExp+1;
    end;
    for j := prevExp to nextExp do
      FPGammaCompressionTab[j] := i;
    if i < 255 then
      prevExp := nextExp+1;
  end;
end;

procedure FPGammaSet(AGamma: single);
var
  GammaLinearFactor: single;
  i,j,prevpos,nextpos,midpos: Int32or64;
begin
  if AGamma = 1 then
  begin
    FPGammaNone;
    exit;
  end;
  FPGammaExpFactor := AGamma;
  //the linear factor is used to normalize expanded values in the range 0..65535
  GammaLinearFactor := 65535 / power(255, FPGammaExpFactor);
  FPGammaExpansionTab[0] := 0;
  nextpos := 0;
  for i := 0 to 255 do
  begin
    prevpos := nextpos;
    midpos := round(power(i, FPGammaExpFactor) * GammaLinearFactor);
    if i = 255 then
      nextpos := 65536
    else
      nextpos := round(power(i+0.5, FPGammaExpFactor) * GammaLinearFactor);
    FPGammaExpansionTab[i] := midpos;
    if i < 255 then
      FPGammaExpansionTabHalf[i] := nextpos;
    for j := prevpos to midpos-1 do
      FPGammaCompressionTab[j] := i;
    for j := midpos to nextpos-1 do
      FPGammaCompressionTab[j] := i;
  end;
  FPGammaCompressionTab[0] := 0;
end;

function FPGammaGet: single;
begin
  result := FPGammaExpFactor;
end;

function FPHueDiff(h1, h2: word): word;
begin
  result := abs(integer(h1)-integer(h2));
  if result > 32768 then result := 65536-result;
end;

function FPHueGSBAToHSLA(ghue: word): word;
const
  segment: array[0..5] of UInt32or64 =
     (13653, 10923, 8192, 13653, 10923, 8192);
var g: UInt32or64;
begin
  g := ghue;
  if g < segment[0] then
    result := g * 10923 div segment[0]
  else
  begin
    dec(g, segment[0]);
    if g < segment[1] then
      result := g * (21845-10923) div segment[1] + 10923
    else
    begin
      dec(g, segment[1]);
      if g < segment[2] then
        result := g * (32768-21845) div segment[2] + 21845
      else
      begin
        dec(g, segment[2]);
        if g < segment[3] then
          result := g * (43691-32768) div segment[3] + 32768
        else
        begin
          dec(g, segment[3]);
          if g < segment[4] then
            result := g * (54613-43691) div segment[4] + 43691
          else
          begin
            dec(g, segment[4]);
            result := g * (65536-54613) div segment[5] + 54613;
          end;
        end;
      end;
    end;
  end;
end;

function FPHueHSLAToGSBA(hue: word): word;
const
  segmentDest: array[0..5] of UInt32or64 =
     (13653, 10923, 8192, 13653, 10923, 8192);
  segmentSrc: array[0..5] of UInt32or64 =
     (10923, 10922, 10923, 10923, 10922, 10923);
var
  h,g: UInt32or64;
begin
  h := hue;
  if h < segmentSrc[0] then
    g := h * segmentDest[0] div segmentSrc[0]
  else
  begin
    g := segmentDest[0];
    dec(h, segmentSrc[0]);
    if h < segmentSrc[1] then
      inc(g, h * segmentDest[1] div segmentSrc[1])
    else
    begin
      inc(g, segmentDest[1]);
      dec(h, segmentSrc[1]);
      if h < segmentSrc[2] then
        inc(g, h * segmentDest[2] div segmentSrc[2])
      else
      begin
        inc(g, segmentDest[2]);
        dec(h, segmentSrc[2]);
        if h < segmentSrc[3] then
          inc(g, h * segmentDest[3] div segmentSrc[3])
        else
        begin
          inc(g, segmentDest[3]);
          dec(h, segmentSrc[3]);
          if h < segmentSrc[4] then
            inc(g, h * segmentDest[4] div segmentSrc[4])
          else
          begin
            inc(g, segmentDest[4]);
            dec(h, segmentSrc[4]);
            inc(g, h * segmentDest[5] div segmentSrc[5]);
          end;
        end;
      end;
    end;
  end;
  result := g;
end;

procedure PrepareReferenceWhiteArray;
begin
  //Source:http://www.easyrgb.com/index.php?X=MATH&H=15#text15
  //domestic, tungsten-filament lighting
  FPReferenceWhiteAdd(2, 'A', 1.09850, 1.00, 0.35585);
  FPReferenceWhiteAdd(10, 'A', 1.11144, 1.00, 0.35200);
  //deprecated daylight
  FPReferenceWhiteAdd(2, 'C', 0.98074, 1.00, 1.18232);
  FPReferenceWhiteAdd(10, 'C', 0.97285, 1.00, 1.16145);
  //daylight
  FPReferenceWhite2D50 :=FPReferenceWhiteAdd(2, 'D50', 0.96422, 1.00, 0.82521);
  FPReferenceWhiteAdd(10, 'D50', 0.96720, 1.00, 0.81427);
  FPReferenceWhiteAdd(2, 'D55', 0.95682, 1.00, 0.92149);
  FPReferenceWhiteAdd(10, 'D55', 0.95799, 1.00, 0.90926);
  FPReferenceWhite2D65 := FPReferenceWhiteAdd(2, 'D65', 0.95047, 1.00, 1.08883);
  FPReferenceWhiteAdd(10, 'D65', 0.94811, 1.00, 1.07304);
  FPReferenceWhiteAdd(2, 'D75', 0.94972, 1.00, 1.22638);
  FPReferenceWhiteAdd(10, 'D75', 0.94416, 1.00, 1.20641);
  //equal energy
  FPReferenceWhite2E := FPReferenceWhiteAdd(2, 'E', 1,1,1);
  FPReferenceWhiteAdd(10, 'E', 1,1,1);
  //fluorescent light
  FPReferenceWhiteAdd(2, 'F2', 0.99187, 1.00, 0.67395);
  FPReferenceWhiteAdd(10, 'F2', 1.03280, 1.00, 0.69026);
  FPReferenceWhiteAdd(2, 'F7', 0.95044, 1.00, 1.08755);
  FPReferenceWhiteAdd(10, 'F7', 0.95792, 1.00, 1.07687);
  FPReferenceWhiteAdd(2, 'F11', 1.00966, 1.00, 0.64370);
  FPReferenceWhiteAdd(10, 'F11', 1.03866, 1.00, 0.65627);
end;

procedure FPReferenceWhiteSet(AObserverAngle: integer; AIlluminant: TIlluminant);
var
  i: integer;
begin
  for i := 0 to Length(FPReferenceWhiteArray) - 1 do
  begin
    if (FPReferenceWhiteArray[i].ObserverAngle = AObserverAngle) and (FPReferenceWhiteArray[i].Illuminant = AIlluminant) then
    begin
      FPReferenceWhite := @FPReferenceWhiteArray[i];
      Break;
    end;
  end;
end;

procedure FPReferenceWhiteSet(AReferenceWhite: TXYZReferenceWhite);
begin
  FPReferenceWhiteSet(AReferenceWhite.ObserverAngle, AReferenceWhite.Illuminant);
  if (FPReferenceWhite<>nil)
  then FPReferenceWhite^ :=AReferenceWhite;
end;

function FPReferenceWhiteGet(AObserverAngle: integer; AIlluminant: TIlluminant): PXYZReferenceWhite;
var
   rp: PXYZReferenceWhite;
   i: integer;

begin
  for i := 0 to Length(FPReferenceWhiteArray) - 1 do
  begin
    rp := @FPReferenceWhiteArray[i];
    if (rp^.ObserverAngle = AObserverAngle) and (rp^.Illuminant = AIlluminant) then
    begin
      result := rp;
      exit;
    end;
  end;
  result := nil;
end;

function FPReferenceWhiteAdd(const AReferenceWhite: TXYZReferenceWhite):PXYZReferenceWhite;
begin
  if FPReferenceWhiteGet(AReferenceWhite.ObserverAngle, AReferenceWhite.Illuminant)<>nil then
    raise FPImageException.Create('Reference white already defined');
  SetLength(FPReferenceWhiteArray, Length(FPReferenceWhiteArray) + 1);
  FPReferenceWhiteArray[Length(FPReferenceWhiteArray) - 1] := AReferenceWhite;
  with FPReferenceWhiteArray[Length(FPReferenceWhiteArray) - 1] do
  begin
    L := max(0.8951*X+0.2664*Y-0.1615*Z, 0);
    M := max(-0.7502*X+1.7135*Y+0.0367*Z, 0);
    S := max(0.0389*X-0.0685*Y+1.0296*Z, 0);
  end;
  Result :=@FPReferenceWhiteArray[Length(FPReferenceWhiteArray) - 1];
end;

function FPReferenceWhiteAdd(AObserverAngle: integer; AIlluminant: TIlluminant; AX, AY, AZ: single):PXYZReferenceWhite;
var
  rp: TXYZReferenceWhite;
begin
  rp.Illuminant := AIlluminant;
  rp.ObserverAngle := AObserverAngle;
  rp.X := AX;
  rp.Y := AY;
  rp.Z := AZ;
  Result :=FPReferenceWhiteAdd(rp);
end;

procedure XYZToLMS(const X,Y,Z: Single; out L,M,S: single);
begin
  L := max(0.8951*X+0.2664*Y-0.1615*Z, 0);
  M := max(-0.7502*X+1.7135*Y+0.0367*Z, 0);
  S := max(0.0389*X-0.0685*Y+1.0296*Z, 0);
end;

procedure LMSToXYZ(const L,M,S: Single; out X,Y,Z: single);
begin
  X := 0.98699*L-0.14705*M+0.16006*S;
  Y := 0.43230*L+0.51836*M+0.04933*S;
  Z := -0.00853*L+0.04004*M+0.96849*S;
end;

procedure FPChromaticAdaptXYZ(var X,Y,Z: Single; const AFrom, ATo: TXYZReferenceWhite);
var
  L, M, S: single;
begin
  if (AFrom.L=ATo.L) and (AFrom.M=ATo.M) and (AFrom.S=ATo.S) then exit;
  XYZToLMS(X,Y,Z, L,M,S);
  L := L * ATo.L/AFrom.L;
  M := M * ATo.M/AFrom.M;
  S := S * ATo.S/AFrom.S;
  LMSToXYZ(L,M,S, X,Y,Z);
end;

procedure FPChromaticAdaptWordXYZ(var X,Y,Z: Word; const AFrom, ATo: TXYZReferenceWhite);
const oneOver50000 = 1/50000;
var Xf,Yf,Zf: Single;
begin
  Xf := X*oneOver50000;
  Yf := Y*oneOver50000;
  Zf := Z*oneOver50000;
  FPChromaticAdaptXYZ(Xf,Yf,Zf, AFrom,ATo);
  X := min(round(Xf*50000),65535);
  Y := min(round(Yf*50000),65535);
  Z := min(round(Zf*50000),65535);
end;


initialization
   FPGammaSet();

   PrepareReferenceWhiteArray;
   FPReferenceWhite :=FPReferenceWhite2D50;


end.
