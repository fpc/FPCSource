{
    Tests for fpcolorspace: sRGB transfer, XYZ, Lab, LCh, HSL, HSV, CMYK,
    YCbCr, Adobe RGB, chromatic adaptation and round trips, against published values.
    See the file COPYING.FPC, included in this distribution, for details.
}
unit tccolorspace;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$modeswitch typehelpers}

interface

uses sysutils, classes, math, fpcunit, testregistry, fpimage, fpimgtests, fpcolorspace;

type
  TTestColorSpace = class(TTestCase)
  private
    // Fails unless the four components are within aTolerance of the expected values.
    procedure CheckRGBA(const aMessage: String; aRed, aGreen, aBlue, aAlpha: Double; const aActual: TStdRGBA; aTolerance: Double);
    // Fails unless X, Y and Z are within aTolerance of the expected values.
    procedure CheckXYZ(const aMessage: String; aX, aY, aZ: Double; const aActual: TXYZA; aTolerance: Double);
    // Fails unless L, a and b are within aTolerance of the expected values.
    procedure CheckLab(const aMessage: String; aL, aA, aB: Double; const aActual: TLabA; aTolerance: Double);
    // Fails unless hue, saturation and lightness are within aTolerance of the expected values.
    procedure CheckHSL(const aMessage: String; aHue, aSaturation, aLightness: Double; const aActual: TStdHSLA; aTolerance: Double);
    // Fails unless hue, saturation and value are within aTolerance of the expected values.
    procedure CheckHSV(const aMessage: String; aHue, aSaturation, aValue: Double; const aActual: TStdHSVA; aTolerance: Double);
    // Fails unless C, M, Y and K are within aTolerance of the expected values.
    procedure CheckCMYK(const aMessage: String; aC, aM, aY, aK: Double; const aActual: TStdCMYK; aTolerance: Double);
    // Fails unless Y, Cb and Cr are within aTolerance of the expected values.
    procedure CheckYCbCr(const aMessage: String; aY, aCb, aCr: Double; const aActual: TYCbCr; aTolerance: Double);
    // Fails unless the YCbCr of the primaries, black and white follow the luma weights aKr and aKb.
    procedure CheckYCbCrStandard(const aName: String; aStd: TYCbCrSTD; aKr, aKb: Double);
    // The colour number aIndex of the test grid: 5 levels per channel, alpha 255, 128 or 0.
    function GridColor(aIndex: Integer; aWithAlpha: Boolean): TFPColor;
    // Number of colours in the test grid.
    function GridCount(aWithAlpha: Boolean): Integer;
    // Adds a D65 reference white, which already exists.
    procedure AddDuplicateWhite;
  published
    procedure TestSRGBToLinearTransfer;
    procedure TestLinearToSRGBTransfer;
    procedure TestTransferEndPoints;
    procedure TestSRGBWhiteToXYZD65;
    procedure TestSRGBPrimariesToXYZ;
    procedure TestXYZOfD65WhiteToLinearRGB;
    procedure TestLabOfWhite;
    procedure TestLabOfPrimaries;
    procedure TestLabOfMidGray;
    procedure TestLabToXYZ;
    procedure TestLabToLCh;
    procedure TestLChToLab;
    procedure TestHSLOfPrimariesAndSecondaries;
    procedure TestHSLOfGrays;
    procedure TestHSVOfPrimariesAndSecondaries;
    procedure TestHSVOfGrays;
    procedure TestHSLToRGB;
    procedure TestHSVToRGB;
    procedure TestHSLToHSV;
    procedure TestHSVToHSL;
    procedure TestCMYKOfPrimaries;
    procedure TestCMYKToRGB;
    procedure TestCMYKToExpandedPixelKeepsAlpha;
    procedure TestYCbCr601;
    procedure TestYCbCr709;
    procedure TestYCbCr2020;
    procedure TestYCbCrJPEG;
    procedure TestYCbCrToRGBIsOpaque;
    procedure TestYCbCrLumaOverloadsAreInverse;
    procedure TestAdobeRGBWhite;
    procedure TestAdobeRGBRed;
    procedure TestAdobeRGBMidGray;
    procedure TestXYZToAdobeRGBOfWhite;
    procedure TestChromaticAdaptationOfWhite;
    procedure TestChromaticAdaptationOfRed;
    procedure TestReferenceWhites;
    procedure TestGammaHelpersAreInverse;
    procedure TestGammaHelpersAreMonotonic;
    procedure TestGammaNoneIsIdentity;
    procedure TestExpandedPixelToStdRGBA;
    procedure TestExpandedPixelIntensityAndLightness;
    procedure TestByteMask;
    procedure TestHSLAPixelOfPrimaries;
    procedure TestWordXYZAOfWhite;
    procedure TestSpectrumOfPerfectReflector;
    procedure TestFPColorHelperNew;
    procedure TestRoundTripThroughStdRGBA;
    procedure TestRoundTripThroughExpandedPixel;
    procedure TestRoundTripThroughStdHSLA;
    procedure TestRoundTripThroughStdHSVA;
    procedure TestRoundTripThroughStdCMYK;
    procedure TestRoundTripThroughLinearRGBA;
    procedure TestRoundTripThroughLab;
    procedure TestRoundTripThroughLCh;
    procedure TestRoundTripThroughYCbCr;
    procedure TestRoundTripThroughHSLAPixel;
    procedure TestRoundTripThroughGSBAPixel;
    procedure TestRoundTripThroughWordXYZA;
  end;

implementation

const
  cD65X = 0.95047;
  cD65Z = 1.08883;
  cD50X = 0.96422;
  cD50Z = 0.82521;
  cLevels: array[0..4] of Byte = (0, 51, 128, 204, 255);
  cAlphas: array[0..2] of Byte = (255, 128, 0);


// The 2 degree D65 reference white, looked up in FPReferenceWhiteArray.
function WhiteD65: TXYZReferenceWhite;

begin
  Result := FPReferenceWhiteGet(2, 'D65')^;
end;


// The 2 degree D50 reference white, looked up in FPReferenceWhiteArray.
function WhiteD50: TXYZReferenceWhite;

begin
  Result := FPReferenceWhiteGet(2, 'D50')^;
end;


// Fails unless aActual is a number within 1e-6 of aExpected.
procedure CheckAlpha(const aMessage: String; aExpected: Double; aActual: Single);

begin
  if IsNan(aActual) then
    TAssert.Fail(Format('%s: expected %g, got NaN', [aMessage, aExpected]));
  if IsInfinite(aActual) or (Abs(aActual - aExpected) > 1e-6) then
    TAssert.Fail(Format('%s: expected %g, got %g', [aMessage, aExpected, Double(aActual)]));
end;


// The sRGB electro-optical transfer function of IEC 61966-2-1.
function SRGBToLinear(aValue: Double): Double;

begin
  if aValue <= 0.04045 then
    Result := aValue / 12.92
  else
    Result := Power((aValue + 0.055) / 1.055, 2.4);
end;


// The inverse sRGB transfer function of IEC 61966-2-1.
function LinearToSRGB(aValue: Double): Double;

begin
  if aValue <= 0.0031308 then
    Result := aValue * 12.92
  else
    Result := 1.055 * Power(aValue, 1 / 2.4) - 0.055;
end;


procedure TTestColorSpace.CheckRGBA(const aMessage: String; aRed, aGreen, aBlue, aAlpha: Double; const aActual: TStdRGBA; aTolerance: Double);

begin
  AssertEquals(aMessage + ': red', aRed, aActual.red, aTolerance);
  AssertEquals(aMessage + ': green', aGreen, aActual.green, aTolerance);
  AssertEquals(aMessage + ': blue', aBlue, aActual.blue, aTolerance);
  AssertEquals(aMessage + ': alpha', aAlpha, aActual.alpha, aTolerance);
end;


procedure TTestColorSpace.CheckXYZ(const aMessage: String; aX, aY, aZ: Double; const aActual: TXYZA; aTolerance: Double);

begin
  AssertEquals(aMessage + ': X', aX, aActual.X, aTolerance);
  AssertEquals(aMessage + ': Y', aY, aActual.Y, aTolerance);
  AssertEquals(aMessage + ': Z', aZ, aActual.Z, aTolerance);
end;


procedure TTestColorSpace.CheckLab(const aMessage: String; aL, aA, aB: Double; const aActual: TLabA; aTolerance: Double);

begin
  AssertEquals(aMessage + ': L*', aL, aActual.L, aTolerance);
  AssertEquals(aMessage + ': a*', aA, aActual.a, aTolerance);
  AssertEquals(aMessage + ': b*', aB, aActual.b, aTolerance);
end;


procedure TTestColorSpace.CheckHSL(const aMessage: String; aHue, aSaturation, aLightness: Double; const aActual: TStdHSLA; aTolerance: Double);

begin
  AssertEquals(aMessage + ': hue', aHue, aActual.hue, aTolerance * 360);
  AssertEquals(aMessage + ': saturation', aSaturation, aActual.saturation, aTolerance);
  AssertEquals(aMessage + ': lightness', aLightness, aActual.lightness, aTolerance);
end;


procedure TTestColorSpace.CheckHSV(const aMessage: String; aHue, aSaturation, aValue: Double; const aActual: TStdHSVA; aTolerance: Double);

begin
  AssertEquals(aMessage + ': hue', aHue, aActual.hue, aTolerance * 360);
  AssertEquals(aMessage + ': saturation', aSaturation, aActual.saturation, aTolerance);
  AssertEquals(aMessage + ': value', aValue, aActual.value, aTolerance);
end;


procedure TTestColorSpace.CheckCMYK(const aMessage: String; aC, aM, aY, aK: Double; const aActual: TStdCMYK; aTolerance: Double);

begin
  AssertEquals(aMessage + ': C', aC, aActual.C, aTolerance);
  AssertEquals(aMessage + ': M', aM, aActual.M, aTolerance);
  AssertEquals(aMessage + ': Y', aY, aActual.Y, aTolerance);
  AssertEquals(aMessage + ': K', aK, aActual.K, aTolerance);
end;


procedure TTestColorSpace.CheckYCbCr(const aMessage: String; aY, aCb, aCr: Double; const aActual: TYCbCr; aTolerance: Double);

begin
  AssertEquals(aMessage + ': Y', aY, aActual.Y, aTolerance);
  AssertEquals(aMessage + ': Cb', aCb, aActual.Cb, aTolerance);
  AssertEquals(aMessage + ': Cr', aCr, aActual.Cr, aTolerance);
end;


procedure TTestColorSpace.CheckYCbCrStandard(const aName: String; aStd: TYCbCrSTD; aKr, aKb: Double);

const
  cColors: array[0..7, 0..2] of Double = ((0, 0, 0), (1, 1, 1), (1, 0, 0), (0, 1, 0),
    (0, 0, 1), (1, 1, 0), (0, 1, 1), (0.5, 0.5, 0.5));

var
  I: Integer;
  lKg, lY: Double;
  lColor: TStdRGBA;

begin
  lKg := 1 - aKr - aKb;
  for I := 0 to High(cColors) do
    begin
    lColor := TStdRGBA.New(cColors[I, 0], cColors[I, 1], cColors[I, 2]);
    lY := aKr * cColors[I, 0] + lKg * cColors[I, 1] + aKb * cColors[I, 2];
    CheckYCbCr(Format('%s (Kr=%g, Kb=%g), Cb = 0.5 + (B-Y)/(2-2Kb), Cr = 0.5 + (R-Y)/(2-2Kr), of RGB (%g,%g,%g)',
      [aName, aKr, aKb, cColors[I, 0], cColors[I, 1], cColors[I, 2]]),
      lY, 0.5 + (cColors[I, 2] - lY) / (2 - 2 * aKb), 0.5 + (cColors[I, 0] - lY) / (2 - 2 * aKr),
      lColor.ToYCbCr(aStd, 0.5), 1e-4);
    end;
  CheckYCbCr(aName + ' of white is (1, 0.5, 0.5)', 1, 0.5, 0.5, TStdRGBA.New(1, 1, 1).ToYCbCr(aStd, 0.5), 1e-4);
  CheckYCbCr(aName + ' of black is (0, 0.5, 0.5)', 0, 0.5, 0.5, TStdRGBA.New(0, 0, 0).ToYCbCr(aStd, 0.5), 1e-4);
  AssertEquals(aName + ' of red has the largest Cr, 1.0', 1.0, TStdRGBA.New(1, 0, 0).ToYCbCr(aStd, 0.5).Cr, 1e-4);
  AssertEquals(aName + ' of blue has the largest Cb, 1.0', 1.0, TStdRGBA.New(0, 0, 1).ToYCbCr(aStd, 0.5).Cb, 1e-4);
end;


function TTestColorSpace.GridColor(aIndex: Integer; aWithAlpha: Boolean): TFPColor;

var
  lAlpha: Byte;

begin
  if aWithAlpha then
    lAlpha := cAlphas[aIndex div 125]
  else
    lAlpha := 255;
  aIndex := aIndex mod 125;
  Result := RGB8(cLevels[aIndex mod 5], cLevels[(aIndex div 5) mod 5], cLevels[aIndex div 25], lAlpha);
end;


function TTestColorSpace.GridCount(aWithAlpha: Boolean): Integer;

begin
  if aWithAlpha then
    Result := 125 * Length(cAlphas)
  else
    Result := 125;
end;


procedure TTestColorSpace.AddDuplicateWhite;

begin
  FPReferenceWhiteAdd(2, 'D65', cD65X, 1, cD65Z);
end;


procedure TTestColorSpace.TestSRGBToLinearTransfer;

const
  cValues: array[0..8] of Double = (0.01, 0.02, 0.04045, 0.05, 0.1, 0.2, 0.5, 0.735, 0.9);

var
  I: Integer;
  lLinear: TLinearRGBA;

begin
  for I := 0 to High(cValues) do
    begin
    lLinear := TStdRGBA.New(cValues[I], cValues[I], cValues[I]).ToLinearRGBA;
    AssertEquals(Format('sRGB %g to linear follows IEC 61966-2-1 (V/12.92 up to 0.04045, ((V+0.055)/1.055)^2.4 above)',
      [cValues[I]]), SRGBToLinear(cValues[I]), lLinear.red, 0.002);
    end;
end;


procedure TTestColorSpace.TestLinearToSRGBTransfer;

const
  cValues: array[0..7] of Double = (0.001, 0.0031308, 0.01, 0.05, 0.2140, 0.5, 0.8, 0.95);

var
  I: Integer;
  lColor: TStdRGBA;

begin
  for I := 0 to High(cValues) do
    begin
    lColor := TLinearRGBA.New(cValues[I], cValues[I], cValues[I]).ToStdRGBA;
    AssertEquals(Format('linear %g to sRGB follows IEC 61966-2-1 (12.92L up to 0.0031308, 1.055L^(1/2.4)-0.055 above)',
      [cValues[I]]), LinearToSRGB(cValues[I]), lColor.red, 0.002);
    end;
end;


procedure TTestColorSpace.TestTransferEndPoints;

begin
  CheckRGBA('sRGB black is linear black', 0, 0, 0, 1, TStdRGBA.New(0, 0, 0).ToLinearRGBA.ToStdRGBA, 1e-6);
  AssertEquals('sRGB white is linear 1', 1.0, TStdRGBA.New(1, 1, 1).ToLinearRGBA.red, 1e-6);
  AssertEquals('sRGB black is linear 0', 0.0, TStdRGBA.New(0, 0, 0).ToLinearRGBA.red, 1e-6);
  AssertEquals('linear 1 is sRGB white', 1.0, TLinearRGBA.New(1, 1, 1).ToStdRGBA.red, 1e-6);
  AssertEquals('linear 0 is sRGB black', 0.0, TLinearRGBA.New(0, 0, 0).ToStdRGBA.red, 1e-6);
end;


procedure TTestColorSpace.TestSRGBWhiteToXYZD65;

begin
  CheckXYZ('sRGB white is the D65 white point (0.9505, 1.0000, 1.0890) of IEC 61966-2-1', 0.9505, 1.0, 1.0890,
    TStdRGBA.New(1, 1, 1).ToLinearRGBA.ToXYZA(WhiteD65), 0.001);
end;


procedure TTestColorSpace.TestSRGBPrimariesToXYZ;

begin
  CheckXYZ('sRGB red is XYZ (0.4124, 0.2126, 0.0193) (IEC 61966-2-1)', 0.4124, 0.2126, 0.0193,
    TLinearRGBA.New(1, 0, 0).ToXYZA(WhiteD65), 5e-4);
  CheckXYZ('sRGB green is XYZ (0.3576, 0.7152, 0.1192) (IEC 61966-2-1)', 0.3576, 0.7152, 0.1192,
    TLinearRGBA.New(0, 1, 0).ToXYZA(WhiteD65), 5e-4);
  CheckXYZ('sRGB blue is XYZ (0.1805, 0.0722, 0.9505) (IEC 61966-2-1)', 0.1805, 0.0722, 0.9505,
    TLinearRGBA.New(0, 0, 1).ToXYZA(WhiteD65), 5e-4);
  CheckXYZ('sRGB red through the sRGB transfer is XYZ (0.4124, 0.2126, 0.0193)', 0.4124, 0.2126, 0.0193,
    TStdRGBA.New(1, 0, 0).ToLinearRGBA.ToXYZA(WhiteD65), 5e-4);
end;


procedure TTestColorSpace.TestXYZOfD65WhiteToLinearRGB;

var
  lLinear: TLinearRGBA;

begin
  lLinear := TXYZA.New(cD65X, 1, cD65Z).ToLinearRGBA(WhiteD65);
  AssertEquals('D65 white is linear red 1', 1.0, lLinear.red, 0.001);
  AssertEquals('D65 white is linear green 1', 1.0, lLinear.green, 0.001);
  AssertEquals('D65 white is linear blue 1', 1.0, lLinear.blue, 0.001);
  CheckAlpha('XYZ to linear RGB keeps alpha', 1.0, lLinear.alpha);
end;


procedure TTestColorSpace.TestLabOfWhite;

begin
  CheckLab('the D65 white point is Lab (100, 0, 0) relative to D65', 100, 0, 0,
    TXYZA.New(cD65X, 1, cD65Z).ToLabA(WhiteD65), 0.01);
  CheckLab('the D50 white point is Lab (100, 0, 0) relative to D50', 100, 0, 0,
    TXYZA.New(cD50X, 1, cD50Z).ToLabA(WhiteD50), 0.01);
  CheckLab('XYZ black is Lab (0, 0, 0)', 0, 0, 0, TXYZA.New(0, 0, 0).ToLabA(WhiteD65), 0.01);
end;


procedure TTestColorSpace.TestLabOfPrimaries;

  // The D65 Lab of a linear sRGB colour.
  function LabOf(aRed, aGreen, aBlue: Single): TLabA;

  begin
    Result := TLinearRGBA.New(aRed, aGreen, aBlue).ToXYZA(WhiteD65).ToLabA(WhiteD65);
  end;

begin
  CheckLab('sRGB red is Lab (53.24, 80.09, 67.20) at D65 (Lindbloom)', 53.2408, 80.0925, 67.2032, LabOf(1, 0, 0), 0.05);
  CheckLab('sRGB green is Lab (87.73, -86.18, 83.18) at D65 (Lindbloom)', 87.7347, -86.1827, 83.1793, LabOf(0, 1, 0), 0.05);
  CheckLab('sRGB blue is Lab (32.30, 79.19, -107.86) at D65 (Lindbloom)', 32.2970, 79.1875, -107.8602, LabOf(0, 0, 1), 0.05);
end;


procedure TTestColorSpace.TestLabOfMidGray;

var
  lLab: TLabA;

begin
  lLab := TStdRGBA.New(0.5, 0.5, 0.5).ToLinearRGBA.ToXYZA(WhiteD65).ToLabA(WhiteD65);
  CheckLab('sRGB 50% gray is Lab (53.39, 0, 0) at D65', 53.389, 0, 0, lLab, 0.1);
end;


procedure TTestColorSpace.TestLabToXYZ;

begin
  CheckXYZ('Lab (100, 0, 0) is the D65 white point', cD65X, 1, cD65Z,
    TLabA.New(100, 0, 0).ToXYZA(WhiteD65), 1e-4);
  CheckXYZ('Lab (50, 0, 0) is Y = ((50+16)/116)^3 = 0.1842 on the gray axis', cD65X * 0.18419, 0.18419, cD65Z * 0.18419,
    TLabA.New(50, 0, 0).ToXYZA(WhiteD65), 1e-4);
  CheckXYZ('Lab (5, 0, 0) is Y = 5/903.3 = 0.005535 in the linear segment', cD65X * 0.005535, 0.005535, cD65Z * 0.005535,
    TLabA.New(5, 0, 0).ToXYZA(WhiteD65), 1e-4);
  CheckXYZ('Lab (53.24, 80.09, 67.20) is sRGB red XYZ (0.4124, 0.2126, 0.0193)', 0.4124, 0.2126, 0.0193,
    TLabA.New(53.2408, 80.0925, 67.2032).ToXYZA(WhiteD65), 5e-4);
  CheckAlpha('Lab to XYZ keeps alpha', 0.25, TLabA.New(50, 10, 10, 0.25).ToXYZA(WhiteD65).alpha);
end;


procedure TTestColorSpace.TestLabToLCh;

var
  lLCh: TLChA;

begin
  lLCh := TLabA.New(50, 0, 50, 0.5).ToLChA;
  AssertEquals('Lab (50, 0, 50) has L 50', 50, lLCh.L, 1e-4);
  AssertEquals('Lab (50, 0, 50) has chroma 50', 50, lLCh.C, 1e-4);
  AssertEquals('Lab (50, 0, 50) has hue 90 degrees', 90, lLCh.h, 1e-3);
  CheckAlpha('Lab to LCh keeps alpha', 0.5, lLCh.alpha);
  lLCh := TLabA.New(50, -30, 0).ToLChA;
  AssertEquals('Lab (50, -30, 0) has chroma 30', 30, lLCh.C, 1e-4);
  AssertEquals('Lab (50, -30, 0) has hue 180 degrees', 180, lLCh.h, 1e-3);
  lLCh := TLabA.New(60, 20, -20).ToLChA;
  AssertEquals('Lab (60, 20, -20) has chroma 28.284', 28.2843, lLCh.C, 1e-3);
  AssertEquals('Lab (60, 20, -20) has hue 315 degrees', 315, lLCh.h, 1e-3);
  lLCh := TLabA.New(60, 20, 0).ToLChA;
  AssertEquals('Lab (60, 20, 0) has hue 0 degrees', 0, lLCh.h, 1e-3);
end;


procedure TTestColorSpace.TestLChToLab;

var
  lLab: TLabA;

begin
  CheckLab('LCh (50, 50, 90) is Lab (50, 0, 50)', 50, 0, 50, TLChA.New(50, 50, 90).ToLabA, 1e-3);
  CheckLab('LCh (70, 40, 225) is Lab (70, -28.284, -28.284)', 70, -28.2843, -28.2843, TLChA.New(70, 40, 225).ToLabA, 1e-3);
  CheckLab('LCh (40, 30, 0) is Lab (40, 30, 0)', 40, 30, 0, TLChA.New(40, 30, 0).ToLabA, 1e-3);
  lLab := TLChA.New(40, 30, 180, 0.75).ToLabA;
  CheckAlpha('LCh to Lab keeps alpha', 0.75, lLab.alpha);
end;


procedure TTestColorSpace.TestHSLOfPrimariesAndSecondaries;

begin
  CheckHSL('red is HSL (0, 1, 0.5)', 0, 1, 0.5, TFPColor.New($FFFF, 0, 0).ToStdHSLA, 1e-4);
  CheckHSL('yellow is HSL (60, 1, 0.5)', 60, 1, 0.5, TFPColor.New($FFFF, $FFFF, 0).ToStdHSLA, 1e-4);
  CheckHSL('green is HSL (120, 1, 0.5)', 120, 1, 0.5, TFPColor.New(0, $FFFF, 0).ToStdHSLA, 1e-4);
  CheckHSL('cyan is HSL (180, 1, 0.5)', 180, 1, 0.5, TFPColor.New(0, $FFFF, $FFFF).ToStdHSLA, 1e-4);
  CheckHSL('blue is HSL (240, 1, 0.5)', 240, 1, 0.5, TFPColor.New(0, 0, $FFFF).ToStdHSLA, 1e-4);
  CheckHSL('magenta is HSL (300, 1, 0.5)', 300, 1, 0.5, TFPColor.New($FFFF, 0, $FFFF).ToStdHSLA, 1e-4);
  CheckHSL('dark red (0.5, 0, 0) is HSL (0, 1, 0.25)', 0, 1, 0.25, TStdRGBA.New(0.5, 0, 0).ToStdHSLA, 1e-4);
  CheckHSL('pink (1, 0.5, 0.5) is HSL (0, 1, 0.75)', 0, 1, 0.75, TStdRGBA.New(1, 0.5, 0.5).ToStdHSLA, 1e-4);
  CheckAlpha('RGB to HSL keeps alpha', 0.5, TStdRGBA.New(1, 0, 0, 0.5).ToStdHSLA.alpha);
end;


procedure TTestColorSpace.TestHSLOfGrays;

begin
  CheckHSL('black is HSL (0, 0, 0)', 0, 0, 0, TFPColor.New(0, 0, 0).ToStdHSLA, 1e-4);
  CheckHSL('white is HSL (0, 0, 1)', 0, 0, 1, TFPColor.New($FFFF, $FFFF, $FFFF).ToStdHSLA, 1e-4);
  CheckHSL('50% gray is HSL (0, 0, 0.5)', 0, 0, 0.5, TStdRGBA.New(0.5, 0.5, 0.5).ToStdHSLA, 1e-4);
  CheckHSL('20% gray is HSL (0, 0, 0.2)', 0, 0, 0.2, TStdRGBA.New(0.2, 0.2, 0.2).ToStdHSLA, 1e-4);
end;


procedure TTestColorSpace.TestHSVOfPrimariesAndSecondaries;

begin
  CheckHSV('red is HSV (0, 1, 1)', 0, 1, 1, TFPColor.New($FFFF, 0, 0).ToStdHSVA, 1e-4);
  CheckHSV('yellow is HSV (60, 1, 1)', 60, 1, 1, TFPColor.New($FFFF, $FFFF, 0).ToStdHSVA, 1e-4);
  CheckHSV('green is HSV (120, 1, 1)', 120, 1, 1, TFPColor.New(0, $FFFF, 0).ToStdHSVA, 1e-4);
  CheckHSV('cyan is HSV (180, 1, 1)', 180, 1, 1, TFPColor.New(0, $FFFF, $FFFF).ToStdHSVA, 1e-4);
  CheckHSV('blue is HSV (240, 1, 1)', 240, 1, 1, TFPColor.New(0, 0, $FFFF).ToStdHSVA, 1e-4);
  CheckHSV('magenta is HSV (300, 1, 1)', 300, 1, 1, TFPColor.New($FFFF, 0, $FFFF).ToStdHSVA, 1e-4);
  CheckHSV('dark red (0.5, 0, 0) is HSV (0, 1, 0.5)', 0, 1, 0.5, TStdRGBA.New(0.5, 0, 0).ToStdHSVA, 1e-4);
  CheckHSV('pink (1, 0.5, 0.5) is HSV (0, 0.5, 1)', 0, 0.5, 1, TStdRGBA.New(1, 0.5, 0.5).ToStdHSVA, 1e-4);
  CheckAlpha('RGB to HSV keeps alpha', 0.5, TStdRGBA.New(1, 0, 0, 0.5).ToStdHSVA.alpha);
end;


procedure TTestColorSpace.TestHSVOfGrays;

begin
  CheckHSV('black is HSV (0, 0, 0)', 0, 0, 0, TFPColor.New(0, 0, 0).ToStdHSVA, 1e-4);
  CheckHSV('white is HSV (0, 0, 1)', 0, 0, 1, TFPColor.New($FFFF, $FFFF, $FFFF).ToStdHSVA, 1e-4);
  CheckHSV('50% gray is HSV (0, 0, 0.5)', 0, 0, 0.5, TStdRGBA.New(0.5, 0.5, 0.5).ToStdHSVA, 1e-4);
end;


procedure TTestColorSpace.TestHSLToRGB;

begin
  CheckRGBA('HSL (0, 1, 0.5) is red', 1, 0, 0, 1, TStdHSLA.New(0, 1, 0.5).ToStdRGBA, 1e-4);
  CheckRGBA('HSL (60, 1, 0.5) is yellow', 1, 1, 0, 1, TStdHSLA.New(60, 1, 0.5).ToStdRGBA, 1e-4);
  CheckRGBA('HSL (120, 1, 0.5) is green', 0, 1, 0, 1, TStdHSLA.New(120, 1, 0.5).ToStdRGBA, 1e-4);
  CheckRGBA('HSL (180, 1, 0.5) is cyan', 0, 1, 1, 1, TStdHSLA.New(180, 1, 0.5).ToStdRGBA, 1e-4);
  CheckRGBA('HSL (240, 1, 0.5) is blue', 0, 0, 1, 1, TStdHSLA.New(240, 1, 0.5).ToStdRGBA, 1e-4);
  CheckRGBA('HSL (300, 1, 0.5) is magenta', 1, 0, 1, 1, TStdHSLA.New(300, 1, 0.5).ToStdRGBA, 1e-4);
  CheckRGBA('HSL (360, 1, 0.5) is red', 1, 0, 0, 1, TStdHSLA.New(360, 1, 0.5).ToStdRGBA, 1e-4);
  CheckRGBA('HSL (-120, 1, 0.5) is blue', 0, 0, 1, 1, TStdHSLA.New(-120, 1, 0.5).ToStdRGBA, 1e-4);
  CheckRGBA('HSL (0, 0, 0.5) is 50% gray', 0.5, 0.5, 0.5, 1, TStdHSLA.New(0, 0, 0.5).ToStdRGBA, 1e-4);
  CheckRGBA('HSL (0, 1, 0.25) is dark red', 0.5, 0, 0, 0.3, TStdHSLA.New(0, 1, 0.25, 0.3).ToStdRGBA, 1e-4);
  AssertColorsEqual('HSL (120, 1, 0.5) is the TFPColor green', TFPColor.New(0, $FFFF, 0), TStdHSLA.New(120, 1, 0.5).ToFPColor, 1);
end;


procedure TTestColorSpace.TestHSVToRGB;

begin
  CheckRGBA('HSV (0, 1, 1) is red', 1, 0, 0, 1, TStdHSVA.New(0, 1, 1).ToStdRGBA, 1e-4);
  CheckRGBA('HSV (60, 1, 1) is yellow', 1, 1, 0, 1, TStdHSVA.New(60, 1, 1).ToStdRGBA, 1e-4);
  CheckRGBA('HSV (120, 1, 1) is green', 0, 1, 0, 1, TStdHSVA.New(120, 1, 1).ToStdRGBA, 1e-4);
  CheckRGBA('HSV (180, 1, 1) is cyan', 0, 1, 1, 1, TStdHSVA.New(180, 1, 1).ToStdRGBA, 1e-4);
  CheckRGBA('HSV (240, 1, 1) is blue', 0, 0, 1, 1, TStdHSVA.New(240, 1, 1).ToStdRGBA, 1e-4);
  CheckRGBA('HSV (300, 1, 1) is magenta', 1, 0, 1, 1, TStdHSVA.New(300, 1, 1).ToStdRGBA, 1e-4);
  CheckRGBA('HSV (0, 0, 0.5) is 50% gray', 0.5, 0.5, 0.5, 1, TStdHSVA.New(0, 0, 0.5).ToStdRGBA, 1e-4);
  CheckRGBA('HSV (0, 0.5, 1) is pink', 1, 0.5, 0.5, 0.3, TStdHSVA.New(0, 0.5, 1, 0.3).ToStdRGBA, 1e-4);
  AssertColorsEqual('HSV (240, 1, 1) is the TFPColor blue', TFPColor.New(0, 0, $FFFF), TStdHSVA.New(240, 1, 1).ToFPColor, 1);
end;


procedure TTestColorSpace.TestHSLToHSV;

var
  lHSV: TStdHSVA;

begin
  CheckHSV('HSL (0, 1, 0.5) is HSV (0, 1, 1)', 0, 1, 1, TStdHSLA.New(0, 1, 0.5).ToStdHSVA, 1e-4);
  CheckHSV('HSL (120, 0.5, 0.25) is HSV (120, 0.6667, 0.375)', 120, 2 / 3, 0.375, TStdHSLA.New(120, 0.5, 0.25).ToStdHSVA, 1e-4);
  CheckHSV('HSL (200, 0, 0.4) is HSV (200, 0, 0.4)', 200, 0, 0.4, TStdHSLA.New(200, 0, 0.4).ToStdHSVA, 1e-4);
  lHSV := TStdHSLA.New(30, 1, 0.5, 0.25).ToStdHSVA;
  CheckAlpha('HSL to HSV keeps alpha', 0.25, lHSV.alpha);
end;


procedure TTestColorSpace.TestHSVToHSL;

var
  lHSL: TStdHSLA;

begin
  CheckHSL('HSV (0, 1, 1) is HSL (0, 1, 0.5)', 0, 1, 0.5, TStdHSVA.New(0, 1, 1).ToStdHSLA, 1e-4);
  CheckHSL('HSV (120, 0.6667, 0.375) is HSL (120, 0.5, 0.25)', 120, 0.5, 0.25, TStdHSVA.New(120, 2 / 3, 0.375).ToStdHSLA, 1e-4);
  CheckHSL('HSV (0, 0, 1) is HSL (0, 0, 1)', 0, 0, 1, TStdHSVA.New(0, 0, 1).ToStdHSLA, 1e-4);
  lHSL := TStdHSVA.New(30, 1, 1, 0.25).ToStdHSLA;
  CheckAlpha('HSV to HSL keeps alpha', 0.25, lHSL.alpha);
end;


procedure TTestColorSpace.TestCMYKOfPrimaries;

begin
  CheckCMYK('red is CMYK (0, 1, 1, 0)', 0, 1, 1, 0, TFPColor.New($FFFF, 0, 0).ToStdCMYK, 1e-4);
  CheckCMYK('green is CMYK (1, 0, 1, 0)', 1, 0, 1, 0, TFPColor.New(0, $FFFF, 0).ToStdCMYK, 1e-4);
  CheckCMYK('blue is CMYK (1, 1, 0, 0)', 1, 1, 0, 0, TFPColor.New(0, 0, $FFFF).ToStdCMYK, 1e-4);
  CheckCMYK('cyan is CMYK (1, 0, 0, 0)', 1, 0, 0, 0, TFPColor.New(0, $FFFF, $FFFF).ToStdCMYK, 1e-4);
  CheckCMYK('magenta is CMYK (0, 1, 0, 0)', 0, 1, 0, 0, TFPColor.New($FFFF, 0, $FFFF).ToStdCMYK, 1e-4);
  CheckCMYK('yellow is CMYK (0, 0, 1, 0)', 0, 0, 1, 0, TFPColor.New($FFFF, $FFFF, 0).ToStdCMYK, 1e-4);
  CheckCMYK('white is CMYK (0, 0, 0, 0)', 0, 0, 0, 0, TFPColor.New($FFFF, $FFFF, $FFFF).ToStdCMYK, 1e-4);
  CheckCMYK('black is CMYK (0, 0, 0, 1)', 0, 0, 0, 1, TFPColor.New(0, 0, 0).ToStdCMYK, 1e-4);
  CheckCMYK('50% gray is CMYK (0, 0, 0, 0.5)', 0, 0, 0, 0.5, TStdRGBA.New(0.5, 0.5, 0.5).ToStdCMYK, 1e-4);
  CheckCMYK('dark red (0.5, 0, 0) is CMYK (0, 1, 1, 0.5)', 0, 1, 1, 0.5, TStdRGBA.New(0.5, 0, 0).ToStdCMYK, 1e-4);
end;


procedure TTestColorSpace.TestCMYKToRGB;

begin
  CheckRGBA('CMYK (0, 1, 1, 0) is red', 1, 0, 0, 1, TStdCMYK.New(0, 1, 1, 0).ToStdRGBA, 1e-4);
  CheckRGBA('CMYK (1, 1, 0, 0) is blue', 0, 0, 1, 1, TStdCMYK.New(1, 1, 0, 0).ToStdRGBA, 1e-4);
  CheckRGBA('CMYK (0, 0, 0, 1) is black', 0, 0, 0, 1, TStdCMYK.New(0, 0, 0, 1).ToStdRGBA, 1e-4);
  CheckRGBA('CMYK (0, 0, 0, 0.5) is 50% gray with the alpha given', 0.5, 0.5, 0.5, 0.4,
    TStdCMYK.New(0, 0, 0, 0.5).ToStdRGBA(0.4), 1e-4);
  AssertColorsEqual('CMYK (0, 1, 1, 0) is the TFPColor red with the alpha given', TFPColor.New($FFFF, 0, 0, $1234),
    TStdCMYK.New(0, 1, 1, 0).ToFPColor($1234), 1);
end;


procedure TTestColorSpace.TestCMYKToExpandedPixelKeepsAlpha;

var
  lPixel: TExpandedPixel;

begin
  lPixel := TStdCMYK.New(0, 0, 0, 0).ToExpandedPixel($1234);
  AssertEquals('CMYK white as expanded pixel is white', $FFFF, lPixel.red);
  AssertEquals('CMYK to expanded pixel with alpha $1234 has alpha $1234', $1234, lPixel.alpha);
end;


procedure TTestColorSpace.TestYCbCr601;

begin
  CheckYCbCrStandard('ITU-R BT.601', YCbCr_601, 0.299, 0.114);
end;


procedure TTestColorSpace.TestYCbCr709;

begin
  CheckYCbCrStandard('ITU-R BT.709', YCbCr_709, 0.2126, 0.0722);
end;


procedure TTestColorSpace.TestYCbCr2020;

begin
  CheckYCbCrStandard('ITU-R BT.2020', YCbCr_2020, 0.2627, 0.0593);
end;


procedure TTestColorSpace.TestYCbCrJPEG;

begin
  CheckYCbCrStandard('JPEG (JFIF full range, scaled to 0..1)', YCbCr_JPG, 0.299, 0.114);
end;


procedure TTestColorSpace.TestYCbCrToRGBIsOpaque;

var
  lColor: TStdRGBA;

begin
  lColor := TYCbCr.New(1, 0.5, 0.5).ToStdRGBA(YCbCr_601, 0.5);
  AssertEquals('YCbCr (1, 0.5, 0.5) is white: red', 1, lColor.red, 1e-4);
  AssertEquals('YCbCr (1, 0.5, 0.5) is white: green', 1, lColor.green, 1e-4);
  AssertEquals('YCbCr (1, 0.5, 0.5) is white: blue', 1, lColor.blue, 1e-4);
  CheckAlpha('a colour made from YCbCr, which has no alpha, is opaque', 1, lColor.alpha);
  lColor := TYCbCr.New(1, 0, 0).ToStdRGBA(0.299, 0.587, 0.114, 0);
  CheckAlpha('a colour made from YCbCr with luma weights is opaque', 1, lColor.alpha);
end;


procedure TTestColorSpace.TestYCbCrLumaOverloadsAreInverse;

var
  lColor: TStdRGBA;

begin
  lColor := TStdRGBA.New(0.8, 0.3, 0.1).ToYCbCr(0.299, 0.587, 0.114).ToStdRGBA(0.299, 0.587, 0.114);
  AssertEquals('ToYCbCr and ToStdRGBA with the same luma weights and default arguments give red back', 0.8, lColor.red, 1e-4);
  AssertEquals('ToYCbCr and ToStdRGBA with the same luma weights and default arguments give green back', 0.3, lColor.green, 1e-4);
  AssertEquals('ToYCbCr and ToStdRGBA with the same luma weights and default arguments give blue back', 0.1, lColor.blue, 1e-4);
end;


procedure TTestColorSpace.TestAdobeRGBWhite;

var
  lXYZ: TXYZA;

begin
  lXYZ := TAdobeRGBA.New(255, 255, 255).ToXYZA(WhiteD65);
  CheckXYZ('Adobe RGB (1998) white is the D65 white point (0.9505, 1.0000, 1.0891)', 0.9505, 1.0, 1.0891, lXYZ, 0.001);
  CheckAlpha('Adobe RGB to XYZ keeps alpha', 1.0, lXYZ.alpha);
end;


procedure TTestColorSpace.TestAdobeRGBRed;

begin
  CheckXYZ('Adobe RGB (1998) red is XYZ (0.5767, 0.2974, 0.0270)', 0.5767, 0.2974, 0.0270,
    TAdobeRGBA.New(255, 0, 0).ToXYZA(WhiteD65), 0.001);
end;


procedure TTestColorSpace.TestAdobeRGBMidGray;

begin
  AssertEquals('Adobe RGB (1998) gray 128 has Y = (128/255)^(563/256) = 0.2197',
    0.2197, TAdobeRGBA.New(128, 128, 128).ToXYZA(WhiteD65).Y, 0.002);
end;


procedure TTestColorSpace.TestXYZToAdobeRGBOfWhite;

var
  lAdobe: TAdobeRGBA;

begin
  lAdobe := TXYZA.New(cD65X, 1, cD65Z).ToAdobeRGBA(WhiteD65);
  AssertEquals('the D65 white point is Adobe RGB red 255', 255, lAdobe.red);
  AssertEquals('the D65 white point is Adobe RGB green 255', 255, lAdobe.green);
  AssertEquals('the D65 white point is Adobe RGB blue 255', 255, lAdobe.blue);
  AssertEquals('XYZ to Adobe RGB keeps alpha', 255, lAdobe.alpha);
end;


procedure TTestColorSpace.TestChromaticAdaptationOfWhite;

var
  lX, lY, lZ: Single;
  lXYZ: TXYZA;

begin
  lX := cD65X;
  lY := 1;
  lZ := cD65Z;
  FPChromaticAdaptXYZ(lX, lY, lZ, WhiteD65, WhiteD50);
  AssertEquals('the D65 white adapted to D50 is the D50 white X 0.9642', cD50X, lX, 0.001);
  AssertEquals('the D65 white adapted to D50 is the D50 white Y 1.0', 1.0, lY, 0.001);
  AssertEquals('the D65 white adapted to D50 is the D50 white Z 0.8252', cD50Z, lZ, 0.001);
  lXYZ := TXYZA.New(cD50X, 1, cD50Z, 0.5);
  lXYZ.ChromaticAdapt(WhiteD50, WhiteD65);
  CheckXYZ('the D50 white adapted to D65 is the D65 white', cD65X, 1, cD65Z, lXYZ, 0.001);
  CheckAlpha('chromatic adaptation keeps alpha', 0.5, lXYZ.alpha);
end;


procedure TTestColorSpace.TestChromaticAdaptationOfRed;

var
  lXYZ: TXYZA;

begin
  lXYZ := TXYZA.New(0.4124564, 0.2126729, 0.0193339);
  lXYZ.ChromaticAdapt(WhiteD65, WhiteD50);
  CheckXYZ('sRGB red adapted from D65 to D50 by Bradford is (0.4361, 0.2225, 0.0139) (Lindbloom)',
    0.4360747, 0.2225045, 0.0139322, lXYZ, 5e-4);
  CheckXYZ('linear sRGB red relative to D50 is the Bradford-adapted red', 0.4360747, 0.2225045, 0.0139322,
    TLinearRGBA.New(1, 0, 0).ToXYZA(WhiteD50), 5e-4);
end;


procedure TTestColorSpace.TestReferenceWhites;

var
  lWhite: PXYZReferenceWhite;

begin
  lWhite := FPReferenceWhiteGet(2, 'D65');
  AssertTrue('FPReferenceWhiteGet finds the 2 degree D65 white', lWhite <> nil);
  AssertEquals('D65 white X is 0.95047', cD65X, lWhite^.X, 1e-5);
  AssertEquals('D65 white Y is 1', 1.0, lWhite^.Y, 1e-5);
  AssertEquals('D65 white Z is 1.08883', cD65Z, lWhite^.Z, 1e-5);
  AssertEquals('D50 white X is 0.96422', cD50X, WhiteD50.X, 1e-5);
  AssertEquals('D50 white Z is 0.82521', cD50Z, WhiteD50.Z, 1e-5);
  AssertTrue('FPReferenceWhiteGet returns nil for an unknown illuminant', FPReferenceWhiteGet(2, 'XYZ') = nil);
  AssertRaises('adding a reference white that exists raises', FPImageException, @AddDuplicateWhite);
  AssertTrue('FPReferenceWhite2D65 points to the D65 entry of FPReferenceWhiteArray', FPReferenceWhite2D65 = lWhite);
  AssertTrue('FPReferenceWhite2D50 points to the D50 entry of FPReferenceWhiteArray',
    FPReferenceWhite2D50 = FPReferenceWhiteGet(2, 'D50'));
  AssertTrue('FPReferenceWhite2E points to the E entry of FPReferenceWhiteArray',
    FPReferenceWhite2E = FPReferenceWhiteGet(2, 'E'));
  AssertTrue('FPReferenceWhite, D50 by default, points into FPReferenceWhiteArray',
    FPReferenceWhite = FPReferenceWhiteGet(2, 'D50'));
end;


procedure TTestColorSpace.TestGammaHelpersAreInverse;

var
  I: Integer;
  lCompressed: Word;

begin
  for I := 0 to 255 do
    begin
    lCompressed := FPGammaCompression(FPGammaExpansion(Word(I * 257)));
    AssertEquals(Format('gamma compression undoes gamma expansion of the 8-bit level %d', [I]), I * 257, lCompressed);
    end;
  AssertEquals('gamma expansion of 0 is 0', 0, FPGammaExpansion(Word(0)));
  AssertEquals('gamma expansion of 65535 is 65535', 65535, FPGammaExpansion(Word(65535)));
  AssertEquals('gamma compression of 0 is 0', 0, FPGammaCompression(0));
  AssertEquals('gamma compression of 65535 is 65535', 65535, FPGammaCompression(65535));
  AssertEquals('gamma expansion of the single 1.0 is 65535', 65535, FPGammaExpansion(Single(1.0)));
  AssertEquals('gamma expansion of the single 0.0 is 0', 0, FPGammaExpansion(Single(0.0)));
end;


procedure TTestColorSpace.TestGammaHelpersAreMonotonic;

var
  I: Integer;
  lPrevExp, lPrevComp, lExp, lComp: Word;

begin
  lPrevExp := 0;
  lPrevComp := 0;
  for I := 0 to 65535 do
    begin
    lExp := FPGammaExpansion(Word(I));
    lComp := FPGammaCompression(Word(I));
    if lExp < lPrevExp then
      Fail(Format('gamma expansion does not decrease: %d gives %d, below %d', [I, lExp, lPrevExp]));
    if lComp < lPrevComp then
      Fail(Format('gamma compression does not decrease: %d gives %d, below %d', [I, lComp, lPrevComp]));
    lPrevExp := lExp;
    lPrevComp := lComp;
    end;
end;


procedure TTestColorSpace.TestGammaNoneIsIdentity;

var
  lGamma: Single;
  I: Integer;

begin
  lGamma := FPGammaGet;
  try
    FPGammaSet(2.2);
    AssertEquals('FPGammaGet returns the gamma set', 2.2, FPGammaGet, 1e-6);
    FPGammaNone;
    AssertEquals('FPGammaNone sets the gamma to 1', 1.0, FPGammaGet, 1e-6);
    for I := 0 to 65535 do
      if Abs(FPGammaExpansion(Word(I)) - I) > 1 then
        Fail(Format('without gamma the expansion of %d is %d, not itself', [I, FPGammaExpansion(Word(I))]));
  finally
    FPGammaSet(lGamma);
  end;
end;


procedure TTestColorSpace.TestExpandedPixelToStdRGBA;

begin
  CheckRGBA('expanded white is sRGB (1, 1, 1, 1)', 1, 1, 1, 1, TExpandedPixel.New($FFFF, $FFFF, $FFFF).ToStdRGBA, 1e-4);
  CheckRGBA('expanded (65535, 0, 0) is sRGB red', 1, 0, 0, 0.5, TExpandedPixel.New($FFFF, 0, 0, $8000).ToStdRGBA, 1e-4);
end;


procedure TTestColorSpace.TestExpandedPixelIntensityAndLightness;

var
  lPixel: TExpandedPixel;

begin
  lPixel := TExpandedPixel.New(100, 2000, 50);
  AssertEquals('the intensity is the largest component', 2000, lPixel.GetIntensity);
  AssertEquals('the lightness of white is 65535', 65535, TExpandedPixel.New($FFFF, $FFFF, $FFFF).GetLightness);
  AssertEquals('the lightness of black is 0', 0, TExpandedPixel.New(0, 0, 0).GetLightness);
  lPixel := TExpandedPixel.New(1000, 500, 0).SetIntensity(2000);
  AssertEquals('SetIntensity scales red to the intensity', 2000, lPixel.red);
  AssertEquals('SetIntensity scales green in proportion', 1000, lPixel.green);
  AssertEquals('ColorImportance of a gray is 0', 0, TExpandedPixel.New(300, 300, 300).ColorImportance);
end;


procedure TTestColorSpace.TestByteMask;

begin
  AssertEquals('the byte mask of white is 255', 255, TExpandedPixel.New($FFFF, $FFFF, $FFFF).ToByteMask.gray);
  AssertEquals('the byte mask of black is 0', 0, TExpandedPixel.New(0, 0, 0).ToByteMask.gray);
  AssertEquals('the byte mask of red is its luma 0.299 * 255', 76, TExpandedPixel.New($FFFF, 0, 0).ToByteMask.gray, 1);
  AssertEquals('a byte mask of 255 is expanded white', $FFFF, TByteMask.New(255).ToExpandedPixel.red);
end;


procedure TTestColorSpace.TestHSLAPixelOfPrimaries;

var
  lPixel: THSLAPixel;

begin
  lPixel := TExpandedPixel.New($FFFF, 0, 0).ToHSLAPixel;
  AssertEquals('red has hue 0', 0, lPixel.hue);
  AssertEquals('red has full saturation', 65535, lPixel.saturation, 1);
  AssertEquals('red has half lightness', 32768, lPixel.lightness, 1);
  lPixel := TExpandedPixel.New(0, $FFFF, 0).ToHSLAPixel;
  AssertEquals('green has hue 65536/3', 21845, lPixel.hue, 1);
  lPixel := TExpandedPixel.New(0, 0, $FFFF).ToHSLAPixel;
  AssertEquals('blue has hue 2*65536/3', 43690, lPixel.hue, 1);
  lPixel := TExpandedPixel.New(1000, 1000, 1000, 1234).ToHSLAPixel;
  AssertEquals('a gray has saturation 0', 0, lPixel.saturation);
  AssertEquals('a gray has its level as lightness', 1000, lPixel.lightness);
  AssertEquals('expanded to HSLA keeps alpha', 1234, lPixel.alpha);
end;


procedure TTestColorSpace.TestWordXYZAOfWhite;

var
  lXYZ: TWordXYZA;

begin
  lXYZ := TExpandedPixel.New($FFFF, $FFFF, $FFFF, $4321).ToWordXYZA(WhiteD65);
  AssertEquals('expanded white is word XYZ X 0.9505 * 50000', 47524, lXYZ.X, 2);
  AssertEquals('expanded white is word XYZ Y 50000', 50000, lXYZ.Y, 2);
  AssertEquals('expanded white is word XYZ Z 1.0890 * 50000', 54442, lXYZ.Z, 3);
  AssertEquals('expanded to word XYZ keeps alpha', $4321, lXYZ.alpha);
end;


procedure TTestColorSpace.TestSpectrumOfPerfectReflector;

var
  lOld: PXYZReferenceWhite;
  lXYZ: TXYZA;

begin
  lOld := FPReferenceWhite;
  try
    FPReferenceWhite := FPReferenceWhiteGet(2, 'D65');
    lXYZ := TXYZA.New(0, 0, 0);
    lXYZ.FromSpectrumRangeReflect(1, 360, 830, 1);
    CheckXYZ('a perfect reflector over the whole spectrum under D65 is the D65 white', cD65X, 1, cD65Z, lXYZ, 0.002);
  finally
    FPReferenceWhite := lOld;
  end;
end;


procedure TTestColorSpace.TestFPColorHelperNew;

var
  lColor: TFPColor;

begin
  lColor := TFPColor.New(1, 2, 3);
  AssertEquals('New sets red', 1, lColor.Red);
  AssertEquals('New sets green', 2, lColor.Green);
  AssertEquals('New sets blue', 3, lColor.Blue);
  AssertEquals('New without alpha is opaque', $FFFF, lColor.Alpha);
  AssertEquals('New with alpha sets alpha', 4, TFPColor.New(1, 2, 3, 4).Alpha);
  CheckRGBA('ToStdRGBA scales to 0..1', 1, 0, 0.5, 0.5, TFPColor.New($FFFF, 0, $8000, $8000).ToStdRGBA, 1e-4);
end;


procedure TTestColorSpace.TestRoundTripThroughStdRGBA;

var
  I: Integer;
  lColor: TFPColor;

begin
  for I := 0 to GridCount(True) - 1 do
    begin
    lColor := GridColor(I, True);
    AssertColorsEqual('TFPColor to TStdRGBA and back', lColor, lColor.ToStdRGBA.ToFPColor, 0);
    end;
end;


procedure TTestColorSpace.TestRoundTripThroughExpandedPixel;

var
  I: Integer;
  lColor: TFPColor;

begin
  for I := 0 to GridCount(True) - 1 do
    begin
    lColor := GridColor(I, True);
    AssertColorsEqual('8-bit TFPColor to gamma expanded pixel and back', lColor, lColor.ToExpanded.ToFPColor, 0);
    AssertColorsEqual('TFPColor to expanded pixel without gamma and back', lColor, lColor.ToExpanded(False).ToFPColor(False), 0);
    end;
end;


procedure TTestColorSpace.TestRoundTripThroughStdHSLA;

var
  I: Integer;
  lColor: TFPColor;

begin
  for I := 0 to GridCount(True) - 1 do
    begin
    lColor := GridColor(I, True);
    AssertColorsEqual('TFPColor to HSL and back (tolerance 2 of 65535)', lColor, lColor.ToStdHSLA.ToFPColor, 2);
    end;
end;


procedure TTestColorSpace.TestRoundTripThroughStdHSVA;

var
  I: Integer;
  lColor: TFPColor;

begin
  for I := 0 to GridCount(True) - 1 do
    begin
    lColor := GridColor(I, True);
    AssertColorsEqual('TFPColor to HSV and back (tolerance 2 of 65535)', lColor, lColor.ToStdHSVA.ToFPColor, 2);
    end;
end;


procedure TTestColorSpace.TestRoundTripThroughStdCMYK;

var
  I: Integer;
  lColor: TFPColor;

begin
  for I := 0 to GridCount(True) - 1 do
    begin
    lColor := GridColor(I, True);
    AssertColorsEqual('TFPColor to CMYK and back with its alpha (tolerance 2 of 65535)', lColor,
      lColor.ToStdCMYK.ToFPColor(lColor.Alpha), 2);
    end;
end;


procedure TTestColorSpace.TestRoundTripThroughLinearRGBA;

var
  I: Integer;
  lColor: TFPColor;

begin
  for I := 0 to GridCount(True) - 1 do
    begin
    lColor := GridColor(I, True);
    AssertColorsEqual('TFPColor to linear RGB and back (tolerance 65 of 65535, 0.001)', lColor,
      lColor.ToStdRGBA.ToLinearRGBA.ToStdRGBA.ToFPColor, 65);
    end;
end;


procedure TTestColorSpace.TestRoundTripThroughLab;

var
  I: Integer;
  lColor: TFPColor;
  lLab: TLabA;

begin
  for I := 0 to GridCount(True) - 1 do
    begin
    lColor := GridColor(I, True);
    lLab := lColor.ToStdRGBA.ToLinearRGBA.ToXYZA(WhiteD65).ToLabA(WhiteD65);
    AssertColorsEqual('TFPColor to Lab (D65) and back (tolerance 131 of 65535, 0.002)', lColor,
      lLab.ToXYZA(WhiteD65).ToLinearRGBA(WhiteD65).ToStdRGBA.ToFPColor, 131);
    end;
end;


procedure TTestColorSpace.TestRoundTripThroughLCh;

var
  I: Integer;
  lLab, lBack: TLabA;

begin
  for I := 0 to GridCount(True) - 1 do
    begin
    lLab := TLabA.New(10 + (I mod 9) * 10, (I mod 7) * 20 - 60, (I mod 11) * 15 - 75, cAlphas[I mod 3] / 255);
    lBack := lLab.ToLChA.ToLabA;
    CheckLab(Format('Lab (%g, %g, %g) to LCh and back', [lLab.L, lLab.a, lLab.b]), lLab.L, lLab.a, lLab.b, lBack, 1e-3);
    CheckAlpha('Lab to LCh and back keeps alpha', lLab.alpha, lBack.alpha);
    end;
end;


procedure TTestColorSpace.TestRoundTripThroughYCbCr;

const
  cStds: array[0..3] of TYCbCrSTD = (YCbCr_601, YCbCr_709, YCbCr_2020, YCbCr_JPG);

var
  I, J: Integer;
  lColor, lBack: TStdRGBA;

begin
  for J := 0 to High(cStds) do
    for I := 0 to GridCount(False) - 1 do
      begin
      lColor := GridColor(I, False).ToStdRGBA;
      lBack := lColor.ToYCbCr(cStds[J], 0.5).ToStdRGBA(cStds[J], 0.5);
      AssertEquals(Format('RGB to YCbCr standard %d and back: red', [J]), lColor.red, lBack.red, 1e-4);
      AssertEquals(Format('RGB to YCbCr standard %d and back: green', [J]), lColor.green, lBack.green, 1e-4);
      AssertEquals(Format('RGB to YCbCr standard %d and back: blue', [J]), lColor.blue, lBack.blue, 1e-4);
      end;
end;


procedure TTestColorSpace.TestRoundTripThroughHSLAPixel;

var
  I: Integer;
  lColor: TFPColor;
  lExpanded, lBack: TExpandedPixel;

begin
  for I := 0 to GridCount(True) - 1 do
    begin
    lColor := GridColor(I, True);
    AssertColorsEqual('TFPColor to THSLAPixel without gamma and back (tolerance 64 of 65535)', lColor,
      lColor.ToHSLAPixel(False).ToFPColor(False), 64);
    lExpanded := lColor.ToExpanded;
    lBack := lColor.ToHSLAPixel.ToExpanded;
    AssertColorsEqual('TFPColor to THSLAPixel with gamma and back to expanded (tolerance 64 of 65535)',
      TFPColor.New(lExpanded.red, lExpanded.green, lExpanded.blue, lExpanded.alpha),
      TFPColor.New(lBack.red, lBack.green, lBack.blue, lBack.alpha), 64);
    end;
end;


procedure TTestColorSpace.TestRoundTripThroughGSBAPixel;

var
  I: Integer;
  lColor: TFPColor;

begin
  for I := 0 to GridCount(True) - 1 do
    begin
    lColor := GridColor(I, True);
    AssertColorsEqual('TFPColor to TGSBAPixel without gamma and back (tolerance 256 of 65535)', lColor,
      lColor.ToGSBAPixel(False).ToFPColor(False), 256);
    end;
end;


procedure TTestColorSpace.TestRoundTripThroughWordXYZA;

var
  I: Integer;
  lPixel, lBack: TExpandedPixel;
  lColor: TFPColor;

begin
  for I := 0 to GridCount(True) - 1 do
    begin
    lColor := GridColor(I, True);
    lPixel := TExpandedPixel.New(lColor.Red, lColor.Green, lColor.Blue, lColor.Alpha);
    lBack := lPixel.ToWordXYZA(WhiteD65).ToExpandedPixel(WhiteD65);
    AssertColorsEqual('expanded pixel to word XYZ (D65) and back (tolerance 16 of 65535)', lColor,
      TFPColor.New(lBack.red, lBack.green, lBack.blue, lBack.alpha), 16);
    lBack := lPixel.ToWordXYZA(WhiteD50).ToExpandedPixel(WhiteD50);
    AssertColorsEqual('expanded pixel to word XYZ (D50) and back (tolerance 16 of 65535)', lColor,
      TFPColor.New(lBack.red, lBack.green, lBack.blue, lBack.alpha), 16);
    end;
end;


initialization
  RegisterTest('colorspace', TTestColorSpace);
end.
