{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by Michael Van Canneyt

    Colour assertions on a rendered surface, for the pixel test suites.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit svgpixels;

{$mode objfpc}{$H+}
{$IFDEF UNICODERTL}
{$modeswitch unicodestrings}
{$ENDIF}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses System.SysUtils, System.Math, FpImage, FpcUnit.Test, fpsvg.types;
{$ELSE FPC_DOTTEDUNITS}
uses sysutils, math, fpimage, fpcunit, fpsvg.types;
{$ENDIF FPC_DOTTEDUNITS}

// The colour of one pixel of a surface.
function SVGPixelAt(aImage: TFPCustomImage; aX, aY: Integer): TSVGColor;
// A colour as four eight bit channels, for use in a failure message.
function SVGPixelText(const aColor: TSVGColor): String;
// Fails unless the pixel holds the colour, to the nearest eight bit step.
procedure AssertPixel(aTest: TAssert; const aMessage: String;
  aImage: TFPCustomImage; aX, aY: Integer; const aExpected: TSVGColor);
// Fails unless every channel of the pixel is within aTolerance eight bit
// steps of the given colour.
procedure AssertPixelNear(aTest: TAssert; const aMessage: String;
  aImage: TFPCustomImage; aX, aY: Integer; const aExpected: TSVGColor;
  aTolerance: Integer);
// Fails unless nothing was painted on the pixel.
procedure AssertPixelClear(aTest: TAssert; const aMessage: String;
  aImage: TFPCustomImage; aX, aY: Integer);

implementation

function SVGPixelAt(aImage: TFPCustomImage; aX, aY: Integer): TSVGColor;

begin
  Result := TSVGColor(aImage.Colors[aX, aY]);
end;


function SVGPixelText(const aColor: TSVGColor): String;

begin
  Result := Format('rgba(%d,%d,%d,%d)', [aColor.Red div 257,
    aColor.Green div 257, aColor.Blue div 257, aColor.Alpha div 257]);
end;


procedure AssertPixel(aTest: TAssert; const aMessage: String;
  aImage: TFPCustomImage; aX, aY: Integer; const aExpected: TSVGColor);

begin
  AssertPixelNear(aTest, aMessage, aImage, aX, aY, aExpected, 0);
end;


procedure AssertPixelNear(aTest: TAssert; const aMessage: String;
  aImage: TFPCustomImage; aX, aY: Integer; const aExpected: TSVGColor;
  aTolerance: Integer);

var
  lActual: TSVGColor;
  lWorst: Integer;

  function Distance(aFirst, aSecond: Word): Integer;
  begin
    Result := Abs(Integer(aFirst) div 257 - Integer(aSecond) div 257);
  end;

begin
  lActual := SVGPixelAt(aImage, aX, aY);
  lWorst := Distance(lActual.Red, aExpected.Red);
  lWorst := Max(lWorst, Distance(lActual.Green, aExpected.Green));
  lWorst := Max(lWorst, Distance(lActual.Blue, aExpected.Blue));
  lWorst := Max(lWorst, Distance(lActual.Alpha, aExpected.Alpha));
  if lWorst <= aTolerance then
    Exit;
  // Both colours go into the message, so a failure reports the colour that
  // was painted.
  aTest.AssertEquals(Format('%s at (%d,%d)', [aMessage, aX, aY]),
    SVGPixelText(aExpected), SVGPixelText(lActual));
end;


procedure AssertPixelClear(aTest: TAssert; const aMessage: String;
  aImage: TFPCustomImage; aX, aY: Integer);

begin
  aTest.AssertEquals(Format('%s at (%d,%d)', [aMessage, aX, aY]), 0,
    SVGPixelAt(aImage, aX, aY).Alpha);
end;


end.
