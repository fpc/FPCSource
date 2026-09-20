{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by Michael Van Canneyt

    Pixel comparison of a rendered surface against a reference image.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit svgcompare;

{$mode objfpc}{$H+}
{$IFDEF UNICODERTL}
{$modeswitch unicodestrings}
{$ENDIF}
{$modeswitch advancedrecords}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses System.SysUtils, FpImage, fpsvg.types;
{$ELSE FPC_DOTTEDUNITS}
uses sysutils, fpimage, fpsvg.types;
{$ENDIF FPC_DOTTEDUNITS}

type
  ESVGCompare = class(ESVGError);

  TSVGCompareResult = (crMatch, crDiffers, crSizeMismatch);

  { The result of comparing two images of the same size. }
  TSVGComparison = record
    Outcome       : TSVGCompareResult;
    Width, Height : Integer;
    Differing     : Integer;
    MaxDifference : Integer;
    // Number of pixels compared.
    function Pixels: Integer;
    // The share of pixels that differ, as a percentage.
    function Percentage: Double;
    // True when no more than aThreshold percent of the pixels differ.
    function Passes(aThreshold: Double): Boolean;
    // The outcome, the count and the percentage, on one line.
    function ToString: String;
  end;

// The colour a rendered surface is composited onto before comparing.
function SVGCompositeOver(const aColor, aBackground: TSVGColor): TSVGColor;
// Compares two images channel by channel. Both are composited onto
// aBackground first. A pixel differs when one of its channels is further
// apart than aTolerance, counted in eight bit steps. When aDiff is not nil
// it receives a map of the differences.
function SVGCompareImages(aActual, aExpected: TFPCustomImage;
  aTolerance: Integer; const aBackground: TSVGColor;
  aDiff: TFPCustomImage): TSVGComparison;
// Draws how far apart two images are, one grey per pixel: white where
// they agree, black where they differ most, and the steps between spread
// over the whole range so that a small difference is still visible.
// Both are composited onto aBackground first.
// Returns the largest distance found, which is the scale the greys are
// drawn to, or zero when the two agree everywhere.
function SVGDistanceImage(aActual, aExpected: TFPCustomImage;
  const aBackground: TSVGColor; aDiff: TFPCustomImage): Integer;
// Differing pixels inside one rectangle of the images, counted the way
// SVGCompareImages counts them over the whole. A rectangle reaching past
// an edge is trimmed to it, and one of no extent counts nothing.
function SVGCountDiffering(aActual, aExpected: TFPCustomImage;
  aTolerance: Integer; const aBackground: TSVGColor;
  aLeft, aTop, aRight, aBottom: Integer): Integer;

implementation

function TSVGComparison.Pixels: Integer;

begin
  Result := Width * Height;
end;


function TSVGComparison.Percentage: Double;

begin
  if Pixels <= 0 then
    Result := 0
  else
    Result := Differing * 100 / Pixels;
end;


function TSVGComparison.Passes(aThreshold: Double): Boolean;

begin
  Result := (Outcome = crMatch)
         or ((Outcome = crDiffers) and (Percentage <= aThreshold));
end;


function TSVGComparison.ToString: String;

begin
  if Outcome = crSizeMismatch then
    Exit('size mismatch');
  Result := Format('%d of %d pixels differ (%s%%), worst channel %d',
    [Differing, Pixels, SVGFormatFloat(Percentage), MaxDifference]);
end;


function SVGCompositeOver(const aColor, aBackground: TSVGColor): TSVGColor;

var
  lAlpha: Cardinal;

begin
  lAlpha := aColor.Alpha;
  if lAlpha = 65535 then
    Exit(aColor);
  Result.Red := (aColor.Red * lAlpha
    + aBackground.Red * (65535 - lAlpha)) div 65535;
  Result.Green := (aColor.Green * lAlpha
    + aBackground.Green * (65535 - lAlpha)) div 65535;
  Result.Blue := (aColor.Blue * lAlpha
    + aBackground.Blue * (65535 - lAlpha)) div 65535;
  Result.Alpha := 65535;
end;


// The largest distance between two colours on any of the three channels,
// counted in eight bit steps.
function ChannelDistance(const aFirst, aSecond: TSVGColor): Integer;

var
  lDelta: Integer;

begin
  Result := Abs(Integer(aFirst.Red) - Integer(aSecond.Red)) div 257;
  lDelta := Abs(Integer(aFirst.Green) - Integer(aSecond.Green)) div 257;
  if lDelta > Result then
    Result := lDelta;
  lDelta := Abs(Integer(aFirst.Blue) - Integer(aSecond.Blue)) div 257;
  if lDelta > Result then
    Result := lDelta;
end;


function SVGDistanceImage(aActual, aExpected: TFPCustomImage;
  const aBackground: TSVGColor; aDiff: TFPCustomImage): Integer;

var
  X, Y, lDistance, lStep: Integer;
  lActual, lExpected: TSVGColor;
  lGrey: TSVGColor;

begin
  Result := 0;
  if (aActual = nil) or (aExpected = nil) or (aDiff = nil) then
    raise ESVGCompare.Create('Nothing to compare');
  if (aActual.Width <> aExpected.Width)
     or (aActual.Height <> aExpected.Height) then
    Exit;
  aDiff.SetSize(aActual.Width, aActual.Height);
  for Y := 0 to aActual.Height - 1 do
    for X := 0 to aActual.Width - 1 do
      begin
      lActual := SVGCompositeOver(TSVGColor(aActual.Colors[X, Y]),
        aBackground);
      lExpected := SVGCompositeOver(TSVGColor(aExpected.Colors[X, Y]),
        aBackground);
      lDistance := ChannelDistance(lActual, lExpected);
      if lDistance > Result then
        Result := lDistance;
      end;
  for Y := 0 to aActual.Height - 1 do
    for X := 0 to aActual.Width - 1 do
      begin
      lActual := SVGCompositeOver(TSVGColor(aActual.Colors[X, Y]),
        aBackground);
      lExpected := SVGCompositeOver(TSVGColor(aExpected.Colors[X, Y]),
        aBackground);
      lDistance := ChannelDistance(lActual, lExpected);
      lStep := 255;
      if Result > 0 then
        lStep := 255 - lDistance * 255 div Result;
      lGrey := TSVGColor.FromBytes(lStep, lStep, lStep, 255);
      aDiff.Colors[X, Y] := TFPColor(lGrey);
      end;
end;


function SVGCountDiffering(aActual, aExpected: TFPCustomImage;
  aTolerance: Integer; const aBackground: TSVGColor;
  aLeft, aTop, aRight, aBottom: Integer): Integer;

var
  X, Y: Integer;
  lActual, lExpected: TSVGColor;

begin
  Result := 0;
  if (aActual = nil) or (aExpected = nil) then
    raise ESVGCompare.Create('Nothing to compare');
  if (aActual.Width <> aExpected.Width)
     or (aActual.Height <> aExpected.Height) then
    Exit;
  if aLeft < 0 then
    aLeft := 0;
  if aTop < 0 then
    aTop := 0;
  if aRight > aActual.Width then
    aRight := aActual.Width;
  if aBottom > aActual.Height then
    aBottom := aActual.Height;
  for Y := aTop to aBottom - 1 do
    for X := aLeft to aRight - 1 do
      begin
      lActual := SVGCompositeOver(TSVGColor(aActual.Colors[X, Y]),
        aBackground);
      lExpected := SVGCompositeOver(TSVGColor(aExpected.Colors[X, Y]),
        aBackground);
      if ChannelDistance(lActual, lExpected) > aTolerance then
        Inc(Result);
      end;
end;


function SVGCompareImages(aActual, aExpected: TFPCustomImage;
  aTolerance: Integer; const aBackground: TSVGColor;
  aDiff: TFPCustomImage): TSVGComparison;

var
  X, Y, lDistance: Integer;
  lActual, lExpected: TSVGColor;
  lMark: TSVGColor;

begin
  FillChar(Result, SizeOf(Result), 0);
  if (aActual = nil) or (aExpected = nil) then
    raise ESVGCompare.Create('Nothing to compare');
  if (aActual.Width <> aExpected.Width)
     or (aActual.Height <> aExpected.Height) then
    begin
    Result.Outcome := crSizeMismatch;
    Exit;
    end;
  Result.Width := aActual.Width;
  Result.Height := aActual.Height;
  if aDiff <> nil then
    aDiff.SetSize(Result.Width, Result.Height);
  for Y := 0 to Result.Height - 1 do
    for X := 0 to Result.Width - 1 do
      begin
      lActual := SVGCompositeOver(TSVGColor(aActual.Colors[X, Y]),
        aBackground);
      lExpected := SVGCompositeOver(TSVGColor(aExpected.Colors[X, Y]),
        aBackground);
      lDistance := ChannelDistance(lActual, lExpected);
      if lDistance > Result.MaxDifference then
        Result.MaxDifference := lDistance;
      if lDistance > aTolerance then
        Inc(Result.Differing);
      if aDiff = nil then
        Continue;
      if lDistance > aTolerance then
        lMark := TSVGColor.FromBytes(255, 0, 0, 255)
      else
        // The pixels that match stay visible in grey, to give the marks
        // some context.
        lMark := TSVGColor.FromBytes(
          192 + lExpected.Red div 1040, 192 + lExpected.Green div 1040,
          192 + lExpected.Blue div 1040, 255);
      aDiff.Colors[X, Y] := TFPColor(lMark);
      end;
  if Result.Differing = 0 then
    Result.Outcome := crMatch
  else
    Result.Outcome := crDiffers;
end;


end.
