{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2025 by the Free Pascal development team

    Definition of Unit of Measure and useful conversion functions.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{ 2025 Massimo Magnano
       part of code ported from bgrabitmap with some modifications and additions}

unit FpUnitOfMeasure;

{$mode objfpc}{$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Types, FpImage;
{$ELSE FPC_DOTTEDUNITS}
uses
  Types, FpImage;
{$ENDIF FPC_DOTTEDUNITS}

type
  TUnitOfMeasure = (
    uomPixels,
    uomCentimeters,
    uomMillimeters,
    uomInches,
    uomPicas,
    uomPoints
  );

const
  {** Equivalence for the denominator unit of a ResolutionUnit and the UnitOfMeasure }
  ResolutionDenominatorUnit: array[TResolutionUnit] of TUnitOfMeasure = (uomPixels, uomInches, uomCentimeters);

  UnitOfMeasureNames: array[TUnitOfMeasure] of string = ('Pixels','Centimeters','Millimeters','Inches','Picas','Points');
  UnitOfMeasureShortNames: array[TUnitOfMeasure] of string = ('px','cm','mm','in','pc','pt');


{** Convert physical size to pixels according to image resolution. If resolution is ill-defined, it is assumed to be 96 DPI. }
function PhysicalSizeToPixels(AUnit: TUnitOfMeasure; ASize: Single;
                              AResolutionUnit: TResolutionUnit = ruPixelsPerInch;
                              AResolution: Single = 96): Single; overload;
function PhysicalSizeToPixels(AUnit: TUnitOfMeasure; ARect: TRectF;
                              AResolutionUnit: TResolutionUnit = ruPixelsPerInch;
                              AResolutionX: Single = 96; AResolutionY: Single = 96): TRectF; overload;
function PhysicalSizeToPixels(AUnit: TUnitOfMeasure; ARect: TRect; PreserveMoreData: Boolean = False;
                              AResolutionUnit: TResolutionUnit = ruPixelsPerInch;
                              AResolutionX: Single = 96; AResolutionY: Single = 96): TRect; overload;

{** Convert pixels to physical size according to image resolution. If resolution is ill-defined, it is assumed to be 96 DPI. }
function PixelsToPhysicalSize(ASizeInPixels: Single; ATargetUnit: TUnitOfMeasure = uomInches;
                              AResolutionUnit: TResolutionUnit = ruPixelsPerInch;
                              AResolution: Single = 96): Single; overload;
function PixelsToPhysicalSize(ARect: TRectF; ATargetUnit: TUnitOfMeasure = uomInches;
                              AResolutionUnit: TResolutionUnit = ruPixelsPerInch;
                              AResolutionX: Single = 96; AResolutionY: Single = 96): TRectF; overload;

{** Converts physical size from one unit of measurement to another.
    If _ASourceUnit_ is set to uomPixel then PixelsToPhysicalSize is called
    If _ATargetUnit_ is set to uomPixel then PhysicalSizeToPixels is called. }
function PhysicalSizeConvert(ASourceUnit: TUnitOfMeasure; ASourceSize: Single;
                             ATargetUnit: TUnitOfMeasure;
                             AResolutionUnit: TResolutionUnit = ruPixelsPerInch;
                             AResolution: Single = 96): Single; overload;
function PhysicalSizeConvert(ASourceUnit: TUnitOfMeasure; ASourceRect: TRectF;
                             ATargetUnit: TUnitOfMeasure;
                             AResolutionUnit: TResolutionUnit = ruPixelsPerInch;
                             AResolution: Single = 96): TRectF; overload;


implementation

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Math;
{$ELSE FPC_DOTTEDUNITS}
uses
  math;
{$ENDIF FPC_DOTTEDUNITS}

const
  //Constants for doing conversion calculations (AValue * InchFactor[Dest]/InchFactor[Source]
  InchFactor: array[TUnitOfMeasure] of Integer =
   (9600,                                     //uomPixels
    254, 2540,                                //uomCentimeters, uomMillimeters
    100, 600, 7200);                          //uomInches, uomPicas, uomPoints

function PhysicalSizeToPixels(AUnit: TUnitOfMeasure; ASize: Single;
                              AResolutionUnit: TResolutionUnit; AResolution: Single): Single;
var
   resolutionDenom: TUnitOfMeasure;

begin
  // already in pixels
  if (AUnit = uomPixels) then exit(ASize);

  // checks if resolution is ill-defined
  if (AResolution = 0) or (AResolutionUnit = ruNone) then
  begin
    // assume legacy 96 DPI
    AResolution := 96;
    AResolutionUnit := ruPixelsPerInch;
  end;

  resolutionDenom := ResolutionDenominatorUnit[AResolutionUnit];
  // already in expected unit
  if AUnit = resolutionDenom then exit(ASize * AResolution);

  Result:= ASize * (InchFactor[resolutionDenom] / InchFactor[AUnit]) * AResolution;
end;

function PhysicalSizeToPixels(AUnit: TUnitOfMeasure; ARect: TRectF;
                              AResolutionUnit: TResolutionUnit; AResolutionX: Single; AResolutionY: Single): TRectF;
begin
  Result.Top := PhysicalSizeToPixels(AUnit, ARect.Top, AResolutionUnit, AResolutionY);
  Result.Left := PhysicalSizeToPixels(AUnit, ARect.Left, AResolutionUnit, AResolutionX);
  Result.Bottom := PhysicalSizeToPixels(AUnit, ARect.Bottom, AResolutionUnit, AResolutionY);
  Result.Right := PhysicalSizeToPixels(AUnit, ARect.Right, AResolutionUnit, AResolutionX);
end;

function PhysicalSizeToPixels(AUnit: TUnitOfMeasure; ARect: TRect; PreserveMoreData: Boolean;
                              AResolutionUnit: TResolutionUnit; AResolutionX: Single; AResolutionY: Single): TRect;
var
   resRect: TRectF;

begin
  resRect:= PhysicalSizeToPixels(AUnit, ARect, AResolutionUnit, AResolutionX, AResolutionY);

  if PreserveMoreData
  then begin
         // Preserve as much data as possible:
         // use Floor for min coordinates, Ceil for max coordinates
         if (resRect.Left <= resRect.Right)
         then begin
                Result.Left   := Floor(resRect.Left);
                Result.Right  := Ceil(resRect.Right);
              end
         else begin
                // flipped horizontally: invert rounding
                Result.Left   := Ceil(resRect.Left);
                Result.Right  := Floor(resRect.Right);
              end;

         if (resRect.Top <= resRect.Bottom)
         then begin
                Result.Top    := Floor(resRect.Top);
                Result.Bottom := Ceil(resRect.Bottom);
              end
         else begin
                // flipped vertically: invert rounding
                Result.Top    := Ceil(resRect.Top);
                Result.Bottom := Floor(resRect.Bottom);
              end;
       end
  else begin
         // Default: round up to closest integer
         Result.Left   := Floor(resRect.Left+0.5);
         Result.Top    := Floor(resRect.Top+0.5);
         Result.Right  := Floor(resRect.Right+0.5);
         Result.Bottom := Floor(resRect.Bottom+0.5);
       end;
end;

function PixelsToPhysicalSize(ASizeInPixels: Single; ATargetUnit: TUnitOfMeasure;
                              AResolutionUnit: TResolutionUnit; AResolution: Single): Single;
var
  resolutionDenom: TUnitOfMeasure;
begin
  // already in expected unit
  if (ATargetUnit = uomPixels) then exit(ASizeInPixels);

  // checks if resolution is ill-defined
  if (AResolution = 0) or (AResolutionUnit = ruNone) then
  begin
    // assume legacy 96 DPI
    AResolution := 96;
    AResolutionUnit := ruPixelsPerInch;
  end;

  resolutionDenom := ResolutionDenominatorUnit[AResolutionUnit];
  // already in expected unit
  if ATargetUnit = resolutionDenom then exit(ASizeInPixels / AResolution);

  Result:= (ASizeInPixels / AResolution) * (InchFactor[ATargetUnit] / InchFactor[resolutionDenom]);
end;

function PixelsToPhysicalSize(ARect: TRectF; ATargetUnit: TUnitOfMeasure;
                              AResolutionUnit: TResolutionUnit; AResolutionX: Single; AResolutionY: Single): TRectF;
begin
  Result.Top := PixelsToPhysicalSize(ARect.Top, ATargetUnit, AResolutionUnit, AResolutionY);
  Result.Left := PixelsToPhysicalSize(ARect.Left, ATargetUnit, AResolutionUnit, AResolutionX);
  Result.Bottom := PixelsToPhysicalSize(ARect.Bottom, ATargetUnit, AResolutionUnit, AResolutionY);
  Result.Right := PixelsToPhysicalSize(ARect.Right, ATargetUnit, AResolutionUnit, AResolutionX);
end;

function PhysicalSizeConvert(ASourceUnit: TUnitOfMeasure; ASourceSize: Single;
                             ATargetUnit: TUnitOfMeasure;
                             AResolutionUnit: TResolutionUnit; AResolution: Single): Single;
begin
  // already in expected unit
  if (ASourceUnit = ATargetUnit) then exit(ASourceSize);

  if (ATargetUnit = uomPixels) then
    Result:= PhysicalSizeToPixels(ASourceUnit, ASourceSize, AResolutionUnit, AResolution)
  else if (ASourceUnit = uomPixels)
       then Result:= PixelsToPhysicalSize(ASourceSize, ATargetUnit, AResolutionUnit, AResolution)
       else Result:= ASourceSize * (InchFactor[ATargetUnit] / InchFactor[ASourceUnit]);
end;

function PhysicalSizeConvert(ASourceUnit: TUnitOfMeasure; ASourceRect: TRectF;
                             ATargetUnit: TUnitOfMeasure;
                             AResolutionUnit: TResolutionUnit; AResolution: Single): TRectF;
var
   cFact: Single;

begin
  // already in expected unit
  if (ASourceUnit = ATargetUnit) then exit(ASourceRect);

  if (ATargetUnit = uomPixels) then
    Result:= PhysicalSizeToPixels(ASourceUnit, ASourceRect, AResolutionUnit, AResolution)
  else if (ASourceUnit = uomPixels)
       then Result:= PixelsToPhysicalSize(ASourceRect, ATargetUnit, AResolutionUnit, AResolution)
       else begin
              cFact:= (InchFactor[ATargetUnit] / InchFactor[ASourceUnit]);
              Result.Top:= ASourceRect.Top * cFact;
              Result.Left:= ASourceRect.Left * cFact;
              Result.Bottom:= ASourceRect.Bottom * cFact;
              Result.Right:= ASourceRect.Right * cFact;
            end;
end;

end.

