{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by the Free Pascal development team

    Handle EXIF data in images

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}{$h+}
{$IFNDEF FPC_DOTTEDUNITS}
unit fpimgexif;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.SysUtils, FpImage;
{$ELSE FPC_DOTTEDUNITS}
uses
  SysUtils, FpImage;
{$ENDIF FPC_DOTTEDUNITS}

const
  // The start of a JPEG APP1 marker that holds EXIF data.
  ExifMarkerHeader: array[0..5] of Byte = (Ord('E'), Ord('x'), Ord('i'), Ord('f'), 0, 0);

// Returns aData without a leading 'Exif'#0#0.
function ExifWithoutHeader(const aData: TBytes): TBytes;
// Returns the orientation 1..8 of the EXIF data aExif, or 0 when it has none.
function ExifOrientation(const aExif: TBytes): Integer;
// Sets the orientation tag of aExif to aValue; False when aExif has no orientation tag.
function ExifSetOrientation(var aExif: TBytes; aValue: Integer): Boolean;
// Turns and mirrors aImage as EXIF orientation aOrientation asks, so that it shows upright.
procedure ExifApplyOrientation(aImage: TFPCustomImage; aOrientation: Integer);
// Applies the orientation of the EXIF metadata of aImage and sets that orientation to 1.
procedure ExifApplyImageOrientation(aImage: TFPCustomImage);

implementation

const
  TagOrientation = $0112;
  TypeShort = 3;

function ExifWithoutHeader(const aData: TBytes): TBytes;

begin
  if (Length(aData) >= 6) and CompareMem(@aData[0], @ExifMarkerHeader[0], 6) then
    Result := Copy(aData, 6, Length(aData) - 6)
  else
    Result := aData;
end;


// Returns the offset of the value of the orientation entry of IFD0 in aExif, or -1; aBig tells the byte order.
function FindOrientation(const aExif: TBytes; out aBig: Boolean): Integer;

var
  lIFD, lCount, i, lEntry: Int64;

  function Get16(aPos: Int64): Integer;

  begin
    if aBig then
      Result := (aExif[aPos] shl 8) or aExif[aPos + 1]
    else
      Result := aExif[aPos] or (aExif[aPos + 1] shl 8);
  end;

  function Get32(aPos: Int64): Int64;

  begin
    if aBig then
      Result := (Int64(aExif[aPos]) shl 24) or (aExif[aPos + 1] shl 16) or (aExif[aPos + 2] shl 8) or aExif[aPos + 3]
    else
      Result := aExif[aPos] or (aExif[aPos + 1] shl 8) or (aExif[aPos + 2] shl 16) or (Int64(aExif[aPos + 3]) shl 24);
  end;

begin
  Result := -1;
  aBig := False;
  if Length(aExif) < 8 then
    exit;
  if (aExif[0] = Ord('M')) and (aExif[1] = Ord('M')) then
    aBig := True
  else if (aExif[0] <> Ord('I')) or (aExif[1] <> Ord('I')) then
    exit;
  if Get16(2) <> 42 then
    exit;
  lIFD := Get32(4);
  if lIFD + 2 > Length(aExif) then
    exit;
  lCount := Get16(lIFD);
  for i := 0 to lCount - 1 do
    begin
    lEntry := lIFD + 2 + i * 12;
    if lEntry + 12 > Length(aExif) then
      exit;
    if (Get16(lEntry) = TagOrientation) and (Get16(lEntry + 2) = TypeShort) and (Get32(lEntry + 4) = 1) then
      exit(lEntry + 8);
    end;
end;


function ExifOrientation(const aExif: TBytes): Integer;

var
  lPos: Integer;
  lBig: Boolean;

begin
  Result := 0;
  lPos := FindOrientation(aExif, lBig);
  if lPos < 0 then
    exit;
  if lBig then
    Result := (aExif[lPos] shl 8) or aExif[lPos + 1]
  else
    Result := aExif[lPos] or (aExif[lPos + 1] shl 8);
  if (Result < 1) or (Result > 8) then
    Result := 0;
end;


function ExifSetOrientation(var aExif: TBytes; aValue: Integer): Boolean;

var
  lPos: Integer;
  lBig: Boolean;

begin
  lPos := FindOrientation(aExif, lBig);
  Result := lPos >= 0;
  if not Result then
    exit;
  if lBig then
    begin
    aExif[lPos] := (aValue shr 8) and $FF;
    aExif[lPos + 1] := aValue and $FF;
    end
  else
    begin
    aExif[lPos] := aValue and $FF;
    aExif[lPos + 1] := (aValue shr 8) and $FF;
    end;
end;


procedure ExifApplyOrientation(aImage: TFPCustomImage; aOrientation: Integer);

var
  lSource: array of TFPColor;
  lWidth, lHeight, x, y, lX, lY: Integer;

begin
  if (aOrientation < 2) or (aOrientation > 8) then
    exit;
  lWidth := aImage.Width;
  lHeight := aImage.Height;
  lSource := nil;
  SetLength(lSource, lWidth * lHeight);
  for y := 0 to lHeight - 1 do
    for x := 0 to lWidth - 1 do
      lSource[y * lWidth + x] := aImage.Colors[x, y];
  aImage.UsePalette := False;
  if aOrientation >= 5 then
    aImage.SetSize(lHeight, lWidth);
  for y := 0 to lHeight - 1 do
    for x := 0 to lWidth - 1 do
      begin
      case aOrientation of
        2: begin lX := lWidth - 1 - x; lY := y; end;
        3: begin lX := lWidth - 1 - x; lY := lHeight - 1 - y; end;
        4: begin lX := x; lY := lHeight - 1 - y; end;
        5: begin lX := y; lY := x; end;
        6: begin lX := lHeight - 1 - y; lY := x; end;
        7: begin lX := lHeight - 1 - y; lY := lWidth - 1 - x; end;
      else
        begin lX := y; lY := lWidth - 1 - x; end;
      end;
      aImage.Colors[lX, lY] := lSource[y * lWidth + x];
      end;
end;


procedure ExifApplyImageOrientation(aImage: TFPCustomImage);

var
  lExif: TBytes;
  lOrientation: Integer;

begin
  lExif := aImage.Metadata[MetaExif];
  lOrientation := ExifOrientation(lExif);
  if lOrientation < 2 then
    exit;
  ExifApplyOrientation(aImage, lOrientation);
  ExifSetOrientation(lExif, 1);
  aImage.Metadata[MetaExif] := lExif;
end;


end.
