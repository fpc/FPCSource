{
    Reader of Radiance HDR (RGBE) images, flat or run-length encoded.
    This file is part of the Free Pascal run time library.
    See the file COPYING.FPC, included in this distribution, for details.
}
{$mode objfpc}{$h+}
{$IFNDEF FPC_DOTTEDUNITS}
unit fpreadhdr;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils, FpImage, FpImage.Common.HDR;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils, FpImage, hdrcomn;
{$ENDIF FPC_DOTTEDUNITS}

type
  { Reads a Radiance HDR image; values above 1 become 65535. }
  TFPReaderHDR = class(TFPCustomImageReader)
  private
    FData: TBytes;
    FPos: Integer;
    function NextByte: Byte;
    procedure ReadScanline(var aLine: array of TRGBE);
  protected
    function InternalCheck(Stream: TStream): Boolean; override;
    procedure InternalRead(Stream: TStream; Img: TFPCustomImage); override;
  end;

implementation

// Reads one header line without its line feed; the line is limited to aMaxLength characters.
function ReadHeaderLine(aStream: TStream; aMaxLength: Integer; out aLine: AnsiString): Boolean;

var
  lChar: Byte;

begin
  aLine := '';
  Result := False;
  while aStream.Read(lChar, 1) = 1 do
    begin
    if lChar = 10 then
      Exit(True);
    if Length(aLine) >= aMaxLength then
      Exit;
    aLine := aLine + AnsiChar(lChar);
    end;
end;


// Splits aLine at spaces into aWords; returns the number of words, or one more than aWords holds when there are more.
function SplitWords(const aLine: AnsiString; var aWords: array of AnsiString): Integer;

var
  I: Integer;

begin
  Result := 0;
  I := 1;
  while I <= Length(aLine) do
    begin
    if aLine[I] = ' ' then
      begin
      Inc(I);
      Continue;
      end;
    if Result > High(aWords) then
      Exit(Result + 1);
    aWords[Result] := '';
    while (I <= Length(aLine)) and (aLine[I] <> ' ') do
      begin
      aWords[Result] := aWords[Result] + aLine[I];
      Inc(I);
      end;
    Inc(Result);
    end;
end;


function TFPReaderHDR.InternalCheck(Stream: TStream): Boolean;

var
  lPos: Int64;
  lLine: AnsiString;

begin
  lPos := Stream.Position;
  try
    Result := ReadHeaderLine(Stream, 64, lLine) and ((lLine = HDRMagic) or (lLine = HDRMagicRGBE));
  finally
    Stream.Position := lPos;
  end;
end;


function TFPReaderHDR.NextByte: Byte;

begin
  if FPos >= Length(FData) then
    raise FPImageException.Create('Unexpected end of HDR data');
  Result := FData[FPos];
  Inc(FPos);
end;


// Reads one scanline, in the run-length encoding of each component or as flat pixels.
procedure TFPReaderHDR.ReadScanline(var aLine: array of TRGBE);

var
  lLength, lIndex, lCount, lComponent: Integer;
  lRun: Int64;
  lShift: Integer;
  lValue: Byte;
  lPixel: TRGBE;

begin
  lLength := Length(aLine);
  if (lLength >= 8) and (lLength <= $7FFF) and (FPos + 4 <= Length(FData))
     and (FData[FPos] = 2) and (FData[FPos + 1] = 2) and (FData[FPos + 2] < $80) then
    begin
    if (FData[FPos + 2] shl 8) or FData[FPos + 3] <> lLength then
      raise FPImageException.Create('HDR scanline has the wrong length');
    Inc(FPos, 4);
    for lComponent := 0 to 3 do
      begin
      lIndex := 0;
      while lIndex < lLength do
        begin
        lCount := NextByte;
        if lCount > 128 then
          begin
          Dec(lCount, 128);
          if lIndex + lCount > lLength then
            raise FPImageException.Create('HDR run passes the end of the scanline');
          lValue := NextByte;
          while lCount > 0 do
            begin
            PByte(@aLine[lIndex])[lComponent] := lValue;
            Inc(lIndex);
            Dec(lCount);
            end;
          end
        else
          begin
          if (lCount = 0) or (lIndex + lCount > lLength) then
            raise FPImageException.Create('Invalid HDR run length');
          while lCount > 0 do
            begin
            PByte(@aLine[lIndex])[lComponent] := NextByte;
            Inc(lIndex);
            Dec(lCount);
            end;
          end;
        end;
      end;
    Exit;
    end;
  lIndex := 0;
  lShift := 0;
  while lIndex < lLength do
    begin
    lPixel.R := NextByte;
    lPixel.G := NextByte;
    lPixel.B := NextByte;
    lPixel.E := NextByte;
    if (lPixel.R = 1) and (lPixel.G = 1) and (lPixel.B = 1) then
      begin
      if (lIndex = 0) or (lShift > 24) then
        raise FPImageException.Create('Invalid HDR repeat');
      lRun := Int64(lPixel.E) shl lShift;
      if lIndex + lRun > lLength then
        raise FPImageException.Create('HDR repeat passes the end of the scanline');
      while lRun > 0 do
        begin
        aLine[lIndex] := aLine[lIndex - 1];
        Inc(lIndex);
        Dec(lRun);
        end;
      Inc(lShift, 8);
      end
    else
      begin
      aLine[lIndex] := lPixel;
      Inc(lIndex);
      lShift := 0;
      end;
    end;
end;


procedure TFPReaderHDR.InternalRead(Stream: TStream; Img: TFPCustomImage);

var
  lLine, lFormat: AnsiString;
  lParts: array[0..3] of AnsiString;
  lSlowY, lFlipX, lFlipY: Boolean;
  lSlowCount, lFastCount, lWidth, lHeight, lScan, lIndex, lX, lY: Integer;
  lPixels: array of TRGBE;
  lSize: Int64;

begin
  if not ReadHeaderLine(Stream, 64, lLine) or ((lLine <> HDRMagic) and (lLine <> HDRMagicRGBE)) then
    raise FPImageException.Create('Not a Radiance HDR image');
  lFormat := HDRFormatRGBE;
  repeat
    if not ReadHeaderLine(Stream, 4096, lLine) then
      raise FPImageException.Create('Invalid HDR header');
    if Copy(lLine, 1, 7) = 'FORMAT=' then
      lFormat := Trim(Copy(lLine, 8, MaxInt));
  until lLine = '';
  if lFormat <> HDRFormatRGBE then
    raise FPImageException.CreateFmt('Unsupported HDR format: %s', [lFormat]);
  if not ReadHeaderLine(Stream, 64, lLine) then
    raise FPImageException.Create('Invalid HDR resolution');
  if (SplitWords(lLine, lParts) <> 4) or (Length(lParts[0]) <> 2) or (Length(lParts[2]) <> 2)
     or not (lParts[0][1] in ['+', '-']) or not (lParts[2][1] in ['+', '-'])
     or not (((lParts[0][2] = 'Y') and (lParts[2][2] = 'X')) or ((lParts[0][2] = 'X') and (lParts[2][2] = 'Y'))) then
    raise FPImageException.CreateFmt('Invalid HDR resolution: %s', [lLine]);
  lSlowCount := StrToIntDef(lParts[1], -1);
  lFastCount := StrToIntDef(lParts[3], -1);
  if (lSlowCount <= 0) or (lFastCount <= 0) or (Int64(lSlowCount) * lFastCount > HDRMaxPixels) then
    raise FPImageException.CreateFmt('Invalid HDR resolution: %s', [lLine]);
  lSlowY := lParts[0][2] = 'Y';
  if lSlowY then
    begin
    lHeight := lSlowCount;
    lWidth := lFastCount;
    lFlipY := lParts[0][1] = '+';
    lFlipX := lParts[2][1] = '-';
    end
  else
    begin
    lWidth := lSlowCount;
    lHeight := lFastCount;
    lFlipX := lParts[0][1] = '-';
    lFlipY := lParts[2][1] = '+';
    end;
  lSize := Stream.Size - Stream.Position;
  if lSize > Int64(lSlowCount) * lFastCount * 5 + Int64(lSlowCount) * 4 then
    lSize := Int64(lSlowCount) * lFastCount * 5 + Int64(lSlowCount) * 4;
  SetLength(FData, lSize);
  if lSize > 0 then
    Stream.ReadBuffer(FData[0], lSize);
  FPos := 0;
  try
    Img.SetSize(lWidth, lHeight);
    SetLength(lPixels, lFastCount);
    for lScan := 0 to lSlowCount - 1 do
      begin
      ReadScanline(lPixels);
      for lIndex := 0 to lFastCount - 1 do
        begin
        if lSlowY then
          begin
          lY := lScan;
          lX := lIndex;
          end
        else
          begin
          lX := lScan;
          lY := lIndex;
          end;
        if lFlipX then
          lX := lWidth - 1 - lX;
        if lFlipY then
          lY := lHeight - 1 - lY;
        Img.Colors[lX, lY] := RGBEToColor(lPixels[lIndex]);
        end;
      end;
    Stream.Seek(FPos - lSize, soCurrent);
  finally
    FData := nil;
  end;
end;


initialization
  ImageHandlers.RegisterImageReader('Radiance HDR', 'hdr', TFPReaderHDR);
end.
