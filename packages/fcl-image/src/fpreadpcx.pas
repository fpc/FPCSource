{ Copyright (C) 2007 Laurent Jacques

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

  Load all format compressed or not

  2023-07  - Massimo Magnano
           - added Resolution support
}

{$IFNDEF FPC_DOTTEDUNITS}
unit FPReadPCX;
{$ENDIF FPC_DOTTEDUNITS}

{$mode objfpc}{$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses FpImage, System.Classes, System.SysUtils, FpImage.Common.PCX;
{$ELSE FPC_DOTTEDUNITS}
uses FpImage, Classes, SysUtils, pcxcomn;
{$ENDIF FPC_DOTTEDUNITS}

type

  { TFPReaderPCX }

  TFPReaderPCX = class(TFPCustomImageReader)
  private
    FCompressed: boolean;
  protected
    Header:     TPCXHeader;
    BytesPerPixel: byte;
    FScanLine:  PByte;
    FLineSize:  integer;
    TotalWrite: longint;
    procedure CreateGrayPalette(Img: TFPCustomImage);
    procedure CreateBWPalette(Img: TFPCustomImage);
    procedure CreatePalette16(Img: TFPCustomImage);
    procedure ReadPalette(Stream: TStream; Img: TFPCustomImage);
    procedure AnalyzeHeader(Img: TFPCustomImage); virtual;
    function InternalCheck(Stream: TStream): boolean; override;
    procedure InternalRead(Stream: TStream; Img: TFPCustomImage); override;
    procedure ReadScanLine(Row: integer; Stream: TStream); virtual;
    procedure UpdateProgress(percent: longint);
    procedure WriteScanLine(Row: integer; Img: TFPCustomImage); virtual;
  public
    property Compressed: boolean Read FCompressed;
  end;

implementation

// Scales an 8-bit palette channel to 16 bits.
function Scale8(aValue: byte): word;

begin
  Result := aValue * 257;
end;


procedure TFPReaderPCX.CreatePalette16(Img: TFPCustomImage);
var
  I: integer;
  c: TFPColor;
begin
  Img.UsePalette := True;
  Img.Palette.Clear;
  for I := 0 to 15 do
  begin
    with c, header do
    begin
      Red   := Scale8(ColorMap[I].Red);
      Green := Scale8(ColorMap[I].Green);
      Blue  := Scale8(ColorMap[I].Blue);
      Alpha := alphaOpaque;
    end;
    Img.Palette.Add(c);
  end;
end;

procedure TFPReaderPCX.CreateGrayPalette(Img: TFPCustomImage);
var
  I: integer;
  c: TFPColor;
begin
  Img.UsePalette := True;
  Img.Palette.Clear;
  for I := 0 to 255 do
  begin
    with c do
    begin
      Red   := Scale8(I);
      Green := Scale8(I);
      Blue  := Scale8(I);
      Alpha := alphaOpaque;
    end;
    Img.Palette.Add(c);
  end;
end;

procedure TFPReaderPCX.CreateBWPalette(Img: TFPCustomImage);
begin
  Img.UsePalette := True;
  Img.Palette.Clear;
  Img.Palette.Add(colBlack);
  Img.Palette.Add(colWhite);
end;

{ The 256-colour palette follows the image data, after a $0C marker byte; when
  that byte is not there, the palette is read from the last 768 bytes. }
procedure TFPReaderPCX.ReadPalette(Stream: TStream; Img: TFPCustomImage);
var
  Entries: array[0..255] of TRGB;
  I:      integer;
  c:      TFPColor;
  Marker: byte;
  OldPos: Int64;
begin
  OldPos := Stream.Position;
  if not ((Stream.Read(Marker, 1) = 1) and (Marker = $0C)
          and (Stream.Read(Entries, SizeOf(Entries)) = SizeOf(Entries))) then
  begin
    if Stream.Size < SizeOf(Entries) then
      raise FPImageException.Create('PCX palette missing');
    Stream.Position := Stream.Size - SizeOf(Entries);
    Stream.ReadBuffer(Entries, SizeOf(Entries));
    Stream.Position := OldPos;
  end;
  for I := 0 to 255 do
  begin
    with c do
    begin
      Red   := Scale8(Entries[I].Red);
      Green := Scale8(Entries[I].Green);
      Blue  := Scale8(Entries[I].Blue);
      Alpha := alphaOpaque;
    end;
    Img.Palette.Color[I] := c;
  end;
end;

procedure TFPReaderPCX.AnalyzeHeader(Img: TFPCustomImage);
begin
  with Header do
  begin
    if not ((FileID in [$0A, $0C]) and (ColorPlanes in [1, 3, 4]) and
      (Version in [0, 2, 3, 5]) and (PaletteType in [1, 2])) then
      raise FPImageException.Create('Unknown/Unsupported PCX image type');
    if not (((BitsPerPixel = 1) and (ColorPlanes in [1, 4]))
         or ((BitsPerPixel in [2, 4]) and (ColorPlanes = 1))
         or ((BitsPerPixel = 8) and (ColorPlanes in [1, 3]))) then
      raise FPImageException.CreateFmt('Unsupported PCX layout: %d bits, %d planes', [BitsPerPixel, ColorPlanes]);
    BytesPerPixel := BitsPerPixel * ColorPlanes;
    FCompressed   := Encoding = 1;
    Img.Width     := XMax - XMin + 1;
    Img.Height    := YMax - YMin + 1;

    Img.ResolutionUnit:=ruPixelsPerInch;
    Img.ResolutionX :=HRes;
    Img.ResolutionY :=VRes;

    FLineSize     := (BytesPerLine * ColorPlanes);
    GetMem(FScanLine, FLineSize);
  end;
end;

procedure TFPReaderPCX.ReadScanLine(Row: integer; Stream: TStream);
var
  P: PByte;
  B: byte;
  bytes, Count: integer;
begin
  P     := FScanLine;
  bytes := FLineSize;
  Count := 0;
  if Compressed then
  begin
    while bytes > 0 do
    begin
      if (Count = 0) then
      begin
        Stream.ReadBuffer(B, 1);
        if (B < $c0) then
          Count := 1
        else
        begin
          Count := B - $c0;
          Stream.ReadBuffer(B, 1);
          if Count = 0 then
            continue;
        end;
      end;
      Dec(Count);
      P[0] := B;
      Inc(P);
      Dec(bytes);
    end;
  end
  else
    Stream.ReadBuffer(FScanLine^, FLineSize);
end;

procedure TFPReaderPCX.UpdateProgress(percent: longint);
var
  continue: boolean;
  Rect:     TRect;
begin
  Rect.Left   := 0;
  Rect.Top    := 0;
  Rect.Right  := 0;
  Rect.Bottom := 0;
  continue    := True;
  Progress(psRunning, percent, False, Rect, '', continue);
end;

procedure TFPReaderPCX.InternalRead(Stream: TStream; Img: TFPCustomImage);
var
  H, Row:   integer;
  continue: boolean;
  Rect:     TRect;
begin
  TotalWrite  := 0;
  Rect.Left   := 0;
  Rect.Top    := 0;
  Rect.Right  := 0;
  Rect.Bottom := 0;
  continue    := True;
  Progress(psStarting, 0, False, Rect, '', continue);
  Stream.ReadBuffer(Header, SizeOf(Header));
  SwapPCXHeader(Header);
  FScanLine := nil;
  try
    AnalyzeHeader(Img);
    case BytesPerPixel of
      1: CreateBWPalette(Img);
      2, 4: CreatePalette16(Img);
      8:
        begin
        Img.UsePalette := True;
        Img.Palette.Clear;
        Img.Palette.Count := 256;
        end;
      else
        Img.UsePalette := False;
    end;
    H := Img.Height;
    TotalWrite := H;
    for Row := 0 to H - 1 do
    begin
      ReadScanLine(Row, Stream);
      WriteScanLine(Row, Img);
      UpdateProgress(trunc(100.0 * (Row + 1) / TotalWrite));
    end;
    if BytesPerPixel = 8 then
      ReadPalette(Stream, Img);
    Progress(psEnding, 100, False, Rect, '', continue);
  finally
    FreeMem(FScanLine);
    FScanLine := nil;
  end;
end;

procedure TFPReaderPCX.WriteScanLine(Row: integer; Img: TFPCustomImage);
var
  Col:   integer;
  C:     TFPColor;
  P, P1, P2, P3: PByte;
  Z2:    word;
  color: byte;
begin
  C.Alpha := AlphaOpaque;
  P  := FScanLine;
  Z2 := Header.BytesPerLine;
  case BytesPerPixel of
    1:
      for Col := 0 to Img.Width - 1 do
        if (P[col div 8] and (128 shr (col mod 8))) <> 0 then
          Img.Pixels[Col, Row] := 1
        else
          Img.Pixels[Col, Row] := 0;
    2:
      for Col := 0 to Img.Width - 1 do
        Img.Pixels[Col, Row] := (P[col div 4] shr (6 - 2 * (col mod 4))) and 3;
    4:
      if Header.ColorPlanes = 1 then
        for Col := 0 to Img.Width - 1 do
          Img.Pixels[Col, Row] := (P[col div 2] shr (4 * (1 - col mod 2))) and $F
      else
      begin
        P1 := P;
        Inc(P1, Z2);
        P2 := P;
        Inc(P2, Z2 * 2);
        P3 := P;
        Inc(P3, Z2 * 3);
        for Col := 0 to Img.Width - 1 do
        begin
          color := 0;
          if (P[col div 8] and (128 shr (col mod 8))) <> 0 then
            Inc(color, 1);
          if (P1[col div 8] and (128 shr (col mod 8))) <> 0 then
            Inc(color, 1 shl 1);
          if (P2[col div 8] and (128 shr (col mod 8))) <> 0 then
            Inc(color, 1 shl 2);
          if (P3[col div 8] and (128 shr (col mod 8))) <> 0 then
            Inc(color, 1 shl 3);
          Img.Pixels[Col, Row] := color;
        end;
      end;
    8:
      for Col := 0 to Img.Width - 1 do
        Img.Pixels[Col, Row] := P[Col];
    24:
      for Col := 0 to Img.Width - 1 do
      begin
        with C do
        begin
          Red   := Scale8(P[col]);
          Green := Scale8(P[col + Z2]);
          Blue  := Scale8(P[col + Z2 * 2]);
          Alpha := alphaOpaque;
        end;
        Img[col, row] := C;
      end;
  end;
end;

function TFPReaderPCX.InternalCheck(Stream: TStream): boolean;
var
  hdr: TPcxHeader;
  n: Integer;
  oldPos: Int64;
begin
  Result:=False;
  if Stream = nil then
    exit;
  oldPos := Stream.Position;
  try
    n:=SizeOf(hdr);
    Result:=(Stream.Read(hdr, n)=n)
            and (hdr.FileID in [$0A, $0C])
            and (hdr.ColorPlanes in [1, 3, 4])
            and (hdr.Version in [0, 2, 3, 5])
            and (hdr.PaletteType in [1, 2]);
  finally
    Stream.Position := oldPos;
  end;
end;


initialization
  ImageHandlers.RegisterImageReader('PCX Format', 'pcx', TFPReaderPCX);
end.
