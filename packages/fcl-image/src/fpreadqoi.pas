{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2022 by the Free Pascal development team

    QOI reader implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}{$h+}
{$IFNDEF FPC_DOTTEDUNITS}
unit FPReadQoi;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses FpImage, System.Classes, System.SysUtils, FpImage.Common.QOI;
{$ELSE FPC_DOTTEDUNITS}
uses FpImage, classes, sysutils, QoiComn;
{$ENDIF FPC_DOTTEDUNITS}

type
  TFPReaderQoi = class (TFPCustomImageReader)
    Private
      QoiHeader : TQoiHeader;  // The header as read from the stream.
      function getUseAlpha:boolean;
    protected
      // required by TFPCustomImageReader
      procedure InternalRead  (Stream:TStream; Img:TFPCustomImage); override;
      function  InternalCheck (Stream:TStream) : boolean; override;
    public
      constructor Create; override;
      property UseAlpha : boolean read getUseAlpha;
  end;

implementation


function RGBAToFPColor(Const RGBA: TQoiPixel) : TFPcolor;

begin
  with Result, RGBA do
    begin
    Red   :=(R shl 8) or R;
    Green :=(G shl 8) or G;
    Blue  :=(B shl 8) or B;
    Alpha :=(A shl 8) or A;
    end;
end;


Constructor TFPReaderQoi.create;

begin
  inherited create;
end;

function TFPReaderQoi.getUseAlpha:boolean;
begin
     result := (QoiHeader.channels=qoChannelRGBA);
end;

function  TFPReaderQoi.InternalCheck (Stream:TStream) : boolean;
var
  n: Int64;
  oldPos: Int64;
begin
  Result:=False;
  if Stream=nil then
    exit;
  oldPos:=Stream.Position;
  try
    n:=SizeOf(TQoiHeader);
    Result:=(Stream.Read(QoiHeader,n)=n) and (QoiHeader.magic = 'qoif');
  finally
    Stream.Position:=oldPos;
  end;
end;


procedure TFPReaderQoi.InternalRead (Stream:TStream; Img:TFPCustomImage);
const
  BufSize = 65536;
  MaxPixels = 400000000;
var
  Buf : array of byte;
  BufPos, BufLen : integer;
  Index : array [0..63] of TQoiPixel;
  px : TQoiPixel;
  x, y, i, run, vg : integer;
  w, h : dword;
  b1, b2 : byte;

  function TryNextByte(out aByte : byte) : boolean;
  begin
    if BufPos >= BufLen then
      begin
      BufLen := Stream.Read(Buf[0], Length(Buf));
      BufPos := 0;
      if BufLen <= 0 then
        begin
        BufLen := 0;
        aByte := 0;
        exit(False);
        end;
      end;
    aByte := Buf[BufPos];
    inc(BufPos);
    Result := True;
  end;

  function NextByte : byte;
  begin
    if not TryNextByte(Result) then
      raise FPImageException.Create('QOI data truncated');
  end;

begin
  Stream.ReadBuffer(QoiHeader, SizeOf(TQoiHeader));
  {$IFDEF ENDIAN_LITTLE}
  QoiHeader.width:=SwapEndian(QoiHeader.width);
  QoiHeader.height:=SwapEndian(QoiHeader.height);
  {$ENDIF}
  w := QoiHeader.width;
  h := QoiHeader.height;
  if (QoiHeader.magic <> 'qoif') or not (QoiHeader.channels in [qoChannelRGB, qoChannelRGBA])
     or (QoiHeader.colorspace > 1) then
    raise FPImageException.Create('Invalid QOI header');
  if (w > MaxPixels) or (h > MaxPixels) or ((w > 0) and (h > MaxPixels div w)) then
    raise FPImageException.Create('QOI dimensions too large');
  Img.SetSize(w, h);
  SetLength(Buf, BufSize);
  BufPos := 0;
  BufLen := 0;
  FillChar(Index, SizeOf(Index), 0);
  dword(px) := 0;
  px.a := 255;
  run := 0;
  for y := 0 to integer(h)-1 do
    for x := 0 to integer(w)-1 do
      begin
      if run > 0 then
        dec(run)
      else
        begin
        b1 := NextByte;
        if b1 = $FE then
          begin
          px.r := NextByte;
          px.g := NextByte;
          px.b := NextByte;
          end
        else if b1 = $FF then
          begin
          px.r := NextByte;
          px.g := NextByte;
          px.b := NextByte;
          px.a := NextByte;
          end
        else
          case b1 shr 6 of
            0 : px := Index[b1];
            1 : begin
                px.r := (integer(px.r) + ((b1 shr 4) and 3) - 2) and 255;
                px.g := (integer(px.g) + ((b1 shr 2) and 3) - 2) and 255;
                px.b := (integer(px.b) + (b1 and 3) - 2) and 255;
                end;
            2 : begin
                b2 := NextByte;
                vg := (b1 and 63) - 32;
                px.r := (integer(px.r) + vg - 8 + (b2 shr 4)) and 255;
                px.g := (integer(px.g) + vg) and 255;
                px.b := (integer(px.b) + vg - 8 + (b2 and 15)) and 255;
                end;
            3 : run := b1 and 63;
          end;
        Index[QoiPixelIndex(px)] := px;
        end;
      Img.Colors[x,y] := RGBAToFPColor(px);
      end;
  // Skip the end marker: seven zero bytes and a one.
  for i := 0 to 7 do
    begin
    if not TryNextByte(b1) then
      break;
    if b1 <> ord(i = 7) then
      begin
      dec(BufPos);
      break;
      end;
    end;
  if BufPos < BufLen then
    Stream.Seek(BufPos - BufLen, soCurrent);
end;

initialization
  ImageHandlers.RegisterImageReader ('QOI Format', 'qoi', TFPReaderQoi);
end.

