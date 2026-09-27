{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2022 by the Free Pascal development team

    QOI writer class.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}{$h+}
{$IFNDEF FPC_DOTTEDUNITS}
unit fpwriteqoi;
{$ENDIF FPC_DOTTEDUNITS}
interface

{$IFDEF FPC_DOTTEDUNITS}
uses FpImage, System.Classes, System.SysUtils, FpImage.Common.QOI;
{$ELSE FPC_DOTTEDUNITS}
uses FpImage, classes, sysutils, QoiComn;
{$ENDIF FPC_DOTTEDUNITS}

type

  TFPWriterQoi = class (TFPCustomImageWriter)
  private
    QoiHeader : TQoiHeader;
    procedure setUseAlpha(useAlpha:boolean);
    function getUseAlpha:boolean;
  protected
    function  SaveHeader(Stream:TStream; Img: TFPCustomImage):boolean; virtual;
    procedure InternalWrite (Stream:TStream; Img: TFPCustomImage); override;
  public
    constructor Create; override;
    property useAlpha:boolean read getUseAlpha write setUseAlpha;
  end;


implementation

constructor TFPWriterQoi.create;
begin
  inherited create;
  with QoiHeader do
    begin
      magic:='qoif';
      channels:=qoChannelRGB;
      colorspace:=0;
    end;
end;


procedure TFPWriterQoi.setUseAlpha(useAlpha:boolean);
begin
     if useAlpha then QoiHeader.channels := qoChannelRGBA else QoiHeader.channels:=qoChannelRGB;
end;

function TFPWriterQoi.getUseAlpha:boolean;
begin
     result:= (QoiHeader.channels=qoChannelRGBA);
end;

function TFPWriterQoi.SaveHeader(Stream:TStream; Img : TFPCustomImage):boolean;
begin
  Result:=False;
  with QoiHeader do
    begin
      Width:=Img.Width;
      Height:=Img.Height;
      //writeln('Save width ',width, '   height  ', height);
    end;

  {$IFDEF ENDIAN_LITTLE}
  QoiHeader.width:=SwapEndian(QoiHeader.width);
  QoiHeader.height:=SwapEndian(QoiHeader.height);
  {$ENDIF}

  //writeln('Save width 2 ',QoiHeader.width, '   height  ', QoiHeader.height);
  Stream.WriteBuffer(QoiHeader,sizeof(TQoiHeader));

  {$IFDEF ENDIAN_LITTLE}
  QoiHeader.width:=SwapEndian(QoiHeader.width);
  QoiHeader.height:=SwapEndian(QoiHeader.height);
  {$ENDIF}
  Result:=true;
end;



// Returns the difference a-b as a signed byte, wrapped to -128..127.
function WrapDiff(a, b : byte) : integer;
begin
  Result := ((integer(a) - integer(b) + 128) and 255) - 128;
end;


procedure TFPWriterQoi.InternalWrite (Stream:TStream; Img:TFPCustomImage);
const
  BufSize = 65536;
var
  Buf : array of byte;
  BufLen : integer;
  Index : array [0..63] of TQoiPixel;
  px, prev : TQoiPixel;
  x, y, i, run, vr, vg, vb, vgr, vgb : integer;
  iA : dword;
  color : TFPColor;

  procedure Flush;
  begin
    if BufLen > 0 then
      Stream.WriteBuffer(Buf[0], BufLen);
    BufLen := 0;
  end;

  procedure Put(aByte : byte);
  begin
    if BufLen = Length(Buf) then
      Flush;
    Buf[BufLen] := aByte;
    inc(BufLen);
  end;

begin
  SaveHeader(Stream,Img);
  SetLength(Buf, BufSize);
  BufLen := 0;
  FillChar(Index, SizeOf(Index), 0);
  dword(prev) := 0;
  prev.a := 255;
  run := 0;
  for y := 0 to Img.Height-1 do
    for x := 0 to Img.Width-1 do
      begin
      color := Img.Colors[x,y];
      px.r := color.Red shr 8;
      px.g := color.Green shr 8;
      px.b := color.Blue shr 8;
      if UseAlpha then
        px.a := color.Alpha shr 8
      else
        px.a := 255;
      if dword(px) = dword(prev) then
        begin
        inc(run);
        if run = 62 then
          begin
          Put($C0 or (run-1));
          run := 0;
          end;
        end
      else
        begin
        if run > 0 then
          begin
          Put($C0 or (run-1));
          run := 0;
          end;
        iA := QoiPixelIndex(px);
        if dword(Index[iA]) = dword(px) then
          Put(iA)
        else
          begin
          Index[iA] := px;
          if px.a = prev.a then
            begin
            vr := WrapDiff(px.r, prev.r);
            vg := WrapDiff(px.g, prev.g);
            vb := WrapDiff(px.b, prev.b);
            vgr := vr - vg;
            vgb := vb - vg;
            if (vr > -3) and (vr < 2) and (vg > -3) and (vg < 2) and (vb > -3) and (vb < 2) then
              Put($40 or ((vr+2) shl 4) or ((vg+2) shl 2) or (vb+2))
            else if (vgr > -9) and (vgr < 8) and (vg > -33) and (vg < 32) and (vgb > -9) and (vgb < 8) then
              begin
              Put($80 or (vg+32));
              Put(((vgr+8) shl 4) or (vgb+8));
              end
            else
              begin
              Put($FE);
              Put(px.r);
              Put(px.g);
              Put(px.b);
              end;
            end
          else
            begin
            Put($FF);
            Put(px.r);
            Put(px.g);
            Put(px.b);
            Put(px.a);
            end;
          end;
        end;
      prev := px;
      end;
  if run > 0 then
    Put($C0 or (run-1));
  for i := 1 to 7 do
    Put(0);
  Put(1);
  Flush;
end;

initialization
  ImageHandlers.RegisterImageWriter ('QOI Format', 'qoi', TFPWriterQoi);
end.
