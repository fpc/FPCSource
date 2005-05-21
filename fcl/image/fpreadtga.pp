{*****************************************************************************}
{
    This file is part of the Free Pascal's "Free Components Library".
    Copyright (c) 2003 by Mazen NEIFER of the Free Pascal development team

    BMP writer implementation.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{*****************************************************************************}

{$mode objfpc}
{$h+}

unit FPReadTGA;

interface

uses FPImage, classes, sysutils, targacmn;

type
  TFPReaderTarga = class (TFPCustomImageReader)
  Private
    Procedure FreeBuffers;       // Free (and nil) buffers.
  protected
    Header         : TTargaHeader;
    Identification : ShortString;
    Compressed,
    BottomUp       : Boolean;
    BytesPerPixel  : Byte;
    FPalette        : PFPColor;
    FScanLine      : PByte;
    FLineSize      : Integer;
    FPaletteSize   : Integer;
    FBlockCount    : Integer;
    FPixelCount    : Integer;
    FLastPixel     : Packed Array[0..3] of byte;
    // AnalyzeHeader will allocate the needed buffers.
    Procedure AnalyzeHeader(Img : TFPCustomImage);
    Procedure ReadPalette(Stream : TStream);
    procedure ReadScanLine(Row : Integer; Stream : TStream); virtual;
    procedure WriteScanLine(Row : Integer; Img : TFPCustomImage); virtual;
    // required by TFPCustomImageReader
    procedure InternalRead  (Stream:TStream; Img:TFPCustomImage); override;
    function  InternalCheck (Stream:TStream) : boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

Implementation

Constructor TFPReaderTarga.Create;

begin
end;

Destructor TFPReaderTarga.Destroy;

begin
  FreeBuffers;
  Inherited;
end;

Procedure TFPReaderTarga.FreeBuffers;

begin
  If (FScanLine<>Nil) then
    begin
    FreeMem(FScanLine);
    FScanLine:=Nil;
    end;
  If (FPalette<>Nil) then
    begin
    FreeMem(FPalette);
    FScanLine:=Nil;
    end;
end;

Procedure TFPReaderTarga.AnalyzeHeader(Img : TFPCustomImage);

begin
  With Header do
    begin
    If (Flags shl 6)<>0 then
      Raise Exception.Create('Interlaced targa images not supported.');
    If MapType>1 then
      Raise Exception.CreateFmt('Unknown targa colormap type: %d',[MapType]);
    if (PixelSize and 7)<>0 then
      Raise Exception.Create('Pixelsize must be multiple of 8');
    BottomUp:=(Flags and $20) <>0;
    BytesPerPixel:=PixelSize shr 3;
    Compressed:=ImgType>8;
    If Compressed then
      ImgType:=ImgType-8;
    Case ImgType of
      1: if (BytesPerPixel<>1) or (MapType<>1) then
           Raise Exception.Create('Error in targa header: Colormapped image needs 1 byte per pixel and maptype 1');
      2: If not (BytesPerPixel in [2..4]) then
           Raise Exception.Create('Error in targa header: RGB image needs bytes per pixel between 2 and 4');
      3: begin
         if BytesPerPixel<>1 then
           Raise Exception.Create('Error in targa header: Grayscale image needs 1 byte per pixel.');
         end;
    else
      Raise Exception.CreateFmt('Unknown/Unsupported Targa image type : %d',[ImgType]);
    end;
    if (ToWord(MapLength)>0) and (MapEntrySize<>24) then
      Raise Exception.CreateFmt('Only targa BGR colormaps are supported. Got : %d',[MapEntrySize]);
    if (ToWord(MapLength)>0) and (MapType<>0) then
      Raise Exception.Create('Empty colormap in Targa image file');
    FLineSize:=BytesPerPixel*ToWord(Width);
    GetMem(FScanLine,FLineSize);
    FPaletteSize:=SizeOf(TFPColor)*ToWord(MapLength);
    GetMem(FPalette,FPaletteSize);
    Img.Width:=ToWord(Width);
    Img.Height:=ToWord(Height);
    end;
end;

Procedure TFPReaderTarga.ReadPalette(Stream : TStream);

Var
  Entry : TBGREntry;
  I : Integer;

begin
  For I:=0 to ToWord(Header.MapLength)-1 do
    begin
    Stream.ReadBuffer(Entry,SizeOf(Entry));
    With FPalette[i] do
      begin
      Red:=Entry.Red;
      Green:=Entry.Green;
      Blue:=Entry.Blue;
      Alpha:=AlphaOpaque;
      end;
    end;
end;

Procedure TFPReaderTarga.InternalRead  (Stream:TStream; Img:TFPCustomImage);

var
  H,Row : Integer;

begin
  Stream.Read(Header,SizeOf(Header));
  AnalyzeHeader(Img);
  If Header.IdLen>0 then
    begin
    SetLength(Identification,Header.IDLen);
    Stream.Read(Identification[1],Header.Idlen);
    If Length(Identification)<>0 then
      Img.Extra[KeyIdentification]:=Identification;
    end;
  If Toword(Header.MapLength)>0 then
    ReadPalette(Stream);
  H:=Img.height;
  If BottomUp then
    For Row:=0 to H-1 do
      begin
      ReadScanLine(Row,Stream);
      WriteScanLine(Row,Img);
      end
  else
    For Row:=H-1 downto 0 do
      begin
      ReadScanLine(Row,Stream);
      WriteScanLine(Row,Img);
      end;
end;

Procedure TFPReaderTarga.ReadScanLine(Row : Integer; Stream : TStream);

Var
  P : PByte;
  B : Byte;
  I,J : Integer;

begin
  If Not Compressed then
    Stream.ReadBuffer(FScanLine^,FLineSize)
  else
    begin
    P:=FScanLine;
    For I:=0 to ToWord(Header.Width)-1 do
      begin
      If (FPixelCount>0) then
        Dec(FPixelCount)
      else
        begin
        Dec(FBlockCount);
        If (FBlockCount<0) then
          begin
          Stream.ReadBuffer(B,1);
          If (B and $80)<>0 then
            begin
            FPixelCount:=B and $7F;
            FblockCount:=0;
            end
          else
            FBlockCount:=B and $7F
          end;
        Stream.ReadBuffer(FlastPixel,BytesPerPixel);
        end;
      For J:=0 to BytesPerPixel-1 do
        begin
        P[0]:=FLastPixel[j];
        Inc(P);
        end;
      end;
    end;
end;

const
  c5to8bits : array[0..32-1] of Byte =
   (  0,   8,  16,  25,  33,  41,  49,  58,
     66,  74,  82,  90,  99, 107, 115, 123,
    132, 140, 148, 156, 165, 173, 181, 189,
    197, 206, 214, 222, 230, 239, 247, 255);


Procedure TFPReaderTarga.WriteScanLine(Row : Integer; Img : TFPCustomImage);

Var
  Col : Integer;
  B   : Byte;
  C   : TFPColor;
  W   : Word;
  P   : PByte;

begin
  C.Alpha:=AlphaOpaque;
  P:=FScanLine;
  Case Header.ImgType of
    1 : for Col:=0 to Img.width-1 do
         Img.Colors[Col,Row]:=FPalette[P[Col]];
    2 : for Col:=0 to Img.Width-1 do
          begin
          // Fill C depending on number of pixels.
          case BytesPerPixel of
            2 : begin
                W:=P[0];
                inc(P);
                W:=W or (P[0] shl 8);
                With C do
                  begin
                  Blue:=c5to8bits[W and $1F];
                  W:=W shr 5;
                  Green:=c5to8bits[W and $1F];
                  W:=W shr 5;
                  Red:=c5to8bits[W and $1F];
                  end;
                end;
            3,4 : With C do
                  begin
                  Blue:=P[0] or (P[0] shl 8);
                  Inc(P);
                  Green:=P[0] or (P[0] shl 8);
                  Inc(P);
                  Red:=P[0] or (P[0] shl 8);
                  If bytesPerPixel=4 then
                    begin
                    Inc(P);
                    // Alpha:=P[0] or (P[0] shl 8); what is TARGA Attribute ??
                    end;
                  end;
          end; // Case BytesPerPixel;
          Img[Col,Row]:=C;
          Inc(P);
          end;
    3 : For Col:=0 to Img.Width-1 do
          begin
          B:=FScanLine[Col];
          B:=B+(B Shl 8);
          With C do
            begin
            Red:=B;
            Green:=B;
            Blue:=B;
            end;
          Img.Colors[Col,Row]:=C;
          end;
  end;
end;

function  TFPReaderTarga.InternalCheck (Stream:TStream) : boolean;

begin
  Result:=True;
end;

initialization
  ImageHandlers.RegisterImageReader ('TARGA Format', 'tga', TFPReaderTarga);
end.
