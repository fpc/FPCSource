{*****************************************************************************}
{
    This file is part of the Free Pascal's "Free Components Library".
    Copyright (c) 2003 by Mazen NEIFER of the Free Pascal development team

    BMP reader implementation.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{*****************************************************************************}
{ 08/2005 by Giulio Bernardi:
   - Added support for 16 and 15 bpp bitmaps.
   - If we have bpp <= 8 make an indexed image instead of converting it to RGB
   - Support for RLE4 and RLE8 decoding
   - Support for top-down bitmaps

  2023-07  - Massimo Magnano
           - added Resolution support
}

{$mode objfpc}
{$h+}

{$IFNDEF FPC_DOTTEDUNITS}
unit FPReadBMP;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses FpImage, System.Types, System.Classes, System.SysUtils, FpImage.Common.Bitmap;
{$ELSE FPC_DOTTEDUNITS}
uses FpImage, types, classes, sysutils, BMPcomn;
{$ENDIF FPC_DOTTEDUNITS}

type
  TFPReaderBMP = class (TFPCustomImageReader)
    Private
      DeltaX, DeltaY : integer; // Used for the never-used delta option in RLE
      TopDown : boolean;        // If set, bitmap is stored top down instead of bottom up
      continue : boolean;       // needed for onprogress event
      percent : byte;
      percentinterval : longword;
      percentacc : longword;
      Rect : TRect;
      FFileStart : Int64;       // Stream position of the file header
      FInfoStart : Int64;       // Stream position of the info header
      FFileHeader : TBitMapFileHeader;
      FCoreHeader : Boolean;    // OS/2 BITMAPCOREHEADER: 12-byte header, 3-byte palette entries
      FMasksRead : Boolean;     // Bit field masks were read from inside a V4/V5 header
      FRLEEnd : Boolean;        // An RLE end-of-bitmap code was met
      FAnyAlpha : Boolean;      // A 32-bit BI_RGB pixel with a non-zero 4th byte was read
      AlphaMask : longword;     // Alpha bit field mask of a V4/V5 header, 0 if none
      AlphaShift : shortint;
      AlphaBits : Byte;
      RedBits, GreenBits, BlueBits : Byte;
      Procedure FreeBufs;       // Free (and nil) buffers.
    protected
      ReadSize : Integer;       // Size (in bytes) of 1 scanline.
      BFI : TBitMapInfoHeader;  // The header as read from the stream.
      FPalette : PFPcolor;      // Buffer with Palette entries. (useless now)
      LineBuf : PByte;          // Buffer for 1 scanline. Can be Byte, Word, TColorRGB or TColorRGBA
      RedMask, GreenMask, BlueMask : longword; //Used if Compression=bi_bitfields
      RedShift, GreenShift, BlueShift : shortint;
      // SetupRead will allocate the needed buffers, and read the colormap if needed.
      procedure SetupRead(nPalette, nRowBits: Integer; Stream : TStream); virtual;
      function CountBits(Value : byte) : shortint;
      function ShiftCount(Mask : longword) : shortint;
      function ExpandColor(value : longword) : TFPColor;
      procedure ExpandRLE8ScanLine(Row : Integer; Stream : TStream);
      procedure ExpandRLE4ScanLine(Row : Integer; Stream : TStream);
      procedure ReadScanLine(Row : Integer; Stream : TStream); virtual;
      procedure WriteScanLine(Row : Integer; Img : TFPCustomImage); virtual;
      // required by TFPCustomImageReader
      procedure InternalRead  (Stream:TStream; Img:TFPCustomImage); override;
      function  InternalCheck (Stream:TStream) : boolean; override;
      class function  InternalSize  (Stream:TStream) : TPoint; override;
    public
      constructor Create; override;
      destructor Destroy; override;
      property XPelsPerMeter : integer read BFI.XPelsPerMeter;
      property YPelsPerMeter : integer read BFI.YPelsPerMeter;
  end;

implementation


function RGBAToFPColor(Const RGBA: TColorRGBA) : TFPcolor;

begin
  with Result, RGBA do
    begin
    Red   :=(R shl 8) or R;
    Green :=(G shl 8) or G;
    Blue  :=(B shl 8) or B;
    Alpha :=(A shl 8) or A;
    end;
end;

Function RGBToFPColor(Const RGB : TColorRGB) : TFPColor;

begin
  with Result,RGB do
    begin  {Use only the high byte to convert the color}
    Red   := (R shl 8) + R;
    Green := (G shl 8) + G;
    Blue  := (B shl 8) + B;
    Alpha := AlphaOpaque;
    end;
end;

Constructor TFPReaderBMP.create;

begin
  inherited create;
end;

Destructor TFPReaderBMP.Destroy;

begin
  FreeBufs;
  inherited destroy;
end;

Procedure TFPReaderBMP.FreeBufs;

begin
  If (LineBuf<>Nil) then
    begin
    FreeMem(LineBuf);
    LineBuf:=Nil;
    end;
  If (FPalette<>Nil) then
    begin
    FreeMem(FPalette);
    FPalette:=Nil;
    end;
end;

{ Counts how many bits are set }
function TFPReaderBMP.CountBits(Value : byte) : shortint;
begin
   Result:=PopCnt(Value);
end;

{ If compression is bi_bitfields, there could be arbitrary masks for colors.
  Although this is not compatible with windows9x it's better to know how to read these bitmaps
  We must determine how to switch the value once masked
  Example: 0000 0111 1110 0000, if we shr 5 we have 00XX XXXX for the color, but these bits must be the
  highest in the color, so we must shr (5-(8-6))=3, and we have XXXX XX00.
  A negative value means "shift left"  }
function TFPReaderBMP.ShiftCount(Mask : longword) : shortint;
begin
  Result:=BsfDWord(Mask or ord(Mask = 0) shl 8); { Also makes the function return 0 on Mask = 0. }
  Result:=Result-(8-popcnt(byte(Mask shr Result)));
end;

// Returns the 8-bit channel aValue, of which only the top aBits bits are significant, with the bits
// below them filled by repeating the top bits, so the largest aBits-bit value becomes 255.
function Replicate(aValue, aBits : byte) : byte;

var
  lBits : integer;

begin
  Result:=aValue;
  if aBits=0 then
    exit;
  lBits:=aBits;
  while lBits<8 do
    begin
    Result:=Result or (Result shr lBits);
    lBits:=lBits*2;
    end;
end;

function TFPReaderBMP.ExpandColor(value : longword) : TFPColor;
var tmpr, tmpg, tmpb : longword;
    col : TColorRGB;
    a : byte;
begin
  {$IFDEF ENDIAN_BIG}
  value:=swap(value);
  {$ENDIF}
  tmpr:=value and RedMask;
  tmpg:=value and GreenMask;
  tmpb:=value and BlueMask;
  if RedShift < 0 then col.R:=byte(tmpr shl (-RedShift))
  else col.R:=byte(tmpr shr RedShift);
  if GreenShift < 0 then col.G:=byte(tmpg shl (-GreenShift))
  else col.G:=byte(tmpg shr GreenShift);
  if BlueShift < 0 then col.B:=byte(tmpb shl (-BlueShift))
  else col.B:=byte(tmpb shr BlueShift);
  col.R:=Replicate(col.R,RedBits);
  col.G:=Replicate(col.G,GreenBits);
  col.B:=Replicate(col.B,BlueBits);
  Result:=RGBToFPColor(col);
  if AlphaMask<>0 then
    begin
    if AlphaShift < 0 then a:=byte((value and AlphaMask) shl (-AlphaShift))
    else a:=byte((value and AlphaMask) shr AlphaShift);
    a:=Replicate(a,AlphaBits);
    Result.Alpha:=(a shl 8) or a;
    end;
end;

procedure TFPReaderBMP.SetupRead(nPalette, nRowBits: Integer; Stream : TStream);

var
  ColInfo: ARRAY OF TColorRGBA;
  i, lCount: Integer;
  lTriple: TColorRGB;

begin
  RedBits:=8;
  GreenBits:=8;
  BlueBits:=8;
  if ((BFI.Compression=BI_RGB) and (BFI.BitCount=16)) then { 5 bits per channel, fixed mask }
  begin
    RedMask:=$7C00; RedShift:=7;
    GreenMask:=$03E0; GreenShift:=2;
    BlueMask:=$001F; BlueShift:=-3;
    RedBits:=5; GreenBits:=5; BlueBits:=5;
  end
  else if ((BFI.Compression=BI_BITFIELDS) and (BFI.BitCount in [16,32])) then { arbitrary mask }
  begin
    if not FMasksRead then
      begin
      Stream.ReadBuffer(RedMask,4);
      Stream.ReadBuffer(GreenMask,4);
      Stream.ReadBuffer(BlueMask,4);
      {$IFDEF ENDIAN_BIG}
      RedMask:=swap(RedMask);
      GreenMask:=swap(GreenMask);
      BlueMask:=swap(BlueMask);
      {$ENDIF}
      end;
    RedShift:=ShiftCount(RedMask);
    GreenShift:=ShiftCount(GreenMask);
    BlueShift:=ShiftCount(BlueMask);
    RedBits:=PopCnt(RedMask);
    GreenBits:=PopCnt(GreenMask);
    BlueBits:=PopCnt(BlueMask);
    if AlphaMask<>0 then
      begin
      AlphaShift:=ShiftCount(AlphaMask);
      AlphaBits:=PopCnt(AlphaMask);
      end;
  end
  else if nPalette>0 then
    begin
    if (BFI.ClrUsed > 0) and (Integer(BFI.ClrUsed) > nPalette) then
      raise FPImageException.Create('Invalid BMP ClrUsed value');
    GetMem(FPalette, nPalette*SizeOf(TFPColor));
    SetLength(ColInfo, nPalette);
    FillChar(ColInfo[0], nPalette*SizeOf(TColorRGBA), 0);
    if BFI.ClrUsed>0 then
      lCount:=BFI.ClrUsed
    else
      lCount:=nPalette;
    if FCoreHeader then
      for i:=0 to lCount-1 do
        begin
        Stream.ReadBuffer(lTriple,SizeOf(lTriple));
        ColInfo[i].RGB:=lTriple;
        end
    else
      Stream.ReadBuffer(ColInfo[0],lCount*SizeOf(TColorRGBA));
    for i := 0 to High(ColInfo) do
      FPalette[i] := RGBToFPColor(ColInfo[i].RGB);
    end
  else if BFI.ClrUsed>0 then { Skip palette }
    Stream.Position := Stream.Position + BFI.ClrUsed*SizeOf(TColorRGBA);
  { The pixels start at the offset given in the file header }
  if (FFileHeader.bfOffset>0) and (FFileStart+FFileHeader.bfOffset>Stream.Position) then
    Stream.Position:=FFileStart+FFileHeader.bfOffset;
  ReadSize:=((nRowBits + 31) div 32) shl 2;
  GetMem(LineBuf,ReadSize);
end;

// Sets the alpha of every pixel of aImg to opaque.
procedure MakeOpaque(aImg : TFPCustomImage);

var
  lX, lY : Integer;
  lColor : TFPColor;

begin
  for lY:=0 to aImg.Height-1 do
    for lX:=0 to aImg.Width-1 do
      begin
      lColor:=aImg.Colors[lX,lY];
      lColor.Alpha:=alphaOpaque;
      aImg.Colors[lX,lY]:=lColor;
      end;
end;

procedure TFPReaderBMP.InternalRead(Stream:TStream; Img:TFPCustomImage);
// NOTE: Assumes that BMP header & Info Header already has been read in InternalCheck
Var
  Row, i, pallen : Integer;
  BadCompression : boolean;
begin
  Rect.Left:=0; Rect.Top:=0; Rect.Right:=0; Rect.Bottom:=0;
  continue:=true;
  Progress(psStarting,0,false,Rect,'',continue);
  if not continue then exit;
  FRLEEnd:=False;
  FAnyAlpha:=False;
  AlphaMask:=0;
  { From V2 headers on (52 bytes and more) the bit field masks are part of the header }
  FMasksRead:=(BFI.Compression=BI_BITFIELDS) and (BFI.Size>=52);
  if FMasksRead then
    begin
    Stream.Position:=FInfoStart+40;
    Stream.ReadBuffer(RedMask,4);
    Stream.ReadBuffer(GreenMask,4);
    Stream.ReadBuffer(BlueMask,4);
    if BFI.Size>=56 then
      Stream.ReadBuffer(AlphaMask,4);
    {$IFDEF ENDIAN_BIG}
    RedMask:=swap(RedMask);
    GreenMask:=swap(GreenMask);
    BlueMask:=swap(BlueMask);
    AlphaMask:=swap(AlphaMask);
    {$ENDIF}
    end;
  { This will move past any junk after the BFI header }
  Stream.Position:=FInfoStart+BFI.Size;
  with BFI do
  begin
    BadCompression:=false;
    if ((Compression=BI_RLE4) and (BitCount<>4)) then BadCompression:=true;
    if ((Compression=BI_RLE8) and (BitCount<>8)) then BadCompression:=true;
    if ((Compression=BI_BITFIELDS) and (not (BitCount in [16,32]))) then BadCompression:=true;
    if not (Compression in [BI_RGB..BI_BITFIELDS]) then BadCompression:=true;
    if BadCompression then
      raise FPImageException.Create('Bad BMP compression mode');
    TopDown:=(Height<0);
    Height:=abs(Height);
    if (Width <= 0) or (Width > 65535) or (Height <= 0) or (Height > 65535) then
      raise FPImageException.Create('Invalid BMP dimensions');
    if (TopDown and (not (Compression in [BI_RGB,BI_BITFIELDS]))) then
      raise FPImageException.Create('Top-down bitmaps cannot be compressed');
    Img.SetSize(0,0);
    if BitCount<=8 then
    begin
      Img.UsePalette:=true;
      Img.Palette.Clear;
    end
    else Img.UsePalette:=false;
    Case BFI.BitCount of
      1 : { Monochrome }
        SetupRead(2,Width,Stream);
      4 :
        SetupRead(16,Width*4,Stream);
      8 :
        SetupRead(256,Width*8,Stream);
      16 :
        SetupRead(0,Width*8*2,Stream);
      24:
        SetupRead(0,Width*8*3,Stream);
      32:
        SetupRead(0,Width*8*4,Stream);
    end;
  end;
  Try
    { Note: it would be better to Fill the image palette in setupread instead of creating FPalette.
      FPalette is indeed useless but we cannot remove it since it's not private :\ }
    pallen:=0;
    if BFI.BitCount<=8 then
      if BFI.ClrUsed>0 then pallen:=BFI.ClrUsed
      else pallen:=(1 shl BFI.BitCount);
    if pallen>0 then
    begin
      Img.Palette.Count:=pallen;
      for i:=0 to pallen-1 do
        Img.Palette.Color[i]:=FPalette[i];
    end;
    Img.SetSize(BFI.Width,BFI.Height);

    Img.ResolutionUnit:=ruPixelsPerCentimeter;
    Img.ResolutionX :=BFI.XPelsPerMeter/100;
    Img.ResolutionY :=BFI.YPelsPerMeter/100;

    percent:=0;
    percentinterval:=(Img.Height*4) div 100;
    if percentinterval=0 then percentinterval:=$FFFFFFFF;
    percentacc:=0;

    DeltaX:=-1; DeltaY:=-1;
      if TopDown then
        for Row:=0 to Img.Height-1 do { A rare case of top-down bitmap! }
        begin
          ReadScanLine(Row,Stream); // Scanline in LineBuf with Size ReadSize.
          WriteScanLine(Row,Img);
          if not continue then exit;
        end
      else
        for Row:=Img.Height-1 downto 0 do
        begin
          ReadScanLine(Row,Stream); // Scanline in LineBuf with Size ReadSize.
          WriteScanLine(Row,Img);
          if not continue then exit;
        end;
    if (BFI.BitCount=32) and (BFI.Compression=BI_RGB) and not FAnyAlpha then
      MakeOpaque(Img);
    Progress(psEnding,100,false,Rect,'',continue);
  finally
    FreeBufs;
  end;
end;

procedure TFPReaderBMP.ExpandRLE8ScanLine(Row : Integer; Stream : TStream);
var i,j : integer;
    b0, b1 : byte;
begin
  FillChar(LineBuf^,ReadSize,0);
  if FRLEEnd then
    exit;
  i:=0;
  while true do
  begin
    { let's see if we must skip pixels because of delta... }
    if DeltaY<>-1 then
    begin
      if Row=DeltaY then j:=DeltaX { If we are on the same line, skip till DeltaX }
      else j:=ReadSize;            { else skip up to the end of this line }
      while (i<j) do
        begin
          LineBuf[i]:=0;
          inc(i);
        end;

      if Row=DeltaY then { we don't need delta anymore }
        DeltaY:=-1
      else break; { skipping must continue on the next line, we are finished here }
    end;

    Stream.ReadBuffer(b0,1); Stream.ReadBuffer(b1,1);
    if b0<>0 then { number of repetitions }
    begin
      if b0+i>ReadSize then
        raise FPImageException.Create('Bad BMP RLE chunk at row '+inttostr(row)+', col '+inttostr(i)+', file offset $'+inttohex(Stream.Position,16) );
      j:=i+b0;
      while (i<j) do
      begin
        LineBuf[i]:=b1;
        inc(i);
      end;
    end
    else
      case b1 of
        0: break; { end of line }
        1: begin FRLEEnd:=True; break; end; { end of bitmap }
        2: begin  { Next pixel position. Skipped pixels should be left untouched, but we set them to zero }
             Stream.ReadBuffer(b0,1); Stream.ReadBuffer(b1,1);
             DeltaX:=i+b0; DeltaY:=Row-b1;
           end
        else begin { absolute mode }
               if b1+i>ReadSize then
                 raise FPImageException.Create('Bad BMP RLE chunk at row '+inttostr(row)+', col '+inttostr(i)+', file offset $'+inttohex(Stream.Position,16) );
               Stream.ReadBuffer(LineBuf[i],b1);
               inc(i,b1);
               { aligned on 2 bytes boundary: every group starts on a 2 bytes boundary, but absolute group
                 could end on odd address if there is a odd number of elements, so we pad it  }
               if (b1 mod 2)<>0 then Stream.Seek(1,soFromCurrent);
             end;
      end;
  end;
end;

procedure TFPReaderBMP.ExpandRLE4ScanLine(Row : Integer; Stream : TStream);
var i,j,tmpsize : integer;
    b0, b1 : byte;
    nibline : pbyte; { temporary array of nibbles }
    even : boolean;
begin
  tmpsize:=ReadSize*2; { ReadSize is in bytes, while nibline is made of nibbles, so it's 2*readsize long }
  getmem(nibline,tmpsize);
  if nibline=nil then
    raise FPImageException.Create('Out of memory');
  try
    FillChar(nibline^,tmpsize,0);
    if FRLEEnd then
      begin
      FillChar(LineBuf^,ReadSize,0);
      exit;
      end;
    i:=0;
    while true do
    begin
      { let's see if we must skip pixels because of delta... }
      if DeltaY<>-1 then
      begin
        if Row=DeltaY then j:=DeltaX { If we are on the same line, skip till DeltaX }
        else j:=tmpsize;            { else skip up to the end of this line }
        while (i<j) do
          begin
            NibLine[i]:=0;
            inc(i);
          end;

        if Row=DeltaY then { we don't need delta anymore }
          DeltaY:=-1
        else break; { skipping must continue on the next line, we are finished here }
      end;

      Stream.ReadBuffer(b0,1); Stream.ReadBuffer(b1,1);
      if b0<>0 then { number of repetitions }
      begin
        if b0+i>tmpsize then
          raise FPImageException.Create('Bad BMP RLE chunk at row '+inttostr(row)+', col '+inttostr(i)+', file offset $'+inttohex(Stream.Position,16) );
        even:=true;
        j:=i+b0;
        while (i<j) do
        begin
          if even then NibLine[i]:=(b1 and $F0) shr 4
          else NibLine[i]:=b1 and $0F;
          inc(i);
          even:=not even;
        end;
      end
      else
        case b1 of
          0: break; { end of line }
          1: begin FRLEEnd:=True; break; end; { end of bitmap }
          2: begin  { Next pixel position. Skipped pixels should be left untouched, but we set them to zero }
               Stream.ReadBuffer(b0,1); Stream.ReadBuffer(b1,1);
               DeltaX:=i+b0; DeltaY:=Row-b1;
             end
          else begin { absolute mode }
                 if b1+i>tmpsize then
                   raise FPImageException.Create('Bad BMP RLE chunk at row '+inttostr(row)+', col '+inttostr(i)+', file offset $'+inttohex(Stream.Position,16) );
                 j:=i+b1;
                 even:=true;
                 while (i<j) do
                 begin
                   if even then
                   begin
                     Stream.ReadBuffer(b0,1);
                     NibLine[i]:=(b0 and $F0) shr 4;
                   end
                   else NibLine[i]:=b0 and $0F;
                   inc(i);
                   even:=not even;
                 end;
               { aligned on 2 bytes boundary: see rle8 for details  }
                 b1:=b1+(b1 mod 2);
                 if (b1 mod 4)<>0 then Stream.Seek(1,soFromCurrent);
               end;
        end;
    end;
    { pack the nibline into the linebuf }
    for i:=0 to ReadSize-1 do
      LineBuf[i]:=(NibLine[i*2] shl 4) or NibLine[i*2+1];
  finally
    FreeMem(nibline)
  end;
end;

procedure TFPReaderBMP.ReadScanLine(Row : Integer; Stream : TStream);
begin
  if BFI.Compression=BI_RLE8 then ExpandRLE8ScanLine(Row,Stream)
  else if BFI.Compression=BI_RLE4 then ExpandRLE4ScanLine(Row,Stream)
  else Stream.ReadBuffer(LineBuf[0],ReadSize);
end;

procedure TFPReaderBMP.WriteScanLine(Row : Integer; Img : TFPCustomImage);

Var
  Column : Integer;

begin
  Case BFI.BitCount of
   1 :
     for Column:=0 to Img.Width-1 do
       if ((LineBuf[Column div 8] shr (7-(Column and 7)) ) and 1) <> 0 then
         img.Pixels[Column,Row]:=1
       else
         img.Pixels[Column,Row]:=0;
   4 :
      for Column:=0 to img.Width-1 do
        img.Pixels[Column,Row]:=(LineBuf[Column div 2] shr (((Column+1) and 1)*4)) and $0f;
   8 :
      for Column:=0 to img.Width-1 do
        img.Pixels[Column,Row]:=LineBuf[Column];
   16 :
      for Column:=0 to img.Width-1 do
        img.colors[Column,Row]:=ExpandColor(PWord(LineBuf)[Column]);
   24 :
      for Column:=0 to img.Width-1 do
        img.colors[Column,Row]:=RGBToFPColor(PColorRGB(LineBuf)[Column]);
   32 :
      for Column:=0 to img.Width-1 do
        if BFI.Compression=BI_BITFIELDS then
          img.colors[Column,Row]:=ExpandColor(PLongWord(LineBuf)[Column])
        else
          begin
          if PColorRGBA(LineBuf)[Column].A<>0 then
            FAnyAlpha:=True;
          img.colors[Column,Row]:=RGBAToFPColor(PColorRGBA(LineBuf)[Column]);
          end;
    end;

    inc(percentacc,4);
    if percentacc>=percentinterval then
    begin
      percent:=percent+(percentacc div percentinterval);
      percentacc:=percentacc mod percentinterval;
      Progress(psRunning,percent,false,Rect,'',continue);
    end;
end;

// Reads a BITMAPINFOHEADER, or a 12-byte OS/2 BITMAPCOREHEADER converted to one; False if the stream is too short.
function ReadInfoHeader(aStream : TStream; out aInfo : TBitMapInfoHeader; out aCore : Boolean) : Boolean;

var
  lCore : packed record
    Width, Height, Planes, BitCount : Word;
  end;

begin
  Result:=False;
  aCore:=False;
  FillChar(aInfo,SizeOf(aInfo),0);
  if aStream.Read(aInfo.Size,4)<>4 then
    exit;
  {$IFDEF ENDIAN_BIG}
  aInfo.Size:=swap(aInfo.Size);
  {$ENDIF}
  if aInfo.Size=12 then
    begin
    if aStream.Read(lCore,SizeOf(lCore))<>SizeOf(lCore) then
      exit;
    {$IFDEF ENDIAN_BIG}
    lCore.Width:=swap(lCore.Width);
    lCore.Height:=swap(lCore.Height);
    lCore.Planes:=swap(lCore.Planes);
    lCore.BitCount:=swap(lCore.BitCount);
    {$ENDIF}
    aCore:=True;
    aInfo.Width:=lCore.Width;
    aInfo.Height:=SmallInt(lCore.Height);
    aInfo.Planes:=lCore.Planes;
    aInfo.BitCount:=lCore.BitCount;
    aInfo.Compression:=BI_RGB;
    end
  else
    begin
    if aStream.Read(aInfo.Width,SizeOf(aInfo)-4)<>SizeOf(aInfo)-4 then
      exit;
    {$IFDEF ENDIAN_BIG}
    aInfo.Size:=swap(aInfo.Size);
    SwapBMPInfoHeader(aInfo);
    {$ENDIF}
    end;
  Result:=True;
end;

function  TFPReaderBMP.InternalCheck (Stream:TStream) : boolean;
// Reads bitmap file header and bitmap info header
var
  lBFH:TBitMapFileHeader;
  lPos,n: Int64;
begin
  Result:=False;
  if Stream=nil then
    exit;
  FFileStart:=Stream.Position;
  n:=SizeOf(lBFH);
  if Stream.Read(lBFH,n)<>n then
    exit;
  {$IFDEF ENDIAN_BIG}
  SwapBMPFileHeader(lBFH);
  {$ENDIF}
  if lBFH.bfType<>BMmagic then
    exit;
  if lBFH.bfReserved<>0 then
    exit;
  FFileHeader:=lBFH;
  FInfoStart:=Stream.Position;
  if not ReadInfoHeader(Stream,BFI,FCoreHeader) then
    exit;
  if not (BFI.Size in [12, 40, 52, 56, 108, 124]) then
    exit;
  if not (BFI.BitCount in [1, 4, 8, 16, 24, 32]) then
    exit;
  if not (BFI.Compression in [BI_RGB..BI_BITFIELDS]) then
    exit;
  Result:=True;
end;

class function TFPReaderBMP.InternalSize (Stream: TStream): TPoint;
var
  fileHdr: TBitmapFileHeader;
  infoHdr: TBitmapInfoHeader;
  n: Int64;
  StartPos: Int64;
  lCore: Boolean;
begin
  Result := Point(0, 0);

  StartPos := Stream.Position;
  try
    n := Stream.Read(fileHdr, SizeOf(fileHdr));
    if n <> SizeOf(fileHdr) then exit;
    if {$IFDEF ENDIAN_BIG}swap(fileHdr.bfType){$ELSE}fileHdr.bfType{$ENDIF} <> BMmagic then exit;
    if not ReadInfoHeader(Stream, infoHdr, lCore) then exit;
    Result := Point(infoHdr.Width, abs(infoHdr.Height));
  finally
    Stream.Position := StartPos;
  end;
end;

initialization
  ImageHandlers.RegisterImageReader ('BMP Format', 'bmp', TFPReaderBMP);
end.
