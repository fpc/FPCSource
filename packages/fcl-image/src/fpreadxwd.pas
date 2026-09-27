{*****************************************************************************}
{
    This file is part of the Free Pascal's "Free Components Library".
    Copyright (c) 2003 by Mazen NEIFER of the Free Pascal development team

    XWD reader implementation.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{*****************************************************************************}

{$mode objfpc}
{$h+}

{$IFNDEF FPC_DOTTEDUNITS}
unit FPReadXWD;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses FpImage, System.Classes, System.SysUtils, Api.Xwdfile;
{$ELSE FPC_DOTTEDUNITS}
uses FpImage, classes, sysutils, xwdfile;
{$ENDIF FPC_DOTTEDUNITS}

type
  TXWDColors = array of TXWDColor;

  { TFPReaderXWD }

  TFPReaderXWD = class (TFPCustomImageReader)
    private
      continue: boolean;              // needed for onprogress event
      percent: byte;
      percentinterval : longword;
      percentacc : longword;
      Rect : TRect;
      FLookup: array of TFPColor;
      procedure SwapXWDFileHeader(var Header: TXWDFileHeader);
      procedure SwapXWDColor(var Color: TXWDColor);
      procedure WriteScanLine(Row: Integer; Img: TFPCustomImage);
      function PixelValue(Column: Integer): Cardinal;
      function MaskedColor(Value: Cardinal): TFPColor;
      function ColormapColor(Value: Cardinal): TFPColor;
    protected
      XWDFileHeader: TXWDFileHeader;  // The header, as read from the file
      WindowName: array of AnsiChar;
      XWDColors: TXWDColors;
      LineBuf: PByte;                 // Buffer for 1 line

      // required by TFPCustomImageReader
      procedure InternalRead  (Stream:TStream; Img:TFPCustomImage); override;
      function  InternalCheck (Stream:TStream) : boolean; override;
    public
      constructor Create; override;
      destructor Destroy; override;
  end;

implementation

//==============================================================================
// Endian utils
//
// Copied from LCLProc unit
//==============================================================================
{$push}{$R-}
function BEtoN(const AValue: DWord): DWord;
begin
  {$IFDEF ENDIAN_BIG}
    Result := AValue;
  {$ELSE}
    Result := (AValue shl 24)
           or ((AValue and $0000FF00) shl 8)
           or ((AValue and $00FF0000) shr 8)
           or (AValue shr 24);
  {$ENDIF}
end;
{$pop}

constructor TFPReaderXWD.create;
begin
  inherited create;

end;

destructor TFPReaderXWD.Destroy;
begin
  If (LineBuf<>Nil) then
    begin
    FreeMem(LineBuf);
    LineBuf:=Nil;
    end;

  SetLength(WindowName, 0);

  SetLength(XWDColors, 0);

  inherited destroy;
end;

procedure TFPReaderXWD.SwapXWDColor(var Color: TXWDColor);
begin
  Color.pixel := BEtoN(Color.pixel);

  Color.red := swap(Color.red);
  Color.green := swap(Color.green);
  Color.blue := swap(Color.blue);
end;

procedure TFPReaderXWD.SwapXWDFileHeader(var Header: TXWDFileHeader);
begin
  Header.header_size := BEtoN(Header.header_size);
  Header.file_version := BEtoN(Header.file_version);
  Header.pixmap_format := BEtoN(Header.pixmap_format);
  Header.pixmap_depth := BEtoN(Header.pixmap_depth);
  Header.pixmap_width := BEtoN(Header.pixmap_width);
  Header.pixmap_height := BEtoN(Header.pixmap_height);
  Header.xoffset := BEtoN(Header.xoffset);
  Header.byte_order := BEtoN(Header.byte_order);
  Header.bitmap_unit := BEtoN(Header.bitmap_unit);
  Header.bitmap_bit_order := BEtoN(Header.bitmap_bit_order);
  Header.bitmap_pad := BEtoN(Header.bitmap_pad);
  Header.bits_per_pixel := BEtoN(Header.bits_per_pixel);
  Header.bytes_per_line := BEtoN(Header.bytes_per_line);
  Header.visual_class := BEtoN(Header.visual_class);
  Header.red_mask := BEtoN(Header.red_mask);
  Header.green_mask := BEtoN(Header.green_mask);
  Header.blue_mask := BEtoN(Header.blue_mask);
  Header.bits_per_rgb := BEtoN(Header.bits_per_rgb);
  Header.colormap_entries := BEtoN(Header.colormap_entries);
  Header.ncolors := BEtoN(Header.ncolors);
  Header.window_width := BEtoN(Header.window_width);
  Header.window_height := BEtoN(Header.window_height);
  Header.window_x := BEtoN(Header.window_x);
  Header.window_y := BEtoN(Header.window_y);
  Header.window_bdrwidth := BEtoN(Header.window_bdrwidth);
end;

const
  XWD_LSBFirst = 0;
  XWD_XYBitmap = 0;
  XWD_XYPixmap = 1;
  XWD_ZPixmap = 2;
  XWD_TrueColor = 4;
  XWD_DirectColor = 5;

// Extracts the bits of aValue under aMask, scaled to 16 bits.
function ScaleMasked(aValue, aMask: Cardinal): Word;
var
  lShift: Integer;
  lMax: Cardinal;
begin
  if aMask = 0 then
    exit(0);
  lShift := BsfDWord(aMask);
  lMax := aMask shr lShift;
  Result := (QWord((aValue and aMask) shr lShift) * $FFFF + lMax div 2) div lMax;
end;


function TFPReaderXWD.PixelValue(Column: Integer): Cardinal;
var
  P: PByte;
  I, lBytes: Integer;
begin
  with XWDFileHeader do
    case bits_per_pixel of
      1: if bitmap_bit_order = XWD_LSBFirst then
           Result := (LineBuf[Column shr 3] shr (Column and 7)) and 1
         else
           Result := (LineBuf[Column shr 3] shr (7 - (Column and 7))) and 1;
      4: if byte_order = XWD_LSBFirst then
           Result := (LineBuf[Column shr 1] shr ((Column and 1) * 4)) and $F
         else
           Result := (LineBuf[Column shr 1] shr ((1 - (Column and 1)) * 4)) and $F;
      8: Result := LineBuf[Column];
    else
      begin
        lBytes := bits_per_pixel div 8;
        P := LineBuf + Column * lBytes;
        Result := 0;
        if byte_order = XWD_LSBFirst then
          for I := lBytes - 1 downto 0 do
            Result := (Result shl 8) or P[I]
        else
          for I := 0 to lBytes - 1 do
            Result := (Result shl 8) or P[I];
      end;
    end;
end;


function TFPReaderXWD.MaskedColor(Value: Cardinal): TFPColor;
begin
  Result.Red := ScaleMasked(Value, XWDFileHeader.red_mask);
  Result.Green := ScaleMasked(Value, XWDFileHeader.green_mask);
  Result.Blue := ScaleMasked(Value, XWDFileHeader.blue_mask);
  Result.Alpha := alphaOpaque;
end;


function TFPReaderXWD.ColormapColor(Value: Cardinal): TFPColor;
var
  I: Integer;
  lMax: Cardinal;
begin
  if Value < Cardinal(Length(FLookup)) then
    exit(FLookup[Value]);
  for I := 0 to Length(XWDColors) - 1 do
    if XWDColors[I].pixel = Value then
    begin
      Result.Red := XWDColors[I].red;
      Result.Green := XWDColors[I].green;
      Result.Blue := XWDColors[I].blue;
      Result.Alpha := alphaOpaque;
      exit;
    end;
  // no colormap entry: a gray ramp over the depth
  if (XWDFileHeader.pixmap_depth > 0) and (XWDFileHeader.pixmap_depth < 32) then
    lMax := (Cardinal(1) shl XWDFileHeader.pixmap_depth) - 1
  else
    lMax := High(Cardinal);
  if Value > lMax then
    Value := lMax;
  Result.Red := Round(Value / lMax * $FFFF);
  Result.Green := Result.Red;
  Result.Blue := Result.Red;
  Result.Alpha := alphaOpaque;
end;


procedure TFPReaderXWD.WriteScanLine(Row : Integer; Img : TFPCustomImage);
var
  Column: Integer;
  lMasks: Boolean;
begin
  lMasks := XWDFileHeader.visual_class in [XWD_TrueColor, XWD_DirectColor];
  for Column := 0 to Img.Width - 1 do
    if lMasks then
      Img.Colors[Column, Row] := MaskedColor(PixelValue(Column))
    else
      Img.Colors[Column, Row] := ColormapColor(PixelValue(Column));
end;

procedure TFPReaderXWD.InternalRead(Stream: TStream; Img: TFPCustomImage);
var
  Size, Row, i: Integer;
  lTable: array of TFPColor;
begin
  Rect.Left:=0; Rect.Top:=0; Rect.Right:=0; Rect.Bottom:=0;
  continue:=true;
  Progress(psStarting,0,false,Rect,'',continue);
  if not continue then exit;

  try
    Stream.ReadBuffer(XWDFileHeader, SizeOf(TXWDFileHeader));
{$ifdef ENDIAN_LITTLE}
    SwapXWDFileHeader(XWDFileHeader);
{$endif}
    with XWDFileHeader do
    begin
      if file_version <> XWD_FILE_VERSION then
        raise FPImageException.CreateFmt('Unsupported XWD version %d', [file_version]);
      if header_size < SizeOf(TXWDFileHeader) then
        raise FPImageException.CreateFmt('Invalid XWD header size %d', [header_size]);
      if header_size - SizeOf(TXWDFileHeader) > 4096 then
        raise FPImageException.Create('XWD window name too long. The file might be corrupted.');
      if not ((pixmap_format = XWD_ZPixmap)
              or ((pixmap_format in [XWD_XYBitmap, XWD_XYPixmap]) and (pixmap_depth = 1))) then
        raise FPImageException.CreateFmt('Unsupported XWD pixmap format %d at depth %d', [pixmap_format, pixmap_depth]);
      if not (bits_per_pixel in [1, 4, 8, 16, 24, 32]) then
        raise FPImageException.CreateFmt('Unsupported XWD bits per pixel %d', [bits_per_pixel]);
      if (pixmap_width = 0) or (pixmap_height = 0) or (pixmap_width > 65535) or (pixmap_height > 65535) then
        raise FPImageException.Create('Invalid XWD dimensions');
      if (bytes_per_line < (QWord(pixmap_width) * bits_per_pixel + 7) div 8) or (bytes_per_line > 16*1024*1024) then
        raise FPImageException.Create('Invalid XWD bytes per line');
      if ncolors > 65536 then
        raise FPImageException.Create('Too many XWD colormap entries');
    end;

    // window name
    Size := XWDFileHeader.header_size - SizeOf(TXWDFileHeader);
    SetLength(WindowName, Size);
    if Size > 0 then
      Stream.ReadBuffer(WindowName[0], Size);

    // colormap
    SetLength(XWDColors, XWDFileHeader.ncolors);
    for i := 0 to Length(XWDColors) - 1 do
    begin
      Stream.ReadBuffer(XWDColors[i], SizeOf(TXWDColor));
      {$ifdef ENDIAN_LITTLE}
      SwapXWDColor(XWDColors[i]);
      {$endif}
    end;

    // for pixels of at most 16 bits, a table with the colour of every possible pixel value
    FLookup := nil;
    if not (XWDFileHeader.visual_class in [XWD_TrueColor, XWD_DirectColor])
       and (XWDFileHeader.bits_per_pixel <= 16) then
    begin
      Size := 1 shl XWDFileHeader.bits_per_pixel;
      SetLength(lTable, Size);
      for i := 0 to Size - 1 do
        lTable[i] := ColormapColor(i);
      FLookup := lTable;
    end;

    // pixels
    Img.SetSize(XWDFileHeader.pixmap_width, XWDFileHeader.pixmap_height);
    GetMem(LineBuf, XWDFileHeader.bytes_per_line);
    try
      for Row := 0 to Img.Height - 1 do
      begin
        Stream.ReadBuffer(LineBuf[0], XWDFileHeader.bytes_per_line);
        WriteScanLine(Row, Img);
        if not continue then exit;
      end;
    finally
      FreeMem(LineBuf);
      LineBuf := nil;
      FLookup := nil;
    end;
  except
    on E: EReadError do
      raise FPImageException.Create('XWD data truncated: ' + E.Message);
  end;

  Progress(psEnding,100,false,Rect,'',continue);
end;

function TFPReaderXWD.InternalCheck (Stream:TStream): boolean;
var
  Header: TXWDFileHeader;
  OldPos: Int64;
begin
  Result := False;
  if Stream = nil then
    exit;
  OldPos := Stream.Position;
  try
    if Stream.Read(Header, SizeOf(Header)) <> SizeOf(Header) then
      exit;
    {$IFDEF ENDIAN_LITTLE}
    SwapXWDFileHeader(Header);
    {$ENDIF}
    Result := Header.file_version = XWD_FILE_VERSION;
  finally
    Stream.Position := OldPos;
  end;
end;

initialization

  ImageHandlers.RegisterImageReader ('XWD Format', 'xwd', TFPReaderXWD);

end.
