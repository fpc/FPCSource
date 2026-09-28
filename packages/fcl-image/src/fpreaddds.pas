{
    Reader of DirectDraw Surface (DDS) images: uncompressed, BC1, BC2 and BC3.
    This file is part of the Free Pascal run time library.
    See the file COPYING.FPC, included in this distribution, for details.
}
{$mode objfpc}{$h+}
{$IFNDEF FPC_DOTTEDUNITS}
unit fpreaddds;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils, System.Types, FpImage;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils, Types, FpImage;
{$ENDIF FPC_DOTTEDUNITS}

const
  DDSMagic = $20534444;
  DDSHeaderSize = 124;
  DDSPixelFormatSize = 32;
  DDPF_ALPHAPIXELS = $1;
  DDPF_ALPHA = $2;
  DDPF_FOURCC = $4;
  DDPF_RGB = $40;
  DDPF_LUMINANCE = $20000;
  DXGI_FORMAT_R8G8B8A8_UNORM = 28;
  DXGI_FORMAT_R8G8B8A8_UNORM_SRGB = 29;
  DXGI_FORMAT_BC1_UNORM = 71;
  DXGI_FORMAT_BC1_UNORM_SRGB = 72;
  DXGI_FORMAT_BC2_UNORM = 74;
  DXGI_FORMAT_BC2_UNORM_SRGB = 75;
  DXGI_FORMAT_BC3_UNORM = 77;
  DXGI_FORMAT_BC3_UNORM_SRGB = 78;
  DXGI_FORMAT_B8G8R8A8_UNORM = 87;
  DXGI_FORMAT_B8G8R8X8_UNORM = 88;
  DXGI_FORMAT_B8G8R8A8_UNORM_SRGB = 91;
  DXGI_FORMAT_B8G8R8X8_UNORM_SRGB = 93;
  // Largest width or height of an image.
  DDSMaxSize = 16384;

type
  // The pixel format of a DDS header.
  TDDSPixelFormat = packed record
    Size, Flags, FourCC, RGBBitCount, RBitMask, GBitMask, BBitMask, ABitMask: Cardinal;
  end;

  // The header after the magic number of a DDS file.
  TDDSHeader = packed record
    Size, Flags, Height, Width, PitchOrLinearSize, Depth, MipMapCount: Cardinal;
    Reserved1: array[0..10] of Cardinal;
    PixelFormat: TDDSPixelFormat;
    Caps, Caps2, Caps3, Caps4, Reserved2: Cardinal;
  end;

  // The extra header of a DDS file with the DX10 four-character code.
  TDDSHeaderDX10 = packed record
    DXGIFormat, ResourceDimension, MiscFlag, ArraySize, MiscFlags2: Cardinal;
  end;

  // The encoding of the pixels of a DDS file.
  TDDSEncoding = (deMasks, deBC1, deBC2, deBC3);

  { Reads the first surface of a DDS file, the largest mipmap of its first face. }
  TFPReaderDDS = class(TFPCustomImageReader)
  private
    FHeader: TDDSHeader;
    FEncoding: TDDSEncoding;
    FPremultiplied: Boolean;
    procedure ReadHeader(aStream: TStream);
    procedure ReadMasks(aStream: TStream; aImage: TFPCustomImage);
    procedure ReadBlocks(aStream: TStream; aImage: TFPCustomImage);
  protected
    function InternalCheck(Stream: TStream): Boolean; override;
    procedure InternalRead(Stream: TStream; Img: TFPCustomImage); override;
    class function InternalSize(Stream: TStream): TPoint; override;
  public
    // The header of the file read last.
    property Header: TDDSHeader read FHeader;
    // The encoding of the file read last.
    property Encoding: TDDSEncoding read FEncoding;
  end;

// Returns the four-character code of aText as a number.
function DDSFourCC(const aText: AnsiString): Cardinal;

implementation

function DDSFourCC(const aText: AnsiString): Cardinal;

begin
  Result := Ord(aText[1]) or (Ord(aText[2]) shl 8) or (Ord(aText[3]) shl 16) or (Cardinal(Ord(aText[4])) shl 24);
end;


function TFPReaderDDS.InternalCheck(Stream: TStream): Boolean;

var
  lPos: Int64;
  lMagic, lSize: Cardinal;

begin
  lPos := Stream.Position;
  try
    Result := (Stream.Read(lMagic, 4) = 4) and (LEtoN(lMagic) = DDSMagic)
              and (Stream.Read(lSize, 4) = 4) and (LEtoN(lSize) = DDSHeaderSize);
  finally
    Stream.Position := lPos;
  end;
end;


class function TFPReaderDDS.InternalSize(Stream: TStream): TPoint;

var
  lMagic: Cardinal;
  lHeader: TDDSHeader;

begin
  Result := Point(-1, -1);
  if (Stream.Read(lMagic, 4) = 4) and (LEtoN(lMagic) = DDSMagic)
     and (Stream.Read(lHeader, SizeOf(lHeader)) = SizeOf(lHeader)) then
    Result := Point(LEtoN(lHeader.Width), LEtoN(lHeader.Height));
end;


// Reads the magic number and the headers, and chooses the encoding.
procedure TFPReaderDDS.ReadHeader(aStream: TStream);

var
  lMagic: Cardinal;
  lDX10: TDDSHeaderDX10;
  lFormat: TDDSPixelFormat;
  lCode: Cardinal;

  procedure SetMasks(aBits, aRed, aGreen, aBlue, aAlpha: Cardinal);

  begin
    FEncoding := deMasks;
    lFormat.Flags := DDPF_RGB;
    if aAlpha <> 0 then
      lFormat.Flags := lFormat.Flags or DDPF_ALPHAPIXELS;
    lFormat.RGBBitCount := aBits;
    lFormat.RBitMask := aRed;
    lFormat.GBitMask := aGreen;
    lFormat.BBitMask := aBlue;
    lFormat.ABitMask := aAlpha;
  end;

begin
  aStream.ReadBuffer(lMagic, 4);
  if LEtoN(lMagic) <> DDSMagic then
    raise FPImageException.Create('Not a DDS image');
  aStream.ReadBuffer(FHeader, SizeOf(FHeader));
  with FHeader do
    begin
    Size := LEtoN(Size);
    Flags := LEtoN(Flags);
    Height := LEtoN(Height);
    Width := LEtoN(Width);
    PitchOrLinearSize := LEtoN(PitchOrLinearSize);
    Depth := LEtoN(Depth);
    MipMapCount := LEtoN(MipMapCount);
    Caps := LEtoN(Caps);
    Caps2 := LEtoN(Caps2);
    end;
  with FHeader.PixelFormat do
    begin
    Size := LEtoN(Size);
    Flags := LEtoN(Flags);
    FourCC := LEtoN(FourCC);
    RGBBitCount := LEtoN(RGBBitCount);
    RBitMask := LEtoN(RBitMask);
    GBitMask := LEtoN(GBitMask);
    BBitMask := LEtoN(BBitMask);
    ABitMask := LEtoN(ABitMask);
    end;
  if (FHeader.Size <> DDSHeaderSize) or (FHeader.PixelFormat.Size <> DDSPixelFormatSize) then
    raise FPImageException.Create('Invalid DDS header');
  if (FHeader.Width = 0) or (FHeader.Height = 0) or (FHeader.Width > DDSMaxSize) or (FHeader.Height > DDSMaxSize) then
    raise FPImageException.CreateFmt('Invalid DDS size: %dx%d', [Int64(FHeader.Width), Int64(FHeader.Height)]);
  FPremultiplied := False;
  lFormat := FHeader.PixelFormat;
  if lFormat.Flags and DDPF_FOURCC = 0 then
    FEncoding := deMasks
  else if lFormat.FourCC = DDSFourCC('DXT1') then
    FEncoding := deBC1
  else if (lFormat.FourCC = DDSFourCC('DXT2')) or (lFormat.FourCC = DDSFourCC('DXT3')) then
    begin
    FEncoding := deBC2;
    FPremultiplied := lFormat.FourCC = DDSFourCC('DXT2');
    end
  else if (lFormat.FourCC = DDSFourCC('DXT4')) or (lFormat.FourCC = DDSFourCC('DXT5')) then
    begin
    FEncoding := deBC3;
    FPremultiplied := lFormat.FourCC = DDSFourCC('DXT4');
    end
  else if lFormat.FourCC = DDSFourCC('DX10') then
    begin
    aStream.ReadBuffer(lDX10, SizeOf(lDX10));
    case LEtoN(lDX10.DXGIFormat) of
      DXGI_FORMAT_BC1_UNORM, DXGI_FORMAT_BC1_UNORM_SRGB : FEncoding := deBC1;
      DXGI_FORMAT_BC2_UNORM, DXGI_FORMAT_BC2_UNORM_SRGB : FEncoding := deBC2;
      DXGI_FORMAT_BC3_UNORM, DXGI_FORMAT_BC3_UNORM_SRGB : FEncoding := deBC3;
      DXGI_FORMAT_R8G8B8A8_UNORM, DXGI_FORMAT_R8G8B8A8_UNORM_SRGB :
        SetMasks(32, $FF, $FF00, $FF0000, $FF000000);
      DXGI_FORMAT_B8G8R8A8_UNORM, DXGI_FORMAT_B8G8R8A8_UNORM_SRGB :
        SetMasks(32, $FF0000, $FF00, $FF, $FF000000);
      DXGI_FORMAT_B8G8R8X8_UNORM, DXGI_FORMAT_B8G8R8X8_UNORM_SRGB :
        SetMasks(32, $FF0000, $FF00, $FF, 0);
    else
      raise FPImageException.CreateFmt('Unsupported DDS DXGI format: %d', [LEtoN(lDX10.DXGIFormat)]);
    end;
    end
  else
    begin
    lCode := lFormat.FourCC;
    raise FPImageException.CreateFmt('Unsupported DDS four-character code: %s',
      [AnsiChar(lCode and $FF) + AnsiChar((lCode shr 8) and $FF) + AnsiChar((lCode shr 16) and $FF) + AnsiChar(lCode shr 24)]);
    end;
  if FEncoding = deMasks then
    begin
    if not (lFormat.RGBBitCount in [8, 16, 24, 32]) then
      raise FPImageException.CreateFmt('Unsupported DDS bit count: %d', [lFormat.RGBBitCount]);
    if lFormat.Flags and (DDPF_RGB or DDPF_LUMINANCE or DDPF_ALPHA) = 0 then
      raise FPImageException.Create('Unsupported DDS pixel format');
    end;
  FHeader.PixelFormat := lFormat;
end;


// Returns the bits of aValue under aMask, scaled to 0..65535.
function MaskedValue(aValue, aMask: Cardinal): Word;

var
  lShift: Integer;
  lMax: QWord;

begin
  if aMask = 0 then
    Exit(0);
  lShift := 0;
  while aMask and (Cardinal(1) shl lShift) = 0 do
    Inc(lShift);
  lMax := aMask shr lShift;
  Result := ((QWord(aValue and aMask) shr lShift) * 65535 + lMax div 2) div lMax;
end;


// Reads pixels whose components are given by the bit masks of the pixel format.
procedure TFPReaderDDS.ReadMasks(aStream: TStream; aImage: TFPCustomImage);

var
  lRow: TBytes;
  lBytes, lX, lY: Integer;
  lValue: Cardinal;
  lColor: TFPColor;
  lFormat: TDDSPixelFormat;

begin
  lFormat := FHeader.PixelFormat;
  lBytes := lFormat.RGBBitCount div 8;
  SetLength(lRow, lBytes * Integer(FHeader.Width));
  for lY := 0 to aImage.Height - 1 do
    begin
    aStream.ReadBuffer(lRow[0], Length(lRow));
    for lX := 0 to aImage.Width - 1 do
      begin
      lValue := 0;
      Move(lRow[lX * lBytes], lValue, lBytes);
      lValue := LEtoN(lValue);
      if lFormat.Flags and DDPF_LUMINANCE <> 0 then
        begin
        lColor.Red := MaskedValue(lValue, lFormat.RBitMask);
        lColor.Green := lColor.Red;
        lColor.Blue := lColor.Red;
        end
      else if lFormat.Flags and DDPF_RGB <> 0 then
        begin
        lColor.Red := MaskedValue(lValue, lFormat.RBitMask);
        lColor.Green := MaskedValue(lValue, lFormat.GBitMask);
        lColor.Blue := MaskedValue(lValue, lFormat.BBitMask);
        end
      else
        begin
        lColor.Red := 0;
        lColor.Green := 0;
        lColor.Blue := 0;
        end;
      if (lFormat.Flags and (DDPF_ALPHAPIXELS or DDPF_ALPHA) <> 0) and (lFormat.ABitMask <> 0) then
        lColor.Alpha := MaskedValue(lValue, lFormat.ABitMask)
      else
        lColor.Alpha := AlphaOpaque;
      aImage.Colors[lX, lY] := lColor;
      end;
    end;
end;


// Expands an RGB 5:6:5 colour to three bytes.
procedure Decode565(aValue: Word; out aRed, aGreen, aBlue: Integer);

begin
  aRed := (aValue shr 11) and $1F;
  aRed := (aRed shl 3) or (aRed shr 2);
  aGreen := (aValue shr 5) and $3F;
  aGreen := (aGreen shl 2) or (aGreen shr 4);
  aBlue := aValue and $1F;
  aBlue := (aBlue shl 3) or (aBlue shr 2);
end;


// Reads 4x4 blocks of BC1, BC2 or BC3 data.
procedure TFPReaderDDS.ReadBlocks(aStream: TStream; aImage: TFPCustomImage);

var
  lBlock: array[0..15] of Byte;
  lColors: PByte;
  lPalette: array[0..3, 0..3] of Integer;
  lAlpha: array[0..15] of Byte;
  lAlphaLevels: array[0..7] of Integer;
  lBlockSize, lBlocksX, lBlocksY, lBX, lBY, lI, lX, lY: Integer;
  lC0, lC1: Word;
  lIndices: Cardinal;
  lAlphaBits: QWord;
  lColor: TFPColor;
  lA, lR0, lG0, lB0, lR1, lG1, lB1: Integer;

  procedure SetEntry(aIndex, aRed, aGreen, aBlue, aAlphaValue: Integer);

  begin
    lPalette[aIndex, 0] := aRed;
    lPalette[aIndex, 1] := aGreen;
    lPalette[aIndex, 2] := aBlue;
    lPalette[aIndex, 3] := aAlphaValue;
  end;

  // Returns the byte of 0..255, un-premultiplied by aAlphaValue when the file is premultiplied.
  function Component(aValue, aAlphaValue: Integer): Word;

  begin
    if FPremultiplied and (aAlphaValue > 0) and (aAlphaValue < 255) then
      aValue := (aValue * 255 + aAlphaValue div 2) div aAlphaValue;
    if aValue > 255 then
      aValue := 255;
    Result := aValue * 257;
  end;

begin
  if FEncoding = deBC1 then
    lBlockSize := 8
  else
    lBlockSize := 16;
  lColors := @lBlock[lBlockSize - 8];
  lBlocksX := (aImage.Width + 3) div 4;
  lBlocksY := (aImage.Height + 3) div 4;
  for lBY := 0 to lBlocksY - 1 do
    for lBX := 0 to lBlocksX - 1 do
      begin
      aStream.ReadBuffer(lBlock, lBlockSize);
      lC0 := lColors[0] or (lColors[1] shl 8);
      lC1 := lColors[2] or (lColors[3] shl 8);
      Decode565(lC0, lR0, lG0, lB0);
      Decode565(lC1, lR1, lG1, lB1);
      SetEntry(0, lR0, lG0, lB0, 255);
      SetEntry(1, lR1, lG1, lB1, 255);
      if (lC0 > lC1) or (FEncoding <> deBC1) then
        begin
        SetEntry(2, (2 * lR0 + lR1) div 3, (2 * lG0 + lG1) div 3, (2 * lB0 + lB1) div 3, 255);
        SetEntry(3, (lR0 + 2 * lR1) div 3, (lG0 + 2 * lG1) div 3, (lB0 + 2 * lB1) div 3, 255);
        end
      else
        begin
        SetEntry(2, (lR0 + lR1) div 2, (lG0 + lG1) div 2, (lB0 + lB1) div 2, 255);
        SetEntry(3, 0, 0, 0, 0);
        end;
      lIndices := lColors[4] or (lColors[5] shl 8) or (lColors[6] shl 16) or (Cardinal(lColors[7]) shl 24);
      case FEncoding of
        deBC2 :
          for lI := 0 to 15 do
            lAlpha[lI] := ((lBlock[lI div 2] shr (4 * (lI and 1))) and $F) * 17;
        deBC3 :
          begin
          lAlphaLevels[0] := lBlock[0];
          lAlphaLevels[1] := lBlock[1];
          if lBlock[0] > lBlock[1] then
            for lI := 1 to 6 do
              lAlphaLevels[lI + 1] := ((7 - lI) * lBlock[0] + lI * lBlock[1]) div 7
          else
            begin
            for lI := 1 to 4 do
              lAlphaLevels[lI + 1] := ((5 - lI) * lBlock[0] + lI * lBlock[1]) div 5;
            lAlphaLevels[6] := 0;
            lAlphaLevels[7] := 255;
            end;
          lAlphaBits := 0;
          for lI := 7 downto 2 do
            lAlphaBits := (lAlphaBits shl 8) or lBlock[lI];
          for lI := 0 to 15 do
            lAlpha[lI] := lAlphaLevels[(lAlphaBits shr (3 * lI)) and 7];
          end;
      else
        ;
      end;
      for lI := 0 to 15 do
        begin
        lX := lBX * 4 + (lI and 3);
        lY := lBY * 4 + (lI shr 2);
        if (lX >= aImage.Width) or (lY >= aImage.Height) then
          Continue;
        with lColor do
          begin
          if FEncoding = deBC1 then
            lA := lPalette[(lIndices shr (2 * lI)) and 3, 3]
          else
            lA := lAlpha[lI];
          Red := Component(lPalette[(lIndices shr (2 * lI)) and 3, 0], lA);
          Green := Component(lPalette[(lIndices shr (2 * lI)) and 3, 1], lA);
          Blue := Component(lPalette[(lIndices shr (2 * lI)) and 3, 2], lA);
          Alpha := lA * 257;
          end;
        aImage.Colors[lX, lY] := lColor;
        end;
      end;
end;


procedure TFPReaderDDS.InternalRead(Stream: TStream; Img: TFPCustomImage);

begin
  ReadHeader(Stream);
  Img.SetSize(FHeader.Width, FHeader.Height);
  if FEncoding = deMasks then
    ReadMasks(Stream, Img)
  else
    ReadBlocks(Stream, Img);
end;


initialization
  ImageHandlers.RegisterImageReader('DirectDraw Surface', 'dds', TFPReaderDDS);
end.
