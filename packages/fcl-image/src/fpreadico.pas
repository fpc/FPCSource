{
    Readers of Windows icon (ICO) and cursor (CUR) files: BMP and PNG entries.
    This file is part of the Free Pascal run time library.
    See the file COPYING.FPC, included in this distribution, for details.
}
{$mode objfpc}{$h+}
{$IFNDEF FPC_DOTTEDUNITS}
unit fpreadico;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils, System.Types, System.Math, FpImage, FpImage.Common.Bitmap,
  FpImage.Reader.Bitmap, FpImage.Reader.PNG, FpImage.Common.ICO;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils, Types, Math, FpImage, BMPcomn, FPReadBMP, FPReadPNG, icocomn;
{$ENDIF FPC_DOTTEDUNITS}

type
  { Reads the largest and deepest image of an icon file, or any of its entries. }
  TFPReaderICO = class(TFPCustomImageReader)
  private
    FStart: Int64;
    FEnd: Int64;
    FEntries: array of TIconEntry;
    FNextFrame: Integer;
    function GetEntry(aIndex: Integer): TIconEntry;
    function GetEntryCount: Integer;
    function ReadDirectory(aStream: TStream): Boolean;
    procedure ScanEntry(aStream: TStream; var aEntry: TIconEntry);
    procedure ReadBMPEntry(const aData: TBytes; aImage: TFPCustomImage);
    procedure ReadPNGEntry(const aData: TBytes; aImage: TFPCustomImage);
  protected
    // Returns the file type the reader accepts, IcoTypeIcon or IcoTypeCursor.
    class function IconType: Word; virtual;
    procedure InternalRead(Stream: TStream; Img: TFPCustomImage); override;
    function InternalCheck(Stream: TStream): Boolean; override;
    class function InternalSize(Stream: TStream): TPoint; override;
    function InternalBeginFrames(Str: TStream): TFPFramesInfo; override;
    function InternalReadFrame(Str: TStream; Img: TFPCustomImage; var aInfo: TFPFrameInfo): Boolean; override;
    procedure InternalEndFrames(Str: TStream); override;
  public
    // Reads the directory of the file that starts at the current position of aStream.
    procedure LoadFromStream(aStream: TStream);
    // Reads the image of entry aIndex of the directory read last into aImage.
    procedure ReadEntry(aStream: TStream; aIndex: Integer; aImage: TFPCustomImage);
    // Returns the index of the entry with the most pixels, and of those the one with the most bits per pixel.
    function BestEntry: Integer;
    // Number of entries in the directory read last.
    property EntryCount: Integer read GetEntryCount;
    // The entries of the directory read last, with the size and depth of their image data.
    property Entries[aIndex: Integer]: TIconEntry read GetEntry;
  end;

  { Reads a cursor file; the hotspot goes to the Extra keys IcoExtraHotSpotX and IcoExtraHotSpotY. }
  TFPReaderCUR = class(TFPReaderICO)
  protected
    class function IconType: Word; override;
  end;

implementation

class function TFPReaderICO.IconType: Word;

begin
  Result := IcoTypeIcon;
end;


function TFPReaderICO.GetEntry(aIndex: Integer): TIconEntry;

begin
  if (aIndex < 0) or (aIndex >= Length(FEntries)) then
    raise FPImageException.CreateFmt('Icon entry index %d out of range', [aIndex]);
  Result := FEntries[aIndex];
end;


function TFPReaderICO.GetEntryCount: Integer;

begin
  Result := Length(FEntries);
end;


// Reads the header and the entries; False when they are not those of a file of IconType.
function TFPReaderICO.ReadDirectory(aStream: TStream): Boolean;

var
  lDir: TIconDir;
  lEntry: TIconDirEntry;
  lDataStart, lSize: Int64;
  i: Integer;

begin
  Result := False;
  FEntries := nil;
  FStart := aStream.Position;
  FEnd := 0;
  if aStream.Read(lDir, SizeOf(lDir)) <> SizeOf(lDir) then
    exit;
  SwapIconDir(lDir);
  if (lDir.Reserved <> 0) or (lDir.IconType <> IconType) or (lDir.Count = 0) then
    exit;
  lDataStart := SizeOf(TIconDir) + Int64(lDir.Count) * SizeOf(TIconDirEntry);
  lSize := aStream.Size - FStart;
  SetLength(FEntries, lDir.Count);
  for i := 0 to lDir.Count - 1 do
    begin
    if aStream.Read(lEntry, SizeOf(lEntry)) <> SizeOf(lEntry) then
      exit;
    SwapIconDirEntry(lEntry);
    if (lEntry.BytesInRes = 0) or (lEntry.ImageOffset < lDataStart)
      or (Int64(lEntry.ImageOffset) + lEntry.BytesInRes > lSize) then
      exit;
    if (IconType = IcoTypeIcon) and ((lEntry.Planes > 1) or not (lEntry.BitCount in [0, 1, 2, 4, 8, 16, 24, 32])) then
      exit;
    with FEntries[i] do
      begin
      Width := lEntry.Width;
      if Width = 0 then
        Width := 256;
      Height := lEntry.Height;
      if Height = 0 then
        Height := 256;
      if IconType = IcoTypeCursor then
        begin
        BitCount := 0;
        HotSpotX := lEntry.Planes;
        HotSpotY := lEntry.BitCount;
        end
      else
        begin
        BitCount := lEntry.BitCount;
        HotSpotX := 0;
        HotSpotY := 0;
        end;
      Offset := lEntry.ImageOffset;
      Size := lEntry.BytesInRes;
      Format := iefBMP;
      if Int64(Offset) + Size > FEnd then
        FEnd := Int64(Offset) + Size;
      end;
    end;
  Result := True;
end;


// Sets the format, size and depth of aEntry from the start of its image data.
procedure TFPReaderICO.ScanEntry(aStream: TStream; var aEntry: TIconEntry);

const
  cPNGChannels: array[0..6] of Byte = (1, 0, 3, 1, 2, 0, 4);

var
  lBuf: array[0..39] of Byte;
  lCount: Integer;

begin
  FillChar(lBuf, SizeOf(lBuf), 0);
  aStream.Position := FStart + aEntry.Offset;
  lCount := aStream.Read(lBuf, Min(SizeOf(lBuf), aEntry.Size));
  if (lCount >= 8) and CompareMem(@lBuf[0], @IcoPNGSignature[0], 8) then
    begin
    aEntry.Format := iefPNG;
    if lCount >= 26 then
      begin
      aEntry.Width := BEtoN(PLongWord(@lBuf[16])^);
      aEntry.Height := BEtoN(PLongWord(@lBuf[20])^);
      if lBuf[25] <= High(cPNGChannels) then
        aEntry.BitCount := lBuf[24] * cPNGChannels[lBuf[25]];
      end;
    end
  else if lCount >= 16 then
    begin
    aEntry.Format := iefBMP;
    aEntry.Width := LEtoN(PLongInt(@lBuf[4])^);
    aEntry.Height := Abs(LEtoN(PLongInt(@lBuf[8])^)) div 2;
    aEntry.BitCount := LEtoN(PWord(@lBuf[14])^);
    end;
end;


procedure TFPReaderICO.ReadPNGEntry(const aData: TBytes; aImage: TFPCustomImage);

var
  lStream: TMemoryStream;
  lReader: TFPReaderPNG;

begin
  lStream := TMemoryStream.Create;
  lReader := TFPReaderPNG.Create;
  try
    lStream.WriteBuffer(aData[0], Length(aData));
    lStream.Position := 0;
    lReader.ImageRead(lStream, aImage);
  finally
    lReader.Free;
    lStream.Free;
  end;
end;


procedure TFPReaderICO.ReadBMPEntry(const aData: TBytes; aImage: TFPCustomImage);

var
  lHeaderSize, lWidth, lHeight, lBits, lCompression, lColors: Integer;
  lPixelStart, lXorRow, lAndRow, lAndStart, lRowStart: Int64;
  lFile: TBitMapFileHeader;
  lHeader: TBytes;
  lStream: TMemoryStream;
  lReader: TFPReaderBMP;
  lBitmap: TFPMemoryImage;
  lUseAlpha, lUseMask: Boolean;
  lColor: TFPColor;
  i, x, y: Integer;

begin
  if Length(aData) < 40 then
    raise FPImageException.Create('Icon bitmap entry too short');
  lHeaderSize := LEtoN(PLongInt(@aData[0])^);
  lWidth := LEtoN(PLongInt(@aData[4])^);
  lHeight := Abs(LEtoN(PLongInt(@aData[8])^)) div 2;
  lBits := LEtoN(PWord(@aData[14])^);
  lCompression := LEtoN(PLongInt(@aData[16])^);
  lColors := LEtoN(PLongInt(@aData[32])^);
  if (lHeaderSize < 40) or (lHeaderSize > Length(aData)) then
    raise FPImageException.Create('Invalid icon bitmap header');
  if (lWidth <= 0) or (lHeight <= 0) or (lWidth > 65535) or (lHeight > 65535) then
    raise FPImageException.Create('Invalid icon bitmap dimensions');
  if lBits > 8 then
    lColors := 0
  else if (lColors <= 0) or (lColors > 1 shl lBits) then
    lColors := 1 shl lBits;
  lPixelStart := lHeaderSize + lColors * 4;
  if (lCompression = BI_BITFIELDS) and (lHeaderSize = 40) then
    Inc(lPixelStart, 12);
  lXorRow := ((Int64(lWidth) * lBits + 31) div 32) * 4;
  lAndRow := ((Int64(lWidth) + 31) div 32) * 4;
  lAndStart := lPixelStart + lXorRow * lHeight;
  lUseMask := lCompression in [BI_RGB, BI_BITFIELDS];
  lUseAlpha := False;
  if (lBits = 32) and (lCompression = BI_RGB) then
    for i := 0 to lWidth * lHeight - 1 do
      if (lPixelStart + i * 4 + 3 < Length(aData)) and (aData[lPixelStart + i * 4 + 3] <> 0) then
        begin
        lUseAlpha := True;
        break;
        end;
  lHeader := Copy(aData, 0, Length(aData));
  PLongInt(@lHeader[8])^ := NtoLE(LongInt(lHeight));
  PLongInt(@lHeader[20])^ := 0;
  lFile.bfType := NtoLE(Word(BMmagic));
  lFile.bfSize := NtoLE(LongInt(SizeOf(lFile) + Length(aData)));
  lFile.bfReserved := 0;
  lFile.bfOffset := NtoLE(LongInt(SizeOf(lFile) + lPixelStart));
  lStream := TMemoryStream.Create;
  lReader := TFPReaderBMP.Create;
  lBitmap := TFPMemoryImage.Create(0, 0);
  try
    lStream.WriteBuffer(lFile, SizeOf(lFile));
    lStream.WriteBuffer(lHeader[0], Length(lHeader));
    lStream.Position := 0;
    lReader.ImageRead(lStream, lBitmap);
    aImage.UsePalette := False;
    aImage.SetSize(lWidth, lHeight);
    for y := 0 to lHeight - 1 do
      begin
      lRowStart := lAndStart + Int64(lHeight - 1 - y) * lAndRow;
      for x := 0 to lWidth - 1 do
        begin
        lColor := lBitmap.Colors[x, y];
        if not lUseAlpha then
          begin
          lColor.Alpha := alphaOpaque;
          if lUseMask and (lRowStart + lAndRow <= Length(aData))
            and ((aData[lRowStart + x div 8] shr (7 - x mod 8)) and 1 <> 0) then
            lColor := colTransparent;
          end;
        aImage.Colors[x, y] := lColor;
        end;
      end;
  finally
    lBitmap.Free;
    lReader.Free;
    lStream.Free;
  end;
end;


function TFPReaderICO.InternalCheck(Stream: TStream): Boolean;

begin
  try
    Result := ReadDirectory(Stream);
  except
    Result := False;
  end;
end;


procedure TFPReaderICO.LoadFromStream(aStream: TStream);

var
  i: Integer;

begin
  if not ReadDirectory(aStream) then
    raise FPImageException.Create('Not a valid icon file');
  for i := 0 to High(FEntries) do
    ScanEntry(aStream, FEntries[i]);
  aStream.Position := FStart + FEnd;
end;


procedure TFPReaderICO.ReadEntry(aStream: TStream; aIndex: Integer; aImage: TFPCustomImage);

var
  lEntry: TIconEntry;
  lData: TBytes;

begin
  lEntry := GetEntry(aIndex);
  lData := nil;
  SetLength(lData, lEntry.Size);
  aStream.Position := FStart + lEntry.Offset;
  aStream.ReadBuffer(lData[0], lEntry.Size);
  if (lEntry.Size >= 8) and CompareMem(@lData[0], @IcoPNGSignature[0], 8) then
    ReadPNGEntry(lData, aImage)
  else
    ReadBMPEntry(lData, aImage);
  if IconType = IcoTypeCursor then
    begin
    aImage.Extra[IcoExtraHotSpotX] := IntToStr(lEntry.HotSpotX);
    aImage.Extra[IcoExtraHotSpotY] := IntToStr(lEntry.HotSpotY);
    end;
end;


function TFPReaderICO.BestEntry: Integer;

var
  i: Integer;

begin
  Result := -1;
  for i := 0 to High(FEntries) do
    if (Result < 0)
      or (Int64(FEntries[i].Width) * FEntries[i].Height > Int64(FEntries[Result].Width) * FEntries[Result].Height)
      or ((Int64(FEntries[i].Width) * FEntries[i].Height = Int64(FEntries[Result].Width) * FEntries[Result].Height)
          and (FEntries[i].BitCount > FEntries[Result].BitCount)) then
      Result := i;
end;


procedure TFPReaderICO.InternalRead(Stream: TStream; Img: TFPCustomImage);

var
  i: Integer;

begin
  for i := 0 to High(FEntries) do
    ScanEntry(Stream, FEntries[i]);
  ReadEntry(Stream, BestEntry, Img);
  Stream.Position := FStart + FEnd;
end;


function TFPReaderICO.InternalBeginFrames(Str: TStream): TFPFramesInfo;

var
  i: Integer;

begin
  for i := 0 to High(FEntries) do
    ScanEntry(Str, FEntries[i]);
  FNextFrame := 0;
  Result := DefaultFramesInfo;
  Result.FrameCount := Length(FEntries);
  if Length(FEntries) > 0 then
    begin
    Result.Width := FEntries[0].Width;
    Result.Height := FEntries[0].Height;
    end;
end;


function TFPReaderICO.InternalReadFrame(Str: TStream; Img: TFPCustomImage; var aInfo: TFPFrameInfo): Boolean;

begin
  Result := FNextFrame < Length(FEntries);
  if not Result then
    exit;
  ReadEntry(Str, FNextFrame, Img);
  Inc(FNextFrame);
  aInfo.Kind := fkVariant;
end;


procedure TFPReaderICO.InternalEndFrames(Str: TStream);

begin
  Str.Position := FStart + FEnd;
end;


class function TFPReaderICO.InternalSize(Stream: TStream): TPoint;

var
  lReader: TFPReaderICO;
  lBest: Integer;

begin
  Result := Point(0, 0);
  lReader := Create;
  try
    try
      lReader.LoadFromStream(Stream);
      lBest := lReader.BestEntry;
      Result := Point(lReader.FEntries[lBest].Width, lReader.FEntries[lBest].Height);
    except
      on FPImageException do
        Result := Point(0, 0);
    end;
  finally
    lReader.Free;
  end;
end;


class function TFPReaderCUR.IconType: Word;

begin
  Result := IcoTypeCursor;
end;


initialization
  ImageHandlers.RegisterImageReader('ICO Format', 'ico', TFPReaderICO);
  ImageHandlers.RegisterImageReader('CUR Format', 'cur', TFPReaderCUR);
end.
