{
    Writers of Windows icon (ICO) and cursor (CUR) files with 32-bit BMP or PNG entries.
    This file is part of the Free Pascal run time library.
    See the file COPYING.FPC, included in this distribution, for details.
}
{$mode objfpc}{$h+}
{$IFNDEF FPC_DOTTEDUNITS}
unit fpwriteico;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils, FpImage, FpImage.Common.Bitmap, FpImage.Writer.PNG, FpImage.Common.ICO;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils, FpImage, BMPcomn, FPWritePNG, icocomn;
{$ENDIF FPC_DOTTEDUNITS}

type
  { How the writer stores an image: iwfAuto uses PNG for images 256 pixels wide or high and BMP for smaller ones. }
  TIconWriteFormat = (iwfAuto, iwfBMP, iwfPNG);

  { An image added to the writer, encoded. }
  TIconWriterEntry = record
    Data: TBytes;
    Width, Height: Integer;
    HotSpotX, HotSpotY: Word;
  end;

  { Writes an icon file with one entry per image added. }
  TFPWriterICO = class(TFPCustomImageWriter)
  private
    FFormat: TIconWriteFormat;
    FEntries: array of TIconWriterEntry;
    function GetCount: Integer;
    function EncodeBMP(aImage: TFPCustomImage): TBytes;
    function EncodePNG(aImage: TFPCustomImage): TBytes;
  protected
    // Returns the file type the writer writes, IcoTypeIcon or IcoTypeCursor.
    class function IconType: Word; virtual;
    procedure InternalWrite(Stream: TStream; Img: TFPCustomImage); override;
    procedure InternalBeginFrames(Str: TStream; const aInfo: TFPFramesInfo); override;
    procedure InternalWriteFrame(Str: TStream; Img: TFPCustomImage; const aInfo: TFPFrameInfo); override;
    procedure InternalEndFrames(Str: TStream); override;
  public
    // Returns the kinds of frames an icon holds several of: sizes and depths of one picture.
    class function FrameKinds: TFPFrameKinds; override;
    // Removes the images added.
    procedure Clear;
    // Encodes aImage as the next entry; it is at most 256 pixels wide and high.
    procedure AddImage(aImage: TFPCustomImage);
    // Writes a file with the images added at the current position of aStream.
    procedure SaveToStream(aStream: TStream);
    // Number of images added.
    property Count: Integer read GetCount;
    // How the images added from now on are stored.
    property Format: TIconWriteFormat read FFormat write FFormat;
  end;

  { Writes a cursor file; the hotspot of an image comes from its Extra keys IcoExtraHotSpotX and IcoExtraHotSpotY. }
  TFPWriterCUR = class(TFPWriterICO)
  protected
    class function IconType: Word; override;
  end;

implementation

class function TFPWriterICO.IconType: Word;

begin
  Result := IcoTypeIcon;
end;


function TFPWriterICO.GetCount: Integer;

begin
  Result := Length(FEntries);
end;


function TFPWriterICO.EncodeBMP(aImage: TFPCustomImage): TBytes;

var
  lInfo: TBitMapInfoHeader;
  lWidth, lHeight, lXorRow, lAndRow, lPos, lMask, x, y: Integer;
  lColor: TFPColor;

begin
  lWidth := aImage.Width;
  lHeight := aImage.Height;
  lXorRow := lWidth * 4;
  lAndRow := ((lWidth + 31) div 32) * 4;
  Result := nil;
  SetLength(Result, SizeOf(lInfo) + (lXorRow + lAndRow) * lHeight);
  FillChar(Result[0], Length(Result), 0);
  FillChar(lInfo, SizeOf(lInfo), 0);
  lInfo.Size := NtoLE(LongInt(SizeOf(lInfo)));
  lInfo.Width := NtoLE(LongInt(lWidth));
  lInfo.Height := NtoLE(LongInt(lHeight * 2));
  lInfo.Planes := NtoLE(Word(1));
  lInfo.BitCount := NtoLE(Word(32));
  lInfo.Compression := NtoLE(LongInt(BI_RGB));
  lInfo.SizeImage := NtoLE(LongInt((lXorRow + lAndRow) * lHeight));
  Move(lInfo, Result[0], SizeOf(lInfo));
  lMask := SizeOf(lInfo) + lXorRow * lHeight;
  for y := 0 to lHeight - 1 do
    for x := 0 to lWidth - 1 do
      begin
      lColor := aImage.Colors[x, y];
      lPos := SizeOf(lInfo) + (lHeight - 1 - y) * lXorRow + x * 4;
      Result[lPos] := lColor.Blue shr 8;
      Result[lPos + 1] := lColor.Green shr 8;
      Result[lPos + 2] := lColor.Red shr 8;
      Result[lPos + 3] := lColor.Alpha shr 8;
      if lColor.Alpha shr 8 = 0 then
        begin
        lPos := lMask + (lHeight - 1 - y) * lAndRow + x div 8;
        Result[lPos] := Result[lPos] or ($80 shr (x mod 8));
        end;
      end;
end;


function TFPWriterICO.EncodePNG(aImage: TFPCustomImage): TBytes;

var
  lStream: TMemoryStream;
  lWriter: TFPWriterPNG;

begin
  lStream := TMemoryStream.Create;
  lWriter := TFPWriterPNG.Create;
  try
    lWriter.UseAlpha := True;
    lWriter.ImageWrite(lStream, aImage);
    Result := nil;
    SetLength(Result, lStream.Size);
    if lStream.Size > 0 then
      Move(lStream.Memory^, Result[0], lStream.Size);
  finally
    lWriter.Free;
    lStream.Free;
  end;
end;


procedure TFPWriterICO.Clear;

begin
  FEntries := nil;
end;


procedure TFPWriterICO.AddImage(aImage: TFPCustomImage);

var
  lIndex: Integer;
  lPNG: Boolean;

begin
  if not Assigned(aImage) then
    raise FPImageException.Create('No image to add');
  if (aImage.Width < 1) or (aImage.Height < 1) or (aImage.Width > 256) or (aImage.Height > 256) then
    raise FPImageException.CreateFmt('An icon image is 1 to 256 pixels wide and high, not %dx%d',
      [aImage.Width, aImage.Height]);
  if Length(FEntries) >= IcoMaxEntries then
    raise FPImageException.Create('Too many icon images');
  case FFormat of
    iwfBMP: lPNG := False;
    iwfPNG: lPNG := True;
  else
    lPNG := (aImage.Width = 256) or (aImage.Height = 256);
  end;
  lIndex := Length(FEntries);
  SetLength(FEntries, lIndex + 1);
  with FEntries[lIndex] do
    begin
    if lPNG then
      Data := EncodePNG(aImage)
    else
      Data := EncodeBMP(aImage);
    Width := aImage.Width;
    Height := aImage.Height;
    HotSpotX := 0;
    HotSpotY := 0;
    if IconType = IcoTypeCursor then
      begin
      HotSpotX := StrToIntDef(aImage.Extra[IcoExtraHotSpotX], 0) and $FFFF;
      HotSpotY := StrToIntDef(aImage.Extra[IcoExtraHotSpotY], 0) and $FFFF;
      end;
    end;
end;


procedure TFPWriterICO.SaveToStream(aStream: TStream);

var
  lDir: TIconDir;
  lEntry: TIconDirEntry;
  lOffset: LongWord;
  i: Integer;

begin
  if Length(FEntries) = 0 then
    raise FPImageException.Create('No icon images to write');
  lDir.Reserved := 0;
  lDir.IconType := IconType;
  lDir.Count := Length(FEntries);
  SwapIconDir(lDir);
  aStream.WriteBuffer(lDir, SizeOf(lDir));
  lOffset := SizeOf(TIconDir) + Length(FEntries) * SizeOf(TIconDirEntry);
  for i := 0 to High(FEntries) do
    begin
    lEntry.Width := FEntries[i].Width and $FF;
    lEntry.Height := FEntries[i].Height and $FF;
    lEntry.ColorCount := 0;
    lEntry.Reserved := 0;
    if IconType = IcoTypeCursor then
      begin
      lEntry.Planes := FEntries[i].HotSpotX;
      lEntry.BitCount := FEntries[i].HotSpotY;
      end
    else
      begin
      lEntry.Planes := 1;
      lEntry.BitCount := 32;
      end;
    lEntry.BytesInRes := Length(FEntries[i].Data);
    lEntry.ImageOffset := lOffset;
    Inc(lOffset, Length(FEntries[i].Data));
    SwapIconDirEntry(lEntry);
    aStream.WriteBuffer(lEntry, SizeOf(lEntry));
    end;
  for i := 0 to High(FEntries) do
    aStream.WriteBuffer(FEntries[i].Data[0], Length(FEntries[i].Data));
end;


procedure TFPWriterICO.InternalWrite(Stream: TStream; Img: TFPCustomImage);

var
  lSaved: array of TIconWriterEntry;

begin
  lSaved := FEntries;
  FEntries := nil;
  try
    AddImage(Img);
    SaveToStream(Stream);
  finally
    FEntries := lSaved;
  end;
end;


class function TFPWriterICO.FrameKinds: TFPFrameKinds;

begin
  Result := [fkVariant];
end;


procedure TFPWriterICO.InternalBeginFrames(Str: TStream; const aInfo: TFPFramesInfo);

begin
  Clear;
end;


procedure TFPWriterICO.InternalWriteFrame(Str: TStream; Img: TFPCustomImage; const aInfo: TFPFrameInfo);

begin
  AddImage(Img);
end;


procedure TFPWriterICO.InternalEndFrames(Str: TStream);

begin
  try
    SaveToStream(Str);
  finally
    Clear;
  end;
end;


class function TFPWriterCUR.IconType: Word;

begin
  Result := IcoTypeCursor;
end;


initialization
  ImageHandlers.RegisterImageWriter('ICO Format', 'ico', TFPWriterICO);
  ImageHandlers.RegisterImageWriter('CUR Format', 'cur', TFPWriterCUR);
end.
