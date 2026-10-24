
{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by the Free Pascal development team

    PSD reader for fpImage.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************

  ToDo: read further images

  2023-07  - Massimo Magnano
           - code fixes for reading palettes
           - added Read of Image Resources Section
           - added Resolution support

}
{$IFNDEF FPC_DOTTEDUNITS}
unit FPReadPSD;
{$ENDIF FPC_DOTTEDUNITS}

{$mode objfpc}{$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils, FpImage, FpImage.Common.PSD, FpImage.ColorSpace;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils, PSDcomn, FPimage, FPColorSpace;
{$ENDIF FPC_DOTTEDUNITS}

type
  TFPReaderPSD = class;

  TPSDCreateCompatibleImgEvent = procedure(Sender: TFPReaderPSD;
                                        var NewImage: TFPCustomImage) of object;

  { TFPReaderPSD }

  TFPReaderPSD = class(TFPCustomImageReader)
  private
    FCompressed: boolean;
    FOnCreateImage: TPSDCreateCompatibleImgEvent;
  protected
    FHeader        : TPSDHeader;
    FBytesPerPixel : Byte;
    FScanLine      : PByte;
    FLineSize      : PtrInt;
    FPalette       : TFPPalette;
    FWidth         : integer;
    FHeight        : Integer;
    FBlockCount    : word;
    FChannelCount  : word;
    FLengthOfLine  : array of Word;
    FByteRead      : PtrInt;
    FRowBytes      : PtrInt;
    FPlaneSize     : PtrInt;
    // Returns channel aChannel of pixel (aX, aY) scaled to 16 bits.
    function Sample(aChannel, aX, aY: Integer): Word;
    procedure CreateGrayPalette;
    procedure CreateBWPalette;
    function ReadPalette(Stream: TStream): boolean;
    procedure AnalyzeHeader;
    procedure ReadResourceBlockData(Img: TFPCustomImage; blockID:Word;
                                    blockName:ShortString; Size:LongWord; Data:Pointer); virtual;
    procedure InternalRead(Stream: TStream; Img: TFPCustomImage); override;
    function ReadScanLine(Stream: TStream): boolean; virtual;
    procedure WriteScanLine(Img: TFPCustomImage); virtual;
    function  InternalCheck(Stream: TStream) : boolean; override;
  public
    constructor Create; override;
    property Compressed: Boolean read FCompressed;
    property ThePalette: TFPPalette read FPalette;
    property Width: integer read FWidth;
    property Height: integer read FHeight;
    property BytesPerPixel: Byte read FBytesPerPixel;
    property BlockCount: word read FBlockCount;
    property ChannelCount: word read FChannelCount;
    property Header: TPSDHeader read FHeader;
    property OnCreateImage: TPSDCreateCompatibleImgEvent read FOnCreateImage write FOnCreateImage;
  end;

implementation

function CorrectCMYK(const C : TFPColor): TFPColor;
var
  MinColor: word;
begin
  if C.red<C.green then MinColor:=C.red
  else MinColor:= C.green;
  if C.blue<MinColor then MinColor:= C.blue;
  if MinColor+ C.alpha>$FFFF then MinColor:=$FFFF-C.alpha;
  Result.red:=C.red-MinColor;
  Result.green:=C.green-MinColor;
  Result.blue:=C.blue-MinColor;
  Result.alpha:=C.alpha+MinColor;
end;

function CMYKtoRGB ( C : TFPColor):  TFPColor;
begin
  C:=CorrectCMYK(C);
  if (C.red + C.Alpha)<$FFFF then
    Result.Red:=$FFFF-(C.red+C.Alpha) else Result.Red:=0;
  if (C.Green+C.Alpha)<$FFFF then
    Result.Green:=$FFFF-(C.Green+C.Alpha) else Result.Green:=0;
  if (C.blue+C.Alpha)<$FFFF then
    Result.blue:=$FFFF-(C.blue+C.Alpha) else Result.blue:=0;
  Result.alpha:=alphaOpaque;
end;


{ TFPReaderPSD }

procedure TFPReaderPSD.CreateGrayPalette;
Var
  I : Integer;
  c : TFPColor;
Begin
  ThePalette.count := 0;
  For I:=0 To 255 Do
  Begin
    With c do
    begin
      Red:=I*257;
      Green:=I*257;
      Blue:=I*257;
      Alpha:=alphaOpaque;
    end;
    ThePalette.Add (c);
  End;
end;

procedure TFPReaderPSD.CreateBWPalette;
begin
  ThePalette.count := 0;
  ThePalette.Add (colBlack);
  ThePalette.Add (colWhite);
end;

function TFPReaderPSD.ReadPalette(Stream: TStream): boolean;
Var
  BufSize:Longint;

  procedure ReadPaletteFromStream;
  var
    i : Integer;
    c : TFPColor;
    {%H-}PalBuf: array[0..767] of Byte;
    ContProgress: Boolean;

  begin
    Stream.ReadBuffer({%H-}PalBuf, BufSize);
    ContProgress:=true;
    Progress(FPimage.psRunning, 0, False, Rect(0,0,0,0), '', ContProgress);
    if not ContProgress then exit;
    for i:=0 to (BufSize div 3) - 1 do
    begin
      with c do
      begin
        Red:=PalBuf[I]*257;
        Green:=PalBuf[I+(BufSize div 3)]*257;
        Blue:=PalBuf[I+(BufSize div 3)* 2]*257;
        Alpha:=alphaOpaque;
      end;
      FPalette.Add(c);
    end;
  end;

begin
  Result:=False;
  BufSize:=0;
  Stream.ReadBuffer(BufSize, SizeOf(BufSize));
  BufSize:=BEtoN(BufSize);
  if (BufSize < 0) or ((FHeader.Mode = PSD_INDEXED) and (BufSize > 768)) then
    raise FPImageException.Create('Invalid PSD color mode data size');
  if FHeader.Mode <> PSD_INDEXED then
    Stream.Seek(BufSize, soCurrent);

  Case FHeader.Mode of
  PSD_BITMAP :begin  // Bitmap (monochrome)
                FPalette := TFPPalette.Create(0);
                CreateBWPalette;
              end;
  PSD_GRAYSCALE,
  PSD_DUOTONE:begin // Gray-scale or Duotone image
                FPalette := TFPPalette.Create(0);
                CreateGrayPalette;
              end;
  PSD_INDEXED:begin // Indexed color (palette color)
                FPalette := TFPPalette.Create(0);
                if (BufSize=0) then exit;
                ReadPaletteFromStream;
              end;
  end;

  Result:=True;
end;

procedure TFPReaderPSD.AnalyzeHeader;
var
  lMinChannels: Integer;
begin
  With FHeader do
  begin
    Depth:=BEtoN(Depth);
    if (Signature <> '8BPS') then
      Raise FPImageException.Create('Unknown/Unsupported PSD image type');
    if BEtoN(Version) <> 1 then
      Raise FPImageException.CreateFmt('Unsupported PSD version %d',[BEtoN(Version)]);
    Channels:=BEtoN(Channels);
    if Channels > 4 then
      FBytesPerPixel:=Depth*4
    else
      FBytesPerPixel:=Depth*Channels;
    Mode:=BEtoN(Mode);
    FWidth:=BEtoN(Columns);
    FHeight:=BEtoN(Rows);
    if (FWidth <= 0) or (FWidth > 300000) or (FHeight <= 0) or (FHeight > 300000) then
      raise FPImageException.Create('Invalid PSD dimensions');
    case Mode of
      PSD_BITMAP: if Depth <> 1 then
                    raise FPImageException.Create('A PSD bitmap has 1 bit per sample');
      PSD_INDEXED: if Depth <> 8 then
                     raise FPImageException.Create('An indexed PSD has 8 bits per sample');
      PSD_GRAYSCALE, PSD_DUOTONE, PSD_MULTICHANNEL, PSD_RGB, PSD_CMYK, PSD_LAB:
        if not (Depth in [8, 16]) then
          raise FPImageException.CreateFmt('Unsupported PSD depth %d',[Depth]);
    else
      raise FPImageException.CreateFmt('Unsupported PSD color mode %d',[Mode]);
    end;
    case Mode of
      PSD_RGB, PSD_LAB: lMinChannels:=3;
      PSD_CMYK: lMinChannels:=4;
    else
      lMinChannels:=1;
    end;
    if Channels < lMinChannels then
      raise FPImageException.CreateFmt('Too few channels (%d) for PSD color mode %d',[Channels, Mode]);
    FChannelCount:=Channels;
    FRowBytes:=(Int64(FWidth)*Depth+7) div 8;
    FPlaneSize:=FRowBytes*FHeight;
    FLineSize:=Int64(FPlaneSize)*Channels;
    if (FLineSize <= 0) or (FLineSize > 2*1024*1024*1024) then
      raise FPImageException.Create('PSD image data too large');
    GetMem(FScanLine,FLineSize);
  end;
end;


function TFPReaderPSD.Sample(aChannel, aX, aY: Integer): Word;
var
  P: PByte;
begin
  P:=FScanLine+aChannel*FPlaneSize+aY*FRowBytes;
  case FHeader.Depth of
    1: if (P[aX shr 3] and ($80 shr (aX and 7))) <> 0 then
         Result:=$FFFF
       else
         Result:=0;
    8: Result:=P[aX]*257;
  else
    Result:=(P[2*aX] shl 8) or P[2*aX+1];
  end;
end;

procedure TFPReaderPSD.ReadResourceBlockData(Img: TFPCustomImage; blockID: Word;
                                             blockName: ShortString; Size: LongWord; Data: Pointer);
var
  ResolutionInfo:TResolutionInfo;
  ResDWord: DWord;

begin
  case blockID of
  PSD_RESN_INFO:begin
            ResolutionInfo :=TResolutionInfo(Data^);
            //MaxM: Do NOT Remove the Casts after BEToN
            Img.ResolutionUnit :=PSDResolutionUnitToResolutionUnit(BEToN(Word(ResolutionInfo.hResUnit)));

            //MaxM: Resolution always recorded in a fixed point implied decimal int32
            //      with 16 bits before point and 16 after (cast as DWord and divide resolution by 2^16)
            ResDWord :=BEToN(DWord(ResolutionInfo.hRes));
            Img.ResolutionX :=ResDWord/65536;
            ResDWord :=BEToN(DWord(ResolutionInfo.vRes));
            Img.ResolutionY :=ResDWord/65536;

            if (Img.ResolutionUnit<>ruNone) and
               (ResolutionInfo.vResUnit<>ResolutionInfo.hResUnit)
            then Case BEToN(Word(ResolutionInfo.vResUnit)) of
                 PSD_RES_INCH: Img.ResolutionY :=Img.ResolutionY/2.54; //Vertical Resolution is in Inch convert to Cm
                 PSD_RES_CM: Img.ResolutionY :=Img.ResolutionY*2.54; //Vertical Resolution is in Cm convert to Inch
                 end;
          end;
  end;
end;

procedure TFPReaderPSD.InternalRead(Stream: TStream; Img: TFPCustomImage);
var
  H: Integer;
  BufSize:Cardinal;
  Encoding:word;
  ContProgress: Boolean;

  procedure ReadResourceBlocks;
  var
     TotalBlockSize,
     pPosition:LongWord;
     blockData,
     curBlock :PPSDResourceBlock;
     curBlockData :PPSDResourceBlockData;
     signature:String[4];
     blockName:ShortString;
     blockID:Word;
     dataSize:LongWord;

  begin
    //MaxM: Do NOT Remove the Casts after BEToN
    Stream.ReadBuffer(TotalBlockSize, 4);
    TotalBlockSize :=BEtoN(DWord(TotalBlockSize));
    if TotalBlockSize > 256*1024*1024 then
      raise FPImageException.Create('PSD resource section too large');
    if TotalBlockSize = 0 then exit;
    GetMem(blockData, TotalBlockSize);
    try
       Stream.ReadBuffer(blockData^, TotalBlockSize);

       pPosition :=0;
       curBlock :=blockData;

       repeat
         // Bounds check: need at least sizeof(TPSDResourceBlock) bytes for the block header
         if pPosition + sizeof(TPSDResourceBlock) > TotalBlockSize then break;

         signature :=curBlock^.Types;

         if (signature=PSD_ResourceSectionSignature) then
         begin
           blockID :=BEtoN(Word(curBlock^.ID));
           blockName :=curBlock^.Name;
           setLength(blockName, curBlock^.NameLen);
           curBlockData :=PPSDResourceBlockData(curBlock);

           Inc(Pointer(curBlockData), sizeof(TPSDResourceBlock));

           if (curBlock^.NameLen>0) then //MaxM: Maybe tested, in all my tests is always 0
           begin
             Inc(Pointer(curBlockData), curBlock^.NameLen);
             if not(Odd(curBlock^.NameLen))
             then Inc(Pointer(curBlockData), 1);
           end;

           pPosition :=Pointer(curBlockData)-Pointer(blockData);
           // Bounds check: need 4 bytes for dataSize
           if pPosition + 4 > TotalBlockSize then break;

           dataSize :=BEtoN(DWord(curBlockData^.Size));
           Inc(Pointer(curBlockData), 4);

           pPosition :=Pointer(curBlockData)-Pointer(blockData);
           // Bounds check: need dataSize bytes for data
           if pPosition + dataSize > TotalBlockSize then break;

           ReadResourceBlockData(Img, blockID, blockName, dataSize, curBlockData);
           Inc(Pointer(curBlockData), dataSize);
         end
         else Inc(Pointer(curBlockData), 1); //skip padding or something went wrong, search for next '8BIM'

         curBlock :=PPSDResourceBlock(curBlockData);
         pPosition :=Pointer(curBlockData)-Pointer(blockData);
       until (pPosition >= TotalBlockSize);

    finally
      FreeMem(blockData, TotalBlockSize);
    end;
  end;

begin
  FScanLine:=nil;
  FPalette:=nil;
  try
  try
    ContProgress:=true;
    Progress(FPimage.psStarting, 0, False, Rect(0,0,0,0), '', ContProgress);
    if not ContProgress then exit;
    // read header
    Stream.ReadBuffer(FHeader, SizeOf(FHeader));
    Progress(FPimage.psRunning, trunc(100.0 * (Stream.position / Stream.size)), False, Rect(0,0,0,0), '', ContProgress);
    if not ContProgress then exit;
    AnalyzeHeader;

    //  color palette
    ReadPalette(Stream);

    if Assigned(OnCreateImage) then
      OnCreateImage(Self,Img);
    Img.SetSize(FWidth,FHeight);

    // Image Resources Section
    ReadResourceBlocks;

    //  mask
    Stream.ReadBuffer(BufSize, SizeOf(BufSize));
    BufSize:=BEtoN(BufSize);
    Stream.Seek(BufSize, soCurrent);
    //  compression type
    Encoding:=0;
    Stream.ReadBuffer(Encoding, SizeOf(Encoding));
    FCompressed:=BEtoN(Encoding) = 1;
    if BEtoN(Encoding)>1 then
      Raise FPImageException.Create('Unknown compression type');
    If FCompressed then
    begin
      SetLength(FLengthOfLine, FHeight * FChannelCount);
      Stream.ReadBuffer(FLengthOfLine[0], 2 * Length(FLengthOfLine));
      FByteRead:=0;
      Progress(FPimage.psRunning, trunc(100.0 * (Stream.position / Stream.size)), False, Rect(0,0,0,0), '', ContProgress);
      if not ContProgress then exit;
      for H := 0 to High(FLengthOfLine) do
        Inc(FByteRead, BEtoN(FLengthOfLine[H]));
    end else
      FByteRead:= FLineSize;

    ReadScanLine(Stream);
    Progress(FPimage.psRunning, trunc(100.0 * (Stream.position / Stream.size)), False, Rect(0,0,0,0), '', ContProgress);
    if not ContProgress then exit;
    WriteScanLine(Img);

    {$ifdef FPC_Debug_Image}
    WriteLn('TFPReaderPSD.InternalRead AAA1 ',Stream.position,' ',Stream.size);
    {$endif}
  except
    on E: EReadError do
      raise FPImageException.Create('PSD data truncated: '+E.Message);
  end;
  finally
    FreeAndNil(FPalette);
    ReAllocMem(FScanLine,0);
  end;
  Progress(FPimage.psEnding, 100, false, Rect(0,0,FWidth,FHeight), '', ContProgress);
end;

function TFPReaderPSD.ReadScanLine(Stream: TStream): boolean;
Var
  P : PByte;
  PEnd : PByte;
  B : Byte;
  I : PtrInt;
  J : integer;
  N : Shortint;
  Count:integer;
  ContProgress: Boolean;
begin
  Result:=false;
  ContProgress:=true;
  If Not Compressed then
    Stream.ReadBuffer(FScanLine^,FLineSize)
  else
    begin
      P:=FScanLine;
      PEnd:=FScanLine + FLineSize;
      i:=FByteRead;
      repeat
        Count:=0;
        N:=0;
        Stream.ReadBuffer(N,1);
        Progress(FPimage.psRunning, trunc(100.0 * (Stream.position / Stream.size)), False, Rect(0,0,0,0), '', ContProgress);
        if not ContProgress then exit;
        dec(i);
        If N = -128 then
        else
        if N < 0 then
        begin
           Count:=-N+1;
           B:=0;
           Stream.ReadBuffer(B,1);
           dec(i);
           For j := 0 to Count-1 do
           begin
             if P >= PEnd then
               raise FPImageException.Create('PSD RLE output overflow');
             P[0]:=B;
             inc(p);
           end;
        end
        else
        begin
           Count:=N+1;
           For j := 0 to Count-1 do
           begin
             Stream.ReadBuffer(B,1);
             if P >= PEnd then
               raise FPImageException.Create('PSD RLE output overflow');
             P[0]:=B;
             inc(p);
             dec(i);
           end;
        end;
      until (i <= 0);
    end;
  Result:=true;
end;

procedure TFPReaderPSD.WriteScanLine(Img: TFPCustomImage);
Var
  Col, Row, Index : Integer;
  C : TFPColor;
  HasAlpha : Boolean;
begin
  case FHeader.Mode of
    PSD_BITMAP:
      for Row:=0 to Img.Height-1 do
        for Col:=0 to Img.Width-1 do
          if Sample(0,Col,Row) <> 0 then
            Img.Colors[Col,Row]:=colBlack
          else
            Img.Colors[Col,Row]:=colWhite;
    PSD_GRAYSCALE, PSD_DUOTONE, PSD_MULTICHANNEL:
      begin
        HasAlpha:=(FChannelCount >= 2) and (FHeader.Mode <> PSD_MULTICHANNEL);
        for Row:=0 to Img.Height-1 do
          for Col:=0 to Img.Width-1 do
          begin
            C.Red:=Sample(0,Col,Row);
            C.Green:=C.Red;
            C.Blue:=C.Red;
            if HasAlpha then
              C.Alpha:=Sample(1,Col,Row)
            else
              C.Alpha:=alphaOpaque;
            Img.Colors[Col,Row]:=C;
          end;
      end;
    PSD_INDEXED:
      for Row:=0 to Img.Height-1 do
        for Col:=0 to Img.Width-1 do
        begin
          Index:=FScanLine[Row*FRowBytes+Col];
          if Index < ThePalette.Count then
            Img.Colors[Col,Row]:=ThePalette[Index]
          else
            Img.Colors[Col,Row]:=colBlack;
        end;
    PSD_RGB:
      begin
        HasAlpha:=FChannelCount >= 4;
        for Row:=0 to Img.Height-1 do
          for Col:=0 to Img.Width-1 do
          begin
            C.Red:=Sample(0,Col,Row);
            C.Green:=Sample(1,Col,Row);
            C.Blue:=Sample(2,Col,Row);
            if HasAlpha then
              C.Alpha:=Sample(3,Col,Row)
            else
              C.Alpha:=alphaOpaque;
            Img.Colors[Col,Row]:=C;
          end;
      end;
    PSD_CMYK:
      // ink is stored inverted; CMYKtoRGB takes the black ink in Alpha
      for Row:=0 to Img.Height-1 do
        for Col:=0 to Img.Width-1 do
        begin
          C.Red:=$FFFF-Sample(0,Col,Row);
          C.Green:=$FFFF-Sample(1,Col,Row);
          C.Blue:=$FFFF-Sample(2,Col,Row);
          C.Alpha:=$FFFF-Sample(3,Col,Row);
          Img.Colors[Col,Row]:=CMYKtoRGB(C);
        end;
    PSD_LAB:
      begin
        HasAlpha:=FChannelCount >= 4;
        for Row:=0 to Img.Height-1 do
          for Col:=0 to Img.Width-1 do
          begin
            C:=TLabA.New(Sample(0,Col,Row)*100/$FFFF,
                         Sample(1,Col,Row)*255/$FFFF-128,
                         Sample(2,Col,Row)*255/$FFFF-128).ToExpandedPixel.ToFPColor;
            if HasAlpha then
              C.Alpha:=Sample(3,Col,Row)
            else
              C.Alpha:=alphaOpaque;
            Img.Colors[Col,Row]:=C;
          end;
      end;
  end;
end;

function TFPReaderPSD.InternalCheck(Stream: TStream): boolean;
var
  OldPos: Int64;
  n: Integer;

begin
  Result:=False;
  if Stream=Nil then
    exit;
  OldPos := Stream.Position;
  try
    n := SizeOf(FHeader);
    Result:=(Stream.Read(FHeader, n) = n)
            and (FHeader.Signature = '8BPS')
            and (BEtoN(FHeader.Version) = 1)
  finally
    Stream.Position := OldPos;
  end;
end;

constructor TFPReaderPSD.Create;
begin
  inherited Create;
end;

initialization
  ImageHandlers.RegisterImageReader ('PSD Format', 'PSD', TFPReaderPSD);
  ImageHandlers.RegisterImageReader ('PDD Format', 'PDD', TFPReaderPSD);

end.

