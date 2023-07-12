{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by the Free Pascal development team

    Tiff reader for fpImage.

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
unit FPReadPSD;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PSDcomn, FPimage;

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

function XYZToRGB(const X, Y, Z :double):TFPColor;
begin
  // ToDo
  Result:=colBlack;
end;

function LabToRGB(const L:TLab):TFPColor;
begin
  // ToDo
  Result:=colBlack;
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
      Red:=I*255;
      Green:=I*255;
      Blue:=I*255;
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
    Stream.Read({%H-}PalBuf, BufSize);
    ContProgress:=true;
    Progress(FPimage.psRunning, 0, False, Rect(0,0,0,0), '', ContProgress);
    if not ContProgress then exit;
    for i:=0 to BufSize div 3 do
    begin
      with c do
      begin
        Red:=PalBuf[I] shl 8;
        Green:=PalBuf[I+(BufSize div 3)] shl 8;
        Blue:=PalBuf[I+(BufSize div 3)* 2] shl 8;
        Alpha:=alphaOpaque;
      end;
      FPalette.Add(c);
    end;
  end;

begin
  Result:=False;
  BufSize:=0;
  Stream.Read(BufSize, SizeOf(BufSize));
  BufSize:=BEtoN(BufSize);

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
begin
  With FHeader do
  begin
    Depth:=BEtoN(Depth);
    if (Signature <> '8BPS') then
      Raise Exception.Create('Unknown/Unsupported PSD image type');
    Channels:=BEtoN(Channels);
    if Channels > 4 then
      FBytesPerPixel:=Depth*4
    else
      FBytesPerPixel:=Depth*Channels;
    Mode:=BEtoN(Mode);
    FWidth:=BEtoN(Columns);
    FHeight:=BEtoN(Rows);
    FChannelCount:=Channels;
    FLineSize:=PtrInt(FHeight)*FWidth*Depth div 8;
    FLineSize:=FLineSize*Channels;
    GetMem(FScanLine,FLineSize);
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
    Stream.Read(TotalBlockSize, 4);
    TotalBlockSize :=BEtoN(DWord(TotalBlockSize));
    GetMem(blockData, TotalBlockSize);
    try
       Stream.Read(blockData^, TotalBlockSize);

       pPosition :=0;
       curBlock :=blockData;

       repeat
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

           dataSize :=BEtoN(DWord(curBlockData^.Size));
           Inc(Pointer(curBlockData), 4);
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
    Stream.Position:=0;
    ContProgress:=true;
    Progress(FPimage.psStarting, 0, False, Rect(0,0,0,0), '', ContProgress);
    if not ContProgress then exit;
    // read header
    Stream.Read(FHeader, SizeOf(FHeader));
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
    Stream.Read(BufSize, SizeOf(BufSize));
    BufSize:=BEtoN(BufSize);
    Stream.Seek(BufSize, soCurrent);
    //  compression type
    Encoding:=0;
    Stream.Read(Encoding, SizeOf(Encoding));
    FCompressed:=BEtoN(Encoding) = 1;
    if BEtoN(Encoding)>1 then
      Raise Exception.Create('Unknown compression type');
    If FCompressed then
    begin
      SetLength(FLengthOfLine, FHeight * FChannelCount);
      Stream.ReadBuffer(FLengthOfLine[0], 2 * Length(FLengthOfLine));
      FByteRead:=0;
      Progress(FPimage.psRunning, trunc(100.0 * (Stream.position / Stream.size)), False, Rect(0,0,0,0), '', ContProgress);
      if not ContProgress then exit;
      for H := 0 to High(FLengthOfLine) do
        Inc(FByteRead, BEtoN(FLengthOfLine[H]));
      if not FHeader.Mode in [ 0, 2] then
        FByteRead := FByteRead * FHeader.Depth div 8;
    end else
      FByteRead:= FLineSize;

    ReadScanLine(Stream);
    Progress(FPimage.psRunning, trunc(100.0 * (Stream.position / Stream.size)), False, Rect(0,0,0,0), '', ContProgress);
    if not ContProgress then exit;
    WriteScanLine(Img);

   {$ifdef FPC_Debug_Image}
    WriteLn('TFPReaderPSD.InternalRead AAA1 ',Stream.position,' ',Stream.size); 
    {$endif}
  finally
    FreeAndNil(FPalette);
    ReAllocMem(FScanLine,0);
  end;
  Progress(FPimage.psEnding, 100, false, Rect(0,0,FWidth,FHeight), '', ContProgress);
end;

function TFPReaderPSD.ReadScanLine(Stream: TStream): boolean;
Var
  P : PByte;
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
  Col : Integer;
  C   : TFPColor;
  P, P1, P2, P3   : PByte;
  Z2  : Longint;
  Row : Integer;
  Lab : TLab;
begin
  C.Alpha:=AlphaOpaque;
  P:=FScanLine;
  Z2:=FHeader.Depth div 8;
  Z2:=Z2 *FHeight*FWidth;
  begin
    case FBytesPerPixel of
      1 : begin
           for Row:=0 to Img.Height-1 do
           begin
             for Col:=0 to Img.Width-1 do
               if (P[col div 8] and (128 shr (col mod 8))) <> 0 then
                 Img.Colors[Col,Row]:=ThePalette[0]
  	       else
                 Img.Colors[Col,Row]:=ThePalette[1];
             inc(P, Img.Width div 8);
           end;
           end;
      8 : begin
           for Row:=0 to Img.Height-1 do
             for Col:=0 to Img.Width-1 do
             begin
               Img.Colors[Col,Row]:=ThePalette[P[0]];
               inc(p);
             end;
          end;
      16 : begin
           for Row:=0 to Img.Height-1 do
             for Col:=0 to Img.Width-1 do
             begin
               Img.Colors[Col,Row]:=ThePalette[BEtoN(PWord(P)^)];
               inc(p,2);
            end;
          end;
      24 :begin
           P1:=P;
           inc(P1,Z2);
           P2:=P;
           inc(P2,Z2*2);
           for Row:=0 to Img.Height-1 do
           for Col:=0 to Img.Width-1 do
           begin
             if (FHeader.Mode =9) then
             begin
               Lab.L:=(P[0]);
               Lab.a:=(P1[0]);
               Lab.b:=(P2[0]);
               C:=LabToRGB(Lab);
             end
             else
              With C do
              begin
                Red:=P[0] or (P[0] shl 8);
                green:=P1[0] or (P1[0] shl 8);
                blue:=P2[0] or (P2[0] shl 8);
                alpha:=alphaOpaque;
              end;
              Inc(P);
              Inc(P1);
              Inc(P2);
//              if (Header.Mode =9) then  C:=XYZtoRGB(C); // Lab color
              Img[col, row] := C;
           end;
          end;
      32 :begin
           P1:=P;
           inc(P1,Z2);
           P2:=P;
           inc(P2,Z2*2);
           P3:=P;
           inc(P3,Z2*3);
           for Row:=0 to Img.Height-1 do
           for Col:=0 to Img.Width-1 do
           begin
             if (FHeader.Mode =4) then
             begin
                 P^ := 255 - P^;
                 P1^ := 255 - P1^;
                 P2^ := 255 - P2^;
                 P3^ := 255 - P3^;
             end;
             C.Red:=P[0] or (P[0] shl 8);
             C.green:=P1[0] or (P1[0] shl 8);
             C.blue:=P2[0] or (P2[0] shl 8);
             C.alpha:=P3[0] or (P3[0] shl 8);
             if (FHeader.Mode =4) then  C:=CMYKtoRGB(C); // CMYK to RGB
             Img[col, row] := C;
             Inc(P);
             Inc(P1);
             Inc(P2);
             Inc(P3);
           end;
          end;
      48 :begin
           P1:=P;
           inc(P1,Z2);
           P2:=P;
           inc(P2,Z2*2);
           C.alpha:=alphaOpaque;
           for Row:=0 to Img.Height-1 do
           for Col:=0 to Img.Width-1 do
           begin
              With C do
              begin
                Red:=BEtoN(PWord(P)^);
                green:=BEtoN(PWord(P1)^);
                blue:=BEtoN(PWord(P2)^);
              end;
              Inc(P,2);
              Inc(P1,2);
              Inc(P2,2);
              Img[col, row] := C;
           end;
          end;
      64 :begin
           P1:=P;
           inc(P1,Z2);
           P2:=P;
           inc(P2,Z2*2);
           P3:=P;
           inc(P3,Z2*3);
           for Row:=0 to Img.Height-1 do
           for Col:=0 to Img.Width-1 do
           begin
             C.Red:=BEtoN(PWord(P)^);
             C.green:=BEtoN(PWord(P1)^);
             C.blue:=BEtoN(PWord(P2)^);
             C.alpha:=BEtoN(PWord(P3)^);
             if (FHeader.Mode =4) then
             begin
                 C.red:=$ffff-C.red;
                 C.green:=$ffff-C.green;
                 C.blue:=$ffff-C.blue;
                 C.alpha:=$ffff-C.alpha;
             end;
             if (FHeader.Mode =4) then  C:=CMYKtoRGB(C); // CMYK to RGB
             Img[col, row] := C;
             Inc(P,2);
             Inc(P1,2);
             Inc(P2,2);
             Inc(P3,2);
           end;
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

