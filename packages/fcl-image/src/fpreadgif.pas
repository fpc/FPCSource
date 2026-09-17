{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by the Free Pascal development team

    GIF reader for fpImage.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$IFNDEF FPC_DOTTEDUNITS}
unit FPReadGif;
{$ENDIF FPC_DOTTEDUNITS}

{$mode objfpc}{$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils, FpImage;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils, FPimage;
{$ENDIF FPC_DOTTEDUNITS}

type
  TGifRGB = packed record
    Red, Green, Blue : Byte;
  end;

  TGIFHeader = packed record
    Signature:array[0..2] of AnsiChar;    //* Header Signature (always "GIF") */
    Version:array[0..2] of AnsiChar;      //* GIF format version("87a" or "89a") */
    // Logical Screen Descriptor
    ScreenWidth:word;                 //* Width of Display Screen in Pixels */
    ScreenHeight:word;                //* Height of Display Screen in Pixels */
    Packedbit,                        //* Screen and Color Map Information */
    BackgroundColor,                  //* Background Color Index */
    AspectRatio:byte;                 //* Pixel Aspect Ratio */
  end;

  TGifImageDescriptor = packed record
    Left,              //* X position of image on the display */
    Top,               //* Y position of image on the display */
    Width,             //* Width of the image in pixels */
    Height:word;       //* Height of the image in pixels */
    Packedbit:byte;    //* Image and Color Table Data Information */
  end;

  TGifGraphicsControlExtension = packed record
    BlockSize,         //* Size of remaining fields (always 04h) */
    Packedbit:byte;    //* Method of graphics disposal to use */
    DelayTime:word;    //* Hundredths of seconds to wait	*/
    ColorIndex,        //* Transparent Color Index */
    Terminator:byte;   //* Block Terminator (always 0) */
  end;

  { What becomes of a frame before the next one is drawn }
  TGifDisposal = (gdUnspecified, gdKeep, gdBackground, gdPrevious);

  { One frame of a GIF: the image, where it goes on the logical screen,
    how long it stays there, and what becomes of it before the next one.
    A frame owns its image only when the reader had to make one itself,
    which it does when OnCreateImage gives none. }
  TGifFrame = class
  public
    Img: TFPCustomImage;
    OwnsImage: Boolean;
    Left, Top: Word;
    Width, Height: Word;
    Delay: Word;             //* Hundredths of seconds to wait */
    Disposal: TGifDisposal;
    Transparent: Boolean;
    TransparentIndex: Byte;
    Interlaced: Boolean;
    Palette: TFPPalette;     //* The colours of this frame, owned by it */
    destructor Destroy; override;
  end;

  TFPReaderGif = class;

  TGifCreateCompatibleImgEvent = procedure(Sender: TFPReaderGif;
                                        var NewImage: TFPCustomImage) of object;

  { TFPReaderGif }

  TFPReaderGif = class(TFPCustomImageReader)
  protected
    FHeader: TGIFHeader;
    FDescriptor: TGifImageDescriptor;
    FGraphicsCtrlExt: TGifGraphicsControlExtension;
    FTransparent: Boolean;
    FGraphCtrlExt: Boolean;
    FScanLine: PByte;
    FLineSize: Integer;
    FPalette: TFPPalette;
    FWidth: integer;
    FHeight: Integer;
    FInterlace: boolean;
    FBitsPerPixel: byte;
    FBackground: byte;
    FResolution: byte;
    FOnCreateImage: TGifCreateCompatibleImgEvent;
    FFrames: TFPList;
    FGlobalPalette: TFPPalette;
    FComposite: Boolean;
    FLoopCount: Word;
    FCanvas: TFPMemoryImage;
    FRestore: TFPMemoryImage;
    function GetImages(Index: integer): TGifFrame;
    function GetScreenHeight: Word;
    function GetScreenWidth: Word;
    procedure ReadPalette(Stream: TStream; Size: integer);
    function AnalyzeHeader: Boolean;
    procedure InternalRead(Stream: TStream; Img: TFPCustomImage); override;
    // Reads the frames of the stream. AFirst takes the first of them when
    // it is given, and the rest are read only when AAllFrames.
    procedure ReadFrames(Stream: TStream; AFirst: TFPCustomImage;
      AAllFrames: Boolean); virtual;
    // Reads one frame at the descriptor the stream stands at, and adds it
    // to the frames. AImage takes it when it is given.
    procedure ReadFrame(Stream: TStream; AImage: TFPCustomImage); virtual;
    // Reads the application extension that says how often an animation
    // plays. The stream stands at the size of the first block of it.
    procedure ReadLoopCount(Stream: TStream); virtual;
    // Lays a frame over what the frames before it left, which is what
    // makes every frame an image of the whole screen.
    procedure CompositeFrame(AFrame: TGifFrame; AIndex: integer); virtual;
    function ReadScanLine(Stream: TStream): boolean; virtual;
    function WriteScanLine(Img: TFPCustomImage): Boolean; virtual;
    // Puts the pixels of the frame just read onto an image at that place,
    // leaving the pixels of the transparent index as they are when
    // ASkipTransparent, so that what is under them shows through.
    function DrawScanLine(Img: TFPCustomImage; ALeft, ATop: integer;
      ASkipTransparent: Boolean): Boolean; virtual;
    function InternalCheck (Stream: TStream) : boolean; override;
    function SkipBlock(Stream: TStream): byte;
    class function InternalSize(Stream: TStream): TPoint; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    // Frees the frames read before.
    procedure Clear;
    // Reads every image of the stream. Handle OnCreateImage to have the
    // frames take images of your own; the reader makes what it is not
    // given, and frees those with the frames.
    procedure LoadFromStream(aStream: TStream; AutoClear: boolean = true);
    // Number of frames read.
    function ImageCount: integer;
    // One of the frames read, in the order the file holds them.
    property Images[Index: integer]: TGifFrame read GetImages;
    // Whether a frame is read as the image of the whole screen that it
    // amounts to, with the frames before it showing through where it is
    // transparent. With it off a frame is the picture the file holds,
    // which Left and Top say the place of.
    property Composite: Boolean read FComposite write FComposite;
    // Times an animation plays, nought for over and over again. Nought
    // as well when the file says nothing about it.
    property LoopCount: Word read FLoopCount;
    // Size of the screen the frames are placed on.
    property ScreenWidth: Word read GetScreenWidth;
    property ScreenHeight: Word read GetScreenHeight;
    property Header: TGIFHeader read FHeader;
    property Descriptor: TGifImageDescriptor read FDescriptor;
    property GraphCtrlExt: Boolean read FGraphCtrlExt;
    property GraphicsCtrlExt: TGifGraphicsControlExtension read FGraphicsCtrlExt;
    property Transparent: Boolean read FTransparent;
    property Palette: TFPPalette read FPalette;
    property Width: integer read FWidth;
    property Height: Integer read FHeight;
    property Interlace: boolean read FInterlace;
    property BitsPerPixel: byte read FBitsPerPixel;
    property Background: byte read FBackground;
    property Resolution: byte read FResolution;
    property OnCreateImage: TGifCreateCompatibleImgEvent read FOnCreateImage write FOnCreateImage;
  end;

implementation

{ TGifFrame }

destructor TGifFrame.Destroy;
begin
  if OwnsImage then
    Img.Free;
  Palette.Free;
  inherited Destroy;
end;

{ TFPReaderGif }

procedure TFPReaderGif.ReadPalette(Stream: TStream; Size: integer);
Var
  RGBEntry : TGifRGB;
  I : Integer;
  c : TFPColor;
begin
  FPalette.count := 0;
  For I:=0 To Size-1 Do
  Begin
    Stream.Read(RGBEntry, SizeOf(RGBEntry));
    With c do
    begin
      Red:=RGBEntry.Red or (RGBEntry.Red shl 8);
      Green:=RGBEntry.Green or (RGBEntry.Green shl 8);
      Blue:=RGBEntry.Blue or (RGBEntry.Blue shl 8);
      Alpha:=alphaOpaque;
    end;
    FPalette.Add(C);
  End;
end;

function TFPReaderGif.AnalyzeHeader: Boolean;
var
    C : TFPColor;
begin
  Result:=false;
  With FHeader do
  begin
    if (Signature = 'GIF') and
       ((Version = '87a') or
       (Version = '89a')) then
    else
    Raise Exception.Create('Unknown/Unsupported GIF image type');

    FResolution := Packedbit and $70 shr 5 + 1;
    FBitsPerPixel:=Packedbit and 7 + 1;
    FBackground := BackgroundColor;

    With FDescriptor do
    begin
      fWidth:=Width;
      fHeight:=Height;
      FInterlace := (Packedbit and $40 = $40);
    end;
    FTransparent:= FBackground <> 0;
    if FGraphCtrlExt then
    begin
      FTransparent:=(FGraphicsCtrlExt.Packedbit and $01)<>0;
      If FTransparent then
        FBackground:=FGraphicsCtrlExt.ColorIndex;
    end;
    if (FWidth <= 0) or (FWidth > 65535) or (FHeight <= 0) or (FHeight > 65535) then
      raise Exception.Create('Invalid GIF dimensions');
    if Int64(FWidth) * (Int64(FHeight) + 1) > 256*1024*1024 then
      raise Exception.Create('GIF image data too large');
    FLineSize:=FWidth*(FHeight+1);
    // Every frame of an animation comes through here, so the line of the
    // frame before it is what this grows or shrinks.
    ReAllocMem(FScanLine,FLineSize);
    FillChar(FScanLine^, FLineSize, 0);
    If FTransparent then
    begin
      C:=FPalette.Color[FBackground];
      C.alpha:=alphaTransparent;
      FPalette.Color[FBackground]:=C;
    end;
  end;
  Result:=true;
end;

procedure TFPReaderGif.InternalRead(Stream: TStream; Img: TFPCustomImage);
begin
  ReadFrames(Stream, Img, False);
end;

procedure TFPReaderGif.LoadFromStream(aStream: TStream; AutoClear: boolean = true);
begin
  if AutoClear then
    Clear;
  ReadFrames(aStream, nil, True);
end;

procedure TFPReaderGif.ReadFrames(Stream: TStream; AFirst: TFPCustomImage;
  AAllFrames: Boolean);
var
  Introducer:byte;
  ColorTableSize :Integer;
  ContProgress: Boolean;
begin
  Clear;
  FScanLine:=nil;
  try
    ContProgress:=true;
    Progress(psStarting, 0, False, Rect(0,0,0,0), '', ContProgress);
    if not ContProgress then exit;

    FPalette := TFPPalette.Create(0);
    FGlobalPalette := TFPPalette.Create(0);

    Stream.Position:=0;
    // header
    Stream.Read(FHeader,SizeOf(FHeader));
    Progress(psRunning, trunc(100.0 * (Stream.position / Stream.size)), False, Rect(0,0,0,0), '', ContProgress);
    if not ContProgress then exit;

    // Endian Fix Mantis 8541. Gif is always little endian
    {$IFDEF ENDIAN_BIG}
      with FHeader do
        begin
          ScreenWidth := LEtoN(ScreenWidth);
          ScreenHeight := LEtoN(ScreenHeight);
        end;
    {$ENDIF}
    // global palette
    if (FHeader.Packedbit and $80) <> 0 then
    begin
      ColorTableSize := FHeader.Packedbit and 7 + 1;
      ReadPalette(stream, 1 shl ColorTableSize);
      FGlobalPalette.Copy(FPalette);
    end;

    repeat
      // The control block of a frame stands before it, so what the frame
      // before it was given does not reach one that has none of its own.
      FGraphCtrlExt:=False;
      // skip extensions
      Repeat
        Introducer:=SkipBlock(Stream);
      until (Introducer = $2C) or (Introducer = $3B) or (Stream.Position>=Stream.Size);

      if Introducer <> $2C then
        Break;

      ReadFrame(Stream, AFirst);
      AFirst:=nil;
    until not AAllFrames or (Stream.Position>=Stream.Size);
  finally
    ReAllocMem(FScanLine,0);
  end;
  Progress(FPimage.psEnding, 100, false, Rect(0,0,FWidth,FHeight), '', ContProgress);
end;

procedure TFPReaderGif.ReadFrame(Stream: TStream; AImage: TFPCustomImage);
var
  ColorTableSize: Integer;
  Frame: TGifFrame;
  Img: TFPCustomImage;
begin
  // descriptor
  Stream.Read(FDescriptor, SizeOf(FDescriptor));
  {$IFDEF ENDIAN_BIG}
    with FDescriptor do
      begin
        Left := LEtoN(Left);
        Top := LEtoN(Top);
        Width := LEtoN(Width);
        Height := LEtoN(Height);
      end;
  {$ENDIF}
  // local palette, or the global one as the colours of this frame
  if (FDescriptor.Packedbit and $80) <> 0 then
  begin
    ColorTableSize := FDescriptor.Packedbit and 7 + 1;
    ReadPalette(stream, 1 shl ColorTableSize);
  end
  else
    FPalette.Copy(FGlobalPalette);

  // parse header. It marks the transparent colour of this frame in the
  // palette of it, which is why the global one is kept apart.
  if not AnalyzeHeader then exit;

  Frame:=TGifFrame.Create;
  FFrames.Add(Frame);
  Frame.Left:=FDescriptor.Left;
  Frame.Top:=FDescriptor.Top;
  Frame.Width:=FWidth;
  Frame.Height:=FHeight;
  Frame.Interlaced:=FInterlace;
  Frame.Transparent:=FTransparent;
  Frame.TransparentIndex:=FBackground;
  if FGraphCtrlExt then
  begin
    Frame.Delay:=LEtoN(FGraphicsCtrlExt.DelayTime);
    Frame.Disposal:=TGifDisposal((FGraphicsCtrlExt.Packedbit shr 2) and 7);
  end;
  Frame.Palette:=TFPPalette.Create(0);
  Frame.Palette.Copy(FPalette);

  // create image
  Img:=AImage;
  if Assigned(OnCreateImage) then
    OnCreateImage(Self,Img);
  Frame.OwnsImage:=Img=nil;
  if Img=nil then
    Img:=TFPMemoryImage.Create(0,0);
  Frame.Img:=Img;

  // read pixels
  if not ReadScanLine(Stream) then exit;
  if FComposite then
    CompositeFrame(Frame, FFrames.Count-1)
  else
  begin
    Img.SetSize(FWidth,FHeight);
    if not DrawScanLine(Img, 0, 0, False) then exit;
  end;
end;

procedure TFPReaderGif.CompositeFrame(AFrame: TGifFrame; AIndex: integer);
var
  Previous: TGifFrame;
  X, Y: Integer;
  C: TFPColor;
begin
  if FCanvas=nil then
    FCanvas:=TFPMemoryImage.Create(0,0);
  if (AIndex = 0) then
  begin
    // If no previous, we keep transparent. 
    // TODO: A GIF can name a background colour for it
    FCanvas.SetSize(ScreenWidth,ScreenHeight);
    C:=colTransparent;
    for Y:=0 to FCanvas.Height-1 do
      for X:=0 to FCanvas.Width-1 do
        FCanvas.Colors[X,Y]:=C;
  end
  else
  begin
    // Draw over the previous frame.
    Previous:=Images[AIndex-1];
    case Previous.Disposal of
      gdBackground :
        begin
          C:=colTransparent;
          for Y:=Previous.Top to Previous.Top+Previous.Height-1 do
            for X:=Previous.Left to Previous.Left+Previous.Width-1 do
              if (X < FCanvas.Width) and (Y < FCanvas.Height) then
                FCanvas.Colors[X,Y]:=C;
        end;
      gdPrevious :
        if FRestore<>nil then
          for Y:=0 to FCanvas.Height-1 do
            for X:=0 to FCanvas.Width-1 do
              FCanvas.Colors[X,Y]:=FRestore.Colors[X,Y];
    end;
  end;
  // Keep what is still needed.
  if AFrame.Disposal = gdPrevious then
  begin
    if FRestore=nil then
      FRestore:=TFPMemoryImage.Create(0,0);
    FRestore.SetSize(FCanvas.Width,FCanvas.Height);
    for Y:=0 to FCanvas.Height-1 do
      for X:=0 to FCanvas.Width-1 do
        FRestore.Colors[X,Y]:=FCanvas.Colors[X,Y];
  end;
  DrawScanLine(FCanvas, AFrame.Left, AFrame.Top, True);
  AFrame.Img.SetSize(FCanvas.Width,FCanvas.Height);
  for Y:=0 to FCanvas.Height-1 do
    for X:=0 to FCanvas.Width-1 do
      AFrame.Img.Colors[X,Y]:=FCanvas.Colors[X,Y];
end;

class function TFPReaderGif.InternalSize(Stream:TStream): TPoint;

  function LocalSkipBlock(Stream: TStream): byte;
  var
    Introducer,
    Labels,
    SkipByte : byte;
  begin
    Stream.read(Introducer,1);
    if Introducer = $21 then
    begin
       Stream.read(Labels,1);
       Case Labels of
         $FE, $FF :     // Comment Extension block or Application Extension block
              while Stream.Position < Stream.Size do
              begin
                if Stream.Read(SkipByte, 1) <> 1 then Break;
                if SkipByte = 0 then Break;
                Stream.Seek(SkipByte, soFromCurrent);
              end;
         $F9 :         // Graphics Control Extension block
              begin
                Stream.Seek(SizeOf(TGifGraphicsControlExtension), soFromCurrent);
              end;
         $01 :        // Plain Text Extension block
              begin
                Stream.Read(SkipByte, 1);
                Stream.Seek(SkipByte, soFromCurrent);
                while Stream.Position < Stream.Size do
                begin
                  if Stream.Read(SkipByte, 1) <> 1 then Break;
                  if SkipByte = 0 then Break;
                  Stream.Seek(SkipByte, soFromCurrent);
                end;
              end;
        end;
    end;
    Result:=Introducer;
  end;

var
  hdr: TGIFHeader;
  introducer: Byte;
  b: Byte = 0;
  skipByte: Byte = 0;
  descr: TGifImageDescriptor;
  n: Integer;
begin
  Result := Point(-1, 1);

  Stream.Read(hdr, SizeOf(hdr));

  // Skip global palette if there is one
  if (hdr.Packedbit and $80) <> 0 then
  begin
    n := hdr.Packedbit and 7 + 1;
    Stream.Seek(1 shl n, soFromCurrent);
  end;
  if Stream.Position >= Stream.Size then
    exit;

  // Skip extensions until image descriptor is found ($2C)
  repeat
    introducer := LocalSkipBlock(Stream);
  until (introducer = $2C) or (Stream.Position>=Stream.Size);
  if Stream.Position>=Stream.Size then
    Exit;

  Stream.Read(descr, SizeOf(descr));
  with descr do
  begin
   {$IFDEF ENDIAN_BIG}
    Width := LEtoN(Width);
    Height := LEtoN(Height);
   {$ENDIF}
    Result.X := Width;
    Result.Y := Height;
  end;
end;

function TFPReaderGif.ReadScanLine(Stream: TStream): Boolean;
var
  OldPos,
  UnpackedSize,
  PackedSize:longint;
  I: Integer;
  Data,
  Bits,
  Code: Cardinal;
  SourcePtr: PByte;
  InCode: Cardinal;

  CodeSize: Cardinal;
  CodeMask: Cardinal;
  FreeCode: Cardinal;
  OldCode: Cardinal;
  Prefix: array[0..4095] of Cardinal;
  Suffix,
  Stack: array [0..4095] of Byte;
  StackPointer: PByte;
  DataComp,
  Target: PByte;
  B,
  FInitialCodeSize,
  FirstChar: Byte;
  ClearCode,
  EOICode: Word;
  ContProgress: Boolean;

begin
  DataComp:=nil;
  ContProgress:=true;
  try
    // read dictionary size
    Stream.read(FInitialCodeSize, 1);

    // search end of compressor table
    OldPos:=Stream.Position;
    PackedSize := 0;
    Repeat
      Stream.read(B, 1);
      if B > 0 then
      begin
        inc(PackedSize, B);
        Stream.Seek(B, soFromCurrent);
        CodeMask := (1 shl CodeSize) - 1;
      end;
    until (B = 0)  or (Stream.Position>=Stream.Size);

   { if Stream.Position>=Stream.Size then
      Exit(False); }

    Progress(psRunning, trunc(100.0 * (Stream.position / Stream.size)),
             False, Rect(0,0,0,0), '', ContProgress);
    if not ContProgress then exit(false);

    Getmem(DataComp, PackedSize);
    // read compressor table
    SourcePtr:=DataComp;
    Stream.Position:=OldPos;
    Repeat
      Stream.read(B, 1);
      if B > 0 then
      begin
         Stream.ReadBuffer(SourcePtr^, B);
         Inc(SourcePtr,B);
      end;
    until (B = 0) or (Stream.Position>=Stream.Size);

   { if Stream.Position>=Stream.Size then
       Exit(False); }


    Progress(psRunning, trunc(100.0 * (Stream.position / Stream.size)),
             False, Rect(0,0,0,0), '', ContProgress);
    if not ContProgress then exit(false);

    SourcePtr:=DataComp;
    Target := FScanLine;
    CodeSize := FInitialCodeSize + 1;
    ClearCode := 1 shl FInitialCodeSize;
    EOICode := ClearCode + 1;
    FreeCode := ClearCode + 2;
    OldCode := 4096;
    CodeMask := (1 shl CodeSize) - 1;
    UnpackedSize:=FWidth * FHeight;
    for I := 0 to ClearCode - 1 do
    begin
      Prefix[I] := 4096;
      Suffix[I] := I;
    end;
    StackPointer := @Stack;
    FirstChar := 0;
    Data := 0;
    Bits := 0;
    // LZW decompression gif
    while (UnpackedSize > 0) and (PackedSize > 0) do
    begin
      Inc(Data, SourcePtr^ shl Bits);
      Inc(Bits, 8);
      while Bits >= CodeSize do
      begin
        Code := Data and CodeMask;
        Data := Data shr CodeSize;
        Dec(Bits, CodeSize);
        if Code = EOICode then Break;
        if Code = ClearCode then
        begin
          CodeSize := FInitialCodeSize + 1;
          CodeMask := (1 shl CodeSize) - 1;
          FreeCode := ClearCode + 2;
          OldCode := 4096;
          Continue;
        end;
        if Code > FreeCode then Break;
        if OldCode = 4096 then
        begin
          FirstChar := Suffix[Code];
          Target^ := FirstChar;
          Inc(Target);
          Dec(UnpackedSize);
          OldCode := Code;
          Continue;
        end;
        InCode := Code;
        if Code = FreeCode then
        begin
          StackPointer^ := FirstChar;
          Inc(StackPointer);
          Code := OldCode;
        end;
        while Code > ClearCode do
        begin
          StackPointer^ := Suffix[Code];
          Inc(StackPointer);
          Code := Prefix[Code];
        end;
        FirstChar := Suffix[Code];
        StackPointer^ := FirstChar;
        Inc(StackPointer);
        Prefix[FreeCode] := OldCode;
        Suffix[FreeCode] := FirstChar;
        if (FreeCode = CodeMask) and
           (CodeSize < 12) then
        begin
          Inc(CodeSize);
          CodeMask := (1 shl CodeSize) - 1;
        end;
        if FreeCode < 4095 then Inc(FreeCode);
        OldCode := InCode;
        repeat
          Dec(StackPointer);
          Target^ := StackPointer^;
          Inc(Target);
          Dec(UnpackedSize);
        until StackPointer = @Stack;
      end;
      Inc(SourcePtr);
      Dec(PackedSize);
    end;
    Progress(psRunning, trunc(100.0 * (Stream.position / Stream.size)),
             False, Rect(0,0,0,0), '', ContProgress);
    if not ContProgress then exit(false);
  finally
    if DataComp<>nil then
      FreeMem(DataComp);
  end;
  Result:=true;
end;

function TFPReaderGif.WriteScanLine(Img: TFPCustomImage): Boolean;
begin
  Result:=DrawScanLine(Img, 0, 0, False);
end;

function TFPReaderGif.DrawScanLine(Img: TFPCustomImage; ALeft, ATop: integer;
  ASkipTransparent: Boolean): Boolean;
Var
  Row, X, Y : Integer;
  Pass, Every : byte;
  P : PByte;

  procedure PutRow;
  var
    I : Integer;
  begin
    Y:=ATop+Row;
    for I:=0 to FWidth-1 do
    begin
      X:=ALeft+I;
      if (X >= 0) and (Y >= 0) and (X < Img.Width) and (Y < Img.Height)
         and not (ASkipTransparent and FTransparent and (P^ = FBackground)) then
        Img.Colors[X,Y]:=FPalette[P^];
      Inc(P);
    end;
  end;

begin
  Result:=false;
  P:=FScanLine;
  If FInterlace then
  begin
    For Pass := 1 to 4 do
    begin
      Case Pass of
         1 : begin
               Row := 0;
               Every := 8;
             end;
         2 : begin
               Row := 4;
               Every := 8;
             end;
         3 : begin
               Row := 2;
               Every := 4;
             end;
         4 : begin
               Row := 1;
               Every := 2;
             end;
        end;
      while Row < FHeight do
      begin
        PutRow;
        Inc(Row, Every);
      end;
    end;
  end
  else
    for Row:=0 to FHeight-1 do
      PutRow;
  Result:=true;
end;

procedure TFPReaderGif.ReadLoopCount(Stream: TStream);
var
  BlockSize : byte;
  Identifier : string[11];
  Data : packed array[0..2] of byte;
begin
  Stream.Read(BlockSize,1);
  if BlockSize = 11 then
  begin
    SetLength(Identifier,11);
    if Stream.Read(Identifier[1],11) <> 11 then Exit;
    if Identifier = 'NETSCAPE2.0' then
    begin
      Stream.Read(BlockSize,1);
      if (BlockSize >= 3) and (Stream.Read(Data,3) = 3) then
        FLoopCount:=Data[1] or (Data[2] shl 8);
      if BlockSize > 3 then
        Stream.Seek(BlockSize-3, soFromCurrent);
    end;
  end
  else if BlockSize > 0 then
    Stream.Seek(BlockSize, soFromCurrent);
  // the blocks that are left of the extension, up to its terminator
  while Stream.Position < Stream.Size do
  begin
    if Stream.Read(BlockSize, 1) <> 1 then Break;
    if BlockSize = 0 then Break;
    Stream.Seek(BlockSize, soFromCurrent);
  end;
end;

function TFPReaderGif.InternalCheck(Stream: TStream): boolean;

var
  OldPos: Int64;
  n: Int64;

begin
  Result:=False;
  if Stream = nil then
    exit;
  OldPos:=Stream.Position;
  try
    n := SizeOf(FHeader);
    Result:=(Stream.Read(FHeader,n)=n)
            and (FHeader.Signature = 'GIF')
            and ((FHeader.Version = '87a') or (FHeader.Version = '89a'));
  finally
    Stream.Position := OldPos;
  end;
end;

function TFPReaderGif.SkipBlock(Stream: TStream): byte;
var
  Introducer,
  Labels,
  SkipByte : byte;
begin
  Stream.read(Introducer,1);
  if Introducer = $21 then
  begin
     Stream.read(Labels,1);
     Case Labels of
       $FF :         // Application Extension block
            ReadLoopCount(Stream);
       $FE :         // Comment Extension block
            while Stream.Position < Stream.Size do
            begin
              if Stream.Read(SkipByte, 1) <> 1 then Break;
              if SkipByte = 0 then Break;
              Stream.Seek(SkipByte, soFromCurrent);
            end;
       $F9 :         // Graphics Control Extension block
            begin
              Stream.Read(FGraphicsCtrlExt, SizeOf(FGraphicsCtrlExt));
              FGraphCtrlExt:=True;
            end;
       $01 :        // Plain Text Extension block
            begin
              Stream.Read(SkipByte, 1);
              Stream.Seek(SkipByte, soFromCurrent);
              while Stream.Position < Stream.Size do
              begin
                if Stream.Read(SkipByte, 1) <> 1 then Break;
                if SkipByte = 0 then Break;
                Stream.Seek(SkipByte, soFromCurrent);
              end;
            end;
      end;
  end;
  Result:=Introducer;
end;

constructor TFPReaderGif.Create;
begin
  inherited Create;
  FFrames:=TFPList.Create;
  FComposite:=True;
end;

destructor TFPReaderGif.Destroy;
begin
  Clear;
  FFrames.Free;
  inherited Destroy;
end;

procedure TFPReaderGif.Clear;
var
  I : Integer;
begin
  for I:=0 to FFrames.Count-1 do
    TGifFrame(FFrames[I]).Free;
  FFrames.Clear;
  FreeAndNil(FPalette);
  FreeAndNil(FGlobalPalette);
  FreeAndNil(FCanvas);
  FreeAndNil(FRestore);
  FLoopCount:=0;
end;

function TFPReaderGif.ImageCount: integer;
begin
  Result:=FFrames.Count;
end;

function TFPReaderGif.GetImages(Index: integer): TGifFrame;
begin
  Result:=TGifFrame(FFrames[Index]);
end;

function TFPReaderGif.GetScreenWidth: Word;
begin
  Result:=FHeader.ScreenWidth;
  if Result = 0 then
    Result:=FWidth;
end;

function TFPReaderGif.GetScreenHeight: Word;
begin
  Result:=FHeader.ScreenHeight;
  if Result = 0 then
    Result:=FHeight;
end;

initialization
  ImageHandlers.RegisterImageReader ('GIF Graphics', 'gif', TFPReaderGif);
end.

