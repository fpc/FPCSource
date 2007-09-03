{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Florian Klaempfl

    This file implements the linux GGI support for the graph unit

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit Graph;
interface

{$i graphh.inc}

Const
  { Supported modes }
  {(sg) GTEXT deactivated because we need mode #0 as default mode}
  {GTEXT             = 0;                 Compatible with VGAlib v1.2 }
  G320x200x16       = 1;
  G640x200x16       = 2;
  G640x350x16       = 3;
  G640x480x16       = 4;
  G320x200x256      = 5;
  G320x240x256      = 6;
  G320x400x256      = 7;
  G360x480x256      = 8;
  G640x480x2        = 9;

  G640x480x256      = 10;
  G800x600x256      = 11;
  G1024x768x256     = 12;

  G1280x1024x256    = 13;   { Additional modes. }

  G320x200x32K      = 14;
  G320x200x64K      = 15;
  G320x200x16M      = 16;
  G640x480x32K      = 17;
  G640x480x64K      = 18;
  G640x480x16M      = 19;
  G800x600x32K      = 20;
  G800x600x64K      = 21;
  G800x600x16M      = 22;
  G1024x768x32K     = 23;
  G1024x768x64K     = 24;
  G1024x768x16M     = 25;
  G1280x1024x32K    = 26;
  G1280x1024x64K    = 27;
  G1280x1024x16M    = 28;

  G800x600x16       = 29;
  G1024x768x16      = 30;
  G1280x1024x16     = 31;

  G720x348x2        = 32;               { Hercules emulation mode }

  G320x200x16M32    = 33;       { 32-bit per pixel modes. }
  G640x480x16M32    = 34;
  G800x600x16M32    = 35;
  G1024x768x16M32   = 36;
  G1280x1024x16M32  = 37;

  { additional resolutions }
  G1152x864x16      = 38;
  G1152x864x256     = 39;
  G1152x864x32K     = 40;
  G1152x864x64K     = 41;
  G1152x864x16M     = 42;
  G1152x864x16M32   = 43;

  G1600x1200x16     = 44;
  G1600x1200x256    = 45;
  G1600x1200x32K    = 46;
  G1600x1200x64K    = 47;
  G1600x1200x16M    = 48;
  G1600x1200x16M32  = 49;


implementation

uses
  termio,x86;

const
  InternalDriverName = 'LinuxVGA';

{$i graph.inc}

  type
    PByte = ^Byte;
    PLongInt = ^LongInt;

    PByteArray = ^TByteArray;
    TByteArray = array [0..MAXINT - 1] of Byte;



{ ---------------------------------------------------------------------
   SVGA bindings.

  ---------------------------------------------------------------------}

{  Link with VGA, gl and c libraries }
{$linklib vga}
{$linklib c}

Const
  { Text }

  WRITEMODE_OVERWRITE = 0;
  WRITEMODE_MASKED    = 1;
  FONT_EXPANDED       = 0;
  FONT_COMPRESSED     = 2;

 { Types }
 type
   pvga_modeinfo = ^vga_modeinfo;
   vga_modeinfo = record
     width,
     height,
     bytesperpixel,
     colors,
     linewidth,          { scanline width in bytes }
     maxlogicalwidth,    { maximum logical scanline width }
     startaddressrange,  { changeable bits set }
     maxpixels,          { video memory / bytesperpixel }
     haveblit,           { mask of blit functions available }
     flags: Longint;     { other flags }
    { Extended fields: }
     chiptype,           { Chiptype detected }
     memory,             { videomemory in KB }
     linewidth_unit: Longint;    { Use only a multiple of this as parameter for                                   set_displaystart }
     linear_aperture: PChar;     { points to mmap secondary mem aperture of card }
     aperture_size: Longint;     { size of aperture in KB if size>=videomemory.}

     set_aperture_page: procedure (page: Longint);
            { if aperture_size<videomemory select a memory page }
     extensions: Pointer;        { points to copy of eeprom for mach32 }
            { depends from actual driver/chiptype.. etc. }
     end;

  PGraphicsContext = ^TGraphicsContext;
  TGraphicsContext = record
                       ModeType: Byte;
                       ModeFlags: Byte;
                       Dummy: Byte;
                       FlipPage: Byte;
                       Width: LongInt;
                       Height: LongInt;
                       BytesPerPixel: LongInt;
                       Colors: LongInt;
                       BitsPerPixel: LongInt;
                       ByteWidth: LongInt;
                       VBuf: pointer;
                       Clip: LongInt;
                       ClipX1: LongInt;
                       ClipY1: LongInt;
                       ClipX2: LongInt;
                       ClipY2: LongInt;
                       ff: pointer;
                     end;

Const
  GLASTMODE         = 49;
  ModeNames : Array[0..GLastMode] of string [18] =
   ('Text',
    'G320x200x16',
    'G640x200x16',
    'G640x350x16',
    'G640x480x16',
    'G320x200x256',
    'G320x240x256',
    'G320x400x256',
    'G360x480x256',
    'G640x480x2',
    'G640x480x256',
    'G800x600x256',
    'G1024x768x256',
    'G1280x1024x256',
    'G320x200x32K',
    'G320x200x64K',
    'G320x200x16M',
    'G640x480x32K',
    'G640x480x64K',
    'G640x480x16M',
    'G800x600x32K',
    'G800x600x64K',
    'G800x600x16M',
    'G1024x768x32K',
    'G1024x768x64K',
    'G1024x768x16M',
    'G1280x1024x32K',
    'G1280x1024x64K',
    'G1280x1024x16M',
    'G800x600x16',
    '1024x768x16',
    '1280x1024x16',
    'G720x348x2',
    'G320x200x16M32',
    'G640x480x16M32',
    'G800x600x16M32',
    'G1024x768x16M32',
    'G1280x1024x16M32',
    'G1152x864x16',
    'G1152x864x256',
    'G1152x864x32K',
    'G1152x864x64K',
    'G1152x864x16M',
    'G1152x864x16M32',
    'G1600x1200x16',
    'G1600x1200x256',
    'G1600x1200x32K',
    'G1600x1200x64K',
    'G1600x1200x16M',
    'G1600x1200x16M32');
{var
   PhysicalScreen: PGraphicsContext; }

 { vga functions }
 Function vga_init: Longint; Cdecl; External;
 Function vga_hasmode(mode: Longint): Boolean; Cdecl; External;
 Function vga_getmodeinfo(mode: Longint): pvga_modeinfo; Cdecl; External;
 Function vga_setmode(mode: Longint): Longint; Cdecl; External;
 Function vga_getcolors: Longint; cdecl;external;
 Function vga_setpalette(index: Longint; red: Longint; green: Longint; blue: Longint) : longint; cdecl;external;
 Function vga_getpalette(index: Longint; var red: Longint; var green: Longint; var blue: Longint): Longint; cdecl;external;
 Function vga_setegacolor(Color: Longint) : longint; cdecl;external;
 Function vga_setcolor(color: Longint): Longint; cdecl;external;
 Function vga_drawpixel(x, y: Longint): Longint; cdecl;external;
 Function vga_getpixel(x, y: Longint): Longint; cdecl;external;
 Function vga_drawline(x1, y1, x2, y2: Longint): Longint; cdecl;external;
 function vga_screenoff: Longint; Cdecl; External;
 function vga_screenon: Longint;  Cdecl; External;
 function vga_getgraphmem: PByteArray; cdecl; external;



var
  OldIO : TermIos;
Procedure SetRawMode(b:boolean);
Var
  Tio : Termios;
Begin
  if b then
   begin
     TCGetAttr(1,Tio);
     OldIO:=Tio;
     CFMakeRaw(Tio);
   end
  else
   Tio:=OldIO;
  TCSetAttr(1,TCSANOW,Tio);
End;


{ ---------------------------------------------------------------------
    Required procedures
  ---------------------------------------------------------------------}

{$INCLUDE graph16.inc}       // Include graphic functions for 16 colours modes

var
  LastColor: smallint;   {Cache the last set color to improve speed}

procedure SetEGAColor(color: smallint);
begin
  if color <> LastColor then begin
    LastColor := color;
    if maxcolor = 16 then
      vga_setegacolor(color)
    else vga_setcolor(color);
  end;
end;


procedure libvga_savevideostate;
begin
end;

procedure libvga_restorevideostate;
begin
  vga_setmode(0);
end;

{
const
  BgiColors: array[0..15] of LongInt
    = ($000000, $000020, $002000, $002020,
       $200000, $200020, $202000, $303030,
       $202020, $00003F, $003F00, $003F3F,
       $3F0000, $3F003F, $3F3F00, $3F3F3F);
}

procedure InitColors(nrColors: longint);

var
  i: smallint;
begin
  for i:=0 to nrColors do
    vga_setpalette(I,DefaultColors[i].red shr 2,
      DefaultColors[i].green shr 2,DefaultColors[i].blue shr 2)
end;

procedure libvga_initmodeproc;

Var Nrcolors : Longint;

begin
  vga_setmode(IntCurrentMode);
  vga_screenon;
  VidMem := vga_getgraphmem;
  nrColors:=vga_getcolors;
  if (nrColors=16) or (nrcolors=256) then
    InitColors(nrColors);
  SetRawMode(True);
end;

Function ClipCoords (Var X,Y : smallint) : Boolean;
{ Adapt to viewport, return TRUE if still in viewport,
  false if outside viewport}

begin
  X:= X + StartXViewPort;
  Y:= Y + StartYViewPort;
  ClipCoords:=Not ClipPixels;
  if ClipPixels then
    Begin
    ClipCoords:=(X < StartXViewPort) or (X > (StartXViewPort + ViewWidth));
    ClipCoords:=ClipCoords or
               ((Y < StartYViewPort) or (Y > (StartYViewPort + ViewHeight)));
    ClipCoords:=Not ClipCoords;
    end;
end;


procedure libvga_directpixelproc(X,Y: smallint);

Var Color : Word;

begin
  case CurrentWriteMode of
    XORPut:
      begin
      { getpixel wants local/relative coordinates }
      Color := GetPixel(x-StartXViewPort,y-StartYViewPort);
      Color := CurrentColor Xor Color;
      end;
    OrPut:
      begin
      { getpixel wants local/relative coordinates }
      Color := GetPixel(x-StartXViewPort,y-StartYViewPort);
      Color := CurrentColor Or Color;
      end;
    AndPut:
      begin
      { getpixel wants local/relative coordinates }
      Color := GetPixel(x-StartXViewPort,y-StartYViewPort);
      Color := CurrentColor And Color;
      end;
    NotPut:
      begin
      Color := Not Color;
      end
  else
    Color:=CurrentColor;
  end;
  SetEGAColor(Color);
  vga_drawpixel(x, y);
end;

procedure libvga_putpixelproc(X,Y: smallint; Color: Word);
begin
  If Not ClipCoords(X,Y) Then exit;
  SetEGAColor(Color);
  vga_drawpixel(x, y);
end;

function libvga_getpixelproc (X,Y: smallint): word;
begin
 ClipCoords(X,Y);
 libvga_getpixelproc:=vga_getpixel(x, y);
end;

procedure libvga_clrviewproc;

Var I,Xmax : longint;

begin
  SetEGAColor(CurrentBkColor);
  Xmax:=StartXViewPort+ViewWidth-1;
  For i:=StartYViewPort to StartYViewPort+ViewHeight-1 do
    vga_drawline(StartXViewPort,I,Xmax,I);
  { reset coordinates }
  CurrentX := 0;
  CurrentY := 0;
end;

{ Bitmap utilities }
{type
  PBitmap = ^TBitmap;
  TBitmap = record
            Width, Height: smallint;
            Data: record end;
            end;
}

procedure libvga_putimageproc (X,Y: smallint; var Bitmap; BitBlt: Word);
begin
{
  With TBitMap(BitMap) do
    gl_putbox(x, y, width, height, @Data);
}
end;

procedure libvga_getimageproc (X1,Y1,X2,Y2: smallint; Var Bitmap);
begin
{  with TBitmap(Bitmap) do
    begin
    Width := x2 - x1 + 1;
    Height := y2 - y1 + 1;
    gl_getbox(x1,y1, x2 - x1 + 1, y2 - y1 + 1, @Data);
    end;
}
end;

{
function  libvga_imagesizeproc (X1,Y1,X2,Y2: smallint): longint;
begin
 libvga_imagesizeproc := SizeOf(TBitmap) + (x2 - x1 + 1) * (y2 - y1 + 1) * PhysicalScreen^.BytesPerPixel;

end;
}
procedure libvga_hlineproc (x, x2,y : smallint);
begin
end;

procedure libvga_vlineproc (x,y,y2: smallint);
begin
end;

procedure libvga_patternlineproc (x1,x2,y: smallint);
begin
end;

procedure libvga_ellipseproc  (X,Y: smallint;XRadius: word;
  YRadius:word; stAngle,EndAngle: word; fp: PatternLineProc);
begin
end;

procedure libvga_lineproc (X1, Y1, X2, Y2 : smallint);
begin
end;

procedure libvga_getscanlineproc (X1,X2,Y : smallint; var data);
begin
end;

procedure libvga_setactivepageproc (page: word);
begin
end;

procedure libvga_setvisualpageproc (page: word);
begin
end;


procedure libvga_savestateproc;
begin
end;

procedure libvga_restorestateproc;
begin
end;

procedure libvga_setrgbpaletteproc(ColorNum, RedValue, GreenValue, BlueValue: smallint);
begin
  vga_setpalette(ColorNum,RedValue shr 2,GreenValue shr 2,BlueValue shr 2);
end;

procedure libvga_getrgbpaletteproc (ColorNum: smallint;
                                    var RedValue, GreenValue, BlueValue: smallint);

Var R,G,B : longint;

begin
  vga_getpalette(ColorNum,R,G,B);
  RedValue:=R * 255 div 63;
  GreenValue:=G * 255 div 63;
  BlueValue:=B * 255 div 63;
end;

{************************************************************************}
{*                       General routines                               *}
{************************************************************************}

 procedure CloseGraph;
 Begin
    If not isgraphmode then
      begin
        _graphresult := grnoinitgraph;
        exit
      end;
    SetRawMode(False);
    RestoreVideoState;
    isgraphmode := false;
 end;

  function QueryAdapterInfo:PModeInfo;
  { This routine returns the head pointer to the list }
  { of supported graphics modes.                      }
  { Returns nil if no graphics mode supported.        }
  { This list is READ ONLY!                           }
   var
    mode: TModeInfo;
    modeinfo : vga_modeinfo;
    i : longint;

   begin
     QueryAdapterInfo := ModeList;
     { If the mode listing already exists... }
     { simply return it, without changing    }
     { anything...                           }
     if assigned(ModeList) then
       exit;
     SaveVideoState:=@libvga_savevideostate;
     RestoreVideoState:=@libvga_restorevideostate;
     vga_init;
     For I:=0 to GLastMode do
       begin
       If vga_hasmode(I) then
         begin
         ModeInfo:=vga_getmodeinfo(i)^;
         InitMode(Mode);
         With Mode do
           begin
           ModeNumber:=I;
           ModeName:=ModeNames[i];
           // Pretend we are VGA always.
           DriverNumber := VGA;
           // MaxX is number of pixels in X direction - 1
           MaxX:=ModeInfo.Width-1;
           // same for MaxY
           MaxY:=ModeInfo.Height-1;
           YAspect:=10000;
           if ((MaxX+1)*35=(MaxY+1)*64) then
             XAspect:=7750
           else if ((MaxX+1)*20=(MaxY+1)*64) then
             XAspect:=4500
           else if ((MaxX+1)*40=(MaxY+1)*64) then
             XAspect:=8333
           else { assume 4:3 }
             XAspect:=10000;
           MaxColor := ModeInfo.colors;
           PaletteSize := MaxColor;
           HardwarePages := 0;
           // necessary hooks ...
           if (MaxColor = 16) and
             (LongInt(ModeInfo.Width) * LongInt(ModeInfo.Height) < 65536*4*2) then
           begin
             // Use optimized graphics routines for 4 bit EGA/VGA modes
             ScrWidth := ModeInfo.Width div 8;
             DirectPutPixel := @DirectPutPixel16;
             PutPixel := @PutPixel16;
             GetPixel := @GetPixel16;
             HLine := @HLine16;
             VLine := @VLine16;
             GetScanLine := @GetScanLine16;
           end
           else
           begin
             DirectPutPixel := @libvga_DirectPixelProc;
             GetPixel       := @libvga_GetPixelProc;
             PutPixel       := @libvga_PutPixelProc;
             { May be implemented later:
             HLine          := @libvga_HLineProc;
             VLine          := @libvga_VLineProc;
             GetScanLine    := @libvga_GetScanLineProc;}
             ClearViewPort  := @libvga_ClrViewProc;
           end;
           SetRGBPalette  := @libvga_SetRGBPaletteProc;
           GetRGBPalette  := @libvga_GetRGBPaletteProc;
           { These are not really implemented yet:
           PutImage       := @libvga_PutImageProc;
           GetImage       := @libvga_GetImageProc;}
{          If you use the default getimage/putimage, you also need the default
           imagesize! (JM)
            ImageSize      := @libvga_ImageSizeProc; }
           { Add later maybe ?
           SetVisualPage  := SetVisualPageProc;
           SetActivePage  := SetActivePageProc;
           Line           := @libvga_LineProc;
           InternalEllipse:= @libvga_EllipseProc;
           PatternLine    := @libvga_PatternLineProc;
           }
           InitMode       := @libvga_InitModeProc;
           end;
         AddMode(Mode);
         end;
       end;
   end;

initialization
  InitializeGraph;
end.
