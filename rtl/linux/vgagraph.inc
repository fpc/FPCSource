{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    svgalib implementation of graph unit.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

  const
   InternalDriverName = 'LinuxVGA';

  var SavePtr : Pointer;

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
var
  PhysicalScreen: PGraphicsContext;

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

{ ---------------------------------------------------------------------
    Required procedures
  ---------------------------------------------------------------------}

procedure libvga_savevideostate;

begin
end;

procedure libvga_restorevideostate;

begin
  vga_setmode(0);
end;

const
  BgiColors: array[0..15] of LongInt
    = ($000000, $000020, $002000, $002020,
       $200000, $200020, $202000, $303030,
       $202020, $00003F, $003F00, $003F3F,
       $3F0000, $3F003F, $3F3F00, $3F3F3F);

procedure InitColors;

var
  i: Integer;
begin
  for i:=0 to 15 do
    vga_setpalette(I,BgiColors[i] shr 16,
                     (BgiColors[i] shr 8) and 255,
                      BgiColors[i] and 255)
end;

procedure libvga_initmodeproc;

Var Nrcolors : Longint;

begin
  vga_setmode(IntCurrentMode);
  vga_screenon;
  nrColors:=vga_getcolors;
  if (nrColors=16) or (nrcolors=256) then
    InitColors;
end;

Function ClipCoords (Var X,Y : Integer) : Boolean;
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


procedure libvga_directpixelproc(X,Y: Integer);

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
  vga_setegaColor(Color);
  vga_drawpixel(x, y);
end;

procedure libvga_putpixelproc(X,Y: Integer; Color: Word);
begin
  If Not ClipCoords(X,Y) Then exit;
  vga_setegaColor(Color);
  vga_drawpixel(x, y);
end;

function libvga_getpixelproc (X,Y: Integer): word;
begin
 ClipCoords(X,Y);
 libvga_getpixelproc:=vga_getpixel(x, y);
end;

procedure libvga_clrviewproc;

Var I,Xmax : longint;

begin
  vga_SetegaColor(CurrentBkColor);
  Xmax:=StartXViewPort+ViewWidth-1;
  For i:=StartYViewPort to StartYViewPort+ViewHeight-1 do
    vga_drawline(StartXViewPort,I,Xmax,I);
end;

{ Bitmap utilities }
type
  PBitmap = ^TBitmap;
  TBitmap = record
            Width, Height: Integer;
            Data: record end;
            end;

procedure libvga_putimageproc (X,Y: Integer; var Bitmap; BitBlt: Word);
begin
{
  With TBitMap(BitMap) do
    gl_putbox(x, y, width, height, @Data);
}
end;

procedure libvga_getimageproc (X1,Y1,X2,Y2: Integer; Var Bitmap);
begin
{  with TBitmap(Bitmap) do
    begin
    Width := x2 - x1 + 1;
    Height := y2 - y1 + 1;
    gl_getbox(x1,y1, x2 - x1 + 1, y2 - y1 + 1, @Data);
    end;
}
end;

function  libvga_imagesizeproc (X1,Y1,X2,Y2: Integer): longint;
begin
 libvga_imagesizeproc := SizeOf(TBitmap) + (x2 - x1 + 1) * (y2 - y1 + 1) * PhysicalScreen^.BytesPerPixel;
end;

procedure libvga_hlineproc (x, x2,y : integer);
begin
end;

procedure libvga_vlineproc (x,y,y2: integer);
begin
end;

procedure libvga_patternlineproc (x1,x2,y: integer);
begin
end;

procedure libvga_ellipseproc  (X,Y: Integer;XRadius: word;
  YRadius:word; stAngle,EndAngle: word; fp: PatternLineProc);
begin
end;

procedure libvga_lineproc (X1, Y1, X2, Y2 : Integer);
begin
end;

procedure libvga_getscanlineproc (X1,X2,Y : integer; var data);
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

procedure libvga_setrgbpaletteproc(ColorNum, RedValue, GreenValue, BlueValue: Integer);
begin
  vga_setpalette(ColorNum,RedValue,GreenValue,BlueValue);
end;

procedure libvga_getrgbpaletteproc (ColorNum: integer;
                                    var RedValue, GreenValue, BlueValue: Integer);

Var R,G,B : longint;

begin
  vga_getpalette(ColorNum,R,G,B);
  RedValue:=R;
  GreenValue:=G;
  BlueValue:=B;
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
     SaveVideoState:=libvga_savevideostate;
     RestoreVideoState:=libvga_restorevideostate;
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
           // Pretend we're VGA always.
           DriverNumber := VGA;
           MaxX:=ModeInfo.Width;
           MaxY:=ModeInfo.height;
           MaxColor := ModeInfo.colors;
           PaletteSize := MaxColor;
           HardwarePages := 0;
           // necessary hooks ...
           DirectPutPixel := @libvga_DirectPixelProc;
           GetPixel       := @Libvga_GetPixelProc;
           PutPixel       := @libvga_PutPixelProc;
           SetRGBPalette  := @libvga_SetRGBPaletteProc;
           GetRGBPalette  := @libvga_GetRGBPaletteProc;
           ClearViewPort  := libvga_ClrViewProc;
           PutImage       := @Libvga_PutImageProc;
           GetImage       := @libvga_GetImageProc;
           ImageSize      := @libvga_ImageSizeProc;
           { Add later maybe ?
           SetVisualPage  := SetVisualPageProc;
           SetActivePage  := SetActivePageProc;
           GetScanLine    := @libvga_GetScanLineProc;
           Line           := @libvga_LineProc;
           InternalEllipse:= @libvga_EllipseProc;
           PatternLine    := @libvga_PatternLineProc;
           HLine          := @libvga_HLineProc;
           VLine          := @libvga_VLineProc;
           }
           InitMode       := @libvga_InitModeProc;
           end;
         AddMode(Mode);
         end;
       end;
   end;

{
$Log$
Revision 1.7  2000-02-06 01:48:55  sg
* Fixed the default palette. libsvga works with a RGB range from 0-63, not
  0-255!
* PutPixel fixed (pixels didn't get drawn before)

Revision 1.6  2000/02/03 20:39:58  michael
+ Version using only vgalib

Revision 1.5  2000/01/07 16:41:42  daniel
  * copyright 2000

Revision 1.4  1999/12/20 11:22:38  peter
  * modes moved to interface
  * integer -> smallint

Revision 1.3  1999/12/11 23:41:39  jonas
  * changed definition of getscanlineproc to "getscanline(x1,x2,y:
    integer; var data);" so it can be used by getimage too
  * changed getimage so it uses getscanline
  * changed floodfill, getscanline16 and definitions in Linux
    include files so they use this new format
  + getscanlineVESA256 for 256 color VESA modes (banked)

Revision 1.2  1999/11/08 00:08:43  michael
* Fist working version of svgalib new graph unit
* Initial implementation of ggi new graph unit

Revision 1.1  1999/11/07 16:57:26  michael
+ Start of common graph implementation

}
