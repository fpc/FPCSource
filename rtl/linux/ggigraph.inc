{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999 by the Free Pascal development team

    GGI implementation of graph unit.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$LINKLIB c}
{$PACKRECORDS C}

const
  InternalDriverName = 'LinuxGGI';

var
  SavePtr: Pointer;

{ ---------------------------------------------------------------------
   GGI bindings  [(c) 1999 Sebastian Guenther]
  ---------------------------------------------------------------------}
const
  GLASTMODE         = 49;
  ModeNames: array[0..GLastMode] of PChar =
   ('[]',                       {Let GGI choose a default mode}
    'S320x200[GT_4BIT]',
    'S640x200[GT_4BIT]',
    'S640x350[GT_4BIT]',
    'S640x480[GT_4BIT]',
    'S320x200[GT_8BIT]',
    'S320x240[GT_8BIT]',
    'S320x400[GT_8BIT]',
    'S360x480[GT_8BIT]',
    'S640x480x[GT_1BIT]',
    'S640x480[GT_8BIT]',
    'S800x600[GT_8BIT]',
    'S1024x768[GT_8BIT]',
    'S1280x1024[GT_8BIT]',
    'S320x200[GT_15BIT]',
    'S320x200[GT_16BIT]',
    'S320x200[GT_24BIT]',
    'S640x480[GT_15BIT]',
    'S640x480[GT_16BIT]',
    'S640x480[GT_24BIT]',
    'S800x600[GT_15BIT]',
    'S800x600[GT_16BIT]',
    'S800x600[GT_24BIT]',
    'S1024x768[GT_15BIT]',
    'S1024x768[GT_16BIT]',
    'S1024x768[GT_24BIT]',
    'S1280x1024[GT_15BIT]',
    'S1280x1024[GT_16BIT]',
    'S1280x1024[GT_24BIT]',
    'S800x600[GT_4BIT]',
    'S1024x768[GT_4BIT]',
    'S1280x1024[GT_4BIT]',
    'S720x348x[GT_1BIT]',
    'S320x200[GT_32BIT]',
    'S640x480[GT_32BIT]',
    'S800x600[GT_32BIT]',
    'S1024x768[GT_32BIT]',
    'S1280x1024[GT_32BIT]',
    'S1152x864[GT_4BIT]',
    'S1152x864[gt_8BIT]',
    'S1152x864[GT_15BIT]',
    'S1152x864[GT_16BIT]',
    'S1152x864[GT_24BIT]',
    'S1152x864[GT_32BIT]',
    'S1600x1200[GT_4BIT]',
    'S1600x1200[gt_8BIT]',
    'S1600x1200[GT_15BIT]',
    'S1600x1200[GT_16BIT]',
    'S1600x1200[GT_24BIT]',
    'S1600x1200[GT_32BIT]');

type
  TGGIVisual = Pointer;
  TGGIResource = Pointer;
  TGGICoord = record
    x, y: SmallInt;
  end;
  TGGIPixel = LongWord;
  PGGIColor = ^TGGIColor;
  TGGIColor = record
    r, g, b, a: Word;
  end;
  PGGIClut = ^TGGIClut;
  TGGIClut = record
    size: SmallInt;
    data: PGGIColor;
  end;
  TGGIGraphType = LongWord;
  TGGIAttr = LongWord;
  TGGIMode = record                     // requested by user and changed by driver
    Frames: LongInt;                    // frames needed
    Visible: TGGICoord;                 // vis. pixels, may change slightly
    Virt: TGGICoord;                    // virtual pixels, may change
    Size: TGGICoord;                    // size of visible in mm
    GraphType: TGGIGraphType;           // which mode ?
    dpp: TGGICoord;                     // dots per pixel
  end;

const
  libggi = 'ggi';
function  ggiInit: Longint; cdecl; external libggi;
procedure ggiExit; cdecl; external libggi;
function  ggiOpen(display: PChar; args: Array of const): TGGIVisual; cdecl; external libggi;
function  ggiClose(vis: TGGIVisual): Longint; cdecl; external libggi;
function  ggiParseMode(s: PChar; var m: TGGIMode): Longint; cdecl; external libggi;
function  ggiSetMode(visual: TGGIVisual; var tm: TGGIMode): Longint; cdecl; external libggi;
function  ggiGetMode(visual: TGGIVisual; var tm: TGGIMode): Longint; cdecl; external libggi;
function  ggiCheckMode(visual: TGGIVisual; var tm: TGGIMode): Longint; cdecl; external libggi;

function  ggiMapColor(vis: TGGIVisual; Color: TGGIColor): TGGIPixel; cdecl; external libggi;

function  ggiPutPixel(vis: TGGIVisual; x, y: Longint; pixel: TGGIPixel): Longint; cdecl; external libggi;
function  ggiGetPixel(vis: TGGIVisual; x, y: Longint; var pixel: TGGIPixel): Longint; cdecl; external libggi;
function  ggiDrawBox(vis: TGGIVisual; x, y, w, h: Longint): Longint; cdecl; external libggi;
function  ggiPutBox(vis: TGGIVisual; x, y, w, h: Longint; var buffer): Longint; cdecl; external libggi;
function  ggiGetBox(vis: TGGIVisual; x, y, w, h: Longint; var buffer): Longint; cdecl; external libggi;

function  ggiGetPalette(vis: TGGIVisual; s, len: Longint; var cmap: TGGIColor): Longint; cdecl; external libggi;
function  ggiSetPalette(vis: TGGIVisual; s, len: Longint; var cmap: TGGIColor): Longint; cdecl; external libggi;


var
  Visual: TGGIVisual;
  CurrentMode, OldMode: TGGIMode;


procedure ggi_savevideostate;
begin
  ggiGetMode(Visual, OldMode);
end;

procedure ggi_restorevideostate;
begin
  ggiSetMode(Visual, OldMode);
end;

const
  BgiColors: array[0..15] of TGGIColor = (
    (r: $0000; g: $0000; b: $0000; a: 0),
    (r: $0000; g: $0000; b: $8000; a: 0),
    (r: $0000; g: $8000; b: $0000; a: 0),
    (r: $0000; g: $8000; b: $8000; a: 0),
    (r: $8000; g: $0000; b: $0000; a: 0),
    (r: $8000; g: $0000; b: $8000; a: 0),
    (r: $8000; g: $8000; b: $0000; a: 0),
    (r: $C000; g: $C000; b: $C000; a: 0),
    (r: $8000; g: $8000; b: $8000; a: 0),
    (r: $0000; g: $0000; b: $FFFF; a: 0),
    (r: $0000; g: $FFFF; b: $0000; a: 0),
    (r: $0000; g: $FFFF; b: $FFFF; a: 0),
    (r: $FFFF; g: $0000; b: $0000; a: 0),
    (r: $FFFF; g: $0000; b: $FFFF; a: 0),
    (r: $FFFF; g: $FFFF; b: $0000; a: 0),
    (r: $FFFF; g: $FFFF; b: $FFFF; a: 0));

procedure ggi_initmodeproc;
begin
  ggiParseMode(ModeNames[IntCurrentMode], CurrentMode);
  ggiSetMode(Visual, CurrentMode);
end;

function ClipCoords(var x, y: SmallInt): Boolean;
{ Adapt to viewport, return TRUE if still in viewport,
  false if outside viewport}
begin
  x := x + StartXViewPort;
  x := y + StartYViewPort;
  ClipCoords := not ClipPixels;
  if ClipCoords then begin
    ClipCoords := (y < StartXViewPort) or (x > (StartXViewPort + ViewWidth));
    ClipCoords := ClipCoords or
                  ((y < StartYViewPort) or (y > (StartYViewPort + ViewHeight)));
    ClipCoords := not ClipCoords;
  end;
end;


procedure ggi_directpixelproc(X, Y: smallint);
var
  Color, CurCol: TGGIPixel;
begin
  CurCol := ggiMapColor(Visual, BgiColors[CurrentColor]);
  case CurrentWriteMode of
    XORPut: begin
        { getpixel wants local/relative coordinates }
        ggiGetPixel(Visual, x-StartXViewPort, y-StartYViewPort, Color);
        Color := CurCol xor Color;
      end;
    OrPut: begin
        { getpixel wants local/relative coordinates }
        ggiGetPixel(Visual, x-StartXViewPort, y-StartYViewPort, Color);
        Color := CurCol or Color;
      end;
    AndPut: begin
        { getpixel wants local/relative coordinates }
        ggiGetPixel(Visual, x-StartXViewPort, y-StartYViewPort, Color);
        Color := CurCol and Color;
      end;
    NotPut:
      Color := not Color;
    else
      Color := CurCol;
  end;
  ggiPutPixel(Visual, x, y, Color);
end;

procedure ggi_putpixelproc(X,Y: smallint; Color: Word);
begin
  If Not ClipCoords(X,Y) Then exit;
  ggiputpixel(Visual,x, y, Color);
end;

function ggi_getpixelproc (X,Y: smallint): word;

Var i : TGGIPixel;

begin
 ClipCoords(X,Y);
 ggigetpixel(Visual,x, y,I);
 ggi_getpixelproc:=i;
end;

procedure ggi_clrviewproc;
begin
  ggidrawbox(Visual,StartXViewPort,StartYViewPort,ViewWidth,ViewHeight);
end;

{ Bitmap utilities }
type
  PBitmap = ^TBitmap;
  TBitmap = record
            Width, Height: longint;
            reserved : longint;
            Data: record end;
            end;

procedure ggi_putimageproc (X,Y: smallint; var Bitmap; BitBlt: Word);
begin
  With TBitMap(BitMap) do
    ggiputbox(Visual,x, y, width, height, @Data);
end;

procedure ggi_getimageproc (X1,Y1,X2,Y2: smallint; Var Bitmap);
begin
  with TBitmap(Bitmap) do
    begin
    Width := x2 - x1 + 1;
    Height := y2 - y1 + 1;
    ggigetbox(Visual,x1,y1, x2 - x1 + 1, y2 - y1 + 1, @Data);
    end;
end;

function  ggi_imagesizeproc (X1,Y1,X2,Y2: smallint): longint;
begin
 // 32 bits per pixel -- change ASAP !!
 ggi_imagesizeproc := SizeOf(TBitmap) + (x2 - x1 + 1) * (y2 - y1 + 1) * SizeOF(longint);
end;

procedure ggi_hlineproc (x, x2,y : smallint);
begin
end;

procedure ggi_vlineproc (x,y,y2: smallint);
begin
end;

procedure ggi_patternlineproc (x1,x2,y: smallint);
begin
end;

procedure ggi_ellipseproc  (X,Y: smallint;XRadius: word;
  YRadius:word; stAngle,EndAngle: word; fp: PatternLineProc);
begin
end;

procedure ggi_lineproc (X1, Y1, X2, Y2 : smallint);
begin
end;

procedure ggi_getscanlineproc (X1, X2, Y : smallint; var data);
begin
end;

procedure ggi_setactivepageproc (page: word);
begin
end;

procedure ggi_setvisualpageproc (page: word);
begin
end;


procedure ggi_savestateproc;
begin
end;

procedure ggi_restorestateproc;
begin
end;

procedure ggi_setrgbpaletteproc(ColorNum, RedValue, GreenValue, BlueValue: smallint);

Var Col : TGGIcolor;

begin
  col.r:=redvalue;
  col.g:=greenvalue;
  col.b:=bluevalue;
  ggisetpalette(Visual,ColorNum,1,col);
end;

procedure ggi_getrgbpaletteproc (ColorNum: smallint;
                                    var RedValue, GreenValue, BlueValue: smallint);

Var Col : TGGIColor;

begin
  ggigetpalette(Visual,ColorNum,1,col);
  RedValue:=Col.R;
  GreenValue:=Col.G;
  BlueValue:=Col.B;
end;

{************************************************************************}
{*                       General routines                               *}
{************************************************************************}

procedure CloseGraph;
begin
  if not IsGraphMode then
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
  ModeInfo: TGGIMode;

  procedure AddGGIMode(i: smallint);     // i is the mode number
  var
    mode: TModeInfo;
  begin
    InitMode(Mode);
    with Mode do begin
      ModeNumber := i;
      ModeName := ModeNames[i];
      // Pretend we're VGA always.
      DriverNumber := VGA;
      MaxX := ModeInfo.Visible.X;
      MaxY := ModeInfo.Visible.Y;
      // MaxColor := ModeInfo.colors;
      MaxColor := 255;
      PaletteSize := MaxColor;
      HardwarePages := 0;
      // necessary hooks ...
      DirectPutPixel := @ggi_DirectPixelProc;
      GetPixel       := @ggi_GetPixelProc;
      PutPixel       := @ggi_PutPixelProc;
      SetRGBPalette  := @ggi_SetRGBPaletteProc;
      GetRGBPalette  := @ggi_GetRGBPaletteProc;
      ClearViewPort  := @ggi_ClrViewProc;
      PutImage       := @ggi_PutImageProc;
      GetImage       := @ggi_GetImageProc;
      ImageSize      := @ggi_ImageSizeProc;
      { Add later maybe ?
      SetVisualPage  := SetVisualPageProc;
      SetActivePage  := SetActivePageProc;
      GetScanLine    := @ggi_GetScanLineProc;
      Line           := @ggi_LineProc;
      InternalEllipse:= @ggi_EllipseProc;
      PatternLine    := @ggi_PatternLineProc;
      HLine          := @ggi_HLineProc;
      VLine          := @ggi_VLineProc;
      }
      InitMode       := @ggi_InitModeProc;
    end;
    AddMode(Mode);
  end;

var
  i: longint;
  OldMode: TGGIMode;
begin
  QueryAdapterInfo := ModeList;
  { If the mode listing already exists... }
  { simply return it, without changing    }
  { anything...                           }
  if Assigned(ModeList) then
    exit;
  SaveVideoState:=ggi_savevideostate;
  RestoreVideoState:=ggi_restorevideostate;

  If ggiInit <> 0 then begin
    _graphresult := grNoInitGraph;
    exit;
  end;

  Visual := ggiOpen(nil, []); // Use default visual

  ggiGetMode(Visual, OldMode);
  ggiParseMode('', ModeInfo);
  ggiSetMode(Visual, ModeInfo);
  ggiGetMode(Visual, ModeInfo);
  ggiSetMode(Visual, OldMode);
  AddGGIMode(0);

  for i := 1 to GLastMode do begin
    // WriteLn('Testing mode: ', ModeNames[i]);
    ggiParseMode(ModeNames[i], ModeInfo);
    If ggiCheckMode(visual, ModeInfo) = 0 then begin
      Writeln('OK for mode: ', ModeNames[i]);
      AddGGIMode(i);
    end;
  end;
end;


{
$Log$
Revision 1.7  1999-12-20 11:22:38  peter
  * modes moved to interface
  * integer -> smallint

Revision 1.6  1999/12/11 23:41:39  jonas
  * changed definition of getscanlineproc to "getscanline(x1,x2,y:
    smallint; var data);" so it can be used by getimage too
  * changed getimage so it uses getscanline
  * changed floodfill, getscanline16 and definitions in Linux
    include files so they use this new format
  + getscanlineVESA256 for 256 color VESA modes (banked)

Revision 1.5  1999/11/12 02:13:01  carl
  * Bugfix if getimage / putimage, format was not standard with FPC
    graph.

Revision 1.4  1999/11/10 10:54:24  sg
* Fixed a LOT of bugs:
* - Default mode should be determined by GGI now
* - Colors are working (only the 16 standard VGA colors, though)

Revision 1.3  1999/11/08 20:04:55  sg
* GGI programs must link to libc, or ggiOpen will fail!
* Changed max length of ModeNames string from 18 to 20 chars

Revision 1.2  1999/11/08 00:08:43  michael
* Fist working version of svgalib new graph unit
* Initial implementation of ggi new graph unit

Revision 1.1  1999/11/07 16:57:26  michael
+ Start of common graph implementation

}
