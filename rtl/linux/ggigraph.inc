{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999 by the Free Pascal development team

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
   GGI bindings.
  ---------------------------------------------------------------------}

Const
  { Supported modes }
  GTEXT             = 0;                { Compatible with VGAlib v1.2 }
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

  GLASTMODE         = 49;
  ModeNames : Array[0..GLastMode] of string [18] = 
   ('80x40[T]',
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
    x, y: Integer;
  end;
  TGGIPixel = LongWord;
  PGGIColor = ^TGGIColor;
  TGGIColor = record
    r, g, b, a: Integer;
  end;
  PGGIClut = ^TGGIClut;
  TGGIClut = record
    size: Integer;
    data: PGGIColor;
  end;
  TGGIGraphType = LongWord;
  TGGIAttr = LongWord;
  TGGIMode = record			// requested by user and changed by driver
    Frames: LongInt;			// frames needed
    Visible: TGGICoord;			// vis. pixels, may change slightly
    Virt: TGGICoord;			// virtual pixels, may change
    Size: TGGICoord;			// size of visible in mm
    GraphType: TGGIGraphType;		// which mode ?
    dpp: TGGICoord;			// dots per pixel
  end;

Const 
  libggi = 'ggi';
function  ggiInit: Integer; cdecl; external libggi;
procedure ggiExit; cdecl; external libggi;
function  ggiOpen(display: PChar; args: Array of const): TGGIVisual; cdecl; external libggi;
function  ggiClose(vis: TGGIVisual): Integer; cdecl; external libggi;
function  ggiParseMode(s: PChar; var m: TGGIMode): Integer; cdecl; external libggi;
function  ggiSetMode(visual: TGGIVisual; var tm: TGGIMode): Integer; cdecl; external libggi;
function  ggiGetMode(visual: TGGIVisual; var tm: TGGIMode): Integer; cdecl; external libggi;
function  ggiCheckMode(visual: TGGIVisual; var tm: TGGIMode): Integer; cdecl; external libggi;

function  ggiPutPixel(vis: TGGIVisual; x, y: Integer; pixel: TGGIPixel): Integer; cdecl; external libggi;
function  ggiGetPixel(vis: TGGIVisual; x, y: Integer; var pixel: TGGIPixel): Integer; cdecl; external libggi;
function  ggiDrawBox(vis: TGGIVisual; x, y, w, h: Integer): Integer; cdecl; external libggi;
function  ggiPutBox(vis: TGGIVisual; x, y, w, h: Integer; var buffer): Integer; cdecl; external libggi;
function  ggiGetBox(vis: TGGIVisual; x, y, w, h: Integer; var buffer): Integer; cdecl; external libggi;

function  ggiGetPalette(vis: TGGIVisual; s, len: Integer; var cmap: TGGIColor): Integer; cdecl; external libggi;
function  ggiSetPalette(vis: TGGIVisual; s, len: Integer; var cmap: TGGIColor): Integer; cdecl; external libggi;


var
  Visual: TGGIVisual;
  CurrentMode : TGGIMode;
    
procedure ggi_savevideostate;

begin
end;

procedure ggi_restorevideostate;

Var mode : TGGIMode;

begin
  ggiparsemode(@ModeNames[Gtext][1],Mode);
  ggisetmode(Visual,Mode);
end;

const
  BgiColors: array[0..15] of LongInt
    = ($000000, $000080, $008000, $008080,
       $800000, $800080, $808000, $C0C0C0,
       $808080, $0000FF, $00FF00, $00FFFF,
       $FF0000, $FF00FF, $FFFF00, $FFFFFF);

procedure ggi_initmodeproc;

Var
  ModeName : string[20];
    
begin
  ggiparsemode(@ModeNames[IntCurrentMode][1],CurrentMode);
  ggisetmode(Visual,CurrentMode);
end;

Function ClipCoords (Var X,Y : Integer) : Boolean;
{ Adapt to viewport, return TRUE if still in viewport,
  false if outside viewport}
  
begin
  X:= X + StartXViewPort;
  Y:= Y + StartYViewPort;
  ClipCoords:=Not ClipPixels;
  if ClipCoords then
    Begin
    ClipCoords:=(X < StartXViewPort) or (X > (StartXViewPort + ViewWidth));
    ClipCoords:=ClipCoords or
               ((Y < StartYViewPort) or (Y > (StartYViewPort + ViewHeight)));
    ClipCoords:=Not ClipCoords;
    end;           
end;  


procedure ggi_directpixelproc(X,Y: Integer);

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
  ggiPutPixel(Visual,x, y, Color);
end;

procedure ggi_putpixelproc(X,Y: Integer; Color: Word);
begin
  If Not ClipCoords(X,Y) Then exit;
  ggiputpixel(Visual,x, y, Color);
end;

function ggi_getpixelproc (X,Y: Integer): word;

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
            Width, Height: Integer;
            Data: record end;
            end;

procedure ggi_putimageproc (X,Y: Integer; var Bitmap; BitBlt: Word);
begin
  With TBitMap(BitMap) do
    ggiputbox(Visual,x, y, width, height, @Data);
end;

procedure ggi_getimageproc (X1,Y1,X2,Y2: Integer; Var Bitmap);
begin
  with TBitmap(Bitmap) do
    begin
    Width := x2 - x1 + 1;
    Height := y2 - y1 + 1;
    ggigetbox(Visual,x1,y1, x2 - x1 + 1, y2 - y1 + 1, @Data);
    end;
end;

function  ggi_imagesizeproc (X1,Y1,X2,Y2: Integer): longint;
begin
 // 32 bits per pixel -- change ASAP !! 
 ggi_imagesizeproc := SizeOf(TBitmap) + (x2 - x1 + 1) * (y2 - y1 + 1) * SizeOF(longint);
end;

procedure ggi_hlineproc (x, x2,y : integer);
begin
end;

procedure ggi_vlineproc (x,y,y2: integer);
begin
end;

procedure ggi_patternlineproc (x1,x2,y: integer);
begin
end;

procedure ggi_ellipseproc  (X,Y: Integer;XRadius: word;
  YRadius:word; stAngle,EndAngle: word; fp: PatternLineProc);
begin
end;

procedure ggi_lineproc (X1, Y1, X2, Y2 : Integer);
begin
end;

procedure ggi_getscanlineproc (Y : integer; var data);
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

procedure ggi_setrgbpaletteproc(ColorNum, RedValue, GreenValue, BlueValue: Integer);

Var Col : TGGIcolor;

begin
  col.r:=redvalue;
  col.g:=greenvalue;
  col.b:=bluevalue;
  ggisetpalette(Visual,ColorNum,1,col);  
end;

procedure ggi_getrgbpaletteproc (ColorNum: integer; 
                                    var RedValue, GreenValue, BlueValue: Integer);

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
    modename : string[20];
    mode: TModeInfo;
    modeinfo : TGGImode;
    i : longint;
    
   begin
     QueryAdapterInfo := ModeList;
     { If the mode listing already exists... }
     { simply return it, without changing    }
     { anything...                           }
     if assigned(ModeList) then
       exit;
     SaveVideoState:=ggi_savevideostate;
     RestoreVideoState:=ggi_restorevideostate;  
     Writeln ('ggiInit');
     If ggiInit<>0 then
       begin
       _graphresult:=grNoInitGraph;
       exit;
       end;
     Writeln ('ggiOPen');  
     Visual:=ggiOpen(nil, []); // Use default visual  
     For I:=0 to GLastMode do
       begin
       Writeln(' testing mode : ',Modenames[I]);
       modename:=ModeNames[I]+#0;
       ggiparsemode(@ModeName[1],modeinfo);
       If ggiCheckMode(visual,modeinfo)=0 then
         begin
         Writeln('OK for mode : ',Modenames[I]);
         InitMode(Mode);
         With Mode do
           begin
           ModeNumber:=I;
           ModeName:=ModeNames[i];
           // Pretend we're VGA always.
           DriverNumber := VGA;
           MaxX:=ModeInfo.Visible.X;
           MaxY:=ModeInfo.Visible.Y;
           // MaxColor := ModeInfo.colors;
           MaxColor:=255;
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
       end;
   end;

{
$Log$
Revision 1.2  1999-11-08 00:08:43  michael
* Fist working version of svgalib new graph unit
* Initial implementation of ggi new graph unit

Revision 1.1  1999/11/07 16:57:26  michael
+ Start of common graph implementation

}
