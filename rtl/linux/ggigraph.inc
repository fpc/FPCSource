{
     $Id$
}
  const
   InternalDriverName = 'LinuxGX';

  var SavePtr : Pointer;

{ ---------------------------------------------------------------------
   SVGA bindings.

  ---------------------------------------------------------------------}

{  Link with VGA, gl and c libraries }
{$linklib vga}
{$linklib vgagl}
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
  { VGA modes }
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
  ModeNames : Array[1..GLastMode] of string [20] = 
   ('G320x200x16',
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

 { vga functions }
 Function vga_init: Longint; Cdecl; External;
 Function vga_getdefaultmode: Longint; Cdecl; External;

 Function vga_hasmode(mode: Longint): Boolean; Cdecl; External;

 Function vga_getmodeinfo(mode: Longint): pvga_modeinfo; Cdecl; External;
 Function vga_setmode(mode: Longint): Longint; Cdecl; External;
 Function vga_getxdim : Longint; cdecl;external;
 Function vga_getydim : longint; cdecl;external;

 { gl functions }
 procedure gl_setpixel(x, y, c: LongInt); Cdecl; External;
 function  gl_getpixel(x, y: LongInt): LongInt; cdecl; external;
 procedure gl_line(x1, y1, x2, y2, c: LongInt); Cdecl; External;
 procedure gl_fillbox(x, y, w, h, c: LongInt); Cdecl; External;
 procedure gl_circle(x, y, r, c: LongInt ); Cdecl; External;
 procedure gl_getbox(x, y, w, h: LongInt; dp: pointer); Cdecl; External;
 procedure gl_putbox(x, y, w, h: LongInt; dp: pointer); Cdecl; External;
 procedure gl_disableclipping; Cdecl; External;
 procedure gl_enableclipping; Cdecl; External;
 procedure gl_putboxpart(x, y, w, h, bw, bh: LongInt; b: pointer; xo, yo: LongInt); Cdecl; External;
 function  gl_rgbcolor(r, g, b: LongInt): LongInt; Cdecl; External;
 function  gl_setcontextvga(m: LongInt): LongInt; Cdecl; External;
 function  gl_allocatecontext: PGraphicsContext; Cdecl; External;
 procedure gl_getcontext(gc: PGraphicsContext); Cdecl; External;
 procedure gl_setrgbpalette; Cdecl; External;
 procedure gl_freecontext(gc: PGraphicsContext); Cdecl; External;
 procedure gl_setclippingwindow(x1, y1, x2, y2: LongInt); Cdecl; External;
 procedure gl_setwritemode(wm: LongInt); Cdecl; External;
 procedure gl_setfontcolors(bg, fg: LongInt); Cdecl; External;
 procedure gl_writen(x, y, n: LongInt; s: PChar); Cdecl; External;
 procedure gl_setfont(fw, fh: LongInt; fdp: pointer); Cdecl; External;

 procedure gl_copyboxfromcontext(var gc: TGraphicsContext; x1, y1, w, h, x2, y2: LongInt); Cdecl; External;
 procedure gl_setcontext(gc: PGraphicsContext); Cdecl; External;

 function  gl_setcontextvgavirtual(m: LongInt): LongInt; cdecl; external;
 procedure gl_font8x8; Cdecl; External;


{ ---------------------------------------------------------------------
    Required procedures
  ---------------------------------------------------------------------}
procedure libvga_initmodeproc;
begin
  vga_setmode(IntCurrentMode);
end;
  
function libvga_getpixelproc (X,Y: Integer): word;
begin
end;

procedure libvga_putpixelproc(X,Y: Integer; Color: Word);
begin
end;

procedure libvga_clrviewproc;
begin
end;

procedure libvga_putimageproc (X,Y: Integer; var Bitmap; BitBlt: Word);
begin
end;

procedure libvga_getimageproc (X1,Y1,X2,Y2: Integer; Var Bitmap);
begin
end;

function  libvga_imagesizeproc (X1,Y1,X2,Y2: Integer): longint;
begin
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

procedue libvga_ellipseproc  (X,Y: Integer;XRadius: word;
  YRadius:word; stAngle,EndAngle: word; fp: PatternLineProc);
begin
end;

procedure libvga_lineproc (X1, Y1, X2, Y2 : Integer);
begin
end;

procedure libvga_getscanlineproc = procedure (Y : integer; var data);
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
end;

procedure libvga_getrgbpaletteproc =(ColorNum: integer; var
     RedValue, GreenValue, BlueValue: Integer);
begin
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
     vga_init;
     For I:=1 to GLastMode do
       If vga_hasmode(I) then
         begin
         ModeInfo:=vga_getmodeinfo(i)^; 
         InitMode(Mode);
         With Mode,ModeInfo do
           begin
           ModeNumber:=I;
           ModeName:=ModeNames[i];
           DriverNumber := 0;
           MaxX:=Width;
           MaxY:=height;
           MaxColor := colors;
           PaletteSize := MaxColor;
           HardwarePages := 0;
           {
           // necessary hooks ... 
           DirectPutPixel : DefPixelProc;
           GetPixel       : GetPixelProc;
           PutPixel       : PutPixelProc;
           SetRGBPalette  : SetRGBPaletteProc;
           GetRGBPalette  : GetRGBPaletteProc;
           // defaults possible ... 
           SetVisualPage  : SetVisualPageProc;
           SetActivePage  : SetActivePageProc;
           ClearViewPort  : ClrViewProc;
           PutImage       : PutImageProc;
           GetImage       : GetImageProc;
           ImageSize      : ImageSizeProc;
           GetScanLine    : GetScanLineProc;
           Line           : LineProc;
           InternalEllipse: EllipseProc;
           PatternLine    : PatternLineProc;
           HLine          : HLineProc;
           VLine          : VLineProc;
           InitMode       : InitModeProc;
           next: PModeInfo;
           DirectPutPixel:={$ifdef fpc}@{$endif}DirectPutPixVESA256;
           PutPixel:={$ifdef fpc}@{$endif}PutPixVESA256;
           GetPixel:={$ifdef fpc}@{$endif}GetPixVESA256;
           SetRGBPalette := {$ifdef fpc}@{$endif}SetVESARGBPalette;
           GetRGBPalette := {$ifdef fpc}@{$endif}GetVESARGBPalette;
           InitMode := {$ifdef fpc}@{$endif}Init640x480x256;
           SetVisualPage := {$ifdef fpc}@{$endif}SetVisualVESA;
           SetActivePage := {$ifdef fpc}@{$endif}SetActiveVESA;
           hline := {$ifdef fpc}@{$endif}HLineVESA256;
           hline := {$ifdef fpc}@{$endif}HLineVESA256;
           XAspect := 10000;
           YAspect := 10000;
           }
           end;
         AddMode(Mode);
         end;
   end;

{
$Log$
Revision 1.1  1999-11-07 16:57:26  michael
+ Start of common graph implementation

}
