unit Graph;

{ *********************************************************************

  $Id$

  Copyright 1997,1998 Matthias K"oppe <mkoeppe@cs.uni-magdeburg.de>
  This library is free software in the sense of the GNU Library GPL;
  see `License Conditions' below.

  Info:

  This unit provides the functions of Borland's Graph unit for linux,
  it uses the SVGAlib to do the actual work, so you must have svgalib

  on your system

  This version requires Free Pascal 0.99.5 or higher.

  Large parts have not yet been implemented or tested.

  History:

  Date       Version  Who     Comments
  ---------- -------- ------- -------------------------------------
  25-Sep-97  0.1      mkoeppe Initial multi-target version.
  05-Oct-97  0.1.1    mkoeppe Linux: Added mouse use. Improved clipping.
                              Added bitmap functions.
  ??-Oct-97  0.1.2    mkoeppe Fixed screenbuf functions.
  07-Feb-98  0.1.3    mkoeppe Fixed a clipping bug in DOS target.
  12-Apr-98  0.1.4    mkoeppe Linux: Using Michael's re-worked SVGALIB
                              interface; prepared for FPC 0.99.5; removed
                              dependencies.
  15-Apr-98  0.1.5    michael Renamed to graph, inserted needed SVGlib
                              declarations here so it can be used independently
                              of the svgalib unit. Removed things that are NOT
                              part of Borland's Graph from the unit interface.
  

  License Conditions:

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Library General Public
  License as published by the Free Software Foundation; either
  version 2 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Library General Public License for more details.

  You should have received a copy of the GNU Library General Public
  License along with this library; if not, write to the Free
  Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


  *********************************************************************}

{
  Functions not currently implemented :
  -------------------------------------
  SetWriteMode
  SetLineStyle
  SetFillPattern
  SetUserCharSize
  SetTextStyle
  FillPoly
  FloodFill
  SetAspectRatio

  (please remove what you implement fom this list)
}


interface


{ ---------------------------------------------------------------------
   Constants

  ---------------------------------------------------------------------}

const
  NormalPut       = 0;
  CopyPut         = 0;
  XORPut          = 1;
  ORPut           = 2;
  ANDPut          = 3;
  NotPut          = 4;
  BackPut         = 8;

  Black           =  0;
  Blue            =  1;
  Green           =  2;
  Cyan            =  3;
  Red             =  4;
  Magenta         =  5;
  Brown           =  6;
  LightGray       =  7;
  DarkGray        =  8;
  LightBlue       =  9;
  LightGreen      = 10;
  LightCyan       = 11;
  LightRed        = 12;
  LightMagenta    = 13;
  Yellow          = 14;
  White           = 15;
  Border          = 16;

  SolidLn         = 0;
  DottedLn        = 1;
  CenterLn        = 2;
  DashedLn        = 3;
  UserBitLn       = 4;

  EmptyFill       = 0;
  SolidFill       = 1;
  LineFill        = 2;
  LtSlashFill     = 3;
  SlashFill       = 4;
  BkSlashFill     = 5;
  LtBkSlashFill   = 6;
  HatchFill       = 7;
  XHatchFill      = 8;
  InterleaveFill  = 9;
  WideDotFill     = 10;
  CloseDotFill    = 11;
  UserFill        = 12;

  NormWidth       = 1;
  ThickWidth      = 3;

const
  LeftText      = 0;
  CenterText    = 1;
  RightText     = 2;
  BottomText    = 0;
  TopText       = 2;
  BaseLine      = 3;
  LeadLine      = 4;

const
  { Error codes }
  grOK             = 0;
  grNoInitGraph    = -1;
  grNotDetected    = -2;
  grFileNotFound   = -3;
  grInvalidDriver  = -4;
  grNoLOadMem      = -5;
  grNoScanMem      = -6;
  grNoFloodMem     = -7;
  grFontNotFound   = -8;
  grNoFontMem      = -9;
  grInvalidmode    = -10;
  grError          = -11;
  grIOerror        = -12;
  grInvalidFont    = -13;
  grInvalidFontNum = -14;
  
  

{ ---------------------------------------------------------------------
   Types

  ---------------------------------------------------------------------}


Type
  FillPatternType = array[1..8] of byte;

  ArcCoordsType = record
     x,y : integer;
     xstart,ystart : integer;
     xend,yend : integer;
  end;

  RGBColor = record
    r,g,b,i : byte;
  end;


  PaletteType = record
     Size   : integer;

     Colors : array[0..767]of Byte;
  end;


  LineSettingsType = record
     linestyle : word;
     pattern : word;
     thickness : word;
  end;

  TextSettingsType = record
     font : word;
     direction : word;
     charsize : word;
     horiz : word;
     vert : word;
  end;

  FillSettingsType = record
     pattern : word;
     color : longint;
  end;

  PointType = record
     x,y : integer;
  end;

  ViewPortType = record
     x1,y1,x2,y2 : integer;
     Clip : boolean;
  end;



 const
  fillpattern : array[0..12] of FillPatternType = (
      ($00,$00,$00,$00,$00,$00,$00,$00),     { Hintergrundfarbe }
      ($ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff),     { Vordergrundfarbe }
      ($ff,$ff,$00,$00,$ff,$ff,$00,$00),     { === }
      ($01,$02,$04,$08,$10,$20,$40,$80),     { /// }
      ($07,$0e,$1c,$38,$70,$e0,$c1,$83),     { /// als dicke Linien }
      ($07,$83,$c1,$e0,$70,$38,$1c,$0e),     { \\\ als dicke Linien }
      ($5a,$2d,$96,$4b,$a5,$d2,$69,$b4),     { \ \\ \ }
      ($ff,$88,$88,$88,$ff,$88,$88,$88),     { K„stchen }
      ($18,$24,$42,$81,$81,$42,$24,$18),     { Rauten }
      ($cc,$33,$cc,$33,$cc,$33,$cc,$33),     { "Mauermuster" }
      ($80,$00,$08,$00,$80,$00,$08,$00),     { weit auseinanderliegende Punkte }
      ($88,$00,$22,$00,$88,$00,$22,$00),     { dichte Punkte}
      (0,0,0,0,0,0,0,0)                      { benutzerdefiniert }
     );



{ ---------------------------------------------------------------------
   Function Declarations

  ---------------------------------------------------------------------}

{ Retrieving coordinates }
function  GetX: Integer;                                        
function  GetY: Integer;                                        

{ Pixel-oriented routines }
procedure PutPixel(X, Y: Integer; Pixel: Word);
function  GetPixel(X, Y: Integer): Word;        

{ Line-oriented primitives }
procedure SetWriteMode(WriteMode: Integer);
procedure LineTo(X, Y: Integer);
procedure LineRel(Dx, Dy: Integer);
procedure MoveTo(X, Y: Integer);
procedure MoveRel(Dx, Dy: Integer);
procedure Line(x1, y1, x2, y2: Integer);
procedure SetLineStyle(LineStyle: Word; Pattern: Word; Thickness: Word);

{ Linearly bounded primitives }
procedure Rectangle(x1, y1, x2, y2: Integer);
procedure Bar(x1, y1, x2, y2: Integer);
procedure Bar3D(x1, y1, x2, y2: Integer; Depth: Word; Top: Boolean);
procedure DrawPoly(NumPoints: Word; var PolyPoints);
procedure FillPoly(NumPoints: Word; var PolyPoints);
procedure SetFillStyle(Pattern: Word; Color: Word);
procedure SetFillPattern(Pattern: FillPatternType; Color: Word);
procedure FloodFill(X, Y: Integer; Border: Word);

{ Nonlinearly bounded primitives }

procedure Arc(X, Y: Integer; StAngle, EndAngle, Radius: Word);
procedure GetArcCoords(var ArcCoords: ArcCoordsType);   
procedure Circle(X, Y: Integer; Radius: Word);
procedure Ellipse(X, Y: Integer; StAngle, EndAngle: Word; XRadius, YRadius : Word);
procedure FillEllipse(X, Y: Integer; XRadius, YRadius : Word);
procedure SetAspectRatio(Xasp, Yasp: Word);
procedure PieSlice(X, Y: Integer; StAngle, EndAngle, Radius: Word);
procedure Sector(X, Y: Integer; StAngle, EndAngle, XRadius, YRadius: Word);

{ Color routines }
procedure SetBkColor(ColorNum: Word);
procedure SetColor(Color: Word);
function  GetMaxColor : Word;

{ Bitmap utilities }
procedure GetImage(x1, y1, x2, y2: Integer; var BitMap);
procedure PutImage(X, Y: Integer; var BitMap; BitBlt: Word);
function ImageSize(x1, y1, x2, y2: Integer): LongInt;

{ Text routines}
procedure OutText(TextString: string);
procedure OutTextXY(X, Y: Integer; TextString: string);
procedure SetTextJustify(Horiz, Vert: Word);
procedure SetTextStyle(Font, Direction: Word; CharSize: Word);
procedure SetUserCharSize(MultX, DivX, MultY, DivY: Word);

{ Graph clipping method }
procedure SetViewPort(x1, y1, x2, y2: Integer; Clip: Boolean);
Procedure ClearViewPort;

{ Init/Done }
procedure InitVideo;
procedure DoneVideo;

{ Other }
function GetResX: Integer;
function GetResY: Integer;
function GetAspect: Real;
function GetMaxX : Integer;
function GetMAxY : Integer;

{ For compatibility }
Procedure DetectGraph (Var Driver,Mode : Integer);
Procedure InitGraph (Var Driver,Mode : Integer;DriverPath : String);
Procedure CloseGraph;
Function GraphResult : Integer;
Procedure GraphDefaults ;
Function GraphErrorMsg (Errcode : Integer) : String;

const
  NoGraphics: Boolean = false;

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



implementation

uses Objects, Linux;


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
   Types, constants and variables

  ---------------------------------------------------------------------}

var
  DrawDelta: TPoint;
  CurX, CurY: Integer;
  TheColor, TheFillColor: LongInt;
  IsVirtual: Boolean;
  PhysicalScreen, BackScreen: PGraphicsContext;
  ColorTable: array[0..15] of LongInt;

const
  BgiColors: array[0..15] of LongInt
    = ($000000, $000080, $008000, $008080,
       $800000, $800080, $808000, $C0C0C0,
       $808080, $0000FF, $00FF00, $00FFFF,
       $FF0000, $FF00FF, $FFFF00, $FFFFFF);

const
  DoUseMarker: Boolean = true;
  TheMarker: Char      = '~';
  TextColor: LongInt   = 15;
  MarkColor: LongInt   = 15;
  BackColor: LongInt   = 0;
  FontWidth: Integer   = 8;
  FontHeight: Integer  = 8;

var
  sHoriz, sVert: Word;

{ initialisierte Variablen }
const
  SourcePage: Word = 0;
  DestPage: Word = 0;

{ Retrieves the capabilities for the current mode }
const
  vmcImage       = 1;
  vmcCopy        = 2;
  vmcSaveRestore = 4;
  vmcBuffer      = 8;
  vmcBackPut     = 16;

{ ---------------------------------------------------------------------
   Graphics Vision Layer
  ---------------------------------------------------------------------}


{ Types and constants }
var
  SizeX, SizeY: Word;

{ Draw origin and clipping rectangle }
var
  DrawOrigin: TPoint;
  ClipRect: TRect;
  MetaClipRect: TRect;
  MetaOrigin: TPoint;

{ Font attributes }
const
  ftNormal          = 0;
  ftBold            = 1;
  ftThin            = 2;
  ftItalic          = 4;

var
  sFont, sColor:Word;
  sCharSpace: Integer;
{ Not used
  sMarker: Char;
  sAttr: Word; }

{ Windows-style text metric }
type
  PTextMetric = ^TTextMetric;
  TTextMetric = record
    tmHeight: Integer;
    tmAscent: Integer;
    tmDescent: Integer;
    tmInternalLeading: Integer;
    tmExternalLeading: Integer;
    tmAveCharWidth: Integer;
    tmMaxCharWidth: Integer;
    tmWeight: Integer;
    tmItalic: Byte;
    tmUnderlined: Byte;
    tmStruckOut: Byte;
    tmFirstChar: Byte;
    tmLastChar: Byte;
    tmDefaultChar: Byte;
    tmBreakChar: Byte;
    tmPitchAndFamily: Byte;
    tmCharSet: Byte;
    tmOverhang: Integer;
    tmDigitizedAspectX: Integer;
    tmDigitizedAspectY: Integer;
  end;


{ Bitmap utilities }
type
  PBitmap = ^TBitmap;
  TBitmap = record
              Width, Height: Integer;
              Data: record end;
            end;
        

 { Storing screen regions }
type
  TVgaBuf = record
    Bounds: TRect;
    Mem: Word;
    Size: Word;
  end;

const
  pbNone  = 0;
  pbCopy  = 1;
  pbClear = 2;

type
  PScreenBuf = ^TScreenBuf;
  TScreenBuf = record
    Mode: Word;
    Rect: TRect;
    Size: LongInt;
    Info: LongInt
  end;



 { Procedures and functions }


procedure SetColors;
var
  i: Integer;
begin
  for i:=0 to 15 do
    ColorTable[i] := gl_rgbcolor(BgiColors[i] shr 16,
                                 (BgiColors[i] shr 8) and 255,
                                 BgiColors[i] and 255)
end;



procedure InitVideo;
var
  VgaMode: Integer;
  ModeInfo: pvga_modeinfo;
begin
  if NoGraphics
  then begin
    SizeX := 640;
    SizeY := 480
  end
  else begin
    VgaMode := vga_getdefaultmode;
    if (VgaMode = -1) then VgaMode := G320X200X256;
    if (not vga_hasmode(VgaMode))
      then begin
        WriteLn('BGI: Mode not available.');
        Halt(1)
      end;
    ModeInfo := vga_getmodeinfo(VgaMode);
    {IsVirtual := (ModeInfo^.colors = 16) or (ModeInfo^.flags and IS_MODEX <> 0);}
    IsVirtual := true;
    { We always want a back screen (for buffering). }
    if IsVirtual
      then begin
        { Create virtual screen }
        gl_setcontextvgavirtual(VgaMode);
        BackScreen := gl_allocatecontext;
        gl_getcontext(BackScreen)
      end;
    vga_setmode(VgaMode);
    gl_setcontextvga(VgaMode);  { Physical screen context. }
    PhysicalScreen := gl_allocatecontext;
    gl_getcontext(PhysicalScreen);
    if (PhysicalScreen^.colors = 256) then gl_setrgbpalette;
    SetColors;
    SizeX := PhysicalScreen^.Width;
    SizeY := PhysicalScreen^.Height
  end
end;

procedure DoneVideo;
begin
  if not NoGraphics
    then begin
      if IsVirtual then gl_freecontext(BackScreen);
      vga_setmode(GTEXT)
    end
end;

procedure SetDelta;
begin
  if ClipRect.Empty
  then begin
    DrawDelta.X := 10000;
    DrawDelta.Y := 10000;
  end
  else begin
    DrawDelta.X := DrawOrigin.X;
    DrawDelta.y := DrawOrigin.y
  end
end;

procedure SetDrawOrigin(x, y: Integer);
begin
  DrawOrigin.x := x;
  DrawOrigin.y := y;
  SetDelta;
end;

procedure SetDrawOriginP(var P: TPoint);
begin
  SetDrawOrigin(P.x, P.y)
end;

procedure SetClipRect(x1, y1, x2, y2: Integer);
begin
  Cliprect.Assign(x1, y1, x2, y2);
  if not NoGraphics
    then begin
      if ClipRect.Empty
        then gl_setclippingwindow(0, 0, 0, 0)
        else gl_setclippingwindow(x1, y1, x2 - 1, y2 - 1);
      {gl_enableclipping(0);}
    end;
  SetDelta
end;

procedure SetClipRectR(var R: TRect);
begin
  SetClipRect(R.A.X, R.A.Y, R.B.X, R.B.Y);
end;

procedure SetMetaOrigin(x, y: Integer);
begin
  MetaOrigin.x := x;
  MetaOrigin.y := y
end;

procedure SetMetaOriginP(P: TPoint);
begin
  SetMetaOrigin(P.x, P.y)
end;

procedure SetMetaClipRect(x1, y1, x2, y2: Integer);
begin
  MetaCliprect.Assign(x1, y1, x2, y2)
end;

procedure SetMetaClipRectR(var R: TRect);
begin
  MetaCliprect := R
end;

function GetBuffer(Size: Word): pointer;
begin
  { No metafiling available. }
  GetBuffer := nil
end;

Procedure HoriLine(x1,y1,x2: Integer);
begin
  Line(x1, y1, x2, y1)
end;

Procedure VertLine(x1,y1,y2: Integer);
begin
  Line(x1, y1, x1, y2)
end;

procedure FillCircle(xm, ym, r: Integer);
begin
  FillEllipse(xm, ym, r, r)
end;

{ Text routines }

function TextWidth(s: string): Integer;
var
  i: Integer;
begin
  if DoUseMarker
  then begin
    For i := Length(s) downto 1 do
      If s[i] = TheMarker then Delete(s, i, 1);
    If s = ''
    then TextWidth := 0
    else TextWidth := Length(s) * FontWidth
  end
  else TextWidth := Length(s) * FontWidth
end;

function TextHeight(s: string): Integer;
begin
  TextHeight := FontHeight
end;


procedure OutText(TextString: string);
begin
  OutTextXY(GetX, GetY, TextString)
end;

procedure OutTextXY(X, Y: Integer; TextString: string);
var
  P, Q: PChar;
  i: Integer;
  col: Boolean;
begin
  if NoGraphics or (TextString='') then Exit;
  gl_setwritemode(FONT_COMPRESSED + WRITEMODE_MASKED);
  case sHoriz of
    CenterText : Dec(x, TextWidth(TextString) div 2);
    RightText  : Dec(x, TextWidth(TextString));
  end; { case }
  case sVert of
    CenterText : Dec(y, TextHeight(TextString) div 2);
    BottomText, BaseLine : Dec(y, TextHeight(TextString));
  end; { case }
  MoveTo(X, Y);
  P := @TextString[1]; Q := P;
  col := false;
  gl_setfontcolors(BackColor, TextColor);
  For i := 1 to Length(TextString) do
  begin
    If (Q[0] = TheMarker) and DoUseMarker
      then begin
        If col then gl_setfontcolors(BackColor, MarkColor)
        else gl_setfontcolors(BackColor, TextColor);
        If Q <> P then begin
          gl_writen(CurX, CurY, Q-P, P);
          MoveRel(FontWidth * (Q-P), 0)
        end;
        col := not col;
        P := Q + 1
      end;
    {Inc(Q)} Q := Q + 1
  end;
  If col then gl_setfontcolors(BackColor, MarkColor)
  else gl_setfontcolors(BackColor, TextColor);
  If Q <> P then begin
    gl_writen(CurX, CurY, Q-P, P);
    MoveRel(FontWidth * (Q-P), 0)
  end
end;

procedure SetTextJustify(Horiz, Vert: Word);
begin
  sHoriz := Horiz; sVert := Vert;
end;

procedure SetTextStyle(Font, Direction: Word; CharSize: Word);
begin
end;

procedure SetUserCharSize(MultX, DivX, MultY, DivY: Word);
begin
end;

procedure SetKern(Enable: Boolean);
begin
end;

procedure SetMarker(Marker: Char);
begin
  TheMarker := Marker
end;


procedure SetTextParams(Font: Word; CharSpace: Integer; Color: Word;
  UseMarker: Boolean);
type
  pp = ^pointer;

function FixCol(Col: Byte): Byte;
{ SVGALIB cannot write black characters... }
begin
  if Col=0 then FixCol := 1 else FixCol := Col
end; { FixCol }

begin
  sColor := Color; sCharSpace := CharSpace; sFont := Font;
  if not NoGraphics then begin
    TextColor := ColorTable[FixCol(Color and 15)];
    MarkColor := ColorTable[FixCol((Color shr 8) and 15)];
    DoUseMarker := UseMarker;
    gl_setfont(8, 8, (pp(@gl_font8x8))^);
  end
end;


function GetResX: Integer;
begin
  GetResX := 96;
end; { GetResX }

function GetResY: Integer;
begin
  GetResY := 96
end; { GetResY }

function GetAspect: Real;
begin
  GetAspect := 1.0
end; { GetAspect }

Var LastViewPort : ViewPortType;

procedure SetViewPort(x1, y1, x2, y2: Integer; Clip: Boolean);
begin
  LastViewPort.X1:=X1;
  LastViewPort.Y1:=Y1;
  LastViewPort.X2:=X2;
  LastViewPort.Y2:=Y2;
  LastViewPort.Clip:=Clip;
  SetDrawOrigin(x1, y1);
  if Clip then SetClipRect(x1, y1, x2+1, y2+1)
  else SetClipRect(0, 0, SizeX, SizeY)
end;


Procedure ClearViewPort;

begin
  With LastViewPort do
  gl_fillbox(X1,Y1,X2-X1,Y2-Y1,BackColor);
end;

{ VGAMEM }

type
  TImage = record
  end;

procedure CopyScreen(x1, y1, x2, y2, x3, y3: Integer);
begin
  if not NoGraphics and (x2 > x1) and (y2 > y1)
    then gl_copyboxfromcontext(PhysicalScreen^, x1, y1, x2 - x1, y2 - y1, x3, y3);
end;

{ BGI-like Image routines
}

function CopyImage(Image: pointer): pointer;
begin
  CopyImage := nil
end;

function CutImage(x1, y1, x2, y2: Integer): pointer;
var
  Image: PBitmap;
begin

  GetMem(Image, ImageSize(x1, y1, x2, y2));
  if Image <> nil
    then GetImage(x1, y1, x2, y2, Image^);
  CutImage := Image;
end;

procedure GetImageExtent(Image: pointer; var Extent: Objects.TPoint);
begin
  if Image = nil
    then begin
      Extent.X := 0;
      Extent.Y := 0
    end
    else begin
      Extent.X := PBitmap(Image)^.Width;
      Extent.Y := PBitmap(Image)^.Height
    end;
end;


procedure FreeImage(Image: pointer);
var
  P: TPoint;
begin
  if Image <> nil
    then begin
      GetImageExtent(Image, P);
      FreeMem(Image, ImageSize(0, 0, P.x - 1, P.y - 1));
    end;
end;


function LoadImage(var S: TStream): pointer;
begin
  LoadImage := nil
end;

function MaskedImage(Image: pointer): pointer;
begin
  MaskedImage := nil;
end;

procedure PasteImage(X, Y: Integer; Image: pointer; BitBlt: Word);
begin
  if Image <> nil then PutImage(X, Y, Image^, BitBlt)
end;

procedure StoreImage(var S: TStream; Image: pointer);
begin
end;

{ Storing screen regions }
function PrepBuf(var R: Objects.TRect; Action: Word; var Buf: TVgaBuf): Boolean;
begin
  if BackScreen <> nil
    then begin
      Buf.Bounds := R;
      gl_setcontext(BackScreen);
      gl_disableclipping;
      case Action of
        pbCopy  : gl_copyboxfromcontext(PhysicalScreen^,
                                        R.A.X, R.A.Y, R.B.X - R.A.X, R.B.Y - R.A.Y,
                                        R.A.X, R.A.Y);
        pbClear : gl_fillbox(R.A.X, R.A.Y, R.B.X - R.A.X, R.B.Y - R.A.Y, 0);
      end;
      PrepBuf := true;
      SetDrawOrigin(0, 0);
      SetClipRectR(R);
    end
    else PrepBuf := false
end; { PrepBuf }

procedure EndBufDraw;
begin
  if not NoGraphics
    then gl_setcontext(PhysicalScreen);
end; { EndBufDraw }

procedure ReleaseBuf(var Buf: TVgaBuf);
begin
end; { ReleaseBuf }

procedure PasteRectAt(var R: Objects.TRect; P: Objects.TPoint; var Buf: TVgaBuf);
begin
  if not NoGraphics and (BackScreen <> nil)
    then gl_copyboxfromcontext(BackScreen^,
                               R.A.X, R.A.Y, R.B.X - R.A.X, R.B.Y - R.A.Y,
                               P.X, P.Y);
end;


procedure PasteRect(var R: Objects.TRect; var Buf: TVgaBuf);
begin
  PasteRectAt(R, R.A, Buf);
end; { PasteRect }


function StoreScreen(x1, y1, x2, y2: Integer): PScreenBuf;
var
  s: LongInt;
  p: pointer;
  SaveOrigin: TPoint;

function NewScreenBuf(AMode: Word; AnInfo: LongInt): PScreenBuf;
 var
   p: PScreenBuf;
 Begin
   New(p);
   p^.Mode := AMode;
   p^.Size := s;
   p^.Rect.Assign(x1, y1, x2, y2);
   p^.Info := AnInfo;
   NewScreenBuf := p
 End;

Begin
  { General Images }
  s := 0;
  SaveOrigin := DrawOrigin;
  SetDrawOrigin(0, 0);
  p := CutImage(x1, y1, x2-1, y2-1);
  SetDrawOriginP(SaveOrigin);
  If p <> nil
    then StoreScreen := NewScreenBuf(2, LongInt(p))
  else StoreScreen := nil
End;

procedure FreeScreenBuf(Buf: PScreenBuf);
Begin
  If Buf <> nil then Begin
    case Buf^.Mode of
      2 : FreeImage(pointer(Buf^.Info));
    end;
    Dispose(Buf)
  End
End;

procedure DrawScreenBufAt(Buf: PScreenBuf; x3, y3: Integer);
var
  SaveOrigin: TPoint;
Begin
  If Buf <> nil then
    case Buf^.Mode of
      2 :
          begin
            SaveOrigin := DrawOrigin;
            SetDrawOrigin(0, 0);
            PasteImage(x3, y3, pointer(Buf^.Info), NormalPut);
            SetDrawOriginP(SaveOrigin);
          end
    end
End;

procedure DrawScreenBuf(Buf: PScreenBuf);
Begin
  If Buf <> nil then
    DrawScreenBufAt(Buf, Buf^.Rect.A.x, Buf^.Rect.A.y)
End;

function GetVgaMemCaps: Word;
begin
  GetVgaMemCaps := vmcCopy
end;

procedure GetTextMetrics(var Metrics: TTextMetric);
begin
  with Metrics do
  begin
    tmHeight := 8;
    tmAscent := 8;
    tmDescent := 0;
    tmInternalLeading := 0;
    tmExternalLeading := 0;
    tmAveCharWidth := 8;
    tmMaxCharWidth := 8;
    tmWeight := 700;
    tmItalic := 0;
    tmUnderlined := 0;
    tmStruckOut := 0;
    tmFirstChar := 0;
    tmLastChar := 255;
    tmDefaultChar := 32;
    tmBreakChar := 32;
    tmPitchAndFamily := 0;
    tmCharSet := 0;
    tmOverhang := 0;
    tmDigitizedAspectX := 100;
    tmDigitizedAspectY := 100
  end;
end;

{ ---------------------------------------------------------------------
   Real graph implementation
  ---------------------------------------------------------------------}


function GetX: Integer;                                 
begin
  GetX := CurX - DrawDelta.X
end;

function GetY: Integer;                                 
begin
  GetY := CurY - DrawDelta.Y
end;

{ Pixel-oriented routines }
procedure PutPixel(X, Y: Integer; Pixel: Word);
begin
  if not NoGraphics
    then gl_setpixel(X + DrawDelta.X, Y + DrawDelta.Y, Pixel)
end;

function GetPixel(X, Y: Integer): Word;                 
begin
  if NoGraphics
    then GetPixel := 0
    else GetPixel := gl_getpixel(X + DrawDelta.X, Y + DrawDelta.Y)
end;

{ Line-oriented primitives }
procedure SetWriteMode(WriteMode: Integer);
begin
{  Graph.SetWriteMode(WriteMode) }
end;

procedure LineTo(X, Y: Integer);
begin
  if not NoGraphics
    then gl_line(CurX, CurY, X + DrawDelta.X, Y + DrawDelta.Y, TheColor);
  CurX := X + DrawDelta.X;
  CurY := Y + DrawDelta.Y
end;

procedure LineRel(Dx, Dy: Integer);
begin
  if not NoGraphics
    then gl_line(CurX, CurY, CurX + Dx, CurY + Dy, TheColor);
  CurX := CurX + Dx;
  CurY := CurY + Dy
end;

procedure MoveTo(X, Y: Integer);
begin
  CurX := X + DrawDelta.X;
  CurY := Y + DrawDelta.Y
end;

procedure MoveRel(Dx, Dy: Integer);
begin
  CurX := CurX + Dx;
  CurY := CurY + Dy
end;

procedure Line(x1, y1, x2, y2: Integer);
begin
  if not NoGraphics
    then gl_line(x1 + DrawDelta.X, y1 + DrawDelta.Y,
                 x2 + DrawDelta.X, y2 + DrawDelta.Y, TheColor)
end;

procedure SetLineStyle(LineStyle: Word; Pattern: Word; Thickness: Word);
begin
end;

procedure SetFillPattern(Pattern: FillPatternType; Color: Word);

begin
end;


{ Linearly bounded primitives }

procedure Rectangle(x1, y1, x2, y2: Integer);
begin
  MoveTo(x1, y1);
  LineTo(x2, y1);
  LineTo(x2, y2);
  LineTo(x1, y2);
  LineTo(x1, y1)
end;

procedure Bar(x1, y1, x2, y2: Integer);
var
  R: TRect;
begin
  if not NoGraphics
    then begin
      R.Assign(x1 + DrawDelta.X, y1 + DrawDelta.Y,
               x2 + DrawDelta.X + 1, y2 + DrawDelta.Y + 1);
      R.Intersect(ClipRect);
      if not R.Empty
        then gl_fillbox(R.A.X, R.A.Y,
                        R.B.X - R.A.X, R.B.Y - R.A.Y, TheFillColor)
    end;
end;

procedure Bar3D(x1, y1, x2, y2: Integer; Depth: Word; Top: Boolean);
begin
  Bar(x1,y1,x2,y2);
  Rectangle(x1,y1,x2,y2);
  if top then begin
     Moveto(x1,y1);
     Lineto(x1+depth,y1-depth);
     Lineto(x2+depth,y1-depth);
     Lineto(x2,y1);
  end;
  Moveto(x2+depth,y1-depth);
  Lineto(x2+depth,y2-depth);
  Lineto(x2,y2);

end;

procedure DrawPoly(NumPoints: Word; var PolyPoints);

type
   ppointtype = ^pointtype;

var
   i : longint;

begin
   line(ppointtype(@polypoints)[NumPoints-1].x,
        ppointtype(@polypoints)[NumPoints-1].y,
        ppointtype(@polypoints)[0].x,
        ppointtype(@polypoints)[0].y);
   for i:=0 to NumPoints-2 do
     line(ppointtype(@polypoints)[i].x,
          ppointtype(@polypoints)[i].y,
          ppointtype(@polypoints)[i+1].x,
          ppointtype(@polypoints)[i+1].y);
end;

procedure FillPoly(NumPoints: Word; var PolyPoints);
begin
end;

procedure SetFillStyle(Pattern: Word; Color: Word);
begin
  TheFillColor := ColorTable[Color]
end;

procedure FloodFill(X, Y: Integer; Border: Word);
begin
end;

{ Nonlinearly bounded primitives
}

Var LastArcCoords : ArcCoordsType;


procedure SetArcCoords (X,y,xradius,yradius,Stangle,endangle : integer);   

begin
  LastArcCoords.X:=X;
  LastArccOords.y:=y;
  Lastarccoords.xstart:=x+round(xradius*cos(stangle*pi/180));
  Lastarccoords.ystart:=y-round(yradius*sin(stangle*pi/180));
  LastArccoords.xend:=x+round(xradius*cos(endangle*pi/180));
  LastArccoords.yend:=y-round(yradius*sin(endangle*pi/180));
end;


procedure GetArcCoords(var ArcCoords: ArcCoordsType);   

begin
  ArcCoords:=LastArcCoords;
end;

procedure Arc(X, Y: Integer; StAngle, EndAngle, Radius: Word);

begin
 Ellipse (X,y,stangle,endangle,Radius,radius);
end;

procedure Circle(X, Y: Integer; Radius: Word);
begin
  if not NoGraphics
    then gl_circle(X + DrawDelta.X, Y + DrawDelta.Y, Radius, TheColor)
end;

procedure Ellipse(X, Y: Integer;
  StAngle, EndAngle: Word; XRadius, YRadius : Word);

Var I : longint;
    tmpang : real;
    
begin
 SetArcCoords (X,Y,xradius,yradius,Stangle,EndAngle);
 For i:= StAngle To EndAngle Do
  Begin
   tmpAng:= i*Pi/180;
   curX:= X + Round (xRadius*Cos (tmpAng));
   curY:= Y - Round (YRadius*Sin (tmpAng));
   PutPixel (curX, curY, TheColor);
  End;
end;

procedure FillEllipse(X, Y: Integer; XRadius, YRadius : Word);

Var I,tmpcolor : longint;
    tmpang : real;
    tmpx,tmpy : Integer;
    
begin
 tmpcolor:=Thecolor;
 SetColor(TheFillColor);
 For i:= 0 to 180 Do
  Begin
   tmpAng:= i*Pi/180;
   curX:= Round (xRadius*Cos (tmpAng));
   curY:= Round (YRadius*Sin (tmpAng));
   tmpX:= X - curx;
   tmpy:= Y + cury;
   curx:=x+curx;
   cury:=y-cury;
   Line (curX, curY,tmpx,tmpy);
   PutPixel (curx,cury,tmpcolor);
   PutPixel (tmpx,tmpy,tmpcolor);
  End;
  SetColor(tmpcolor);
end;

procedure SetAspectRatio(Xasp, Yasp: Word);
begin
end;

procedure PieSlice(X, Y: Integer; StAngle, EndAngle, Radius: Word);

Begin
 sector (x,y,stangle,endangle,radius,radius);
end;

procedure Sector(X, Y: Integer;
  StAngle, EndAngle, XRadius, YRadius: Word);

Var I,tmpcolor : longint;
    tmpang : real;
    ac : arccoordstype;
    
begin
 tmpcolor:=Thecolor;
 SetColor(TheFillColor);
 For i:= stangle to endangle Do
   Begin
   tmpAng:= i*Pi/180;
   curX:= x+Round (xRadius*Cos (tmpAng));
   curY:= y-Round (YRadius*Sin (tmpAng));
   Line (x,y,curX, curY);
   PutPixel (curx,cury,tmpcolor);
   End;
 SetColor(tmpcolor);
 getarccoords(ac);
 Line (x,y,ac.xstart,ac.ystart);
 Line (x,y,ac.xend,ac.yend);
end;

{ Color routines
}

procedure SetBkColor(ColorNum: Word);
begin
  BackColor := ColorTable[ColorNum];
end;

procedure SetColor(Color: Word);
begin
  TheColor := ColorTable[Color];
end;

function getmaxcolor : Word;

begin
  getmaxcolor:=16;
end;

procedure GetImage(x1, y1, x2, y2: Integer; var BitMap);        
var
  SaveClipRect: TRect;
begin
  with TBitmap(Bitmap) do
  begin
    Width := x2 - x1 + 1;
    Height := y2 - y1 + 1;
    if not NoGraphics
      then begin
        {gl_disableclipping(0);}
        SaveClipRect := ClipRect;
        SetClipRect(0, 0, SizeX, SizeY);
        gl_getbox(x1 + DrawDelta.X, y1 + DrawDelta.Y,
                  x2 - x1 + 1, y2 - y1 + 1, @Data);
        SetClipRectR(SaveClipRect)
      end;
  end;
end;

procedure PutImage(X, Y: Integer; var BitMap; BitBlt: Word);
var
  R: TRect;
  SaveClipRect: TRect;
begin
  if not NoGraphics then
    with TBitmap(Bitmap) do
    begin
      {gl_putbox(x + DrawDelta.X, y + DrawDelta.Y, Width, Height, @Data)}
      R.Assign(X + DrawDelta.X, Y + DrawDelta.Y,
               X + DrawDelta.X + Width, Y + DrawDelta.Y + Height);
      R.Intersect(ClipRect);
      if not R.Empty
        then begin
          {gl_disableclipping(0);}
          SaveClipRect := ClipRect;
          SetClipRect(0, 0, SizeX, SizeY);
          gl_putboxpart(R.A.X, R.A.Y,
                        R.B.X - R.A.X, R.B.Y - R.A.Y,
                        Width, Height,
                        @Data,
                        R.A.X - X, R.A.Y - Y);
          SetClipRectR(SaveClipRect);
        end;
    end;
end; { PutImage }

function ImageSize(x1, y1, x2, y2: Integer): LongInt;
begin
  if NoGraphics
    then ImageSize := SizeOf(TBitmap)
    else ImageSize := SizeOf(TBitmap)
      + LongInt(x2 - x1 + 1) * LongInt(y2 - y1 + 1) * PhysicalScreen^.BytesPerPixel;
end;

function GetMaxX : Integer;

begin
  GetMaxX:=vga_getxdim;
end;

function GetMAxY : Integer;
begin
  GetMaxY:=vga_getydim;
end;

Procedure DetectGraph (Var Driver,Mode : Integer);

begin
  Driver:=9;
  Mode:=vga_getdefaultmode;  
  If Mode=-1 then mode:=0;
end;

Procedure InitGraph (Var Driver,Mode : Integer;DriverPath : String);

var
  VgaMode: Integer;
  ModeInfo: pvga_modeinfo;

begin
    If Mode=0 then
      VgaMode := vga_getdefaultmode
    else 
      VGAMode :=Mode;
    if (VgaMode = -1) then VgaMode := G320X200X256;
    if (not vga_hasmode(VgaMode))
      then begin
        WriteLn('BGI: Mode not available.');
        Halt(1)
      end;
    ModeInfo := vga_getmodeinfo(VgaMode);
    {IsVirtual := (ModeInfo^.colors = 16) or (ModeInfo^.flags and IS_MODEX <> 0);}
    IsVirtual := true;
    { We always want a back screen (for buffering). }
    if IsVirtual
      then begin
        { Create virtual screen }
        gl_setcontextvgavirtual(VgaMode);
        BackScreen := gl_allocatecontext;
        gl_getcontext(BackScreen)
      end;
    vga_setmode(VgaMode);
    gl_setcontextvga(VgaMode);  { Physical screen context. }
    PhysicalScreen := gl_allocatecontext;
    gl_getcontext(PhysicalScreen);
    if (PhysicalScreen^.colors = 256) then gl_setrgbpalette;
    SetColors;
    SizeX := PhysicalScreen^.Width;
    SizeY := PhysicalScreen^.Height
end;

Procedure CloseGraph;

begin
  DoneVideo;
end;

Function GraphResult : Integer;

begin
  GraphResult:=0;
end;

Procedure GraphDefaults ;

begin
end;

Function GraphErrorMsg (Errcode : Integer) : String;

begin
  GraphErrorMsg:='';
end;


begin

  { Give up root permissions if we are root.  }
  if geteuid = 0 then vga_init;
end.

{
  $Log$
  Revision 1.8  1998-09-11 09:24:55  michael
  Added missing functions so mandel compiles and runs

  Revision 1.7  1998/08/24 08:23:47  michael
  Better initgraph handling.

  Revision 1.6  1998/08/14 09:20:36  michael
  Typo fixed. linklib gl to linklib vgagl

  Revision 1.5  1998/08/12 14:01:08  michael
  small fix in sector, pieslice replaced by call to sector

  Revision 1.4  1998/08/12 13:25:33  michael
  + added arc,ellipse,fillelipse,sector,pieslice

  Revision 1.3  1998/08/10 09:01:58  michael
  + Added some functions to improve compatibility

  Revision 1.2  1998/05/12 10:42:47  peter
    * moved getopts to inc/, all supported OS's need argc,argv exported
    + strpas, strlen are now exported in the systemunit
    * removed logs
    * removed $ifdef ver_above

  Revision 1.1  1998/04/15 13:40:11  michael
  + Initial implementation of graph unit

}
