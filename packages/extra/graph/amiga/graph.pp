unit Graph;

{ *********************************************************************

  Info:

  This units mimics some parts of borland's graph unit for
  Amiga.

  You have to use crt for readln, readkey and stuff like
  that for your programs. When the show is over you should
  just press a key or hit return to close everything down.

  If that doesn't work just flip the screens with left-Amiga n
  and activate the shell you started from.

  I have compiled and run mandel.pp without any problems.

  This version requires Free Pascal 0.99.5c or higher.

  It will also use some amigaunits, when the unit gets
  better we can remove those units.

  Large parts have not yet been implemented or tested.

  nils.sjoholm@mailbox.swipnet.se  (Nils Sjoholm)

  History:

  Date       Version  Who      Comments
  ---------- -------- -------  -------------------------------------
  27-Nov-98  0.1      nsjoholm Initial version.

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


interface

uses Exec, Intuition, Graphics, Utility;

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
function  GetPixel(X, Y: Integer): Integer;

{ Line-oriented primitives }
procedure LineTo(X, Y: Integer);
procedure LineRel(Dx, Dy: Integer);
procedure MoveTo(X, Y: Integer);
procedure MoveRel(Dx, Dy: Integer);
procedure Line(x1, y1, x2, y2: Integer);

{ Linearly bounded primitives }
procedure Rectangle(x1, y1, x2, y2: Integer);
procedure Bar(x1, y1, x2, y2: Integer);
procedure Bar3D(x1, y1, x2, y2: Integer; Depth: Word; Top: Boolean);
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
Function  GetBkColor : Word;
Function  GetColor : Word;
function  GetMaxColor : Word;

function  GetMaxX : Integer;
function  GetMAxY : Integer;
function  GetAspect: Real;
procedure GetAspectRatio(var x,y : Word);

{ Graph clipping method }
Procedure ClearViewPort;

function GraphResult: Integer;

{ For compatibility }
Procedure InitGraph (Var Driver,Mode : Integer;DriverPath : String);
Procedure CloseGraph;

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

{$I tagutils.inc}

{ ---------------------------------------------------------------------
   Types, constants and variables

  ---------------------------------------------------------------------}
VAR     GraphScr     :pScreen;
        GraphWin     :pWindow;
        CurrentRastPort : pRastPort;
        TheAspect   : Real;
        GraphResultCode : Integer;

        Msg     :pIntuiMessage;
        Ende    :Boolean;

var
  DrawDelta: TPoint;
  CurX, CurY: Integer;
  TheColor, TheFillColor: LongInt;
  IsVirtual: Boolean;
  ColorTable: array[0..15] of LongInt;
  TheFillPattern : FillPatternType;
  TheLineSettings : LineSettingsType;
  ThePalette : PaletteType;
  TheTextSettings : TextSettingsType;
  TheFillSettings : FillSettingsType;

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

{ Bitmap utilities }
type
  PBitmap = ^TBitmap;
  TBitmap = record
              Width, Height: Integer;
              Data: record end;
            end;


const
  pbNone  = 0;
  pbCopy  = 1;
  pbClear = 2;

procedure SetColors;
begin
   SetRGB4(@GraphScr^.ViewPort, Black    , 0,0,0);
   SetRGB4(@GraphScr^.ViewPort, Blue     , 0,0,15);
   SetRGB4(@GraphScr^.ViewPort, Green    , 0,15,0);
   SetRGB4(@GraphScr^.ViewPort, Cyan     , 0,15,15);
   SetRGB4(@GraphScr^.ViewPort, Red      , 15,0,0);
   SetRGB4(@GraphScr^.ViewPort, Magenta  , 15,0,15);
   SetRGB4(@GraphScr^.ViewPort, Brown    , 6,2,0);
   SetRGB4(@GraphScr^.ViewPort, LightGray, 13,13,13);
   SetRGB4(@GraphScr^.ViewPort, DarkGray , 4,4,4);
   SetRGB4(@GraphScr^.ViewPort, LightBlue, 5,5,5);
   SetRGB4(@GraphScr^.ViewPort, LightGreen ,9,15,1);
   SetRGB4(@GraphScr^.ViewPort, LightRed   ,14,5,0);
   SetRGB4(@GraphScr^.ViewPort, LightMagenta ,0,15,8);
   SetRGB4(@GraphScr^.ViewPort, Yellow   ,15,15,0);
   SetRGB4(@GraphScr^.ViewPort, White    ,15,15,15);
end;


{ ---------------------------------------------------------------------
   Real graph implementation
  ---------------------------------------------------------------------}

function GraphResult: Integer;
begin
   GraphResult := GraphResultCode;
end;

Procedure ClearViewPort;
begin
   SetRast(CurrentRastPort,Black);
end;

function GetX: Integer;
begin
  GetX := CurX;
end;

function GetY: Integer;
begin
  GetY := CurY;
end;

function GetAspect: Real;
begin
   GetAspect := GetMaxY/GetMaxX;
end;

procedure GetAspectRatio(var x,y : Word);
begin
   x := GetMaxX;
   y := GetMaxY;
end;

{ Pixel-oriented routines }
procedure PutPixel(x,y : Integer; Pixel : Word);
begin
   SetAPen(CurrentRastPort,Pixel);
   WritePixel(CurrentRastPort,x,y);
   CurX := x;
   CurY := y;
end;

function GetPixel(X, Y: Integer): Integer;
begin
   GetPixel := ReadPixel(CurrentRastPort,X,Y);
end;

{ Line-oriented primitives }

procedure LineTo(X, Y: Integer);
begin
   Draw(CurrentRastPort,X,Y);
   CurX := X;
   CurY := Y;
end;

procedure LineRel(Dx, Dy: Integer);
begin
   CurX := CurX + Dx;
   CurY := CurY + Dy;
   Draw(CurrentRastPort, Curx, CurY);
end;

procedure MoveTo(X, Y: Integer);
begin
   Move(CurrentRastPort, X , Y);
   CurX := X;
   CurY := Y;
end;

procedure MoveRel(Dx, Dy: Integer);
begin
   CurX := CurX + Dx;
   CurY := CurY + Dy;
   Move(CurrentRastPort, Curx, CurY);
end;

procedure Line(x1,y1,x2,y2: Integer);
begin
   Move(CurrentRastPort,x1,y1);
   Draw(CurrentRastPort,x2,y2);
   Move(CurrentRastPort,CurX, CurY);
end;

procedure Rectangle(x1, y1, x2, y2: Integer);
begin
   Move(CurrentRastPort, x1, y1);
   Draw(CurrentRastPort, x2, y1);
   Draw(CurrentRastPort, x2, y2);
   Draw(CurrentRastPort, x1, y2);
   Draw(CurrentRastPort, x1, y1);
   CurX := x1;
   CurY := y1;
end;

procedure Bar(x1, y1, x2, y2: Integer);
begin
   RectFill(CurrentRastPort, x1, y1, x2, y2);
   CurX := x1;
   CurY := y1;
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

procedure FloodFill(X, Y: Integer; Border: Word);
begin

end;

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
   DrawEllipse(CurrentRastPort, x, y, Round(Radius * TheAspect), Radius);
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
  //!! Needs implementing.
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
  SetBPen(CurrentRastPort, ColorNum);
  BackColor := ColorNum;
end;

Function GetBkColor : Word;

begin
  GetBkColor:=BackColor;
end;

Function GetColor : Word;

begin
  GetColor:=TheColor;
end;

procedure SetColor(color : Word);
begin
   SetAPen(CurrentRastPort,color);
   TheColor := color;
end;

function GetMaxColor: word;
begin
   GetMaxColor := 15;
end;

function GetMaxX: Integer;
begin
   GetMaxX := GraphWin^.Width;
end;

function GetMaxY: Integer;
begin
   GetMaxY := GraphWin^.Height;
end;

Procedure InitGraph (Var Driver,Mode : Integer;DriverPath : String);
var
  thetags : array[0..3] of tTagItem;

BEGIN
  GraphResultCode := grOK;
  GfxBase := OpenLibrary(GRAPHICSNAME,0);
  if GfxBase = nil then begin
      GraphResultCode := grNoInitGraph;
      Exit;
  end;

  GraphScr:=Nil;  GraphWin:=Nil;

  { Will open an hires interlace screen, if you
    want just an hires screen change HIRESLACE_KEY
    to HIRES_KEY
  }
  thetags[0] := TagItem(SA_Depth,     4);
  thetags[1] := TagItem(SA_DisplayID, HIRESLACE_KEY);
  thetags[2].ti_Tag := TAG_END;

  GraphScr := OpenScreenTagList(NIL,@thetags);
  If GraphScr=Nil Then begin
      GraphResultCode := grNoInitGraph;
      Exit;
  end;

  thetags[0] := TagItem(WA_Flags, WFLG_BORDERLESS);
  thetags[1] := TagItem(WA_IDCMP, IDCMP_MOUSEBUTTONS);
  thetags[2] := TagItem(WA_CustomScreen, Longint(GraphScr));
  thetags[3].ti_Tag := TAG_DONE;

  GraphWin:=OpenWindowTagList(Nil, @thetags);
  If GraphWin=Nil Then CloseGraph;

  CurrentRastPort := GraphWin^.RPort;

  SetColors;
  TheAspect := GetAspect;
END;

PROCEDURE CloseGraph;
BEGIN
  { Ende:=false;
  Repeat
    Msg:=pIntuiMessage(GetMsg(GraphWin^.UserPort));
    If Msg<>Nil Then Begin
      ReplyMsg(Pointer(Msg));
      Ende:=true;
    End;
  Until Ende;}
  If GraphWin<>Nil Then
     CloseWindow(GraphWin);
  If (GraphScr<>Nil) then CloseScreen(GraphScr);
  if GfxBase <> nil then CloseLibrary(GfxBase);
  Halt;
END;

begin

  CurX := 0;
  CurY := 0;
end.
