{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993-98 by Florian Klaempf & Gernot Tenchio
    members of the Free Pascal development team.

    Graph unit for BP7 compatible RTL

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit GRAPH;

{ there are some problems with ranges in this file !! (PM) }
{$R-}
{$Q-}
{ $DEFINE DEBUG}
{$I os.inc}

{$ifdef DEBUG}
{$define TEST_24BPP}
{$define Test_Linear}
{$endif DEBUG}

{ Don't use smartlinking, because of the direct assembler that is used }
{$ifndef VER0_99_8}
{$SMARTLINK OFF}
{$endif not VER0_99_8}

interface

uses go32,mmx;

{$I GLOBAL.PPI}
{$I STDCOLOR.PPI}

procedure CloseGraph;
function  GraphResult : Integer;
procedure InitGraph(var GraphDriver:Integer;var GraphMode:Integer;const PathToDriver:String);
procedure SetGraphMode(GraphMode : integer);
procedure GraphDefaults;
procedure RestoreCRTMode;
procedure SetGraphBufSize(BufSize : longint);
function  RegisterBGIdriver(driver : pointer) : integer;
function  InstallUserDriver(const DriverFileName : string;AutoDetectPtr : pointer) : integer;
function  GetDriverName: String;
function  GetModeName(Mode:Integer):String;
function  GetGraphMode:Integer;
procedure GetAspectRatio(var _Xasp,_Yasp : word);
procedure SetAspectRatio(_Xasp,_Yasp : word);
function GraphErrorMsg(ErrorCode: Integer): string;

function  GetMaxMode : Integer;
function  GetMaxX : Integer;
function  GetMaxY : Integer;
function  GetX : Integer;
function  GetY : Integer;
procedure Bar(x1,y1,x2,y2 : Integer);
procedure bar3D(x1, y1, x2, y2 : integer;depth : word;top : boolean);
procedure GetViewSettings(var viewport : ViewPortType);
procedure SetActivePage(page : word);
procedure SetVisualPage(page : word);
procedure SetWriteMode(WriteMode : integer);
procedure SetViewPort(x1,y1,x2,y2 : integer;clip : boolean);
procedure Cleardevice;
procedure ClearViewport;
procedure Rectangle(x1,y1,x2,y2 : integer);

{ PIXEL.PPI }
function  GetPixel(x,y : integer):longint;
procedure PutPixel(x,y : integer; Colour: longint);

{ LINE.PPI }
procedure Line(x1,y1,x2,y2 : integer);
procedure LineTo(x,y : integer);
procedure LineRel(dx,dy : integer);
procedure MoveTo(x,y : integer);
procedure MoveRel(dx,dy : integer);
procedure GetLineSettings(var LineInfo : LineSettingsType);
procedure SetLineStyle(LineStyle : word;pattern : word;thickness : word);

procedure DrawPoly(points : word;var polypoints);

{ PALETTE.PPI }
procedure GetRGBPalette(ColorNum:byte; var RedValue,GreenValue,BlueValue:byte);
procedure SetRGBPalette(ColorNum,RedValue,GreenValue,BlueValue:byte);
procedure SetAllPalette(var Palette : PaletteType);
procedure GetPalette(var Palette : PaletteType);
procedure SetPalette(ColorNum:word;Color:byte);

{ ELLIPSE.PPI }
procedure FillEllipse(x,y:Integer;XRadius,YRadius:Word);
procedure Circle(x,y:Integer;Radius:Word);
procedure Ellipse(x,y,alpha,beta:Integer;XRad,YRad:word);
procedure Sector(X,Y,alpha,beta:integer;XRadius,YRadius: Word);

{ ARC.PPI }
procedure Arc(x,y,alpha,beta:Integer;Radius:Word);
procedure GetArcCoords(var ArcCoords:ArcCoordsType);
procedure PieSlice(X,Y,alpha,beta:integer;Radius: Word);

{ COLORS.PPI }
function  GetBkColor : longint;
function  GetColor : longint;
function  GetMaxColor : longint;
procedure SetColor(Color : longint);
procedure SetBkColor(Color : longint);

{ FILL.PPI }
procedure FloodFill(x,y:integer; Border:longint);
procedure GetFillSettings(var FillInfo : FillSettingsType);
procedure GetFillPattern(var FillPattern : FillPatternType);
procedure SetFillStyle(pattern : word;color : longint);
procedure SetFillPattern(pattern : FillPatternType;color : longint);
{ just dummy not implemented yet }
procedure FillPoly(points : word;var polypoints);

{ IMAGE.PPI }
function  ImageSize(x1,y1,x2,y2 : integer) : longint;
procedure GetImage(x1,y1,x2,y2 : integer;var BitMap);
procedure PutImage(x,y : integer;var BitMap;BitBlt : word);

{ TEXT.PPI }
procedure GetTextSettings(var TextInfo : TextSettingsType);
procedure OutText(const TextString : string);
procedure OutTextXY(x,y : integer;const TextString : string);
procedure OutText(const Charakter : char);
procedure OutTextXY(x,y : integer;const Charakter : char);
procedure SetTextJustify(horiz,vert : word);
procedure SetTextStyle(Font, Direction : word; CharSize : word);
procedure SetUserCharSize(Multx,Divx,Multy,Divy : word);
function  TextHeight(const TextString : string) : word;
function  TextWidth(const TextString : string) : word;
function  RegisterBGIfont(font : pointer) : integer;
function  InstallUserFont(const FontFileName : string) : integer;

{ extended non Borland-compatible }

{ TRIANGLE.PPI }
procedure FillTriangle(A,B,C:Pointtype);

{ to compare colors on different resolutions }
function ColorsEqual(c1,c2 : longint) : boolean;
{ this will return true if the two colors will appear
  equal in the current video mode }
  
procedure WaitRetrace;
{$ifdef debug}
procedure pixel(offset:longint);
function  Convert(color:longint):longint;
function  UnConvert(color:longint):longint;
{$endif debug}

{$ifdef Test_linear}
  const
     UseLinear : boolean = false;
     { the two below are the settings the work for ATI 3D Rage Pro !! }
     switch_physical_address : boolean = true;
     split_physical_address : boolean = false;
{$endif Test_linear}

{$I MODES.PPI}

implementation

{$ASMMODE DIRECT}

type
  PString=^String;
  PInteger=^integer;
  PWord=^word;
  PLong=^longint;

  VgaInfoBlock = record
    VESASignature: array[1..4]of Char;
    VESAloVersion: Byte;
    VESAhiVersion: Byte;
    OEMStringPtr : longint;
    Capabilities : longint;
    VideoModePtr : longint;
    TotalMem     : word;
  { VESA 2.0 }
    OEMversion   : word;
    VendorPtr    : longint;
    ProductPtr   : longint;
    RevisionPtr  : longint;
    filler       : Array[1..478]of Byte;
  end;

  VesaInfoBlock=record
    ModeAttributes : word;
    WinAAttributes : byte;
    WinBAttributes : byte;
    WinGranularity : word;
    WinSize        : word;
    segWINA        : word;
    segWINB        : word;
    RealWinFuncPtr : longint;
    BPL            : word;
  { VESA 1.2 }
    XResolution    : word;
    YResolution    : word;
    XCharSize      : byte;
    YCharSize      : byte;
    MumberOfPlanes : byte;
    BitsPerPixel   : byte;
    NumberOfBanks  : byte;
    MemoryModel    : byte;
    BankSize       : byte;
    NumberOfPages  : byte;
    reserved       : byte;
    rm_size        : byte;
    rf_pos         : byte;
    gm_size        : byte;
    gf_pos         : byte;
    bm_size        : byte;
    bf_pos         : byte;
    res_mask       : word;
    DirectColorInfo: byte;
  { VESA 2.0 }
    PhysAddress    : longint;
    OffscreenPtr   : longint;
    OffscreenMem   : word;
    reserved2      : Array[1..458]of Byte;
   end;

const
     CheckRange    : Boolean=true;
     isVESA2       : Boolean=false;
     core          : longint=$E0000000;

var    { X/Y Verhaeltnis des Bildschirm }
       AspectRatio  : real;
       XAsp , YAsp  : Word;
       { Zeilen & Spalten des aktuellen Graphikmoduses }
       _maxx,_maxy : longint;
       { Current color internal format (depending on bitsperpixel) }
       aktcolor : longint;
       { Current color RGB value }
       truecolor : longint;
       { Current background color internal format (depending on bitsperpixel) }
       aktbackcolor : longint;
       { Current background color RGB value }
       truebackcolor : longint;
       { Videospeicherbereiche }
       wbuffer,rbuffer,wrbuffer : ^byte;
       { aktueller Ausgabebereich }
       aktviewport : ViewPortType;
       aktscreen   : ViewPortType;
       { der Graphikmodus, der beim Start gesetzt war }
       startmode : byte;
       { Position des Graphikcursors }
       curx,cury : longint;
       { true, wenn die Routinen des Graphikpaketes verwendet werden dÅrfen }
       isgraphmode : boolean;
       { Einstellung zum Linien zeichnen }
       aktlineinfo : LineSettingsType;
       { Fehlercode, wird von graphresult zurÅckgegeben }
       _graphresult : integer;
       { aktuell eingestellte FÅllart }
       aktfillsettings : FillSettingsType;
       aktfillbkcolor : longint;
       { aktuelles FÅllmuster }
       aktfillpattern : FillPatternType;
       { Schreibmodus }
       aktwritemode : word;
       { put background color around text }
       ClearText : boolean;
       { Schrifteinstellung }
       akttextinfo : TextSettingsType;
       { momentan gesetzte Textskalierungswerte }
       aktmultx,aktdivx,aktmulty,aktdivy : word;
       { Pfad zu den Fonts }
       bgipath : string;
       { Pointer auf Hilfsspeicher }
       buffermem : pointer;
       { momentane Grî·e des Buffer }
       buffersize : longint;
       { in diesem Puffer werden bei SetFillStyle bereits die Pattern in der }
       { zu verwendenden Farbe abgelegt }
       PatternBuffer : Array[0..63]of LongInt;

       X_Array         : array[0..1280]of LongInt;
       Y_Array         : array[0..1024]of LongInt;

       Sel,Seg      : word;
       VGAInfo      : VGAInfoBlock;
       VESAInfo     : VESAInfoBlock;
   { Selectors for Protected Mode }
       seg_WRITE    : word;
       seg_READ     : word;
   { linear Frame Buffer }
       LinearFrameBufferSupported : boolean;
       FrameBufferLinearAddress : longint;
       UseLinearFrameBuffer : Boolean;
   const
       EnableLinearFrameBuffer = $4000;
   { Registers for RealModeInterrupts in DPMI-Mode }
   var
       dregs        : TRealRegs;
       { read and write bank are allways equal !! }
       A_Bank       : longint;
       AW_window    : longint;
       AR_Window    : longint;
       same_window  : boolean;
   const
       AWindow = 0;
       BWindow = 1;
       
   { Variables for Bankswitching }
   var
       BytesPerLine : longint;
       BytesPerPixel: Word;
       WinSize      : longint;   { Expample $0x00010000 . $0x00008000 }
       WinLoMask    : longint;   {          $0x0000FFFF   $0x00007FFF }
       WinLoMaskMinusPixelSize : longint;   {          $0x0000FFFF   $0x00007FFF }
       WinShift     : byte;
       GranShift    : byte;
       Granular     : longint;
       Granularity  : longint;
       graphgetmemptr,
       graphfreememptr,
       bankswitchptr :pointer;
       isDPMI        :Boolean;
       SwitchCS,SwitchIP : word;


function ColorsEqual(c1,c2 : longint) : boolean;
Begin
  ColorsEqual:=((BytesPerPixel=1) and ((c1 and $FF)=(c2 and $FF))) or
         ((GetMaxColor=$7FFF) and ((c1 and $F8F8F8)=(c2 and $F8F8F8))) or
         ((GetMaxColor=$FFFF) and ((c1 and $F8FCF8)=(c2 and $F8FCF8))) or
         ((BytesPerPixel>2) and ((c1 and $FFFFFF)=(c2 and $FFFFFF)));
End;
   
function GraphErrorMsg(ErrorCode: Integer): string;
Begin
 GraphErrorMsg:='';
 case ErrorCode of
  grOk,grFileNotFound,grInvalidDriver: exit;
  grNoInitGraph: GraphErrorMsg:='Graphics driver not installed';
  grNotDetected: GraphErrorMsg:='Graphics hardware not detected';
  grNoLoadMem,grNoScanMem,grNoFloodMem: GraphErrorMsg := 'Not enough memory for graphics';
  grNoFontMem: GraphErrorMsg := 'Not enough memory to load font';
  grFontNotFound: GraphErrorMsg:= 'Font file not found';
  grInvalidMode: GraphErrorMsg := 'Invalid graphics mode';
  grError: GraphErrorMsg:='Graphics error';
  grIoError: GraphErrorMsg:='Graphics I/O error';
  grInvalidFont,grInvalidFontNum: GraphErrorMsg := 'Invalid font';
  grInvalidVersion: GraphErrorMsg:='Invalid driver version';
 end;
end;



procedure Oh_Kacke(ErrString:String);
begin
  CloseGraph;
  writeln('Error in Unit VESA: ',ErrString);
  halt;
end;

{$I MOVE.PPI}
{$I IBM.PPI}

procedure WaitRetrace;
begin
  asm
    cli
    movw  $0x03Da,%dx
WaitNotHSyncLoop:
    inb   %dx,%al
    testb $0x8,%al
    jnz   WaitNotHSyncLoop
WaitHSyncLoop:
    inb   %dx,%al
    testb $0x8,%al
    jz    WaitHSyncLoop
    sti
  end;
end;

procedure getmem(var p : pointer;size : longint);
begin
  asm
    pushl 12(%ebp)
    pushl 8(%ebp)
    movl _GRAPHGETMEMPTR,%eax
    call %eax
  end;
end;

procedure freemem(var p : pointer;size : longint);
begin
  asm
    pushl 12(%ebp)
    pushl 8(%ebp)
    movl _GRAPHFREEMEMPTR,%eax
    call %eax
  end;
end;


{$I COLORS.PPI}

procedure graphdefaults;
      begin
         _graphresult:=grOk;
         if not isgraphmode then
           begin
              _graphresult:=grnoinitgraph;
              exit;
           end;
         { Linientyp }
         aktlineinfo.linestyle:=solidln;
         aktlineinfo.thickness:=normwidth;


         { std colors }
         setstdcolors;
         { Zeichenfarbe }
         aktcolor:=convert(white);
         aktbackcolor:=convert(black);

         { FÅllmuster }
         setfillstyle(solidfill,white);
         { necessary to load patternbuffer !! (PM)
         aktfillsettings.color:=white;
         aktfillsettings.pattern:=solidfill;      }

         { Viewport setzen }
         aktviewport.clip:=true;
         aktviewport.x1:=0;
         aktviewport.y1:=0;
         aktviewport.x2:=_maxx-1;
         aktviewport.y2:=_maxy-1;

         aktscreen:=aktviewport;

         { normaler Schreibmodus }
         setwritemode(normalput);

         { Schriftart einstellen }
         akttextinfo.font:=DefaultFont;
         akttextinfo.direction:=HorizDir;
         akttextinfo.charsize:=1;
         akttextinfo.horiz:=LeftText;
         akttextinfo.vert:=TopText;

         { Vergrî·erungsfaktoren}
         XAsp:=10000; YAsp:=10000;
         aspectratio:=1;
      end;

{ ############################################################### }
{ #################  Ende der internen Routinen  ################ }
{ ############################################################### }

{$I PALETTE.PPI}
{$I PIXEL.PPI}
{$I LINE.PPI}
{$I ELLIPSE.PPI}
{$I TRIANGLE.PPI}
{$I ARC.PPI}
{$I IMAGE.PPI}
{$I TEXT.PPI}
{$I FILL.PPI}

function GetDrivername:String;
begin
  if not isgraphmode then
    begin
      _graphresult:=grNoInitGraph;
      Exit;
    end;
  GetDriverName:=('internal VESA-Driver');
end;

function GetModeName(Mode:Integer):String;
var s1,s2,s3:string;
begin
  if not isgraphmode then
    begin
      _graphresult:=grNoInitGraph;
      Exit;
    end;
  str(_maxx,s1);
  str(_maxy,s2);
  str(getmaxcolor+1,s3);
  GetModeName:=('VESA '+s1+'x'+s2+'x'+s3);
end;

function GetGraphMode:Integer;
begin
  if not isgraphmode then
    begin
      _graphresult:=grNoInitGraph;
      Exit;
    end;
  GetGraphMode:=GetVesaMode;
end;

procedure ClearViewport;
var bank1,bank2,diff,c:longint;
    ofs1,ofs2         :longint;
    y : integer;
    storewritemode : word;
    
begin
  if not isgraphmode then
    begin
      _graphresult:=grNoInitGraph;
      Exit;
    end;
  c:=aktcolor;
  aktcolor:=aktbackcolor;
  storewritemode:=aktwritemode;
  aktwritemode:=normalput;
  ofs1:=Y_ARRAY[aktviewport.y1] + X_ARRAY[aktviewport.x1] ;
  ofs2:=Y_ARRAY[aktviewport.y1] + X_ARRAY[aktviewport.x2] ;
  for y:=aktviewport.y1 to aktviewport.y2 do
  begin
    bank1:=ofs1 shr winshift;
    bank2:=ofs2 shr winshift;
    if bank1 <> A_BANK then
    begin
      Switchbank(bank1);
    end;
    if bank1 <> bank2 then
    begin
      diff:=((bank2 shl winshift)-ofs1) div BytesPerPixel;
      horizontalline(aktviewport.x1, aktviewport.x1+diff-1, y);
      Switchbank(bank2);
      horizontalline(aktviewport.x1+diff, aktviewport.x2, y);
    end else horizontalline(aktviewport.x1, aktviewport.x2, y);
    ofs1:=ofs1 + BytesPerLine;
    ofs2:=ofs2 + BytesPerLine;
  end;
  aktwritemode:=storewritemode;
  aktcolor:=c;
end;

procedure GetAspectRatio(var _Xasp,_Yasp : word);
begin
  _graphresult:=grOk;
  if not isgraphmode then
    begin
      _graphresult:=grnoinitgraph;
      exit;
    end;
    _XAsp:=XAsp; _YAsp:=YAsp;
end;

procedure SetAspectRatio(_Xasp, _Yasp : word);
begin
  _graphresult:=grOk;
    if not isgraphmode then
      begin
        _graphresult:=grnoinitgraph;
        exit;
      end;
    Xasp:=_XAsp; YAsp:=_YAsp;
end;


procedure ClearDevice;
var Viewport:ViewportType;
begin
  if not isgraphmode then
    begin
      _graphresult:=grNoInitGraph;
      Exit;
    end;
  Viewport:=aktviewport;
  SetViewport(0,0,_maxx-1,_maxy-1,Clipon);
  ClearViewport;
  aktviewport:=viewport;
end;

procedure Rectangle(x1,y1,x2,y2:integer);
begin
  if not isgraphmode then
    begin
      _graphresult:=grNoInitGraph;
      Exit;
    end;
  Line(x1,y1,x2,y1);
  Line(x1,y1,x1,y2);
  Line(x2,y1,x2,y2);
  Line(x1,y2,x2,y2);
end;

procedure Bar(x1,y1,x2,y2:integer);
var y               : Integer;
    origcolor       : longint;
    origlinesettings: Linesettingstype;
begin
  if not isgraphmode then
    begin
      _graphresult:=grNoInitGraph;
      Exit;
    end;
  origlinesettings:=aktlineinfo;
  origcolor:=aktcolor;
  aktlineinfo.linestyle:=solidln;
  aktlineinfo.thickness:=normwidth;
  case aktfillsettings.pattern of
     emptyfill : begin
           aktcolor:=aktbackcolor;
           for y:=y1 to y2 do line(x1,y,x2,y);
         end;
     solidfill : begin
           aktcolor:=aktfillsettings.color;
           for y:=y1 to y2 do line(x1,y,x2,y);
         end;
     else for y:=y1 to y2 do patternline(x1,x2,y);
  end;
  aktcolor:=origcolor;
  aktlineinfo:=origlinesettings;
end;

procedure bar3D(x1, y1, x2, y2 : integer;depth : word;top : boolean);
begin
  if not isgraphmode then
    begin
      _graphresult:=grNoInitGraph;
      Exit;
    end;
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

procedure SetGraphBufSize(BufSize : longint);
begin
  if assigned(buffermem) then
  freemem(buffermem,buffersize);
  getmem(buffermem,bufsize);
  if not assigned(buffermem) then
    buffersize:=0
  else buffersize:=bufsize;
end;

const
  { Vorgabegrî·e fÅr Hilfsspeicher }
  bufferstandardsize = 64*8196;      { 0,5 MB }

procedure CloseGraph;
begin
  if isgraphmode then
    begin
       SetVESAMode(startmode);
       { DoneVESA; only in exitproc !! PM }
       isgraphmode:=false;
       if assigned(buffermem) then
         freemem(buffermem,buffersize);
       buffermem:=nil;
       buffersize:=0;
     end;
end;

procedure InitGraph(var GraphDriver:Integer;var GraphMode:Integer;const PathToDriver:String);
var i,index:Integer;
begin
    { Pfad zu den Fonts }
    bgipath:=PathToDriver;
    if bgipath[length(bgipath)]<>'\' then
    bgipath:=bgipath+'\';
  if Graphdriver=detect then GraphMode:=GetMaxMode;
    { Standardfonts installieren }
    InstallUserFont('TRIP');
    InstallUserFont('LITT');
    InstallUserFont('SANS');
    InstallUserFont('GOTH');
    InstallUserFont('SCRI');
    InstallUserFont('SIMP');
    InstallUserFont('TSCR');
    InstallUserFont('LCOM');
    InstallUserFont('EURO');
    InstallUserFont('BOLD');

  GetVESAInfo(GraphMode);
{$IFDEF DEBUG}
   {$I VESADEB.PPI}
{$ENDIF}
  for i:=VESANumber downto 0 do
    if GraphMode=VESAModes[i] then break;
  { the modes can be refused due to the monitor ? }
  { that happens by me at home Pierre Muller      }
  while i>=0 do begin
    isgraphmode:=SetVESAMode(GraphMode);
    if isgraphmode then begin
      GetVESAInfo(GraphMode);
      if UseLinearFrameBuffer then
        isgraphmode:=SetVESAMode(GraphMode or EnableLinearFrameBuffer);
      for index:=0 to VESAInfo.XResolution do X_Array[index]:=index * BytesPerPixel;
      for index:=0 to VESAInfo.YResolution do Y_Array[index]:=index * BytesPerLine;
      SetGraphBufSize(bufferstandardsize);
      graphdefaults;
      exit;
    end;
    dec(i);
    GraphMode:=VESAModes[i];
  end;
  _graphresult:=grInvalidMode
end;

procedure SetGraphMode(GraphMode:Integer);

var index:Integer;
begin
   _graphresult:=grOk;
   if not isgraphmode then
    begin
      _graphresult:=grNoInitGraph;
      Exit;
    end;
    if GetVesaInfo(GraphMode) then
      begin
         isgraphmode:=SetVESAMode(GraphMode);
         if isgraphmode then
           begin
              if UseLinearFrameBuffer then
                isgraphmode:=SetVESAMode(GraphMode or EnableLinearFrameBuffer);
              for index:=0 to VESAInfo.XResolution do
                X_Array[index]:=index * BytesPerPixel;
              for index:=0 to VESAInfo.YResolution do
                Y_Array[index]:=index * BytesPerLine;
              graphdefaults;
              exit;
           end;
      end;
    _graphresult:=grInvalidMode;
end;

function RegisterBGIdriver(driver : pointer) : integer;
begin
   RegisterBGIdriver:=grerror;
end;

function InstallUserDriver(const DriverFileName : string;AutoDetectPtr : pointer) : integer;
begin
   installuserdriver:=grerror;
end;

function GetMaxMode:Integer;
var i:Byte;
begin
  for i:=VESANumber downto 0 do
    if GetVesaInfo(VESAModes[i]) then
    begin
       GetMaxMode:=VESAModes[i];
       Exit;
    end;
end;

function GetMaxX:Integer;
begin
  if not isgraphmode then
    begin
      _graphresult:=grNoInitGraph;
      Exit;
    end;
    GetMaxX:=VESAInfo.XResolution-1;
end;

function GetMaxY:Integer;
begin
  if not isgraphmode then
    begin
      _graphresult:=grNoInitGraph;
      Exit;
    end;
    GetMaxY:=VESAInfo.YResolution-1;
end;

function GetX : integer;
begin
  _graphresult:=grOk;
  if not isgraphmode then
   begin
     _graphresult:=grNoInitGraph;
     Exit;
   end;
   GetX:=curx;
end;

function GetY : integer;
begin
  _graphresult:=grOk;
  if not isgraphmode then
   begin
     _graphresult:=grNoInitGraph;
     Exit;
   end;
GetY:=cury;
end;

procedure SetViewPort(x1,y1,x2,y2 : integer;clip : boolean);

begin
  _graphresult:=grOk;
  if not isgraphmode then
    begin
      _graphresult:=grNoInitGraph;
      exit;
    end;
  { Daten ÅberprÅfen }
  if (x1<0) or (y1<0) or (x2>=_maxx) or (y2>=_maxy) then exit;
  aktviewport.x1:=x1;
  aktviewport.y1:=y1;
  aktviewport.x2:=x2;
  aktviewport.y2:=y2;
  aktviewport.clip:=clip;
end;

procedure GetViewSettings(var viewport : ViewPortType);

begin
  _graphresult:=grOk;
  if not isgraphmode then
    begin
    _graphresult:=grNoInitGraph;
    exit;
  end;
  viewport:=aktviewport;
end;

{ mehrere Bildschirmseiten werden nicht unterstÅtzt }
{ Dummy aus KompatibilitÑtsgrÅnden                  }
procedure SetVisualPage(page : word);

begin
  _graphresult:=grOk;
  if not isgraphmode then
    begin
      _graphresult:=grNoInitGraph;;
      exit;
    end;
end;

{ mehrere Bildschirmseiten werden nicht unterstÅtzt }
{ Dummy aus KompatibilitÑtsgrÅnden                  }
procedure SetActivePage(page : word);

  begin
     _graphresult:=grOk;
     if not isgraphmode then
       begin
         _graphresult:=grNoInitGraph;;
          exit;
       end;
  end;

procedure SetWriteMode(WriteMode : integer);
begin
  _graphresult:=grOk;
  if not isgraphmode then
    begin
      _graphresult:=grNoInitGraph;;
      exit;
    end;
  if ((writemode and 7)<>xorput) and ((writemode and 7)<>normalput) then
   begin
      _graphresult:=grError;
      exit;
   end;
  aktwritemode:=(writemode and 7);
  if (writemode and BackPut)<>0 then
    ClearText:=true
  else
    ClearText:=false;
end;

function GraphResult:Integer;
begin
  GraphResult:=_graphresult;
end;

procedure RestoreCRTMode;
begin
  if not isgraphmode then
    begin
      _graphresult:=grNoInitGraph;
      Exit;
    end;
  SetVESAMode(startmode);
  isgraphmode:=false;
end;

var PrevExitProc : pointer;

procedure GraphExit;
begin
  ExitProc:=PrevExitProc;
  CloseGraph;
  DoneVesa; { frees the ldt descriptos seg_read and seg_write !! }
end;

begin
  InitVESA;
  if not DetectVESA then
    Oh_Kacke('VESA-BIOS not found...');
  startmode:=GetVESAMode;
  bankswitchptr:=@switchbank;
  GraphGetMemPtr:=@system.getmem;
  GraphFreeMemPtr:=@system.freemem;
  Getdefaultfont;
  if not isDPMI then begin
   wrbuffer:=pointer($D0000000);
   rbuffer:=pointer($D0200000);
   wbuffer:=pointer($D0200000);
  end else begin
   wrbuffer:=pointer($0);
   rbuffer:=pointer($0);
   wbuffer:=pointer($0);
  end;
end.

{
  $Log$
  Revision 1.9  1998-11-19 15:09:33  pierre
    * several bugfixes for sector/ellipse/floodfill
    + graphic driver mode const in interface G800x600x256...
    + added backput mode as in linux graph.pp
      (clears the background of textoutput)

  Revision 1.8  1998/11/19 09:48:45  pierre
    + added some functions missing like sector ellipse getarccoords
      (the filling of sector and ellipse is still buggy
       I use floodfill but sometimes the starting point
       is outside !!)
    * fixed a bug in floodfill for patterns
      (still has problems !!)

  Revision 1.7  1998/11/18 09:31:29  pierre
    * changed color scheme
      all colors are in RGB format if more than 256 colors
    + added 24 and 32 bits per pixel mode
      (compile with -dDEBUG)
      24 bit mode with banked still as problems on pixels across
      the bank boundary, but works in LinearFrameBufferMode
      Look at install/demo/nmandel.pp

  Revision 1.6  1998/10/22 09:44:57  pierre
   * PatternBuffer was not set on entry !!

  Revision 1.5  1998/09/16 16:47:25  peter
    * merged fixes

  Revision 1.4.2.1  1998/09/16 16:15:41  peter
    * no smartlinking!

  Revision 1.4  1998/05/31 14:18:14  peter
    * force att or direct assembling
    * cleanup of some files

  Revision 1.3  1998/05/22 00:39:23  peter
    * go32v1, go32v2 recompiles with the new objects
    * remake3 works again with go32v2
    - removed some "optimizes" from daniel which were wrong

}
