{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    TPostScriptCanvas implementation.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{ ---------------------------------------------------------------------
  This code is heavily based on Tony Maro's initial TPostScriptCanvas
  implementation in the LCL, but was adapted to work with the custom
  canvas code and to work with streams instead of strings.
  ---------------------------------------------------------------------}


{$mode objfpc}
{$H+}

{$IFNDEF FPC_DOTTEDUNITS}
unit pscanvas;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils,FpImage,FpImage.Canvas,FpImage.ImageCanvas;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils,fpimage,fpcanvas,fpimgcanv;
{$ENDIF FPC_DOTTEDUNITS}

type
  TPostScript = class;

  TPSPaintType = (ptColored, ptUncolored);
  TPSTileType = (ttConstant, ttNoDistortion, ttFast);
  TPostScriptCanvas = class; // forward reference

  {Remember, modifying a pattern affects that pattern for the ENTIRE document!}
  TPSPattern = class(TFPCanvasHelper)
  private
    FStream : TMemoryStream;
    FPatternCanvas : TPostScriptCanvas;
    FOldName: String;
    FOnChange: TNotifyEvent;
    FBBox: TRect;
    FName: String;
    FPaintType: TPSPaintType;
    FPostScript: TStringList;
    FTilingType: TPSTileType;
    FXStep: Real;
    FYStep: Real;
    function GetpostScript: TStringList;
    procedure SetBBox(const AValue: TRect);
    procedure SetName(const AValue: String);
    procedure SetPaintType(const AValue: TPSPaintType);
    procedure SetTilingType(const AValue: TPSTileType);
    procedure SetXStep(const AValue: Real);
    procedure SetYStep(const AValue: Real);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure Changed;
    property BBox: TRect read FBBox write SetBBox;
    property PaintType: TPSPaintType read FPaintType write SetPaintType;
    property TilingType: TPSTileType read FTilingType write SetTilingType;
    property XStep: Real read FXStep write SetXStep;
    property YStep: Real read FYStep write SetYStep;
    property Name: String read FName write SetName;
    property GetPS: TStringList read GetPostscript;
    property OldName: string read FOldName write FOldName; // used when notifying that name changed
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    Property PatternCanvas : TPostScriptCanvas Read FPatternCanvas;
  end;
  PPSPattern = ^TPSPattern; // used for array

  { Pen and brush object both right now...}
  TPSPen = class(TFPCustomPen)
  private
    FPattern: TPSPattern;
    procedure SetPattern(const AValue: TPSPattern);
  public
    destructor Destroy; override;
    property Pattern: TPSPattern read FPattern write SetPattern;
    function AsString: String;
  end;

  TPSBrush = Class(TFPCustomBrush)
  Private
    Function GetAsString : String;
  Public
    Property AsString : String Read GetAsString;
  end;

  TPSFont = Class(TFPCustomFont)
  end;

  EPostScriptCanvas = class(Exception);

  { Custom canvas-like object that handles postscript code }
  TPostScriptCanvas = class(TFPCustomCanvas)
  private
    FHeight,FWidth : Integer;
    FStream : TStream;
    FLineSpacing: Integer;
    LastX: Integer;
    LastY: Integer;
    FHashWidth: Integer;
    FHatchOrigin: THatchOrigin;
    FRelativeBrushImage: Boolean;
    FNonZeroWindingRule: Boolean;
    FEvenOdd: Boolean;
    FShadow: Boolean;
    FShadowImage, FSnapshot: TFPMemoryImage;
    FShadowCanvas: TFPImageCanvas;
    FNopPenStyle: TFPPenStyle;
    FNopPen: Boolean;
    function TranslateY(Ycoord: Integer): Integer; // Y axis is backwards in postscript
    procedure AddFill(const aShapeOrigin: TPoint; aEllipse: Boolean = False);
    // Returns the fill operator of the current path: eofill for an even-odd polygon.
    function FillOperator: String;
    procedure WriteHatchFill(const aShapeOrigin: TPoint; aEllipse: Boolean);
    procedure WritePatternFill;
    procedure WriteImageFill(const aOrigin: TPoint);
    // Starts clipping to ClipRect when Clipping is on.
    procedure BeginClip;
    // Ends the clipping started by BeginClip.
    procedure EndClip;
    // Returns the index in PSCoreFonts of the font for Font.
    function FontIndex: Integer;
    // Returns Font.Size, or 10 when it is not set.
    function FontSize: Integer;
    // Selects the core font of Font, re-encoded for ISO 8859-1.
    procedure WriteFont;
    // Returns the width of the ISO 8859-1 text aText in points.
    function TextPoints(const aText: RawByteString): Double;
    // Writes aImage stretched over the aWidth x aHeight device pixels from (aX, aY); when aBlend, pixels
    // below half opacity are left unpainted.
    procedure WriteImage(aX, aY, aWidth, aHeight: Integer; aImage: TFPCustomImage; aInterpolate, aBlend: Boolean);
    // Adds the arc of the ellipse at (acx, acy) with radii arx, ary to the path, from the ray at aStart16
    // over aLength16, in 1/16 degree counter-clockwise from 3 o'clock.
    procedure WriteArc(acx, acy, arx, ary: Double; aStart16, aLength16: Integer);
    // Draws the pie of the ellipse at (acx, acy) with radii arx, ary, from the ray at aStart16 over
    // aLength16, in 1/16 degree counter-clockwise from 3 o'clock.
    procedure WritePie(acx, acy, arx, ary: Double; aStart16, aLength16: Integer; const aOrigin: TPoint);
    procedure SetShadow(aValue: Boolean);
    // Creates or frees the shadow raster to follow Shadow and the canvas size.
    procedure UpdateShadow;
    // Copies the drawing state to the shadow canvas.
    procedure SyncShadow;
    // Returns whether aColor combines with the destination under DrawingMode.
    function BlendsColor(const aColor: TFPColor): Boolean;
    // Starts a shape with a pen and/or a fill. Returns True when its result depends on the pixels under
    // it: the shape is then drawn only in the shadow raster and EndShape writes the changed pixels.
    function StartShape(aPen, aFill: Boolean): Boolean;
    // Ends the shape started by StartShape, writing the patch when aPatch.
    procedure EndShape(aPatch: Boolean);
    // Copies the shadow raster to FSnapshot.
    procedure TakeSnapshot;
    // Writes the shadow pixels that differ from FSnapshot as a masked image.
    procedure WritePatch;
    // Writes the pixel outline of the region aMask of the aWidth x aHeight box at aLeft, aTop as a path.
    procedure WriteRegionPath(const aMask: array of Boolean; aLeft, aTop, aWidth, aHeight: Integer);
    procedure PSLineTo(X1, Y1: Integer);
    procedure PSLine(X1, Y1, X2, Y2: Integer);
    procedure PSPolyline(const Points: array of TPoint);
    procedure PSPolygon(const Points: array of TPoint);
    procedure PSPolygonFill(const Points: array of TPoint);
    procedure PSPolyBezier(Points: PPoint; NumPts: Integer; Filled, Continuous: Boolean);
    procedure PSArc(const R: TRect; Angle16Deg, Angle16DegLength: Integer);
    procedure ResetPos; // reset back to last moveto location
    procedure SetWidth (AValue : integer); override;
    function  GetWidth : integer; override;
    procedure SetHeight (AValue : integer); override;
    function  GetHeight : integer; override;
  Protected
    Procedure WritePS(Const Cmd : String);
    Procedure WritePS(Const Fmt : String; Args : Array of Const);
    // Writes the colour and width of the pen.
    procedure WritePen;
    procedure DrawRectangle(const Bounds: TRect; DoFill : Boolean);
    procedure DrawEllipse(const Bounds: TRect; DoFill : Boolean);
    // Fills the device pixel (x, y) with Value.
    procedure SetColor(x, y: Integer; const Value: TFPColor); override;
    // Returns the shadow raster pixel at (x, y); raises EPostScriptCanvas without Shadow.
    function GetColor(x, y: Integer): TFPColor; override;
    procedure DoGetTextSize(text: AnsiString; var w, h: Integer); override;
    function DoGetTextHeight(text: AnsiString): Integer; override;
    function DoGetTextWidth(text: AnsiString): Integer; override;
    procedure DoCopyRect(x, y: Integer; canvas: TFPCustomCanvas; const SourceRect: TRect); override;
    procedure DoDraw(x, y: Integer; const image: TFPCustomImage); override;
    procedure DoRadialPie(x1, y1, x2, y2, StartAngle16Deg, Angle16DegLength: Integer); override;
    // Fills the region of the colour at (x, y) in the shadow raster with the brush.
    procedure DoFloodFill(x, y: Integer); override;
  public
    constructor Create(AStream : TStream);
    destructor Destroy; override;
    function DoCreateDefaultFont : TFPCustomFont; override;
    function DoCreateDefaultPen : TFPCustomPen; override;
    function DoCreateDefaultBrush : TFPCustomBrush; override;
    property LineSpacing: Integer read FLineSpacing write FLineSpacing;
    // The distance between the lines of hatch brushes.
    property HashWidth: Integer read FHashWidth write FHashWidth;
    // Where hatch brushes start counting their lines.
    property HatchOrigin: THatchOrigin read FHatchOrigin write FHatchOrigin;
    // Whether polygons fill by the non-zero winding rule instead of the even-odd rule.
    property PolygonNonZeroWindingRule: Boolean read FNonZeroWindingRule write FNonZeroWindingRule;
    // Whether bsImage brushes tile from the shape instead of the canvas origin.
    property RelativeBrushImage: Boolean read FRelativeBrushImage write FRelativeBrushImage;
    // Whether a raster copy of the page is kept for pixel reads, flood fills and destination-dependent modes.
    property Shadow: Boolean read FShadow write SetShadow;
    // Clears the shadow raster to a white page.
    procedure ClearShadow;
    Procedure DoMoveTo(X1,Y1 : Integer); override;
    Procedure DoLineTo(X1,Y1 : Integer); override;
    Procedure DoLine(X1,Y1,X2,Y2 : Integer); override;
    Procedure DoRectangle(Const Bounds : TRect); override;
    Procedure DoRectangleFill(Const Bounds : TRect); override;
    procedure DoPolyline(Const Points: Array of TPoint); override;
    // Strokes the closed outline through Points with the pen.
    procedure DoPolygon(const Points: array of TPoint); override;
    // Fills the polygon through Points with the brush.
    procedure DoPolygonFill(const Points: array of TPoint); override;
    // Writes the UTF-8 Text with its baseline at (X, Y) in the core font of Font.
    procedure DoTextOut(X, Y: Integer; Text: AnsiString); override;
    // Copies SourceRect of canvas to (x, y) as an image.
    procedure CopyRect(x, y: Integer; canvas: TFPCustomCanvas; SourceRect: TRect); override;
    // Draws image with its top-left pixel at (x, y).
    procedure Draw(x, y: Integer; image: TFPCustomImage); override;
    // Draws source over w x h pixels, scaled by PostScript when Interpolation is nil.
    procedure StretchDraw(x, y, w, h: Integer; source: TFPCustomImage); override;
    // Fills ARect with an axial shading from AStartColor to AEndColor.
    procedure GradientFill(const ARect: TRect; AStartColor, AEndColor: TFPColor;
                           ADirection: TFPGradientDirection); override;
    // Draws the Bezier curves of Points with curveto, filled when Filled.
    procedure DoPolyBezier(Points: PPoint; NumPts: Integer; Filled: boolean = False;
                           Continuous: boolean = False); override;
    procedure DoEllipse(const Bounds: TRect); override;
    procedure DoEllipseFill(const Bounds: TRect); override;
    procedure DoPie(x,y,awidth,aheight,angle1,angle2 : Integer);
    // Strokes the arc of the ellipse in the bounds, angles in 1/16 degree counter-clockwise from 3 o'clock.
    procedure Arc(ALeft, ATop, ARight, ABottom, Angle16Deg, Angle16DegLength: Integer); overload; override;
    //procedure Pie(x,y,width,height,SX,SY,EX,EY : Integer);
    procedure Writeln(AString: String);
    //procedure Chord(x,y,width,height,angle1,angle2 : Integer);
    //procedure Chord(x,y,width,height,SX,SY,EX,EY : Integer);
    //procedure PolyBezier(Points: PPoint; NumPts: Integer;
    //                     Filled: boolean{$IFDEF VER1_1} = False{$ENDIF};
    //                     Continuous: boolean{$IFDEF VER1_1} = False{$ENDIF});
    //procedure PolyBezier(const Points: array of TPoint;
    //                     Filled: boolean{$IFDEF VER1_1} = False{$ENDIF};
    //                     Continuous: boolean{$IFDEF VER1_1} = False{$ENDIF});
    //procedure PolyBezier(const Points: array of TPoint);
    //procedure Polygon(const Points: array of TPoint;
    //                  Winding: Boolean{$IFDEF VER1_1} = False{$ENDIF};
    //                  StartIndex: Integer{$IFDEF VER1_1} = 0{$ENDIF};
    //                  NumPts: Integer {$IFDEF VER1_1} = -1{$ENDIF});
    //procedure Polygon(Points: PPoint; NumPts: Integer;
    //                  Winding: boolean{$IFDEF VER1_1} = False{$ENDIF});
    //Procedure Polygon(const Points: array of TPoint);
    //Procedure FillRect(const Rect : TRect);
    //procedure FloodFill(X, Y: Integer; FillColor: TFPColor; FillStyle: TFillStyle);
    //Procedure RoundRect(X1, Y1, X2, Y2: Integer; RX,RY : Integer);
    //Procedure RoundRect(const Rect : TRect; RX,RY : Integer);
    Property Stream : TStream read FStream;
  end;

  { Encapsulates ALL the postscript and uses the TPostScriptCanvas object for a single page }
  TPostScript = class(TComponent)
  private
    FDocStarted : Boolean;
    FCreator : String;
    FStream : TStream;
    FCanvas: TPostScriptCanvas;
    FHeight: Integer;
    FLineSpacing: Integer;
    FPageNumber: Integer;
    FTitle: String;
    FWidth: Integer;
    FPatterns: TList;   // array of pointers to pattern objects
    procedure SetHeight(const AValue: Integer);
    procedure SetLineSpacing(const AValue: Integer);
    procedure SetWidth(const AValue: Integer);
    procedure UpdateBoundingBox;
    procedure PatternChanged(Sender: TObject);
    procedure InsertPattern(APattern: TPSPattern); // adds the pattern to the postscript
    Procedure SetStream (Value : TStream);
    Function GetCreator : String;
  Protected
    Procedure WritePS(Const Cmd : String);
    Procedure WritePS(Const Fmt : String; Args : Array of Const);
    Procedure WriteDocumentHeader; virtual;
    Procedure WriteStandardFont; virtual;
    Procedure WritePage; virtual;
    Procedure FreePatterns;
    Procedure CheckStream;
  public
    Constructor Create(AOwner : TComponent);
    destructor Destroy; override;

    procedure AddPattern(APSPattern: TPSPattern);
    function FindPattern(AName: String): TPSPattern;
    function DelPattern(AName: String): Boolean;
    function NewPattern(AName: String): TPSPattern;
    property Canvas: TPostScriptCanvas read FCanvas;
    property Height: Integer read FHeight write SetHeight;
    property Width: Integer read FWidth write SetWidth;
    property PageNumber: Integer read FPageNumber;
    property Title: String read FTitle write FTitle;
    property LineSpacing: Integer read FLineSpacing write SetLineSpacing;
    procedure BeginDoc;
    procedure NewPage;
    procedure EndDoc;
    Property Stream : TStream Read FStream Write SetStream;
    Property Creator : String Read GetCreator Write FCreator;
  end;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses System.Math;
{$ELSE FPC_DOTTEDUNITS}
uses Math;
{$ENDIF FPC_DOTTEDUNITS}

Resourcestring
  SErrNoStreamAssigned = 'Invalid operation: No stream assigned';
  SErrDocumentAlreadyStarted = 'Cannot start document twice.';
  SErrCannotReadPixels = 'The PostScript canvas cannot read pixels without Shadow.';
  SErrNeedsShadow = 'The PostScript canvas needs Shadow for flood fills, pen modes and drawing modes that read the page.';

type
  TPSFontMetrics = record
    Name : String;
    Ascender, Descender, UnderlinePosition, UnderlineThickness, XHeight : SmallInt;
    Widths : array[0..255] of Word;
  end;

{$i pscorefonts.inc}

var
  // Real numbers in PostScript always use a '.' as decimal separator.
  PSFormat : TFormatSettings;

// Returns aColor as PostScript RGB components from 0 to 1.
function PSColor(const aColor : TFPColor) : String;
begin
  Result:=Format('%.4f %.4f %.4f',[aColor.Red/$FFFF,aColor.Green/$FFFF,aColor.Blue/$FFFF],PSFormat);
end;

// Returns aText as a PostScript string literal.
function PSString(const aText : String) : String;
var
  i : Integer;
begin
  Result:='(';
  for i:=1 to Length(aText) do
    case aText[i] of
      '(', ')', '\' : Result:=Result+'\'+aText[i];
      #0..#31, #127..#255 : Result:=Result+'\'+OctStr(Ord(aText[i]),3);
    else
      Result:=Result+aText[i];
    end;
  Result:=Result+')';
end;

// Returns the UTF-8 aText in ISO 8859-1, with '?' for the characters it lacks.
function Latin1Text(const aText : String) : RawByteString;
var
  lText : UnicodeString;
  i, lLen : Integer;
  c : Word;
begin
  lText:=UTF8Decode(aText);
  SetLength(Result,Length(lText));
  lLen:=0;
  for i:=1 to Length(lText) do
    begin
    c:=Ord(lText[i]);
    if (c>=$DC00) and (c<=$DFFF) then
      continue;
    if (c<32) or ((c>=127) and (c<160)) or (c>255) then
      c:=Ord('?');
    Inc(lLen);
    Result[lLen]:=AnsiChar(c);
    end;
  SetLength(Result,lLen);
end;

// Returns whether a pixel of aImage is not fully opaque.
function HasTransparency(aImage : TFPCustomImage) : Boolean;
var
  x, y : Integer;
begin
  Result:=True;
  for y:=0 to aImage.Height-1 do
    for x:=0 to aImage.Width-1 do
      if aImage.Colors[x,y].Alpha<>alphaOpaque then
        exit;
  Result:=False;
end;

const
  PSPenPatterns : array[psDash..psDashDotDot] of LongWord =
    ($EEEEEEEE, $AAAAAAAA, $E4E4E4E4, $EAEAEAEA);

// Returns the setdash arguments of the 32-bit pen pattern aPattern: set bits are drawn, the most
// significant bit first.
function PatternDash(aPattern : LongWord) : String;

  function Bit(i : Integer) : Boolean;
  begin
    Result:=((aPattern shr (31 - (i mod 32))) and 1) <> 0;
  end;

var
  i, lStart, lRun, lPeriod : Integer;
  lOn : Boolean;
begin
  if aPattern = $FFFFFFFF then
    exit('[] 0');
  if aPattern = 0 then
    exit('[0 1] 0');
  // lPeriod: the smallest rotation that leaves the pattern unchanged
  lPeriod:=1;
  while (lPeriod < 32) and (RolDWord(aPattern, lPeriod) <> aPattern) do
    lPeriod:=lPeriod*2;
  lStart:=0;
  while not (Bit(lStart) and not Bit(lStart + 31)) do
    Inc(lStart);
  Result:='[';
  lOn:=True;
  lRun:=0;
  for i:=lStart to lStart + lPeriod - 1 do
    begin
    if Bit(i) <> lOn then
      begin
      Result:=Result+IntToStr(lRun)+' ';
      lOn:=not lOn;
      lRun:=0;
      end;
    Inc(lRun);
    end;
  Result:=Result+IntToStr(lRun)+'] '+IntToStr((lPeriod - lStart mod lPeriod) mod lPeriod);
end;

// Returns the line cap, line join and dash settings of aPen.
function PenStrokeSettings(aPen : TFPCustomPen) : String;
const
  Caps : array[TFPPenEndCap] of Char = ('1', '2', '0');
  Joins : array[TFPPenJoinStyle] of Char = ('1', '2', '0');
begin
  case aPen.Style of
    psDash, psDot, psDashDot, psDashDotDot:
      Result:='0 setlinecap '+PatternDash(PSPenPatterns[aPen.Style])+' setdash';
    psPattern:
      Result:='0 setlinecap '+PatternDash(aPen.Pattern)+' setdash';
  else
    Result:=Caps[aPen.EndCap]+' setlinecap [] 0 setdash';
  end;
  Result:=Result+' '+Joins[aPen.JoinStyle]+' setlinejoin';
end;


{ TPostScriptCanvas ----------------------------------------------------------}

Procedure TPostScriptCanvas.WritePS(const Cmd : String);
var
  ss : shortstring;
begin
  If length(Cmd)>0 then
    FStream.Write(Cmd[1],Length(Cmd));
  ss:=LineEnding;
  FStream.Write(ss[1],Length(ss));
end;

Procedure TPostScriptCanvas.WritePS(Const Fmt : String; Args : Array of Const);

begin
  WritePS(Format(Fmt,Args));
end;

{ Y coords in postscript are backwards... }
function TPostScriptCanvas.TranslateY(Ycoord: Integer): Integer;
begin
  Result:=Height-Ycoord;
end;

{ Adds a fill finishing line to any path we desire to fill }
function TPostScriptCanvas.FillOperator: String;
begin
  if FEvenOdd then
    Result := 'eofill'
  else
    Result := 'fill';
end;

procedure TPostScriptCanvas.AddFill(const aShapeOrigin: TPoint; aEllipse: Boolean);
begin
  case Brush.Style of
    bsSolid:
      WritePs('gsave '+PSColor(Brush.FPColor)+' setrgbcolor '+FillOperator+' grestore');
    bsHorizontal, bsVertical, bsFDiagonal, bsBDiagonal, bsCross, bsDiagCross:
      WriteHatchFill(aShapeOrigin, aEllipse);
    bsPattern:
      WritePatternFill;
    bsImage:
      if Assigned(Brush.Image) then
        if FRelativeBrushImage then
          WriteImageFill(aShapeOrigin)
        else
          WriteImageFill(Point(0, 0));
  end;
end;

{ Clips to the current path after gsave, stores its bounding box in HLlx,
  HLly, HUrx and HUry, and clears the path; the caller ends with grestore. }
procedure WriteClipStart(Canvas: TPostScriptCanvas);
begin
  if Canvas.FEvenOdd then
    Canvas.WritePS('gsave eoclip pathbbox /HUry exch def /HUrx exch def /HLly exch def /HLlx exch def newpath')
  else
    Canvas.WritePS('gsave clip pathbbox /HUry exch def /HUrx exch def /HLly exch def /HLlx exch def newpath');
end;

{ Hatch lines are placed like on the pixel canvases: every HashWidth from the
  origin, diagonals where x - y or x + y (canvas coordinates) is a multiple. }
procedure TPostScriptCanvas.WriteHatchFill(const aShapeOrigin: TPoint; aEllipse: Boolean);
var
  lShape: Boolean;
  lWidth: Integer;
begin
  lWidth := Max(1, FHashWidth);
  case FHatchOrigin of
    hoShape: lShape := True;
    hoCanvas: lShape := False;
  else
    lShape := not aEllipse;
  end;
  WriteClipStart(Self);
  WritePS(PSColor(Brush.FPColor)+' setrgbcolor 1 setlinewidth 0 setlinecap [] 0 setdash');
  if lShape then
    WritePS('/HOX %d def /HOY %d def /HW %d def', [aShapeOrigin.X, TranslateY(aShapeOrigin.Y), lWidth])
  else
    WritePS('/HOX 0 def /HOY %d def /HW %d def', [Height, lWidth]);
  if Brush.Style in [bsHorizontal, bsCross] then
    WritePS('HOY HUry sub HW div ceiling cvi 1 HOY HLly sub HW div floor cvi '+
            '{ HW mul HOY exch sub dup HLlx exch moveto HUrx exch lineto } for');
  if Brush.Style in [bsVertical, bsCross] then
    WritePS('HLlx HOX sub HW div ceiling cvi 1 HUrx HOX sub HW div floor cvi '+
            '{ HW mul HOX add dup HLly moveto HUry lineto } for');
  if Brush.Style in [bsFDiagonal, bsDiagCross] then
    WritePS('HLlx HLly add HOX sub HOY sub HW div ceiling cvi 1 HUrx HUry add HOX sub HOY sub HW div floor cvi '+
            '{ HW mul HOX add HOY add dup HLly sub HLly moveto HUry sub HUry lineto } for');
  if Brush.Style in [bsBDiagonal, bsDiagCross] then
    WritePS('HLlx HUry sub 1 add HOX sub HOY add HW div ceiling cvi 1 HUrx HLly sub 1 add HOX sub HOY add HW div floor cvi '+
            '{ HW mul 1 sub HOX add HOY sub dup HLly add HLly moveto HUry add HUry lineto } for');
  WritePS('stroke grestore');
end;

{ The 32x32 pattern is tiled from the canvas origin, the most significant bit leftmost. }
procedure TPostScriptCanvas.WritePatternFill;
var
  lHex: String;
  i: Integer;
begin
  lHex := '';
  for i := 0 to High(Brush.Pattern) do
    lHex := lHex + IntToHex(Brush.Pattern[i], 8);
  WriteClipStart(Self);
  WritePS(PSColor(Brush.FPColor)+' setrgbcolor /HPat <'+lHex+'> def');
  WritePS('HLlx 32 div floor cvi 1 HUrx 32 div floor cvi { /HTX exch def');
  WritePS('  %d HUry sub 32 div floor cvi 1 %d HLly sub 32 div floor cvi { /HTY exch def', [Height, Height]);
  WritePS('    gsave HTX 32 mul 0.5 sub %d HTY 32 mul sub 31.5 sub translate 32 32 scale', [Height]);
  WritePS('    32 32 true [32 0 0 -32 0 32] {HPat} imagemask grestore } for } for');
  WritePS('grestore');
end;

{ The brush image is tiled from aOrigin. }
procedure TPostScriptCanvas.WriteImageFill(const aOrigin: TPoint);
var
  lImage: TFPCustomImage;
  lHex: String;
  x, y: Integer;
  c: TFPColor;
begin
  lImage := Brush.Image;
  if (lImage.Width = 0) or (lImage.Height = 0) or (lImage.Width * lImage.Height * 3 > 65535) then
    begin
    WritePs('gsave '+PSColor(Brush.FPColor)+' setrgbcolor '+FillOperator+' grestore');
    exit;
    end;
  lHex := '';
  for y := 0 to lImage.Height - 1 do
    for x := 0 to lImage.Width - 1 do
      begin
      c := lImage.Colors[x, y];
      lHex := lHex + IntToHex(c.Red shr 8, 2) + IntToHex(c.Green shr 8, 2) + IntToHex(c.Blue shr 8, 2);
      end;
  WriteClipStart(Self);
  WritePS('/HImg <'+lHex+'> def /HIW %d def /HIH %d def', [lImage.Width, lImage.Height]);
  WritePS('HLlx %d sub HIW div floor cvi 1 HUrx %d sub HIW div floor cvi { /HTX exch def', [aOrigin.X, aOrigin.X]);
  WritePS('  %d HUry sub HIH div floor cvi 1 %d HLly sub HIH div floor cvi { /HTY exch def',
          [Height - aOrigin.Y, Height - aOrigin.Y]);
  WritePS('    gsave HTX HIW mul %d add 0.5 sub %d HTY HIH mul sub HIH sub 0.5 add translate HIW HIH scale',
          [aOrigin.X, Height - aOrigin.Y]);
  WritePS('    HIW HIH 8 [HIW 0 0 HIH neg 0 HIH] {HImg} false 3 colorimage grestore } for } for');
  WritePS('grestore');
end;

procedure TPostScriptCanvas.WritePen;
begin
  if (Pen is TPSPen) and (Pen.Mode = pmCopy) then
    WritePS(TPSPen(Pen).AsString)
  else
    WritePS(PSColor(PenModeColor(Pen.Mode, Pen.FPColor, colBlack))+' setrgbcolor '+IntToStr(Pen.Width)+
            ' setlinewidth '+PenStrokeSettings(Pen));
end;

procedure TPostScriptCanvas.BeginClip;
var
  R: TRect;
begin
  if not Clipping then
    exit;
  R := DeviceClipRect;
  // the clip path lies on the outer edges of the pixels of ClipRect
  WritePS(Format('gsave newpath %.1f %.1f moveto %.1f %.1f lineto %.1f %.1f lineto %.1f %.1f lineto closepath clip newpath',
                 [R.Left - 0.5, Height - R.Top + 0.5, R.Right + 0.5, Height - R.Top + 0.5,
                  R.Right + 0.5, Height - R.Bottom - 0.5, R.Left - 0.5, Height - R.Bottom - 0.5], PSFormat));
end;

procedure TPostScriptCanvas.EndClip;
begin
  if Clipping then
    WritePS('grestore');
end;

procedure TPostScriptCanvas.GradientFill(const ARect: TRect; AStartColor, AEndColor: TFPColor;
  ADirection: TFPGradientDirection);
var
  R: TRect;
  X1, Y1, X2, Y2: Double;
begin
  if not DeviceRect(ARect, R) then
    exit;
  if BlendsColor(AStartColor) or BlendsColor(AEndColor) then
    begin
    if not FShadow then
      raise EPostScriptCanvas.Create(SErrNeedsShadow);
    SyncShadow;
    TakeSnapshot;
    FShadowCanvas.GradientFill(R, AStartColor, AEndColor, ADirection);
    WritePatch;
    exit;
    end;
  if FShadow then
    begin
    SyncShadow;
    FShadowCanvas.GradientFill(R, AStartColor, AEndColor, ADirection);
    end;
  BeginClip;
  // the shading covers the pixels of R; the colours run between the centres of the first and last pixel
  WritePS(Format('gsave newpath %.1f %.1f moveto %.1f %.1f lineto %.1f %.1f lineto %.1f %.1f lineto closepath clip',
                 [R.Left - 0.5, Height - R.Top + 0.5, R.Right + 0.5, Height - R.Top + 0.5,
                  R.Right + 0.5, Height - R.Bottom - 0.5, R.Left - 0.5, Height - R.Bottom - 0.5], PSFormat));
  if ADirection = gdVertical then
    begin
    X1 := R.Left; X2 := R.Left;
    Y1 := Height - R.Top; Y2 := Height - R.Bottom;
    end
  else
    begin
    X1 := R.Left; X2 := R.Right;
    Y1 := Height - R.Top; Y2 := Height - R.Top;
    end;
  WritePS(Format('<< /ShadingType 2 /ColorSpace /DeviceRGB /Coords [%.1f %.1f %.1f %.1f] /Extend [true true]',
                 [X1, Y1, X2, Y2], PSFormat));
  WritePS('   /Function << /FunctionType 2 /Domain [0 1] /C0 ['+PSColor(AStartColor)+'] /C1 ['+PSColor(AEndColor)+'] /N 1 >> >> shfill grestore');
  EndClip;
end;

{ Return to last moveto location }
procedure TPostScriptCanvas.ResetPos;
begin
  WritePS(inttostr(LastX)+' '+inttostr(TranslateY(LastY))+' moveto');
end;

constructor TPostScriptCanvas.Create(AStream : TStream);

begin
  inherited create;
  FStream:=AStream;
  FHashWidth:=15;
  Pen.Mode:=pmCopy;
  Height := 792; // length of page in points at 72 ppi
  { // Choose a standard font in case the user doesn't
  FFontFace := 'AvantGarde-Book';
  SetFontSize(10);
    FLineSpacing := MPostScript.LineSpacing;
  end;
  FPen := TPSPen.Create;
  FPen.Width := 1;
  FPen.FPColor := 0;
  FPen.OnChange := @PenChanged;

  FBrush := TPSPen.Create;
  FBrush.Width := 1;
  FBrush.FPColor := -1;
  // don't notify us that the brush changed...
  }
end;

destructor TPostScriptCanvas.Destroy;
begin
{
  FPostScript.Free;
  FPen.Free;
  FBrush.Free;
}
  FShadowCanvas.Free;
  FShadowImage.Free;
  FSnapshot.Free;
  inherited Destroy;
end;

procedure TPostScriptCanvas.SetWidth (AValue : integer);

begin
  FWidth:=AValue;
  UpdateShadow;
end;

function  TPostScriptCanvas.GetWidth : integer;

begin
  Result:=FWidth;
end;

procedure TPostScriptCanvas.SetHeight (AValue : integer);

begin
  FHeight:=AValue;
  UpdateShadow;
end;

function  TPostScriptCanvas.GetHeight : integer;

begin
  Result:=FHeight;
end;


{ Move draw location }
procedure TPostScriptCanvas.DoMoveTo(X1, Y1: Integer);

var
  Y: Integer;

begin
  Y := TranslateY(Y1);
  WritePS(inttostr(X1)+' '+inttostr(Y)+' moveto');
  LastX := X1;
  LastY := Y1;
end;

{ Draw a line from current location to these coords }
procedure TPostScriptCanvas.PSLineTo(X1, Y1: Integer);
begin
  BeginClip;
  WritePen;
  WritePS('newpath %d %d moveto %d %d lineto stroke',
          [LastX, TranslateY(LastY), X1, TranslateY(Y1)]);
  LastX := X1;
  LastY := Y1;
  EndClip;
  ResetPos;
end;

procedure TPostScriptCanvas.PSLine(X1, Y1, X2, Y2: Integer);
var
  Y12, Y22: Integer;

begin
  Y12 := TranslateY(Y1);
  Y22 := TranslateY(Y2);
  BeginClip;
  WritePen;
  WritePS('newpath '+inttostr(X1)+' '+inttostr(Y12)+' moveto '+
          inttostr(X2)+' '+inttostr(Y22)+' lineto stroke');
  // go back to last moveto position
  EndClip;
  ResetPos;
end;

procedure TPostScriptCanvas.DoLineTo(X1, Y1: Integer);
var
  lPatch: Boolean;
begin
  lPatch := StartShape(True, False);
  if FShadow then
    FShadowCanvas.Line(LastX, LastY, X1, Y1);
  if lPatch or (Pen.Style = psClear) then
    begin
    LastX := X1;
    LastY := Y1;
    end
  else
    PSLineTo(X1, Y1);
  EndShape(lPatch);
end;

procedure TPostScriptCanvas.DoLine(X1, Y1, X2, Y2: Integer);
var
  lPatch: Boolean;
begin
  lPatch := StartShape(True, False);
  if not lPatch and (Pen.Style <> psClear) then
    PSLine(X1, Y1, X2, Y2);
  if FShadow then
    FShadowCanvas.Line(X1, Y1, X2, Y2);
  EndShape(lPatch);
end;

procedure TPostScriptCanvas.DoPolyline(Const Points: Array of TPoint);
var
  lPatch: Boolean;
begin
  lPatch := StartShape(True, False);
  if not lPatch and (Pen.Style <> psClear) then
    PSPolyline(Points);
  if FShadow then
    FShadowCanvas.Polyline(Points);
  EndShape(lPatch);
end;

procedure TPostScriptCanvas.DoPolygon(const Points: array of TPoint);
var
  lPatch: Boolean;
begin
  lPatch := StartShape(True, False);
  if not lPatch and (Pen.Style <> psClear) then
    PSPolygon(Points);
  if FShadow then
    begin
    FShadowCanvas.Brush.Style := bsClear;
    FShadowCanvas.Polygon(Points);
    end;
  EndShape(lPatch);
end;

procedure TPostScriptCanvas.DoPolygonFill(const Points: array of TPoint);
var
  lPatch: Boolean;
begin
  lPatch := StartShape(False, True);
  if not lPatch then
    PSPolygonFill(Points);
  if FShadow then
    begin
    FShadowCanvas.Pen.Style := psClear;
    FShadowCanvas.Polygon(Points);
    end;
  EndShape(lPatch);
end;

procedure TPostScriptCanvas.DoPolyBezier(Points: PPoint; NumPts: Integer; Filled: boolean;
  Continuous: boolean);
var
  lPatch: Boolean;
begin
  lPatch := StartShape(True, Filled);
  if not lPatch then
    PSPolyBezier(Points, NumPts, Filled, Continuous);
  if FShadow then
    FShadowCanvas.PolyBezier(Points, NumPts, Filled, Continuous);
  EndShape(lPatch);
end;

procedure TPostScriptCanvas.SetShadow(aValue: Boolean);
begin
  if FShadow = aValue then
    exit;
  FShadow := aValue;
  UpdateShadow;
end;

procedure TPostScriptCanvas.UpdateShadow;
begin
  if FShadow and (Width > 0) and (Height > 0) then
    begin
    if Assigned(FShadowImage) and (FShadowImage.Width = Width) and (FShadowImage.Height = Height) then
      exit;
    FreeAndNil(FShadowCanvas);
    FreeAndNil(FShadowImage);
    FShadowImage := TFPMemoryImage.Create(Width, Height);
    FShadowCanvas := TFPImageCanvas.Create(FShadowImage);
    FShadowCanvas.RectangleMode := rmInclude;
    ClearShadow;
    end
  else
    begin
    FreeAndNil(FShadowCanvas);
    FreeAndNil(FShadowImage);
    FShadow := FShadow and ((Width <= 0) or (Height <= 0));
    end;
end;

procedure TPostScriptCanvas.ClearShadow;
var
  x, y: Integer;
begin
  if not Assigned(FShadowImage) then
    exit;
  for y := 0 to FShadowImage.Height - 1 do
    for x := 0 to FShadowImage.Width - 1 do
      FShadowImage.Colors[x, y] := colWhite;
end;

procedure TPostScriptCanvas.SyncShadow;
begin
  UpdateShadow;
  with FShadowCanvas do
    begin
    Pen.Style := Self.Pen.Style;
    Pen.Width := Max(1, Self.Pen.Width);
    Pen.Mode := Self.Pen.Mode;
    Pen.FPColor := Self.Pen.FPColor;
    Pen.EndCap := Self.Pen.EndCap;
    Pen.JoinStyle := Self.Pen.JoinStyle;
    Pen.Pattern := Self.Pen.Pattern;
    Brush.Style := Self.Brush.Style;
    Brush.FPColor := Self.Brush.FPColor;
    Brush.Image := Self.Brush.Image;
    Brush.Pattern := Self.Brush.Pattern;
    DrawingMode := Self.DrawingMode;
    OnCombineColors := Self.OnCombineColors;
    EllipseMode := Self.EllipseMode;
    HashWidth := FHashWidth;
    HatchOrigin := FHatchOrigin;
    RelativeBrushImage := FRelativeBrushImage;
    PolygonNonZeroWindingRule := FNonZeroWindingRule;
    Interpolation := Self.Interpolation;
    Clipping := Self.Clipping;
    if Self.Clipping then
      ClipRect := Self.DeviceClipRect;
    end;
end;

function TPostScriptCanvas.BlendsColor(const aColor: TFPColor): Boolean;
begin
  Result := (DrawingMode = dmCustom) or ((DrawingMode = dmAlphaBlend) and (aColor.Alpha <> alphaOpaque));
end;

function TPostScriptCanvas.StartShape(aPen, aFill: Boolean): Boolean;
begin
  aPen := aPen and (Pen.Style <> psClear);
  aFill := aFill and (Brush.Style <> bsClear);
  Result := (aPen and ((Pen.Mode in [pmCopy, pmBlack, pmWhite, pmNop, pmNotCopy]) = False))
            or (aPen and (Pen.Mode = pmCopy) and BlendsColor(Pen.FPColor))
            or (aFill and (BlendsColor(Brush.FPColor) or ((DrawingMode = dmAlphaBlend) and (Brush.Style = bsImage))));
  if Result and not FShadow then
    raise EPostScriptCanvas.Create(SErrNeedsShadow);
  if FShadow then
    SyncShadow;
  if Result then
    TakeSnapshot;
  FNopPen := not Result and aPen and (Pen.Mode = pmNop);
  if FNopPen then
    begin
    FNopPenStyle := Pen.Style;
    Pen.Style := psClear;
    end;
end;

procedure TPostScriptCanvas.EndShape(aPatch: Boolean);
begin
  if FNopPen then
    begin
    Pen.Style := FNopPenStyle;
    FNopPen := False;
    end;
  if aPatch then
    WritePatch;
end;

procedure TPostScriptCanvas.TakeSnapshot;
var
  x, y: Integer;
begin
  if not Assigned(FSnapshot) then
    FSnapshot := TFPMemoryImage.Create(FShadowImage.Width, FShadowImage.Height)
  else if (FSnapshot.Width <> FShadowImage.Width) or (FSnapshot.Height <> FShadowImage.Height) then
    FSnapshot.SetSize(FShadowImage.Width, FShadowImage.Height);
  for y := 0 to FShadowImage.Height - 1 do
    for x := 0 to FShadowImage.Width - 1 do
      FSnapshot.Colors[x, y] := FShadowImage.Colors[x, y];
end;

procedure TPostScriptCanvas.WritePatch;
var
  x, y, lLeft, lTop, lRight, lBottom: Integer;
  lPatch: TFPMemoryImage;
  c: TFPColor;
begin
  lLeft := FShadowImage.Width;
  lTop := FShadowImage.Height;
  lRight := -1;
  lBottom := -1;
  for y := 0 to FShadowImage.Height - 1 do
    for x := 0 to FShadowImage.Width - 1 do
      if FShadowImage.Colors[x, y] <> FSnapshot.Colors[x, y] then
        begin
        lLeft := Min(lLeft, x);
        lRight := Max(lRight, x);
        lTop := Min(lTop, y);
        lBottom := Max(lBottom, y);
        end;
  if lRight < 0 then
    exit;
  lPatch := TFPMemoryImage.Create(lRight - lLeft + 1, lBottom - lTop + 1);
  try
    for y := lTop to lBottom do
      for x := lLeft to lRight do
        begin
        c := FShadowImage.Colors[x, y];
        if c <> FSnapshot.Colors[x, y] then
          c.Alpha := alphaOpaque
        else
          c.Alpha := alphaTransparent;
        lPatch.Colors[x - lLeft, y - lTop] := c;
        end;
    WriteImage(lLeft, lTop, lPatch.Width, lPatch.Height, lPatch, False, True);
  finally
    lPatch.Free;
  end;
end;

{ The outline runs along the pixel edges, clockwise on the canvas around the
  region and counter-clockwise around its holes, so the nonzero rule fills it. }
procedure TPostScriptCanvas.WriteRegionPath(const aMask: array of Boolean; aLeft, aTop, aWidth, aHeight: Integer);
var
  lFrom, lTo, lHead, lNext: array of Integer;
  lUsed: array of Boolean;
  lCount, lStride, x, y, e, lStart, lVertex, lPrevDX, lPrevDY, lDX, lDY: Integer;
  lLine: String;
  lPoints: Integer;

  function Inside(ax, ay: Integer): Boolean;
  begin
    Result := (ax >= 0) and (ay >= 0) and (ax < aWidth) and (ay < aHeight) and aMask[ay * aWidth + ax];
  end;

  procedure AddEdge(x1, y1, x2, y2: Integer);
  begin
    lFrom[lCount] := y1 * lStride + x1;
    lTo[lCount] := y2 * lStride + x2;
    lNext[lCount] := lHead[lFrom[lCount]];
    lHead[lFrom[lCount]] := lCount;
    Inc(lCount);
  end;

  procedure AddPoint(aVertex: Integer; const aOperator: String);
  begin
    lLine := lLine + Format('%.1f %.1f %s ', [aLeft + (aVertex mod lStride) - 0.5,
                                             Height - (aTop + (aVertex div lStride) - 0.5), aOperator], PSFormat);
    Inc(lPoints);
    if lPoints mod 6 = 0 then
      begin
      WritePS(TrimRight(lLine));
      lLine := '';
      end;
  end;

begin
  lStride := aWidth + 1;
  SetLength(lHead, lStride * (aHeight + 1));
  for x := 0 to High(lHead) do
    lHead[x] := -1;
  SetLength(lFrom, 4 * aWidth * aHeight);
  SetLength(lTo, Length(lFrom));
  SetLength(lNext, Length(lFrom));
  lCount := 0;
  for y := 0 to aHeight - 1 do
    for x := 0 to aWidth - 1 do
      if aMask[y * aWidth + x] then
        begin
        if not Inside(x, y - 1) then
          AddEdge(x, y, x + 1, y);
        if not Inside(x + 1, y) then
          AddEdge(x + 1, y, x + 1, y + 1);
        if not Inside(x, y + 1) then
          AddEdge(x + 1, y + 1, x, y + 1);
        if not Inside(x - 1, y) then
          AddEdge(x, y + 1, x, y);
        end;
  SetLength(lUsed, lCount);
  WritePS('newpath');
  lLine := '';
  lPoints := 0;
  for e := 0 to lCount - 1 do
    if not lUsed[e] then
      begin
      lStart := lFrom[e];
      AddPoint(lStart, 'moveto');
      lPrevDX := 0;
      lPrevDY := 0;
      lVertex := e;
      repeat
        lUsed[lVertex] := True;
        lDX := (lTo[lVertex] mod lStride) - (lFrom[lVertex] mod lStride);
        lDY := (lTo[lVertex] div lStride) - (lFrom[lVertex] div lStride);
        // a corner is written where the direction changes
        if ((lDX <> lPrevDX) or (lDY <> lPrevDY)) and (lFrom[lVertex] <> lStart) then
          AddPoint(lFrom[lVertex], 'lineto');
        lPrevDX := lDX;
        lPrevDY := lDY;
        if lTo[lVertex] = lStart then
          break;
        x := lHead[lTo[lVertex]];
        while (x >= 0) and lUsed[x] do
          x := lNext[x];
        lVertex := x;
      until lVertex < 0;
      lLine := lLine + 'closepath ';
      end;
  if lLine <> '' then
    WritePS(TrimRight(lLine));
end;

procedure TPostScriptCanvas.DoFloodFill(x, y: Integer);
var
  lScratch: TFPImageCanvas;
  lSeed, lSentinel: TFPColor;
  lMask: array of Boolean;
  lX, lY, lLeft, lTop, lRight, lBottom: Integer;
  lPatch: Boolean;
begin
  if not FShadow then
    raise EPostScriptCanvas.Create(SErrNeedsShadow);
  if (Brush.Style = bsClear) or (x < 0) or (y < 0) or (x >= Width) or (y >= Height) then
    exit;
  SyncShadow;
  TakeSnapshot;
  lSeed := FSnapshot.Colors[x, y];
  lSentinel := lSeed;
  lSentinel.Red := lSentinel.Red xor $FFFF;
  lScratch := TFPImageCanvas.Create(FSnapshot);
  try
    lScratch.Clipping := Clipping;
    if Clipping then
      lScratch.ClipRect := DeviceClipRect;
    lScratch.Brush.Style := bsSolid;
    lScratch.Brush.FPColor := lSentinel;
    lScratch.FloodFill(x, y);
  finally
    lScratch.Free;
  end;
  lLeft := Width;
  lTop := Height;
  lRight := -1;
  lBottom := -1;
  for lY := 0 to Height - 1 do
    for lX := 0 to Width - 1 do
      if FSnapshot.Colors[lX, lY] <> FShadowImage.Colors[lX, lY] then
        begin
        lLeft := Min(lLeft, lX);
        lRight := Max(lRight, lX);
        lTop := Min(lTop, lY);
        lBottom := Max(lBottom, lY);
        end;
  if lRight < 0 then
    exit;
  lPatch := (DrawingMode = dmCustom) or ((DrawingMode = dmAlphaBlend)
            and ((Brush.Style = bsImage) or (Brush.FPColor.Alpha <> alphaOpaque)));
  if not lPatch then
    begin
    SetLength(lMask, (lRight - lLeft + 1) * (lBottom - lTop + 1));
    for lY := lTop to lBottom do
      for lX := lLeft to lRight do
        lMask[(lY - lTop) * (lRight - lLeft + 1) + lX - lLeft] :=
          FSnapshot.Colors[lX, lY] <> FShadowImage.Colors[lX, lY];
    WriteRegionPath(lMask, lLeft, lTop, lRight - lLeft + 1, lBottom - lTop + 1);
    AddFill(Point(x, y), True);
    WritePS('newpath');
    end
  else
    TakeSnapshot;
  FShadowCanvas.FloodFill(x, y);
  if lPatch then
    WritePatch;
  ResetPos;
end;

{ Draw a rectangle }

procedure TPostScriptCanvas.DoRectangleFill(const Bounds: TRect);
var
  lPatch: Boolean;
begin
  lPatch := StartShape(False, True);
  if not lPatch then
    DrawRectangle(Bounds, True);
  if FShadow then
    FShadowCanvas.FillRect(Bounds);
  EndShape(lPatch);
end;

procedure TPostScriptCanvas.DoRectangle(const Bounds: TRect);
var
  lPatch: Boolean;
begin
  lPatch := StartShape(True, False);
  if not lPatch then
    DrawRectangle(Bounds, False);
  if FShadow then
    begin
    FShadowCanvas.Brush.Style := bsClear;
    FShadowCanvas.Rectangle(Bounds);
    end;
  EndShape(lPatch);
end;

procedure TPostScriptCanvas.DrawRectangle(const Bounds: TRect; DoFill : Boolean);

var
  Inset, X1, Y1, X2, Y2: Double;

begin
  if DoFill and (Brush.Style=bsClear) then
    exit;
  if not DoFill and (Pen.Style=psClear) then
    exit;
  // a thick outline stays inside the bounds, as on the pixel canvases
  Inset := 0;
  if not DoFill then
    Inset := Max(0, Min((Pen.Width - 1) / 2, Min(Abs(Bounds.Right - Bounds.Left), Abs(Bounds.Bottom - Bounds.Top)) / 2));
  X1 := Min(Bounds.Left, Bounds.Right) + Inset;
  X2 := Max(Bounds.Left, Bounds.Right) - Inset;
  Y1 := Height - (Min(Bounds.Top, Bounds.Bottom) + Inset);
  Y2 := Height - (Max(Bounds.Top, Bounds.Bottom) - Inset);
  BeginClip;
  if not DoFill then
    WritePen;
  WritePS('newpath');
  WritePS(Format('%.3f %.3f moveto %.3f %.3f lineto %.3f %.3f lineto %.3f %.3f lineto',
                 [X1, Y1, X2, Y1, X2, Y2, X1, Y2], PSFormat));
  WritePS('closepath');
  If DoFill then
    begin
    AddFill(Point(Min(Bounds.Left, Bounds.Right), Min(Bounds.Top, Bounds.Bottom)));
    WritePS('newpath');
    end
  else
    WritePS('stroke');
  EndClip;
  ResetPos;
end;

{ Draw a series of lines }
procedure TPostScriptCanvas.PSPolyline(const Points: array of TPoint);
var
  i : Longint;
begin
  if Length(Points) = 0 then
    exit;
  BeginClip;
  WritePen;
  WritePS('newpath %d %d moveto', [Points[0].X, TranslateY(Points[0].Y)]);
  For i := 1 to High(Points) do
    WritePS('%d %d lineto', [Points[i].X, TranslateY(Points[i].Y)]);
  WritePS('stroke');
  EndClip;
  ResetPos;
end;

// Returns the top-left corner of the bounding box of Points.
function TopLeftOf(const Points: array of TPoint): TPoint;
var
  i : Integer;
begin
  Result := Points[0];
  for i := 1 to High(Points) do
    begin
    Result.X := Min(Result.X, Points[i].X);
    Result.Y := Min(Result.Y, Points[i].Y);
    end;
end;

{ Writes the path through Points, closed }
procedure WritePolygonPath(Canvas: TPostScriptCanvas; const Points: array of TPoint);
var
  i : Integer;
begin
  Canvas.WritePS('newpath');
  for i := 0 to High(Points) do
    if i = 0 then
      Canvas.WritePS('%d %d moveto', [Points[i].X, Canvas.TranslateY(Points[i].Y)])
    else
      Canvas.WritePS('%d %d lineto', [Points[i].X, Canvas.TranslateY(Points[i].Y)]);
  Canvas.WritePS('closepath');
end;

procedure TPostScriptCanvas.PSPolygon(const Points: array of TPoint);
begin
  if Length(Points) = 0 then
    exit;
  BeginClip;
  WritePen;
  WritePolygonPath(Self, Points);
  WritePS('stroke');
  EndClip;
  ResetPos;
end;

procedure TPostScriptCanvas.PSPolygonFill(const Points: array of TPoint);
begin
  if (Length(Points) = 0) or (Brush.Style = bsClear) then
    exit;
  BeginClip;
  WritePolygonPath(Self, Points);
  FEvenOdd := not FNonZeroWindingRule;
  AddFill(TopLeftOf(Points));
  FEvenOdd := False;
  WritePS('newpath');
  EndClip;
  ResetPos;
end;

procedure TPostScriptCanvas.DoTextOut(X, Y: Integer; Text: AnsiString);
var
  lText: RawByteString;
  lWidth, lScale: Double;
begin
  lText := Latin1Text(Text);
  BeginClip;
  WriteFont;
  WritePS(PSColor(Font.FPColor)+' setrgbcolor');
  WritePS('gsave %d %d translate', [X, TranslateY(Y)]);
  if Font.Orientation <> 0 then
    WritePS(Format('%.1f rotate', [Font.Orientation / 10], PSFormat));
  WritePS('0 0 moveto '+PSString(lText)+' show');
  if Font.Underline or Font.StrikeThrough then
    with PSCoreFonts[FontIndex] do
      begin
      lWidth := TextPoints(lText);
      lScale := FontSize / 1000;
      if Font.Underline then
        WritePS(Format('0 %.2f %.2f %.2f rectfill',
                [(UnderlinePosition - UnderlineThickness / 2) * lScale, lWidth, UnderlineThickness * lScale], PSFormat));
      if Font.StrikeThrough then
        WritePS(Format('0 %.2f %.2f %.2f rectfill',
                [(XHeight - UnderlineThickness) / 2 * lScale, lWidth, UnderlineThickness * lScale], PSFormat));
      end;
  WritePS('grestore');
  EndClip;
  ResetPos;
end;

procedure TPostScriptCanvas.PSPolyBezier(Points: PPoint; NumPts: Integer; Filled, Continuous: Boolean);
var
  i, Segments, Idx : Integer;
  lTopLeft : TPoint;
begin
  if Continuous then
    Segments := (NumPts - 1) div 3
  else
    Segments := NumPts div 4;
  if Segments < 1 then
    exit;
  BeginClip;
  WritePS('newpath');
  Idx := 0;
  for i := 0 to Segments - 1 do
    begin
    if (i = 0) or not Continuous then
      begin
      if Filled and (i > 0) then
        WritePS('closepath');
      WritePS('%d %d moveto', [Points[Idx].X, TranslateY(Points[Idx].Y)]);
      Inc(Idx);
      end;
    WritePS('%d %d %d %d %d %d curveto',
            [Points[Idx].X, TranslateY(Points[Idx].Y), Points[Idx+1].X, TranslateY(Points[Idx+1].Y),
             Points[Idx+2].X, TranslateY(Points[Idx+2].Y)]);
    Inc(Idx, 3);
    end;
  if Filled then
    begin
    WritePS('closepath');
    if Brush.Style <> bsClear then
      begin
      lTopLeft := Points[0];
      for i := 1 to NumPts - 1 do
        begin
        lTopLeft.X := Min(lTopLeft.X, Points[i].X);
        lTopLeft.Y := Min(lTopLeft.Y, Points[i].Y);
        end;
      FEvenOdd := not FNonZeroWindingRule;
      AddFill(lTopLeft);
      FEvenOdd := False;
      end;
    end;
  if Pen.Style <> psClear then
    begin
    WritePen;
    WritePS('stroke');
    end
  else
    WritePS('newpath');
  EndClip;
  ResetPos;
end;

{ This was a pain to figure out... }

procedure TPostScriptCanvas.DoEllipse(Const Bounds : TRect);
var
  lPatch: Boolean;
begin
  lPatch := StartShape(True, False);
  if not lPatch then
    DrawEllipse(Bounds, False);
  if FShadow then
    begin
    FShadowCanvas.Brush.Style := bsClear;
    FShadowCanvas.Ellipse(Bounds);
    end;
  EndShape(lPatch);
end;

procedure TPostScriptCanvas.DoEllipseFill(Const Bounds : TRect);
var
  lPatch: Boolean;
begin
  lPatch := StartShape(False, True);
  if not lPatch then
    DrawEllipse(Bounds, True);
  if FShadow then
    begin
    FShadowCanvas.Pen.Style := psClear;
    FShadowCanvas.Ellipse(Bounds);
    end;
  EndShape(lPatch);
end;

procedure TPostScriptCanvas.DrawEllipse(Const Bounds : TRect; DoFill : Boolean);

var
  rx, ry, cx, cy: Double;

begin
  if DoFill and (Brush.Style=bsClear) then
    exit;
  if not DoFill and (Pen.Style=psClear) then
    exit;
  With Bounds do
    begin
    rx := (Right-Left) / 2;
    ry := (Bottom-Top) / 2;
    cx := (Right+Left) / 2;
    cy := (Top+Bottom) / 2;
    end;
  // emInside: the outer edge of the stroke lies on the outer edge of the bounds
  if not DoFill and (EllipseMode = emInside) and (Pen.Width > 1) then
    begin
    rx := rx - (Pen.Width - 1) / 2;
    ry := ry - (Pen.Width - 1) / 2;
    end;
  if (rx <= 0) or (ry <= 0) then
    exit;
  BeginClip;
  if not DoFill then
    WritePen;
  // the unit circle scaled to the ellipse; the matrix is restored before painting
  WritePS(Format('newpath matrix currentmatrix %.3f %.3f translate %.3f %.3f scale 0 0 1 0 360 arc setmatrix closepath',
                 [cx, Height-cy, rx, ry], PSFormat));
  if DoFill then
    begin
    AddFill(Point(Min(Bounds.Left, Bounds.Right), Min(Bounds.Top, Bounds.Bottom)), True);
    WritePS('newpath');
    end
  else
    WritePS('stroke');
  EndClip;
  ResetPos;
end;

{ The rays are at geometric angles; the scaled unit circle takes the matching parametric angles. }
procedure TPostScriptCanvas.WriteArc(acx, acy, arx, ary: Double; aStart16, aLength16: Integer);

  function Parametric(aDegrees: Double): Double;
  begin
    Result := RadToDeg(ArcTan2(arx * Sin(DegToRad(aDegrees)), ary * Cos(DegToRad(aDegrees))));
  end;

const
  cOperator: array[Boolean] of String = ('arc', 'arcn');
var
  t0, dt: Double;
begin
  aLength16 := EnsureRange(aLength16, -360*16, 360*16);
  t0 := Parametric(aStart16 / 16);
  if (aLength16 = 0) or (Abs(aLength16) = 360*16) then
    dt := aLength16 / 16
  else
    begin
    dt := Parametric((aStart16 + aLength16) / 16) - t0;
    if aLength16 > 0 then
      while dt <= 0 do
        dt := dt + 360
    else
      while dt >= 0 do
        dt := dt - 360;
    end;
  WritePS(Format('matrix currentmatrix %.3f %.3f translate %.3f %.3f scale 0 0 1 %.3f %.3f %s setmatrix',
                 [acx, Height - acy, arx, ary, t0, t0 + dt, cOperator[aLength16 < 0]], PSFormat));
end;

procedure TPostScriptCanvas.WritePie(acx, acy, arx, ary: Double; aStart16, aLength16: Integer; const aOrigin: TPoint);
begin
  if (arx <= 0) or (ary <= 0) then
    exit;
  BeginClip;
  WritePS(Format('newpath %.1f %.1f moveto', [acx, Height - acy], PSFormat));
  WriteArc(acx, acy, arx, ary, aStart16, aLength16);
  WritePS('closepath');
  if Brush.Style<>bsClear then
    AddFill(aOrigin);
  if Pen.Style<>psClear then
    begin
    WritePen;
    WritePS('stroke');
    end
  else
    WritePS('newpath');
  EndClip;
  ResetPos;
end;

procedure TPostScriptCanvas.DoRadialPie(x1, y1, x2, y2, StartAngle16Deg, Angle16DegLength: Integer);
var
  lPatch: Boolean;
begin
  lPatch := StartShape(True, True);
  if not lPatch then
    WritePie((x1 + x2) / 2, (y1 + y2) / 2, Abs(x2 - x1) / 2, Abs(y2 - y1) / 2,
             StartAngle16Deg, Angle16DegLength, Point(Min(x1, x2), Min(y1, y2)));
  if FShadow then
    FShadowCanvas.RadialPie(x1, y1, x2, y2, StartAngle16Deg, Angle16DegLength);
  EndShape(lPatch);
end;

procedure TPostScriptCanvas.Arc(ALeft, ATop, ARight, ABottom, Angle16Deg, Angle16DegLength: Integer);
var
  R: TRect;
  lPatch: Boolean;
begin
  if (Pen.Style = psClear) or not DeviceRect(Rect(ALeft, ATop, ARight, ABottom), R) then
    exit;
  if (R.Right = R.Left) or (R.Bottom = R.Top) then
    begin
    inherited Arc(ALeft, ATop, ARight, ABottom, Angle16Deg, Angle16DegLength);
    exit;
    end;
  lPatch := StartShape(True, False);
  if not lPatch and (Pen.Style <> psClear) then
    PSArc(R, Angle16Deg, Angle16DegLength);
  if FShadow then
    FShadowCanvas.Arc(R.Left, R.Top, R.Right, R.Bottom, Angle16Deg, Angle16DegLength);
  EndShape(lPatch);
end;

procedure TPostScriptCanvas.PSArc(const R: TRect; Angle16Deg, Angle16DegLength: Integer);
begin
  BeginClip;
  WritePen;
  WritePS('newpath');
  WriteArc((R.Left + R.Right) / 2, (R.Top + R.Bottom) / 2, (R.Right - R.Left) / 2, (R.Bottom - R.Top) / 2,
           Angle16Deg, Angle16DegLength);
  WritePS('stroke');
  EndClip;
  ResetPos;
end;

{ The pie from the centre (x, y) with radii AWidth and AHeight, counter-clockwise from angle1 to angle2 in degrees. }
procedure TPostScriptCanvas.DoPie(x, y, AWidth, AHeight, angle1, angle2: Integer);
var
  lLength: Integer;
  lPatch: Boolean;
begin
  lLength := angle2 - angle1;
  while lLength < 0 do
    Inc(lLength, 360);
  lPatch := StartShape(True, True);
  if not lPatch then
    WritePie(x, y, Abs(AWidth), Abs(AHeight), angle1 * 16, lLength * 16, Point(x - Abs(AWidth), y - Abs(AHeight)));
  if FShadow then
    FShadowCanvas.RadialPie(x - Abs(AWidth), y - Abs(AHeight), x + Abs(AWidth), y + Abs(AHeight),
                            angle1 * 16, lLength * 16);
  EndShape(lPatch);
end;

{ Writes text with a carriage return }
procedure TPostScriptCanvas.Writeln(AString: String);
begin
  TextOut(LastX, LastY, AString);
  LastY := LastY+Font.Size+FLineSpacing;
  MoveTo(LastX, LastY);
end;


function TPostScriptCanvas.FontIndex: Integer;
var
  lName: String;
  i: Integer;
begin
  for i := 0 to High(PSCoreFonts) do
    if SameText(Font.Name, PSCoreFonts[i].Name) then
      exit(i);
  lName := LowerCase(Font.Name);
  if (Pos('courier', lName) > 0) or (Pos('mono', lName) > 0) then
    Result := 8
  else if (Pos('times', lName) > 0) or (Pos('roman', lName) > 0)
          or ((Pos('serif', lName) > 0) and (Pos('sans', lName) = 0)) then
    Result := 4
  else
    Result := 0;
  if Font.Bold then
    Inc(Result);
  if Font.Italic then
    Inc(Result, 2);
end;

function TPostScriptCanvas.FontSize: Integer;
begin
  Result := Font.Size;
  if Result <= 0 then
    Result := 10;
end;

procedure TPostScriptCanvas.WriteFont;
var
  lName: String;
begin
  lName := PSCoreFonts[FontIndex].Name;
  WritePS('FontDirectory /FPC-%s known not { /FPC-%s /%s findfont dup length dict begin', [lName, lName, lName]);
  WritePS('  { 1 index /FID ne { def } { pop pop } ifelse } forall');
  WritePS('  /Encoding ISOLatin1Encoding 256 array copy dup 39 /quotesingle put dup 45 /hyphen put dup 96 /grave put def');
  WritePS('  currentdict end definefont pop } if');
  WritePS('/FPC-%s findfont %d scalefont setfont', [lName, FontSize]);
end;

function TPostScriptCanvas.TextPoints(const aText: RawByteString): Double;
var
  i, lIndex: Integer;
begin
  lIndex := FontIndex;
  Result := 0;
  for i := 1 to Length(aText) do
    Result := Result + PSCoreFonts[lIndex].Widths[Ord(aText[i])];
  Result := Result * FontSize / 1000;
end;

procedure TPostScriptCanvas.DoGetTextSize(text: AnsiString; var w, h: Integer);
begin
  w := DoGetTextWidth(text);
  h := DoGetTextHeight(text);
end;

function TPostScriptCanvas.DoGetTextHeight(text: AnsiString): Integer;
begin
  with PSCoreFonts[FontIndex] do
    Result := Round((Ascender - Descender) * FontSize / 1000);
end;

function TPostScriptCanvas.DoGetTextWidth(text: AnsiString): Integer;
begin
  Result := Round(TextPoints(Latin1Text(text)));
end;

procedure TPostScriptCanvas.SetColor(x, y: Integer; const Value: TFPColor);
begin
  WritePS(PSColor(Value)+Format(' setrgbcolor %.1f %.1f 1 1 rectfill', [x - 0.5, Height - y - 0.5], PSFormat));
  if FShadow then
    FShadowImage.Colors[x, y] := Value;
end;

function TPostScriptCanvas.GetColor(x, y: Integer): TFPColor;
begin
  Result := colTransparent;
  if not FShadow then
    raise EPostScriptCanvas.Create(SErrCannotReadPixels);
  if (x >= 0) and (y >= 0) and (x < FShadowImage.Width) and (y < FShadowImage.Height) then
    Result := FShadowImage.Colors[x, y];
end;

procedure TPostScriptCanvas.WriteImage(aX, aY, aWidth, aHeight: Integer; aImage: TFPCustomImage;
  aInterpolate, aBlend: Boolean);
const
  cBool: array[Boolean] of String = ('false', 'true');
  cPixelsPerLine = 40;
var
  lDict, lLine, lKeyHex: String;
  lMasked: Boolean;
  lUsed: array of Byte;
  x, y, lRGB, lKey: Integer;
  c: TFPColor;
begin
  if (aImage.Width = 0) or (aImage.Height = 0) or (aWidth <= 0) or (aHeight <= 0) then
    exit;
  lMasked := aBlend and HasTransparency(aImage);
  lKey := 0;
  if lMasked then
    begin
    // the key colour is the first 24-bit colour no painted pixel has
    SetLength(lUsed, 1 shl 21);
    FillChar(lUsed[0], Length(lUsed), 0);
    for y := 0 to aImage.Height - 1 do
      for x := 0 to aImage.Width - 1 do
        begin
        c := aImage.Colors[x, y];
        if c.Alpha >= $8000 then
          begin
          lRGB := (c.Red shr 8) shl 16 or (c.Green shr 8) shl 8 or (c.Blue shr 8);
          lUsed[lRGB shr 3] := lUsed[lRGB shr 3] or (1 shl (lRGB and 7));
          end;
        end;
    while (lUsed[lKey shr 3] and (1 shl (lKey and 7))) <> 0 do
      Inc(lKey);
    end;
  lKeyHex := IntToHex(lKey, 6);
  BeginClip;
  WritePS(Format('gsave %.1f %.1f translate %d %d scale /DeviceRGB setcolorspace',
                 [aX - 0.5, Height - aY - aHeight + 0.5, aWidth, aHeight], PSFormat));
  lDict := Format('/Width %d /Height %d /BitsPerComponent 8 /ImageMatrix [%d 0 0 %d 0 %d]',
                  [aImage.Width, aImage.Height, aImage.Width, -aImage.Height, aImage.Height]);
  if lMasked then
    WritePS('<< /ImageType 4 '+lDict+' /Decode [0 1 0 1 0 1] /Interpolate '+cBool[aInterpolate]+
            Format(' /MaskColor [%d %d %d]', [lKey shr 16, (lKey shr 8) and $FF, lKey and $FF]))
  else
    WritePS('<< /ImageType 1 '+lDict+' /Decode [0 1 0 1 0 1] /Interpolate '+cBool[aInterpolate]);
  WritePS('   /DataSource currentfile /ASCIIHexDecode filter >> image');
  for y := 0 to aImage.Height - 1 do
    begin
    lLine := '';
    for x := 0 to aImage.Width - 1 do
      begin
      c := aImage.Colors[x, y];
      if lMasked and (c.Alpha < $8000) then
        lLine := lLine + lKeyHex
      else
        lLine := lLine + IntToHex(c.Red shr 8, 2) + IntToHex(c.Green shr 8, 2) + IntToHex(c.Blue shr 8, 2);
      if ((x + 1) mod cPixelsPerLine = 0) and (x < aImage.Width - 1) then
        begin
        WritePS(lLine);
        lLine := '';
        end;
      end;
    WritePS(lLine);
    end;
  WritePS('> grestore');
  EndClip;
end;

procedure TPostScriptCanvas.DoDraw(x, y: Integer; const image: TFPCustomImage);
var
  lPatch: Boolean;
begin
  lPatch := (DrawingMode = dmCustom) or (FShadow and (DrawingMode = dmAlphaBlend) and HasTransparency(image));
  if lPatch and not FShadow then
    raise EPostScriptCanvas.Create(SErrNeedsShadow);
  if FShadow then
    SyncShadow;
  if lPatch then
    TakeSnapshot
  else
    WriteImage(x, y, image.Width, image.Height, image, False, DrawingMode = dmAlphaBlend);
  if FShadow then
    FShadowCanvas.Draw(x, y, image);
  if lPatch then
    WritePatch;
end;

procedure TPostScriptCanvas.DoCopyRect(x, y: Integer; canvas: TFPCustomCanvas; const SourceRect: TRect);
var
  lImage: TFPMemoryImage;
  lX, lY: Integer;
begin
  with SourceRect do
    begin
    if (Right < Left) or (Bottom < Top) then
      exit;
    lImage := TFPMemoryImage.Create(Right - Left + 1, Bottom - Top + 1);
    end;
  try
    for lY := 0 to lImage.Height - 1 do
      for lX := 0 to lImage.Width - 1 do
        lImage.Colors[lX, lY] := canvas.Colors[SourceRect.Left + lX, SourceRect.Top + lY];
    WriteImage(x, y, lImage.Width, lImage.Height, lImage, False, False);
    if FShadow then
      FShadowCanvas.Draw(x, y, lImage);
  finally
    lImage.Free;
  end;
end;

procedure TPostScriptCanvas.CopyRect(x, y: Integer; canvas: TFPCustomCanvas; SourceRect: TRect);
var
  P: TPoint;
begin
  if HasTransform then
    begin
    P := TransformPoint(x, y);
    x := P.X;
    y := P.Y;
    end;
  if UserRect(SourceRect, SourceRect) then
    begin
    if FShadow then
      begin
      SyncShadow;
      FShadowCanvas.DrawingMode := dmOpaque;
      end;
    DoCopyRect(x, y, canvas, SourceRect);
    end;
end;

procedure TPostScriptCanvas.Draw(x, y: Integer; image: TFPCustomImage);
var
  P: TPoint;
begin
  if HasTransform then
    begin
    P := TransformPoint(x, y);
    x := P.X;
    y := P.Y;
    end;
  DoDraw(x, y, image);
end;

procedure TPostScriptCanvas.StretchDraw(x, y, w, h: Integer; source: TFPCustomImage);
var
  lPatch: Boolean;
  P: TPoint;
  lImage: TFPMemoryImage;
  lCanvas: TFPImageCanvas;
begin
  if (w <= 0) or (h <= 0) then
    exit;
  if HasTransform then
    begin
    P := TransformPoint(x, y);
    x := P.X;
    y := P.Y;
    end;
  lPatch := (DrawingMode = dmCustom) or (FShadow and (DrawingMode = dmAlphaBlend) and HasTransparency(source));
  if lPatch and not FShadow then
    raise EPostScriptCanvas.Create(SErrNeedsShadow);
  if FShadow then
    begin
    SyncShadow;
    if lPatch then
      TakeSnapshot;
    FShadowCanvas.StretchDraw(x, y, w, h, source);
    FShadowCanvas.Interpolation := nil;
    if lPatch then
      begin
      WritePatch;
      exit;
      end;
    end;
  if Interpolation = nil then
    begin
    WriteImage(x, y, w, h, source, True, DrawingMode = dmAlphaBlend);
    exit;
    end;
  lImage := TFPMemoryImage.Create(w, h);
  try
    lCanvas := TFPImageCanvas.Create(lImage);
    try
      lCanvas.Interpolation := Interpolation;
      lCanvas.StretchDraw(0, 0, w, h, source);
    finally
      lCanvas.Interpolation := nil;
      lCanvas.Free;
    end;
    WriteImage(x, y, w, h, lImage, False, DrawingMode = dmAlphaBlend);
  finally
    lImage.Free;
  end;
end;

function TPostScriptCanvas.DoCreateDefaultFont : TFPCustomFont;

begin
  Result:=TPSFont.Create;
end;


function TPostScriptCanvas.DoCreateDefaultPen : TFPCustomPen;

begin
  Result:=TPSPen.Create;
end;

function TPostScriptCanvas.DoCreateDefaultBrush : TFPCustomBrush;

begin
  Result:=TPSBrush.Create;
end;



{ TPostScript -------------------------------------------------------------- }

procedure TPostScript.SetHeight(const AValue: Integer);
begin
  if FHeight=AValue then exit;
  FHeight:=AValue;
  UpdateBoundingBox;
  // filter down to the canvas height property
  if assigned(FCanvas) then
    FCanvas.Height := FHeight;
end;

procedure TPostScript.SetLineSpacing(const AValue: Integer);
begin
  if FLineSpacing=AValue then exit;
  FLineSpacing:=AValue;
  // filter down to the canvas
  if assigned(FCanvas) then FCanvas.LineSpacing := AValue;
end;

procedure TPostScript.SetWidth(const AValue: Integer);
begin
  if FWidth=AValue then exit;
    FWidth:=AValue;
  UpdateBoundingBox;
end;

{ Passes the size on to the canvas; a started document keeps the %%BoundingBox it has written }
procedure TPostScript.UpdateBoundingBox;
begin
  if Assigned(FCanvas) then
    begin
    FCanvas.Width:=FWidth;
    FCanvas.Height:=FHeight;
    end;
end;

{ Pattern changed so update the postscript code }
procedure TPostScript.PatternChanged(Sender: TObject);
begin
     // called anytime a pattern changes.  Update the postscript code.
     // look for and delete the current postscript code for this pattern
     // then paste the pattern back into the code before the first page
     InsertPattern(Sender As TPSPattern);
end;

{ Places a pattern definition into the bottom of the header in postscript }
procedure TPostScript.InsertPattern(APattern: TPSPattern);
var
   I, J: Integer;
   MyStrings: TStringList;
begin
{     I := 0;
     if FDocument.Count < 1 then begin
        // added pattern when no postscript exists - this shouldn't happen
        raise exception.create('Pattern inserted with no postscript existing');
        exit;
     end;

     for I := 0 to FDocument.count - 1 do begin
         if (FDocument[I] = '%%Page: 1 1') then begin
            // found it!
            // insert into just before that
            MyStrings := APattern.GetPS;
            for J := 0 to MyStrings.Count - 1 do begin
                FDocument.Insert(I-1+J, MyStrings[j]);
            end;
            exit;
         end;
     end;
}
end;

constructor TPostScript.Create(AOwner : TComponent);
begin
  inherited create(AOwner);
  // Set some defaults
  FHeight := 792; // 11 inches at 72 dpi
  FWidth := 612; // 8 1/2 inches at 72 dpi
end;

Procedure TPostScript.WritePS(const Cmd : String);
var
  ss : shortstring;
begin
  If length(Cmd)>0 then
    FStream.Write(Cmd[1],Length(Cmd));
  ss:=LineEnding;
  FStream.Write(ss[1],Length(ss));
end;

Procedure TPostScript.WritePS(Const Fmt : String; Args : Array of Const);

begin
  WritePS(Format(Fmt,Args));
end;

Procedure TPostScript.WriteDocumentHeader;

begin
  WritePS('%!PS-Adobe-3.0');
  WritePS('%%BoundingBox: 0 0 '+IntToStr(FWidth)+' '+IntToStr(FHeight));
  WritePS('%%Creator: '+Creator);
  WritePS('%%Title: '+FTitle);
  WritePS('%%Pages: (atend)');
  WritePS('%%PageOrder: Ascend');
  WriteStandardFont;
end;

Procedure TPostScript.WriteStandardFont;

begin
  // Choose a standard font in case the user doesn't
  WritePS('/AvantGarde-Book findfont');
  WritePS('10 scalefont');
  WritePS('setfont');
end;

Procedure TPostScript.FreePatterns;

Var
  i : Integer;

begin
  If Assigned(FPatterns) then
    begin
    For I:=0 to FPatterns.Count-1 do
      TObject(FPatterns[i]).Free;
    FreeAndNil(FPatterns);
    end;
end;

destructor TPostScript.Destroy;

begin
  Stream:=Nil;
  FreePatterns;
  inherited Destroy;
end;

{ add a pattern to the array }
procedure TPostScript.AddPattern(APSPattern: TPSPattern);
begin
  If Not Assigned(FPatterns) then
    FPatterns:=Tlist.Create;
  FPatterns.Add(APSPattern);
end;

{ Find a pattern object by it's name }

function TPostScript.FindPattern(AName: String): TPSPattern;

var
   I: Integer;

begin
  Result := nil;
  If Assigned(FPatterns) then
    begin
    I:=Fpatterns.Count-1;
    While (Result=Nil) and (I>=0) do
      if TPSPattern(FPatterns[I]).Name = AName then
        result := TPSPattern(FPatterns[i])
      else
        Dec(i)
   end;
end;

function TPostScript.DelPattern(AName: String): Boolean;
begin
  // can't do that yet...
  Result:=false;
end;


{ Create a new pattern and inserts it into the array for safe keeping }
function TPostScript.NewPattern(AName: String): TPSPattern;
var
   MyPattern: TPSPattern;
begin
  MyPattern := TPSPattern.Create;
  AddPattern(MyPattern);
  MyPattern.Name := AName;
  MyPattern.OnChange := @PatternChanged;
  MyPattern.OldName := '';
  // add this to the postscript now...
  InsertPattern(MyPattern);
  result := MyPattern;
end;

{ Start a new document }
procedure TPostScript.BeginDoc;

var
   I: Integer;

begin
  CheckStream;
  If FDocStarted then
    Raise Exception.Create(SErrDocumentAlreadyStarted);
  FCanvas:=TPostScriptCanvas.Create(FStream);
  FCanvas.Height:=Self.Height;
  FCanvas.Width:=Self.width;
  FDocStarted:=True;
  FreePatterns;
  WriteDocumentHeader;
  // start our first page
  FPageNumber := 1;
  WritePage;
  UpdateBoundingBox;
end;

Procedure TPostScript.WritePage;

begin
  WritePS('%%Page: '+inttostr(FPageNumber)+' '+inttostr(FPageNumber));
  WritePS('newpath');
end;

{ Copy current page into the postscript and start a new one }
procedure TPostScript.NewPage;
begin
  // dump the current page into our postscript first
  // put end page definition...
  WritePS('stroke');
  WritePS('showpage');
  if Assigned(FCanvas) then
    FCanvas.ClearShadow;
  FPageNumber := FPageNumber+1;
  WritePage;
end;

{ Finish off the document }
procedure TPostScript.EndDoc;
begin
  // Start printing the document after closing out the pages
  WritePS('stroke');
  WritePS('showpage');
  WritePS('%%Pages: '+inttostr(FPageNumber));
  // okay, the postscript is all ready, so dump it to the text file
  // or to the printer
  FDocStarted:=False;
  FreeAndNil(FCanvas);
end;

Function TPostScript.GetCreator : String;

begin
  If (FCreator='') then
    Result:=ClassName
  else
    Result:=FCreator;
end;


Procedure TPostScript.SetStream (Value : TStream);

begin
  if (FStream<>Value) then
    begin
    If (FStream<>Nil) and FDocStarted then
      EndDoc;
    FStream:=Value;
    FDocStarted:=False;
    end;
end;

Procedure TPostScript.CheckStream;

begin
  If Not Assigned(FStream) then
    Raise Exception.Create(SErrNoStreamAssigned);
end;

{ TPSPen }

procedure TPSPen.SetPattern(const AValue: TPSPattern);
begin
  if FPattern<>AValue then
    begin
    FPattern:=AValue;
    // NotifyCanvas;
    end;
end;


destructor TPSPen.Destroy;
begin
  // Do NOT free the pattern object from here...
  inherited Destroy;
end;


{ Return the pen definition as a postscript string }
function TPSPen.AsString: String;

begin
  Result:='';
  if FPattern <> nil then
    begin
    if FPattern.PaintType = ptColored then
      Result:='/Pattern setcolorspace '+FPattern.Name+' setcolor '
    else
      begin
      Result:='[/Pattern /DeviceRGB] setcolorspace '+PSColor(FPColor)+' '+FPattern.Name+' setcolor ';
      end;
    end
  else // no pattern do this:
    Result:=PSColor(FPColor)+' setrgbcolor ';
  Result := Result + IntToStr(Width)+' setlinewidth '+PenStrokeSettings(Self)+' ';
end;

{ TPSPattern }

{ Returns the pattern definition as postscript }
function TPSPattern.GetpostScript: TStringList;

var
   I: Integer;
   S : String;

begin
  // If nothing in the canvas, error
  if FStream.Size=0 then
    raise exception.create('Empty pattern');
  FPostScript.Clear;
  With FPostScript do
    begin
    add('%% PATTERN '+FName);
    add('/'+FName+'proto 12 dict def '+FName+'proto begin');
    add('/PatternType 1 def');
    add(Format('/PaintType %d def',[ord(FPaintType)+1]));
    add(Format('/TilingType %d def',[ord(FTilingType)+1]));
    add('/BBox ['+inttostr(FBBox.Left)+' '+inttostr(FBBox.Top)+' '+inttostr(FBBox.Right)+' '+inttostr(FBBox.Bottom)+'] def');
    add('/XStep '+format('%f',[FXStep],PSFormat)+' def');
    add('/YStep '+format('%f',[FYstep],PSFormat)+' def');
    add('/PaintProc { begin');
    // insert the canvas
    SetLength(S,FStream.Size);
    FStream.Seek(0,soFromBeginning);
    FStream.Read(S[1],FStream.Size);
    Add(S);
    // add support for custom matrix later
    add('end } def end '+FName+'proto [1 0 0 1 0 0] makepattern /'+FName+' exch def');
    add('%% END PATTERN '+FName);
    end;
  Result := FPostScript;
end;

procedure TPSPattern.SetBBox(const AValue: TRect);
begin
  if FBBox<>AValue then
    begin
    FBBox:=AValue;
    FPatternCanvas.Width := FBBox.Right - FBBox.Left;
    FPatternCanvas.Height := FBBox.Bottom - FBBox.Top;
    Changed;
    end;
end;

procedure TPSPattern.SetName(const AValue: String);
begin
  FOldName := FName;
  if (FName<>AValue) then
    begin
    FName:=AValue;
    // NotifyCanvas;
    end;
end;

procedure TPSPattern.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TPSPattern.SetPaintType(const AValue: TPSPaintType);
begin
  if FPaintType=AValue then exit;
  FPaintType:=AValue;
  changed;
end;

procedure TPSPattern.SetTilingType(const AValue: TPSTileType);
begin
  if FTilingType=AValue then exit;
  FTilingType:=AValue;
  changed;
end;

procedure TPSPattern.SetXStep(const AValue: Real);
begin
  if FXStep=AValue then exit;
  FXStep:=AValue;
  changed;
end;

procedure TPSPattern.SetYStep(const AValue: Real);
begin
  if FYStep=AValue then exit;
  FYStep:=AValue;
  changed;
end;

constructor TPSPattern.Create;
begin
  FPostScript := TStringList.Create;
  FPaintType := ptColored;
  FTilingType := ttConstant;
  FStream:=TmemoryStream.Create;
  FPatternCanvas := TPostScriptCanvas.Create(FStream);
  FName := 'Pattern1';
end;

destructor TPSPattern.Destroy;
begin
  FPostScript.Free;
  FPatternCanvas.Free;
  FStream.Free;
  inherited Destroy;
end;

{ ---------------------------------------------------------------------
    TPSBrush
  ---------------------------------------------------------------------}


Function TPSBrush.GetAsString : String;

begin
  Result:=PSColor(FPColor)+' setrgbcolor ';
end;


initialization
  PSFormat:=DefaultFormatSettings;
  PSFormat.DecimalSeparator:='.';
  PSFormat.ThousandSeparator:=#0;
end.
