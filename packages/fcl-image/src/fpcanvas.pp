{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    Basic canvas definitions.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}{$h+}
{$IFNDEF FPC_DOTTEDUNITS}
unit FPCanvas;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses System.Math, System.SysUtils, System.Classes, FpImage, System.Types;
{$ELSE FPC_DOTTEDUNITS}
uses Math, sysutils, classes, FpImage, Types;
{$ENDIF FPC_DOTTEDUNITS}

const
  PatternBitCount = sizeof(longword) * 8;

type

  PPoint = ^TPoint;
  TFPCanvasException = class (Exception);
  TFPPenException = class (TFPCanvasException);
  TFPBrushException = class (TFPCanvasException);
  TFPFontException = class (TFPCanvasException);

  TFPCanvasPointArray = array of TPoint;

  { TFPCanvasMatrix }

  TFPCanvasMatrix = object
    _00, _01, _10, _11: Double;  // 2x2 linear part (rotation, scale, skew)
    _20, _21: Double;            // translation
    function Transform(const APoint: TPoint): TPoint; overload;
    function Transform(X, Y: Integer): TPoint; overload;
    class function Identity: TFPCanvasMatrix; static;
    class function CreateTranslation(DX, DY: Double): TFPCanvasMatrix; static;
    class function CreateScale(SX, SY: Double): TFPCanvasMatrix; static;
    class function CreateRotation(ARadians: Double): TFPCanvasMatrix; static;
    function Multiply(const Other: TFPCanvasMatrix): TFPCanvasMatrix;
  end;

  TFPCustomCanvas = class;

  { TFPCanvasHelper }

  TFPCanvasHelper = class(TPersistent)
  private
    FDelayAllocate: boolean;
    FFPColor : TFPColor;
    FAllocated,
    FFixedCanvas : boolean;
    FCanvas : TFPCustomCanvas;
    FFlags : word;
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;
    procedure NotifyCanvas;
  protected
    // flags 0-15 are reserved for FPCustomCanvas
    function GetAllocated: boolean; virtual;
    procedure SetFlags (index:integer; AValue:boolean); virtual;
    function GetFlags (index:integer) : boolean; virtual;
    procedure CheckAllocated (ValueNeeded:boolean);
    procedure SetFixedCanvas (AValue : boolean);
    procedure DoAllocateResources; virtual;
    procedure DoDeAllocateResources; virtual;
    procedure DoCopyProps (From:TFPCanvasHelper); virtual;
    procedure SetFPColor (const AValue:TFPColor); virtual;
    procedure Changing; dynamic;
    procedure Changed; dynamic;
    Procedure Lock;
    Procedure UnLock;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    // prepare helper for use
    procedure AllocateResources (ACanvas : TFPCustomCanvas;
                                 CanDelay: boolean = true);
    // free all resource used by this helper
    procedure DeallocateResources;
    property Allocated : boolean read GetAllocated;
    // properties cannot be changed when allocated
    property FixedCanvas : boolean read FFixedCanvas;
    // Canvas for which the helper is allocated
    property Canvas : TFPCustomCanvas read FCanvas;
    // color of the helper
    property FPColor : TFPColor read FFPColor Write SetFPColor;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property DelayAllocate: boolean read FDelayAllocate write FDelayAllocate;
  end;

  TFPCustomFont = class (TFPCanvasHelper)
  private
    FName : string;
    FOrientation,
    FSize : integer;
  protected
    procedure DoCopyProps (From:TFPCanvasHelper); override;
    procedure SetName (AValue:string); virtual;
    procedure SetSize (AValue:integer); virtual;
    procedure SetOrientation (AValue:integer); virtual;
    function GetOrientation : Integer;
  public
    function CopyFont : TFPCustomFont;
    // Creates a copy of the font with all properties the same, but not allocated
    procedure GetTextSize (text:ansistring; var w,h:integer);
    function GetTextHeight (text:ansistring) : integer;
    function GetTextWidth (text:ansistring) : integer;
    property Name : string read FName write SetName;
    property Size : integer read FSize write SetSize;
    property Bold : boolean index 5 read GetFlags write SetFlags;
    property Italic : boolean index 6 read GetFlags write SetFlags;
    property Underline : boolean index 7 read GetFlags write SetFlags;
    property StrikeThrough : boolean index 8 read GetFlags write SetFlags;
    property Orientation: Integer read GetOrientation write SetOrientation default 0;

  end;
  TFPCustomFontClass = class of TFPCustomFont;

  TFPPenStyle = (psSolid, psDash, psDot, psDashDot, psDashDotDot, psinsideFrame, psPattern,psClear);
  TFPPenStyleSet = set of TFPPenStyle;
  TFPPenMode = (pmBlack, pmWhite, pmNop, pmNot, pmCopy, pmNotCopy,
                pmMergePenNot, pmMaskPenNot, pmMergeNotPen, pmMaskNotPen, pmMerge,
                pmNotMerge, pmMask, pmNotMask, pmXor, pmNotXor);
  TPenPattern = Longword;
  TFPPenEndCap = (
    pecRound,
    pecSquare,
    pecFlat
  );
  TFPPenJoinStyle = (
    pjsRound,
    pjsBevel,
    pjsMiter
  );

  { TFPCustomPen }

  TFPCustomPen = class (TFPCanvasHelper)
  private
    FStyle : TFPPenStyle;
    FWidth : Integer;
    FMode : TFPPenMode;
    FPattern : longword;
    FEndCap: TFPPenEndCap;
    FJoinStyle: TFPPenJoinStyle;
  protected
    procedure DoCopyProps (From:TFPCanvasHelper); override;
    procedure SetMode (AValue : TFPPenMode); virtual;
    procedure SetWidth (AValue : Integer); virtual;
    procedure SetStyle (AValue : TFPPenStyle); virtual;
    procedure SetPattern (AValue : longword); virtual;
    procedure SetEndCap(AValue: TFPPenEndCap); virtual;
    procedure SetJoinStyle(AValue: TFPPenJoinStyle); virtual;
  public
    function CopyPen : TFPCustomPen;
    // Creates a copy of the pen with all properties the same, but not allocated
    property Style : TFPPenStyle read FStyle write SetStyle;
    property Width : Integer read FWidth write SetWidth;
    property Mode : TFPPenMode read FMode write SetMode;
    property Pattern : longword read FPattern write SetPattern;
    property EndCap : TFPPenEndCap read FEndCap write SetEndCap;
    property JoinStyle : TFPPenJoinStyle read FJoinStyle write SetJoinStyle;
  end;
  TFPCustomPenClass = class of TFPCustomPen;

  TFPBrushStyle = (bsSolid, bsClear, bsHorizontal, bsVertical, bsFDiagonal,
                   bsBDiagonal, bsCross, bsDiagCross, bsImage, bsPattern);
  TBrushPattern = array[0..PatternBitCount-1] of TPenPattern;
  PBrushPattern = ^TBrushPattern;

  TFPCustomBrush = class (TFPCanvasHelper)
  private
    FStyle : TFPBrushStyle;
    FImage : TFPCustomImage;
    FPattern : TBrushPattern;
  protected
    procedure SetStyle (AValue : TFPBrushStyle); virtual;
    procedure SetImage (AValue : TFPCustomImage); virtual;
    procedure DoCopyProps (From:TFPCanvasHelper); override;
  public
    function CopyBrush : TFPCustomBrush;
    property Style : TFPBrushStyle read FStyle write SetStyle;
    property Image : TFPCustomImage read FImage write SetImage;
    property Pattern : TBrushPattern read FPattern write FPattern;
  end;
  TFPCustomBrushClass = class of TFPCustomBrush;

  { TFPCustomInterpolation }

  TFPCustomInterpolation = class
  private
    fcanvas: TFPCustomCanvas;
    fimage: TFPCustomImage;
  protected
    procedure Initialize (aimage:TFPCustomImage; acanvas:TFPCustomCanvas); virtual;
    procedure Execute (x,y,w,h:integer); virtual; abstract;
  public
    property Canvas : TFPCustomCanvas read fcanvas;
    property Image : TFPCustomImage read fimage;
  end;

  { TFPBoxInterpolation }

  TFPBoxInterpolation = class(TFPCustomInterpolation)
  public
    procedure Execute (x,y,w,h:integer); override;
  end;

  { TFPBaseInterpolation }

  TFPBaseInterpolation = class (TFPCustomInterpolation)
  protected
    procedure Execute (x,y,w,h : integer); override;
    function Filter (x : double): double; virtual;
    function MaxSupport : double; virtual;
  end;

  { TMitchellInterpolation }

  TMitchellInterpolation = class (TFPBaseInterpolation)
  protected
    function Filter (x : double) : double; override;
    function MaxSupport : double; override;
  end;
  TMitchelInterpolation = TMitchellInterpolation deprecated 'Use TMitchellInterpolation';

  TFPCustomRegion = class
  public
    function GetBoundingRect: TRect; virtual; abstract;
    function IsPointInRegion(AX, AY: Integer): Boolean; virtual; abstract;
  end;

  { TFPRectRegion }

  TFPRectRegion = class(TFPCustomRegion)
  public
    Rect: TRect;
    function GetBoundingRect: TRect; override;
    function IsPointInRegion(AX, AY: Integer): Boolean; override;
  end;

  TFPDrawingMode = (dmOpaque, dmAlphaBlend, dmCustom);
  TFPCanvasCombineColors = function(const color1, color2: TFPColor): TFPColor of object;
  TFPGradientDirection = (gdVertical, gdHorizontal);
  // rmInclude: Right and Bottom of a rectangle are painted; rmExclude: they are not (as in TRect)
  TRectangleMode = (rmInclude, rmExclude);
  // emCentered: a thick ellipse outline is centred on the bounds; emInside: it stays inside them
  TEllipseMode = (emCentered, emInside);
  // hoDefault: rectangles and polygons are hatched from their own corner, ellipses and flood fills
  // from the canvas origin; hoShape: every hatch starts at the top-left of the shape, or at the start
  // point of a flood fill; hoCanvas: every hatch starts at the canvas origin.
  THatchOrigin = (hoDefault, hoShape, hoCanvas);
  // toBaseline: the y of TextOut is on the baseline of the text; toTop: it is at the top of the text cell.
  TFPTextOrigin = (toBaseline, toTop);
  // tmInk: TextWidth and TextHeight measure the drawn pixels; tmFont: the advance width and ascent plus descent.
  TFPTextMeasure = (tmInk, tmFont);
  // Font metrics in pixels; Descender counts down from the baseline and is positive.
  TFPTextMetric = record
    Ascender, Descender, Height: Integer;
  end;
  // ffSurface: fill the area of the given colour around the start point; ffBorder: fill up to pixels of the given colour.
  TFPFloodFillStyle = (ffSurface, ffBorder);
  // The vertical placement of text in TextRect.
  TFPTextLayout = (ftlTop, ftlCenter, ftlBottom);
  // How TextRect lays out text; the fields are those of the LCL TTextStyle.
  TFPTextStyle = packed record
    Alignment : TAlignment;
    Layout : TFPTextLayout;
    SingleLine : boolean;
    Clipping : boolean;
    ExpandTabs : boolean;
    ShowPrefix : boolean;
    Wordbreak : boolean;
    Opaque : boolean;
    SystemFont : boolean;
    RightToLeft : boolean;
    EndEllipsis : boolean;
  end;

  { TFPCustomCanvas }

  TFPCustomCanvas = class(TPersistent)
  private
    FMatrix: TFPCanvasMatrix;
    FClipping,
    FManageResources: boolean;
    FRemovingHelpers : boolean;
    FHelpers : TList;
    FLocks : integer;
    FInterpolation : TFPCustomInterpolation;
    FDrawingMode : TFPDrawingMode;
    FOnCombineColors : TFPCanvasCombineColors;
    FRectangleMode : TRectangleMode;
    FEllipseMode : TEllipseMode;
    FHashWidth : word;
    FHatchOrigin : THatchOrigin;
    FRelativeBrushImage : boolean;
    FNonZeroWindingRule : boolean;
    FTextOrigin : TFPTextOrigin;
    FTextMeasure : TFPTextMeasure;
    FPenShapeLevel : integer;
    FPenShapeWidth, FPenShapeHeight : integer;
    FPenShapeDone : array of byte;
    FTextStyle : TFPTextStyle;
    // Fills Pts with the brush and outlines them with the pen.
    procedure ShapePolygon(const Pts: array of TPoint);
    // Adds the lines of aLine broken between words to fit aWidth pixels to aLines, each with its start offset in aLine.
    procedure WrapTextLine(const aLine: string; aWidth: integer; aLines: TStrings);
    // Returns aLine, or its start followed by '...' when it is wider than aWidth pixels.
    function EllipsisText(const aLine: string; aWidth: integer): string;
    // Moves (x, y) from TextOrigin to the origin DoTextOut uses, across the rotation of the font.
    procedure MoveToTextOrigin(var x, y: integer);
    // True when TextMeasure asks for font metrics from a canvas that measures ink.
    function MeasuresByFont: boolean;
    function AllowFont (AFont : TFPCustomFont) : boolean;
    function AllowBrush (ABrush : TFPCustomBrush) : boolean;
    function AllowPen (APen : TFPCustomPen) : boolean;
    function CreateDefaultFont : TFPCustomFont;
    function CreateDefaultPen : TFPCustomPen;
    function CreateDefaultBrush : TFPCustomBrush;
    procedure RemoveHelpers;
    function GetFont : TFPCustomFont;
    function GetBrush : TFPCustomBrush;
    function GetPen : TFPCustomPen;
  protected
    FDefaultFont, FFont : TFPCustomFont;
    FDefaultBrush, FBrush : TFPCustomBrush;
    FDefaultPen, FPen : TFPCustomPen;
    FPenPos : TPoint;
    FClipRegion : TFPCustomRegion;
    // Starts a shape in which the pen combines each pixel with Pen.Mode at most once, even where
    // its strokes overlap, until the matching EndPenShape.
    procedure BeginPenShape;
    // Ends a shape started with BeginPenShape.
    procedure EndPenShape;
    function DoCreateDefaultFont : TFPCustomFont; virtual; abstract;
    function DoCreateDefaultPen : TFPCustomPen; virtual; abstract;
    function DoCreateDefaultBrush : TFPCustomBrush; virtual; abstract;
    procedure SetFont (AValue:TFPCustomFont); virtual;
    procedure SetBrush (AValue:TFPCustomBrush); virtual;
    procedure SetPen (AValue:TFPCustomPen); virtual;
    function  DoAllowFont (AFont : TFPCustomFont) : boolean; virtual;
    function  DoAllowPen (APen : TFPCustomPen) : boolean; virtual;
    function  DoAllowBrush (ABrush : TFPCustomBrush) : boolean; virtual;
    procedure SetColor (x,y:integer; const Value:TFPColor); Virtual; abstract;
    function  GetColor (x,y:integer) : TFPColor; Virtual; abstract;
    procedure SetHeight (AValue : integer); virtual; abstract;
    function  GetHeight : integer; virtual; abstract;
    procedure SetWidth (AValue : integer); virtual; abstract;
    function  GetWidth : integer; virtual; abstract;
    function  GetClipRect: TRect; virtual;
    function  GetDeviceClipRect: TRect; virtual;
    procedure SetClipRect(const AValue: TRect); virtual;
    function  GetClipping: boolean; virtual;
    procedure SetClipping(const AValue: boolean); virtual;
    procedure SetPenPos(const AValue: TPoint); virtual;
    procedure SetClipRegion(const AValue: TFPCustomRegion);
    procedure DoLockCanvas; virtual;
    procedure DoUnlockCanvas; virtual;
    procedure DoTextOut (x,y:integer;text:ansistring); virtual; abstract;
    procedure DoGetTextSize (text:ansistring; var w,h:integer); virtual; abstract;
    function  DoGetTextHeight (text:ansistring) : integer; virtual; abstract;
    function  DoGetTextWidth (text:ansistring) : integer; virtual; abstract;
    procedure DoTextOut (x,y:integer;text:unicodestring); virtual;
    procedure DoGetTextSize (text:unicodestring; var w,h:integer); virtual;
    function  DoGetTextHeight (text:unicodestring) : integer; virtual;
    function  DoGetTextWidth (text:unicodestring) : integer; virtual;
    // Returns where DoTextOut puts the y of the text; toTop unless a descendant draws from the baseline.
    function GetNativeTextOrigin : TFPTextOrigin; virtual;
    // Returns what DoGetTextWidth and DoGetTextHeight measure; tmFont unless a descendant measures ink.
    function GetNativeTextMeasure : TFPTextMeasure; virtual;
    // Returns the metrics of Font in pixels; False when the canvas has none.
    function DoGetTextMetrics (out aMetrics: TFPTextMetric) : boolean; virtual;
    // Returns the advance width of text in pixels.
    function DoGetTextAdvance (text:ansistring) : integer; virtual;
    // Fills and outlines the chord of the ellipse in the device pixels Bounds (Right and Bottom included).
    procedure DoChord (const Bounds: TRect; aStart16, aLength16: integer); virtual;
    // Fills and outlines the device pixels Bounds (Right and Bottom included) with corners rounded by RX x RY ellipses.
    procedure DoRoundRect (const Bounds: TRect; RX, RY: integer); virtual;
    // Flood fills from the device pixel (x, y) with the brush, as FloodFill with a FillStyle does.
    procedure DoFloodFillStyle (x, y: integer; const FillColor: TFPColor; FillStyle: TFPFloodFillStyle); virtual;
    procedure DoRectangle (Const Bounds:TRect); virtual; abstract;
    procedure DoRectangleFill (Const Bounds:TRect); virtual; abstract;
    procedure DoRectangleAndFill (Const Bounds:TRect); virtual;
    procedure DoEllipseFill (Const Bounds:TRect); virtual; abstract;
    procedure DoEllipse (Const Bounds:TRect); virtual; abstract;
    procedure DoEllipseAndFill (Const Bounds:TRect); virtual;
    procedure DoPolygonFill (const points:array of TPoint); virtual; abstract;
    procedure DoPolygon (const points:array of TPoint); virtual; abstract;
    procedure DoPolygonAndFill (const points:array of TPoint); virtual;
    procedure DoPolyline (const points:array of TPoint); virtual; abstract;
    procedure DoFloodFill (x,y:integer); virtual; abstract;
    procedure DoMoveTo (x,y:integer); virtual;
    procedure DoLineTo (x,y:integer); virtual;
    procedure DoLine (x1,y1,x2,y2:integer); virtual; abstract;
    // Copies the device pixels SourceRect (Right and Bottom included) of canvas to the device pixel (x, y).
    procedure DoCopyRect (x,y:integer; canvas:TFPCustomCanvas; Const SourceRect:TRect); virtual;
    // Draws image with its top-left pixel at the device pixel (x, y), combined by DrawingMode.
    procedure DoDraw (x,y:integer; Const image:TFPCustomImage); virtual;
    // Draws source scaled to the w x h device pixels from (x, y) with Interpolation, Mitchell when nil.
    procedure DoStretchDraw (x,y,w,h:integer; source:TFPCustomImage); virtual;
    procedure DoRadialPie(x1, y1, x2, y2, StartAngle16Deg, Angle16DegLength: Integer); virtual;
    procedure DoPolyBezier(Points: PPoint; NumPts: Integer;
                           Filled: boolean = False;
                           Continuous: boolean = False); virtual;
    procedure CheckHelper (AHelper:TFPCanvasHelper); virtual;
    procedure AddHelper (AHelper:TFPCanvasHelper);
    function TransformPoint(X, Y: Integer): TPoint;
    function TransformRect(const R: TRect): TRect;
    function TransformPoints(const Points: array of TPoint): TFPCanvasPointArray;
    function HasRotation: Boolean;
    // Returns in aResult aRect with its corners ordered and, under rmExclude, Right and Bottom moved in
    // by one pixel so that both are included; returns False when the rectangle covers no pixel.
    function UserRect(const aRect: TRect; out aResult: TRect): Boolean;
    // Returns in aResult the device pixels covered by aRect: the UserRect result mapped through the
    // transformation, Right and Bottom included; returns False when it covers no pixel.
    function DeviceRect(const aRect: TRect; out aResult: TRect): Boolean;
    // The clip rectangle in device pixels, Right and Bottom included, whatever RectangleMode is.
    property DeviceClipRect : TRect read GetDeviceClipRect;
  public
    constructor create;
    destructor destroy; override;
    procedure LockCanvas;
    procedure UnlockCanvas;
    function Locked: boolean;
    function CreateFont : TFPCustomFont;
    function CreatePen : TFPCustomPen;
    function CreateBrush : TFPCustomBrush;
    // using font
    procedure TextOut (x,y:integer;text:Ansistring); virtual;
    procedure GetTextSize (text:Ansistring; var w,h:integer);
    function GetTextHeight (text:Ansistring) : integer;
    function GetTextWidth (text:Ansistring) : integer;
    function TextExtent(const Text: Ansistring): TSize; virtual;
    function TextHeight(const Text: Ansistring): Integer; virtual;
    function TextWidth(const Text: Ansistring): Integer; virtual;
    procedure TextOut (x,y:integer;text:unicodestring); virtual;
    procedure GetTextSize (text:unicodestring; var w,h:integer);
    function GetTextHeight (text:unicodestring) : integer;
    function GetTextWidth (text:unicodestring) : integer;
    function TextExtent(const Text: unicodestring): TSize; virtual;
    function TextHeight(const Text: unicodestring): Integer; virtual;
    function TextWidth(const Text: unicodestring): Integer; virtual;
    // using pen and brush
    procedure Arc(ALeft, ATop, ARight, ABottom, Angle16Deg, Angle16DegLength: Integer); virtual;
    procedure Arc(ALeft, ATop, ARight, ABottom, SX, SY, EX, EY: Integer); virtual;
    procedure Ellipse (Const Bounds:TRect); virtual;
    procedure Ellipse (left,top,right,bottom:integer); virtual;
    procedure EllipseC (x,y:integer; rx,ry:longword);
    procedure Polygon (Const points:array of TPoint); virtual;
    procedure Polyline (Const points:array of TPoint); virtual;
    procedure RadialPie(x1, y1, x2, y2, StartAngle16Deg, Angle16DegLength: Integer); virtual;
    procedure PolyBezier(Points: PPoint; NumPts: Integer;
                         Filled: boolean = False;
                         Continuous: boolean = False);  virtual;
    procedure PolyBezier(const Points: array of TPoint;
                         Filled: boolean = False;
                         Continuous: boolean = False); virtual;
    procedure Rectangle (Const Bounds : TRect); virtual;
    procedure Rectangle (left,top,right,bottom:integer); virtual;
    procedure FillRect(const ARect: TRect);  virtual;
    procedure FillRect(X1,Y1,X2,Y2: Integer); virtual;
    // using brush
    procedure FloodFill (x,y:integer); virtual;
    procedure Clear;
    // using pen
    procedure MoveTo (x,y:integer);
    procedure MoveTo (p:TPoint);
    procedure LineTo (x,y:integer);
    procedure LineTo (p:TPoint);
    procedure Line (x1,y1,x2,y2:integer);
    procedure Line (const p1,p2:TPoint);
    procedure Line (const points:TRect);
    // other procedures
    procedure CopyRect (x,y:integer; canvas:TFPCustomCanvas; SourceRect:TRect); virtual;
    procedure Draw (x,y:integer; image:TFPCustomImage); virtual;
    procedure StretchDraw (x,y,w,h:integer; source:TFPCustomImage); virtual;
    // Draws source scaled to DestRect.
    procedure StretchDraw (const DestRect: TRect; source: TFPCustomImage);
    // Copies Source of SrcCanvas scaled onto Dest.
    procedure CopyRect (const Dest: TRect; SrcCanvas: TFPCustomCanvas; const Source: TRect); virtual;
    // Draws the line from PenPos to the start of the arc, the arc as Arc does, and moves PenPos to the end of the arc.
    procedure ArcTo (ALeft, ATop, ARight, ABottom, SX, SY, EX, EY: Integer); virtual;
    // Draws the line from PenPos to the arc of the circle at (X, Y) over SweepAngle degrees from StartAngle, the arc, and moves PenPos to its end.
    procedure AngleArc (X, Y: Integer; Radius: Longword; StartAngle, SweepAngle: Single);
    // Fills and outlines the chord of the ellipse cut by the rays at Angle16Deg and Angle16Deg + Angle16DegLength, in 1/16 degree.
    procedure Chord (x1, y1, x2, y2, Angle16Deg, Angle16DegLength: Integer); virtual;
    // Fills and outlines the chord of the ellipse cut by the rays through (SX, SY) and (EX, EY).
    procedure Chord (x1, y1, x2, y2, SX, SY, EX, EY: Integer); virtual;
    // Fills and outlines the pie of the ellipse from the ray through (StartX, StartY) counter-clockwise to the ray through (EndX, EndY).
    procedure Pie (EllipseX1, EllipseY1, EllipseX2, EllipseY2, StartX, StartY, EndX, EndY: Integer); virtual;
    // Fills and outlines the rectangle with its corners rounded by ellipses of RX x RY pixels.
    procedure RoundRect (X1, Y1, X2, Y2: Integer; RX, RY: Integer); virtual;
    // Fills and outlines Rect with its corners rounded by ellipses of RX x RY pixels.
    procedure RoundRect (const Rect: TRect; RX, RY: Integer);
    // Draws the outline of ARect with the pen.
    procedure Frame (const ARect: TRect); virtual;
    // Draws the outline of the rectangle with the pen.
    procedure Frame (X1, Y1, X2, Y2: Integer);
    // Draws a border of one pixel around the inside of ARect with the brush.
    procedure FrameRect (const ARect: TRect); virtual;
    // Draws a border of one pixel around the inside of the rectangle with the brush.
    procedure FrameRect (X1, Y1, X2, Y2: Integer);
    // Draws FrameWidth rings inside ARect, left and top in TopColor, right and bottom in BottomColor, and shrinks ARect by them.
    procedure Frame3D (var ARect: TRect; const TopColor, BottomColor: TFPColor; const FrameWidth: integer); virtual;
    // Draws a dotted outline of ARect with pmXor, which a second call removes.
    procedure DrawFocusRect (const ARect: TRect); virtual;
    // Flood fills from (X, Y) with the brush: the area of FillColor with ffSurface, the area up to FillColor with ffBorder.
    procedure FloodFill (X, Y: Integer; const FillColor: TFPColor; FillStyle: TFPFloodFillStyle); virtual;
    // Draws NumPts points of Points from StartIndex (all when -1) as a polygon, filled by the non-zero rule when Winding.
    procedure Polygon (const Points: array of TPoint; Winding: Boolean; StartIndex: Integer = 0; NumPts: Integer = -1);
    // Draws the NumPts points at Points as a polygon, filled by the non-zero rule when Winding.
    procedure Polygon (Points: PPoint; NumPts: Integer; Winding: boolean = False); virtual;
    // Draws NumPts points of Points from StartIndex (all when -1) as connected lines.
    procedure Polyline (const Points: array of TPoint; StartIndex: Integer; NumPts: Integer = -1);
    // Draws the NumPts points at Points as connected lines.
    procedure Polyline (Points: PPoint; NumPts: Integer); virtual;
    // Writes Text in ARect laid out by TextStyle, from (X, Y) when it is left and top aligned.
    procedure TextRect (const ARect: TRect; X, Y: integer; const Text: string);
    // Writes Text in ARect laid out by Style, from (X, Y) when it is left and top aligned.
    procedure TextRect (ARect: TRect; X, Y: integer; const Text: string; const Style: TFPTextStyle); virtual;
    // Returns how many characters of the UTF-8 Text fit in MaxWidth pixels.
    function TextFitInfo (const Text: string; MaxWidth: Integer): Integer; virtual;
    procedure Erase;virtual;
    procedure DrawPixel(const x, y: integer; const newcolor: TFPColor);
    // Draws a pen pixel, combined with the pixel at (x, y) by Pen.Mode.
    procedure DrawPenPixel(const x, y: integer; const newcolor: TFPColor);
    procedure GradientFill(const ARect: TRect; AStartColor, AEndColor: TFPColor; ADirection: TFPGradientDirection); virtual;
    // coordinate transformation
    property TransformMatrix: TFPCanvasMatrix read FMatrix write FMatrix;
    procedure Translate(DX, DY: Double);
    procedure Scale(SX, SY: Double);
    procedure Rotate(ARadians: Double);
    procedure ResetTransform;
    function HasTransform: Boolean;
    // properties
    property LockCount: Integer read FLocks;
    property Font : TFPCustomFont read GetFont write SetFont;
    property Pen : TFPCustomPen read GetPen write SetPen;
    property Brush : TFPCustomBrush read GetBrush write SetBrush;
    property Interpolation : TFPCustomInterpolation read FInterpolation write FInterpolation;
    property Colors [x,y:integer] : TFPColor read GetColor write SetColor;
    property ClipRect : TRect read GetClipRect write SetClipRect;
    property ClipRegion : TFPCustomRegion read FClipRegion write SetClipRegion;
    property Clipping : boolean read GetClipping write SetClipping;
    property PenPos : TPoint read FPenPos write SetPenPos;
    property Height : integer read GetHeight write SetHeight;
    property Width : integer read GetWidth write SetWidth;
    property ManageResources: boolean read FManageResources write FManageResources;
    property DrawingMode : TFPDrawingMode read FDrawingMode write FDrawingMode;
    // Whether the Right and Bottom of rectangles given to the canvas (shapes, fills, ClipRect, CopyRect) are painted.
    property RectangleMode : TRectangleMode read FRectangleMode write FRectangleMode;
    // Whether a thick ellipse outline is centred on its bounds or drawn inside them.
    property EllipseMode : TEllipseMode read FEllipseMode write FEllipseMode;
    property OnCombineColors : TFPCanvasCombineColors read FOnCombineColors write FOnCombineColors;
    // The distance in pixels between the lines of hatch brushes.
    property HashWidth : word read FHashWidth write FHashWidth;
    // Where hatch brushes start counting their lines.
    property HatchOrigin : THatchOrigin read FHatchOrigin write FHatchOrigin;
    // Whether bsImage brushes tile from the shape instead of the canvas origin.
    property RelativeBrushImage : boolean read FRelativeBrushImage write FRelativeBrushImage;
    // Whether polygons fill by the non-zero winding rule instead of the even-odd rule.
    property PolygonNonZeroWindingRule : boolean read FNonZeroWindingRule write FNonZeroWindingRule;
    // Returns the ascender, descender and height of Font in pixels; False when they are not known.
    function GetTextMetrics (out aMetrics: TFPTextMetric) : boolean;
    // Where the y of TextOut lies; starts as the convention of the canvas.
    property TextOrigin : TFPTextOrigin read FTextOrigin write FTextOrigin;
    // How TextWidth and TextHeight measure; starts as the convention of the canvas, tmInk needs a canvas that measures ink.
    property TextMeasure : TFPTextMeasure read FTextMeasure write FTextMeasure;
    // The layout of TextRect without a style; starts as the LCL default.
    property TextStyle : TFPTextStyle read FTextStyle write FTextStyle;
  end;

  TFPCustomDrawFont = class (TFPCustomFont)
  private
    procedure DrawText (x,y:integer; text:Ansistring);
    procedure GetTextSize (text:ansistring; var w,h:integer);
    function GetTextHeight (text:ansistring) : integer;
    function GetTextWidth (text:ansistring) : integer;
    procedure DrawText (x,y:integer; text:unicodestring);
    procedure GetTextSize (text: unicodestring; var w,h:integer);
    function GetTextHeight (text: unicodestring) : integer;
    function GetTextWidth (text: unicodestring) : integer;
  protected
    procedure DoDrawText (x,y:integer; text:ansistring); virtual; abstract;
    procedure DoGetTextSize (text:ansistring; var w,h:integer); virtual; abstract;
    function DoGetTextHeight (text:ansistring) : integer; virtual; abstract;
    function DoGetTextWidth (text:ansistring) : integer; virtual; abstract;
    procedure DoDrawText (x,y:integer; text:unicodestring); virtual;
    procedure DoGetTextSize (text: unicodestring; var w,h:integer); virtual;
    function DoGetTextHeight (text: unicodestring) : integer; virtual;
    function DoGetTextWidth (text: unicodestring) : integer; virtual;
    // Returns the metrics of the font in pixels; False when they are not known.
    function DoGetTextMetrics (out aMetrics: TFPTextMetric) : boolean; virtual;
    // Returns the advance width of text in pixels.
    function DoGetTextAdvance (text:ansistring) : integer; virtual;
    function DoGetTextAdvance (text: unicodestring) : integer; virtual;
  end;

  TFPEmptyFont = class (TFPCustomFont)
  end;

  TFPCustomDrawPen = class (TFPCustomPen)
  private
    procedure DrawLine (x1,y1,x2,y2:integer);
    procedure Polyline (const points:array of TPoint; close:boolean);
    procedure Ellipse (left,top, right,bottom:integer);
    procedure Rectangle (left,top, right,bottom:integer);
  protected
    procedure DoDrawLine (x1,y1,x2,y2:integer); virtual; abstract;
    procedure DoPolyline (const points:array of TPoint; close:boolean); virtual; abstract;
    procedure DoEllipse (left,top, right,bottom:integer); virtual; abstract;
    procedure DoRectangle (left,top, right,bottom:integer); virtual; abstract;
  end;

  TFPEmptyPen = class (TFPCustomPen)
  end;

  TFPCustomDrawBrush = class (TFPCustomBrush)
  private
    procedure Rectangle (left,top, right,bottom:integer);
    procedure FloodFill (x,y:integer);
    procedure Ellipse (left,top, right,bottom:integer);
    procedure Polygon (const points:array of TPoint);
  public
    procedure DoRectangle (left,top, right,bottom:integer); virtual; abstract;
    procedure DoEllipse (left,top, right,bottom:integer); virtual; abstract;
    procedure DoFloodFill (x,y:integer); virtual; abstract;
    procedure DoPolygon (const points:array of TPoint); virtual; abstract;
  end;

  TFPEmptyBrush = class (TFPCustomBrush)
  end;

procedure DecRect (var rect : TRect; delta:integer);
procedure IncRect (var rect : TRect; delta:integer);
procedure DecRect (var rect : TRect);
procedure IncRect (var rect : TRect);
// Returns the colour that pen mode aMode gives when pen colour aPen is drawn over pixel colour aDest,
// computed per RGB channel; the result has the alpha of aDest.
function PenModeColor(aMode: TFPPenMode; const aPen, aDest: TFPColor): TFPColor;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses FpImage.Clipping;
{$ELSE FPC_DOTTEDUNITS}
uses clipping;
{$ENDIF FPC_DOTTEDUNITS}

const
  EFont = 'Font';
  EPen = 'Pen';
  EBrush = 'Brush';
  ErrAllocation = '%s %s be allocated.';
  ErrAlloc : array [boolean] of string = ('may not','must');
  ErrCouldNotCreate = 'Could not create a %s.';
  ErrNoLock = 'Canvas not locked.';

procedure DecRect (var rect : TRect; delta:integer);
begin
  with rect do
    begin
    left := left + delta;
    right := right - delta;
    top := top + delta;
    bottom := bottom - delta;
    end;
end;

procedure DecRect (var rect : trect);
begin
  DecRect (rect, 1);
end;

procedure IncRect (var rect : trect);
begin
  IncRect (rect, 1);
end;

procedure IncRect (var rect : TRect; delta:integer);
begin
  with rect do
    begin
    left := left - delta;
    right := right + delta;
    top := top - delta;
    bottom := bottom + delta;
    end;
end;

function PenModeColor(aMode: TFPPenMode; const aPen, aDest: TFPColor): TFPColor;

  function Channel(aP, aD: Word): Word;
  begin
    case aMode of
      pmBlack: Result := 0;
      pmWhite: Result := $FFFF;
      pmNop: Result := aD;
      pmNot: Result := not aD;
      pmCopy: Result := aP;
      pmNotCopy: Result := not aP;
      pmMergePenNot: Result := aP or not aD;
      pmMaskPenNot: Result := aP and not aD;
      pmMergeNotPen: Result := not aP or aD;
      pmMaskNotPen: Result := not aP and aD;
      pmMerge: Result := aP or aD;
      pmNotMerge: Result := not (aP or aD);
      pmMask: Result := aP and aD;
      pmNotMask: Result := not (aP and aD);
      pmXor: Result := aP xor aD;
    else
      Result := not (aP xor aD);
    end;
  end;

begin
  Result.Red := Channel(aPen.Red, aDest.Red);
  Result.Green := Channel(aPen.Green, aDest.Green);
  Result.Blue := Channel(aPen.Blue, aDest.Blue);
  Result.Alpha := aDest.Alpha;
end;


{ TFPRectRegion }

function TFPRectRegion.GetBoundingRect: TRect;
begin
  Result := Rect;
end;

function TFPRectRegion.IsPointInRegion(AX, AY: Integer): Boolean;
begin
  Result := (AX >= Rect.Left) and (AX <= Rect.Right) and
    (AY >= Rect.Top) and (AY <= Rect.Bottom);
end;

{$i fpmatrix.inc}
{$i FPHelper.inc}
{$i FPFont.inc}
{$i FPPen.inc}
{$i FPBrush.inc}
{$i fpinterpolation.inc}
{$i FPCanvas.inc}
{$i FPCDrawH.inc}

end.
