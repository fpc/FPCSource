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
unit FPCanvas;

interface

uses Math, sysutils, classes, FPImage;

const
  PatternBitCount = sizeof(longword) * 8;

type

  TFPCanvasException = class (Exception);
  TFPPenException = class (TFPCanvasException);
  TFPBrushException = class (TFPCanvasException);
  TFPFontException = class (TFPCanvasException);

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
    FSize : integer;
  protected
    procedure DoCopyProps (From:TFPCanvasHelper); override;
    procedure SetName (AValue:string); virtual;
    procedure SetSize (AValue:integer); virtual;
  public
    function CopyFont : TFPCustomFont;
    // Creates a copy of the font with all properties the same, but not allocated
    procedure GetTextSize (text:string; var w,h:integer);
    function GetTextHeight (text:string) : integer;
    function GetTextWidth (text:string) : integer;
    property Name : string read FName write SetName;
    property Size : integer read FSize write SetSize;
    property Bold : boolean index 5 read GetFlags write SetFlags;
    property Italic : boolean index 6 read GetFlags write SetFlags;
    property Underline : boolean index 7 read GetFlags write SetFlags;
    property StrikeTrough : boolean index 8 read GetFlags write SetFlags;
  end;
  TFPCustomFontClass = class of TFPCustomFont;

  TFPPenStyle = (psSolid, psDash, psDot, psDashDot, psDashDotDot, psinsideFrame, psPattern,psClear);
  TFPPenStyleSet = set of TFPPenStyle;
  TFPPenMode = (pmBlack, pmWhite, pmNop, pmNot, pmCopy, pmNotCopy,
                pmMergePenNot, pmMaskPenNot, pmMergeNotPen, pmMaskNotPen, pmMerge,
                pmNotMerge, pmMask, pmNotMask, pmXor, pmNotXor);
  TPenPattern = Longword;

  TFPCustomPen = class (TFPCanvasHelper)
  private
    FStyle : TFPPenStyle;
    FWidth : Integer;
    FMode : TFPPenMode;
    FPattern : longword;
  protected
    procedure DoCopyProps (From:TFPCanvasHelper); override;
    procedure SetMode (AValue : TFPPenMode); virtual;
    procedure SetWidth (AValue : Integer); virtual;
    procedure SetStyle (AValue : TFPPenStyle); virtual;
    procedure SetPattern (AValue : longword); virtual;
  public
    function CopyPen : TFPCustomPen;
    // Creates a copy of the pen with all properties the same, but not allocated
    property Style : TFPPenStyle read FStyle write SetStyle;
    property Width : Integer read FWidth write SetWidth;
    property Mode : TFPPenMode read FMode write SetMode;
    property Pattern : longword read FPattern write SetPattern;
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

  { TFPBaseInterpolation }

  TFPBaseInterpolation = class (TFPCustomInterpolation)
  private
    procedure CreatePixelWeights (OldSize, NewSize: integer;
      out Entries: Pointer; out EntrySize: integer; out Support: integer);
  protected
    procedure Execute (x,y,w,h : integer); override;
    function Filter (x : double): double; virtual;
    function MaxSupport : double; virtual;
  end;

  { TMitchelInterpolation }

  TMitchelInterpolation = class (TFPBaseInterpolation)
  protected
    function Filter (x : double) : double; override;
    function MaxSupport : double; override;
  end;

  { TFPCustomCanvas }

  TFPCustomCanvas = class(TPersistent)
  private
    FClipping,
    FManageResources: boolean;
    FRemovingHelpers : boolean;
    FDefaultFont,
    FFont : TFPCustomFont;
    FDefaultBrush,
    FBrush : TFPCustomBrush;
    FDefaultPen,
    FPen : TFPCustomPen;
    FPenPos : TPoint;
    FClipRect : TRect;
    FHelpers : TList;
    FLocks : integer;
    FInterpolation : TFPCustomInterpolation;
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
    procedure SetClipRect(const AValue: TRect); virtual;
    procedure SetPenPos(const AValue: TPoint); virtual;
    procedure DoLockCanvas; virtual;
    procedure DoUnlockCanvas; virtual;
    procedure DoTextOut (x,y:integer;text:string); virtual; abstract;
    procedure DoGetTextSize (text:string; var w,h:integer); virtual; abstract;
    function  DoGetTextHeight (text:string) : integer; virtual; abstract;
    function  DoGetTextWidth (text:string) : integer; virtual; abstract;
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
    procedure DoCopyRect (x,y:integer; canvas:TFPCustomCanvas; Const SourceRect:TRect); virtual; abstract;
    procedure DoDraw (x,y:integer; Const image:TFPCustomImage); virtual; abstract;
    procedure CheckHelper (AHelper:TFPCanvasHelper); virtual;
    procedure AddHelper (AHelper:TFPCanvasHelper);
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
    procedure TextOut (x,y:integer;text:string);
    procedure GetTextSize (text:string; var w,h:integer);
    function GetTextHeight (text:string) : integer;
    function GetTextWidth (text:string) : integer;
    // using pen and brush
    procedure Ellipse (Const Bounds:TRect);
    procedure Ellipse (left,top,right,bottom:integer);
    procedure EllipseC (x,y:integer; rx,ry:longword);
    procedure Polygon (Const points:array of TPoint);
    procedure Polyline (Const points:array of TPoint);
    procedure Rectangle (Const Bounds:TRect);
    procedure Rectangle (left,top,right,bottom:integer);
    // using brush
    procedure FloodFill (x,y:integer);
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
    procedure CopyRect (x,y:integer; canvas:TFPCustomCanvas; SourceRect:TRect);
    procedure Draw (x,y:integer; image:TFPCustomImage);
    procedure StretchDraw (x,y,w,h:integer; source:TFPCustomImage);
    procedure Erase;virtual;
    // properties
    property LockCount: Integer read FLocks;
    property Font : TFPCustomFont read GetFont write SetFont;
    property Pen : TFPCustomPen read GetPen write SetPen;
    property Brush : TFPCustomBrush read GetBrush write SetBrush;
    property Interpolation : TFPCustomInterpolation read FInterpolation write FInterpolation;
    property Colors [x,y:integer] : TFPColor read GetColor write SetColor;
    property ClipRect : TRect read GetClipRect write SetClipRect;
    property Clipping : boolean read FClipping write FClipping;
    property PenPos : TPoint read FPenPos write SetPenPos;
    property Height : integer read GetHeight write SetHeight;
    property Width : integer read GetWidth write SetWidth;
    property ManageResources: boolean read FManageResources write FManageResources;
  end;

  TFPCustomDrawFont = class (TFPCustomFont)
  private
    procedure DrawText (x,y:integer; text:string);
    procedure GetTextSize (text:string; var w,h:integer);
    function GetTextHeight (text:string) : integer;
    function GetTextWidth (text:string) : integer;
  protected
    procedure DoDrawText (x,y:integer; text:string); virtual; abstract;
    procedure DoGetTextSize (text:string; var w,h:integer); virtual; abstract;
    function DoGetTextHeight (text:string) : integer; virtual; abstract;
    function DoGetTextWidth (text:string) : integer; virtual; abstract;
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

implementation

uses clipping;

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

{$i FPHelper.inc}
{$i FPFont.inc}
{$i FPPen.inc}
{$i FPBrush.inc}
{$i fpinterpolation.inc}
{$i FPCanvas.inc}
{$i FPCDrawH.inc}

end.
