{
    $Id$
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

uses sysutils, classes, FPImage;

type

  TFPCanvasException = class (Exception);
  TFPPenException = class (TFPCanvasException);
  TFPBrushException = class (TFPCanvasException);
  TFPFontException = class (TFPCanvasException);

  TFPCustomCanvas = class;

  TFPCanvasHelper = class
  private
    FColor : TFPColor;
    FAllocated,
    FFixedCanvas : boolean;
    FCanvas : TFPCustomCanvas;
    FFlags : word;
    function GetAllocated : boolean;
    procedure NotifyCanvas;
  protected
    // flags 0-15 are reserved for FPCustomCanvas
    procedure SetFlags (index:integer; AValue:boolean);
    function GetFlags (index:integer) : boolean;
    procedure CheckAllocated (ValueNeeded:boolean);
    procedure SetFixedCanvas (AValue : boolean);
    procedure DoAllocateResources; virtual;
    procedure DoDeAllocateResources; virtual;
    procedure DoCopyProps (From:TFPCanvasHelper); virtual;
    procedure SetColor (AValue:TFPColor); virtual;
  public
    constructor Create; virtual;
    destructor destroy; override;
    // prepare helper for use
    procedure AllocateResources (ACanvas : TFPCustomCanvas);
    // free all resource used bby this helper
    procedure DeallocateResources;
    property Allocated : boolean read GetAllocated;
    // properties cannot be changed when allocated
    property FixedCanvas : boolean read FFixedCanvas;
    // Canvas for which the helper is allocated
    property Canvas : TFPCustomCanvas read FCanvas;
    // color of the helper
    property Color : TFPColor read FColor Write SetColor;
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

  TFPPenStyle = (psClear, psSolid, psDash, psDot, psDashDot, psDashDotDot, psPattern);
  TFPPenStyleSet = set of TFPPenStyle;
  TFPPenMode = (pmCopy, pmAnd, pmOr, pmXor);

  TFPCustomPen = class (TFPCanvasHelper)
  private
    FStyle : TFPPenStyle;
    FWidth : byte;
    FMode : TFPPenMode;
    FPattern : longword;
  protected
    procedure DoCopyProps (From:TFPCanvasHelper); override;
    procedure SetMode (AValue : TFPPenMode); virtual;
    procedure SetWidth (AValue : byte); virtual;
    procedure SetStyle (AValue : TFPPenStyle); virtual;
    procedure SetPattern (AValue : longword); virtual;
  public
    function CopyPen : TFPCustomPen;
    // Creates a copy of the font with all properties the same, but not allocated
    property Style : TFPPenStyle read FStyle write SetStyle;
    property Width : byte read FWidth write SetWidth;
    property Mode : TFPPenMode read FMode write SetMode;
    property Pattern : longword read FPattern write SetPattern;
  end;
  TFPCustomPenClass = class of TFPCustomPen;

  TFPBrushStyle = (bsClear, bsSolid, bsDiagonal, bsFDiagonal, bsCross,bsDiagCross,
                   bsHorizontal, bsVertical, bsImage, bsPattern);

  TFPCustomBrush = class (TFPCanvasHelper)
  private
    FStyle : TFPBrushStyle;
    FImage : TFPCustomImage;
  protected
    procedure SetStyle (AValue : TFPBrushStyle); virtual;
    procedure SetImage (AValue : TFPCustomImage); virtual;
    procedure DoCopyProps (From:TFPCanvasHelper); override;
  public
    function CopyBrush : TFPCustomBrush;
    property Style : TFPBrushStyle read FStyle write SetStyle;
    property Image : TFPCustomImage read FImage write SetImage;
  end;
  TFPCustomBrushClass = class of TFPCustomBrush;

  TFPCustomCanvas = class
  private
    FClipping,
    FRemovingHelpers : boolean;
    FDefaultFont,
    FFont : TFPCustomFont;
    FDefaultBrush,
    FBrush : TFPCustomBrush;
    FDefaultPen,
    FPen : TFPCustomPen;
    FCurrent : TPoint;
    FClipRect : TRect;
    FHelpers : TList;
    FLocks : integer;
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
    procedure SetColor (x,y:integer; Value:TFPColor); Virtual; abstract;
    function  GetColor (x,y:integer) : TFPColor; Virtual; abstract;    
    procedure SetHeight (AValue : integer); virtual; abstract;
    function  GetHeight : integer; virtual; abstract;
    procedure SetWidth (AValue : integer); virtual; abstract;
    function  GetWidth : integer; virtual; abstract;
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
    procedure DoCopyRect (x,y:integer; canvas:TFPCustomCanvas; SourceRect:TRect); virtual; abstract;
    procedure DoDraw (x,y:integer; image:TFPCustomImage); virtual; abstract;
    procedure CheckHelper (AHelper:TFPCanvasHelper); virtual;
    procedure AddHelper (AHelper:TFPCanvasHelper);
  public
    constructor create;
    destructor destroy; override;
    procedure LockCanvas;
    procedure UnlockCanvas;
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
    procedure Line (p1,p2:TPoint);
    procedure Line (const points:TRect);
    // other procedures
    procedure CopyRect (x,y:integer; canvas:TFPCustomCanvas; SourceRect:TRect);
    procedure Draw (x,y:integer; image:TFPCustomImage);
    // properties
    property Font : TFPCustomFont read GetFont write SetFont;
    property Pen : TFPCustomPen read GetPen write SetPen;
    property Brush : TFPCustomBrush read GetBrush write SetBrush;
    property Colors [x,y:integer] : TFPColor read GetColor write SetColor;
    property ClipRect : TRect read FClipRect write FClipRect;
    property Clipping : boolean read FClipping write FClipping;
    property PenPos : TPoint read FCurrent write FCurrent;
    property Height : integer read GetHeight write SetHeight;
    property Width : integer read GetWidth write SetWidth;
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
    procedure Polyline (points:array of TPoint; close:boolean);
    procedure Ellipse (left,top, right,bottom:integer);
    procedure Rectangle (left,top, right,bottom:integer);
  protected
    procedure DoDrawLine (x1,y1,x2,y2:integer); virtual; abstract;
    procedure DoPolyline (points:array of TPoint; close:boolean); virtual; abstract;
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
    procedure Polygon (points:array of TPoint);
  public
    procedure DoRectangle (left,top, right,bottom:integer); virtual; abstract;
    procedure DoEllipse (left,top, right,bottom:integer); virtual; abstract;
    procedure DoFloodFill (x,y:integer); virtual; abstract;
    procedure DoPolygon (points:array of TPoint); virtual; abstract;
  end;

  TFPEmptyBrush = class (TFPCustomBrush)
  end;

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

{$i FPHelper.inc}
{$i FPFont.inc}
{$i FPPen.inc}
{$i FPBrush.inc}
{$i FPCanvas.inc}
{$i FPCDrawH.inc}

end.
