{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2002 by Florian Klaempfl,
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$MODE OBJFPC}
unit Types;

  interface
{$modeswitch advancedrecords}
{$modeswitch class}
{$if defined(win32) or defined(win64) or defined(wince)}
    uses
       Windows;
{$elseif defined(win16)}
    uses
       WinTypes;
{$endif}

{$if defined(win32) or defined(win64)}
const
  RT_RCDATA = Windows.RT_RCDATA deprecated 'Use Windows.RT_RCDATA instead';
{$elseif defined(win16)}
const
  RT_RCDATA = WinTypes.RT_RCDATA deprecated 'Use WinTypes.RT_RCDATA instead';
{$endif}

Const
  Epsilon: Single = 1E-40;
  Epsilon2: Single = 1E-30;


type
  TEndian =  Objpas.TEndian;
  TDirection = (FromBeginning, FromEnd);
  TValueRelationship = -1..1;
  
  DWORD = LongWord;

  PLongint = System.PLongint;
  PSmallInt = System.PSmallInt;
{$ifndef FPUNONE}
  PDouble = System.PDouble;
{$endif}
  PByte = System.PByte;
  Largeint = int64;
  LARGE_INT = LargeInt;
  PLargeInt = ^LargeInt;
  LargeUint = qword;
  LARGE_UINT= LargeUInt;
  PLargeuInt = ^LargeuInt;

  TBooleanDynArray = array of Boolean;
  TByteDynArray = array of Byte;
  TClassicByteDynArray = TByteDynArray;
  
  TCardinalDynArray = array of Cardinal;
  TInt64DynArray = array of Int64;
  TIntegerDynArray = array of Integer;
  TLongWordDynArray = array of LongWord;
  TPointerDynArray = array of Pointer;
  TQWordDynArray = array of QWord;
  TShortIntDynArray = array of ShortInt;
  TSmallIntDynArray = array of SmallInt;

  TRTLStringDynArray = array of Ansistring;
  TAnsiStringDynArray = Array of AnsiString;
  TWideStringDynArray   = array of WideString;
  TUnicodeStringDynArray = array of UnicodeString;
{$if SIZEOF(CHAR)=2}  
  TStringDynArray = Array of UnicodeString;
{$ELSE}
  TStringDynArray = Array of AnsiString;
{$ENDIF}

  TClassicStringDynArray = TStringDynArray;

  TObjectDynArray = array of TObject;
  TWordDynArray = array of Word;
  TCurrencyArray = Array of currency;
{$ifndef FPUNONE}
  TSingleDynArray = array of Single;
  TDoubleDynArray = array of Double;
  TExtendedDynArray = array of Extended;
  TCompDynArray = array of Comp;
{$endif}

{$if defined(win32) or defined(win64) or defined(wince)}
  TArray4IntegerType = Windows.TArray4IntegerType;
  TSmallPoint = Windows.TSmallPoint;
  PSmallPoint = Windows.PSmallPoint;

  TSize  = Windows.TSize;
  TagSize  = Windows.tagSize deprecated;
  PSize  = Windows.PSize;

  TPoint = Windows.TPoint;
  TagPoint = Windows.TagPoint deprecated;
  PPoint = Windows.PPoint;

  TRect  = Windows.TRect;
  PRect  = Windows.PRect;
  TSplitRectType = Windows.TSplitRectType;
const
  srLeft = TSplitRectType.srLeft;
  srRight = TSplitRectType.srRight;
  srTop = TSplitRectType.srTop;
  srBottom = TSplitRectType.srBottom;
type
{$else}
  {$i typshrdh.inc}
   TagSize = tSize deprecated;
   TagPoint = TPoint deprecated;
{$endif}

  { TPointF }
  PPointF = ^TPointF;
  TPointF =
{$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
  packed
{$endif FPC_REQUIRES_PROPER_ALIGNMENT}
  record
       x,y : Single;
       public
          function Add(const apt: TPoint): TPointF;
          function Add(const apt: TPointF): TPointF;
          function Distance(const apt : TPointF) : Single;
          function DotProduct(const apt : TPointF) : Single;
          function IsZero : Boolean;
          function Subtract(const apt : TPointF): TPointF;
          function Subtract(const apt : TPoint): TPointF;
          procedure SetLocation(const apt :TPointF);
          procedure SetLocation(const apt :TPoint);
          procedure SetLocation(ax,ay : Single);
          procedure Offset(const apt :TPointF);
          procedure Offset(const apt :TPoint);
          procedure Offset(dx,dy : Single);
          function EqualsTo(const apt: TPointF; const aEpsilon : Single): Boolean;

          function  Scale (afactor:Single)  : TPointF;
          function  Ceiling : TPoint;
          function  Truncate: TPoint;
          function  Floor   : TPoint;
          function  Round   : TPoint;
          function  Length  : Single;

          function Rotate(angle: single): TPointF;
          function Reflect(const normal: TPointF): TPointF;
          function MidPoint(const b: TPointF): TPointF;
          class function PointInCircle(const pt, center: TPointF; radius: single): Boolean; static;
          class function PointInCircle(const pt, center: TPointF; radius: integer): Boolean; static;
          class function Zero: TPointF; inline; static;
          function Angle(const b: TPointF): Single;
          function AngleCosine(const b: TPointF): single;
          function CrossProduct(const apt: TPointF): Single;
          function Normalize: TPointF;

          class function Create(const ax, ay: Single): TPointF; overload; static; inline;
          class function Create(const apt: TPoint): TPointF; overload; static; inline;
          class operator = (const apt1, apt2 : TPointF) : Boolean;
          class operator <> (const apt1, apt2 : TPointF): Boolean;
          class operator + (const apt1, apt2 : TPointF): TPointF;
          class operator - (const apt1, apt2 : TPointF): TPointF;
          class operator - (const apt1 : TPointF): TPointF;
          class operator * (const apt1, apt2: TPointF): TPointF;
          class operator * (const apt1: TPointF; afactor: single): TPointF;
          class operator * (afactor: single; const apt1: TPointF): TPointF;
          class operator / (const apt1: TPointF; afactor: single): TPointF;
          class operator := (const apt: TPoint): TPointF;
          class operator ** (const apt1, apt2: TPointF): Single; // scalar product
       end;

  { TSizeF }
  PSizeF = ^TSizeF;
  TSizeF =
{$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
  packed
{$endif FPC_REQUIRES_PROPER_ALIGNMENT}
  record
       cx,cy : Single;
       public
          function Add(const asz: TSize): TSizeF;
          function Add(const asz: TSizeF): TSizeF;
          function Distance(const asz : TSizeF) : Single;
          function IsZero : Boolean;
          function Subtract(const asz : TSizeF): TSizeF;
          function Subtract(const asz : TSize): TSizeF;
          function SwapDimensions:TSizeF;

          function  Scale (afactor:Single)  : TSizeF;
          function  Ceiling : TSize;
          function  Truncate: TSize;
          function  Floor   : TSize;
          function  Round   : TSize;
          function  Length  : Single;

          class function Create(const ax, ay: Single): TSizeF; overload; static; inline;
          class function Create(const asz: TSize): TSizeF; overload; static; inline;
          class operator = (const asz1, asz2 : TSizeF) : Boolean;
          class operator <> (const asz1, asz2 : TSizeF): Boolean;
          class operator + (const asz1, asz2 : TSizeF): TSizeF;
          class operator - (const asz1, asz2 : TSizeF): TSizeF;
          class operator - (const asz1 : TSizeF): TSizeF;
          class operator * (const asz1: TSizeF; afactor: single): TSizeF;
          class operator * (afactor: single; const asz1: TSizeF): TSizeF;
          class operator := (const apt: TPointF): TSizeF;
          class operator := (const asz: TSize): TSizeF;
          class operator := (const asz: TSizeF): TPointF;

          property Width: Single read cx write cx;
          property Height: Single read cy write cy;
       end;

  {$SCOPEDENUMS ON}
  TVertRectAlign = (Center, Top, Bottom);
  THorzRectAlign = (Center, Left, Right);
  {$SCOPEDENUMS OFF}

  { TRectF }
  PRectF = ^TRectF;
  TRectF =
{$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
  packed
{$endif FPC_REQUIRES_PROPER_ALIGNMENT}
  record
  private
    function  GetLocation: TPointF;
    function  GetSize: TSizeF;
    procedure SetSize(AValue: TSizeF);
    function GetHeight: Single; inline;
    function GetWidth: Single;  inline;
    procedure SetHeight(AValue: Single);
    procedure SetWidth (AValue: Single);
  public
    constructor Create(Origin: TPointF); // empty rect at given origin
    constructor Create(Origin: TPointF; AWidth, AHeight: Single);
    constructor Create(ALeft, ATop, ARight, ABottom: Single);
    constructor Create(P1, P2: TPointF; Normalize: Boolean = False);
    constructor Create(R: TRectF; Normalize: Boolean = False);
    constructor Create(R: TRect; Normalize: Boolean = False);

    class operator = (L, R: TRectF): Boolean;
    class operator <> (L, R: TRectF): Boolean;
    class operator + (L, R: TRectF): TRectF; // union
    class operator * (L, R: TRectF): TRectF; // intersection
    class operator := (const arc: TRect): TRectF;
    class function Empty: TRectF; static;

    class function Intersect(R1: TRectF; R2: TRectF): TRectF; static;
    class function Union(const Points: array of TPointF): TRectF; static;
    class function Union(R1, R2: TRectF): TRectF; static;
    Function Ceiling : TRectF;
    function CenterAt(const Dest: TRectF): TRectF;
    function CenterPoint: TPointF;
    function Contains(Pt: TPointF): Boolean;
    function Contains(R: TRectF): Boolean;
    function EqualsTo(const R: TRectF; const Epsilon: Single = 0): Boolean;
    function Fit(const Dest: TRectF): Single; deprecated 'Use FitInto';
    function FitInto(const Dest: TRectF): TRectF; overload;
    function FitInto(const Dest: TRectF; out Ratio: Single): TRectF; overload;
    function IntersectsWith(R: TRectF): Boolean;
    function IsEmpty: Boolean;
    function PlaceInto(const Dest: TRectF; const AHorzAlign: THorzRectAlign = THorzRectAlign.Center;  const AVertAlign: TVertRectAlign = TVertRectAlign.Center): TRectF;
    function Round: TRect;
    function SnapToPixel(AScale: Single; APlaceBetweenPixels: Boolean = True): TRectF;
    function Truncate: TRect;
    procedure Inflate(DL, DT, DR, DB: Single);
    procedure Inflate(DX, DY: Single);
    procedure Intersect(R: TRectF);
    procedure NormalizeRect;
    procedure Offset (const dx,dy : Single); inline;
    procedure Offset (DP: TPointF); inline;
    procedure SetLocation(P: TPointF);
    procedure SetLocation(X, Y: Single);
    procedure Union  (const r: TRectF); inline;
    property  Width  : Single read GetWidth write SetWidth;
    property  Height : Single read GetHeight write SetHeight;
    property  Size   : TSizeF read getSize   write SetSize;
    property  Location: TPointF read getLocation write setLocation;
    case Integer of
     0: (Left, Top, Right, Bottom: Single);
     1: (TopLeft, BottomRight: TPointF);
    end;

  TDuplicates = (dupIgnore, dupAccept, dupError);

  TPoint3D =
  {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
    packed
  {$endif FPC_REQUIRES_PROPER_ALIGNMENT}
    record

    public
     Type TSingle3Array = array[0..2] of single;
     constructor Create(const ax,ay,az:single);
     procedure   Offset(const adeltax,adeltay,adeltaz:single); inline;
     procedure   Offset(const adelta:TPoint3D); inline;
   public  
     case Integer of
      0: (data:TSingle3Array);
      1: (x,y,z : single);
    end;


type
  TOleChar = WideChar;
  POleStr = PWideChar;
  PPOleStr = ^POleStr;

  TListCallback = procedure(data,arg:pointer) of object;
  TListStaticCallback = procedure(data,arg:pointer);

const
  GUID_NULL: TGUID  = '{00000000-0000-0000-0000-000000000000}';

  STGTY_STORAGE   = 1;
  STGTY_STREAM    = 2;
  STGTY_LOCKBYTES = 3;
  STGTY_PROPERTY  = 4;

  STREAM_SEEK_SET = 0;
  STREAM_SEEK_CUR = 1;
  STREAM_SEEK_END = 2;

  LOCK_WRITE     = 1;
  LOCK_EXCLUSIVE = 2;
  LOCK_ONLYONCE  = 4;

  STATFLAG_DEFAULT   	      = 0;
  STATFLAG_NONAME    	      = 1;
  STATFLAG_NOOPEN    	      = 2;

{$ifndef Wince}
  // in Wince these are in unit windows. Under 32/64 in ActiveX.
  // for now duplicate them. Not that bad for untyped constants.

  E_FAIL 		      = HRESULT($80004005);
  E_INVALIDARG                = HRESULT($80070057);

  STG_E_INVALIDFUNCTION       = HRESULT($80030001);
  STG_E_FILENOTFOUND          = HRESULT($80030002);
  STG_E_PATHNOTFOUND          = HRESULT($80030003);
  STG_E_TOOMANYOPENFILES      = HRESULT($80030004);
  STG_E_ACCESSDENIED          = HRESULT($80030005);
  STG_E_INVALIDHANDLE         = HRESULT($80030006);
  STG_E_INSUFFICIENTMEMORY    = HRESULT($80030008);
  STG_E_INVALIDPOINTER        = HRESULT($80030009);
  STG_E_NOMOREFILES           = HRESULT($80030012);
  STG_E_DISKISWRITEPROTECTED  = HRESULT($80030013);
  STG_E_SEEKERROR             = HRESULT($80030019);
  STG_E_WRITEFAULT            = HRESULT($8003001D);
  STG_E_READFAULT             = HRESULT($8003001E);
  STG_E_SHAREVIOLATION        = HRESULT($80030020);
  STG_E_LOCKVIOLATION         = HRESULT($80030021);
  STG_E_FILEALREADYEXISTS     = HRESULT($80030050);
  STG_E_INVALIDPARAMETER      = HRESULT($80030057);
  STG_E_MEDIUMFULL            = HRESULT($80030070);
  STG_E_PROPSETMISMATCHED     = HRESULT($800300F0);
  STG_E_ABNORMALAPIEXIT       = HRESULT($800300FA);
  STG_E_INVALIDHEADER         = HRESULT($800300FB);
  STG_E_INVALIDNAME           = HRESULT($800300FC);
  STG_E_UNKNOWN               = HRESULT($800300FD);
  STG_E_UNIMPLEMENTEDFUNCTION = HRESULT($800300FE);
  STG_E_INVALIDFLAG           = HRESULT($800300FF);
  STG_E_INUSE                 = HRESULT($80030100);
  STG_E_NOTCURRENT            = HRESULT($80030101);
  STG_E_REVERTED              = HRESULT($80030102);
  STG_E_CANTSAVE              = HRESULT($80030103);
  STG_E_OLDFORMAT             = HRESULT($80030104);
  STG_E_OLDDLL                = HRESULT($80030105);
  STG_E_SHAREREQUIRED         = HRESULT($80030106);
  STG_E_EXTANTMARSHALLINGS    = HRESULT($80030108);
  STG_E_DOCFILECORRUPT        = HRESULT($80030109);
  STG_E_BADBASEADDRESS        = HRESULT($80030110);
  STG_E_INCOMPLETE            = HRESULT($80030201);
  STG_E_TERMINATED            = HRESULT($80030202);

  STG_S_CONVERTED             = $00030200;
  STG_S_BLOCK                 = $00030201;
  STG_S_RETRYNOW              = $00030202;
  STG_S_MONITORING            = $00030203;
{$endif}

{$if (not defined(win32)) and (not defined(win64)) and (not defined(wince))}
type
  PCLSID = PGUID;
  TCLSID = TGUID;

  PDWord = ^DWord;

  PDisplay = Pointer;
  PEvent = Pointer;

  TXrmOptionDescRec = record
  end;
  XrmOptionDescRec = TXrmOptionDescRec;
  PXrmOptionDescRec = ^TXrmOptionDescRec;

  Widget = Pointer;
  WidgetClass = Pointer;
  ArgList = Pointer;
  Region = Pointer;

  _FILETIME =
{$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
  packed
{$endif FPC_REQUIRES_PROPER_ALIGNMENT}
  record
     dwLowDateTime : DWORD;
     dwHighDateTime : DWORD;
  end;
  TFileTime = _FILETIME;
  FILETIME = _FILETIME;
  PFileTime = ^TFileTime;
{$else}
type
  PCLSID    = Windows.PCLSID;
  TCLSID    = Windows.CLSID;
  TFiletime = Windows.TFileTime;
  Filetime  = Windows.FileTime;
  PFiletime = Windows.PFileTime;
{$endif Windows}

type
  tagSTATSTG = record
     pwcsName      : POleStr;
     dwType        : DWord;
     cbSize        : Large_uint;
     mtime         : TFileTime;
     ctime         : TFileTime;
     atime         : TFileTime;
     grfMode       : DWord;
     grfLocksSupported : DWord;
     clsid         : TCLSID;
     grfStateBits  : DWord;
     reserved      : DWord;
  end;
  TStatStg = tagSTATSTG;
  STATSTG = TStatStg;
  PStatStg = ^TStatStg;

  { classes depends on these interfaces, we can't use the activex unit in classes though }
  IClassFactory = Interface(IUnknown) ['{00000001-0000-0000-C000-000000000046}']
     Function CreateInstance(Const unkOuter : IUnknown;Const riid : TGUID;Out vObject) : HResult;StdCall;
     Function LockServer(fLock : LongBool) : HResult;StdCall;
  End;

  ISequentialStream = interface(IUnknown)
     ['{0c733a30-2a1c-11ce-ade5-00aa0044773d}']
     function Read(pv : Pointer;cb : DWORD;pcbRead : PDWORD) : HRESULT;stdcall;
     function Write(pv : Pointer;cb : DWORD;pcbWritten : PDWORD): HRESULT;stdcall;
  end;

  IStream = interface(ISequentialStream) ['{0000000C-0000-0000-C000-000000000046}']
     function Seek(dlibMove : LargeInt; dwOrigin : DWORD; out libNewPosition : LargeUInt) : HResult;stdcall;
     function SetSize(libNewSize : LargeUInt) : HRESULT;stdcall;
     function CopyTo(stm: IStream;cb : LargeUInt;out cbRead : LargeUInt; out cbWritten : LargeUInt) : HRESULT;stdcall;
     function Commit(grfCommitFlags : DWORD) : HRESULT;stdcall;
     function Revert : HRESULT;stdcall;
     function LockRegion(libOffset : LargeUInt;cb : LargeUInt; dwLockType : DWORD) : HRESULT;stdcall;
     function UnlockRegion(libOffset : LargeUInt;cb : LargeUInt; dwLockType : DWORD) : HRESULT;stdcall;
     Function Stat(out statstg : TStatStg;grfStatFlag : DWORD) : HRESULT;stdcall;
     function Clone(out stm : IStream) : HRESULT;stdcall;
  end;

function EqualRect(const r1,r2 : TRect) : Boolean;
function EqualRect(const r1,r2 : TRectF) : Boolean;
function NormalizeRectF(const Pts: array of TPointF): TRectF; overload;
function NormalizeRect(const ARect: TRectF): TRectF; overload;
function Rect(Left,Top,Right,Bottom : Integer) : TRect; inline;
function RectF(Left,Top,Right,Bottom : Single) : TRectF; inline;
function Bounds(ALeft,ATop,AWidth,AHeight : Integer) : TRect; inline;
function Point(x,y : Integer) : TPoint; inline;
function PointF(x,y: Single) : TPointF; inline;
function PtInRect(const Rect : TRect; const p : TPoint) : Boolean;
function PtInRect(const Rect : TRectF; const p : TPointF) : Boolean;
function IntersectRect(const Rect1, Rect2: TRect): Boolean;
function IntersectRect(const Rect1, Rect2: TRectF): Boolean;
function IntersectRect(var Rect : TRect; const R1,R2 : TRect) : Boolean;
function IntersectRect(var Rect : TRectF; const R1,R2 : TRectF) : Boolean;
function RectCenter(var R: TRect; const Bounds: TRect): TRect;
function RectCenter(var R: TRectF; const Bounds: TRectF): TRectF;
function RectHeight(const Rect: TRect): Integer; inline; 
function RectHeight(const Rect: TRectF): Single; inline; 
function UnionRect(var Rect : TRect; const R1,R2 : TRect) : Boolean;
function UnionRect(var Rect : TRectF; const R1,R2 : TRectF) : Boolean;
function UnionRect(const R1,R2 : TRect) : TRect;
function UnionRect(const R1,R2 : TRectF) : TRectF;
function IsRectEmpty(const Rect : TRectF) : Boolean;
function IsRectEmpty(const Rect : TRect) : Boolean;
function OffsetRect(var Rect : TRect;DX : Integer;DY : Integer) : Boolean;
function OffsetRect(var Rect : TRectF;DX : Single;DY : Single) : Boolean;
procedure MultiplyRect(var R: TRectF; const DX, DY: Single);
function CenterPoint(const Rect: TRect): TPoint;
function InflateRect(var Rect: TRect; dx: Integer; dy: Integer): Boolean;
function InflateRect(var Rect: TRectF; dx: single; dy: Single): Boolean;
function Size(AWidth, AHeight: Integer): TSize; inline;
function Size(const ARect: TRect): TSize;
function ScalePoint(const P: TPointF; dX, dY: Single): TPointF; overload;
function ScalePoint(const P: TPoint; dX, dY: Single): TPoint; overload;
function MinPoint(const P1, P2: TPointF): TPointF; overload;
function MinPoint(const P1, P2: TPoint): TPoint; overload;
function SplitRect(const Rect: TRect; SplitType: TSplitRectType; Size: Integer): TRect; overload;
function SplitRect(const Rect: TRect; SplitType: TSplitRectType; Percent: Double): TRect; overload;
function CenteredRect(const SourceRect: TRect; const aCenteredRect: TRect): TRect;
function IntersectRectF(out Rect: TRectF; const R1, R2: TRectF): Boolean;
function UnionRectF(out Rect: TRectF; const R1, R2: TRectF): Boolean;

{$ifndef VER3_0}
type
  TBitConverter = class
    generic class procedure UnsafeFrom<T>(const ASrcValue: T; var ADestination: Array of Byte; AOffset: Integer = 0); static; {inline;}
    generic class procedure From<T>(const ASrcValue: T; var ADestination: Array of Byte; AOffset: Integer = 0); static;
    generic class function UnsafeInTo<T>(const ASource: Array of Byte; AOffset: Integer = 0): T; static; {inline;}
    generic class function InTo<T>(const ASource: Array of Byte; AOffset: Integer = 0): T; static;
  end;
{$endif}

implementation

Uses Math;

{$if (not defined(win32)) and (not defined(win64)) and (not defined(wince))}
  {$i typshrd.inc}
{$endif}

function SmallPoint(X, Y: Integer): TSmallPoint; inline; overload;
begin
  Result.X:=X;
  Result.Y:=Y;
end;

function SmallPoint(XY: LongWord): TSmallPoint; overload;

begin
  Result.X:=SmallInt(XY and $0000FFFF);
  Result.Y:=SmallInt(XY shr 16);
end;

function MinPoint(const P1, P2: TPointF): TPointF; overload;

begin
  Result:=P1;
  if (P2.Y<P1.Y) 
     or ((P2.Y=P1.Y) and (P2.X<P1.X)) then
    Result:=P2;
end;

function MinPoint(const P1, P2: TPoint): TPoint; overload;

begin
  Result:=P1;
  if (P2.Y<P1.Y) 
     or ((P2.Y=P1.Y) and (P2.X<P1.X)) then
    Result:=P2;
end;

function ScalePoint(const P: TPointF; dX, dY: Single): TPointF; overload;

begin
  Result.X:=P.X*dX;
  Result.Y:=P.Y*dY;
end;

function ScalePoint(const P: TPoint; dX, dY: Single): TPoint; overload;

begin
  Result.X:=Round(P.X*dX);
  Result.Y:=Round(P.Y*dY);
end;

function NormalizeRectF(const Pts: array of TPointF): TRectF; 

var
  Pt: TPointF;

begin
  Result.Left:=$FFFF;
  Result.Top:=$FFFF;
  Result.Right:=-$FFFF;
  Result.Bottom:=-$FFFF;
  for Pt in Pts do
    begin
    Result.Left:=Min(Pt.X,Result.left);
    Result.Top:=Min(Pt.Y,Result.Top);
    Result.Right:=Max(Pt.X,Result.Right);
    Result.Bottom:=Max(Pt.Y,Result.Bottom);
    end;  
end;

function NormalizeRect(const aRect : TRectF): TRectF; 

begin
  With aRect do
   Result:=NormalizeRectF([PointF(Left,Top),  
                           PointF(Right,Top),
                           PointF(Right,Bottom), 
                           PointF(Left,Bottom)]);  
end; 

function EqualRect(const r1,r2 : TRect) : Boolean;

begin
  EqualRect:=(r1.left=r2.left) and (r1.right=r2.right) and (r1.top=r2.top) and (r1.bottom=r2.bottom);
end;

function EqualRect(const r1,r2 : TRectF) : Boolean;

begin
  EqualRect:=r1.EqualsTo(r2);
end;

function Rect(Left,Top,Right,Bottom : Integer) : TRect; inline;

begin
  Rect.Left:=Left;
  Rect.Top:=Top;
  Rect.Right:=Right;
  Rect.Bottom:=Bottom;
end;

function RectF(Left,Top,Right,Bottom : Single) : TRectF; inline;

begin
  RectF.Left:=Left;
  RectF.Top:=Top;
  RectF.Right:=Right;
  RectF.Bottom:=Bottom;
end;

function Bounds(ALeft,ATop,AWidth,AHeight : Integer) : TRect; inline;

begin
  Bounds.Left:=ALeft;
  Bounds.Top:=ATop;
  Bounds.Right:=ALeft+AWidth;
  Bounds.Bottom:=ATop+AHeight;
end;

function Point(x,y : Integer) : TPoint; inline;

begin
  Point.x:=x;
  Point.y:=y;
end;

function PointF(x,y: Single) : TPointF; inline;

begin
  PointF.x:=x;
  PointF.y:=y;
end;

function PtInRect(const Rect : TRect;const p : TPoint) : Boolean;

begin
  PtInRect:=(p.y>=Rect.Top) and
            (p.y<Rect.Bottom) and
            (p.x>=Rect.Left) and
            (p.x<Rect.Right);
end;

function PtInRect(const Rect : TRectF;const p : TPointF) : Boolean;

begin
  PtInRect:=(p.y>=Rect.Top) and
            (p.y<Rect.Bottom) and
            (p.x>=Rect.Left) and
            (p.x<Rect.Right);
end;

function IntersectRectF(out Rect: TRectF; const R1, R2: TRectF): Boolean;
begin
  Result:=IntersectRect(Rect,R1,R2);
end;

function UnionRectF(out Rect: TRectF; const R1, R2: TRectF): Boolean;

begin
  Result:=UnionRect(Rect,R1,R2);
end;


function IntersectRect(const Rect1, Rect2: TRect): Boolean;
begin
  Result:=(Rect1.Left<Rect2.Right)
           and (Rect1.Right>Rect2.Left)
           and (Rect1.Top<Rect2.Bottom)
           and (Rect1.Bottom>Rect2.Top);
end;

function IntersectRect(var Rect : TRect;const R1,R2 : TRect) : Boolean;
var
  lRect: TRect;
begin
  lRect := R1;
  if R2.Left > R1.Left then
    lRect.Left := R2.Left;
  if R2.Top > R1.Top then
    lRect.Top := R2.Top;
  if R2.Right < R1.Right then
    lRect.Right := R2.Right;
  if R2.Bottom < R1.Bottom then
    lRect.Bottom := R2.Bottom;

  // The var parameter is only assigned in the end to avoid problems
  // when passing the same rectangle in the var and const parameters.
  // See http://bugs.freepascal.org/view.php?id=17722
  Result:=not IsRectEmpty(lRect);
  if Result then
    Rect := lRect
  else
    FillChar(Rect,SizeOf(Rect),0);
end;

function IntersectRect(const Rect1, Rect2: TRectF): Boolean;
begin
  Result:=(Rect1.Left<Rect2.Right)
           and (Rect1.Right>Rect2.Left)
           and (Rect1.Top<Rect2.Bottom)
           and (Rect1.Bottom>Rect2.Top);
end;


function IntersectRect(var Rect : TRectF;const R1,R2 : TRectF) : Boolean;
var
  lRect: TRectF;
begin
  lRect := R1;
  if R2.Left > R1.Left then
    lRect.Left := R2.Left;
  if R2.Top > R1.Top then
    lRect.Top := R2.Top;
  if R2.Right < R1.Right then
    lRect.Right := R2.Right;
  if R2.Bottom < R1.Bottom then
    lRect.Bottom := R2.Bottom;

  // The var parameter is only assigned in the end to avoid problems
  // when passing the same rectangle in the var and const parameters.
  // See http://bugs.freepascal.org/view.php?id=17722
  Result:=not IsRectEmpty(lRect);
  if Result then
    Rect := lRect
  else
    FillChar(Rect,SizeOf(Rect),0);
end;

function SplitRect(const Rect: TRect; SplitType: TSplitRectType; Size: Integer): TRect; overload;

begin
  Result:=Rect.SplitRect(SplitType,Size);
end;

function SplitRect(const Rect: TRect; SplitType: TSplitRectType; Percent: Double): TRect; overload;

begin
  Result:=Rect.SplitRect(SplitType,Percent);
end;

function CenteredRect(const SourceRect: TRect; const aCenteredRect: TRect): TRect;

var
  W,H: Integer;
  Center : TPoint;
begin
  W:=aCenteredRect.Width;
  H:=aCenteredRect.Height;
  Center:=SourceRect.CenterPoint;
  With Center do
    Result:= Rect(X-(W div 2),Y-(H div 2),X+((W+1) div 2),Y+((H+1) div 2));
end;

function RectWidth(const Rect: TRect): Integer;

begin
  Result:=Rect.Width;
end;

function RectWidth(const Rect: TRectF): Single;

begin
  Result:=Rect.Width;
end;

function RectHeight(const Rect: TRect): Integer; inline; 

begin
  Result:=Rect.Height;
end;

function RectHeight(const Rect: TRectF): Single; inline; 

begin
  Result:=Rect.Height
end;

function RectCenter(var R: TRect; const Bounds: TRect): TRect;

var
  C : TPoint;
  CS : TPoint;

begin
  C:=Bounds.CenterPoint;
  CS:=R.CenterPoint;
  OffsetRect(R,C.X-CS.X,C.Y-CS.Y);
  Result:=R;
end;

function RectCenter(var R: TRectF; const Bounds: TRectF): TRectF;

Var
  C,CS : TPointF;

begin
  C:=Bounds.CenterPoint;
  CS:=R.CenterPoint;
  OffsetRect(R,C.X-CS.X,C.Y-CS.Y);
  Result:=R;
end;

procedure MultiplyRect(var R: TRectF; const DX, DY: Single);

begin
  R.Left:=DX*R.Left;
  R.Right:=DX*R.Right;
  R.Top:=DY*R.Top;
  R.Bottom:=DY*R.Bottom;
end;

function UnionRect(const R1,R2 : TRect) : TRect;

begin
  Result:=Default(TRect);
  UnionRect(Result,R1,R2);
end;

function UnionRect(const R1,R2 : TRectF) : TRectF;

begin
  Result:=Default(TRectF);
  UnionRect(Result,R1,R2);
end;

function UnionRect(var Rect : TRect;const R1,R2 : TRect) : Boolean;
var
  lRect: TRect;
begin
  lRect:=R1;
  if R2.Left<R1.Left then
    lRect.Left:=R2.Left;
  if R2.Top<R1.Top then
    lRect.Top:=R2.Top;
  if R2.Right>R1.Right then
    lRect.Right:=R2.Right;
  if R2.Bottom>R1.Bottom then
    lRect.Bottom:=R2.Bottom;

  Result:=not IsRectEmpty(lRect);
  if Result then
    Rect := lRect
  else
    FillChar(Rect,SizeOf(Rect),0);
end;

function UnionRect(var Rect : TRectF;const R1,R2 : TRectF) : Boolean;
var
  lRect: TRectF;
begin
  lRect:=R1;
  if R2.Left<R1.Left then
    lRect.Left:=R2.Left;
  if R2.Top<R1.Top then
    lRect.Top:=R2.Top;
  if R2.Right>R1.Right then
    lRect.Right:=R2.Right;
  if R2.Bottom>R1.Bottom then
    lRect.Bottom:=R2.Bottom;

  Result:=not IsRectEmpty(lRect);
  if Result then
    Rect := lRect
  else
    FillChar(Rect,SizeOf(Rect),0);
end;

function IsRectEmpty(const Rect : TRect) : Boolean;
begin
  IsRectEmpty:=(Rect.Right<=Rect.Left) or (Rect.Bottom<=Rect.Top);
end;

function IsRectEmpty(const Rect : TRectF) : Boolean;
begin
  IsRectEmpty:=(Rect.Right<=Rect.Left) or (Rect.Bottom<=Rect.Top);
end;

function OffsetRect(var Rect : TRect;DX : Integer;DY : Integer) : Boolean;
begin
  Result:=assigned(@Rect);
  if Result then
    with Rect do
      begin
        inc(Left,dx);
        inc(Top,dy);
        inc(Right,dx);
        inc(Bottom,dy);
      end;
end;

function Avg(a, b: Longint): Longint;
begin
  if a < b then
    Result := a + ((b - a) shr 1)
  else
    Result := b + ((a - b) shr 1);
end;

function OffsetRect(var Rect: TRectF; DX: Single; DY: Single): Boolean;
begin
  Result:=assigned(@Rect);
  if Result then
    with Rect do
      begin
        Left:=Left+dx;
        Right:=Right+dx;
        Top:=Top+dy;
        Bottom:=Bottom+dy;
      end;
end;

function CenterPoint(const Rect: TRect): TPoint;
begin
  with Rect do
    begin
      Result.X := Avg(Left, Right);
      Result.Y := Avg(Top, Bottom);
    end;
end;

function InflateRect(var Rect: TRect; dx: Integer; dy: Integer): Boolean;
begin
  Result:=assigned(@Rect);
  if Result then
    with Rect do
      begin
        dec(Left, dx);
        dec(Top, dy);
        inc(Right, dx);
        inc(Bottom, dy);
      end;
end;

function InflateRect(var Rect: TRectF; dx: Single; dy: Single): Boolean;
begin
  Result:=assigned(@Rect);
  if Result then
    with Rect do
      begin
        Left:=Left-dx;
        Top:=Top-dy;
        Right:=Right+dx;
        Bottom:=Bottom+dy;
      end;
end;

function Size(AWidth, AHeight: Integer): TSize; inline;
begin
  Result.cx := AWidth;
  Result.cy := AHeight;
end;

function Size(const ARect: TRect): TSize; inline;
begin
  Result.cx := ARect.Right - ARect.Left;
  Result.cy := ARect.Bottom - ARect.Top;
end;


{ TPointF}

function TPointF.Add(const apt: TPoint): TPointF;
begin
  result.x:=x+apt.x;
  result.y:=y+apt.y;
end;

function TPointF.Add(const apt: TPointF): TPointF;
begin
  result.x:=x+apt.x;
  result.y:=y+apt.y;
end;

function TPointF.Subtract(const apt : TPointF): TPointF;
begin
  result.x:=x-apt.x;
  result.y:=y-apt.y;
end;

function TPointF.Subtract(const apt: TPoint): TPointF;
begin
  result.x:=x-apt.x;
  result.y:=y-apt.y;
end;

function TPointF.Distance(const apt : TPointF) : Single;
begin
  result:=sqrt(sqr(apt.x-x)+sqr(apt.y-y));
end;

function TPointF.DotProduct(const apt: TPointF): Single;
begin
  result:=x*apt.x+y*apt.y;
end;

function TPointF.IsZero : Boolean;
begin
  result:=SameValue(x,0.0) and SameValue(y,0.0);
end;

procedure TPointF.Offset(const apt :TPointF);
begin
  x:=x+apt.x;
  y:=y+apt.y;
end;

procedure TPointF.Offset(const apt: TPoint);
begin
  x:=x+apt.x;
  y:=y+apt.y;
end;

procedure TPointF.Offset(dx,dy : Single);
begin
  x:=x+dx;
  y:=y+dy;
end;

function TPointF.EqualsTo(const apt: TPointF; const aEpsilon: Single): Boolean;

  function Eq(a,b : single) : boolean; inline;

  begin
    result:=abs(a-b)<=aEpsilon;
  end;

begin
  Result:=Eq(X,apt.X) and Eq(Y,apt.Y);
end;

function TPointF.Scale(afactor: Single): TPointF;
begin
  result.x:=afactor*x;
  result.y:=afactor*y;
end;

function TPointF.Ceiling: TPoint;
begin
  result.x:=ceil(x);
  result.y:=ceil(y);
end;

function TPointF.Truncate: TPoint;
begin
  result.x:=trunc(x);
  result.y:=trunc(y);
end;

function TPointF.Floor: TPoint;
begin
  result.x:=Math.floor(x);
  result.y:=Math.floor(y);
end;

function TPointF.Round: TPoint;
begin
  result.x:=System.round(x);
  result.y:=System.round(y);
end;

function TPointF.Length: Single;
begin
  result:=sqrt(sqr(x)+sqr(y));
end;

function TPointF.Rotate(angle: single): TPointF;
var
  sina, cosa: single;
begin
  sincos(angle, sina, cosa);
  result.x := x * cosa - y * sina;
  result.y := x * sina + y * cosa;
end;

function TPointF.Reflect(const normal: TPointF): TPointF;
begin
  result := self + (-2 * normal ** self) * normal;
end;

function TPointF.MidPoint(const b: TPointF): TPointF;
begin
  result.x := 0.5 * (x + b.x);
  result.y := 0.5 * (y + b.y);
end;

class function TPointF.Zero: TPointF; 

begin
  Result.X:=0;
  Result.Y:=0;
end;

class function TPointF.PointInCircle(const pt, center: TPointF; radius: single): Boolean;
begin
  result := sqr(center.x - pt.x) + sqr(center.y - pt.y) < sqr(radius);
end;

class function TPointF.PointInCircle(const pt, center: TPointF; radius: integer): Boolean;
begin
  result := sqr(center.x - pt.x) + sqr(center.y - pt.y) < sqr(single(radius));
end;

function TPointF.Angle(const b: TPointF): Single;
begin
  result := ArcTan2(y - b.y, x - b.x);
end;

function TPointF.AngleCosine(const b: TPointF): single;
begin
  result := EnsureRange((self ** b) / sqrt((sqr(x) + sqr(y)) * (sqr(b.x) + sqr(b.y))), -1, 1);
end;

class operator TPointF.= (const apt1, apt2 : TPointF) : Boolean;
begin
  result:=SameValue(apt1.x,apt2.x) and SameValue(apt1.y,apt2.y);
end;

class operator TPointF.<> (const apt1, apt2 : TPointF): Boolean;
begin
  result:=NOT (SameValue(apt1.x,apt2.x) and Samevalue(apt1.y,apt2.y));
end;

class operator TPointF. * (const apt1, apt2: TPointF): TPointF;
begin
  result.x:=apt1.x*apt2.x;
  result.y:=apt1.y*apt2.y;
end;

class operator TPointF. * (afactor: single; const apt1: TPointF): TPointF;
begin
  result:=apt1.Scale(afactor);
end;

class operator TPointF. * (const apt1: TPointF; afactor: single): TPointF;
begin
  result:=apt1.Scale(afactor);
end;

class operator TPointF. ** (const apt1, apt2: TPointF): Single;
begin
  result:=apt1.x*apt2.x + apt1.y*apt2.y;
end;

class operator TPointF.+ (const apt1, apt2 : TPointF): TPointF;
begin
  result.x:=apt1.x+apt2.x;
  result.y:=apt1.y+apt2.y;
end;

class operator TPointF.- (const apt1, apt2 : TPointF): TPointF;
begin
  result.x:=apt1.x-apt2.x;
  result.y:=apt1.y-apt2.y;
end;

class operator TPointF. - (const apt1: TPointF): TPointF;
begin
  Result.x:=-apt1.x;
  Result.y:=-apt1.y;
end;

class operator TPointF. / (const apt1: TPointF; afactor: single): TPointF;
begin
  result:=apt1.Scale(1/afactor);
end;

class operator TPointF. := (const apt: TPoint): TPointF;
begin
  Result.x:=apt.x;
  Result.y:=apt.y;
end;

procedure TPointF.SetLocation(const apt :TPointF);
begin
 x:=apt.x; y:=apt.y;
end;

procedure TPointF.SetLocation(const apt: TPoint);
begin
  x:=apt.x; y:=apt.y;
end;

procedure TPointF.SetLocation(ax,ay : Single);
begin
  x:=ax; y:=ay;
end;

class function TPointF.Create(const ax, ay: Single): TPointF;
begin
  Result.x := ax;
  Result.y := ay;
end;

class function TPointF.Create(const apt: TPoint): TPointF;
begin
  Result.x := apt.X;
  Result.y := apt.Y;
end;


function TPointF.CrossProduct(const apt: TPointF): Single;
begin
  Result:=X*apt.Y-Y*apt.X;
end;

function TPointF.Normalize: TPointF;

var
  L: Single;
  
begin
  L:=Sqrt(Sqr(X)+Sqr(Y));
  if SameValue(L,0,Epsilon) then
    Result:=Self
  else
    begin
    Result.X:=X/L;
    Result.Y:=Y/L;
    end;
end;


{ TSizeF }

function TSizeF.Add(const asz: TSize): TSizeF;
begin
  result.cx:=cx+asz.cx;
  result.cy:=cy+asz.cy;
end;

function TSizeF.Add(const asz: TSizeF): TSizeF;
begin
  result.cx:=cx+asz.cx;
  result.cy:=cy+asz.cy;
end;

function TSizeF.Subtract(const asz : TSizeF): TSizeF;
begin
  result.cx:=cx-asz.cx;
  result.cy:=cy-asz.cy;
end;

function TSizeF.SwapDimensions:TSizeF;
begin
  result.cx:=cy;
  result.cy:=cx;
end;

function TSizeF.Subtract(const asz: TSize): TSizeF;
begin
  result.cx:=cx-asz.cx;
  result.cy:=cy-asz.cy;
end;

function TSizeF.Distance(const asz : TSizeF) : Single;
begin
  result:=sqrt(sqr(asz.cx-cx)+sqr(asz.cy-cy));
end;

function TSizeF.IsZero : Boolean;
begin
  result:=SameValue(cx,0.0) and SameValue(cy,0.0);
end;

function TSizeF.Scale(afactor: Single): TSizeF;
begin
  result.cx:=afactor*cx;
  result.cy:=afactor*cy;
end;

function TSizeF.Ceiling: TSize;
begin
  result.cx:=ceil(cx);
  result.cy:=ceil(cy);
end;

function TSizeF.Truncate: TSize;
begin
  result.cx:=trunc(cx);
  result.cy:=trunc(cy);
end;

function TSizeF.Floor: TSize;
begin
  result.cx:=Math.floor(cx);
  result.cy:=Math.floor(cy);
end;

function TSizeF.Round: TSize;
begin
  result.cx:=System.round(cx);
  result.cy:=System.round(cy);
end;

function TSizeF.Length: Single;
begin     //distance(self) ?
  result:=sqrt(sqr(cx)+sqr(cy));
end;

class operator TSizeF.= (const asz1, asz2 : TSizeF) : Boolean;
begin
  result:=SameValue(asz1.cx,asz2.cx) and SameValue(asz1.cy,asz2.cy);
end;

class operator TSizeF.<> (const asz1, asz2 : TSizeF): Boolean;
begin
  result:=NOT (SameValue(asz1.cx,asz2.cx) and Samevalue(asz1.cy,asz2.cy));
end;

class operator TSizeF. * (afactor: single; const asz1: TSizeF): TSizeF;
begin
  result:=asz1.Scale(afactor);
end;

class operator TSizeF. * (const asz1: TSizeF; afactor: single): TSizeF;
begin
  result:=asz1.Scale(afactor);
end;

class operator TSizeF.+ (const asz1, asz2 : TSizeF): TSizeF;
begin
  result.cx:=asz1.cx+asz2.cx;
  result.cy:=asz1.cy+asz2.cy;
end;

class operator TSizeF.- (const asz1, asz2 : TSizeF): TSizeF;
begin
  result.cx:=asz1.cx-asz2.cx;
  result.cy:=asz1.cy-asz2.cy;
end;

class operator TSizeF. - (const asz1: TSizeF): TSizeF;
begin
  Result.cx:=-asz1.cx;
  Result.cy:=-asz1.cy;
end;

class operator TSizeF. := (const apt: TPointF): TSizeF;
begin
  Result.cx:=apt.x;
  Result.cy:=apt.y;
end;

class operator TSizeF. := (const asz: TSize): TSizeF;
begin
  Result.cx := asz.cx;
  Result.cy := asz.cy;
end;

class operator TSizeF. := (const asz: TSizeF): TPointF;
begin
  Result.x := asz.cx;
  Result.y := asz.cy;
end;

class function TSizeF.Create(const ax, ay: Single): TSizeF;
begin
  Result.cx := ax;
  Result.cy := ay;
end;

class function TSizeF.Create(const asz: TSize): TSizeF;
begin
  Result.cx := asz.cX;
  Result.cy := asz.cY;
end;

{ TRectF }

class operator TRectF. * (L, R: TRectF): TRectF;
begin
  Result := TRectF.Intersect(L, R);
end;

class operator TRectF. + (L, R: TRectF): TRectF;
begin
  Result := TRectF.Union(L, R);
end;

class operator TRectF. := (const arc: TRect): TRectF;
begin
  Result.Left:=arc.Left;
  Result.Top:=arc.Top;
  Result.Right:=arc.Right;
  Result.Bottom:=arc.Bottom;
end;

class operator TRectF. <> (L, R: TRectF): Boolean;
begin
  Result := not(L=R);
end;

class operator TRectF. = (L, R: TRectF): Boolean;
begin
  Result :=
    SameValue(L.Left,R.Left) and SameValue(L.Right,R.Right) and
    SameValue(L.Top,R.Top) and SameValue(L.Bottom,R.Bottom);
end;

constructor TRectF.Create(ALeft, ATop, ARight, ABottom: Single);
begin
  Left := ALeft;
  Top := ATop;
  Right := ARight;
  Bottom := ABottom;
end;

constructor TRectF.Create(P1, P2: TPointF; Normalize: Boolean);
begin
  TopLeft := P1;
  BottomRight := P2;
  if Normalize then
    NormalizeRect;
end;

constructor TRectF.Create(Origin: TPointF);
begin
  TopLeft := Origin;
  BottomRight := Origin;
end;

constructor TRectF.Create(Origin: TPointF; AWidth, AHeight: Single);
begin
  TopLeft := Origin;
  Width := AWidth;
  Height := AHeight;
end;

constructor TRectF.Create(R: TRectF; Normalize: Boolean);
begin
  Self := R;
  if Normalize then
    NormalizeRect;
end;

constructor TRectF.Create(R: TRect; Normalize: Boolean);
begin
  Self := R;
  if Normalize then
    NormalizeRect;
end;

function TRectF.CenterPoint: TPointF;
begin
  Result.X := (Right-Left) / 2 + Left;
  Result.Y := (Bottom-Top) / 2 + Top;
end;

function TRectF.Ceiling: TRectF;
begin
  Result.BottomRight:=BottomRight.Ceiling;
  Result.TopLeft:=TopLeft.Ceiling;
end;

function TRectF.CenterAt(const Dest: TRectF): TRectF;
begin
  Result:=Self;
  RectCenter(Result,Dest);
end;

function TRectF.Fit(const Dest: TRectF): Single;

var
  R : TRectF;

begin
  R:=FitInto(Dest,Result);
  Self:=R;
end;

function TRectF.FitInto(const Dest: TRectF; out Ratio: Single): TRectF;
begin
  if (Dest.Width<=0) or (Dest.Height<=0) then
  begin
    Ratio:=1.0;
    exit(Self);
  end;
  Ratio:=Max(Self.Width / Dest.Width, Self.Height / Dest.Height);
  if Ratio=0 then
    exit(Self);
  Result.Width:=Self.Width / Ratio;
  Result.Height:=Self.Height / Ratio;
  Result.Left:=Self.Left + (Self.Width - Result.Width) / 2;
  Result.Top:=Self.Top + (Self.Height - Result.Height) / 2;
end;

function TRectF.FitInto(const Dest: TRectF): TRectF;
var
  Ratio: Single;
begin
  Result:=FitInto(Dest,Ratio);
end;

function TRectF.PlaceInto(const Dest: TRectF; const AHorzAlign: THorzRectAlign = THorzRectAlign.Center;  const AVertAlign: TVertRectAlign = TVertRectAlign.Center): TRectF;

var
  R : TRectF;
  X,Y : Single;
  D : TRectF absolute dest;

begin
  if (Height>Dest.Height) or (Width>Dest.Width) then
    R:=FitInto(Dest)
  else
    R:=Self;
  case AHorzAlign of
     THorzRectAlign.Left:
       X:=D.Left;
     THorzRectAlign.Center:
       X:=(D.Left+D.Right-R.Width)/2;
     THorzRectAlign.Right:
       X:=D.Right-R.Width;
  end;
  case AVertAlign of
    TVertRectAlign.Top:
      Y:=D.Top;
    TVertRectAlign.Center:
      Y:=(D.Top+D.Bottom-R.Height)/2;
    TVertRectAlign.Bottom:
      Y:=D.Bottom-R.Height;
  end;
  R.SetLocation(PointF(X,Y));
  Result:=R;
end;

function TRectF.SnapToPixel(AScale: Single; APlaceBetweenPixels: Boolean): TRectF;

  function sc (S : single) : single; inline;

  begin
    Result:=System.Trunc(S*AScale)/AScale;
  end;

var
  R : TRectF;
  Off: Single;

begin
  if AScale<=0 then
    AScale := 1;
  R.Top:=Sc(Top);
  R.Left:=Sc(Left);
  R.Width:=Sc(Width);
  R.Height:=Sc(Height);
  if APlaceBetweenPixels then
    begin
    Off:=1/(2*aScale);
    R.Offset(Off,Off);
    end;
  Result:=R;
end;


function TRectF.Contains(Pt: TPointF): Boolean;
begin
  Result := (Left <= Pt.X) and (Pt.X < Right) and (Top <= Pt.Y) and (Pt.Y < Bottom);
end;

function TRectF.Contains(R: TRectF): Boolean;
begin
  Result := (Left <= R.Left) and (R.Right <= Right) and (Top <= R.Top) and (R.Bottom <= Bottom);
end;

class function TRectF.Empty: TRectF;
begin
  Result := TRectF.Create(0,0,0,0);
end;

function TRectF.EqualsTo(const R: TRectF; const Epsilon: Single): Boolean;
begin
  Result:=TopLeft.EqualsTo(R.TopLeft,Epsilon);
  Result:=Result and BottomRight.EqualsTo(R.BottomRight,Epsilon);
end;

function TRectF.GetHeight: Single;
begin
  result:=bottom-top;
end;

function TRectF.GetLocation: TPointF;
begin
  result.x:=Left; result.y:=top;
end;

function TRectF.GetSize: TSizeF;
begin
  result.cx:=width; result.cy:=height;
end;

function TRectF.GetWidth: Single;
begin
  result:=right-left;
end;

procedure TRectF.Inflate(DX, DY: Single);
begin
  Left:=Left-dx;
  Top:=Top-dy;
  Right:=Right+dx;
  Bottom:=Bottom+dy;
end;

procedure TRectF.Intersect(R: TRectF);
begin
  Self := Intersect(Self, R);
end;

class function TRectF.Intersect(R1: TRectF; R2: TRectF): TRectF;
begin
  Result := R1;
  if R2.Left > R1.Left then
    Result.Left := R2.Left;
  if R2.Top > R1.Top then
    Result.Top := R2.Top;
  if R2.Right < R1.Right then
    Result.Right := R2.Right;
  if R2.Bottom < R1.Bottom then
    Result.Bottom := R2.Bottom;
end;

function TRectF.IntersectsWith(R: TRectF): Boolean;
begin
  Result := (Left < R.Right) and (R.Left < Right) and (Top < R.Bottom) and (R.Top < Bottom);
end;

function TRectF.IsEmpty: Boolean;
begin
  Result := (CompareValue(Right,Left)<=0) or (CompareValue(Bottom,Top)<=0);
end;

procedure TRectF.NormalizeRect;
var
  x: Single;
begin
  if Top>Bottom then
  begin
    x := Top;
    Top := Bottom;
    Bottom := x;
  end;
  if Left>Right then
  begin
    x := Left;
    Left := Right;
    Right := x;
  end
end;

procedure TRectF.Inflate(DL, DT, DR, DB: Single);
begin
  Left:=Left-dl;
  Top:=Top-dt;
  Right:=Right+dr;
  Bottom:=Bottom+db;
end;

procedure TRectF.Offset(const dx, dy: Single);
begin
  left:=left+dx; right:=right+dx;
  bottom:=bottom+dy; top:=top+dy;
end;

procedure TRectF.Offset(DP: TPointF);
begin
  left:=left+DP.x; right:=right+DP.x;
  bottom:=bottom+DP.y; top:=top+DP.y;
end;

function TRectF.Truncate: TRect;
begin
  Result.BottomRight:=BottomRight.Truncate;
  Result.TopLeft:=TopLeft.Truncate;
end;

function TRectF.Round: TRect;
begin
  Result.BottomRight:=BottomRight.Round;
  Result.TopLeft:=TopLeft.Round;
end;

procedure TRectF.SetHeight(AValue: Single);
begin
  bottom:=top+avalue;
end;

procedure TRectF.SetLocation(X, Y: Single);
begin
  Offset(X-Left, Y-Top);
end;

procedure TRectF.SetLocation(P: TPointF);
begin
  SetLocation(P.X, P.Y);
end;

procedure TRectF.SetSize(AValue: TSizeF);
begin
  bottom:=top+avalue.cy;
  right:=left+avalue.cx;
end;

procedure TRectF.SetWidth(AValue: Single);
begin
  right:=left+avalue;
end;

class function TRectF.Union(const Points: array of TPointF): TRectF;
var
  i: Integer;
begin
  if Length(Points) > 0 then
  begin
    Result.TopLeft := Points[Low(Points)];
    Result.BottomRight := Points[Low(Points)];

    for i := Low(Points)+1 to High(Points) do
    begin
      if Points[i].X < Result.Left then Result.Left := Points[i].X;
      if Points[i].X > Result.Right then Result.Right := Points[i].X;
      if Points[i].Y < Result.Top then Result.Top := Points[i].Y;
      if Points[i].Y > Result.Bottom then Result.Bottom := Points[i].Y;
    end;
  end else
    Result := Empty;
end;

procedure TRectF.Union(const r: TRectF);
begin
  left:=min(r.left,left);
  top:=min(r.top,top);
  right:=max(r.right,right);
  bottom:=max(r.bottom,bottom);
end;

class function TRectF.Union(R1, R2: TRectF): TRectF;
begin
  Result:=R1;
  Result.Union(R2);
end;

{ TPoint3D }

constructor TPoint3D.Create(const ax,ay,az:single);
begin
  x:=ax; y:=ay; z:=az;
end;

procedure   TPoint3D.Offset(const adeltax,adeltay,adeltaz:single);
begin
  x:=x+adeltax; y:=y+adeltay; z:=z+adeltaz;
end;

procedure   TPoint3D.Offset(const adelta:TPoint3D);
begin
  x:=x+adelta.x; y:=y+adelta.y; z:=z+adelta.z;
end;

{$ifndef VER3_0}
generic class procedure TBitConverter.UnsafeFrom<T>(const ASrcValue: T; var ADestination: Array of Byte; AOffset: Integer = 0);
begin
  move(ASrcValue, ADestination[AOffset], SizeOf(T));
end;

generic class procedure TBitConverter.From<T>(const ASrcValue: T; var ADestination: Array of Byte; AOffset: Integer = 0);
begin
  if AOffset < 0 then
    System.Error(reRangeError);

  if IsManagedType(T) then
    System.Error(reInvalidCast);

  if Length(ADestination) < (SizeOf(T) + AOffset) then
    System.Error(reRangeError);

  TBitConverter.specialize UnsafeFrom<T>(ASrcValue, ADestination, AOffset);
end;

generic class function TBitConverter.UnsafeInTo<T>(const ASource: Array of Byte; AOffset: Integer = 0): T;
begin
  move(ASource[AOffset], Result, SizeOf(T));
end;

generic class function TBitConverter.InTo<T>(const ASource: Array of Byte; AOffset: Integer = 0): T;
begin
  if AOffset < 0 then
    System.Error(reRangeError);

  if IsManagedType(T) then
    System.Error(reInvalidCast);

  if Length(ASource) < (SizeOf(T) + AOffset) then
    System.Error(reRangeError);

  Result := TBitConverter.specialize UnsafeInTo<T>(ASource, AOffset);
end;
{$endif}

end.
