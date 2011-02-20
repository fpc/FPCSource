{ Parsed from Foundation.framework NSGeometry.h }

{$ifdef TYPES}
{$ifndef NSGEOMETRY_PAS_T}
{$define NSGEOMETRY_PAS_T}

{$if defined(cpu64) or defined(cpuarm) or defined(win32)}
{ Types }
type
  NSPoint = CGPoint;
  NSPointPtr = ^NSPoint;
  NSPointPointer = ^NSPointPtr;
  NSPointArray = ^NSPoint;
  NSSize = CGSize;
  NSSizePtr = ^NSSize;
  NSSizePointer = NSSizePtr;
  NSSizeArray = ^NSSize;
  NSRect = CGRect;
  NSRectPtr = ^NSRect;
  NSRectPointer = NSRectPtr;
  NSRectArray = ^NSRect;

{ Defines }
{$define  NSGEOMETRY_TYPES_SAME_AS_CGGEOMETRY_TYPES}

{$endif cpu64 or cpuarm or win32}

{ Types }
type
{$ifdef NSGEOMETRY_TYPES_SAME_AS_CGGEOMETRY_TYPES}
  NSRectEdge = NSUInteger;
{$else}
  NSRectEdge = culong;
{$endif}
  NSRectEdgePtr = ^NSRectEdge;

{ Constants }
{$ifdef NSGEOMETRY_TYPES_SAME_AS_CGGEOMETRY_TYPES}
const
  NSMinXEdge = CGRectMinXEdge;
  NSMinYEdge = CGRectMinYEdge;
  NSMaxXEdge = CGRectMaxXEdge;
  NSMaxYEdge = CGRectMaxYEdge;

{$else NSGEOMETRY_TYPES_SAME_AS_CGGEOMETRY_TYPES}
const
  NSMinXEdge = 0;
  NSMinYEdge = 1;
  NSMaxXEdge = 2;
  NSMaxYEdge = 3;
{$endif NSGEOMETRY_TYPES_SAME_AS_CGGEOMETRY_TYPES}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSGEOMETRY_PAS_R}
{$define NSGEOMETRY_PAS_R}

{ Records }
{$ifndef NSGEOMETRY_TYPES_SAME_AS_CGGEOMETRY_TYPES}
type
  _NSPoint = record
    x: CGFloat;
    y: CGFloat;
  end;
  NSPoint = _NSPoint;
  NSPointPtr = ^NSPoint;
  NSPointPointer = NSPointPtr;
  NSPointArray = ^NSPoint;

type
  _NSSize = record
    width: CGFloat;
    height: CGFloat;
  end;
  NSSize = _NSSize;
  NSSizePtr = ^NSSize;
  NSSizePointer = NSSizePtr;
  NSSizeArray = ^NSSize;

type
  _NSRect = record
    origin: NSPoint;
    size: NSSize;
  end;
  NSRect = _NSRect;
  NSRectPtr = ^NSRect;
  NSRectPointer = NSRectPtr;
  NSRectArray = ^NSRect;
{$endif not NSGEOMETRY_TYPES_SAME_AS_CGGEOMETRY_TYPES}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSGEOMETRY_PAS_F}
{$define NSGEOMETRY_PAS_F}

{ Functions }
function NSEqualPoints(aPoint: NSPoint; bPoint: NSPoint): Boolean; cdecl; external;
function NSEqualSizes(aSize: NSSize; bSize: NSSize): Boolean; cdecl; external;
function NSEqualRects(aRect: NSRect; bRect: NSRect): Boolean; cdecl; external;
function NSIsEmptyRect(aRect: NSRect): Boolean; cdecl; external;
function NSInsetRect(aRect: NSRect; dX: CGFloat; dY: CGFloat): NSRect; cdecl; external;
function NSIntegralRect(aRect: NSRect): NSRect; cdecl; external;
function NSUnionRect(aRect: NSRect; bRect: NSRect): NSRect; cdecl; external;
function NSIntersectionRect(aRect: NSRect; bRect: NSRect): NSRect; cdecl; external;
function NSOffsetRect(aRect: NSRect; dX: CGFloat; dY: CGFloat): NSRect; cdecl; external;
procedure NSDivideRect(inRect: NSRect; slice: NSRectPtr; rem: NSRectPtr; amount: CGFloat; edge: NSRectEdge); cdecl; external;
function NSPointInRect(aPoint: NSPoint; aRect: NSRect): Boolean; cdecl; external;
function NSMouseInRect(aPoint: NSPoint; aRect: NSRect; flipped: Boolean): Boolean; cdecl; external;
function NSContainsRect(aRect: NSRect; bRect: NSRect): Boolean; cdecl; external;
function NSIntersectsRect(aRect: NSRect; bRect: NSRect): Boolean; cdecl; external;
function NSStringFromPoint(aPoint: NSPoint): NSString; cdecl; external;
function NSStringFromSize(aSize: NSSize): NSString; cdecl; external;
function NSStringFromRect(aRect: NSRect): NSString; cdecl; external;
function NSPointFromString(aString: NSString): NSPoint; cdecl; external;
function NSSizeFromString(aString: NSString): NSSize; cdecl; external;
function NSRectFromString(aString: NSString): NSRect; cdecl; external;

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSGEOMETRY_PAS_S}
{$define NSGEOMETRY_PAS_S}

{ External symbols }
var
  NSZeroPoint: NSPoint; cvar; external;
  NSZeroSize: NSSize; cvar; external;
  NSZeroRect: NSRect; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}

{$endif}

{$ifdef CLASSES}
{$ifndef NSGEOMETRY_PAS_C}
{$define NSGEOMETRY_PAS_C}

{ NSValueGeometryExtensionsCategory }
  NSValueGeometryExtensionsCategory = objccategory external (NSValue)
    class function valueWithPoint(point: NSPoint): NSValue; message 'valueWithPoint:';
    class function valueWithSize(size: NSSize): NSValue; message 'valueWithSize:';
    class function valueWithRect(rect: NSRect): NSValue; message 'valueWithRect:';
    function pointValue: NSPoint; message 'pointValue';
    function sizeValue: NSSize; message 'sizeValue';
    function rectValue: NSRect; message 'rectValue';
  end;

{ NSGeometryCodingCategory }
  NSGeometryCodingCategory = objccategory external (NSCoder)
    procedure encodePoint(point: NSPoint); message 'encodePoint:';
    function decodePoint: NSPoint; message 'decodePoint';
    procedure encodeSize(size: NSSize); message 'encodeSize:';
    function decodeSize: NSSize; message 'decodeSize';
    procedure encodeRect(rect: NSRect); message 'encodeRect:';
    function decodeRect: NSRect; message 'decodeRect';
  end;

{ NSGeometryKeyedCodingCategory }
  NSGeometryKeyedCodingCategory = objccategory external (NSCoder)
    procedure encodePoint_forKey(point: NSPoint; key: NSString); message 'encodePoint:forKey:';
    procedure encodeSize_forKey(size: NSSize; key: NSString); message 'encodeSize:forKey:';
    procedure encodeRect_forKey(rect: NSRect; key: NSString); message 'encodeRect:forKey:';
    function decodePointForKey(key: NSString): NSPoint; message 'decodePointForKey:';
    function decodeSizeForKey(key: NSString): NSSize; message 'decodeSizeForKey:';
    function decodeRectForKey(key: NSString): NSRect; message 'decodeRectForKey:';
  end;

{$endif}
{$endif}
