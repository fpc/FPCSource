{ Parsed from Foundation.framework NSGeometry.h }

{$if defined(__LP64__) or defined(TARGET_OS_EMBEDDED) or defined(TARGET_OS_IPHONE) or defined(TARGET_OS_WIN32) or defined(NS_BUILD_32_LIKE_64) or defined(NSGEOMETRY_TYPES_SAME_AS_CGGEOMETRY_TYPES)}
{$ifdef TYPES}
type
  NSPoint = CGPoint;
  NSPointPtr = ^NSPoint;
  NSPointPointer = ^NSPoint;
  NSSize = CGSize;
  NSSizePtr = ^NSSize;
  NSSizePointer = ^NSSize;
  NSRect = CGRect;
  NSRectPtr = ^NSRect;
  NSRectPointer = ^NSRect;

const
  NSMinXEdge = CGRectMinXEdge;
  NSMinYEdge = CGRectMinYEdge;
  NSMaxXEdge = CGRectMaxXEdge;
  NSMaxYEdge = CGRectMaxYEdge;

type
  NSRectEdge = NSUInteger;
  NSRectEdgePtr = ^NSRectEdge;

const
  NSGEOMETRY_TYPES_SAME_AS_CGGEOMETRY_TYPES = 1;
{$endif}

{$else}
{$ifdef TYPES}
type
  _NSPoint = record
    x: CGFloat;
    y: CGFloat;
  end;
type
  NSPoint = _NSPoint;
type
  NSPointPtr = ^_NSPoint;
type
  _NSPointPtr = ^_NSPoint;

type
  NSPointPointer = ^NSPoint;

type
  _NSSize = record
    width: CGFloat;
    height: CGFloat;
  end;
type
  NSSize = _NSSize;
type
  NSSizePtr = ^_NSSize;
type
  _NSSizePtr = ^_NSSize;

type
  NSSizePointer = ^NSSize;

type
  _NSRect = record
    origin: NSPoint;
    size: NSSize;
  end;
type
  NSRect = _NSRect;
type
  NSRectPtr = ^_NSRect;
type
  _NSRectPtr = ^_NSRect;

type
  NSRectPointer = ^NSRect;

const
  NSMinXEdge = 0;
  NSMinYEdge = 1;
  NSMaxXEdge = 2;
  NSMaxYEdge = 3;
type
  NSRectEdge = clong;
  NSRectEdgePtr = ^NSRectEdge;

{$endif}

{$endif}

{$ifdef TYPES}
type
	TNSPointArray = array[word] of NSPoint;
	NSPointArray = ^TNSPointArray;
	TNSSizeArray = array[word] of NSSize;
	NSSizeArray = ^TNSSizeArray;
	TNSRectArray = array[word] of NSRect;
	NSRectArray = ^TNSRectArray;
{$endif}

{$ifdef TYPES}
{$define NSEDGEINSETS_DEFINED}

type
  NSEdgeInsets = record
    top: CGFloat;
    left: CGFloat;
    bottom: CGFloat;
    right: CGFloat;
  end;
type
  NSEdgeInsetsPtr = ^NSEdgeInsets;

type
  NSAlignmentOptions = culonglong;
  NSAlignmentOptionsPtr = ^NSAlignmentOptions;

const
  NSAlignMinXInward = 1 shl 0;
  NSAlignMinYInward = 1 shl 1;
  NSAlignMaxXInward = 1 shl 2;
  NSAlignMaxYInward = 1 shl 3;
  NSAlignWidthInward = 1 shl 4;
  NSAlignHeightInward = 1 shl 5;
  NSAlignMinXOutward = 1 shl 8;
  NSAlignMinYOutward = 1 shl 9;
  NSAlignMaxXOutward = 1 shl 10;
  NSAlignMaxYOutward = 1 shl 11;
  NSAlignWidthOutward = 1 shl 12;
  NSAlignHeightOutward = 1 shl 13;
  NSAlignMinXNearest = 1 shl 16;
  NSAlignMinYNearest = 1 shl 17;
  NSAlignMaxXNearest = 1 shl 18;
  NSAlignMaxYNearest = 1 shl 19;
  NSAlignWidthNearest = 1 shl 20;
  NSAlignHeightNearest = 1 shl 21;
  NSAlignRectFlipped = 1 shl 63;
  NSAlignAllEdgesInward = NSAlignMinXInward + NSAlignMaxXInward + NSAlignMinYInward + NSAlignMaxYInward;
  NSAlignAllEdgesOutward = NSAlignMinXOutward + NSAlignMaxXOutward + NSAlignMinYOutward + NSAlignMaxYOutward;
  NSAlignAllEdgesNearest = NSAlignMinXNearest + NSAlignMaxXNearest + NSAlignMinYNearest + NSAlignMaxYNearest;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSZeroPoint: NSPoint; cvar; external;
  NSZeroSize: NSSize; cvar; external;
  NSZeroRect: NSRect; cvar; external;
  NSEdgeInsetsZero: NSEdgeInsets { available in 10_10, 8_0 }; cvar; external;
{$endif}

{$ifdef FUNCTIONS}
function NSEqualPoints(aPoint: NSPoint; bPoint: NSPoint): ObjCBOOL; cdecl; external;
function NSEqualSizes(aSize: NSSize; bSize: NSSize): ObjCBOOL; cdecl; external;
function NSEqualRects(aRect: NSRect; bRect: NSRect): ObjCBOOL; cdecl; external;
function NSIsEmptyRect(aRect: NSRect): ObjCBOOL; cdecl; external;
function NSEdgeInsetsEqual(aInsets: NSEdgeInsets; bInsets: NSEdgeInsets): ObjCBOOL; cdecl; external; { available in 10_10, 8_0 }
function NSInsetRect(aRect: NSRect; dX: CGFloat; dY: CGFloat): NSRect; cdecl; external;
function NSIntegralRect(aRect: NSRect): NSRect; cdecl; external;
function NSIntegralRectWithOptions(aRect: NSRect; opts: NSAlignmentOptions): NSRect; cdecl; external; { available in 10_7, 5_0 }
function NSUnionRect(aRect: NSRect; bRect: NSRect): NSRect; cdecl; external;
function NSIntersectionRect(aRect: NSRect; bRect: NSRect): NSRect; cdecl; external;
function NSOffsetRect(aRect: NSRect; dX: CGFloat; dY: CGFloat): NSRect; cdecl; external;
procedure NSDivideRect(inRect: NSRect; slice: NSRectPtr; rem: NSRectPtr; amount: CGFloat; edge: NSRectEdge); cdecl; external;
function NSPointInRect(aPoint: NSPoint; aRect: NSRect): ObjCBOOL; cdecl; external;
function NSMouseInRect(aPoint: NSPoint; aRect: NSRect; flipped: ObjCBOOL): ObjCBOOL; cdecl; external;
function NSContainsRect(aRect: NSRect; bRect: NSRect): ObjCBOOL; cdecl; external;
function NSIntersectsRect(aRect: NSRect; bRect: NSRect): ObjCBOOL; cdecl; external;
function NSStringFromPoint(aPoint: NSPoint): NSString; cdecl; external;
function NSStringFromSize(aSize: NSSize): NSString; cdecl; external;
function NSStringFromRect(aRect: NSRect): NSString; cdecl; external;
function NSPointFromString(aString: NSString): NSPoint; cdecl; external;
function NSSizeFromString(aString: NSString): NSSize; cdecl; external;
function NSRectFromString(aString: NSString): NSRect; cdecl; external;
{$endif}

{$ifdef CLASSES}

type
  NSValueGeometryExtensions = objccategory external (NSValue)
    class function valueWithPoint (point: NSPoint): NSValue; message 'valueWithPoint:';
    class function valueWithSize (size: NSSize): NSValue; message 'valueWithSize:';
    class function valueWithRect (rect: NSRect): NSValue; message 'valueWithRect:';
    class function valueWithEdgeInsets (insets: NSEdgeInsets): NSValue; message 'valueWithEdgeInsets:'; { available in 10_10, 8_0 }
    function pointValue: NSPoint; message 'pointValue';
    function sizeValue: NSSize; message 'sizeValue';
    function rectValue: NSRect; message 'rectValue';
    function edgeInsetsValue: NSEdgeInsets; message 'edgeInsetsValue';
  end;

type
  NSGeometryCoding = objccategory external (NSCoder)
    procedure encodePoint (point: NSPoint); message 'encodePoint:';
    function decodePoint: NSPoint; message 'decodePoint';
    procedure encodeSize (size: NSSize); message 'encodeSize:';
    function decodeSize: NSSize; message 'decodeSize';
    procedure encodeRect (rect: NSRect); message 'encodeRect:';
    function decodeRect: NSRect; message 'decodeRect';
  end;

type
  NSGeometryKeyedCoding = objccategory external (NSCoder)
    procedure encodePoint_forKey (point: NSPoint; key: NSString); message 'encodePoint:forKey:';
    procedure encodeSize_forKey (size: NSSize; key: NSString); message 'encodeSize:forKey:';
    procedure encodeRect_forKey (rect: NSRect; key: NSString); message 'encodeRect:forKey:';
    function decodePointForKey (key: NSString): NSPoint; message 'decodePointForKey:';
    function decodeSizeForKey (key: NSString): NSSize; message 'decodeSizeForKey:';
    function decodeRectForKey (key: NSString): NSRect; message 'decodeRectForKey:';
  end;
{$endif}

