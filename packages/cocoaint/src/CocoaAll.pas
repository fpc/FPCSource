unit CocoaAll;

{$mode objfpc}
{$modeswitch objectivec1}
{$define NSGEOMETRY_TYPES_SAME_AS_CGGEOMETRY_TYPES}

interface

uses 
  ctypes, MacOSAll;
  
{$linkframework Cocoa}
{$define INTERFACE}

{$include UndefinedTypes.inc}

{$define HEADER}
{$include foundation/Foundation.inc}
{$include appkit/AppKit.inc}
{$undef HEADER}

{$define TYPES}
{$include foundation/Foundation.inc}
{$include appkit/AppKit.inc}
{$undef TYPES}

{$define RECORDS}
{$include foundation/Foundation.inc}
{$include appkit/AppKit.inc}
{$undef RECORDS}

type
{$define FORWARD}
{$include foundation/Foundation.inc}
{$include appkit/AppKit.inc}
{$undef FORWARD}

{$include UndefinedClasses.inc}

{$define CLASSES}
{$include foundation/Foundation.inc}
{$include appkit/AppKit.inc}
{$undef CLASSES}
 
{$define PROTOCOLS}
{$include foundation/Foundation.inc}
{$include appkit/AppKit.inc}
{$undef PROTOCOLS}

{$define FUNCTIONS}
{$include foundation/Foundation.inc}
{$include appkit/AppKit.inc}
{$undef FUNCTIONS}

{$define EXTERNAL_SYMBOLS}
{$include foundation/Foundation.inc}
{$include appkit/AppKit.inc}
{$undef EXTERNAL_SYMBOLS}

{$define USER_PATCHES}
{$include foundation/Foundation.inc}
{$include appkit/AppKit.inc}
{$undef USER_PATCHES}

{define IBOutlet := }
{define IBAction := }

{ Inline functions }
function NSSTR (inString: PChar): NSString;
function NSMakeRange (loc: NSUInteger; len: NSUInteger): NSRange;
function NSMaxRange (range: NSRange): NSUInteger;
function NSLocationInRange (loc: NSUInteger; range: NSRange): boolean;
function NSEqualRanges (range1, range2: NSRange): boolean;
function NSMakePoint (x: CGFloat; y: CGFloat): NSPoint;
function NSMakeSize(w: CGFloat; h: CGFloat): NSSize;
function NSMakeRect(x, y: CGFloat; w, h: CGFloat): NSRect;
function NSMaxX (aRect: NSRect): CGFloat;
function NSMaxY (aRect: NSRect): CGFloat;
function NSMidX (aRect: NSRect): CGFloat;
function NSMidY (aRect: NSRect): CGFloat;
function NSMinX (aRect: NSRect): CGFloat;
function NSMinY (aRect: NSRect): CGFloat;
function NSWidth (aRect: NSRect): CGFloat;
function NSHeight (aRect: NSRect): CGFloat;
function NSRectFromCGRect (aRect: CGRect): NSRect;
function NSRectToCGRect (aRect: NSRect): CGRect;
function NSPointFromCGPoint (aPoint: CGPoint): NSPoint;
function NSPointToCGPoint (aPoint: NSPoint): CGPoint;
function NSSizeFromCGSize(aSize: CGSize): NSSize;
function NSSizeToCGSize(aSize: NSSize): CGSize;

{$undef INTERFACE}

implementation
{$define IMPLEMENTATION}

{$include InlineFunctions.inc}

{$define USER_PATCHES}
{$include foundation/Foundation.inc}
{$include appkit/AppKit.inc}
{$undef USER_PATCHES}

{$undef IMPLEMENTATION}
end.