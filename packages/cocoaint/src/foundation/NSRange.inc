{ Parsed from Foundation.framework NSRange.h }

{$ifdef TYPES}
{$ifndef NSRANGE_PAS_T}
{$define NSRANGE_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSRANGE_PAS_R}
{$define NSRANGE_PAS_R}

{ Records }
type
  _NSRange = record
    location: NSUInteger;
    length: NSUInteger;
  end;
NSRange = _NSRange;
NSRangePtr = ^NSRange;
NSRangePointer = NSRangePtr;


{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSRANGE_PAS_F}
{$define NSRANGE_PAS_F}

{ Functions }
function NSUnionRange(range1: NSRange; range2: NSRange): NSRange; cdecl; external;
function NSIntersectionRange(range1: NSRange; range2: NSRange): NSRange; cdecl; external;
function NSStringFromRange(range: NSRange): NSString; cdecl; external;
function NSRangeFromString(aString: NSString): NSRange; cdecl; external;

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSRANGE_PAS_S}
{$define NSRANGE_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}

{$endif}

{$ifdef CLASSES}
{$ifndef NSRANGE_PAS_C}
{$define NSRANGE_PAS_C}

{ NSValueRangeExtensionsCategory }
  NSValueRangeExtensionsCategory = objccategory external (NSValue)
    class function valueWithRange(range: NSRange): NSValue; message 'valueWithRange:';
    function rangeValue: NSRange; message 'rangeValue';
  end;

{$endif}
{$endif}
