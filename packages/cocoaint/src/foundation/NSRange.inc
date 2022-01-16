{ Parsed from Foundation.framework NSRange.h }

{$ifdef TYPES}
type
  _NSRange = record
    location: NSUInteger;
    length: NSUInteger;
  end;
type
  NSRange = _NSRange;
type
  NSRangePtr = ^_NSRange;
type
  _NSRangePtr = ^_NSRange;

type
  NSRangePointer = ^NSRange;
{$endif}

{$ifdef FUNCTIONS}
function NSUnionRange(range1: NSRange; range2: NSRange): NSRange; cdecl; external;
function NSIntersectionRange(range1: NSRange; range2: NSRange): NSRange; cdecl; external;
function NSStringFromRange(range: NSRange): NSString; cdecl; external;
function NSRangeFromString(aString: NSString): NSRange; cdecl; external;
{$endif}

{$ifdef CLASSES}

type
  NSValueRangeExtensions = objccategory external (NSValue)
    class function valueWithRange (range: NSRange): NSValue; message 'valueWithRange:';
    function rangeValue: NSRange; message 'rangeValue';
  end;
{$endif}

