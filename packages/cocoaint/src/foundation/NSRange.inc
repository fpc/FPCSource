{ Parsed from Foundation.framework NSRange.h }
{ Version 1.0 beta - Wed Mar 18 15:38:12 ICT 2009 }


{$ifdef TYPES}
{$ifndef NSRANGE_PAS_T}
{$define NSRANGE_PAS_T}

{ Records }
type
  _NSRange = packed record
    location: UInt32;
    length: UInt32;
  end;
  
{$ifdef NSGEOMETRY_TYPES_SAME_AS_CGGEOMETRY_TYPES}
NSRange = CFRange;
{$else}
NSRange = _NSRange;
{$endif}

NSRangePointer = ^NSRange;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSRANGE_PAS_R}
{$define NSRANGE_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSRANGE_PAS_F}
{$define NSRANGE_PAS_F}

function NSUnionRange(range1: NSRange; range2: NSRange): NSRange; cdecl; external name 'NSUnionRange';
function NSIntersectionRange(range1: NSRange; range2: NSRange): NSRange; cdecl; external name 'NSIntersectionRange';
function NSRangeFromString(var aString: CFStringRef): NSRange; cdecl; external name 'NSRangeFromString';

{$endif}
{$endif}
