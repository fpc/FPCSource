{ Parsed from Foundation.framework NSGeometry.h }
{ Version 1.0 beta - Wed Mar 18 15:38:09 ICT 2009 }


{$ifdef TYPES}
{$ifndef NSGEOMETRY_PAS_T}
{$define NSGEOMETRY_PAS_T}

{ Sets }

type
  NSRectEdge = (NSMinXEdge = 0, NSMinYEdge = 1, NSMaxXEdge = 2, NSMaxYEdge = 3);

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSGEOMETRY_PAS_R}
{$define NSGEOMETRY_PAS_R}

{ Records }
type
  _NSPoint = packed record
    x: CGFloat;
    y: CGFloat;
  end;
  
{$ifdef NSGEOMETRY_TYPES_SAME_AS_CGGEOMETRY_TYPES}
NSPoint = CGPoint;
{$else}
NSPoint = _NSPoint;
{$endif}

NSPointPointer = ^NSPoint;
TNSPointArray = array[word] of NSPoint;
PNSPointArray = ^TNSPointArray;
NSPointArray = ^NSPoint;

type
  _NSSize = packed record
    width: CGFloat;
    height: CGFloat;
  end;
  
{$ifdef NSGEOMETRY_TYPES_SAME_AS_CGGEOMETRY_TYPES}
NSSize = CGSize;
{$else}
NSSize = _NSSize;
{$endif}

NSSizePointer = ^NSSize;
TNSSizeArray = array[word] of NSSize;
PNSSizeArray = ^TNSSizeArray;
NSSizeArray = ^NSSize;

type
  _NSRect = packed record
    origin: NSPoint;
    size: NSSize;
  end;
  
{$ifdef NSGEOMETRY_TYPES_SAME_AS_CGGEOMETRY_TYPES}
NSRect = CGRect;
{$else}
NSRect = _NSRect;
{$endif}

NSRectPointer = ^NSRect;
TNSRectArray = array[word] of NSRect;
PNSRectArray = ^TNSRectArray;
NSRectArray = PNSRectArray;


{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSGEOMETRY_PAS_F}
{$define NSGEOMETRY_PAS_F}

{$endif}
{$endif}
