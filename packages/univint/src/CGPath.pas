{ CoreGraphics - CGPath.h
 * Copyright (c) 2001-2002 Apple Computer, Inc.
 * All rights reserved.
 }
{       Pascal Translation:  Peter N Lewis, <peter@stairways.com.au>, August 2005 }
{
    Modified for use with Free Pascal
    Version 210
    Please report any bugs to <gpc@microbizz.nl>
}

{$mode macpas}
{$packenum 1}
{$macro on}
{$inline on}
{$calling mwpascal}

unit CGPath;
interface
{$setc UNIVERSAL_INTERFACES_VERSION := $0342}
{$setc GAP_INTERFACES_VERSION := $0210}

{$ifc not defined USE_CFSTR_CONSTANT_MACROS}
    {$setc USE_CFSTR_CONSTANT_MACROS := TRUE}
{$endc}

{$ifc defined CPUPOWERPC and defined CPUI386}
	{$error Conflicting initial definitions for CPUPOWERPC and CPUI386}
{$endc}
{$ifc defined FPC_BIG_ENDIAN and defined FPC_LITTLE_ENDIAN}
	{$error Conflicting initial definitions for FPC_BIG_ENDIAN and FPC_LITTLE_ENDIAN}
{$endc}

{$ifc not defined __ppc__ and defined CPUPOWERPC}
	{$setc __ppc__ := 1}
{$elsec}
	{$setc __ppc__ := 0}
{$endc}
{$ifc not defined __i386__ and defined CPUI386}
	{$setc __i386__ := 1}
{$elsec}
	{$setc __i386__ := 0}
{$endc}

{$ifc defined __ppc__ and __ppc__ and defined __i386__ and __i386__}
	{$error Conflicting definitions for __ppc__ and __i386__}
{$endc}

{$ifc defined __ppc__ and __ppc__}
	{$setc TARGET_CPU_PPC := TRUE}
	{$setc TARGET_CPU_X86 := FALSE}
{$elifc defined __i386__ and __i386__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_X86 := TRUE}
{$elsec}
	{$error Neither __ppc__ nor __i386__ is defined.}
{$endc}
{$setc TARGET_CPU_PPC_64 := FALSE}

{$ifc defined FPC_BIG_ENDIAN}
	{$setc TARGET_RT_BIG_ENDIAN := TRUE}
	{$setc TARGET_RT_LITTLE_ENDIAN := FALSE}
{$elifc defined FPC_LITTLE_ENDIAN}
	{$setc TARGET_RT_BIG_ENDIAN := FALSE}
	{$setc TARGET_RT_LITTLE_ENDIAN := TRUE}
{$elsec}
	{$error Neither FPC_BIG_ENDIAN nor FPC_LITTLE_ENDIAN are defined.}
{$endc}
{$setc ACCESSOR_CALLS_ARE_FUNCTIONS := TRUE}
{$setc CALL_NOT_IN_CARBON := FALSE}
{$setc OLDROUTINENAMES := FALSE}
{$setc OPAQUE_TOOLBOX_STRUCTS := TRUE}
{$setc OPAQUE_UPP_TYPES := TRUE}
{$setc OTCARBONAPPLICATION := TRUE}
{$setc OTKERNEL := FALSE}
{$setc PM_USE_SESSION_APIS := TRUE}
{$setc TARGET_API_MAC_CARBON := TRUE}
{$setc TARGET_API_MAC_OS8 := FALSE}
{$setc TARGET_API_MAC_OSX := TRUE}
{$setc TARGET_CARBON := TRUE}
{$setc TARGET_CPU_68K := FALSE}
{$setc TARGET_CPU_MIPS := FALSE}
{$setc TARGET_CPU_SPARC := FALSE}
{$setc TARGET_OS_MAC := TRUE}
{$setc TARGET_OS_UNIX := FALSE}
{$setc TARGET_OS_WIN32 := FALSE}
{$setc TARGET_RT_MAC_68881 := FALSE}
{$setc TARGET_RT_MAC_CFM := FALSE}
{$setc TARGET_RT_MAC_MACHO := TRUE}
{$setc TYPED_FUNCTION_POINTERS := TRUE}
{$setc TYPE_BOOL := FALSE}
{$setc TYPE_EXTENDED := FALSE}
{$setc TYPE_LONGLONG := TRUE}
uses MacTypes,CGBase,CGAffineTransforms,CFBase,CGGeometry;
{$ALIGN POWER}


type
	CGMutablePathRef = ^SInt32; { an opaque 32-bit type }
type
	CGPathRef = ^SInt32; { an opaque 32-bit type }


{ Return the CFTypeID for CGPathRefs. }

function CGPathGetTypeID: CFTypeID; external name '_CGPathGetTypeID'; (* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)

{ Create a mutable path. }

function CGPathCreateMutable: CGMutablePathRef; external name '_CGPathCreateMutable'; (* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)

{ Create a copy of `path'. }

function CGPathCreateCopy( path: CGPathRef ): CGPathRef; external name '_CGPathCreateCopy'; (* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)

{ Create a mutable copy of `path'. }

function CGPathCreateMutableCopy( path: CGPathRef ): CGMutablePathRef; external name '_CGPathCreateMutableCopy'; (* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)

{ Equivalent to `CFRetain(path)', except it doesn't crash (as CFRetain
 * does) if `path' is NULL. }

function CGPathRetain( path: CGPathRef ): CGPathRef; external name '_CGPathRetain'; (* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)

{ Equivalent to `CFRelease(path)', except it doesn't crash (as CFRelease
 * does) if `path' is NULL. }

procedure CGPathRelease( path: CGPathRef ); external name '_CGPathRelease'; (* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)

{ Return true if `path1' is equal to `path2'; false otherwise. }

function CGPathEqualToPath( path1: CGPathRef; path2: CGPathRef ): CBool; external name '_CGPathEqualToPath'; (* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)

{** Path construction functions. **}

{ Move the current point to `(x, y)' in `path' and begin a new subpath.
 * If `m' is non-NULL, then transform `(x, y)' by `m' first. }

procedure CGPathMoveToPoint( path: CGMutablePathRef; const (*var*) m: CGAffineTransform; x: Float32; y: Float32 ); external name '_CGPathMoveToPoint'; (* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)

{ Append a straight line segment from the current point to `(x, y)' in
 * `path' and move the current point to `(x, y)'.  If `m' is non-NULL, then
 * transform `(x, y)' by `m' first. }

procedure CGPathAddLineToPoint( path: CGMutablePathRef; const (*var*) m: CGAffineTransform; x: Float32; y: Float32 ); external name '_CGPathAddLineToPoint'; (* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)

{ Append a quadratic curve from the current point to `(x, y)' with control
 * point `(cpx, cpy)' in `path' and move the current point to `(x, y)'.  If
 * `m' is non-NULL, then transform all points by `m' first. }

procedure CGPathAddQuadCurveToPoint( path: CGMutablePathRef; const (*var*) m: CGAffineTransform; cpx: Float32; cpy: Float32; x: Float32; y: Float32 ); external name '_CGPathAddQuadCurveToPoint'; (* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)

{ Append a cubic Bezier curve from the current point to `(x,y)' with
 * control points `(cp1x, cp1y)' and `(cp2x, cp2y)' in `path' and move the
 * current point to `(x, y)'. If `m' is non-NULL, then transform all points
 * by `m' first. }

procedure CGPathAddCurveToPoint( path: CGMutablePathRef; const (*var*) m: CGAffineTransform; cp1x: Float32; cp1y: Float32; cp2x: Float32; cp2y: Float32; x: Float32; y: Float32 ); external name '_CGPathAddCurveToPoint'; (* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)

{ Append a line from the current point to the starting point of the
 * current subpath of `path' and end the subpath. }

procedure CGPathCloseSubpath( path: CGMutablePathRef ); external name '_CGPathCloseSubpath'; (* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)

{** Path construction convenience functions. **}

{ Add `rect' to `path'. If `m' is non-NULL, then first transform `rect' by
 * `m' before adding it to `path'. }

procedure CGPathAddRect( path: CGMutablePathRef; const (*var*) m: CGAffineTransform; rect: CGRect ); external name '_CGPathAddRect'; (* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)

{ Add each rectangle specified by `rects', an array of `count' CGRects, to
 * `path'. If `m' is non-NULL, then first transform each rectangle by `m'
 * before adding it to `path'. }

procedure CGPathAddRects( path: CGMutablePathRef; const (*var*) m: CGAffineTransform; {const} rects: {variable-size-array} CGRectPtr; count: size_t ); external name '_CGPathAddRects'; (* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)

{ Move to the first element of `points', an array of `count' CGPoints, and
 * append a line from each point to the next point in `points'.  If `m' is
 * non-NULL, then first transform each point by `m'. }

procedure CGPathAddLines( path: CGMutablePathRef; const (*var*) m: CGAffineTransform; {const} points: {variable-size-array} CGPointPtr; count: size_t ); external name '_CGPathAddLines'; (* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)

{ Add an ellipse (an oval) inside `rect' to `path'.  The ellipse is
 * approximated by a sequence of Bezier curves.  The center of the ellipse
 * is the midpoint of `rect'.  If `rect' is square, then the ellipse will
 * be circular with radius equal to one-half the width (equivalently,
 * one-half the height) of `rect'.  If `rect' is rectangular, then the
 * major- and minor-axes will be the `width' and `height' of rect.  The
 * ellipse forms a complete subpath of `path' --- that is, it begins with a
 * "move to" and ends with a "close subpath" --- oriented in the clockwise
 * direction.  If `m' is non-NULL, then the constructed Bezier curves
 * representing the ellipse will be transformed by `m' before they are
 * added to `path'. }

procedure CGPathAddEllipseInRect( path: CGMutablePathRef; const (*var*) m: CGAffineTransform; rect: CGRect ); external name '_CGPathAddEllipseInRect'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Add an arc of a circle to `path', possibly preceded by a straight line
 * segment. The arc is approximated by a sequence of cubic Bezier
 * curves. `(x, y)' is the center of the arc; `radius' is its radius;
 * `startAngle' is the angle to the first endpoint of the arc; `endAngle'
 * is the angle to the second endpoint of the arc; and `clockwise' is true
 * if the arc is to be drawn clockwise, false otherwise.  `startAngle' and
 * `endAngle' are measured in radians.  If `m' is non-NULL, then the
 * constructed Bezier curves representing the arc will be transformed by
 * `m' before they are added to `path'. }

procedure CGPathAddArc( path: CGMutablePathRef; const (*var*) m: CGAffineTransform; x: Float32; y: Float32; radius: Float32; startAngle: Float32; endAngle: Float32; clockwise: CBool ); external name '_CGPathAddArc'; (* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)

{ Add an arc of a circle to `path', possibly preceded by a straight line
 * segment.  The arc is approximated by a sequence of cubic Bezier curves.
 * `radius' is the radius of the arc.  The resulting arc is tangent to the
 * line from the current point of `path' to `(x1, y1)', and the line from
 * `(x1, y1)' to `(x2, y2)'.  If `m' is non-NULL, then the constructed
 * Bezier curves representing the arc will be transformed by `m' before
 * they are added to `path'. }

procedure CGPathAddArcToPoint( path: CGMutablePathRef; const (*var*) m: CGAffineTransform; x1: Float32; y1: Float32; x2: Float32; y2: Float32; radius: Float32 ); external name '_CGPathAddArcToPoint'; (* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)

{ Add `path2' to `path1'. If `m' is non-NULL, then the points in `path2'
* will be transformed by `m' before they are added to `path1'.}

procedure CGPathAddPath( path1: CGMutablePathRef; const (*var*) m: CGAffineTransform; path2: CGPathRef ); external name '_CGPathAddPath'; (* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)

{** Path information functions. **}

{ Return true if `path' contains no elements, false otherwise. }

function CGPathIsEmpty( path: CGPathRef ): CBool; external name '_CGPathIsEmpty'; (* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)

{ Return true if `path' represents a rectangle, false otherwise. }

function CGPathIsRect( path: CGPathRef; var rect: CGRect ): CBool; external name '_CGPathIsRect'; (* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)

{ Return the current point of the current subpath of `path'.  If there is
 * no current point, then return CGPointZero. }

function CGPathGetCurrentPoint( path: CGPathRef ): CGPoint; external name '_CGPathGetCurrentPoint'; (* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)

{ Return the bounding box of `path'.  The bounding box is the smallest
 * rectangle completely enclosing all points in the path, including control
 * points for Bezier and quadratic curves. If the path is empty, then
 * return CGRectNull. }

function CGPathGetBoundingBox( path: CGPathRef ): CGRect; external name '_CGPathGetBoundingBox'; (* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)

{ Return true if `point' is contained in `path'; false otherwise.  A point
 * is contained in a path if it is inside the painted region when the path
 * is filled; if `eoFill' is true, then the even-odd fill rule is used to
 * evaluate the painted region of the path, otherwise, the winding-number
 * fill rule is used. If `m' is non-NULL, then the point is transformed by
 * `m' before determining whether the path contains it. }

function CGPathContainsPoint( path: CGPathRef; const (*var*) m: CGAffineTransform; point: CGPoint; eoFill: CBool ): CBool; external name '_CGPathContainsPoint'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

type
	CGPathElementType = SInt32;
const
	kCGPathElementMoveToPoint = 0;
	kCGPathElementAddLineToPoint = 1;
	kCGPathElementAddQuadCurveToPoint = 2;
	kCGPathElementAddCurveToPoint = 3;
	kCGPathElementCloseSubpath = 4;

type
	CGPathElement = record
		typ: CGPathElementType;
		points: CGPointPtr;
	end;

type
	CGPathApplierFunction = procedure( info: UnivPtr; const (*var*) element: CGPathElement );

procedure CGPathApply( path: CGPathRef; info: UnivPtr; func: CGPathApplierFunction ); external name '_CGPathApply'; (* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


end.
