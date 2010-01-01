{ CoreGraphics - CGPath.h
 * Copyright (c) 2001-2008 Apple Inc.
 * All rights reserved. }
{       Pascal Translation:  Peter N Lewis, <peter@stairways.com.au>, August 2005 }
{       Pascal Translation Updated:  Jonas Maebe, <jonas@freepascal.org>, October 2009 }
{
    Modified for use with Free Pascal
    Version 308
    Please report any bugs to <gpc@microbizz.nl>
}

{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}
{$mode macpas}
{$packenum 1}
{$macro on}
{$inline on}
{$calling mwpascal}

unit CGPath;
interface
{$setc UNIVERSAL_INTERFACES_VERSION := $0400}
{$setc GAP_INTERFACES_VERSION := $0308}

{$ifc not defined USE_CFSTR_CONSTANT_MACROS}
    {$setc USE_CFSTR_CONSTANT_MACROS := TRUE}
{$endc}

{$ifc defined CPUPOWERPC and defined CPUI386}
	{$error Conflicting initial definitions for CPUPOWERPC and CPUI386}
{$endc}
{$ifc defined FPC_BIG_ENDIAN and defined FPC_LITTLE_ENDIAN}
	{$error Conflicting initial definitions for FPC_BIG_ENDIAN and FPC_LITTLE_ENDIAN}
{$endc}

{$ifc not defined __ppc__ and defined CPUPOWERPC32}
	{$setc __ppc__ := 1}
{$elsec}
	{$setc __ppc__ := 0}
{$endc}
{$ifc not defined __ppc64__ and defined CPUPOWERPC64}
	{$setc __ppc64__ := 1}
{$elsec}
	{$setc __ppc64__ := 0}
{$endc}
{$ifc not defined __i386__ and defined CPUI386}
	{$setc __i386__ := 1}
{$elsec}
	{$setc __i386__ := 0}
{$endc}
{$ifc not defined __x86_64__ and defined CPUX86_64}
	{$setc __x86_64__ := 1}
{$elsec}
	{$setc __x86_64__ := 0}
{$endc}
{$ifc not defined __arm__ and defined CPUARM}
	{$setc __arm__ := 1}
{$elsec}
	{$setc __arm__ := 0}
{$endc}

{$ifc defined cpu64}
  {$setc __LP64__ := 1}
{$elsec}
  {$setc __LP64__ := 0}
{$endc}


{$ifc defined __ppc__ and __ppc__ and defined __i386__ and __i386__}
	{$error Conflicting definitions for __ppc__ and __i386__}
{$endc}

{$ifc defined __ppc__ and __ppc__}
	{$setc TARGET_CPU_PPC := TRUE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := FALSE}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
{$elifc defined __ppc64__ and __ppc64__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := TRUE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := FALSE}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
{$elifc defined __i386__ and __i386__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := TRUE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := FALSE}
{$ifc defined(iphonesim)}
 	{$setc TARGET_OS_MAC := FALSE}
	{$setc TARGET_OS_IPHONE := TRUE}
	{$setc TARGET_IPHONE_SIMULATOR := TRUE}
{$elsec}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
{$endc}
{$elifc defined __x86_64__ and __x86_64__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := TRUE}
	{$setc TARGET_CPU_ARM := FALSE}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
{$elifc defined __arm__ and __arm__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := TRUE}
	{ will require compiler define when/if other Apple devices with ARM cpus ship }
	{$setc TARGET_OS_MAC := FALSE}
	{$setc TARGET_OS_IPHONE := TRUE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
{$elsec}
	{$error __ppc__ nor __ppc64__ nor __i386__ nor __x86_64__ nor __arm__ is defined.}
{$endc}

{$ifc defined __LP64__ and __LP64__ }
  {$setc TARGET_CPU_64 := TRUE}
{$elsec}
  {$setc TARGET_CPU_64 := FALSE}
{$endc}

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
{$endc} {not MACOSALLINCLUDE}

{$ALIGN POWER}


type
	CGMutablePathRef = ^SInt32; { an opaque type }
type
	CGPathRef = ^SInt32; { an opaque type }


{ Return the CFTypeID for CGPathRefs. }

function CGPathGetTypeID: CFTypeID; external name '_CGPathGetTypeID';
(* CG_AVAILABLE_STARTING(__MAC_10_2, __IPHONE_2_0) *)

{ Create a mutable path. }

function CGPathCreateMutable: CGMutablePathRef; external name '_CGPathCreateMutable';
(* CG_AVAILABLE_STARTING(__MAC_10_2, __IPHONE_2_0) *)

{ Create a copy of `path'. }

function CGPathCreateCopy( path: CGPathRef ): CGPathRef; external name '_CGPathCreateCopy';
(* CG_AVAILABLE_STARTING(__MAC_10_2, __IPHONE_2_0) *)

{ Create a mutable copy of `path'. }

function CGPathCreateMutableCopy( path: CGPathRef ): CGMutablePathRef; external name '_CGPathCreateMutableCopy';
(* CG_AVAILABLE_STARTING(__MAC_10_2, __IPHONE_2_0) *)

{ Equivalent to `CFRetain(path)', except it doesn't crash (as CFRetain
   does) if `path' is NULL. }

function CGPathRetain( path: CGPathRef ): CGPathRef; external name '_CGPathRetain';
(* CG_AVAILABLE_STARTING(__MAC_10_2, __IPHONE_2_0) *)

{ Equivalent to `CFRelease(path)', except it doesn't crash (as CFRelease
   does) if `path' is NULL. }

procedure CGPathRelease( path: CGPathRef ); external name '_CGPathRelease';
(* CG_AVAILABLE_STARTING(__MAC_10_2, __IPHONE_2_0) *)

{ Return true if `path1' is equal to `path2'; false otherwise. }

function CGPathEqualToPath( path1: CGPathRef; path2: CGPathRef ): CBool; external name '_CGPathEqualToPath';
(* CG_AVAILABLE_STARTING(__MAC_10_2, __IPHONE_2_0) *)

{** Path construction functions. **}

{ Move the current point to `(x, y)' in `path' and begin a new subpath. If
   `m' is non-NULL, then transform `(x, y)' by `m' first. }

procedure CGPathMoveToPoint( path: CGMutablePathRef; const (*var*) m: CGAffineTransform; x: CGFloat; y: CGFloat ); external name '_CGPathMoveToPoint';
(* CG_AVAILABLE_STARTING(__MAC_10_2, __IPHONE_2_0) *)

{ Append a straight line segment from the current point to `(x, y)' in
   `path' and move the current point to `(x, y)'. If `m' is non-NULL, then
   transform `(x, y)' by `m' first. }

procedure CGPathAddLineToPoint( path: CGMutablePathRef; const (*var*) m: CGAffineTransform; x: CGFloat; y: CGFloat ); external name '_CGPathAddLineToPoint';
(* CG_AVAILABLE_STARTING(__MAC_10_2, __IPHONE_2_0) *)

{ Append a quadratic curve from the current point to `(x, y)' with control
   point `(cpx, cpy)' in `path' and move the current point to `(x, y)'. If
   `m' is non-NULL, then transform all points by `m' first. }

procedure CGPathAddQuadCurveToPoint( path: CGMutablePathRef; const (*var*) m: CGAffineTransform; cpx: CGFloat; cpy: CGFloat; x: CGFloat; y: CGFloat ); external name '_CGPathAddQuadCurveToPoint';
(* CG_AVAILABLE_STARTING(__MAC_10_2, __IPHONE_2_0) *)

{ Append a cubic BŽzier curve from the current point to `(x,y)' with
   control points `(cp1x, cp1y)' and `(cp2x, cp2y)' in `path' and move the
   current point to `(x, y)'. If `m' is non-NULL, then transform all points
   by `m' first. }

procedure CGPathAddCurveToPoint( path: CGMutablePathRef; const (*var*) m: CGAffineTransform; cp1x: CGFloat; cp1y: CGFloat; cp2x: CGFloat; cp2y: CGFloat; x: CGFloat; y: CGFloat ); external name '_CGPathAddCurveToPoint';
(* CG_AVAILABLE_STARTING(__MAC_10_2, __IPHONE_2_0) *)

{ Append a line from the current point to the starting point of the current
   subpath of `path' and end the subpath. }

procedure CGPathCloseSubpath( path: CGMutablePathRef ); external name '_CGPathCloseSubpath';
(* CG_AVAILABLE_STARTING(__MAC_10_2, __IPHONE_2_0) *)

{** Path construction convenience functions. **}

{ Add `rect' to `path'. If `m' is non-NULL, then first transform `rect' by
   `m' before adding it to `path'. }

procedure CGPathAddRect( path: CGMutablePathRef; const (*var*) m: CGAffineTransform; rect: CGRect ); external name '_CGPathAddRect';
(* CG_AVAILABLE_STARTING(__MAC_10_2, __IPHONE_2_0) *)

{ Add each rectangle specified by `rects', an array of `count' CGRects, to
   `path'. If `m' is non-NULL, then first transform each rectangle by `m'
   before adding it to `path'. }

procedure CGPathAddRects( path: CGMutablePathRef; const (*var*) m: CGAffineTransform; {const} rects: {variable-size-array} CGRectPtr; count: size_t ); external name '_CGPathAddRects';
(* CG_AVAILABLE_STARTING(__MAC_10_2, __IPHONE_2_0) *)

{ Move to the first element of `points', an array of `count' CGPoints, and
   append a line from each point to the next point in `points'. If `m' is
   non-NULL, then first transform each point by `m'. }

procedure CGPathAddLines( path: CGMutablePathRef; const (*var*) m: CGAffineTransform; {const} points: {variable-size-array} CGPointPtr; count: size_t ); external name '_CGPathAddLines';
(* CG_AVAILABLE_STARTING(__MAC_10_2, __IPHONE_2_0) *)

{ Add an ellipse (an oval) inside `rect' to `path'. The ellipse is
   approximated by a sequence of BŽzier curves. The center of the ellipse is
   the midpoint of `rect'. If `rect' is square, then the ellipse will be
   circular with radius equal to one-half the width (equivalently, one-half
   the height) of `rect'. If `rect' is rectangular, then the major- and
   minor-axes will be the `width' and `height' of rect. The ellipse forms a
   complete subpath of `path' --- that is, it begins with a "move to" and
   ends with a "close subpath" --- oriented in the clockwise direction. If
   `m' is non-NULL, then the constructed BŽzier curves representing the
   ellipse will be transformed by `m' before they are added to `path'. }

procedure CGPathAddEllipseInRect( path: CGMutablePathRef; const (*var*) m: CGAffineTransform; rect: CGRect ); external name '_CGPathAddEllipseInRect';
(* CG_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_2_0) *)

{ Add an arc of a circle to `path', possibly preceded by a straight line
   segment. The arc is approximated by a sequence of BŽzier curves. `(x, y)'
   is the center of the arc; `radius' is its radius; `startAngle' is the
   angle to the first endpoint of the arc; `endAngle' is the angle to the
   second endpoint of the arc; and `clockwise' is true if the arc is to be
   drawn clockwise, false otherwise. `startAngle' and `endAngle' are
   measured in radians. If `m' is non-NULL, then the constructed BŽzier
   curves representing the arc will be transformed by `m' before they are
   added to `path'. }

procedure CGPathAddArc( path: CGMutablePathRef; const (*var*) m: CGAffineTransform; x: CGFloat; y: CGFloat; radius: CGFloat; startAngle: CGFloat; endAngle: CGFloat; clockwise: CBool ); external name '_CGPathAddArc';
(* CG_AVAILABLE_STARTING(__MAC_10_2, __IPHONE_2_0) *)

{ Add an arc of a circle to `path', possibly preceded by a straight line
   segment. The arc is approximated by a sequence of BŽzier curves. `radius'
   is the radius of the arc. The resulting arc is tangent to the line from
   the current point of `path' to `(x1, y1)', and the line from `(x1, y1)'
   to `(x2, y2)'. If `m' is non-NULL, then the constructed BŽzier curves
   representing the arc will be transformed by `m' before they are added to
   `path'. }

procedure CGPathAddArcToPoint( path: CGMutablePathRef; const (*var*) m: CGAffineTransform; x1: CGFloat; y1: CGFloat; x2: CGFloat; y2: CGFloat; radius: CGFloat ); external name '_CGPathAddArcToPoint';
(* CG_AVAILABLE_STARTING(__MAC_10_2, __IPHONE_2_0) *)

{ Add `path2' to `path1'. If `m' is non-NULL, then the points in `path2'
   will be transformed by `m' before they are added to `path1'. }

procedure CGPathAddPath( path1: CGMutablePathRef; const (*var*) m: CGAffineTransform; path2: CGPathRef ); external name '_CGPathAddPath';
(* CG_AVAILABLE_STARTING(__MAC_10_2, __IPHONE_2_0) *)

{** Path information functions. **}

{ Return true if `path' contains no elements, false otherwise. }

function CGPathIsEmpty( path: CGPathRef ): CBool; external name '_CGPathIsEmpty';
(* CG_AVAILABLE_STARTING(__MAC_10_2, __IPHONE_2_0) *)

{ Return true if `path' represents a rectangle, false otherwise. }

function CGPathIsRect( path: CGPathRef; var rect: CGRect ): CBool; external name '_CGPathIsRect';
(* CG_AVAILABLE_STARTING(__MAC_10_2, __IPHONE_2_0) *)

{ Return the current point of the current subpath of `path'. If there is no
   current point, then return CGPointZero. }

function CGPathGetCurrentPoint( path: CGPathRef ): CGPoint; external name '_CGPathGetCurrentPoint';
(* CG_AVAILABLE_STARTING(__MAC_10_2, __IPHONE_2_0) *)

{ Return the bounding box of `path'. The bounding box is the smallest
   rectangle completely enclosing all points in the path, including control
   points for BŽzier cubic and quadratic curves. If the path is empty, then
   return `CGRectNull'. }

function CGPathGetBoundingBox( path: CGPathRef ): CGRect; external name '_CGPathGetBoundingBox';
(* CG_AVAILABLE_STARTING(__MAC_10_2, __IPHONE_2_0) *)

{ Return the path bounding box of `path'. The path bounding box is the
   smallest rectangle completely enclosing all points in the path, *not*
   including control points for BŽzier cubic and quadratic curves. If the
   path is empty, then return `CGRectNull'. }

function CGPathGetPathBoundingBox( path: CGPathRef ): CGRect; external name '_CGPathGetPathBoundingBox';
(* CG_AVAILABLE_STARTING(__MAC_10_6, __IPHONE_NA) *)

{ Return true if `point' is contained in `path'; false otherwise. A point
   is contained in a path if it is inside the painted region when the path
   is filled; if `eoFill' is true, then the even-odd fill rule is used to
   evaluate the painted region of the path, otherwise, the winding-number
   fill rule is used. If `m' is non-NULL, then the point is transformed by
   `m' before determining whether the path contains it. }

function CGPathContainsPoint( path: CGPathRef; const (*var*) m: CGAffineTransform; point: CGPoint; eoFill: CBool ): CBool; external name '_CGPathContainsPoint';
(* CG_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_2_0) *)

{ The types of path elements returned by `CGPathApply'. }

type
	CGPathElementType = SInt32;
const
	kCGPathElementMoveToPoint = 0;
	kCGPathElementAddLineToPoint = 1;
	kCGPathElementAddQuadCurveToPoint = 2;
	kCGPathElementAddCurveToPoint = 3;
	kCGPathElementCloseSubpath = 4;

{ An element of a path returned by `CGPathApply'. }

type
	CGPathElement = record
		typ: CGPathElementType;
{$ifc TARGET_CPU_64}
		__alignment_dummy: SInt32;
{$endc}
		points: CGPointPtr;
	end;

{ The prototype for the function called by `CGPathApplyFunction'. }

type
	CGPathApplierFunction = procedure( info: UnivPtr; const (*var*) element: CGPathElement );

{ For element of `path', call `function', passing it the path element and
   `info'. }

procedure CGPathApply( path: CGPathRef; info: UnivPtr; func: CGPathApplierFunction ); external name '_CGPathApply';
(* CG_AVAILABLE_STARTING(__MAC_10_2, __IPHONE_2_0) *)

{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}
