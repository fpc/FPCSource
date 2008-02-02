{ CoreGraphics - CGGeometry.h
 * Copyright (c) 1998-2003 Apple Computer, Inc.
 * All rights reserved.
 }
{       Pascal Translation Updated:  Peter N Lewis, <peter@stairways.com.au>, August 2005 }
{
    Modified for use with Free Pascal
    Version 200
    Please report any bugs to <gpc@microbizz.nl>
}

{$mode macpas}
{$packenum 1}
{$macro on}
{$inline on}
{$CALLING MWPASCAL}

unit CGGeometry;
interface
{$setc UNIVERSAL_INTERFACES_VERSION := $0342}
{$setc GAP_INTERFACES_VERSION := $0200}

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
uses MacTypes,CFBase,CGBase;
{$ALIGN POWER}


{ Points. }

type
	CGPointPtr = ^CGPoint;
	CGPoint = record
		x: Float32;
		y: Float32;
	end;

{ Sizes. }

type
	CGSizePtr = ^CGSize;
	CGSize = record
		width: Float32;
		height: Float32;
	end;

{ Rectangles. }

type
	CGRectPtr = ^CGRect;
	CGRect = record
		origin: CGPoint;
		size: CGSize;
	end;

{ Rectangle edges. }

type
	CGRectEdge = SInt32;
const
	CGRectMinXEdge = 0;
	CGRectMinYEdge = 1;
	CGRectMaxXEdge = 2;
	CGRectMaxYEdge = 3;

{ The "zero" point -- equivalent to CGPointMake(0, 0). } 

var CGPointZero: CGPoint; external name '_CGPointZero'; (* attribute const *)

{ The "zero" size -- equivalent to CGSizeMake(0, 0). } 

var CGSizeZero: CGSize; external name '_CGSizeZero'; (* attribute const *)

{ The "zero" rectangle -- equivalent to CGRectMake(0, 0, 0, 0). } 

var CGRectZero: CGRect; external name '_CGRectZero'; (* attribute const *)

{ The "empty" rect.  This is the rectangle returned when, for example, we
 * intersect two disjoint rectangles.  Note that the null rect is not the
 * same as the zero rect. }

var CGRectNull: CGRect; external name '_CGRectNull'; (* attribute const *)

{ The infinite rectangle. }

var CGRectInfinite: CGRect; external name '_CGRectInfinite'; (* attribute const *) (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Make a point from `(x, y)'. }
// CG_INLINE CGPoint CGPointMake(float x, float y);

{ Make a size from `(width, height)'. }
// CG_INLINE CGSize CGSizeMake(float width, float height);

{ Make a rect from `(x, y; width, height)'. }
// CG_INLINE CGRect CGRectMake(float x, float y, float width, float height);

{ Return the leftmost x-value of `rect'. }

function CGRectGetMinX( rect: CGRect ): Float32; external name '_CGRectGetMinX';

{ Return the midpoint x-value of `rect'. }

function CGRectGetMidX( rect: CGRect ): Float32; external name '_CGRectGetMidX';

{ Return the rightmost x-value of `rect'. }

function CGRectGetMaxX( rect: CGRect ): Float32; external name '_CGRectGetMaxX';

{ Return the bottommost y-value of `rect'. }

function CGRectGetMinY( rect: CGRect ): Float32; external name '_CGRectGetMinY';

{ Return the midpoint y-value of `rect'. }

function CGRectGetMidY( rect: CGRect ): Float32; external name '_CGRectGetMidY';

{ Return the topmost y-value of `rect'. }

function CGRectGetMaxY( rect: CGRect ): Float32; external name '_CGRectGetMaxY';

{ Return the width of `rect'. }

function CGRectGetWidth( rect: CGRect ): Float32; external name '_CGRectGetWidth';

{ Return the height of `rect'. }

function CGRectGetHeight( rect: CGRect ): Float32; external name '_CGRectGetHeight';

{ Return 1 if `point1' and `point2' are the same, 0 otherwise. }

function CGPointEqualToPoint( point1: CGPoint; point2: CGPoint ): SInt32; external name '_CGPointEqualToPoint';

{ Return 1 if `size1' and `size2' are the same, 0 otherwise. }

function CGSizeEqualToSize( size1: CGSize; size2: CGSize ): SInt32; external name '_CGSizeEqualToSize';

{ Return 1 if `rect1' and `rect2' are the same, 0 otherwise. }

function CGRectEqualToRect( rect1: CGRect; rect2: CGRect ): SInt32; external name '_CGRectEqualToRect';

{ Standardize `rect' -- i.e., convert it to an equivalent rect which has
 * positive width and height. }

function CGRectStandardize( rect: CGRect ): CGRect; external name '_CGRectStandardize';

{ Return 1 if `rect' is empty -- i.e., if it has zero width or height.  A
 * null rect is defined to be empty. }

function CGRectIsEmpty( rect: CGRect ): SInt32; external name '_CGRectIsEmpty';

{ Return 1 if `rect' is null -- e.g., the result of intersecting two
 * disjoint rectangles is a null rect. }

function CGRectIsNull( rect: CGRect ): SInt32; external name '_CGRectIsNull';

{ Return true if `rect' is the infinite rectangle, false otherwise. }

function CGRectIsInfinite( rect: CGRect ): CBool; external name '_CGRectIsInfinite'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Inset `rect' by `(dx, dy)' -- i.e., offset its origin by `(dx, dy)', and
 * decrease its size by `(2*dx, 2*dy)'. }

function CGRectInset( rect: CGRect; dx: Float32; dy: Float32 ): CGRect; external name '_CGRectInset';

{ Expand `rect' to the smallest rect containing it with integral origin
 * and size. }

function CGRectIntegral( rect: CGRect ): CGRect; external name '_CGRectIntegral';

{ Return the union of `r1' and `r2'. }

function CGRectUnion( r1: CGRect; r2: CGRect ): CGRect; external name '_CGRectUnion';

{ Return the intersection of `r1' and `r2'.  This may return a null
 * rect. }

function CGRectIntersection( r1: CGRect; r2: CGRect ): CGRect; external name '_CGRectIntersection';

{ Offset `rect' by `(dx, dy)'. }

function CGRectOffset( rect: CGRect; dx: Float32; dy: Float32 ): CGRect; external name '_CGRectOffset';

{ Make two new rectangles, `slice' and `remainder', by dividing `rect'
 * with a line that's parallel to one of its sides, specified by `edge' --
 * either `CGRectMinXEdge', `CGRectMinYEdge', `CGRectMaxXEdge', or
 * `CGRectMaxYEdge'.  The size of `slice' is determined by `amount', which
 * measures the distance from the specified edge. }

procedure CGRectDivide( rect: CGRect; var slice: CGRect; var remainder: CGRect; amount: Float32; edge: CGRectEdge ); external name '_CGRectDivide';

{ Return 1 if `point' is contained in `rect', 0 otherwise. }

function CGRectContainsPoint( rect: CGRect; point: CGPoint ): SInt32; external name '_CGRectContainsPoint';

{ Return 1 if `rect2' is contained in `rect1', 0 otherwise.  `rect2' is
 * contained in `rect1' if the union of `rect1' and `rect2' is equal to
 * `rect1'. }

function CGRectContainsRect( rect1: CGRect; rect2: CGRect ): SInt32; external name '_CGRectContainsRect';

{ Return 1 if `rect1' intersects `rect2', 0 otherwise.  `rect1' intersects
 * `rect2' if the intersection of `rect1' and `rect2' is not the null
 * rect. }

function CGRectIntersectsRect( rect1: CGRect; rect2: CGRect ): SInt32; external name '_CGRectIntersectsRect';

(*
{** Definitions of inline functions. **}
// CG_INLINE CGPoint CGPointMake(float x, float y)
{
    CGPoint p; p.x = x; p.y = y; return p;
}
// CG_INLINE CGSize CGSizeMake(float width, float height)
{
    CGSize size; size.width = width; size.height = height; return size;
}
// CG_INLINE CGRect CGRectMake(float x, float y, float width, float height)
{
    CGRect rect;
    rect.origin.x = x; rect.origin.y = y;
    rect.size.width = width; rect.size.height = height;
    return rect;
}
*)

end.
