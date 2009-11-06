{ CoreGraphics - CGAffineTransform.h
 * Copyright (c) 1998-2009 Apple Inc.
 * All rights reserved.
 }
{       Pascal Translation Updated:  Peter N Lewis, <peter@stairways.com.au>, August 2005 }
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

unit CGAffineTransforms;
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
	{$setc TARGET_CPU_PPC := TFALSE}
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
uses MacTypes,CGBase,CGGeometry;
{$endc} {not MACOSALLINCLUDE}

{$ALIGN POWER}


type
	CGAffineTransformPtr = ^CGAffineTransform;
	CGAffineTransform = record
		a, b, c, d: CGFloat;
		tx, ty: CGFloat;
	end;

{ The identity transform: [ 1 0 0 1 0 0 ]. }

var CGAffineTransformIdentity: CGAffineTransform; external name '_CGAffineTransformIdentity'; (* attribute const *)

{ Return the transform [ a b c d tx ty ]. }

function CGAffineTransformMake( a: CGFloat; b: CGFloat; c: CGFloat; d: CGFloat; tx: CGFloat; ty: CGFloat ): CGAffineTransform; external name '_CGAffineTransformMake';

{ Return a transform which translates by `(tx, ty)':
 *   t' = [ 1 0 0 1 tx ty ] }

function CGAffineTransformMakeTranslation( tx: CGFloat; ty: CGFloat ): CGAffineTransform; external name '_CGAffineTransformMakeTranslation';

{ Return a transform which scales by `(sx, sy)':
 *   t' = [ sx 0 0 sy 0 0 ] }

function CGAffineTransformMakeScale( sx: CGFloat; sy: CGFloat ): CGAffineTransform; external name '_CGAffineTransformMakeScale';

{ Return a transform which rotates by `angle' radians:
 *   t' = [ cos(angle) sin(angle) -sin(angle) cos(angle) 0 0 ] }

function CGAffineTransformMakeRotation( angle: CGFloat ): CGAffineTransform; external name '_CGAffineTransformMakeRotation';

{ Return true if `t' is the identity transform, false otherwise. }

function CGAffineTransformIsIdentity( t: CGAffineTransform ): CBool; external name '_CGAffineTransformIsIdentity'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Translate `t' by `(tx, ty)' and return the result:
 *   t' = [ 1 0 0 1 tx ty ] * t }

function CGAffineTransformTranslate( t: CGAffineTransform; tx: CGFloat; ty: CGFloat ): CGAffineTransform; external name '_CGAffineTransformTranslate';

{ Scale `t' by `(sx, sy)' and return the result:
 *   t' = [ sx 0 0 sy 0 0 ] * t }

function CGAffineTransformScale( t: CGAffineTransform; sx: CGFloat; sy: CGFloat ): CGAffineTransform; external name '_CGAffineTransformScale';

{ Rotate `t' by `angle' radians and return the result:
 *   t' =  [ cos(angle) sin(angle) -sin(angle) cos(angle) 0 0 ] * t }

function CGAffineTransformRotate( t: CGAffineTransform; angle: CGFloat ): CGAffineTransform; external name '_CGAffineTransformRotate';

{ Invert `t' and return the result.  If `t' has zero determinant, then `t'
 * is returned unchanged. }

function CGAffineTransformInvert( t: CGAffineTransform ): CGAffineTransform; external name '_CGAffineTransformInvert';

{ Concatenate `t2' to `t1' and return the result:
 *   t' = t1 * t2 }

function CGAffineTransformConcat( t1: CGAffineTransform; t2: CGAffineTransform ): CGAffineTransform; external name '_CGAffineTransformConcat';

{ Return true if `t1' and `t2' are equal, false otherwise. }

function CGAffineTransformEqualToTransform( t1: CGAffineTransform; t2: CGAffineTransform ): CBool; external name '_CGAffineTransformEqualToTransform'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Transform `point' by `t' and return the result:
 *   p' = p * t
 * where p = [ x y 1 ]. }

function CGPointApplyAffineTransform( point: CGPoint; t: CGAffineTransform ): CGPoint; external name '_CGPointApplyAffineTransform';

{ Transform `size' by `t' and return the result:
 *   s' = s * t
 * where s = [ width height 0 ]. }

function CGSizeApplyAffineTransform( size: CGSize; t: CGAffineTransform ): CGSize; external name '_CGSizeApplyAffineTransform';

{ Transform `rect' by `t' and return the result. Since affine transforms
 * do not preserve rectangles in general, this function returns the
 * smallest rectangle which contains the transformed corner points of
 * `rect'. If `t' consists solely of scales, flips and translations, then
 * the returned rectangle coincides with the rectangle constructed from the
 * four transformed corners. }

function CGRectApplyAffineTransform( rect: CGRect; t: CGAffineTransform ): CGRect; external name '_CGRectApplyAffineTransform'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

(*
{** Definitions of inline functions. **}

CG_INLINE CGAffineTransform
__CGAffineTransformMake(float a, float b, float c, float d, float tx, float ty)
{
    CGAffineTransform t;

    t.a = a; t.b = b; t.c = c; t.d = d; t.tx = tx; t.ty = ty;
    return t;
}

#define CGAffineTransformMake __CGAffineTransformMake

CG_INLINE CGPoint
__CGPointApplyAffineTransform(CGPoint point, CGAffineTransform t)
{
    CGPoint p;

    p.x = t.a * point.x + t.c * point.y + t.tx;
    p.y = t.b * point.x + t.d * point.y + t.ty;
    return p;
}

#define CGPointApplyAffineTransform __CGPointApplyAffineTransform

CG_INLINE CGSize
__CGSizeApplyAffineTransform(CGSize size, CGAffineTransform t)
{
    CGSize s;

    s.width = t.a * size.width + t.c * size.height;
    s.height = t.b * size.width + t.d * size.height;
    return s;
}

#define CGSizeApplyAffineTransform __CGSizeApplyAffineTransform
*)
{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}
