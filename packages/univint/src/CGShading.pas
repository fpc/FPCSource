{ CoreGraphics - CGShading.h
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

unit CGShading;
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
uses MacTypes,CGBase,CGColorSpace,CGFunction,CGGeometry,CFBase;
{$ALIGN POWER}


type
	CGShadingRef = ^SInt32; { an opaque 32-bit type }


{! @function CGShadingGetTypeID
 *   Return the CFTypeID for CGShadingRefs.
 }

function CGShadingGetTypeID: CFTypeID; external name '_CGShadingGetTypeID'; (* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)

{! @function CGShadingCreateAxial
 *
 * Create a shading defining a color blend which varies along a linear axis
 * between two endpoints and extends indefinitely perpendicular to that
 * axis. The shading may optionally extend beyond either endpoint by
 * continuing the boundary colors indefinitely.
 *
 * @param colorspace
 *   The colorspace in which color values are expressed.
 * @param start
 *   The starting point of the axis, in the shading's target coordinate space.
 * @param end
 *   The ending point of the axis, in the shading's target coordinate space.
 * @param function
 *   A 1-in, N-out function, where N is one more (for alpha) than the
 *   number of color components in the shading's colorspace.  The input
 *   value 0 corresponds to the color at the starting point of the shading;
 *   the input value 1 corresponds to the color at the ending point of the
 *   shading.
 * @param extendStart
 *   A boolean specifying whether to extend the shading beyond the starting
 *   point of the axis.
 * @param extendEnd
 *   A boolean specifying whether to extend the shading beyond the ending
 *   point of the axis.
 }

function CGShadingCreateAxial( colorspace: CGColorSpaceRef; start: CGPoint; finish: CGPoint; func: CGFunctionRef; extendStart: CBool; extendEnd: CBool ): CGShadingRef; external name '_CGShadingCreateAxial'; (* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)

{! @function CGShadingCreateRadial
 *
 * Create a shading defining a color blend which varies between two
 * circles.  The shading may optionally extend beyond either circle by
 * continuing the boundary colors.
 *
 * @param colorspace
 *   The colorspace in which color values are expressed.
 * @param start
 *   The center of the starting circle, in the shading's target coordinate
 *   space.
 * @param startRadius
 *   The radius of the starting circle, in the shading's target coordinate
 *   space.
 * @param end
 *   The center of the ending circle, in the shading's target coordinate
 *   space.
 * @param endRadius
 *   The radius of the ending circle, in the shading's target coordinate
 *   space.
 * @param function
 *   A 1-in, N-out function, where N is one more (for alpha) than the
 *   number of color components in the shading's colorspace.  The input
 *   value 0 corresponds to the color of the starting circle; the input
 *   value 1 corresponds to the color of the ending circle.
 * @param extendStart
 *   A boolean specifying whether to extend the shading beyond the starting
 *   circle.
 * @param extendEnd
 *   A boolean specifying whether to extend the shading beyond the ending
 *   circle.
 }

function CGShadingCreateRadial( colorspace: CGColorSpaceRef; start: CGPoint; startRadius: Float32; finish: CGPoint; endRadius: Float32; func: CGFunctionRef; extendStart: CBool; extendEnd: CBool ): CGShadingRef; external name '_CGShadingCreateRadial'; (* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)

{! @function CGShadingRetain
 *
 * Equivalent to <tt>CFRetain(shading)</tt>.
 }

function CGShadingRetain( shading: CGShadingRef ): CGShadingRef; external name '_CGShadingRetain'; (* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)

{! @function CGShadingRelease
 *
 * Equivalent to <tt>CFRelease(shading)</tt>.
 }

procedure CGShadingRelease( shading: CGShadingRef ); external name '_CGShadingRelease'; (* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


end.
