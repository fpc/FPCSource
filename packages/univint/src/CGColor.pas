{ CoreGraphics - CGColor.h
 * Copyright (c) 2003 Apple Computer, Inc.
 * All rights reserved.
 }
{       Pascal Translation:  Peter N Lewis, <peter@stairways.com.au>, August 2005 }
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

unit CGColor;
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
uses MacTypes,CFBase,CGBase,CGColorSpace;
{$ALIGN POWER}


// CGColorRef defined in CGBase


{ Return the CFTypeID for CGColors. }

function CGColorGetTypeID: CFTypeID; external name '_CGColorGetTypeID'; (* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

{ Create a color in colorspace `colorspace' with color components
 * (including alpha) specified by `components'.  `colorspace' may be any
 * colorspace except a pattern colorspace. }

function CGColorCreate( colorspace: CGColorSpaceRef; {const} components: {variable-size-array} Float32Ptr ): CGColorRef; external name '_CGColorCreate'; (* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

{ Create a color in colorspace `colorspace' with pattern `pattern' and
 * components `components'.  `colorspace' must be a pattern colorspace. }

function CGColorCreateWithPattern( colorspace: CGColorSpaceRef; pattern: CGPatternRef; {const} components: {variable-size-array} Float32Ptr ): CGColorRef; external name '_CGColorCreateWithPattern'; (* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

{ Create a copy of `color'. }

function CGColorCreateCopy( color: CGColorRef ): CGColorRef; external name '_CGColorCreateCopy'; (* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

{ Create a copy of `color' with alpha set to `alpha'. }

function CGColorCreateCopyWithAlpha( color: CGColorRef; alpha: Float32 ): CGColorRef; external name '_CGColorCreateCopyWithAlpha'; (* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

{ Equivalent to `CFRetain(color)', except it doesn't crash (as CFRetain
 * does) if `color' is NULL. }

function CGColorRetain( color: CGColorRef ): CGColorRef; external name '_CGColorRetain'; (* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

{ Equivalent to `CFRelease(color)', except it doesn't crash (as CFRelease
 * does) if `color' is NULL. }

procedure CGColorRelease( color: CGColorRef ); external name '_CGColorRelease'; (* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

{ Return true if `color1' is equal to `color2'; false otherwise. }

function CGColorEqualToColor( color1: CGColorRef; color2: CGColorRef ): CBool; external name '_CGColorEqualToColor'; (* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

{ Return the number of color components (including alpha) associated with
 * `color'. }

function CGColorGetNumberOfComponents( color: CGColorRef ): size_t; external name '_CGColorGetNumberOfComponents'; (* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

{ Return the color components (including alpha) associated with
 * `color'. }

function CGColorGetComponents( color: CGColorRef ): Float32Ptr; external name '_CGColorGetComponents'; (* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

{ Return the alpha component associated with `color'. }

function CGColorGetAlpha( color: CGColorRef ): Float32; external name '_CGColorGetAlpha'; (* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

{ Return the colorspace associated with `color'. }

function CGColorGetColorSpace( color: CGColorRef ): CGColorSpaceRef; external name '_CGColorGetColorSpace'; (* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

{ Return the pattern associated with `color', if it's a color in a pattern
 * colorspace; NULL otherwise. }

function CGColorGetPattern( color: CGColorRef ): CGPatternRef; external name '_CGColorGetPattern'; (* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)


end.
