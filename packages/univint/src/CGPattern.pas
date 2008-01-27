{ CoreGraphics - CGPattern.h
 * Copyright (c) 2000-2002 Apple Computer, Inc.
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

unit CGPattern;
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
uses MacTypes,CFBase,CGGeometry,CGAffineTransforms,CGBase,CGContext;
{$ALIGN POWER}


// CGPatternRef defined in CGBase


{ kCGPatternTilingNoDistortion: The pattern cell is not distorted when
 * painted, however the spacing between pattern cells may vary by as much
 * as 1 device pixel.
 *
 * kCGPatternTilingConstantSpacingMinimalDistortion: Pattern cells are
 * spaced consistently, however the pattern cell may be distorted by as
 * much as 1 device pixel when the pattern is painted.
 *
 * kCGPatternTilingConstantSpacing: Pattern cells are spaced consistently
 * as with kCGPatternTilingConstantSpacingMinimalDistortion, however the
 * pattern cell may be distorted additionally to permit a more efficient
 * implementation. }

type
	CGPatternTiling = SInt32;
const
	kCGPatternTilingNoDistortion = 0;
	kCGPatternTilingConstantSpacingMinimalDistortion = 1;
	kCGPatternTilingConstantSpacing = 2;


{ The drawing of the pattern is delegated to the callbacks.  The callbacks
 * may be called one or many times to draw the pattern.
 *
 * `version' is the version number of the structure passed in as a
 * parameter to the CGPattern creation functions. The structure defined
 * below is version 0.
 *
 * `drawPattern' should draw the pattern in the context `c'. `info' is the
 * parameter originally passed to the CGPattern creation functions.
 *
 * `releaseInfo' is called when the pattern is deallocated. }

type
	CGPatternDrawPatternCallback = procedure( info: UnivPtr; c: CGContextRef );
type
	CGPatternReleaseInfoCallback = procedure( info: UnivPtr );

type
	CGPatternCallbacksPtr = ^CGPatternCallbacks;
	CGPatternCallbacks = record
		version: UInt32;
		drawPattern: CGPatternDrawPatternCallback;
		releaseInfo: CGPatternReleaseInfoCallback;
	end;

{ Return the CFTypeID for CGPatternRefs. }

function CGPatternGetTypeID: CFTypeID; external name '_CGPatternGetTypeID'; (* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)

{ Create a pattern. }

function CGPatternCreate( info: UnivPtr; bounds: CGRect; matrix: CGAffineTransform; xStep: Float32; yStep: Float32; tiling: CGPatternTiling; isColored: CBool; const (*var*) callbacks: CGPatternCallbacks ): CGPatternRef; external name '_CGPatternCreate';

{ Equivalent to `CFRetain(pattern)', except it doesn't crash (as CF does)
 * if `pattern' is NULL. }

function CGPatternRetain( pattern: CGPatternRef ): CGPatternRef; external name '_CGPatternRetain';

{ Equivalent to `CFRelease(pattern)', except it doesn't crash (as CF does)
 * if `pattern' is NULL. }

procedure CGPatternRelease( pattern: CGPatternRef ); external name '_CGPatternRelease';


end.
