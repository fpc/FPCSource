{ CoreGraphics - CGLayer.h
 * Copyright (c) 2004 Apple Computer, Inc.
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

unit CGLayer;
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
uses MacTypes,CFBase,CFDictionary,CGBase,CGGeometry,CGContext;
{$ALIGN POWER}


type
	CGLayerRef = ^SInt32; { an opaque 32-bit type }


{ Create a layer of size `size' relative to the context `context'. The
 * value of `size' is specified in default user space (base space) units.
 * If `size' is NULL, then the underlying size of `context' is used. The
 * parameter `auxiliaryInfo' should be NULL; it is reserved for future
 * expansion. }

function CGLayerCreateWithContext( context: CGContextRef; size: CGSize; auxiliaryInfo: CFDictionaryRef ): CGLayerRef; external name '_CGLayerCreateWithContext'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Equivalent to `CFRetain(layer)', except it doesn't crash (as CFRetain
 * does) if `layer' is NULL. }

function CGLayerRetain( layer: CGLayerRef ): CGLayerRef; external name '_CGLayerRetain'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Equivalent to `CFRelease(layer)', except it doesn't crash (as CFRelease
 * does) if `layer' is NULL. }

procedure CGLayerRelease( layer: CGLayerRef ); external name '_CGLayerRelease'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Return the size of the layer `layer'. }

function CGLayerGetSize( layer: CGLayerRef ): CGSize; external name '_CGLayerGetSize'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Return the context of `layer'. }

function CGLayerGetContext( layer: CGLayerRef ): CGContextRef; external name '_CGLayerGetContext'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Draw the contents of `layer' into `rect' of `context'. The contents are
 * scaled, if necessary, to fit into `rect'; the rectangle `rect' is in
 * user space. }

procedure CGContextDrawLayerInRect( context: CGContextRef; rect: CGRect; layer: CGLayerRef ); external name '_CGContextDrawLayerInRect'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Draw the contents of `layer' at `point' in `context'.  This is
 * equivalent to calling "CGContextDrawLayerInRect" with a rectangle having
 * origin at `point' and size equal to the size of `layer'. }

procedure CGContextDrawLayerAtPoint( context: CGContextRef; point: CGPoint; layer: CGLayerRef ); external name '_CGContextDrawLayerAtPoint'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Return the CFTypeID for CGLayerRefs. }

function CGLayerGetTypeID: CFTypeID; external name '_CGLayerGetTypeID'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


end.
