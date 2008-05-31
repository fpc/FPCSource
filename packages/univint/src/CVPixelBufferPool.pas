{
 *  CVPixelBufferPool.h
 *  CoreVideo
 *
 *  Copyright 2004 Apple Computer, Inc. All rights reserved.
 *
 }
 {	 Pascal Translation:  Gale R Paeper, <gpaeper@empirenet.com>, 2008 }
 
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

unit CVPixelBufferPool;
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
uses MacTypes, CFBase, CFDictionary, CVBase, CVPixelBuffer, CVReturns;
{$ALIGN POWER}


 {! @header CVPixelBufferPool.h
	@copyright 2004 Apple Computer, Inc. All rights reserved.
	@availability Mac OS X 10.4 or later
    @discussion CVPixelBufferPool is a utility object for managing a set of CVPixelBuffer objects that are going to be recycled.
		   
}


type
	CVPixelBufferPoolRef = ^SInt32; { an opaque 32-bit type }

// By default, buffers will age out after one second.   If required, setting an age of zero will disable
// the age-out mechanism completely.

var kCVPixelBufferPoolMinimumBufferCountKey: CFStringRef; external name '_kCVPixelBufferPoolMinimumBufferCountKey'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCVPixelBufferPoolMaximumBufferAgeKey: CFStringRef; external name '_kCVPixelBufferPoolMaximumBufferAgeKey'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


function CVPixelBufferPoolGetTypeID: CFTypeID; external name '_CVPixelBufferPoolGetTypeID';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{!
    @function   CVPixelBufferPoolRetain
    @abstract   Retains a CVPixelBufferPoolRef object
    @discussion Equivalent to CFRetain, but NULL safe
    @param      buffer A CVPixelBufferPoolRef object that you want to retain.
    @result     A CVPixelBufferPoolRef object that is the same as the passed in buffer.
}
function CVPixelBufferPoolRetain( pixelBufferPool: CVPixelBufferPoolRef ): CVPixelBufferPoolRef; external name '_CVPixelBufferPoolRetain';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *) // NULL-safe

{!
    @function   CVPixelBufferPoolRelease
    @abstract   Releases a CVPixelBufferPoolRef object
    @discussion Equivalent to CFRelease, but NULL safe
    @param      buffer A CVPixelBufferPoolRef object that you want to release.
}
procedure CVPixelBufferPoolRelease( pixelBufferPool: CVPixelBufferPoolRef ); external name '_CVPixelBufferPoolRelease';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *) // NULL-safe

{!
    @function   CVPixelBufferPoolCreate
    @abstract   Creates a new Pixel Buffer pool.
    @param      allocator The CFAllocatorRef to use for allocating this buffer pool.  May be NULL.
    @param      attributes   A CFDictionaryRef containing the attributes to be used for creating new PixelBuffers within the pool.
    @param      poolOut   The newly created pool will be placed here
    @result     Returns kCVReturnSuccess on success
}
function CVPixelBufferPoolCreate( allocator: CFAllocatorRef; poolAttributes: CFDictionaryRef; pixelBufferAttributes: CFDictionaryRef; var poolOut: CVPixelBufferPoolRef ): CVReturn; external name '_CVPixelBufferPoolCreate';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{!
    @function   CVPixelBufferPoolGetAttributes
    @abstract   Returns the pool attributes dictionary for a CVPixelBufferPool
    @param      pool  The CVPixelBufferPoolRef to retrieve the attributes from
    @result     Returns the pool attributes dictionary, or NULL on failure.
}
function CVPixelBufferPoolGetAttributes( pool: CVPixelBufferPoolRef ): CFDictionaryRef; external name '_CVPixelBufferPoolGetAttributes';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{!
    @function   CVPixelBufferPoolGetPixelBufferAttributes
    @abstract   Returns the attributes of pixel buffers that will be created from this pool.
    @discussion This function is provided for those cases where you may need to know some information about the buffers that
                will be created up front.
    @param      pool  The CVPixelBufferPoolRef to retrieve the attributes from
    @result     Returns the pixel buffer attributes dictionary, or NULL on failure.
}
function CVPixelBufferPoolGetPixelBufferAttributes( pool: CVPixelBufferPoolRef ): CFDictionaryRef; external name '_CVPixelBufferPoolGetPixelBufferAttributes';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{!
    @function   CVPixelBufferPoolCreatePixelBuffer
    @abstract   Creates a new PixelBuffer object from the pool.
    @discussion The function creates a new (attachment-free) CVPixelBuffer using the pixel buffer attributes specifed during pool creation.
    @param      allocator The CFAllocatorRef to use for creating the pixel buffer.  May be NULL.
    @param      pool      The CVPixelBufferPool that should create the new CVPixelBuffer.
    @param      pixelBufferOut   The newly created pixel buffer will be placed here
    @result     Returns kCVReturnSuccess on success
}
function CVPixelBufferPoolCreatePixelBuffer( allocator: CFAllocatorRef; pixelBufferPool: CVPixelBufferPoolRef; var pixelBufferOut: CVPixelBufferRef ): CVReturn; external name '_CVPixelBufferPoolCreatePixelBuffer';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


end.
