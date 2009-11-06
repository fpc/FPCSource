{ CoreGraphics - CGBitmapContext.h
 * Copyright (c) 2000-2008 Apple Inc.
 * All rights reserved. }
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

unit CGBitmapContext;
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
uses MacTypes,CGColorSpace,CGImage,CGBase,CGContext;
{$endc} {not MACOSALLINCLUDE}

{$ALIGN POWER}

{ The callback for releasing the data supplied to
   `CGBitmapContextCreateWithData'. }

{$ifc TARGET_OS_MAC}
type
	CGBitmapContextReleaseDataCallback = procedure( releaseInfo: UnivPtr; data: UnivPtr );

{ Create a bitmap context. The context draws into a bitmap which is `width'
   pixels wide and `height' pixels high. The number of components for each
   pixel is specified by `space', which may also specify a destination color
   profile. The number of bits for each component of a pixel is specified by
   `bitsPerComponent'. The number of bytes per pixel is equal to
   `(bitsPerComponent * number of components + 7)/8'. Each row of the bitmap
   consists of `bytesPerRow' bytes, which must be at least `width * bytes
   per pixel' bytes; in addition, `bytesPerRow' must be an integer multiple
   of the number of bytes per pixel. `data', if non-NULL, points to a block
   of memory at least `bytesPerRow * height' bytes. If `data' is NULL, the
   context will allocate the data itself; this data will be freed when the
   context is deallocated. `bitmapInfo' specifies whether the bitmap should
   contain an alpha channel and how it's to be generated, along with whether
   the components are floating-point or integer. If `releaseCallback' is
   non-NULL, it is called when the context is freed with `releaseInfo' and
   `data' as arguments. }

function CGBitmapContextCreateWithData(data: UnivPtr; width: size_t; height: size_t; bitsPerComponent: size_t; bytesPerRow: size_t; colorspace: CGColorSpaceRef; bitmapInfo: CGBitmapInfo; releaseCallback: CGBitmapContextReleaseDataCallback; releaseInfo: UnivPtr): CGContextRef; external name '_CGBitmapContextCreateWithData'; (* AVAILABLE_MAC_OS_X_VERSION_10_6_AND_LATER *)
{$endc}

{ Create a bitmap context.  The context draws into a bitmap which is
 * `width' pixels wide and `height' pixels high.  The number of components
 * for each pixel is specified by `colorspace', which also may specify a
 * destination color profile. The number of bits for each component of a
 * pixel is specified by `bitsPerComponent', which must be 1, 2, 4, or 8.
 * Each row of the bitmap consists of `bytesPerRow' bytes, which must be at
 * least `(width * bitsPerComponent * number of components + 7)/8' bytes.
 * `data' points a block of memory at least `bytesPerRow * height' bytes.
 * `bitmapInfo' specifies whether the bitmap should contain an alpha
 * channel and how it's to be generated, along with whether the components
 * are floating-point or integer. }

function CGBitmapContextCreate( data: UnivPtr; width: size_t; height: size_t; bitsPerComponent: size_t; bytesPerRow: size_t; colorspace: CGColorSpaceRef; bitmapInfo: CGBitmapInfo ): CGContextRef; external name '_CGBitmapContextCreate';

{ Return the data associated with the bitmap context `c', or NULL if `c'
 * is not a bitmap context. }

function CGBitmapContextGetData( c: CGContextRef ): UnivPtr; external name '_CGBitmapContextGetData'; (* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)

{ Return the width of the bitmap context `c', or 0 if `c' is not a bitmap
 * context. }

function CGBitmapContextGetWidth( c: CGContextRef ): size_t; external name '_CGBitmapContextGetWidth'; (* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)

{ Return the height of the bitmap context `c', or 0 if `c' is not a bitmap
 * context. }

function CGBitmapContextGetHeight( c: CGContextRef ): size_t; external name '_CGBitmapContextGetHeight'; (* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)

{ Return the bits per component of the bitmap context `c', or 0 if `c' is
 * not a bitmap context. }

function CGBitmapContextGetBitsPerComponent( c: CGContextRef ): size_t; external name '_CGBitmapContextGetBitsPerComponent'; (* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)

{ Return the bits per pixel of the bitmap context `c', or 0 if `c' is not
 * a bitmap context. }

function CGBitmapContextGetBitsPerPixel( c: CGContextRef ): size_t; external name '_CGBitmapContextGetBitsPerPixel'; (* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)

{ Return the bytes per row of the bitmap context `c', or 0 if `c' is not a
 * bitmap context. }

function CGBitmapContextGetBytesPerRow( c: CGContextRef ): size_t; external name '_CGBitmapContextGetBytesPerRow'; (* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)

{ Return the colorspace of the bitmap context `c', or NULL if `c' is not a
 * bitmap context. }

function CGBitmapContextGetColorSpace( c: CGContextRef ): CGColorSpaceRef; external name '_CGBitmapContextGetColorSpace'; (* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)

{ Return the alpha info of the bitmap context `c', or kCGImageAlphaNone if
 * `c' is not a bitmap context. }

function CGBitmapContextGetAlphaInfo( c: CGContextRef ): CGImageAlphaInfo; external name '_CGBitmapContextGetAlphaInfo'; (* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)

{ Return the bitmap info of the bitmap context `c', or 0 if `c' is not a
 * bitmap context. }

function CGBitmapContextGetBitmapInfo( c: CGContextRef ): CGBitmapInfo; external name '_CGBitmapContextGetBitmapInfo'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Return an image containing a snapshot of the bitmap context `c'.  If
 * context is not a bitmap context, or if the image cannot be created for
 * any reason, this function returns NULL.  This is a "copy" operation ---
 * subsequent changes to context will not affect the contents of the
 * returned image.
 *
 * Note that in some cases the copy will actually follow "copy-on-write"
 * semantics, so that the actual physical copy of the bits will only occur
 * if the underlying data in the bitmap context is modified.  As a
 * consequence, you may wish to use the resulting image and release it
 * before performing more drawing into the bitmap context; in this way, the
 * actual physical copy of the data may be avoided. }

function CGBitmapContextCreateImage( c: CGContextRef ): CGImageRef; external name '_CGBitmapContextCreateImage'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}
