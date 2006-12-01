{
 * ImageIO - CGImageDestination.h
 * Copyright (c) 2004 Apple Computer, Inc. All rights reserved.
 *
 }

{	 Pascal Translation:  Gale R Paeper, <gpaeper@empirenet.com>, 2006 }

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

unit CGImageDestination;
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
uses MacTypes, CFArray, CFBase, CFData, CFDictionary, CFURL, CGDataConsumer, CGImage, CGImageSource;
{$ALIGN POWER}


type
	CGImageDestinationRef = ^SInt32; { an opaque 32-bit type }


{* Properties which may be passed to "CGImageDestinationAddImage"
 ** or "CGImageDestinationAddImageFromSource" to effect the output.
 ** The values apply to a single image of an image destination. *}


{ The desired compression quality to use when writing to an image 
 * destination. If present, the value of this key is a CFNumberRef 
 * in the range 0.0 to 1.0. A value of 1.0 implies lossless
 * compression is desired if destination format supports it. 
 * A value of 0.0 implies that that maximum compression is 
 * desired. }

var kCGImageDestinationLossyCompressionQuality: CFStringRef; external name '_kCGImageDestinationLossyCompressionQuality'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ The desired background color to composite against when writing 
 * an image with alpha to a destination format that does not support 
 * alpha. If present, the value of this key is a CGColorRef without
 * any alpha component of its own.  If not present a white color
 * will be used if needed. }

var kCGImageDestinationBackgroundColor: CFStringRef; external name '_kCGImageDestinationBackgroundColor'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Return the CFTypeID for CGImageDestinations. }

function CGImageDestinationGetTypeID: CFTypeID; external name '_CGImageDestinationGetTypeID';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Return an array of supported type identifiers. }

function CGImageDestinationCopyTypeIdentifiers: CFArrayRef; external name '_CGImageDestinationCopyTypeIdentifiers';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Create an image destination writing to the data consumer `consumer'.
 * The parameter `type' specifies the type identifier of the resulting
 * image file.  The parameter `count' specifies number of images (not
 * including thumbnails) that the image file will contain. The `options'
 * dictionary is reserved for future use; currently, you should pass NULL
 * for this parameter. }

function CGImageDestinationCreateWithDataConsumer( consumer: CGDataConsumerRef; typ: CFStringRef; count: size_t; options: CFDictionaryRef ): CGImageDestinationRef; external name '_CGImageDestinationCreateWithDataConsumer';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Create an image destination writing to `data'. The parameter `type'
 * specifies the type identifier of the resulting image file.  The
 * parameter `count' specifies number of images (not including thumbnails)
 * that the image file will contain. The `options' dictionary is reserved
 * for future use; currently, you should pass NULL for this parameter. }

function CGImageDestinationCreateWithData( data: CFMutableDataRef; typ: CFStringRef; count: size_t; options: CFDictionaryRef ): CGImageDestinationRef; external name '_CGImageDestinationCreateWithData';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Create an image destination writing to `url'. The parameter `type'
 * specifies the type identifier of the resulting image file.  The
 * parameter `count' specifies number of images (not including thumbnails)
 * that the image file will contain. The `options' dictionary is reserved
 * for future use; currently, you should pass NULL for this parameter.
 * Note that if `url' already exists, it will be overwritten. }

function CGImageDestinationCreateWithURL( url: CFURLRef; typ: CFStringRef; count: size_t; options: CFDictionaryRef ): CGImageDestinationRef; external name '_CGImageDestinationCreateWithURL';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Specify the dictionary `properties' of properties which apply to all
 * images in the image destination `idst'. }

procedure CGImageDestinationSetProperties( idst: CGImageDestinationRef; properties: CFDictionaryRef ); external name '_CGImageDestinationSetProperties';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Set the next image in the image destination `idst' to be `image' with
 * optional properties specified in `properties'.  An error is logged if
 * more images are added than specified in the original count of the image
 * destination. }

procedure CGImageDestinationAddImage( idst: CGImageDestinationRef; image: CGImageRef; properties: CFDictionaryRef ); external name '_CGImageDestinationAddImage';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Set the next image in the image destination `idst' to be the image at
 * `index' in the image source `isrc'.  The index is zero-based. The
 * properties of the source image can be added to or overriden by supplying
 * additional keys/values in `properties'.  If a key in `properties' has
 * the value kCFNull, the corresponding property in the destination will be
 * removed. }

procedure CGImageDestinationAddImageFromSource( idst: CGImageDestinationRef; isrc: CGImageSourceRef; index: size_t; properties: CFDictionaryRef ); external name '_CGImageDestinationAddImageFromSource';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Write everything to the destination data, url or consumer of the image
 * destination `idst'.  You must call this function or the image
 * destination will not be valid.  After this function is called, no
 * additional data will be written to the image destination.  Return true
 * if the image was successfully written; false otherwise. }

function CGImageDestinationFinalize( idst: CGImageDestinationRef ): CBool; external name '_CGImageDestinationFinalize';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


end.
