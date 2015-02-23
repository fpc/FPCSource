{
 * ImageIO - CGImageDestination.h
 * Copyright (c) 2004-2010 Apple Inc. All rights reserved.
 *
 }

{  Pascal Translation:  Gale R Paeper, <gpaeper@empirenet.com>, 2006 }
{  Pascal Translation Updated:  Jonas Maebe, <jonas@freepascal.org>, October 2009 }
{  Pascal Translation Updated:  Jonas Maebe, <jonas@freepascal.org>, October 2012 }

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

unit CGImageDestination;
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
{$ifc not defined __arm64__ and defined CPUAARCH64}
  {$setc __arm64__ := 1}
{$elsec}
  {$setc __arm64__ := 0}
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
	{$setc TARGET_CPU_ARM64 := FALSE}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
	{$setc TARGET_OS_EMBEDDED := FALSE}
{$elifc defined __ppc64__ and __ppc64__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := TRUE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := FALSE}
	{$setc TARGET_CPU_ARM64 := FALSE}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
	{$setc TARGET_OS_EMBEDDED := FALSE}
{$elifc defined __i386__ and __i386__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := TRUE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := FALSE}
	{$setc TARGET_CPU_ARM64 := FALSE}
{$ifc defined(iphonesim)}
 	{$setc TARGET_OS_MAC := FALSE}
	{$setc TARGET_OS_IPHONE := TRUE}
	{$setc TARGET_IPHONE_SIMULATOR := TRUE}
{$elsec}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
{$endc}
	{$setc TARGET_OS_EMBEDDED := FALSE}
{$elifc defined __x86_64__ and __x86_64__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := TRUE}
	{$setc TARGET_CPU_ARM := FALSE}
	{$setc TARGET_CPU_ARM64 := FALSE}
{$ifc defined(iphonesim)}
 	{$setc TARGET_OS_MAC := FALSE}
	{$setc TARGET_OS_IPHONE := TRUE}
	{$setc TARGET_IPHONE_SIMULATOR := TRUE}
{$elsec}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
{$endc}
	{$setc TARGET_OS_EMBEDDED := FALSE}
{$elifc defined __arm__ and __arm__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := TRUE}
	{$setc TARGET_CPU_ARM64 := FALSE}
	{ will require compiler define when/if other Apple devices with ARM cpus ship }
	{$setc TARGET_OS_MAC := FALSE}
	{$setc TARGET_OS_IPHONE := TRUE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
	{$setc TARGET_OS_EMBEDDED := TRUE}
{$elifc defined __arm64__ and __arm64__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := FALSE}
	{$setc TARGET_CPU_ARM64 := TRUE}
	{ will require compiler define when/if other Apple devices with ARM cpus ship }
	{$setc TARGET_OS_MAC := FALSE}
	{$setc TARGET_OS_IPHONE := TRUE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
	{$setc TARGET_OS_EMBEDDED := TRUE}
{$elsec}
	{$error __ppc__ nor __ppc64__ nor __i386__ nor __x86_64__ nor __arm__ nor __arm64__ is defined.}
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
uses MacTypes, CFArray, CFBase, CFData, CFDictionary, CFError, CFURL, CGDataConsumer, CGImage, CGImageSource, CGImageMetadata;
{$endc} {not MACOSALLINCLUDE}


{$ALIGN POWER}


type
	CGImageDestinationRef = ^OpaqueCGImageDestinationRef; { an opaque type }
	OpaqueCGImageDestinationRef = record end;


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
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)


{ The desired background color to composite against when writing 
 * an image with alpha to a destination format that does not support 
 * alpha. If present, the value of this key is a CGColorRef without
 * any alpha component of its own.  If not present a white color
 * will be used if needed. }

var kCGImageDestinationBackgroundColor: CFStringRef; external name '_kCGImageDestinationBackgroundColor'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)


{ Return the CFTypeID for CGImageDestinations. }

function CGImageDestinationGetTypeID: CFTypeID; external name '_CGImageDestinationGetTypeID';
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)

{ Return an array of supported type identifiers. }

function CGImageDestinationCopyTypeIdentifiers: CFArrayRef; external name '_CGImageDestinationCopyTypeIdentifiers';
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)

{ Create an image destination writing to the data consumer `consumer'.
 * The parameter `type' specifies the type identifier of the resulting
 * image file.  The parameter `count' specifies number of images (not
 * including thumbnails) that the image file will contain. The `options'
 * dictionary is reserved for future use; currently, you should pass NULL
 * for this parameter. }

function CGImageDestinationCreateWithDataConsumer( consumer: CGDataConsumerRef; typ: CFStringRef; count: size_t; options: CFDictionaryRef ): CGImageDestinationRef; external name '_CGImageDestinationCreateWithDataConsumer';
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)

{ Create an image destination writing to `data'. The parameter `type'
 * specifies the type identifier of the resulting image file.  The
 * parameter `count' specifies number of images (not including thumbnails)
 * that the image file will contain. The `options' dictionary is reserved
 * for future use; currently, you should pass NULL for this parameter. }

function CGImageDestinationCreateWithData( data: CFMutableDataRef; typ: CFStringRef; count: size_t; options: CFDictionaryRef ): CGImageDestinationRef; external name '_CGImageDestinationCreateWithData';
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)

{ Create an image destination writing to `url'. The parameter `type'
 * specifies the type identifier of the resulting image file.  The
 * parameter `count' specifies number of images (not including thumbnails)
 * that the image file will contain. The `options' dictionary is reserved
 * for future use; currently, you should pass NULL for this parameter.
 * Note that if `url' already exists, it will be overwritten. }

function CGImageDestinationCreateWithURL( url: CFURLRef; typ: CFStringRef; count: size_t; options: CFDictionaryRef ): CGImageDestinationRef; external name '_CGImageDestinationCreateWithURL';
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)

{ Specify the dictionary `properties' of properties which apply to all
 * images in the image destination `idst'. }

procedure CGImageDestinationSetProperties( idst: CGImageDestinationRef; properties: CFDictionaryRef ); external name '_CGImageDestinationSetProperties';
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)

{ Set the next image in the image destination `idst' to be `image' with
 * optional properties specified in `properties'.  An error is logged if
 * more images are added than specified in the original count of the image
 * destination. }

procedure CGImageDestinationAddImage( idst: CGImageDestinationRef; image: CGImageRef; properties: CFDictionaryRef ); external name '_CGImageDestinationAddImage';
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)

{ Set the next image in the image destination `idst' to be the image at
 * `index' in the image source `isrc'.  The index is zero-based. The
 * properties of the source image can be added to or overriden by supplying
 * additional keys/values in `properties'.  If a key in `properties' has
 * the value kCFNull, the corresponding property in the destination will be
 * removed. }

procedure CGImageDestinationAddImageFromSource( idst: CGImageDestinationRef; isrc: CGImageSourceRef; index: size_t; properties: CFDictionaryRef ); external name '_CGImageDestinationAddImageFromSource';
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)

{ Write everything to the destination data, url or consumer of the image
 * destination `idst'.  You must call this function or the image
 * destination will not be valid.  After this function is called, no
 * additional data will be written to the image destination.  Return true
 * if the image was successfully written; false otherwise. }

function CGImageDestinationFinalize( idst: CGImageDestinationRef ): CBool; external name '_CGImageDestinationFinalize';
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)

{$ifc TARGET_OS_MAC}
{ Set the next image in the image destination `idst' to be `image' with
 * metadata properties specified in `metadata'. An error is logged if more
 * images are added than specified in the original count of the image
 * destination. }
procedure CGImageDestinationAddImageAndMetadata( idst: CGImageDestinationRef; image: CGImageRef; metadata: CGImageMetadataRef; options: CFDictionaryRef ); external name '_CGImageDestinationAddImageAndMetadata';
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_8, __IPHONE_NA) *)

{*
 ** Keys which may be used in the 'options' dictionary of
 ** "CGImageDestinationCopyImageSource" to effect the output.
 *}

{ Set the metadata tags for the image destination. If present, the value of
 * this key is a CGImageMetadataRef. By default, all EXIF, IPTC, and XMP tags
 * will be replaced. Use kCGImageDestinationMergeMetadata to merge the tags
 * with the existing tags in the image source.
 }
var kCGImageDestinationMetadata: CFStringRef; external name '_kCGImageDestinationMetadata'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_8, __IPHONE_NA) *)

{ If true, The metadata will be copied from the source and merged with the tags
 * specified in kCGImageDestinationMetadata. If a tag does not exist in the 
 * source, it will be added. If the tag exists in the source, it will be 
 * updated. A metadata tag can be removed by setting the tag's value to 
 * kCFNull. If present, the value of this key is a CFBoooleanRef. The default
 * is kCFBooleanFalse.
 } 
var kCGImageDestinationMergeMetadata: CFStringRef; external name '_kCGImageDestinationMergeMetadata'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_8, __IPHONE_NA) *)

{ XMP data will not be written to the destination. If used in conjunction with 
 * kCGImageDestinationMetadata, EXIF and/or IPTC tags will be preserved, but 
 * an XMP packet will not be written to the file. If present, the value for 
 * this key is a CFBooleanRef. The default is kCFBooleanFalse.
 }
var kCGImageMetadataShouldExcludeXMP: CFStringRef; external name '_kCGImageMetadataShouldExcludeXMP'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_8, __IPHONE_NA) *)

{ Updates the DateTime parameters of the image metadata. Only values
 * present in the original image will updated. If present, the value should
 * be a CFStringRef or a CFDateRef. If CFString, the value must be in 
 * Exif DateTime or ISO 8601 DateTime format. This option is mutually
 * exclusive with kCGImageDestinationMetadata.
 }
var kCGImageDestinationDateTime: CFStringRef; external name '_kCGImageDestinationDateTime'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_8, __IPHONE_NA) *)

{ Updates the orientation in the image metadata. The image data itself will
 * not be rotated. If present, the value should be a CFNumberRef from 1 to 8. 
 * This option is mutually exclusive with kCGImageDestinationMetadata.
 }
var kCGImageDestinationOrientation: CFStringRef; external name '_kCGImageDestinationOrientation'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_8, __IPHONE_NA) *)

{ Losslessly copies the contents of the image source, 'isrc', to the 
 * destination, 'idst'. The image data will not be modified. The image's 
 * metadata can be modified by adding the keys and values defined above to 
 * 'options'. No other images should be added to the image destination. 
 * CGImageDestinationFinalize() should not be called afterward -
 * the result is saved to the destination when this function returns. 
 * The image type of the destination must match the image source. Returns true
 * if the operation was successful. If an error occurs, false will be returned 
 * and 'err' will be set to a CFErrorRef. Not all image formats are supported 
 * for this operation. }
function CGImageDestinationCopyImageSource( idst: CGImageDestinationRef; isrc: CGImageSourceRef; options: CFDictionaryRef; var err: CFErrorRef ): CBool; external name '_CGImageDestinationCopyImageSource';
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_8, __IPHONE_NA) *)

{$endc} {TARGET_OS_MAC}
{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}
