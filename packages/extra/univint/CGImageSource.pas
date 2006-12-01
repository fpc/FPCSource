{
 * ImageIO - CGImageSource.h
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

unit CGImageSource;
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
uses MacTypes, CFArray, CFBase, CFData, CFDictionary, CFURL, CGDataProvider, CGImage;
{$ALIGN POWER}


type
	CGImageSourceRef = ^SInt32; { an opaque 32-bit type }


type
	CGImageSourceStatus = SInt32;
const
	kCGImageStatusUnexpectedEOF = -5;
	kCGImageStatusInvalidData = -4;
	kCGImageStatusUnknownType = -3;
	kCGImageStatusReadingHeader = -2;
	kCGImageStatusIncomplete = -1;
	kCGImageStatusComplete = 0;

{* Keys for the options dictionary when creating a CGImageSourceRef. *}

{ Specifies the "best guess" of the type identifier for the format of the
 * image source file. If specified, the value of this key must be a
 * CFStringRef. For more information about type identifiers, see "UTType.h"
 * in the Application Services framework. }

var kCGImageSourceTypeIdentifierHint: CFStringRef; external name '_kCGImageSourceTypeIdentifierHint'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{* Keys for the options dictionary of "CGImageSourceCopyPropertiesAtIndex"
 ** and "CGImageSourceCreateImageAtIndex". *}

{ Specifies whether the image should be cached in a decoded form. The
 * value of this key must be a CFBooleanRef; the default value is
 * kCFBooleanFalse. }

var kCGImageSourceShouldCache: CFStringRef; external name '_kCGImageSourceShouldCache'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Specifies whether the image should be returned as a floating
 * point CGImageRef if supported by the file format. Extended
 * range floating point CGImageRef may require additional
 * processing  to render pleasingly.  The value of this key must
 * be a CFBooleanRef; the default value is kCFBooleanFalse. }

var kCGImageSourceShouldAllowFloat: CFStringRef; external name '_kCGImageSourceShouldAllowFloat'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{* Keys for the options dictionary of
 ** "CGImageSourceCreateThumbnailAtIndex". *}

{ Specifies whether a thumbnail should be automatically created for an
 * image if a thumbnail isn't present in the image source file.  The
 * thumbnail will be created from the full image, subject to the limit
 * specified by kCGImageSourceThumbnailMaxPixelSize---if a maximum pixel
 * size isn't specified, then the thumbnail will be the size of the full
 * image, which probably isn't what you want. The value of this key must be
 * a CFBooleanRef; the default value of this key is kCFBooleanFalse. }

var kCGImageSourceCreateThumbnailFromImageIfAbsent: CFStringRef; external name '_kCGImageSourceCreateThumbnailFromImageIfAbsent'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Specifies whether a thumbnail should be created from the full image even
 * if a thumbnail is present in the image source file. The thumbnail will
 * be created from the full image, subject to the limit specified by
 * kCGImageSourceThumbnailMaxPixelSize---if a maximum pixel size isn't
 * specified, then the thumbnail will be the size of the full image, which
 * probably isn't what you want. The value of this key must be a
 * CFBooleanRef; the default value of this key is kCFBooleanFalse. }

var kCGImageSourceCreateThumbnailFromImageAlways: CFStringRef; external name '_kCGImageSourceCreateThumbnailFromImageAlways'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Specifies the maximum width and height in pixels of a thumbnail.  If
 * this this key is not specified, the width and height of a thumbnail is
 * not limited and thumbnails may be as big as the image itself.  If
 * present, this value of this key must be a CFNumberRef. }

var kCGImageSourceThumbnailMaxPixelSize: CFStringRef; external name '_kCGImageSourceThumbnailMaxPixelSize'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Specifies whether the thumbnail should be rotated and scaled according
 * to the orientation and pixel aspect ratio of the full image. The value
 * of this key must be a CFBooleanRef; the default value of this key is 
 * kCFBooleanFalse. }

var kCGImageSourceCreateThumbnailWithTransform: CFStringRef; external name '_kCGImageSourceCreateThumbnailWithTransform'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Return the CFTypeID for CGImageSources. }

function CGImageSourceGetTypeID: CFTypeID; external name '_CGImageSourceGetTypeID';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Return an array of supported type identifiers. }

function CGImageSourceCopyTypeIdentifiers: CFArrayRef; external name '_CGImageSourceCopyTypeIdentifiers';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Create an image source reading from the data provider `provider'. The
 * `options' dictionary may be used to request additional creation options;
 * see the list of keys above for more information. }

function CGImageSourceCreateWithDataProvider( provider: CGDataProviderRef; options: CFDictionaryRef ): CGImageSourceRef; external name '_CGImageSourceCreateWithDataProvider';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Create an image source reading from `data'.  The `options' dictionary
 * may be used to request additional creation options; see the list of keys
 * above for more information. }

function CGImageSourceCreateWithData( data: CFDataRef; options: CFDictionaryRef ): CGImageSourceRef; external name '_CGImageSourceCreateWithData';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Create an image source reading from `url'. The `options' dictionary may
 * be used to request additional creation options; see the list of keys
 * above for more information. }

function CGImageSourceCreateWithURL( url: CFURLRef; options: CFDictionaryRef ): CGImageSourceRef; external name '_CGImageSourceCreateWithURL';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Return the type identifier of the image source `isrc'.  This type is the
 * type of the source "container", which is not necessarily the type of the
 * image(s) in the container.  For example, the .icns format supports
 * embedded JPEG2000 but the source type will be "com.apple.icns". }

function CGImageSourceGetType( isrc: CGImageSourceRef ): CFStringRef; external name '_CGImageSourceGetType';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Return the number of images (not including thumbnails) in the image
 * source `isrc'. }

function CGImageSourceGetCount( isrc: CGImageSourceRef ): size_t; external name '_CGImageSourceGetCount';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Return the properties of the image source `isrc'.  These properties
 * apply to the container in general but not necessarily to any individual
 * image that it contains. }

function CGImageSourceCopyProperties( isrc: CGImageSourceRef; options: CFDictionaryRef ): CFDictionaryRef; external name '_CGImageSourceCopyProperties';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Return the properties of the image at `index' in the image source
 * `isrc'.  The index is zero-based. The `options' dictionary may be used
 * to request additional options; see the list of keys above for more
 * information. }

function CGImageSourceCopyPropertiesAtIndex( isrc: CGImageSourceRef; index: size_t; options: CFDictionaryRef ): CFDictionaryRef; external name '_CGImageSourceCopyPropertiesAtIndex';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Return the image at `index' in the image source `isrc'.  The index is
 * zero-based. The `options' dictionary may be used to request additional
 * creation options; see the list of keys above for more information. }

function CGImageSourceCreateImageAtIndex( isrc: CGImageSourceRef; index: size_t; options: CFDictionaryRef ): CGImageRef; external name '_CGImageSourceCreateImageAtIndex';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Return the thumbnail of the image at `index' in the image source `isrc'.
 * The index is zero-based. The `options' dictionary may be used to request
 * additional thumbnail creation options; see the list of keys above for
 * more information. }

function CGImageSourceCreateThumbnailAtIndex( isrc: CGImageSourceRef; index: size_t; options: CFDictionaryRef ): CGImageRef; external name '_CGImageSourceCreateThumbnailAtIndex';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Create an incremental image source. No data is provided at creation
 * time; it is assumed that data will eventually be provided using
 * "CGImageSourceUpdateDataProvider" or "CGImageSourceUpdateData".  The
 * `options' dictionary may be used to request additional creation options;
 * see the list of keys above for more information. }

function CGImageSourceCreateIncremental( options: CFDictionaryRef ): CGImageSourceRef; external name '_CGImageSourceCreateIncremental';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Update the incremental image source `isrc' with new data.  The new data
 * must include all the previous data plus any additional new data. The
 * `final' parameter should be true when the final set of data is provided;
 * false otherwise. }

procedure CGImageSourceUpdateData( isrc: CGImageSourceRef; data: CFDataRef; final: CBool ); external name '_CGImageSourceUpdateData';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Update the incremental image source `isrc' with a new data provider.
 * The new data provider must provide all the previous data plus any
 * additional new data. The `final' parameter should be true when the final
 * set of data is provided; false otherwise. }

procedure CGImageSourceUpdateDataProvider( isrc: CGImageSourceRef; provider: CGDataProviderRef; final: CBool ); external name '_CGImageSourceUpdateDataProvider';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Return the overall status of the image source `isrc'.  The status is
 * particularly informative for incremental image sources, but may be used
 * by clients providing non-incremental data as well. }

function CGImageSourceGetStatus( isrc: CGImageSourceRef ): CGImageSourceStatus; external name '_CGImageSourceGetStatus';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Return the current status of the image at `index' in the image source
 * `isrc'. The index is zero-based. The returned status is particularly
 * informative for incremental image sources but may used by clients
 * providing non-incremental data as well. }

function CGImageSourceGetStatusAtIndex( isrc: CGImageSourceRef; index: size_t ): CGImageSourceStatus; external name '_CGImageSourceGetStatusAtIndex';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


end.
