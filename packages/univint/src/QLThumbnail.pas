{
 *  QLThumbnail.h
 *  Quick Look
 *
 *  Copyright 2007-2010 Apple Inc.
 *  All rights reserved.
 *
 }
{ Initial Pascal Translation: Jonas Maebe <jonas@freepascal.org>, October 2012 }
{
    Modified for use with Free Pascal
    Version 308
    Please report any bugs to <gpc@microbizz.nl>
}

{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}
{$mode macpas}
{$modeswitch cblocks}
{$packenum 1}
{$macro on}
{$inline on}
{$calling mwpascal}

unit QLThumbnail;
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
{$ifc defined iphonesim}
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
{$ifc defined iphonesim}
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
{$ifc defined ios}
	{$setc TARGET_OS_MAC := FALSE}
	{$setc TARGET_OS_IPHONE := TRUE}
	{$setc TARGET_OS_EMBEDDED := TRUE}
{$elsec}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_OS_EMBEDDED := FALSE}
{$endc}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
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
uses MacTypes,CFBase,CFURL,CFDictionary,CGGeometry,CGImage,QLBase,QLThumbnailImage;
{$endc} {not MACOSALLINCLUDE}


{$ifc TARGET_OS_MAC}
{$ALIGN POWER}



type
	QLThumbnailRef = ^__QLThumbnail; { an opaque type }
	__QLThumbnail = record end;

function QLThumbnailGetTypeID: CFTypeID; external name '_QLThumbnailGetTypeID';

{
 @function QLThumbnailCreate
 @abstract Creates a thumbnail instance. The thumbnail image will be computed in background.
 @param allocator The allocator to use to create the instance.
 @param url The URL of the document to thumbnail.
 @param maxThumbnailSize Maximum size (in points) allowed for the thumbnail image.
 @param options Optional hints for the thumbnail. (Only kQLThumbnailOptionScaleFactorKey is available for now)
 @result The thumbnail instance.
 }
function QLThumbnailCreate( allocator: CFAllocatorRef; url: CFURLRef; maxThumbnailSize: CGSize; options: CFDictionaryRef ): QLThumbnailRef; external name '_QLThumbnailCreate';
(* AVAILABLE_MAC_OS_X_VERSION_10_6_AND_LATER *)

{
 @function QLThumbnailCopyDocumentURL
 @abstract Returns the URL of the document to thumbnail.
 @param thumbnail The thumbnail to compute.
 }
function QLThumbnailCopyDocumentURL( thumbnail: QLThumbnailRef ): CFURLRef; external name '_QLThumbnailCopyDocumentURL';
(* AVAILABLE_MAC_OS_X_VERSION_10_6_AND_LATER *)

{
 @function QLThumbnailGetMaximumSize
 @abstract Returns the maximum size (in points) allowed for the thumbnail image.
 @param thumbnail The thumbnail to compute.
 }
function QLThumbnailGetMaximumSize( thumbnail: QLThumbnailRef ): CGSize; external name '_QLThumbnailGetMaximumSize';
(* AVAILABLE_MAC_OS_X_VERSION_10_6_AND_LATER *)

{
 @function QLThumbnailCopyOptions
 @abstract Returns the options for the thumbnail.
 @param thumbnail The thumbnail to compute.
 }
function QLThumbnailCopyOptions( thumbnail: QLThumbnailRef ): CFDictionaryRef; external name '_QLThumbnailCopyOptions';
(* AVAILABLE_MAC_OS_X_VERSION_10_6_AND_LATER *)

{$ifdef BLOCKS_SUPPORT}
{
 @function QLThumbnailDispatchAsync
 @abstract Start computing thumbnail in background.
 @param thumbnail The thumbnail to compute.
 @param queue Where the completion block will be dispatched.
 @param completion The completion block called upon thumbnail completion.
 @discussion The completion block will always be called, even if the thumbnail computation has been cancelled.
 }
procedure QLThumbnailDispatchAsync( thumbnail: QLThumbnailRef; queue: dispatch_queue_t; completion: dispatch_block_t ); external name '_QLThumbnailDispatchAsync';
(* AVAILABLE_MAC_OS_X_VERSION_10_6_AND_LATER *)
{$endif}

{
 @function QLThumbnailCopyImage
 @abstract Returns the image computed by Quick Look or NULL if not thumbnail was created.
 @param thumbnail The thumbnail to compute.
 @result The thumbnail image or NULL.
 @discussion If called without a previous of QLThumbnailDispatchAsync(), the call will block until the thumbnail is computed.
             QLThumbnailCopyImage() should not be called during async dispatch (before the completion block has been called)
 }
function QLThumbnailCopyImage( thumbnail: QLThumbnailRef ): CGImageRef; external name '_QLThumbnailCopyImage';
(* AVAILABLE_MAC_OS_X_VERSION_10_6_AND_LATER *)

{
 @function QLThumbnailGetContentRect
 @abstract Returns the the effective rect within the thumbnail image representing the
           content of the document. In icon mode, this is the part of the image without
           all the image decorations.
 @param thumbnail The thumbnail to compute.
 @result The content rect of the thumbnail expressed in pixel coordinates.
 }
function QLThumbnailGetContentRect( thumbnail: QLThumbnailRef ): CGRect; external name '_QLThumbnailGetContentRect';
(* AVAILABLE_MAC_OS_X_VERSION_10_6_AND_LATER *)


{
 @function QLThumbnailCancel
 @abstract Cancels the computation of the thumbnail.
 @param thumbnail The thumbnail to compute.
 @discussion If used with QLThumbnailDispatchAsync() the completion callback will be called.
             If used in synchronous mode, QLThumbnailCopyImage() will return immediately NULL.
 }
procedure QLThumbnailCancel( thumbnail: QLThumbnailRef ); external name '_QLThumbnailCancel';
(* AVAILABLE_MAC_OS_X_VERSION_10_6_AND_LATER *)

{
 @function QLThumbnailIsCancelled
 @abstract Returns wether the thumbnail computation has been cancelled.
 @param thumbnail The thumbnail to compute.
 }
function QLThumbnailIsCancelled( thumbnail: QLThumbnailRef ): Boolean; external name '_QLThumbnailIsCancelled';
(* AVAILABLE_MAC_OS_X_VERSION_10_6_AND_LATER *)


{$endc} {TARGET_OS_MAC}
{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}
