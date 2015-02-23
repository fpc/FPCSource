{
 *  CVBuffer.h
 *  CoreVideo
 *
 *  Copyright (c) 2004 Apple Computer, Inc. All rights reserved.
 *
 }
 
{  Pascal Translation:  Gale R Paeper, <gpaeper@empirenet.com>, 2008 }
{  Pascal Translation Update:  Gorazd Krosl, <gorazd_1957@yahoo.ca>, 2009 }
{  Pascal Translation Update: Jonas Maebe <jonas@freepascal.org>, October 2012 }

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

unit CVBuffer;
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
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
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
uses MacTypes, CFBase, CFDictionary, CVBase, CVReturns;
{$endc} {not MACOSALLINCLUDE}


{$ALIGN POWER}

 
 {! @header CVBuffer.h
	@copyright 2004 Apple Computer, Inc. All rights reserved.
	@availability Mac OS X 10.4 or later
    @discussion CVBufferRef types are abstract and only define ways to attach meta data to buffers (such as timestamps,
	        colorspace information, etc.).    CVBufferRefs do not imply any particular kind of data storage.  It could
		be compressed data, image data, etc.
		   
}


//#pragma mark CVBufferRef attribute keys

{ The following two keys are useful with the CoreVideo pool and texture cache APIs so that you can specify
   an initial set of default buffer attachments to automatically be attached to the buffer when it is created. }
var kCVBufferPropagatedAttachmentsKey: CFStringRef; external name '_kCVBufferPropagatedAttachmentsKey'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4,__IPHONE_4_0) *)
var kCVBufferNonPropagatedAttachmentsKey: CFStringRef; external name '_kCVBufferNonPropagatedAttachmentsKey'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4,__IPHONE_4_0) *)

//#pragma mark CVBufferRef attachment keys

var kCVBufferMovieTimeKey: CFStringRef; external name '_kCVBufferMovieTimeKey'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4,__IPHONE_4_0) *)	// Generally only available for frames emitted by QuickTime; CFDictionary containing these two keys:
var kCVBufferTimeValueKey: CFStringRef; external name '_kCVBufferTimeValueKey'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4,__IPHONE_4_0) *)
var kCVBufferTimeScaleKey: CFStringRef; external name '_kCVBufferTimeScaleKey'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4,__IPHONE_4_0) *)


//#pragma mark CVBufferRef

const
	kCVAttachmentMode_ShouldNotPropagate = 0;
	kCVAttachmentMode_ShouldPropagate    = 1;
type
	CVAttachmentMode = UInt32;

{!
    @typedef	CVBufferRef
    @abstract   Base type for all CoreVideo buffers

}
type
	CVBufferRef = ^__CVBuffer; { an opaque type }
	__CVBuffer = record end;

{!
    @function   CVBufferRetain
    @abstract   Retains a CVBuffer object
    @discussion Like CFRetain CVBufferRetain increments the retain count of a CVBuffer object. In contrast to the CF call it is NULL safe.
    @param      buffer A CVBuffer object that you want to retain.
    @result     A CVBuffer object that is the same as the passed in buffer.
}
function CVBufferRetain( buffer: CVBufferRef ): CVBufferRef; external name '_CVBufferRetain';
(* __OSX_AVAILABLE_STARTING(__MAC_10_4,__IPHONE_4_0) *)
{!
    @function   CVBufferRelease
    @abstract   Release a CVBuffer object
    @discussion Like CFRetain CVBufferRetain decrements the retain count of a CVBuffer object. If that count consequently becomes zero the memory allocated to the object is deallocated and the object is destroyed. In contrast to the CF call it is NULL safe.
    @param      buffer A CVBuffer object that you want to release.
}
procedure CVBufferRelease( buffer: CVBufferRef ); external name '_CVBufferRelease';
(* __OSX_AVAILABLE_STARTING(__MAC_10_4,__IPHONE_4_0) *)

//#pragma mark CVBufferAttachment

{!
    @function   CVBufferSetAttachment
    @abstract   Sets or adds a attachment of a CVBuffer object
    @discussion You can attach any CF object to a CVBuffer object to store additional information. CVBufferGetAttachment stores an attachement identified by a key. If the key doesn't exist, the attachment will be added. If the key does exist, the existing attachment will be replaced. In bouth cases the retain count of the attachment will be incremented. The value can be any CFType but nil has no defined behavior.
    @param      buffer  Target CVBuffer object.
    @param      key     Key in form of a CFString identifying the desired attachment.
    @param      value	Attachment in form af a CF object.
    @param      attachmentMode	Specifies which attachment mode is desired for this attachment.   A particular attachment key may only exist in
                                a single mode at a time.
}
procedure CVBufferSetAttachment( buffer: CVBufferRef; key: CFStringRef; value: CFTypeRef; attachmentMode: CVAttachmentMode ); external name '_CVBufferSetAttachment';
(* __OSX_AVAILABLE_STARTING(__MAC_10_4,__IPHONE_4_0) *)


{!
    @function   CVBufferGetAttachment
    @abstract   Returns a specific attachment of a CVBuffer object
    @discussion You can attach any CF object to a CVBuffer object to store additional information. CVBufferGetAttachment retrieves an attachement identified by a key.
    @param      buffer  Target CVBuffer object.
    @param      key	Key in form of a CFString identifying the desired attachment.
    @param      attachmentMode.  Returns the mode of the attachment, if desired.  May be NULL.
    @result     If found the attachment object
}
function CVBufferGetAttachment( buffer: CVBufferRef; key: CFStringRef; var attachmentMode: CVAttachmentMode ): CFTypeRef; external name '_CVBufferGetAttachment';
(* __OSX_AVAILABLE_STARTING(__MAC_10_4,__IPHONE_4_0) *)

{!
    @function   CVBufferRemoveAttachment
    @abstract   Removes a specific attachment of a CVBuffer object
    @discussion CVBufferRemoveAttachment removes an attachement identified by a key. If found the attachement is removed and the retain count decremented.
    @param      buffer  Target CVBuffer object.
    @param      key	Key in form of a CFString identifying the desired attachment.
}
procedure CVBufferRemoveAttachment( buffer: CVBufferRef; key: CFStringRef ); external name '_CVBufferRemoveAttachment';
(* __OSX_AVAILABLE_STARTING(__MAC_10_4,__IPHONE_4_0) *)

{!
    @function   CVBufferRemoveAllAttachments
    @abstract   Removes all attachments of a CVBuffer object
    @discussion While CVBufferRemoveAttachment removes a specific attachement identified by a key CVBufferRemoveAllAttachments removes all attachments of a buffer and decrements their retain counts.
    @param      buffer  Target CVBuffer object.
}
procedure CVBufferRemoveAllAttachments( buffer: CVBufferRef ); external name '_CVBufferRemoveAllAttachments';
(* __OSX_AVAILABLE_STARTING(__MAC_10_4,__IPHONE_4_0) *)

{!
    @function   CVBufferGetAttachments
    @abstract   Returns all attachments of a CVBuffer object
    @discussion CVBufferGetAttachments is a convenience call that returns all attachments with their corresponding keys in a CFDictionary.
    @param      buffer  Target CVBuffer object.
    @result     A CFDictionary with all buffer attachments identified by there keys. If no attachment is present, the dictionary is empty.  Returns NULL
		for invalid attachment mode.
}
function CVBufferGetAttachments( buffer: CVBufferRef; attachmentMode: CVAttachmentMode ): CFDictionaryRef; external name '_CVBufferGetAttachments';
(* __OSX_AVAILABLE_STARTING(__MAC_10_4,__IPHONE_4_0) *)

{!
    @function   CVBufferSetAttachments
    @abstract   Sets a set of attachments for a CVBuffer
    @discussion CVBufferSetAttachments is a convenience call that in turn calls CVBufferSetAttachment for each key and value in the given dictionary. All key value pairs must be in the root level of the dictionary.
    @param      buffer  Target CVBuffer object.
}
procedure CVBufferSetAttachments( buffer: CVBufferRef; theAttachments: CFDictionaryRef; attachmentMode: CVAttachmentMode ); external name '_CVBufferSetAttachments';
(* __OSX_AVAILABLE_STARTING(__MAC_10_4,__IPHONE_4_0) *)

{!
    @function   CVBufferPropagateAttachments
    @abstract   Copy all propagatable attachments from one buffer to another.
    @discussion CVBufferPropagateAttachments is a convenience call that copies all attachments with a mode of kCVAttachmentMode_ShouldPropagate from one
                buffer to another.
    @param      sourceBuffer  CVBuffer to copy attachments from.
    @param      destinationBuffer  CVBuffer to copy attachments to.
}
procedure CVBufferPropagateAttachments( sourceBuffer: CVBufferRef; destinationBuffer: CVBufferRef ); external name '_CVBufferPropagateAttachments';
(* __OSX_AVAILABLE_STARTING(__MAC_10_4,__IPHONE_4_0) *)

{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}
