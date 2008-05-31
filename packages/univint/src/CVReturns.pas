{
 *  CVReturn.h
 *  CoreVideo
 *
 *  Copyright (c) 2004 Apple Computer, Inc. All rights reserved.
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

unit CVReturns;
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
uses MacTypes;
{$ALIGN POWER}

 
 {! @header CVReturn.h
	@copyright 2004 Apple Computer, Inc. All rights reserved.
	@availability Mac OS X 10.4 or later
    @discussion Here you can find all the CoreVideo specific error codes. 
		   
}


{!
    @enum	CVReturn
    @abstract   CoreVideo specific error codes

    @constant   kCVReturnSuccess Function executed successfully without errors.
    @constant   kCVReturnFirst Placeholder to mark the beginning of the range of CVReturn codes.
    @constant   kCVReturnLast Placeholder to mark the end of the range of CVReturn codes.
 
    @constant   kCVReturnInvalidArgument At least one of the arguments passed in is not valid. Either out of range or the wrong type.
    @constant   kCVReturnAllocationFailed The allocation for a buffer or buffer pool failed. Most likely because of lack of resources.

    @constant   kCVReturnInvalidDisplay A CVDisplayLink cannot be created for the given DisplayRef.
    @constant   kCVReturnDisplayLinkAlreadyRunning The CVDisplayLink is already started and running.
    @constant   kCVReturnDisplayLinkNotRunning The CVDisplayLink has not been started.
    @constant   kCVReturnDisplayLinkCallbacksNotSet The render and display callbacks or the output callback is not set. You have to set either the render/display pair or the single output callback.

    @constant   kCVReturnInvalidPixelFormat The requested pixelformat is not supported for the CVBuffer type.
    @constant   kCVReturnInvalidSize The requested size (most likely too big) is not supported for the CVBuffer type.
    @constant   kCVReturnInvalidPixelBufferAttributes A CVBuffer cannot be created with the given attributes.
    @constant   kCVReturnPixelBufferNotOpenGLCompatible The Buffer cannot be used with OpenGL as either its size, pixelformat or attributes are not supported by OpenGL.

    @constant   kCVReturnPoolAllocationFailed The allocation for the buffer pool failed. Most likely because of lack of resources. Check if your parameters are in range.
    @constant   kCVReturnInvalidPoolAttributes A CVBufferPool cannot be created with the given attributes.
}

type
	CVReturn = SInt32;
const
	kCVReturnSuccess          = 0;
	kCVReturnFirst            = -6660;
	kCVReturnError            = kCVReturnFirst;
	kCVReturnInvalidArgument  = -6661;
	kCVReturnAllocationFailed = -6662;
    
    // DisplayLink related errors
	kCVReturnInvalidDisplay             = -6670;
	kCVReturnDisplayLinkAlreadyRunning  = -6671;
	kCVReturnDisplayLinkNotRunning      = -6672;
	kCVReturnDisplayLinkCallbacksNotSet = -6673;
    
    // Buffer related errors
	kCVReturnInvalidPixelFormat             = -6680;
	kCVReturnInvalidSize                    = -6681;
	kCVReturnInvalidPixelBufferAttributes   = -6682;
	kCVReturnPixelBufferNotOpenGLCompatible = -6683;
    
    // Buffer Pool related errors
	kCVReturnPoolAllocationFailed  = -6690;
	kCVReturnInvalidPoolAttributes = -6691;
	kCVReturnLast                  = -6699;


end.
