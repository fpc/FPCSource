{
 *  CVPixelBuffer.h
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

unit CVPixelBuffer;
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
uses MacTypes, CFArray, CFBase, CFDictionary, CVBase, CVImageBuffer, CVReturns;
{$ALIGN POWER}

 
  {! @header CVPixelBuffer.h
	@copyright 2004 Apple Computer, Inc. All rights reserved.
	@availability Mac OS X 10.4 or later
    @discussion CVPixelBuffers are CVImageBuffers that hold the pixels in main memory
		   
}


//#pragma mark BufferAttributeKeys
var kCVPixelBufferPixelFormatTypeKey: CFStringRef; external name '_kCVPixelBufferPixelFormatTypeKey'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)		    // A single CFNumber or a CFArray of CFNumbers (OSTypes)
var kCVPixelBufferMemoryAllocatorKey: CFStringRef; external name '_kCVPixelBufferMemoryAllocatorKey'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)		    // CFAllocatorRef
var kCVPixelBufferWidthKey: CFStringRef; external name '_kCVPixelBufferWidthKey'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)			    // CFNumber
var kCVPixelBufferHeightKey: CFStringRef; external name '_kCVPixelBufferHeightKey'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)			    // CFNumber
var kCVPixelBufferExtendedPixelsLeftKey: CFStringRef; external name '_kCVPixelBufferExtendedPixelsLeftKey'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)	    // CFNumber
var kCVPixelBufferExtendedPixelsTopKey: CFStringRef; external name '_kCVPixelBufferExtendedPixelsTopKey'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)		    // CFNumber
var kCVPixelBufferExtendedPixelsRightKey: CFStringRef; external name '_kCVPixelBufferExtendedPixelsRightKey'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)	    // CFNumber
var kCVPixelBufferExtendedPixelsBottomKey: CFStringRef; external name '_kCVPixelBufferExtendedPixelsBottomKey'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)	    // CFNumber
var kCVPixelBufferBytesPerRowAlignmentKey: CFStringRef; external name '_kCVPixelBufferBytesPerRowAlignmentKey'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)	    // CFNumber
var kCVPixelBufferCGBitmapContextCompatibilityKey: CFStringRef; external name '_kCVPixelBufferCGBitmapContextCompatibilityKey'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)  // CFBoolean
var kCVPixelBufferCGImageCompatibilityKey: CFStringRef; external name '_kCVPixelBufferCGImageCompatibilityKey'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)	    // CFBoolean
var kCVPixelBufferOpenGLCompatibilityKey: CFStringRef; external name '_kCVPixelBufferOpenGLCompatibilityKey'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)	    // CFBoolean

{!
    @typedef	CVPixelBufferRef
    @abstract   Based on the image buffer type. The pixel buffer implements the memory storage for an image buffer.

}
type
	CVPixelBufferRef = CVImageBufferRef;

function CVPixelBufferGetTypeID: CFTypeID; external name '_CVPixelBufferGetTypeID';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{!
    @function   CVPixelBufferRetain
    @abstract   Retains a CVPixelBuffer object
    @discussion Equivalent to CFRetain, but NULL safe
    @param      buffer A CVPixelBuffer object that you want to retain.
    @result     A CVPixelBuffer object that is the same as the passed in buffer.
}
function CVPixelBufferRetain( texture: CVPixelBufferRef ): CVPixelBufferRef; external name '_CVPixelBufferRetain';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{!
    @function   CVPixelBufferRelease
    @abstract   Releases a CVPixelBuffer object
    @discussion Equivalent to CFRelease, but NULL safe
    @param      buffer A CVPixelBuffer object that you want to release.
}
procedure CVPixelBufferRelease( texture: CVPixelBufferRef ); external name '_CVPixelBufferRelease';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{!
    @function   CVPixelBufferCreateResolvedAttributesDictionary
    @abstract   Takes a CFArray of CFDictionary objects describing various pixel buffer attributes and tries to resolve them into a
                single dictionary.
    @discussion This is useful when you need to resolve multiple requirements between different potential clients of a buffer.
    @param      attributes CFArray of CFDictionaries containing kCVPixelBuffer key/value pairs.
    @param      resolvedDictionaryOut The resulting dictionary will be placed here.
    @result     Return value that may be useful in discovering why resolution failed.
}
function CVPixelBufferCreateResolvedAttributesDictionary( allocator: CFAllocatorRef; attributes: CFArrayRef; var resolvedDictionaryOut: CFDictionaryRef ): CVReturn; external name '_CVPixelBufferCreateResolvedAttributesDictionary';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{!
    @function   CVPixelBufferCreate
    @abstract   Call to create a single PixelBuffer for a given size and pixelFormatType.
    @discussion Creates a single PixelBuffer for a given size and pixelFormatType. It allocates the necessary memory based on the pixel dimensions, pixelFormatType and extended pixels described in the pixelBufferAttributes. Not all parameters of the pixelBufferAttributes will be used here.
    @param      width   Width of the PixelBuffer in pixels.
    @param      height  Height of the PixelBuffer in pixels.
    @param	pixelFormatType		Pixel format indentified by its respective OSType.
    @param	pixelBufferAttributes      A dictionary with additonal attributes for a a pixel buffer. This parameter is optional. See PixelBufferAttributes for more details.
    @param      pixelBufferOut          The new pixel buffer will be returned here
    @result	returns kCVReturnSuccess on success.
}    
function CVPixelBufferCreate( allocator: CFAllocatorRef; width: size_t; height: size_t; pixelFormatType: OSType; pixelBufferAttributes: CFDictionaryRef; var pixelBufferOut: CVPixelBufferRef ): CVReturn; external name '_CVPixelBufferCreate';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

type
	CVPixelBufferReleaseBytesCallback = procedure( releaseRefCon: UnivPtr; baseAddress: {const} UnivPtr );

{!
    @function   CVPixelBufferCreateWithBytes
    @abstract   Call to create a single PixelBuffer for a given size and pixelFormatType based on a passed in piece of memory.
    @discussion Creates a single PixelBuffer for a given size and pixelFormatType. Not all parameters of the pixelBufferAttributes will be used here. It requires a release callback function that will be called, when the PixelBuffer gets destroyed so that the owner of the pixels can free the memory.
    @param      width   Width of the PixelBuffer in pixels
    @param      height  Height of the PixelBuffer in pixels
    @param      pixelFormatType		Pixel format indentified by its respective OSType.
    @param      baseAddress		Address of the memory storing the pixels.
    @param      bytesPerRow		Row bytes of the pixel storage memory.
    @param      releaseCallback         CVPixelBufferReleaseBytePointerCallback function that gets called when the PixelBuffer gets destroyed.
    @param      releaseRefCon           User data identifying the PixelBuffer for the release callback.
    @param      pixelBufferAttributes      A dictionary with additonal attributes for a a pixel buffer. This parameter is optional. See PixelBufferAttributes for more details.
    @param      pixelBufferOut          The new pixel buffer will be returned here
    @result	returns kCVReturnSuccess on success.
}
function CVPixelBufferCreateWithBytes( allocator: CFAllocatorRef; width: size_t; height: size_t; pixelFormatType: OSType; baseAddress: UnivPtr; bytesPerRow: size_t; releaseCallback: CVPixelBufferReleaseBytesCallback; releaseRefCon: UnivPtr; pixelBufferAttributes: CFDictionaryRef; var pixelBufferOut: CVPixelBufferRef ): CVReturn; external name '_CVPixelBufferCreateWithBytes';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

type
	CVPixelBufferReleasePlanarBytesCallback = procedure( releaseRefCon: UnivPtr; dataPtr: {const} UnivPtr; dataSize: size_t; numberOfPlanes: size_t; {const} planeAddresses: {variable-size-array} UnivPtr );

{!
    @function   CVPixelBufferCreateWithPlanarBytes
    @abstract   Call to create a single PixelBuffer in planar format for a given size and pixelFormatType based on a passed in piece of memory.
    @discussion Creates a single PixelBuffer for a given size and pixelFormatType. Not all parameters of the pixelBufferAttributes will be used here. It requires a release callback function that will be called, when the PixelBuffer gets destroyed so that the owner of the pixels can free the memory.
    @param      width			Width of the PixelBuffer in pixels
    @param      height			Height of the PixelBuffer in pixels
    @param      pixelFormatType		Pixel format indentified by its respective OSType.
    @param	dataPtr			Pass a pointer to a plane descriptor block, or NULL.
    @param	dataSize		pass size if planes are contiguous, NULL if not.
    @param	numberOfPlanes		Number of planes.
    @param	planeBaseAddress	Array of base addresses for the planes.
    @param	planeWidth		Array of plane widths.
    @param	planeHeight		Array of plane heights.
    @param	planeBytesPerRow	Array of plane bytesPerRow values.
    @param	releaseCallback		CVPixelBufferReleaseBytePointerCallback function that gets called when the PixelBuffer gets destroyed.
    @param	releaseRefCon		User data identifying the PixelBuffer for the release callback.
    @param	pixelBufferAttributes      A dictionary with additonal attributes for a a pixel buffer. This parameter is optional. See PixelBufferAttributes for more details.
    @param      pixelBufferOut          The new pixel buffer will be returned here
    @result	returns kCVReturnSuccess on success.
}
function CVPixelBufferCreateWithPlanarBytes( allocator: CFAllocatorRef; width: size_t; height: size_t; pixelFormatType: OSType; dataPtr: {pass a pointer to a plane descriptor block, or NULL} UnivPtr; dataSize: {pass size if planes are contiguous, NULL if not} size_t; numberOfPlanes: size_t; planeAddresses: {variable-size-array} UnivPtr; planeWidth: {variable-size-array} size_t_Ptr; planeHeight: {variable-size-array} size_t_Ptr; planeBytesPerRow: {variable-size-array} size_t_Ptr; releaseCallback: CVPixelBufferReleasePlanarBytesCallback; releaseRefCon: UnivPtr; pixelBufferAttributes: CFDictionaryRef; var pixelBufferOut: CVPixelBufferRef ): CVReturn; external name '_CVPixelBufferCreateWithPlanarBytes';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{!
    @function   CVPixelBufferLockBaseAddress
    @abstract   Description Locks the BaseAddress of the PixelBuffer to ensure that the is available.
    @param      pixelBuffer Target PixelBuffer.
    @param      lockFlags No options currently defined, pass 0.
    @result     kCVReturnSuccess if the lock succeeded, or error code on failure
}
function CVPixelBufferLockBaseAddress( pixelBuffer: CVPixelBufferRef; lockFlags: CVOptionFlags ): CVReturn; external name '_CVPixelBufferLockBaseAddress';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{!
    @function   CVPixelBufferUnlockBaseAddress
    @abstract   Description Unlocks the BaseAddress of the PixelBuffer.
    @param      pixelBuffer Target PixelBuffer.
    @param      unlockFlags No options currently defined, pass 0.
    @result     kCVReturnSuccess if the unlock succeeded, or error code on failure
}
function CVPixelBufferUnlockBaseAddress( pixelBuffer: CVPixelBufferRef; unlockFlags: CVOptionFlags ): CVReturn; external name '_CVPixelBufferUnlockBaseAddress';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{!
    @function   CVPixelBufferGetWidth
    @abstract   Returns the width of the PixelBuffer.
    @param      pixelBuffer Target PixelBuffer.
    @result     Width in pixels.
}
function CVPixelBufferGetWidth( pixelBuffer: CVPixelBufferRef ): size_t; external name '_CVPixelBufferGetWidth';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{!
    @function   CVPixelBufferGetHeight
    @abstract   Returns the height of the PixelBuffer.
    @param      pixelBuffer Target PixelBuffer.
    @result     Height in pixels.
}
function CVPixelBufferGetHeight( pixelBuffer: CVPixelBufferRef ): size_t; external name '_CVPixelBufferGetHeight';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{!
    @function   CVPixelBufferGetPixelFormatType
    @abstract   Returns the PixelFormatType of the PixelBuffer.
    @param      pixelBuffer Target PixelBuffer.
    @result     OSType identifying the pixel format by its type.
}
function CVPixelBufferGetPixelFormatType( pixelBuffer: CVPixelBufferRef ): OSType; external name '_CVPixelBufferGetPixelFormatType';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{!
    @function   CVPixelBufferGetBaseAddress
    @abstract   Returns the base address of the PixelBuffer.
    @discussion Retrieving the base address for a PixelBuffer requires that the buffer base address be locked
                via a successful call to CVPixelBufferLockBaseAddress.
    @param      pixelBuffer Target PixelBuffer.
    @result     Base address of the pixels.
		For chunky buffers, this will return a pointer to the pixel at 0,0 in the buffer
		For planar buffers this will return a pointer to a PlanarComponentInfo struct (defined in QuickTime).
}
function CVPixelBufferGetBaseAddress( pixelBuffer: CVPixelBufferRef ): UnivPtr; external name '_CVPixelBufferGetBaseAddress';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{!
    @function   CVPixelBufferGetBytesPerRow
    @abstract   Returns the rowBytes of the PixelBuffer.
    @param      pixelBuffer Target PixelBuffer.
    @result     Bytes per row of the image data.   For planar buffers this will return a rowBytes value such that bytesPerRow * height
                will cover the entire image including all planes.
}
function CVPixelBufferGetBytesPerRow( pixelBuffer: CVPixelBufferRef ): size_t; external name '_CVPixelBufferGetBytesPerRow';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{!
    @function   CVPixelBufferGetDataSize
    @abstract   Returns the data size for contigous planes of the PixelBuffer.
    @param      pixelBuffer Target PixelBuffer.
    @result     Data size used in CVPixelBufferCreateWithPlanarBytes.
}
function CVPixelBufferGetDataSize( pixelBuffer: CVPixelBufferRef ): size_t; external name '_CVPixelBufferGetDataSize';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{!
    @function   CVPixelBufferIsPlanar
    @abstract   Returns if the PixelBuffer is planar.
    @param      pixelBuffer Target PixelBuffer.
    @result     True if the PixelBuffer was created using CVPixelBufferCreateWithPlanarBytes.
}
function CVPixelBufferIsPlanar( pixelBuffer: CVPixelBufferRef ): Boolean; external name '_CVPixelBufferIsPlanar';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{!
    @function   CVPixelBufferGetPlaneCount
    @abstract   Returns number of planes of the PixelBuffer.
    @param      pixelBuffer Target PixelBuffer.
    @result     Number of planes.  Returns 0 for non-planar CVPixelBufferRefs.
}
function CVPixelBufferGetPlaneCount( pixelBuffer: CVPixelBufferRef ): size_t; external name '_CVPixelBufferGetPlaneCount';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{!
    @function   CVPixelBufferGetWidthOfPlane
    @abstract   Returns the width of the plane at planeIndex in the PixelBuffer.
    @param      pixelBuffer Target PixelBuffer.
    @param      planeIndex  Identifying the plane.
    @result     Width in pixels, or 0 for non-planar CVPixelBufferRefs.
}
function CVPixelBufferGetWidthOfPlane( pixelBuffer: CVPixelBufferRef; planeIndex: size_t ): size_t; external name '_CVPixelBufferGetWidthOfPlane';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{!
    @function   CVPixelBufferGetHeightOfPlane
    @abstract   Returns the height of the plane at planeIndex in the PixelBuffer.
    @param      pixelBuffer Target PixelBuffer.
    @param      planeIndex  Identifying the plane.
    @result     Height in pixels, or 0 for non-planar CVPixelBufferRefs.
}
function CVPixelBufferGetHeightOfPlane( pixelBuffer: CVPixelBufferRef; planeIndex: size_t ): size_t; external name '_CVPixelBufferGetHeightOfPlane';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{!
    @function   CVPixelBufferGetBaseAddressOfPlane
    @abstract   Returns the base address of the plane at planeIndex in the PixelBuffer.
    @discussion Retrieving the base address for a PixelBuffer requires that the buffer base address be locked
                via a successful call to CVPixelBufferLockBaseAddress.
    @param      pixelBuffer Target PixelBuffer.
    @param      planeIndex  Identifying the plane.
    @result     Base address of the plane, or NULL for non-planar CVPixelBufferRefs.
}
function CVPixelBufferGetBaseAddressOfPlane( pixelBuffer: CVPixelBufferRef; planeIndex: size_t ): UnivPtr; external name '_CVPixelBufferGetBaseAddressOfPlane';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{!
    @function   CVPixelBufferGetBytesPerRowOfPlane
    @abstract   Returns the row bytes of the plane at planeIndex in the PixelBuffer.
    @param      pixelBuffer Target PixelBuffer.
    @param      planeIndex  Identifying the plane.
    @result     Row bytes of the plane, or NULL for non-planar CVPixelBufferRefs.
}
function CVPixelBufferGetBytesPerRowOfPlane( pixelBuffer: CVPixelBufferRef; planeIndex: size_t ): size_t; external name '_CVPixelBufferGetBytesPerRowOfPlane';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{!
    @function   CVPixelBufferGetExtendedPixels
    @abstract   Returns the size of extended pixels of the PixelBuffer.
    @param      pixelBuffer Target PixelBuffer.
    @param      extraColumnsOnLeft Returns the pixel row padding to the left.  May be NULL.
    @param      extraRowsOnTop Returns the pixel row padding to the top.  May be NULL. 
    @param      extraColumnsOnRight Returns the pixel row padding to the right. May be NULL.
    @param      extraRowsOnBottom Returns the pixel row padding to the bottom. May be NULL.
}
procedure CVPixelBufferGetExtendedPixels( pixelBuffer: CVPixelBufferRef; var extraColumnsOnLeft: size_t; var extraColumnsOnRight: size_t; var extraRowsOnTop: size_t; var extraRowsOnBottom: size_t ); external name '_CVPixelBufferGetExtendedPixels';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{!
    @function   CVPixelBufferFillExtendedPixels
    @abstract   Fills the extended pixels of the PixelBuffer with Zero.   This function replicates edge pixels to fill the entire extended region of the image.
    @param      pixelBuffer Target PixelBuffer.
}
function CVPixelBufferFillExtendedPixels( pixelBuffer: CVPixelBufferRef ): CVReturn; external name '_CVPixelBufferFillExtendedPixels';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


end.
