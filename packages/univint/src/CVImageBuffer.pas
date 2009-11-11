{
 *  CVImageBuffer.h
 *  CoreVideo
 *
 *  Copyright (c) 2004 Apple Computer, Inc. All rights reserved.
 *
 }
{	 Pascal Translation:  Gale R Paeper, <gpaeper@empirenet.com>, 2008 }
{  Pascal Translation Updated:  Jonas Maebe, <jonas@freepascal.org>, October 2009 }
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

unit CVImageBuffer;
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
	{$setc TARGET_CPU_PPC := FALSE}
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
uses CFBase, CGColorSpace, CGGeometry,CVBuffer;
{$endc} {not MACOSALLINCLUDE}


{$ifc TARGET_OS_MAC}

{$ALIGN POWER}

 
 {! @header CVImageBuffer.h
	@copyright 2004 Apple Computer, Inc. All rights reserved.
	@availability Mac OS X 10.4 or later
    @discussion CVImageBufferRef types are abstract and define various attachments and convenience
		calls for retreiving image related bits of data.
		   
}

//#pragma mark CVImageBufferRef attachment keys

var kCVImageBufferCGColorSpaceKey: CFStringRef; external name '_kCVImageBufferCGColorSpaceKey'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)				// CGColorSpaceRef

var kCVImageBufferCleanApertureKey: CFStringRef; external name '_kCVImageBufferCleanApertureKey'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)				// CFDictionary containing the following four keys
var kCVImageBufferCleanApertureWidthKey: CFStringRef; external name '_kCVImageBufferCleanApertureWidthKey'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)			// CFNumber
var kCVImageBufferCleanApertureHeightKey: CFStringRef; external name '_kCVImageBufferCleanApertureHeightKey'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)			// CFNumber
var kCVImageBufferCleanApertureHorizontalOffsetKey: CFStringRef; external name '_kCVImageBufferCleanApertureHorizontalOffsetKey'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)		// CFNumber
var kCVImageBufferCleanApertureVerticalOffsetKey: CFStringRef; external name '_kCVImageBufferCleanApertureVerticalOffsetKey'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)		// CFNumber
var kCVImageBufferPreferredCleanApertureKey: CFStringRef; external name '_kCVImageBufferPreferredCleanApertureKey'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)			// CFDictionary containing same keys as kCVImageBufferCleanApertureKey

var kCVImageBufferFieldCountKey: CFStringRef; external name '_kCVImageBufferFieldCountKey'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)				// CFNumber
var kCVImageBufferFieldDetailKey: CFStringRef; external name '_kCVImageBufferFieldDetailKey'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)				// CFString with one of the following four values
var kCVImageBufferFieldDetailTemporalTopFirst: CFStringRef; external name '_kCVImageBufferFieldDetailTemporalTopFirst'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)		// CFString
var kCVImageBufferFieldDetailTemporalBottomFirst: CFStringRef; external name '_kCVImageBufferFieldDetailTemporalBottomFirst'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)		// CFString
var kCVImageBufferFieldDetailSpatialFirstLineEarly: CFStringRef; external name '_kCVImageBufferFieldDetailSpatialFirstLineEarly'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)		// CFString
var kCVImageBufferFieldDetailSpatialFirstLineLate: CFStringRef; external name '_kCVImageBufferFieldDetailSpatialFirstLineLate'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)		// CFString

var kCVImageBufferPixelAspectRatioKey: CFStringRef; external name '_kCVImageBufferPixelAspectRatioKey'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)				// CFDictionary with the following two keys
var kCVImageBufferPixelAspectRatioHorizontalSpacingKey: CFStringRef; external name '_kCVImageBufferPixelAspectRatioHorizontalSpacingKey'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)	// CFNumber
var kCVImageBufferPixelAspectRatioVerticalSpacingKey: CFStringRef; external name '_kCVImageBufferPixelAspectRatioVerticalSpacingKey'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)	// CFNumber

var kCVImageBufferDisplayDimensionsKey: CFStringRef; external name '_kCVImageBufferDisplayDimensionsKey'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)				// CFDictionary with the following two keys
var kCVImageBufferDisplayWidthKey: CFStringRef; external name '_kCVImageBufferDisplayWidthKey'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)				// CFNumber
var kCVImageBufferDisplayHeightKey: CFStringRef; external name '_kCVImageBufferDisplayHeightKey'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)				// CFNumber

var kCVImageBufferGammaLevelKey: CFStringRef; external name '_kCVImageBufferGammaLevelKey'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)				// CFNumber describing the gamma level, used in absence of (or ignorance of) kCVImageBufferTransferFunctionKey

var kCVImageBufferICCProfileKey: CFStringRef; external name '_kCVImageBufferICCProfileKey'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_6_AND_LATER *)				// CFData representation of the ICC profile

var kCVImageBufferYCbCrMatrixKey: CFStringRef; external name '_kCVImageBufferYCbCrMatrixKey'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)				// CFString describing the color matrix for YCbCr->RGB. This key can be one of the following values:
var kCVImageBufferYCbCrMatrix_ITU_R_709_2: CFStringRef; external name '_kCVImageBufferYCbCrMatrix_ITU_R_709_2'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)			// CFString
var kCVImageBufferYCbCrMatrix_ITU_R_601_4: CFStringRef; external name '_kCVImageBufferYCbCrMatrix_ITU_R_601_4'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)			// CFString
var kCVImageBufferYCbCrMatrix_SMPTE_240M_1995: CFStringRef; external name '_kCVImageBufferYCbCrMatrix_SMPTE_240M_1995'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)		// CFString

var kCVImageBufferColorPrimariesKey: CFStringRef; external name '_kCVImageBufferColorPrimariesKey'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)				// CFString describing the color primaries. This key can be one of the following values
var kCVImageBufferColorPrimaries_ITU_R_709_2: CFStringRef; external name '_kCVImageBufferColorPrimaries_ITU_R_709_2'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)
var kCVImageBufferColorPrimaries_EBU_3213: CFStringRef; external name '_kCVImageBufferColorPrimaries_EBU_3213'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)
var kCVImageBufferColorPrimaries_SMPTE_C: CFStringRef; external name '_kCVImageBufferColorPrimaries_SMPTE_C'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)

var kCVImageBufferTransferFunctionKey: CFStringRef; external name '_kCVImageBufferTransferFunctionKey'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)				// CFString describing the transfer function. This key can be one of the following values
var kCVImageBufferTransferFunction_ITU_R_709_2: CFStringRef; external name '_kCVImageBufferTransferFunction_ITU_R_709_2'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)
var kCVImageBufferTransferFunction_SMPTE_240M_1995: CFStringRef; external name '_kCVImageBufferTransferFunction_SMPTE_240M_1995'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_6_AND_LATER *)
var kCVImageBufferTransferFunction_UseGamma: CFStringRef; external name '_kCVImageBufferTransferFunction_UseGamma'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_6_AND_LATER *)
var kCVImageBufferTransferFunction_EBU_3213: CFStringRef; external name '_kCVImageBufferTransferFunction_EBU_3213'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER_BUT_DEPRECATED *)			// Should not be used.
var kCVImageBufferTransferFunction_SMPTE_C: CFStringRef; external name '_kCVImageBufferTransferFunction_SMPTE_C'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER_BUT_DEPRECATED *)			// Should not be used.

{ Chroma siting information. For progressive images, only the TopField value is used. }
var kCVImageBufferChromaLocationTopFieldKey: CFStringRef; external name '_kCVImageBufferChromaLocationTopFieldKey'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)			// CFString with one of the following CFString values
var kCVImageBufferChromaLocationBottomFieldKey: CFStringRef; external name '_kCVImageBufferChromaLocationBottomFieldKey'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)		// CFString with one of the following CFString values
var kCVImageBufferChromaLocation_Left: CFStringRef; external name '_kCVImageBufferChromaLocation_Left'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)			    // Chroma sample is horizontally co-sited with the left column of luma samples, but centered vertically.
var kCVImageBufferChromaLocation_Center: CFStringRef; external name '_kCVImageBufferChromaLocation_Center'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)			    // Chroma sample is fully centered
var kCVImageBufferChromaLocation_TopLeft: CFStringRef; external name '_kCVImageBufferChromaLocation_TopLeft'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)			    // Chroma sample is co-sited with the top-left luma sample.
var kCVImageBufferChromaLocation_Top: CFStringRef; external name '_kCVImageBufferChromaLocation_Top'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)			    // Chroma sample is horizontally centered, but co-sited with the top row of luma samples.
var kCVImageBufferChromaLocation_BottomLeft: CFStringRef; external name '_kCVImageBufferChromaLocation_BottomLeft'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)		    // Chroma sample is co-sited with the bottom-left luma sample.
var kCVImageBufferChromaLocation_Bottom: CFStringRef; external name '_kCVImageBufferChromaLocation_Bottom'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)			    // Chroma sample is horizontally centered, but co-sited with the bottom row of luma samples.
var kCVImageBufferChromaLocation_DV420: CFStringRef; external name '_kCVImageBufferChromaLocation_DV420'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)			    // Cr and Cb samples are alternately co-sited with the left luma samples of the same field.

// These describe the format of the original subsampled data before conversion to 422/2vuy.   In order to use
// these tags, the data must have been converted to 4:2:2 via simple pixel replication.
var kCVImageBufferChromaSubsamplingKey: CFStringRef; external name '_kCVImageBufferChromaSubsamplingKey'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)		// CFString/CFNumber with one of the following values
var kCVImageBufferChromaSubsampling_420: CFStringRef; external name '_kCVImageBufferChromaSubsampling_420'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)
var kCVImageBufferChromaSubsampling_422: CFStringRef; external name '_kCVImageBufferChromaSubsampling_422'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)
var kCVImageBufferChromaSubsampling_411: CFStringRef; external name '_kCVImageBufferChromaSubsampling_411'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)

//#pragma mark CVImageBufferRef

{!
    @typedef	CVImageBufferRef
    @abstract   Base type for all CoreVideo image buffers

}
type
	CVImageBufferRef = CVBufferRef;

{!
    @function   CVImageBufferGetEncodedSize
    @abstract   Returns the full encoded dimensions of a CVImageBuffer.  For example, for an NTSC DV frame this would be 720x480
    @discussion Note: When creating a CIImage from a CVImageBuffer, this is the call you should use for retrieving the image size.
    @param      imageBuffer A CVImageBuffer that you wish to retrieve the encoded size from.
    @result     A CGSize returning the full encoded size of the buffer
		Returns zero size if called with a non-CVImageBufferRef type or NULL.
}
function CVImageBufferGetEncodedSize( imageBuffer: CVImageBufferRef ): CGSize; external name '_CVImageBufferGetEncodedSize';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{!
    @function   CVImageBufferGetDisplaySize
    @abstract   Returns the nominal output display size (in square pixels) of a CVImageBuffer.  
                For example, for an NTSC DV frame this would be 640x480
    @param      imageBuffer A CVImageBuffer that you wish to retrieve the display size from.
    @result     A CGSize returning the nominal display size of the buffer
		Returns zero size if called with a non-CVImageBufferRef type or NULL.
}
function CVImageBufferGetDisplaySize( imageBuffer: CVImageBufferRef ): CGSize; external name '_CVImageBufferGetDisplaySize';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{!
    @function   CVImageBufferGetCleanRect
    @abstract   Returns the source rectangle of a CVImageBuffer that represents the clean aperture
		of the buffer in encoded pixels.    For example, an NTSC DV frame would return a CGRect with an
		origin of 8,0 and a size of 704,480.		
		Note that the origin of this rect always the lower left	corner.   This is the same coordinate system as
		used by CoreImage.
    @param      imageBuffer A CVImageBuffer that you wish to retrieve the display size from.
    @result     A CGSize returning the nominal display size of the buffer
		Returns zero rect if called with a non-CVImageBufferRef type or NULL.
}
function CVImageBufferGetCleanRect( imageBuffer: CVImageBufferRef ): CGRect; external name '_CVImageBufferGetCleanRect';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{!
    @function   CVImageBufferGetColorSpace
    @abstract   Returns the color space of a CVImageBuffer.
    @param      imageBuffer A CVImageBuffer that you wish to retrieve the color space from.
    @result     A CGColorSpaceRef representing the color space of the buffer.
		Returns NULL if called with a non-CVImageBufferRef type or NULL.
}
function CVImageBufferGetColorSpace( imageBuffer: CVImageBufferRef ): CGColorSpaceRef; external name '_CVImageBufferGetColorSpace';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{$endc} {TARGET_OS_MAC}

{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}
