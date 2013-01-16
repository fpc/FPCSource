{
 * ImageIO - CGImageProperties.h
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

unit CGImageProperties;
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
	{$setc TARGET_OS_EMBEDDED := FALSE}
{$elifc defined __ppc64__ and __ppc64__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := TRUE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := FALSE}
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
	{ will require compiler define when/if other Apple devices with ARM cpus ship }
	{$setc TARGET_OS_MAC := FALSE}
	{$setc TARGET_OS_IPHONE := TRUE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
	{$setc TARGET_OS_EMBEDDED := TRUE}
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
uses MacTypes,CFBase,CGBase;
{$endc} {not MACOSALLINCLUDE}


{$ALIGN POWER}


{ Properties that, if returned by CGImageSourceCopyProperties or 
 * CGImageSourceCopyPropertiesAtIndex, contain a dictionary of file-format 
 * or metadata-format specific key-values. }

var kCGImagePropertyTIFFDictionary: CFStringRef; external name '_kCGImagePropertyTIFFDictionary'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyGIFDictionary: CFStringRef; external name '_kCGImagePropertyGIFDictionary'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyJFIFDictionary: CFStringRef; external name '_kCGImagePropertyJFIFDictionary'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyExifDictionary: CFStringRef; external name '_kCGImagePropertyExifDictionary'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyPNGDictionary: CFStringRef; external name '_kCGImagePropertyPNGDictionary'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyIPTCDictionary: CFStringRef; external name '_kCGImagePropertyIPTCDictionary'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyGPSDictionary: CFStringRef; external name '_kCGImagePropertyGPSDictionary'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyRawDictionary: CFStringRef; external name '_kCGImagePropertyRawDictionary'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyCIFFDictionary: CFStringRef; external name '_kCGImagePropertyCIFFDictionary'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyMakerCanonDictionary: CFStringRef; external name '_kCGImagePropertyMakerCanonDictionary'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_4_0) *)
var kCGImagePropertyMakerNikonDictionary: CFStringRef; external name '_kCGImagePropertyMakerNikonDictionary'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_4_0) *)
var kCGImagePropertyMakerMinoltaDictionary: CFStringRef; external name '_kCGImagePropertyMakerMinoltaDictionary'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_4_0) *)
var kCGImagePropertyMakerFujiDictionary: CFStringRef; external name '_kCGImagePropertyMakerFujiDictionary'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_4_0) *)
var kCGImagePropertyMakerOlympusDictionary: CFStringRef; external name '_kCGImagePropertyMakerOlympusDictionary'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_4_0) *)
var kCGImagePropertyMakerPentaxDictionary: CFStringRef; external name '_kCGImagePropertyMakerPentaxDictionary'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_4_0) *)
var kCGImageProperty8BIMDictionary: CFStringRef; external name '_kCGImageProperty8BIMDictionary'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyDNGDictionary: CFStringRef; external name '_kCGImagePropertyDNGDictionary'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_4_0) *)
var kCGImagePropertyExifAuxDictionary: CFStringRef; external name '_kCGImagePropertyExifAuxDictionary'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_4_0) *)


{* Properties which may be returned by "CGImageSourceCopyProperties".  The
 ** values apply to the container in general but not necessarily to any
 ** individual image that it contains. *}

{ The size of the image file in bytes, if known. If present, the value of
 * this key is a CFNumberRef. }

var kCGImagePropertyFileSize: CFStringRef; external name '_kCGImagePropertyFileSize'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)


{* Properties which may be returned by "CGImageSourceCopyPropertiesAtIndex".
 ** The values apply to a single image of an image source file. *}

{ The number of pixels in the x- and y-dimensions. The value of these keys 
 * is a CFNumberRef. }

var kCGImagePropertyPixelHeight: CFStringRef; external name '_kCGImagePropertyPixelHeight'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyPixelWidth: CFStringRef; external name '_kCGImagePropertyPixelWidth'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)

{ The DPI in the x- and y-dimensions, if known. If present, the value of
 * these keys is a CFNumberRef. }

var kCGImagePropertyDPIHeight: CFStringRef; external name '_kCGImagePropertyDPIHeight'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyDPIWidth: CFStringRef; external name '_kCGImagePropertyDPIWidth'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)

{ The number of bits in each color sample of each pixel. The value of this 
 * key is a CFNumberRef. }

var kCGImagePropertyDepth: CFStringRef; external name '_kCGImagePropertyDepth'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)

{ The intended display orientation of the image. If present, the value 
 * of this key is a CFNumberRef with the same value as defined by the 
 * TIFF and Exif specifications.  That is:
 *   1  =  0th row is at the top, and 0th column is on the left.  
 *   2  =  0th row is at the top, and 0th column is on the right.  
 *   3  =  0th row is at the bottom, and 0th column is on the right.  
 *   4  =  0th row is at the bottom, and 0th column is on the left.  
 *   5  =  0th row is on the left, and 0th column is the top.  
 *   6  =  0th row is on the right, and 0th column is the top.  
 *   7  =  0th row is on the right, and 0th column is the bottom.  
 *   8  =  0th row is on the left, and 0th column is the bottom.  
 * If not present, a value of 1 is assumed. } 
 
var kCGImagePropertyOrientation: CFStringRef; external name '_kCGImagePropertyOrientation'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)

{ The value of this key is kCFBooleanTrue if the image contains floating- 
 * point pixel samples } 
 
var kCGImagePropertyIsFloat: CFStringRef; external name '_kCGImagePropertyIsFloat'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)

{ The value of this key is kCFBooleanTrue if the image contains indexed 
 * (a.k.a. paletted) pixel samples } 
 
var kCGImagePropertyIsIndexed: CFStringRef; external name '_kCGImagePropertyIsIndexed'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)

{ The value of this key is kCFBooleanTrue if the image contains an alpha 
 * (a.k.a. coverage) channel } 
 
var kCGImagePropertyHasAlpha: CFStringRef; external name '_kCGImagePropertyHasAlpha'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)

{ The color model of the image such as "RGB", "CMYK", "Gray", or "Lab".
 * The value of this key is CFStringRef. } 

var kCGImagePropertyColorModel: CFStringRef; external name '_kCGImagePropertyColorModel'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)

{ The name of the optional ICC profile embedded in the image, if known.  
 * If present, the value of this key is a CFStringRef. }

var kCGImagePropertyProfileName: CFStringRef; external name '_kCGImagePropertyProfileName'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)


{ Possible values for kCGImagePropertyColorModel property }

var kCGImagePropertyColorModelRGB: CFStringRef; external name '_kCGImagePropertyColorModelRGB'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyColorModelGray: CFStringRef; external name '_kCGImagePropertyColorModelGray'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyColorModelCMYK: CFStringRef; external name '_kCGImagePropertyColorModelCMYK'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyColorModelLab: CFStringRef; external name '_kCGImagePropertyColorModelLab'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)


{ Possible keys for kCGImagePropertyTIFFDictionary }

var kCGImagePropertyTIFFCompression: CFStringRef; external name '_kCGImagePropertyTIFFCompression'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyTIFFPhotometricInterpretation: CFStringRef; external name '_kCGImagePropertyTIFFPhotometricInterpretation'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyTIFFDocumentName: CFStringRef; external name '_kCGImagePropertyTIFFDocumentName'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyTIFFImageDescription: CFStringRef; external name '_kCGImagePropertyTIFFImageDescription'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyTIFFMake: CFStringRef; external name '_kCGImagePropertyTIFFMake'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyTIFFModel: CFStringRef; external name '_kCGImagePropertyTIFFModel'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyTIFFOrientation: CFStringRef; external name '_kCGImagePropertyTIFFOrientation'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyTIFFXResolution: CFStringRef; external name '_kCGImagePropertyTIFFXResolution'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyTIFFYResolution: CFStringRef; external name '_kCGImagePropertyTIFFYResolution'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyTIFFResolutionUnit: CFStringRef; external name '_kCGImagePropertyTIFFResolutionUnit'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyTIFFSoftware: CFStringRef; external name '_kCGImagePropertyTIFFSoftware'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyTIFFTransferFunction: CFStringRef; external name '_kCGImagePropertyTIFFTransferFunction'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyTIFFDateTime: CFStringRef; external name '_kCGImagePropertyTIFFDateTime'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyTIFFArtist: CFStringRef; external name '_kCGImagePropertyTIFFArtist'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyTIFFHostComputer: CFStringRef; external name '_kCGImagePropertyTIFFHostComputer'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyTIFFCopyright: CFStringRef; external name '_kCGImagePropertyTIFFCopyright'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyTIFFWhitePoint: CFStringRef; external name '_kCGImagePropertyTIFFWhitePoint'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyTIFFPrimaryChromaticities: CFStringRef; external name '_kCGImagePropertyTIFFPrimaryChromaticities'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)

{ Possible keys for kCGImagePropertyJFIFDictionary }

var kCGImagePropertyJFIFVersion: CFStringRef; external name '_kCGImagePropertyJFIFVersion'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyJFIFXDensity: CFStringRef; external name '_kCGImagePropertyJFIFXDensity'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyJFIFYDensity: CFStringRef; external name '_kCGImagePropertyJFIFYDensity'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyJFIFDensityUnit: CFStringRef; external name '_kCGImagePropertyJFIFDensityUnit'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyJFIFIsProgressive: CFStringRef; external name '_kCGImagePropertyJFIFIsProgressive'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)


{ Possible keys for kCGImagePropertyExifDictionary }

var kCGImagePropertyExifExposureTime: CFStringRef; external name '_kCGImagePropertyExifExposureTime'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyExifFNumber: CFStringRef; external name '_kCGImagePropertyExifFNumber'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyExifExposureProgram: CFStringRef; external name '_kCGImagePropertyExifExposureProgram'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyExifSpectralSensitivity: CFStringRef; external name '_kCGImagePropertyExifSpectralSensitivity'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyExifISOSpeedRatings: CFStringRef; external name '_kCGImagePropertyExifISOSpeedRatings'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyExifOECF: CFStringRef; external name '_kCGImagePropertyExifOECF'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyExifVersion: CFStringRef; external name '_kCGImagePropertyExifVersion'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyExifDateTimeOriginal: CFStringRef; external name '_kCGImagePropertyExifDateTimeOriginal'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyExifDateTimeDigitized: CFStringRef; external name '_kCGImagePropertyExifDateTimeDigitized'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyExifComponentsConfiguration: CFStringRef; external name '_kCGImagePropertyExifComponentsConfiguration'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyExifCompressedBitsPerPixel: CFStringRef; external name '_kCGImagePropertyExifCompressedBitsPerPixel'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyExifShutterSpeedValue: CFStringRef; external name '_kCGImagePropertyExifShutterSpeedValue'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyExifApertureValue: CFStringRef; external name '_kCGImagePropertyExifApertureValue'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyExifBrightnessValue: CFStringRef; external name '_kCGImagePropertyExifBrightnessValue'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyExifExposureBiasValue: CFStringRef; external name '_kCGImagePropertyExifExposureBiasValue'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyExifMaxApertureValue: CFStringRef; external name '_kCGImagePropertyExifMaxApertureValue'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyExifSubjectDistance: CFStringRef; external name '_kCGImagePropertyExifSubjectDistance'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyExifMeteringMode: CFStringRef; external name '_kCGImagePropertyExifMeteringMode'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyExifLightSource: CFStringRef; external name '_kCGImagePropertyExifLightSource'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyExifFlash: CFStringRef; external name '_kCGImagePropertyExifFlash'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyExifFocalLength: CFStringRef; external name '_kCGImagePropertyExifFocalLength'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyExifSubjectArea: CFStringRef; external name '_kCGImagePropertyExifSubjectArea'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyExifMakerNote: CFStringRef; external name '_kCGImagePropertyExifMakerNote'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyExifUserComment: CFStringRef; external name '_kCGImagePropertyExifUserComment'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyExifSubsecTime: CFStringRef; external name '_kCGImagePropertyExifSubsecTime'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyExifSubsecTimeOrginal: CFStringRef; external name '_kCGImagePropertyExifSubsecTimeOrginal'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyExifSubsecTimeDigitized: CFStringRef; external name '_kCGImagePropertyExifSubsecTimeDigitized'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyExifFlashPixVersion: CFStringRef; external name '_kCGImagePropertyExifFlashPixVersion'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyExifColorSpace: CFStringRef; external name '_kCGImagePropertyExifColorSpace'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyExifPixelXDimension: CFStringRef; external name '_kCGImagePropertyExifPixelXDimension'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyExifPixelYDimension: CFStringRef; external name '_kCGImagePropertyExifPixelYDimension'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyExifRelatedSoundFile: CFStringRef; external name '_kCGImagePropertyExifRelatedSoundFile'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyExifFlashEnergy: CFStringRef; external name '_kCGImagePropertyExifFlashEnergy'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyExifSpatialFrequencyResponse: CFStringRef; external name '_kCGImagePropertyExifSpatialFrequencyResponse'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyExifFocalPlaneXResolution: CFStringRef; external name '_kCGImagePropertyExifFocalPlaneXResolution'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyExifFocalPlaneYResolution: CFStringRef; external name '_kCGImagePropertyExifFocalPlaneYResolution'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyExifFocalPlaneResolutionUnit: CFStringRef; external name '_kCGImagePropertyExifFocalPlaneResolutionUnit'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyExifSubjectLocation: CFStringRef; external name '_kCGImagePropertyExifSubjectLocation'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyExifExposureIndex: CFStringRef; external name '_kCGImagePropertyExifExposureIndex'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyExifSensingMethod: CFStringRef; external name '_kCGImagePropertyExifSensingMethod'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyExifFileSource: CFStringRef; external name '_kCGImagePropertyExifFileSource'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyExifSceneType: CFStringRef; external name '_kCGImagePropertyExifSceneType'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyExifCFAPattern: CFStringRef; external name '_kCGImagePropertyExifCFAPattern'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyExifCustomRendered: CFStringRef; external name '_kCGImagePropertyExifCustomRendered'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyExifExposureMode: CFStringRef; external name '_kCGImagePropertyExifExposureMode'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyExifWhiteBalance: CFStringRef; external name '_kCGImagePropertyExifWhiteBalance'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyExifDigitalZoomRatio: CFStringRef; external name '_kCGImagePropertyExifDigitalZoomRatio'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyExifFocalLenIn35mmFilm: CFStringRef; external name '_kCGImagePropertyExifFocalLenIn35mmFilm'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyExifSceneCaptureType: CFStringRef; external name '_kCGImagePropertyExifSceneCaptureType'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyExifGainControl: CFStringRef; external name '_kCGImagePropertyExifGainControl'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyExifContrast: CFStringRef; external name '_kCGImagePropertyExifContrast'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyExifSaturation: CFStringRef; external name '_kCGImagePropertyExifSaturation'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyExifSharpness: CFStringRef; external name '_kCGImagePropertyExifSharpness'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyExifDeviceSettingDescription: CFStringRef; external name '_kCGImagePropertyExifDeviceSettingDescription'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyExifSubjectDistRange: CFStringRef; external name '_kCGImagePropertyExifSubjectDistRange'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyExifImageUniqueID: CFStringRef; external name '_kCGImagePropertyExifImageUniqueID'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)

var kCGImagePropertyExifCameraOwnerName: CFStringRef; external name '_kCGImagePropertyExifCameraOwnerName'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_7, __IPHONE_5_0) *)
var kCGImagePropertyExifBodySerialNumber: CFStringRef; external name '_kCGImagePropertyExifBodySerialNumber'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_7, __IPHONE_5_0) *)
var kCGImagePropertyExifLensSpecification: CFStringRef; external name '_kCGImagePropertyExifLensSpecification'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_7, __IPHONE_5_0) *)
var kCGImagePropertyExifLensMake: CFStringRef; external name '_kCGImagePropertyExifLensMake'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_7, __IPHONE_5_0) *)
var kCGImagePropertyExifLensModel: CFStringRef; external name '_kCGImagePropertyExifLensModel'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_7, __IPHONE_5_0) *)
var kCGImagePropertyExifLensSerialNumber: CFStringRef; external name '_kCGImagePropertyExifLensSerialNumber'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_7, __IPHONE_5_0) *)

var kCGImagePropertyExifGamma: CFStringRef; external name '_kCGImagePropertyExifGamma'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)

{ Possible keys for kCGImagePropertyExifAuxDictionary }
var kCGImagePropertyExifAuxLensInfo: CFStringRef; external name '_kCGImagePropertyExifAuxLensInfo'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_4_0) *)
var kCGImagePropertyExifAuxLensModel: CFStringRef; external name '_kCGImagePropertyExifAuxLensModel'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_4_0) *)
var kCGImagePropertyExifAuxSerialNumber: CFStringRef; external name '_kCGImagePropertyExifAuxSerialNumber'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_4_0) *)
var kCGImagePropertyExifAuxLensID: CFStringRef; external name '_kCGImagePropertyExifAuxLensID'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_4_0) *)
var kCGImagePropertyExifAuxLensSerialNumber: CFStringRef; external name '_kCGImagePropertyExifAuxLensSerialNumber'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_4_0) *)
var kCGImagePropertyExifAuxImageNumber: CFStringRef; external name '_kCGImagePropertyExifAuxImageNumber'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_4_0) *)
var kCGImagePropertyExifAuxFlashCompensation: CFStringRef; external name '_kCGImagePropertyExifAuxFlashCompensation'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_4_0) *)
var kCGImagePropertyExifAuxOwnerName: CFStringRef; external name '_kCGImagePropertyExifAuxOwnerName'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_4_0) *)
var kCGImagePropertyExifAuxFirmware: CFStringRef; external name '_kCGImagePropertyExifAuxFirmware'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_4_0) *)

{ Possible keys for kCGImagePropertyGIFDictionary }

var kCGImagePropertyGIFLoopCount: CFStringRef; external name '_kCGImagePropertyGIFLoopCount'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyGIFDelayTime: CFStringRef; external name '_kCGImagePropertyGIFDelayTime'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyGIFImageColorMap: CFStringRef; external name '_kCGImagePropertyGIFImageColorMap'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyGIFHasGlobalColorMap: CFStringRef; external name '_kCGImagePropertyGIFHasGlobalColorMap'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyGIFUnclampedDelayTime: CFStringRef; external name '_kCGImagePropertyGIFUnclampedDelayTime'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_7, __IPHONE_4_0) *)

{ Possible keys for kCGImagePropertyPNGDictionary }

var kCGImagePropertyPNGGamma: CFStringRef; external name '_kCGImagePropertyPNGGamma'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyPNGInterlaceType: CFStringRef; external name '_kCGImagePropertyPNGInterlaceType'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyPNGXPixelsPerMeter: CFStringRef; external name '_kCGImagePropertyPNGXPixelsPerMeter'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyPNGYPixelsPerMeter: CFStringRef; external name '_kCGImagePropertyPNGYPixelsPerMeter'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyPNGsRGBIntent: CFStringRef; external name '_kCGImagePropertyPNGsRGBIntent'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyPNGChromaticities: CFStringRef; external name '_kCGImagePropertyPNGChromaticities'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)

var kCGImagePropertyPNGAuthor: CFStringRef; external name '_kCGImagePropertyPNGAuthor'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_7, __IPHONE_5_0) *)
var kCGImagePropertyPNGCopyright: CFStringRef; external name '_kCGImagePropertyPNGCopyright'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_7, __IPHONE_5_0) *)
var kCGImagePropertyPNGCreationTime: CFStringRef; external name '_kCGImagePropertyPNGCreationTime'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_7, __IPHONE_5_0) *)
var kCGImagePropertyPNGDescription: CFStringRef; external name '_kCGImagePropertyPNGDescription'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_7, __IPHONE_5_0) *)
var kCGImagePropertyPNGModificationTime: CFStringRef; external name '_kCGImagePropertyPNGModificationTime'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_7, __IPHONE_5_0) *)
var kCGImagePropertyPNGSoftware: CFStringRef; external name '_kCGImagePropertyPNGSoftware'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_7, __IPHONE_5_0) *)
var kCGImagePropertyPNGTitle: CFStringRef; external name '_kCGImagePropertyPNGTitle'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_7, __IPHONE_5_0) *)

{ Possible keys for kCGImagePropertyGPSDictionary }

var kCGImagePropertyGPSVersion: CFStringRef; external name '_kCGImagePropertyGPSVersion'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyGPSLatitudeRef: CFStringRef; external name '_kCGImagePropertyGPSLatitudeRef'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyGPSLatitude: CFStringRef; external name '_kCGImagePropertyGPSLatitude'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyGPSLongitudeRef: CFStringRef; external name '_kCGImagePropertyGPSLongitudeRef'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyGPSLongitude: CFStringRef; external name '_kCGImagePropertyGPSLongitude'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyGPSAltitudeRef: CFStringRef; external name '_kCGImagePropertyGPSAltitudeRef'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyGPSAltitude: CFStringRef; external name '_kCGImagePropertyGPSAltitude'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyGPSTimeStamp: CFStringRef; external name '_kCGImagePropertyGPSTimeStamp'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyGPSSatellites: CFStringRef; external name '_kCGImagePropertyGPSSatellites'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyGPSStatus: CFStringRef; external name '_kCGImagePropertyGPSStatus'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyGPSMeasureMode: CFStringRef; external name '_kCGImagePropertyGPSMeasureMode'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyGPSDOP: CFStringRef; external name '_kCGImagePropertyGPSDOP'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyGPSSpeedRef: CFStringRef; external name '_kCGImagePropertyGPSSpeedRef'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyGPSSpeed: CFStringRef; external name '_kCGImagePropertyGPSSpeed'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyGPSTrackRef: CFStringRef; external name '_kCGImagePropertyGPSTrackRef'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyGPSTrack: CFStringRef; external name '_kCGImagePropertyGPSTrack'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyGPSImgDirectionRef: CFStringRef; external name '_kCGImagePropertyGPSImgDirectionRef'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyGPSImgDirection: CFStringRef; external name '_kCGImagePropertyGPSImgDirection'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyGPSMapDatum: CFStringRef; external name '_kCGImagePropertyGPSMapDatum'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyGPSDestLatitudeRef: CFStringRef; external name '_kCGImagePropertyGPSDestLatitudeRef'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyGPSDestLatitude: CFStringRef; external name '_kCGImagePropertyGPSDestLatitude'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyGPSDestLongitudeRef: CFStringRef; external name '_kCGImagePropertyGPSDestLongitudeRef'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyGPSDestLongitude: CFStringRef; external name '_kCGImagePropertyGPSDestLongitude'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyGPSDestBearingRef: CFStringRef; external name '_kCGImagePropertyGPSDestBearingRef'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyGPSDestBearing: CFStringRef; external name '_kCGImagePropertyGPSDestBearing'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyGPSDestDistanceRef: CFStringRef; external name '_kCGImagePropertyGPSDestDistanceRef'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyGPSDestDistance: CFStringRef; external name '_kCGImagePropertyGPSDestDistance'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyGPSProcessingMethod: CFStringRef; external name '_kCGImagePropertyGPSProcessingMethod'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyGPSAreaInformation: CFStringRef; external name '_kCGImagePropertyGPSAreaInformation'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyGPSDateStamp: CFStringRef; external name '_kCGImagePropertyGPSDateStamp'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyGPSDifferental: CFStringRef; external name '_kCGImagePropertyGPSDifferental'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)

{ Possible keys for kCGImagePropertyIPTCDictionary }

var kCGImagePropertyIPTCObjectTypeReference: CFStringRef; external name '_kCGImagePropertyIPTCObjectTypeReference'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyIPTCObjectAttributeReference: CFStringRef; external name '_kCGImagePropertyIPTCObjectAttributeReference'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyIPTCObjectName: CFStringRef; external name '_kCGImagePropertyIPTCObjectName'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyIPTCEditStatus: CFStringRef; external name '_kCGImagePropertyIPTCEditStatus'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyIPTCEditorialUpdate: CFStringRef; external name '_kCGImagePropertyIPTCEditorialUpdate'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyIPTCUrgency: CFStringRef; external name '_kCGImagePropertyIPTCUrgency'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyIPTCSubjectReference: CFStringRef; external name '_kCGImagePropertyIPTCSubjectReference'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyIPTCCategory: CFStringRef; external name '_kCGImagePropertyIPTCCategory'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyIPTCSupplementalCategory: CFStringRef; external name '_kCGImagePropertyIPTCSupplementalCategory'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyIPTCFixtureIdentifier: CFStringRef; external name '_kCGImagePropertyIPTCFixtureIdentifier'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyIPTCKeywords: CFStringRef; external name '_kCGImagePropertyIPTCKeywords'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyIPTCContentLocationCode: CFStringRef; external name '_kCGImagePropertyIPTCContentLocationCode'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyIPTCContentLocationName: CFStringRef; external name '_kCGImagePropertyIPTCContentLocationName'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyIPTCReleaseDate: CFStringRef; external name '_kCGImagePropertyIPTCReleaseDate'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyIPTCReleaseTime: CFStringRef; external name '_kCGImagePropertyIPTCReleaseTime'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyIPTCExpirationDate: CFStringRef; external name '_kCGImagePropertyIPTCExpirationDate'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyIPTCExpirationTime: CFStringRef; external name '_kCGImagePropertyIPTCExpirationTime'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyIPTCSpecialInstructions: CFStringRef; external name '_kCGImagePropertyIPTCSpecialInstructions'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyIPTCActionAdvised: CFStringRef; external name '_kCGImagePropertyIPTCActionAdvised'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyIPTCReferenceService: CFStringRef; external name '_kCGImagePropertyIPTCReferenceService'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyIPTCReferenceDate: CFStringRef; external name '_kCGImagePropertyIPTCReferenceDate'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyIPTCReferenceNumber: CFStringRef; external name '_kCGImagePropertyIPTCReferenceNumber'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyIPTCDateCreated: CFStringRef; external name '_kCGImagePropertyIPTCDateCreated'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyIPTCTimeCreated: CFStringRef; external name '_kCGImagePropertyIPTCTimeCreated'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyIPTCDigitalCreationDate: CFStringRef; external name '_kCGImagePropertyIPTCDigitalCreationDate'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyIPTCDigitalCreationTime: CFStringRef; external name '_kCGImagePropertyIPTCDigitalCreationTime'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyIPTCOriginatingProgram: CFStringRef; external name '_kCGImagePropertyIPTCOriginatingProgram'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyIPTCProgramVersion: CFStringRef; external name '_kCGImagePropertyIPTCProgramVersion'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyIPTCObjectCycle: CFStringRef; external name '_kCGImagePropertyIPTCObjectCycle'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyIPTCByline: CFStringRef; external name '_kCGImagePropertyIPTCByline'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyIPTCBylineTitle: CFStringRef; external name '_kCGImagePropertyIPTCBylineTitle'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyIPTCCity: CFStringRef; external name '_kCGImagePropertyIPTCCity'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyIPTCSubLocation: CFStringRef; external name '_kCGImagePropertyIPTCSubLocation'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyIPTCProvinceState: CFStringRef; external name '_kCGImagePropertyIPTCProvinceState'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyIPTCCountryPrimaryLocationCode: CFStringRef; external name '_kCGImagePropertyIPTCCountryPrimaryLocationCode'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyIPTCCountryPrimaryLocationName: CFStringRef; external name '_kCGImagePropertyIPTCCountryPrimaryLocationName'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyIPTCOriginalTransmissionReference: CFStringRef; external name '_kCGImagePropertyIPTCOriginalTransmissionReference'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyIPTCHeadline: CFStringRef; external name '_kCGImagePropertyIPTCHeadline'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyIPTCCredit: CFStringRef; external name '_kCGImagePropertyIPTCCredit'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyIPTCSource: CFStringRef; external name '_kCGImagePropertyIPTCSource'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyIPTCCopyrightNotice: CFStringRef; external name '_kCGImagePropertyIPTCCopyrightNotice'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyIPTCContact: CFStringRef; external name '_kCGImagePropertyIPTCContact'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyIPTCCaptionAbstract: CFStringRef; external name '_kCGImagePropertyIPTCCaptionAbstract'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyIPTCWriterEditor: CFStringRef; external name '_kCGImagePropertyIPTCWriterEditor'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyIPTCImageType: CFStringRef; external name '_kCGImagePropertyIPTCImageType'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyIPTCImageOrientation: CFStringRef; external name '_kCGImagePropertyIPTCImageOrientation'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyIPTCLanguageIdentifier: CFStringRef; external name '_kCGImagePropertyIPTCLanguageIdentifier'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyIPTCStarRating: CFStringRef; external name '_kCGImagePropertyIPTCStarRating'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)
var kCGImagePropertyIPTCCreatorContactInfo: CFStringRef; external name '_kCGImagePropertyIPTCCreatorContactInfo'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_6, __IPHONE_4_0) *)	// IPTC Core
var kCGImagePropertyIPTCRightsUsageTerms: CFStringRef; external name '_kCGImagePropertyIPTCRightsUsageTerms'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_6, __IPHONE_4_0) *)	// IPTC Core
var kCGImagePropertyIPTCScene: CFStringRef; external name '_kCGImagePropertyIPTCScene'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_6, __IPHONE_4_0) *)				// IPTC Core

{ Possible keys for kCGImagePropertyIPTCCreatorContactInfo dictionary (part of IPTC Core - above) }

var kCGImagePropertyIPTCContactInfoCity: CFStringRef; external name '_kCGImagePropertyIPTCContactInfoCity'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_6, __IPHONE_4_0) *)
var kCGImagePropertyIPTCContactInfoCountry: CFStringRef; external name '_kCGImagePropertyIPTCContactInfoCountry'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_6, __IPHONE_4_0) *)
var kCGImagePropertyIPTCContactInfoAddress: CFStringRef; external name '_kCGImagePropertyIPTCContactInfoAddress'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_6, __IPHONE_4_0) *)
var kCGImagePropertyIPTCContactInfoPostalCode: CFStringRef; external name '_kCGImagePropertyIPTCContactInfoPostalCode'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_6, __IPHONE_4_0) *)
var kCGImagePropertyIPTCContactInfoStateProvince: CFStringRef; external name '_kCGImagePropertyIPTCContactInfoStateProvince'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_6, __IPHONE_4_0) *)
var kCGImagePropertyIPTCContactInfoEmails: CFStringRef; external name '_kCGImagePropertyIPTCContactInfoEmails'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_6, __IPHONE_4_0) *)
var kCGImagePropertyIPTCContactInfoPhones: CFStringRef; external name '_kCGImagePropertyIPTCContactInfoPhones'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_6, __IPHONE_4_0) *)
var kCGImagePropertyIPTCContactInfoWebURLs: CFStringRef; external name '_kCGImagePropertyIPTCContactInfoWebURLs'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_6, __IPHONE_4_0) *)

{ Possible keys for kCGImageProperty8BIMDictionary }

var kCGImageProperty8BIMLayerNames: CFStringRef; external name '_kCGImageProperty8BIMLayerNames'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_4_0) *)


{ Possible keys for kCGImagePropertyDNGDictionary }

var kCGImagePropertyDNGVersion: CFStringRef; external name '_kCGImagePropertyDNGVersion'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_4_0) *)
var kCGImagePropertyDNGBackwardVersion: CFStringRef; external name '_kCGImagePropertyDNGBackwardVersion'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_4_0) *)
var kCGImagePropertyDNGUniqueCameraModel: CFStringRef; external name '_kCGImagePropertyDNGUniqueCameraModel'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_4_0) *)
var kCGImagePropertyDNGLocalizedCameraModel: CFStringRef; external name '_kCGImagePropertyDNGLocalizedCameraModel'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_4_0) *)
var kCGImagePropertyDNGCameraSerialNumber: CFStringRef; external name '_kCGImagePropertyDNGCameraSerialNumber'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_4_0) *)
var kCGImagePropertyDNGLensInfo: CFStringRef; external name '_kCGImagePropertyDNGLensInfo'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_4_0) *)


{ Possible keys for kCGImagePropertyCIFFDictionary }

var kCGImagePropertyCIFFDescription: CFStringRef; external name '_kCGImagePropertyCIFFDescription'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_4_0) *)
var kCGImagePropertyCIFFFirmware: CFStringRef; external name '_kCGImagePropertyCIFFFirmware'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_4_0) *)
var kCGImagePropertyCIFFOwnerName: CFStringRef; external name '_kCGImagePropertyCIFFOwnerName'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_4_0) *)
var kCGImagePropertyCIFFImageName: CFStringRef; external name '_kCGImagePropertyCIFFImageName'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_4_0) *)
var kCGImagePropertyCIFFImageFileName: CFStringRef; external name '_kCGImagePropertyCIFFImageFileName'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_4_0) *)
var kCGImagePropertyCIFFReleaseMethod: CFStringRef; external name '_kCGImagePropertyCIFFReleaseMethod'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_4_0) *)
var kCGImagePropertyCIFFReleaseTiming: CFStringRef; external name '_kCGImagePropertyCIFFReleaseTiming'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_4_0) *)
var kCGImagePropertyCIFFRecordID: CFStringRef; external name '_kCGImagePropertyCIFFRecordID'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_4_0) *)
var kCGImagePropertyCIFFSelfTimingTime: CFStringRef; external name '_kCGImagePropertyCIFFSelfTimingTime'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_4_0) *)
var kCGImagePropertyCIFFCameraSerialNumber: CFStringRef; external name '_kCGImagePropertyCIFFCameraSerialNumber'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_4_0) *)
var kCGImagePropertyCIFFImageSerialNumber: CFStringRef; external name '_kCGImagePropertyCIFFImageSerialNumber'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_4_0) *)
var kCGImagePropertyCIFFContinuousDrive: CFStringRef; external name '_kCGImagePropertyCIFFContinuousDrive'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_4_0) *)
var kCGImagePropertyCIFFFocusMode: CFStringRef; external name '_kCGImagePropertyCIFFFocusMode'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_4_0) *)
var kCGImagePropertyCIFFMeteringMode: CFStringRef; external name '_kCGImagePropertyCIFFMeteringMode'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_4_0) *)
var kCGImagePropertyCIFFShootingMode: CFStringRef; external name '_kCGImagePropertyCIFFShootingMode'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_4_0) *)
var kCGImagePropertyCIFFLensModel: CFStringRef; external name '_kCGImagePropertyCIFFLensModel'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_4_0) *)
var kCGImagePropertyCIFFLensMaxMM: CFStringRef; external name '_kCGImagePropertyCIFFLensMaxMM'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_4_0) *)
var kCGImagePropertyCIFFLensMinMM: CFStringRef; external name '_kCGImagePropertyCIFFLensMinMM'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_4_0) *)
var kCGImagePropertyCIFFWhiteBalanceIndex: CFStringRef; external name '_kCGImagePropertyCIFFWhiteBalanceIndex'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_4_0) *)
var kCGImagePropertyCIFFFlashExposureComp: CFStringRef; external name '_kCGImagePropertyCIFFFlashExposureComp'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_4_0) *)
var kCGImagePropertyCIFFMeasuredEV: CFStringRef; external name '_kCGImagePropertyCIFFMeasuredEV'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_4_0) *)


{ Possible keys for kCGImagePropertyMakerNikonDictionary }

var kCGImagePropertyMakerNikonISOSetting: CFStringRef; external name '_kCGImagePropertyMakerNikonISOSetting'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_4_0) *)
var kCGImagePropertyMakerNikonColorMode: CFStringRef; external name '_kCGImagePropertyMakerNikonColorMode'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_4_0) *)
var kCGImagePropertyMakerNikonQuality: CFStringRef; external name '_kCGImagePropertyMakerNikonQuality'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_4_0) *)
var kCGImagePropertyMakerNikonWhiteBalanceMode: CFStringRef; external name '_kCGImagePropertyMakerNikonWhiteBalanceMode'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_4_0) *)
var kCGImagePropertyMakerNikonSharpenMode: CFStringRef; external name '_kCGImagePropertyMakerNikonSharpenMode'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_4_0) *)
var kCGImagePropertyMakerNikonFocusMode: CFStringRef; external name '_kCGImagePropertyMakerNikonFocusMode'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_4_0) *)
var kCGImagePropertyMakerNikonFlashSetting: CFStringRef; external name '_kCGImagePropertyMakerNikonFlashSetting'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_4_0) *)
var kCGImagePropertyMakerNikonISOSelection: CFStringRef; external name '_kCGImagePropertyMakerNikonISOSelection'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_4_0) *)
var kCGImagePropertyMakerNikonFlashExposureComp: CFStringRef; external name '_kCGImagePropertyMakerNikonFlashExposureComp'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_4_0) *)
var kCGImagePropertyMakerNikonImageAdjustment: CFStringRef; external name '_kCGImagePropertyMakerNikonImageAdjustment'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_4_0) *)
var kCGImagePropertyMakerNikonLensAdapter: CFStringRef; external name '_kCGImagePropertyMakerNikonLensAdapter'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_4_0) *)
var kCGImagePropertyMakerNikonLensType: CFStringRef; external name '_kCGImagePropertyMakerNikonLensType'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_4_0) *)
var kCGImagePropertyMakerNikonLensInfo: CFStringRef; external name '_kCGImagePropertyMakerNikonLensInfo'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_4_0) *)
var kCGImagePropertyMakerNikonFocusDistance: CFStringRef; external name '_kCGImagePropertyMakerNikonFocusDistance'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_4_0) *)
var kCGImagePropertyMakerNikonDigitalZoom: CFStringRef; external name '_kCGImagePropertyMakerNikonDigitalZoom'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_4_0) *)
var kCGImagePropertyMakerNikonShootingMode: CFStringRef; external name '_kCGImagePropertyMakerNikonShootingMode'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_4_0) *)
var kCGImagePropertyMakerNikonCameraSerialNumber: CFStringRef; external name '_kCGImagePropertyMakerNikonCameraSerialNumber'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_4_0) *)
var kCGImagePropertyMakerNikonShutterCount: CFStringRef; external name '_kCGImagePropertyMakerNikonShutterCount'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_4_0) *)

{ Possible keys for kCGImagePropertyMakerCanonDictionary }

var kCGImagePropertyMakerCanonOwnerName: CFStringRef; external name '_kCGImagePropertyMakerCanonOwnerName'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_4_0) *)
var kCGImagePropertyMakerCanonCameraSerialNumber: CFStringRef; external name '_kCGImagePropertyMakerCanonCameraSerialNumber'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_4_0) *)
var kCGImagePropertyMakerCanonImageSerialNumber: CFStringRef; external name '_kCGImagePropertyMakerCanonImageSerialNumber'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_4_0) *)
var kCGImagePropertyMakerCanonFlashExposureComp: CFStringRef; external name '_kCGImagePropertyMakerCanonFlashExposureComp'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_4_0) *)
var kCGImagePropertyMakerCanonContinuousDrive: CFStringRef; external name '_kCGImagePropertyMakerCanonContinuousDrive'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_4_0) *)
var kCGImagePropertyMakerCanonLensModel: CFStringRef; external name '_kCGImagePropertyMakerCanonLensModel'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_4_0) *)
var kCGImagePropertyMakerCanonFirmware: CFStringRef; external name '_kCGImagePropertyMakerCanonFirmware'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_4_0) *)
var kCGImagePropertyMakerCanonAspectRatioInfo: CFStringRef; external name '_kCGImagePropertyMakerCanonAspectRatioInfo'; (* attribute const *)
(* IMAGEIO_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_4_0) *)

{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}
