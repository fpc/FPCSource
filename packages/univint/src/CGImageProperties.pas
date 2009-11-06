{
 * ImageIO - CGImageProperties.h
 * Copyright (c) 2004 Apple Computer, Inc. All rights reserved.
 *
 }
{	 Pascal Translation:  Gale R Paeper, <gpaeper@empirenet.com>, 2006 }
{	 Pascal Translation Updated:  Jonas Maebe, <jonas@freepascal.org>, October 2009 }
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
uses MacTypes,CFBase,CGBase;
{$endc} {not MACOSALLINCLUDE}


{$ifc TARGET_OS_MAC}

{$ALIGN POWER}


{ Properties that, if returned by CGImageSourceCopyProperties or 
 * CGImageSourceCopyPropertiesAtIndex, contain a dictionary of file-format 
 * or metadata-format specific key-values. }

var kCGImagePropertyTIFFDictionary: CFStringRef; external name '_kCGImagePropertyTIFFDictionary'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyGIFDictionary: CFStringRef; external name '_kCGImagePropertyGIFDictionary'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyJFIFDictionary: CFStringRef; external name '_kCGImagePropertyJFIFDictionary'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyExifDictionary: CFStringRef; external name '_kCGImagePropertyExifDictionary'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyPNGDictionary: CFStringRef; external name '_kCGImagePropertyPNGDictionary'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyIPTCDictionary: CFStringRef; external name '_kCGImagePropertyIPTCDictionary'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyGPSDictionary: CFStringRef; external name '_kCGImagePropertyGPSDictionary'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyRawDictionary: CFStringRef; external name '_kCGImagePropertyRawDictionary'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyCIFFDictionary: CFStringRef; external name '_kCGImagePropertyCIFFDictionary'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyMakerCanonDictionary: CFStringRef; external name '_kCGImagePropertyMakerCanonDictionary'; (* attribute const *)
var kCGImagePropertyMakerNikonDictionary: CFStringRef; external name '_kCGImagePropertyMakerNikonDictionary'; (* attribute const *)
var kCGImagePropertyMakerMinoltaDictionary: CFStringRef; external name '_kCGImagePropertyMakerMinoltaDictionary'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA) *)
var kCGImagePropertyMakerFujiDictionary: CFStringRef; external name '_kCGImagePropertyMakerFujiDictionary'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA) *)
var kCGImagePropertyMakerOlympusDictionary: CFStringRef; external name '_kCGImagePropertyMakerOlympusDictionary'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA) *)
var kCGImagePropertyMakerPentaxDictionary: CFStringRef; external name '_kCGImagePropertyMakerPentaxDictionary'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA) *)
var kCGImageProperty8BIMDictionary: CFStringRef; external name '_kCGImageProperty8BIMDictionary'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyDNGDictionary: CFStringRef; external name '_kCGImagePropertyDNGDictionary'; (* attribute const *)
var kCGImagePropertyExifAuxDictionary: CFStringRef; external name '_kCGImagePropertyExifAuxDictionary'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA) *)


{* Properties which may be returned by "CGImageSourceCopyProperties".  The
 ** values apply to the container in general but not necessarily to any
 ** individual image that it contains. *}

{ The size of the image file in bytes, if known. If present, the value of
 * this key is a CFNumberRef. }

var kCGImagePropertyFileSize: CFStringRef; external name '_kCGImagePropertyFileSize'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)


{* Properties which may be returned by "CGImageSourceCopyPropertiesAtIndex".
 ** The values apply to a single image of an image source file. *}

{ The number of pixels in the x- and y-dimensions. The value of these keys 
 * is a CFNumberRef. }

var kCGImagePropertyPixelHeight: CFStringRef; external name '_kCGImagePropertyPixelHeight'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyPixelWidth: CFStringRef; external name '_kCGImagePropertyPixelWidth'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)

{ The DPI in the x- and y-dimensions, if known. If present, the value of
 * these keys is a CFNumberRef. }

var kCGImagePropertyDPIHeight: CFStringRef; external name '_kCGImagePropertyDPIHeight'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyDPIWidth: CFStringRef; external name '_kCGImagePropertyDPIWidth'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)

{ The number of bits in each color sample of each pixel. The value of this 
 * key is a CFNumberRef. }

var kCGImagePropertyDepth: CFStringRef; external name '_kCGImagePropertyDepth'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)

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
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)

{ The value of this key is kCFBooleanTrue if the image contains floating- 
 * point pixel samples } 
 
var kCGImagePropertyIsFloat: CFStringRef; external name '_kCGImagePropertyIsFloat'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)

{ The value of this key is kCFBooleanTrue if the image contains indexed 
 * (a.k.a. paletted) pixel samples } 
 
var kCGImagePropertyIsIndexed: CFStringRef; external name '_kCGImagePropertyIsIndexed'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)

{ The value of this key is kCFBooleanTrue if the image contains an alpha 
 * (a.k.a. coverage) channel } 
 
var kCGImagePropertyHasAlpha: CFStringRef; external name '_kCGImagePropertyHasAlpha'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)

{ The color model of the image such as "RGB", "CMYK", "Gray", or "Lab".
 * The value of this key is CFStringRef. } 

var kCGImagePropertyColorModel: CFStringRef; external name '_kCGImagePropertyColorModel'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)

{ The name of the optional ICC profile embedded in the image, if known.  
 * If present, the value of this key is a CFStringRef. }

var kCGImagePropertyProfileName: CFStringRef; external name '_kCGImagePropertyProfileName'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)


{ Possible values for kCGImagePropertyColorModel property }

var kCGImagePropertyColorModelRGB: CFStringRef; external name '_kCGImagePropertyColorModelRGB'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyColorModelGray: CFStringRef; external name '_kCGImagePropertyColorModelGray'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyColorModelCMYK: CFStringRef; external name '_kCGImagePropertyColorModelCMYK'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyColorModelLab: CFStringRef; external name '_kCGImagePropertyColorModelLab'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)


{ Possible keys for kCGImagePropertyTIFFDictionary }

var kCGImagePropertyTIFFCompression: CFStringRef; external name '_kCGImagePropertyTIFFCompression'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyTIFFPhotometricInterpretation: CFStringRef; external name '_kCGImagePropertyTIFFPhotometricInterpretation'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyTIFFDocumentName: CFStringRef; external name '_kCGImagePropertyTIFFDocumentName'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyTIFFImageDescription: CFStringRef; external name '_kCGImagePropertyTIFFImageDescription'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyTIFFMake: CFStringRef; external name '_kCGImagePropertyTIFFMake'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyTIFFModel: CFStringRef; external name '_kCGImagePropertyTIFFModel'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyTIFFOrientation: CFStringRef; external name '_kCGImagePropertyTIFFOrientation'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyTIFFXResolution: CFStringRef; external name '_kCGImagePropertyTIFFXResolution'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyTIFFYResolution: CFStringRef; external name '_kCGImagePropertyTIFFYResolution'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyTIFFResolutionUnit: CFStringRef; external name '_kCGImagePropertyTIFFResolutionUnit'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyTIFFSoftware: CFStringRef; external name '_kCGImagePropertyTIFFSoftware'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyTIFFTransferFunction: CFStringRef; external name '_kCGImagePropertyTIFFTransferFunction'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyTIFFDateTime: CFStringRef; external name '_kCGImagePropertyTIFFDateTime'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyTIFFArtist: CFStringRef; external name '_kCGImagePropertyTIFFArtist'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyTIFFHostComputer: CFStringRef; external name '_kCGImagePropertyTIFFHostComputer'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyTIFFCopyright: CFStringRef; external name '_kCGImagePropertyTIFFCopyright'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyTIFFWhitePoint: CFStringRef; external name '_kCGImagePropertyTIFFWhitePoint'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyTIFFPrimaryChromaticities: CFStringRef; external name '_kCGImagePropertyTIFFPrimaryChromaticities'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)

{ Possible keys for kCGImagePropertyJFIFDictionary }

var kCGImagePropertyJFIFVersion: CFStringRef; external name '_kCGImagePropertyJFIFVersion'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyJFIFXDensity: CFStringRef; external name '_kCGImagePropertyJFIFXDensity'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyJFIFYDensity: CFStringRef; external name '_kCGImagePropertyJFIFYDensity'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyJFIFDensityUnit: CFStringRef; external name '_kCGImagePropertyJFIFDensityUnit'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyJFIFIsProgressive: CFStringRef; external name '_kCGImagePropertyJFIFIsProgressive'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)


{ Possible keys for kCGImagePropertyExifDictionary }

var kCGImagePropertyExifExposureTime: CFStringRef; external name '_kCGImagePropertyExifExposureTime'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyExifFNumber: CFStringRef; external name '_kCGImagePropertyExifFNumber'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyExifExposureProgram: CFStringRef; external name '_kCGImagePropertyExifExposureProgram'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyExifSpectralSensitivity: CFStringRef; external name '_kCGImagePropertyExifSpectralSensitivity'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyExifISOSpeedRatings: CFStringRef; external name '_kCGImagePropertyExifISOSpeedRatings'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyExifOECF: CFStringRef; external name '_kCGImagePropertyExifOECF'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyExifVersion: CFStringRef; external name '_kCGImagePropertyExifVersion'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyExifDateTimeOriginal: CFStringRef; external name '_kCGImagePropertyExifDateTimeOriginal'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyExifDateTimeDigitized: CFStringRef; external name '_kCGImagePropertyExifDateTimeDigitized'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyExifComponentsConfiguration: CFStringRef; external name '_kCGImagePropertyExifComponentsConfiguration'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyExifCompressedBitsPerPixel: CFStringRef; external name '_kCGImagePropertyExifCompressedBitsPerPixel'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyExifShutterSpeedValue: CFStringRef; external name '_kCGImagePropertyExifShutterSpeedValue'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyExifApertureValue: CFStringRef; external name '_kCGImagePropertyExifApertureValue'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyExifBrightnessValue: CFStringRef; external name '_kCGImagePropertyExifBrightnessValue'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyExifExposureBiasValue: CFStringRef; external name '_kCGImagePropertyExifExposureBiasValue'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyExifMaxApertureValue: CFStringRef; external name '_kCGImagePropertyExifMaxApertureValue'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyExifSubjectDistance: CFStringRef; external name '_kCGImagePropertyExifSubjectDistance'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyExifMeteringMode: CFStringRef; external name '_kCGImagePropertyExifMeteringMode'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyExifLightSource: CFStringRef; external name '_kCGImagePropertyExifLightSource'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyExifFlash: CFStringRef; external name '_kCGImagePropertyExifFlash'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyExifFocalLength: CFStringRef; external name '_kCGImagePropertyExifFocalLength'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyExifSubjectArea: CFStringRef; external name '_kCGImagePropertyExifSubjectArea'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyExifMakerNote: CFStringRef; external name '_kCGImagePropertyExifMakerNote'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyExifUserComment: CFStringRef; external name '_kCGImagePropertyExifUserComment'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyExifSubsecTime: CFStringRef; external name '_kCGImagePropertyExifSubsecTime'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyExifSubsecTimeOrginal: CFStringRef; external name '_kCGImagePropertyExifSubsecTimeOrginal'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyExifSubsecTimeDigitized: CFStringRef; external name '_kCGImagePropertyExifSubsecTimeDigitized'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyExifFlashPixVersion: CFStringRef; external name '_kCGImagePropertyExifFlashPixVersion'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyExifColorSpace: CFStringRef; external name '_kCGImagePropertyExifColorSpace'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyExifPixelXDimension: CFStringRef; external name '_kCGImagePropertyExifPixelXDimension'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyExifPixelYDimension: CFStringRef; external name '_kCGImagePropertyExifPixelYDimension'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyExifRelatedSoundFile: CFStringRef; external name '_kCGImagePropertyExifRelatedSoundFile'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyExifFlashEnergy: CFStringRef; external name '_kCGImagePropertyExifFlashEnergy'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyExifSpatialFrequencyResponse: CFStringRef; external name '_kCGImagePropertyExifSpatialFrequencyResponse'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyExifFocalPlaneXResolution: CFStringRef; external name '_kCGImagePropertyExifFocalPlaneXResolution'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyExifFocalPlaneYResolution: CFStringRef; external name '_kCGImagePropertyExifFocalPlaneYResolution'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyExifFocalPlaneResolutionUnit: CFStringRef; external name '_kCGImagePropertyExifFocalPlaneResolutionUnit'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyExifSubjectLocation: CFStringRef; external name '_kCGImagePropertyExifSubjectLocation'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyExifExposureIndex: CFStringRef; external name '_kCGImagePropertyExifExposureIndex'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyExifSensingMethod: CFStringRef; external name '_kCGImagePropertyExifSensingMethod'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyExifFileSource: CFStringRef; external name '_kCGImagePropertyExifFileSource'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyExifSceneType: CFStringRef; external name '_kCGImagePropertyExifSceneType'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyExifCFAPattern: CFStringRef; external name '_kCGImagePropertyExifCFAPattern'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyExifCustomRendered: CFStringRef; external name '_kCGImagePropertyExifCustomRendered'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyExifExposureMode: CFStringRef; external name '_kCGImagePropertyExifExposureMode'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyExifWhiteBalance: CFStringRef; external name '_kCGImagePropertyExifWhiteBalance'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyExifDigitalZoomRatio: CFStringRef; external name '_kCGImagePropertyExifDigitalZoomRatio'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyExifFocalLenIn35mmFilm: CFStringRef; external name '_kCGImagePropertyExifFocalLenIn35mmFilm'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyExifSceneCaptureType: CFStringRef; external name '_kCGImagePropertyExifSceneCaptureType'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyExifGainControl: CFStringRef; external name '_kCGImagePropertyExifGainControl'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyExifContrast: CFStringRef; external name '_kCGImagePropertyExifContrast'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyExifSaturation: CFStringRef; external name '_kCGImagePropertyExifSaturation'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyExifSharpness: CFStringRef; external name '_kCGImagePropertyExifSharpness'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyExifDeviceSettingDescription: CFStringRef; external name '_kCGImagePropertyExifDeviceSettingDescription'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyExifSubjectDistRange: CFStringRef; external name '_kCGImagePropertyExifSubjectDistRange'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyExifImageUniqueID: CFStringRef; external name '_kCGImagePropertyExifImageUniqueID'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyExifGamma: CFStringRef; external name '_kCGImagePropertyExifGamma'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)

{ Possible keys for kCGImagePropertyExifAuxDictionary }
var kCGImagePropertyExifAuxLensInfo: CFStringRef; external name '_kCGImagePropertyExifAuxLensInfo'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA) *)
var kCGImagePropertyExifAuxLensModel: CFStringRef; external name '_kCGImagePropertyExifAuxLensModel'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA) *)
var kCGImagePropertyExifAuxSerialNumber: CFStringRef; external name '_kCGImagePropertyExifAuxSerialNumber'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA) *)
var kCGImagePropertyExifAuxLensID: CFStringRef; external name '_kCGImagePropertyExifAuxLensID'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA) *)
var kCGImagePropertyExifAuxLensSerialNumber: CFStringRef; external name '_kCGImagePropertyExifAuxLensSerialNumber'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA) *)
var kCGImagePropertyExifAuxImageNumber: CFStringRef; external name '_kCGImagePropertyExifAuxImageNumber'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA) *)
var kCGImagePropertyExifAuxFlashCompensation: CFStringRef; external name '_kCGImagePropertyExifAuxFlashCompensation'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA) *)
var kCGImagePropertyExifAuxOwnerName: CFStringRef; external name '_kCGImagePropertyExifAuxOwnerName'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA) *)
var kCGImagePropertyExifAuxFirmware: CFStringRef; external name '_kCGImagePropertyExifAuxFirmware'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA) *)

{ Possible keys for kCGImagePropertyGIFDictionary }

var kCGImagePropertyGIFLoopCount: CFStringRef; external name '_kCGImagePropertyGIFLoopCount'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyGIFDelayTime: CFStringRef; external name '_kCGImagePropertyGIFDelayTime'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyGIFImageColorMap: CFStringRef; external name '_kCGImagePropertyGIFImageColorMap'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyGIFHasGlobalColorMap: CFStringRef; external name '_kCGImagePropertyGIFHasGlobalColorMap'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)

{ Possible keys for kCGImagePropertyPNGDictionary }

var kCGImagePropertyPNGGamma: CFStringRef; external name '_kCGImagePropertyPNGGamma'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyPNGInterlaceType: CFStringRef; external name '_kCGImagePropertyPNGInterlaceType'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyPNGXPixelsPerMeter: CFStringRef; external name '_kCGImagePropertyPNGXPixelsPerMeter'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyPNGYPixelsPerMeter: CFStringRef; external name '_kCGImagePropertyPNGYPixelsPerMeter'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyPNGsRGBIntent: CFStringRef; external name '_kCGImagePropertyPNGsRGBIntent'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyPNGChromaticities: CFStringRef; external name '_kCGImagePropertyPNGChromaticities'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)

{ Possible keys for kCGImagePropertyGPSDictionary }

var kCGImagePropertyGPSVersion: CFStringRef; external name '_kCGImagePropertyGPSVersion'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyGPSLatitudeRef: CFStringRef; external name '_kCGImagePropertyGPSLatitudeRef'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyGPSLatitude: CFStringRef; external name '_kCGImagePropertyGPSLatitude'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyGPSLongitudeRef: CFStringRef; external name '_kCGImagePropertyGPSLongitudeRef'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyGPSLongitude: CFStringRef; external name '_kCGImagePropertyGPSLongitude'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyGPSAltitudeRef: CFStringRef; external name '_kCGImagePropertyGPSAltitudeRef'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyGPSAltitude: CFStringRef; external name '_kCGImagePropertyGPSAltitude'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyGPSTimeStamp: CFStringRef; external name '_kCGImagePropertyGPSTimeStamp'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyGPSSatellites: CFStringRef; external name '_kCGImagePropertyGPSSatellites'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyGPSStatus: CFStringRef; external name '_kCGImagePropertyGPSStatus'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyGPSMeasureMode: CFStringRef; external name '_kCGImagePropertyGPSMeasureMode'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyGPSDOP: CFStringRef; external name '_kCGImagePropertyGPSDOP'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyGPSSpeedRef: CFStringRef; external name '_kCGImagePropertyGPSSpeedRef'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyGPSSpeed: CFStringRef; external name '_kCGImagePropertyGPSSpeed'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyGPSTrackRef: CFStringRef; external name '_kCGImagePropertyGPSTrackRef'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyGPSTrack: CFStringRef; external name '_kCGImagePropertyGPSTrack'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyGPSImgDirectionRef: CFStringRef; external name '_kCGImagePropertyGPSImgDirectionRef'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyGPSImgDirection: CFStringRef; external name '_kCGImagePropertyGPSImgDirection'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyGPSMapDatum: CFStringRef; external name '_kCGImagePropertyGPSMapDatum'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyGPSDestLatitudeRef: CFStringRef; external name '_kCGImagePropertyGPSDestLatitudeRef'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyGPSDestLatitude: CFStringRef; external name '_kCGImagePropertyGPSDestLatitude'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyGPSDestLongitudeRef: CFStringRef; external name '_kCGImagePropertyGPSDestLongitudeRef'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyGPSDestLongitude: CFStringRef; external name '_kCGImagePropertyGPSDestLongitude'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyGPSDestBearingRef: CFStringRef; external name '_kCGImagePropertyGPSDestBearingRef'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyGPSDestBearing: CFStringRef; external name '_kCGImagePropertyGPSDestBearing'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyGPSDestDistanceRef: CFStringRef; external name '_kCGImagePropertyGPSDestDistanceRef'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyGPSDestDistance: CFStringRef; external name '_kCGImagePropertyGPSDestDistance'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyGPSProcessingMethod: CFStringRef; external name '_kCGImagePropertyGPSProcessingMethod'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyGPSAreaInformation: CFStringRef; external name '_kCGImagePropertyGPSAreaInformation'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyGPSDateStamp: CFStringRef; external name '_kCGImagePropertyGPSDateStamp'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyGPSDifferental: CFStringRef; external name '_kCGImagePropertyGPSDifferental'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)

{ Possible keys for kCGImagePropertyIPTCDictionary }

var kCGImagePropertyIPTCObjectTypeReference: CFStringRef; external name '_kCGImagePropertyIPTCObjectTypeReference'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyIPTCObjectAttributeReference: CFStringRef; external name '_kCGImagePropertyIPTCObjectAttributeReference'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyIPTCObjectName: CFStringRef; external name '_kCGImagePropertyIPTCObjectName'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyIPTCEditStatus: CFStringRef; external name '_kCGImagePropertyIPTCEditStatus'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyIPTCEditorialUpdate: CFStringRef; external name '_kCGImagePropertyIPTCEditorialUpdate'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyIPTCUrgency: CFStringRef; external name '_kCGImagePropertyIPTCUrgency'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyIPTCSubjectReference: CFStringRef; external name '_kCGImagePropertyIPTCSubjectReference'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyIPTCCategory: CFStringRef; external name '_kCGImagePropertyIPTCCategory'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyIPTCSupplementalCategory: CFStringRef; external name '_kCGImagePropertyIPTCSupplementalCategory'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyIPTCFixtureIdentifier: CFStringRef; external name '_kCGImagePropertyIPTCFixtureIdentifier'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyIPTCKeywords: CFStringRef; external name '_kCGImagePropertyIPTCKeywords'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyIPTCContentLocationCode: CFStringRef; external name '_kCGImagePropertyIPTCContentLocationCode'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyIPTCContentLocationName: CFStringRef; external name '_kCGImagePropertyIPTCContentLocationName'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyIPTCReleaseDate: CFStringRef; external name '_kCGImagePropertyIPTCReleaseDate'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyIPTCReleaseTime: CFStringRef; external name '_kCGImagePropertyIPTCReleaseTime'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyIPTCExpirationDate: CFStringRef; external name '_kCGImagePropertyIPTCExpirationDate'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyIPTCExpirationTime: CFStringRef; external name '_kCGImagePropertyIPTCExpirationTime'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyIPTCSpecialInstructions: CFStringRef; external name '_kCGImagePropertyIPTCSpecialInstructions'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyIPTCActionAdvised: CFStringRef; external name '_kCGImagePropertyIPTCActionAdvised'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyIPTCReferenceService: CFStringRef; external name '_kCGImagePropertyIPTCReferenceService'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyIPTCReferenceDate: CFStringRef; external name '_kCGImagePropertyIPTCReferenceDate'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyIPTCReferenceNumber: CFStringRef; external name '_kCGImagePropertyIPTCReferenceNumber'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyIPTCDateCreated: CFStringRef; external name '_kCGImagePropertyIPTCDateCreated'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyIPTCTimeCreated: CFStringRef; external name '_kCGImagePropertyIPTCTimeCreated'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyIPTCDigitalCreationDate: CFStringRef; external name '_kCGImagePropertyIPTCDigitalCreationDate'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyIPTCDigitalCreationTime: CFStringRef; external name '_kCGImagePropertyIPTCDigitalCreationTime'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyIPTCOriginatingProgram: CFStringRef; external name '_kCGImagePropertyIPTCOriginatingProgram'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyIPTCProgramVersion: CFStringRef; external name '_kCGImagePropertyIPTCProgramVersion'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyIPTCObjectCycle: CFStringRef; external name '_kCGImagePropertyIPTCObjectCycle'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyIPTCByline: CFStringRef; external name '_kCGImagePropertyIPTCByline'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyIPTCBylineTitle: CFStringRef; external name '_kCGImagePropertyIPTCBylineTitle'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyIPTCCity: CFStringRef; external name '_kCGImagePropertyIPTCCity'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyIPTCSubLocation: CFStringRef; external name '_kCGImagePropertyIPTCSubLocation'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyIPTCProvinceState: CFStringRef; external name '_kCGImagePropertyIPTCProvinceState'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyIPTCCountryPrimaryLocationCode: CFStringRef; external name '_kCGImagePropertyIPTCCountryPrimaryLocationCode'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyIPTCCountryPrimaryLocationName: CFStringRef; external name '_kCGImagePropertyIPTCCountryPrimaryLocationName'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyIPTCOriginalTransmissionReference: CFStringRef; external name '_kCGImagePropertyIPTCOriginalTransmissionReference'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyIPTCHeadline: CFStringRef; external name '_kCGImagePropertyIPTCHeadline'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyIPTCCredit: CFStringRef; external name '_kCGImagePropertyIPTCCredit'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyIPTCSource: CFStringRef; external name '_kCGImagePropertyIPTCSource'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyIPTCCopyrightNotice: CFStringRef; external name '_kCGImagePropertyIPTCCopyrightNotice'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyIPTCContact: CFStringRef; external name '_kCGImagePropertyIPTCContact'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyIPTCCaptionAbstract: CFStringRef; external name '_kCGImagePropertyIPTCCaptionAbstract'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyIPTCWriterEditor: CFStringRef; external name '_kCGImagePropertyIPTCWriterEditor'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyIPTCImageType: CFStringRef; external name '_kCGImagePropertyIPTCImageType'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyIPTCImageOrientation: CFStringRef; external name '_kCGImagePropertyIPTCImageOrientation'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyIPTCLanguageIdentifier: CFStringRef; external name '_kCGImagePropertyIPTCLanguageIdentifier'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyIPTCStarRating: CFStringRef; external name '_kCGImagePropertyIPTCStarRating'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)
var kCGImagePropertyIPTCCreatorContactInfo: CFStringRef; external name '_kCGImagePropertyIPTCCreatorContactInfo'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_6, __IPHONE_NA) *)	// IPTC Core
var kCGImagePropertyIPTCRightsUsageTerms: CFStringRef; external name '_kCGImagePropertyIPTCRightsUsageTerms'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_6, __IPHONE_NA) *)	// IPTC Core
var kCGImagePropertyIPTCScene: CFStringRef; external name '_kCGImagePropertyIPTCScene'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_6, __IPHONE_NA) *)				// IPTC Core

{ Possible keys for kCGImagePropertyIPTCCreatorContactInfo dictionary (part of IPTC Core - above) }

var kCGImagePropertyIPTCContactInfoCity: CFStringRef; external name '_kCGImagePropertyIPTCContactInfoCity'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_6, __IPHONE_NA) *)
var kCGImagePropertyIPTCContactInfoCountry: CFStringRef; external name '_kCGImagePropertyIPTCContactInfoCountry'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_6, __IPHONE_NA) *)
var kCGImagePropertyIPTCContactInfoAddress: CFStringRef; external name '_kCGImagePropertyIPTCContactInfoAddress'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_6, __IPHONE_NA) *)
var kCGImagePropertyIPTCContactInfoPostalCode: CFStringRef; external name '_kCGImagePropertyIPTCContactInfoPostalCode'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_6, __IPHONE_NA) *)
var kCGImagePropertyIPTCContactInfoStateProvince: CFStringRef; external name '_kCGImagePropertyIPTCContactInfoStateProvince'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_6, __IPHONE_NA) *)
var kCGImagePropertyIPTCContactInfoEmails: CFStringRef; external name '_kCGImagePropertyIPTCContactInfoEmails'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_6, __IPHONE_NA) *)
var kCGImagePropertyIPTCContactInfoPhones: CFStringRef; external name '_kCGImagePropertyIPTCContactInfoPhones'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_6, __IPHONE_NA) *)
var kCGImagePropertyIPTCContactInfoWebURLs: CFStringRef; external name '_kCGImagePropertyIPTCContactInfoWebURLs'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_6, __IPHONE_NA) *)

{ Possible keys for kCGImageProperty8BIMDictionary }

var kCGImageProperty8BIMLayerNames: CFStringRef; external name '_kCGImageProperty8BIMLayerNames'; (* attribute const *)


{ Possible keys for kCGImagePropertyDNGDictionary }

var kCGImagePropertyDNGVersion: CFStringRef; external name '_kCGImagePropertyDNGVersion'; (* attribute const *)
var kCGImagePropertyDNGBackwardVersion: CFStringRef; external name '_kCGImagePropertyDNGBackwardVersion'; (* attribute const *)
var kCGImagePropertyDNGUniqueCameraModel: CFStringRef; external name '_kCGImagePropertyDNGUniqueCameraModel'; (* attribute const *)
var kCGImagePropertyDNGLocalizedCameraModel: CFStringRef; external name '_kCGImagePropertyDNGLocalizedCameraModel'; (* attribute const *)
var kCGImagePropertyDNGCameraSerialNumber: CFStringRef; external name '_kCGImagePropertyDNGCameraSerialNumber'; (* attribute const *)
var kCGImagePropertyDNGLensInfo: CFStringRef; external name '_kCGImagePropertyDNGLensInfo'; (* attribute const *)


{ Possible keys for kCGImagePropertyCIFFDictionary }

var kCGImagePropertyCIFFDescription: CFStringRef; external name '_kCGImagePropertyCIFFDescription'; (* attribute const *)
var kCGImagePropertyCIFFFirmware: CFStringRef; external name '_kCGImagePropertyCIFFFirmware'; (* attribute const *)
var kCGImagePropertyCIFFOwnerName: CFStringRef; external name '_kCGImagePropertyCIFFOwnerName'; (* attribute const *)
var kCGImagePropertyCIFFImageName: CFStringRef; external name '_kCGImagePropertyCIFFImageName'; (* attribute const *)
var kCGImagePropertyCIFFImageFileName: CFStringRef; external name '_kCGImagePropertyCIFFImageFileName'; (* attribute const *)
var kCGImagePropertyCIFFReleaseMethod: CFStringRef; external name '_kCGImagePropertyCIFFReleaseMethod'; (* attribute const *)
var kCGImagePropertyCIFFReleaseTiming: CFStringRef; external name '_kCGImagePropertyCIFFReleaseTiming'; (* attribute const *)
var kCGImagePropertyCIFFRecordID: CFStringRef; external name '_kCGImagePropertyCIFFRecordID'; (* attribute const *)
var kCGImagePropertyCIFFSelfTimingTime: CFStringRef; external name '_kCGImagePropertyCIFFSelfTimingTime'; (* attribute const *)
var kCGImagePropertyCIFFCameraSerialNumber: CFStringRef; external name '_kCGImagePropertyCIFFCameraSerialNumber'; (* attribute const *)
var kCGImagePropertyCIFFImageSerialNumber: CFStringRef; external name '_kCGImagePropertyCIFFImageSerialNumber'; (* attribute const *)
var kCGImagePropertyCIFFContinuousDrive: CFStringRef; external name '_kCGImagePropertyCIFFContinuousDrive'; (* attribute const *)
var kCGImagePropertyCIFFFocusMode: CFStringRef; external name '_kCGImagePropertyCIFFFocusMode'; (* attribute const *)
var kCGImagePropertyCIFFMeteringMode: CFStringRef; external name '_kCGImagePropertyCIFFMeteringMode'; (* attribute const *)
var kCGImagePropertyCIFFShootingMode: CFStringRef; external name '_kCGImagePropertyCIFFShootingMode'; (* attribute const *)
var kCGImagePropertyCIFFLensModel: CFStringRef; external name '_kCGImagePropertyCIFFLensModel'; (* attribute const *)
var kCGImagePropertyCIFFLensMaxMM: CFStringRef; external name '_kCGImagePropertyCIFFLensMaxMM'; (* attribute const *)
var kCGImagePropertyCIFFLensMinMM: CFStringRef; external name '_kCGImagePropertyCIFFLensMinMM'; (* attribute const *)
var kCGImagePropertyCIFFWhiteBalanceIndex: CFStringRef; external name '_kCGImagePropertyCIFFWhiteBalanceIndex'; (* attribute const *)
var kCGImagePropertyCIFFFlashExposureComp: CFStringRef; external name '_kCGImagePropertyCIFFFlashExposureComp'; (* attribute const *)
var kCGImagePropertyCIFFMeasuredEV: CFStringRef; external name '_kCGImagePropertyCIFFMeasuredEV'; (* attribute const *)


{ Possible keys for kCGImagePropertyMakerNikonDictionary }

var kCGImagePropertyMakerNikonISOSetting: CFStringRef; external name '_kCGImagePropertyMakerNikonISOSetting'; (* attribute const *)
var kCGImagePropertyMakerNikonColorMode: CFStringRef; external name '_kCGImagePropertyMakerNikonColorMode'; (* attribute const *)
var kCGImagePropertyMakerNikonQuality: CFStringRef; external name '_kCGImagePropertyMakerNikonQuality'; (* attribute const *)
var kCGImagePropertyMakerNikonWhiteBalanceMode: CFStringRef; external name '_kCGImagePropertyMakerNikonWhiteBalanceMode'; (* attribute const *)
var kCGImagePropertyMakerNikonSharpenMode: CFStringRef; external name '_kCGImagePropertyMakerNikonSharpenMode'; (* attribute const *)
var kCGImagePropertyMakerNikonFocusMode: CFStringRef; external name '_kCGImagePropertyMakerNikonFocusMode'; (* attribute const *)
var kCGImagePropertyMakerNikonFlashSetting: CFStringRef; external name '_kCGImagePropertyMakerNikonFlashSetting'; (* attribute const *)
var kCGImagePropertyMakerNikonISOSelection: CFStringRef; external name '_kCGImagePropertyMakerNikonISOSelection'; (* attribute const *)
var kCGImagePropertyMakerNikonFlashExposureComp: CFStringRef; external name '_kCGImagePropertyMakerNikonFlashExposureComp'; (* attribute const *)
var kCGImagePropertyMakerNikonImageAdjustment: CFStringRef; external name '_kCGImagePropertyMakerNikonImageAdjustment'; (* attribute const *)
var kCGImagePropertyMakerNikonLensAdapter: CFStringRef; external name '_kCGImagePropertyMakerNikonLensAdapter'; (* attribute const *)
var kCGImagePropertyMakerNikonLensType: CFStringRef; external name '_kCGImagePropertyMakerNikonLensType'; (* attribute const *)
var kCGImagePropertyMakerNikonLensInfo: CFStringRef; external name '_kCGImagePropertyMakerNikonLensInfo'; (* attribute const *)
var kCGImagePropertyMakerNikonFocusDistance: CFStringRef; external name '_kCGImagePropertyMakerNikonFocusDistance'; (* attribute const *)
var kCGImagePropertyMakerNikonDigitalZoom: CFStringRef; external name '_kCGImagePropertyMakerNikonDigitalZoom'; (* attribute const *)
var kCGImagePropertyMakerNikonShootingMode: CFStringRef; external name '_kCGImagePropertyMakerNikonShootingMode'; (* attribute const *)
var kCGImagePropertyMakerNikonCameraSerialNumber: CFStringRef; external name '_kCGImagePropertyMakerNikonCameraSerialNumber'; (* attribute const *)
var kCGImagePropertyMakerNikonShutterCount: CFStringRef; external name '_kCGImagePropertyMakerNikonShutterCount'; (* attribute const *)

{ Possible keys for kCGImagePropertyMakerCanonDictionary }

var kCGImagePropertyMakerCanonOwnerName: CFStringRef; external name '_kCGImagePropertyMakerCanonOwnerName'; (* attribute const *)
var kCGImagePropertyMakerCanonCameraSerialNumber: CFStringRef; external name '_kCGImagePropertyMakerCanonCameraSerialNumber'; (* attribute const *)
var kCGImagePropertyMakerCanonImageSerialNumber: CFStringRef; external name '_kCGImagePropertyMakerCanonImageSerialNumber'; (* attribute const *)
var kCGImagePropertyMakerCanonFlashExposureComp: CFStringRef; external name '_kCGImagePropertyMakerCanonFlashExposureComp'; (* attribute const *)
var kCGImagePropertyMakerCanonContinuousDrive: CFStringRef; external name '_kCGImagePropertyMakerCanonContinuousDrive'; (* attribute const *)
var kCGImagePropertyMakerCanonLensModel: CFStringRef; external name '_kCGImagePropertyMakerCanonLensModel'; (* attribute const *)
var kCGImagePropertyMakerCanonFirmware: CFStringRef; external name '_kCGImagePropertyMakerCanonFirmware'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA) *)
var kCGImagePropertyMakerCanonAspectRatioInfo: CFStringRef; external name '_kCGImagePropertyMakerCanonAspectRatioInfo'; (* attribute const *)
(* __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA) *)

{$endc} {TARGET_OS_MAC}

{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}
