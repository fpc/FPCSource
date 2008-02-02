{
 * ImageIO - CGImageProperties.h
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

unit CGImageProperties;
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
uses CFBase;
{$ALIGN POWER}


{ Properties that, if returned by CGImageSourceCopyProperties or 
 * CGImageSourceCopyPropertiesAtIndex, contain a dictionary of file-format 
 * or metadata-format specific key-values. }

var kCGImagePropertyTIFFDictionary: CFStringRef; external name '_kCGImagePropertyTIFFDictionary'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyGIFDictionary: CFStringRef; external name '_kCGImagePropertyGIFDictionary'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyJFIFDictionary: CFStringRef; external name '_kCGImagePropertyJFIFDictionary'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyExifDictionary: CFStringRef; external name '_kCGImagePropertyExifDictionary'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyPNGDictionary: CFStringRef; external name '_kCGImagePropertyPNGDictionary'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyIPTCDictionary: CFStringRef; external name '_kCGImagePropertyIPTCDictionary'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyGPSDictionary: CFStringRef; external name '_kCGImagePropertyGPSDictionary'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyRawDictionary: CFStringRef; external name '_kCGImagePropertyRawDictionary'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyCIFFDictionary: CFStringRef; external name '_kCGImagePropertyCIFFDictionary'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImageProperty8BIMDictionary: CFStringRef; external name '_kCGImageProperty8BIMDictionary'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{* Properties which may be returned by "CGImageSourceCopyProperties".  The
 ** values apply to the container in general but not necessarily to any
 ** individual image that it contains. *}

{ The size of the image file in bytes, if known. If present, the value of
 * this key is a CFNumberRef. }

var kCGImagePropertyFileSize: CFStringRef; external name '_kCGImagePropertyFileSize'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{* Properties which may be returned by "CGImageSourceCopyPropertiesAtIndex".
 ** The values apply to a single image of an image source file. *}

{ The number of pixels in the x- and y-dimensions. The value of these keys 
 * is a CFNumberRef. }

var kCGImagePropertyPixelHeight: CFStringRef; external name '_kCGImagePropertyPixelHeight'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyPixelWidth: CFStringRef; external name '_kCGImagePropertyPixelWidth'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ The DPI in the x- and y-dimensions, if known. If present, the value of
 * these keys is a CFNumberRef. }

var kCGImagePropertyDPIHeight: CFStringRef; external name '_kCGImagePropertyDPIHeight'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyDPIWidth: CFStringRef; external name '_kCGImagePropertyDPIWidth'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ The number of bits in each color sample of each pixel. The value of this 
 * key is a CFNumberRef. }

var kCGImagePropertyDepth: CFStringRef; external name '_kCGImagePropertyDepth'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

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
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ The value of this key is kCFBooleanTrue if the image contains floating- 
 * point pixel samples } 
 
var kCGImagePropertyIsFloat: CFStringRef; external name '_kCGImagePropertyIsFloat'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ The value of this key is kCFBooleanTrue if the image contains indexed 
 * (a.k.a. paletted) pixel samples } 
 
var kCGImagePropertyIsIndexed: CFStringRef; external name '_kCGImagePropertyIsIndexed'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ The value of this key is kCFBooleanTrue if the image contains an alpha 
 * (a.k.a. coverage) channel } 
 
var kCGImagePropertyHasAlpha: CFStringRef; external name '_kCGImagePropertyHasAlpha'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ The color model of the image such as "RGB", "CMYK", "Gray", or "Lab".
 * The value of this key is CFStringRef. } 

var kCGImagePropertyColorModel: CFStringRef; external name '_kCGImagePropertyColorModel'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ The name of the optional ICC profile embedded in the image, if known.  
 * If present, the value of this key is a CFStringRef. }

var kCGImagePropertyProfileName: CFStringRef; external name '_kCGImagePropertyProfileName'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Possible values for kCGImagePropertyColorModel property }

var kCGImagePropertyColorModelRGB: CFStringRef; external name '_kCGImagePropertyColorModelRGB'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyColorModelGray: CFStringRef; external name '_kCGImagePropertyColorModelGray'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyColorModelCMYK: CFStringRef; external name '_kCGImagePropertyColorModelCMYK'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyColorModelLab: CFStringRef; external name '_kCGImagePropertyColorModelLab'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Possible keys for kCGImagePropertyTIFFDictionary }

var kCGImagePropertyTIFFCompression: CFStringRef; external name '_kCGImagePropertyTIFFCompression'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyTIFFPhotometricInterpretation: CFStringRef; external name '_kCGImagePropertyTIFFPhotometricInterpretation'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyTIFFDocumentName: CFStringRef; external name '_kCGImagePropertyTIFFDocumentName'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyTIFFImageDescription: CFStringRef; external name '_kCGImagePropertyTIFFImageDescription'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyTIFFMake: CFStringRef; external name '_kCGImagePropertyTIFFMake'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyTIFFModel: CFStringRef; external name '_kCGImagePropertyTIFFModel'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyTIFFOrientation: CFStringRef; external name '_kCGImagePropertyTIFFOrientation'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyTIFFXResolution: CFStringRef; external name '_kCGImagePropertyTIFFXResolution'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyTIFFYResolution: CFStringRef; external name '_kCGImagePropertyTIFFYResolution'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyTIFFResolutionUnit: CFStringRef; external name '_kCGImagePropertyTIFFResolutionUnit'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyTIFFSoftware: CFStringRef; external name '_kCGImagePropertyTIFFSoftware'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyTIFFTransferFunction: CFStringRef; external name '_kCGImagePropertyTIFFTransferFunction'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyTIFFDateTime: CFStringRef; external name '_kCGImagePropertyTIFFDateTime'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyTIFFArtist: CFStringRef; external name '_kCGImagePropertyTIFFArtist'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyTIFFHostComputer: CFStringRef; external name '_kCGImagePropertyTIFFHostComputer'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyTIFFCopyright: CFStringRef; external name '_kCGImagePropertyTIFFCopyright'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyTIFFWhitePoint: CFStringRef; external name '_kCGImagePropertyTIFFWhitePoint'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyTIFFPrimaryChromaticities: CFStringRef; external name '_kCGImagePropertyTIFFPrimaryChromaticities'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Possible keys for kCGImagePropertyJFIFDictionary }

var kCGImagePropertyJFIFVersion: CFStringRef; external name '_kCGImagePropertyJFIFVersion'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyJFIFXDensity: CFStringRef; external name '_kCGImagePropertyJFIFXDensity'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyJFIFYDensity: CFStringRef; external name '_kCGImagePropertyJFIFYDensity'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyJFIFDensityUnit: CFStringRef; external name '_kCGImagePropertyJFIFDensityUnit'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyJFIFIsProgressive: CFStringRef; external name '_kCGImagePropertyJFIFIsProgressive'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Possible keys for kCGImagePropertyExifDictionary }

var kCGImagePropertyExifExposureTime: CFStringRef; external name '_kCGImagePropertyExifExposureTime'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyExifFNumber: CFStringRef; external name '_kCGImagePropertyExifFNumber'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyExifExposureProgram: CFStringRef; external name '_kCGImagePropertyExifExposureProgram'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyExifSpectralSensitivity: CFStringRef; external name '_kCGImagePropertyExifSpectralSensitivity'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyExifISOSpeedRatings: CFStringRef; external name '_kCGImagePropertyExifISOSpeedRatings'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyExifOECF: CFStringRef; external name '_kCGImagePropertyExifOECF'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyExifVersion: CFStringRef; external name '_kCGImagePropertyExifVersion'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyExifDateTimeOriginal: CFStringRef; external name '_kCGImagePropertyExifDateTimeOriginal'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyExifDateTimeDigitized: CFStringRef; external name '_kCGImagePropertyExifDateTimeDigitized'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyExifComponentsConfiguration: CFStringRef; external name '_kCGImagePropertyExifComponentsConfiguration'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyExifCompressedBitsPerPixel: CFStringRef; external name '_kCGImagePropertyExifCompressedBitsPerPixel'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyExifShutterSpeedValue: CFStringRef; external name '_kCGImagePropertyExifShutterSpeedValue'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyExifApertureValue: CFStringRef; external name '_kCGImagePropertyExifApertureValue'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyExifBrightnessValue: CFStringRef; external name '_kCGImagePropertyExifBrightnessValue'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyExifExposureBiasValue: CFStringRef; external name '_kCGImagePropertyExifExposureBiasValue'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyExifMaxApertureValue: CFStringRef; external name '_kCGImagePropertyExifMaxApertureValue'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyExifSubjectDistance: CFStringRef; external name '_kCGImagePropertyExifSubjectDistance'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyExifMeteringMode: CFStringRef; external name '_kCGImagePropertyExifMeteringMode'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyExifLightSource: CFStringRef; external name '_kCGImagePropertyExifLightSource'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyExifFlash: CFStringRef; external name '_kCGImagePropertyExifFlash'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyExifFocalLength: CFStringRef; external name '_kCGImagePropertyExifFocalLength'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyExifSubjectArea: CFStringRef; external name '_kCGImagePropertyExifSubjectArea'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyExifMakerNote: CFStringRef; external name '_kCGImagePropertyExifMakerNote'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyExifUserComment: CFStringRef; external name '_kCGImagePropertyExifUserComment'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyExifSubsecTime: CFStringRef; external name '_kCGImagePropertyExifSubsecTime'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyExifSubsecTimeOrginal: CFStringRef; external name '_kCGImagePropertyExifSubsecTimeOrginal'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyExifSubsecTimeDigitized: CFStringRef; external name '_kCGImagePropertyExifSubsecTimeDigitized'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyExifFlashPixVersion: CFStringRef; external name '_kCGImagePropertyExifFlashPixVersion'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyExifColorSpace: CFStringRef; external name '_kCGImagePropertyExifColorSpace'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyExifPixelXDimension: CFStringRef; external name '_kCGImagePropertyExifPixelXDimension'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyExifPixelYDimension: CFStringRef; external name '_kCGImagePropertyExifPixelYDimension'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyExifRelatedSoundFile: CFStringRef; external name '_kCGImagePropertyExifRelatedSoundFile'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyExifFlashEnergy: CFStringRef; external name '_kCGImagePropertyExifFlashEnergy'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyExifSpatialFrequencyResponse: CFStringRef; external name '_kCGImagePropertyExifSpatialFrequencyResponse'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyExifFocalPlaneXResolution: CFStringRef; external name '_kCGImagePropertyExifFocalPlaneXResolution'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyExifFocalPlaneYResolution: CFStringRef; external name '_kCGImagePropertyExifFocalPlaneYResolution'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyExifFocalPlaneResolutionUnit: CFStringRef; external name '_kCGImagePropertyExifFocalPlaneResolutionUnit'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyExifSubjectLocation: CFStringRef; external name '_kCGImagePropertyExifSubjectLocation'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyExifExposureIndex: CFStringRef; external name '_kCGImagePropertyExifExposureIndex'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyExifSensingMethod: CFStringRef; external name '_kCGImagePropertyExifSensingMethod'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyExifFileSource: CFStringRef; external name '_kCGImagePropertyExifFileSource'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyExifSceneType: CFStringRef; external name '_kCGImagePropertyExifSceneType'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyExifCFAPattern: CFStringRef; external name '_kCGImagePropertyExifCFAPattern'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyExifCustomRendered: CFStringRef; external name '_kCGImagePropertyExifCustomRendered'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyExifExposureMode: CFStringRef; external name '_kCGImagePropertyExifExposureMode'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyExifWhiteBalance: CFStringRef; external name '_kCGImagePropertyExifWhiteBalance'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyExifDigitalZoomRatio: CFStringRef; external name '_kCGImagePropertyExifDigitalZoomRatio'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyExifFocalLenIn35mmFilm: CFStringRef; external name '_kCGImagePropertyExifFocalLenIn35mmFilm'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyExifSceneCaptureType: CFStringRef; external name '_kCGImagePropertyExifSceneCaptureType'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyExifGainControl: CFStringRef; external name '_kCGImagePropertyExifGainControl'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyExifContrast: CFStringRef; external name '_kCGImagePropertyExifContrast'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyExifSaturation: CFStringRef; external name '_kCGImagePropertyExifSaturation'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyExifSharpness: CFStringRef; external name '_kCGImagePropertyExifSharpness'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyExifDeviceSettingDescription: CFStringRef; external name '_kCGImagePropertyExifDeviceSettingDescription'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyExifSubjectDistRange: CFStringRef; external name '_kCGImagePropertyExifSubjectDistRange'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyExifImageUniqueID: CFStringRef; external name '_kCGImagePropertyExifImageUniqueID'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyExifGamma: CFStringRef; external name '_kCGImagePropertyExifGamma'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Possible keys for kCGImagePropertyGIFDictionary }

var kCGImagePropertyGIFLoopCount: CFStringRef; external name '_kCGImagePropertyGIFLoopCount'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyGIFDelayTime: CFStringRef; external name '_kCGImagePropertyGIFDelayTime'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyGIFImageColorMap: CFStringRef; external name '_kCGImagePropertyGIFImageColorMap'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyGIFHasGlobalColorMap: CFStringRef; external name '_kCGImagePropertyGIFHasGlobalColorMap'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Possible keys for kCGImagePropertyPNGDictionary }

var kCGImagePropertyPNGGamma: CFStringRef; external name '_kCGImagePropertyPNGGamma'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyPNGInterlaceType: CFStringRef; external name '_kCGImagePropertyPNGInterlaceType'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyPNGXPixelsPerMeter: CFStringRef; external name '_kCGImagePropertyPNGXPixelsPerMeter'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyPNGYPixelsPerMeter: CFStringRef; external name '_kCGImagePropertyPNGYPixelsPerMeter'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyPNGsRGBIntent: CFStringRef; external name '_kCGImagePropertyPNGsRGBIntent'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyPNGChromaticities: CFStringRef; external name '_kCGImagePropertyPNGChromaticities'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Possible keys for kCGImagePropertyGPSDictionary }

var kCGImagePropertyGPSVersion: CFStringRef; external name '_kCGImagePropertyGPSVersion'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyGPSLatitudeRef: CFStringRef; external name '_kCGImagePropertyGPSLatitudeRef'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyGPSLatitude: CFStringRef; external name '_kCGImagePropertyGPSLatitude'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyGPSLongitudeRef: CFStringRef; external name '_kCGImagePropertyGPSLongitudeRef'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyGPSLongitude: CFStringRef; external name '_kCGImagePropertyGPSLongitude'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyGPSAltitudeRef: CFStringRef; external name '_kCGImagePropertyGPSAltitudeRef'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyGPSAltitude: CFStringRef; external name '_kCGImagePropertyGPSAltitude'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyGPSTimeStamp: CFStringRef; external name '_kCGImagePropertyGPSTimeStamp'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyGPSSatellites: CFStringRef; external name '_kCGImagePropertyGPSSatellites'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyGPSStatus: CFStringRef; external name '_kCGImagePropertyGPSStatus'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyGPSMeasureMode: CFStringRef; external name '_kCGImagePropertyGPSMeasureMode'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyGPSDOP: CFStringRef; external name '_kCGImagePropertyGPSDOP'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyGPSSpeedRef: CFStringRef; external name '_kCGImagePropertyGPSSpeedRef'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyGPSSpeed: CFStringRef; external name '_kCGImagePropertyGPSSpeed'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyGPSTrackRef: CFStringRef; external name '_kCGImagePropertyGPSTrackRef'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyGPSTrack: CFStringRef; external name '_kCGImagePropertyGPSTrack'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyGPSImgDirectionRef: CFStringRef; external name '_kCGImagePropertyGPSImgDirectionRef'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyGPSImgDirection: CFStringRef; external name '_kCGImagePropertyGPSImgDirection'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyGPSMapDatum: CFStringRef; external name '_kCGImagePropertyGPSMapDatum'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyGPSDestLatitudeRef: CFStringRef; external name '_kCGImagePropertyGPSDestLatitudeRef'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyGPSDestLatitude: CFStringRef; external name '_kCGImagePropertyGPSDestLatitude'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyGPSDestLongitudeRef: CFStringRef; external name '_kCGImagePropertyGPSDestLongitudeRef'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyGPSDestLongitude: CFStringRef; external name '_kCGImagePropertyGPSDestLongitude'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyGPSDestBearingRef: CFStringRef; external name '_kCGImagePropertyGPSDestBearingRef'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyGPSDestBearing: CFStringRef; external name '_kCGImagePropertyGPSDestBearing'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyGPSDestDistanceRef: CFStringRef; external name '_kCGImagePropertyGPSDestDistanceRef'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyGPSDestDistance: CFStringRef; external name '_kCGImagePropertyGPSDestDistance'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyGPSProcessingMethod: CFStringRef; external name '_kCGImagePropertyGPSProcessingMethod'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyGPSAreaInformation: CFStringRef; external name '_kCGImagePropertyGPSAreaInformation'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyGPSDateStamp: CFStringRef; external name '_kCGImagePropertyGPSDateStamp'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyGPSDifferental: CFStringRef; external name '_kCGImagePropertyGPSDifferental'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Possible keys for kCGImagePropertyIPTCDictionary }

var kCGImagePropertyIPTCObjectTypeReference: CFStringRef; external name '_kCGImagePropertyIPTCObjectTypeReference'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyIPTCObjectAttributeReference: CFStringRef; external name '_kCGImagePropertyIPTCObjectAttributeReference'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyIPTCObjectName: CFStringRef; external name '_kCGImagePropertyIPTCObjectName'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyIPTCEditStatus: CFStringRef; external name '_kCGImagePropertyIPTCEditStatus'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyIPTCEditorialUpdate: CFStringRef; external name '_kCGImagePropertyIPTCEditorialUpdate'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyIPTCUrgency: CFStringRef; external name '_kCGImagePropertyIPTCUrgency'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyIPTCSubjectReference: CFStringRef; external name '_kCGImagePropertyIPTCSubjectReference'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyIPTCCategory: CFStringRef; external name '_kCGImagePropertyIPTCCategory'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyIPTCSupplementalCategory: CFStringRef; external name '_kCGImagePropertyIPTCSupplementalCategory'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyIPTCFixtureIdentifier: CFStringRef; external name '_kCGImagePropertyIPTCFixtureIdentifier'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyIPTCKeywords: CFStringRef; external name '_kCGImagePropertyIPTCKeywords'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyIPTCContentLocationCode: CFStringRef; external name '_kCGImagePropertyIPTCContentLocationCode'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyIPTCContentLocationName: CFStringRef; external name '_kCGImagePropertyIPTCContentLocationName'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyIPTCReleaseDate: CFStringRef; external name '_kCGImagePropertyIPTCReleaseDate'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyIPTCReleaseTime: CFStringRef; external name '_kCGImagePropertyIPTCReleaseTime'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyIPTCExpirationDate: CFStringRef; external name '_kCGImagePropertyIPTCExpirationDate'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyIPTCExpirationTime: CFStringRef; external name '_kCGImagePropertyIPTCExpirationTime'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyIPTCSpecialInstructions: CFStringRef; external name '_kCGImagePropertyIPTCSpecialInstructions'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyIPTCActionAdvised: CFStringRef; external name '_kCGImagePropertyIPTCActionAdvised'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyIPTCReferenceService: CFStringRef; external name '_kCGImagePropertyIPTCReferenceService'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyIPTCReferenceDate: CFStringRef; external name '_kCGImagePropertyIPTCReferenceDate'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyIPTCReferenceNumber: CFStringRef; external name '_kCGImagePropertyIPTCReferenceNumber'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyIPTCDateCreated: CFStringRef; external name '_kCGImagePropertyIPTCDateCreated'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyIPTCTimeCreated: CFStringRef; external name '_kCGImagePropertyIPTCTimeCreated'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyIPTCDigitalCreationDate: CFStringRef; external name '_kCGImagePropertyIPTCDigitalCreationDate'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyIPTCDigitalCreationTime: CFStringRef; external name '_kCGImagePropertyIPTCDigitalCreationTime'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyIPTCOriginatingProgram: CFStringRef; external name '_kCGImagePropertyIPTCOriginatingProgram'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyIPTCProgramVersion: CFStringRef; external name '_kCGImagePropertyIPTCProgramVersion'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyIPTCObjectCycle: CFStringRef; external name '_kCGImagePropertyIPTCObjectCycle'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyIPTCByline: CFStringRef; external name '_kCGImagePropertyIPTCByline'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyIPTCBylineTitle: CFStringRef; external name '_kCGImagePropertyIPTCBylineTitle'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyIPTCCity: CFStringRef; external name '_kCGImagePropertyIPTCCity'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyIPTCSubLocation: CFStringRef; external name '_kCGImagePropertyIPTCSubLocation'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyIPTCProvinceState: CFStringRef; external name '_kCGImagePropertyIPTCProvinceState'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyIPTCCountryPrimaryLocationCode: CFStringRef; external name '_kCGImagePropertyIPTCCountryPrimaryLocationCode'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyIPTCCountryPrimaryLocationName: CFStringRef; external name '_kCGImagePropertyIPTCCountryPrimaryLocationName'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyIPTCOriginalTransmissionReference: CFStringRef; external name '_kCGImagePropertyIPTCOriginalTransmissionReference'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyIPTCHeadline: CFStringRef; external name '_kCGImagePropertyIPTCHeadline'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyIPTCCredit: CFStringRef; external name '_kCGImagePropertyIPTCCredit'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyIPTCSource: CFStringRef; external name '_kCGImagePropertyIPTCSource'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyIPTCCopyrightNotice: CFStringRef; external name '_kCGImagePropertyIPTCCopyrightNotice'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyIPTCContact: CFStringRef; external name '_kCGImagePropertyIPTCContact'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyIPTCCaptionAbstract: CFStringRef; external name '_kCGImagePropertyIPTCCaptionAbstract'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyIPTCWriterEditor: CFStringRef; external name '_kCGImagePropertyIPTCWriterEditor'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyIPTCImageType: CFStringRef; external name '_kCGImagePropertyIPTCImageType'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyIPTCImageOrientation: CFStringRef; external name '_kCGImagePropertyIPTCImageOrientation'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyIPTCLanguageIdentifier: CFStringRef; external name '_kCGImagePropertyIPTCLanguageIdentifier'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCGImagePropertyIPTCStarRating: CFStringRef; external name '_kCGImagePropertyIPTCStarRating'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


end.
