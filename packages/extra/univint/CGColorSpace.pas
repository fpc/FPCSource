{ CoreGraphics - CGColorSpace.h
 * Copyright (c) 1999-2004 Apple Computer, Inc.
 * All rights reserved.
 }
{       Pascal Translation Updated:  Peter N Lewis, <peter@stairways.com.au>, August 2005 }
{       Pascal Translation Updated:  Gale R Paeper, <gpaeper@empirenet.com>, 2006 }

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

unit CGColorSpace;
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
uses MacTypes,CFBase,CFString,CMTypes,CGBase,CGDataProvider;
{$ALIGN POWER}


type
	CGColorSpaceRef = ^SInt32; { an opaque 32-bit type }

type
	TristimulusValue = array[0..2] of Float32;
	RedGreenBlueValue = array[0..2] of Float32;
	Single4 = array[0..3] of Float32;
	Single9 = array[0..8] of Float32;

type
	CGColorRenderingIntent = SInt32;
const
	kCGRenderingIntentDefault = 0;
	kCGRenderingIntentAbsoluteColorimetric = 1;
	kCGRenderingIntentRelativeColorimetric = 2;
	kCGRenderingIntentPerceptual = 3;
	kCGRenderingIntentSaturation = 4;

var kCGColorSpaceGenericGray: CFStringRef; external name '_kCGColorSpaceGenericGray'; (* attribute const *) (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

var kCGColorSpaceGenericRGB: CFStringRef; external name '_kCGColorSpaceGenericRGB'; (* attribute const *) (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

var kCGColorSpaceGenericCMYK: CFStringRef; external name '_kCGColorSpaceGenericCMYK'; (* attribute const *) (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Return the CFTypeID for CGColorSpaces. }

function CGColorSpaceGetTypeID: CFTypeID; external name '_CGColorSpaceGetTypeID'; (* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)

{* Device-dependent color spaces.  *}

{ Create a DeviceGray colorspace. }

function CGColorSpaceCreateDeviceGray: CGColorSpaceRef; external name '_CGColorSpaceCreateDeviceGray';

{ Create a DeviceRGB colorspace. }

function CGColorSpaceCreateDeviceRGB: CGColorSpaceRef; external name '_CGColorSpaceCreateDeviceRGB';

{ Create a DeviceCMYK colorspace. }

function CGColorSpaceCreateDeviceCMYK: CGColorSpaceRef; external name '_CGColorSpaceCreateDeviceCMYK';

{* Device-independent color spaces. *}

{ Create a calibrated gray colorspace.  `whitePoint' is an array of 3
 * numbers specifying the tristimulus value, in the CIE 1931 XYZ-space, of
 * the diffuse white point.  `blackPoint' is an array of 3 numbers
 * specifying the tristimulus value, in CIE 1931 XYZ-space, of the diffuse
 * black point. `gamma' defines the gamma for the gray component. }

function CGColorSpaceCreateCalibratedGray( const (*var*) whitePoint: TristimulusValue; const (*var*) blackPoint: TristimulusValue; gamma: Float32 ): CGColorSpaceRef; external name '_CGColorSpaceCreateCalibratedGray';

{ Create a calibrated RGB colorspace.  `whitePoint' is an array of 3
 * numbers specifying the tristimulus value, in the CIE 1931 XYZ-space, of
 * the diffuse white point.  `blackPoint' is an array of 3 numbers
 * specifying the tristimulus value, in CIE 1931 XYZ-space, of the diffuse
 * black point. `gamma' is an array of 3 numbers specifying the gamma for
 * the red, green, and blue components of the color space. `matrix' is an
 * array of 9 numbers specifying the linear interpretation of the
 * gamma-modified RGB values of the colorspace with respect to the final
 * XYZ representation. }

function CGColorSpaceCreateCalibratedRGB( const (*var*) whitePoint: TristimulusValue; const (*var*) blackPoint: TristimulusValue; const (*var*) gamma: RedGreenBlueValue; const (*var*) matrix: Single9 ): CGColorSpaceRef; external name '_CGColorSpaceCreateCalibratedRGB';

{ Create an L*a*b* colorspace.  `whitePoint' is an array of 3 numbers
 * specifying the tristimulus value, in the CIE 1931 XYZ-space, of the
 * diffuse white point.  `blackPoint' is an array of 3 numbers specifying
 * the tristimulus value, in CIE 1931 XYZ-space, of the diffuse black
 * point. `range' is an array of four numbers specifying the range of valid
 * values for the a* and b* components of the color space. }

function CGColorSpaceCreateLab(const (*var*) whitePoint: TristimulusValue; const (*var*) blackPoint: TristimulusValue; const (*var*) range: Single4): CGColorSpaceRef; external name '_CGColorSpaceCreateLab';

{ Create an ICC-based colorspace.  `nComponents' specifies the number of
 * color components in the color space defined by the ICC profile data.
 * This must match the number of components actually in the ICC profile,
 * and must be 1, 3, or 4.  `range' is an array of 2*nComponents numbers
 * specifying the minimum and maximum valid values of the corresponding
 * color components, so that for color component k, range[2*k] <= c[k] <=
 * range[2*k+1], where c[k] is the k'th color component.  `profile' is a
 * data provider specifying the ICC profile.  `alternate' specifies an
 * alternate colorspace to be used in case the ICC profile is not
 * supported.  It must have `nComponents' color components. If `alternate'
 * is NULL, then the color space used will be DeviceGray, DeviceRGB, or
 * DeviceCMYK, depending on whether `nComponents' is 1, 3, or 4,
 * respectively. }

function CGColorSpaceCreateICCBased( nComponents: size_t; {const} range: {variable-size-array} Float32Ptr; profile: CGDataProviderRef; alternate: CGColorSpaceRef ): CGColorSpaceRef; external name '_CGColorSpaceCreateICCBased';

{* Special colorspaces. *}

{ Create an indexed colorspace.  A sample value in an indexed color space
 * is treated as an index into the color table of the color space.  `base'
 * specifies the base color space in which the values in the color table
 * are to be interpreted. `lastIndex' is an integer which specifies the
 * maximum valid index value; it must be less than or equal to 255.
 * `colorTable' is an array of m * (lastIndex + 1) bytes, where m is
 * the number of color components in the base color space.  Each byte
 * is an unsigned integer in the range 0 to 255 that is scaled to the
 * range of the corresponding color component in the base color space. }

function CGColorSpaceCreateIndexed( baseSpace: CGColorSpaceRef; lastIndex: size_t; colorTable: UInt8Ptr ): CGColorSpaceRef; external name '_CGColorSpaceCreateIndexed';

{ Create a pattern colorspace. `baseSpace' is the underlying colorspace of
 * the pattern colorspace.  For colored patterns, `baseSpace' should be
 * NULL; for uncolored patterns, `baseSpace' specifies the colorspace of
 * colors which will be painted through the pattern. }

function CGColorSpaceCreatePattern( baseSpace: CGColorSpaceRef ): CGColorSpaceRef; external name '_CGColorSpaceCreatePattern';

{ Create a CGColorSpace using `platformColorSpaceReference', a
 * platform-specific color space reference. For MacOS X,
 * `platformColorSpaceReference' should be a CMProfileRef. }

function CGColorSpaceCreateWithPlatformColorSpace( platformColorSpaceReference: UnivPtr ): CGColorSpaceRef; external name '_CGColorSpaceCreateWithPlatformColorSpace';

{ Create a colorspace using `name' as the identifier for the colorspace. }

function CGColorSpaceCreateWithName( name: CFStringRef ): CGColorSpaceRef; external name '_CGColorSpaceCreateWithName'; (* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)

{* Colorspace information. *}

{ Return the number of color components in the colorspace `cs'. }

function CGColorSpaceGetNumberOfComponents( cs: CGColorSpaceRef ): size_t; external name '_CGColorSpaceGetNumberOfComponents';

{* Retaining & releasing colorspaces. *}

{ Equivalent to `CFRetain(cs)'. }

function CGColorSpaceRetain( cs: CGColorSpaceRef ): CGColorSpaceRef; external name '_CGColorSpaceRetain';

{ Equivalent to `CFRelease(cs)'. }

procedure CGColorSpaceRelease( cs: CGColorSpaceRef ); external name '_CGColorSpaceRelease';


{* Deprecated APIs. *}

{ Use "kCGColorSpaceGenericGray" instead. }
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kCGColorSpaceUserGray CFSTRP('kCGColorSpaceUserGray')}
{$endc}

{ Use "kCGColorSpaceGenericRGB" instead. }
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kCGColorSpaceUserRGB CFSTRP('kCGColorSpaceUserRGB')}
{$endc}

{ Use "kCGColorSpaceGenericCMYK" instead. }
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kCGColorSpaceUserCMYK CFSTRP('kCGColorSpaceUserCMYK')}
{$endc}


end.
