{ CoreGraphics - CGColorSpace.h
   Copyright (c) 1999-2009 Apple Inc.
   All rights reserved. }
{       Pascal Translation Updated:  Peter N Lewis, <peter@stairways.com.au>, August 2005 }
{       Pascal Translation Updated:  Gale R Paeper, <gpaeper@empirenet.com>, 2006 }
{       Pascal Translation Updated:  Jonas Maebe, <jonas@freepascal.org>, October 2009 }

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

unit CGColorSpace;
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
uses MacTypes,CFBase,CFData,CFString,CGBase,CGDataProvider;
{$endc} {not MACOSALLINCLUDE}

{$ALIGN POWER}


type
	CGColorSpaceRef = ^OpaqueCGColorSpaceRef; { an opaque type }
	OpaqueCGColorSpaceRef = record end;

type
	TristimulusValue = array[0..2] of CGFloat;
	RedGreenBlueValue = array[0..2] of CGFloat;
	Single4 = array[0..3] of CGFloat;
	Single9 = array[0..8] of CGFloat;

{ Color rendering intents. }

type
	CGColorRenderingIntent = SInt32;
const
	kCGRenderingIntentDefault = 0;
	kCGRenderingIntentAbsoluteColorimetric = 1;
	kCGRenderingIntentRelativeColorimetric = 2;
	kCGRenderingIntentPerceptual = 3;
	kCGRenderingIntentSaturation = 4;

{ The model of a color space. }

type
	CGColorSpaceModel = SInt32;
const
	kCGColorSpaceModelUnknown = -1;
	kCGColorSpaceModelMonochrome = -1 + 1;
	kCGColorSpaceModelRGB = -1 + 2;
	kCGColorSpaceModelCMYK = -1 + 3;
	kCGColorSpaceModelLab = -1 + 4;
	kCGColorSpaceModelDeviceN = -1 + 5;
	kCGColorSpaceModelIndexed = -1 + 6;
	kCGColorSpaceModelPattern = -1 + 7;

{$ifc TARGET_OS_MAC}
{ The name of the "Generic" gray color space. }

var kCGColorSpaceGenericGray: CFStringRef; external name '_kCGColorSpaceGenericGray'; (* attribute const *)
(* CG_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)

{ The name of the "Generic" RGB color space. }

var kCGColorSpaceGenericRGB: CFStringRef; external name '_kCGColorSpaceGenericRGB'; (* attribute const *)
(* CG_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)

{ The name of the "Generic" CMYK color space. }

var kCGColorSpaceGenericCMYK: CFStringRef; external name '_kCGColorSpaceGenericCMYK'; (* attribute const *)
(* CG_AVAILABLE_STARTING(__MAC_10_4, __IPHONE_NA) *)

{ The name of the "Generic" linear RGB color space. This is the same as
   `kCGColorSpaceGenericRGB' but with a 1.0 gamma. }

var kCGColorSpaceGenericRGBLinear: CFStringRef; external name '_kCGColorSpaceGenericRGBLinear'; (* attribute const *)
(* CG_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA) *)

{ The name of the Adobe RGB (1998) color space. For more information, see
  "Adobe RGB (1998) Color Image Encoding", Version 2005-05, Adobe Systems
  Inc. (http://www.adobe.com). }

var kCGColorSpaceAdobeRGB1998: CFStringRef; external name '_kCGColorSpaceAdobeRGB1998'; (* attribute const *)
(* CG_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA) *)

{ The name of the sRGB color space. The capitalization in the name, while
   strictly inaccurate, avoids interpretational ambiguity. For more
   information, see IEC 61966-2-1 (1999-10): "Multimedia systems and
   equipment - Colour measurement and management - Part 2-1: Colour
   management - Default RGB colour space - sRGB". }

var kCGColorSpaceSRGB: CFStringRef; external name '_kCGColorSpaceSRGB'; (* attribute const *)
(* CG_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA) *)

{ The "Generic" gray color space with gamma = 2.2. }

var kCGColorSpaceGenericGrayGamma2_2: CFStringRef; external name '_kCGColorSpaceGenericGrayGamma2_2'; (* attribute const *)
(* CG_AVAILABLE_STARTING(__MAC_10_6, __IPHONE_NA) *)
{$endc}

{* Device-dependent color spaces.  *}

{ Create a DeviceGray color space. }

function CGColorSpaceCreateDeviceGray: CGColorSpaceRef; external name '_CGColorSpaceCreateDeviceGray';
(* CG_AVAILABLE_STARTING(__MAC_10_0, __IPHONE_2_0) *)

{ Create a DeviceRGB color space. }

function CGColorSpaceCreateDeviceRGB: CGColorSpaceRef; external name '_CGColorSpaceCreateDeviceRGB';
(* CG_AVAILABLE_STARTING(__MAC_10_0, __IPHONE_2_0) *)

{ Create a DeviceCMYK color space. }

function CGColorSpaceCreateDeviceCMYK: CGColorSpaceRef; external name '_CGColorSpaceCreateDeviceCMYK';
(* CG_AVAILABLE_STARTING(__MAC_10_0, __IPHONE_2_0) *)

{* Device-independent color spaces. *}

{ Create a calibrated gray color space. `whitePoint' is an array of 3
   numbers specifying the tristimulus value, in the CIE 1931 XYZ-space, of
   the diffuse white point. `blackPoint' is an array of 3 numbers specifying
   the tristimulus value, in CIE 1931 XYZ-space, of the diffuse black point.
   `gamma' defines the gamma for the gray component. }

function CGColorSpaceCreateCalibratedGray( const (*var*) whitePoint: TristimulusValue; const (*var*) blackPoint: TristimulusValue; gamma: CGFloat ): CGColorSpaceRef; external name '_CGColorSpaceCreateCalibratedGray';
(* CG_AVAILABLE_STARTING(__MAC_10_0, __IPHONE_2_0) *)

{ Create a calibrated RGB color space. `whitePoint' is an array of 3
   numbers specifying the tristimulus value, in the CIE 1931 XYZ-space, of
   the diffuse white point. `blackPoint' is an array of 3 numbers specifying
   the tristimulus value, in CIE 1931 XYZ-space, of the diffuse black point.
   `gamma' is an array of 3 numbers specifying the gamma for the red, green,
   and blue components of the color space. `matrix' is an array of 9 numbers
   specifying the linear interpretation of the gamma-modified RGB values of
   the color space with respect to the final XYZ representation. }

function CGColorSpaceCreateCalibratedRGB( const (*var*) whitePoint: TristimulusValue; const (*var*) blackPoint: TristimulusValue; const (*var*) gamma: RedGreenBlueValue; const (*var*) matrix: Single9 ): CGColorSpaceRef; external name '_CGColorSpaceCreateCalibratedRGB';
(* CG_AVAILABLE_STARTING(__MAC_10_0, __IPHONE_2_0) *)
    
{ Create an L*a*b* color space. `whitePoint' is an array of 3 numbers
   specifying the tristimulus value, in the CIE 1931 XYZ-space, of the
   diffuse white point. `blackPoint' is an array of 3 numbers specifying the
   tristimulus value, in CIE 1931 XYZ-space, of the diffuse black point.
   `range' is an array of four numbers specifying the range of valid values
   for the a* and b* components of the color space. }

function CGColorSpaceCreateLab(const (*var*) whitePoint: TristimulusValue; const (*var*) blackPoint: TristimulusValue; const (*var*) range: Single4): CGColorSpaceRef; external name '_CGColorSpaceCreateLab';
(* CG_AVAILABLE_STARTING(__MAC_10_0, __IPHONE_2_0) *)

{ Create an ICC-based color space using the ICC profile specified by
   `data'. }

function CGColorSpaceCreateWithICCProfile( data: CFDataRef ): CGColorSpaceRef; external name '_CGColorSpaceCreateWithICCProfile';
(* CG_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_2_0) *)

{ Create an ICC-based color space. `nComponents' specifies the number of
   color components in the color space defined by the ICC profile data. This
   must match the number of components actually in the ICC profile, and must
   be 1, 3, or 4. `range' is an array of 2*nComponents numbers specifying
   the minimum and maximum valid values of the corresponding color
   components, so that for color component k, range[2*k] <= c[k] <=
   range[2*k+1], where c[k] is the k'th color component. `profile' is a data
   provider specifying the ICC profile. `alternate' specifies an alternate
   color space to be used in case the ICC profile is not supported. It must
   have `nComponents' color components. If `alternate' is NULL, then the
   color space used will be DeviceGray, DeviceRGB, or DeviceCMYK, depending
   on whether `nComponents' is 1, 3, or 4, respectively. }

function CGColorSpaceCreateICCBased( nComponents: size_t; {const} range: {variable-size-array} CGFloatPtr; profile: CGDataProviderRef; alternate: CGColorSpaceRef ): CGColorSpaceRef; external name '_CGColorSpaceCreateICCBased';
(* CG_AVAILABLE_STARTING(__MAC_10_0, __IPHONE_2_0) *)

{* Special colorspaces. *}

{ Create an indexed color space. A sample value in an indexed color space
   is treated as an index into the color table of the color space. `base'
   specifies the base color space in which the values in the color table are
   to be interpreted. `lastIndex' is an integer which specifies the maximum
   valid index value; it must be less than or equal to 255. `colorTable' is
   an array of m * (lastIndex + 1) bytes, where m is the number of color
   components in the base color space. Each byte is an unsigned integer in
   the range 0 to 255 that is scaled to the range of the corresponding color
   component in the base color space. }

function CGColorSpaceCreateIndexed( baseSpace: CGColorSpaceRef; lastIndex: size_t; colorTable: UInt8Ptr ): CGColorSpaceRef; external name '_CGColorSpaceCreateIndexed';
(* CG_AVAILABLE_STARTING(__MAC_10_0, __IPHONE_2_0) *)

{ Create a pattern color space. `baseSpace' is the underlying color space
   of the pattern color space. For colored patterns, `baseSpace' should be
   NULL; for uncolored patterns, `baseSpace' specifies the color space of
   colors which will be painted through the pattern. }

function CGColorSpaceCreatePattern( baseSpace: CGColorSpaceRef ): CGColorSpaceRef; external name '_CGColorSpaceCreatePattern';
(* CG_AVAILABLE_STARTING(__MAC_10_0, __IPHONE_2_0) *)

{$ifc TARGET_OS_MAC}
{ Create a color space using `ref', a platform-specific color space
   reference. For MacOS X, `ref' should be a CMProfileRef. }

function CGColorSpaceCreateWithPlatformColorSpace( ref: {const} UnivPtr ): CGColorSpaceRef; external name '_CGColorSpaceCreateWithPlatformColorSpace';
(* CG_AVAILABLE_STARTING(__MAC_10_0, __IPHONE_NA) *)
{$endc}

{ Create a color space using `name' as the identifier for the color
   space. }

function CGColorSpaceCreateWithName( name: CFStringRef ): CGColorSpaceRef; external name '_CGColorSpaceCreateWithName';
(* CG_AVAILABLE_STARTING(__MAC_10_2, __IPHONE_2_0) *)

{ Equivalent to `CFRetain(space)', except it doesn't crash (as CFRetain
   does) if `space' is NULL. }

function CGColorSpaceRetain( space: CGColorSpaceRef ): CGColorSpaceRef; external name '_CGColorSpaceRetain';
(* CG_AVAILABLE_STARTING(__MAC_10_0, __IPHONE_2_0) *)

{ Equivalent to `CFRelease(space)', except it doesn't crash (as CFRelease
   does) if `space' is NULL. }

procedure CGColorSpaceRelease( space: CGColorSpaceRef ); external name '_CGColorSpaceRelease';
(* CG_AVAILABLE_STARTING(__MAC_10_0, __IPHONE_2_0) *)

{$ifc TARGET_OS_MAC}
{ Return the name used to create the color space `space', or NULL if the
   color space was not created using `CGColorSpaceCreateWithName'. }

function CGColorSpaceCopyName( space: CGColorSpaceRef ): CFStringRef; external name '_CGColorSpaceCopyName';
(* CG_AVAILABLE_STARTING(__MAC_10_6, __IPHONE_NA) *)
{$endc}

{* Colorspace information. *}

{ Return the CFTypeID for CGColorSpaces. }

function CGColorSpaceGetTypeID: CFTypeID; external name '_CGColorSpaceGetTypeID';
(* CG_AVAILABLE_STARTING(__MAC_10_2, __IPHONE_2_0) *)

{ Return the number of color components in the color space `space'. }

function CGColorSpaceGetNumberOfComponents( space: CGColorSpaceRef ): size_t; external name '_CGColorSpaceGetNumberOfComponents';
(* CG_AVAILABLE_STARTING(__MAC_10_0, __IPHONE_2_0) *)

{ Return the color space model of `space'. }

function CGColorSpaceGetModel( space: CGColorSpaceRef ): CGColorSpaceModel; external name '_CGColorSpaceGetModel';
(* CG_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_2_0) *)

{ Return the base color space of `space' if `space' is a pattern or indexed
   color space; otherwise, return NULL. To determine whether a color space
   is an indexed or pattern color space, use `CGColorSpaceGetModel'. }

function CGColorSpaceGetBaseColorSpace( space: CGColorSpaceRef ): CGColorSpaceRef; external name '_CGColorSpaceGetBaseColorSpace';
(* CG_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_2_0) *)

{ Return the number of entries in the color table of `space' if `space' is
   an indexed color space; otherwise, return 0. To determine whether a color
   space is an indexed color space, use `CGColorSpaceGetModel'. }

function CGColorSpaceGetColorTableCount( space: CGColorSpaceRef ): size_t; external name '_CGColorSpaceGetColorTableCount';
(* CG_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_2_0) *)

{ Copy the entries in the color table of `space' to `table' if `space' is
   an indexed color space; otherwise, do nothing. The array pointed to by
   `table' should be at least as large as the number of entries in the color
   table; the returned data is in the same format as that passed to
   `CGColorSpaceCreateIndexed'. To determine whether a color space is an
   indexed color space, use `CGColorSpaceGetModel'. }

procedure CGColorSpaceGetColorTable( space: CGColorSpaceRef; table: UInt8Ptr ); external name '_CGColorSpaceGetColorTable';
(* CG_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_2_0) *)

{$ifc TARGET_OS_MAC}
{ Return a copy of the ICC profile of `space', or NULL if the color space
   doesn't have an ICC profile. }

function CGColorSpaceCopyICCProfile( space: CGColorSpaceRef ): CFDataRef; external name '_CGColorSpaceCopyICCProfile';
(* CG_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA) *)
{$endc}

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

{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}
