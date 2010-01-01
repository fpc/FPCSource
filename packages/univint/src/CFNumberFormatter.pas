{	CFNumberFormatter.h
	Copyright (c) 2003-2009, Apple Inc. All rights reserved.
}
{   Pascal Translation:  Peter N Lewis, <peter@stairways.com.au>, 2004 }
{   Pascal Translation Updated:  Peter N Lewis, <peter@stairways.com.au>, September 2005 }
{   Pascal Translation Updated:  Gorazd Krosl, <gorazd_1957@yahoo.ca>, October 2009 }
{   Pascal Translation Updated:  Jonas Maebe, <jonas@freepascal.org>, October 2009 }
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

unit CFNumberFormatter;
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
uses MacTypes,CFBase,CFNumber,CFLocale;
{$endc} {not MACOSALLINCLUDE}

{$ALIGN POWER}


{#if MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3}


type
	CFNumberFormatterRef = ^SInt32; { an opaque type }

// CFNumberFormatters are not thread-safe.  Do not use one from multiple threads!

function CFNumberFormatterGetTypeID: CFTypeID; external name '_CFNumberFormatterGetTypeID';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

// number format styles
type
	CFNumberFormatterStyle = SIGNEDLONG;
const
	kCFNumberFormatterNoStyle = 0;
	kCFNumberFormatterDecimalStyle = 1;
	kCFNumberFormatterCurrencyStyle = 2;
	kCFNumberFormatterPercentStyle = 3;
	kCFNumberFormatterScientificStyle = 4;
	kCFNumberFormatterSpellOutStyle = 5;


function CFNumberFormatterCreate( allocator: CFAllocatorRef; locale: CFLocaleRef; style: CFNumberFormatterStyle ): CFNumberFormatterRef; external name '_CFNumberFormatterCreate';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)
	// Returns a CFNumberFormatter, localized to the given locale, which
	// will format numbers to the given style.

function CFNumberFormatterGetLocale( formatter: CFNumberFormatterRef ): CFLocaleRef; external name '_CFNumberFormatterGetLocale';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

function CFNumberFormatterGetStyle( formatter: CFNumberFormatterRef ): CFNumberFormatterStyle; external name '_CFNumberFormatterGetStyle';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)
	// Get the properties with which the number formatter was created.

function CFNumberFormatterGetFormat( formatter: CFNumberFormatterRef ): CFStringRef; external name '_CFNumberFormatterGetFormat';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

procedure CFNumberFormatterSetFormat( formatter: CFNumberFormatterRef; formatString: CFStringRef ); external name '_CFNumberFormatterSetFormat';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)
	// Set the format description string of the number formatter.  This
	// overrides the style settings.  The format of the format string
	// is as defined by the ICU library, and is similar to that found
	// in Microsoft Excel and NSNumberFormatter (and Java I believe).
	// The number formatter starts with a default format string defined
	// by the style argument with which it was created.


function CFNumberFormatterCreateStringWithNumber( allocator: CFAllocatorRef; formatter: CFNumberFormatterRef; number: CFNumberRef ): CFStringRef; external name '_CFNumberFormatterCreateStringWithNumber';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

function CFNumberFormatterCreateStringWithValue( allocator: CFAllocatorRef; formatter: CFNumberFormatterRef; numberType: CFNumberType; valuePtr: {const} UnivPtr ): CFStringRef; external name '_CFNumberFormatterCreateStringWithValue';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)
	// Create a string representation of the given number or value
	// using the current state of the number formatter.


type
	CFNumberFormatterOptionFlags = UNSIGNEDLONG;
const
	kCFNumberFormatterParseIntegersOnly = 1;	{ only parse integers }

function CFNumberFormatterCreateNumberFromString( allocator: CFAllocatorRef; formatter: CFNumberFormatterRef; strng: CFStringRef; rangep: CFRangePtr; options: CFOptionFlags ): CFNumberRef; external name '_CFNumberFormatterCreateNumberFromString';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

function CFNumberFormatterGetValueFromString( formatter: CFNumberFormatterRef; strng: CFStringRef; rangep: CFRangePtr; numberType: CFNumberType; valuePtr: UnivPtr ): Boolean; external name '_CFNumberFormatterGetValueFromString';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)
	// Parse a string representation of a number using the current state
	// of the number formatter.  The range parameter specifies the range
	// of the string in which the parsing should occur in input, and on
	// output indicates the extent that was used; this parameter can
	// be NULL, in which case the whole string may be used.  The
	// return value indicates whether some number was computed and
	// (if valuePtr is not NULL) stored at the location specified by
	// valuePtr.  The numberType indicates the type of value pointed
	// to by valuePtr.


procedure CFNumberFormatterSetProperty( formatter: CFNumberFormatterRef; key: CFStringRef; value: CFTypeRef ); external name '_CFNumberFormatterSetProperty';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

function CFNumberFormatterCopyProperty( formatter: CFNumberFormatterRef; key: CFStringRef ): CFTypeRef; external name '_CFNumberFormatterCopyProperty';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)
	// Set and get various properties of the number formatter, the set of
	// which may be expanded in the future.

var kCFNumberFormatterCurrencyCode: CFStringRef; external name '_kCFNumberFormatterCurrencyCode'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)		// CFString
var kCFNumberFormatterDecimalSeparator: CFStringRef; external name '_kCFNumberFormatterDecimalSeparator'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)	// CFString
var kCFNumberFormatterCurrencyDecimalSeparator: CFStringRef; external name '_kCFNumberFormatterCurrencyDecimalSeparator'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *) // CFString
var kCFNumberFormatterAlwaysShowDecimalSeparator: CFStringRef; external name '_kCFNumberFormatterAlwaysShowDecimalSeparator'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *) // CFBoolean
var kCFNumberFormatterGroupingSeparator: CFStringRef; external name '_kCFNumberFormatterGroupingSeparator'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)	// CFString
var kCFNumberFormatterUseGroupingSeparator: CFStringRef; external name '_kCFNumberFormatterUseGroupingSeparator'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)	// CFBoolean
var kCFNumberFormatterPercentSymbol: CFStringRef; external name '_kCFNumberFormatterPercentSymbol'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)		// CFString
var kCFNumberFormatterZeroSymbol: CFStringRef; external name '_kCFNumberFormatterZeroSymbol'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)		// CFString
var kCFNumberFormatterNaNSymbol: CFStringRef; external name '_kCFNumberFormatterNaNSymbol'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)		// CFString
var kCFNumberFormatterInfinitySymbol: CFStringRef; external name '_kCFNumberFormatterInfinitySymbol'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)		// CFString
var kCFNumberFormatterMinusSign: CFStringRef; external name '_kCFNumberFormatterMinusSign'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)		// CFString
var kCFNumberFormatterPlusSign: CFStringRef; external name '_kCFNumberFormatterPlusSign'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)		// CFString
var kCFNumberFormatterCurrencySymbol: CFStringRef; external name '_kCFNumberFormatterCurrencySymbol'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)		// CFString
var kCFNumberFormatterExponentSymbol: CFStringRef; external name '_kCFNumberFormatterExponentSymbol'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)		// CFString
var kCFNumberFormatterMinIntegerDigits: CFStringRef; external name '_kCFNumberFormatterMinIntegerDigits'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)	// CFNumber
var kCFNumberFormatterMaxIntegerDigits: CFStringRef; external name '_kCFNumberFormatterMaxIntegerDigits'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)	// CFNumber
var kCFNumberFormatterMinFractionDigits: CFStringRef; external name '_kCFNumberFormatterMinFractionDigits'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)	// CFNumber
var kCFNumberFormatterMaxFractionDigits: CFStringRef; external name '_kCFNumberFormatterMaxFractionDigits'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)	// CFNumber
var kCFNumberFormatterGroupingSize: CFStringRef; external name '_kCFNumberFormatterGroupingSize'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)		// CFNumber
var kCFNumberFormatterSecondaryGroupingSize: CFStringRef; external name '_kCFNumberFormatterSecondaryGroupingSize'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)	// CFNumber
var kCFNumberFormatterRoundingMode: CFStringRef; external name '_kCFNumberFormatterRoundingMode'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)		// CFNumber
var kCFNumberFormatterRoundingIncrement: CFStringRef; external name '_kCFNumberFormatterRoundingIncrement'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)	// CFNumber
var kCFNumberFormatterFormatWidth: CFStringRef; external name '_kCFNumberFormatterFormatWidth'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)		// CFNumber
var kCFNumberFormatterPaddingPosition: CFStringRef; external name '_kCFNumberFormatterPaddingPosition'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)	// CFNumber
var kCFNumberFormatterPaddingCharacter: CFStringRef; external name '_kCFNumberFormatterPaddingCharacter'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)	// CFString
var kCFNumberFormatterDefaultFormat: CFStringRef; external name '_kCFNumberFormatterDefaultFormat'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)		// CFString
var kCFNumberFormatterMultiplier: CFStringRef; external name '_kCFNumberFormatterMultiplier'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)		// CFNumber
var kCFNumberFormatterPositivePrefix: CFStringRef; external name '_kCFNumberFormatterPositivePrefix'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)		// CFString
var kCFNumberFormatterPositiveSuffix: CFStringRef; external name '_kCFNumberFormatterPositiveSuffix'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)		// CFString
var kCFNumberFormatterNegativePrefix: CFStringRef; external name '_kCFNumberFormatterNegativePrefix'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)		// CFString
var kCFNumberFormatterNegativeSuffix: CFStringRef; external name '_kCFNumberFormatterNegativeSuffix'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)		// CFString
var kCFNumberFormatterPerMillSymbol: CFStringRef; external name '_kCFNumberFormatterPerMillSymbol'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)		// CFString
var kCFNumberFormatterInternationalCurrencySymbol: CFStringRef; external name '_kCFNumberFormatterInternationalCurrencySymbol'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *) // CFString
var kCFNumberFormatterCurrencyGroupingSeparator: CFStringRef; external name '_kCFNumberFormatterCurrencyGroupingSeparator'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *) // CFString
var kCFNumberFormatterIsLenient: CFStringRef; external name '_kCFNumberFormatterIsLenient'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)		// CFBoolean
var kCFNumberFormatterUseSignificantDigits: CFStringRef; external name '_kCFNumberFormatterUseSignificantDigits'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)	// CFBoolean
var kCFNumberFormatterMinSignificantDigits: CFStringRef; external name '_kCFNumberFormatterMinSignificantDigits'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)	// CFNumber
var kCFNumberFormatterMaxSignificantDigits: CFStringRef; external name '_kCFNumberFormatterMaxSignificantDigits'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)	// CFNumber

type
	CFNumberFormatterRoundingMode = SIGNEDLONG;
const
	kCFNumberFormatterRoundCeiling = 0;
	kCFNumberFormatterRoundFloor = 1;
	kCFNumberFormatterRoundDown = 2;
	kCFNumberFormatterRoundUp = 3;
	kCFNumberFormatterRoundHalfEven = 4;
	kCFNumberFormatterRoundHalfDown = 5;
	kCFNumberFormatterRoundHalfUp = 6;

type
	CFNumberFormatterPadPosition = SIGNEDLONG;
const
	kCFNumberFormatterPadBeforePrefix = 0;
	kCFNumberFormatterPadAfterPrefix = 1;
	kCFNumberFormatterPadBeforeSuffix = 2;
	kCFNumberFormatterPadAfterSuffix = 3;


function CFNumberFormatterGetDecimalInfoForCurrencyCode( currencyCode: CFStringRef; defaultFractionDigits: SInt32Ptr; roundingIncrement: Float64Ptr ): Boolean; external name '_CFNumberFormatterGetDecimalInfoForCurrencyCode';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)
	// Returns the number of fraction digits that should be displayed, and
	// the rounding increment (or 0.0 if no rounding is done by the currency)
	// for the given currency.  Returns false if the currency code is unknown
	// or the information is not available.
	// Not localized because these are properties of the currency.


{#endif}

{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}
