{	CFDateFormatter.h
	Copyright (c) 2003-2009, Apple Inc. All rights reserved.
}
{	  Pascal Translation:  Peter N Lewis, <peter@stairways.com.au>, 2004 }
{	  Pascal Translation Updated:  Peter N Lewis, <peter@stairways.com.au>, September 2005 }
{	  Pascal Translation Updated:  Gorazd Krosl, <gorazd_1957@yahoo.ca>, October 2009 }
{	  Pascal Translation Updated:  Jonas Maebe, <jonas@freepascal.org>, October 2009 }
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

unit CFDateFormatter;
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
uses MacTypes,CFBase,CFDate,CFLocale;
{$endc} {not MACOSALLINCLUDE}

{$ALIGN POWER}


{#if MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3}

type
	CFDateFormatterRef = ^SInt32; { an opaque type }

// CFDateFormatters are not thread-safe.  Do not use one from multiple threads!

function CFDateFormatterCreateDateFormatFromTemplate( allocator: CFAllocatorRef; tmplate: CFStringRef; options: CFOptionFlags; locale: CFLocaleRef ): CFStringRef; external name '_CFDateFormatterCreateDateFormatFromTemplate';
(* AVAILABLE_MAC_OS_X_VERSION_10_6_AND_LATER *)
	// no options defined, pass 0 for now

function CFDateFormatterGetTypeID: CFTypeID; external name '_CFDateFormatterGetTypeID';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

type
	CFDateFormatterStyle = SIGNEDLONG;
const
// date and time format styles
	kCFDateFormatterNoStyle = 0;
	kCFDateFormatterShortStyle = 1;
	kCFDateFormatterMediumStyle = 2;
	kCFDateFormatterLongStyle = 3;
	kCFDateFormatterFullStyle = 4;

// The exact formatted result for these date and time styles depends on the
// locale, but generally:
//     Short is completely numeric, such as "12/13/52" or "3:30pm"
//     Medium is longer, such as "Jan 12, 1952"
//     Long is longer, such as "January 12, 1952" or "3:30:32pm"
//     Full is pretty complete; e.g. "Tuesday, April 12, 1952 AD" or "3:30:42pm PST"
// The specifications though are left fuzzy, in part simply because a user's
// preference choices may affect the output, and also the results may change
// from one OS release to another.  To produce an exactly formatted date you
// should not rely on styles and localization, but set the format string and
// use nothing but numbers.

function CFDateFormatterCreate( allocator: CFAllocatorRef; locale: CFLocaleRef; dateStyle: CFDateFormatterStyle; timeStyle: CFDateFormatterStyle ): CFDateFormatterRef; external name '_CFDateFormatterCreate';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)
	// Returns a CFDateFormatter, localized to the given locale, which
	// will format dates to the given date and time styles.

function CFDateFormatterGetLocale( formatter: CFDateFormatterRef ): CFLocaleRef; external name '_CFDateFormatterGetLocale';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

function CFDateFormatterGetDateStyle( formatter: CFDateFormatterRef ): CFDateFormatterStyle; external name '_CFDateFormatterGetDateStyle';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

function CFDateFormatterGetTimeStyle( formatter: CFDateFormatterRef ): CFDateFormatterStyle; external name '_CFDateFormatterGetTimeStyle';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)
	// Get the properties with which the date formatter was created.

function CFDateFormatterGetFormat( formatter: CFDateFormatterRef ): CFStringRef; external name '_CFDateFormatterGetFormat';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

procedure CFDateFormatterSetFormat( formatter: CFDateFormatterRef; formatString: CFStringRef ); external name '_CFDateFormatterSetFormat';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)
	// Set the format description string of the date formatter.  This
	// overrides the style settings.  The format of the format string
	// is as defined by the ICU library.  The date formatter starts with a
	// default format string defined by the style arguments with
	// which it was created.


function CFDateFormatterCreateStringWithDate( allocator: CFAllocatorRef; formatter: CFDateFormatterRef; date: CFDateRef ): CFStringRef; external name '_CFDateFormatterCreateStringWithDate';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

function CFDateFormatterCreateStringWithAbsoluteTime( allocator: CFAllocatorRef; formatter: CFDateFormatterRef; at: CFAbsoluteTime ): CFStringRef; external name '_CFDateFormatterCreateStringWithAbsoluteTime';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)
	// Create a string representation of the given date or CFAbsoluteTime
	// using the current state of the date formatter.


function CFDateFormatterCreateDateFromString( allocator: CFAllocatorRef; formatter: CFDateFormatterRef; strng: CFStringRef; rangep: CFRangePtr ): CFDateRef; external name '_CFDateFormatterCreateDateFromString';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

function CFDateFormatterGetAbsoluteTimeFromString( formatter: CFDateFormatterRef; strng: CFStringRef; rangep: CFRangePtr; atp: CFAbsoluteTimePtr ): Boolean; external name '_CFDateFormatterGetAbsoluteTimeFromString';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)
	// Parse a string representation of a date using the current state
	// of the date formatter.  The range parameter specifies the range
	// of the string in which the parsing should occur in input, and on
	// output indicates the extent that was used; this parameter can
	// be NULL, in which case the whole string may be used.  The
	// return value indicates whether some date was computed and
	// (if atp is not NULL) stored at the location specified by atp.


procedure CFDateFormatterSetProperty( formatter: CFDateFormatterRef; key: CFStringRef; value: CFTypeRef ); external name '_CFDateFormatterSetProperty';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

function CFDateFormatterCopyProperty( formatter: CFDateFormatterRef; key: CFStringRef ): CFTypeRef; external name '_CFDateFormatterCopyProperty';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)
	// Set and get various properties of the date formatter, the set of
	// which may be expanded in the future.

var kCFDateFormatterIsLenient: CFStringRef; external name '_kCFDateFormatterIsLenient'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)	// CFBoolean
var kCFDateFormatterTimeZone: CFStringRef; external name '_kCFDateFormatterTimeZone'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)		// CFTimeZone
var kCFDateFormatterCalendarName: CFStringRef; external name '_kCFDateFormatterCalendarName'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)	// CFString
var kCFDateFormatterDefaultFormat: CFStringRef; external name '_kCFDateFormatterDefaultFormat'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)	// CFString
var kCFDateFormatterTwoDigitStartDate: CFStringRef; external name '_kCFDateFormatterTwoDigitStartDate'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *) // CFDate
var kCFDateFormatterDefaultDate: CFStringRef; external name '_kCFDateFormatterDefaultDate'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)	// CFDate
var kCFDateFormatterCalendar: CFStringRef; external name '_kCFDateFormatterCalendar'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)		// CFCalendar
var kCFDateFormatterEraSymbols: CFStringRef; external name '_kCFDateFormatterEraSymbols'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)	// CFArray of CFString
var kCFDateFormatterMonthSymbols: CFStringRef; external name '_kCFDateFormatterMonthSymbols'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)	// CFArray of CFString
var kCFDateFormatterShortMonthSymbols: CFStringRef; external name '_kCFDateFormatterShortMonthSymbols'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *) // CFArray of CFString
var kCFDateFormatterWeekdaySymbols: CFStringRef; external name '_kCFDateFormatterWeekdaySymbols'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)	// CFArray of CFString
var kCFDateFormatterShortWeekdaySymbols: CFStringRef; external name '_kCFDateFormatterShortWeekdaySymbols'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *) // CFArray of CFString
var kCFDateFormatterAMSymbol: CFStringRef; external name '_kCFDateFormatterAMSymbol'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)		// CFString
var kCFDateFormatterPMSymbol: CFStringRef; external name '_kCFDateFormatterPMSymbol'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)		// CFString
var kCFDateFormatterLongEraSymbols: CFStringRef; external name '_kCFDateFormatterLongEraSymbols'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)   // CFArray of CFString
var kCFDateFormatterVeryShortMonthSymbols: CFStringRef; external name '_kCFDateFormatterVeryShortMonthSymbols'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *) // CFArray of CFString
var kCFDateFormatterStandaloneMonthSymbols: CFStringRef; external name '_kCFDateFormatterStandaloneMonthSymbols'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *) // CFArray of CFString
var kCFDateFormatterShortStandaloneMonthSymbols: CFStringRef; external name '_kCFDateFormatterShortStandaloneMonthSymbols'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *) // CFArray of CFString
var kCFDateFormatterVeryShortStandaloneMonthSymbols: CFStringRef; external name '_kCFDateFormatterVeryShortStandaloneMonthSymbols'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *) // CFArray of CFString
var kCFDateFormatterVeryShortWeekdaySymbols: CFStringRef; external name '_kCFDateFormatterVeryShortWeekdaySymbols'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *) // CFArray of CFString
var kCFDateFormatterStandaloneWeekdaySymbols: CFStringRef; external name '_kCFDateFormatterStandaloneWeekdaySymbols'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *) // CFArray of CFString
var kCFDateFormatterShortStandaloneWeekdaySymbols: CFStringRef; external name '_kCFDateFormatterShortStandaloneWeekdaySymbols'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *) // CFArray of CFString
var kCFDateFormatterVeryShortStandaloneWeekdaySymbols: CFStringRef; external name '_kCFDateFormatterVeryShortStandaloneWeekdaySymbols'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *) // CFArray of CFString
var kCFDateFormatterQuarterSymbols: CFStringRef; external name '_kCFDateFormatterQuarterSymbols'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *) 	// CFArray of CFString
var kCFDateFormatterShortQuarterSymbols: CFStringRef; external name '_kCFDateFormatterShortQuarterSymbols'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *) // CFArray of CFString
var kCFDateFormatterStandaloneQuarterSymbols: CFStringRef; external name '_kCFDateFormatterStandaloneQuarterSymbols'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *) // CFArray of CFString
var kCFDateFormatterShortStandaloneQuarterSymbols: CFStringRef; external name '_kCFDateFormatterShortStandaloneQuarterSymbols'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *) // CFArray of CFString
var kCFDateFormatterGregorianStartDate: CFStringRef; external name '_kCFDateFormatterGregorianStartDate'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *) // CFDate
var kCFDateFormatterDoesRelativeDateFormattingKey: CFStringRef; external name '_kCFDateFormatterDoesRelativeDateFormattingKey'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_6_AND_LATER *) // CFBoolean

// See CFLocale.h for these calendar constants:
//	const CFStringRef kCFGregorianCalendar;
//	const CFStringRef kCFBuddhistCalendar;
//	const CFStringRef kCFJapaneseCalendar;
//	const CFStringRef kCFIslamicCalendar;
//	const CFStringRef kCFIslamicCivilCalendar;
//	const CFStringRef kCFHebrewCalendar;
//	const CFStringRef kCFChineseCalendar;
//	const CFStringRef kCFRepublicOfChinaCalendar;
//	const CFStringRef kCFPersianCalendar;
//	const CFStringRef kCFIndianCalendar;
//	const CFStringRef kCFISO8601Calendar;   not yet implemented

{#endif}

{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}
